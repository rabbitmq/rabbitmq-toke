%%  The contents of this file are subject to the Mozilla Public License
%%  Version 1.1 (the "License"); you may not use this file except in
%%  compliance with the License. You may obtain a copy of the License
%%  at http://www.mozilla.org/MPL/
%%
%%  Software distributed under the License is distributed on an "AS IS"
%%  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%  the License for the specific language governing rights and
%%  limitations under the License.
%%
%%  The Original Code is RabbitMQ.
%%
%%  The Initial Developer of the Original Code is VMware, Inc.
%%  Copyright (c) 2009-2011 VMware, Inc.  All rights reserved.
%%

-module(rabbit_msg_store_toke_index).

-behaviour(rabbit_msg_store_index).

-rabbit_boot_step({rabbit_toke,
                   [{description, "Tokyo Cabinet for rabbit_msg_store"},
                    {mfa,         {application, set_env,
                                   [rabbit, msg_store_index_module, ?MODULE]}},
                    {enables,     queue_sup_queue_recovery}]}).

-export([new/1, recover/1,
         lookup/2, insert/2, update/2, update_fields/3, delete/2,
         delete_object/2, delete_by_file/2, terminate/1]).

-include_lib("rabbit_common/include/rabbit_msg_store_index.hrl").

-define(FILENAME, "msg_store_toke.tch").

new(Dir) ->
    {Toke, Path} = init(Dir),
    ok = toke_drv:open(Toke, Path, [read, write, create, truncate, no_lock]),
    Toke.

recover(Dir) ->
    {Toke, Path} = init(Dir),
    case toke_drv:open(Toke, Path, [read, write, no_lock]) of
        ok  -> {ok, Toke};
        Err -> {error, Err}
    end.

init(Dir) ->
    {ok, Toke} = toke_drv:start_link(),
    ok = toke_drv:new(Toke),
    ok = toke_drv:set_cache(Toke, 1000000),
    ok = toke_drv:set_df_unit(Toke, 0),
    ok = toke_drv:tune(Toke, 40000000, -1, 15, [large]),
    {Toke, filename:join(Dir, ?FILENAME)}.

lookup(Key, Toke) -> %% Key is MsgId which is binary already
    case toke_drv:get(Toke, Key) of
        not_found -> not_found;
        Entry     -> #msg_location {} = binary_to_term(Entry)
    end.

insert(Obj = #msg_location { guid = Guid }, Toke) ->
    ok = toke_drv:insert_async(Toke, Guid, term_to_binary(Obj)).

update(Obj, Toke) ->
    insert(Obj, Toke).

update_fun({Position, NewValue}, ObjAcc) ->
    setelement(Position, ObjAcc, NewValue).

update_fields(Key, Updates, Toke) ->
    Fun = fun (ObjBin) ->
                  Obj = #msg_location {} = binary_to_term(ObjBin),
                  term_to_binary(
                    case is_list(Updates) of
                        true  -> lists:foldl(fun update_fun/2, Obj, Updates);
                        false -> update_fun(Updates, Obj)
                    end)
          end,
    ok = toke_drv:update_atomically(Toke, Key, Fun).

delete(Key, Toke) ->
    ok = toke_drv:delete(Toke, Key).

delete_object(Obj = #msg_location { guid = Guid }, Toke) ->
    ok = toke_drv:delete_if_value_eq(Toke, Guid, term_to_binary(Obj)).

delete_by_file(File, Toke) ->
    DeleteMe = toke_drv:fold(
                 fun (Key, Obj, Acc) ->
                         case (binary_to_term(Obj))#msg_location.file of
                             File -> [Key | Acc];
                             _    -> Acc
                         end
                 end, [], Toke),
    [ok = toke_drv:delete(Toke, Key) || Key <- DeleteMe],
    ok.

terminate(Toke) ->
    ok = toke_drv:close(Toke),
    ok = toke_drv:stop(Toke).
