%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_msg_store_toke_index).

-behaviour(rabbit_msg_store_index).

-include_lib("rabbit_common/include/rabbit_msg_store.hrl").

-rabbit_boot_step({rabbit_toke,
                   [{description, "Tokyo Cabinet for rabbit_msg_store"},
                    {mfa,         {application, set_env,
                                   [rabbit, msg_store_index_module, ?MODULE]}},
                    {enables,     recovery}]}).

-export([new/1, recover/1,
         lookup/2, insert/2, update/2, update_fields/3, delete/2,
         delete_object/2, clean_up_temporary_reference_count_entries_without_file/1, terminate/1]).

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

insert(Obj = #msg_location { msg_id = MsgId }, Toke) ->
    ok = toke_drv:insert_async(Toke, MsgId, term_to_binary(Obj)).

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

delete_object(Obj = #msg_location { msg_id = MsgId }, Toke) ->
    ok = toke_drv:delete_if_value_eq(Toke, MsgId, term_to_binary(Obj)).

clean_up_temporary_reference_count_entries_without_file(Toke) ->
    DeleteMe = toke_drv:fold(
                 fun (Key, Obj, Acc) ->
                         case (binary_to_term(Obj))#msg_location.file of
                             undefined -> [Key | Acc];
                             _         -> Acc
                         end
                 end, [], Toke),
    [ok = toke_drv:delete(Toke, Key) || Key <- DeleteMe],
    ok.

terminate(Toke) ->
    ok = toke_drv:close(Toke),
    ok = toke_drv:stop(Toke).
