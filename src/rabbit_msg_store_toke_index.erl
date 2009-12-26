%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developers of the Original Code are LShift Ltd,
%%   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
%%   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
%%   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
%%   Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd are Copyright (C) 2007-2009 LShift
%%   Ltd. Portions created by Cohesive Financial Technologies LLC are
%%   Copyright (C) 2007-2009 Cohesive Financial Technologies
%%   LLC. Portions created by Rabbit Technologies Ltd are Copyright
%%   (C) 2007-2009 Rabbit Technologies Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%

-module(rabbit_msg_store_toke_index).
-export([init/1, lookup/2, insert/2, update/2, update_fields/3, delete/2,
         delete_by_file/2, terminate/1]).

-rabbit_boot_step({rabbit_toke,
                   [{description, "Tokyo Cabinet for rabbit_msg_store"},
                    {mfa,         {application, set_env,
                                   [rabbit, msg_store_index_module, ?MODULE]}},
                    {pre,         message_store_queue_sup_queue_recovery}]}).

%% TODO: work out how to share this with the rabbit_msg_store.hrl
-record(msg_location,
        {msg_id, ref_count, file, offset, total_size}).

-define(FILENAME, "msg_store_toke.tch").

init(Dir) ->
    {ok, Toke} = toke_drv:start_link(),
    ok = toke_drv:new(Toke),
    ok = toke_drv:set_cache(Toke, 1000000),
    ok = toke_drv:set_df_unit(Toke, 50000),
    ok = toke_drv:tune(Toke, 2000000, 5, 15, [large]),
    ok = toke_drv:open(Toke, filename:join(Dir, ?FILENAME),
                       [read, write, create, truncate, no_lock]),
    Toke.

lookup(Key, Toke) -> %% Key is MsgId which is binary already
    case toke_drv:get(Toke, Key) of
        not_found -> not_found;
        Entry     -> #msg_location {} = binary_to_term(Entry)
    end.

insert(Obj = #msg_location{ msg_id = MsgId }, Toke) ->
    ok = toke_drv:insert_async(Toke, MsgId, term_to_binary(Obj)).

update(Obj, Toke) ->
    insert(Obj, Toke).

update_fun({Position, NewValue}, ObjAcc) ->
    setelement(Position, ObjAcc, NewValue).

update_fields(Key, Updates, Toke) ->
    Obj = #msg_location {} = lookup(Key, Toke),
    ok = insert(case is_list(Updates) of
                    true  -> lists:foldl(fun update_fun/2, Obj, Updates);
                    false -> update_fun(Updates, Obj)
                end, Toke).

delete(Key, Toke) ->
    ok = toke_drv:delete(Toke, Key).

delete_by_file(File, Toke) ->
    ok = toke_drv:fold(
           fun (Key, Obj, ok) ->
                   #msg_location { file = FileObj } = binary_to_term(Obj),
                   case FileObj == File of
                       true  -> toke_drv:delete(Key, Toke);
                       false -> ok
                   end
           end, ok, Toke).

terminate(Toke) ->
    ok = toke_drv:close(Toke),
    ok = toke_drv:delete(Toke),
    ok = toke_drv:stop(Toke).
