-module(rabbit_toke).

-rabbit_boot_step({rabbit_toke,
                   [{description, "Tokyo Cabinet for rabbit_msg_store"},
                    {mfa,         {application, start, [rabbit_toke]}},
                    {pre,         message_store_queue_sup_queue_recovery}]}).

-export([start/0, stop/0, start/2, stop/1]).

-define(LIBNAME, "libtoke").

start() ->
    rabbit_toke_sup:start_link(),
    ok.

stop() ->
    ok.

start(normal, []) ->
    rabbit_toke_sup:start_link().

stop(_State) ->
    ok.
