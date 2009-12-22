{application, rabbit_toke,
 [{description, "Tokyo Cabinet for Erlang in RabbitMQ to do message store index"},
  {vsn, "0.01"},
  {modules, [
    rabbit_toke,
    rabbit_toke_sup,
    rabbit_msg_store_toke_index,
    toke_drv
  ]},
  {registered, []},
  {mod, {rabbit_toke, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
