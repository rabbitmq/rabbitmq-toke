{application, rabbitmq_toke,
 [{description, "Tokyo Cabinet for Erlang in RabbitMQ to do message store index"},
  {vsn, "%%VSN%%"},
  {modules, [
    rabbit_msg_store_toke_index
  ]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib, toke]}]}.
