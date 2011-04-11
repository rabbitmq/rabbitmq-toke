DEPS:=rabbitmq-server rabbitmq-erlang-client toke
NOT_BUILDABLE:=$(shell toke/check-deps.sh)