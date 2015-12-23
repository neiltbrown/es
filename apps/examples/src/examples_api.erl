-module(examples_api).

-export([send/0]).

send() ->
    es_command_bus:send(examples_user, #{aggregate_id => <<"user123">>, user_name => "Neil"}).
