-module(user_command_handler).

-behaviour(es_command_handler).

-export([
         handle_command/1
        ]).

handle_command(_Command) ->
    error_logger:info_report("Handling command", []),
    {handled, replay, #{user_name => "Neil"}}.
