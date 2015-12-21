-module(user_event_handler).

-behaviour(es_event_handler).

-export([
         handle_event/1
        ]).

handle_event(_Event) ->
    error_logger:info_report("Applying event", []),
    ok.
