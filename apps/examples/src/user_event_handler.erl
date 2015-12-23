-module(user_event_handler).

-behaviour(es_event_handler).

-export([
         handle_event/1
        ]).

handle_event(Event) ->
    io:fwrite("Applying event ~n~p~n", [Event]),
    ok.
