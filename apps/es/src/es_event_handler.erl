-module(es_event_handler).

-callback handle_event(Event :: map()) ->
    ok | {error, Reason :: term()}.
