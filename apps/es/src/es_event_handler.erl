-module(es_event_handler).

-callback handle_command(Command :: map()) ->
    ok | {error, Reason :: term()}.
