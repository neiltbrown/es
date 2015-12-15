-module(es_aggregate).

-callback init() ->
    State :: map().
-callback apply_event(Event :: map()) ->
    State :: map().
-callback replay(Events :: list()) ->
    State :: map().


