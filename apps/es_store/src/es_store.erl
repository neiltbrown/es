-module('es_store').

-type event() :: map().
-type reason() :: term().
-type error() :: {error, reason()}.

-callback init() ->
    ok | error().

-callback store(event()) ->
    ok | error().

-callback stream() ->
    [event()].
