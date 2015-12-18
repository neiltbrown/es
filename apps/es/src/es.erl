-module(es).

-export([start/0]).
-export([stop/0]).

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).
