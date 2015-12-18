-module(es).

-export([start/0, start_es/1]).
-export([stop/0]).

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).

start_es(Config) ->
    es_sup:start_es(Config).
