-module(es_store_ets).

-behaviour(es_store).

-export([
         init/0,
         store/1,
         stream/1
        ]).

init() ->
    ets:new(event_store, [set, named_table, private]).

store(#{aggregate_id := AggregateId} = Event) ->
    case ets:lookup(event_store, AggregateId) of
        [] ->
            ets:insert(event_store, {AggregateId, [Event]});
        [{AggregateId, Events}] ->
            ets:insert(event_store, {AggregateId, [Event | Events]})
    end.

stream(AggregateId) ->
    ets:lookup(event_store, AggregateId).
