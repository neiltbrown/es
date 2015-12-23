-module(es_event_bus).

-behaviour(gen_event).

%% API
-export([start_link/0, register_event_handlers/2, publish/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {event_handlers = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_event:start_link({local, ?SERVER}).

register_event_handlers(EventMgrRef, EventHandlers) ->
    io:fwrite("Adding event handlers ~p~n", [EventHandlers]),
    gen_event:add_handler(EventMgrRef, ?MODULE, EventHandlers).

publish(EventMgrRef, Event) ->
    io:fwrite("publishing event"),
    gen_event:notify(EventMgrRef, Event).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init(EventHandlers) ->
    {ok, #state{event_handlers = EventHandlers}}.

handle_event(Event, #state{event_handlers = EventHandlers} = State) ->
    io:fwrite("Notify event handlers ~p~n", [EventHandlers]),
    notify_event_handlers(Event, EventHandlers),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_event_handlers(Event, EventHandlers) ->
    lists:foreach(
      fun(Handler) ->
              Handler:handle_event(Event)
      end,
      EventHandlers
     ).
