-module(es_aggregate_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, handle_command/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {aggregate = undefined,
                aggregate_state = undefined,
                event_mgr_ref = undefined,
                command_handlers = [],
                event_handlers = [],
                event_store = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Aggregate) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Aggregate], []).

handle_command(AggregateManager, Command) ->
    gen_server:cast(AggregateManager, Command).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Aggregate]) ->
    error_logger:info_report("Init Agg Man", []),
    [{Aggregate, Config}] = ets:lookup(aggregates, Aggregate),
    self() ! {Config, event_store},
    self() ! {Config, init_aggregate},
    self() ! {Config, init_handlers},
    self() ! {Config, init_replay},
    
    {ok, EventMgrRef} = gen_event:start_link(), 
    {ok, #state{event_mgr_ref = EventMgrRef}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Command, #state{command_handlers = CommandHandlers,
                            aggregate = Aggregate,
                            aggregate_state = AggregateState,
                            event_mgr_ref = EventMgrRef,
                            event_store = EventStore} = State) ->
    error_logger:info_report("Handle command", []),
    Events = apply_command_handlers(Command, CommandHandlers),
    io:fwrite("~p", Events),
    _ = store_events(Events, EventStore),
    AggregateState2 = apply_events_to_aggregate(Events,
                                                Aggregate,
                                                AggregateState),
    publish_events(EventMgrRef, Events),
    {noreply, State#state{aggregate_state = AggregateState2}}.

handle_info({#{aggregate := Aggregate}, init_aggregate}, State) ->
    error_logger:info_report("Init agg state", []),
    AggregateState = Aggregate:init(),
    {noreply, State#state{aggregate = Aggregate,
                          aggregate_state = AggregateState}};

handle_info({#{command_handlers := CommandHandlers,
               event_handlers := EventHandlers},init_handlers},
            #state{event_mgr_ref = EventMgrRef} = State) ->
    _ = es_event_bus:register_event_handlers(EventMgrRef, EventHandlers),
    {noreply, State#state{command_handlers = CommandHandlers,
                          event_handlers = EventHandlers}};

handle_info({#{event_store := EventStore}, event_store}, State) ->
    {noreply, State#state{event_store = EventStore}};

handle_info({#{aggregate := Aggregate}, init_replay},
            #state{event_store = EventStore,
                   aggregate_state = AggregateState} = State) ->
    Events = EventStore:stream(),
    AggregateState2 = apply_events_to_aggregate(Aggregate, Events, AggregateState),
    {noreply, State#state{aggregate_state = AggregateState2}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_command_handlers(Command, CommandHandlers) ->
    DeepList = lists:foldl(
                 fun(Handler, A) ->
                         [Handler:handle_command(Command) | A]
                 end,
                 [],
                 CommandHandlers),
    lists:flatten(DeepList).

store_events(Events, EventStore) ->
    lists:map(fun(Event) ->
                      EventStore:store(Event)
              end,
              Events).

apply_events_to_aggregate(Events, Aggregate, AggregateState) ->
    lists:foldl(
      fun(Event, A) ->
              Aggregate:apply_event(Event, A)
      end,
      AggregateState,
      Events).

publish_events(EventMgrRef, Events) ->
    lists:foreach(
      fun(Event) ->
              es_event_bus:publish(EventMgrRef, Event)
      end,
      Events).

