-module(es_aggregate_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, handle_command/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {aggregate = undefined,
                command_handlers = [],
                event_handlers = []}).

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
    self() ! {Aggregate, init_aggregate},
    self() ! {Aggregate, init_handlers},
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Command, #state{command_handlers = CommandHandlers,
                        event_handlers = EventHandlers} = State) ->
    Events = lists:foldl(
               fun(Handler, A) ->
                       [Handler:handle_command(Command) | A]
               end,
               [],
               CommandHandlers),
    {noreply, State}.

handle_info({Aggregate, init_aggregate}, State) ->
    {noreply, State#state{aggregate = Aggregate}};

handle_info({Aggregate, init_handlers}, State) ->
    [{_, {_, CommandHandlers}, {_, EventHandlers}}] = 
        ets:lookup(aggregates, Aggregate),
    {noreply, State#state{command_handlers = CommandHandlers,
                          event_handlers = EventHandlers}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
