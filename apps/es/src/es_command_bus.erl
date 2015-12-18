-module(es_command_bus).

-behaviour(gen_server).

%% API
-export([start_link/0, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

send(Aggregate, Command) ->
    gen_server:cast(?MODULE, {Aggregate, Command}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({Aggregate,
            #{aggregate_id := AggregateId} = Command},
            State) ->
    {ok, AggregateManagerRef} =
        es_aggregate_sup:start_aggregate(Aggregate, AggregateId),

    _ = es_aggregate_manager:handle_command(AggregateManagerRef, Command),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
