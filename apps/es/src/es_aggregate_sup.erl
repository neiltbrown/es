-module(es_aggregate_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_aggregate/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_aggregate(Aggregate, AggregateId) ->
    AggregateManager = {aggregate_manager_ref(Aggregate, AggregateId),
                  {es_aggregate_manager, start_link, [Aggregate]},
                  permanent,
                  5000,
                  worker,
                  [es_aggregate_manager]},
    case supervisor:start_child(?SERVER, AggregateManager) of
        {ok, _P} = Pid -> Pid;
        {error, {already_started, P}} -> {ok, P};
        {error, _Reason} = Error -> Error
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

aggregate_manager_ref(Aggregate, AggregateId) ->
    A = atom_to_binary(Aggregate, utf8),
    <<A/binary, "@", AggregateId/binary>>.
