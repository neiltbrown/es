%%%-------------------------------------------------------------------
%% @doc es top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('es_sup').

-behaviour(supervisor).

%% API
-export([start_link/0, start_es/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_es(Config) ->
    TabId = ets:new(aggregates, [protected, named_table, set]),
    store_config(Config, TabId),
    CommandBus = {es_command_bus,
                  {es_command_bus, start_link, []},
                  permanent,
                  5000,
                  worker,
                  [es_command_bus]},
    supervisor:start_child(?SERVER, CommandBus).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    AggregateSup = {es_aggregate_sup,
                    {es_aggregate_sup, start_link, []},
                    permanent,
                    5000,
                    supervisor,
                    [es_aggregate_sup]},
    {ok, { {one_for_one, 10, 1}, [AggregateSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================

store_config(Config, TabId) ->
    lists:foreach(
      fun(#{aggregate := Aggregate} = AggregateConfig) ->
              true = ets:insert(TabId, {Aggregate, AggregateConfig})
                  end,
      Config
     ).
