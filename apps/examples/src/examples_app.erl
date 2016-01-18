%%%-------------------------------------------------------------------
%% @doc examples public API
%% @end
%%%-------------------------------------------------------------------

-module('examples_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Config = [#{aggregate => examples_user,
                command_handlers =>[user_command_handler],
                event_handlers => [user_event_handler]}],
    es:start_es(Config),
    'examples_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
