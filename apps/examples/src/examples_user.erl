-module(examples_user).

%-behaviour(es_aggregate).

-export([
         apply_event/2,
         replay/1,
         init/0
        ]).

init() ->
    %error_logger:info_report("Init User", []),
    #{}.

apply_event(#{user_name := UserName}, State) ->
    error_logger:info_report("Username ~p", [UserName]),
    State#{user_name => UserName}.

replay(Events) ->
    lists:foldl(
      fun(Event, State) ->
              apply_event(Event, State)
      end,
      init(),
      Events
     ).
