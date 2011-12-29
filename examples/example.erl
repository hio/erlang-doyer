% ----------------------------------------------------------------------------
% examples/example.erl
% ----------------------------------------------------------------------------
-module(example).
-export([main/0]).
-export([run/0]).
-include_lib("doyer/include/doyer.hrl").

-spec main() -> ok.
main() ->
  try
    io:format("run_1 = ~p~n", [run_1()]),
    io:format("run_2 = ~p~n", [run_2()]),
    io:format("run_3 = ~p~n", [run_3()]),
    ok
  catch
    C:R ->
      io:format("~p:~p:~n~p~n", [C,R,erlang:get_stacktrace()])
  end,

  init:stop(),
  ok.

-spec run() -> term().
run() ->
  {
    run_1(),
    run_2()
  }.

run_1() ->
  % Prelude> :{
  %   do x <- return 10;
  %      y <- return 20;
  %      return 100;
  %      return (x,y)
  %      :: Maybe (Int, Int)
  %   :}
  % ==> Just (10,20)
  ?do([
    X = {value, 10}, % ?return(10).
    Y = {value, 20},
    {value, 100},
    {value, {X,Y}}
  ]).

run_2() ->
  % Prelude Control.Monad.Error> :{
  %   do x <- return 10;
  %      y <- return 20;
  %      return 100;
  %      return (x,y)
  %      :: Either String (Int, Int)
  %   :}
  % ==> Right (10,20)
  ?do(begin
    X = {right, 10}, % ?return(10).
    Y = {right, 20},
    {right, 100},
    {right, {X, Y}}
  end).

run_3() ->
  % Prelude Control.Monad.State> :{
  %   runState
  %     (do x <- return 10;
  %         y <- State $ \s -> (s, 30);
  %         return 100;
  %         return (x,y))
  %    20
  %   :}
  % ==> ((10,20),30)
  Do = ?do([
    % ?default_monad_selector()
    _
  ||
    X <- doyer_state:return(10),
    Y <- doyer_state:state(fun(Seed) ->
      io:format(".. seed=~p~n",[Seed]),
      {Seed,30}
    end),
    doyer_state:return(100),
    doyer_state:return({X,Y})
  ]),
  io:format(".. runState~n"),
  doyer_state:runState(Do, 20).

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
