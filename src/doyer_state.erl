% ----------------------------------------------------------------------------
% doyer_state.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer_state).
-export([can_bind/1]).
-export([bind/2]).
-export([return/1]).
-export([state/1]).
-export([runState/2]).

-export_type([state/2]).

-record(doyer_state, {
	runState :: fun( (_) -> {_,_} ) % runstate_fun(_SeedT, _GenT)
}).

-type runstate_fun(SeedT, GenT) :: fun((SeedT) -> {GenT,SeedT}).
-type state(_SeedT, _GenT) :: #doyer_state {
		runState :: fun( (_) -> {_,_} ) % runstate_fun(_SeedT, _GenT)
	}.

% ----------------------------------------------------------------------------
-spec can_bind(
		MVal :: term() | state(_SeedT, _GenT)
	) -> boolean().
	%(
can_bind(#doyer_state{}) ->
	true;
can_bind(_) ->
	false.

% ----------------------------------------------------------------------------
-spec return(
		Val :: GenT
	) -> state(_SeedT,GenT).
return(Val) ->
  #doyer_state {
		runState = fun(Seed) ->
			{Val,Seed}
		end
	}.

%newtype State s a = State { runState :: (s -> (a,s)) } 
%instance Monad (State s) where 
%    return a        = State $ \s -> (a,s)
%    (State x) >>= f = State $ \s -> let (v,s') = x s in runState (f v) s'
%-
%newtype State ST AT = StateData { runState :: (ST -> (AT,ST)) } 
%instance Monad (State ST) where 
%    return a            = StateData $ \s -> (a,s)
%    (StateData x) >>= f = StateData $ \s -> let (v,s') = x s
%                                            f_v = f v
%                                            ff = runState f_v
%                                        in ff s'

% ----------------------------------------------------------------------------
-spec bind(
		M_a :: state(SeedT, GenT),
		F   :: fun((GenT) -> state(SeedT, GenT))
	) -> state(SeedT, GenT).
bind(#doyer_state {}=ST_In, IncrFun) ->
	#doyer_state {
		runState = fun(Seed) ->
			% このactionが呼ばれたら, まず先のactionを実行
			% するようにする.
			{Val_1, Seed_2} = runState(ST_In, Seed),
			% 自分のaction(IncrFun)を取得する.
			St_From_Val = IncrFun(Val_1),
			% そしてそれを実行.
			{Val_2,Seed_3} = runState(St_From_Val, Seed_2),
			{Val_2,Seed_3}
		end
	}.

% ----------------------------------------------------------------------------
-spec state (
		RunStateFun :: runstate_fun(SeedT, GenT)
	) -> state(SeedT, GenT).
state(RunStateFun) ->
	#doyer_state {
		runState = RunStateFun
	}.

% ----------------------------------------------------------------------------
-spec runState(
		ST  :: state(SeedT,GenT),
		ST0 :: SeedT
	) -> {GenT,SeedT}.
runState(ST, Seed) ->
	RunStateFun = ST#doyer_state.runState,
	RunStateFun(Seed).

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
