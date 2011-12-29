% ----------------------------------------------------------------------------
% doyer_ok_error.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer_ok_error).
-export([can_bind/1]).
-export([bind/2]).
-export([return/1]).
-export([mzero/0]).

-export_type([ok_error/2]).

-type ok_error(E,T) :: ok | error | {ok, T} | {error, E}.

% ----------------------------------------------------------------------------
-spec can_bind(
		MVal :: doyer:monad(_T)
	) -> boolean().
	%(
	%	MVal :: ok_error(_E, _T)
	%) -> true;
	%%(
	%	MVal :: term()
	%) -> false.
can_bind(ok) ->
	true;
can_bind({ok, _Value}) ->
	true;
can_bind(error) ->
	true;
can_bind({error, _Value}) ->
	true;
can_bind(_) ->
	false.

% ----------------------------------------------------------------------------
-spec bind(
		M_a :: ok_error(E, T_a),
		F   :: fun((T_a) -> ok_error(E, T_b))
	) -> ok_error(E, T_b).
bind(error, _F) ->
	error;
bind({error, _Value}=E, _F) ->
	E;
bind(ok, F) ->
	bind_2(ok, F);
bind({ok, Value}, F) ->
	bind_2(Value, F).

bind_2(Val, F) ->
	case F(Val) of
		ok = X ->
			X;
		{ok,_} = X ->
			X;
		error = X ->
			X;
		{error,_} = X ->
			X
	end.

% ----------------------------------------------------------------------------
-spec return(
		Val :: T
	) -> {ok, T}.
return(Val) ->
	{ok, Val}.

% ----------------------------------------------------------------------------
-spec mzero() -> error.
mzero() ->
	error.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
