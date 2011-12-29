% ----------------------------------------------------------------------------
% doyer_maybe.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer_maybe).
-export([can_bind/1]).
-export([bind/2]).
-export([return/1]).
-export([mzero/0]).

-export_type([maybe/1]).

-type maybe(T) :: {value, T} | none.
-type value_of_maybe(T) :: {value, T}.
-type none_of_maybe(_T) :: none.

% ----------------------------------------------------------------------------
-spec can_bind(
		MVal :: term()
	) -> boolean().
	%(
	%	MVal :: maybe(_T)
	%) -> true;
	%%(
	%	MVal :: term()
	%) -> false.
can_bind({value, _Value}) ->
	true;
can_bind(none) ->
	true;
can_bind(_) ->
	false.

% ----------------------------------------------------------------------------
-spec bind(
		M_a :: maybe(T_a),
		F   :: fun((T_a) -> maybe(T_b))
	) -> maybe(T_b).
bind(none, _F) ->
	none;
bind({value, Val}, F) ->
	case F(Val) of
		{value, Val_2} ->
			{value, Val_2};
		none ->
			none
	end.

% ----------------------------------------------------------------------------
-spec return(
		Val :: T
	) -> value_of_maybe(T).
return(Val) ->
	{value, Val}.

% ----------------------------------------------------------------------------
-spec mzero() -> none_of_maybe(_T).
mzero() ->
	none.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
