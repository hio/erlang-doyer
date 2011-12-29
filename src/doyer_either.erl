% ----------------------------------------------------------------------------
% doyer_either.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer_either).
-export([can_bind/1]).
-export([bind/2]).

-export_type([either/2]).

-type either(Left,Right) :: {left, Left} | {right, Right}.

% ----------------------------------------------------------------------------
-spec can_bind(
		MVal :: term()
	) -> boolean().
	%(
	%	MVal :: either(_L,_R)
	%) -> true;
	%(
	%	MVal :: term()
	%) -> false.
can_bind({left, _Left}) ->
	true;
can_bind({right, _Right}) ->
	true;
can_bind(_) ->
	false.

% ----------------------------------------------------------------------------
-spec bind(
		M_a :: either(Left, Right),
		F   :: fun((Right) -> either(Left, Right))
	) -> either(Left, Right).
bind({left, Left}, _F) ->
	{left, Left};
bind({right, Right}, F) ->
	F(Right).

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
