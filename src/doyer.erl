% ----------------------------------------------------------------------------
% doyer.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer).
-export([version/0]).
-export([version_string/0]).

-export_type([monad/1]).
-export_type([monad_selector/0]).

% private.
-export([parse_transform/2]).

-include("doyer_private.hrl").

-type monad(_T) :: term().
-type monad_selector() :: #doyer_monad_selector{}.

-spec version() -> {non_neg_integer(),non_neg_integer(),non_neg_integer(),non_neg_integer()}.
version() ->
  {0,1,0,0}.

-spec version_string() -> string().
version_string() ->
  {Major,Minor,Patch,Build} = version(),
  lists:flatten(io_lib:format("~p.~p.~p.~p", [Major,Minor,Patch,Build])).

-spec parse_transform(
    Forms :: [doyer_transform:erl_parse_abstract_form()],
    Opts  :: [compile:option()]
  ) -> [doyer_transform:erl_parse_abstract_form()].
parse_transform(Form, Opts) ->
  doyer_transform:parse_transform(Form, Opts).

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
