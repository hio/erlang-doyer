% ----------------------------------------------------------------------------
% doyer_execute.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer_execute).
-export([default_monad_selector/0]).
-export([bind/3]).

-include("doyer_private.hrl").

-spec default_monad_selector() -> doyer:monad_selector().
default_monad_selector() ->
  #doyer_monad_selector {
    candidates = [
      doyer_either,
      doyer_maybe,
      doyer_ok_error,
      doyer_state
    ]
  }.

-spec bind(
    MVal      :: _M_a,
    Action    :: fun((_a) -> M_b),
    MonadMods :: [module()]
  ) -> M_b.
bind(MVal, Action, MonadMods) ->
  MonadMods_2 = [ M || M <- MonadMods, M:can_bind(MVal) ],
  case MonadMods_2 of
    [MonadMod] ->
      MonadMod:bind(MVal, Action);
    [] ->
      error({no_binder, MVal, MonadMods});
    _ ->
      error({binder_ambitious, MVal, MonadMods_2})
  end.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
