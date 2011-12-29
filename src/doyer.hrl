% ----------------------------------------------------------------------------
% doyer.hrl.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-ifndef(doyer_hrl).
-define(doyer_hrl, 1).

-compile([{parse_transform, doyer}]).

-define(DOYER_FAKEMOD, doyer_fakemod).

-define(do(X),     ?DOYER_FAKEMOD:do(X)).
-define(do(M,X),   ?DOYER_FAKEMOD:do(M, X)).
-define(return(X), ?DOYER_FAKEMOD:return(X)).
-define(default_monad_selector(), ?DOYER_FAKEMOD:default_monad_selector()).

-endif.
% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------

