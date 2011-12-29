% ----------------------------------------------------------------------------
% doyer_transform.
% ----------------------------------------------------------------------------
% Copyright 2011 YAMASHINA Hio. All rights reserved.
% License: BSD (2-clause)
% ----------------------------------------------------------------------------
-module(doyer_transform).
-export([parse_transform/2]).
-export([format_error/1]).

-export_type([erl_parse_abstract_form/0]).

-include("doyer_private.hrl").

-type lineno() :: erl_scan:line().
-type erl_parse_abstract_form() :: tuple().
%-type abstract_form() :: erl_parse:abstract_form().
-type abstract_form() :: erl_parse_abstract_form().

-define(LOG_DEBUG(F,A), (case os:getenv("TRACE") of
    false -> ok;
    ""    -> ok;
    "0"   -> ok;
    _     -> io:format(F, A)
  end)).

-define(FAKEMOD, doyer_fakemod).
-define(EXECMOD, doyer_execute).

-record(error_descriptor, {
  msg :: string()
}).
-type error_descriptor() :: #error_descriptor{}.
-type err_info_item()    :: {lineno(), module(), error_descriptor()}.
-type err_info()         :: {file:name(), [err_info_item()]}.

-record(parsed_info, {
  file        = "" :: file:name(),
  monadmods   = [] :: [module()],
  errors      = [] :: [err_info()],
  warnings    = []
}).

% ----------------------------------------------------------------------------
% format_error(ErrDesc).
% called from compiler framework.
%
-spec format_error(error_descriptor()) -> string().
format_error(ErrDesc) ->
  ErrDesc#error_descriptor.msg.

% ----------------------------------------------------------------------------
-spec default_monadmods() -> [module()].
default_monadmods() ->
  MonadSel  = ?EXECMOD:default_monad_selector(),
  MonadMods = MonadSel#doyer_monad_selector.candidates,
  MonadMods.

% ----------------------------------------------------------------------------
% parse_transform.
%
-spec parse_transform(
    Form :: [abstract_form()],
    Opts :: [compile:option()]
  ) -> [abstract_form()].
parse_transform(F, O) ->
  ?LOG_DEBUG("F:~n~p~nO:~n~p~n", [F, O]),
  Result  = [],
  ParsedInfo = #parsed_info {
    file      = "",
    monadmods = default_monadmods()
  },
  F2 = parse_1(F, Result, ParsedInfo, O),
  F2.

parse_1([{function,_LineNo,_FunName,_Arity, _Clauses}=Head | Rest], Result, ParsedInfo, O) ->
  {Result_2, ParsedInfo_2} = rewrite_function(Head, Result, ParsedInfo),
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([{attribute, _LineNo, file, {FileName, _LineNo2}}=Head | Rest], Result, ParsedInfo, O) ->
  ParsedInfo_2 = ParsedInfo#parsed_info {
    file = FileName
  },
  Result_2 = [Head | Result],
  parse_1(Rest, Result_2, ParsedInfo_2, O);

parse_1([Head | Rest], Result, ParsedInfo, O) ->
  Result_2 = [Head | Result],
  parse_1(Rest, Result_2, ParsedInfo, O);

parse_1([], Result, ParsedInfo, _O) ->
  ?LOG_DEBUG("R:~n~p~n~p~n", [ParsedInfo, Result]),

  Result_2 = lists:reverse(Result),
  make_compiler_result(Result_2, ParsedInfo).

make_compiler_result(Result, ParsedInfo) ->
  if
    ParsedInfo#parsed_info.errors /= [] ->
      ErrorList = ParsedInfo#parsed_info.errors,
      WarnList  = ParsedInfo#parsed_info.warnings,
      {error, ErrorList, WarnList};
    ParsedInfo#parsed_info.warnings /= [] ->
      WarnList  = ParsedInfo#parsed_info.warnings,
      {warning, Result, WarnList};
    true -> % otherwise.
      Result % [abstract_form()].
  end.

% ----------------------------------------------------------------------------
-record(decode_list_shallowly, {
  nil_lineno :: lineno(),
  elements   :: [ { ConsLineNo::lineno(), Hd :: abstract_form() } ]
}).

decode_list_shallowly({nil, LineNo}) ->
  List = #decode_list_shallowly {
    nil_lineno = LineNo,
    elements   = []
  },
  {ok, List};
decode_list_shallowly({cons, ConsLineNo, Hd, Tl}) ->
  case decode_list_shallowly(Tl) of
    {ok, Tl_2} ->
      Elems = Tl_2#decode_list_shallowly.elements,
      TL_3 = Tl_2#decode_list_shallowly {
        elements   = [ { ConsLineNo, Hd } | Elems ]
      },
      {ok, TL_3};
    error ->
      error
  end;
decode_list_shallowly(_) ->
  error.

% ----------------------------------------------------------------------------
rewrite_function({function, LineNo, FunName, Arity, Clauses}, Result, ParsedInfo) ->
  {Clauses_2, ParsedInfo_2} = rewrite_clauses(Clauses, ParsedInfo),
  FunOut = {function, LineNo, FunName, Arity, Clauses_2},
  Result_2 = [FunOut | Result],
  {Result_2, ParsedInfo_2}.

rewrite_clauses(Clauses, ParsedInfo) ->
  {Clauses_4, ParsedInfo_4} = lists:foldl(fun(Clause, AccIn) ->
     {Clauses_2, ParsedInfo_2} = AccIn,
     {Clause_2, ParsedInfo_3} = rewrite_clause(Clause, ParsedInfo_2),
     Clauses_3 = [Clause_2 | Clauses_2],
     {Clauses_3, ParsedInfo_3}
  end, {[], ParsedInfo}, Clauses),
  Clauses_5 = lists:reverse(Clauses_4),
  {Clauses_5, ParsedInfo_4}.


rewrite_clause({clause, LineNo, Term1, Term2, Body}, ParsedInfo) ->
  {Body_2, ParsedInfo_2} = rewrite_function_body(Body, ParsedInfo),
  Clause_2 = {clause, LineNo, Term1, Term2, Body_2},
  {Clause_2, ParsedInfo_2}.

rewrite_function_body(Body, ParsedInfo) ->
  {Body_2, ParsedInfo_2} = lists:foldl(fun(ExprIn, AccIn) ->
    {Result_1, ParsedInfo_2} = AccIn,
    {ExprOut, ParsedInfo_3} = rewrite_expr(ExprIn, ParsedInfo_2),
    Result_2 = [ExprOut | Result_1],
    {Result_2, ParsedInfo_3}
  end, {[], ParsedInfo}, Body),
  Body_3 = lists:reverse(Body_2),
  {Body_3, ParsedInfo_2}.

rewrite_expr({call, LineNo1, FunLoc, Args}=Expr, ParsedInfo) ->
  case FunLoc of
    {remote, _LineNo2, {atom, _LineNo3, ?FAKEMOD}, {atom, _LineNo4, Fun}} ->
      Arity = length(Args),
      case {Fun, Arity, Args} of
        % ?do([...]).
        {do, 1, [{cons, _LineNo5, _Hd,_Tl}=List]} ->
          rewrite_do_list(List, none, Expr, ParsedInfo);
        % ?do(monadmod, [...]).
        % ?do([m1, m2, ...], [...]).
        {do, 2, [MonadMods, {cons, _LineNo6, _Hd,_Tl}=List]} ->
          rewrite_do_list(List, {value, MonadMods}, Expr, ParsedInfo);

        % ?do({...}).
        {do, 1, [{tuple, LineNo5, List}]} ->
          rewrite_do_tuple(List, LineNo5, none, Expr, ParsedInfo);
        % ?do(monadmod, {...}).
        % ?do([m1, m2, ...], {...}).
        {do, 2, [MonadMods, {tuple, LineNo5, List}]} ->
          rewrite_do_tuple(List, LineNo5, {value, MonadMods}, Expr, ParsedInfo);

        % ?do(begin ... end).
        {do, 1, [{block, LineNo5, List}]} ->
          rewrite_do_block(List, LineNo5, none, Expr, ParsedInfo);
        % ?do(monadmod, begin ... end).
        % ?do([m1, m2, ...], begin ... end).
        {do, 2, [MonadMods, {block, LineNo5, List}]} ->
          rewrite_do_block(List, LineNo5, {value, MonadMods}, Expr, ParsedInfo);

        % ?do([monadmod || ... ]).
        % ?do([[m1, m2, ...] || ... ]).
        {do, 1, [{lc, LineNo5, MonadMods, List}]} ->
          % lc = list comprehension.
          rewrite_do_lc(List, LineNo5, {value, MonadMods}, Expr, ParsedInfo);

        {return, 1, [_]} ->
          Msg = "return/1 is currently not supported",
          ParsedInfo_2 = report_error(Expr, Msg, ParsedInfo),
          {Expr, ParsedInfo_2};

        _X ->
          Msg = "invalid doyer syntax",
          ParsedInfo_2 = report_error(Expr, Msg, ParsedInfo),
          {Expr, ParsedInfo_2}
      end;
    _ ->
      % normal call. apply recursively.
      {Args_2, ParsedInfo_2} = lists:foldl(fun(E1, {L1, P1}) ->
        {E2, P2} = rewrite_expr(E1,P1),
        { [E2|L1], P2 }
      end, {[], ParsedInfo}, Args),
      Args_3 = lists:reverse(Args_2),
      Expr_2 = {call, LineNo1, FunLoc, Args_3},
      {Expr_2, ParsedInfo_2}
  end;

rewrite_expr({match, LineNo, Var, Val}, ParsedInfo) ->
  {Var_2, ParsedInfo_2} = rewrite_expr(Var, ParsedInfo),
  {Val_2, ParsedInfo_3} = rewrite_expr(Val, ParsedInfo_2),
  Expr_2 = {match, LineNo, Var_2, Val_2},
  {Expr_2, ParsedInfo_3};

rewrite_expr({'fun', LineNo, {clauses, Clauses}}, ParsedInfo) ->
  {Clauses_2, ParsedInfo_2} = rewrite_clauses(Clauses, ParsedInfo),
  Fun = {'fun', LineNo, {clauses, Clauses_2}},
  {Fun, ParsedInfo_2};

rewrite_expr({'case', LineNo, Val, Clauses}, ParsedInfo) ->
  {Clauses_2, ParsedInfo_2} = rewrite_clauses(Clauses, ParsedInfo),
  Case = {'case', LineNo, Val, Clauses_2},
  {Case, ParsedInfo_2};

rewrite_expr(Expr, ParsedInfo) ->
  {Expr, ParsedInfo}.

% ----------------------------------------------------------------------------
rewrite_do_list(List, MonadMods, Expr, ParsedInfo) ->
  case decode_list_shallowly(List) of
    {ok, List_2} ->
      LineNo = form_lineno(List),
      rewrite_do_2(List_2, LineNo, MonadMods, Expr, ParsedInfo);
    error ->
      Msg = "invalid doyer syntax (do/list)",
      ParsedInfo_2 = report_error(Expr, Msg, ParsedInfo),
      {Expr, ParsedInfo_2}
  end.

rewrite_do_tuple(List, LineNo, MonadMods, Expr, ParsedInfo) ->
  % fake #decode_list_shallowly{}.
  List_2 = #decode_list_shallowly {
    nil_lineno = LineNo,
    elements   = [ {form_lineno(Elem), Elem} || Elem <- List ]
  },
  rewrite_do_2(List_2, LineNo, MonadMods, Expr, ParsedInfo).

rewrite_do_block(List, LineNo, MonadMods, Expr, ParsedInfo) ->
  % fake #decode_list_shallowly{}.
  List_2 = #decode_list_shallowly {
    nil_lineno = LineNo,
    elements   = [ {form_lineno(Elem), Elem} || Elem <- List ]
  },
  rewrite_do_2(List_2, LineNo, MonadMods, Expr, ParsedInfo).

rewrite_do_lc(List, LineNo, MonadMods, Expr, ParsedInfo) ->
  % fake #decode_list_shallowly{}.
  List_2 = #decode_list_shallowly {
    nil_lineno = LineNo,
    elements   = [ {form_lineno(Elem), Elem} || Elem <- List ]
  },
  rewrite_do_2(List_2, LineNo, MonadMods, Expr, ParsedInfo).

% ----------------------------------------------------------------------------
rewrite_do_2(List, LineNo, MonadMods, Expr, ParsedInfo) ->
  case determise_monad_mods(MonadMods, ParsedInfo) of
    {ok, MonadMods_2} ->
      rewrite_do_3(List, LineNo, MonadMods_2, Expr, ParsedInfo);
    error ->
      Msg = "invalid doyer syntax (do/monadmods)",
      ParsedInfo_2 = report_error(Expr, Msg, ParsedInfo),
      {Expr, ParsedInfo_2}
  end.

% ----------------------------------------------------------------------------
-record(rewrite_do_arg, {
  lineno_0    :: lineno(),
  monadmods   :: [module()],
  expr        :: abstract_form(),
  parsed_info :: #parsed_info{}
}).

rewrite_do_3(List, LineNo, MonadMods, Expr, ParsedInfo) ->
  case List#decode_list_shallowly.elements of
    [] ->
      Msg = "no code to execute",
      ParsedInfo_2 = report_error(Expr, Msg, ParsedInfo),
      {Expr, ParsedInfo_2};
    [{_ConsLineNo, Form}] ->
      {Form, ParsedInfo};
    List_2 -> % two or more exprs.
      Arg = #rewrite_do_arg {
        lineno_0    = LineNo,
        monadmods   = MonadMods,
        expr        = Expr,
        parsed_info = ParsedInfo
      },
      rewrite_do_4(List_2, Arg)
  end.

rewrite_do_4(List, Arg) ->
  % List_2 ::= [ {Var,Val,ConsLineNo} ].
  List_2 = [
    case Form_2 of
      % A = B.
      {match,_LineNo,Var_2,Val_2} ->
        {{value,Var_2}, Val_2, ConsLineNo_2};
      % A <- B.
      {generate,_LineNo,Var_2,Val_2} ->
        {{value,Var_2}, Val_2, ConsLineNo_2};
      _ ->
        {none, Form_2, ConsLineNo_2}
    end
  ||
    {ConsLineNo_2, Form_2} <- List
  ],
  rewrite_do_5(List_2, Arg).

rewrite_do_5(List, Arg) ->
  % cascading bind variables.
  {Var_3,List_3_Rev} = lists:foldl(fun(Elem, AccIn) ->
    {CurVar, CurVal, ConsLineNo_3} = Elem,
    {PrevVar,ListIn} = AccIn,
    ListOut = [ {PrevVar, CurVal, ConsLineNo_3} | ListIn ],
    {CurVar, ListOut}
  end, {none, []}, List),

  case Var_3 of
    none ->
      rewrite_do_6(List_3_Rev, Arg);
    {value, Var_7} ->
      ParsedInfo = Arg#rewrite_do_arg.parsed_info,
      Msg = "last expression in do-notation never have bind variable",
      ParsedInfo_2 = report_error(Var_7, Msg, ParsedInfo),
      Expr = Arg#rewrite_do_arg.expr,
      {Expr, ParsedInfo_2}
  end.

% nested apply.
rewrite_do_6(List_3_Rev, Arg) ->
  ParsedInfo = Arg#rewrite_do_arg.parsed_info,
  {List_3, ParsedInfo_2} = lists:foldl(fun(Elem, AccIn) ->
    {Var_3_2, Form_3_2, LineNo_3_2} = Elem,
    {ListIn, ParsedInfoIn} = AccIn,
    {FormOut, ParsedInfoOut} = rewrite_expr(Form_3_2, ParsedInfoIn),
    ElemOut = {Var_3_2, FormOut, LineNo_3_2},
    { [ ElemOut | ListIn ], ParsedInfoOut }
  end, {[], ParsedInfo}, List_3_Rev),
  Arg_2 = Arg#rewrite_do_arg {
    parsed_info = ParsedInfo_2
  },
  rewrite_do_7(List_3, Arg_2).

% upgrade expr into fun.
rewrite_do_7(List_3, Arg) ->
  LineNo_0 = Arg#rewrite_do_arg.lineno_0,
  MonadMods = Arg#rewrite_do_arg.monadmods,
  MonadMods_Form = lists:foldl(fun(MonadMod, AccIn) ->
    Form_6 = {atom, LineNo_0, MonadMod},
    {cons, LineNo_0, Form_6, AccIn}
  end, {nil, LineNo_0}, lists:reverse(MonadMods)),

  RevList = lists:reverse(List_3),
  [Last | RevRest] = RevList,

  Ret = lists:foldl(fun(Elem, AccIn) ->
    {Var_1, Val, ConsLineNo} = Elem,

    {PrevVar, PrevVal, PrevConsLineNo} = AccIn,
    Var_3 = case PrevVar of
      {value, Var_2} ->
        Var_2;
      none ->
        {var,PrevConsLineNo,'_'}
    end,
    Clause = {clause,PrevConsLineNo,[Var_3],[],[PrevVal]},
    Fun = {'fun', PrevConsLineNo, {clauses,[Clause]} },

    Call = mk_call_form(PrevConsLineNo, ?EXECMOD, bind, [Val, Fun, MonadMods_Form]),
    {Var_1, Call, ConsLineNo}
  end, Last, RevRest),

  {_Var, DoExpr, _ConsLineNo} = Ret,
  rewrite_do_8(DoExpr, Arg).
  
rewrite_do_8(DoExpr, Arg) ->
  ParsedInfo = Arg#rewrite_do_arg.parsed_info,
  {DoExpr, ParsedInfo}.


% ----------------------------------------------------------------------------
mk_call_form(LineNo, Mod, Fun, Args) ->
  ModAtom = {atom, LineNo, Mod},
  FunAtom = {atom, LineNo, Fun},
  {call, LineNo, {remote, LineNo, ModAtom, FunAtom}, Args}.

% ----------------------------------------------------------------------------
form_lineno(Form) ->
  element(2, Form).
% ----------------------------------------------------------------------------
determise_monad_mods(none, ParsedInfo) ->
  {ok, ParsedInfo#parsed_info.monadmods};
determise_monad_mods({value, {var, _LineNo, '_'}}, _ParsedInfo) ->
  % special case (var _).
  {ok, default_monadmods()};
determise_monad_mods({value, {atom, _LineNo, '_'}}, _ParsedInfo) ->
  % special case (atom '_').
  {ok, default_monadmods()};
determise_monad_mods({value, {atom, _LineNo, MonadMod}}, _ParsedInfo) ->
  {ok, [MonadMod]};
determise_monad_mods({value, {cons, _LineNo, _Hd, _Tl}=MonadMods}, _ParsedInfo) ->
  case decode_list_shallowly(MonadMods) of
    {ok, List} ->
      determise_monad_mods_2(List);
    error ->
      error
  end;
determise_monad_mods({value, {call, _LineNo1, {remote, _LineNo2, {atom, _LineNo3, ?FAKEMOD}, {atom, _LineNo4, default_monad_selector}}, []}}, _ParsedInfo) ->
  {ok, default_monadmods()};
determise_monad_mods({value, _}, _ParsedInfo) ->
  error.

determise_monad_mods_2(List) ->
  lists:foldl(
    fun
      (Elem, {ok, List_2}) ->
        case Elem of
          {_ConsLineNo, {atom, _LineNo, MonadMod}} ->
            {ok, [ MonadMod | List_2 ]};
          _ ->
            error
        end;
      (_, _) ->
        error
    end,
    {ok, []},
    List#decode_list_shallowly.elements
  ).

% ----------------------------------------------------------------------------
report_error(File, LineNo, Msg, ParsedInfo) ->
  ErrDesc      = #error_descriptor { msg = Msg },
  Error        = {File, [{LineNo, ?MODULE, ErrDesc}]},
  ErrorList    = ParsedInfo#parsed_info.errors,
  ParsedInfo_2 = ParsedInfo#parsed_info {
    errors = [Error | ErrorList]
  },
  ParsedInfo_2.

report_error(Form, Msg, ParsedInfo) ->
  File   = ParsedInfo#parsed_info.file,
  LineNo = form_lineno(Form),
  ParsedInfo_2 = report_error(File, LineNo, Msg, ParsedInfo),
  ParsedInfo_2.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
