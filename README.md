
erlang-doyer
============

Do-notation in Erlang.

see exapmles/exapmlesest.erl as a sample code.

DESCRIPTION
-----------

under construction.


BUILD AND INSTALL
-----------------

small Makefile is contained, just type make.

 $ make
 $ make check
 $ make install

DESTDIR also works.


SYNTAX
------

1. 

  ?do([
    _X = {value, 10}, % ?return(10).
    _Y = {value, 20},
    %?return(_X+_Y),
    {value, 100},
    {value, {_X,_Y}}
  ]).
  
  ?do(monadmod, [
    ..
  ]).

  ?do([m1, m2, ...], [
    ..
  ]).

putting '_' for monadmod gets same behavior as ?do([..]).

  % same as ?do([..]).
  ?do('_', [
    ..
  ]).

2. begin .. end block version of (1).

  ?do(begin
    ..
  end).

  ?do(MonadMod, begin
    ..
  end).

3. tuple representation version of (1).

  ?do({
    ..
  }).

  ?do(MonadMod, {
    ..
  }).

4.

  ?do([
    MonadMod
  ||
    X <- doyer_state:return(1),
    Y <- doyer_state:state(fun(_Seed) ->
      io:format(".. seed=~p~n",[_Seed]),
      {2,xx}
    end),
    doyer_state:return({X,Y})
  ]).


criteria of selection monadmod
------------------------------

currently, monadmod is selected by left hand value of bind.
and it is occured at each invocation of bind.
but these are not well as monadic behavior.


(ja) 現在のmonad選択基準::

bindの左辺値が先に確定するので, その値をMod:can_bind/1に渡し,
trueが返ってきたらそのモジュールのbindを呼んでいる.
つまり実行時に動的に決定されている.

returnはそれが要求されるコンテキストが必要になるため実装未定.

コンパイル時に一意に定めるように変更予定.
monadmod からインスタンス構造を示すようにして,
値がそれを満たすかを静的に検証?
判定しきれないことの方がおおそうなので?do_runtime()とかににげるかも..
確定できなければかならず１つを指定にしてそれを使うにする?
実行するまで動くかはわからないけどそこはやっぱりhaskellじゃないし..?
１つに確定することができればreturnを使うことは可能に.

% ----------------------------------------------------------------------------
% End of File.
% ----------------------------------------------------------------------------
