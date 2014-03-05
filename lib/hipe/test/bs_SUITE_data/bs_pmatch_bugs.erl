%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
-module(bs_pmatch_bugs).

-export([test/0]).

test() ->
  Bin = <<"123.123">>,
  <<49,50,51>> = lex_digits1(Bin, 1, []),
  <<49,50,51>> = lex_digits2(Bin, 1, []),
  ok = var_bind_bug(<<1, 2, 3, 4, 5, 6, 7, 8>>),
  ok.

%%--------------------------------------------------------------------
%% One of the lex_digits functions below gave incorrect results due to
%% incorrect pattern matching compilation of binaries by the byte code
%% compiler. Fixed by Bjorn Gustavsson on 5/3/2003.
%% --------------------------------------------------------------------
lex_digits1(<<$., Rest/binary>>, _Val, _Acc) ->
  Rest;
lex_digits1(<<N, Rest/binary>>, Val, Acc) when N >= $0, N =< $9 ->
  lex_digits1(Rest, Val * 10 + dec(N), Acc);
lex_digits1(_Other, _Val, _Acc) ->
  not_ok.

lex_digits2(<<N, Rest/binary>>,Val, Acc) when N >= $0, N =< $9 ->
  lex_digits2(Rest, Val * 10 + dec(N), Acc);
lex_digits2(<<$., Rest/binary>>, _Val, _Acc) ->
  Rest;
lex_digits2(_Other, _Val, _Acc) ->
  not_ok.

dec(A) ->
  A - $0.

%%--------------------------------------------------------------------
%% From: Bernard Duggan
%% Date: 11/3/2011
%%
%% I've just run into an interesting little bit of behaviour that
%% doesn't seem quite right.  erlc gives me the warning
%%
%%   43: Warning: this clause cannot match because a previous
%%		  clause at line 42 always matches
%%	 (line 42 is the "B -> wrong;" line).
%%
%% And sure enough, if you run test/0 you get 'wrong' back.
%%
%% That, in itself, is curious to me since by my understanding B should
%% be bound by the function header, and have no guarantee of being the
%% same as A.  I can't see how it could be unbound.
%%
%% Doubly curious, is that if I stop using B as the size specifier of C,
%% like this:
%%
%%   match(<<A:1/binary, B:8/integer, _C:1/binary, _Rest/binary>>) ->
%%
%% the warning goes away.  And the result becomes 'ok' (in spite of
%% nothing in the body having changed, and the only thing changing in
%% the header being the size of an unused variable at the tail of the
%% binary).
%%--------------------------------------------------------------------
var_bind_bug(<<A:1/binary, B:8/integer, _C:B/binary, _Rest/binary>>) ->
  case A of
    B -> wrong;
    _ -> ok
  end.
