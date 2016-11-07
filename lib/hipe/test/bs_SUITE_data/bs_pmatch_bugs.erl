%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
-module(bs_pmatch_bugs).

-export([test/0]).

test() ->
  Bin = <<"123.123">>,
  <<49,50,51>> = lex_digits1(Bin, 1, []),
  <<49,50,51>> = lex_digits2(Bin, 1, []),
  ok = var_bind_bug(<<1, 2, 3, 4, 5, 6, 7, 8>>),
  ok = bs_match_string_bug(),
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

%%--------------------------------------------------------------------
%% From: Andreas Schultz
%% Date: 2/11/2016
%%
%% Either HiPE is messing up binary matches in some cases or I'm not
%% seeing the problem. ... <SNIP PROGRAM - CLEANED UP VERSION BELOW>
%% With Erlang 19.1.3 the HiPE compiled version behaves differently
%% than the non-HiPE version: ... <SNIP TEST RUNS>
%% So, do I do something wrong here or is this a legitimate HiPE bug?
%%
%% Yes, this was a legitimate HiPE bug: The BEAM to ICode tranaslation
%% of the bs_match_string instruction, written long ago for binaries
%% (i.e., with byte-sized strings), tried to do a `clever' translation
%% of even bit-sized strings using a HiPE primop that took a `Size'
%% argument expressed in *bytes*. ICode is not really the place to do
%% such a thing, and moreover there is really no reason for the HiPE
%% primop not to take a Size argument expressed in *bits* instead.
%% The bug was fixed by changing the `Size' argument to be in bits,
%% postponing the translation of the bs_match_string primop until RTL
%% and doing a proper translation using bit-sized quantities there.
%%--------------------------------------------------------------------

bs_match_string_bug() ->
  ok = test0(<<50>>),
  Bin = data(),
  ok = test1(Bin),
  ok = test2(Bin),
  ok.

%% Minimal test case showing the problem matching with strings
test0(<<6:5, 0:1, 0:2>>) -> weird;
test0(<<6:5, _:1, _:2>>) -> ok;
test0(_) -> default.

data() -> <<50,16,0>>.

%% This was the problematic test case in HiPE: 'default' was returned
test1(<<1:3, 1:1, _:1, 0:1, 0:1, 0:1, _/binary>>) -> weird;
test1(<<1:3, 1:1, _:1, _:1, _:1, _:1, _/binary>>) -> ok;
test1(_) -> default.

%% This variation of test1/1 above worked OK, even in HiPE
test2(<<1:3, 1:1, _:1, A:1, B:1, C:1, _/binary>>)
  when A =:= 1; B =:= 1; C =:= 1 -> ok;
test2(<<1:3, 1:1, _:1, 0:1, 0:1, 0:1, _/binary>>) -> weird;
test2(_) -> default.
