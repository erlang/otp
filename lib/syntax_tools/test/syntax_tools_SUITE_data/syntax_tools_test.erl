%%
%% File:    syntax_tools_test.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-10-23
%%

-module(syntax_tools_test).

-export([foo1/0,foo2/2,foo3/0,foo4/3,foo5/1]).

-include_lib("kernel/include/file.hrl").
-record(state, { a, b, c, d}).
-attribute([foo/0]).

-define(attrib, some_attrib).

-?attrib([foo2/2]).

-define(macro_simple1, ok).
-define(MACRO_SIMPLE2, (other)).
-define(macro_simple3, ?MODULE).
-define(macro_simple4, [?macro_simple3,?MODULE,?MACRO_SIMPLE2]).
-define(macro_simple5, (process_info)).
-define(macro_string, "hello world").
-define(macro_argument1(X), (X + 3)).
-define(macro_argument2(X,Y), (X + 3 * Y)).
-define(macro_block(X), begin X end).
-define(macro_if(X1,X2), if X1 -> X2; true -> none end).


-ifdef(macro_def1).
-define(macro_cond1, yep).
-else.
-define(macro_cond1, nope).
-endif.
-ifndef(macro_def2).
-define(macro_cond2, nope).
-else.
-define(macro_cond2, yep).
-endif.
-undef(macro_def1).
-undef(macro_def2).

%% basic test
foo1() ->
    ok.

%% macro test
foo2(A,B) ->
    % string combining ?
    [?macro_string, ?macro_string
     ?macro_string,
     "hello world "
     "more hello",
     [?macro_simple1,
      ?MACRO_SIMPLE2,
      ?macro_simple3,
      ?macro_simple4,
      ?macro_simple5,
      ?macro_string,
      ?macro_cond1,
      ?macro_cond2,
      ?macro_block(A),
      ?macro_if(A,B),
      ?macro_argument1(A),
      ?macro_argument1(begin A end),
      ?macro_block(<<"hello">>),
      ?macro_block("hello"),
      ?macro_block([$h,$e,$l,$l,$0]),
      ?macro_argument1(id(<<"hello">>)),
      ?macro_argument1(if A -> B; true -> 3.14 end),
      ?macro_argument1(case A of ok -> B; C -> C end),
      ?macro_argument1(receive M -> M after 100 -> 3 end),
      ?macro_argument1(try foo5(A) catch C:?macro_simple5 -> {C,B} end),
      ?macro_argument2(A,B)],
     A,B,ok].

id(I) -> I.
%% basic terms

foo3() ->
    [atom,
     'some other atom',
     {tuple,1,2,3},
     1,2,3,3333,
     3,3333,2,1,
     [$a,$b,$c],
     "hello world",
     <<"hello world">>,
     <<1,2,3,4,5:6>>,
     3.1415,
     1.03e33].

%% application and records

foo4(A,B,#state{c = C}=S) ->
    Ls = foo3(),
    S1 = #state{ a = 1, b = 2 },
    [foo2(A,Ls),B,C,
     B(3,C),
     erlang:process_info(self()),
     erlang:?macro_simple5(self()),
     A:?MACRO_SIMPLE2(),
     A:?macro_simple1(),
     A:process_info(self()),
     A:B(3),
     S#state{ a = 2, b = B, d = S1 }].

foo5(A) ->
    try foo2(A,A) of
	R -> R
    catch
	error:?macro_simple5 ->
	    nope
    end.
