-module(literals).

%% Bad records inside structures used to be ignored. The reason:
%% v3_core:unfold() does not annotate the parts of a literal.
%% This example does not work perfectly yet.

-export([t1/0, t2/0, t3/0, t4/0, m1/1, m2/1, m3/1, m4/1]).

-record(r, {id :: integer}).

t1() ->
    #r{id = a}. % violation

t2() ->
    [#r{id = a}]. % violation

t3() ->
    {#r{id = a}}. % violation

t4() ->
    #{a => #r{id = a}}. % violation

m1(#r{id = a}) -> % violation
    ok.

m2([#r{id = a}]) -> % violation
    ok.

m3({#r{id = a}}) -> % can never match; not so good
    ok.

m4(#{a := #r{id = a}}) -> % violation
    ok.
