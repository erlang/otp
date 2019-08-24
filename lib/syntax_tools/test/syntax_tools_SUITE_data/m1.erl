%%
%% File:    m1.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-10-24
%%

-module(m1).

-export([foo/0,bar/1,baz/2]).

foo() ->
    [m2:foo(),
     m2:bar()].

bar(A) ->
    [m2:foo(A),
     m2:bar(A),
     m2:record_update(3,m2:record())].

baz(A,B) ->
    [m2:foo(A,B),
     m2:bar(A,B)].
