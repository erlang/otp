%%
%% File:    m2.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-10-24
%%

-module(m2).


-export([foo/0,foo/1,foo/2,
	 bar/0,bar/1,bar/2,
	 record_update/2, record/0]).

foo() -> ok.
foo(A) -> [item,A].
foo(A,B) -> A + B.

bar() -> true.
bar(A) -> {element,A}.
bar(A,B) -> A*B.

-record(rec, {a,b}).

record() -> #rec{a=3,b=0}.
record_update(V,#rec{a=V0}=R) ->
    R#rec{a=V0+V,b=V0}.
