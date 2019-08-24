%%---------------------------------------------------------------------------
%% From: Per Hedeland <per@hedeland.org>
%% Date: 11 Feb 2010
%%
%% The code below demonstrates a bug in dialyzer - it produces the warning:
%%	Clause guard cannot succeed.
%%	The variable Cs was matched against the type any()
%% for the first test/1 clause, but of course the claim can easily be easily
%% refuted by calling test(#cs{}).
%%---------------------------------------------------------------------------

-module(or_bug).

-export([test/1]).

-record(cs, {children = [], actions = []}).

-define(is_internal(X), ((X#cs.children =/= []) or
                         (X#cs.actions =/= []))).
-define(has_children(X), (X#cs.children /= [])).

test(Cs) when not ?is_internal(Cs)  -> foo;
test(Cs) when not ?has_children(Cs) -> bar;
test(Cs) when Cs#cs.children =/= [] -> baz.
