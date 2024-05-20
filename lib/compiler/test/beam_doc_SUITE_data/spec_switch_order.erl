-module(spec_switch_order).

-export([foo/1, bar/1, other/0, not_false/0]).

-spec foo(integer()) -> mytype().



-doc "
bar(Var)
".
-spec bar(integer()) -> another_type().




-type mytype() :: ok.
-type another_type() :: ok.



-doc "
foo(Var)

Foo does X
".
foo(_X) ->
    ok.

-doc hidden.
bar(_X) ->
    ok.


%% Ordering issue
-doc false.
-spec other() -> ok.
%% docs #{ {other, 0} => {hidden, none, #{}}
%% need to reset fields

-spec not_false() -> ok.
%% create entry with current fields
%% reset state

-doc #{}.
other() ->
    ok.
%% fetch or create entry with for other
%% update unset fields
%% we need to differenciate between unset and set with default

not_false() ->
    ok.
