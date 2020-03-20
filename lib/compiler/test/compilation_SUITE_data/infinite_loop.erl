-module(infinite_loop).
-compile([export_all]).

?MODULE() ->
    ok.

%% When a series of calls initiated by a fun went into an infinite loop,
%% the compiler went into an infinite loop too.
foo() ->
    fun bar/0().

bar() ->
    baz().

baz() ->
    bar().

foobar() ->
    fun barfoo/0().

barfoo() ->
    barfoo().
