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

%% GH-6474. The compiler would go into an infinite loop.

bc_infinite_loop() ->
    mutually_recursive(<<0 || false>>).

mutually_recursive(X) ->
    %% This LC will be implemented as mutually recursive functions.
    %% Analyzing them would cause an infinite loop.
    [0 || _ <- [], <<_>> <= X].
