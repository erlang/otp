-module(overloaded).

-export([t/1, v/1]).

-export([over/1]).

-spec t(a | b) -> term().

t(A) ->
    u(A).

-spec u(a | b | c) -> term().

u(X) ->
    X.

-spec v(_) -> ok.

v(A) ->
    x(A).

-spec x(_) -> {ok, term} | ok.

x(a) ->
    {ok, term};
x(b) ->
    ok.

-spec over(A) -> ffyy1 when A :: a | b;
          (B) -> {ffyy2, integer()} when B :: c | d.

over(a) ->
    ffyy1;
over(c) ->
    {ffyy2, 1};
over(d) ->
    ffyy3.
