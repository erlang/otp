%% Same as ./type_specs.erl, but without macros.
-module(igor_type_specs).

-include_lib("syntax_tools/include/merl.hrl").

-export([f/1, b/0, c/2]).

-export_type([t/0, ot/2, ff2/0]).

-type aa() :: _.

-type t() :: integer().

-type ff(A) :: ot(A, A) | tuple() | 1..3 | map() | {}.
-type ff1() :: ff(bin()) | foo:bar().
-type ff2() :: {list(), [_], list(integer()),
                nonempty_list(), nonempty_list(atom()), [ff1(), ...],
                nil(), []}.
-type bin() :: <<>>
             | <<_:(+4)>>
             | <<_:_*8>>
             | <<_:12, _:_*16>>
             | <<_:16, _:_*(0)>> % same as "<<_:16>>"
             | <<_:16, _:_*(+0)>>.

-callback cb() -> t().

-optional_callbacks([cb/0]).

-opaque ot(A, B) :: {A, B}.

-type f1() :: fun().
-type f2() :: fun((...) -> t()).
-type f3() :: fun(() -> t()).
-type f4() :: fun((t(), t()) -> t()).

-wild(attribute).

-record(par, {a :: undefined | igor_type_specs}).

-record(r0, {}).

-record(r,
        {f1 :: integer(),
         f2 = a :: atom(),
         f3 :: fun(),
         f4 = 7}).

-type r0() :: #r0{} | #r{f1 :: 3} | #r{f1 :: 3, f2 :: 'sju'}.

-type m1() :: #{}.
-type m2() :: #{a => m1(), b => #{} | fy:m2()}.
-type b1() :: B1 :: binary() | (BitString :: bitstring()).

-define(PAIR(A, B), {(A), (B)}).

-spec igor_type_specs:f({r0(), r0()}) -> {t(), t()}.

f({R, R}) ->
    _ = "igor_type_specs" ++ "hej",
    _ = <<"foo">>,
    _ = R#r.f1,
    _ = R#r{f1 = 17, f2 = b},
    {1, 1}.

-spec igor_type_specs:b() -> integer() | fun().

b() ->
    case foo:bar() of
        #{a := 2} -> 19
    end.

-spec c(Atom :: atom(), Integer :: integer()) -> {atom(), integer()};
       (X, Y) -> {atom(), float()} when X :: atom(),
                                        is_subtype(Y, float());
       (integer(), atom()) -> {integer(), atom()}.

c(A, B) ->
    _ = integer,
    {A, B}.
