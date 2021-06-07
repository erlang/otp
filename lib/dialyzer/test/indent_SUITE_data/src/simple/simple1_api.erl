-module(simple1_api).

-export([t1/1, adt_t1/1, t2/1, adt_t2/1, tup/0, t3/0, t4/0, t5/0, t6/0, t7/0,
         t8/0, adt_t3/0, adt_t4/0, adt_t7/0, adt_t8/0, adt_t5/0,
         c1/2, c2/2, c2/0, c3/0, c4/0, tt1/0, tt2/0,
         cmp1/0, cmp2/0, cmp3/0, cmp4/0,
         ty_cmp1/0, ty_cmp2/0, ty_cmp3/0, ty_cmp4/0,
         f1/0, f2/0, adt_f1/0, adt_f2/0, f3/0, f4/0, adt_f3/0, adt_f4/0,
         adt_f4_a/0, adt_f4_b/0,
         bool_t1/0, bool_t2/0, bool_t3/0, bool_t4/0, bool_t5/1, bool_t6/1,
         bool_t7/0, bool_adt_t1/0, bool_adt_t2/0, bool_adt_t5/1,
         bool_adt_t6/1, bool_t8/0, bool_adt_t8/2, bool_t9/0, bool_adt_t9/2,
         bit_t1/0, bit_adt_t1/0, bit_t3/1, bit_adt_t2/0, bit_adt_t3/1,
         bit_t5/1, bit_t4/1, bit_adt_t4/1, bit_t5/0, bit_adt_t5/0,
         call_f/1, call_f_adt/1, call_m_adt/1, call_m/1, call_f_i/1,
         call_m_i/1, call_m_adt_i/1, call_f_adt_i/1,
         eq1/0, eq2/0, c5/0, c6/2, c7/2, c8/0]).

%%% Equal opaque types

-export_type([o1/0, o2/0]).

-export_type([d1/0, d2/0]).

-opaque o1() :: a | b | c.

-opaque o2() :: a | b | c.

-export_type([i1/0, i2/0, di1/0, di2/0]).

-export_type([b1/0, b2/0]).

-export_type([bit1/0]).

-export_type([a/0, i/0]).

%% The derived spec is
%%  -spec t1('a' | 'b') -> simple1_api:o1('a') | simple1_api:o2('a').
%% but that is not tested...

t1(a) ->
    o1();
t1(b) ->
    o2().

-spec o1() -> o1().

o1() -> a.

-spec o2() -> o2().

o2() -> a.

%% The derived spec is
%% -spec adt_t1('a' | 'b') -> simple1_adt:o1('a') | simple1_adt:o2('a').
%% but that is not tested...

adt_t1(a) ->
    simple1_adt:o1();
adt_t1(b) ->
    simple1_adt:o2().

%%% Disjunct opaque types

-opaque d1() :: a | b | c.

-opaque d2() :: d | e | f.

%% -spec t2('a' | 'b') -> simple1_api:d1('a') | simple1_api:d2('d').

t2(a) ->
    d1();
t2(b) ->
    d2().

-spec d1() -> d1().

d1() -> a.

-spec d2() -> d2().

d2() -> d.

%% -spec adt_t2('a' | 'b') -> simple1_adt:d1('a') | simple1_adt:d2('d').

adt_t2(a) ->
    simple1_adt:d1();
adt_t2(b) ->
    simple1_adt:d2().

-spec tup() -> simple1_adt:tuple1(). % invalid type spec

tup() ->
    {a, b}.

%%% Matching equal opaque types with different names

t3() ->
    A = n1(),
    B = n2(),
    A = A, % OK, of course
    A = B. % OK since o1() and o2() are local opaque types

t4() ->
    A = n1(),
    B = n2(),
    true = A =:= A, % OK, of course
    A =:= B. % OK since o1() and o2() are local opaque types

t5() ->
    A = d1(),
    B = d2(),
    A =:= B. % can never evaluate to true

t6() ->
    A = d1(),
    B = d2(),
    A = B. % can never succeed

t7() ->
    A = d1(),
    B = d2(),
    A =/= B. % OK (always true?)

t8() ->
    A = d1(),
    B = d2(),
    A /= B. % OK (always true?)

-spec n1() -> o1().

n1() -> a.

-spec n2() -> o2().

n2() -> a.

adt_t3() ->
    A = simple1_adt:n1(),
    B = simple1_adt:n2(),
    true = A =:= A, % OK.
    A =:= B. % opaque test, not OK

adt_t4() ->
    A = simple1_adt:n1(),
    B = simple1_adt:n2(),
    A = A, % OK
    A = B. % opaque terms

adt_t7() ->
    A = simple1_adt:n1(),
    B = simple1_adt:n2(),
    false = A =/= A, % OK
    A =/= B. % opaque test, not OK

adt_t8() ->
    A = simple1_adt:n1(),
    B = simple1_adt:n2(),
    false = A /= A, % OK
    A /= B. % opaque test, not OK

adt_t5() ->
    A = simple1_adt:c1(),
    B = simple1_adt:c2(),
    A =:= B. % opaque test, not OK

%% Comparison in guard

-spec c1(simple1_adt:d1(), simple1_adt:d2()) -> boolean().

c1(A, B) when A =< B -> true. % succ type of A and B is any() (type spec is OK)

-spec c2(simple1_adt:d1(), simple1_adt:d2()) -> boolean().

c2(A, B) ->
    if A =< B -> true end. % succ type of A and B is any() (type spec is OK)

c2() ->
    A = simple1_adt:d1(),
    B = simple1_adt:d2(),
    if A =< B -> ok end. % opaque terms

c3() ->
    B = simple1_adt:d2(),
    if a =< B -> ok end. % opaque term

c4() ->
    A = simple1_adt:d1(),
    if A =< d -> ok end. % opaque term

tt1() ->
    A = o1(),
    is_integer(A). % OK

tt2() ->
    A = simple1_adt:d1(),
    is_integer(A). % breaks the opacity

%% Comparison with integers

-opaque i1() :: 1 | 2.

-opaque i2() :: 1 | 2.

-opaque di1() :: 1 | 2.

-opaque di2() :: 3 | 4.

-spec i1() -> i1().

i1() -> 1.

-type ty_i1() :: 1 | 2.

-spec ty_i1() -> ty_i1().

ty_i1() -> 1.

cmp1() ->
    A = i1(),
    if A > 3 -> ok end. % can never succeed

cmp2() ->
    A = simple1_adt:i1(),
    if A > 3 -> ok end. % opaque term

cmp3() ->
    A = i1(),
    if A < 3 -> ok end.

cmp4() ->
    A = simple1_adt:i1(),
    if A < 3 -> ok end. % opaque term

%% -type

ty_cmp1() ->
    A = ty_i1(),
    if A > 3 -> ok end. % can never succeed

ty_cmp2() ->
    A = simple1_adt:ty_i1(),
    if A > 3 -> ok end. % can never succeed

ty_cmp3() ->
    A = ty_i1(),
    if A < 3 -> ok end.

ty_cmp4() ->
    A = simple1_adt:ty_i1(),
    if A < 3 -> ok end.

%% is_function

f1() ->
    T = n1(),
    if is_function(T) -> ok end. % can never succeed

f2() ->
    T = n1(),
    is_function(T). % ok

adt_f1() ->
    T = simple1_adt:n1(),
    if is_function(T) -> ok end. % breaks the opacity

adt_f2() ->
    T = simple1_adt:n1(),
    is_function(T). % breaks the opacity

f3() ->
    A = i1(),
    T = n1(),
    if is_function(T, A) -> ok end. % can never succeed

f4() ->
    A = i1(),
    T = n1(),
    is_function(T, A). % ok

adt_f3() ->
    A = simple1_adt:i1(),
    T = simple1_adt:n1(),
    if is_function(T, A) -> ok end. % breaks the opacity

adt_f4() ->
    A = simple1_adt:i1(),
    T = simple1_adt:n1(),
    is_function(T, A). % breaks the opacity

adt_f4_a() ->
    A = simple1_adt:i1(),
    T = n1(),
    is_function(T, A). % opaque term


adt_f4_b() ->
    A = i1(),
    T = simple1_adt:n1(),
    is_function(T, A). % breaks the opacity

%% A few Boolean examples

bool_t1() ->
    B = b2(),
    if B -> ok end. % B =:= true can never succeed

bool_t2() ->
    A = b1(),
    B = b2(),
    if A and not B -> ok end.

bool_t3() ->
    A = b1(),
    if not A -> ok end. % can never succeed

bool_t4() ->
    A = n1(),
    if not ((A >= 1) and not (A < 1)) -> ok end. % can never succeed

-spec bool_t5(i1()) -> integer().

bool_t5(A) ->
    if [not (A > 1)] =:=
       [false]-> 1 end.

-spec bool_t6(b1()) -> integer().

bool_t6(A) ->
    if [not A] =:=
       [false]-> 1 end.

-spec bool_t7() -> integer().

bool_t7() ->
    A = i1(),
    if [not A] =:= % cannot succeed
       [false]-> 1 end.

bool_adt_t1() ->
    B = simple1_adt:b2(),
    if B -> ok end. % opaque term

bool_adt_t2() ->
    A = simple1_adt:b1(),
    B = simple1_adt:b2(),
    if A and not B -> ok end. % opaque term

-spec bool_adt_t5(simple1_adt:i1()) -> integer().

bool_adt_t5(A) ->
    if [not (A > 1)] =:= % succ type of A is any() (type spec is OK)
       [false]-> 1 end.

-spec bool_adt_t6(simple1_adt:b1()) -> integer(). % invalid type spec

bool_adt_t6(A) ->
    if [not A] =:= % succ type of A is 'true'
       [false]-> 1 end.

-spec bool_t8() -> integer().

bool_t8() ->
    A = i1(),
    if [A and A] =:= % cannot succeed
       [false]-> 1 end.

-spec bool_adt_t8(simple1_adt:b1(), simple1_adt:b2()) -> integer(). % invalid

bool_adt_t8(A, B) ->
    if [A and B] =:=
       [false]-> 1 end.

-spec bool_t9() -> integer().

bool_t9() ->
    A = i1(),
    if [A or A] =:= % cannot succeed
       [false]-> 1 end.

-spec bool_adt_t9(simple1_adt:b1(), simple1_adt:b2()) -> integer(). % invalid

bool_adt_t9(A, B) ->
    if [A or B] =:=
       [false]-> 1 end.

-opaque b1() :: boolean().

-opaque b2() :: boolean().

-spec b1() -> b1().

b1() -> true.

-spec b2() -> b2().

b2() -> false.

%% Few (very few...) examples with bit syntax

bit_t1() ->
    A = i1(),
    <<100:(A)>>.

bit_adt_t1() ->
    A = simple1_adt:i1(),
    <<100:(A)>>. % breaks the opacity

bit_t3(A) ->
    B = i1(),
    case none:none() of
        <<A:B>> -> 1
    end.

bit_adt_t2() ->
    A = simple1_adt:i1(),
    case <<"hej">> of
        <<_:A>> -> ok % breaks the opacity (but the message is strange)
    end.


bit_adt_t3(A) ->
    B = simple1_adt:i1(),
    case none:none() of
        <<A:  % breaks the opacity (the message is less than perfect)
          B>> -> 1
    end.

bit_t5(A) ->
    B = o1(),
    case none:none() of % the type is any(); should fix that XXX
        <<A:B>> -> 1 % can never match (local opaque type is OK)
    end.

-spec bit_t4(<<_:1>>) -> integer().

bit_t4(A) ->
    Sz = i1(),
    case A of
        <<_:Sz>> -> 1
    end.

-spec bit_adt_t4(<<_:1>>) -> integer().

bit_adt_t4(A) ->
    Sz = simple1_adt:i1(),
    case A of
        <<_:Sz>> -> 1 % breaks the opacity
    end.

bit_t5() ->
    A = bit1(),
    case A of
        <<_/binary>> -> 1
    end.

bit_adt_t5() ->
    A = simple1_adt:bit1(),
    case A of
        <<_/binary>> -> 1 % breaks the opacity
    end.

-opaque bit1() :: binary().

-spec bit1() -> bit1().

bit1() ->
    <<"hej">>.

%% Calls with variable module or function

call_f(A) ->
    A = a(),
    foo:A(A).

call_f_adt(A) ->
    A = simple1_adt:a(),
    foo:A(A). % breaks the opacity

call_m(A) ->
    A = a(),
    A:foo(A).

call_m_adt(A) ->
    A = simple1_adt:a(),
    A:foo(A). % breaks the opacity

-opaque a() :: atom().

-opaque i() :: integer().

-spec a() -> a().

a() ->
    e.

call_f_i(A) ->
    A = i(),
    foo:A(A). % A is not atom() but i()

call_f_adt_i(A) ->
    A = simple1_adt:i(),
    foo:A(A). % A is not atom() but simple1_adt:i()

call_m_i(A) ->
    A = i(),
    A:foo(A). % A is not atom() but i()

call_m_adt_i(A) ->
    A = simple1_adt:i(),
    A:foo(A). % A is not atom() but simple1_adt:i()

-spec eq1() -> integer().

eq1() ->
    A = simple1_adt:d2(),
    B = simple1_adt:d1(),
    if
        A == B -> % opaque terms
            0;
        A == A ->
            1;
        A =:= A -> % compiler finds this one cannot match
            2;
        true -> % compiler finds this one cannot match
            3
    end.

eq2() ->
    A = simple1_adt:d1(),
    if
        {A} >= {A} ->
            1;
        A >= 3 -> % opaque term
            2;
        A == 3 -> % opaque term
            3;
        A =:= 3 -> % opaque term
            4;
        A == A ->
            5;
        A =:= A -> % compiler finds this one cannot match
            6
    end.

c5() ->
    A = simple1_adt:d1(),
    A < 3. % opaque term

c6(A, B) ->
    A = simple1_adt:d1(),
    B = simple1_adt:d1(),
    A =< B. % same type - no warning

c7(A, B) ->
    A = simple1_adt:d1(),
    B = simple1_adt:d2(),
    A =< B. % opaque terms

c8() ->
    D = digraph:new(),
    E = ets:new(foo, []),
    if {D, a} > {D, E} -> true; % OK
       {1.0, 2} > {{D}, {E}} -> true; % OK
       {D, 3} > {D, E} -> true  % opaque term 2
    end.

-spec i() -> i().

i() ->
    1.
