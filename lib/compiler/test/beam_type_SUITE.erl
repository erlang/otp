%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(beam_type_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 integers/1,numbers/1,coverage/1,booleans/1,setelement/1,
         cons/1,tuple/1,
         record_float/1,binary_float/1,float_compare/1,float_overflow/1,
	 arity_checks/1,elixir_binaries/1,find_best/1,
         test_size/1,cover_lists_functions/1,list_append/1,bad_binary_unit/1,
         none_argument/1,success_type_oscillation/1,type_subtraction/1,
         container_subtraction/1,is_list_opt/1,connected_tuple_elements/1,
         switch_fail_inference/1,failures/1,
         cover_maps_functions/1,min_max_mixed_types/1,
         not_equal/1,infer_relops/1,binary_unit/1,premature_concretization/1,
         funs/1,will_succeed/1,float_confusion/1]).

%% Force id/1 to return 'any'.
-export([id/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [integers,
       numbers,
       coverage,
       booleans,
       setelement,
       cons,
       tuple,
       record_float,
       binary_float,
       float_compare,
       float_overflow,
       arity_checks,
       elixir_binaries,
       find_best,
       test_size,
       cover_lists_functions,
       list_append,
       bad_binary_unit,
       none_argument,
       success_type_oscillation,
       type_subtraction,
       container_subtraction,
       is_list_opt,
       connected_tuple_elements,
       switch_fail_inference,
       failures,
       cover_maps_functions,
       min_max_mixed_types,
       not_equal,
       infer_relops,
       binary_unit,
       premature_concretization,
       funs,
       will_succeed,
       float_confusion
      ]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

integers(_Config) ->
    a = do_integers_1(2#11000),
    b = do_integers_1(2#11001),

    a = do_integers_2(<<0:1>>),
    {'EXIT',{{case_clause,-1},_}} = (catch do_integers_2(<<1:1>>)),

    college = do_integers_3(),

    zero = do_integers_4(<<0:1>>, 0),
    one = do_integers_4(<<1:1>>, 0),
    other = do_integers_4(<<1:1>>, 2),

    zero = do_integers_5(0, 0),
    one = do_integers_5(0, 1),
    two = do_integers_5(0, 2),
    three = do_integers_5(0, 3),

    {'EXIT',{badarith,_}} = (catch do_integers_6()),

    house = do_integers_7(),

    {'EXIT',{badarith,_}} = (catch do_integers_8()),

    -693 = do_integers_9(id(7), id(1)),

    3 = do_integers_10(1, 2),
    10 = do_integers_10(-2, -5),

    {'EXIT',{badarith,_}} = catch do_integers_11(42),
    {'EXIT',{badarith,_}} = catch do_integers_11({a,b}),

    {'EXIT',{system_limit,_}} = catch do_integers_12(42),
    {'EXIT',{system_limit,_}} = catch do_integers_12([]),

    {'EXIT',{{badmatch,42},_}} = catch do_integers_13(-43),
    {'EXIT',{{badmatch,0},_}} = catch do_integers_13(-1),
    {'EXIT',{{badmatch,-1},_}} = catch do_integers_13(0),
    {'EXIT',{{badmatch,-18},_}} = catch do_integers_13(17),

    ok.

do_integers_1(B0) ->
    B = B0 band 1,
    case B band 15 of
	0 -> a;
	1 -> b
    end.

do_integers_2(Bin) ->
    <<B:1/signed>> = Bin,
    case B of
	0 -> a;
	1 -> b
    end.

do_integers_3() ->
    case try 0 after [] end of
	0 -> college;
	1 -> 0
    end.

do_integers_4(<<X:1,T/bits>>, C) ->
    %% Binary matching gives the range 0-1 for X.
    %% The range for `X bor C` is unknown. It must not be inherited
    %% from X. (`X bor C` will reuse the register used for X.)
    case X bor C of
        0 -> do_integers_4(T, C, zero);
        1 -> do_integers_4(T, C, one);
        _ -> do_integers_4(T, C, other)
    end.

do_integers_4(_, _, Res) ->
    Res.

do_integers_5(X0, Y0) ->
    %% _X and Y will use the same register.
    _X = X0 band 1,
    Y = Y0 band 3,
    case Y of
        0 -> zero;
        1 -> one;
        2 -> two;
        3 -> three
    end.

do_integers_6() ->
    try b after 1 end band 0.

do_integers_7() ->
    try
        0
        band
        try
            0:any(),
            ok
        catch
            bad_class:_:_ ->
                {tag, "nt"}
        end
    catch
        _:_:_ ->
            house
    end.

do_integers_8() ->
    -1 band ((0 div 0) band 0).

do_integers_9(X, Y) ->
    X * (-100 bor (Y band 1)).

do_integers_10(A, B) when is_integer(A), is_integer(B), A < 2, B < 5 ->
    if
        A < B -> A + B;
        true -> A * B
    end.

do_integers_11(V) ->
    true - V bsl [].

do_integers_12(X) ->
    (1 bsl (1 bsl 100)) + X.

%% GH-6427.
do_integers_13(X) ->
    try do_integers_13_1(<<X>>) of
        _ -> error(should_fail)
    catch
        C:R:_ ->
            try do_integers_13_2(X) of
                _ -> error(should_fail)
            catch
                C:R:_ ->
                    try do_integers_13_3(X) of
                        _ -> error(should_fail)
                    catch
                        C:R:Stk ->
                            erlang:raise(C, R, Stk)
                    end
            end
    end.

do_integers_13_1(<<X>>) ->
    <<(X = bnot X)>>.

do_integers_13_2(X) when is_integer(X), -64 < X, X < 64 ->
    (X = bnot X) + 1.

do_integers_13_3(X) when is_integer(X), -64 < X, X < 64 ->
    X = bnot X,
    X + 1.

numbers(_Config) ->
    Int = id(42),
    true = is_integer(Int),
    true = is_number(Int),
    false = is_float(Int),

    Float = id(42.0),
    true = is_float(Float),
    true = is_number(Float),
    false = is_integer(Float),

    Number = id(1) + id(2),
    true = is_number(Number),
    true = is_integer(Number),
    false = is_float(Number),

    AnotherNumber = id(99.0) + id(1),
    true = is_float(AnotherNumber),
    true = is_number(AnotherNumber),
    false = is_integer(AnotherNumber),

    NotNumber = id(atom),
    true = is_atom(NotNumber),
    false = is_number(NotNumber),
    false = is_integer(NotNumber),
    false = is_float(NotNumber),

    true = is_number(Int),
    true = is_number(Float),
    true = is_number(Number),
    true = is_number(AnotherNumber),

    %% Cover beam_ssa_type:join/2.

    Join1 = case id(a) of
                a -> 3 + id(7);                 %Number.
                b -> id(5) / id(2)              %Float.
            end,
    true = is_integer(Join1),

    Join2 = case id(a) of
                a -> id(5) / 2;                 %Float.
                b -> 3 + id(7)                  %Number.
            end,
    true = is_float(Join2),

    %% Cover beam_ssa_type:meet/2.

    Meet1 = id(0) + -10.0,                       %Float.
    10.0 = abs(Meet1),                           %Number.

    %% Cover code in beam_call_types:beam_bounds_type/3.
    ok = fcmp(0.0, 1.0),
    error = fcmp(1.0, 0.0),

    ok.

fcmp(0.0, 0.0) -> ok;
fcmp(F1, F2) when (F1 - F2) / F2 < 0.0000001 -> ok;
fcmp(_, _) -> error.

coverage(Config) ->
    {'EXIT',{badarith,_}} = (catch id(1) bsl 0.5),
    {'EXIT',{badarith,_}} = (catch id(2.0) bsl 2),
    {'EXIT',{badarith,_}} = (catch a + 0.5),
    {'EXIT',{badarith,_}} = (catch 2.0 * b),

    {'EXIT',{badarith,_}} = (catch id(42.0) / (1 bsl 2000)),

    id(id(42) band 387439739874298734983787934283479243879),
    id(-1 band id(13)),

    error = if
                is_map(Config), is_integer(Config) -> ok;
                true -> error
            end,
    error = if
                is_map(Config), is_atom(Config) -> ok;
                true -> error
            end,
    error = if
                is_map(Config), is_tuple(Config) -> ok;
                true -> error
            end,
    error = if
                is_integer(Config), is_bitstring(Config) -> ok;
                true -> error
            end,

    ok = case Config of
             <<_>> when is_binary(Config) ->
                 impossible;
             [_|_] ->
                 ok
         end,

    %% Cover beam_type:verified_type(none).
    {'EXIT',{badarith,_}} = (catch (id(2) / id(1)) band 16#ff),

    false = fun lot:life/147 == #{},

    {'EXIT',{badarith,_}} = catch coverage_1(),

    {'EXIT',{badarith,_}} = catch coverage_2(),

    {'EXIT',{function_clause,_}} = catch coverage_3("a"),
    {'EXIT',{function_clause,_}} = catch coverage_3("b"),

    Number = id(1),
    if
        0 =< Number, Number < 10 ->
            0 = coverage_4(-1, Number),
            10 = coverage_4(0, Number),
            20 = coverage_4(1, Number),
            30 = coverage_4(2, Number)
    end,

    {'EXIT',{badarg,_}} = catch false ++ true,
    {'EXIT',{badarg,_}} = catch false -- true,

    ok.

coverage_1() ->
    try
        []
    catch
        _:_ ->
            42
    end
    *
    [].

coverage_2() ->
    tl("abc") bsr [].

%% Cover beam_ssa_type:infer_br_value(V, Bool, none).
coverage_3("a" = V) when is_function(V, false) ->
    0.

coverage_4(X, Y) ->
    10 * (X + Y).

booleans(_Config) ->
    {'EXIT',{{case_clause,_},_}} = (catch do_booleans_1(42)),

    ok = do_booleans_2(42, 41),
    error = do_booleans_2(42, 42),

    ok = do_booleans_3(id([]), id(false)),
    error = do_booleans_3(id([]), id(true)),
    error = do_booleans_3(id([a]), id(false)),
    error = do_booleans_3(id([a]), id(true)),

    AnyAtom = id(atom),
    true = is_atom(AnyAtom),
    false = is_boolean(AnyAtom),

    MaybeBool = id('maybe'),
    case MaybeBool of
        true -> ok;
        'maybe' -> ok;
        false -> ok
    end,
    false = is_boolean(MaybeBool),

    NotBool = id(a),
    case NotBool of
        a -> ok;
        b -> ok;
        c -> ok
    end,
    false = is_boolean(NotBool),

    {'EXIT',{{case_clause,false},_}} = catch do_booleans_4(42),
    {'EXIT',{{case_clause,true},_}} = catch do_booleans_4(a),
    {'EXIT',{{case_clause,true},_}} = catch do_booleans_4(false),
    {'EXIT',{{badmatch,true},_}} = catch do_booleans_4(true),

    true = do_booleans_5(id(0), id(<<0>>), id(0)),
    {'EXIT',{function_clause,_}} = catch do_booleans_6(id(0), id(0), id(0)),

    {'EXIT',{{bad_filter,_},_}} = catch do_booleans_7(id(0)),
    {'EXIT',{function_clause,_}} = catch do_booleans_8(id(0)),
    {'EXIT',{{try_clause,_},_}} = catch do_booleans_9(id(0)),

    ok.

do_booleans_1(B) ->
    case is_integer(B) of
	yes -> yes;
	no -> no
    end.

do_booleans_2(A, B) ->
    Not = not do_booleans_cmp(A, B),
    case Not of
        true ->
            case Not of
                true -> error;
                false -> ok
            end;
        false -> ok
    end.

do_booleans_cmp(A, B) -> A > B.

do_booleans_3(NewContent, IsAnchor) ->
    if NewContent == [] andalso not IsAnchor ->
            ok;
       true ->
            error
    end.

do_booleans_4(X) ->
    case is_atom(X) of
        Y when X ->
            false = Y,
            0
    end.

do_booleans_5(X, <<X>>, X) when true; (0 rem 0) ->
    (-2147483648 < X) orelse [0 || _ <- X].

do_booleans_6(X, X, (X = [_ | X])) when true; self() ->
    [0 || _ <- {X}].

%% GH-6603: The boolean optimization pass was clever enough to see that boolean
%% operations like `xor` never failed when both arguments were booleans, but
%% neither the type pass nor the validator could see that.
do_booleans_7(X) ->
    do_booleans_7_a(
        try [0 || true xor (ok =/= ((?MODULE:id([]) ++ []) -- []))] of
            Y ->
                <<0 || do_booleans_7_a(Y)>>
        catch
            [] ->
                X
        end
    ).

do_booleans_7_a(_) ->
    [].

do_booleans_8([X | Y]) ->
    try
        ([ok || _ <- Y] > catch <<ok>>) xor false
    catch
        <<Z:X>> ->
            Z
    end.

do_booleans_9(X) ->
    (try
        (true or garbage_collect()) xor
            is_tuple(catch (1 / 0))
    of
        #{} ->
            ok
    catch
        _ ->
            ok
    end) < X.        

-record(update_tuple_a, {a,b}).
-record(update_tuple_b, {a,b,c}).

setelement(_Config) ->
    T0 = id({a,42}),
    {a,_} = T0,
    {b,_} = setelement(1, T0, b),
    {z,b} = do_setelement_1(<<(id(1)):32>>, {a,b}, z),
    {new,two} = do_setelement_2(<<(id(1)):1>>, {one,two}, new),
    {x,b} = setelement(id(1), id({a,b}), x),

    Index0 = case id(1) of
                 0 -> 1;
                 1 -> 2
            end,
    {a,x,c} = setelement(Index0, {a,b,c}, x),

    Index1 = case id(1) of
                 0 -> 4;
                 1 -> 5
             end,
    {'EXIT',{badarg,_}} = catch setelement(Index1, {a,b,c}, y),

    %% Cover some edge cases in beam_call_types:will_succeed/3 and
    %% beam_call_types:types/3
    {y} = setelement(1, tuple_or_integer(0), y),
    {y} = setelement(1, record_or_integer(0), y),
    {'EXIT',{badarg,_}} = catch setelement(2, tuple_or_integer(id(0)), y),
    {'EXIT',{badarg,_}} = catch setelement(2, tuple_or_integer(id(1)), y),
    {'EXIT',{badarg,_}} = catch setelement(2, record_or_integer(id(0)), y),
    {'EXIT',{badarg,_}} = catch setelement(2, record_or_integer(id(1)), y),
    {'EXIT',{badarg,_}} = catch setelement(id(2), not_a_tuple, y),

    %% Cover some edge cases in beam_types:update_tuple/2
    {'EXIT',{badarg,_}} = catch setelement(2, not_a_tuple, y),
    {'EXIT',{badarg,_}} = catch setelement(not_an_index, {a,b,c}, y),
    {'EXIT',{badarg,_}} = catch setelement(8, {out_of_range}, y),
    {y,_,_} = update_tuple_1(#update_tuple_a{}, y),
    {y,_,_,_} = update_tuple_1(#update_tuple_b{}, y),
    #update_tuple_a{a=y} = update_tuple_2(#update_tuple_a{}, y),
    #update_tuple_b{a=y} = update_tuple_2(#update_tuple_b{}, y),
    {'EXIT',{badarg,_}} = catch update_tuple_3(id(#update_tuple_a{}), y),
    {'EXIT',{badarg,_}} = catch update_tuple_3(id(#update_tuple_b{}), y),
    {'EXIT',{badarg,_}} = catch update_tuple_4(id(#update_tuple_a{}), y),
    #update_tuple_b{c=y} = update_tuple_4(id(#update_tuple_b{}), y),

    ok.

record_or_integer(0) ->
    {tuple};
record_or_integer(N) when is_integer(N) ->
    N.

tuple_or_integer(0) ->
    {id(tuple)};
tuple_or_integer(N) when is_integer(N) ->
    N.

do_setelement_1(<<N:32>>, Tuple, NewValue) ->
    _ = element(N, Tuple),
    %% While updating the type for Tuple, beam_ssa_type would do:
    %%   maps:without(lists:seq(0, 4294967295), Elements)
    setelement(N, Tuple, NewValue).

do_setelement_2(<<N:1>>, Tuple, NewValue) ->
    %% Cover the second clause in remove_element_info/2. The
    %% type for the second element will be kept.
    two = element(2, Tuple),
    setelement(N, Tuple, NewValue).

update_tuple_1(Tuple, Value0) ->
    Value = case Tuple of
                #update_tuple_a{} -> Value0;
                #update_tuple_b{} -> Value0
            end,
    setelement(1, Tuple, Value).

update_tuple_2(Tuple, Value0) ->
    Value = case Tuple of
                #update_tuple_a{} -> Value0;
                #update_tuple_b{} -> Value0
            end,
    setelement(2, Tuple, Value).

update_tuple_3(Tuple, Value0) ->
    Value = case Tuple of
                #update_tuple_a{} -> Value0;
                #update_tuple_b{} -> Value0
            end,
    setelement(47, Tuple, Value).

update_tuple_4(Tuple, Value0) ->
    Value = case Tuple of
                #update_tuple_a{} -> Value0;
                #update_tuple_b{} -> Value0
            end,
    %% #update_tuple_a{} is three elements long, so this should only work for
    %% #update_tuple_b{}.
    setelement(4, Tuple, Value).

cons(_Config) ->
    [did] = cons(assigned, did),

    true = cons_is_empty_list([]),
    false = cons_is_empty_list([a]),

    false = cons_not(true),
    true = cons_not(false),

    {$a,"bc"} = cons_hdtl(true),
    {$d,"ef"} = cons_hdtl(false),
    ok.

cons(assigned, Instrument) ->
    [Instrument] = [did].

cons_is_empty_list(L) ->
    Cons = case L of
               [] -> "true";
               _ -> "false"
           end,
    id(1),
    case Cons of
        "true" -> true;
        "false" -> false
    end.

cons_not(B) ->
    Cons = case B of
               true -> "true";
               false -> "false"
           end,
    id(1),
    case Cons of
        "true" -> false;
        "false" -> true
    end.

cons_hdtl(B) ->
    Cons = case B of
               true -> "abc";
               false -> "def"
           end,
    id(1),
    {id(hd(Cons)),id(tl(Cons))}.

-record(bird, {a=a,b=id(42)}).

tuple(_Config) ->
    {'EXIT',{{badmatch,{necessary}},_}} = (catch do_tuple()),

    [] = [X || X <- [], #bird{a = a} == {r,X,foo}],
    [] = [X || X <- [], #bird{b = b} == {bird,X}],
    [] = [X || X <- [], 3 == X#bird.a],

    1 = do_literal_tuple_1(1),
    1 = do_literal_tuple_1(20),
    {'EXIT', _} = catch do_literal_tuple_1(id(0)),
    {'EXIT', _} = catch do_literal_tuple_1(id(bad)),

    2 = do_literal_tuple_2(1),
    2 = do_literal_tuple_2(15),
    2 = do_literal_tuple_2(20),

    Counters0 = id({0,0,0}),                    %Inexact size.
    {1,0,0} = Counters1 = increment_element(1, Counters0),
    {1,1,0} = increment_element(2, Counters1),

    Counters10 = {id(0),id(0),id(0)},           %Exact size.
    {0,-1,0} = decrement_element(2, Counters10),
    {0,0,-1} = decrement_element(3, Counters10),
    {'EXIT',{badarg,_}} = catch decrement_element(4, Counters10),

    [] = gh_6458(id({true})),
    {'EXIT',{function_clause,_}} = catch gh_6458(id({false})),
    {'EXIT',{function_clause,_}} = catch gh_6458(id({42})),
    {'EXIT',{function_clause,_}} = catch gh_6458(id(a)),

    {'EXIT',{badarg,_}} = catch gh_6927(id({a,b})),
    {'EXIT',{badarg,_}} = catch gh_6927(id([])),

    ok.

do_tuple() ->
    {0, _} = {necessary}.

do_literal_tuple_1(X) ->
    element(X, {1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1}).

do_literal_tuple_2(X) ->
    element(X, {2,2,2,2,2, 2,2,2,2,2, 2,2,2,2,2, 2,2,2,2,2}).

increment_element(Pos, Cs) ->
    Ns = element(Pos, Cs),
    setelement(Pos, Cs, Ns + 1).

decrement_element(Pos, Cs) ->
    Ns = element(Pos, Cs),
    setelement(Pos, Cs, Ns - 1).

gh_6458({X}) when X; (X orelse false) ->
    (X orelse {}),
    [
     {X}#{
          gh_6458() orelse X => []
         }
     || _ <- []
    ].

gh_6458() ->
    true.

gh_6927(X) ->
    %% beam_validator would complain because beam_call_types:will_succeed/3
    %% said `maybe`, but beam_call_types:types/3 returned the type `none`.
    element(42,
            case X of
                {_,_} -> X;
                _ -> ok
            end).

-record(x, {a}).

record_float(_Config) ->
    17.0 = record_float(#x{a={0}}, 1700),
    23.0 = record_float(#x{a={0}}, 2300.0),
    {'EXIT',{if_clause,_}} = (catch record_float(#x{a={1}}, 88)),
    {'EXIT',{if_clause,_}} = (catch record_float(#x{a={}}, 88)),
    {'EXIT',{if_clause,_}} = (catch record_float(#x{}, 88)),
    ok.

record_float(R, N0) ->
    N = N0 / 100,
    if element(1, R#x.a) =:= 0 ->
            N
    end.

binary_float(_Config) ->
    <<-1/float>> = binary_negate_float(<<1/float>>),
    {'EXIT',{badarg,_}} = catch binary_float_1(id(64.0), id(0)),
    ok.

binary_negate_float(<<Float/float>>) ->
    <<-Float/float>>.

%% GH-7147.
binary_float_1(X, Y) ->
    _ = <<Y:(ceil(64.0 = X))/float, (binary_to_integer(ok))>>,
    ceil(X) band Y.

float_compare(_Config) ->
    false = do_float_compare(-42.0),
    false = do_float_compare(-42),
    false = do_float_compare(0),
    false = do_float_compare(0.0),
    true = do_float_compare(42),
    true = do_float_compare(42.0),
    ok.

do_float_compare(X) ->
    %% ERL-433: Used to fail before OTP 20. Was accidentally fixed
    %% in OTP 20. Add a test case to ensure it stays fixed.

    Y = X + 1.0,
    case X > 0 of
        T when (T =:= nil) or (T =:= false) -> T;
        _T -> Y > 0
    end.

float_overflow(_Config) ->
    Res1 = id((1 bsl 1023) * two()),
    Res1 = float_overflow_1(),

    Res2 = id((-1 bsl 1023) * two()),
    Res2 = float_overflow_2(),

    {'EXIT',{{bad_filter,[0]},_}} = catch float_overflow_3(),

    ok.

%% GH-7178: There would be an overflow when converting a number range
%% to a float range.
float_overflow_1() ->
    round(
      try
          round(float(1 bsl 1023)) * two()
      catch
          _:_ ->
              0.0
      end
     ).

float_overflow_2() ->
    round(
      try
          round(float(-1 bsl 1023)) * two()
      catch
          _:_ ->
              0.0
      end
     ).

two() -> 2.

float_overflow_3() ->
    [0 || <<>> <= <<>>,
          [0 || (floor(1.7976931348623157e308) bsl 1) >= (1.0 + map_size(#{}))]
    ].

arity_checks(_Config) ->
    %% ERL-549: an unsafe optimization removed a test_arity instruction,
    %% causing the following to return 'broken' instead of 'ok'.
    ok = do_record_arity_check({rgb, 255, 255, 255, 1}),
    ok = do_tuple_arity_check({255, 255, 255, 1}).
 
-record(rgb, {r = 255, g = 255, b = 255}).

do_record_arity_check(RGB) when
        (element(2, RGB) >= 0), (element(2, RGB) =< 255),
        (element(3, RGB) >= 0), (element(3, RGB) =< 255),
        (element(4, RGB) >= 0), (element(4, RGB) =< 255) ->
    if
        element(1, RGB) =:= rgb, is_record(RGB, rgb) -> broken;
        true -> ok
    end.

do_tuple_arity_check(RGB) when is_tuple(RGB),
        (element(1, RGB) >= 0), (element(1, RGB) =< 255),
        (element(2, RGB) >= 0), (element(2, RGB) =< 255),
        (element(3, RGB) >= 0), (element(3, RGB) =< 255) ->
    case RGB of
        {255, _, _} -> broken;
        _ -> ok
    end.

elixir_binaries(_Config) ->
    <<"foo blitzky baz">> = elixir_binary_1(<<"blitzky">>),
    <<"foo * baz">> = elixir_binary_2($*),
    <<7:4,755:10>> = elixir_bitstring_3(<<755:10>>),
    ok.

elixir_binary_1(Bar) when is_binary(Bar) ->
    <<"foo ",
      case Bar of
          Rewrite when is_binary(Rewrite) ->
              Rewrite;
          Rewrite ->
              list_to_binary(Rewrite)
      end/binary,
      " baz">>.

elixir_binary_2(Arg) ->
    Bin = <<Arg>>,
    <<"foo ",
      case Bin of
          Rewrite when is_binary(Rewrite) ->
              Rewrite;
          Rewrite ->
              list_to_binary:to_string(Rewrite)
      end/binary,
      " baz">>.

elixir_bitstring_3(Bar) when is_bitstring(Bar) ->
    <<7:4,
      case Bar of
          Rewrite when is_bitstring(Rewrite) ->
              Rewrite;
          Rewrite ->
              list_to_bitstring(Rewrite)
      end/bitstring>>.

find_best(_Config) ->
    ok = find_best([a], nil),
    ok = find_best([<<"a">>], nil),
    {error,_} = find_best([], nil),
    ok.

%% Failed because beam_type assumed that the operand
%% for bs_context_binary must be a binary. Not true!
find_best([a|Tail], Best) ->
    find_best(Tail,
      case Best of
          X when X =:= nil orelse X =:= false -> a;
          X -> X
      end);
find_best([<<"a">>|Tail], Best) ->
    find_best(Tail,
      case Best of
          X when X =:= nil orelse X =:= false -> <<"a">>;
          X -> X
      end);
find_best([], a) ->
    ok;
find_best([], <<"a">>) ->
    ok;
find_best([], nil) ->
    {error,<<"should not get here">>}.

test_size(_Config) ->
    2 = do_test_size({a,b}),
    4 = do_test_size(<<42:32>>),
    ok.

do_test_size(Term) when is_tuple(Term) ->
    size(Term);
do_test_size(Term) when is_binary(Term) ->
    size(Term).

cover_lists_functions(Config) ->
    foo = lists:foldl(id(fun(_, _) -> foo end), foo, Config),
    foo = lists:foldl(fun(_, _) -> foo end, foo, Config),
    {'EXIT',_} = catch lists:foldl(not_a_fun, foo, Config),

    foo = lists:foldr(id(fun(_, _) -> foo end), foo, Config),
    foo = lists:foldr(fun(_, _) -> foo end, foo, Config),
    {'EXIT',_} = catch lists:foldr(not_a_fun, foo, Config),

    {data_dir,_DataDir} = lists:keyfind(data_dir, id(1), Config),
    {'EXIT',_} = catch lists:keyfind(data_dir, not_a_position, Config),
    {'EXIT',_} = catch lists:keyfind(data_dir, 1, not_a_list),

    {'EXIT',_} = catch lists:map(not_a_fun, Config),
    {'EXIT',_} = catch lists:map(not_a_fun, []),
    {'EXIT',_} = catch lists:map(fun id/1, not_a_list),
    Config = lists:map(id(fun id/1), Config),

    case lists:suffix([no|Config], Config) of
        true ->
            ct:fail(should_be_false);
        false ->
            ok
    end,

    [] = lists:zip([], []),
    {'EXIT',_} = (catch lists:zip(not_list, [b])),

    Zipper = fun(A, B) -> {A,B} end,

    [] = lists:zipwith(Zipper, [], []),

    Zipped = lists:zipwith(Zipper,
                           lists:duplicate(length(Config), zip),
                           Config),
    [{zip,_}|_] = Zipped,

    DoubleZip = lists:zipwith(id(Zipper),
                              lists:duplicate(length(Zipped), zip_zip),
                              Zipped),
    [{zip_zip,{zip,_}}|_] = DoubleZip,

    {'EXIT',_} = (catch lists:zipwith(not_a_fun, [a], [b])),
    {'EXIT',{bad,_}} = (catch lists:zipwith(fun(_A, _B) -> error(bad) end,
                                            [a], [b])),
    {'EXIT',_} = (catch lists:zipwith(fun(_A, _B) -> error(bad) end,
                                      not_list, [b])),
    {'EXIT',{bad,_}} = (catch lists:zipwith(fun(_A, _B) -> error(bad) end,
                                            lists:duplicate(length(Zipped), zip_zip),
                                            Zipped)),

    {'EXIT',_} = catch lists:unzip(not_a_list),
    {'EXIT',_} = catch lists:unzip([not_a_tuple]),
    {[_|_],[_|_]} = lists:unzip(Zipped),

    ok.

list_append(_Config) ->
    %% '++'/2 has a quirk where it returns the right-hand argument as-is when
    %% the left-hand is [].
    hello = id([]) ++ id(hello),
    ok.

%% OTP-15872: The compiler would treat the "Unit" of bs_init instructions as
%% the unit of the result instead of the required unit of the input, causing
%% is_binary checks to be wrongly optimized away.
bad_binary_unit(_Config) ->
    Bin = id(<<1,2,3>>),
    Bitstring = <<Bin/binary,1:1>>,
    false = is_binary(Bitstring),
    ok.

%% ERL-1013: The compiler would crash during the type optimization pass.
none_argument(_Config) ->
    Binary = id(<<3:16, 42>>),
    error = id(case Binary of
                   <<Len:16, Body/binary>> when length(Body) == Len - 2 ->
                       %% The type for Body will be none. It means
                       %% that this clause will never match and that
                       %% uncompress/1 will never be called.
                       uncompress(Body);
                   _ ->
                       error
               end),
    ok.

uncompress(CompressedBinary) ->
    %% The type for CompressedBinary is none, which beam_ssa_type
    %% did not handle properly.
    zlib:uncompress(CompressedBinary).

%% ERL-1289: The compiler could enter an endless loop when a return/argument
%% type pairing was joined with another.
%%
%% While this always resulted in correct success types, the joined argument
%% types could now cover more cases than they did before, making the effective
%% return type less specific. When a function affected by this was analyzed
%% again its success typing could become more specific again and start the
%% process anew.
success_type_oscillation(_Config) ->
    Base = {a, []},

    Base = sto_1(id(case_1_1)),
    Base = sto_1(id(case_2_1)),
    {b, [Base]} = sto_1(id(case_2_2)),

    ok.

sto_1(case_1_1) -> {a, []};
sto_1(case_1_2) -> {a, []};
sto_1(case_2_1) -> sto_1(case_1_1);
sto_1(case_2_2) -> {b, [sto_1(case_1_1)]};
sto_1(case_2_3) -> {b, [sto_1(case_1_1)]};
sto_1(case_2_4) -> {b, [sto_1(case_1_2)]};
sto_1(case_3_1) -> {b, [sto_1(case_2_1)]};
sto_1(case_3_2) -> {b, [sto_1(case_2_2)]};
sto_1(case_3_3) -> {b, [sto_1(case_2_3)]};
sto_1(case_3_4) -> {b, [sto_1(case_2_4)]};
sto_1(case_4_1) -> {b, [sto_1(case_3_1)]};
sto_1(case_4_2) -> {b, [sto_1(case_3_2)]};
sto_1(step_4_3) -> {b, [sto_1(case_3_3)]}.

%% ERL-1440: On inequality, we subtracted the type *common to* both variables
%% rather than the left-hand type from the right-hand variable and vice versa,
%% giving an erroneously narrow type.
%%
%% In the test below, we have functions returning integers ranged 1..2 and
%% 2..3 and test for their equality. We know that it can only succeed when both
%% return 2, but they can fail when the former returns 1 or the latter returns
%% 3, so we must not subtract 2 on the failure path.
type_subtraction(Config) when is_list(Config) ->
    true = type_subtraction_1(id(<<"A">>)),

    ok = type_subtraction_2(id(true)),
    <<"aaaa">> = type_subtraction_2(id(false)),
    {'EXIT', _} = catch type_subtraction_3(id(false)),
    ok = catch type_subtraction_4(id(ok)),
    {'EXIT', _} = catch type_subtraction_4(id(false)),

    ok.


type_subtraction_1(_x@1) ->
    _a@1 = ts_12(_x@1),
    _b@1 = ts_23(_x@1),
    case _a@1 /= _b@1 of
        false -> error;
        true -> _a@1 =:= 3 andalso _b@1 =:= 2
    end.

ts_12(_x@1) ->
    case _x@1 == <<"A">> of
        false ->
            2;
        true ->
            3
    end.

ts_23(_x@1) ->
    case _x@1 == <<"A">> of
        false ->
            1;
        true ->
            2
    end.

type_subtraction_2(X) ->
    case ts_34(X) of
        Tuple when element(1, Tuple) =:= ok ->
            ok;
        Tuple when element(1, Tuple) =:= error ->
            element(2, Tuple)
    end.

ts_34(X) ->
    case X of
        true -> {ok};
        false -> {error, <<"aaaa">>}
    end.

type_subtraction_3(_V0) when is_boolean(_V0), is_binary(_V0), _V0 andalso _V0 ->
    ok.

type_subtraction_4(_V0) ->
    try
        _V0 = ok
    catch
        _ ->
            <<
                0
             || _V0 := _ <- ok,
                (try ok of
                    _ when _V0, (_V0 andalso _V0) orelse trunc(ok) ->
                        ok
                catch
                    _ ->
                        ok
                end)
            >>
    end.

%% GH-4774: The validator didn't update container contents on type subtraction.
container_subtraction(Config) when is_list(Config) ->
    A = id(baz),

    cs_1({foo,[]}),
    cs_1({bar,A}),
    cs_2({bar,A}),

    ok.

cs_1({_,[]}) ->
    ok;
cs_1({_,_}=Other) ->
    cs_2(Other).

cs_2({bar,baz}) ->
    ok.

is_list_opt(_Config) ->
    true = is_list_opt_1(id(<<"application/a2l">>)),
    false = is_list_opt_1(id(<<"">>)),

    ok = is_list_opt_3(id([])),
    true = is_list_opt_3(id([a])),
    {'EXIT',{badarg,_}} = catch is_list_opt_3(id(no_list)),

    ok.

is_list_opt_1(Type) ->
    %% The call to is_list/1 would be optimized to an is_nonempty_list
    %% instruction, which is illegal in a return context. That would
    %% crash beam_ssa_codegen.
    is_list(is_list_opt_2(Type)).

is_list_opt_2(<<"application/a2l">>) -> [<<"a2l">>];
is_list_opt_2(_Type) -> nil.

is_list_opt_3([]) ->
    ok;
is_list_opt_3(A) ->
    %% The call to is_list/1 would be optimized to an is_nonempty_list
    %% instruction, which only exists as a guard test that cannot
    %% produce boolean value.
    _ = (Bool = is_list(A)) orelse binary_to_integer(<<"">>),
    Bool.


%% We used to determine the type of `get_tuple_element` at the time of
%% extraction, which is simple but sometimes throws away type information when 
%% on tuple unions.
%%
%% This normally doesn't cause any issues other than slightly less optimized
%% code, but would crash the type pass in rare cases. Consider the following
%% SSA:
%%
%% ----
%%  %% (Assume _0 is either `{a, 1}`, {b, 2}, or `{c, {d}}`)
%%  0: 
%%      _1 = get_tuple_element _0, `0`
%%      _2 = get_tuple_element _0, `1`
%%      switch _1, ^3, [
%%          { `a`, ^1 },
%%          { `b`, ^2 }
%%      ]
%%  1: ... snip ...
%%  2: ... snip ...
%%  3:
%%      _3 = get_tuple_element _0, `1`
%%      @ssa_bool = is_tagged_tuple _3, 1, `d`
%%      br @ssa_bool ^4, ^1234
%%  4:
%%      _4 = get_tuple_element _3, `0`
%% ----
%%
%% In block 0 we determine that the type of _1 is `a | b | c` and that _2
%% is `1 | 2 | {d}`. From this, we know that _0 is `{a, 1}` in block 1,
%% `{b, 2}` in block 2, and `{c, {d}}` in block 3.
%%
%% In block 3, we remove the `is_tagged_tuple` test since it's redundant.
%%
%% ... but then the compiler replaces _3 with _2 since they're the same value.
%% However, the type is still the same old `1 | 2 | {d}` we got in block 0,
%% which is not safe for `get_tuple_element`, crashing the type pass.
%% 
%% We fixed this by determining the type of `get_tuple_element` when the result
%% is used instead of when it was extracted, giving the correct type `{d}` in
%% block 4.
connected_tuple_elements(_Config) ->
    {c, 1, 2, 3} = cte_match(id(gurka), cte_generate(id(2))),
    ok.

cte_match(_, {a, A, B}) ->
    {a, id(A), id(B)};
cte_match(_, {b, A, B}) ->
    {b, id(A), id(B)};
cte_match(gurka, {c, A, {{B}, {C}}}) ->
    {c, id(A), id(B), id(C)}.

cte_generate(0) ->
    {a, id(1), id(2)};
cte_generate(1) ->
    {b, id(1), id(2)};
cte_generate(2) ->
    {c, id(1), {{id(2)}, {id(3)}}}.

%% ERIERL-799: Type inference for the fail label on switch terminators was
%% weaker than that of the case labels, sometimes causing the compiler to crash
%% when they were inverted.
switch_fail_inference(_Config) ->
    ok = sfi(id([])),
    ok = sfi(id([{twiddle,frobnitz}, eof])),
    {error, gaffel, gurka} = sfi(id([{twiddle, frobnitz}, {error, gurka}])),
    {error, gaffel, gurka} = sfi(id([{ok, frobnitz}, {error, gurka}])),

    ok = sfi_5(id("GET")),
    error = sfi_5(id("OTHER")),

    ok.

sfi(Things) ->
    case sfi_1(Things) of
        {ok, _} -> ok;
        {error, {Left, Right}} -> {error, Left, Right}
    end.

sfi_1(Things) ->
    case sfi_2(Things) of
        {ok, Value} -> {ok, Value};
        {error, Reason} -> {error, Reason}
    end.

sfi_2([Thing | Rest]) ->
    case sfi_3(Thing) of
        {ok, _} -> sfi_2(Rest);
        {error, Reason} -> {error, Reason}
    end;
sfi_2([]) ->
    {ok, done}.

sfi_3({twiddle, _}) ->
    {ok, twiddle};
sfi_3(Thing) ->
    case sfi_4(Thing) of
        {twiddle, _}=More -> sfi_3(More);
        {ok, Value} -> {ok, Value};
        {error, Reason} -> {error, Reason}
    end.

sfi_4(eof) ->
    {ok, eof};
sfi_4({ok, IgnoredLater}) ->
    {twiddle, IgnoredLater};
sfi_4({error, Reason}) ->
    {error, {gaffel, Reason}}.

sfi_5(Info) ->
    ReturnValue = case Info of
                      "GET" ->
                          {304};
                      _ ->
                          {412}
                  end,
    sfi_send_return_value(ReturnValue).

sfi_send_return_value({304}) ->
    ok;
sfi_send_return_value({412}) ->
    error.

failures(_Config) ->
    {'EXIT',{function_clause,_}} = catch failures_1(id(a), id(b), id(c)),
    {'EXIT',{badarith,_}} = catch failures_1([], 2, 3),
    {'EXIT',{badarith,_}} = catch failures_1([], x, y),
    ok.

failures_1([] = V1, V2, V3)  ->
    %% beam_call_types:types(erlang, '-', [any,nil]) would return
    %% {none,_,_}, indicating that the call would fail, while
    %% beam_call_types:will_succeed/3 called with the same arguments
    %% would return `maybe` instead of `no`. That would cause
    %% generation of incorrect BEAM code that would ultimately cause
    %% beam_clean to crash.
    {V1 - V3, (V1 = V2) - V3}.

%% Covers various edge cases in beam_call_types:types/3 relating to maps
cover_maps_functions(_Config) ->
    {'EXIT',_} = catch maps:filter(fun(_, _) -> true end, not_a_map),
    {'EXIT',_} = catch maps:filter(not_a_predicate, #{}),

    error = maps:find(key_not_present, #{}),

    {'EXIT',_} = catch maps:fold(fun(_, _, _) -> true end, init, not_a_map),
    {'EXIT',_} = catch maps:fold(not_a_fun, init, #{}),

    #{} = maps:from_keys([], gurka),
    #{ hello := gurka } = maps:from_keys([hello], gurka),
    {'EXIT',_} = catch maps:from_keys(not_a_list, gurka),

    #{} = catch maps:from_list([]),
    {'EXIT',_} = catch maps:from_list([not_a_tuple]),

    default = maps:get(key_not_present, #{}, default),
    {'EXIT',_} = catch maps:get(key_not_present, #{}),

    [] = maps:keys(#{}),
    {'EXIT',_} = catch maps:keys(not_a_map),

    #{ a := ok } = catch maps:map(fun(_, _) -> ok end, #{ a => a }),
    {'EXIT',_} = catch maps:map(fun(_, _) -> error(crash) end, #{ a => a }),
    {'EXIT',_} = catch maps:map(not_a_fun, #{}),
    {'EXIT',_} = catch maps:map(fun(_, _) -> ok end, not_a_map),

    {'EXIT',_} = catch maps:merge(not_a_map, #{}),

    #{} = maps:new(),

    {'EXIT',_} = catch maps:put(key, value, not_a_map),

    #{} = maps:remove(a, #{ a => a }),
    {'EXIT',_} = catch maps:remove(gurka, not_a_map),

    error = maps:take(key_not_present, #{}),
    {'EXIT',_} = catch maps:take(key, not_a_map),

    {'EXIT',_} = catch maps:to_list(not_a_map),

    #{ a := ok } = maps:update_with(a, fun(_) -> ok end, #{ a => a }),
    {'EXIT',_} = catch maps:update_with(a, fun(_) -> error(a) end, #{ a => a }),
    {'EXIT',_} = catch maps:update_with(key_not_present, fun(_) -> ok end, #{}),
    {'EXIT',_} = catch maps:update_with(key, not_a_fun, not_a_map),

    [] = maps:values(#{}),
    {'EXIT',_} = catch maps:values(not_a_map),

    #{} = maps:with([key_not_present], #{}),
    {'EXIT',_} = catch maps:with(not_a_list, #{}),
    {'EXIT',_} = catch maps:with([], not_a_map),
    {'EXIT',_} = catch maps:with([foobar], not_a_map),

    {'EXIT',_} = catch maps:without(not_a_list, #{}),
    {'EXIT',_} = catch maps:without([], not_a_map),

    ok.

%% The types for erlang:min/2 and erlang:max/2 were wrong, assuming that the
%% result was a float if either argument was a float.
min_max_mixed_types(_Config) ->
    NotFloatA = min(id(12), 100.0),
    id(NotFloatA * 0.5),

    NotFloatB = max(id(12.0), 100),
    id(NotFloatB * 0.5),

    %% Cover more of the type analysis code.
    1 = id(min(id(0)+1, 42)),
    -10 = id(min(id(0)+1, -10)),
    43 = id(max(3, id(42)+1)),
    42 = id(max(-99, id(41)+1)),

    ok.

%% GH-6183. beam_validator had a stronger type analysis for '=/=' and
%% is_ne_exact than the beam_ssa_type pass. It would figure out that
%% at the time the comparison 'a /= V' was evaluated, V must be equal
%% to 'true' and the comparison would therefore always return 'false'.
%% beam_validator would report that as a type conflict.

not_equal(_Config) ->
    true = do_not_equal(true),
    {'EXIT',{function_clause,_}} = catch do_not_equal(false),
    {'EXIT',{function_clause,_}} = catch do_not_equal(0),
    {'EXIT',{function_clause,_}} = catch do_not_equal(42),
    {'EXIT',{function_clause,_}} = catch do_not_equal(self()),

    ok.

do_not_equal(V) when (V / V < (V orelse true)); V; V ->
    (V = (a /= V)) orelse 0.


infer_relops(_Config) ->
    {'EXIT',{badarith,_}} = catch infer_relops_1(),
    {'EXIT',{badarith,_}} = catch infer_relops_2(),
    {'EXIT',{badarith,_}} = catch infer_relops_3(id(0)),
    infer_relops_4(),

    ok.

%% GH-6568: Type inference for relational operations returned erroneous results
%% for singletons.
infer_relops_1() ->
    <<0 || ((0 rem 0) > [0 || _ <- catch (node())]), _ <- []>>.

infer_relops_2() ->
    X = self() + 0,
    [0 || [0 || _ <- date()] =< X, _ <- []].

infer_relops_3(X) ->
    <<0 || ((+(is_alive())) <
            [
                infer_relops_3(infer_relops_3(0))
             || X
            ]) andalso ok
    >>.

infer_relops_4() ->
    [
     ok
     || <<X>> <= <<>>,
        <<Y:X>> <= <<>>,
        0 > Y,
        [
         Y
         || _ <- []
        ]
    ].

%% GH-6593: the type pass would correctly determine that the tail unit of
%% <<0:integer/8,I:integer/8>> was 16, but would fail to see that after
%% optimizing it to <<"\0",I:integer/8>>
binary_unit(_Config) ->
    F = id(binary_unit_1()),

    <<0,1>> = F([1]),
    <<0,0>> = F([0,1]),

    ok.

binary_unit_1() ->
    fun Foo(X) ->
        I = hd([Y || Y <- X, _ <- X, (Foo >= ok)]),
        <<0, I>>
    end.

%% ERIERL-918: A call to a local function (in this case `pm_concretization_3`)
%% forced the extracted type of `Tagged` to be concretized before we checked
%% `Status`, passing an unknown type to `pm_concretization_4`.
premature_concretization(_Config) ->
    ok = pm_concretization_1(id(tagged), id({tagged, foo})),
    error = pm_concretization_1(id(flurb), id({tagged, foo})),
    ok.

pm_concretization_1(Frobnitz, Tagged) ->
    {Status, NewTagged} = pm_concretization_2(Frobnitz, Tagged),
    pm_concretization_3(NewTagged),
    case Status of
        ok -> pm_concretization_4(NewTagged);
        error -> error
    end.

pm_concretization_2(tagged, {tagged, _Nonsense}=T) -> {ok, T};
pm_concretization_2(_, Tagged) -> {error, Tagged}.

pm_concretization_3(_) -> ok.
pm_concretization_4(_) -> ok.

funs(_Config) ->
    {'EXIT',{badarg,_}} = catch gh_7179(),
    false = is_function(id(fun() -> ok end), 1024),

    {'EXIT',{badarg,_}} = catch gh_7197(),

    ok.

%% GH-7179: The beam_ssa_type pass would crash.
gh_7179() ->
    << <<0>> || is_function([0 || <<_>> <= <<>>], -1),
                [] <- [] >>.

%% GH-7197: The beam_ssa_type pass would crash.
gh_7197() ->
    [0 || is_function([ok || <<_>> <= <<>>], get_keys()),
          fun (_) ->
                  ok
          end].

will_succeed(_Config) ->
    b = will_succeed_1(id(ok), id(#{})),
    ok.

%% OTP-18576: the beam_call_types:will_succeed/3 check was incorrect for 'bsl',
%% erroneously stating that it would never fail in some instances.
will_succeed_1(_V0, _V1)
  when (1 bsl ((map_size(_V1) bxor 288230376151711743)
               band 288230376151711743)) =:= _V0 ->
    a;
will_succeed_1(_, _) ->
    b.

%% GH-7901: Range operations did not honor the total order of floats.
float_confusion(_Config) ->
    ok = float_confusion_1(catch (true = ok), -0.0),
    ok = float_confusion_1(ok, 0.0),
    {'EXIT', _} = catch float_confusion_2(),
    {'EXIT', _} = catch float_confusion_3(id(0.0)),
    ok = float_confusion_4(id(1)),
    {'EXIT', _} = catch float_confusion_5(),
    {'EXIT', _} = catch float_confusion_6(),
    ok.

float_confusion_1(_, _) ->
    ok.

float_confusion_2() ->
    [ok || _ := _ <- ok,
     float_confusion_crash(catch float_confusion_crash(ok, -1), -0.0)].

float_confusion_crash(_, 18446744073709551615) ->
    ok.

float_confusion_3(V) ->
    -0.0 = abs(V),
    ok.

float_confusion_4(V) when -0.0 < floor(V band 1) ->
    ok.

float_confusion_5() ->
    -0.0 =
        case
            fun() ->
                ok
            end
        of
            _V2 when (_V2 > ok) ->
                2147483647.0;
            _ ->
                -2147483648
        end * 0,
    ok.

%% GH-8097: Record keys weren't compared in total order, confusing +0.0 and
%% -0.0 and crashing the compiler.
float_confusion_6() ->
    <<
        (ok)
     || _ := {_V1} <- ok,
        (maybe
            {} ?= _V1
        else
            -0.0 ->
                [];
            0.0 ->
                []
        end)
    >>.

%%%
%%% Common utilities.
%%%

id(I) ->
    I.
