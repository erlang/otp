%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2018. All Rights Reserved.
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
	 cons/1,tuple/1,record_float/1,binary_float/1,float_compare/1,
	 arity_checks/1,elixir_binaries/1,find_best/1,
         test_size/1,cover_lists_functions/1,list_append/1,bad_binary_unit/1,
         none_argument/1]).

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
       arity_checks,
       elixir_binaries,
       find_best,
       test_size,
       cover_lists_functions,
       list_append,
       bad_binary_unit,
       none_argument
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

    ok.

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

    ok.

booleans(_Config) ->
    {'EXIT',{{case_clause,_},_}} = (catch do_booleans_1(42)),

    ok = do_booleans_2(42, 41),
    error = do_booleans_2(42, 42),

    AnyAtom = id(atom),
    true = is_atom(AnyAtom),
    false = is_boolean(AnyAtom),

    MaybeBool = id(maybe),
    case MaybeBool of
        true -> ok;
        maybe -> ok;
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

setelement(_Config) ->
    T0 = id({a,42}),
    {a,_} = T0,
    {b,_} = setelement(1, T0, b),
    {z,b} = do_setelement_1(<<(id(1)):32>>, {a,b}, z),
    {new,two} = do_setelement_2(<<(id(1)):1>>, {one,two}, new),
    ok.

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

    ok.

do_tuple() ->
    {0, _} = {necessary}.

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
    ok.

binary_negate_float(<<Float/float>>) ->
    <<-Float/float>>.

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
    case lists:suffix([no|Config], Config) of
        true ->
            ct:fail(should_be_false);
        false ->
            ok
    end,
    Zipped = lists:zipwith(fun(A, B) -> {A,B} end,
                           lists:duplicate(length(Config), zip),
                           Config),
    true = is_list(Zipped),
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

id(I) ->
    I.
