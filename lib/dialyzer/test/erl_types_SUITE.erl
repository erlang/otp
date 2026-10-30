%% -*- erlang-indent-level: 4 -*-
%% %CopyrightBegin%
%% 
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2018-2026. All Rights Reserved.
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
-module(erl_types_SUITE).

-export([all/0,groups/0,init_per_group/2,end_per_group/2,
         consistency_and_to_string/1,misc/1,map_multiple_representations/1,
         bin_type_modules_mentioned/1,bin_type_operations/1,
         bin_type_lattice/1,
         absorption/1,associativity/1,commutativity/1,idempotence/1,
         identity/1,limit/1
        ]).

%% Simplify calls into erl_types and avoid importing the entire module.
-define(M, erl_types).

-include_lib("common_test/include/ct.hrl").

all() ->
    [consistency_and_to_string,
     misc,
     map_multiple_representations,
     bin_type_modules_mentioned,
     bin_type_operations,
     bin_type_lattice,
     {group,property_tests}
    ].

groups() ->
    [{property_tests,[parallel],
      [absorption,
       associativity,
       commutativity,
       idempotence,
       identity,
       limit]}].

init_per_group(property_tests, Config0) ->
    case ct_property_test:init_per_suite(Config0) of
        [_|_]=Config ->
            try proper_erlang_abstract_code:module() of
                _ ->
                    Config
            catch
                error:undef ->
                    {skip, "No proper_erlang_abstract_code module"}
            end;
        Other ->
            Other
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

consistency_and_to_string(_Config) ->
    %% Check consistency of types
    Atom1  = ?M:t_atom(),
    Atom2  = ?M:t_atom(foo),
    Atom3  = ?M:t_atom(bar),
    true   = ?M:t_is_atom(Atom2),

    True   = ?M:t_atom(true),
    False  = ?M:t_atom(false),
    Bool   = ?M:t_boolean(),
    true   = ?M:t_is_boolean(True),
    true   = ?M:t_is_boolean(Bool),
    false  = ?M:t_is_boolean(Atom1),

    Binary = ?M:t_binary(),
    true   = ?M:t_is_binary(Binary),

    Bitstr = ?M:t_bitstr(),
    true   = ?M:t_is_bitstr(Bitstr),

    Bitstr1 = ?M:t_bitstr(7, 3),
    true   = ?M:t_is_bitstr(Bitstr1),
    false  = ?M:t_is_binary(Bitstr1),

    Bitstr2 = ?M:t_bitstr(16, 8),
    true   = ?M:t_is_bitstr(Bitstr2),
    true   = ?M:t_is_binary(Bitstr2),

    BitStr816 = ?M:t_bitstr(8,16),
    BitStr816 = ?M:t_subtract(?M:t_bitstr(4, 12), ?M:t_bitstr(8, 12)),

    Int1   = ?M:t_integer(),
    Int2   = ?M:t_integer(1),
    Int3   = ?M:t_integer(16#ffffffff),
    true   = ?M:t_is_integer(Int2),
    true   = ?M:t_is_byte(Int2),
    false  = ?M:t_is_byte(Int3),
    false  = ?M:t_is_byte(?M:t_from_range(-1, 1)),
    true   = ?M:t_is_byte(?M:t_from_range(1, 255)),

    Tuple1 = ?M:t_tuple(),
    Tuple2 = ?M:t_tuple(3),
    Tuple3 = ?M:t_tuple([Atom1, Int1]),
    Tuple4 = ?M:t_tuple([Tuple1, Tuple2]),
    Tuple5 = ?M:t_tuple([Tuple3, Tuple4]),
    Tuple6 = ?M:t_limit(Tuple5, 2),
    Tuple7 = ?M:t_limit(Tuple5, 3),
    true   = ?M:t_is_tuple(Tuple1),

    Port   = ?M:t_port(),
    Pid    = ?M:t_pid(),
    Ref    = ?M:t_reference(),
    Identifier = ?M:t_identifier(),
    false  = ?M:t_is_reference(Port),
    true   = ?M:t_is_identifier(Port),

    Function1 = ?M:t_fun(),
    Function2 = ?M:t_fun(Pid),
    Function3 = ?M:t_fun([], Pid),
    Function4 = ?M:t_fun([Port, Pid], Pid),
    Function5 = ?M:t_fun([Pid, Atom1], Int2),
    true      = ?M:t_is_fun(Function3),

    List1 = ?M:t_list(),
    List2 = ?M:t_list(?M:t_boolean()),
    List3 = ?M:t_cons(?M:t_boolean(), List2),
    List4 = ?M:t_cons(?M:t_boolean(), ?M:t_atom()),
    List5 = ?M:t_cons(?M:t_boolean(), ?M:t_nil()),
    List6 = ?M:t_cons_tl(List5),
    List7 = ?M:t_sup(List4, List5),
    List8 = ?M:t_inf(List7, ?M:t_list()),
    List9 = ?M:t_cons(),
    List10 = ?M:t_cons_tl(List9),
    true  = ?M:t_is_boolean(?M:t_cons_hd(List5)),
    true  = ?M:t_is_list(List5),
    false = ?M:t_is_list(List4),

    Product1 = ?M:t_product([Atom1, Atom2]),
    Product2 = ?M:t_product([Atom3, Atom1]),
    Product3 = ?M:t_product([Atom3, Atom2]),

    Union1 = ?M:t_sup(Atom2, Atom3),
    Union2 = ?M:t_sup(Tuple2, Tuple3),
    Union3 = ?M:t_sup(Int2, Atom3),
    Union4 = ?M:t_sup(Port, Pid),
    Union5 = ?M:t_sup(Union4, Int1),
    Union6 = ?M:t_sup(Function1, Function2),
    Union7 = ?M:t_sup(Function4, Function5),
    Union8 = ?M:t_sup(True, False),
    true   = ?M:t_is_boolean(Union8),
    Union9 = ?M:t_sup(Int2, ?M:t_integer(2)),
    true   = ?M:t_is_byte(Union9),
    Union10 = ?M:t_sup(?M:t_tuple([?M:t_atom(true), ?M:t_any()]),
                       ?M:t_tuple([?M:t_atom(false), ?M:t_any()])),

    Any   = ?M:t_any(),
    Any   = ?M:t_sup(Product3, Function5),

    Atom3  = ?M:t_inf(Union3, Atom1),
    Union2 = ?M:t_inf(Union2, Tuple1),
    Int2   = ?M:t_inf(Int1, Union3),
    Union4 = ?M:t_inf(Union4, Identifier),
    Port   = ?M:t_inf(Union5, Port),
    Function4 = ?M:t_inf(Union7, Function4),
    None   = ?M:t_none(),
    None   = ?M:t_inf(Product2, Atom1),
    Product3 = ?M:t_inf(Product1, Product2),
    Function5 = ?M:t_inf(Union7, Function5),
    true   = ?M:t_is_byte(?M:t_inf(Union9, ?M:t_number())),
    true   = ?M:t_is_char(?M:t_inf(Union9, ?M:t_number())),

    RecDict = #{{record, foo} => {{?FILE, ?LINE}, [{2, [{bar, [], ?M:t_any()},
                                                        {baz, [], ?M:t_any()}]}]}},
    Record1 = ?M:t_from_term({foo, [1,2], {1,2,3}}),

    %% Check string representations
    "atom()" = ?M:t_to_string(Atom1),
    "'foo'"  = ?M:t_to_string(Atom2),
    "'bar'"  = ?M:t_to_string(Atom3),

    "binary()" = ?M:t_to_string(Binary),

    "integer()" = ?M:t_to_string(Int1),
    "1" = ?M:t_to_string(Int2),

    "tuple()" = ?M:t_to_string(Tuple1),
    "{_,_,_}" = ?M:t_to_string(Tuple2),
    "{atom(),integer()}" = ?M:t_to_string(Tuple3),
    "{tuple(),{_,_,_}}" = ?M:t_to_string(Tuple4),
    "{{atom(),integer()},{tuple(),{_,_,_}}}" = ?M:t_to_string(Tuple5),
    "{{_,_},{_,_}}" = ?M:t_to_string(Tuple6),
    "{{atom(),integer()},{tuple(),{_,_,_}}}" = ?M:t_to_string(Tuple7),

    "reference()" = ?M:t_to_string(Ref),
    "port()" = ?M:t_to_string(Port),
    "pid()" = ?M:t_to_string(Pid),
    "identifier()" = ?M:t_to_string(Identifier),

    "[any()]" = ?M:t_to_string(List1),
    "[boolean()]" = ?M:t_to_string(List2),
    "[boolean(),...]" = ?M:t_to_string(List3),
    "nonempty_improper_list(boolean(),atom())" = ?M:t_to_string(List4),
    "[boolean(),...]" = ?M:t_to_string(List5),
    "[boolean()]" = ?M:t_to_string(List6),
    "nonempty_maybe_improper_list(boolean(),atom() | [])" = ?M:t_to_string(List7),
    "[boolean(),...]" = ?M:t_to_string(List8),
    "nonempty_maybe_improper_list()" = ?M:t_to_string(List9),
    "any()" = ?M:t_to_string(List10),

    "fun()" = ?M:t_to_string(Function1),
    "fun((...) -> pid())" = ?M:t_to_string(Function2),
    "fun(() -> pid())" = ?M:t_to_string(Function3),
    "fun((port(),pid()) -> pid())" = ?M:t_to_string(Function4),
    "fun((pid(),atom()) -> 1)" = ?M:t_to_string(Function5),

    "<atom(),'foo'>" = ?M:t_to_string(Product1),
    "<'bar',atom()>" = ?M:t_to_string(Product2),

    "#foo{bar::[1 | 2,...],baz::{1,2,3}}" = ?M:t_to_string(Record1, RecDict),

    "'bar' | 'foo'" = ?M:t_to_string(Union1),
    "{atom(),integer()} | {_,_,_}" = ?M:t_to_string(Union2),
    "'bar' | 1" = ?M:t_to_string(Union3),
    "pid() | port()" = ?M:t_to_string(Union4),
    "pid() | port() | integer()" = ?M:t_to_string(Union5),
    "fun()" = ?M:t_to_string(Union6),
    "fun((pid() | port(),atom() | pid()) -> pid() | 1)" = ?M:t_to_string(Union7),
    "boolean()" = ?M:t_to_string(Union8),
    "{'false',_} | {'true',_}" = ?M:t_to_string(Union10),
    "{'true',integer()}" = ?M:t_to_string(?M:t_inf(Union10, ?M:t_tuple([?M:t_atom(true), ?M:t_integer()]))).

misc(_Config) ->
    %% Miscellaneous cases which have been fixed.

    %% Used to take "forever".
    B1_1 = ?M:t_bitstr(6442450944, 1456),
    B1_2 = ?M:t_bitstr(85, 7),
    R1_R = ?M:t_bitstr(547608330240, 347892352432),
    R1_R = ?M:t_inf(B1_1, B1_2),

    %% Used to loop forever. The nominal set of Anno has a structural part
    %% that overlaps the opaque a:t() of structure any(), and a:t() sorts
    %% before erl_anno:anno() in the set.
    Loc = ?M:t_nominal({erl_anno, location, 0, transparent},
                       ?M:t_non_neg_integer()),
    AnnoName = {erl_anno, anno, 0, opaque},
    Anno = ?M:t_nominal(AnnoName, ?M:t_sup(Loc, ?M:t_nonempty_list())),
    Set = ?M:t_sup(?M:t_nominal({a, t, 0, opaque}, ?M:t_any()),
                   ?M:t_nominal(AnnoName, ?M:t_any())),
    Sup = ?M:t_sup(Anno, Set),
    Sup = ?M:t_sup(Set, Anno),
    true = ?M:t_is_subtype(Anno, Sup),
    true = ?M:t_is_subtype(Set, Sup),
    ok.

%% OTP-17537.
map_multiple_representations(_Config) ->
    DefV = erl_types:t_atom(),
    fun() ->
            P2 = {erl_types:t_integer(0), optional, DefV},
            Ps = [P2],
            DefK = erl_types:t_pos_integer(),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{non_neg_integer()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_integer(-1), optional, DefV},
            P2 = {erl_types:t_integer(0), optional, DefV},
            Ps = [P1, P2],
            DefK = erl_types:t_pos_integer(),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{integer()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_integer(0), optional, DefV}, % integer()
            P2 = {erl_types:t_integer(1), optional, DefV}, % extra
            Ps = [P1, P2],
            DefK = erl_types:t_neg_integer(),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{integer()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_nil(), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_nonempty_list(),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{[any()]=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_nil(), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_nonempty_string(),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{string()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_nil(), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_sup(erl_types:t_nonempty_string(),
                                   erl_types:t_nil()),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{string()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_nil(), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_sup(erl_types:t_nonempty_string(),
                                   erl_types:t_atom()),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{atom() | string()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_integer(0), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_sup(erl_types:t_pos_integer(),
                                   erl_types:t_atom()),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{atom() | non_neg_integer()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_integer(8), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_from_range(9, 12),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{8 | 9 | 10 | 11 | 12=>atom()}" = erl_types:t_to_string(T)

    end(),
    fun() ->
            P1 = {erl_types:t_integer(13), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_from_range(9, 12),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{9 | 10 | 11 | 12 | 13=>atom()}" = erl_types:t_to_string(T)

    end(),
    fun() ->
            P1 = {erl_types:t_atom(a), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_sup([erl_types:t_atom(a01),
                                    erl_types:t_atom(a02),
                                    erl_types:t_atom(a03),
                                    erl_types:t_atom(a04),
                                    erl_types:t_atom(a05),
                                    erl_types:t_atom(a06),
                                    erl_types:t_atom(a07),
                                    erl_types:t_atom(a08),
                                    erl_types:t_atom(a09),
                                    erl_types:t_atom(a10),
                                    erl_types:t_atom(a11),
                                    erl_types:t_atom(a12),
                                    erl_types:t_atom(a13)]),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{atom()=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_atom(a), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_sup([erl_types:t_atom(b),
                                    erl_types:t_atom(c)]),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{'a' | 'b' | 'c'=>atom()}" = erl_types:t_to_string(T)
    end(),
    fun() ->
            P1 = {erl_types:t_atom(a), optional, DefV},
            Ps = [P1],
            DefK = erl_types:t_atom(b),
            T = erl_types:t_map(Ps, DefK, DefV),
            "#{'a'=>atom(), 'b'=>atom()}" = erl_types:t_to_string(T)
    end(),
    ok.

bin_type_modules_mentioned(_Config) ->
    %% A standalone bin_type form has no module dependencies.
    [] = ?M:type_form_to_remote_modules({bin_type, 0, <<"foo">>}),

    %% A union of bin_type forms has no module dependencies.
    [] = ?M:type_form_to_remote_modules(
           {type, 0, union, [{bin_type, 0, <<"a">>},
                             {bin_type, 0, <<"b">>}]}),

    %% A union of bin_type and remote_type extracts the remote module.
    [some_mod] = ?M:type_form_to_remote_modules(
                   {type, 0, union,
                    [{bin_type, 0, <<"foo">>},
                     {remote_type, 0, [{atom, 0, some_mod},
                                       {atom, 0, some_type},
                                       []]}]}),

    %% bin_type inside a list type has no module dependencies.
    [] = ?M:type_form_to_remote_modules(
           {type, 0, list, [{bin_type, 0, <<"x">>}]}),

    ok.

bin_type_operations(_Config) ->
    %% Construction and t_to_string.
    Foo = ?M:t_binary_val(<<"foo">>),
    Bar = ?M:t_binary_val(<<"bar">>),
    Baz = ?M:t_binary_val(<<"baz">>),
    Empty = ?M:t_binary_val(<<"">>),
    "<<\"foo\">>" = ?M:t_to_string(Foo),
    "<<\"bar\">>" = ?M:t_to_string(Bar),
    %% The empty binary has a single, structural representation.
    Empty = ?M:t_bitstr(0, 0),
    "<<>>" = ?M:t_to_string(Empty),

    %% t_is_binary / t_is_bitstr.
    true = ?M:t_is_binary(Foo),
    true = ?M:t_is_bitstr(Foo),

    %% t_from_term for binaries.
    Foo = ?M:t_from_term(<<"foo">>),
    Empty = ?M:t_from_term(<<"">>),

    %% t_sup — same-size vals merge into a multi-val type.
    FooBar = ?M:t_sup(Foo, Bar),
    "<<\"bar\">> | <<\"foo\">>" = ?M:t_to_string(FooBar),
    %% Large binaries are not tracked by value.
    Edge = binary:copy(<<"x">>, 64),
    Big = binary:copy(<<"x">>, 65),
    true = ?M:t_is_singleton(?M:t_from_term(Edge)),
    BigType = ?M:t_bitstr(0, 520),
    BigType = ?M:t_binary_val(Big),
    BigType = ?M:t_from_term(Big),
    "<<_:520>>" = ?M:t_to_string(BigType),
    true = ?M:t_bitstr(512, 8) =:= ?M:t_sup(?M:t_from_term(<<"a">>), BigType),

    %% Values that are valid UTF-8 with non-ASCII characters.
    "<<\"\x{e9}\"/utf8>>" = ?M:t_to_string(?M:t_binary_val(<<195,169>>)),
    "<<\"\x{ff}\">>" = ?M:t_to_string(?M:t_binary_val(<<255>>)),

    %% t_sup — different-size vals are kept.
    Hi = ?M:t_binary_val(<<"hi">>),
    FooHi = ?M:t_sup(Foo, Hi),
    true = ?M:t_is_binary(FooHi),
    "<<\"foo\">> | <<\"hi\">>" = ?M:t_to_string(FooHi),
    Hi = ?M:t_inf(FooHi, ?M:t_bitstr(0, 16)),

    %% A structural supertype absorbs binary values.
    Binary = ?M:t_binary(),
    Binary = ?M:t_sup(Foo, Binary),
    Binary = ?M:t_sup(FooHi, Binary),

    %% Value sets are limited in size, then fall back to a structural type.
    Bytes = [?M:t_binary_val(<<C>>) || C <- lists:seq($a, $n)],
    13 = length(?M:t_elements(?M:t_sup(tl(Bytes)))),
    B8 = ?M:t_bitstr(0, 8),
    B8 = ?M:t_sup(Bytes),

    %% t_inf — val∩val.
    None = ?M:t_none(),
    Foo = ?M:t_inf(Foo, Foo),
    None = ?M:t_inf(Foo, Bar),
    %% t_inf — val∩binary().
    Foo = ?M:t_inf(Foo, ?M:t_binary()),
    %% t_inf — val∩incompatible.
    None = ?M:t_inf(Foo, ?M:t_atom()),

    %% t_subtract.
    FooBarBaz = ?M:t_sup([Foo, Bar, Baz]),
    FooBaz = ?M:t_subtract(FooBarBaz, Bar),
    "<<\"baz\">> | <<\"foo\">>" = ?M:t_to_string(FooBaz),
    Foo = ?M:t_subtract(FooHi, Hi),
    Foo = ?M:t_subtract(FooHi, ?M:t_bitstr(0, 16)),
    %% A set of values can not cover a structural type.
    B24 = ?M:t_bitstr(0, 24),
    B24 = ?M:t_subtract(B24, FooBarBaz),

    %% t_elements — decompose into individual vals.
    [Foo] = ?M:t_elements(Foo),
    2 = length(?M:t_elements(FooBar)),

    %% t_is_singleton — single vs multiple vals.
    true = ?M:t_is_singleton(Foo),
    false = ?M:t_is_singleton(FooBar),

    %% UTF-8 encoded binary literal types produce the same type as raw bytes.
    %% <<"é"/utf8>> encodes to <<195,169>> — same as the raw binary.
    Utf8Bin = ?M:t_binary_val(<<195,169>>),
    Utf8Bin = ?M:t_binary_val(unicode:characters_to_binary([233], unicode, utf8)),

    ok.

%% Checks lattice laws and soundness with respect to concrete values
%% for all combinations of a set of binary types.
bin_type_lattice(_Config) ->
    Vals = [<<>>, <<"a">>, <<"b">>, <<"bc">>, <<"zz">>, <<"xyz">>,
            <<1:1>>, <<255>>, binary:copy(<<"y">>, 65)],
    Singletons = [?M:t_from_term(V) || V <- Vals],
    Structural = [?M:t_none(), ?M:t_binary(), ?M:t_bitstr(),
                  ?M:t_nonempty_binary(), ?M:t_bitstr(0, 8),
                  ?M:t_bitstr(0, 16), ?M:t_bitstr(8, 8), ?M:t_atom(ok)],
    Sets = [?M:t_sup(Ts) || Ts <- [[A, B] || A <- Singletons,
                                             B <- Singletons, A < B]],
    Types = lists:usort(Singletons ++ Structural ++ Sets),
    Member = fun(V, T) -> ?M:t_is_subtype(?M:t_from_term(V), T) end,
    Check = fun(Law, true, _Args) -> Law;
               (Law, false, Args) -> ct:fail({Law, Args})
            end,
    _ = [begin
             Sup = ?M:t_sup(A, B),
             Inf = ?M:t_inf(A, B),
             Sub = ?M:t_subtract(A, B),
             Check(sup_commutative, Sup =:= ?M:t_sup(B, A), [A, B]),
             Check(inf_commutative, Inf =:= ?M:t_inf(B, A), [A, B]),
             Check(subtype, ?M:t_is_subtype(A, B) =:= (Inf =:= A), [A, B]),
             Check(subtract_subtype, ?M:t_is_subtype(Sub, A), [A, B]),
             [begin
                  InA = Member(V, A),
                  InB = Member(V, B),
                  Check(sup_sound, not (InA orelse InB) orelse Member(V, Sup),
                        [V, A, B]),
                  Check(inf_sound, not (InA andalso InB) orelse Member(V, Inf),
                        [V, A, B]),
                  Check(subtract_sound,
                        not (InA andalso not InB) orelse Member(V, Sub),
                        [V, A, B])
              end || V <- Vals]
         end || A <- Types, B <- Types],
    _ = [Check(sup_associative,
               ?M:t_sup(A, ?M:t_sup(B, C)) =:= ?M:t_sup(?M:t_sup(A, B), C),
               [A, B, C]) ||
            A <- Types, B <- Types, C <- Types],
    ok.

absorption(Config) ->
    %% manual test: proper:quickcheck(erl_types_prop:absorption()).
    true = ct_property_test:quickcheck(erl_types_prop:absorption(), Config).

associativity(Config) ->
    %% manual test: proper:quickcheck(erl_types_prop:associativity()).
    true = ct_property_test:quickcheck(erl_types_prop:associativity(), Config).

commutativity(Config) ->
    %% manual test: proper:quickcheck(erl_types_prop:commutativity()).
    true = ct_property_test:quickcheck(erl_types_prop:commutativity(), Config).

idempotence(Config) ->
    %% manual test: proper:quickcheck(erl_types_prop:idempotence()).
    true = ct_property_test:quickcheck(erl_types_prop:idempotence(), Config).

identity(Config) ->
    %% manual test: proper:quickcheck(erl_types_prop:identity()).
    true = ct_property_test:quickcheck(erl_types_prop:identity(), Config).

limit(Config) ->
    %% manual test: proper:quickcheck(erl_types_prop:limit()).
    true = ct_property_test:quickcheck(erl_types_prop:limit(), Config).
