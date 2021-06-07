%% -*- erlang-indent-level: 4 -*-
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
-module(erl_types_SUITE).

-export([all/0,
         consistency_and_to_string/1]).

%% Simplify calls into erl_types and avoid importing the entire module.
-define(M, erl_types).

-include_lib("common_test/include/ct.hrl").

all() ->
    [consistency_and_to_string].

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
