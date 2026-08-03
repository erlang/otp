%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

%%
%% Tests of the dictionary file compiler.
%%

-module(diameter_codegen_SUITE).

%% testcases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,

         %% The test cases
         verify_enum_encode_decode_calls_correct_module_in_inheritance_chain/1,
         verify_enum_empty_value_calls_are_not_duplicated/1
        ]).

-include("diameter_util.hrl").


%% ===========================================================================

-define(base, "base_rfc3588.dia").
-define(S, atom_to_list).
-define(L, integer_to_list).

-define(CL(F),    ?CL(F, [])).
-define(CL(F, A), ?LOG("DCG", F, A)).

-define(OPTS, [hrl, erl, forms, return]).
-define(OPTS_INHERITS, ?OPTS ++ [indirect_inherits]).

-define(DICT(Name, Prefix),
        "@id 18\n"
        "@name " Name "\n"
        "@prefix " Prefix "\n"
       ).

-define(AVP_DICT_A, ?AVP_DICT_A([
                                 "AAA 111 Unsigned32 M",
                                 "BBB 222 Unsigned32 -",
                                 "CCC 333 Unsigned64 -"])).
-define(AVP_DICT_A(Avps),
        ?DICT("diameter_test_a", "a")
        "@avp_types\n" ++
            lists:join("\n", Avps)
       ).

-define(ENUM_DICT_A,
        lists:flatten(?AVP_DICT_A ++
                          "DDD 444 Enumerated -\n" ++
                          "@enum DDD ZERO 0 ONE 1\n")
       ).

-define(ENUM_DICT_B(Avps, Inherits, Enums),
        lists:flatten(?DICT("diameter_test_b", "d") ++
                          "@avp_types\n" ++
                          lists:join("\n", Avps ++ Inherits ++ Enums))
       ).

-define(ENUM_DICT_C(Avps, Inherits, Enums),
        lists:flatten(?DICT("diameter_test_c", "c") ++
                          "@avp_types\n" ++
                          lists:join("\n", Avps ++ Inherits ++ Enums))
       ).

-define(ENUM_DICT_D(Avps, Inherits, Enums),
        lists:flatten(?DICT("diameter_test_d", "d") ++
                          "@avp_types\n" ++
                          lists:join("\n", Avps ++ Inherits ++ Enums))
       ).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 200}}].

all() ->
    [verify_enum_encode_decode_calls_correct_module_in_inheritance_chain,
     verify_enum_empty_value_calls_are_not_duplicated].

init_per_suite(Config) ->
    ?CL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?CL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:end_per_suite(Config).


init_per_testcase(Case, Config) ->
    ?CL("init_per_testcase(~w) -> entry", [Case]),
    Config.


end_per_testcase(Case, Config) when is_list(Config) ->
    ?CL("end_per_testcase(~w) -> entry", [Case]),
    Config.


%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run(List)
  when is_list(List) ->
    Tmp = ?MKTEMP("diameter_codegen"),
    try
        run(List, Tmp)
    after
        file:del_dir_r(Tmp)
    end.

%% run/2

run(List, Dir)
  when is_list(List) ->
    Path = filename:join([?LIB_DIR(diameter, src), "dict", ?base]),
    {ok, Bin} = file:read_file(Path),
    ?RUN([{{?MODULE, F, [{Bin, Dir}]}, 180000} || F <- List]);

run(F, Config) ->
    run([F], proplists:get_value(priv_dir, Config)).

%% ===========================================================================

load_forms(Forms) ->
    {ok, Mod, Bin, _} = compile:forms(Forms, [return]),
    {module, Mod} = code:load_binary(Mod, ?S(Mod), Bin),
    Mod.

%% ===========================================================================

get_enumerated_avps_for_remote_calls(Forms) ->
    {function, _, enumerated_avp, _, Clauses} = lists:keyfind(enumerated_avp, 3, Forms),
    lists:foldl(fun({clause, _, [_, {_, _, Name}, _], _, [{Type, _, Call, _}]}, AccIn) ->
                        case {Type, maps:get(Name, AccIn, undefined)} of
                            {call, undefined} ->
                                maps:put(Name, [Call], AccIn);
                            {call, Previous} ->
                                NewValue = [Call | Previous],
                                maps:put(Name, NewValue, AccIn);
                            _ ->
                                AccIn
                        end;
                   (_Other, AccIn) ->
                        AccIn
                end, maps:new(), Clauses).

%% ===========================================================================

get_empty_value_calls_for_enums(Forms) ->
    {function, _, empty_value, _, Clauses} = lists:keyfind(empty_value, 3, Forms),
    lists:filtermap(fun({clause, _, [{_, _, Name}, _], _, _}) ->
                            {true, Name};
                       (_Other) ->
                            false
                    end, Clauses).

%% ===========================================================================

get_specific_elements(Elem, List) ->
    lists:filter(fun(E) -> Elem == E end, List).

%% ===========================================================================

verify_enum_encode_decode_calls_correct_module_in_inheritance_chain(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, if enum value is inherited from module some levels
    %% above in the inheritance chain, verify that codegen created code that jumps into nearest
    %% module in the inheritance chain when encoding/decoding such value

    DictA = ?ENUM_DICT_A,

    AvpsB = ["EEE 555 Enumerated -", "FFF 666 Enumerated -"],
    InheritsB = ["@inherits diameter_test_a"],
    EnumsB = ["@enum EEE A 0 B 1", "@enum DDD TWO 2 THREE 3", "@enum FFF Z 100 Y 99"],
    DictB = ?ENUM_DICT_B(AvpsB, InheritsB, EnumsB),

    AvpsC = ["GGG 777 Enumerated -"],
    InheritsC = ["@inherits diameter_test_b"],
    EnumsC = ["@enum DDD FOUR 4 FIVE 5", "@enum FFF X 98 W 97"],
    DictC = ?ENUM_DICT_C(AvpsC, InheritsC, EnumsC),

    AvpsD = [],
    InheritsD = ["@inherits diameter_test_c"],
    EnumsD = ["@enum DDD SIX 6 SEVEN 7", "@enum FFF V 96 U 95", "@enum GGG TEN 10"],
    DictD = ?ENUM_DICT_D(AvpsD, InheritsD, EnumsD),

    {ok, [HA, EA, FA]} = diameter_make:codec(DictA, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {ok, [HB, EB, FB]} = diameter_make:codec(DictB, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = load_forms(FB),
    RB = get_enumerated_avps_for_remote_calls(FB),
    [{remote, _, {_, _, diameter_test_a}, {_, _, enumerated_avp}}] = maps:get('DDD', RB),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = load_forms(FC),
    RC = get_enumerated_avps_for_remote_calls(FC),
    [{remote, _, {_, _, diameter_test_b}, {_, _, enumerated_avp}}] = maps:get('DDD', RC),
    undefined = maps:get('EEE', RC, undefined),
    [{remote, _, {_, _, diameter_test_b}, {_, _, enumerated_avp}}] = maps:get('FFF', RC),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = load_forms(FD),
    RD = get_enumerated_avps_for_remote_calls(FD),
    [{remote, _, {_, _, diameter_test_c}, {_, _, enumerated_avp}}] = maps:get('DDD', RD),
    undefined = maps:get('EEE', RD, undefined),
    [{remote, _, {_, _, diameter_test_c}, {_, _, enumerated_avp}}] = maps:get('FFF', RD),
    undefined = maps:get('GGG', RD, undefined).

%% ===========================================================================

verify_enum_empty_value_calls_are_not_duplicated(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, if enum value is inherited from module some levels
    %% above in the inheritance chain, verify that codegen created code that

    DictA = ?ENUM_DICT_A,

    AvpsB = ["EEE 555 Enumerated -", "FFF 666 Enumerated -"],
    InheritsB = ["@inherits diameter_test_a"],
    EnumsB = ["@enum EEE A 0 B 1", "@enum DDD TWO 2 THREE 3", "@enum FFF Z 100 Y 99"],
    DictB = ?ENUM_DICT_B(AvpsB, InheritsB, EnumsB),

    AvpsC = ["GGG 777 Enumerated -"],
    InheritsC = ["@inherits diameter_test_b FFF"],
    EnumsC = ["@enum DDD FOUR 4 FIVE 5", "@enum FFF X 98 W 97"],
    DictC = ?ENUM_DICT_C(AvpsC, InheritsC, EnumsC),

    AvpsD = [],
    InheritsD = ["@inherits diameter_test_c GGG"],
    EnumsD = ["@enum DDD SIX 6 SEVEN 7", "@enum FFF V 96 U 95", "@enum GGG TEN 10"],
    DictD = ?ENUM_DICT_D(AvpsD, InheritsD, EnumsD),

    {ok, [HA, EA, FA]} = diameter_make:codec(DictA, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),
    RA = get_empty_value_calls_for_enums(FA),
    ['DDD'] = get_specific_elements('DDD', RA),
    [] = get_specific_elements('EEE', RA),
    [] = get_specific_elements('FFF', RA),
    [] = get_specific_elements('GGG', RA),

    {ok, [HB, EB, FB]} = diameter_make:codec(DictB, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = load_forms(FB),
    RB = get_empty_value_calls_for_enums(FB),
    ['DDD'] = get_specific_elements('DDD', RB),
    ['EEE'] = get_specific_elements('EEE', RB),
    ['FFF'] = get_specific_elements('FFF', RB),
    [] = get_specific_elements('GGG', RB),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = load_forms(FC),
    RC = get_empty_value_calls_for_enums(FC),
    ['DDD'] = get_specific_elements('DDD', RC),
    [] = get_specific_elements('EEE', RC),
    ['FFF'] = get_specific_elements('FFF', RC),
    [] = get_specific_elements('GGG', RC),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = load_forms(FD),
    RD = get_empty_value_calls_for_enums(FD),
    ['DDD'] = get_specific_elements('DDD', RD),
    [] = get_specific_elements('EEE', RD),
    ['FFF'] = get_specific_elements('FFF', RD),
    ['GGG'] = get_specific_elements('GGG', RD).
