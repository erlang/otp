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

-module(diameter_indirect_inherits_SUITE).

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
         verify_multiple_limited_imports_same_file/1,
         verify_multiple_whole_dict_imports_same_file/1,
         verify_multiple_limited_then_whole_dict_import_same_file/1,
         verify_both_limited_imports_are_kept_with_multiple_inherits/1,
         verify_multiple_limited_imports_are_resolved_when_overlapping/1,
         verify_limited_import_is_replaced_with_whole_dict_import/1,
         verify_whole_dict_import_is_not_replaced_with_limited_import/1,
         verify_enum_values_are_imported_along_the_inheritance_chain/1,
         verify_enum_values_are_imported_in_order_if_there_are_additional_enums_along_the_chain/1,
         verify_grouped_avps_are_indirectly_inherited/1,
         verify_vendor_ids_are_indirectly_inherited/1,
         verify_vendor_ids_can_only_be_overridden_by_inheriting_module/1,
         verify_inherits_can_be_added_by_options/1,
         verify_inherits_can_be_reset_by_options/1,
         verify_inherits_can_be_overridden_by_options/1
        ]).

-include("diameter_util.hrl").


%% ===========================================================================

-define(base, "base_rfc3588.dia").
-define(S, atom_to_list).
-define(L, integer_to_list).

-define(CL(F),    ?CL(F, [])).
-define(CL(F, A), ?LOG("DII", F, A)).

-define(OPTS, [hrl, erl, forms, return]).
-define(OPTS_INHERITS, ?OPTS ++ [indirect_inherits]).

-define(AAA, "AAA 111 Unsigned32 M").
-define(BBB, "BBB 222 Unsigned32 -").
-define(CCC, "CCC 333 Unsigned64 -").
-define(DEFAULT_AVPS, [?AAA, ?BBB, ?CCC]).

-define(DEFAULT_GROUPED_AVPS, ["HHH 888 Grouped -"]).
-define(DEFAULT_AVPS_WITH_GROUPS, ?DEFAULT_AVPS ++ ?DEFAULT_GROUPED_AVPS).

-define(VENDOR_AVPS_A, ["III 999 Unsigned32 V", "JJJ 1111 Enumerated V"]).
-define(VENDOR_AVPS_B, ["KKK 2222 Grouped V"]).
-define(VENDOR_AVPS_C, ["LLL 3333 Grouped V"]).

-define(VENDOR_AVPS_AB, ?VENDOR_AVPS_A ++ ?VENDOR_AVPS_B).
-define(VENDOR_AVPS_BC, ?VENDOR_AVPS_B ++ ?VENDOR_AVPS_C).
-define(VENDOR_AVPS_ABC, ?VENDOR_AVPS_A ++ ?VENDOR_AVPS_B ++ ?VENDOR_AVPS_C).

-define(DEFAULT_VENDOR_IDS, #{
                              'III' => 1,
                              'JJJ' => 1,
                              'KKK' => 2,
                              'LLL' => 3
                             }).

-define(AVP_DICT_A, ?AVP_DICT_A(?DEFAULT_AVPS)).
-define(AVP_DICT_A(Avps),
        lists:flatten(dict("diameter_test_a", "a", Avps) ++
                          get_messages_from_avps("A", "1", Avps))
       ).

-define(AVP_DICT_B(Inherits),
        lists:flatten(dict("diameter_test_b", "b", [], Inherits) ++
                          get_messages_from_avps("B", "2", Inherits))
       ).

-define(AVP_DICT_C(Inherits), ?AVP_DICT_C(Inherits, Inherits)).
-define(AVP_DICT_C(Inherits, KnownAvps),
        lists:flatten(dict("diameter_test_c", "c", [], Inherits) ++
                          get_messages_from_avps("C", "3", KnownAvps))
       ).

-define(AVP_DICT_D(Inherits), ?AVP_DICT_D(Inherits, Inherits)).
-define(AVP_DICT_D(Inherits, KnownAvps),
        lists:flatten(dict("diameter_test_d", "d", [], Inherits) ++
                          get_messages_from_avps("D", "4", KnownAvps))
       ).

-define(ENUM_DICT_A,
        lists:flatten(dict("diameter_test_a", "a", ?DEFAULT_AVPS) ++
                          "DDD 444 Enumerated -\n" ++
                          "@enum DDD ZERO 0 ONE 1\n" ++
                          get_messages_from_avps("A", "1", ["AAA", "BBB", "CCC", "DDD"]))
       ).

-define(ENUM_DICT_B,
        lists:flatten(dict("diameter_test_b", "b", [], ["@inherits diameter_test_a"]) ++
                          "@enum DDD TWO 2 THREE 3\n" ++
                          get_messages_from_avps("B", "2", ["AAA", "BBB", "CCC", "DDD"]))
       ).

-define(ENUM_DICT_B(Avps, Inherits, Enums),
        lists:flatten(dict("diameter_test_b", "d", Avps, Inherits) ++
                          lists:join("\n", Enums) ++
                          get_messages_from_avps("B", "2", Avps))
       ).

-define(ENUM_DICT_C,
        lists:flatten(dict("diameter_test_c", "c", [], ["@inherits diameter_test_b"]) ++
                          "@enum DDD FOUR 4 FIVE 5\n" ++
                          get_messages_from_avps("C", "3", ["AAA", "BBB", "CCC", "DDD"]))
       ).

-define(ENUM_DICT_C(Avps, Inherits, Enums),
        lists:flatten(dict("diameter_test_c", "c", Avps, Inherits) ++
                          lists:join("\n", Enums) ++
                          get_messages_from_avps("C", "3", Avps))
       ).

-define(ENUM_DICT_D,
        lists:flatten(dict("diameter_test_d", "d", [], ["@inherits diameter_test_c"]) ++
                          "@enum DDD SIX 6 SEVEN 7\n" ++
                          get_messages_from_avps("D", "4", ["AAA", "BBB", "CCC", "DDD"]))
       ).

-define(ENUM_DICT_D(Avps, Inherits, Enums),
        lists:flatten(dict("diameter_test_d", "d", Avps, Inherits) ++
                          lists:join("\n", Enums) ++
                          get_messages_from_avps("D", "4", Avps))
       ).

-define(GROUPED_DICT_A,
        lists:flatten(dict("diameter_test_a", "a", ?DEFAULT_AVPS_WITH_GROUPS) ++
                          get_groups_from_avps(?DEFAULT_AVPS_WITH_GROUPS) ++
                          get_messages_from_avps("A", "1", ?DEFAULT_AVPS_WITH_GROUPS))
       ).

-define(GROUPED_DICT_B,
        lists:flatten(dict("diameter_test_b", "b", [], ["@inherits diameter_test_a"]) ++
                          get_messages_from_avps("B", "2", ?DEFAULT_AVPS_WITH_GROUPS))
       ).

-define(GROUPED_DICT_C,
        lists:flatten(dict("diameter_test_c", "c", [], ["@inherits diameter_test_b"]) ++
                          get_messages_from_avps("C", "3", ?DEFAULT_AVPS_WITH_GROUPS))
       ).

-define(VENDOR_DICT_A,
        lists:flatten(dict("diameter_test_a", "a", ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A, [], "1 A") ++
                          get_messages_from_avps("A", "1", ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A))
       ).

-define(VENDOR_DICT_B, ?VENDOR_DICT_B([])).
-define(VENDOR_DICT_B(VendorIds),
        lists:flatten(dict("diameter_test_b", "b", ?VENDOR_AVPS_B, ["@inherits diameter_test_a"], "2 B") ++
                          lists:join("\n", VendorIds) ++
                          get_groups_from_avps(?DEFAULT_AVPS ++ ?VENDOR_AVPS_AB) ++
                          get_messages_from_avps("B", "2", ?DEFAULT_AVPS ++ ?VENDOR_AVPS_AB))
       ).

-define(VENDOR_DICT_C, ?VENDOR_DICT_C([])).
-define(VENDOR_DICT_C(VendorIds),
        lists:flatten(dict("diameter_test_c", "c", ?VENDOR_AVPS_C, ["@inherits diameter_test_b"], "3 C") ++
                          lists:join("\n", VendorIds) ++
                          get_groups_from_avps(?DEFAULT_AVPS ++ ?VENDOR_AVPS_A ++ ?VENDOR_AVPS_C) ++
                          get_messages_from_avps("C", "3", ?DEFAULT_AVPS ++ ?VENDOR_AVPS_ABC))
       ).

-define(OPTIONS_DICT_A,
        lists:flatten(dict("diameter_test_a", "a", [?AAA, ?BBB]) ++
                          get_messages_from_avps("A", "1", [?AAA, ?BBB]))
       ).

-define(OPTIONS_DICT_B, ?OPTIONS_DICT_B(["@inherits diameter_test_a AAA"])).
-define(OPTIONS_DICT_B(Inherits),
        lists:flatten(dict("diameter_test_b", "b", [?CCC], Inherits) ++
                          get_messages_from_avps("B", "2", [?AAA, ?CCC]))
       ).

-define(OPTIONS_DICT_C, ?OPTIONS_DICT_C(?DEFAULT_AVPS)).
-define(OPTIONS_DICT_C(KnownAvps),
        lists:flatten(dict("diameter_test_c", "c", [], ["@inherits diameter_test_b CCC"]) ++
                          get_messages_from_avps("C", "3", KnownAvps))
       ).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 200}}].

all() ->
    [verify_multiple_limited_imports_same_file,
     verify_multiple_whole_dict_imports_same_file,
     verify_multiple_limited_then_whole_dict_import_same_file,
     verify_both_limited_imports_are_kept_with_multiple_inherits,
     verify_multiple_limited_imports_are_resolved_when_overlapping,
     verify_limited_import_is_replaced_with_whole_dict_import,
     verify_whole_dict_import_is_not_replaced_with_limited_import,
     verify_enum_values_are_imported_along_the_inheritance_chain,
     verify_enum_values_are_imported_in_order_if_there_are_additional_enums_along_the_chain,
     verify_grouped_avps_are_indirectly_inherited,
     verify_vendor_ids_are_indirectly_inherited,
     verify_vendor_ids_can_only_be_overridden_by_inheriting_module,
     verify_inherits_can_be_added_by_options,
     verify_inherits_can_be_reset_by_options,
     verify_inherits_can_be_overridden_by_options].

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
    Tmp = ?MKTEMP("diameter_indirect_inherits"),
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

dict(Name, Prefix) ->
    dict(Name, Prefix, []).
dict(Name, Prefix, Avps) ->
    dict(Name, Prefix, Avps, []).
dict(Name, Prefix, Avps, Inherits) ->
    dict(Name, Prefix, Avps, Inherits, undefined).
dict(Name, Prefix, Avps, Inherits, Vendor) ->
    Dict = "@id 18\n" ++
        "@name " ++ Name ++ "\n" ++
        "@prefix " ++ Prefix ++ "\n",
    VendorRes = case Vendor of
                    undefined -> "";
                    _ -> "@vendor " ++ Vendor ++ "\n"
                end,
    AvpsRes = case Avps of
                  [] -> "";
                  _ -> "@avp_types\n" ++ lists:join("\n", Avps) ++ "\n"
              end,
    InheritsRes = case Inherits of
                      [] -> "";
                      _ -> lists:join("\n", Inherits) ++ "\n"
                  end,
    Dict ++ VendorRes ++ AvpsRes ++ InheritsRes.

%% ===========================================================================

load_forms(Forms) ->
    {ok, Mod, Bin, _} = compile:forms(Forms, [return]),
    {module, Mod} = code:load_binary(Mod, ?S(Mod), Bin),
    Mod.

%% ===========================================================================

get_avp_names(Avps) ->
    lists:map(fun get_avp_name/1, Avps).

%% ===========================================================================

get_avp_name(Name) when is_atom(Name) ->
    Name;
get_avp_name(Avp) when is_list(Avp) ->
    [Name | _] = string:split(Avp, " "),
    list_to_atom(Name).

%% ===========================================================================

avp_header(Name, VendorIds) ->
    VendorId = maps:get(Name, VendorIds, undefined),
    case Name of
        'AAA' -> {111, 64, undefined};
        'BBB' -> {222, 0, undefined};
        'CCC' -> {333, 0, undefined};
        'DDD' -> {444, 0, undefined};
        'EEE' -> {555, 0, undefined};
        'FFF' -> {666, 0, undefined};
        'GGG' -> {777, 0, undefined};
        'HHH' -> {888, 0, undefined};
        'III' -> {999, 128, VendorId};
        'JJJ' -> {1111, 128, VendorId};
        'KKK' -> {2222, 128, VendorId};
        'LLL' -> {3333, 128, VendorId}
    end.

%% ===========================================================================

verify_avps(M, PresentAvps) ->
    verify_avps(M, ?DEFAULT_VENDOR_IDS, PresentAvps).

verify_avps(M, VendorIds, PresentAvps) when is_map(VendorIds) ->
    verify_avps(M, VendorIds, PresentAvps, []);

verify_avps(M, PresentAvps, NotPresentAvps) ->
    verify_avps(M, ?DEFAULT_VENDOR_IDS, PresentAvps, NotPresentAvps).

verify_avps(M, VendorIds, PresentAvps, NotPresentAvps) ->
    lists:foreach(fun(Avp) ->
                          Name = get_avp_name(Avp),
                          Header = avp_header(Name, VendorIds),
                          Header = M:avp_header(Name)
                  end, PresentAvps),
    lists:foreach(fun(Avp) ->
                          Name = get_avp_name(Avp),
                          {'EXIT', {badarg, _}} = catch M:avp_header(Name)
                  end, NotPresentAvps).

%% ===========================================================================

verify_enum_values(M, Name, PresentValues) ->
    verify_enum_values(M, Name, PresentValues, []).

verify_enum_values(M, Name, PresentValues, NotPresentValues) ->
    lists:foreach(fun(Value) ->
                          <<0, 0, 0, Value>> = Encoded = M:enumerated_avp(encode, Name, Value),
                          Value = M:enumerated_avp(decode, Name, Encoded)
                  end, PresentValues),
    lists:foreach(fun(Value) ->
                          {'EXIT', {badarg, _}} = catch M:enumerated_avp(encode, Name, Value),
                          {'EXIT', {badarg, _}} = catch M:enumerated_avp(decode, Name, <<0, 0, 0, Value>>)
                  end, NotPresentValues).

%% ===========================================================================

verify_group_avps(M, Groups, PresentAvps) ->
    PresentNames = get_avp_names(PresentAvps),
    lists:foreach(fun(Group) ->
                          GroupName = get_avp_name(Group),
                          Avps = M:avp_arity(GroupName),
                          lists:foreach(fun(Name) ->
                                                {Name, _Arity} = lists:keyfind(Name, 1, Avps)
                                        end, PresentNames)
                  end, Groups).

%% ===========================================================================

codec_list_of_options(Dict) ->
    codec_list_of_options(Dict, [?OPTS, ?OPTS_INHERITS]).

codec_list_of_options(Dict, [First | _] = ListOfOpts) when not is_list(First) ->
    codec_list_of_options(Dict, [ListOfOpts]);
codec_list_of_options(Dict, ListsOfOpts) ->
    lists:foldl(fun(Opts, ignore) -> diameter_make:codec(Dict, Opts);
                   (Opts, Acc) -> Acc = diameter_make:codec(Dict, Opts)
                end, ignore, ListsOfOpts).

%% ===========================================================================

get_messages_from_avps(_Prefix, _Id, []) ->
    "";
get_messages_from_avps(Prefix, Id, AvpsOrInherits) ->
    MapFun = fun(Avp) ->
                     [Name | _] = string:split(Avp, " "),
                     "  [ " ++ Name ++ " ]"
             end,
    FoldFun = fun(AvpOrInherit, Acc) ->
                      case string:split(AvpOrInherit, " ", all) of
                          ["@inherits", _] ->
                              lists:append(Acc, lists:map(MapFun, ?DEFAULT_AVPS));
                          ["@inherits", _ | Avps] ->
                              lists:append(Acc, lists:map(MapFun, Avps));
                          [Name | _] ->
                              lists:append(Acc, lists:map(MapFun, [Name]))
                      end
              end,
    Names = lists:usort(lists:foldl(FoldFun, [], AvpsOrInherits)),
    "\n@messages\n" ++
        Prefix ++ "R ::= < Diameter Header: " ++ Id ++ ", REQ >\n" ++
        lists:join("\n", Names) ++
        "\n* [ AVP ]\n" ++
        Prefix ++ "A ::= < Diameter Header: " ++ Id ++ " >\n" ++
        lists:join("\n", Names) ++
        "\n* [ AVP ]\n".

%% ===========================================================================

get_groups_from_avps([]) ->
    "";
get_groups_from_avps(Avps) ->
    Fun = fun(Avp, {GroupedAcc, OtherAcc}) ->
                  case string:split(Avp, " ", all) of
                      [Name, Code, "Grouped", _Flags] ->
                          {lists:append(GroupedAcc, [{Name, Code}]), OtherAcc};
                      [Name, _Code, _Type, _Flags] ->
                          {GroupedAcc, lists:append(OtherAcc, [Name])}
                  end
          end,
    {GroupedAvps, OtherAvps} = lists:foldl(Fun, {[], []}, Avps),
    "@grouped\n" ++ lists:map(fun({Name, Code}) ->
                                      Name ++
                                          " ::= < AVP Header: " ++
                                          Code ++
                                          " >\n  [ " ++
                                          lists:join(" ]\n  [ ", OtherAvps) ++
                                          " ]\n* [ AVP ]\n"
                              end, GroupedAvps).

%% ===========================================================================

verify_multiple_limited_imports_same_file(_) ->
    %% This test checks that when you inherit same avp twice you get avp_already_defined error.
    DictA = ?AVP_DICT_A,
    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA", "@inherits diameter_test_a AAA"]),

    {ok, [HA, EA, FA]} = diameter_make:codec(DictA, ?OPTS),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {error, {avp_already_defined, _}} = codec_list_of_options(DictB).

%% ===========================================================================

verify_multiple_whole_dict_imports_same_file(_) ->
    %% This test checks that when you inherit same dictionary twice you get duplicate_import error.
    DictA = ?AVP_DICT_A,
    DictB = ?AVP_DICT_B(["@inherits diameter_test_a", "@inherits diameter_test_a"]),

    {ok, [HA, EA, FA]} = diameter_make:codec(DictA, ?OPTS),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {error, {duplicate_import, _}} = codec_list_of_options(DictB).

%% ===========================================================================

verify_multiple_limited_then_whole_dict_import_same_file(_) ->
    %% This test checks that when you inherit avp from dictionary, and then inherit whole
    %% dictionary, you get duplicate_import error.
    DictA = ?AVP_DICT_A,

    Inherits = [
                "@inherits diameter_test_a AAA",
                "@inherits diameter_test_a BBB",
                "@inherits diameter_test_a"
               ],
    DictB = ?AVP_DICT_B(Inherits),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {error, {duplicate_import, _}} = codec_list_of_options(DictB).

%% ===========================================================================

verify_both_limited_imports_are_kept_with_multiple_inherits(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits one avp from dict a
    %% then dict c inherits one avp from dict a AND inherits dict b, dict c and dict d should have
    %% inherited both avps of dict a.
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a BBB", "@inherits diameter_test_b"], ["AAA", "BBB"]),
    %% Check reverse order as DictC
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a BBB"], ["AAA", "BBB"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"], ["AAA", "BBB"]),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = load_forms(FB),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ['AAA', 'BBB'], ['CCC']),

    {ok, [HC_R, EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC_R, EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ['AAA', 'BBB'], ['CCC']),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ['AAA', 'BBB'], ['CCC']).

%% ===========================================================================

verify_multiple_limited_imports_are_resolved_when_overlapping(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits AAA and BBB from dict a,
    %% dict c inherits dict b and BBB and CCC from dict a, dict c and dict d should have resolved
    %% inherits, so that they see AAA, BBB, CCC
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA BBB"]),
    %% Check reverse order as DictB
    DictB_R = ?AVP_DICT_B(["@inherits diameter_test_a BBB AAA"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a BBB CCC", "@inherits diameter_test_b"]),
    %% Check reverse order as DictB
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a BBB CCC"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVPS),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ['AAA', 'BBB']),

    {ok, [HB_R, EB_R, FB_R]} = codec_list_of_options(DictB_R),
    ct:pal("~s~n~s~n", [HB_R, EB_R]),
    diameter_test_b = MB_R = load_forms(FB_R),
    verify_avps(MB_R, ['AAA', 'BBB']),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS),

    {ok, [HC_R, EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC_R, EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ?DEFAULT_AVPS),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVPS).

%% ===========================================================================

verify_limited_import_is_replaced_with_whole_dict_import(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits one avp from dict a
    %% then dict c inherits whole dict a AND inherits dict b, dict c and dict d should have
    %% inherited all avps of dict a.
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a AAA"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a", "@inherits diameter_test_b"]),
    %% Check reverse order as DictC
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = load_forms(FB),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS),

    {ok, [HC_R, EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC_R, EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ?DEFAULT_AVPS),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVPS).

%% ===========================================================================

verify_whole_dict_import_is_not_replaced_with_limited_import(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict b inherits whole dict a
    %% then dict c inherits one avp from dict a AND inherits dict b, dict c and dict d should have
    %% inherited all avps of dict a.
    DictA = ?AVP_DICT_A,

    DictB = ?AVP_DICT_B(["@inherits diameter_test_a"]),

    DictC = ?AVP_DICT_C(["@inherits diameter_test_a AAA", "@inherits diameter_test_b"]),
    %% Check reverse order as Dict3
    DictC_R = ?AVP_DICT_C(["@inherits diameter_test_b", "@inherits diameter_test_a AAA"]),

    DictD = ?AVP_DICT_D(["@inherits diameter_test_c"]),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = load_forms(FA),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = load_forms(FB),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS),

    {ok, [HC_R, EC_R, FC_R]} = diameter_make:codec(DictC_R, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC_R, EC_R]),
    diameter_test_c = MC_R = load_forms(FC_R),
    verify_avps(MC_R, ?DEFAULT_AVPS),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVPS).

%% ===========================================================================

verify_enum_values_are_imported_along_the_inheritance_chain(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict a defines an enum with 2 values,
    %% and then each dict in the chain adds additional values, the last dict should
    %% have all enum values inherited.

    DictA = ?ENUM_DICT_A,
    DictB = ?ENUM_DICT_B,
    DictC = ?ENUM_DICT_C,
    DictD = ?ENUM_DICT_D,

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVPS ++ ['DDD']),
    verify_enum_values(MA, 'DDD', [0, 1], [2, 3, 4, 5, 6, 7]),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ?DEFAULT_AVPS ++ ['DDD']),
    verify_enum_values(MB, 'DDD', [0, 1, 2, 3], [4, 5, 6, 7]),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS ++ ['DDD']),
    verify_enum_values(MC, 'DDD', [0, 1, 2, 3, 4, 5], [6, 7]),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVPS ++ ['DDD']),
    verify_enum_values(MD, 'DDD', [0, 1, 2, 3, 4, 5, 6, 7]).

%% ===========================================================================

verify_enum_values_are_imported_in_order_if_there_are_additional_enums_along_the_chain(_) ->
    %% Given dictionaries a <-- b <-- c <-- d, when dict a defined an enum with 2 values,
    %% and then each dict in the chain adds additional values, and also adds additional enum avps
    %% the last dict should have all enum values inherited from all imported avps, if
    %% enum was not imported along the chain, it should not be present

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

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVPS ++ ['DDD'], ['EEE', 'FFF', 'GGG']),
    verify_enum_values(MA, 'DDD', [0, 1], [2, 3, 4, 5, 6, 7]),
    verify_enum_values(MA, 'EEE', [], [0, 1]),
    verify_enum_values(MA, 'FFF', [], [100, 99, 98, 97, 96, 95]),
    verify_enum_values(MA, 'GGG', [], [10]),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ?DEFAULT_AVPS ++ ['DDD', 'EEE', 'FFF'], ['GGG']),
    verify_enum_values(MB, 'DDD', [0, 1, 2, 3], [4, 5, 6, 7]),
    verify_enum_values(MB, 'EEE', [0, 1]),
    verify_enum_values(MB, 'FFF', [100, 99], [98, 97, 96, 95]),
    verify_enum_values(MB, 'GGG', [], [10]),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS ++ ['DDD', 'FFF', 'GGG'], ['EEE']),
    verify_enum_values(MC, 'DDD', [0, 1, 2, 3, 4, 5], [6, 7]),
    verify_enum_values(MC, 'EEE', [], [0, 1]),
    verify_enum_values(MC, 'FFF', [100, 99, 98, 97], [96, 95]),
    verify_enum_values(MC, 'GGG', [], [10]),

    {ok, [HD, ED, FD]} = diameter_make:codec(DictD, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HD, ED]),
    diameter_test_d = MD = load_forms(FD),
    verify_avps(MD, ?DEFAULT_AVPS ++ ['DDD', 'FFF'], ['EEE']),
    verify_enum_values(MD, 'DDD', [0, 1, 2, 3, 4, 5, 6, 7]),
    verify_enum_values(MD, 'EEE', [], [0, 1]),
    verify_enum_values(MD, 'FFF', [100, 99, 98, 97, 96, 95]),
    verify_enum_values(MD, 'GGG', [10]).

%% ===========================================================================

verify_grouped_avps_are_indirectly_inherited(_) ->
    %% Given dictionaries a <- b <- c, when dictionary a defines grouped avp
    %% and then dict b inherits it from a, and dict c inherits dict b, the
    %% grouped avp should be available in dict c

    DictA = ?GROUPED_DICT_A,
    DictB = ?GROUPED_DICT_B,
    DictC = ?GROUPED_DICT_C,

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVPS_WITH_GROUPS),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ?DEFAULT_AVPS_WITH_GROUPS),
    verify_group_avps(MB, ?DEFAULT_GROUPED_AVPS, ?DEFAULT_AVPS),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS_WITH_GROUPS),
    verify_group_avps(MC, ?DEFAULT_GROUPED_AVPS, ?DEFAULT_AVPS).

%% ===========================================================================

verify_vendor_ids_are_indirectly_inherited(_) ->
    %% Given dictionaries a <- b <- c, when dictionary a defines vendor avp
    %% and then dict b inherits it from a, and dict c inherits dict b, the
    %% vendor avp should be available in dict c

    DictA = ?VENDOR_DICT_A,
    DictB = ?VENDOR_DICT_B,
    DictC = ?VENDOR_DICT_C,

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A, ?VENDOR_AVPS_BC),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_AB, ?VENDOR_AVPS_C),
    verify_group_avps(MB, ?VENDOR_AVPS_B, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_ABC),
    verify_group_avps(MC, ?VENDOR_AVPS_BC, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A).

%% ===========================================================================

verify_vendor_ids_can_only_be_overridden_by_inheriting_module(_) ->
    %% Given dictionaries a <- b <- c, when dictionary a defines vendor avp
    %% and then dict b inherits it from a, overrides it's vendor id, it should see the overridden
    %% vendor id. Then when dict c inherits this avp, and overrides it again, it should see the
    %% overridden vendor id as set by dict c, not dict b. Vendor id that was overridden by dict b
    %% but wasn't by dict c should have it's default vendor id as defined in dict a.
    DictA = ?VENDOR_DICT_A,
    DictB = ?VENDOR_DICT_B(["@avp_vendor_id 4 III"]),
    DictC = ?VENDOR_DICT_C(["@avp_vendor_id 5 JJJ KKK"]),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A, ?VENDOR_AVPS_BC),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    VB = maps:merge(?DEFAULT_VENDOR_IDS, #{'III' => 4}),
    verify_avps(MB, VB, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_AB, ?VENDOR_AVPS_C),
    verify_group_avps(MB, ?VENDOR_AVPS_B, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A),

    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, ?OPTS_INHERITS),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    VC = maps:merge(?DEFAULT_VENDOR_IDS, #{'JJJ' => 5, 'KKK' => 5}),
    verify_avps(MC, VC, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_ABC),
    verify_group_avps(MC, ?VENDOR_AVPS_BC, ?DEFAULT_AVPS ++ ?VENDOR_AVPS_A).

%% ===========================================================================

verify_inherits_can_be_added_by_options(_) ->
    %% Given dictionaries a <- b <- c, dict a having defined avp's AAA and BBB, but dict B inherits
    %% only AAA from a, then dict c inherits CCC from dict b, while also adding inherit to whole
    %% dict a via options, dict c should see all avps.
    DictA = ?OPTIONS_DICT_A,
    DictB = ?OPTIONS_DICT_B,
    DictC = ?OPTIONS_DICT_C,

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, [?AAA, ?BBB], [?CCC]),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, [?AAA, ?CCC], [?BBB]),

    OptsC = [{inherits, "diameter_test_a"} | ?OPTS_INHERITS],
    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, OptsC),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS).

%% ===========================================================================

verify_inherits_can_be_reset_by_options(_) ->
    %% Given dictionaries a <- b <- c, dict a having defined avp's AAA and BBB, but dict B inherits
    %% only AAA from a, then dict c inherits CCC from dict b, while also resetting inherits via options,
    %% dict c should see no avps, because it does not define any itself.
    DictA = ?OPTIONS_DICT_A,
    DictB = ?OPTIONS_DICT_B,
    DictC = ?OPTIONS_DICT_C([]),

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, [?AAA, ?BBB], [?CCC]),

    {ok, [HB, EB, FB]} = codec_list_of_options(DictB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, [?AAA, ?CCC], [?BBB]),

    OptsC = [{inherits, "-"} | ?OPTS_INHERITS],
    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, OptsC),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, [], ?DEFAULT_AVPS).

%% ===========================================================================

verify_inherits_can_be_overridden_by_options(_) ->
    %% Given dictionaries a <- b <- c, dict a having defined avp's AAA and BBB, but dict B inherits
    %% non-existing dictionary d, but is compiled with options to replace this inherit with inherit
    %% of dict a. After that when dict c inherits CCC from dict b, it should also get all avps from
    %% dict a. Dict c is compiled with options to replace inherit of dict a, from dict b, and that
    %% should be a no-op since dict c does not directly inherit dict a, so it cannot modify inherits
    %% of dict b.
    DictA = ?OPTIONS_DICT_A,
    DictB = ?OPTIONS_DICT_B(["@inherits diameter_test_d"]),
    DictC = ?OPTIONS_DICT_C,

    {ok, [HA, EA, FA]} = codec_list_of_options(DictA),
    ct:pal("~s~n~s~n", [HA, EA]),
    diameter_test_a = MA = load_forms(FA),
    verify_avps(MA, [?AAA, ?BBB], [?CCC]),

    InheritsB = {inherits, "diameter_test_d/diameter_test_a"},
    OptsB = [[InheritsB | ?OPTS], [InheritsB | ?OPTS_INHERITS]],
    {ok, [HB, EB, FB]} = codec_list_of_options(DictB, OptsB),
    ct:pal("~s~n~s~n", [HB, EB]),
    diameter_test_b = MB = load_forms(FB),
    verify_avps(MB, ?DEFAULT_AVPS),

    OptsC = [{inherits, "diameter_test_a/diameter_test_d"} | ?OPTS_INHERITS],
    {ok, [HC, EC, FC]} = diameter_make:codec(DictC, OptsC),
    ct:pal("~s~n~s~n", [HC, EC]),
    diameter_test_c = MC = load_forms(FC),
    verify_avps(MC, ?DEFAULT_AVPS).
