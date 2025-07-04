%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2001-2025. All Rights Reserved.
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
%% Purpose: Test suite for the ASN.1 application

-module(asn1_SUITE).

%% Suppress compilation of an addititional module compiled for maps.
-define(NO_MAPS_MODULE, asn1_test_lib_no_maps).

-define(only_ber(Func),
    if Rule =:= ber -> Func;
       true -> ok
    end).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%------------------------------------------------------------------------------
%% Suite definition
%%------------------------------------------------------------------------------

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap,{minutes,60}}].

all() ->
    [xref,
     xref_export_all,

     c_string,
     constraint_equivalence,

     ber_decode_invalid_length,
     ber_choiceinseq,
     ber_optional,
     tagdefault_automatic,

     cover,

     parse,
     test_undecoded_rest,
     specialized_decodes,
     special_decode_performance,
     exclusive_decode_rest,

     testMegaco,
     testConstraints,
     testCompactBitString,
     default,
     testPrim,
     rtUI,
     testPrimStrings,

     per,
     ber,
     der,

     h323test,
     testExtensibilityImplied,
     testChoice,
     testDefaultOctetString,
     testMultipleLevels,
     testOpt,
     testSeqDefault,
     testMaps,

     testTypeValueNotation,

     testExternal,

     testSeqExtension,
     testSeqOptional,
     testSeqPrim,
     testSeqTypeRefCho,
     testSeqTypeRefPrim,
     testSeqTypeRefSeq,
     testSeqTypeRefSet,

     testSeqOf,
     testSeqOfIndefinite,
     testSeqOfCho,
     testSeqOfChoExt,

     testExtensionAdditionGroup,

     testSet,
     testSetOf,

     testEnumExt,
     value_test,
     testSeq2738,
     constructed,
     ber_decode_error,
     otp_14440,
     testSeqSetIndefinite,
     testChoiceIndefinite,
     per_open_type,
     testInfObjectClass,
     testUniqueObjectSets,
     testInfObjExtract,
     testParam,
     testFragmented,
     testMergeCompile,
     testobj,
     testDeepTConstr,
     testImport,
     testDER,
     testDEFAULT,
     testExtensionDefault,
     testMvrasn6,
     testContextSwitchingTypes,
     testOpenTypeImplicitTag,
     testROSE,
     testINSTANCE_OF,
     testTCAP,
     test_ParamTypeInfObj,
     test_Defed_ObjectIdentifier,
     testSelectionType,
     testSSLspecs,
     testNortel,
     test_WS_ParamClass,
     test_modified_x420,
     testContaining,

     %% Some heavy tests.
     testTcapsystem,
     testNBAPsystem,
     testS1AP,
     testRfcs,

     test_compile_options,
     testDoubleEllipses,
     test_x691,
     ticket_6143,
     test_OTP_9688,
     testValueTest,

     testComment,
     testName2Number,
     ticket_7407,
     ticket7904,

     {group, performance}].

groups() ->
    [{performance, [],
      [testTimer_ber,
       testTimer_ber_maps,
       testTimer_per,
       testTimer_per_maps,
       testTimer_uper,
       testTimer_uper_maps]}].

%%------------------------------------------------------------------------------
%% Init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config) ->
    CaseDir = filename:join(proplists:get_value(priv_dir, Config), Func),
    ok = filelib:ensure_dir(filename:join([CaseDir, dummy_file])),
    true = code:add_patha(CaseDir),
    [{case_dir, CaseDir}|Config].

end_per_testcase(_Func, Config) ->
    CaseDir = proplists:get_value(case_dir, Config),
    unload_modules(CaseDir),
    asn1_test_lib:rm_dirs([CaseDir]),
    code:del_path(CaseDir).

unload_modules(CaseDir) ->
    F = fun(Name0, Acc) ->
                Name1 = filename:rootname(filename:basename(Name0)),
                Name = list_to_existing_atom(Name1),
                [Name|Acc]
        end,
    Beams1 = lists:usort(filelib:fold_files(CaseDir, "[.]beam\$", true, F, [])),
    Beams = [M || M <- Beams1, code:is_loaded(M) =/= false],
    _ = [begin
             code:purge(M),
             code:delete(M),
             code:purge(M),
             io:format("Unloaded ~p", [M])
         end || M <- Beams],
    ok.

%%------------------------------------------------------------------------------
%% Test runners
%%------------------------------------------------------------------------------

test(Config, TestF) ->
    test(Config, TestF, [per, uper, ber, jer, {ber,[ber,jer]}]).

test(Config, TestF, Rules) ->
    Fun = fun(C, R, O) ->
                  M = element(2, erlang:fun_info(TestF, module)),
                  F = element(2, erlang:fun_info(TestF, name)),
                  io:format("Running ~p:~p with ~p...~n", [M, F, {R, O}]),
                  try
                      TestF(C, R, O)
                  catch
                      Class:Reason:Stk ->
                          NewReason = {Reason, [{rule, R}, {options, O}]},
                          erlang:raise(Class, NewReason, Stk)
                  end
          end,
    Result = [run_case(Config, Fun, rule(Rule), opts(Rule)) || Rule <- Rules],
    case lists:usort(Result) of
        [true|_Skips] -> true; % At least one test ran
        Skips        -> {skip, [R || {skip, R} <- Skips]} % All skipped
    end.

rule(A) when is_atom(A) -> A;
rule({A, _Opts} )       -> A.

opts(Rule) when is_atom(Rule) -> [];
opts({_Rule, Opts})         -> Opts.

run_case(Config, Fun, Rule, Opts) ->
    CaseDir = proplists:get_value(case_dir, Config),
    Dir = filename:join([CaseDir, join(Rule, Opts)]),
    ok = filelib:ensure_dir(filename:join([Dir, dummy_file])),
    replace_path(CaseDir, Dir),
    NewConfig = lists:keyreplace(case_dir, 1, Config, {case_dir, Dir}),

    % Run the actual test function
    Result = Fun(NewConfig, Rule, Opts),

    replace_path(Dir, CaseDir),
    case Result of
        {skip, _Reason} -> Result;
        _               -> true
    end.

replace_path(PathA, PathB) ->
    true = code:del_path(PathA),
    true = code:add_patha(PathB).

join(Rule, Opts) ->
    lists:join("_", [atom_to_list(Rule)|lists:map(fun atom_to_list/1, Opts)]).

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

%% Cover run-time functions that are only called by the ASN.1 compiler
%% (if any).
cover(_) ->
    Wc = filename:join([code:lib_dir(asn1),"ebin","asn1ct_eval_*.beam"]),
    Beams = filelib:wildcard(Wc),
    true = Beams =/= [],
    [begin
	 M0 = filename:basename(Beam),
	 M1 = filename:rootname(M0),
	 M = list_to_atom(M1),
	 "asn1ct_eval_" ++ Group0 = M1,
	 Group = list_to_atom(Group0),
	 io:format("%%\n"
		   "%% ~s\n"
		   "%%\n", [M]),
	 asn1ct_func:start_link(),
	 [asn1ct_func:need({Group,F,A}) ||
	     {F,A} <- M:module_info(exports), F =/= module_info],
	 asn1ct_func:generate(group_leader())
     end || Beam <- Beams],
    ok.

testPrim(Config) -> test(Config, fun testPrim/3).
testPrim(Config, Rule, Opts) ->
    Files = ["Prim","Real"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    do_test_prim(Rule, false),
    asn1_test_lib:compile_all(Files, Config, [no_ok_wrapper,Rule|Opts]),
    do_test_prim(Rule, true).

do_test_prim(Rule, NoOkWrapper) ->
    io:format("No ok wrapper: ~p\n", [NoOkWrapper]),
    put(no_ok_wrapper, NoOkWrapper),
    testPrim:bool(Rule),
    testPrim:int(Rule),
    testPrim:enum(Rule),
    testPrim:obj_id(Rule),
    testPrim:rel_oid(Rule),
    testPrim:null(Rule),
    Rule =/= jer andalso testPrim:real(Rule). %% Temporary workaround for JER

testCompactBitString(Config) -> test(Config, fun testCompactBitString/3).
testCompactBitString(Config, Rule, Opts) ->
    Files = ["PrimStrings", "Constraints"],
    asn1_test_lib:compile_all(Files, Config, [Rule, compact_bit_string|Opts]),
    testCompactBitString:compact_bit_string(Rule),
    testCompactBitString:bit_string_unnamed(Rule),
    testCompactBitString:bit_string_unnamed(Rule),
    testCompactBitString:ticket_7734(Rule),
    testCompactBitString:otp_4869(Rule).

testPrimStrings(Config) ->
    test(Config, fun testPrimStrings/3, [ber,{ber,[der]},per,uper,jer]).
testPrimStrings(Config, jer=Rule, Opts) ->
    Files = ["PrimStrings", "BitStr"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testPrimStrings_cases(Rule, Opts),
    testPrimStrings:more_strings(Rule);
testPrimStrings(Config, Rule, Opts) ->
    LegacyOpts = [legacy_erlang_types|Opts],
    Files = ["PrimStrings", "BitStr"],
    asn1_test_lib:compile_all(Files, Config, [Rule|LegacyOpts]),
    testPrimStrings_cases(Rule, LegacyOpts),
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testPrimStrings_cases(Rule, Opts),
    asn1_test_lib:compile_all(Files, Config, [legacy_bit_string,Rule|Opts]),
    testPrimStrings:bit_string(Rule, Opts),
    asn1_test_lib:compile_all(Files, Config, [compact_bit_string,Rule|Opts]),
    testPrimStrings:bit_string(Rule, Opts),
    testPrimStrings:more_strings(Rule).

testPrimStrings_cases(Rule, Opts) ->
    testPrimStrings:bit_string(Rule, Opts),
    testPrimStrings:octet_string(Rule),
    testPrimStrings:numeric_string(Rule),
    testPrimStrings:other_strings(Rule),
    case Rule of
        jer ->
            ok;
        _ ->
            testPrimStrings:universal_string(Rule),
            testPrimStrings:bmp_string(Rule),
            testPrimStrings:times(Rule),
            testPrimStrings:utf8_string(Rule),
            testPrimStrings:fragmented(Rule)
    end.

testExternal(Config) -> test(Config, fun testExternal/3).
testExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External",
			       "ChoExternal",
			       "PrimExternal",
			       "SeqExternal",
			       "SeqOfExternal",
			       "SeqOfTag",
			       "SeqTag",
			       "SetExtension",
			       "SetExternal",
			       "SetOfExternal",
			       "SetOfTag",
			       "SetTag"],
			      Config, [Rule|Opts]),
    testChoExternal:external(Rule),
    testPrimExternal:external(Rule),
    testSeqExternal:main(Rule),
    testSeqOfExternal:main(Rule),
    testSeqOfTag:main(Rule),
    testSeqTag:main(Rule),
    testSetExtension:main(Rule),
    testSetExternal:main(Rule),
    testSetOfExternal:main(Rule),
    testSetOfTag:main(Rule),
    testSetTag:main(Rule).

testExtensibilityImplied(Config) ->
    test(Config, fun testExtensibilityImplied/3).
testExtensibilityImplied(Config, Rule, Opts) ->
    asn1_test_lib:compile("ExtensibilityImplied", Config,
			  [Rule,no_ok_wrapper|Opts]),
    testExtensibilityImplied:main(Rule).

testChoice(Config) -> test(Config, fun testChoice/3).
testChoice(Config, Rule, Opts) ->
    Files = ["ChoPrim",
             "ChoExtension",
             "ChoOptional",
             "ChoOptionalImplicitTag",
             "ChoRecursive",
             "ChoTypeRefCho",
             "ChoTypeRefPrim",
             "ChoTypeRefSeq",
             "ChoTypeRefSet"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testChoPrim:bool(Rule),
    testChoPrim:int(Rule),
    testChoExtension:extension(Rule),
    testChoOptional:run(),
    testChoRecursive:recursive(Rule),
    testChoTypeRefCho:choice(Rule),
    testChoTypeRefPrim:prim(Rule),
    testChoTypeRefSeq:seq(Rule),
    testChoTypeRefSet:set(Rule).

testDefaultOctetString(Config) -> test(Config, fun testDefaultOctetString/3).
testDefaultOctetString(Config, Rule, Opts) ->
    asn1_test_lib:compile("DefaultOctetString", Config, [Rule|Opts]),
    testDefaultOctetString:dos(Rule).

testMultipleLevels(Config) -> test(Config, fun testMultipleLevels/3).
testMultipleLevels(Config, Rule, Opts) ->
    asn1_test_lib:compile("MultipleLevels", Config, [Rule|Opts]),
    testMultipleLevels:main(Rule).

testDEFAULT(Config) ->
    test(Config, fun testDEFAULT/3, [ber,{ber,[der]},per,uper]).
testDEFAULT(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["Def","Default"], Config, [Rule|Opts]),
    testDef:main(Rule),
    testSeqSetDefaultVal:main(Rule, Opts),
    asn1_test_lib:compile_all(["Def","Default"], Config,
			      [legacy_erlang_types,Rule|Opts]),
    testDef:main(Rule),
    testSeqSetDefaultVal:main(Rule, Opts).

testExtensionDefault(Config) ->
    test(Config, fun testExtensionDefault/3).
testExtensionDefault(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["ExtensionDefault"], Config, [Rule|Opts]),
    case lists:member(ber, Opts) andalso lists:member(jer, Opts) of
        true ->
            %% JER back-end disables maps for BER, too.
            ok;
        false ->
            testExtensionDefault:main(Rule)
    end.

testMaps(Config) ->
    RulesAndOptions =
        [{ber,[maps,no_ok_wrapper]},
         {ber,[maps,der,no_ok_wrapper]},
         {per,[maps,no_ok_wrapper]},
         {uper,[maps,no_ok_wrapper]},
         {jer,[maps,no_ok_wrapper]}],
    test(Config, fun testMaps/3, RulesAndOptions).

testMaps(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(['Maps'], Config, [Rule|Opts]),
    testMaps:main(Rule).

testOpt(Config) -> test(Config, fun testOpt/3).
testOpt(Config, Rule, Opts) ->
    asn1_test_lib:compile("Opt", Config, [Rule|Opts]),
    testOpt:main(Rule).

testEnumExt(Config) -> test(Config, fun testEnumExt/3).
testEnumExt(Config, Rule, Opts) ->
    asn1_test_lib:compile("EnumExt", Config, [Rule|Opts]),
    testEnumExt:main(Rule).

%% Test of OTP-2523 ENUMERATED with extensionmark.
testSeqDefault(Config) -> test(Config, fun testSeqDefault/3).
testSeqDefault(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqDefault", Config, [Rule|Opts]),
    testSeqDefault:main(Rule).

testSeqExtension(Config) -> test(Config, fun testSeqExtension/3, [ber,uper]).
testSeqExtension(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External",
			       "SeqExtension",
			       "SeqExtension2"],
			      Config,
                              [Rule|Opts]),
    DataDir = proplists:get_value(data_dir, Config),
    testSeqExtension:main(Rule, DataDir, [Rule|Opts]).

testSeqOptional(Config) -> test(Config, fun testSeqOptional/3).
testSeqOptional(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqOptional", Config, [Rule|Opts]),
    testSeqOptional:main(Rule).

testSeqPrim(Config) -> test(Config, fun testSeqPrim/3).
testSeqPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqPrim", Config, [Rule|Opts]),
    testSeqPrim:main(Rule).

%% Test of OTP-2738 Detect corrupt optional component.
testSeq2738(Config) -> test(Config, fun testSeq2738/3).
testSeq2738(Config, Rule, Opts) ->
    asn1_test_lib:compile("Seq2738", Config, [Rule|Opts]),
    testSeq2738:main(Rule).

testSeqTypeRefCho(Config) -> test(Config, fun testSeqTypeRefCho/3).
testSeqTypeRefCho(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqTypeRefCho", Config, [Rule|Opts]),
    testSeqTypeRefCho:main(Rule).

testSeqTypeRefPrim(Config) -> test(Config, fun testSeqTypeRefPrim/3).
testSeqTypeRefPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqTypeRefPrim", Config, [Rule|Opts]),
    testSeqTypeRefPrim:main(Rule).

testSeqTypeRefSeq(Config) -> test(Config, fun testSeqTypeRefSeq/3).
testSeqTypeRefSeq(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqTypeRefSeq", Config, [Rule|Opts]),
    testSeqTypeRefSeq:main(Rule).

testSeqTypeRefSet(Config) -> test(Config, fun testSeqTypeRefSet/3).
testSeqTypeRefSet(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqTypeRefSet", Config, [Rule|Opts]),
    testSeqTypeRefSet:main(Rule).

testSeqOf(Config) -> test(Config, fun testSeqOf/3).
testSeqOf(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["SeqOf", "SeqOfEnum", "XSeqOf"], Config,
                              [Rule|Opts]),
    testSeqOf:main(Rule).

testSeqOfCho(Config) -> test(Config, fun testSeqOfCho/3).
testSeqOfCho(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqOfCho", Config, [Rule|Opts]),
    testSeqOfCho:main(Rule).

testSeqOfChoExt(Config) -> test(Config, fun testSeqOfChoExt/3).
testSeqOfChoExt(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqOfChoExt", Config, [Rule|Opts]),
    testSeqOfChoExt:main(Rule).

testSeqOfIndefinite(Config) ->
    test(Config, fun testSeqOfIndefinite/3, [ber]).
testSeqOfIndefinite(Config, Rule, Opts) ->
    Files = ["Mvrasn-Constants-1", "Mvrasn-DataTypes-1", "Mvrasn-21-4",
             "Mvrasn-20-4", "Mvrasn-19-4", "Mvrasn-18-4", "Mvrasn-17-4",
             "Mvrasn-15-4", "Mvrasn-14-4", "Mvrasn-11-4", "SeqOf"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testSeqOfIndefinite:main().

testSet(Config) -> test(Config, fun testSet/3).
testSet(Config, Rule, Opts) ->
    Files = ["SetDefault",
             "SetOptional",
             "SetPrim",
             "SetTypeRefCho",
             "SetTypeRefPrim",
             "SetTypeRefSeq",
             "SetTypeRefSet"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),

    testSetDefault:main(Rule),
    testSetOptional:ticket_7533(Rule),
    testSetOptional:main(Rule),

    testSetPrim:main(Rule),
    testSetTypeRefCho:main(Rule),
    testSetTypeRefPrim:main(Rule),
    testSetTypeRefSeq:main(Rule),
    testSetTypeRefSet:main(Rule).

testSetOf(Config) -> test(Config, fun testSetOf/3).
testSetOf(Config, Rule, Opts) ->
    Files = ["SetOf",
             "SetOfCho"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testSetOf:main(Rule),
    testSetOfCho:main(Rule).

c_string(Config) ->
    test(Config, fun c_string/3).
c_string(Config, Rule, Opts) ->
    asn1_test_lib:compile("String", Config, [Rule|Opts]),
    asn1ct:test('String').

constraint_equivalence(Config) ->
    constraint_equivalence_abs(Config),
    test(Config, fun constraint_equivalence/3).

constraint_equivalence(Config, Rule, Opts) ->
    M = 'ConstraintEquivalence',
    asn1_test_lib:compile(M, Config, [Rule|Opts]).

constraint_equivalence_abs(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    CaseDir = proplists:get_value(case_dir, Config),
    Asn1Spec = "ConstraintEquivalence",
    Asn1Src = filename:join(DataDir, Asn1Spec),
    ok = asn1ct:compile(Asn1Src, [abs,{outdir,CaseDir}]),
    AbsFile = filename:join(CaseDir, Asn1Spec++".abs"),
    {ok,Terms} = file:consult(AbsFile),
    Cs = [begin
	      Constraints = element(4, Type),
	      Name1 = atom_to_list(Name0),
	      {Name,_} = lists:splitwith(fun(C) -> C =/= $X end, Name1),
	      {Name,Constraints}
	  end || {typedef,_,_,Name0,Type} <- Terms],
    R = sofs:relation(Cs, [{name,constraint}]),
    F0 = sofs:relation_to_family(R),
    F = sofs:to_external(F0),
    Diff = [E || {_,L}=E <- F, length(L) > 1],
    case Diff of
	[] ->
	    ok;
	[_|_] ->
	    io:put_chars("Not equivalent:\n"),
	    [io:format("~s: ~p\n", [N,D]) || {N,D} <- Diff],
	    ct:fail(length(Diff))
    end.

parse(Config) ->
    [asn1_test_lib:compile(M, Config, [abs]) || M <- test_modules()].

per(Config) ->
    test(Config, fun per/3,
         [per,uper,{per,[maps]},{uper,[maps]},{per,[jer]}]).
per(Config, Rule, Opts) ->
    module_test(per_modules(), Config, Rule, Opts).

ber(Config) ->
    test(Config, fun ber/3, [ber,{ber,[maps]}]).

ber(Config, Rule, Opts) ->
    module_test(ber_modules(), Config, Rule, Opts).

der(Config) ->
    asn1_test_lib:compile_all(ber_modules(), Config, [der]).

module_test(Modules, Config, Rule, Opts) ->
    asn1_test_lib:compile_all(Modules, Config, [Rule,?NO_MAPS_MODULE|Opts]),
    _ = [do_module_test(M, Config, Opts) || M <- Modules],
    ok.

do_module_test(M0, Config, Opts) ->
    case list_to_atom(M0) of
	'LDAP' ->
	    %% Because of the recursive definition of 'Filter' in
	    %% the LDAP module, the construction of a sample
	    %% value for 'Filter' is not guaranteed to terminate.
	    ok;
	M ->
	    TestOpts = [{i, proplists:get_value(case_dir, Config)}],
	    case asn1ct:test(M, TestOpts) of
		ok ->
		    ok;
		Error ->
		    erlang:error({test_failed, M, Opts, Error})
	    end
    end.

ber_decode_invalid_length(_Config) ->
    Bin = <<48,129,157,48,0,2,1,2,164,0,48,129,154,49,24,48,22,6,
            3,85,4,10,19,15,69,120,97,109,112,108,101,32,67,111,
            109,112,97,110,121,49,29,48,27,6,9,42,134,72,134,247,
            13,1,9,1,22,14,99,97,64,101,120,97,109,112,108,101,46,
            99,111,109,49,13,48,11,6,3,85,4,7,19,4,79,117,108,117,
            49,26,48,24,6,3,85,4,8,19,17,80,111,104,106,111,105,
            115,45,80,111,104,106,97,110,109,97,97,49,11,48,9,6,3,
            85,4,6,19,2,70,73,49,19,48,17,6,3,85,4,3,19,10,69,120,
            97,109,112,108,101,32,67,65,49,11,48,16,6,3,85,4,11,
            19,9,84,101>>,
    {'EXIT',{error,{asn1,{invalid_value,12}}}} = (catch asn1rt_nif:decode_ber_tlv(Bin)),
    ok.

ber_choiceinseq(Config) ->
    test(Config, fun ber_choiceinseq/3, [ber]).
ber_choiceinseq(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoiceInSeq", Config, [Rule|Opts]).

ber_optional(Config) ->
    test(Config, fun ber_optional/3, [ber]).
ber_optional(Config, Rule, Opts) ->
    asn1_test_lib:compile("SOpttest", Config, [Rule|Opts]),
    V = {'S', {'A', 10, asn1_NOVALUE, asn1_NOVALUE},
              {'B', asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE},
              {'C', asn1_NOVALUE, 111, asn1_NOVALUE}},
    asn1_test_lib:roundtrip('SOpttest', 'S', V).

tagdefault_automatic(Config) ->
    test(Config, fun tagdefault_automatic/3, [ber]).
tagdefault_automatic(Config, Rule, Opts) ->
    asn1_test_lib:compile("TAGDEFAULT-AUTOMATIC", Config, [Rule|Opts]),
    << 48,8,128,2,100,101,129,2,110,111 >> =
	asn1_test_lib:roundtrip_enc('TAGDEFAULT-AUTOMATIC', 'Tagged', {'Tagged', << 100,101 >>, << 110,111 >>}),
    << 48,8,128,2,100,101,129,2,110,111 >> =
	asn1_test_lib:roundtrip_enc('TAGDEFAULT-AUTOMATIC', 'Untagged', {'Untagged', << 100,101 >>, << 110,111 >>}),
    << 48,8,4,2,100,101,130,2,110,111 >> =
	asn1_test_lib:roundtrip_enc('TAGDEFAULT-AUTOMATIC', 'Mixed', {'Mixed', << 100,101 >>, << 110,111 >>}),

    ok.

%% records used by test-case default
-record('Def1', {bool0,
                 bool1 = asn1_DEFAULT,
                 bool2 = asn1_DEFAULT,
                 bool3 = asn1_DEFAULT}).

default(Config) -> test(Config, fun default/3).
default(Config, Rule, Opts) ->
    asn1_test_lib:compile("Def", Config, [Rule|Opts]),
    asn1_test_lib:roundtrip('Def',
			    'Def1',
			    #'Def1'{bool0=true},
			    #'Def1'{bool0=true,bool1=false,
				    bool2=false,bool3=false}),
    asn1_test_lib:roundtrip('Def',
			    'Def1',
			    #'Def1'{bool0=true,bool2=false},
			    #'Def1'{bool0=true,bool1=false,
				    bool2=false,bool3=false}).

value_test(Config) -> test(Config, fun value_test/3).
value_test(Config, Rule, Opts) ->
    asn1_test_lib:compile("ObjIdValues", Config, [Rule|Opts]),
    {ok, _} = asn1ct:test('ObjIdValues', 'ObjIdType',
                          'ObjIdValues':'mobileDomainId'()).

constructed(Config) ->
    test(Config, fun constructed/3, [ber]).
constructed(Config, Rule, Opts) ->
    asn1_test_lib:compile("Constructed", Config, [Rule|Opts]),
    <<40,3,1,1,0>> =
	asn1_test_lib:roundtrip_enc('Constructed', 'S', {'S',false}),
    <<40,5,48,3,1,1,0>> =
	asn1_test_lib:roundtrip_enc('Constructed', 'S2', {'S2',false}),
    <<136,1,10>> =
	asn1_test_lib:roundtrip_enc('Constructed', 'I', 10),
    ok.

ber_decode_error(Config) ->
    test(Config, fun ber_decode_error/3, [ber]).
ber_decode_error(Config, Rule, Opts) ->
    asn1_test_lib:compile("Constructed", Config, [Rule|Opts]),
    ber_decode_error:run(Opts).

otp_14440(_Config) ->
    {ok, Peer, N} = ?CT_PEER(),
    Result = rpc:call(N, ?MODULE, otp_14440_decode, []),
    io:format("Decode result = ~p~n", [Result]),
    peer:stop(Peer),
    case Result of
        {exit,{error,{asn1,{invalid_value,5}}}} ->
            ok;
        %% We get this if stack depth limit kicks in:
        {exit,{error,{asn1,{unknown,_}}}} ->
            ok;
        _ ->
            ct:fail(Result)
    end.
%%
otp_14440_decode() ->
    Data =
        iolist_to_binary(
          lists:duplicate(
            32, list_to_binary(lists:duplicate(1024, 16#7f)))),
    try asn1rt_nif:decode_ber_tlv(Data) of
        Result ->
            {unexpected_return,Result}
    catch
        Class:Reason ->
            {Class,Reason}
    end.


h323test(Config) -> test(Config, fun h323test/3).
h323test(Config, Rule, Opts) ->
    Files = ["H235-SECURITY-MESSAGES", "H323-MESSAGES",
             "MULTIMEDIA-SYSTEM-CONTROL"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    h323test:run(Rule).

per_open_type(Config) -> test(Config, fun per_open_type/3, [per]).
per_open_type(Config, Rule, Opts) ->
    asn1_test_lib:compile("OpenType", Config, [Rule|Opts]),
    {ok, _} = asn1ct:test('OpenType', 'Ot', {'Stype', 10, true}).

testConstraints(Config) -> test(Config, fun testConstraints/3).
testConstraints(Config, Rule, Opts) ->
    Files = ["Constraints", "LargeConstraints"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testConstraints:int_constraints(Rule),
    case Rule of
	ber -> ok;
        jer -> ok; % subtype constraint is not checked
	_ -> testConstraints:refed_NNL_name(Rule)
    end.

testSeqSetIndefinite(Config) ->
    test(Config, fun testSeqSetIndefinite/3, [ber]).
testSeqSetIndefinite(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqSetIndefinite", Config, [Rule|Opts]),
    testSeqSetIndefinite:main().

testChoiceIndefinite(Config) ->
    test(Config, fun testChoiceIndefinite/3, [ber]).

testChoiceIndefinite(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoiceIndef", Config, [Rule|Opts]),
    testChoiceIndefinite:main(Rule).

testInfObjectClass(Config) -> test(Config, fun testInfObjectClass/3).
testInfObjectClass(Config, Rule, Opts) ->
    Files       = ["ErrorClass", "InfClass"],
    InfObjFiles = ["RANAPextract1", "InfObj", "MAP-ExtensionDataTypes",
                   "Objects", "INAPv2extract"],
    RANAPFiles  = ["RANAP-CommonDataTypes", "RANAP-Constants",
                   "RANAP-Containers", "RANAP-IEs", "RANAP-PDU-Contents",
                   "RANAP-PDU-Descriptions"],
    asn1_test_lib:compile_all(Files ++ InfObjFiles ++ RANAPFiles, Config,
                              [Rule|Opts]),
    testInfObjectClass:main(Rule),
    testInfObj:main(Rule).

testUniqueObjectSets(Config) -> test(Config, fun testUniqueObjectSets/3).
testUniqueObjectSets(Config, Rule, Opts) ->
    CaseDir = proplists:get_value(case_dir, Config),
    testUniqueObjectSets:main(CaseDir, Rule, Opts).

testInfObjExtract(Config) -> test(Config, fun testInfObjExtract/3).
testInfObjExtract(Config, Rule, Opts) ->
    asn1_test_lib:compile("InfObjExtract", Config, [Rule|Opts]),
    testInfObjExtract:main(Rule).

testParam(Config) ->
    test(Config, fun testParam/3, [ber,{ber,[der]},per,uper]).
testParam(Config, Rule, Opts) ->
    Files = ["ParamBasic","Param","Param2"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testParamBasic:main(Rule),
    testParameterizedInfObj:main(Config, Rule),
    asn1_test_lib:compile("Param", Config,
			  [legacy_erlang_types,Rule|Opts]),
    testParameterizedInfObj:param(Rule).

testFragmented(Config) ->
    test(Config, fun testFragmented/3).
testFragmented(Config, Rule, Opts) ->
    asn1_test_lib:compile("Fragmented", Config, [Rule|Opts]),
    testFragmented:main(Rule).

testMergeCompile(Config) -> test(Config, fun testMergeCompile/3).
testMergeCompile(Config, Rule, Opts) ->
    Files = ["MS.set.asn", "RANAPSET.set.asn1", "Mvrasn4.set.asn",
             "Mvrasn6.set.asn"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testMergeCompile:main(Rule),
    testMergeCompile:mvrasn(Rule).

testobj(Config) -> test(Config, fun testobj/3).
testobj(_Config, jer, _Opts) -> ok;
testobj(Config, Rule, Opts) ->
    asn1_test_lib:compile("RANAP", Config, [legacy_erlang_types,
					    Rule|Opts]),
    asn1_test_lib:compile_erlang("testobj", Config, []),
    ok = testobj:run(),
    ok = testParameterizedInfObj:ranap(Rule).

testDeepTConstr(Config) -> test(Config, fun testDeepTConstr/3).
testDeepTConstr(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["TConstrChoice", "TConstr"], Config,
                              [Rule|Opts]),
    testDeepTConstr:main(Rule).

testImport(Config) ->
    test(Config, fun testImport/3).
testImport(Config, Rule, Opts) ->
    Files = ["ImportsFrom","ImportsFrom2","ImportsFrom3",
	     "Importing","Exporting"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    42 = 'ImportsFrom':i(),
    testImporting:main(Rule),
    ok.

testMegaco(Config) -> test(Config, fun testMegaco/3).
testMegaco(Config, Rule, Opts) ->
    {ok, Module1, Module2} = testMegaco:compile(Config, Rule,
						[legacy_erlang_types|Opts]),
    ok = testMegaco:main(Module1, Config),
    ok = testMegaco:main(Module2, Config).

testMvrasn6(Config) -> test(Config, fun testMvrasn6/3).
testMvrasn6(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["Mvrasn-21-4", "Mvrasn-20-6", "Mvrasn-19-6",
                               "Mvrasn-15-6", "Mvrasn-18-6", "Mvrasn-14-6",
                               "Mvrasn-11-6"], Config, [Rule|Opts]).

testContextSwitchingTypes(Config) ->
    test(Config, fun testContextSwitchingTypes/3).
testContextSwitchingTypes(Config, Rule, Opts) ->
    asn1_test_lib:compile("ContextSwitchingTypes", Config, [Rule|Opts]),
    testContextSwitchingTypes:test(Rule,Config).

testTypeValueNotation(Config) -> test(Config, fun testTypeValueNotation/3).
testTypeValueNotation(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqTypeRefPrim", Config, [Rule|Opts]),
    testTypeValueNotation:main(Rule, Opts).

testValueTest(Config) -> test(Config, fun testValueTest/3).
testValueTest(Config, Rule, Opts) ->
    asn1_test_lib:compile("ValueTest", Config, [Rule|Opts]),
    testValueTest:main().

testOpenTypeImplicitTag(Config) ->
    test(Config, fun testOpenTypeImplicitTag/3).
testOpenTypeImplicitTag(Config, Rule, Opts) ->
    asn1_test_lib:compile("OpenTypeImplicitTag", Config, [Rule|Opts]),
    testOpenTypeImplicitTag:main(Rule).

rtUI(Config) -> test(Config, fun rtUI/3).
rtUI(Config, Rule, Opts) ->
    asn1_test_lib:compile("Prim", Config, [Rule|Opts]),
    _ = 'Prim':info(),
    Rule = 'Prim':encoding_rule(),
    io:format("Default BIT STRING format: ~p\n",
	      ['Prim':bit_string_format()]).

testROSE(Config) -> test(Config, fun testROSE/3).
testROSE(Config, Rule, Opts) ->
    asn1_test_lib:compile("Remote-Operations-Merged.set.asn1", Config,
                          [Rule|Opts]).

testINSTANCE_OF(Config) -> test(Config, fun testINSTANCE_OF/3).
testINSTANCE_OF(Config, Rule, Opts) ->
    asn1_test_lib:compile("INSTANCEOF.asn1", Config, [Rule|Opts]),
    testINSTANCE_OF:main(Rule).

testTCAP(Config) ->
    test(Config, fun testTCAP/3).
testTCAP(Config, Rule, Opts) ->
    testTCAP:compile(Config, [Rule|Opts]),
    testTCAP:test(Rule, Config),
    case Rule of
        ber ->
	    testTCAP:compile_asn1config(Config, [Rule, asn1config]),
	    testTCAP:test_asn1config();
        _ -> ok
    end.

testDER(Config) ->
    test(Config, fun testDER/3, [ber]).
testDER(Config, Rule, Opts) ->
    asn1_test_lib:compile("DERSpec", Config, [Rule, der|Opts]),
    testDER:test().

specialized_decodes(Config) ->
    test(Config, fun specialized_decodes/3, [ber]).
specialized_decodes(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["PartialDecSeq.asn",
                               "PartialDecSeq2.asn",
                               "PartialDecSeq3.asn",
                               "PartialDecMyHTTP.asn",
                               "P-Record",
                               "PartialDecChoExtension.asn",
                               "OCSP-2013-88.asn1",
                               "PKIX1Explicit88.asn1"],
                              Config,
			      [Rule,asn1config|Opts]),
    asn1_test_lib:compile("MEDIA-GATEWAY-CONTROL.asn",
                          Config,
                          [Rule,legacy_erlang_types,asn1config|Opts]),
    test_partial_incomplete_decode:test(Config),
    test_selective_decode:test().

special_decode_performance(Config) ->
    test(Config, fun special_decode_performance/3, [ber]).
special_decode_performance(Config, Rule, Opts) ->
    Files = ["MEDIA-GATEWAY-CONTROL", "PartialDecSeq"],
    asn1_test_lib:compile_all(Files, Config, [Rule, asn1config|Opts]),
    test_special_decode_performance:go(all).

exclusive_decode_rest(Config) ->
    test(Config, fun exclusive_decode_rest/3, [ber]).
exclusive_decode_rest(Config, Rule, Opts) ->
    asn1_test_lib:compile("SwCDR.py", Config,
                          [Rule, undec_rest, asn1config|Opts]),
    test_exclusive_decode_rest:test().

test_ParamTypeInfObj(Config) ->
    asn1_test_lib:compile("IN-CS-1-Datatypes", Config, [ber]).

test_WS_ParamClass(Config) -> test(Config, fun test_WS_ParamClass/3).
test_WS_ParamClass(Config, Rule, Opts) ->
    asn1_test_lib:compile("InformationFramework", Config, [Rule|Opts]),
    ?only_ber(testWSParamClass:main(Rule)),
    ok.

test_Defed_ObjectIdentifier(Config) ->
    test(Config, fun test_Defed_ObjectIdentifier/3).
test_Defed_ObjectIdentifier(Config, Rule, Opts) ->
    asn1_test_lib:compile("UsefulDefinitions", Config, [Rule|Opts]).

testSelectionType(Config) -> test(Config, fun testSelectionType/3).
testSelectionType(Config, Rule, Opts) ->
    asn1_test_lib:compile("SelectionType", Config, [Rule|Opts]),
    testSelectionTypes:test().

testSSLspecs(Config) ->
    test(Config, fun testSSLspecs/3, [ber]).
testSSLspecs(Config, Rule, Opts) ->
    ok = testSSLspecs:compile(Config,
                              [Rule, compact_bit_string, der|Opts]),
    testSSLspecs:run(Rule),
    ok = testSSLspecs:compile_combined(Config, Rule),
    ok = testSSLspecs:run_combined(Rule).

testNortel(Config) -> test(Config, fun testNortel/3).
testNortel(Config, Rule, Opts) ->
    asn1_test_lib:compile("Nortel", Config, [Rule|Opts]).

test_undecoded_rest(Config) ->
    test(Config, fun test_undecoded_rest/3, [per, uper, ber]).
test_undecoded_rest(Config, Rule, Opts) ->
    do_test_undecoded_rest(Config, Rule, Opts),
    do_test_undecoded_rest(Config, Rule, [no_ok_wrapper|Opts]),
    do_test_undecoded_rest(Config, Rule, [undec_rest|Opts]),
    do_test_undecoded_rest(Config, Rule, [no_ok_wrapper,undec_rest|Opts]).

do_test_undecoded_rest(Config, Rule, Opts) ->
    asn1_test_lib:compile("P-Record", Config, [Rule|Opts]),
    test_undecoded_rest:test(Opts, Config).

testTcapsystem(Config) ->
    test(Config, fun testTcapsystem/3).
testTcapsystem(Config, Rule, Opts) ->
    testTcapsystem:compile(Config, [Rule|Opts]).

testNBAPsystem(Config) -> test(Config, fun testNBAPsystem/3, [per]).
testNBAPsystem(Config, Rule, Opts) ->
    testNBAPsystem:compile(Config, [Rule|Opts]),
    testNBAPsystem:test(Rule, Config).

testS1AP(Config) -> test(Config, fun testS1AP/3).
testS1AP(Config, Rule, Opts) ->
    S1AP = ["S1AP-CommonDataTypes",
	    "S1AP-Constants",
	    "S1AP-Containers",
	    "S1AP-IEs",
	    "S1AP-PDU-Contents",
	    "S1AP-PDU-Descriptions"],
    asn1_test_lib:compile_all(S1AP, Config, [Rule|Opts]),

    %% OTP-7876.
    case Rule of
	per ->
	    Enc = <<0,2,64,49,0,0,5,0,0,0,4,128,106,56,197,0,8,0,3,64,2,134,0,
		    100,64,8,0,66,240,153,0,7,192,16,0,67,64,6,0,66,240,153,70,
		    1,0,107,64,5,0,0,0,0,0>>,
	    {ok,{initiatingMessage,_}} = 'S1AP-PDU-Descriptions':decode('S1AP-PDU', Enc);
	uper ->
	    ok;
	ber ->
	    ok;
        jer ->
            ok
    end.

testRfcs() ->
    [{timetrap,{minutes,90}}].

testRfcs(Config) ->  test(Config, fun testRfcs/3,
                          [{ber,[der,?NO_MAPS_MODULE]},
                           {ber,[der,maps]}]).
testRfcs(Config, Rule, Opts) ->
    case erlang:system_info(system_architecture) of
	"sparc-sun-solaris2.10" ->
	    {skip,"Too slow for an old Sparc"};
	_ ->
	    testRfcs:compile(Config, Rule, Opts),
	    testRfcs:test()
    end.

test_compile_options(Config) ->
    ok = test_compile_options:wrong_path(Config),
    ok = test_compile_options:path(Config),
    ok = test_compile_options:noobj(Config),
    ok = test_compile_options:record_name_prefix(Config),
    ok = test_compile_options:verbose(Config),
    ok = test_compile_options:maps(Config),
    ok = test_compile_options:determinism(Config).

testDoubleEllipses(Config) -> test(Config, fun testDoubleEllipses/3).
testDoubleEllipses(Config, Rule, Opts) ->
    asn1_test_lib:compile("DoubleEllipses", Config, [Rule|Opts]),
    testDoubleEllipses:main(Rule).

test_modified_x420(Config) ->
    test(Config, fun test_modified_x420/3, [ber]).
test_modified_x420(Config, Rule, Opts) ->
    Files = [filename:join(modified_x420, F) || F <- ["PKCS7",
                                                      "InformationFramework",
                                                      "AuthenticationFramework"]],
    asn1_test_lib:compile_all(Files, Config, [Rule,der|Opts]),
    test_modified_x420:test(Config).


test_x691(Config) ->
    test(Config, fun test_x691/3, [per, uper]).
test_x691(Config, Rule, Opts) ->
    Files = ["P-RecordA1", "P-RecordA2", "P-RecordA3"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    test_x691:cases(Rule),

    %% OTP-7708.
    asn1_test_lib:compile("EUTRA-extract-55", Config,
			  [legacy_erlang_types,Rule|Opts]),

    %% OTP-7763.
    Val = {'Seq',15,lists:duplicate(8, 0),[0],lists:duplicate(28, 0),15,true},
    CompactVal = {'Seq',15,{0,<<0>>},{7,<<0>>},{4,<<0,0,0,0>>},15,true},
    {ok,Bin} = 'EUTRA-extract-55':encode('Seq', Val),
    {ok,Bin} = 'EUTRA-extract-55':encode('Seq', CompactVal),

    %% OTP-7678.
    asn1_test_lib:compile("UPERDefault", Config, [Rule|Opts]),
    DefVal = 'UPERDefault':seq(),
    {ok,DefBin} = 'UPERDefault':encode('Seq', DefVal),
    {ok,DefVal} = 'UPERDefault':decode('Seq', DefBin),
    case Rule of
	uper -> <<0,6,0>> = DefBin;
	_ -> ok
    end,

    ok.

ticket_6143(Config) ->
    asn1_test_lib:compile("AA1", Config, [?NO_MAPS_MODULE]).

testExtensionAdditionGroup(Config) ->
    test(Config, fun testExtensionAdditionGroup/3).
testExtensionAdditionGroup(Config, Rule, Opts) ->
    asn1_test_lib:compile("Extension-Addition-Group", Config, [Rule|Opts]),
    asn1_test_lib:compile_erlang("extensionAdditionGroup", Config,
                                 [debug_info]),
    asn1_test_lib:compile("EUTRA-RRC-Definitions", Config,
			  [Rule,{record_name_prefix,"RRC-"}|Opts]),
    extensionAdditionGroup:run(Rule).

testContaining(Config) ->
    test(Config, fun testContaining/3).
testContaining(Config, Rule, Opts) ->
    asn1_test_lib:compile("Containing", Config, [Rule|Opts]),
    testContaining:containing(Rule),
    case Rule of
        per ->
            io:format("Testing with both per and jer...\n"),
            asn1_test_lib:compile("Containing", Config, [jer,Rule|Opts]),
            testContaining:containing(per_jer);
        _ ->
            ok
    end.

per_modules() ->
    [X || X <- test_modules()].

ber_modules() ->
    [X || X <- test_modules(),
          X =/= "H323-MESSAGES",
          X =/= "H235-SECURITY-MESSAGES"].

test_modules() ->
    ["BitStr",
     "CAP",
     "CommonDataTypes",
     "Constraints",
     "ContextSwitchingTypes",
     "CoverParser",
     "DS-EquipmentUser-CommonFunctionOrig-TransmissionPath",
     "Enum",
     "From",
     "H235-SECURITY-MESSAGES",
     "H323-MESSAGES",
     "HighTagNumbers",
     "Import",
     "Int",
     "MAP-commonDataTypes",
     "Null",
     "NullTest",
     "Octetstr",
     "One",
     "P-Record",
     "P",
     "Person",
     "PrimStrings",
     "Real",
     "XSeq",
     "XSeqOf",
     "XSet",
     "XSetOf",
     "String",
     "SwCDR",
     "Time",
     "SeqSetLib", % must be compiled before Seq and Set
     "Seq",
     "Set",
     "SetOf",
     "SeqOf",
     "Prim",
     "Cho",
     "Def",
     "Opt",
     "ELDAPv3",
     "LDAP",
     "SeqOptional2",
     "CCSNARG3"].

test_OTP_9688(Config) ->
    PrivDir = proplists:get_value(case_dir, Config),
    Data = "
OTP-9688 DEFINITIONS ::= BEGIN

 foo INTEGER ::= 1
 bar INTEGER ::= 42

 Baz ::= INTEGER {x-y-z1(foo), x-y-z2(bar)}
 Qux ::= SEQUENCE {flerpInfo SEQUENCE {x INTEGER (-10 | -9 | (0..4))} OPTIONAL}

END
",
    File = filename:join(PrivDir, "OTP-9688.asn1"),
    ok = file:write_file(File, Data),
    %% Does it compile with changes to asn1ct_check and asn1ct_gen_per_rt2ct?
    %% (see ticket)
    ok = asn1ct:compile(File, [{outdir, PrivDir}]).


timer_compile(Config, Opts0) ->
    Files = ["H235-SECURITY-MESSAGES", "H323-MESSAGES"],
    Opts = [no_ok_wrapper,?NO_MAPS_MODULE|Opts0],
    asn1_test_lib:compile_all(Files, Config, Opts).

testTimer_ber(Config) ->
    timer_compile(Config, [ber]),
    testTimer:go().

testTimer_per(Config) ->
    timer_compile(Config, [per]),
    testTimer:go().

testTimer_uper(Config) ->
    timer_compile(Config, [uper]),
    testTimer:go().

testTimer_ber_maps(Config) ->
    timer_compile(Config, [ber,maps]),
    testTimer:go().

testTimer_per_maps(Config) ->
    timer_compile(Config, [per,maps]),
    testTimer:go().

testTimer_uper_maps(Config) ->
    timer_compile(Config, [uper,maps]),
    testTimer:go().

%% Test of multiple-line comment, OTP-8043
testComment(Config) ->
    asn1_test_lib:compile("Comment", Config, []),
    asn1_test_lib:roundtrip('Comment', 'Seq', {'Seq',12,true}).

testName2Number(Config) ->
    N2NOptions0 = [{n2n,Type} ||
                     Type <- ['Cause-Misc', 'CauseProtocol']],
    N2NOptions = [?NO_MAPS_MODULE|N2NOptions0],
    asn1_test_lib:compile("EnumN2N", Config, N2NOptions),

    0 = 'EnumN2N':'name2num_Cause-Misc'('control-processing-overload'),
    'unknown-PLMN' = 'EnumN2N':'num2name_Cause-Misc'(5),
    4 = 'EnumN2N':name2num_CauseProtocol('semantic-error'),
    'transfer-syntax-error' = 'EnumN2N':num2name_CauseProtocol(0),

    %% OTP-10144
    %% Test that n2n option generates name2num and num2name functions supporting
    %% values not within the extension root if the enumeration type has an
    %% extension marker.
    N2NOptionsExt = [?NO_MAPS_MODULE,{n2n,'NoExt'},{n2n,'Ext'},{n2n,'Ext2'}],
    asn1_test_lib:compile("EnumN2N", Config, N2NOptionsExt),

    %% Previously, name2num and num2name was not generated if the type didn't
    %% have an extension marker:
    0 = 'EnumN2N':name2num_NoExt('blue'),
    2 = 'EnumN2N':name2num_NoExt('green'),
    blue = 'EnumN2N':num2name_NoExt(0),
    green = 'EnumN2N':num2name_NoExt(2),

    %% Test enumeration extension:
    7 = 'EnumN2N':name2num_Ext2('orange'),
    orange = 'EnumN2N':num2name_Ext2(7),
    %% 7 is not defined in Ext, only in Ext2.
    {asn1_enum, 7} = 'EnumN2N':num2name_Ext(7),
    7 = 'EnumN2N':name2num_Ext({asn1_enum, 7}),
    42 = 'EnumN2N':name2num_Ext2({asn1_enum, 42}),
    ok.

ticket_7407(Config) ->
    Opts = [uper,?NO_MAPS_MODULE],
    asn1_test_lib:compile("EUTRA-extract-7407", Config, Opts),
    ticket_7407_code(true),
    asn1_test_lib:compile("EUTRA-extract-7407", Config,
                          [no_final_padding|Opts]),
    ticket_7407_code(false).

ticket_7407_code(FinalPadding) ->
    Msg1 = {Type1,_} = eutra1(msg),
    {ok,B1} = 'EUTRA-extract-7407':encode(Type1, Msg1),
    B1 = eutra1(result, FinalPadding),

    Msg2 = {Type2,_} = eutra2(msg),
    {ok,B2} = 'EUTRA-extract-7407':encode(Type2, Msg2),
    B2 = eutra2(result, FinalPadding),
    ok.

eutra1(msg) ->
    {'BCCH-BCH-Message',
     {'MasterInformationBlock',<<2#0101:4>>,<<2#1010:4>>,
      {'PHICH-Configuration',short,ffs},<<2#10100000>>}}.

eutra1(result, true) ->
    <<90,80,0>>;
eutra1(result, false) ->
    <<90,80,0:1>>.

eutra2(msg) ->
    {'BCCH-DL-SCH-Message',
     {c1,
      {systemInformation1,
       {'SystemInformationBlockType1',
	{'SystemInformationBlockType1_cellAccessRelatedInformation',
	 [{'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',
	   {'PLMN-Identity'},true},
	  {'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',
	   {'PLMN-Identity'},false},
	  {'SystemInformationBlockType1_cellAccessRelatedInformation_plmn-IdentityList_SEQOF',
	   {'PLMN-Identity'},true}],
	 {'TrackingAreaCode'},
	 {'CellIdentity'},
	 false,
	 true,
	 true,
	 true
	},
	{'SystemInformationBlockType1_cellSelectionInfo',-50},
	24,
	[{'SystemInformationBlockType1_schedulinInformation_SEQOF',
	  {'SystemInformationBlockType1_schedulinInformation_SEQOF_si-MessageType'},
	  ms320,
	  {'SystemInformationBlockType1_schedulinInformation_SEQOF_sib-MappingInfo'}}],
	0
       }
      }
     }
    }.

eutra2(result, true) ->
%% 55 5C A5 E0
    <<85,92,165,224>>;
eutra2(result, false) ->
    <<85,92,165,14:4>>.

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{first,second}).

ticket7904(Config) ->
    asn1_test_lib:compile("RANAPextract1", Config, [per]),

    Val1 = #'InitiatingMessage'{procedureCode=1,
                                criticality=ignore,
                                value=#'Iu-ReleaseCommand'{
                                  first=13,
                                  second=true}},

    {ok,_} = 'RANAPextract1':encode('InitiatingMessage', Val1),
    {ok,_} = 'RANAPextract1':encode('InitiatingMessage', Val1).


%% Make sure that functions exported from other modules are
%% actually used.

xref(_Config) ->
    S = ?FUNCTION_NAME,
    xref:start(S),
    xref:set_default(S, [{verbose,false},{warnings,false},{builtins,true}]),
    Test = filename:dirname(code:which(?MODULE)),
    {ok,_PMs} = xref:add_directory(S, Test),
    Q = "X - XU - \".*_SUITE\" : Mod",
    UnusedExports = xref:q(S, Q),
    xref:stop(S),
    case UnusedExports of
	{ok,[]} ->
	    ok;
	{ok,[_|_]=Res} ->
	    ct:fail("Exported, but unused: ~p\n", [Res])
    end.

%% Ensure that all functions that are implicitly exported by
%% 'export_all' in this module are actually used.

xref_export_all(_Config) ->
    S = ?FUNCTION_NAME,
    xref:start(S),
    xref:set_default(S, [{verbose,false},{warnings,false},{builtins,true}]),
    {ok,_PMs} = xref:add_module(S, code:which(?MODULE)),
    AllCalled = all_called(),
    Def = "Called := " ++ lists:flatten(io_lib:format("~p", [AllCalled])),
    {ok,_} = xref:q(S, Def),
    {ok,Unused} = xref:q(S, "X - Called - range (closure E | Called)"),
    xref:stop(S),
    case Unused -- [{?MODULE,otp_14440_decode,0}] of
        [] ->
            ok;
        [_|_] ->
            Msg = [io_lib:format("~p:~p/~p\n", [M,F,A]) || {M,F,A} <- Unused],
            ct:fail("There are unused functions:\n\n~s\n", [Msg])
    end.

%% Collect all functions that common_test will call in this module.

all_called() ->
    [{?MODULE,end_per_group,2},
     {?MODULE,end_per_suite,1},
     {?MODULE,end_per_testcase,2},
     {?MODULE,init_per_group,2},
     {?MODULE,init_per_suite,1},
     {?MODULE,init_per_testcase,2},
     {?MODULE,suite,0}] ++
        all_called_1(all() ++ groups()).

all_called_1([{_,_}|T]) ->
    all_called_1(T);
all_called_1([{_Name,_Flags,Fs}|T]) ->
    all_called_1(Fs ++ T);
all_called_1([F|T]) when is_atom(F) ->
    L = case erlang:function_exported(?MODULE, F, 0) of
            false ->
                [{?MODULE,F,1}];
            true ->
                [{?MODULE,F,0},{?MODULE,F,1}]
        end,
    L ++ all_called_1(T);
all_called_1([]) ->
    [].
