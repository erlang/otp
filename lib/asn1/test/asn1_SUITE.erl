%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose: Test suite for the ASN.1 application

-module(asn1_SUITE).

-define(only_per(Func),
    if  Rule == per orelse Rule == per_bin -> Func;
        true                               -> ok
    end).
-define(only_ber(Func),
    if  Rule == ber orelse Rule == ber_bin orelse Rule == ber_bin_v2 -> Func;
        true                                                         -> ok
    end).
-define(only_uper(Func),
    case Rule of
        uper_bin -> Func;
        _        -> ok
    end).
-define(only_per_nif(Func),
    case {Rule, lists:member(optimize, Opts)} of
        {per_bin, true} -> Func;
        _               -> ok
    end).
-define(only_ber_nif(Func),
    case {Rule, lists:member(nif, Opts)} of
        {ber_bin_v2, true} -> Func;
        _               -> ok
    end).

-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

%%------------------------------------------------------------------------------
%% Suite definition
%%------------------------------------------------------------------------------

suite() -> [{ct_hooks, [ts_install_cth]},
	    {timetrap,{minutes,60}}].

all() ->
    [{group, parallel},
     {group, app_test},
     {group, appup_test},

     % TODO: Investigate parallel running of these:
     testComment,
     testName2Number,
     ticket_7407,
     ticket7904,

     {group, performance}].

groups() ->
    [{compile, parallel([]),
      [c_syntax,
       c_string,
       c_implicit_before_choice]},

     {ber, parallel([]),
      [ber_choiceinseq,
       % Uses 'SOpttest'
       {group, [], [ber_optional,
                    ber_optional_keyed_list]}]},

     {app_test, [], [{asn1_app_test, all}]},

     {appup_test, [], [{asn1_appup_test, all}]},

     {parallel, parallel([]),
      [{group, compile},
       {group, ber},
       % Uses 'P-Record', 'Constraints', 'MEDIA-GATEWAY-CONTROL'...
       {group, [], [parse,
                    test_driver_load,
                    test_undecoded_rest,
                    test_inline,
                    specialized_decodes,
                    special_decode_performance,
                    testMegaco,
                    testConstraints,
                    testCompactBitString]},
       default,
       % Uses 'Def', 'MULTIMEDIA-SYSTEM-CONTROL', 'H323-MESSAGES', 'Prim',
       %   'Real'
       {group, [], [testPrim,
                    rtUI,
                    testPrimStrings,
                    testInvokeMod,
                    per,
                    ber_other,
                    h323test,
                    per_GeneralString]},
       testChoPrim,
       testChoExtension,
       testChoOptional,
       testChoOptionalImplicitTag,
       testChoRecursive,
       testChoTypeRefCho,
       testChoTypeRefPrim,
       testChoTypeRefSeq,
       testChoTypeRefSet,
       testDef,
       testOpt,
       testSeqDefault,
       % Uses 'External'
       {group, [], [testChoExternal,
                    testPrimExternal,
                    testSeqExtension,
                    testSeqExternal,
                    testSeqOfExternal,
                    testSeqOfTag,
                    testSeqTag,
                    testSetExtension,
                    testSetExternal,
                    testSetOfExternal,
                    testSetOfTag,
                    testSetTag]},
       testSeqOptional,
       testSeqPrim,
       testSeqTypeRefCho,
       % Uses 'SeqTypeRefPrim'
       {group, [], [testSeqTypeRefPrim,
                    testTypeValueNotation]},
       testSeqTypeRefSeq,
       testSeqTypeRefSet,
       % Uses 'SeqOf'
       {group, [], [testSeqOf,
                    testSeqOfIndefinite]}, % Uses 'Mvrasn*'
       testSeqOfCho,
       testSetDefault,
       testExtensionAdditionGroup,
       testSetOptional,
       testSetPrim,
       testSetTypeRefCho,
       testSetTypeRefPrim,
       testSetTypeRefSeq,
       testSetTypeRefSet,
       testSetOf,
       testSetOfCho,
       testEnumExt,
       value_test,
       value_bad_enum_test,
       testSeq2738,
       % Uses 'Constructed'
       {group, [], [constructed,
                    ber_decode_error]},
       % Uses 'SeqSetIndefinite'
       {group, [], [testSeqIndefinite,
                    testSetIndefinite]},
       testChoiceIndefinite,
       per_open_type,
       testInfObjectClass,
       testParameterizedInfObj,
       testMergeCompile,
       testobj,
       testDeepTConstr,
       testExport,
       testImport,
       % Uses 'ParamBasic'
       {group, [], [testParamBasic,
                    testDER]},
       testMvrasn6,
       testContextSwitchingTypes,
       testOpenTypeImplicitTag,
       duplicate_tags,
       testROSE,
       testINSTANCE_OF,
       testTCAP,
       test_ParamTypeInfObj,
       test_WS_ParamClass,
       test_Defed_ObjectIdentifier,
       testSelectionType,
       testSSLspecs,
       testNortel,
       % Uses 'PKCS7'
       {group, [], [test_modified_x420,
                    testX420]},
       testTcapsystem,
       testNBAPsystem,
       test_compile_options,
       testDoubleEllipses,
       test_x691,
       ticket_6143,
       testExtensionAdditionGroup,
       test_OTP_9688]},

     {performance, [],
      [testTimer_ber,
       testTimer_ber_bin,
       testTimer_ber_bin_opt,
       testTimer_ber_bin_opt_driver,
       testTimer_per,
       testTimer_per_bin,
       testTimer_per_bin_opt,
       testTimer_uper_bin,
       smp]}].

parallel(Options) ->
    case erlang:system_info(smp_support) andalso
         erlang:system_info(schedulers) > 1 of
        true  -> [parallel|Options];
        false -> Options
    end.

%%------------------------------------------------------------------------------
%% Init/end
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    true = code:add_patha(PrivDir),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config) ->
    CaseDir = filename:join([?config(priv_dir, Config), ?MODULE, Func]),
    ok = filelib:ensure_dir(filename:join([CaseDir, dummy_file])),
    true = code:add_patha(CaseDir),

    [{case_dir, CaseDir}|Config].

end_per_testcase(_Func, Config) ->
    code:del_path(?config(case_dir, Config)).

%%------------------------------------------------------------------------------
%% Test runners
%%------------------------------------------------------------------------------

test(Config, TestF) ->
    test(Config, TestF, [per,
                         per_bin,
                         {per_bin, [optimize]},
                         uper_bin,
                         ber,
                         ber_bin,
                         ber_bin_v2,
                         % TODO: {ber_bin_v2, [optimize, nif]} ?
                         {ber_bin_v2, [nif]}]).

test(Config, TestF, Rules) ->
    Fun = fun(C, R, O) ->
                  M = element(2, erlang:fun_info(TestF, module)),
                  F = element(2, erlang:fun_info(TestF, name)),
                  io:format("Running ~p:~p with ~p...~n", [M, F, {R, O}]),
                  try
                      TestF(C, R, O)
                  catch
                      Class:Reason ->
                          NewReason = {Reason, [{rule, R}, {options, O}]},
                          erlang:raise(Class, NewReason,
                                       erlang:get_stacktrace())
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
    CaseDir = ?config(case_dir, Config),
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
    string:join([atom_to_list(Rule)|lists:map(fun atom_to_list/1, Opts)], "_").

case_dir([], _Dir) ->
    exit(no_case_dir);
case_dir([{case_dir, _}|Config], Dir) ->
    [{case_dir, Dir}|Config];
case_dir([C|Config], Opt) ->
    [C|case_dir(Config, Opt)].

%%------------------------------------------------------------------------------
%% Test cases
%%------------------------------------------------------------------------------

testPrim(Config) -> test(Config, fun testPrim/3).
testPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["Prim", "Real"], Config, [Rule|Opts]),
    testPrim:bool(Rule),
    testPrim:int(Rule),
    testPrim:enum(Rule),
    testPrim:obj_id(Rule),
    testPrim:rel_oid(Rule),
    testPrim:null(Rule),
    testPrim:real(Rule).

testCompactBitString(Config) -> test(Config, fun testCompactBitString/3).
testCompactBitString(Config, Rule, Opts) ->
    asn1_test_lib:compile("PrimStrings", Config,
                          [Rule, compact_bit_string|Opts]),
    testCompactBitString:compact_bit_string(Rule),
    ?only_uper(testCompactBitString:bit_string_unnamed(Rule)),
    ?only_per(testCompactBitString:bit_string_unnamed(Rule)),
    ?only_per_nif(testCompactBitString:ticket_7734(Rule)),
    ?only_per_nif(asn1_test_lib:compile("Constraints", Config,
                                        [Rule, compact_bit_string|Opts])),
    ?only_per_nif(testCompactBitString:otp_4869(Rule)).

testPrimStrings(Config) -> test(Config, fun testPrimStrings/3).
testPrimStrings(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["PrimStrings", "BitStr"], Config, [Rule|Opts]),
    testPrimStrings_cases(Rule),
    ?only_ber(testPrimStrings:more_strings(Rule)).

testPrimStrings_cases(Rule) ->
    testPrimStrings:bit_string(Rule),
    testPrimStrings:bit_string_unnamed(Rule),
    testPrimStrings:octet_string(Rule),
    testPrimStrings:numeric_string(Rule),
    testPrimStrings:other_strings(Rule),
    testPrimStrings:universal_string(Rule),
    testPrimStrings:bmp_string(Rule),
    testPrimStrings:times(Rule),
    testPrimStrings:utf8_string(Rule).

testPrimExternal(Config) -> test(Config, fun testPrimExternal/3).
testPrimExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "PrimExternal"], Config,
                              [Rule|Opts]),
    testPrimExternal:external(Rule),
    ?only_ber_nif(asn1_test_lib:compile_all(["PrimStrings", "BitStr"], Config,
                                            [Rule|Opts])),
    ?only_ber_nif(testPrimStrings_cases(Rule)),
    ?only_ber_nif(testPrimStrings:more_strings(Rule)).

testChoPrim(Config) -> test(Config, fun testChoPrim/3).
testChoPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoPrim", Config, [Rule|Opts]),
    testChoPrim:bool(Rule),
    testChoPrim:int(Rule).

testChoExtension(Config) -> test(Config, fun testChoExtension/3).
testChoExtension(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoExtension", Config, [Rule|Opts]),
    testChoExtension:extension(Rule).

testChoExternal(Config) -> test(Config, fun testChoExternal/3).
testChoExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "ChoExternal"], Config, [Rule|Opts]),
    testChoExternal:external(Rule).

testChoOptional(Config) -> test(Config, fun testChoOptional/3).
testChoOptional(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoOptional", Config, [Rule|Opts]),
    testChoOptional:optional(Rule).

testChoOptionalImplicitTag(Config) ->
    test(Config, fun testChoOptionalImplicitTag/3,
             [ber, ber_bin, ber_bin_v2]).
testChoOptionalImplicitTag(Config, Rule, Opts) ->
    %% Only meaningful for ber & co
    asn1_test_lib:compile("ChoOptionalImplicitTag", Config, [Rule|Opts]),
    testChoOptionalImplicitTag:optional(Rule).

testChoRecursive(Config) -> test(Config, fun testChoRecursive/3).
testChoRecursive(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoRecursive", Config, [Rule|Opts]),
    testChoRecursive:recursive(Rule).

testChoTypeRefCho(Config) -> test(Config, fun testChoTypeRefCho/3).
testChoTypeRefCho(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoTypeRefCho", Config, [Rule|Opts]),
    testChoTypeRefCho:choice(Rule).

testChoTypeRefPrim(Config) -> test(Config, fun testChoTypeRefPrim/3).
testChoTypeRefPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoTypeRefPrim", Config, [Rule|Opts]),
    testChoTypeRefPrim:prim(Rule).

testChoTypeRefSeq(Config) -> test(Config, fun testChoTypeRefSeq/3).
testChoTypeRefSeq(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoTypeRefSeq", Config, [Rule|Opts]),
    testChoTypeRefSeq:seq(Rule).

testChoTypeRefSet(Config) -> test(Config, fun testChoTypeRefSet/3).
testChoTypeRefSet(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoTypeRefSet", Config, [Rule|Opts]),
    testChoTypeRefSet:set(Rule).

testDef(Config) -> test(Config, fun testDef/3).
testDef(Config, Rule, Opts) ->
    asn1_test_lib:compile("Def", Config, [Rule|Opts]),
    testDef:main(Rule).

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

testSeqExtension(Config) -> test(Config, fun testSeqExtension/3).
testSeqExtension(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SeqExtension"], Config,
                              [Rule|Opts]),
    testSeqExtension:main(Rule).

testSeqExternal(Config) -> test(Config, fun testSeqExternal/3).
testSeqExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SeqExternal"], Config, [Rule|Opts]),
    testSeqExternal:main(Rule).

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

testSeqTag(Config) -> test(Config, fun testSeqTag/3).
testSeqTag(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SeqTag"], Config, [Rule|Opts]),
    testSeqTag:main(Rule).

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

testSeqOfIndefinite(Config) ->
    test(Config, fun testSeqOfIndefinite/3,
             [ber, ber_bin, ber_bin_v2, {ber_bin_v2, [nif]}]).
testSeqOfIndefinite(Config, Rule, Opts) ->
    Files = ["Mvrasn-Constants-1", "Mvrasn-DataTypes-1", "Mvrasn-21-4",
             "Mvrasn-20-4", "Mvrasn-19-4", "Mvrasn-18-4", "Mvrasn-17-4",
             "Mvrasn-15-4", "Mvrasn-14-4", "Mvrasn-11-4", "SeqOf"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testSeqOfIndefinite:main().

testSeqOfExternal(Config) -> test(Config, fun testSeqOfExternal/3).
testSeqOfExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SeqOfExternal"], Config,
                              [Rule|Opts]),
    testSeqOfExternal:main(Rule).

testSeqOfTag(Config) -> test(Config, fun testSeqOfTag/3).
testSeqOfTag(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SeqOfTag"], Config, [Rule|Opts]),
    testSeqOfTag:main(Rule).

testSetDefault(Config) -> test(Config, fun testSetDefault/3).
testSetDefault(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetDefault", Config, [Rule|Opts]),
    testSetDefault:main(Rule).

testParamBasic(Config) -> test(Config, fun testParamBasic/3).
testParamBasic(Config, Rule, Opts) ->
    asn1_test_lib:compile("ParamBasic", Config, [Rule|Opts]),
    testParamBasic:main(Rule).

testSetExtension(Config) -> test(Config, fun testSetExtension/3).
testSetExtension(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SetExtension"], Config,
                              [Rule|Opts]),
    testSetExtension:main(Rule).

testSetExternal(Config) -> test(Config, fun testSetExternal/3).
testSetExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SetExternal"], Config, [Rule|Opts]),
    testSetExternal:main(Rule).

testSetOptional(Config) -> test(Config, fun testSetOptional/3).
testSetOptional(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetOptional", Config, [Rule|Opts]),
    testSetOptional:ticket_7533(Rule),
    testSetOptional:main(Rule).

testSetPrim(Config) -> test(Config, fun testSetPrim/3).
testSetPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetPrim", Config, [Rule|Opts]),
    testSetPrim:main(Rule).

testSetTag(Config) -> test(Config, fun testSetTag/3).
testSetTag(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SetTag"], Config, [Rule|Opts]),
    testSetTag:main(Rule).

testSetTypeRefCho(Config) -> test(Config, fun testSetTypeRefCho/3).
testSetTypeRefCho(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetTypeRefCho", Config, [Rule|Opts]),
    testSetTypeRefCho:main(Rule).

testSetTypeRefPrim(Config) -> test(Config, fun testSetTypeRefPrim/3).
testSetTypeRefPrim(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetTypeRefPrim", Config, [Rule|Opts]),
    testSetTypeRefPrim:main(Rule).

testSetTypeRefSeq(Config) -> test(Config, fun testSetTypeRefSeq/3).
testSetTypeRefSeq(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetTypeRefSeq", Config, [Rule|Opts]),
    testSetTypeRefSeq:main(Rule).

testSetTypeRefSet(Config) -> test(Config, fun testSetTypeRefSet/3).
testSetTypeRefSet(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetTypeRefSet", Config, [Rule|Opts]),
    testSetTypeRefSet:main(Rule).

testSetOf(Config) -> test(Config, fun testSetOf/3).
testSetOf(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetOf", Config, [Rule|Opts]),
    testSetOf:main(Rule).

testSetOfCho(Config) -> test(Config, fun testSetOfCho/3).
testSetOfCho(Config, Rule, Opts) ->
    asn1_test_lib:compile("SetOfCho", Config, [Rule|Opts]),
    testSetOfCho:main(Rule).

testSetOfExternal(Config) -> test(Config, fun testSetOfExternal/3).
testSetOfExternal(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SetOfExternal"], Config,
                              [Rule|Opts]),
    testSetOfExternal:main(Rule).

testSetOfTag(Config) -> test(Config, fun testSetOfTag/3).
testSetOfTag(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["External", "SetOfTag"], Config, [Rule|Opts]),
    testSetOfTag:main(Rule).

c_syntax(Config) ->
    DataDir = ?config(data_dir, Config),
    [{error, _} = asn1ct:compile(filename:join(DataDir, F))
     || F <-["Syntax",
             "BadTypeEnding",
             "BadValueAssignment1",
             "BadValueAssignment2",
             "BadValueSet",
             "ChoiceBadExtension",
             "EnumerationBadExtension",
             "Example",
             "Export1",
             "MissingEnd",
             "SequenceBadComma",
             "SequenceBadComponentName",
             "SequenceBadComponentType",
             "SeqBadComma"]].

c_string(Config) ->
    test(Config, fun c_string/3, [per, per_bin, ber, ber_bin, ber_bin_v2]).
c_string(Config, Rule, Opts) ->
    asn1_test_lib:compile("String", Config, [Rule|Opts]).

c_implicit_before_choice(Config) ->
    test(Config, fun c_implicit_before_choice/3,
             [ber, ber_bin, ber_bin_v2]).
c_implicit_before_choice(Config, Rule, Opts) ->
    DataDir = ?config(data_dir, Config),
    CaseDir = ?config(case_dir, Config),
    {error, _R2} = asn1ct:compile(filename:join(DataDir, "CCSNARG3"),
                                  [Rule, {outdir, CaseDir}|Opts]).

parse(Config) ->
    [asn1_test_lib:compile(M, Config, [abs]) || M <- test_modules()].

per(Config) ->
    test(Config, fun per/3, [per, per_bin, {per_bin, [optimize]}]).
per(Config, Rule, Opts) ->
    [module_test(M, Config, Rule, Opts) || M <- per_modules()].

ber_other(Config) ->
    test(Config, fun ber_other/3, [ber, ber_bin, ber_bin_v2]).
ber_other(Config, Rule, Opts) ->
    [module_test(M, Config, Rule, Opts) || M <- ber_modules()].


module_test(M, Config, Rule, Opts) ->
    asn1_test_lib:compile(M, Config, [Rule|Opts]),
    case asn1ct:test(list_to_atom(M), [{i, ?config(case_dir, Config)}]) of
        ok    -> ok;
        Error ->
            erlang:error({test_failed, M, Opts, Error})
    end.


ber_choiceinseq(Config) ->
    test(Config, fun ber_choiceinseq/3, [ber, ber_bin, ber_bin_v2]).
ber_choiceinseq(Config, Rule, Opts) ->
    asn1_test_lib:compile("ChoiceInSeq", Config, [Rule|Opts]).

ber_optional(Config) ->
    test(Config, fun ber_optional/3, [ber, ber_bin, ber_bin_v2]).
ber_optional(Config, Rule, Opts) ->
    asn1_test_lib:compile("SOpttest", Config, [Rule|Opts]),
    V = {'S', {'A', 10, asn1_NOVALUE, asn1_NOVALUE},
              {'B', asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE},
              {'C', asn1_NOVALUE, 111, asn1_NOVALUE}},
    {ok, B} = asn1_wrapper:encode('SOpttest', 'S', V),
    Bytes = lists:flatten(B),
    V2 = asn1_wrapper:decode('SOpttest', 'S', Bytes),
    V = element(2, V2).

ber_optional_keyed_list(Config) ->
    test(Config, fun ber_optional_keyed_list/3, [ber, ber_bin]).
ber_optional_keyed_list(Config, Rule, Opts) ->
    asn1_test_lib:compile("SOpttest", Config, [Rule, keyed_list|Opts]),
    Vrecord = {'S', {'A', 10,           asn1_NOVALUE, asn1_NOVALUE},
        {'B', asn1_NOVALUE, asn1_NOVALUE, asn1_NOVALUE},
        {'C', asn1_NOVALUE, 111,          asn1_NOVALUE}},
    V = [{a, [{scriptKey, 10}]},
        {b, []},
        {c, [{callingPartysCategory, 111}]}],
    {ok, B} = asn1_wrapper:encode('SOpttest', 'S', V),
    Bytes = lists:flatten(B),
    V2 = asn1_wrapper:decode('SOpttest', 'S', Bytes),
    Vrecord = element(2, V2).

%% records used by test-case default
-record('Def1', {bool0,
                 bool1 = asn1_DEFAULT,
                 bool2 = asn1_DEFAULT,
                 bool3 = asn1_DEFAULT}).

default(Config) -> test(Config, fun default/3).
default(Config, Rule, Opts) ->
    asn1_test_lib:compile("Def", Config, [Rule|Opts]),
    {ok, Bytes1} = asn1_wrapper:encode('Def', 'Def1', #'Def1'{bool0 = true}),
    {ok, {'Def1', true, false, false, false}} =
        asn1_wrapper:decode('Def', 'Def1', lists:flatten(Bytes1)),

    {ok, Bytes2} = asn1_wrapper:encode('Def', 'Def1', #'Def1'{bool0 = true,
                                                              bool2 = false}),
    {ok, {'Def1', true, false, false, false}} =
        asn1_wrapper:decode('Def', 'Def1', lists:flatten(Bytes2)).

value_test(Config) -> test(Config, fun value_test/3).
value_test(Config, Rule, Opts) ->
    asn1_test_lib:compile("ObjIdValues", Config, [Rule|Opts]),
    {ok, _} = asn1ct:test('ObjIdValues', 'ObjIdType',
                          'ObjIdValues':'mobileDomainId'()).

value_bad_enum_test(Config) ->
    case ?MODULE of
        asn1_SUITE ->
            {error, _} = asn1ct:compile(?config(data_dir, Config)
                                            ++ "BadEnumValue1",
                                        [{outdir, ?config(case_dir, Config)}]);
        _ -> {skip, "Runs in asn1_SUITE only"}
    end.

constructed(Config) ->
    test(Config, fun constructed/3, [ber, ber_bin, ber_bin_v2]).
constructed(Config, Rule, Opts) ->
    asn1_test_lib:compile("Constructed", Config, [Rule|Opts]),
    {ok, B} = asn1_wrapper:encode('Constructed', 'S', {'S', false}),
    [40, 3, 1, 1, 0] = lists:flatten(B),
    {ok, B1} = asn1_wrapper:encode('Constructed', 'S2', {'S2', false}),
    [40, 5, 48, 3, 1, 1, 0] = lists:flatten(B1),
    {ok, B2} = asn1_wrapper:encode('Constructed', 'I', 10),
    [136, 1, 10] = lists:flatten(B2).

ber_decode_error(Config) ->
    test(Config, fun ber_decode_error/3, [ber, ber_bin, ber_bin_v2]).
ber_decode_error(Config, Rule, Opts) ->
    asn1_test_lib:compile("Constructed", Config, [Rule|Opts]),
    ber_decode_error:run(Opts).

h323test(Config) -> test(Config, fun h323test/3).
h323test(Config, Rule, Opts) ->
    Files = ["H235-SECURITY-MESSAGES", "H323-MESSAGES",
             "MULTIMEDIA-SYSTEM-CONTROL"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    h323test:run(Rule).

per_GeneralString(Config) ->
    test(Config, fun per_GeneralString/3, [per, per_bin]).
per_GeneralString(Config, Rule, Opts) ->
    asn1_test_lib:compile("MULTIMEDIA-SYSTEM-CONTROL", Config, [Rule|Opts]),
    UI = [109, 64, 1, 57],
    {ok, _V} = asn1_wrapper:decode('MULTIMEDIA-SYSTEM-CONTROL',
                                   'MultimediaSystemControlMessage', UI).

per_open_type(Config) -> test(Config, fun per_open_type/3, [per, per_bin]).
per_open_type(Config, Rule, Opts) ->
    asn1_test_lib:compile("OpenType", Config, [Rule|Opts]),
    {ok, _} = asn1ct:test('OpenType', 'Ot', {'Stype', 10, true}).

testConstraints(Config) -> test(Config, fun testConstraints/3).
testConstraints(Config, Rule, Opts) ->
    asn1_test_lib:compile("Constraints", Config, [Rule|Opts]),
    asn1_test_lib:compile("LargeConstraints", Config, [Rule|Opts]),
    testConstraints:int_constraints(Rule).


testSeqIndefinite(Config) ->
    test(Config, fun testSeqIndefinite/3, [ber, ber_bin, ber_bin_v2,
                                               {ber_bin_v2, [nif]}]).
testSeqIndefinite(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqSetIndefinite", Config, [Rule|Opts]),
    testSeqIndefinite:main(Rule).


testSetIndefinite(Config) ->
    test(Config, fun testSetIndefinite/3, [ber, ber_bin, ber_bin_v2,
                                               {ber_bin_v2, [nif]}]).
testSetIndefinite(Config, Rule, Opts) ->
    asn1_test_lib:compile("SeqSetIndefinite", Config, [Rule|Opts]),
    testSetIndefinite:main(Rule).


testChoiceIndefinite(Config) ->
    test(Config, fun testChoiceIndefinite/3, [ber, ber_bin, ber_bin_v2,
                                                  {ber_bin_v2, [nif]}]).
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

testParameterizedInfObj(Config) ->
    test(Config, fun testParameterizedInfObj/3).
testParameterizedInfObj(Config, Rule, Opts) ->
    asn1_test_lib:compile("Param", Config, [Rule|Opts]),
    testParameterizedInfObj:main(Rule).

testMergeCompile(Config) -> test(Config, fun testMergeCompile/3).
testMergeCompile(Config, Rule, Opts) ->
    Files = ["MS.set.asn", "RANAPSET.set.asn1", "Mvrasn4.set.asn",
             "Mvrasn6.set.asn"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    testMergeCompile:main(Rule),
    testMergeCompile:mvrasn(Rule).

testobj(Config) -> test(Config, fun testobj/3).
testobj(Config, Rule, Opts) ->
    asn1_test_lib:compile("RANAP", Config, [Rule|Opts]),
    asn1_test_lib:compile_erlang("testobj", Config, []),
    ok = testobj:run(),
    ok = testParameterizedInfObj:ranap(Rule).

testDeepTConstr(Config) -> test(Config, fun testDeepTConstr/3).
testDeepTConstr(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["TConstrChoice", "TConstr"], Config,
                              [Rule|Opts]),
    testDeepTConstr:main(Rule).

testInvokeMod(Config) -> test(Config, fun testInvokeMod/3).
testInvokeMod(Config, Rule, Opts) ->
    asn1_test_lib:compile("PrimStrings", Config, [Rule|Opts]),
    {ok, _Result2} = 'PrimStrings':encode('Bs1', [1, 0, 1, 0]).

testExport(Config) ->
    case ?MODULE of
        asn1_SUITE ->
            {error, {asn1, _Reason}} =
                asn1ct:compile(filename:join(?config(data_dir, Config),
                                             "IllegalExport"),
                               [{outdir, ?config(case_dir, Config)}]);
        _ ->
            {skip, "Runs in asn1_SUITE only"}
    end.

testImport(Config) ->
    test(Config, fun testImport/3, [ber, ber_bin, ber_bin_v2]).
testImport(Config, Rule, Opts) ->
    {error, _} = asn1ct:compile(filename:join(?config(data_dir, Config),
                                              "ImportsFrom"),
                                [Rule, {outdir, ?config(priv_dir, Config)}
                                 |Opts]).

testMegaco(Config) -> test(Config, fun testMegaco/3).
testMegaco(Config, Rule, Opts) ->
    {ok, Module1, Module2} = testMegaco:compile(Config, Rule, Opts),
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
    testContextSwitchingTypes:test(Config).

testTypeValueNotation(Config) -> test(Config, fun testTypeValueNotation/3).
testTypeValueNotation(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["SeqTypeRefPrim", "ValueTest"], Config,
                              [Rule|Opts]),
    testTypeValueNotation:main(Rule, Opts).

testOpenTypeImplicitTag(Config) ->
    test(Config, fun testOpenTypeImplicitTag/3).
testOpenTypeImplicitTag(Config, Rule, Opts) ->
    asn1_test_lib:compile("OpenTypeImplicitTag", Config, [Rule|Opts]),
    testOpenTypeImplicitTag:main(Rule).

duplicate_tags(Config) ->
    case ?MODULE of
        asn1_SUITE ->
            DataDir = ?config(data_dir, Config),
            CaseDir = ?config(case_dir, Config),
            {error, {asn1, [{error, {type, _, _, 'SeqOpt1Imp', {asn1, {duplicates_of_the_tags, _}}}}]}} =
            asn1ct:compile(filename:join(DataDir, "SeqOptional2"),
                           [abs, {outdir, CaseDir}]);
        _ ->
            {skip, "Runs in asn1_SUITE only"}
    end.

rtUI(Config) -> test(Config, fun rtUI/3, [per, per_bin, ber,
            ber_bin, ber_bin_v2]).
rtUI(Config, Rule, Opts) ->
    asn1_test_lib:compile("Prim", Config, [Rule|Opts]),
    {ok, _} = asn1rt:info('Prim').

testROSE(Config) -> test(Config, fun testROSE/3).
testROSE(Config, Rule, Opts) ->
    asn1_test_lib:compile("Remote-Operations-Merged.set.asn1", Config,
                          [Rule|Opts]).

testINSTANCE_OF(Config) -> test(Config, fun testINSTANCE_OF/3).
testINSTANCE_OF(Config, Rule, Opts) ->
    asn1_test_lib:compile("INSTANCEOF.asn1", Config, [Rule|Opts]),
    testINSTANCE_OF:main(Rule).

testTCAP(Config) ->
    test(Config, fun testTCAP/3,
             [ber, ber_bin, ber_bin_v2, {ber_bin_v2, [nif]}]).
testTCAP(Config, Rule, Opts) ->
    testTCAP:compile(Config, [Rule|Opts]),
    testTCAP:test(Rule, Config),
    case Rule of
        ber_bin_v2 -> testTCAP:compile_asn1config(Config, [Rule, asn1config]),
                      testTCAP:test_asn1config();
        _          -> ok
    end.

testDER(Config) ->
    test(Config, fun testDER/3, [ber, ber_bin, ber_bin_v2]).
testDER(Config, Rule, Opts) ->
    asn1_test_lib:compile("DERSpec", Config, [Rule, der|Opts]),
    testDER:test(),
    asn1_test_lib:compile("ParamBasic", Config, [Rule, der|Opts]),
    testParamBasic:main(der),
    asn1_test_lib:compile("Default", Config, [Rule, der|Opts]),
    testSeqSetDefaultVal:main(Rule).

specialized_decodes(Config) ->
    test(Config, fun specialized_decodes/3, [ber_bin_v2]).
specialized_decodes(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["PartialDecSeq.asn",
                               "PartialDecSeq2.asn",
                               "PartialDecSeq3.asn",
                               "PartialDecMyHTTP.asn",
                               "MEDIA-GATEWAY-CONTROL.asn",
                               "P-Record"],
                              Config, [Rule, optimize, asn1config|Opts]),
    test_partial_incomplete_decode:test(Config),
    test_selective_decode:test().

special_decode_performance(Config) ->
    test(Config, fun special_decode_performance/3,
             [{ber_bin, [optimize]}, {ber_bin_v2, [optimize, nif]}]).
special_decode_performance(Config, Rule, Opts) ->
    Files = ["MEDIA-GATEWAY-CONTROL", "PartialDecSeq"],
    asn1_test_lib:compile_all(Files, Config, [Rule, asn1config|Opts]),
    test_special_decode_performance:go(all).


test_driver_load(Config) ->
    test(Config, fun test_driver_load/3, [{per_bin, [optimize]}]).
test_driver_load(Config, Rule, Opts) ->
    asn1_test_lib:compile("P-Record", Config, [Rule|Opts]),
    test_driver_load:test(5).

test_ParamTypeInfObj(Config) ->
    asn1_test_lib:compile("IN-CS-1-Datatypes", Config, [ber_bin]).

test_WS_ParamClass(Config) ->
    asn1_test_lib:compile("InformationFramework", Config, [ber_bin]).

test_Defed_ObjectIdentifier(Config) ->
    asn1_test_lib:compile("UsefulDefinitions", Config, [ber_bin]).

testSelectionType(Config) -> test(Config, fun testSelectionType/3).
testSelectionType(Config, Rule, Opts) ->
    asn1_test_lib:compile("SelectionType", Config, [Rule|Opts]),
    {ok, _}  = testSelectionTypes:test().

testSSLspecs(Config) ->
    test(Config, fun testSSLspecs/3, [ber, ber_bin, ber_bin_v2, {ber_bin_v2, [optimize]}]).
testSSLspecs(Config, Rule, Opts) ->
    ok = testSSLspecs:compile(Config,
                              [Rule, compact_bit_string, der|Opts]),
    testSSLspecs:run(Rule),

    case code:which(asn1ct) of
       cover_compiled ->
           ok;
       _ ->
           ok = testSSLspecs:compile_inline(Config, Rule),
           ok = testSSLspecs:run_inline(Rule)
    end.

testNortel(Config) -> test(Config, fun testNortel/3).
testNortel(Config, Rule, Opts) ->
    asn1_test_lib:compile("Nortel", Config, [Rule|Opts]).

test_undecoded_rest(Config) -> test(Config, fun test_undecoded_rest/3).
test_undecoded_rest(Config, Rule, Opts) ->
    asn1_test_lib:compile("P-Record", Config, [Rule|Opts]),
    ok = test_undecoded_rest:test([], Config),
    asn1_test_lib:compile("P-Record", Config, [Rule,undec_rest|Opts]),
    case Rule of
        ber_bin_v2 -> ok;
        _ -> test_undecoded_rest:test(undec_rest, Config)
    end.

test_inline(Config) ->
    test(Config, fun test_inline/3, [ber, ber_bin, ber_bin_v2]).
test_inline(Config, Rule, Opts) ->
    case code:which(asn1ct) of
        cover_compiled ->
            {skip, "Not runnable when cover compiled"};
        _  ->
            test_inline:compile(Config, Opts),
            test_inline:main(Config, Rule),
            test_inline:inline1(Config, Rule, Opts),
            test_inline:performance2()
    end.

testTcapsystem(Config) ->
    test(Config, fun testTcapsystem/3, [ber, ber_bin, ber_bin_v2]).
testTcapsystem(Config, Rule, Opts) ->
    testTcapsystem:compile(Config, [Rule|Opts]).

testNBAPsystem(Config) -> test(Config, fun testNBAPsystem/3,
        [per, per_bin, {per_bin, [optimize]}]).
testNBAPsystem(Config, Rule, Opts) ->
    testNBAPsystem:compile(Config, [Rule|Opts]),
    testNBAPsystem:test(Rule, Config).

test_compile_options(Config) ->
    case code:which(asn1ct) of
        cover_compiled ->
            {skip, "Not runnable when cover compiled"};
        _  ->
            ok = test_compile_options:wrong_path(Config),
            ok = test_compile_options:path(Config),
            ok = test_compile_options:noobj(Config),
            ok = test_compile_options:record_name_prefix(Config),
            ok = test_compile_options:verbose(Config),
            ok = test_compile_options:warnings_as_errors(Config)
    end.

testDoubleEllipses(Config) -> test(Config, fun testDoubleEllipses/3).
testDoubleEllipses(Config, Rule, Opts) ->
    asn1_test_lib:compile("DoubleEllipses", Config, [Rule|Opts]),
    testDoubleEllipses:main(Rule).

test_modified_x420(Config) ->
    Files = [filename:join(modified_x420, F) || F <- ["PKCS7",
                                                      "InformationFramework",
                                                      "AuthenticationFramework"]],
    asn1_test_lib:compile_all(Files, Config, [der]),
    test_modified_x420:test_io(Config).


testX420() ->
    [{timetrap,{minutes,90}}].
testX420(Config) ->
    test(Config, fun testX420/3, [ber, ber_bin, ber_bin_v2]).
testX420(Config, Rule, Opts) ->
    testX420:compile(Rule, [der|Opts], Config),
    ok = testX420:ticket7759(Rule, Config),
    testX420:compile(Rule, Opts, Config).

test_x691(Config) ->
    test(Config, fun test_x691/3,
             [per, per_bin, uper_bin, {per_bin, [optimize]}]).
test_x691(Config, Rule, Opts) ->
    Files = ["P-RecordA1", "P-RecordA2", "P-RecordA3"],
    asn1_test_lib:compile_all(Files, Config, [Rule|Opts]),
    test_x691:cases(Rule, case Rule of
                              uper_bin -> unaligned;
                              _ -> aligned
                          end),
    asn1_test_lib:ticket_7708(Config, []),
    asn1_test_lib:ticket_7763(Config).

ticket_6143(Config) ->
    ok = test_compile_options:ticket_6143(Config).

testExtensionAdditionGroup(Config) ->
    %% FIXME problems with automatic tags [ber_bin], [ber_bin, optimize]
    test(Config, fun testExtensionAdditionGroup/3,
             [per_bin, {per_bin, [optimize]}, uper_bin]).
testExtensionAdditionGroup(Config, Rule, Opts) ->
    asn1_test_lib:compile("Extension-Addition-Group", Config, [Rule|Opts]),
    asn1_test_lib:compile_erlang("extensionAdditionGroup", Config,
                                 [debug_info]),
    extensionAdditionGroup:run([Rule|Opts]),
    extensionAdditionGroup:run2([Rule|Opts]),
    asn1_test_lib:compile("EUTRA-RRC-Definitions", Config, [Rule, {record_name_prefix, "RRC-"}|Opts]),
    extensionAdditionGroup:run3([Rule|Opts]).

% parse_modules() ->
%   ["ImportsFrom"].

per_modules() ->
    [X || X <- test_modules()].

ber_modules() ->
    [X || X <- test_modules(),
          X =/= "CommonDataTypes",
          X =/= "DS-EquipmentUser-CommonFunctionOrig-TransmissionPath",
          X =/= "H323-MESSAGES",
          X =/= "H235-SECURITY-MESSAGES",
          X =/= "MULTIMEDIA-SYSTEM-CONTROL"].

test_modules() ->
    ["BitStr",
     "CAP",
     "CommonDataTypes",
     "Constraints",
     "ContextSwitchingTypes",
     "DS-EquipmentUser-CommonFunctionOrig-TransmissionPath",
     "Enum",
     "From",
     "H235-SECURITY-MESSAGES",
     "H323-MESSAGES",
     "Import",
     "Int",
     "MAP-commonDataTypes",
     "Null",
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
     "LDAP"].

test_OTP_9688(Config) ->
    PrivDir = ?config(case_dir, Config),
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


timer_compile(Config, Rule, Opts) ->
    asn1_test_lib:compile_all(["H235-SECURITY-MESSAGES", "H323-MESSAGES"],
                              Config, [Rule|Opts]).

testTimer_ber(suite) -> [];
testTimer_ber(Config) ->
    timer_compile(Config,ber,[]),
    testTimer:go(Config,ber).

testTimer_ber_bin(suite) -> [];
testTimer_ber_bin(Config) ->
    timer_compile(Config,ber_bin,[]),
    testTimer:go(Config,ber_bin).

testTimer_ber_bin_opt(suite) -> [];
testTimer_ber_bin_opt(Config) ->
    timer_compile(Config,ber_bin,[optimize]),
    testTimer:go(Config,ber_bin).

testTimer_ber_bin_opt_driver(suite) -> [];
testTimer_ber_bin_opt_driver(Config) ->
    timer_compile(Config,ber_bin,[optimize,driver]),
    testTimer:go(Config,ber_bin).

testTimer_per(suite) -> [];
testTimer_per(Config) ->
    timer_compile(Config,per,[]),
    testTimer:go(Config,per).

testTimer_per_bin(suite) -> [];
testTimer_per_bin(Config) ->
    timer_compile(Config,per_bin,[]),
    testTimer:go(Config,per_bin).

testTimer_per_bin_opt(suite) -> [];
testTimer_per_bin_opt(Config) ->
    timer_compile(Config,per_bin,[optimize]),
    testTimer:go(Config,per_bin).


testTimer_uper_bin(suite) -> [];
testTimer_uper_bin(Config) ->
    timer_compile(Config,uper_bin,[]),
    {comment,_} = testTimer:go(Config,uper_bin).

%% Test of multiple-line comment, OTP-8043
testComment(suite) -> [];
testComment(Config) ->
    asn1_test_lib:compile("Comment", Config, []),
    {ok,Enc} = asn1_wrapper:encode('Comment','Seq',{'Seq',12,true}),
    {ok,{'Seq',12,true}} = asn1_wrapper:decode('Comment','Seq',Enc),
    ok.

testName2Number(suite) -> [];
testName2Number(Config) ->
    N2NOptions = [{n2n,Type} || Type <- ['CauseMisc', 'CauseProtocol',
                                         'CauseRadioNetwork',
                                         'CauseTransport','CauseNas']],
    asn1_test_lib:compile("S1AP-IEs", Config, N2NOptions),

    0 = 'S1AP-IEs':name2num_CauseMisc('control-processing-overload'),
    'unknown-PLMN' = 'S1AP-IEs':num2name_CauseMisc(5),

    %% OTP-10144
    %% Test that n2n option generates name2num and num2name functions supporting
    %% values not within the extension root if the enumeration type has an
    %% extension marker.
    N2NOptionsExt = [{n2n, 'NoExt'}, {n2n, 'Ext'}, {n2n, 'Ext2'}],
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
    asn1_test_lib:compile("EUTRA-extract-7407", Config, [uper_bin]),
    asn1_test_lib:ticket_7407_code(true),

    asn1_test_lib:compile("EUTRA-extract-7407", Config,
                          [uper_bin, no_final_padding]),
    asn1_test_lib:ticket_7407_code(false).

smp(suite) -> [];
smp(Config)  ->
    case erlang:system_info(smp_support) of
        true ->
            NumOfProcs = erlang:system_info(schedulers),
            io:format("smp starting ~p workers\n",[NumOfProcs]),

            Msg = {initiatingMessage, testNBAPsystem:cell_setup_req_msg()},
            ok = testNBAPsystem:compile(Config, [per_bin, optimize]),

            enc_dec(NumOfProcs,Msg,2),

            N = 10000,

            {Time1,ok} = timer:tc(?MODULE,enc_dec,[NumOfProcs,Msg, N]),
            {Time1S,ok} = timer:tc(?MODULE,enc_dec,[1, Msg, NumOfProcs * N]),

            ok = testNBAPsystem:compile(Config, [ber_bin, optimize, nif]),
            {Time3,ok} = timer:tc(?MODULE,enc_dec,[NumOfProcs,Msg, N]),

            {Time3S,ok} = timer:tc(?MODULE,enc_dec,[1, Msg, NumOfProcs * N]),

            {comment,lists:flatten(
                       io_lib:format(
                         "Encode/decode time parallell with ~p cores: ~p [microsecs]~n"
                         "Encode/decode time sequential: ~p [microsecs]",
                         [NumOfProcs,Time1+Time3,Time1S+Time3S]))};
        false ->
            {skipped,"No smp support"}
    end.

per_performance(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    NifDir = filename:join(PrivDir,"nif"),
    ErlDir = filename:join(PrivDir,"erl"),
    file:make_dir(NifDir),file:make_dir(ErlDir),

    Msg = {initiatingMessage, testNBAPsystem:cell_setup_req_msg()},
    ok = testNBAPsystem:compile([{priv_dir,NifDir}|Config],
                                      [per_bin, optimize]),
    ok = testNBAPsystem:compile([{priv_dir,ErlDir}|Config],
                                      [per_bin]),

    Modules = ['NBAP-CommonDataTypes',
               'NBAP-Constants',
               'NBAP-Containers',
               'NBAP-IEs',
               'NBAP-PDU-Contents',
               'NBAP-PDU-Discriptions'],


    PreNif = fun() ->
                     code:add_patha(NifDir),
                     lists:foreach(fun(M) ->
                                           code:purge(M),
                                           code:load_file(M)
                                   end,Modules)
             end,

    PreErl = fun() ->
                     code:add_patha(ErlDir),
                     lists:foreach(fun(M) ->
                                           code:purge(M),
                                           code:load_file(M)
                                   end,Modules)
             end,

    Func = fun() ->
                   element(1,timer:tc(
                               asn1_wrapper,encode,['NBAP-PDU-Discriptions',
                                                    'NBAP-PDU',
                                                    Msg]))
           end,

    nif_vs_erlang_performance({{{PreNif,Func},{PreErl,Func}},100000,32}).

ber_performance(Config) ->

    Msg = {initiatingMessage, testNBAPsystem:cell_setup_req_msg()},
    ok = testNBAPsystem:compile(Config, [ber_bin, optimize, nif]),


    BerFun = fun() ->
                     {ok,B} = asn1_wrapper:encode('NBAP-PDU-Discriptions',
                                                  'NBAP-PDU', Msg),
                     asn1_wrapper:decode(
                        'NBAP-PDU-Discriptions',
                        'NBAP-PDU',
                        B)
             end,
    nif_vs_erlang_performance({BerFun,100000,32}).

cert_pem_performance(Config) when is_list(Config) ->
    cert_pem_performance({100000, 32});
cert_pem_performance({N,S}) ->
    nif_vs_erlang_performance({fun pem_performance:cert_pem/0,N,S}).

dsa_pem_performance(Config) when is_list(Config) ->
    dsa_pem_performance({100000, 32});
dsa_pem_performance({N,S}) ->
    nif_vs_erlang_performance({fun pem_performance:dsa_pem/0,N,S}).


nif_vs_erlang_performance({{TC1,TC2},N,Sched}) ->
    random:seed({123,456,789}),
    io:format("Running a ~p sample with ~p max procs...~n~n",[N,Sched]),

    {True,False} = exec(TC1,TC2,Sched,N+1),

    io:format("~ndone!~n"),

    io:format("~n"),TStats = print_stats(strip(True,N div 20)),
    io:format("~n"),FStats = print_stats(strip(False,N div 20)),
    Str = io_lib:format("~nNifs are ~.3f% faster than erlang!~n",
                        [(element(2,FStats) - element(2,TStats)) /
                             element(2,FStats) * 100]),
    io:format(Str),
    {comment, lists:flatten(Str)};
nif_vs_erlang_performance({T,N,Sched}) ->
    PTC1 = fun() ->
                  application:set_env(asn1, nif_loadable, true)
           end,
    PTC2 = fun() ->
                  application:set_env(asn1, nif_loadable, false)
           end,
    TC = fun() ->
                 element(1,timer:tc(T))
         end,
    nif_vs_erlang_performance({{{PTC1,TC},{PTC2,TC}},N,Sched}).


print_stats(Data) ->
    Length = length(Data),
    Mean = lists:sum(Data) / Length,
    Variance = lists:foldl(fun(N,Acc) -> math:pow(N - Mean, 2)+Acc end, 0, Data),
    StdDev = math:sqrt(Variance / Length),
    Median = lists:nth(round(Length/2),Data),
    Min = lists:min(Data),
    Max = lists:max(Data),
    if Length < 20 ->
            io:format("Data: ~w~n",[Data]);
       true ->
            ok
    end,
    io:format("Length: ~p~nMean: ~p~nStdDev: ~p~nMedian: ~p~nMin: ~p~nMax: ~p~n",
              [Length,Mean,StdDev,Median,Min,Max]),
    {Length,Mean,StdDev,Median,Min,Max}.

collect(Acc) ->
    receive
        {Tag,Val} ->
            Prev = proplists:get_value(Tag,Acc,[]),
            collect(lists:keystore(Tag,1,Acc,{Tag,[Val|Prev]}))
    after 100 ->
            Acc
    end.

exec(One,Two,Max,N) ->
    exec(One,Two,Max,N,{[],[]}).
exec(_,_,_,1,{D1,D2}) ->
    {lists:flatten(D1),lists:flatten(D2)};
exec({PreOne,One} = O,{PreTwo,Two} = T,MaxProcs, N, {D1,D2}) ->
    Num = random:uniform(round(N/2)),
    if Num rem 3 == 0 ->
            timer:sleep(Num rem 1000);
       true ->
            ok
    end,
    Procs = random:uniform(MaxProcs),
    io:format("\tBatch: ~p items in ~p processes, ~p left~n",[Num,Procs,N-Num]),
    if Num rem 2 == 1 ->
            erlang:garbage_collect(),
            PreOne(),
            MoreOne = pexec(One, Num, Procs, []),
            erlang:garbage_collect(),
            PreTwo(),
            MoreTwo = pexec(Two, Num, Procs, []);
       true ->
            erlang:garbage_collect(),
            PreTwo(),
            MoreTwo = pexec(Two, Num, Procs, []),
            erlang:garbage_collect(),
            PreOne(),
            MoreOne = pexec(One, Num, Procs, [])
    end,
    exec(O,T,MaxProcs,N-Num,{[MoreOne|D1],
                             [MoreTwo|D2]}).

pexec(_Fun, _, 0, []) ->
    [];
pexec(Fun, _, 0, [{Ref,Pid}|Rest]) ->
    receive
        {data,D} ->
            [D|pexec(Fun,0,0,[{Ref,Pid}|Rest])];
        {'DOWN', Ref, process, Pid, normal} ->
            pexec(Fun, 0,0,Rest)
    end;
pexec(Fun, 0, 1, AccProcs) ->
    pexec(Fun, 0, 0, AccProcs);
pexec(Fun, N, 1, AccProcs) ->
    [Fun()|pexec(Fun, N - 1, 1, AccProcs)];
pexec(Fun, N, Procs, AccProcs) ->
    S = self(),
    Pid = spawn(fun() ->
                        S ! {data,pexec(Fun,N,1,[])}
                end),
    Ref = erlang:monitor(process, Pid),
    pexec(Fun, N, Procs - 1, [{Ref,Pid}|AccProcs]).

strip(Data,Num) ->
    {_,R} = lists:split(Num,lists:sort(Data)),
    element(2,lists:split(Num,lists:reverse(R))).

faster(A,B) ->
    (B - A)/B * 100.

enc_dec(1, Msg, N) ->
    worker_loop(N, Msg);
enc_dec(NumOfProcs,Msg, N) ->
    pforeach(fun(_) ->
                     worker_loop(N, Msg)
             end, [I || I <- lists:seq(1,NumOfProcs)]).

worker_loop(0, _Msg) ->
    ok;
worker_loop(N, Msg) ->
    {ok,B}=asn1_wrapper:encode('NBAP-PDU-Discriptions',
                                     'NBAP-PDU',
                                     Msg),
    {ok,_Msg}=asn1_wrapper:decode('NBAP-PDU-Discriptions',
                                        'NBAP-PDU',
                                        B),
    worker_loop(N - 1, Msg).


pforeach(Fun, List) ->
    pforeach(Fun, List, []).
pforeach(Fun, [], [{Pid,Ref}|Pids]) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            pforeach(Fun, [], Pids)
    end;
pforeach(Fun, [H|T], Pids) ->
    Pid = spawn(fun() -> Fun(H) end),
    Ref = erlang:monitor(process, Pid),
    pforeach(Fun, T, [{Pid, Ref}|Pids]);
pforeach(_Fun,[],[]) ->
    ok.

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{first,second}).

ticket7904(Config) ->
    asn1_test_lib:compile("RANAPextract1", Config, [per_bin, optimize]),

    Val1 = #'InitiatingMessage'{procedureCode=1,
                                criticality=ignore,
                                value=#'Iu-ReleaseCommand'{
                                  first=13,
                                  second=true}},

    {ok,_} = 'RANAPextract1':encode('InitiatingMessage', Val1),
    {ok,_} = 'RANAPextract1':encode('InitiatingMessage', Val1).
