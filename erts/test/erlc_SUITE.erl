%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-module(erlc_SUITE).

%% Tests the erlc command by compiling various types of files.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
         compile_erl/1,
         compile_yecc/1, compile_script/1,
         compile_mib/1, good_citizen/1, deep_cwd/1, arg_overflow/1,
         make_dep_options/1,
         unicode_paths/1,
         features_erlc_describe/1,
         features_erlc_unknown/1,
         features_directives/1,
         features_atom_warnings/1,
         features_macros/1,
         features_disable/1,
         features_all/1,
         features_load/1,
         features_runtime/1,
         features_include/1,
         features/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,with_server},{group,without_server},
     {group,features_with_server}].

groups() ->
    Tests = tests(),
    [{with_server,[],Tests},
     {features_with_server,[],feature_tests()},
     {without_server,[],Tests ++ feature_tests()}].

tests() ->
    [compile_erl, compile_yecc, compile_script, compile_mib,
     good_citizen, deep_cwd, arg_overflow, make_dep_options,
     unicode_paths].

feature_tests() ->
    [features_erlc_describe,
     features_erlc_unknown,
     features_directives,
     features_atom_warnings,
     features_macros,
     features_disable,
     features_all,
     features_load,
     features_runtime,
     features_include,
     features].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(with_server, Config) ->
    os:putenv("ERLC_USE_SERVER", "yes"),
    Config;
init_per_group(features_with_server, Config) ->
    os:putenv("ERLC_USE_SERVER", "yes"),
    timer:sleep(12 * 1000),
    Config;
init_per_group(without_server, Config) ->
    os:putenv("ERLC_USE_SERVER", "no"),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    os:unsetenv("ERLC_USE_SERVER"),
    os:unsetenv("OTP_TEST_FEATURES"),
    Config.

init_per_testcase(TestCase, Config) ->
    case lists:member(TestCase, feature_tests()) of
        true ->
            os:putenv("OTP_TEST_FEATURES", "true");
        false ->
            ok
        end,
    Config.

end_per_testcase(TestCase, Config) ->
    case lists:member(TestCase, feature_tests()) of
        true ->
            os:putenv("OTP_TEST_FEATURES", "true");
        false ->
            ok
        end,
    Config.


%% Copy from erlc_SUITE_data/include/erl_test.hrl.

-record(person, {name, shoe_size}).

%% Tests that compiling Erlang source code works.

compile_erl(Config) when is_list(Config) ->
    {SrcDir, OutDir, Cmd0} = get_cmd(Config),
    Cmd = Cmd0 ++ " +brief",
    FileName = filename:join(SrcDir, "erl_test_ok.erl"),

    %% By default, warnings are now turned on.
    run(Config, Cmd, FileName, "",
        ["Warning: function foo/0 is unused\$", "_OK_"]),

    %% Test that the compiled file is where it should be,
    %% and that it is runnable.

    {module, erl_test_ok} = code:load_abs(filename:join(OutDir, "erl_test_ok")),
    42 = erl_test_ok:shoe_size(#person{shoe_size=42}),
    code:purge(erl_test_ok),

    %% Try disabling warnings.

    run(Config, Cmd, FileName, "-W0", ["_OK_"]),

    %% Try treating warnings as errors.

    run(Config, Cmd, FileName, "-Werror",
        ["compile: warnings being treated as errors\$",
         "function foo/0 is unused\$", "_ERROR_"]),

    %% Check a bad file.

    BadFile = filename:join(SrcDir, "erl_test_bad.erl"),
    run(Config, Cmd, BadFile, "", ["function non_existing/1 undefined\$",
                                   "_ERROR_"]),
    ok.

%% Test that compiling yecc source code works.

compile_yecc(Config) when is_list(Config) ->
    {SrcDir, _, OutDir} = get_dirs(Config),
    Cmd = erlc() ++ " -o" ++ OutDir ++ " ",
    FileName = filename:join(SrcDir, "yecc_test_ok.yrl"),
    run(Config, Cmd, FileName, "-W0", ["_OK_"]),
    true = exists(filename:join(OutDir, "yecc_test_ok.erl")),

    BadFile = filename:join(SrcDir, "yecc_test_bad.yrl"),
    run(Config, Cmd, BadFile, "-W0",
        ["Nonterminals is missing\$",
         "rootsymbol form is not a nonterminal\$",
         "undefined nonterminal: form\$",
         "_ERROR_"]),
    exists(filename:join(OutDir, "yecc_test_ok.erl")),
    ok.

%% Test that compiling start scripts works.

compile_script(Config) when is_list(Config) ->
    {SrcDir, OutDir, Cmd} = get_cmd(Config),
    FileName = filename:join(SrcDir, "start_ok.script"),
    run(Config, Cmd, FileName, "", ["_OK_"]),
    true = exists(filename:join(OutDir, "start_ok.boot")),

    BadFile = filename:join(SrcDir, "start_bad.script"),
    run(Config, Cmd, BadFile, "", ["syntax error before:", "_ERROR_"]),
    ok.

%% Test that compiling SNMP mibs works.

compile_mib(Config) when is_list(Config) ->
    {SrcDir, OutDir, Cmd} = get_cmd(Config),
    FileName = filename:join(SrcDir, "GOOD-MIB.mib"),
    run(Config, Cmd, FileName, "", ["_OK_"]),
    Output = filename:join(OutDir, "GOOD-MIB.bin"),
    true = exists(Output),

    %% Try -W option.

    ok = file:delete(Output),
    run(Config, Cmd, FileName, "-W",
        ["_OK_"]),
    true = exists(Output),

    %% Try -W option and more verbose.

    ok = file:delete(Output),
    case os:type() of
        {unix,_} ->
            run(Config, Cmd, FileName, "-W +'{verbosity,info}'",
                ["\\[GOOD-MIB[.]mib\\]\\[INF\\]: No accessfunction for 'sysDescr' => using default",
                 "_OK_"]),
            true = exists(Output),
            ok = file:delete(Output);
        _ -> ok				%Don't bother -- too much work.
    end,

    %% Try a bad file.

    BadFile = filename:join(SrcDir, "BAD-MIB.mib"),
    run(Config, Cmd, BadFile, "",
        ["BAD-MIB.mib: 1: syntax error before: mibs\$",
         "compilation_failed_ERROR_"]),

    %% Make sure that no -I option works.

    NewCmd = erlc() ++ " -o" ++ OutDir ++ " ",
    run(Config, NewCmd, FileName, "", ["_OK_"]),
    true = exists(Output),

    ok.

%% Checks that 'erlc' doesn't eat any input (important when called from a
%% shell script with redirected input).
good_citizen(Config) when is_list(Config) ->
    case os:type() of
        {unix, _} ->
            PrivDir = proplists:get_value(priv_dir, Config),
            Answer = filename:join(PrivDir, "answer"),
            Script = filename:join(PrivDir, "test_script"),
            Test = filename:join(PrivDir, "test.erl"),
            S = ["#! /bin/sh\n", "erlc ", Test, "\n",
                 "read reply\n", "echo $reply\n"],
            ok = file:write_file(Script, S),
            ok = file:write_file(Test, "-module(test).\n"),
            Cmd = "echo y | sh " ++ Script ++ " > " ++ Answer,
            os:cmd(Cmd),
            {ok, Answer0} = file:read_file(Answer),
            [$y|_] = binary_to_list(Answer0),
            ok;
        _ ->
            {skip, "Unix specific"}
    end.

%% Make sure that compiling an Erlang module deep down in
%% in a directory with more than 255 characters works.
deep_cwd(Config) when is_list(Config) ->
    case os:type() of
        {unix, _} ->
            PrivDir = proplists:get_value(priv_dir, Config),
            deep_cwd_1(PrivDir);
        _ ->
            {skip, "Only a problem on Unix"}
    end.

deep_cwd_1(PrivDir) ->
    DeepDir0 = filename:join(PrivDir, lists:duplicate(128, $a)),
    DeepDir = filename:join(DeepDir0, lists:duplicate(128, $b)),
    ok = filelib:ensure_dir(filename:join(DeepDir,"any_file")),
    ok = file:set_cwd(DeepDir),
    ok = file:write_file("test.erl", "-module(test).\n\n"),
    io:format("~s\n", [os:cmd("erlc test.erl")]),
    true = filelib:is_file("test.beam"),
    ok.

%% Test that a large number of command line switches does not
%% overflow the argument buffer
arg_overflow(Config) when is_list(Config) ->
    {SrcDir, _OutDir, Cmd0} = get_cmd(Config),
    Cmd = Cmd0 ++ " +brief",
    FileName = filename:join(SrcDir, "erl_test_ok.erl"),
    %% Each -D option will be expanded to three arguments when
    %% invoking 'erl'.
    NumDOptions = num_d_options(),
    Args = lists:flatten([ ["-D", integer_to_list(N, 36), "=1 "] ||
                           N <- lists:seq(1, NumDOptions) ]),
    run(Config, Cmd, FileName, Args,
        ["Warning: function foo/0 is unused\$",
         "_OK_"]),
    ok.

num_d_options() ->
    case {os:type(),os:version()} of
        {{win32,_},_} ->
            %% The maximum size of a command line in the command
            %% shell on Windows is 8191 characters.
            %% Each -D option is expanded to "@dv NN 1", i.e.
            %% 8 characters. (Numbers up to 1295 can be expressed
            %% as two 36-base digits.)
            1000;
        {{unix,linux},Version} when Version < {2,6,23} ->
            %% On some older 64-bit versions of Linux, the maximum number
            %% of arguments is 16383.
            %% See: http://www.in-ulm.de/~mascheck/various/argmax/
            5440;
        {{unix,darwin},{Major,_,_}} when Major >= 11 ->
            %% "getconf ARG_MAX" still reports 262144 (as in previous
            %% version of MacOS X), but the useful space seem to have
            %% shrunk significantly (or possibly the number of arguments).
            %% 7673
            7500;
        {_,_} ->
            12000
    end.

erlc() ->
    case os:find_executable("erlc") of
        false ->
            ct:fail("Can't find erlc");
        Erlc ->
            "\"" ++ Erlc ++ "\""
    end.

make_dep_options(Config) ->
    {SrcDir,OutDir,Cmd0} = get_cmd(Config),
    Cmd = Cmd0 ++ " +brief",
    FileName = filename:join(SrcDir, "erl_test_ok.erl"),
    BeamFileName = filename:join(OutDir, "erl_test_ok.beam"),

    DepRE = ["/erl_test_ok[.]beam: \\\\$",
             "/erlc_SUITE_data/src/erl_test_ok[.]erl \\\\$",
             "/erlc_SUITE_data/include/erl_test[.]hrl$",
             "_OK_"],

    DepRETarget =
    ["^target: \\\\$",
     "/erlc_SUITE_data/src/erl_test_ok[.]erl \\\\$",
     "/erlc_SUITE_data/include/erl_test[.]hrl$",
     "_OK_"],

    DepREMP =
    ["/erl_test_ok[.]beam: \\\\$",
     "/erlc_SUITE_data/src/erl_test_ok[.]erl \\\\$",
     "/erlc_SUITE_data/include/erl_test[.]hrl$",
     [],
     "/erlc_SUITE_data/include/erl_test.hrl:$",
     "_OK_"],

    DepREMissing =
    ["/erl_test_missing_header[.]beam: \\\\$",
     "/erlc_SUITE_data/src/erl_test_missing_header[.]erl \\\\$",
     "/erlc_SUITE_data/include/erl_test[.]hrl \\\\$",
     "missing.hrl$",
     "_OK_"],

    file:delete(BeamFileName),

    %% Test plain -M
    run(Config, Cmd, FileName, "-M", DepRE),
    false = exists(BeamFileName),

    %% Test -MF File
    DepFile = filename:join(OutDir, "my.deps"),
    run(Config, Cmd, FileName, "-MF "++DepFile, ["_OK_"]),
    {ok,MFBin} = file:read_file(DepFile),
    verify_result(binary_to_list(MFBin)++["_OK_"], DepRE),
    false = exists(BeamFileName),

    %% Test -MD
    run(Config, Cmd, FileName, "-MD", ["_OK_"]),
    MDFile = filename:join(OutDir, "erl_test_ok.Pbeam"),
    {ok,MFBin} = file:read_file(MDFile),
    file:delete(MDFile), %% used further down!
    false = exists(BeamFileName),

    %% Test -M -MT Target
    run(Config, Cmd, FileName, "-M -MT target", DepRETarget),
    false = exists(BeamFileName),

    %% Test -MF File -MT Target
    TargetDepFile = filename:join(OutDir, "target.deps"),
    run(Config, Cmd, FileName, "-MF "++TargetDepFile++" -MT target",
        ["_OK_"]),
    {ok,TargetBin} = file:read_file(TargetDepFile),
    verify_result(binary_to_list(TargetBin)++["_OK_"], DepRETarget),
    file:delete(TargetDepFile),
    false = exists(BeamFileName),

    %% Test -MD -MT Target
    run(Config, Cmd, FileName, "-MD -MT target", ["_OK_"]),
    TargetMDFile = filename:join(OutDir, "erl_test_ok.Pbeam"),
    {ok,TargetBin} = file:read_file(TargetMDFile),
    file:delete(TargetDepFile),
    false = exists(BeamFileName),

    %% Test -M -MQ Target. (Note: Passing a $ on the command line
    %% portably for Unix and Windows is tricky, so we will just test
    %% that MQ works at all.)
    run(Config, Cmd, FileName, "-M -MQ target", DepRETarget),
    false = exists(BeamFileName),

    %% Test -M -MP
    run(Config, Cmd, FileName, "-M -MP", DepREMP),
    false = exists(BeamFileName),

    %% Test -M -MG
    MissingHeader = filename:join(SrcDir, "erl_test_missing_header.erl"),
    run(Config, Cmd, MissingHeader, "-M -MG", DepREMissing),
    false = exists(BeamFileName),

    %%
    %% check the above variants with side-effect -MMD
    %%

    %% since compiler is run on the erlang code a warning will be
    %% issued by the compiler, match that.
    WarningRE =
         "/erlc_SUITE_data/src/erl_test_ok.erl:[0-9]+:[0-9]+: "
        "Warning: function foo/0 is unused$",
    ErrorRE = "/erlc_SUITE_data/src/erl_test_missing_header.erl:"
        "[0-9]+:[0-9]+: can't find include file \"missing.hrl\"$",

    DepRE_MMD = insert_before("_OK_", WarningRE, DepRE),
    DepRETarget_MMD = insert_before("_OK_", WarningRE, DepRETarget),
    DepREMP_MMD = insert_before("_OK_",WarningRE,DepREMP),
    DepREMissing_MMD = (insert_before("_OK_",ErrorRE,DepREMissing)--
                            ["_OK_"]) ++ ["_ERROR_"],
    CompRE = [WarningRE,"_OK_"],


    %% Test plain -MMD -M
    run(Config, Cmd, FileName, "-MMD -M", DepRE_MMD),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -MF File
    DepFile = filename:join(OutDir, "my.deps"),
    run(Config, Cmd, FileName, "-MMD -MF "++DepFile, CompRE),
    {ok,MFBin} = file:read_file(DepFile),
    verify_result(binary_to_list(MFBin)++["_OK_"], DepRE),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -MD
    run(Config, Cmd, FileName, "-MMD -MD", CompRE),
    MDFile = filename:join(OutDir, "erl_test_ok.Pbeam"),
    {ok,MFBin} = file:read_file(MDFile),
    file:delete(MDFile), %% used further down!
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -M -MT Target
    run(Config, Cmd, FileName, "-MMD -M -MT target", DepRETarget_MMD),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -MF File -MT Target
    TargetDepFile = filename:join(OutDir, "target.deps"),
    run(Config, Cmd, FileName, "-MMD -MF "++TargetDepFile++" -MT target",
        CompRE),
    {ok,TargetBin} = file:read_file(TargetDepFile),
    verify_result(binary_to_list(TargetBin)++["_OK_"], DepRETarget),
    file:delete(TargetDepFile),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -MD -MT Target
    run(Config, Cmd, FileName, "-MMD -MD -MT target", CompRE),
    TargetMDFile = filename:join(OutDir, "erl_test_ok.Pbeam"),
    {ok,TargetBin} = file:read_file(TargetMDFile),
    file:delete(TargetDepFile),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -M -MQ Target. (Note: Passing a $ on the command line
    %% portably for Unix and Windows is tricky, so we will just test
    %% that MQ works at all.)
    run(Config, Cmd, FileName, "-MMD -M -MQ target", DepRETarget_MMD),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -M -MP
    run(Config, Cmd, FileName, "-MMD -M -MP", DepREMP_MMD),
    true = exists(BeamFileName),
    file:delete(BeamFileName),

    %% Test -MMD -M -MG
    MissingHeader = filename:join(SrcDir, "erl_test_missing_header.erl"),
    run(Config, Cmd, MissingHeader, "-MMD -M -MG", DepREMissing_MMD),
    false = exists(BeamFileName),
    ok.

unicode_paths(Config) ->
    case {os:type(), file:native_name_encoding()} of
        {{win32,_}, _} -> {skip, "Unicode paths not supported on windows"};
        {_,latin1} -> {skip, "Cannot interpret unicode filenames when native_name_encoding is latin1"};
        _ ->
            DepRE = ["_OK_"],
            {SrcDir,OutDir0,Cmd0} = get_cmd(Config),
            OutDir = filename:join(OutDir0,"😀"),
            ok = case file:make_dir(OutDir) of
                {error, eexist} -> ok;
                ok -> ok;
                E -> E
            end,
            Cmd = Cmd0 ++ " +brief -o "++OutDir,
            FileName = filename:join([SrcDir, "😀", "erl_test_unicode.erl"]),
            BeamFileName = filename:join(OutDir, "erl_test_unicode.beam"),
            run(Config, Cmd, FileName, "", DepRE),
            true = exists(BeamFileName),
            file:delete(BeamFileName),
            file:delete(OutDir),
            ok
    end.

%%% Tests related to the features mechanism
%% Support macros and functions
-define(OK(Lines), Lines ++ ["_OK_"]).
-define(NOTOK(Lines), Lines ++ ["_ERROR_"]).

flatfmt(FStr, Args) ->
    lists:flatten(io_lib:format(FStr, Args)).

defopt(Name) -> flatfmt("-D~w", [Name]).
defopt(Name, Value) -> flatfmt("-D~w=~w", [Name, Value]).

longopt(enable, Ftr) -> flatfmt("-enable-feature ~w", [Ftr]);
longopt(disable, Ftr) -> flatfmt("-disable-feature ~w", [Ftr]).

plusopt(enable, Ftr) -> flatfmt("+\"{feature, ~w, enable}\"", [Ftr]);
plusopt(disable, Ftr) -> flatfmt("+\"{feature, ~w, disable}\"", [Ftr]).

options(Opts) -> lists:flatten(lists:join(" ", Opts)).

peer(Args) ->
    {ok, Peer, Node} =
        ?CT_PEER(#{args => Args,
                   connection => 0}),
    {Peer, Node}.

%% Error messages to expect
nosingle(Ftr) -> flatfmt("the feature '~w' does not exist", [Ftr]).

nomultiple(Ftrs) ->
    Q = fun(A) -> flatfmt("'~w'", [A]) end,
    flatfmt("the features ~s and '~w' do not exist",
            [lists:join(", ",
                        lists:map(Q, lists:droplast(Ftrs))),
             lists:last(Ftrs)]).

not_config(Ftr) -> flatfmt("the feature '~w' is not configurable", [Ftr]).

syntax(Offender) -> flatfmt("syntax error before: ~w", [Offender]).

misplaced_directive() ->
    "feature directive not allowed after exports or record definitions".

atom_warning(Atom, Ftr) ->
    flatfmt("atom '~w' is reserved in the experimental feature '~w'",
            [Atom, Ftr]).

compile_fun(Config) ->
    {SrcDir, OutDir, Cmd} = get_cmd(Config),
    Compile = fun(FName, Opts, Expected) ->
                      Path = case FName of
                                 "" -> "";
                                 FName -> filename:join(SrcDir, FName)
                             end,
                      run(Config, Cmd, Path, Opts, Expected)
              end,
    {Compile, SrcDir, OutDir}.

%% Tests
features_erlc_describe(Config) when is_list(Config) ->
    {Compile, _, _} = compile_fun(Config),

    Compile("", "-list-features",
            ?NOTOK(["Available features:",
                    "approved_ftr_1",
                    "approved_ftr_2",
                    "experimental_ftr_1",
                    "experimental_ftr_2"])),

    Compile("", "-describe-feature experimental_ftr_1",
            ?NOTOK(["experimental_ftr_1",
                    "Type",
                    "Status",
                    "Keywords",
                    skip_lines])),

    Compile("", "-describe-feature loop_macro",
            ?NOTOK(["Unknown feature: loop_macro"])).

features_erlc_unknown(Config) when is_list(Config) ->
    {Compile, _, _} = compile_fun(Config),

    %% Test for single invalid feature, using long and +options
    Compile("nofile.erl", longopt(enable, no_ftr),
            ?NOTOK([nosingle(no_ftr)])),
    Compile("nofile.erl", plusopt(enable, no_ftr),
            ?NOTOK([nosingle(no_ftr)])),
    Compile("nofile.erl", longopt(disable, no_ftr),
            ?NOTOK([nosingle(no_ftr)])),
    Compile("nofile.erl", plusopt(disable, no_ftr),
            ?NOTOK([nosingle(no_ftr)])),

    %% Test for multiple invalid features
    Compile("nofile.erl", options([longopt(enable, no_ftr),
                                   longopt(enable, un_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr])])),
    Compile("nofile.erl", options([longopt(enable, no_ftr),
                                   longopt(enable, un_ftr),
                                   longopt(disable, mis_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr, mis_ftr])])),
    Compile("nofile.erl", options([plusopt(enable, no_ftr),
                                   plusopt(enable, un_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr])])),
    Compile("nofile.erl", options([plusopt(enable, no_ftr),
                                   plusopt(enable, un_ftr),
                                   plusopt(disable, mis_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr, mis_ftr])])),

    Compile("nofile.erl", options([longopt(enable, no_ftr),
                                   plusopt(enable, un_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr])])),
    Compile("nofile.erl", options([plusopt(enable, no_ftr),
                                   longopt(enable, un_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr])])),

    Compile("nofile.erl", options([longopt(enable, no_ftr),
                                   longopt(enable, un_ftr),
                                   plusopt(disable, mis_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr, mis_ftr])])),
    Compile("nofile.erl", options([longopt(enable, no_ftr),
                                   plusopt(enable, un_ftr),
                                   plusopt(disable, mis_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr, mis_ftr])])),

    Compile("nofile.erl", options([longopt(enable, no_ftr),
                                   plusopt(enable, un_ftr),
                                   longopt(disable, mis_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr, mis_ftr])])),
    Compile("nofile.erl", options([plusopt(enable, no_ftr),
                                   plusopt(enable, un_ftr),
                                   longopt(disable, mis_ftr)]),
            ?NOTOK([nomultiple([no_ftr, un_ftr, mis_ftr])])),

    %% Rejected and permanent features can not be configured.
    %% Rejected feature
    Compile("nofile.erl",
            options([longopt(enable, rejected_ftr)]),
            ?NOTOK([not_config(rejected_ftr),
                    skip_lines])),

    %% Permanent feature
    Compile("nofile.erl",
            options([longopt(enable, permanent_ftr)]),
            ?NOTOK([not_config(permanent_ftr),
                    skip_lines])).

features_directives(Config) when is_list(Config) ->
    {Compile, _, _} = compile_fun(Config),

    %% Test for misplaced feature directive (enable)
    Compile("f_directives.erl",
            options([defopt(misplaced_enable)]),
            ?NOTOK([misplaced_directive(),
                    "[0-9]+\| -feature.*misplaced",
                    skip_lines])),
    %% Test for misplaced feature directive (disable)
    Compile("f_directives.erl",
            options([defopt(misplaced_disable)]),
            ?NOTOK([misplaced_directive(),
                    "[0-9]+\| -feature.*misplaced",
                    skip_lines])),

    %% Test for unknown feature in directive (enable)
    Compile("f_directives.erl",
            options([defopt(enable_unknown)]),
            ?NOTOK([nosingle(unlesser),
                    "[0-9]+\| -feature",
                    skip_lines])),
    %% Test for unknown feature in directive (disable)
    Compile("f_directives.erl",
            options([defopt(disable_unknown)]),
            ?NOTOK([nosingle(unlesser),
                    "[0-9]+\| -feature",
                    skip_lines])),

    %% Enable a feature on the command line and disable the same in a
    %% directive
    Compile("f_directives.erl",
            options([longopt(enable, experimental_ftr_2),
                     defopt(disable_exp2),
                     defopt(no_dir_enable_exp2)]),
            ?OK([])).

features_atom_warnings(Config) when is_list(Config) ->
    {Compile, _, _} = compile_fun(Config),

    %% Check for keyword warnings.  Not all are checked.
    Compile("ignorant.erl", "+warn_keywords",
            ?OK([atom_warning(until, experimental_ftr_2),
                 skip_lines,
                 atom_warning(ifn, experimental_ftr_1),
                 skip_lines,
                 atom_warning(while, experimental_ftr_2),
                 skip_lines])),

    %% Check for keyword warnings, resulting in error.
    %% Not all warnings are checked.
    Compile("ignorant.erl", "+warn_keywords -Werror",
            ?NOTOK([skip_lines,
                    atom_warning(until, experimental_ftr_2),
                    skip_lines,
                    atom_warning(ifn, experimental_ftr_1),
                    skip_lines,
                    atom_warning(while, experimental_ftr_2),
                    skip_lines])),

    %% File has quoted atoms which are keywords in experimental_ftr_2.
    %% We should see no warnings.
    Compile("foo.erl", options([longopt(enable, experimental_ftr_2),
                                "+warn_keywords"]),
            ?OK([])).

features(Config) when is_list(Config) ->
    {Compile, _, _} = compile_fun(Config),

    %% Simple compile - baseline for this file.  Really needed?
    %% It uses atoms that are keywords in  experimental features.
    Compile("ignorant.erl",
            [],
            ?OK([])),

    %% Enable and disable the same feature on the command line to
    %% check that the ordering semantics is honoured.
    Compile("ignorant.erl",
            options([longopt(enable, experimental_ftr_1),
                     longopt(disable, experimental_ftr_1)]),
            ?OK([])),

    %% As above, with +options
    Compile("ignorant.erl",
            options([plusopt(enable, experimental_ftr_1),
                     plusopt(disable, experimental_ftr_1)]),
            ?OK([])),

    %% Mixed options
    Compile("ignorant.erl",
            options([longopt(enable, experimental_ftr_1),
                     plusopt(disable, experimental_ftr_1)]),
            ?OK([])),

    %% Mixed options
    Compile("ignorant.erl",
            options([plusopt(enable, experimental_ftr_1),
                     longopt(disable, experimental_ftr_1)]),
            ?OK([])),

    %% Enable a feature and see errors
    Compile("ignorant.erl",
            options([longopt(enable, experimental_ftr_1)]),
            ?NOTOK([syntax(ifn),
                    skip_lines,
                    syntax(ifn),
                    skip_lines])),

    %% Enable another feature and see other errors
    Compile("ignorant.erl",
            options([longopt(enable, experimental_ftr_2)]),
            ?NOTOK([syntax(until),
                    skip_lines,
                    syntax(until),
                    skip_lines,
                    syntax(while),
                    skip_lines,
                    syntax(while),
                    skip_lines,
                    syntax(until),
                    skip_lines])),

    %% Enable both features and see even more errors
    Compile("ignorant.erl",
            options([longopt(enable, experimental_ftr_1),
                     longopt(enable, experimental_ftr_2)]),
            ?NOTOK([syntax(until),
                    skip_lines,
                    syntax(until),
                    skip_lines,
                    syntax(ifn),
                    skip_lines,
                    syntax(while),
                    skip_lines,
                    syntax(until),
                    skip_lines,
                    syntax(ifn),
                    skip_lines])),

    ok.

features_macros(Config) when is_list(Config) ->
    {Compile, _, OutDir} = compile_fun(Config),

    Compile("f_macros.erl", "", ?OK([])),

    {Peer0, Node0} = peer(["-pa", OutDir]),
    Call = fun(Node, Fun) ->
                   erpc:call(Node, f_macros, Fun, [])
           end,

    {module, f_macros} =
        erpc:call(Node0, code, load_file, [f_macros]),

    true = Call(Node0, has_experimental),
    false = Call(Node0, has_hindley_milner),
    false = Call(Node0, with_hm),
    false = Call(Node0, uses_experimental),
    false = Call(Node0, uses_exp2),

    peer:stop(Peer0),

    Compile("f_macros.erl", longopt(enable, experimental_ftr_1),
            ?OK([])),

    {Peer1, Node1} = peer(["-pa", OutDir]),

    true = erpc:call(Node1, erlang, module_loaded, [erl_features]),

    %% Starting from OTP 26, compile-time features don't need to be
    %% enabled in the runtime system.
    {module, f_macros} =
        erpc:call(Node1, code, load_file, [f_macros]),
    %% Check features enabled during compilation
    [approved_ftr_1, approved_ftr_2, experimental_ftr_1] =
        erpc:call(Node1, erl_features, used, [f_macros]),

    peer:stop(Peer1),

    %% Restart with feature enabled in runtime
    {Peer2, Node2} = peer(["-pa", OutDir,
                           "-enable-feature","experimental_ftr_1"]),
    %% Now we can load it
    {module, f_macros} =
        erpc:call(Node2, code, load_file, [f_macros]),

    true = Call(Node2, uses_experimental),
    false = Call(Node2, uses_exp2),
    peer:stop(Peer2),

    Compile("f_macros.erl", longopt(enable, experimental_ftr_2),
            ?OK([])),
    {Peer3, Node3} = peer(["-pa", OutDir,
                           "-enable-feature","experimental_ftr_2"]),
    false = Call(Node3, uses_experimental),
    true = Call(Node3, uses_exp2),
    peer:stop(Peer3),

    Compile("f_macros.erl", options([longopt(enable, experimental_ftr_1),
                                     longopt(enable, experimental_ftr_2)]),
            ?OK([])),
    {Peer4, Node4} = peer(["-pa", OutDir,
                           "-enable-feature","experimental_ftr_1",
                           "-enable-feature","experimental_ftr_2"]),
    true = Call(Node4, uses_experimental),
    true = Call(Node4, uses_exp2),
    peer:stop(Peer4),

    ok.

features_disable(Config) when is_list(Config) ->
    {Compile, _, OutDir} = compile_fun(Config),

    Call = fun(Node, Fun) ->
                   erpc:call(Node, f_disable, Fun, [])
           end,

    Compile("f_disable.erl", options([longopt(enable, experimental_ftr_1),
                                      longopt(enable, experimental_ftr_2)]),
            ?OK([])),

    {Peer, Node} = peer(["-pa", OutDir,
                         "-enable-feature","experimental_ftr_1",
                         "-enable-feature","experimental_ftr_2"]),
    %% Check features enabled during compilation
    [approved_ftr_2] =
        erpc:call(Node, erl_features, used, [f_disable]),

    no_experimental = Call(Node, no_experimental),
    no_exp2 = Call(Node, no_ftrs),
    peer:stop(Peer),

    ok.

features_all(Config) when is_list(Config) ->
    {Compile, _, OutDir} = compile_fun(Config),

    Compile("foo.erl", longopt(enable, all),
            ?OK([])),

    {Peer0, Node0} = peer(["-pa", OutDir]),
    %% Check features enabled during compilation
    [approved_ftr_1,approved_ftr_2,experimental_ftr_1,experimental_ftr_2] =
        erpc:call(Node0, erl_features, used, [foo]),
    peer:stop(Peer0),

    Compile("foo.erl", longopt(disable, all),
            ?OK([])),

    {Peer1, Node1} = peer(["-pa", OutDir]),
    %% Check features enabled during compilation
    [] = erpc:call(Node1, erl_features, used, [foo]),
    {module, foo} = erpc:call(Node1, code, load_file, [foo]),
    peer:stop(Peer1),

    Compile("foo.erl", options([longopt(disable, all),
                                longopt(enable, approved_ftr_2)]),
            ?OK([])),

    {Peer2, Node2} = peer(["-pa", OutDir,
                           "-disable-feature", "all"]),
    %% Check features enabled during compilation
    [approved_ftr_2] = erpc:call(Node2, erl_features, used, [foo]),
    peer:stop(Peer2),

    ok.

features_load(Config) when is_list(Config) ->
    {_Compile, SrcDir, _OutDir} = compile_fun(Config),

    %% Note that we put SrcDir in the load path as there is where we
    %% have the precompiled beam file.
    {Peer0, Node0} = peer(["-pa", SrcDir]),

    %% For a file compiled with an older version, i.e., with no Meta
    %% chunk, we should see no used features.
    [] = erpc:call(Node0, erl_features, used, [older]),
    %% .. and we should be able to load it.
    {module,older} = erpc:call(Node0, code, load_file, [older]),

    [] = erpc:call(Node0, erl_features, used,
                   [filename:join(SrcDir, "older.beam")]),

    %% Behaviour for non existent modules
    not_found = erpc:call(Node0, erl_features, used, [none]),
    not_found = erpc:call(Node0, erl_features, used, ["none.beam"]),
    peer:stop(Peer0),

    ok.

features_runtime(Config) when is_list(Config) ->
    AllFtrs = [approved_ftr_1,
               approved_ftr_2,
               experimental_ftr_1,
               experimental_ftr_2,
               permanent_ftr,
               rejected_ftr],
    ConfigFtrs = [approved_ftr_1,
                  approved_ftr_2,
                  experimental_ftr_1,
                  experimental_ftr_2],
    Approved = [approved_ftr_2,
                approved_ftr_1],

    {_Compile, _SrcDir, _OutDir} = compile_fun(Config),

    {Peer0, Node0} = peer([]),

    %% Get all known features
    AllFtrs = erpc:call(Node0, erl_features, all, []),
    ConfigFtrs = erpc:call(Node0, erl_features, configurable, []),
    Approved =  erpc:call(Node0, erl_features, enabled, []),

    %% Keywords from enabled (here the approved) features
    %% (does not need to be quoted since it comes from the peer node)
    [unless] =  erpc:call(Node0, erl_features, keywords, []),

    Info = erpc:call(Node0, erl_features, info, [permanent_ftr]),
    true = is_map(Info),
    true = lists:all(fun(K) -> is_map_key(K, Info) end,
                     [status,type,description,short,experimental]),

    %% Try to get info for unknown feature - raises error
    try
        erpc:call(Node0, erl_features, info, [unknown_feature]) of
        Value ->
            ct:fail({value_returned_for_unknown_feature, Value})
    catch
        error:{exception, invalid_feature, _} ->
            ok;
        Class:Reason ->
            ct:fail({unexpected_exception, {Class, Reason}})
    end,

    peer:stop(Peer0),

    {Peer1, Node1} = peer(["-enable-feature", "experimental_ftr_2"]),
    [experimental_ftr_2, approved_ftr_2, approved_ftr_1] =
        erpc:call(Node1, erl_features, enabled, []),
    [while, until, unless] =  erpc:call(Node1, erl_features, keywords, []),

    peer:stop(Peer1),

    {Peer2, Node2} = peer(["-disable-feature", "all"]),
    [] = erpc:call(Node2, erl_features, enabled, []),
    [] =  erpc:call(Node2, erl_features, keywords, []),

    peer:stop(Peer2),
    ok.

features_include(Config) when is_list(Config) ->
    {Compile, _SrcDir, OutDir} = compile_fun(Config),

    %% Ensure that the feature experimental_ftr_1 is enabled in the
    %% include file and generates an error
    Compile("f_include_1.erl", [],
            ?NOTOK([syntax(ifn),
                    skip_lines])),

    %% This will disable a feature after a record has been defined.
    %% Error expected
    Compile("f_include_1.erl", defopt(end_include),
            ?NOTOK([misplaced_directive(),
                    skip_lines])),

    %% Ensure that the macro knows that experimental_ftr_1 is enabled
    %% in the include file.
    Compile("f_include_2.erl", defopt(end_include),
            ?OK([])),

    {Peer0, Node0} = peer(["-pa", OutDir,
                           "-enable-feature", "experimental_ftr_1"]),

    {module, f_include_2} = erpc:call(Node0, code, load_file, [f_include_2]),
    active = erpc:call(Node0, f_include_2, foo, [2]),
    peer:stop(Peer0),

    Compile("f_include_3.erl", [],
            ?OK([])),

    {Peer1, Node1} = peer(["-pa", OutDir,
                           "-enable-feature", "all"]),
    exp2_enabled = erpc:call(Node1, f_include_3, foo, [1]),
    peer:stop(Peer1),

    Compile("f_include_3.erl", defopt(end_prefix),
            ?NOTOK([misplaced_directive(),
                    "experimental_ftr_2",
                    skip_lines])),

    Compile("f_include_exp2.erl", defopt(enable_exp_2, 0),
            ?OK([])),
    {Peer2, Node2} = peer(["-pa", OutDir]),
    {conditional, on, until} = erpc:call(Node2, f_include_exp2, foo, []),
    peer:stop(Peer2),

    Compile("f_include_exp2.erl",
            options([defopt(enable_exp_2, 0),
                     longopt(enable, experimental_ftr_2)]),
            ?NOTOK([syntax(until),
                    skip_lines])),

    Compile("f_include_exp2.erl",
            options([defopt(enable_exp_2, 1)]),
            ?NOTOK([syntax(until),
                    skip_lines])),

    Compile("f_include_exp2.erl",
            options([defopt(enable_exp_2, 2),
                     longopt(enable, experimental_ftr_2)]),
            ?OK([])),

    {Peer3, Node3} = peer(["-pa", OutDir,
                           "-enable-feature", "experimental_ftr_2"]),
    {conditional, off, none} = erpc:call(Node3, f_include_exp2, foo, []),

    [approved_ftr_1, approved_ftr_2, experimental_ftr_2] =
        erpc:call(Node3, erl_features, used, [f_include_exp2]),
    peer:stop(Peer3),

    ok.

%% Runs a command.

run(Config, Cmd0, Name, Options, Expect) ->
    Cmd = Cmd0 ++ " " ++ Options ++ " " ++ Name,
    io:format("~ts", [Cmd]),
    Result = run_command(Config, Cmd),
    verify_result(Result, Expect).

verify_result(Result, Expect) ->
    Messages = split(Result, [], []),
    io:format("Result: ~p", [Messages]),
    io:format("Expected: ~p", [Expect]),
    match_messages_x(Messages, Expect).

%% insert What before Item, crash if Item is not found
insert_before(Item, What, [Item|List]) ->
    [What,Item|List];
insert_before(Item, What, [Other|List]) ->
    [Other|insert_before(Item, What, List)].

split([$\n|Rest], Current, Lines) ->
    split(Rest, [], [lists:reverse(Current)|Lines]);
split([$\r|Rest], Current, Lines) ->
    split(Rest, Current, Lines);
split([Char|Rest], Current, Lines) ->
    split(Rest, [Char|Current], Lines);
split([], [], Lines) ->
    lists:reverse(Lines);
split([], Current, Lines) ->
    split([], [], [lists:reverse(Current)|Lines]).

match_messages_x(Msgs0, Regexps0) ->
    Msgs = lists:droplast(Msgs0),
    Regexps = lists:droplast(Regexps0),
    Return = lists:last(Msgs0),
    ExpRet = lists:last(Regexps0),
    match_messages(Msgs, Regexps),
    match_one(Return, ExpRet).

match_messages(_, [skip_lines]) ->
    ok;
match_messages([_Msg|Rest1], [skip_one|Rest2]) ->
    match_messages(Rest1, Rest2);
match_messages([Msg|Rest1], [skip_lines, Regexp|Rest2]) ->
    case match(Msg, Regexp) of
        match ->
            match_messages(Rest1, Rest2);
        nomatch ->
            match_messages(Rest1, [skip_lines, Regexp|Rest2])
    end;
match_messages([Msg|Rest1], [Regexp|Rest2]) ->
    match_one(Msg, Regexp),
    match_messages(Rest1, Rest2);
match_messages([], [Expect|Rest]) ->
    ct:fail({too_few_messages, [Expect|Rest]});
match_messages([Msg|Rest], []) ->
    ct:fail({too_many_messages, [Msg|Rest]});
match_messages([], []) ->
    ok.

match_one(Msg, Regexp) ->
    case match(Msg, Regexp) of
        match ->
            ok;
        nomatch ->
            io:format("Not matching: ~s\n", [Msg]),
            io:format("Regexp      : ~s\n", [Regexp]),
            ct:fail(message_mismatch)
    end.

match(Msg, Regexp) ->
    re:run(Msg, Regexp, [{capture,none}, unicode]).

get_cmd(Cfg) ->
    {SrcDir, IncDir, OutDir} = get_dirs(Cfg),
    Cmd = erlc() ++ " -I" ++ IncDir ++ " -o" ++ OutDir ++ " ",
    {SrcDir, OutDir, Cmd}.

get_dirs(Cfg) ->
    DataDir = proplists:get_value(data_dir, Cfg),
    PrivDir = proplists:get_value(priv_dir, Cfg),
    SrcDir = filename:join(DataDir, "src"),
    IncDir = filename:join(DataDir, "include"),
    {SrcDir, IncDir, PrivDir}.

exists(Name) ->
    filelib:is_file(Name).

%% Runs the command using os:cmd/1.
%%
%% Returns the output from the command (as a list of characters with
%% embedded newlines).  The very last line will indicate the
%% exit status of the command, where _OK_ means zero, and _ERROR_
%% a non-zero exit status.

run_command(Config, Cmd) ->
    TmpDir = filename:join(proplists:get_value(priv_dir, Config), "tmp"),
    file:make_dir(TmpDir),
    {RunFile, Run, Script} = run_command(TmpDir, os:type(), Cmd),
    ok = file:write_file(RunFile, unicode:characters_to_binary(Script)),
    os:cmd(Run).

run_command(Dir, {win32, _}, Cmd) ->
    BatchFile = filename:join(Dir, "run.bat"),
    Run = re:replace(filename:rootname(BatchFile), "/", "\\",
                     [global,{return,list}]),
    {BatchFile,
     Run,
     ["@echo off\r\n",
      "set ERLC_EMULATOR=", ct:get_progname(), "\r\n",
      Cmd, "\r\n",
      "if errorlevel 1 echo _ERROR_\r\n",
      "if not errorlevel 1 echo _OK_\r\n"]};
run_command(Dir, {unix, _}, Cmd) ->
    Name = filename:join(Dir, "run"),
    {Name,
     "/bin/sh " ++ Name,
     ["#!/bin/sh\n",
      "ERLC_EMULATOR='", ct:get_progname(), "'\n",
      "export ERLC_EMULATOR\n",
      Cmd, "\n",
      "case $? in\n",
      "  0) echo '_OK_';;\n",
      "  *) echo '_ERROR_';;\n",
      "esac\n"]};
run_command(_Dir, Other, _Cmd) ->
    ct:fail("Don't know how to test exit code for ~p", [Other]).
