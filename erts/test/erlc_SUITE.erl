%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
         init_per_group/2,end_per_group/2, compile_erl/1,
         compile_yecc/1, compile_script/1,
         compile_mib/1, good_citizen/1, deep_cwd/1, arg_overflow/1,
         make_dep_options/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [compile_erl, compile_yecc, compile_script, compile_mib,
     good_citizen, deep_cwd, arg_overflow, make_dep_options].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Copy from erlc_SUITE_data/include/erl_test.hrl.

-record(person, {name, shoe_size}).

%% Tests that compiling Erlang source code works.

compile_erl(Config) when is_list(Config) ->
    {SrcDir, OutDir, Cmd} = get_cmd(Config),
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
        ["rootsymbol form is not a nonterminal\$",
         "undefined nonterminal: form\$",
         "Nonterminals is missing\$",
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
    case test_server:os_type() of
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
    ok = file:make_dir(DeepDir0),
    ok = file:make_dir(DeepDir),
    ok = file:set_cwd(DeepDir),
    ok = file:write_file("test.erl", "-module(test).\n\n"),
    io:format("~s\n", [os:cmd("erlc test.erl")]),
    true = filelib:is_file("test.beam"),
    ok.

%% Test that a large number of command line switches does not
%% overflow the argument buffer
arg_overflow(Config) when is_list(Config) ->
    {SrcDir, _OutDir, Cmd} = get_cmd(Config),
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
    {SrcDir,OutDir,Cmd} = get_cmd(Config),
    FileName = filename:join(SrcDir, "erl_test_ok.erl"),
    BeamFileName = filename:join(OutDir, "erl_test_ok.beam"),

    DepRE = ["/erl_test_ok[.]beam: \\\\$",
             "/system_test/erlc_SUITE_data/src/erl_test_ok[.]erl \\\\$",
             "/system_test/erlc_SUITE_data/include/erl_test[.]hrl$",
             "_OK_"],

    DepRETarget =
    ["^target: \\\\$",
     "/system_test/erlc_SUITE_data/src/erl_test_ok[.]erl \\\\$",
     "/system_test/erlc_SUITE_data/include/erl_test[.]hrl$",
     "_OK_"],

    DepREMP =
    ["/erl_test_ok[.]beam: \\\\$",
     "/system_test/erlc_SUITE_data/src/erl_test_ok[.]erl \\\\$",
     "/system_test/erlc_SUITE_data/include/erl_test[.]hrl$",
     [],
     "/system_test/erlc_SUITE_data/include/erl_test.hrl:$",
     "_OK_"],

    DepREMissing =
    ["/erl_test_missing_header[.]beam: \\\\$",
     "/system_test/erlc_SUITE_data/src/erl_test_missing_header[.]erl \\\\$",
     "/system_test/erlc_SUITE_data/include/erl_test[.]hrl \\\\$",
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
    WarningRE = "/system_test/erlc_SUITE_data/src/erl_test_ok.erl:[0-9]+: "
        "Warning: function foo/0 is unused$",
    ErrorRE = "/system_test/erlc_SUITE_data/src/erl_test_missing_header.erl:"
        "[0-9]+: can't find include file \"missing.hrl\"$",

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
    match_messages(Messages, Expect).

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

match_messages([Msg|Rest1], [Regexp|Rest2]) ->
    case re:run(Msg, Regexp, [{capture,none}, unicode]) of
        match ->
            ok;
        nomatch ->
            io:format("Not matching: ~s\n", [Msg]),
            io:format("Regexp      : ~s\n", [Regexp]),
            ct:fail(message_mismatch)
    end,
    match_messages(Rest1, Rest2);
match_messages([], [Expect|Rest]) ->
    ct:fail({too_few_messages, [Expect|Rest]});
match_messages([Msg|Rest], []) ->
    ct:fail({too_many_messages, [Msg|Rest]});
match_messages([], []) ->
    ok.

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
    ok = file:write_file(filename:join(TmpDir, RunFile), unicode:characters_to_binary(Script)),
    os:cmd(Run).

run_command(Dir, {win32, _}, Cmd) ->
    BatchFile = filename:join(Dir, "run.bat"),
    Run = re:replace(filename:rootname(BatchFile), "/", "\\",
                     [global,{return,list}]),
    {BatchFile,
     Run,
     ["@echo off\r\n",
      "set ERLC_EMULATOR=", atom_to_list(lib:progname()), "\r\n",
      Cmd, "\r\n",
      "if errorlevel 1 echo _ERROR_\r\n",
      "if not errorlevel 1 echo _OK_\r\n"]};
run_command(Dir, {unix, _}, Cmd) ->
    Name = filename:join(Dir, "run"),
    {Name,
     "/bin/sh " ++ Name,
     ["#!/bin/sh\n",
      "ERLC_EMULATOR='", atom_to_list(lib:progname()), "'\n",
      "export ERLC_EMULATOR\n",
      Cmd, "\n",
      "case $? in\n",
      "  0) echo '_OK_';;\n",
      "  *) echo '_ERROR_';;\n",
      "esac\n"]};
run_command(_Dir, Other, _Cmd) ->
    ct:fail("Don't know how to test exit code for ~p", [Other]).
