%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(erlc_SUITE).

%% Tests the erlc command by compiling various types of files.

-export([all/1, compile_erl/1, compile_yecc/1, compile_script/1,
	 compile_mib/1, good_citizen/1, deep_cwd/1]).

-include_lib("test_server/include/test_server.hrl").

all(suite) ->
    [compile_erl, compile_yecc, compile_script, compile_mib,
     good_citizen, deep_cwd].

%% Copy from erlc_SUITE_data/include/erl_test.hrl.

-record(person, {name, shoe_size}).

%% Tests that compiling Erlang source code works.

compile_erl(Config) when is_list(Config) ->
    ?line {SrcDir, OutDir, Cmd} = get_cmd(Config),
    ?line FileName = filename:join(SrcDir, "erl_test_ok.erl"),

    %% By default, warnings are now turned on.
    ?line run(Config, Cmd, FileName, "",
	      ["Warning: function foo/0 is unused\$",
	       "_OK_"]),

    %% Test that the compiled file is where it should be,
    %% and that it is runnable.

    ?line {module, erl_test_ok} = code:load_abs(filename:join(OutDir,
							      "erl_test_ok")),
    ?line 42 = erl_test_ok:shoe_size(#person{shoe_size=42}),
    ?line code:purge(erl_test_ok),

    %% Try disabling warnings.

    ?line run(Config, Cmd, FileName, "-W0", ["_OK_"]),

    %% Try treating warnings as errors.

    ?line run(Config, Cmd, FileName, "-Werror",
	      ["compile: warnings being treated as errors\$",
	       "Warning: function foo/0 is unused\$",
	       "_ERROR_"]),

    %% Check a bad file.

    ?line BadFile = filename:join(SrcDir, "erl_test_bad.erl"),
    ?line run(Config, Cmd, BadFile, "", ["function non_existing/1 undefined\$",
				 "_ERROR_"]),

    ok.

%% Test that compiling yecc source code works.

compile_yecc(Config) when is_list(Config) ->
    ?line {SrcDir, _, OutDir} = get_dirs(Config),
    ?line Cmd = erlc() ++ " -o" ++ OutDir ++ " ",
    ?line FileName = filename:join(SrcDir, "yecc_test_ok.yrl"),
    ?line run(Config, Cmd, FileName, "-W0", ["_OK_"]),
    ?line true = exists(filename:join(OutDir, "yecc_test_ok.erl")),

    ?line BadFile = filename:join(SrcDir, "yecc_test_bad.yrl"),
    ?line run(Config, Cmd, BadFile, "-W0", 
	      ["rootsymbol form is not a nonterminal\$",
               "undefined nonterminal: form\$",
               "Nonterminals is missing\$",
               "_ERROR_"]),
    ?line exists(filename:join(OutDir, "yecc_test_ok.erl")),

    ok.

%% Test that compiling start scripts works.

compile_script(Config) when is_list(Config) ->
    ?line {SrcDir, OutDir, Cmd} = get_cmd(Config),
    ?line FileName = filename:join(SrcDir, "start_ok.script"),
    ?line run(Config, Cmd, FileName, "", ["_OK_"]),
    ?line true = exists(filename:join(OutDir, "start_ok.boot")),

    ?line BadFile = filename:join(SrcDir, "start_bad.script"),
    ?line run(Config, Cmd, BadFile, "", ["syntax error before:", "_ERROR_"]),
    ok.

%% Test that compiling SNMP mibs works.

compile_mib(Config) when is_list(Config) ->
    ?line {SrcDir, OutDir, Cmd} = get_cmd(Config),
    ?line FileName = filename:join(SrcDir, "GOOD-MIB.mib"),
    ?line run(Config, Cmd, FileName, "", ["_OK_"]),
    ?line Output = filename:join(OutDir, "GOOD-MIB.bin"),
    ?line true = exists(Output),

    %% Try -W option.

    ?line ok = file:delete(Output),
    ?line run(Config, Cmd, FileName, "-W",
	      ["_OK_"]),
    ?line true = exists(Output),

    %% Try -W option and more verbose.

    ?line ok = file:delete(Output),
    ?line case test_server:os_type() of
	      {unix,_} ->
		  ?line run(Config, Cmd, FileName, "-W +'{verbosity,info}'",
			    ["\\[GOOD-MIB[.]mib\\]\\[INF\\]: No accessfunction for 'sysDescr' => using default",
			     "_OK_"]),
		  ?line true = exists(Output),
		  ?line ok = file:delete(Output);
	      _ -> ok				%Don't bother -- too much work.
	  end,

    %% Try a bad file.

    ?line BadFile = filename:join(SrcDir, "BAD-MIB.mib"),
    ?line run(Config, Cmd, BadFile, "",
	      ["Error: syntax error before: mibs\$", "compilation_failed_ERROR_"]),

    %% Make sure that no -I option works.

    ?line NewCmd = erlc() ++ " -o" ++ OutDir ++ " ",
    ?line run(Config, NewCmd, FileName, "", ["_OK_"]),
    ?line true = exists(Output),

    ok.

%% Checks that 'erlc' doesn't eat any input (important when called from a
%% shell script with redirected input).
good_citizen(Config) when is_list(Config) ->
    case os:type() of
	{unix, _} ->
	    ?line PrivDir = ?config(priv_dir, Config),
	    ?line Answer = filename:join(PrivDir, "answer"),
	    ?line Script = filename:join(PrivDir, "test_script"),
	    ?line Test = filename:join(PrivDir, "test.erl"),
	    ?line S = ["#! /bin/sh\n", "erlc ", Test, "\n",
		       "read reply\n", "echo $reply\n"],
	    ?line ok = file:write_file(Script, S),
	    ?line ok = file:write_file(Test, "-module(test).\n"),
	    ?line Cmd = "echo y | sh " ++ Script ++ " > " ++ Answer,
	    ?line os:cmd(Cmd),
	    ?line {ok, Answer0} = file:read_file(Answer),
	    ?line [$y|_] = binary_to_list(Answer0),
	    ok;
	_ ->
	    {skip, "Unix specific"}
    end.

%% Make sure that compiling an Erlang module deep down in
%% in a directory with more than 255 characters works.
deep_cwd(Config) when is_list(Config) ->
    case os:type() of
	{unix, _} ->
	    PrivDir = ?config(priv_dir, Config),
	    deep_cwd_1(PrivDir);
	_ ->
	    {skip, "Only a problem on Unix"}
    end.

deep_cwd_1(PrivDir) ->
    ?line DeepDir0 = filename:join(PrivDir, lists:duplicate(128, $a)),
    ?line DeepDir = filename:join(DeepDir0, lists:duplicate(128, $b)),
    ?line ok = file:make_dir(DeepDir0),
    ?line ok = file:make_dir(DeepDir),
    ?line ok = file:set_cwd(DeepDir),
    ?line ok = file:write_file("test.erl", "-module(test).\n\n"),
    ?line io:format("~s\n", [os:cmd("erlc test.erl")]),
    ?line true = filelib:is_file("test.beam"),
    ok.

erlc() ->
    case os:find_executable("erlc") of
	false ->
	    test_server:fail("Can't find erlc");
	Erlc ->
	    Erlc
    end.
	    
%% Runs a command.

run(Config, Cmd0, Name, Options, Expect) ->
    Cmd = Cmd0 ++ " " ++ Options ++ " " ++ Name,
    io:format("~s", [Cmd]),
    Result = run_command(Config, Cmd),
    Messages = split(Result, [], []),
    io:format("Result: ~p", [Messages]),
    io:format("Expected: ~p", [Expect]),
    match_messages(Messages, Expect).

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
    case re:run(Msg, Regexp, [{capture,none}]) of
	match ->
	    ok;
	nomatch ->
	    io:format("Not matching: ~s\n", [Msg]),
	    io:format("Regexp      : ~s\n", [Regexp]),
	    test_server:fail(message_mismatch)
    end,
    match_messages(Rest1, Rest2);
match_messages([], [Expect|Rest]) ->
    test_server:fail({too_few_messages, [Expect|Rest]});
match_messages([Msg|Rest], []) ->
    test_server:fail({too_many_messages, [Msg|Rest]});
match_messages([], []) ->
    ok.

get_cmd(Cfg) ->
    ?line {SrcDir, IncDir, OutDir} = get_dirs(Cfg),
    ?line Cmd = erlc() ++ " -I" ++ IncDir ++ " -o" ++ OutDir ++ " ",
    {SrcDir, OutDir, Cmd}.

get_dirs(Cfg) ->
    ?line DataDir = ?config(data_dir, Cfg),
    ?line PrivDir = ?config(priv_dir, Cfg),
    ?line SrcDir = filename:join(DataDir, "src"),
    ?line IncDir = filename:join(DataDir, "include"),
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
    TmpDir = filename:join(?config(priv_dir, Config), "tmp"),
    file:make_dir(TmpDir),
    {RunFile, Run, Script} = run_command(TmpDir, os:type(), Cmd),
    ok = file:write_file(filename:join(TmpDir, RunFile), Script),
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
    M = io_lib:format("Don't know how to test exit code for ~p", [Other]),
    test_server:fail(lists:flatten(M)).
