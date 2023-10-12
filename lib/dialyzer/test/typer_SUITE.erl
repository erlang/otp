%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2023. All Rights Reserved.
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
-module(typer_SUITE).

-export([all/0,suite/0,
         smoke/1,
         smoke_incremental_plt/1,
         gh_6296_no_spec_flag_does_not_break_records/1,
         contract_violation/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [smoke,
     smoke_incremental_plt,
     gh_6296_no_spec_flag_does_not_break_records,
     contract_violation].

smoke(Config) ->
    OutDir = proplists:get_value(priv_dir, Config),
    case dialyzer_common:check_plt(OutDir) of
        fail -> {skip, "Plt creation/check failed."};
        ok ->
          Code = <<"-module(typer_test_module).
                   -compile([export_all,nowarn_export_all]).
                   a(L) ->
                     L ++ [1,2,3].">>,
          PrivDir = proplists:get_value(priv_dir, Config),
          Src = filename:join(PrivDir, "typer_test_module.erl"),
          ok = file:write_file(Src, Code),
          Args = "--plt " ++ PrivDir ++ "dialyzer_plt",
          Res = ["^$",
                 "^%% File:",
                 "^%% ----",
                 "^-spec a",
                 "^_OK_"],
          run(Config, Args, Src, Res),
          ok
    end.

gh_6296_no_spec_flag_does_not_break_records(Config) ->
    Code = <<"-module(gh_6296).
              -export([record_pattern/1]).

              -record(my_rec, {is_foo :: boolean(),
                               bar :: non_neg_integer()}).

              -spec record_pattern(#my_rec{}) -> atom().
              record_pattern(#my_rec{is_foo = IsFoo}) ->
                IsFoo.

              some_other_function_to_trigger_the_issue(undefined) -> ok.">>,
    PrivDir = proplists:get_value(priv_dir, Config),
    Src = filename:join(PrivDir, "gh_6296.erl"),
    ok = file:write_file(Src, Code),
    {ok, Beam} = compile(Config, Code, gh_6296, []),
    Plt = PrivDir ++ "dialyzer_iplt",
    _ = dialyzer:run([{analysis_type, incremental},
                      {files, [Beam]},
                      {apps, [stdlib, kernel, erts]},
                      {from, byte_code},
                      {init_plt, Plt},
                      {output_plt, Plt}]),
    Args = io_lib:format("--no_spec --show --plt ~ts", [Plt]),
    Res = ["^$",
           "^%% File:",
           "^%% ----",
           "^-spec record_pattern",
           "^-spec some_other_function_to_trigger_the_issue",
           "^_OK_"],
    run(Config, Args, Src, Res),
    ok.

smoke_incremental_plt(Config) ->
    Code = <<"-module(typer_test_module).
             -compile([export_all,nowarn_export_all]).
             a(L) ->
               L ++ [1,2,3].">>,
    PrivDir = proplists:get_value(priv_dir, Config),
    Src = filename:join(PrivDir, "typer_test_module.erl"),
    ok = file:write_file(Src, Code),
    {ok, Beam} = compile(Config, Code, typer_test_module, []),
    Plt = PrivDir ++ "dialyzer_iplt",
    _ = dialyzer:run([{analysis_type, incremental},
                      {files, [Beam]},
                      {apps, [stdlib, kernel, erts]},
                      {from, byte_code},
                      {init_plt, Plt},
                      {output_plt, Plt}]),
    Args = "--plt " ++ Plt,
    Res = ["^$",
           "^%% File:",
           "^%% ----",
           "^-spec a",
           "^_OK_"],
    run(Config, Args, Src, Res),
    ok.

contract_violation(Config) ->
    OutDir = proplists:get_value(priv_dir, Config),
    case dialyzer_common:check_plt(OutDir) of
        fail ->
            {skip, "Plt creation/check failed."};
        ok ->
            Code = <<"-module(typer_test_module).
                   -export([foo/1]).
                   -spec foo(boolean()) -> string().
                   foo(N) ->
                       integer_to_list(N).">>,
            PrivDir = proplists:get_value(priv_dir, Config),
            Src = filename:join(PrivDir, "typer_test_module.erl"),
            ok = file:write_file(Src, Code),
            Args = "--plt " ++ PrivDir ++ "dialyzer_plt",
            Res = ["^typer: Error in contract of function typer_test_module:foo/1",
                   "^\t The contract is: \\(boolean\\(\\)\\) -> string\\(\\)",
                   "^\t but the inferred signature is: \\(integer\\(\\)\\) -> string\\(\\)",
                   "_ERROR_"],
            run(Config, Args, Src, Res),
            ok
    end.

compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

typer() ->
    case os:find_executable("typer") of
        false ->
            ct:fail("Can't find typer");
        Typer ->
            Typer
    end.

%% Runs a command.

run(Config, Args0, Name, Expect) ->
    Args = Args0 ++ " " ++ Name,
    Result = run_command(Config, Args),
    verify_result(Result, Expect).

verify_result(Result, Expect) ->
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

%% Runs the command using os:cmd/1.
%%
%% Returns the output from the command (as a list of characters with
%% embedded newlines).  The very last line will indicate the
%% exit status of the command, where _OK_ means zero, and _ERROR_
%% a non-zero exit status.

run_command(Config, Args) ->
    TmpDir = filename:join(proplists:get_value(priv_dir, Config), "tmp"),
    file:make_dir(TmpDir),
    {RunFile, Run, Script} = run_command(TmpDir, os:type(), Args),
    ok = file:write_file(filename:join(TmpDir, RunFile),
                         unicode:characters_to_binary(Script)),
    io:format("~ts\n", [Script]),
    os:cmd(Run).

run_command(Dir, {win32, _}, Args) ->
    BatchFile = filename:join(Dir, "run.bat"),
    Run = re:replace(filename:rootname(BatchFile), "/", "\\",
                     [global,{return,list}]),
    Typer = typer(),
    {BatchFile,
     Run,
     ["@echo off\r\n",
      "\"",Typer,"\" ",Args, "\r\n",
      "if errorlevel 1 echo _ERROR_\r\n",
      "if not errorlevel 1 echo _OK_\r\n"]};
run_command(Dir, {unix, _}, Args) ->
    TyperDir = filename:dirname(typer()),
    Name = filename:join(Dir, "run"),
    {Name,
     "/bin/sh " ++ Name,
     ["#!/bin/sh\n",
      "PATH=\"",TyperDir,":$PATH\"\n",
      "typer ",Args,"\n",
      "case $? in\n",
      "  0) echo '_OK_';;\n",
      "  *) echo '_ERROR_';;\n",
      "esac\n"]};
run_command(_Dir, Other, _Args) ->
    ct:fail("Don't know how to test exit code for ~p", [Other]).
