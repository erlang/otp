%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2022. All Rights Reserved.
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
-module(dialyzer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, test_server:minutes(10)).
-define(application, dialyzer).

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1, appup_test/1]).
-export([cplt_info/1, iplt_info/1,
         incremental_plt_given_to_classic_mode/1,
         classic_plt_given_to_incremental_mode/1,
         if_output_plt_is_missing_incremental_mode_makes_it/1,
         file_list/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, cplt_info, iplt_info,
     incremental_plt_given_to_classic_mode,
     classic_plt_given_to_incremental_mode,
     if_output_plt_is_missing_incremental_mode_makes_it,
     file_list].

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


init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

%%%
%%% Test cases starts here.
%%%

app_test(doc) ->
    ["Test that the .app file does not contain any `basic' errors"];
app_test(suite) ->
    [];
app_test(Config) when is_list(Config) ->
    test_server:app_test(dialyzer).

%% Test that the .appup file does not contain any `basic' errors
appup_test(Config) when is_list(Config) ->
    ok = test_server:appup_test(dialyzer).

cplt_info(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).

         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt1 = filename:join(PrivDir, "cplt_info.plt"),
    _ = dialyzer:run([{analysis_type, plt_build},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {from, byte_code}]),

    {ok, [{files, [Beam1]}]} = dialyzer:plt_info(Plt1).

iplt_info(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).

         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt1 = filename:join(PrivDir, "iplt_info.plt"),
    _ = dialyzer:run([{analysis_type, incremental},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {from, byte_code}]),

    {ok, {incremental, [{modules, [foo]}]}} = dialyzer:plt_info(Plt1).

incremental_plt_given_to_classic_mode(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).

         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt1 = filename:join(PrivDir, "my_incremental.iplt"),
    _ = dialyzer:run([{analysis_type, incremental},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {output_plt, Plt1},
                       {from, byte_code}]),

    ?assertException(throw, {dialyzer_error, "Given file is an incremental PLT file, but outside of incremental mode, a classic PLT file is expected: {init_plt_file," ++ _ },
                 dialyzer:run([{analysis_type, plt_check},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {from, byte_code}])).

classic_plt_given_to_incremental_mode(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).

         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt1 = filename:join(PrivDir, "my_classic.plt"),
    _ = dialyzer:run([{analysis_type, plt_build},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {output_plt, Plt1},
                       {from, byte_code}]),

    ?assertException(throw, {dialyzer_error, "Given file is a classic PLT file, but in incremental mode, an incremental PLT file is expected: {init_plt_file," ++ _ },
                 dialyzer:run([{analysis_type, incremental},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {from, byte_code}])).

if_output_plt_is_missing_incremental_mode_makes_it(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).

         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt1 = filename:join(PrivDir, "brand_new.iplt"),
    ?assertMatch(false, filelib:is_regular(Plt1)),

    _ = dialyzer:run([{analysis_type, incremental},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {output_plt, Plt1},
                       {from, byte_code}]),

    ?assertMatch(true, filelib:is_regular(Plt1)).

file_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    case dialyzer_common:check_plt(PrivDir) of
        fail -> ct:fail("Plt creation/check failed");
        ok -> ok
    end,

    Files = generate_modules(PrivDir, 26),
    ListFile = filename:join(PrivDir, "list_of_files"),
    ok = file:write_file(ListFile, [lists:join("\n", Files), "\n"]),

    Expected = expected(Files),
    ExpectedFile = filename:join(PrivDir, "expected"),
    ok = file:write_file(ExpectedFile, Expected),

    Plt = dialyzer_common:plt_file(PrivDir),
    Result = os:cmd("dialyzer --plt " ++ Plt ++ " -q --src --input_list_file " ++ ListFile),
    ResultFile = filename:join(PrivDir, "result"),
    ok = file:write_file(ResultFile, Result),

    case file_utils:diff(ResultFile, ExpectedFile) of
        same ->
            ok;
        Diff ->
            io:format("~p\n", [Diff]),
            ct:fail(unexpected_result)
    end.

generate_modules(_Dir, 0) ->
    [];
generate_modules(Dir, N) ->
    Name = "module_" ++ integer_to_list(N),
    File = filename:join(Dir, Name ++ ".erl"),
    Code = <<"-module(",(list_to_binary(Name))/binary,").\n",
             "-export([main/1]).\n",
             "main(L) ->\n",
             "  case list_to_atom(L) of\n",
             "    Atom when is_atom(Atom) -> {ok,Atom};\n",
             "    _ -> error\n",
             "  end.\n"
           >>,
    ok = file:write_file(File, Code),
    [File|generate_modules(Dir, N - 1)].

expected(Files0) ->
    Files = lists:sort(Files0),
    S = "\n" ++
        [filename:basename(F) ++
             ":6:5: The variable _ can never match since previous clauses completely covered the type \n"
         "          atom()\n" || F <- Files],
    iolist_to_binary(S).

