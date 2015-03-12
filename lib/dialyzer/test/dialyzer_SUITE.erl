%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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
-module(dialyzer_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, dialyzer).

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([app_test/1, appup_test/1, beam_tests/1,
	 run_plt_check/1, run_succ_typings/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, beam_tests, run_plt_check, run_succ_typings].

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
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%%
%%% Test cases starts here.
%%%

app_test(doc) ->
    ["Test that the .app file does not contain any `basic' errors"];
app_test(suite) ->
    [];
app_test(Config) when is_list(Config) ->
    ?line ?t:app_test(dialyzer).

%% Test that the .appup file does not contain any `basic' errors
appup_test(Config) when is_list(Config) ->
    ok = ?t:appup_test(dialyzer).

beam_tests(Config) when is_list(Config) ->
    Prog = <<"
              -module(no_auto_import).

              %% Copied from erl_lint_SUITE.erl, clash6

              -export([size/1]).

              size([]) ->
                  0;
              size({N,_}) ->
                  N;
              size([_|T]) ->
                  1+size(T).
             ">>,
    Opts = [no_auto_import],
    {ok, BeamFile} = compile(Config, Prog, no_auto_import, Opts),
    [] = run_dialyzer(plt_build, [BeamFile], []),
    ok.

run_plt_check(Config) when is_list(Config) ->
    Mod1 = <<"
	      -module(run_plt_check1).
	     ">>,

    Mod2A = <<"
	       -module(run_plt_check2).
	      ">>,

    {ok, BeamFile1} = compile(Config, Mod1, run_plt_check1, []),
    {ok, BeamFile2} = compile(Config, Mod2A, run_plt_check2, []),
    [] = run_dialyzer(plt_build, [BeamFile1, BeamFile2], []),

    Mod2B = <<"
	       -module(run_plt_check2).

	       -export([call/1]).

	       call(X) -> run_plt_check1:call(X).
	     ">>,

    {ok, BeamFile2} = compile(Config, Mod2B, run_plt_check2, []),

    % callgraph warning as run_plt_check2:call/1 makes a call to unexported
    % function run_plt_check1:call/1.
    [_] = run_dialyzer(plt_check, [], []),

    ok.

run_succ_typings(Config) when is_list(Config) ->
    Mod1A = <<"
	       -module(run_succ_typings1).

	       -export([call/0]).

	       call() -> a.
	      ">>,

    {ok, BeamFile1} = compile(Config, Mod1A, run_succ_typings1, []),
    [] = run_dialyzer(plt_build, [BeamFile1], []),

    Mod1B = <<"
	       -module(run_succ_typings1).

	       -export([call/0]).

	       call() -> b.
	     ">>,

    Mod2 = <<"
	      -module(run_succ_typings2).

	      -export([call/0]).

	      -spec call() -> b.
	      call() -> run_succ_typings1:call().
	     ">>,

    {ok, BeamFile1} = compile(Config, Mod1B, run_succ_typings1, []),
    {ok, BeamFile2} = compile(Config, Mod2, run_succ_typings2, []),
    % contract types warning as run_succ_typings2:call/0 makes a call to
    % run_succ_typings1:call/0, which returns a (not b) in the PLT.
    [_] = run_dialyzer(succ_typings, [BeamFile2], [{check_plt, false}]),
    % warning not returned as run_succ_typings1 is updated in the PLT.
    [] = run_dialyzer(succ_typings, [BeamFile2], [{check_plt, true}]),

    ok.

compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = ?config(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

run_dialyzer(Analysis, Files, Opts) ->
    dialyzer:run([{analysis_type, Analysis},
		  {files, Files},
		  {from, byte_code},
		  {warnings, [no_unknown]} |
		  Opts]).
