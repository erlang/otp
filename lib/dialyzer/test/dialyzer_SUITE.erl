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
-export([app_test/1, appup_test/1, beam_tests/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, beam_tests].

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
    [] = run_dialyzer([BeamFile]),
    ok.

compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = ?config(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

run_dialyzer(Files) ->
    dialyzer:run([{analysis_type, plt_build},
                  {files, Files},
                  {from, byte_code},
                  {check_plt, false}]).
