%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2022. All Rights Reserved.
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
-module(eunit_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 app_test/1, appup_test/1, eunit_test/1, eunit_exact_test/1,
         surefire_utf8_test/1, surefire_latin_test/1,
	 surefire_c0_test/1, surefire_ensure_dir_test/1,
	 stacktrace_at_timeout_test/1]).

-include_lib("common_test/include/ct.hrl").
-define(TIMEOUT, 1000).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, eunit_test, eunit_exact_test, surefire_utf8_test,
     surefire_latin_test, surefire_c0_test, surefire_ensure_dir_test,
     stacktrace_at_timeout_test].

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

app_test(Config) when is_list(Config) ->
    ok = test_server:app_test(eunit).

appup_test(Config) when is_list(Config) ->
    ok = test_server:appup_test(eunit).

eunit_test(Config) when is_list(Config) ->
    ok = file:set_cwd(code:lib_dir(eunit)),
    ok = eunit:test(eunit).

eunit_exact_test(Config) when is_list(Config) ->
    ok = file:set_cwd(code:lib_dir(eunit)),
    ok = eunit:test([eunit, eunit_tests],
                    [{report, {eunit_test_listener, [self()]}}]),
    check_test_results(14, 0, 0, 0),
    ok = eunit:test([eunit, eunit_tests],
                    [{report, {eunit_test_listener, [self()]}},
                     {exact_execution, false}]),
    check_test_results(14, 0, 0, 0),
    ok = eunit:test([eunit, eunit_tests],
                    [{report, {eunit_test_listener, [self()]}},
                     {exact_execution, true}]),
    check_test_results(7, 0, 0, 0),
    ok.

check_test_results(Pass, Fail, Skip, Cancel) ->
    receive
        {test_report, TestReport} ->
            #{pass := Pass, fail := Fail,
              skip := Skip, cancel := Cancel} = TestReport
    after ?TIMEOUT ->
            ct:fail(no_test_report_not_received)
    end.

surefire_latin_test(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config, ".")),
	check_surefire(tlatin),
	ok.

surefire_utf8_test(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config, ".")),
	check_surefire(tutf8),
	ok.

surefire_c0_test(Config) when is_list(Config) ->
    ok = file:set_cwd(proplists:get_value(priv_dir, Config, ".")),
    Chars = check_surefire(tc0),
    %% Check that these characters were not stripped
    true = lists:member($\n, Chars),
    true = lists:member($\r, Chars),
    true = lists:member($\t, Chars),
    ok.

surefire_ensure_dir_test(Config) when is_list(Config) ->
    XMLDir = filename:join(proplists:get_value(priv_dir, Config), "c1"),
    ok = eunit:test(tc0, [{report,{eunit_surefire,[{dir,XMLDir}]}}]),
    ok = file:del_dir_r(XMLDir).

stacktrace_at_timeout_test(Config) when is_list(Config) ->
    Chars = check_surefire(ttimesout),
    case string:find(Chars, "in call from") of
        nomatch ->
            ct:pal("Surefire XML:~n~ts", [Chars]),
            ct:fail(missing_stacktrace_in_surefire);
        _ ->
            ok
    end.

check_surefire(Module) ->
	File = "TEST-"++atom_to_list(Module)++".xml",
	file:delete(File),
	% ignore test result, some fail on purpose
	eunit:test(Module, [{report,{eunit_surefire,[{dir,"."}]}}]),
	{ok, Bin} = file:read_file(File),
	Chars = unicode:characters_to_list(Bin, unicode),
	%% Check that unicode decoding succeeded
	[_|_] = Chars,
	%% Check that file is valid XML
	xmerl_scan:file(File),
	Chars.
