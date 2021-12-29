%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2020. All Rights Reserved.
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

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 app_test/1,appup_test/1,eunit_test/1,surefire_utf8_test/1,surefire_latin_test/1,
	 surefire_c0_test/1, surefire_ensure_dir_test/1,
	 stacktrace_at_timeout_test/1, eunit_duplicates_test/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, eunit_test, surefire_utf8_test, surefire_latin_test,
     surefire_c0_test, surefire_ensure_dir_test, stacktrace_at_timeout_test,
     eunit_duplicates_test].

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

eunit_duplicates_test(Config) when is_list(Config) ->
    LogFile = "eunit-events-duplicates.log",
    ok = file:set_cwd(code:lib_dir(eunit)),
    Tests = [[eunit, eunit_tests, eunit], eunit, eunit_tests, [eunit],
             [eunit_tests], [eunit, eunit], [eunit, eunit_tests],
             [eunit_tests, eunit], [eunit_tests, eunit_tests]],
    case eunit_duplicates(LogFile, Tests) of
    ok ->
        ok;
    {error, Test, Log} ->
        ct:fail("Test point ~p must have only one event in:~n~p", [Test, Log])
    end.

eunit_duplicates(LogFile, []) ->
    ok = file:delete(LogFile),
    ok;
eunit_duplicates(LogFile, [H|T]) ->
    ok = eunit:test(H, [{event_log, LogFile}]),
    {ok, Events} = file:consult(LogFile),
    case [Event || Event = {_, [Count], _} <- Events, Count /= 1] of
    [] ->
        eunit_duplicates(LogFile, T);
    [_|_] ->
        {error, H, Events}
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
