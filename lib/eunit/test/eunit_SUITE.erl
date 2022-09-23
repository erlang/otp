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
         fixture_test/1, primitive_test/1, surefire_utf8_test/1,
         surefire_latin_test/1, surefire_c0_test/1, surefire_ensure_dir_test/1,
	 stacktrace_at_timeout_test/1]).

-export([sample_gen/0]).

-include_lib("common_test/include/ct.hrl").
-define(TIMEOUT, 1000).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, eunit_test, eunit_exact_test, primitive_test,
     fixture_test, surefire_utf8_test, surefire_latin_test, surefire_c0_test,
     surefire_ensure_dir_test, stacktrace_at_timeout_test].

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
    {ok, fib} = compile:file("./examples/fib.erl", [{outdir,"./examples/"}]),
    TestPrimitive =
        fun(Primitive, Expected) ->
                ok = eunit:test(Primitive,
                                [{report, {eunit_test_listener, [self()]}},
                                 {exact_execution, true}]),
                check_test_results(Primitive, Expected)
        end,
    Primitives =
        [
         {[eunit, eunit_tests],
          #{pass => 7, fail => 0, skip => 0, cancel => 0}},
         {{application, stdlib},
          #{pass => 0, fail => 0, skip => 0, cancel => 0}},
         {{file, "./ebin/eunit.beam"},
          #{pass => 0, fail => 0, skip => 0, cancel => 0}},
         {{file, "./ebin/eunit_tests.beam"},
          #{pass => 7, fail => 0, skip => 0, cancel => 0}},
         {{dir, "./examples/"},
          #{pass => 8, fail => 0, skip => 0, cancel => 0}},
         {{generator, fun() -> fun () -> ok end end},
          #{pass => 1, fail => 0, skip => 0, cancel => 0}},
         {{generator, ?MODULE, sample_gen},
          #{pass => 1, fail => 0, skip => 0, cancel => 0}},
         {{with, value, [fun(_V) -> ok end]},
          #{pass => 1, fail => 0, skip => 0, cancel => 0}}
        ],
    [TestPrimitive(P, E) || {P, E} <- Primitives],
    ok.


primitive_test(Config) when is_list(Config) ->
    ok = file:set_cwd(code:lib_dir(eunit)),
    {ok, fib} = compile:file("./examples/fib.erl", [{outdir,"./examples/"}]),
    TestPrimitive =
        fun(Primitive, Expected) ->
                ok = eunit:test(Primitive,
                                [{report, {eunit_test_listener, [self()]}}]),
                check_test_results(Primitive, Expected),
                ok = eunit:test(Primitive,
                                [{report, {eunit_test_listener, [self()]}},
                                {exact_execution, false}]),
                check_test_results(Primitive, Expected)
        end,
    Primitives =
        [
         {[eunit, eunit_tests],
          #{pass => 14, fail => 0, skip => 0, cancel => 0}},
         {{application, stdlib},
          #{pass => 0, fail => 0, skip => 0, cancel => 0}},
         {{file, "./ebin/eunit.beam"},
          #{pass => 7, fail => 0, skip => 0, cancel => 0}},
         {{file, "./ebin/eunit_tests.beam"},
          #{pass => 7, fail => 0, skip => 0, cancel => 0}},
         {{dir, "./examples/"},
          #{pass => 8, fail => 0, skip => 0, cancel => 0}},
         {{generator, fun() -> fun () -> ok end end},
          #{pass => 1, fail => 0, skip => 0, cancel => 0}},
         {{generator, ?MODULE, sample_gen},
          #{pass => 1, fail => 0, skip => 0, cancel => 0}},
         {{with, value, [fun(_V) -> ok end]},
          #{pass => 1, fail => 0, skip => 0, cancel => 0}}
        ],
    [TestPrimitive(P, E) || {P, E} <- Primitives],
    ok.

sample_gen() ->
    fun () -> ok end.

fixture_test(Config) when is_list(Config) ->
    eunit:test({setup, fun() -> ok end, fun() -> fun() -> ok end end}),
    eunit:test({setup, fun() -> ok end, [{module, eunit_tests}]}),
    eunit:test({foreach, fun() -> ok end, [fun() -> ok end]}),
    eunit:test({foreachx, fun(_A) -> ok end,
                [{1, fun(_A, _B) -> fun() -> a_test end end}]}),
    ok.

check_test_results(Primitive, Expected) ->
    receive
        {test_report, TestReport} ->
            case Expected == TestReport of
                true ->
                    ok;
                _ ->
                    ct:pal("~p Expected: ~w Received: ~w",
                           [Primitive, Expected, TestReport]),
                    ct:fail(unexpected_result)
            end
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
