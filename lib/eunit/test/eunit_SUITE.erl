%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
         stacktrace_at_timeout_test/1, scale_timeouts_test/1]).

%% Two eunit tests:
-export([times_out_test_/0, times_out_default_test/0]).

-export([sample_gen/0]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(TIMEOUT, 1000).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_test, appup_test, eunit_test, eunit_exact_test, primitive_test,
     fixture_test, surefire_utf8_test, surefire_latin_test, surefire_c0_test,
     surefire_ensure_dir_test, stacktrace_at_timeout_test,
     scale_timeouts_test].

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

scale_timeouts_test(_Config) ->
    %% Scaling with integers
    %% The times_out_test_ will timeout after 1 second.
    %% Scale it up by a factor of 2 and check that at least 2s have passed.
    Millis1 = run_eunit_test_that_times_out(times_out_test_,
                                            [{scale_timeouts, 2}]),
    ?assert(Millis1 >= 2000, #{duration => Millis1}),
    ?assert(Millis1 <  5000, #{duration => Millis1}),

    %% Scaling with float: should get rounded
    %% Scaling down should work too
    Millis2 = run_eunit_test_that_times_out(times_out_test_,
                                            [{scale_timeouts, 0.25}]),
    ?assert(Millis2 >= 250, #{duration => Millis2}),
    ?assert(Millis2 < 1000, #{duration => Millis2}),

    %% It should be possible to scale the default timeout as well
    Millis3 = run_eunit_test_that_times_out(times_out_default_test,
                                            [{scale_timeouts, 0.01}]),
    ?assert(Millis3 > 0, #{duration => Millis3}),
    ?assert(Millis3 < 1000, #{duration => Millis3}),
    ok.

run_eunit_test_that_times_out(TestFn, Options) ->
    T0 = erlang:monotonic_time(millisecond),
    %% Expect error due to the timeout:
    case lists:suffix("_test_", atom_to_list(TestFn)) of
        true ->
            error = eunit:test({generator, ?MODULE, TestFn}, Options);
        false ->
            error = eunit:test({?MODULE, TestFn}, Options)
    end,
    T1 = erlang:monotonic_time(millisecond),
    T1 - T0.

%% an eunit test generator:
times_out_test_() ->
    {timeout, 1, % the fun should timeout after this many seconds
     fun() -> timer:sleep(10_000) % long enough to cause a timeout
     end}.

%% an eunit test:
times_out_default_test() ->
    %% The default timeout for an xyz_test/0 is 5s,
    %% so this is long enough to cause a time out.
    timer:sleep(20_000).

