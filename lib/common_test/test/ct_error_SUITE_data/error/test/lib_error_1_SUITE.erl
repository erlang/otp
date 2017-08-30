%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(lib_error_1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,3}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [lines_error, lines_exit, lines_hang, lines_throw,
     no_lines_error, no_lines_exit, no_lines_hang, no_lines_throw,
     init_tc_error, init_tc_exit, init_tc_throw,
     end_tc_error, end_tc_exit, end_tc_throw].

lines_error(_) ->
    io:format("Calling lib function..."),
    lib_lines:do_error(),
    ok.

lines_exit(_) ->
    io:format("Calling lib function..."),
    lib_lines:do_exit(),
    ok.

lines_hang(_) ->
    io:format("Calling lib function..."),
    lib_lines:do_hang(),
    ok.

lines_throw(_) ->
    io:format("Calling lib function..."),
    lib_lines:do_throw(),
    ok.

no_lines_error(_) ->
    io:format("Calling lib function..."),
    lib_no_lines:do_error(),
    ok.

no_lines_exit(_) ->
    io:format("Calling lib function..."),
    lib_no_lines:do_exit(),
    ok.

no_lines_hang(_) ->
    io:format("Calling lib function..."),
    lib_no_lines:do_hang(),
    ok.

no_lines_throw(_) ->
    io:format("Calling lib function..."),
    lib_no_lines:do_throw(),
    ok.

init_tc_error() ->
    put('$test_server_framework_test',
	fun(init_tc, _Default) -> lib_no_lines:do_error(), ok;
	   (_, Default) -> Default
	end), [].

init_tc_error(_) ->
    ok.

init_tc_exit() ->
    put('$test_server_framework_test',
	fun(init_tc, _Default) -> lib_no_lines:do_exit(), ok;
	   (_, Default) -> Default
	end), [].

init_tc_exit(_) ->
    ok.

init_tc_throw() ->
    put('$test_server_framework_test',
	fun(init_tc, _Default) -> lib_no_lines:do_throw(), ok;
	   (_, Default) -> Default
	end), [].

init_tc_throw(_) ->
    ok.

end_tc_error(_) ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) -> lib_no_lines:do_error(), ok;
	   (_, Default) -> Default
	end), ok.

end_tc_exit(_) ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) -> lib_no_lines:do_exit(), ok;
	   (_, Default) -> Default
	end), ok.

end_tc_throw(_) ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) -> lib_no_lines:do_throw(), ok;
	   (_, Default) -> Default
	end), ok.
