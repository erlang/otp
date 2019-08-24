%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(cfg_error_12_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(tc2, _Config) ->
    ct:sleep(2000),
    exit(this_should_not_be_printed);
end_per_testcase(tc4, _Config) ->
    ct:sleep(2000),
    exit(this_should_not_be_printed);
end_per_testcase(_, _) ->
    ok.

all() ->
    [tc1, tc2, tc3, tc4].

%%%-----------------------------------------------------------------
tc1() ->
    put('$test_server_framework_test',
	fun(init_tc, _Default) ->
		ct:pal("init_tc(~p): Night time...",[self()]),
		ct:sleep(2000),
		ct:pal("init_tc(~p): Day time!",[self()]),
		exit(this_should_not_be_printed);
	   (_, Default) -> Default
	end),
    [{timetrap,500}].

tc1(_) ->
    exit(this_should_not_be_printed).

%%%-----------------------------------------------------------------
tc2() ->
    [{timetrap,500}].

tc2(_) ->
    ok.

%%%-----------------------------------------------------------------
tc3() ->
    [{timetrap,500}].

tc3(_) ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) ->
		ct:pal("end_tc(~p): Night time...",[self()]),
		ct:sleep(1000),
		ct:pal("end_tc(~p): Day time!",[self()]);
	   (_, Default) -> Default
	end),
    {comment,"should succeed since ct_fw cancels timetrap in end_tc"}.

%%%-----------------------------------------------------------------
tc4() ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) ->
		ct:pal("end_tc(~p): Night time...",[self()]),
		ct:sleep(1000),
		ct:pal("end_tc(~p): Day time!",[self()]);
	   (_, Default) -> Default
	end),
    [{timetrap,500}].

tc4(_) ->
    ok.
