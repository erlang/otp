%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
-module(cfg_error_13_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite() ->
    put('$test_server_framework_test',
	fun(end_tc, _Default) ->
		ct:pal("end_tc(~p): Night time...",[self()]),
		ct:sleep(1000),
		ct:pal("end_tc(~p): Day time!",[self()]);
	   (_, Default) -> Default
	end),
    [{timetrap,500}].

init_per_suite(Config) ->
    ct:comment("should succeed since ct_fw cancels timetrap in end_tc"),
    Config.

end_per_suite(_) ->
    ok.

all() ->
    [tc1].

%%%-----------------------------------------------------------------
tc1(_) ->
    dummy.
