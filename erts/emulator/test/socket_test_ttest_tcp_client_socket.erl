%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

-module(socket_test_ttest_tcp_client_socket).

-export([
         start/4, start/5, start/7, start/8,
         stop/1
        ]).

-define(TRANSPORT_MOD, socket_test_ttest_tcp_socket).
-define(MOD(M),        {?TRANSPORT_MOD, #{method => Method}}).

start(Method, Active, Addr, Port) ->
    socket_test_ttest_tcp_client:start_monitor(?MOD(Method), Active, Addr, Port).

start(Method, Active, Addr, Port, MsgID) ->
    socket_test_ttest_tcp_client:start(?MOD(Method),
                                       Active, Addr, Port, MsgID).

start(Method, Active, Addr, Port, MsgID, MaxOutstanding, RunTime) ->
    socket_test_ttest_tcp_client:start(false,
				       ?MOD(Method),
                                       Active, Addr, Port,
                                       MsgID, MaxOutstanding, RunTime).

start(Quiet, Method, Active, Addr, Port, MsgID, MaxOutstanding, RunTime) ->
    socket_test_ttest_tcp_client:start(Quiet,
				       ?MOD(Method),
                                       Active, Addr, Port,
                                       MsgID, MaxOutstanding, RunTime).

stop(Pid) ->
    socket_test_ttest_client:stop(Pid).
