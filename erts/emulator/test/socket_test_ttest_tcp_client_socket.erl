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
         start/3, start/4, start/6, start/7,
         stop/1
        ]).

-define(TRANSPORT_MOD, socket_test_ttest_tcp_socket).
-define(MOD(D, M),     {?TRANSPORT_MOD, #{domain => D, method => M}}).

start(Method, ServerInfo, Active)
  when is_list(ServerInfo) ->
    Domain = local,
    socket_test_ttest_tcp_client:start_monitor(?MOD(Domain, Method),
                                               ServerInfo, Active);
start(Method, ServerInfo = {Addr, _}, Active)
  when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
    Domain = inet,
    socket_test_ttest_tcp_client:start_monitor(?MOD(Domain, Method),
                                               ServerInfo, Active);
start(Method, ServerInfo = {Addr, _}, Active)
  when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
    Domain = inet6,
    socket_test_ttest_tcp_client:start_monitor(?MOD(Domain, Method),
                                               ServerInfo, Active).

start(Method, ServerInfo, Active, MsgID)
  when is_list(ServerInfo) ->
    %% This is just a simplification
    Domain = local,
    socket_test_ttest_tcp_client:start(?MOD(Domain, Method),
                                       ServerInfo, Active, MsgID);
start(Method, ServerInfo = {Addr, _}, Active, MsgID)
  when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
    %% This is just a simplification
    Domain = inet,
    socket_test_ttest_tcp_client:start(?MOD(Domain, Method),
                                       ServerInfo, Active, MsgID);
start(Method, ServerInfo = {Addr, _}, Active, MsgID)
  when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
    Domain = inet6,
    socket_test_ttest_tcp_client:start(?MOD(Domain, Method),
                                       ServerInfo, Active, MsgID).

start(Method, ServerInfo, Active, MsgID, MaxOutstanding, RunTime)
  when is_list(ServerInfo) ->
    Domain = local,
    socket_test_ttest_tcp_client:start(false,
				       ?MOD(Domain, Method),
                                       ServerInfo, Active,
                                       MsgID, MaxOutstanding, RunTime);
start(Method, ServerInfo = {Addr, _}, Active, MsgID, MaxOutstanding, RunTime)
  when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
    Domain = inet,
    socket_test_ttest_tcp_client:start(false,
				       ?MOD(Domain, Method),
                                       ServerInfo, Active,
                                       MsgID, MaxOutstanding, RunTime);
start(Method, ServerInfo = {Addr, _}, Active, MsgID, MaxOutstanding, RunTime)
  when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
    Domain = inet6,
    socket_test_ttest_tcp_client:start(false,
				       ?MOD(Domain, Method),
                                       ServerInfo, Active,
                                       MsgID, MaxOutstanding, RunTime).

start(Quiet, Method, ServerInfo, Active, MsgID, MaxOutstanding, RunTime)
  when is_list(ServerInfo) ->
    Domain = local,
    socket_test_ttest_tcp_client:start(Quiet,
				       ?MOD(Domain, Method),
                                       ServerInfo, Active,
                                       MsgID, MaxOutstanding, RunTime);
start(Quiet, Method, ServerInfo = {Addr, _}, Active, MsgID, MaxOutstanding, RunTime)
  when is_tuple(Addr) andalso (size(Addr) =:= 4) ->
    Domain = inet,
    socket_test_ttest_tcp_client:start(Quiet,
				       ?MOD(Domain, Method),
                                       ServerInfo, Active,
                                       MsgID, MaxOutstanding, RunTime);
start(Quiet, Method, ServerInfo = {Addr, _}, Active, MsgID, MaxOutstanding, RunTime)
  when is_tuple(Addr) andalso (size(Addr) =:= 8) ->
    Domain = inet6,
    socket_test_ttest_tcp_client:start(Quiet,
				       ?MOD(Domain, Method),
                                       ServerInfo, Active,
                                       MsgID, MaxOutstanding, RunTime).

stop(Pid) ->
    socket_test_ttest_client:stop(Pid).
