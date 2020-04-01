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

-module(socket_test_ttest_tcp_server_socket).

-export([
         start/4,
         stop/1
        ]).

-define(TRANSPORT_MOD, socket_test_ttest_tcp_socket).
%% -define(MOD(M),        {?TRANSPORT_MOD, #{async          => false, 
%%                                           method         => M,
%%                                           stats_interval => 10000}}).
-define(MOD(D,M,A),      {?TRANSPORT_MOD, #{domain => D,
                                            async  => A,
                                            method => M}}).

start(Method, Domain, Async, Active) ->
    socket_test_ttest_tcp_server:start(?MOD(Domain, Method, Async), Active).
    %%     {ok, {Pid, AddrPort}} ->
    %%         MRef = erlang:monitor(process, Pid),
    %%         {ok, {Pid, MRef, AddrPort}};
    %%     {error, _} = ERROR ->
    %%         ERROR
    %% end.

stop(Pid) ->
    socket_test_ttest_tcp_server:stop(Pid).
