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

-module(socket_test_ttest_tcp_server_gen).

-export([
         start/1, start/2,
         stop/1
        ]).

-define(TRANSPORT_MOD, socket_test_ttest_tcp_gen).
-define(MOD(D),        {?TRANSPORT_MOD, #{domain => D}}).

start(Active) ->
    start(inet, Active).

start(Domain, Active) ->
    socket_test_ttest_tcp_server:start(?MOD(Domain), Active).
            

stop(Pid) ->
    socket_test_ttest_tcp_server:stop(Pid).
