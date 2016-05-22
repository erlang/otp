%%------------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%
%%------------------------------------------------------------------------
%% Description: a very simple implementation of PullConsumer interface
%%------------------------------------------------------------------------
-module(event_test_PullC_impl).
 
-export([init/1, terminate/2, disconnect_pull_consumer/1, do_pull/1, do_try_pull/1]).
 
init(Proxy) ->
        {ok, Proxy}.
 
terminate(_From, _Reason) ->
    ok.
 
disconnect_pull_consumer(Proxy) ->
    io:format("event_test_PullC terminates~n",[]),
    {stop, normal, ok, Proxy}.
 
do_pull(Proxy) ->
    {reply, 'CosEventComm_PullSupplier':pull(Proxy), Proxy}.

do_try_pull(Proxy) ->
    {reply, 'CosEventComm_PullSupplier':try_pull(Proxy), Proxy}.

