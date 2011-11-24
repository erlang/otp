%%------------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

