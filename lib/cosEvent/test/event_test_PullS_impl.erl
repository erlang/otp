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
%% Description: a very simple implementation of Pull Supplier interface
%%------------------------------------------------------------------------
-module(event_test_PullS_impl).

-include_lib("orber/include/corba.hrl").

-export([init/1, terminate/2, pull/1, try_pull/1, disconnect_pull_supplier/1,
	 add_event/2]).
 
init(_) ->
        {ok, []}.
 
terminate(_From, _Reason) ->
        ok.
 
pull([]) ->
    corba:raise(#'INTERNAL'{completion_status = ?COMPLETED_NO});
pull([Event|Events]) ->
    {reply, Event, Events}.
 
try_pull([]) ->
    {reply, {#any{typecode=tk_null, value = null}, false}, []};
try_pull([Event|Events]) ->
        {reply, {Event, true}, Events}.
 
disconnect_pull_supplier(Events) ->
    io:format("event_test_PullS terminates ~p~n", [Events]),
    {stop, normal, ok, Events}.


add_event(Events, Event) ->
    %% Store in FIFO order; don't really care if we use '++' since
    %% this operation is used in tests only.
    {reply, ok, Events ++ [Event]}.


