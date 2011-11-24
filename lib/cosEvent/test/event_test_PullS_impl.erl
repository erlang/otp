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


