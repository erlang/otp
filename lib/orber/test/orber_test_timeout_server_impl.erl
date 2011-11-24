%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : orber_test_timeout_server_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(orber_test_timeout_server_impl).

-export([oneway_function/3, twoway_function/3]).


%%--------------- gen_server specific ------------------------
-export([init/1, terminate/2, code_change/3, handle_info/2]).
      
%%------------------------------------------------------------
%% function : server specific
%%------------------------------------------------------------
init(State) ->
    %% 'trap_exit' optional
    process_flag(trap_exit,true),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% If use IC option {{handle_info, "Module::Interface"}, true}
handle_info(_Info, State) ->
    %% Await the next invocation.
    {noreply, State}.

%%--- two-way ------------------------------------------------
twoway_function(_OE_THIS, State, Time) ->
    timer:sleep(Time),
    {reply, ok, State}.


%%--- one-way ------------------------------------------------
oneway_function(_OE_THIS, State, Time) ->
    timer:sleep(Time),
    {noreply, State}.

%%--------------- END OF MODULE ------------------------------

