%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
-module(group_leader).
-behaviour(gen_server).

%% External exports
-export([start_link/0, code_change/3]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() -> gen_server:start_link({local,aa}, ?MODULE, [], []).

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
init([]) ->
    Self = self(),
    Pid = spawn(fun() -> stupid_child(Self) end) ,
    receive {Pid, registration_done} -> ok end,
    process_flag(trap_exit, true),
    {ok,state}.

handle_call(transient, _From, State) ->
    X = application:get_all_env(transient),
    {reply,X,State}.

handle_cast(transient, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stupid_child(Parent) ->
    register(nisse, self()),
    Parent ! {self(), registration_done},
    receive
	_Msg -> ok
    end.
