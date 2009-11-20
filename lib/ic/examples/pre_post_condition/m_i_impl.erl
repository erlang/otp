%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%------------------------------------------------------------
%%
%% Example 
%%
%%------------------------------------------------------------
-module(m_i_impl).

%% Standard functions
-export([init/1, terminate/2]).
%% Interface functions
-export([f/2, g/2]).

init(_Env) ->
    {ok, []}.

terminate(_From, _Reason) ->
    ok.

f(State, In) ->
    io:format("f working ....\n", []),
    {reply, In, State}.

g(State, _In)  ->
    io:format("g working ....\n", []),
    {noreply, State}.





