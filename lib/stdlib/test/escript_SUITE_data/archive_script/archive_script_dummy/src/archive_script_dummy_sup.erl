%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
-module(archive_script_dummy_sup).
-behaviour(supervisor).

%% Public
-export([start_link/1]).

%% Internal
-export([init/1]).

start_link(Debug) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Debug]).

init([Debug]) ->
    Flags = {one_for_one, 0, 3600},
    {ok, {Flags, []}}.
