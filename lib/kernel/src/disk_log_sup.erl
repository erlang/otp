%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(disk_log_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local, disk_log_sup}, disk_log_sup, []).

init([]) ->
    SupFlags = {simple_one_for_one, 4, 3600},
    Child = {disk_log, {disk_log, istart_link, []}, temporary,
	     1000, worker, [disk_log]},
    {ok, {SupFlags, [Child]}}.
