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

%%
-module(mnesia_kernel_sup).

-behaviour(supervisor).

-export([start/0, init/1, supervisor_timeout/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% top supervisor callback functions

start() ->
    supervisor:start_link({local, mnesia_kernel_sup}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sub supervisor callback functions

init([]) ->
    ProcLib = [mnesia_monitor, proc_lib],
    Flags = {one_for_all, 0, timer:hours(24)}, % Trust the top supervisor
    Workers = [worker_spec(mnesia_monitor, timer:seconds(3), [gen_server]),
	       worker_spec(mnesia_subscr, timer:seconds(3), [gen_server]),
	       worker_spec(mnesia_locker, timer:seconds(3), ProcLib),
	       worker_spec(mnesia_recover, timer:minutes(3), [gen_server]),
	       worker_spec(mnesia_tm, timer:seconds(30), ProcLib),
	       supervisor_spec(mnesia_checkpoint_sup),
	       supervisor_spec(mnesia_snmp_sup),
	       worker_spec(mnesia_controller, timer:seconds(3), [gen_server]),
	       worker_spec(mnesia_late_loader, timer:seconds(3), ProcLib)
	      ],
    {ok, {Flags, Workers}}.

worker_spec(Name, KillAfter, Modules) ->
    KA = supervisor_timeout(KillAfter),
    {Name, {Name, start, []}, permanent, KA, worker, [Name] ++ Modules}.

supervisor_spec(Name) ->
    {Name, {Name, start, []}, permanent, infinity, supervisor,
     [Name, supervisor]}.
    
-ifdef(debug_shutdown).
supervisor_timeout(_KillAfter) -> timer:hours(24).
-else.
supervisor_timeout(KillAfter) -> KillAfter.
-endif.    

    
