%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(snmpa_agent_sup).

-include("snmp_debug.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_link/1, start_subagent/3, stop_subagent/1]).

%% Internal exports
-export([init/1]).

-define(SERVER, ?MODULE).
%% Always use plain ets for sub-agents
-ifdef(snmp_debug).
-define(DEFAULT_SA_OPTS, [{mib_storage, [{module, snmpa_mib_storage_ets}]}, 
			  {verbosity, trace}]).
-else.
-define(DEFAULT_SA_OPTS, [{mib_storage, [{module, snmpa_mib_storage_ets}]}]).
-endif.


%%%-----------------------------------------------------------------
%%% This is a supervisor for the mib processes.  Each agent has one
%%% mib process.
%%%-----------------------------------------------------------------
start_link() ->
    ?d("start_link -> entry", []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [[]]).

start_link(AgentSpec) ->
    ?d("start_link -> entry with"
	"~n   AgentSpec: ~p", [AgentSpec]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [[AgentSpec]]).

start_subagent(ParentAgent, Subtree, Mibs) ->
    ?d("start_subagent -> entry with"
	"~n   ParentAgent: ~p"
	"~n   Subtree:     ~p"
	"~n   Mibs:        ~p", [ParentAgent, Subtree, Mibs]),
    Children = supervisor:which_children(?SERVER),
    ?d("start_subagent -> Children: ~n~p", [Children]),
    Max = find_max(Children, 1),
    ?d("start_subagent -> Max: ~p", [Max]),
    [{_, Prio}] = ets:lookup(snmp_agent_table, priority),
    ?d("start_subagent -> Prio: ~p", [Prio]),
    Ref = make_ref(),
    ?d("start_subagent -> Ref: ~p", [Ref]),
    Options = [{priority, Prio}, 
	       {mibs,     Mibs}, 
	       {misc_sup, snmpa_misc_sup} | ?DEFAULT_SA_OPTS],
    Agent = {{sub_agent, Max},
	     {snmpa_agent, start_link,
	      [Prio, ParentAgent, Ref, Options]},
	     permanent, 2000, worker, [snmpa_agent]},
    case supervisor:start_child(?SERVER, Agent) of
	{ok, SA} -> 
	    ?d("start_subagent -> SA: ~p", [SA]),
	    snmpa_agent:register_subagent(ParentAgent, Subtree, SA),
	    {ok, SA};
	Error ->
	    ?d("start_subagent -> Error: ~p", [Error]),
	    Error
    end.

stop_subagent(SubAgentPid) ->
    case find_name(supervisor:which_children(?SERVER), SubAgentPid) of
	undefined -> 
	    no_such_child;
	Name ->
	    supervisor:terminate_child(?SERVER, Name),
	    supervisor:delete_child(?SERVER, Name),
	    ok
    end.

init([Children]) ->
    ?d("init -> entry with"
	"~n   Children: ~p", [Children]),
    %% 20 restarts in ten minutes.  If the agent crashes and restarts,
    %% it may very well crash again, because the management application
    %% tries to resend the very same request.  This depends on the resend
    %% strategy used by the management application.
    SupFlags = {one_for_one, 20, 600},
    {ok, {SupFlags, Children}}.


find_max([{{sub_agent, N}, _, _, _} | T], M) when N >= M -> find_max(T, N+1);
find_max([_|T], M) -> find_max(T, M);
find_max([], M) -> M.

find_name([{Name, Pid, _, _} | _T], Pid)-> Name;
find_name([_|T], Pid) -> find_name(T, Pid);
find_name([], _Pid) -> undefined.


% i(F) ->
%     i(F, []).

% i(F, A) ->
%     io:format("~p:~p: " ++ F ++ "~n", [node(),?MODULE|A]).
