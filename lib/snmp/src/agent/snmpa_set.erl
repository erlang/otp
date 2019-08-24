%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(snmpa_set).

-behaviour(snmpa_set_mechanism).

-define(VMODULE,"SET").
-include("snmp_verbosity.hrl").


%%%-----------------------------------------------------------------
%%% This module implements a simple, basic atomic set mechanism.
%%%-----------------------------------------------------------------
%%% Table of contents
%%% =================
%%% 1. SET REQUEST
%%% 1.1 SET phase one
%%% 1.2 SET phase two
%%% 2. Misc functions
%%%-----------------------------------------------------------------

%% External exports
-export([do_set/2, do_subagent_set/1]).

%%%-----------------------------------------------------------------
%%% 1. SET REQUEST
%%%
%%% 1) Perform set_phase_one for all own vars
%%% 2) Perform set_phase_one for all SAs
%%%    IF nok THEN 2.1 ELSE 3
%%% 2.1) Perform set_phase_two(undo) for all SAs that have performed
%%%      set_phase_one.
%%% 3) Perform set_phase_two for all own vars
%%% 4) Perform set_phase_two(set) for all SAs
%%%    IF nok THEN 4.1 ELSE 5
%%% 4.1) Perform set_phase_two(undo) for all SAs that have performed
%%%      set_phase_one but not set_phase_two(set).
%%% 5) noError
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% First of all - validate MibView for all varbinds. In this way
%% we don't have to send the MibView to all SAs for validation.
%%-----------------------------------------------------------------
do_set(MibView, UnsortedVarbinds) ->
    ?vtrace("do set with"
	    "~n   MibView: ~p",[MibView]),
    case snmpa_acm:validate_all_mib_view(UnsortedVarbinds, MibView) of
	true ->
	    {MyVarbinds , SubagentVarbinds} = 
		sort_varbindlist(UnsortedVarbinds),
	    case set_phase_one(MyVarbinds, SubagentVarbinds) of
		{noError, 0} -> set_phase_two(MyVarbinds, SubagentVarbinds);
		{Reason, Index} -> {Reason, Index}
	    end;
	{false, Index} ->
	    {noAccess, Index}
    end.

%%-----------------------------------------------------------------
%% This function is called when a subagents receives a message
%% concerning some set_phase.
%% Mandatory messages for all subagents:
%%   [phase_one, UnsortedVarbinds]
%%   [phase_two, set, UnsortedVarbinds]
%%   [phase_two, undo, UnsortedVarbinds]
%%-----------------------------------------------------------------
do_subagent_set([phase_one, UnsortedVarbinds]) ->
    ?vtrace("do subagent set, phase one",[]),
    {MyVarbinds, SubagentVarbinds} = sort_varbindlist(UnsortedVarbinds),
    set_phase_one(MyVarbinds, SubagentVarbinds);
do_subagent_set([phase_two, State, UnsortedVarbinds]) ->
    ?vtrace("do subagent set, phase two",[]),
    {MyVarbinds, SubagentVarbinds} = sort_varbindlist(UnsortedVarbinds),
    set_phase_two(State, MyVarbinds, SubagentVarbinds).
    
%%%-----------------------------------------------------------------
%%% 1.1 SET phase one
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: set_phase_one/3
%% Purpose: First, do set_phase_one for my own variables (i.e. 
%%          variables handled by this agent). Then, do set_phase_one
%%          for all subagents. If any SA failed, do set_phase_two
%%          (undo) for all SA that have done set_phase_one.
%% Returns: {noError, 0} | {ErrorStatus, Index}
%%-----------------------------------------------------------------
set_phase_one(MyVarbinds, SubagentVarbinds) ->
    ?vtrace("set phase one: "
	    "~n   MyVarbinds:       ~p"
	    "~n   SubagentVarbinds: ~p",
	    [MyVarbinds, SubagentVarbinds]),
    case set_phase_one_my_variables(MyVarbinds) of
	{noError, 0} ->
	    case set_phase_one_subagents(SubagentVarbinds, []) of
		{noError, 0} ->
		    {noError, 0};
		{{ErrorStatus, Index}, PerformedSubagents} ->
		    case set_phase_two_undo(MyVarbinds, PerformedSubagents) of
			{noError, 0} ->
			    {ErrorStatus, Index};
			{WorseErrorStatus, WorseIndex} ->
			    {WorseErrorStatus, WorseIndex}
		    end
	    end;
	{ErrorStatus, Index} ->
	    {ErrorStatus, Index}
    end.

set_phase_one_my_variables(MyVarbinds) ->
    ?vtrace("my variables set, phase one:"
	    "~n   ~p",[MyVarbinds]),
    case snmpa_set_lib:is_varbinds_ok(MyVarbinds) of
	{noError, 0} ->
	    snmpa_set_lib:consistency_check(MyVarbinds);
	{ErrorStatus, Index} ->
	    {ErrorStatus, Index}
    end.

%%-----------------------------------------------------------------
%% Loop all subagents, and perform set_phase_one for them.
%%-----------------------------------------------------------------
set_phase_one_subagents([{SubAgentPid, SAVbs}|SubagentVarbinds], Done) ->
    {_SAOids, Vbs} = sa_split(SAVbs),
    case (catch snmpa_agent:subagent_set(SubAgentPid, [phase_one, Vbs])) of
	{noError, 0} ->
	    set_phase_one_subagents(SubagentVarbinds, 
				    [{SubAgentPid, SAVbs} | Done]);
	{'EXIT', Reason} ->
	    user_err("Lost contact with subagent (set phase_one)"
		     "~n~w. Using genErr", [Reason]),
	    {{genErr, 0}, Done};
	{ErrorStatus, ErrorIndex} ->
	    {{ErrorStatus, ErrorIndex}, Done}
    end;
set_phase_one_subagents([], _Done) ->
    {noError, 0}.

%%%-----------------------------------------------------------------
%%% 1.2 SET phase two
%%%-----------------------------------------------------------------
%% returns:  {ErrStatus, ErrIndex}
set_phase_two(MyVarbinds, SubagentVarbinds) ->
    ?vtrace("set phase two: "
	    "~n   MyVarbinds:       ~p"
	    "~n   SubagentVarbinds: ~p",
	    [MyVarbinds, SubagentVarbinds]),
    case snmpa_set_lib:try_set(MyVarbinds) of
	{noError, 0} ->
            ?vtrace("set phase two: (local) varbinds set ok", []),
	    set_phase_two_subagents(SubagentVarbinds);
	{ErrorStatus, ErrorIndex} ->
            ?vlog("set phase two: (local) varbinds set failed"
                  "~n   ErrorStatus: ~p" 
                  "~n   ErrorIndex:  ~p", [ErrorStatus, ErrorIndex]),
	    set_phase_two_undo_subagents(SubagentVarbinds),
	    {ErrorStatus, ErrorIndex}
    end.

%%-----------------------------------------------------------------
%% This function is called for each phase_two state in the
%% subagents. The undo state just pass undo along to each of its
%% subagents.
%%-----------------------------------------------------------------
set_phase_two(set, MyVarbinds, SubagentVarbinds) ->
    set_phase_two(MyVarbinds, SubagentVarbinds);
set_phase_two(undo, MyVarbinds, SubagentVarbinds) ->
    set_phase_two_undo(MyVarbinds, SubagentVarbinds).

%%-----------------------------------------------------------------
%% Loop all subagents, and perform set_phase_two(set) for them.
%% If any fails, perform set_phase_two(undo) for the not yet
%% called SAs.
%%-----------------------------------------------------------------
set_phase_two_subagents([{SubAgentPid, SAVbs} | SubagentVarbinds]) ->
    {_SAOids, Vbs} = sa_split(SAVbs),
    case catch snmpa_agent:subagent_set(SubAgentPid, [phase_two, set, Vbs]) of
	{noError, 0} ->
            ?vtrace("set phase two: subagent ~p varbinds set ok", [SubAgentPid]),
	    set_phase_two_subagents(SubagentVarbinds);
	{'EXIT', Reason} ->
	    user_err("Lost contact with subagent (set)~n~w. Using genErr", 
		     [Reason]),
	    set_phase_two_undo_subagents(SubagentVarbinds),
	    {genErr, 0};
	{ErrorStatus, ErrorIndex} ->
            ?vlog("set phase two: subagent ~p varbinds set failed"
                  "~n   ErrorStatus: ~p"
                  "~n   ErrorIndex:  ~p", [SubAgentPid, ErrorStatus, ErrorIndex]),
	    set_phase_two_undo_subagents(SubagentVarbinds),
	    {ErrorStatus, ErrorIndex}
    end;
set_phase_two_subagents([]) ->
    ?vtrace("set phase two: subagent(s) set ok", []),
    {noError, 0}.

%%-----------------------------------------------------------------
%% This function undos phase_one, own and subagent.
%%-----------------------------------------------------------------
set_phase_two_undo(MyVarbinds, SubagentVarbinds) ->
    case set_phase_two_undo_my_variables(MyVarbinds) of
	{noError, 0} ->
	    set_phase_two_undo_subagents(SubagentVarbinds);
	{ErrorStatus, Index} ->
	    set_phase_two_undo_subagents(SubagentVarbinds),
	    {ErrorStatus, Index}
    end.

set_phase_two_undo_my_variables(MyVarbinds) ->
    snmpa_set_lib:undo_varbinds(MyVarbinds).

set_phase_two_undo_subagents([{SubAgentPid, SAVbs} | SubagentVarbinds]) ->
    {_SAOids, Vbs} = sa_split(SAVbs),
    case catch snmpa_agent:subagent_set(SubAgentPid, [phase_two, undo, Vbs]) of
	{noError, 0} ->
	    set_phase_two_undo_subagents(SubagentVarbinds);
	{'EXIT', Reason} ->
	    user_err("Lost contact with subagent (undo)~n~w. Using genErr", 
		     [Reason]),
	    {genErr, 0};
	{ErrorStatus, ErrorIndex} ->
	    {ErrorStatus, ErrorIndex}
    end;
set_phase_two_undo_subagents([]) ->
    {noError, 0}.

%%%-----------------------------------------------------------------
%%% 2. Misc functions
%%%-----------------------------------------------------------------
sort_varbindlist(Varbinds) ->
    snmpa_svbl:sort_varbindlist(get(mibserver), Varbinds).

sa_split(SubagentVarbinds) ->
    snmpa_svbl:sa_split(SubagentVarbinds).


user_err(F, A) ->
    snmpa_error:user_err(F, A).
