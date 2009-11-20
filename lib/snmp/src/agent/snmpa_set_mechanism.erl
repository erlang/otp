%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(snmpa_set_mechanism).

-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [{do_set, 2}, {do_subagent_set, 1}];
behaviour_info(_) ->
    undefined.
 

%%-----------------------------------------------------------------
%% do_set(MibView, UnsortedVarbinds) 
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% do_subagent_set(Args)
%% 
%% This function is called when a subagent receives a message
%% concerning some set_phase.
%% Mandatory messages for all subagents:
%%   [phase_one, UnsortedVarbinds]
%%   [phase_two, set, UnsortedVarbinds]
%%   [phase_two, undo, UnsortedVarbinds]
%%-----------------------------------------------------------------
