%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
-module(snmpa_set_mechanism).

%% -export([behaviour_info/1]).
 
%% behaviour_info(callbacks) ->
%%     [{do_set, 2}, {do_subagent_set, 1}];
%% behaviour_info(_) ->
%%     undefined.


%%-----------------------------------------------------------------
%% do_set(MibView, UnsortedVarbinds) 
%%-----------------------------------------------------------------

-callback do_set(MibView, UnsortedVBs) ->
    {noError, 0} | {ErrStatus, ErrIndex} when
      MibView     :: snmp_view_based_acm_mib:mibview(),
      UnsortedVBs :: [snmp:varbind()],
      ErrStatus   :: snmp:error_status(),
      ErrIndex    :: snmp:error_index().


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

%% -callback do_subagent_set(Args) ->
%%     {noError, 0} | {ErrStatus, ErrIndex} when
%%       Args        :: [phase_one, UnsortedVBs] |
%%                      [phase_two, set, UnsortedVBs] |
%%                      [phase_two, undo, UnsortedVBs],
%%       ErrStatus   :: snmp:error_status(),
%%       ErrIndex    :: snmp:error_index(),
%%       UnsortedVBs :: [snmp:varbind()].

-callback do_subagent_set(Args) ->
    {noError, 0} | {ErrStatus, ErrIndex} when
      Args      :: list(),
      ErrStatus :: snmp:error_status(),
      ErrIndex  :: snmp:error_index().
