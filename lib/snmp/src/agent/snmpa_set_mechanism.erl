%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
