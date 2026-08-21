%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% The beam_ssa_alias and beam_ssa_ss modules are both used by the
%% alias analysis pass of the compiler, where beam_ssa_ss provides the
%% underlying database manipulated by the beam_ssa_alias
%% module. Debugging beam_ssa_alias is simplified when the module can
%% print out the internal state of beam_ssa_ss, but as that code is
%% redundant otherwise it is ifdefed-out, this header exist in order
%% to avoid having to modify multiple modules when toggling debugging
%% traces for beam_ssa_alias and beam_ssa_ss.

%% Uncomment the following to get trace printouts.

%% -define(DEBUG_ALIAS, true). % For beam_ssa_alias

%% -define(DEBUG_SS, true). % For beam_ssa_ss

%% Uncomment the following to check that all invariants for the state
%% hold. These checks are expensive and not enabled by default.

%% -define(SS_EXTRA_ASSERTS, true).

-if(defined(DEBUG_ALIAS) orelse defined(DEBUG_SS)
    orelse defined(SS_EXTRA_ASSERTS)).
-define(PROVIDE_DUMP, true).
-endif.
