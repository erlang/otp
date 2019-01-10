%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-include("beam_ssa.hrl").

-record(func_info,
        {%% Local calls going in/out of this function.
         in = ordsets:new() :: ordsets:ordset(func_id()),
         out = ordsets:new() :: ordsets:ordset(func_id()),

         %% Whether the function is exported or not; some optimizations may
         %% need to be suppressed if it is.
         exported = true :: boolean()}).

%% Per-function metadata used by various optimization passes to perform
%% module-level optimization. If a function is absent it means that
%% module-level optimization has been turned off for said function.

-type func_id() :: beam_ssa:b_local().
-type func_info_db() :: #{ func_id() => #func_info{} }.
