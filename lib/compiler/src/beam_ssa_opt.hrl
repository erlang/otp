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
         exported = true :: boolean(),

         %% The inferred types of each argument (as opposed to parameter),
         %% indexed by call site.
         %%
         %% This is more effective than the naive approach of joining into a
         %% "parameter_type" as we go as it lets us narrow parameter types
         %% without having to visit all callers on each pass, which helps a lot
         %% when dealing with co-recursive functions.
         arg_types = [] :: list(arg_type_map()),

         %% The success types of this function, grouping return values by their
         %% argument types at the time of return.
         %%
         %% This gives us more precise types than a naive join of all returned
         %% values, as we can rule out the cases where the arguments are
         %% incompatible with the ones we're passing.
         %%
         %% Note that the argument types are those seen on successful return,
         %% they do not cover all types that are provided to the function.
         succ_types = [] :: success_type_set()}).

-type arg_key() :: {CallerId :: func_id(),
                    CallDst :: beam_ssa:b_var()}.
-type arg_type_map() :: #{ arg_key() => term() }.

-type call_self() :: {call_self, ArgTypes :: [term()]}.
-type success_type_set() :: [{ArgTypes :: [term()],
                              RetType :: call_self() | term()}].

%% Per-function metadata used by various optimization passes to perform
%% module-level optimization. If a function is absent it means that
%% module-level optimization has been turned off for said function.
-type func_id() :: beam_ssa:b_local().
-type func_info_db() :: #{ func_id() => #func_info{} }.

-record(opt_st, {ssa :: [{beam_ssa:label(),beam_ssa:b_blk()}] |
                        beam_ssa:block_map(),
                 args :: [beam_ssa:b_var()],
                 cnt :: beam_ssa:label(),
                 anno :: beam_ssa:anno()}).
-type st_map() :: #{ func_id() => #opt_st{} }.
