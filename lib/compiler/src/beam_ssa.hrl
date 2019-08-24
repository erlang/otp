%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

-record(b_module, {anno=#{} :: beam_ssa:anno(),
                   name :: module(),
                   exports :: [{atom(),arity()}],
                   attributes :: list(),
                   body :: [beam_ssa:b_function()]}).
-record(b_function, {anno=#{} :: beam_ssa:anno(),
                     args :: [beam_ssa:b_var()],
                     bs :: #{beam_ssa:label():=beam_ssa:b_blk()},
                     cnt :: beam_ssa:label()}).

-record(b_blk, {anno=#{} :: beam_ssa:anno(),
                is :: [beam_ssa:b_set()],
                last :: beam_ssa:terminator()}).
-record(b_set, {anno=#{} :: beam_ssa:anno(),
                dst=none :: 'none'|beam_ssa:b_var(),
                op :: beam_ssa:op(),
                args=[] :: [beam_ssa:argument()]}).

%% Terminators.
-record(b_ret, {anno=#{} :: beam_ssa:anno(),
                arg :: beam_ssa:value()}).

-record(b_br, {anno=#{},
               bool :: beam_ssa:value(),
               succ :: beam_ssa:label(),
               fail :: beam_ssa:label()}).

-record(b_switch, {anno=#{} :: beam_ssa:anno(),
                   arg :: beam_ssa:value(),
                   fail :: beam_ssa:label(),
                   list :: [{beam_ssa:b_literal(),beam_ssa:label()}]}).

%% Values.
-record(b_var, {name :: beam_ssa:var_name()}).

-record(b_literal, {val :: beam_ssa:literal_value()}).

-record(b_remote, {mod   :: beam_ssa:value(),
                   name  :: beam_ssa:value(),
                   arity :: non_neg_integer()}).

-record(b_local, {name  :: beam_ssa:b_literal(),
                  arity :: non_neg_integer()}).

%% If this block exists, it calls erlang:error(badarg).
-define(BADARG_BLOCK, 1).
