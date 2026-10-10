%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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

-if(?MODULE =/= beam_ssa).
-import_record(beam_ssa,
               [b_module, b_function, b_blk, b_set,
                b_ret, b_br, b_switch,
                b_var, b_literal, b_remote, b_local]).
-endif.

%% This is a psuedo-block used to express that certain instructions and BIFs
%% throw exceptions on failure. The code generator rewrites all branches to
%% this block to {f,0} which causes the instruction to throw an exception
%% instead of branching.
%%
%% Since this is not an ordinary block, it's illegal to merge it with other
%% blocks, and jumps are only valid when we know that an exception will be
%% thrown by the operation that branches here; the *block itself* does not
%% throw an exception.
-define(EXCEPTION_BLOCK, 1).
