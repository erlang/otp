%% -*- erlang-indent-level: 2 -*-
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
%%----------------------------------------------------------------------
%% File    : hipe_rtl_arith_64.erl
%% Created : Feb 2004
%% Purpose : Implements arithmetic for 64-bit target architectures.
%%----------------------------------------------------------------------

-module(hipe_rtl_arith_64).
-export([eval_alu/3, eval_alub/4, eval_cond/3, eval_cond_bits/5]).

-define(BITS, 64).
-define(SIGN_BIT, 63).
-define(WORDMASK,        16#ffffffffffffffff).
-define(MAX_SIGNED_INT,  16#7fffffffffffffff).
-define(MIN_SIGNED_INT, -16#8000000000000000).
-define(MAX_UNSIGNED_INT,16#ffffffffffffffff).

-include("../main/hipe.hrl").    %% for ?EXIT

-include("hipe_rtl_arith.inc").
