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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% ====================================================================
%%  Filename : 	hipe_rtl_arith_32.erl
%%  Module   :	hipe_rtl_arith_32
%%  Purpose  :  To implement 32-bit RTL-arithmetic 
%%  Notes    :  The arithmetic works on 32-bit signed and unsigned
%%              integers.
%%              The implementation is taken from the implementation
%%              of arithmetic on SPARC.
%%              XXX: This code is seldom used, and hence also
%%                   seldom tested. 
%%                   Look here for strange bugs appearing when
%%                   turning on rtl_prop.
%%
%%  History  :	* 2002-10-23 Erik Stenman (happi@it.uu.se): Created.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_arith_32).

-export([eval_alu/3, eval_alub/4, eval_cond/3, eval_cond_bits/5]).

-define(BITS, 32).
-define(SIGN_BIT, 31).
-define(WORDMASK,         16#ffffffff).
-define(MAX_SIGNED_INT,   16#7fffffff).
-define(MIN_SIGNED_INT,  -16#80000000).
-define(MAX_UNSIGNED_INT, 16#ffffffff).

-include("../main/hipe.hrl").    %% for ?EXIT

-include("hipe_rtl_arith.inc").
