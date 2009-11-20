%% -*- erlang-indent-level: 2 -*-
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
