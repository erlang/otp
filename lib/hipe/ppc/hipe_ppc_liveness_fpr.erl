%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_ppc_liveness_fpr).
-export([analyse/1]).
-export([liveout/2]).

-include("hipe_ppc.hrl").
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_ppc_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_ppc_cfg:postorder(CFG).
cfg_succ(CFG, L) -> hipe_ppc_cfg:succ(CFG, L).
uses(Insn) -> hipe_ppc_defuse:insn_use_fpr(Insn).
defines(Insn) -> hipe_ppc_defuse:insn_def_fpr(Insn).
liveout_no_succ() -> [].
