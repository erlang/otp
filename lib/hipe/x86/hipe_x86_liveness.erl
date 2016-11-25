%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% x86_liveness -- compute register liveness for x86 CFGs

-ifdef(HIPE_AMD64).
-define(HIPE_X86_LIVENESS,	hipe_amd64_liveness).
-define(HIPE_X86_DEFUSE,	hipe_amd64_defuse).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).
-else.
-define(HIPE_X86_LIVENESS,	hipe_x86_liveness).
-define(HIPE_X86_DEFUSE,	hipe_x86_defuse).
-define(HIPE_X86_REGISTERS,	hipe_x86_registers).
-endif.

-module(?HIPE_X86_LIVENESS).

-export([analyse/1]).
-export([liveout/2]).
-export([uses/1, defines/1]).	% used in hipe_*_spill_restore modules

-include("../x86/hipe_x86.hrl").  % ../x86/ is needed when included in amd64
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_x86_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_x86_cfg:postorder(CFG).
cfg_succ(CFG, L) -> hipe_x86_cfg:succ(CFG, L).
uses(Insn) -> ?HIPE_X86_DEFUSE:insn_use(Insn).
defines(Insn) -> ?HIPE_X86_DEFUSE:insn_def(Insn).
liveout_no_succ() ->
  ordsets:from_list(lists:map(fun({Reg,Type}) ->
				  hipe_x86:mk_temp(Reg, Type)
			      end,
			      ?HIPE_X86_REGISTERS:live_at_return())).

-ifdef(DEBUG_LIVENESS).
cfg_labels(CFG) -> hipe_x86_cfg:labels(CFG).
cfg_bb_add(CFG,L,NewBB) -> hipe_x86_cfg:bb_add(CFG,L,NewBB).
mk_comment(Text) -> hipe_x86:mk_comment(Text).
-endif.
