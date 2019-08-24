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

-module(hipe_sparc_liveness_gpr).
-export([analyse/1]).
-export([liveout/2]).

-include("hipe_sparc.hrl").
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_sparc_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_sparc_cfg:postorder(CFG).
cfg_succ(CFG, L) -> hipe_sparc_cfg:succ(CFG, L).
uses(Insn) -> hipe_sparc_defuse:insn_use_gpr(Insn).
defines(Insn) -> hipe_sparc_defuse:insn_def_gpr(Insn).
liveout_no_succ() ->
  ordsets:from_list(lists:map(fun({Reg,Type}) ->
				  hipe_sparc:mk_temp(Reg, Type)
			      end,
			      hipe_sparc_registers:live_at_return())).
