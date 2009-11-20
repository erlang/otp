%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

-module(hipe_sparc_liveness_fpr).
-export([analyse/1]).
-export([liveout/2]).

-include("hipe_sparc.hrl").
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_sparc_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_sparc_cfg:postorder(CFG).
cfg_succ(CFG, L) -> hipe_sparc_cfg:succ(CFG, L).
uses(Insn) -> hipe_sparc_defuse:insn_use_fpr(Insn).
defines(Insn) -> hipe_sparc_defuse:insn_def_fpr(Insn).
liveout_no_succ() -> [].
