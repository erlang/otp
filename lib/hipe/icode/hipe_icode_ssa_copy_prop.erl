%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
%%-------------------------------------------------------------------
%% File        : hipe_icode_ssa_copy_prop.erl
%% Author      : Tobias Lindahl <tobiasl@it.uu.se>
%% Description : Performs copy propagation on SSA form.
%%
%% Created     : 4 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%-------------------------------------------------------------------

-module(hipe_icode_ssa_copy_prop).

%%
%% modules given as parameters
%%
-define(code, hipe_icode).
-define(cfg, hipe_icode_cfg).

%%
%% appropriate include files
%%
-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").
-include("../ssa/hipe_ssa_copy_prop.inc").
