%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
