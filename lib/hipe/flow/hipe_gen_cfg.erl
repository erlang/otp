%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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

-module(hipe_gen_cfg).

-export([start_label/1,
	 succ/2,
         pred/2
	]).

%%-define(DO_ASSERT, true).
-define(GEN_CFG, true).	    % needed for cfg.inc

-include("../main/hipe.hrl").
-include("cfg.hrl").

-spec succ(cfg(), cfg_lbl()) -> [cfg_lbl()].
-spec pred(cfg(), cfg_lbl()) -> [cfg_lbl()].

-include("cfg.inc").

