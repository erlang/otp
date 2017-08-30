%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

