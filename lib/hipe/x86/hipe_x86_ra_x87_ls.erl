%% $Id$
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%% Linear Scan register allocator for x87

-ifdef(HIPE_AMD64).
-define(HIPE_X86_RA_X87_LS, hipe_amd64_ra_x87_ls).
-define(HIPE_X86_SPECIFIC_X87, hipe_amd64_specific_x87).
-define(HIPE_X86_PP, hipe_amd64_pp).
-define(HIPE_X86_RA_LS, hipe_amd64_ra_ls).
-else.
-define(HIPE_X86_RA_X87_LS, hipe_x86_ra_x87_ls).
-define(HIPE_X86_SPECIFIC_X87, hipe_x86_specific_x87).
-define(HIPE_X86_PP, hipe_x86_pp).
-define(HIPE_X86_RA_LS, hipe_x86_ra_ls).
-endif.

-module(?HIPE_X86_RA_X87_LS).
-export([ra/2]).

%%-define(DEBUG,1).

-define(HIPE_INSTRUMENT_COMPILER, false). %% Turn off instrumentation.
-include("../main/hipe.hrl").

ra(Defun, Options) ->
    ?inc_counter(ra_calls_counter,1),
    CFG = hipe_x86_cfg:init(Defun),
    %% ?inc_counter(ra_caller_saves_counter,count_caller_saves(CFG)),
    SpillIndex = 0,
    SpillLimit = ?HIPE_X86_SPECIFIC_X87:number_of_temporaries(CFG),
    ?inc_counter(bbs_counter, length(hipe_x86_cfg:labels(CFG))),

    ?inc_counter(ra_iteration_counter,1),
    %% ?HIPE_X86_PP:pp(Defun),
    Cfg = hipe_x86_cfg:init(Defun), % XXX: didn't we just compute this above?

    {Coloring,NewSpillIndex} =
	?HIPE_X86_RA_LS:regalloc(Cfg,
				 ?HIPE_X86_SPECIFIC_X87:allocatable(),
				 [hipe_x86_cfg:start_label(Cfg)],
				 SpillIndex, SpillLimit, Options,
				 ?HIPE_X86_SPECIFIC_X87),

    ?add_spills(Options, NewSpillIndex),
    {Defun, Coloring, NewSpillIndex}.
