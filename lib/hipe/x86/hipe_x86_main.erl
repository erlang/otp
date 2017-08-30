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

-ifdef(HIPE_AMD64).
-define(HIPE_X86_MAIN, hipe_amd64_main).
-define(RTL_TO_X86, rtl_to_amd64). % XXX: kill this crap
-define(HIPE_RTL_TO_X86, hipe_rtl_to_amd64).
-define(HIPE_X86_RA, hipe_amd64_ra).
-define(HIPE_X86_FRAME, hipe_amd64_frame).
-define(HIPE_X86_PP, hipe_amd64_pp).
-define(X86TAG, amd64). % XXX: kill this crap
-define(X86STR, "amd64").
-define(HIPE_X86_SPILL_RESTORE, hipe_amd64_spill_restore).
-else.
-define(HIPE_X86_MAIN, hipe_x86_main).
-define(RTL_TO_X86, rtl_to_x86). % XXX: kill this crap
-define(HIPE_RTL_TO_X86, hipe_rtl_to_x86).
-define(HIPE_X86_RA, hipe_x86_ra).
-define(HIPE_X86_FRAME, hipe_x86_frame).
-define(HIPE_X86_PP, hipe_x86_pp).
-define(X86TAG, x86). % XXX: kill this crap
-define(X86STR, "x86").
-define(HIPE_X86_SPILL_RESTORE, hipe_x86_spill_restore).
-endif.

-module(?HIPE_X86_MAIN).
-export([?RTL_TO_X86/3]). % XXX: change to 'from_rtl' to avoid $ARCH substring

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

?RTL_TO_X86(MFA, RTL, Options) ->
  Translated = ?option_time(?HIPE_RTL_TO_X86:translate(RTL),
			    "RTL-to-"?X86STR, Options),
  SpillRest = 
    case proplists:get_bool(caller_save_spill_restore, Options) of
      true ->
	?option_time(?HIPE_X86_SPILL_RESTORE:spill_restore(Translated, Options),
		     ?X86STR" spill restore", Options);
      false ->
	Translated
    end,
  Allocated  = ?option_time(?HIPE_X86_RA:ra(SpillRest, Options),
			    ?X86STR" register allocation", Options),
  Framed     = ?option_time(?HIPE_X86_FRAME:frame(Allocated, Options), 
			    ?X86STR" frame", Options),
  Finalised  = ?option_time(hipe_x86_postpass:postpass(Framed, Options),
			    ?X86STR" finalise", Options),
  ?HIPE_X86_PP:optional_pp(Finalised, MFA, Options),
  {native, ?X86TAG, {unprofiled, Finalised}}.
