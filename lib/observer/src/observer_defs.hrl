%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-record(match_spec, {name = "",
		     term = [],
		     str  = [],
		     func = ""}).

-record(tpattern, {m, fa, ms}).

-record(traced_func, {func_name, %atom
		      arity, %integer
		      match_spec = #match_spec{}}).

-record(create_menu,
	{id,
	 text,
	 help = [],
	 type = append,
	 check = false
	}).

-record(colors, {fg, even, odd}).
-record(attrs, {even, odd, searched, deleted, changed_odd, changed_even, new_odd, new_even}).
-define(EVEN(Row), ((Row rem 2) =:= 0)).
-define(BG_EVEN,    {230,230,250}).
-define(BG_ODD,     {255,255,255}).
-define(BG_DELETED, {100,100,100}).
-define(FG_DELETED, {230,230,230}).
-define(BG_SEARCHED,{235,215,90}).
-define(BG_CHANGED, {184,207,184}).
-define(BG_NEW,     {123,168,123}).

-define(LCTRL_WDECR, 4). %% Remove some pixels in column width to avoid creating unnecessary scrollbar

-define(SASH_STYLE, ?wxSP_LIVE_UPDATE bor ?wxSP_NOBORDER bor ?wxSP_3DSASH).

-define(DISP_FREQ, 10). %% per second
-define(FETCH_DATA, 2). %% per second
-define(DISP_SECONDS, 60).

-record(ti, {tick=0, disp=?DISP_FREQ/?FETCH_DATA, fetch=?FETCH_DATA, secs=?DISP_SECONDS}).

-record(win, {name, panel, size, geom,
	      graphs=[], no_samples=0,
	      max, state,
	      info=[]}).
