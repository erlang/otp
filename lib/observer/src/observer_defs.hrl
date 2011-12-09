%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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

-record(attrs, {even, odd, deleted, changed, searched}).
-define(EVEN(Row), ((Row rem 2) =:= 0)).
-define(BG_EVEN,    {230,230,250}).
-define(BG_ODD,     {255,255,255}).
-define(BG_DELETED, {100,100,100}).
-define(FG_DELETED, {240,30,30}).
-define(BG_SEARCHED,{235,215,90}).
-define(BG_CHANGED, {230,230,250}).

-define(LCTRL_WDECR, 4). %% Remove some pixels in column width to avoid creating unnecessary scrollbar
