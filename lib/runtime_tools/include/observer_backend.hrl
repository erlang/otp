%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2013. All Rights Reserved.
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

-record(etop_info, 
	{now = {0, 0, 0},
	 n_procs = 0,
	 wall_clock,
	 runtime,
	 run_queue = 0,
	 alloc_areas = [],
	 memi = [{total, 0},
		 {processes, 0}, 
		 {ets, 0},
		 {atom, 0},
		 {code, 0},
		 {binary, 0}],
	 procinfo = []
	}).

-record(etop_proc_info,
	{pid,
	 mem=0,
	 reds=0,
	 name,
	 runtime=0,
	 cf,
	 mq=0}).
