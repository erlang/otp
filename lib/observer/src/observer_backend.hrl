%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-record(etop_info, 
	{now = {0, 0, 0},
	 n_procs = 0,
	 wall_clock = {0, 0},
	 runtime = {0, 0},
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
