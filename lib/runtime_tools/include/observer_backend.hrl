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
