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
-define(space, undefined).
-define(unknown, "unknown").
-define(r16b01_dump_vsn, [0,2]). % =erl_crash_dump:0.2

-record(menu_item,{index,picture,text,depth,children,state,target}).

-record(general_info,
	{created,
	 slogan,
	 system_vsn,
	 compile_time,
	 taints,
	 node_name,
	 num_atoms,
	 num_procs,
	 num_ets,
	 num_timers,
	 num_fun,
	 mem_tot,
	 mem_max,
	 instr_info,
	 thread
	}).

-record(proc,
	%% Initial data according to the follwoing:
	%% 
	%% msg_q_len, reds, memory and stack_heap are integers because it must
	%% be possible to sort on them. All other fields are strings
	%%
	%% for old dumps start_time, parent and number of heap framents
	%% do not exist
	%%
	%% current_func can be both "current function" and
	%% "last scheduled in for"
	%%
	%% stack_dump, message queue and dictionaries should only be 
	%% displayed as a link to "Expand" (if dump is from OTP R9B 
	%% or newer)
	{pid,
	 name,
	 init_func,
	 parent=?unknown,
	 start_time=?unknown,
	 state,
	 current_func,
	 msg_q_len=0,
	 msg_q,
	 last_calls,
	 links,
	 monitors,
	 mon_by,
	 prog_count,
	 cp,
	 arity,
	 dict,
	 reds=0,
	 num_heap_frag=?unknown,
	 heap_frag_data,
	 stack_heap=0,
	 old_heap,
	 heap_unused,
	 old_heap_unused,
	 new_heap_start,
	 new_heap_top,
	 stack_top,
	 stack_end,
	 old_heap_start,
	 old_heap_top,
	 old_heap_end,
	 memory,
	 stack_dump,
	 run_queue=?unknown,
	 int_state
	}).

-record(port,
	{id,
	 slot,
	 connected,
	 links,
	 name,
	 monitors,
	 controls}).

-record(sched,
	{name,
	 process,
	 port,
	 run_q=0,
	 port_q=0,
	 details=#{}
	}).



-record(ets_table,
	{pid,
	 slot,
	 id,
	 name,
	 data_type="hash",
	 buckets="-",
	 size,
	 memory,
	 details= #{}
	}).

-record(timer,
	{pid,
	 name,
	 msg,
	 time}).

-record(fu,
	{module,
	 uniq,
	 index,
	 address,
	 native_address,
	 refc}).

-record(nod,
	{name,
	 channel,
	 conn_type,
	 controller,
	 creation,
	 remote_links=[],
	 remote_mon=[],
	 remote_mon_by=[],
	 error}).

-record(loaded_mod,
	{mod,
	 current_size,
	 current_attrib,
	 current_comp_info,
	 old_size,
	 old_attrib,
	 old_comp_info}).

-record(hash_table,
	{name,
	 size,
	 used,
	 objs,
	 depth}).

-record(index_table,
	{name,
	 size,
	 limit,
	 used,
	 rate,
	 entries}).
