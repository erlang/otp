%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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
-define(space, "&nbsp;").
-define(unknown, "unknown").
-define(r16b01_dump_vsn, [0,2]). % =erl_crash_dump:0.2

-record(menu_item,{index,picture,text,depth,children,state,target}).

-record(general_info,
	{created,
	 slogan=?space,
	 system_vsn=?space,
	 compile_time=?space,
	 taints=?space,
	 node_name=?space,
	 num_atoms=?space,
	 num_procs=?space,
	 num_ets=?space,
	 num_timers=?space,
	 num_fun=?space,
	 mem_tot=?space,
	 mem_max=?space,
	 instr_info=?space}).

-record(proc,
	%% Initial data according to the follwoing:
	%% 
	%% msg_q_len, reds and stack_heap are integers because it must 
	%% be possible to sort on them. All other fields are strings
	%%
	%% for old dumps start_time, parent and number of heap frament
	%% does not exist
	%%
	%% current_func can be both "current function" and
	%% "last scheduled in for"
	%%
	%% stack_dump, message queue and dictionaries should only be 
	%% displayed as a link to "Expand" (if dump is from OTP R9B 
	%% or newer)
	{pid,
	 name=?space,
	 init_func=?space,
	 parent=?unknown,
	 start_time=?unknown,
	 state=?space,
	 current_func={"Current Function",?space},
	 msg_q_len=0,
	 msg_q=?space,
	 last_calls=?space,
	 links=?space,
	 prog_count=?space,
	 cp=?space,
	 arity=?space,
	 dict=?space,
	 debug_dict=?space,
	 reds=0,
	 num_heap_frag=?unknown,
	 heap_frag_data=?space,
	 stack_heap=0,
	 old_heap=?space,
	 heap_unused=?space,
	 old_heap_unused=?space,
	 new_heap_start=?space,
	 new_heap_top=?space,
	 stack_top=?space,
	 stack_end=?space,
	 old_heap_start=?space,
	 old_heap_top=?space,
	 old_heap_end=?space,
	 memory,
	 stack_dump=?space}).

-record(port,
	{id,
	 slot=?space,
	 connected=?space,
	 links=?space,
	 name=?space,
	 monitors=?space,
	 controls=?space}).

-record(ets_table,
	{pid,
	 slot=?space,
	 id=?space,
	 name=?space,
	 type="hash",
	 buckets=?space,
	 size=?space,
	 memory=?space}).

-record(timer,
	{pid,
	 msg=?space,
	 time=?space}).

-record(fu,
	{module=?space,
	 uniq=?space,
	 index=?space,
	 address=?space,
	 native_address=?space,
	 refc=?space}).

-record(nod,
	{name=?space,
	 channel,
	 controller=?space,
	 creation=?space,
	 remote_links=?space,
	 remote_mon=?space,
	 remote_mon_by=?space,
	 error=?space}).

-record(loaded_mod,
	{mod,
	 current_size=?space,
	 current_attrib=?space,
	 current_comp_info=?space,
	 old_size=?space,
	 old_attrib=?space,
	 old_comp_info=?space}).

-record(hash_table,
	{name,
	 size=?space,
	 used=?space,
	 objs=?space,
	 depth=?space}).

-record(index_table,
	{name,
	 size=?space,
	 used=?space,
	 limit=?space,
	 rate=?space,
	 entries=?space}).
