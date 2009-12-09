%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
	 num_fun,
	 mem_tot,
	 mem_max,
	 instr_info}).

-record(proc,
	{pid,
	 name,
	 init_func,
	 parent,
	 start_time,
	 state,
	 current_func,
	 msg_q_len,
	 msg_q,
	 last_calls,
	 links,
	 prog_count,
	 cp,
	 arity,
	 dict,
	 debug_dict,
	 reds,
	 num_heap_frag,
	 heap_frag_data,
	 stack_heap,
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
	 stack_dump}).

-record(port,
	{id,
	 slot,
	 connected,
	 links,
	 controls}).

-record(ets_table,
	{pid,
	 slot,
	 id,
	 name,
	 type,
	 buckets,
	 size,
	 memory}).

-record(timer,
	{pid,
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
	 controller,
	 creation,
	 remote_links,
	 remote_mon,
	 remote_mon_by,
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
	 used,
	 limit,
	 rate}).
