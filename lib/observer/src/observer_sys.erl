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

-module(observer_sys).

-export([node_info/0, node_name_str/1, no_procs_str/1, no_cpu_str/1,
	 no_cpu_available_str/1, no_cpu_online_str/1, tot_alloc_str/1,
	 proc_used_str/1, proc_alloc_str/1, atom_used_str/1, atom_alloc_str/1,
	 binary_alloc_str/1, code_alloc_str/1, ets_alloc_str/1]).

-record(node_info, {node_name,
		    no_procs, % number of processes
		    no_cpu, % number of logical cpu's
		    no_cpu_available, %number of logical cpu's available
		    no_cpu_online, % number of logical cpu's online
		    tot_alloc, % total memory allocated
		    proc_used, % memory used by processes
		    proc_alloc, % memory alloc by processes,
		    atom_used, % memory used by atoms
		    atom_alloc, % memory allocated by atoms
		    binary_alloc, % memory allocated for binaries
		    code_alloc, % memory allocated by code
		    ets_alloc}).% memory allocated by ets


%%%%%%%%%%%%%%%%%%%%%%% functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_info() ->
    #node_info{node_name = node_name(),
	       no_procs = process_count(),
	       no_cpu = logical_cpus(),
	       no_cpu_available = logical_cpus_available(),
	       no_cpu_online = logical_cpus_online(),
	       tot_alloc = total_alloc(),
	       proc_used = processes_used(),
	       proc_alloc = processes_alloc(),
	       atom_used = atom_used(),
	       atom_alloc = atom_alloc(),
	       binary_alloc = binary_alloc(),
	       code_alloc = code_alloc(),
	       ets_alloc = ets_alloc()
	      }.

node_name() ->
    node().

process_count() ->
    erlang:system_info(process_count).

logical_cpus() ->
    erlang:system_info(logical_processors). % detected number of logical cpus configured on system

logical_cpus_available() -> % detected number of logical cpus available to erlang runtime system
    erlang:system_info(logical_processors_available).

logical_cpus_online() -> % detected number of logical cpus online on system
    erlang:system_info(logical_processors_online).

total_alloc() ->
    erlang:memory(total). % total amount of memory currently allocated

processes_used() -> % amount of memory currently used by the erlang processes
    erlang:memory(processes_used).

processes_alloc() -> % allocated by erlang processes
    erlang:memory(processes).

atom_used() ->  % amount of memory used for atoms
    erlang:memory(atom_used).

atom_alloc() -> % amount allocated for atoms
    erlang:memory(atom).

binary_alloc() -> % amount allocated for binaries
    erlang:memory(binary).

code_alloc() -> % amount allocated for code
    erlang:memory(code).

ets_alloc() -> % amount allocated for ets tables
    erlang:memory(ets).


%% formatting functions, from the record-value to string
node_name_str(#node_info{node_name = ToReturn}) ->
    erlang:atom_to_list(ToReturn).
no_procs_str(#node_info{no_procs = ToReturn}) ->
    erlang:integer_to_list(ToReturn).
no_cpu_str(#node_info{no_cpu = ToReturn}) ->
    erlang:integer_to_list(ToReturn).
no_cpu_available_str(#node_info{no_cpu_available = ToReturn}) ->
    erlang:integer_to_list(ToReturn).
no_cpu_online_str(#node_info{no_cpu_online = ToReturn}) ->
    erlang:integer_to_list(ToReturn).
tot_alloc_str(#node_info{tot_alloc = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

proc_used_str(#node_info{proc_used = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

proc_alloc_str(#node_info{proc_alloc = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

atom_used_str(#node_info{atom_used = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

atom_alloc_str(#node_info{atom_alloc = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

binary_alloc_str(#node_info{binary_alloc = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

code_alloc_str(#node_info{code_alloc = ToReturn}) ->
    erlang:integer_to_list(ToReturn).

ets_alloc_str(#node_info{ets_alloc = ToReturn}) ->
    erlang:integer_to_list(ToReturn).
