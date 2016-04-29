%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(os_mon_mib).
%%%-----------------------------------------------------------------
%%% Description: This module implements the OS-MON-MIB.
%%% The tables are implemented as shadow tables with the module
%%% snmp_shadow_table.  Here the update functions are implemented.
%%%-----------------------------------------------------------------

-include("../../otp_mibs/include/OTP-MIB.hrl").
      
%% API
-export([load/1, unload/1]).

%% Deprecated API
-export([init/1, stop/1]).

-deprecated([{init,1,eventually},
	     {stop,1,eventually}]).

%% SNMP instrumentation
-export([load_table/1, load_table/3, disk_table/1, disk_table/3,
	 mem_sys_mark/1, mem_proc_mark/1, disk_threshold/1]).

%% SNMP shadow functions
-export([update_load_table/0, update_disk_table/0]).

%% Exported for internal use via rpc
-export([get_load/1, get_disks/1]).

%% Shadow tables  
-record(loadTable, {
	loadErlNodeName, 
	loadSystemTotalMemory, 
	loadSystemUsedMemory,
	loadLargestErlProcess, 
	loadLargestErlProcessUsedMemory,
	loadCpuLoad, 
	loadCpuLoad5, 
	loadCpuLoad15,
	loadOsWordsize,
	loadSystemTotalMemory64, 
	loadSystemUsedMemory64,
	loadLargestErlProcessUsedMemory64}).

-record(diskTable,
	{key, diskDescr, diskKBytes, diskCapacity}).

%% Shadow argument macros 
-define(loadShadowArgs, 
	{loadTable, string, record_info(fields, loadTable), 5000,
	 fun os_mon_mib:update_load_table/0}).
	
-define(diskShadowArgs, 
	{diskTable, {integer, integer}, record_info(fields, diskTable), 5000,
	 fun os_mon_mib:update_disk_table/0}).

%% Misc
-record(diskAlloc, {diskDescr, diskId}).

%%%=========================================================================
%%%  API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% load(Agent) ->  ok | {error, Reason}
%% Agent - pid() | atom()
%% Reason - term()
%% Description: Loads the OTP-OS-MON-MIB
%%-------------------------------------------------------------------------
load(Agent) ->
    MibDir = filename:join(code:priv_dir(os_mon), "mibs"),
    snmpa:load_mibs(Agent, [filename:join(MibDir, "OTP-OS-MON-MIB")]).

%%-------------------------------------------------------------------------
%% unload(Agent) ->  ok | {error, Reason}
%% Agent - pid() | atom()
%% Reason - term()
%% Description: Unloads the OTP-OS-MON-MIB
%%-------------------------------------------------------------------------
unload(Agent) ->
    snmpa:unload_mibs(Agent, ["OTP-OS-MON-MIB"]).
    
%% To be backwards compatible
init(Agent) ->
    load(Agent).
stop(Agent) ->
    unload(Agent).

%%%=========================================================================
%%%  SNMP instrumentation
%%%=========================================================================
load_table(Op) ->
    snmp_shadow_table:table_func(Op, ?loadShadowArgs).
load_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?loadShadowArgs).

disk_table(new) ->
    Tab = diskAlloc,
    Storage = ram_copies, 
    case lists:member(Tab, mnesia:system_info(tables)) of
	true ->
	    case mnesia:table_info(Tab, storage_type) of
		unknown ->
		    {atomic, ok}=mnesia:add_table_copy(Tab, node(), Storage);
		Storage ->
		    catch delete_all(Tab)
	    end;
	false ->
	    Nodes = [node()],
	    Props = [{type, set},
		     {attributes, record_info(fields, diskAlloc)},
		     {local_content, true},
		     {Storage, Nodes}],
	    {atomic, ok} = mnesia:create_table(Tab, Props)
    
    end,
    Rec = #diskAlloc{diskDescr = next_index, diskId = 1},
    ok = mnesia:dirty_write(Rec),
    snmp_shadow_table:table_func(new, ?diskShadowArgs).

disk_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?diskShadowArgs).

mem_sys_mark(get) ->
    {value, memsup:get_sysmem_high_watermark()};
mem_sys_mark(_) ->
    ok.

mem_proc_mark(get) ->
    {value, memsup:get_procmem_high_watermark()};
mem_proc_mark(_) ->
    ok.

disk_threshold(get) ->
    {value, disksup:get_almost_full_threshold()};
disk_threshold(_) ->
    ok.

%%%=========================================================================
%%%  SNMP shadow functions
%%%=========================================================================
update_load_table() ->
    delete_all(loadTable),
    lists:foreach(
      fun(Node) ->
	      case rpc:call(Node, os_mon_mib, get_load, [Node]) of
		  Load when is_record(Load,loadTable) ->
		      ok = mnesia:dirty_write(Load);
		  _Else ->
		      ok
	      end
      end, [node() | nodes()]).


update_disk_table() ->
    delete_all(diskTable),
    node_update_disk_table(
      otp_mib:erl_node_table(get_next, [], [?erlNodeName,?erlNodeOutBytes])).

%%%========================================================================
%%% Exported for internal use via rpc
%%%========================================================================
get_load(Node) ->
    {Total, Allocated, PidString, PidAllocated} = case memsup:get_memory_data() of
	{MemTot, MemAlloc, undefined}     -> {MemTot, MemAlloc, "undefined", 0};
	{MemTot, MemAlloc, {Pid, PidMem}} -> {MemTot, MemAlloc, pid_to_str(Pid), PidMem} 
    end,
    OsWordsize = case memsup:get_os_wordsize() of
	WS when is_integer(WS) -> WS;
	_                      -> 0
    end,
    #loadTable{
	loadErlNodeName                   = atom_to_list(Node),
	loadSystemTotalMemory             = mask_int32(Total),
	loadSystemUsedMemory              = mask_int32(Allocated),
	loadLargestErlProcess             = PidString,
	loadLargestErlProcessUsedMemory   = mask_int32(PidAllocated),
	loadCpuLoad                       = get_cpu_load(avg1),   
	loadCpuLoad5                      = get_cpu_load(avg5),
	loadCpuLoad15                     = get_cpu_load(avg15),
	loadOsWordsize                    = OsWordsize,  
	loadSystemTotalMemory64           = Total,
	loadSystemUsedMemory64            = Allocated,
	loadLargestErlProcessUsedMemory64 = PidAllocated
    }.

mask_int32(Value) -> Value band ((1 bsl 32) - 1).

get_disks(NodeId) ->
    element(1,
	    lists:mapfoldl(
	      fun({Descr, KByte, Capacity}, DiskId) ->
		      {#diskTable{key = {NodeId, DiskId}, 
				  diskDescr = Descr,
				  diskKBytes = KByte, 
				  diskCapacity = Capacity},
		       DiskId + 1}
	      end, 1, disksup:get_disk_data())).


%%%========================================================================
%%% Internal functions
%%%========================================================================
node_update_disk_table([_, endOfTable]) -> 
    ok;

node_update_disk_table([{[?erlNodeName | IndexList], NodeStr}, _]) ->
    Disks = rpc:call(list_to_atom(NodeStr), os_mon_mib, get_disks, 
		     IndexList),
    lists:foreach(fun(Disk) ->
			  mnesia:dirty_write(Disk)
		  end, Disks),
    node_update_disk_table(otp_mib:erl_node_table(get_next, 
						  IndexList, 
						  [?erlNodeName, 
						   ?erlNodeOutBytes])).

get_cpu_load(X) when X == avg1; X == avg5; X == avg15 ->
    case erlang:round(apply(cpu_sup, X, [])/2.56) of
	Large when Large > 100 ->
	    100;
	Load ->
	    Load
    end.

delete_all(Name) -> delete_all(mnesia:dirty_first(Name), Name).
delete_all('$end_of_table', _Name) -> done;
delete_all(Key, Name) ->
    Next = mnesia:dirty_next(Name, Key),
    ok = mnesia:dirty_delete({Name, Key}),
    delete_all(Next, Name).

pid_to_str(Pid) -> lists:flatten(io_lib:format("~w", [Pid])).
