%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
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
-module(os_mon_mib_SUITE).

%%-----------------------------------------------------------------
%% This suite can no longer be executed standalone, i.e. it must be
%% executed with common test. The reason is that ct_snmp is used
%% instead of the snmp application directly. The suite requires a
%% config file, os_mon_mib_SUITE.cfg, found in the same directory as
%% the suite.
%%
%% Execute with:
%% > ct_run -suite os_mon_mib_SUITE -config os_mon_mib_SUITE.cfg
%%-----------------------------------------------------------------

-include_lib("test_server/include/test_server.hrl").
-include_lib("os_mon/include/OTP-OS-MON-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).


% Test cases must be exported.
-export([update_load_table/1]).

-export([get_mem_sys_mark/1, get_mem_proc_mark/1, get_disk_threshold/1,
	 get_load_table/1, get_disk_table/1,
	 real_snmp_request/1, load_unload/1]).

-export([sys_tot_mem/1, sys_used_mem/1, large_erl_process/1,
	 large_erl_process_mem/1, cpu_load/1, cpu_load5/1, cpu_load15/1,
	 os_wordsize/1, sys_tot_mem64/1, sys_used_mem64/1,
	 large_erl_process_mem64/1, disk_descr/1, disk_kbytes/1,
	 disk_capacity/1]).

-export([]).
-export([otp_6351/1, otp_7441/1]).

-define(TRAP_UDP, 5000).
-define(AGENT_UDP, 4000).
-define(CONF_FILE_VER, [v2]).
-define(SYS_NAME, "Test os_mon_mibs").
-define(MAX_MSG_SIZE, 484).
-define(ENGINE_ID, "mgrEngine").
-define(MGR_PORT, 5001).

%%---------------------------------------------------------------------
init_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:minutes(6)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    Config.

suite() -> [{ct_hooks,[ts_install_cth]},
	    {require, snmp_mgr_agent, snmp}].

all() -> 
    [load_unload, get_mem_sys_mark, get_mem_proc_mark,
     get_disk_threshold, get_load_table,
     {group, get_next_load_table}, get_disk_table,
     {group, get_next_disk_table}, real_snmp_request,
     update_load_table, {group, tickets}].

groups() -> 
    [{tickets, [], [otp_6351, otp_7441]},
     {get_next_load_table, [],
      [sys_tot_mem, sys_used_mem, large_erl_process,
       large_erl_process_mem, cpu_load, cpu_load5, cpu_load15,
       os_wordsize, sys_tot_mem64, sys_used_mem64,
       large_erl_process_mem64]},
     {get_next_disk_table, [],
      [disk_descr, disk_kbytes, disk_capacity]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%---------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?line application:start(sasl),
    ?line application:start(mnesia),
    ?line application:start(os_mon),

    ok = ct_snmp:start(Config,snmp_mgr_agent),

    %% Load the mibs that should be tested
    otp_mib:load(snmp_master_agent),
    os_mon_mib:load(snmp_master_agent),

    Config.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ConfDir = filename:join(PrivDir,"conf"),
    DbDir = filename:join(PrivDir,"db"),
    MgrDir = filename:join(PrivDir, "mgr"),

    %% Uload mibs
    snmpa:unload_mibs(snmp_master_agent,["OTP-OS-MON-MIB"]),
    otp_mib:unload(snmp_master_agent),

    %% Clean up
    application:stop(snmp),
    application:stop(mnesia),
    application:stop(os_mon),

    del_dir(ConfDir),
    del_dir(DbDir),
    (catch del_dir(MgrDir)),
    ok.

%%---------------------------------------------------------------------
%% Test cases
%%---------------------------------------------------------------------
load_unload(doc) ->
    ["Test to unload and the reload the OTP.mib "];
load_unload(suite) -> [];
load_unload(Config) when is_list(Config) ->
    ?line os_mon_mib:unload(snmp_master_agent),
    ?line os_mon_mib:load(snmp_master_agent),
    ok.
%%---------------------------------------------------------------------

update_load_table(doc) ->
    ["check os_mon_mib:update_load_table error handling"];
update_load_table(suite) ->
    [];
update_load_table(Config) when is_list(Config) ->
    ?line Node = start_node(),
    ?line ok = rpc:call(Node,application,start,[sasl]),
    ?line ok = rpc:call(Node,application,start,[os_mon]),
    ?line ok = os_mon_mib:update_load_table(),
    ?line rpc:call(Node,application,stop,[os_mon]),
    ?line ok = os_mon_mib:update_load_table(),
    ?line stop_node(Node),
    ok.

otp_6351(doc) ->
    ["like update_load_table, when memsup_system_only==true"];
otp_6351(suite) ->
    [];
otp_6351(Config) when is_list(Config) ->
    ?line Node = start_node(),
    ?line ok = rpc:call(Node,application,start,[sasl]),
    ?line ok = rpc:call(Node,application,load,[os_mon]),
    ?line ok = rpc:call(Node,application,set_env,
			[os_mon,memsup_system_only,true]),
    ?line ok = rpc:call(Node,application,start,[os_mon]),
    ?line Res = rpc:call(Node,os_mon_mib,get_load,[Node]),
    if
	is_tuple(Res), element(1, Res)==loadTable ->
	    ?line ok;
	true ->
	    ?line ?t:fail(Res)
    end,
    ?line rpc:call(Node,application,stop,[os_mon]),
    ?line stop_node(Node),
    ok.




%%---------------------------------------------------------------------
get_mem_sys_mark(doc) ->
    ["Simulates a get call to test the instrumentation function "
     "for the loadMemorySystemWatermark variable."];
get_mem_sys_mark(suite) ->
    [];
get_mem_sys_mark(Config) when is_list(Config) ->
    case  os_mon_mib:mem_sys_mark(get) of
	{value, SysMark} when is_integer(SysMark) ->
	    ok;
	_ ->
	    ?line test_server:fail(sys_mark_value_not_integer)
    end.
%%---------------------------------------------------------------------
get_mem_proc_mark(doc) ->
    ["Simulates a get call to test the instrumentation function "
     "for the loadMemoryErlProcWatermark variable."];
get_mem_proc_mark(suite) ->
    [];
get_mem_proc_mark(Config) when is_list(Config) ->
    case os_mon_mib:mem_proc_mark(get) of
	{value, ProcMark} when is_integer(ProcMark) ->
	    ok;
	_ ->
	    ?line test_server:fail(proc_mark_value_not_integer)
    end.
%%---------------------------------------------------------------------
get_disk_threshold(doc) ->
    ["Simulates a get call to test the instrumentation function "
     "for the diskAlmostFullThreshold variable."];
get_disk_threshold(suite) ->
    [];
get_disk_threshold(Config) when is_list(Config) ->
     case os_mon_mib:disk_threshold(get) of
	{value, ProcMark} when is_integer(ProcMark) ->
	    ok;
	_ ->
	    ?line test_server:fail(disk_threshold_value_not_integer)
    end.
%%---------------------------------------------------------------------

%%% Note that when we have a string key, as in loadTable, the
%%% instrumentation will deal with the [length(String), String]. We
%%% have to know about this, when short cutting SNMP and calling
%%% instrumentation functions directly as done in most test cases in
%%% this test suite

get_load_table(doc) ->
    ["Simulates get calls to test the instrumentation function "
     "for the loadTable"];
get_load_table(suite) ->
    [];
get_load_table(Config) when is_list(Config) ->

    NodeStr = atom_to_list(node()),
    NodeLen = length(NodeStr),

    {_, _, {Pid, _}} = memsup:get_memory_data(),
    PidStr = lists:flatten(io_lib:format("~w", [Pid])),
    ?line [{value, NodeStr},{value, PidStr}] =
	os_mon_mib:load_table(get, [NodeLen  | NodeStr],
			      [?loadErlNodeName, ?loadLargestErlProcess]),

    ?line Values = os_mon_mib:load_table(get, [NodeLen  | NodeStr] ,
					 [?loadSystemTotalMemory,
					  ?loadSystemUsedMemory,
					  ?loadLargestErlProcessUsedMemory,
					  ?loadCpuLoad,
					  ?loadCpuLoad5,
					  ?loadCpuLoad15,
					  ?loadOsWordsize,
					  ?loadSystemTotalMemory64,
					  ?loadSystemUsedMemory64,
					  ?loadLargestErlProcessUsedMemory64]),

    IsInt = fun({value, Val}) when is_integer(Val) ->
		    true;
	       (_) ->
		    false
	    end,

    NewValues = lists:filter(IsInt, Values),

    case length(NewValues) of
	10 ->
	    ok;
	_ ->
	    ?line test_server:fail(value_not_integer)
    end,

    ?line [{noValue,noSuchInstance}, {noValue,noSuchInstance},
	   {noValue,noSuchInstance}, {noValue,noSuchInstance},
	   {noValue,noSuchInstance}, {noValue,noSuchInstance},
	   {noValue,noSuchInstance}, {noValue,noSuchInstance},
	   {noValue,noSuchInstance}, {noValue,noSuchInstance},
	   {noValue,noSuchInstance}, {noValue,noSuchInstance}] =
	os_mon_mib:load_table(get, [3, 102, 111, 111],
			      [?loadErlNodeName,
			       ?loadSystemTotalMemory,
			       ?loadSystemUsedMemory,
			       ?loadLargestErlProcess,
			       ?loadLargestErlProcessUsedMemory,
			       ?loadCpuLoad,
			       ?loadCpuLoad5,
			       ?loadCpuLoad15,
			       ?loadOsWordsize,
			       ?loadSystemTotalMemory64,
			       ?loadSystemUsedMemory64,
			       ?loadLargestErlProcessUsedMemory64]),

    ok.
%%---------------------------------------------------------------------

sys_tot_mem(doc) ->
    [];
sys_tot_mem(suite) ->
    [];
sys_tot_mem(Config) when is_list(Config) ->
    ?line [{[?loadSystemTotalMemory, Len | NodeStr], Mem}] =
	os_mon_mib:load_table(get_next, [], [?loadSystemTotalMemory]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
	Mem when is_integer(Mem) ->
	    ok;
	_ ->
	    ?line test_server:fail(sys_tot_mem_value_not_integer)
    end.

sys_used_mem(doc) ->
    [];
sys_used_mem(suite) -> [];
sys_used_mem(Config) when is_list(Config) ->
    ?line [{[?loadSystemUsedMemory, Len | NodeStr], Mem}] =
	os_mon_mib:load_table(get_next,[], [?loadSystemUsedMemory]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
	Mem when is_integer(Mem) ->
	    ok;
	_ ->
	    ?line test_server:fail(sys_used_mem_value_not_integer)
    end.

large_erl_process(doc) ->
    [];
large_erl_process(suite) ->
    [];
large_erl_process(Config) when is_list(Config) ->
    {_, _, {Pid, _}} = memsup:get_memory_data(),
    PidStr = lists:flatten(io_lib:format("~w", [Pid])),
    ?line [{[?loadLargestErlProcess, Len | NodeStr], PidStr}] =
	os_mon_mib:load_table(get_next,[], [?loadLargestErlProcess]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),
    ok.

large_erl_process_mem(doc) ->
    [];
large_erl_process_mem(suite) ->
    [];
large_erl_process_mem(Config) when is_list(Config) ->

    ?line [{[?loadLargestErlProcessUsedMemory, Len | NodeStr], Mem}] =
	os_mon_mib:load_table(get_next,[],
			      [?loadLargestErlProcessUsedMemory]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

     case Mem of
	Mem when is_integer(Mem) ->
	    ok;
	_ ->
	    ?line test_server:fail(erl_pid_mem_value_not_integer)
    end.

cpu_load(doc) ->
    [];
cpu_load(suite) ->
    [];
cpu_load(Config) when is_list(Config) ->
    ?line [{[?loadCpuLoad, Len | NodeStr], Load}] =
	os_mon_mib:load_table(get_next,[], [?loadCpuLoad]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Load of
	Load when is_integer(Load) ->
	    ok;
	_ ->
	    ?line test_server:fail(cpu_load_value_not_integer)
    end.

cpu_load5(doc) ->
    [];
cpu_load5(suite) ->
    [];
cpu_load5(Config) when is_list(Config) ->
    ?line [{[?loadCpuLoad5, Len | NodeStr], Load}] =
	os_mon_mib:load_table(get_next,[], [?loadCpuLoad5]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Load of
	Load when is_integer(Load) ->
	    ok;
	_ ->
	    ?line test_server:fail(cpu_load5_value_not_integer)
    end.

cpu_load15(doc) ->
    [];
cpu_load15(suite) ->
    [];
cpu_load15(Config) when is_list(Config) ->
    ?line [{[?loadCpuLoad15, Len | NodeStr], Load}] =
	os_mon_mib:load_table(get_next,[], [?loadCpuLoad15]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

     case Load of
	 Load when is_integer(Load) ->
	     ok;
	 _ ->
	     ?line test_server:fail(cpu_load15_value_not_integer)
     end.

os_wordsize(doc) ->
    [];
os_wordsize(suite) ->
    [];
os_wordsize(Config) when is_list(Config) ->
    ?line [{[?loadOsWordsize, Len | NodeStr], Wordsize}] =
	os_mon_mib:load_table(get_next,[], [?loadOsWordsize]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

     case Wordsize of
	 Wordsize when is_integer(Wordsize) ->
	     ok;
	 _ ->
	     ?line test_server:fail(os_wordsize_value_not_integer)
     end.

sys_tot_mem64(doc) ->
    [];
sys_tot_mem64(suite) ->
    [];
sys_tot_mem64(Config) when is_list(Config) ->
    ?line [{[?loadSystemTotalMemory64, Len | NodeStr], Mem}] =
	os_mon_mib:load_table(get_next, [], [?loadSystemTotalMemory64]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
	Mem when is_integer(Mem) ->
	    ok;
	_ ->
	    ?line test_server:fail(sys_tot_mem_value_not_integer)
    end.

sys_used_mem64(doc) ->
    [];
sys_used_mem64(suite) -> [];
sys_used_mem64(Config) when is_list(Config) ->
    ?line [{[?loadSystemUsedMemory64, Len | NodeStr], Mem}] =
	os_mon_mib:load_table(get_next,[], [?loadSystemUsedMemory64]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
	Mem when is_integer(Mem) ->
	    ok;
	_ ->
	    ?line test_server:fail(sys_used_mem_value_not_integer)
    end.

large_erl_process_mem64(doc) ->
    [];
large_erl_process_mem64(suite) ->
    [];
large_erl_process_mem64(Config) when is_list(Config) ->

    ?line [{[?loadLargestErlProcessUsedMemory64, Len | NodeStr], Mem}] =
	os_mon_mib:load_table(get_next,[],
			      [?loadLargestErlProcessUsedMemory64]),
    ?line Len = length(NodeStr),
    ?line true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

     case Mem of
	Mem when is_integer(Mem) ->
	    ok;
	_ ->
	    ?line test_server:fail(erl_pid_mem_value_not_integer)
    end.
%%---------------------------------------------------------------------
get_disk_table(doc) ->
    ["Simulates get calls to test the instrumentation function "
     "for the diskTable."];
get_disk_table(suite) ->
    [];
get_disk_table(Config) when is_list(Config) ->

    DiskData = disksup:get_disk_data(),
    DiskDataLen = length(DiskData),

    if
	DiskDataLen > 0 ->
	    ?line [{value, Value}] =
		os_mon_mib:disk_table(get, [1,1], [?diskDescr]),

	    case is_list(Value) of
		true ->
		    ok;
		false ->
		    ?line test_server:fail(value_not_a_string)
	    end,

	    ?line Values = os_mon_mib:disk_table(get, [1,1],
						 [?diskId,
						  ?diskKBytes,
						  ?diskCapacity]),

	    IsInt = fun({value, Val}) when is_integer(Val) ->
		    true;
		       (_) ->
			    false
		    end,

	    NewValues = lists:filter(IsInt, Values),

	    case length(NewValues) of
		3 ->
		    ok;
		_ ->
		    ?line test_server:fail(value_not_integer)
	    end
    end,

    ?line [{noValue,noSuchInstance}, {noValue,noSuchInstance},
	   {noValue,noSuchInstance}, {noValue,noSuchInstance}] =
	os_mon_mib:disk_table(get, [1, DiskDataLen + 1], [?diskId,
							  ?diskDescr,
							  ?diskKBytes,
							  ?diskCapacity]),

    ok.

%%---------------------------------------------------------------------

disk_descr(doc) ->
    [];
disk_descr(suite) ->
    [];
disk_descr(Config) when is_list(Config) ->
    ?line [{[?diskDescr, 1,1], Descr}] =
	os_mon_mib:disk_table(get_next, [], [?diskDescr]),

    case Descr of
	Descr when is_list(Descr) ->
	    ok;
	_ ->
	    ?line test_server:fail(disk_descr_value_not_a_string)
    end.

disk_kbytes(doc) ->
    [];
disk_kbytes(suite) -> [];
disk_kbytes(Config) when is_list(Config) ->
    ?line [{[?diskKBytes, 1,1], Kbytes}] =
	os_mon_mib:disk_table(get_next,[], [?diskKBytes]),

    case Kbytes of
	Kbytes when is_integer(Kbytes) ->
	    ok;
	_ ->
	    ?line test_server:fail(disk_kbytes_value_not_integer)
    end.


disk_capacity(doc) ->
    [];
disk_capacity(suite) -> [];
disk_capacity(Config) when is_list(Config) ->
    ?line [{[?diskCapacity, 1,1], Capacity}] =
	os_mon_mib:disk_table(get_next,[], [?diskCapacity]),

    case Capacity of
	Capacity when is_integer(Capacity) ->
	    ok;
	_ ->
	    ?line test_server:fail(disk_capacity_value_not_integer)
    end.

%%---------------------------------------------------------------------
real_snmp_request(doc) ->
    ["Starts an snmp manager and sends a real snmp-request. i.e. "
     "sends a udp message on the correct format."];
real_snmp_request(suite) -> [];
real_snmp_request(Config) when is_list(Config) ->
    NodStr = atom_to_list(node()),
    Len = length(NodStr),
    {_, _, {Pid, _}} = memsup:get_memory_data(),
    PidStr = lists:flatten(io_lib:format("~w", [Pid])),
    io:format("FOO: ~p~n", [PidStr]),
    ?line ok = snmp_get([?loadEntry ++
			 [?loadLargestErlProcess, Len | NodStr]],
			PidStr),
    ?line ok = snmp_get_next([?loadEntry ++
			      [?loadSystemUsedMemory, Len | NodStr]],
			     ?loadEntry ++ [?loadSystemUsedMemory + 1, Len
					    | NodStr], PidStr),
    ?line ok = snmp_set([?loadEntry ++ [?loadLargestErlProcess,  Len | NodStr]],
			s, "<0.101.0>", Config),
    ok.

otp_7441(doc) ->
    ["Starts an snmp manager and requests total memory. Was previously
     integer32 which was errornous on 64 bit machines."];
otp_7441(suite) ->
    [];
otp_7441(Config) when is_list(Config) ->
    NodStr = atom_to_list(node()),
    Len = length(NodStr),
    Oids = [Oid|_] = [?loadEntry ++ [?loadSystemTotalMemory, Len | NodStr]],
    {noError,0,[#varbind{oid = Oid, variabletype = 'Unsigned32'}]} =
	ct_snmp:get_values(os_mon_mib_test, Oids, snmp_mgr_agent),

    ok.

%%---------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------
start_node() ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line {ok,Node} = test_server:start_node(testnisse, slave,
					     [{args, " -pa " ++ Pa}]),
    Node.

stop_node(Node) ->
    test_server:stop_node(Node).

del_dir(Dir) ->
    io:format("Deleting: ~s~n",[Dir]),
    {ok, Files} = file:list_dir(Dir),
    FullPathFiles = lists:map(fun(File) -> filename:join(Dir, File) end,
			      Files),
    lists:foreach(fun file:delete/1, FullPathFiles),
    file:del_dir(Dir).

%%---------------------------------------------------------------------
snmp_get(Oids = [Oid |_], Result) ->
    {noError,0,[#varbind{oid = Oid,
			 variabletype = 'OCTET STRING',
			 value = Result}]} =
	ct_snmp:get_values(os_mon_mib_test, Oids, snmp_mgr_agent),
    ok.

snmp_get_next(Oids, NextOid, Result) ->
    {noError,0,[#varbind{oid = NextOid,
			 variabletype = 'OCTET STRING',
			 value = Result}]} =
	ct_snmp:get_next_values(os_mon_mib_test, Oids, snmp_mgr_agent),
    ok.

snmp_set(Oid, ValuType, Value, Config) ->
    {notWritable, _, _} =
	ct_snmp:set_values(os_mon_mib_test, [{Oid, ValuType, Value}],
			   snmp_mgr_agent, Config),
    ok.
