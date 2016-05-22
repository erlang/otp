%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").
-include_lib("os_mon/include/OTP-OS-MON-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

% Test server specific exports
-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1]).


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

-export([otp_6351/1, otp_7441/1]).

-define(TRAP_UDP, 5000).
-define(AGENT_UDP, 4000).
-define(CONF_FILE_VER, [v2]).
-define(SYS_NAME, "Test os_mon_mibs").
-define(MAX_MSG_SIZE, 484).
-define(ENGINE_ID, "mgrEngine").
-define(MGR_PORT, 5001).

%%---------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,6}},
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
    application:start(sasl),
    application:start(mnesia),
    application:start(os_mon),

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
    PrivDir = proplists:get_value(priv_dir, Config),
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
    
%% Test to unload and the reload the OTP.mib
load_unload(Config) when is_list(Config) ->
    os_mon_mib:unload(snmp_master_agent),
    os_mon_mib:load(snmp_master_agent),
    ok.
%%---------------------------------------------------------------------

%% check os_mon_mib:update_load_table error handling
update_load_table(Config) when is_list(Config) ->
    Node = start_node(),
    ok = rpc:call(Node,application,start,[sasl]),
    ok = rpc:call(Node,application,start,[os_mon]),
    ok = os_mon_mib:update_load_table(),
    rpc:call(Node,application,stop,[os_mon]),
    ok = os_mon_mib:update_load_table(),
    stop_node(Node),
    ok.

%% like update_load_table, when memsup_system_only==true
otp_6351(Config) when is_list(Config) ->
    Node = start_node(),
    ok = rpc:call(Node,application,start,[sasl]),
    ok = rpc:call(Node,application,load,[os_mon]),
    ok = rpc:call(Node,application,set_env,
                  [os_mon,memsup_system_only,true]),
    ok = rpc:call(Node,application,start,[os_mon]),
    Res = rpc:call(Node,os_mon_mib,get_load,[Node]),
    if
        is_tuple(Res), element(1, Res)==loadTable ->
            ok;
        true ->
            ct:fail(Res)
    end,
    rpc:call(Node,application,stop,[os_mon]),
    stop_node(Node),
    ok.


%%---------------------------------------------------------------------
%% Simulates a get call to test the instrumentation function
%% for the loadMemorySystemWatermark variable.
get_mem_sys_mark(Config) when is_list(Config) ->
    case  os_mon_mib:mem_sys_mark(get) of
        {value, SysMark} when is_integer(SysMark) ->
            ok;
        _ ->
            ct:fail(sys_mark_value_not_integer)
    end.
%%---------------------------------------------------------------------
%% Simulates a get call to test the instrumentation function
%% for the loadMemoryErlProcWatermark variable.
get_mem_proc_mark(Config) when is_list(Config) ->
    case os_mon_mib:mem_proc_mark(get) of
        {value, ProcMark} when is_integer(ProcMark) ->
            ok;
        _ ->
            ct:fail(proc_mark_value_not_integer)
    end.
%%---------------------------------------------------------------------
%% Simulates a get call to test the instrumentation function
%% for the diskAlmostFullThreshold variable.
get_disk_threshold(Config) when is_list(Config) ->
    case os_mon_mib:disk_threshold(get) of
        {value, ProcMark} when is_integer(ProcMark) ->
            ok;
        _ ->
            ct:fail(disk_threshold_value_not_integer)
    end.
%%---------------------------------------------------------------------

%%% Note that when we have a string key, as in loadTable, the
%%% instrumentation will deal with the [length(String), String]. We
%%% have to know about this, when short cutting SNMP and calling
%%% instrumentation functions directly as done in most test cases in
%%% this test suite

%% Simulates get calls to test the instrumentation function
%% for the loadTable
get_load_table(Config) when is_list(Config) ->

    NodeStr = atom_to_list(node()),
    NodeLen = length(NodeStr),

    {_, _, {Pid, _}} = memsup:get_memory_data(),
    PidStr = lists:flatten(io_lib:format("~w", [Pid])),
    [{value, NodeStr},{value, PidStr}] =
    os_mon_mib:load_table(get, [NodeLen  | NodeStr],
                          [?loadErlNodeName, ?loadLargestErlProcess]),

    Values = os_mon_mib:load_table(get, [NodeLen  | NodeStr] ,
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
            ct:fail(value_not_integer)
    end,

    [{noValue,noSuchInstance}, {noValue,noSuchInstance},
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

sys_tot_mem(Config) when is_list(Config) ->
    [{[?loadSystemTotalMemory, Len | NodeStr], Mem}] =
    os_mon_mib:load_table(get_next, [], [?loadSystemTotalMemory]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
        Mem when is_integer(Mem) ->
            ok;
        _ ->
            ct:fail(sys_tot_mem_value_not_integer)
    end.

sys_used_mem(Config) when is_list(Config) ->
    [{[?loadSystemUsedMemory, Len | NodeStr], Mem}] =
    os_mon_mib:load_table(get_next,[], [?loadSystemUsedMemory]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
        Mem when is_integer(Mem) ->
            ok;
        _ ->
            ct:fail(sys_used_mem_value_not_integer)
    end.

large_erl_process(Config) when is_list(Config) ->
    {_, _, {Pid, _}} = memsup:get_memory_data(),
    PidStr = lists:flatten(io_lib:format("~w", [Pid])),
    [{[?loadLargestErlProcess, Len | NodeStr], PidStr}] =
    os_mon_mib:load_table(get_next,[], [?loadLargestErlProcess]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),
    ok.

large_erl_process_mem(Config) when is_list(Config) ->

    [{[?loadLargestErlProcessUsedMemory, Len | NodeStr], Mem}] =
    os_mon_mib:load_table(get_next,[],
                          [?loadLargestErlProcessUsedMemory]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
        Mem when is_integer(Mem) ->
            ok;
        _ ->
            ct:fail(erl_pid_mem_value_not_integer)
    end.

cpu_load(Config) when is_list(Config) ->
    [{[?loadCpuLoad, Len | NodeStr], Load}] =
    os_mon_mib:load_table(get_next,[], [?loadCpuLoad]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Load of
        Load when is_integer(Load) ->
            ok;
        _ ->
            ct:fail(cpu_load_value_not_integer)
    end.

cpu_load5(Config) when is_list(Config) ->
    [{[?loadCpuLoad5, Len | NodeStr], Load}] =
    os_mon_mib:load_table(get_next,[], [?loadCpuLoad5]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Load of
        Load when is_integer(Load) ->
            ok;
        _ ->
            ct:fail(cpu_load5_value_not_integer)
    end.

cpu_load15(Config) when is_list(Config) ->
    [{[?loadCpuLoad15, Len | NodeStr], Load}] =
    os_mon_mib:load_table(get_next,[], [?loadCpuLoad15]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Load of
        Load when is_integer(Load) ->
            ok;
        _ ->
            ct:fail(cpu_load15_value_not_integer)
    end.

os_wordsize(Config) when is_list(Config) ->
    [{[?loadOsWordsize, Len | NodeStr], Wordsize}] =
    os_mon_mib:load_table(get_next,[], [?loadOsWordsize]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Wordsize of
        Wordsize when is_integer(Wordsize) ->
            ok;
        _ ->
            ct:fail(os_wordsize_value_not_integer)
    end.

sys_tot_mem64(Config) when is_list(Config) ->
    [{[?loadSystemTotalMemory64, Len | NodeStr], Mem}] =
    os_mon_mib:load_table(get_next, [], [?loadSystemTotalMemory64]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
        Mem when is_integer(Mem) ->
            ok;
        _ ->
            ct:fail(sys_tot_mem_value_not_integer)
    end.

sys_used_mem64(Config) when is_list(Config) ->
    [{[?loadSystemUsedMemory64, Len | NodeStr], Mem}] =
    os_mon_mib:load_table(get_next,[], [?loadSystemUsedMemory64]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
        Mem when is_integer(Mem) ->
            ok;
        _ ->
            ct:fail(sys_used_mem_value_not_integer)
    end.

large_erl_process_mem64(Config) when is_list(Config) ->

    [{[?loadLargestErlProcessUsedMemory64, Len | NodeStr], Mem}] =
    os_mon_mib:load_table(get_next,[],
                          [?loadLargestErlProcessUsedMemory64]),
    Len = length(NodeStr),
    true = lists:member(list_to_atom(NodeStr), [node() | nodes()]),

    case Mem of
        Mem when is_integer(Mem) ->
            ok;
        _ ->
            ct:fail(erl_pid_mem_value_not_integer)
    end.
%%---------------------------------------------------------------------
%% Simulates get calls to test the instrumentation function
%% for the diskTable.
get_disk_table(Config) when is_list(Config) ->

    DiskData = disksup:get_disk_data(),
    DiskDataLen = length(DiskData),

    if
        DiskDataLen > 0 ->
            [{value, Value}] =
            os_mon_mib:disk_table(get, [1,1], [?diskDescr]),

            case is_list(Value) of
                true ->
                    ok;
                false ->
                    ct:fail(value_not_a_string)
            end,

            Values = os_mon_mib:disk_table(get, [1,1],
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
                    ct:fail(value_not_integer)
            end
    end,

    [{noValue,noSuchInstance}, {noValue,noSuchInstance},
     {noValue,noSuchInstance}, {noValue,noSuchInstance}] =
    os_mon_mib:disk_table(get, [1, DiskDataLen + 1], [?diskId,
                                                      ?diskDescr,
                                                      ?diskKBytes,
                                                      ?diskCapacity]),

    ok.

%%---------------------------------------------------------------------

disk_descr(Config) when is_list(Config) ->
    [{[?diskDescr, 1,1], Descr}] =
    os_mon_mib:disk_table(get_next, [], [?diskDescr]),

    case Descr of
        Descr when is_list(Descr) ->
            ok;
        _ ->
            ct:fail(disk_descr_value_not_a_string)
    end.

disk_kbytes(Config) when is_list(Config) ->
    [{[?diskKBytes, 1,1], Kbytes}] =
    os_mon_mib:disk_table(get_next,[], [?diskKBytes]),

    case Kbytes of
        Kbytes when is_integer(Kbytes) ->
            ok;
        _ ->
            ct:fail(disk_kbytes_value_not_integer)
    end.


disk_capacity(Config) when is_list(Config) ->
    [{[?diskCapacity, 1,1], Capacity}] =
    os_mon_mib:disk_table(get_next,[], [?diskCapacity]),

    case Capacity of
        Capacity when is_integer(Capacity) ->
            ok;
        _ ->
            ct:fail(disk_capacity_value_not_integer)
    end.

%%---------------------------------------------------------------------
%% Starts an snmp manager and sends a real snmp-request. i.e.
%% sends a udp message on the correct format.
real_snmp_request(Config) when is_list(Config) ->
    NodStr = atom_to_list(node()),
    Len = length(NodStr),
    {_, _, {Pid, _}} = memsup:get_memory_data(),
    PidStr = lists:flatten(io_lib:format("~w", [Pid])),
    io:format("FOO: ~p~n", [PidStr]),
    ok = snmp_get([?loadEntry ++
                   [?loadLargestErlProcess, Len | NodStr]],
                  PidStr),
    ok = snmp_get_next([?loadEntry ++
                        [?loadSystemUsedMemory, Len | NodStr]],
                       ?loadEntry ++ [?loadSystemUsedMemory + 1, Len
                                      | NodStr], PidStr),
    ok = snmp_set([?loadEntry ++ [?loadLargestErlProcess,  Len | NodStr]],
                  s, "<0.101.0>", Config),
    ok.

%% Starts an snmp manager and requests total memory. Was previously
%% integer32 which was errornous on 64 bit machines.
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
    Pa = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(testnisse, slave, [{args, " -pa " ++ Pa}]),
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
