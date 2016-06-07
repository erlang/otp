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
-module(otp_mib).
%%%-----------------------------------------------------------------
%%% Description: This module implements the OTP-MIB.
%%% The tables are implemented as shadow tables with the module
%%% snmp_shadow_table.
%%%-----------------------------------------------------------------

%% API
-export([load/1, unload/1]).

%% SNMP instrumentation
-export([erl_node_table/1, erl_node_table/3, appl_table/1, appl_table/3]).

%% SNMP shadow functions
-export([update_erl_node_table/0, update_appl_table/0]).

%% Exported for internal use via rpc
-export([get_erl_node/1, get_appls/1]).

%% Shadow tables
-record(erlNodeTable,
	{erlNodeId, erlNodeName, erlNodeMachine, erlNodeVersion,
	 erlNodeRunQueue,
         erlNodeRunTime, erlNodeWallClock, erlNodeReductions,
	 erlNodeProcesses, erlNodeInBytes, erlNodeOutBytes}).

-record(applTable, {key = '_', applName = '_', applDescr = '_',
		    applVsn = '_'}).

%% Shadow argument macros
-define(erlNodeShadowArgs,
	{erlNodeTable, integer, record_info(fields, erlNodeTable), 5000,
	 fun otp_mib:update_erl_node_table/0}).

-define(applShadowArgs,
	{applTable, {integer, integer}, record_info(fields, applTable),
	 5000, fun otp_mib:update_appl_table/0}).

%% Misc
-record(erlNodeAlloc, {nodeName, nodeId}).

%%%=========================================================================
%%%  API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% load(Agent) ->  ok | {error, Reason}
%% Agent - pid() | atom()
%% Reason - term()
%% Description: Loads the OTP-MIB
%%-------------------------------------------------------------------------
load(Agent) ->
    MibDir = code:priv_dir(otp_mibs) ++ "/mibs",
    snmpa:load_mibs(Agent, [MibDir ++ "/OTP-MIB"]).

%%-------------------------------------------------------------------------
%% unload(Agent) ->  ok | {error, Reason}
%% Agent - pid() | atom()
%% Reason - term()
%% Description: Loads the OTP-MIB
%%-------------------------------------------------------------------------
unload(Agent) ->
    snmpa:unload_mibs(Agent, ["OTP-MIB"]).


%%%=========================================================================
%%%  SNMP instrumentation
%%%=========================================================================
erl_node_table(new) ->
    Tab = erlNodeAlloc,
    Storage = ram_copies,
    case lists:member(Tab, mnesia:system_info(tables)) of
	true ->
	    case mnesia:table_info(Tab, storage_type) of
		unknown ->
		    {atomic, ok} = mnesia:add_table_copy(Tab, node(), Storage);
		Storage ->
		    catch delete_all(Tab)
	    end;
	false ->
	    Nodes = [node()],
	    Props = [{type, set},
		     {attributes, record_info(fields, erlNodeAlloc)},
		     {local_content, true},
		     {Storage, Nodes}],
	    {atomic, ok} = mnesia:create_table(Tab, Props)
    end,
    ok = mnesia:dirty_write({erlNodeAlloc, next_index, 1}),
    update_node_alloc([node() | nodes()]),
    snmp_shadow_table:table_func(new, ?erlNodeShadowArgs).

erl_node_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?erlNodeShadowArgs).


appl_table(Op) ->
    snmp_shadow_table:table_func(Op, ?applShadowArgs).
appl_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?applShadowArgs).


%%%=========================================================================
%%%  SNMP shadow functions
%%%=========================================================================
update_erl_node_table() ->
    delete_all(erlNodeTable),
    Nodes = [node() | nodes()],
    update_node_alloc(Nodes),
    lists:foreach(
      fun(Node) ->
	      [{_,_,Idx}] = mnesia:dirty_read({erlNodeAlloc, Node}),
	      ErlNode = rpc:call(Node, otp_mib, get_erl_node, [Idx]),
	      ok = mnesia:dirty_write(ErlNode)
      end, Nodes).

update_appl_table() ->
    delete_all(applTable),
    Nodes = [node() | nodes()],
    update_node_alloc(Nodes),
    lists:foreach(
      fun(Node) ->
	      [{_,_,Idx}] = mnesia:dirty_read({erlNodeAlloc, Node}),
	      Appls = rpc:call(Node, otp_mib, get_appls, [Idx]),
	      lists:foreach(fun(Appl) ->
				    ok = mnesia:dirty_write(Appl)
			    end, Appls)
      end, Nodes).

%%%========================================================================
%%% Exported for internal use via rpc
%%%========================================================================
get_erl_node(Id) ->
    RunQueue = erlang:statistics(run_queue),
    RunTime = element(1, erlang:statistics(runtime)),
    WallClock = element(1, erlang:statistics(wall_clock)),
    Reductions = element(1, erlang:statistics(reductions)),
    Processes = length(processes()),
    IO = erlang:statistics(io),
    InBytes = element(2, element(1, IO)),
    OutBytes = element(2, element(2, IO)),
    #erlNodeTable{erlNodeId = truncate_int('Integer32', Id),
		  erlNodeName = atom_to_list(node()),
		  erlNodeVersion = erlang:system_info(version),
		  erlNodeMachine = erlang:system_info(machine),
		  erlNodeRunQueue = truncate_int('Unsigned32', RunQueue),
		  erlNodeRunTime = truncate_int('Counter64', RunTime),
		  erlNodeWallClock = truncate_int('Counter64', WallClock),
		  erlNodeReductions = truncate_int('Counter64', Reductions),
		  erlNodeProcesses = truncate_int('Unsigned32', Processes),
		  erlNodeInBytes = truncate_int('Counter64', InBytes),
		  erlNodeOutBytes = truncate_int('Counter64', OutBytes)}.

get_appls(NodeId) ->
    element(1,
	    lists:mapfoldl(
	      fun({ApplName, ApplDescr, ApplVsn}, ApplId) ->
		      {#applTable{key = {NodeId, ApplId},
				  applName = atom_to_list(ApplName),
				  applDescr = ApplDescr,
				  applVsn = ApplVsn},
		       ApplId + 1}
	      end, 1, application:which_applications())).

%%%========================================================================
%%% Internal functions
%%%========================================================================
update_node_alloc([Node | T]) ->
    case mnesia:dirty_read({erlNodeAlloc, Node}) of
	[] ->
	    [{_, _, Idx}] = mnesia:dirty_read({erlNodeAlloc, next_index}),
	    ok = mnesia:dirty_write(#erlNodeAlloc{nodeName = Node,
						  nodeId = Idx}),
	    ok = mnesia:dirty_write({erlNodeAlloc, next_index, Idx + 1});
	_ ->
	    ok
    end,
    update_node_alloc(T);
update_node_alloc([]) -> ok.

delete_all(Name) -> delete_all(mnesia:dirty_first(Name), Name).
delete_all('$end_of_table', _Name) -> done;
delete_all(Key, Name) ->
    Next = mnesia:dirty_next(Name, Key),
    ok = mnesia:dirty_delete({Name, Key}),
    delete_all(Next, Name).

%% This will return a value limited to fit into the specified type.
%% While counter types will be resetted, other integer types will
%% only be restricted to the valid range.
truncate_int('Counter64', Value) when Value < 0 -> 0;
truncate_int('Counter64', Value) -> Value rem 18446744073709551615;
truncate_int('Unsigned32', Value) when Value < 0 -> 0;
truncate_int('Unsigned32', Value) when Value > 4294967295 -> 4294967295;
truncate_int('Unsigned32', Value) -> Value;
truncate_int('Integer32', Value) when Value < -2147483648 -> -2147483648;
truncate_int('Integer32', Value) when Value > 2147483647 -> 2147483647;
truncate_int('Integer32', Value) -> Value.
