%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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

%%%----------------------------------------------------------------------
%%% File    : node_container_SUITE.erl
%%% Author  : Rickard <rickard.green@uab.ericsson.se>
%%% Purpose : 
%%% Created : 24 Jul 2002 by Rickard <rickard.green@uab.ericsson.se>
%%%----------------------------------------------------------------------

-module(node_container_SUITE).
-author('rickard.green@uab.ericsson.se').

%-define(line_trace, 1).

-include_lib("test_server/include/test_server.hrl").

%-compile(export_all).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, init_per_testcase/2, 
	 end_per_testcase/2,
	 node_container_refc_check/1]).

-export([term_to_binary_to_term_eq/1,
	 round_trip_eq/1,
	 cmp/1,
	 ref_eq/1,
	 node_table_gc/1,
	 dist_link_refc/1,
	 dist_monitor_refc/1,
	 node_controller_refc/1,
	 ets_refc/1,
	 match_spec_refc/1,
	 timer_refc/1,
	 otp_4715/1,
	 pid_wrap/1,
	 port_wrap/1,
	 bad_nc/1,
	 unique_pid/1,
	 iter_max_procs/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(10)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [term_to_binary_to_term_eq, round_trip_eq, cmp, ref_eq,
     node_table_gc, dist_link_refc, dist_monitor_refc,
     node_controller_refc, ets_refc, match_spec_refc,
     timer_refc, otp_4715, pid_wrap, port_wrap, bad_nc,
     unique_pid, iter_max_procs].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    available_internal_state(false).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


available_internal_state(Bool) when Bool == true; Bool == false ->
    case {Bool,
	  (catch erts_debug:get_internal_state(available_internal_state))} of
	{true, true} ->
	    true;
	{false, true} ->
	    erts_debug:set_internal_state(available_internal_state, false),
	    true;
	{true, _} ->
	    erts_debug:set_internal_state(available_internal_state, true),
	    false;
	{false, _} ->
	    false
    end.

init_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    available_internal_state(true),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

-define(MAX_PIDS_PORTS, ((1 bsl 28) - 1)).

%%
%% Test case: term_to_binary_to_term_eq
%%
term_to_binary_to_term_eq(doc) ->
    ["Tests that node container terms that are converted to external format "
     "and back stay equal to themselves."];
term_to_binary_to_term_eq(suite) -> [];
term_to_binary_to_term_eq(Config) when is_list(Config) ->
    ?line ThisNode = {node(), erlang:system_info(creation)},
    % Get local node containers
    ?line LPid = self(),
    ?line LXPid = mk_pid(ThisNode, 32767, 8191),
    ?line LPort = hd(erlang:ports()),
    ?line LXPort = mk_port(ThisNode, 268435455),
    ?line LLRef = make_ref(),
    ?line LHLRef = mk_ref(ThisNode, [47, 11]),
    ?line LSRef = mk_ref(ThisNode, [4711]),
    % Test local nc:s
    ?line LPid = binary_to_term(term_to_binary(LPid)),
    ?line LXPid = binary_to_term(term_to_binary(LXPid)),
    ?line LPort = binary_to_term(term_to_binary(LPort)),
    ?line LXPort = binary_to_term(term_to_binary(LXPort)),
    ?line LLRef = binary_to_term(term_to_binary(LLRef)),
    ?line LHLRef = binary_to_term(term_to_binary(LHLRef)),
    ?line LSRef = binary_to_term(term_to_binary(LSRef)),
    % Get remote node containers
    ?line RNode = {get_nodename(), 3},
    ?line RPid = mk_pid(RNode, 4711, 1),
    ?line RXPid = mk_pid(RNode, 32767, 8191),
    ?line RPort = mk_port(RNode, 4711),
    ?line RXPort = mk_port(RNode, 268435455),
    ?line RLRef = mk_ref(RNode, [4711, 4711, 4711]),
    ?line RHLRef = mk_ref(RNode, [4711, 4711]),
    ?line RSRef = mk_ref(RNode, [4711]),
    % Test remote nc:s
    ?line RPid = binary_to_term(term_to_binary(RPid)),
    ?line RXPid = binary_to_term(term_to_binary(RXPid)),
    ?line RPort = binary_to_term(term_to_binary(RPort)),
    ?line RXPort = binary_to_term(term_to_binary(RXPort)),
    ?line RLRef = binary_to_term(term_to_binary(RLRef)),
    ?line RHLRef = binary_to_term(term_to_binary(RHLRef)),
    ?line RSRef = binary_to_term(term_to_binary(RSRef)),
    ?line nc_refc_check(node()),
    ?line ok.


%%
%% Test case: round_trip_eq
%%
round_trip_eq(doc) ->
    ["Tests that node containers that are sent beteen nodes stay equal to "
     "themselves."];
round_trip_eq(suite) -> [];
round_trip_eq(Config) when is_list(Config) ->
    ?line ThisNode = {node(), erlang:system_info(creation)},
    ?line NodeFirstName = get_nodefirstname(),
    ?line ?line {ok, Node} = start_node(NodeFirstName),
    ?line Self = self(),
    ?line RPid = spawn_link(Node,
			    fun () ->
				    receive
					{Self, Data} ->
					    Self ! {self(), Data}
				    end
			    end),
    ?line SentPid = self(),
    ?line SentXPid = mk_pid(ThisNode, 17471, 8190),
    ?line SentPort = hd(erlang:ports()),
    ?line SentXPort = mk_port(ThisNode, 268435451),
    ?line SentLRef = make_ref(),
    ?line SentHLRef = mk_ref(ThisNode, [4711, 17]),
    ?line SentSRef = mk_ref(ThisNode, [4711]),
    ?line RPid ! {Self, {SentPid,
			 SentXPid,
			 SentPort,
			 SentXPort,
			 SentLRef,
			 SentHLRef,
			 SentSRef}},
    receive
	{RPid, {RecPid,
		RecXPid,
		RecPort,
		RecXPort,
		RecLRef,
		RecHLRef,
		RecSRef}} ->
	    ?line stop_node(Node),
	    ?line SentPid = RecPid,
	    ?line SentXPid = RecXPid,
	    ?line SentPort = RecPort,
	    ?line SentXPort = RecXPort,
	    ?line SentLRef = RecLRef,
	    ?line SentHLRef = RecHLRef,
	    ?line SentSRef = RecSRef,
	    ?line nc_refc_check(node()),
	    ?line ok
    end.
	    


%%
%% Test case: cmp
%%
cmp(doc) ->
    ["Tests that Erlang term comparison works as it should on node "
     "containers."];
cmp(suite) -> [];
cmp(Config) when is_list(Config) ->

    %% Inter type comparison ---------------------------------------------------

    %% The Erlang term order:
    %% number < atom < ref < fun < port < pid < tuple < nil < cons < binary
    RNode = {get_nodename(), 2},

    IRef = make_ref(),
    ERef = mk_ref({get_nodename(), 2}, [1,2,3]),
    
    IPid = self(),
    EPid = mk_pid(RNode, 1, 2),

    IPort = hd(erlang:ports()),
    EPort = mk_port(RNode, 1),
    
    %% Test pids ----------------------------------------------------
    ?line true = 1 < IPid,
    ?line true = 1.3 < IPid,
    ?line true = (1 bsl 64) < IPid,
    ?line true = an_atom < IPid,
    ?line true = IRef < IPid,
    ?line true = ERef < IPid,
    ?line true = fun () -> a_fun end < IPid,
    ?line true = IPort < IPid,
    ?line true = EPort < IPid,
    ?line true = IPid < {a, tuple},
    ?line true = IPid < [],
    ?line true = IPid < [a|cons],
    ?line true = IPid < <<"a binary">>,

    ?line true = 1 < EPid,
    ?line true = 1.3 < EPid,
    ?line true = (1 bsl 64) < EPid,
    ?line true = an_atom < EPid,
    ?line true = IRef < EPid,
    ?line true = ERef < EPid,
    ?line true = fun () -> a_fun end < EPid,
    ?line true = IPort < EPid,
    ?line true = EPort < EPid,
    ?line true = EPid < {a, tuple},
    ?line true = EPid < [],
    ?line true = EPid < [a|cons],
    ?line true = EPid < <<"a binary">>,

    %% Test ports --------------------------------------------------
    ?line true = 1 < IPort,
    ?line true = 1.3 < IPort,
    ?line true = (1 bsl 64) < IPort,
    ?line true = an_atom < IPort,
    ?line true = IRef < IPort,
    ?line true = ERef < IPort,
    ?line true = fun () -> a_fun end < IPort,
    ?line true = IPort < IPid,
    ?line true = IPort < EPid,
    ?line true = IPort < {a, tuple},
    ?line true = IPort < [],
    ?line true = IPort < [a|cons],
    ?line true = IPort < <<"a binary">>,

    ?line true = 1 < EPort,
    ?line true = 1.3 < EPort,
    ?line true = (1 bsl 64) < EPort,
    ?line true = an_atom < EPort,
    ?line true = IRef < EPort,
    ?line true = ERef < EPort,
    ?line true = fun () -> a_fun end < EPort,
    ?line true = EPort < IPid,
    ?line true = EPort < EPid,
    ?line true = EPort < {a, tuple},
    ?line true = EPort < [],
    ?line true = EPort < [a|cons],
    ?line true = EPort < <<"a binary">>,

    %% Test refs ----------------------------------------------------
    ?line true = 1 < IRef,
    ?line true = 1.3 < IRef,
    ?line true = (1 bsl 64) < IRef,
    ?line true = an_atom < IRef,
    ?line true = IRef < fun () -> a_fun end,
    ?line true = IRef < IPort,
    ?line true = IRef < EPort,
    ?line true = IRef < IPid,
    ?line true = IRef < EPid,
    ?line true = IRef < {a, tuple},
    ?line true = IRef < [],
    ?line true = IRef < [a|cons],
    ?line true = IRef < <<"a binary">>,

    ?line true = 1 < ERef,
    ?line true = 1.3 < ERef,
    ?line true = (1 bsl 64) < ERef,
    ?line true = an_atom < ERef,
    ?line true = ERef < fun () -> a_fun end,
    ?line true = ERef < IPort,
    ?line true = ERef < EPort,
    ?line true = ERef < IPid,
    ?line true = ERef < EPid,
    ?line true = ERef < {a, tuple},
    ?line true = ERef < [],
    ?line true = ERef < [a|cons],
    ?line true = ERef < <<"a binary">>,


    %% Intra type comparison ---------------------------------------------------
	

    %% Test pids ----------------------------------------------------
    %%
    %% Significance (most -> least):
    %%   serial, number, nodename, creation
    %%

    ?line Pid = mk_pid({b@b, 2}, 4711, 1),

    ?line true = mk_pid({a@b, 1}, 4710, 2) > Pid,
    ?line true = mk_pid({a@b, 1}, 4712, 1) > Pid,
    ?line true = mk_pid({c@b, 1}, 4711, 1) > Pid,
    ?line true = mk_pid({b@b, 3}, 4711, 1) > Pid,
    ?line true = mk_pid({b@b, 2}, 4711, 1) =:= Pid,

    %% Test ports ---------------------------------------------------
    %%
    %% Significance (most -> least):
    %%   nodename, creation, number 
    %%
    %% OBS: Comparison between ports has changed in R9. This
    %%      since it wasn't stable in R8 (and eariler releases).
    %%      Significance used to be: dist_slot, number,
    %%      creation.

    ?line Port = mk_port({b@b, 2}, 4711),

    ?line true = mk_port({c@b, 1}, 4710) > Port,
    ?line true = mk_port({b@b, 3}, 4710) > Port,
    ?line true = mk_port({b@b, 2}, 4712) > Port,
    ?line true = mk_port({b@b, 2}, 4711) =:= Port,

    %% Test refs ----------------------------------------------------
    %% Significance (most -> least):
    %% nodename, creation, (number high, number mid), number low, 
    %%
    %% OBS: Comparison between refs has changed in R9. This
    %%      since it wasn't stable in R8 (and eariler releases).
    %%      Significance used to be: dist_slot, number,
    %%      creation.
    %%

    ?line Ref = mk_ref({b@b, 2}, [4711, 4711, 4711]),

    ?line true = mk_ref({c@b, 1}, [4710, 4710, 4710]) > Ref,
    ?line true = mk_ref({b@b, 3}, [4710, 4710, 4710]) > Ref,
    ?line true = mk_ref({b@b, 2}, [4710, 4710, 4712]) > Ref,
    ?line true = mk_ref({b@b, 2}, [4710, 4712, 4711]) > Ref,
    ?line true = mk_ref({b@b, 2}, [4712, 4711, 4711]) > Ref,
    ?line true = mk_ref({b@b, 2}, [4711, 4711, 4711]) =:= Ref,

    ok.

%%
%% Test case: ref_eq
%%
ref_eq(doc) -> ["Test that one word refs \"works\"."];
ref_eq(suite) -> [];
ref_eq(Config) when is_list(Config) ->
    ?line ThisNode = {node(), erlang:system_info(creation)},
    ?line AnotherNode = {get_nodename(),2},
    ?line LLongRef = mk_ref(ThisNode, [4711, 0, 0]),
    ?line LHalfLongRef = mk_ref(ThisNode, [4711, 0]),
    ?line LShortRef = mk_ref(ThisNode, [4711]),
    ?line true = LLongRef =:= LShortRef,
    ?line true = LLongRef =:= LHalfLongRef,
    ?line true = LLongRef =:= LLongRef,
    ?line true = LHalfLongRef =:= LShortRef,
    ?line true = LHalfLongRef =:= LHalfLongRef,
    ?line true = LShortRef =:= LShortRef,
    ?line false = LShortRef == mk_ref(ThisNode, [4711, 0, 1]), % Not any more
    ?line RLongRef = mk_ref(AnotherNode, [4711, 0, 0]),
    ?line RHalfLongRef = mk_ref(AnotherNode, [4711, 0]),
    ?line RShortRef = mk_ref(AnotherNode, [4711]),
    ?line true = RLongRef =:= RShortRef,
    ?line true = RLongRef =:= RHalfLongRef,
    ?line true = RLongRef =:= RLongRef,
    ?line true = RHalfLongRef =:= RShortRef,
    ?line true = RHalfLongRef =:= RHalfLongRef,
    ?line true = RShortRef =:= RShortRef,
    ?line false = RShortRef == mk_ref(AnotherNode, [4711, 0, 1]), % Not any more
    ?line nc_refc_check(node()),
    ?line ok.
    
%%
%% Test case: node_table_gc
%%
node_table_gc(doc) ->
    ["Tests that node tables are garbage collected."];
node_table_gc(suite) -> [];
node_table_gc(Config) when is_list(Config) ->
    ?line PreKnown = nodes(known),
    ?line ?t:format("PreKnown = ~p~n", [PreKnown]),
    ?line make_node_garbage(0, 200000, 1000, []),
    ?line PostKnown = nodes(known),
    ?line PostAreas = erlang:system_info(allocated_areas),
    ?line ?t:format("PostKnown = ~p~n", [PostKnown]),
    ?line ?t:format("PostAreas = ~p~n", [PostAreas]),
    ?line true = length(PostKnown) =< length(PreKnown),
    ?line nc_refc_check(node()),
    ?line ok.

make_node_garbage(N, L, I, Ps) when N < L ->
    ?line Self = self(),
    ?line P = spawn_link(fun () ->
				 % Generate two node entries and one dist
				 % entry per node name
				 ?line PL1 = make_faked_pid_list(N,
								 I div 2,
								 1),
				 ?line put(a, PL1),
				 ?line PL2 = make_faked_pid_list(N,
								 I div 2,
								 2),
				 ?line put(b, PL2),
				 ?line Self ! {self(), length(nodes(known))}
			 end),
    ?line receive
	      {P, KnownLength} ->
		  ?line true = KnownLength >= I div 2
	  end,
    ?line make_node_garbage(N+(I div 2)*2, L, I, [P|Ps]);
make_node_garbage(_, _, _, Ps) ->
    %% Cleanup garbage...
    ProcIsCleanedUp
	= fun (Proc) ->
		  undefined == erts_debug:get_internal_state({process_status,
							      Proc})
	  end,
    lists:foreach(fun (P) -> wait_until(fun () -> ProcIsCleanedUp(P) end) end,
		  Ps),
    ?line ok.


make_faked_pid_list(Start, No, Creation) ->
    make_faked_pid_list(Start, No, Creation, []).

make_faked_pid_list(_Start, 0, _Creation, Acc) ->
    Acc;
make_faked_pid_list(Start, No, Creation, Acc) ->
    make_faked_pid_list(Start+1,
			No-1,
			Creation,
			[mk_pid({"faked_node-"
				 ++ integer_to_list(Start rem 50000)
				 ++ "@"
				 ++ atom_to_list(?MODULE),
				 Creation},
				4711,
				3) | Acc]).

%%
%% Test case: dist_link_refc
%%
dist_link_refc(doc) ->
    ["Tests that external reference counts are incremented and decremented "
     "as they should for distributed links"];
dist_link_refc(suite) -> [];
dist_link_refc(Config) when is_list(Config) ->
    ?line NodeFirstName = get_nodefirstname(),
    ?line ?line {ok, Node} = start_node(NodeFirstName),
    ?line RP = spawn_execer(Node),
    ?line LP = spawn_link_execer(node()),
    ?line true = sync_exec(RP, fun () -> link(LP) end),
    ?line wait_until(fun () ->
			     ?line {links, Links} = process_info(LP, links),
			     ?line lists:member(RP, Links)
		     end),
    ?line NodeCre = sync_exec(RP, fun() -> erlang:system_info(creation) end),
    ?line 1 = reference_type_count(
		link,
		refering_entity_id({process, LP},
				   get_node_references({Node, NodeCre}))),
    ?line exec(RP, fun() -> exit(normal) end),
    ?line wait_until(fun () ->
			     ?line {links, Links} = process_info(LP, links),
			     ?line not lists:member(RP, Links)
		     end),
    ?line 0 = reference_type_count(
		link,
		refering_entity_id({process, LP},
				   get_node_references({Node, NodeCre}))),
    ?line exit(LP, normal),
    ?line stop_node(Node),
    ?line nc_refc_check(node()),
    ?line ok.


%%
%% Test case: dist_monitor_refc
%%
dist_monitor_refc(doc) ->
    ["Tests that external reference counts are incremented and decremented "
     "as they should for distributed monitors"];
dist_monitor_refc(suite) -> [];
dist_monitor_refc(Config) when is_list(Config) ->
    ?line NodeFirstName = get_nodefirstname(),
    ?line {ok, Node} = start_node(NodeFirstName),
    ?line RP = spawn_execer(Node),
    ?line LP = spawn_link_execer(node()),
    ?line RMon = sync_exec(RP, fun () -> erlang:monitor(process, LP) end),
    ?line true = is_reference(RMon),
    ?line LMon = sync_exec(LP, fun () -> erlang:monitor(process, RP) end),
    ?line true = is_reference(LMon),
    ?line NodeCre = sync_exec(RP, fun() -> erlang:system_info(creation) end),
    ?line wait_until(fun () ->
			     ?line {monitored_by, MonBy}
				 = process_info(LP, monitored_by),
			     ?line {monitors, Mon}
				 = process_info(LP, monitors),
			     ?line (lists:member(RP, MonBy)
				    and lists:member({process,RP}, Mon))
		     end),
    ?line 3 = reference_type_count(
		monitor,
		refering_entity_id({process, LP},
				   get_node_references({Node, NodeCre}))),
    ?line exec(RP, fun () -> exit(normal) end),
    ?line wait_until(fun () ->
			     ?line {monitored_by, MonBy}
				 = process_info(LP, monitored_by),
			     ?line {monitors, Mon}
				 = process_info(LP, monitors),
			     ?line ((not lists:member(RP, MonBy))
				    and (not lists:member({process,RP}, Mon)))
		     end),
    ?line ok = sync_exec(LP,
			 fun () ->
				 receive
				     {'DOWN', LMon, process, _, _} ->
					 ok
				 end
			 end),
    ?line 0 = reference_type_count(
		link,
		refering_entity_id({process, LP},
				   get_node_references({Node, NodeCre}))),
    ?line exit(LP, normal),
    ?line stop_node(Node),
    ?line nc_refc_check(node()),
    ?line ok.


%%
%% Test case: node_controller_refc
%%
node_controller_refc(doc) ->
    ["Tests that external reference counts are incremented and decremented "
     "as they should for entities controlling a connections."];
node_controller_refc(suite) -> [];
node_controller_refc(Config) when is_list(Config) ->
    ?line NodeFirstName = get_nodefirstname(),
    ?line ?line {ok, Node} = start_node(NodeFirstName),
    ?line true = lists:member(Node, nodes()),
    ?line 1 = reference_type_count(control, get_dist_references(Node)),
    ?line P = spawn_link_execer(node()),
    ?line Node
	= sync_exec(P,
		    fun () ->
			    put(remote_net_kernel,
				rpc:call(Node,erlang,whereis,[net_kernel])),
			    node(get(remote_net_kernel))
		    end),
    ?line Creation = rpc:call(Node, erlang, system_info, [creation]),
    ?line monitor_node(Node,true),
    ?line stop_node(Node),
    ?line receive {nodedown, Node} -> ok end,
    ?line DistRefs = get_dist_references(Node),
    ?line true = reference_type_count(node, DistRefs) > 0,
    ?line 0 = reference_type_count(control, DistRefs),
    % Get rid of all references to Node
    ?line exec(P, fun () -> exit(normal) end),
    ?line wait_until(fun () -> not is_process_alive(P) end),
    lists:foreach(fun (Proc) -> garbage_collect(Proc) end, processes()),
    ?line false = get_node_references({Node,Creation}),
    ?line false = get_dist_references(Node),
    ?line false = lists:member(Node, nodes(known)),
    ?line nc_refc_check(node()),
    ?line ok.

%%
%% Test case: ets_refc
%%
ets_refc(doc) ->
    ["Tests that external reference counts are incremented and decremented "
     "as they should for data stored in ets tables."];
ets_refc(suite) -> [];
ets_refc(Config) when is_list(Config) ->
    ?line RNode = {get_nodename(), 1},
    ?line RPid = mk_pid(RNode, 4711, 2),
    ?line RPort = mk_port(RNode, 4711),
    ?line RRef = mk_ref(RNode, [4711, 47, 11]),
    ?line Tab = ets:new(ets_refc, []),
    ?line 0 = reference_type_count(ets, get_node_references(RNode)),
    ?line true = ets:insert(Tab, [{a, self()},
				  {b, RPid},
				  {c, hd(erlang:ports())},
				  {d, RPort},
				  {e, make_ref()}]),
    ?line 2 = reference_type_count(ets, get_node_references(RNode)),
    ?line true = ets:insert(Tab, {f, RRef}),
    ?line 3 = reference_type_count(ets, get_node_references(RNode)),
    ?line true = ets:delete(Tab, d),
    ?line 2 = reference_type_count(ets, get_node_references(RNode)),
    ?line true = ets:delete_all_objects(Tab),
    ?line 0 = reference_type_count(ets, get_node_references(RNode)),
    ?line true = ets:insert(Tab, [{b, RPid}, {e, make_ref()}]),
    ?line 1 = reference_type_count(ets, get_node_references(RNode)),
    ?line true = ets:delete(Tab),
    ?line 0 = reference_type_count(ets, get_node_references(RNode)),
    ?line nc_refc_check(node()),
    ?line ok.

%%
%% Test case: match_spec_refc
%%
match_spec_refc(doc) ->
    ["Tests that external reference counts are incremented and decremented "
     "as they should for data stored in match specifications."];
match_spec_refc(suite) -> [];
match_spec_refc(Config) when is_list(Config) ->
    ?line RNode = {get_nodename(), 1},
    ?line RPid = mk_pid(RNode, 4711, 2),
    ?line RPort = mk_port(RNode, 4711),
    ?line RRef = mk_ref(RNode, [4711, 47, 11]),
    ?line ok = do_match_spec_test(RNode, RPid, RPort, RRef),
    ?line garbage_collect(),
    ?line NodeRefs = get_node_references(RNode),
    ?line 0 = reference_type_count(binary, NodeRefs),
    ?line 0 = reference_type_count(ets, NodeRefs),
    ?line nc_refc_check(node()),
    ?line ok.

do_match_spec_test(RNode, RPid, RPort, RRef) ->
    ?line Tab = ets:new(match_spec_refc, []),
    ?line true = ets:insert(Tab, [{a, RPid, RPort, RRef},
				  {b, self(), RPort, RRef},
				  {c, RPid, RPort, make_ref()},
				  {d, RPid, RPort, RRef}]),
    ?line {M1, C1} = ets:select(Tab, [{{'$1',RPid,RPort,RRef},[],['$1']}], 1),
    ?line NodeRefs = get_node_references(RNode),
    ?line 3 = reference_type_count(binary, NodeRefs),
    ?line 10 = reference_type_count(ets, NodeRefs),
    ?line {M2, C2} = ets:select(C1),
    ?line '$end_of_table' = ets:select(C2),
    ?line ets:delete(Tab),
    ?line [a,d] = lists:sort(M1++M2),
    ?line ok.
    

%%
%% Test case: ets_refc
%%
timer_refc(doc) ->
    ["Tests that external reference counts are incremented and decremented "
     "as they should for data stored in bif timers."];
timer_refc(suite) -> [];
timer_refc(Config) when is_list(Config) ->
    ?line RNode = {get_nodename(), 1},
    ?line RPid = mk_pid(RNode, 4711, 2),
    ?line RPort = mk_port(RNode, 4711),
    ?line RRef = mk_ref(RNode, [4711, 47, 11]),
    ?line 0 = reference_type_count(timer, get_node_references(RNode)),
    ?line Pid = spawn(fun () -> receive after infinity -> ok end end),
    ?line erlang:start_timer(10000, Pid, {RPid, RPort, RRef}),
    ?line 3 = reference_type_count(timer, get_node_references(RNode)),
    ?line exit(Pid, kill),
    ?line Mon = erlang:monitor(process, Pid),
    ?line receive {'DOWN', Mon, process, Pid, _} -> ok end,
    ?line 0 = reference_type_count(timer, get_node_references(RNode)),
    ?line erlang:send_after(500, Pid, {timer, RPid, RPort, RRef}),
    ?line 0 = reference_type_count(timer, get_node_references(RNode)),
    ?line erlang:send_after(500, self(), {timer, RPid, RPort, RRef}),
    ?line erlang:send_after(400, bananfluga, {timer, RPid, RPort, RRef}),
    ?line 6 = reference_type_count(timer, get_node_references(RNode)),
    ?line receive {timer, RPid, RPort, RRef} -> ok end,
    ?line 0 = reference_type_count(timer, get_node_references(RNode)),
    ?line nc_refc_check(node()),
    ?line ok.

otp_4715(doc) -> [];
otp_4715(suite) -> [];
otp_4715(Config) when is_list(Config) ->
    case ?t:is_release_available("r9b") of
	true -> otp_4715_1(Config);
	false -> {skip,"No R9B found"}
    end.

otp_4715_1(Config) ->
    case erlang:system_info(compat_rel) of
	9 ->
	    ?line run_otp_4715(Config);
	_ ->
	    ?line Pa = filename:dirname(code:which(?MODULE)),
	    ?line ?t:run_on_shielded_node(fun () ->
						  run_otp_4715(Config)
					  end,
					  "+R9 -pa " ++ Pa)
    end.

run_otp_4715(Config) when is_list(Config) ->
    ?line erts_debug:set_internal_state(available_internal_state, true),
    ?line PidList = [mk_pid({a@b, 1}, 4710, 2),
		     mk_pid({a@b, 1}, 4712, 1),
		     mk_pid({c@b, 1}, 4711, 1),
		     mk_pid({b@b, 3}, 4711, 1),
		     mk_pid({b@b, 2}, 4711, 1)],

    ?line R9Sorted = old_mod:sort_on_old_node(PidList),
    ?line R9Sorted = lists:sort(PidList).

pid_wrap(doc) -> [];
pid_wrap(suite) -> [];
pid_wrap(Config) when is_list(Config) -> ?line pp_wrap(pid).

port_wrap(doc) -> [];
port_wrap(suite) -> [];
port_wrap(Config) when is_list(Config) ->
    ?line case ?t:os_type() of
	      {unix, _} ->
		  ?line pp_wrap(port);
	      _ ->
		  ?line {skip, "Only run on unix"}
	  end.

get_next_id(pid) ->
    erts_debug:get_internal_state(next_pid);
get_next_id(port) ->
    erts_debug:get_internal_state(next_port).

set_next_id(pid, N) ->
    erts_debug:set_internal_state(next_pid, N);
set_next_id(port, N) ->
    erts_debug:set_internal_state(next_port, N).

pp_wrap(What) ->
    ?line N = set_high_pp_next(What),
    ?line Cre = N + 100,
    ?line ?t:format("no creations = ~p~n", [Cre]),
    ?line PreCre = get_next_id(What),
    ?line ?t:format("pre creations = ~p~n", [PreCre]),
    ?line true = is_integer(PreCre),
    ?line do_pp_creations(What, Cre),
    ?line PostCre = get_next_id(What),
    ?line ?t:format("post creations = ~p~n", [PostCre]),
    ?line true = is_integer(PostCre),
    ?line true = PreCre > PostCre,
    ?line Now = set_next_id(What, ?MAX_PIDS_PORTS div 2),
    ?line ?t:format("reset to = ~p~n", [Now]),
    ?line true = is_integer(Now),
    ?line ok.

set_high_pp_next(What) ->
    ?line set_high_pp_next(What, ?MAX_PIDS_PORTS-1).
    
set_high_pp_next(What, N) ->
    ?line M = set_next_id(What, N),
    ?line true = is_integer(M),
    ?line case {M >= N, M =< ?MAX_PIDS_PORTS} of
	      {true, true} ->
		  ?line ?MAX_PIDS_PORTS - M + 1;
	      _ ->
		  ?line set_high_pp_next(What, N - 100)
	  end.

do_pp_creations(_What, N) when is_integer(N), N =< 0 ->
    ?line done;
do_pp_creations(pid, N) when is_integer(N) ->
    %% Create new pid and make sure it works...
    ?line Me = self(),
    ?line Ref = make_ref(),
    ?line Pid = spawn_link(fun () ->
				   receive
				       Ref ->
					   Me ! Ref
				   end
			   end),
    ?line Pid ! Ref,
    ?line receive
	      Ref ->
		  ?line do_pp_creations(pid, N - 1)
	  end;
do_pp_creations(port, N) when is_integer(N) ->
    %% Create new port and make sure it works...
    ?line "hej" = os:cmd("echo hej") -- "\n",
    ?line do_pp_creations(port, N - 1).

bad_nc(doc) -> [];
bad_nc(suite) -> [];
bad_nc(Config) when is_list(Config) ->
    % Make sure emulator don't crash on bad node containers...
    ?line MaxPidNum = (1 bsl 15) - 1,
    ?line MaxPidSer = ?MAX_PIDS_PORTS bsr 15,
    ?line ThisNode = {node(), erlang:system_info(creation)},
    ?line {'EXIT', {badarg, mk_pid, _}}
	= (catch mk_pid(ThisNode, MaxPidNum + 1, 17)),
    ?line {'EXIT', {badarg, mk_pid, _}}
	= (catch mk_pid(ThisNode, 4711, MaxPidSer + 1)),
    ?line {'EXIT', {badarg, mk_port, _}}
	= (catch mk_port(ThisNode, ?MAX_PIDS_PORTS + 1)),
    ?line {'EXIT', {badarg, mk_ref, _}}
	= (catch mk_ref(ThisNode,[(1 bsl 18), 4711, 4711])),
    ?line {'EXIT', {badarg, mk_ref, _}}
	= (catch mk_ref(ThisNode, [4711, 4711, 4711, 4711, 4711, 4711, 4711])),
    ?line RemNode = {x@y, 2},
    ?line {'EXIT', {badarg, mk_pid, _}}
	= (catch mk_pid(RemNode, MaxPidNum + 1, MaxPidSer)),
    ?line {'EXIT', {badarg, mk_pid, _}}
	= (catch mk_pid(RemNode, MaxPidNum, MaxPidSer + 1)),
    ?line {'EXIT', {badarg, mk_port, _}}
	= (catch mk_port(RemNode, ?MAX_PIDS_PORTS + 1)),
    ?line {'EXIT', {badarg, mk_ref, _}}
	= (catch mk_ref(RemNode, [(1 bsl 18), 4711, 4711])),
    ?line {'EXIT', {badarg, mk_ref, _}}
	= (catch mk_ref(RemNode, [4711, 4711, 4711, 4711, 4711, 4711, 4711])),
    ?line BadNode = {x@y, 4},
    ?line {'EXIT', {badarg, mk_pid, _}}
	= (catch mk_pid(BadNode, 4711, 17)),
    ?line {'EXIT', {badarg, mk_port, _}}
	= (catch mk_port(BadNode, 4711)),
    ?line {'EXIT', {badarg, mk_ref, _}}
	= (catch mk_ref(BadNode, [4711, 4711, 17])),
    ?line ok.



-define(NO_PIDS, 1000000).

unique_pid(doc) -> [];
unique_pid(suite) -> [];
unique_pid(Config) when is_list(Config) ->
    case catch erlang:system_info(modified_timing_level) of
	Level when is_integer(Level) ->
	    {skip,
	     "Modified timing (level " ++ integer_to_list(Level)
	     ++ ") is enabled. spawn() is too slow for this "
	     " test when modified timing is enabled."};
	_ ->
	    ?line ?NO_PIDS = length(lists:usort(mkpidlist(?NO_PIDS, []))),
	    ?line ok
    end.
    
mkpidlist(0, Ps) -> Ps;
mkpidlist(N, Ps) -> mkpidlist(N-1, [spawn(fun () -> ok end)|Ps]).


iter_max_procs(doc) -> [];
iter_max_procs(suite) -> [];
iter_max_procs(Config) when is_list(Config) ->
    ?line NoMoreTests = make_ref(),
    ?line erlang:send_after(10000, self(), NoMoreTests),
    ?line Res = chk_max_proc_line(),
    ?line Res = chk_max_proc_line(),
    ?line done = chk_max_proc_line_until(NoMoreTests, Res),
    ?line {comment,
	   io_lib:format("max processes = ~p; "
			 "process line length = ~p",
			 [element(2, Res), element(1, Res)])}.
    
    
max_proc_line(Root, Parent, N) ->
    Me = self(),
    case catch spawn_link(fun () -> max_proc_line(Root, Me, N+1) end) of
	{'EXIT', {system_limit, _}} when Root /= self() ->
	    Root ! {proc_line_length, N, self()},
	    receive remove_proc_line -> Parent ! {exiting, Me} end;
	P when is_pid(P), Root =/= self() ->
	    receive {exiting, P} -> Parent ! {exiting, Me} end;
	P when is_pid(P) ->
	    P;
	Unexpected ->
	    exit({unexpected_spawn_result, Unexpected})
    end.

chk_max_proc_line() ->
    ?line Child = max_proc_line(self(), self(), 0),
    ?line receive
	      {proc_line_length, PLL, End} ->
		  ?line PC = erlang:system_info(process_count),
		  ?line LP = length(processes()),
		  ?line ?t:format("proc line length = ~p; "
				  "process count = ~p; "
				  "length processes = ~p~n",
				  [PLL, PC, LP]),
		  ?line End ! remove_proc_line,
		  ?line PC = LP,
		  ?line receive {exiting, Child} -> ok end,
		  ?line {PLL, PC}
	  end.

chk_max_proc_line_until(NoMoreTests, Res) ->
    receive
	NoMoreTests ->
	    ?line done
    after 0 ->
	    ?line Res = chk_max_proc_line(),
	    ?line chk_max_proc_line_until(NoMoreTests, Res)
    end.

%%
%% -- Internal utils ---------------------------------------------------------
%%

-define(ND_REFS, erts_debug:get_internal_state(node_and_dist_references)).

node_container_refc_check(Node) when is_atom(Node) ->
    AIS = available_internal_state(true),
    nc_refc_check(Node),
    available_internal_state(AIS).

nc_refc_check(Node) when is_atom(Node) ->
    Ref = make_ref(),
    Self = self(),
    ?t:format("Starting reference count check of node ~w~n", [Node]),
    spawn_link(Node,
	       fun () ->
		       {{node_references, NodeRefs},
			{dist_references, DistRefs}} = ?ND_REFS,
		       check_nd_refc({node(), erlang:system_info(creation)},
				     NodeRefs,
				     DistRefs,
				     fun (ErrMsg) ->
					     Self ! {Ref, ErrMsg, failed},
					     exit(normal)
				     end),
		       Self ! {Ref, succeded}
	       end),
    receive
	{Ref, ErrorMsg, failed} ->
	    ?t:format("~s~n", [ErrorMsg]),
	    ?t:fail(reference_count_check_failed);
	{Ref, succeded} ->
	    ?t:format("Reference count check of node ~w succeded!~n", [Node]),
	    ok
    end.

check_nd_refc({ThisNodeName, ThisCreation}, NodeRefs, DistRefs, Fail) ->
    case catch begin
		   check_refc(ThisNodeName,ThisCreation,"node table",NodeRefs),
		   check_refc(ThisNodeName,ThisCreation,"dist table",DistRefs),
		   ok
	       end of
	ok ->
	    ok;
	{'EXIT', Reason} ->
	    {Y,Mo,D} = date(),
	    {H,Mi,S} = time(),
	    ErrMsg = io_lib:format("~n"
				   "*** Reference count check of node ~w "
				   "failed (~p) at ~w~w~w ~w:~w:~w~n"
				   "*** Node table references:~n ~p~n"
				   "*** Dist table references:~n ~p~n",
				   [node(), Reason, Y, Mo, D, H, Mi, S,
				    NodeRefs, DistRefs]),
	    Fail(lists:flatten(ErrMsg))
    end.


check_refc(ThisNodeName,ThisCreation,Table,EntryList) when is_list(EntryList) ->
    lists:foreach(
      fun ({Entry, Refc, ReferrerList}) ->
	      FoundRefs =
		  lists:foldl(
		    fun ({_Referrer, ReferencesList}, A1) ->
			    A1 + lists:foldl(fun ({_T,Rs},A2) ->
						     A2+Rs
					     end,
					     0,
					     ReferencesList)
		    end,
		    0,
		    ReferrerList),
	      
	      %% Reference count equals found references ?
	      case Refc =:= FoundRefs of
		  true ->
		      ok;
		  false ->
		      exit({invalid_reference_count, Table, Entry})
	      end,

	      %% All entries in table referred to?
	      case {Entry, Refc} of
		  {ThisNodeName, 0} -> ok;
		  {{ThisNodeName, ThisCreation}, 0} -> ok;
		  {_, 0} -> exit({not_referred_entry_in_table, Table, Entry});
		  {_, _} -> ok 
	      end

      end,
      EntryList),
    ok.

get_node_references({NodeName, Creation} = Node) when is_atom(NodeName),
						      is_integer(Creation) ->
    {{node_references, NodeRefs},
	   {dist_references, DistRefs}} = ?ND_REFS,
    check_nd_refc({node(), erlang:system_info(creation)},
			NodeRefs,
			DistRefs,
			fun (ErrMsg) ->
				?t:format("~s", [ErrMsg]),
				?t:fail(reference_count_check_failed)
			end),
    find_references(Node, NodeRefs).

get_dist_references(NodeName) when is_atom(NodeName) ->
    ?line {{node_references, NodeRefs},
	   {dist_references, DistRefs}} = ?ND_REFS,
    ?line check_nd_refc({node(), erlang:system_info(creation)},
			NodeRefs,
			DistRefs,
			fun (ErrMsg) ->
				?line ?t:format("~s", [ErrMsg]),
				?line ?t:fail(reference_count_check_failed)
			end),
    ?line find_references(NodeName, DistRefs).

find_references(N, NRefList) ->
    case lists:keysearch(N, 1, NRefList) of
	      {value, {N, _, ReferrersList}} -> ReferrersList;
	      _ -> false
	  end.

%% Currently unused
% refering_entity_type(RefererType, ReferingEntities) ->
%     lists:filter(fun ({{RT, _}, _}) when RT == RefererType ->
% 			 true;
% 		     (_) ->
% 			 false
% 		 end,
% 		 ReferingEntities).

refering_entity_id(ReferingEntityId, [{ReferingEntityId,_} = ReferingEntity
				      | _ReferingEntities]) ->
    ReferingEntity;
refering_entity_id(ReferingEntityId, [_ | ReferingEntities]) ->
    refering_entity_id(ReferingEntityId, ReferingEntities);
refering_entity_id(_, []) ->
    false.

reference_type_count(_, false) ->
    0;
reference_type_count(Type, {_, _ReferenceCountList} = ReferingEntity) ->
    reference_type_count(Type, [ReferingEntity]);
reference_type_count(Type, ReferingEntities) when is_list(ReferingEntities) ->
    lists:foldl(fun ({_, ReferenceCountList}, Acc1) ->
			lists:foldl(fun ({T, N}, Acc2) when T == Type ->
					    N + Acc2;
					(_, Acc2) ->
					    Acc2
				    end,
				    Acc1,
				    ReferenceCountList)
		end,
		0,
		ReferingEntities).


start_node(Name, Args) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line Res = test_server:start_node(Name,
				       slave,
				       [{args, "-pa "++Pa++" "++Args}]),
    ?line {ok, Node} = Res,
    ?line rpc:call(Node, erts_debug, set_internal_state,
		   [available_internal_state, true]),
    ?line Res.
    
start_node(Name) ->
    ?line start_node(Name, "").

stop_node(Node) ->
    ?line nc_refc_check(Node),
    ?line true = test_server:stop_node(Node).

hostname() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

wait_until(Pred) ->
    case Pred() of
	true -> ok;
	false -> receive after 100 -> wait_until(Pred) end
    end.


get_nodefirstname() ->
    {A, B, C} = now(),
    list_to_atom(atom_to_list(?MODULE)
		 ++ "-"
		 ++ integer_to_list(A)
		 ++ "-"
		 ++ integer_to_list(B)
		 ++ "-"
		 ++ integer_to_list(C)).

get_nodename() ->
    {A, B, C} = now(),
    list_to_atom(atom_to_list(?MODULE)
		 ++ "-"
		 ++ integer_to_list(A)
		 ++ "-"
		 ++ integer_to_list(B)
		 ++ "-"
		 ++ integer_to_list(C)
		 ++ "@"
		 ++ hostname()).
    


-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).


uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).



mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    mk_pid({atom_to_list(NodeName), Creation}, Number, Serial);
mk_pid({NodeName, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					?PID_EXT,
					?ATOM_EXT,
					uint16_be(length(NodeName)),
					NodeName,
					uint32_be(Number),
					uint32_be(Serial),
					uint8(Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeName, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    mk_port({atom_to_list(NodeName), Creation}, Number);
mk_port({NodeName, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PORT_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeName, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    mk_ref({atom_to_list(NodeName), Creation}, Numbers);
mk_ref({NodeName, Creation}, [Number]) when is_list(NodeName),
					    is_integer(Creation),
					    is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?REFERENCE_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, [Number]]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end;
mk_ref({NodeName, Creation}, Numbers) when is_list(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?NEW_REFERENCE_EXT,
					      uint16_be(length(Numbers)),
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint8(Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

exec_loop() ->
    receive
	{exec_fun, Fun} when is_function(Fun) ->
	    Fun();
	{sync_exec_fun, From, Fun} when is_pid(From), is_function(Fun)  ->
	    From ! {sync_exec_fun_res, self(), Fun()}
    end,
    exec_loop().

spawn_execer(Node) ->
    spawn(Node, fun () -> exec_loop() end).

spawn_link_execer(Node) ->
    spawn_link(Node, fun () -> exec_loop() end).

exec(Pid, Fun) when is_pid(Pid), is_function(Fun) ->
    Pid ! {exec_fun, Fun}.

sync_exec(Pid, Fun) when is_pid(Pid), is_function(Fun) ->
    Pid ! {sync_exec_fun, self(), Fun},
    receive
	{sync_exec_fun_res, Pid, Res} ->
	    Res
    end.
