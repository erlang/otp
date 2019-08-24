%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2018. All Rights Reserved.
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

%%%----------------------------------------------------------------------
%%% File    : node_container_SUITE.erl
%%% Author  : Rickard <rickard.green@uab.ericsson.se>
%%% Purpose : 
%%% Created : 24 Jul 2002 by Rickard <rickard.green@uab.ericsson.se>
%%%----------------------------------------------------------------------

-module(node_container_SUITE).
-author('rickard.green@uab.ericsson.se').

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
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
         pid_wrap/1,
         port_wrap/1,
         bad_nc/1,
         unique_pid/1,
         iter_max_procs/1,
         magic_ref/1,
         dist_entry_gc/1,
         persistent_term/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 12}}].


all() -> 
    [dist_entry_gc, term_to_binary_to_term_eq, round_trip_eq, cmp, ref_eq,
     node_table_gc, dist_link_refc, dist_monitor_refc,
     node_controller_refc, ets_refc, match_spec_refc,
     timer_refc, pid_wrap, port_wrap, bad_nc,
     unique_pid, iter_max_procs,
     magic_ref, persistent_term].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(node_tab_delayed_delete, -1), %% restore original value
    erts_test_utils:available_internal_state(false).

init_per_testcase(_Case, Config) when is_list(Config) ->
    erts_test_utils:available_internal_state(true),
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

-define(MAX_PIDS_PORTS, ((1 bsl 28) - 1)).

%%
%% Test case: term_to_binary_to_term_eq
%%
%% Tests that node container terms that are converted to external format
%% and back stay equal to themselves.
term_to_binary_to_term_eq(Config) when is_list(Config) ->
    ThisNode = {node(), erlang:system_info(creation)},
    % Get local node containers
    LPid = self(),
    LXPid = mk_pid(ThisNode, 32767, 8191),
    LPort = hd(erlang:ports()),
    LXPort = mk_port(ThisNode, 268435455),
    LLRef = make_ref(),
    LHLRef = mk_ref(ThisNode, [47, 11]),
    LSRef = mk_ref(ThisNode, [4711]),
    % Test local nc:s
    LPid = binary_to_term(term_to_binary(LPid)),
    LXPid = binary_to_term(term_to_binary(LXPid)),
    LPort = binary_to_term(term_to_binary(LPort)),
    LXPort = binary_to_term(term_to_binary(LXPort)),
    LLRef = binary_to_term(term_to_binary(LLRef)),
    LHLRef = binary_to_term(term_to_binary(LHLRef)),
    LSRef = binary_to_term(term_to_binary(LSRef)),
    % Get remote node containers
    ttbtteq_do_remote({get_nodename(), 3}),
    ttbtteq_do_remote({get_nodename(), 4}),
    ttbtteq_do_remote({get_nodename(), 16#adec0ded}),
    nc_refc_check(node()),
    ok.

ttbtteq_do_remote(RNode) ->
    RPid = mk_pid(RNode, 4711, 1),
    RXPid = mk_pid(RNode, 32767, 8191),
    RPort = mk_port(RNode, 4711),
    RXPort = mk_port(RNode, 268435455),
    RLRef = mk_ref(RNode, [4711, 4711, 4711]),
    RHLRef = mk_ref(RNode, [4711, 4711]),
    RSRef = mk_ref(RNode, [4711]),
    % Test remote nc:s
    RPid = binary_to_term(term_to_binary(RPid)),
    RXPid = binary_to_term(term_to_binary(RXPid)),
    RPort = binary_to_term(term_to_binary(RPort)),
    RXPort = binary_to_term(term_to_binary(RXPort)),
    RLRef = binary_to_term(term_to_binary(RLRef)),
    RHLRef = binary_to_term(term_to_binary(RHLRef)),
    RSRef = binary_to_term(term_to_binary(RSRef)),
    ok.


%%
%% Test case: round_trip_eq
%%
%% Tests that node containers that are sent between nodes stay equal to themselves.
round_trip_eq(Config) when is_list(Config) ->
    ThisNode = {node(), erlang:system_info(creation)},
    NodeFirstName = get_nodefirstname(),
    {ok, Node} = start_node(NodeFirstName),
    Self = self(),
    RPid = spawn_link(Node,
                      fun () ->
                              receive
                                  {Self, Data} ->
                                      Self ! {self(), Data}
                              end
                      end),
    SentPid = self(),
    SentXPid = mk_pid(ThisNode, 17471, 8190),
    SentPort = hd(erlang:ports()),
    SentXPort = mk_port(ThisNode, 268435451),
    SentLRef = make_ref(),
    SentHLRef = mk_ref(ThisNode, [4711, 17]),
    SentSRef = mk_ref(ThisNode, [4711]),
    RPid ! {Self, {SentPid,
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
            stop_node(Node),
            SentPid = RecPid,
            SentXPid = RecXPid,
            SentPort = RecPort,
            SentXPort = RecXPort,
            SentLRef = RecLRef,
            SentHLRef = RecHLRef,
            SentSRef = RecSRef,
            nc_refc_check(node()),
            ok
    end.



%%
%% Test case: cmp
%%
%% Tests that Erlang term comparison works as it should on node containers.
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
    true = 1 < IPid,
    true = 1.3 < IPid,
    true = (1 bsl 64) < IPid,
    true = an_atom < IPid,
    true = IRef < IPid,
    true = ERef < IPid,
    true = fun () -> a_fun end < IPid,
    true = IPort < IPid,
    true = EPort < IPid,
    true = IPid < {a, tuple},
    true = IPid < [],
    true = IPid < [a|cons],
    true = IPid < <<"a binary">>,

    true = 1 < EPid,
    true = 1.3 < EPid,
    true = (1 bsl 64) < EPid,
    true = an_atom < EPid,
    true = IRef < EPid,
    true = ERef < EPid,
    true = fun () -> a_fun end < EPid,
    true = IPort < EPid,
    true = EPort < EPid,
    true = EPid < {a, tuple},
    true = EPid < [],
    true = EPid < [a|cons],
    true = EPid < <<"a binary">>,

    %% Test ports --------------------------------------------------
    true = 1 < IPort,
    true = 1.3 < IPort,
    true = (1 bsl 64) < IPort,
    true = an_atom < IPort,
    true = IRef < IPort,
    true = ERef < IPort,
    true = fun () -> a_fun end < IPort,
    true = IPort < IPid,
    true = IPort < EPid,
    true = IPort < {a, tuple},
    true = IPort < [],
    true = IPort < [a|cons],
    true = IPort < <<"a binary">>,

    true = 1 < EPort,
    true = 1.3 < EPort,
    true = (1 bsl 64) < EPort,
    true = an_atom < EPort,
    true = IRef < EPort,
    true = ERef < EPort,
    true = fun () -> a_fun end < EPort,
    true = EPort < IPid,
    true = EPort < EPid,
    true = EPort < {a, tuple},
    true = EPort < [],
    true = EPort < [a|cons],
    true = EPort < <<"a binary">>,

    %% Test refs ----------------------------------------------------
    true = 1 < IRef,
    true = 1.3 < IRef,
    true = (1 bsl 64) < IRef,
    true = an_atom < IRef,
    true = IRef < fun () -> a_fun end,
    true = IRef < IPort,
    true = IRef < EPort,
    true = IRef < IPid,
    true = IRef < EPid,
    true = IRef < {a, tuple},
    true = IRef < [],
    true = IRef < [a|cons],
    true = IRef < <<"a binary">>,

    true = 1 < ERef,
    true = 1.3 < ERef,
    true = (1 bsl 64) < ERef,
    true = an_atom < ERef,
    true = ERef < fun () -> a_fun end,
    true = ERef < IPort,
    true = ERef < EPort,
    true = ERef < IPid,
    true = ERef < EPid,
    true = ERef < {a, tuple},
    true = ERef < [],
    true = ERef < [a|cons],
    true = ERef < <<"a binary">>,


    %% Intra type comparison ---------------------------------------------------


    %% Test pids ----------------------------------------------------
    %%
    %% Significance (most -> least):
    %%   serial, number, nodename, creation
    %%

    Pid = mk_pid({b@b, 2}, 4711, 1),

    true = mk_pid({a@b, 1}, 4710, 2) > Pid,
    true = mk_pid({a@b, 1}, 4712, 1) > Pid,
    true = mk_pid({c@b, 1}, 4711, 1) > Pid,
    true = mk_pid({b@b, 3}, 4711, 1) > Pid,
    true = mk_pid({b@b, 2}, 4711, 1) =:= Pid,

    %% Test ports ---------------------------------------------------
    %%
    %% Significance (most -> least):
    %%   nodename, creation, number 
    %%
    %% OBS: Comparison between ports has changed in R9. This
    %%      since it wasn't stable in R8 (and eariler releases).
    %%      Significance used to be: dist_slot, number,
    %%      creation.

    Port = mk_port({b@b, 2}, 4711),

    true = mk_port({c@b, 1}, 4710) > Port,
    true = mk_port({b@b, 3}, 4710) > Port,
    true = mk_port({b@b, 2}, 4712) > Port,
    true = mk_port({b@b, 2}, 4711) =:= Port,

    %% Test refs ----------------------------------------------------
    %% Significance (most -> least):
    %% nodename, creation, (number high, number mid), number low, 
    %%
    %% OBS: Comparison between refs has changed in R9. This
    %%      since it wasn't stable in R8 (and eariler releases).
    %%      Significance used to be: dist_slot, number,
    %%      creation.
    %%

    Ref = mk_ref({b@b, 2}, [4711, 4711, 4711]),

    true = mk_ref({c@b, 1}, [4710, 4710, 4710]) > Ref,
    true = mk_ref({b@b, 3}, [4710, 4710, 4710]) > Ref,
    true = mk_ref({b@b, 2}, [4710, 4710, 4712]) > Ref,
    true = mk_ref({b@b, 2}, [4710, 4712, 4711]) > Ref,
    true = mk_ref({b@b, 2}, [4712, 4711, 4711]) > Ref,
    true = mk_ref({b@b, 2}, [4711, 4711, 4711]) =:= Ref,

    ok.

%%
%% Test case: ref_eq
%%
%% Test that one word refs works
ref_eq(Config) when is_list(Config) ->
    ThisNode = {node(), erlang:system_info(creation)},
    AnotherNode = {get_nodename(),2},
    LLongRef = mk_ref(ThisNode, [4711, 0, 0]),
    LHalfLongRef = mk_ref(ThisNode, [4711, 0]),
    LShortRef = mk_ref(ThisNode, [4711]),
    true = LLongRef =:= LShortRef,
    true = LLongRef =:= LHalfLongRef,
    true = LLongRef =:= LLongRef,
    true = LHalfLongRef =:= LShortRef,
    true = LHalfLongRef =:= LHalfLongRef,
    true = LShortRef =:= LShortRef,
    false = LShortRef == mk_ref(ThisNode, [4711, 0, 1]), % Not any more
    RLongRef = mk_ref(AnotherNode, [4711, 0, 0]),
    RHalfLongRef = mk_ref(AnotherNode, [4711, 0]),
    RShortRef = mk_ref(AnotherNode, [4711]),
    true = RLongRef =:= RShortRef,
    true = RLongRef =:= RHalfLongRef,
    true = RLongRef =:= RLongRef,
    true = RHalfLongRef =:= RShortRef,
    true = RHalfLongRef =:= RHalfLongRef,
    true = RShortRef =:= RShortRef,
    false = RShortRef == mk_ref(AnotherNode, [4711, 0, 1]), % Not any more
    nc_refc_check(node()),
    ok.

%%
%% Test case: node_table_gc
%%
%% Tests that node tables are garbage collected.
node_table_gc(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(node_tab_delayed_delete, 0),
    PreKnown = nodes(known),
    io:format("PreKnown = ~p~n", [PreKnown]),
    make_node_garbage(0, 200000, 1000, []),
    receive after 1000 -> ok end, %% Wait for thread progress...
    PostKnown = nodes(known),
    PostAreas = erlang:system_info(allocated_areas),
    io:format("PostKnown = ~p~n", [PostKnown]),
    io:format("PostAreas = ~p~n", [PostAreas]),
    true = length(PostKnown) =< length(PreKnown),
    nc_refc_check(node()),
    erts_debug:set_internal_state(node_tab_delayed_delete, -1), %% restore original value
    ok.

make_node_garbage(N, L, I, Ps) when N < L ->
    Self = self(),
    P = spawn_link(fun () ->
                           % Generate two node entries and one dist
                           % entry per node name
                           PL1 = make_faked_pid_list(N,
                                                     I div 2,
                                                     1),
                           put(a, PL1),
                           PL2 = make_faked_pid_list(N,
                                                     I div 2,
                                                     2),
                           put(b, PL2),
                           Self ! {self(), length(nodes(known))}
                   end),
    receive
        {P, KnownLength} ->
            true = KnownLength >= I div 2
    end,
    make_node_garbage(N+(I div 2)*2, L, I, [P|Ps]);
make_node_garbage(_, _, _, Ps) ->
    %% Cleanup garbage...
    ProcIsCleanedUp
    = fun (Proc) ->
              undefined == erts_debug:get_internal_state({process_status,
                                                          Proc})
      end,
    lists:foreach(fun (P) -> wait_until(fun () -> ProcIsCleanedUp(P) end) end,
                  Ps),
    ok.


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
%% Tests that external reference counts are incremented and decremented
%% as they should for distributed links
dist_link_refc(Config) when is_list(Config) ->
    NodeFirstName = get_nodefirstname(),
    {ok, Node} = start_node(NodeFirstName),
    RP = spawn_execer(Node),
    LP = spawn_link_execer(node()),
    true = sync_exec(RP, fun () -> link(LP) end),
    wait_until(fun () ->
                       {links, Links} = process_info(LP, links),
                       lists:member(RP, Links)
               end),
    NodeCre = sync_exec(RP, fun() -> erlang:system_info(creation) end),
    1 = reference_type_count(
          link,
          refering_entity_id({process, LP},
                             get_node_references({Node, NodeCre}))),
    exec(RP, fun() -> exit(normal) end),
    wait_until(fun () ->
                       {links, Links} = process_info(LP, links),
                       not lists:member(RP, Links)
               end),
    0 = reference_type_count(
          link,
          refering_entity_id({process, LP},
                             get_node_references({Node, NodeCre}))),
    exit(LP, normal),
    stop_node(Node),
    nc_refc_check(node()),
    ok.


%%
%% Test case: dist_monitor_refc
%%
%% Tests that external reference counts are incremented and decremented
%% as they should for distributed monitors
dist_monitor_refc(Config) when is_list(Config) ->
    NodeFirstName = get_nodefirstname(),
    {ok, Node} = start_node(NodeFirstName),
    RP = spawn_execer(Node),
    LP = spawn_link_execer(node()),
    RMon = sync_exec(RP, fun () -> erlang:monitor(process, LP) end),
    true = is_reference(RMon),
    LMon = sync_exec(LP, fun () -> erlang:monitor(process, RP) end),
    true = is_reference(LMon),
    NodeCre = sync_exec(RP, fun() -> erlang:system_info(creation) end),
    wait_until(fun () ->
                       {monitored_by, MonBy}
                       = process_info(LP, monitored_by),
                       {monitors, Mon}
                       = process_info(LP, monitors),
                       (lists:member(RP, MonBy)
                        and lists:member({process,RP}, Mon))
               end),
    3 = reference_type_count(
          monitor,
          refering_entity_id({process, LP},
                             get_node_references({Node, NodeCre}))),
    exec(RP, fun () -> exit(normal) end),
    wait_until(fun () ->
                       {monitored_by, MonBy}
                       = process_info(LP, monitored_by),
                       {monitors, Mon}
                       = process_info(LP, monitors),
                       ((not lists:member(RP, MonBy))
                        and (not lists:member({process,RP}, Mon)))
               end),
    ok = sync_exec(LP,
                   fun () ->
                           receive
                               {'DOWN', LMon, process, _, _} ->
                                   ok
                           end
                   end),
    0 = reference_type_count(
          link,
          refering_entity_id({process, LP},
                             get_node_references({Node, NodeCre}))),
    exit(LP, normal),
    stop_node(Node),
    nc_refc_check(node()),
    ok.


%%
%% Test case: node_controller_refc
%%
%% Tests that external reference counts are incremented and decremented
%% as they should for entities controlling a connections.
node_controller_refc(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    erts_debug:set_internal_state(node_tab_delayed_delete, 0),
    NodeFirstName = get_nodefirstname(),
    {ok, Node} = start_node(NodeFirstName),
    true = lists:member(Node, nodes()),
    1 = reference_type_count(control, get_dist_references(Node)),
    P = spawn_link_execer(node()),
    Node
    = sync_exec(P,
                fun () ->
                        put(remote_net_kernel,
                            rpc:call(Node,erlang,whereis,[net_kernel])),
                        node(get(remote_net_kernel))
                end),
    Creation = rpc:call(Node, erlang, system_info, [creation]),
    monitor_node(Node,true),
    stop_node(Node),
    receive {nodedown, Node} -> ok end,
    DistRefs = get_dist_references(Node),
    true = reference_type_count(node, DistRefs) > 0,
    0 = reference_type_count(control, DistRefs),
    % Get rid of all references to Node
    exec(P, fun () -> exit(normal) end),
    wait_until(fun () -> not is_process_alive(P) end),
    lists:foreach(fun (Proc) -> garbage_collect(Proc) end, processes()),
    false = get_node_references({Node,Creation}),
    wait_until(fun () ->
                       case get_dist_references(Node) of
                           false ->
                               true;
                           [{{system,thread_progress_delete_timer},
                             [{system,1}]}] ->
                               false;
                           Other ->
                               ct:fail(Other)
                       end
               end),
    false = lists:member(Node, nodes(known)),
    nc_refc_check(node()),
    erts_debug:set_internal_state(node_tab_delayed_delete, -1), %% restore original value
    ok.

%%
%% Test case: ets_refc
%%
%% Tests that external reference counts are incremented and decremented
%% as they should for data stored in ets tables.
ets_refc(Config) when is_list(Config) ->
    RNode = {get_nodename(), 1},
    RPid = mk_pid(RNode, 4711, 2),
    RPort = mk_port(RNode, 4711),
    RRef = mk_ref(RNode, [4711, 47, 11]),
    Tab = ets:new(ets_refc, []),
    0 = reference_type_count(ets, get_node_references(RNode)),
    true = ets:insert(Tab, [{a, self()},
                            {b, RPid},
                            {c, hd(erlang:ports())},
                            {d, RPort},
                            {e, make_ref()}]),
    2 = reference_type_count(ets, get_node_references(RNode)),
    true = ets:insert(Tab, {f, RRef}),
    3 = reference_type_count(ets, get_node_references(RNode)),
    true = ets:delete(Tab, d),
    2 = reference_type_count(ets, get_node_references(RNode)),
    true = ets:delete_all_objects(Tab),
    0 = reference_type_count(ets, get_node_references(RNode)),
    true = ets:insert(Tab, [{b, RPid}, {e, make_ref()}]),
    1 = reference_type_count(ets, get_node_references(RNode)),
    true = ets:delete(Tab),
    0 = reference_type_count(ets, get_node_references(RNode)),
    nc_refc_check(node()),
    ok.

%%
%% Test case: match_spec_refc
%%
%% Tests that external reference counts are incremented and decremented
%% as they should for data stored in match specifications.
match_spec_refc(Config) when is_list(Config) ->
    RNode = {get_nodename(), 1},
    RPid = mk_pid(RNode, 4711, 2),
    RPort = mk_port(RNode, 4711),
    RRef = mk_ref(RNode, [4711, 47, 11]),
    ok = do_match_spec_test(RNode, RPid, RPort, RRef),
    garbage_collect(),
    NodeRefs = get_node_references(RNode),
    0 = reference_type_count(binary, NodeRefs),
    0 = reference_type_count(ets, NodeRefs),
    nc_refc_check(node()),
    ok.

do_match_spec_test(RNode, RPid, RPort, RRef) ->
    Tab = ets:new(match_spec_refc, []),
    true = ets:insert(Tab, [{a, RPid, RPort, RRef},
                            {b, self(), RPort, RRef},
                            {c, RPid, RPort, make_ref()},
                            {d, RPid, RPort, RRef}]),
    {M1, C1} = ets:select(Tab, [{{'$1',RPid,RPort,RRef},[],['$1']}], 1),
    NodeRefs = get_node_references(RNode),
    3 = reference_type_count(binary, NodeRefs),
    10 = reference_type_count(ets, NodeRefs),
    {M2, C2} = ets:select(C1),
    '$end_of_table' = ets:select(C2),
    ets:delete(Tab),
    [a,d] = lists:sort(M1++M2),
    ok.


%%
%% Test case: ets_refc
%%
%% Tests that external reference counts are incremented and decremented
%% as they should for data stored in bif timers.
timer_refc(Config) when is_list(Config) ->
    RNode = {get_nodename(), 1},
    RPid = mk_pid(RNode, 4711, 2),
    RPort = mk_port(RNode, 4711),
    RRef = mk_ref(RNode, [4711, 47, 11]),
    0 = reference_type_count(timer, get_node_references(RNode)),
    Pid = spawn(fun () -> receive after infinity -> ok end end),
    erlang:start_timer(10000, Pid, {RPid, RPort, RRef}),
    3 = reference_type_count(timer, get_node_references(RNode)),
    exit(Pid, kill),
    Mon = erlang:monitor(process, Pid),
    receive {'DOWN', Mon, process, Pid, _} -> ok end,
    0 = reference_type_count(timer, get_node_references(RNode)),
    erlang:send_after(500, Pid, {timer, RPid, RPort, RRef}),
    0 = reference_type_count(timer, get_node_references(RNode)),
    erlang:send_after(500, self(), {timer, RPid, RPort, RRef}),
    erlang:send_after(400, bananfluga, {timer, RPid, RPort, RRef}),
    6 = reference_type_count(timer, get_node_references(RNode)),
    receive {timer, RPid, RPort, RRef} -> ok end,
    0 = reference_type_count(timer, get_node_references(RNode)),
    nc_refc_check(node()),
    ok.

pid_wrap(Config) when is_list(Config) -> pp_wrap(pid).

port_wrap(Config) when is_list(Config) ->
    case os:type() of
        {unix, _} ->
            pp_wrap(port);
        _ ->
            {skip, "Only run on unix"}
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
    N = set_high_pp_next(What),
    Cre = N + 100,
    io:format("no creations = ~p~n", [Cre]),
    PreCre = get_next_id(What),
    io:format("pre creations = ~p~n", [PreCre]),
    true = is_integer(PreCre),
    do_pp_creations(What, Cre),
    PostCre = get_next_id(What),
    io:format("post creations = ~p~n", [PostCre]),
    true = is_integer(PostCre),
    true = PreCre > PostCre,
    Now = set_next_id(What, ?MAX_PIDS_PORTS div 2),
    io:format("reset to = ~p~n", [Now]),
    true = is_integer(Now),
    ok.

set_high_pp_next(What) ->
    set_high_pp_next(What, ?MAX_PIDS_PORTS-1).

set_high_pp_next(What, N) ->
    M = set_next_id(What, N),
    true = is_integer(M),
    case {M >= N, M =< ?MAX_PIDS_PORTS} of
        {true, true} ->
            ?MAX_PIDS_PORTS - M + 1;
        _ ->
            set_high_pp_next(What, N - 100)
    end.

do_pp_creations(_What, N) when is_integer(N), N =< 0 ->
    done;
do_pp_creations(pid, N) when is_integer(N) ->
    %% Create new pid and make sure it works...
    Me = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun () ->
                             receive
                                 Ref ->
                                     Me ! Ref
                             end
                     end),
    Pid ! Ref,
    receive
        Ref ->
            do_pp_creations(pid, N - 1)
    end;
do_pp_creations(port, N) when is_integer(N) ->
    %% Create new port and make sure it works...
    "hej" = os:cmd("echo hej") -- "\n",
    do_pp_creations(port, N - 1).

bad_nc(Config) when is_list(Config) ->
    % Make sure emulator don't crash on bad node containers...
    MaxPidNum = (1 bsl 15) - 1,
    MaxPidSer = ?MAX_PIDS_PORTS bsr 15,
    ThisNode = {node(), erlang:system_info(creation)},
    {'EXIT', {badarg, mk_pid, _}}
    = (catch mk_pid(ThisNode, MaxPidNum + 1, 17)),
    {'EXIT', {badarg, mk_pid, _}}
    = (catch mk_pid(ThisNode, 4711, MaxPidSer + 1)),
    {'EXIT', {badarg, mk_port, _}}
    = (catch mk_port(ThisNode, ?MAX_PIDS_PORTS + 1)),
    {'EXIT', {badarg, mk_ref, _}}
    = (catch mk_ref(ThisNode,[(1 bsl 18), 4711, 4711])),
    {'EXIT', {badarg, mk_ref, _}}
    = (catch mk_ref(ThisNode, [4711, 4711, 4711, 4711, 4711, 4711, 4711])),
    RemNode = {x@y, 2},
    {'EXIT', {badarg, mk_pid, _}}
    = (catch mk_pid(RemNode, MaxPidNum + 1, MaxPidSer)),
    {'EXIT', {badarg, mk_pid, _}}
    = (catch mk_pid(RemNode, MaxPidNum, MaxPidSer + 1)),
    {'EXIT', {badarg, mk_port, _}}
    = (catch mk_port(RemNode, ?MAX_PIDS_PORTS + 1)),
    {'EXIT', {badarg, mk_ref, _}}
    = (catch mk_ref(RemNode, [(1 bsl 18), 4711, 4711])),
    {'EXIT', {badarg, mk_ref, _}}
    = (catch mk_ref(RemNode, [4711, 4711, 4711, 4711, 4711, 4711, 4711])),
    BadNode = {x@y, bad_creation},
    {'EXIT', {badarg, mk_pid, _}}
    = (catch mk_pid(BadNode, 4711, 17)),
    {'EXIT', {badarg, mk_port, _}}
    = (catch mk_port(BadNode, 4711)),
    {'EXIT', {badarg, mk_ref, _}}
    = (catch mk_ref(BadNode, [4711, 4711, 17])),
    ok.



-define(NO_PIDS, 1000000).

unique_pid(Config) when is_list(Config) ->
    case catch erlang:system_info(modified_timing_level) of
        Level when is_integer(Level) ->
            {skip,
             "Modified timing (level " ++ integer_to_list(Level)
             ++ ") is enabled. spawn() is too slow for this "
             " test when modified timing is enabled."};
        _ ->
            ?NO_PIDS = length(lists:usort(mkpidlist(?NO_PIDS, []))),
            ok
    end.

mkpidlist(0, Ps) -> Ps;
mkpidlist(N, Ps) -> mkpidlist(N-1, [spawn(fun () -> ok end)|Ps]).


iter_max_procs(Config) when is_list(Config) ->
    NoMoreTests = make_ref(),
    erlang:send_after(10000, self(), NoMoreTests),
    Res = chk_max_proc_line(),
    Res = chk_max_proc_line(),
    done = chk_max_proc_line_until(NoMoreTests, Res),
    Cmt = io_lib:format("max processes = ~p; "
                        "process line length = ~p",
                        [element(2, Res), element(1, Res)]),
    {comment, lists:flatten(Cmt)}.

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
    Child = max_proc_line(self(), self(), 0),
    receive
        {proc_line_length, PLL, End} ->
            PC = erlang:system_info(process_count),
            LP = length(processes()),
            io:format("proc line length = ~p; "
                      "process count = ~p; "
                      "length processes = ~p~n",
                      [PLL, PC, LP]),
                  End ! remove_proc_line,
                  PC = LP,
                  receive {exiting, Child} -> ok end,
                  {PLL, PC}
    end.

chk_max_proc_line_until(NoMoreTests, Res) ->
    receive
        NoMoreTests ->
            done
    after 0 ->
              Res = chk_max_proc_line(),
              chk_max_proc_line_until(NoMoreTests, Res)
    end.

magic_ref(Config) when is_list(Config) ->
    {MRef0, Addr0} = erts_debug:set_internal_state(make, magic_ref),
    true = is_reference(MRef0),
    {Addr0, 1, true} = erts_debug:get_internal_state({magic_ref,MRef0}),
    MRef1 = binary_to_term(term_to_binary(MRef0)),
    {Addr0, 2, true} = erts_debug:get_internal_state({magic_ref,MRef1}),
    MRef0 = MRef1,
    Me = self(),
    {Pid, Mon} = spawn_opt(fun () ->
				   receive
				       {Me, MRef} ->
					   Me ! {self(), erts_debug:get_internal_state({magic_ref,MRef})}
				   end
			   end,
			   [link, monitor]),
    Pid ! {self(), MRef0},
    receive
	{Pid, Info} ->
	    {Addr0, 3, true} = Info
    end,
    receive
	{'DOWN', Mon, process, Pid, _} ->
	    ok
    end,
    MaxTime = erlang:monotonic_time(millisecond) + 1000,
    %% The DOWN signal is sent before heap is cleaned up,
    %% so we might need to wait some time after the DOWN
    %% signal has been received before the heap actually
    %% has been cleaned up...
    wait_until(fun () ->
                       case erts_debug:get_internal_state({magic_ref,MRef0}) of
                           {Addr0, 2, true} ->
                               true;
                           {Addr0, 3, true} ->
                               true = MaxTime >= erlang:monotonic_time(millisecond),
                               false;
                           Error ->
                               ct:fail(Error)
                       end
               end),
    id(MRef0),
    id(MRef1),
    MRefExt = term_to_binary(erts_debug:set_internal_state(make, magic_ref)),
    garbage_collect(),
    {MRef2, _Addr2} = binary_to_term(MRefExt),
    true = is_reference(MRef2),
    true = erts_debug:get_internal_state({magic_ref,MRef2}),
    ok.

persistent_term(Config) when is_list(Config) ->
    {ok, Node} = start_node(get_nodefirstname()),
    Self = self(),
    NcData = make_ref(),
    RPid = spawn_link(Node,
                      fun () ->
                              Self ! {NcData, self(), hd(erlang:ports()), erlang:make_ref()}
                      end),
    Data = receive
               {NcData, RPid, RPort, RRef} ->
                   {RPid, RPort, RRef}
           end,
    unlink(RPid),
    stop_node(Node),
    Stuff = lists:foldl(fun (N, Acc) ->
                                persistent_term:put({?MODULE, N}, Data),
                                persistent_term:erase({?MODULE, N-1}),
                                node_container_refc_check(node()),
                                Data = persistent_term:get({?MODULE, N}),
                                try
                                    persistent_term:get({?MODULE, N-1})
                                catch
                                    error:badarg ->
                                        ok
                                end,
                                case N rem 4 of
                                    0 -> [persistent_term:get({?MODULE, N})|Acc];
                                    _ -> Acc
                                end
                        end,
                        [],
                        lists:seq(1, 100)),
    persistent_term:erase({?MODULE, 100}),
    receive after 2000 -> ok end, %% give literal gc some time to run...
    node_container_refc_check(node()),
    id(Stuff),
    ok.


lost_pending_connection(Node) ->
    _ = (catch erts_internal:new_connection(Node)),
    ok.

dist_entry_gc(Config) when is_list(Config) ->
    Me = self(),
    {ok, Node} = start_node(get_nodefirstname(), "+zdntgc 0"),
    P = spawn_link(Node,
                   fun () ->
                           LostNode = list_to_atom("lost_pending_connection@" ++ hostname()),
                           lost_pending_connection(LostNode),
                           garbage_collect(), %% Could crash...
                           Me ! {self(), ok}
                   end),
    receive
        {P, ok} -> ok
    end,
    unlink(P),
    stop_node(Node),
    ok.

%%
%% -- Internal utils ---------------------------------------------------------
%%

id(X) ->
    X.

-define(ND_REFS, erts_debug:get_internal_state(node_and_dist_references)).

node_container_refc_check(Node) when is_atom(Node) ->
    AIS = erts_test_utils:available_internal_state(true),
    nc_refc_check(Node),
    erts_test_utils:available_internal_state(AIS).

nc_refc_check(Node) when is_atom(Node) ->
    Ref = make_ref(),
    Self = self(),
    io:format("Starting reference count check of node ~w~n", [Node]),
    spawn_link(Node,
               fun () ->
                       erts_test_utils:check_node_dist(
                         fun (ErrMsg) ->
                                 Self ! {Ref, ErrMsg, failed},
                                 exit(normal)
                         end),
                       Self ! {Ref, succeded}
               end),
    receive
        {Ref, ErrorMsg, failed} ->
            io:format("~s~n", [ErrorMsg]),
            ct:fail(reference_count_check_failed);
        {Ref, succeded} ->
            io:format("Reference count check of node ~w succeded!~n", [Node]),
            ok
    end.

get_node_references({NodeName, Creation} = Node) when is_atom(NodeName),
                                                      is_integer(Creation) ->
    {{node_references, NodeRefs},
     {dist_references, DistRefs}} = ?ND_REFS,
    erts_test_utils:check_node_dist(
      fun (ErrMsg) ->
              io:format("~s", [ErrMsg]),
              ct:fail(reference_count_check_failed)
      end,
      NodeRefs, DistRefs),
    find_references(Node, NodeRefs).

get_dist_references(NodeName) when is_atom(NodeName) ->
    {{node_references, NodeRefs},
     {dist_references, DistRefs}} = ?ND_REFS,
    erts_test_utils:check_node_dist(fun (ErrMsg) ->
                                            io:format("~s", [ErrMsg]),
                                            ct:fail(reference_count_check_failed)
                                    end,
                                    NodeRefs, DistRefs),
    find_references(NodeName, DistRefs).

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
    Pa = filename:dirname(code:which(?MODULE)),
    Res = test_server:start_node(Name,
                                 slave,
                                 [{args, "-pa "++Pa++" "++Args}]),
    {ok, Node} = Res,
    rpc:call(Node, erts_debug, set_internal_state,
             [available_internal_state, true]),
    Res.

start_node(Name) ->
    start_node(Name, "").

stop_node(Node) ->
    nc_refc_check(Node),
    true = test_server:stop_node(Node).

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

get_nodefirstname_string() ->
    atom_to_list(?MODULE)
    ++ "-"
    ++ integer_to_list(erlang:system_time(second))
    ++ "-"
    ++ integer_to_list(erlang:unique_integer([positive])).

get_nodefirstname() ->
    list_to_atom(get_nodefirstname_string()).

get_nodename() ->
    list_to_atom(get_nodefirstname_string()
                 ++ "@"
                 ++ hostname()).

mk_pid({NodeName, Creation}, Number, Serial) ->
    erts_test_utils:mk_ext_pid({NodeName, Creation}, Number, Serial).

mk_port({NodeName, Creation}, Number) ->
    erts_test_utils:mk_ext_port({NodeName, Creation}, Number).

mk_ref({NodeName, Creation}, Numbers) ->
    erts_test_utils:mk_ext_ref({NodeName, Creation}, Numbers).


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
