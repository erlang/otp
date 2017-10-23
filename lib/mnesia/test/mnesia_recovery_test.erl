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

%%
-module(mnesia_recovery_test).
-author('hakan@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([coord_dies/1, after_full_disc_partition/1,
         disc_less/1, garb_decision/1,
         system_upgrade/1,
         delete_during_start/1,
         no_master_2/1, no_master_3/1, one_master_2/1, one_master_3/1,
         two_master_2/1, two_master_3/1, all_master_2/1,
         all_master_3/1,
         dirty_read_during_down/1, trans_read_during_down/1,
         mnesia_down_during_startup_disk_ram/1,
         mnesia_down_during_startup_init_ram/1,
         mnesia_down_during_startup_init_disc/1,
         mnesia_down_during_startup_init_disc_only/1,
         mnesia_down_during_startup_tm_ram/1,
         mnesia_down_during_startup_tm_disc/1,
         mnesia_down_during_startup_tm_disc_only/1,
         with_checkpoint_same/1, with_checkpoint_other/1,
         explicit_stop_during_snmp/1,
         sym_trans_before_commit_kill_coord_node/1,
         sym_trans_before_commit_kill_coord_pid/1,
         sym_trans_before_commit_kill_part_after_ask/1,
         sym_trans_before_commit_kill_part_before_ask/1,
         sym_trans_after_commit_kill_coord_node/1,
         sym_trans_after_commit_kill_coord_pid/1,
         sym_trans_after_commit_kill_part_after_ask/1,
         sym_trans_after_commit_kill_part_do_commit_pre/1,
         sym_trans_after_commit_kill_part_do_commit_post/1,
         sync_dirty_pre_kill_part/1,
         sync_dirty_pre_kill_coord_node/1,
         sync_dirty_pre_kill_coord_pid/1,
         sync_dirty_post_kill_part/1,
         sync_dirty_post_kill_coord_node/1,
         sync_dirty_post_kill_coord_pid/1,
         async_dirty_pre_kill_part/1,
         async_dirty_pre_kill_coord_node/1,
         async_dirty_pre_kill_coord_pid/1,
         async_dirty_post_kill_part/1,
         async_dirty_post_kill_coord_node/1,
         async_dirty_post_kill_coord_pid/1,
         asymtrans_part_ask/1,
         asymtrans_part_commit_vote/1,
         asymtrans_part_pre_commit/1,
         asymtrans_part_log_commit/1,
         asymtrans_part_do_commit/1,
         asymtrans_coord_got_votes/1,
         asymtrans_coord_pid_got_votes/1,
         asymtrans_coord_log_commit_rec/1,
         asymtrans_coord_pid_log_commit_rec/1,
         asymtrans_coord_log_commit_dec/1,
         asymtrans_coord_pid_log_commit_dec/1,
         asymtrans_coord_rec_acc_pre_commit_log_commit/1,
         asymtrans_coord_pid_rec_acc_pre_commit_log_commit/1,
         asymtrans_coord_rec_acc_pre_commit_done_commit/1,
         asymtrans_coord_pid_rec_acc_pre_commit_done_commit/1,
         after_corrupt_files_decision_log_head/1,
         after_corrupt_files_decision_log_tail/1,
         after_corrupt_files_latest_log_head/1,
         after_corrupt_files_latest_log_tail/1,
         after_corrupt_files_table_dat_head/1,
         after_corrupt_files_table_dat_tail/1,
         after_corrupt_files_schema_dat_head/1,
         after_corrupt_files_schema_dat_tail/1]).

-export([reader/2, check/0, get_all_retainers/1,
         verify_data/2, verify_where2read/1,
         do_trans_loop/2,
         start_stop/3, do_sym_trans/2, do_sync_dirty/2, do_async_dirty/2,
         do_asym_trans/2, garb_handler/1, mymnesia_start/1
        ]).


-include("mnesia_test_lib.hrl").
-include_lib("kernel/include/file.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

-define(receive_messages(Msgs), receive_messages(Msgs, ?FILE, ?LINE)).

% First Some debug logging 
-define(dgb, true).
-ifdef(dgb).
-define(dl(X, Y), ?verbose("**TRACING: " ++ X ++ "**~n", Y)).
-else. 
-define(dl(X, Y), ok).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [{group, mnesia_down}, {group, explicit_stop},
     coord_dies, {group, schema_trans}, {group, async_dirty},
     {group, sync_dirty}, {group, sym_trans},
     {group, asym_trans}, after_full_disc_partition,
     {group, after_corrupt_files}, disc_less, garb_decision,
     system_upgrade].

groups() -> 
    [{schema_trans, [],
      [{mnesia_schema_recovery_test, all}]},
     {mnesia_down, [],
      [{group, mnesia_down_during_startup},
       {group, master_node_tests}, {group, read_during_down},
       {group, with_checkpoint}, delete_during_start]},
     {master_node_tests, [],
      [no_master_2, no_master_3, one_master_2, one_master_3,
       two_master_2, two_master_3, all_master_2,
       all_master_3]},
     {read_during_down, [],
      [dirty_read_during_down, trans_read_during_down]},
     {mnesia_down_during_startup, [],
      [mnesia_down_during_startup_disk_ram,
       mnesia_down_during_startup_init_ram,
       mnesia_down_during_startup_init_disc,
       mnesia_down_during_startup_init_disc_only,
       mnesia_down_during_startup_tm_ram,
       mnesia_down_during_startup_tm_disc,
       mnesia_down_during_startup_tm_disc_only]},
     {with_checkpoint, [],
      [with_checkpoint_same, with_checkpoint_other]},
     {explicit_stop, [], [explicit_stop_during_snmp]},
     {sym_trans, [],
      [sym_trans_before_commit_kill_coord_node,
       sym_trans_before_commit_kill_coord_pid,
       sym_trans_before_commit_kill_part_after_ask,
       sym_trans_before_commit_kill_part_before_ask,
       sym_trans_after_commit_kill_coord_node,
       sym_trans_after_commit_kill_coord_pid,
       sym_trans_after_commit_kill_part_after_ask,
       sym_trans_after_commit_kill_part_do_commit_pre,
       sym_trans_after_commit_kill_part_do_commit_post]},
     {sync_dirty, [],
      [sync_dirty_pre_kill_part,
       sync_dirty_pre_kill_coord_node,
       sync_dirty_pre_kill_coord_pid,
       sync_dirty_post_kill_part,
       sync_dirty_post_kill_coord_node,
       sync_dirty_post_kill_coord_pid]},
     {async_dirty, [],
      [async_dirty_pre_kill_part,
       async_dirty_pre_kill_coord_node,
       async_dirty_pre_kill_coord_pid,
       async_dirty_post_kill_part,
       async_dirty_post_kill_coord_node,
       async_dirty_post_kill_coord_pid]},
     {asym_trans, [],
      [asymtrans_part_ask,
       asymtrans_part_commit_vote,
       asymtrans_part_pre_commit,
       asymtrans_part_log_commit,
       asymtrans_part_do_commit,
       asymtrans_coord_got_votes,
       asymtrans_coord_pid_got_votes,
       asymtrans_coord_log_commit_rec,
       asymtrans_coord_pid_log_commit_rec,
       asymtrans_coord_log_commit_dec,
       asymtrans_coord_pid_log_commit_dec,
       asymtrans_coord_rec_acc_pre_commit_log_commit,
       asymtrans_coord_pid_rec_acc_pre_commit_log_commit,
       asymtrans_coord_rec_acc_pre_commit_done_commit,
       asymtrans_coord_pid_rec_acc_pre_commit_done_commit]},
     {after_corrupt_files, [],
      [after_corrupt_files_decision_log_head,
       after_corrupt_files_decision_log_tail,
       after_corrupt_files_latest_log_head,
       after_corrupt_files_latest_log_tail,
       after_corrupt_files_table_dat_head,
       after_corrupt_files_table_dat_tail,
       after_corrupt_files_schema_dat_head,
       after_corrupt_files_schema_dat_tail]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

tpcb_config(ReplicaType, _NodeConfig, Nodes) ->
    [{n_branches, 5},
     {n_drivers_per_node, 5},
     {replica_nodes, Nodes},
     {driver_nodes, Nodes},
     {use_running_mnesia, true},
     {report_interval, infinity},
     {n_accounts_per_branch, 20},
     {replica_type, ReplicaType}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



no_master_2(suite) -> [];
no_master_2(Config) when is_list(Config) ->    mnesia_down_2(no, Config).
    
no_master_3(suite) -> [];
no_master_3(Config) when is_list(Config) ->    mnesia_down_3(no, Config).

one_master_2(suite) -> [];
one_master_2(Config) when is_list(Config) ->   mnesia_down_2(one, Config).

one_master_3(suite) -> [];
one_master_3(Config) when is_list(Config) ->   mnesia_down_3(one, Config).

two_master_2(suite) -> [];
two_master_2(Config) when is_list(Config) ->   mnesia_down_2(two, Config).

two_master_3(suite) -> [];
two_master_3(Config) when is_list(Config) ->   mnesia_down_3(two, Config).

all_master_2(suite) -> [];
all_master_2(Config) when is_list(Config) ->   mnesia_down_2(all, Config).

all_master_3(suite) -> [];
all_master_3(Config) when is_list(Config) ->   mnesia_down_3(all, Config).

mnesia_down_2(Masters, Config) -> 
    Nodes = [N1, N2] = ?acquire_nodes(2, Config),
    ?match({atomic, ok}, mnesia:create_table(tab1, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab2, [{disc_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab3, [{disc_only_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab4, [{ram_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab5, [{ram_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab6, [{disc_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab7, [{disc_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab8, [{disc_only_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab9, [{disc_only_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab10, [{ram_copies, [N1]}, {disc_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab11, [{ram_copies, [N2]}, {disc_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab12, [{ram_copies, [N1]}, {disc_only_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab13, [{ram_copies, [N2]}, {disc_only_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab14, [{disc_only_copies, [N1]}, {disc_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab15, [{disc_only_copies, [N2]}, {disc_copies, [N1]}])),

    Tabs = [tab1, tab2, tab3, tab4, tab5, tab6, tab7, tab8, 
	    tab9, tab10, tab11, tab12, tab13, tab14, tab15],
    [?match(ok, rpc:call(Node, mnesia, wait_for_tables, [Tabs, 10000])) || Node <- Nodes],
    [insert_data(Tab, 20) || Tab <- Tabs],

    VTabs =
	case Masters of
	    no ->
		Tabs -- [tab4, tab5]; % ram copies
	    one ->
		?match(ok, rpc:call(N1, mnesia, set_master_nodes, [[N1]])),
		Tabs -- [tab1, tab4, tab5, tab10, tab12]; % ram_copies
	    two ->
		?match(ok, rpc:call(N1, mnesia, set_master_nodes, [Nodes])),
		Tabs -- [tab4, tab5];
	    all -> 
		[?match(ok, rpc:call(Node, mnesia, set_master_nodes, [[Node]])) || Node <- Nodes],
		Tabs -- [tab1, tab4, tab5, tab10, tab11, tab12, tab13]
	end,

    mnesia_test_lib:kill_mnesia([N1]),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    [?match(ok, rpc:call(N1, ?MODULE, verify_data, [Tab, 20])) || Tab <- VTabs],
    [?match(ok, rpc:call(N2, ?MODULE, verify_data, [Tab, 20])) || Tab <- VTabs],
    ?verify_mnesia(Nodes, []).

mnesia_down_3(Masters, Config) -> 
    Nodes = [N1, N2, N3] = ?acquire_nodes(3, Config),
    ?match({atomic, ok}, mnesia:create_table(tab1, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab2, [{disc_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab3, [{disc_only_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab4, [{ram_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab5, [{ram_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab16, [{ram_copies, [N3]}])),
    ?match({atomic, ok}, mnesia:create_table(tab6, [{disc_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab7, [{disc_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab17, [{disc_copies, [N3]}])),
    ?match({atomic, ok}, mnesia:create_table(tab8, [{disc_only_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab9, [{disc_only_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab18, [{disc_only_copies, [N3]}])),
    ?match({atomic, ok}, mnesia:create_table(tab10, [{ram_copies, [N1]}, {disc_copies, [N2, N3]}])),
    ?match({atomic, ok}, mnesia:create_table(tab11, [{ram_copies, [N2]}, {disc_copies, [N3, N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab19, [{ram_copies, [N3]}, {disc_copies, [N1, N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab12, [{ram_copies, [N1]}, {disc_only_copies, [N2, N3]}])),
    ?match({atomic, ok}, mnesia:create_table(tab13, [{ram_copies, [N2]}, {disc_only_copies, [N3, N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab20, [{ram_copies, [N3]}, {disc_only_copies, [N1, N2]}])),
    ?match({atomic, ok}, mnesia:create_table(tab14, [{disc_only_copies, [N1]}, {disc_copies, [N2, N3]}])),
    ?match({atomic, ok}, mnesia:create_table(tab15, [{disc_only_copies, [N2]}, {disc_copies, [N3, N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab21, [{disc_only_copies, [N3]}, {disc_copies, [N1, N2]}])),

    Tabs = [tab1, tab2, tab3, tab4, tab5, tab6, tab7, tab8, 
	    tab9, tab10, tab11, tab12, tab13, tab14, tab15,
	    tab16, tab17, tab18, tab19, tab20, tab21],
    [?match(ok, rpc:call(Node, mnesia, wait_for_tables, [Tabs, 10000])) || Node <- Nodes],
    [insert_data(Tab, 20) || Tab <- Tabs],

    VTabs = 
	case Masters of
	    no ->
		Tabs -- [tab4, tab5, tab16]; % ram copies
	    one ->
		?match(ok, rpc:call(N1, mnesia, set_master_nodes, [[N1]])),
		Tabs -- [tab1, tab4, tab5, tab16, tab10, tab12]; % ram copies
	    two ->
		?match(ok, rpc:call(N1, mnesia, set_master_nodes, [Nodes])),
		Tabs -- [tab4, tab5, tab16]; % ram copies
	    all -> 
		[?match(ok, rpc:call(Node, mnesia, set_master_nodes, [[Node]])) || Node <- Nodes],
		Tabs -- [tab1, tab4, tab5, tab16, tab10, 
			 tab11, tab19, tab12, tab13, tab20] % ram copies
	end,
    
    mnesia_test_lib:kill_mnesia([N1]),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),
    
    ?match([], mnesia_test_lib:kill_mnesia([N2])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    ?match([], mnesia_test_lib:kill_mnesia([N3])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    ?match([], mnesia_test_lib:kill_mnesia([N2, N1])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    ?match([], mnesia_test_lib:kill_mnesia([N2, N3])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    ?match([], mnesia_test_lib:kill_mnesia([N1, N3])),
    ?match([], mnesia_test_lib:start_mnesia(Nodes, Tabs)),

    [?match(ok, rpc:call(N1, ?MODULE, verify_data, [Tab, 20])) || Tab <- VTabs],
    [?match(ok, rpc:call(N2, ?MODULE, verify_data, [Tab, 20])) || Tab <- VTabs],
    [?match(ok, rpc:call(N3, ?MODULE, verify_data, [Tab, 20])) || Tab <- VTabs],
    
    ?verify_mnesia(Nodes, []).



dirty_read_during_down(suite) ->
    [];
dirty_read_during_down(Config) when is_list(Config) ->
    read_during_down(dirty, Config).

trans_read_during_down(suite) ->
    [];
trans_read_during_down(Config) when is_list(Config) ->
    read_during_down(trans, Config).


read_during_down(Op, Config) when is_list(Config) ->
    Ns = [N1|TNs] = ?acquire_nodes(3, Config),
    Tabs = [ram, disc, disco],
    
    ?match({atomic, ok}, mnesia:create_table(ram, [{ram_copies, TNs}])),
    ?match({atomic, ok}, mnesia:create_table(disc, [{disc_copies, TNs}])),
    ?match({atomic, ok}, mnesia:create_table(disco, [{disc_only_copies, TNs}])),

    %% Create some work for mnesia_controller when a node goes down
    [{atomic, ok} = mnesia:create_table(list_to_atom("temp" ++ integer_to_list(N)),
					[{ram_copies, Ns}]) || N <- lists:seq(1, 50)],    
    
    Write = fun(Tab) -> mnesia:write({Tab, key, val}) end,
    ?match([ok,ok,ok],
	   [mnesia:sync_dirty(Write, [Tab]) || Tab <- Tabs]),
    
    Readers = [spawn_link(N1, ?MODULE, reader, [Tab, Op]) || Tab <- Tabs],
    [_|_] = W2R= [mnesia:table_info(Tab, where_to_read) || Tab <- Tabs],
    ?log("W2R ~p~n", [W2R]),
    loop_and_kill_mnesia(10, hd(W2R), Tabs),
    [Pid ! self() || Pid <- Readers],
    ?match([ok, ok, ok],
	   [receive ok -> ok after 5000 -> {Pid, mnesia_lib:dist_coredump()} end
	    || Pid <- Readers]),
    ?verify_mnesia(Ns, []).

reader(Tab, OP) ->
    Res = case OP of 
	      dirty ->
		  catch mnesia:dirty_read({Tab, key});
	      trans ->
		  Read = fun() -> mnesia:read({Tab, key}) end,
		  {_, Temp} = mnesia:transaction(Read),
		  Temp
	  end,
    case Res of 
	[{Tab, key, val}] -> ok;
	Else -> 
	    ?error("Expected ~p Got ~p ~n", [[{Tab, key, val}], Else]),
	    erlang:error(test_failed)
    end,
    receive
	Pid when is_pid(Pid) ->
	    Pid ! ok;
	Other ->
	    io:format("Msg: ~p~n", [Other]),
	    error(Other)
    after 50 ->
	    reader(Tab, OP)
    end.

loop_and_kill_mnesia(0, _Node, _Tabs) -> ok;
loop_and_kill_mnesia(N, Node, Tabs) ->
    mnesia_test_lib:kill_mnesia([Node]),
    timer:sleep(100), 
    ?match([], mnesia_test_lib:start_mnesia([Node], Tabs)),
    [KN | _] = W2R= [mnesia:table_info(Tab, where_to_read) || Tab <- Tabs],
    ?match([KN, KN,KN], W2R),
    timer:sleep(100), 
    loop_and_kill_mnesia(N-1, KN, Tabs).


mnesia_down_during_startup_disk_ram(suite) -> [];
mnesia_down_during_startup_disk_ram(Config) when is_list(Config)->
    [Node1, Node2] = ?acquire_nodes(2, Config ++ 
				    [{tc_timeout, timer:minutes(2)}]),
    Tab = down_during_startup,
    Def = [{ram_copies, [Node2]}, {disc_copies, [Node1]}],
    
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match(ok, mnesia:dirty_write({Tab, 876234, test_ok})),
    timer:sleep(500),
    mnesia_test_lib:kill_mnesia([Node1, Node2]),
    timer:sleep(500),
    mnesia_test_lib:start_mnesia([Node1, Node2], [Tab]),
    mnesia_test_lib:kill_mnesia([Node1]),
    timer:sleep(500),
    ?match([], mnesia_test_lib:start_mnesia([Node1], [Tab])),
    ?match([{Tab, 876234, test_ok}], mnesia:dirty_read({Tab,876234})),
    ?verify_mnesia([Node1, Node2], []).
    
mnesia_down_during_startup_init_ram(suite) -> [];
mnesia_down_during_startup_init_ram(Config) when is_list(Config) ->
    ?is_debug_compiled,
    DP = {mnesia_loader, do_get_network_copy},
    Type = ram_copies,
    mnesia_down_during_startup2(Config, Type, DP, self()).

mnesia_down_during_startup_init_disc(suite)  -> [];
mnesia_down_during_startup_init_disc(Config) when is_list(Config) ->
    ?is_debug_compiled,
    DP = {mnesia_loader, do_get_network_copy},
    Type = disc_copies,
    mnesia_down_during_startup2(Config, Type, DP, self()).

mnesia_down_during_startup_init_disc_only(suite) -> [];
mnesia_down_during_startup_init_disc_only(Config) when is_list(Config) ->
    ?is_debug_compiled,
    DP = {mnesia_loader, do_get_network_copy},
    Type = disc_only_copies,
    mnesia_down_during_startup2(Config, Type, DP, self()).

mnesia_down_during_startup_tm_ram(suite) -> [];
mnesia_down_during_startup_tm_ram(Config) when is_list(Config) ->
    ?is_debug_compiled,
    DP = {mnesia_tm, init},
    Type = ram_copies,
    mnesia_down_during_startup2(Config, Type, DP, self()).

mnesia_down_during_startup_tm_disc(suite) -> [];
mnesia_down_during_startup_tm_disc(Config) when is_list(Config) ->
    ?is_debug_compiled,
    DP = {mnesia_tm, init},
    Type = disc_copies,
    mnesia_down_during_startup2(Config, Type, DP, self()).

mnesia_down_during_startup_tm_disc_only(suite) -> [];
mnesia_down_during_startup_tm_disc_only(Config) when is_list(Config) ->
    ?is_debug_compiled,
    DP = {mnesia_tm, init},
    Type = disc_only_copies,
    mnesia_down_during_startup2(Config, Type, DP, self()).

mnesia_down_during_startup2(Config, ReplicaType, Debug_Point, _Father) -> 
    ?log("TC~n mnesia_down_during_startup with type ~w and stops at ~w~n",
	 [ReplicaType, Debug_Point]),
    Tpcb_tabs = [history,teller,account,branch],
    Nodes = ?acquire_nodes(2, Config),
    Node1 = hd(Nodes),
    {success, [A]} = ?start_activities([Node1]),
    TpcbConfig = tpcb_config(ReplicaType, 2, Nodes),
    mnesia_tpcb:init(TpcbConfig),
    A ! fun () -> mnesia_tpcb:run(TpcbConfig) end,
    ?match_receive(timeout),
    timer:sleep(timer:seconds(10)),  % Let tpcb run for a while
    mnesia_tpcb:stop(),
    ?match(ok, mnesia_tpcb:verify_tabs()), 
    mnesia_test_lib:kill_mnesia([Node1]),
    timer:sleep(timer:seconds(2)),
    Self = self(),
    TestFun = fun(_MnesiaEnv, _EvalEnv) ->
		      ?deactivate_debug_fun(Debug_Point),
		      Self ! fun_done, 
		      spawn(mnesia_test_lib, kill_mnesia, [[Node1]])
	      end,    
    ?activate_debug_fun(Debug_Point, TestFun, []),      % Kill when debug has been reached
    mnesia:start(),
    Res = receive fun_done -> ok after timer:minutes(3) -> timeout end, % Wait till it's killed
    ?match(ok, Res),
    ?match(ok, timer:sleep(timer:seconds(2))), % Wait a while, at least till it dies;
    ?match([], mnesia_test_lib:start_mnesia([Node1], Tpcb_tabs)),  
    ?match(ok, mnesia_tpcb:verify_tabs()), % Verify it
    ?verify_mnesia(Nodes, []).    	    



with_checkpoint_same(suite) -> [];
with_checkpoint_same(Config) when is_list(Config) -> 
    with_checkpoint(Config, same).

with_checkpoint_other(suite) -> [];
with_checkpoint_other(Config) when is_list(Config) -> 
    with_checkpoint(Config, other).

with_checkpoint(Config, Type) when is_list(Config) ->
    Nodes = [Node1, Node2] = ?acquire_nodes(2, Config),
    Kill = case Type of
	       same ->    %% Node1 is the one used for creating the checkpoint
		   Node1; %% and which we bring down
	       other ->
		   Node2  %% Here we bring node2 down..
	   end,
    
    ?match({atomic, ok}, mnesia:create_table(ram, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(disc, [{disc_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(disco, [{disc_only_copies, Nodes}])),
    Tabs = [ram, disc, disco],
    
    ?match({ok, sune, _}, mnesia:activate_checkpoint([{name, sune}, 
						      {max, mnesia:system_info(tables)}, 
						      {ram_overrides_dump, true}])),

    ?match([], check_retainers(sune, Nodes)),
    
    ?match(ok, mnesia:deactivate_checkpoint(sune)),
    ?match([], check_chkp(Nodes)),
    
    timer:sleep(500),  %% Just to help debugging the io:formats now comes in the
    %% correct order... :-)    

    ?match({ok, sune, _}, mnesia:activate_checkpoint([{name, sune}, 
	{max, mnesia:system_info(tables)}, 
	{ram_overrides_dump, true}])),

    [[mnesia:dirty_write({Tab,Key,Key}) || Key <- lists:seq(1,10)] || Tab <- Tabs],

    mnesia_test_lib:kill_mnesia([Kill]),
    timer:sleep(100),     
    mnesia_test_lib:start_mnesia([Kill], Tabs),
    io:format("Mnesia on ~p started~n", [Kill]),
    ?match([], check_retainers(sune, Nodes)),
    ?match(ok, mnesia:deactivate_checkpoint(sune)),
    ?match([], check_chkp(Nodes)),

    Wait = fun(Loop) ->
		   timer:sleep(300),
		   sys:get_status(mnesia_monitor),
		   case lists:member(Kill, mnesia_lib:val({current, db_nodes})) of
		       true -> Loop(Loop);
		       false -> ok
		   end
	   end,

    case Kill of
	Node1 -> 
	    ignore;
	Node2 ->
	    mnesia_test_lib:kill_mnesia([Kill]),
	    Wait(Wait),
	    ?match({ok, sune, _}, mnesia:activate_checkpoint([{name, sune}, 
		{max, mnesia:system_info(tables)}, 
		{ram_overrides_dump, true}])),

	    [[mnesia:dirty_write({Tab,Key,Key+2}) || Key <- lists:seq(1,10)] ||
		Tab <- Tabs],

	    mnesia_test_lib:start_mnesia([Kill], Tabs),
	    io:format("Mnesia on ~p started ~n", [Kill]),
	    ?match([], check_retainers(sune, Nodes)),
	    ?match(ok, mnesia:deactivate_checkpoint(sune)),
	    ?match([], check_chkp(Nodes)),	    
	    ok
    end,
    ?verify_mnesia(Nodes, []).

check_chkp(Nodes) ->
    {Good, Bad} = rpc:multicall(Nodes, ?MODULE, check, []),
    lists:flatten(Good ++ Bad).

check() ->
    [PCP] = ets:match_object(mnesia_gvar, {pending_checkpoint_pids, '_'}),
    [PC]  = ets:match_object(mnesia_gvar, {pending_checkpoints, '_'}),
    [CPN] = ets:match_object(mnesia_gvar, {checkpoints, '_'}),    
    F = lists:filter(fun({_, []}) -> false; (_W) -> true end, 
		     [PCP,PC,CPN]),    
    CPP = ets:match_object(mnesia_gvar, {{checkpoint, '_'}, '_'}),
    Rt  = ets:match_object(mnesia_gvar, {{'_', {retainer, '_'}}, '_'}),
    F ++ CPP ++ Rt.


check_retainers(CHP, Nodes) ->
    {[R1,R2], []} = rpc:multicall(Nodes, ?MODULE, get_all_retainers, [CHP]),
    (R1 -- R2) ++ (R2 -- R1).
        
get_all_retainers(CHP) ->
    Tabs = mnesia:system_info(local_tables),
    Iter = fun(Tab) ->
		   {ok, Res} = 
		       mnesia_checkpoint:iterate(CHP, Tab, fun(R, A) -> [R|A] end, [],
						 retainer, checkpoint),
%%		   io:format("Retainer content ~w ~n", [Res]),
		   Res
	   end,
    Elements = [Iter(Tab) || Tab <- Tabs],
    lists:sort(lists:flatten(Elements)).
    
delete_during_start(doc) ->
    ["Test that tables can be delete during start, hopefully with tables"
     " in the loader queue or soon to be"];
delete_during_start(suite) -> [];
delete_during_start(Config) when is_list(Config) ->
    [N1, N2, N3] = Nodes = ?acquire_nodes(3, Config),
    Tabs = [list_to_atom("tab" ++ integer_to_list(I)) || I <- lists:seq(1, 30)],
    ?match({atomic, ok}, mnesia:change_table_copy_type(schema, N2, ram_copies)),
    ?match({atomic, ok}, mnesia:change_table_copy_type(schema, N3, ram_copies)),

    [?match({atomic, ok},mnesia:create_table(Tab, [{ram_copies,Nodes}])) || Tab <- Tabs],
    lists:foldl(fun(Tab, I) ->
			?match({atomic, ok},
			       mnesia:change_table_load_order(Tab,I)),
			I+1
		end, 1, Tabs),
    mnesia_test_lib:kill_mnesia([N2,N3]),
%%    timer:sleep(500),
    ?match({[ok,ok],[]}, rpc:multicall([N2,N3], mnesia,start, 
				       [[{extra_db_nodes,[N1]}, {schema, ?BACKEND}]])),
    [Tab1,Tab2,Tab3|_] = Tabs,
    ?match({atomic, ok}, mnesia:delete_table(Tab1)),
    ?match({atomic, ok}, mnesia:delete_table(Tab2)),
    
    ?log("W4T ~p~n", [rpc:multicall([N2,N3], mnesia, wait_for_tables, [[Tab1,Tab2,Tab3],1])]),
    
    Remain = Tabs--[Tab1,Tab2],
    ?match(ok, rpc:call(N2, mnesia, wait_for_tables, [Remain,10000])),
    ?match(ok, rpc:call(N3, mnesia, wait_for_tables, [Remain,10000])),

    ?match(ok, rpc:call(N2, ?MODULE, verify_where2read, [Remain])),
    ?match(ok, rpc:call(N3, ?MODULE, verify_where2read, [Remain])),

    ?verify_mnesia(Nodes, []).
    
verify_where2read([Tab|Tabs]) ->
    true = (node() == mnesia:table_info(Tab,where_to_read)),
    verify_where2read(Tabs);
verify_where2read([]) -> ok.


%%-------------------------------------------------------------------------------------------
%% This is a bad implementation, but at least gives a indication if something is wrong
explicit_stop_during_snmp(suite) -> [];
explicit_stop_during_snmp(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(2, Config),
    [Node1, Node2] = Nodes,
    Tab = snmp_tab,
    Def = [{attributes, [key, value]},
	   {snmp, [{key, integer}]},
	   {mnesia_test_lib:storage_type(disc_copies, Config), 
	    [Node1, Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write({Tab, 1, 1}) end)),

    Do_trans_Pid1 = spawn_link(Node2, ?MODULE, do_trans_loop, [Tab, self()]),
    Do_trans_Pid2 = spawn_link(?MODULE, do_trans_loop, [Tab, self()]),
    Start_stop_Pid = spawn_link(?MODULE, start_stop, [Node1, 5, self()]),
    receive 
	test_done -> 
	    ok
    after timer:minutes(5) ->
	    ?error("test case time out~n", [])
    end,
    ?verify_mnesia(Nodes, []),
    exit(Do_trans_Pid1, kill),
    exit(Do_trans_Pid2, kill),
    exit(Start_stop_Pid, kill),
    ok.

do_trans_loop(Tab, Father) ->
    %% Do not trap exit
    do_trans_loop2(Tab, Father).
do_trans_loop2(Tab, Father) ->
    Trans =
	fun() ->
		[{Tab, 1, Val}] = mnesia:read({Tab, 1}),
		mnesia:write({Tab, 1, Val + 1})
	end,
    case mnesia:transaction(Trans) of
	{atomic, ok} ->
	    timer:sleep(100),
	    do_trans_loop2(Tab, Father);
	{aborted, {node_not_running, N}} when N == node() ->
	    timer:sleep(100),
	    do_trans_loop2(Tab, Father);
	{aborted, {no_exists, Tab}} ->
	    timer:sleep(100),
	    do_trans_loop2(Tab, Father);
	Else ->
	    ?error("Transaction failed: ~p ~n", [Else]),
            io:format("INFO: ~p~n",[erlang:process_info(self())]),
	    Father ! test_done,
	    exit(shutdown)
    end.

start_stop(_Node1, 0, Father) ->
    Father ! test_done,
    exit(shutdown);
start_stop(Node1, N, Father) when N > 0->
    timer:sleep(timer:seconds(2)),
    ?match(stopped, rpc:call(Node1, mnesia, stop, [])),
    timer:sleep(timer:seconds(1)),
    ?match([], mnesia_test_lib:start_mnesia([Node1])),
    start_stop(Node1, N-1, Father).    

coord_dies(suite) -> [];
coord_dies(doc) -> [""];
coord_dies(Config) when is_list(Config) ->
    Nodes = [N1, N2] = ?acquire_nodes(2, Config),
    ?match({atomic, ok}, mnesia:create_table(tab1, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(tab2, [{ram_copies, [N1]}])),
    ?match({atomic, ok}, mnesia:create_table(tab3, [{ram_copies, [N2]}])),
    Tester = self(),

    U1 = fun(Tab) -> 
		 [{Tab,key,Val}] = mnesia:read(Tab,key,write),
		 mnesia:write({Tab,key, Val+1}),
		 Tester ! {self(),continue},
		 receive 
		     continue -> exit(crash)
		 end
	 end,
    U2 = fun(Tab) -> 
		 [{Tab,key,Val}] = mnesia:read(Tab,key,write),
		 mnesia:write({Tab,key, Val+1}),
		 mnesia:transaction(U1, [Tab])
	 end,
    [mnesia:dirty_write(Tab,{Tab,key,0}) || Tab <- [tab1,tab2,tab3]],
    Pid1 = spawn(fun() -> mnesia:transaction(U2, [tab1]) end), 
    Pid2 = spawn(fun() -> mnesia:transaction(U2, [tab2]) end), 
    Pid3 = spawn(fun() -> mnesia:transaction(U2, [tab3]) end), 
    [receive {Pid,continue} -> ok end || Pid <- [Pid1,Pid2,Pid3]],
    Pid1 ! continue,    Pid2 ! continue,    Pid3 ! continue,
    ?match({atomic,[{_,key,1}]}, mnesia:transaction(fun() -> mnesia:read({tab1,key}) end)),
    ?match({atomic,[{_,key,1}]}, mnesia:transaction(fun() -> mnesia:read({tab2,key}) end)),
    ?match({atomic,[{_,key,1}]}, mnesia:transaction(fun() -> mnesia:read({tab3,key}) end)),

    Pid4 = spawn(fun() -> mnesia:transaction(U2, [tab1]) end), 
    Pid5 = spawn(fun() -> mnesia:transaction(U2, [tab2]) end), 
    Pid6 = spawn(fun() -> mnesia:transaction(U2, [tab3]) end), 
    erlang:monitor(process, Pid4),erlang:monitor(process, Pid5),erlang:monitor(process, Pid6),
    
    [receive {Pid,continue} -> ok end || Pid <- [Pid4,Pid5,Pid6]],
    exit(Pid4,crash),    
    ?match_receive({'DOWN',_,_,Pid4, _}),
    ?match({atomic,[{_,key,1}]}, mnesia:transaction(fun() -> mnesia:read({tab1,key}) end)),
    exit(Pid5,crash),
    ?match_receive({'DOWN',_,_,Pid5, _}),
    ?match({atomic,[{_,key,1}]}, mnesia:transaction(fun() -> mnesia:read({tab2,key}) end)),
    exit(Pid6,crash),
    ?match_receive({'DOWN',_,_,Pid6, _}),
    ?match({atomic,[{_,key,1}]}, mnesia:transaction(fun() -> mnesia:read({tab3,key}) end)),
    
    ?verify_mnesia(Nodes, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 

%kill_after_debug_point(Config, TestCase, {Debug_node, Debug_Point}, TransFun, Tab)

sym_trans_before_commit_kill_coord_node(suite) -> [];
sym_trans_before_commit_kill_coord_node(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_before_commit_kill_coord,
    Def = [{attributes, [key, value]}, {ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, multi_commit_sym}},
			   do_sym_trans, [{Tab, Def}], Nodes).

sym_trans_before_commit_kill_coord_pid(suite) -> [];
sym_trans_before_commit_kill_coord_pid(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_before_commit_kill_coord,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, multi_commit_sym}},
			   do_sym_trans, [{Tab, Def}], Nodes).

sym_trans_before_commit_kill_part_after_ask(suite) -> [];
sym_trans_before_commit_kill_part_after_ask(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_before_commit_kill_part_after_ask,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(Part1, {Coord, {mnesia_tm, multi_commit_sym}},
			   do_sym_trans, [{Tab, Def}], Nodes).

sym_trans_before_commit_kill_part_before_ask(suite) -> [];
sym_trans_before_commit_kill_part_before_ask(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_before_commit_kill_part_before_ask,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, doit_ask_commit}},
			   do_sym_trans, [{Tab, Def}], Nodes).

sym_trans_after_commit_kill_coord_node(suite) -> [];
sym_trans_after_commit_kill_coord_node(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_after_commit_kill_coord,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, multi_commit_sym, post}},
			  do_sym_trans, [{Tab, Def}], Nodes).

sym_trans_after_commit_kill_coord_pid(suite) -> [];
sym_trans_after_commit_kill_coord_pid(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_after_commit_kill_coord,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, multi_commit_sym, post}},
			  do_sym_trans, [{Tab,Def}], Nodes).

sym_trans_after_commit_kill_part_after_ask(suite) -> [];
sym_trans_after_commit_kill_part_after_ask(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab =  sym_trans_after_commit_kill_part_after_ask,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    kill_after_debug_point(Part1, {Coord, {mnesia_tm, multi_commit_sym, post}},
			   do_sym_trans, [{Tab, Def}], Nodes).

sym_trans_after_commit_kill_part_do_commit_pre(suite) -> [];
sym_trans_after_commit_kill_part_do_commit_pre(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_after_commit_kill_part_do_commit_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, do_commit, pre}},
			   TransFun, [{Tab, Def}], Nodes).

sym_trans_after_commit_kill_part_do_commit_post(suite) -> [];
sym_trans_after_commit_kill_part_do_commit_post(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sym_trans_after_commit_kill_part_do_commit_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, do_commit, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

do_sym_trans([Tab], _Fahter) ->
    ?dl("Starting SYM_TRANS with active debug fun ", []),
    Trans = fun() -> 
		    [{_,_,Val}] = mnesia:read({Tab, 1}),
		    mnesia:write({Tab, 1, Val+1})
	    end,
    Res = mnesia:transaction(Trans),
    case Res of
	{atomic, ok} -> ok;
	{aborted, _Reason} -> ok;
	Else -> ?error("Wrong output from mensia:transaction(FUN):~n ~p~n",
		       [Else])
    end,
    ?dl("SYM_TRANSACTION done: ~p  (deactiv dbgfun) ", [Res]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sync_dirty_pre_kill_part(suite) -> [];
sync_dirty_pre_kill_part(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sync_dirty_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sync_dirty,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, sync_dirty, pre}}, 
			   TransFun, [{Tab, Def}], Nodes).

sync_dirty_pre_kill_coord_node(suite) -> [];
sync_dirty_pre_kill_coord_node(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sync_dirty_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sync_dirty,
    kill_after_debug_point(Coord, {Part1, {mnesia_tm, sync_dirty, pre}}, 
			   TransFun, [{Tab, Def}], Nodes).

sync_dirty_pre_kill_coord_pid(suite) -> [];
sync_dirty_pre_kill_coord_pid(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sync_dirty_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sync_dirty,
    kill_after_debug_point(coord_pid, {Part1, {mnesia_tm, sync_dirty, pre}}, 
			   TransFun, [{Tab, Def}], Nodes).

sync_dirty_post_kill_part(suite) -> [];
sync_dirty_post_kill_part(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sync_dirty_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sync_dirty,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, sync_dirty, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

sync_dirty_post_kill_coord_node(suite) -> [];
sync_dirty_post_kill_coord_node(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sync_dirty_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sync_dirty,
    kill_after_debug_point(Coord, {Part1, {mnesia_tm, sync_dirty, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

sync_dirty_post_kill_coord_pid(suite) -> [];
sync_dirty_post_kill_coord_pid(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = sync_dirty_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_sync_dirty,
    kill_after_debug_point(coord_pid, {Part1, {mnesia_tm, sync_dirty, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

do_sync_dirty([Tab], _Father) ->
    ?dl("Starting SYNC_DIRTY", []),
    SYNC = fun() -> 
		   [{_,_,Val}] = mnesia:read({Tab, 1}),
		   mnesia:write({Tab, 1, Val+1})
	   end,
    {_, Res} = ?match(ok, mnesia:sync_dirty(SYNC)),
    ?dl("SYNC_DIRTY done: ~p ", [Res]),
    ok.


async_dirty_pre_kill_part(suite) -> [];
async_dirty_pre_kill_part(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = async_dirty_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_async_dirty,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, async_dirty, pre}}, 
			   TransFun, [{Tab, Def}], Nodes).

async_dirty_pre_kill_coord_node(suite) -> [];
async_dirty_pre_kill_coord_node(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = async_dirty_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_async_dirty,
    kill_after_debug_point(Coord, {Part1, {mnesia_tm, async_dirty, pre}}, 
			   TransFun, [{Tab, Def}], Nodes).

async_dirty_pre_kill_coord_pid(suite) -> [];
async_dirty_pre_kill_coord_pid(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = async_dirty_pre,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_async_dirty,
    kill_after_debug_point(coord_pid, {Part1, {mnesia_tm, async_dirty, pre}}, 
			   TransFun, [{Tab, Def}], Nodes).

async_dirty_post_kill_part(suite) -> [];
async_dirty_post_kill_part(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = async_dirty_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_async_dirty,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, async_dirty, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

async_dirty_post_kill_coord_node(suite) -> [];
async_dirty_post_kill_coord_node(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = async_dirty_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_async_dirty,
    kill_after_debug_point(Coord, {Part1, {mnesia_tm, async_dirty, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

async_dirty_post_kill_coord_pid(suite) -> [];
async_dirty_post_kill_coord_pid(Config) when is_list(Config) ->
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab = async_dirty_post,
    Def = [{attributes, [key, value]},{ram_copies, [Part2]},{disc_copies, [Coord, Part1]}],
    TransFun = do_async_dirty,
    kill_after_debug_point(coord_pid, {Part1, {mnesia_tm, async_dirty, post}}, 
			   TransFun, [{Tab, Def}], Nodes).

do_async_dirty([Tab], _Fahter) ->
    ?dl("Starting ASYNC", []),
    ASYNC = fun() -> 
		    [{_,_,Val}] = mnesia:read({Tab, 1}),
		    mnesia:write({Tab, 1, Val+1})
	    end,
    {_, Res} = ?match(ok, mnesia:async_dirty(ASYNC)),
    ?dl("ASYNC done: ~p ", [Res]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


asymtrans_part_ask(suite) -> [];
asymtrans_part_ask(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, doit_ask_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_part_commit_vote(suite) -> [];
asymtrans_part_commit_vote(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, commit_participant, vote_yes}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_part_pre_commit(suite) -> [];
asymtrans_part_pre_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, commit_participant, pre_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_part_log_commit(suite) -> [];
asymtrans_part_log_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, commit_participant, log_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_part_do_commit(suite) -> [];
asymtrans_part_do_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Part1, {Part1, {mnesia_tm, commit_participant, do_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_got_votes(suite) -> [];
asymtrans_coord_got_votes(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, multi_commit_asym_got_votes}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_pid_got_votes(suite) -> [];
asymtrans_coord_pid_got_votes(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, multi_commit_asym_got_votes}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_log_commit_rec(suite) -> [];
asymtrans_coord_log_commit_rec(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, multi_commit_asym_log_commit_rec}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_pid_log_commit_rec(suite) -> [];
asymtrans_coord_pid_log_commit_rec(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, multi_commit_asym_log_commit_rec}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_log_commit_dec(suite) -> [];
asymtrans_coord_log_commit_dec(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, multi_commit_asym_log_commit_dec}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_pid_log_commit_dec(suite) -> [];
asymtrans_coord_pid_log_commit_dec(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, multi_commit_asym_log_commit_dec}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_rec_acc_pre_commit_log_commit(suite) -> [];
asymtrans_coord_rec_acc_pre_commit_log_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, rec_acc_pre_commit_log_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_pid_rec_acc_pre_commit_log_commit(suite) -> [];
asymtrans_coord_pid_rec_acc_pre_commit_log_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, rec_acc_pre_commit_log_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_rec_acc_pre_commit_done_commit(suite) -> [];
asymtrans_coord_rec_acc_pre_commit_done_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(Coord, {Coord, {mnesia_tm, rec_acc_pre_commit_done_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

asymtrans_coord_pid_rec_acc_pre_commit_done_commit(suite) -> [];
asymtrans_coord_pid_rec_acc_pre_commit_done_commit(Config) when is_list(Config) -> 
    ?is_debug_compiled,
    Nodes = ?acquire_nodes(3, Config ++ [{tc_timeout, timer:minutes(2)}]),
    [Coord, Part1, Part2] = Nodes,
    Tab1 = {asym1, [{ram_copies, [Part2]}, {disc_copies, [Coord]}]},
    Tab2 = {asym2, [{ram_copies, [Coord]}, {disc_copies, [Part1]}]},
    TransFun = do_asym_trans,
    kill_after_debug_point(coord_pid, {Coord, {mnesia_tm, rec_acc_pre_commit_done_commit}}, 
			   TransFun, [Tab1, Tab2], Nodes).

do_asym_trans([Tab1, Tab2 | _R], Garbhandler) ->
    ?dl("Starting asym trans ", []),
    ASym_Trans = fun() ->
			 TidTs = {_Mod, Tid, _Store} =
			     mnesia:get_activity_id(),
			 ?verbose("===> asym_trans: ~w~n", [TidTs]),
			 Garbhandler ! {trans_id, Tid},
			 [{_, _, Val1}] = mnesia:read({Tab1, 1}),
			 [{_, _, Val2}] = mnesia:read({Tab2, 1}),
			 mnesia:write({Tab1, 1, Val1+1}),
			 mnesia:write({Tab2, 1, Val2+1})
		 end,
    Res = mnesia:transaction(ASym_Trans),
    case Res of
	{atomic, ok} -> ok;
	{aborted, _Reason} -> ok;
	_Else -> ?error("Wrong output from mensia:transaction(FUN):~n ~p~n", [Res])
    end,			
    ?dl("Asym trans finished with: ~p ", [Res]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kill_after_debug_point(Kill, {DebugNode, Debug_Point}, TransFun, TabsAndDefs, Nodes) ->
    [Coord | _rest] = Nodes,

    Create = fun({Tab, Def}) -> ?match({atomic, ok}, mnesia:create_table(Tab, Def)) end,
    lists:foreach(Create, TabsAndDefs),
    Tabs = [T || {T, _} <- TabsAndDefs],
    Write = fun(Tab) -> ?match(ok, mnesia:dirty_write({Tab, 1, 100})) end,
    lists:foreach(Write, Tabs),

    Self = self(),    
    SyncFun = fun(_Env1, _Env2) ->  % Just Sync with test prog
		      Self ! {self(), fun_in_position}, 
		      ?dl("SyncFun, sending fun_in_position ", []),
		      receive continue -> 
			      ?dl("SyncFun received continue ",[]),
			      ok
		      after timer:seconds(60) -> 
			      ?error("Timeout in sync_fun on ~p~n", [node()])
		      end
	      end,

    Garb_handler = spawn_link(?MODULE, garb_handler, [[]]),

    ?remote_activate_debug_fun(DebugNode, Debug_Point, SyncFun, []),
    ?dl("fun_in_position activated at ~p with ~p", [DebugNode, Debug_Point]),
    %% Spawn and do the transaction
    Pid = spawn(Coord, ?MODULE, TransFun, [Tabs, Garb_handler]),
    %% Wait till all the Nodes are in correct position
    [{StoppedPid,_}] = ?receive_messages([fun_in_position]),
    ?dl("Received fun_in_position; Removing the debug funs ~p", [DebugNode]),
    ?remote_deactivate_debug_fun(DebugNode, Debug_Point),    

    case Kill of
	coord_pid -> 
	    ?dl("Intentionally killing pid ~p ", [Pid]),
	    exit(Pid, normal);
	Node -> 		
	    mnesia_test_lib:kill_mnesia([Node])
    end,

    StoppedPid ! continue,    %% Send continue, it may still be alive

    %% Start and check that the databases are consistent
    ?dl("Done, Restarting and verifying result ",[]),
    case Kill of
	coord_pid -> ok;
	_ ->  % Ok, mnesia on some node was killed restart it
	    timer:sleep(timer:seconds(3)), %% Just let it have the time to die
	    ?match(ok, rpc:call(Kill, mnesia, start, [[]])),
	    ?match(ok, rpc:call(Kill, mnesia, wait_for_tables, [Tabs, 60000]))
    end,
    Trans_res = verify_tabs(Tabs, Nodes),
    case TransFun of
	do_asym_trans ->
	    %% Verifies that decisions are garbed, only valid for asym_tran
	    Garb_handler ! {get_tids, self()},
	    Tid_list = receive 
			   {tids, List} -> 
			       ?dl("Fun rec ~w", [List]),
			       List
		       end,
	    garb_of_decisions(Kill, Nodes, Tid_list, Trans_res);
	_ ->
	    ignore
    end,
    ?verify_mnesia(Nodes, []).

garb_of_decisions(Kill, Nodes, Tid_list, Trans_res) ->
    [Coord, Part1, Part2]  = Nodes,
    %% Check that decision log is empty on all nodes after the trans is finished
    verify_garb_decision_log(Nodes, Tid_list),  
    case Trans_res of
	aborted ->
	    %% Check that aborted trans have not been restarted!!
	    ?match(1, length(Tid_list)),
	    %% Check the transient decision logs
	    %% A transaction should only be aborted in an early stage of 
	    %% the trans before the any Node have logged anything
	    verify_garb_transient_logs(Nodes, Tid_list, aborted),
	    %% And only when the coordinator are have died
	    %% Else he would have restarted the transaction
	    ?match(Kill, Coord);
	updated ->
	    case length(Tid_list) of
		1 -> 
		    %% If there was only one transaction, it should be logged as
		    %% comitted on every node!
		    [Tid1] = Tid_list,
		    verify_garb_transient_logs(Nodes, [Tid1], committed);
		2 -> 
		    %% If there is two transaction id, then the first
		    %% TID should have been aborted and the transaction
		    %% restarted with a new TID
		    [Tid1, Tid2] = Tid_list,
		    verify_garb_transient_logs(Nodes, [Tid1], aborted),
		    %% If mnesia is killed on a node i.e Coord and Part1 than they
		    %% won't know about the restarted trans! The rest of the nodes
		    %% should know that the trans was committed
		    case Kill of 
			coord_pid -> 
			    verify_garb_transient_logs(Nodes, [Tid2], committed);
			Coord -> 
			    verify_garb_transient_logs([Part1, Part2], [Tid2], committed),
			    verify_garb_transient_logs([Coord], [Tid2], not_found);
			Part1 ->
			    verify_garb_transient_logs([Coord, Part2], [Tid2], committed),
			    verify_garb_transient_logs([Part1], [Tid2], not_found)
		    end
	    end
    end.

verify_garb_decision_log([], _Tids) -> ok;
verify_garb_decision_log([Node|R], Tids) -> 
    Check = fun(Tid) ->   %% Node, Tid used in debugging!
		    ?match({{not_found, _}, Node, Tid}, 
			   {outcome(Tid, [mnesia_decision]), Node, Tid})
	    end,
    rpc:call(Node, lists, foreach, [Check, Tids]),
    verify_garb_decision_log(R, Tids).

verify_garb_transient_logs([], _Tids, _) -> ok;
verify_garb_transient_logs([Node|R], Tids, Exp_Res) -> 
    Check = fun(Tid) -> 
		    LatestTab = mnesia_lib:val(latest_transient_decision),
		    PrevTabs = mnesia_lib:val(previous_transient_decisions),
		    case outcome(Tid, [LatestTab | PrevTabs]) of
			{found, {_, [{_,_Tid, Exp_Res}]}} -> ok;
			{not_found, _} when Exp_Res == not_found -> ok;
			{not_found, _} when Exp_Res == aborted -> ok;
			Else  -> ?error("Expected ~p in trans ~p on ~p got ~p~n",
					[Exp_Res, Tid, Node, Else])
		    end
	    end,    
    rpc:call(Node, lists, foreach, [Check, Tids]),
    verify_garb_transient_logs(R, Tids, Exp_Res). 
    
outcome(Tid, Tabs) ->
    outcome(Tid, Tabs, Tabs).

outcome(Tid, [Tab | Tabs], AllTabs) ->
    case catch ets:lookup(Tab, Tid) of
	{'EXIT', _} ->
	    outcome(Tid, Tabs, AllTabs);
	[] ->
	    outcome(Tid, Tabs, AllTabs);
	Val ->
	    {found, {Tab, Val}}
    end;
outcome(_Tid, [], AllTabs) ->
    {not_found, AllTabs}.


verify_tabs([Tab|R], Nodes) -> 
    [_Coord, Part1, Part2 | _rest] = Nodes, 
    Read = fun() -> mnesia:read({Tab, 1}) end,
    {success, A} = ?match({atomic, _}, mnesia:transaction(Read)),
    ?match(A, rpc:call(Part1, mnesia, transaction, [Read])),
    ?match(A, rpc:call(Part2, mnesia, transaction, [Read])),
    {atomic, [{Tab, 1, Res}]} = A,
    verify_tabs(R, Nodes, Res).

verify_tabs([], _Nodes, Res) -> 
    case Res of 
	100 -> aborted;
	101 -> updated
    end;

verify_tabs([Tab | Rest], Nodes, Res) ->
    [Coord, Part1, Part2 | _rest] = Nodes, 
    Read = fun() -> mnesia:read({Tab, 1}) end,
    Exp = {atomic, [{Tab, 1, Res}]},
    ?match(Exp, rpc:call(Coord, mnesia, transaction, [Read])),
    ?match(Exp, rpc:call(Part1, mnesia, transaction, [Read])),
    ?match(Exp, rpc:call(Part2, mnesia, transaction, [Read])),
    verify_tabs(Rest, Nodes, Res).

%% Gather TIDS and send them to requesting process and exit!
garb_handler(List) -> 
    receive 
	{trans_id, ID} -> garb_handler([ID|List]);
	{get_tids, Pid} -> Pid ! {tids, lists:reverse(List)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%
receive_messages([], _File, _Line) -> [];
receive_messages(ListOfMsgs, File, Line) ->
    receive 
	{Pid, Msg} ->     
	    case lists:member(Msg, ListOfMsgs) of
		false -> 
		    mnesia_test_lib:log("<>WARNING<>~n"
					"Received unexpected msg~n ~p ~n"
					"While waiting for ~p~n",  
					[{Pid, Msg}, ListOfMsgs], File, Line),
		    receive_messages(ListOfMsgs, File, Line);
		true -> 
		    ?dl("Got msg ~p from ~p ", [Msg, node(Pid)]),
		    [{Pid, Msg} | receive_messages(ListOfMsgs -- [Msg], File, Line)]
	    end;
	Else -> mnesia_test_lib:log("<>WARNING<>~n"
				    "Recevied unexpected or bad formatted msg~n ~p ~n"
				    "While waiting for ~p~n", 
	 			    [Else, ListOfMsgs], File, Line),
		receive_messages(ListOfMsgs, File, Line)
    after timer:minutes(2) -> 
	    ?error("Timeout in receive msgs while waiting for ~p~n", 
		   [ListOfMsgs])
    end.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

after_full_disc_partition(doc) ->
    ["Verify that the database does not get corrupt",
     "when Mnesia encounters a full disc partition"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interrupted_fallback_start 
%% is implemented in consistency interupted_install_fallback!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

after_corrupt_files_decision_log_head(suite) -> [];
after_corrupt_files_decision_log_head(Config) when is_list(Config) ->
    after_corrupt_files(Config, "DECISION.LOG", head, repair).

after_corrupt_files_decision_log_tail(suite) -> [];
after_corrupt_files_decision_log_tail(Config) when is_list(Config) ->
    after_corrupt_files(Config, "DECISION.LOG", tail, repair).

after_corrupt_files_latest_log_head(suite) -> [];
after_corrupt_files_latest_log_head(Config) when is_list(Config) ->
    after_corrupt_files(Config, "LATEST.LOG", head, repair).

after_corrupt_files_latest_log_tail(suite) -> [];
after_corrupt_files_latest_log_tail(Config) when is_list(Config) ->
    after_corrupt_files(Config, "LATEST.LOG", tail, repair).

after_corrupt_files_table_dat_head(suite) -> [];
after_corrupt_files_table_dat_head(Config) when is_list(Config) ->
    after_corrupt_files(Config, "rec_files.DAT", head, crash).

after_corrupt_files_table_dat_tail(suite) -> [];
after_corrupt_files_table_dat_tail(Config) when is_list(Config) ->
    after_corrupt_files(Config, "rec_files.DAT", tail, repair).

after_corrupt_files_schema_dat_head(suite) -> [];
after_corrupt_files_schema_dat_head(Config) when is_list(Config) ->
    after_corrupt_files(Config, "schema.DAT", head, crash).

after_corrupt_files_schema_dat_tail(suite) -> [];
after_corrupt_files_schema_dat_tail(Config) when is_list(Config) ->
    after_corrupt_files(Config, "schema.DAT", tail, crash).



%%% BUGBUG: We should also write testcase's for autorepair=false i.e. 
%%% not the standard case!
after_corrupt_files(Config, File, Where, Behaviour) ->
    [Node] = ?acquire_nodes(1, Config ++ [{tc_timeout, timer:minutes(2)}]),
    Tab = rec_files,
    Def = [{disc_only_copies, [Node]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert_data(Tab, 100),
    Dir = mnesia:system_info(directory),
    mnesia_test_lib:kill_mnesia([Node]),
    timer:sleep(timer:seconds(10)),   % Let dets finish whatever it does

    DirFile = Dir ++ "/" ++ File,

    {ok, Fd} = file:open(DirFile, read_write),
    {ok, FileInfo} = file:read_file_info(DirFile),
    case Where of
	head -> 
	    ?match({ok, _NewP}, file:position(Fd, {bof, 1})),
	    ?match(ok, file:write(Fd, [255, 255, 255, 255, 255, 255, 255, 255, 254])),
	    ok;
	tail ->
	    Size = FileInfo#file_info.size,
	    Half = Size div 2,

	    ?dl(" Size = ~p Half = ~p ", [Size, Half]),
	    ?match({ok, _NewP}, file:position(Fd, {bof, Half})),
	    ?match(ok, file:truncate(Fd)),
	    ok
    end,
    ?match(ok, file:close(Fd)),

    ?warning("++++++SOME OF THE after_corrupt* TEST CASES WILL INTENTIONALLY CRASH MNESIA+++++++~n", []),
    Pid = spawn_link(?MODULE, mymnesia_start, [self()]),
    receive
	{Pid, ok} -> 
	    ?match(ok, mnesia:wait_for_tables([schema, Tab], 10000)),
	    ?match(ok, verify_data(Tab, 100)),
	    case mnesia_monitor:get_env(auto_repair) of
		false ->
		    ?error("Mnesia should have crashed in ~p ~p ~n",
			   [File, Where]);
		true ->
		    ok
	    end,
	    ?verify_mnesia([Node], []);
	{Pid, {error, ED}} ->
	    case {mnesia_monitor:get_env(auto_repair), Behaviour} of
		{true, repair} ->
		    ?error("Mnesia crashed with ~p: in ~p ~p ~n",
			   [ED, File, Where]);
		_ ->  %% Every other can crash!
		    ok
	    end,
	    ?verify_mnesia([], [Node]);
	Msg ->
	    ?error("~p ~p: Got ~p during start of Mnesia~n",
		   [File, Where, Msg])
    end.

mymnesia_start(Tester) ->
    Res = mnesia:start(),
    unlink(Tester),
    Tester ! {self(), Res}.

verify_data(_, 0) -> ok;
verify_data(Tab, N) -> 
    Actual = mnesia:dirty_read({Tab, N}),
    Expected = [{Tab, N, N}],
    if
	Expected == Actual ->
	    verify_data(Tab, N - 1);
	true  ->
	    mnesia:schema(Tab),
	    {not_equal, node(), Expected, Actual}
    end.

insert_data(_Tab, 0) -> ok;
insert_data(Tab, N) ->
    ok = mnesia:sync_dirty(fun() -> mnesia:write({Tab, N, N}) end),
    insert_data(Tab, N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disc_less(doc) -> 
    ["Here is a simple test case of a simple recovery of a disc less node. "
     "However a lot more test cases involving disc less nodes should "
     "be written"];
disc_less(suite) -> [];
disc_less(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    case mnesia_test_lib:diskless(Config) of
	true -> skip;
	false ->    
	    ?match({atomic, ok}, mnesia:change_table_copy_type(schema, Node3, ram_copies))
    end,
    Tab1 = disc_less1,
    Tab2 = disc_less2,
    Tab3 = disc_less3,
    Def1 = [{ram_copies, [Node3]}, {disc_copies, [Node1, Node2]}],
    Def2 = [{ram_copies, [Node3]}, {disc_copies, [Node1]}],
    Def3 = [{ram_copies, [Node3]}, {disc_copies, [Node2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab1, Def1)),
    ?match({atomic, ok}, mnesia:create_table(Tab2, Def2)),
    ?match({atomic, ok}, mnesia:create_table(Tab3, Def3)),   
    insert_data(Tab1, 100),
    insert_data(Tab2, 100),
    insert_data(Tab3, 100),

    mnesia_test_lib:kill_mnesia([Node1, Node2]),
    timer:sleep(500),
    mnesia_test_lib:kill_mnesia([Node3]),
    ?match(ok, rpc:call(Node1, mnesia, start, [])),
    ?match(ok, rpc:call(Node2, mnesia, start, [])),

    timer:sleep(500),
    ?match(ok, rpc:call(Node3, mnesia, start, [[{extra_db_nodes, [Node1, Node2]}, {schema, ?BACKEND}]])),
    ?match(ok, rpc:call(Node3, mnesia, wait_for_tables, [[Tab1, Tab2, Tab3], 20000])),
    ?match(ok, rpc:call(Node1, mnesia, wait_for_tables, [[Tab1, Tab2, Tab3], 20000])),

    ?match(ok, rpc:call(Node3, ?MODULE, verify_data, [Tab1, 100])),
    ?match(ok, rpc:call(Node3, ?MODULE, verify_data, [Tab2, 100])),
    ?match(ok, rpc:call(Node3, ?MODULE, verify_data, [Tab3, 100])),


    ?match(ok, rpc:call(Node2, ?MODULE, verify_data, [Tab1, 100])),
    ?match(ok, rpc:call(Node2, ?MODULE, verify_data, [Tab2, 100])),
    ?match(ok, rpc:call(Node2, ?MODULE, verify_data, [Tab3, 100])),

    ?match(ok, rpc:call(Node1, ?MODULE, verify_data, [Tab1, 100])),
    ?match(ok, rpc:call(Node1, ?MODULE, verify_data, [Tab2, 100])),
    ?match(ok, rpc:call(Node1, ?MODULE, verify_data, [Tab3, 100])),
    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

system_upgrade(doc) ->
    ["Test on-line and off-line upgrade of the Mnesia application"].

garb_decision(doc) ->
    ["Test that decisions are garbed correctly."];
garb_decision(suite) -> [];
garb_decision(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    check_garb(Nodes),
    ?match({atomic, ok},mnesia:create_table(a, [{disc_copies, Nodes}])),
    check_garb(Nodes),
    ?match({atomic, ok},mnesia:create_table(b, [{ram_copies, Nodes}])),
    check_garb(Nodes),
    ?match({atomic, ok},mnesia:create_table(c, [{ram_copies, [Node1, Node3]},
						{disc_copies, [Node2]}])),
    check_garb(Nodes),
    ?match({atomic, ok},mnesia:create_table(d, [{disc_copies, [Node1, Node3]},
						{ram_copies, [Node2]}])),
    check_garb(Nodes),

    W = fun(Tab) ->  mnesia:write({Tab,1,1}) end,
    A = fun(Tab) ->  mnesia:write({Tab,1,1}), exit(1) end,

    ?match({atomic, ok}, mnesia:transaction(W,[a])),
    check_garb(Nodes),
    ?match({atomic, ok}, mnesia:transaction(W,[b])),
    check_garb(Nodes),
    ?match({atomic, ok}, mnesia:transaction(W,[c])),
    check_garb(Nodes),
    ?match({atomic, ok}, mnesia:transaction(W,[d])),
    check_garb(Nodes),
    ?match({aborted,1}, mnesia:transaction(A,[a])),
    check_garb(Nodes),
    ?match({aborted,1}, mnesia:transaction(A,[b])),
    check_garb(Nodes),
    ?match({aborted,1}, mnesia:transaction(A,[c])),
    check_garb(Nodes),
    ?match({aborted,1}, mnesia:transaction(A,[d])),
    check_garb(Nodes),

    rpc:call(Node2, mnesia, lkill, []),
    ?match({atomic, ok}, mnesia:transaction(W,[a])),
    ?match({atomic, ok}, mnesia:transaction(W,[b])),
    ?match({atomic, ok}, mnesia:transaction(W,[c])),
    ?match({atomic, ok}, mnesia:transaction(W,[d])),
    check_garb(Nodes),
    ?match([], mnesia_test_lib:start_mnesia([Node2])),    
    check_garb(Nodes),
    timer:sleep(2000),
    check_garb(Nodes),
    %%%%%% Check transient_decision logs %%%%%

    ?match(dumped, mnesia:dump_log()), sys:get_status(mnesia_recover), % sync   
    [{atomic, ok} = mnesia:transaction(W,[a]) || _ <- lists:seq(1,30)],
    ?match(dumped, mnesia:dump_log()), sys:get_status(mnesia_recover), % sync   
    TD0 = mnesia_lib:val(latest_transient_decision),
    ?match(0, ets:info(TD0, size)),
    {atomic, ok} = mnesia:transaction(W,[a]),
    ?match(dumped, mnesia:dump_log()), sys:get_status(mnesia_recover), % sync   
    ?match(TD0, mnesia_lib:val(latest_transient_decision)),
    [{atomic, ok} = mnesia:transaction(W,[a]) || _ <- lists:seq(1,30)],
    ?match(dumped, mnesia:dump_log()), sys:get_status(mnesia_recover), % sync   
    ?match(false, TD0 =:= mnesia_lib:val(latest_transient_decision)),
    ?match(true, lists:member(TD0, mnesia_lib:val(previous_transient_decisions))),
    ?verify_mnesia(Nodes, []).

check_garb(Nodes) ->
    rpc:multicall(Nodes, sys, get_status, [mnesia_recover]),
    ?match({_, []},rpc:multicall(Nodes, erlang, apply, [fun check_garb/0, []])).

check_garb() ->
    try 
	Ds = ets:tab2list(mnesia_decision),
	Check = fun({trans_tid,serial, _}) -> false;
		   ({mnesia_down,_,_,_}) -> false;
		   (_Else) -> true
		end,
	Node = node(),
	?match({Node, []}, {node(), lists:filter(Check, Ds)})
    catch _:_ -> ok  
    end,
    ok.
