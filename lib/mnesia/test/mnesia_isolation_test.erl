%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(mnesia_isolation_test).
-author('hakan@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([no_conflict/1, simple_queue_conflict/1,
         advanced_queue_conflict/1, simple_deadlock_conflict/1,
         advanced_deadlock_conflict/1, schema_deadlock/1, lock_burst/1,
         nasty/1, basic_sticky_functionality/1, sticky_sync/1,
         unbound1/1, unbound2/1,
         create_table/1, delete_table/1, move_table_copy/1,
         add_table_index/1, del_table_index/1, transform_table/1,
         snmp_open_table/1, snmp_close_table/1,
         change_table_copy_type/1, change_table_access/1,
         add_table_copy/1, del_table_copy/1, dump_tables/1,
         del_table_copy_1/1, del_table_copy_2/1, del_table_copy_3/1,
         add_table_copy_1/1, add_table_copy_2/1, add_table_copy_3/1,
         add_table_copy_4/1, move_table_copy_1/1, move_table_copy_2/1,
         move_table_copy_3/1, move_table_copy_4/1,
         dirty_updates_visible_direct/1,
         dirty_reads_regardless_of_trans/1,
         trans_update_invisibible_outside_trans/1,
         trans_update_visible_inside_trans/1, write_shadows/1,
         delete_shadows/1, write_delete_shadows_bag/1,
         write_delete_shadows_bag2/1,
         shadow_search/1, snmp_shadows/1,
         rr_kill_copy/1, foldl/1, first_next/1]).

-export([do_fun/4, burst_counter/3, burst_incr/2, get_held/0, get_info/1,
         get_sticky/0, op/4, update_own/3, update_shared/3]).

-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
    [{group, locking}, {group, visibility}].

groups() -> 
    [{locking, [],
      [no_conflict, simple_queue_conflict,
       advanced_queue_conflict, simple_deadlock_conflict,
       advanced_deadlock_conflict, schema_deadlock, lock_burst,
       {group, sticky_locks}, {group, unbound_locking},
       {group, admin_conflict}, nasty]},
     {sticky_locks, [],
      [basic_sticky_functionality,sticky_sync]},
     {unbound_locking, [], [unbound1, unbound2]},
     {admin_conflict, [],
      [create_table, delete_table, move_table_copy,
       add_table_index, del_table_index, transform_table,
       snmp_open_table, snmp_close_table,
       change_table_copy_type, change_table_access,
       add_table_copy, del_table_copy, dump_tables,
       {group, extra_admin_tests}]},
     {extra_admin_tests, [],
      [del_table_copy_1, del_table_copy_2, del_table_copy_3,
       add_table_copy_1, add_table_copy_2, add_table_copy_3,
       add_table_copy_4, move_table_copy_1, move_table_copy_2,
       move_table_copy_3, move_table_copy_4]},
     {visibility, [],
      [dirty_updates_visible_direct,
       dirty_reads_regardless_of_trans,
       trans_update_invisibible_outside_trans,
       trans_update_visible_inside_trans, write_shadows,
       delete_shadows, write_delete_shadows_bag,
       write_delete_shadows_bag2, {group, iteration},
       shadow_search, snmp_shadows]},
     {removed_resources, [], [rr_kill_copy]},
     {iteration, [], [foldl, first_next]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
no_conflict(suite) -> [];
no_conflict(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = no_conflict, 
    create_conflict_table(Tab, [Node1]), 
    Fun = fun(OtherOid, Lock1, Lock2) ->
		  %% Start two transactions
		  {success, [B, A]} = ?start_activities([Node1, Node1]), 
		  ?start_transactions([B, A]), 
		  
		  A ! fun() -> Lock1(one_oid(Tab)),  ok end, 
		  ?match_receive({A, ok}), 
		  B ! fun() -> Lock2(OtherOid),  ok end, 
		  ?match_receive({B, ok}), 
		  A ! fun() -> mnesia:abort(ok) end, 
		  ?match_receive({A, {aborted, ok}}), 
		  B ! fun() -> mnesia:abort(ok) end, 
		  ?match_receive({B, {aborted, ok}})
	  end, 
    NoLocks = lock_funs(no_lock, any_granularity), 
    SharedLocks = lock_funs(shared_lock, any_granularity), 
    AnyLocks = lock_funs(any_lock, any_granularity), 
    OneOneFun = fun(Lock1, Lock2) -> Fun(one_oid(Tab), Lock1, Lock2) end, 
    fun_loop(OneOneFun, NoLocks, AnyLocks), 
    fun_loop(OneOneFun, AnyLocks, NoLocks), 
    fun_loop(OneOneFun, SharedLocks, SharedLocks), 

    %% Lock different objects
    OneOtherFun = fun(Lock1, Lock2) -> Fun(other_oid(Tab), Lock1, Lock2) end, 
    OneSharedLocks = lock_funs(shared_lock, one), 
    OneExclusiveLocks = lock_funs(exclusive_lock, one), 
    fun_loop(OneOtherFun, OneSharedLocks, OneExclusiveLocks), 
    fun_loop(OneOtherFun, OneExclusiveLocks, OneSharedLocks), 
    fun_loop(OneOtherFun, OneExclusiveLocks, OneExclusiveLocks), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simple_queue_conflict(suite) -> [];
simple_queue_conflict(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = simple_queue_conflict, 
    create_conflict_table(Tab, [Node1]), 
    Fun = fun(OneLock, OtherLock) ->
		  %% Start two transactions
		  {success, [B, A]} = ?start_activities([Node1, Node1]), 
		  ?start_transactions([B, A]), 
		  
		  A ! fun() -> OneLock(one_oid(Tab)),  ok end, 
		  ?match_receive({A, ok}), 
		  B ! fun() -> OtherLock(one_oid(Tab)),  ok end, 
		  wait_for_lock(B, [Node1], 20), % Max 10 sec
		  A ! end_trans, 
		  ?match_multi_receive([{A, {atomic, end_trans}}, {B, ok}]), 
		  B ! fun() -> mnesia:abort(ok) end, 
		  ?match_receive({B, {aborted, ok}})
	  end, 
    OneSharedLocks = lock_funs(shared_lock, one), 
    AllSharedLocks = lock_funs(shared_lock, all), 
    OneExclusiveLocks = lock_funs(exclusive_lock, one), 
    AllExclusiveLocks = lock_funs(exclusive_lock, all), 
    fun_loop(Fun, OneExclusiveLocks, OneExclusiveLocks), 
    fun_loop(Fun, AllExclusiveLocks, AllExclusiveLocks), 
    fun_loop(Fun, OneExclusiveLocks, AllExclusiveLocks), 
    fun_loop(Fun, AllExclusiveLocks, OneExclusiveLocks), 
    fun_loop(Fun, OneSharedLocks, AllExclusiveLocks), 
    fun_loop(Fun, AllSharedLocks, OneExclusiveLocks), 
    ok.

wait_for_lock(Pid, Nodes, Retry) ->
    wait_for_lock(Pid, Nodes, Retry, queue).

wait_for_lock(Pid, _Nodes, 0, queue) ->
    Queue = mnesia:system_info(lock_queue),
    ?error("Timeout while waiting for lock on Pid ~p in queue ~p~n", [Pid, Queue]);
wait_for_lock(Pid, _Nodes, 0, held) ->
    Held = mnesia:system_info(held_locks),
    ?error("Timeout while waiting for lock on Pid ~p (held) ~p~n", [Pid, Held]);
wait_for_lock(Pid, Nodes, N, Where) ->
    rpc:multicall(Nodes, sys, get_status, [mnesia_locker]),
    List = case Where of
	       queue ->
		   [rpc:call(Node, mnesia, system_info, [lock_queue]) || Node <- Nodes];
	       held ->
		   [rpc:call(Node, mnesia, system_info, [held_locks]) || Node <- Nodes]
           end,
    Q = lists:append(List),
    check_q(Pid, Q, Nodes, N, Where).

check_q(Pid, [{_Oid, _Op, Pid, _Tid, _WFT} | _Tail], _N, _Count, _Where) -> ok;
check_q(Pid, [{_Oid, _Op, {tid,_,Pid}} | _Tail], _N, _Count, _Where) -> ok;
check_q(Pid, [_ | Tail], N, Count, Where) -> check_q(Pid, Tail, N, Count, Where);
check_q(Pid, [], N, Count, Where) ->
    timer:sleep(200),
    wait_for_lock(Pid, N, Count - 1, Where).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
advanced_queue_conflict(suite) -> [];
advanced_queue_conflict(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = advanced_queue_conflict, 
    create_conflict_table(Tab, [Node1]), 
    OneRec = {Tab, 3, 3}, 
    OneOid = {Tab, 3}, 
    OtherRec = {Tab, 4, 4}, 
    OtherOid = {Tab, 4}, 
    
    %% Start four transactions
    {success, [D, C, B, A]} = ?start_activities(lists:duplicate(4, Node1)), 
    ?start_transactions([D, C, B, A]), 
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 

    %% Acquire some locks
    A ! fun() -> mnesia:write(OneRec) end, 
    ?match_receive({A, ok}), 
    A ! fun() -> mnesia:read(OneOid) end, 
    ?match_receive({A, [OneRec]}), 
    
    B ! fun() -> mnesia:write(OtherRec) end, 
    ?match_receive({B, ok}), 
    B ! fun() -> mnesia:read(OneOid) end, 
    ?match_receive(timeout), 
    
    C ! fun() -> mnesia:read(OtherOid) end, 
    ?match_receive(timeout), 
    D ! fun() -> mnesia:wread(OtherOid) end, 
    ?match_receive(timeout), 
    
    %% and release them in a certain order
    A ! end_trans, 
    ?match_multi_receive([{A, {atomic, end_trans}}, {B, [OneRec]}]), 
    B ! end_trans, 
    ?match_multi_receive([{B, {atomic, end_trans}}, {C, [OtherRec]}]), 
    C ! end_trans, 
    ?match_multi_receive([{C, {atomic, end_trans}}, {D, [OtherRec]}]), 
    D ! end_trans, 
    ?match_receive({D, {atomic, end_trans}}), 
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simple_deadlock_conflict(suite) -> [];
simple_deadlock_conflict(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = simple_deadlock_conflict, 
    create_conflict_table(Tab, [Node1]), 
    Rec = {Tab, 4, 4}, 
    Oid = {Tab, 4}, 
    
    %% Start two transactions
    {success, [B, A]} = ?start_activities(lists:duplicate(2, Node1)), 
    mnesia_test_lib:start_transactions([B, A], 0),  % A is newest
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 

    B ! fun() -> mnesia:write(Rec) end, 
    ?match_receive({B, ok}), 
    A ! fun() -> mnesia:read(Oid) end, 
    ?match_receive({A, {aborted, nomore}}), 
    B ! end_trans, 
    ?match_receive({B, {atomic, end_trans}}), 

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
advanced_deadlock_conflict(suite) -> [];
advanced_deadlock_conflict(Config) when is_list(Config) ->
    [Node1, Node2] = ?acquire_nodes(2, Config), 
    Tab = advanced_deadlock_conflict, 
    create_conflict_table(Tab, [Node2]), 
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    Rec = {Tab, 4, 4}, 
    Oid = {Tab, 4}, 
    
    %% Start two transactions
    {success, [B, A]} = ?start_activities([Node1, Node2]), 
    mnesia_test_lib:start_sync_transactions([B, A], 0),  % A is newest
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 

    B ! fun() -> mnesia:write(Rec) end, 
    ?match_receive({B, ok}), 
    A ! fun() -> mnesia:read(Oid) end, 
    ?match_receive({A, {aborted, nomore}}), 
    B ! end_trans, 
    ?match_receive({B, {atomic, end_trans}}), 

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

%%  Verify (and regression test) deadlock in del_table_copy(schema, Node)
schema_deadlock(Config) when is_list(Config) ->
    Ns = [Node1, Node2] = ?acquire_nodes(2, Config),
    ?match({atomic, ok}, mnesia:create_table(a, [{disc_copies, Ns}])),
    ?match({atomic, ok}, mnesia:create_table(b, [{disc_copies, Ns}])),

    Tester = self(),

    Deadlocker = fun() ->
			 mnesia:write({a,1,1}),  %% grab write lock on A
			 receive
			     continue ->
				 mnesia:write({b,1,1}), %% grab write lock on B
				 end_trans
			 end
		 end,

    ?match(stopped, rpc:call(Node2, mnesia, stop, [])),
    timer:sleep(500), %% Let Node1 reconfigure
    sys:get_status(mnesia_monitor),

    DoingTrans = spawn_link(fun() ->  Tester ! {self(),mnesia:transaction(Deadlocker)} end),
    wait_for_lock(DoingTrans, [Node1], 10, held),
    %% Will grab write locks on schema, a, and b
    DoingSchema = spawn_link(fun() -> Tester ! {self(), mnesia:del_table_copy(schema, Node2)} end),
    timer:sleep(500), %% Let schema trans start, and try to grab locks
    DoingTrans ! continue,

    ?match(ok, receive {DoingTrans,  {atomic, end_trans}} -> ok after 5000 -> timeout end),
    ?match(ok, receive {DoingSchema,  {atomic, ok}} -> ok after 5000 -> timeout end),

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)),
    ?match([], mnesia:system_info(lock_queue)),
    ok.


one_oid(Tab) -> {Tab, 1}.
other_oid(Tab) -> {Tab, 2}.
    
create_conflict_table(Tab, Nodes) ->
    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					     {ram_copies, Nodes}, 
					     {attributes, [key, val]}, 
					     {index, [val]}
					    ])),
    ?match([], mnesia_test_lib:sync_tables(Nodes, [Tab])),
    init_conflict_table(Tab).

init_conflict_table(Tab) ->
    Recs = mnesia:dirty_match_object({Tab, '_', '_'}), 
    lists:foreach(fun(R) -> mnesia:dirty_delete_object(R) end, Recs), 
    Keys = [one_oid(Tab), other_oid(Tab)], 
    [mnesia:dirty_write({T, K, K}) || {T, K} <- Keys].

%% Apply Fun for each X and Y
fun_loop(Fun, Xs, Ys) ->
    lists:foreach(fun(X) -> lists:foreach(fun(Y) -> do_fun(Fun, X, Y) end, Ys) end, Xs).

do_fun(Fun, X, Y) ->
    Pid = spawn_link(?MODULE, do_fun, [self(), Fun, X, Y]), 
    receive
	{done_fun, Pid} -> done_fun
    end.

do_fun(Monitor, Fun, X, Y) ->
    ?log("{do_fun ~p~n", [[Fun, X, Y]]), 
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    Fun(X, Y), 
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    unlink(Monitor), 
    Monitor ! {done_fun, self()}, 
    exit(done_fun).

%% Returns a list of fun's
lock_funs(no_lock, one) ->
    [
     fun(Oid) -> mnesia:dirty_read(Oid) end, 
     fun({Tab, Key}) -> mnesia:dirty_write({Tab, Key, Key}) end, 
     fun({Tab, Key}) -> mnesia:dirty_write({Tab, Key, Key}), 
		       mnesia:dirty_update_counter({Tab, Key}, 0) end, 
     fun(Oid) -> mnesia:dirty_delete(Oid) end, 
     fun({Tab, Key}) -> mnesia:dirty_delete_object({Tab, Key, Key}) end, 
     fun({Tab, Key}) -> mnesia:dirty_match_object({Tab, Key, Key}) end, 
     fun({Tab, Key}) -> mnesia:dirty_index_match_object({Tab, Key, Key}, val) end, 
     fun({Tab, Key}) -> mnesia:dirty_index_read(Tab, Key, val) end, 
     fun({Tab, Key}) -> mnesia:dirty_index_match_object({Tab, '_', Key}, val) end
    ];
lock_funs(no_lock, all) ->
    [
     fun({Tab, _}) -> mnesia:dirty_match_object({Tab, '_', '_'}) end, 
     fun({Tab, _}) -> slot_iter(Tab) end, 
     fun({Tab, _}) -> key_iter(Tab) end
    ];
lock_funs(shared_lock, one) ->
    
    [
     fun(Oid) -> mnesia:read(Oid) end, 
     fun({Tab, Key}) ->
	     init_conflict_table(Tab), 
	     mnesia:dirty_delete(other_oid(Tab)), 
	     mnesia:match_object({Tab, Key, Key}) end
     ];
lock_funs(shared_lock, all) ->
    [
     fun({Tab, _}) -> mnesia:read_lock_table(Tab) end, 
     fun({Tab, Key}) -> mnesia:match_object({Tab, '_', Key}) end, 
     fun({Tab, _}) -> mnesia:match_object({Tab, '_', '_'}) end, 
     fun({Tab, _}) -> mnesia:all_keys(Tab) end, 
     fun({Tab, Key}) -> mnesia:index_match_object({Tab, '_', Key}, val) end,
     fun({Tab, Key}) -> mnesia:index_read(Tab, Key, val) end
    ];
lock_funs(exclusive_lock, one) ->
    [
     fun(Oid) -> mnesia:wread(Oid) end, 
     fun({Tab, Key}) -> mnesia:write({Tab, Key, Key}) end, 
     fun(Oid) -> mnesia:delete(Oid) end, 
     fun({Tab, Key}) -> mnesia:delete_object({Tab, Key, Key}) end, 
     fun({Tab, Key}) -> mnesia:s_write({Tab, Key, Key}) end, 
     fun(Oid) -> mnesia:s_delete(Oid) end, 
     fun({Tab, Key}) -> mnesia:s_delete_object({Tab, Key, Key}) end
    ];
lock_funs(exclusive_lock, all) ->
    [
     fun({Tab, _}) -> mnesia:write_lock_table(Tab) end
    ];
lock_funs(Compatibility, any_granularity) ->
    lists:append([lock_funs(Compatibility, Granularity) ||
		     Granularity <- [one, all]]);
lock_funs(any_lock, Granularity) ->
    lists:append([lock_funs(Compatibility, Granularity) ||
		     Compatibility <- [no_lock, shared_lock, exclusive_lock]]).

slot_iter(Tab) ->
    slot_iter(Tab, mnesia:dirty_slot(Tab, 0), 1).
slot_iter(_Tab, '$end_of_table', _) ->
    [];
slot_iter(Tab, Recs, Slot) ->
    Recs ++ slot_iter(Tab, mnesia:dirty_slot(Tab, Slot), Slot+1).

key_iter(Tab) ->
    key_iter(Tab, mnesia:dirty_first(Tab)).
key_iter(_Tab, '$end_of_table') ->
    [];
key_iter(Tab, Key) ->
    [Key | key_iter(Tab, mnesia:dirty_next(Tab, Key))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lock_burst(suite) -> [];
lock_burst(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = burst, 
    ?match({atomic, ok},  mnesia:create_table(Tab, 
					    [{attributes,  [a, b]}, 
					     {ram_copies, [Node1]}])), 
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ?match(ok, burst_em(Tab, 1000)), 
    ?match([{burst,1,1000}], mnesia:dirty_read(Tab,1)),
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

burst_em(Tab, N) ->
    spawn_link(?MODULE, burst_counter, [self(), Tab, N]), 
    receive
	burst_counter_done -> ok
    end.

burst_counter(Monitor, Tab, N) when N > 0 ->
    ?match(ok, burst_gen(Tab, N, self())), 
    Monitor ! burst_receiver(N).

burst_receiver(0) ->
    burst_counter_done;
burst_receiver(N) ->
    receive
	burst_incr_done ->
	    burst_receiver(N-1)
    end.
    
burst_gen(_, 0, _) ->
    ok;
burst_gen(Tab, N, Father) when is_integer(N), N > 0 ->
    spawn_link(?MODULE,  burst_incr,  [Tab, Father]), 
    burst_gen(Tab, N-1, Father).

burst_incr(Tab, Father) ->
    Fun = fun() ->
		  Val = 
		      case mnesia:read({Tab, 1}) of
			  [{Tab, 1, V}] -> V;
			  [] -> 0
		      end, 
		  mnesia:write({Tab, 1, Val+1})
	  end, 
    ?match({atomic, ok}, mnesia:transaction(Fun)), 
    Father ! burst_incr_done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_sticky_functionality(suite) -> [];
basic_sticky_functionality(Config) when is_list(Config) -> 
    [N1, N2] = Nodes = ?acquire_nodes(2, Config),
    Tab = basic_table,
    Storage = mnesia_test_lib:storage_type(disc_copies, Config),
    ?match({atomic, ok}, mnesia:create_table(Tab, [{Storage, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(sync, [{ram_copies, Nodes}])),
    Trans1 = fun() ->
		     ?match(ok, mnesia:s_write({Tab, 1, 2})),
		     ?match([{Tab, 1, 2}], mnesia:read({Tab, 1})),
		     ?match(timeout, receive M -> M after 500 -> timeout end),
		     ?match(ok, mnesia:s_write({Tab, 2, 2})),
		     ?match(ok, mnesia:write({Tab, 42, 4711}))
	     end,
    Trans2 = fun() ->
		     ?match([{Tab, 1, 2}],  mnesia:read({Tab, 1})),
		     ?match(timeout, receive M -> M after 500 -> timeout end),
		     ?match(ok, mnesia:write({Tab, 1, 4711})),
		     ?match(ok, mnesia:s_write({Tab, 2, 4})),
		     ?match(ok, mnesia:delete({Tab, 42}))
	     end,
    rpc:call(N1, mnesia, transaction, [Trans1]),
    ?match([{Tab,N1}], rpc:call(N1, ?MODULE, get_sticky, [])),
    ?match([{Tab,N1}], rpc:call(N2, ?MODULE, get_sticky, [])),

    rpc:call(N2, mnesia, transaction, [Trans2]),
    ?match([], rpc:call(N1, ?MODULE, get_sticky, [])),
    ?match([], rpc:call(N2, ?MODULE, get_sticky, [])),
    
    Slock = fun() -> mnesia:read({sync,sync}),get_sticky() end,
    ?match({atomic, [{Tab,1, 4711}]}, mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),
    ?match({atomic, [{Tab,2, 4}]},    mnesia:transaction(fun() -> mnesia:read({Tab, 2}) end)),
    ?match({atomic, [{Tab,N1}]}, rpc:call(N1, mnesia, transaction, 
					  [fun() -> mnesia:s_write({Tab, 1, 3}),Slock() end])),
    ?match([{Tab,N1}], rpc:call(N2, ?MODULE, get_sticky, [])),
    
    ?match({atomic,[]}, rpc:call(N2, mnesia, transaction, 
				 [fun() -> mnesia:s_write({Tab, 1, 4}),Slock()  end])),

    ?match([], rpc:call(N1, ?MODULE, get_sticky, [])),
    ?match([], rpc:call(N2, ?MODULE, get_sticky, [])),
    
    ?match({atomic,[{Tab,N2}]}, rpc:call(N2, mnesia, transaction, 
					 [fun() -> mnesia:s_write({Tab, 1, 4}),Slock() end])),
    
    ?match({atomic,[]}, rpc:call(N1, mnesia, transaction, 
				 [fun() -> mnesia:s_write({Tab, 1, 5}),Slock()  end])),
    ?match({atomic,[{Tab,N1}]}, rpc:call(N1, mnesia, transaction, 
					 [fun() -> mnesia:s_write({Tab, 1, 5}),Slock()  end])),
    ?match({atomic,[]}, rpc:call(N2, mnesia, transaction, 
				 [fun() -> mnesia:s_write({Tab, 1, 6}),Slock()  end])),
    ?match({atomic,[{Tab,N2}]}, rpc:call(N2, mnesia, transaction, 
					 [fun() -> mnesia:s_write({Tab, 1, 7}),Slock()  end])),
    
    ?match([{Tab,N2}], get_sticky()),
    ?match({atomic, [{Tab,1, 7}]}, mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),
    ?match([{Tab,N2}], get_sticky()),
    ?match({atomic, [{Tab,2, 4}]}, mnesia:transaction(fun() -> mnesia:read({Tab, 2}) end)),
    ?match([{Tab,N2}], get_sticky()),
    ?match({atomic,[{Tab,N2}]}, rpc:call(N2, mnesia, transaction, 
					 [fun() -> mnesia:s_write({Tab, 1, 6}),Slock()  end])),
    ?match([{Tab,N2}], get_sticky()),
    ?match({atomic, [{Tab,1, 6}]}, mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),
    ?match([{Tab,N2}], get_sticky()),
    ?match({atomic, [{Tab,2, 4}]}, mnesia:transaction(fun() -> mnesia:read({Tab, 2}) end)),
    ?match([{Tab,N2}], get_sticky()),
    ?verify_mnesia(Nodes, []).

get_sticky() ->
    mnesia_locker ! {get_table, self(), mnesia_sticky_locks},
    receive {mnesia_sticky_locks, Locks} -> Locks end.

get_held() ->
    mnesia_locker ! {get_table, self(), mnesia_sticky_locks},
    receive {mnesia_sticky_locks, Locks} -> Locks end.

sticky_sync(suite) -> [];
sticky_sync(Config) when is_list(Config) ->
    %% BUG ERIERL-768
    Nodes = [N1, N2] = ?acquire_nodes(2, Config),

    mnesia:create_table(dc, [{type, ordered_set}, {disc_copies, Nodes}]),
    mnesia:create_table(ec, [{type, ordered_set}, {ram_copies, [N2]}]),

    TestFun =
        fun(I) ->
                %% In first transaction we initialise {dc, I} record with value 0
                First = fun() ->
                                %% Do a lot of writes into ram copies table
                                %% which on the Slave in do_commit will be
                                %% processed first
                                lists:foreach(fun(J) -> ok = mnesia:write(ec, {ec, J, 0}, write) end,
                                              lists:seq(1, 750)),
                                %% Then set initial value of {dc, I} record to 0 with sticky_write
                                mnesia:write(dc, {dc, I, 0}, sticky_write)
                        end,
                ok = mnesia:activity(transaction, First),
                %% In second transaction we set value of {dc, I} record to 1
                Upd = fun() ->
                              %% Modify a single ram copies record with ensured lock grant
                              %% (key not used in previous transactions)
                              %% we use this second table only to force asym_trans protocol
                              mnesia:write(ec, {ec, 1001 + I, 0}, write),
                              %% And set final version of {dc, I} record to 1 with sticky_write
                              mnesia:write(dc, {dc, I, 1}, sticky_write)
                    end,
                ok = mnesia:activity(transaction, Upd)
        end,

    %% Fill 1000 dc records. At the end all dc records should have value 1.
    lists:foreach(TestFun, lists:seq(1,1000)),
    io:format("Written, check content~n",[]),
    All = fun() -> mnesia:select(dc, [ {{dc, '_', 0}, [] ,['$_']} ]) end,
    ?match({atomic, []}, rpc:call(N1, mnesia, sync_transaction, [All])),
    ?match({atomic, []}, rpc:call(N2, mnesia, sync_transaction, [All])),

    ?verify_mnesia(Nodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unbound1(suite) -> [];
unbound1(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config),
    
    ?match({atomic, ok}, mnesia:create_table(ul, [])),
    
    Tester = self(),
    Write = fun() ->
		    mnesia:write({ul, {key, {17,42}}, val}),
		    ?log("~p Got write lock waiting...~n", [self()]),  
		    Tester ! continue,
		    receive 
			continue ->
			    ok
		    end,
		    ?log("..continuing~n", []),
		    ok
	    end,
    
    {success, [A]} = ?start_activities([Node1]),
    ?start_transactions([A]),    
    A ! Write,    
    
    receive continue -> ok end,

    Match = fun() ->		    
		    case catch mnesia:match_object({ul, {key, {'_', '$0'}}, '_'}) of
			{'EXIT', What} ->  %% Cyclic first time
			    ?log("Cyclic Restarting~n", []),
			    A ! continue,
			    A ! end_trans,
			    exit(What);
			Res ->
			    ?log("Got match log ~p...~n", [Res]),
			    Res
		    end
	    end,
    ?match({atomic, [{ul,{key,{17,42}},val}]}, mnesia:transaction(Match)),

    ?match_receive({A, ok}),
    ?match_receive({A, {atomic, end_trans}}),
    ok.

unbound2(suite) -> [];
unbound2(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config),
    
    ?match({atomic, ok}, mnesia:create_table(ul, [])),
        
    {success, [B, A]} = ?start_activities([Node1, Node1]),
    
    Me = self(),
    
    Write = fun() ->
		    mnesia:write({ul, {key, {17,42}}, val}),
		    ?log("~p Got write lock waiting... Tid ~p ~n", 
			 [self(), get(mnesia_activity_state)]),  
		    Me ! ok_lock,
		    receive 
			continue ->
			    ok
		    end,
		    ?log("..continuing~n", []),
		    ok
	    end,
    
    Match = fun() ->  
		    receive 
			continueB -> 
			    ?log("~p, moving on TID ~p~n", 
				 [self(), get(mnesia_activity_state)]),
			    Me ! {self(), continuing}
		    end,
		    case catch mnesia:match_object({ul, {key, {'_', '$0'}}, 
						    '_'}) of
			{'EXIT', What} ->  %% Cyclic first time
			    ?log("Cyclic Restarting ~p ~n", [What]),
			    {should_not_happen,What};
			Res ->
			    ?log("Got match log ~p...~n", [Res]),
			    Res
		    end
	    end,

    B ! fun() -> mnesia:transaction(Match) end,
    timer:sleep(100),  %% Let B be started first..
    A ! fun() -> mnesia:transaction(Write) end,    
    
    receive ok_lock -> ok end,

    B ! continueB,
    ?match_receive({B, continuing}),

    %% B should now be in lock queue.
    A ! continue,
    ?match_multi_receive([{A, {atomic, ok}},
			  {B, {atomic, [{ul,{key,{17,42}},val}]}}]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_table(suite) -> [];
create_table(Config) when is_list(Config) ->
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = c_t_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 1, updated}) end,
    ?match_receive({A, ok}),   %% A is executed
    
    DiskMaybe = mnesia_test_lib:storage_type(disc_copies, Config),

    Pid = spawn_link(?MODULE, op, [self(), mnesia, create_table,
				   [test_tab1, [{DiskMaybe, [ThisNode]}]]]),
    ?match_multi_receive([{Pid, {atomic, ok}},
			  {'EXIT', Pid, normal}]), %% No Locks! op should be exec.

    Pid2 = spawn_link(?MODULE, op, [self(), mnesia, create_table,
				   [test_tab2, [{ram_copies, [Node2]}]]]),

    ?match_multi_receive([{Pid2, {atomic, ok}},
			  {'EXIT', Pid2, normal}]), %% No Locks! op should be exec.

    A ! end_trans,        
    ?match_receive({A,{atomic,end_trans}}),
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

delete_table(suite) -> [];
delete_table(Config) when is_list(Config) ->
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = d_t_tab,
    Def = [{ram_copies, [ThisNode, Node2]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:read({Tab, 1}) end,
    ?match_receive({A, [{Tab, 1, 1, 0}]}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, delete_table,
				   [Tab]]),
    
    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     

    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

move_table_copy(suite) -> [];
move_table_copy(Config) when is_list(Config) ->
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = m_t_c_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 2, 3}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, move_table_copy,
				   [Tab, ThisNode, Node2]]),
    
    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     

    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    timer:sleep(500), %% Don't know how to sync this !!!
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    sys:get_status(whereis(mnesia_tm)),    % Explicit sync, release locks is async
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

add_table_index(suite) -> [];
add_table_index(Config) when is_list(Config) ->
    [ThisNode, _Node2] = ?acquire_nodes(2, Config ++ [{tc_timeout, 60000}]),
    Tab = a_t_i_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 1, updated}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, 
				   add_table_index, [Tab, attr1]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

del_table_index(suite) -> [];
del_table_index(Config) when is_list(Config) ->
    [ThisNode, _Node2] = ?acquire_nodes(2, Config),
    Tab = d_t_i_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    ?match({atomic, ok}, mnesia:add_table_index(Tab, attr1)),    

    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 51, 51, attr2}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, del_table_index,
				   [Tab, attr1]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    %% Locks released! op should be exec.
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

transform_table(suite) -> [];
transform_table(Config) when is_list(Config) -> 
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = t_t_tab,
    Def = [{ram_copies, [ThisNode, Node2]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:read({Tab, 1}) end,
    ?match_receive({A, [{Tab, 1, 1, 0}]}),   %% A is executed

    Transform = fun({Table, Key, Attr1, Attr2}) -> % Need todo a transform
			{Table, Key, {Attr1, Attr2}} end,    
    Pid = spawn_link(?MODULE, op, [self(), mnesia, transform_table,
				   [Tab, Transform, [key, attr1]]]),
    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

snmp_open_table(suite) -> [];
snmp_open_table(Config) when is_list(Config) -> 
    [ThisNode, _Node2] = ?acquire_nodes(2, Config),
    Tab = s_o_t_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 1, 100}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, snmp_open_table,
				   [Tab, [{key, integer}]]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    %% Locks released! op should be exec. Can take a while (thats the timeout)
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

snmp_close_table(suite) -> [];
snmp_close_table(Config) when is_list(Config) -> 
    [ThisNode, _Node2] = ?acquire_nodes(2, Config),
    Tab = s_c_t_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    ?match({atomic, ok}, mnesia:snmp_open_table(Tab, [{key, integer}])),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 1, 100}) end,
    ?match_receive({A, ok}),   %% A is executed    

    Pid = spawn_link(?MODULE, op, [self(), mnesia, snmp_close_table, [Tab]]),   
    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    %% Locks released! op should be exec. Can take a while (thats the timeout)
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

change_table_copy_type(suite) -> [];
change_table_copy_type(Config) when is_list(Config) ->
    [ThisNode, _Node2] = ?acquire_nodes(2, Config),
    Tab = c_t_c_t_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),
    A ! fun() -> mnesia:write({Tab, 1, 1, updated}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, change_table_copy_type, 
				   [Tab, ThisNode, disc_copies]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

change_table_access(suite) -> [];
change_table_access(Config) when is_list(Config) -> 
    [ThisNode, _Node2] = ?acquire_nodes(2, Config),
    Tab = c_t_a_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 1, updated}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, change_table_access_mode,
				   [Tab, read_only]]),

    
    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

add_table_copy(suite) -> [];
add_table_copy(Config) when is_list(Config) -> 
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = a_t_c_tab,
    Def = [{ram_copies, [ThisNode]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]),
    mnesia_test_lib:start_sync_transactions([A], 0),

    A ! fun() -> mnesia:write({Tab, 1, 1, updated}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, add_table_copy,
				   [Tab, Node2, ram_copies]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),

    receive
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    ?match_receive({'EXIT', Pid, normal}),

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)),
    ?match([], mnesia:system_info(lock_queue)),

    {atomic, ok} = mnesia:del_table_copy(Tab, Node2),
    Self = self(),
    New = spawn_link(Node2,
                     fun () ->
                             application:stop(mnesia),
                             Self ! {self(), ok},
                             io:format(user, "restart mnesia~n", []),
                             Self ! {self(), catch application:start(mnesia)}
                     end),
    receive {New,ok} -> ok end,

    Add = fun Add() ->
                  case mnesia:add_table_copy(Tab, Node2, disc_copies) of
                      {atomic, ok} -> ok;
                      _R -> io:format(user, "aborted with reason ~p~n", [_R]),
                            timer:sleep(10),
                            Add()
                  end
          end,

    ?match(ok, Add()),
    ?match_receive({New,ok}),

    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)),
    ?match([], mnesia:system_info(lock_queue)),
    ok.

del_table_copy(suite) -> [];
del_table_copy(Config) when is_list(Config) -> 
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = d_t_c_tab,
    Def = [{ram_copies, [ThisNode, Node2]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),
    A ! fun() -> mnesia:write({Tab, 1, 2, 5}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, del_table_copy,
				   [Tab, ThisNode]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A
    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A, {atomic,end_trans}}),     

    ?match_receive({Pid, {atomic, ok}}),
    ?match_receive({'EXIT', Pid, normal}),

    timer:sleep(500), %% Don't know how to sync this !!!
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    sys:get_status(whereis(mnesia_tm)),    % Explicit sync, release locks is async
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

dump_tables(suite) -> [];
dump_tables(Config) when is_list(Config) -> 
    [ThisNode, Node2] = ?acquire_nodes(2, Config),
    Tab = dump_t_tab,
    Def = [{ram_copies, [ThisNode, Node2]}, {attributes, [key, attr1, attr2]}],
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 50),
    {success, [A]} = ?start_activities([ThisNode]), 
    mnesia_test_lib:start_sync_transactions([A], 0),
    A ! fun() -> mnesia:write({Tab, 1, 1, updated}) end,
    ?match_receive({A, ok}),   %% A is executed

    Pid = spawn_link(?MODULE, op, [self(), mnesia, dump_tables,
				   [[Tab]]]),

    ?match_receive(timeout),   %% op waits for locks occupied by A

    A ! end_trans,             %% Kill A, locks should be released
    ?match_receive({A,{atomic,end_trans}}),     
    
    receive 
	Msg -> ?match({Pid, {atomic, ok}}, Msg)
    after
	timer:seconds(20) -> ?error("Operation timed out", [])
    end,
    
    sys:get_status(whereis(mnesia_locker)), % Explicit sync, release locks is async
    ?match([], mnesia:system_info(held_locks)), 
    ?match([], mnesia:system_info(lock_queue)), 
    ok.

op(Father, Mod, Fun, Args) -> 
    Res = apply(Mod, Fun, Args),
    Father ! {self(), Res}.

insert(_Tab, 0) -> ok;
insert(Tab, N) when N > 0 ->
    ok = mnesia:sync_dirty(fun() -> mnesia:write({Tab, N, N, 0}) end),
    insert(Tab, N-1).


update_own(Tab, Key, Acc) ->
    Update = 
	fun() -> 
		Res = mnesia:read({Tab, Key}),
		case Res of 
		    [{Tab, Key, Extra, Acc}] ->
			mnesia:write({Tab,Key,Extra, Acc+1});
		    Val ->
			{read, Val, {acc, Acc}}
		end
	end,
    receive 
	{Pid, quit} -> Pid ! {self(), Acc}
    after
	0 -> 
	    case mnesia:transaction(Update) of
		{atomic, ok} -> 	    
		    update_own(Tab, Key, Acc+1);
		Else -> 
		    ?error("Trans failed on ~p with ~p~n"
			   "Info w2read ~p w2write ~p w2commit ~p storage ~p ~n", 
			   [node(), 
			    Else,
			    mnesia:table_info(Tab, where_to_read),
			    mnesia:table_info(Tab, where_to_write),
			    mnesia:table_info(Tab, where_to_commit),
			    mnesia:table_info(Tab, storage_type)])
	    end
    end.

update_shared(Tab, Me, Acc) ->
    Update = 
	fun() ->
		W2R = mnesia:table_info(Tab, where_to_read),
		Res = mnesia:read({Tab, 0}),
		case Res of 
		    [{Tab, Key, Extra, Val}] when element(Me, Extra) == Acc ->
			Extra1 = setelement(Me, Extra, Acc+1),
			Term = {Tab, Key, Extra1, Val+1},
			ok = mnesia:write(Term),
%			?log("At ~p: ~p w2r ~p w2w ~p ~n", 
%			     [node(), Term, 
%			      mnesia:table_info(Tab, where_to_read),
			W2W = mnesia:table_info(Tab, where_to_write),
			W2C = mnesia:table_info(Tab, where_to_commit),
%%			      mnesia:table_info(Tab, storage_type)
%			     ]),
			{_Mod, Tid, Ts} = get(mnesia_activity_state),
			io:format("~p ~p~n", [Tid, ets:tab2list(element(2,Ts))]),
			{ok,Term,{W2R,W2W,W2C}};
		    Val ->
			Info = [{acc, Acc}, {me, Me}, 
				{tid, element(2, mnesia:get_activity_id())},
				{locks, mnesia:system_info(held_locks)}],
			{read, Val, Info}
		end
	end,
    receive 
	{Pid, quit} -> Pid ! {self(), Acc}
    after
	0 -> 
	    case mnesia:transaction(Update) of
		{atomic, {ok,Term,W2}} ->
		    io:format("~p:~p:(~p,~p) ~w@~w~n",
			      [erlang:unique_integer([monotonic,positive]),
			       node(),Me,Acc,Term,W2]),
		    update_shared(Tab, Me, Acc+1);
		Else -> 
		    ?error("Trans failed on ~p with ~p~n"
			   "Info w2read ~p w2write ~p w2commit ~p storage ~p ~n", 
			   [node(), 
			    Else,
			    mnesia:table_info(Tab, where_to_read),
			    mnesia:table_info(Tab, where_to_write),
			    mnesia:table_info(Tab, where_to_commit),
			    mnesia:table_info(Tab, storage_type)			   
			   ])
	    end
    end.

init_admin(Def, N1, N2, N3) ->
    Tab = schema_ops,
    ?match({atomic, ok}, mnesia:create_table(Tab, Def)),
    insert(Tab, 1002),
    
    Pid1 = spawn_link(N1, ?MODULE, update_own, [Tab, 1, 0]),
    Pid2 = spawn_link(N2, ?MODULE, update_own, [Tab, 2, 0]),
    Pid3 = spawn_link(N3, ?MODULE, update_own, [Tab, 3, 0]),
    
    ?match({atomic, ok}, 
	   mnesia:transaction(fun() -> mnesia:write({Tab, 0, {0,0,0}, 0}) end)),

    Pid4 = spawn_link(N1, ?MODULE, update_shared, [Tab, 1, 0]),
    Pid5 = spawn_link(N2, ?MODULE, update_shared, [Tab, 2, 0]),
    Pid6 = spawn_link(N3, ?MODULE, update_shared, [Tab, 3, 0]),
    
    {Pid1, Pid2, Pid3, Pid4, Pid5, Pid6}.

verify_results({P1, P2, P3, P4, P5, P6}) ->
    Tab = schema_ops, N1 = node(P1),  N2 = node(P2),  N3 = node(P3),

    try 
	P1 ! {self(), quit}, 
	R1 = receive {P1, Res1} -> Res1 after 9000 -> throw({timeout,P1}) end,
	P2 ! {self(), quit}, 
	R2 = receive {P2, Res2} -> Res2 after 9000 -> throw({timeout,P2}) end,
	P3 ! {self(), quit}, 
	R3 = receive {P3, Res3} -> Res3 after 9000 -> throw({timeout,P3}) end,

	P4 ! {self(), quit}, 
	R4 = receive {P4, Res4} -> Res4 after 9000 -> throw({timeout,P4}) end,
	P5 ! {self(), quit}, 
	R5 = receive {P5, Res5} -> Res5 after 9000 -> throw({timeout,P5}) end,
	P6 ! {self(), quit}, 
	R6 = receive {P6, Res6} -> Res6 after 9000 -> throw({timeout,P6}) end,

	?match({atomic, ok}, mnesia:transaction(fun() -> mnesia:write_lock_table(Tab) end)),
	?log("Should be ~p~n", [R1]),
	?match([{_, _, _, R1}], rpc:call(N1, mnesia, dirty_read, [{Tab, 1}])),
	?match([{_, _, _, R1}], rpc:call(N2, mnesia, dirty_read, [{Tab, 1}])),
	?match([{_, _, _, R1}], rpc:call(N3, mnesia, dirty_read, [{Tab, 1}])),
	?log("Should be ~p~n", [R2]),
	?match([{_, _, _, R2}], rpc:call(N1, mnesia, dirty_read, [{Tab, 2}])),
	?match([{_, _, _, R2}], rpc:call(N2, mnesia, dirty_read, [{Tab, 2}])),
	?match([{_, _, _, R2}], rpc:call(N3, mnesia, dirty_read, [{Tab, 2}])),
	?log("Should be ~p~n", [R3]),
	?match([{_, _, _, R3}], rpc:call(N1, mnesia, dirty_read, [{Tab, 3}])),
	?match([{_, _, _, R3}], rpc:call(N2, mnesia, dirty_read, [{Tab, 3}])),
	?match([{_, _, _, R3}], rpc:call(N3, mnesia, dirty_read, [{Tab, 3}])),

	Res = R4+R5+R6,
	?log("Should be {~p+~p+~p}= ~p~n", [R4, R5, R6, Res]),
	?match([{_, _, {R4,R5,R6}, Res}], rpc:call(N1, mnesia, dirty_read, [{Tab, 0}])),
	?match([{_, _, {R4,R5,R6}, Res}], rpc:call(N2, mnesia, dirty_read, [{Tab, 0}])),
	?match([{_, _, {R4,R5,R6}, Res}], rpc:call(N3, mnesia, dirty_read, [{Tab, 0}]))
    catch throw:{timeout, Pid}  ->
	    mnesia_lib:dist_coredump(),
	    ?error("Timeout ~p ~n", [Pid])
    end.
    

get_info(Tab) ->
    Info = mnesia:table_info(Tab, all),
    mnesia_lib:verbose("~p~n", [Info]).

del_table_copy_1(suite) -> [];
del_table_copy_1(Config) when is_list(Config) ->
    [_Node1, Node2, _Node3] = Nodes = ?acquire_nodes(3, Config),
    del_table(Node2, Node2, Nodes). %Called on same Node as deleted
del_table_copy_2(suite) -> [];
del_table_copy_2(Config) when is_list(Config) ->
    [Node1, Node2, _Node3] = Nodes = ?acquire_nodes(3, Config),
    del_table(Node1, Node2, Nodes). %Called from other Node 
del_table_copy_3(suite) -> [];
del_table_copy_3(Config) when is_list(Config) ->
    [_Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    del_table(Node3, Node2, Nodes). %Called from Node w.o. table

%%% The actual test
del_table(CallFrom, DelNode, [Node1, Node2, Node3]) ->
    Def = [{ram_copies, [Node1]}, {disc_copies, [Node2]}, 
	   {attributes, [key, attr1, attr2]}],
    Tab = schema_ops,
    Pids = init_admin(Def, Node1, Node2, Node3),

    ?log("Call from ~p delete table from ~p ~n", [CallFrom, DelNode]),
    rpc:multicall([Node1, Node2, Node3], ?MODULE, get_info, [Tab]),
    
    ?match({atomic, ok}, 
	   rpc:call(CallFrom, mnesia, del_table_copy, [Tab, DelNode])),

    verify_results(Pids),
    rpc:multicall([Node1, Node2, Node3], ?MODULE, get_info, [Tab]),
    ?verify_mnesia([Node1, Node2, Node3], []).    

add_table_copy_1(suite) -> [];
add_table_copy_1(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_only_copies, [Node1, Node2]}, 
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node1, Node3, Nodes, Def).
add_table_copy_2(suite) -> [];
add_table_copy_2(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_only_copies, [Node1, Node2]}, 
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node2, Node3, Nodes, Def).
add_table_copy_3(suite) -> [];
add_table_copy_3(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_only_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node3, Node3, Nodes, Def).
add_table_copy_4(suite) -> [];
add_table_copy_4(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_only_copies, [Node1]}, 
	   {attributes, [key, attr1, attr2]}], 
    add_table(Node2, Node3, Nodes, Def).
%%% The actual test
add_table(CallFrom, AddNode, [Node1, Node2, Node3], Def) ->
    Pids = init_admin(Def, Node1, Node2, Node3),    
    Tab = schema_ops,
    ?log("Call from ~p add table to ~p ~n", [CallFrom, AddNode]),
    rpc:multicall([Node1, Node2, Node3], ?MODULE, get_info, [Tab]),
    ?match({atomic, ok}, rpc:call(CallFrom, mnesia, add_table_copy, 
				  [Tab, AddNode, ram_copies])),
    verify_results(Pids),
    rpc:multicall([Node1, Node2, Node3], ?MODULE, get_info, [Tab]),
    ?verify_mnesia([Node1, Node2, Node3], []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_table_copy_1(suite) -> [];
move_table_copy_1(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node1, Node1, Node3, Nodes, Def).
move_table_copy_2(suite) -> [];
move_table_copy_2(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node2, Node1, Node3, Nodes, Def).
move_table_copy_3(suite) -> [];
move_table_copy_3(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_copies, [Node1, Node2]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node3, Node1, Node3, Nodes, Def).
move_table_copy_4(suite) -> [];
move_table_copy_4(Config) when is_list(Config) ->
    [Node1, Node2, Node3] = Nodes = ?acquire_nodes(3, Config),
    Def = [{disc_copies, [Node1]},
	   {attributes, [key, attr1, attr2]}], 
    move_table(Node2, Node1, Node3, Nodes, Def).
%%% The actual test
move_table(CallFrom, FromNode, ToNode, [Node1, Node2, Node3], Def) ->
    Pids = init_admin(Def, Node1, Node2, Node3),    
    Tab = schema_ops,   
    ?log("Call from ~p move table from ~p to ~p ~n", [CallFrom, FromNode, ToNode]),
    rpc:multicall([Node1, Node2, Node3], ?MODULE, get_info, [Tab]),
    ?match({atomic, ok}, rpc:call(CallFrom, mnesia, move_table_copy, 
				  [Tab, FromNode, ToNode])),
    verify_results(Pids),
    rpc:multicall([Node1, Node2, Node3], ?MODULE, get_info, [Tab]),
    ?verify_mnesia([Node1, Node2, Node3], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dirty_updates_visible_direct(doc) ->
    ["One process can immediately see dirty updates of another"];
dirty_updates_visible_direct(suite) -> [];
dirty_updates_visible_direct(Config) when is_list(Config) ->
    dirty_visibility(outside_trans, Config).

dirty_reads_regardless_of_trans(doc) ->
    ["Dirty reads are not affected by transaction context"];
dirty_reads_regardless_of_trans(suite) -> [];
dirty_reads_regardless_of_trans(Config) when is_list(Config) ->
    dirty_visibility(inside_trans, Config).

dirty_visibility(Mode, Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = list_to_atom(lists:concat([dirty_visibility, '_', Mode])), 

   ?match({atomic, ok},  mnesia:create_table([{name, Tab}, {ram_copies, [Node1]}])), 
    ValPos = 3, 

    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)), 
    
    %% Start two processes
    {success, [A]} = ?start_activities([Node1]), 

    case Mode of
	inside_trans ->
	    ?start_transactions([A]), 
	    A ! fun() ->
			mnesia:write({Tab, a, 11}), 
			mnesia:write({Tab, b, 22}), 
			mnesia:write({Tab, c, 1}), 
			mnesia:write({Tab, d, 2}), 
			mnesia:write({Tab, e, 3}), 
			lists:sort(mnesia:all_keys(Tab))
		end, 
	    ?match_receive({A, [a, b, c, d, e]});
	outside_trans ->
	    ignore
    end, 

    RecA = {Tab, a, 1}, 
    PatA = {Tab, '$1', 1}, 
    RecB = {Tab, b, 3}, 
    PatB = {Tab, '$1', 3}, 
    RecB2 = {Tab, b, 2}, 
    PatB2 = {Tab, '$1', 2}, 
    ?match([], mnesia:dirty_read({Tab, a})), 
    ?match([], mnesia:dirty_read({Tab, b})), 
    ?match([], mnesia:dirty_match_object(PatA)), 
    ?match([], mnesia:dirty_match_object(PatB)), 
    ?match([], mnesia:dirty_match_object(PatB2)), 
    ?match([], mnesia:dirty_index_read(Tab, 1, ValPos)), 
    ?match([], mnesia:dirty_index_read(Tab, 3, ValPos)), 
    ?match([], mnesia:dirty_index_match_object(PatA, ValPos)), 
    ?match([], mnesia:dirty_index_match_object(PatB, ValPos)), 
    ?match([], mnesia:dirty_index_match_object(PatB2, ValPos)), 
    ?match('$end_of_table', mnesia:dirty_first(Tab)), 

    %% dirty_write
    A ! fun() -> mnesia:dirty_write(RecA) end, 
    ?match_receive({A, ok}), 
    ?match([RecA], mnesia:dirty_read({Tab, a})), 
    ?match([RecA], mnesia:dirty_match_object(PatA)), 
    ?match(a, mnesia:dirty_first(Tab)), 
    ?match([RecA], mnesia:dirty_index_read(Tab, 1, ValPos)), 
    ?match([RecA], mnesia:dirty_index_match_object(PatA, ValPos)), 
    ?match('$end_of_table', mnesia:dirty_next(Tab, a)), 

    %% dirty_create
    A ! fun() -> mnesia:dirty_write(RecB) end, 
    ?match_receive({A, ok}), 
    ?match([RecB], mnesia:dirty_read({Tab, b})), 
    ?match([RecB], mnesia:dirty_match_object(PatB)), 
    ?match([RecB], mnesia:dirty_index_read(Tab, 3, ValPos)), 
    ?match([RecB], mnesia:dirty_index_match_object(PatB, ValPos)), 
    ?match('$end_of_table', 
	   mnesia:dirty_next(Tab, mnesia:dirty_next(Tab, mnesia:dirty_first(Tab)))), 
    
    %% dirty_update_counter
    A ! fun() -> mnesia:dirty_update_counter({Tab, b}, -1) end, 
    ?match_receive({A, _}), 
    ?match([RecB2], mnesia:dirty_read({Tab, b})), 
    ?match([], mnesia:dirty_match_object(PatB)), 
    ?match([RecB2], mnesia:dirty_match_object(PatB2)), 
    ?match([RecB2], mnesia:dirty_index_read(Tab, 2, ValPos)), 
    ?match([], mnesia:dirty_index_match_object(PatB, ValPos)), 
    ?match([RecB2], mnesia:dirty_index_match_object(PatB2, ValPos)), 
    ?match('$end_of_table', 
	   mnesia:dirty_next(Tab, mnesia:dirty_next(Tab, mnesia:dirty_first(Tab)))), 

    %% dirty_delete
    A ! fun() -> mnesia:dirty_delete({Tab, b}) end, 
    ?match_receive({A, ok}), 
    ?match([], mnesia:dirty_read({Tab, b})), 
    ?match([], mnesia:dirty_match_object(PatB2)), 
    ?match([], mnesia:dirty_index_read(Tab, 3, ValPos)), 
    ?match([], mnesia:dirty_index_match_object(PatB2, ValPos)), 
    ?match(a, mnesia:dirty_first(Tab)), 
    ?match('$end_of_table', mnesia:dirty_next(Tab, a)), 

    %% dirty_delete_object
    ?match([RecA], mnesia:dirty_match_object(PatA)), 
    A ! fun() -> mnesia:dirty_delete_object(RecA) end, 
    ?match_receive({A, ok}), 
    ?match([], mnesia:dirty_read({Tab, a})), 
    ?match([], mnesia:dirty_match_object(PatA)), 
    ?match([], mnesia:dirty_index_read(Tab, 1, ValPos)), 
    ?match([], mnesia:dirty_index_match_object(PatA, ValPos)), 
    ?match('$end_of_table', mnesia:dirty_first(Tab)), 
    
    case Mode of
	inside_trans ->
	    A ! end_trans, 
	    ?match_receive({A, {atomic, end_trans}});
	outside_trans ->
	    ignore
    end, 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans_update_invisibible_outside_trans(doc) ->
    ["Updates in a transaction are invisible outside the transaction"];
trans_update_invisibible_outside_trans(suite) -> [];
trans_update_invisibible_outside_trans(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = trans_update_invisibible_outside_trans, 

    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					     {ram_copies, [Node1]}])), 
    ValPos = 3, 
    RecA = {Tab, a, 1}, 
    PatA = {Tab, '$1', 1}, 
    RecB = {Tab, b, 3}, 
    PatB = {Tab, '$1', 3}, 
    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)), 

    Verify =
	fun() ->
		?match([], mnesia:dirty_read({Tab, a})), 
		?match([], mnesia:dirty_read({Tab, b})), 
		?match([], mnesia:dirty_match_object(PatA)), 
		?match([], mnesia:dirty_match_object(PatB)), 
		?match([], mnesia:dirty_index_read(Tab, 1, ValPos)), 
		?match([], mnesia:dirty_index_read(Tab, 3, ValPos)), 
		?match([], mnesia:dirty_index_match_object(PatA, ValPos)), 
		?match([], mnesia:dirty_index_match_object(PatB, ValPos)), 
		?match('$end_of_table', mnesia:dirty_first(Tab))
	     end, 

    Fun = fun() ->
		  ?match(ok, mnesia:write(RecA)), 
		  Verify(), 

		  ?match(ok, mnesia:write(RecB)), 
		  Verify(), 

		  ?match(ok, mnesia:delete({Tab, b})), 
		  Verify(), 
		  
		  ?match([RecA], mnesia:match_object(PatA)), 
		  Verify(), 

		  ?match(ok, mnesia:delete_object(RecA)), 
		  Verify(), 
		  ok
	  end, 
    ?match({atomic, ok}, mnesia:transaction(Fun)), 
    Verify(), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans_update_visible_inside_trans(doc) ->
    ["Updates in a transaction are visible in the same transaction"];
trans_update_visible_inside_trans(suite) -> [];
trans_update_visible_inside_trans(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = trans_update_visible_inside_trans, 

    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					     {ram_copies, [Node1]}])), 
    ValPos = 3, 
    RecA = {Tab, a, 1},
    RecA2 = {Tab, a, 2},
    PatA = {Tab, '$1', 1}, 
    RecB = {Tab, b, 3}, 
    PatB = {Tab, '$1', 3}, 
    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)), 

    Fun = fun() ->
		  %% write
		  ?match(ok, mnesia:write(RecA)), 
		  ?match([RecA], mnesia:read({Tab, a})), 
		  ?match([RecA], mnesia:wread({Tab, a})), 
		  ?match([RecA], mnesia:match_object(PatA)), 
		  ?match([a], mnesia:all_keys(Tab)), 
		  ?match([RecA], mnesia:index_match_object(PatA, ValPos)), 
		  ?match([RecA], mnesia:index_read(Tab, 1, ValPos)), 

		  %% create
		  ?match(ok, mnesia:write(RecB)), 
		  ?match([RecB], mnesia:read({Tab, b})), 
		  ?match([RecB], mnesia:wread({Tab, b})), 
		  ?match([RecB], mnesia:match_object(PatB)), 
		  ?match([RecB], mnesia:index_match_object(PatB, ValPos)), 
		  ?match([RecB], mnesia:index_read(Tab, 3, ValPos)), 
		  
		  %% delete
		  ?match(ok, mnesia:delete({Tab, b})), 
		  ?match([], mnesia:read({Tab, b})), 
		  ?match([], mnesia:wread({Tab, b})), 
		  ?match([], mnesia:match_object(PatB)), 
		  ?match([a], mnesia:all_keys(Tab)), 
		  ?match([], mnesia:index_match_object(PatB, ValPos)), 
		  ?match([], mnesia:index_read(Tab, 2, ValPos)), 
		  ?match([], mnesia:index_read(Tab, 3, ValPos)), 

		  %% delete_object
		  ?match(ok, mnesia:delete_object(RecA2)),
		  ?match([RecA], mnesia:read({Tab, a})),
		  ?match([RecA], mnesia:wread({Tab, a})),
		  ?match([RecA], mnesia:match_object(PatA)),
		  ?match([a], mnesia:all_keys(Tab)),
		  ?match([RecA], mnesia:index_match_object(PatA, ValPos)),
		  ?match([RecA], mnesia:index_read(Tab, 1, ValPos)),

		  ?match(ok, mnesia:delete_object(RecA)), 
		  ?match([], mnesia:read({Tab, a})), 
		  ?match([], mnesia:wread({Tab, a})), 
		  ?match([], mnesia:match_object(PatA)), 
		  ?match([], mnesia:all_keys(Tab)), 
		  ?match([], mnesia:index_match_object(PatA, ValPos)), 
		  ?match([], mnesia:index_read(Tab, 2, ValPos)), 
		  ?match([], mnesia:index_read(Tab, 3, ValPos)), 
		  ok
	  end, 
    ?match({atomic, ok}, mnesia:transaction(Fun)), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_shadows(doc) ->
    ["Tests whether the shadow shows the correct object when",
     "writing to the table"];
write_shadows(suite) -> [];
write_shadows(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = write_shadows, 

    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					     {ram_copies, [Node1]}, 
					     {type,  set}])), 
    ValPos = 3, 
    RecA1 = {Tab, a, 1}, 
    PatA1 = {Tab, '$1', 1}, 
    RecA2 = {Tab, a, 2}, 
    PatA2 = {Tab, '$1', 2}, 


    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)), 
       
    Fun1 = fun() ->
		   ?match(ok, mnesia:write(RecA1)), 
		   ok
	   end, 

    ?match({atomic, ok}, mnesia:transaction(Fun1)), 

    Fun2 = fun() ->
		  %% write shadow old write - is the confirmed value visable
		  %%                          in the shadow ?
		  ?match([RecA1], mnesia:read({Tab, a})), 
		  ?match([RecA1], mnesia:wread({Tab, a})), 
		  ?match([RecA1], mnesia:match_object(PatA1)), 
		  ?match([a], mnesia:all_keys(Tab)), 
		  ?match([RecA1], mnesia:index_match_object(PatA1, ValPos)), 
		  ?match([RecA1], mnesia:index_read(Tab, 1, ValPos)), 

		  %% write shadow new write - is a new value visable instead
		  %%                          of the old value ?
		  ?match(ok, mnesia:write(RecA2)), 

		  ?match([RecA2], mnesia:read({Tab, a})), 
		  ?match([RecA2], mnesia:wread({Tab, a})), 
		   ?match([],      mnesia:match_object(PatA1)), %% delete shadow old but not new write
		   ?match([RecA2], mnesia:match_object(PatA2)), %% is the new value visable

		  ?match([a], mnesia:all_keys(Tab)), 
		  ?match([RecA2], mnesia:index_match_object(PatA2, ValPos)), 
		  ?match([RecA2], mnesia:index_read(Tab, 2, ValPos)), 
		  ok

	  end, 
    ?match({atomic, ok}, mnesia:transaction(Fun2)), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_shadows(doc) ->
    ["Test whether the shadow shows the correct object when deleting objects"];
delete_shadows(suite) -> [];
delete_shadows(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = delete_shadows, 

    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					     {ram_copies, [Node1]}, 
					     {type,  set}])), 
    ValPos = 3, 
    OidA  = {Tab, a}, 
    RecA1 = {Tab, a, 1}, 
    PatA1 = {Tab, '$1', 1}, 
    RecA2 = {Tab, a, 2}, 
    PatA2 = {Tab, '$1', 2}, 


    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)), 
       
    Fun1 = fun() ->
		   ?match(ok, mnesia:write(RecA1)), 
		   ok
	   end, 

    ?match({atomic, ok}, mnesia:transaction(Fun1)), 

    Fun2 = fun() ->
		   
		   
		  %% delete shadow old write - is the confirmed value invisible
		  %%                           when deleted in the transaction ?
		  ?match(ok, mnesia:delete(OidA)), 

		  ?match([], mnesia:read({Tab, a})), 
		  ?match([], mnesia:wread({Tab, a})), 
		  ?match([], mnesia:match_object(PatA1)), 
		  ?match([], mnesia:all_keys(Tab)), 
		  ?match([], mnesia:index_match_object(PatA1, ValPos)), 
		  ?match([], mnesia:index_read(Tab, 1, ValPos)), 

		  %% delete shadow old but not new write - is the new value visable
		  %%                           when the old one was deleted ?
		  ?match(ok, mnesia:write(RecA2)), 

		  ?match([RecA2], mnesia:read({Tab, a})), 
		  ?match([RecA2], mnesia:wread({Tab, a})), 
		  ?match([],  mnesia:match_object(PatA1)),
		  ?match([RecA2], mnesia:match_object(PatA2)), 
		  ?match([a], mnesia:all_keys(Tab)), 
		  ?match([RecA2], mnesia:index_match_object(PatA2, ValPos)), 
		  ?match([RecA2], mnesia:index_read(Tab, 2, ValPos)), 
	
		   %% delete shadow old and new write - is the new value invisable
		  %%                           when deleted ?
		  ?match(ok, mnesia:delete(OidA)), 

		  ?match([], mnesia:read({Tab, a})), 
		  ?match([], mnesia:wread({Tab, a})), 
		  ?match([], mnesia:match_object(PatA2)), 
		  ?match([], mnesia:all_keys(Tab)), 
		  ?match([], mnesia:index_match_object(PatA2, ValPos)), 
		  ?match([], mnesia:index_read(Tab, 2, ValPos)), 
		  ok

	  end, 
    ?match({atomic, ok}, mnesia:transaction(Fun2)), 
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_delete_shadows_bag(doc) ->
    ["Test the visibility of written and deleted objects in an bag type table"];
write_delete_shadows_bag(suite) -> [];
write_delete_shadows_bag(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab = write_delete_shadows_bag, 

    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					       {ram_copies, [Node1]}, 
					       {type,  bag}])), 
    ValPos = 3, 
    OidA  = {Tab, a}, 

    RecA1 = {Tab, a, 1}, 
    PatA1 = {Tab, '$1', 1}, 

    RecA2 = {Tab, a, 2}, 
    PatA2 = {Tab, '$1', 2}, 

    RecA3 = {Tab, a, 3}, 
    PatA3 = {Tab, '$1', 3}, 

    PatA  = {Tab, a, '_'}, 


    ?match({atomic, ok},  mnesia:add_table_index(Tab, ValPos)), 

    Fun1 = fun() ->
		   ?match(ok, mnesia:write(RecA1)), 
		   ?match(ok, mnesia:write(RecA2)), 
		   ok
	   end, 

    ?match({atomic, ok}, mnesia:transaction(Fun1)), 

    Fun2 = fun() ->		   		   
		   %% delete shadow old write - is the confirmed value invisible
		   %%                           when deleted in the transaction ?
		   ?match(ok, mnesia:delete_object(RecA1)), 

		   ?match([RecA2], mnesia:read({Tab, a})), 
		   ?match([RecA2], mnesia:wread({Tab, a})), 
		   ?match([RecA2], mnesia:match_object(PatA2)), 
		   ?match([a], mnesia:all_keys(Tab)), 
		   ?match([RecA2], mnesia:index_match_object(PatA2, ValPos)), 
		   ?match([RecA2], mnesia:index_read(Tab, 2, ValPos)), 

		   ?match(ok, mnesia:delete(OidA)), 

		   ?match([], mnesia:read({Tab, a})), 
		   ?match([], mnesia:wread({Tab, a})), 
		   ?match([], mnesia:match_object(PatA1)), 
		   ?match([], mnesia:all_keys(Tab)), 
		   ?match([], mnesia:index_match_object(PatA1, ValPos)), 
		   ?match([], mnesia:index_read(Tab, 1, ValPos)), 

		   %% delete shadow old but not new write - are both new value visable
		   %%                           when the old one was deleted ?
		   ?match(ok, mnesia:write(RecA2)), 
		   ?match(ok, mnesia:write(RecA3)), 


		   ?match([RecA2, RecA3], lists:sort(mnesia:read({Tab, a}))), 
		   ?match([RecA2, RecA3], lists:sort(mnesia:wread({Tab, a}))), 
		   ?match([RecA2], mnesia:match_object(PatA2)), 
		   ?match([a], mnesia:all_keys(Tab)), 
		   ?match([RecA2, RecA3], lists:sort(mnesia:match_object(PatA))),
		   ?match([RecA2], mnesia:index_match_object(PatA2, ValPos)),
		   ?match([RecA3], mnesia:index_match_object(PatA3, ValPos)),
		   ?match([RecA2], mnesia:index_read(Tab, 2, ValPos)), 

		   %% delete shadow old and new write - is the new value invisable
		   %%                           when deleted ?
		   ?match(ok, mnesia:delete(OidA)), 

		   ?match([], mnesia:read({Tab, a})), 
		   ?match([], mnesia:wread({Tab, a})), 
		   ?match([], mnesia:match_object(PatA2)), 
		   ?match([], mnesia:all_keys(Tab)), 
		   ?match([], mnesia:index_match_object(PatA2, ValPos)), 
		   ?match([], mnesia:index_read(Tab, 2, ValPos)), 
		   ok
	   end, 
    ?match({atomic, ok}, mnesia:transaction(Fun2)), 
    ok.

write_delete_shadows_bag2(doc) ->
    ["Test the visibility of written and deleted objects in an bag type table " 
     "and verifies the results"];
write_delete_shadows_bag2(suite) -> [];
write_delete_shadows_bag2(Config) when is_list(Config) ->

    [Node1] = ?acquire_nodes(1, Config), 
    Tab = w_d_s_b, 

    ?match({atomic, ok},  mnesia:create_table([{name, Tab}, 
					       {ram_copies, [Node1]}, 
					       {type,  bag}])),     
    Del = fun() ->
		  R1 = mnesia:read({Tab, 1}),
		  mnesia:delete({Tab, 1}),
		  R2 = mnesia:read({Tab, 1}),
		  mnesia:write({Tab, 1, 1}),
		  mnesia:write({Tab, 1, 2}),
		  R3 = mnesia:read({Tab, 1}),
		  {R1, R2, R3}
	  end,
    DelObj = fun() ->
		     R1 = mnesia:read({Tab, 2}),
		     mnesia:delete_object({Tab, 2, 2}),
		     R2 = mnesia:read({Tab, 2}),
		     mnesia:write({Tab, 2, 1}),
		     mnesia:write({Tab, 2, 2}),
		     R3 = mnesia:read({Tab, 2}),
		     {R1, R2, R3}
	     end,
    Both1 = [{Tab, 1, 1}, {Tab, 1, 2}],   
    Both2 = [{Tab, 2, 1}, {Tab, 2, 2}],   
    ?match({atomic, {[], [], Both1}}, mnesia:transaction(Del)),
    ?match({atomic, {Both1, [], Both1}}, mnesia:transaction(Del)),
    ?match({atomic, Both1}, mnesia:transaction(fun() -> mnesia:read({Tab, 1}) end)),
    ?match({atomic, {[], [], Both2}}, mnesia:transaction(DelObj)),
    ?match({atomic, {Both2, [{Tab, 2, 1}], Both2}}, mnesia:transaction(DelObj)),
    ?match({atomic, Both2}, mnesia:transaction(fun() -> mnesia:read({Tab, 2}) end)),
    ?verify_mnesia([Node1], []).

shadow_search(doc) ->
    ["Verifies that ordered_set tables are ordered, and the order is kept" 
     "even when table is shadowed by transaction updates"];
shadow_search(suite) -> [];
shadow_search(Config) when is_list(Config) ->
    [Node1] = ?acquire_nodes(1, Config), 
    Tab1  = ss_oset,
    Tab2 = ss_set,
    Tab3 = ss_bag,
    Tabs = [Tab1,Tab2,Tab3],
    RecName = ss,
    ?match({atomic, ok},  mnesia:create_table([{name, Tab1}, 
					       {ram_copies, [Node1]}, 
					       {record_name, RecName},
					       {type,  ordered_set}])),
    ?match({atomic, ok},  mnesia:create_table([{name, Tab2},
					       {record_name, RecName},
					       {ram_copies, [Node1]},
					       {type,  set}])),
    ?match({atomic, ok},  mnesia:create_table([{name, Tab3},
					       {record_name, RecName},
					       {ram_copies, [Node1]}, 
					       {type,  bag}])),
    Recs = [{RecName, K, K} || K <- [1,3,5]],
    [mnesia:dirty_write(Tab1, R) || R <- Recs],
    [mnesia:dirty_write(Tab2, R) || R <- Recs],
    [mnesia:dirty_write(Tab3, R) || R <- Recs],
    
    Match  = fun(Tab) -> mnesia:match_object(Tab, {'_','_','_'}, write) end,
    Select = fun(Tab) -> mnesia:select(Tab, [{'_', [], ['$_']}]) end,
%    Trans = fun(Fun,Args) -> mnesia:transaction(Fun,Args) end,
    LoopHelp = fun('$end_of_table',_) -> [];
		  ({Res,Cont},Fun) -> 
		       Sel = mnesia:select(Cont),
		       Res ++ Fun(Sel, Fun)
	       end,
    SelLoop = fun(Table) -> 
		      Sel = mnesia:select(Table, [{'_', [], ['$_']}], 1, read),
		      LoopHelp(Sel,LoopHelp)
	      end,
    
    R1 = {RecName, 2, 2}, R2 = {RecName, 4, 4},
    R3 = {RecName, 2, 3}, R4 = {RecName, 3, 1},
    R5 = {RecName, 104, 104},
    W1 = fun(Tab,Search) -> mnesia:write(Tab,R1,write),
			    mnesia:write(Tab,R2,write), 
			    Search(Tab)
	 end,
    S1 = lists:sort([R1,R2|Recs]),
    ?match({atomic,S1}, mnesia:transaction(W1, [Tab1,Select])),
    ?match({atomic,S1}, mnesia:transaction(W1, [Tab1,Match])),
    ?match({atomic,S1}, mnesia:transaction(W1, [Tab1,SelLoop])),
    ?match({atomic,S1}, sort_res(mnesia:transaction(W1, [Tab2,Select]))),
    ?match({atomic,S1}, sort_res(mnesia:transaction(W1, [Tab2,SelLoop]))),
    ?match({atomic,S1}, sort_res(mnesia:transaction(W1, [Tab2,Match]))),
    ?match({atomic,S1}, sort_res(mnesia:transaction(W1, [Tab3,Select]))),
    ?match({atomic,S1}, sort_res(mnesia:transaction(W1, [Tab3,SelLoop]))),
    ?match({atomic,S1}, sort_res(mnesia:transaction(W1, [Tab3,Match]))),
    [mnesia:dirty_delete_object(Tab,R) || R <- [R1,R2], Tab <- Tabs],

    W2 = fun(Tab,Search) -> 
		 mnesia:write(Tab,R3,write),
		 mnesia:write(Tab,R1,write), 
		 Search(Tab)
	 end,
    S2 = lists:sort([R1|Recs]),
    S2Bag = lists:sort([R1,R3|Recs]),
    ?match({atomic,S2}, mnesia:transaction(W2, [Tab1,Select])),
    ?match({atomic,S2}, mnesia:transaction(W2, [Tab1,SelLoop])),
    ?match({atomic,S2}, mnesia:transaction(W2, [Tab1,Match])),
    ?match({atomic,S2}, sort_res(mnesia:transaction(W2, [Tab2,Select]))),
    ?match({atomic,S2}, sort_res(mnesia:transaction(W2, [Tab2,SelLoop]))),
    ?match({atomic,S2}, sort_res(mnesia:transaction(W2, [Tab2,Match]))),
    ?match({atomic,S2Bag}, sort_res(mnesia:transaction(W2, [Tab3,Select]))),
    ?match({atomic,S2Bag}, sort_res(mnesia:transaction(W2, [Tab3,SelLoop]))),
    ?match({atomic,S2Bag}, sort_res(mnesia:transaction(W2, [Tab3,Match]))),
%%    [mnesia:dirty_delete_object(Tab,R) || R <- [R1,R3], Tab <- Tabs],

    W3 = fun(Tab,Search) -> 
		 mnesia:write(Tab,R4,write),
		 mnesia:delete(Tab,element(2,R1),write), 
		 Search(Tab)
	 end,
    S3Bag = lists:sort([R4|lists:delete(R1,Recs)]),
    S3 = lists:delete({RecName,3,3},S3Bag),
    ?match({atomic,S3}, mnesia:transaction(W3, [Tab1,Select])),
    ?match({atomic,S3}, mnesia:transaction(W3, [Tab1,SelLoop])),
    ?match({atomic,S3}, mnesia:transaction(W3, [Tab1,Match])),
    ?match({atomic,S3}, sort_res(mnesia:transaction(W3, [Tab2,SelLoop]))),
    ?match({atomic,S3}, sort_res(mnesia:transaction(W3, [Tab2,Select]))),
    ?match({atomic,S3}, sort_res(mnesia:transaction(W3, [Tab2,Match]))),
    ?match({atomic,S3Bag}, sort_res(mnesia:transaction(W3, [Tab3,Select]))),
    ?match({atomic,S3Bag}, sort_res(mnesia:transaction(W3, [Tab3,SelLoop]))),
    ?match({atomic,S3Bag}, sort_res(mnesia:transaction(W3, [Tab3,Match]))),

    W4 = fun(Tab,Search) -> 
		 mnesia:delete(Tab,-1,write),
		 mnesia:delete(Tab,4 ,write),
		 mnesia:delete(Tab,17,write),
		 mnesia:delete_object(Tab,{RecName, -1, x},write),
		 mnesia:delete_object(Tab,{RecName, 4, x},write),
		 mnesia:delete_object(Tab,{RecName, 42, x},write),
		 mnesia:delete_object(Tab,R2,write),
		 mnesia:write(Tab, R5, write),
		 Search(Tab)
	 end,
    S4Bag = lists:sort([R5|S3Bag]),
    S4    = lists:sort([R5|S3]),
    ?match({atomic,S4}, mnesia:transaction(W4, [Tab1,Select])),
    ?match({atomic,S4}, mnesia:transaction(W4, [Tab1,SelLoop])),
    ?match({atomic,S4}, mnesia:transaction(W4, [Tab1,Match])),
    ?match({atomic,S4}, sort_res(mnesia:transaction(W4, [Tab2,Select]))),
    ?match({atomic,S4}, sort_res(mnesia:transaction(W4, [Tab2,SelLoop]))),
    ?match({atomic,S4}, sort_res(mnesia:transaction(W4, [Tab2,Match]))),
    ?match({atomic,S4Bag}, sort_res(mnesia:transaction(W4, [Tab3,Select]))),
    ?match({atomic,S4Bag}, sort_res(mnesia:transaction(W4, [Tab3,SelLoop]))),
    ?match({atomic,S4Bag}, sort_res(mnesia:transaction(W4, [Tab3,Match]))),
    [mnesia:dirty_delete_object(Tab,R) || R <- [{RecName,3,3},R5], Tab <- Tabs],
        
    %% hmmm anything more??
    
    ?verify_mnesia([Node1], []).


rr_kill_copy(suite) -> [];
rr_kill_copy(Config) when is_list(Config) ->
    Ns = ?acquire_nodes(3,Config ++ [{tc_timeout, 60000}]),
    DeleteMe = fun(_Tab,Where2read) ->
		       ?match([], mnesia_test_lib:kill_mnesia([Where2read]))
	       end,
    Del = removed_resources(Ns, DeleteMe),
    ?verify_mnesia(Ns -- [Del], []).

removed_resources([_N1,N2,N3], DeleteRes) ->
    Tab = del_res,
    ?match({atomic, ok}, mnesia:create_table(Tab,[{ram_copies, [N2,N3]}])),
    
    Init = fun() -> [mnesia:write({Tab,Key,Key}) || Key <- lists:seq(0,99)] end,
    ?match([], [Bad || Bad <- mnesia:sync_dirty(Init), Bad /= ok]),
    
    Where2Read = mnesia:table_info(Tab, where_to_read),
    [Keep]  = [N2,N3] -- [Where2Read],
    Tester = self(),
    
    Conflict = fun() -> 
		       %% Read a value..
		       [{Tab,1,Val}] = mnesia:read({Tab,1}),
		       case get(restart) of
			   undefined -> 
			       Tester ! {pid_1, self()},
			       %% Wait for sync, the read value have been 
			       %% updated and this function should be restarted.
			       receive {Tester,sync} -> ok  end,
			       put(restart, restarted);
			   restarted ->
			       ok
		       end,		       
		       mnesia:write({Tab,1,Val+10})
	       end,
    Lucky    = fun() -> 
		       [{Tab,1,Val}] = mnesia:read({Tab,1}),
		       mnesia:write({Tab,1,Val+100})
	       end,
    
    CPid = spawn_link(fun() -> Tester ! {self(), mnesia:transaction(Conflict)} end),
    
    %% sync first transaction
    receive {pid_1, CPid} -> synced end,

    DeleteRes(Tab, Where2Read),

    ?match(Keep, mnesia:table_info(Tab, where_to_read)),
    
    %% Run the other/Lucky transaction, this should work since 
    %% it won't grab a lock on the conflicting transactions Where2Read node.
    
    LPid = spawn_link(Keep, fun() -> Tester ! {self(),mnesia:transaction(Lucky)} end),
    ?match_receive({LPid,{atomic,ok}}),
    
    %% Continue Transaction no 1
    CPid ! {self(), sync},
    
    ?match(ok, receive {CPid,{atomic,ok}} -> ok after 2000 -> process_info(self()) end),
    
    ?match({atomic,[{del_res,1,111}]}, mnesia:transaction(fun() -> mnesia:read({Tab,1}) end)),
    Where2Read.

nasty(suite) -> [];

nasty(doc) ->
    ["Tries to fullfill a rather nasty locking scenario, where we have had "
     "bugs, the testcase tries a combination of locks in locker queue"];

%%  This testcase no longer works as it was intended to show errors when 
%%  tablelocks was allowed to be placed in the queue though locks existed 
%%  in the queue with less Tid's. This is no longer allowed and the testcase
%%  has been update.

nasty(Config) ->
    ?acquire_nodes(1, Config),
    Tab = nasty,
    ?match({atomic, ok}, mnesia:create_table(Tab, [])),    
    Coord = self(),        				    
    Write = fun(Key) ->
		    mnesia:write({Tab, Key, write}),	 
		    Coord ! {write, Key, self(), mnesia:get_activity_id()},
		    receive 
			continue ->
			    ok
		    end,	    
		    Coord ! {done, {write, Key}, self()}		    
	    end,
    
    Update = fun(Key) ->
		     Coord ! {update, Key, self(), mnesia:get_activity_id()},
		     receive
			 continue ->
			     ok
		     end,
		     mnesia:read({Tab, Key}),
		     mnesia:write({Tab, Key, update}),
		     receive
			 continue ->
			     ok
		     end,
		     
		     Coord ! {done, {update, Key}, self()}
	     end,
    
    TabLock = fun() ->
		      Coord ! {tablock, Tab, self(), mnesia:get_activity_id()},
		      receive
			  continue ->
			      ok
		      end,
		      mnesia:lock({table, Tab}, write),
		      Coord ! {done, {tablock, Tab}, self()}
	      end,

    Up = spawn_link(mnesia, transaction, [Update,  [0]]),
    ?match_receive({update, 0, Up, _Tid}),
    TL = spawn_link(mnesia, transaction, [TabLock]), 
    ?match_receive({tablock, Tab, _Tl, _Tid}),
    W0 = spawn_link(mnesia, transaction, [Write, [0]]), 
    ?match_receive({write, 0, W0, _Tid}),
    W1 = spawn_link(mnesia, transaction, [Write, [1]]), 
    ?match_receive({write, 1, W1, _Tid}),
    
    %% Nothing should be in msg queue!
    ?match(timeout, receive A -> A after 1000 -> timeout end),
    Up ! continue,  %% Should be queued
    ?match(timeout, receive A -> A after 1000 -> timeout end),
    TL ! continue,  %% Should be restarted
%    ?match({tablock, _, _, _}, receive A -> A after 1000 -> timeout end),
    ?match(timeout, receive A -> A after 1000 -> timeout end),
    
    LQ1 = mnesia_locker:get_lock_queue(),    
    ?match({2, _}, {length(LQ1), LQ1}),
    W0 ! continue,                          % Up should be in queue
    ?match_receive({done, {write, 0}, W0}),
    ?match_receive({'EXIT', W0, normal}),

    TL ! continue,   % Should stay in queue W1
    ?match(timeout, receive A -> A after 1000 -> timeout end),
    Up ! continue,   % Should stay in queue (TL got higher tid)
    ?match(timeout, receive A -> A after 1000 -> timeout end),

    LQ2 = mnesia_locker:get_lock_queue(),
    ?match({2, _}, {length(LQ2), LQ2}),    
    
    W1 ! continue,
    ?match_receive({done, {write, 1}, W1}),
    get_exit(W1),
    get_exit(TL),
    ?match_receive({done, {tablock,Tab}, TL}),
    get_exit(Up),
    ?match_receive({done, {update, 0}, Up}),
    
    ok.

get_exit(Pid) ->
    receive 
	{'EXIT', Pid, normal} ->
	    ok
    after 10000 ->
	    ?error("Timeout EXIT ~p~n", [Pid])
    end.


foldl(doc) ->
    [""];
foldl(suite) ->
    [];
foldl(Config) when is_list(Config) ->
    Nodes = [_,N2] = ?acquire_nodes(2, Config),
    Tab1 = foldl_local,
    Tab2 = foldl_remote,
    Tab3 = foldl_ordered, 
    Tab11 = foldr_local,
    Tab21 = foldr_remote,
    Tab31 = foldr_ordered, 
    ?match({atomic, ok}, mnesia:create_table(Tab1, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(Tab2, [{ram_copies, [N2]}, {type, bag}])),
    ?match({atomic, ok}, mnesia:create_table(Tab3, [{ram_copies, Nodes}, 
						    {type, ordered_set}])),    
    ?match({atomic, ok}, mnesia:create_table(Tab11, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(Tab21, [{ram_copies, [N2]}, {type, bag}])),
    ?match({atomic, ok}, mnesia:create_table(Tab31, [{ram_copies, Nodes}, 
						    {type, ordered_set}])),    


    Tab1Els = [{Tab1, N, N} || N <- lists:seq(1, 10)],
    Tab2Els = [{Tab2, 1, 2} | [{Tab2, N, N} || N <- lists:seq(1, 10)]],
    Tab3Els = [{Tab3, N, N} || N <- lists:seq(1, 10)],
    Tab11Els = [{Tab11, N, N} || N <- lists:seq(1, 10)],
    Tab21Els = [{Tab21, 1, 2} | [{Tab21, N, N} || N <- lists:seq(1, 10)]],
    Tab31Els = [{Tab31, N, N} || N <- lists:seq(1, 10)],
    
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab1Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab2Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab3Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab11Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab21Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab31Els],
    
    Get = fun(E, A) -> [E | A] end,
    
    %% Before 
    AddB = fun(Tab, Func) -> 
		   mnesia:write({Tab, 0, 0}),
		   mnesia:write({Tab, 1, 0}),
		   mnesia:write({Tab, 11, 0}),
		   mnesia:Func(Get, [], Tab)
	   end,        
    AddT1 = [{Tab1, 0, 0}, {Tab1, 1, 0}] ++ tl(Tab1Els) ++ [{Tab1, 11, 0}],
    AddT2 = lists:sort([{Tab2, 0, 0}, {Tab2, 1, 0}] ++ Tab2Els ++ [{Tab2, 11, 0}]),
    AddT3 = [{Tab3, 0, 0}, {Tab3, 1, 0}] ++ tl(Tab3Els) ++ [{Tab3, 11, 0}],
    AddT11 = [{Tab11, 0, 0}, {Tab11, 1, 0}] ++ tl(Tab11Els) ++ [{Tab11, 11, 0}],
    AddT21 = lists:sort([{Tab21, 0, 0}, {Tab21, 1, 0}] ++ Tab21Els ++ [{Tab21, 11, 0}]),
    AddT31 = [{Tab31, 0, 0}, {Tab31, 1, 0}] ++ tl(Tab31Els) ++ [{Tab31, 11, 0}],
    
    ?match({atomic, AddT1}, sort_res(mnesia:transaction(AddB, [Tab1, foldl]))),
    ?match({atomic, AddT2}, sort_res(mnesia:transaction(AddB, [Tab2, foldl]))),
    ?match({atomic, AddT3}, rev_res(mnesia:transaction(AddB, [Tab3, foldl]))),
    ?match({atomic, AddT11}, sort_res(mnesia:transaction(AddB, [Tab11, foldr]))),
    ?match({atomic, AddT21}, sort_res(mnesia:transaction(AddB, [Tab21, foldr]))),
    ?match({atomic, AddT31}, mnesia:transaction(AddB, [Tab31, foldr])),
    
    ?match({atomic, ok}, mnesia:create_table(copy, [{ram_copies, [N2]},
						    {record_name, Tab1}])),
    CopyRec = fun(NewRec, Acc) -> 
		      %% OTP-5495
		      W = fun() -> mnesia:write(copy, NewRec, write), [NewRec| Acc] end,
		      {atomic,Res} = sort_res(mnesia:transaction(W)),
		      Res
	      end,
    Copy = fun() -> 
		   AddT1 = mnesia:foldl(CopyRec, [], Tab1),
		   AddT1 = sort_res(mnesia:foldl(Get, [], copy))
	   end,
    ?match({atomic, AddT1}, sort_res(mnesia:transaction(Copy))),
    
    Del  = fun(E, A) -> mnesia:delete_object(E), [E|A] end,
    DelD = fun(Tab) ->
		   mnesia:write({Tab, 12, 12}),
		   mnesia:delete({Tab, 0}),
		   mnesia:foldr(Del, [], Tab),
		   mnesia:foldl(Get, [], Tab)
	   end,
    ?match({atomic, []}, sort_res(mnesia:transaction(DelD, [Tab1]))),
    ?match({atomic, []}, sort_res(mnesia:transaction(DelD, [Tab2]))),
    ?match({atomic, []}, rev_res(mnesia:transaction(DelD, [Tab3]))),
    
    ListWrite = fun(Tab) ->   %% OTP-3893 
			mnesia:write({Tab, [12], 12}),	
			mnesia:foldr(Get, [], Tab)
		end,
    ?match({atomic, [{Tab1, [12], 12}]}, sort_res(mnesia:transaction(ListWrite, [Tab1]))),
    ?match({atomic, [{Tab2, [12], 12}]}, sort_res(mnesia:transaction(ListWrite, [Tab2]))),
    ?match({atomic, [{Tab3, [12], 12}]}, rev_res(mnesia:transaction(ListWrite, [Tab3]))),
    
    ?verify_mnesia(Nodes, []).	   

sort_res({atomic, List}) when is_list(List) ->
    {atomic, lists:sort(List)};
sort_res(Else) when is_list(Else) ->
    lists:sort(Else);
sort_res(Else) ->
    Else.

rev_res({atomic, List}) ->
    {atomic, lists:reverse(List)};
rev_res(Else) ->
    Else.


first_next(doc) ->    [""];
first_next(suite) ->  [];
first_next(Config) when is_list(Config) ->
    Nodes = [_,N2] = ?acquire_nodes(2, Config),
    Tab1 = local,
    Tab2 = remote,
    Tab3 = ordered, 
    Tab4 = bag,
    Tabs = [Tab1,Tab2,Tab3,Tab4],
    
    ?match({atomic, ok}, mnesia:create_table(Tab1, [{ram_copies, Nodes}])),
    ?match({atomic, ok}, mnesia:create_table(Tab2, [{ram_copies, [N2]}])),
    ?match({atomic, ok}, mnesia:create_table(Tab3, [{ram_copies, Nodes}, 
						    {type, ordered_set}])),
    ?match({atomic, ok}, mnesia:create_table(Tab4, [{ram_copies, Nodes}, 
						    {type, bag}])),

    %% Some Helpers
    Trans = fun(Fun) -> mnesia:transaction(Fun) end,
    Continue = fun(first) -> next;
		  (last) -> prev
	       end,
    LoopHelp = fun('$end_of_table',_,_,_Fun) -> [];
		  (Key,Tab,Op,Fun) -> 
		       Next = mnesia:Op(Tab,Key),
		       [Next |Fun(Next,Tab,Op,Fun)] 
	       end,
    Loop = fun(Tab,Start) -> 
		   First = mnesia:Start(Tab),
		   Res = [First|LoopHelp(First,Tab,Continue(Start),LoopHelp)],
		   case mnesia:table_info(Tab, type) of
		       ordered_set when Start == first -> Res;
		       ordered_set -> 
			   {L1,L2} = lists:split(length(Res)-1,Res),
			   lists:reverse(L1) ++ L2;
		       _ -> lists:sort(Res)
		   end	  
	   end,

    %% Verify empty tables
    [?match({atomic, ['$end_of_table']}, 
 	    Trans(fun() -> Loop(Tab,first) end)) 
     || Tab <- Tabs],
    [?match({atomic, ['$end_of_table']}, 
 	    Trans(fun() -> Loop(Tab,last) end)) 
     || Tab <- Tabs],
    %% Verify that trans write is visible inside trans
    [?match({atomic, [0,10,'$end_of_table']}, 
 	    Trans(fun() ->
 			  mnesia:write({Tab,0,0}),
 			  mnesia:write({Tab,10,10}),
 			  Loop(Tab,first) end)) 
     || Tab <- Tabs],
    [?match({atomic, ['$end_of_table']}, 
 	    Trans(fun() -> 
 			  mnesia:delete({Tab,0}),
 			  mnesia:delete({Tab,10}),
 			  Loop(Tab,first) end)) 
     || Tab <- Tabs],

    [?match({atomic, [0,10,'$end_of_table']}, 
 	    Trans(fun() -> 
 			  mnesia:write({Tab,0,0}),
 			  mnesia:write({Tab,10,10}),
 			  Loop(Tab,last) end))
     || Tab <- Tabs],
    [?match({atomic, ['$end_of_table']}, 
 	    Trans(fun() -> 
 			  mnesia:delete({Tab,0}),
 			  mnesia:delete({Tab,10}),
 			  Loop(Tab,last) end)) 
     || Tab <- Tabs],

    Tab1Els = [{Tab1, N, N} || N <- lists:seq(1, 5)],
    Tab2Els = [{Tab2, N, N} || N <- lists:seq(1, 5)],
    Tab3Els = [{Tab3, N, N} || N <- lists:seq(1, 5)],
    Tab4Els = [{Tab4, 1, 2} | [{Tab4, N, N} || N <- lists:seq(1, 5)]],

    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab1Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab2Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab3Els],
    [mnesia:sync_dirty(fun() -> mnesia:write(E) end) || E <- Tab4Els],
    Keys = lists:sort(mnesia:dirty_all_keys(Tab1)),
    R1  =  Keys++ ['$end_of_table'],
    [?match({atomic, R1}, Trans(fun() -> Loop(Tab,first) end)) 
     || Tab <- Tabs],

    [?match({atomic, R1}, Trans(fun() -> Loop(Tab,last) end)) 
     || Tab <- Tabs],
    R2 = R1 -- [3],

    [?match({atomic, R2}, Trans(fun() -> mnesia:delete({Tab,3}),Loop(Tab,first) end)) 
     || Tab <- Tabs],
    [?match({atomic, R1}, Trans(fun() -> mnesia:write({Tab,3,3}),Loop(Tab,first) end)) 
     || Tab <- Tabs],
    [?match({atomic, R2}, Trans(fun() -> mnesia:delete({Tab,3}),Loop(Tab,last) end)) 
     || Tab <- Tabs],
    [?match({atomic, R1}, Trans(fun() -> mnesia:write({Tab,3,3}),Loop(Tab,last) end)) 
     || Tab <- Tabs],
    [?match({atomic, R1}, Trans(fun() -> mnesia:write({Tab,4,19}),Loop(Tab,first) end)) 
     || Tab <- Tabs],
    [?match({atomic, R1}, Trans(fun() -> mnesia:write({Tab,4,4}),Loop(Tab,last) end)) 
     || Tab <- Tabs],

    ?verify_mnesia(Nodes, []).


snmp_shadows(doc) ->    [""];
snmp_shadows(suite) ->  [];
snmp_shadows(Config) when is_list(Config) ->
    Nodes = ?acquire_nodes(1, Config),
    Tab = snmp_shadows,
    io:format("With fixstring~n", []),
    ?match({atomic, ok}, mnesia:create_table(Tab,[{snmp,[{key,{fix_string,integer}}]}])),
    snmp_shadows_test(Tab),
    ?match({atomic, ok}, mnesia:delete_table(Tab)),
    io:format("Without fixstring~n", []),
    ?match({atomic, ok}, mnesia:create_table(Tab,[{snmp,[{key,{string,integer}}]}])),
    snmp_shadows_test(Tab),
    ?verify_mnesia(Nodes, []).

snmp_shadows_test(Tab) ->
    [mnesia:dirty_write({Tab, {"string", N}, {N, init}}) || N <- lists:seq(2,8,2)],
    
    CheckOrder = fun(A={_,_,{_,_,State}}, Prev) ->
			 ?match({true, A, Prev}, {Prev < A, A, Prev}),
			 {State,A} 
		 end,
    R1 = mnesia:sync_dirty(fun() -> loop_snmp(Tab, []) end),
    lists:mapfoldl(CheckOrder, {[],foo,foo}, R1),
    R2 = mnesia:transaction(fun() -> loop_snmp(Tab, []) end),
    ?match({atomic, R1}, R2),
    
    Shadow = fun() ->
		     ok = mnesia:write({Tab, {"string",1}, {1,update}}),
		     ok = mnesia:write({Tab, {"string",4}, {4,update}}),
		     ok = mnesia:write({Tab, {"string",6}, {6,update}}),
		     ok = mnesia:delete({Tab, {"string",6}}),
		     ok = mnesia:write({Tab, {"string",9}, {9,update}}),
		     ok = mnesia:write({Tab, {"string",3}, {3,update}}),
		     ok = mnesia:write({Tab, {"string",5}, {5,update}}),
		     [Row5] = mnesia:read({Tab, {"string",5}}),
		     ok = mnesia:delete_object(Row5),
		     loop_snmp(Tab, [])
	     end,
    R3 = mnesia:sync_dirty(Shadow),
    {L3,_} = lists:mapfoldl(CheckOrder, {[],foo,foo}, R3),
    ?match([{1,update},{2,init},{3,update},{4,update},{8,init},{9,update}], L3),
    ?match({atomic, ok}, mnesia:clear_table(Tab)),

    [mnesia:dirty_write({Tab, {"string", N}, {N, init}}) || N <- lists:seq(2,8,2)],
    {atomic, R3} = mnesia:transaction(Shadow),
    {L4,_} = lists:mapfoldl(CheckOrder, {[],foo,foo}, R3),
    ?match([{1,update},{2,init},{3,update},{4,update},{8,init},{9,update}], L4),
    ok.
   
loop_snmp(Tab,Prev) ->
    case mnesia:snmp_get_next_index(Tab,Prev) of
	{ok, SKey} ->
	    {{ok,Row},_} = {mnesia:snmp_get_row(Tab, SKey),{?LINE,Prev,SKey}},
	    {{ok,MKey},_} = {mnesia:snmp_get_mnesia_key(Tab,SKey),{?LINE,Prev,SKey}},
	    ?match({[Row],Row,SKey,MKey}, {mnesia:read({Tab,MKey}),Row,SKey,MKey}),
	    [{SKey, MKey, Row} | loop_snmp(Tab, SKey)];
	endOfTable ->
	    []
    end.
