%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(transactions_SUITE).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%% Local
-include_lib("cosTransactions/src/ETraP_Common.hrl").
-include_lib("cosTransactions/include/CosTransactions.hrl").
-include("etrap_test_lib.hrl").
 
-include_lib("common_test/include/ct.hrl").

-define(default_timeout, test_server:minutes(20)).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, cases/0,
	 init_per_suite/1, end_per_suite/1, resource_api/1, etrap_api/1,
	 init_per_testcase/2, end_per_testcase/2, app_test/1]).
 
%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [etrap_api, resource_api, app_test].
	
%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    'oe_CosTransactions':'oe_register'(), 
    'oe_etrap_test':'oe_register'(), 
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    'oe_etrap_test':'oe_unregister'(), 
    'oe_CosTransactions':'oe_unregister'(), 
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    orber:install([node()]),
    application:start(mnesia),
    application:start(orber),
    if
        is_list(Config) ->
	    Config;
        true ->
            exit("Config not a list")
    end.
 
end_per_suite(Config) ->
    application:stop(orber),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    Config.
 
%%-----------------------------------------------------------------
%%  Tests app file
%%-----------------------------------------------------------------
app_test(_Config) ->
    ok=test_server:app_test(cosTransactions),
    ok.

%%-----------------------------------------------------------------
%%  API tests 
%%-----------------------------------------------------------------
etrap_api(_Config) ->
    ?match(ok, application:start(cosTransactions),
		 "Starting the cosTransactions application"),
    TrFac = cosTransactions:start_factory(),
    %% Start a new transaction:
    %%      RootCoord
    %%      /       \
    %% SubCoord1  SubCoord2
    Control = 'CosTransactions_TransactionFactory':create(TrFac, 0),
    Term = 'CosTransactions_Control':get_terminator(Control),
    Coord = 'CosTransactions_Control':get_coordinator(Control),
    SubCont1 = 'CosTransactions_Coordinator':create_subtransaction(Coord),
    SubCont2 = 'CosTransactions_Coordinator':create_subtransaction(Coord),
    SubCoord1 = 'CosTransactions_Control':get_coordinator(SubCont1),
    SubCoord2 = 'CosTransactions_Control':get_coordinator(SubCont2),


    %%------ Test CosTransactions::Coordinator ------
    ?match(true, 
		 'CosTransactions_Coordinator':is_same_transaction(Coord, Coord),
		 "'CosTransactions_Coordinator':is_same_transaction"),
    ?match(false, 
		 'CosTransactions_Coordinator':is_same_transaction(Coord, SubCoord1),
		 "'CosTransactions_Coordinator':is_same_transaction"),
    ?match(true, 
		 'CosTransactions_Coordinator':is_descendant_transaction(Coord, Coord),
		 "'CosTransactions_Coordinator':is_descendant_transaction"),
    ?match(false, 
		 'CosTransactions_Coordinator':is_descendant_transaction(Coord, SubCoord1),
		 "'CosTransactions_Coordinator':is_descendant_transaction"),
    ?match(true, 
		 'CosTransactions_Coordinator':is_descendant_transaction(SubCoord1, Coord),
		 "'CosTransactions_Coordinator':is_descendant_transaction"),
    ?match(false, 
		 'CosTransactions_Coordinator':is_descendant_transaction(SubCoord1, SubCoord2),
		 "'CosTransactions_Coordinator':is_descendant_transaction"),
    ?match(true, 
		 'CosTransactions_Coordinator':is_top_level_transaction(Coord),
		 "'CosTransactions_Coordinator':is_top_level_transaction"),
    ?match(false, 
		 'CosTransactions_Coordinator':is_top_level_transaction(SubCoord2),
		 "'CosTransactions_Coordinator':is_top_level_transaction"),

    RootHash  = 'CosTransactions_Coordinator':hash_transaction(Coord),
    RepeatHash= 'CosTransactions_Coordinator':hash_transaction(Coord),
    RootHash2 = 'CosTransactions_Coordinator':hash_top_level_tran(SubCoord1),
    RootHash3 = 'CosTransactions_Coordinator':hash_top_level_tran(Coord),
    _SubHash   = 'CosTransactions_Coordinator':hash_transaction(SubCoord2),
    ?match(RootHash, RepeatHash,
		 "'CosTransactions_Coordinator':hash_transaction"),
    ?match(RootHash, RootHash2,
		 "'CosTransactions_Coordinator':hash_top_level_tran"),
    ?match(RootHash, RootHash3,
		 "'CosTransactions_Coordinator':hash_top_level_tran"),
%    ?match_inverse(RootHash, SubHash,
%		 "'CosTransactions_Coordinator':hash_transaction"),

    ?match('StatusActive', 
		 'CosTransactions_Coordinator':get_status(Coord),
		 "'CosTransactions_Coordinator':get_status"),
    ?match('StatusActive', 
		 'CosTransactions_Coordinator':get_status(SubCoord1),
		 "'CosTransactions_Coordinator':get_status"),
    ?match('StatusActive', 
		 'CosTransactions_Coordinator':get_parent_status(Coord),
		 "'CosTransactions_Coordinator':get_parent_status"),
    ?match('StatusActive', 
		 'CosTransactions_Coordinator':get_parent_status(SubCoord1),
		 "'CosTransactions_Coordinator':get_parent_status"),
    ?match('StatusActive', 
		 'CosTransactions_Coordinator':get_top_level_status(Coord),
		 "'CosTransactions_Coordinator':get_top_level_status"),
    ?match('StatusActive', 
		 'CosTransactions_Coordinator':get_top_level_status(SubCoord1),
		 "'CosTransactions_Coordinator':get_top_level_status"),

    %% Create a CosTransactions::Resource to experiments with.
    %% Start a new transaction:
    %%      RootCoord
    %%      /       \
    %% SubCoord1  SubCoord2
    %%    /
    %% Resource
    N1 = 'ETraP_Common':create_name("test"),
    O1 = etrap_test_server:oe_create(?nop, {global, N1}),
    _RC1 = 'CosTransactions_Coordinator':register_resource(SubCoord1, O1),
%    'CosTransactions_Coordinator':register_synchronization(SubCoord1, O1),

    ?match('VoteCommit',
		 'CosTransactions_Resource':prepare(SubCoord1),
		 "'CosTransactions_Coordinator':prepare"),
    %% The Transaction are no longer in 'StatusActive' state. No new
    %% "members" allowed.
    ?match('StatusPrepared',
		 'CosTransactions_Coordinator':get_status(SubCoord1),
		 "'CosTransactions_Coordinator':get_status"),
%    ?match({'EXCEPTION', ?tr_inactive},
%		 'CosTransactions_Coordinator':register_synchronization(SubCoord1, O1),
%		 "'CosTransactions_Coordinator':register_synchronization"),
    ?match({'EXCEPTION', ?tr_inactive},
		 'CosTransactions_Coordinator':register_resource(SubCoord1, O1),
		 "'CosTransactions_Coordinator':register_resource"),
    ?match({'EXCEPTION', ?tr_inactive},
		 'CosTransactions_Coordinator':create_subtransaction(SubCoord1),
		 "'CosTransactions_Coordinator':create_subtransaction"),

    catch corba:dispose(SubCoord1),
    catch corba:dispose(SubCoord2),
    catch corba:dispose(SubCont1),
    catch corba:dispose(SubCont2),
    catch corba:dispose(Term),
    catch corba:dispose(Control),
    catch corba:dispose(Coord),
    catch corba:dispose(O1),

    cosTransactions:stop_factory(TrFac),
    application:stop(cosTransactions),
    ok.

%%-----------------------------------------------------------------
%%  API tests 
%%-----------------------------------------------------------------
resource_api(_Config) ->
    ?match(ok, application:start(cosTransactions),
		 "Starting the cosTransactions application"),
    TrFac = cosTransactions:start_factory([{typecheck, true}]),

    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
		 run(TrFac, 0, {?nop, ?nop, ?nop, ?prepare_rollback}),
		 "TESTCASE #1: Prepare rollback Resource 4"),
    ?match({'EXCEPTION', ?tr_mixed},
		 run(TrFac, 0, {?nop, ?nop, ?commit_mix, ?nop}),
		 "TESTCASE #2: Heuristic Mixed exception Resource 3"),
    ?match(ok, 
		 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop}),
		 "TESTCASE #3: Normal completion. No errors."),
    ?match(ok,
		 run(TrFac, 0, {?nop, ?nop, ?nop, ?commit_cm}),
		 "TESTCASE #4: Heuristic Commit Exception Resource 4"),
    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
		 run(TrFac, 0, {?nop, ?rollback_rb, ?nop, ?prepare_rollback}),
		 "TESTCASE #5: Heuristic Rollbac Resource 2, Resource 4 reply 'VoteRollback'"),
    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
		 run(TrFac, 0, {?nop, ?nop, ?prepare_rollback, ?rollback_rb}),
		 "TESTCASE #6: Heuristic Rollbac Resource 4, Resource 3 reply 'VoteRollback'"),
    ?match(ok,
		 run(TrFac, 0, {?nop, ?nop, ?commit_delay, ?nop}),
		 "TESTCASE #7: Resource 3 delay during commit. No timeout."),
    ?match(ok,
		 run(TrFac, 0, {?nop, ?nop, ?prepare_delay, ?nop}),
		 "TESTCASE #8: Resource 3 delay during prepare. No timeout."),
    ?match(ok,
		 run(TrFac, ?TIMEOUT, {?nop, ?commit_delay, ?nop, ?nop}),
		 "TESTCASE #9: Resource 3 delay during commit. Timeout."),
    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
		 run(TrFac, ?TIMEOUT, {?nop, ?prepare_delay, ?nop, ?nop}),
		 "TESTCASE #10: Resource 3 delay during prepare. Timeout."),
    case ?is_debug_compiled of
	true ->
	    %% Testing the Coordinators (root and sub).
	    ?match(ok,
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?nop, ?nop,?crash_transient(commit), ?nop]}),
			 "TESTCASE #11: SubCoord 3 crash transient during commit."),
	    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{}},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?nop, ?nop,?crash_transient(send_prepare), ?nop]}),
			 "TESTCASE #12: SubCoord 3 crash transient during send prepare."),
	    ?match({'EXCEPTION', ?tr_hazard},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?nop, ?nop,?crash_permanent(commit), ?nop]}),
			 "TESTCASE #13: SubCoord 3 crash permanent during commit."),
	    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{}},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?nop, ?nop,?crash_permanent(send_prepare), ?nop]}),
			 "TESTCASE #14: SubCoord 3 crash permanent during prepare."),
	    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{}},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?nop, ?crash_transient(send_prepare), ?crash_transient(commit), ?nop]}),
			 "TESTCASE #15: SubCoord 2 crash transient during prepare. SubCoord 3 crash transient during commit"),
	    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?crash_transient(send_prepare), ?nop, ?nop, ?nop]}),
			 "TESTCASE #16: RootCoord crash transient during send prepare."),
	    ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{}},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?nop, [?nop, ?crash_transient(prepare1), ?nop, ?nop]}),
			 "TESTCASE #17: SubCoord 1 crash transient during prepare1."),
	    ?match({'EXCEPTION', ?tr_mixed}, 
			 run(TrFac, 0, {?nop, ?prepare_mix, ?nop, ?nop, [?nop, ?nop, ?crash_transient(prepare2), ?nop]}),
			 "TESTCASE #18: SubCoord 3 crash transient during prepare2. Resource 2 raise Heuristic Mixed during prepare"),
	    ?match({'EXCEPTION', ?tr_mixed},
			 run(TrFac, 0, {?nop, ?commit_mix, ?nop, ?nop, [?nop, ?nop, ?crash_transient(commit2), ?nop]}),
			 "TESTCASE #19: Resource 2 raise Heurist mixed during commit. SubCoord crash transient commit2"),
	    ?match({'EXCEPTION', ?tr_mixed},
			 run(TrFac, 0, {?nop, ?rollback_cm, ?nop, ?prepare_rollback, [?nop, ?crash_transient(rollback2), ?nop, ?nop]}),
			 "TESTCASE #20: Resource 2 raise Heuristic Commit during rollback. Resource 4 'VoteRollback'. SubCoord 2 crash transient rollback2."),
	    ?match({'EXCEPTION', ?tr_mixed},
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?commit_mix, [?nop, ?nop, ?crash_transient(send_forget1), ?nop]}),
			 "TESTCASE #21: Resource 4 raise Heuristic Mixed during commit. SubCoord 2 crash transient send_forget1."),
	    ?match({'EXCEPTION', ?tr_mixed}, 
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?commit_mix, [?crash_transient(send_forget1), ?nop, ?nop, ?nop]}),
			 "TESTCASE #22: Resource 4 raise Heuristic Mixed during commit. Root Coord crash transient send_forget1."),
	    ?match({'EXCEPTION', ?tr_mixed}, 
			 run(TrFac, 0, {?nop, ?nop, ?nop, ?commit_mix, [?crash_transient(send_forget3), ?nop, ?crash_transient(send_forget1), ?nop]}),
			 "TESTCASE #23: Resource 4 raise Heuristic Mixed during commit. Root Coord crash transient send_forget3. SubCoord 3 crash transient send_forget1."),
		 ?match({'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
			 run(TrFac, ?TIMEOUT, {?nop, ?nop, ?nop, ?nop, [?delay_transient(root_delay, ?TIMEOUT*2), ?nop, ?nop, ?nop]}),
			 "TESTCASE #24: Delay RootCoord. Timeout."),
	    %% Testing the Terminator.
	    ?match({'EXCEPTION', ?tr_mixed},
			 run(TrFac, ?TIMEOUT, {?nop, ?prepare_mix, ?nop, ?nop, [?nop, ?nop, ?nop, ?crash_transient(commit_heuristic1)]}),
			 "TESTCASE #25: Terminator crash transient after received and logged Heuristic mix."),
	    ?match(ok,
			 run(TrFac, ?TIMEOUT, {?nop, ?nop, ?nop, ?nop, [?nop, ?nop, ?nop, ?crash_transient(commit_ok2)]}),
			 "TESTCASE #26: Terminator crash transient after received and logged 'ok'.");
	_ ->
	    ok
    end,

    cosTransactions:stop_factory(TrFac),
    application:stop(cosTransactions),
    ok.
 
%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

run(TrFac, Time, Spec) ->
    Control = 'CosTransactions_TransactionFactory':create(TrFac, Time),
    Term = 'CosTransactions_Control':get_terminator(Control),
    Coord = 'CosTransactions_Control':get_coordinator(Control),
    SubCont1 = 'CosTransactions_Coordinator':create_subtransaction(Coord),
    SubCont2 = 'CosTransactions_Coordinator':create_subtransaction(Coord),
    SubCoord1 = 'CosTransactions_Control':get_coordinator(SubCont1),
    SubCoord2 = 'CosTransactions_Control':get_coordinator(SubCont2),
    %% Start resources/participants.
    {O1, O2, O3, O4, Ctx} = start_resources(Spec),

    %% Get generated names to use for debugging.
    CoordN = 'CosTransactions_Coordinator':get_transaction_name(Coord),
    SubC1N = 'CosTransactions_Coordinator':get_transaction_name(SubCoord1),
    SubC2N = 'CosTransactions_Coordinator':get_transaction_name(SubCoord2),

   ?set_debug_context([CoordN, SubC1N, SubC2N, Term], Ctx),

    %% Register the resources as participants.
    _RC1 = 'CosTransactions_Coordinator':register_resource(SubCoord1, O1),
    _RC2 = 'CosTransactions_Coordinator':register_resource(SubCoord1, O2),
    _RC3 = 'CosTransactions_Coordinator':register_resource(SubCoord2, O3),
    _RC4 = 'CosTransactions_Coordinator':register_resource(SubCoord2, O4),

    'CosTransactions_Coordinator':register_subtran_aware(SubCoord1, O4),
%    'CosTransactions_Coordinator':register_synchronization(SubCoord1, O2),

%    Reply = (catch 'CosTransactions_Terminator':commit(Term, true)),
    Reply = (catch 'ETraP_Common':send_stubborn('CosTransactions_Terminator',
						commit, [Term, true],
						?tr_max_retries,
						?tr_comm_failure_wait)),

    catch corba:dispose(SubCoord1),
    catch corba:dispose(SubCoord2),
    catch corba:dispose(SubCont1),
    catch corba:dispose(SubCont2),
    catch corba:dispose(Term),
    catch corba:dispose(Control),
    catch corba:dispose(Coord),
    catch corba:dispose(O1),
    catch corba:dispose(O2),
    catch corba:dispose(O3),
    catch corba:dispose(O4),
    Reply.

start_resources({A1, A2, A3, A4})->
    start_resources({A1, A2, A3, A4, ?no_context});
start_resources({A1, A2, A3, A4, Ctx})->
    N1 = 'ETraP_Common':create_name("test"),
    N2 = 'ETraP_Common':create_name("test"),
    N3 = 'ETraP_Common':create_name("test"),
    N4 = 'ETraP_Common':create_name("test"),
    {_,_,O1} = supervisor:start_child(?SUPERVISOR_NAME, ?SUP_TEST(A1, N1)),
    {_,_,O2} = supervisor:start_child(?SUPERVISOR_NAME, ?SUP_TEST(A2, N2)),
%    {_,_,O2} = supervisor:start_child(?SUPERVISOR_NAME, ?SUP_TEST([{sync,true}|A2], N2)),
    {_,_,O3} = supervisor:start_child(?SUPERVISOR_NAME, ?SUP_TEST(A3, N3)),
    {_,_,O4} = supervisor:start_child(?SUPERVISOR_NAME, ?SUP_TEST(A4, N4)),
    {O1, O2, O3, O4, Ctx}.
