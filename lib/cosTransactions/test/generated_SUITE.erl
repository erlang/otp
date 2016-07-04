%%-----------------------------------------------------------------
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
%%
%%-----------------------------------------------------------------
%% File    : generated_SUITE.erl
%% Purpose : 
%% Created : 27 Jan 2004
%%-----------------------------------------------------------------

-module(generated_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").

-define(default_timeout, test_server:minutes(3)).

-define(match(ExpectedRes, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
			AcTuAlReS;
		    _ ->
			io:format("###### ERROR ERROR ######~n~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS)
		end
	end()).

-define(nomatch(Not, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    Not ->
			io:format("###### ERROR ERROR ######~n~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS);
		    _ ->
			AcTuAlReS
		end
	end()).


-define(checktc(_Op),
        fun(TC) ->
		case orber_tc:check_tc(TC) of
		    false ->
			io:format("###### ERROR ERROR ######~n~p - ~p~n", [Op, TC]),
			exit(TC);
		    true ->
			true
		end
	end).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    ['CosTransactions_Control',
     'CosTransactions_Coordinator',
     'CosTransactions_HeuristicCommit',
     'CosTransactions_HeuristicHazard',
     'CosTransactions_HeuristicMixed',
     'CosTransactions_HeuristicRollback',
     'CosTransactions_Inactive',
     'CosTransactions_InvalidControl',
     'CosTransactions_NoTransaction',
     'CosTransactions_NotPrepared',
     'CosTransactions_NotSubtransaction',
     'CosTransactions_RecoveryCoordinator',
     'CosTransactions_Resource',
     'CosTransactions_SubtransactionAwareResource',
     'CosTransactions_SubtransactionsUnavailable',
     'CosTransactions_Terminator',
     'CosTransactions_TransactionFactory',
     'CosTransactions_Unavailable',
     'CosTransactions_SynchronizationUnavailable',
     'CosTransactions_TransIdentity',
     'CosTransactions_PropagationContext',
     'CosTransactions_otid_t',
     'CosTransactions_WrongTransaction', 'ETraP_Server'].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_HeuristicCommit'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_HeuristicCommit'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_HeuristicCommit':tc())),
    ?match("IDL:omg.org/CosTransactions/HeuristicCommit:1.0", 
	   'CosTransactions_HeuristicCommit':id()),
    ?match("CosTransactions_HeuristicCommit", 
	   'CosTransactions_HeuristicCommit':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_HeuristicHazard'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_HeuristicHazard'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_HeuristicHazard':tc())),
    ?match("IDL:omg.org/CosTransactions/HeuristicHazard:1.0", 
	   'CosTransactions_HeuristicHazard':id()),
    ?match("CosTransactions_HeuristicHazard", 
	   'CosTransactions_HeuristicHazard':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_HeuristicMixed'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_HeuristicMixed'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_HeuristicMixed':tc())),
    ?match("IDL:omg.org/CosTransactions/HeuristicMixed:1.0", 
	   'CosTransactions_HeuristicMixed':id()),
    ?match("CosTransactions_HeuristicMixed", 
	   'CosTransactions_HeuristicMixed':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_HeuristicRollback'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_HeuristicRollback'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_HeuristicRollback':tc())),
    ?match("IDL:omg.org/CosTransactions/HeuristicRollback:1.0", 
	   'CosTransactions_HeuristicRollback':id()),
    ?match("CosTransactions_HeuristicRollback", 
	   'CosTransactions_HeuristicRollback':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_Inactive'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_Inactive'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_Inactive':tc())),
    ?match("IDL:omg.org/CosTransactions/Inactive:1.0", 
	   'CosTransactions_Inactive':id()),
    ?match("CosTransactions_Inactive", 
	   'CosTransactions_Inactive':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_InvalidControl'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_InvalidControl'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_InvalidControl':tc())),
    ?match("IDL:omg.org/CosTransactions/InvalidControl:1.0", 
	   'CosTransactions_InvalidControl':id()),
    ?match("CosTransactions_InvalidControl", 
	   'CosTransactions_InvalidControl':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_NoTransaction'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_NoTransaction'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_NoTransaction':tc())),
    ?match("IDL:omg.org/CosTransactions/NoTransaction:1.0", 
	   'CosTransactions_NoTransaction':id()),
    ?match("CosTransactions_NoTransaction", 
	   'CosTransactions_NoTransaction':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_NotPrepared'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_NotPrepared'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_NotPrepared':tc())),
    ?match("IDL:omg.org/CosTransactions/NotPrepared:1.0", 
	   'CosTransactions_NotPrepared':id()),
    ?match("CosTransactions_NotPrepared", 
	   'CosTransactions_NotPrepared':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_NotSubtransaction'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_NotSubtransaction'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_NotSubtransaction':tc())),
    ?match("IDL:omg.org/CosTransactions/NotSubtransaction:1.0", 
	   'CosTransactions_NotSubtransaction':id()),
    ?match("CosTransactions_NotSubtransaction", 
	   'CosTransactions_NotSubtransaction':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_SubtransactionsUnavailable'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_SubtransactionsUnavailable'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_SubtransactionsUnavailable':tc())),
    ?match("IDL:omg.org/CosTransactions/SubtransactionsUnavailable:1.0", 
	   'CosTransactions_SubtransactionsUnavailable':id()),
    ?match("CosTransactions_SubtransactionsUnavailable", 
	   'CosTransactions_SubtransactionsUnavailable':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_Unavailable'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_Unavailable'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_Unavailable':tc())),
    ?match("IDL:omg.org/CosTransactions/Unavailable:1.0", 
	   'CosTransactions_Unavailable':id()),
    ?match("CosTransactions_Unavailable", 
	   'CosTransactions_Unavailable':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_SynchronizationUnavailable'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_SynchronizationUnavailable'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_SynchronizationUnavailable':tc())),
    ?match("IDL:omg.org/CosTransactions/SynchronizationUnavailable:1.0", 
	   'CosTransactions_SynchronizationUnavailable':id()),
    ?match("CosTransactions_SynchronizationUnavailable", 
	   'CosTransactions_SynchronizationUnavailable':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_TransIdentity'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_TransIdentity'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_TransIdentity':tc())),
    ?match("IDL:omg.org/CosTransactions/TransIdentity:1.0", 
	   'CosTransactions_TransIdentity':id()),
    ?match("CosTransactions_TransIdentity", 
	   'CosTransactions_TransIdentity':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_PropagationContext'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_PropagationContext'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_PropagationContext':tc())),
    ?match("IDL:omg.org/CosTransactions/PropagationContext:1.0", 
	   'CosTransactions_PropagationContext':id()),
    ?match("CosTransactions_PropagationContext", 
	   'CosTransactions_PropagationContext':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_otid_t'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_otid_t'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_otid_t':tc())),
    ?match("IDL:omg.org/CosTransactions/otid_t:1.0", 
	   'CosTransactions_otid_t':id()),
    ?match("CosTransactions_otid_t", 
	   'CosTransactions_otid_t':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_WrongTransaction'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_WrongTransaction'(_) ->
    ?match(true, orber_tc:check_tc('CosTransactions_WrongTransaction':tc())),
    ?match("IDL:omg.org/CosTransactions/WrongTransaction:1.0", 
	   'CosTransactions_WrongTransaction':id()),
    ?match("CosTransactions_WrongTransaction", 
	   'CosTransactions_WrongTransaction':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_Control'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_Control'(_) ->
    ?nomatch(undefined, 'CosTransactions_Control':oe_tc(get_terminator)),
    ?nomatch(undefined, 'CosTransactions_Control':oe_tc(get_coordinator)),
    ?match(undefined, 'CosTransactions_Control':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_Control':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/Control:1.0", 
	   'CosTransactions_Control':typeID()),
    check_tc('CosTransactions_Control':oe_get_interface()),
    ?match(true, 'CosTransactions_Control':oe_is_a('CosTransactions_Control':typeID())),
    ?match(false, 'CosTransactions_Control':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_Coordinator'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_Coordinator'(_) ->
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(get_status)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(get_parent_status)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(get_top_level_status)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(is_same_transaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(is_related_transaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(is_ancestor_transaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(is_descendant_transaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(is_top_level_transaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(hash_transaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(hash_top_level_tran)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(register_resource)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(register_subtran_aware)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(rollback_only)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(get_transaction_name)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(create_subtransaction)),
    ?nomatch(undefined, 'CosTransactions_Coordinator':oe_tc(get_txcontext)),
    ?match(undefined, 'CosTransactions_Coordinator':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_Coordinator':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/Coordinator:1.0", 
	   'CosTransactions_Coordinator':typeID()),
    check_tc('CosTransactions_Coordinator':oe_get_interface()),
    ?match(true, 'CosTransactions_Coordinator':oe_is_a('CosTransactions_Coordinator':typeID())),
    ?match(false, 'CosTransactions_Coordinator':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_RecoveryCoordinator'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_RecoveryCoordinator'(_) ->
    ?nomatch(undefined, 'CosTransactions_RecoveryCoordinator':oe_tc(replay_completion)),
    ?match(undefined, 'CosTransactions_RecoveryCoordinator':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_RecoveryCoordinator':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/RecoveryCoordinator:1.0", 
	   'CosTransactions_RecoveryCoordinator':typeID()),
    check_tc('CosTransactions_RecoveryCoordinator':oe_get_interface()),
    ?match(true, 'CosTransactions_RecoveryCoordinator':oe_is_a('CosTransactions_RecoveryCoordinator':typeID())),
    ?match(false, 'CosTransactions_RecoveryCoordinator':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_Resource'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_Resource'(_) ->
    ?nomatch(undefined, 'CosTransactions_Resource':oe_tc(prepare)),
    ?nomatch(undefined, 'CosTransactions_Resource':oe_tc(rollback)),
    ?nomatch(undefined, 'CosTransactions_Resource':oe_tc(commit)),
    ?nomatch(undefined, 'CosTransactions_Resource':oe_tc(commit_one_phase)),
    ?nomatch(undefined, 'CosTransactions_Resource':oe_tc(forget)),
    ?match(undefined, 'CosTransactions_Resource':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_Resource':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/Resource:1.0", 
	   'CosTransactions_Resource':typeID()),
    check_tc('CosTransactions_Resource':oe_get_interface()),
    ?match(true, 'CosTransactions_Resource':oe_is_a('CosTransactions_Resource':typeID())),
    ?match(false, 'CosTransactions_Resource':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_SubtransactionAwareResource'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_SubtransactionAwareResource'(_) ->
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(commit_subtransaction)),
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(rollback_subtransaction)),
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(prepare)),
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(rollback)),
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(commit)),
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(commit_one_phase)),
    ?nomatch(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(forget)),
    ?match(undefined, 'CosTransactions_SubtransactionAwareResource':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_SubtransactionAwareResource':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/SubtransactionAwareResource:1.0", 
	   'CosTransactions_SubtransactionAwareResource':typeID()),
    check_tc('CosTransactions_SubtransactionAwareResource':oe_get_interface()),
    ?match(true, 'CosTransactions_SubtransactionAwareResource':oe_is_a('CosTransactions_SubtransactionAwareResource':typeID())),
    ?match(true, 'CosTransactions_SubtransactionAwareResource':oe_is_a('CosTransactions_Resource':typeID())),
    ?match(false, 'CosTransactions_SubtransactionAwareResource':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_Terminator'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_Terminator'(_) ->
    ?nomatch(undefined, 'CosTransactions_Terminator':oe_tc(commit)),
    ?nomatch(undefined, 'CosTransactions_Terminator':oe_tc(rollback)),
    ?match(undefined, 'CosTransactions_Terminator':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_Terminator':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/Terminator:1.0", 
	   'CosTransactions_Terminator':typeID()),
    check_tc('CosTransactions_Terminator':oe_get_interface()),
    ?match(true, 'CosTransactions_Terminator':oe_is_a('CosTransactions_Terminator':typeID())),
    ?match(false, 'CosTransactions_Terminator':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTransactions_TransactionFactory'
%% Description: 
%%-----------------------------------------------------------------
'CosTransactions_TransactionFactory'(_) ->
    ?nomatch(undefined, 'CosTransactions_TransactionFactory':oe_tc(create)),
    ?nomatch(undefined, 'CosTransactions_TransactionFactory':oe_tc(recreate)),
    ?match(undefined, 'CosTransactions_TransactionFactory':oe_tc(undefined)),
    ?match([_|_], 'CosTransactions_TransactionFactory':oe_get_interface()),
    ?match("IDL:omg.org/CosTransactions/TransactionFactory:1.0", 
	   'CosTransactions_TransactionFactory':typeID()),
    check_tc('CosTransactions_TransactionFactory':oe_get_interface()),
    ?match(true, 'CosTransactions_TransactionFactory':oe_is_a('CosTransactions_TransactionFactory':typeID())),
    ?match(false, 'CosTransactions_TransactionFactory':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'ETraP_Server'
%% Description: 
%%-----------------------------------------------------------------
'ETraP_Server'(_) ->
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_status)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_parent_status)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_top_level_status)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(is_same_transaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(is_related_transaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(is_ancestor_transaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(is_descendant_transaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(is_top_level_transaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(hash_transaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(hash_top_level_tran)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(register_resource)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(register_subtran_aware)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(rollback_only)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_transaction_name)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(create_subtransaction)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_txcontext)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(prepare)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(rollback)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(commit)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(commit_one_phase)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(forget)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(replay_completion)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_terminator)),
    ?nomatch(undefined, 'ETraP_Server':oe_tc(get_coordinator)),
    ?match(undefined, 'ETraP_Server':oe_tc(undefined)),
    ?match([_|_], 'ETraP_Server':oe_get_interface()),
    ?match("IDL:omg.org/ETraP/Server:1.0", 
	   'ETraP_Server':typeID()),
    check_tc('ETraP_Server':oe_get_interface()),
    ?match(true, 'ETraP_Server':oe_is_a('ETraP_Server':typeID())),
    ?match(true, 'ETraP_Server':oe_is_a('CosTransactions_Coordinator':typeID())),
    ?match(true, 'ETraP_Server':oe_is_a('CosTransactions_Resource':typeID())),
    ?match(true, 'ETraP_Server':oe_is_a('CosTransactions_RecoveryCoordinator':typeID())),
    ?match(true, 'ETraP_Server':oe_is_a('CosTransactions_Control':typeID())),
    ?match(false, 'ETraP_Server':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% MISC functions
%%-----------------------------------------------------------------
check_tc([]) ->
    ok;
check_tc([{Op, {RetType, InParameters, OutParameters}}|T]) ->
    io:format("checked - ~s~n", [Op]),
    lists:all(?checktc(Op), [RetType|InParameters]),
    lists:all(?checktc(Op), OutParameters),
    check_tc(T).
    
    
