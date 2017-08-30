%%--------------------------------------------------------------------
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
%%---------------------------------------------------------------------
%% File    : ETraP_Common.hrl
%% Purpose : 
%%---------------------------------------------------------------------

-ifndef(ETRAP_COMMON_HRL).
-define(ETRAP_COMMON_HRL, true).

%%--------------- INCLUDES ---------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("cosTransactions/include/CosTransactions.hrl").

%%-------- CONSTANTS ---------------------------------------------------
%% Timeouts
-define(tr_comm_failure_wait, 
	case catch application:get_env(cosTransactions, comFailWait) of
	    {ok, _Time} when is_integer(_Time) ->
		_Time;
	    _ ->
		5000
	end).

-define(tr_max_retries, 
	case catch application:get_env(cosTransactions, maxRetries) of
	    {ok, _Max} when is_integer(_Max) ->
		_Max;
	    _ ->
		40
	end). 

%% Exceptions
% Heuristic
-define(tr_mixed, 
	#'CosTransactions_HeuristicMixed' {}).
-define(tr_hazard, 
	#'CosTransactions_HeuristicHazard' {}).
-define(tr_commit, 
	#'CosTransactions_HeuristicCommit' {}).
-define(tr_rollback, 
	#'CosTransactions_HeuristicRollback' {}).
%% Standard
-define(tr_subunavailable,
	#'CosTransactions_SubtransactionsUnavailable' {}).
-define(tr_unavailable,
	#'CosTransactions_Unavailable' {}).
-define(tr_unprepared, 
	#'CosTransactions_NotPrepared' {}).
-define(tr_inactive, 
	#'CosTransactions_Inactive' {}).
-define(tr_nosync,
	#'CosTransactions_SynchronizationUnavailable' {}).
-define(tr_badparam, 
	#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
-define(tr_NotSubtr,
	#'CosTransactions_NotSubtransaction' {}).

%% TypeID:s
-define(tr_Terminator, 
	'CosTransactions_Terminator':typeID()).
-define(tr_Coordinator, 
	'CosTransactions_Coordinator':typeID()).
-define(tr_Control, 
	'CosTransactions_Control':typeID()).
-define(tr_RecoveryCoordinator, 
	'CosTransactions_RecoveryCoordinator':typeID()).
-define(tr_SubtransactionAwareResource,
	'CosTransactions_SubtransactionAwareResource':typeID()).
-define(tr_Synchronization,
	'CosTransactions_Synchronization':typeID()).
-define(tr_Resource, 
	'CosTransactions_Resource':typeID()).
-define(tr_ETraP, 
	'ETraP_Server':typeID()).
-define(tr_TransactionalObject, 
	'CosTransactions_TransactionalObject':typeID()).


%%-------- MISC --------------------------------------------------------

-define(tr_error_msg(Txt, Arg),
error_logger:error_msg("============ CosTransactions ==============~n"
		       Txt
		       "===========================================~n",
		       Arg)).


-define(tr_NIL_OBJ_REF, corba:create_nil_objref()).

-define(tr_FAC_DEF, [{hash_max, 1013},
		     {allow_subtr, true},
		     {typecheck, true},
		     {tty, false},
		     {logfile, false}]).


%%-------- Supervisor child-specs ------------------------------------
-define(FACTORY_NAME,    oe_cosTransactionsFactory).
-define(SUPERVISOR_NAME, cosTransactions_sup).
-define(SUP_FLAG,        {simple_one_for_one,50,10}).

-define(SUP_FAC(Env), 
	['CosTransactions_TransactionFactory',Env, 
	 [{sup_child, true}, {regname, {local, ?FACTORY_NAME}}]]).

-define(SUP_ETRAP(Env), 
	['ETraP_Server', Env, 
	 [{sup_child, true}, {persistent, true}, 
	  {regname, {global, ?tr_get_etrap(Env)}}]]).

-define(SUP_TERMINATOR(Env),
	['CosTransactions_Terminator', Env, 
	 [{sup_child, true}, {persistent, true}, 
	  {regname, {global, ?tr_get_etrap(Env)}}]]).

-define(SUP_CHILD, 
	{"oe_child",
	 {'ETraP_Common',create_link, []},
	  transient,100000,worker,
	 ['ETraP_Common',
	  'ETraP_Server_impl', 
	  'ETraP_Server',
	  'CosTransactions_Terminator_impl', 
	  'CosTransactions_Terminator',
	  'CosTransactions_TransactionFactory_impl', 
	  'CosTransactions_TransactionFactory']}).


-define(tr_start_child(SPEC),
	case supervisor:start_child(?SUPERVISOR_NAME, SPEC) of
            {ok, Pid, Obj} when is_pid(Pid) ->
		Obj;
            _Other->
		corba:raise(?tr_badparam)
        end).

-define(tr_start_child_pid(SPEC),
	supervisor:start_child(?SUPERVISOR_NAME, SPEC)).

-define(tr_terminate_child(Child),
	supervisor:terminate_child(?SUPERVISOR_NAME, Child)).

-define(tr_delete_child(Child),
	supervisor:delete_child(?SUPERVISOR_NAME, Child)).

%%-------- DATASTRUCTURES ----------------------------------------------
%% tr_*_* 
-record(context, {terminator, etrap, recCoord, alarm = infinity, 
		  timeout = infinity, parents=[], trid, typeCheck,
		  sub_tr_allowed, hashMax, local, rollback=false,
		  reportH, maxRetries, comFailWait}).


%%-------- FUNS --------------------------------------------------------
-define(tr_IS_MEMBER(Obj), 
	fun(X) ->
		case catch corba_object:is_equivalent(Obj, X) of
		    true ->
			true;
		    _ ->
			false
		end
	end).

%% Managing conditional debug functions
-define(is_debug_compiled, 'ETraP_Common':is_debug_compiled()).
-define(set_debug_context(L, C),
	etrap_test_lib:set_debug_context(L, C, ?FILE, ?LINE)).	


-ifdef(debug).
-define(put_debug_data(Key, Data), erlang:put(Key, Data)).
-define(get_debug_data(Key), erlang:get(Key)).
-define(eval_debug_fun(I, E),
	etrap_test_lib:eval_debug_fun(I, E, ?FILE, ?LINE)).
-define(activate_debug_fun(I, F, C),
	etrap_test_lib:activate_debug_fun(I, F, C, ?FILE, ?LINE)).
-define(deactivate_debug_fun(I),
	etrap_test_lib:deactivate_debug_fun(I, ?FILE, ?LINE)).
-define(debug_print(F,A),
	io:format("[LINE: ~p] "++F,[?LINE]++A)).
-define(scratch_debug_fun, 
	etrap_test_lib:scratch_debug_fun()).
-else.
-define(put_debug_data(Key, Data), ok).
-define(get_debug_data(Key), ok).
-define(eval_debug_fun(I, E), ok).
-define(activate_debug_fun(I, F, C), ok).
-define(deactivate_debug_fun(I), ok).
-define(debug_print(F,A), ok).
-define(scratch_debug_fun, ok).
-endif.    


%%-------- CONSTRUCTORS ------------------------------------------------

-define(tr_create_context(ETraP, Terminator, TypeCheck, HM, SubtrOK, MaxRetries,
			  ComFailWait),
	#context{etrap = ETraP, terminator  = Terminator, typeCheck = TypeCheck, 
		 hashMax = HM, sub_tr_allowed = SubtrOK, maxRetries = MaxRetries,
		 comFailWait = ComFailWait}).


%%-------- MISC --------------------------------------------------------
-define(tr_notimeout(Context), 
	'ETraP_Common':try_timeout(Context#context.alarm) == false).
-define(tr_is_root(Context), Context#context.parents == []).
-define(tr_dont_reportH(Context), Context#context.reportH == false).
-define(tr_is_retransmit(Context), 
	Context#context.reportH =/= undefined,
	Context#context.reportH =/= true,
	Context#context.reportH =/= false).

%%-------- SELECTORS ---------------------------------------------------

-define(tr_get_reportH(Context), 
	Context#context.reportH).

-define(tr_get_rollback(Context), 
	Context#context.rollback).

-define(tr_get_subTraOK(Context), 
	Context#context.sub_tr_allowed).

-define(tr_get_hashMax(Context), 
	Context#context.hashMax).

-define(tr_get_local(Context), 
	Context#context.local).

-define(tr_get_trid(Context), 
	Context#context.trid).

-define(tr_get_typeCheck(Context), 
	Context#context.typeCheck).

-define(tr_get_recCoord(Context), 
	Context#context.recCoord).

-define(tr_get_alarm(Context), 
	Context#context.alarm).

-define(tr_get_timeout(Context), 
	Context#context.timeout).

-define(tr_get_etrap(Context), 
	Context#context.etrap).

-define(tr_get_terminator(Context), 
	Context#context.terminator).

-define(tr_get_id(Context), 
	Context#context.self).

-define(tr_get_maxW(Context), 
	Context#context.comFailWait).

-define(tr_get_maxR(Context), 
	Context#context.maxRetries).

-define(tr_get_parents(Context), 
	Context#context.parents).

-define(tr_get_parent(Context), 
	lists:nth(1, Context#context.parents)).

%%-------- MODIFIERS ---------------------------------------------------

-define(tr_set_reportH(Context, Bool), 
	Context#context{reportH = Bool}).

-define(tr_set_rollback(Context, Bool), 
	Context#context{rollback = Bool}).

-define(tr_set_subTraOK(Context, Bool), 
	Context#context{sub_tr_allowed = Bool}).

-define(tr_set_hashMax(Context, HM), 
	Context#context{hashMax = HM}).

-define(tr_reset_local(Context), 
	Context#context{local = undefined}).

-define(tr_set_local(Context, Local), 
	Context#context{local = Local}).

-define(tr_set_trid(Context, TRID), 
	Context#context{trid = TRID}).

-define(tr_set_typeCheck(Context, Bool), 
	Context#context{typeCheck = Bool}).

-define(tr_set_id(Context, ID), 
	Context#context{self = ID}).

-define(tr_set_parents(Context, Parents), 
	Context#context{parents = Parents}).

-define(tr_add_parent(Context, Parent), 
	Context#context{parents = [Parent] ++ Context#context.parents}).

-define(tr_set_recCoord(Context, R), 
	Context#context{recCoord = R}).

-define(tr_set_alarm(Context, EC), 
	Context#context{alarm = EC}).

-define(tr_set_timeout(Context, T), 
	Context#context{timeout = T}).

-define(tr_set_etrap(Context, ETraP), 
	Context#context{etrap = ETraP}).

-define(tr_set_terminator(Context, T), 
	Context#context{terminator = T}).

-endif.
 
%%-------------- EOF ---------------------------------------------------


