%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% File    : ETraP_Server_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------
%% GENERAL CODE COMMENTS:
%% ######################
%% TypeChecking incoming arguments:
%% --------------------------------
%% We allow the user to configure the system so that external calls
%% (not CosTransactions calls) may be typechecked or not when calling
%% for example 'replay_completion'. With typecheck the user will get
%% instant feedback. But since 'is_a' add quiet a lot extra overhead 
%% if the object is located on a remote ORB. Hence, it is up to the
%% user to decide; speed vs. "safety".
%%
%% Log behavior
%% ------------
%% Log files are created in the current directory, which is why the 
%% application requires read/write rights for current directory. The
%% file name looks like:
%% "oe_nonode@nohost_subc_939_383117_295538" (the last part is now())
%% It is equal to what the object is started as, i.e., {regname, {global, X}}.
%% 
%% If the application is unable to read the log it will exit and the 
%% supervisor definitions (found in ETraP_Common.hrl) determines how
%% many times we will retry. If it's impossible to read the log it's
%% considered as a disaster, i.e., user intervention is needed.
%% 
%% If an Object is unreachable when a Coordinator is trying to inform
%% of the true outcome of the transaction the application will retry N
%% times with T seconds wait in between. If it's still impossible to 
%% reach the object it's considered as a disaster, i.e., user 
%% intervention is needed.
%% 
%%----------------------------------------------------------------------

-module('ETraP_Server_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").

%% Local
-include_lib("cosTransactions/src/ETraP_Common.hrl").
-include_lib("cosTransactions/include/CosTransactions.hrl").


%%--------------- IMPORTS-------------------------------------
-import('ETraP_Common', [try_timeout/1]).

%%--------------- EXPORTS-------------------------------------
%%--------------- Inherit from CosTransactions::Resource ----
-export([prepare/2, 
	 rollback/2, 
	 commit/2, 
	 commit_one_phase/2, 
	 forget/2]).

%%--------------- Inherit from CosTransactions::Control -----
-export([get_terminator/2, 
	 get_coordinator/2]).

%%----- Inherit from CosTransactions::RecoveryCoordinator ---
-export([replay_completion/3]).

%%--------------- Inherit from CosTransactions::Coordinator -
-export([create_subtransaction/2,
	 get_txcontext/2,
	 get_transaction_name/2, 
	 get_parent_status/2, 
	 get_status/2, 
	 get_top_level_status/2,
	 hash_top_level_tran/2,
	 hash_transaction/2,
	 is_ancestor_transaction/3, 
	 is_descendant_transaction/3,
	 is_related_transaction/3,
	 is_same_transaction/3, 
	 is_top_level_transaction/2, 
	 register_resource/3, 
	 register_subtran_aware/3, 
	 register_synchronization/3,
	 rollback_only/2]).

%%--------- Inherit from CosTransactions::Synchronization ---
%-export([before_completion/2,
%	 after_completion/3]).


%%--------------- gen_server specific ------------------------
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).



%%--------------- LOCAL DATA ---------------------------------
-record(exc, 
	{rollback   = false,
	 mixed      = false,
	 hazard     = false,
	 unprepared = false,
	 commit     = false}).

%%--------------- LOCAL DEFINITIONS --------------------------

%%--------------- MISC MACROS --------------------------------
-define(etr_log(Log, Data),    etrap_logmgr:log_safe(Log, Data)).
-define(etr_read(Log, Cursor), etrap_logmgr:get_next(Log, Cursor)).

-record(coord, 
	{status,           %% Status of the transaction. 
	 members = [],     %% List of registred resources.
	 votedCommit = [], %% List of the ones that voted commit.
	 raisedHeuristic = [], %% The members which raised an Heur. exc.
	 subAw = [],       %% Resorces which want to be informed  of outcome.
	 sync = [],
	 exc = void,
	 self,
	 etsR}).

%% Selectors
-define(etr_get_status(L),     L#coord.status).
-define(etr_get_members(L),    lists:reverse(L#coord.members)).
-define(etr_get_vc(L),         lists:reverse(L#coord.votedCommit)).
-define(etr_get_raisedH(L),    lists:reverse(L#coord.raisedHeuristic)).
-define(etr_get_exc(L),        L#coord.exc).
-define(etr_get_subAw(L),      lists:reverse(L#coord.subAw)).
-define(etr_get_sync(L),       lists:reverse(L#coord.sync)).
-define(etr_get_self(L),       L#coord.self).
-define(etr_get_etsR(L),       L#coord.etsR).
-define(etr_get_init(Env),     #coord{}).
-define(etr_get_exc_init(),    #exc{}).
%% Modifiers
-define(etr_set_status(L, D),  L#coord{status = D}).
-define(etr_set_members(L, D), L#coord{members = D}).
-define(etr_add_member(L, D),  L#coord{members = [D|L#coord.members]}).
-define(etr_set_vc(L, D),      L#coord{votedCommit = D}).
-define(etr_add_vc(L, D),      L#coord{votedCommit = [D|L#coord.votedCommit]}).
-define(etr_remove_vc(L, D),   L#coord{votedCommit =
				       lists:delete(D, ?etr_get_vc(L))}).
-define(etr_set_raisedH(L, D), L#coord{raisedHeuristic = [D]}).
-define(etr_add_raisedH(L, D), L#coord{raisedHeuristic = 
				       [D|L#coord.raisedHeuristic]}).
-define(etr_remove_raisedH(L, D), L#coord{raisedHeuristic =
					  lists:delete(D, ?etr_get_raisedH(L))}).
-define(etr_set_exc(L, D),     L#coord{exc = D}).
-define(etr_set_subAw(L, D),   L#coord{subAw = [D]}).
-define(etr_add_subAw(L, D),   L#coord{subAw = [D|L#coord.subAw]}).
-define(etr_remove_subAw(L, D), L#coord{subAw =
					lists:delete(D,?etr_get_subAw(L))}).
-define(etr_set_sync(L, D),    L#coord{sync = [D]}).
-define(etr_add_sync(L, D),    L#coord{sync = [D|L#coord.sync]}).
-define(etr_remove_sync(L, D), L#coord{sync = lists:delete(D,?etr_get_sync(L))}).
-define(etr_set_self(L, D),    L#coord{self = D}).
-define(etr_set_etsR(L, D),    L#coord{etsR = D}).


%%------------------------------------------------------------
%% function : init, terminate
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the module ic.
%%------------------------------------------------------------

init(Env) ->
    process_flag(trap_exit,true),
    case catch start_object(Env) of
	{'EXIT', Reason} ->
	    %% Happens when, for example, we encounter an 
	    %% error when reading from the log file.
	    {stop, Reason};
	{'EXCEPTION', E} ->
	    self() ! {suicide, self()},
	    corba:raise(E);
	Other ->
	    Other
    end.



terminate(Reason, {Env, _Local}) ->
    ?debug_print("STOP ~p   ~p~n", [?tr_get_etrap(Env), Reason]),
    case Reason of
	normal -> 
	    %% normal termination. Transaction completed.
	    etrap_logmgr:stop(?tr_get_etrap(Env)),
	    file:delete(?tr_get_etrap(Env)),
	    ok;
	_ ->
	    ?tr_error_msg("Object(~p) terminated abnormal.~n",[?tr_get_etrap(Env)]),
	    ok
    end.


%%------------------------------------------------------------
%% function : handle_call, handle_cast, handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_,_, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.


handle_info(Info, {Env, Local}) ->
    ?debug_print("ETraP_Server:handle_info(~p)~n", [Info]),
    Pid = self(),
    case Info of
	timeout ->
	    ?tr_error_msg("Object( ~p ) timeout. Rolling back.~n",
			  [?tr_get_etrap(Env)]),
            {stop, normal, {Env, Local}};
        {suicide, Pid} ->
            {stop, normal, {Env, Local}};
        _->
            {noreply, {Env, Local}}
    end.


%%--------------- Inherit from CosTransactions::Control -----
%%-----------------------------------------------------------%
%% function : get_terminator
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : a Terminator object reference.
%% Effect   : Supports operations for termination of a transaction
%%------------------------------------------------------------

get_terminator(Self, {Env, Local}) ->
    %% Only allows the root-coordinator to export the termonator.
    %% The reason for this is that only the root-coordinator is allowed
    %% to initiate termination of a transaction. This is however possible
    %% to change and add restictions elsewhere, i.e. to verify if the
    %% commit or rollback call is ok.
    case catch ?tr_get_parents(Env) of
	[] -> % No parents, it's a root-coordinator.
	    % Create terminators environment.
	    TEnv = ?tr_set_etrap(Env, Self),
	    T = ?tr_start_child(?SUP_TERMINATOR(TEnv)),
	    {reply, T, {Env, Local}, ?tr_get_timeout(TEnv)};
	_ ->
	    corba:raise(?tr_unavailable)
    end.

%%-----------------------------------------------------------%
%% function : get_coordinator
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : a Coordinator object reference. The OMG specification
%%            states that a object reference must be returned.
%% Effect   : Supports operations needed by resources to participate
%%            in the transaction.
%%------------------------------------------------------------

get_coordinator(Self, State) ->
    {reply, Self, State}.

%%----- Inherit from CosTransactions::RecoveryCoordinator ---
%%-----------------------------------------------------------%
%% function : replay_completion
%% Arguments:
%% Returns  : Status
%% Effect   : Provides a hint to the Coordinator that the commit
%%            or rollback operations have not been performed on
%%            the resource.
%%------------------------------------------------------------

replay_completion(_Self, {Env, Local}, Resource) ->
    type_check(?tr_get_typeCheck(Env), ?tr_Resource,
	       "RecoveryCoordinator:replay_completion", Resource),
    case ?etr_get_status(Local) of
	'StatusActive' ->
	    corba:raise(?tr_unprepared);
	Status ->
	    case lists:any(?tr_IS_MEMBER(Resource), ?etr_get_members(Local)) of
		true ->
		    {reply, Status, {Env, Local}};
		_ ->
		    corba:raise(#'NO_PERMISSION'{completion_status=?COMPLETED_YES})
	    end
    end.

%%--------------- Inherit from CosTransactions::Resource ----
%%-----------------------------------------------------------%
%% function : prepare
%% Arguments: 
%% Returns  : a Vote
%% Effect   : Is invoked to begin the two-phase-commit on the
%%            resource.
%%------------------------------------------------------------

prepare(_Self, {Env, Local}) ->    
    %% Set status as prepared. No new Resources are allowed to register.
    NewL = ?etr_set_status(Local, 'StatusPrepared'),

    ?eval_debug_fun({?tr_get_etrap(Env), root_delay}, Env),

    case catch send_prepare(?etr_get_members(NewL), 
			    ?tr_get_alarm(Env)) of
	readOnly -> 
	    %% All voted ReadOnly, done. No need to log.
	    {stop, normal, 'VoteReadOnly', {Env, NewL}};
	%% Replace the reply above if allow synchronization
%	    case ?etr_get_sync(Local) of
%		[] ->
%		    {stop, normal, 'VoteReadOnly', {Env, NewL}};
%		_ ->
%		    {reply, 'VoteReadOnly', {Env, NewL}}
%	    end;
	{commit, VC} -> 
	    %% All voted Commit.
	    NewL2 = ?etr_set_vc(NewL, VC),
	    case catch try_timeout(?tr_get_alarm(Env)) of
		false -> 
		    case ?etr_log(?tr_get_etrap(Env), {pre_vote, commit, NewL2}) of
			ok ->
			    ?eval_debug_fun({?tr_get_etrap(Env), prepare1}, Env),
			    {reply, 'VoteCommit', {Env, NewL2}};
			_->
			    %% Cannot log. Better to be safe than sorry; do rollback.
			    %% However, try to log rollback.
			    ?etr_log(?tr_get_etrap(Env),{pre_vote, rollback, NewL2}),
			    send_decision({Env, NewL2}, 'VoteRollback', rollback)
		    end;
		_-> 
		    ?etr_log(?tr_get_etrap(Env),
			     {pre_vote, rollback, NewL2}),
		    %% timeout, reply rollback.
		    send_decision({Env, NewL2}, 'VoteRollback', rollback)
	    end;
	{rollback, VC} -> 
	    %% Rollback vote received. 
	    %% Send rollback to commit voters.
	    N2 = ?etr_set_vc(NewL, VC),
	    NewL2 = ?etr_set_status(N2,'StatusRolledBack'), 
	    ?etr_log(?tr_get_etrap(Env), {pre_vote, rollback, NewL2}),
	    send_decision({Env, NewL2}, 'VoteRollback', rollback);
	{'EXCEPTION', E, VC, Obj} ->
	    NewL2 = case is_heuristic(E) of
				true ->
			    N2 = ?etr_set_vc(NewL, VC),
			    N3 = ?etr_set_exc(N2, E),
			    ?etr_set_raisedH(N3, Obj);
			_->
			    ?etr_set_vc(NewL, VC)
		    end,
	    ?etr_log(?tr_get_etrap(Env),{pre_vote,rollback, NewL2}),
	    ?eval_debug_fun({?tr_get_etrap(Env), prepare2}, Env),	    
	    send_decision({Env, NewL2}, {'EXCEPTION', E}, rollback);
	{failed, VC} ->
	    NewL2 = ?etr_set_vc(NewL, VC),
	    ?etr_log(?tr_get_etrap(Env),{pre_vote, rollback, NewL2}),
	    send_decision({Env, NewL2}, 
			  {'EXCEPTION', ?tr_hazard}, rollback)
    end.


%%-----------------------------------------------------------%
%% function : rollback
%% Arguments: Self - the servers own objref.
%%            {Env, Local} - the servers internal state.
%% Returns  : ok
%% Effect   : Rollback the transaction. If its status is
%%            "StatusRolledBack", this is not the first
%%            rollback call to this server. Might occur if
%%            the parent coordinator just recoeverd from a crasch.
%% Exception: HeuristicCommit, HeuristicMixed, HeuristicHazard
%%------------------------------------------------------------

rollback(Self, {Env, Local}) ->
    case ?etr_get_status(Local) of
	'StatusRolledBack' ->
	    case ?etr_get_exc(Local) of
		void ->
		    {stop, normal, ok, {Env, Local}};
		%% Replace the reply above if allow synchronization
		    %% Rolled back successfullly earlier.
%		    case ?etr_get_sync(Local) of
%			[] ->
%			    {stop, normal, ok, {Env, Local}};
%			_ ->
%			    {reply, ok, {Env, Local}}
%		    end;
		E ->
		    %% Already rolledback with heuristic decision
		    corba:raise(E)
	    end;
	'StatusPrepared' ->
	    NewL = ?etr_set_status(Local, 'StatusRolledBack'),
	    ?eval_debug_fun({?tr_get_etrap(Env), rollback}, Env),
	    ?etr_log(?tr_get_etrap(Env), rollback),
	    ?eval_debug_fun({?tr_get_etrap(Env), rollback2}, Env),
	    send_decision({Env, NewL}, ok, rollback);
	'StatusActive' ->
	    NewL = ?etr_set_status(Local, 'StatusRolledBack'),
	    ?etr_log(?tr_get_etrap(Env), {rollback, NewL}),	    
	    send_info(?etr_get_members(NewL), 'CosTransactions_Resource', rollback),
	    notify_subtrAware(rollback, ?etr_get_subAw(NewL), Self),
	    {stop, normal, ok, {Env, NewL}}
%% Replace the reply above if allow synchronization
%	    case ?etr_get_sync(Local) of
%		[] ->
%		    {stop, normal, ok, {NewEnv, NewL}};
%		_ ->
%		    {reply, ok, {NewEnv, NewL}}
%	    end;
    end.


%%-----------------------------------------------------------%
%% function : commit
%% Arguments: Self - the servers own objref.
%%            {Env, Local} - the servers internal state.
%% Returns  : ok
%% Effect   : Commit the transaction.
%% Exception: HeuristicRollback, HeuristicMixed, HeuristicHazard,
%%            NotPrepared
%%------------------------------------------------------------

commit(_Self, {Env, Local}) ->
    case ?etr_get_status(Local) of
	'StatusPrepared' -> 
	    ?eval_debug_fun({?tr_get_etrap(Env), commit}, Env),
	    NewL = ?etr_set_status(Local, 'StatusCommitted'),
	    ?etr_log(?tr_get_etrap(Env),commit),
	    ?eval_debug_fun({?tr_get_etrap(Env), commit2}, Env),
	    send_decision({Env, NewL}, ok, commit);
	'StatusCommitted' ->
	    case ?etr_get_exc(Local) of
		void ->
		    {stop, normal, ok, {Env, Local}};
		%% Replace the reply above if allow synchronization
%		    case ?etr_get_sync(Local) of
%			[] ->
%			    {stop, normal, ok, {Env, Local}};
%			_ ->
%			    {reply, ok, {Env, Local}}
%		    end;
		E->
		    corba:raise(E)
	    end;
	_ -> 
	    corba:raise(?tr_unprepared)
    end.

%%-----------------------------------------------------------%
%% function : commit_one_phase
%% Arguments: Self - the servers own objref.
%%            {Env, Local} - the servers internal state.
%% Returns  : ok
%% Effect   : Commit the transaction using one-phase commit.
%%            Use ONLY when there is only one registered Resource.
%% Exception: HeuristicRollback, HeuristicMixed, HeuristicHazard,
%%            TRANSACTION_ROLLEDBACK
%%------------------------------------------------------------

commit_one_phase(_Self, {Env, Local}) ->
    case ?etr_get_members(Local) of
	[Resource] ->
	    case ?etr_get_status(Local) of
		'StatusActive' ->
		    %% Set status as prepared. No new Resources are allowed to register.
		    NewL = ?etr_set_status(Local, 'StatusPrepared'),
		    ?eval_debug_fun({?tr_get_etrap(Env), onePC}, Env),
		    case try_timeout(?tr_get_alarm(Env)) of
			false -> 
			    case catch 'CosTransactions_Resource':prepare(Resource) of
				'VoteCommit' ->
				    case try_timeout(?tr_get_alarm(Env)) of
					false -> 
					    send_decision({Env, NewL}, ok, commit, [Resource]);
					_-> 
					    %% Timeout, rollback. 
					    send_decision({Env, NewL}, 
							  {'EXCEPTION', 
							   #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
							  rollback, [Resource])
				    end;
				'VoteRollback' ->
				    {stop, normal, 
				     {'EXCEPTION', 
				      #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
				     {Env, NewL}};
				'VoteReadOnly' ->
				    {stop, normal, ok, {Env, NewL}};
				{'EXCEPTION', E} 
				when is_record(E, 'CosTransactions_HeuristicMixed') ->
				    {reply, {'EXCEPTION', E}, {Env, NewL}};
				{'EXCEPTION', E} 
				when is_record(E, 'CosTransactions_HeuristicHazard') ->
				    {reply, {'EXCEPTION', E}, {Env, NewL}};
				Other ->
				    ?tr_error_msg("Coordinator:prepare( ~p ) failed. REASON ~p~n", 
						  [Resource, Other]),
				    {stop, normal, 
				     {'EXCEPTION', 
				      #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
				     {Env, NewL}}
			    end;
			_->
			    NewL2 = ?etr_set_status(NewL, 'StatusRolledBack'),
			    send_info(Resource, 'CosTransactions_Resource', rollback),
			    {stop, normal, 
			     {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
			     {Env, NewL2}}
			%% Replace the reply above if allow synchronization
%			    case ?etr_get_sync(NewL2) of
%				[] ->
%				    send_info(Resource, 'CosTransactions_Resource', rollback),
%				    {stop, normal, 
%				     {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
%				     {Env, NewL2}};
%				_ ->
%				    send_info(Resource, 'CosTransactions_Resource', rollback),
%				    {reply, 
%				     {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
%				     {Env, NewL2}}
%			    end
		    end;
		_ ->
		    case evaluate_status(?etr_get_status(Local)) of
			commit ->
			    test_exc(set_exception(?etr_get_exc_init(), 
						   ?etr_get_exc(Local)),
				     commit, ok, {Env, Local});
			_->
			    test_exc(set_exception(?etr_get_exc_init(),
						   ?etr_get_exc(Local)),
				     rollback, 
				     {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
				     {Env, Local})
		    end
	    end;
	_->
	    {reply, {'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}, 
	     {Env, Local}}
    end.

%%-----------------------------------------------------------%
%% function : forget
%% Arguments: Self  - the servers own objref.
%%            State - the servers internal state.
%% Returns  : ok
%% Effect   : The resource can forget all knowledge about the
%%            transaction. Terminate this server.
%%------------------------------------------------------------

forget(_Self, {Env, Local}) ->
    ?etr_log(?tr_get_etrap(Env), forget_phase),
    send_forget(?etr_get_raisedH(Local), ?tr_get_etrap(Env)),
    {stop, normal, ok, {Env, ?etr_set_exc(Local, void)}}.
%% Replace the reply above if allow synchronization
%    case ?etr_get_sync(Local) of
%	[] ->
%	    {stop, normal, ok, {Env, ?etr_set_exc(Local, void)}};
%	_ ->
%	    {reply, ok, {Env, ?etr_set_exc(Local, void)}}
%    end.

%%--------------- Inherrit from CosTransactions::Coordinator -
 
%%-----------------------------------------------------------%
%% function : get_status
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : Status
%% Effect   : Returns the status of the transaction associated
%%            with the target object.
%%------------------------------------------------------------

get_status(_Self, {Env, Local}) ->
    {reply, ?etr_get_status(Local), {Env, Local}}.


%%-----------------------------------------------------------%
%% function : get_parent_status
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : Status
%% Effect   : Returns the status of the parent transaction
%%            associated with the target object. If top-level
%%            transaction equal to get_status.
%%------------------------------------------------------------

get_parent_status(_Self, {Env, Local}) ->
    case catch ?tr_get_parents(Env) of
	[] ->
	    {reply, ?etr_get_status(Local), {Env, Local}};
	[Parent|_] ->
	    case catch 'CosTransactions_Coordinator':get_status(Parent) of
		{'EXCEPTION', _E} -> 
		    corba:raise(?tr_unavailable);
		{'EXIT', _} -> 
		    corba:raise(?tr_unavailable);
		Status ->
		    {reply, Status, {Env, Local}}
	    end
    end.


%%-----------------------------------------------------------%
%% function : get_top_level_status
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : Status
%% Effect   : Returns the status of the top-level transaction
%%            associated with the target object. If top-level
%%            transaction equal to get_status.
%%------------------------------------------------------------

get_top_level_status(_Self, {Env, Local}) ->
    case catch ?tr_get_parents(Env) of
	[] ->
	    {reply, ?etr_get_status(Local), {Env, Local}};
	Ancestrors ->
	    case catch 'CosTransactions_Coordinator':get_status(lists:last(Ancestrors)) of
		{'EXCEPTION', _E} ->
		    corba:raise(?tr_unavailable);
		{'EXIT', _} ->
		    corba:raise(?tr_unavailable);
		Status ->
		    {reply, Status, {Env, Local}}
	    end
    end.


%%-----------------------------------------------------------%
%% function : is_same_transaction
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            Coordinator object reference
%% Returns  : boolean
%% Effect   : 
%%------------------------------------------------------------

is_same_transaction(Self, {Env, Local}, Coordinator) ->
    type_check(?tr_get_typeCheck(Env), ?tr_Coordinator,
	       "Coordinator:is_same_transaction", Coordinator),
    {reply, corba_object:is_equivalent(Self, Coordinator), {Env, Local}}.

%%------------------------------------------------------------
%% function : is_related_transaction
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            Coordinator object reference
%% Returns  : boolean
%% Effect   :
%%------------------------------------------------------------

is_related_transaction(_Self, {_Env, _Local}, _Coordinator) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_YES}).
%    type_check(?tr_get_typeCheck(Env), ?tr_Coordinator,
%	       "Coordinator:is_related_transaction", Coordinator),
%    {reply, false, {Env, Local}}.


%%------------------------------------------------------------
%% function : is_ancestor_transaction
%%            Coordinator object reference
%% Returns  : boolean
%% Effect   :
%%------------------------------------------------------------

is_ancestor_transaction(_Self, {_Env, _Local}, _Coordinator) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_YES}).
%    type_check(?tr_get_typeCheck(Env), ?tr_Coordinator,
%	       "Coordinator:is_ancestor_transaction", Coordinator),
%    {reply, false, {Env, Local}}.


%%-----------------------------------------------------------%
%% function : is_descendant_transaction
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            Coordinator object reference
%% Returns  : boolean
%% Effect   :
%%------------------------------------------------------------

is_descendant_transaction(Self, {Env, Local}, Coordinator) ->
    type_check(?tr_get_typeCheck(Env), ?tr_Coordinator,
	       "Coordinator:is_descendant_transaction", Coordinator),
    {reply, 
     lists:any(?tr_IS_MEMBER(Coordinator), [Self|?tr_get_parents(Env)]), 
     {Env, Local}}.

%%-----------------------------------------------------------%
%% function : is_top_level_transaction
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : boolean
%% Effect   :
%%------------------------------------------------------------

is_top_level_transaction(_Self, {Env, Local}) ->
     case catch ?tr_get_parents(Env) of
	[] ->
	     {reply, true, {Env, Local}};
	 _ ->
	     {reply, false, {Env, Local}}
     end.

%%-----------------------------------------------------------%
%% function : hash_transaction
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : hash code
%% Effect   : Returns a hash code for the transaction associated
%%            with the target object.
%%------------------------------------------------------------

hash_transaction(Self, {Env, Local}) ->
    {reply, corba_object:hash(Self, ?tr_get_hashMax(Env)), {Env, Local}}.


%%-----------------------------------------------------------%
%% function : hash_top_level_tran
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : hash code
%% Effect   : Returns a hash code for the top-level transaction 
%%            associated with the target object. Equals 
%%            hash_transaction if it's a top-level transaction.
%%------------------------------------------------------------

hash_top_level_tran(Self, {Env, Local}) ->
    case ?tr_get_parents(Env) of
	[] ->
	    {reply, 
	     corba_object:hash(Self, ?tr_get_hashMax(Env)), 
	     {Env, Local}};
	Ancestrors ->
	    case catch corba_object:hash(lists:last(Ancestrors), 
					 ?tr_get_hashMax(Env)) of
		{'EXCEPTION', _E} -> 
		    corba:raise(?tr_unavailable);
		Hash ->
		    {reply, Hash, {Env, Local}}
	    end
    end.



%%-----------------------------------------------------------%
%% function : register_resource
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            Resource object reference
%% Returns  : RecoveryCoordinator (can be used during recovery)
%% Effect   : Registers the specified resource as as participant
%%            in the transaction associated with the target object.
%% Exception: Inactive - Is prepared or terminated.
%%------------------------------------------------------------

register_resource(Self, {Env, Local}, Resource) ->
    type_check(?tr_get_typeCheck(Env), ?tr_Resource,
	       "Coordinator:register_resource", Resource),
    case ?etr_get_status(Local) of
	'StatusActive' -> % ok to register the Resource.
	    NewLocal = ?etr_add_member(Local, Resource),
	    RecoveryCoord = corba:create_subobject_key(Self, ?tr_get_etrap(Env)),
	    {reply, RecoveryCoord, {Env, NewLocal}, ?tr_get_timeout(Env)};
	_-> % Not active anymore. New members not ok.
	    corba:raise(?tr_inactive)
    end.

	

%%-----------------------------------------------------------%
%% function : register_subtran_aware
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            SubTransactionAwareResource object reference
%% Returns  : -
%% Effect   : Registers the specified object such that it
%%            will be notified when the subtransaction has 
%%            commited or rolled back.
%%------------------------------------------------------------

register_subtran_aware(Self, {Env, Local}, SubTrAwareResource) ->
    case ?tr_get_parents(Env) of
	[] ->
	    corba:raise(?tr_NotSubtr);
	_->
	    type_check(?tr_get_typeCheck(Env), ?tr_SubtransactionAwareResource,
		       "Coordinator:register_subtran_aware", SubTrAwareResource),
	    NewL = ?etr_add_subAw(Local, SubTrAwareResource),
	    {reply, ok, {Env, ?etr_set_self(NewL, Self)}, 
	     ?tr_get_timeout(Env)}
    end.

%%-----------------------------------------------------------%
%% function : register_synchronization
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            Synchronization
%% Returns  : -
%% Effect   : 
%%------------------------------------------------------------

register_synchronization(_Self, {_Env, _Local}, _Synchronization) ->
    corba:raise(#'CosTransactions_SynchronizationUnavailable'{}).

%register_synchronization(Self, {Env, Local}, Synchronization) ->
%    type_check(?tr_get_typeCheck(Env), ?tr_Synchronization,
%	       "Coordinator:register_synchronization", Synchronization),
%    case ?etr_get_status(Local) of
%	'StatusActive' ->
%	    case catch ?tr_get_parents(Env) of
%		[] ->
%		    {reply, ok, {Env, ?etr_add_sync(Local, Synchronization)}, 
%		     ?tr_get_timeout(Env)};
%		[Parent|_] ->
%		    case catch 'ETraP_Server':register_synchronization(Parent, Self) of
%			{'EXCEPTION', E} ->
%			    corba:raise(E);
%			ok ->
%			    {reply, ok, {Env, ?etr_add_sync(Local, Synchronization)}, 
%			     ?tr_get_timeout(Env)};
%			What ->
%			    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE})
%		    end
%	    end;
%	_ ->
%	    corba:raise(?tr_inactive)
%    end.

%%-----------------------------------------------------------%
%% function : rollback_only
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : -
%% Effect   : The transaction associated with the target object
%%            is modified so that rollback IS the result.
%%------------------------------------------------------------

rollback_only(Self, {Env, Local}) ->
    case ?etr_get_status(Local) of
	'StatusActive' ->
	    NewL = ?etr_set_status(Local, 'StatusRolledBack'),
	    NewEnv = ?tr_set_rollback(Env, true),
	    ?etr_log(?tr_get_etrap(Env),{rollback, NewL}),
	    send_info(?etr_get_members(NewL), 'CosTransactions_Resource', rollback),
	    notify_subtrAware(rollback, ?etr_get_subAw(NewL), Self),
	    {stop, normal, ok, {NewEnv, NewL}};
%% Replace the reply above if allow synchronization
%	    case ?etr_get_sync(Local) of
%		[] ->
%		    {stop, normal, ok, {NewEnv, NewL}};
%		_ ->
%		    {reply, ok, {NewEnv, NewL}}
%	    end;
	_ ->
	    corba:raise(?tr_inactive)
    end.

%%-----------------------------------------------------------%
%% function : get_transaction_name
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : string - which describes the transaction associated 
%%                     with the target object.
%% Effect   : Intended for debugging.
%%------------------------------------------------------------

get_transaction_name(_Self, {Env, Local}) ->
    {reply, ?tr_get_etrap(Env), {Env, Local}}.

%%-----------------------------------------------------------%
%% function : create_subtransaction
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : A control object if subtransactions are allowed,
%%            otherwise an exception is raised.
%% Effect   : A new subtransaction is created whos parent is
%%            the transaction associated with the target object.
%% Exception: SubtransactionUnavailabe - no support for nested
%%                                       transactions.
%%            Inactive - already been prepared.
%%------------------------------------------------------------

create_subtransaction(Self, {Env, Local}) ->
    case ?etr_get_status(Local) of
	'StatusActive' ->
	    case ?tr_get_subTraOK(Env) of
		true ->
		    ETraPName = 'ETraP_Common':create_name("subc"),
		    Tname   = 'ETraP_Common':create_name("subt"),
		    
		    %% Create context for the new object.
		    State   = ?tr_create_context(ETraPName, Tname, 
						 ?tr_get_typeCheck(Env), 
						 ?tr_get_hashMax(Env), 
						 ?tr_get_subTraOK(Env),
						 ?tr_get_maxR(Env),
						 ?tr_get_maxW(Env)),


		    State2  = ?tr_add_parent(State, Self),
		    
		    State3  = ?tr_set_alarm(State2, ?tr_get_alarm(Env)),

		    State4  = ?tr_set_timeout(State3, ?tr_get_timeout(Env)),

		    Control = ?tr_start_child(?SUP_ETRAP(State4)),
		    %% Set the SubCoordinator object reference and register it as participant.
		    SubCoord = 'CosTransactions_Control':get_coordinator(Control),
		    NewLocal = ?etr_add_member(Local, SubCoord),
		    {reply, Control, {Env, NewLocal}, ?tr_get_timeout(Env)};
		_ ->
		    %% subtransactions not allowed, raise exception.
		    corba:raise(?tr_subunavailable)
	    end;
	_->
	    corba:raise(?tr_inactive)
    end.

%%-----------------------------------------------------------%
%% function : get_txcontext
%% Arguments: 
%% Returns  : PropagationContext
%% Effect   : 
%%------------------------------------------------------------

get_txcontext(_Self, {_Env, _Local}) ->
    corba:raise(#'CosTransactions_Unavailable'{}).

%get_txcontext(Self, {Env, Local}) ->
%    Otid = #'CosTransactions_otid_t'{formatID=0, 
%				     bqual_length=0, 
%				     tid=[corba_object:hash(Self, 
%							   ?tr_get_hashMax(Env))]},
%    TrIDs = create_TransIdentities(?tr_get_parents(Env), Env, [], Otid),
%    C=case ?tr_get_parents(Env) of
%	  [] ->
%	      #'CosTransactions_TransIdentity'{coord=Self, 
%					       term=?tr_get_terminator(Env), 
%					       otid=Otid};
%	  _->
%	      #'CosTransactions_TransIdentity'{coord=Self, 
%					       term=?tr_NIL_OBJ_REF, 
%					       otid=Otid}
%      end,
%    case ?tr_get_timeout(Env) of
%	infinity ->
%	    #'CosTransactions_PropagationContext'{timeout=0, 
%						  current= C, 
%						  parents=TrIDs};
%	T ->
%	    #'CosTransactions_PropagationContext'{timeout=T/1000, 
%						  current= C, 
%						  parents=TrIDs}
%    end.

%create_TransIdentities([], _, Parents, _) -> Parents;
%create_TransIdentities([Phead|Ptail], Env, Parents, Otid) ->
%    NO=Otid#'CosTransactions_TransIdentity'{otid=
%					    corba_object:hash(Phead, 
%							      ?tr_get_hashMax(Env))},
%    create_TransIdentities([Phead|Ptail], Env, Parents++
%			   [#'CosTransactions_TransIdentity'{coord=Phead, 
%							     term=?tr_NIL_OBJ_REF, 
%							     otid=NO}], 
%			   Otid).


%%--------- Inherit from CosTransactions::Synchronization ---

%%-----------------------------------------------------------%
%% function : before_completion
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

%before_completion(Self, {Env, Local}) ->
%    send_info(?etr_get_sync(Local), 
%	      'CosTransactions_Synchronization', before_completion),
%    {reply, ok, {Env, Local}}.

%%-----------------------------------------------------------%
%% function : after_completion
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

%after_completion(Self, {Env, Local}, Status) ->
%    send_info(?etr_get_sync(Local), Status,
%	      'CosTransactions_Synchronization', after_completion),
%    {stop, normal, ok, {Env, Local}}.

%%--------------- IMPLEMENTATION SPECIFIC -------------------
%%-----------------------------------------------------------%
%% function : start_object
%% Arguments: 
%% Returns  : EXIT, EXCEPTION, or {ok, State}
%% Effect   : used by init/1 only.
%%------------------------------------------------------------

start_object(Env)->
    ?put_debug_data(self, Env),
    Local = ?etr_get_init(Env),
    LogName = ?tr_get_etrap(Env),
    case catch file:read_file_info(LogName) of
	{error, enoent} -> 
	    %% File does not exist. It's the first time. No restart.
	    ?debug_print("ETraP_Server:init(~p)~n",[?tr_get_etrap(Env)]),
	    etrap_logmgr:start(LogName),
	    {ok, 
	     {Env, ?etr_set_status(Local, 'StatusActive')}, 
	     ?tr_get_timeout(Env)};
	{error, Reason} -> 
	    %% File exist but error occurred.
	    ?tr_error_msg("Control (~p) Cannot open log file: ~p~n",
			  [LogName, Reason]),
	    {stop, "unable_to_open_log"};
	_ -> 
	    %% File exists, perform restart.
	    etrap_logmgr:start(LogName),
	    ?debug_print("RESTART ~p~n", [?tr_get_etrap(Env)]),
	    prepare_restart({Env, ?etr_set_status(Local, 'StatusUnknown')},
			    ?etr_read(?tr_get_etrap(Env), start))
    end.


%%-----------------------------------------------------------%
%% function : send_prepare
%% Arguments: List of registred resources. 
%% Returns  : ok - equal to void
%% Effect   : calls send_prepare/3, which sends a prepare call
%% to resources participating in the transaction and then collect 
%% their votes. send_prepare will block until
%% it recieves a reply from the resource.
%%------------------------------------------------------------

send_prepare(RegResources, Alarm) ->
    send_prepare(RegResources, [], Alarm).

% All voted ReadOnly. We are done.
send_prepare([], [], _) -> 
    readOnly;

% All voted commit (VC) or ReadOnly.
send_prepare([], VC, Alarm) -> 
    case catch try_timeout(Alarm) of
	false -> 
	    {commit, VC};
	_-> 
	    {rollback, VC}
    end;

send_prepare([Rhead|Rtail], VC, Alarm) ->
    ?debug_print("send_prepare()~n",[]),
    case catch 'CosTransactions_Resource':prepare(Rhead) of
	'VoteCommit' ->
	    case catch try_timeout(Alarm) of
		false -> 
		    _Env = ?get_debug_data(self),
		    ?eval_debug_fun({?tr_get_etrap(_Env), send_prepare}, _Env),
		    send_prepare(Rtail, VC++[Rhead], Alarm);
		_-> 
		    %% Timeout, rollback. However, the resource did vote 
		    %% commit. Add it to the list.
		    send_info(Rtail, 'CosTransactions_Resource', rollback),
		    {rollback, VC++[Rhead]}
	    end;
	'VoteRollback' ->
	    %% Don't care about timeout since we voted rollback.
	    %% A rollback received. No need for more prepare-calls.
	    %% See OMG 10-51, Transaction Service:v1.0 
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    {rollback, VC};
	'VoteReadOnly' ->
	    case catch try_timeout(Alarm) of
		false ->
		    send_prepare(Rtail, VC, Alarm);
		_-> 
		    %% timeout, reply rollback.
		    send_info(Rtail, 'CosTransactions_Resource', rollback),
		    {rollback, VC}
	    end;
	{'EXCEPTION',E} when is_record(E, 'TIMEOUT') ->
	    ?tr_error_msg("Coordinator:prepare( ~p )~nObject unreachable.~n",
			  [Rhead]),
	    %% Since we use presumed abort we will rollback the transaction.
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    {rollback, VC};
	{'EXCEPTION',E} when is_record(E, 'TRANSIENT') ->
	    ?tr_error_msg("Coordinator:prepare( ~p )~nObject unreachable.~n",
			  [Rhead]),
	    %% Since we use presumed abort we will rollback the transaction.
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    {rollback, VC};
	{'EXCEPTION',E} when is_record(E, 'COMM_FAILURE') ->
	    ?tr_error_msg("Coordinator:prepare( ~p )~nObject unreachable.~n",
			  [Rhead]),
	    %% Since we use presumed abort we will rollback the transaction.
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    {rollback, VC};
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    ?tr_error_msg("Coordinator:prepare( ~p )~nObject unreachable.~n",
			  [Rhead]),
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    {rollback, VC};
	{'EXCEPTION', Exc} ->
	    ?tr_error_msg("Coordinator:prepare( ~p )~nThe Object raised exception: ~p~n", 
			  [Rhead, Exc]),
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    %% This can occur if a subtransaction get one or more 
	    %% "VoteCommit" followed by a "VoteRollback". 
	    %% The subtransaction then do a send_decision(rollback),
	    %% which can generate Heuristic decisions. Must rollback 
	    %% since at least one participant voted rollback.
	    {'EXCEPTION', Exc, VC, Rhead}; 
	Other ->
	    ?tr_error_msg("Coordinator:prepare( ~p ) failed. REASON ~p~n", 
			  [Rhead, Other]),
	    send_info(Rtail, 'CosTransactions_Resource', rollback),
	    {failed, VC}
    end.

%%-----------------------------------------------------------%
%% function : type_check
%% Arguments: Bool - perform typecheck?
%%            ID   - Type it should be.
%%            Func - Name of the function (for error_msg)
%%            Obj  - objectrefernce to test.
%% Returns  : 'ok' or raises exception.
%% Effect   : 
%%------------------------------------------------------------
type_check(false, _, _, _) ->
    ok;
type_check(_, ID, Func, Obj) ->
    case catch corba_object:is_a(Obj,ID) of
	true ->
	    ok;
	_ ->
	    ?tr_error_msg("~p( ~p )  Bad argument!!~n", [Func, Obj]),
	    corba:raise(?tr_badparam)
    end.

%%-----------------------------------------------------------%
%% function : is_heuristic
%% Arguments: Exception
%% Returns  : boolean
%% Effect   : Returns true if the exception is a heuristic exc.
%%------------------------------------------------------------

is_heuristic(E) when is_record(E, 'CosTransactions_HeuristicMixed')    -> true;
is_heuristic(E) when is_record(E, 'CosTransactions_HeuristicHazard')   -> true;
is_heuristic(E) when is_record(E, 'CosTransactions_HeuristicCommit')   -> true;
is_heuristic(E) when is_record(E, 'CosTransactions_HeuristicRollback') -> true;
is_heuristic(_)            -> false.

%%-----------------------------------------------------------%
%% function : exception_set
%% Arguments: Genserver state
%% Returns  : 
%% Effect   : Used when restarting.
%%------------------------------------------------------------

exception_set({Env,Local}) ->
    case ?etr_get_exc(Local) of
	void ->
	    self() ! {suicide, self()},
	    {ok, {Env, Local}};
	_ ->
	    {ok, {Env, Local}}
    end.

%%-----------------------------------------------------------%
%% function : set_exception
%% Arguments: Locally defined #exc{}
%%            Heuristic mixed or hazard Exeption
%% Returns  : Altered locally defined #exc{}
%% Effect   : Set the correct tuple member to true.
%%------------------------------------------------------------

set_exception(Exc, E) when is_record(E, 'CosTransactions_HeuristicMixed')  -> 
    Exc#exc{mixed  = true};
set_exception(Exc, E) when is_record(E, 'CosTransactions_HeuristicHazard') -> 
    Exc#exc{hazard = true};
set_exception(Exc, _)            -> Exc.

%%-----------------------------------------------------------%
%% function : send_forget 
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

send_forget([], _) -> ok;
send_forget([Rhead|Rtail], LogName) ->
    ?debug_print("send_forget()~n",[]),
    _Env = ?get_debug_data(self),
    case catch 'CosTransactions_Resource':forget(Rhead) of
	ok ->
	    ?eval_debug_fun({?tr_get_etrap(_Env), send_forget1}, _Env),
	    ?etr_log(LogName, {forgotten, Rhead}),
	    ?eval_debug_fun({?tr_get_etrap(_Env), send_forget2}, _Env),
	    send_forget(Rtail, LogName);
	Other ->
	    ?tr_error_msg("CosTransactions_Coordinator failed sending forget to  ~p~nREASON: ~p~n",
			  [Rhead, Other]),
	    ?eval_debug_fun({?tr_get_etrap(_Env), send_forget3}, _Env),
	    ?etr_log(LogName, {not_forgotten, Rhead}),
	    ?eval_debug_fun({?tr_get_etrap(_Env), send_forget4}, _Env),
	    send_forget(Rtail, LogName)
    end.

%%-----------------------------------------------------------%
%% function : send_decision 
%% Arguments: List of registred resources which vote commit.
%%            Vote - the outcome of the transaction.
%% Returns  : ok - equal to void
%% Effect   : Inform those who voted commit of the outcome.
%% They who voted rollback already knows the outcome.
%% They who voted ReadOnly are not affected.
%%------------------------------------------------------------

%%-- Adding extra parameters
send_decision({Env, Local}, Reply, Vote) ->
    send_decision({Env, Local}, Reply, ?etr_get_vc(Local), Vote, #exc{}, [], 0).
send_decision({Env, Local}, Reply, Vote, VC) ->
    send_decision({Env, Local}, Reply, VC, Vote, #exc{}, [], 0).
send_decision(State, no_reply, VC, Vote, Exc) ->
    send_decision(State, no_reply, VC, Vote, Exc, [], 0).

%%-- Decision sent to all members. Do not reply (used when restarting).
send_decision({Env, Local}, no_reply, [], _, #exc{mixed = true}, [], _) -> 
    {Env, ?etr_set_exc(Local, ?tr_mixed)};
send_decision({Env, Local}, no_reply, [], _, #exc{hazard = true}, [], _) -> 
    {Env, ?etr_set_exc(Local, ?tr_hazard)};
send_decision({Env, Local}, no_reply, [], _, _, [], _) -> 
    {Env, Local};
send_decision({Env, Local}, no_reply, [], Vote, Exc, Failed, Times) -> 
    case ?tr_get_maxR(Env) of
	Times ->
	    ?tr_error_msg("MAJOR ERROR, failed sending commit decision to: ~p. Tried ~p times.", [Failed,Times]),
	    {Env, ?etr_set_exc(Local, ?tr_hazard)};
	_->
	    timer:sleep(?tr_get_maxW(Env)),
	    NewTimes = Times+1,
	    send_decision({Env, Local}, no_reply, Failed, Vote, Exc, [], NewTimes)
    end;
%%-- end special cases.

%% Decision sent to all members. Test exceptions.
send_decision({Env, Local}, Reply, [], Vote, Exc, [], _) -> 
    notify_subtrAware(Vote, ?etr_get_subAw(Local), ?etr_get_self(Local)),
    test_exc(Exc, Vote, Reply, {Env, Local});
%% Decision not sent to all members (one or more failed). Retry.
send_decision({Env, Local}, Reply, [], Vote, Exc, Failed, Times) -> 
    case ?tr_get_maxR(Env) of
	Times ->
	    ?tr_error_msg("MAJOR ERROR, failed sending commit decision to: ~p. Tried ~p times.", [Failed,Times]),
	    notify_subtrAware(Vote, ?etr_get_subAw(Local), ?etr_get_self(Local)),
	    test_exc(Exc#exc{hazard = true}, Vote, Reply, {Env, Local});
	_->
	    NewTimes = Times+1,
	    timer:sleep(?tr_get_maxW(Env)),
	    send_decision({Env, Local}, Reply, Failed, Vote, Exc, [], NewTimes)
    end;

send_decision({Env, Local}, Reply, [Rhead|Rtail], Vote, Exc, Failed, Times) ->
    ?debug_print("Coordinator:send_decision(~p) Try: ~p~n",[Vote, Times]),
    case catch 'CosTransactions_Resource':Vote(Rhead) of
        ok ->
	    ?etr_log(?tr_get_etrap(Env),{sent, Rhead}),
            send_decision({Env, Local}, Reply, Rtail, Vote, Exc, Failed, Times);
        {'EXCEPTION', E} when Vote == commit andalso
			      is_record(E, 'CosTransactions_NotPrepared') ->
	    ?debug_print("send_decision resource unprepared~n",[]),
            case catch 'CosTransactions_Resource':prepare(Rhead) of
                'VoteCommit' ->
                    send_decision({Env, Local}, Reply, [Rhead|Rtail], Vote, Exc, Failed, Times);
		'VoteRollback' ->
                    send_decision({Env, Local}, Reply, Rtail, Vote, Exc#exc{mixed = true}, Failed, Times);
		{'EXCEPTION', E} ->
		    {SetExc, NewL, DidFail} = 
			evaluate_answer(E, Rhead, Vote, Exc, 
					?tr_get_etrap(Env), Local),
		    send_decision({Env, NewL}, Reply, Rtail, Vote, SetExc, DidFail++Failed, Times)
            end;
	{'EXCEPTION', E} ->
	    {SetExc, NewL, DidFail} = 
		evaluate_answer(E, Rhead, Vote, Exc, ?tr_get_etrap(Env), Local),
	    ?tr_error_msg("Resource:~p( ~p )~nRaised Exception: ~p~n",
			  [Vote, Rhead, E]),
	    send_decision({Env, NewL}, Reply, Rtail, Vote, SetExc, DidFail++Failed, Times);
	{'EXIT', _} ->
	    send_decision({Env, Local}, Reply, Rtail, 
			  Vote, Exc, [Rhead|Failed], Times);
	Other ->
	    ?tr_error_msg("Resource:~p( ~p ) failed.~nREASON: ~p~n", 
			  [Vote, Rhead, Other]),
	    case catch corba_object:non_existent(Rhead) of
		true when Vote == commit ->
		    %% Presumed rollback
		    send_decision({Env, Local}, Reply, Rtail, Vote, 
				  Exc#exc{mixed = true}, Failed, Times);
		true ->
		    %% Presumed rollback
		    send_decision({Env, Local}, Reply, Rtail, Vote, 
				  Exc#exc{hazard = true}, Failed, Times);
		_ ->
		    send_decision({Env, Local}, Reply, Rtail, 
				  Vote, Exc, [Rhead|Failed], Times)
	    end
    end.

%%-----------------------------------------------------------%
%% function : notify_subtrAware, 
%% Arguments: 
%% Returns  : 
%% Effect   : Invoke an operation on a list of objects. We don't
%%            care about return values or exceptions.
%%------------------------------------------------------------

notify_subtrAware(commit, Resources, Self) ->
    send_info(Resources, Self, 
	      'CosTransactions_SubtransactionAwareResource',
	      commit_subtransaction);
notify_subtrAware(_, Resources, _) ->
    send_info(Resources, 'CosTransactions_SubtransactionAwareResource',
	      rollback_subtransaction).

%%-----------------------------------------------------------%
%% function : send_info
%% Arguments: ObjectList - List of object refernces to call.
%%            M - Module
%%            F - Function
%%            (Arg - required arguments)
%% Returns  : ok
%% Effect   : A lightweight function to be used when we don't
%%            "care" about the return value.
%%------------------------------------------------------------

send_info([], _, _, _) ->
    ok;
send_info([Rhead|Rtail], Arg, M, F) ->
    ?debug_print("~p( ~p )~n",[F, Arg]),
    case catch M:F(Rhead, Arg) of
	{'EXIT',R} ->
	    ?tr_error_msg("~p:~p(~p, ~p) returned {'EXIT',~p}", [M,F,Rhead,Arg,R]);
	{'EXCEPTION',E} ->
	    ?tr_error_msg("~p:~p(~p, ~p) returned {'EXCEPTION',~p}", [M,F,Rhead,Arg,E]);
	_->
	    ok
    end,
    send_info(Rtail, Arg, M, F).

send_info([], _, _) -> 
    ok;
send_info([Rhead|Rtail], M, F) ->
    ?debug_print("~p( )~n",[F]),
    case catch M:F(Rhead) of
	{'EXIT',R} ->
	    ?tr_error_msg("~p:~p(~p) returned {'EXIT',~p}", [M,F,Rhead,R]);
	{'EXCEPTION',E} ->
	    ?tr_error_msg("~p:~p(~p) returned {'EXCEPTION',~p}", [M,F,Rhead,E]);
	_->
	    ok
    end,
    send_info(Rtail, M, F).
    
%%-----------------------------------------------------------%
%% function : evaluate_answer
%% Arguments: 
%% Returns  : 
%% Effect   : Check what kind of exception we received.
%%------------------------------------------------------------

evaluate_answer(E, Rhead, _Vote, Exc, Log, Local) 
  when is_record(E, 'CosTransactions_HeuristicMixed') ->
    ?etr_log(Log, {heuristic, {Rhead, E}}),
    {Exc#exc{mixed = true}, ?etr_add_raisedH(Local, Rhead), []};
evaluate_answer(E, Rhead, _Vote, Exc, Log, Local)
  when is_record(E, 'CosTransactions_HeuristicHazard') ->
    ?etr_log(Log, {heuristic, {Rhead, E}}),
    {Exc#exc{hazard = true}, ?etr_add_raisedH(Local, Rhead), []};
evaluate_answer(E, Rhead, Vote, Exc, Log, Local)
  when is_record(E, 'CosTransactions_HeuristicCommit') ->
    case Vote of
	commit ->
	    ?etr_log(Log, {heuristic, {Rhead, E}}),
	    {Exc, ?etr_add_raisedH(Local, Rhead), []};
	_->
	    ?etr_log(Log, {heuristic, {Rhead, ?tr_mixed}}),
	    {Exc#exc{mixed = true}, ?etr_add_raisedH(Local, Rhead), []}
    end;
evaluate_answer(E, Rhead, Vote, Exc, Log, Local) 
  when is_record(E, 'CosTransactions_HeuristicRollback')->
    case Vote of
	rollback ->
	    ?etr_log(Log, {heuristic, {Rhead, ?tr_rollback}}),
	    {Exc, ?etr_add_raisedH(Local, Rhead), []};
	_->
	    ?etr_log(Log, {heuristic, {Rhead, ?tr_mixed}}),
	    {Exc#exc{mixed = true}, ?etr_add_raisedH(Local, Rhead), []}
    end;
evaluate_answer(E, Rhead, Vote, Exc, Log, Local) 
  when Vote == commit andalso is_record(E, 'TRANSACTION_ROLLEDBACK') ->
    ?etr_log(Log, {heuristic, {Rhead, ?tr_mixed}}),
    {Exc#exc{mixed = true}, ?etr_add_raisedH(Local, Rhead), []};
evaluate_answer(E, Rhead, Vote, Exc, Log, Local) when is_record(E, 'TIMEOUT') ->
    ?tr_error_msg("Coordinator:~p( ~p ) Object unreachable.~nReason: ~p~n",
		  [Vote, Rhead, E]),
    case catch corba_object:non_existent(Rhead) of
	true ->
	    %% Since we have presumed abort, the child will
	    %% assume rollback if this server do not exist any more.
	    ?etr_log(Log, {heuristic, {Rhead, ?tr_hazard}}),
	    {Exc#exc{hazard = true}, Local, []};
	_ ->
	    {Exc, Local, [Rhead]}
    end;
evaluate_answer(E, Rhead, Vote, Exc, Log, Local) when is_record(E, 'TRANSIENT') ->
    ?tr_error_msg("Coordinator:~p( ~p ) Object unreachable.~nReason: ~p~n",
		  [Vote, Rhead, E]),
    case catch corba_object:non_existent(Rhead) of
	true ->
	    %% Since we have presumed abort, the child will
	    %% assume rollback if this server do not exist any more.
	    ?etr_log(Log, {heuristic, {Rhead, ?tr_hazard}}),
	    {Exc#exc{hazard = true}, Local, []};
	_ ->
	    {Exc, Local, [Rhead]}
    end;
evaluate_answer(E, Rhead, Vote, Exc, Log, Local) when is_record(E, 'COMM_FAILURE') ->
    ?tr_error_msg("Coordinator:~p( ~p ) Object unreachable.~nReason: ~p~n",
		  [Vote, Rhead, E]),
    case catch corba_object:non_existent(Rhead) of
	true ->
	    %% Since we have presumed abort, the child will
	    %% assume rollback if this server do not exist any more.
	    ?etr_log(Log, {heuristic, {Rhead, ?tr_hazard}}),
	    {Exc#exc{hazard = true}, Local, []};
	_ ->
	    {Exc, Local, [Rhead]}
    end;
evaluate_answer(E, Rhead, Vote, Exc, Log, Local)when is_record(E, 'OBJECT_NOT_EXIST') ->
    ?tr_error_msg("Coordinator:~p( ~p ) Object unreachable.~nReason: ~p~n",
		  [Vote, Rhead, E]),
    %% Since we have presumed abort, the child will
    %% assume rollback if this server do not exist any more.
    ?etr_log(Log, {heuristic, {Rhead, ?tr_hazard}}),
    {Exc#exc{hazard = true}, Local, []};
evaluate_answer(Unknown, Rhead, Vote, Exc, _Log, Local)->
    ?tr_error_msg("Coordinator:~p( ~p ). Unknown reply: ~p.~n", 
		  [Vote, Rhead, Unknown]),
    {Exc, Local, []}.


%%-----------------------------------------------------------%
%% function : test_exc
%% Arguments: Exc   - instance of #exc{} locally declared.
%%            Vote  - 'rollback' or 'commit'
%%            Reply - If no exceptions this is the default reply.
%%            State - genserver state
%% Returns  : 
%% Effect   : Raise the correct exception or simply reply to
%%            the genserver. NOTE that the testing for exceptions
%%            differs if we are performing a rollback or commit.
%%            Check if Mixed first; takes priority over Hazard.
%%            HeuristicRollback and VoteCommit together give 
%%            HeuristicMixed
%%            HeuristicCommit and VoteRollback together give
%%            HeuristicMixed
%%------------------------------------------------------------

test_exc(#exc{mixed = true}, _, _, {Env, Local}) ->
    {reply, {'EXCEPTION', ?tr_mixed}, {Env, ?etr_set_exc(Local, ?tr_mixed)}};
% Left out for now to avoid dialyzer warning.
%test_exc(#exc{rollback = true}, commit, _, {Env, Local}) ->
%    {reply, {'EXCEPTION', ?tr_mixed}, {Env, ?etr_set_exc(Local, ?tr_mixed)}};
% Left out for now to avoid dialyzer warning.
%test_exc(#exc{commit = true}, rollback, _, {Env, Local}) ->
%    {reply, {'EXCEPTION', ?tr_mixed}, {Env, ?etr_set_exc(Local, ?tr_mixed)}};
test_exc(#exc{hazard = true}, _, _, {Env, Local}) ->
    {reply, {'EXCEPTION', ?tr_hazard}, {Env, ?etr_set_exc(Local, ?tr_hazard)}};
test_exc(_, _, {'EXCEPTION', E}, {Env, Local}) 
  when is_record(E, 'TRANSACTION_ROLLEDBACK')->
    {stop, normal, {'EXCEPTION', E}, {Env, Local}};
%% Replace the case above if allow synchronization
%test_exc(_, _, {'EXCEPTION', E}, {Env, Local}) 
%  when record(E, 'TRANSACTION_ROLLEDBACK')->
%    case ?etr_get_sync(Local) of
%	[] ->
%	    {stop, normal, {'EXCEPTION', E}, {Env, Local}};
%	_->
%	    {reply, {'EXCEPTION', E}, {Env, Local}}
%    end;
test_exc(_, _, {'EXCEPTION', E}, State) ->
    {reply, {'EXCEPTION', E}, State};
test_exc(_, _, Reply, {Env, Local}) ->
    {stop, normal, Reply, {Env, Local}}.
%% Replace the case above if allow synchronization
%test_exc(_, _, Reply, {Env, Local}) ->
%    case ?etr_get_sync(Local) of
%	[] ->
%	    {stop, normal, Reply, {Env, Local}};
%	_ ->
%	    {reply, Reply, {Env, Local}}
%    end.
	
%%-----------------------------------------------------------%
%% function : evaluate_status
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

evaluate_status(Status) ->
 case Status of
     'StatusCommitted'      -> commit;
     'StatusCommitting'     -> commit;
     'StatusMarkedRollback' -> rollback;
     'StatusRollingBack'    -> rollback;
     'StatusRolledBack'     -> rollback;
     'StatusActive'         -> rollback;
     'StatusPrepared'       -> rollback;
     'StatusUnknown'        -> rollback;
     'StatusNoTransaction'  -> rollback;
     'StatusPreparing'      -> rollback;
     _-> rollback
 end.


%%-----------------------------------------------------------%
%% function : prepare_restart
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

%% The file contains no data. The coordinator crashed before
%% a prepare-call was made. Presumed rollback.
prepare_restart(State, eof) -> 
    ?debug_print("prepare_restart: eof, init~n",[]),
    self() ! {suicide, self()},
    {ok, State};
%% Collected all necessary votes. Do commit_restart.
prepare_restart({Env, _}, {{pre_vote, _Vote, Data}, Cursor}) ->
    ?debug_print("prepare_restart: pre_vote( ~p )~n",[_Vote]),
    if
	?tr_is_root(Env) ->
	    commit_restart({Env, Data}, 
			   ?etr_read(?tr_get_etrap(Env), Cursor), root);
	true ->	
	    commit_restart({Env, Data}, 
			   ?etr_read(?tr_get_etrap(Env), Cursor), subCoord)
    end;
%% 'rollback' called without 'prepare'. This case occurs if the Coordinator
%% crashes when send_info or notify_subtrAware.
prepare_restart({Env, _}, {{rollback, NewL}, _Cursor}) ->
    ?debug_print("prepare_restart: pre_vote( rollback )~n",[]),
    send_info(?etr_get_members(NewL), 'CosTransactions_Resource', rollback),
    notify_subtrAware(rollback, ?etr_get_subAw(NewL), ?etr_get_self(NewL)),
    self() ! {suicide, self()},
    {ok, {Env, NewL}};
%% Something is wrong in the log.
prepare_restart(_, _) ->
    ?tr_error_msg("Internal log read failed:~n", []),
    {stop, {error, "restart failed"}}.

%%-----------------------------------------------------------%
%% function : commit_restart
%% Arguments: Env - server context
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
commit_restart({Env, Local}, Data, Phase) ->
    Exc = set_exception(#exc{}, ?etr_get_exc(Local)),
    commit_restart({Env, Local}, Data, Phase, Exc).

%% Normal case. No errors no exceptions.
commit_restart({Env, Local}, {{sent, Obj}, Cursor}, Vote, Exc) ->
    ?debug_print("commit_restart: sent~n",[]),
    commit_restart({Env, ?etr_remove_vc(Local, Obj)}, 
		   ?etr_read(?tr_get_etrap(Env), Cursor), Vote, Exc);
commit_restart({Env, Local}, {{heuristic, {Obj,E}}, Cursor}, Vote, Exc) ->
    ?debug_print("commit_restart: heuristic    ~p~n",[E]),
    NewExc = set_exception(Exc, E),
    commit_restart({Env, ?etr_add_raisedH(Local, Obj)},
		   ?etr_read(?tr_get_etrap(Env), Cursor), Vote, NewExc);


%% --- cases which only can occure once in the log ------------

%% The file contains no data. The coordinator crashed before
%% a decision was made. Causes rollback.
commit_restart({E, L}, eof, root, Exc) -> 
    ?debug_print("commit_restart: eof init (root only)~n",[]),
    {Env, Local} = send_decision({E, L}, no_reply, ?etr_get_vc(L), 
				 rollback, Exc),
    exception_set({Env, Local});
%% Replace the reply above if allow synchronization
%    case ?etr_get_sync(Local) of
%	[] ->
%	    exception_set({Env, Local});
%	SynchObjs ->
%	    {ok, {Env, Local}}
%    end;


%% Passed the prepare_restart. Not received a commit decision from the
%% parent.
commit_restart({E, L}, eof, subCoord, Exc) -> 
    ?debug_print("commit_restart: eof init (subcoord only)~n",[]),
    case catch corba_object:non_existent(?tr_get_parent(E)) of
	true ->
	    %% Presumed rollback.
	    {Env, Local} = send_decision({E, L}, no_reply, ?etr_get_vc(L), 
					 rollback, Exc),
	    self() ! {suicide, self()},
	    {ok, {Env, Local}};
%% Replace the reply above if allow synchronization
%	    case ?etr_get_sync(Local) of
%		[] ->
%		    self() ! {suicide, self()},
%		    {ok, {Env, Local}};
%		SynchObjs ->
%		    case ?tr_get_parents(Env) of
%			[] ->
%			    send_info(SynchObjs, ?etr_get_status(Local),
%				      'CosTransactions_Synchronization', after_completion);
%			_->
%			    ok
%		    end,
%		    self() ! {suicide, self()},
%		    {ok, {Env, Local}}
%	    end;
	_->
	    {ok, {E, L}}
    end;

commit_restart({Env, Local}, eof, Vote, Exc) -> 
    ?debug_print("commit_restart: eof    VOTE: ~p~n",[Vote]),
    case ?etr_get_vc(Local) of
	[] ->
	    ?debug_print("commit_restart: all sent, test exc~n",[]),
            exception_set({Env, Local});
	VC ->
	    ?debug_print("commit_restart: note done. send more~n",[]),
	    State = send_decision({Env, Local}, no_reply, VC, Vote, Exc),
	    exception_set(State)
    end;

%% Decision made, i.e. rollback or commit.
commit_restart({Env, Local}, {rollback, Cursor}, _Phase, Exc) ->
    ?debug_print("commit_restart: decided rollback~n",[]),
    commit_restart({Env, ?etr_set_status(Local, 'StatusRolledBack')}, 
		   ?etr_read(?tr_get_etrap(Env), Cursor), rollback, Exc);
commit_restart({Env, Local}, {commit, Cursor}, _Phase, Exc) ->
    ?debug_print("commit_restart: decided commit~n",[]),
    commit_restart({Env, ?etr_set_status(Local, 'StatusCommitted')}, 
		   ?etr_read(?tr_get_etrap(Env), Cursor), commit, Exc);
commit_restart({Env, Local}, {forget_phase, Cursor}, _, _) ->
    ?debug_print("commit_restart: start sending forget~n",[]),
    forget_restart({Env, Local}, ?etr_read(?tr_get_etrap(Env), Cursor));

commit_restart({_Env, _Local}, _R, _, _) ->
    ?debug_print("RESTART FAIL: ~p~n",[_R]),
    ?tr_error_msg("Internal log read failed:~n", []),
    exit("restart failed").

%%-----------------------------------------------------------%
%% function : forget_restart
%% Arguments: {Env, Local} - server context
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

%% Exception logged. Test if we issued a 'forget()' to the Resource.
forget_restart({Env, Local}, eof) ->
    case ?etr_get_raisedH(Local) of
	[] ->
	    ?debug_print("forget_restart: all done~n",[]);
	Left ->
	    ?debug_print("forget_restart: not done. send more~n",[]),
	    send_forget(Left, ?tr_get_etrap(Env))
    end,
    self() ! {suicide, self()},
    {ok, {Env, Local}};
%% Replace the reply above if allow synchronization
%    case ?etr_get_sync(Local) of
%	[] ->
%	    self() ! {suicide, self()},
%	    {ok, {Env, Local}};
%	SynchObjs ->
%	    case ?tr_get_parents(Env) of
%		[] ->
%		    send_info(SynchObjs, ?etr_get_status(Local),
%			      'CosTransactions_Synchronization', after_completion),
%		    self() ! {suicide, self()},
%		    {ok, {Env, Local}};
%		_->
%		    {ok, {Env, Local}}
%	    end
%    end;
forget_restart({Env, Local}, {{forgotten, Obj}, Cursor}) ->
    ?debug_print("forget_restart: forgotten  heuristic~n",[]),
    NewL = ?etr_remove_raisedH(Local, Obj),
    forget_restart({Env, NewL}, ?etr_read(?tr_get_etrap(Env), Cursor));
forget_restart({Env, Local}, {{not_forgotten, Obj}, Cursor}) ->
    ?debug_print("forget_restart: not_forgotten~n",[]),
    NewL = ?etr_remove_raisedH(Local, Obj),
    send_forget([Obj], dummy),
    forget_restart({Env, NewL}, ?etr_read(?tr_get_etrap(Env), Cursor)).

%%--------------- END OF MODULE ------------------------------
