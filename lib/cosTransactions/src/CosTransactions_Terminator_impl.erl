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
%%----------------------------------------------------------------------
%% File    : CosTransactions_Terminator_impl.erl
%% Purpose : Support operations to commit or roll-back a transaction.
%%----------------------------------------------------------------------

-module('CosTransactions_Terminator_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
%% Local
-include_lib("ETraP_Common.hrl").
-include_lib("CosTransactions.hrl").

%%--------------- IMPORTS-------------------------------------
-import(etrap_logmgr, [log_safe/2, get_next/2]).

%%--------------- EXPORTS-------------------------------------
%%-compile(export_all).
-export([commit/3, rollback/2]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%--------------- LOCAL DATA ---------------------------------
%-record(terminator, {reg_resources, rollback_only, regname, coordinator}).

%%------------------------------------------------------------
%% function : init, terminate
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the module ic. Used to initiate
%%            and terminate a gen_server.
%%------------------------------------------------------------

init(State) ->
    process_flag(trap_exit,true),
    case catch start_object(State) of
	{'EXIT', Reason} ->
	    %% Happens when, for example, we encounter an 
	    %% error when reading from the log file.
	    {stop, Reason};
	Other ->
	    Other
    end.

start_object(State) ->
    case catch file:read_file_info(?tr_get_terminator(State)) of
	{error, enoent} -> 
	    %% File does not exist. It's the first time. No restart.
	    ?debug_print("Terminator:init(~p)~n", [?tr_get_terminator(State)]),
	    etrap_logmgr:start(?tr_get_terminator(State)),
	    {ok, State, ?tr_get_timeout(State)};
	{error, Reason} -> % File exist but error occurred.
	    ?tr_error_msg("CosTransactions_Terminator( ~p ) Cannot open log file: ~p~n",
			  [?tr_get_terminator(State), Reason]),
	    {stop, {error, "unable_to_open_log"}};
	_ -> % File exists, perform restart.
	    etrap_logmgr:start(?tr_get_terminator(State)),
	    ?debug_print("RESTART Terminator:init(~p)~n", 
			 [?tr_get_terminator(State)]),
	    do_restart(State, get_next(?tr_get_terminator(State), start), init)
    end.


terminate(Reason, State) ->
    ?debug_print("STOP ~p   ~p~n", [?tr_get_terminator(State), Reason]),
    case Reason of
	normal -> 
	    %% normal termination. Transaction completed. 
	    log_safe(?tr_get_terminator(State), done),
	    etrap_logmgr:stop(?tr_get_terminator(State)),
	    file:delete(?tr_get_terminator(State)),
	    ok;
	_ ->
	    ok
    end.

%%------------------------------------------------------------
%% function : handle_call, handle_cast, handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the module ic. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_,_, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.
    

handle_info(Info, State) ->
    ?debug_print("Terminator:handle_info(~p)~n", [Info]),
    Pid = self(),
    case Info of
	timeout ->
	    ?tr_error_msg("Object( ~p ) timeout. Rolling back.~n",
			  [?tr_get_terminator(State)]),
            {stop, normal, State};
        {suicide, Pid} ->
            {stop, normal, State};
        _->
            {noreply, State}
    end.

%%------------------------------------------------------------
%% function : commit
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%%            Heuristics - boolean; report heuristic decisions?
%% Returns  : ok - equal to void
%% Effect   : 
%% Exception: HeuristicMixed  - Highest priority
%%            HeuristicHazard - Lowest  priority
%%------------------------------------------------------------

commit(_Self, State, _Heuristics) when ?tr_is_retransmit(State) ->
    ?debug_print("Terminator:commit() recalled.~n", []),
    {stop, normal, ?tr_get_reportH(State), State};
commit(Self, State, Heuristics) ->
    ?debug_print("Terminator:commit() called.~n", []),
    NewState = ?tr_set_reportH(State, Heuristics),
    log_safe(?tr_get_terminator(NewState), {init_commit, NewState}),
    transmit(Self, NewState, Heuristics).


transmit(Self, State, Heuristics) ->
    case catch 'ETraP_Common':try_timeout(?tr_get_alarm(State)) of
        false ->
%	    catch 'ETraP_Server':before_completion(?tr_get_etrap(State)),
	    case catch 'CosTransactions_Resource':prepare(?tr_get_etrap(State)) of
		'VoteCommit' ->
		    evaluate_answer(Self, State, Heuristics, 
				    'ETraP_Common':try_timeout(?tr_get_alarm(State)));
		'VoteRollback' ->
		    {stop, normal, 
		     {'EXCEPTION', 
		      #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
		     State};
		'VoteReadOnly' ->
		    {stop, normal, ok, State};
		{'EXCEPTION', E} when is_record(E,'CosTransactions_HeuristicMixed'),
				      Heuristics==true->
		    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
		    {stop, normal, {'EXCEPTION', E}, State};
		{'EXCEPTION', E} when is_record(E,'CosTransactions_HeuristicHazard'),
				      Heuristics==true->
		    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
		    {stop, normal, {'EXCEPTION', E}, State};
		{'EXCEPTION', E} when is_record(E,'CosTransactions_HeuristicMixed') ->
		    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
		    {stop, normal, 
		     {'EXCEPTION',#'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
		     State};
		{'EXCEPTION', E} when is_record(E,'CosTransactions_HeuristicHazard') ->
		    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
		    {stop, normal, 
		     {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}},
		     State};
		Other ->
		    ?tr_error_msg("Coordinator:prepare( ~p ) failed. REASON ~p~n", 
				  [?tr_get_etrap(State), Other]),
		    {stop, normal, 
		     {'EXCEPTION', 
		      #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
		     State}
	    end;
	_ ->
	    %% Timeout, rollback.
	    log_safe(?tr_get_terminator(State), rolled_back),
	    catch 'ETraP_Server':rollback(?tr_get_etrap(State)),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, 
	     {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
	     State}
    end.

evaluate_answer(Self, State, Heuristics, false) ->
    evaluate_answer(Self, State, Heuristics, commit);
evaluate_answer(Self, State, Heuristics, true) ->
    evaluate_answer(Self, State, Heuristics, rollback);
evaluate_answer(_Self, State, Heuristics, Vote) ->
    case catch 'ETraP_Common':send_stubborn('ETraP_Server', Vote, 
					    ?tr_get_etrap(State), 
					    ?tr_get_maxR(State), 
					    ?tr_get_maxW(State)) of
	ok ->
	    ?eval_debug_fun({_Self, commit_ok1}, State),
	    log_safe(?tr_get_terminator(State), committed),
	    ?eval_debug_fun({_Self, commit_ok2}, State),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusCommitted'),
	    {stop, normal, ok, State};
	{'EXCEPTION', E} when Heuristics == true andalso
			      is_record(E,'CosTransactions_HeuristicMixed') ->
	    log_safe(?tr_get_terminator(State), {heuristic, State, E}),
	    ?eval_debug_fun({_Self, commit_heuristic1}, State),
	    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, {'EXCEPTION', E}, State};
	{'EXCEPTION', E} when Heuristics == true andalso
			      is_record(E, 'CosTransactions_HeuristicHazard') ->
	    log_safe(?tr_get_terminator(State), {heuristic, State, E}),
	    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, {'EXCEPTION', E}, State};
	{'EXCEPTION', E} when is_record(E, 'OBJECT_NOT_EXIST') ->
	    log_safe(?tr_get_terminator(State), rolled_back),
	    {stop, normal, {'EXCEPTION', ?tr_hazard}, State};
	{'EXCEPTION', E} when is_record(E, 'TRANSACTION_ROLLEDBACK') ->
	    log_safe(?tr_get_terminator(State), rolled_back),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, {'EXCEPTION', E}, State};
	{'EXCEPTION', E} when is_record(E, 'CosTransactions_HeuristicCommit') ->
	    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, ok, State};
	{'EXCEPTION', E} when is_record(E, 'CosTransactions_HeuristicRollback') ->
	    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusCommitted'),
	    {stop, normal, 
	     {'EXCEPTION', 
	      #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
	     State};
	_Other when Heuristics == true ->
	    log_safe(?tr_get_terminator(State), rolled_back),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, {'EXCEPTION', ?tr_hazard}, State};
	_Other ->
	    log_safe(?tr_get_terminator(State), rolled_back),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    {stop, normal, ok, State}
    end.

%%-----------------------------------------------------------%
%% function : rollback
%% Arguments: Self  - its own object reference.
%%            State - Gen-Server State
%% Returns  : ok - equal to void
%% Effect   : 
%%------------------------------------------------------------

rollback(_Self, State) ->
    ?debug_print("Terminator:rollback() called.~n", []),
    log_safe(?tr_get_terminator(State), rolled_back),
    catch 'ETraP_Server':rollback(?tr_get_etrap(State)),
    {stop, normal, ok, State}.

%%-----------------------------------------------------------%
%% function : do_restart
%% Arguments: State - server context
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

%% No data in file. Commit never initiated so we rollback (presumed rollback.
do_restart(State, eof, init) -> 
    log_safe(?tr_get_terminator(State), rolled_back),
    catch 'ETraP_Server':rollback(?tr_get_etrap(State)),
%    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),  'StatusRolledBack'),
    self() ! {suicide, self()},
    {ok, State};

do_restart(State, {error, Reason}, _) ->
    ?tr_error_msg("CosTransactions_Terminator (~p) failed. Cannot read log file: ~p~n",
		  [?tr_get_terminator(State), Reason]),
    {stop, Reason};
do_restart(State, eof, Phase) ->
    ?debug_print("Terminator:do_restart(~p)~n", [Phase]),
    case Phase of
	committed ->
	    {ok, ?tr_set_reportH(State, ok)};
	rolled_back ->
	    self() ! {suicide, self()},
	    {ok, State};
        init_commit ->
	    case catch corba_object:non_existent(?tr_get_etrap(State)) of
		true ->
		    self() ! {suicide, self()},
		    {ok, State};
		_->
		    case transmit(false, State, ?tr_get_reportH(State)) of
			{stop, normal, ok, NewState} ->
			    {ok, NewState};
			{stop, normal, 
			 {'EXCEPTION', #'TRANSACTION_ROLLEDBACK'{completion_status=?COMPLETED_YES}}, 
			 NewState} ->
			    self() ! {suicide, self()},
			    {ok, NewState};
			{stop, normal, {'EXCEPTION', Exc}, NewState} ->
			    if
				?tr_dont_reportH(State) ->
				    self() ! {suicide, self()},
				    {ok, NewState};
				true ->
				    {ok, ?tr_set_reportH(NewState, Exc)}
			    end
		    end
	    end;
        {heuristic, Exc} ->
	    catch 'ETraP_Server':forget(?tr_get_etrap(State)),
%	    catch 'ETraP_Server':after_completion(?tr_get_etrap(State),
%						  'StatusRolledBack'),
	    if
		?tr_dont_reportH(State) ->
		    self() ! {suicide, self()},
		    {ok, State};
		true ->
		    {ok, ?tr_set_reportH(State, {'EXCEPTION',Exc})}
	    end
    end;
%% All done.
do_restart(State, {done, _Cursor}, _Phase) ->
    ?debug_print("Terminator:do_restart(~p)~n", [_Phase]),
    self() ! {suicide, self()},
    {ok, State};
do_restart(State, {rolled_back, Cursor}, _Phase) ->
    ?debug_print("Terminator:do_restart(~p)~n", [_Phase]),
    do_restart(State, get_next(?tr_get_terminator(State), Cursor), rolled_back);
do_restart(State, {committed, Cursor}, _Phase) ->
    ?debug_print("Terminator:do_restart(~p)~n", [_Phase]),
    do_restart(State, get_next(?tr_get_terminator(State), Cursor), committed);
do_restart(State, {{heuristic, SavedState, Exc}, Cursor}, _Phase) ->
    ?debug_print("Terminator:do_restart(~p)~n", [_Phase]),
    do_restart(SavedState, get_next(?tr_get_terminator(State), Cursor), 
	       {heuristic, Exc});
do_restart(State, {{init_commit, SavedState}, Cursor}, _) ->
    ?debug_print("Terminator:do_restart(~p)~n", [init_commit]),
    do_restart(SavedState, get_next(?tr_get_terminator(State), Cursor), init_commit).

%%--------------- END OF MODULE ------------------------------
