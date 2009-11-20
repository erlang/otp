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
%% File    : CosTransactions_TransactionFactory_impl.erl
%% Purpose : Is provided to allow the transaction originator to begin
%%           a transaction.
%%----------------------------------------------------------------------

-module('CosTransactions_TransactionFactory_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").

%% Local
-include_lib("ETraP_Common.hrl").
-include_lib("CosTransactions.hrl").
%%--------------- IMPORTS-------------------------------------
-import('ETraP_Common', [get_option/3]).

%%--------------- EXPORTS-------------------------------------
-export([create/3, recreate/3, init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%--------------- LOCAL DATA ---------------------------------
-record(factory, {hashMax, subtrOK, typeCheck, maxRetries, comFailWait}).

%%--------------- LOCAL DEFINITIONS --------------------------

%%------------------------------------------------------------
%% function : init
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the module ic. Used to initiate
%%            a gen_server.
%%------------------------------------------------------------

init(Options) when is_list(Options) ->
    ?debug_print("Factory:init(~p)~n", [Options]),
    process_flag(trap_exit,true),
    DefaultValues  = [{maxRetries, ?tr_max_retries},
		     {comFailWait, ?tr_comm_failure_wait}|?tr_FAC_DEF],
    Hash           = get_option(hash_max, Options, DefaultValues),
    SubtrOK        = get_option(allow_subtr, Options, DefaultValues),
    TypeCheck      = get_option(typecheck, Options, DefaultValues),
    TTY            = get_option(tty, Options, DefaultValues),
    LogFile        = get_option(logfile, Options, DefaultValues),
    MaxRetries     = get_option(maxRetries, Options, DefaultValues),
    ComFailWait    = get_option(comFailWait, Options, DefaultValues),
    error_logger:tty(TTY),
    case LogFile of
	false ->
	    ok;
	_->
	    error_logger:logfile({open, LogFile})
    end,
    {ok, #factory{typeCheck = TypeCheck, hashMax = Hash, subtrOK = SubtrOK,
		  maxRetries = MaxRetries, comFailWait = ComFailWait}};

init(Options) ->
    ?tr_error_msg("TransactionFactory~nBad argument: ~p~n", [Options]),
    corba:raise(?tr_badparam).
    

%%------------------------------------------------------------
%% function : terminate
%% Arguments: 
%% Returns  : 
%% Effect   : Function demanded by the module ic. Used to 
%%            terminate a gen_server.
%%------------------------------------------------------------

terminate(_Reason, _State) ->
    ?debug_print("Factory:terminate(~p)~n", [_Reason]),
    ok.

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
    

handle_info({'EXIT',_From,shutdown}, State) ->
    ?debug_print("Factory:handle_info(~p)~n", [shutdown]),
    {stop, shutdown, State};
handle_info(_Info, State) ->
    ?debug_print("Factory:handle_info(~p)~n", [_Info]),
    {noreply, State}.

%%------------------------------------------------------------
%% function : create
%% Arguments: TimeOut - rollback the transaction after TimeOut 
%%            seconds. If 0 no timeout.
%% Returns  : a Control object
%% Effect   : Creates a new top-level transaction. The Control
%%            can be used to manage or control participation
%%            in the new transaction. Used for direct context
%%            management.
%%------------------------------------------------------------

create(_Self, State, TimeOut) when is_integer(TimeOut) ->
    %% Generate objectnames.
    ETraPName    = 'ETraP_Common':create_name("root"),
    TermName     = 'ETraP_Common':create_name("term"),
    EState       = ?tr_create_context(ETraPName, TermName,
				      State#factory.typeCheck, 
				      State#factory.hashMax, 
				      State#factory.subtrOK,
				      State#factory.maxRetries,
				      State#factory.comFailWait),

    case TimeOut of
	0 ->
	    ETraP = ?tr_start_child(?SUP_ETRAP(EState)),
	    {reply, ETraP, State};
	_ ->
	    if
		TimeOut > 0 ->
		    {MegaSecs, Secs, _Microsecs} = erlang:now(),
		    EState2 = ?tr_set_alarm(EState, MegaSecs*1000000+Secs+TimeOut),
		    EState3 = ?tr_set_timeout(EState2, TimeOut*1000),
		    ETraP = ?tr_start_child(?SUP_ETRAP(EState3)),
		    {reply, ETraP, State};
		true ->
		    ?tr_error_msg("TransactionFactory:create( Integer >= 0 )~nBad argument. Not an integer.~n", []),
		    corba:raise(?tr_badparam)
	    end
    end;

create(_Self, _State, _TimeOut) ->
    ?tr_error_msg("TransactionFactory:create( Integer >= 0 )~nBad argument. Not an integer.~n", []),
    corba:raise(?tr_badparam).


%%------------------------------------------------------------
%% function : recreate
%% Arguments: PropagationContext
%% Returns  : a Control object
%% Effect   : 
%%------------------------------------------------------------

recreate(_Self, _State, #'CosTransactions_PropagationContext'{current = _C}) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_YES}).
%recreate(Self, State, #'CosTransactions_PropagationContext'{current = C}) ->
%    {reply, C#'CosTransactions_TransIdentity'.coord, State}.


%%--------------- END OF MODULE ------------------------------

