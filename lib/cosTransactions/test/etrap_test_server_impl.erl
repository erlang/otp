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
-module(etrap_test_server_impl).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Local
-include_lib("cosTransactions/src/ETraP_Common.hrl").
-include_lib("cosTransactions/include/CosTransactions.hrl").
%%--------------- IMPORTS-------------------------------------
%%--------------- EXPORTS-------------------------------------
-export([prepare/2, 
	 rollback/2, 
	 commit/2, 
	 commit_one_phase/2, 
	 forget/2,
%	 before_completion/2,
%	 after_completion/3,
	 commit_subtransaction/3,
	 rollback_subtransaction/2]).


%%--------------- gen_server specific ------------------------
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%------------------------------------------------------------
%% function : init, terminate
%%------------------------------------------------------------
init(State) ->
    process_flag(trap_exit,true),
    io:format("etrap_test_server:init ~p~n", [State]),
    ?debug_print("STARTING etrap_test_server( ~p )~n", [State]),
    {ok, State}.

terminate(Reason, _State) ->
    io:format("etrap_test_server:terminate ~p~n", [Reason]),
    ?debug_print("STOPREASON etrap_test_server( ~p )~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_call(_,_, State) ->
    {noreply, State}.
handle_cast(_, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.

%%-- Inherit from CosTransactions::SubtransactionAwareResource --
prepare(_Self, State) ->    
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:prepare ~p~n", [State]);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:prepare ~p~n", [State]),
    action(prepare, State, {reply, 'VoteCommit', State}).

rollback(_Self, State) ->
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:rollback ~p~n", [State]);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:rollback ~p~n", [State]),
    case sync_test(State) of
	true ->
	    action(rollback, State, {reply, ok, State});
	_->
	    action(rollback, State, {stop, normal, ok, State})
    end.

commit(_Self, State) ->
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:commit ~p~n", [State]);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:commit ~p~n", [State]),
    case sync_test(State) of
	true ->
	    action(commit, State, {reply, ok, State});
	_->
	    action(commit, State, {stop, normal, ok, State})
    end.

commit_one_phase(_Self, State) ->
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:commit_one_phase ~p~n", [State]);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:commit_one_phase ~p~n", [State]),
    case sync_test(State) of
	true ->
	    {reply, ok, State};
	_->
	    {stop, normal, ok, State}
    end.

forget(_Self, State) ->
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:forget ~p~n", [State]);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:forget ~p~n", [State]),
    case sync_test(State) of
	true ->
	    {reply, ok, State};
	_->
	    {stop, normal, ok, State}
    end.

commit_subtransaction(_Self, State, Parent) ->
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:commit_subtransaction( ~p )~n", [Parent]);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:commit_subtransaction( ~p )~n", [Parent]),
    {reply, ok, State}.
rollback_subtransaction(_Self, State) ->
    case ?is_debug_compiled of
	true ->
	    io:format("etrap_test_server:rollback_subtransaction()~n", []);
	_->
	    ok
    end,
%    ?debug_print("etrap_test_server:rollback_subtransaction()~n", []),
    {reply, ok, State}.

%before_completion(_Self, State) ->
%    case ?is_debug_compiled of
%	true ->
%	    io:format("etrap_test_server:before_completion()~n", []);
%	_->
%	    ok
%    end,
%%    ?debug_print("etrap_test_server:before_completion()~n", []),
%    {reply, ok, State}.
%after_completion(_Self, State, Status) ->
%    case ?is_debug_compiled of
%	true ->
%	    io:format("etrap_test_server:after_completion( ~p )~n", [Status]);
%	_->
%	    ok
%    end,
%%    ?debug_print("etrap_test_server:after_completion( ~p )~n", [Status]),
%    {stop, normal, ok, State}.

%%--------------- LOCAL FUNCTIONS ----------------------------
action(Key, State, Default) ->
    case catch lists:keysearch(Key, 1, State) of
        {value,{Key, stop_reply, R}} ->
	    case sync_test(State) of
		true ->
		    {reply, R, State};
		_->
		    {stop, normal, R, State}
	    end;
        {value,{Key, reply, R}} ->
	    {reply, R, State};
        {value,{Key, exc, E}} ->
	    corba:raise(E);
        {value,{Key, delay, Time}} ->
            timer:sleep(Time*1000),
	    Default;
        {value,{Key,Value}} ->
            Value;
        _ ->
	    Default
    end.

sync_test(State) ->
    case catch lists:keysearch(sync, 1, State) of
        {value,{sync, true}} ->
	    true;
        _ ->
	    false
    end.


%%--------------- END OF MODULE ------------------------------

