%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2015. All Rights Reserved.
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
%% File    : ETraP_Common.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('ETraP_Common').

%%--------------- INCLUDES ----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Local
-include_lib("ETraP_Common.hrl").
-include_lib("CosTransactions.hrl").

%%--------------- EXPORTS -----------------------------------
-export([try_timeout/1, 
	 get_option/3, 
	 create_name/2, 
	 create_name/1,
	 is_debug_compiled/0,
	 send_stubborn/5,
	 create_link/3]).

%%--------------- DEFINITIONS OF CONSTANTS ------------------
%%------------------------------------------------------------
%% function : create_link
%% Arguments: Module - which Module to call
%%            Env/ARgList - ordinary oe_create arguments.
%% Returns  : 
%% Exception: 
%% Effect   : Necessary since we want the supervisor to be a 
%%            'simple_one_for_one'. Otherwise, using for example,
%%            'one_for_one', we have to call supervisor:delete_child
%%            to remove the childs startspecification from the 
%%            supervisors internal state.
%%------------------------------------------------------------
create_link(Module, Env, ArgList) ->
    Module:oe_create_link(Env, ArgList).

%%------------------------------------------------------------
%% function : get_option
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

get_option(Key, OptionList, DefaultList) ->
    case lists:keysearch(Key, 1, OptionList) of
        {value,{Key,Value}} ->
            Value;
        _ ->
            case lists:keysearch(Key, 1, DefaultList) of
                {value,{Key,Value}} ->
                    Value;
                _->
                    {error, "Invalid option"}
            end
    end.
%%------------------------------------------------------------
%% function : create_name/2
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
 
create_name(Name,Type) ->
    Time = erlang:system_time(),
    Unique = erlang:unique_integer([positive]),
    lists:concat(['oe_',node(),'_',Type,'_',Name,'_',Time,'_',Unique]).
 
%%------------------------------------------------------------
%% function : create_name/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
 
create_name(Type) ->
    Time = erlang:system_time(),
    Unique = erlang:unique_integer([positive]),
    lists:concat(['oe_',node(),'_',Type,'_',Time,'_',Unique]).

%%------------------------------------------------------------
%% function : try_timeout
%% Arguments: Id - name of the timeoutSrv server.
%% Returns  : Boolean
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

try_timeout(TimeoutAt) ->
    case TimeoutAt of
	infinity ->
	    false;
	_->
	    TimeSec = erlang:monotonic_time(seconds),
	    if 
		TimeSec < TimeoutAt ->
		    false;
		true ->
		    true
	    end
    end.

%%------------------------------------------------------------
%% function : send_stubborn
%% Arguments: M - module
%%            F - function
%%            A - arguments
%%            MaxR - Maximum no retries
%%            Wait - sleep Wait seconds before next try.
%% Returns  : see effect
%% Exception: 
%% Effect   : Retries repeatedly until anything else besides
%%            'EXIT', 'COMM_FAILURE' or 'OBJECT_NOT_EXIST'
%%------------------------------------------------------------

send_stubborn(M, F, A, MaxR, Wait) when is_list(A) ->
    send_stubborn(M, F, A, MaxR, Wait, 0);
send_stubborn(M, F, A, MaxR, Wait) ->
    send_stubborn(M, F, [A], MaxR, Wait, 0).
send_stubborn(M, F, A, MaxR, _Wait, MaxR) ->
    ?tr_error_msg("~p:~p( ~p ) failed!! Tried ~p times.~n", [M,F,A,MaxR]),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
send_stubborn(M, F, A, MaxR, Wait, Times) ->
    ?debug_print("~p:~p(~p)  # of retries: ~p~n", [M,F,A, Times]),    
    case catch apply(M,F,A) of
	{'EXCEPTION', E} when is_record(E, 'COMM_FAILURE')->
	    NewTimes = Times +1,
	    timer:sleep(Wait),
	    send_stubborn(M, F, A, MaxR, Wait, NewTimes);
	{'EXCEPTION', E} when is_record(E, 'TRANSIENT')->
	    NewTimes = Times +1,
	    timer:sleep(Wait),
	    send_stubborn(M, F, A, MaxR, Wait, NewTimes);
	{'EXCEPTION', E} when is_record(E, 'TIMEOUT')->
	    NewTimes = Times +1,
	    timer:sleep(Wait),
	    send_stubborn(M, F, A, MaxR, Wait, NewTimes);
	{'EXIT', _} ->
	    NewTimes = Times +1,
	    timer:sleep(Wait),
	    send_stubborn(M, F, A, MaxR, Wait, NewTimes);
	Other ->
	    ?debug_print("~p:~p(~p) Resulted in: ~p~n", [M,F,A, Other]),    
	    Other
    end.

%%------------------------------------------------------------
%% function : is_debug_compiled
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

-ifdef(debug).
    is_debug_compiled() -> true.
-else.
    is_debug_compiled() -> false.
-endif.

%%--------------- END OF MODULE ------------------------------
