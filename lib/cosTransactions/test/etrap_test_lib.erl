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

-module(etrap_test_lib).

%%--------------- INCLUDES ---------------------------------------------
-include("etrap_test_lib.hrl").
-include_lib("cosTransactions/src/ETraP_Common.hrl").

%%--------------- EXPORTS ----------------------------------------------
-export([scratch_debug_fun/0,
	 activate_debug_fun/5,
	 update_debug_info/1,
	 deactivate_debug_fun/3,
	 eval_debug_fun/4,
	 set_debug_context/4]).

%%--------------- CONSTANTS/DEFINITIONS --------------------------------
-define(DEBUG_TAB, etrap_debug).
-record(debug_info, {id, function, type, file, line}).

%%--------------- DEBUG FUNCTIONS --------------------------------------
%% Managing conditional debug functions
%%
%% The main idea with the debug_fun's is to allow test programs
%% to control the internal behaviour of CosTransactions. 
%%
%% First should calls to ?eval_debug_fun be inserted at well
%% defined places in CosTransaction's code. E.g. in critical situations
%% of startup, transaction commit, backups etc.
%%
%% Then compile CosTransactions with the compiler option 'debug'.
%%
%% In test programs ?activate_debug_fun should be called
%% in order to bind a fun to the debug identifier stated
%% in the call to ?eval_debug_fun.

scratch_debug_fun() ->
    catch ets:delete(?DEBUG_TAB),
    ets:new(?DEBUG_TAB,
	    [set, public, named_table, {keypos, 2}]).

activate_debug_fun(FunId, Fun, Type, File, Line) ->
    io:format("Activiating ~p   RETRIES: ~p  WAIT: ~p~n", 
	      [FunId, ?tr_max_retries, ?tr_comm_failure_wait]),
    Info = #debug_info{id = FunId,
                       function = Fun,
                       type = Type,
                       file = File,
                       line = Line},
    update_debug_info(Info).

update_debug_info(Info) ->
    case catch ets:insert(?DEBUG_TAB, Info) of
        {'EXIT', _} ->
            scratch_debug_fun(),
            ets:insert(?DEBUG_TAB, Info);
        _ ->
            ok
    end,
    ok.

deactivate_debug_fun(FunId, _File, _Line) ->
    catch ets:delete(?DEBUG_TAB, FunId),
    ok.

eval_debug_fun(FunId, Env, File, Line) ->
    case catch ets:lookup(?DEBUG_TAB, FunId) of
        [] ->
            ok;
        [Info] ->
            Fun = Info#debug_info.function,
            case Info#debug_info.type of
		transient ->
		    deactivate_debug_fun(FunId, File, Line);
		_->
		    ok
	    end,
	    io:format("Running debug fun ~p:~p (LINE: ~p)~n", [File, FunId, Line]),
	    Fun(Env);
	{'EXIT', _R} -> 
	    ok    
    end.


set_debug_context([], [], _, _)-> ok;
set_debug_context([], _, _, _)->
    ets:delete(?DEBUG_TAB),
    exit("failed transactions_SUITE. Bad configuration.");
set_debug_context(_, [], _, _)->
    ets:delete(?DEBUG_TAB),
    exit("failed transactions_SUITE Bad configuration.");
set_debug_context([RHead|RTail], [CHead|CTail], File, Line)->
    write_context(RHead, CHead, File, Line),
    set_debug_context(RTail, CTail, File, Line).

write_context(_Resource, [], _, _)-> ok;
write_context(Resource, [{Func, Fun, Type}|PTail], File, Line)->
    etrap_test_lib:activate_debug_fun({Resource, Func}, 
				      Fun, Type, 
				      File, Line),
    write_context(Resource, PTail, File, Line);
write_context(_,_, _, _) ->
    ets:delete(?DEBUG_TAB),
    exit("failed transactions_SUITE. Bad configuration.").


%%--------------- END OF MODULE ----------------------------------------
