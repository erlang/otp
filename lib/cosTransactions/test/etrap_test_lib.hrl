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

-ifndef(ETRAP_TEST_LIB_HRL).
-define(ETRAP_TEST_LIB_HRL, true).

-define(match(ExpectedRes, Expr, Msg),
	fun() ->
	       AcTuAlReS = (catch (Expr)),
	       case AcTuAlReS of
		   ExpectedRes ->
		       io:format("~n------ CORRECT RESULT ------~n~p~n~p~n",
				 [AcTuAlReS, Msg]),
		       ok;
		   _ ->
		       io:format("~n###### ERROR ERROR ######~n~p~n~p~n",
				 [AcTuAlReS, Msg]),
		       exit(AcTuAlReS)
	       end
       end()).

-define(match_inverse(NotExpectedRes, Expr, Msg),
	fun() ->
		AcTuAlReS = (catch (Expr)),
	       case AcTuAlReS of
		   NotExpectedRes ->
		       io:format("~n###### ERROR ERROR ######~n ~p~n~p~n",
				 [AcTuAlReS, Msg]),
		       exit(AcTuAlReS);
		   _ ->
		       io:format("~n------ CORRECT RESULT ------~n~p~n~p~n",
				 [AcTuAlReS, Msg]),
		       ok
	       end
       end()).


-define(crash_and_recover, fun(_Env)-> exit(crash_and_burn) end).

-define(crash_no_recovery, fun(Env)-> throw({stop,normal,{Env,state}}) end).

-define(delay(Time), fun(_Id) -> timer:sleep(Time*1000) end).

-define(TIMEOUT,        4).
-define(SUP_TEST(Env, Name), 
	['etrap_test_server', Env, 
	 [{sup_child, true}, {persistent, true}, 
	  {regname, {global, Name}}]]).

-define(no_context, [[],[],[], []]).
-define(nop, []).
-define(delay_transient(Tag, Ti),
	[{Tag, ?delay(Ti), transient}]).
-define(crash_transient(Tag),
	[{Tag, ?crash_and_recover, transient}]).
-define(crash_permanent(Tag),
	[{Tag, ?crash_no_recovery, permanent}]).

%%-----------------------------------------------------------
%% Definition of 'Resource' action.
%%                         function  action  reply
%%-----------------------------------------------------------
%% raise #'CosTransactions_HeuristicMixed' {}
-define(rollback_mix,     [{rollback, exc, ?tr_mixed}]).
-define(commit_mix,       [{commit,   exc, ?tr_mixed}]).
-define(prepare_mix,      [{prepare,  exc, ?tr_mixed}]).
%% raise #'CosTransactions_HeuristicRollback' {}
-define(rollback_rb,      [{rollback, exc, ?tr_rollback}]).
-define(commit_rb,        [{commit,   exc, ?tr_rollback}]).
%% raise #'CosTransactions_HeuristicCommit' {}
-define(rollback_cm,      [{rollback, exc, ?tr_commit}]).
-define(commit_cm,        [{commit,   exc, ?tr_commit}]).
%% delay reply
-define(rollback_delay,   [{rollback, delay, ?TIMEOUT*2}]).
-define(commit_delay,     [{commit,   delay, ?TIMEOUT*2}]).
-define(prepare_delay,    [{prepare,  delay, ?TIMEOUT*2}]).
%% other reply than default
-define(prepare_commit,   [{prepare,  reply, 'VoteCommit'}]).
-define(prepare_rollback, [{prepare,  stop_reply, 'VoteRollback'}]).

-endif.

%%-------------- EOF ---------------------------------------------------
