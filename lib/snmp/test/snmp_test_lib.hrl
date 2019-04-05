%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2019. All Rights Reserved.
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
%% The Initial Developer of the Original Code is Ericsson AB.
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
%% Purpose: Define common macros for testing
%%----------------------------------------------------------------------

%% - (some of the) Macros stolen from the test server -

%% -define(line,put(test_server_loc,{?MODULE,?LINE}),).


%% - Misc macros -

-ifndef(APPLICATION).
-define(APPLICATION,    snmp).
-endif.

-define(SCONF(K,D,C),               snmp_test_lib:set_config(K,D,C)).
-define(GCONF(K,C),                 snmp_test_lib:get_config(K,C)).
-define(RCONF(K,C,V),               snmp_test_lib:replace_config(K,C,V)).
-define(HOSTNAME(N),                snmp_test_lib:hostname(N)).
-define(LOCALHOST(),                snmp_test_lib:localhost()).
-define(LOCALHOST(Family),          snmp_test_lib:localhost(Family)).
-define(SZ(X),                      snmp_test_lib:sz(X)).
-define(OSTYPE(),                   snmp_test_lib:os_type()).
-define(DISPLAY_SUITE_INFO(),       snmp_test_lib:display_suite_info(?MODULE)).


%% - Test case macros - 
-define(OS_BASED_SKIP(Skippable),
        snmp_test_lib:os_based_skip(Skippable)).
-define(NON_PC_TC_MAYBE_SKIP(Config, Condition),
        snmp_test_lib:non_pc_tc_maybe_skip(Config, Condition, ?MODULE, ?LINE)).
-define(SKIP(Reason),   snmp_test_lib:skip(Reason, ?MODULE, ?LINE)).
-define(FAIL(Reason),   snmp_test_lib:fail(Reason, ?MODULE, ?LINE)).


%% - Time macros -

-ifdef(DONT_USE_TEST_SERVER).
-define(HOURS(N),       snmp_test_lib:hours(N)).
-define(MINS(N),        snmp_test_lib:minutes(N)).
-define(SECS(N),        snmp_test_lib:seconds(N)).
-else.
-define(HOURS(N),       test_server:hours(N)).
-define(MINS(N),        test_server:minutes(N)).
-define(SECS(N),        test_server:seconds(N)).
-endif.

-ifdef(DONT_USE_TEST_SERVER).
-define(WD_START(T),    snmp_test_lib:watchdog_start(T)).
-define(WD_STOP(P),     snmp_test_lib:watchdog_stop(P)).
-else.
-define(WD_START(T),    test_server:timetrap(T)).
-define(WD_STOP(P),     test_server:timetrap_cancel(P)).
-endif.

-define(SLEEP(MSEC),    snmp_test_lib:sleep(MSEC)).

%% - Process utility macros - 

-define(FLUSH(),        snmp_test_lib:flush_mqueue()).
-define(ETRAP_GET(),    snmp_test_lib:trap_exit()).
-define(ETRAP_SET(O),   snmp_test_lib:trap_exit(O)).


%% - Node utility macros - 

-define(PING(N),            snmp_test_lib:ping(N)).
-define(LNODES(),           snmp_test_lib:local_nodes()).
-define(NODES(H),           snmp_test_lib:nodes_on(H)).
-define(START_NODE(N,A),    snmp_test_lib:start_node(N,A)).


%% - Application and Crypto utility macros - 

-define(IS_APP_RUNNING(A),   snmp_test_lib:is_app_running(A)).
-define(IS_SNMP_RUNNING(),   snmp_test_lib:is_snmp_running()).
-define(IS_MNESIA_RUNNING(), snmp_test_lib:is_mnesia_running()).
-define(IS_CRYPTO_RUNNING(), snmp_test_lib:is_crypto_running()).
-define(CRYPTO_START(),      snmp_test_lib:crypto_start()).
-define(CRYPTO_SUPPORT(),    snmp_test_lib:crypto_support()).


%% - Dir macros -

-define(DEL_DIR(D),         snmp_test_lib:del_dir(D)).


%% - Print macros

-define(P(C),               snmp_test_lib:p(?MODULE, C)).
-define(P1(F),              snmp_test_lib:p(F, [])).
-define(P2(F, A),           snmp_test_lib:p(F, A)).

-ifdef(snmp_debug).
-ifndef(snmp_log).
-define(snmp_log,true).
-endif.
-ifndef(snmp_error).
-define(snmp_error,true).
-endif.
-else.
-ifdef(snmp_log).
-ifndef(snmp_error).
-define(snmp_error,true).
-endif.
-endif.
-endif.

-ifdef(snmp_debug).
-define(DBG(F,A), ?PRINT("DBG", F, A)).
-else.
-define(DBG(F,A), ok).
-endif.

-ifdef(snmp_log).
-define(LOG(F,A), ?PRINT("LOG", F, A)).
-else.
-define(LOG(F,A), ok).
-endif.

-ifdef(snmp_error).
-define(ERR(F,A), ?PRINT("ERR", F, A)).
-else.
-define(ERR(F,A), ok).
-endif.

-define(INF(F,A), ?PRINT("INF", F, A)).

-define(PRINT(P,F,A),
	snmp_test_lib:print(P, ?MODULE, ?LINE, F, A)).

-define(PRINT1(F, A),  snmp_test_lib:print1(F, A)).
-define(EPRINT1(F, A), ?PRINT1("<ERROR> " ++ F, A)).

-define(PRINT2(F, A),  snmp_test_lib:print2(F, A)).
-define(EPRINT2(F, A), ?PRINT2("<ERROR> " ++ F, A)).

