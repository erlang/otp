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
%% Purpose: Define common macros for (the snmp) testing
%%----------------------------------------------------------------------

%% - Misc macros -

-ifndef(APPLICATION).
-define(APPLICATION,    snmp).
-endif.

-define(LIB, snmp_test_lib).

-define(SCONF(K,D,C),             ?LIB:set_config(K,D,C)).
-define(GCONF(K,C),               ?LIB:get_config(K,C)).
-define(RCONF(K,C,V),             ?LIB:replace_config(K,C,V)).
-define(HOSTNAME(N),              ?LIB:hostname(N)).
-define(LOCALHOST(),              ?LIB:localhost()).
-define(LOCALHOST(Family),        ?LIB:localhost(Family)).
-define(SZ(X),                    ?LIB:sz(X)).
-define(OSTYPE(),                 ?LIB:os_type()).
-define(DISPLAY_SUITE_INFO(),     ?LIB:display_suite_info(?MODULE)).


%% - Test case macros - 
-define(TC_TRY(C, TC),            ?LIB:tc_try(C, TC)).
-define(TC_TRY(C, TCCond, TC),    ?LIB:tc_try(C, TCCond, TC)).
-define(OS_BASED_SKIP(Skippable), ?LIB:os_based_skip(Skippable)).
-define(NON_PC_TC_MAYBE_SKIP(Config, Condition),
        ?LIB:non_pc_tc_maybe_skip(Config, Condition, ?MODULE, ?LINE)).
-define(SKIP(Reason),        ?LIB:skip(Reason, ?MODULE, ?LINE)).
-define(FAIL(Reason),        ?LIB:fail(Reason, ?MODULE, ?LINE)).
-define(IS_IPV6_HOST(),      ?LIB:is_ipv6_host()).
-define(IS_IPV6_HOST(H),     ?LIB:is_ipv6_host(H)).
-define(HAS_SUPPORT_IPV6(),  ?LIB:has_support_ipv6()).
-define(HAS_SUPPORT_IPV6(H), ?LIB:has_support_ipv6(H)).


%% - Time macros -

-ifdef(DONT_USE_TEST_SERVER).
-define(HOURS(N),       ?LIB:hours(N)).
-define(MINS(N),        ?LIB:minutes(N)).
-define(SECS(N),        ?LIB:seconds(N)).
-else.
-define(HOURS(N),       test_server:hours(N)).
-define(MINS(N),        test_server:minutes(N)).
-define(SECS(N),        test_server:seconds(N)).
-endif.

-ifdef(DONT_USE_TEST_SERVER).
-define(WD_START(T),    ?LIB:watchdog_start(T)).
-define(WD_STOP(P),     ?LIB:watchdog_stop(P)).
-else.
-define(WD_START(T),    test_server:timetrap(T)).
-define(WD_STOP(P),     test_server:timetrap_cancel(P)).
-endif.

-define(SLEEP(MSEC),    ?LIB:sleep(MSEC)).

%% - Process utility macros - 

-define(FLUSH(),        ?LIB:flush_mqueue()).
-define(ETRAP_GET(),    ?LIB:trap_exit()).
-define(ETRAP_SET(O),   ?LIB:trap_exit(O)).


%% - Node utility macros - 

-define(PING(N),            ?LIB:ping(N)).
-define(LNODES(),           ?LIB:local_nodes()).
-define(NODES(H),           ?LIB:nodes_on(H)).
-define(START_NODE(N,A),    ?LIB:start_node(N,A)).
-define(STOP_NODE(N),       ?LIB:stop_node(N)).


%% - Application and Crypto utility macros - 

-define(IS_APP_RUNNING(A),   ?LIB:is_app_running(A)).
-define(IS_SNMP_RUNNING(),   ?LIB:is_snmp_running()).
-define(IS_MNESIA_RUNNING(), ?LIB:is_mnesia_running()).
-define(IS_CRYPTO_RUNNING(), ?LIB:is_crypto_running()).
-define(CRYPTO_START(),      ?LIB:crypto_start()).
-define(CRYPTO_SUPPORT(),    ?LIB:crypto_support()).


%% - Dir macros -

-define(DEL_DIR(D),         ?LIB:del_dir(D)).


%% - Print macros

-define(P(C),               ?LIB:p(?MODULE, C)).
-define(P1(F),              ?LIB:p(F, [])).
-define(P2(F, A),           ?LIB:p(F, A)).
-define(F(F, A),            ?LIB:f(F, A)).

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

-define(INF(F,A),      ?PRINT("INF", F, A)).

-define(PRINT(P,F,A),  ?LIB:print(P, ?MODULE, ?LINE, F, A)).

-define(PRINT1(F, A),  ?LIB:print1(F, A)).
-define(PRINT1(F),     ?PRINT1(F, [])).
-define(EPRINT1(F, A), ?PRINT1("<ERROR> " ++ F, A)).

-define(PRINT2(F, A),  ?LIB:print2(F, A)).
-define(PRINT2(F),     ?PRINT2(F, [])).
-define(EPRINT2(F, A), ?PRINT2("<ERROR> " ++ F, A)).

-define(FTS(),         snmp_misc:formated_timestamp()).
-define(FTS(TS),       snmp_misc:format_timestamp(TS)).

