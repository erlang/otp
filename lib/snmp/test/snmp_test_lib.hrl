%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2022. All Rights Reserved.
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

-define(EXPLICIT_INET_BACKEND(),  ?LIB:explicit_inet_backend()).
-define(TEST_INET_BACKENDS(),     ?LIB:test_inet_backends()).


%% - Test case macros - 

-define(TC_TRY(C, TC),                     ?LIB:tc_try(C, TC)).
-define(TC_TRY(C, TCCond, TC),             ?LIB:tc_try(C, TCCond, TC)).
-define(TC_TRY(C, Pre, TC, Post),          ?LIB:tc_try(C, Pre, TC, Post)).
-define(TC_TRY(C, TCCond, Pre, TC, Post),  ?LIB:tc_try(C, TCCond, Pre, TC, Post)).

-define(OS_BASED_SKIP(Skippable), ?LIB:os_based_skip(Skippable)).
-define(NON_PC_TC_MAYBE_SKIP(Config, Condition),
        ?LIB:non_pc_tc_maybe_skip(Config, Condition, ?MODULE, ?LINE)).

-define(SKIP(Reason),        ?LIB:skip(Reason, ?MODULE, ?LINE)).
-define(FAIL(Reason),        ?LIB:fail(Reason, ?MODULE, ?LINE)).
-define(HAS_SUPPORT_IPV6(),  ?LIB:has_support_ipv6()).

-define(PCALL(F, T, D),      ?LIB:proxy_call(F, T, D)).


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
-define(MQUEUE(),       ?LIB:mqueue()).
-define(MQUEUE(P),      ?LIB:mqueue(P)).
-define(ETRAP_GET(),    ?LIB:trap_exit()).
-define(ETRAP_SET(O),   ?LIB:trap_exit(O)).
-define(PINFO(__P__),   try process_info(__P__)
                        catch _:_:_ ->
                                {not_running, __P__}
                        end).


%% - Node utility macros - 

-define(PING(N),            ?LIB:ping(N)).
-define(LNODES(),           ?LIB:local_nodes()).
-define(NODES(H),           ?LIB:nodes_on(H)).

%% - Application and Crypto utility macros - 

-define(IS_APP_RUNNING(A),   ?LIB:is_app_running(A)).
-define(IS_SNMP_RUNNING(),   ?LIB:is_snmp_running()).
-define(IS_MNESIA_RUNNING(), ?LIB:is_mnesia_running()).
-define(IS_CRYPTO_RUNNING(), ?LIB:is_crypto_running()).
-define(CRYPTO_START(),      ?LIB:crypto_start()).
-define(CRYPTO_SUPPORT(),    ?LIB:crypto_support()).


-define(ENSURE_NOT_RUNNING(N, S, T), ?LIB:ensure_not_running(N, S, T)).


%% - Dir macros -

-define(DEL_DIR(D),         ?LIB:del_dir(D)).


%% - Print macros

%% Used for indicating the start of a test case
-define(P(C),               ?LIB:p(?MODULE, C)).

%% Takes a format call (such as io:format) and produces a printable string
-define(F(F, A),            ?LIB:f(F, A)).

-ifdef(snmp_debug).
-define(DBG(F,A),      ?IPRINT(F, A)).
-else.
-define(DBG(F,A), ok).
-endif.

%% ERROR print
-define(EPRINT(F),     ?LIB:eprint(F, [])).
-define(EPRINT(F, A),  ?LIB:eprint(F, A)).

%% WARNING print
-define(WPRINT(F),     ?LIB:wprint(F, [])).
-define(WPRINT(F, A),  ?LIB:wprint(F, A)).

%% NOTICE print
-define(NPRINT(F),     ?LIB:nprint(F, [])).
-define(NPRINT(F, A),  ?LIB:nprint(F, A)).

%% INFO print
-define(IPRINT(F),     ?LIB:iprint(F, [])).
-define(IPRINT(F, A),  ?LIB:iprint(F, A)).

-define(FTS(),         snmp_misc:formated_timestamp()).
-define(FTS(TS),       snmp_misc:format_timestamp(TS)).

%% This needs to be a macro-definition to capture ?FUNCTION_NAME and ?MODULE.
-define (START_PEER(Kind), ?CT_PEER(#{
    name => ?CT_PEER_NAME(atom_to_list(?FUNCTION_NAME) ++ Kind),
    args => ["-s", "snmp_test_sys_monitor", "start", "-s", "global", "sync"]
})).
-define(STOP_PEER(__P__), peer:stop(__P__)).
