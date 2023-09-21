%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

-ifndef(kernel_test_lib_hrl).
-define(kernel_test_lib_hrl, true).

-define(LIB,                     kernel_test_lib).

-define(LOOKUP(__Key__, __Config__, __Default__),
        ?LIB:lookup(__Key__, __Config__, __Default__)).
-define(GOOD_HOSTS(__N__), ?LIB:good_hosts(__N__)).

-define(SKIPT(R),                throw({skip, R})).
-define(SKIPE(R),                exit({skip, R})).

-define(TC_TRY(Case, TC),                  ?LIB:tc_try(Case, TC)).
-define(TC_TRY(Case, Cond, TC),            ?LIB:tc_try(Case, Cond, TC)).
-define(TC_TRY(Case, Pre, TC, Post),       ?LIB:tc_try(Case, Pre,TC,Post)).
-define(TC_TRY(Case, Cond, Pre, TC, Post), ?LIB:tc_try(Case, Cond, Pre,TC,Post)).

-define(SOCKET_TYPE(C),          ?LIB:socket_type(C)).
-define(LISTEN(C),               ?LIB:listen(C, 0, [])).
-define(LISTEN(C, P),            ?LIB:listen(C, P, [])).
-define(LISTEN(C, P, O),         ?LIB:listen(C, P, O)).
-define(CONNECT(__C__, __H__, __P__),
        ?LIB:connect(__C__, __H__, __P__, [])).
-define(CONNECT(__C__, __H__, __P__, __O__),
        ?LIB:connect(__C__, __H__, __P__, __O__)).
-define(CONNECT(__C__, __H__, __P__, __O__, __T__),
        ?LIB:connect(__C__, __H__, __P__, __O__, __T__)).

-define(OPEN(C),                 ?LIB:open(C, 0, [])).
-define(OPEN(C, P),              ?LIB:open(C, P, [])).
-define(OPEN(C, P, O),           ?LIB:open(C, P, O)).

-define(INET_BACKEND_OPTS(C),    ?LIB:inet_backend_opts(C)).
-define(EXPLICIT_INET_BACKEND(), ?LIB:explicit_inet_backend()).
-define(TEST_INET_BACKENDS(),    ?LIB:test_inet_backends()).
-define(WHICH_INET_BACKEND(C),   ?LIB:which_inet_backend(C)).
-define(IS_SOCKET_BACKEND(C),    ?LIB:is_socket_backend(C)).

-define(HAS_SUPPORT_IPV4(),      ?LIB:has_support_ipv4()).
-define(HAS_SUPPORT_IPV6(),      ?LIB:has_support_ipv6()).
-define(WHICH_LOCAL_ADDR(D),     ?LIB:which_local_addr((D))).

-define(START_NODE(__N__, __A__),
        ?LIB:start_node(__N__, __A__)).
-define(START_NODE(__N__, __A__, __O__),
        ?LIB:start_node(__N__, __A__, __O__)).

-define(STOP_NODE(__N__),        ?LIB:stop_node(__N__)).
                         
-define(F(FORMAT, ARGS),         ?LIB:f((FORMAT), (ARGS))).
-define(P(F),                    ?LIB:print(F)).
-define(P(F,A),                  ?LIB:print(F, A)).
-define(FTS(),                   ?LIB:formated_timestamp()).

-define(SECS(I),                 timer:seconds(I)).
-define(MINS(I),                 timer:minutes(I)).

-define(SLEEP(T),                ct:sleep(T)).
-define(TT(T),                   ct:timetrap(T)).

-endif. % -ifdef(kernel_test_lib_hrl).
