%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

-ifndef(diameter_util__).
-define(diameter_util__, true).

-define(DUTIL,                        diameter_util).
-define(DEL,                          diameter_event_logger).

-define(LOG(F, A),                    ?LOG(atom_to_list(?MODULE), F, A)).
-define(LOG(MS, F, A),                ?DUTIL:log(MS, ?LINE, F, A)).

-define(F(F, A),                      ?DUTIL:f(F, A)).
-define(FS(),                         ?DUTIL:formated_timestamp()).
-define(FS(TS),                       ?DUTIL:formated_timestamp(TS)).

-define(HAVE_SCTP(),                  ?DUTIL:have_sctp()).
-define(MKTEMP(S),                    ?DUTIL:mktemp(S)).
-define(RUN(A),                       ?DUTIL:run(A)).
-define(PEER(P),                      ?DUTIL:peer(P)).
-define(LPORT(P, R),                  ?DUTIL:lport(P, R)).
-define(CONSULT(N, S),                ?DUTIL:consult(N, S)).

-define(LISTEN(SN, Proto),            ?DUTIL:listen(SN, Proto)).
-define(LISTEN(SN, Proto, Opts),      ?DUTIL:listen(SN, Proto, Opts)).
-define(CONNECT(SN, Proto, LR),       ?DUTIL:connect(SN, Proto, LR)).
-define(CONNECT(SN, Proto, LR, Opts), ?DUTIL:connect(SN, Proto, LR, Opts)).
-define(DISCONNECT(CN, CR, SN, SR),   ?DUTIL:disconnect(CN, CR, SN, SR)).

-define(INFO(),                       ?DUTIL:info()).
-define(SCRAMBLE(SS),                 ?DUTIL:scramble(SS)).

-define(CHOOSE(L),                    ?DUTIL:choose(L)).

-define(UNIQUE_STRING(),              ?DUTIL:unique_string()).

-define(PCALL(F), 
        ?DUTIL:proxy_call((F), infinity, infinity, {error, timeout})).
-define(PCALL(F, T, PT),
        ?DUTIL:proxy_call((F), (T), (PT), {error, timeout})).
-define(PCALL(F, T, PT, DRes),
        ?DUTIL:proxy_call((F), (T), (PT), (DRes))).

-define(TS(),                         erlang:system_time(millisecond)).
-define(MINS(M),                      timer:minutes((M))).
-define(SECS(S),                      timer:seconds((S))).

-define(LIB_DIR(A, SD),               ?DUTIL:lib_dir((A), (SD))).

-define(DEL_START(),                  ?DEL:start_link()).
-define(DEL_STOP(),                   ?DEL:stop()).
-define(DEL_REG(S),                   ?DEL:register_service((S))).
-define(DEL_UNREG(S),                 ?DEL:unregister_service((S))).

-endif.
