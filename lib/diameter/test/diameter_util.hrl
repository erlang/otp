%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024-2024. All Rights Reserved.
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

-define(LOG(F, A),                    ?LOG(atom_to_list(?MODULE), F, A)).
-define(LOG(MS, F, A),                ?DUTIL:log(MS, ?LINE, F, A)).

-define(F(F, A),                      ?DUTIL:f(F, A)).

-define(DEL_START(N, S),              ?DUTIL:diameter_event_logger_start(N, S)).
-define(DEL_STOP(L),                  ?DUTIL:diameter_event_logger_stop(L)).
		                 
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

-define(UNIQUE_STRING(),              ?DUTIL:unique_string()).

-define(PCALL(F), 
        ?DUTIL:proxy_call((F), infinity, infinity, {error, timeout})).
-define(PCALL(F, T, PT),
        ?DUTIL:proxy_call((F), (T), (PT), {error, timeout})).
-define(PCALL(F, T, PT, DRes),
        ?DUTIL:proxy_call((F), (T), (PT), (DRes))).
-define(TS(),          erlang:system_time(millisecond)).
-define(MINS(M),       timer:minutes((M))).
-define(SECS(S),       timer:seconds((S))).

-endif.
