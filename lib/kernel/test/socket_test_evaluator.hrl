%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-ifndef(socket_test_evaluator).
-define(socket_test_evaluator, true).

-record(ev, {name :: string(),
             pid  :: pid(),
             mref :: reference()}).

-define(SEV,                              socket_test_evaluator).

-define(SEV_START(N, S, IS),              ?SEV:start(N, S, IS)).
-define(SEV_AWAIT_FINISH(Evs),            ?SEV:await_finish(Evs)).

-define(SEV_ANNOUNCE_START(To),           ?SEV:announce_start(To)).
-define(SEV_ANNOUNCE_START(To, Ex),       ?SEV:announce_start(To, Ex)).
-define(SEV_ANNOUNCE_CONTINUE(To, S),     ?SEV:announce_continue(To, S)).
-define(SEV_ANNOUNCE_CONTINUE(To, S, Ex), ?SEV:announce_continue(To, S, Ex)).
-define(SEV_ANNOUNCE_READY(To, S),        ?SEV:announce_ready(To, S)).
-define(SEV_ANNOUNCE_READY(To, S, Ex),    ?SEV:announce_ready(To, S, Ex)).
-define(SEV_ANNOUNCE_TERMINATE(To),       ?SEV:announce_terminate(To)).

-define(SEV_AWAIT_START(),                ?SEV:await_start()).
-define(SEV_AWAIT_START(P),               ?SEV:await_start(P)).
-define(SEV_AWAIT_CONTINUE(F, N, S),      ?SEV:await_continue(F, N, S)).
-define(SEV_AWAIT_CONTINUE(F, N, S, Ps),  ?SEV:await_continue(F, N, S, Ps)).
-define(SEV_AWAIT_READY(F, N, S),         ?SEV:await_ready(F, N, S)).
-define(SEV_AWAIT_READY(F, N, S, Ps),     ?SEV:await_ready(F, N, S, Ps)).
-define(SEV_AWAIT_TERMINATE(F, N),        ?SEV:await_terminate(F, N)).
-define(SEV_AWAIT_TERMINATE(F, N, Ps),    ?SEV:await_terminate(F, N, Ps)).
-define(SEV_AWAIT_TERMINATION(P),         ?SEV:await_termination(P)).
-define(SEV_AWAIT_TERMINATION(P, R),      ?SEV:await_termination(P, R)).

-define(SEV_IPRINT(F, A),                 ?SEV:iprint(F, A)).
-define(SEV_IPRINT(F),                    ?SEV_IPRINT(F, [])).
-define(SEV_EPRINT(F, A),                 ?SEV:eprint(F, A)).
-define(SEV_EPRINT(F),                    ?SEV_EPRINT(F, [])).

-define(SEV_SLEEP(T), #{desc => "sleep",
                        cmd  => fun(_) ->
                                        ?SLEEP(T),
                                        ok
                                end}).
-define(SEV_FINISH_NORMAL, #{desc => "finish",
                             cmd  => fun(_) ->
                                             {ok, normal}
                                     end}).

-endif. % -ifdef(socket_test_evaluator).

