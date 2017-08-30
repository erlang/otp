%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

-module(diameter_ct).

%%
%% Module used to run suites from Makefile.
%%

-export([run/1,
         cover/0]).

%% The makefile looks for signs of failure so ignore the ct:run_test/1
%% return value.

run(Suites) ->
    ct_run([{suite, Suites}]).

cover() ->
    ct_run([{spec, "./testspec"}]).

ct_run(Opts) ->
    Start = info(),
    ct:run_test([{logdir, "./log"},
                 {auto_compile, false}
                 | Opts]),
    info(Start , info()).

info() ->
    [{time, diameter_lib:now()},
     {process_count, erlang:system_info(process_count)}
     | erlang:memory()].

info(L0, L1) ->
    [T, C | M]
        = lists:zipwith(fun({T,N0}, {T,N1}) -> {T, N1, diff(T, N0, N1)} end,
                        L0,
                        L1),
    Diff = [T, C, {memory, M}],
    io:format("INFO: ~p~n", [Diff]).

diff(time, T0, T1) ->
    diameter_lib:micro_diff(T1, T0);
diff(_, N0, N1) ->
    N1 - N0.
