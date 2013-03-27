%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
    [{time, now()},
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
    timer:now_diff(T1, T0);
diff(_, N0, N1) ->
    N1 - N0.
