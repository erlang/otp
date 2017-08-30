%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2015. All Rights Reserved.
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
-module(msacc_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0]).

%% Test cases
-export([
	%% API-test
	api_file/1,
        api_start_stop/1,
	api_timer/1,
        api_print/1
    ]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
all() ->
    [
     api_start_stop,
     api_file,
     api_timer,
     api_print
    ].

suite() -> [
	{timetrap,{minutes,1}},
	{ct_hooks,[ts_install_cth]}
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

api_timer(_Config) ->

    %% Run msacc for about 100ms
    msacc:start(100),

    %% Verify that scheduler 1 executed+slept for about 100ms
    [Sched1] = [S || S = #{ type := scheduler, id := 1 } <- msacc:stats()],

    #{ counters := Cnt } = Sched1,

    %% Time should be in us
    Time = maps:fold(fun(_,V,Acc) -> V + Acc end, 0, Cnt),

    if Time < 120000 andalso Time > 80000 -> ok;
       true -> ct:fail({inaccurate_time, Time, msacc:stats()})
    end.

%% We just do a basic check that none of the apis crash
api_start_stop(_Config) ->
    msacc:start(),
    timer:sleep(100),
    msacc:stop(),
    Runtime = msacc:stats(system_runtime, msacc:stats()),
    Realtime = msacc:stats(system_realtime, msacc:stats()),
    true = Runtime < Realtime,

    RuntimeCnt = msacc:stats(runtime, msacc:stats()),
    RealtimeCnt = msacc:stats(realtime, msacc:stats()),
    TypeCnt = msacc:stats(type, msacc:stats()),

    %% These should be very similar
    RuntimeTypeCnt = msacc:stats(type, RuntimeCnt),
    TypeRuntimeCnt = msacc:stats(runtime, TypeCnt),
    lists:map(fun({#{ system := T1 },#{ system := T2}}) ->
                      if T1-0.5 < T2 orelse T1+0.5 > T2 -> ok;
                         true -> ct:fail({inaccurate_stats, RuntimeTypeCnt,
                                          TypeRuntimeCnt})
                      end
              end, lists:zip(RuntimeTypeCnt, TypeRuntimeCnt)),

    %% These should be very similar
    RealtimeTypeCnt = msacc:stats(type, RealtimeCnt),
    TypeRealtimeCnt = msacc:stats(realtime, TypeCnt),
    lists:map(fun({#{ system := T1 },#{ system := T2}}) ->
                      if T1-0.5 < T2 orelse T1+0.5 > T2 -> ok;
                         true -> ct:fail({inaccurate_stats, RealtimeTypeCnt,
                                          TypeRealtimeCnt})
                      end
              end,lists:zip(RealtimeTypeCnt, TypeRealtimeCnt)),

    msacc:reset().

api_file(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    File = filename:join(PrivDir, "msacc.stats"),
    Stats = msacc:stats(),
    ok = msacc:to_file(File),
    Stats = msacc:from_file(File),

    PrintFile = filename:join(PrivDir, "msacc.txt"),
    msacc:print(PrintFile, Stats, #{}).

%% We just check that it is possible to print in a couple of different ways
api_print(_Config) ->
    msacc:start(100),
    io:format("msacc:print(msacc:stats()).~n"),
    msacc:print(msacc:stats()),
    io:format("msacc:print(msacc:stats(),#{ system => true }).~n"),
    msacc:print(msacc:stats(), #{ system => true }),
    io:format("msacc:print(msacc:stats(runtime,msacc:stats())).~n"),
    msacc:print(msacc:stats(runtime, msacc:stats())),
    io:format("msacc:print(msacc:stats(type,msacc:stats())).~n"),
    msacc:print(msacc:stats(type, msacc:stats())),
    io:format("msacc:print(msacc:stats(realtime,msacc:stats())).~n"),
    msacc:print(msacc:stats(realtime, msacc:stats())),
    io:format("msacc:stats(type,msacc:stats(runtime,msacc:stats())).~n"),
    msacc:print(msacc:stats(type, msacc:stats(runtime, msacc:stats()))).
