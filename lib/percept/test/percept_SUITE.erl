%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(percept_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).

%% Test cases
-export([app/1,
         appup/1,
         profile/1,
         analyze/1,
         analyze_dist/1,
         webserver/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [app, appup, webserver, profile,
     analyze, analyze_dist].


%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%% Test that the percept app file is ok
app(Config) when is_list(Config) ->
    ok = test_server:app_test(percept).

%% Test that the percept appup file is ok
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(percept).

%% Percept webserver test.
webserver(Config) when is_list(Config) ->
    % Explicit start inets?
    {started, _, Port} = percept:start_webserver(),
    ok = percept:stop_webserver(Port), 
    {started, _, _} = percept:start_webserver(),
    ok = percept:stop_webserver(),
    {started, _, NewPort} = percept:start_webserver(),
    ok = percept:stop_webserver(NewPort),
    application:stop(inets),
    ok.

%% Percept profile test.
profile(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    File = filename:join([Path,"profile_test.dat"]),
    {ok, _} = percept:profile(File, [procs]),
    ipc_tree:go(7),
    ok = percept:stop_profile(),
    ok.

%% Percept analyze test.
analyze(Config) when is_list(Config) ->
    Begin = processes(),
    Path = proplists:get_value(data_dir, Config),
    File = filename:join([Path,"profile_test.dat"]),
    T0 = erlang:monotonic_time(milli_seconds),
    ok = percept:analyze(File),
    T1 = erlang:monotonic_time(milli_seconds),
    io:format("percept:analyze/1 took ~w ms.~n", [T1 - T0]),
    {stopped, _} = percept_db:stop(),
    print_remainers(remainers(Begin, processes())),
    ok.

%% Percept analyze distribution test.
analyze_dist(Config) when is_list(Config) ->
    Begin = processes(),
    Path = proplists:get_value(data_dir, Config),
    File = filename:join([Path,"ipc-dist.dat"]),
    T0 = erlang:monotonic_time(milli_seconds),
    ok = percept:analyze(File),
    T1 = erlang:monotonic_time(milli_seconds),
    io:format("percept:analyze/1 took ~w ms.~n", [T1 - T0]),
    {stopped, _} = percept_db:stop(),
    print_remainers(remainers(Begin, processes())),
    ok.

%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------

print_remainers([])   -> ok;
print_remainers([Pid|Pids]) ->
    io:format("[Pid ~p] [Entry ~p] [Name ~p]~n", [
	Pid,
	erlang:process_info(Pid, initial_call),
	erlang:process_info(Pid, registered_name)
    ]),
    print_remainers(Pids).

remainers(Begin, End) -> remainers(Begin, End, []).
remainers(_, [], Out) -> lists:reverse(Out);
remainers(Begin, [Pid|End], Out) ->
    case lists:member(Pid, Begin) of
	true  -> remainers(Begin, End, Out);
	false -> remainers(Begin, End, [Pid|Out])
    end.
