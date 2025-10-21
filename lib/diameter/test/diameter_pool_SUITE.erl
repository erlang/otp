%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
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

%%
%% Test of the pool_size option in connecting nodes with multiple
%% connections.
%%

-module(diameter_pool_SUITE).

%% testcases, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([
         %% Framework functions
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
        
         %% The test cases
         tcp/1,
         sctp/1,
         any/1
        ]).


-include("diameter_util.hrl").


%% ===========================================================================

%% Config for diameter:start_service/2.
-define(SERVICE(Host),
        [{'Origin-Host', Host ++ ".ericsson.com"},
         {'Origin-Realm', "ericsson.com"},
         {'Host-IP-Address', [{127,0,0,1}]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [0]},  %% common
         {'Acct-Application-Id', [3]},  %% accounting
         {restrict_connections, false},
         {spawn_opt, {diameter_dist, route_session, []}},
         {application, [{alias, common},
                        {dictionary, diameter_gen_base_rfc6733},
                        {module, diameter_callback}]},
         {application, [{alias, accounting},
                        {dictionary, diameter_gen_acct_rfc6733},
                        {module, diameter_callback}]}]).


-define(PL(F),    ?PL(F, [])).
-define(PL(F, A), ?LOG("DPOOL", F, A)).


%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 45}}].

all() ->
    [tcp, sctp, any].


init_per_suite(Config) ->
    ?PL("init_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:init_per_suite(Config).

end_per_suite(Config) ->
    ?PL("end_per_suite -> entry with"
        "~n   Config: ~p", [Config]),
    ?DUTIL:end_per_suite(Config).


%% This test case can take a *long* time, so if the machine is too slow, skip
%% init_per_testcase(parallel = Case, Config) when is_list(Config) ->
%%     ?RL("init_per_testcase(~w) -> check factor", [Case]),
%%     Key = dia_factor,
%%     case lists:keysearch(Key, 1, Config) of
%%         {value, {Key, Factor}} when (Factor > 10) ->
%%             ?RL("init_per_testcase(~w) -> Too slow (~w) => SKIP",
%%                 [Case, Factor]),
%%             {skip, {machine_too_slow, Factor}};
%%         _ ->
%%             ?RL("init_per_testcase(~w) -> run test", [Case]),
%%             Config
%%     end;
init_per_testcase(Case, Config) ->
    ?PL("init_per_testcase(~w) -> entry", [Case]),
    Config.


end_per_testcase(Case, Config) when is_list(Config) ->
    ?PL("end_per_testcase(~w) -> entry", [Case]),
    Config.


%% ===========================================================================

tcp(_Config) ->
    ?PL("tcp -> entry with"
        "~n   Config: ~p", [_Config]),
    run([tcp]).

sctp(_Config) ->
    ?PL("sctp -> entry with"
        "~n   Config: ~p", [_Config]),
    case ?HAVE_SCTP() of
        true  -> run([sctp]);
        false -> {skip, no_sctp}
    end.

any(_Config) ->
    ?PL("any -> entry with"
        "~n   Config: ~p", [_Config]),
    run([any]).


%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run(tcp = T) ->
    ?PL("run(tcp) -> entry"),
    connect(T, T);

run(sctp = T) ->
    ?PL("run(sctp) -> entry"),
    connect(T, T);

run(any = T) ->
    ?PL("run(any) -> entry"),
    connect(T, tcp);

run(List) ->
    ?PL("run -> entry with"
        "~n   List: ~p", [List]),
    ok = diameter:start(),
    try
        ?RUN([[{[fun run/1, T], 30000}  || T <- List]])
    after
        ok = diameter:stop()
    end.

%% connect/2

%% Establish multiple connections between a client and server.
connect(ClientProt, ServerProt) ->
    ?PL("connect -> start 'server' (~w) and 'client' services (~w)",
        [ClientProt, ServerProt]),
    [] = [{S,T} || S <- ["server", "client"],
                   T <- [diameter:start_service(S, ?SERVICE(S))],
                   T /= ok],
    %% Listen with a single transport with pool_size = 4. Ensure the
    %% expected number of transport processes are started.
    ?PL("connect -> start 'server' listener with pool size 4"),
    LRef = ?LISTEN("server", ServerProt, [{pool_size, 4}]),
    {4,0} = count("server", LRef, accept), %% 4 transports, no connections
    %% Establish 5 connections.
    N = pool_size(5),  %% connections to establish
    ?PL("connect -> start 'client' connections with pool size ~p", [N]),
    Ref = ?CONNECT("client", ClientProt, LRef, [{pool_size, N}]),
    {N,N} = count("client", Ref, pool),    %% N connections
    %% Ensure the server has started replacement transports within a
    %% reasonable time. Sleepsince there's no guarantee the
    %% replacements have been started before the client has received
    %% 'up' events. (Although it's likely.)
    ?PL("connect -> sleep some"),
    timer:sleep(1000),
    N4 = N + 4,
    ?PL("connect -> verify 'server' acceptors"),
    {N4,N} = count("server", LRef, accept), %% N connections + 4 accepting
    %% Ensure there are still the expected number of accepting transports
    %% after stopping the client service.
    ?PL("connect -> stop 'client' service"),
    ok = diameter:stop_service("client"),
    ?PL("connect -> sleep some"),
    timer:sleep(1000),
    ?PL("connect -> verify 'server' acceptors"),
    {4,0} = count("server", LRef, accept), %% 4 transports, no connections
    %% Done.
    ?PL("connect -> remote 'client' transport"),
    ok = diameter:remove_transport("client", true),
    ?PL("connect -> remote 'server' transport"),
    ok = diameter:remove_transport("server", true),
    ?PL("connect -> stop 'server' service"),
    ok = diameter:stop_service("server"),
    ?PL("connect -> done"),
    ok.

count(Name, Ref, Key) ->
    [{transport, [[{ref, Ref} | T]]},
     {connections, Cs}]
        = diameter:service_info(Name, [transport, connections]),
    {Key, Ps} = lists:keyfind(Key, 1, T),
    {length(Ps), length(Cs)}.  %% number of processes, connections

%% Simultaneous connections are often refused on Darwin: don't try to
%% establish more than one.
pool_size(N) ->
    case os:type() of
        {_, 'darwin'} -> 1;
        _             -> rand:uniform(N)
    end.
