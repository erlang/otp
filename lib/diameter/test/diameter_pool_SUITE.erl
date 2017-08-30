%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2017. All Rights Reserved.
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

-export([suite/0,
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([tcp_connect/1,
         sctp_connect/1,
         any_connect/1]).

%% ===========================================================================

-define(util, diameter_util).

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
         {application, [{alias, common},
                        {dictionary, diameter_gen_base_rfc6733},
                        {module, diameter_callback}]},
         {application, [{alias, accounting},
                        {dictionary, diameter_gen_acct_rfc6733},
                        {module, diameter_callback}]}]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [tcp_connect,
     sctp_connect,
     any_connect].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, _Config) ->
    diameter:stop().

init_per_suite(Config) ->
    [{sctp, ?util:have_sctp()} | Config].

end_per_suite(_Config) ->
    ok.

%% ===========================================================================

tcp_connect(_Config) ->
    connect(tcp, tcp).

sctp_connect(Config) ->
    case lists:member({sctp, true}, Config) of
       true  -> connect(sctp, sctp);
       false -> {skip, no_sctp}
    end.

any_connect(_Config) ->
    connect(any, tcp).

%% connect/2

%% Establish multiple connections between a client and server.
connect(ClientProt, ServerProt) ->
    ok = diameter:start(),
    [] = [{S,T} || S <- ["server", "client"],
                   T <- [diameter:start_service(S, ?SERVICE(S))],
                   T /= ok],
    %% Listen with a single transport with pool_size = 4. Ensure the
    %% expected number of transport processes are started.
    LRef = ?util:listen("server", ServerProt, [{pool_size, 4}]),
    {4,0} = count("server", LRef, accept), %% 4 transports, no connections
    %% Establish 5 connections.
    Ref = ?util:connect("client", ClientProt, LRef, [{pool_size, 5}]),
    {5,5} = count("client", Ref, pool),    %% 5 connections
    %% Ensure the server has started replacement transports within a
    %% reasonable time. Sleepsince there's no guarantee the
    %% replacements have been started before the client has received
    %% 'up' events. (Although it's likely.)
    sleep(),
    {9,5} = count("server", LRef, accept), %% 5 connections + 4 accepting
    %% Ensure there are still the expected number of accepting transports
    %% after stopping the client service.
    ok = diameter:stop_service("client"),
    sleep(),
    {4,0} = count("server", LRef, accept), %% 4 transports, no connections
    %% Done.
    ok = diameter:stop_service("server").

count(Name, Ref, Key) ->
    [{transport, [[{ref, Ref} | T]]},
     {connections, Cs}]
        = diameter:service_info(Name, [transport, connections]),
    {Key, Ps} = lists:keyfind(Key, 1, T),
    {length(Ps), length(Cs)}.  %% number of processes, connections

sleep() ->
    receive after 1000 -> ok end.
