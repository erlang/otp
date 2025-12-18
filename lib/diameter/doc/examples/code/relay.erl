%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

-module(relay).

%%
%% An example Diameter relay agent.
%%
%% Simplets usage to connect to a server listening on TCP at
%% 127.0.0.1:3868 and listen for connections on TCP at the same
%% endpoint:
%%
%%   relay:start().
%%   relay:connect(tcp).
%%   relay:listen(sctp).
%%

%% Interface.
-export([start/1,
         start/2,
         listen/2,
         connect/2,
         stop/1]).

%% Convenience functions using the default service name.
-export([start/0,
         listen/1,
         connect/1,
         stop/0]).

%% Default service name.
-define(DEF_SVC_NAME, ?MODULE).

%% Service configuration.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "RelayAgent"},
                        {'Auth-Application-Id', [16#FFFFFFFF]},
                        {decode_format, map},
                        {restrict_connections, false},
                        {string_decode, false},
                        {strict_mbit, false},
                        {application, [{alias, relay},
                                       {dictionary, diameter_gen_relay},
                                       {module, relay_cb},
                                       {call_mutates_state, false}]}]).

%% start/2

start(Name, Opts) ->
    Defaults = [T || {K,_} = T <- ?SERVICE(Name),
                     not lists:keymember(K, 1, Opts)],
    diameter:start_service(Name, Opts ++ Defaults).

%% start/1

start(Opts) ->
    start(?DEF_SVC_NAME, Opts).

%% start/0

start() ->
    start(?DEF_SVC_NAME, []).

%% listen/2

listen(Name, Opts) ->
    server:listen(Name, Opts).

%% listen/1

listen(Opts) ->
    listen(?DEF_SVC_NAME, Opts).

%% connect/2

connect(Name, Opts) ->
    client:connect(Name, Opts).

%% connect/1

connect(Opts) ->
    connect(?DEF_SVC_NAME, Opts).

%% stop/1

stop(Name) ->
    diameter:stop_service(Name).

%% stop/0

stop() ->
    stop(?DEF_SVC_NAME).
