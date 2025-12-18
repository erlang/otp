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

-module(redirect).

%%
%% An example Diameter redirect agent.
%%
%% Simplest usage to listen on TCP at 127.0.0.1:3868.
%%
%%   redirect:start().
%%   redirect:listen(tcp).
%%

%% Interface.
-export([start/1,
         listen/2,
         stop/1]).

%% Convenience functions using the default service name.
-export([start/0,
         listen/1,
         stop/0]).

-define(DEF_SVC_NAME, ?MODULE).

%% The service configuration.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "RedirectAgent"},
                        {'Auth-Application-Id', [16#FFFFFFFF]},
                        {decode_format, map},
                        {restrict_connections, false},
                        {strict_mbit, false},
                        {string_decode, false},
                        {application, [{alias, redirect},
                                       {dictionary, diameter_gen_relay},
                                       {module, redirect_cb},
                                       {call_mutates_state, false}]}]).

%% start/0

start() ->
    start(?DEF_SVC_NAME).

%% start/1

start(Name)
  when is_atom(Name) ->
    start(Name, []);

start(Opts)
  when is_list(Opts) ->
    start(?DEF_SVC_NAME, Opts).

%% start/2

start(Name, Opts) ->
    Defaults = [T || {K,_} = T <- ?SERVICE(Name),
                     not lists:keymember(K, 1, Opts)],
    diameter:start_service(Name, Opts ++ Defaults).

%% listen/2

listen(Name, Opts) ->
    server:listen(Name, Opts).

%% listen/1

listen(Opts) ->
    listen(?DEF_SVC_NAME, Opts).

%% stop/1

stop(Name) ->
    diameter:stop_service(Name).

%% stop.0

stop() ->
    stop(?DEF_SVC_NAME).
