%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% An example Diameter relay agent.
%%
%% Usage to connect to a server listening on the default port over TCP
%% and to listen on the default port over SCTP is as follows, assuming
%% diameter is already started (eg. diameter:start()).
%%
%% Eg.  relay:start().
%%      relay:connect(tcp).
%%      relay:listen(sctp).
%%

-module(relay).

-export([start/1,
         start/2,
         listen/2,
         connect/2,
         stop/1]).

-export([start/0,
         listen/1,
         connect/1,
         stop/0]).

-define(DEF_SVC_NAME, ?MODULE).

%% The service configuration.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "RelayAgent"},
                        {'Auth-Application-Id', [16#FFFFFFFF]},
                        {string_decode, false},
                        {application, [{alias, relay},
                                       {dictionary, diameter_gen_relay},
                                       {module, relay_cb}]}]).

%% start/1

start(Name)
  when is_atom(Name) ->
    start(Name, []).

%% start/1

start() ->
    start(?DEF_SVC_NAME).

%% start/2

start(Name, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name),
                                   false == lists:keymember(K, 1, Opts)]).

%% listen/2

listen(Name, T) ->
    node:listen(Name, T).

listen(T) ->
    listen(?DEF_SVC_NAME, T).

%% connect/2

connect(Name, T) ->
    node:connect(Name, T).

connect(T) ->
    connect(?DEF_SVC_NAME, T).

%% stop/1

stop(Name) ->
    node:stop(Name).

stop() ->
    stop(?DEF_SVC_NAME).
