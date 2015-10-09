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

%%
%% An example Diameter server that can respond to the base protocol
%% RAR sent by the client example.
%%
%% The simplest example to start a server listening on the loopback
%% address (which will serve the example usage given in client.erl) is
%% like this assuming diameter is already started (eg. diameter:start()):
%%
%%   server:start().
%%   server:listen(tcp).
%%
%% The first call starts a service, the second adds a transport listening
%% on the default port.
%%

-module(server).

-export([start/1,    %% start a service
         start/2,    %%
         listen/2,   %% add a listening transport
         stop/1]).   %% stop a service

%% Convenience functions using the default service name.
-export([start/0,
         listen/1,
         stop/0]).

-define(DEF_SVC_NAME, ?MODULE).

%% The service configuration. In a server supporting multiple Diameter
%% applications each application may have its own, although they could all
%% be configured with a common callback module.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {restrict_connections, false},
                        {string_decode, false},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, server_cb}]}]).

%% start/1

start(Name)
  when is_atom(Name) ->
    start(Name, []);

start(Opts)
  when is_list(Opts) ->
    start(?DEF_SVC_NAME, Opts).

%% start/0

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

%% stop/1

stop(Name) ->
    node:stop(Name).

stop() ->
    stop(?DEF_SVC_NAME).
