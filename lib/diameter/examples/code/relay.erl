%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
                                       {dictionary, diameter_relay},
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
