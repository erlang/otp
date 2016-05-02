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

-module(diameter_sctp_sup).

-behaviour(supervisor).

%% interface
-export([start/0,
         start_child/1]).

%% internal exports
-export([start_link/1,
         init/1]).

%% Start multiple supervisors only because a child can't start another
%% child before supervisor:start_child/2 has returned.
-define(TRANSPORT_SUP, diameter_sctp_transport_sup).
-define(LISTENER_SUP,  diameter_sctp_listener_sup).

%% start/0
%%
%% Start the TCP-specific supervisors.

start() ->
    diameter_transport_sup:start_child(?TRANSPORT_SUP, ?MODULE),
    diameter_transport_sup:start_child(?LISTENER_SUP,  ?MODULE).

%% start_child/1
%%
%% Start a worker under one of the child supervisors.

start_child(T) ->
    SupRef = case element(1,T) of
                 connect   -> ?TRANSPORT_SUP;
                 accept    -> ?TRANSPORT_SUP;
                 listen    -> ?LISTENER_SUP
             end,
    supervisor:start_child(SupRef, [T]).

%% start_link/1
%%
%% Callback from diameter_transport_sup as a result of start/0.
%% Starts a child supervisor under the transport supervisor.

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Mod = diameter_sctp,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 1000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
