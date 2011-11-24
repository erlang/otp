%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(diameter_tcp_sup).

-behaviour(supervisor).

%% interface
-export([start/0,
         start_child/1]).

%% internal exports
-export([start_link/1,
         init/1]).

%% Start multiple supervisors only because a child can't start another
%% child before supervisor:start_child/2 has returned, although two is
%% really sufficient (listeners and monitors can be under the same).
-define(TRANSPORT_SUP, diameter_tcp_transport_sup).
-define(LISTENER_SUP,  diameter_tcp_listener_sup).
-define(MONITOR_SUP,   diameter_tcp_monitor_sup).

%% start/0
%%
%% Start the TCP-specific supervisors.

start() ->
    diameter_transport_sup:start_child(?TRANSPORT_SUP, ?MODULE),
    diameter_transport_sup:start_child(?LISTENER_SUP,  ?MODULE),
    diameter_transport_sup:start_child(?MONITOR_SUP,   ?MODULE).

%% start_child/1
%%
%% Start a worker under one of the child supervisors.

start_child(T) ->
    SupRef = case element(1,T) of
                 accept  -> ?TRANSPORT_SUP;
                 connect -> ?TRANSPORT_SUP;
                 listen  -> ?LISTENER_SUP;
                 monitor -> ?MONITOR_SUP
             end,
    supervisor:start_child(SupRef, [T]).

%% start_link/1
%%
%% Callback from diameter_transport_sup as a result of start/0.
%% Starts a child supervisor under the transport supervisor.

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Mod = diameter_tcp,
    Flags = {simple_one_for_one, 0, 1},
    ChildSpec = {Mod,
                 {Mod, start_link, []},
                 temporary,
                 1000,
                 worker,
                 [Mod]},
    {ok, {Flags, [ChildSpec]}}.
