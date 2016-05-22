%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The supervisor for acceptor processes in the http server, 
%%          hangs under the httpd_instance_sup_<Addr>_<Port> supervisor.
%%----------------------------------------------------------------------

-module(httpd_acceptor_sup).

-behaviour(supervisor).

-include("httpd_internal.hrl").

%% API
-export([start_link/1]).
%%, start_acceptor/6, start_acceptor/7, stop_acceptor/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link([Addr, Port, Config| _] = Args) ->
    Profile = proplists:get_value(profile, Config, ?DEFAULT_PROFILE),
    SupName = make_name(Addr, Port, Profile),
    supervisor:start_link({local, SupName}, ?MODULE, [Args]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Args]) ->    
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = [child_spec(Args)],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================  
child_spec([Address, Port, ConfigList, AcceptTimeout, ListenInfo]) ->
    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE),
    Name = id(Address, Port, Profile),
    Manager = httpd_util:make_name("httpd", Address, Port, Profile),
    SockType = proplists:get_value(socket_type, ConfigList, ip_comm),
    IpFamily = proplists:get_value(ipfamily, ConfigList, inet),
    StartFunc = case ListenInfo of
		    undefined ->
			{httpd_acceptor, start_link, 
			 [Manager, SockType, Address, Port, IpFamily,
			  httpd_util:make_name("httpd_conf", Address, Port, Profile), 
			  AcceptTimeout]};
		    _ ->
			{httpd_acceptor, start_link, 
			 [Manager, SockType, Address, Port, ListenInfo,
			  IpFamily,
			  httpd_util:make_name("httpd_conf", Address, Port, Profile), 
			  AcceptTimeout]}
		end,
    Restart = transient, 
    Shutdown = brutal_kill,
    Modules = [httpd_acceptor],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port, Profile) ->
    {httpd_acceptor_sup, Address, Port, Profile}.

make_name(Addr, Port, Profile) ->
    httpd_util:make_name("httpd_acceptor_sup", Addr, Port, Profile).

