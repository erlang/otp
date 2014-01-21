%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: The supervisor for acceptor processes in the http server, 
%%          hangs under the httpd_instance_sup_<Addr>_<Port> supervisor.
%%----------------------------------------------------------------------

-module(httpd_acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
%%, start_acceptor/6, start_acceptor/7, stop_acceptor/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link([Addr, Port| _] = Args) ->
    SupName = make_name(Addr, Port),
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
    Name = id(Address, Port),
    Manager = httpd_util:make_name("httpd", Address, Port),
    SockType = proplists:get_value(socket_type, ConfigList, ip_comm),
    IpFamily = proplists:get_value(ipfamily, ConfigList, inet),
    StartFunc = case ListenInfo of
		    undefined ->
			{httpd_acceptor, start_link, [Manager, SockType, Address, Port, IpFamily,
						      httpd_util:make_name("httpd_conf", Address, Port), 
						      AcceptTimeout]};
		    _ ->
			{httpd_acceptor, start_link, [Manager, SockType, Address, Port, ListenInfo,
						      IpFamily,
						      httpd_util:make_name("httpd_conf", Address, Port), 
						      AcceptTimeout]}
		end,
    Restart = transient, 
    Shutdown = brutal_kill,
    Modules = [httpd_acceptor],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

id(Address, Port) ->
    {httpd_acceptor_sup, Address, Port}.

make_name(Addr,Port) ->
    httpd_util:make_name("httpd_acceptor_sup", Addr, Port).

