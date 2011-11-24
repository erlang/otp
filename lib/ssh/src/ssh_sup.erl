%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% Purpose: The top supervisor for the ssh application.
%%----------------------------------------------------------------------
-module(ssh_sup).

-behaviour(supervisor).

-export([init/1]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([]) ->
    SupFlags = {one_for_one, 10, 3600},
    Children = children(), 
    {ok, {SupFlags, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
get_services() ->
    case (catch application:get_env(ssh, services)) of
	{ok, Services} ->
	    Services;
	_ ->
	    []
    end. 

children() ->
    Services = get_services(),
    Clients = [Service || Service <- Services, is_client(Service)],
    Servers =  [Service || Service <- Services, is_server(Service)],

    [server_child_spec(Servers), client_child_spec(Clients),
     ssh_userauth_reg_spec()].

server_child_spec(Servers) ->
    Name = sshd_sup,
    StartFunc = {sshd_sup, start_link, [Servers]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [sshd_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

client_child_spec(Clients) ->
    Name = sshc_sup,
    StartFunc = {sshc_sup, start_link, [Clients]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [sshc_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

ssh_userauth_reg_spec() ->
    Name = ssh_userreg,
    StartFunc = {ssh_userreg, start_link, []},
    Restart = transient, 
    Shutdown = 5000,
    Modules = [ssh_userreg],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


is_server({sftpd, _}) ->
    true;
is_server({shelld, _}) ->
    true;
is_server(_) ->
    false.

is_client({sftpc, _}) ->
    true;
is_client({shellc, _}) ->
    true;
is_client(_) ->
    false.




