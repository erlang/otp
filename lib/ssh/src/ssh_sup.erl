%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% Purpose: The top supervisor for the ssh application.
%%----------------------------------------------------------------------
-module(ssh_sup).

-behaviour(supervisor).

-export([init/1]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
-spec init( [term()] ) -> {ok,{supervisor:sup_flags(),[supervisor:child_spec()]}} | ignore .

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

    [server_child_spec(Servers), client_child_spec(Clients)].

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




