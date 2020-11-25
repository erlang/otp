%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% Purpose: Handles an ssh connection, e.i. both the
%% setup SSH Transport Layer Protocol (RFC 4253), Authentication
%% Protocol (RFC 4252) and SSH connection Protocol (RFC 4255)
%% Details of the different protocols are
%% implemented in ssh_transport.erl, ssh_auth.erl and ssh_connection.erl
%% ----------------------------------------------------------------------

-module(ssh_controller).

-behaviour(gen_server).


%%====================================================================
%%% Exports
%%====================================================================

%%% API
-export([start_system_subsystem/7,
         stop_system/2
        ]).

%%% Start and stop
-export([start_link/2
	]).

-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

%%====================================================================
%% Start / stop
%%====================================================================

start_link(Role, RegName) ->
    gen_server:start_link({local,RegName}, ?MODULE, [Role], []).

%%====================================================================
%% Internal application API
%%====================================================================

start_system_subsystem(Controller, Sup, Host, Port, Profile, Options, ChildSpec) ->
    gen_server:call(Controller, {start_system_subsystem, Sup, Host, Port, Profile, Options, ChildSpec}).

stop_system(Controller, SysSup) ->
    gen_server:call(Controller, {stop_system,SysSup}).

%%====================================================================
%% Internal process state
%%====================================================================
-record(data, {
          role
	 }).

%%====================================================================
%% Intitialisation
%%====================================================================
init([Role]=_Args) ->
    {ok, #data{role=Role}}.
        
%%====================================================================
%% gen_server callbacks
%%====================================================================
handle_call({start_system_subsystem, Sup, Address, Port, Profile, Options, ChildSpec}, _From, D) ->
    try
        {ok,SystemSup0} =
            case ssh_system_sup:system_supervisor(Address, Port, Profile) of
                undefined ->
                    supervisor:start_child(Sup, ChildSpec);
                Pid ->
                    {ok,Pid}
            end,
        {SystemSup0, ssh_system_sup:start_subsystem(SystemSup0, D#data.role, Address, Port, Profile, Options)}
    of
        {SystemSup, {ok,SubSysSup}} ->
             {reply, {ok,{SystemSup,SubSysSup}}, D}
    catch
        C:E:S ->
            {reply, {error,{failed,C,E,S}}, D}
    end;


handle_call({stop_system,SysSup}, _From, D) ->
    case supervisor:which_children(SysSup) of
        [] ->
            ssh_system_sup:stop_system(D#data.role, SysSup);
        _X ->
            ok
    end,
    {reply, ok, D}.




handle_cast(_Request, D) ->
    {noreply, D}.
