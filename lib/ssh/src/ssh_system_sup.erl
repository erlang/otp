%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% Purpose: The ssh server instance supervisor, an instans of this supervisor
%%          exists for every ip-address and port combination, hangs under
%%          sshd_sup.
%%----------------------------------------------------------------------

-module(ssh_system_sup).
-moduledoc false.

-behaviour(supervisor).

-include("ssh.hrl").

-export([start_link/2,
         stop_listener/1,
	 stop_system/1,
         start_system/2,
         start_connection/4,
	 get_daemon_listen_address/1,
         addresses/1,
         get_options/2,
         get_acceptor_options/1,
         restart_acceptor/2
        ]).

%% Supervisor callback
-export([init/1]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1,
         ssh_dbg_format/2]).

%%%=========================================================================
%%% API
%%%=========================================================================

start_system(Address0, Options) ->
    case find_system_sup(Address0) of
        {ok,{SysPid,Address}} ->
            start_acceptor(SysPid, Address, Options);
        {error,not_found} ->
            supervisor:start_child(sshd_sup,
                                   #{id       => {?MODULE,Address0},
                                     start    => {?MODULE, start_link, [Address0, Options]},
                                     restart  => temporary,
                                     type     => supervisor
                                    })
    end.

%%%----------------------------------------------------------------
stop_system(SysSup) when is_pid(SysSup) ->
    case lists:keyfind(SysSup, 2, supervisor:which_children(sup(server))) of
        {{?MODULE, Id}, SysSup, _, _} -> stop_system(Id);
        false -> ok
    end;
stop_system(Id) ->
    supervisor:terminate_child(sup(server), {?MODULE, Id}).


%%%----------------------------------------------------------------
stop_listener(SystemSup) when is_pid(SystemSup) ->
    {Id, _, _, _} = lookup(ssh_acceptor_sup, SystemSup),
    supervisor:terminate_child(SystemSup, Id),
    supervisor:delete_child(SystemSup, Id).

%%%----------------------------------------------------------------
get_daemon_listen_address(SystemSup) ->
    try lookup(ssh_acceptor_sup, SystemSup)
    of
        {{ssh_acceptor_sup,Address}, _, _, _} ->
            {ok, Address};
        _ ->
            {error, not_found}
    catch
        _:_ ->
            {error, not_found}
    end.

%%%----------------------------------------------------------------
%%% Start the connection child. It is a significant child of the system
%%% supervisor (callback = this module) for server and non-significant
%%% child of sshc_sup for client
start_connection(Role = client, _, Socket, Options) ->
    do_start_connection(Role, sup(client), false, Socket, Options);
start_connection(Role = server, Address=#address{}, Socket, Options) ->
    case get_system_sup(Address, Options) of
        {ok, SysPid} ->
            do_start_connection(Role, SysPid, true, Socket, Options);
        Others ->
            Others
    end.

do_start_connection(Role, SupPid, Significant, Socket, Options0) ->
    Id = make_ref(),
    Options = ?PUT_INTERNAL_OPT([{user_pid, self()}], Options0),
    case supervisor:start_child(SupPid,
                                #{id          => Id,
                                  start       => {ssh_connection_sup, start_link,
                                                  [Role,Id,Socket,Options]
                                                 },
                                  restart     => temporary,
                                  significant => Significant,
                                  type        => supervisor
                                 })
    of
        {ok,_ConnectionSupPid} ->
            try
                receive
                    {new_connection_ref, Id, ConnPid} ->
                        ssh_connection_handler:takeover(ConnPid, Role, Socket, Options)
                after 10000 ->
                        error(timeout)
                end
            catch
                error:{badmatch,{error,Error}} ->
                    {error,Error};
                error:timeout ->
                    %% The connection was started, but the takover procedure timed out,
                    %% therefore it exists a subtree, but it is not quite ready and
                    %% must be removed (by the supervisor above):
                    supervisor:terminate_child(SupPid, Id),
                    {error, connection_start_timeout}
            end;
        Others ->
            Others
    end.

%%%----------------------------------------------------------------
start_link(Address, Options) ->
    supervisor:start_link(?MODULE, [Address, Options]).

%%%----------------------------------------------------------------
addresses(#address{address=Address, port=Port, profile=Profile}) ->
    [{SysSup,A} || {{ssh_system_sup,A},SysSup,supervisor,_} <-
                     supervisor:which_children(sshd_sup),
                 Address == any orelse A#address.address == Address,
                 Port == any    orelse A#address.port == Port,
                 Profile == any orelse A#address.profile == Profile].

%%%----------------------------------------------------------------
%% SysPid is the DaemonRef
get_acceptor_options(SysPid) ->
    case get_daemon_listen_address(SysPid) of
        {ok,Address} ->
            get_options(SysPid, Address);
        {error,not_found} ->
            {error,bad_daemon_ref}
    end.

restart_acceptor(SysPid, Options0) ->
    case get_daemon_listen_address(SysPid) of
        {ok,Address} ->
            try
                stop_listener(SysPid)
            of
                ok ->
                    Options = refresh_lsocket(Options0),
                    start_acceptor(SysPid, Address, Options)
            catch
                error:_Error ->
                    Options = refresh_lsocket(Options0),
                    start_acceptor(SysPid, Address, Options)
            end;
        {error,Error} ->
            {error,Error}
    end.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([Address, Options]) ->
    ssh_lib:set_label(server, system_sup),
    SupFlags = #{strategy      => one_for_one,
                 auto_shutdown => all_significant,
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs =
        case is_socket_server(Options) of
            false ->
                [acceptor_sup_child_spec(_SysSup=self(), Address, Options)];
            _ ->
                []
        end,
    {ok, {SupFlags,ChildSpecs}}.

%%%=========================================================================
%%% Service API
%%%=========================================================================

%% A macro to keep get_options/2 and acceptor_sup_child_spec/3 synchronized
-define(accsup_start(SysSup,Addr,Opts),
        {ssh_acceptor_sup, start_link, [SysSup,Addr,Opts]}
       ).

get_options(Sup, Address = #address{}) ->
    %% Lookup the Option parameter in the running ssh_acceptor_sup:
    try
        {ok, #{start:=?accsup_start(_, _, Options)}} =
            supervisor:get_childspec(Sup, {ssh_acceptor_sup,Address}),
        {ok, Options}
    catch
        _:_ -> {error,not_found}
    end.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================

%% A separate function because this spec is need in >1 places
acceptor_sup_child_spec(SysSup, Address, Options) ->
    #{id       => {ssh_acceptor_sup,Address},
      start    => ?accsup_start(SysSup, Address, Options),
      restart  => transient,
      significant => true,
      type     => supervisor
     }.

lookup(SupModule, SystemSup) ->
    lists:keyfind([SupModule], 4, supervisor:which_children(SystemSup)).

get_system_sup(Address0, Options) ->
    case find_system_sup(Address0) of
        {ok,{SysPid,_Address}} ->
            {ok,SysPid};
        {error,not_found} ->
            start_system(Address0, Options);
        {error,Error} ->
            {error,Error}
    end.

find_system_sup(Address0) ->
    case addresses(Address0) of
        [{SysSupPid,Address}] ->
            {ok,{SysSupPid,Address}};
        [] -> {error,not_found};
        [_,_|_] -> {error,ambiguous}
    end.

sup(client) -> sshc_sup;
sup(server) -> sshd_sup.


is_socket_server(Options) ->
    undefined =/= ?GET_INTERNAL_OPT(connected_socket,Options,undefined).

start_acceptor(SysPid, Address, Options) ->
    case lookup(ssh_acceptor_sup, SysPid) of
        {_,_,supervisor,_} ->
            {error, eaddrinuse};
        false ->
            ChildSpec = acceptor_sup_child_spec(SysPid, Address, Options),
            case supervisor:start_child(SysPid, ChildSpec) of
                {ok,_ChildPid} ->
                    {ok,SysPid}; % sic!
                {error,Error} ->
                    {error,Error}
            end
    end.

refresh_lsocket(Options0) ->
    {_OldLSock, LHost, LPort, _SockOwner} =
        ?GET_INTERNAL_OPT(lsocket, Options0, lsocket_undefined),
    case ssh_lsocket:get_lsocket(LHost, LPort, Options0) of
        {ok, {LSocketProvider, LSocket}} ->
            {_Host, _Port, Options} =
                ssh:update_lsocket(LSocket, LSocketProvider, Options0),
            Options;
        Error = {error, _} ->
            Error
    end.

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [connections].

ssh_dbg_flags(connections) -> [c].

ssh_dbg_on(connections) ->
    dbg:tpl(?MODULE, stop_listener, 1, x),
    dbg:tpl(?MODULE, start_acceptor, 3, x).

ssh_dbg_off(connections) ->
    dbg:ctpl(?MODULE, stop_listener, 1),
    dbg:ctpl(?MODULE, start_acceptor, 3).

ssh_dbg_format(Tracepoint, Event = {call, {?MODULE, Function, Args}}) ->
    [io_lib:format("~w:~w/~w> ~s", [?MODULE, Function, length(Args)] ++
                       ssh_dbg_comment(Tracepoint, Event))];
ssh_dbg_format(Tracepoint, Event = {return_from, {?MODULE,Function,Arity}, Ret}) ->
    [io_lib:format("~w:~w/~w returned ~W> ~s", [?MODULE, Function, Arity, Ret, 3] ++
                  ssh_dbg_comment(Tracepoint, Event))].

ssh_dbg_comment(_, _) ->
    [""].
