%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(ssh_acceptor).
-moduledoc false.

-include("ssh.hrl").

%% Internal application API
-export([start_link/4,
	 number_of_connections/1]).

%% spawn export  
-export([acceptor_init/5, acceptor_loop/9]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2, ssh_dbg_format/3]).

-define(SLEEP_TIME, 200).

%%====================================================================
%% Internal application API
%%====================================================================
%% Supposed to be called in a child-spec of the ssh_acceptor_sup
start_link(SystemSup, LSock, Address, Options) ->
    proc_lib:start_link(?MODULE, acceptor_init, [self(), SystemSup, LSock, Address, Options]).

%%%----------------------------------------------------------------
accept(ListenSocket, AcceptTimeout, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:accept(ListenSocket, AcceptTimeout).

close(Socket, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:close(Socket).
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
acceptor_init(Parent, SystemSup, LSock,
              #address{address=Address, port=Port, profile=_Profile},
              Opts) ->
    ssh_lib:set_label(server,
                      {acceptor,
                       list_to_binary(ssh_lib:format_address_port(Address, Port))}),
    proc_lib:init_ack(Parent, {ok, self()}),
    AcceptTimeout = ?GET_INTERNAL_OPT(timeout, Opts, ?DEFAULT_TIMEOUT),
    MaxSessions = ?GET_OPT(max_sessions, Opts),
    ParallelLogin = ?GET_OPT(parallel_login, Opts),
    acceptor_loop(Parent, Port, Address, Opts, LSock, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup).

%%%----------------------------------------------------------------    
acceptor_loop(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup) ->
    try
        accept(ListenSocket, AcceptTimeout, Opts)
    of
        {ok, Socket} ->
                handle_parallel_login(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup, Socket);
        {error, Error} ->
                handle_error(Error, Address, Port, undefined),
                ?MODULE:acceptor_loop(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup)
    catch
        Class:Err:Stack ->
            handle_error({error, {unhandled, Class, Err, Stack}}, Address, Port, undefined),
            ?MODULE:acceptor_loop(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup)
    end.

handle_parallel_login(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, true, SystemSup, Socket) ->
    supervisor:start_child(Parent, []),
    try_handle_connection(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, true, SystemSup, Socket);
handle_parallel_login(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup, Socket) ->
    try_handle_connection(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup, Socket).

try_handle_connection(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup, Socket) ->
    try
        PeerName = inet:peername(Socket),
        NumSessions = number_of_connections(SystemSup),
        handle_connection(Address, Port, PeerName, Opts, Socket, MaxSessions, NumSessions)
    of
        {error, Error} ->
            catch close(Socket, Opts),
            handle_error(Error, Address, Port, PeerName);
        _ ->
            ok
    catch
        Class:Err:Stack ->
            catch close(Socket, Opts),
            handle_error({error, {unhandled, Class, Err, Stack}}, Address, Port, undefined)
    end,
    handle_nonparallel_login(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup).

handle_nonparallel_login(_Parent, _Port, _Address, _Opts, _ListenSocket, _AcceptTimeout, _MaxSessions, true, _SystemSup) ->
    ok;
handle_nonparallel_login(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup) ->
    ?MODULE:acceptor_loop(Parent, Port, Address, Opts, ListenSocket, AcceptTimeout, MaxSessions, ParallelLogin, SystemSup).

%%%----------------------------------------------------------------
handle_connection(_Address, _Port, _Peer, _Options, _Socket, MaxSessions, NumSessions)
  when NumSessions >= MaxSessions->
    {error,{max_sessions,MaxSessions}};

handle_connection(_Address, _Port, {error,Error}, _Options, _Socket, _MaxSessions, _NumSessions) ->
    {error,Error};

handle_connection(Address, Port, _Peer, Options, Socket, _MaxSessions, _NumSessions) ->
    handle_connection(Address, Port, Options, Socket).



handle_connection(Address, Port, Options0, Socket) ->
    Options = ?PUT_INTERNAL_OPT([{user_pid, self()}
                                ], Options0),
    ssh_system_sup:start_subsystem(server,
                                   #address{address = Address,
                                            port = Port,
                                            profile = ?GET_OPT(profile,Options)
                                           },
                                   Socket,
                                   Options).

%%%----------------------------------------------------------------
handle_error(Reason, ToAddress, ToPort, {ok, {FromIP,FromPort}}) ->
    handle_error(Reason, ToAddress, ToPort, FromIP, FromPort);

handle_error(Reason, ToAddress, ToPort, _) ->
    handle_error(Reason, ToAddress, ToPort, undefined, undefined).


handle_error(Reason, ToAddress, ToPort, FromAddress, FromPort) ->
    case Reason of
        {max_sessions, MaxSessions} ->
            error_logger:info_report(
              lists:concat(["Ssh login attempt to ",ssh_lib:format_address_port(ToAddress,ToPort),
                            " from ",ssh_lib:format_address_port(FromAddress,FromPort),
                            " denied due to option max_sessions limits to ",
                            MaxSessions, " sessions."
                           ])
             );

        Limit when Limit==enfile ; Limit==emfile ->
            %% Out of sockets...
            error_logger:info_report([atom_to_list(Limit),": out of accept sockets on ",
                                      ssh_lib:format_address_port(ToAddress, ToPort),
                                      " - retrying"]),
            timer:sleep(?SLEEP_TIME);

        closed ->
            error_logger:info_report(["The ssh accept socket on ",ssh_lib:format_address_port(ToAddress,ToPort),
                                      "was closed by a third party."]
                                    );

        timeout ->
            ok;

        Error when is_list(Error) ->
            ok;
        Error when FromAddress=/=undefined,
                   FromPort=/=undefined ->
            error_logger:info_report(["Accept failed on ",ssh_lib:format_address_port(ToAddress,ToPort),
                                      " for connect from ",ssh_lib:format_address_port(FromAddress,FromPort),
                                      io_lib:format(": ~p", [Error])]);
        Error ->
            error_logger:info_report(["Accept failed on ",ssh_lib:format_address_port(ToAddress,ToPort),
                                      io_lib:format(": ~p", [Error])])
    end.

%%%----------------------------------------------------------------
number_of_connections(SysSupPid) ->
    lists:foldl(fun({_Ref,_Pid,supervisor,[ssh_subsystem_sup]}, N) -> N+1;
                   (_, N) -> N
                end, 0, supervisor:which_children(SysSupPid)).

%%%################################################################
%%%#
%%%# Tracing
%%%#

ssh_dbg_trace_points() -> [connections, tcp].

ssh_dbg_flags(tcp) -> [c];
ssh_dbg_flags(connections) -> [c].

ssh_dbg_on(tcp) -> dbg:tp(?MODULE, listen, 2, x),
                   dbg:tpl(?MODULE, accept, 3, x),
                   dbg:tpl(?MODULE, close, 2, x);
                   
ssh_dbg_on(connections) -> dbg:tp(?MODULE,  acceptor_init, 5, x),
                           dbg:tpl(?MODULE, handle_connection, 4, x).

ssh_dbg_off(tcp) -> dbg:ctpg(?MODULE, listen, 2),
                    dbg:ctpl(?MODULE, accept, 3),
                    dbg:ctpl(?MODULE, close, 2);

ssh_dbg_off(connections) -> dbg:ctp(?MODULE, acceptor_init, 5),
                            dbg:ctp(?MODULE, handle_connection, 4).

ssh_dbg_format(tcp, {call, {?MODULE,listen, [Port,_Opts]}}, Stack) ->
    {skip, [{port,Port}|Stack]};
ssh_dbg_format(tcp, {return_from, {?MODULE,listen,2}, {ok,Sock}}, [{port,Port}|Stack]) ->
    {["TCP listener started\n",
      io_lib:format("Port: ~p~n"
                    "ListeningSocket: ~p~n", [Port,Sock])
     ],
     Stack};
ssh_dbg_format(tcp, {return_from, {?MODULE,listen,2}, Result}, [{port,Port}|Stack]) ->
    {["TCP listener start ERROR\n",
      io_lib:format("Port: ~p~n"
                    "Return: ~p~n", [Port,Result])
     ],
     Stack};

ssh_dbg_format(tcp, {call, {?MODULE,accept, [ListenSocket, _AcceptTimeout, _Options]}}, Stack) ->
    {skip, [{lsock,ListenSocket}|Stack]};
ssh_dbg_format(tcp, {return_from, {?MODULE,accept,3}, {ok,Sock}}, [{lsock,ListenSocket}|Stack]) ->
    {["TCP accept\n",
      io_lib:format("ListenSock: ~p~n"
                    "New Socket: ~p~n", [ListenSocket,Sock])
     ], Stack};
ssh_dbg_format(tcp, {return_from, {?MODULE,accept,3}, {error,timeout}}, [{lsock,_ListenSocket}|Stack]) ->
    {skip, Stack};
ssh_dbg_format(tcp, {return_from, {?MODULE,accept,3}, Return}, [{lsock,ListenSocket}|Stack]) ->
    {["TCP accept returned\n",
      io_lib:format("ListenSock: ~p~n"
                    "Return: ~p~n", [ListenSocket,Return])
     ], Stack}.

ssh_dbg_format(tcp, {call, {?MODULE,close, [Socket, _Options]}}) ->
    ["TCP close listen socket\n",
     io_lib:format("Socket: ~p~n", [Socket])];
ssh_dbg_format(tcp, {return_from, {?MODULE,close,2}, _Return}) ->
    skip;

ssh_dbg_format(connections, {call, {?MODULE,acceptor_init, [_Parent, _SysSup, _LSock, Address, _Opts]}}) ->
    [io_lib:format("Starting LISTENER on ~s\n", [ssh_lib:format_address(Address)])
    ];
ssh_dbg_format(connections, {return_from, {?MODULE,acceptor_init,5}, _Ret}) ->
    skip;

ssh_dbg_format(connections, {call, {?MODULE,handle_connection,[_Address,_Port,_Options,_Sock]}}) ->
    skip;
ssh_dbg_format(connections, {return_from, {?MODULE,handle_connection,4}, {error,Error}}) ->
    ["Starting connection to server failed:\n",
     io_lib:format("Error = ~p", [Error])
    ].
