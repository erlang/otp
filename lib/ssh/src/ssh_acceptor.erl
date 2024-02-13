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
-export([start_link/3,
	 number_of_connections/1,
	 listen/2]).

%% spawn export  
-export([acceptor_init/4, acceptor_loop/6]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2, ssh_dbg_format/3]).

-define(SLEEP_TIME, 200).

%%====================================================================
%% Internal application API
%%====================================================================
%% Supposed to be called in a child-spec of the ssh_acceptor_sup
start_link(SystemSup, Address, Options) ->
    proc_lib:start_link(?MODULE, acceptor_init, [self(),SystemSup,Address,Options]).

%%%----------------------------------------------------------------
listen(Port, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    SockOpts = [{active, false}, {reuseaddr,true} | ?GET_OPT(socket_options, Options)],
    case Callback:listen(Port, SockOpts) of
	{error, nxdomain} ->
	    Callback:listen(Port, lists:delete(inet6, SockOpts));
	{error, enetunreach} ->
	    Callback:listen(Port, lists:delete(inet6, SockOpts));
	{error, eafnosupport} ->
	    Callback:listen(Port, lists:delete(inet6, SockOpts));
	Other ->
	    Other
    end.

accept(ListenSocket, AcceptTimeout, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:accept(ListenSocket, AcceptTimeout).

close(Socket, Options) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    Callback:close(Socket).
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
acceptor_init(Parent, SystemSup,
              #address{address=Address, port=Port, profile=_Profile},
              Opts) ->
    AcceptTimeout = ?GET_INTERNAL_OPT(timeout, Opts, ?DEFAULT_TIMEOUT),
    case ?GET_INTERNAL_OPT(lsocket, Opts, undefined) of
        {LSock, SockOwner} ->
            %% A listening socket (or fd option) was provided in the ssh:daemon call
            case inet:sockname(LSock) of
                {ok,{_,Port}} ->
                    %% A usable, open LSock
                    proc_lib:init_ack(Parent, {ok, self()}),
                    request_ownership(LSock, SockOwner),
                    acceptor_loop(Port, Address, Opts, LSock, AcceptTimeout, SystemSup);

                {error,_Error} ->
                    %% Not open, a restart
                    %% Allow gen_tcp:listen to fail 4 times if eaddrinuse (It is a bug fix):
                    case try_listen(Port, Opts, 4) of
                        {ok,NewLSock} ->
                            proc_lib:init_ack(Parent, {ok, self()}),
                            Opts1 = ?DELETE_INTERNAL_OPT(lsocket, Opts),
                            acceptor_loop(Port, Address, Opts1, NewLSock, AcceptTimeout, SystemSup);
                        {error,Error} ->
                            proc_lib:init_fail(Parent, {error,Error}, {exit, normal})
                    end
            end;

        undefined ->
            %% No listening socket (nor fd option) was provided; open a listening socket:
            case listen(Port, Opts) of
                {ok,LSock} ->
                    proc_lib:init_ack(Parent, {ok, self()}),
                    acceptor_loop(Port, Address, Opts, LSock, AcceptTimeout, SystemSup);
                {error,Error} ->
                    proc_lib:init_fail(Parent, {error,Error}, {exit, normal})
            end
    end.


try_listen(Port, Opts, NtriesLeft) ->
    try_listen(Port, Opts, 1, NtriesLeft).

try_listen(Port, Opts, N, Nmax) ->
    case listen(Port, Opts) of
        {error,eaddrinuse} when N<Nmax ->
            timer:sleep(10*N), % Sleep 10, 20, 30,... ms
            try_listen(Port, Opts, N+1, Nmax);
        Other ->
            Other
    end.


request_ownership(LSock, SockOwner) ->
    SockOwner ! {request_control,LSock,self()},
    receive
	{its_yours,LSock} -> ok
    end.
    
%%%----------------------------------------------------------------    
acceptor_loop(Port, Address, Opts, ListenSocket, AcceptTimeout, SystemSup) ->
    try
        case accept(ListenSocket, AcceptTimeout, Opts) of
            {ok,Socket} ->
                PeerName = inet:peername(Socket),
                MaxSessions = ?GET_OPT(max_sessions, Opts),
                NumSessions = number_of_connections(SystemSup),
                ParallelLogin = ?GET_OPT(parallel_login, Opts),
                case handle_connection(Address, Port, PeerName, Opts, Socket, MaxSessions, NumSessions, ParallelLogin) of
                    {error,Error} ->
                        catch close(Socket, Opts),
                        handle_error(Error, Address, Port, PeerName);
                    _ ->
                        ok
                end;
            {error,Error} ->
                handle_error(Error, Address, Port, undefined)
        end
    catch
        Class:Err:Stack ->
            handle_error({error, {unhandled,Class,Err,Stack}}, Address, Port, undefined)
    end,
    ?MODULE:acceptor_loop(Port, Address, Opts, ListenSocket, AcceptTimeout, SystemSup).

%%%----------------------------------------------------------------
handle_connection(_Address, _Port, _Peer, _Options, _Socket, MaxSessions, NumSessions, _ParallelLogin)
  when NumSessions >= MaxSessions->
    {error,{max_sessions,MaxSessions}};

handle_connection(_Address, _Port, {error,Error}, _Options, _Socket, _MaxSessions, _NumSessions, _ParallelLogin) ->
    {error,Error};

handle_connection(Address, Port, _Peer, Options, Socket, _MaxSessions, _NumSessions, ParallelLogin)
  when ParallelLogin == false ->
    handle_connection(Address, Port, Options, Socket);

handle_connection(Address, Port, _Peer, Options, Socket, _MaxSessions, _NumSessions, ParallelLogin)
  when ParallelLogin == true ->
    Ref = make_ref(),
    Pid = spawn_link(
            fun() ->
                    process_flag(trap_exit, true),
                    receive
                        {start,Ref} ->
                            handle_connection(Address, Port, Options, Socket)
                    after 10000 ->
                            {error, timeout2}
                    end
            end),
    catch gen_tcp:controlling_process(Socket, Pid),
    Pid ! {start,Ref},
    ok.



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
                   
ssh_dbg_on(connections) -> dbg:tp(?MODULE,  acceptor_init, 4, x),
                           dbg:tpl(?MODULE, handle_connection, 4, x).

ssh_dbg_off(tcp) -> dbg:ctpg(?MODULE, listen, 2),
                    dbg:ctpl(?MODULE, accept, 3),
                    dbg:ctpl(?MODULE, close, 2);

ssh_dbg_off(connections) -> dbg:ctp(?MODULE, acceptor_init, 4),
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

ssh_dbg_format(connections, {call, {?MODULE,acceptor_init, [_Parent, _SysSup, Address, _Opts]}}) ->
    [io_lib:format("Starting LISTENER on ~s\n", [ssh_lib:format_address(Address)])
    ];
ssh_dbg_format(connections, {return_from, {?MODULE,acceptor_init,4}, _Ret}) ->
    skip;

ssh_dbg_format(connections, {call, {?MODULE,handle_connection,[_Address,_Port,_Options,_Sock]}}) ->
    skip;
ssh_dbg_format(connections, {return_from, {?MODULE,handle_connection,4}, {error,Error}}) ->
    ["Starting connection to server failed:\n",
     io_lib:format("Error = ~p", [Error])
    ].
