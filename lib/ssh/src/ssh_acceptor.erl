%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-include("ssh.hrl").

%% Internal application API
-export([start_link/4,
	 number_of_connections/1,
	 listen/2,
	 handle_established_connection/4]).

%% spawn export  
-export([acceptor_init/5, acceptor_loop/6]).

-export([dbg_trace/3]).

-define(SLEEP_TIME, 200).

%%====================================================================
%% Internal application API
%%====================================================================
start_link(Port, Address, Options, AcceptTimeout) ->
    Args = [self(), Port, Address, Options, AcceptTimeout],
    proc_lib:start_link(?MODULE, acceptor_init, Args).

%%%----------------------------------------------------------------
number_of_connections(SystemSup) ->
    length([X || 
	       {R,X,supervisor,[ssh_subsystem_sup]} <- supervisor:which_children(SystemSup),
	       is_pid(X),
	       is_reference(R)
	  ]).

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

%%%----------------------------------------------------------------
handle_established_connection(Address, Port, Options, Socket) ->
    {_, Callback, _} = ?GET_OPT(transport, Options),
    handle_connection(Callback, Address, Port, Options, Socket).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
acceptor_init(Parent, Port, Address, Opts, AcceptTimeout) ->
    try
        ?GET_INTERNAL_OPT(lsocket, Opts)
    of
        {LSock, SockOwner} ->
            case inet:sockname(LSock) of
                {ok,{_,Port}} -> % A usable, open LSock
                    proc_lib:init_ack(Parent, {ok, self()}),
                    request_ownership(LSock, SockOwner),
                    {_, Callback, _} =  ?GET_OPT(transport, Opts),
                    acceptor_loop(Callback, Port, Address, Opts, LSock, AcceptTimeout);

                {error,_} -> % Not open, a restart
                    %% Allow gen_tcp:listen to fail 4 times if eaddrinuse:
                    {ok,NewLSock} = try_listen(Port, Opts, 4),
                    proc_lib:init_ack(Parent, {ok, self()}),
                    Opts1 = ?DELETE_INTERNAL_OPT(lsocket, Opts),
                    {_, Callback, _} =  ?GET_OPT(transport, Opts1),
                    acceptor_loop(Callback, Port, Address, Opts1, NewLSock, AcceptTimeout)
            end
    catch
        _:_ ->
            {error,use_existing_socket_failed}
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
acceptor_loop(Callback, Port, Address, Opts, ListenSocket, AcceptTimeout) ->
    case (catch Callback:accept(ListenSocket, AcceptTimeout)) of
	{ok, Socket} ->
	    handle_connection(Callback, Address, Port, Opts, Socket),
	    ?MODULE:acceptor_loop(Callback, Port, Address, Opts,
				  ListenSocket, AcceptTimeout);
	{error, Reason} ->
	    handle_error(Reason),
	    ?MODULE:acceptor_loop(Callback, Port, Address, Opts,
				  ListenSocket, AcceptTimeout);
	{'EXIT', Reason} ->
	    handle_error(Reason),
	    ?MODULE:acceptor_loop(Callback, Port, Address, Opts,
				  ListenSocket, AcceptTimeout)
    end.

%%%----------------------------------------------------------------
handle_connection(Callback, Address, Port, Options, Socket) ->
    Profile =  ?GET_OPT(profile, Options),
    SystemSup = ssh_system_sup:system_supervisor(Address, Port, Profile),

    MaxSessions = ?GET_OPT(max_sessions, Options),
    case number_of_connections(SystemSup) < MaxSessions of
	true ->
	    {ok, SubSysSup} = 
                ssh_system_sup:start_subsystem(SystemSup, server, Address, Port, Profile, Options),
	    ConnectionSup = ssh_subsystem_sup:connection_supervisor(SubSysSup),
	    NegTimeout = ?GET_OPT(negotiation_timeout, Options),
	    ssh_connection_handler:start_connection(server, Socket,
                                                    ?PUT_INTERNAL_OPT(
                                                       {supervisors, [{system_sup, SystemSup},
                                                                      {subsystem_sup, SubSysSup},
                                                                      {connection_sup, ConnectionSup}]},
                                                       Options), NegTimeout);
	false ->
	    Callback:close(Socket),
	    IPstr = if is_tuple(Address) -> inet:ntoa(Address);
		     true -> Address
		  end,
	    Str = try io_lib:format('~s:~p',[IPstr,Port])
		  catch _:_ -> "port "++integer_to_list(Port)
		  end,
	    error_logger:info_report("Ssh login attempt to "++Str++" denied due to option "
				     "max_sessions limits to "++ io_lib:write(MaxSessions) ++
				     " sessions."
				     ),
	    {error,max_sessions}
    end.

%%%----------------------------------------------------------------
handle_error(timeout) ->
    ok;

handle_error(enfile) ->
    %% Out of sockets...
    timer:sleep(?SLEEP_TIME);

handle_error(emfile) ->
    %% Too many open files -> Out of sockets...
    timer:sleep(?SLEEP_TIME);

handle_error(closed) ->
    error_logger:info_report("The ssh accept socket was closed by " 
			     "a third party. "
			     "This will not have an impact on ssh "
			     "that will open a new accept socket and " 
			     "go on as nothing happened. It does however "
			     "indicate that some other software is behaving "
			     "badly."),
    exit(normal);

handle_error(Reason) ->
    String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
    error_logger:error_report(String),
    exit({accept_failed, String}).    

%%%################################################################
%%%#
%%%# Tracing
%%%#

dbg_trace(points,         _,  _) -> [connections];

dbg_trace(flags,  connections,  _) -> [c];
dbg_trace(on,     connections,  _) -> dbg:tp(?MODULE,  acceptor_init, 5, x),
                                      dbg:tpl(?MODULE, handle_connection, 5, x);
dbg_trace(off,    connections,  _) -> dbg:ctp(?MODULE, acceptor_init, 5),
                                      dbg:ctp(?MODULE, handle_connection, 5);
dbg_trace(format, connections, {call, {?MODULE,acceptor_init,
                                       [_Parent, Port, Address, _Opts, _AcceptTimeout]}}) ->
    [io_lib:format("Starting LISTENER on ~s:~p\n", [ntoa(Address),Port])
    ];
dbg_trace(format, connections, {return_from, {?MODULE,handle_connection,5}, {error,Error}}) ->
    ["Starting connection to server failed:\n",
     io_lib:format("Error = ~p", [Error])
    ].



ntoa(A) ->
    try inet:ntoa(A)
    catch
        _:_ when is_list(A) -> A;
        _:_ -> io_lib:format('~p',[A])
    end.
            
