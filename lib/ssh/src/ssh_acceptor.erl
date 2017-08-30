%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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
-export([start_link/5,
	 number_of_connections/1,
	 callback_listen/3,
	 handle_connection/5]).

%% spawn export  
-export([acceptor_init/6, acceptor_loop/6]).

-define(SLEEP_TIME, 200).

%%====================================================================
%% Internal application API
%%====================================================================
start_link(Port, Address, SockOpts, Opts, AcceptTimeout) ->
    Args = [self(), Port, Address, SockOpts, Opts, AcceptTimeout],
    proc_lib:start_link(?MODULE, acceptor_init, Args).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
acceptor_init(Parent, Port, Address, SockOpts, Opts, AcceptTimeout) ->
    {_, Callback, _} =  
	proplists:get_value(transport, Opts, {tcp, gen_tcp, tcp_closed}),

    SockOwner = proplists:get_value(lsock_owner, Opts),
    LSock = proplists:get_value(lsocket, Opts),
    UseExistingSocket =
	case catch inet:sockname(LSock) of
	    {ok,{_,Port}} -> is_pid(SockOwner);
	    _ -> false
	end,

    case UseExistingSocket of
	true ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    request_ownership(LSock, SockOwner),
	    acceptor_loop(Callback, Port, Address, Opts, LSock, AcceptTimeout);

	false -> 
	    case (catch do_socket_listen(Callback, Port, SockOpts)) of
		{ok, ListenSocket} ->
		    proc_lib:init_ack(Parent, {ok, self()}),
		    acceptor_loop(Callback, 
				  Port, Address, Opts, ListenSocket, AcceptTimeout);
		Error ->
		    proc_lib:init_ack(Parent, Error),
		    error
	    end
    end.

request_ownership(LSock, SockOwner) ->
    SockOwner ! {request_control,LSock,self()},
    receive
	{its_yours,LSock} -> ok
    end.
    
   
do_socket_listen(Callback, Port0, Opts) ->
    Port =
	case proplists:get_value(fd, Opts) of
	    undefined -> Port0;
	    _ -> 0
	end,
    callback_listen(Callback, Port, Opts).

callback_listen(Callback, Port, Opts0) ->
    Opts = [{active, false}, {reuseaddr,true} | Opts0],
    case Callback:listen(Port, Opts) of
	{error, nxdomain} ->
	    Callback:listen(Port, lists:delete(inet6, Opts));
	{error, enetunreach} ->
	    Callback:listen(Port, lists:delete(inet6, Opts));
	{error, eafnosupport} ->
	    Callback:listen(Port, lists:delete(inet6, Opts));
	Other ->
	    Other
    end.
    
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

handle_connection(Callback, Address, Port, Options, Socket) ->
    SSHopts = proplists:get_value(ssh_opts, Options, []),
    Profile =  proplists:get_value(profile, SSHopts, ?DEFAULT_PROFILE),
    SystemSup = ssh_system_sup:system_supervisor(Address, Port, Profile),

    MaxSessions = proplists:get_value(max_sessions,SSHopts,infinity),
    case number_of_connections(SystemSup) < MaxSessions of
	true ->
	    {ok, SubSysSup} = ssh_system_sup:start_subsystem(SystemSup, Options),
	    ConnectionSup = ssh_subsystem_sup:connection_supervisor(SubSysSup),
	    Timeout = proplists:get_value(negotiation_timeout, SSHopts, 2*60*1000),
	    ssh_connection_handler:start_connection(server, Socket,
						    [{supervisors, [{system_sup, SystemSup},
								    {subsystem_sup, SubSysSup},
								    {connection_sup, ConnectionSup}]}
						     | Options], Timeout);
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


number_of_connections(SystemSup) ->
    length([X || 
	       {R,X,supervisor,[ssh_subsystem_sup]} <- supervisor:which_children(SystemSup),
	       is_pid(X),
	       is_reference(R)
	  ]).
