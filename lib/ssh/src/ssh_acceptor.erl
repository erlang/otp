%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

-module(ssh_acceptor).

%% Internal application API
-export([start_link/5,
	 number_of_connections/1]).

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
    case (catch do_socket_listen(Callback, Port, SockOpts)) of
	{ok, ListenSocket} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    acceptor_loop(Callback, 
			  Port, Address, Opts, ListenSocket, AcceptTimeout);
	Error ->
	    proc_lib:init_ack(Parent, Error),
	    error
    end.
   
do_socket_listen(Callback, Port, Opts) ->
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
    SystemSup = ssh_system_sup:system_supervisor(Address, Port),
    SSHopts = proplists:get_value(ssh_opts, Options, []),
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
