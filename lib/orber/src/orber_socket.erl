%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
%%%% %CopyrightEnd%
%%
%%
%%-----------------------------------------------------------------
%% File: orber_socket.erl
%% 
%% Description:
%%    This file contains a standard interface to the sockets to handle the differences
%%    between the implementations used.
%%
%%-----------------------------------------------------------------
-module(orber_socket).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, connect/4, listen/3, listen/4, accept/2, accept/3, write/3,
	 controlling_process/3, close/2, peername/2, sockname/2, 
	 peerdata/2, peercert/2, sockdata/2, setopts/3,
	 clear/2, shutdown/3, post_accept/2, post_accept/3,
	 get_ip_family_opts/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Internal defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 6).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start() ->	
    inet_db:start().

%%-----------------------------------------------------------------
%% Invoke the required setopts (i.e., inet or ssl)
setopts(normal, Socket, Opts) ->
    inet:setopts(Socket, Opts);
setopts(ssl, Socket, Opts) ->
    ssl:setopts(Socket, Opts).

%%-----------------------------------------------------------------
%% Connect to IIOP Port at Host in CDR mode, in order to 
%% establish a connection.
%%
connect(Type, Host, Port, Options) ->
    Timeout = orber:iiop_setup_connection_timeout(),
    Generation = orber_env:ssl_generation(),
    Options1 = check_options(Type, Options, Generation),
    Options2 = 
	case Type of
	    normal ->
		[{keepalive, orber_env:iiop_out_keepalive()}|Options1];
	    _ ->
		Options1
	end,
    case orber:iiop_out_ports() of
	{Min, Max} when Type == normal ->
	    multi_connect(get_port_sequence(Min, Max), orber_env:iiop_out_ports_attempts(),
			  Type, Host, Port, [binary, {reuseaddr, true}, 
					     {packet,cdr}| Options2], Timeout);
	{Min, Max} when Generation > 2 ->
	    multi_connect(get_port_sequence(Min, Max), orber_env:iiop_out_ports_attempts(),
			  Type, Host, Port, [binary, {reuseaddr, true}, 
					     {packet,cdr}| Options2], Timeout);
	{Min, Max} ->
	    %% reuseaddr not available for older SSL versions
	    multi_connect(get_port_sequence(Min, Max), orber_env:iiop_out_ports_attempts(), 
			  Type, Host, Port, [binary, {packet,cdr}| Options2], Timeout);
	_ ->
	    connect(Type, Host, Port, [binary, {packet,cdr}| Options2], Timeout)
    end.

connect(normal, Host, Port, Options, Timeout) ->
    case catch gen_tcp:connect(Host, Port, Options, Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} ->
	    orber:dbg("[~p] orber_socket:connect(normal, ~p, ~p, ~p);~n"
		      "Timeout after ~p msec.", 
		      [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4),
					completion_status=?COMPLETED_NO});
	Error ->
	    orber:dbg("[~p] orber_socket:connect(normal, ~p, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
connect(ssl, Host, Port, Options, Timeout) ->
    case catch ssl:connect(Host, Port, Options, Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} ->
	    orber:dbg("[~p] orber_socket:connect(ssl, ~p, ~p, ~p);~n"
		      "Timeout after ~p msec.", 
		      [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4), 
					completion_status=?COMPLETED_NO});
	Error ->
	    orber:dbg("[~p] orber_socket:connect(ssl, ~p, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

multi_connect([], _Retries, Type, Host, Port, Options, _) ->
    orber:dbg("[~p] orber_socket:multi_connect(~p, ~p, ~p, ~p);~n"
	      "Unable to use any of the sockets defined by 'iiop_out_ports'.~n"
	      "Either all ports are in use or to many connections already exists.", 
	      [?LINE, Type, Host, Port, Options], ?DEBUG_LEVEL),
    corba:raise(#'IMP_LIMIT'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
multi_connect([CurrentPort|Rest], Retries, normal, Host, Port, Options, Timeout) ->
    case catch gen_tcp:connect(Host, Port, [{port, CurrentPort}|Options], Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} when Retries =< 1 ->
	    orber:dbg("[~p] orber_socket:multi_connect(normal, ~p, ~p, ~p);~n"
		      "Timeout after ~p msec.", 
		      [?LINE, Host, Port, [{port, CurrentPort}|Options],
		       Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4),
					completion_status=?COMPLETED_NO});
	_ ->
	    multi_connect(Rest, Retries - 1, normal, Host, Port, Options, Timeout)
    end;
multi_connect([CurrentPort|Rest], Retries, ssl, Host, Port, Options, Timeout) ->
    case catch ssl:connect(Host, Port, [{port, CurrentPort}|Options], Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} when Retries =< 1 ->
	    orber:dbg("[~p] orber_socket:multi_connect(ssl, ~p, ~p, ~p);~n"
		      "Timeout after ~p msec.", 
		      [?LINE, Host, Port, [{port, CurrentPort}|Options], 
		       Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4), 
					completion_status=?COMPLETED_NO});
	_ ->
	    multi_connect(Rest, Retries - 1, ssl, Host, Port, Options, Timeout)
    end.
  

get_port_sequence(Min, Max) ->
    case orber_env:iiop_out_ports_random() of
	true ->
	    {A1,A2,A3} = now(),
	    random:seed(A1, A2, A3),
	    Seq = lists:seq(Min, Max),
	    random_sequence((Max - Min) + 1, Seq, []);
	_ ->
	    lists:seq(Min, Max)
    end.

random_sequence(0, _, Acc) ->
    Acc;
random_sequence(Length, Seq, Acc) ->
    Nth = random:uniform(Length),
    Value = lists:nth(Nth, Seq),
    NewSeq = lists:delete(Value, Seq),
    random_sequence(Length-1, NewSeq, [Value|Acc]).

%%-----------------------------------------------------------------
%% Create a listen socket at Port in CDR mode for 
%% data connection.
%%
listen(Type, Port, Options) ->
    listen(Type, Port, Options, true).

listen(normal, Port, Options, Exception) ->
    Options1 = check_options(normal, Options, 0),
    Backlog = orber:iiop_backlog(),
    Keepalive = orber_env:iiop_in_keepalive(),
    Options2 = case orber:iiop_max_in_requests() of
		   infinity ->
		       Options1;
		   _MaxRequests ->
		       [{active, once}|Options1]
	       end,
    Options3 = case orber_env:iiop_packet_size() of
		   infinity ->
		       Options2;
		   MaxSize ->
		       [{packet_size, MaxSize}|Options2]
	       end,
    Options4 = [binary, {packet,cdr}, {keepalive, Keepalive},
				     {reuseaddr,true}, {backlog, Backlog} |
				     Options3],

    case catch gen_tcp:listen(Port, Options4) of
	{ok, ListenSocket} ->
	    {ok, ListenSocket, check_port(Port, normal, ListenSocket)};
	{error, Reason} when Exception == false ->
	    {error, Reason};
 	{error, eaddrinuse} ->
	    orber:dbg("[~p] orber_socket:listen(normal, ~p, ~p);~n"
		      "Looks like the listen port is already in use.~n"
		      "Check if another Orber is started~n"
		      "on the same node and uses the same listen port (iiop_port). But it may also~n"
		      "be used by any other application; confirm with 'netstat'.",
		      [?LINE, Port, Options4], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
	Error ->
	    orber:dbg("[~p] orber_socket:listen(normal, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Port, Options4, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
listen(ssl, Port, Options, Exception) ->
    Backlog = orber:iiop_ssl_backlog(),
    Generation = orber_env:ssl_generation(),
    Options1 = check_options(ssl, Options, Generation),
    Options2 = case orber:iiop_max_in_requests() of
		   infinity ->
		       Options1;
		   _MaxRequests ->
		       [{active, once}|Options1]
	       end,
    Options3 = case orber_env:iiop_packet_size() of
		   infinity ->
		       Options2;
		   MaxSize ->
		       [{packet_size, MaxSize}|Options2]
	       end,
    Options4 = if
		   Generation > 2 ->
		       [{reuseaddr, true} |Options3];
		   true ->
		       Options3
	       end,
    Options5 = [binary, {packet,cdr}, {backlog, Backlog} | Options4],
    case catch ssl:listen(Port, Options5) of
	{ok, ListenSocket} ->
	    {ok, ListenSocket, check_port(Port, ssl, ListenSocket)};
	{error, Reason} when Exception == false ->
	    {error, Reason};
	{error, eaddrinuse} ->	
	    orber:dbg("[~p] orber_socket:listen(ssl, ~p, ~p);~n"
		      "Looks like the listen port is already in use. Check if~n"
		      "another Orber is started on the same node and uses the~n"
		      "same listen port (iiop_port). But it may also~n"
		      "be used by any other application; confirm with 'netstat'.",
		      [?LINE, Port, Options5], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
	Error ->
	    orber:dbg("[~p] orber_socket:listen(ssl, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Port, Options5, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------------
%% Wait in accept on the socket
%% 
accept(Type, ListenSocket) ->
    accept(Type, ListenSocket, infinity).

accept(normal, ListenSocket, _Timeout) ->
    case catch gen_tcp:accept(ListenSocket) of
	{ok, S} ->
	    S;
	Error ->
	    orber:dbg("[~p] orber_socket:accept(normal, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
accept(ssl, ListenSocket, Timeout) ->
    case catch ssl:transport_accept(ListenSocket, Timeout) of
	{ok, S} ->
	    S;
	Error ->
	    orber:dbg("[~p] orber_socket:accept(ssl, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

post_accept(Type, Socket) ->
    post_accept(Type, Socket, infinity).

post_accept(normal, _Socket, _Timeout) ->
    ok;
post_accept(ssl, Socket, Timeout) ->
    case catch ssl:ssl_accept(Socket, Timeout) of
	ok ->
	    ok;
	Error ->
	    orber:dbg("[~p] orber_socket:post_accept(ssl, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Socket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.


%%-----------------------------------------------------------------
%% Close the socket
%% 
close(normal, Socket) ->
    (catch gen_tcp:close(Socket));
close(ssl, Socket) ->
    (catch ssl:close(Socket)).

%%-----------------------------------------------------------------
%% Write to socket
%% 
write(normal, Socket, Bytes) ->
    gen_tcp:send(Socket, Bytes);
write(ssl, Socket, Bytes) ->
    ssl:send(Socket, Bytes).

%%-----------------------------------------------------------------
%% Change the controlling process for the socket
%% 
controlling_process(normal, Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid);
controlling_process(ssl, Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).

%%-----------------------------------------------------------------
%% Get peername
%% 
peername(normal, Socket) ->
    inet:peername(Socket);
peername(ssl, Socket) ->
    ssl:peername(Socket).

%%-----------------------------------------------------------------
%% Get peercert
%% 
peercert(ssl, Socket) ->
    ssl:peercert(Socket);
peercert(Type, _Socket) ->
    orber:dbg("[~p] orber_socket:peercert(~p);~n"
 	      "Only available for SSL sockets.",
 	      [?LINE, Type], ?DEBUG_LEVEL),
    {error, ebadsocket}.

%%-----------------------------------------------------------------
%% Get peerdata
%% 
peerdata(normal, Socket) ->
    create_data(inet:peername(Socket));
peerdata(ssl, Socket) ->
    create_data(ssl:peername(Socket)).

%%-----------------------------------------------------------------
%% Get sockname
%% 
sockname(normal, Socket) ->
    inet:sockname(Socket);
sockname(ssl, Socket) ->
    ssl:sockname(Socket).

%%-----------------------------------------------------------------
%% Get sockdata
%% 
sockdata(normal, Socket) ->
    create_data(inet:sockname(Socket));
sockdata(ssl, Socket) ->
    create_data(ssl:sockname(Socket)).


create_data({ok, {Addr, Port}}) ->
    {orber_env:addr2str(Addr), Port};
create_data(What) ->
    orber:dbg("[~p] orber_socket:peername() or orber_socket:sockname();~n"
	      "Failed with reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
    {"Unable to lookup peer- or sockname", 0}.


%%-----------------------------------------------------------------
%% Shutdown Connection
%% How = read | write | read_write
shutdown(normal, Socket, How) ->
    gen_tcp:shutdown(Socket, How);
shutdown(ssl, Socket, How) ->
    Generation = orber_env:ssl_generation(),
    if
	Generation > 2 ->
	    ssl:shutdown(Socket, How);
	How == read_write ->
	    %% Older versions of SSL do no support shutdown.
	    %% For now we'll use this solution instead.
	    close(ssl, Socket);
	true ->
	    {error, undefined}
    end.

%%-----------------------------------------------------------------
%% Remove Messages from queue
%%
clear(normal, Socket) ->
    tcp_clear(Socket);
clear(ssl, Socket) ->
    ssl_clear(Socket).



%% Inet also checks for the following messages:
%%  * {S, {data, Data}}
%%  * {inet_async, S, Ref, Status}, 
%%  * {inet_reply, S, Status}
%% SSL doesn't.
tcp_clear(Socket) ->
    receive
        {tcp, Socket, _Data} ->
            tcp_clear(Socket);
        {tcp_closed, Socket} ->
            tcp_clear(Socket);
        {tcp_error, Socket, _Reason} ->
            tcp_clear(Socket)
    after 0 -> 
            ok
    end.

ssl_clear(Socket) ->
    receive
        {ssl, Socket, _Data} ->
            ssl_clear(Socket);
        {ssl_closed, Socket} ->
            ssl_clear(Socket);
        {ssl_error, Socket, _Reason} ->
            ssl_clear(Socket)
    after 0 -> 
            ok
    end.



%%-----------------------------------------------------------------
%% Check Port. If the user supplies 0 we pick any vacant port. But then
%% we must change the associated environment variable
check_port(0, normal, Socket) ->
    case inet:port(Socket) of
	{ok, Port} ->
	    orber:configure_override(iiop_port, Port),
	    Port;
	What ->
	    orber:dbg("[~p] orber_socket:check_port(~p);~n"
		      "Unable to extract the port number via inet:port/1~n",
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
check_port(0, ssl, Socket) ->
    case ssl:sockname(Socket) of
	{ok, {_Address, Port}} ->
	    orber:configure_override(iiop_ssl_port, Port),
	    Port;
	What ->
	    orber:dbg("[~p] orber_socket:check_port(~p);~n"
		      "Unable to extract the port number via ssl:sockname/1~n",
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
check_port(Port, _, _) ->
    Port.

%%-----------------------------------------------------------------
%% Check Options. 
check_options(normal, Options, _Generation) ->
    Options;
check_options(ssl, Options, Generation) ->
    if
	Generation > 2 ->
	    [{ssl_imp, new}|Options];
	true ->
	    [{ssl_imp, old}|Options]
    end.

		
%%-----------------------------------------------------------------
%% Check IP Family. 
get_ip_family_opts(Host) ->
    case inet:parse_address(Host) of
	{ok, {_,_,_,_}} -> 
	    [inet];
	{ok, {_,_,_,_,_,_,_,_}} -> 
	    [inet6];
	{error, einval} ->
	    check_family_for_name(Host, orber_env:ip_version())
    end.

check_family_for_name(Host, inet) ->
    case inet:getaddr(Host, inet) of
	{ok, _Address} ->
	    [inet];
	{error, _} ->
	    case inet:getaddr(Host, inet6) of
		{ok, _Address} ->
		    [inet6];
		{error, _} ->
		    [inet]
	    end
    end;
check_family_for_name(Host, inet6) ->
    case inet:getaddr(Host, inet6) of
	{ok, _Address} ->
	    [inet6];
	{error, _} ->
	    case inet:getaddr(Host, inet) of
		{ok, _Address} ->
		    [inet];
		{error, _} ->
		    [inet6]
	    end
    end.

