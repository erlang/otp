%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(inet_tcp_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% internal exports

-export([accept_loop/2,do_accept/6,do_setup/6,getstat/1,tick/1]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").



-define(to_port(Socket, Data, Opts),
	case inet_tcp:send(Socket, Data, Opts) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).


-include("dist.hrl").
-include("dist_util.hrl").

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_,_Host] -> true;
	_ -> false
    end.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name) ->
    case do_listen([{active, false}, {packet,2}, {reuseaddr, true}]) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Socket),
	    {_,Port} = TcpAddress#net_address.address,
	    {ok, Creation} = erl_epmd:register_node(Name, Port),
	    {ok, {Socket, TcpAddress, Creation}};
	Error ->
	    Error
    end.

do_listen(Options0) ->
    {First,Last} = case application:get_env(kernel,inet_dist_listen_min) of
		       {ok,N} when is_integer(N) ->
			   case application:get_env(kernel,
						    inet_dist_listen_max) of
			       {ok,M} when is_integer(M) ->
				   {N,M};
			       _ ->
				   {N,N}
			   end;
		       _ ->
			   {0,0}
		   end,
    Options = case application:get_env(kernel, inet_dist_use_interface) of
		   {ok, Ip} ->
		       [{ip, Ip} | Options0];
		   _ ->
		       Options0
	       end,
    do_listen(First, Last, [{backlog,128}|Options]).

do_listen(First,Last,_) when First > Last ->
    {error,eaddrinuse};
do_listen(First,Last,Options) ->
    case inet_tcp:listen(First, Options) of
	{error, eaddrinuse} ->
	    do_listen(First+1,Last,Options);
	Other ->
	    Other
    end.

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    spawn_opt(?MODULE, accept_loop, [self(), Listen], [link, {priority, max}]).

accept_loop(Kernel, Listen) ->
    case inet_tcp:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,inet,tcp},
	    controller(Kernel, Socket),
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    inet_tcp:controlling_process(Socket, Pid),
	    flush_controller(Pid, Socket),
	    Pid ! {self(), controller};
	{Kernel, unsupported_protocol} ->
	    exit(unsupported_protocol)
    end.

flush_controller(Pid, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Pid ! {tcp, Socket, Data},
	    flush_controller(Pid, Socket);
	{tcp_closed, Socket} ->
	    Pid ! {tcp_closed, Socket},
	    flush_controller(Pid, Socket)
    after 0 ->
	    ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, do_accept,
	      [self(), AcceptPid, Socket, MyNode, Allowed, SetupTime],
	      [link, {priority, max}]).

do_accept(Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(Socket) of
		true ->
		    HSData = #hs_data{
		      kernel_pid = Kernel,
		      this_node = MyNode,
		      socket = Socket,
		      timer = Timer,
		      this_flags = 0,
		      allowed = Allowed,
		      f_send = fun(S,D) -> inet_tcp:send(S,D) end,
		      f_recv = fun(S,N,T) -> inet_tcp:recv(S,N,T) 
			       end,
		      f_setopts_pre_nodeup = 
		      fun(S) ->
			      inet:setopts(S, 
					   [{active, false},
					    {packet, 4},
					    nodelay()])
		      end,
		      f_setopts_post_nodeup = 
		      fun(S) ->
			      inet:setopts(S, 
					   [{active, true},
					    {deliver, port},
					    {packet, 4},
					    nodelay()])
		      end,
		      f_getll = fun(S) ->
					inet:getll(S)
				end,
		      f_address = fun get_remote_id/2,
		      mf_tick = fun ?MODULE:tick/1,
		      mf_getstat = fun ?MODULE:getstat/1
		     },
		    dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_msg("** Connection attempt from "
			      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.


%% we may not always want the nodelay behaviour
%% for performance reasons

nodelay() ->
    case application:get_env(kernel, dist_nodelay) of
	undefined ->
	    {nodelay, true};
	{ok, true} ->
	    {nodelay, true};
	{ok, false} ->
	    {nodelay, false};
	_ ->
	    {nodelay, true}
    end.
	    

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------
get_remote_id(Socket, Node) ->
    case inet:peername(Socket) of
	{ok,Address} ->
	    case split_node(atom_to_list(Node), $@, []) of
		[_,Host] ->
		    #net_address{address=Address,host=Host,
				 protocol=tcp,family=inet};
		_ ->
		    %% No '@' or more than one '@' in node name.
		    ?shutdown(no_node)
	    end;
	{error, _Reason} ->
	    ?shutdown(no_node)
    end.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    spawn_opt(?MODULE, do_setup, 
	      [self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
	      [link, {priority, max}]).

do_setup(Kernel, Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    ?trace("~p~n",[{inet_tcp_dist,self(),setup,Node}]),
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case erl_epmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
		    dist_util:reset_timer(Timer),
		    case inet_tcp:connect(Ip, TcpPort, 
					  [{active, false}, 
					   {packet,2}]) of
			{ok, Socket} ->
			    HSData = #hs_data{
			      kernel_pid = Kernel,
			      other_node = Node,
			      this_node = MyNode,
			      socket = Socket,
			      timer = Timer,
			      this_flags = 0,
			      other_version = Version,
			      f_send = fun inet_tcp:send/2,
			      f_recv = fun inet_tcp:recv/3,
			      f_setopts_pre_nodeup = 
			      fun(S) ->
				      inet:setopts
					(S, 
					 [{active, false},
					  {packet, 4},
					  nodelay()])
			      end,
			      f_setopts_post_nodeup = 
			      fun(S) ->
				      inet:setopts
					(S, 
					 [{active, true},
					  {deliver, port},
					  {packet, 4},
					  nodelay()])
			      end,
			      f_getll = fun inet:getll/1,
			      f_address = 
			      fun(_,_) ->
				      #net_address{
				   address = {Ip,TcpPort},
				   host = Address,
				   protocol = tcp,
				   family = inet}
			      end,
			      mf_tick = fun ?MODULE:tick/1,
			      mf_getstat = fun ?MODULE:getstat/1,
			      request_type = Type
			     },
			    dist_util:handshake_we_started(HSData);
			_ ->
			    %% Other Node may have closed since 
			    %% port_please !
			    ?trace("other node (~p) "
				   "closed since port_please.~n", 
				   [Node]),
			    ?shutdown(Node)
		    end;
		_ ->
		    ?trace("port_please (~p) "
			   "failed.~n", [Node]),
		    ?shutdown(Node)
	    end;
	_Other ->
	    ?trace("inet_getaddr(~p) "
		   "failed (~p).~n", [Node,_Other]),
	    ?shutdown(Node)
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    inet_tcp:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames =:= longnames ->
		    error_msg("** System running to use "
			      "fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		L when length(L) > 1, LongOrShortNames =:= shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		_ ->
		    [Name, Host]
	    end;
	[_] ->
	    error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

%% ------------------------------------------------------------
%% Fetch local information about a Socket.
%% ------------------------------------------------------------
get_tcp_address(Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = tcp,
		  family = inet
		 }.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Socket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(Socket) of
		{ok, IFs, IP} ->
		    check_ip(IFs, IP);
		_ ->
		    ?shutdown(no_node)
	    end;
	_ ->
	    true
    end.

get_ifs(Socket) ->
    case inet:peername(Socket) of
	{ok, {IP, _}} ->
	    case inet:getif(Socket) of
		{ok, IFs} -> {ok, IFs, IP};
		Error     -> Error
	    end;
	Error ->
	    Error
    end.

check_ip([{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {mask(Netmask, PeerIP), mask(Netmask, OwnIP)} of
	{M, M} -> true;
	_      -> check_ip(IFs, PeerIP)
    end;
check_ip([], PeerIP) ->
    {false, PeerIP}.
    
mask({M1,M2,M3,M4}, {IP1,IP2,IP3,IP4}) ->
    {M1 band IP1,
     M2 band IP2,
     M3 band IP3,
     M4 band IP4}.

is_node_name(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, _Host] -> true;
	_ -> false
    end;
is_node_name(_Node) ->
    false.

tick(Sock) ->
    ?to_port(Sock,[],[force]).

getstat(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
	{ok, Stat} ->
	    split_stat(Stat,0,0,0);
	Error ->
	    Error
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.


