%%<copyright>
%% <year>2000-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
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
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
-module(inet_ssl_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([childspecs/0, listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% internal exports

-export([accept_loop/2,do_accept/6,do_setup/6, getstat/1,tick/1]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").



-define(to_port(Socket, Data, Opts),
	case ssl_prim:send(Socket, Data, Opts) of
	    {error, closed} ->
		self() ! {ssl_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).


-include("dist.hrl").
-include("dist_util.hrl").

%% -------------------------------------------------------------
%% This function should return a valid childspec, so that 
%% the primitive ssl_server gets supervised
%% -------------------------------------------------------------
childspecs() ->
    {ok, [{ssl_server_prim,{ssl_server, start_link_prim, []},
	  permanent, 2000, worker, [ssl_server]}]}.


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
    case ssl_prim:listen(0, [{active, false}, {packet,4}] ++ 
			 get_ssl_options(server)) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Socket),
	    {_,Port} = TcpAddress#net_address.address,
	    {ok, Creation} = erl_epmd:register_node(Name, Port),
	    {ok, {Socket, TcpAddress, Creation}};
	Error ->
	    Error
    end.

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    spawn_link(?MODULE, accept_loop, [self(), Listen]).

accept_loop(Kernel, Listen) ->
    process_flag(priority, max),
    case ssl_prim:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,inet,ssl},
	    controller(Kernel, Socket),
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    ssl_prim:controlling_process(Socket, Pid),
	    flush_controller(Pid, Socket),
	    Pid ! {self(), controller};
	{Kernel, unsupported_protocol} ->
	    exit(unsupported_protocol)
    end.

flush_controller(Pid, Socket) ->
    receive
	{ssl, Socket, Data} ->
	    Pid ! {ssl, Socket, Data},
	    flush_controller(Pid, Socket);
	{ssl_closed, Socket} ->
	    Pid ! {ssl_closed, Socket},
	    flush_controller(Pid, Socket)
    after 0 ->
	    ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    spawn_link(?MODULE, do_accept,
	       [self(), AcceptPid, Socket, MyNode,
		Allowed, SetupTime]).

do_accept(Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    process_flag(priority, max),
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
		      f_send = fun(S,D) -> ssl_prim:send(S,D) end,
		      f_recv = fun(S,N,T) -> ssl_prim:recv(S,N,T) 
			       end,
		      f_setopts_pre_nodeup = 
		      fun(S) ->
			      ssl_prim:setopts(S, 
					       [{active, false}])
		      end,
		      f_setopts_post_nodeup = 
		      fun(S) ->
			      ssl_prim:setopts(S, 
					       [{deliver, port},
						{active, true}])
		      end,
		      f_getll = fun(S) ->
					ssl_prim:getll(S)
				end,
		      f_address = fun get_remote_id/2,
		      mf_tick = {?MODULE, tick},
		      mf_getstat = {?MODULE,getstat}
		     },
		    dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_msg("** Connection attempt from "
			      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------

get_remote_id(Socket, Node) ->
    {ok, Address} = ssl_prim:peername(Socket),
    [_, Host] = split_node(atom_to_list(Node), $@, []),
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = ssl,
		  family = inet }.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(),
				   Node,
				   Type,
				   MyNode,
				   LongOrShortNames,
				   SetupTime]).

do_setup(Kernel, Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    process_flag(priority, max),
    ?trace("~p~n",[{inet_ssl_dist,self(),setup,Node}]),
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case erl_epmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
		    dist_util:reset_timer(Timer),
		    case ssl_prim:connect(Ip, TcpPort, 
					  [{active, false}, 
					   {packet,4}] ++ 
					  get_ssl_options(client)) of
			{ok, Socket} ->
			    HSData = #hs_data{
			      kernel_pid = Kernel,
			      other_node = Node,
			      this_node = MyNode,
			      socket = Socket,
			      timer = Timer,
			      this_flags = 0,
			      other_version = Version,
			      f_send = fun(S,D) -> 
					       ssl_prim:send(S,D) 
				       end,
			      f_recv = fun(S,N,T) -> 
					       ssl_prim:recv(S,N,T) 
				       end,
			      f_setopts_pre_nodeup = 
			      fun(S) ->
				      ssl_prim:setopts
					(S, 
					 [{active, false}])
			      end,
			      f_setopts_post_nodeup = 
			      fun(S) ->
				      ssl_prim:setopts
					(S, 
					 [{deliver, port},{active, true}])
			      end,
			      f_getll = fun(S) ->
						ssl_prim:getll(S)
					end,
			      f_address = 
			      fun(_,_) ->
				      #net_address {
				   address = {Ip,TcpPort},
				   host = Address,
				   protocol = ssl,
				   family = inet}
			      end,
			      mf_tick = {?MODULE, tick},
			      mf_getstat = {?MODULE,getstat},
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
		   "failed (~p).~n", [Node,Other]),
	    ?shutdown(Node)
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    ssl_prim:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames == longnames ->
		    error_msg("** System running to use "
			      "fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		[_, _ | _] when LongOrShortNames == shortnames ->
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
    {ok, Address} = ssl_prim:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = ssl,
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
    case ssl_prim:peername(Socket) of
	{ok, {IP, _}} ->
	    case ssl_prim:getif(Socket) of
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
    case ssl_prim:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
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


get_ssl_options(Type) ->
    case init:get_argument(ssl_dist_opt) of
	{ok, Args} ->
	    ssl_options(Type, Args);
	_ ->
	    []
    end.

ssl_options(_,[]) ->
    [];
ssl_options(server, [["server_certfile", Value]|T]) ->
    [{certfile, Value} | ssl_options(server,T)];
ssl_options(client, [["client_certfile", Value]|T]) ->
    [{certfile, Value} | ssl_options(client,T)];
ssl_options(server, [["server_cacertfile", Value]|T]) ->
    [{cacertfile, Value} | ssl_options(server,T)];
ssl_options(server, [["server_keyfile", Value]|T]) ->
    [{keyfile, Value} | ssl_options(server,T)];
ssl_options(Type, [["client_certfile", _Value]|T]) ->
    ssl_options(Type,T);
ssl_options(Type, [["server_certfile", _Value]|T]) ->
    ssl_options(Type,T);
ssl_options(Type, [[Item, Value]|T]) ->
    [{atomize(Item),fixup(Value)} | ssl_options(Type,T)];
ssl_options(Type, [[Item,Value |T1]|T2]) ->
    ssl_options(atomize(Type),[[Item,Value],T1|T2]);
ssl_options(_,_) ->
    exit(malformed_ssl_dist_opt).
    
fixup(Value) ->
    case catch list_to_integer(Value) of
	{'EXIT',_} ->
	    Value;
	Int ->
	    Int
    end.

atomize(List) when is_list(List) ->
    list_to_atom(List);
atomize(Atom) when is_atom(Atom) ->
    Atom.
