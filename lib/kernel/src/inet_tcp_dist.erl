%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(inet_tcp_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% Optional
-export([setopts/2, getopts/2]).

%% Generalized dist API
-export([gen_listen/2, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_select/2]).

%% internal exports

-export([accept_loop/3,do_accept/7,do_setup/7,getstat/1,tick/2]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").




-include("dist.hrl").
-include("dist_util.hrl").

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    gen_select(inet_tcp, Node).

gen_select(Driver, Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, Host] ->
	    case inet:getaddr(Host, Driver:family()) of
                {ok,_} -> true;
                _ -> false
            end;
	_ -> false
    end.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name) ->
    gen_listen(inet_tcp, Name).

gen_listen(Driver, Name) ->
    case do_listen(Driver, [{active, false}, {packet,2}, {reuseaddr, true}]) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Driver, Socket),
	    {_,Port} = TcpAddress#net_address.address,
	    ErlEpmd = net_kernel:epmd_module(),
	    case ErlEpmd:register_node(Name, Port, Driver) of
		{ok, Creation} ->
		    {ok, {Socket, TcpAddress, Creation}};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

do_listen(Driver, Options) ->
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
    do_listen(Driver, First, Last, listen_options([{backlog,128}|Options])).

do_listen(_Driver, First,Last,_) when First > Last ->
    {error,eaddrinuse};
do_listen(Driver, First,Last,Options) ->
    case Driver:listen(First, Options) of
	{error, eaddrinuse} ->
	    do_listen(Driver, First+1,Last,Options);
	Other ->
	    Other
    end.

listen_options(Opts0) ->
    Opts1 =
	case application:get_env(kernel, inet_dist_use_interface) of
	    {ok, Ip} ->
		[{ip, Ip} | Opts0];
	    _ ->
		Opts0
	end,
    case application:get_env(kernel, inet_dist_listen_options) of
	{ok,ListenOpts} ->
	    ListenOpts ++ Opts1;
	_ ->
	    Opts1
    end.


%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    gen_accept(inet_tcp, Listen).

gen_accept(Driver, Listen) ->
    spawn_opt(?MODULE, accept_loop, [Driver, self(), Listen], [link, {priority, max}]).

accept_loop(Driver, Kernel, Listen) ->
    case Driver:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,Driver:family(),tcp},
	    _ = controller(Driver, Kernel, Socket),
	    accept_loop(Driver, Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Driver, Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    Driver:controlling_process(Socket, Pid),
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
    gen_accept_connection(inet_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).

gen_accept_connection(Driver, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, do_accept,
	      [Driver, self(), AcceptPid, Socket, MyNode, Allowed, SetupTime],
	      [link, {priority, max}]).

do_accept(Driver, Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(Driver, Socket) of
		true ->
		    HSData = #hs_data{
		      kernel_pid = Kernel,
		      this_node = MyNode,
		      socket = Socket,
		      timer = Timer,
		      this_flags = 0,
		      allowed = Allowed,
		      f_send = fun Driver:send/2,
		      f_recv = fun Driver:recv/3,
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
		      f_address = fun(S, Node) -> get_remote_id(Driver, S, Node) end,
		      mf_tick = fun(S) -> ?MODULE:tick(Driver, S) end,
		      mf_getstat = fun ?MODULE:getstat/1,
		      mf_setopts = fun ?MODULE:setopts/2,
		      mf_getopts = fun ?MODULE:getopts/2
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
get_remote_id(Driver, Socket, Node) ->
    case inet:peername(Socket) of
	{ok,Address} ->
	    case split_node(atom_to_list(Node), $@, []) of
		[_,Host] ->
		    #net_address{address=Address,host=Host,
				 protocol=tcp,family=Driver:family()};
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
    gen_setup(inet_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    spawn_opt(?MODULE, do_setup, 
	      [Driver, self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
	      [link, {priority, max}]).

do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?trace("~p~n",[{inet_tcp_dist,self(),setup,Node}]),
    [Name, Address] = splitnode(Driver, Node, LongOrShortNames),
    AddressFamily = Driver:family(),
    case inet:getaddr(Address, AddressFamily) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    ErlEpmd = net_kernel:epmd_module(),
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
		    dist_util:reset_timer(Timer),
		    case
			Driver:connect(
			  Ip, TcpPort,
			  connect_options([{active, false}, {packet, 2}]))
		    of
			{ok, Socket} ->
			    HSData = #hs_data{
			      kernel_pid = Kernel,
			      other_node = Node,
			      this_node = MyNode,
			      socket = Socket,
			      timer = Timer,
			      this_flags = 0,
			      other_version = Version,
			      f_send = fun Driver:send/2,
			      f_recv = fun Driver:recv/3,
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
				   family = AddressFamily}
			      end,
			      mf_tick = fun(S) -> ?MODULE:tick(Driver, S) end,
			      mf_getstat = fun ?MODULE:getstat/1,
			      request_type = Type,
			      mf_setopts = fun ?MODULE:setopts/2,
			      mf_getopts = fun ?MODULE:getopts/2
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

connect_options(Opts) ->
    case application:get_env(kernel, inet_dist_connect_options) of
	{ok,ConnectOpts} ->
	    ConnectOpts ++ Opts;
	_ ->
	    Opts
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    inet_tcp:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(Driver, Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames =:= longnames ->
                    case Driver:parse_address(Host) of
                        {ok, _} ->
                            [Name, Host];
                        _ ->
                            error_msg("** System running to use "
                                      "fully qualified "
                                      "hostnames **~n"
                                      "** Hostname ~ts is illegal **~n",
                                      [Host]),
                            ?shutdown(Node)
                    end;
		L when length(L) > 1, LongOrShortNames =:= shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~ts is illegal **~n",
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
get_tcp_address(Driver, Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = tcp,
		  family = Driver:family()
		 }.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Driver, Socket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(Socket) of
		{ok, IFs, IP} ->
		    check_ip(Driver, IFs, IP);
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

check_ip(Driver, [{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {Driver:mask(Netmask, PeerIP), Driver:mask(Netmask, OwnIP)} of
	{M, M} -> true;
	_      -> check_ip(Driver, IFs, PeerIP)
    end;
check_ip(_Driver, [], PeerIP) ->
    {false, PeerIP}.
    
is_node_name(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, _Host] -> true;
	_ -> false
    end;
is_node_name(_Node) ->
    false.

tick(Driver, Socket) ->
    case Driver:send(Socket, [], [force]) of
	{error, closed} ->
	    self() ! {tcp_closed, Socket},
	    {error, closed};
	R ->
	    R
    end.

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


setopts(S, Opts) ->
    case [Opt || {K,_}=Opt <- Opts,
		 K =:= active orelse K =:= deliver orelse K =:= packet] of
	[] -> inet:setopts(S,Opts);
	Opts1 -> {error, {badopts,Opts1}}
    end.

getopts(S, Opts) ->
    inet:getopts(S, Opts).
