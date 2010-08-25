%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 22 Jun 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(inet_proxy_dist).

-export([childspecs/0, listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

-include_lib("kernel/src/net_address.hrl").
-include_lib("kernel/src/dist.hrl").
-include_lib("kernel/src/dist_util.hrl").

-import(error_logger,[error_msg/2]).

childspecs() ->
    io:format("childspecs called~n",[]),
    {ok, [{proxy_server,{proxy_server, start_link, []},
	   permanent, 2000, worker, [proxy_server]}]}.

select(Node) ->
    io:format("Select called~n",[]),
    inet_ssl_dist:select(Node).

is_node_name(Name) ->
    io:format("is_node_name~n",[]),
    inet_ssl_dist:is_node_name(Name).

listen(Name) ->
    io:format("listen called~n",[]),
    gen_server:call(proxy_server, {listen, Name}, infinity).

accept(Listen) ->
    io:format("accept called~n",[]),
    gen_server:call(proxy_server, {accept, Listen}, infinity).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    io:format("accept_connection called ~n",[]),
    Kernel = self(),
    spawn_link(fun() -> do_accept(Kernel, AcceptPid, Socket, 
				  MyNode, Allowed, SetupTime) end).

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    io:format("setup called~n",[]),
    Kernel = self(),
    spawn(fun() -> do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) end).
		   
do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case erl_epmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
		    dist_util:reset_timer(Timer),
		    case gen_server:call(proxy_server, {connect, Ip, TcpPort}, infinity) of
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
					       io:format("Kernel call send~n",[]),
					       gen_tcp:send(S,D) 
				       end,
			      f_recv = fun(S,N,T) -> 
					       io:format("Kernel call receive~n",[]),
					       gen_tcp:recv(S,N,T) 
				       end,
			      f_setopts_pre_nodeup = 
				  fun(S) ->
					  io:format("Kernel pre nodeup~n",[]),
					  inet:setopts(S, [{active, false}, {packet, 4}])
				  end,
			      f_setopts_post_nodeup = 
				  fun(S) -> 
					  io:format("Kernel post nodeup~n",[]),
					  inet:setopts(S, [{deliver, port},{active, true}])
				  end,
			      f_getll = fun(S) -> inet:getll(S) end,
			      f_address = 
				  fun(_,_) ->
					  #net_address{address = {Ip,TcpPort},
						       host = Address,
						       protocol = proxy,
						       family = inet}
				  end,
			      mf_tick = fun(S) -> gen_tcp:send(S, <<>>) end,
			      mf_getstat = fun(S) ->
						   {ok, Stats} = inet:getstat(S, [recv_cnt, send_cnt, send_pend]),
						   R = proplists:get_value(recv_cnt, Stats, 0),
						   W = proplists:get_value(send_cnt, Stats, 0),
						   P = proplists:get_value(send_pend, Stats, 0),
						   {ok, R,W,P}
					   end,
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

close(Socket) ->
    try
	erlang:error(foo)
    catch _:_ ->
	    io:format("close called ~p ~p~n",[Socket, erlang:get_stacktrace()])
    end,
    gen_tcp:close(Socket),
    ok.

do_accept(Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    process_flag(priority, max),
    io:format("~p: in do_accept~n", [self()]),
    receive
	{AcceptPid, controller} ->
	    io:format("~p: do_accept controller~n", [self()]),
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
		      f_send = fun(S,D) -> 
				       io:format("Kernel call send~n",[]),
				       gen_tcp:send(S,D) end,
		      f_recv = fun(S,N,T) -> 				  
				       io:format("Kernel call receive~n",[]),
				       gen_tcp:recv(S,N,T) end,
		      f_setopts_pre_nodeup = 
			  fun(S) ->
				  io:format("Kernel pre nodeup~n",[]),
				  inet:setopts(S, [{active, false}, {packet, 4}])
			  end,
		      f_setopts_post_nodeup = 
			  fun(S) ->
				  io:format("Kernel post nodeup~n",[]),
				  inet:setopts(S, [{deliver, port},{active, true}])
			  end,
		      f_getll = fun(S) -> inet:getll(S) end,
		      f_address = fun get_remote_id/2,
		      mf_tick = fun(S) -> gen_tcp:send(S, <<>>) end,
		      mf_getstat = fun(S) ->
					   {ok, Stats} = inet:getstat(S, [recv_cnt, send_cnt, send_pend]),
					   R = proplists:get_value(recv_cnt, Stats, 0),
					   W = proplists:get_value(send_cnt, Stats, 0),
					   P = proplists:get_value(send_pend, Stats, 0),
					   {ok, R,W,P}
				   end
		     },
		    dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_logger:error_msg("** Connection attempt from "
					   "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.

get_remote_id(Socket, Node) ->
    gen_server:call(proxy_server, {get_remote_id, {Socket,Node}}, infinity).

check_ip(_) ->
    true.


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

