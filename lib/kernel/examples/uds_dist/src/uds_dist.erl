%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(uds_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([childspecs/0, listen/1, accept/1, accept_connection/5,
	 setup/4, close/1, select/1, is_node_name/1]).

%% internal exports

-export([accept_loop/2,do_accept/6,do_setup/5, getstat/1,tick/1]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").



-define(to_port(Socket, Data),
	case uds:send(Socket, Data) of
	    {error, closed} ->
		self() ! {uds_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).


-include("dist.hrl").
-include("dist_util.hrl").
-record(tick, {read = 0,
	       write = 0,
	       tick = 0,
	       ticked = 0
	       }).


%% -------------------------------------------------------------
%% This function should return a valid childspec, so that 
%% the primitive ssl_server gets supervised
%% -------------------------------------------------------------
childspecs() ->
    {ok, [{uds_server,{uds_server, start_link, []},
	   permanent, 2000, worker, [uds_server]}]}.


%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    {ok, MyHost} = inet:gethostname(),
    case split_node(atom_to_list(Node), $@, []) of
	[_, MyHost] ->
	    true;
	_ -> 
	    false
    end.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name) ->
    case uds:listen(atom_to_list(Name)) of
	{ok, Socket} ->
	    {ok, {Socket, 
		  #net_address{address = [], 
			       host = inet:gethostname(),
			       protocol = uds, 
			       family = uds}, 
		  uds:get_creation(Socket)}};
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
    case uds:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,uds,uds},
	    controller(Kernel, Socket),
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    uds:controlling_process(Socket, Pid),
	    Pid ! {self(), controller};
	{Kernel, unsupported_protocol} ->
	    exit(unsupported_protocol)
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
	    HSData = #hs_data{
	      kernel_pid = Kernel,
	      this_node = MyNode,
	      socket = Socket,
	      timer = Timer,
	      this_flags = ?DFLAG_PUBLISHED bor
	      ?DFLAG_ATOM_CACHE bor
	      ?DFLAG_EXTENDED_REFERENCES bor
	      ?DFLAG_DIST_MONITOR bor
	      ?DFLAG_FUN_TAGS,
	      allowed = Allowed,
	      f_send = fun(S,D) -> uds:send(S,D) end,
	      f_recv = fun(S,N,T) -> uds:recv(S) 
		       end,
	      f_setopts_pre_nodeup = 
	      fun(S) ->
		      uds:set_mode(S, intermediate)
	      end,
	      f_setopts_post_nodeup = 
	      fun(S) ->
		      uds:set_mode(S, data)
	      end,
	      f_getll = fun(S) ->
				uds:get_port(S)
			end,
	      f_address = fun get_remote_id/2,
	      mf_tick = {?MODULE, tick},
	      mf_getstat = {?MODULE,getstat}
	     },
	    dist_util:handshake_other_started(HSData)
    end.

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------

get_remote_id(Socket, Node) ->
    [_, Host] = split_node(atom_to_list(Node), $@, []),
    #net_address {
		  address = [],
		  host = Host,
		  protocol = uds,
		  family = uds }.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, MyNode, LongOrShortNames,SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(),
				   Node,
				   MyNode,
				   LongOrShortNames,
				   SetupTime]).

do_setup(Kernel, Node, MyNode, LongOrShortNames,SetupTime) ->
    process_flag(priority, max),
    ?trace("~p~n",[{uds_dist,self(),setup,Node}]),
    [Name, Address] = splitnode(Node, LongOrShortNames),
    {ok, MyName} = inet:gethostname(), 
    case Address of
	MyName ->
	    Timer = dist_util:start_timer(SetupTime),
	    case uds:connect(Name) of
		{ok, Socket} ->
		    HSData = #hs_data{
		      kernel_pid = Kernel,
		      other_node = Node,
		      this_node = MyNode,
		      socket = Socket,
		      timer = Timer,
		      this_flags = ?DFLAG_PUBLISHED bor
		      ?DFLAG_ATOM_CACHE bor
		      ?DFLAG_EXTENDED_REFERENCES bor
		      ?DFLAG_DIST_MONITOR bor
		      ?DFLAG_FUN_TAGS,
		      other_version = 1,
		      f_send = fun(S,D) -> 
				       uds:send(S,D) 
			       end,
			      f_recv = fun(S,N,T) -> 
					       uds:recv(S) 
				       end,
		      f_setopts_pre_nodeup = 
		      fun(S) ->
			      uds:set_mode(S, intermediate)
		      end,
		      f_setopts_post_nodeup = 
		      fun(S) ->
			      uds:set_mode(S, data)
		      end,
		      f_getll = fun(S) ->
					uds:get_port(S)
				end,
		      f_address = 
		      fun(_,_) ->
			      #net_address{
			        address = [],
			        host = Address,
			        protocol = uds,
			        family = uds}
		      end,
		      mf_tick = {?MODULE, tick},
		      mf_getstat = {?MODULE,getstat}
		     },
		    dist_util:handshake_we_started(HSData);
		_ ->
		    ?shutdown(Node)
	    end;
	Other ->
	    ?shutdown(Node)
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    uds:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail /= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames == longnames ->
		    error_msg("** System running to use "
			      "fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		L when length(L) > 1, LongOrShortNames == shortnames ->
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

is_node_name(Node) when atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, Host] -> true;
	_ -> false
    end;
is_node_name(Node) ->
    false.

tick(Sock) ->
    uds:tick(Sock).
getstat(Socket) ->
    uds:get_status_counters(Socket).
