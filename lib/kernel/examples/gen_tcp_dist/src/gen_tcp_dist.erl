%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2019. All Rights Reserved.
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
-module(gen_tcp_dist).

%%
%% This is an example of how to plug in an arbitrary distribution
%% carrier for Erlang using distribution processes.
%%
%% This example uses gen_tcp for transportation of data, but
%% you can use whatever underlying protocol you want as long
%% as your implementation reliably delivers data chunks to the
%% receiving VM in the order they were sent from the sending
%% VM.
%%
%% This code is a rewrite of the lib/kernel/src/inet_tcp_dist.erl
%% distribution implementation for TCP used by default. The default
%% implementation uses distribution ports instead of distribution
%% processes and is more efficient compared to this implementation.
%% This example more or less gets the distribution processes
%% in between the VM and the ports without any specific gain.
%%

-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% Optional
-export([setopts/2, getopts/2]).

%% internal exports

-export([dist_cntrlr_setup/1, dist_cntrlr_input_setup/3,
         dist_cntrlr_tick_handler/1]).

-export([accept_loop/2,do_accept/6,do_setup/6]).

-import(error_logger,[error_msg/2]).

-include_lib("kernel/include/net_address.hrl").

-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, Host] ->
	    case inet:getaddr(Host, inet) of
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
    case do_listen([binary, {active, false}, {packet,2}, {reuseaddr, true}]) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Socket),
	    {_,Port} = TcpAddress#net_address.address,
	    ErlEpmd = net_kernel:epmd_module(),
	    case ErlEpmd:register_node(Name, Port) of
		{ok, Creation} ->
		    {ok, {Socket, TcpAddress, Creation}};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

do_listen(Options) ->
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
    do_listen(First, Last, listen_options([{backlog,128}|Options])).

do_listen(First,Last,_) when First > Last ->
    {error,eaddrinuse};
do_listen(First,Last,Options) ->
    case gen_tcp:listen(First, Options) of
	{error, eaddrinuse} ->
	    do_listen(First+1,Last,Options);
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
    spawn_opt(?MODULE, accept_loop, [self(), Listen], [link, {priority, max}]).

accept_loop(Kernel, Listen) ->
    ?trace("~p~n",[{?MODULE, accept_loop, self()}]),
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
            DistCtrl = spawn_dist_cntrlr(Socket), 
            ?trace("~p~n",[{?MODULE, accept_loop, accepted, Socket, DistCtrl, self()}]),
	    flush_controller(DistCtrl, Socket),
	    gen_tcp:controlling_process(Socket, DistCtrl),
	    flush_controller(DistCtrl, Socket),
	    Kernel ! {accept,self(),DistCtrl,inet,tcp},
            receive
                {Kernel, controller, Pid} ->
                    call_ctrlr(DistCtrl, {supervisor, Pid}),
                    Pid ! {self(), controller};
                {Kernel, unsupported_protocol} ->
                    exit(unsupported_protocol)
            end,
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
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

accept_connection(AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, do_accept,
	      [self(), AcceptPid, DistCtrl, MyNode, Allowed, SetupTime],
	      [link, {priority, max}]).

do_accept(Kernel, AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    ?trace("~p~n",[{?MODULE, do_accept, self(), MyNode}]),
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(DistCtrl) of
		true ->
                    HSData0 = hs_data_common(DistCtrl),
		    HSData = HSData0#hs_data{kernel_pid = Kernel,
                                             this_node = MyNode,
                                             socket = DistCtrl,
                                             timer = Timer,
                                             this_flags = 0,
                                             allowed = Allowed},
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
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    spawn_opt(?MODULE, do_setup, 
	      [self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
	      [link, {priority, max}]).

do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?trace("~p~n",[{?MODULE, do_setup, self(), Node}]),
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
	{ok, Ip} ->
	    Timer = dist_util:start_timer(SetupTime),
	    ErlEpmd = net_kernel:epmd_module(),
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n", 
			   [Node,Version]),
		    dist_util:reset_timer(Timer),
		    case
			gen_tcp:connect(
			  Ip, TcpPort,
			  connect_options([binary, {active, false}, {packet, 2}]))
		    of
			{ok, Socket} ->
                            DistCtrl = spawn_dist_cntrlr(Socket), 
                            call_ctrlr(DistCtrl, {supervisor, self()}),
                            flush_controller(DistCtrl, Socket),
                            gen_tcp:controlling_process(Socket, DistCtrl),
                            flush_controller(DistCtrl, Socket),
                            HSData0 = hs_data_common(DistCtrl),
			    HSData = HSData0#hs_data{kernel_pid = Kernel,
                                                     other_node = Node,
                                                     this_node = MyNode,
                                                     socket = DistCtrl,
                                                     timer = Timer,
                                                     this_flags = 0,
                                                     other_version = Version,
                                                     request_type = Type},
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
close(Listen) ->
    gen_tcp:close(Listen).


%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames =:= longnames ->
                    case inet:parse_address(Host) of
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
check_ip(DistCtrl) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(DistCtrl) of
		{ok, IFs, IP} ->
		    check_ip(IFs, IP);
		_ ->
		    ?shutdown(no_node)
	    end;
	_ ->
	    true
    end.

get_ifs(DistCtrl) ->
    Socket = call_ctrlr(DistCtrl, socket),
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
    case {inet_tcp:mask(Netmask, PeerIP), inet_tcp:mask(Netmask, OwnIP)} of
	{M, M} -> true;
	_      -> check_ip(IFs, PeerIP)
    end;
check_ip([], PeerIP) ->
    {false, PeerIP}.
    
is_node_name(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, _Host] -> true;
	_ -> false
    end;
is_node_name(_Node) ->
    false.

hs_data_common(DistCtrl) ->
    TickHandler = call_ctrlr(DistCtrl, tick_handler),
    Socket = call_ctrlr(DistCtrl, socket),
    #hs_data{f_send = send_fun(),
             f_recv = recv_fun(),
             f_setopts_pre_nodeup = setopts_pre_nodeup_fun(),
             f_setopts_post_nodeup = setopts_post_nodeup_fun(),
             f_getll = getll_fun(),
             f_handshake_complete = handshake_complete_fun(),
             f_address = address_fun(),
             mf_setopts = setopts_fun(DistCtrl, Socket),
             mf_getopts = getopts_fun(DistCtrl, Socket),
             mf_getstat = getstat_fun(DistCtrl, Socket),
             mf_tick = tick_fun(DistCtrl, TickHandler)}.

%%% ------------------------------------------------------------
%%% Distribution controller processes
%%% ------------------------------------------------------------

%%
%% There will be five parties working together when the
%% connection is up:
%% - The gen_tcp socket. Providing a tcp/ip connection
%%   to the other node.
%% - The output handler. It will dispatch all outgoing
%%   traffic from the VM to the gen_tcp socket. This
%%   process is registered as distribution controller
%%   for this channel with the VM.
%% - The input handler. It will dispatch all incoming
%%   traffic from the gen_tcp socket to the VM. This
%%   process is also the socket owner and receives
%%   incoming traffic using active-N.
%% - The tick handler. Dispatches asynchronous tick
%%   requests to the socket. It executes on max priority
%%   since it is important to get ticks through to the
%%   other end.
%% - The channel supervisor (provided by dist_util). It
%%   monitors traffic. Issue tick requests to the tick
%%   handler when no outgoing traffic is seen and bring
%%   the connection down if no incoming traffic is seen.
%%   This process also executes on max priority.
%%
%%   These parties are linked togheter so should one
%%   of them fail, all of them are terminated and the
%%   connection is taken down.
%%

%% In order to avoid issues with lingering signal binaries
%% we enable off-heap message queue data as well as fullsweep
%% after 0. The fullsweeps will be cheap since we have more
%% or less no live data.
-define(DIST_CNTRL_COMMON_SPAWN_OPTS,
        [{message_queue_data, off_heap},
         {fullsweep_after, 0}]).

tick_fun(DistCtrl, TickHandler) ->
    fun (Ctrl) when Ctrl == DistCtrl ->
            TickHandler ! tick
    end.

getstat_fun(DistCtrl, Socket) ->
    fun (Ctrl) when Ctrl == DistCtrl ->
            case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
                {ok, Stat} ->
                    split_stat(Stat,0,0,0);
                Error ->
                    Error
            end
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

setopts_fun(DistCtrl, Socket) ->
    fun (Ctrl, Opts) when Ctrl == DistCtrl ->
            setopts(Socket, Opts)
    end.

getopts_fun(DistCtrl, Socket) ->
    fun (Ctrl, Opts) when Ctrl == DistCtrl ->
            getopts(Socket, Opts)
    end.

setopts(S, Opts) ->
    case [Opt || {K,_}=Opt <- Opts,
		 K =:= active orelse K =:= deliver orelse K =:= packet] of
	[] -> inet:setopts(S,Opts);
	Opts1 -> {error, {badopts,Opts1}}
    end.

getopts(S, Opts) ->
    inet:getopts(S, Opts).

send_fun() ->
    fun (Ctrlr, Packet) ->
            call_ctrlr(Ctrlr, {send, Packet})
    end.

recv_fun() ->
    fun (Ctrlr, Length, Timeout) ->
            case call_ctrlr(Ctrlr, {recv, Length, Timeout}) of
                {ok, Bin} when is_binary(Bin) ->
                    {ok, binary_to_list(Bin)};
                Other ->
                    Other
            end
    end.

getll_fun() ->
    fun (Ctrlr) ->
            call_ctrlr(Ctrlr, getll)
    end.

address_fun() ->
    fun (Ctrlr, Node) ->
            case call_ctrlr(Ctrlr, {address, Node}) of
                {error, no_node} -> %% No '@' or more than one '@' in node name.
		    ?shutdown(no_node);
                Res ->
                    Res
            end
    end.

setopts_pre_nodeup_fun() ->
    fun (Ctrlr) ->
            call_ctrlr(Ctrlr, pre_nodeup)
    end.

setopts_post_nodeup_fun() ->
    fun (Ctrlr) ->
            call_ctrlr(Ctrlr, post_nodeup)
    end.

handshake_complete_fun() ->
    fun (Ctrlr, Node, DHandle) ->
            call_ctrlr(Ctrlr, {handshake_complete, Node, DHandle})
    end.

call_ctrlr(Ctrlr, Msg) ->
    Ref = erlang:monitor(process, Ctrlr),
    Ctrlr ! {Ref, self(), Msg},
    receive
        {Ref, Res} ->
            erlang:demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, Ctrlr, Reason} ->
            exit({dist_controller_exit, Reason})
    end.

%%
%% The tick handler process writes a tick to the
%% socket when it receives a 'tick' message from
%% the connection supervisor.
%%
%% We are not allowed to block the connection
%% superviser when writing a tick and we also want
%% the tick to go through even during a heavily
%% loaded system. gen_tcp does not have a
%% non-blocking send operation exposed in its API
%% and we don't want to run the distribution
%% controller under high priority. Therefore this
%% sparate process with max prio that dispatches
%% ticks.
%%
dist_cntrlr_tick_handler(Socket) ->
    receive
        tick ->
            %% May block due to busy port...
            sock_send(Socket, "");
        _ ->
            ok
    end,
    dist_cntrlr_tick_handler(Socket).

spawn_dist_cntrlr(Socket) ->
    spawn_opt(?MODULE, dist_cntrlr_setup, [Socket],
              [{priority, max}] ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS).

dist_cntrlr_setup(Socket) ->
    TickHandler = spawn_opt(?MODULE, dist_cntrlr_tick_handler,
                            [Socket], 
                            [link, {priority, max}] 
                            ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS),
    dist_cntrlr_setup_loop(Socket, TickHandler, undefined).

%%
%% During the handshake phase we loop in dist_cntrlr_setup().
%% When the connection is up we spawn an input handler and
%% continue as output handler.
%%
dist_cntrlr_setup_loop(Socket, TickHandler, Sup) ->
    receive
        {tcp_closed, Socket} ->
            exit(connection_closed);

        {Ref, From, {supervisor, Pid}} ->
            Res = link(Pid),
            From ! {Ref, Res},
            dist_cntrlr_setup_loop(Socket, TickHandler, Pid);

        {Ref, From, tick_handler} ->
            From ! {Ref, TickHandler},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, socket} ->
            From ! {Ref, Socket},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, {send, Packet}} ->
            Res = gen_tcp:send(Socket, Packet),
            From ! {Ref, Res},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, {recv, Length, Timeout}} ->
            Res = gen_tcp:recv(Socket, Length, Timeout),
            From ! {Ref, Res},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, getll} ->
            From ! {Ref, {ok, self()}},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, {address, Node}} ->
            Res = case inet:peername(Socket) of
                      {ok, Address} ->
                          case split_node(atom_to_list(Node), $@, []) of
                              [_,Host] ->
                                  #net_address{address=Address,host=Host,
                                               protocol=tcp, family=inet};
                              _ ->
                                  {error, no_node}
                          end
                  end,
            From ! {Ref, Res},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, pre_nodeup} ->
            Res = inet:setopts(Socket, 
                               [{active, false},
                                {packet, 4},
                                nodelay()]),
            From ! {Ref, Res},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, post_nodeup} ->
            Res = inet:setopts(Socket,
                               [{active, false},
                                {packet, 4},
                                nodelay()]),
            From ! {Ref, Res},
            dist_cntrlr_setup_loop(Socket, TickHandler, Sup);

        {Ref, From, {handshake_complete, _Node, DHandle}} ->
            From ! {Ref, ok},
            %% Handshake complete! Begin dispatching traffic...

            %% We use separate process for dispatching input. This
            %% is not necessary, but it enables parallel execution
            %% of independent work loads at the same time as it
            %% simplifies the the implementation...
            InputHandler = spawn_opt(?MODULE, dist_cntrlr_input_setup,
                                     [DHandle, Socket, Sup],
                                     [link] ++ ?DIST_CNTRL_COMMON_SPAWN_OPTS),

	    flush_controller(InputHandler, Socket),
	    gen_tcp:controlling_process(Socket, InputHandler),
	    flush_controller(InputHandler, Socket),

            ok = erlang:dist_ctrl_input_handler(DHandle, InputHandler),

            InputHandler ! DHandle,

            %% From now on we execute on normal priority
            process_flag(priority, normal),
            erlang:dist_ctrl_get_data_notification(DHandle),
            case init:get_argument(gen_tcp_dist_output_loop) of
                error ->
                    dist_cntrlr_output_loop(DHandle, Socket);
                {ok, [[ModStr, FuncStr]]} -> % For testing...
                    apply(list_to_atom(ModStr),
                          list_to_atom(FuncStr),
                          [DHandle, Socket])
            end
    end.

%% We use active 10 for good throughput while still
%% maintaining back-pressure if the input controller
%% isn't able to handle all incoming messages...
-define(ACTIVE_INPUT, 10).

dist_cntrlr_input_setup(DHandle, Socket, Sup) ->
    link(Sup),
    %% Ensure we don't try to put data before registerd
    %% as input handler...
    receive
        DHandle ->
            dist_cntrlr_input_loop(DHandle, Socket, 0)
    end.

dist_cntrlr_input_loop(DHandle, Socket, N) when N =< ?ACTIVE_INPUT/2 ->
    inet:setopts(Socket, [{active, ?ACTIVE_INPUT - N}]),
    dist_cntrlr_input_loop(DHandle, Socket, ?ACTIVE_INPUT);
dist_cntrlr_input_loop(DHandle, Socket, N) ->
    receive
        {tcp_closed, Socket} ->
            %% Connection to remote node terminated...
            exit(connection_closed);

        {tcp, Socket, Data} ->
            %% Incoming data from remote node...
            try erlang:dist_ctrl_put_data(DHandle, Data)
            catch _ : _ -> death_row()
            end,
            dist_cntrlr_input_loop(DHandle, Socket, N-1);

        _ ->
            %% Ignore...
            dist_cntrlr_input_loop(DHandle, Socket, N)
    end.

dist_cntrlr_send_data(DHandle, Socket) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            erlang:dist_ctrl_get_data_notification(DHandle);
        Data ->
            sock_send(Socket, Data),
            dist_cntrlr_send_data(DHandle, Socket)
    end.


dist_cntrlr_output_loop(DHandle, Socket) ->
    receive
        dist_data ->
            %% Outgoing data from this node...
            try dist_cntrlr_send_data(DHandle, Socket)
            catch _ : _ -> death_row()
            end,
            dist_cntrlr_output_loop(DHandle, Socket);

        {send, From, Ref, Data} ->
            %% This is for testing only!
            %%
            %% Needed by some OTP distribution
            %% test suites...
            sock_send(Socket, Data),
            From ! {Ref, ok},
            dist_cntrlr_output_loop(DHandle, Socket);

        _ ->
            %% Drop garbage message...
            dist_cntrlr_output_loop(DHandle, Socket)

    end.

sock_send(Socket, Data) ->
    try gen_tcp:send(Socket, Data) of
        ok -> ok;
        {error, Reason} -> death_row({send_error, Reason})
    catch
        Type : Reason -> death_row({send_error, {Type, Reason}})
    end.

death_row() ->
    death_row(connection_closed).

death_row(normal) ->
    %% We do not want to exit with normal
    %% exit reason since it wont bring down
    %% linked processes...
    death_row();
death_row(Reason) ->
    %% When the connection is on its way down operations
    %% begin to fail. We catch the failures and call
    %% this function waiting for termination. We should
    %% be terminated by one of our links to the other
    %% involved parties that began bringing the
    %% connection down. By waiting for termination we
    %% avoid altering the exit reason for the connection
    %% teardown. We however limit the wait to 5 seconds
    %% and bring down the connection ourselves if not
    %% terminated...
    receive after 5000 -> exit(Reason) end.
