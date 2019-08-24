%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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
%% -------------------------------------------------------------------------
%%
%% Module for encrypted Erlang protocol - a minimal encrypted
%% distribution protocol based on only a shared secret
%% and the crypto application
%%
-module(inet_crypto_dist).
-define(DIST_NAME, inet_crypto).
-define(DIST_PROTO, crypto).
-define(DRIVER, inet_tcp).
-define(FAMILY, inet).

-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% Generalized dist API, for sibling IPv6 module inet6_crypto_dist
-export([gen_listen/2, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_close/2, gen_select/2]).

-export([nodelay/0]).

%% Debug
%%%-compile(export_all).
-export([dbg/0, test_server/0, test_client/1]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(PACKET_SIZE, 65536).
-define(BUFFER_SIZE, (?PACKET_SIZE bsl 4)).

%% -------------------------------------------------------------------------

-record(params,
        {socket,
         dist_handle,
         hmac_algorithm = sha256,
         aead_cipher = aes_gcm,
         rekey_key,
         iv = 12,
         key = 16,
         tag_len = 16,
         rekey_interval = 262144
        }).

params(Socket) ->
    #params{socket = Socket}.


-record(key_pair,
        {type = ecdh,
         %% The curve choice greatly affects setup time,
         %% we really want an Edwards curve but that would
         %% require a very new openssl version.
         %% Twisted brainpool curves (*t1) are faster than
         %% non-twisted (*r1), 256 is much faster than 384,
         %% and so on...
%%%         params = brainpoolP384t1,
         params = brainpoolP256t1,
         public,
         private}).

-define(KEY_PAIR_LIFE_TIME, 3600000). % 1 hour
-define(KEY_PAIR_LIFE_COUNT, 256). % Number of connection setups


%% -------------------------------------------------------------------------
%% Keep the node's public/private key pair in the process state
%% of a key pair server linked to the acceptor process.
%% Create the key pair the first time it is needed
%% so crypto gets time to start first.
%%

start_key_pair_server() ->
    monitor_dist_proc(
      spawn_link(
        fun () ->
                register(?MODULE, self()),
                key_pair_server()
        end)).

key_pair_server() ->
    key_pair_server(undefined, undefined, undefined).
%%
key_pair_server(KeyPair) ->
    key_pair_server(
      KeyPair,
      erlang:start_timer(?KEY_PAIR_LIFE_TIME, self(), discard),
      ?KEY_PAIR_LIFE_COUNT).
%%    
key_pair_server(_KeyPair, Timer, 0) ->
    cancel_timer(Timer),
    key_pair_server();
key_pair_server(KeyPair, Timer, Count) ->
    receive
        {Pid, Tag, get_key_pair} ->
            case KeyPair of
                undefined ->
                    KeyPair_1 = generate_key_pair(),
                    Pid ! {Tag, KeyPair_1},
                    key_pair_server(KeyPair_1);
                #key_pair{} ->
                    Pid ! {Tag, KeyPair},
                    key_pair_server(KeyPair, Timer, Count - 1)
            end;
        {Pid, Tag, get_new_key_pair} ->
            cancel_timer(Timer),
            KeyPair_1 = generate_key_pair(),
            Pid ! {Tag, KeyPair_1},
            key_pair_server(KeyPair_1);
        {timeout, Timer, discard} when is_reference(Timer) ->
            key_pair_server()
    end.

generate_key_pair() ->
    #key_pair{type = Type, params = Params} = #key_pair{},
    {Public, Private} =
        crypto:generate_key(Type, Params),
    #key_pair{public = Public, private = Private}.

cancel_timer(undefined) ->
    ok;
cancel_timer(Timer) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Timer, _} -> ok
            end;
        _RemainingTime ->
            ok
    end.

get_key_pair() ->
    call_key_pair_server(get_key_pair).

get_new_key_pair() ->
    call_key_pair_server(get_new_key_pair).

call_key_pair_server(Request) ->
    Pid = whereis(?MODULE),
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            error(Reason)
    end.

compute_shared_secret(
  #key_pair{
     type = PublicKeyType,
     params = PublicKeyParams,
     private = PrivKey}, PubKey) ->
    %%
    crypto:compute_key(PublicKeyType, PubKey, PrivKey, PublicKeyParams).

%% -------------------------------------------------------------------------
%% Erlang distribution plugin structure explained to myself
%% -------
%% These are the processes involved in the distribution:
%% * net_kernel
%% * The Acceptor
%% * The Controller | Handshaker | Ticker
%% * The DistCtrl process that may be split into:
%%   + The Output controller
%%   + The Input controller
%%   For the regular inet_tcp_dist distribution module, DistCtrl
%%   is not one or two processes, but one port - a gen_tcp socket
%%
%% When the VM is started with the argument "-proto_dist inet_crypto"
%% net_kernel registers the module inet_crypto_dist acli,oams distribution
%% module.  net_kernel calls listen/1 to create a listen socket
%% and then accept/1 with the listen socket as argument to spawn
%% the Acceptor process, which is linked to net_kernel.  Apparently
%% the listen socket is owned by net_kernel - I wonder if it could
%% be owned by the Acceptor process instead...
%%
%% The Acceptor process calls blocking accept on the listen socket
%% and when an incoming socket is returned it spawns the DistCtrl
%% process a linked to the Acceptor.  The ownership of the accepted
%% socket is transferred to the DistCtrl process.
%% A message is sent to net_kernel to inform it that an incoming
%% connection has appeared and the Acceptor awaits a reply from net_kernel.
%%
%% net_kernel then calls accept_connection/5 to spawn the Controller |
%% Handshaker | Ticker process that is linked to net_kernel.
%% The Controller then awaits a message from the Acceptor process.
%%
%% When net_kernel has spawned the Controller it replies with a message
%% to the Acceptor that then calls DistCtrl to changes its links
%% so DistCtrl ends up linked to the Controller and not to the Acceptor.
%% The Acceptor then sends a message to the Controller.  The Controller
%% then changes role into the Handshaker creates a #hs_data{} record
%% and calls dist_util:handshake_other_started/1.  After this
%% the Acceptor goes back into a blocking accept on the listen socket.
%%
%% For the regular distribution inet_tcp_dist DistCtrl is a gen_tcp socket
%% and when it is a process it also acts as a socket.  The #hs_data{}
%% record used by dist_util presents a set of funs that are used
%% by dist_util to perform the distribution handshake.  These funs
%% make sure to transfer the handshake messages through the DistCtrl
%% "socket".
%%
%% When the handshake is finished a fun for this purpose in #hs_data{}
%% is called, which tells DistCtrl that it does not need to be prepared
%% for any more #hs_data{} handshake calls.  The DistCtrl process in this
%% module then spawns the Input controller process that gets ownership
%% of the connection's gen_tcp socket and changes into {active, N} mode
%% so now it gets all incoming traffic and delivers that to the VM.
%% The original DistCtrl process changes role into the Output controller
%% process and starts asking the VM for outbound messages and transfers
%% them on the connection socket.
%%
%% The Handshaker now changes into the Ticker role, and uses only two
%% functions in the #hs_data{} record; one to get socket statistics
%% and one to send a tick.  None of these may block for any reason
%% in particular not for a congested socket since that would destroy
%% connection supervision.
%%
%%
%% For an connection net_kernel calls setup/5 which spawns the
%% Controller process as linked to net_kernel.  This Controller process
%% connects to the other node's listen socket and when that is succesful
%% spawns the DistCtrl process as linked to the controller and transfers
%% socket ownership to it.
%%
%% Then the Controller creates the #hs_data{} record and calls
%% dist_util:handshake_we_started/1 which changes the process role
%% into Handshaker.
%%
%% When the distribution handshake is finished the procedure is just
%% as for an incoming connection above.
%%
%%
%% To sum it up.
%%
%% There is an Acceptor process that is linked to net_kernel and
%% informs it when new connections arrive.
%%
%% net_kernel spawns Controllers for incoming and for outgoing connections.
%% these Controllers use the DistCtrl processes to do distribution
%% handshake and after that becomes Tickers that supervise the connection.
%%
%% The Controller | Handshaker | Ticker is linked to net_kernel, and to
%% DistCtrl, one or both.  If any of these connection processes would die
%% all others should be killed by the links.  Therefore none of them may
%% terminate with reason 'normal'.
%% -------------------------------------------------------------------------

-compile({inline, [socket_options/0]}).
socket_options() ->
    [binary, {active, false}, {packet, 2}, {nodelay, true},
     {sndbuf, ?BUFFER_SIZE}, {recbuf, ?BUFFER_SIZE},
     {buffer, ?BUFFER_SIZE}].

%% -------------------------------------------------------------------------
%% select/1 is called by net_kernel to ask if this distribution protocol
%% is willing to handle Node
%%

select(Node) ->
    gen_select(Node, ?DRIVER).

gen_select(Node, Driver) ->
    case dist_util:split_node(Node) of
        {node, _, Host} ->
	    case Driver:getaddr(Host) of
		{ok, _} -> true;
		_ -> false
	    end;
        _ ->
            false
    end.

%% -------------------------------------------------------------------------

is_node_name(Node) ->
    dist_util:is_node_name(Node).

%% -------------------------------------------------------------------------
%% Called by net_kernel to create a listen socket for this
%% distribution protocol.  This listen socket is used by
%% the Acceptor process.
%%

listen(Name) ->
    gen_listen(Name, ?DRIVER).

gen_listen(Name, Driver) ->
    case inet_tcp_dist:gen_listen(Driver, Name) of
        {ok, {Socket, Address, Creation}} ->
            inet:setopts(Socket, socket_options()),
            {ok,
             {Socket, Address#net_address{protocol = ?DIST_PROTO}, Creation}};
        Other ->
            Other
    end.

%% -------------------------------------------------------------------------
%% Called by net_kernel to spawn the Acceptor process that awaits
%% new connection in a blocking accept and informs net_kernel
%% when a new connection has appeared, and starts the DistCtrl
%% "socket" process for the connection.
%%

accept(Listen) ->
    gen_accept(Listen, ?DRIVER).

gen_accept(Listen, Driver) ->
    NetKernel = self(),
    %%
    %% Spawn Acceptor process
    %%
    monitor_dist_proc(
      spawn_opt(
        fun () ->
                start_key_pair_server(),
                accept_loop(Listen, Driver, NetKernel)
        end,
        [link, {priority, max}])).

accept_loop(Listen, Driver, NetKernel) ->
    case Driver:accept(trace(Listen)) of
        {ok, Socket} ->
            wait_for_code_server(),
            Timeout = net_kernel:connecttime(),
            DistCtrl = start_dist_ctrl(trace(Socket), Timeout),
            %% DistCtrl is a "socket"
            NetKernel !
                trace({accept,
                       self(), DistCtrl, Driver:family(), ?DIST_PROTO}),
            receive
                {NetKernel, controller, Controller} ->
                    call_dist_ctrl(DistCtrl, {controller, Controller, self()}),
                    Controller ! {self(), controller, Socket};
                {NetKernel, unsupported_protocol} ->
                    exit(unsupported_protocol)
            end,
            accept_loop(Listen, Driver, NetKernel);
        AcceptError ->
            exit({accept, AcceptError})
    end.

wait_for_code_server() ->
    %% This is an ugly hack.  Starting encryption on a connection
    %% requires the crypto module to be loaded.  Loading the crypto
    %% module triggers its on_load function, which calls
    %% code:priv_dir/1 to find the directory where its NIF library is.
    %% However, distribution is started earlier than the code server,
    %% so the code server is not necessarily started yet, and
    %% code:priv_dir/1 might fail because of that, if we receive
    %% an incoming connection on the distribution port early enough.
    %%
    %% If the on_load function of a module fails, the module is
    %% unloaded, and the function call that triggered loading it fails
    %% with 'undef', which is rather confusing.
    %%
    %% So let's avoid that by waiting for the code server to start.
    %%
    case whereis(code_server) of
	undefined ->
	    timer:sleep(10),
	    wait_for_code_server();
	Pid when is_pid(Pid) ->
	    ok
    end.

%% -------------------------------------------------------------------------
%% Called by net_kernel when a new connection has appeared, to spawn
%% a Controller process that performs the handshake with the new node,
%% and then becomes the Ticker connection supervisor.
%% -------------------------------------------------------------------------

accept_connection(Acceptor, DistCtrl, MyNode, Allowed, SetupTime) ->
    gen_accept_connection(
      Acceptor, DistCtrl, MyNode, Allowed, SetupTime, ?DRIVER).

gen_accept_connection(
  Acceptor, DistCtrl, MyNode, Allowed, SetupTime, Driver) ->
    NetKernel = self(),
    %%
    %% Spawn Controller/handshaker/ticker process
    %%
    monitor_dist_proc(
      spawn_opt(
        fun() ->
                do_accept(
                  Acceptor, DistCtrl,
                  trace(MyNode), Allowed, SetupTime, Driver, NetKernel)
        end,
        [link, {priority, max}])).

do_accept(
  Acceptor, DistCtrl, MyNode, Allowed, SetupTime, Driver, NetKernel) ->
    %%
    receive
	{Acceptor, controller, Socket} ->
	    Timer = dist_util:start_timer(SetupTime),
            HSData =
                hs_data_common(
                  NetKernel, MyNode, DistCtrl, Timer,
                  Socket, Driver:family()),
            HSData_1 =
                HSData#hs_data{
                  this_node = MyNode,
                  this_flags = 0,
                  allowed = Allowed},
            dist_util:handshake_other_started(trace(HSData_1))
    end.

%% -------------------------------------------------------------------------
%% Called by net_kernel to spawn a Controller process that sets up
%% a new connection to another Erlang node, performs the handshake
%% with the other it, and then becomes the Ticker process
%% that supervises the connection.
%% -------------------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    gen_setup(Node, Type, MyNode, LongOrShortNames, SetupTime, ?DRIVER).

gen_setup(Node, Type, MyNode, LongOrShortNames, SetupTime, Driver) ->
    NetKernel = self(),
    %%
    %% Spawn Controller/handshaker/ticker process
    %%
    monitor_dist_proc(
      spawn_opt(
        setup_fun(
          Node, Type, MyNode, LongOrShortNames, SetupTime, Driver, NetKernel),
        [link, {priority, max}])).

-spec setup_fun(_,_,_,_,_,_,_) -> fun(() -> no_return()).
setup_fun(
  Node, Type, MyNode, LongOrShortNames, SetupTime, Driver, NetKernel) ->
    %%
    fun() ->
            do_setup(
              trace(Node), Type, MyNode, LongOrShortNames, SetupTime,
              Driver, NetKernel)
    end.

-spec do_setup(_,_,_,_,_,_,_) -> no_return().
do_setup(
  Node, Type, MyNode, LongOrShortNames, SetupTime, Driver, NetKernel) ->
    %%
    {Name, Address} = split_node(Driver, Node, LongOrShortNames),
    ErlEpmd = net_kernel:epmd_module(),
    {ARMod, ARFun} = get_address_resolver(ErlEpmd, Driver),
    Timer = trace(dist_util:start_timer(SetupTime)),
    case ARMod:ARFun(Name, Address, Driver:family()) of
        {ok, Ip, TcpPort, Version} ->
            do_setup_connect(
              Node, Type, MyNode, Timer, Driver, NetKernel,
              Ip, TcpPort, Version);
	{ok, Ip} ->
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
                do_setup_connect(
                  Node, Type, MyNode, Timer, Driver, NetKernel,
                  Ip, TcpPort, trace(Version));
		Other ->
                    _ = trace(
                          {ErlEpmd, port_please, [Name, Ip], Other}),
                    ?shutdown(Node)
	    end;
	Other ->
            _ = trace(
                  {ARMod, ARFun, [Name, Address, Driver:family()],
                   Other}),
            ?shutdown(Node)
    end.

-spec do_setup_connect(_,_,_,_,_,_,_,_,_) -> no_return().

do_setup_connect(
  Node, Type, MyNode, Timer, Driver, NetKernel,
  Ip, TcpPort, Version) ->
    dist_util:reset_timer(Timer),
    ConnectOpts = trace(connect_options(socket_options())),
    case Driver:connect(Ip, TcpPort, ConnectOpts) of
        {ok, Socket} ->
            DistCtrl =
                try start_dist_ctrl(Socket, net_kernel:connecttime())
                catch error : {dist_ctrl, _} = DistCtrlError ->
                        _ = trace(DistCtrlError),
                        ?shutdown(Node)
                end,
            %% DistCtrl is a "socket"
            HSData =
                hs_data_common(
                  NetKernel, MyNode, DistCtrl, Timer,
                  Socket, Driver:family()),
            HSData_1 =
                HSData#hs_data{
                  other_node = Node,
                  this_flags = 0,
                  other_version = Version,
                  request_type = Type},
            dist_util:handshake_we_started(trace(HSData_1));
        ConnectError ->
            _ = trace(
                  {Driver, connect, [Ip, TcpPort, ConnectOpts],
                   ConnectError}),
            ?shutdown(Node)
    end.

%% -------------------------------------------------------------------------
%% close/1 is only called by net_kernel on the socket returned by listen/1.

close(Socket) ->
    gen_close(Socket, ?DRIVER).

gen_close(Socket, Driver) ->
    Driver:close(trace(Socket)).

%% -------------------------------------------------------------------------


hs_data_common(NetKernel, MyNode, DistCtrl, Timer, Socket, Family) ->
    %% Field 'socket' below is set to DistCtrl, which makes
    %% the distribution handshake process (ticker) call
    %% the funs below with DistCtrl as the S argument.
    %% So, S =:= DistCtrl below...
    #hs_data{
       kernel_pid = NetKernel,
       this_node = MyNode,
       socket = DistCtrl,
       timer = Timer,
       %%
       f_send = % -> ok | {error, closed}=>?shutdown()
           fun (S, Packet) when S =:= DistCtrl ->
                   try call_dist_ctrl(S, {send, Packet})
                   catch error : {dist_ctrl, Reason} ->
                           _ = trace(Reason),
                           {error, closed}
                   end
           end,
       f_recv = % -> {ok, List} | Other=>?shutdown()
           fun (S, 0, infinity) when S =:= DistCtrl ->
                   try call_dist_ctrl(S, recv) of
                       {ok, Bin} when is_binary(Bin) ->
                           {ok, binary_to_list(Bin)};
                       Error ->
                           Error
                   catch error : {dist_ctrl, Reason} ->
                           {error, trace(Reason)}
                   end
           end,
       f_setopts_pre_nodeup =
           fun (S) when S =:= DistCtrl ->
                   ok
           end,
       f_setopts_post_nodeup =
           fun (S) when S =:= DistCtrl ->
                   ok
           end,
       f_getll =
           fun (S) when S =:= DistCtrl ->
                   {ok, S} %% DistCtrl is the distribution port
           end,
       f_address = % -> #net_address{} | ?shutdown()
           fun (S, Node) when S =:= DistCtrl ->
                   try call_dist_ctrl(S, peername) of
                       {ok, Address} ->
                           case dist_util:split_node(Node) of
                               {node, _, Host} ->
                                   #net_address{
                                      address = Address,
                                      host = Host,
                                      protocol = ?DIST_PROTO,
                                      family = Family};
                               _ ->
                                   ?shutdown(Node)
                           end;
                       Error ->
                           _ = trace(Error),
                           ?shutdown(Node)
                   catch error : {dist_ctrl, Reason} ->
                           _ = trace(Reason),
                           ?shutdown(Node)
                   end
           end,
       f_handshake_complete = % -> ok | ?shutdown()
           fun (S, Node, DistHandle) when S =:= DistCtrl ->
                   try call_dist_ctrl(S, {handshake_complete, DistHandle})
                   catch error : {dist_ctrl, Reason} ->
                           _ = trace(Reason),
                           ?shutdown(Node)
                   end
           end,
       %%
       %% mf_tick/1, mf_getstat/1, mf_setopts/2 and mf_getopts/2
       %% are called by the ticker any time after f_handshake_complete/3
       %% so they may not block the caller even for congested socket
       mf_tick =
           fun (S) when S =:= DistCtrl ->
                   S ! dist_tick
           end,
       mf_getstat = % -> {ok, RecvCnt, SendCnt, SendPend} | Other=>ignore_it
           fun (S) when S =:= DistCtrl ->
                   case
                       inet:getstat(Socket, [recv_cnt, send_cnt, send_pend])
                   of
                       {ok, Stat} ->
                           split_stat(Stat, 0, 0, 0);
                       Error ->
                           trace(Error)
                   end
           end,
       mf_setopts =
           fun (S, Opts) when S =:= DistCtrl ->
                   inet:setopts(Socket, setopts_filter(Opts))
           end,
       mf_getopts =
           fun (S, Opts) when S =:= DistCtrl ->
                   inet:getopts(Socket, Opts)
           end}.

setopts_filter(Opts) ->
    [Opt ||
        Opt <- Opts,
        case Opt of
            {K, _} when K =:= active; K =:= deliver; K =:= packet -> false;
            K when K =:= list; K =:= binary -> false;
            K when K =:= inet; K =:= inet6 -> false;
            _ -> true
        end].

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

%% ------------------------------------------------------------
%% Determine if EPMD module supports address resolving. Default
%% is to use inet_tcp:getaddr/2.
%% ------------------------------------------------------------
get_address_resolver(EpmdModule, _Driver) ->
    case erlang:function_exported(EpmdModule, address_please, 3) of
        true -> {EpmdModule, address_please};
        _    -> {erl_epmd, address_please}
    end.


%% If Node is illegal terminate the connection setup!!
split_node(Driver, Node, LongOrShortNames) ->
    case dist_util:split_node(Node) of
        {node, Name, Host} ->
	    check_node(Driver, Node, Name, Host, LongOrShortNames);
	{host, _} ->
	    error_logger:error_msg(
              "** Nodename ~p illegal, no '@' character **~n",
              [Node]),
	    ?shutdown2(Node, trace({illegal_node_n@me, Node}));
	_ ->
	    error_logger:error_msg(
              "** Nodename ~p illegal **~n", [Node]),
	    ?shutdown2(Node, trace({illegal_node_name, Node}))
    end.

check_node(Driver, Node, Name, Host, LongOrShortNames) ->
    case string:split(Host, ".", all) of
	[_] when LongOrShortNames =:= longnames ->
	    case Driver:parse_address(Host) of
		{ok, _} ->
		    {Name, Host};
		_ ->
		    error_logger:error_msg(
                      "** System running to use "
                      "fully qualified hostnames **~n"
                      "** Hostname ~s is illegal **~n",
                      [Host]),
		    ?shutdown2(Node, trace({not_longnames, Host}))
	    end;
	[_, _|_] when LongOrShortNames =:= shortnames ->
	    error_logger:error_msg(
              "** System NOT running to use "
              "fully qualified hostnames **~n"
              "** Hostname ~s is illegal **~n",
              [Host]),
	    ?shutdown2(Node, trace({not_shortnames, Host}));
	_ ->
	    {Name, Host}
    end.

%% -------------------------------------------------------------------------

connect_options(Opts) ->
    case application:get_env(kernel, inet_dist_connect_options) of
	{ok, ConnectOpts} ->
            Opts ++ setopts_filter(ConnectOpts);
	_ ->
	    Opts
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The DistCtrl process(es).
%%
%% At net_kernel handshake_complete spawns off the input controller that
%% takes over the socket ownership, and itself becomes the output controller
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% XXX Missing to "productified":
%%% * Cryptoanalysis by experts, this is crypto amateur work.
%%% * Is it useful over inet_tls_dist; i.e to not have to bother
%%%   with certificates but instead manage a secret cluster cookie?
%%% * An application to belong to (kernel)
%%% * Restart and/or code reload policy (not needed in kernel)
%%% * Fitting into the epmd/Erlang distro protocol version framework
%%%   (something needs to be created for multiple protocols, epmd,
%%%    multiple address families, fallback to previous version, etc)


%% Debug client and server

test_server() ->
    {ok, Listen} = gen_tcp:listen(0, socket_options()),
    {ok, Port} = inet:port(Listen),
    io:format(?MODULE_STRING":test_client(~w).~n", [Port]),
    {ok, Socket} = gen_tcp:accept(Listen),
    test(Socket).

test_client(Port) ->
    {ok, Socket} = gen_tcp:connect(localhost, Port, socket_options()),
    test(Socket).

test(Socket) ->
    start_dist_ctrl(Socket, 10000).

%% -------------------------------------------------------------------------

start_dist_ctrl(Socket, Timeout) ->
    Secret = atom_to_binary(auth:get_cookie(), latin1),
    Controller = self(),
    Server =
        monitor_dist_proc(
          spawn_opt(
            fun () ->
                    receive
                        {?MODULE, From, start} ->
                            {SendParams, RecvParams} =
                                init(Socket, Secret),
                            reply(From, self()),
                            handshake(SendParams, 1, RecvParams, 1, Controller)
                    end
            end,
            [link,
             {priority, max},
             {message_queue_data, off_heap},
             {fullsweep_after, 0}])),
    ok = gen_tcp:controlling_process(Socket, Server),
    call_dist_ctrl(Server, start, Timeout).


call_dist_ctrl(Server, Msg) ->
    call_dist_ctrl(Server, Msg, infinity).
%%
call_dist_ctrl(Server, Msg, Timeout) ->
    Ref = erlang:monitor(process, Server),
    Server ! {?MODULE, {Ref, self()}, Msg},
    receive
        {Ref, Res} ->
            erlang:demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, Server, Reason} ->
            error({dist_ctrl, Reason})
    after Timeout -> % Timeout < infinity is only used by start_dist_ctrl/2
            receive
                {'DOWN', Ref, process, Server, _} ->
                    receive {Ref, _} -> ok after 0 -> ok end,
                    error({dist_ctrl, timeout})
                    %% Server will be killed by link
            end
    end.

reply({Ref, Pid}, Msg) ->
    Pid ! {Ref, Msg},
    ok.

%% -------------------------------------------------------------------------

-define(TCP_ACTIVE, 16).
-define(CHUNK_SIZE, (?PACKET_SIZE - 512)).

-define(HANDSHAKE_CHUNK, 1).
-define(DATA_CHUNK, 2).
-define(TICK_CHUNK, 3).
-define(REKEY_CHUNK, 4).

%% -------------------------------------------------------------------------
%% Crypto strategy
%% -------
%% The crypto strategy is as simple as possible to get an encrypted
%% connection as benchmark reference.  It is geared around AEAD
%% ciphers in particular AES-GCM.
%%
%% The init message and the start message must fit in the TCP buffers
%% since both sides start with sending the init message, waits
%% for the other end's init message, sends the start message
%% and waits for the other end's start message.  So if the send
%% blocks we have a deadlock.
%%
%% The init + start sequence tries to implement Password Encrypted
%% Key Exchange using a node public/private key pair and the
%% shared secret (the Cookie) to create session encryption keys
%% that can not be re-created if the shared secret is compromized,
%% which should create forward secrecy.  You need both nodes'
%% key pairs and the shared secret to decrypt the traffic
%% between the nodes.
%%
%% All exchanged messages uses {packet, 2} i.e 16 bit size header.
%%
%% The init message contains a random number and encrypted: the public key
%% and two random numbers.  The encryption is done with Key and IV hashed
%% from the unencrypted random number and the shared secret.
%%
%% The other node's public key is used with the own node's private
%% key to create a shared key that is hashed with one of the encrypted
%% random numbers from each side to create Key and IV for the session.
%%
%% The start message contains the two encrypted random numbers
%% this time encrypted with the session keys for verification
%% by the other side, plus the rekey interval.  The rekey interval
%% is just there to get an early check for if the other side's
%% maximum rekey interal is acceptable, it is just an embryo
%% of some better check.  Any side may rekey earlier but if the
%% rekey interval is exceeded the connection fails.
%%
%% Subsequent encrypted messages has the sequence number and the length
%% of the message as AAD data, and an incrementing IV.  These messages
%% has got a message type that differentiates data from ticks and rekeys.
%% Ticks have a random size in an attempt to make them less obvious to spot.
%%
%% Rekeying is done by the sender that creates a new key pair and
%% a new shared secret from the other end's public key and with
%% this and the current key and iv hashes a new key and iv.
%% The new public key is sent to the other end that uses it
%% and its old private key to create the same new shared
%% secret and from that a new key and iv.
%% So the receiver keeps its private key, and the sender keeps
%% the receivers public key for the connection's life time.
%% While the sender generates a new key pair at every rekey,
%% which changes the shared secret at every rekey.
%%
%% The only reaction to errors is to crash noisily (?) wich will bring
%% down the connection and hopefully produce something useful
%% in the local log, but all the other end sees is a closed connection.
%% -------------------------------------------------------------------------

init(Socket, Secret) ->
    #key_pair{public = PubKey} = KeyPair = get_key_pair(),
    Params = params(Socket),
    {R2, R3, Msg} = init_msg(Params, PubKey, Secret),
    ok = gen_tcp:send(Socket, Msg),
    init_recv(Params, Secret, KeyPair, R2, R3).

init_recv(
  #params{socket = Socket, iv = IVLen} = Params, Secret, KeyPair, R2, R3) ->
    %%
    {ok, InitMsg} = gen_tcp:recv(Socket, 0),
    IVSaltLen = IVLen - 6,
    try
        case init_msg(Params, Secret, KeyPair, R2, R3, InitMsg) of
            {#params{iv = <<IV2ASalt:IVSaltLen/binary, IV2ANo:48>>} =
                 SendParams,
             RecvParams, SendStartMsg} ->
                ok = gen_tcp:send(Socket, SendStartMsg),
                {ok, RecvStartMsg} = gen_tcp:recv(Socket, 0),
                #params{
                   iv = <<IV2BSalt:IVSaltLen/binary, IV2BNo:48>>} =
                    RecvParams_1 =
                    start_msg(RecvParams, R2, R3, RecvStartMsg),
                {SendParams#params{iv = {IV2ASalt, IV2ANo}},
                 RecvParams_1#params{iv = {IV2BSalt, IV2BNo}}}
        end
    catch
        error : Reason : Stacktrace->
            _ = trace({Reason, Stacktrace}),
            exit(connection_closed)
    end.



init_msg(
  #params{
     hmac_algorithm = HmacAlgo,
     aead_cipher = AeadCipher,
     key = KeyLen,
     iv = IVLen,
     tag_len = TagLen}, PubKeyA, Secret) ->
    %%
    RLen = KeyLen + IVLen,
    <<R1A:RLen/binary, R2A:RLen/binary, R3A:RLen/binary>> =
        crypto:strong_rand_bytes(3 * RLen),
    {Key1A, IV1A} = hmac_key_iv(HmacAlgo, R1A, Secret, KeyLen, IVLen),
    Plaintext = [R2A, R3A, PubKeyA],
    MsgLen = byte_size(R1A) + TagLen + iolist_size(Plaintext),
    AAD = [<<MsgLen:32>>, R1A],
    {Ciphertext, Tag} =
        crypto:block_encrypt(AeadCipher, Key1A, IV1A, {AAD, Plaintext, TagLen}),
    Msg = [R1A, Tag, Ciphertext],
    {R2A, R3A, Msg}.
%%
init_msg(
  #params{
     hmac_algorithm = HmacAlgo,
     aead_cipher = AeadCipher,
     key = KeyLen,
     iv = IVLen,
     tag_len = TagLen,
     rekey_interval = RekeyInterval} = Params,
  Secret, KeyPair, R2A, R3A, Msg) ->
    %%
    RLen = KeyLen + IVLen,
    case Msg of
        <<R1B:RLen/binary, Tag:TagLen/binary, Ciphertext/binary>> ->
            {Key1B, IV1B} = hmac_key_iv(HmacAlgo, R1B, Secret, KeyLen, IVLen),
            MsgLen = byte_size(Msg),
            AAD = [<<MsgLen:32>>, R1B],
            case
                crypto:block_decrypt(
                  AeadCipher, Key1B, IV1B, {AAD, Ciphertext, Tag})
            of
                <<R2B:RLen/binary, R3B:RLen/binary, PubKeyB/binary>> ->
                    SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
                    %%
                    {Key2A, IV2A} =
                        hmac_key_iv(
                          HmacAlgo, SharedSecret, [R2A, R3B], KeyLen, IVLen),
                    SendParams =
                        Params#params{
                          rekey_key = PubKeyB,
                          key = Key2A, iv = IV2A},
                    %%
                    StartCleartext = [R2B, R3B, <<RekeyInterval:32>>],
                    StartMsgLen = TagLen + iolist_size(StartCleartext),
                    StartAAD = <<StartMsgLen:32>>,
                    {StartCiphertext, StartTag} =
                        crypto:block_encrypt(
                          AeadCipher, Key2A, IV2A,
                          {StartAAD, StartCleartext, TagLen}),
                    StartMsg = [StartTag, StartCiphertext],
                    %%
                    {Key2B, IV2B} =
                        hmac_key_iv(
                          HmacAlgo, SharedSecret, [R2B, R3A], KeyLen, IVLen),
                    RecvParams =
                        Params#params{
                          rekey_key = KeyPair,
                          key = Key2B, iv = IV2B},
                    %%
                    {SendParams, RecvParams, StartMsg}
            end
    end.

start_msg(
  #params{
     aead_cipher = AeadCipher,
     key = Key2B,
     iv = IV2B,
     tag_len = TagLen,
     rekey_interval = RekeyIntervalA} = RecvParams, R2A, R3A, Msg) ->
    %%
    case Msg of
        <<Tag:TagLen/binary, Ciphertext/binary>> ->
            KeyLen = byte_size(Key2B),
            IVLen = byte_size(IV2B),
            RLen = KeyLen + IVLen,
            MsgLen = byte_size(Msg),
            AAD = <<MsgLen:32>>,
            case
                crypto:block_decrypt(
                  AeadCipher, Key2B, IV2B, {AAD, Ciphertext, Tag})
            of
                <<R2A:RLen/binary, R3A:RLen/binary, RekeyIntervalB:32>>
                  when RekeyIntervalA =< (RekeyIntervalB bsl 2),
                       RekeyIntervalB =< (RekeyIntervalA bsl 2) ->
                    RecvParams#params{rekey_interval = RekeyIntervalB}
            end
    end.

hmac_key_iv(HmacAlgo, MacKey, Data, KeyLen, IVLen) ->
    <<Key:KeyLen/binary, IV:IVLen/binary>> =
        crypto:hmac(HmacAlgo, MacKey, Data, KeyLen + IVLen),
    {Key, IV}.

%% -------------------------------------------------------------------------
%% net_kernel distribution handshake in progress
%%

handshake(
  SendParams, SendSeq,
  #params{socket = Socket} = RecvParams, RecvSeq, Controller) ->
    receive
        {?MODULE, From, {controller, Controller_1, Parent}} ->
            Result = link(Controller_1),
            true = unlink(Parent),
            reply(From, Result),
            handshake(SendParams, SendSeq, RecvParams, RecvSeq, Controller_1);
        {?MODULE, From, {handshake_complete, DistHandle}} ->
            InputHandler =
                monitor_dist_proc(
                  spawn_opt(
                    fun () ->
                            link(Controller),
                            receive
                                DistHandle ->
                                    ok =
                                        inet:setopts(
                                          Socket,
                                          [{active, ?TCP_ACTIVE},
                                           nodelay()]),
                                    input_handler(
                                      RecvParams#params{
                                        dist_handle = DistHandle},
                                      RecvSeq, empty_q(), infinity)
                            end
                    end,
                    [link,
                     {priority, normal},
                     {message_queue_data, off_heap},
                     {fullsweep_after, 0}])),
            _ = monitor(process, InputHandler), % For the benchmark test
            ok = gen_tcp:controlling_process(Socket, InputHandler),
            ok = erlang:dist_ctrl_input_handler(DistHandle, InputHandler),
            InputHandler ! DistHandle,
            crypto:rand_seed_alg(crypto_cache),
            reply(From, ok),
            process_flag(priority, normal),
            erlang:dist_ctrl_get_data_notification(DistHandle),
            output_handler(
              SendParams#params{dist_handle = DistHandle}, SendSeq);
        %%
        {?MODULE, From, {send, Data}} ->
            case
                encrypt_and_send_chunk(
                  SendParams, SendSeq, [?HANDSHAKE_CHUNK, Data])
            of
                {SendParams_1, SendSeq_1, ok} ->
                    reply(From, ok),
                    handshake(
                      SendParams_1, SendSeq_1, RecvParams, RecvSeq,
                      Controller);
                {_, _, Error} ->
                    reply(From, {error, closed}),
                    death_row({send, trace(Error)})
            end;
        {?MODULE, From, recv} ->
            case recv_and_decrypt_chunk(RecvParams, RecvSeq) of
                {RecvParams_1, RecvSeq_1, {ok, _} = Reply} ->
                    reply(From, Reply),
                    handshake(
                      SendParams, SendSeq, RecvParams_1, RecvSeq_1,
                      Controller);
                {_, _, Error} ->
                    reply(From, Error),
                    death_row({recv, trace(Error)})
            end;
        {?MODULE, From, peername} ->
            reply(From, inet:peername(Socket)),
            handshake(SendParams, SendSeq, RecvParams, RecvSeq, Controller);
        %%
        _Alien ->
            handshake(SendParams, SendSeq, RecvParams, RecvSeq, Controller)
    end.

recv_and_decrypt_chunk(#params{socket = Socket} = RecvParams, RecvSeq) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Chunk} ->
            case decrypt_chunk(RecvParams, RecvSeq, Chunk) of
                <<?HANDSHAKE_CHUNK, Cleartext/binary>> ->
                    {RecvParams, RecvSeq + 1, {ok, Cleartext}};
                OtherChunk when is_binary(OtherChunk) ->
                    {RecvParams, RecvSeq + 1, {error, decrypt_error}};
                #params{} = RecvParams_1 ->
                    recv_and_decrypt_chunk(RecvParams_1, 0);
                error ->
                    {RecvParams, RecvSeq, {error, decrypt_error}}
            end;
        Error ->
            {RecvParams, RecvSeq, Error}
    end.

%% -------------------------------------------------------------------------
%% Output handler process
%%
%% The game here is to flush all dist_data and dist_tick messages,
%% prioritize dist_data over dist_tick, and to not use selective receive

output_handler(Params, Seq) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(Params, Seq);
                dist_tick ->
                    output_handler_tick(Params, Seq);
                Other ->
                    %% Ignore
                    _ = trace(Other),
                    output_handler(Params, Seq)
            end
    end.

output_handler_data(Params, Seq) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(Params, Seq);
                dist_tick ->
                    output_handler_data(Params, Seq);
                Other ->
                    %% Ignore
                    _ = trace(Other),
                    output_handler_data(Params, Seq)
            end
    after 0 ->
            DistHandle = Params#params.dist_handle,
            Q = get_data(DistHandle, empty_q()),
            {Params_1, Seq_1} = output_handler_send(Params, Seq, Q),
            erlang:dist_ctrl_get_data_notification(DistHandle),
            output_handler(Params_1, Seq_1)
    end.

output_handler_tick(Params, Seq) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(Params, Seq);
                dist_tick ->
                    output_handler_tick(Params, Seq);
                Other ->
                    %% Ignore
                    _ = trace(Other),
                    output_handler_tick(Params, Seq)
            end
    after 0 ->
            TickSize = 7 + rand:uniform(56),
            TickData = binary:copy(<<0>>, TickSize),
            case
                encrypt_and_send_chunk(Params, Seq, [?TICK_CHUNK, TickData])
            of
                {Params_1, Seq_1, ok} ->
                    output_handler(Params_1, Seq_1);
                {_, _, Error} ->
                    _ = trace(Error),
                    death_row()
            end
    end.

output_handler_send(Params, Seq, {_, Size, _} = Q) ->
    if
        ?CHUNK_SIZE < Size ->
            output_handler_send(Params, Seq, Q, ?CHUNK_SIZE);
        true ->
            case get_data(Params#params.dist_handle, Q) of
                {_, 0, _} ->
                    {Params, Seq};
                {_, Size, _} = Q_1 -> % Got no more
                    output_handler_send(Params, Seq, Q_1, Size);
                Q_1 ->
                    output_handler_send(Params, Seq, Q_1)
            end
    end.

output_handler_send(Params, Seq, Q, Size) ->
    {Cleartext, Q_1} = deq_iovec(Size, Q),
    case
        encrypt_and_send_chunk(Params, Seq, [?DATA_CHUNK, Cleartext])
    of
        {Params_1, Seq_1, ok} ->
            output_handler_send(Params_1, Seq_1, Q_1);
        {_, _, Error} ->
            _ = trace(Error),
            death_row()
    end.

%% -------------------------------------------------------------------------
%% Input handler process
%%
%% Here is T = 0|infinity to steer if we should try to receive
%% more data or not; start with infinity, and when we get some
%% data try with 0 to see if more is waiting

input_handler(#params{socket = Socket} = Params, Seq, Q, T) ->
    receive
        Msg ->
            case Msg of
                {tcp_passive, Socket} ->
                    ok = inet:setopts(Socket, [{active, ?TCP_ACTIVE}]),
                    Q_1 =
                        case T of
                            0 ->
                                deliver_data(Params#params.dist_handle, Q);
                            infinity ->
                                Q
                        end,
                    input_handler(Params, Seq, Q_1, infinity);
                {tcp, Socket, Chunk} ->
                    input_chunk(Params, Seq, Q, T, Chunk);
                {tcp_closed, Socket} ->
                    exit(connection_closed);
                Other ->
                    %% Ignore...
                    _ = trace(Other),
                    input_handler(Params, Seq, Q, T)
            end
    after T ->
            Q_1 = deliver_data(Params#params.dist_handle, Q),
            input_handler(Params, Seq, Q_1, infinity)
    end.

input_chunk(Params, Seq, Q, T, Chunk) ->
    case decrypt_chunk(Params, Seq, Chunk) of
        <<?DATA_CHUNK, Cleartext/binary>> ->
            input_handler(Params, Seq + 1, enq_binary(Cleartext, Q), 0);
        <<?TICK_CHUNK, _/binary>> ->
            input_handler(Params, Seq + 1, Q, T);
        OtherChunk when is_binary(OtherChunk) ->
            _ = trace(invalid_chunk),
            exit(connection_closed);
        #params{} = Params_1 ->
            input_handler(Params_1, 0, Q, T);
        error ->
            _ = trace(decrypt_error),
            exit(connection_closed)
    end.

%% -------------------------------------------------------------------------
%% erlang:dist_ctrl_* helpers

%% Get data for sending from the VM and place it in a queue
%%
get_data(DistHandle, {Front, Size, Rear}) ->
    get_data(DistHandle, Front, Size, Rear).
%%
get_data(DistHandle, Front, Size, Rear) ->
    case erlang:dist_ctrl_get_data(DistHandle) of
        none ->
            {Front, Size, Rear};
        Bin when is_binary(Bin)  ->
            Len = byte_size(Bin),
            get_data(
              DistHandle, Front, Size + 4 + Len,
              [Bin, <<Len:32>>|Rear]);
        [Bin1, Bin2] ->
            Len = byte_size(Bin1) + byte_size(Bin2),
            get_data(
              DistHandle, Front, Size + 4 + Len,
              [Bin2, Bin1, <<Len:32>>|Rear]);
        Iovec ->
            Len = iolist_size(Iovec),
            get_data(
              DistHandle, Front, Size + 4 + Len,
              lists:reverse(Iovec, [<<Len:32>>|Rear]))
    end.

%% De-packet and deliver received data to the VM from a queue
%%
deliver_data(DistHandle, Q) ->
    case Q of
        {[], Size, []} ->
            Size = 0, % Assert
            Q;
        {[], Size, Rear} ->
            [Bin|Front] = lists:reverse(Rear),
            deliver_data(DistHandle, Front, Size, [], Bin);
        {[Bin|Front], Size, Rear} ->
            deliver_data(DistHandle, Front, Size, Rear, Bin)
    end.
%%
deliver_data(DistHandle, Front, Size, Rear, Bin) ->
    case Bin of
        <<DataSizeA:32, DataA:DataSizeA/binary,
          DataSizeB:32, DataB:DataSizeB/binary, Rest/binary>> ->
            erlang:dist_ctrl_put_data(DistHandle, DataA),
            erlang:dist_ctrl_put_data(DistHandle, DataB),
            deliver_data(
              DistHandle,
              Front, Size - (4 + DataSizeA + 4 + DataSizeB), Rear,
              Rest);
        <<DataSize:32, Data:DataSize/binary, Rest/binary>> ->
            erlang:dist_ctrl_put_data(DistHandle, Data),
            deliver_data(DistHandle, Front, Size - (4 + DataSize), Rear, Rest);
        <<DataSize:32, FirstData/binary>> ->
            TotalSize = 4 + DataSize,
            if
                TotalSize =< Size ->
                    BinSize = byte_size(Bin),
                    {MoreData, Q} =
                        deq_iovec(
                          TotalSize - BinSize,
                          Front, Size - BinSize, Rear),
                    erlang:dist_ctrl_put_data(DistHandle, [FirstData|MoreData]),
                    deliver_data(DistHandle, Q);
                true -> % Incomplete data
                    {[Bin|Front], Size, Rear}
            end;
        <<_/binary>> ->
            BinSize = byte_size(Bin),
            if
                4 =< Size -> % Fragmented header - extract a header bin
                    {RestHeader, {Front_1, _Size_1, Rear_1}} =
                        deq_iovec(4 - BinSize, Front, Size - BinSize, Rear),
                    Header = iolist_to_binary([Bin|RestHeader]),
                    deliver_data(DistHandle, Front_1, Size, Rear_1, Header);
                true -> % Incomplete header
                    {[Bin|Front], Size, Rear}
            end
    end.

%% -------------------------------------------------------------------------
%% Encryption and decryption helpers

encrypt_and_send_chunk(
  #params{
     socket = Socket,
     rekey_interval = Seq,
     rekey_key = PubKeyB,
     key = Key,
     iv = {IVSalt, IVNo},
     hmac_algorithm = HmacAlgo} = Params,
  Seq, Cleartext) ->
    %%
    KeyLen = byte_size(Key),
    IVSaltLen = byte_size(IVSalt),
    #key_pair{public = PubKeyA} = KeyPair = get_new_key_pair(),
    case
        gen_tcp:send(
          Socket, encrypt_chunk(Params, Seq, [?REKEY_CHUNK, PubKeyA]))
    of
        ok ->
            SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
            IV = <<(IVNo + Seq):48>>,
            {Key_1, <<IVSalt_1:IVSaltLen/binary, IVNo_1:48>>} =
                hmac_key_iv(
                  HmacAlgo, SharedSecret, [Key, IVSalt, IV],
                  KeyLen, IVSaltLen + 6),
            Params_1 = Params#params{key = Key_1, iv = {IVSalt_1, IVNo_1}},
            Result =
                gen_tcp:send(Socket, encrypt_chunk(Params_1, 0, Cleartext)),
            {Params_1, 1, Result};
        SendError ->
            {Params, Seq + 1, SendError}
    end;
encrypt_and_send_chunk(#params{socket = Socket} = Params, Seq, Cleartext) ->
    Result = gen_tcp:send(Socket, encrypt_chunk(Params, Seq, Cleartext)),
    {Params, Seq + 1, Result}.

encrypt_chunk(
  #params{
     aead_cipher = AeadCipher,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen}, Seq, Cleartext) ->
    %%
    ChunkLen = iolist_size(Cleartext) + TagLen,
    AAD = <<Seq:32, ChunkLen:32>>,
    IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
    {Ciphertext, CipherTag} =
        crypto:block_encrypt(AeadCipher, Key, IVBin, {AAD, Cleartext, TagLen}),
    Chunk = [Ciphertext,CipherTag],
    Chunk.

decrypt_chunk(
  #params{
     aead_cipher = AeadCipher,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen} = Params, Seq, Chunk) ->
    %%
    ChunkLen = byte_size(Chunk),
    if
        ChunkLen < TagLen ->
            error;
        true ->
            AAD = <<Seq:32, ChunkLen:32>>,
            IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
            CiphertextLen = ChunkLen - TagLen,
            case Chunk of
                <<Ciphertext:CiphertextLen/binary,
                  CipherTag:TagLen/binary>> ->
                    block_decrypt(
                      Params, Seq, AeadCipher, Key, IVBin,
                      {AAD, Ciphertext, CipherTag});
                _ ->
                    error
            end
    end.

block_decrypt(
  #params{
     rekey_key = #key_pair{public = PubKeyA} = KeyPair,
     rekey_interval = RekeyInterval} = Params,
  Seq, AeadCipher, Key, IV, Data) ->
    %%
    case crypto:block_decrypt(AeadCipher, Key, IV, Data) of
        <<?REKEY_CHUNK, Rest/binary>> ->
            PubKeyLen = byte_size(PubKeyA),
            case Rest of
                <<PubKeyB:PubKeyLen/binary>> ->
                    SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
                    KeyLen = byte_size(Key),
                    IVLen = byte_size(IV),
                    IVSaltLen = IVLen - 6,
                    {Key_1, <<IVSalt:IVSaltLen/binary, IVNo:48>>} =
                        hmac_key_iv(
                          Params#params.hmac_algorithm,
                          SharedSecret, [Key, IV], KeyLen, IVLen),
                    Params#params{iv = {IVSalt, IVNo}, key = Key_1};
                _ ->
                    error
            end;
        Chunk when is_binary(Chunk) ->
            case Seq of
                RekeyInterval ->
                    %% This was one chunk too many without rekeying
                    error;
                _ ->
                    Chunk
            end;
        error ->
            error
    end.

%% -------------------------------------------------------------------------
%% Queue of binaries i.e an iovec queue

empty_q() ->
    {[], 0, []}.

enq_binary(Bin, {Front, Size, Rear}) ->
    {Front, Size + byte_size(Bin), [Bin|Rear]}.

deq_iovec(GetSize, {Front, Size, Rear}) when GetSize =< Size ->
    deq_iovec(GetSize, Front, Size, Rear, []).
%%
deq_iovec(GetSize, Front, Size, Rear) ->
    deq_iovec(GetSize, Front, Size, Rear, []).
%%
deq_iovec(GetSize, [], Size, Rear, Acc) ->
    deq_iovec(GetSize, lists:reverse(Rear), Size, [], Acc);
deq_iovec(GetSize, [Bin|Front], Size, Rear, Acc) ->
    BinSize = byte_size(Bin),
    if
        BinSize < GetSize ->
            deq_iovec(
              GetSize - BinSize, Front, Size - BinSize, Rear, [Bin|Acc]);
        GetSize < BinSize ->
            {Bin1,Bin2} = erlang:split_binary(Bin, GetSize),
            {lists:reverse(Acc, [Bin1]), {[Bin2|Front], Size - GetSize, Rear}};
        true ->
            {lists:reverse(Acc, [Bin]), {Front, Size - BinSize, Rear}}
    end.

%% -------------------------------------------------------------------------

death_row() -> death_row(connection_closed).
%%
death_row(normal) -> death_row(connection_closed);
death_row(Reason) -> receive after 5000 -> exit(Reason) end.

%% -------------------------------------------------------------------------

%% Trace point
trace(Term) -> Term.

%% Keep an eye on this Pid (debug)
-ifndef(undefined).
monitor_dist_proc(Pid) ->
    Pid.
-else.
monitor_dist_proc(Pid) ->
    spawn(
      fun () ->
              MRef = erlang:monitor(process, Pid),
              receive
                  {'DOWN', MRef, _, _, normal} ->
                      error_logger:error_report(
                        [dist_proc_died,
                         {reason, normal},
                         {pid, Pid}]);
                  {'DOWN', MRef, _, _, Reason} ->
                      error_logger:info_report(
                        [dist_proc_died,
                         {reason, Reason},
                         {pid, Pid}])
              end
      end),
    Pid.
-endif.

dbg() ->
    dbg:stop(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(?MODULE, trace, cx),
    dbg:tpl(erlang, dist_ctrl_get_data_notification, cx),
    dbg:tpl(erlang, dist_ctrl_get_data, cx),
    dbg:tpl(erlang, dist_ctrl_put_data, cx),
    ok.
