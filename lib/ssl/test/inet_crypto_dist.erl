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

-define(PROTOCOL, inet_crypto_dist_v1).
-define(HASH_ALGORITHM, sha256).
-define(BLOCK_CRYPTO, aes_gcm).
-define(IV_LEN, 12).
-define(KEY_LEN, 16).
-define(TAG_LEN, 16).
-define(REKEY_INTERVAL, 32768).

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
%% net_kernel registers the module inet_crypto_dist as distribution
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
            inet:setopts(Socket, [binary, {nodelay, true}]),
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
    ConnectOpts =
        trace(
          connect_options(
            [binary, {active, false}, {packet, 2}, {nodelay, true}])),
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
%%% * Cryptoanalysis by experts, e.g Forward Secrecy is missing, right?
%%% * Is it useful?
%%% * An application to belong to (kernel)
%%% * Restart and/or code reload policy (not needed in kernel)


%% Debug client and server

test_server() ->
    {ok, Listen} = gen_tcp:listen(0, [{packet, 2}, {active, false}, binary]),
    {ok, Port} = inet:port(Listen),
    io:format(?MODULE_STRING":test_client(~w).~n", [Port]),
    {ok, Socket} = gen_tcp:accept(Listen),
    test(Socket).

test_client(Port) ->
    {ok, Socket} =
        gen_tcp:connect(
          localhost, Port, [{packet, 2}, {active, false}, binary]),
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

-record(params,
        {protocol, % Encryption protocol tag
         socket,
         dist_handle,
         hash_algorithm,
         block_crypto,
         rekey_interval,
         iv,
         key,
         tag_len}).

-define(TCP_ACTIVE, 16).
-define(CHUNK_SIZE, (65536 - 512)).

%% The start chunk starts with zeros, so it seems logical to
%% not have a chunk type with value 0
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
%% The init message is unencrypted and contains the block cipher and hash
%% algorithms the sender will use, the IV and a key salt.  Both sides'
%% key salt is used with the mutual secret as input to the hash algorithm
%% to create different encryption/decryption keys for both directions.
%%
%% The start message is the first encrypted message and contains just
%% encrypted zeros the width of the key, with the header of the init
%% message as AAD data.  Successfully decrypting this message
%% verifies that we have an encrypted channel.
%%
%% Subsequent encrypted messages has the sequence number and the length
%% of the message as AAD data.  These messages has got a message type
%% differentiating data from ticks.  Ticks have a random size in an
%% attempt to make them less obvious to spot.
%%
%% The only reaction to errors is to crash noisily wich will bring
%% down the connection and hopefully produce something useful
%% in the local log, but all the other end sees is a closed connection.
%% -------------------------------------------------------------------------

init(Socket, Secret) ->
    {SendParams, Header, Data} = init_params(Socket),
    ok = gen_tcp:send(Socket, [Header, Data]),
    init(
      SendParams, Secret,
      [<<(byte_size(Header) + byte_size(Data)):32>>, Header]).

init(
  #params{
     socket = Socket,
     hash_algorithm = SendHashAlgorithm,
     key = {SendKeySalt, SendOtherSalt}} = SendParams, Secret, SendAAD) ->
    %%
    {ok, InitMsg} = gen_tcp:recv(Socket, 0),
    try
        init_params(Socket, InitMsg)
    of
        {#params{
            hash_algorithm = RecvHashAlgorithm,
            key = {RecvKeySalt, RecvOtherSalt}} = RecvParams,
         RecvHeader} ->
            RecvAAD = [<<(byte_size(InitMsg)):32>>, RecvHeader],
            SendKey =
                hash_key(
                  SendHashAlgorithm, SendKeySalt, [RecvOtherSalt, Secret]),
            RecvKey =
                hash_key(
                  RecvHashAlgorithm, RecvKeySalt, [SendOtherSalt, Secret]),
            SendParams_1 = SendParams#params{key = SendKey},
            RecvParams_1 = RecvParams#params{key = RecvKey},
            send_start(SendParams_1, SendAAD),
            recv_start(RecvParams_1, RecvAAD),
            {SendParams_1, RecvParams_1}
    catch
        error : Reason ->
            _ = trace(Reason),
            exit(connection_closed)
    end.

send_start(
  #params{
     socket = Socket,
     block_crypto = BlockCrypto,
     iv = {IVSalt, IVNo},
     key = Key,
     tag_len = TagLen}, AAD) ->
    %%
    KeyLen = byte_size(Key),
    Zeros = binary:copy(<<0>>, KeyLen),
    IVBin = <<IVSalt/binary, IVNo:48>>,
    {Ciphertext, CipherTag} =
        crypto:block_encrypt(BlockCrypto, Key, IVBin, {AAD, Zeros, TagLen}),
    ok = gen_tcp:send(Socket,  [Ciphertext, CipherTag]).

recv_start(
  #params{
     socket = Socket,
     block_crypto = BlockCrypto,
     iv = {IVSalt, IVNo},
     key = Key,
     tag_len = TagLen}, AAD) ->
    %%
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    KeyLen = byte_size(Key),
    PacketLen = KeyLen,
    <<Ciphertext:PacketLen/binary, CipherTag:TagLen/binary>> = Packet,
    IVBin = <<IVSalt/binary, IVNo:48>>,
    Zeros = binary:copy(<<0>>, KeyLen),
    case
        crypto:block_decrypt(
          BlockCrypto, Key, IVBin, {AAD, Ciphertext, CipherTag})
    of
        Zeros ->
            ok;
        _ ->
            _ = trace(decrypt_error),
            exit(connection_closed)
    end.



init_params(Socket) ->
    Protocol = ?PROTOCOL,
    HashAlgorithm = ?HASH_ALGORITHM,
    BlockCrypto = ?BLOCK_CRYPTO,
    IVSaltLen = ?IV_LEN - 6,
    KeyLen = ?KEY_LEN,
    TagLen = ?TAG_LEN,
    RekeyInterval = ?REKEY_INTERVAL,
    %%
    ProtocolBin = atom_to_binary(Protocol, utf8),
    HashAlgorithmBin = atom_to_binary(HashAlgorithm, utf8),
    BlockCryptoBin = atom_to_binary(BlockCrypto, utf8),
    Header =
	<<(byte_size(ProtocolBin)), ProtocolBin/binary,
	  (byte_size(HashAlgorithmBin)), HashAlgorithmBin/binary,
	  (byte_size(BlockCryptoBin)), BlockCryptoBin/binary,
	  KeyLen, TagLen>>,
    <<KeySalt:KeyLen/binary, OtherSalt:KeyLen/binary,
      IVSalt:IVSaltLen/binary, IVNo:48>> =
        Data = crypto:strong_rand_bytes(KeyLen + KeyLen + IVSaltLen + 6),
    {#params{
        socket = Socket,
        protocol = Protocol,
        hash_algorithm = HashAlgorithm,
        block_crypto = BlockCrypto,
        iv = {IVSalt, IVNo},
        key = {KeySalt, OtherSalt},
        tag_len = TagLen,
        rekey_interval = RekeyInterval}, Header, Data}.

init_params(Socket, Msg) ->
    Protocol = ?PROTOCOL,
    ProtocolBin = atom_to_binary(Protocol, utf8),
    ProtocolBinLen = byte_size(ProtocolBin),
    case Msg of
	<<ProtocolBinLen, ProtocolBin:ProtocolBinLen/binary,
	  Rest_1/binary>> ->
	    HashAlgorithm = ?HASH_ALGORITHM,
	    BlockCrypto = ?BLOCK_CRYPTO,
	    HashAlgorithmBin = atom_to_binary(HashAlgorithm, utf8),
	    HashAlgorithmBinLen = byte_size(HashAlgorithmBin),
	    BlockCryptoBin = atom_to_binary(BlockCrypto, utf8),
	    BlockCryptoBinLen = byte_size(BlockCryptoBin),
	    case Rest_1 of
		<<HashAlgorithmBinLen,
		  HashAlgorithmBin:HashAlgorithmBinLen/binary,
		  BlockCryptoBinLen,
		  BlockCryptoBin:BlockCryptoBinLen/binary,
		  Rest_2/binary>> ->
                    IVSaltLen = ?IV_LEN - 6, KeyLen = ?KEY_LEN,
		    TagLen = ?TAG_LEN, RekeyInterval = ?REKEY_INTERVAL,
                    <<KeyLen, TagLen,
                      KeySalt:KeyLen/binary,
                      OtherSalt:KeyLen/binary,
                      IVSalt:IVSaltLen/binary, IVNo:48>> = Rest_2,
                    HeaderLen = byte_size(Msg) - (byte_size(Rest_2) - 2),
                    <<Header:HeaderLen/binary, _/binary>> = Msg,
		    {#params{
                        socket = Socket,
                        protocol = Protocol,
                        hash_algorithm = HashAlgorithm,
                        block_crypto = BlockCrypto,
                        iv = {IVSalt, IVNo},
                        key = {KeySalt, OtherSalt},
                        tag_len = TagLen,
                        rekey_interval = RekeyInterval},
                     Header}
            end
    end.



hash_key(HashAlgorithm, KeySalt, KeyData) ->
    KeyLen = byte_size(KeySalt),
    <<Key:KeyLen/binary, _/binary>> =
        crypto:hash(HashAlgorithm, [KeySalt, KeyData]),
    Key.

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
     socket = Socket, rekey_interval = Seq,
     key = Key, iv = {IVSalt, _}, hash_algorithm = HashAlgorithm} = Params,
  Seq, Cleartext) ->
    %%
    KeyLen = byte_size(Key),
    IVSaltLen = byte_size(IVSalt),
    Chunk = <<KeySalt:KeyLen/binary, IVSalt_1:IVSaltLen/binary, IVNo_1:48>> =
        crypto:strong_rand_bytes(KeyLen + IVSaltLen + 6),
    case
        gen_tcp:send(
          Socket, encrypt_chunk(Params, Seq, [?REKEY_CHUNK, Chunk]))
    of
        ok ->
            Key_1 = hash_key(HashAlgorithm, Key, KeySalt),
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
     block_crypto = BlockCrypto,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen}, Seq, Cleartext) ->
    %%
    ChunkLen = iolist_size(Cleartext) + TagLen,
    AAD = <<Seq:32, ChunkLen:32>>,
    IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
    {Ciphertext, CipherTag} =
        crypto:block_encrypt(BlockCrypto, Key, IVBin, {AAD, Cleartext, TagLen}),
    Chunk = [Ciphertext,CipherTag],
    Chunk.

decrypt_chunk(
  #params{
     block_crypto = BlockCrypto,
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
                      Params, Seq, BlockCrypto, Key, IVBin,
                      {AAD, Ciphertext, CipherTag});
                _ ->
                    error
            end
    end.

block_decrypt(
  #params{rekey_interval = RekeyInterval} = Params,
  Seq, CipherName, Key, IV, Data) ->
    %%
    case crypto:block_decrypt(CipherName, Key, IV, Data) of
        <<?REKEY_CHUNK, Rest/binary>> ->
            KeyLen = byte_size(Key),
            IVSaltLen = byte_size(IV) - 6,
            case Rest of
                <<KeySalt:KeyLen/binary, IVSalt:IVSaltLen/binary, IVNo:48>> ->
                    Key_1 =
                        hash_key(
                          Params#params.hash_algorithm, Key, KeySalt),
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
monitor_dist_proc(Pid) ->
%%%    spawn(
%%%      fun () ->
%%%              MRef = erlang:monitor(process, Pid),
%%%              receive
%%%                  {'DOWN', MRef, _, _, normal} ->
%%%                      error_logger:error_report(
%%%                        [dist_proc_died,
%%%                         {reason, normal},
%%%                         {pid, Pid}]);
%%%                  {'DOWN', MRef, _, _, Reason} ->
%%%                      error_logger:info_report(
%%%                        [dist_proc_died,
%%%                         {reason, Reason},
%%%                         {pid, Pid}])
%%%              end
%%%      end),
    Pid.

dbg() ->
    dbg:stop(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(?MODULE, trace, cx),
    dbg:tpl(erlang, dist_ctrl_get_data_notification, cx),
    dbg:tpl(erlang, dist_ctrl_get_data, cx),
    dbg:tpl(erlang, dist_ctrl_put_data, cx),
    ok.
