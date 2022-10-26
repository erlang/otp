%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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

-export([supported/0, listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1]).

%% Generalized dist API, for sibling IPv6 module inet6_crypto_dist
-export([gen_listen/2, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_close/2, gen_select/2]).

%% Debug
%%%-compile(export_all).
-export([dbg/0, test_server/0, test_client/1]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(PACKET_SIZE, 65536).
-define(BUFFER_SIZE, (?PACKET_SIZE bsl 4)).

%% -------------------------------------------------------------------------

%% The curve choice greatly affects setup time,
%% we really want an Edwards curve but that would
%% require a very new openssl version.
%% Twisted brainpool curves (*t1) are faster than
%% non-twisted (*r1), 256 is much faster than 384,
%% and so on...
%%% -define(CURVE, brainpoolP384t1).
%%% -define(CURVE, brainpoolP256t1).
-define(CURVE, secp256r1).
-define(CIPHER, aes_gcm).
-define(HMAC, sha256).

-record(params,
        {socket,
         dist_handle,
         hmac_algorithm = ?HMAC,
         aead_cipher = ?CIPHER,
         rekey_key,
         iv = 12,
         key = 16,
         tag_len = 16,
         rekey_count = 262144,
         rekey_time = 7200000, % 2 hours
         rekey_msg
        }).

params(Socket) ->
    #params{socket = Socket}.


-record(key_pair,
        {type = ecdh,
         params = ?CURVE,
         public,
         private,
         life_time = 3600000, % 1 hour
         life_count = 256 % Number of connection setups
        }).

supported() ->
    Curve = lists:member(?CURVE, crypto:supports(curves)),
    Cipher = lists:member(?CIPHER, crypto:supports(ciphers)),
    Hmac =
        lists:member(hmac, crypto:supports(macs)) andalso
        lists:member(?HMAC, crypto:supports(hashs)),
    if
        not Curve ->
            "curve " ++ atom_to_list(?CURVE);
        not Cipher ->
            "cipher " ++ atom_to_list(?CIPHER);
        not Hmac ->
            "HMAC " ++ atom_to_list(?HMAC);
        true ->
            ok
    end.

%% -------------------------------------------------------------------------
%% Keep the node's public/private key pair in the process state
%% of a key pair server linked to the acceptor process.
%% Create the key pair the first time it is needed
%% so crypto gets time to start first.
%%

start_key_pair_server() ->
    monitor_dist_proc(
      key_pair_server,
      spawn_link(
        fun () ->
                register(?MODULE, self()),
                key_pair_server()
        end)).

key_pair_server() ->
    key_pair_server(undefined, undefined, undefined).
%%
key_pair_server(
  #key_pair{life_time = LifeTime, life_count = LifeCount} = KeyPair) ->
    %% Presuming: 1 < LifeCount
    Timer =
        case LifeCount of
            1 ->
                undefined;
            _ ->
                erlang:start_timer(LifeTime, self(), discard)
        end,
    key_pair_server(KeyPair, Timer, LifeCount - 1).
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
    erlang_cancel_timer(Timer).

start_rekey_timer(Time) ->
    Timer = erlang:start_timer(Time, self(), rekey_time),
    {timeout, Timer, rekey_time}.

cancel_rekey_timer({timeout, Timer, rekey_time}) ->
    erlang_cancel_timer(Timer).

erlang_cancel_timer(Timer) ->
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
%% connects to the other node's listen socket and when that is successful
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
    {ok, Host} = inet:gethostname(),
    case inet_tcp_dist:gen_listen(Driver, Name, Host) of
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
      acceptor,
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
      accept_controller,
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
                hs_data(
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
      setup_controller,
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
                hs_data(
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


hs_data(NetKernel, MyNode, DistCtrl, Timer, Socket, Family) ->
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
          output_handler,
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
%% by the other side, plus the rekey count.  The rekey count
%% is just there to get an early check for if the other side's
%% maximum rekey count is acceptable, it is just an embryo
%% of some better check.  Any side may rekey earlier but if the
%% rekey count is exceeded the connection fails.  Rekey is also
%% triggered by a timer.
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
%% The only reaction to errors is to crash noisily (?) which will bring
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
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:info_report(
              [init_recv_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
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
        crypto:crypto_one_time_aead(
          AeadCipher, Key1A, IV1A, Plaintext, AAD, TagLen, true),
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
     rekey_count = RekeyCount} = Params,
  Secret, KeyPair, R2A, R3A, Msg) ->
    %%
    RLen = KeyLen + IVLen,
    case Msg of
        <<R1B:RLen/binary, Tag:TagLen/binary, Ciphertext/binary>> ->
            {Key1B, IV1B} = hmac_key_iv(HmacAlgo, R1B, Secret, KeyLen, IVLen),
            MsgLen = byte_size(Msg),
            AAD = [<<MsgLen:32>>, R1B],
            case
                crypto:crypto_one_time_aead(
                  AeadCipher, Key1B, IV1B, Ciphertext, AAD, Tag, false)
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
                    StartCleartext = [R2B, R3B, <<RekeyCount:32>>],
                    StartMsgLen = TagLen + iolist_size(StartCleartext),
                    StartAAD = <<StartMsgLen:32>>,
                    {StartCiphertext, StartTag} =
                        crypto:crypto_one_time_aead(
                          AeadCipher, Key2A, IV2A,
                          StartCleartext, StartAAD, TagLen, true),
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
     rekey_count = RekeyCountA} = RecvParams, R2A, R3A, Msg) ->
    %%
    case Msg of
        <<Tag:TagLen/binary, Ciphertext/binary>> ->
            KeyLen = byte_size(Key2B),
            IVLen = byte_size(IV2B),
            RLen = KeyLen + IVLen,
            MsgLen = byte_size(Msg),
            AAD = <<MsgLen:32>>,
            case
                crypto:crypto_one_time_aead(
                  AeadCipher, Key2B, IV2B, Ciphertext, AAD, Tag, false)
            of
                <<R2A:RLen/binary, R3A:RLen/binary, RekeyCountB:32>>
                  when RekeyCountA =< (RekeyCountB bsl 2),
                       RekeyCountB =< (RekeyCountA bsl 2) ->
                    RecvParams#params{rekey_count = RekeyCountB}
            end
    end.

hmac_key_iv(HmacAlgo, MacKey, Data, KeyLen, IVLen) ->
    <<Key:KeyLen/binary, IV:IVLen/binary>> =
        crypto:macN(hmac, HmacAlgo, MacKey, Data, KeyLen + IVLen),
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
                  input_handler,
                  spawn_opt(
                    fun () ->
                            link(Controller),
                            receive
                                DistHandle ->
                                    input_handler(
                                      RecvParams, RecvSeq, DistHandle)
                            end
                    end,
                    [link,
                     {priority, normal},
                     {message_queue_data, off_heap},
                     {fullsweep_after, 0}])),
            _ = monitor(process, InputHandler), % For the benchmark test
            ok = gen_tcp:controlling_process(Socket, InputHandler),
            false = erlang:dist_ctrl_set_opt(DistHandle, get_size, true),
            ok = erlang:dist_ctrl_input_handler(DistHandle, InputHandler),
            InputHandler ! DistHandle,
            reply(From, ok),
            process_flag(priority, normal),
            output_handler(SendParams, SendSeq, DistHandle);
        %%
        {?MODULE, From, {send, Data}} ->
            {SendParams_1, SendSeq_1, Result} =
                encrypt_and_send_chunk(
                  SendParams, SendSeq,
                  [?HANDSHAKE_CHUNK, Data], 1 + iolist_size(Data)),
            if
                Result =:= ok ->
                    reply(From, ok),
                    handshake(
                      SendParams_1, SendSeq_1, RecvParams, RecvSeq,
                      Controller);
                true ->
                    reply(From, {error, closed}),
                    death_row({send, trace(Result)})
            end;
        {?MODULE, From, recv} ->
            {RecvParams_1, RecvSeq_1, Result} =
                recv_and_decrypt_chunk(RecvParams, RecvSeq),
            case Result of
                {ok, _} ->
                    reply(From, Result),
                    handshake(
                      SendParams, SendSeq, RecvParams_1, RecvSeq_1,
                      Controller);
                {error, _} ->
                    reply(From, Result),
                    death_row({recv, trace(Result)})
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
                UnknownChunk when is_binary(UnknownChunk) ->
                    error_logger:error_report(
                      [?FUNCTION_NAME,
                       {reason,unknown_chunk}]),
                    {RecvParams, RecvSeq + 1, {error, unknown_chunk}};
                #params{} = RecvParams_1 ->
                    recv_and_decrypt_chunk(RecvParams_1, 0);
                error ->
                    error_logger:error_report(
                      [?FUNCTION_NAME,
                       {reason,decrypt_error}]),
                    {RecvParams, RecvSeq, {error, decrypt_error}}
            end;
        Error ->
            {RecvParams, RecvSeq, Error}
    end.

%% -------------------------------------------------------------------------
%% Output handler process
%%
%% Await an event about what to do; fetch dist data from the VM,
%% send a dist tick, or rekey outbound encryption parameters.
%%
%% In case we are overloaded and could get many accumulated
%% dist_data or dist_tick messages; make sure to flush all of them
%% before proceeding with what to do.  But, do not use selective
%% receive since that does not perform well when there are
%% many messages in the process mailbox.

%% Entry function
output_handler(Params, Seq, DistHandle) ->
    try
        _ = crypto:rand_seed_alg(crypto_cache),
        erlang:dist_ctrl_get_data_notification(DistHandle),
        output_handler(
          Params#params{
            dist_handle = DistHandle,
            rekey_msg = start_rekey_timer(Params#params.rekey_time)},
          Seq)
    catch
        Class : Reason : Stacktrace ->
            error_logger:info_report(
              [output_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Loop top
%%
%% State: lurking until any interesting message
output_handler(Params, Seq) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(Params, Seq);
                dist_tick ->
                    output_handler_tick(Params, Seq);
                _ when Msg =:= Params#params.rekey_msg ->
                    output_handler_rekey(Params, Seq);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler(Params, Seq)
            end
    end.

%% State: we have received at least one dist_data message
output_handler_data(Params, Seq) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(Params, Seq);
                dist_tick ->
                    output_handler_data(Params, Seq);
                _ when Msg =:= Params#params.rekey_msg ->
                    output_handler_rekey(Params, Seq);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler_data(Params, Seq)
            end
    after 0 ->
            {Params_1, Seq_1} = output_handler_xfer(Params, Seq),
            erlang:dist_ctrl_get_data_notification(Params#params.dist_handle),
            output_handler(Params_1, Seq_1)
    end.

%% State: we have received at least one dist_tick but no dist_data message
output_handler_tick(Params, Seq) ->
    receive
        Msg ->
            case Msg of
                dist_data ->
                    output_handler_data(Params, Seq);
                dist_tick ->
                    output_handler_tick(Params, Seq);
                _ when Msg =:= Params#params.rekey_msg ->
                    output_handler_rekey(Params, Seq);
                _ ->
                    %% Ignore
                    _ = trace(Msg),
                    output_handler_tick(Params, Seq)
            end
    after 0 ->
            TickSize = 7 + rand:uniform(56),
            TickData = binary:copy(<<0>>, TickSize),
            {Params_1, Seq_1, Result} =
                encrypt_and_send_chunk(
                  Params, Seq, [?TICK_CHUNK, TickData], 1 + TickSize),
            if
                Result =:= ok ->
                    output_handler(Params_1, Seq_1);
                true ->
                    death_row({send_tick, trace(Result)})
            end
    end.

output_handler_rekey(Params, Seq) ->
    case encrypt_and_send_rekey_chunk(Params, Seq) of
        #params{} = Params_1 ->
            output_handler(Params_1, 0);
        SendError ->
            death_row({send_rekey, trace(SendError)})
    end.


%% Get outbound data from VM; encrypt and send,
%% until the VM has no more
%%
output_handler_xfer(Params, Seq) ->
    output_handler_xfer(Params, Seq, [], 0, []).
%%
%% Front,Size,Rear is an Okasaki queue of binaries with total byte Size
%%
output_handler_xfer(Params, Seq, Front, Size, Rear)
  when ?CHUNK_SIZE =< Size ->
    %%
    %% We have a full chunk or more
    %% -> collect one chunk or less and send
    output_handler_collect(Params, Seq, Front, Size, Rear);
output_handler_xfer(Params, Seq, Front, Size, Rear) ->
    %% when Size < ?CHUNK_SIZE ->
    %%
    %% We do not have a full chunk -> try to fetch more from VM
    case erlang:dist_ctrl_get_data(Params#params.dist_handle) of
        none ->
            if
                Size =:= 0 ->
                    %% No more data from VM, nothing buffered
                    %% -> go back to lurking
                    {Params, Seq};
                true ->
                    %% The VM had no more -> send what we have
                    output_handler_collect(Params, Seq, Front, Size, Rear)
            end;
        {Len,Iov} ->
            output_handler_enq(
              Params, Seq, Front, Size + 4 + Len, [<<Len:32>>|Rear], Iov)
    end.

%% Enqueue VM data while splitting large binaries into ?CHUNK_SIZE
%%
output_handler_enq(Params, Seq, Front, Size, Rear, []) ->
    output_handler_xfer(Params, Seq, Front, Size, Rear);
output_handler_enq(Params, Seq, Front, Size, Rear, [Bin|Iov]) ->
    output_handler_enq(Params, Seq, Front, Size, Rear, Iov, Bin).
%%
output_handler_enq(Params, Seq, Front, Size, Rear, Iov, Bin) ->
    BinSize = byte_size(Bin),
    if
        BinSize =< ?CHUNK_SIZE ->
            output_handler_enq(
              Params, Seq, Front, Size, [Bin|Rear], Iov);
        true ->
            <<Bin1:?CHUNK_SIZE/binary, Bin2/binary>> = Bin,
            output_handler_enq(
              Params, Seq, Front, Size, [Bin1|Rear], Iov, Bin2)
    end.

%% Collect small binaries into chunks of at most ?CHUNK_SIZE
%%
output_handler_collect(Params, Seq, [], Zero, []) ->
    0 = Zero, % Assert
    %% No more enqueued -> try to get more form VM
    output_handler_xfer(Params, Seq);
output_handler_collect(Params, Seq, Front, Size, Rear) ->
    output_handler_collect(Params, Seq, Front, Size, Rear, [], 0).
%%
output_handler_collect(Params, Seq, [], Zero, [], Acc, DataSize) ->
    0 = Zero, % Assert
    output_handler_chunk(Params, Seq, [], Zero, [], Acc, DataSize);
output_handler_collect(Params, Seq, [], Size, Rear, Acc, DataSize) ->
    %% Okasaki queue transfer Rear -> Front
    output_handler_collect(
      Params, Seq, lists:reverse(Rear), Size, [], Acc, DataSize);
output_handler_collect(
  Params, Seq, [Bin|Iov] = Front, Size, Rear, Acc, DataSize) ->
    BinSize = byte_size(Bin),
    DataSize_1 = DataSize + BinSize,
    if
        ?CHUNK_SIZE < DataSize_1 ->
            %% Bin does not fit in chunk -> send Acc
            output_handler_chunk(
              Params, Seq, Front, Size, Rear, Acc, DataSize);
        DataSize_1 < ?CHUNK_SIZE ->
            %% Chunk not full yet -> try to accumulate more
            output_handler_collect(
              Params, Seq, Iov, Size - BinSize, Rear, [Bin|Acc], DataSize_1);
        true -> % DataSize_1 == ?CHUNK_SIZE ->
            %% Optimize one iteration; Bin fits exactly -> accumulate and send
            output_handler_chunk(
              Params, Seq, Iov, Size - BinSize, Rear, [Bin|Acc], DataSize_1)
    end.

%% Encrypt and send a chunk
%%
output_handler_chunk(Params, Seq, Front, Size, Rear, Acc, DataSize) ->
    Data = lists:reverse(Acc),
    {Params_1, Seq_1, Result} =
        encrypt_and_send_chunk(Params, Seq, [?DATA_CHUNK|Data], 1 + DataSize),
    if
        Result =:= ok ->
            %% Try to collect another chunk
            output_handler_collect(Params_1, Seq_1, Front, Size, Rear);
        true ->
            death_row({send_chunk, trace(Result)})
    end.

%% -------------------------------------------------------------------------
%% Input handler process
%%

%% Entry function
input_handler(#params{socket = Socket} = Params, Seq, DistHandle) ->
    try
        ok =
            inet:setopts(
              Socket, [{active, ?TCP_ACTIVE}, inet_tcp_dist:nodelay()]),
        input_handler(
          Params#params{dist_handle = DistHandle},
          Seq)
    catch
        Class : Reason : Stacktrace ->
            error_logger:info_report(
              [input_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% Loop top
input_handler(Params, Seq) ->
    %% Shortcut into the loop
    {Params_1, Seq_1, Data} = input_data(Params, Seq),
    input_handler(Params_1, Seq_1, Data, [], byte_size(Data)).
%%
input_handler(Params, Seq, First, Buffer, Size) ->
    %% Size is size of First + Buffer
    case First of
        <<Packet1Size:32, Packet1:Packet1Size/binary,
          Packet2Size:32, Packet2:Packet2Size/binary, Rest/binary>> ->
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet1),
            erlang:dist_ctrl_put_data(DistHandle, Packet2),
            input_handler(
              Params, Seq, Rest,
              Buffer, Size - (8 + Packet1Size + Packet2Size));
        <<PacketSize:32, Packet:PacketSize/binary, Rest/binary>> ->
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(
              Params, Seq, Rest, Buffer, Size - (4 + PacketSize));
        <<PacketSize:32, PacketStart/binary>> ->
            %% Partial packet in First
            input_handler(
              Params, Seq, PacketStart, Buffer, Size - 4, PacketSize);
        <<Bin/binary>> ->
            %% Partial header in First
            if
                4 =< Size ->
                    %% Complete header in First + Buffer
                    {First_1, Buffer_1, PacketSize} =
                        input_get_packet_size(Bin, lists:reverse(Buffer)),
                    input_handler(
                      Params, Seq, First_1, Buffer_1, Size - 4, PacketSize);
                true ->
                    %% Incomplete header received so far
                    {Params_1, Seq_1, More} = input_data(Params, Seq),
                    input_handler(
                      Params_1, Seq_1, Bin,
                      [More|Buffer], Size + byte_size(More))
            end
    end.
%%
input_handler(Params, Seq, PacketStart, Buffer, Size, PacketSize) ->
    %% Size is size of PacketStart + Buffer
    RestSize = Size - PacketSize,
    if
        RestSize < 0 ->
            %% Incomplete packet received so far
            {Params_1, Seq_1, More} = input_data(Params, Seq),
            input_handler(
              Params_1, Seq_1, PacketStart,
              [More|Buffer], Size + byte_size(More), PacketSize);
        0 < RestSize, Buffer =:= [] ->
            %% Rest data in PacketStart
            <<Packet:PacketSize/binary, Rest/binary>> = PacketStart,
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(Params, Seq, Rest, [], RestSize);
        Buffer =:= [] -> % RestSize == 0
            %% No rest data
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, PacketStart),
            input_handler(Params, Seq);
        true ->
            %% Split packet from rest data
            LastBin = hd(Buffer),
            <<PacketLast:(byte_size(LastBin) - RestSize)/binary,
              Rest/binary>> = LastBin,
            Packet = [PacketStart|lists:reverse(tl(Buffer), PacketLast)],
            DistHandle = Params#params.dist_handle,
            erlang:dist_ctrl_put_data(DistHandle, Packet),
            input_handler(Params, Seq, Rest, [], RestSize)
    end.

input_get_packet_size(First, [Bin|Buffer]) ->
    MissingSize = 4 - byte_size(First),
    if
        MissingSize =< byte_size(Bin) ->
            <<Last:MissingSize/binary, Rest/binary>> = Bin,
            <<PacketSize:32>> = <<First/binary, Last/binary>>,
            {Rest, lists:reverse(Buffer), PacketSize};
        true ->
            input_get_packet_size(<<First/binary, Bin/binary>>, Buffer)
    end.

input_data(Params, Seq) ->
    receive Msg -> input_data(Params, Seq, Msg) end.
%%
input_data(#params{socket = Socket} = Params, Seq, Msg) ->
    case Msg of
        {tcp_passive, Socket} ->
            ok = inet:setopts(Socket, [{active, ?TCP_ACTIVE}]),
            input_data(Params, Seq);
        {tcp, Socket, Ciphertext} ->
            case decrypt_chunk(Params, Seq, Ciphertext) of
                <<?DATA_CHUNK, Chunk/binary>> ->
                    {Params, Seq + 1, Chunk};
                <<?TICK_CHUNK, _Dummy/binary>> ->
                    input_data(Params, Seq + 1);
                <<UnknownChunk/binary>> ->
                    error_logger:error_report(
                      [?FUNCTION_NAME,
                       {reason, unknown_chunk}]),
                    _ = trace(UnknownChunk),
                    exit(connection_closed);
                #params{} = Params_1 ->
                    input_data(Params_1, 0);
                error ->
                    _ = trace(decrypt_error),
                    exit(connection_closed)
            end;
        {tcp_closed = Reason, Socket} ->
            error_logger:info_report(
              [?FUNCTION_NAME,
               {reason, Reason}]),
            exit(connection_closed);
        Other ->
            %% Ignore...
            _ = trace(Other),
            input_data(Params, Seq)
    end.

%% -------------------------------------------------------------------------
%% Encryption and decryption helpers

encrypt_and_send_chunk(
  #params{
     socket = Socket, rekey_count = RekeyCount, rekey_msg = RekeyMsg} = Params,
  Seq, Cleartext, Size) when Seq =:= RekeyCount ->
    %%
    cancel_rekey_timer(RekeyMsg),
    case encrypt_and_send_rekey_chunk(Params, Seq) of
        #params{} = Params_1 ->
            Result =
                gen_tcp:send(
                  Socket, encrypt_chunk(Params, 0, Cleartext, Size)),
            {Params_1, 1, Result};
        SendError ->
            {Params, Seq + 1, SendError}
    end;
encrypt_and_send_chunk(
  #params{socket = Socket} = Params, Seq, Cleartext, Size) ->
    Result =
        gen_tcp:send(Socket, encrypt_chunk(Params, Seq, Cleartext, Size)),
    {Params, Seq + 1, Result}.

encrypt_and_send_rekey_chunk(
  #params{
     socket = Socket,
     rekey_key = PubKeyB,
     key = Key,
     iv = {IVSalt, IVNo},
     hmac_algorithm = HmacAlgo} = Params,
  Seq) ->
    %%
    KeyLen = byte_size(Key),
    IVSaltLen = byte_size(IVSalt),
    #key_pair{public = PubKeyA} = KeyPair = get_new_key_pair(),
    case
        gen_tcp:send(
          Socket,
          encrypt_chunk(
            Params, Seq, [?REKEY_CHUNK, PubKeyA], 1 + byte_size(PubKeyA)))
    of
        ok ->
            SharedSecret = compute_shared_secret(KeyPair, PubKeyB),
            IV = <<(IVNo + Seq):48>>,
            {Key_1, <<IVSalt_1:IVSaltLen/binary, IVNo_1:48>>} =
                hmac_key_iv(
                  HmacAlgo, SharedSecret, [Key, IVSalt, IV],
                  KeyLen, IVSaltLen + 6),
            Params#params{
              key = Key_1, iv = {IVSalt_1, IVNo_1},
              rekey_msg = start_rekey_timer(Params#params.rekey_time)};
        SendError ->
            SendError
    end.

encrypt_chunk(
  #params{
     aead_cipher = AeadCipher,
     iv = {IVSalt, IVNo}, key = Key, tag_len = TagLen},
  Seq, Cleartext, Size) ->
    %%
    ChunkLen = Size + TagLen,
    AAD = <<Seq:32, ChunkLen:32>>,
    IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
    {Ciphertext, CipherTag} =
        crypto:crypto_one_time_aead(
          AeadCipher, Key, IVBin, Cleartext, AAD, TagLen, true),
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
            error_logger:error_report(
              [?FUNCTION_NAME,
               {reason,short_chunk}]),
            error;
        true ->
            AAD = <<Seq:32, ChunkLen:32>>,
            IVBin = <<IVSalt/binary, (IVNo + Seq):48>>,
            CiphertextLen = ChunkLen - TagLen,
            <<Ciphertext:CiphertextLen/binary,
              CipherTag:TagLen/binary>> = Chunk,
            block_decrypt(
              Params, Seq, AeadCipher, Key, IVBin,
              Ciphertext, AAD, CipherTag)
    end.

block_decrypt(
  #params{
     rekey_key = #key_pair{public = PubKeyA} = KeyPair,
     rekey_count = RekeyCount} = Params,
  Seq, AeadCipher, Key, IV, Ciphertext, AAD, CipherTag) ->
    case
        crypto:crypto_one_time_aead(
          AeadCipher, Key, IV, Ciphertext, AAD, CipherTag, false)
    of
        <<?REKEY_CHUNK, Chunk/binary>> ->
            PubKeyLen = byte_size(PubKeyA),
            case Chunk of
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
                    error_logger:error_report(
                      [?FUNCTION_NAME,
                       {reason,bad_rekey_chunk}]),
                    error
            end;
        Chunk when is_binary(Chunk) ->
            case Seq of
                RekeyCount ->
                    %% This was one chunk too many without rekeying
                    error_logger:error_report(
                      [?FUNCTION_NAME,
                       {reason,rekey_overdue}]),
                    error;
                _ ->
                    Chunk
            end;
        error ->
            error_logger:error_report(
              [?FUNCTION_NAME,
               {reason,decrypt_error}]),
            error
    end.

%% -------------------------------------------------------------------------

%% Wait for getting killed by process link,
%% and if that does not happen - drop dead

death_row(Reason) ->
    error_logger:info_report(
      [?FUNCTION_NAME,
       {reason, Reason},
       {pid, self()}]),
    receive
    after 5000 ->
            death_row_timeout(Reason)
    end.

death_row_timeout(Reason) ->
    error_logger:error_report(
      [?FUNCTION_NAME,
       {reason, Reason},
       {pid, self()}]),
    exit(Reason).

%% -------------------------------------------------------------------------

%% Trace point
trace(Term) -> Term.

%% Keep an eye on this Pid (debug)
-ifdef(undefined).
monitor_dist_proc(_Tag, Pid) ->
    Pid.
-else.
monitor_dist_proc(Tag, Pid) ->
    spawn(
      fun () ->
              MRef = erlang:monitor(process, Pid),
              error_logger:info_report(
                [?FUNCTION_NAME,
                 {type, Tag},
                 {pid, Pid}]),
              receive
                  {'DOWN', MRef, _, _, normal} ->
                      error_logger:error_report(
                        [?FUNCTION_NAME,
                         {reason, normal},
                         {pid, Pid}]);
                  {'DOWN', MRef, _, _, Reason} ->
                      error_logger:info_report(
                        [?FUNCTION_NAME,
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
