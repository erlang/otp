%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(socket).

-compile({no_auto_import,[error/1]}).

%% Administrative and "global" utility functions
-export([
	 on_load/0, on_load/1, on_load/2,
	 info/0
        ]).

-export([
         open/2, open/3, open/4,
         bind/2,
         connect/2, connect/3,
         listen/1, listen/2,
         accept/1, accept/2,

         send/2, send/3, send/4,
         sendto/4, sendto/5,
         %% sendmsg/4,
         %% writev/4, OR SENDV? It will be strange for recv then: recvv (instead of readv)

         recv/2, recv/3, recv/4,
         recvfrom/1, recvfrom/2, recvfrom/3, recvfrom/4,
         %% recvmsg/4,
         %% readv/3,

         close/1,
         shutdown/2,

         setopt/4,
         getopt/3,

         %% Some IPv6 utility functions
         link_if2idx/1,
         link_idx2if/1,
         link_ifs/0
        ]).

-export_type([
              domain/0,
              type/0,
              protocol/0,
              socket/0,

              ip_address/0,
              ip4_address/0,
              ip6_address/0,
              in_sockaddr/0,
              in4_sockaddr/0,
              in6_sockaddr/0,
              port_number/0,

              accept_flags/0,
              accept_flag/0,

              send_flags/0,
              send_flag/0,

              shutdown_how/0,

              sockopt_level/0,
              otp_socket_option/0,
              socket_option/0,
              ip_socket_option/0,
              ipv6_socket_option/0,
              tcp_socket_option/0,
              udp_socket_option/0,
              sctp_socket_option/0,

              ip_tos_flag/0
             ]).


%% We support only a subset of all domains.
-type domain() :: local | inet | inet6.

%% We support only a subset of all types.
-type type()   :: stream | dgram | raw | seqpacket.

%% We support only a subset of all protocols:
-type protocol() :: ip | tcp | udp | sctp.

-type ip_address() :: ip4_address() | ip6_address().

-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.

-type uint20()        :: 0..16#FFFFF.
-type uint32()        :: 0..16#FFFFFFFF.
-type in6_flow_info() :: uint20().
-type in6_scope_id()  :: uint32().

-type ip6_address() ::
           {0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535}.

%% <KOLLA>
%% Should we do these as maps instead?
%% If we do we may need to include the family (domain) in the
%% map (as the native type do. See struct sockaddr_in6).
%% </KOLLA>
-type in_sockaddr() :: in4_sockaddr() | in6_sockaddr().

-record(in4_sockaddr, {port = 0   :: port_number(),
                       addr = any :: any | loopback | ip4_address()}).
-type in4_sockaddr() :: #in4_sockaddr{}.
-record(in6_sockaddr, {port     = 0   :: port_number(),
                       addr     = any :: any | loopback | ip6_address(),
                       flowinfo = 0   :: in6_flow_info(),
                       scope_id = 0   :: in6_scope_id()}).
-type in6_sockaddr() :: #in6_sockaddr{}.

-type port_number() :: 0..65535.

%% otp    - The option is internal to our (OTP) imeplementation.
%% socket - The socket layer (SOL_SOCKET).
%% ip     - The IP layer (SOL_IP or is it IPPROTO_IP?).
%% ipv6   - The IPv6 layer (SOL_IPV6).
%% tcp    - The TCP (Transport Control Protocol) layer (IPPROTO_TCP).
%% udp    - The UDP (User Datagram Protocol) layer (IPPROTO_UDP).
%% sctp   - The SCTP (Stream Control Transmission Protocol) layer (IPPROTO_SCTP).
%% Int    - Raw level, sent down and used "as is".
-type sockopt_level() :: otp |
                         socket |
                         ip | ipv6 | tcp | udp | sctp |
                         non_neg_integer().

%% There are some options that are 'read-only'.
%% Should those be included here or in a special list?
%% Should we just document it and leave it to the user?
%% Or catch it in the encode functions?
%% A setopt for a readonly option leads to einval?

-type otp_socket_option() :: debug |
                             iow |
                             rcvbuf |
                             sndbuf.
%% Shall we have special treatment of linger??
%% read-only options:
%% domain | protocol | type.
%% FreeBSD (only?): acceptfilter
-type socket_option() :: acceptconn |
                         acceptfilter |
                         bindtodevice |
                         broadcast |
                         busy_poll |
                         debug |
                         dontroute |
                         error |
                         keepalive |
                         linger |
                         mark |
                         oobinline |
                         passcred |
                         peek_off |
                         peek_cred |
                         priority |
                         rcvbuf |
                         rcvbufforce |
                         rcvlowat |
                         rcvtimeo |
                         reuseaddr |
                         reuseport |
                         rxq_ovfl |
                         sndlowat |
                         sndtimeo |
                         setfib |
                         sndbuf |
                         sndbufforce |
                         timestamp |
                         type.

%% Read-only options:
%% mtu
%%
%% Options only valid on FreeBSD?:
%% dontfrag
%% Options only valid for RAW sockets:
%% nodefrag (linux only?)
-type ip_socket_option() :: add_membership |
                            add_source_membership |
                            block_source |
                            dontfrag |
                            drop_membership |
                            drop_source_membership |
                            freebind |
                            hdrincl |
                            minttl |
                            msfilter |
                            mtu |
                            mtu_discover |
                            multicast_all |
                            multicast_if |
                            multicast_loop |
                            multicast_ttl |
                            nodefrag |
                            options |
                            pktinfo |
                            recverr |
                            recvif |
                            recvdstaddr |
                            recvopts |
                            recvorigdstaddr |
                            recvtos |
                            recvttl |
                            retopts |
                            router_alert |
                            sndsrcaddr |
                            tos |
                            transparent |
                            ttl |
                            unblock_source.
-type ipv6_socket_option() ::
        addform |
        add_membership |
        authhdr |
        auth_level |
        checksum |
        drop_membership |
        dstopts |
        esp_trans_level |
        esp_network_level |
        faith |
        flowinfo |
        hoplimit |
        hopopts |
        ipcomp_level |
        join_group |
        leave_group |
        mtu |
        mtu_discover |
        multicast_hops |
        multicast_if |
        multicast_loop |
        portrange |
        pktinfo |
        pktoptions |
        recverr |
        recvpktinfo |
        recvtclass |
        router_alert |
        rthdr |
        tclass |
        unicast_hops |
        use_min_mtu |
        v6only.

-type tcp_socket_option()  :: congestion |
                              cork |
                              maxseg |
                              nodelay.

-type udp_socket_option() :: cork.

-type sctp_socket_option() ::
        adaption_layer |
        associnfo |
        auth_active_key |
        auth_asconf |
        auth_chunk |
        auth_key |
        auth_delete_key |
        autoclose |
        context |
        default_send_params |
        delayed_ack_time |
        disable_fragments |
        hmac_ident |
        events |
        explicit_eor |
        fragment_interleave |
        get_peer_addr_info |
        initmsg |
        i_want_mapped_v4_addr |
        local_auth_chunks |
        maxseg |
        maxburst |
        nodelay |
        partial_delivery_point |
        peer_addr_params |
        peer_auth_chunks |
        primary_addr |
        reset_streams |
        rtoinfo |
        set_peer_primary_addr |
        status |
        use_ext_recvinfo.

%% -type plain_socket_options()  :: integer().
%% -type sockopts() :: otp_socket_options() |
%%                     socket_options() |
%%                     ip_socket_options() |
%%                     ipv6_socket_options() |
%%                     tcp_socket_options() |
%%                     udp_socket_options() |
%%                     sctp_socket_options() |
%%                     plain_socket_options().

%% If the integer value is used its up to the caller to ensure its valid!
-type ip_tos_flag() :: lowdeley |
                       throughput |
                       reliability |
                       mincost |
                       integer().

-type socket_info() :: map().
-record(socket, {info :: socket_info(),
                 ref  :: reference()}).
%% -opaque socket() :: {socket, socket_info(), reference()}.
-opaque socket() :: #socket{}.

-type accept_flags() :: [accept_flag()].
-type accept_flag()  :: nonblock | cloexec.

-type send_flags() :: [send_flag()].
-type send_flag()  :: confirm |
                      dontroute |
                      eor |
                      more |
                      nosignal |
                      oob.

%% Extend with OWN flags for other usage:
%%   - adapt-buffer-sz:
%%     This will have the effect that the nif recvfrom will use
%%     MSG_PEEK to ensure no part of the message is lost, but if
%%     necessary adapt (increase) the buffer size until all of
%%     it fits.
%%
%% Note that not all of these flags is useful for every recv function!
%%
-type recv_flags() :: [recv_flag()].
-type recv_flag()  :: cmsg_cloexec |
                      errqueue |
                      oob |
                      peek |
                      trunc.

-type shutdown_how() :: read | write | read_write.

-record(msg_hdr,
        {
          %% Optional address
          %% On an unconnected socket this is used to specify the target
          %% address for a datagram.
          %% For a connected socket, this field should be specifiedset to [].
          name       :: list(),

          %% Scatter/gather array
          iov        :: [binary()], % iovec(),

          %% Ancillary (control) data
          ctrl       :: binary(),

          %% Unused
          flags = [] :: list()
        }).
-type msg_hdr() :: #msg_hdr{}.


-define(SOCKET_DOMAIN_LOCAL, 1).
-define(SOCKET_DOMAIN_UNIX,  ?SOCKET_DOMAIN_LOCAL).
-define(SOCKET_DOMAIN_INET,  2).
-define(SOCKET_DOMAIN_INET6, 3).

-define(SOCKET_TYPE_STREAM,    1).
-define(SOCKET_TYPE_DGRAM,     2).
-define(SOCKET_TYPE_RAW,       3).
%% -define(SOCKET_TYPE_RDM,       4).
-define(SOCKET_TYPE_SEQPACKET, 5).

-define(SOCKET_PROTOCOL_IP,    1).
-define(SOCKET_PROTOCOL_TCP,   2).
-define(SOCKET_PROTOCOL_UDP,   3).
-define(SOCKET_PROTOCOL_SCTP,  4).

-define(SOCKET_LISTEN_BACKLOG_DEFAULT, 5).

-define(SOCKET_ACCEPT_TIMEOUT_DEFAULT, infinity).

-define(SOCKET_SEND_FLAG_CONFIRM,    0).
-define(SOCKET_SEND_FLAG_DONTROUTE,  1).
-define(SOCKET_SEND_FLAG_EOR,        2).
-define(SOCKET_SEND_FLAG_MORE,       3).
-define(SOCKET_SEND_FLAG_NOSIGNAL,   4).
-define(SOCKET_SEND_FLAG_OOB,        5).

-define(SOCKET_SEND_FLAGS_DEFAULT,     []).
-define(SOCKET_SEND_TIMEOUT_DEFAULT,   infinity).
-define(SOCKET_SENDTO_TIMEOUT_DEFAULT, ?SOCKET_SEND_TIMEOUT_DEFAULT).

-define(SOCKET_RECV_FLAG_CMSG_CLOEXEC, 0).
-define(SOCKET_RECV_FLAG_ERRQUEUE,     1).
-define(SOCKET_RECV_FLAG_OOB,          2).
-define(SOCKET_RECV_FLAG_PEEK,         3).
-define(SOCKET_RECV_FLAG_TRUNC,        4).

-define(SOCKET_RECV_FLAGS_DEFAULT,   []).
-define(SOCKET_RECV_TIMEOUT_DEFAULT, infinity).

-define(SOCKET_OPT_LEVEL_OTP,       0).
-define(SOCKET_OPT_LEVEL_SOCKET,    1).
-define(SOCKET_OPT_LEVEL_IP,        2).
-define(SOCKET_OPT_LEVEL_IPV6,      3).
-define(SOCKET_OPT_LEVEL_TCP,       4).
-define(SOCKET_OPT_LEVEL_UDP,       5).
-define(SOCKET_OPT_LEVEL_SCTP,      6).

-define(SOCKET_OPT_OTP_DEBUG,           0).
-define(SOCKET_OPT_OTP_IOW,             1).

-define(SOCKET_OPT_SOCK_BROADCAST,       4).
-define(SOCKET_OPT_SOCK_DONTROUTE,       7).
-define(SOCKET_OPT_SOCK_KEEPALIVE,       9).
-define(SOCKET_OPT_SOCK_LINGER,         10).
-define(SOCKET_OPT_SOCK_PRIORITY,       16).
-define(SOCKET_OPT_SOCK_RCVBUF,         17).
-define(SOCKET_OPT_SOCK_REUSEADDR,      21).
-define(SOCKET_OPT_SOCK_SNDBUF,         27).

-define(SOCKET_OPT_IP_RECVTOS,          25).
-define(SOCKET_OPT_IP_ROUTER_ALERT,     28).
-define(SOCKET_OPT_IP_TOS,              30).
-define(SOCKET_OPT_IP_TTL,              32).

-define(SOCKET_OPT_IPV6_HOPLIMIT,       12).

-define(SOCKET_OPT_TCP_CONGESTION,      0).
-define(SOCKET_OPT_TCP_CORK,            2).
-define(SOCKET_OPT_TCP_MAXSEG,          3).
-define(SOCKET_OPT_TCP_NODELAY,         4).

-define(SOCKET_OPT_UDP_CORK,            0).

-define(SOCKET_OPT_SCTP_AUTOCLOSE,      7).
-define(SOCKET_OPT_SCTP_NODELAY,        22).

-define(SOCKET_SHUTDOWN_HOW_READ,       0).
-define(SOCKET_SHUTDOWN_HOW_WRITE,      1).
-define(SOCKET_SHUTDOWN_HOW_READ_WRITE, 2).



%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

-spec on_load() -> ok.

%% Should we require that the Extra arg is a map?
on_load() ->
    on_load(#{}).

-spec on_load(Extra) -> ok when
      Extra :: maps:map().

on_load(Extra) when is_map(Extra) ->
    on_load(atom_to_list(?MODULE), Extra).

-spec on_load(Path, Extra) -> ok when
      Path  :: string(),
      Extra :: maps:map().

on_load(Path, Extra) when is_list(Path) andalso is_map(Extra) ->
    on_load(nif_is_loaded(), Path, Extra).

on_load(true, _Path, _Extra) ->
    ok;
on_load(false, Path, Extra) ->
    %% ok = erlang:load_nif(Path, maps:put(timestamp, formated_timestamp(), Extra)).
    ok = erlang:load_nif(Path, Extra).



-spec info() -> list().

info() ->
    nif_info().



%% ===========================================================================
%%
%% The proper socket API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% open - create an endpoint for communication
%%

-spec open(Domain, Type) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type) ->
    open(Domain, Type, null).

-spec open(Domain, Type, Protocol) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: null | protocol(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type, Protocol) ->
    open(Domain, Type, Protocol, #{}).

-spec open(Domain, Type, Protocol, Extra) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: null | protocol(),
      Extra    :: map(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type, Protocol0, Extra) when is_map(Extra) ->
    try
        begin
            Protocol  = default_protocol(Protocol0, Type),
            EDomain   = enc_domain(Domain),
            EType     = enc_type(Domain, Type),
            EProtocol = enc_protocol(Type, Protocol),
            case nif_open(EDomain, EType, EProtocol, Extra) of
                {ok, SockRef} ->
                    SocketInfo = #{domain   => Domain,
                                   type     => Type,
                                   protocol => Protocol},
                    Socket = #socket{info = SocketInfo,
                                     ref  = SockRef},
                    {ok, Socket};
                {error, _} = ERROR ->
                    ERROR
            end
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason}
    end.

%% Note that this is just a convenience function for when the protocol was
%% not specified. If its actually specied, then that will be selected.
%% Also, this only works for the some of the type's (stream, dgram and
%% seqpacket).
default_protocol(null, stream)    -> tcp;
default_protocol(null, dgram)     -> udp;
default_protocol(null, seqpacket) -> sctp;
default_protocol(null, Type)      -> throw({error, {no_default_protocol, Type}});
default_protocol(Protocol, _)     -> Protocol.


%% ===========================================================================
%%
%% bind - bind a name to a socket
%%

-spec bind(Socket, FileOrAddr) -> ok | {error, Reason} when
      Socket     :: socket(),
      FileOrAddr :: binary() | string() | in_sockaddr() | any | loopback,
      Reason     :: term().

bind(Socket, File) when is_binary(File) ->
    if
        byte_size(File) =:= 0 ->
            {error, einval};
        byte_size(File) =< 255 ->
            nif_bind(Socket, File);
        true ->
            {error, einval}
    end;
bind(Socket, File) when is_list(File) andalso (File =/= []) ->
    Bin = unicode:characters_to_binary(File, file:native_name_encoding()),
    if
        byte_size(Bin) =< 255 ->
            nif_bind(Socket, Bin);
        true ->
            {error, einval}
    end;
bind(#socket{ref = SockRef} = _Socket, SockAddr) 
  when is_record(SockAddr, in4_sockaddr) orelse
       is_record(SockAddr, in6_sockaddr) orelse
       (SockAddr =:= any) orelse (SockAddr =:= loopback) ->
    nif_bind(SockRef, SockAddr).



%% ===========================================================================
%%
%% connect - initiate a connection on a socket
%%

-spec connect(Socket, SockAddr) -> ok | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: in_sockaddr(),
      Reason   :: term().

connect(Socket, SockAddr) ->
    connect(Socket, SockAddr, infinity).

-spec connect(Socket, SockAddr, Timeout) -> ok | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: in_sockaddr(),
      Timeout  :: timeout(),
      Reason   :: term().

connect(_Socket, _SockAddr, Timeout)
  when (is_integer(Timeout) andalso (Timeout =< 0)) ->
    {error, timeout};
connect(#socket{ref = SockRef}, SockAddr, Timeout)
  when (is_record(SockAddr, in4_sockaddr) orelse 
        is_record(SockAddr, in6_sockaddr)) andalso
       ((Timeout =:= infinity) orelse is_integer(Timeout)) ->
    TS = timestamp(Timeout),
    case nif_connect(SockRef, SockAddr) of
        ok ->
            %% Connected!
            ok;
        {ok, Ref} ->
            %% Connecting...
	    NewTimeout = next_timeout(TS, Timeout),
	    receive
		{select, SockRef, Ref, ready_output} ->
		    nif_finalize_connection(SockRef)
	    after NewTimeout ->
                    nif_cancel(SockRef, connect, Ref),
		    {error, timeout}
	    end;
	{error, _} = ERROR ->
	    ERROR
    end.



%% ===========================================================================
%%
%% listen - listen for connections on a socket
%%

-spec listen(Socket) -> ok | {error, Reason} when
      Socket  :: socket(),
      Reason  :: term().

listen(Socket) ->
    listen(Socket, ?SOCKET_LISTEN_BACKLOG_DEFAULT).

-spec listen(Socket, Backlog) -> ok | {error, Reason} when
      Socket  :: socket(),
      Backlog :: pos_integer(),
      Reason  :: term().

listen(#socket{ref = SockRef}, Backlog)
  when (is_integer(Backlog) andalso (Backlog >= 0)) ->
    nif_listen(SockRef, Backlog).




%% ===========================================================================
%%
%% accept, accept4 - accept a connection on a socket
%%

-spec accept(LSocket) -> {ok, Socket} | {error, Reason} when
      LSocket :: socket(),
      Socket  :: socket(),
      Reason  :: term().

accept(Socket) ->
    accept(Socket, ?SOCKET_ACCEPT_TIMEOUT_DEFAULT).

-spec accept(LSocket, Timeout) -> {ok, Socket} | {error, Reason} when
      LSocket :: socket(),
      Timeout :: timeout(),
      Socket  :: socket(),
      Reason  :: term().

%% Do we really need this optimization?
accept(_, Timeout) when is_integer(Timeout) andalso (Timeout =< 0) ->
    {error, timeout};
accept(#socket{info = SI, ref = LSockRef}, Timeout)
  when is_integer(Timeout) orelse (Timeout =:= infinity) ->
    do_accept(LSockRef, SI, Timeout).

do_accept(LSockRef, SI, Timeout) ->
    TS     = timestamp(Timeout),
    AccRef = make_ref(),
    case nif_accept(LSockRef, AccRef) of
        {ok, SockRef} ->
            SocketInfo = #{domain    => maps:get(domain,   SI),
                           type      => maps:get(type,     SI),
                           protocol  => maps:get(protocol, SI)},
            Socket = #socket{info = SocketInfo,
                             ref  = SockRef},
            {ok, Socket};
        {error, eagain} ->
	    NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, LSockRef, AccRef, ready_input} ->
                    do_accept(LSockRef, SI, next_timeout(TS, Timeout));

                {nif_abort, AccRef, Reason} ->
                    {error, Reason}

            after NewTimeout ->
                    nif_cancel(LSockRef, accept, AccRef),
                    flush_select_msgs(LSockRef, AccRef),
                    {error, timeout}
            end
    end.



%% ===========================================================================
%%
%% send, sendto, sendmsg - send a message on a socket
%%

send(Socket, Data) ->
    send(Socket, Data, ?SOCKET_SEND_FLAGS_DEFAULT, ?SOCKET_SEND_TIMEOUT_DEFAULT).

send(Socket, Data, Flags) when is_list(Flags) ->
    send(Socket, Data, Flags, ?SOCKET_SEND_TIMEOUT_DEFAULT);
send(Socket, Data, Timeout) ->
    send(Socket, Data, ?SOCKET_SEND_FLAGS_DEFAULT, Timeout).

-spec send(Socket, Data, Flags, Timeout) -> ok | {error, Reason} when
      Socket  :: socket(),
      Data    :: iodata(),
      Flags   :: send_flags(),
      Timeout :: timeout(),
      Reason  :: term().

send(Socket, Data, Flags, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    send(Socket, Bin, Flags, Timeout);
send(#socket{ref = SockRef}, Data, Flags, Timeout)
  when is_binary(Data) andalso is_list(Flags)  ->
    EFlags = enc_send_flags(Flags),
    do_send(SockRef, Data, EFlags, Timeout).

do_send(SockRef, Data, EFlags, Timeout) ->
    TS      = timestamp(Timeout),
    SendRef = make_ref(),
    case nif_send(SockRef, SendRef, Data, EFlags) of
        ok ->
            ok;
        {ok, Written} ->
	    NewTimeout = next_timeout(TS, Timeout),
	    %% We are partially done, wait for continuation
            receive
                {select, SockRef, SendRef, ready_output} when (Written > 0) ->
                    <<_:Written/binary, Rest/binary>> = Data,
                    do_send(SockRef, Rest, EFlags,
                            next_timeout(TS, Timeout));
                {select, SockRef, SendRef, ready_output} ->
                    do_send(SockRef, Data, EFlags,
                            next_timeout(TS, Timeout));

                {nif_abort, SendRef, Reason} ->
                    {error, Reason}

            after NewTimeout ->
                    nif_cancel(SockRef, send, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, {timeout, size(Data)}}
            end;
        {error, eagain} ->
            receive
                {select, SockRef, SendRef, ready_output} ->
                    do_send(SockRef, Data, EFlags,
                            next_timeout(TS, Timeout));

                {nif_abort, SendRef, Reason} ->
                    {error, Reason}

            after Timeout ->
                    nif_cancel(SockRef, send, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, {timeout, size(Data)}}
            end;

        {error, _} = ERROR ->
            ERROR
    end.




%% ---------------------------------------------------------------------------
%%

sendto(Socket, Data, Flags, DestSockAddr) ->
    sendto(Socket, Data, Flags, DestSockAddr, ?SOCKET_SENDTO_TIMEOUT_DEFAULT).

-spec sendto(Socket, Data, Flags, DestSockAddr, Timeout) ->
                    ok | {error, Reason} when
      Socket       :: socket(),
      Data         :: binary(),
      Flags        :: send_flags(),
      DestSockAddr :: null | in_sockaddr(),
      Timeout      :: timeout(),
      Reason       :: term().

sendto(Socket, Data, Flags, DestSockAddr, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    sendto(Socket, Bin, Flags, DestSockAddr, Timeout);
sendto(#socket{ref = SockRef}, Data, Flags, DestSockAddr, Timeout)
  when is_binary(Data) andalso
       is_list(Flags) andalso
       (is_record(DestSockAddr, in4_sockaddr) orelse 
        is_record(DestSockAddr, in6_sockaddr) orelse
        (DestSockAddr =:= null)) andalso
       (is_integer(Timeout) orelse (Timeout =:= infinity)) ->
    EFlags = enc_send_flags(Flags),
    do_sendto(SockRef, Data, EFlags, DestSockAddr, Timeout).

do_sendto(SockRef, Data, EFlags, DestSockAddr, Timeout) ->
    TS      = timestamp(Timeout),
    SendRef = make_ref(),
    case nif_sendto(SockRef, SendRef, Data, EFlags, DestSockAddr) of
        ok ->
            %% We are done
            ok;

        {ok, Written} ->
	    %% We are partially done, wait for continuation
            receive
                {select, SockRef, SendRef, ready_output} when (Written > 0) ->
                    <<_:Written/binary, Rest/binary>> = Data,
                    do_sendto(SockRef, Rest, EFlags, DestSockAddr,
                              next_timeout(TS, Timeout));
                {select, SockRef, SendRef, ready_output} ->
                    do_sendto(SockRef, Data, EFlags, DestSockAddr,
                              next_timeout(TS, Timeout));

                {nif_abort, SendRef, Reason} ->
                    {error, Reason}

            after Timeout ->
                    nif_cancel(SockRef, sendto, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, timeout}
            end;

        {error, eagain} ->
            receive
                {select, SockRef, SendRef, ready_output} ->
                    do_sendto(SockRef, Data, EFlags, DestSockAddr,
                              next_timeout(TS, Timeout))
            after Timeout ->
                    nif_cancel(SockRef, sendto, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, timeout}
            end;

        {error, _} = ERROR ->
            ERROR
    end.



%% ---------------------------------------------------------------------------

%% -spec sendmsg(Socket, MsgHdr, Flags) -> ok | {error, Reason} when
%%       Socket   :: socket(),
%%       MsgHdr   :: msg_header(),
%%       Flags    :: send_flags(),
%%       Reason   :: term().



%% ===========================================================================
%%
%% writev - write data into multiple buffers
%%

%% send(Socket, Data, Flags, Timeout)
%%   when (is_list(Data) orelse is_binary(Data)) andalso is_list(Flags)  ->
%%     IOVec  = erlang:iolist_to_iovec(Data),
%%     EFlags = enc_send_flags(Flags),
%%     send_iovec(Socket, IOVec, EFlags, Timeout).


%% %% Iterate over the IO-vector (list of binaries).

%% send_iovec(_Socket, [] = _IOVec, _EFlags, _Timeout) ->
%%     ok;
%% send_iovec({socket, _, SockRef} = Socket, [Bin|IOVec], EFlags, Timeout) ->
%%     case do_send(SockRef, make_ref(), Bin, EFlags, Timeout) of
%%         {ok, NewTimeout} ->
%%             send_iovec(Socket, IOVec, EFlags, NewTimeout);
%%         {error, _} = ERROR ->
%%             ERROR
%%     end.


%% do_send(SockRef, SendRef, Data, _EFlags, Timeout)
%%   when (Timeout < 0) ->
%%     nif_cancel(SockRef, SendRef),
%%     flush_select_msgs(SockRef, SendRef),
%%     {error, {timeout, size(Data)}};
%% do_send(SockRef, SendRef, Data, EFlags, Timeout) ->
%%     TS = timestamp(Timeout),
%%     case nif_send(SockRef, SendRef, Data, EFlags) of
%%         ok ->
%%             {ok, next_timeout(TS, Timeout)};
%%         {ok, Written} ->
%% 	    %% We are partially done, wait for continuation
%%             receive
%%                 {select, SockRef, SendRef, ready_output} ->
%%                     <<_:Written/binary, Rest/binary>> = Data,
%%                     do_send(SockRef, make_ref(), Rest, EFlags,
%%                             next_timeout(TS, Timeout))
%%             after Timeout ->
%%                     nif_cancel(SockRef, SendRef),
%%                     flush_select_msgs(SockRef, SendRef),
%%                     {error, timeout}
%%             end;
%%         {error, eagain} ->
%%             receive
%%                 {select, SockRef, SendRef, ready_output} ->
%%                     do_send(SockRef, SendRef, Data, EFlags,
%%                             next_timeout(TS, Timeout))
%%             after Timeout ->
%%                     nif_cancel(SockRef, SendRef),
%%                     flush_select_msgs(SockRef, SendRef),
%%                     {error, timeout}
%%             end;

%%         {error, _} = ERROR ->
%%             ERROR
%%     end.


%% ===========================================================================
%%
%% recv, recvfrom, recvmsg - receive a message from a socket
%%
%% Description:
%% There is a special case for the argument Length. If its set to zero (0),
%% it means "give me everything you have".
%%
%% Returns: {ok, Binary} | {error, Reason}
%% Binary  - The received data as a binary
%% Reason  - The error reason:
%%              timeout | {timeout, AccData} |
%%              posix() | {posix(), AccData} |
%%              atom()  | {atom(), AccData}
%% AccData - The data (as a binary) that we did manage to receive
%%           before the timeout.
%%
%% Arguments:
%% Socket  - The socket to read from.
%% Length  - The number of bytes to read.
%% Flags   - A list of "options" for the read.
%% Timeout - Time-out in milliseconds.

recv(Socket, Length) ->
    recv(Socket, Length,
         ?SOCKET_RECV_FLAGS_DEFAULT,
         ?SOCKET_RECV_TIMEOUT_DEFAULT).

recv(Socket, Length, Flags) when is_list(Flags) ->
    recv(Socket, Length, Flags, ?SOCKET_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length, Timeout) ->
    recv(Socket, Length, ?SOCKET_RECV_FLAGS_DEFAULT, Timeout).

-spec recv(Socket, Length, Flags, Timeout) -> {ok, Data} | {error, Reason} when
      Socket  :: socket(),
      Length  :: non_neg_integer(),
      Flags   :: recv_flags(),
      Timeout :: timeout(),
      Data    :: binary(),
      Reason  :: term().

recv(#socket{ref = SockRef}, Length, Flags, Timeout)
  when (is_integer(Length) andalso (Length >= 0)) andalso
       is_list(Flags) andalso
       (is_integer(Timeout) orelse (Timeout =:= infinity)) ->
    EFlags = enc_recv_flags(Flags),
    do_recv(SockRef, undefined, Length, EFlags, <<>>, Timeout).

%% We need to pass the "old recv ref" around because of the special case
%% with Length = 0. This case makes it neccessary to have a timeout function
%% clause since we may never wait for anything (no receive select), and so the
%% the only timeout check will be the function clause.
do_recv(SockRef, _OldRef, Length, EFlags, Acc, Timeout)
  when (Timeout =:= infinity) orelse
       (is_integer(Timeout) andalso (Timeout > 0)) ->
    TS      = timestamp(Timeout),
    RecvRef = make_ref(),
    case nif_recv(SockRef, RecvRef, Length, EFlags) of
        {ok, true = _Complete, Bin} when (size(Acc) =:= 0) ->
            {ok, Bin};
        {ok, true = _Complete, Bin} ->
            {ok, <<Acc/binary, Bin/binary>>};

        %% It depends on the amount of bytes we tried to read:
        %%    0   - Read everything available
        %%          We got something, but there may be more - keep reading.
        %%    > 0 - We got a part of the message and we will be notified
        %%          when there is more to read (a select message)
        {ok, false = _Complete, Bin} when (Length =:= 0) ->
            do_recv(SockRef, RecvRef,
                    Length, EFlags,
                    <<Acc/binary, Bin/binary>>,
                    next_timeout(TS, Timeout));

        {ok, false = _Completed, Bin} when (size(Acc) =:= 0) ->
            %% We got the first chunk of it.
            %% We will be notified (select message) when there
            %% is more to read.
	    NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recv(SockRef, RecvRef,
                            Length-size(Bin), EFlags,
                            Bin,
                            next_timeout(TS, Timeout));

                {nif_abort, RecvRef, Reason} ->
                    {error, Reason}


            after NewTimeout ->
                    nif_cancel(SockRef, recv, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, {timeout, Acc}}
            end;

        {ok, false = _Completed, Bin} ->
            %% We got a chunk of it!
	    NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recv(SockRef, RecvRef,
                            Length-size(Bin), EFlags,
                            <<Acc/binary, Bin/binary>>,
                            next_timeout(TS, Timeout));

                {nif_abort, RecvRef, Reason} ->
                    {error, Reason}


            after NewTimeout ->
                    nif_cancel(SockRef, recv, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, {timeout, Acc}}
            end;

        %% We return with the accumulated binary regardless if its empty...
        {error, eagain} when (Length =:= 0) ->
            {ok, Acc};

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recv(SockRef, RecvRef,
                            Length, EFlags,
                            Acc,
                            next_timeout(TS, Timeout));

                {nif_abort, RecvRef, Reason} ->
                    {error, Reason}

            after NewTimeout ->
                    nif_cancel(SockRef, recv, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, timeout}
            end;

        {error, _} = ERROR when (size(Acc) =:= 0) ->
            ERROR;

        {error, Reason} ->
            {error, {Reason, Acc}}

    end;

do_recv(SockRef, RecvRef, 0 = _Length, _Eflags, Acc, _Timeout) ->
    %% The current recv operation is to be cancelled, so no need for a ref...
    %% The cancel will end our 'read everything you have' and "activate"
    %% any waiting reader.
    nif_cancel(SockRef, recv, RecvRef),
    {ok, Acc};
do_recv(_SockRef, _RecvRef, _Length, _EFlags, Acc, _Timeout) when (size(Acc) > 0) ->
    {error, {timeout, Acc}};
do_recv(_SockRef, _RecvRef, _Length, _EFlags, _Acc, _Timeout) ->
    {error, timeout}.



%% ---------------------------------------------------------------------------
%%
%% With recvfrom we get messages, which means that regardless of how
%% much we want to read, we return when we get a message.
%% The MaxSize argument basically defines the size of our receive
%% buffer. By setting the size to zero (0), we use the configured
%% size (see setopt).
%% It may be impossible to know what (buffer) size is appropriate
%% "in advance", and in those cases it may be convenient to use the
%% (recv) 'peek' flag. When this flag is provided the message is *not*
%% "consumed" from the underlying buffers, so another recvfrom call
%% is needed, possibly with a then adjusted buffer size.
%%

recvfrom(Socket) ->
    recvfrom(Socket, 0).

recvfrom(Socket, BufSz) ->
    recvfrom(Socket, BufSz,
             ?SOCKET_RECV_FLAGS_DEFAULT,
             ?SOCKET_RECV_TIMEOUT_DEFAULT).


recvfrom(Socket, Flags, Timeout) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, Timeout);
recvfrom(Socket, BufSz, Flags) when is_list(Flags) ->
    recvfrom(Socket, BufSz, Flags, ?SOCKET_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz, Timeout) ->
    recvfrom(Socket, BufSz, ?SOCKET_RECV_FLAGS_DEFAULT, Timeout).

-spec recvfrom(Socket, BufSz, Flags, Timeout) -> {ok, {Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Flags     :: recv_flags(),
      Timeout   :: timeout(),
      Source    :: in_sockaddr() | string() | undefined,
      Data      :: binary(),
      Reason    :: term().

recvfrom(#socket{ref = SockRef}, BufSz, Flags, Timeout)
  when (is_integer(BufSz) andalso (BufSz >= 0)) andalso
       is_list(Flags) andalso
       (is_integer(Timeout) orelse (Timeout =:= infinity)) ->
    EFlags = enc_recv_flags(Flags),
    do_recvfrom(SockRef, BufSz, EFlags, Timeout).

do_recvfrom(SockRef, BufSz, EFlags, Timeout)  ->
    TS      = timestamp(Timeout),
    RecvRef = make_ref(),
    case nif_recvfrom(SockRef, RecvRef, BufSz, EFlags) of
        {ok, {_Source, _NewData}} = OK ->
            OK;

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recvfrom(SockRef, BufSz, EFlags,
                                next_timeout(TS, Timeout));

                {nif_abort, RecvRef, Reason} ->
                    {error, Reason}

            after NewTimeout ->
                    nif_cancel(SockRef, recvfrom, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, timeout}
            end;

        {error, _} = ERROR ->
            ERROR

    end.



%% ---------------------------------------------------------------------------
%%

%% -spec recvmsg(Socket, [out] MsgHdr, Flags) -> {ok, Data} | {error, Reason} when
%%       Socket :: socket(),
%%       MsgHdr :: msg_header(),
%%       Flags  :: recv_flags(),
%%       Data   :: binary(),
%%       Reason :: term().



%% ===========================================================================
%%
%% readv - read data into multiple buffers
%%



%% ===========================================================================
%%
%% close - close a file descriptor
%%

-spec close(Socket) -> ok | {error, Reason} when
      Socket :: socket(),
      Reason :: term().

close(#socket{ref = SockRef}) ->
    case nif_close(SockRef) of
        ok ->
            nif_finalize_close(SockRef);
        {ok, CloseRef} ->
            %% We must wait
            receive
		{close, CloseRef} ->
                    %% <KOLLA>
                    %%
                    %% WHAT HAPPENS IF THIS PROCESS IS KILLED
                    %% BEFORE WE CAN EXECUTE THE FINAL CLOSE???
                    %%
                    %% </KOLLA>
                    nif_finalize_close(SockRef)
            end;
        {error, _} = ERROR ->
            ERROR
    end.




%% ===========================================================================
%%
%% shutdown - shut down part of a full-duplex connection
%%

-spec shutdown(Socket, How) -> ok | {error, Reason} when
      Socket :: socket(),
      How    :: shutdown_how(),
      Reason :: term().

shutdown(#socket{ref = SockRef}, How) ->
    try
        begin
            EHow = enc_shutdown_how(How),
            nif_shutdown(SockRef, EHow)
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason}
    end.




%% ===========================================================================
%%
%% setopt - manipulate individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol)
%% If its an "invalid" option (or value), we should not crash but return some
%% useful error...
%%
%% <KOLLA>
%%
%% WE NEED TO MAKE SURE THAT THE USER DOES NOT MAKE US BLOCKING
%% AS MUCH OF THE CODE EXPECTS TO BE NON-BLOCKING!!
%%
%% </KOLLA>

-spec setopt(Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: otp,
      Opt    :: otp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: socket,
      Opt    :: socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: ip,
      Opt    :: ip_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: ipv6,
      Opt    :: ipv6_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: tcp,
      Opt    :: tcp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: udp,
      Opt    :: udp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Opt, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: sctp,
      Opt    :: sctp_socket_option(),
      Value  :: term(),
      Reason :: term().

setopt(#socket{info = Info, ref = SockRef}, Level, Key, Value) ->
    try
        begin
            Domain               = maps:get(domain, Info),
            Type                 = maps:get(type, Info),
            Protocol             = maps:get(protocol, Info),
            {EIsEncoded, ELevel} = enc_setopt_level(Level),
            EKey = enc_setopt_key(Level, Key, Domain, Type, Protocol),
            EVal = enc_setopt_value(Level, Key, Value, Domain, Type, Protocol),
            nif_setopt(SockRef, EIsEncoded, ELevel, EKey, EVal)
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason} % Process more?
    end.



%% ===========================================================================
%%
%% getopt - retrieve individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol).
%% If its an "invalid" option, we should not crash but return some
%% useful error...
%%
%% When specifying level as an integer, and therefor using "native mode",
%% we should make it possible to specify common types instead of the
%% value size. Example: int | bool | {string, pos_integer()} | non_neg_integer()
%%

-spec getopt(Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: otp,
      Key    :: otp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: socket,
      Key    :: socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: ip,
      Key    :: ip_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: ipv6,
      Key    :: ipv6_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: tcp,
      Key    :: tcp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: udp,
      Key    :: udp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: sctp,
      Key    :: sctp_socket_option(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key) -> ok | {ok, Value} | {error, Reason} when
      Socket    :: socket(),
      Level     :: integer(),
      Key       :: {NativeOpt, ValueSize},
      NativeOpt :: integer(),
      ValueSize :: int | bool | non_neg_integer(),
      Value     :: term(),
      Reason    :: term().

getopt(#socket{info = Info, ref = SockRef}, Level, Key) ->
    try
        begin
            Domain   = maps:get(domain, Info),
            Type     = maps:get(type, Info),
            Protocol = maps:get(protocol, Info),
            {EIsEncoded, ELevel} = enc_getopt_level(Level),
            EKey     = enc_getopt_key(Level, Key, Domain, Type, Protocol),
            %% We may need to decode the value (for the same reason
            %% we (may have) needed to encode the value for setopt).
            case nif_getopt(SockRef, EIsEncoded, ELevel, EKey) of
                ok ->
                    ok;
                {ok, EVal} ->
                    Val = dec_getopt_value(Level, Key, EVal,
                                           Domain, Type, Protocol),
                    {ok, Val};
                {error, _} = ERROR ->
                    ERROR
            end
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason} % Process more?
    end.



%% ===========================================================================
%%
%% link_if2idx - Mappings between network interface names and indexes: if -> idx
%%
%%

-spec link_if2idx(If) -> {ok, Idx} | {error, Reason} when
      If     :: string(),
      Idx    :: non_neg_integer(),
      Reason :: term().

link_if2idx(If) when is_list(If) ->
    nif_link_if2idx(If).


%% ===========================================================================
%%
%% link_idx2if - Mappings between network interface names and indexes: idx -> if
%%
%%

-spec link_idx2if(Idx) -> {ok, If} | {error, Reason} when
      Idx    :: non_neg_integer(),
      If     :: string(),
      Reason :: term().

link_idx2if(Idx) when is_integer(Idx) ->
    nif_link_idx2if(Idx).



%% ===========================================================================
%%
%% link_ifs - get network interface names and indexes
%%
%%

-spec link_ifs() -> Names | {error, Reason} when
      Names  :: [{Idx, If}],
      Idx    :: non_neg_integer(),
      If     :: string(),
      Reason :: term().

link_ifs() ->
    nif_link_ifs().



%% ===========================================================================
%%
%% Encode / decode
%%
%% ===========================================================================

-spec enc_domain(Domain) -> non_neg_integer() when
      Domain :: domain().

enc_domain(local)  -> ?SOCKET_DOMAIN_LOCAL;
enc_domain(inet)   -> ?SOCKET_DOMAIN_INET;
enc_domain(inet6)  -> ?SOCKET_DOMAIN_INET6;
enc_domain(Domain) -> throw({error, {invalid_domain, Domain}}).

-spec enc_type(Domain, Type) -> non_neg_integer() when
      Domain :: domain(),
      Type   :: type().

%% What combos are valid?
enc_type(_, stream)    -> ?SOCKET_TYPE_STREAM;
enc_type(_, dgram)     -> ?SOCKET_TYPE_DGRAM;
enc_type(_, raw)       -> ?SOCKET_TYPE_RAW;
enc_type(_, seqpacket) -> ?SOCKET_TYPE_SEQPACKET;
enc_type(_, Type)      -> throw({error, {invalid_type, Type}}).

-spec enc_protocol(Type, Protocol) -> non_neg_integer() when
      Type     :: type(),
      Protocol :: protocol().

enc_protocol(dgram,     ip)   -> ?SOCKET_PROTOCOL_IP;
enc_protocol(stream,    tcp)  -> ?SOCKET_PROTOCOL_TCP;
enc_protocol(dgram,     udp)  -> ?SOCKET_PROTOCOL_UDP;
enc_protocol(seqpacket, sctp) -> ?SOCKET_PROTOCOL_SCTP;
enc_protocol(Type, Proto)     -> throw({error, {invalid_protocol, {Type, Proto}}}).


-spec enc_send_flags(Flags) -> non_neg_integer() when
      Flags :: send_flags().

enc_send_flags(Flags) ->
    EFlags = [{confirm,   ?SOCKET_SEND_FLAG_CONFIRM},
              {dontroute, ?SOCKET_SEND_FLAG_DONTROUTE},
              {eor,       ?SOCKET_SEND_FLAG_EOR},
              {more,      ?SOCKET_SEND_FLAG_MORE},
              {nosignal,  ?SOCKET_SEND_FLAG_NOSIGNAL},
              {oob,       ?SOCKET_SEND_FLAG_OOB}],
    enc_flags(Flags, EFlags).

-spec enc_recv_flags(Flags) -> non_neg_integer() when
      Flags :: recv_flags().

enc_recv_flags(Flags) ->
    EFlags = [{cmsg_cloexec, ?SOCKET_RECV_FLAG_CMSG_CLOEXEC},
              {errqueue,     ?SOCKET_RECV_FLAG_ERRQUEUE},
              {oob,          ?SOCKET_RECV_FLAG_OOB},
              {peek,         ?SOCKET_RECV_FLAG_PEEK},
              {trunc,        ?SOCKET_RECV_FLAG_TRUNC}],
    enc_flags(Flags, EFlags).


enc_flags([], _) ->
    0;
enc_flags(Flags, EFlags) ->
    F = fun(Flag, Acc) ->
                case lists:keysearch(Flag, 1, EFlags) of
                    {value, {Flag, EFlag}} ->
                        Acc bor (1 bsl EFlag);
                    false ->
                        throw({error, {unknown_flag, Flag}})
                end
        end,
    lists:foldl(F, 0, Flags).


%% +++ Encode setopt level +++

-spec enc_setopt_level(Level) -> {IsEncoded, EncodedLevel} when
      Level        :: sockopt_level(),
      IsEncoded    :: boolean(),
      EncodedLevel :: integer().

enc_setopt_level(otp) ->
    {true, ?SOCKET_OPT_LEVEL_OTP};
enc_setopt_level(socket) ->
    {true, ?SOCKET_OPT_LEVEL_SOCKET};
enc_setopt_level(ip) ->
    {true, ?SOCKET_OPT_LEVEL_IP};
enc_setopt_level(ipv6) ->
    {true, ?SOCKET_OPT_LEVEL_IPV6};
enc_setopt_level(tcp) ->
    {true, ?SOCKET_OPT_LEVEL_TCP};
enc_setopt_level(udp) ->
    {true, ?SOCKET_OPT_LEVEL_UDP};
%% Any option that is of an plain level must be provided as a binary
%% already fully encoded!
enc_setopt_level(L) when is_integer(L) ->
    {false, L}.


%% +++ Encode setopt key +++

%% We should ...really... do something with the domain, type and protocol args...
%% Also, any option (key) which has an integer level (plain) must also be provided
%% in a plain mode, that is, as an integer.
%% Also, not all options are available on all platforms. That is something we
%% don't check here, but in the nif-code.

enc_setopt_key(Level, Opt, Domain, Type, Protocol) ->
    enc_sockopt_key(Level, Opt, set, Domain, Type, Protocol).


%% +++ Encode setopt value +++
%%
%% For the most part this function does *not* do an actual encode,
%% it simply validates the value type. But in some cases it will
%% encode the value into an more "manageable" type.
%% It also handles "aliases" (see linger).

enc_setopt_value(otp, debug, V, _, _, _) when is_boolean(V) ->
    V;
enc_setopt_value(otp, iow, V, _, _, _) when is_boolean(V) ->
    V;
enc_setopt_value(otp = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(socket, broadcast, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, keepalive, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, linger, abort, D, T, P) ->
    enc_setopt_value(socket, linger, {true, 0}, D, T, P);
enc_setopt_value(socket, linger, {OnOff, Secs} = V, _D, _T, _P)
  when is_boolean(OnOff) andalso is_integer(Secs) andalso (Secs >= 0) ->
    V;
enc_setopt_value(socket, priority, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, rcvbuf, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, reuseaddr, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, sndbuf, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(ip, recvtos, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, router_alert, V, _D, _T, _P)
  when is_integer(V) ->
    V;
enc_setopt_value(ip, tos, V, _D, _T, _P)
  when (V =:= lowdelay) orelse
       (V =:= throughput) orelse
       (V =:= reliability) orelse
       (V =:= mincost) orelse
       is_integer(V) ->
    V;
enc_setopt_value(ip, ttl, V, _D, _T, _P)
  when is_integer(V) ->
    V;
enc_setopt_value(ip = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(ipv6, hoplimit, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6 = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(tcp, congestion, V, _D, T, P)
  when is_list(V) andalso
       (T =:= stream) andalso
       (P =:= tcp) ->
    V;
enc_setopt_value(tcp, maxseg, V, _D, T, P)
  when is_integer(V) andalso
       (T =:= stream) andalso
       (P =:= tcp) ->
    V;
enc_setopt_value(tcp, nodelay, V, _D, T, P)
  when is_boolean(V) andalso
       (T =:= stream) andalso
       (P =:= tcp) ->
    V;
enc_setopt_value(tcp = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(udp, cork, V, _D, T, P)
  when is_boolean(V) andalso
       (T =:= dgram) andalso
       (P =:= udp) ->
    V;
enc_setopt_value(udp = L, Opt, _V, _D, _T, _P) ->
    not_supported({L, Opt});

enc_setopt_value(sctp, autoclose, V, _D, _T, P)
  when is_integer(V) andalso
       (P =:= sctp) ->
    V;
enc_setopt_value(sctp, nodelay, V, _D, _T, P)
  when is_boolean(V) andalso
       (P =:= sctp) ->
    V;

enc_setopt_value(L, Opt, V, _, _, _)
  when is_integer(L) andalso is_integer(Opt) andalso is_binary(V) ->
    V.




%% +++ Encode getopt value +++

enc_getopt_level(Level) ->
    enc_setopt_level(Level).


%% +++ Encode getopt key +++

enc_getopt_key(Level, Opt, Domain, Type, Protocol) ->
    enc_sockopt_key(Level, Opt, get, Domain, Type, Protocol).


%% +++ Decode getopt value +++
%%
%% For the most part, we simply let the value pass through, but for some
%% values we do an actual decode.
%%

%% Let the user deal with this...
dec_getopt_value(_L, _Opt, V, _D, _T, _P) ->
    V.



%% +++ Encode socket option key +++

%% Most options are usable both for set and get, but some are
%% are only available for e.g. get.
-spec enc_sockopt_key(Level, Opt,
                      Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: otp,
      Direction :: set | get,
      Opt       :: otp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: socket,
      Direction :: set | get,
      Opt       :: socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: ip,
      Direction :: set | get,
      Opt       :: ip_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: ipv6,
      Direction :: set | get,
      Opt       :: ipv6_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: tcp,
      Direction :: set | get,
      Opt       :: tcp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: udp,
      Direction :: set | get,
      Opt       :: udp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: sctp,
      Direction :: set | get,
      Opt       :: sctp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: integer(),
      Direction :: set,
      Opt       :: integer(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol()
                   ; (Level, Direction, Opt,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: integer(),
      Direction :: get,
      Opt       :: {NativeOpt, ValueSize},
      NativeOpt :: integer(),
      ValueSize :: non_neg_integer(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol().


%% +++ OTP socket options +++
enc_sockopt_key(otp, debug, _, _, _, _) ->
    ?SOCKET_OPT_OTP_DEBUG;
enc_sockopt_key(otp, iow, _, _, _, _) ->
    ?SOCKET_OPT_OTP_IOW;

%% +++ SOCKET socket options +++
enc_sockopt_key(socket, acceptconn = Opt, get = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, acceptfilter = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% Before linux 3.8, this socket option could be set.
%% Size of buffer for name: IFNAMSZ
%% So, we let the implementation decide.
enc_sockopt_key(socket, bindtodevide = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, broadcast = _Opt, _Dir, _D, dgram = _T, _P) ->
    ?SOCKET_OPT_SOCK_BROADCAST;
enc_sockopt_key(socket, busy_poll = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, debug = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, dontroute = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_DONTROUTE;
enc_sockopt_key(socket, error = Opt, get = _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% This is only for connection-oriented sockets, but who are those?
%% Type = stream or Protocol = tcp?
%% For now, we just let is pass and it will fail later if not ok...
enc_sockopt_key(socket, keepalive = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_KEEPALIVE;
enc_sockopt_key(socket, linger = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_LINGER;
enc_sockopt_key(socket, mark = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, oobinline = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, passcred = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, peek_off = Opt, _Dir, local = _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, peek_cred = Opt, get = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, priority = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_PRIORITY;
enc_sockopt_key(socket, rcvbuf = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_RCVBUF;
enc_sockopt_key(socket, rcvbufforce = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% May not work on linux.
enc_sockopt_key(socket, rcvlowat = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, rcvtimeo = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, reuseaddr = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_REUSEADDR;
enc_sockopt_key(socket, reuseport = Opt, _Dir, D, _T, _P)
  when ((D =:= inet) orelse (D =:= inet6)) ->
    not_supported(Opt);
enc_sockopt_key(socket, rxq_ovfl = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, setfib = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, sndbuf = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SOCK_SNDBUF;
enc_sockopt_key(socket, sndbufforce = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% Not changeable on linux.
enc_sockopt_key(socket, sndlowat = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, sndtimeo = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, timestamp = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(socket, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown(UnknownOpt);

%% +++ IP socket options +++
enc_sockopt_key(ip, add_membership = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, add_source_membership = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, block_source = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% FreeBSD only?
%% Only respected on udp and raw ip (unless the hdrincl option has been set).
enc_sockopt_key(ip, dontfrag = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, drop_membership = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, drop_source_membership = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% Linux only?
enc_sockopt_key(ip, free_bind = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, hdrincl = Opt, _Dir, _D, raw = _T, _P) ->
    not_supported(Opt);
%% FreeBSD only?
enc_sockopt_key(ip, minttl = Opt, _Dir, _D, raw = _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, msfilter = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, mtu = Opt, get = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, mtu_discover = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, multicast_all = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, multicast_if = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, multicast_loop = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, multicast_ttl = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, nodefrag = Opt, _Dir, _D, raw = _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, options = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, pktinfo = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
%% This require special code for accessing the errors.
%% via calling the recvmsg with the MSG_ERRQUEUE flag set,
enc_sockopt_key(ip, recverr = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, recvif = Opt, _Dir, _D, dgram = _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, recvdstaddr = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, recvopts = Opt, _Dir, _D, T, _P) when (T =/= stream) ->
    not_supported(Opt);
enc_sockopt_key(ip, recvorigdstaddr = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, recvtos = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_IP_RECVTOS;
enc_sockopt_key(ip, recvttl = Opt, _Dir, _D, T, _P) when (T =/= stream) ->
    not_supported(Opt);
enc_sockopt_key(ip, retopts = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, router_alert = _Opt, _Dir, _D, raw = _T, _P) ->
    ?SOCKET_OPT_IP_ROUTER_ALERT;
%% On FreeBSD it specifies that this option is only valid
%% for stream, dgram and "some" raw sockets...
%% No such condition on linux (in the man page)...
enc_sockopt_key(ip, tos = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_IP_TOS;
enc_sockopt_key(ip, transparent = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, ttl = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_IP_TTL;
enc_sockopt_key(ip, unblock_source = Opt, set = _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(ip, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown(UnknownOpt);

%% IPv6 socket options
enc_sockopt_key(ipv6, hoplimit = _Opt, _Dir, _D, T, _P)
  when (T =:= dgram) orelse (T =:= raw) ->
    ?SOCKET_OPT_IPV6_HOPLIMIT;
enc_sockopt_key(ipv6, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown(UnknownOpt);

%% TCP socket options
%% There are other options that would be useful; info,
%% but they are difficult to get portable...
enc_sockopt_key(tcp, congestion = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_TCP_CONGESTION;
enc_sockopt_key(tcp, cork = Opt, _Dir, _D, _T, _P) ->
    not_supported(Opt);
enc_sockopt_key(tcp, maxseg = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_TCP_MAXSEG;
enc_sockopt_key(tcp, nodelay = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_TCP_NODELAY;
enc_sockopt_key(tcp, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown(UnknownOpt);

%% UDP socket options
enc_sockopt_key(udp, cork = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_UDP_CORK;
enc_sockopt_key(udp, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown(UnknownOpt);

%% SCTP socket options
enc_sockopt_key(sctp, autoclose = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SCTP_AUTOCLOSE;
enc_sockopt_key(sctp, nodelay = _Opt, _Dir, _D, _T, _P) ->
    ?SOCKET_OPT_SCTP_NODELAY;
enc_sockopt_key(sctp, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown(UnknownOpt);

%% +++ "Native" socket options +++
enc_sockopt_key(Level, Opt, set = _Dir, _D, _T, _P)
  when is_integer(Level) andalso is_integer(Opt) ->
    Opt;
enc_sockopt_key(Level, {NativeOpt, ValueSize} = Opt, get = _Dir, _D, _T, _P)
  when is_integer(Level) andalso
       is_integer(NativeOpt) andalso
       ((is_integer(ValueSize) andalso (ValueSize >= 0)) orelse
        ((ValueSize =:= int) orelse (ValueSize =:= bool))) ->
    Opt;

enc_sockopt_key(Level, Opt, _Dir, _Domain, _Type, _Protocol) ->
    unknown({Level, Opt}).



enc_shutdown_how(read) ->
    ?SOCKET_SHUTDOWN_HOW_READ;
enc_shutdown_how(write) ->
    ?SOCKET_SHUTDOWN_HOW_WRITE;
enc_shutdown_how(read_write) ->
    ?SOCKET_SHUTDOWN_HOW_READ_WRITE.




%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

flush_select_msgs(LSRef, Ref) ->
    receive
        {select, LSRef, Ref, _} ->
            flush_select_msgs(LSRef, Ref)
    after 0 ->
            ok
    end.


%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(Now) ->
%%     N2T = fun(N) -> calendar:now_to_local_time(N) end,
%%     format_timestamp(Now, N2T, true).

%% format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
%%     FormatExtra = ".~.2.0w",
%%     ArgsExtra   = [N3 div 10000],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra);
%% format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
%%     FormatExtra = "",
%%     ArgsExtra   = [],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra).

%% format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
%%     {Date, Time}   = N2T(N),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate =
%%         io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
%%                       [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
%%     lists:flatten(FormatDate).


%% A timestamp in ms

timestamp(infinity) ->
    undefined;
timestamp(_) ->
    timestamp().

timestamp() ->
    {A,B,C} = os:timestamp(),
    A*1000000000+B*1000+(C div 1000).

next_timeout(_, infinity = Timeout) ->
    Timeout;
next_timeout(TS, Timeout) ->
    NewTimeout = Timeout - tdiff(TS, timestamp()),
    if
        (NewTimeout > 0) ->
            NewTimeout;
        true ->
            0
    end.

tdiff(T1, T2) ->
    T2 - T1.




%% ===========================================================================
%%
%% Error functions
%%
%% ===========================================================================

not_supported(What) ->
    error({not_supported, What}).

unknown(What) ->
    error({unknown, What}).

error(Reason) ->
    throw({error, Reason}).


%% ===========================================================================
%%
%% Below follows the actual NIF-functions.
%%
%% ===========================================================================

nif_is_loaded() -> false.

nif_info() ->
    erlang:error(badarg).

nif_open(_Domain, _Type, _Protocol, _Extra) ->
    erlang:error(badarg).

nif_bind(_SRef, _SockAddr) ->
    erlang:error(badarg).

nif_connect(_SRef, _SockAddr) ->
    erlang:error(badarg).

nif_finalize_connection(_SRef) ->
    erlang:error(badarg).

nif_listen(_SRef, _Backlog) ->
    erlang:error(badarg).

nif_accept(_SRef, _Ref) ->
    erlang:error(badarg).

nif_send(_SockRef, _SendRef, _Data, _Flags) ->
    erlang:error(badarg).

nif_sendto(_SRef, _SendRef, _Data, _Flags, _DestSockAddr) ->
    erlang:error(badarg).

nif_recv(_SRef, _RecvRef, _Length, _Flags) ->
    erlang:error(badarg).

nif_recvfrom(_SRef, _RecvRef, _Length, _Flags) ->
    erlang:error(badarg).

nif_cancel(_SRef, _Op, _Ref) ->
    erlang:error(badarg).

nif_close(_SRef) ->
    erlang:error(badarg).

nif_shutdown(_SRef, _How) ->
    erlang:error(badarg).

nif_finalize_close(_SRef) ->
    erlang:error(badarg).

nif_setopt(_Ref, _IsEnc, _Lev, _Key, _Val) ->
    erlang:error(badarg).

nif_getopt(_Ref, _IsEnc, _Lev, _Key) ->
    erlang:error(badarg).

nif_link_if2idx(_Name) ->
    erlang:error(badarg).

nif_link_idx2if(_Id) ->
    erlang:error(badarg).

nif_link_ifs() ->
    erlang:error(badarg).
