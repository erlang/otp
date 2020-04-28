%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2020. All Rights Reserved.
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

-module(prim_socket).

-compile(no_native).

-export([on_load/0, on_load/1]).

-export(
   [encode_path/1, encode_sockaddr/1,
    info/0, info/1,
    debug/1, socket_debug/1,
    supports/0, supports/1, supports/2,
    open/2, open/4,
    bind/2, bind/3,
    connect/1, connect/2,
    listen/2,
    accept/2,
    send/4, sendto/5, sendmsg/4,
    recv/4, recvfrom/4, recvmsg/5,
    close/1, finalize_close/1,
    shutdown/2, setopt/4, getopt/3,
    sockname/1, peername/1,
    cancel/3
   ]).

%% Also in socket
-define(REGISTRY, socket_registry).


%% ===========================================================================
%%
%% Defaults
%%

-define(ESOCK_SEND_FLAGS_DEFAULT,      []).
-define(ESOCK_SEND_TIMEOUT_DEFAULT,    infinity).
-define(ESOCK_SENDTO_FLAGS_DEFAULT,    []).
-define(ESOCK_SENDTO_TIMEOUT_DEFAULT,  ?ESOCK_SEND_TIMEOUT_DEFAULT).
-define(ESOCK_SENDMSG_FLAGS_DEFAULT,   []).
-define(ESOCK_SENDMSG_TIMEOUT_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).

-define(ESOCK_RECV_FLAGS_DEFAULT,   []).
-define(ESOCK_RECV_TIMEOUT_DEFAULT, infinity).

-define(ESOCK_SOCKADDR_IN4_DEFAULTS,
        (#{port => 0, addr => any})).
-define(ESOCK_SOCKADDR_IN6_DEFAULTS,
        (#{port => 0, addr => any,
           flowinfo => 0, scope_id => 0})).

%% ===========================================================================
%%
%% Constants common to prim_socket_nif.c - has to be "identical"
%%

-define(ESOCK_SEND_FLAG_CONFIRM,      (1 bsl 0)).
-define(ESOCK_SEND_FLAG_DONTROUTE,    (1 bsl 1)).
-define(ESOCK_SEND_FLAG_EOR,          (1 bsl 2)).
-define(ESOCK_SEND_FLAG_MORE,         (1 bsl 3)).
-define(ESOCK_SEND_FLAG_NOSIGNAL,     (1 bsl 4)).
-define(ESOCK_SEND_FLAG_OOB,          (1 bsl 5)).

-define(ESOCK_RECV_FLAG_CMSG_CLOEXEC, (1 bsl 0)).
-define(ESOCK_RECV_FLAG_ERRQUEUE,     (1 bsl 1)).
-define(ESOCK_RECV_FLAG_OOB,          (1 bsl 2)).
-define(ESOCK_RECV_FLAG_PEEK,         (1 bsl 3)).
-define(ESOCK_RECV_FLAG_TRUNC,        (1 bsl 4)).


%% shutdown/2
-define(ESOCK_SHUTDOWN_HOW_READ,       0).
-define(ESOCK_SHUTDOWN_HOW_WRITE,      1).
-define(ESOCK_SHUTDOWN_HOW_READ_WRITE, 2).



%% ----------------------------------
%% Address domain / protcol family

-define(ESOCK_DOMAIN_LOCAL, 1).
-define(ESOCK_DOMAIN_UNIX,  ?ESOCK_DOMAIN_LOCAL).
-define(ESOCK_DOMAIN_INET,  2).
-define(ESOCK_DOMAIN_INET6, 3).

%% ----------------------------------
%% Protocol type

-define(ESOCK_TYPE_STREAM,    101).
-define(ESOCK_TYPE_DGRAM,     102).
-define(ESOCK_TYPE_RAW,       103).
%% -define(ESOCK_TYPE_RDM,       104).
-define(ESOCK_TYPE_SEQPACKET, 105).

%% ----------------------------------
%% Protocol

-define(ESOCK_PROTOCOL_DEFAULT, 200).
-define(ESOCK_PROTOCOL_IP,      201).
-define(ESOCK_PROTOCOL_TCP,     202).
-define(ESOCK_PROTOCOL_UDP,     203).
-define(ESOCK_PROTOCOL_SCTP,    204).
-define(ESOCK_PROTOCOL_ICMP,    205).
-define(ESOCK_PROTOCOL_IGMP,    206).

%% ----------------------------------
%% Option level

-define(ESOCK_OPT_LEVEL_OTP,            301).
-define(ESOCK_OPT_LEVEL_SOCKET,         302).
-define(ESOCK_OPT_LEVEL_IP,             303).
-define(ESOCK_OPT_LEVEL_IPV6,           304).
-define(ESOCK_OPT_LEVEL_TCP,            305).
-define(ESOCK_OPT_LEVEL_UDP,            306).
-define(ESOCK_OPT_LEVEL_SCTP,           307).

%% ----------------------------------
%% *** OTP (socket) options

-define(ESOCK_OPT_OTP_DEBUG,           1001).
-define(ESOCK_OPT_OTP_IOW,             1002).
-define(ESOCK_OPT_OTP_CTRL_PROC,       1003).
-define(ESOCK_OPT_OTP_RCVBUF,          1004).
%%-define(ESOCK_OPT_OTP_SNDBUF,          1005).
-define(ESOCK_OPT_OTP_RCVCTRLBUF,      1006).
-define(ESOCK_OPT_OTP_SNDCTRLBUF,      1007).
-define(ESOCK_OPT_OTP_FD,              1008).
-define(ESOCK_OPT_OTP_META,            1009).
%%
-define(ESOCK_OPT_OTP_DOMAIN,          1999). % INTERNAL
%%-define(ESOCK_OPT_OTP_TYPE,            1998). % INTERNAL
%%-define(ESOCK_OPT_OTP_PROTOCOL,        1997). % INTERNAL
%%-define(ESOCK_OPT_OTP_DTP,             1996). % INTERNAL

%% ----------------------------------
%% *** SOCKET (socket) options

-define(ESOCK_OPT_SOCK_ACCEPTCONN,     2001).
-define(ESOCK_OPT_SOCK_ACCEPTFILTER,   2002). % FreeBSD
-define(ESOCK_OPT_SOCK_BINDTODEVICE,   2003).
-define(ESOCK_OPT_SOCK_BROADCAST,      2004).
-define(ESOCK_OPT_SOCK_BUSY_POLL,      2005).
-define(ESOCK_OPT_SOCK_DEBUG,          2006).
-define(ESOCK_OPT_SOCK_DOMAIN,         2007).
-define(ESOCK_OPT_SOCK_DONTROUTE,      2008).
-define(ESOCK_OPT_SOCK_ERROR,          2009).
-define(ESOCK_OPT_SOCK_KEEPALIVE,      2010).
-define(ESOCK_OPT_SOCK_LINGER,         2011).
-define(ESOCK_OPT_SOCK_MARK,           2012).
-define(ESOCK_OPT_SOCK_OOBINLINE,      2013).
-define(ESOCK_OPT_SOCK_PASSCRED,       2014).
-define(ESOCK_OPT_SOCK_PEEK_OFF,       2015).
-define(ESOCK_OPT_SOCK_PEERCRED,       2016).
-define(ESOCK_OPT_SOCK_PRIORITY,       2017).
-define(ESOCK_OPT_SOCK_PROTOCOL,       2018).
-define(ESOCK_OPT_SOCK_RCVBUF,         2019).
-define(ESOCK_OPT_SOCK_RCVBUFFORCE,    2020).
-define(ESOCK_OPT_SOCK_RCVLOWAT,       2021).
-define(ESOCK_OPT_SOCK_RCVTIMEO,       2022).
-define(ESOCK_OPT_SOCK_REUSEADDR,      2023).
-define(ESOCK_OPT_SOCK_REUSEPORT,      2024).
-define(ESOCK_OPT_SOCK_RXQ_OVFL,       2025).
-define(ESOCK_OPT_SOCK_SETFIB,         2026). % FreeBSD
-define(ESOCK_OPT_SOCK_SNDBUF,         2027).
-define(ESOCK_OPT_SOCK_SNDBUFFORCE,    2028).
-define(ESOCK_OPT_SOCK_SNDLOWAT,       2029).
-define(ESOCK_OPT_SOCK_SNDTIMEO,       2030).
-define(ESOCK_OPT_SOCK_TIMESTAMP,      2031).
-define(ESOCK_OPT_SOCK_TYPE,           2032).

%% ----------------------------------
%% *** IP (socket) options

-define(ESOCK_OPT_IP_ADD_MEMBERSHIP,        3001).
-define(ESOCK_OPT_IP_ADD_SOURCE_MEMBERSHIP, 3002).
-define(ESOCK_OPT_IP_BLOCK_SOURCE,          3003).
-define(ESOCK_OPT_IP_DONTFRAG,              3004). % FreeBSD
-define(ESOCK_OPT_IP_DROP_MEMBERSHIP,       3005).
-define(ESOCK_OPT_IP_DROP_SOURCE_MEMBERSHIP,3006).
-define(ESOCK_OPT_IP_FREEBIND,              3007).
-define(ESOCK_OPT_IP_HDRINCL,               3008).
-define(ESOCK_OPT_IP_MINTTL,                3009).
-define(ESOCK_OPT_IP_MSFILTER,              3010).
-define(ESOCK_OPT_IP_MTU,                   3011).
-define(ESOCK_OPT_IP_MTU_DISCOVER,          3012).
-define(ESOCK_OPT_IP_MULTICAST_ALL,         3013).
-define(ESOCK_OPT_IP_MULTICAST_IF,          3014).
-define(ESOCK_OPT_IP_MULTICAST_LOOP,        3015).
-define(ESOCK_OPT_IP_MULTICAST_TTL,         3016).
-define(ESOCK_OPT_IP_NODEFRAG,              3017).
-define(ESOCK_OPT_IP_OPTIONS,               3018). % FreeBSD
-define(ESOCK_OPT_IP_PKTINFO,               3019).
-define(ESOCK_OPT_IP_RECVDSTADDR,           3020). % FreeBSD
-define(ESOCK_OPT_IP_RECVERR,               3021).
-define(ESOCK_OPT_IP_RECVIF,                3022).
-define(ESOCK_OPT_IP_RECVOPTS,              3023).
-define(ESOCK_OPT_IP_RECVORIGDSTADDR,       3024).
-define(ESOCK_OPT_IP_RECVTOS,               3025).
-define(ESOCK_OPT_IP_RECVTTL,               3026).
-define(ESOCK_OPT_IP_RETOPTS,               3027).
-define(ESOCK_OPT_IP_ROUTER_ALERT,          3028).
-define(ESOCK_OPT_IP_SENDSRCADDR,           3029). % FreeBSD
-define(ESOCK_OPT_IP_TOS,                   3030).
-define(ESOCK_OPT_IP_TRANSPARENT,           3031).
-define(ESOCK_OPT_IP_TTL,                   3032).
-define(ESOCK_OPT_IP_UNBLOCK_SOURCE,        3033).

%% ----------------------------------
%% *** IPv6 (socket) options

-define(ESOCK_OPT_IPV6_ADDRFORM,          4001).
-define(ESOCK_OPT_IPV6_ADD_MEMBERSHIP,    4002).
-define(ESOCK_OPT_IPV6_AUTHHDR,           4003). % Obsolete?
-define(ESOCK_OPT_IPV6_AUTH_LEVEL,        4004). % FreeBSD
-define(ESOCK_OPT_IPV6_CHECKSUM,          4005). % FreeBSD
-define(ESOCK_OPT_IPV6_DROP_MEMBERSHIP,   4006).
-define(ESOCK_OPT_IPV6_DSTOPTS,           4007).
-define(ESOCK_OPT_IPV6_ESP_NETWORK_LEVEL, 4008). % FreeBSD
-define(ESOCK_OPT_IPV6_ESP_TRANS_LEVEL,   4009). % FreeBSD
-define(ESOCK_OPT_IPV6_FAITH,             4010). % FreeBSD
-define(ESOCK_OPT_IPV6_FLOWINFO,          4011).
-define(ESOCK_OPT_IPV6_HOPLIMIT,          4012).
-define(ESOCK_OPT_IPV6_HOPOPTS,           4013).
-define(ESOCK_OPT_IPV6_IPCOMP_LEVEL,      4014). % FreeBSD
-define(ESOCK_OPT_IPV6_JOIN_GROUP,        4015). % FreeBSD
-define(ESOCK_OPT_IPV6_LEAVE_GROUP,       4016). % FreeBSD
-define(ESOCK_OPT_IPV6_MTU,               4017).
-define(ESOCK_OPT_IPV6_MTU_DISCOVER,      4018).
-define(ESOCK_OPT_IPV6_MULTICAST_HOPS,    4019).
-define(ESOCK_OPT_IPV6_MULTICAST_IF,      4020).
-define(ESOCK_OPT_IPV6_MULTICAST_LOOP,    4021).
-define(ESOCK_OPT_IPV6_PORTRANGE,         4022). % FreeBSD
-define(ESOCK_OPT_IPV6_PKTOPTIONS,        4023). % FreeBSD
-define(ESOCK_OPT_IPV6_RECVERR,           4024).
-define(ESOCK_OPT_IPV6_RECVHOPLIMIT,      4025).
-define(ESOCK_OPT_IPV6_RECVPKTINFO,       4026). % On FreeBSD: PKTINFO
-define(ESOCK_OPT_IPV6_RECVTCLASS,        4027).
-define(ESOCK_OPT_IPV6_ROUTER_ALERT,      4028).
-define(ESOCK_OPT_IPV6_RTHDR,             4029).
-define(ESOCK_OPT_IPV6_TCLASS,            4030). % FreeBSD
-define(ESOCK_OPT_IPV6_UNICAST_HOPS,      4031).
-define(ESOCK_OPT_IPV6_USE_MIN_MTU,       4032). % FreeBSD
-define(ESOCK_OPT_IPV6_V6ONLY,            4033).

%% ----------------------------------
%% *** TCP (socket) options

-define(ESOCK_OPT_TCP_CONGESTION,     5001).
-define(ESOCK_OPT_TCP_CORK,           5002).
-define(ESOCK_OPT_TCP_INFO,           5003).
-define(ESOCK_OPT_TCP_KEEPCNT,        5004).
-define(ESOCK_OPT_TCP_KEEPIDLE,       5005).
-define(ESOCK_OPT_TCP_KEEPINTVL,      5006).
-define(ESOCK_OPT_TCP_MAXSEG,         5007).
-define(ESOCK_OPT_TCP_MD5SIG,         5008).
-define(ESOCK_OPT_TCP_NODELAY,        5009).
-define(ESOCK_OPT_TCP_NOOPT,          5010).
-define(ESOCK_OPT_TCP_NOPUSH,         5011).
-define(ESOCK_OPT_TCP_SYNCNT,         5012).
-define(ESOCK_OPT_TCP_USER_TIMEOUT,   5013).

%% ----------------------------------
%% *** UDP (socket) options

-define(ESOCK_OPT_UDP_CORK,           6001).

%% ----------------------------------
%% *** SCTP (socket) options

-define(ESOCK_OPT_SCTP_ADAPTION_LAYER,         7001).
-define(ESOCK_OPT_SCTP_ASSOCINFO,              7002).
-define(ESOCK_OPT_SCTP_AUTH_ACTIVE_KEY,        7003).
-define(ESOCK_OPT_SCTP_AUTH_ASCONF,            7004).
-define(ESOCK_OPT_SCTP_AUTH_CHUNK,             7005).
-define(ESOCK_OPT_SCTP_AUTH_KEY,               7006).
-define(ESOCK_OPT_SCTP_AUTH_DELETE_KEY,        7007).
-define(ESOCK_OPT_SCTP_AUTOCLOSE,              7008).
-define(ESOCK_OPT_SCTP_CONTEXT,                7009).
-define(ESOCK_OPT_SCTP_DEFAULT_SEND_PARAMS,    7010).
-define(ESOCK_OPT_SCTP_DELAYED_ACK_TIME,       7011).
-define(ESOCK_OPT_SCTP_DISABLE_FRAGMENTS,      7012).
-define(ESOCK_OPT_SCTP_HMAC_IDENT,             7013).
-define(ESOCK_OPT_SCTP_EVENTS,                 7014).
-define(ESOCK_OPT_SCTP_EXPLICIT_EOR,           7015).
-define(ESOCK_OPT_SCTP_FRAGMENT_INTERLEAVE,    7016).
-define(ESOCK_OPT_SCTP_GET_PEER_ADDR_INFO,     7017).
-define(ESOCK_OPT_SCTP_INITMSG,                7018).
-define(ESOCK_OPT_SCTP_I_WANT_MAPPED_V4_ADDR,  7019).
-define(ESOCK_OPT_SCTP_LOCAL_AUTH_CHUNKS,      7020).
-define(ESOCK_OPT_SCTP_MAXSEG,                 7021).
-define(ESOCK_OPT_SCTP_MAXBURST,               7022).
-define(ESOCK_OPT_SCTP_NODELAY,                7023).
-define(ESOCK_OPT_SCTP_PARTIAL_DELIVERY_POINT, 7024).
-define(ESOCK_OPT_SCTP_PEER_ADDR_PARAMS,       7025).
-define(ESOCK_OPT_SCTP_PEER_AUTH_CHUNKS,       7026).
-define(ESOCK_OPT_SCTP_PRIMARY_ADDR,           7027).
-define(ESOCK_OPT_SCTP_RESET_STREAMS,          7028).
-define(ESOCK_OPT_SCTP_RTOINFO,                7029).
-define(ESOCK_OPT_SCTP_SET_PEER_PRIMARY_ADDR,  7030).
-define(ESOCK_OPT_SCTP_STATUS,                 7031).
-define(ESOCK_OPT_SCTP_USE_EXT_RECVINFO,       7032).


%% ===========================================================================
%%
%% Guard macros
%%

%% Check that there are 1:s just in the lowest 8 bits of all 4 elements
-define(
   IS_IPV4_ADDR(A),
   (is_tuple(A) andalso tuple_size(A) =:= 4
    andalso
      ((element(1, (A)) bor element(2, (A))
            bor element(3, (A)) bor element(4, (A))
       ) band (bnot 16#FF)
      ) =:= 0)).

%% Check that there are 1:s just in the lowest 16 bits of all 8 elements
-define(
   IS_IPV6_ADDR(A),
   (is_tuple(A) andalso tuple_size(A) =:= 8
    andalso
      ((element(1, (A)) bor element(2, (A))
           bor element(3, (A)) bor element(4, (A))
           bor element(5, (A)) bor element(6, (A))
           bor element(7, (A)) bor element(8, (A))
       ) band (bnot 16#FFFF)
      ) =:= 0)).

%% ===========================================================================
%% API for 'erl_init'
%%

on_load() ->
    on_load(#{}).

on_load(Extra) when is_map(Extra) ->
    %% This is spawned as a system process to prevent init:restart/0 from
    %% killing it.
    Pid = erts_internal:spawn_system_process(?REGISTRY, start, []),
    DebugFilename =
        case os:get_env_var("ESOCK_DEBUG_FILENAME") of
            "*" ->
                "/tmp/esock-dbg-??????";
            F ->
                F
        end,
    ok  =
        erlang:load_nif(
          atom_to_list(?MODULE),
          case DebugFilename of
              false ->
                  Extra#{registry => Pid};
              _ ->
                  Extra
                      #{registry => Pid,
                        debug => true,
                        socket_debug => true,
                        debug_filename =>
                            encode_path(DebugFilename)}
          end).

%% ===========================================================================
%% API for 'socket'
%%

%% File names has to be encoded according to
%% the native file encoding
%%
encode_path(Path) ->
    %% These are all BIFs - will not cause code loading
    unicode:characters_to_binary(Path, file:native_name_encoding()).

encode_sockaddr(SockAddr) ->
    enc_sockaddr(SockAddr).

%% ----------------------------------

info() ->
    nif_info().

info(SockRef) ->
    nif_info(SockRef).

%% ----------------------------------

debug(D) ->
    nif_command(#{command => ?FUNCTION_NAME, data => D}).


socket_debug(D) ->
    nif_command(#{command => ?FUNCTION_NAME, data => D}).

%% ----------------------------------

supports() ->
    nif_supports().
     
supports(Key) ->
    nif_supports(Key).

supports(options = Key, Level) ->
    case Level of
        socket ->
            nif_supports(Key, ?ESOCK_OPT_LEVEL_SOCKET);
        ip ->
            nif_supports(Key, ?ESOCK_OPT_LEVEL_IP);
        ipv6 ->
            nif_supports(Key, ?ESOCK_OPT_LEVEL_IPV6);
        tcp ->
            nif_supports(Key, ?ESOCK_OPT_LEVEL_TCP);
        udp ->
            nif_supports(Key, ?ESOCK_OPT_LEVEL_UDP);
        sctp ->
            nif_supports(Key, ?ESOCK_OPT_LEVEL_SCTP);
        _ ->
            []
    end;
supports(_Key1, _Key2) ->
    [].

%% ----------------------------------

open(FD, Opts) when is_map(Opts) ->
    EOpts =
        maps:map(
          fun (Key, Val) ->
                  case Key of
                      domain ->
                          enc_domain(Val);
                      type ->
                          enc_type(Val);
                      protocol ->
                          enc_protocol(Val);
                      _ ->
                          Val
                  end
          end, Opts),
    nif_open(FD, EOpts).

open(Domain, Type, Protocol, Opts) when is_map(Opts) ->
    EDomain   = enc_domain(Domain),
    EType     = enc_type(Type),
    EProtocol = enc_protocol(Protocol),
    EOpts =
        case Opts of
            #{netns := Path} when is_list(Path) ->
                Opts#{netns := encode_path(Path)};
            _ ->
                Opts
        end,
    nif_open(EDomain, EType, EProtocol, EOpts).

%% ----------------------------------

bind(SockRef, Addr) ->
    nif_bind(SockRef, enc_sockaddr(Addr)).

bind(SockRef, Addrs, Action) when is_list(Addrs) ->
    EAddrs = [enc_sockaddr(Addr) || Addr <- Addrs],
    nif_bind(SockRef, EAddrs, Action).

%% ----------------------------------

connect(SockRef, SockAddr) ->
    nif_connect(SockRef, enc_sockaddr(SockAddr)).

connect(SockRef) ->
    nif_connect(SockRef).

%% ----------------------------------

listen(SockRef, Backlog) ->
    nif_listen(SockRef, Backlog).

%% ----------------------------------

accept(ListenSockRef, AccRef) ->
    case nif_accept(ListenSockRef, AccRef) of
        {ok, _SockRef} = Result ->
            Result;
        {error, eagain} ->
            select;
        {error, _} = Result ->
            Result
    end.

%% ----------------------------------

send(SockRef, SendRef, Data, Flags) ->
    EFlags = enc_send_flags(Flags),
    send_result(
      nif_send(SockRef, SendRef, Data, EFlags)).

sendto(SockRef, SendRef, Data, To, Flags) ->
    ETo = enc_sockaddr(To),
    EFlags = enc_send_flags(Flags),
    send_result(
      nif_sendto(SockRef, SendRef, Data, ETo, EFlags)).

sendmsg(SockRef, SendRef, MsgHdr, Flags) ->
    EMsgHdr = enc_msghdr(MsgHdr),
    EFlags = enc_send_flags(Flags),
    send_result(
      nif_sendmsg(SockRef, SendRef, EMsgHdr, EFlags)).

send_result(Result) ->
    case Result of
        ok -> ok;
        {ok, _Written} = OK -> OK;
        {error, eagain} -> select;
        {error, _} = ERROR -> ERROR
    end.

%% ----------------------------------

recv(SockRef, RecvRef, Length, Flags) ->
    EFlags = enc_recv_flags(Flags),
    case nif_recv(SockRef, RecvRef, Length, EFlags) of
        {ok, true, Bin} ->
            {ok, Bin};
        %%
        {ok, false, Bin} ->
            %% Depending on the number of bytes we tried to read:
            if
                Length =:= 0 ->
                    %% 0 - Read everything available
                    %% We got something, but there may be more
                    %% - keep reading.
                    {more, Bin};
                true ->
                    %% > 0 - We got a part of the message
                    %% and we will be notified when there is more to read
                    %% (a select message)
                    {select, Bin}
            end;
        %%
        {error, eagain} ->
            select;
        {error, _} = ERROR ->
            ERROR
    end.

recvfrom(SockRef, RecvRef, Length, Flags) ->
    EFlags = enc_recv_flags(Flags),
    recvfromsg_result(
      nif_recvfrom(SockRef, RecvRef, Length, EFlags)).

recvmsg(SockRef, RecvRef, BufSz, CtrlSz, Flags) ->
    EFlags = enc_recv_flags(Flags),
    recvfromsg_result(
      nif_recvmsg(SockRef, RecvRef, BufSz, CtrlSz, EFlags)).

recvfromsg_result(Result) ->
    case Result of
        {ok, _} = OK -> OK;
        {error, eagain} -> select;
        {error, _} = ERROR -> ERROR
    end.

%% ----------------------------------

close(SockRef) ->
    nif_close(SockRef).

finalize_close(SockRef) ->    
    nif_finalize_close(SockRef).

%% ----------------------------------

shutdown(SockRef, How) ->
    nif_shutdown(SockRef, enc_shutdown_how(How)).

%% ----------------------------------

setopt(SockRef, Level, Opt, Val)
  when is_integer(Level), is_integer(Opt), is_binary(Val) ->
    nif_setopt(SockRef, false, Level, Opt, Val);
setopt(SockRef, Level, Opt, Val)
  when is_integer(Opt), is_binary(Val) ->
    ELevel = enc_sockopt_type(Level, []),
    nif_setopt(SockRef, true, ELevel, Opt, Val);
setopt(SockRef, Level, Opt, Value) when Opt =/= [] ->
    case enc_sockopt_type(Level, Opt) of
        {undefined, _ELevel, _EOpt} ->
            {error, einval};
        {Type, ELevel, EOpt} ->
            EValue = enc_setopt_value(Level, Opt, Value, Type),
            nif_setopt(SockRef, true, ELevel, EOpt, EValue)
    end.

getopt(SockRef, Level, Opt)
  when is_integer(Level) ->
    nif_getopt(SockRef, false, Level, Opt);
getopt(SockRef, Level, Opt) when is_atom(Opt) ->
    {_Type, ELevel, EOpt} = enc_sockopt_type(Level, Opt),
    case nif_getopt(SockRef, true, ELevel, EOpt) of
        {ok, Value} ->
            {ok, dec_getopt_value(Value, Level, Opt)};
        {error, _} = Error ->
            Error
    end;
getopt(SockRef, Level, Opt) ->
    ELevel = enc_sockopt_type(Level, []),
    nif_getopt(SockRef, true, ELevel, Opt).

%% ----------------------------------

sockname(Ref) ->
    nif_sockname(Ref).

peername(Ref) ->
    nif_peername(Ref).

%% ----------------------------------

cancel(SRef, Op, Ref) ->
    nif_cancel(SRef, Op, Ref).

%% ===========================================================================
%% Encode / decode
%%

enc_domain(local)  -> ?ESOCK_DOMAIN_LOCAL;
enc_domain(inet)   -> ?ESOCK_DOMAIN_INET;
enc_domain(inet6)  -> ?ESOCK_DOMAIN_INET6;
enc_domain(Domain) -> invalid(domain, Domain).

enc_type(stream)    -> ?ESOCK_TYPE_STREAM;
enc_type(dgram)     -> ?ESOCK_TYPE_DGRAM;
enc_type(raw)       -> ?ESOCK_TYPE_RAW;
enc_type(seqpacket) -> ?ESOCK_TYPE_SEQPACKET;
enc_type(Type)      -> invalid(type, Type).

enc_protocol(default) -> ?ESOCK_PROTOCOL_DEFAULT;
enc_protocol(ip)      -> ?ESOCK_PROTOCOL_IP;
enc_protocol(tcp)     -> ?ESOCK_PROTOCOL_TCP;
enc_protocol(udp)     -> ?ESOCK_PROTOCOL_UDP;
enc_protocol(sctp)    -> ?ESOCK_PROTOCOL_SCTP;
enc_protocol(icmp)    -> ?ESOCK_PROTOCOL_ICMP;
enc_protocol(igmp)    -> ?ESOCK_PROTOCOL_IGMP;
enc_protocol({raw, P} = RAW) when is_integer(P) -> RAW;
enc_protocol(Proto) ->
    invalid(protocol, Proto).


enc_sockaddr(#{family := inet} = SockAddr) ->
    maps:merge(?ESOCK_SOCKADDR_IN4_DEFAULTS, SockAddr);
enc_sockaddr(#{family := inet6} = SockAddr) ->
    maps:merge(?ESOCK_SOCKADDR_IN6_DEFAULTS, SockAddr);
enc_sockaddr(#{family := local, path := Path} = SockAddr)
  when is_list(Path) andalso 
       (length(Path) > 0) andalso 
       (length(Path) =< 255) ->
    BinPath = encode_path(Path),
    enc_sockaddr(SockAddr#{path => BinPath});
enc_sockaddr(#{family := local, path := Path} = SockAddr) 
  when is_binary(Path) andalso 
       (byte_size(Path) > 0) andalso 
       (byte_size(Path) =< 255) ->
    SockAddr;
enc_sockaddr(SockAddr) ->
    invalid(address, SockAddr).


enc_send_flags([]) -> 0;
enc_send_flags([Flag | Flags]) ->
    case Flag of
        confirm ->   ?ESOCK_SEND_FLAG_CONFIRM;
        dontroute -> ?ESOCK_SEND_FLAG_DONTROUTE;
        eor ->       ?ESOCK_SEND_FLAG_EOR;
        more ->      ?ESOCK_SEND_FLAG_MORE;
        nosignal ->  ?ESOCK_SEND_FLAG_NOSIGNAL;
        oob ->       ?ESOCK_SEND_FLAG_OOB;
        _ -> invalid(flag, Flag)
    end bor enc_send_flags(Flags).


enc_recv_flags([]) -> 0;
enc_recv_flags([Flag | Flags]) ->
    case Flag of
        cmsg_cloexec -> ?ESOCK_RECV_FLAG_CMSG_CLOEXEC;
        errqueue ->     ?ESOCK_RECV_FLAG_ERRQUEUE;
        oob ->          ?ESOCK_RECV_FLAG_OOB;
        peek ->         ?ESOCK_RECV_FLAG_PEEK;
        trunc ->        ?ESOCK_RECV_FLAG_TRUNC;
        _ -> invalid(flag, Flag)
    end bor enc_recv_flags(Flags).
    

enc_msghdr(#{ctrl := []} = M) ->
    enc_msghdr(maps:remove(ctrl, M));
enc_msghdr(#{iov := IOV, addr := Addr} = M) 
  when is_list(IOV), IOV =/= [] ->
    M#{iov => erlang:iolist_to_iovec(IOV),
       addr => enc_sockaddr(Addr)};
enc_msghdr(#{iov := IOV} = M) 
  when is_list(IOV), IOV =/= [] ->
    M#{iov => erlang:iolist_to_iovec(IOV)};
enc_msghdr(#{} = M) ->
    not_supported(M).


%% Common to setopt and getopt
%% - the returned first tuple element is a type tag
%% that is used by setopt through enc_setopt_value/4
%% and ignored by getopt;
%% 'undefined' means that encoding for setopt/4
%% cannot be done and setopt/4 returns {error, einval}.
%%
%% Opt values not handled here will be passed to not_supported/1,2
%% which causes a runtime error.
%%
%% Values that fails encoding in enc_setopt_value/4
%% will be passed to not_supported/3.
%%
%% So, all combinations of Level and Opt enumerated here
%% will either return {error, einval} because they can not
%% be encoded for setopt/4, or they are passed to
%% the setopt or getopt NIF.  The NIF will return
%% {error, einval} for unknown Level and Opt.
%%
%% Therefore all Level and Opt enumerated here will produce
%% a return value from setopt/4 and getopt/3 and should
%% be in the type spec for socket:setopt() and socket:getopt().
%%
%% Opt =:= [] just encodes the Level
%%
enc_sockopt_type(otp = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_OTP,
    case Opt of
        [] -> L;
        %%
        debug ->        {boolean,       L, ?ESOCK_OPT_OTP_DEBUG};
        iow ->          {boolean,       L, ?ESOCK_OPT_OTP_IOW};
        controlling_process ->  {pid,   L, ?ESOCK_OPT_OTP_CTRL_PROC};
        rcvbuf ->       {Opt,           L, ?ESOCK_OPT_OTP_RCVBUF};
        rcvctrlbuf ->   {buf,           L, ?ESOCK_OPT_OTP_RCVCTRLBUF};
        sndctrlbuf ->   {buf,           L, ?ESOCK_OPT_OTP_SNDCTRLBUF};
        fd ->           {undefined,     L, ?ESOCK_OPT_OTP_FD};
        meta ->         {term,          L, ?ESOCK_OPT_OTP_META};
        domain ->       {undefined,     L, ?ESOCK_OPT_OTP_DOMAIN};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(socket = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_SOCKET,
    case Opt of
        [] -> L;
        %%
        acceptconn ->   {undefined,     L, ?ESOCK_OPT_SOCK_ACCEPTCONN};
        acceptfilter -> {undefined,     L, ?ESOCK_OPT_SOCK_ACCEPTFILTER};
        bindtodevice ->
            %% Before linux 3.8, this socket option
            %% could be set but not get.
            %% Maximum size of buffer for name: IFNAMSIZ
            %% So, we let the implementation decide.
            {list,                      L, ?ESOCK_OPT_SOCK_BINDTODEVICE};
        broadcast ->    {boolean,       L, ?ESOCK_OPT_SOCK_BROADCAST};
        busy_poll ->    {undefined,     L, ?ESOCK_OPT_SOCK_BUSY_POLL};
        debug ->        {integer,       L, ?ESOCK_OPT_SOCK_DEBUG};
        domain ->       {undefined,     L, ?ESOCK_OPT_SOCK_DOMAIN};
        dontroute ->    {boolean,       L, ?ESOCK_OPT_SOCK_DONTROUTE};
        error ->        {undefined,     L, ?ESOCK_OPT_SOCK_ERROR};
        keepalive ->    {boolean,       L, ?ESOCK_OPT_SOCK_KEEPALIVE};
        linger ->       {linger,        L, ?ESOCK_OPT_SOCK_LINGER};
        mark ->         {undefined,     L, ?ESOCK_OPT_SOCK_MARK};
        oobinline ->    {boolean,       L, ?ESOCK_OPT_SOCK_OOBINLINE};
        passcred ->     {boolean,       L, ?ESOCK_OPT_SOCK_PASSCRED};
        peek_off ->     {integer,       L, ?ESOCK_OPT_SOCK_PEEK_OFF};
        peercred ->     {undefined,     L, ?ESOCK_OPT_SOCK_PEERCRED};
        priority ->     {integer,       L, ?ESOCK_OPT_SOCK_PRIORITY};
        protocol ->     {undefined,     L, ?ESOCK_OPT_SOCK_PROTOCOL};
        rcvbuf ->       {integer,       L, ?ESOCK_OPT_SOCK_RCVBUF};
        rcvbufforce ->  {undefined,     L, ?ESOCK_OPT_SOCK_RCVBUFFORCE};
        rcvlowat ->
            %% May not work on Linux
            {integer,                   L, ?ESOCK_OPT_SOCK_RCVLOWAT};
        rcvtimeo ->     {timeval,       L, ?ESOCK_OPT_SOCK_RCVTIMEO};
        reuseaddr ->    {boolean,       L, ?ESOCK_OPT_SOCK_REUSEADDR};
        reuseport ->    {boolean,       L, ?ESOCK_OPT_SOCK_REUSEPORT};
        rxq_ovfl ->     {undefined,     L, ?ESOCK_OPT_SOCK_RXQ_OVFL};
        setfib ->       {undefined,     L, ?ESOCK_OPT_SOCK_SETFIB};
        sndbuf ->       {integer,       L, ?ESOCK_OPT_SOCK_SNDBUF};
        sndbufforce ->  {undefined,     L, ?ESOCK_OPT_SOCK_SNDBUFFORCE};
        sndlowat ->
            %% Not changeable on Linux
            {integer,                   L, ?ESOCK_OPT_SOCK_SNDLOWAT};
        sndtimeo ->     {timeval,       L, ?ESOCK_OPT_SOCK_SNDTIMEO};
        timestamp ->    {boolean,       L, ?ESOCK_OPT_SOCK_TIMESTAMP};
        type ->         {undefined,     L, ?ESOCK_OPT_SOCK_TYPE};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(ip = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_IP,
    case Opt of
        [] -> L;
        %%
        add_membership ->
            {addr_if,           L, ?ESOCK_OPT_IP_ADD_MEMBERSHIP};
        add_source_membership ->
            {addr_if_src,       L, ?ESOCK_OPT_IP_ADD_SOURCE_MEMBERSHIP};
        block_source ->
            {addr_if_src,       L, ?ESOCK_OPT_IP_BLOCK_SOURCE};
        dontfrag ->
            %% FreeBSD only?
            %% Only respected on udp and raw ip
            %% (unless the hdrincl option has been set)
            {boolean,           L, ?ESOCK_OPT_IP_DONTFRAG};
        drop_membership ->
            {addr_if,           L, ?ESOCK_OPT_IP_DROP_MEMBERSHIP};
        drop_source_membership ->
            {addr_if_src,       L, ?ESOCK_OPT_IP_DROP_SOURCE_MEMBERSHIP};
        freebind ->
            %% Linux only?
            {boolean,           L, ?ESOCK_OPT_IP_FREEBIND};
        hdrincl ->
            {boolean,           L, ?ESOCK_OPT_IP_HDRINCL};
        minttl ->
            {integer,           L, ?ESOCK_OPT_IP_MINTTL};
        msfilter ->     {Opt,   L, ?ESOCK_OPT_IP_MSFILTER};
        mtu ->          {undefined, L, ?ESOCK_OPT_IP_MTU};
        mtu_discover -> {Opt,   L, ?ESOCK_OPT_IP_MTU_DISCOVER};
        multicast_all ->
            {boolean,           L, ?ESOCK_OPT_IP_MULTICAST_ALL};
        multicast_if ->
            {boolean,           L, ?ESOCK_OPT_IP_MULTICAST_ALL};
        multicast_loop ->
            {boolean,           L, ?ESOCK_OPT_IP_MULTICAST_LOOP};
        multicast_ttl ->
            {byte,              L, ?ESOCK_OPT_IP_MULTICAST_TTL};
        nodefrag ->
            {boolean,           L, ?ESOCK_OPT_IP_NODEFRAG};
        options ->
            {undefined,         L, ?ESOCK_OPT_IP_OPTIONS};
        pktinfo ->
            {boolean,           L, ?ESOCK_OPT_IP_PKTINFO};
        recvdstaddr ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVDSTADDR};
        recverr ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVERR};
        recvif ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVIF};
        recvopts ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVOPTS};
        recvorigdstaddr ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVORIGDSTADDR};
        recvtos ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVTOS};
        recvttl ->
            {boolean,           L, ?ESOCK_OPT_IP_RECVTTL};
        retopts ->
            {boolean,           L, ?ESOCK_OPT_IP_RETOPTS};
        router_alert ->
            {integer,           L, ?ESOCK_OPT_IP_ROUTER_ALERT};
        sendsrcaddr ->
            {boolean,           L, ?ESOCK_OPT_IP_SENDSRCADDR};
        tos ->
            %% On FreeBSD it specifies that this option is only valid
            %% for stream, dgram and "some" raw sockets...
            %% No such condition on linux (in the man page)...
            {Opt,               L, ?ESOCK_OPT_IP_TOS};
        transparent ->
            {boolean,           L, ?ESOCK_OPT_IP_TRANSPARENT};
        ttl ->
            {integer,           L, ?ESOCK_OPT_IP_TTL};
        unblock_source ->
            {addr_if_src,       L, ?ESOCK_OPT_IP_UNBLOCK_SOURCE};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(ipv6 = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_IPV6,
    case Opt of
        [] -> L;
        %%
        addrform ->     {Opt,   L, ?ESOCK_OPT_IPV6_ADDRFORM};
        add_membership ->
            {addr_if,           L, ?ESOCK_OPT_IPV6_ADD_MEMBERSHIP};
        authhdr ->
            %% Is this obsolete? When get, the result is enoprotoopt
            %% and in the  header file it says 'obsolete'...
            %% But there might be (old?) versions of linux
            %% where it still works...
            {boolean,           L, ?ESOCK_OPT_IPV6_AUTHHDR};
        auth_level ->
            {undefined,         L, ?ESOCK_OPT_IPV6_AUTH_LEVEL};
        checksum ->
            {undefined,         L, ?ESOCK_OPT_IPV6_CHECKSUM};
        drop_membership ->
            {addr_if,           L, ?ESOCK_OPT_IPV6_DROP_MEMBERSHIP};
        dstopts ->
            {boolean,           L, ?ESOCK_OPT_IPV6_DSTOPTS};
        esp_network_level ->
            {undefined,         L, ?ESOCK_OPT_IPV6_ESP_NETWORK_LEVEL};
        esp_trans_level ->
            {undefined,         L, ?ESOCK_OPT_IPV6_ESP_TRANS_LEVEL};
        flowinfo ->
            {boolean,           L, ?ESOCK_OPT_IPV6_FLOWINFO};
        hoplimit ->
            {boolean,           L, ?ESOCK_OPT_IPV6_HOPLIMIT};
        hopopts ->
            {boolean,           L, ?ESOCK_OPT_IPV6_HOPOPTS};
        ipcomp_level ->
            {undefined,         L, ?ESOCK_OPT_IPV6_IPCOMP_LEVEL};
        join_group ->
            {undefined,         L, ?ESOCK_OPT_IPV6_JOIN_GROUP};
        leave_group ->
            {undefined,         L, ?ESOCK_OPT_IPV6_LEAVE_GROUP};
        mtu ->
            {integer,           L, ?ESOCK_OPT_IPV6_MTU};
        mtu_discover -> {Opt,   L, ?ESOCK_OPT_IPV6_MTU_DISCOVER};
        multicast_hops ->
            {hops,              L, ?ESOCK_OPT_IPV6_MULTICAST_HOPS};
        multicast_if ->
            {integer,           L, ?ESOCK_OPT_IPV6_MULTICAST_IF};
        multicast_loop ->
            {boolean,           L, ?ESOCK_OPT_IPV6_MULTICAST_LOOP};
        portrange ->
            {undefined,         L, ?ESOCK_OPT_IPV6_PORTRANGE};
        pktoptions ->
            {undefined,         L, ?ESOCK_OPT_IPV6_PKTOPTIONS};
        recverr ->
            {boolean,           L, ?ESOCK_OPT_IPV6_RECVERR};
        recvhoplimit ->
            {boolean,           L, ?ESOCK_OPT_IPV6_RECVHOPLIMIT};
        recvpktinfo ->
            {boolean,           L, ?ESOCK_OPT_IPV6_RECVPKTINFO};
        pktinfo -> % alias on FreeBSD
            {boolean,           L, ?ESOCK_OPT_IPV6_RECVPKTINFO};
        recvtclass ->
            {boolean,           L, ?ESOCK_OPT_IPV6_RECVTCLASS};
        router_alert ->
            {integer,           L, ?ESOCK_OPT_IPV6_ROUTER_ALERT};
        rthdr ->
            {boolean,           L, ?ESOCK_OPT_IPV6_RTHDR};
        tclass ->
            {boolean,           L, ?ESOCK_OPT_IPV6_TCLASS};
        unicast_hops ->
            {hops,              L, ?ESOCK_OPT_IPV6_UNICAST_HOPS};
        use_min_mtu ->
            {undefined,         L, ?ESOCK_OPT_IPV6_USE_MIN_MTU};
        v6only ->
            {boolean,           L, ?ESOCK_OPT_IPV6_V6ONLY};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(tcp = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_TCP,
    case Opt of
        [] -> L;
        %%
        congestion ->
            {list,              L, ?ESOCK_OPT_TCP_CONGESTION};
        cork ->
            {boolean,           L, ?ESOCK_OPT_TCP_CORK};
        info ->
            {undefined,         L, ?ESOCK_OPT_TCP_INFO};
        keepcnt ->
            {undefined,         L, ?ESOCK_OPT_TCP_KEEPCNT};
        keepidle ->
            {undefined,         L, ?ESOCK_OPT_TCP_KEEPIDLE};
        keepintvl ->
            {undefined,         L, ?ESOCK_OPT_TCP_KEEPINTVL};
        maxseg ->
            {integer,           L, ?ESOCK_OPT_TCP_MAXSEG};
        md5seg ->
            {undefined,         L, ?ESOCK_OPT_TCP_MD5SIG};
        nodelay ->
            {boolean,           L, ?ESOCK_OPT_TCP_NODELAY};
        noopt ->
            {undefined,         L, ?ESOCK_OPT_TCP_NOOPT};
        nopush ->
            {undefined,         L, ?ESOCK_OPT_TCP_NOPUSH};
        syncnt ->
            %% Only set? 1..255
            {undefined,         L, ?ESOCK_OPT_TCP_SYNCNT};
        user_timeout ->
            {undefined,         L, ?ESOCK_OPT_TCP_USER_TIMEOUT};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(udp = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_UDP,
    case Opt of
        [] -> L;
        %%
        cork ->
            {boolean,           L, ?ESOCK_OPT_UDP_CORK};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(sctp = Level, Opt) ->
    L = ?ESOCK_OPT_LEVEL_SCTP,
    case Opt of
        [] -> L;
        %%
        adaption_layer ->
            {undefined,         L, ?ESOCK_OPT_SCTP_ADAPTION_LAYER};
        associnfo ->    {Opt,   L, ?ESOCK_OPT_SCTP_ASSOCINFO};
        auth_active_key ->
            {undefined,         L, ?ESOCK_OPT_SCTP_AUTH_ACTIVE_KEY};
        auth_asconf ->
            {undefined,         L, ?ESOCK_OPT_SCTP_AUTH_ASCONF};
        auth_chunk ->
            {undefined,         L, ?ESOCK_OPT_SCTP_AUTH_CHUNK};
        auth_key ->
            {undefined,         L, ?ESOCK_OPT_SCTP_AUTH_KEY};
        auth_delete_key ->
            {undefined,         L, ?ESOCK_OPT_SCTP_AUTH_DELETE_KEY};
        autoclose ->
            {integer,           L, ?ESOCK_OPT_SCTP_AUTOCLOSE};
        context ->
            {undefined,         L, ?ESOCK_OPT_SCTP_CONTEXT};
        default_send_params ->
            {undefined,         L, ?ESOCK_OPT_SCTP_DEFAULT_SEND_PARAMS};
        delayed_ack_time ->
            {undefined,         L, ?ESOCK_OPT_SCTP_DELAYED_ACK_TIME};
        disable_fragments ->
            {boolean,           L, ?ESOCK_OPT_SCTP_DISABLE_FRAGMENTS};
        hmac_ident ->
            {undefined,         L, ?ESOCK_OPT_SCTP_HMAC_IDENT};
        events ->       {Opt,   L, ?ESOCK_OPT_SCTP_EVENTS};
        explicit_eor ->
            {undefined,         L, ?ESOCK_OPT_SCTP_EXPLICIT_EOR};
        fragment_intreleave ->
            {undefined,         L, ?ESOCK_OPT_SCTP_FRAGMENT_INTERLEAVE};
        get_peer_addr_info ->
            {undefined,         L, ?ESOCK_OPT_SCTP_GET_PEER_ADDR_INFO};
        initmsg ->      {Opt,   L, ?ESOCK_OPT_SCTP_INITMSG};
        i_want_mapped_v4_addr ->
            {undefined,         L, ?ESOCK_OPT_SCTP_I_WANT_MAPPED_V4_ADDR};
        local_auth_chunks ->
            {undefined,         L, ?ESOCK_OPT_SCTP_LOCAL_AUTH_CHUNKS};
        maxseg ->
            {integer,           L, ?ESOCK_OPT_SCTP_MAXSEG};
        maxburst ->
            {undefined,         L, ?ESOCK_OPT_SCTP_MAXBURST};
        nodelay ->
            {boolean,           L, ?ESOCK_OPT_SCTP_NODELAY};
        partial_delivery_point ->
            {undefined,         L, ?ESOCK_OPT_SCTP_PARTIAL_DELIVERY_POINT};
        peer_addr_params ->
            {undefined,         L, ?ESOCK_OPT_SCTP_PEER_ADDR_PARAMS};
        peer_auth_chunks ->
            {undefined,         L, ?ESOCK_OPT_SCTP_PEER_AUTH_CHUNKS};
        primary_addr ->
            {undefined,         L, ?ESOCK_OPT_SCTP_PRIMARY_ADDR};
        reset_streams ->
            {undefined,         L, ?ESOCK_OPT_SCTP_RESET_STREAMS};
        rtoinfo ->      {Opt,   L, ?ESOCK_OPT_SCTP_RTOINFO};
        set_peer_primary_addr ->
            {undefined,         L, ?ESOCK_OPT_SCTP_SET_PEER_PRIMARY_ADDR};
        status -> % ?ESOCK_OPT_SCTP_RTOINFO;
            {undefined,         L, ?ESOCK_OPT_SCTP_STATUS};
        use_ext_recvinfo ->
            {undefined,         L, ?ESOCK_OPT_SCTP_USE_EXT_RECVINFO};
        _ ->
            not_supported(Level, Opt)
    end;
enc_sockopt_type(Level, _Opt) ->
    not_supported(Level).

%% Validate and possibly encode the setopt value
%%
enc_setopt_value(Level, Opt, Value, Type) ->
    %%
    %% When guards are enough
    %%
    case Type of
        boolean when is_boolean(Value) ->
            Value;
        integer when is_integer(Value) ->
            Value;
        non_neg_integer when is_integer(Value), 0 =< Value ->
            Value;
        byte when is_integer(Value), 0 =< Value, Value =< 255 ->
            Value;
        pid when is_pid(Value) ->
            Value;
        list when is_list(Value) ->
            Value;
        term ->
            Value;
        %%
        mtu_discover
          when Value =:= want;
               Value =:= dont;
               Value =:= do;
               Value =:= probe;
               is_integer(Value) ->
            Value;
        tos
          when Value =:= lowdelay;
               Value =:= throughput;
               Value =:= reliability;
               Value =:= mincost;
               is_integer(Value) ->
            Value;
        addrform when Value =:= inet ->
            enc_domain(Value);
        _ ->
            enc_setopt_value(Level, Opt, {Type, Value})
    end.
%%
enc_setopt_value(Level, Opt, TypeValue) ->
    %%
    %% When matching is needed
    %%
    case TypeValue of
        {rcvbuf, {N, BufSz} = BufSpec}
          when is_integer(N), 0 < N, is_integer(BufSz), 0 < BufSz ->
            %% N:     Number of reads (when specifying length = 0)
            %% BufSz: Size of the "read" buffer
            BufSpec;
        {rcvbuf, Value} ->
            enc_setopt_value(Level, Opt, Value, buf);
        %%
        {buf, default} ->
            0; % This will cause the nif-code to choose the default value
        {buf, Value} ->
            enc_setopt_value(Level, Opt, Value, non_neg_integer);
        %%
        {linger, abort} ->
            {true, 0};
        {linger, {OnOff, Secs} = Linger}
          when is_boolean(OnOff), is_integer(Secs), 0 =< Secs ->
            Linger;
        %%
        {timeval,
         #{sec := Sec, usec := USec} = Timeval}
          when is_integer(Sec), is_integer(USec) ->
            Timeval;
        %%
        {addr_if,
         #{multiaddr := MA, interface := IF} = AddrIf}
          when Level =:= ip
               andalso ?IS_IPV4_ADDR(MA)
               andalso (IF =:= any orelse ?IS_IPV4_ADDR(IF)) ->
            AddrIf;
        {addr_if,
         #{multiaddr := MA, interface := IF} = AddrIf}
          when Level =:= ipv6
               andalso ?IS_IPV6_ADDR(MA)
               andalso (is_integer(IF) andalso (0 =< IF)) ->
            AddrIf;
        %%
        {addr_if_src,
         #{multiaddr := MA, interface := IF, sourceaddr := SA} = AddrIfSrc}
          when ?IS_IPV4_ADDR(MA)
               andalso ?IS_IPV4_ADDR(IF)
               andalso ?IS_IPV4_ADDR(SA) ->
            AddrIfSrc;
        %%
        {msfilter, null = Value} ->
            Value;
        {addr_msfilter,
         #{multiaddr := MA, interface := IF, fmode := FMode, slist := SL} =
             AddrMsfilter}
          when ?IS_IPV4_ADDR(MA)
               andalso ?IS_IPV4_ADDR(IF)
               andalso (FMode =:= include orelse FMode =:= exclude)
               andalso is_list(SL) ->
            AddrMsfilter;
        {hops, default} ->
            -1;
        {hops, Value} ->
            enc_setopt_value(Level, Opt, Value, byte);
        %%
        {associnfo,
         #{assoc_id       := AssocId,
           asocmaxrxt     := MaxRxt,
           num_peer_dests := NumPeerDests,
           peer_rwnd      := PeerRWND,
           local_rwnd     := LocalRWND,
           cookie_life    := CLife} = AssocInfo}
          when is_integer(AssocId),
               is_integer(MaxRxt), (MaxRxt >= 0),
               is_integer(NumPeerDests), (NumPeerDests >= 0),
               is_integer(PeerRWND), (PeerRWND >= 0),
               is_integer(LocalRWND), (LocalRWND >= 0),
               is_integer(CLife), (CLife >= 0) ->
            AssocInfo;
        {events,
         #{data_in          := DataIn,
           association      := Assoc,
           address          := Addr,
           send_failure     := SndFailure,
           peer_error       := PeerError,
           shutdown         := Shutdown,
           partial_delivery := PartialDelivery,
           adaptation_layer := AdaptLayer,
           authentication   := Auth,
           sender_dry       := SndDry} = Events}
          when is_boolean(DataIn),
               is_boolean(Assoc),
               is_boolean(Addr),
               is_boolean(SndFailure),
               is_boolean(PeerError),
               is_boolean(Shutdown),
               is_boolean(PartialDelivery),
               is_boolean(AdaptLayer),
               is_boolean(Auth),
               is_boolean(SndDry) ->
            Events;
        {initmsg,
         #{num_outstreams := NumOut,
           max_instreams  := MaxIn,
           max_attempts   := MaxAttempts,
           max_init_timeo := MaxInitTO} = InitMsg}
          when is_integer(NumOut), (NumOut >= 0),
               is_integer(MaxIn), (MaxIn >= 0),
               is_integer(MaxAttempts), (MaxAttempts >= 0),
               is_integer(MaxInitTO), (MaxInitTO >= 0) ->
            InitMsg;
        {rtoinfo,
         #{assoc_id := AssocId,
           initial  := Init,
           max      := Max,
           min      := Min} = RTOInfo}
          when is_integer(AssocId),
               is_integer(Init), (Init >= 0),
               is_integer(Max), (Max >= 0),
               is_integer(Min), (Min >= 0) ->
            RTOInfo;
        {Type, Value} when Type =/= undefined ->
            %% No match
            not_supported(Level, Opt, Value)
    end.


enc_shutdown_how(How) ->
    case How of
        read ->       ?ESOCK_SHUTDOWN_HOW_READ;
        write ->      ?ESOCK_SHUTDOWN_HOW_WRITE;
        read_write -> ?ESOCK_SHUTDOWN_HOW_READ_WRITE;
        _ ->
            invalid(how, How)
    end.


%% +++ Decode getopt value +++
%%
%% For the most part, we simply let the value pass through, but for some
%% values we may need to do an actual decode.
%%
dec_getopt_value(Alg, tcp, congestion) ->
    %% This string is NULL-terminated, but the general function we use
    %% in the nif code does not know that. So, deal with it here.
    {Str, _} = lists:splitwith(fun(0) -> false; (_) -> true end, Alg),
    Str;
dec_getopt_value(Value, _Level, _Opt) ->    
    Value.

%% ===========================================================================
%% Error functions
%%

-dialyzer({no_return, not_supported/3}).
not_supported(Level, Opt, Value) ->
    not_supported({Level, Opt, Value}).
%%
-dialyzer({no_return, not_supported/2}).
not_supported(Level, Opt) ->
    not_supported({Level, Opt}).
%%
-dialyzer({no_return, not_supported/1}).
not_supported(What) ->
    invalid(not_supported, What).

-dialyzer({no_return, invalid/2}).
invalid(What, Info) ->
    err({invalid, {What, Info}}).

err(Reason) ->
    erlang:error(Reason).

%% ===========================================================================
%% NIF functions
%%

nif_info() -> erlang:nif_error(undef).
nif_info(_SRef) -> erlang:nif_error(undef).

nif_command(_Command) -> erlang:nif_error(undef).

nif_supports() -> erlang:nif_error(undef).
nif_supports(_Key) -> erlang:nif_error(undef).
nif_supports(_Key1, _Key2) -> erlang:nif_error(undef).

nif_open(_FD, _Opts) -> erlang:nif_error(undef).
nif_open(_Domain, _Type, _Protocol, _Opts) -> erlang:nif_error(undef).

nif_bind(_SRef, _SockAddr) -> erlang:nif_error(undef).
nif_bind(_SRef, _SockAddrs, _Action) -> erlang:nif_error(undef).

nif_connect(_SRef) -> erlang:nif_error(undef).
nif_connect(_SRef, _SockAddr) -> erlang:nif_error(undef).

nif_listen(_SRef, _Backlog) -> erlang:nif_error(undef).

nif_accept(_SRef, _Ref) -> erlang:nif_error(undef).

nif_send(_SockRef, _SendRef, _Data, _Flags) -> erlang:nif_error(undef).
nif_sendto(_SRef, _SendRef, _Data, _Dest, _Flags) -> erlang:nif_error(undef).
nif_sendmsg(_SRef, _SendRef, _MsgHdr, _Flags) -> erlang:nif_error(undef).

nif_recv(_SRef, _RecvRef, _Length, _Flags) -> erlang:nif_error(undef).
nif_recvfrom(_SRef, _RecvRef, _Length, _Flags) -> erlang:nif_error(undef).
nif_recvmsg(_SRef, _RecvRef, _BufSz, _CtrlSz, _Flags) -> erlang:nif_error(undef).

nif_close(_SRef) -> erlang:nif_error(undef).
nif_finalize_close(_SRef) -> erlang:nif_error(undef).
nif_shutdown(_SRef, _How) -> erlang:nif_error(undef).

nif_setopt(_Ref, _IsEnc, _Lev, _Opt, _Val) -> erlang:nif_error(undef).
nif_getopt(_Ref, _IsEnc, _Lev, _Opt) -> erlang:nif_error(undef).

nif_sockname(_Ref) -> erlang:nif_error(undef).
nif_peername(_Ref) -> erlang:nif_error(undef).

nif_cancel(_SRef, _Op, _Ref) -> erlang:nif_error(undef).

%% ===========================================================================
