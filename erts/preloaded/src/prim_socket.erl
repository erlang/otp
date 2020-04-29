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
    shutdown/2,
    setopt/4, setopt_native/4, getopt/3, getopt_native/4,
    sockname/1, peername/1,
    cancel/3
   ]).

%% Also in socket
-define(REGISTRY, socket_registry).


%% ===========================================================================
%%
%% Defaults
%%

-define(ESOCK_SOCKADDR_IN4_DEFAULTS,
        (#{port => 0, addr => any})).
-define(ESOCK_SOCKADDR_IN6_DEFAULTS,
        (#{port => 0, addr => any,
           flowinfo => 0, scope_id => 0})).

%% ===========================================================================
%%
%% Constants common to prim_socket_nif.c - has to be "identical"
%%

%% Send flags
-define(ESOCK_SEND_FLAG_CONFIRM,      (1 bsl 0)).
-define(ESOCK_SEND_FLAG_DONTROUTE,    (1 bsl 1)).
-define(ESOCK_SEND_FLAG_EOR,          (1 bsl 2)).
-define(ESOCK_SEND_FLAG_MORE,         (1 bsl 3)).
-define(ESOCK_SEND_FLAG_NOSIGNAL,     (1 bsl 4)).
-define(ESOCK_SEND_FLAG_OOB,          (1 bsl 5)).

%% Recv flags
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
%% Option encodings

-define(ESOCK_OPT_NATIVE_VALUE, 250).
-define(ESOCK_OPT_NATIVE_OPT,   251).
-define(ESOCK_OPT_NATIVE_LEVEL, 252).

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
    case unicode:characters_to_binary(Path, file:native_name_encoding()) of
        {error, _Bin, _Rest} ->
            invalid(path, Path);
        {incomplete, _Bin1, _Bin2} ->
            invalid(path, Path);
        BinPath when is_binary(BinPath) ->
            BinPath
    end.

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
    nif_accept(ListenSockRef, AccRef).

%% ----------------------------------

send(SockRef, SendRef, Data, Flags) ->
    EFlags = enc_send_flags(Flags),
    nif_send(SockRef, SendRef, Data, EFlags).

sendto(SockRef, SendRef, Data, To, Flags) ->
    ETo = enc_sockaddr(To),
    EFlags = enc_send_flags(Flags),
    nif_sendto(SockRef, SendRef, Data, ETo, EFlags).

sendmsg(SockRef, SendRef, MsgHdr, Flags) ->
    EMsgHdr = enc_msghdr(MsgHdr),
    EFlags = enc_send_flags(Flags),
    nif_sendmsg(SockRef, SendRef, EMsgHdr, EFlags).

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
        Result -> Result
    end.

recvfrom(SockRef, RecvRef, Length, Flags) ->
    EFlags = enc_recv_flags(Flags),
    nif_recvfrom(SockRef, RecvRef, Length, EFlags).

recvmsg(SockRef, RecvRef, BufSz, CtrlSz, Flags) ->
    EFlags = enc_recv_flags(Flags),
    nif_recvmsg(SockRef, RecvRef, BufSz, CtrlSz, EFlags).

%% ----------------------------------

close(SockRef) ->
    nif_close(SockRef).

finalize_close(SockRef) ->    
    nif_finalize_close(SockRef).

%% ----------------------------------

shutdown(SockRef, How) ->
    nif_shutdown(SockRef, enc_shutdown_how(How)).

%% ----------------------------------

setopt(SockRef, Level, Opt, Value) ->
    {ELevel, EOpt} = enc_sockopt(Level, Opt),
    nif_setopt(SockRef, ELevel, EOpt, Value, 0).

setopt_native(SockRef, Level, Opt, Value)
  when is_integer(Level), is_integer(Opt) ->
    ArgEncoding = ?ESOCK_OPT_NATIVE_LEVEL,
    nif_setopt(SockRef, Level, Opt, Value, ArgEncoding);
setopt_native(SockRef, Level, Opt, Value)
  when is_integer(Opt) ->
    ELevel = enc_sockopt_level(Level),
    ArgEncoding = ?ESOCK_OPT_NATIVE_OPT,
    nif_setopt(SockRef, ELevel, Opt, Value, ArgEncoding);
setopt_native(SockRef, Level, Opt, Value) ->
    {ELevel, EOpt} = enc_sockopt(Level, Opt),
    ArgEncoding = ?ESOCK_OPT_NATIVE_VALUE,
    nif_setopt(SockRef, ELevel, EOpt, Value, ArgEncoding).


getopt(SockRef, Level, Opt) ->
    {ELevel, EOpt} = enc_sockopt(Level, Opt),
    nif_getopt(SockRef, ELevel, EOpt, undefined, 0).

getopt_native(SockRef, Level, Opt, ValueSpec)
  when is_integer(Level), is_integer(Opt) ->
    ArgEncoding = ?ESOCK_OPT_NATIVE_LEVEL,
    nif_getopt(SockRef, Level, Opt, ValueSpec, ArgEncoding);
getopt_native(SockRef, Level, Opt, ValueSpec)
  when is_integer(Opt) ->
    ELevel = enc_sockopt_level(Level),
    ArgEncoding = ?ESOCK_OPT_NATIVE_OPT,
    nif_getopt(SockRef, ELevel, Opt, ValueSpec, ArgEncoding);
getopt_native(SockRef, Level, Opt, ValueSpec) ->
    {ELevel, EOpt} = enc_sockopt(Level, Opt),
    ArgEncoding = ?ESOCK_OPT_NATIVE_VALUE,
    nif_getopt(SockRef, ELevel, EOpt, ValueSpec, ArgEncoding).

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
enc_sockaddr(#{family := local, path := Path} = SockAddr) ->
  if
      is_list(Path), 0 < length(Path), length(Path) =< 255 ->
          BinPath = encode_path(Path),
          enc_sockaddr(SockAddr#{path => BinPath});
      is_binary(Path), 0 < byte_size(Path), byte_size(Path) =< 255 ->
          SockAddr;
      true ->
          invalid(sockaddr, SockAddr)
  end;
enc_sockaddr(#{family := local} = SockAddr) ->
    invalid(sockaddr, SockAddr);
enc_sockaddr(#{family := _} = SockAddr) ->
    SockAddr;
enc_sockaddr(SockAddr) ->
    invalid(sockaddr, SockAddr).


enc_send_flags([]) -> 0;
enc_send_flags([Flag | Flags]) ->
    case Flag of
        confirm ->   ?ESOCK_SEND_FLAG_CONFIRM;
        dontroute -> ?ESOCK_SEND_FLAG_DONTROUTE;
        eor ->       ?ESOCK_SEND_FLAG_EOR;
        more ->      ?ESOCK_SEND_FLAG_MORE;
        nosignal ->  ?ESOCK_SEND_FLAG_NOSIGNAL;
        oob ->       ?ESOCK_SEND_FLAG_OOB;
        _ -> invalid(send_flag, Flag)
    end bor enc_send_flags(Flags).


enc_recv_flags([]) -> 0;
enc_recv_flags([Flag | Flags]) ->
    case Flag of
        cmsg_cloexec -> ?ESOCK_RECV_FLAG_CMSG_CLOEXEC;
        errqueue ->     ?ESOCK_RECV_FLAG_ERRQUEUE;
        oob ->          ?ESOCK_RECV_FLAG_OOB;
        peek ->         ?ESOCK_RECV_FLAG_PEEK;
        trunc ->        ?ESOCK_RECV_FLAG_TRUNC;
        _ -> invalid(recv_flag, Flag)
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
enc_msghdr(M) ->
    invalid(msghdr, M).


%% Common to setopt and getopt
%%
%% Opt values not handled here will be passed to invalid/2
%% which causes a runtime error.
%%
enc_sockopt(otp = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        debug ->                {L, ?ESOCK_OPT_OTP_DEBUG};
        iow ->                  {L, ?ESOCK_OPT_OTP_IOW};
        controlling_process ->  {L, ?ESOCK_OPT_OTP_CTRL_PROC};
        rcvbuf ->               {L, ?ESOCK_OPT_OTP_RCVBUF};
        rcvctrlbuf ->           {L, ?ESOCK_OPT_OTP_RCVCTRLBUF};
        sndctrlbuf ->           {L, ?ESOCK_OPT_OTP_SNDCTRLBUF};
        fd ->                   {L, ?ESOCK_OPT_OTP_FD};
        meta ->                 {L, ?ESOCK_OPT_OTP_META};
        domain ->               {L, ?ESOCK_OPT_OTP_DOMAIN};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;

enc_sockopt(socket = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        acceptconn ->   {L, ?ESOCK_OPT_SOCK_ACCEPTCONN};
        acceptfilter -> {L, ?ESOCK_OPT_SOCK_ACCEPTFILTER};
        bindtodevice ->
            %% Before linux 3.8, this socket option
            %% could be set but not get.
            %% Maximum size of buffer for name: IFNAMSIZ
            %% So, we let the implementation decide.
            {L, ?ESOCK_OPT_SOCK_BINDTODEVICE};
        broadcast ->    {L, ?ESOCK_OPT_SOCK_BROADCAST};
        busy_poll ->    {L, ?ESOCK_OPT_SOCK_BUSY_POLL};
        debug ->        {L, ?ESOCK_OPT_SOCK_DEBUG};
        domain ->       {L, ?ESOCK_OPT_SOCK_DOMAIN};
        dontroute ->    {L, ?ESOCK_OPT_SOCK_DONTROUTE};
        error ->        {L, ?ESOCK_OPT_SOCK_ERROR};
        keepalive ->    {L, ?ESOCK_OPT_SOCK_KEEPALIVE};
        linger ->       {L, ?ESOCK_OPT_SOCK_LINGER};
        mark ->         {L, ?ESOCK_OPT_SOCK_MARK};
        oobinline ->    {L, ?ESOCK_OPT_SOCK_OOBINLINE};
        passcred ->     {L, ?ESOCK_OPT_SOCK_PASSCRED};
        peek_off ->     {L, ?ESOCK_OPT_SOCK_PEEK_OFF};
        peercred ->     {L, ?ESOCK_OPT_SOCK_PEERCRED};
        priority ->     {L, ?ESOCK_OPT_SOCK_PRIORITY};
        protocol ->     {L, ?ESOCK_OPT_SOCK_PROTOCOL};
        rcvbuf ->       {L, ?ESOCK_OPT_SOCK_RCVBUF};
        rcvbufforce ->  {L, ?ESOCK_OPT_SOCK_RCVBUFFORCE};
        rcvlowat ->
            %% May not work on Linux
            {L, ?ESOCK_OPT_SOCK_RCVLOWAT};
        rcvtimeo ->     {L, ?ESOCK_OPT_SOCK_RCVTIMEO};
        reuseaddr ->    {L, ?ESOCK_OPT_SOCK_REUSEADDR};
        reuseport ->    {L, ?ESOCK_OPT_SOCK_REUSEPORT};
        rxq_ovfl ->     {L, ?ESOCK_OPT_SOCK_RXQ_OVFL};
        setfib ->       {L, ?ESOCK_OPT_SOCK_SETFIB};
        sndbuf ->       {L, ?ESOCK_OPT_SOCK_SNDBUF};
        sndbufforce ->  {L, ?ESOCK_OPT_SOCK_SNDBUFFORCE};
        sndlowat ->
            %% Not changeable on Linux
            {L, ?ESOCK_OPT_SOCK_SNDLOWAT};
        sndtimeo ->     {L, ?ESOCK_OPT_SOCK_SNDTIMEO};
        timestamp ->    {L, ?ESOCK_OPT_SOCK_TIMESTAMP};
        type ->         {L, ?ESOCK_OPT_SOCK_TYPE};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;

enc_sockopt(ip = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        add_membership ->       {L, ?ESOCK_OPT_IP_ADD_MEMBERSHIP};
        add_source_membership -> {L, ?ESOCK_OPT_IP_ADD_SOURCE_MEMBERSHIP};
        block_source ->         {L, ?ESOCK_OPT_IP_BLOCK_SOURCE};
        dontfrag ->
            %% FreeBSD only?
            %% Only respected on udp and raw ip
            %% (unless the hdrincl option has been set)
            {L, ?ESOCK_OPT_IP_DONTFRAG};
        drop_membership ->      {L, ?ESOCK_OPT_IP_DROP_MEMBERSHIP};
        drop_source_membership -> {L, ?ESOCK_OPT_IP_DROP_SOURCE_MEMBERSHIP};
        freebind ->
            %% Linux only?
            {L, ?ESOCK_OPT_IP_FREEBIND};
        hdrincl ->              {L, ?ESOCK_OPT_IP_HDRINCL};
        minttl ->               {L, ?ESOCK_OPT_IP_MINTTL};
        msfilter ->             {L, ?ESOCK_OPT_IP_MSFILTER};
        mtu ->                  {L, ?ESOCK_OPT_IP_MTU};
        mtu_discover ->         {L, ?ESOCK_OPT_IP_MTU_DISCOVER};
        multicast_all ->        {L, ?ESOCK_OPT_IP_MULTICAST_ALL};
        multicast_if ->         {L, ?ESOCK_OPT_IP_MULTICAST_ALL};
        multicast_loop ->       {L, ?ESOCK_OPT_IP_MULTICAST_LOOP};
        multicast_ttl ->        {L, ?ESOCK_OPT_IP_MULTICAST_TTL};
        nodefrag ->             {L, ?ESOCK_OPT_IP_NODEFRAG};
        options ->              {L, ?ESOCK_OPT_IP_OPTIONS};
        pktinfo ->              {L, ?ESOCK_OPT_IP_PKTINFO};
        recvdstaddr ->          {L, ?ESOCK_OPT_IP_RECVDSTADDR};
        recverr ->              {L, ?ESOCK_OPT_IP_RECVERR};
        recvif ->               {L, ?ESOCK_OPT_IP_RECVIF};
        recvopts ->             {L, ?ESOCK_OPT_IP_RECVOPTS};
        recvorigdstaddr ->      {L, ?ESOCK_OPT_IP_RECVORIGDSTADDR};
        recvtos ->              {L, ?ESOCK_OPT_IP_RECVTOS};
        recvttl ->              {L, ?ESOCK_OPT_IP_RECVTTL};
        retopts ->              {L, ?ESOCK_OPT_IP_RETOPTS};
        router_alert ->         {L, ?ESOCK_OPT_IP_ROUTER_ALERT};
        sendsrcaddr ->          {L, ?ESOCK_OPT_IP_SENDSRCADDR};
        tos ->
            %% On FreeBSD it specifies that this option is only valid
            %% for stream, dgram and "some" raw sockets...
            %% No such condition on linux (in the man page)...
            {L, ?ESOCK_OPT_IP_TOS};
        transparent ->          {L, ?ESOCK_OPT_IP_TRANSPARENT};
        ttl ->                  {L, ?ESOCK_OPT_IP_TTL};
        unblock_source ->       {L, ?ESOCK_OPT_IP_UNBLOCK_SOURCE};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;

enc_sockopt(ipv6 = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        addrform ->             {L, ?ESOCK_OPT_IPV6_ADDRFORM};
        add_membership ->       {L, ?ESOCK_OPT_IPV6_ADD_MEMBERSHIP};
        authhdr ->
            %% Is this obsolete? When get, the result is enoprotoopt
            %% and in the  header file it says 'obsolete'...
            %% But there might be (old?) versions of linux
            %% where it still works...
            {L, ?ESOCK_OPT_IPV6_AUTHHDR};
        auth_level ->           {L, ?ESOCK_OPT_IPV6_AUTH_LEVEL};
        checksum ->             {L, ?ESOCK_OPT_IPV6_CHECKSUM};
        drop_membership ->      {L, ?ESOCK_OPT_IPV6_DROP_MEMBERSHIP};
        dstopts ->              {L, ?ESOCK_OPT_IPV6_DSTOPTS};
        esp_network_level ->    {L, ?ESOCK_OPT_IPV6_ESP_NETWORK_LEVEL};
        esp_trans_level ->      {L, ?ESOCK_OPT_IPV6_ESP_TRANS_LEVEL};
        flowinfo ->             {L, ?ESOCK_OPT_IPV6_FLOWINFO};
        hoplimit ->             {L, ?ESOCK_OPT_IPV6_HOPLIMIT};
        hopopts ->              {L, ?ESOCK_OPT_IPV6_HOPOPTS};
        ipcomp_level ->         {L, ?ESOCK_OPT_IPV6_IPCOMP_LEVEL};
        join_group ->           {L, ?ESOCK_OPT_IPV6_JOIN_GROUP};
        leave_group ->          {L, ?ESOCK_OPT_IPV6_LEAVE_GROUP};
        mtu ->                  {L, ?ESOCK_OPT_IPV6_MTU};
        mtu_discover ->         {L, ?ESOCK_OPT_IPV6_MTU_DISCOVER};
        multicast_hops ->       {L, ?ESOCK_OPT_IPV6_MULTICAST_HOPS};
        multicast_if ->         {L, ?ESOCK_OPT_IPV6_MULTICAST_IF};
        multicast_loop ->       {L, ?ESOCK_OPT_IPV6_MULTICAST_LOOP};
        portrange ->            {L, ?ESOCK_OPT_IPV6_PORTRANGE};
        pktoptions ->           {L, ?ESOCK_OPT_IPV6_PKTOPTIONS};
        recverr ->              {L, ?ESOCK_OPT_IPV6_RECVERR};
        recvhoplimit ->         {L, ?ESOCK_OPT_IPV6_RECVHOPLIMIT};
        recvpktinfo ->          {L, ?ESOCK_OPT_IPV6_RECVPKTINFO};
        pktinfo -> % alias on FreeBSD
            {L, ?ESOCK_OPT_IPV6_RECVPKTINFO};
        recvtclass ->           {L, ?ESOCK_OPT_IPV6_RECVTCLASS};
        router_alert ->         {L, ?ESOCK_OPT_IPV6_ROUTER_ALERT};
        rthdr ->                {L, ?ESOCK_OPT_IPV6_RTHDR};
        tclass ->               {L, ?ESOCK_OPT_IPV6_TCLASS};
        unicast_hops ->         {L, ?ESOCK_OPT_IPV6_UNICAST_HOPS};
        use_min_mtu ->          {L, ?ESOCK_OPT_IPV6_USE_MIN_MTU};
        v6only ->               {L, ?ESOCK_OPT_IPV6_V6ONLY};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;

enc_sockopt(tcp = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        congestion ->           {L, ?ESOCK_OPT_TCP_CONGESTION};
        cork ->                 {L, ?ESOCK_OPT_TCP_CORK};
        info ->                 {L, ?ESOCK_OPT_TCP_INFO};
        keepcnt ->              {L, ?ESOCK_OPT_TCP_KEEPCNT};
        keepidle ->             {L, ?ESOCK_OPT_TCP_KEEPIDLE};
        keepintvl ->            {L, ?ESOCK_OPT_TCP_KEEPINTVL};
        maxseg ->               {L, ?ESOCK_OPT_TCP_MAXSEG};
        md5seg ->               {L, ?ESOCK_OPT_TCP_MD5SIG};
        nodelay ->              {L, ?ESOCK_OPT_TCP_NODELAY};
        noopt ->                {L, ?ESOCK_OPT_TCP_NOOPT};
        nopush ->               {L, ?ESOCK_OPT_TCP_NOPUSH};
        syncnt ->
            %% Only set? 1..255
            {L, ?ESOCK_OPT_TCP_SYNCNT};
        user_timeout ->         {L, ?ESOCK_OPT_TCP_USER_TIMEOUT};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;

enc_sockopt(udp = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        [] -> L;
        %%
        cork -> {L, ?ESOCK_OPT_UDP_CORK};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;

enc_sockopt(sctp = Level, Opt) ->
    L = enc_sockopt_level(Level),
    case Opt of
        adaption_layer ->       {L, ?ESOCK_OPT_SCTP_ADAPTION_LAYER};
        associnfo ->            {L, ?ESOCK_OPT_SCTP_ASSOCINFO};
        auth_active_key ->      {L, ?ESOCK_OPT_SCTP_AUTH_ACTIVE_KEY};
        auth_asconf ->          {L, ?ESOCK_OPT_SCTP_AUTH_ASCONF};
        auth_chunk ->           {L, ?ESOCK_OPT_SCTP_AUTH_CHUNK};
        auth_key ->             {L, ?ESOCK_OPT_SCTP_AUTH_KEY};
        auth_delete_key ->      {L, ?ESOCK_OPT_SCTP_AUTH_DELETE_KEY};
        autoclose ->            {L, ?ESOCK_OPT_SCTP_AUTOCLOSE};
        context ->              {L, ?ESOCK_OPT_SCTP_CONTEXT};
        default_send_params ->  {L, ?ESOCK_OPT_SCTP_DEFAULT_SEND_PARAMS};
        delayed_ack_time ->     {L, ?ESOCK_OPT_SCTP_DELAYED_ACK_TIME};
        disable_fragments ->    {L, ?ESOCK_OPT_SCTP_DISABLE_FRAGMENTS};
        hmac_ident ->           {L, ?ESOCK_OPT_SCTP_HMAC_IDENT};
        events ->               {L, ?ESOCK_OPT_SCTP_EVENTS};
        explicit_eor ->         {L, ?ESOCK_OPT_SCTP_EXPLICIT_EOR};
        fragment_intreleave ->  {L, ?ESOCK_OPT_SCTP_FRAGMENT_INTERLEAVE};
        get_peer_addr_info ->   {L, ?ESOCK_OPT_SCTP_GET_PEER_ADDR_INFO};
        initmsg ->              {L, ?ESOCK_OPT_SCTP_INITMSG};
        i_want_mapped_v4_addr -> {L, ?ESOCK_OPT_SCTP_I_WANT_MAPPED_V4_ADDR};
        local_auth_chunks ->    {L, ?ESOCK_OPT_SCTP_LOCAL_AUTH_CHUNKS};
        maxseg ->               {L, ?ESOCK_OPT_SCTP_MAXSEG};
        maxburst ->             {L, ?ESOCK_OPT_SCTP_MAXBURST};
        nodelay ->              {L, ?ESOCK_OPT_SCTP_NODELAY};
        partial_delivery_point -> {L, ?ESOCK_OPT_SCTP_PARTIAL_DELIVERY_POINT};
        peer_addr_params ->     {L, ?ESOCK_OPT_SCTP_PEER_ADDR_PARAMS};
        peer_auth_chunks ->     {L, ?ESOCK_OPT_SCTP_PEER_AUTH_CHUNKS};
        primary_addr ->         {L, ?ESOCK_OPT_SCTP_PRIMARY_ADDR};
        reset_streams ->        {L, ?ESOCK_OPT_SCTP_RESET_STREAMS};
        rtoinfo ->              {L, ?ESOCK_OPT_SCTP_RTOINFO};
        set_peer_primary_addr -> {L, ?ESOCK_OPT_SCTP_SET_PEER_PRIMARY_ADDR};
        status -> % ?ESOCK_OPT_SCTP_RTOINFO;
            {L, ?ESOCK_OPT_SCTP_STATUS};
        use_ext_recvinfo ->     {L, ?ESOCK_OPT_SCTP_USE_EXT_RECVINFO};
        _ ->
            invalid(socket_option, {Level, Opt})
    end;
enc_sockopt(Level, _Opt) ->
    invalid(sockopt_level, Level).

enc_sockopt_level(Level) ->
    case Level of
        otp ->      ?ESOCK_OPT_LEVEL_OTP;            
        socket ->   ?ESOCK_OPT_LEVEL_SOCKET;
        ip ->       ?ESOCK_OPT_LEVEL_IP;
        ipv6 ->     ?ESOCK_OPT_LEVEL_IPV6;
        tcp ->      ?ESOCK_OPT_LEVEL_TCP;
        udp ->      ?ESOCK_OPT_LEVEL_UDP;
        sctp ->     ?ESOCK_OPT_LEVEL_SCTP;
        _ ->
            invalid(sockopt_level, Level)
    end.

%% Encode the setopt value
%%
enc_shutdown_how(How) ->
    case How of
        read ->       ?ESOCK_SHUTDOWN_HOW_READ;
        write ->      ?ESOCK_SHUTDOWN_HOW_WRITE;
        read_write -> ?ESOCK_SHUTDOWN_HOW_READ_WRITE;
        _ ->
            invalid(shutdown_how, How)
    end.

%% ===========================================================================
%% Error functions
%%

invalid(What, Info) ->
    erlang:error({invalid, {What, Info}}).

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

nif_setopt(_Ref, _Lev, _Opt, _Val, _ArgEnc) -> erlang:nif_error(undef).
nif_getopt(_Ref, _Lev, _Opt, _ValSpec, _ArgEnc) -> erlang:nif_error(undef).

nif_sockname(_Ref) -> erlang:nif_error(undef).
nif_peername(_Ref) -> erlang:nif_error(undef).

nif_cancel(_SRef, _Op, _Ref) -> erlang:nif_error(undef).

%% ===========================================================================
