%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
         number_of/0,
         which_sockets/0, which_sockets/1,

         debug/1, socket_debug/1, use_registry/1,
	 info/0, info/1,
         supports/0, supports/1, supports/2,
         is_supported/1, is_supported/2, is_supported/3
        ]).

-export([
         open/1, open/2, open/3, open/4,
         bind/2, bind/3,
         connect/1, connect/2, connect/3,
         listen/1, listen/2,
         accept/1, accept/2,

         send/2, send/3, send/4,
         sendto/3, sendto/4, sendto/5,
         sendmsg/2, sendmsg/3, sendmsg/4,

         recv/1, recv/2, recv/3, recv/4,
         recvfrom/1, recvfrom/2, recvfrom/3, recvfrom/4,
         recvmsg/1, recvmsg/2, recvmsg/3, recvmsg/5,

         close/1,
         shutdown/2,

         setopt/4,
         getopt/3,

         sockname/1,
         peername/1,

         cancel/2
        ]).

-export_type([
              socket/0,

              select_tag/0,
              select_ref/0,
              select_info/0,

              socket_counters/0,
              socket_info/0,

              domain/0,
              type/0,
              protocol/0,

              port_number/0,
              ip4_address/0,
              ip6_address/0,
              sockaddr/0,
              sockaddr_in4/0,
              sockaddr_in6/0,
              sockaddr_un/0,
              sockaddr_ll/0,

              send_flags/0,
              send_flag/0,

              recv_flags/0,
              recv_flag/0,

              shutdown_how/0,

              sockopt_level/0,
              otp_socket_option/0,
              socket_option/0,
              ip_socket_option/0,
              ipv6_socket_option/0,
              tcp_socket_option/0,
              udp_socket_option/0,
              sctp_socket_option/0,
              raw_socket_option/0,

              timeval/0,
              ip_tos/0,
              ip_mreq/0,
              ip_mreq_source/0,
              ip_pmtudisc/0,
              ip_msfilter_mode/0,
              ip_msfilter/0,
              ip_pktinfo/0,
              ipv6_mreq/0,
              ipv6_pmtudisc/0,
              ipv6_pktinfo/0,
              in6_flow_info/0,
              in6_scope_id/0,
              sctp_assoc_id/0,
              sctp_sndrcvinfo/0,
              sctp_event_subscribe/0,
              sctp_assocparams/0,
              sctp_initmsg/0,
              sctp_rtoinfo/0,


              msghdr_flag/0,
              msghdr_flags/0,
              msghdr/0,
              cmsghdr_level/0,
              cmsghdr_type/0,
              %% cmsghdr_data/0,
              cmsghdr_recv/0, cmsghdr_send/0,

              ee_origin/0,
              icmp_dest_unreach/0,
              icmpv6_dest_unreach/0,
              extended_err/0,

              uint8/0,
              uint16/0,
              uint20/0,
              uint32/0,
              int32/0
             ]).

%% Also in prim_socket
-define(REGISTRY, socket_registry).


-type socket_counters() :: #{read_byte     := non_neg_integer(),
                             read_fails    := non_neg_integer(),
                             read_pkg      := non_neg_integer(),
                             read_pkg_max  := non_neg_integer(),
                             read_tries    := non_neg_integer(),
                             read_waits    := non_neg_integer(),
                             write_byte    := non_neg_integer(),
                             write_fails   := non_neg_integer(),
                             write_pkg     := non_neg_integer(),
                             write_pkg_max := non_neg_integer(),
                             write_tries   := non_neg_integer(),
                             write_waits   := non_neg_integer(),
                             acc_success   := non_neg_integer(),
                             acc_fails     := non_neg_integer(),
                             acc_tries     := non_neg_integer(),
                             acc_waits     := non_neg_integer()}.
-type socket_info() :: #{domain        := domain(),
                         type          := type(),
                         protocol      := protocol(),
                         ctrl          := pid(),
                         ctype         := normal | fromfd | {fromfd, integer()},
                         counters      := socket_counters(),
                         num_readers   := non_neg_integer(),
                         num_writers   := non_neg_integer(),
                         num_acceptors := non_neg_integer(),
                         writable      := boolean(),
                         readable      := boolean()}.

-type uint8()  :: 0..16#FF.
-type uint16() :: 0..16#FFFF.
-type uint20() :: 0..16#FFFFF.
-type uint32() :: 0..16#FFFFFFFF.
-type int32()  :: -2147483648..2147483647.


%% We support only a subset of all domains.
-type domain() :: local | inet | inet6.

%% We support only a subset of all types.
%% RDM - Reliably Delivered Messages
-type type()   :: stream | dgram | raw | rdm | seqpacket.

%% We support only a subset of all protocols:
%% Note that the '{raw, integer()}' construct is intended
%% to be used with type = raw.
%% Note also that only the "superuser" can create a raw socket.
-type protocol() :: ip | tcp | udp | sctp | icmp | igmp | {raw, integer()}.

-type port_number() :: 0..65535.

-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.

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

-type timeval() :: #{sec  := integer(),
                     usec := integer()}.

-type ip_pktinfo() :: #{
                        ifindex  := non_neg_integer(), % Interface Index
                        spec_dst := ip4_address(),     % Local Address
                        addr     := ip4_address()      % Header Destination address
                       }.

%% If the integer value is used, its up to the caller to ensure its valid!
-type ip_tos() :: lowdelay |
                  throughput |
                  reliability |
                  mincost |
                  integer().

%% This type is used when requesting to become member of a multicast
%% group with a call to setopt. Example: 
%%
%%     socket:setopt(Socket, ip, add_membership, #{multiaddr => Addr,
%%                                                 interface => any}).
%%
%% Its also used when removing from a multicast group. Example:
%%
%%     socket:setopt(Socket, ip, drop_membership, #{multiaddr => Addr,
%%                                                  interface => any}).
%%

-type ip_mreq() :: #{multiaddr := ip4_address(),
                     interface := any | ip4_address()}.
%% -type ip_mreqn() :: #{multiaddr := ip4_address(),
%%                       address   := any | ip4_address(),
%%                       ifindex   := integer()}.

-type ip_mreq_source() :: #{multiaddr  := ip4_address(),
                            interface  := ip4_address(),
                            sourceaddr := ip4_address()}.

-type ip_pmtudisc() :: want | dont | do | probe.

%% multiaddr: Multicast group address
%% interface: Address of local interface
%% mode:      Filter mode
%% slist:     List of source addresses
-type ip_msfilter_mode() :: include | exclude.

-type ip_msfilter() :: #{multiaddr := ip4_address(),
                         interface := ip4_address(),
                         mode      := ip_msfilter_mode(),
                         slist     := [ip4_address()]}.

-type ipv6_mreq() :: #{multiaddr := ip6_address(),
                       interface := non_neg_integer()}.

-type ipv6_pmtudisc() :: ip_pmtudisc().

-type ipv6_pktinfo() :: #{
                          addr    := ip6_address(),
                          ifindex := integer()
                         }.


-type sctp_assoc_id() :: int32().
-type sctp_sndrcvinfo() :: #{
                             stream     := uint16(),
                             ssn        := uint16(),
                             flags      := uint16(),
                             ppid       := uint16(),
                             context    := uint16(),
                             timetolive := uint16(),
                             tsn        := uint16(),
                             cumtsn     := uint16(),
                             assoc_id   := sctp_assoc_id()
                            }.

-type sctp_event_subscribe() :: #{data_in          := boolean(),
                                  association      := boolean(),
                                  address          := boolean(),
                                  send_failure     := boolean(),
                                  peer_error       := boolean(),
                                  shutdown         := boolean(),
                                  partial_delivery := boolean(),
                                  adaptation_layer := boolean(),
                                  authentication   := boolean(),
                                  sender_dry       := boolean()}.

-type sctp_assocparams() :: #{assoc_id       := sctp_assoc_id(),
                              max_rxt        := uint16(),
                              num_peer_dests := uint16(),
                              peer_rwnd      := uint32(),
                              local_rwnd     := uint32(),
                              cookie_life    := uint32()}.

-type sctp_initmsg() :: #{num_outstreams := uint16(),
                          max_instreams  := uint16(),
                          max_attempts   := uint16(),
                          max_init_timeo := uint16()
                         }.

-type sctp_rtoinfo() :: #{assoc_id := sctp_assoc_id(),
                          initial  := uint32(),
                          max      := uint32(),
                          min      := uint32()}.

-type sockaddr_un()  :: #{family := local,
                          path   := binary() | string()}.
-type sockaddr_in4() :: #{family := inet,
                          port   := port_number(),
			  %% The 'broadcast' here is the "limited broadcast"
                          addr   := any | broadcast | loopback | ip4_address()}.
-type sockaddr_in6() :: #{family   := inet6,
                          port     := port_number(),
                          addr     := any | loopback | ip6_address(),
                          flowinfo := in6_flow_info(),
                          scope_id := in6_scope_id()}.
-type sockaddr_ll() :: #{family   := packet,
                         protocol := non_neg_integer(),
                         ifindex  := integer(),
                         pkttype  := packet_type(),
                         hatype   := non_neg_integer(),
                         addr     := binary()}.
-type packet_type() :: host | broadcast | multicast | otherhost |
                       outgoing | loopback | user | kernel | fastroute |
                       non_neg_integer().
-type sockaddr() :: sockaddr_in4() |
                    sockaddr_in6() |
                    sockaddr_un()  |
                    sockaddr_ll().

%% otp    - This option is internal to our (OTP) implementation.
%% socket - The socket layer (SOL_SOCKET).
%% ip     - The IP layer (SOL_IP or is it IPPROTO_IP?).
%% ipv6   - The IPv6 layer (SOL_IPV6).
%% tcp    - The TCP (Transport Control Protocol) layer (IPPROTO_TCP).
%% udp    - The UDP (User Datagram Protocol) layer (IPPROTO_UDP).
%% sctp   - The SCTP (Stream Control Transmission Protocol) layer (IPPROTO_SCTP).
%% Int    - Raw level, sent down and used "as is".
%%          Its up to the caller to make sure this is correct!
-type sockopt_level() :: otp |
                         socket |
                         ip | ipv6 | tcp | udp | sctp |
                         non_neg_integer().

%% There are some options that are 'read-only'.
%% Should those be included here or in a special list?
%% Should we just document it and leave it to the user?
%% Or catch it in the encode functions?
%% A setopt for a readonly option leads to einval?
%% Do we really need a sndbuf?

-type otp_socket_option() :: debug |
                             use_registry |
                             iow |
                             controlling_process |
                             rcvbuf | % sndbuf |
                             rcvctrlbuf |
                             sndctrlbuf |
                             meta |
                             fd.
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
                         domain |
                         dontroute |
                         error |
                         keepalive |
                         linger |
                         mark |
                         oobinline |
                         passcred |
                         peek_off |
                         peercred |
                         priority |
                         protocol |
                         rcvbuf |
                         rcvbufforce |
                         rcvlowat |
                         rcvtimeo |
                         reuseaddr |
                         reuseport |
                         rxq_ovfl |
                         setfib |
                         sndbuf |
                         sndbufforce |
                         sndlowat |
                         sndtimeo |
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
        addrform |
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
        pktoptions |
        recverr |
        recvhoplimit | hoplimit |
        recvpktinfo | pktinfo |
        recvtclass |
        router_alert |
        rthdr |
        tclass |
        unicast_hops |
        use_min_mtu |
        v6only.

-type tcp_socket_option()  :: congestion |
                              cork |
                              info |
                              keepcnt |
                              keepidle |
                              keepintvl |
                              maxseg |
                              md5sig |
                              nodelay |
                              noopt |
                              nopush |
                              syncnt |
                              user_timeout.

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

-type raw_socket_option() :: filter.

%% -type plain_socket_option()  :: integer().
%% -type sockopt() :: otp_socket_option() |
%%                    socket_option() |
%%                    ip_socket_option() |
%%                    ipv6_socket_option() |
%%                    tcp_socket_option() |
%%                    udp_socket_option() |
%%                    sctp_socket_option() |
%%                    raw_socket_option() |
%%                    plain_socket_option().

%% The names of these macros match the names of corresponding
%%C functions in the NIF code, so a search will match both
%%
-define(socket_tag, '$socket').
%%
%% Our socket abstract data type
-define(mk_socket(Ref), {?socket_tag, (Ref)}).
%%
%% Messages sent from the nif-code to erlang processes:
-define(mk_socket_msg(Socket, Tag, Info), {?socket_tag, (Socket), (Tag), (Info)}).

-opaque socket() :: ?mk_socket(reference()).

-type send_flags() :: [send_flag()].
-type send_flag()  :: confirm |
                      dontroute |
                      eor |
                      more |
                      nosignal |
                      oob.

%% Note that not all of these flags are useful for every recv function!
%%
-type recv_flags() :: [recv_flag()].
-type recv_flag()  :: cmsg_cloexec |
                      errqueue |
                      oob |
                      peek |
                      trunc.

-type shutdown_how() :: read | write | read_write.

-type msghdr_flag()  :: ctrunc | eor | errqueue | oob | trunc.
-type msghdr_flags() :: [msghdr_flag()].
-type msghdr() :: #{
                    %% *Optional* target address
                    %% Used on an unconnected socket to specify the 
                    %% target address for a datagram.
                    addr  := sockaddr(),
                    
                    iov   := [binary()],

                    %% The maximum size of the control buffer is platform
                    %% specific. It is the users responsibility to ensure 
                    %% that its not exceeded.
                    ctrl  := [cmsghdr_recv()] | [cmsghdr_send()],

                    %% Only valid with recvmsg
                    flags := msghdr_flags()
                   }.
%% We are able to (completely) decode *some* control message headers.
%% Even if we are able to decode both level and type, we may not be
%% able to decode the data, in which case it will be a binary.

-type cmsghdr_level() :: socket | ip | ipv6 | integer().
-type cmsghdr_type()  :: credentials |
                         hoplevel    |
                         origdstaddr |
                         pktinfo     |
                         recvtos     |
                         rights |
                         timestamp   |
                         tos |
                         ttl         |
                         integer().
-type cmsghdr_recv() :: 
        #{level := socket,    type := timestamp,   data := timeval()}      |
        #{level := socket,    type := rights,      data := binary()}       |
        #{level := socket,    type := credentials, data := binary()}       |
        #{level := socket,    type := integer(),   data := binary()}       |
        #{level := ip,        type := tos,         data := ip_tos()}       |
        #{level := ip,        type := recvtos,     data := ip_tos()}       |
        #{level := ip,        type := ttl,         data := integer()}      |
        #{level := ip,        type := recvttl,     data := integer()}      |
        #{level := ip,        type := pktinfo,     data := ip_pktinfo()}   |
        #{level := ip,        type := origdstaddr, data := sockaddr_in4()} |
        #{level := ip,        type := recverr,     data := extended_err() | binary()} |
        #{level := ip,        type := integer(),   data := binary()}       |
        #{level := ipv6,      type := hoplevel,    data := integer()}      |
        #{level := ipv6,      type := pktinfo,     data := ipv6_pktinfo()} |
        #{level := ipv6,      type := recverr,     data := extended_err() | binary()} |
        #{level := ipv6,      type := tclass,      data := integer()}      |
        #{level := ipv6,      type := integer(),   data := binary()}       |
        #{level := integer(), type := integer(),   data := binary()}.
-type cmsghdr_send() :: 
        #{level := socket,    type := timestamp,   data := binary()} |
        #{level := socket,    type := rights,      data := binary()} |
        #{level := socket,    type := credentials, data := binary()} |
        #{level := socket,    type := integer(),   data := binary()} |
        #{level := ip,        type := tos,         data := ip_tos()  | binary()} |
        #{level := ip,        type := ttl,         data := integer() | binary()} |
        #{level := ip,        type := integer(),   data := binary()} |
        #{level := ipv6,      type := tclass,      data := integer()}      |
        #{level := ipv6,      type := integer(),   data := binary()} |
        #{level := udp,       type := integer(),   data := binary()} |
        #{level := integer(), type := integer(),   data := binary()}.

-type ee_origin() :: none | local | icmp | icmp6 | uint8().
-type icmp_dest_unreach() :: net_unreach | host_unreach | port_unreach | frag_needed |
                             net_unknown | host_unknown | uint8().
-type icmpv6_dest_unreach() :: noroute | adm_prohibited | not_neighbour | addr_unreach |
                               port_unreach | policy_fail | reject_route | uint8().
-type extended_err() ::
        #{error    := term(),
          origin   := icmp,
          type     := dest_unreach,
          code     := icmp_dest_unreach(),
          info     := uint32(),
          data     := uint32(),
          offender := undefined | sockaddr()} |
        #{error    := term(),
          origin   := icmp,
          type     := time_exceeded | uint8(),
          code     := uint8(),
          info     := uint32(),
          data     := uint32(),
          offender := undefined | sockaddr()} |
        #{error    := term(),
          origin   := icmp6,
          type     := dest_unreach,
          code     := icmpv6_dest_unreach(),
          info     := uint32(),
          data     := uint32(),
          offender := undefined | sockaddr()} |
        #{error    := term(),
          origin   := icmp6,
          type     := pkt_toobig | time_exceeded | uint8(),
          code     := uint8(),
          info     := uint32(),
          data     := uint32(),
          offender := undefined | sockaddr()} |
        #{error    := term(),
          origin   := ee_origin(),
          type     := uint8(),
          code     := uint8(),
          info     := uint32(),
          data     := uint32(),
          offender := undefined | sockaddr()}.

-type errcode() ::
        inet:posix() | % closed | timeout | not_owner |
        exalloc | exmonitor | exselect | exself.

%% ===========================================================================
%%
%% Interface term formats
%%

-opaque select_tag() :: atom().
-opaque select_ref() :: reference().

-type select_info() :: {select_info, select_tag(), select_ref()}.

-define(SELECT_INFO(T, R), {select_info, T, R}).
-define(SELECT(T, R),      {select, ?SELECT_INFO(T, R)}).


%% ===========================================================================
%%
%% Defaults
%%

-define(ESOCK_LISTEN_BACKLOG_DEFAULT, 5).

-define(ESOCK_ACCEPT_TIMEOUT_DEFAULT, infinity).

-define(ESOCK_SEND_FLAGS_DEFAULT,      []).
-define(ESOCK_SEND_TIMEOUT_DEFAULT,    infinity).
-define(ESOCK_SENDTO_FLAGS_DEFAULT,    []).
-define(ESOCK_SENDTO_TIMEOUT_DEFAULT,  ?ESOCK_SEND_TIMEOUT_DEFAULT).
-define(ESOCK_SENDMSG_FLAGS_DEFAULT,   []).
-define(ESOCK_SENDMSG_TIMEOUT_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).

-define(ESOCK_RECV_FLAGS_DEFAULT,   []).
-define(ESOCK_RECV_TIMEOUT_DEFAULT, infinity).


%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

%% *** number_of ***
%%
%% Interface function to the socket registry
%% returns the number of existing (and "alive") sockets.
%%
-spec number_of() -> non_neg_integer().

number_of() ->
    ?REGISTRY:number_of().


%% *** which_sockets/0,1 ***
%%
%% Interface function to the socket registry
%% Returns a list of all the sockets, accoring to the filter rule.
%%
-spec which_sockets() -> [socket()].

which_sockets() ->
    ?REGISTRY:which_sockets(fun(_) -> true end).

-spec which_sockets(FilterRule) -> [socket()] when
      FilterRule :: inet | inet6 |
                    stream | dgram | seqpacket |
                    sctp | tcp | udp |
                    pid() |
                    fun((socket_info()) -> boolean()).

which_sockets(Domain)
  when ((Domain =:= inet) orelse (Domain =:= inet6)) ->
    ?REGISTRY:which_sockets(fun(#{domain := D}) when (D =:= Domain) -> true; 
                               (_) -> false end);
which_sockets(Type)
  when ((Type =:= stream) orelse (Type =:= dgram) orelse (Type =:= seqpacket)) ->
    ?REGISTRY:which_sockets(fun(#{type := T}) when (T =:= Type) -> true;
                               (_) -> false end);
which_sockets(Proto)
  when ((Proto =:= sctp) orelse (Proto =:= tcp) orelse (Proto =:= udp)) ->
    ?REGISTRY:which_sockets(fun(#{protocol := P}) when (P =:= Proto) -> true;
                               (_) -> false end);
which_sockets(CTRL)
  when is_pid(CTRL) ->
    ?REGISTRY:which_sockets(fun(#{ctrl := C}) when (C =:= CTRL) -> true;
                               (_) -> false end);
which_sockets(Filter) when is_function(Filter, 1) ->
    ?REGISTRY:which_sockets(Filter).


%% ===========================================================================
%%
%% Debug features
%%
%% ===========================================================================


-spec info() -> map().
%%
info() ->
    prim_socket:info().


-spec debug(D :: boolean()) -> ok.
%%
debug(D) when is_boolean(D) ->
    prim_socket:debug(D).


-spec socket_debug(D :: boolean()) -> ok.
%%
socket_debug(D) when is_boolean(D) ->
    prim_socket:socket_debug(D).


-spec use_registry(D :: boolean()) -> ok.
%%
use_registry(D) when is_boolean(D) ->
    prim_socket:use_registry(D).


%% ===========================================================================
%%
%% info - Get miscellaneous information about a socket.
%%
%% Generates a list of various info about the socket, such as counter values.
%%
%% Do *not* call this function often.
%% 
%% ===========================================================================

-spec info(Socket) -> socket_info() when
      Socket :: socket().
%%
info(?mk_socket(SockRef)) when is_reference(SockRef) ->
    prim_socket:info(SockRef);
info(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% supports - get information about what the platform "supports".
%%
%% Generates a list of various info about what the plaform can support. 
%% The most obvious case is 'options'. 
%%
%% Each item in a 'supports'-list will appear only *one* time.
%% 
%% ===========================================================================

-spec supports() -> [{Key1 :: term(),
                      boolean() | [{Key2 :: term(),
                                    boolean() | [{Key3 :: term(),
                                                  boolean()}]}]}].
supports() ->
    [{Key1, supports(Key1)}
     || Key1 <- [options, send_flags, recv_flags]]
        ++ prim_socket:supports().

-spec supports(Key1 :: term()) ->
                      [{Key2 :: term(),
                        boolean() | [{Key3 :: term(),
                                      boolean()}]}].
%%
supports(options) ->
    [{Level, supports(options, Level)}
     || Level <- [socket, ip, ipv6, tcp, udp, sctp]];
supports(Key) ->
    prim_socket:supports(Key).

-spec supports(Key1 :: term(), Key2 :: term()) ->
                      [{Key3 :: term(),
                        boolean()}].
%%
supports(Key1, Key2) ->
    prim_socket:supports(Key1, Key2).


-spec is_supported(Key1 :: term()) ->
                          boolean().
is_supported(Key1) ->
    get_is_supported(Key1, supports()).
%%
-spec is_supported(Key1 :: term(), Key2 :: term()) ->
                          boolean().
is_supported(Key1, Key2) ->
    get_is_supported(Key2, supports(Key1)).
%%
-spec is_supported(Key1 :: term(), Key2 :: term(), Key3 :: term()) ->
                          boolean().
is_supported(Key1, Key2, Key3) ->
    get_is_supported(Key3, supports(Key1, Key2)).


get_is_supported(Key, Supported) ->
    case lists:keyfind(Key, 1, Supported) of
        false ->
            false;
        {_, Value} ->
            if
                is_boolean(Value) ->
                    Value;
                is_list(Value) ->
                    false
            end
    end.


%% ===========================================================================
%%
%% The proper socket API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% <KOLLA>
%%
%% The nif sets up a monitor to this process, and if it dies the socket
%% is closed. It is also used if someone wants to monitor the socket.
%%
%% We may therefor need monitor function(s): 
%%
%%               socket:monitor(Socket)
%%               socket:demonitor(Socket)
%%
%% </KOLLA>
%%

%% ===========================================================================
%%
%% open - create an endpoint for communication
%%

-spec open(FD) -> {ok, Socket} | {error, Reason} when
      FD     :: integer(),
      Socket :: socket(),
      Reason :: errcode().

open(FD) when is_integer(FD) ->
    open(FD, #{});
open(FD) ->
    erlang:error(badarg, [FD]).
                  
-spec open(FD, Opts) -> {ok, Socket} | {error, Reason} when
      FD       :: integer(),
      Opts     ::
        #{domain       => domain(),
          type         => type(),
          protocol     => protocol(),
          dup          => boolean(),
	  debug        => boolean(),
	  use_registry => boolean()},
      Socket   :: socket(),
      Reason   :: errcode();

          (Domain, Type) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Socket   :: socket(),
      Reason   :: errcode().

open(FD, Opts) when is_integer(FD), is_map(Opts) ->
    case prim_socket:open(FD, Opts) of
        {ok, SockRef} ->
            Socket = ?mk_socket(SockRef),
            {ok, Socket};
        {error, _} = ERROR ->
            ERROR
    end;
open(Domain, Type) ->
    open(Domain, Type, default).

-spec open(Domain, Type, Protocol) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: default | protocol(),
      Socket   :: socket(),
      Reason   :: errcode().

open(Domain, Type, Protocol) ->
    open(Domain, Type, Protocol, #{}).

-spec open(Domain, Type, Protocol, Opts) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: default | protocol(),
      Opts     ::
        #{netns        => string(),
	  debug        => boolean(),
	  use_registry => boolean()},
      Socket   :: socket(),
      Reason   :: errcode().

open(Domain, Type, Protocol, Opts) when is_map(Opts) ->
    case prim_socket:open(Domain, Type, Protocol, Opts) of
        {ok, SockRef} ->
            Socket = ?mk_socket(SockRef),
            {ok, Socket};
        {error, _} = ERROR ->
            ERROR
    end;
open(Domain, Type, Protocol, Opts) ->
    erlang:error(badarg, [Domain, Type, Protocol, Opts]).


%% ===========================================================================
%%
%% bind - bind a name (an address) to a socket
%%
%% Note that the short (atom) addresses only work for some domains,
%% and that the nif will reject 'broadcast' for other domains than 'inet'
%%

-spec bind(Socket, Addr) -> {ok, Port} | {error, Reason} when
      Socket :: socket(),
      Addr   :: sockaddr() | any | broadcast | loopback,
      Port   :: port_number(),
      Reason :: inet:posix() | closed.

bind(?mk_socket(SockRef) = Socket, Addr) when is_reference(SockRef) ->
    if
        is_map(Addr) ->
            prim_socket:bind(SockRef, Addr);
        %%
        Addr =:= any;
        Addr =:= broadcast;
        Addr =:= loopback ->
            case prim_socket:getopt(SockRef, otp, domain) of
                {ok, Domain}
                  when Domain =:= inet;
                       Domain =:= inet6 ->
                    prim_socket:bind(
                      SockRef, #{family => Domain, addr => Addr});
                {ok, _Domain} ->
                    {error, eafnosupport};
                {error, _} = ERROR ->
                    ERROR
            end;
        %%
        true ->
            erlang:error(badarg, [Socket, Addr])
    end;
bind(Socket, Addr) ->
    erlang:error(badarg, [Socket, Addr]).


%% ===========================================================================
%%
%% bind - Add or remove a bind addresses on a socket
%%
%% Calling this function is only valid if the socket is: 
%%   type     = seqpacket
%%   protocol = sctp
%%
%% If the domain is inet, then all addresses *must* be IPv4.
%% If the domain is inet6, the addresses can be aither IPv4 or IPv6.
%%

-spec bind(Socket, Addrs, Action) -> ok | {error, Reason} when
      Socket :: socket(),
      Addrs  :: [sockaddr()],
      Action :: add | remove,
      Reason :: inet:posix() | closed.

bind(?mk_socket(SockRef), Addrs, Action)
  when is_reference(SockRef)
       andalso is_list(Addrs)
       andalso (Action =:= add
                orelse Action =:= remove) ->
    prim_socket:bind(SockRef, Addrs, Action);
bind(Socket, Addrs, Action) ->
    erlang:error(badarg, [Socket, Addrs, Action]).


%% ===========================================================================
%%
%% connect - initiate a connection on a socket
%%

-spec connect(Socket) -> ok | {error, Reason} when
    Socket   :: socket(),
    Reason   :: errcode() | closed.

%% Finalize connect after connect(,, nowait) and received
%% select message - see connect_deadline/3
%%
connect(?mk_socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:connect(SockRef);
connect(Socket) ->
    erlang:error(badarg, [Socket]).


-spec connect(Socket, SockAddr) -> ok | {error, Reason} when
    Socket   :: socket(),
    SockAddr :: sockaddr(),
    Reason   :: errcode() | closed.

connect(Socket, SockAddr) ->
    connect(Socket, SockAddr, infinity).


-spec connect(Socket, SockAddr, nowait) ->
           ok | {select, SelectInfo} | {error, Reason} when
    Socket     :: socket(),
    SockAddr   :: sockaddr(),
    SelectInfo :: select_info(),
    Reason     :: errcode() | closed;

             (Socket, SockAddr, Timeout) -> ok | {error, Reason} when
    Socket   :: socket(),
    SockAddr :: sockaddr(),
    Timeout  :: timeout(),
    Reason   :: errcode() | closed | timeout.

%% <KOLLA>
%% Is it possible to connect with family = local for the (dest) sockaddr?
%% </KOLLA>
connect(?mk_socket(SockRef) = Socket, SockAddr, Timeout)
  when is_reference(SockRef) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, SockAddr, Timeout]);
        nowait ->
            connect_nowait(SockRef, SockAddr);
        Deadline ->
            connect_deadline(SockRef, SockAddr, Deadline)
    end;
connect(Socket, SockAddr, Timeout) ->
    erlang:error(badarg, [Socket, SockAddr, Timeout]).

connect_nowait(SockRef, SockAddr) ->
    case prim_socket:connect(SockRef, SockAddr) of
        {select, Ref} ->
            ?SELECT(connect, Ref);
        Result ->
            Result
    end.

connect_deadline(SockRef, SockAddr, Deadline) ->
    case prim_socket:connect(SockRef, SockAddr) of
        {select, Ref} ->
            %% Connecting...
            Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(_Socket, select, Ref) ->
                    prim_socket:connect(SockRef);
                ?mk_socket_msg(_Socket, abort, {Ref, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    cancel(SockRef, connect, Ref),
                    {error, timeout}
            end;
        Result ->
            Result
    end.


%% ===========================================================================
%%
%% listen - listen for connections on a socket
%%

-spec listen(Socket) -> ok | {error, Reason} when
      Socket  :: socket(),
      Reason  :: inet:posix() | closed.

listen(Socket) ->
    listen(Socket, ?ESOCK_LISTEN_BACKLOG_DEFAULT).

-spec listen(Socket, Backlog) -> ok | {error, Reason} when
      Socket  :: socket(),
      Backlog :: integer(),
      Reason  :: inet:posix() | closed.

listen(?mk_socket(SockRef), Backlog)
  when is_reference(SockRef), is_integer(Backlog) ->
    prim_socket:listen(SockRef, Backlog);
listen(Socket, Backlog) ->
    erlang:error(badarg, [Socket, Backlog]).


%% ===========================================================================
%%
%% accept, accept4 - accept a connection on a socket
%%

-spec accept(LSocket) -> {ok, Socket} | {error, Reason} when
      LSocket :: socket(),
      Socket  :: socket(),
      Reason  :: errcode() | closed.

accept(Socket) ->
    accept(Socket, ?ESOCK_ACCEPT_TIMEOUT_DEFAULT).

-spec accept(LSocket, nowait) -> 
                    {ok, Socket} |
                    {select, SelectInfo} |
                    {error, Reason} when
      LSocket    :: socket(),
      Socket     :: socket(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

            (LSocket, Timeout) -> {ok, Socket} | {error, Reason} when
      LSocket    :: socket(),
      Timeout    :: timeout(),
      Socket     :: socket(),
      Reason     :: errcode() | closed | timeout.

accept(?mk_socket(LSockRef) = Socket, Timeout)
  when is_reference(LSockRef) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, Timeout]);
        nowait ->
            accept_nowait(LSockRef);
        Deadline ->
            accept_deadline(LSockRef, Deadline)
    end;
accept(Socket, Timeout) ->
    erlang:error(badarg, [Socket, Timeout]).

accept_nowait(LSockRef) ->
    AccRef = make_ref(),
    case prim_socket:accept(LSockRef, AccRef) of
        select ->
            ?SELECT(accept, AccRef);
        Result ->
            accept_result(LSockRef, AccRef, Result)
    end.

accept_deadline(LSockRef, Deadline) ->
    AccRef = make_ref(),
    case prim_socket:accept(LSockRef, AccRef) of
        select ->
            %% Each call is non-blocking, but even then it takes
            %% *some* time, so just to be sure, recalculate before 
            %% the receive.
	    Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(?mk_socket(LSockRef), select, AccRef) ->
                    accept_deadline(LSockRef, Deadline);
                ?mk_socket_msg(_Socket, abort, {AccRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    cancel(LSockRef, accept, AccRef),
                    {error, timeout}
            end;
        Result ->
            accept_result(LSockRef, AccRef, Result)
    end.

accept_result(LSockRef, AccRef, Result) ->
    case Result of
        {ok, SockRef} ->
            Socket = ?mk_socket(SockRef),
            {ok, Socket};
        {error, _} = ERROR ->
            cancel(LSockRef, accept, AccRef), % Just to be on the safe side...
            ERROR
    end.


%% ===========================================================================
%%
%% send, sendto, sendmsg - send a message on a socket
%%

-spec send(Socket, Data) -> ok | {error, Reason} when
      Socket  :: socket(),
      Data    :: iodata(),
      Reason  :: term().

send(Socket, Data) ->
    send(Socket, Data, ?ESOCK_SEND_FLAGS_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).

-spec send(Socket, Data, Flags) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: send_flags(),
      Reason     :: {errcode() | closed,
                     Remaining :: pos_integer()};

          (Socket, Data, Timeout :: nowait) ->
                  ok |
                  {ok, {binary(), SelectInfo}} |
                  {select, SelectInfo} |
                  {ok, {RestData, SelectInfo}} |
                  {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      RestData   :: binary(),
      SelectInfo :: select_info(),
      Reason     :: {errcode() | closed,
                     Remaining :: pos_integer()};

          (Socket, Data, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Timeout    :: timeout(),
      Reason     :: {errcode() | closed | timeout,
                     Remaining :: pos_integer()}.

send(Socket, Data, Flags) when is_list(Flags) ->
    send(Socket, Data, Flags, ?ESOCK_SEND_TIMEOUT_DEFAULT);
send(Socket, Data, Timeout) ->
    send(Socket, Data, ?ESOCK_SEND_FLAGS_DEFAULT, Timeout).

-spec send(Socket, Data, Flags, nowait) -> ok |
                                           {select, SelectInfo} |
                                           {ok, {RestData, SelectInfo}} |
                                           {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: send_flags(),
      RestData   :: binary(),
      SelectInfo :: select_info(),
      Reason     :: {errcode() | closed,
                     Remaining :: pos_integer()};

          (Socket, Data, Flags, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: send_flags(),
      Timeout    :: timeout(),
      Reason     :: {errcode() | closed | timeout,
                     Remaining :: pos_integer()}.

send(Socket, Data, Flags, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    send(Socket, Bin, Flags, Timeout);
send(?mk_socket(SockRef) = Socket, Data, Flags, Timeout)
  when is_reference(SockRef), is_binary(Data), is_list(Flags) ->
    To = undefined,
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, Data, Flags, Timeout]);
        nowait ->
            send_common_nowait(SockRef, Data, To, Flags, send);
        Deadline ->
            send_common_deadline(SockRef, Data, To, Flags, Deadline, send)
    end;
send(Socket, Data, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Data, Flags, Timeout]).

send_common_nowait(SockRef, Data, To, Flags, SendName) ->
    SendRef = make_ref(),
    case
        case SendName of
            send ->
                prim_socket:send(SockRef, SendRef, Data, Flags);
            sendto ->
                prim_socket:sendto(SockRef, SendRef, Data, To, Flags)
        end
    of
        {ok, Written} ->
	    %% We are partially done, but the user don't want to wait (here) 
            %% for completion
            <<_:Written/binary, Rest/binary>> = Data,
            {ok, {Rest, ?SELECT_INFO(SendName, SendRef)}};
        select ->
            ?SELECT(SendName, SendRef);
        Result ->
            send_common_result(Data, Result)
    end.

send_common_deadline(SockRef, Data, To, Flags, Deadline, SendName) ->
    SendRef = make_ref(),
    case
        case SendName of
            send ->
                prim_socket:send(SockRef, SendRef, Data, Flags);
            sendto ->
                prim_socket:sendto(SockRef, SendRef, Data, To, Flags)
        end
    of
        {ok, Written} ->
	    %% We are partially done, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(_Socket, select, SendRef)
                  when (Written > 0) -> 
                    <<_:Written/binary, Rest/binary>> = Data,
                    send_common_deadline(
                      SockRef, Rest, To, Flags, Deadline, SendName);
                ?mk_socket_msg(_Socket, select, SendRef) ->
                    send_common_deadline(
                      SockRef, Data, To, Flags, Deadline, SendName);
                ?mk_socket_msg(_Socket, abort, {SendRef, Reason}) ->
                    {error, {Reason, byte_size(Data)}}
            after Timeout ->
                    _ = cancel(SockRef, SendName, SendRef),
                    {error, {timeout, byte_size(Data)}}
            end;
        select ->
            %% Wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(_Socket, select, SendRef) ->
                    send_common_deadline(
                      SockRef, Data, To, Flags, Deadline, SendName);
                ?mk_socket_msg(_Socket, abort, {SendRef, Reason}) ->
                    {error, {Reason, byte_size(Data)}}
            after Timeout ->
                    _ = cancel(SockRef, SendName, SendRef),
                    {error, {timeout, byte_size(Data)}}
            end;
        %%
        {error, ealready = Reason} ->
            %% Internal error:
            %%   we called send, got eagain, and called send again
            %%   - without waiting for select message
            erlang:error(Reason);
        Result ->
            send_common_result(Data, Result)
    end.

send_common_result(Data, Result) ->
    case Result of
        ok ->
            ok;
        {error, Reason} ->
            {error, {Reason, byte_size(Data)}}
    end.


%% ---------------------------------------------------------------------------
%%

-spec sendto(Socket, Data, Dest) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Data   :: binary(),
      Dest   :: sockaddr(),
      Reason :: {errcode() | closed,
                     Remaining :: pos_integer()}.

sendto(Socket, Data, Dest) ->
    sendto(Socket, Data, Dest, ?ESOCK_SENDTO_FLAGS_DEFAULT).

-spec sendto(Socket, Data, Dest, Flags) ->
                    ok | {error, Reason} when
      Socket    :: socket(),
      Data      :: binary(),
      Dest      :: sockaddr(),
      Flags     :: send_flags(),
      Reason    :: {errcode() | closed,
                     Remaining :: pos_integer()};

            (Socket, Data, Dest, Timeout :: nowait) ->
                    ok |
                    {select, SelectInfo} |
                    {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      SelectInfo :: select_info(),
      Reason     :: {errcode() | closed,
                     Remaining :: pos_integer()};

            (Socket, Data, Dest, Timeout) ->
                    ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Timeout    :: timeout(),
      Reason     :: {errcode() | closed | timeout,
                     Remaining :: pos_integer()}.

sendto(Socket, Data, Dest, Flags) when is_list(Flags) ->
    sendto(Socket, Data, Dest, Flags, ?ESOCK_SENDTO_TIMEOUT_DEFAULT);
sendto(Socket, Data, Dest, Timeout) ->
    sendto(Socket, Data, Dest, ?ESOCK_SENDTO_FLAGS_DEFAULT, Timeout).


-spec sendto(Socket, Data, Dest, Flags, nowait) ->
                    ok |
                    {ok, {binary(), SelectInfo}} |
                    {select, SelectInfo} |
                    {error, Reason} when
      Socket     :: socket(),
      Data       :: binary(),
      Dest       :: sockaddr(),
      Flags      :: send_flags(),
      SelectInfo :: select_info(),
      Reason     :: {errcode() | closed,
                     Remaining :: pos_integer()};

            (Socket, Data, Dest, Flags, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: binary(),
      Dest       :: sockaddr(),
      Flags      :: send_flags(),
      Timeout    :: timeout(),
      Reason     :: {errcode() | closed | timeout,
                     Remaining :: pos_integer()}.

sendto(Socket, Data, Dest, Flags, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    sendto(Socket, Bin, Dest, Flags, Timeout);
sendto(?mk_socket(SockRef) = Socket, Data, Dest, Flags, Timeout)
  when is_reference(SockRef), is_binary(Data), is_list(Flags) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, Data, Dest, Flags, Timeout]);
        nowait ->
            send_common_nowait(SockRef, Data, Dest, Flags, sendto);
        Deadline ->
            send_common_deadline(
              SockRef, Data, Dest, Flags, Deadline, sendto)
    end;
sendto(Socket, Data, Dest, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Data, Dest, Flags, Timeout]).


%% ---------------------------------------------------------------------------
%%
%% The only part of the msghdr() that *must* exist (a connected
%% socket need not specify the addr field) is the iov.
%% The ctrl field is optional, and the addr and flags are not
%% used when sending.
%%

-spec sendmsg(Socket, MsgHdr) ->
                     ok |
                     {ok, Remaining} |
                     {error, Reason} when
      Socket  :: socket(),
      MsgHdr  :: msghdr(),
      Remaining :: erlang:iovec(),
      Reason  :: term().

sendmsg(Socket, MsgHdr) ->
    sendmsg(Socket, MsgHdr,
            ?ESOCK_SENDMSG_FLAGS_DEFAULT, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT).


-spec sendmsg(Socket, MsgHdr, Flags) -> ok | {error, Reason} when
      Socket  :: socket(),
      MsgHdr  :: msghdr(),
      Flags   :: send_flags(),
      Reason  :: errcode() | closed;

             (Socket, MsgHdr, Timeout :: nowait) ->
                     ok |
                     {ok, Remaining} |
                     {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Remaining :: erlang:iovec(),
      Reason     :: errcode() | closed;

             (Socket, MsgHdr, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Timeout    :: timeout(),
      Reason     :: errcode() | closed | timeout.

sendmsg(Socket, MsgHdr, Flags) when is_list(Flags) ->
    sendmsg(Socket, MsgHdr, Flags, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT);
sendmsg(Socket, MsgHdr, Timeout) ->
    sendmsg(Socket, MsgHdr, ?ESOCK_SENDMSG_FLAGS_DEFAULT, Timeout).


-spec sendmsg(Socket, MsgHdr, Flags, nowait) -> 
                     ok |
                     {ok, Remaining} |
                     {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Flags      :: send_flags(),
      Remaining  :: erlang:iovec(),
      Reason     :: errcode() | closed;

             (Socket, MsgHdr, Flags, Timeout) -> 
                     ok |
                     {ok, Remaining} |
                     {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Flags      :: send_flags(),
      Timeout    :: timeout(),
      Remaining  :: erlang:iovec(),
      Reason     :: errcode() | closed | timeout.

sendmsg(?mk_socket(SockRef) = Socket, MsgHdr, Flags, Timeout)
  when is_reference(SockRef), is_map(MsgHdr), is_list(Flags) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, MsgHdr, Flags, Timeout]);
        Deadline ->
            sendmsg_loop(SockRef, MsgHdr, Flags, Deadline)
    end;
sendmsg(Socket, MsgHdr, Flags, Timeout) ->
    erlang:error(badarg, [Socket, MsgHdr, Flags, Timeout]).

sendmsg_loop(SockRef, MsgHdr, Flags, Deadline) ->
    SendRef = make_ref(),
    case prim_socket:sendmsg(SockRef, SendRef, MsgHdr, Flags) of
        ok ->
            %% We are done
            ok;
        %%
        {ok, Written} when is_integer(Written) andalso (Written > 0) ->
            %% We should not retry here since the protocol may not
            %% be able to handle a message being split. Leave it to
            %% the caller to figure out (call again with the rest).
            %%
            %% We need to cancel this partial write.
            %%
            _ = cancel(SockRef, sendmsg, SendRef),
            {ok, sendmsg_rest(maps:get(iov, MsgHdr), Written)};
        %%
        select when (Deadline =:= nowait) ->
            ?SELECT(sendmsg, SendRef);
        select ->
	    Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(?mk_socket(SockRef), select, SendRef) ->
                    sendmsg_loop(SockRef, MsgHdr, Flags, Deadline);
                ?mk_socket_msg(_Socket, abort, {SendRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, sendmsg, SendRef),
                    {error, timeout}
            end;
        %%
        {error, ealready = Reason} when Deadline =/= nowait ->
            %% Internal error:
            %%   we called send, got eagain, and called send again
            %%   - without waiting for select message
            erlang:error(Reason);
        {error, _} = ERROR ->
            ERROR
    end.

sendmsg_rest([B|IOVec], Written) when Written >= byte_size(B) ->
    sendmsg_rest(IOVec, Written - byte_size(B));
sendmsg_rest([B|IOVec], Written) ->
    <<_:Written/binary, Rest/binary>> = B,
    [Rest|IOVec].


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

-spec recv(Socket) ->
                  {ok, Data} |
                  {error, Reason} when
      Socket :: socket(),
      Data   :: binary(),
      Reason ::
        errcode() | closed |
        {errcode() | closed, Data :: binary()}.
                          
recv(Socket) ->
    recv(Socket, 0).

-spec recv(Socket, Length) ->
                  {ok, Data} |
                  {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Data   :: binary(),
      Reason ::
        errcode() | closed |
        {errcode() | closed, Data :: binary()}.

recv(Socket, Length) ->
    recv(Socket, Length,
         ?ESOCK_RECV_FLAGS_DEFAULT,
         ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recv(Socket, Length, Flags) ->
                  {ok, Data} |
                  {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Flags  :: recv_flags(),
      Data   :: binary(),
      Reason ::
        errcode() | closed |
        {errcode() | closed, Data :: binary()};

          (Socket, Length, Timeout :: nowait) ->
                  {ok, Data} |
                  {ok, {Data, SelectInfo}} |
                  {select, SelectInfo} |
                  {error, Reason} when
      Socket     :: socket(),
      Length     :: non_neg_integer(),
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     ::
        errcode() | closed |
        {errcode() | closed, Data :: binary()};

          (Socket, Length, Timeout) ->
                  {ok, Data} |
                  {error, Reason} when
      Socket  :: socket(),
      Length  :: non_neg_integer(),
      Timeout :: timeout(),
      Data    :: binary(),
      Reason  ::
        errcode() | closed | timeout |
        {errcode() | closed | timeout, Data :: binary()}.

recv(Socket, Length, Flags) when is_list(Flags) ->
    recv(Socket, Length, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length, Timeout) ->
    recv(Socket, Length, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).

-spec recv(Socket, Length, Flags, nowait) ->
                  {ok, Data} |
                  {ok, {Data, SelectInfo}} |
                  {select, SelectInfo} |
                  {error, Reason} when
      Socket     :: socket(),
      Length     :: non_neg_integer(),
      Flags      :: recv_flags(),
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     ::
        errcode() | closed |
        {errcode() | closed, Data :: binary()};

          (Socket, Length, Flags, Timeout) ->
                  {ok, Data} |
                  {error, Reason} when
      Socket     :: socket(),
      Length     :: non_neg_integer(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      Data       :: binary(),
      Reason     ::
        errcode() | closed | timeout |
        {errcode() | closed | timeout, Data :: binary()}.

recv(?mk_socket(SockRef) = Socket, Length, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(Length), Length >= 0,
       is_list(Flags) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, Length, Flags, Timeout]);
        nowait ->
            recv_nowait(SockRef, Length, Flags, <<>>);
        Deadline ->
            recv_deadline(SockRef, Length, Flags, Deadline, <<>>)
    end;
recv(Socket, Length, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Length, Flags, Timeout]).

%% We will only recurse with Length == 0 if Length is 0,
%% so Length == 0 means to return all available data also when recursing

recv_nowait(SockRef, Length, Flags, Acc) ->
    RecvRef = make_ref(),
    case prim_socket:recv(SockRef, RecvRef, Length, Flags) of
        {more, Bin} ->
            %% We got what we requested but will not waste more time
            %% although there might be more data available
            {ok, bincat(Acc, Bin)};
        {select, Bin} ->
            %% We got less than requested so the caller will
            %% get a select message when there might be more to read
            {ok, {bincat(Acc, Bin), ?SELECT_INFO(recv, RecvRef)}};
        select ->
            %% The caller will get a select message when there
            %% might me data to read
            if
                byte_size(Acc) =:= 0 ->
                    ?SELECT(recv, RecvRef);
                true ->
                    {ok, {Acc, ?SELECT_INFO(recv, RecvRef)}}
            end;
        Result ->
            recv_result(Acc, Result)
    end.

recv_deadline(SockRef, Length, Flags, Deadline, Acc) ->
    RecvRef = make_ref(),
    case prim_socket:recv(SockRef, RecvRef, Length, Flags) of
        {more, Bin} ->
            %% There is more data readily available
            %% - repeat unless time's up
            Timeout = timeout(Deadline),
            if
                0 < Timeout ->
                    %% Recv more
                    recv_deadline(
                      SockRef, Length, Flags, Deadline, bincat(Acc, Bin));
                true ->
                    {ok, bincat(Acc, Bin)}
            end;
        %%
        {select, Bin} ->
            %% We got less than requested
	    Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(?mk_socket(SockRef), select, RecvRef) ->
                    if
                        0 < Timeout ->
                            %% Recv more
                            recv_deadline(
                              SockRef, Length - byte_size(Bin), Flags,
                              Deadline, bincat(Acc, Bin));
                        true ->
                            {error, {timeout, bincat(Acc, Bin)}}
                    end;
                ?mk_socket_msg(_Socket, abort, {RecvRef, Reason}) ->
                    {error, {Reason, bincat(Acc, Bin)}}
            after Timeout ->
                    cancel(SockRef, recv, RecvRef),
                    {error, {timeout, bincat(Acc, Bin)}}
            end;
        %%
        select when Length =:= 0, 0 < byte_size(Acc) ->
            %% We first got some data and are then asked to wait,
            %% but we only want the first that comes
            %% - cancel and return what we have
            cancel(SockRef, recv, RecvRef),
            {ok, Acc};
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(?mk_socket(SockRef), select, RecvRef) ->
                    if
                        0 < Timeout ->
                            %% Retry
                            recv_deadline(
                              SockRef, Length, Flags, Deadline, Acc);
                        true ->
                            recv_error(Acc, timeout)
                    end;
                ?mk_socket_msg(_Socket, abort, {RecvRef, Reason}) ->
                    recv_error(Acc, Reason)
            after Timeout ->
                    cancel(SockRef, recv, RecvRef),
                    recv_error(Acc, timeout)
            end;
        %%
        {error, ealready = Reason} ->
            %% Internal error:
            %%   we called recv, got eagain, and called recv again
            %%   - without waiting for select message
            erlang:error(Reason);
        Result ->
            recv_result(Acc, Result)
    end.

recv_result(Acc, Result) ->
    case Result of
        {ok, Bin} ->
            {ok, bincat(Acc, Bin)};
        {error, _} = ERROR when byte_size(Acc) =:= 0 ->
            ERROR;
        {error, Reason} ->
            {error, {Reason, Acc}}
    end.

recv_error(Acc, Reason) ->
    if
        byte_size(Acc) =:= 0 ->
            {error, Reason};
        true ->
            {error, {Reason, Acc}}
    end.

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
%% "consumed" from the underlying (OS) buffers, so another recvfrom call
%% is needed, possibly with a then adjusted buffer size.
%%

-spec recvfrom(Socket) -> {ok, {Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      Source    :: sockaddr() | undefined,
      Data      :: binary(),
      Reason    :: errcode() | closed.

recvfrom(Socket) ->
    recvfrom(Socket, 0).

-spec recvfrom(Socket, BufSz) -> {ok, {Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Source    :: sockaddr() | undefined,
      Data      :: binary(),
      Reason    :: errcode() | closed.

recvfrom(Socket, BufSz) ->
    recvfrom(Socket, BufSz,
             ?ESOCK_RECV_FLAGS_DEFAULT,
             ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recvfrom(Socket, Flags, nowait) -> 
                      {ok, {Source, Data}} |
                      {select, SelectInfo} |
                      {error, Reason} when
      Socket     :: socket(),
      Flags      :: recv_flags(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

              (Socket, Flags, Timeout) -> 
                      {ok, {Source, Data}} |
                      {error, Reason} when
      Socket     :: socket(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      Reason     :: errcode() | closed | timeout;

              (Socket, BufSz, Flags) -> 
                      {ok, {Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Flags     :: recv_flags(),
      Source    :: sockaddr() | undefined,
      Data      :: binary(),
      Reason    :: errcode() | closed;

              (Socket, BufSz, nowait) -> 
                      {ok, {Source, Data}} |
                      {select, SelectInfo} |
                      {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

              (Socket, BufSz, Timeout) -> 
                      {ok, {Source, Data}} | {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      Timeout    :: timeout(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      Reason     :: errcode() | closed | timeout.

recvfrom(Socket, Flags, Timeout) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, Timeout);
recvfrom(Socket, BufSz, Flags) when is_list(Flags) ->
    recvfrom(Socket, BufSz, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz, Timeout) ->
    recvfrom(Socket, BufSz, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).

-spec recvfrom(Socket, BufSz, Flags, nowait) -> 
                      {ok, {Source, Data}} |
                      {select, SelectInfo} |
                      {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      Flags      :: recv_flags(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

              (Socket, BufSz, Flags, Timeout) -> 
                      {ok, {Source, Data}} |
                      {error, Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Flags   :: recv_flags(),
      Timeout :: timeout(),
      Source  :: sockaddr() | undefined,
      Data    :: binary(),
      Reason  :: errcode() | closed | timeout.

recvfrom(?mk_socket(SockRef) = Socket, BufSz, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(BufSz), 0 =< BufSz,
       is_list(Flags) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, BufSz, Flags, Timeout]);
        nowait ->
            recvfrom_nowait(SockRef, BufSz, Flags);
        Deadline ->
            recvfrom_deadline(SockRef, BufSz, Flags, Deadline)
    end;
recvfrom(Socket, BufSz, Flags, Timeout) ->
    erlang:error(badarg, [Socket, BufSz, Flags, Timeout]).

recvfrom_nowait(SockRef, BufSz, Flags) ->
    RecvRef = make_ref(),
    case prim_socket:recvfrom(SockRef, RecvRef, BufSz, Flags) of
        select ->
            ?SELECT(recvfrom, RecvRef);
        Result ->
            recvfrom_result(Result)
    end.

recvfrom_deadline(SockRef, BufSz, Flags, Deadline) ->
    RecvRef = make_ref(),
    case prim_socket:recvfrom(SockRef, RecvRef, BufSz, Flags) of
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(?mk_socket(SockRef), select, RecvRef) ->
                    recvfrom_deadline(SockRef, BufSz, Flags, Deadline);
                ?mk_socket_msg(_Socket, abort, {RecvRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    cancel(SockRef, recvfrom, RecvRef),
                    {error, timeout}
            end;
        {error, ealready = Reason} ->
            %% Internal error:
            %%   we called recvfrom, got eagain, and called recvfrom again
            %%   - without waiting for select message
            erlang:error(Reason);
        Result ->
            recvfrom_result(Result)
    end.

recvfrom_result(Result) ->
    case Result of
        {ok, {_Source, _NewData}} = OK ->
            OK;
        {error, _Reason} = ERROR ->
            ERROR
    end.


%% ---------------------------------------------------------------------------
%%

-spec recvmsg(Socket) -> {ok, MsgHdr} | {error, Reason} when
      Socket  :: socket(),
      MsgHdr  :: msghdr(),
      Reason  :: errcode() | closed.

recvmsg(Socket) ->
    recvmsg(Socket, 0, 0,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recvmsg(Socket, Flags) -> {ok, MsgHdr} | {error, Reason} when
      Socket  :: socket(),
      Flags   :: recv_flags(),
      MsgHdr  :: msghdr(),
      Reason  :: errcode() | closed;

             (Socket, Timeout :: nowait) -> {ok, MsgHdr} |
                                            {select, SelectInfo} |
                                            {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

             (Socket, Timeout) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      Timeout    :: timeout(),
      MsgHdr     :: msghdr(),
      Reason     :: errcode() | closed | timeout.

recvmsg(Socket, Flags) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvmsg(Socket, Timeout) ->
    recvmsg(Socket, 0, 0, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).

-spec recvmsg(Socket, Flags, nowait) -> {ok, MsgHdr} |
                                        {select, SelectInfo} |
                                        {error, Reason} when
      Socket     :: socket(),
      Flags      :: recv_flags(),
      MsgHdr     :: msghdr(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

             (Socket, Flags, Timeout) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      MsgHdr     :: msghdr(),
      Reason     :: errcode() | closed | timeout;

             (Socket, BufSz, CtrlSz) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      CtrlSz     :: non_neg_integer(),
      MsgHdr     :: msghdr(),
      Reason     :: errcode() | closed.

recvmsg(Socket, Flags, Timeout) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, Timeout);
recvmsg(Socket, BufSz, CtrlSz) when is_integer(BufSz), is_integer(CtrlSz) ->
    recvmsg(Socket, BufSz, CtrlSz,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).


-spec recvmsg(Socket, BufSz, CtrlSz, Flags, nowait) ->
                     {ok, MsgHdr} |
                     {select, SelectInfo} |
                     {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      CtrlSz     :: non_neg_integer(),
      Flags      :: recv_flags(),
      MsgHdr     :: msghdr(),
      SelectInfo :: select_info(),
      Reason     :: errcode() | closed;

             (Socket, BufSz, CtrlSz, Flags, Timeout) ->
                     {ok, MsgHdr} |
                     {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      CtrlSz     :: non_neg_integer(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      MsgHdr     :: msghdr(),
      Reason     :: errcode() | closed | timeout.

recvmsg(?mk_socket(SockRef) = Socket, BufSz, CtrlSz, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(BufSz), 0 =< BufSz,
       is_integer(CtrlSz), 0 =< CtrlSz,
       is_list(Flags) ->
    case deadline(Timeout) of
        badarg = Reason ->
            erlang:error(Reason, [Socket, BufSz, CtrlSz, Flags, Timeout]);
        nowait ->
            recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags);
        Deadline ->
            recvmsg_deadline(SockRef, BufSz, CtrlSz, Flags, Deadline)
    end;
recvmsg(Socket, BufSz, CtrlSz, Flags, Timeout) ->
    erlang:error(badarg, [Socket, BufSz, CtrlSz, Flags, Timeout]).

recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags)  ->
    RecvRef = make_ref(),
    case prim_socket:recvmsg(SockRef, RecvRef, BufSz, CtrlSz, Flags) of
        select ->
            ?SELECT(recvmsg, RecvRef);
        Result ->
            recvmsg_result(Result)
    end.

recvmsg_deadline(SockRef, BufSz, CtrlSz, Flags, Deadline)  ->
    RecvRef = make_ref(),
    case prim_socket:recvmsg(SockRef, RecvRef, BufSz, CtrlSz, Flags) of
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?mk_socket_msg(?mk_socket(SockRef), select, RecvRef) ->
                    recvmsg_deadline(
                      SockRef, BufSz, CtrlSz, Flags, Deadline);
                ?mk_socket_msg(_Socket, abort, {RecvRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    cancel(SockRef, recvmsg, RecvRef),
                    {error, timeout}
            end;
        %%
        {error, ealready = Reason} ->
            %% Internal error:
            %%   we called recvmsg, got eagain, and called recvmsg again
            %%   - without waiting for select message
            erlang:error(Reason);
        Result ->
            recvmsg_result(Result)
    end.

recvmsg_result(Result) ->
    case Result of
        {ok, _MsgHdr} = OK ->
            OK;
        {error, _Reason} = ERROR ->
            ERROR
    end.


%% ===========================================================================
%%
%% close - close a file descriptor
%%
%% Closing a socket is a two stage rocket (because of linger).
%% We need to perform the actual socket close while in BLOCKING mode.
%% But that would hang the entire VM, so what we do is divide the 
%% close in two steps: 
%% 1) prim_socket:nif_close + the socket_stop (nif) callback function
%%    This is for everything that can be done safely NON-BLOCKING.
%% 2) prim_socket:nif_finalize_close which is executed by a *dirty* scheduler
%%    Before we call the socket close function, we set the socket 
%%    BLOCKING. Thereby linger is handled properly.

-spec close(Socket) -> ok | {error, Reason} when
      Socket :: socket(),
      Reason :: errcode() | closed | timeout.

close(?mk_socket(SockRef))
  when is_reference(SockRef) ->
    case prim_socket:close(SockRef) of
        ok ->
            prim_socket:finalize_close(SockRef);
        {ok, CloseRef} ->
            %% We must wait for the socket_stop callback function to 
            %% complete its work
            receive
                ?mk_socket_msg(?mk_socket(SockRef), close, CloseRef) ->
                    prim_socket:finalize_close(SockRef)
            end;
        {error, _} = ERROR ->
            ERROR
    end;
close(Socket) ->
    erlang:error(badarg, [Socket]).



%% ===========================================================================
%%
%% shutdown - shut down part of a full-duplex connection
%%

-spec shutdown(Socket, How) -> ok | {error, Reason} when
      Socket :: socket(),
      How    :: shutdown_how(),
      Reason :: inet:posix() | closed.

shutdown(?mk_socket(SockRef), How)
  when is_reference(SockRef) ->
    prim_socket:shutdown(SockRef, How);
shutdown(Socket, How) ->
    erlang:error(badarg, [Socket, How]).


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

-spec setopt(Socket, otp, otp_socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: errcode() | closed | not_owner;

            (Socket, socket, socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, ip, ip_socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, ipv6, ipv6_socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, tcp, tcp_socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, udp, udp_socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, sctp, sctp_socket_option(), Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, Level, Key, Value) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Level  :: non_neg_integer(),
      Key    :: non_neg_integer(),
      Value  :: binary(),
      Reason :: inet:posix() | closed.

setopt(?mk_socket(SockRef), Level, Key, Value)
  when is_reference(SockRef) ->
    prim_socket:setopt(SockRef, Level, Key, Value);
setopt(Socket, Level, Key, Value) ->
    erlang:error(badarg, [Socket, Level, Key, Value]).


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

-spec getopt(Socket, otp, otp_socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: einval | closed;

            (Socket, socket, socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, ip, ip_socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, ipv6, ipv6_socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, tcp, tcp_socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, udp, udp_socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, sctp, sctp_socket_option()) ->
                    {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: inet:posix() | closed;

            (Socket, Level, Key) ->
                    ok | {ok, Value} | {error, Reason} when
      Socket    :: socket(),
      Level     :: integer(),
      Key       :: {NativeOpt, ValueSize},
      NativeOpt :: integer(),
      ValueSize :: int | bool | non_neg_integer(),
      Value     :: term(),
      Reason    :: inet:posix() | closed.

getopt(?mk_socket(SockRef), Level, Key)
  when is_reference(SockRef) ->
    prim_socket:getopt(SockRef, Level, Key);
getopt(Socket, Level, Key) ->
    erlang:error(badarg, [Socket, Level, Key]).


%% ===========================================================================
%%
%% sockname - return the current address of the socket.
%%
%%

-spec sockname(Socket) -> {ok, SockAddr} | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: inet:posix() | closed.

sockname(?mk_socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:sockname(SockRef);
sockname(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% peername - return the address of the peer *connected* to the socket.
%%
%%

-spec peername(Socket) -> {ok, SockAddr} | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: inet:posix() | closed.

peername(?mk_socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:peername(SockRef);
peername(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% cancel - cancel an operation resulting in a select
%%
%% A call to accept, recv/recvfrom/recvmsg and send/sendto/sendmsg
%% can result in a select if they are called with the Timeout argument
%% set to nowait. This is indicated by the return of the select-info.
%% Such a operation can be cancelled by calling this function.
%%

-spec cancel(Socket, SelectInfo) -> ok | {error, Reason} when
      Socket     :: socket(),
      SelectInfo :: select_info(),
      Reason     :: einval | closed | exself.

cancel(?mk_socket(SockRef), ?SELECT_INFO(Tag, Ref))
  when is_reference(SockRef) ->
    cancel(SockRef, Tag, Ref);
cancel(Socket, SelectInfo) ->
    erlang:error(badarg, [Socket, SelectInfo]).


cancel(SockRef, Op, OpRef) ->
    case prim_socket:cancel(SockRef, Op, OpRef) of
        %% The select has already completed
        {error, select_sent} ->
            flush_select_msg(SockRef, OpRef),
            _ = flush_abort_msg(SockRef, OpRef),
            ok;
        {error, not_found} ->
            _ = flush_abort_msg(SockRef, OpRef),
            {error, einval};
        Other ->
            _ = flush_abort_msg(SockRef, OpRef),
            Other
    end.

flush_select_msg(SockRef, Ref) ->
    receive
        ?mk_socket_msg(?mk_socket(SockRef), select, Ref) ->
            ok
    after 0 ->
            ok
    end.

flush_abort_msg(SockRef, Ref) ->
    receive
        ?mk_socket_msg(?mk_socket(SockRef), abort, {Ref, Reason}) ->
            Reason
    after 0 ->
            ok
    end.


%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

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


deadline(Timeout) ->
    case Timeout of
        nowait ->
            Timeout;
        infinity ->
            Timeout;
        0 ->
            zero;
        _ when is_integer(Timeout), 0 < Timeout ->
            timestamp() + Timeout;
        _ ->
            badarg
    end.

timeout(Deadline) ->
    case Deadline of
        infinity ->
            Deadline;
        zero ->
            0;
        _ ->
            Now = timestamp(),
            if
                Deadline > Now ->
                    Deadline - Now;
                true ->
                    0
            end
    end.

timestamp() ->
    erlang:monotonic_time(milli_seconds).


-compile({inline, [bincat/2]}).
bincat(<<>>, <<_/binary>> = B) -> B;
bincat(<<_/binary>> = A, <<>>) -> A;
bincat(<<_/binary>> = A, <<_/binary>> = B) ->
    <<A/binary, B/binary>>.


%% p(F) ->
%%     p(F, []).

%% p(F, A) ->
%%     p(get(sname), F, A).

%% p(undefined, F, A) ->
%%     p("***", F, A);
%% p(SName, F, A) ->
%%     TS = formated_timestamp(),
%%     io:format(user,"[~s][~s,~p] " ++ F ++ "~n", [TS, SName, self()|A]),
%%     io:format("[~s][~s,~p] " ++ F ++ "~n", [TS, SName, self()|A]).
