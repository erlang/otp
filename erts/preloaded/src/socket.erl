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

-module(socket).

-compile(no_native).
-compile({no_auto_import,[error/1]}).

%% Administrative and "global" utility functions
-export([
	 on_load/0, on_load/1,

         number_of/0,
         which_sockets/0, which_sockets/1,

         ensure_sockaddr/1,

         debug/1,
         %% command/1,
	 info/0, info/1,
         supports/0, supports/1, supports/2, supports/3
        ]).

-export([
         open/2, open/3, open/4,
         bind/2, bind/3,
         connect/2, connect/3,
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
              select_tag/0,
              select_ref/0,
              select_info/0,

              socket_counters/0,
              socket_counter/0,
              socket_info/0,

              %% command/0,

              domain/0,
              type/0,
              protocol/0,
              socket/0,

              port_number/0,
              ip_address/0,
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


-define(REGISTRY, socket_registry).


%% The command type has the general form: 
%% #{
%%   command := atom(),
%%   data    := term()
%%  }
%% But only certain values are actually valid, so the type gets the form:
-type debug_command() :: #{
                           command := debug,
                           data    := boolean()
                          }.
%% -type command() :: debug_command().

-type socket_counters() :: [{socket_counter(), non_neg_integer()}].
-type socket_counter()  :: read_byte | read_fails | read_pkg | read_pkg_max |
                           read_tries | read_waits |
                           write_byte | write_fails | write_pkg | write_pkg_max |
                           write_tries | write_waits |
                           acc_success | acc_fails | acc_tries | acc_waits.
-type socket_info() :: #{domain        := domain(),
                         type          := type(),
                         protocol      := protocol(),
                         ctrl          := pid(),
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

-type ip_address() :: ip4_address() | ip6_address().

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

-define(SOCKADDR_IN4_DEFAULTS(A), #{port => 0,
                                    addr => A}).
-define(SOCKADDR_IN4_DEFAULTS,    ?SOCKADDR_IN4_DEFAULTS(any)).
-define(SOCKADDR_IN4_DEFAULT(A),  (?SOCKADDR_IN4_DEFAULTS(A))#{family => inet}).
-define(SOCKADDR_IN6_DEFAULTS(A), #{port     => 0,
                                    addr     => A,
                                    flowinfo => 0, 
                                    scope_id => 0}).
-define(SOCKADDR_IN6_DEFAULTS,    ?SOCKADDR_IN6_DEFAULTS(any)).
-define(SOCKADDR_IN6_DEFAULT(A),  (?SOCKADDR_IN6_DEFAULTS(A))#{family => inet6}).

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
                             iow |
                             controlling_process |
                             rcvbuf | % sndbuf |
                             rcvctrlbuf |
                             sndctrlbuf |
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

-record(socket, {ref :: reference()}).

-opaque socket() :: #socket{}.

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

-opaque select_tag() :: atom().
-opaque select_ref() :: reference().

%% -record(select_info, {tag :: select_tag(), ref :: select_ref()}).

-type select_info() :: {select_info, select_tag(), select_ref()}.

-define(SELECT_INFO(T, R), {select_info, T, R}).
-define(SELECT(T, R),      {select, ?SELECT_INFO(T, R)}).


%% This is used in messages sent from the nif-code to erlang processes:
%%
%%         {?ESOCK_TAG, Socket :: socket(), Tag :: atom(), Info :: term()}
%%
-define(ESOCK_TAG, '$socket').

-define(ESOCK_DOMAIN_LOCAL, 1).
-define(ESOCK_DOMAIN_UNIX,  ?ESOCK_DOMAIN_LOCAL).
-define(ESOCK_DOMAIN_INET,  2).
-define(ESOCK_DOMAIN_INET6, 3).

-define(ESOCK_TYPE_STREAM,    1).
-define(ESOCK_TYPE_DGRAM,     2).
-define(ESOCK_TYPE_RAW,       3).
%% -define(ESOCK_TYPE_RDM,       4).
-define(ESOCK_TYPE_SEQPACKET, 5).

-define(ESOCK_PROTOCOL_DEFAULT, 0).
-define(ESOCK_PROTOCOL_IP,      1).
-define(ESOCK_PROTOCOL_TCP,     2).
-define(ESOCK_PROTOCOL_UDP,     3).
-define(ESOCK_PROTOCOL_SCTP,    4).
-define(ESOCK_PROTOCOL_ICMP,    5).
-define(ESOCK_PROTOCOL_IGMP,    6).

-define(ESOCK_LISTEN_BACKLOG_DEFAULT, 5).

-define(ESOCK_ACCEPT_TIMEOUT_DEFAULT, infinity).

-define(ESOCK_SEND_FLAG_CONFIRM,    0).
-define(ESOCK_SEND_FLAG_DONTROUTE,  1).
-define(ESOCK_SEND_FLAG_EOR,        2).
-define(ESOCK_SEND_FLAG_MORE,       3).
-define(ESOCK_SEND_FLAG_NOSIGNAL,   4).
-define(ESOCK_SEND_FLAG_OOB,        5).

-define(ESOCK_SEND_FLAGS_DEFAULT,      []).
-define(ESOCK_SEND_TIMEOUT_DEFAULT,    infinity).
-define(ESOCK_SENDTO_FLAGS_DEFAULT,    []).
-define(ESOCK_SENDTO_TIMEOUT_DEFAULT,  ?ESOCK_SEND_TIMEOUT_DEFAULT).
-define(ESOCK_SENDMSG_FLAGS_DEFAULT,   []).
-define(ESOCK_SENDMSG_TIMEOUT_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).

-define(ESOCK_RECV_FLAG_CMSG_CLOEXEC, 0).
-define(ESOCK_RECV_FLAG_ERRQUEUE,     1).
-define(ESOCK_RECV_FLAG_OOB,          2).
-define(ESOCK_RECV_FLAG_PEEK,         3).
-define(ESOCK_RECV_FLAG_TRUNC,        4).

-define(ESOCK_RECV_FLAGS_DEFAULT,   []).
-define(ESOCK_RECV_TIMEOUT_DEFAULT, infinity).

-define(ESOCK_OPT_LEVEL_OTP,            0).
-define(ESOCK_OPT_LEVEL_SOCKET,         1).
-define(ESOCK_OPT_LEVEL_IP,             2).
-define(ESOCK_OPT_LEVEL_IPV6,           3).
-define(ESOCK_OPT_LEVEL_TCP,            4).
-define(ESOCK_OPT_LEVEL_UDP,            5).
-define(ESOCK_OPT_LEVEL_SCTP,           6).

%% *** OTP (socket) options
-define(ESOCK_OPT_OTP_DEBUG,            1).
-define(ESOCK_OPT_OTP_IOW,              2).
-define(ESOCK_OPT_OTP_CTRL_PROC,        3).
-define(ESOCK_OPT_OTP_RCVBUF,           4).
%%-define(ESOCK_OPT_OTP_SNDBUF,           5).
-define(ESOCK_OPT_OTP_RCVCTRLBUF,       6).
-define(ESOCK_OPT_OTP_SNDCTRLBUF,       7).
-define(ESOCK_OPT_OTP_FD,               8).
-define(ESOCK_OPT_OTP_META,             9).
-define(ESOCK_OPT_OTP_DOMAIN,           16#FF01). % INTERNAL
-define(ESOCK_OPT_OTP_TYPE,             16#FF02). % INTERNAL
-define(ESOCK_OPT_OTP_PROTOCOL,         16#FF03). % INTERNAL
-define(ESOCK_OPT_OTP_DTP,              16#FF04). % INTERNAL

%% *** SOCKET (socket) options
-define(ESOCK_OPT_SOCK_ACCEPTCONN,      1).
%% -define(ESOCK_OPT_SOCK_ACCEPTFILTER,    2). % FreeBSD
-define(ESOCK_OPT_SOCK_BINDTODEVICE,    3).
-define(ESOCK_OPT_SOCK_BROADCAST,       4).
%% -define(ESOCK_OPT_SOCK_BUSY_POLL,       5).
-define(ESOCK_OPT_SOCK_DEBUG,           6).
-define(ESOCK_OPT_SOCK_DOMAIN,          7).
-define(ESOCK_OPT_SOCK_DONTROUTE,       8).
%% -define(ESOCK_OPT_SOCK_ERROR,           9).
-define(ESOCK_OPT_SOCK_KEEPALIVE,      10).
-define(ESOCK_OPT_SOCK_LINGER,         11).
%% -define(ESOCK_OPT_SOCK_MARK,           12).
-define(ESOCK_OPT_SOCK_OOBINLINE,      13).
-define(ESOCK_OPT_SOCK_PASSCRED,       14).
-define(ESOCK_OPT_SOCK_PEEK_OFF,       15).
%% -define(ESOCK_OPT_SOCK_PEERCRED,       16).
-define(ESOCK_OPT_SOCK_PRIORITY,       17).
-define(ESOCK_OPT_SOCK_PROTOCOL,       18).
-define(ESOCK_OPT_SOCK_RCVBUF,         19).
%% -define(ESOCK_OPT_SOCK_RCVBUFFORCE,    20).
-define(ESOCK_OPT_SOCK_RCVLOWAT,       21).
-define(ESOCK_OPT_SOCK_RCVTIMEO,       22).
-define(ESOCK_OPT_SOCK_REUSEADDR,      23).
-define(ESOCK_OPT_SOCK_REUSEPORT,      24).
%% -define(ESOCK_OPT_SOCK_RXQ_OVFL,       25).
%% -define(ESOCK_OPT_SOCK_SETFIB,         26). % FreeBSD
-define(ESOCK_OPT_SOCK_SNDBUF,         27).
%% -define(ESOCK_OPT_SOCK_SNDBUFFORCE,    28).
-define(ESOCK_OPT_SOCK_SNDLOWAT,       29).
-define(ESOCK_OPT_SOCK_SNDTIMEO,       30).
-define(ESOCK_OPT_SOCK_TIMESTAMP,      31).
-define(ESOCK_OPT_SOCK_TYPE,           32).

%% *** IP (socket) options
-define(ESOCK_OPT_IP_ADD_MEMBERSHIP,         1).
-define(ESOCK_OPT_IP_ADD_SOURCE_MEMBERSHIP,  2).
-define(ESOCK_OPT_IP_BLOCK_SOURCE,           3).
%% -define(ESOCK_OPT_IP_DONTFRAG,               4). % FreeBSD
-define(ESOCK_OPT_IP_DROP_MEMBERSHIP,        5).
-define(ESOCK_OPT_IP_DROP_SOURCE_MEMBERSHIP, 6).
-define(ESOCK_OPT_IP_FREEBIND,               7).
-define(ESOCK_OPT_IP_HDRINCL,                8).
-define(ESOCK_OPT_IP_MINTTL,                 9).
-define(ESOCK_OPT_IP_MSFILTER,              10).
-define(ESOCK_OPT_IP_MTU,                   11).
-define(ESOCK_OPT_IP_MTU_DISCOVER,          12).
-define(ESOCK_OPT_IP_MULTICAST_ALL,         13).
-define(ESOCK_OPT_IP_MULTICAST_IF,          14).
-define(ESOCK_OPT_IP_MULTICAST_LOOP,        15).
-define(ESOCK_OPT_IP_MULTICAST_TTL,         16).
-define(ESOCK_OPT_IP_NODEFRAG,              17).
%% -define(ESOCK_OPT_IP_OPTIONS,               18). % FreeBSD
-define(ESOCK_OPT_IP_PKTINFO,               19).
-define(ESOCK_OPT_IP_RECVDSTADDR,           20). % FreeBSD
-define(ESOCK_OPT_IP_RECVERR,               21).
-define(ESOCK_OPT_IP_RECVIF,                22).
-define(ESOCK_OPT_IP_RECVOPTS,              23).
-define(ESOCK_OPT_IP_RECVORIGDSTADDR,       24).
-define(ESOCK_OPT_IP_RECVTOS,               25).
-define(ESOCK_OPT_IP_RECVTTL,               26).
-define(ESOCK_OPT_IP_RETOPTS,               27).
-define(ESOCK_OPT_IP_ROUTER_ALERT,          28).
-define(ESOCK_OPT_IP_SENDSRCADDR,           29). % FreeBSD
-define(ESOCK_OPT_IP_TOS,                   30).
-define(ESOCK_OPT_IP_TRANSPARENT,           31).
-define(ESOCK_OPT_IP_TTL,                   32).
-define(ESOCK_OPT_IP_UNBLOCK_SOURCE,        33).

%% *** IPv6 (socket) options
-define(ESOCK_OPT_IPV6_ADDRFORM,           1).
-define(ESOCK_OPT_IPV6_ADD_MEMBERSHIP,     2).
-define(ESOCK_OPT_IPV6_AUTHHDR,            3). % Obsolete?
%% -define(ESOCK_OPT_IPV6_AUTH_LEVEL,         4). % FreeBSD
%% -define(ESOCK_OPT_IPV6_CHECKSUM,           5). % FreeBSD
-define(ESOCK_OPT_IPV6_DROP_MEMBERSHIP,    6).
-define(ESOCK_OPT_IPV6_DSTOPTS,            7).
%% -define(ESOCK_OPT_IPV6_ESP_NETWORK_LEVEL,  8). % FreeBSD
%% -define(ESOCK_OPT_IPV6_ESP_TRANS_LEVEL,    9). % FreeBSD
%% -define(ESOCK_OPT_IPV6_FAITH,             10). % FreeBSD
-define(ESOCK_OPT_IPV6_FLOWINFO,          11).
-define(ESOCK_OPT_IPV6_HOPLIMIT,          12).
-define(ESOCK_OPT_IPV6_HOPOPTS,           13).
%% -define(ESOCK_OPT_IPV6_IPCOMP_LEVEL,      14). % FreeBSD
%% -define(ESOCK_OPT_IPV6_JOIN_GROUP,        15). % FreeBSD
%% -define(ESOCK_OPT_IPV6_LEAVE_GROUP,       16). % FreeBSD
-define(ESOCK_OPT_IPV6_MTU,               17).
-define(ESOCK_OPT_IPV6_MTU_DISCOVER,      18).
-define(ESOCK_OPT_IPV6_MULTICAST_HOPS,    19).
-define(ESOCK_OPT_IPV6_MULTICAST_IF,      20).
-define(ESOCK_OPT_IPV6_MULTICAST_LOOP,    21).
%% -define(ESOCK_OPT_IPV6_PORTRANGE,         22). % FreeBSD
%% -define(ESOCK_OPT_IPV6_PKTOPTIONS,        23). % FreeBSD
-define(ESOCK_OPT_IPV6_RECVERR,           24).
-define(ESOCK_OPT_IPV6_RECVHOPLIMIT,      25).
-define(ESOCK_OPT_IPV6_RECVPKTINFO,       26). % On FreeBSD: PKTINFO
-define(ESOCK_OPT_IPV6_RECVTCLASS,        27).
-define(ESOCK_OPT_IPV6_ROUTER_ALERT,      28).
-define(ESOCK_OPT_IPV6_RTHDR,             29).
-define(ESOCK_OPT_IPV6_TCLASS,            30). % FreeBSD
-define(ESOCK_OPT_IPV6_UNICAST_HOPS,      31).
%% -define(ESOCK_OPT_IPV6_USE_MIN_MTU,       32). % FreeBSD
-define(ESOCK_OPT_IPV6_V6ONLY,            33).

%% *** TCP (socket) options
-define(ESOCK_OPT_TCP_CONGESTION,      1).
-define(ESOCK_OPT_TCP_CORK,            2).
%% -define(ESOCK_OPT_TCP_INFO,            3).
%% -define(ESOCK_OPT_TCP_KEEPCNT,         4).
%% -define(ESOCK_OPT_TCP_KEEPIDLE,        5).
%% -define(ESOCK_OPT_TCP_KEEPINTVL,       6).
-define(ESOCK_OPT_TCP_MAXSEG,          7).
%% -define(ESOCK_OPT_TCP_MD5SIG,          8).
-define(ESOCK_OPT_TCP_NODELAY,         9).
%% -define(ESOCK_OPT_TCP_NOOPT,          10).
%% -define(ESOCK_OPT_TCP_NOPUSH,         11).
%% -define(ESOCK_OPT_TCP_SYNCNT,         12).
%% -define(ESOCK_OPT_TCP_USER_TIMEOUT,   13).

%% *** UDP (socket) options
-define(ESOCK_OPT_UDP_CORK,            1).

%% *** SCTP (socket) options
%% -define(ESOCK_OPT_SCTP_ADAPTION_LAYER,          1).
-define(ESOCK_OPT_SCTP_ASSOCINFO,               2).
%% -define(ESOCK_OPT_SCTP_AUTH_ACTIVE_KEY,         3).
%% -define(ESOCK_OPT_SCTP_AUTH_ASCONF,             4).
%% -define(ESOCK_OPT_SCTP_AUTH_CHUNK,              5).
%% -define(ESOCK_OPT_SCTP_AUTH_KEY,                6).
%% -define(ESOCK_OPT_SCTP_AUTH_DELETE_KEY,         7).
-define(ESOCK_OPT_SCTP_AUTOCLOSE,               8).
%% -define(ESOCK_OPT_SCTP_CONTEXT,                 9).
%% -define(ESOCK_OPT_SCTP_DEFAULT_SEND_PARAMS,    10).
%% -define(ESOCK_OPT_SCTP_DELAYED_ACK_TIME,       11).
-define(ESOCK_OPT_SCTP_DISABLE_FRAGMENTS,      12).
%% -define(ESOCK_OPT_SCTP_HMAC_IDENT,             13).
-define(ESOCK_OPT_SCTP_EVENTS,                 14).
%% -define(ESOCK_OPT_SCTP_EXPLICIT_EOR,           15).
%% -define(ESOCK_OPT_SCTP_FRAGMENT_INTERLEAVE,    16).
%% -define(ESOCK_OPT_SCTP_GET_PEER_ADDR_INFO,     17).
-define(ESOCK_OPT_SCTP_INITMSG,                18).
%% -define(ESOCK_OPT_SCTP_I_WANT_MAPPED_V4_ADDR,  19).
%% -define(ESOCK_OPT_SCTP_LOCAL_AUTH_CHUNKS,      20).
-define(ESOCK_OPT_SCTP_MAXSEG,                 21).
%% -define(ESOCK_OPT_SCTP_MAXBURST,               22).
-define(ESOCK_OPT_SCTP_NODELAY,                23).
%% -define(ESOCK_OPT_SCTP_PARTIAL_DELIVERY_POINT, 24).
%% -define(ESOCK_OPT_SCTP_PEER_ADDR_PARAMS,       25).
%% -define(ESOCK_OPT_SCTP_PEER_AUTH_CHUNKS,       26).
%% -define(ESOCK_OPT_SCTP_PRIMARY_ADDR,           27).
%% -define(ESOCK_OPT_SCTP_RESET_STREAMS,          28).
-define(ESOCK_OPT_SCTP_RTOINFO,                29).
%% -define(ESOCK_OPT_SCTP_SET_PEER_PRIMARY_ADDR,  30).
%% -define(ESOCK_OPT_SCTP_STATUS,                 31).
%% -define(ESOCK_OPT_SCTP_USE_EXT_RECVINFO,       32).

-define(ESOCK_SHUTDOWN_HOW_READ,       0).
-define(ESOCK_SHUTDOWN_HOW_WRITE,      1).
-define(ESOCK_SHUTDOWN_HOW_READ_WRITE, 2).


-define(ESOCK_SUPPORTS_OPTIONS,      16#0001).
-define(ESOCK_SUPPORTS_SCTP,         16#0002).
-define(ESOCK_SUPPORTS_IPV6,         16#0003).
-define(ESOCK_SUPPORTS_LOCAL,        16#0004).
-define(ESOCK_SUPPORTS_SEND_FLAGS,   16#0005).
-define(ESOCK_SUPPORTS_RECV_FLAGS,   16#0006).


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
      Extra :: map().

on_load(Extra) ->
    %% This is spawned as a system process to prevent init:restart/0 from
    %% killing it.
    Pid = erts_internal:spawn_system_process(?REGISTRY, start, []),
    ok  = erlang:load_nif(atom_to_list(?MODULE), Extra#{registry => Pid}).


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





-spec info() -> map().

info() ->
    nif_info().


-spec debug(D) -> ok when
      D :: boolean().

debug(D) when is_boolean(D) ->
    command(#{command => debug,
              data    => D}).


-spec command(Command) -> ok when
      Command :: debug_command().

command(#{command := debug,
          data    := Dbg} = Command) when is_boolean(Dbg) ->
    nif_command(Command).



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

info(#socket{ref = SockRef}) ->
    nif_info(SockRef).



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

-type supports_options_socket() :: [{socket_option(),      boolean()}].
-type supports_options_ip()     :: [{ip_socket_option(),   boolean()}].
-type supports_options_ipv6()   :: [{ipv6_socket_option(), boolean()}].
-type supports_options_tcp()    :: [{tcp_socket_option(),  boolean()}].
-type supports_options_udp()    :: [{udp_socket_option(),  boolean()}].
-type supports_options_sctp()   :: [{sctp_socket_option(), boolean()}].
-type supports_options()        :: [{socket, supports_options_socket()} |
                                    {ip,     supports_options_ip()}     |
                                    {ipv6,   supports_options_ipv6()}   |
                                    {tcp,    supports_options_tcp()}    |
                                    {udp,    supports_options_udp()}    |
                                    {sctp,   supports_options_sctp()}].
-type supports_send_flags()     :: [{send_flag(), boolean()}].
-type supports_recv_flags()     :: [{recv_flag(), boolean()}].

-spec supports() -> [{options,    supports_options()} | 
                     {sctp,       boolean()} |
                     {ipv6,       boolean()} |
                     {local,      boolean()} |
                     {send_flags, supports_send_flags()} |
                     {recv_flags, supports_recv_flags()}].

supports() ->
    [{options,    supports(options)},
     {sctp,       supports(sctp)},
     {ipv6,       supports(ipv6)},
     {local,      supports(local)},
     {send_flags, supports(send_flags)},
     {recv_flags, supports(recv_flags)}].


-dialyzer({no_contracts, supports/1}).
-spec supports(options)    -> supports_options();
              (sctp)       -> boolean();
              (ipv6)       -> boolean();
              (local)      -> boolean();
              (send_flags) -> supports_send_flags();
              (recv_flags) -> supports_recv_flags();
              (Key1)    -> false when
      Key1 :: term().
                        
supports(options) ->
    nif_supports(?ESOCK_SUPPORTS_OPTIONS);
supports(sctp) ->
    nif_supports(?ESOCK_SUPPORTS_SCTP);
supports(ipv6) ->
    nif_supports(?ESOCK_SUPPORTS_IPV6);
supports(local) ->
    nif_supports(?ESOCK_SUPPORTS_LOCAL);
supports(send_flags) ->
    nif_supports(?ESOCK_SUPPORTS_SEND_FLAGS);
supports(recv_flags) ->
    nif_supports(?ESOCK_SUPPORTS_RECV_FLAGS);
supports(_Key1) ->
    false.

-dialyzer({no_contracts, supports/2}).
-spec supports(options,    socket)   -> supports_options_socket();
              (options,    ip)       -> supports_options_ip();
              (options,    ipv6)     -> supports_options_ipv6();
              (options,    tcp)      -> supports_options_tcp();
              (options,    udp)      -> supports_options_udp();
              (options,    sctp)     -> supports_options_sctp();
              (send_flags, SendFlag :: send_flag()) -> boolean();
              (recv_flags, RecvFlag :: recv_flag()) -> boolean();
              (Key1,       Key2)     -> false when
      Key1     :: term(),
      Key2     :: term().

supports(options, Level) ->
    proplists:get_value(Level, supports(options), false);
supports(send_flags, Flag) ->
    proplists:get_value(Flag, supports(send_flags), false);
supports(recv_flags, Flag) ->
    proplists:get_value(Flag, supports(recv_flags), false);
supports(_Key1, _Level) ->
    false.


-spec supports(options, socket, Opt :: socket_option()) -> boolean();
              (options, ip,     Opt :: ip_socket_option()) -> boolean();
              (options, ipv6,   Opt :: ipv6_socket_option()) -> boolean();
              (options, tcp,    Opt :: tcp_socket_option()) -> boolean();
              (options, udp,    Opt :: udp_socket_option()) -> boolean();
              (options, sctp,   Opt :: sctp_socket_option()) -> boolean();
              (Key1, Key2, Key3) -> false when
      Key1 :: term(),
      Key2 :: term(),
      Key3 :: term().

-dialyzer({no_contracts, supports/3}).
supports(options, Level, Opt) ->
    case supports(options, Level) of
        S when is_list(S) ->
            proplists:get_value(Opt, S, false);
        _ ->
            false
    end;
supports(_Key1, _Key2, _Key3) ->
    false.



%% ===========================================================================
%%
%% The proper socket API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% <KOLLA>
%%
%% How do we handle the case when an fd has been created (somehow)
%% and we shall create a socket "from it".
%% Can we figure out Domain, Type and Protocol from fd?
%% No we can't: For instance, its not possible to 'get' domain on FreeBSD.
%% 
%% Instead, require: open(Domain, Stream, Proto, #{fd => FD}).
%% The last argument, Extra, is used to provide the fd.
%% 
%% </KOLLA>
%%
%%
%% <KOLLA>
%%
%% Possibly add a "registry" in the nif, allowing the user processes to 
%% "register" themselves.
%% The point of this would be to ensure that these processes are
%% informed if the socket "terminates". Could possibly be used for
%% other things? If gen_tcp implements the active feature using
%% a reader process, the nif may need to know about this process,
%% since its probably "hidden" from the socket "owner" (someone
%% needs to handle it if it dies).
%% Register under a name?
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
%% Extra: Currently only used for netns
%%

-spec open(Domain, Type) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type) ->
    open(Domain, Type, default).

-spec open(Domain, Type, Protocol) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: default | protocol(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type, Protocol) ->
    open(Domain, Type, Protocol, #{}).

-spec open(Domain, Type, Protocol, Extra) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: default | protocol(),
      Extra    :: map(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type, Protocol, Extra) when is_map(Extra) ->
    try
        begin
            EDomain   = enc_domain(Domain),
            EType     = enc_type(Type),
            EProtocol = enc_protocol(Protocol),
            nif_open(EDomain, EType, EProtocol, Extra)
        end
    of
        {ok, SockRef} ->
            Socket = #socket{ref = SockRef},
            {ok, Socket};
        {error, _} = ERROR ->
            ERROR
    catch
        throw:ERROR ->
            ERROR
    end.



%% ===========================================================================
%%
%% bind - bind a name to a socket
%%
%% Note that Addr can only have the value of broadcast *if* Domain =:= inet!
%%

-spec bind(Socket, Addr) -> {ok, Port} | {error, Reason} when
      Socket :: socket(),
      Addr   :: any | broadcast | loopback | sockaddr(),
      Port   :: port_number(),
      Reason :: term().

bind(#socket{ref = SockRef}, Addr)
  when ((Addr =:= any) orelse
	(Addr =:= broadcast) orelse
	(Addr =:= loopback)) ->
    try
        case which_domain(SockRef) of
            inet ->
                nif_bind(SockRef, ?SOCKADDR_IN4_DEFAULT(Addr));
            inet6 when (Addr =:= any) orelse (Addr =:= loopback) ->
                nif_bind(SockRef, ?SOCKADDR_IN6_DEFAULT(Addr));
            Domain ->
                invalid_domain(Domain)
        end
    catch
        throw:ERROR ->
            ERROR
    end;
bind(#socket{ref = SockRef} = _Socket, Addr) when is_map(Addr) ->
    try
        nif_bind(SockRef, ensure_sockaddr(Addr))
    catch
        throw:ERROR ->
            ERROR
    end.



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
      Reason :: term().

bind(#socket{ref = SockRef}, Addrs, Action) 
  when is_list(Addrs) andalso ((Action =:= add) orelse (Action =:= remove)) ->
    try
        begin
            {Domain, Type, Proto} = which_dtp(SockRef),
            ensure_domain(Domain, [inet, inet6]),
            ensure_type(Type, seqpacket),
            ensure_protocol(Proto, sctp),
            validate_addrs(Domain, Addrs),
            nif_bind(SockRef, Addrs, Action)
        end
    catch
        throw:ERROR ->
            ERROR
    end.

ensure_domain(Domain, [Domain | _]) -> ok;
ensure_domain(Domain, [_ | Domains]) -> ensure_domain(Domain, Domains);
ensure_domain(Domain, []) -> invalid_domain(Domain).

ensure_type(Type, Type) -> ok;
ensure_type(Type, _) -> invalid_type(Type).

ensure_protocol(Proto, Proto) -> ok;
ensure_protocol(Proto, _) -> invalid_protocol(Proto).


validate_addrs(inet = _Domain, Addrs) ->
    validate_inet_addrs(Addrs);
validate_addrs(inet6 = _Domain, Addrs) ->
    validate_inet6_addrs(Addrs).

validate_inet_addrs(Addrs) ->
    Validator = fun(#{family := inet,
                      addrs  := Addr}) when is_tuple(Addr) andalso 
                                            (size(Addr) =:= 4) ->
                        ok;
                   (X) ->
                        throw({error, {invalid_address, X}})
                end,
    lists:foreach(Validator, Addrs).

validate_inet6_addrs(Addrs) ->
    Validator = fun(#{family := inet,
                      addrs  := Addr}) when is_tuple(Addr) andalso 
                                            (size(Addr) =:= 4) ->
                        ok;
                   (#{family := inet6,
                      addrs  := Addr}) when is_tuple(Addr) andalso 
                                            (size(Addr) =:= 8) ->
                        ok;
                   (X) ->
                        throw({error, {invalid_address, X}})
                end,
    lists:foreach(Validator, Addrs).


%% ===========================================================================
%%
%% connect - initiate a connection on a socket
%%

-spec connect(Socket, SockAddr) -> ok | {error, Reason} when
    Socket   :: socket(),
    SockAddr :: sockaddr(),
    Reason   :: term().

connect(Socket, SockAddr) ->
    connect(Socket, SockAddr, infinity).

-spec connect(Socket, SockAddr, nowait) ->
           ok | {select, SelectInfo} | {error, Reason} when
    Socket     :: socket(),
    SockAddr   :: sockaddr(),
    SelectInfo :: select_info(),
    Reason     :: term()
        ; (Socket, SockAddr, Timeout) -> ok | {error, Reason} when
    Socket   :: socket(),
    SockAddr :: sockaddr(),
    Timeout  :: timeout(),
    Reason   :: term().

%% <KOLLA>
%% Is it possible to connect with family = local for the (dest) sockaddr?
%% </KOLLA>
connect(#socket{ref = SockRef}, SockAddr, Timeout) ->
    try
        do_connect(SockRef, ensure_sockaddr(SockAddr), deadline(Timeout))
    catch
        throw:ERROR ->
            ERROR
    end.


do_connect(SockRef, SockAddr, Deadline) ->
    case nif_connect(SockRef, ensure_sockaddr(SockAddr)) of

        ok ->
            %% Connected!
            ok;


        {ok, Ref} when (Deadline =:= nowait) ->
            %% Connecting, but the caller does not want to wait...
            ?SELECT(connect, Ref);

        {ok, Ref} ->
            %% Connecting...
            Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, Ref} ->
                    nif_finalize_connection(SockRef)
            after Timeout ->
                    cancel(SockRef, connect, Ref),
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
    listen(Socket, ?ESOCK_LISTEN_BACKLOG_DEFAULT).

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
    accept(Socket, ?ESOCK_ACCEPT_TIMEOUT_DEFAULT).

-spec accept(LSocket, nowait) -> 
                    {ok, Socket} |
                    {select, SelectInfo} |
                    {error, Reason} when
      LSocket    :: socket(),
      Socket     :: socket(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (LSocket, Timeout) -> {ok, Socket} | {error, Reason} when
      LSocket    :: socket(),
      Timeout    :: timeout(),
      Socket     :: socket(),
      Reason     :: term().

accept(#socket{ref = LSockRef}, Timeout) ->
    try
        do_accept(LSockRef, deadline(Timeout))
    catch
        throw:ERROR ->
            ERROR
    end.

do_accept(LSockRef, Deadline) ->
    AccRef = make_ref(),
    case nif_accept(LSockRef, AccRef) of

        {ok, SockRef} ->
            Socket = #socket{ref = SockRef},
            {ok, Socket};


        {error, eagain} when (Deadline =:= nowait) ->
            ?SELECT(accept, AccRef);

        {error, eagain} ->
            %% Each call is non-blocking, but even then it takes
            %% *some* time, so just to be sure, recalculate before 
            %% the receive.
	    Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = LSockRef}, select, AccRef} ->
                    do_accept(LSockRef, Deadline);

                {?ESOCK_TAG, _Socket, abort, {AccRef, Reason}} ->
                    {error, Reason}

            after Timeout ->
                    cancel(LSockRef, accept, AccRef),
                    {error, timeout}
            end;


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
      Reason     :: term();
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
      Reason     :: term();
          (Socket, Data, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Timeout    :: timeout(),
      Reason     :: term().

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
      Reason     :: term()
                 ; (Socket, Data, Flags, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: send_flags(),
      Timeout    :: timeout(),
      Reason     :: term().

send(Socket, Data, Flags, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    send(Socket, Bin, Flags, Timeout);
send(#socket{ref = SockRef}, Data, Flags, Timeout)
  when is_binary(Data), is_list(Flags) ->
    To = undefined,
    try
        begin
            EFlags = enc_send_flags(Flags),
            Deadline = deadline(Timeout),
            send_common(SockRef, Data, To, EFlags, Deadline, send)
        end
    catch
        throw:ERROR ->
            ERROR
    end.

send_common(SockRef, Data, To, EFlags, Deadline, SendName) ->

    SendRef = make_ref(),

    case
        case SendName of
            send ->
                nif_send(SockRef, SendRef, Data, EFlags);
            sendto ->
                nif_sendto(SockRef, SendRef, Data, To, EFlags)
        end
    of

        ok -> ok;


        {ok, Written} when (Deadline =:= nowait) ->
	    %% We are partially done, but the user don't want to wait (here) 
            %% for completion
            <<_:Written/binary, Rest/binary>> = Data,
            {ok, {Rest, ?SELECT_INFO(SendName, SendRef)}};

        {ok, Written} ->
	    %% We are partially done, wait for continuation
            Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, SendRef}
                  when (Written > 0) -> 
                    <<_:Written/binary, Rest/binary>> = Data,
                    send_common(
                      SockRef, Rest, To, EFlags, Deadline, SendName);

                {?ESOCK_TAG, #socket{ref = SockRef}, select, SendRef} ->
                    send_common(
                      SockRef, Data, To, EFlags, Deadline, SendName);

                {?ESOCK_TAG, _Socket, abort, {SendRef, Reason}} ->
                    {error, {Reason, size(Data)}}

            after Timeout ->
                    _ = cancel(SockRef, SendName, SendRef),
                    {error, {timeout, size(Data)}}
            end;


        {error, exbusy} = Error when Deadline =:= nowait -> Error;

        {error, exbusy = Reason} ->
            %% Internal error:
            %%   we called send, got eagain, and called send again
            %%   - without waiting for select message
            erlang:error(Reason);


        {error, eagain} when (Deadline =:= nowait) ->
            ?SELECT(SendName, SendRef);

        {error, eagain} ->
            Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, SendRef} ->
                    send_common(
                      SockRef, Data, To, EFlags, Deadline, SendName);

                {?ESOCK_TAG, _Socket, abort, {SendRef, Reason}} ->
                    {error, {Reason, size(Data)}}

            after Timeout ->
                    _ = cancel(SockRef, SendName, SendRef),
                    {error, {timeout, size(Data)}}
            end;


        {error, Reason} ->
            {error, {Reason, size(Data)}}
    end.


%% ---------------------------------------------------------------------------
%%

-spec sendto(Socket, Data, Dest) ->
                    ok | {error, Reason} when
      Socket :: socket(),
      Data   :: binary(),
      Dest   :: sockaddr(),
      Reason :: term().

sendto(Socket, Data, Dest) ->
    sendto(Socket, Data, Dest, ?ESOCK_SENDTO_FLAGS_DEFAULT).

-spec sendto(Socket, Data, Dest, Flags) -> ok | {error, Reason} when
      Socket    :: socket(),
      Data      :: binary(),
      Dest      :: sockaddr(),
      Flags     :: send_flags(),
      Reason    :: term()
                   ; (Socket, Data, Dest, Timeout :: nowait) -> ok |
                                                  {select, SelectInfo} |
                                                  {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (Socket, Data, Dest, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Timeout    :: timeout(),
      Reason     :: term().

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
      Reason     :: term()
                 ; (Socket, Data, Dest, Flags, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      Data       :: binary(),
      Dest       :: sockaddr(),
      Flags      :: send_flags(),
      Timeout    :: timeout(),
      Reason     :: term().

sendto(Socket, Data, Dest, Flags, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    sendto(Socket, Bin, Dest, Flags, Timeout);
sendto(#socket{ref = SockRef}, Data, Dest, Flags, Timeout)
  when is_binary(Data), is_list(Flags) ->
    try
        begin
            To = ensure_sockaddr(Dest),
            EFlags = enc_send_flags(Flags),
            Deadline = deadline(Timeout),
            send_common(SockRef, Data, To, EFlags, Deadline, sendto)
        end
    catch
        throw:ERROR ->
            ERROR
    end.


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
      Reason  :: term();
             (Socket, MsgHdr, Timeout :: nowait) ->
                     ok |
                     {ok, Remaining} |
                     {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Remaining :: erlang:iovec(),
      Reason     :: term();
             (Socket, MsgHdr, Timeout) -> ok | {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Timeout    :: timeout(),
      Reason     :: term().

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
      Reason     :: term()
                   ; (Socket, MsgHdr, Flags, Timeout) -> 
                     ok |
                     {ok, Remaining} |
                     {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      Flags      :: send_flags(),
      Timeout    :: timeout(),
      Remaining  :: erlang:iovec(),
      Reason     :: term().

sendmsg(#socket{ref = SockRef}, #{iov := IOV} = MsgHdr, Flags, Timeout)
  when is_list(IOV), is_list(Flags) ->
    try
        begin
            M = ensure_msghdr(MsgHdr),
            EFlags = enc_send_flags(Flags),
            Deadline = deadline(Timeout),
            do_sendmsg(SockRef, M, EFlags, Deadline)
        end
    catch
        throw:ERROR ->
            ERROR
    end.


do_sendmsg(SockRef, MsgHdr, EFlags, Deadline) ->

    SendRef = make_ref(),

    case nif_sendmsg(SockRef, SendRef, MsgHdr, EFlags) of
        ok ->
            %% We are done
            ok;


        {ok, Written} when is_integer(Written) andalso (Written > 0) ->
            %% We should not retry here since the protocol may not
            %% be able to handle a message being split. Leave it to
            %% the caller to figure out (call again with the rest).
            %%
            %% We need to cancel this partial write.
            %%
            _ = cancel(SockRef, sendmsg, SendRef),
            {ok, do_sendmsg_rest(maps:get(iov, MsgHdr), Written)};


        {error, exbusy} = Error when Deadline =:= nowait -> Error;

        {error, exbusy = Reason} ->
            %% Internal error:
            %%   we called send, got eagain, and called send again
            %%   - without waiting for select message
            erlang:error(Reason);


        {error, eagain} when (Deadline =:= nowait) ->
            ?SELECT(sendmsg, SendRef);
            
        {error, eagain} ->
	    Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, SendRef} ->
                    do_sendmsg(SockRef, MsgHdr, EFlags, Deadline);

                {?ESOCK_TAG, _Socket, abort, {SendRef, Reason}} ->
                    {error, Reason}

            after Timeout ->
                    _ = cancel(SockRef, sendmsg, SendRef),
                    {error, timeout}
            end;


        {error, _} = ERROR ->
            ERROR
    end.

do_sendmsg_rest([B|IOVec], Written) when (Written >= size(B)) ->
    do_sendmsg_rest(IOVec, Written - size(B));
do_sendmsg_rest([B|IOVec], Written) ->
    <<_:Written/binary, Rest/binary>> = B,
    [Rest|IOVec].

ensure_msghdr(#{ctrl := []} = M) ->
    ensure_msghdr(maps:remove(ctrl, M));
ensure_msghdr(#{iov := IOV, addr := Addr} = M) 
  when is_list(IOV) andalso (IOV =/= []) ->
    M#{iov => erlang:iolist_to_iovec(IOV), addr => ensure_sockaddr(Addr)};
ensure_msghdr(#{iov := IOV} = M) 
  when is_list(IOV) andalso (IOV =/= []) ->
    M#{iov => erlang:iolist_to_iovec(IOV)};
ensure_msghdr(_) ->
    einval().



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

-spec recv(Socket) -> {ok, Data} | {error, Reason} when
      Socket :: socket(),
      Data   :: binary(),
      Reason :: term().
                          
recv(Socket) ->
    recv(Socket, 0).

-spec recv(Socket, Length) -> {ok, Data} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Data   :: binary(),
      Reason :: term().

recv(Socket, Length) ->
    recv(Socket, Length,
         ?ESOCK_RECV_FLAGS_DEFAULT,
         ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recv(Socket, Length, Flags) -> {ok, Data} |
                                     {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Flags  :: recv_flags(),
      Data   :: binary(),
      Reason :: term()
                ; (Socket, Length, Timeout :: nowait) -> {ok, Data} |
                                              {select, SelectInfo} |
                                              {ok, {Data, SelectInfo}} |
                                              {error, Reason} when
      Socket     :: socket(),
      Length     :: non_neg_integer(),
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (Socket, Length, Timeout) -> {ok, Data} |
                                                {error, Reason} when
      Socket  :: socket(),
      Length  :: non_neg_integer(),
      Timeout :: timeout(),
      Data    :: binary(),
      Reason  :: term().

recv(Socket, Length, Flags) when is_list(Flags) ->
    recv(Socket, Length, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length, Timeout) ->
    recv(Socket, Length, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).

-spec recv(Socket, Length, Flags, nowait) -> {ok, Data} |
                                             {select, SelectInfo} |
                                             {ok, {Data, SelectInfo}} |
                                             {error, Reason} when
      Socket     :: socket(),
      Length     :: non_neg_integer(),
      Flags      :: recv_flags(),
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (Socket, Length, Flags, Timeout) -> {ok, Data} |
                                                       {error, Reason} when
      Socket     :: socket(),
      Length     :: non_neg_integer(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      Data       :: binary(),
      Reason     :: term().

recv(#socket{ref = SockRef}, Length, Flags, Timeout)
  when is_integer(Length), Length >= 0, is_list(Flags) ->
    try
        EFlags = enc_recv_flags(Flags),
        Deadline = deadline(Timeout),
        do_recv(SockRef, Length, EFlags, Deadline, <<>>)
    catch
        throw:ERROR ->
            ERROR
    end.

%% We will only recurse with Length == 0 if Length is 0,
%% so Length == 0 means to return all available data also when recursing
%%
%% Note that the Deadline value of 'nowait' has a special meaning. It means
%% that we will either return with data or with the with {error, NNNN}. In
%% wich case the caller will receive a select message at some later time.
%%
do_recv(SockRef, Length, EFlags, Deadline, Acc) ->

    RecvRef = make_ref(),
    case nif_recv(SockRef, RecvRef, Length, EFlags) of

        {ok, true = _Complete, Bin} ->
            {ok, bincat(Acc, Bin)};


        %% It depends on the amount of bytes we tried to read:
        %%    0   - Read everything available
        %%          We got something, but there may be more - keep reading.
        %%    > 0 - We got a part of the message and we will be notified
        %%          when there is more to read (a select message)
        {ok, false = _Complete, Bin} when Length =:= 0 ->
            Timeout = timeout(Deadline),
            if
                0 < Timeout ->
                    do_recv(
                      SockRef, Length, EFlags, Deadline, bincat(Acc, Bin));
                true ->
                    {ok, bincat(Acc, Bin)}
            end;

        %% Did not get all the user asked for, but the user also
        %% specified 'nowait', so deliver what we got and the 
        %% select info.
        {ok, false = _Completed, Bin} when Deadline =:= nowait ->
            {ok, {bincat(Acc, Bin), ?SELECT_INFO(recv, RecvRef)}};

        {ok, false = _Completed, Bin} ->
            %% We got a chunk of it!
	    Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, RecvRef} ->
                    if
                        0 < Timeout ->
                            do_recv(
                              SockRef, Length - byte_size(Bin), EFlags,
                              Deadline, bincat(Acc, Bin));
                        true ->
                            {error, {timeout, bincat(Acc, Bin)}}
                    end;

                {?ESOCK_TAG, _Socket, abort, {RecvRef, Reason}} ->
                    {error, Reason}

            after Timeout ->
                    cancel(SockRef, recv, RecvRef),
                    {error, {timeout, bincat(Acc, Bin)}}
            end;


        {error, exbusy} = Error when Deadline =:= nowait -> Error;

        {error, exbusy = Reason} ->
            %% Internal error:
            %%   we called recv, got eagain, and called recv again
            %%   - without waiting for select message
            erlang:error(Reason);


        %% The user does not want to wait!
        %% The user will be informed that there is something to read
        %% via the select socket message (see below).
        {error, eagain} when Deadline =:= nowait ->
            if
                byte_size(Acc) =:= 0 ->
                    ?SELECT(recv, RecvRef);
                true ->
                    {ok, {Acc, ?SELECT_INFO(recv, RecvRef)}}
            end;


        %% We return with the accumulated binary (if its non-empty)
        {error, eagain} when Length =:= 0, 0 < byte_size(Acc) ->
            cancel(SockRef, recv, RecvRef),
            {ok, Acc};

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, RecvRef} ->
                    if
                        0 < Timeout ->
                            do_recv(
                              SockRef, Length, EFlags, Deadline, Acc);
                        0 < byte_size(Acc) ->
                            {error, {timeout, Acc}};
                        true ->
                            {error, timeout}
                    end;

                {?ESOCK_TAG, _Socket, abort, {RecvRef, Reason}} ->
                    {error, Reason}

            after Timeout ->
                    cancel(SockRef, recv, RecvRef),
                    {error, timeout}
            end;


        {error, _} = ERROR when byte_size(Acc) =:= 0 ->
            ERROR;

        {error, Reason} ->
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
      Reason    :: term().

recvfrom(Socket) ->
    recvfrom(Socket, 0).

-spec recvfrom(Socket, BufSz) -> {ok, {Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Source    :: sockaddr() | undefined,
      Data      :: binary(),
      Reason    :: term().

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
      Reason     :: term()
                 ; (Socket, Flags, Timeout) -> 
                      {ok, {Source, Data}} |
                      {error, Reason} when
      Socket     :: socket(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      Reason     :: term()
                 ; (Socket, BufSz, Flags) -> 
                      {ok, {Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Flags     :: recv_flags(),
      Source    :: sockaddr() | undefined,
      Data      :: binary(),
      Reason    :: term()
                ; (Socket, BufSz, nowait) -> 
                      {ok, {Source, Data}} |
                      {select, SelectInfo} |
                      {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (Socket, BufSz, Timeout) -> 
                      {ok, {Source, Data}} | {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      Timeout    :: timeout(),
      Source     :: sockaddr() | undefined,
      Data       :: binary(),
      Reason     :: term().

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
      Reason     :: term()
                 ; (Socket, BufSz, Flags, Timeout) -> 
                      {ok, {Source, Data}} |
                      {error, Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Flags   :: recv_flags(),
      Timeout :: timeout(),
      Source  :: sockaddr() | undefined,
      Data    :: binary(),
      Reason  :: term().

recvfrom(#socket{ref = SockRef}, BufSz, Flags, Timeout)
  when is_integer(BufSz), 0 =< BufSz, is_list(Flags) ->
    try
        EFlags = enc_recv_flags(Flags),
        Deadline = deadline(Timeout),
        do_recvfrom(SockRef, BufSz, EFlags, Deadline)
    catch
        throw:ERROR ->
            ERROR
    end.

do_recvfrom(SockRef, BufSz, EFlags, Deadline)  ->

    RecvRef = make_ref(),
    case nif_recvfrom(SockRef, RecvRef, BufSz, EFlags) of

        {ok, {_Source, _NewData}} = OK ->
            OK;


        {error, exbusy} = Error when Deadline =:= nowait -> Error;

        {error, exbusy = Reason} ->
            %% Internal error:
            %%   we called recvfrom, got eagain, and called recvfrom again
            %%   - without waiting for select message
            erlang:error(Reason);


        {error, eagain} when Deadline =:= nowait ->
            ?SELECT(recvfrom, RecvRef);

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, RecvRef} ->
                    do_recvfrom(SockRef, BufSz, EFlags, Deadline);

                {?ESOCK_TAG, _Socket, abort, {RecvRef, Reason}} ->
                    {error, Reason}

            after Timeout ->
                    cancel(SockRef, recvfrom, RecvRef),
                    {error, timeout}
            end;


        {error, _Reason} = ERROR ->
            ERROR

    end.


%% ---------------------------------------------------------------------------
%%

-spec recvmsg(Socket) -> {ok, MsgHdr} | {error, Reason} when
      Socket  :: socket(),
      MsgHdr  :: msghdr(),
      Reason  :: term().

recvmsg(Socket) ->
    recvmsg(Socket, 0, 0,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recvmsg(Socket, Flags) -> {ok, MsgHdr} | {error, Reason} when
      Socket  :: socket(),
      Flags   :: recv_flags(),
      MsgHdr  :: msghdr(),
      Reason  :: term()
                 ; (Socket, Timeout :: nowait) -> {ok, MsgHdr} |
                                                  {select, SelectInfo} |
                                                  {error, Reason} when
      Socket     :: socket(),
      MsgHdr     :: msghdr(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (Socket, Timeout) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      Timeout    :: timeout(),
      MsgHdr     :: msghdr(),
      Reason     :: term().

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
      Reason     :: term()
                 ; (Socket, Flags, Timeout) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      MsgHdr     :: msghdr(),
      Reason     :: term()
                 ; (Socket, BufSz, CtrlSz) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      CtrlSz     :: non_neg_integer(),
      MsgHdr     :: msghdr(),
      Reason     :: term().

recvmsg(Socket, Flags, Timeout) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, Timeout);
recvmsg(Socket, BufSz, CtrlSz) when is_integer(BufSz), is_integer(CtrlSz) ->
    recvmsg(Socket, BufSz, CtrlSz,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).


-spec recvmsg(Socket, 
              BufSz, CtrlSz,
              Flags, nowait) -> {ok, MsgHdr} |
                                {select, SelectInfo} |
                                {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      CtrlSz     :: non_neg_integer(),
      Flags      :: recv_flags(),
      MsgHdr     :: msghdr(),
      SelectInfo :: select_info(),
      Reason     :: term()
                 ; (Socket, 
                    BufSz, CtrlSz,
                    Flags, Timeout) -> {ok, MsgHdr} | {error, Reason} when
      Socket     :: socket(),
      BufSz      :: non_neg_integer(),
      CtrlSz     :: non_neg_integer(),
      Flags      :: recv_flags(),
      Timeout    :: timeout(),
      MsgHdr     :: msghdr(),
      Reason     :: term().

recvmsg(#socket{ref = SockRef}, BufSz, CtrlSz, Flags, Timeout)
  when is_integer(BufSz), 0 =< BufSz,
       is_integer(CtrlSz), 0 =< CtrlSz,
       is_list(Flags) ->
    try
        EFlags = enc_recv_flags(Flags),
        Deadline = deadline(Timeout),
        do_recvmsg(SockRef, BufSz, CtrlSz, EFlags, Deadline)
    catch
        throw:ERROR ->
            ERROR
    end.

do_recvmsg(SockRef, BufSz, CtrlSz, EFlags, Deadline)  ->

    RecvRef = make_ref(),
    case nif_recvmsg(SockRef, RecvRef, BufSz, CtrlSz, EFlags) of

        {ok, _MsgHdr} = OK ->
            OK;


        {error, exbusy} = Error when Deadline =:= nowait -> Error;

        {error, exbusy = Reason} ->
            %% Internal error:
            %%   we called recvmsg, got eagain, and called recvmsg again
            %%   - without waiting for select message
            erlang:error(Reason);


        {error, eagain} when Deadline =:= nowait ->
            ?SELECT(recvmsg, RecvRef);

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, select, RecvRef} ->
                    do_recvmsg(SockRef, BufSz, CtrlSz, EFlags, Deadline);

                {?ESOCK_TAG, _Socket, abort, {RecvRef, Reason}} ->
                    {error, Reason}

            after Timeout ->
                    cancel(SockRef, recvmsg, RecvRef),
                    {error, timeout}
            end;


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
%% 1) nif_close + the socket_stop (nif) callback function
%%    This is for everything that can be done safely NON-BLOCKING.
%% 2) nif_finalize_close which is executed by a *dirty* scheduler
%%    Before we call the socket close function, we set the socket 
%%    BLOCKING. Thereby linger is handled properly.

-spec close(Socket) -> ok | {error, Reason} when
      Socket :: socket(),
      Reason :: term().

close(#socket{ref = SockRef}) ->
    case nif_close(SockRef) of
        ok ->
            nif_finalize_close(SockRef);
        {ok, CloseRef} ->
            %% We must wait for the socket_stop callback function to 
            %% complete its work
            receive
                {?ESOCK_TAG, #socket{ref = SockRef}, close, CloseRef} ->
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
        nif_shutdown(SockRef, enc_shutdown_how(How))
    catch
        throw:T ->
            T;
        %% <WIN32-TEMPORARY>
        error:notsup:S ->
            erlang:raise(error, notsup, S);
        %% </WIN32-TEMPORARY>
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

-spec setopt(Socket, otp, otp_socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, socket, socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, ip, ip_socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, ipv6, ipv6_socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, tcp, tcp_socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, udp, udp_socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, sctp, sctp_socket_option(), Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, Level, Key, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: non_neg_integer(),
      Key    :: non_neg_integer(),
      Value  :: binary(),
      Reason :: term().

setopt(#socket{ref = SockRef}, Level, Key, Value) ->
    try
        begin
            {Domain, Type, Proto} = which_dtp(SockRef),
            {EIsEncoded, ELevel} = enc_setopt_level(Level),
            EKey = enc_setopt_key(Level, Key, Domain, Type, Proto),
            EVal = enc_setopt_value(Level, Key, Value, Domain, Type, Proto),
            nif_setopt(SockRef, EIsEncoded, ELevel, EKey, EVal)
        end
    catch
        throw:ERROR ->
            ERROR
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

-spec getopt(Socket, otp, otp_socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, socket, socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, ip, ip_socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, ipv6, ipv6_socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, tcp, tcp_socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, udp, udp_socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Value  :: term(),
      Reason :: term()
                ; (Socket, sctp, sctp_socket_option()) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
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

getopt(#socket{ref = SockRef}, Level, Key) ->
    try
        begin
            {Domain, Type, Proto} = which_dtp(SockRef),
            {EIsEncoded, ELevel} = enc_getopt_level(Level),
            EKey     = enc_getopt_key(Level, Key, Domain, Type, Proto),
            %% We may need to decode the value (for the same reason
            %% we (may have) needed to encode the value for setopt).
            case nif_getopt(SockRef, EIsEncoded, ELevel, EKey) of
                ok ->
                    ok;
                {ok, EVal} ->
                    Val =
                        dec_getopt_value(
                          Level, Key, EVal, Domain, Type, Proto),
                    {ok, Val};
                {error, _} = E ->
                    E
            end
        end
    catch
        throw:ERROR ->
            ERROR
    end.


%% These are internal "shortcut" functions for the options
%% domain, type and protocol.

-spec which_domain(SockRef) -> Domain when
      SockRef :: reference(),
      Domain  :: domain().

which_domain(SockRef) ->
    case nif_getopt(SockRef, true,
                    ?ESOCK_OPT_LEVEL_OTP, ?ESOCK_OPT_OTP_DOMAIN) of
        {ok, Domain} ->
            if
                is_atom(Domain) ->
                    Domain;
                is_integer(Domain) ->
                    invalid_domain(Domain)
            end;
        {error, _} = ERROR ->
            throw(ERROR)
    end.
        

%%%-spec which_type(SockRef) -> Type when
%%%      SockRef :: reference(),
%%%      Type    :: type().
%%%
%%%which_type(SockRef) ->
%%%    case nif_getopt(SockRef, true,
%%%                    ?ESOCK_OPT_LEVEL_OTP, ?ESOCK_OPT_OTP_TYPE) of
%%%        {ok, Type} ->
%%%            if
%%%                is_atom(Type) ->
%%%                    Type;
%%%                is_integer(Type) ->
%%%                    invalid_type(Type)
%%%            end;
%%%        {error, _} = ERROR ->
%%%            throw(ERROR)
%%%    end.
%%%
%%%-spec which_protocol(SockRef) -> Protocol when
%%%      SockRef  :: reference(),
%%%      Protocol :: protocol().
%%%
%%%which_protocol(SockRef) ->
%%%    case nif_getopt(SockRef, true,
%%%                    ?ESOCK_OPT_LEVEL_OTP, ?ESOCK_OPT_OTP_PROTOCOL) of
%%%        {ok, Proto} ->
%%%            if
%%%                is_atom(Proto) ->
%%%                    Proto;
%%%                is_integer(Proto) ->
%%%                    invalid_protocol(Proto)
%%%            end;
%%%        {error, _} = ERROR ->
%%%            throw(ERROR)
%%%    end.

which_dtp(SockRef) ->
    case
        nif_getopt(
          SockRef, true, ?ESOCK_OPT_LEVEL_OTP, ?ESOCK_OPT_OTP_DTP)
    of
        {ok, {Domain, Type, Proto} = DTP} ->
            if
                is_integer(Domain) ->
                    invalid_domain(Domain);
                is_integer(Type) ->
                    invalid_type(Type);
                is_integer(Proto) ->
                    invalid_protocol(Proto);
                is_atom(Domain), is_atom(Type), is_atom(Proto) ->
                    DTP
            end;
        {error, _} = ERROR ->
            throw(ERROR)
    end.



%% ===========================================================================
%%
%% sockname - return the current address of the socket.
%%
%%

-spec sockname(Socket) -> {ok, SockAddr} | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: term().

sockname(#socket{ref = SockRef}) ->
    nif_sockname(SockRef).



%% ===========================================================================
%%
%% peername - return the address of the peer *connected* to the socket.
%%
%%

-spec peername(Socket) -> {ok, SockAddr} | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: term().

peername(#socket{ref = SockRef}) ->
    nif_peername(SockRef).


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
      Reason     :: term().

cancel(#socket{ref = SockRef}, ?SELECT_INFO(Tag, Ref)) ->
    cancel(SockRef, Tag, Ref).



%% ===========================================================================
%%
%% Encode / decode
%%
%% ===========================================================================

%% -spec enc_domain(Domain) -> non_neg_integer() when
%%       Domain :: domain().

enc_domain(local)  -> ?ESOCK_DOMAIN_LOCAL;
enc_domain(inet)   -> ?ESOCK_DOMAIN_INET;
enc_domain(inet6)  -> ?ESOCK_DOMAIN_INET6;
enc_domain(Domain) -> invalid_domain(Domain).

%% -spec enc_type(Type) -> non_neg_integer() when
%%       Type   :: type().

enc_type(stream)    -> ?ESOCK_TYPE_STREAM;
enc_type(dgram)     -> ?ESOCK_TYPE_DGRAM;
enc_type(raw)       -> ?ESOCK_TYPE_RAW;
enc_type(seqpacket) -> ?ESOCK_TYPE_SEQPACKET;
enc_type(Type)      -> invalid_type(Type).

-spec enc_protocol(Protocol) -> non_neg_integer() |
                                {raw, non_neg_integer()} when
      Protocol :: protocol().

enc_protocol(default) -> ?ESOCK_PROTOCOL_DEFAULT;
enc_protocol(ip)      -> ?ESOCK_PROTOCOL_IP;
enc_protocol(tcp)     -> ?ESOCK_PROTOCOL_TCP;
enc_protocol(udp)     -> ?ESOCK_PROTOCOL_UDP;
enc_protocol(sctp)    -> ?ESOCK_PROTOCOL_SCTP;
enc_protocol(icmp)    -> ?ESOCK_PROTOCOL_ICMP;
enc_protocol(igmp)    -> ?ESOCK_PROTOCOL_IGMP;
enc_protocol({raw, P} = RAW) when is_integer(P) -> RAW;
enc_protocol(Proto) ->
    invalid_protocol(Proto).


-spec enc_send_flags(Flags) -> non_neg_integer() when
      Flags :: send_flags().

enc_send_flags(Flags) ->
    EFlags = [{confirm,   ?ESOCK_SEND_FLAG_CONFIRM},
              {dontroute, ?ESOCK_SEND_FLAG_DONTROUTE},
              {eor,       ?ESOCK_SEND_FLAG_EOR},
              {more,      ?ESOCK_SEND_FLAG_MORE},
              {nosignal,  ?ESOCK_SEND_FLAG_NOSIGNAL},
              {oob,       ?ESOCK_SEND_FLAG_OOB}],
    enc_flags(Flags, EFlags).

-spec enc_recv_flags(Flags) -> non_neg_integer() when
      Flags :: recv_flags().

enc_recv_flags(Flags) ->
    EFlags = [{cmsg_cloexec, ?ESOCK_RECV_FLAG_CMSG_CLOEXEC},
              {errqueue,     ?ESOCK_RECV_FLAG_ERRQUEUE},
              {oob,          ?ESOCK_RECV_FLAG_OOB},
              {peek,         ?ESOCK_RECV_FLAG_PEEK},
              {trunc,        ?ESOCK_RECV_FLAG_TRUNC}],
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
    {true, ?ESOCK_OPT_LEVEL_OTP};
enc_setopt_level(socket) ->
    {true, ?ESOCK_OPT_LEVEL_SOCKET};
enc_setopt_level(ip) ->
    {true, ?ESOCK_OPT_LEVEL_IP};
enc_setopt_level(ipv6) ->
    {true, ?ESOCK_OPT_LEVEL_IPV6};
enc_setopt_level(tcp) ->
    {true, ?ESOCK_OPT_LEVEL_TCP};
enc_setopt_level(udp) ->
    {true, ?ESOCK_OPT_LEVEL_UDP};
enc_setopt_level(sctp) ->
    {true, ?ESOCK_OPT_LEVEL_SCTP};
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

-spec enc_setopt_value(otp, otp_socket_option(),
                       Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
                  ; (socket, socket_option(),
                     Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
           ; (ip, ip_socket_option(),
              Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
           ; (ipv6, ipv6_socket_option(),
              Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
           ; (tcp, tcp_socket_option(),
              Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
           ; (udp, udp_socket_option(),
              Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
           ; (sctp, sctp_socket_option(),
              Value, Domain, Type, Protocol) -> term() when
      Value    :: term(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol()
           ; (Level, Opt,
              Value, Domain, Type, Protocol) -> term() when
      Level    :: integer(),
      Opt      :: integer(),
      Value    :: binary(),
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: protocol().
      
enc_setopt_value(otp, debug, V, _, _, _) when is_boolean(V) ->
    V;
enc_setopt_value(otp, iow, V, _, _, _) when is_boolean(V) ->
    V;
enc_setopt_value(otp, controlling_process, V, _, _, _) when is_pid(V) ->
    V;
enc_setopt_value(otp, rcvbuf, V, _, _, _) when (V =:= default) ->
    0; % This will cause the nif-code to choose the default value
enc_setopt_value(otp, rcvbuf, V, _, _, _) when is_integer(V) andalso (V > 0) ->
    V;
%% N: Number of reads (when specifying length = 0)
%% V: Size of the "read" buffer
enc_setopt_value(otp, rcvbuf, {N, BufSz} = V, _, stream = _T, _P) 
  when (is_integer(N) andalso (N > 0)) andalso 
       (is_integer(BufSz) andalso (BufSz > 0)) ->
    V;
enc_setopt_value(otp, rcvctrlbuf, V, _, _, _) when (V =:= default) ->
    0;
enc_setopt_value(otp, rcvctrlbuf, V, _, _, _) when is_integer(V) andalso (V > 0) ->
    V;
enc_setopt_value(otp, sndctrlbuf, V, _, _, _) when (V =:= default) ->
    0;
enc_setopt_value(otp, sndctrlbuf, V, _, _, _) when is_integer(V) andalso (V > 0) ->
    V;
enc_setopt_value(otp, meta, V, _, _, _) ->
    V;
enc_setopt_value(otp = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(socket, bindtodevice, V, _D, _T, _P) when is_list(V) ->
    V;
enc_setopt_value(socket, broadcast, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, debug, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, dontroute, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, keepalive, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, linger, abort, D, T, P) ->
    enc_setopt_value(socket, linger, {true, 0}, D, T, P);
enc_setopt_value(socket, linger, {OnOff, Secs} = V, _D, _T, _P)
  when is_boolean(OnOff) andalso is_integer(Secs) andalso (Secs >= 0) ->
    V;
enc_setopt_value(socket, oobinline, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, passcred, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, peek_off = Opt, V, _D, _T, _P) when is_integer(V) ->
    %% V;
    not_supported(Opt);
enc_setopt_value(socket, priority, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, rcvbuf, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, rcvlowat, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, rcvtimeo, #{sec := Sec, usec := USec} = V, _D, _T, _P) 
  when is_integer(Sec) andalso is_integer(USec) ->
    V;
enc_setopt_value(socket, reuseaddr, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, reuseport, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket, sndbuf, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, sndlowat, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(socket, sndtimeo, #{sec := Sec, usec := USec} = V, _D, _T, _P) 
  when is_integer(Sec) andalso is_integer(USec) ->
    V;
enc_setopt_value(socket, timestamp, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(socket = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(ip, add_membership, #{multiaddr := MA,
                                       interface := IF} = V, _D, _T, _P)
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       ((IF =:= any) orelse (is_tuple(IF) andalso (size(IF) =:= 4))) ->
    V;
enc_setopt_value(ip, add_source_membership, #{multiaddr  := MA,
                                              interface  := IF,
                                              sourceaddr := SA} = V, _D, _T, _P)
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       (is_tuple(IF) andalso (size(IF) =:= 4)) andalso
       (is_tuple(SA) andalso (size(SA) =:= 4)) ->
    V;
enc_setopt_value(ip, block_source, #{multiaddr  := MA,
                                     interface  := IF,
                                     sourceaddr := SA} = V, _D, _T, _P)
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       (is_tuple(IF) andalso (size(IF) =:= 4)) andalso
       (is_tuple(SA) andalso (size(SA) =:= 4)) ->
    V;
enc_setopt_value(ip, drop_membership, #{multiaddr := MA,
                                        interface := IF} = V, _D, _T, _P)
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       ((IF =:= any) orelse (is_tuple(IF) andalso (size(IF) =:= 4))) ->
    V;
enc_setopt_value(ip, drop_source_membership, #{multiaddr  := MA,
                                               interface  := IF,
                                               sourceaddr := SA} = V, _D, _T, _P)
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       (is_tuple(IF) andalso (size(IF) =:= 4)) andalso
       (is_tuple(SA) andalso (size(SA) =:= 4)) ->
    V;
enc_setopt_value(ip, freebind, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(ip, hdrincl, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(ip, minttl, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(ip, msfilter, null = V, _D, _T, _P) ->
    V;
enc_setopt_value(ip, msfilter, #{multiaddr := MA,
                                 interface := IF,
                                 fmode     := FMode,
                                 slist     := SL} = V, _D, _T, _P) 
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       (is_tuple(IF) andalso (size(IF) =:= 4)) andalso
       ((FMode =:= include) orelse (FMode =:= exclude)) andalso 
       is_list(SL) ->
    ensure_ip_msfilter_slist(SL),
    V;
enc_setopt_value(ip, mtu_discover, V, _D, _T, _P)
  when (V =:= want)  orelse
       (V =:= dont)  orelse
       (V =:= do)    orelse
       (V =:= probe) orelse
       is_integer(V) ->
    V;
enc_setopt_value(ip, multicast_all, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, multicast_if, V, _D, _T, _P)
  when (V =:= any) orelse (is_tuple(V) andalso (size(V) =:= 4)) ->
    V;
enc_setopt_value(ip, multicast_loop, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, multicast_ttl, V, _D, _T, _P)
  when is_integer(V) andalso (0 =< V) andalso (V =< 255) ->
    V;
enc_setopt_value(ip, nodefrag, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, pktinfo, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recvdstaddr, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recverr, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recvif, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recvopts, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recvorigdstaddr, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recvtos, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, recvttl, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, retopts, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, router_alert, V, _D, _T, _P)
  when is_integer(V) ->
    V;
enc_setopt_value(ip, sendsrcaddr, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, tos, V, _D, _T, _P)
  when (V =:= lowdelay)    orelse
       (V =:= throughput)  orelse
       (V =:= reliability) orelse
       (V =:= mincost)     orelse
       is_integer(V) ->
    V;
enc_setopt_value(ip, transparent, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ip, ttl, V, _D, _T, _P)
  when is_integer(V) ->
    V;
enc_setopt_value(ip, unblock_source, #{multiaddr  := MA,
                                       interface  := IF,
                                       sourceaddr := SA} = V, _D, _T, _P)
  when (is_tuple(MA) andalso (size(MA) =:= 4)) andalso
       (is_tuple(IF) andalso (size(IF) =:= 4)) andalso
       (is_tuple(SA) andalso (size(SA) =:= 4)) ->
    V;
enc_setopt_value(ip = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(ipv6, addrform, inet = V, _D, _T, _P) ->
    enc_domain(V);
enc_setopt_value(ipv6, add_membership, #{multiaddr := MA,
                                         interface := IF} = V, _D, _T, _P)
  when ((is_tuple(MA) andalso (size(MA) =:= 8)) andalso
        (is_integer(IF) andalso (IF >= 0))) ->
    V;
%% Is this obsolete? When get, the result is enoprotoopt and in the 
%% header file it says 'obsolete'...
%% But there might be (old?) versions of linux where it still works...
enc_setopt_value(ipv6, authhdr, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, dstopts, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, drop_membership, #{multiaddr := MA,
                                          interface := IF} = V, _D, _T, _P)
  when ((is_tuple(MA) andalso (size(MA) =:= 8)) andalso
        (is_integer(IF) andalso (IF >= 0))) ->
    V;
enc_setopt_value(ipv6, flowinfo, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, hoplimit, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, hopopts, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, mtu, V, _D, _T, _P) when is_integer(V) ->
    V;
enc_setopt_value(ipv6, mtu_discover, V, _D, _T, _P)
  when (V =:= want)  orelse
       (V =:= dont)  orelse
       (V =:= do)    orelse
       (V =:= probe) orelse
       is_integer(V) ->
    V;
enc_setopt_value(ipv6, multicast_hops, V, _D, _T, _P)
  when (V =:= default) ->
    -1;
enc_setopt_value(ipv6, multicast_hops, V, _D, _T, _P)
  when is_integer(V) andalso (V >= 0) andalso (V =< 255) ->
    V;
enc_setopt_value(ipv6, multicast_if, V, _D, _T, _P)
  when is_integer(V) ->
    V;
enc_setopt_value(ipv6, multicast_loop, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ipv6, recverr, V, _D, _T, _P)
  when is_boolean(V) ->
    V;
enc_setopt_value(ipv6, recvhoplimit, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, Opt, V, _D, _T, _P)
  when ((Opt =:= recvpktinfo) orelse (Opt =:= pktinfo)) andalso 
       is_boolean(V) ->
    V;
enc_setopt_value(ipv6, recvtclass, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, router_alert, V, _D, T, _P)
  when is_integer(V) andalso (T =:= raw) ->
    V;
enc_setopt_value(ipv6, rthdr, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, tclass, V, _D, T, _P)
  when is_boolean(V) andalso ((T =:= dgram) orelse (T =:= raw)) ->
    V;
enc_setopt_value(ipv6, unicast_hops, V, _D, _T, _P)
  when (V =:= default) ->
    -1;
enc_setopt_value(ipv6, unicast_hops, V, _D, _T, _P)
  when is_integer(V) andalso (V >= 0) andalso (V =< 255) ->
    V;
enc_setopt_value(ipv6, v6only, V, _D, _T, _P) when is_boolean(V) ->
    V;
enc_setopt_value(ipv6 = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

enc_setopt_value(tcp, congestion, V, _D, T, P)
  when is_list(V) andalso
       (T =:= stream) andalso
       (P =:= tcp) ->
    V;
enc_setopt_value(tcp, cork, V, _D, T, P)
  when is_boolean(V) andalso (T =:= stream) andalso (P =:= tcp) ->
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
  when is_boolean(V) andalso (T =:= dgram) andalso (P =:= udp) ->
    V;
enc_setopt_value(udp = L, Opt, _V, _D, _T, _P) ->
    not_supported({L, Opt});

enc_setopt_value(sctp, associnfo, #{assoc_id       := AssocId,
                                    asocmaxrxt     := MaxRxt,
                                    num_peer_dests := NumPeerDests,
                                    peer_rwnd      := PeerRWND,
                                    local_rwnd     := LocalRWND,
                                    cookie_life    := CLife} = V,
                 _D, _T, P)
  when is_integer(AssocId) andalso
       is_integer(MaxRxt)       andalso (MaxRxt >= 0)       andalso
       is_integer(NumPeerDests) andalso (NumPeerDests >= 0) andalso
       is_integer(PeerRWND)     andalso (PeerRWND >= 0)     andalso
       is_integer(LocalRWND)    andalso (LocalRWND >= 0)    andalso
       is_integer(CLife)        andalso (CLife >= 0)        andalso
       (P =:= sctp) ->
    V;
enc_setopt_value(sctp, autoclose, V, _D, _T, P)
  when is_integer(V) andalso (V >= 0) andalso (P =:= sctp) ->
    V;
enc_setopt_value(sctp, disable_fragments, V, _D, _T, P)
  when is_boolean(V) andalso (P =:= sctp) ->
    V;
enc_setopt_value(sctp, events, #{data_in          := DataIn,
                                 association      := Assoc,
                                 address          := Addr,
                                 send_failure     := SndFailure,
                                 peer_error       := PeerError,
                                 shutdown         := Shutdown,
                                 partial_delivery := PartialDelivery,
                                 adaptation_layer := AdaptLayer,
                                 authentication   := Auth,
                                 sender_dry       := SndDry} = V, _D, _T, P)
  when is_boolean(DataIn)          andalso
       is_boolean(Assoc)           andalso
       is_boolean(Addr)            andalso
       is_boolean(SndFailure)      andalso
       is_boolean(PeerError)       andalso
       is_boolean(Shutdown)        andalso
       is_boolean(PartialDelivery) andalso
       is_boolean(AdaptLayer)      andalso
       is_boolean(Auth)            andalso
       is_boolean(SndDry)          andalso
       (P =:= sctp) ->
    V;
enc_setopt_value(sctp, initmsg, #{num_outstreams := NumOut,
                                  max_instreams  := MaxIn,
                                  max_attempts   := MaxAttempts,
                                  max_init_timeo := MaxInitTO} = V,
                 _D, _T, P)
  when is_integer(NumOut)      andalso (NumOut >= 0)      andalso
       is_integer(MaxIn)       andalso (MaxIn >= 0)       andalso
       is_integer(MaxAttempts) andalso (MaxAttempts >= 0) andalso
       is_integer(MaxInitTO)   andalso (MaxInitTO >= 0)   andalso
       (P =:= sctp) ->
    V;
enc_setopt_value(sctp, maxseg, V, _D, _T, P)
  when is_integer(V) andalso (V >= 0) andalso (P =:= sctp) ->
    V;
enc_setopt_value(sctp, nodelay, V, _D, _T, P)
  when is_boolean(V) andalso (P =:= sctp) ->
    V;
enc_setopt_value(sctp, rtoinfo, #{assoc_id := AssocId,
                                  initial  := Init,
                                  max      := Max,
                                  min      := Min} = V,
                 _D, _T, P)
  when is_integer(AssocId) andalso
       is_integer(Init) andalso (Init >= 0) andalso
       is_integer(Max)  andalso (Max >= 0)  andalso
       is_integer(Min)  andalso (Min >= 0)  andalso
       (P =:= sctp) ->
    V;
enc_setopt_value(sctp = L, Opt, V, _D, _T, _P) ->
    not_supported({L, Opt, V});

%% enc_setopt_value(raw = L, Opt, _V, _D, _T, _P) ->
%%     not_supported({L, Opt});

%% Is this correct? What about getopt?
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
%% values we may need to do an actual decode.
%%

%% This string is NULL-terminated, but the general function we use
%% in the nif code does not know that. So, deal with it here.
dec_getopt_value(tcp = _L, congestion = _Opt, Alg, _D, _T, _P) when is_list(Alg) ->
    {Str, _} = lists:splitwith(fun(0) -> false; (_) -> true end, Alg),
    Str;
%% Let the user deal with the rest for now...
dec_getopt_value(_L, _Opt, V, _D, _T, _P) ->
    V.



%% +++ Encode socket option key +++

%% Most options are usable both for set and get, but some are
%% are only available for e.g. get.
-spec enc_sockopt_key(Level, Opt, Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: otp,
      Direction :: set | get,
      Opt       :: otp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                     (Level, Opt, Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: socket,
      Direction :: set | get,
      Opt       :: socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                     (Level, Opt, Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: ip,
      Direction :: set | get,
      Opt       :: ip_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                     (Level, Opt, Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: ipv6,
      Direction :: set | get,
      Opt       :: ipv6_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                   (Level, Opt, Direction,
                    Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: tcp,
      Direction :: set | get,
      Opt       :: tcp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                     (Level, Opt, Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: udp,
      Direction :: set | get,
      Opt       :: udp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                     (Level, Opt, Direction,
                      Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: sctp,
      Direction :: set | get,
      Opt       :: sctp_socket_option(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                   (Level, Opt, Direction,
                    Domain, Type, Protocol) -> non_neg_integer() when
      Level     :: integer(),
      Direction :: set,
      Opt       :: integer(),
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol();
                   (Level, Opt, Direction,
                    Domain, Type, Protocol) -> {NativeOpt, ValueSize} when
      Level     :: integer(),
      Direction :: get,
      Opt       :: {NativeOpt, ValueSize},
      NativeOpt :: integer(),
      ValueSize :: non_neg_integer() | 'int' | 'bool',
      Domain    :: domain(),
      Type      :: type(),
      Protocol  :: protocol().


%% +++ OTP socket options +++
enc_sockopt_key(otp, debug, _, _, _, _) ->
    ?ESOCK_OPT_OTP_DEBUG;
enc_sockopt_key(otp, iow, _, _, _, _) ->
    ?ESOCK_OPT_OTP_IOW;
enc_sockopt_key(otp, controlling_process, _, _, _, _) ->
    ?ESOCK_OPT_OTP_CTRL_PROC;
enc_sockopt_key(otp, rcvbuf, _, _, _, _) ->
    ?ESOCK_OPT_OTP_RCVBUF;
enc_sockopt_key(otp, rcvctrlbuf, _, _, _, _) ->
    ?ESOCK_OPT_OTP_RCVCTRLBUF;
enc_sockopt_key(otp, sndctrlbuf, _, _, _, _) ->
    ?ESOCK_OPT_OTP_SNDCTRLBUF;
enc_sockopt_key(otp, fd, get = _Dir, _, _, _) ->
    ?ESOCK_OPT_OTP_FD;
enc_sockopt_key(otp, meta, _, _, _, _) ->
    ?ESOCK_OPT_OTP_META;
enc_sockopt_key(otp = L, Opt, _, _, _, _) ->
    not_supported({L, Opt});

%% +++ SOCKET socket options +++
enc_sockopt_key(socket = _L, acceptconn = _Opt, get = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_ACCEPTCONN;
enc_sockopt_key(socket = L, acceptconn = Opt, Dir, _D, _T, _P) ->
    not_supported({L, Opt, Dir});
enc_sockopt_key(socket = L, acceptfilter = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
%% Before linux 3.8, this socket option could be set but not get.
%% Maximum size of buffer for name: IFNAMSIZ
%% So, we let the implementation decide.
enc_sockopt_key(socket = _L, bindtodevice = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_BINDTODEVICE;
enc_sockopt_key(socket, broadcast = _Opt, _Dir, _D, dgram = _T, _P) ->
    ?ESOCK_OPT_SOCK_BROADCAST;
enc_sockopt_key(socket = L, busy_poll = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(socket = _L, debug = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_DEBUG;
enc_sockopt_key(socket, domain = _Opt, get = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_DOMAIN;
enc_sockopt_key(socket, dontroute = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_DONTROUTE;
enc_sockopt_key(socket = L, error = Opt, get = _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
%% This is only for connection-oriented sockets, but who are those?
%% Type = stream or Protocol = tcp?
%% For now, we just let is pass and it will fail later if not ok...
enc_sockopt_key(socket, keepalive = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_KEEPALIVE;
enc_sockopt_key(socket, linger = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_LINGER;
enc_sockopt_key(socket = L, mark = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(socket = _L, oobinline = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_OOBINLINE;
enc_sockopt_key(socket, passcred, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_PASSCRED;
enc_sockopt_key(socket = L, peek_off = Opt, _Dir, local = _D, _T, _P) ->
    %% ?ESOCK_OPT_SOCK_PEEK_OFF;
    not_supported({L, Opt});
enc_sockopt_key(socket = L, peercred = Opt, get = _Dir, local = _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(socket, priority = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_PRIORITY;
enc_sockopt_key(socket, protocol = _Opt, get = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_PROTOCOL;
enc_sockopt_key(socket, rcvbuf = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_RCVBUF;
enc_sockopt_key(socket = L, rcvbufforce = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
%% May not work on linux.
enc_sockopt_key(socket = _L, rcvlowat = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_RCVLOWAT;
enc_sockopt_key(socket = _L, rcvtimeo = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_RCVTIMEO;
enc_sockopt_key(socket = _L, reuseaddr = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_REUSEADDR;
enc_sockopt_key(socket = _L, reuseport = _Opt, _Dir, D, _T, _P)
  when ((D =:= inet) orelse (D =:= inet6)) ->
    ?ESOCK_OPT_SOCK_REUSEPORT;
enc_sockopt_key(socket = L, rxq_ovfl = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(socket = L, setfib = Opt, set = _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(socket = _L, sndbuf = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_SNDBUF;
enc_sockopt_key(socket = L, sndbufforce = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
%% Not changeable on linux.
enc_sockopt_key(socket = _L, sndlowat = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_SNDLOWAT;
enc_sockopt_key(socket = _L, sndtimeo = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_SNDTIMEO;
enc_sockopt_key(socket = _L, timestamp = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_TIMESTAMP;
enc_sockopt_key(socket = _L, type = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SOCK_TYPE;
enc_sockopt_key(socket = L, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown({L, UnknownOpt});

%% +++ IP socket options +++
enc_sockopt_key(ip = _L, add_membership = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_ADD_MEMBERSHIP;
enc_sockopt_key(ip = _L, add_source_membership = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_ADD_SOURCE_MEMBERSHIP;
enc_sockopt_key(ip = _L, block_source = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_BLOCK_SOURCE;
%% FreeBSD only?
%% Only respected on udp and raw ip (unless the hdrincl option has been set).
enc_sockopt_key(ip = L, dontfrag = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ip = _L, drop_membership = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_DROP_MEMBERSHIP;
enc_sockopt_key(ip = _L, drop_source_membership = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_DROP_SOURCE_MEMBERSHIP;
%% Linux only?
enc_sockopt_key(ip = _L, freebind = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_FREEBIND;
enc_sockopt_key(ip = _L, hdrincl = _Opt, _Dir, _D, raw = _T, _P) ->
    ?ESOCK_OPT_IP_HDRINCL;
enc_sockopt_key(ip = _L, minttl = _Opt, _Dir, _D, raw = _T, _P) ->
    ?ESOCK_OPT_IP_MINTTL;
enc_sockopt_key(ip = _L, msfilter = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MSFILTER;
enc_sockopt_key(ip = _L, mtu = _Opt, get = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MTU;
enc_sockopt_key(ip = _L, mtu_discover = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MTU_DISCOVER;
enc_sockopt_key(ip = _L, multicast_all = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MULTICAST_ALL;
enc_sockopt_key(ip = _L, multicast_if = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MULTICAST_IF;
enc_sockopt_key(ip = _L, multicast_loop = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MULTICAST_LOOP;
enc_sockopt_key(ip = _L, multicast_ttl = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_MULTICAST_TTL;
enc_sockopt_key(ip = _L, nodefrag = _Opt, _Dir, _D, raw = _T, _P) ->
    ?ESOCK_OPT_IP_NODEFRAG;
enc_sockopt_key(ip = L, options = Opt, _Dir, _D, _T, _P) ->
    not_supported({Opt, L});
enc_sockopt_key(ip = _L, pktinfo = _Opt, _Dir, _D, dgram = _T, _P) ->
    ?ESOCK_OPT_IP_PKTINFO;
enc_sockopt_key(ip = _L, recvdstaddr = _Opt, _Dir, _D, T, _P) when (T =:= dgram) ->
    ?ESOCK_OPT_IP_RECVDSTADDR;
enc_sockopt_key(ip = _L, recverr = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_RECVERR;
enc_sockopt_key(ip = _L, recvif = _Opt, _Dir, _D, T, _P)
  when (T =:= dgram) orelse (T =:= raw) ->
    ?ESOCK_OPT_IP_RECVIF;
enc_sockopt_key(ip = _L, recvopts = _Opt, _Dir, _D, T, _P) when (T =/= stream) ->
    ?ESOCK_OPT_IP_RECVOPTS;
enc_sockopt_key(ip = _L, recvorigdstaddr = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_RECVORIGDSTADDR;
enc_sockopt_key(ip, recvtos = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_RECVTOS;
enc_sockopt_key(ip = _L, recvttl = _Opt, _Dir, _D, T, _P) when (T =/= stream) ->
    ?ESOCK_OPT_IP_RECVTTL;
enc_sockopt_key(ip = _L, retopts = _Opt, _Dir, _D, T, _P) when (T =/= stream) ->
    ?ESOCK_OPT_IP_RETOPTS;
enc_sockopt_key(ip, router_alert = _Opt, _Dir, _D, raw = _T, _P) ->
    ?ESOCK_OPT_IP_ROUTER_ALERT;
enc_sockopt_key(ip, sendsrcaddr = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_SENDSRCADDR;
%% On FreeBSD it specifies that this option is only valid
%% for stream, dgram and "some" raw sockets...
%% No such condition on linux (in the man page)...
enc_sockopt_key(ip, tos = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_TOS;
enc_sockopt_key(ip = _L, transparent = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_TRANSPARENT;
enc_sockopt_key(ip, ttl = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_TTL;
enc_sockopt_key(ip = _L, unblock_source = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IP_UNBLOCK_SOURCE;
enc_sockopt_key(ip = L, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown({L, UnknownOpt});

%% IPv6 socket options
enc_sockopt_key(ipv6 = _L, addrform = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_ADDRFORM;
enc_sockopt_key(ipv6, add_membership = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_ADD_MEMBERSHIP;
enc_sockopt_key(ipv6 = _L, authhdr = _Opt, _Dir, _D, T, _P) 
  when ((T =:= dgram) orelse (T =:= raw)) ->
    ?ESOCK_OPT_IPV6_AUTHHDR;
enc_sockopt_key(ipv6 = L, auth_level = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = L, checksum = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6, drop_membership = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_DROP_MEMBERSHIP;
enc_sockopt_key(ipv6 = _L, dstopts = _Opt, _Dir, _D, T, _P)
  when (T =:= dgram) orelse (T =:= raw) ->
    ?ESOCK_OPT_IPV6_DSTOPTS;
enc_sockopt_key(ipv6 = L, esp_trans_level = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = L, esp_network_level = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = _L, flowinfo = _Opt, _Dir, _D, T, _P)
  when (T =:= dgram) orelse (T =:= raw) ->
    ?ESOCK_OPT_IPV6_FLOWINFO;
enc_sockopt_key(ipv6, hoplimit = _Opt, _Dir, _D, T, _P)
  when (T =:= dgram) orelse (T =:= raw) ->
    ?ESOCK_OPT_IPV6_HOPLIMIT;
enc_sockopt_key(ipv6 = _L, hopopts = _Opt, _Dir, _D, T, _P) 
  when ((T =:= dgram) orelse (T =:= raw)) ->
    ?ESOCK_OPT_IPV6_HOPOPTS;
enc_sockopt_key(ipv6 = L, ipcomp_level = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = L, join_group = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = L, leave_group = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = _L, mtu = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_MTU;
enc_sockopt_key(ipv6 = _L, mtu_discover = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_MTU_DISCOVER;
enc_sockopt_key(ipv6 = _L, multicast_hops = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_MULTICAST_HOPS;
enc_sockopt_key(ipv6 = _L, multicast_if = _Opt, _Dir, _D, T, _P) 
  when (T =:= dgram) orelse (T =:= raw) ->
    ?ESOCK_OPT_IPV6_MULTICAST_IF;
enc_sockopt_key(ipv6 = _L, multicast_loop = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_MULTICAST_LOOP;
enc_sockopt_key(ipv6 = L, portrange = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = L, pktoptions = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = _L, recverr = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_RECVERR;
enc_sockopt_key(ipv6, recvhoplimit = _Opt, _Dir, _D, T, _P)
  when (T =:= dgram) orelse (T =:= raw) ->
    ?ESOCK_OPT_IPV6_RECVHOPLIMIT;
enc_sockopt_key(ipv6 = _L, Opt, _Dir, _D, T, _P) 
  when ((Opt =:= recvpktinfo) orelse (Opt =:= pktinfo)) andalso 
       ((T =:= dgram) orelse (T =:= raw)) ->
    ?ESOCK_OPT_IPV6_RECVPKTINFO;
enc_sockopt_key(ipv6 = _L, recvtclass = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_RECVTCLASS;
enc_sockopt_key(ipv6 = _L, router_alert = _Opt, _Dir, _D, T, _P) when (T =:= raw) ->
    ?ESOCK_OPT_IPV6_ROUTER_ALERT;
enc_sockopt_key(ipv6 = _L, rthdr = _Opt, _Dir, _D, T, _P) 
  when ((T =:= dgram) orelse (T =:= raw)) ->
    ?ESOCK_OPT_IPV6_RTHDR;
enc_sockopt_key(ipv6 = _L, tclass = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_TCLASS;
enc_sockopt_key(ipv6 = _L, unicast_hops = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_UNICAST_HOPS;
enc_sockopt_key(ipv6 = L, use_min_mtu = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(ipv6 = _L, v6only = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_IPV6_V6ONLY;
enc_sockopt_key(ipv6 = L, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown({L, UnknownOpt});

%% TCP socket options
%% There are other options that would be useful; info,
%% but they are difficult to get portable...
enc_sockopt_key(tcp, congestion = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_TCP_CONGESTION;
enc_sockopt_key(tcp = _L, cork = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_TCP_CORK;
enc_sockopt_key(tcp = L, keepidle = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp = L, keepintvl = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp = L, keepcnt = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp, maxseg = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_TCP_MAXSEG;
enc_sockopt_key(tcp = L, md5sig = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp, nodelay = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_TCP_NODELAY;
enc_sockopt_key(tcp = L, noopt = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp = L, nopush = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp = L, syncnt = Opt, _Dir, _D, _T, _P) -> % Only set? 1..255
    not_supported({L, Opt});
enc_sockopt_key(tcp = L, user_timeout = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(tcp = L, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown({L, UnknownOpt});

%% UDP socket options
enc_sockopt_key(udp, cork = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_UDP_CORK;
enc_sockopt_key(udp = L, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown({L, UnknownOpt});

%% SCTP socket options
enc_sockopt_key(sctp = L, adaption_layer = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = _L, associnfo = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_ASSOCINFO;
enc_sockopt_key(sctp = L, auth_active_key = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, auth_asconf = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, auth_chunk = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, auth_key = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, auth_delete_key = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp, autoclose = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_AUTOCLOSE;
enc_sockopt_key(sctp = L, context = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, default_send_params = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, delayed_ack_time = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = _L, disable_fragments = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_DISABLE_FRAGMENTS;
enc_sockopt_key(sctp = L, hmac_ident = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = _L, events = _Opt, set = _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_EVENTS;
enc_sockopt_key(sctp = L, explicit_eor = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, fragment_interleave = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, get_peer_addr_info = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = _L, initmsg = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_INITMSG;
enc_sockopt_key(sctp = L, i_want_mapped_v4_addr = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, local_auth_chunks = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = _L, maxseg = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_MAXSEG;
enc_sockopt_key(sctp = L, maxburst = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp, nodelay = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_NODELAY;
enc_sockopt_key(sctp = L, partial_delivery_point = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, peer_addr_params = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, peer_auth_chunks = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, primary_addr = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, reset_streams = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = _L, rtoinfo = _Opt, _Dir, _D, _T, _P) ->
    ?ESOCK_OPT_SCTP_RTOINFO;
enc_sockopt_key(sctp = L, set_peer_primary_addr = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, status = Opt, get = _Dir, _D, _T, _P) ->
    not_supported({L, Opt}); % ?ESOCK_OPT_SCTP_RTOINFO;
enc_sockopt_key(sctp = L, use_exp_recvinfo = Opt, _Dir, _D, _T, _P) ->
    not_supported({L, Opt});
enc_sockopt_key(sctp = L, UnknownOpt, _Dir, _D, _T, _P) ->
    unknown({L, UnknownOpt});

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
    ?ESOCK_SHUTDOWN_HOW_READ;
enc_shutdown_how(write) ->
    ?ESOCK_SHUTDOWN_HOW_WRITE;
enc_shutdown_how(read_write) ->
    ?ESOCK_SHUTDOWN_HOW_READ_WRITE.




%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

-dialyzer({nowarn_function, ensure_ip_msfilter_slist/1}).
ensure_ip_msfilter_slist(SL) ->
    EnsureSA = fun(SA) when is_tuple(SA) andalso (size(SA) =:= 4) -> ok;
                  (_) -> einval()
               end,
    lists:foreach(EnsureSA, SL).


ensure_sockaddr(#{family := inet} = SockAddr) ->
    maps:merge(?SOCKADDR_IN4_DEFAULTS, SockAddr);
ensure_sockaddr(#{family := inet6} = SockAddr) ->
    maps:merge(?SOCKADDR_IN6_DEFAULTS, SockAddr);
ensure_sockaddr(#{family := local, path := Path} = SockAddr) 
  when is_list(Path) andalso 
       (length(Path) > 0) andalso 
       (length(Path) =< 255) ->
    BinPath = unicode:characters_to_binary(Path, file:native_name_encoding()),
    ensure_sockaddr(SockAddr#{path => BinPath});
ensure_sockaddr(#{family := local, path := Path} = SockAddr) 
  when is_binary(Path) andalso 
       (byte_size(Path) > 0) andalso 
       (byte_size(Path) =< 255) ->
    SockAddr;
ensure_sockaddr(SockAddr) ->
    invalid_address(SockAddr).



cancel(SockRef, Op, OpRef) ->
    case nif_cancel(SockRef, Op, OpRef) of
        %% The select has already completed
        {error, select_sent} ->
            flush_select_msg(SockRef, OpRef),
            _ = flush_abort_msg(SockRef, OpRef),
            ok;
        Other ->
            _ = flush_abort_msg(SockRef, OpRef),
            Other
    end.

flush_select_msg(SockRef, Ref) ->
    receive
        {?ESOCK_TAG, #socket{ref = SockRef}, select, Ref} ->
            ok
    after 0 ->
            ok
    end.

flush_abort_msg(SockRef, Ref) ->
    receive
        {?ESOCK_TAG, #socket{ref = SockRef}, abort, {Ref, Reason}} ->
            Reason
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


deadline(Timeout) ->
    case Timeout of
        nowait -> Timeout;
        infinity -> Timeout;
        _ when is_integer(Timeout), 0 =< Timeout ->
            timestamp() + Timeout;
        _ ->
            invalid_timeout(Timeout)
    end.

timeout(Deadline) ->
    case Deadline of
        nowait -> 0;
        infinity -> infinity;
        _ ->
            Now = timestamp(),
            if
                Now < Deadline -> Deadline - Now;
                true -> 0
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



%% ===========================================================================
%%
%% Error functions
%%
%% ===========================================================================

-spec invalid_domain(Domain) -> no_return() when
      Domain :: term().
invalid_domain(Domain) ->
    error({invalid_domain, Domain}).

-spec invalid_type(Type) -> no_return() when
      Type :: term().
invalid_type(Type) ->
    error({invalid_type, Type}).

-spec invalid_protocol(Proto) -> no_return() when
      Proto :: term().
invalid_protocol(Proto) ->
    error({invalid_protocol, Proto}).

-spec invalid_address(SockAddr) -> no_return() when
      SockAddr :: term().
invalid_address(SockAddr) ->
    error({invalid_address, SockAddr}).

-spec invalid_timeout(Timeout) -> no_return() when
      Timeout :: term().
invalid_timeout(Timeout) ->
    error({invalid_timeout, Timeout}).

-spec not_supported(What) -> no_return() when
      What :: term().
not_supported(What) ->
    error({not_supported, What}).

-spec unknown(What) -> no_return() when
      What :: term().
unknown(What) ->
    error({unknown, What}).

-spec einval() -> no_return().
einval() ->
    error(einval).

-spec error(Reason) -> no_return() when
      Reason :: term().
error(Reason) ->
    throw({error, Reason}).


%% ===========================================================================
%%
%% Below follows the actual NIF-functions.
%%
%% ===========================================================================

nif_info() ->
    erlang:nif_error(undef).

nif_info(_SRef) ->
    erlang:nif_error(undef).

nif_command(_Command) ->
    erlang:nif_error(undef).

nif_supports(_Key) ->
    erlang:nif_error(undef).

nif_open(_Domain, _Type, _Protocol, _Extra) ->
    erlang:nif_error(undef).

nif_bind(_SRef, _SockAddr) ->
    erlang:nif_error(undef).

nif_bind(_SRef, _SockAddrs, _Action) ->
    erlang:nif_error(undef).

nif_connect(_SRef, _SockAddr) ->
    erlang:nif_error(undef).

nif_finalize_connection(_SRef) ->
    erlang:nif_error(undef).

nif_listen(_SRef, _Backlog) ->
    erlang:nif_error(undef).

nif_accept(_SRef, _Ref) ->
    erlang:nif_error(undef).

nif_send(_SockRef, _SendRef, _Data, _Flags) ->
    erlang:nif_error(undef).

nif_sendto(_SRef, _SendRef, _Data, _Dest, _Flags) ->
    erlang:nif_error(undef).

nif_sendmsg(_SRef, _SendRef, _MsgHdr, _Flags) ->
    erlang:nif_error(undef).

nif_recv(_SRef, _RecvRef, _Length, _Flags) ->
    erlang:nif_error(undef).

nif_recvfrom(_SRef, _RecvRef, _Length, _Flags) ->
    erlang:nif_error(undef).

nif_recvmsg(_SRef, _RecvRef, _BufSz, _CtrlSz, _Flags) ->
    erlang:nif_error(undef).

nif_cancel(_SRef, _Op, _Ref) ->
    erlang:nif_error(undef).

nif_close(_SRef) ->
    erlang:nif_error(undef).

nif_shutdown(_SRef, _How) ->
    erlang:nif_error(undef).

nif_finalize_close(_SRef) ->
    erlang:nif_error(undef).

nif_setopt(_Ref, _IsEnc, _Lev, _Key, _Val) ->
    erlang:nif_error(undef).

nif_getopt(_Ref, _IsEnc, _Lev, _Key) ->
    erlang:nif_error(undef).

nif_sockname(_Ref) ->
    erlang:nif_error(undef).

nif_peername(_Ref) ->
    erlang:nif_error(undef).

