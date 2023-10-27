%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

-compile({no_auto_import, [error/1, monitor/1]}).

%% Administrative and "global" utility functions
-export([
	 %% (registry) Socket functions
         number_of/0,
         which_sockets/0, which_sockets/1,

	 %% (registry) Socket monitor functions
         number_of_monitors/0, number_of_monitors/1,
         which_monitors/1,
	 monitored_by/1,

         debug/1, socket_debug/1, use_registry/1,
	 info/0, info/1,
	 i/0, i/1, i/2,
         tables/0, table/1,
         monitor/1, cancel_monitor/1,
         supports/0, supports/1, supports/2,
         is_supported/1, is_supported/2, is_supported/3,

	 to_list/1
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

         sendfile/2, sendfile/3, sendfile/4, sendfile/5,

         recv/1, recv/2, recv/3, recv/4,
         recvfrom/1, recvfrom/2, recvfrom/3, recvfrom/4,
         recvmsg/1, recvmsg/2, recvmsg/3, recvmsg/4, recvmsg/5,

         close/1,
         shutdown/2,

         setopt/3, setopt_native/3, setopt/4,
         getopt/2, getopt_native/3, getopt/3,

         sockname/1,
         peername/1,

         ioctl/2, ioctl/3, ioctl/4,

         cancel/2
        ]).

%% Misc utility functions
-export([
	 which_socket_kind/1,
         options/0, options/1, options/2, option/1, option/2,
         protocols/0, protocol/1
	]).

-export_type([
              socket/0,
              socket_handle/0,

              select_tag/0,
              select_handle/0,
              select_info/0,

              completion_tag/0,
              completion_handle/0,
              completion_info/0,

              invalid/0,
              eei/0,

              socket_counters/0,
              socket_info/0,

              domain/0,
              type/0,
              protocol/0,

              port_number/0,
              in_addr/0,
              in6_addr/0,
              sockaddr/0, sockaddr_recv/0,
              sockaddr_in/0,
              sockaddr_in6/0,
              sockaddr_un/0,
              sockaddr_ll/0,
              sockaddr_dl/0,
              sockaddr_unspec/0,
              sockaddr_native/0,

              msg_flag/0,

              level/0,
              otp_socket_option/0,
              socket_option/0,

              %% Option values' types
              linger/0,
              timeval/0,
              ip_mreq/0,
              ip_mreq_source/0,
              ip_msfilter/0,
              ip_pmtudisc/0,
              ip_tos/0,
              ip_pktinfo/0,

              ipv6_mreq/0,
              ipv6_pmtudisc/0,
              ipv6_hops/0,
              ipv6_pktinfo/0,

              sctp_assocparams/0,
              sctp_event_subscribe/0,
              sctp_initmsg/0,
              sctp_rtoinfo/0,

              msg/0, msg_send/0, msg_recv/0,
              cmsg/0, cmsg_send/0, cmsg_recv/0,

              ee_origin/0,
              icmp_dest_unreach/0,
              icmpv6_dest_unreach/0,
              extended_err/0,

	      info_keys/0
             ]).

%% DUMMY
-export_type([ioctl_device_flag/0, ioctl_device_map/0]).

%% We need #file_descriptor{} for sendfile/2,3,4,5
-include("file_int.hrl").
%% -include("socket_int.hrl").

%% -define(DBG(T),
%%         erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

%% Also in prim_socket
-define(REGISTRY, socket_registry).

-type invalid() :: {invalid, What :: term()}.

%% Extended Error Information
-type eei() :: #{info := econnreset | econnaborted |
                 netname_deleted | too_many_cmds | atom(),
                 raw_info := term()}.

-type info() ::
        #{counters     := #{atom() := non_neg_integer()},
          iov_max      := non_neg_integer(),
          use_registry := boolean(),
          io_backend   := #{name := atom()}}.

-type socket_counters() :: #{read_byte        := non_neg_integer(),
                             read_fails       := non_neg_integer(),
                             read_pkg         := non_neg_integer(),
                             read_pkg_max     := non_neg_integer(),
                             read_tries       := non_neg_integer(),
                             read_waits       := non_neg_integer(),
                             write_byte       := non_neg_integer(),
                             write_fails      := non_neg_integer(),
                             write_pkg        := non_neg_integer(),
                             write_pkg_max    := non_neg_integer(),
                             write_tries      := non_neg_integer(),
                             write_waits      := non_neg_integer(),
                             sendfile         => non_neg_integer(),
                             sendfile_byte    => non_neg_integer(),
                             sendfile_fails   => non_neg_integer(),
                             sendfile_max     => non_neg_integer(),
                             sendfile_pkg     => non_neg_integer(),
                             sendfile_pkg_max => non_neg_integer(),
                             sendfile_tries   => non_neg_integer(),
                             sendfile_waits   => non_neg_integer(),
                             acc_success      := non_neg_integer(),
                             acc_fails        := non_neg_integer(),
                             acc_tries        := non_neg_integer(),
                             acc_waits        := non_neg_integer()}.

-type socket_info() :: #{domain        := domain() | integer(),
                         type          := type() | integer(),
                         protocol      := protocol() | integer(),
                         owner         := pid(),
                         ctype         := normal | fromfd | {fromfd, integer()},
                         counters      := socket_counters(),
                         num_readers   := non_neg_integer(),
                         num_writers   := non_neg_integer(),
                         num_acceptors := non_neg_integer(),
                         writable      := boolean(),
                         readable      := boolean(),
			 rstates       := [atom()],
			 wstates       := [atom()]}.


%% We support only a subset of all domains.
-type domain() :: inet | inet6 | local | unspec.

%% We support only a subset of all types.
%% RDM - Reliably Delivered Messages
-type type()   :: stream | dgram | raw | rdm | seqpacket.

%% We support all protocols enumerated by getprotoent(),
%% and all of ip | ipv6 | tcp | udp | sctp that are supported
%% by the platform, even if not enumerated by getprotoent()
-type protocol() :: atom().

-type port_number() :: 0..65535.

-type in_addr() :: {0..255, 0..255, 0..255, 0..255}.

-type in6_flow_info() :: 0..16#FFFFF.
-type in6_scope_id()  :: 0..16#FFFFFFFF.

-type in6_addr() ::
           {0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535}.

-type linger() ::
        #{onoff  := boolean(),
          linger := non_neg_integer()}.

-type timeval() ::
        #{sec  := integer(),
          usec := integer()}.

-type ip_mreq() ::
        #{multiaddr := in_addr(),
          interface := in_addr()}.

-type ip_mreq_source() ::
        #{multiaddr  := in_addr(),
          interface  := in_addr(),
          sourceaddr := in_addr()}.

-type ip_msfilter() ::
        #{multiaddr := in_addr(),
          interface := in_addr(),
          mode      := 'include' | 'exclude',
          slist     := [ in_addr() ]}.

-type ip_pmtudisc() ::
        want | dont | do | probe.

%% If the integer value is used, its up to the caller to ensure its valid!
-type ip_tos() :: lowdelay |
                  throughput |
                  reliability |
                  mincost.

-type ip_pktinfo() ::
        #{ifindex  := non_neg_integer(), % Interface Index
          spec_dst := in_addr(),         % Local Address
          addr     := in_addr()          % Header Destination address
         }.


-type ipv6_mreq() ::
        #{multiaddr := in6_addr(),
          interface := non_neg_integer()}.

-type ipv6_pmtudisc() ::
        want | dont | do | probe.

-type ipv6_hops() ::
        default | 0..255.

-type ipv6_pktinfo() ::
        #{addr    := in6_addr(),
          ifindex := integer()
         }.

-type sctp_assocparams() ::
        #{assoc_id                := integer(),
          asocmaxrxt              := 0..16#ffff,
          numbe_peer_destinations := 0..16#ffff,
          peer_rwnd               := 0..16#ffffffff,
          local_rwnd              := 0..16#ffffffff,
          cookie_life             := 0..16#ffffffff}.

-type sctp_event_subscribe() ::
        #{data_io          := boolean(),
          association      := boolean(),
          address          := boolean(),
          send_failure     := boolean(),
          peer_error       := boolean(),
          shutdown         := boolean(),
          partial_delivery := boolean(),
          adaptation_layer => boolean(),
          sender_dry       => boolean()}.

-type sctp_initmsg() ::
        #{num_ostreams   := 0..16#ffff,
          max_instreams  := 0..16#ffff,
          max_attempts   := 0..16#ffff,
          max_init_timeo := 0..16#ffff}.

-type sctp_rtoinfo() ::
        #{assoc_id := integer(),
          initial  := 0..16#ffffffff,
          max      := 0..16#ffffffff,
          min      := 0..16#ffffffff}.

-type packet_type() :: host | broadcast | multicast | otherhost |
                       outgoing | loopback | user | kernel | fastroute |
                       non_neg_integer().

-type hatype() :: netrom | eether | ether | ax25 | pronet | chaos |
                  ieee802 | arcnet | appletlk | dlci | atm | metricom |
                  ieee1394 | eui64 | infiniband |
                  tunnel | tunnel6 | loopback | localtlk |
                  none | void |
                  non_neg_integer().

-type sockaddr_un() ::
        #{family := 'local',
          path   := binary() | string()}.
-type sockaddr_in() ::
        #{family := 'inet',
          port   := port_number(),
          %% The 'broadcast' here is the "limited broadcast"
          addr   := 'any' | 'broadcast' | 'loopback' | in_addr()}.
-type sockaddr_in6() ::
        #{family   := 'inet6',
          port     := port_number(),
          addr     := 'any' | 'loopback' | in6_addr(),
          flowinfo := in6_flow_info(),
          scope_id := in6_scope_id()}.
-type sockaddr_ll() ::
        #{family   := 'packet',
          protocol := non_neg_integer(),
          ifindex  := integer(),
          pkttype  := packet_type(),
          hatype   := hatype(),
          addr     := binary()}.
-type sockaddr_dl() ::
        #{family   := 'link',
          index    := non_neg_integer(),
          type     := non_neg_integer(),
          nlen     := non_neg_integer(),
          alen     := non_neg_integer(),
          slen     := non_neg_integer(),
          data     := binary()}.
-type sockaddr_unspec() ::
        #{family := 'unspec', addr := binary()}.
-type sockaddr_native() ::
        #{family := integer(), addr := binary()}.
-type sockaddr() ::
        sockaddr_in()      |
        sockaddr_in6()     |
        sockaddr_un()      |
        sockaddr_ll()      |
        sockaddr_dl()      |
        sockaddr_unspec()  |
        sockaddr_native().

-type sockaddr_recv() ::
        sockaddr() | binary().

%% (otp)      - This option is internal to our (OTP) implementation.
%% socket     - The socket layer (SOL_SOCKET).
%% (Int)      - Raw level, sent down and used "as is".
%% protocol() - Protocol number; ip | ipv6 | tcp | udp | sctp | ...
-type level() ::
        %% otp | % Has got own clauses in setopt/getopt
        %% integer() % Has also got own clauses
        socket | %% Handled explicitly
        protocol().

%% There are some options that are 'read-only'.
%% Should those be included here or in a special list?
%% Should we just document it and leave it to the user?
%% Or catch it in the encode functions?
%% A setopt for a readonly option leads to {error, invalid()}?
%% Do we really need a sndbuf?

-type otp_socket_option() ::
        debug |
        iow |
        controlling_process |
        rcvbuf | % sndbuf |
        rcvctrlbuf |
        sndctrlbuf |
        meta |
        use_registry |
        fd |
        domain.

-type socket_option() ::
        {Level :: socket,
         Opt ::
           acceptconn |
           acceptfilter |
           bindtodevice |
           broadcast |
           bsp_state |
           busy_poll |
           debug |
           domain |
           dontroute |
           error |
           exclusiveaddruse |
           keepalive |
           linger |
           mark |
           maxdg |
           max_msg_size |
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
           type} |
        {Level :: ip,
         Opt ::
           add_membership |
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
           recvdstaddr |
           recverr |
           recvif |
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
           unblock_source} |
        {Level :: ipv6,
         Opt ::
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
           v6only} |
        {Level :: tcp,
         Opt ::
           congestion |
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
           user_timeout} |
        {Level :: udp, Opt :: cork} |
        {Level :: sctp,
         Opt ::
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
           use_ext_recvinfo}.


%% The names of these macros match the names of corresponding
%%C functions in the NIF code, so a search will match both
%%
-define(socket_tag, '$socket').
%%
%% Our socket abstract data type
-define(socket(Ref), {?socket_tag, (Ref)}).
%%
%% Messages sent from the nif-code to erlang processes:
-define(socket_msg(Socket, Tag, Info), {?socket_tag, (Socket), (Tag), (Info)}).

-type socket()          :: ?socket(socket_handle()).
-opaque socket_handle() :: reference().


%% Some flags are used for send, others for recv, and yet again
%% others are found in a cmsg().  They may occur in multiple locations..
-type msg_flag() ::
        cmsg_cloexec |
        confirm |
        ctrunc |
        dontroute |
        eor |
        errqueue |
        more |
        oob |
        peek |
        trunc.

-type msg() :: msg_send() | msg_recv().

-type msg_send() ::
        #{
           %% *Optional* target address
           %% Used on an unconnected socket to specify the
           %% destination address for a message.
           addr => sockaddr(),
                    
           iov := erlang:iovec(),

           %% *Optional* control message list (ancillary data).
           %% The maximum size of the control buffer is platform
           %% specific. It is the users responsibility to ensure
           %% that its not exceeded.
           %%
           ctrl  =>
               ([cmsg_send() |
                 #{level := level() | integer(),
                   type  := integer(),
                   data  := binary()}])
         }.

-type msg_recv() ::
        #{
           %% *Optional* target address
           %% Used on an unconnected socket to return the
           %% source address for a message.
           addr => sockaddr_recv(),

           iov := erlang:iovec(),

           %% Control messages (ancillary data).
           %% The maximum size of the control buffer is platform
           %% specific. It is the users responsibility to ensure
           %% that its not exceeded.
           %%
           ctrl :=
               ([cmsg_recv() |
                 #{level := level() | integer(),
                   type  := integer(),
                   data  := binary()}]),

           %% Received message flags
           flags := [msg_flag() | integer()]
         }.


%% We are able to (completely) decode *some* control message headers.
%% Even if we are able to decode both level and type, we may not be
%% able to decode the data.  The data is always delivered as a binary()
%% and a decoded value is delivered in the 'value' field, if decoding
%% is successful.

-type cmsg() :: cmsg_recv() | cmsg_send().

-type cmsg_recv() ::
        #{level := socket,  type := timestamp,    data := binary(),
          value => timeval()}                                       |
        #{level := socket,  type := rights,       data := binary()} |
        #{level := socket,  type := credentials,  data := binary()} |
        #{level := ip,      type := tos,          data := binary(),
          value => ip_tos() | integer()}                            |
        #{level := ip,      type := recvtos,      data := binary(),
          value := ip_tos() | integer()}                            |
        #{level := ip,      type := ttl,          data := binary(),
          value => integer()}                                       |
        #{level := ip,      type := recvttl,      data := binary(),
          value := integer()}                                       |
        #{level := ip,      type := pktinfo,      data := binary(),
          value => ip_pktinfo()}                                    |
        #{level := ip,      type := origdstaddr,  data := binary(),
          value => sockaddr_recv()}                                 |
        #{level := ip,      type := recverr,      data := binary(),
          value => extended_err()}                                  |
        #{level := ipv6,    type := hoplimit,     data := binary(),
          value => integer()}                                       |
        #{level := ipv6,    type := pktinfo,      data := binary(),
          value => ipv6_pktinfo()}                                  |
        #{level := ipv6,    type := recverr,      data := binary(),
          value => extended_err()}                                  |
        #{level := ipv6,    type := tclass,       data := binary(),
          value =>        integer()}.

-type native_value() :: integer() | boolean() | binary().
%% Possible to add type tagged values a'la {uint16, 0..16#FFFF}

-type cmsg_send() ::
        #{level := socket,  type := timestamp,    data => native_value(),
          value => timeval()}                                             |
        #{level := socket,  type := rights,       data := native_value()} |
        #{level := socket,  type := credentials,  data := native_value()} |
        #{level := ip,      type := tos,          data => native_value(),
          value => ip_tos() | integer()}                                  |
        #{level := ip,      type := ttl,          data => native_value(),
          value => integer()}                                             |
        #{level := ip,      type := hoplimit,     data => native_value(),
          value => integer()}                                             |
        #{level := ipv6,    type := tclass,       data => native_value(),
          value => integer()}.

-type ee_origin() :: none | local | icmp | icmp6.
-type icmp_dest_unreach() ::
        net_unreach | host_unreach | port_unreach | frag_needed |
        net_unknown | host_unknown.
-type icmpv6_dest_unreach() ::
        noroute | adm_prohibited | not_neighbour | addr_unreach |
        port_unreach | policy_fail | reject_route.
-type extended_err() ::
        #{error    := posix(),
          origin   := icmp,
          type     := dest_unreach,
          code     := icmp_dest_unreach() | 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := icmp,
          type     := time_exceeded | 0..16#FF,
          code     := 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := icmp6,
          type     := dest_unreach,
          code     := icmpv6_dest_unreach() | 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := icmp6,
          type     := pkt_toobig | time_exceeded | 0..16#FF,
          code     := 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()} |
        #{error    := posix(),
          origin   := ee_origin() | 0..16#FF,
          type     := 0..16#FF,
          code     := 0..16#FF,
          info     := 0..16#FFFFFFFF,
          data     := 0..16#FFFFFFFF,
          offender := sockaddr_recv()}.

-type posix() :: inet:posix().

-type info_keys() :: [
		      'domain' | 'type' | 'protocol' |
		      'fd' | 'owner' |
		      'local_address' | 'remote_address' |
		      'recv' | 'sent' |
		      'state'
		     ].


%% Note that not all flags exist on all platforms!
-type ioctl_device_flag() :: up | broadcast | debug | loopback | pointopoint |
                             notrailers | knowsepoch | running | noarp | promisc | allmulti |
                             master | oactive | slave | simplex |
			     link0 | link1 | link2 |
			     multicast | portsel | automedia |
			     cantconfig | ppromisc |
                             dynamic |
			     monitor | staticarp | dying | renaming | nogroup |
			     lower_up | dormant | echo.

%% When reading the device map (gifmap), the resulting map will be 
%% "fully" populated.
%% <DOES-THIS-WORK>
%% When writing, it is expected that only the fields that is
%% to be set is present.
%% </DOES-THIS-WORK>
-type ioctl_device_map() :: #{mem_start := non_neg_integer(),
                              mem_end   := non_neg_integer(),
                              base_addr := non_neg_integer(),
                              irq       := non_neg_integer(),
                              dma       := non_neg_integer(),
                              port      := non_neg_integer()}.


%% ===========================================================================
%%
%% Interface term formats
%%

-define(ASYNCH_DATA_TAG, (recv | recvfrom | recvmsg |
                          send | sendto | sendmsg)).
-define(ASYNCH_TAG,      ((accept | connect) | ?ASYNCH_DATA_TAG)).

%% -type asynch_data_tag() :: send | sendto | sendmsg |
%%                            recv | recvfrom | recvmsg |
%%                            sendfile.
%% -type asynch_tag()      :: connect | accept |
%%                            asynch_data_tag().
%% -type select_tag()      :: asynch_tag() |
%%                            {asynch_data_tag(), ContData :: term()}.
%% -type completion_tag()  :: asynch_tag().
-type select_tag()      :: ?ASYNCH_TAG | sendfile | 
                           {?ASYNCH_DATA_TAG | sendfile, ContData :: term()}.
-type completion_tag()  :: ?ASYNCH_TAG.

-type select_handle() :: reference().
-type completion_handle() :: reference().

-type select_info() ::
        {select_info,
         SelectTag :: select_tag(),
         SelectHandle :: select_handle()}.
-type completion_info() ::
        {completion_info,
         CompletionTag :: completion_tag(),
         CompletionHandle :: completion_handle()}.

-define(SELECT_INFO(Tag, SelectHandle),
        {select_info, Tag, SelectHandle}).

-define(COMPLETION_INFO(Tag, CompletionHandle),
        {completion_info, Tag, CompletionHandle}).


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
%% Returns a list of all the sockets, according to the filter rule.
%%

-spec which_sockets() -> [socket()].

which_sockets() ->
    ?REGISTRY:which_sockets(true).

-spec which_sockets(FilterRule) -> [socket()] when
	FilterRule :: 'inet' | 'inet6' | 'local' |
	'stream' | 'dgram' | 'seqpacket' |
	'sctp' | 'tcp' | 'udp' |
	pid() |
	fun((socket_info()) -> boolean()).

which_sockets(Domain)
  when Domain =:= inet;
       Domain =:= inet6;
       Domain =:= local ->
    ?REGISTRY:which_sockets({domain, Domain});

which_sockets(Type)
  when Type =:= stream;
       Type =:= dgram;
       Type =:= seqpacket ->
    ?REGISTRY:which_sockets({type, Type});

which_sockets(Proto)
  when Proto =:= sctp;
       Proto =:= tcp;
       Proto =:= udp ->
    ?REGISTRY:which_sockets({protocol, Proto});

which_sockets(Owner)
  when is_pid(Owner) ->
    ?REGISTRY:which_sockets({owner, Owner});

which_sockets(Filter) when is_function(Filter, 1) ->
    ?REGISTRY:which_sockets(Filter);

which_sockets(Other) ->
    erlang:error(badarg, [Other]).




%% *** number_of_monitors ***
%%
%% Interface function to the socket registry
%% returns the number of existing socket monitors.
%%

-spec number_of_monitors() -> non_neg_integer().

number_of_monitors() ->
    ?REGISTRY:number_of_monitors().

-spec number_of_monitors(pid()) -> non_neg_integer().

number_of_monitors(Pid) when is_pid(Pid) ->
    ?REGISTRY:number_of_monitors(Pid).


%% *** which_monitors/1 ***
%%
%% Interface function to the socket registry
%% Returns a list of all the monitors of the process or socket.
%%

-spec which_monitors(Pid) -> [reference()] when
      Pid :: pid();
                    (Socket) -> [reference()] when
      Socket :: socket().

which_monitors(Pid) when is_pid(Pid) ->
    ?REGISTRY:which_monitors(Pid);
which_monitors(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    ?REGISTRY:which_monitors(Socket);
which_monitors(Socket) ->
    erlang:error(badarg, [Socket]).


%% *** monitor_by/1 ***
%%
%% Interface function to the socket registry
%% Returns a list of all the process'es monitoring the socket.
%%

-spec monitored_by(Socket) -> [reference()] when
						Socket :: socket().

monitored_by(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    ?REGISTRY:monitored_by(Socket);
monitored_by(Socket) ->
    erlang:error(badarg, [Socket]).


%% *** to_list/1 ***
%%
%% This is intended to convert a socket() to a printable string.
%%

-spec to_list(Socket) -> list() when
      Socket :: socket().
    
to_list(?socket(SockRef)) when is_reference(SockRef) ->
    "#Ref" ++ Id = erlang:ref_to_list(SockRef),
    "#Socket" ++ Id;
to_list(Socket) ->
    erlang:error(badarg, [Socket]).


%% *** which_socket_kind/1 ***
%%
%% Utility function that returns the "kind" of socket.
%% That is, if its a "plain" socket or a compatibillity socket.
%%

-spec which_socket_kind(Socket :: socket()) -> plain | compat.

which_socket_kind(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    case prim_socket:getopt(SockRef, {otp,meta}) of
	{ok, undefined} ->
	    plain;
	{ok, _} ->
	    compat;
	{error, _} ->
	    erlang:error(badarg, [Socket])
    end;
which_socket_kind(Socket) ->
    erlang:error(badarg, [Socket]).



%% ===========================================================================
%%
%% Debug features
%%
%% ===========================================================================


-spec debug(D :: boolean()) -> 'ok'.
%%
debug(D) when is_boolean(D) ->
    prim_socket:debug(D);
debug(D) ->
    erlang:error(badarg, [D]).


-spec socket_debug(D :: boolean()) -> 'ok'.
%%
socket_debug(D) when is_boolean(D) ->
    prim_socket:socket_debug(D);
socket_debug(D) ->
    erlang:error(badarg, [D]).



-spec use_registry(D :: boolean()) -> 'ok'.
%%
use_registry(D) when is_boolean(D) ->
    prim_socket:use_registry(D).


tables() ->
    #{protocols      => table(protocols),
      options        => table(options),
      ioctl_requests => table(ioctl_requests),
      ioctl_flags    => table(ioctl_flags),
      msg_flags      => table(msg_flags)}.

table(Table) ->
    prim_socket:p_get(Table).


%% ===========================================================================
%%
%% i/0,1,2 - List sockets
%%
%% This produces a list of "all" the sockets, and some info about each one.
%% This function is intended as a utility and debug function.
%% The sockets can be selected from domain, type or protocol.
%% The sockets are not sorted.
%% 
%% ===========================================================================

-spec default_info_keys() -> info_keys().

default_info_keys() ->
    [
     domain, type, protocol, fd, owner,
     local_address, remote_address,
     recv, sent,
     state
    ].

-spec i() -> ok.
     
i() ->
    do_i(which_sockets(), default_info_keys()).

-spec i(InfoKeys) -> ok when
        InfoKeys :: info_keys();
       (Domain) -> ok when
        Domain :: inet | inet6 | local;
       (Proto) -> ok when
        Proto :: sctp | tcp | udp;
       (Type) -> ok when
        Type :: dgram | seqpacket | stream.

i(InfoKeys) when is_list(InfoKeys) ->
    do_i(which_sockets(), InfoKeys);
i(Domain) when (Domain =:= inet) orelse
	       (Domain =:= inet6) orelse
	       (Domain =:= local) ->
    do_i(which_sockets(Domain), default_info_keys());
i(Proto) when (Proto =:= tcp) orelse
	      (Proto =:= udp) orelse
	      (Proto =:= sctp) ->
    do_i(which_sockets(Proto), default_info_keys());
i(Type) when (Type =:= dgram) orelse
	     (Type =:= seqpacket) orelse
	     (Type =:= stream) ->
    do_i(which_sockets(Type), default_info_keys()).

-spec i(Domain, InfoKeys) -> ok when
        Domain :: inet | inet6 | local,
	InfoKeys :: info_keys();
       (Proto, InfoKeys) -> ok when
	Proto :: sctp | tcp | udp,
	InfoKeys :: info_keys();
       (Type, InfoKeys) -> ok when
	Type :: dgram | seqpacket | stream,
	InfoKeys :: info_keys().

i(Domain, InfoKeys)
  when ((Domain =:= inet) orelse
	(Domain =:= inet6) orelse
	(Domain =:= local)) andalso
       is_list(InfoKeys) ->
    do_i(which_sockets(Domain), InfoKeys);
i(Proto, InfoKeys)
  when ((Proto =:= tcp) orelse
	(Proto =:= udp) orelse
	(Proto =:= sctp)) andalso
       is_list(InfoKeys) ->
    do_i(which_sockets(Proto), InfoKeys);
i(Type, InfoKeys)
  when ((Type =:= dgram) orelse
	(Type =:= seqpacket) orelse
	(Type =:= stream)) andalso
       is_list(InfoKeys) ->
    do_i(which_sockets(Type), InfoKeys).

do_i(Sockets, InfoKeys) ->
    Lines = case i_sockets(Sockets, InfoKeys) of
		[] -> [];
		InfoLines -> [header_line(InfoKeys) | InfoLines]
	    end,
    Maxs = lists:foldl(fun(Line, Max0) -> smax(Max0, Line) end,
		       lists:duplicate(length(InfoKeys), 0), Lines),
    Fmt = lists:append(["~-" ++ integer_to_list(N) ++ "s " ||
			   N <- Maxs]) ++ "~n",
    lists:foreach(fun(Line) -> io:format(Fmt, Line) end, Lines).

header_line(Fields) ->
    [header_field(atom_to_list(F)) || F <- Fields].
header_field([C | Cs]) ->
    [string:to_upper(C) | header_field_rest(Cs)].
header_field_rest([$_, C | Cs]) ->
    [$\s, string:to_upper(C) | header_field_rest(Cs)];
header_field_rest([C|Cs]) ->
    [C | header_field_rest(Cs)];
header_field_rest([]) ->
    [].

smax([Max|Ms], [Str|Strs]) ->
    N = length(Str),
    [if N > Max -> N; true -> Max end | smax(Ms, Strs)];
smax([], []) ->
    [].

i_sockets(Sockets, InfoKeys) ->
    [i_socket(Socket, InfoKeys) || Socket <- Sockets].

i_socket(Socket, InfoKeys) ->
    %% Most of the stuff we need, is in 'socket info'
    %% so we can just as well get it now.
    Info = #{protocol := Proto} = info(Socket),
    i_socket(Proto, Socket, Info, InfoKeys).

i_socket(Proto, Socket, Info, InfoKeys) ->
    [i_socket_info(Proto, Socket, Info, InfoKey) || InfoKey <- InfoKeys].

i_socket_info(_Proto, _Socket, #{domain := Domain} = _Info, domain) ->
    atom_to_list(Domain);
i_socket_info(_Proto, _Socket, #{type := Type} = _Info, type) ->
    string:to_upper(atom_to_list(Type));
i_socket_info(Proto, _Socket, #{type := Type} = _Info, protocol) ->
    string:to_upper(atom_to_list(if
                                     (Proto =:= 0) ->
                                         case Type of
                                             stream -> tcp;
                                             dgram  -> udp;
                                             _      -> unknown
                                         end;
                                     true ->
                                         Proto
                                 end));
i_socket_info(_Proto, Socket, _Info, fd) ->
    try socket:getopt(Socket, otp, fd) of
	{ok,   FD} -> integer_to_list(FD);
	{error, _} -> " "
    catch
        _:_ -> " "
    end;
i_socket_info(_Proto, _Socket, #{owner := Pid} = _Info, owner) ->
    pid_to_list(Pid);
i_socket_info(Proto, Socket, _Info, local_address) ->
    case sockname(Socket) of
	{ok,  Addr} ->
	    fmt_sockaddr(Addr, Proto);
	{error, _} ->
	    " "
    end;
i_socket_info(Proto, Socket, _Info, remote_address) ->
    try peername(Socket) of
	{ok,  Addr} ->
	    fmt_sockaddr(Addr, Proto);
	{error, _} ->
	    " "
    catch
        _:_ ->
            " "
    end;
i_socket_info(_Proto, _Socket,
	      #{counters := #{read_byte := N}} = _Info, recv) ->
    integer_to_list(N);
i_socket_info(_Proto, _Socket,
	      #{counters := #{write_byte := N}} = _Info, sent) ->
    integer_to_list(N);
i_socket_info(_Proto, _Socket, #{rstates := RStates,
				 wstates := WStates} = _Info, state) ->
    fmt_states(RStates, WStates);
i_socket_info(_Proto, _Socket, _Info, _Key) ->
    " ".

fmt_states([], []) ->
    " ";
fmt_states(RStates, []) ->
    fmt_states(RStates) ++ ", -";
fmt_states([], WStates) ->
    " - , " ++ fmt_states(WStates);
fmt_states(RStates, WStates) ->
    fmt_states(RStates) ++ " , " ++ fmt_states(WStates).

fmt_states([H]) ->
    fmt_state(H);
fmt_states([H|T]) ->
    fmt_state(H) ++ ":"  ++ fmt_states(T).

fmt_state(accepting) ->
    "A";
fmt_state(bound) ->
    "BD";
fmt_state(busy) ->
    "BY";
fmt_state(connected) ->
    "CD";
fmt_state(connecting) ->
    "CG";
fmt_state(listen) ->
    "LN";
fmt_state(listening) ->
    "LG";
fmt_state(open) ->
    "O";
fmt_state(selected) ->
    "SD";
fmt_state(X) when is_atom(X) ->
    string:uppercase(atom_to_list(X)).


fmt_sockaddr(#{family := Fam,
	       addr   := Addr,
	       port   := Port}, Proto)
  when (Fam =:= inet) orelse (Fam =:= inet6) ->
    case Addr of
	{0,0,0,0}         -> "*:" ++ fmt_port(Port, Proto);
	{0,0,0,0,0,0,0,0} -> "*:" ++ fmt_port(Port, Proto);
	{127,0,0,1}       -> "localhost:" ++ fmt_port(Port, Proto);
	{0,0,0,0,0,0,0,1} -> "localhost:" ++ fmt_port(Port, Proto);
	IP                -> inet_parse:ntoa(IP) ++ ":" ++ fmt_port(Port, Proto)
    end;
fmt_sockaddr(#{family := local,
	       path   := Path}, _Proto) ->
    "local:" ++ 
	if is_list(Path) ->
		Path;
	   is_binary(Path) ->
		binary_to_list(Path)
	end.


fmt_port(N, Proto) ->
    case inet:getservbyport(N, Proto) of
	{ok, Name} -> f("~s (~w)", [Name, N]);
	_ -> integer_to_list(N)
    end.


%% ===========================================================================
%%
%% info - Get miscellaneous information about a socket
%% or about the socket library.
%%
%% Generates a list of various info about the socket, such as counter values.
%%
%% Do *not* call this function often.
%% 
%% ===========================================================================

-spec info() -> info().
%%
info() ->
    try
        prim_socket:info()
    catch error:undef:ST ->
            case ST of
                %% We rewrite errors coming from prim_socket not existing
                %% to enotsup.
                [{prim_socket,info,[],_}|_] ->
                    erlang:raise(error,notsup,ST);
                _ ->
                    erlang:raise(error,undef,ST)
            end
    end.

-spec info(Socket) -> socket_info() when
					Socket :: socket().
%%
info(?socket(SockRef)) when is_reference(SockRef) ->
    prim_socket:info(SockRef);
info(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% monitor - Monitor a socket
%%
%% If a socket "dies", a down message, similar to erlang:monitor, will be
%% sent to the requesting process:
%%
%%       {'DOWN', MonitorRef, socket, Socket, Info}
%%
%% ===========================================================================

-spec monitor(Socket) -> reference() when
      Socket :: socket().

monitor(?socket(SockRef) = Socket) when is_reference(SockRef) ->
    case prim_socket:setopt(SockRef, {otp, use_registry}, true) of
        ok ->
            socket_registry:monitor(Socket);
        {error, closed = SReason} ->
            MRef = make_ref(),
            self() ! {'DOWN', MRef, socket, Socket, SReason},
	    MRef
    end;
monitor(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% cancel_monitor - Cancel a socket monitor
%%
%% If MRef is a reference that the socket obtained
%% by calling monitor/1, this monitoring is turned off.
%% If the monitoring is already turned off, nothing happens.
%%
%% ===========================================================================

-spec cancel_monitor(MRef) -> boolean() when
      MRef :: reference().

cancel_monitor(MRef) when is_reference(MRef) ->
    case socket_registry:cancel_monitor(MRef) of
	ok ->
	    true;
	{error, unknown_monitor} ->
	    false;
	{error, not_owner} ->
	    erlang:error(badarg, [MRef]);
	{error, Reason} ->
	    erlang:error({invalid, Reason})
    end;
cancel_monitor(MRef) ->
    erlang:error(badarg, [MRef]).


%% ===========================================================================
%%
%% supports - get information about what the platform "supports".
%%
%% Generates a list of various info about what the platform can support. 
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
     || Key1 <- [ioctl_requests, ioctl_flags,
                 options, msg_flags, protocols]]
        ++ prim_socket:supports().

-spec supports(Key1 :: term()) ->
                      [{Key2 :: term(),
                        boolean() | [{Key3 :: term(),
                                      boolean()}]}].
%%
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
    prim_socket:is_supported(Key1).
%%
-spec is_supported(Key1 :: term(), Key2 :: term()) ->
                          boolean().
is_supported(Key1, Key2) ->
    prim_socket:is_supported(Key1, Key2).
%%
%% Undocumented legacy function
is_supported(options, Level, Opt) when is_atom(Level), is_atom(Opt) ->
    is_supported(options, {Level,Opt}).


options() ->
    lists:sort(supports(options)).

options(Level) ->
    [{Opt, Supported} || {{Lvl, Opt}, Supported} <- options(), (Lvl =:= Level)].

options(Level, Supported) ->
    [Opt || {Opt, Sup} <- options(Level), (Sup =:= Supported)].

option({Level, Opt}) ->
    lists:member(Opt, options(Level, true)).
option(Level, Opt) ->
    option({Level, Opt}).


protocols() ->
    lists:sort(supports(protocols)).

protocol(Proto) ->
    case lists:keysearch(Proto, 1, protocols()) of
        {value, {Proto, Supported}} ->
            Supported;
        false ->
            false
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
%% We may therefore need monitor function(s): 
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

-spec open(FD) -> {'ok', Socket} | {'error', Reason} when
      FD     :: integer(),
      Socket :: socket(),
      Reason ::
        posix() | 'domain' | 'type' | 'protocol'.

open(FD) when is_integer(FD) ->
    open(FD, #{});
open(FD) ->
    erlang:error(badarg, [FD]).
                  
-spec open(FD, Opts) -> {'ok', Socket} | {'error', Reason} when
      FD       :: integer(),
      Opts     ::
        #{'domain'       => domain() | integer(),
          'type'         => type() | integer(),
          'protocol'     => 'default' | protocol() | integer(),
          'dup'          => boolean(),
	  'debug'        => boolean(),
	  'use_registry' => boolean()},
      Socket   :: socket(),
      Reason   ::
        posix() | 'domain' | 'type' | 'protocol';

          (Domain, Type) -> {'ok', Socket} | {'error', Reason} when
      Domain   :: domain() | integer(),
      Type     :: type() | integer(),
      Socket   :: socket(),
      Reason   :: posix() | 'protocol'.

open(FD, Opts) when is_map(Opts) ->
    if
        is_integer(FD) ->
            case prim_socket:open(FD, Opts) of
                {ok, SockRef} ->
                    Socket = ?socket(SockRef),
                    {ok, Socket};
                {error, _} = ERROR ->
                    ERROR
            end;
        true ->
            erlang:error(badarg, [FD, Opts])
    end;
open(Domain, Type) ->
    open(Domain, Type, 0).

-spec open(Domain, Type, Opts) -> {'ok', Socket} | {'error', Reason} when
      Domain   :: domain() | integer(),
      Type     :: type() | integer(),
      Opts     :: map(),
      Socket   :: socket(),
      Reason   :: posix() | 'protocol';
          (Domain, Type, Protocol) -> {'ok', Socket} | {'error', Reason} when
      Domain   :: domain() | integer(),
      Type     :: type() | integer(),
      Protocol :: 'default' | protocol() | integer(),
      Socket   :: socket(),
      Reason   :: posix() | 'protocol'.

open(Domain, Type, Opts) when is_map(Opts) ->
    open(Domain, Type, 0, Opts);
open(Domain, Type, Protocol) ->
    open(Domain, Type, Protocol, #{}).

-spec open(Domain, Type, Protocol, Opts) ->
                  {'ok', Socket} | {'error', Reason} when
      Domain   :: domain() | integer(),
      Type     :: type() | integer(),
      Protocol :: 'default' | protocol() | integer(),
      Opts     ::
        #{'netns'        => string(),
	  'debug'        => boolean(),
	  'use_registry' => boolean()},
      Socket   :: socket(),
      Reason   :: posix() | 'protocol'.

open(Domain, Type, Protocol, Opts) when is_map(Opts) ->
    case prim_socket:open(Domain, Type, Protocol, Opts) of
        {ok, SockRef} ->
            Socket = ?socket(SockRef),
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

-spec bind(Socket, Addr) -> 'ok' | {'error', Reason} when
      Socket    :: socket(),
      Addr      :: sockaddr() | 'any' | 'broadcast' | 'loopback',
      Reason    :: posix() | 'closed' | invalid().

bind(?socket(SockRef), Addr) when is_reference(SockRef) ->
    if
        Addr =:= any;
        Addr =:= broadcast;
        Addr =:= loopback ->
            case prim_socket:getopt(SockRef, {otp, domain}) of
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
        is_atom(Addr) ->
            {error, {invalid, {sockaddr, Addr}}};
        true ->
            prim_socket:bind(SockRef, Addr)
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

-spec bind(Socket, Addrs, Action) -> 'ok' | {'error', Reason} when
      Socket :: socket(),
      Addrs  :: [sockaddr()],
      Action :: 'add' | 'remove',
      Reason :: posix() | 'closed'.

bind(?socket(SockRef), Addrs, Action)
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

-spec connect(Socket, SockAddr) ->
                     'ok' |
                     {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: posix() | 'closed' | invalid() | 'already'.

connect(Socket, SockAddr) ->
    connect(Socket, SockAddr, infinity).


-spec connect(Socket, SockAddr, Timeout :: 'nowait') ->
                     'ok' |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      SockAddr       :: sockaddr(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() | 'already' |
                        'not_bound' |
                        {add_socket,             posix()} |
                        {update_connect_context, posix()};

             (Socket, SockAddr, Handle :: select_handle() | completion_handle()) ->
                     'ok' |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      SockAddr       :: sockaddr(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() | 'already' |
                        'not_bound' |
                        {add_socket,             posix()} |
                        {update_connect_context, posix()};

             (Socket, SockAddr, Timeout :: 'infinity') ->
                     'ok' |
                     {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: posix() | 'closed' | invalid() | 'already' |
                  'not_bound' |
                  {add_socket,             posix()} |
                  {update_connect_context, posix()};

             (Socket, SockAddr, Timeout :: non_neg_integer()) ->
                     'ok' |
                     {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr(),
      Reason   :: posix() | 'closed' | invalid() | 'already' |
                  'not_bound' | 'timeout' |
                  {add_socket,             posix()} |
                  {update_connect_context, posix()}.

%% <KOLLA>
%% Is it possible to connect with family = local for the (dest) sockaddr?
%% </KOLLA>
connect(?socket(SockRef), SockAddr, TimeoutOrHandle)
  when is_reference(SockRef) ->
    case deadline(TimeoutOrHandle) of
        invalid ->
            erlang:error({invalid, {timeout, TimeoutOrHandle}});
        nowait ->
            Handle = make_ref(),
            connect_nowait(SockRef, SockAddr, Handle);
        handle ->
            Handle = TimeoutOrHandle,
            connect_nowait(SockRef, SockAddr, Handle);
        Deadline ->
            connect_deadline(SockRef, SockAddr, Deadline)
    end;
connect(Socket, SockAddr, Timeout) ->
    erlang:error(badarg, [Socket, SockAddr, Timeout]).

connect_nowait(SockRef, SockAddr, Handle) ->
    case prim_socket:connect(SockRef, Handle, SockAddr) of
        select ->
            {select, ?SELECT_INFO(connect, Handle)};
        completion ->
            {completion, ?COMPLETION_INFO(connect, Handle)};
        Result ->
            Result
    end.

connect_deadline(SockRef, SockAddr, Deadline) ->
    Ref = make_ref(),
    case prim_socket:connect(SockRef, Ref, SockAddr) of
        select ->
            %% Connecting...
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Ref) ->
                    prim_socket:connect(SockRef);
                ?socket_msg(_Socket, abort, {Ref, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, connect, Ref),
                    {error, timeout}
            end;
        completion ->
            %% Connecting...
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, completion, {Ref, CompletionStatus}) ->
                    CompletionStatus;
                ?socket_msg(_Socket, abort, {Ref, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, connect, Ref),
                    {error, timeout}
            end;
        Result ->
            Result
    end.


-spec connect(Socket) -> 'ok' | {'error', Reason} when
      Socket   :: socket(),
      Reason   :: posix() | 'closed' | invalid().

%% Finalize connect after connect(,, nowait | select_handle())
%% and received select message - see connect_deadline/3 as an example
%%
connect(?socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:connect(SockRef);
connect(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% listen - listen for connections on a socket
%%

-spec listen(Socket) -> 'ok' | {'error', Reason} when
      Socket  :: socket(),
      Reason  :: posix() | 'closed' | 'not_bound'.

listen(Socket) ->
    listen(Socket, ?ESOCK_LISTEN_BACKLOG_DEFAULT).

-spec listen(Socket, Backlog) -> 'ok' | {'error', Reason} when
      Socket  :: socket(),
      Backlog :: integer(),
      Reason  :: posix() | 'closed'.

listen(?socket(SockRef), Backlog)
  when is_reference(SockRef), is_integer(Backlog) ->
    prim_socket:listen(SockRef, Backlog);
listen(Socket, Backlog) ->
    erlang:error(badarg, [Socket, Backlog]).


%% ===========================================================================
%%
%% accept, accept4 - accept a connection on a socket
%%

-spec accept(ListenSocket) -> {'ok', Socket} | {'error', Reason} when
      ListenSocket :: socket(),
      Socket       :: socket(),
      Reason       :: posix() | 'closed' | invalid().

accept(ListenSocket) ->
    accept(ListenSocket, ?ESOCK_ACCEPT_TIMEOUT_DEFAULT).

-spec accept(ListenSocket, Timeout :: 'nowait') ->
                    {'ok', Socket} |
                    {'select', SelectInfo} |
          {'completion', CompletionInfo} |
                    {'error', Reason} when
      ListenSocket    :: socket(),
      Socket          :: socket(),
      SelectInfo      :: select_info(),
      CompletionInfo  :: completion_info(),
      Reason          :: posix() | closed | invalid() |
                         {create_accept_socket,  posix()} |
                         {add_accept_socket,     posix()} |
                         {update_accept_context, posix()};

            (ListenSocket, Handle :: select_handle() | completion_handle()) ->
                    {'ok', Socket} |
                    {'select', SelectInfo} |
                    {'completion', CompletionInfo} |
                    {'error', Reason} when
      ListenSocket      :: socket(),
      Socket            :: socket(),
      SelectInfo        :: select_info(),
      CompletionInfo    :: completion_info(),
      Reason            :: posix() | 'closed' | invalid() |
                           {create_accept_socket,  posix()} |
                           {add_socket,            posix()} |
                           {update_accept_context, posix()};

            (ListenSocket, Timeout :: 'infinity') ->
                    {'ok', Socket} |
                    {'error', Reason} when
      ListenSocket :: socket(),
      Socket       :: socket(),
      Reason       :: posix() | 'closed' | invalid() |
                      {create_accept_socket,  posix()} |
                      {add_socket,            posix()} |
                      {update_accept_context, posix()};

            (ListenSocket, Timeout :: non_neg_integer()) ->
                    {'ok', Socket} |
                    {'error', Reason} when
      ListenSocket :: socket(),
      Socket       :: socket(),
      Reason       :: posix() | 'closed' | invalid() | 'timeout' |
                      {create_accept_socket,  posix()} |
                      {add_socket,            posix()} |
                      {update_accept_context, posix()}.

accept(?socket(LSockRef), Timeout)
  when is_reference(LSockRef) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            accept_nowait(LSockRef, Handle);
        handle ->
            Handle = Timeout,
            accept_nowait(LSockRef, Handle);
        Deadline ->
            accept_deadline(LSockRef, Deadline)
    end;
accept(ListenSocket, Timeout) ->
    erlang:error(badarg, [ListenSocket, Timeout]).

accept_nowait(LSockRef, Handle) ->
    case prim_socket:accept(LSockRef, Handle) of
        select ->
            {select,     ?SELECT_INFO(accept, Handle)};
        completion ->
            {completion, ?COMPLETION_INFO(accept, Handle)};
        Result ->
            accept_result(LSockRef, Handle, Result)
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
                ?socket_msg(?socket(LSockRef), select, AccRef) ->
                    accept_deadline(LSockRef, Deadline);
                ?socket_msg(_Socket, abort, {AccRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(LSockRef, accept, AccRef),
                    {error, timeout}
            end;
        completion ->
            %% Each call is non-blocking, but even then it takes
            %% *some* time, so just to be sure, recalculate before 
            %% the receive.
	    Timeout = timeout(Deadline),
            receive
                %% CompletionStatus = {ok, Socket} | {error, Reason}
                ?socket_msg(?socket(LSockRef), completion,
                            {AccRef, CompletionStatus}) ->
                    CompletionStatus;
                ?socket_msg(_Socket, abort, {AccRef, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(LSockRef, accept, AccRef),
                    {error, timeout}
            end;
        Result ->
            accept_result(LSockRef, AccRef, Result)
    end.

accept_result(LSockRef, AccRef, Result) ->
    case Result of
        {ok, SockRef} ->
            Socket = ?socket(SockRef),
            {ok, Socket};
        {error, _} = ERROR ->
            %% Just to be on the safe side...
            _ = cancel(LSockRef, accept, AccRef),
            ERROR
    end.


%% ===========================================================================
%%
%% send, sendto, sendmsg - send a message on a socket
%%

-spec send(Socket, Data) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid().

send(Socket, Data) ->
    send(Socket, Data, ?ESOCK_SEND_FLAGS_DEFAULT, ?ESOCK_SEND_TIMEOUT_DEFAULT).


-spec send(Socket, Data, Flags) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

          (Socket, Data, Cont) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

          (Socket, Data, Handle :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() |
                        netname_deleted | too_many_cmds | eei();

          (Socket, Data, Handle :: select_handle() | completion_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() |
                        netname_deleted | too_many_cmds | eei();

          (Socket, Data, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid() |
                    netname_deleted | too_many_cmds | eei();

          (Socket, Data, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid() |
                    netname_deleted | too_many_cmds | eei().

send(Socket, Data, Flags_Cont)
  when is_list(Flags_Cont);
       is_tuple(Flags_Cont) ->
    send(Socket, Data, Flags_Cont, ?ESOCK_SEND_TIMEOUT_DEFAULT);
send(Socket, Data, Timeout) ->
    send(Socket, Data, ?ESOCK_SEND_FLAGS_DEFAULT, Timeout).


-spec send(Socket, Data, Flags, Handle :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Flags          :: [msg_flag() | integer()],
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() |
                        netname_deleted | too_many_cmds | eei();

          (Socket, Data, Flags, Handle :: select_handle() | completion_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Flags          :: [msg_flag() | integer()],
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid() |
                        netname_deleted | too_many_cmds | eei();

          (Socket, Data, Flags, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid() |
                    netname_deleted | too_many_cmds | eei();

          (Socket, Data, Flags, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid() |
                    netname_deleted | too_many_cmds | eei();

          (Socket, Data, Cont, SelectHandle :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Cont           :: select_info(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Data, Cont, SelectHandle :: select_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'error', Reason}
                      when
      Socket       :: socket(),
      Data         :: iodata(),
      Cont         :: select_info(),
      RestData     :: binary(),
      SelectInfo   :: select_info(),
      Reason       :: posix() | 'closed' | invalid();

          (Socket, Data, Cont, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

          (Socket, Data, Cont, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid().

send(?socket(SockRef), Data, ?SELECT_INFO(SelectTag, _) = Cont, Timeout)
  when is_reference(SockRef), is_binary(Data) ->
    case SelectTag of
        {send, ContData} ->
            case deadline(Timeout) of
                invalid ->
                    erlang:error({invalid, {timeout, Timeout}});
                nowait ->
                    SelectHandle = make_ref(),
                    send_nowait_cont(SockRef, Data, ContData, SelectHandle);
                handle ->
                    SelectHandle = Timeout,
                    send_nowait_cont(SockRef, Data, ContData, SelectHandle);
                Deadline ->
                    HasWritten = false,
                    send_deadline_cont(
                      SockRef, Data, ContData, Deadline, HasWritten)
            end;
        _ ->
            {error, {invalid, Cont}}
    end;
send(?socket(SockRef), Data, Flags, Timeout)
  when is_reference(SockRef), is_binary(Data), is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            send_nowait(SockRef, Data, Flags, Handle);
        handle ->
            Handle = Timeout,
            send_nowait(SockRef, Data, Flags, Handle);
        Deadline ->
            send_deadline(SockRef, Data, Flags, Deadline)
    end;
send(?socket(SockRef) = Socket, [Bin], Flags, Timeout)
  when is_reference(SockRef), is_binary(Bin) ->
    send(Socket, Bin, Flags, Timeout);
send(?socket(SockRef) = Socket, Data, Flags, Timeout)
  when is_reference(SockRef), is_list(Data) ->
    try erlang:list_to_binary(Data) of
        Bin ->
            send(Socket, Bin, Flags, Timeout)
    catch
        error : badarg ->
            erlang:error({invalid, {data, Data}})
    end;
send(Socket, Data, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Data, Flags, Timeout]).

send_nowait(SockRef, Bin, Flags, Handle) ->
    send_common_nowait_result(
      Handle, send,
      prim_socket:send(SockRef, Bin, Flags, Handle)).

%% On Windows, writes either succeed directly (it their entirety),
%% they are scheduled (completion) or they fail. *No* partial success,
%% and therefor no need to handle theme here (in cont).
send_nowait_cont(SockRef, Bin, Cont, SelectHandle) ->
    send_common_nowait_result(
      SelectHandle, send,
      prim_socket:send(SockRef, Bin, Cont, SelectHandle)).

send_deadline(SockRef, Bin, Flags, Deadline) ->
    Handle = make_ref(),
    HasWritten = false,
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       send, fun send_deadline_cont/5,
       prim_socket:send(SockRef, Bin, Flags, Handle)).

send_deadline_cont(SockRef, Bin, Cont, Deadline, HasWritten) ->
    Handle = make_ref(),
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       send, fun send_deadline_cont/5,
       prim_socket:send(SockRef, Bin, Cont, Handle)).



-compile({inline, [send_common_nowait_result/3]}).
send_common_nowait_result(Handle, Op, Result) ->
    case Result of
        completion ->
            {completion, ?COMPLETION_INFO(Op, Handle)};
        {select, ContData} ->
            {select, ?SELECT_INFO({Op, ContData}, Handle)};
        {select, Data, ContData} ->
            {select, {?SELECT_INFO({Op, ContData}, Handle), Data}};
        %%
        Result ->
            Result
    end.

-compile({inline, [send_common_deadline_result/8]}).
send_common_deadline_result(
  SockRef, Data, Handle, Deadline, HasWritten,
  Op, Fun, SendResult) ->
    %%
    case SendResult of
        {select, Cont} ->
            %% Would block, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Handle) ->
                    Fun(SockRef, Data, Cont, Deadline, HasWritten);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data, HasWritten)
            after Timeout ->
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data, HasWritten)
            end;
        {select, Data_1, Cont} ->
            %% Partial send success, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, Handle) ->
                    Fun(SockRef, Data_1, Cont, Deadline, true);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data_1, true)
            after Timeout ->
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data_1, true)
            end;

        completion ->
            %% Would block, wait for continuation
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, completion, {Handle, CompletionStatus}) ->
                    CompletionStatus;
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    send_common_error(Reason, Data, false)
            after Timeout ->
		    %% ?DBG(['completion send timeout - cancel']),
                    _ = cancel(SockRef, Op, Handle),
                    send_common_error(timeout, Data, false)
            end;

        %%
        {error, {_Reason, RestIOV}} = Error when is_list(RestIOV) ->
            Error;
        {error, Reason} ->
            send_common_error(Reason, Data, HasWritten);
        Result ->
            Result
    end.


send_common_error(Reason, Data, HasWritten) ->
    case HasWritten of
        false ->
            %% We have not managed to send any data;
            %% do not return what remains
            {error, Reason};
        true ->
            %% Error on subsequent send - we have sent some data;
            %% return the remaining
            case Data of
                Bin when is_binary(Bin) ->
                    {error, {Reason, Bin}};
                IOVec when is_list(IOVec) ->
                    {error, {Reason, IOVec}};
                #{iov := IOVec} = _Msg ->
                    {error, {Reason, IOVec}}
            end
    end.


%% ---------------------------------------------------------------------------
%%

-spec sendto(Socket, Data, Dest) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();
            (Socket, Data, Cont) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid().

sendto(Socket, Data, Dest_Cont) ->
    sendto(Socket, Data, Dest_Cont, ?ESOCK_SENDTO_FLAGS_DEFAULT).

-spec sendto(Socket, Data, Dest, Flags) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Handle :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Dest           :: sockaddr(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Handle :: select_handle() | completion_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Dest           :: sockaddr(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                     when
      Socket    :: socket(),
      Data      :: iodata(),
      Dest      :: sockaddr(),
      RestData  :: binary(),
      Reason    :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

            (Socket, Data, Cont, SelectHandle :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Cont           :: select_info(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      Reason         :: posix() | 'closed' | invalid();

            (Socket, Data, Cont, SelectHandle :: select_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Cont           :: select_info(),
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      Reason         :: posix() | 'closed' | invalid();

            (Socket, Data, Cont, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

            (Socket, Data, Cont, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Cont       :: select_info(),
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid().

sendto(Socket, Data, Dest, Flags) when is_list(Flags) ->
    sendto(Socket, Data, Dest, Flags, ?ESOCK_SENDTO_TIMEOUT_DEFAULT);
sendto(
  ?socket(SockRef) = Socket, Data,
  ?SELECT_INFO(SelectTag, _) = Cont, Timeout)
  when is_reference(SockRef) ->
    case SelectTag of
        {sendto, ContData} ->
            case Data of
                Bin when is_binary(Bin) ->
                    sendto_timeout_cont(SockRef, Bin, ContData, Timeout);
                [Bin] when is_binary(Bin) ->
                    sendto_timeout_cont(SockRef, Bin, ContData, Timeout);
                IOV when is_list(IOV) ->
                    try erlang:list_to_binary(IOV) of
                        Bin ->
                            sendto_timeout_cont(
                              SockRef, Bin, ContData, Timeout)
                    catch
                        error : badarg ->
                            erlang:error({invalid, {data, Data}})
                    end;
                _ ->
                    erlang:error(badarg, [Socket, Data, Cont, Timeout])
            end;
        _ ->
            {error, {invalid, Cont}}
    end;
sendto(Socket, Data, Dest, Timeout) ->
    sendto(Socket, Data, Dest, ?ESOCK_SENDTO_FLAGS_DEFAULT, Timeout).


-spec sendto(Socket, Data, Dest, Flags, Handle :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Dest           :: sockaddr(),
      Flags          :: [msg_flag() | integer()],
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Flags, Handle :: select_handle() | completion_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason}
                      when
      Socket         :: socket(),
      Data           :: iodata(),
      Dest           :: sockaddr(),
      Flags          :: [msg_flag() | integer()],
      RestData       :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Flags, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid();

            (Socket, Data, Dest, Flags, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: iodata(),
      Dest       :: sockaddr(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: binary(),
      Reason     :: posix() | 'closed' | invalid().

sendto(?socket(SockRef), Data, Dest, Flags, Timeout)
  when is_reference(SockRef), is_binary(Data), is_list(Flags) ->
    %%
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendto_nowait(SockRef, Data, Dest, Flags, SelectHandle);
        handle ->
            Handle = Timeout,
            sendto_nowait(SockRef, Data, Dest, Flags, Handle);
        Deadline ->
            HasWritten = false,
            sendto_deadline(SockRef, Data, Dest, Flags, Deadline, HasWritten)
    end;
sendto(?socket(SockRef) = Socket, [Bin], Dest, Flags, Timeout)
  when is_reference(SockRef), is_binary(Bin) ->
    sendto(Socket, Bin, Dest, Flags, Timeout);
sendto(?socket(SockRef) = Socket, Data, Dest, Flags, Timeout)
  when is_reference(SockRef), is_list(Data) ->
    try erlang:list_to_binary(Data) of
        Bin ->
            sendto(Socket, Bin, Dest, Flags, Timeout)
    catch
        error : badarg ->
            erlang:error({invalid, {data, Data}})
    end;
sendto(Socket, Data, Dest, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Data, Dest, Flags, Timeout]).

sendto_timeout_cont(SockRef, Bin, Cont, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendto_nowait_cont(SockRef, Bin, Cont, SelectHandle);
        handle ->
            Handle = Timeout,
            sendto_nowait_cont(SockRef, Bin, Cont, Handle);
        Deadline ->
            HasWritten = false,
            sendto_deadline_cont(SockRef, Bin, Cont, Deadline, HasWritten)
    end.

sendto_nowait(SockRef, Bin, To, Flags, Handle) ->
    send_common_nowait_result(
      Handle, sendto,
      prim_socket:sendto(SockRef, Bin, To, Flags, Handle)).

sendto_nowait_cont(SockRef, Bin, Cont, Handle) ->
    send_common_nowait_result(
      Handle, sendto,
      prim_socket:sendto(SockRef, Bin, Cont, Handle)).

sendto_deadline(SockRef, Bin, To, Flags, Deadline, HasWritten) ->
    Handle = make_ref(),
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       sendto, fun sendto_deadline_cont/5,
       prim_socket:sendto(SockRef, Bin, To, Flags, Handle)).

sendto_deadline_cont(SockRef, Bin, Cont, Deadline, HasWritten) ->
    Handle = make_ref(),
    send_common_deadline_result(
       SockRef, Bin, Handle, Deadline, HasWritten,
       sendto, fun sendto_deadline_cont/5,
       prim_socket:sendto(SockRef, Bin, Cont, Handle)).


%% ---------------------------------------------------------------------------
%%
%% The only part of the msg_send() that *must* exist (a connected
%% socket need not specify the addr field) is the iov.
%% The ctrl field is optional, and the addr and flags are not
%% used when sending.
%%

-spec sendmsg(Socket, Msg) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Msg        :: msg_send(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid().

sendmsg(Socket, Msg) ->
    sendmsg(Socket, Msg,
            ?ESOCK_SENDMSG_FLAGS_DEFAULT, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT).


-spec sendmsg(Socket, Msg, Flags) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Msg        :: msg_send(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Data, Cont) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: erlang:iovec(),
      Cont       :: select_info(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Msg, Timeout :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Msg            :: msg_send(),
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Msg, Handle :: select_handle() | completion_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Msg            :: msg_send(),
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Msg, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Msg        :: msg_send(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Msg, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Msg        :: msg_send(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid().

sendmsg(Socket, Data, Flags_Cont)
  when is_list(Flags_Cont);
       is_tuple(Flags_Cont) ->
    sendmsg(Socket, Data, Flags_Cont, ?ESOCK_SENDMSG_TIMEOUT_DEFAULT);
sendmsg(Socket, Msg, Timeout) ->
    sendmsg(Socket, Msg, ?ESOCK_SENDMSG_FLAGS_DEFAULT, Timeout).


-spec sendmsg(Socket, Msg, Flags, Timeout :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Msg            :: msg_send(),
      Flags          :: [msg_flag() | integer()],
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Msg, Flags, Handle :: select_handle() | completion_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Msg            :: msg_send(),
      Flags          :: [msg_flag() | integer()],
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Msg, Flags, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Msg        :: msg_send(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Msg, Flags, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Msg        :: msg_send(),
      Flags      :: [msg_flag() | integer()],
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, Timeout :: 'nowait') ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket         :: socket(),
      Data           :: msg_send() | erlang:iovec(),
      Cont           :: select_info(),
      RestData       :: erlang:iovec(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, SelectHandle :: select_handle()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, RestData}} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: msg_send() | erlang:iovec(),
      Cont       :: select_info(),
      RestData   :: erlang:iovec(),
      SelectInfo :: select_info(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, Timeout :: 'infinity') ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason} |
                  {'error', {Reason, RestData}}
                      when
      Socket     :: socket(),
      Data       :: msg_send() | erlang:iovec(),
      Cont       :: select_info(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid();

             (Socket, Data, Cont, Timeout :: non_neg_integer()) ->
                  'ok' |
                  {'ok', RestData} |
                  {'error', Reason | 'timeout'} |
                  {'error', {Reason | 'timeout', RestData}}
                      when
      Socket     :: socket(),
      Data       :: msg_send() | erlang:iovec(),
      Cont       :: select_info(),
      RestData   :: erlang:iovec(),
      Reason     :: posix() | 'closed' | invalid().

sendmsg(
  ?socket(SockRef) = Socket, RestData,
  ?SELECT_INFO(SelectTag, _) = Cont, Timeout) ->
    %%
    case SelectTag of
        {sendmsg, ContData} ->
            case RestData of
                #{iov := IOV} ->
                    sendmsg_timeout_cont(SockRef, IOV, ContData, Timeout);
                IOV when is_list(IOV) ->
                    sendmsg_timeout_cont(SockRef, IOV, ContData, Timeout);
                _ ->
                    erlang:error(badarg, [Socket, RestData, Cont, Timeout])
            end;
        _ ->
            {error, {invalid, Cont}}
    end;
sendmsg(?socket(SockRef), #{iov := IOV} = Msg, Flags, Timeout)
  when is_reference(SockRef), is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            sendmsg_nowait(SockRef, Msg, Flags, Handle, IOV);
        handle ->
            Handle = Timeout,
            sendmsg_nowait(SockRef, Msg, Flags, Handle, IOV);
        Deadline ->
            HasWritten = false,
            sendmsg_deadline(SockRef, Msg, Flags, Deadline, HasWritten, IOV)
    end;
sendmsg(Socket, Msg, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Msg, Flags, Timeout]).

sendmsg_timeout_cont(SockRef, RestData, Cont, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendmsg_nowait_cont(SockRef, RestData, Cont, SelectHandle);
        handle ->
            SelectHandle = Timeout,
            sendmsg_nowait_cont(SockRef, RestData, Cont, SelectHandle);
        Deadline ->
            HasWritten = false,
            sendmsg_deadline_cont(
              SockRef, RestData, Cont, Deadline, HasWritten)
    end.

sendmsg_nowait(SockRef, Msg, Flags, Handle, IOV) ->
    send_common_nowait_result(
      Handle, sendmsg,
      prim_socket:sendmsg(SockRef, Msg, Flags, Handle, IOV)).

sendmsg_nowait_cont(SockRef, RestData, Cont, SelectHandle) ->
    send_common_nowait_result(
      SelectHandle, sendmsg,
      prim_socket:sendmsg(SockRef, RestData, Cont, SelectHandle)).

sendmsg_deadline(SockRef, Msg, Flags, Deadline, HasWritten, IOV) ->
    Handle = make_ref(),
    send_common_deadline_result(
      SockRef, IOV, Handle, Deadline, HasWritten,
      sendmsg, fun sendmsg_deadline_cont/5,
      prim_socket:sendmsg(SockRef, Msg, Flags, Handle, IOV)).

sendmsg_deadline_cont(SockRef, Data, Cont, Deadline, HasWritten) ->
    SelectHandle = make_ref(),
    send_common_deadline_result(
      SockRef, Data, SelectHandle, Deadline, HasWritten,
      sendmsg, fun sendmsg_deadline_cont/5,
      prim_socket:sendmsg(SockRef, Data, Cont, SelectHandle)).


%% ===========================================================================
%%
%% sendfile - send a file on a socket
%%

sendfile(Socket, FileHandle) ->
    sendfile(Socket, FileHandle, 0, 0, infinity).

sendfile(Socket, FileHandle, Timeout) ->
    sendfile(Socket, FileHandle, 0, 0, Timeout).

sendfile(Socket, FileHandle_Cont, Offset, Count) ->
    sendfile(Socket, FileHandle_Cont, Offset, Count, infinity).


-spec sendfile(Socket, Cont, Offset, Count,
               SelectHandle :: 'nowait') ->
                      {'ok', BytesSent} |
                      {'select', SelectInfo} |
                      {'select', {SelectInfo, BytesSent}} |
                      {'error', Reason}
                          when
      Socket     :: socket(),
      Cont       :: select_info(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      SelectInfo :: select_info(),
      Reason     :: posix() | 'closed' | invalid();

              (Socket, Cont, Offset, Count,
               SelectHandle :: select_handle()) ->
                      {'ok', BytesSent} |
                      {'select', SelectInfo} |
                      {'select', {SelectInfo, BytesSent}} |
                      {'error', Reason}
                          when
      Socket     :: socket(),
      Cont       :: select_info(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      SelectInfo :: select_info(),
      Reason     :: posix() | 'closed' | invalid();

              (Socket, Cont, Offset, Count,
               Timeout :: 'infinity') ->
                      {'ok', BytesSent} |
                      {'error', Reason} |
                      {'error', {Reason, BytesSent}}
                          when
      Socket     :: socket(),
      Cont       :: select_info(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      Reason     :: posix() | 'closed' | invalid();

              (Socket, Cont, Offset, Count,
               Timeout :: non_neg_integer()) ->
                      {'ok', BytesSent} |
                      {'error', Reason | 'timeout'} |
                      {'error', {Reason | 'timeout', BytesSent}}
                          when
      Socket     :: socket(),
      Cont       :: select_info(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      Reason     :: posix() | 'closed' | invalid();


              (Socket, FileHandle, Offset, Count,
               SelectHandle :: 'nowait') ->
                      {'ok', BytesSent} |
                      {'select', SelectInfo} |
                      {'select', {SelectInfo, BytesSent}} |
                      {'error', Reason}
                          when
      Socket     :: socket(),
      FileHandle :: file:fd(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      SelectInfo :: select_info(),
      Reason     :: posix() | 'closed' | invalid();

              (Socket, FileHandle, Offset, Count,
               SelectHandle :: select_handle()) ->
                      {'ok', BytesSent} |
                      {'select', SelectInfo} |
                      {'select', {SelectInfo, BytesSent}} |
                      {'error', Reason}
                          when
      Socket     :: socket(),
      FileHandle :: file:fd(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      SelectInfo :: select_info(),
      Reason     :: posix() | 'closed' | invalid();

              (Socket, FileHandle, Offset, Count,
               Timeout :: 'infinity') ->
                      {'ok', BytesSent} |
                      {'error', Reason} |
                      {'error', {Reason, BytesSent}}
                          when
      Socket     :: socket(),
      FileHandle :: file:fd(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      Reason     :: posix() | 'closed' | invalid();

              (Socket, FileHandle, Offset, Count,
               Timeout :: non_neg_integer()) ->
                      {'ok', BytesSent} |
                      {'error', Reason | 'timeout'} |
                      {'error', {Reason | 'timeout', BytesSent}}
                          when
      Socket     :: socket(),
      FileHandle :: file:fd(),
      Offset     :: integer(),
      Count      :: non_neg_integer(),
      BytesSent  :: non_neg_integer(),
      Reason     :: posix() | 'closed' | invalid().

sendfile(
  ?socket(SockRef) = Socket, FileHandle_Cont, Offset, Count, Timeout)
  when is_integer(Offset), is_integer(Count), 0 =< Count ->
    %%
    case FileHandle_Cont of
        #file_descriptor{module = Module} = FileHandle ->
            GetFRef = internal_get_nif_resource,
            try Module:GetFRef(FileHandle) of
                FRef ->
                    State = {FRef, Offset, Count},
                    sendfile_int(SockRef, State, Timeout)
            catch
                %% We could just crash here, since the caller
                %% maybe broke the API and did not provide
                %% a raw file as FileHandle, i.e GetFRef
                %% is not implemented in Module;
                %% but instead handle that nicely
                Class : Reason : Stacktrace
                  when Class =:= error, Reason =:= undef ->
                    case Stacktrace of
                        [{Module, GetFRef, Args, _} | _]
                          when Args =:= 1;        % Arity 1
                               tl(Args) =:= [] -> % Arity 1
                            erlang:error(
                              badarg,
                              [Socket, FileHandle_Cont,
                               Offset, Count, Timeout]);
                        _ -> % Re-raise
                            erlang:raise(Class, Reason, Stacktrace)
                    end
            end;
        ?SELECT_INFO(SelectTag, _) = Cont ->
            case SelectTag of
                {sendfile, FRef} ->
                    State = {FRef, Offset, Count},
                    sendfile_int(SockRef, State, Timeout);
                sendfile ->
                    State = {Offset, Count},
                    sendfile_int(SockRef, State, Timeout);
                _ ->
                    {error, {invalid, Cont}}
            end;
        _ ->
            erlang:error(
              badarg, [Socket, FileHandle_Cont, Offset, Count, Timeout])
    end;
sendfile(Socket, FileHandle_Cont, Offset, Count, Timeout) ->
    erlang:error(
      badarg, [Socket, FileHandle_Cont, Offset, Count, Timeout]).

sendfile_int(SockRef, State, Timeout) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            SelectHandle = make_ref(),
            sendfile_nowait(SockRef, State, SelectHandle);
        handle ->
            SelectHandle = Timeout,
            sendfile_nowait(SockRef, State, SelectHandle);
        Deadline ->
            BytesSent = 0,
            sendfile_deadline(SockRef, State, BytesSent, Deadline)
    end.


-compile({inline, [prim_socket_sendfile/3]}).
prim_socket_sendfile(SockRef, {FRef, Offset, Count}, SelectHandle) ->
    %% Start call
    prim_socket:sendfile(SockRef, FRef, Offset, Count, SelectHandle);
prim_socket_sendfile(SockRef, {Offset, Count}, SelectHandle) ->
    %% Continuation call
    prim_socket:sendfile(SockRef, Offset, Count, SelectHandle).

sendfile_nowait(SockRef, State, SelectHandle) ->
    case prim_socket_sendfile(SockRef, State, SelectHandle) of
        select ->
            %% Can only happen when we are enqueued after
            %% a send in progress so BytesSent is 0;
            %% wait for continuation and later repeat start call
            {FRef, _Offset, _Count} = State,
            {select, ?SELECT_INFO({sendfile, FRef}, SelectHandle)};
        {select, BytesSent} ->
            {select, {?SELECT_INFO(sendfile, SelectHandle), BytesSent}};
        %%
        Result ->
            Result
    end.

sendfile_deadline(SockRef, State, BytesSent_0, Deadline) ->
    SelectHandle = make_ref(),
    case prim_socket_sendfile(SockRef, State, SelectHandle) of
        select ->
            %% Can only happen when we are enqueued after
            %% a send in progress so BytesSent is 0;
            %% wait for continuation and repeat start call
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(_Socket, select, SelectHandle) ->
                    sendfile_deadline(
                      SockRef, State, BytesSent_0, Deadline);
                ?socket_msg(_Socket, abort, {SelectHandle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, sendfile, SelectHandle),
                    {error, timeout}
            end;
        {select, BytesSent} ->
            %% Partial send success; wait for continuation
            Timeout = timeout(Deadline),
            BytesSent_1 = BytesSent_0 + BytesSent,
            receive
                ?socket_msg(_Socket, select, SelectHandle) ->
                    sendfile_deadline(
                      SockRef,
                      sendfile_next(BytesSent, State),
                      BytesSent_1, Deadline);
                ?socket_msg(_Socket, abort, {SelectHandle, Reason}) ->
                    {error, {Reason, BytesSent_1}}
            after Timeout ->
                    _ = cancel(SockRef, sendfile, SelectHandle),
                    {error, {timeout, BytesSent_1}}
            end;
        {error, _} = Result when tuple_size(State) =:= 3 ->
            Result;
        {error, Reason} when tuple_size(State) =:= 2 ->
            {error, {Reason, BytesSent_0}};
        {ok, BytesSent} ->
            {ok, BytesSent_0 + BytesSent}
    end.

sendfile_next(BytesSent, {_FRef, Offset, Count}) ->
    sendfile_next(BytesSent, Offset, Count);
sendfile_next(BytesSent, {Offset, Count}) ->
    sendfile_next(BytesSent, Offset, Count).
%%
sendfile_next(BytesSent, Offset, Count) ->
    {Offset + BytesSent,
     if
         Count =:= 0 ->
             0;
         BytesSent < Count ->
             Count - BytesSent
     end}.

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
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid().
                          
recv(Socket) ->
    recv(Socket, 0, ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recv(Socket, Flags) ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Flags  :: [msg_flag() | integer()],
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid();

          (Socket, Length) ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid().

recv(Socket, Flags) when is_list(Flags) ->
    recv(Socket, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length) when is_integer(Length) andalso (Length >= 0) ->
    recv(Socket, Length,
         ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recv(Socket, Flags, Handle :: 'nowait') ->
                  {'ok', Data} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, Data}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket         :: socket(),
      Flags          :: [msg_flag() | integer()],
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Flags, Handle :: select_handle() | completion_handle()) ->
                  {'ok', Data} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, Data}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket         :: socket(),
      Flags          :: [msg_flag() | integer()],
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Flags, Timeout :: 'infinity') ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Flags  :: [msg_flag() | integer()],
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid();

          (Socket, Flags, Timeout :: non_neg_integer()) ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Flags  :: [msg_flag() | integer()],
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid() | 'timeout';

          (Socket, Length, Flags) ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Flags  :: [msg_flag() | integer()],
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid();

          (Socket, Length, Handle :: 'nowait') ->
                  {'ok', Data} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, Data}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket         :: socket(),
      Length         :: non_neg_integer(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Length, Handle :: select_handle() | completion_handle()) ->
                  {'ok', Data} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, Data}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket         :: socket(),
      Length         :: non_neg_integer(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Length, Timeout :: 'infinity') ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid();

          (Socket, Length, Timeout :: non_neg_integer()) ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid() | 'timeout'.

recv(Socket, Flags, Timeout) when is_list(Flags) ->
    recv(Socket, 0, Flags, Timeout);
recv(Socket, Length, Flags) when is_list(Flags) ->
    recv(Socket, Length, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length, Timeout) ->
    recv(Socket, Length, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).

-spec recv(Socket, Length, Flags, Handle :: 'nowait') ->
                  {'ok', Data} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, Data}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket         :: socket(),
      Length         :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Length, Flags, Handle :: select_handle() | completion_handle()) ->
                  {'ok', Data} |
                  {'select', SelectInfo} |
                  {'select', {SelectInfo, Data}} |
                  {'completion', CompletionInfo} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket         :: socket(),
      Length         :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

          (Socket, Length, Flags, Timeout :: 'infinity') ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket  :: socket(),
      Length  :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid();

          (Socket, Length, Flags, Timeout :: non_neg_integer()) ->
                  {'ok', Data} |
                  {'error', Reason} |
                  {'error', {Reason, Data}} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Flags  :: [msg_flag() | integer()],
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid() | 'timeout'.

recv(?socket(SockRef), Length, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(Length), Length >= 0,
       is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            recv_nowait(SockRef, Length, Flags, Handle, <<>>);
        handle ->
            Handle = Timeout,
            recv_nowait(SockRef, Length, Flags, Handle, <<>>);
        zero ->
            case prim_socket:recv(SockRef, Length, Flags, zero) of
                ok ->
                    {error, timeout};
                Result ->
                    Result
            end;
        Deadline ->
            recv_deadline(SockRef, Length, Flags, Deadline, <<>>)
    end;
recv(Socket, Length, Flags, Timeout) ->
    erlang:error(badarg, [Socket, Length, Flags, Timeout]).

%% We will only recurse with Length == 0 if Length is 0,
%% so Length == 0 means to return all available data also when recursing

recv_nowait(SockRef, Length, Flags, Handle, Acc) ->
    case prim_socket:recv(SockRef, Length, Flags, Handle) of
        {more, Bin} ->
            %% We got what we requested but will not waste more time
            %% although there might be more data available
            {ok, bincat(Acc, Bin)};
        {select, Bin} ->
            %% We got less than requested so the caller will
            %% get a select message when there might be more to read
            {select, {?SELECT_INFO(recv, Handle), bincat(Acc, Bin)}};
        select ->
            %% The caller will get a select message when there
            %% might be data to read
            if
                byte_size(Acc) =:= 0 ->
                    {select, ?SELECT_INFO(recv, Handle)};
                true ->
                    {select, {?SELECT_INFO(recv, Handle), Acc}}
            end;
        completion ->
            %% The caller will get a completion message (with the
            %% result) when the data arrives. *No* further action
            %% is required.
            {completion, ?COMPLETION_INFO(recv, Handle)};
        Result ->
            recv_result(Acc, Result)
    end.

recv_deadline(SockRef, Length, Flags, Deadline, Acc) ->
    Handle = make_ref(),
    case prim_socket:recv(SockRef, Length, Flags, Handle) of
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
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    if
                        0 < Timeout ->
                            %% Recv more
                            recv_deadline(
                              SockRef, Length - byte_size(Bin), Flags,
                              Deadline, bincat(Acc, Bin));
                        true ->
                            {error, {timeout, bincat(Acc, Bin)}}
                    end;
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, {Reason, bincat(Acc, Bin)}}
            after Timeout ->
                    _ = cancel(SockRef, recv, Handle),
                    {error, {timeout, bincat(Acc, Bin)}}
            end;
        %%
        select when Length =:= 0, 0 < byte_size(Acc) ->
            %% We first got some data and are then asked to wait,
            %% but we only want the first that comes
            %% - cancel and return what we have
            _ = cancel(SockRef, recv, Handle),
            {ok, Acc};
        %%
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    if
                        0 < Timeout ->
                            %% Retry
                            recv_deadline(
                              SockRef, Length, Flags, Deadline, Acc);
                        true ->
                            recv_error(Acc, timeout)
                    end;
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    recv_error(Acc, Reason)
            after Timeout ->
                    _ = cancel(SockRef, recv, Handle),
                    recv_error(Acc, timeout)
            end;

        %%
        completion ->
            %% There is nothing just now, but we will be notified when the
            %% data has been read (with a completion message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {ok, _Bin} = OK})
                  when (Length =:= 0) ->
                    recv_result(Acc, OK);
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {ok, Bin} = OK})
                  when (Length =:= byte_size(Bin)) ->
                    recv_result(Acc, OK);
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {ok, Bin}}) ->
                    if
                        0 < Timeout ->
                            %% Recv more
                            recv_deadline(
                              SockRef, Length - byte_size(Bin), Flags,
                              Deadline, bincat(Acc, Bin));
                        true ->
                            {error, {timeout, bincat(Acc, Bin)}}
                    end;
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, {error, Reason}}) ->
                    recv_error(Acc, Reason);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    recv_error(Acc, Reason)
            after Timeout ->
                    _ = cancel(SockRef, recv, Handle),
                    recv_error(Acc, timeout)
            end;

        %% We got some data, but not all
        {ok, Bin} when (Length > byte_size(Bin)) ->
            Timeout = timeout(Deadline),
            if
                0 < Timeout ->
                    %% Recv more
                    recv_deadline(
                      SockRef, Length - byte_size(Bin), Flags,
                      Deadline, bincat(Acc, Bin));
                true ->
                    {error, {timeout, bincat(Acc, Bin)}}
            end;
            
        %%
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

-spec recvfrom(Socket) ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket    :: socket(),
      Source    :: sockaddr_recv(),
      Data      :: binary(),
      Reason    :: posix() | 'closed' | invalid().

recvfrom(Socket) ->
    recvfrom(Socket, 0).

-spec recvfrom(Socket, Flags) ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket    :: socket(),
      Flags     :: [msg_flag() | integer()],
      Source    :: sockaddr_recv(),
      Data      :: binary(),
      Reason    :: posix() | 'closed' | invalid();

              (Socket, BufSz) ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Source    :: sockaddr_recv(),
      Data      :: binary(),
      Reason    :: posix() | 'closed' | invalid().

recvfrom(Socket, Flags) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz) ->
    recvfrom(Socket, BufSz,
             ?ESOCK_RECV_FLAGS_DEFAULT,
             ?ESOCK_RECV_TIMEOUT_DEFAULT).

-spec recvfrom(Socket, Flags, Handle :: 'nowait') ->
                      {'ok', {Source, Data}} |
                      {'select', SelectInfo} |
                      {'completion', CompletionInfo} |
                      {'error', Reason} when
      Socket         :: socket(),
      Flags          :: [msg_flag() | integer()],
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

              (Socket, Flags, Handle :: select_handle() | completion_handle()) ->
                      {'ok', {Source, Data}} |
                      {'select', SelectInfo} |
                      {'completion', CompletionInfo} |
                      {'error', Reason} when
      Socket         :: socket(),
      Flags          :: [msg_flag() | integer()],
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

              (Socket, Flags, Timeout :: 'infinity') ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket  :: socket(),
      Flags   :: [msg_flag() | integer()],
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid();

              (Socket, Flags, Timeout :: non_neg_integer()) ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket  :: socket(),
      Flags   :: [msg_flag() | integer()],
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout';

              (Socket, BufSz, Flags) -> 
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket :: socket(),
      BufSz  :: non_neg_integer(),
      Flags  :: [msg_flag() | integer()],
      Source :: sockaddr_recv(),
      Data   :: binary(),
      Reason :: posix() | 'closed' | invalid();

              (Socket, BufSz, Handle :: 'nowait') ->
                      {'ok', {Source, Data}} |
                      {'select', SelectInfo} |
                      {'completion', CompletionInfo} |
                      {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

              (Socket, BufSz, Handle :: select_handle() | completion_handle()) ->
                      {'ok', {Source, Data}} |
                      {'select', SelectInfo} |
                      {'completion', CompletionInfo} |
                      {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

              (Socket, BufSz, Timeout :: 'infinity') ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid();

              (Socket, BufSz, Timeout :: non_neg_integer()) ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout'.

recvfrom(Socket, Flags, Timeout) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, Timeout);
recvfrom(Socket, BufSz, Flags) when is_list(Flags) ->
    recvfrom(Socket, BufSz, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz, Timeout) ->
    recvfrom(Socket, BufSz, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).

-spec recvfrom(Socket, BufSz, Flags, Handle :: 'nowait') ->
                      {'ok', {Source, Data}} |
                      {'select', SelectInfo} |
                      {'completion', CompletionInfo} |
                      {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

              (Socket, BufSz, Flags, Handle :: select_handle() | completion_handle()) ->
                      {'ok', {Source, Data}} |
                      {'select', SelectInfo} |
                      {'completion', CompletionInfo} |
                      {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Source         :: sockaddr_recv(),
      Data           :: binary(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

              (Socket, BufSz, Flags, Timeout :: 'infinity') ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid();

              (Socket, BufSz, Flags, Timeout :: non_neg_integer()) ->
                      {'ok', {Source, Data}} |
                      {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Source  :: sockaddr_recv(),
      Data    :: binary(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout'.

recvfrom(?socket(SockRef), BufSz, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(BufSz), 0 =< BufSz,
       is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            recvfrom_nowait(SockRef, BufSz, Handle, Flags);
        handle ->
            Handle = Timeout,
            recvfrom_nowait(SockRef, BufSz, Handle, Flags);
        zero ->
            case prim_socket:recvfrom(SockRef, BufSz, Flags, zero) of
                ok ->
                    {error, timeout};
                Result ->
                    recvfrom_result(Result)
            end;
        Deadline ->
            recvfrom_deadline(SockRef, BufSz, Flags, Deadline)
    end;
recvfrom(Socket, BufSz, Flags, Timeout) ->
    erlang:error(badarg, [Socket, BufSz, Flags, Timeout]).

recvfrom_nowait(SockRef, BufSz, Handle, Flags) ->
    case prim_socket:recvfrom(SockRef, BufSz, Flags, Handle) of
        select = Tag ->
            {Tag, ?SELECT_INFO(recvfrom, Handle)};
        completion = Tag ->
            {Tag, ?COMPLETION_INFO(recvfrom, Handle)};
        Result ->
            recvfrom_result(Result)
    end.

recvfrom_deadline(SockRef, BufSz, Flags, Deadline) ->
    Handle = make_ref(),
    case prim_socket:recvfrom(SockRef, BufSz, Flags, Handle) of
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    recvfrom_deadline(SockRef, BufSz, Flags, Deadline);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvfrom, Handle),
                    {error, timeout}
            end;

        completion ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a completion message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, CompletionStatus}) ->
                    recvfrom_result(CompletionStatus);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvfrom, Handle),
                    {error, timeout}
            end;

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

-spec recvmsg(Socket) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket :: socket(),
      Msg    :: msg_recv(),
      Reason :: posix() | 'closed' | invalid().

recvmsg(Socket) ->
    recvmsg(Socket, 0, 0,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).


-spec recvmsg(Socket, Flags) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket :: socket(),
      Flags  :: [msg_flag() | integer()],
      Msg    :: msg_recv(),
      Reason :: posix() | 'closed' | invalid();

             (Socket, Timeout :: 'nowait') ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Handle :: select_handle() | completion_handle()) ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Timeout :: 'infinity') ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid();

             (Socket, Timeout :: non_neg_integer()) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout'.

recvmsg(Socket, Flags) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, ?ESOCK_RECV_TIMEOUT_DEFAULT);
recvmsg(Socket, Timeout) ->
    recvmsg(Socket, 0, 0, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).


-spec recvmsg(Socket, BufSz, CtrlSz, Timeout :: 'nowait') ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      CtrlSz         :: non_neg_integer(),
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Handle :: select_handle() | completion_handle()) ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      CtrlSz         :: non_neg_integer(),
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Timeout :: 'infinity') ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      CtrlSz  :: non_neg_integer(),
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Timeout :: non_neg_integer()) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      CtrlSz  :: non_neg_integer(),
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout'.

recvmsg(Socket, BufSz, CtrlSz, Timeout) ->
    recvmsg(Socket, BufSz, CtrlSz, ?ESOCK_RECV_FLAGS_DEFAULT, Timeout).


-spec recvmsg(Socket, Flags, Timeout :: 'nowait') ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      Flags          :: [msg_flag() | integer()],
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Flags, Handle :: select_handle() | completion_handle()) ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      Flags          :: [msg_flag() | integer()],
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, Flags, Timeout :: 'infinity') ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      Flags   :: [msg_flag() | integer()],
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid();

             (Socket, Flags, Timeout :: non_neg_integer()) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      Flags   :: [msg_flag() | integer()],
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout';

             (Socket, BufSz, CtrlSz) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket :: socket(),
      BufSz  :: non_neg_integer(),
      CtrlSz :: non_neg_integer(),
      Msg    :: msg_recv(),
      Reason :: posix() | 'closed' | invalid().

recvmsg(Socket, Flags, Timeout) when is_list(Flags) ->
    recvmsg(Socket, 0, 0, Flags, Timeout);
recvmsg(Socket, BufSz, CtrlSz) when is_integer(BufSz), is_integer(CtrlSz) ->
    recvmsg(Socket, BufSz, CtrlSz,
            ?ESOCK_RECV_FLAGS_DEFAULT, ?ESOCK_RECV_TIMEOUT_DEFAULT).


-spec recvmsg(Socket, BufSz, CtrlSz, Flags, Timeout :: 'nowait') ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket        :: socket(),
      BufSz          :: non_neg_integer(),
      CtrlSz         :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Flags, Handle :: select_handle() | completion_handle()) ->
                     {'ok', Msg} |
                     {'select', SelectInfo} |
                     {'completion', CompletionInfo} |
                     {'error', Reason} when
      Socket         :: socket(),
      BufSz          :: non_neg_integer(),
      CtrlSz         :: non_neg_integer(),
      Flags          :: [msg_flag() | integer()],
      Msg            :: msg_recv(),
      SelectInfo     :: select_info(),
      CompletionInfo :: completion_info(),
      Reason         :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Flags, Timeout :: 'infinity') ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      CtrlSz  :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid();

             (Socket, BufSz, CtrlSz, Flags, Timeout :: non_neg_integer()) ->
                     {'ok', Msg} |
                     {'error', Reason} when
      Socket  :: socket(),
      BufSz   :: non_neg_integer(),
      CtrlSz  :: non_neg_integer(),
      Flags   :: [msg_flag() | integer()],
      Msg     :: msg_recv(),
      Reason  :: posix() | 'closed' | invalid() | 'timeout'.

recvmsg(?socket(SockRef), BufSz, CtrlSz, Flags, Timeout)
  when is_reference(SockRef),
       is_integer(BufSz), 0 =< BufSz,
       is_integer(CtrlSz), 0 =< CtrlSz,
       is_list(Flags) ->
    case deadline(Timeout) of
        invalid ->
            erlang:error({invalid, {timeout, Timeout}});
        nowait ->
            Handle = make_ref(),
            recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags, Handle);
        handle ->
            Handle = Timeout,
            recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags, Handle);
        zero ->
            case prim_socket:recvmsg(SockRef, BufSz, CtrlSz, Flags, zero) of
                ok ->
                    {error, timeout};
                Result ->
                    recvmsg_result(Result)
            end;
        Deadline ->
            recvmsg_deadline(SockRef, BufSz, CtrlSz, Flags, Deadline)
    end;
recvmsg(Socket, BufSz, CtrlSz, Flags, Timeout) ->
    erlang:error(badarg, [Socket, BufSz, CtrlSz, Flags, Timeout]).

recvmsg_nowait(SockRef, BufSz, CtrlSz, Flags, Handle)  ->
    case prim_socket:recvmsg(SockRef, BufSz, CtrlSz, Flags, Handle) of
        select ->
            {select, ?SELECT_INFO(recvmsg, Handle)};
        completion ->
            {completion, ?COMPLETION_INFO(recvmsg, Handle)};
        Result ->
            recvmsg_result(Result)
    end.

recvmsg_deadline(SockRef, BufSz, CtrlSz, Flags, Deadline)  ->
    Handle = make_ref(),
    case prim_socket:recvmsg(SockRef, BufSz, CtrlSz, Flags, Handle) of
        select ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), select, Handle) ->
                    recvmsg_deadline(
                      SockRef, BufSz, CtrlSz, Flags, Deadline);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvmsg, Handle),
                    {error, timeout}
            end;

        completion ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a completion message).
            Timeout = timeout(Deadline),
            receive
                ?socket_msg(?socket(SockRef), completion,
                            {Handle, CompletionStatus}) ->
                    recvmsg_result(CompletionStatus);
                ?socket_msg(_Socket, abort, {Handle, Reason}) ->
                    {error, Reason}
            after Timeout ->
                    _ = cancel(SockRef, recvmsg, Handle),
                    {error, timeout}
            end;

        Result ->
            recvmsg_result(Result)
    end.

recvmsg_result(Result) ->
    %% ?DBG([{result, Result}]),
    case Result of
        {ok, _Msg} = OK ->
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

-spec close(Socket) -> 'ok' | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed' | 'timeout'.

close(?socket(SockRef))
  when is_reference(SockRef) ->
    case prim_socket:close(SockRef) of
        ok ->
            prim_socket:finalize_close(SockRef);
        {ok, CloseRef} ->
            %% We must wait for the socket_stop callback function to 
            %% complete its work
            receive
                ?socket_msg(?socket(SockRef), close, CloseRef) ->
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

-spec shutdown(Socket, How) -> 'ok' | {'error', Reason} when
      Socket :: socket(),
      How    :: 'read' | 'write' | 'read_write',
      Reason :: posix() | 'closed'.

shutdown(?socket(SockRef), How)
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

-spec setopt(socket(),
             SocketOption ::
               {Level :: 'otp', Opt :: otp_socket_option()},
             _) ->
                    'ok' | {'error', invalid() | 'closed'};
            (socket(),
             SocketOption :: socket_option(),
             _) ->
                    'ok' | {'error', posix() | invalid() | 'closed'}.

setopt(?socket(SockRef), SocketOption, Value)
  when is_reference(SockRef) ->
    prim_socket:setopt(SockRef, SocketOption, Value);
setopt(Socket, SocketOption, Value) ->
    erlang:error(badarg, [Socket, SocketOption, Value]).


%% Backwards compatibility
setopt(Socket, Level, Opt, Value)
  when is_integer(Opt), is_binary(Value) ->
    setopt_native(Socket, {Level,Opt}, Value);
setopt(Socket, Level, Opt, Value) ->
    setopt(Socket, {Level,Opt}, Value).


-spec setopt_native(socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level()
                              | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    Value :: native_value()) ->
                           'ok' | {'error', posix() | invalid() | 'closed'}.

setopt_native(?socket(SockRef), SocketOption, Value)
  when is_reference(SockRef) ->
    prim_socket:setopt_native(SockRef, SocketOption, Value);
setopt_native(Socket, SocketOption, Value) ->
    erlang:error(badarg, [Socket, SocketOption, Value]).


%% ===========================================================================
%%
%% getopt - retrieve individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol).
%% If its an "invalid" option, we should not crash but return some
%% useful error...
%%
%% When specifying level as an integer, and therefore using "native mode",
%% we should make it possible to specify common types instead of the
%% value size. Example: int | bool | {string, pos_integer()} | non_neg_integer()
%%

-spec getopt(socket(),
             SocketOption ::
               {Level :: 'otp',
                Opt :: otp_socket_option()}) ->
                    {'ok', Value :: term()} |
                    {'error', invalid() | 'closed'};
            (socket(),
             SocketOption :: socket_option()) ->
                    {'ok', Value :: term()} |
                    {'error', posix() | invalid() | 'closed'}.

getopt(?socket(SockRef), SocketOption)
  when is_reference(SockRef) ->
    prim_socket:getopt(SockRef, SocketOption).

%% Backwards compatibility
getopt(Socket, Level, {NativeOpt, ValueSpec})
  when is_integer(NativeOpt) ->
    getopt_native(Socket, {Level,NativeOpt}, ValueSpec);
getopt(Socket, Level, Opt) ->
    getopt(Socket, {Level,Opt}).

-spec getopt_native(socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level()
                              | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueType :: 'integer') ->
                           {'ok', Value :: integer()} |
                           {'error', posix() | invalid() | 'closed'};
                   (socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level()
                              | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueType :: 'boolean') ->
                           {'ok', Value :: boolean()} |
                           {'error', posix() | invalid() | 'closed'};
                   (socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level()
                              | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueSize :: non_neg_integer()) ->
                           {'ok', Value :: binary()} |
                           {'error', posix() | invalid() | 'closed'};
                   (socket(),
                    SocketOption ::
                      socket_option() |
                      {Level :: level()
                              | (NativeLevel :: integer()),
                       NativeOpt :: integer()},
                    ValueSpec :: binary()) ->
                           {'ok', Value :: binary()} |
                           {'error', posix() | invalid() | 'closed'}.
%% Compare ValueType, ValueSpec and ValueSize to native_value()
%% which are the types valid to setopt_native

getopt_native(?socket(SockRef), SocketOption, ValueSpec) ->
    prim_socket:getopt_native(SockRef, SocketOption, ValueSpec).


%% ===========================================================================
%%
%% sockname - return the current address of the socket.
%%
%%

-spec sockname(Socket) -> {'ok', SockAddr} | {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr_recv(),
      Reason   :: posix() | 'closed'.

sockname(?socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:sockname(SockRef);
sockname(Socket) ->
    erlang:error(badarg, [Socket]).


%% ===========================================================================
%%
%% peername - return the address of the peer *connected* to the socket.
%%
%%

-spec peername(Socket) -> {'ok', SockAddr} | {'error', Reason} when
      Socket   :: socket(),
      SockAddr :: sockaddr_recv(),
      Reason   :: posix() | 'closed'.

peername(?socket(SockRef))
  when is_reference(SockRef) ->
    prim_socket:peername(SockRef);
peername(Socket) ->
    erlang:error(badarg, [Socket]).



%% ===========================================================================
%%
%% ioctl - control device - get requests
%%
%%

-spec ioctl(Socket, GetRequest :: 'gifconf') ->
          {'ok', IFConf :: [#{name := string, addr := sockaddr()}]} |
          {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed';

           (Socket, GetRequest :: 'nread' | 'nwrite' | 'nspace') ->
          {'ok', NumBytes :: non_neg_integer()} | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed';

           (Socket, GetRequest :: 'atmark') ->
          {'ok', Available :: boolean()} | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed';

           (Socket, GetRequest :: 'tcp_info') ->
          {'ok', Info :: map()} | {'error', Reason} when
      Socket :: socket(),
      Reason :: posix() | 'closed'.

%% gifconf | nread | nwrite | nspace | atmark |
%% {gifaddr, string()} | {gifindex, string()} | {gifname, integer()}
ioctl(?socket(SockRef), gifconf = GetRequest) ->
    prim_socket:ioctl(SockRef, GetRequest);
ioctl(?socket(SockRef), GetRequest) when (nread =:= GetRequest) orelse
                                         (nwrite =:= GetRequest) orelse
                                         (nspace =:= GetRequest) ->
    prim_socket:ioctl(SockRef, GetRequest);
ioctl(?socket(SockRef), GetRequest) when (atmark =:= GetRequest) ->
    prim_socket:ioctl(SockRef, GetRequest);
ioctl(Socket, GetRequest) when (tcp_info =:= GetRequest) ->
    ioctl(Socket, GetRequest, 0);
ioctl(Socket, GetRequest) ->
    erlang:error(badarg, [Socket, GetRequest]).

%% -spec ioctl(Socket, GetRequest, Index) -> {'ok', Name} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifname',
%%       Index      :: integer(),
%%       Name       :: string(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Index} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifindex',
%%       Name       :: string(),
%%       Index      :: integer(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Addr} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifaddr',
%%       Name       :: string(),
%%       Addr       :: sockaddr(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', DestAddr} | {'error', Reason} when
%%       Socket      :: socket(),
%%       GetRequest  :: 'gifdstaddr',
%%       Name        :: string(),
%%       DestAddr    :: sockaddr(),
%%       Reason      :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', BroadcastAddr} | {'error', Reason} when
%%       Socket        :: socket(),
%%       GetRequest    :: 'gifbrdaddr',
%%       Name          :: string(),
%%       BroadcastAddr :: sockaddr(),
%%       Reason        :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Netmask} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifnetmask',
%%       Name       :: string(),
%%       Netmask    :: sockaddr(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', HWAddr} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifhwaddr',
%%       Name       :: string(),
%%       HWAddr     :: sockaddr(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', MTU} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifmtu',
%%       Name       :: string(),
%%       MTU        :: integer(),
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', TransmitQLen} | {'error', Reason} when
%%       Socket       :: socket(),
%%       GetRequest   :: 'giftxqlen',
%%       Name         :: string(),
%%       TransmitQLen :: integer(),
%%       Reason       :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', Flags} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifflags',
%%       Name       :: string(),
%%       Flags      :: [ioctl_device_flag() | integer()],
%%       Reason     :: posix() | 'closed';
%%            (Socket, GetRequest, Name) -> {'ok', DevMap} | {'error', Reason} when
%%       Socket     :: socket(),
%%       GetRequest :: 'gifmap',
%%       Name       :: string(),
%%       DevMap     :: ioctl_device_map(),
%%       Reason     :: posix() | 'closed'.

-spec ioctl(Socket, GetRequest, NameOrIndex) -> {'ok', Result} | {'error', Reason} when
      Socket      :: socket(),
      GetRequest  :: 'gifname' | 'gifindex' |
                     'gifaddr' | 'gifdstaddr' | 'gifbrdaddr' |
                     'gifnetmask' | 'gifhwaddr' |
                     'gifmtu' | 'giftxqlen' | 'gifflags' |
		     'tcp_info',
      NameOrIndex :: string() | integer(),
      Result      :: term(),
      Reason      :: posix() | 'closed';
	   (Socket, SetRequest, Value) -> ok | {'error', Reason} when
      Socket     :: socket(),
      SetRequest :: 'rcvall',
      Value      :: off | on | iplevel,
      Reason     :: posix() | 'closed';
	   (Socket, SetRequest, Value) -> ok | {'error', Reason} when
      Socket     :: socket(),
      SetRequest :: 'rcvall_igmpmcast' | 'rcvall_mcast',
      Value      :: off | on,
      Reason     :: posix() | 'closed'.

ioctl(?socket(SockRef), gifname = GetRequest, Index)
  when is_integer(Index) ->
    prim_socket:ioctl(SockRef, GetRequest, Index);
ioctl(?socket(SockRef), gifindex = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifdstaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifbrdaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifnetmask = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifmtu = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifhwaddr = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), giftxqlen = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifflags = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);
ioctl(?socket(SockRef), gifmap = GetRequest, Name)
  when is_list(Name) ->
    prim_socket:ioctl(SockRef, GetRequest, Name);

ioctl(?socket(SockRef), tcp_info = GetRequest, Version)
  when (Version =:= 0) ->
    prim_socket:ioctl(SockRef, GetRequest, Version);

ioctl(?socket(SockRef), rcvall = SetRequest, Value)
  when (Value =:= off) orelse
       (Value =:= on)  orelse
       (Value =:= iplevel) ->
    prim_socket:ioctl(SockRef, SetRequest, Value);
ioctl(?socket(SockRef), SetRequest, Value)
  when ((SetRequest =:= rcvall_igmpmcast) orelse
        (SetRequest =:= rcvall_mcast)) andalso
       ((Value =:= off) orelse
        (Value =:= on)) ->
    prim_socket:ioctl(SockRef, SetRequest, Value);

ioctl(Socket, Request, Arg) ->
    erlang:error(badarg, [Socket, Request, Arg]).


-spec ioctl(Socket, SetRequest, Name, Value) -> 'ok' | {'error', Reason} when
      Socket     :: socket(),
      SetRequest :: 'sifflags' |
                    'sifaddr' | 'sifdstaddr' | 'sifbrdaddr' |
                    'sifnetmask' | 'sifhwaddr' |
                    'gifmtu' | 'siftxqlen',
      Name       :: string(),
      Value      :: term(),
      Reason     :: posix() | 'closed'.

ioctl(?socket(SockRef), sifflags = SetRequest, Name, Flags)
  when is_list(Name) andalso is_map(Flags) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, Flags);
ioctl(?socket(SockRef), sifaddr = SetRequest, Name, Addr)
  when is_list(Name) andalso is_map(Addr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(Addr));
ioctl(?socket(SockRef), sifdstaddr = SetRequest, Name, DstAddr)
  when is_list(Name) andalso is_map(DstAddr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(DstAddr));
ioctl(?socket(SockRef), sifbrdaddr = SetRequest, Name, BrdAddr)
  when is_list(Name) andalso is_map(BrdAddr) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(BrdAddr));
ioctl(?socket(SockRef), sifnetmask = SetRequest, Name, NetMask)
  when is_list(Name) andalso is_map(NetMask) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, prim_socket:enc_sockaddr(NetMask));
ioctl(?socket(SockRef), sifmtu = SetRequest, Name, MTU)
  when is_list(Name) andalso is_integer(MTU) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, MTU);
ioctl(?socket(SockRef), siftxqlen = SetRequest, Name, QLen)
  when is_list(Name) andalso is_integer(QLen) ->
    prim_socket:ioctl(SockRef, SetRequest, Name, QLen);
ioctl(Socket, SetRequest, Arg1, Arg2) ->
    erlang:error(badarg, [Socket, SetRequest, Arg1, Arg2]).


%% ===========================================================================
%%
%% cancel - cancel an operation resulting in a select
%%
%% A call to accept, recv/recvfrom/recvmsg and send/sendto/sendmsg
%% can result in a select if they are called with the Timeout argument
%% set to nowait. This is indicated by the return of the select-info.
%% Such a operation can be cancelled by calling this function.
%%

-spec cancel(Socket, SelectInfo) -> 'ok' | {'error', Reason} when
      Socket         :: socket(),
      SelectInfo     :: select_info(),
      Reason         :: 'closed' | invalid();

            (Socket, CompletionInfo) -> 'ok' | {'error', Reason} when
      Socket         :: socket(),
      CompletionInfo :: completion_info(),
      Reason         :: 'closed' | invalid().

cancel(?socket(SockRef), ?SELECT_INFO(SelectTag, SelectHandle) = SelectInfo)
  when is_reference(SockRef) ->
    case SelectTag of
        {Op, _} when is_atom(Op) ->
            ok;
        Op when is_atom(Op) ->
            ok
    end,
    case cancel(SockRef, Op, SelectHandle) of
        ok ->
            ok;
        invalid ->
            {error, {invalid, SelectInfo}};
        Result ->
            Result
    end;
cancel(?socket(SockRef),
       ?COMPLETION_INFO(CompletionTag, CompletionHandle) = CompletionInfo)
  when is_reference(SockRef) ->
    case CompletionTag of
        {Op, _} when is_atom(Op) ->
            ok;
        Op when is_atom(Op) ->
            ok
    end,
    case cancel(SockRef, Op, CompletionHandle) of
        ok ->
            ok;
        invalid ->
            {error, {invalid, CompletionInfo}};
        Result ->
            Result
    end;
cancel(Socket, Info) ->
    erlang:error(badarg, [Socket, Info]).


cancel(SockRef, Op, Handle) ->
    case prim_socket:cancel(SockRef, Op, Handle) of
        select_sent ->
            _ = flush_select_msg(SockRef, Handle),
            _ = flush_abort_msg(SockRef, Handle),
            ok;
        not_found ->
            _ = flush_completion_msg(SockRef, Handle),
            _ = flush_abort_msg(SockRef, Handle),
            invalid;
        Result ->
            %% Since we do not actually know if we are using
            %% select or completion here, so flush both...
            _ = flush_select_msg(SockRef, Handle),
            _ = flush_completion_msg(SockRef, Handle),
            _ = flush_abort_msg(SockRef, Handle),
	    %% ?DBG([{op, Op}, {result, Result}]),
            Result
    end.

flush_select_msg(SockRef, Ref) ->
    receive
        ?socket_msg(?socket(SockRef), select, Ref) ->
            ok
    after 0 ->
            ok
    end.

flush_completion_msg(SockRef, Ref) ->
    receive
        ?socket_msg(?socket(SockRef), completion, {Ref, Result}) ->
            Result
    after 0 ->
            ok
    end.

flush_abort_msg(SockRef, Ref) ->
    receive
        ?socket_msg(?socket(SockRef), abort, {Ref, Reason}) ->
            Reason
    after 0 ->
            ok
    end.


%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

deadline(Timeout) ->
    case Timeout of
        nowait ->
            Timeout;
        infinity ->
            Timeout;
        Handle when is_reference(Handle) ->
            handle;
        0 ->
            zero;
        _ when is_integer(Timeout), 0 < Timeout ->
            timestamp() + Timeout;
        _ ->
            invalid
    end.

timeout(Deadline) ->
    case Deadline of
        %% nowait | handle shall not be passed here
        %%
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


f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

%% mq() ->
%%     pi(messages).

%% pi(Item) ->
%%     {Item, Val} = process_info(self(), Item),
%%     Val.
    

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
