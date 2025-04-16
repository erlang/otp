%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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

-module(prim_net).
-moduledoc false.

-compile(no_native).

%% Administrative and "global" utility functions
-export([
	 on_load/0, on_load/1,
	 info/0,
         debug/1
        ]).

-export([
         gethostname/0,
         getnameinfo/2,
         getaddrinfo/2,
         getifaddrs/1,
         get_adapters_addresses/1,
         get_if_entry/1,
         get_interface_info/1,
         get_ip_address_table/1,
         getservbyname/2,
         getservbyport/2,

         if_name2index/1,
         if_index2name/1,
         if_names/0
        ]).

-export_type([
              getifaddrs_args/0,
              interface_info_args/0,
              if_entry_args/0,
              ip_address_table_args/0,

              if_type/0,
              if_admin_status/0,
              internal_if_oper_status/0,
              mib_if_row/0,

              ip_adapter_index_map/0,
              ip_interface_info/0,

              mib_ip_address_row/0,
              mib_ip_address_table/0,

              adapters_addresses_flags/0,
              adapters_addresses_args/0,
              adapter_unicast_dad_state/0,
              adapter_unicast_prefix_origin/0,
              adapter_unicast_suffix_origin/0,
              adapter_unicast_flags/0,
              adapter_unicast_addr/0,
              adapter_anycast_flags/0,
              adapter_anycast_addr/0,
              adapter_multicast_flags/0,
              adapter_multicast_addr/0,
              adapter_dns_server_addr/0,
              adapter_flags/0,
              adapter_type/0,
              adapter_oper_status/0,
              adapter_prefix/0,
              adapter_addresses/0,
              adapters_addresses/0
             ]).


-type getifaddrs_args() :: #{netns => string()}.

-type ifaddrs_flag() :: up | broadcast | debug | loopback | pointopoint |
                        notrailers | running | noarp | promisc |
                        master | slave |
                        multicast | portsel | automedia | dynamic.
-type ifaddrs_flags() :: [ifaddrs_flag()].

%% Note that not all of these fields are mandatory.
%% *Also*, broadaddr and dstaddr are mutually exclusive!

-type ifaddrs() :: #{name      := string(),
                     flags     := ifaddrs_flags(),
                     addr      => socket:sockaddr(),
                     netmask   => socket:sockaddr(),
                     %% 'broadaddr' and 'dstaddr' are mutually exclusive
                     broadaddr => socket:sockaddr(),
                     dstaddr   => socket:sockaddr()}.

-type interface_info_args() :: #{debug => boolean()}.
-type if_entry_args() :: #{index := non_neg_integer(),
                           debug => boolean()}.
-type ip_address_table_args() :: #{debug => boolean()}.

-type if_type()       :: other | ethernet_csmacd | iso88025_tokenring |
                         fddi | ppp | software_loopback | atm | ieee80211 |
                         tunnel | ieee1394 | ieee80216_wman | wwanpp | wwanpp2.
-type if_admin_status() :: enabled | disabled.
-type internal_if_oper_status() :: non_operational |
                                   unreachable |
                                   disconnected |
                                   connecting | connected |
                                   operational |
                                   non_neg_integer().
-type mib_if_row()    :: #{name                 := string(),
                           index                := non_neg_integer(),
                           type                 := if_type(),
                           mtu                  := non_neg_integer(),
                           speed                := non_neg_integer(),
                           phys_addr            := binary(),
                           admin_status         := if_admin_status(),
                           internal_oper_status := internal_if_oper_status(),
                           last_change          := non_neg_integer(),
                           in_octets            := non_neg_integer(),
                           in_ucast_pkts        := non_neg_integer(),
                           in_nucast_pkts       := non_neg_integer(),
                           in_discards          := non_neg_integer(),
                           in_errors            := non_neg_integer(),
                           in_unknown_protos    := non_neg_integer(),
                           out_octets           := non_neg_integer(),
                           out_ucast_pkts       := non_neg_integer(),
                           out_nucast_pkts      := non_neg_integer(),
                           out_discards         := non_neg_integer(),
                           out_errors           := non_neg_integer(),
                           out_qlen             := non_neg_integer(),
                           description          := binary()}.

-type ip_adapter_index_map() :: #{index := integer(),
                                  name  := string()}.

-type ip_interface_info()    :: [ip_adapter_index_map()].

%% addr:           The IPv4 address in network byte order.
%% index:          The index of the interface associated with this
%%                 IPv4 address.
%% mask:           The subnet mask for the IPv4 address in network
%%                 byte order.
%% broadcast_addr: The broadcast address in network byte order.
%%                 A broadcast address is typically the IPv4 address
%%                 with the host portion set to either all zeros or
%%                 all ones.
%% reasm_size:     The maximum re-assembly size for received datagrams.
-type mib_ip_address_row()   :: #{addr           := non_neg_integer(),
                                  index          := non_neg_integer(),
                                  mask           := non_neg_integer(),
                                  broadcast_addr := non_neg_integer(),
                                  reasm_size     := non_neg_integer()}.
-type mib_ip_address_table() :: [mib_ip_address_row()].

-type adapters_addresses_flags() ::
        #{skip_unicast                => boolean(),  % default false
          skip_anycast                => boolean(),  % default true
          skip_multicast              => boolean(),  % default true
          skip_dns_server             => boolean(),  % default true
          skip_friendly_name          => boolean(),  % default true
          include_prefix              => boolean(),  % default true
          include_wins_info           => boolean(),  % default false
          include_gateways            => boolean(),  % default false
          include_all_interfaces      => boolean(),  % default false
          include_all_compartments    => boolean(),  % default false
          include_tunnel_bindingorder => boolean()}. % default false)

%% defaults to:
%% #{family => unspec,
%%   flags  => #{skip_unicast                => false
%%               skip_anycast                => true
%%               skip_multicast              => true
%%               skip_dns_server             => true
%%               skip_friendly_name          => true
%%               include_prefix              => true
%%               include_wins_info           => false
%%               include_gateways            => false
%%               include_all_interfaces      => false
%%               include_all_compartments    => false
%%               include_tunnel_bindingorder => false},
%%   debug  => false}.
-type adapters_addresses_args() :: #{family => unspec | inet | inet6,
                                     flags  => adapters_addresses_flags(),
                                     debug  => boolean()}.

-type adapter_unicast_dad_state() :: invalid | tentative | duplicate |
                                     deprecated | preferred |
                                     integer().
-type adapter_unicast_prefix_origin() :: other | manual | well_known | dhcp |
                                         router_advertisement | unchanged |
                                         integer().
-type adapter_unicast_suffix_origin() :: other | manual | well_known | dhcp |
                                         link_layer_address | random |
                                         unchanged |
                                         integer().
-type adapter_unicast_flags() :: #{dns_eligible := boolean(),
                                   transient    := boolean()}.

-type adapter_unicast_addr() ::
        #{addr               := socket:sockaddr(),
          dad_state          := adapter_unicast_dad_state(),
          flags              := adapter_unicast_flags(),
          prefix_origin      := adapter_unicast_prefix_origin(),
          suffix_origin      := adapter_unicast_suffix_origin(),
          lease_lifetime     := non_neg_integer(),
          preferred_lifetime := non_neg_integer(),
          valid_lifetime     := non_neg_integer()}.

-type adapter_anycast_flags() :: #{dns_eligible := boolean(),
                                   transient    := boolean()}.

-type adapter_anycast_addr() :: #{addr  := socket:sockaddr(),
                                  flags := adapter_anycast_flags()}.

-type adapter_multicast_flags() :: #{dns_eligible := boolean(),
                                     transient    := boolean()}.

-type adapter_multicast_addr() :: #{addr  := socket:sockaddr(),
                                    flags := adapter_multicast_flags()}.

-type adapter_dns_server_addr() :: #{addr := socket:sockaddr()}.

-type adapter_flags() :: #{ddns_enabled                          := boolean(),
                           register_adapter_suffix               := boolean(),
                           dhcp_v4_enabled                       := boolean(),
                           receive_only                          := boolean(),
                           no_multicast                          := boolean(),
                           ipv6_other_stateful_config            := boolean(),
                           netbios_over_tcpip_enabled            := boolean(),
                           ipv4_enabled                          := boolean(),
                           ipv6_enabled                          := boolean(),
                           ipv6_managed_address_config_supported := boolean()}.

-type adapter_type() :: other | ethernet_csmacd | iso88025_tokenring |
                        fddi | ppp | software_loopback | atm | ieee80211 |
                        tunnel | ieee1394 | ieee80216_wman | wwanpp | wwanpp2 |
                        integer().

-type adapter_oper_status() :: up | down | testing | dormant | not_present |
                               lower_layer_down |
                               integer().

-type adapter_prefix() :: #{addr   := socket:sockaddr(),
                            length := non_neg_integer()}.
-type adapter_addresses()  ::
        #{index            := non_neg_integer(),
          name             := string(),
          unicast_addrs    := [adapter_unicast_addr()],
          anycast_addrs    := [adapter_anycast_addr()],
          multicast_addrs  := [adapter_multicast_addr()],
          dns_server_addrs := [adapter_dns_server_addr()],
          dns_suffix       := string(),
          description      := string(),
          friendly_name    := string(),
          phys_addr        := binary(),
          flags            := adapter_flags(),
          mtu              := non_neg_integer(),
          type             := adapter_type(),
          oper_status      := adapter_oper_status(),
          zone_indices     := [non_neg_integer()], % Length 16
          ipv6_index       := non_neg_integer(),
          prefixes         := [adapter_prefix()]}.
-type adapters_addresses() :: [adapter_addresses()].

-type name_info_flag()          :: namereqd |
                                   dgram |
                                   nofqdn |
                                   numerichost |
                                   numericserv.
%% The following (ext) flags has been removed
%% (as they are deprecated by later version of gcc):
%%    idn_allow_unassigned | idn_use_std3_ascii_rules.
-type name_info_flag_ext()      :: idn.
-type name_info_flags()         :: [name_info_flag()|name_info_flag_ext()].
-type name_info()               :: #{host    := string(),
                                     service := string()}.
-type address_info()            :: #{family   := socket:domain(),
                                     socktype := socket:type(),
                                     protocol := socket:protocol(),
                                     address  := socket:sockaddr()}.
-type network_interface_name()  :: string().
-type network_interface_index() :: non_neg_integer().


-define(NO_SKIPS_ALL_INCLUDES,
        #{skip_unicast                => false,
          skip_anycast                => false,
          skip_multicast              => false,
          skip_dns_server             => false,
          skip_friendly_name          => false,
          include_prefix              => true,
          include_wins_info           => true,
          include_gateways            => true,
          include_all_interfaces      => true,
          include_all_compartments    => true,
          include_tunnel_bindingorder => true}).
-define(ALL_SKIPS_NO_INCLUDES,
        #{skip_unicast                => true,
          skip_anycast                => true,
          skip_multicast              => true,
          skip_dns_server             => true,
          skip_friendly_name          => true,
          include_prefix              => false,
          include_wins_info           => false,
          include_gateways            => false,
          include_all_interfaces      => false,
          include_all_compartments    => false,
          include_tunnel_bindingorder => false}).
-define(NO_SKIPS_NO_INCLUDES,
        #{skip_unicast                => false,
          skip_anycast                => false,
          skip_multicast              => false,
          skip_dns_server             => false,
          skip_friendly_name          => false,
          include_prefix              => false,
          include_wins_info           => false,
          include_gateways            => false,
          include_all_interfaces      => false,
          include_all_compartments    => false,
          include_tunnel_bindingorder => false}).
-define(ALL_SKIPS_ALL_INCLUDES,
        #{skip_unicast                => true,
          skip_anycast                => true,
          skip_multicast              => true,
          skip_dns_server             => true,
          skip_friendly_name          => true,
          include_prefix              => true,
          include_wins_info           => true,
          include_gateways            => true,
          include_all_interfaces      => true,
          include_all_compartments    => true,
          include_tunnel_bindingorder => true}).


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
    %% This will fail if the user has disabled esock support, making all NIFs
    %% fall back to their Erlang implementation which throws `notsup`.
    _ = erlang:load_nif(atom_to_list(net), Extra),
    ok.


-spec info() -> map().

info() ->
    nif_info().


debug(D) ->
    nif_command(#{command => ?FUNCTION_NAME, data => D}).



%% ===========================================================================
%%
%% The proper net API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% gethostname - Get the name of the current host.
%%
%%

-spec gethostname() -> {ok, HostName} | {error, Reason} when
      HostName :: string(),
      Reason   :: term().

gethostname() ->
    nif_gethostname().


%% ===========================================================================
%%
%% getnameinfo - Address-to-name translation in protocol-independent manner.
%%
%%

-spec getnameinfo(SockAddr, Flags) -> {ok, Info} | {error, Reason} when
      SockAddr :: socket:sockaddr(),
      Flags    :: name_info_flags() | undefined,
      Info     :: name_info(),
      Reason   :: term().

getnameinfo(SockAddr, Flags) ->
    try prim_socket:enc_sockaddr(SockAddr) of
        ESockAddr ->
            nif_getnameinfo(ESockAddr, Flags)
    catch
        throw : ERROR ->
            ERROR
    end.


%% ===========================================================================
%%
%% getaddrinfo - Network address and service translation
%%
%% There is also a "hint" argument that we "at some point" should implement.

-spec getaddrinfo(Host, undefined) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Info    :: [address_info()],
      Reason  :: term()
                 ; (undefined, Service) -> {ok, Info} | {error, Reason} when
      Service :: string(),
      Info    :: [address_info()],
      Reason  :: term()
                 ; (Host, Service) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Service :: string(),
      Info    :: [address_info()],
      Reason  :: term().

getaddrinfo(Host, Service)
  when (is_list(Host) orelse (Host =:= undefined)) andalso
       (is_list(Service) orelse (Service =:= undefined)) andalso
       (not ((Service =:= undefined) andalso (Host =:= undefined))) ->
    Result = nif_getaddrinfo(Host, Service, undefined),
    case Result of
        {ok, []} ->
            Result;
        {ok, Addrs} ->
            Protocols = prim_socket:p_get(protocols),
            {ok,
             [case Addr of
                  #{protocol := Num} ->
                      case Protocols of
                          #{Num := [Protocol | _Aliases]} ->
                              Addr#{protocol := Protocol};
                          #{} ->
                              Addr
                      end;
                  #{} ->
                      Addr
              end || Addr <- Addrs]};
        Error ->
            Error
    end.



%% ===========================================================================
%%
%% getifaddrs - Get interface addresses
%%

-spec getifaddrs(Extra) -> {ok, IfInfo} | {error, Reason} when
      Extra   :: map(),
      IfInfo  :: [ifaddrs()],
      Reason  :: term().

getifaddrs(Extra) when is_map(Extra) ->
    nif_getifaddrs(Extra).



%% ===========================================================================
%%
%% get_adapters_addresses - Get adapters addresses
%%

-spec get_adapters_addresses(Args) -> {ok, Addrs} | {error, Reason} when
      Args   :: default |
                no_skips_all_includes | no_skips_no_includes |
                all_skips_no_includes | all_skips_all_includes |
                adapters_addresses_args(),
      Addrs  :: adapters_addresses(),
      Reason :: term().

get_adapters_addresses(default) ->
    Args = #{},
    get_adapters_addresses(Args);
get_adapters_addresses(no_skips_all_includes) ->
    Args = #{flags => ?NO_SKIPS_ALL_INCLUDES},
    get_adapters_addresses(Args);
get_adapters_addresses(no_skips_no_includes) ->
    Args = #{flags => ?NO_SKIPS_NO_INCLUDES},
    get_adapters_addresses(Args);
get_adapters_addresses(all_skips_no_includes) ->
    Args = #{flags => ?ALL_SKIPS_NO_INCLUDES},
    get_adapters_addresses(Args);
get_adapters_addresses(all_skips_all_includes) ->
    Args = #{flags => ?ALL_SKIPS_ALL_INCLUDES},
    get_adapters_addresses(Args);
get_adapters_addresses(Args) when is_map(Args) ->
    nif_get_adapters_addresses(Args).



%% ===========================================================================
%%
%% get_interface_info - Get interface info
%%

-spec get_interface_info(Args) -> {ok, IfInfo} | {error, Reason} when
      Args   :: interface_info_args(),
      IfInfo :: ip_interface_info(),
      Reason :: term().

get_interface_info(Args) when is_map(Args) ->
    nif_get_interface_info(Args).



%% ===========================================================================
%%
%% get_if_entry - Get interface info
%%

-spec get_if_entry(Args) -> {ok, Info} | {error, Reason} when
      Args   :: if_entry_args(),
      Info   :: mib_if_row(),
      Reason :: term().

get_if_entry(Args) when is_map(Args) ->
    nif_get_if_entry(Args).



%% ===========================================================================
%%
%% get_ip_address_table - Get (mib) address table
%%

-spec get_ip_address_table(Args) -> {ok, Info} | {error, Reason} when
      Args   :: ip_address_table_args(),
      Info   :: term(),
      Reason :: term().

get_ip_address_table(Args) when is_map(Args) ->
    nif_get_ip_address_table(Args).



%% ===========================================================================
%%
%% getservbyname - Get service by name
%%

getservbyname(Name, Proto) when is_list(Name) andalso is_list(Proto) ->
    nif_getservbyname(Name, Proto).



%% ===========================================================================
%%
%% getservbyport - Get service by name
%%

getservbyport(PortNumber, Proto)
  when is_integer(PortNumber) andalso is_list(Proto) ->
    nif_getservbyport(PortNumber, Proto).



%% ===========================================================================
%%
%% if_name2index - Mappings between network interface names and indexes:
%%                 name -> idx
%%
%%

-spec if_name2index(Name) -> {ok, Idx} | {error, Reason} when
      Name   :: network_interface_name(),
      Idx    :: network_interface_index(),
      Reason :: term().

if_name2index(If) when is_list(If) ->
    nif_if_name2index(If).



%% ===========================================================================
%%
%% if_index2name - Mappings between network interface index and names:
%%                 idx -> name
%%
%%

-spec if_index2name(Idx) -> {ok, Name} | {error, Reason} when
      Idx    :: network_interface_index(),
      Name   :: network_interface_name(),
      Reason :: term().

if_index2name(Idx) when is_integer(Idx) ->
    nif_if_index2name(Idx).



%% ===========================================================================
%%
%% if_names - Get network interface names and indexes
%%
%%

-spec if_names() -> {ok, Names} | {error, Reason} when
      Names  :: [{Idx, If}],
      Idx    :: network_interface_index(),
      If     :: network_interface_name(),
      Reason :: term().

if_names() ->
    nif_if_names().



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


%% ===========================================================================
%%
%% The actual NIF-functions.
%%
%% ===========================================================================

nif_info() ->
    erlang:nif_error(notsup).

nif_command(_Cmd) ->
    erlang:nif_error(notsup).

nif_gethostname() ->
    erlang:nif_error(notsup).

nif_getnameinfo(_Addr, _Flags) ->
    erlang:nif_error(notsup).

nif_getaddrinfo(_Host, _Service, _Hints) ->
    erlang:nif_error(notsup).

nif_getifaddrs(_Extra) ->
    erlang:nif_error(notsup).

nif_get_adapters_addresses(_Args) ->
    erlang:nif_error(notsup).

nif_get_if_entry(_Args) ->
    erlang:nif_error(notsup).

nif_get_interface_info(_Args) ->
    erlang:nif_error(notsup).

nif_get_ip_address_table(_Args) ->
    erlang:nif_error(notsup).

nif_getservbyname(_Name, _Proto) ->
    erlang:nif_error(notsup).

nif_getservbyport(_PortNumber, _Proto) ->
    erlang:nif_error(notsup).

nif_if_name2index(_Name) ->
    erlang:nif_error(notsup).

nif_if_index2name(_Id) ->
    erlang:nif_error(notsup).

nif_if_names() ->
    erlang:nif_error(notsup).

