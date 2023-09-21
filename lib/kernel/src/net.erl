%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2023. All Rights Reserved.
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

-module(net).

%% Administrative and utility functions
-export([
	 info/0,
         command/1
        ]).

-export([
         gethostname/0,
         getnameinfo/1, getnameinfo/2,
         getaddrinfo/1, getaddrinfo/2,
         getifaddrs/0,  getifaddrs/1, getifaddrs/2,

         if_name2index/1,
         if_index2name/1,
         if_names/0
        ]).

%% Deprecated functions from the "old" net module
-export([call/4,
	 cast/4,
	 broadcast/3,
	 ping/1,
	 sleep/1]).

%% Should we define these here or refer to the prim_net module
-export_type([
              address_info/0,
              ifaddrs/0,
              name_info/0,

              ifaddrs_flag/0,
              ifaddrs_flags/0,

              name_info_flags/0,
              name_info_flag/0,
              name_info_flag_ext/0,

              network_interface_name/0,
              network_interface_index/0
             ]).


-deprecated({call,      4, "use rpc:call/4 instead"}).
-deprecated({cast,      4, "use rpc:cast/4 instead"}).
-deprecated({broadcast, 3, "use rpc:eval_everywhere/3 instead"}).
-deprecated({ping,      1, "use net_adm:ping/1 instead"}).
-removed({relay,        1, "use fun Relay(Pid) -> receive X -> Pid ! X end, Relay(Pid) instead"}).
-deprecated({sleep,     1, "use 'receive after T -> ok end' instead"}).


-type ifaddrs_flag() :: up | broadcast | debug | loopback | pointopoint |
                        notrailers | running | noarp | promisc |
                        master | slave |
                        multicast | portsel | automedia | dynamic.
-type ifaddrs_flags() :: [ifaddrs_flag()].

%% Note that not all of these fields are mandatory.
%% Actually there are (error) cases when only the name will be included.
%% And broadaddr and dstaddr are mutually exclusive!

-type ifaddrs() :: #{name      := string(),
                     flags     := ifaddrs_flags(),
                     addr      => socket:sockaddr(),
                     netmask   => socket:sockaddr(),
                     broadaddr => socket:sockaddr(),
                     dstaddr   => socket:sockaddr()}.

-type ifaddrs_filter()     :: all | default | inet | inet6 | packet |
                              ifaddrs_filter_map() |
                              ifaddrs_filter_fun().
-type ifaddrs_filter_map() :: #{family := default | local |
                                inet | inet6 | packet | all,
                                flags  := any | [ifaddrs_flag()]}.
-type ifaddrs_filter_fun() :: fun((ifaddrs()) -> boolean()).

-type name_info_flags()         :: [name_info_flag()|name_info_flag_ext()].
-type name_info_flag()          :: namereqd |
                                   dgram |
                                   nofqdn |
                                   numerichost |
                                   numericserv.
%% The following (ext) flags has been removed
%% (as they are deprecated by later version of gcc):
%%    idn_allow_unassigned | idn_use_std3_ascii_rules.
-type name_info_flag_ext()      :: idn.
-type name_info()               :: #{host    := string(),
                                     service := string()}.
-type address_info()            :: #{family   := socket:domain(),
                                     socktype := socket:type(),
                                     protocol := socket:protocol(),
                                     address  := socket:sockaddr()}.
-type network_interface_name()  :: string().
-type network_interface_index() :: non_neg_integer().


-define(GAA_NAMES_FLAGS(),
        #{skip_unicast       => true,
          skip_friendly_name => false,
          include_prefix     => false}).
-define(GAA_INDEXES_FLAGS(),
        #{skip_unicast       => true,
          skip_friendly_name => true,
          include_prefix     => false}).

%% This is just a "fake" based on what it looks like on linux...
-define(BADDR_FOR_PHYS_ADDR,
        <<16#FF:8,16#FF:8,16#FF:8,16#FF:8,16#FF:8,16#FF:8>>).



%% ===========================================================================
%%
%% D E P R E C A T E D   F U N C T I O N S
%%
%% ===========================================================================

call(N,M,F,A) -> rpc:call(N,M,F,A).
cast(N,M,F,A) -> rpc:cast(N,M,F,A).
broadcast(M,F,A) -> rpc:eval_everywhere(M,F,A).
ping(Node) -> net_adm:ping(Node).
sleep(T) -> receive after T -> ok end.


%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

-spec info() -> map().
info() ->
    prim_net:info().

-spec command(Cmd :: term()) -> term().
command(Cmd) ->
    prim_net:command(Cmd).

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
    prim_net:gethostname().

%% ===========================================================================
%%
%% getnameinfo - Address-to-name translation in protocol-independent manner.
%%
%%

-spec getnameinfo(SockAddr) -> {ok, Info} | {error, Reason} when
      SockAddr :: socket:sockaddr(),
      Info     :: name_info(),
      Reason   :: term().
getnameinfo(SockAddr) ->
    getnameinfo(SockAddr, undefined).

-spec getnameinfo(SockAddr, Flags) -> {ok, Info} | {error, Reason} when
      SockAddr :: socket:sockaddr(),
      Flags    :: name_info_flags() | undefined,
      Info     :: name_info(),
      Reason   :: term().
getnameinfo(SockAddr, Flags)
  when is_map(SockAddr), is_list(Flags);
       is_map(SockAddr), Flags =:= undefined ->
    prim_net:getnameinfo(SockAddr, Flags).

%% ===========================================================================
%%
%% getaddrinfo - Network address and service translation
%%
%% There is also a "hint" argument that we "at some point" should implement.

-spec getaddrinfo(Host) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Info    :: [address_info()],
      Reason  :: term().
getaddrinfo(Host) when is_list(Host) ->
    getaddrinfo(Host, undefined).

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
    prim_net:getaddrinfo(Host, Service).

%% ===========================================================================
%%
%% getifaddrs - Get interface addresses
%%

-spec getifaddrs() -> {ok, IfAddrs} | {error, Reason} when
      IfAddrs :: [ifaddrs()],
      Reason  :: term().
getifaddrs() ->
    getifaddrs(default).

-spec getifaddrs(Filter) -> {ok, IfAddrs} | {error, Reason} when
      Filter    :: ifaddrs_filter(),
      IfAddrs   :: [ifaddrs()],
      Reason    :: term();
                (Namespace) -> {ok, IfAddrs} | {error, Reason} when
      Namespace :: file:filename_all(),
      IfAddrs   :: [ifaddrs()],
      Reason    :: term().
getifaddrs(Filter) when is_atom(Filter) orelse is_map(Filter) ->
    do_getifaddrs(getifaddrs_filter_map(Filter),
                  fun() -> prim_net:getifaddrs(#{}) end);
getifaddrs(Filter) when is_function(Filter, 1) ->
    do_getifaddrs(Filter, fun() -> prim_net:getifaddrs(#{}) end);
getifaddrs(Namespace) when is_list(Namespace) ->
    getifaddrs(default, Namespace).

-spec getifaddrs(Filter, Namespace) -> {ok, IfAddrs} | {error, Reason} when
      Filter    :: ifaddrs_filter(),
      Namespace :: file:filename_all(),
      IfAddrs   :: [ifaddrs()],
      Reason    :: term().
getifaddrs(Filter, Namespace)
  when (is_atom(Filter) orelse is_map(Filter)) andalso is_list(Namespace) ->
    do_getifaddrs(getifaddrs_filter_map(Filter),
                  fun() -> getifaddrs(Namespace) end);
getifaddrs(Filter, Namespace)
  when is_function(Filter, 1) andalso is_list(Namespace) ->
    do_getifaddrs(Filter, fun() -> getifaddrs(Namespace) end).


do_getifaddrs(Filter, GetIfAddrs) ->
    try GetIfAddrs() of
        {ok, IfAddrs0} when is_function(Filter) ->
            {ok, lists:filtermap(Filter, IfAddrs0)};
        {ok, IfAddrs0} when is_map(Filter) ->
            FilterFun = fun(Elem) -> getifaddrs_filter(Filter, Elem) end,
            {ok, lists:filtermap(FilterFun, IfAddrs0)};
        {error, _} = ERROR ->
            ERROR
    catch
        C : E : S when (C =:= error) andalso (E =:= notsup) ->
            %% This means we are on windows, and should
            %% call the windows specific functions:
            %%    get_interface_info
            %%    get_ip_addr_table
            %%    get_adapters_addresses
            try win_getifaddrs(Filter)
            catch
                _WC:_WE:_WS ->
                    erlang:raise(C, E, S)
            end
    end.

getifaddrs_filter_map(all) ->
    getifaddrs_filter_map_all();
getifaddrs_filter_map(default) ->
    getifaddrs_filter_map_default();
getifaddrs_filter_map(inet) ->
    getifaddrs_filter_map_inet();
getifaddrs_filter_map(inet6) ->
    getifaddrs_filter_map_inet6();
getifaddrs_filter_map(packet) ->
    getifaddrs_filter_map_packet();
getifaddrs_filter_map(FilterMap) when is_map(FilterMap) ->
    maps:merge(getifaddrs_filter_map_default(), FilterMap).

getifaddrs_filter_map_all() ->
    #{family => all, flags => any}.

getifaddrs_filter_map_default() ->
    #{family => default, flags => any}.

getifaddrs_filter_map_inet() ->
    #{family => inet, flags => any}.

getifaddrs_filter_map_inet6() ->
    #{family => inet6, flags => any}.

getifaddrs_filter_map_packet() ->
    #{family => packet, flags => any}.

-compile({nowarn_unused_function, getifaddrs_filter/2}).

getifaddrs_filter(#{family := FFamily, flags := FFlags},
                  #{addr := #{family := Family}, flags := Flags} = _Entry)
  when (FFamily =:= default) andalso
       ((Family =:= inet) orelse (Family =:= inet6)) ->
    getifaddrs_filter_flags(FFlags, Flags);
getifaddrs_filter(#{family := FFamily, flags := FFlags},
                  #{addr := #{family := Family}, flags := Flags} = _Entry)
  when (FFamily =:= inet) andalso (Family =:= inet) ->
    getifaddrs_filter_flags(FFlags, Flags);
getifaddrs_filter(#{family := FFamily, flags := FFlags},
                  #{addr := #{family := Family}, flags := Flags} = _Entry)
  when (FFamily =:= inet6) andalso (Family =:= inet6) ->
    getifaddrs_filter_flags(FFlags, Flags);
getifaddrs_filter(#{family := FFamily, flags := FFlags},
                  #{addr := #{family := Family}, flags := Flags} = _Entry)
  when (FFamily =:= packet) andalso (Family =:= packet) ->
    getifaddrs_filter_flags(FFlags, Flags);
getifaddrs_filter(#{family := FFamily, flags := FFlags},
                  #{flags := Flags} = _Entry)
  when (FFamily =:= all) ->
    getifaddrs_filter_flags(FFlags, Flags);
getifaddrs_filter(_Filter, _Entry) ->
    false.

-compile({nowarn_unused_function, getifaddrs_filter_flags/2}).

getifaddrs_filter_flags(any, _Flags) ->
    true;
getifaddrs_filter_flags(FilterFlags, Flags) ->
    [] =:= (FilterFlags -- Flags).


win_getifaddrs(Filter) ->
    AdsAddrs  = get_adapters_addresses(),
    IpAddrTab = get_ip_address_table(AdsAddrs),
    IpIfInfo  = get_interface_info(),
    win_getifaddrs(Filter, AdsAddrs, IpAddrTab, IpIfInfo).

get_adapters_addresses() ->
    case prim_net:get_adapters_addresses(#{}) of
        {ok, AdaptersAddresses} ->
            AdaptersAddresses;
        {error, _} ->
            []
    end.

get_ip_address_table([]) ->
    case prim_net:get_ip_address_table(#{}) of
        {ok, IpAddressTable} ->
            IpAddressTable;
        {error, _} ->
            []
    end;
get_ip_address_table(_) ->
    [].

get_interface_info() ->
    case prim_net:get_interface_info(#{}) of
        {ok, InterfaceInfo} ->
            InterfaceInfo;
        {error, _} ->
            []
    end.
    

win_getifaddrs(Filter, [], IpAddrTab, IpIfInfo) when (IpAddrTab =/= []) ->
    win_getifaddrs_iat(Filter, IpAddrTab, IpIfInfo);
win_getifaddrs(Filter, AdsAddrs, _IpAddrTab, IpIfInfo) when (AdsAddrs =/= []) ->
    win_getifaddrs_aa(Filter, AdsAddrs, IpIfInfo);
win_getifaddrs(_Filter, AdsAddrs, IpAddrTab, IpIfInfo) ->
    throw({error, {insufficient_info, AdsAddrs, IpAddrTab, IpIfInfo}}).

win_getifaddrs_iat(Filter, IpAddrTab, IpIfInfos) when is_function(Filter) ->
    IfAddrs = win_getifaddrs_iat2(IpAddrTab, IpIfInfos),
    {ok, lists:filtermap(Filter, IfAddrs)};
win_getifaddrs_iat(Filter, IpAddrTab, IpIfInfos) when is_map(Filter) ->
    IfAddrs   = win_getifaddrs_iat2(IpAddrTab, IpIfInfos),
    FilterFun = fun(Elem) -> getifaddrs_filter(Filter, Elem) end,
    {ok, lists:filtermap(FilterFun, IfAddrs)}.

win_getifaddrs_iat2(IpAddrTab, IpIfInfos) ->
    win_getifaddrs_iat2(IpAddrTab, IpIfInfos, []).

win_getifaddrs_iat2([], _IpIfInfos, Acc) ->
    lists:reverse(Acc);
win_getifaddrs_iat2([#{index := Idx} = IpAddr|IpAddrTab], IpIfInfos, Acc) ->
    case prim_net:get_if_entry(#{index => Idx}) of
        {ok, #{name := Name} = IfEntry} when (Name =/= "") ->
            %% Note that here its (IfAddr) *not* a list!
            {IfAddr, PktIfAddr} = win_getifaddrs_iat3(Name, IpAddr, IfEntry),
            win_getifaddrs_iat2(IpAddrTab, IpIfInfos, [IfAddr, PktIfAddr|Acc]);
        {ok, #{name        := Name,
               description := Desc} = IfEntry} when (Name =:= "") ->
            case if_info_search(Idx, IpIfInfos) of
                {value, #{name := Name2}} ->
                    %% Note that here its (IfAddr) *not* a list!
                    {IfAddr, PktIfAddr} =
                        win_getifaddrs_iat3(Name2, IpAddr, IfEntry),
                    win_getifaddrs_iat2(IpAddrTab, IpIfInfos,
                                        [IfAddr, PktIfAddr|Acc]);
                false ->
                    %% Use description
                    %% Note that here its (IfAddr) *not* a list!
                    {IfAddr, PktIfAddr} =
                        win_getifaddrs_iat3(Desc, IpAddr, IfEntry),
                    win_getifaddrs_iat2(IpAddrTab, IpIfInfos,
                                        [IfAddr, PktIfAddr|Acc])
            end;
        {error, _} ->
            win_getifaddrs_iat2(IpAddrTab, IpIfInfos, Acc)
    end.

win_getifaddrs_iat3(Name, 
                   #{addr                 := Addr,
                     mask                 := Mask,
                     %% Why do we skip this and "make" our own?
                     bcast_addr           := _BCastAddr} = _IpAddr,
                   #{type                 := Type,
                     admin_status         := AStatus,
                     internal_oper_status := _OStatus,
                     phys_addr            := PhysAddr,
                     index                := Idx} = _IfEntry) ->
    Flags1 = case Type of
                 ethernet_csmacd ->
                     [broadcast,multicast];
                 software_loopback ->
                     [loopback];
                 _ ->
                     []
             end,
    Flags2 = case AStatus of
                  non_operational ->
                     [];
                connecting ->
                     [up, pointtopoint];
                 connected ->
                     [up, runnning, pointtopoint];
                 operational ->
                     [up, running];
                 _ ->
                     [up]
             end,
    Flags  = lists:sort(Flags1 ++ Flags2),
    HaType = type2hatype(Type),
    PktSockAddr = #{addr     => process_phys_addr(HaType, PhysAddr),
                    family   => packet,
                    hatype   => HaType,
                    ifindex  => Idx,
                    pkttype  => host,
                    protocol => 0},
    PktIfAddr =
        case HaType of
            loopback ->
                #{name  => Name,
                  addr  => PktSockAddr,
                  flags => Flags};
            _ ->
                #{name      => Name,
                  addr      => PktSockAddr,
                  broadaddr => ?BADDR_FOR_PHYS_ADDR,
                  flags     => Flags}
        end,
    IfAddr = #{name      => Name,
               flags     => Flags,
               addr      => mk_sockaddr_in(Addr),
               netmask   => mk_sockaddr_in(Mask),
               %% And fake the broadcast address...
               broadaddr => mk_sockaddr_in(iat_broadaddr(Addr, Mask))},
    {IfAddr, PktIfAddr}.

mk_sockaddr_in(Addr) ->
    #{addr => Addr, family => inet, port => 0}.

win_getifaddrs_aa(Filter, AdsAddrs, IpIfInfo) when is_function(Filter) ->
    IfAddrs = win_getifaddrs_aa2(AdsAddrs, IpIfInfo),
    {ok, lists:filtermap(Filter, IfAddrs)};
win_getifaddrs_aa(Filter, AdsAddrs, IpIfInfo) when is_map(Filter) ->
    IfAddrs   = win_getifaddrs_aa2(AdsAddrs, IpIfInfo),
    FilterFun = fun(Elem) -> getifaddrs_filter(Filter, Elem) end,
    {ok, lists:filtermap(FilterFun, IfAddrs)}.

win_getifaddrs_aa2(AdsAddrs, IpIfInfo) ->
    win_getifaddrs_aa2(AdsAddrs, IpIfInfo, []).

win_getifaddrs_aa2([], _IpIfInfo, Acc) ->
    lists:reverse(lists:flatten(Acc));
win_getifaddrs_aa2([#{index := Idx} = AdAddrs|AdsAddrs], IpIfInfos, Acc) ->
    case prim_net:get_if_entry(#{index => Idx}) of
        {ok, #{name := Name} = IfEntry} when (Name =/= "") ->
            {IfAddrs, PktIfAddr} = win_getifaddrs_aa3(Name, AdAddrs, IfEntry),
            win_getifaddrs_aa2(AdsAddrs, IpIfInfos, [IfAddrs,PktIfAddr|Acc]);
        {ok, #{name        := Name,
               description := Desc} = IfEntry} when (Name =:= "") ->
            case if_info_search(Idx, IpIfInfos) of
                {value, #{name := Name2}} ->
                    {IfAddrs, PktIfAddr} =
                        win_getifaddrs_aa3(Name2, AdAddrs, IfEntry),
                    win_getifaddrs_aa2(AdsAddrs, IpIfInfos,
                                       [IfAddrs, PktIfAddr|Acc]);
                false ->
                    %% Use description
                    {IfAddrs, PktIfAddr} =
                        win_getifaddrs_aa3(Desc, AdAddrs, IfEntry),
                    win_getifaddrs_aa2(AdsAddrs, IpIfInfos,
                                       [IfAddrs, PktIfAddr|Acc])
            end;
        {error, _} ->
            win_getifaddrs_aa2(AdsAddrs, IpIfInfos, Acc)
    end.

win_getifaddrs_aa3(Name,
                   #{flags                := #{no_multicast := NoMC},
                     unicast_addrs        := UCastAddrs,
                     prefixes             := Prefixes} = _AdAddrs,
                   #{type                 := Type,
                     admin_status         := AStatus,
                     internal_oper_status := _OStatus,
                     phys_addr            := PhysAddr,
                     index                := Idx} = _IfEntry) ->
    Flags1 =
        if (NoMC =:= false) ->
                [multicast];
           true ->
                []
        end,                          
    Flags2 = case Type of
                 ethernet_csmacd ->
                     [broadcast];
                 software_loopback ->
                     [loopback];
                 _ ->
                     []
             end,
    Flags3 = case AStatus of
                 non_operational ->
                     [];
                 connecting ->
                     [up, pointtopoint];
                 connected ->
                     [up, runnning, pointtopoint];
                 operational ->
                     [up, running];
                 _ ->
                     [up]
             end,
    Flags  = lists:sort(Flags1 ++ Flags2 ++ Flags3),
    HaType = type2hatype(Type),
    PktSockAddr = #{addr     => process_phys_addr(HaType, PhysAddr),
                    family   => packet,
                    hatype   => HaType,
                    ifindex  => Idx,
                    pkttype  => host,
                    protocol => 0},
    PktIfAddr =
        case HaType of
            loopback ->
                #{name  => Name,
                  addr  => PktSockAddr,
                  flags => Flags};
            _ ->
                #{name      => Name,
                  addr      => PktSockAddr,
                  broadaddr => ?BADDR_FOR_PHYS_ADDR,
                  flags     => Flags}
        end,
    IfAddrs = [win_getifaddrs_aa4(Name,
                                  Flags,
                                  Prefixes,
                                  UCastAddr) || UCastAddr <- UCastAddrs],
    {IfAddrs, PktIfAddr}.

type2hatype(ethernet_csmacd) ->
    ether;
type2hatype(software_loopback) ->
    loopback;
type2hatype(Other) ->
    Other.

process_phys_addr(loopback, <<>>) ->
    <<0:8,0:8,0:8,0:8,0:8,0:8>>;
process_phys_addr(_, Bin) when is_binary(Bin) ->
    Bin.

                               

win_getifaddrs_aa4(Name, Flags, Prefixes,
                   #{addr := #{family := Fam} = Addr} = UCAddr) ->
    SPrefix = shortest_matching_prefix(UCAddr, Prefixes),
    Mask    = win_getifaddrs_mask(SPrefix),
    case lists:member(broadcast, Flags) of
        true when (Fam =:= inet) ->
            BroadAddr = win_getifaddrs_broadaddr(Mask, SPrefix),
            #{name      => Name,
              flags     => Flags,
              addr      => Addr,
              netmask   => Mask,
              broadaddr => BroadAddr};
        _ ->
            #{name      => Name,
              flags     => Flags,
              addr      => Addr,
              netmask   => Mask}
    end.

shortest_matching_prefix(#{addr := Addr} = UCAddr, Prefixes) ->
    SPrefix = #{addr => Addr, length => undefined},
    shortest_matching_prefix(UCAddr, Prefixes, SPrefix).

shortest_matching_prefix(#{addr := #{family := inet,
                                     addr   := {A, _, _, _}} = Addr}, [],
                           #{length := undefined}) ->
    %% Fall back to old classfull network addresses
    Shortest =
        if
            (A < 128) ->
                %% Class A: 1-127
                8;
            (A < 192) ->
                %% Class B: 128-191
                16;
            (A < 224) ->
                %% Class C: 192-223
                24;
            true ->
                %% Class D: 224-239
                32
        end,
    #{addr => Addr, length => Shortest};
shortest_matching_prefix(#{addr := #{family := inet6} = Addr}, [],
                         #{length := undefined}) ->
    %% Just play it safe
    #{addr => Addr, length => 128};
shortest_matching_prefix(_UCAddr, [], SPrefix) ->
    SPrefix;
shortest_matching_prefix(
  #{addr := #{family  := Fam, addr := Addr}} = UCAddr,
  [#{addr   := #{family := Fam, addr := PAddr},
     length := PLen} = Prefix|Prefixes],
  #{length := SPLen} = SPrefix)
  when (Fam =:= inet) ->
    Mask = <<(16#FFFFFFFF bsl (32 - PLen)):32>>,
    case masked_eq(Mask, ipv4_to_bin(Addr), ipv4_to_bin(PAddr)) of
        true ->
            if
                (SPLen =:= undefined) ->
                    shortest_matching_prefix(UCAddr, Prefixes, Prefix);
                (PLen < SPLen) ->
                    shortest_matching_prefix(UCAddr, Prefixes, Prefix);
                true ->
                    shortest_matching_prefix(UCAddr, Prefixes, SPrefix)
            end;
        false ->
            shortest_matching_prefix(UCAddr, Prefixes, SPrefix)
    end;
shortest_matching_prefix(
  #{addr := #{family  := Fam, addr := Addr}} = UCAddr,
  [#{addr   := #{family := Fam, addr := PAddr},
     length := PLen} = Prefix|Prefixes],
  #{length := SPLen} = SPrefix) when (Fam =:= inet6) ->
    Mask = <<(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF bsl (128 - PLen)):128>>,
    case masked_eq(Mask, ipv6_to_bin(Addr), ipv6_to_bin(PAddr)) of
        true ->
            if
                (SPLen =:= undefined) ->
                    shortest_matching_prefix(UCAddr, Prefixes, Prefix);
                (PLen < SPLen) ->
                    shortest_matching_prefix(UCAddr, Prefixes, Prefix);
               true ->
                    shortest_matching_prefix(UCAddr, Prefixes, SPrefix)
            end;
        false ->
            shortest_matching_prefix(UCAddr, Prefixes, SPrefix)
    end;
shortest_matching_prefix(UCAddr, [_Prefix|Prefixes], SPrefix) ->
    shortest_matching_prefix(UCAddr, Prefixes, SPrefix).

masked_eq(<<M:32>>,
          <<A:32>>,
          <<PA:32>>) ->
    (A band M) =:= (PA band M);
masked_eq(<<M01:8,  M02:8,  M03:8,  M04:8,
            M05:8,  M06:8,  M07:8,  M08:8,
            M09:8,  M10:8,  M11:8,  M12:8,
            M13:8,  M14:8,  M15:8,  M16:8>>,
          <<A01:8,  A02:8,  A03:8,  A04:8,
            A05:8,  A06:8,  A07:8,  A08:8,
            A09:8,  A10:8,  A11:8,  A12:8,
            A13:8,  A14:8,  A15:8,  A16:8>>,
          <<PA01:8, PA02:8, PA03:8, PA04:8,
            PA05:8, PA06:8, PA07:8, PA08:8,
            PA09:8, PA10:8, PA11:8, PA12:8,
            PA13:8, PA14:8, PA15:8, PA16:8>>) ->
    ((A01 band M01) =:= (PA01 band M01)) andalso
    ((A02 band M02) =:= (PA02 band M02)) andalso
    ((A03 band M03) =:= (PA03 band M03)) andalso
    ((A04 band M04) =:= (PA04 band M04)) andalso
    ((A05 band M05) =:= (PA05 band M05)) andalso
    ((A06 band M06) =:= (PA06 band M06)) andalso
    ((A07 band M07) =:= (PA07 band M07)) andalso
    ((A08 band M08) =:= (PA08 band M08)) andalso
    ((A09 band M09) =:= (PA09 band M09)) andalso
    ((A10 band M10) =:= (PA10 band M10)) andalso
    ((A11 band M11) =:= (PA11 band M11)) andalso
    ((A12 band M12) =:= (PA12 band M12)) andalso
    ((A13 band M13) =:= (PA13 band M13)) andalso
    ((A14 band M14) =:= (PA14 band M14)) andalso
    ((A15 band M15) =:= (PA15 band M15)) andalso
    ((A16 band M16) =:= (PA16 band M16)).

ipv4_to_bin({A1, A2, A3, A4}) ->
    <<A1:8, A2:8, A3:8, A4:8>>.

ipv6_to_bin({A1, A2, A3, A4, A5, A6, A7, A8}) ->
    <<A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16, A8:16>>.

if_info_search(_Idx, []) ->
    false;
if_info_search(Idx, [#{index := Idx} = IfInfo|_IfInfos]) ->
    {value, IfInfo};
if_info_search(Idx, [_|IfInfos]) ->
    if_info_search(Idx, IfInfos).
    

win_getifaddrs_mask(#{addr   := #{addr := _Addr, family := inet = Fam},
                      length := Len}) ->
    <<M1:8, M2:8, M3:8, M4:8>> = <<(16#FFFFFFFF bsl (32 - Len)):32>>,
    #{addr   => {M1, M2, M3, M4},
      family => Fam,
      port   => 0};
win_getifaddrs_mask(#{addr   := #{addr := _Addr, family := inet6 = Fam},
                      length := Len}) ->
    <<M1:16, M2:16, M3:16, M4:16, M5:16, M6:16, M7:16, M8:16>> =
        <<(16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF bsl (128 - Len)):128>>,
    #{addr     => {M1, M2, M3, M4, M5, M6, M7, M8},
      family   => Fam,
      flowinfo => 0,
      port     => 0,
      scope_id => 0}.

win_getifaddrs_broadaddr(
  #{addr := {M1, M2, M3, M4}, family := Fam} = _Mask,
  #{addr := #{addr := {PA1, PA2, PA3, PA4}}} = _Prefix) ->
    BA1 = 16#FF band (PA1 bor (bnot M1)),
    BA2 = 16#FF band (PA2 bor (bnot M2)),
    BA3 = 16#FF band (PA3 bor (bnot M3)),
    BA4 = 16#FF band (PA4 bor (bnot M4)),
    #{family => Fam,
      addr   => {BA1, BA2, BA3, BA4},
      port   => 0}.
    

iat_broadaddr({A1, A2, A3, A4}, {M1, M2, M3, M4}) ->
    BA1 = 16#FF band (A1 bor (bnot M1)),
    BA2 = 16#FF band (A2 bor (bnot M2)),
    BA3 = 16#FF band (A3 bor (bnot M3)),
    BA4 = 16#FF band (A4 bor (bnot M4)),
    #{family => inet,
      addr   => {BA1, BA2, BA3, BA4},
      port   => 0}.    

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
if_name2index(Name) when is_list(Name) ->
    try prim_net:if_name2index(Name) of
        Result ->
            Result
    catch
        C : E : S when (C =:= error) andalso (E =:= notsup) ->
            %% This is *most likely* Windows, so try that.
            %% If not (another catch), raise the original catched error.
            try win_name2index(Name)
            catch
                _:_:_ ->
                    erlang:raise(C, E, S)
            end
    end.

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
    try prim_net:if_index2name(Idx) of
        Result ->
            Result
    catch
        C : E : S when (C =:= error) andalso (E =:= notsup) ->
            %% This is *most likely* Windows, so try that.
            %% If not (another catch), raise the original catched error.
            try win_index2name(Idx)
            catch
                _:_:_ ->
                    erlang:raise(C, E, S)
            end
    end.

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
    try prim_net:if_names() of
        Result ->
            Result
    catch
        C : E : S when (C =:= error) andalso (E =:= notsup) ->
            %% This is *most likely* Windows, so try that.
            %% If not (another catch), raise the original catched error.
            try {ok, win_names()}
            catch
                _:_:_ ->
                    erlang:raise(C, E, S)
            end
    end.

%% ===========================================================================
%%
%% -- Windows specific functions:
%%

win_names() ->
    [{Idx, win_name(Idx)} || Idx <- win_indexes()].

win_indexes() ->
    case prim_net:get_adapters_addresses(#{flags => ?GAA_INDEXES_FLAGS()}) of
        {ok, AA} ->
            Indexes = [Idx || #{index := Idx} <- AA],
            lists:sort(Indexes);
        {error, _} ->
            case prim_net:get_ip_address_table(#{}) of
                {ok, Tab} ->
                    Indexes = [Idx || #{index := Idx} <- Tab],
                    lists:sort(Indexes);
                {error, _} ->
                    throw({error, no_index})
            end
    end.

win_name(Idx) ->
    case prim_net:get_if_entry(#{index => Idx}) of
        {ok, #{name := Name}} ->
            Name;
        {error, _} ->
            throw({error, no_entry})
    end.
    

win_index2name(Idx) ->
    case lists:keysearch(Idx, 1, win_names()) of
        {value, {Idx, Name}} ->
            {ok, Name};
        false ->
            {error, enxio} % This is to be "compatible" with unix
    end.

win_name2index(Name) ->
    case lists:keysearch(Name, 2, win_names()) of
        {value, {Idx, Name}} ->
            {ok, Idx};
        false ->
            {error, enodev} % This is to be "compatible" with unix
    end.

%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

%% d(F) ->
%%     d(F, []).

%% d(F, A) ->
%%     io:format("~w:" ++ F ++ "~n", [?MODULE|A]).
