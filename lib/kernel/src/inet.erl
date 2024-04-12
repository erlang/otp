%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-module(inet).

-include("inet.hrl").
-include("inet_int.hrl").
-include("inet_sctp.hrl").

%% socket
-export([peername/1, sockname/1, port/1, send/2,
	 peernames/1, peernames/2, socknames/1, socknames/2,
	 setopts/2, getopts/2, 
	 getifaddrs/0, getifaddrs/1,
	 getif/1, getif/0, getiflist/0, getiflist/1,
	 ifget/3, ifget/2, ifset/3, ifset/2,
	 getstat/1, getstat/2,
         info/1, socket_to_list/1,
	 ip/1, is_ipv4_address/1, is_ipv6_address/1, is_ip_address/1,
	 stats/0, options/0, 
	 pushf/3, popf/1, close/1, gethostname/0, gethostname/1, 
	 parse_ipv4_address/1, parse_ipv6_address/1,
         parse_ipv4strict_address/1, parse_ipv6strict_address/1,
         parse_address/1, parse_strict_address/1,
         parse_address/2, parse_strict_address/2,
         ntoa/1, ipv4_mapped_ipv6_address/1]).

-export([connect_options/2, listen_options/2, udp_options/2, sctp_options/2]).
-export([udp_module/1, udp_module/2,
         tcp_module/1, tcp_module/2,
         sctp_module/1]).
-export([gen_tcp_module/1, gen_udp_module/1]).

-export([i/0, i/1, i/2]).

-export([getll/1, getfd/1, open/8, open_bind/8, fdopen/6]).

-export([tcp_controlling_process/2, udp_controlling_process/2,
	 tcp_close/1, udp_close/1]).

%% used by sendfile
-export([lock_socket/2]).

%% used by socks5
-export([setsockname/2, setpeername/2]).

%% resolve
-export([gethostbyname/1, gethostbyname/2, gethostbyname/3, 
	 gethostbyname_tm/3]).
-export([gethostbyname_string/2, gethostbyname_self/2]).
-export([gethostbyaddr/1, gethostbyaddr/2, 
	 gethostbyaddr_tm/2]).

-export([getservbyname/2, getservbyport/2]).
-export([getaddrs/2, getaddrs/3, getaddrs_tm/3,
	 getaddr/2, getaddr/3, getaddr_tm/3]).
-export([translate_ip/2]).

-export([get_rc/0]).

%% format error
-export([format_error/1]).

%% timer interface
-export([start_timer/1, timeout/1, timeout/2, stop_timer/1]).

%% Socket monitoring
-export([monitor/1, cancel_monitor/1]).

%% Socket utility functions
-export([ensure_sockaddr/1]).

-export_type([address_family/0, socket_protocol/0, hostent/0, hostname/0, ip4_address/0,
              ip6_address/0, ip_address/0, port_number/0,
	      family_address/0, local_address/0,
              socket_address/0, returned_non_ip_address/0,
	      socket_setopt/0, socket_getopt/0, socket_optval/0,
              ancillary_data/0,
	      posix/0, socket/0, inet_backend/0, stat_option/0]).
%% imports
-import(lists, [append/1, duplicate/2, filter/2, foldl/3]).

-define(DEFAULT_KERNEL_INET_BACKEND, inet). % inet_backend()

%% Record Signature
-define(RS(Record),
	{Record, record_info(size, Record)}).
%% Record Signature Check (guard)
-define(RSC(Record, RS),
	element(1, Record) =:= element(1, RS),
	tuple_size(Record) =:= element(2, RS)).

%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).


%%% ---------------------------------
%%% Contract type definitions


-type hostent() :: #hostent{}.
-type hostname() :: atom() | string().
-type ip4_address() :: {0..255,0..255,0..255,0..255}.
-type ip6_address() :: {0..65535,0..65535,0..65535,0..65535,
			0..65535,0..65535,0..65535,0..65535}.
-type ip_address() :: ip4_address() | ip6_address().
-type port_number() :: 0..65535.
-type family_address() :: inet_address() | inet6_address() | local_address().
-type inet_address() ::
        {'inet', {ip4_address() | 'any' | 'loopback', port_number()}}.
-type inet6_address() ::
        {'inet6', {ip6_address() | 'any' | 'loopback', port_number()}}.
-type local_address() :: {'local', File :: binary() | string()}.
-type returned_non_ip_address() ::
	{'local', binary()} |
	{'unspec', <<>>} |
	{'undefined', any()}.
-type posix() ::
        'eaddrinuse' | 'eaddrnotavail' | 'eafnosupport' | 'ealready' |
        'econnaborted' | 'econnrefused' | 'econnreset' |
        'edestaddrreq' |
        'ehostdown' | 'ehostunreach' |
        'einprogress' | 'eisconn' |
        'emsgsize' |
        'enetdown' | 'enetunreach' |
        'enopkg' | 'enoprotoopt' | 'enotconn' | 'enotty' | 'enotsock' |
        'eproto' | 'eprotonosupport' | 'eprototype' |
        'esocktnosupport' |
        'etimedout' |
        'ewouldblock' |
        'exbadport' | 'exbadseq' | file:posix().
-type module_socket() :: {'$inet', Handler :: module(), Handle :: term()}.
-define(module_socket(Handler, Handle), {'$inet', (Handler), (Handle)}).
-type socket() :: port() | module_socket().
-type inet_backend() :: {'inet_backend', 'inet' | 'socket'}.

-type socket_setopt() ::
        gen_sctp:option() | gen_tcp:option() | gen_udp:option().

-type socket_optval() ::
        gen_sctp:option_value() | gen_tcp:option() | gen_udp:option() |
        gen_tcp:pktoptions_value().

-type socket_getopt() ::
        gen_sctp:option_name() | gen_tcp:option_name() | gen_udp:option_name().

-type ether_address() :: [0..255].

-type if_setopt() ::
      {'addr', ip_address()} |
      {'broadaddr', ip_address()} |
      {'dstaddr', ip_address()} |
      {'mtu', non_neg_integer()} |
      {'netmask', ip_address()} |
      {'flags', ['up' | 'down' | 'broadcast' | 'no_broadcast' |
		 'pointtopoint' | 'no_pointtopoint' | 
		 'running' | 'multicast']} |
      {'hwaddr', ether_address()}.

-type if_getopt() ::
      'addr' | 'broadaddr' | 'dstaddr' | 
      'mtu' | 'netmask' | 'flags' |'hwaddr'.

-type if_getopt_result() ::
      {'addr', ip_address()} |
      {'broadaddr', ip_address()} |
      {'dstaddr', ip_address()} |
      {'mtu', non_neg_integer()} |
      {'netmask', ip_address()} |
      {'flags', ['up' | 'down' | 'broadcast' | 'no_broadcast' |
		 'pointtopoint' | 'no_pointtopoint' |
		 'running' | 'multicast' | 'loopback']} |
      {'hwaddr', ether_address()}.

-type getifaddrs_ifopts() ::
        [Ifopt :: {flags, Flags :: [up | broadcast | loopback |
                                    pointtopoint | running | multicast]} |
                  {addr, Addr :: ip_address()} |
                  {netmask, Netmask :: ip_address()} |
                  {broadaddr, Broadaddr :: ip_address()} |
                  {dstaddr, Dstaddr :: ip_address()} |
                  {hwaddr, Hwaddr :: [byte()]}].

-type address_family() :: 'inet' | 'inet6' | 'local'.
-type socket_protocol() :: 'tcp' | 'udp' | 'sctp'.
-type socket_type() :: 'stream' | 'dgram' | 'seqpacket'.
-type socket_address() ::
	ip_address() | 'any' | 'loopback' | local_address().
-type stat_option() :: 
	'recv_cnt' | 'recv_max' | 'recv_avg' | 'recv_oct' | 'recv_dvi' |
	'send_cnt' | 'send_max' | 'send_avg' | 'send_oct' | 'send_pend'.

-type ancillary_data() ::
        [ {'tos', byte()} | {'tclass', byte()} | {'ttl', byte()} ].

%%% ---------------------------------

-spec get_rc() -> [{Par :: atom(), Val :: any()} |
                   {Par :: atom(), Val1 :: any(), Val2 :: any()}].

get_rc() ->
    inet_db:get_rc().

-spec close(Socket) -> 'ok' when
      Socket :: socket().

close(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
close(Socket) ->
    prim_inet:close(Socket),
    receive
	{Closed, Socket} when Closed =:= tcp_closed; Closed =:= udp_closed ->
	    ok
    after 0 ->
	    ok
    end.


%% -- Socket monitor

-spec monitor(Socket) -> reference() when
      Socket :: socket().

monitor({'$inet', GenSocketMod, _} = Socket) when is_atom(GenSocketMod) ->
    MRef = GenSocketMod:?FUNCTION_NAME(Socket),
    case inet_db:put_socket_type(MRef, {socket, GenSocketMod}) of
        ok ->
            MRef;
	error ->
	    GenSocketMod:cancel_monitor(MRef),
	    erlang:error({invalid, Socket})
    end;
monitor(Socket) when is_port(Socket) ->
    MRef = erlang:monitor(port, Socket),
    case inet_db:put_socket_type(MRef, port) of
	ok ->
	    MRef;
	error ->
	    erlang:demonitor(MRef, [flush]),
	    erlang:error({invalid, Socket})
    end;
monitor(Socket) ->
    erlang:error(badarg, [Socket]).


%% -- Cancel socket monitor

-spec cancel_monitor(MRef) -> boolean() when
      MRef :: reference().

cancel_monitor(MRef) when is_reference(MRef) ->
    case inet_db:take_socket_type(MRef) of
	{ok, port} ->
	    erlang:demonitor(MRef, [info]);
	{ok, {socket, GenSocketMod}} ->
	    GenSocketMod:?FUNCTION_NAME(MRef);
	error -> % Assume it has the monitor has already been cancel'ed
	    false
    end;
cancel_monitor(MRef) ->
    erlang:error(badarg, [MRef]).


%% -- Socket peername

-spec peername(Socket :: socket()) ->
		      {ok,
		       {ip_address(), port_number()} |
		       returned_non_ip_address()} |
		      {error, posix()}.

peername(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
peername(Socket) -> 
    prim_inet:peername(Socket).

-spec setpeername(
	Socket :: socket(),
	Address ::
	  {ip_address() | 'any' | 'loopback',
	   port_number()} |
	  socket_address()) ->
			 'ok' | {'error', any()}.

setpeername(Socket, {IP,Port}) ->
    prim_inet:setpeername(Socket, {IP,Port});
setpeername(Socket, undefined) ->
    prim_inet:setpeername(Socket, undefined).

-spec peernames(Socket :: socket()) ->
		       {ok,
			[{ip_address(), port_number()} |
			 returned_non_ip_address()]} |
		       {error, posix()}.

peernames(Socket) ->
    prim_inet:peernames(Socket).

-spec peernames(Socket, Assoc) ->
		       {ok, [{Address, Port}]} | {error, posix()} when
      Socket :: socket(),
      Assoc :: #sctp_assoc_change{} | gen_sctp:assoc_id(),
      Address :: ip_address(),
      Port :: non_neg_integer().

peernames(Socket, Assoc) ->
    prim_inet:peernames(Socket, Assoc).


-spec sockname(Socket :: socket()) ->
		      {ok,
		       {ip_address(), port_number()} |
		       returned_non_ip_address()} |
		      {error, posix()}.

sockname(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
sockname(Socket) -> 
    prim_inet:sockname(Socket).

-spec setsockname(
	Socket :: socket(),
	Address ::
	  {ip_address() | 'any' | 'loopback',
	   port_number()} |
	  socket_address()) ->
	'ok' | {'error', any()}.

setsockname(Socket, {IP,Port}) -> 
    prim_inet:setsockname(Socket, {IP,Port});
setsockname(Socket, undefined) ->
    prim_inet:setsockname(Socket, undefined).

-spec socknames(Socket :: socket()) ->
		       {ok,
			[{ip_address(), port_number()} |
			 returned_non_ip_address()]} |
		       {error, posix()}.

socknames(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
socknames(Socket) ->
    prim_inet:socknames(Socket).

-spec socknames(Socket, Assoc) ->
		       {ok, [{Address, Port}]} | {error, posix()} when
      Socket :: socket(),
      Assoc :: #sctp_assoc_change{} | gen_sctp:assoc_id(),
      Address :: ip_address(),
      Port :: non_neg_integer().

socknames(Socket, Assoc) ->
    prim_inet:socknames(Socket, Assoc).


-spec port(Socket) -> {'ok', Port} | {'error', any()} when
      Socket :: socket(),
      Port :: port_number().

port(?module_socket(GenSocketMod, _) = Socket) when is_atom(GenSocketMod) ->
    case GenSocketMod:sockname(Socket) of
        {ok, {_, Port}} -> {ok, Port};
        {error, _} = Error -> Error
    end;
port(Socket) ->
    case prim_inet:sockname(Socket) of
	{ok, {_,Port}} -> {ok, Port};
	Error -> Error
    end.

-spec send(Socket :: socket(), Packet :: iolist()) -> % iolist()?
	'ok' | {'error', posix()}.

send(Socket, Packet) -> 
    prim_inet:send(Socket, Packet).
    
-spec setopts(Socket, Options) -> ok | {error, posix()} when
      Socket :: socket(),
      Options :: [socket_setopt()].

setopts(?module_socket(GenSocketMod, _) = Socket, Opts) when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, Opts);
setopts(Socket, Opts) -> 
    SocketOpts =
	[case Opt of
	     {netns,NS} ->
		 {netns,filename2binary(NS)};
	     _ ->
		 Opt
	 end || Opt <- Opts],
    prim_inet:setopts(Socket, SocketOpts).

-spec getopts(Socket, Options) ->
	{'ok', OptionValues} | {'error', posix()} when
      Socket :: socket(),
      Options :: [socket_getopt()],
      OptionValues :: [socket_optval()].

getopts(?module_socket(GenSocketMod, _) = Socket, Opts)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, Opts);
getopts(Socket, Opts) ->
    case prim_inet:getopts(Socket, Opts) of
	{ok,OptionValues} ->
	    {ok,
	     [case OptionValue of
		  {netns,Bin} ->
		      {netns,binary2filename(Bin)};
		  _ ->
		      OptionValue
	      end || OptionValue <- OptionValues]};
	Other ->
	    Other
    end.

-spec getifaddrs(
        [Option :: {netns, Namespace :: file:filename_all()}]
        | socket()) ->
                        {'ok', [{Ifname :: string(),
                                 Ifopts :: getifaddrs_ifopts()}]}
                            | {'error', posix()}.
getifaddrs(Opts) when is_list(Opts) ->
    withsocket(fun(S) -> prim_inet:getifaddrs(S) end, Opts);
getifaddrs(Socket) ->
    prim_inet:getifaddrs(Socket).

-spec getifaddrs() ->
                        {'ok', [{Ifname :: string(),
                                 Ifopts :: getifaddrs_ifopts()}]}
                            | {'error', posix()}.
getifaddrs() ->
    withsocket(fun(S) -> prim_inet:getifaddrs(S) end).


-spec getiflist(
        [Option :: {netns, Namespace :: file:filename_all()}]
        | socket()) ->
                       {'ok', [string()]} | {'error', posix()}.

getiflist(Opts) when is_list(Opts) ->
    withsocket(fun(S) -> prim_inet:getiflist(S) end, Opts);
getiflist(Socket) -> 
    prim_inet:getiflist(Socket).

-spec getiflist() -> {'ok', [string()]} | {'error', posix()}.

getiflist() -> 
    withsocket(fun(S) -> prim_inet:getiflist(S) end).
    
-spec ifget(Socket :: socket(),
            Name :: string() | atom(),
	    Opts :: [if_getopt()]) ->
	{'ok', [if_getopt_result()]} | {'error', posix()}.

ifget(Socket, Name, Opts) -> 
    prim_inet:ifget(Socket, Name, Opts).

-spec ifget(
        Name :: string() | atom(),
        Opts :: [if_getopt() |
                 {netns, Namespace :: file:filename_all()}]) ->
	{'ok', [if_getopt_result()]} | {'error', posix()}.

ifget(Name, Opts) ->
    {NSOpts,IFOpts} =
        lists:partition(
          fun ({netns,_}) -> true;
              (_) -> false
          end, Opts),
    withsocket(fun(S) -> prim_inet:ifget(S, Name, IFOpts) end, NSOpts).

-spec ifset(Socket :: socket(),
            Name :: string() | atom(),
	    Opts :: [if_setopt()]) ->
	'ok' | {'error', posix()}.

ifset(Socket, Name, Opts) -> 
    prim_inet:ifset(Socket, Name, Opts).

-spec ifset(
        Name :: string() | atom(),
        Opts :: [if_setopt() |
                 {netns, Namespace :: file:filename_all()}]) ->
	'ok' | {'error', posix()}.

ifset(Name, Opts) ->
    {NSOpts,IFOpts} =
        lists:partition(
          fun ({netns,_}) -> true;
              (_) -> false
          end, Opts),
    withsocket(fun(S) -> prim_inet:ifset(S, Name, IFOpts) end, NSOpts).

-spec getif() ->
	{'ok', [{ip_address(), ip_address() | 'undefined', ip_address()}]} | 
	{'error', posix()}.

getif() -> 
    withsocket(fun(S) -> getif(S) end).

%% backwards compatible getif
-spec getif(
        [Option :: {netns, Namespace :: file:filename_all()}]
        | socket()) ->
	{'ok', [{ip_address(), ip_address() | 'undefined', ip_address()}]} | 
	{'error', posix()}.

getif(Opts) when is_list(Opts) ->
    withsocket(fun(S) -> getif(S) end, Opts);
getif(Socket) ->
    case prim_inet:getiflist(Socket) of
	{ok, IfList} ->
	    {ok, lists:foldl(
		   fun(Name,Acc) ->
			   case prim_inet:ifget(Socket,Name,
						[addr,broadaddr,netmask]) of
			       {ok,[{addr,A},{broadaddr,B},{netmask,M}]} ->
				   [{A,B,M}|Acc];
			       %% Some interfaces does not have a b-addr
			       {ok,[{addr,A},{netmask,M}]} ->
				   [{A,undefined,M}|Acc];
			       _ ->
				   Acc
			   end
		   end, [], IfList)};
	Error -> Error
    end.

withsocket(Fun) ->
    withsocket(Fun, []).
%%
withsocket(Fun, Opts) ->
    case inet_udp:open(0, Opts) of
	{ok,Socket} ->
	    Res = Fun(Socket),
	    inet_udp:close(Socket),
	    Res;
	Error ->
	    Error
    end.

pushf(_Socket, Fun, _State) when is_function(Fun) ->
    {error, einval}.

popf(_Socket) ->
    {error, einval}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the hostname is not cached any more because this
% could cause troubles on at least windows with plug-and-play
% and network-cards inserted and removed in conjunction with
% use of the DHCP-protocol
% should never fail

-spec gethostname() -> {'ok', Hostname} when
      Hostname :: string().

%%% XXX gethostname() -> net:gethostname().

gethostname() ->
    case inet_udp:open(0,[]) of
	{ok,U} ->
	    {ok,Res} = gethostname(U),
	    inet_udp:close(U),
	    {Res2,_} = lists:splitwith(fun($.)->false;(_)->true end,Res),
	    {ok, Res2};
	_ ->
	    {ok, "nohost.nodomain"}
    end.

-spec gethostname(Socket :: socket()) ->
	{'ok', string()} | {'error', posix()}.

gethostname(Socket) ->
    prim_inet:gethostname(Socket).

-spec getstat(Socket) ->
	{ok, OptionValues} | {error, posix()} when
      Socket :: socket(),
      OptionValues :: [{stat_option(), integer()}].

getstat(Socket) ->
    getstat(Socket, stats()).

-spec getstat(Socket, Options) ->
	{ok, OptionValues} | {error, posix()} when
      Socket :: socket(),
      Options :: [stat_option()],
      OptionValues :: [{stat_option(), integer()}].

getstat(?module_socket(GenSocketMod, _) = Socket, What)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket, What);
getstat(Socket, What) ->
    prim_inet:getstat(Socket, What).

-spec gethostbyname(Hostname) -> {ok, Hostent} | {error, posix()} when
      Hostname :: hostname(),
      Hostent :: hostent().

gethostbyname(Name) -> 
    case inet_db:res_option(inet6) of
	true ->
	    gethostbyname_tm(Name, inet6, false);
	false ->
	    gethostbyname_tm(Name, inet, false)
    end.

-spec gethostbyname(Hostname, Family) ->
                           {ok, Hostent} | {error, posix()} when
      Hostname :: hostname(),
      Family :: address_family(),
      Hostent :: hostent().

gethostbyname(Name,Family) -> 
    gethostbyname_tm(Name, Family, false).

-spec gethostbyname(Name :: hostname(),
	            Family :: address_family(),
	            Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', #hostent{}} | {'error', posix()}.
	
gethostbyname(Name,Family,Timeout) ->
    Timer = start_timer(Timeout),
    Res = gethostbyname_tm(Name,Family,Timer),
    _ = stop_timer(Timer),
    Res.

gethostbyname_tm(Name, Family, Timer) ->
    %% ?DBG([{name, Name}, {family, Family}, {timer, Timer}]),
    Opts0 = inet_db:res_option(lookup),
    %% ?DBG([{opts0, Opts0}]),
    Opts =
	case (lists:member(native, Opts0) orelse
	      lists:member(string, Opts0) orelse
	      lists:member(nostring, Opts0)) of
	    true ->
		Opts0;
	    false ->
		[string|Opts0]
	end,
    %% ?DBG([{opts, Opts}]),
    gethostbyname_tm(Name, Family, Timer, Opts).


-spec gethostbyaddr(Address) -> {ok, Hostent} | {error, posix()} when
      Address :: string() | ip_address(),
      Hostent :: hostent().

gethostbyaddr(Address) ->
    gethostbyaddr_tm(Address, false).

-spec gethostbyaddr(Address :: string() | ip_address(), 
	            Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', #hostent{}} | {'error', posix()}.

gethostbyaddr(Address,Timeout) ->
    Timer = start_timer(Timeout),    
    Res = gethostbyaddr_tm(Address, Timer),
    _ = stop_timer(Timer),
    Res.

gethostbyaddr_tm(Address,Timer) ->
    gethostbyaddr_tm(Address, Timer, inet_db:res_option(lookup)).


-spec socket_to_list(Socket) -> list() when
      Socket :: socket().

socket_to_list({'$inet', GenSocketMod, _} = Socket)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
socket_to_list(Socket) when is_port(Socket) ->
    erlang:port_to_list(Socket).



-spec info(Socket) -> Info when
      Socket :: socket(),
      Info :: term().

info({'$inet', GenSocketMod, _} = Socket)
  when is_atom(GenSocketMod) ->
    GenSocketMod:?FUNCTION_NAME(Socket);
info(Socket) when is_port(Socket) ->
    case port_info(Socket) of
	#{states := _} = PortInfo ->
            case inet:getopts(Socket, [active]) of
                {ok, [{active, Active}]} ->
                    PortInfo#{active => Active};
                _ ->
                    PortInfo
            end;
	PortInfo0 ->
	    %% Its actually possible to call this function for non-socket ports,
	    %% but in that case we have no status or statistics.
	    PortInfo1 =
		case prim_inet:getstatus(Socket) of
		    {ok, State} ->
			PortInfo0#{states => State};
		    _ ->
			PortInfo0
		end,
	    case getstat(Socket) of
		{ok, Stats0} ->
		    PortInfo1#{counters => maps:from_list(Stats0)};
		_ ->
		    PortInfo1
	    end
    end.

port_info(P) when is_port(P) ->
    case erlang:port_info(P) of
	PI0 when is_list(PI0) ->
	    PI1 = port_info(PI0, [connected, links, input, output]) ++
		[erlang:port_info(P, memory), erlang:port_info(P, monitors)],
	    PI2 = pi_replace([{connected, owner}], PI1),
	    maps:from_list(PI2);
	_ ->
	    #{states => [closed]}
    end.

port_info(PI, Items) when is_list(PI) ->
    port_info(PI, Items, []).

port_info(_PI, [], Acc) ->
    Acc;
port_info(PI, [Item | Items], Acc) ->
    Val = proplists:get_value(Item, PI),
    port_info(PI, Items, [{Item, Val} | Acc]).

pi_replace([], Items) ->
    Items;
pi_replace([{Key1, Key2}|Keys], Items) ->
    case lists:keysearch(Key1, 1, Items) of
        {value, {Key1, Value}} ->
            Items2 = lists:keyreplace(Key1, 1, Items, {Key2, Value}),
            pi_replace(Keys, Items2);
        false ->
            pi_replace(Keys, Items)
    end.

-spec ip(Ip :: ip_address() | string() | atom()) ->
	{'ok', ip_address()} | {'error', posix()}.

ip({A,B,C,D}) when ?ip(A,B,C,D) ->
    {ok, {A,B,C,D}};
ip(Name) ->
    case gethostbyname(Name) of
	{ok, Ent} ->
	    {ok, hd(Ent#hostent.h_addr_list)};
	Error -> Error
    end.

-spec is_ipv4_address(IPv4Address) -> boolean() when
      IPv4Address :: ip4_address() | term().
is_ipv4_address({A,B,C,D}) when ?ip(A,B,C,D) ->
    true;
is_ipv4_address(_) ->
    false.

-spec is_ipv6_address(IPv6Address) -> boolean() when
      IPv6Address :: ip6_address() | term().
is_ipv6_address({A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    true;
is_ipv6_address(_) ->
    false.

-spec is_ip_address(IPAddress) -> boolean() when
      IPAddress :: ip_address() | term().
is_ip_address(Address) ->
    is_ipv4_address(Address) orelse is_ipv6_address(Address).

%% This function returns the erlang port used (with inet_drv)

-spec getll(Socket :: socket()) -> {'ok', socket()}.

getll(Socket) when is_port(Socket) ->
    {ok, Socket}.

%%
%% Return the internal file descriptor number
%%

-spec getfd(Socket :: socket()) ->
	{'ok', non_neg_integer()} | {'error', posix()}.

getfd(Socket) ->
    prim_inet:getfd(Socket).

%%
%% Lookup an ip address
%%

-spec getaddr(Host, Family) -> {ok, Address} | {error, posix()} when
      Host :: ip_address() | hostname(),
      Family :: address_family(),
      Address :: ip_address().

getaddr(Address, Family) ->
    getaddr(Address, Family, infinity).

-spec getaddr(Host :: ip_address() | hostname(),
	      Family :: address_family(),
	      Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', ip_address()} | {'error', posix()}.

getaddr(Address, Family, Timeout) ->
    %% ?DBG([{address, Address}, {family, Family}, {timeout, Timeout}]),
    Timer = start_timer(Timeout),
    Res   = getaddr_tm(Address, Family, Timer),
    %% ?DBG([{res, Res}]),
    _     = stop_timer(Timer),
    Res.

getaddr_tm(Address, Family, Timer) ->
    %% ?DBG([{address, Address}, {family, Family}, {timer, Timer}]),
    case getaddrs_tm(Address, Family, Timer) of
	{ok, [IP|_]} ->
	    %% ?DBG([{ip, IP}]),
	    {ok, IP};
	Error ->
	    %% ?DBG([{error, Error}]),
	    Error
    end.

-spec getaddrs(Host, Family) ->
	{ok, Addresses} | {error, posix()} when
      Host :: ip_address() | hostname(),
      Family :: address_family(),
      Addresses :: [ip_address()].

getaddrs(Address, Family) -> 
    getaddrs(Address, Family, infinity).

-spec getaddrs(Host :: ip_address() | string() | atom(),
	       Family :: address_family(),
	       Timeout :: non_neg_integer() | 'infinity') ->
	{'ok', [ip_address()]} | {'error', posix()}.

getaddrs(Address, Family, Timeout) ->
    Timer = start_timer(Timeout),
    Res = getaddrs_tm(Address, Family, Timer),
    _ = stop_timer(Timer),
    Res.

-spec getservbyport(Port :: port_number(), Protocol :: atom() | string()) ->
	{'ok', string()} | {'error', posix()}.

getservbyport(Port, Proto) ->
    case inet_udp:open(0, []) of
	{ok,U} ->
	    Res = prim_inet:getservbyport(U, Port, Proto),
	    inet_udp:close(U),
	    Res;
	Error -> Error
    end.

-spec getservbyname(Name :: atom() | string(),
	            Protocol :: atom() | string()) ->
	{'ok', port_number()} | {'error', posix()}.

getservbyname(Name, Protocol) when is_atom(Name) ->
    case inet_udp:open(0, []) of
	{ok,U} ->
	    Res = prim_inet:getservbyname(U, Name, Protocol),
	    inet_udp:close(U),
	    Res;
	Error -> Error
    end.

-spec ntoa(IpAddress) -> Address | {error, einval} when
      Address :: string(),
      IpAddress :: ip_address().
ntoa(Addr) ->
    inet_parse:ntoa(Addr).

-spec parse_ipv4_address(Address) ->
	{ok, IPv4Address} | {error, einval} when
      Address :: string(),
      IPv4Address :: ip4_address().
parse_ipv4_address(Addr) ->
    inet_parse:ipv4_address(Addr).

-spec parse_ipv6_address(Address) ->
	{ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_ipv6_address(Addr) ->
    inet_parse:ipv6_address(Addr).

-spec parse_ipv4strict_address(Address) ->
	{ok, IPv4Address} | {error, einval} when
      Address :: string(),
      IPv4Address :: ip4_address().
parse_ipv4strict_address(Addr) ->
    inet_parse:ipv4strict_address(Addr).

-spec parse_ipv6strict_address(Address) ->
	{ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_ipv6strict_address(Addr) ->
    inet_parse:ipv6strict_address(Addr).

-spec parse_address(Address) ->
	{ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address().
parse_address(Addr) ->
    inet_parse:address(Addr).

-spec parse_address(Address, inet) ->
          {ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address();
                   (Address, inet6) ->
          {ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_address(Addr, inet) ->
    inet_parse:ipv4_address(Addr);
parse_address(Addr, inet6) ->
    inet_parse:ipv6_address(Addr).

-spec parse_strict_address(Address) ->
	{ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address().
parse_strict_address(Addr) ->
    inet_parse:strict_address(Addr).

-spec parse_strict_address(Address, inet) ->
          {ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address();
                   (Address, inet6) ->
          {ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip6_address().
parse_strict_address(Addr, inet) ->
    inet_parse:ipv4strict_address(Addr);
parse_strict_address(Addr, inet6) ->
    inet_parse:ipv6strict_address(Addr).

-spec ipv4_mapped_ipv6_address(ip_address()) -> ip_address().
ipv4_mapped_ipv6_address({D1,D2,D3,D4})
  when (D1 bor D2 bor D3 bor D4) < 256 ->
    {0,0,0,0,0,16#ffff,(D1 bsl 8) bor D2,(D3 bsl 8) bor D4};
ipv4_mapped_ipv6_address({D1,D2,D3,D4,D5,D6,D7,D8})
  when (D1 bor D2 bor D3 bor D4 bor D5 bor D6 bor D7 bor D8) < 65536 ->
    {D7 bsr 8,D7 band 255,D8 bsr 8,D8 band 255}.

%% Return a list of available options
options() ->
    [
     tos, tclass, priority, reuseaddr, keepalive, dontroute, linger,
     broadcast, sndbuf, recbuf, nodelay, ipv6_v6only,
     buffer, header, active, packet, deliver, mode,
     multicast_if, multicast_ttl, multicast_loop,
     exit_on_close, high_watermark, low_watermark,
     high_msgq_watermark, low_msgq_watermark,
     send_timeout, send_timeout_close, show_econnreset
    ].

%% Return a list of statistics options

-spec stats() -> [stat_option(),...].

stats() ->
    [recv_oct, recv_cnt, recv_max, recv_avg, recv_dvi,
     send_oct, send_cnt, send_max, send_avg, send_pend].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:connect
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_options() ->
    [debug,
     tos, tclass, priority, reuseaddr, reuseport, reuseport_lb,
     exclusiveaddruse, keepalive,
     linger, nodelay, sndbuf, recbuf, recvtos, recvtclass, ttl, recvttl,
     header, active, packet, packet_size, buffer, mode, deliver, line_delimiter,
     exit_on_close, high_watermark, low_watermark, high_msgq_watermark,
     low_msgq_watermark, send_timeout, send_timeout_close, delay_send, raw,
     show_econnreset, bind_to_device].
    
connect_options(Opts, Mod) ->
    BaseOpts = 
	case application:get_env(kernel, inet_default_connect_options) of
	    {ok, List} when is_list(List) ->
		NList = [{active, true} | lists:keydelete(active,1,List)],     
		#connect_opts{opts = NList};
	    {ok, {active,_Bool}} -> 
		#connect_opts{opts = [{active,true}]};
	    {ok, Option} -> 
		#connect_opts{opts = [{active,true}, Option]};
	    _ ->
		#connect_opts{opts = [{active,true}]}
	end,
    case con_opt(Opts, BaseOpts, connect_options()) of
	{ok, R} ->
	    {ok, R#connect_opts {
		   opts = lists:reverse(R#connect_opts.opts),
		   ifaddr = Mod:translate_ip(R#connect_opts.ifaddr)
		  }};
	Error -> Error	    
    end.

con_opt([{raw,A,B,C}|Opts],#connect_opts{} = R,As) ->
    con_opt([{raw,{A,B,C}}|Opts],R,As);
con_opt([Opt | Opts], #connect_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
	{ifaddr, Addr} when is_map(Addr) ->
            con_opt(Opts, R#connect_opts{ ifaddr = ensure_sockaddr(Addr) }, As);
	{ifaddr, Addr} ->
            con_opt(Opts, R#connect_opts{ ifaddr = Addr }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{ip,IP} when is_map(IfAddr) ->
            con_opt(Opts, R#connect_opts{ ifaddr = IfAddr#{addr => IP} }, As);
	{ip,IP}     -> con_opt(Opts, R#connect_opts{ ifaddr = IP }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{port,P} when is_map(IfAddr) ->
            con_opt(Opts, R#connect_opts{ ifaddr = IfAddr#{port => P} }, As);
	{port,P}    -> con_opt(Opts, R#connect_opts{ port = P }, As);

	{fd,Fd}     -> con_opt(Opts, R#connect_opts{ fd = Fd }, As);

	binary      -> con_add(mode, binary, R, Opts, As);

	list        -> con_add(mode, list, R, Opts, As);

	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    con_opt(Opts, R#connect_opts { fd = [{netns,BinNS}] }, As);
		false ->
		    {error, badarg}
	    end;

        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            NOpts = lists:keydelete(active, 1, R#connect_opts.opts),
            con_opt(Opts, R#connect_opts { opts = [{active,N}|NOpts] }, As);

	{line_delimiter,C} when is_integer(C), C >= 0, C =< 255 ->
	    con_add(line_delimiter, C, R, Opts, As);

	{Name,Val} when is_atom(Name) -> con_add(Name, Val, R, Opts, As);

	_ -> {error, badarg}
    end;
con_opt([], #connect_opts{} = R, _) ->
    {ok, R}.

con_add(Name, Val, #connect_opts{} = R, Opts, AllOpts) ->
    case add_opt(Name, Val, R#connect_opts.opts, AllOpts) of
	{ok, SOpts} ->
	    con_opt(Opts, R#connect_opts { opts = SOpts }, AllOpts);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:listen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen_options() ->
    [debug,
     tos, tclass,
     priority, reuseaddr, reuseport, reuseport_lb, exclusiveaddruse, keepalive,
     linger, sndbuf, recbuf, nodelay, recvtos, recvtclass, ttl, recvttl,
     header, active, packet, buffer, mode, deliver, backlog, ipv6_v6only,
     exit_on_close, high_watermark, low_watermark, high_msgq_watermark,
     low_msgq_watermark, send_timeout, send_timeout_close, delay_send,
     packet_size, raw, show_econnreset, bind_to_device].

listen_options(Opts, Mod) ->
    BaseOpts = 
	case application:get_env(kernel, inet_default_listen_options) of
	    {ok,List} when is_list(List) ->
		NList = [{active, true} | lists:keydelete(active,1,List)],		       
		#listen_opts{ opts = NList};
	    {ok,{active,_Bool}} -> 
		#listen_opts{ opts = [{active,true}]};
	    {ok,Option} -> 
		#listen_opts{ opts = [{active,true}, Option]};
	    _ ->
		#listen_opts{ opts = [{active,true}]}
	end,
    case list_opt(Opts, BaseOpts, listen_options()) of
	{ok, R} ->
	    {ok, R#listen_opts {
		   opts = lists:reverse(R#listen_opts.opts),
		   ifaddr = Mod:translate_ip(R#listen_opts.ifaddr)
		  }};
	Error -> Error
    end.
	
list_opt([{raw,A,B,C}|Opts], #listen_opts{} = R, As) ->
    list_opt([{raw,{A,B,C}}|Opts], R, As);
list_opt([Opt | Opts], #listen_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
	{ifaddr, Addr} when is_map(Addr) ->
            list_opt(Opts, R#listen_opts{ ifaddr = ensure_sockaddr(Addr) }, As);
	{ifaddr, Addr} ->
            list_opt(Opts, R#listen_opts{ ifaddr = Addr }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{ip,IP} when is_map(IfAddr) ->
            list_opt(Opts, R#listen_opts{ ifaddr = IfAddr#{addr => IP} }, As);
	{ip,IP}      ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);

        %% This is when a previous value of ifaddr was a sockaddr_in6()
	{port,P} when is_map(IfAddr) ->
            list_opt(Opts, R#listen_opts{ ifaddr = IfAddr#{port => P} }, As);
	{port,P}     ->  list_opt(Opts, R#listen_opts { port = P }, As);

	{fd,Fd}      ->  list_opt(Opts, R#listen_opts { fd = Fd }, As);
	{backlog,BL} ->  list_opt(Opts, R#listen_opts { backlog = BL }, As);
	binary       ->  list_add(mode, binary, R, Opts, As);
	list         ->  list_add(mode, list, R, Opts, As);
	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    list_opt(Opts, R#listen_opts { fd = [{netns,BinNS}] }, As);
		false ->
		    {error, badarg}
	    end;
        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            NOpts = lists:keydelete(active, 1, R#listen_opts.opts),
            list_opt(Opts, R#listen_opts { opts = [{active,N}|NOpts] }, As);
	{Name,Val} when is_atom(Name) -> list_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
list_opt([], #listen_opts{} = R, _SockOpts) ->
    {ok, R}.

list_add(Name, Val, #listen_opts{} = R, Opts, As) ->
    case add_opt(Name, Val, R#listen_opts.opts, As) of
	{ok, SOpts} ->
	    list_opt(Opts, R#listen_opts { opts = SOpts }, As);
	Error -> Error
    end.

tcp_module(Opts) ->
    tcp_module_1(Opts, undefined).

tcp_module(Opts, Addr) ->
    Address = {undefined,Addr},
    %% Address has to be a 2-tuple but the first element is ignored
    tcp_module_1(Opts, Address).

tcp_module_1(Opts, Address) ->
    mod(
      Opts, tcp_module, Address,
      #{inet => inet_tcp, inet6 => inet6_tcp, local => local_tcp}).

gen_tcp_module([{inet_backend, Flag}|Opts]) ->
    gen_tcp_module(Opts, Flag);
gen_tcp_module(Opts) ->
    gen_tcp_module(
      Opts,
      persistent_term:get(
        {kernel, inet_backend}, ?DEFAULT_KERNEL_INET_BACKEND)).
%%
gen_tcp_module(Opts, inet) ->
    {gen_tcp, Opts};
gen_tcp_module(Opts, socket) ->
    {gen_tcp_socket, Opts}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for udp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
udp_options() ->
    [
     debug,
     tos, tclass, priority,
     reuseaddr, reuseport, reuseport_lb, exclusiveaddruse,
     sndbuf, recbuf, header, active, buffer, mode,
     recvtos, recvtclass, ttl, recvttl, deliver, ipv6_v6only,
     broadcast, dontroute, multicast_if, multicast_ttl, multicast_loop,
     add_membership, drop_membership, read_packets, raw,
     high_msgq_watermark, low_msgq_watermark, bind_to_device
    ].


udp_options(Opts, Mod) ->
    case udp_opt(Opts, #udp_opts { }, udp_options()) of
	{ok, R} ->
	    {ok, R#udp_opts {
		   opts = lists:reverse(R#udp_opts.opts),
		   ifaddr = Mod:translate_ip(R#udp_opts.ifaddr)
		  }};
	Error -> Error
    end.

udp_opt([{raw,A,B,C}|Opts], #udp_opts{} = R, As) ->
    udp_opt([{raw,{A,B,C}}|Opts], R, As);
udp_opt([Opt | Opts], #udp_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
	{ifaddr, Addr} when is_map(Addr) ->
            udp_opt(Opts, R#udp_opts { ifaddr = ensure_sockaddr(Addr) }, As);
	{ifaddr, Addr} ->
            udp_opt(Opts, R#udp_opts { ifaddr = Addr }, As);

	{ip, IP} when is_map(IfAddr) ->
            udp_opt(Opts, R#udp_opts { ifaddr = IfAddr#{addr => IP} }, As);
	{ip, IP}                     ->
            udp_opt(Opts, R#udp_opts { ifaddr = IP }, As);

	{port, P} when is_map(IfAddr) ->
            udp_opt(Opts, R#udp_opts { ifaddr = IfAddr#{port => P} }, As);
	{port, P}                     ->
            udp_opt(Opts, R#udp_opts { port = P }, As);

	{fd,Fd}     ->  udp_opt(Opts, R#udp_opts { fd = Fd }, As);
	binary      ->  udp_add(mode, binary, R, Opts, As);
	list        ->  udp_add(mode, list, R, Opts, As);
	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    udp_opt(Opts, R#udp_opts { fd = [{netns,BinNS}] }, As);
		false ->
		    {error, badarg}
	    end;
        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            POpts = lists:keydelete(active, 1, R#udp_opts.opts),
            udp_opt(Opts, R#udp_opts { opts = [{active,N}|POpts] }, As);

        {Membership, {MAddr, If}}
          when ((Membership =:= add_membership) orelse
                (Membership =:= drop_membership)) andalso
               (tuple_size(MAddr) =:= 4) andalso
               ((If =:= any) orelse (tuple_size(If) =:= 4)) ->
            MembershipOpt = {Membership, {MAddr, If, 0}},
            POpts         = R#udp_opts.opts,
            udp_opt(Opts, R#udp_opts{opts = [MembershipOpt|POpts]}, As);

	{Name,Val} when is_atom(Name) -> udp_add(Name, Val, R, Opts, As);

	_ -> {error, badarg}
    end;
udp_opt([], #udp_opts{} = R, _SockOpts) ->
    {ok, R}.

udp_add(Name, Val, #udp_opts{} = R, Opts, As) ->
    case add_opt(Name, Val, R#udp_opts.opts, As) of
	{ok, SOpts} ->
	    udp_opt(Opts, R#udp_opts { opts = SOpts }, As);
	Error -> Error
    end.

udp_module(Opts) ->
    udp_module_1(Opts, undefined).

udp_module(Opts, Addr) ->
    Address = {undefined, Addr},
    %% Address has to be a 2-tuple but the first element is ignored
    udp_module_1(Opts, Address).

udp_module_1(Opts, Address) ->
    mod(
      Opts, udp_module, Address,
      #{inet => inet_udp, inet6 => inet6_udp, local => local_udp}).

gen_udp_module([{inet_backend, Flag}|Opts]) ->
    gen_udp_module(Opts, Flag);
gen_udp_module(Opts) ->
    gen_udp_module(
      Opts,
      persistent_term:get(
        {kernel, inet_backend}, ?DEFAULT_KERNEL_INET_BACKEND)).
%%
gen_udp_module(Opts, inet) ->
    {gen_udp, Opts};
gen_udp_module(Opts, socket) ->
    {gen_udp_socket, Opts}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for sctp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Currently supported options include:
%  (*) {mode,   list|binary}	 or just list|binary
%  (*) {active, true|false|once|N}
%  (*) {sctp_module, inet_sctp|inet6_sctp} or just inet|inet6
%  (*) options set via setsockopt.
%      The full list is below in sctp_options/0 .
%  All other options are currently NOT supported. In particular:
%  (*) multicast on SCTP is not (yet) supported, as it may be incompatible
%      with automatic associations;
%  (*) passing of open FDs ("fdopen") is not supported.
sctp_options() ->
[   % The following are generic inet options supported for SCTP sockets:
    debug,
    mode, active, buffer, tos, tclass, ttl,
    priority, dontroute,
    reuseaddr, reuseport, reuseport_lb, exclusiveaddruse,
    linger, recvtos, recvtclass, recvttl,
    sndbuf, recbuf, ipv6_v6only, high_msgq_watermark, low_msgq_watermark,
    bind_to_device,

    % Other options are SCTP-specific (though they may be similar to their
    % TCP and UDP counter-parts):
    sctp_rtoinfo,   		 sctp_associnfo,	sctp_initmsg,
    sctp_autoclose,		 sctp_nodelay,		sctp_disable_fragments,
    sctp_i_want_mapped_v4_addr,  sctp_maxseg,		sctp_primary_addr,
    sctp_set_peer_primary_addr,  sctp_adaptation_layer,	sctp_peer_addr_params,
    sctp_default_send_param,	 sctp_events,		sctp_delayed_ack_time,
    sctp_status,	   	 sctp_get_peer_addr_info
].

sctp_options(Opts, Mod)  ->
    case sctp_opt(Opts, Mod, #sctp_opts{}, sctp_options()) of
	{ok,SO} ->
	    {ok,SO#sctp_opts{opts=lists:reverse(SO#sctp_opts.opts)}};
	Error -> Error
    end.

sctp_opt([Opt|Opts], Mod, #sctp_opts{ifaddr = IfAddr} = R, As) ->
    case Opt of
        %% what if IfAddr is already a map (=sockaddr)?
        %% Shall we allow ifaddr as a list of sockaddr?
	{ifaddr, Addr} when is_map(Addr) ->
            sctp_opt(Opts, Mod, R#sctp_opts{ifaddr = ensure_sockaddr(Addr)}, As);
	{ifaddr, IP} ->
	    sctp_opt_ifaddr(Opts, Mod, R, As, IP);

	{ip, IP} when is_map(IfAddr) ->
            IP2 = Mod:translate_ip(IP),
            sctp_opt(Opts, Mod, R#sctp_opts{ifaddr = IfAddr#{addr => IP2}}, As);
	{ip, IP} ->
	    sctp_opt_ifaddr(Opts, Mod, R, As, IP);

	{port, Port} ->
	    case Mod:getserv(Port) of
		{ok, P} when is_map(IfAddr) ->
		    sctp_opt(Opts,
                             Mod,
                             R#sctp_opts{ifaddr = IfAddr#{port => P}}, As);
		{ok, P} ->
		    sctp_opt(Opts, Mod, R#sctp_opts{port = P}, As);
		Error ->
                    Error
	    end;

	{type, Type} when Type =:= seqpacket; Type =:= stream ->
            sctp_opt(Opts, Mod, R#sctp_opts{type = Type}, As);

	binary		-> sctp_opt (Opts, Mod, R, As, mode, binary);
	list		-> sctp_opt (Opts, Mod, R, As, mode, list);

	{netns,NS} ->
	    BinNS = filename2binary(NS),
	    case prim_inet:is_sockopt_val(netns, BinNS) of
		true ->
		    sctp_opt(
		      Opts, Mod,
		      R#sctp_opts { fd = [{netns,BinNS}] },
		      As);
		false ->
		    {error, badarg}
	    end;

        {active,N} when is_integer(N), N < 32768, N >= -32768 ->
            NOpts = lists:keydelete(active, 1, R#sctp_opts.opts),
            sctp_opt(Opts, Mod, R#sctp_opts { opts = [{active,N}|NOpts] }, As);

	{Name,Val}	->
            sctp_opt(Opts, Mod, R, As, Name, Val);

	_ ->
            {error, badarg}
    end;
sctp_opt([], _Mod, #sctp_opts{ifaddr=IfAddr}=R, _SockOpts) ->
    if is_list(IfAddr) ->
	    {ok, R#sctp_opts{ifaddr=lists:reverse(IfAddr)}};
       true ->
	    {ok, R}
    end.

sctp_opt(Opts, Mod, #sctp_opts{} = R, As, Name, Val) ->
    case add_opt(Name, Val, R#sctp_opts.opts, As) of
	{ok,SocketOpts} ->
	    sctp_opt(Opts, Mod, R#sctp_opts{opts=SocketOpts}, As);
	Error -> Error
    end.

sctp_opt_ifaddr(Opts, Mod, #sctp_opts{ifaddr=IfAddr}=R, As, Addr) ->
    IP = Mod:translate_ip(Addr),
    sctp_opt(Opts, Mod, 
	     R#sctp_opts{
	       ifaddr=case IfAddr of
			  undefined              -> IP;
			  _ when is_list(IfAddr) -> [IP|IfAddr];
			  _                      -> [IP,IfAddr]
		      end}, As).

sctp_module(Opts) ->
    mod(
      Opts, sctp_module, undefined,
      #{inet => inet_sctp, inet6 => inet6_sctp}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Util to check and insert option in option list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_opt(Name, Val, Opts, As) ->
    case lists:member(Name, As) of
	true ->
	    case prim_inet:is_sockopt_val(Name, Val) of
		true when Name =:= raw ->
		    {ok, [{Name,Val} | Opts]};
		true ->
		    Opts1 = lists:keydelete(Name, 1, Opts),
		    {ok, [{Name,Val} | Opts1]};
		false -> {error,badarg}
	    end;
	false -> {error,badarg}
    end.
	

%% Passthrough all unknown - catch type errors later
filename2binary(List) when is_list(List) ->
    OutEncoding = file:native_name_encoding(),
    try unicode:characters_to_binary(List, unicode, OutEncoding) of
	Bin when is_binary(Bin) ->
	    Bin;
	_ ->
	    List
    catch
	error:badarg ->
	    List
    end;
filename2binary(Bin) ->
    Bin.

binary2filename(Bin) ->
    InEncoding = file:native_name_encoding(),
    case unicode:characters_to_list(Bin, InEncoding) of
	Filename when is_list(Filename) ->
	    Filename;
	_ ->
	    %% For getopt/setopt of netns this should only happen if
	    %% a binary with wrong encoding was used when setting the
	    %% option, hence the user shall eat his/her own medicine.
	    %%
	    %% I.e passthrough here too for now.
	    %% Future usecases will most probably not want this,
	    %% rather Unicode error or warning
	    %% depending on emulator flag instead.
	    Bin
    end.

%% Protocol independent, i.e common code for all
%% inet_* and inet6_* modules
%%
translate_ip(any,      inet)  -> {0,0,0,0};
translate_ip(loopback, inet)  -> {127,0,0,1};
translate_ip(any,      inet6) -> {0,0,0,0,0,0,0,0};
translate_ip(loopback, inet6) -> {0,0,0,0,0,0,0,1};
translate_ip(IP, _)           -> IP.  % undefined goes here

mod(Opts, Tag, Address, Map) ->
    mod(Opts, Tag, Address, Map, undefined, []).
%%
mod([{Tag, M}|Opts], Tag, Address, Map, Mod, Acc) ->
    mod(Opts, Tag, Address, Map, Mod, Acc, M);
mod([{T, _} = Opt|Opts], Tag, _Address, Map, Mod, Acc)
  when T =:= ip; T =:= ifaddr->
    mod(Opts, Tag, Opt, Map, Mod, [Opt|Acc]);
mod([Family|Opts], Tag, Address, Map, Mod, Acc) when is_atom(Family) ->
    case Map of
	#{Family := M} ->
	    mod(Opts, Tag, Address, Map, Mod, Acc, M);
	#{} ->
	    mod(Opts, Tag, Address, Map, Mod, [Family|Acc])
    end;
mod([Opt|Opts], Tag, Address, Map, Mod, Acc) ->
    mod(Opts, Tag, Address, Map, Mod, [Opt|Acc]);
mod([], Tag, Address, Map, undefined, Acc) ->
    {case Address of
	 {_, {local, _}} ->
	     case Map of
		 #{local := Mod} ->
		     Mod;
		 #{} ->
		     inet_db:Tag()
	     end;
	 {_, IP} when tuple_size(IP) =:= 8 ->
	     #{inet := IPv4Mod} = Map,
	     %% Get the mod, but IPv6 address overrides default IPv4
	     case inet_db:Tag() of
		 IPv4Mod ->
		     #{inet6 := IPv6Mod} = Map,
		     IPv6Mod;
		 Mod ->
		     Mod
	     end;
	 {_, #{family := inet6}} ->
	     #{inet := IPv4Mod} = Map,
	     %% Get the mod, but IPv6 address overrides default IPv4
	     case inet_db:Tag() of
		 IPv4Mod ->
		     #{inet6 := IPv6Mod} = Map,
		     IPv6Mod;
		 Mod ->
		     Mod
	     end;
	 _ ->
	     inet_db:Tag()
     end, lists:reverse(Acc)};
mod([], _Tag, _Address, _Map, Mod, Acc) ->
    {Mod, lists:reverse(Acc)}.
%%
mod(Opts, Tag, Address, Map, undefined, Acc, M) ->
    mod(Opts, Tag, Address, Map, M, Acc);
mod(Opts, Tag, Address, Map, Mod, Acc, _M) ->
    mod(Opts, Tag, Address, Map, Mod, Acc).

getaddrs_tm({A,B,C,D} = IP, Fam, _)  ->
    %% Only "syntactic" validation and check of family.
    if 
	?ip(A,B,C,D) ->
	    if
		Fam =:= inet -> {ok,[IP]};
		true -> {error,eafnosupport}
	    end;
	true -> {error,einval}
    end;
getaddrs_tm({A,B,C,D,E,F,G,H} = IP, Fam, _) ->
    %% Only "syntactic" validation; we assume that the address was
    %% "semantically" validated when it was converted to a tuple.
    if 
	?ip6(A,B,C,D,E,F,G,H) ->
	    if
		Fam =:= inet6 -> {ok,[IP]};
		true -> {error,eafnosupport}
	    end;
	true -> {error,einval}
    end;
getaddrs_tm(Address, Family, Timer) when is_atom(Address) ->
    getaddrs_tm(atom_to_list(Address), Family, Timer);
getaddrs_tm(Address, Family, Timer) ->
    %% ?DBG([{address, Address}, {family, Family}, {timer, Timer}]),
    case inet_parse:visible_string(Address) of
	false ->
	    {error,einval};
	true ->
	    %% Address is a host name or a valid IP address,
	    %% either way check it with the resolver.
	    case gethostbyname_tm(Address, Family, Timer) of
		{ok, Ent} ->
		    %% ?DBG([{ent, Ent}]),
		    {ok, Ent#hostent.h_addr_list};
		Error ->
		    %% ?DBG([{error, Error}]),
		    Error
	    end
    end.

%%
%% gethostbyname with option search
%%
gethostbyname_tm(Name, Type, Timer, [string|_]=Opts) ->
    %% ?DBG([string, {name, Name}, {type, Type}, {timer, Timer}]),
    Result = gethostbyname_string(Name, Type),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [dns|_]=Opts) ->
    %% ?DBG([dns, {name, Name}, {type, Type}, {timer, Timer}]),
    Result = inet_res:gethostbyname_tm(Name, Type, Timer),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [file|_]=Opts) ->
    %% ?DBG([file, {name, Name}, {type, Type}, {timer, Timer}]),
    Result = inet_hosts:gethostbyname(Name, Type),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [yp|_]=Opts) ->
    %% ?DBG([yp, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [nis|_]=Opts) ->
    %% ?DBG([nis, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [nisplus|_]=Opts) ->
    %% ?DBG([niplus, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [wins|_]=Opts) ->
    %% ?DBG([wins, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [native|_]=Opts) ->
    %% ?DBG([native, {name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [_|Opts]) ->
    %% ?DBG([{name, Name}, {type, Type}, {timer, Timer}]),
    gethostbyname_tm(Name, Type, Timer, Opts);
%% Make sure we always can look up our own hostname.
gethostbyname_tm(Name, Type, Timer, []) ->
    %% ?DBG([{name, Name}, {type, Type}, {timer, Timer}]),
    Result = gethostbyname_self(Name, Type),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, [], Result).

gethostbyname_tm(Name, Type, Timer, Opts, Result) ->
    %% ?DBG([string, {name, Name}, {type, Type}, {timer, Timer},
    %% 	  {opts, Opts}, {result, Result}]),
    case Result of
	{ok,_} ->
	    Result;
	{error,formerr} ->
	    {error,einval};
	{error,_} when Opts =:= [] ->
	    {error,nxdomain};
	{error,_} ->
	    gethostbyname_tm(Name, Type, Timer, tl(Opts))
    end.

gethostbyname_tm_native(Name, Type, Timer, Opts) ->
    %% ?DBG([{name, Name}, {type, Type}, {timer, Timer}, {opts, Opts}]),
    %% Fixme: add (global) timeout to gethost_native
    Result = inet_gethost_native:gethostbyname(Name, Type),
    %% ?DBG([{result, Result}]),
    gethostbyname_tm(Name, Type, Timer, Opts, Result).



gethostbyname_self(Name, Type) when is_atom(Name) ->
    %% ?DBG([{name, Name}, {type, Type}]),
    gethostbyname_self(atom_to_list(Name), Type);
gethostbyname_self(Name, Type)
  when is_list(Name), Type =:= inet;
       is_list(Name), Type =:= inet6 ->
    %% ?DBG([{name, Name}, {type, Type}]),
    N    = inet_db:tolower(Name),
    Self = inet_db:gethostname(),
    %% ?DBG([{n, N}, {self, Self}]),
    %%
    %% This is the final fallback that pretends /etc/hosts has got
    %% a line for the hostname on the loopback address.
    %% Lookups into /etc/hosts are case insensitive and return
    %% what is in the file. Therefore the letter case may differ between
    %% the returned hostent record and the hostname that was asked for.
    %%
    case inet_db:tolower(Self) of
	N ->
	    %% ?DBG([{n, N}]),
	    {ok,
	     make_hostent(
	       Self, [translate_ip(loopback, Type)], [], Type)};
	_ ->
	    case inet_db:res_option(domain) of
		"" ->
		    %% ?DBG(['res option empty domain']),
		    {error,nxdomain};
		Domain ->
		    %% ?DBG([{domain, Domain}]),
		    FQDN = lists:append([Self,".",Domain]),
		    case inet_db:tolower(FQDN) of
			N ->
			    {ok,
			     make_hostent(
			       FQDN,
			       [translate_ip(loopback, Type)], [], Type)};
			_ ->
			    %% ?DBG(['invalid domain', {fqdn, FQDN}]),
			    {error,nxdomain}
		    end
	    end
    end;
gethostbyname_self(_, _) ->
    {error,formerr}.

gethostbyname_string(Name, Type) when is_atom(Name) ->
    gethostbyname_string(atom_to_list(Name), Type);
gethostbyname_string(Name, Type)
  when is_list(Name), Type =:= inet;
       is_list(Name), Type =:= inet6 ->
    case
	case Type of
	    inet ->
		inet_parse:ipv4_address(Name);
	    inet6 ->
		inet_parse:ipv6strict_address(Name)
	end of
	{ok,IP} ->
	    {ok,make_hostent(Name, [IP], [], Type)};
	{error,einval} ->
	    {error,nxdomain}
    end;
gethostbyname_string(_, _) ->
    {error,formerr}.

make_hostent(Name, Addrs, Aliases, Type) ->
    #hostent{h_name = Name,
	     h_aliases = Aliases,
	     h_addrtype = Type,
	     h_length = case Type of inet -> 4; inet6 -> 16 end,
	     h_addr_list = Addrs}.

%%
%% gethostbyaddr with option search
%%
gethostbyaddr_tm(Addr, Timer, [dns | Opts]) ->
    Res = inet_res:gethostbyaddr_tm(Addr,Timer),
    case Res of
	{ok,_} -> Res;
	{error,timeout} -> Res;
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts)
    end;    
gethostbyaddr_tm(Addr, Timer, [file | Opts]) ->
    case inet_hosts:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts);
	Result -> Result
    end;
gethostbyaddr_tm(Addr, Timer, [yp | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [nis | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer,  [nisplus | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [wins | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [native | Opts]) ->
    gethostbyaddr_tm_native(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, [_ | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, Opts);
gethostbyaddr_tm({127,0,0,1}=IP, _Timer, []) ->
    gethostbyaddr_self(IP, inet);
gethostbyaddr_tm({0,0,0,0,0,0,0,1}=IP, _Timer, []) ->
    gethostbyaddr_self(IP, inet6);
gethostbyaddr_tm(_Addr, _Timer, []) ->
    {error, nxdomain}.

gethostbyaddr_self(IP, Type) ->
    Name = inet_db:gethostname(),
    case inet_db:res_option(domain) of
	"" ->
	    {ok,make_hostent(Name, [IP], [], Type)};
	Domain ->
	    {ok,make_hostent(Name++"."++Domain, [IP], [Name], Type)}
    end.
	    
gethostbyaddr_tm_native(Addr, Timer, Opts) ->
    %% Fixme: user timer for timeoutvalue
    case inet_gethost_native:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts);
	Result -> Result
    end.


-spec open(Fd_or_OpenOpts :: integer() | list(),
	   BAddr ::
             socket:sockaddr_in6() |
	     socket_address() |
	     {ip_address() | 'any' | 'loopback', % Unofficial
	      port_number()} |
	     {inet, % Unofficial
	      {ip4_address() | 'any' | 'loopback',
	       port_number()}} |
	     {inet6, % Unofficial
	      {ip6_address() | 'any' | 'loopback',
	       port_number()}} |
	     undefined, % Internal - no bind()
	   BPort :: port_number(),
	   Opts :: [socket_setopt()],
	   Protocol :: socket_protocol(),
	   Family :: address_family(),
	   Type :: socket_type(),
	   Module :: atom()) ->
	{'ok', port()} | {'error', posix()}.

open(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module)
  when is_integer(Fd), 0 =< Fd ->
    open_fd(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module);
open(Fd_or_OpenOpts, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    open_opts(
      Fd_or_OpenOpts,
      if
          BAddr =:= undefined, BPort =/= 0 ->
              translate_ip(any, Family);
          true ->
              BAddr
      end, BPort, Opts, Protocol, Family, Type, Module).

%% The only difference between open/8 and open_bind/8 is that
%% if Fd_or_OpenOpts is not a FileDescriptor :: non_neg_integer()
%% i.e option {fd,Fd} has not been used hence we are not handling
%% an already open socket handle, and also if no bind address
%% has been specified (BAddr =:= undefined).
%%
%% Then open_bind/8 will bind to the wildcard address and the
%% specified port (BPort, 0 = wildcard port per default),
%% which is the legacy behaviour by this module when opening a socket.
%%
%% In the same situation, open/8 will bind to the wildcard address
%% and the specified port, only if BPort is not 0, i.e a bind port
%% has been specified to not be the wildcard port.
%%
%% So open/8 per default does not bind to an address, which
%% is used by TCP connect to let the OS automatically bind to
%% an address later, during TCP connect operation.  This
%% gives the OS more freedom in choosing the originating port and
%% therefore makes far more effective use of the port range.

-spec open_bind(Fd_or_OpenOpts :: integer() | list(),
                BAddr ::
                  socket:sockaddr_in6() |
                  socket_address() |
                  {ip_address() | 'any' | 'loopback', % Unofficial
                   port_number()} |
                  {inet, % Unofficial
                   {ip4_address() | 'any' | 'loopback',
                    port_number()}} |
                  {inet6, % Unofficial
                   {ip6_address() | 'any' | 'loopback',
                    port_number()}} |
                  undefined, % Internal - translated to 'any'
                BPort :: port_number(),
                Opts :: [socket_setopt()],
                Protocol :: socket_protocol(),
                Family :: address_family(),
                Type :: socket_type(),
                Module :: atom()) ->
                       {'ok', port()} | {'error', posix()}.

open_bind(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module)
  when is_integer(Fd), 0 =< Fd ->
    %% ?DBG([{fd, Fd},
    %%       {baddr, BAddr}, {bport, BPort},
    %%       {opts, Opts}, {proto, Protocol}, {fam, Family},
    %%       {type, Type}, {mod, Module}]),
    open_fd(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module);
open_bind(
  Fd_or_OpenOpts, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    %% ?DBG([{fd_or_openopts, Fd_or_OpenOpts},
    %%       {baddr, BAddr}, {bport, BPort},
    %%       {opts, Opts}, {proto, Protocol}, {fam, Family},
    %%       {type, Type}, {mod, Module}]),
    open_opts(
      Fd_or_OpenOpts,
      if
          BAddr =:= undefined ->
              translate_ip(any, Family);
          true ->
              BAddr
      end, BPort, Opts, Protocol, Family, Type, Module).


open_fd(Fd, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    DoNotBind =
	%% We do not do any binding if no port+addr options
	%% were given, in order to keep backwards compatibility
	%% with pre Erlang/OTP 17
        BAddr =:= undefined, % Presumably already bound
    if
        DoNotBind ->
            0 = BPort, ok; % Assertion
        true ->
            ok
    end,
    case prim_inet:fdopen(Protocol, Family, Type, Fd, DoNotBind) of
	{ok, S} ->
            open_setopts(S, BAddr, BPort, Opts, Module);
        Error ->
            Error
    end.

open_opts(Fd_or_OpenOpts, BAddr, BPort, Opts, Protocol, Family, Type, Module) ->
    %% ?DBG([{fd_or_openopts, Fd_or_OpenOpts},
    %%       {baddr, BAddr}, {bport, BPort},
    %%       {opts, Opts}, {proto, Protocol}, {fam, Family},
    %%       {type, Type}, {mod, Module}]),
    OpenOpts =
	if
            is_list(Fd_or_OpenOpts) -> Fd_or_OpenOpts;
	    true -> []
	end,
    case prim_inet:open(Protocol, Family, Type, OpenOpts) of
	{ok,S} ->
            %% ?DBG(['prim_inet:open', {s, S}]),
            open_setopts(S, BAddr, BPort, Opts, Module);
        Error ->
            Error
    end.

%% If BAddr is undefined - do not bind to an address
%%
open_setopts(S, BAddr, BPort, Opts, Module) ->
    %% ?DBG([{s, S}, {baddr, BAddr}, {bport, BPort}, {opts, Opts}, {mod, Module}]),
    %% ok = prim_inet:setopts(S, [{debug, true}]),
    case prim_inet:setopts(S, Opts) of
        ok when BAddr =:= undefined ->
            %% ?DBG("ok -> register socket"),
            inet_db:register_socket(S, Module),
            {ok,S};
        ok ->
            %% ?DBG("ok -> try bind"),
            try bind(S, BAddr, BPort) of
                {ok, _} ->
                    %% ?DBG("bound"),
                    inet_db:register_socket(S, Module),
                    {ok,S};
                Error  ->
                    %% ?DBG(["bind error", {error, Error}]),
                    prim_inet:close(S),
                    Error
            catch
                BC:BE:BS ->
                    %% ?DBG(["bind failed", {class, BC}, {error, BE}, {stack, BS}]),
                    prim_inet:close(S),
                    erlang:raise(BC, BE, BS)
            end;
        Error  ->
            %% ?DBG(["error", {error, Error}]),
            prim_inet:close(S),
            Error
    end.



bind(S, Addr, Port) when is_list(Addr) ->
    bindx(S, Addr, Port);
bind(S, Addr, Port) ->
    %% ?DBG([{s, S}, {addr, Addr}, {port, Port}]),
    prim_inet:bind(S, Addr, Port).

bindx(S, [Addr], Port0) ->
    {IP, Port} = set_bindx_port(Addr, Port0),
    prim_inet:bind(S, IP, Port);
bindx(S, Addrs, Port0) ->
    [{IP, Port} | Rest] = [set_bindx_port(Addr, Port0) || Addr <- Addrs],
    case prim_inet:bind(S, IP, Port) of
	{ok, AssignedPort} when Port =:= 0 ->
	    %% On newer Linux kernels, Solaris and FreeBSD, calling
	    %% bindx with port 0 is ok, but on SuSE 10, it results in einval
	    Rest2 = [change_bindx_0_port(Addr, AssignedPort) || Addr <- Rest],
	    prim_inet:bind(S, add, Rest2);
	{ok, _} ->
	    prim_inet:bind(S, add, Rest);
	Error ->
	    Error
    end.

set_bindx_port({_IP, _Port}=Addr, _OtherPort) ->
    Addr;
set_bindx_port(IP, Port) ->
    {IP, Port}.

change_bindx_0_port({IP, 0}, AssignedPort) ->
    {IP, AssignedPort};
change_bindx_0_port({_IP, _Port}=Addr, _AssignedPort) ->
    Addr.


-spec fdopen(Fd :: non_neg_integer(),
	     Opts :: [socket_setopt()],
	     Protocol :: socket_protocol(),
	     Family :: address_family(),
	     Type :: socket_type(),
	     Module :: atom()) ->
	{'ok', socket()} | {'error', posix()}.

fdopen(Fd, Opts, Protocol, Family, Type, Module)
  when is_integer(Fd), 0 =< Fd ->
    open_fd(Fd, undefined, 0, Opts, Protocol, Family, Type, Module).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  socket stat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec i() -> ok.
i() -> i(tcp), i(udp), i(sctp).

-spec i(socket_protocol()) -> ok.
i(Proto) -> i(Proto, [port, module, recv, sent, owner,
		      local_address, foreign_address, state, type]).

-spec i(socket_protocol(), [atom()]) -> ok.
i(tcp, Fs) ->
    ii(tcp_sockets(), Fs, tcp);
i(udp, Fs) ->
    ii(udp_sockets(), Fs, udp);
i(sctp, Fs) ->
    ii(sctp_sockets(), Fs, sctp).

ii(Ss, Fs, Proto) ->
    LLs =
	case info_lines(Ss, Fs, Proto) of
	    [] -> [];
	    InfoLines -> [h_line(Fs) | InfoLines]
	end,
    Maxs = foldl(
	     fun(Line,Max0) -> smax(Max0,Line) end, 
	     duplicate(length(Fs),0),LLs),
    Fmt = append(["~-" ++ integer_to_list(N) ++ "s " || N <- Maxs]) ++ "\n",
    lists:foreach(fun(Line) -> io:format(Fmt, Line) end, LLs).

smax([Max|Ms], [Str|Strs]) ->
    N = length(Str),
    [if N > Max -> N; true -> Max end | smax(Ms, Strs)];
smax([], []) -> [].

info_lines(Ss, Fs, Proto) -> [i_line(S, Fs,Proto) || S <- Ss].
i_line(S, Fs, Proto)      -> [info(S, F, Proto) || F <- Fs].

h_line(Fs) -> [h_field(atom_to_list(F)) || F <- Fs].

h_field([C|Cs]) -> [upper(C) | hh_field(Cs)].

hh_field([$_,C|Cs]) -> [$\s,upper(C) | hh_field(Cs)];
hh_field([C|Cs]) -> [C|hh_field(Cs)];
hh_field([]) -> [].

upper(C) when C >= $a, C =< $z -> (C-$a) + $A;
upper(C) -> C.

    
info({'$inet', GenSocketMod, _} = S, F, Proto) when is_atom(GenSocketMod) ->
    case F of
	owner ->
	    case GenSocketMod:info(S) of
		#{owner := Owner} when is_pid(Owner) -> pid_to_list(Owner);
		_ -> " "
	    end;
	port ->
	    case GenSocketMod:getopts(S, [fd]) of
		{ok, [{fd, FD}]} ->
		    "esock[" ++ integer_to_list(FD) ++ "]";
		_ ->
		    "esock"
	    end;
	sent ->
	    case GenSocketMod:getstat(S, [send_oct]) of
		{ok, [{send_oct, N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	recv ->
	    case GenSocketMod:getstat(S, [recv_oct]) of
		{ok, [{recv_oct, N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	local_address ->
	    fmt_addr(GenSocketMod:sockname(S), Proto);
	foreign_address ->
	    fmt_addr(GenSocketMod:peername(S), Proto);
	state ->
	    case GenSocketMod:info(S) of
		#{rstates := RStates,
		  wstates := WStates} -> fmt_compat_status(RStates, WStates);
		_ -> " "
	    end;
	packet ->
	    case GenSocketMod:which_packet_type(S) of
		{ok, Type} -> atom_to_list(Type);
		_ -> " "
	    end;
	type ->
	    case GenSocketMod:info(S) of
		#{type := stream} -> "STREAM";
		_ -> " "
	    end;
	%% Why do we have this here? Its never called (see i/2 calling i/2).
	fd ->
	    case GenSocketMod:getopts(S, [fd]) of
		{ok, [{fd, Fd}]} -> integer_to_list(Fd);
		_ -> " "
	    end;
	module ->
	    atom_to_list(GenSocketMod)
    end;
info(S, F, Proto) ->
    case F of
	owner ->
	    case erlang:port_info(S, connected) of
		{connected, Owner} -> pid_to_list(Owner);
		_ -> " "
	    end;
	port ->
	    case erlang:port_info(S,id) of
		{id, Id}  -> integer_to_list(Id);
		undefined -> " "
	    end;
	sent ->
	    case prim_inet:getstat(S, [send_oct]) of
		{ok,[{send_oct,N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	recv ->
	    case  prim_inet:getstat(S, [recv_oct]) of
		{ok,[{recv_oct,N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	local_address ->
	    fmt_addr(prim_inet:sockname(S), Proto);
	foreign_address ->
	    fmt_addr(prim_inet:peername(S), Proto);
	state ->
	    case prim_inet:getstatus(S) of
		{ok,Status} -> fmt_status(Status);
		_ -> " "
	    end;
	packet ->
	    case prim_inet:getopt(S, packet) of
		{ok,Type} when is_atom(Type) -> atom_to_list(Type);
		{ok,Type} when is_integer(Type) -> integer_to_list(Type);
		_ -> " "
	    end;
	type ->
	    case prim_inet:gettype(S) of
		{ok,{_,stream}} -> "STREAM";
		{ok,{_,dgram}}  -> "DGRAM";
		{ok,{_,seqpacket}} -> "SEQPACKET";
		_ -> " "
	    end;
	%% Why do we have this here? Its never called (see i/2 calling i/2).
	fd ->
	    case prim_inet:getfd(S) of
		{ok, Fd} -> integer_to_list(Fd);
		_ -> " "
	    end;
	module ->
	    case inet_db:lookup_socket(S) of
		{ok,Mod} -> atom_to_list(Mod);
		_ -> "prim_inet"
	    end
    end.

%% Possible flags: (sorted)
%% [accepting,bound,busy,connected,connecting,listen,listening,open]
%% Actually, we no longer gets listening...
fmt_status(Flags) ->
    case lists:sort(Flags) of
	[accepting | _]               -> "ACCEPTING";
	[bound,busy,connected|_]      -> "CONNECTED(BB)";
	[bound,connected|_]           -> "CONNECTED(B)";
	[bound,listen,listening | _]  -> "LISTENING";
	[bound,listen | _]            -> "LISTEN";
	[bound,connecting | _]        -> "CONNECTING";
	[bound,open]                  -> "BOUND";
	[connected,open]              -> "CONNECTED(O)";
	[open]                        -> "IDLE";
	[]                            -> "CLOSED";
	Sorted                        -> fmt_status2(Sorted)
    end.

fmt_compat_status(RFlags, WFlags) ->
    fmt_status(fmt_compat_status_merge(RFlags, WFlags)).

fmt_compat_status_merge(RFlags, WFlags) ->
    fmt_compat_status_merge(RFlags, WFlags, []).
    
fmt_compat_status_merge([], WFlags, Merged) ->
    Merged ++ WFlags;
fmt_compat_status_merge([RFlag|RFlags], WFlags, Merged) ->
    fmt_compat_status_merge(RFlags,
			    lists:delete(RFlag, WFlags),
			    [RFlag|Merged]).

fmt_status2([H]) ->
    fmt_status3(H);
fmt_status2([H|T]) ->
    fmt_status3(H) ++ ":"  ++ fmt_status2(T).

fmt_status3(accepting) ->
    "A";
fmt_status3(bound) ->
    "BD";
fmt_status3(busy) ->
    "BY";
fmt_status3(connected) ->
    "CD";
fmt_status3(connecting) ->
    "CG";
fmt_status3(listen) ->
    "LN";
fmt_status3(listening) ->
    "LG";
fmt_status3(open) ->
    "O";
fmt_status3(selected) ->
    "SD";
fmt_status3(X) when is_atom(X) ->
    string:uppercase(atom_to_list(X)).


fmt_addr({error,enotconn}, _) -> "*:*";
fmt_addr({error,_}, _)        -> " ";
fmt_addr({ok,Addr}, Proto) ->
    case Addr of
	%%Dialyzer {0,0}            -> "*:*";
	{{0,0,0,0},Port} -> "*:" ++ fmt_port(Port, Proto);
	{{0,0,0,0,0,0,0,0},Port} -> "*:" ++ fmt_port(Port, Proto);
	{{127,0,0,1},Port} -> "localhost:" ++ fmt_port(Port, Proto);
	{{0,0,0,0,0,0,0,1},Port} -> "localhost:" ++ fmt_port(Port, Proto);
	{local, Path} -> "local:" ++ binary_to_list(Path);
	{IP,Port} -> inet_parse:ntoa(IP) ++ ":" ++ fmt_port(Port, Proto)
    end.

fmt_port(N, Proto) ->
    case inet:getservbyport(N, Proto) of
	{ok, Name} -> Name;
	_ -> integer_to_list(N)
    end.

%% Return a list of all tcp sockets
tcp_sockets()  -> port_list("tcp_inet") ++ gen_tcp_socket:which_sockets().
udp_sockets()  -> port_list("udp_inet") ++ gen_udp_socket:which_sockets().
sctp_sockets() -> port_list("sctp_inet").

%% Return all ports having the name 'Name'
port_list(Name) ->
    filter(
      fun(Port) ->
	      case erlang:port_info(Port, name) of
		  {name, Name} -> true;
		  _ -> false
	      end
      end, erlang:ports()).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec format_error(Reason) -> string() when
      Reason :: posix() | system_limit.

format_error(exbadport) -> "invalid port state";
format_error(exbadseq) ->  "bad command sequence";
format_error(system_limit) ->
    "a system limit was hit, probably not enough ports";
format_error(Tag) ->
    erl_posix_msg:message(Tag).

%% Close a TCP socket.
tcp_close(S) when is_port(S) ->
    %% if exit_on_close is set we must force a close even if remotely closed!!!
    prim_inet:close(S),
    receive {tcp_closed, S} -> ok after 0 -> ok end.

%% Close a UDP socket.
udp_close(S) when is_port(S) ->
    receive 
	{udp_closed, S} -> ok
    after 0 ->
	    prim_inet:close(S),
	    receive {udp_closed, S} -> ok after 0 -> ok end
    end.

%% Set controlling process for TCP socket.
tcp_controlling_process(S, NewOwner) when is_port(S), is_pid(NewOwner) ->
    case erlang:port_info(S, connected) of
	{connected, NewOwner} ->
	    ok;
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	undefined ->
	    {error, einval};
	_ ->
	    case prim_inet:getopt(S, active) of
		{ok, A0} ->
		    SetOptRes =
			case A0 of
			    false -> ok;
			    _ -> prim_inet:setopt(S, active, false)
			end,
		    case {tcp_sync_input(S, NewOwner, false), SetOptRes} of
			{true, _} ->  %% socket already closed
			    ok;
			{false, ok} ->
			    try erlang:port_connect(S, NewOwner) of
				true -> 
				    unlink(S), %% unlink from port
				    case A0 of
					false -> ok;
					_ -> prim_inet:setopt(S, active, A0)
				    end
			    catch
				error:Reason -> 
				    {error, Reason}
			    end;
			{false, Error} ->
			    Error
		    end;
		Error ->
		    Error
	    end
    end.

tcp_sync_input(S, Owner, Flag) ->
    receive
	{tcp, S, Data} ->
	    Owner ! {tcp, S, Data},
	    tcp_sync_input(S, Owner, Flag);
	{tcp_closed, S} ->
	    Owner ! {tcp_closed, S},
	    tcp_sync_input(S, Owner, true);
	{S, {data, Data}} ->
	    Owner ! {S, {data, Data}},
	    tcp_sync_input(S, Owner, Flag);	    
	{inet_async, S, Ref, Status} ->
	    Owner ! {inet_async, S, Ref, Status},
	    tcp_sync_input(S, Owner, Flag);
	{inet_reply, S, Status} ->
	    Owner ! {inet_reply, S, Status},
	    tcp_sync_input(S, Owner, Flag)
    after 0 -> 
	    Flag
    end.

%% Set controlling process for UDP or SCTP socket.
udp_controlling_process(S, NewOwner) when is_port(S), is_pid(NewOwner) ->
    case erlang:port_info(S, connected) of
	{connected, NewOwner} ->
	    ok;
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	_ ->
	    {ok, A0} = prim_inet:getopt(S, active),
	    ok = prim_inet:setopt(S, active, false),
	    udp_sync_input(S, NewOwner),
	    try erlang:port_connect(S, NewOwner) of
		true -> 
		    unlink(S),
		    ok = prim_inet:setopt(S, active, A0)
	    catch
		error:Reason -> 
		    {error, Reason}
	    end
    end.

udp_sync_input(S, Owner) ->
    receive
	{sctp, S, _, _, _}=Msg    -> udp_sync_input(S, Owner, Msg);
	{udp, S, _, _, _}=Msg     -> udp_sync_input(S, Owner, Msg);
	{udp_closed, S}=Msg       -> udp_sync_input(S, Owner, Msg);
	{S, {data,_}}=Msg         -> udp_sync_input(S, Owner, Msg);
	{inet_async, S, _, _}=Msg -> udp_sync_input(S, Owner, Msg);
	{inet_reply, S, _}=Msg    -> udp_sync_input(S, Owner, Msg)
    after 0 ->
	    ok
    end.

udp_sync_input(S, Owner, Msg) ->
    Owner ! Msg,
    udp_sync_input(S, Owner).

start_timer(infinity) -> false;
start_timer(Timeout) ->
    erlang:start_timer(Timeout, self(), inet).

timeout(false) -> infinity;
timeout(Timer) ->
    case erlang:read_timer(Timer) of
	false -> 0;
	Time  -> Time
    end.

timeout(Time, false) -> Time;
timeout(Time, Timer) ->
    TimerTime = timeout(Timer),
    if TimerTime < Time -> TimerTime;
       true -> Time
    end.

stop_timer(false) -> false;
stop_timer(Timer) ->
    case erlang:cancel_timer(Timer) of
	false ->
	    receive
		{timeout,Timer,_} -> false
	    after 0 ->
		    false
	    end;
	T -> T
    end.


lock_socket(S,Val) ->
    case erlang:port_info(S, connected) of
	{connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	undefined ->
	    {error, einval};
	_ ->
	    prim_inet:ignorefd(S,Val)
    end.


ensure_sockaddr(SockAddr) ->
    try prim_socket:enc_sockaddr(SockAddr)
    catch
        throw : {invalid, _} = Invalid : Stacktrace ->
            erlang:raise(error, Invalid, Stacktrace)
    end.
