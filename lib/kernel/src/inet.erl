%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(inet).

-include("inet.hrl").
-include("inet_int.hrl").
-include("inet_sctp.hrl").

%% socket
-export([peername/1, sockname/1, port/1, send/2,
	 setopts/2, getopts/2, 
	 getifaddrs/0, getifaddrs/1,
	 getif/1, getif/0, getiflist/0, getiflist/1,
	 ifget/3, ifget/2, ifset/3, ifset/2,
	 getstat/1, getstat/2,
	 ip/1, stats/0, options/0, 
	 pushf/3, popf/1, close/1, gethostname/0, gethostname/1, 
	 parse_ipv4_address/1, parse_ipv6_address/1, parse_ipv4strict_address/1,
	 parse_ipv6strict_address/1, parse_address/1, parse_strict_address/1]).

-export([connect_options/2, listen_options/2, udp_options/2, sctp_options/2]).

-export([i/0, i/1, i/2]).

-export([getll/1, getfd/1, open/8, fdopen/6]).

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

-export_type([address_family/0, hostent/0, hostname/0, ip4_address/0,
              ip6_address/0, ip_address/0, posix/0, socket/0,
              port_number/0]).

%% imports
-import(lists, [append/1, duplicate/2, filter/2, foldl/3]).

%% Record Signature
-define(RS(Record),
	{Record, record_info(size, Record)}).
%% Record Signature Check (guard)
-define(RSC(Record, RS),
	element(1, Record) =:= element(1, RS),
	tuple_size(Record) =:= element(2, RS)).

%%% ---------------------------------
%%% Contract type definitions


-type hostent() :: #hostent{}.
-type hostname() :: atom() | string().
-type ip4_address() :: {0..255,0..255,0..255,0..255}.
-type ip6_address() :: {0..65535,0..65535,0..65535,0..65535,
			0..65535,0..65535,0..65535,0..65535}.
-type ip_address() :: ip4_address() | ip6_address().
-type port_number() :: 0..65535.
-type posix() :: exbadport | exbadseq | file:posix().
-type socket() :: port().

-type socket_setopt() ::
        gen_sctp:option() | gen_tcp:option() | gen_udp:option().

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

-type address_family() :: 'inet' | 'inet6'.
-type socket_protocol() :: 'tcp' | 'udp' | 'sctp'.
-type socket_type() :: 'stream' | 'dgram' | 'seqpacket'.
-type stat_option() :: 
	'recv_cnt' | 'recv_max' | 'recv_avg' | 'recv_oct' | 'recv_dvi' |
	'send_cnt' | 'send_max' | 'send_avg' | 'send_oct' | 'send_pend'.

%%% ---------------------------------

-spec get_rc() -> [{Par :: any(), Val :: any()}].

get_rc() ->
    inet_db:get_rc().

-spec close(Socket) -> 'ok' when
      Socket :: socket().

close(Socket) ->
    prim_inet:close(Socket),
    receive
	{Closed, Socket} when Closed =:= tcp_closed; Closed =:= udp_closed ->
	    ok
    after 0 ->
	    ok
    end.

-spec peername(Socket) ->  {ok, {Address, Port}} | {error, posix()} when
      Socket :: socket(),
      Address :: ip_address(),
      Port :: non_neg_integer().

peername(Socket) -> 
    prim_inet:peername(Socket).

-spec setpeername(Socket :: socket(), Address :: {ip_address(), port_number()}) ->
	'ok' | {'error', any()}.

setpeername(Socket, {IP,Port}) ->
    prim_inet:setpeername(Socket, {IP,Port});
setpeername(Socket, undefined) ->
    prim_inet:setpeername(Socket, undefined).


-spec sockname(Socket) -> {ok, {Address, Port}} | {error, posix()} when
      Socket :: socket(),
      Address :: ip_address(),
      Port :: non_neg_integer().

sockname(Socket) -> 
    prim_inet:sockname(Socket).

-spec setsockname(Socket :: socket(), Address :: {ip_address(), port_number()}) ->
	'ok' | {'error', any()}.

setsockname(Socket, {IP,Port}) -> 
    prim_inet:setsockname(Socket, {IP,Port});
setsockname(Socket, undefined) ->
    prim_inet:setsockname(Socket, undefined).

-spec port(Socket) -> {'ok', Port} | {'error', any()} when
      Socket :: socket(),
      Port :: port_number().

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

setopts(Socket, Opts) -> 
    prim_inet:setopts(Socket, Opts).

-spec getopts(Socket, Options) ->
	{'ok', OptionValues} | {'error', posix()} when
      Socket :: socket(),
      Options :: [socket_getopt()],
      OptionValues :: [socket_setopt()].

getopts(Socket, Opts) ->
    prim_inet:getopts(Socket, Opts).

-spec getifaddrs(Socket :: socket()) ->
	{'ok', [string()]} | {'error', posix()}.

getifaddrs(Socket) ->
    prim_inet:getifaddrs(Socket).

-spec getifaddrs() -> {ok, Iflist} | {error, posix()} when
      Iflist :: [{Ifname,[Ifopt]}],
      Ifname :: string(),
      Ifopt :: {flag,[Flag]} | {addr,Addr} | {netmask,Netmask}
             | {broadaddr,Broadaddr} | {dstaddr,Dstaddr}
             | {hwaddr,Hwaddr},
      Flag :: up | broadcast | loopback | pointtopoint
            | running | multicast,
      Addr :: ip_address(),
      Netmask :: ip_address(),
      Broadaddr :: ip_address(),
      Dstaddr :: ip_address(),
      Hwaddr :: [byte()].

getifaddrs() ->
    withsocket(fun(S) -> prim_inet:getifaddrs(S) end).

-spec getiflist(Socket :: socket()) ->
	{'ok', [string()]} | {'error', posix()}.

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

-spec ifget(Name :: string() | atom(), Opts :: [if_getopt()]) ->
	{'ok', [if_getopt_result()]} | {'error', posix()}.

ifget(Name, Opts) ->
    withsocket(fun(S) -> prim_inet:ifget(S, Name, Opts) end).

-spec ifset(Socket :: socket(),
            Name :: string() | atom(),
	    Opts :: [if_setopt()]) ->
	'ok' | {'error', posix()}.

ifset(Socket, Name, Opts) -> 
    prim_inet:ifset(Socket, Name, Opts).

-spec ifset(Name :: string() | atom(), Opts :: [if_setopt()]) ->
	'ok' | {'error', posix()}.

ifset(Name, Opts) ->
    withsocket(fun(S) -> prim_inet:ifset(S, Name, Opts) end).

-spec getif() ->
	{'ok', [{ip_address(), ip_address() | 'undefined', ip_address()}]} | 
	{'error', posix()}.

getif() -> 
    withsocket(fun(S) -> getif(S) end).

%% backwards compatible getif
-spec getif(Socket :: socket()) ->
	{'ok', [{ip_address(), ip_address() | 'undefined', ip_address()}]} | 
	{'error', posix()}.

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
    case inet_udp:open(0,[]) of
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
    prim_inet:getstat(Socket, stats()).

-spec getstat(Socket, Options) ->
	{ok, OptionValues} | {error, posix()} when
      Socket :: socket(),
      Options :: [stat_option()],
      OptionValues :: [{stat_option(), integer()}].

getstat(Socket,What) ->
    prim_inet:getstat(Socket, What).

-spec gethostbyname(Hostname) -> {ok, Hostent} | {error, posix()} when
      Hostname :: hostname(),
      Hostent :: hostent().

gethostbyname(Name) -> 
    gethostbyname_tm(Name, inet, false).

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
    stop_timer(Timer),
    Res.

gethostbyname_tm(Name,Family,Timer) ->
    Opts0 = inet_db:res_option(lookup),
    Opts =
	case (lists:member(native, Opts0) orelse
	      lists:member(string, Opts0) orelse
	      lists:member(nostring, Opts0)) of
	    true ->
		Opts0;
	    false ->
		[string|Opts0]
	end,
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
    stop_timer(Timer),
    Res.

gethostbyaddr_tm(Address,Timer) ->
    gethostbyaddr_tm(Address, Timer, inet_db:res_option(lookup)).

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
    Timer = start_timer(Timeout),
    Res = getaddr_tm(Address, Family, Timer),
    stop_timer(Timer),
    Res.

getaddr_tm(Address, Family, Timer) ->
    case getaddrs_tm(Address, Family, Timer) of
	{ok, [IP|_]} -> {ok, IP};
	Error -> Error
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
    stop_timer(Timer),
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

-spec parse_ipv4_address(Address) ->
	{ok, IPv4Address} | {error, einval} when
      Address :: string(),
      IPv4Address :: ip_address().
parse_ipv4_address(Addr) ->
    inet_parse:ipv4_address(Addr).

-spec parse_ipv6_address(Address) ->
	{ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip_address().
parse_ipv6_address(Addr) ->
    inet_parse:ipv6_address(Addr).

-spec parse_ipv4strict_address(Address) ->
	{ok, IPv4Address} | {error, einval} when
      Address :: string(),
      IPv4Address :: ip_address().
parse_ipv4strict_address(Addr) ->
    inet_parse:ipv4strict_address(Addr).

-spec parse_ipv6strict_address(Address) ->
	{ok, IPv6Address} | {error, einval} when
      Address :: string(),
      IPv6Address :: ip_address().
parse_ipv6strict_address(Addr) ->
    inet_parse:ipv6strict_address(Addr).

-spec parse_address(Address) ->
	{ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address().
parse_address(Addr) ->
    inet_parse:address(Addr).

-spec parse_strict_address(Address) ->
	{ok, IPAddress} | {error, einval} when
      Address :: string(),
      IPAddress :: ip_address().
parse_strict_address(Addr) ->
    inet_parse:strict_address(Addr).

%% Return a list of available options
options() ->
    [
     tos, priority, reuseaddr, keepalive, dontroute, linger,
     broadcast, sndbuf, recbuf, nodelay, ipv6_v6only,
     buffer, header, active, packet, deliver, mode,
     multicast_if, multicast_ttl, multicast_loop,
     exit_on_close, high_watermark, low_watermark,
     high_msgq_watermark, low_msgq_watermark,
     send_timeout, send_timeout_close
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
    [tos, priority, reuseaddr, keepalive, linger, sndbuf, recbuf, nodelay,
     header, active, packet, packet_size, buffer, mode, deliver,
     exit_on_close, high_watermark, low_watermark, high_msgq_watermark,
     low_msgq_watermark, send_timeout, send_timeout_close, delay_send, raw].
    
connect_options(Opts, Family) ->
    BaseOpts = 
	case application:get_env(kernel, inet_default_connect_options) of
	    {ok,List} when is_list(List) ->
		NList = [{active, true} | lists:keydelete(active,1,List)],     
		#connect_opts{ opts = NList};
	    {ok,{active,_Bool}} -> 
		#connect_opts{ opts = [{active,true}]};
	    {ok,Option} -> 
		#connect_opts{ opts = [{active,true}, Option]};
	    _ ->
		#connect_opts{ opts = [{active,true}]}
	end,
    case con_opt(Opts, BaseOpts, connect_options()) of
	{ok, R} ->
	    {ok, R#connect_opts {
		   ifaddr = translate_ip(R#connect_opts.ifaddr, Family)
		  }};
	Error -> Error	    
    end.

con_opt([{raw,A,B,C}|Opts],R,As) ->
    con_opt([{raw,{A,B,C}}|Opts],R,As);
con_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}     -> con_opt(Opts, R#connect_opts { ifaddr = IP }, As);
	{ifaddr,IP} -> con_opt(Opts, R#connect_opts { ifaddr = IP }, As);
	{port,P}    -> con_opt(Opts, R#connect_opts { port = P }, As);
	{fd,Fd}     -> con_opt(Opts, R#connect_opts { fd = Fd }, As);
	binary      -> con_add(mode, binary, R, Opts, As);
	list        -> con_add(mode, list, R, Opts, As);
	{tcp_module,_}  -> con_opt(Opts, R, As);
	inet        -> con_opt(Opts, R, As);
	inet6       -> con_opt(Opts, R, As);
	{Name,Val} when is_atom(Name) -> con_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
con_opt([], R, _) ->
    {ok, R}.

con_add(Name, Val, R, Opts, AllOpts) ->
    case add_opt(Name, Val, R#connect_opts.opts, AllOpts) of
	{ok, SOpts} ->
	    con_opt(Opts, R#connect_opts { opts = SOpts }, AllOpts);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:listen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen_options() ->
    [tos, priority, reuseaddr, keepalive, linger, sndbuf, recbuf, nodelay,
     header, active, packet, buffer, mode, deliver, backlog, ipv6_v6only,
     exit_on_close, high_watermark, low_watermark, high_msgq_watermark,
     low_msgq_watermark, send_timeout, send_timeout_close, delay_send,
     packet_size, raw].

listen_options(Opts, Family) ->
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
		   ifaddr = translate_ip(R#listen_opts.ifaddr, Family)
		  }};
	Error -> Error
    end.
	
list_opt([{raw,A,B,C}|Opts], R, As) ->
    list_opt([{raw,{A,B,C}}|Opts], R, As);
list_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}      ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);
	{ifaddr,IP}  ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);
	{port,P}     ->  list_opt(Opts, R#listen_opts { port = P }, As);
	{fd,Fd}      ->  list_opt(Opts, R#listen_opts { fd = Fd }, As);
	{backlog,BL} ->  list_opt(Opts, R#listen_opts { backlog = BL }, As);
	binary       ->  list_add(mode, binary, R, Opts, As);
	list         ->  list_add(mode, list, R, Opts, As);
	{tcp_module,_}  -> list_opt(Opts, R, As);
	inet         -> list_opt(Opts, R, As);
	inet6        -> list_opt(Opts, R, As);
	{Name,Val} when is_atom(Name) -> list_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
list_opt([], R, _SockOpts) ->
    {ok, R}.

list_add(Name, Val, R, Opts, As) ->
    case add_opt(Name, Val, R#listen_opts.opts, As) of
	{ok, SOpts} ->
	    list_opt(Opts, R#listen_opts { opts = SOpts }, As);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for udp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
udp_options() ->
    [tos, priority, reuseaddr, sndbuf, recbuf, header, active, buffer, mode, 
     deliver, ipv6_v6only,
     broadcast, dontroute, multicast_if, multicast_ttl, multicast_loop,
     add_membership, drop_membership, read_packets,raw,
     high_msgq_watermark, low_msgq_watermark].


udp_options(Opts, Family) ->
    case udp_opt(Opts, #udp_opts { }, udp_options()) of
	{ok, R} ->
	    {ok, R#udp_opts {
		   ifaddr = translate_ip(R#udp_opts.ifaddr, Family)
		  }};
	Error -> Error
    end.

udp_opt([{raw,A,B,C}|Opts], R, As) ->
    udp_opt([{raw,{A,B,C}}|Opts], R, As);
udp_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}     ->  udp_opt(Opts, R#udp_opts { ifaddr = IP }, As);
	{ifaddr,IP} ->  udp_opt(Opts, R#udp_opts { ifaddr = IP }, As);
	{port,P}    ->  udp_opt(Opts, R#udp_opts { port = P }, As);
	{fd,Fd}     ->  udp_opt(Opts, R#udp_opts { fd = Fd }, As);
	binary      ->  udp_add(mode, binary, R, Opts, As);
	list        ->  udp_add(mode, list, R, Opts, As);
	{udp_module,_} -> udp_opt(Opts, R, As);
	inet        -> udp_opt(Opts, R, As);
	inet6       -> udp_opt(Opts, R, As);
	{Name,Val} when is_atom(Name) -> udp_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
udp_opt([], R, _SockOpts) ->
    {ok, R}.

udp_add(Name, Val, R, Opts, As) ->
    case add_opt(Name, Val, R#udp_opts.opts, As) of
	{ok, SOpts} ->
	    udp_opt(Opts, R#udp_opts { opts = SOpts }, As);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for sctp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Currently supported options include:
%  (*) {mode,   list|binary}	 or just list|binary
%  (*) {active, true|false|once}
%  (*) {sctp_module, inet_sctp|inet6_sctp} or just inet|inet6
%  (*) options set via setsockopt.
%      The full list is below in sctp_options/0 .
%  All other options are currently NOT supported. In particular:
%  (*) multicast on SCTP is not (yet) supported, as it may be incompatible
%      with automatic associations;
%  (*) passing of open FDs ("fdopen") is not supported.
sctp_options() ->
[   % The following are generic inet options supported for SCTP sockets:
    mode, active, buffer, tos, priority, dontroute, reuseaddr, linger, sndbuf,
    recbuf, ipv6_v6only, high_msgq_watermark, low_msgq_watermark,

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
	{ok,#sctp_opts{ifaddr=undefined}=SO} -> 
	    {ok,SO#sctp_opts{ifaddr=Mod:translate_ip(?SCTP_DEF_IFADDR)}};
	{ok,_}=OK ->
	    OK;
	Error -> Error
    end.

sctp_opt([Opt|Opts], Mod, R, As) ->
    case Opt of
	{ip,IP} ->
	    sctp_opt_ifaddr(Opts, Mod, R, As, IP);
	{ifaddr,IP} ->
	    sctp_opt_ifaddr(Opts, Mod, R, As, IP);
	{port,Port} ->
	    case Mod:getserv(Port) of
		{ok,P} ->
		    sctp_opt(Opts, Mod, R#sctp_opts{port=P}, As);
		Error -> Error
	    end;
	{type,Type} when Type =:= seqpacket; Type =:= stream ->
	    sctp_opt(Opts, Mod, R#sctp_opts{type=Type}, As);
	binary		-> sctp_opt (Opts, Mod, R, As, mode, binary);
	list		-> sctp_opt (Opts, Mod, R, As, mode, list);
	{sctp_module,_}	-> sctp_opt (Opts, Mod, R, As); % Done with
	inet		-> sctp_opt (Opts, Mod, R, As); % Done with
	inet6		-> sctp_opt (Opts, Mod, R, As); % Done with
	{Name,Val}	-> sctp_opt (Opts, Mod, R, As, Name, Val);
	_ -> {error,badarg}
    end;
sctp_opt([], _Mod, #sctp_opts{ifaddr=IfAddr}=R, _SockOpts) ->
    if is_list(IfAddr) ->
	    {ok, R#sctp_opts{ifaddr=lists:reverse(IfAddr)}};
       true ->
	    {ok, R}
    end.

sctp_opt(Opts, Mod, R, As, Name, Val) ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Util to check and insert option in option list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_opt(Name, Val, Opts, As) ->
    case lists:member(Name, As) of
	true ->
	    case prim_inet:is_sockopt_val(Name, Val) of
		true ->
		    Opts1 = lists:keydelete(Name, 1, Opts),
		    {ok, [{Name,Val} | Opts1]};
		false -> {error,badarg}
	    end;
	false -> {error,badarg}
    end.
	

translate_ip(any,      inet) -> {0,0,0,0};
translate_ip(loopback, inet) -> {127,0,0,1};
translate_ip(any,      inet6) -> {0,0,0,0,0,0,0,0};
translate_ip(loopback, inet6) -> {0,0,0,0,0,0,0,1};
translate_ip(IP, _) -> IP.


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
    case inet_parse:visible_string(Address) of
	false ->
	    {error,einval};
	true ->
	    %% Address is a host name or a valid IP address,
	    %% either way check it with the resolver.
	    case gethostbyname_tm(Address, Family, Timer) of
		{ok,Ent} -> {ok,Ent#hostent.h_addr_list};
		Error -> Error
	    end
    end.

%%
%% gethostbyname with option search
%%
gethostbyname_tm(Name, Type, Timer, [string|_]=Opts) ->
    Result = gethostbyname_string(Name, Type),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [dns|_]=Opts) ->
    Result = inet_res:gethostbyname_tm(Name, Type, Timer),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [file|_]=Opts) ->
    Result = inet_hosts:gethostbyname(Name, Type),
    gethostbyname_tm(Name, Type, Timer, Opts, Result);
gethostbyname_tm(Name, Type, Timer, [yp|_]=Opts) ->
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [nis|_]=Opts) ->
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [nisplus|_]=Opts) ->
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [wins|_]=Opts) ->
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [native|_]=Opts) ->
    gethostbyname_tm_native(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, [_|_]=Opts) ->
    gethostbyname_tm(Name, Type, Timer, Opts);
%% Make sure we always can look up our own hostname.
gethostbyname_tm(Name, Type, Timer, []) ->
    Result = gethostbyname_self(Name, Type),
    gethostbyname_tm(Name, Type, Timer, [], Result).

gethostbyname_tm(Name, Type, Timer, Opts, Result) ->
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
    %% Fixme: add (global) timeout to gethost_native
    Result = inet_gethost_native:gethostbyname(Name, Type),
    gethostbyname_tm(Name, Type, Timer, Opts, Result).



gethostbyname_self(Name, Type) when is_atom(Name) ->
    gethostbyname_self(atom_to_list(Name), Type);
gethostbyname_self(Name, Type)
  when is_list(Name), Type =:= inet;
       is_list(Name), Type =:= inet6 ->
    N = inet_db:tolower(Name),
    Self = inet_db:gethostname(),
    %%
    %% This is the final fallback that pretends /etc/hosts has got
    %% a line for the hostname on the loopback address.
    %% Lookups into /etc/hosts are case insensitive and return
    %% what is in the file. Therefore the letter case may differ between
    %% the returned hostent record and the hostname that was asked for.
    %%
    case inet_db:tolower(Self) of
	N ->
	    {ok,
	     make_hostent(
	       Self, [translate_ip(loopback, Type)], [], Type)};
	_ ->
	    case inet_db:res_option(domain) of
		"" ->
		    {error,nxdomain};
		Domain ->
		    FQDN = lists:append([Self,".",Domain]),
		    case inet_db:tolower(FQDN) of
			N ->
			    {ok,
			     make_hostent(
			       FQDN,
			       [translate_ip(loopback, Type)], [], Type)};
			_ ->
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
		%% XXX should we really translate IPv4 addresses here
		%% even if we do not know if this host can do IPv6?
		inet_parse:ipv6_address(Name)
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

-spec open(Fd :: integer(),
	   Addr :: ip_address(),
	   Port :: port_number(),
	   Opts :: [socket_setopt()],
	   Protocol :: socket_protocol(),
	   Family :: address_family(),
	   Type :: socket_type(),
	   Module :: atom()) ->
	{'ok', socket()} | {'error', posix()}.

open(Fd, Addr, Port, Opts, Protocol, Family, Type, Module) when Fd < 0 ->
    case prim_inet:open(Protocol, Family, Type) of
	{ok,S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    case if is_list(Addr) ->
				 bindx(S, Addr, Port);
			    true ->
				 prim_inet:bind(S, Addr, Port)
			 end of
			{ok, _} -> 
			    inet_db:register_socket(S, Module),
			    {ok,S};
			Error  ->
			    prim_inet:close(S),
			    Error
		    end;
		Error  ->
		    prim_inet:close(S),
		    Error
	    end;
	Error ->
	    Error
    end;
open(Fd, _Addr, _Port, Opts, Protocol, Family, Type, Module) ->
    fdopen(Fd, Opts, Protocol, Family, Type, Module).

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

fdopen(Fd, Opts, Protocol, Family, Type, Module) ->
    case prim_inet:fdopen(Protocol, Family, Type, Fd) of
	{ok, S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    inet_db:register_socket(S, Module),
		    {ok, S};
		Error ->
		    prim_inet:close(S), Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  socket stat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i() -> i(tcp), i(udp), i(sctp).

i(Proto) -> i(Proto, [port, module, recv, sent, owner,
		      local_address, foreign_address, state, type]).

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
%%
fmt_status(Flags) ->
    case lists:sort(Flags) of
	[accepting | _]               -> "ACCEPTING";
	[bound,busy,connected|_]      -> "CONNECTED*";
	[bound,connected|_]           -> "CONNECTED";
	[bound,listen,listening | _]  -> "LISTENING";
	[bound,listen | _]            -> "LISTEN";
	[bound,connecting | _]        -> "CONNECTING";
	[bound,open]                  -> "BOUND";
	[open]                        -> "IDLE";
	[]                            -> "CLOSED";
	_                             -> "????"
    end.

fmt_addr({error,enotconn}, _) -> "*:*";
fmt_addr({error,_}, _)        -> " ";
fmt_addr({ok,Addr}, Proto) ->
    case Addr of
	%%Dialyzer {0,0}            -> "*:*";
	{{0,0,0,0},Port} -> "*:" ++ fmt_port(Port, Proto);
	{{0,0,0,0,0,0,0,0},Port} -> "*:" ++ fmt_port(Port, Proto);
	{{127,0,0,1},Port} -> "localhost:" ++ fmt_port(Port, Proto);
	{{0,0,0,0,0,0,0,1},Port} -> "localhost:" ++ fmt_port(Port, Proto);
	{IP,Port} -> inet_parse:ntoa(IP) ++ ":" ++ fmt_port(Port, Proto)
    end.

fmt_port(N, Proto) ->
    case inet:getservbyport(N, Proto) of
	{ok, Name} -> Name;
	_ -> integer_to_list(N)
    end.

%% Return a list of all tcp sockets
tcp_sockets() -> port_list("tcp_inet").
udp_sockets() -> port_list("udp_inet").
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
		    case A0 of
			false -> ok;
			_ -> prim_inet:setopt(S, active, false)
		    end,
		    case tcp_sync_input(S, NewOwner, false) of
			true ->  %% socket already closed, 
			    ok;
			false ->
			    try erlang:port_connect(S, NewOwner) of
				true -> 
				    unlink(S), %% unlink from port
				    case A0 of
					false -> ok;
					_ -> prim_inet:setopt(S, active, A0)
				    end,
				    ok
			    catch
				error:Reason -> 
				    {error, Reason}
			    end
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
	    prim_inet:setopt(S, active, false),
	    udp_sync_input(S, NewOwner),
	    try erlang:port_connect(S, NewOwner) of
		true -> 
		    unlink(S),
		    prim_inet:setopt(S, active, A0),
		    ok
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
