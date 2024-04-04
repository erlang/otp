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
-module(gen_udp).

-export([open/1, open/2, close/1]).
-export([send/2, send/3, send/4, send/5, recv/2, recv/3, connect/2, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").

-define(module_socket(Handler, Handle),
        {'$inet', (Handler), (Handle)}).

%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

-type option() ::
        {active,          true | false | once | -32768..32767} |
        {add_membership,  membership()} |
        {broadcast,       boolean()} |
        {buffer,          non_neg_integer()} |
        {debug,           boolean()} |
        {deliver,         port | term} |
        {dontroute,       boolean()} |
        {drop_membership, membership()} |
        {exclusiveaddruse, boolean()} |
        {header,          non_neg_integer()} |
        {high_msgq_watermark, pos_integer()} |
        {low_msgq_watermark, pos_integer()} |
        {mode,            list | binary} | list | binary |
        {multicast_if,    multicast_if()} |
        {multicast_loop,  boolean()} |
        {multicast_ttl,   non_neg_integer()} |
        {priority,        non_neg_integer()} |
        {raw,
         Protocol :: non_neg_integer(),
         OptionNum :: non_neg_integer(),
         ValueBin :: binary()} |
        {read_packets,    non_neg_integer()} |
        {recbuf,          non_neg_integer()} |
        {reuseaddr,       boolean()} |
        {reuseport,       boolean()} |
        {reuseport_lb,    boolean()} |
        {sndbuf,          non_neg_integer()} |
        {tos,             non_neg_integer()} |
        {tclass,          non_neg_integer()} |
        {ttl,             non_neg_integer()} |
	{recvtos,         boolean()} |
	{recvtclass,      boolean()} |
	{recvttl,         boolean()} |
	{ipv6_v6only,     boolean()}.
-type option_name() ::
        active |
        broadcast |
        buffer |
        debug |
        deliver |
        dontroute |
        exclusiveaddruse |
        header |
        high_msgq_watermark |
        low_msgq_watermark |
        mode |
        multicast_if |
        multicast_loop |
        multicast_ttl |
        priority |
        {raw,
         Protocol :: non_neg_integer(),
         OptionNum :: non_neg_integer(),
         ValueSpec :: (ValueSize :: non_neg_integer()) |
                      (ValueBin :: binary())} |
        read_packets |
        recbuf |
        reuseaddr |
        reuseport |
        reuseport_lb |
        sndbuf |
        tos |
        tclass |
        ttl |
        recvtos |
        recvtclass |
        recvttl |
        pktoptions |
	ipv6_v6only.

-type open_option() :: {ip,             inet:socket_address()}
                     | {fd,             non_neg_integer()}
                     | {ifaddr,         socket:sockaddr_in() |
                                        socket:sockaddr_in6() |
                                        inet:socket_address()}
                     | inet:address_family()
                     | {port,           inet:port_number()}
                     | {netns,          file:filename_all()}
                     | {bind_to_device, binary()}
                     | option().

-type socket() :: inet:socket().

-type ip_multicast_if()  :: inet:ip4_address().
-type ip6_multicast_if() :: integer(). % interface index
-type multicast_if()     :: ip_multicast_if() | ip6_multicast_if().


%% Note that for IPv4, the tuple with size 3 is *not*
%% supported on all platforms.
%% 'ifindex' defaults to zero (0) on platforms that
%% supports the 3-tuple variant.
-type ip_membership()  :: {MultiAddress :: inet:ip4_address(),    % multiaddr
                           Interface    :: inet:ip4_address()} |  % local addr
                          {MultiAddress :: inet:ip4_address(),    % multiaddr
                           Address      :: inet:ip4_address(),    % local addr
                           IfIndex      :: integer()}.            % ifindex
-type ip6_membership() :: {MultiAddress :: inet:ip6_address(),    % multiaddr
                           IfIndex      :: integer()}.            % ifindex
-type membership()     :: ip_membership() | ip6_membership().

-export_type([option/0, open_option/0, option_name/0, socket/0,
              multicast_if/0, ip_multicast_if/0, ip6_multicast_if/0,
              membership/0, ip_membership/0, ip6_membership/0]).


%% -- open ------------------------------------------------------------------

-spec open(Port) -> {ok, Socket} | {error, Reason} when
      Port   :: inet:port_number(),
      Socket :: socket(),
      Reason :: system_limit | inet:posix().

open(Port) -> 
    open(Port, []).

-spec open(Port, Opts) -> {ok, Socket} | {error, Reason} when
      Port   :: inet:port_number(),
      Opts   :: [inet:inet_backend() | open_option()],
      Socket :: socket(),
      Reason :: system_limit | inet:posix().

open(Port, Opts0) ->
    %% ?DBG(['entry', {port, Port}, {opts0, Opts0}]),
    case inet:gen_udp_module(Opts0) of
	{?MODULE, Opts} ->
	    open1(Port, Opts);
	{GenUdpMod, Opts} ->
	    GenUdpMod:open(Port, Opts)
    end.

open1(Port, Opts0) ->
    %% ?DBG(['entry', {port, Port}, {opts0, Opts0}]),
    {Mod, Opts} = inet:udp_module(Opts0),
    %% ?DBG([{mod, Mod}, {opts, Opts}]),
    {ok, UP} = Mod:getserv(Port),
    %% ?DBG([{up, UP}]),
    Mod:open(UP, Opts).
    

%% -- close -----------------------------------------------------------------

-spec close(Socket) -> ok when
      Socket :: socket().

close(?module_socket(GenUdpMod, _) = S) when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S);
close(S) ->
    inet:udp_close(S).


%% -- send ------------------------------------------------------------------

%% Connected send

-spec send(Socket, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix().

send(?module_socket(GenUdpMod, _) = S, Packet)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Packet);
send(S, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:send(S, Packet);
	Error ->
	    Error
    end.

-spec send(Socket, Destination, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Destination :: {inet:ip_address(), inet:port_number()} |
		     inet:family_address() |
                     socket:sockaddr_in() | socket:sockaddr_in6(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix().

send(?module_socket(GenUdpMod, _) = S, Destination, Packet)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Destination, Packet);
send(Socket, Destination, Packet) ->
    send(Socket, Destination, [], Packet).

-spec send(Socket, Host, Port, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Host :: inet:hostname() | inet:ip_address(),
      Port :: inet:port_number() | atom(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix();
%%%
          (Socket, Destination, AncData, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Destination :: {inet:ip_address(), inet:port_number()} |
                     inet:family_address() |
                     socket:sockaddr_in() | socket:sockaddr_in6(),
      AncData :: inet:ancillary_data(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix();
%%%
          (Socket, Destination, PortZero, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Destination :: {inet:ip_address(), inet:port_number()} |
                     inet:family_address(),
      PortZero :: inet:port_number(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix().

send(?module_socket(GenUdpMod, _) = S, Arg2, Arg3, Packet)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Arg2, Arg3, Packet);

send(S, #{family := Fam} = Destination, AncData, Packet)
  when is_port(S) andalso
       ((Fam =:= inet) orelse (Fam =:= inet6)) andalso
       is_list(AncData) ->
    case inet_db:lookup_socket(S) of
        {ok, Mod} ->
            Mod:send(S, inet:ensure_sockaddr(Destination), AncData, Packet);
        Error ->
            Error
    end;
send(S, {_,_} = Destination, PortZero = AncData, Packet) when is_port(S) ->
    %% Destination is {Family,Addr} | {IP,Port},
    %% so it is complete - argument PortZero is redundant
    if
        PortZero =:= 0 ->
            case inet_db:lookup_socket(S) of
                {ok, Mod} ->
                    Mod:send(S, Destination, [], Packet);
                Error ->
                    Error
            end;
        is_integer(PortZero) ->
            %% Redundant PortZero; must be 0
            {error, einval};
        is_list(AncData) ->
            case inet_db:lookup_socket(S) of
                {ok, Mod} ->
                    Mod:send(S, Destination, AncData, Packet);
                Error ->
                    Error
            end
    end;
send(S, Host, Port, Packet) when is_port(S) ->
    send(S, Host, Port, [], Packet).

-spec send(Socket, Host, Port, AncData, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Host :: inet:hostname() | inet:ip_address() | inet:local_address(),
      Port :: inet:port_number() | atom(),
      AncData :: inet:ancillary_data(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix().

send(?module_socket(GenUdpMod, _) = S, Host, Port, AncData, Packet)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Host, Port, AncData, Packet);

send(S, Host, Port, AncData, Packet)
  when is_port(S), is_list(AncData) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    case Mod:getaddr(Host) of
		{ok,IP} ->
		    case Mod:getserv(Port) of
			{ok,P} -> Mod:send(S, {IP,P}, AncData, Packet);
			{error,einval} -> exit(badarg);
			Error -> Error
		    end;
		{error,einval} -> exit(badarg);
		Error -> Error
	    end;
	Error ->
	    Error
    end.


%% -- recv ------------------------------------------------------------------

-spec recv(Socket, Length) ->
                  {ok, RecvData} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      RecvData :: {Address, Port, Packet} | {Address, Port, AncData, Packet},
      Address :: inet:ip_address() | inet:returned_non_ip_address(),
      Port :: inet:port_number(),
      AncData :: inet:ancillary_data(),
      Packet :: string() | binary(),
      Reason :: not_owner | inet:posix().

recv(?module_socket(GenUdpMod, _) = S, Len)
  when is_atom(GenUdpMod) andalso is_integer(Len) ->
    GenUdpMod:?FUNCTION_NAME(S, Len);
recv(S, Len) when is_port(S) andalso is_integer(Len) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Len);
	Error ->
	    Error
    end.

-spec recv(Socket, Length, Timeout) ->
                  {ok, RecvData} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Timeout :: timeout(),
      RecvData :: {Address, Port, Packet} | {Address, Port, AncData, Packet},
      Address :: inet:ip_address() | inet:returned_non_ip_address(),
      Port :: inet:port_number(),
      AncData :: inet:ancillary_data(),
      Packet :: string() | binary(),
      Reason :: not_owner | timeout | inet:posix().

recv(?module_socket(GenUdpMod, _) = S, Len, Time)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Len, Time);
recv(S, Len, Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Len, Time);
	Error ->
	    Error
    end.


%% -- connect ---------------------------------------------------------------

-spec connect(Socket, SockAddr) -> ok | {error, Reason} when
      Socket   :: socket(),
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Reason   :: inet:posix().

connect(S, SockAddr) when is_port(S) andalso is_map(SockAddr) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
            Mod:connect(S, inet:ensure_sockaddr(SockAddr));
	Error ->
	    Error
    end.

-spec connect(Socket, Address, Port) -> ok | {error, Reason} when
      Socket   :: socket(),
      Address  :: inet:socket_address() | inet:hostname(),
      Port     :: inet:port_number(),
      Reason   :: inet:posix().

connect(?module_socket(GenUdpMod, _) = S, Address, Port)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, Address, Port);

connect(S, Address, Port) when is_port(S) ->
    %% ?DBG([{address, Address}, {port, Port}]),
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    %% ?DBG([{mod, Mod}]),
	    case Mod:getaddr(Address) of    
		{ok, IP} ->
		    %% ?DBG([{ip, IP}]),
		    Mod:connect(S, IP, Port);
		Error ->
		    %% ?DBG(['getaddr', {error, Error}]),
		    Error
	    end;
	Error ->
	    %% ?DBG(['lookup', {error, Error}]),
	    Error
    end.


%% -- controlling_process ---------------------------------------------------

-spec controlling_process(Socket, Pid) -> ok | {error, Reason} when
      Socket :: socket(),
      Pid :: pid(),
      Reason :: closed | not_owner | badarg | inet:posix().

controlling_process(?module_socket(GenUdpMod, _) = S, NewOwner)
  when is_atom(GenUdpMod) ->
    GenUdpMod:?FUNCTION_NAME(S, NewOwner);

controlling_process(S, NewOwner) ->
    inet:udp_controlling_process(S, NewOwner).


%% -- fdopen ----------------------------------------------------------------

%%
%% Create a port/socket from a file descriptor
%%
fdopen(Fd, Opts0) ->
    {Mod,Opts} = inet:udp_module(Opts0),
    Mod:fdopen(Fd, Opts).
