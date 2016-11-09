%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-export([send/2, send/4, recv/2, recv/3, connect/3]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").

-type option() ::
        {active,          true | false | once | -32768..32767} |
        {add_membership,  {inet:ip_address(), inet:ip_address()}} |
        {broadcast,       boolean()} |
        {buffer,          non_neg_integer()} |
        {deliver,         port | term} |
        {dontroute,       boolean()} |
        {drop_membership, {inet:ip_address(), inet:ip_address()}} |
        {header,          non_neg_integer()} |
        {high_msgq_watermark, pos_integer()} |
        {low_msgq_watermark, pos_integer()} |
        {mode,            list | binary} | list | binary |
        {multicast_if,    inet:ip_address()} |
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
        {sndbuf,          non_neg_integer()} |
        {tos,             non_neg_integer()} |
	{ipv6_v6only,     boolean()}.
-type option_name() ::
        active |
        broadcast |
        buffer |
        deliver |
        dontroute |
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
        sndbuf |
        tos |
	ipv6_v6only.
-type socket() :: port().

-export_type([option/0, option_name/0, socket/0]).

-spec open(Port) -> {ok, Socket} | {error, Reason} when
      Port :: inet:port_number(),
      Socket :: socket(),
      Reason :: inet:posix().

open(Port) -> 
    open(Port, []).

-spec open(Port, Opts) -> {ok, Socket} | {error, Reason} when
      Port :: inet:port_number(),
      Opts :: [Option],
      Option :: {ip, inet:socket_address()}
              | {fd, non_neg_integer()}
              | {ifaddr, inet:socket_address()}
              | inet:address_family()
              | {port, inet:port_number()}
              | option(),
      Socket :: socket(),
      Reason :: inet:posix().

open(Port, Opts0) ->
    {Mod, Opts} = inet:udp_module(Opts0),
    {ok, UP} = Mod:getserv(Port),
    Mod:open(UP, Opts).

-spec close(Socket) -> ok when
      Socket :: socket().

close(S) ->
    inet:udp_close(S).

-spec send(Socket, Address, Port, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Address :: inet:socket_address() | inet:hostname(),
      Port :: inet:port_number(),
      Packet :: iodata(),
      Reason :: not_owner | inet:posix().

send(S, Address, Port, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    case Mod:getaddr(Address) of
		{ok,IP} ->
		    case Mod:getserv(Port) of
			{ok,UP} -> Mod:send(S, IP, UP, Packet);
			{error,einval} -> exit(badarg);
			Error -> Error
		    end;
		{error,einval} -> exit(badarg);
		Error -> Error
	    end;
	Error ->
	    Error
    end.

send(S, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:send(S, Packet);
	Error ->
	    Error
    end.

-spec recv(Socket, Length) ->
                  {ok, {Address, Port, Packet}} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Address :: inet:ip_address() | inet:returned_non_ip_address(),
      Port :: inet:port_number(),
      Packet :: string() | binary(),
      Reason :: not_owner | inet:posix().

recv(S,Len) when is_port(S), is_integer(Len) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Len);
	Error ->
	    Error
    end.

-spec recv(Socket, Length, Timeout) ->
                  {ok, {Address, Port, Packet}} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Timeout :: timeout(),
      Address :: inet:ip_address() | inet:returned_non_ip_address(),
      Port :: inet:port_number(),
      Packet :: string() | binary(),
      Reason :: not_owner | inet:posix().

recv(S,Len,Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Len,Time);
	Error ->
	    Error
    end.

connect(S, Address, Port) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    case Mod:getaddr(Address) of    
		{ok, IP} ->
		    Mod:connect(S, IP, Port);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

-spec controlling_process(Socket, Pid) -> ok | {error, Reason} when
      Socket :: socket(),
      Pid :: pid(),
      Reason :: closed | not_owner | badarg | inet:posix().

controlling_process(S, NewOwner) ->
    inet:udp_controlling_process(S, NewOwner).

%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts0) ->
    {Mod,Opts} = inet:udp_module(Opts0),
    Mod:fdopen(Fd, Opts).
