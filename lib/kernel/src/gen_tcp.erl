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

-module(gen_tcp).


-export([connect/3, connect/4, listen/2, accept/1, accept/2,
	 shutdown/2, close/1]).
-export([send/2, recv/2, recv/3, unrecv/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").
-include("file.hrl").

-type option() ::
        {active,          true | false | once | -32768..32767} |
        {buffer,          non_neg_integer()} |
        {delay_send,      boolean()} |
        {deliver,         port | term} |
        {dontroute,       boolean()} |
        {exit_on_close,   boolean()} |
        {header,          non_neg_integer()} |
        {high_msgq_watermark, pos_integer()} |
        {high_watermark,  non_neg_integer()} |
        {keepalive,       boolean()} |
        {linger,          {boolean(), non_neg_integer()}} |
        {low_msgq_watermark, pos_integer()} |
        {low_watermark,   non_neg_integer()} |
        {mode,            list | binary} | list | binary |
        {nodelay,         boolean()} |
        {packet,
         0 | 1 | 2 | 4 | raw | sunrm |  asn1 |
         cdr | fcgi | line | tpkt | http | httph | http_bin | httph_bin } |
        {packet_size,     non_neg_integer()} |
        {priority,        non_neg_integer()} |
        {raw,
         Protocol :: non_neg_integer(),
         OptionNum :: non_neg_integer(),
         ValueBin :: binary()} |
        {recbuf,          non_neg_integer()} |
        {reuseaddr,       boolean()} |
        {send_timeout,    non_neg_integer() | infinity} |
        {send_timeout_close, boolean()} |
        {show_econnreset, boolean()} |
        {sndbuf,          non_neg_integer()} |
        {tos,             non_neg_integer()} |
	{ipv6_v6only,     boolean()}.
-type option_name() ::
        active |
        buffer |
        delay_send |
        deliver |
        dontroute |
        exit_on_close |
        header |
        high_msgq_watermark |
        high_watermark |
        keepalive |
        linger |
        low_msgq_watermark |
        low_watermark |
        mode |
        nodelay |
        packet |
        packet_size |
        priority |
        {raw,
         Protocol :: non_neg_integer(),
         OptionNum :: non_neg_integer(),
         ValueSpec :: (ValueSize :: non_neg_integer()) |
                      (ValueBin :: binary())} |
        recbuf |
        reuseaddr |
        send_timeout |
        send_timeout_close |
        show_econnreset |
        sndbuf |
        tos |
	ipv6_v6only.
-type connect_option() ::
        {ip, inet:socket_address()} |
        {fd, Fd :: non_neg_integer()} |
        {ifaddr, inet:socket_address()} |
        inet:address_family() |
        {port, inet:port_number()} |
        {tcp_module, module()} |
        option().
-type listen_option() ::
        {ip, inet:socket_address()} |
        {fd, Fd :: non_neg_integer()} |
        {ifaddr, inet:socket_address()} |
        inet:address_family() |
        {port, inet:port_number()} |
        {backlog, B :: non_neg_integer()} |
        {tcp_module, module()} |
        option().
-type socket() :: port().

-export_type([option/0, option_name/0, connect_option/0, listen_option/0,
              socket/0]).

%%
%% Connect a socket
%%

-spec connect(Address, Port, Options) -> {ok, Socket} | {error, Reason} when
      Address :: inet:socket_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Socket :: socket(),
      Reason :: inet:posix().

connect(Address, Port, Opts) -> 
    connect(Address,Port,Opts,infinity).

-spec connect(Address, Port, Options, Timeout) ->
                     {ok, Socket} | {error, Reason} when
      Address :: inet:socket_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Timeout :: timeout(),
      Socket :: socket(),
      Reason :: inet:posix().

connect(Address, Port, Opts, Time) ->
    Timer = inet:start_timer(Time),
    Res = (catch connect1(Address,Port,Opts,Timer)),
    _ = inet:stop_timer(Timer),
    case Res of
	{ok,S} -> {ok,S};
	{error, einval} -> exit(badarg);
	{'EXIT',Reason} -> exit(Reason);
	Error -> Error
    end.

connect1(Address, Port, Opts0, Timer) ->
    {Mod, Opts} = inet:tcp_module(Opts0, Address),
    case Mod:getaddrs(Address,Timer) of
	{ok,IPs} ->
	    case Mod:getserv(Port) of
		{ok,TP} -> try_connect(IPs,TP,Opts,Timer,Mod,{error,einval});
		Error -> Error
	    end;
	Error -> Error
    end.

try_connect([IP|IPs], Port, Opts, Timer, Mod, _) ->
    Time = inet:timeout(Timer),
    case Mod:connect(IP, Port, Opts, Time) of
	{ok,S} -> {ok,S};
	{error,einval} -> {error, einval};
	{error,timeout} -> {error,timeout};
	Err1 -> try_connect(IPs, Port, Opts, Timer, Mod, Err1)
    end;
try_connect([], _Port, _Opts, _Timer, _Mod, Err) ->
    Err.

    

%%
%% Listen on a tcp port
%%

-spec listen(Port, Options) -> {ok, ListenSocket} | {error, Reason} when
      Port :: inet:port_number(),
      Options :: [listen_option()],
      ListenSocket :: socket(),
      Reason :: system_limit | inet:posix().

listen(Port, Opts0) ->
    {Mod, Opts} = inet:tcp_module(Opts0),
    case Mod:getserv(Port) of
	{ok,TP} ->
	    Mod:listen(TP, Opts);
	{error,einval} ->
	    exit(badarg);
	Other -> Other
    end.

%%
%% Generic tcp accept
%%

-spec accept(ListenSocket) -> {ok, Socket} | {error, Reason} when
      ListenSocket :: socket(),
      Socket :: socket(),
      Reason :: closed | timeout | system_limit | inet:posix().

accept(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:accept(S);
	Error ->
	    Error
    end.

-spec accept(ListenSocket, Timeout) -> {ok, Socket} | {error, Reason} when
      ListenSocket :: socket(),
      Timeout :: timeout(),
      Socket :: socket(),
      Reason :: closed | timeout | system_limit | inet:posix().

accept(S, Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:accept(S, Time);
	Error ->
	    Error
    end.

%%
%% Generic tcp shutdown
%%

-spec shutdown(Socket, How) -> ok | {error, Reason} when
      Socket :: socket(),
      How :: read | write | read_write,
      Reason :: inet:posix().

shutdown(S, How) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:shutdown(S, How);
	Error ->
	    Error
    end.

%%
%% Close
%%

-spec close(Socket) -> ok when
      Socket :: socket().

close(S) ->
    inet:tcp_close(S).

%%
%% Send
%%

-spec send(Socket, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Packet :: iodata(),
      Reason :: closed | inet:posix().

send(S, Packet) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:send(S, Packet);
	Error ->
	    Error
    end.

%%
%% Receive data from a socket (passive mode)
%%

-spec recv(Socket, Length) -> {ok, Packet} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Packet :: string() | binary() | HttpPacket,
      Reason :: closed | inet:posix(),
      HttpPacket :: term().

recv(S, Length) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Length);
	Error ->
	    Error
    end.

-spec recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Timeout :: timeout(),
      Packet :: string() | binary() | HttpPacket,
      Reason :: closed | inet:posix(),
      HttpPacket :: term().

recv(S, Length, Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Length, Time);
	Error ->
	    Error
    end.

unrecv(S, Data) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:unrecv(S, Data);
	Error ->
	    Error
    end.

%%
%% Set controlling process
%%

-spec controlling_process(Socket, Pid) -> ok | {error, Reason} when
      Socket :: socket(),
      Pid :: pid(),
      Reason :: closed | not_owner | inet:posix().

controlling_process(S, NewOwner) ->
    case inet_db:lookup_socket(S) of
	{ok, _Mod} -> % Just check that this is an open socket
	    inet:tcp_controlling_process(S, NewOwner);
	Error ->
	    Error
    end.



%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts0) ->
    {Mod, Opts} = inet:tcp_module(Opts0),
    Mod:fdopen(Fd, Opts).
