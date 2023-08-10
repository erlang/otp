%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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


-export([connect/2, connect/3, connect/4,
         listen/2,
         accept/1, accept/2,
	 shutdown/2, close/1]).
-export([send/2, recv/2, recv/3, unrecv/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").
-include("file.hrl").

-define(module_socket(Handler, Handle),
        {'$inet', (Handler), (Handle)}).

-type option() ::
        {active,          true | false | once | -32768..32767} |
        {buffer,          non_neg_integer()} |
        {debug,           boolean()} |
        {delay_send,      boolean()} |
        {deliver,         port | term} |
        {dontroute,       boolean()} |
        {exit_on_close,   boolean()} |
        {exclusiveaddruse, boolean()} |
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
        {reuseport,       boolean()} |
        {reuseport_lb,    boolean()} |
        {send_timeout,    non_neg_integer() | infinity} |
        {send_timeout_close, boolean()} |
        {show_econnreset, boolean()} |
        {sndbuf,          non_neg_integer()} |
        {tos,             non_neg_integer()} |
        {tclass,          non_neg_integer()} |
        {ttl,             non_neg_integer()} |
	{recvtos,         boolean()} |
	{recvtclass,      boolean()} |
	{recvttl,         boolean()} |
	{ipv6_v6only,     boolean()}.
-type pktoptions_value() ::
        {pktoptions, inet:ancillary_data()}.
-type option_name() ::
        active |
        buffer |
        debug |
        delay_send |
        deliver |
        dontroute |
        exit_on_close |
        exclusiveaddruse |
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
        reuseport |
        reuseport_lb |
        send_timeout |
        send_timeout_close |
        show_econnreset |
        sndbuf |
        tos |
        tclass |
        ttl |
        recvtos |
        recvtclass |
        recvttl |
        pktoptions |
	ipv6_v6only.
-type connect_option() ::
        {fd, Fd :: non_neg_integer()} |
        inet:address_family() |
        {ifaddr, socket:sockaddr_in() | socket:sockaddr_in6() |
                 inet:socket_address()} |
        {ip, inet:socket_address()} |
        {port, inet:port_number()} |
        {tcp_module, module()} |
        {netns, file:filename_all()} |
        {bind_to_device, binary()} |
        option().
-type listen_option() ::
        {fd, Fd :: non_neg_integer()} |
        inet:address_family() |
        {ifaddr, socket:sockaddr_in() | socket:sockaddr_in6() |
                 inet:socket_address()} |
        {ip, inet:socket_address()} |
        {port, inet:port_number()} |
        {backlog, B :: non_neg_integer()} |
        {tcp_module, module()} |
        {netns, file:filename_all()} |
        {bind_to_device, binary()} |
        option().
-type socket() :: inet:socket().

-export_type([option/0, option_name/0, connect_option/0, listen_option/0,
              socket/0, pktoptions_value/0]).


%% -define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).


%%
%% Connect a socket
%%

-spec connect(SockAddr, Opts) -> {ok, Socket} | {error, Reason} when
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: [inet:inet_backend() | connect_option()],
      Socket   :: socket(),
      Reason   :: inet:posix().

connect(SockAddr, Opts) ->
    connect(SockAddr, Opts, infinity).

-spec connect(Address, Port, Opts) -> {ok, Socket} | {error, Reason} when
      Address  :: inet:socket_address() | inet:hostname(),
      Port     :: inet:port_number(),
      Opts     :: [inet:inet_backend() | connect_option()],
      Socket   :: socket(),
      Reason   :: inet:posix();
             (SockAddr, Opts, Timeout) -> {ok, Socket} | {error, Reason} when
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: [inet:inet_backend() | connect_option()],
      Timeout  :: timeout(),
      Socket   :: socket(),
      Reason   :: timeout | inet:posix().

connect(Address, Port, Opts)
  when is_tuple(Address) orelse
       is_list(Address)  orelse
       is_atom(Address)  orelse
       (Address =:= any) orelse
       (Address =:= loopback) ->
    connect(Address, Port, Opts, infinity);
connect(#{family := Fam} = SockAddr, Opts, Timeout)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) ->
    %% Ensure that its a proper sockaddr_in6, with all fields
    %% ?DBG([{fam, Fam}, {sa, SockAddr}, {opts, Opts}, {timeout, Timeout}]),
    SockAddr2 = inet:ensure_sockaddr(SockAddr),
    %% ?DBG([{sa2, SockAddr2}]),
    case inet:gen_tcp_module(Opts) of
        {?MODULE, Opts2} ->
            Timer = inet:start_timer(Timeout),
            Res = (catch connect2(SockAddr2, Opts2, Timer)),
            _ = inet:stop_timer(Timer),
            case Res of
                {ok, S}          -> {ok,S};
                {error,  einval} -> exit(badarg);
                {'EXIT', Reason} -> exit(Reason);
                Error            -> Error
            end;
        {GenTcpMod, Opts3} ->
            GenTcpMod:connect(SockAddr2, Opts3, Timeout)
    end.


-spec connect(Address, Port, Opts, Timeout) ->
                     {ok, Socket} | {error, Reason} when
      Address :: inet:socket_address() | inet:hostname(),
      Port    :: inet:port_number(),
      Opts    :: [inet:inet_backend() | connect_option()],
      Timeout :: timeout(),
      Socket  :: socket(),
      Reason  :: timeout | inet:posix().

connect(Address, Port, Opts0, Timeout) ->
    case inet:gen_tcp_module(Opts0) of
        {?MODULE, Opts} ->
            Timer = inet:start_timer(Timeout),
            Res = (catch connect1(Address,Port,Opts,Timer)),
            _ = inet:stop_timer(Timer),
            case Res of
                {ok,S} -> {ok,S};
                {error, einval} -> exit(badarg);
                {'EXIT',Reason} -> exit(Reason);
                Error -> Error
            end;
        {GenTcpMod, Opts} ->
            GenTcpMod:connect(Address, Port, Opts, Timeout)
    end.

connect1(Address, Port, Opts0, Timer) ->
    {Mod, Opts} = inet:tcp_module(Opts0, Address),
    case Mod:getaddrs(Address, Timer) of
	{ok,IPs} ->
	    case Mod:getserv(Port) of
		{ok,TP} -> try_connect(IPs,TP,Opts,Timer,Mod,{error,einval});
		Error -> Error
	    end;
	Error -> Error
    end.

connect2(SockAddr, Opts0, Timer) ->
    {Mod, Opts} = inet:tcp_module(Opts0, SockAddr),
    Mod:connect(SockAddr, Opts, inet:timeout(Timer)).

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
      Options :: [inet:inet_backend() | listen_option()],
      ListenSocket :: socket(),
      Reason :: system_limit | inet:posix().

listen(Port, Opts0) ->
    %% ?DBG([{port, Port}, {opts0, Opts0}]),
    case inet:gen_tcp_module(Opts0) of
        {?MODULE, Opts1} ->
            %% ?DBG([{opts1, Opts1}]),
            {Mod, Opts} = inet:tcp_module(Opts1),
            %% ?DBG([{mod, Mod}, {opts, Opts}]),
            case Mod:getserv(Port) of
                {ok, TP} ->
                    %% ?DBG([{tp, TP}]),
                    Mod:listen(TP, Opts);
                {error,einval} ->
                    %% ?DBG("failed getserv (einval)"),
                    exit(badarg);
                Other -> Other
            end;
        {GenTcpMod, Opts} ->
            GenTcpMod:listen(Port, Opts)
    end.

%%
%% Generic tcp accept
%%

-spec accept(ListenSocket) -> {ok, Socket} | {error, Reason} when
      ListenSocket :: socket(),
      Socket :: socket(),
      Reason :: closed | system_limit | inet:posix().

accept(?module_socket(GenTcpMod, _) = S) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, infinity);
accept(S) when is_port(S) ->
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

accept(?module_socket(GenTcpMod, _) = S, Time) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, Time);
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

shutdown(?module_socket(GenTcpMod, _) = S, How) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, How);
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

close(?module_socket(GenTcpMod, _) = S) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S);
close(S) ->
    inet:tcp_close(S).

%%
%% Send
%%

-spec send(Socket, Packet) -> ok | {error, Reason} when
      Socket :: socket(),
      Packet :: iodata(),
      Reason :: closed | {timeout, RestData} | inet:posix(),
      RestData :: binary().

send(?module_socket(GenTcpMod, _) = S, Packet) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, Packet);
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

recv(?module_socket(GenTcpMod, _) = S, Length) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, Length, infinity);
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
      Reason :: closed | timeout | inet:posix(),
      HttpPacket :: term().

recv(?module_socket(GenTcpMod, _) = S, Length, Time) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, Length, Time);
recv(S, Length, Time) when is_port(S) ->
    case inet_db:lookup_socket(S) of
	{ok, Mod} ->
	    Mod:recv(S, Length, Time);
	Error ->
	    Error
    end.

unrecv(?module_socket(GenTcpMod, _) = S, Data) when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, Data);
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
      Reason :: closed | not_owner | badarg | inet:posix().

controlling_process(?module_socket(GenTcpMod, _) = S, NewOwner)
  when is_atom(GenTcpMod) ->
    GenTcpMod:?FUNCTION_NAME(S, NewOwner);
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
    case inet:gen_tcp_module(Opts0) of
        {?MODULE, Opts1} ->
            {Mod, Opts} = inet:tcp_module(Opts1),
            Mod:fdopen(Fd, Opts);
        {GenTcpMod, Opts} ->
            GenTcpMod:fdopen(Fd, Opts)
    end.
