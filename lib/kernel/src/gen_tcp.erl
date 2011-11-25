%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-module(gen_tcp).


-export([connect/3, connect/4, listen/2, accept/1, accept/2,
	 shutdown/2, close/1]).
-export([send/2, recv/2, recv/3, unrecv/2, sendfile/2, sendfile/5]).
-export([controlling_process/2]).
-export([fdopen/2]).

-include("inet_int.hrl").
-include("file.hrl").

-type option() ::
        {active,          true | false | once} |
        {bit8,            clear | set | on | off} |
        {buffer,          non_neg_integer()} |
        {delay_send,      boolean()} |
        {deliver,         port | term} |
        {dontroute,       boolean()} |
        {exit_on_close,   boolean()} |
        {header,          non_neg_integer()} |
        {high_watermark,  non_neg_integer()} |
        {keepalive,       boolean()} |
        {linger,          {boolean(), non_neg_integer()}} |
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
        {sndbuf,          non_neg_integer()} |
        {tos,             non_neg_integer()}.
-type option_name() ::
        active |
        bit8 |
        buffer |
        delay_send |
        deliver |
        dontroute |
        exit_on_close |
        header |
        high_watermark |
        keepalive |
        linger |
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
        sndbuf |
        tos.
-type connect_option() ::
        {ip, inet:ip_address()} |
        {fd, Fd :: non_neg_integer()} |
        {ifaddr, inet:ip_address()} |
        inet:address_family() |
        {port, inet:port_number()} |
        {tcp_module, module()} |
        option().
-type listen_option() ::
        {ip, inet:ip_address()} |
        {fd, Fd :: non_neg_integer()} |
        {ifaddr, inet:ip_address()} |
        inet:address_family() |
        {port, inet:port_number()} |
        {backlog, B :: non_neg_integer()} |
        {tcp_module, module()} |
        option().
-type socket() :: port().
-type sendfile_option() :: {chunk_size, non_neg_integer()} |
			   {headers, Hdrs :: list(iodata())} |
			   {trailers, Tlrs :: list(iodata())} |
			   sf_nodiskio | sf_mnowait | sf_sync.

-export_type([option/0, option_name/0, connect_option/0, listen_option/0,
	      sendfile_option/0]).

%%
%% Connect a socket
%%

-spec connect(Address, Port, Options) -> {ok, Socket} | {error, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Socket :: socket(),
      Reason :: inet:posix().

connect(Address, Port, Opts) -> 
    connect(Address,Port,Opts,infinity).

-spec connect(Address, Port, Options, Timeout) ->
                     {ok, Socket} | {error, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [connect_option()],
      Timeout :: timeout(),
      Socket :: socket(),
      Reason :: inet:posix().

connect(Address, Port, Opts, Time) ->
    Timer = inet:start_timer(Time),
    Res = (catch connect1(Address,Port,Opts,Timer)),
    inet:stop_timer(Timer),
    case Res of
	{ok,S} -> {ok,S};
	{error, einval} -> exit(badarg);
	{'EXIT',Reason} -> exit(Reason);
	Error -> Error
    end.

connect1(Address,Port,Opts,Timer) ->
    Mod = mod(Opts, Address),
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
      Reason :: inet:posix().

listen(Port, Opts) ->
    Mod = mod(Opts, undefined),
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
      Reason :: closed | timeout | inet:posix().

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
      Reason :: closed | timeout | inet:posix().

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
      Reason :: inet:posix().

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
%% Send data using sendfile
%%

-define(MAX_CHUNK_SIZE, (1 bsl 20)*20). %% 20 MB, has to fit in primary memory

-spec sendfile(File, Sock, Offset, Bytes, Opts) ->
   {'ok', non_neg_integer()} | {'error', inet:posix()} when
      File :: file:io_device(),
      Sock :: socket(),
      Offset :: non_neg_integer(),
      Bytes :: non_neg_integer(),
      Opts :: [sendfile_option()].
sendfile(File, Sock, Offset, Bytes, Opts) when is_pid(File) ->
    Ref = erlang:monitor(process, File),
    File ! {file_request,self(),File,
	    {sendfile,Sock,Offset,Bytes,Opts}},
    receive
	{file_reply,File,Reply} ->
	    erlang:demonitor(Ref,[flush]),
	    Reply;
	{'DOWN', Ref, _, _, _} ->
	    {error, terminated}
    end;
sendfile(File, Sock, Offset, Bytes, []) ->
    sendfile(File, Sock, Offset, Bytes, ?MAX_CHUNK_SIZE, undefined, undefined,
	     false, false, false);
sendfile(File, Sock, Offset, Bytes, Opts) ->
    ChunkSize0 = proplists:get_value(chunk_size, Opts, ?MAX_CHUNK_SIZE),
    ChunkSize = if ChunkSize0 > ?MAX_CHUNK_SIZE ->
			?MAX_CHUNK_SIZE;
		   true -> ChunkSize0
		end,
    Headers = proplists:get_value(headers, Opts),
    Trailers = proplists:get_value(trailers, Opts),
    sendfile(File, Sock, Offset, Bytes, ChunkSize, Headers, Trailers,
	     lists:member(sf_nodiskio,Opts),lists:member(sf_mnowait,Opts),
	     lists:member(sf_sync,Opts)).

%% sendfile/2
-spec sendfile(File, Sock) ->
   {'ok', non_neg_integer()} | {'error', inet:posix() | badarg}
      when File :: file:name(),
	   Sock :: socket().
sendfile(File, Sock)  ->
    case file:open(File, [read, raw, binary]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Fd} ->
	    Res = sendfile(Fd, Sock, 0, 0, []),
	    file:close(Fd),
	    Res
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
fdopen(Fd, Opts) ->
    Mod = mod(Opts, undefined),
    Mod:fdopen(Fd, Opts).

%% Get the tcp_module, but IPv6 address overrides default IPv4
mod(Address) ->
    case inet_db:tcp_module() of
	inet_tcp when tuple_size(Address) =:= 8 ->
	    inet6_tcp;
	Mod ->
	    Mod
    end.

%% Get the tcp_module, but option tcp_module|inet|inet6 overrides
mod([{tcp_module,Mod}|_], _Address) ->
    Mod;
mod([inet|_], _Address) ->
    inet_tcp;
mod([inet6|_], _Address) ->
    inet6_tcp;
mod([{ip, Address}|Opts], _) ->
    mod(Opts, Address);
mod([{ifaddr, Address}|Opts], _) ->
    mod(Opts, Address);
mod([_|Opts], Address) ->
    mod(Opts, Address);
mod([], Address) ->
    mod(Address).


%% Internal sendfile functions
sendfile(#file_descriptor{ module = Mod } = Fd, Sock, Offset, Bytes,
	 ChunkSize, Headers, Trailers, Nodiskio, MNowait, Sync)
  when is_port(Sock) ->
    ok = prim_inet:ignorefd(Sock,true),
    {ok, SockFd} = prim_inet:getfd(Sock),
    case Mod:sendfile(Fd, SockFd, Offset, Bytes, ChunkSize, Headers, Trailers,
		      Nodiskio, MNowait, Sync) of
	{error, enotsup} ->
	    ok = prim_inet:ignorefd(Sock,false),
	    sendfile_fallback(Fd, Sock, Offset, Bytes, ChunkSize,
			      Headers, Trailers);
	Else ->
	    ok = prim_inet:ignorefd(Sock,false),
	    Else
    end;
sendfile(_,_,_,_,_,_,_,_,_,_) ->
    {error, badarg}.

%%%
%% Sendfile Fallback
%%%
sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize,
		  Headers, Trailers)
  when is_list(Headers) == false ->
    case sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize) of
	{ok, BytesSent} when is_list(Trailers),is_integer(Headers) ->
	    sendfile_send(Sock, Trailers, BytesSent+Headers);
	{ok, BytesSent} when is_list(Trailers) ->
	    sendfile_send(Sock, Trailers, BytesSent);
	{ok, BytesSent} when is_integer(Headers) ->
	    {ok, BytesSent + Headers};
	Else ->
	    Else
    end;
sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize, Headers, Trailers) ->
    case sendfile_send(Sock, Headers, 0) of
	{ok, BytesSent} ->
	    sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize, BytesSent,
			      Trailers);
	Else ->
	    Else
    end.


sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize) ->
    {ok, CurrPos} = file:position(File, {cur, 0}),
    {ok, _NewPos} = file:position(File, {bof, Offset}),
    Res = sendfile_fallback_int(File, Sock, Bytes, ChunkSize, 0),
    file:position(File, {bof, CurrPos}),
    Res.


sendfile_fallback_int(File, Sock, Bytes, ChunkSize, BytesSent)
  when Bytes > BytesSent; Bytes == 0 ->
    Size = if Bytes == 0 ->
		   ChunkSize;
	       (Bytes - BytesSent + ChunkSize) > 0 ->
		   Bytes - BytesSent;
	      true ->
		   ChunkSize
	   end,
    case file:read(File, Size) of
	{ok, Data} ->
	    case sendfile_send(Sock, Data, BytesSent) of
		{ok,NewBytesSent} ->
		    sendfile_fallback_int(
		      File, Sock, Bytes, ChunkSize,
		      NewBytesSent);
		Error ->
		    Error
	    end;
	eof ->
	    {ok, BytesSent};
	Error ->
	    Error
    end;
sendfile_fallback_int(_File, _Sock, BytesSent, _ChunkSize, BytesSent) ->
    {ok, BytesSent}.

sendfile_send(Sock, Data, Old) ->
    Len = iolist_size(Data),
    case send(Sock, Data) of
	ok ->
	    {ok, Len+Old};
	Else ->
	    Else
    end.
