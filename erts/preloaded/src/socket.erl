%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

%% Administrative and "global" utility functions
-export([
	 on_load/0, on_load/1, on_load/2,
	 info/0
        ]).

-export([
         open/2, open/3, open/4,
         bind/2, bind/3,
         connect/3,
         listen/1, listen/2,
         accept/1, accept/2,

         send/2, send/3, send/4,
         sendto/5,
         %% sendmsg/4,
         %% writev/4, OR SENDV? It will be strange for recv then: recvv (instead of readv)

         recv/2, recv/3, recv/4,
         recvfrom/1, recvfrom/2, recvfrom/3, recvfrom/4,
         %% recvmsg/4,
         %% readv/3,

         close/1,

         setopt/4,
         getopt/3
        ]).

-export_type([
              domain/0,
              type/0,
              protocol/0,
              socket/0,

              ip_address/0,
              ip4_address/0,
              ip6_address/0,
              in6_sockaddr/0,
              port_number/0,

              accept_flags/0,
              accept_flag/0,

              send_flags/0,
              send_flag/0
             ]).


%% We support only a subset of all domains.
-type domain() :: local | inet | inet6.

%% We support only a subset of all types.
-type type()   :: stream | dgram | raw | seqpacket.

%% We support only a subset of all protocols:
-type protocol() :: ip | tcp | udp | sctp.

-type ip_address() :: ip4_address() | ip6_address().

-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.

-type uint32()        :: 0..16#FFFFFFFF.
-type ip6_flow_info() :: uint32().
-type ip6_scope_id()  :: uint32().

-type ip6_address() ::
           {0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535}.
%% We need to polish this further...
-record(in6_sockaddr, {addr         :: ip6_address(),
                       flowinfo = 0 :: ip6_flow_info(),
                       scope_id     :: ip6_scope_id()}).
-type in6_sockaddr() :: #in6_sockaddr{}.

-type port_number() :: 0..65535.

%% otp    - The option is internal to our (OTP) imeplementation.
%% socket - The socket layer (SOL_SOCKET).
%% ip     - The ip layer (SOL_IP).
%% tcp    - The TCP (Transport Control Protocol) layer (IPPROTO_TCP).
%% udp    - The UDP (User Datagram Protocol) layer (IPPROTO_UDP).
%% Int    - Raw level, sent down and used "as is".
-type option_level() :: otp | socket | ip | tcp | udp | non_neg_integer().

-type socket_info() :: map().
-record(socket, {info :: socket_info(),
                 ref  :: reference()}).
%% -opaque socket() :: {socket, socket_info(), reference()}.
-opaque socket() :: #socket{}.

-type accept_flags() :: [accept_flag()].
-type accept_flag()  :: nonblock | cloexec.

-type send_flags() :: [send_flag()].
-type send_flag()  :: confirm |
                      dontroute |
                      eor |
                      more |
                      nosignal |
                      oob.

%% Extend with OWN flags for other usage:
%%   - adapt-buffer-sz:
%%     This will have the effect that the nif recvfrom will use
%%     MSG_PEEK to ensure no part of the message is lost, but if
%%     necessary adapt (increase) the buffer size until all of
%%     it fits.
%%
%% Note that not all of these flags is useful for every recv function!
%%
-type recv_flags() :: [recv_flag()].
-type recv_flag()  :: cmsg_cloexec |
                      errqueue |
                      oob |
                      peek |
                      trunc.

-type setopt_key() :: foo.
-type getopt_key() :: foo.

-record(msg_hdr,
        {
          %% Optional address
          %% On an unconnected socket this is used to specify the target
          %% address for a datagram.
          %% For a connected socket, this field should be specified [].
          name       :: list(),

          %% Scatter/gather array
          iov        :: [binary()], % iovec(),

          %% Ancillary (control) data
          ctrl       :: binary(),

          %% Unused
          flags = [] :: list()
        }).
-type msg_hdr() :: #msg_hdr{}.


-define(SOCKET_DOMAIN_LOCAL, 1).
-define(SOCKET_DOMAIN_UNIX,  ?SOCKET_DOMAIN_LOCAL).
-define(SOCKET_DOMAIN_INET,  2).
-define(SOCKET_DOMAIN_INET6, 3).

-define(SOCKET_TYPE_STREAM,    1).
-define(SOCKET_TYPE_DGRAM,     2).
-define(SOCKET_TYPE_RAW,       3).
%% -define(SOCKET_TYPE_RDM,       4).
-define(SOCKET_TYPE_SEQPACKET, 5).

-define(SOCKET_PROTOCOL_IP,    1).
-define(SOCKET_PROTOCOL_TCP,   2).
-define(SOCKET_PROTOCOL_UDP,   3).
-define(SOCKET_PROTOCOL_SCTP,  4).

-define(SOCKET_LISTEN_BACKLOG_DEFAULT, 5).

-define(SOCKET_ACCEPT_TIMEOUT_DEFAULT, infinity).

-define(SOCKET_SEND_FLAG_CONFIRM,    0).
-define(SOCKET_SEND_FLAG_DONTROUTE,  1).
-define(SOCKET_SEND_FLAG_EOR,        2).
-define(SOCKET_SEND_FLAG_MORE,       3).
-define(SOCKET_SEND_FLAG_NOSIGNAL,   4).
-define(SOCKET_SEND_FLAG_OOB,        5).

-define(SOCKET_SEND_FLAGS_DEFAULT,     []).
-define(SOCKET_SEND_TIMEOUT_DEFAULT,   infinity).
-define(SOCKET_SENDTO_TIMEOUT_DEFAULT, ?SOCKET_SEND_TIMEOUT_DEFAULT).

-define(SOCKET_RECV_FLAG_CMSG_CLOEXEC, 0).
-define(SOCKET_RECV_FLAG_ERRQUEUE,     1).
-define(SOCKET_RECV_FLAG_OOB,          2).
-define(SOCKET_RECV_FLAG_PEEK,         3).
-define(SOCKET_RECV_FLAG_TRUNC,        4).

-define(SOCKET_RECV_FLAGS_DEFAULT,   []).
-define(SOCKET_RECV_TIMEOUT_DEFAULT, infinity).

-define(SOCKET_SETOPT_LEVEL_ENCODED,   0).
-define(SOCKET_SETOPT_LEVEL_RAW,       1).
-define(SOCKET_SETOPT_LEVEL_OTP,       0).
-define(SOCKET_SETOPT_LEVEL_SOCKET,    1).
-define(SOCKET_SETOPT_LEVEL_IP,        2).
-define(SOCKET_SETOPT_LEVEL_TCP,       3).
-define(SOCKET_SETOPT_LEVEL_UDP,       4).

-define(SOCKET_SETOPT_KEY_DEBUG,       0).

-define(SOCKET_GETOPT_KEY_DEBUG,       ?SOCKET_SETOPT_KEY_DEBUG).



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
      Extra :: maps:map().

on_load(Extra) when is_map(Extra) ->
    on_load(atom_to_list(?MODULE), Extra).

-spec on_load(Path, Extra) -> ok when
      Path  :: string(),
      Extra :: maps:map().

on_load(Path, Extra) when is_list(Path) andalso is_map(Extra) ->
    on_load(nif_is_loaded(), Path, Extra).

on_load(true, _Path, _Extra) ->
    ok;
on_load(false, Path, Extra) ->
    ok = erlang:load_nif(Path, maps:put(timestamp, formated_timestamp(), Extra)).



-spec info() -> list().

info() ->
    nif_info().



%% ===========================================================================
%%
%% The proper socket API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% open - create an endpoint for communication
%%

open(Domain, Type) ->
    open(Domain, Type, null).

-spec open(Domain, Type, Protocol) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: null | protocol(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type, Protocol) ->
    open(Domain, Type, Protocol, #{}).

-spec open(Domain, Type, Protocol, Extra) -> {ok, Socket} | {error, Reason} when
      Domain   :: domain(),
      Type     :: type(),
      Protocol :: null | protocol(),
      Extra    :: map(),
      Socket   :: socket(),
      Reason   :: term().

open(Domain, Type, Protocol0, Extra) when is_map(Extra) ->
    try
        begin
            Protocol  = default_protocol(Protocol0, Type),
            EDomain   = enc_domain(Domain),
            EType     = enc_type(Domain, Type),
            EProtocol = enc_protocol(Type, Protocol),
            case nif_open(EDomain, EType, EProtocol, Extra) of
                {ok, SockRef} ->
                    SocketInfo = #{domain   => Domain,
                                   type     => Type,
                                   protocol => Protocol},
                    Socket = #socket{info = SocketInfo,
                                     ref  = SockRef},
                    {ok, Socket};
                {error, _} = ERROR ->
                    ERROR
            end
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason}
    end.

%% Note that this is just a convenience function for when the protocol was
%% not specified. If its actually specied, then that will be selected.
%% Also, this only works for the some of the type's (stream, dgram and
%% seqpacket).
default_protocol(null, stream)    -> tcp;
default_protocol(null, dgram)     -> udp;
default_protocol(null, seqpacket) -> sctp;
default_protocol(null, Type)      -> throw({error, {no_default_protocol, Type}});
default_protocol(Protocol, _)     -> Protocol.


%% ===========================================================================
%%
%% bind - bind a name to a socket
%%

-spec bind(Socket, FileOrAddr) -> ok | {error, Reason} when
      Socket     :: socket(),
      FileOrAddr :: binary() | string() | ip_address() | any | loopback,
      Reason     :: term().

bind(Socket, File) when is_binary(File) ->
    if
        byte_size(File) =:= 0 ->
            {error, einval};
        byte_size(File) =< 255 ->
            nif_bind(Socket, File);
        true ->
            {error, einval}
    end;
bind(Socket, File) when is_list(File) andalso (File =/= []) ->
    Bin = unicode:characters_to_binary(File, file:native_name_encoding()),
    if
        byte_size(Bin) =< 255 ->
            nif_bind(Socket, Bin);
        true ->
            {error, einval}
    end;
bind(Socket, Addr) when is_tuple(Addr) orelse
                        (Addr =:= any) orelse
                        (Addr =:= loopback) ->
    bind(Socket, Addr, 0).


-spec bind(Socket, Address, Port) -> ok | {ok, NewPort} | {error, Reason} when
      Socket  :: socket(),
      Address :: ip_address() | any | loopback,
      Port    :: port_number(),
      NewPort :: port_number(),
      Reason  :: term().

%% Shall we keep info about domain so that we can verify address?
bind(#socket{ref = SockRef}, Addr, Port)
  when (is_tuple(Addr) andalso
        ((size(Addr) =:= 4) orelse (size(Addr) =:= 8))) orelse
       ((Addr =:= any) orelse (Addr =:= loopback)) andalso
       (is_integer(Port) andalso (Port >= 0)) ->
    nif_bind(SockRef, {Addr, Port}).



%% ===========================================================================
%%
%% connect - initiate a connection on a socket
%%

-spec connect(Socket, Addr, Port) -> ok | {error, Reason} when
      Socket :: socket(),
      Addr   :: ip_address(),
      Port   :: port_number(),
      Reason :: term().

connect(Socket, Addr, Port) ->
    connect(Socket, Addr, Port, infinity).

-spec connect(Socket, Addr, Port, Timeout) -> ok | {error, Reason} when
      Socket  :: socket(),
      Addr    :: ip_address(),
      Port    :: port_number(),
      Timeout :: timeout(),
      Reason  :: term().

connect(_Socket, _Addr, _Port, Timeout)
  when (is_integer(Timeout) andalso (Timeout =< 0)) ->
    {error, timeout};
connect(#socket{ref = SockRef}, Addr, Port, Timeout)
  when (is_tuple(Addr) andalso
        ((size(Addr) =:= 4) orelse (size(Addr) =:= 8))) andalso
       (is_integer(Port) andalso (Port >= 0)) andalso
       ((Timeout =:= infinity) orelse is_integer(Timeout)) ->
    TS = timestamp(Timeout),
    case nif_connect(SockRef, Addr, Port) of
        ok ->
            %% Connected!
            ok;
        {ok, Ref} ->
            %% Connecting...
	    NewTimeout = next_timeout(TS, Timeout),
	    receive
		{select, SockRef, Ref, ready_output} ->
		    nif_finalize_connection(SockRef)
	    after NewTimeout ->
                    nif_cancel(SockRef, connect, Ref),
		    {error, timeout}
	    end;
	{error, _} = ERROR ->
	    ERROR
    end.



%% ===========================================================================
%%
%% listen - listen for connections on a socket
%%

-spec listen(Socket, Backlog) -> ok | {error, Reason} when
      Socket  :: socket(),
      Backlog :: pos_integer(),
      Reason  :: term().

listen(Socket) ->
    listen(Socket, ?SOCKET_LISTEN_BACKLOG_DEFAULT).

listen(#socket{ref = SockRef}, Backlog)
  when (is_integer(Backlog) andalso (Backlog >= 0)) ->
    nif_listen(SockRef, Backlog).




%% ===========================================================================
%%
%% accept, accept4 - accept a connection on a socket
%%

-spec accept(LSocket, Timeout) -> {ok, Socket} | {error, Reason} when
      LSocket :: socket(),
      Timeout :: timeout(),
      Socket  :: socket(),
      Reason  :: term().

accept(Socket) ->
    accept(Socket, ?SOCKET_ACCEPT_TIMEOUT_DEFAULT).

%% Do we really need this optimization?
accept(_, Timeout) when is_integer(Timeout) andalso (Timeout =< 0) ->
    {error, timeout};
accept(#socket{info = SI, ref = LSockRef}, Timeout)
  when is_integer(Timeout) orelse (Timeout =:= infinity) ->
    do_accept(LSockRef, SI, Timeout).

do_accept(LSockRef, SI, Timeout) ->
    TS     = timestamp(Timeout),
    AccRef = make_ref(),
    case nif_accept(LSockRef, AccRef) of
        {ok, SockRef} ->
            SocketInfo = #{domain    => maps:get(domain,   SI),
                           type      => maps:get(type,     SI),
                           protocol  => maps:get(protocol, SI)},
            Socket = #socket{info = SocketInfo,
                             ref  = SockRef},
            {ok, Socket};
        {error, eagain} ->
	    NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, LSockRef, AccRef, ready_input} ->
                    do_accept(LSockRef, SI, next_timeout(TS, Timeout))
            after NewTimeout ->
                    nif_cancel(LSockRef, accept, AccRef),
                    flush_select_msgs(LSockRef, AccRef),
                    {error, timeout}
            end
    end.



%% ===========================================================================
%%
%% send, sendto, sendmsg - send a message on a socket
%%

send(Socket, Data) ->
    send(Socket, Data, ?SOCKET_SEND_FLAGS_DEFAULT, ?SOCKET_SEND_TIMEOUT_DEFAULT).

send(Socket, Data, Flags) when is_list(Flags) ->
    send(Socket, Data, Flags, ?SOCKET_SEND_TIMEOUT_DEFAULT);
send(Socket, Data, Timeout) ->
    send(Socket, Data, ?SOCKET_SEND_FLAGS_DEFAULT, Timeout).

-spec send(Socket, Data, Flags, Timeout) -> ok | {error, Reason} when
      Socket  :: socket(),
      Data    :: iodata(),
      Flags   :: send_flags(),
      Timeout :: timeout(),
      Reason  :: term().

send(Socket, Data, Flags, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    send(Socket, Bin, Flags, Timeout);
send(#socket{ref = SockRef}, Data, Flags, Timeout)
  when is_binary(Data) andalso is_list(Flags)  ->
    EFlags = enc_send_flags(Flags),
    do_send(SockRef, Data, EFlags, Timeout).

do_send(SockRef, Data, EFlags, Timeout) ->
    TS      = timestamp(Timeout),
    SendRef = make_ref(),
    case nif_send(SockRef, SendRef, Data, EFlags) of
        ok ->
            ok;
        {ok, Written} ->
	    NewTimeout = next_timeout(TS, Timeout),
	    %% We are partially done, wait for continuation
            receive
                {select, SockRef, SendRef, ready_output} when (Written > 0) ->
                    <<_:Written/binary, Rest/binary>> = Data,
                    do_send(SockRef, Rest, EFlags,
                            next_timeout(TS, Timeout));
                {select, SockRef, SendRef, ready_output} ->
                    do_send(SockRef, Data, EFlags,
                            next_timeout(TS, Timeout))
            after NewTimeout ->
                    nif_cancel(SockRef, send, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, {timeout, size(Data)}}
            end;
        {error, eagain} ->
            receive
                {select, SockRef, SendRef, ready_output} ->
                    do_send(SockRef, Data, EFlags,
                            next_timeout(TS, Timeout))
            after Timeout ->
                    nif_cancel(SockRef, send, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, {timeout, size(Data)}}
            end;

        {error, _} = ERROR ->
            ERROR
    end.




%% ---------------------------------------------------------------------------
%%

sendto(Socket, Data, Flags, DestAddr, DestPort) ->
    sendto(Socket, Data, Flags, DestAddr, DestPort, ?SOCKET_SENDTO_TIMEOUT_DEFAULT).

-spec sendto(Socket, Data, Flags, DestAddr, DestPort, Timeout) ->
                    ok | {error, Reason} when
      Socket   :: socket(),
      Data     :: binary(),
      Flags    :: send_flags(),
      DestAddr :: null | ip_address(),
      DestPort :: port_number(),
      Timeout  :: timeout(),
      Reason   :: term().

sendto(Socket, Data, Flags, DestAddr, DestPort, Timeout) when is_list(Data) ->
    Bin = erlang:list_to_binary(Data),
    sendto(Socket, Bin, Flags, DestAddr, DestPort, Timeout);
sendto(#socket{ref = SockRef}, Data, Flags, DestAddr, DestPort, Timeout)
  when is_binary(Data) andalso
       is_list(Flags) andalso
       (is_tuple(DestAddr) orelse (DestAddr =:= null)) andalso
       is_integer(DestPort) andalso
       (is_integer(Timeout) orelse (Timeout =:= infinity)) ->
    EFlags = enc_send_flags(Flags),
    do_sendto(SockRef, Data, EFlags, DestAddr, DestPort, Timeout).

do_sendto(SockRef, Data, EFlags, DestAddr, DestPort, Timeout) ->
    TS      = timestamp(Timeout),
    SendRef = make_ref(),
    case nif_sendto(SockRef, SendRef, Data, EFlags, DestAddr, DestPort) of
        ok ->
            %% We are done
            ok;

        {ok, Written} ->
	    %% We are partially done, wait for continuation
            receive
                {select, SockRef, SendRef, ready_output} when (Written > 0) ->
                    <<_:Written/binary, Rest/binary>> = Data,
                    do_sendto(SockRef, Rest, EFlags, DestAddr, DestPort,
                              next_timeout(TS, Timeout));
                {select, SockRef, SendRef, ready_output} ->
                    do_sendto(SockRef, Data, EFlags, DestAddr, DestPort,
                              next_timeout(TS, Timeout))
            after Timeout ->
                    nif_cancel(SockRef, sendto, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, timeout}
            end;

        {error, eagain} ->
            receive
                {select, SockRef, SendRef, ready_output} ->
                    do_sendto(SockRef, Data, EFlags, DestAddr, DestPort,
                              next_timeout(TS, Timeout))
            after Timeout ->
                    nif_cancel(SockRef, sendto, SendRef),
                    flush_select_msgs(SockRef, SendRef),
                    {error, timeout}
            end;

        {error, _} = ERROR ->
            ERROR
    end.



%% ---------------------------------------------------------------------------

%% -spec sendmsg(Socket, MsgHdr, Flags) -> ok | {error, Reason} when
%%       Socket   :: socket(),
%%       MsgHdr   :: msg_header(),
%%       Flags    :: send_flags(),
%%       Reason   :: term().



%% ===========================================================================
%%
%% writev - write data into multiple buffers
%%

%% send(Socket, Data, Flags, Timeout)
%%   when (is_list(Data) orelse is_binary(Data)) andalso is_list(Flags)  ->
%%     IOVec  = erlang:iolist_to_iovec(Data),
%%     EFlags = enc_send_flags(Flags),
%%     send_iovec(Socket, IOVec, EFlags, Timeout).


%% %% Iterate over the IO-vector (list of binaries).

%% send_iovec(_Socket, [] = _IOVec, _EFlags, _Timeout) ->
%%     ok;
%% send_iovec({socket, _, SockRef} = Socket, [Bin|IOVec], EFlags, Timeout) ->
%%     case do_send(SockRef, make_ref(), Bin, EFlags, Timeout) of
%%         {ok, NewTimeout} ->
%%             send_iovec(Socket, IOVec, EFlags, NewTimeout);
%%         {error, _} = ERROR ->
%%             ERROR
%%     end.


%% do_send(SockRef, SendRef, Data, _EFlags, Timeout)
%%   when (Timeout < 0) ->
%%     nif_cancel(SockRef, SendRef),
%%     flush_select_msgs(SockRef, SendRef),
%%     {error, {timeout, size(Data)}};
%% do_send(SockRef, SendRef, Data, EFlags, Timeout) ->
%%     TS = timestamp(Timeout),
%%     case nif_send(SockRef, SendRef, Data, EFlags) of
%%         ok ->
%%             {ok, next_timeout(TS, Timeout)};
%%         {ok, Written} ->
%% 	    %% We are partially done, wait for continuation
%%             receive
%%                 {select, SockRef, SendRef, ready_output} ->
%%                     <<_:Written/binary, Rest/binary>> = Data,
%%                     do_send(SockRef, make_ref(), Rest, EFlags,
%%                             next_timeout(TS, Timeout))
%%             after Timeout ->
%%                     nif_cancel(SockRef, SendRef),
%%                     flush_select_msgs(SockRef, SendRef),
%%                     {error, timeout}
%%             end;
%%         {error, eagain} ->
%%             receive
%%                 {select, SockRef, SendRef, ready_output} ->
%%                     do_send(SockRef, SendRef, Data, EFlags,
%%                             next_timeout(TS, Timeout))
%%             after Timeout ->
%%                     nif_cancel(SockRef, SendRef),
%%                     flush_select_msgs(SockRef, SendRef),
%%                     {error, timeout}
%%             end;

%%         {error, _} = ERROR ->
%%             ERROR
%%     end.


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

recv(Socket, Length) ->
    recv(Socket, Length,
         ?SOCKET_RECV_FLAGS_DEFAULT,
         ?SOCKET_RECV_TIMEOUT_DEFAULT).

recv(Socket, Length, Flags) when is_list(Flags) ->
    recv(Socket, Length, Flags, ?SOCKET_RECV_TIMEOUT_DEFAULT);
recv(Socket, Length, Timeout) ->
    recv(Socket, Length, ?SOCKET_RECV_FLAGS_DEFAULT, Timeout).

-spec recv(Socket, Length, Flags, Timeout) -> {ok, Data} | {error, Reason} when
      Socket  :: socket(),
      Length  :: non_neg_integer(),
      Flags   :: recv_flags(),
      Timeout :: timeout(),
      Data    :: binary(),
      Reason  :: term().

recv(#socket{ref = SockRef}, Length, Flags, Timeout)
  when (is_integer(Length) andalso (Length >= 0)) andalso
       is_list(Flags) andalso
       (is_integer(Timeout) orelse (Timeout =:= infinity)) ->
    EFlags = enc_recv_flags(Flags),
    do_recv(SockRef, undefined, Length, EFlags, <<>>, Timeout).

%% We need to pass the "old recv ref" around because of the special case
%% with Length = 0. This case makes it neccessary to have a timeout function
%% clause since we may never wait for anything (no receive select), and so the
%% the only timeout check will be the function clause.
do_recv(SockRef, _OldRef, Length, EFlags, Acc, Timeout)
  when (Timeout =:= infinity) orelse
       (is_integer(Timeout) andalso (Timeout > 0)) ->
    TS      = timestamp(Timeout),
    RecvRef = make_ref(),
    case nif_recv(SockRef, RecvRef, Length, EFlags) of
        {ok, true = _Complete, Bin} when (size(Acc) =:= 0) ->
            {ok, Bin};
        {ok, true = _Complete, Bin} ->
            {ok, <<Acc/binary, Bin/binary>>};

        %% It depends on the amount of bytes we tried to read:
        %%    0   - Read everything available
        %%          We got something, but there may be more - keep reading.
        %%    > 0 - We got a part of the message and we will be notified
        %%          when there is more to read (a select message)
        {ok, false = _Complete, Bin} when (Length =:= 0) ->
            do_recv(SockRef, RecvRef,
                    Length, EFlags,
                    <<Acc/binary, Bin/binary>>,
                    next_timeout(TS, Timeout));

        {ok, false = _Completed, Bin} when (size(Acc) =:= 0) ->
            %% We got the first chunk of it.
            %% We will be notified (select message) when there
            %% is more to read.
	    NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recv(SockRef, RecvRef,
                            Length-size(Bin), EFlags,
                            Bin,
                            next_timeout(TS, Timeout))
            after NewTimeout ->
                    nif_cancel(SockRef, recv, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, {timeout, Acc}}
            end;

        {ok, false = _Completed, Bin} ->
            %% We got a chunk of it!
	    NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recv(SockRef, RecvRef,
                            Length-size(Bin), EFlags,
                            <<Acc/binary, Bin/binary>>,
                            next_timeout(TS, Timeout))
            after NewTimeout ->
                    nif_cancel(SockRef, recv, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, {timeout, Acc}}
            end;

        %% We return with the accumulated binary regardless if its empty...
        {error, eagain} when (Length =:= 0) ->
            {ok, Acc};

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recv(SockRef, RecvRef,
                            Length, EFlags,
                            Acc,
                            next_timeout(TS, Timeout))
            after NewTimeout ->
                    nif_cancel(SockRef, recv, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, timeout}
            end;

        {error, _} = ERROR when (size(Acc) =:= 0) ->
            ERROR;

        {error, Reason} ->
            {error, {Reason, Acc}}

    end;

do_recv(SockRef, RecvRef, 0 = _Length, _Eflags, Acc, _Timeout) ->
    %% The current recv operation is to be cancelled, so no need for a ref...
    %% The cancel will end our 'read everything you have' and "activate"
    %% any waiting reader.
    nif_cancel(SockRef, recv, RecvRef),
    {ok, Acc};
do_recv(_SockRef, _RecvRef, _Length, _EFlags, Acc, _Timeout) when (size(Acc) > 0) ->
    {error, {timeout, Acc}};
do_recv(_SockRef, _RecvRef, _Length, _EFlags, _Acc, _Timeout) ->
    {error, timeout}.



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
%% "consumed" from the underlying buffers, so another recvfrom call
%% is needed, possibly with a then adjusted buffer size.
%%

recvfrom(Socket) ->
    recvfrom(Socket, 0).

recvfrom(Socket, BufSz) ->
    recvfrom(Socket, BufSz,
             ?SOCKET_RECV_FLAGS_DEFAULT,
             ?SOCKET_RECV_TIMEOUT_DEFAULT).


recvfrom(Socket, Flags, Timeout) when is_list(Flags) ->
    recvfrom(Socket, 0, Flags, Timeout);
recvfrom(Socket, BufSz, Flags) when is_list(Flags) ->
    recvfrom(Socket, BufSz, Flags, ?SOCKET_RECV_TIMEOUT_DEFAULT);
recvfrom(Socket, BufSz, Timeout) ->
    recvfrom(Socket, BufSz, ?SOCKET_RECV_FLAGS_DEFAULT, Timeout).

-spec recvfrom(Socket, BufSz, Flags, Timeout) -> {ok, {SrcDomain, Source, Data}} | {error, Reason} when
      Socket    :: socket(),
      BufSz     :: non_neg_integer(),
      Flags     :: recv_flags(),
      Timeout   :: timeout(),
      SrcDomain :: domain() | undefined,
      Source    :: {ip_address(), port_number()} | string() | undefined,
      Data      :: binary(),
      Reason    :: term().

recvfrom(#socket{ref = SockRef}, BufSz, Flags, Timeout)
  when (is_integer(BufSz) andalso (BufSz >= 0)) andalso
       is_list(Flags) andalso
       (is_integer(Timeout) orelse (Timeout =:= infinity)) ->
    EFlags = enc_recv_flags(Flags),
    do_recvfrom(SockRef, BufSz, EFlags, Timeout).

do_recvfrom(SockRef, BufSz, EFlags, Timeout)  ->
    TS      = timestamp(Timeout),
    RecvRef = make_ref(),
    case nif_recvfrom(SockRef, RecvRef, BufSz, EFlags) of
        {ok, {_Domain, _Source, _NewData}} = OK ->
            OK;

        {error, eagain} ->
            %% There is nothing just now, but we will be notified when there
            %% is something to read (a select message).
            NewTimeout = next_timeout(TS, Timeout),
            receive
                {select, SockRef, RecvRef, ready_input} ->
                    do_recvfrom(SockRef, BufSz, EFlags,
                                next_timeout(TS, Timeout))
            after NewTimeout ->
                    nif_cancel(SockRef, recvfrom, RecvRef),
                    flush_select_msgs(SockRef, RecvRef),
                    {error, timeout}
            end;

        {error, _} = ERROR ->
            ERROR

    end.



%% ---------------------------------------------------------------------------
%%

%% -spec recvmsg(Socket, [out] MsgHdr, Flags) -> {ok, Data} | {error, Reason} when
%%       Socket :: socket(),
%%       MsgHdr :: msg_header(),
%%       Flags  :: recv_flags(),
%%       Data   :: binary(),
%%       Reason :: term().



%% ===========================================================================
%%
%% readv - read data into multiple buffers
%%



%% ===========================================================================
%%
%% close - close a file descriptor
%%

-spec close(Socket) -> ok | {error, Reason} when
      Socket :: socket(),
      Reason :: term().

close(#socket{ref = SockRef}) ->
    case nif_close(SockRef) of
        ok ->
            nif_finalize_close(SockRef);
        {ok, CloseRef} ->
            %% We must wait
            receive
		{close, CloseRef} ->
                    %% <KOLLA>
                    %%
                    %% WHAT HAPPENS IF THIS PROCESS IS KILLED
                    %% BEFORE WE CAN EXECUTE THE FINAL CLOSE???
                    %%
                    %% </KOLLA>
                    nif_finalize_close(SockRef)
            end;
        {error, _} = ERROR ->
            ERROR
    end.




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
%% WE NEED TOP MAKE SURE THAT THE USER DOES NOT MAKE US BLOCKING
%% AS MUCH OF THE CODE EXPECTS TO BE NON-BLOCKING!!
%%
%% </KOLLA>

-spec setopt(Socket, Level, Key, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Level  :: option_level(),
      Key    :: setopt_key(),
      Value  :: term(),
      Reason :: term().

setopt(#socket{info = Info, ref = SockRef}, Level, Key, Value) ->
    try
        begin
            Domain   = maps:get(domain, Info),
            Type     = maps:get(type, Info),
            Protocol = maps:get(protocol, Info),
            ELevel   = enc_setopt_level(Level),
            EKey     = enc_setopt_key(Level, Key, Domain, Type, Protocol),
            EVal     = enc_setopt_value(Level, Key, Value, Domain, Type, Protocol),
            nif_setopt(SockRef, ELevel, EKey, EVal)
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason} % Process more?
    end.


%% getopt - retrieve individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol).
%% If its an "invalid" option, we should not crash but return some
%% useful error...
%%

-spec getopt(Socket, Level, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Level  :: option_level(),
      Key    :: getopt_key(),
      Value  :: term(),
      Reason :: term().

getopt(#socket{info = Info, ref = SockRef}, Level, Key) ->
    try
        begin
            Domain   = maps:get(domain, Info),
            Type     = maps:get(type, Info),
            Protocol = maps:get(protocol, Info),
            ELevel   = enc_getopt_level(Level),
            EKey     = enc_getopt_key(Level, Key, Domain, Type, Protocol),
            %% We may need to decode the value (for the same reason
            %% we needed to encode the value for setopt).
            case nif_getopt(SockRef, ELevel, EKey) of
                {ok, EVal} ->
                    Val = dec_getopt_value(Level, Key, EVal, Domain, Type, Protocol),
                    {ok, Val};
                {error, _} = ERROR ->
                    ERROR
            end
        end
    catch
        throw:T ->
            T;
        error:Reason ->
            {error, Reason} % Process more?
    end.



%% ===========================================================================
%%
%% Encode / decode
%%
%% ===========================================================================

-spec enc_domain(Domain) -> non_neg_integer() when
      Domain :: domain().

enc_domain(local)  -> ?SOCKET_DOMAIN_LOCAL;
enc_domain(inet)   -> ?SOCKET_DOMAIN_INET;
enc_domain(inet6)  -> ?SOCKET_DOMAIN_INET6;
enc_domain(Domain) -> throw({error, {invalid_domain, Domain}}).

-spec enc_type(Domain, Type) -> non_neg_integer() when
      Domain :: domain(),
      Type   :: type().

%% What combos are valid?
enc_type(_, stream)    -> ?SOCKET_TYPE_STREAM;
enc_type(_, dgram)     -> ?SOCKET_TYPE_DGRAM;
enc_type(_, raw)       -> ?SOCKET_TYPE_RAW;
enc_type(_, seqpacket) -> ?SOCKET_TYPE_SEQPACKET;
enc_type(_, Type)      -> throw({error, {invalid_type, Type}}).

-spec enc_protocol(Type, Protocol) -> non_neg_integer() when
      Type     :: type(),
      Protocol :: protocol().

enc_protocol(dgram,     ip)   -> ?SOCKET_PROTOCOL_IP;
enc_protocol(stream,    tcp)  -> ?SOCKET_PROTOCOL_TCP;
enc_protocol(dgram,     udp)  -> ?SOCKET_PROTOCOL_UDP;
enc_protocol(seqpacket, sctp) -> ?SOCKET_PROTOCOL_SCTP;
enc_protocol(Type, Proto)     -> throw({error, {invalid_protocol, {Type, Proto}}}).


-spec enc_send_flags(Flags) -> non_neg_integer() when
      Flags :: send_flags().

enc_send_flags(Flags) ->
    EFlags = [{confirm,   ?SOCKET_SEND_FLAG_CONFIRM},
              {dontroute, ?SOCKET_SEND_FLAG_DONTROUTE},
              {eor,       ?SOCKET_SEND_FLAG_EOR},
              {more,      ?SOCKET_SEND_FLAG_MORE},
              {nosignal,  ?SOCKET_SEND_FLAG_NOSIGNAL},
              {oob,       ?SOCKET_SEND_FLAG_OOB}],
    enc_flags(Flags, EFlags).

-spec enc_recv_flags(Flags) -> non_neg_integer() when
      Flags :: recv_flags().

enc_recv_flags(Flags) ->
    EFlags = [{cmsg_cloexec, ?SOCKET_RECV_FLAG_CMSG_CLOEXEC},
              {errqueue,     ?SOCKET_RECV_FLAG_ERRQUEUE},
              {oob,          ?SOCKET_RECV_FLAG_OOB},
              {peek,         ?SOCKET_RECV_FLAG_PEEK},
              {trunc,        ?SOCKET_RECV_FLAG_TRUNC}],
    enc_flags(Flags, EFlags).


enc_flags([], _) ->
    0;
enc_flags(Flags, EFlags) ->
    F = fun(Flag, Acc) ->
                case lists:keysearch(Flag, 1, EFlags) of
                    {value, {Flag, EFlag}} ->
                        Acc bor (1 bsl EFlag);
                    false ->
                        throw({error, {unknown_flag, Flag}})
                end
        end,
    lists:foldl(F, 0, Flags).

enc_setopt_level(otp) ->
    {?SOCKET_SETOPT_LEVEL_ENCODED, ?SOCKET_SETOPT_LEVEL_OTP};
enc_setopt_level(socket) ->
    {?SOCKET_SETOPT_LEVEL_ENCODED, ?SOCKET_SETOPT_LEVEL_SOCKET};
enc_setopt_level(ip) ->
    {?SOCKET_SETOPT_LEVEL_ENCODED, ?SOCKET_SETOPT_LEVEL_IP};
enc_setopt_level(tcp) ->
    {?SOCKET_SETOPT_LEVEL_ENCODED, ?SOCKET_SETOPT_LEVEL_TCP};
enc_setopt_level(udp) ->
    {?SOCKET_SETOPT_LEVEL_ENCODED, ?SOCKET_SETOPT_LEVEL_UDP};
%% Any option that is of an raw level must be provided as a binary
%% already fully encoded!
enc_setopt_level(L) when is_integer(L) ->
    {?SOCKET_SETOPT_LEVEL_RAW, L}.


%% We should ...really... do something with the domain, type and protocol args...
%% Also, any option which has an integer level (raw) must also be provided
%% in a raw mode, that is, as an integer.
enc_setopt_key(L, K, _, _, _) when is_integer(L) andalso is_integer(K) ->
    K;
enc_setopt_key(otp, debug, _, _, _) ->
    ?SOCKET_SETOPT_KEY_DEBUG.

%% We should ...really... do something with the domain, type and protocol args...
enc_setopt_value(otp, debug, V, _, _, _) when is_boolean(V) ->
    V;
enc_setopt_value(socket, linger, abort, D, T, P) ->
    enc_setopt_value(socket, linger, {true, 0}, D, T, P);
enc_setopt_value(socket, linger, {OnOff, Secs} = V, _D, _T, _P)
  when is_boolean(OnOff) andalso is_integer(Secs) andalso (Secs >= 0) ->
    V;
enc_setopt_value(L, _, V, _, _, _) when is_integer(L) andalso is_binary(V) ->
    V.



enc_getopt_level(Level) ->
    enc_setopt_level(Level).

%% We should ...really... do something with the domain, type and protocol args...
enc_getopt_key(otp, debug, _, _, _) ->
    ?SOCKET_GETOPT_KEY_DEBUG.

%% We should ...really... do something with the domain, type and protocol args...
dec_getopt_value(otp, debug, B, _, _, _) when is_boolean(B) ->
    B.



%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

flush_select_msgs(LSRef, Ref) ->
    receive
        {select, LSRef, Ref, _} ->
            flush_select_msgs(LSRef, Ref)
    after 0 ->
            ok
    end.


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(Now, N2T, true).

format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
    FormatExtra = ".~.2.0w",
    ArgsExtra   = [N3 div 10000],
    format_timestamp(N, N2T, FormatExtra, ArgsExtra);
format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
    FormatExtra = "",
    ArgsExtra   = [],
    format_timestamp(N, N2T, FormatExtra, ArgsExtra).

format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
    {Date, Time}   = N2T(N),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
                      [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
    lists:flatten(FormatDate).


%% A timestamp in ms

timestamp(infinity) ->
    undefined;
timestamp(_) ->
    timestamp().

timestamp() ->
    {A,B,C} = os:timestamp(),
    A*1000000000+B*1000+(C div 1000).

next_timeout(_, infinity = Timeout) ->
    Timeout;
next_timeout(TS, Timeout) ->
    NewTimeout = Timeout - tdiff(TS, timestamp()),
    if
        (NewTimeout > 0) ->
            NewTimeout;
        true ->
            0
    end.

tdiff(T1, T2) ->
    T2 - T1.





%% ===========================================================================
%%
%% Below follows the actual NIF-functions.
%%
%% ===========================================================================

nif_is_loaded() -> false.

nif_info() ->
    erlang:error(badarg).

nif_open(_Domain, _Type, _Protocol, _Extra) ->
    erlang:error(badarg).

nif_bind(_SRef, _LocalAddr) ->
    erlang:error(badarg).

nif_connect(_SRef, _Addr, _Port) ->
    erlang:error(badarg).

nif_finalize_connection(_SRef) ->
    erlang:error(badarg).

nif_listen(_SRef, _Backlog) ->
    erlang:error(badarg).

nif_accept(_SRef, _Ref) ->
    erlang:error(badarg).

nif_send(_SockRef, _SendRef, _Data, _Flags) ->
    erlang:error(badarg).

nif_sendto(_SRef, _SendRef, _Data, _Flags, _Dest, _Port) ->
    erlang:error(badarg).

nif_recv(_SRef, _RecvRef, _Length, _Flags) ->
    erlang:error(badarg).

nif_recvfrom(_SRef, _RecvRef, _Length, _Flags) ->
    erlang:error(badarg).

nif_cancel(_SRef, _Op, _Ref) ->
    erlang:error(badarg).

nif_close(_SRef) ->
    erlang:error(badarg).

nif_finalize_close(_SRef) ->
    erlang:error(badarg).

nif_setopt(_Ref, _Lev, _Key, _Val) ->
    erlang:error(badarg).

nif_getopt(_Ref, _Lev, _Key) ->
    erlang:error(badarg).
