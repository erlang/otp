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

         send/2, send/3, sendto/5,
         recv/1, recv/2, recvfrom/1, recvfrom/2,

         close/1,

         setopt/3,
         getopt/2,
         formated_timestamp/0
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

-type socket_info() :: map().
%% -record(socket, {info :: socket_info,
%%                  ref  :: reference()}).
-opaque socket() :: {socket, socket_info(), reference()}.
%% -opaque socket() :: #socket{}.

-type accept_flags() :: [accept_flag()].
-type accept_flag()  :: nonblock | cloexec.

-type send_flags() :: [send_flag()].
-type send_flag()  :: confirm |
                      dontroute |
                      dontwait |
                      eor |
                      more |
                      nosignal |
                      oob.

-type recv_flags() :: [recv_flag()].
-type recv_flag()  :: cmsg_cloexec |
                      dontwait |
                      errqueue |
                      oob |
                      peek |
                      trunc |
                      waitall.

-type setopt_key() :: foo.
-type getopt_key() :: foo.

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

%% Bit numbers (from right).
-define(SOCKET_ACCEPT_FLAG_NONBLOCK, 0).
-define(SOCKET_ACCEPT_FLAG_CLOEXEC,  1).

-define(SOCKET_ACCEPT_FLAGS_DEFAULT, []).

-define(SOCKET_SEND_FLAG_CONFIRM,    0).
-define(SOCKET_SEND_FLAG_DONTROUTE,  1).
-define(SOCKET_SEND_FLAG_DONTWAIT,   2).
-define(SOCKET_SEND_FLAG_EOR,        3).
-define(SOCKET_SEND_FLAG_MORE,       4).
-define(SOCKET_SEND_FLAG_NOSIGNAL,   5).
-define(SOCKET_SEND_FLAG_OOB,        6).

-define(SOCKET_SEND_FLAGS_DEFAULT, []).

-define(SOCKET_RECV_FLAG_CMSG_CLOEXEC, 0).
-define(SOCKET_RECV_FLAG_DONTWAIT,     1).
-define(SOCKET_RECV_FLAG_ERRQUEUE,     2).
-define(SOCKET_RECV_FLAG_OOB,          3).
-define(SOCKET_RECV_FLAG_PEEK,         4).
-define(SOCKET_RECV_FLAG_TRUNC,        5).
-define(SOCKET_RECV_FLAG_WAITALL,      6).

-define(SOCKET_RECV_FLAGS_DEFAULT, []).

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
                    Socket = {socket, SocketInfo, SockRef},
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
bind({socket, _, SockRef}, Addr, Port)
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
connect({socket, _, SockRef}, Addr, Port, Timeout)
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
                    nif_cancel(SockRef, Ref),
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

listen({socket, _, SockRef}, Backlog)
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
    accept(Socket, infinity).

%% Do we really need this optimization?
accept(_, Timeout) when is_integer(Timeout) andalso (Timeout < 0) ->
    {error, timeout};
accept({socket, SI, LSockRef}, Timeout)
  when is_integer(Timeout) orelse (Timeout =:= infinity) ->
    Ref = make_ref(),
    do_accept(LSockRef, SI, Ref, Timeout).

do_accept(_, _, _Ref, Timeout) when is_integer(Timeout) andalso (Timeout < 0) ->
    {error, timeout};
do_accept(LSockRef, SI, Ref, Timeout) ->
    TS = timestamp(Timeout),
    case nif_accept(LSockRef, Ref) of
        {ok, SockRef} ->
            SocketInfo = #{domain    => maps:get(domain,   SI),
                           type      => maps:get(type,     SI),
                           protocol  => maps:get(protocol, SI)},
            Socket = {socket, SocketInfo, SockRef},
            {ok, Socket};
        {error, eagain} ->
            receive
                {select, LSockRef, Ref, ready_input} ->
                    do_accept(LSockRef, SI, make_ref(), next_timeout(TS, Timeout))
            after Timeout ->
                    nif_cancel(LSockRef, Ref),
                    flush_select_msgs(LSockRef, Ref),
                    {error, timeout}
            end
    end.

flush_select_msgs(LSRef, Ref) ->
    receive
        {select, LSRef, Ref, _} ->
            flush_select_msgs(LSRef, Ref)
    after 0 ->
            ok
    end.


%% ===========================================================================
%%
%% send, sendto, sendmsg - send a message on a socket
%%

-spec send(Socket, Data, Flags) -> ok | {error, Reason} when
      Socket :: socket(),
      Data   :: binary(),
      Flags  :: send_flags(),
      Reason :: term().

send(Socket, Data) ->
    send(Socket, Data, ?SOCKET_SEND_FLAGS_DEFAULT).

send({socket, _, SockRef}, Data, Flags)
  when is_binary(Data) andalso is_list(Flags) ->
    EFlags = enc_send_flags(Flags),
    nif_send(SockRef, Data, EFlags).


%% ---------------------------------------------------------------------------

-spec sendto(Socket, Data, Flags, DestAddr, Port) -> ok | {error, Reason} when
      Socket   :: socket(),
      Data     :: binary(),
      Flags    :: send_flags(),
      DestAddr :: ip_address(),
      Port     :: port_number(),
      Reason   :: term().

sendto({socket, _, SockRef}, Data, Flags, DestAddr, DestPort)
  when is_binary(Data) andalso
       is_list(Flags) andalso
       (is_tuple(DestAddr) andalso
        ((size(DestAddr) =:= 4) orelse
         (size(DestAddr) =:= 8))) andalso
       (is_integer(DestPort) andalso (DestPort >= 0)) ->
    EFlags = enc_send_flags(Flags),
    nif_sendto(SockRef, Data, EFlags, DestAddr, DestPort).


%% ---------------------------------------------------------------------------

%% -spec sendmsg(Socket, MsgHdr, Flags) -> ok | {error, Reason} when
%%       Socket   :: socket(),
%%       MsgHdr   :: msg_header(),
%%       Flags    :: send_flags(),
%%       Reason   :: term().



%% recv, recvfrom, recvmsg - receive a message from a socket

-spec recv(Socket, Flags) -> {ok, Data} | {error, Reason} when
      Socket :: socket(),
      Flags  :: recv_flags(),
      Data   :: binary(),
      Reason :: term().

recv(Socket) ->
    recv(Socket, ?SOCKET_RECV_FLAGS_DEFAULT).

%% WE "may" need a timeout option here...
recv({socket, _, SockRef}, Flags) when is_list(Flags) ->
    EFlags = enc_recv_flags(Flags),
    nif_recv(SockRef, EFlags).


-spec recvfrom(Socket, Flags) -> {ok, Data, SrcAddr, SrcPort} | {error, Reason} when
      Socket  :: socket(),
      Flags   :: recv_flags(),
      Data    :: binary(),
      SrcAddr :: ip_address(),
      SrcPort :: port_number(),
      Reason  :: term().

recvfrom(Socket) ->
    recvfrom(Socket, ?SOCKET_RECV_FLAGS_DEFAULT).

recvfrom({socket, _, SockRef}, Flags) when is_list(Flags) ->
    EFlags = enc_recv_flags(Flags),
    nif_recvfrom(SockRef, EFlags).

%% -spec recvmsg(Socket, [out] MsgHdr, Flags) -> {ok, Data} | {error, Reason} when
%%       Socket :: socket(),
%%       MsgHdr :: msg_header(),
%%       Flags  :: recv_flags(),
%%       Data   :: binary(),
%%       Reason :: term().



%% close - close a file descriptor

-spec close(Socket) -> ok | {error, Reason} when
      Socket :: socket(),
      Reason :: term().

close({socket, _, SockRef}) ->
    nif_close(SockRef).



%% setopt - manipulate individual properties of a socket
%%
%% What properties are valid depend on what kind of socket it is
%% (domain, type and protocol)
%% If its an "invalid" option (or value), we should not crash but return some
%% useful error...
%%

-spec setopt(Socket, Key, Value) -> ok | {error, Reason} when
      Socket :: socket(),
      Key    :: setopt_key(),
      Value  :: term(),
      Reason :: term().

setopt({socket, Info, SockRef}, Key, Value) ->
    try
        begin
            Domain   = maps:get(domain, Info),
            Type     = maps:get(type, Info),
            Protocol = maps:get(protocol, Info),
            EKey     = enc_setopt_key(Key, Domain, Type, Protocol),
            EVal     = enc_setopt_value(Key, Value, Domain, Type, Protocol),
            nif_setopt(SockRef, EKey, EVal)
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

-spec getopt(Socket, Key) -> {ok, Value} | {error, Reason} when
      Socket :: socket(),
      Key    :: getopt_key(),
      Value  :: term(),
      Reason :: term().

getopt({socket, Info, SockRef}, Key) ->
    try
        begin
            Domain   = maps:get(domain, Info),
            Type     = maps:get(type, Info),
            Protocol = maps:get(protocol, Info),
            EKey     = enc_getopt_key(Key, Domain, Type, Protocol),
            %% We may need to decode the value (for the same reason
            %% we needed to encode the value for setopt).
            case nif_getopt(SockRef, EKey) of
                {ok, EVal} ->
                    Val = dec_getopt_value(Key, EVal, Domain, Type, Protocol),
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
              {dontwait,  ?SOCKET_SEND_FLAG_DONTWAIT},
              {eor,       ?SOCKET_SEND_FLAG_EOR},
              {more,      ?SOCKET_SEND_FLAG_MORE},
              {nosignal,  ?SOCKET_SEND_FLAG_NOSIGNAL},
              {oob,       ?SOCKET_SEND_FLAG_OOB}],
    enc_flags(Flags, EFlags).

-spec enc_recv_flags(Flags) -> non_neg_integer() when
      Flags :: recv_flags().

enc_recv_flags(Flags) ->
    EFlags = [{cmsg_cloexec, ?SOCKET_RECV_FLAG_CMSG_CLOEXEC},
              {dontwait,     ?SOCKET_RECV_FLAG_DONTWAIT},
              {errqueue,     ?SOCKET_RECV_FLAG_ERRQUEUE},
              {oob,          ?SOCKET_RECV_FLAG_OOB},
              {peek,         ?SOCKET_RECV_FLAG_PEEK},
              {trunc,        ?SOCKET_RECV_FLAG_TRUNC},
              {waitall,      ?SOCKET_RECV_FLAG_WAITALL}],
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

%% We should ...really... do something with the domain, type and protocol args...
enc_setopt_key(debug, _, _, _) ->
    ?SOCKET_SETOPT_KEY_DEBUG.

%% We should ...really... do something with the domain, type and protocol args...
enc_setopt_value(debug, V, _, _, _) when is_boolean(V) ->
    V.


%% We should ...really... do something with the domain, type and protocol args...
enc_getopt_key(debug, _, _, _) ->
    ?SOCKET_GETOPT_KEY_DEBUG.

%% We should ...really... do something with the domain, type and protocol args...
dec_getopt_value(debug, B, _, _, _) when is_boolean(B) ->
    B.



%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

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

next_timeout(infinity = Timeout, _) ->
    Timeout;
next_timeout(Timeout, TS) ->
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

nif_send(_SRef, _Data, _Flags) ->
    erlang:error(badarg).

nif_sendto(_SRef, _Data, _Flags, _Dest, _Port) ->
    erlang:error(badarg).

nif_recv(_SRef, _Flags) ->
    erlang:error(badarg).

nif_recvfrom(_SRef, _Flags) ->
    erlang:error(badarg).

nif_cancel(_SRef, _Ref) ->
    erlang:error(badarg).

nif_close(_SRef) ->
    erlang:error(badarg).

nif_setopt(_Ref, _Key, _Val) ->
    erlang:error(badarg).

nif_getopt(_Ref, _Key) ->
    erlang:error(badarg).
