%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2026 Zeyu Zhang <zzy___1@sina.com>
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

-module(tls_ktls).
-moduledoc false.

%% {ktls, true}: after a TLS 1.3 handshake the connection process hands
%% the transport socket to the kernel (Linux kTLS) and to the owner, and
%% exits. Application data then uses plain gen_tcp send/recv. The socket
%% (an inet_drv port, or the socket NIF under gen_tcp_socket) keeps the
%% kTLS state, see erl_ktls.h: whichever process meets a control record,
%% or calls ssl:update_keys/2, derives the next key here and installs it
%% there. Keys are only updated on a peer KeyUpdate or by update_keys/2.

-export([init/3, handover/2, recv/3, handle_control/2, update_keys/2,
         controlling_process/2, close/1, shutdown/2, info/2, set_ktls/1,
         set_key/6]).

-include("ssl_connection.hrl").
-include("tls_handshake_1_3.hrl").

%% Linux struct tls12_crypto_info_*, see crypto_info/4
-define(TLS_1_3_VERSION, 16#0304).
-define(TLS_CIPHER_AES_GCM_128, 51).
-define(TLS_CIPHER_AES_GCM_256, 52).
-define(TLS_CIPHER_AES_CCM_128, 53).
-define(TLS_CIPHER_CHACHA20_POLY1305, 54).
-define(TLS_TX, 1).
-define(TLS_RX, 2).
%% Requests and flags, see erl_ktls.h
-define(KTLS_INIT, 1).
-define(KTLS_STATE, 2).
-define(KTLS_INFO, 3).
-define(KTLS_CONTROL, 4).
-define(KTLS_UPDATE, 5).
-define(KTLS_ALERT, 6).
-define(KTLS_KEY, 7).
-define(KTLS_RX, 1).
-define(KTLS_TX, 2).
-define(KTLS_MAY_OWE, 4).
-define(SEND_RETRY, 1). % ms, send buffer full

%% A handed over socket: an inet_drv port, or a gen_tcp_socket around
%% the socket NIF resource the handshake used
-type handle() :: port() | {'$inet', gen_tcp_socket, {pid(), socket:socket()}}.
%% The socket's kTLS state: the cipher suite and the traffic secrets
%% the next receive and send keys derive from
-type state() :: {Suite :: <<_:16>>, RxSecret :: binary(), TxSecret :: binary()}.
-type key_update_request() :: ?UPDATE_NOT_REQUESTED | ?UPDATE_REQUESTED.
-type active() :: boolean() | once | -32768..32767.
-type reason() :: closed | timeout | inet:posix() |
                  {tls_alert, {atom(), string()}} | term().

%% Connection start: read one record at a time until the handover, which
%% applies the read_ahead socket option, so that no record after the
%% handshake is read. A socket:socket() handshake (tls_socket_tcp) is
%% bounded by tls_gen_connection:ktls_read_size/1 instead.
-spec init(SslOptions, Socket, ProtocolSpecific) -> ProtocolSpecific when
      SslOptions :: map(),
      Socket :: handle() | socket:socket(),
      ProtocolSpecific :: map().
init(#{ktls := true}, Socket, PS) ->
    _ = case Socket of
            {'$socket', _} -> ok;
            _ -> inet:setopts(Socket, [{packet, ssl_tls}, {read_ahead, false}])
        end,
    PS#{active_n => 1};
init(_, _, PS) ->
    PS.

%% Install the handshake's keys in the kernel, give the socket to the
%% owner and answer ssl:connect/4 or ssl:handshake/3 with it; the
%% connection process stops right after
-spec handover(#state{}, Info :: [{atom(), term()}]) ->
          {ok, #sslsocket{}} | {error, reason()}.
handover(#state{static_env = #static_env{role = Role, socket = Socket0,
                                         transport_cb = Transport},
                connection_env = #connection_env{user_application = {_, Owner},
                                                 negotiated_version = Version},
                socket_options = #socket_options{mode = Mode, packet = Packet,
                                                 header = Header, packet_size = Size,
                                                 read_ahead = ReadAhead, active = Active},
                ssl_options = SslOpts,
                connection_states = #{current_read := Read,
                                      current_write := Write} = ConnectionStates,
                session = #session{peer_certificate = Cert},
                handshake_env = #handshake_env{alpn = ALPN, negotiated_protocol = NP},
                user_data_buffer = {Front, _, Rear},
                protocol_specific = PS}, Info) ->
    maybe
        {ok, S} ?= transport(Transport, Socket0, PS),
        %% A failure here can leave one direction keyed, so close the socket
        close_on_error(
          S,
          maybe
              %% The kernel sends full size records
              true ?= maps:get(max_fragment_length, ConnectionStates, undefined)
                  =:= undefined orelse {error, {ktls_notsup, max_fragment_length}},
              ok ?= os_check(),
              Protocol = case ALPN of undefined -> NP; _ -> ALPN end,
              ApiInfo = #{role => Role,
                          connection_information =>
                              {ok, [Item || {_, Value} = Item <- Info,
                                            Value =/= undefined]},
                          peercert => result(Cert, no_peercert),
                          negotiated_protocol =>
                              result(Protocol, protocol_not_negotiated)},
              %% Early data the handshake read goes back to the socket
              {ok, Init} ?= init_request(Version, Read, Write, ApiInfo,
                                         [Front | lists:reverse(Rear)]),
              ok ?= drained(S, Init),
              Socket = #sslsocket{socket_handle = S, transport_cb = gen_tcp,
                                  connection_cb = ?MODULE},
              %% Distribution keeps a plain port: dist_util expects
              %% {tcp_closed, Port} and the port's data module
              ok ?= case maps:get(erl_dist, SslOpts, false) of
                        true -> ok;
                        false -> (mod(S)):tls_socket(S, Socket)
                    end,
              ok ?= gen_tcp:controlling_process(S, Owner),
              ok ?= inet:setopts(S, [{mode, Mode}, {packet, Packet}, {header, Header},
                                     {packet_size, Size}, {read_ahead, ReadAhead},
                                     {active, Active}]),
              {ok, Socket}
          end)
    end.

-spec init_request(ssl_record:ssl_version(), ssl_record:connection_state(),
                   ssl_record:connection_state(), map(), iodata()) ->
          {ok, iodata()} | {error, reason()}.
init_request(Version,
             #{security_parameters :=
                   #security_parameters{cipher_suite = Suite,
                                        application_traffic_secret = RxSecret},
               cipher_state := RxCipher, sequence_number := RxSeq},
             #{security_parameters :=
                   #security_parameters{application_traffic_secret = TxSecret},
               cipher_state := TxCipher, sequence_number := TxSeq},
             ApiInfo, Early) ->
    maybe
        {ok, RxInfo} ?= crypto_info(Version, Suite, RxCipher, RxSeq),
        {ok, TxInfo} ?= crypto_info(Version, Suite, TxCipher, TxSeq),
        Blob = term_to_binary(ApiInfo),
        {ok, [?KTLS_INIT, Suite, byte_size(RxSecret), RxSecret, TxSecret,
              byte_size(RxInfo), RxInfo, byte_size(TxInfo), TxInfo,
              <<(byte_size(Blob)):32>>, Blob | Early]}
    end.

-spec close_on_error(handle(), Result) -> Result when
      Result :: {ok, #sslsocket{}} | {error, reason()}.
close_on_error(_, {ok, _} = Ok) -> Ok;
close_on_error(S, {error, _} = Error) -> _ = gen_tcp:close(S), Error.

-spec transport(module(), port() | socket:socket(), map()) ->
          {ok, handle()} | {error, reason()}.
transport(gen_tcp, Port, _) ->
    maybe
        ok ?= inet:setopts(Port, [{active, false}]),
        {ok, Port}
    end;
transport(tls_socket_tcp, Socket, PS) ->
    _ = [socket:cancel(Socket, SelInfo) || #{sel_info := SelInfo} <- [PS],
                                           SelInfo =/= undefined],
    gen_tcp_socket:adopt(Socket);
transport(Transport, _, _) ->
    {error, {transport, Transport}}.

-spec result(term(), atom()) -> {ok, term()} | {error, atom()}.
result(undefined, Reason) -> {error, Reason};
result(Value, _) -> {ok, Value}.

%% The time spent is measured rather than a timer started and cancelled
%% on every call; only a recv that handles a control record needs it
-spec recv(#sslsocket{}, Length :: non_neg_integer(), Timeout :: timeout()) ->
          {ok, string() | binary()} | {error, reason()}.
recv(#sslsocket{socket_handle = S} = Socket, Length, Timeout) ->
    recv(Socket, S, Length, Timeout, erlang:monotonic_time(millisecond)).

-spec recv(#sslsocket{}, handle(), non_neg_integer(), timeout(), integer()) ->
          {ok, string() | binary()} | {error, reason()}.
recv(Socket, S, Length, Timeout, Start) ->
    case gen_tcp:recv(S, Length, Timeout) of
        {error, Reason} when Reason =:= eio; Reason =:= ekeyexpired ->
            maybe
                ok ?= handle_control(Socket, false),
                Now = erlang:monotonic_time(millisecond),
                Left = case Timeout of
                           infinity -> infinity;
                           _ -> max(0, Timeout - (Now - Start))
                       end,
                recv(Socket, S, Length, Left, Now)
            end;
        Result ->
            Result
    end.

%% Handle the control records that paused input, then restore Active
-spec handle_control(#sslsocket{}, active()) -> ok | {error, reason()}.
handle_control(#sslsocket{socket_handle = S}, Active) ->
    case control(S) of
        ok when Active =/= false -> inet:setopts(S, [{active, Active}]);
        Result -> Result
    end.

%% Every control record at the head of the stream. A record holding a
%% KeyUpdate stays there until the request installing its key reads it.
%% A failure closes the socket.
-spec control(handle()) -> ok | {error, reason()}.
control(S) ->
    case request(S, <<?KTLS_CONTROL>>) of
        {ok, <<0>>} ->
            ok;
        {ok, <<1, Type, Data/binary>>} ->
            maybe ok ?= closing(S, record(S, Type, Data)), control(S) end;
        {ok, <<2, Len, State:Len/binary, ?HANDSHAKE, Data/binary>>} ->
            maybe ok ?= closing(S, key_update(S, Data, state(State))), control(S) end;
        {ok, <<3>>} ->
            closing(S, {error, closed});
        {error, _} = Error ->
            closing(S, Error)
    end.

-spec closing(handle(), ok | {error, reason()}) -> ok | {error, reason()}.
closing(_, ok) -> ok;
closing(S, {error, _} = Error) -> _ = close_socket(S), Error.

%% Not yet a gen_tcp_socket around the socket, which then answers
%% {error, closed} until ssl:close/1, as a closed port does
-spec close_socket(handle()) -> ok | {error, reason()}.
close_socket(S) when is_port(S) -> gen_tcp:close(S);
close_socket({'$inet', gen_tcp_socket, {_, Socket}}) -> socket:close(Socket).

%% A control record the socket has read
-spec record(handle(), byte(), binary()) -> ok | {error, reason()}.
record(S, ?ALERT, <<Level, Description>>) ->
    Role = role(S),
    Alert = #alert{level = Level, description = Description,
                   role = ssl_gen_statem:opposite_role(Role)},
    {error, ssl_alert:reason_code(Alert, Role, "TLS", connection)};
record(S, ?ALERT, _) ->
    own_alert(S, ?DECODE_ERROR);
record(S, ?HANDSHAKE, Data) ->
    tickets(S, Data);
record(S, _, _) ->
    own_alert(S, ?UNEXPECTED_MESSAGE).

%% Post-handshake messages, which must be whole: a message split across
%% records is not supported. Tickets received after the handover are not
%% stored.
-spec tickets(handle(), binary()) -> ok | {error, reason()}.
tickets(_, <<>>) ->
    ok;
tickets(S, <<?NEW_SESSION_TICKET, Length:24, _:Length/binary, Rest/binary>>) ->
    tickets(S, Rest);
tickets(S, _) ->
    own_alert(S, ?UNEXPECTED_MESSAGE).

%% A record holding a KeyUpdate, which must end it (RFC 8446, 5.1)
-spec key_update(handle(), binary(), state()) -> ok | {error, reason()}.
key_update(S, <<?NEW_SESSION_TICKET, Length:24, _:Length/binary, Rest/binary>>, State) ->
    key_update(S, Rest, State);
key_update(S, <<?KEY_UPDATE, 1:24, Request>>, State)
  when Request =:= ?UPDATE_NOT_REQUESTED; Request =:= ?UPDATE_REQUESTED ->
    rx_update(S, Request, State);
key_update(S, <<?KEY_UPDATE, 1:24, Request, _/binary>>, _)
  when Request =:= ?UPDATE_NOT_REQUESTED; Request =:= ?UPDATE_REQUESTED ->
    own_alert(S, ?UNEXPECTED_MESSAGE);
key_update(S, <<?KEY_UPDATE, 1:24, _/binary>>, _) ->
    own_alert(S, ?ILLEGAL_PARAMETER);
key_update(S, <<?KEY_UPDATE, _/binary>>, _) ->
    own_alert(S, ?DECODE_ERROR);
key_update(S, _, _) ->
    own_alert(S, ?UNEXPECTED_MESSAGE).

%% The next receive key, and when the peer requests it our next send
%% key, which the socket owes, sending it before any more data, when it
%% cannot be sent now. Installed only if derived from the current
%% secrets: another process may have installed this KeyUpdate's key, and
%% a stale send key is derived again when control/1 meets the KeyUpdate
%% again.
-spec rx_update(handle(), key_update_request(), state()) -> ok | {error, reason()}.
rx_update(S, Request, {Suite, Rx, Tx}) ->
    {RxInfo, RxNew} = next_key(Suite, Rx),
    RxPart = [byte_size(Rx), Rx, RxNew, byte_size(RxInfo), RxInfo],
    Update = case Request of
                 ?UPDATE_NOT_REQUESTED ->
                     [?KTLS_UPDATE, ?KTLS_RX | RxPart];
                 ?UPDATE_REQUESTED ->
                     {TxInfo, TxNew} = next_key(Suite, Tx),
                     [?KTLS_UPDATE, ?KTLS_RX bor ?KTLS_TX bor ?KTLS_MAY_OWE,
                      RxPart, Tx, TxNew, byte_size(TxInfo), TxInfo,
                      ?UPDATE_NOT_REQUESTED]
             end,
    case request(S, Update) of
        {ok, <<_>>} -> ok;
        {error, _} = Error -> Error
    end.

-spec update_keys(#sslsocket{}, write | read_write) -> ok | {error, reason()}.
update_keys(#sslsocket{socket_handle = S}, Type)
  when Type =:= write; Type =:= read_write ->
    Request = case Type of
                  write -> ?UPDATE_NOT_REQUESTED;
                  read_write -> ?UPDATE_REQUESTED
              end,
    closing(S, tx_update(S, Request));
update_keys(_, Type) ->
    {error, {illegal_parameter, Type}}.

%% Our KeyUpdate goes out with the old key and the new key in before
%% any other send; it waits while the send buffer is full. A stale
%% update is derived again from the secret that replaced ours.
-spec tx_update(handle(), key_update_request()) -> ok | {error, reason()}.
tx_update(S, Request) ->
    maybe
        {ok, Bin} ?= request(S, <<?KTLS_STATE>>),
        {Suite, _, Tx} = state(Bin),
        {TxInfo, TxNew} = next_key(Suite, Tx),
        case request(S, [?KTLS_UPDATE, ?KTLS_TX, byte_size(Tx), Tx, TxNew,
                         byte_size(TxInfo), TxInfo, Request]) of
            {ok, <<0>>} -> ok;
            {ok, <<2>>} -> tx_update(S, Request);
            {error, eagain} -> receive after ?SEND_RETRY -> tx_update(S, Request) end;
            {error, _} = Error -> Error
        end
    end.

%% The next key of a direction, in the kernel's format, and its secret
-spec next_key(<<_:16>>, binary()) -> {binary(), binary()}.
next_key(Suite, Secret0) ->
    #{prf := HKDF} = ssl_cipher_format:suite_bin_to_map(Suite),
    Secret = tls_v1:update_traffic_secret(HKDF, Secret0),
    {Key, IV} = tls_v1:calculate_traffic_keys(HKDF, tls_v1:key_length(Suite), Secret),
    {ok, Info} = crypto_info(?TLS_1_3, Suite, #cipher_state{key = Key, iv = IV}, 0),
    {Info, Secret}.

-spec state(binary()) -> state().
state(<<Suite:2/binary, Len, Rx:Len/binary, Tx:Len/binary>>) ->
    {Suite, Rx, Tx}.

-spec controlling_process(#sslsocket{}, pid()) -> ok | {error, reason()}.
controlling_process(#sslsocket{socket_handle = S} = Socket, NewOwner) ->
    maybe
        ok ?= gen_tcp:controlling_process(S, NewOwner),
        %% A paused socket has sent {ssl_control, ...} to the old owner;
        %% the new owner must handle it
        receive
            {ssl_control, Socket, _} = Control -> NewOwner ! Control, ok
        after 0 -> ok
        end
    end.

%% The alert is queued behind the output the socket still holds
-spec close(#sslsocket{}) -> ok | {error, reason()}.
close(#sslsocket{socket_handle = S}) ->
    _ = alert(S, ?WARNING, ?CLOSE_NOTIFY),
    gen_tcp:close(S).

-spec shutdown(#sslsocket{}, read | write | read_write) -> ok | {error, reason()}.
shutdown(#sslsocket{socket_handle = S}, How) ->
    _ = How =:= read orelse alert(S, ?WARNING, ?CLOSE_NOTIFY),
    gen_tcp:shutdown(S, How).

-spec alert(handle(), byte(), byte()) -> {ok, binary()} | {error, reason()}.
alert(S, Level, Description) ->
    request(S, <<?KTLS_ALERT, Level, Description>>).

%% A fatal alert for a record the peer should not have sent. The kernel
%% sends nothing after a record fails to decrypt (EBADMSG).
-spec own_alert(handle(), byte()) -> {error, reason()}.
own_alert(S, Description) ->
    Role = role(S),
    _ = alert(S, ?FATAL, Description),
    Alert = #alert{level = ?FATAL, description = Description,
                   where = ?ST_LOCATION, role = Role},
    {error, ssl_alert:reason_code(Alert, Role, "TLS", connection)}.

-spec role(handle()) -> client | server.
role(S) ->
    case request(S, <<?KTLS_INFO>>) of
        {ok, Info} -> maps:get(role, binary_to_term(Info));
        {error, _} -> client
    end.

%% What the ssl API answers from the handover
-spec info(#sslsocket{}, connection_information | peercert | negotiated_protocol) ->
          {ok, term()} | {error, reason()}.
info(#sslsocket{socket_handle = S}, Key) ->
    case request(S, <<?KTLS_INFO>>) of
        {ok, Info} -> maps:get(Key, binary_to_term(Info));
        {error, _} -> {error, closed}
    end.

%% The module that tags a socket's active messages
-spec mod(handle()) -> prim_inet | gen_tcp_socket.
mod(S) when is_port(S) -> prim_inet;
mod(_) -> gen_tcp_socket.

%% A kTLS request, see erl_ktls.h
-spec request(handle() | socket:socket(), iodata()) ->
          {ok, binary()} | {error, reason()}.
request(S, Request) when is_port(S) ->
    case prim_inet:tls(S, Request) of
        {error, einval} = Error ->
            case erlang:port_info(S, id) of
                undefined -> {error, closed};
                _ -> Error
            end;
        Result ->
            Result
    end;
request({'$inet', gen_tcp_socket, {_, Socket}}, Request) ->
    request(Socket, Request);
request({'$socket', Ref}, Request) ->
    prim_socket:tls(Ref, Request).

%% A port refuses a send key while it still holds output, which must go
%% out with the keys of before; wait, at most ?DEFAULT_TIMEOUT, until it
%% is sent
-spec drained(handle() | socket:socket(), iodata()) -> ok | {error, reason()}.
drained(S, Request) ->
    case request(S, Request) of
        {error, eagain} when is_port(S) ->
            drain(S),
            ok(request(S, Request));
        Result ->
            ok(Result)
    end.

-spec ok({ok, binary()} | {error, reason()}) -> ok | {error, reason()}.
ok({ok, _}) -> ok;
ok({error, _} = Error) -> Error.

-spec drain(port()) -> ok.
drain(S) ->
    case prim_inet:subscribe(S, [subs_empty_out_q]) of
        {ok, [{subs_empty_out_q, N}]} when N > 0 ->
            receive {empty_out_q, S} -> ok after ?DEFAULT_TIMEOUT -> ok end;
        _ ->
            ok
    end.

-spec os_check() -> ok | {error, reason()}.
os_check() ->
    case os:type() of
        {unix, linux} -> ok;
        OS -> {error, {ktls_notsup, {os, OS}}}
    end.

%% Install the keys of a port or socket, with no kTLS state
-spec set_ktls(#{socket := handle() | socket:socket(),
                 tls_version := ssl_record:ssl_version(),
                 cipher_suite := ssl_cipher_format:cipher_suite(),
                 write_state := #cipher_state{}, write_seq := non_neg_integer(),
                 read_state := #cipher_state{}, read_seq := non_neg_integer(),
                 atom() => term()}) -> ok | {error, reason()}.
set_ktls(#{socket := S, tls_version := Version, cipher_suite := Suite,
           write_state := Write, write_seq := WriteSeq,
           read_state := Read, read_seq := ReadSeq}) ->
    maybe
        ok ?= os_check(),
        ok ?= set_key(S, tx, Version, Suite, Write, WriteSeq),
        set_key(S, rx, Version, Suite, Read, ReadSeq)
    end.

-spec set_key(Socket, tx | rx, Version, Suite, #cipher_state{}, Seq) ->
          ok | {error, reason()} when
      Socket :: handle() | socket:socket(),
      Version :: ssl_record:ssl_version(),
      Suite :: ssl_cipher_format:cipher_suite(),
      Seq :: non_neg_integer().
set_key(S, Dir, Version, Suite, CipherState, Seq) ->
    maybe
        {ok, Info} ?= crypto_info(Version, Suite, CipherState, Seq),
        drained(S, [?KTLS_KEY, case Dir of tx -> ?TLS_TX; rx -> ?TLS_RX end,
                    byte_size(Info), Info])
    end.

%% Linux struct tls12_crypto_info_*: version, cipher_type, iv, key, salt, rec_seq
-spec crypto_info(ssl_record:ssl_version(), ssl_cipher_format:cipher_suite(),
                  #cipher_state{}, non_neg_integer()) ->
          {ok, binary()} | {error, {ktls_notsup, term()}}.
crypto_info(?TLS_1_3, Suite,
            #cipher_state{key = Key, iv = <<Salt:4/bytes, IV:8/bytes>> = Nonce}, Seq)
  when is_integer(Seq) ->
    Info = fun(Cipher, CipherInfo) ->
                   {ok, <<?TLS_1_3_VERSION:16/native, Cipher:16/native,
                          CipherInfo/bytes, Seq:64>>}
           end,
    case Suite of
        ?TLS_AES_128_GCM_SHA256 ->
            Info(?TLS_CIPHER_AES_GCM_128, <<IV/bytes, Key/bytes, Salt/bytes>>);
        ?TLS_AES_256_GCM_SHA384 ->
            Info(?TLS_CIPHER_AES_GCM_256, <<IV/bytes, Key/bytes, Salt/bytes>>);
        ?TLS_AES_128_CCM_SHA256 ->
            Info(?TLS_CIPHER_AES_CCM_128, <<IV/bytes, Key/bytes, Salt/bytes>>);
        ?TLS_CHACHA20_POLY1305_SHA256 ->
            Info(?TLS_CIPHER_CHACHA20_POLY1305, <<Nonce/bytes, Key/bytes>>);
        _ ->
            {error, {ktls_notsup, {cipher, Suite}}}
    end;
crypto_info(Version, Suite, _, _) ->
    {error, {ktls_notsup, {cipher, Version, Suite}}}.
