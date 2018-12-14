%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: TODO
%%----------------------------------------------------------------------

%% RFC 8446
%% A.1.  Client
%%
%%                               START <----+
%%                Send ClientHello |        | Recv HelloRetryRequest
%%           [K_send = early data] |        |
%%                                 v        |
%%            /                 WAIT_SH ----+
%%            |                    | Recv ServerHello
%%            |                    | K_recv = handshake
%%        Can |                    V
%%       send |                 WAIT_EE
%%      early |                    | Recv EncryptedExtensions
%%       data |           +--------+--------+
%%            |     Using |                 | Using certificate
%%            |       PSK |                 v
%%            |           |            WAIT_CERT_CR
%%            |           |        Recv |       | Recv CertificateRequest
%%            |           | Certificate |       v
%%            |           |             |    WAIT_CERT
%%            |           |             |       | Recv Certificate
%%            |           |             v       v
%%            |           |              WAIT_CV
%%            |           |                 | Recv CertificateVerify
%%            |           +> WAIT_FINISHED <+
%%            |                  | Recv Finished
%%            \                  | [Send EndOfEarlyData]
%%                               | K_send = handshake
%%                               | [Send Certificate [+ CertificateVerify]]
%%     Can send                  | Send Finished
%%     app data   -->            | K_send = K_recv = application
%%     after here                v
%%                           CONNECTED
%%
%% A.2.  Server
%%
%%                               START <-----+
%%                Recv ClientHello |         | Send HelloRetryRequest
%%                                 v         |
%%                              RECVD_CH ----+
%%                                 | Select parameters
%%                                 v
%%                              NEGOTIATED
%%                                 | Send ServerHello
%%                                 | K_send = handshake
%%                                 | Send EncryptedExtensions
%%                                 | [Send CertificateRequest]
%%  Can send                       | [Send Certificate + CertificateVerify]
%%  app data                       | Send Finished
%%  after   -->                    | K_send = application
%%  here                  +--------+--------+
%%               No 0-RTT |                 | 0-RTT
%%                        |                 |
%%    K_recv = handshake  |                 | K_recv = early data
%%  [Skip decrypt errors] |    +------> WAIT_EOED -+
%%                        |    |       Recv |      | Recv EndOfEarlyData
%%                        |    | early data |      | K_recv = handshake
%%                        |    +------------+      |
%%                        |                        |
%%                        +> WAIT_FLIGHT2 <--------+
%%                                 |
%%                        +--------+--------+
%%                No auth |                 | Client auth
%%                        |                 |
%%                        |                 v
%%                        |             WAIT_CERT
%%                        |        Recv |       | Recv Certificate
%%                        |       empty |       v
%%                        | Certificate |    WAIT_CV
%%                        |             |       | Recv
%%                        |             v       | CertificateVerify
%%                        +-> WAIT_FINISHED <---+
%%                                 | Recv Finished
%%                                 | K_recv = application
%%                                 v
%%                             CONNECTED

-module(tls_connection_1_3).

-include("ssl_alert.hrl").
-include("ssl_connection.hrl").
-include("tls_handshake.hrl").
-include("tls_handshake_1_3.hrl").

%% gen_statem helper functions
-export([start/4,
         negotiated/4
        ]).

start(internal,
      #client_hello{} = Hello,
      #state{connection_states = _ConnectionStates0,
	     ssl_options = #ssl_options{ciphers = _ServerCiphers,
                                        signature_algs = _ServerSignAlgs,
                                        signature_algs_cert = _SignatureSchemes, %% TODO: Check??
                                        supported_groups = _ServerGroups0,
                                        versions = _Versions} = SslOpts,
             session = #session{own_certificate = Cert}} = State0,
      _Module) ->

    Env = #{cert => Cert},
    case tls_handshake_1_3:handle_client_hello(Hello, SslOpts, Env) of
        #alert{} = Alert ->
            ssl_connection:handle_own_alert(Alert, {3,4}, start, State0);
        M ->
            %% update connection_states with cipher
            State = update_state(State0, M),
            {next_state, negotiated, State, [{next_event, internal, M}]}

    end.

%% TODO: move these functions
update_state(#state{connection_states = ConnectionStates0,
                    session = Session} = State,
             #{cipher := Cipher,
               key_share := KeyShare,
               session_id := SessionId}) ->
    #{security_parameters := SecParamsR0} = PendingRead =
        maps:get(pending_read, ConnectionStates0),
    #{security_parameters := SecParamsW0} = PendingWrite =
        maps:get(pending_write, ConnectionStates0),
    SecParamsR = ssl_cipher:security_parameters_1_3(SecParamsR0, Cipher),
    SecParamsW = ssl_cipher:security_parameters_1_3(SecParamsW0, Cipher),
    ConnectionStates =
        ConnectionStates0#{pending_read => PendingRead#{security_parameters => SecParamsR},
                           pending_write => PendingWrite#{security_parameters => SecParamsW}},
    State#state{connection_states = ConnectionStates,
                key_share = KeyShare,
                session = Session#session{session_id = SessionId}}.


negotiated(internal,
           Map,
           #state{connection_states = ConnectionStates0,
                  session = #session{session_id = SessionId,
                                     own_certificate = OwnCert},
                  cert_db = CertDbHandle,
                  cert_db_ref = CertDbRef,
                  ssl_options = #ssl_options{} = SslOpts,
                  key_share = KeyShare,
                  tls_handshake_history = HHistory0,
                  static_env = #static_env{socket = Socket,
                                           transport_cb = Transport}}, _Module) ->

    %% Create server_hello
    %% Extensions: supported_versions, key_share, (pre_shared_key)
    ServerHello = tls_handshake_1_3:server_hello(SessionId, KeyShare,
                                                 ConnectionStates0, Map),
    %% Update handshake_history (done in encode!)
    %% Encode handshake
    {BinMsg, ConnectionStates, HHistory} =
        tls_connection:encode_handshake(ServerHello, {3,4}, ConnectionStates0, HHistory0),
    %% Send server_hello
    tls_connection:send(Transport, Socket, BinMsg),
    Report = #{direction => outbound,
               protocol => 'tls_record',
               message => BinMsg},
    Msg = #{direction => outbound,
            protocol => 'handshake',
            message => ServerHello},
    ssl_logger:debug(SslOpts#ssl_options.log_level, Msg, #{domain => [otp,ssl,handshake]}),
    ssl_logger:debug(SslOpts#ssl_options.log_level, Report, #{domain => [otp,ssl,tls_record]}),

    #{security_parameters := SecParamsR} =
        ssl_record:pending_connection_state(ConnectionStates, read),
    #security_parameters{prf_algorithm = HKDFAlgo,
                         cipher_suite = CipherSuite} = SecParamsR,

    %% Calculate handshake_secret
    EarlySecret = tls_v1:key_schedule(early_secret, HKDFAlgo , {psk, <<>>}),

    ClientKey = maps:get(client_share, Map),        %% Raw data?! What about EC?
    SelectedGroup = maps:get(group, Map),

    PrivateKey = get_server_private_key(KeyShare),  %% #'ECPrivateKey'{}

    IKM = calculate_shared_secret(ClientKey, PrivateKey, SelectedGroup),
    HandshakeSecret = tls_v1:key_schedule(handshake_secret, HKDFAlgo, IKM, EarlySecret),

    %% Calculate [sender]_handshake_traffic_secret
    {Messages, _} =  HHistory,
    ClientHSTrafficSecret =
        tls_v1:client_handshake_traffic_secret(HKDFAlgo, HandshakeSecret, lists:reverse(Messages)),
    ServerHSTrafficSecret =
        tls_v1:server_handshake_traffic_secret(HKDFAlgo, HandshakeSecret, lists:reverse(Messages)),

    %% Calculate traffic keys
    #{cipher := Cipher} = ssl_cipher_format:suite_definition(CipherSuite),
    {ReadKey, ReadIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ClientHSTrafficSecret),
    {WriteKey, WriteIV} = tls_v1:calculate_traffic_keys(HKDFAlgo, Cipher, ServerHSTrafficSecret),

    %% Update pending connection state
    PendingRead0 = ssl_record:pending_connection_state(ConnectionStates0, read),
    PendingWrite0 = ssl_record:pending_connection_state(ConnectionStates0, write),

    PendingRead = update_conn_state(PendingRead0, HandshakeSecret, ReadKey, ReadIV),
    PendingWrite = update_conn_state(PendingWrite0, HandshakeSecret, WriteKey, WriteIV),

    %% Update pending and copy to current (activate)
    %% All subsequent handshake messages are encrypted
    %% ([sender]_handshake_traffic_secret)
    ConnectionStates1 = ConnectionStates0#{current_read => PendingRead,
                                           current_write => PendingWrite,
                                           pending_read => PendingRead,
                                           pending_write => PendingWrite},

    %% Create Certificate, CertificateVerify
    Certificate = tls_handshake_1_3:certificate(OwnCert, CertDbHandle, CertDbRef, <<>>, server),
    io:format("### Certificate: ~p~n", [Certificate]),

    %% CertificateVerify = tls_handshake_1_3:certificate_verify(),

    %% Send Certificate, CertifricateVerify

    %% Send finished

    %% Next record/Next event

    ConnectionStates1.

    %% K_send = handshake ???
    %% (Send EncryptedExtensions)
    %% ([Send CertificateRequest])
    %% [Send Certificate + CertificateVerify]
    %% Send Finished
    %% K_send = application ???

    %% Will be called implicitly
    %% {Record, State} = Connection:next_record(State2#state{session = Session}),
    %% Connection:next_event(wait_flight2, Record, State, Actions),
    %% OR
    %% Connection:next_event(WAIT_EOED, Record, State, Actions)


get_server_private_key(#key_share_server_hello{server_share = ServerShare}) ->
    get_private_key(ServerShare).

get_private_key(#key_share_entry{
                   key_exchange = #'ECPrivateKey'{} = PrivateKey}) ->
    PrivateKey;
get_private_key(#key_share_entry{
                      key_exchange =
                          {_, PrivateKey}}) ->
    PrivateKey.


%% X25519, X448
calculate_shared_secret(OthersKey, MyKey, Group)
  when is_binary(OthersKey) andalso is_binary(MyKey) andalso
       (Group =:= x25519 orelse Group =:= x448)->
    crypto:compute_key(ecdh, OthersKey, MyKey, Group);
%% FFDHE
calculate_shared_secret(OthersKey, MyKey, Group)
  when is_binary(OthersKey) andalso is_binary(MyKey) ->
    Params = #'DHParameter'{prime = P} = ssl_dh_groups:dh_params(Group),
    S = public_key:compute_key(OthersKey, MyKey, Params),
    Size = byte_size(binary:encode_unsigned(P)),
    ssl_cipher:add_zero_padding(S, Size);
%% ECDHE
calculate_shared_secret(OthersKey, MyKey = #'ECPrivateKey'{}, _Group)
  when is_binary(OthersKey) ->
    Point = #'ECPoint'{point = OthersKey},
    public_key:compute_key(Point, MyKey).


update_conn_state(ConnectionState = #{security_parameters := SecurityParameters0},
                  HandshakeSecret, Key, IV) ->
    %% Store secret
    SecurityParameters = SecurityParameters0#security_parameters{
                           master_secret = HandshakeSecret},
    ConnectionState#{security_parameters => SecurityParameters,
                     cipher_state => cipher_init(Key, IV)}.


cipher_init(Key, IV) ->
    #cipher_state{key = Key, iv = IV, tag_len = 16}.
