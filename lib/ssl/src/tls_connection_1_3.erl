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


negotiated(internal,
           Map,
           #state{connection_states = ConnectionStates0,
                  session = #session{session_id = SessionId,
                                     own_certificate = OwnCert},
                  ssl_options = #ssl_options{} = SslOpts,
                  key_share = KeyShare,
                  handshake_env = #handshake_env{tls_handshake_history = HHistory0},
                  private_key = CertPrivateKey,
                  static_env = #static_env{
                                  cert_db = CertDbHandle,
                                  cert_db_ref = CertDbRef,
                                  socket = Socket,                                  
                                  transport_cb = Transport}} = State0, _Module) ->
    Env = #{connection_states => ConnectionStates0,
            session_id => SessionId,
            own_certificate => OwnCert,
            cert_db => CertDbHandle,
            cert_db_ref => CertDbRef,
            ssl_options => SslOpts,
            key_share => KeyShare,
            tls_handshake_history => HHistory0,
            transport_cb => Transport,
            socket => Socket,
            private_key => CertPrivateKey},
    case tls_handshake_1_3:do_negotiated(Map, Env) of
        #alert{} = Alert ->
            ssl_connection:handle_own_alert(Alert, {3,4}, negotiated, State0);
        M ->
            %% TODO: implement update_state
            %% State = update_state(State0, M),
            {next_state, wait_flight2, State0, [{next_event, internal, M}]}

    end.


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
