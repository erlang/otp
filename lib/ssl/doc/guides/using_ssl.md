<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Using SSL application API

To see relevant version information for ssl, call `ssl:versions/0` .

To see all supported cipher suites, call
[`ssl:cipher_suites(all, 'tlsv1.3')` ](`ssl:cipher_suites/2`). The available
cipher suites for a connection depend on the TLS version and pre TLS-1.3 also on
the certificate. To see the default cipher suite list change `all` to `default`.
Note that TLS 1.3 and previous versions do not have any cipher suites in common,
for listing cipher suites for a specific version use
[`ssl:cipher_suites(exclusive, 'tlsv1.3')` ](`ssl:cipher_suites/2`). Specific
cipher suites that you want your connection to use can also be specified.
Default is to use the strongest available.

The following sections shows small examples of how to set up client/server
connections using the Erlang shell. The returned value of the `sslsocket` is
abbreviated with `[...]` as it can be fairly large and is opaque to the user
except for the purpose of pattern matching.

> #### Note {: .info }
>
> Note that client certificate verification is optional for the server and needs
> additional conguration on both sides to work. The Certificate and keys, in the
> examples, are provided using the [certs_keys](`t:ssl:certs_keys/0`) option
> introduced in OTP-25.

## Basic Client

```erlang
 1 > ssl:start(), ssl:connect("google.com", 443, [{verify, verify_peer},
    {cacerts, public_key:cacerts_get()}]).
   {ok,{sslsocket, [...]}}
```

## Basic Connection

_Step 1:_ Start the server side:

```erlang
1 server> ssl:start().
ok
```

_Step 2:_ with alternative certificates, in this example the EDDSA certificate
will be preferred if TLS-1.3 is negotiated and the RSA certificate will always
be used for TLS-1.2 as it does not support the EDDSA algorithm:

```erlang
2 server> {ok, ListenSocket} =
ssl:listen(9999, [{certs_keys, [#{certfile => "eddsacert.pem",
                                  keyfile => "eddsakey.pem"},
			        #{certfile => "rsacert.pem",
                                  keyfile => "rsakey.pem",
			          password => "foobar"}
			       ]},{reuseaddr, true}]).
{ok,{sslsocket, [...]}}
```

_Step 3:_ Do a transport accept on the TLS listen socket:

```erlang
3 server> {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket).
{ok,{sslsocket, [...]}}
```

> #### Note {: .info }
>
> ssl:transport_accept/1 and ssl:handshake/2 are separate functions so that the
> handshake part can be called in a new erlang process dedicated to handling the
> connection

_Step 4:_ Start the client side:

```erlang
1 client> ssl:start().
ok
```

Be sure to configure trusted certificates to use for server certificate
verification.

```erlang
2 client> {ok, Socket} = ssl:connect("localhost", 9999,
      [{verify, verify_peer},
      {cacertfile, "cacerts.pem"}, {active, once}], infinity).
{ok,{sslsocket, [...]}}
```

_Step 5:_ Do the TLS handshake:

```erlang
4 server> {ok, Socket} = ssl:handshake(TLSTransportSocket).
{ok,{sslsocket, [...]}}
```

> #### Note {: .info }
>
> A real server should use ssl:handshake/2 that has a timeout to avoid DoS
> attacks. In the example the timeout defaults to infinty.

_Step 6:_ Send a message over TLS:

```erlang
5 server> ssl:send(Socket, "foo").
ok
```

_Step 7:_ Flush the shell message queue to see that the message sent on the
server side is recived by the client side:

```erlang
3 client> flush().
Shell got {ssl,{sslsocket,[...]},"foo"}
ok
```

## Upgrade Example - TLS only

Upgrading a a TCP/IP connection to a TLS connections is mostly used when there
is a desire have unencrypted communication first and then later secure the
communication channel by using TLS. Note that the client and server need to
agree to do the upgrade in the protocol doing the communication. This is concept
is often referenced as `STARTLS` and used in many protocols such as `SMTP`,
`FTPS` and `HTTPS` via a proxy.

> #### Warning {: .warning }
>
> Maximum security recommendations are however moving away from such solutions.

To upgrade to a TLS connection:

_Step 1:_ Start the server side:

```erlang
1 server> ssl:start().
  ok
```

_Step 2:_ Create a normal TCP listen socket and ensure `active` is set to
`false` and not set to any active mode otherwise TLS handshake messages can be
delivered to the wrong process.

```erlang
2 server> {ok, ListenSocket} = gen_tcp:listen(9999, [{reuseaddr, true},
  {active, false}]).
  {ok, #Port<0.475>}
```

_Step 3:_ Accept client connection:

```erlang
3 server> {ok, Socket} = gen_tcp:accept(ListenSocket).
  {ok, #Port<0.476>}
```

_Step 4:_ Start the client side:

```erlang
1 client> ssl:start().
  ok
```

```erlang
2 client> {ok, Socket} = gen_tcp:connect("localhost", 9999,  [], infinity).
```

_Step 5:_ Do the TLS handshake:

```erlang
4 server> {ok, TLSSocket} = ssl:handshake(Socket, [{verify, verify_peer},
  {fail_if_no_peer_cert, true},
  {cacertfile, "cacerts.pem"},
  {certs_keys, [#{certfile => "cert.pem", keyfile => "key.pem"}]}]).
  {ok,{sslsocket,[...]}}
```

_Step 6:_ Upgrade to a TLS connection. The client and server must agree upon the
upgrade. The server must be prepared to be a TLS server before the client can do
a successful connect.

```erlang
3 client>{ok, TLSSocket} = ssl:connect(Socket, [{verify, verify_peer},
  {cacertfile, "cacerts.pem"},
  {certs_keys, [#{certfile => "cert.pem", keyfile => "key.pem"}]}], infinity).
{ok,{sslsocket,[...]}}
```

_Step 7:_ Send a message over TLS:

```erlang
4 client> ssl:send(TLSSocket, "foo").
      ok
```

_Step 8:_ Set `active once` on the TLS socket:

```erlang
5 server> ssl:setopts(TLSSocket, [{active, once}]).
      ok
```

_Step 9:_ Flush the shell message queue to see that the message sent on the
client side is recived by the server side:

```erlang
5 server> flush().
      Shell got {ssl,{sslsocket,[...]},"foo"}
      ok
```

## Customizing cipher suites

Fetch default cipher suite list for a TLS/DTLS version. Change default to all to
get all possible cipher suites.

```erlang
1>  Default = ssl:cipher_suites(default, 'tlsv1.2').
    [#{cipher => aes_256_gcm,key_exchange => ecdhe_ecdsa,
    mac => aead,prf => sha384}, ....]
```

In OTP 20 it is desirable to remove all cipher suites that uses rsa key exchange
(removed from default in 21)

```erlang
2> NoRSA =
    ssl:filter_cipher_suites(Default,
                            [{key_exchange, fun(rsa) -> false;
			                        (_) -> true
			                    end}]).
    [...]
```

Pick just a few suites

```erlang
 3> Suites =
    ssl:filter_cipher_suites(Default,
                            [{key_exchange, fun(ecdh_ecdsa) -> true;
			                        (_) -> false
			                    end},
                             {cipher, fun(aes_128_cbc) -> true;
			                  (_) ->false
			              end}]).
    [#{cipher => aes_128_cbc,key_exchange => ecdh_ecdsa,
     mac => sha256,prf => sha256},
     #{cipher => aes_128_cbc,key_exchange => ecdh_ecdsa,mac => sha,
     prf => default_prf}]
```

Make some particular suites the most preferred, or least preferred by changing
prepend to append.

```erlang
 4>ssl:prepend_cipher_suites(Suites, Default).
  [#{cipher => aes_128_cbc,key_exchange => ecdh_ecdsa,
     mac => sha256,prf => sha256},
   #{cipher => aes_128_cbc,key_exchange => ecdh_ecdsa,mac => sha,
     prf => default_prf},
   #{cipher => aes_256_cbc,key_exchange => ecdhe_ecdsa,
     mac => sha384,prf => sha384}, ...]
```

## Customizing signature algorithms(TLS-1.2)/schemes(TLS-1.3)

Starting from TLS-1.2 signature algorithms (called signature schemes in TLS-1.3)
is something that can be negotiated and hence also configured. These
algorithms/schemes will be used for digital signatures in protocol messages and
in certificates.

> #### Note {: .info }
>
> TLS-1.3 schemes have atom names whereas TLS-1.2 configuration is two element
> tuples composed by one hash algorithm and one signature algorithm. When both
> versions are supported the configuration can be a mix of these as both
> versions might be negotiated. All `rsa_pss` based schemes are back ported to
> TLS-1.2 and can be used also in a TLS-1.2 configuration. In TLS-1.2 the
> signature algorithms chosen by the server will also be affected by the chiper
> suite that is chosen, which is not the case in TLS-1.3.

Using the function `ssl:signature_algs/2` will let you inspect diffrent aspects
of possible configurations for your system. For example if TLS-1.3 and TLS-1.2
is supported the default signature_algorithm list in OTP-26 and cryptolib from
OpenSSL 3.0.2 would look like:

```erlang
 1>  ssl:signature_algs(default, 'tlsv1.3').
 %% TLS-1.3 schemes
 [eddsa_ed25519,eddsa_ed448,ecdsa_secp521r1_sha512,
  ecdsa_secp384r1_sha384,ecdsa_secp256r1_sha256,
  rsa_pss_pss_sha512,rsa_pss_pss_sha384,rsa_pss_pss_sha256,
  rsa_pss_rsae_sha512,rsa_pss_rsae_sha384,rsa_pss_rsae_sha256,
  %% Legacy schemes only valid for certificate signatures in TLS-1.3
  %% (would have a tuple name in TLS-1.2 only configuration)
  rsa_pkcs1_sha512,rsa_pkcs1_sha384,rsa_pkcs1_sha256
  %% TLS 1.2 algorithms
  {sha512,ecdsa},
  {sha384,ecdsa},
  {sha256,ecdsa}]
```

If you want to add support for non default supported algorithms you should
append them to the default list as the configuration is in prefered order,
something like this:

```erlang
    MySignatureAlgs = ssl:signature_algs(default, 'tlsv1.3') ++ [{sha, rsa}, {sha, dsa}],
    ssl:connect(Host,Port,[{signature_algs, MySignatureAlgs,...]}),
    ...
```

See also `ssl:signature_algs/2` and [sign_algo()](`t:ssl:signature_algs/0`)

## Using an Engine Stored Key

Erlang ssl application is able to use private keys provided by OpenSSL engines
using the following mechanism:

```erlang
1> ssl:start().
ok
```

Load a crypto engine, should be done once per engine used. For example
dynamically load the engine called `MyEngine`:

```erlang
2> {ok, EngineRef} =
crypto:engine_load(<<"dynamic">>,
[{<<"SO_PATH">>, "/tmp/user/engines/MyEngine"},<<"LOAD">>],
[]).
{ok,#Ref<0.2399045421.3028942852.173962>}
```

Create a map with the engine information and the algorithm used by the engine:

```erlang
3> PrivKey =
 #{algorithm => rsa,
   engine => EngineRef,
   key_id => "id of the private key in Engine"}.
```

Use the map in the ssl key option:

```erlang
4> {ok, SSLSocket} =
ssl:connect("localhost", 9999,
            [{cacertfile, "cacerts.pem"},
	    {certs_keys, [#{certfile => "cert.pem", key => PrivKey}]}
	    ], infinity).
```

See also [crypto documentation](`e:crypto:engine_load.md#engine_load`)

## NSS keylog

The NSS keylog debug feature can be used by authorized users to for instance
enable wireshark to decrypt TLS packets.

_Server (with NSS key logging)_

```erlang
    server() ->
        application:load(ssl),
        {ok, _} = application:ensure_all_started(ssl),
        Port = 11029,
        LOpts = [{certs_keys, [#{certfile => "cert.pem", keyfile => "key.pem"}]},
        {reuseaddr, true},
        {versions, ['tlsv1.2','tlsv1.3']},
        {keep_secrets, true} %% Enable NSS key log (debug option)
        ],
        {ok, LSock} = ssl:listen(Port, LOpts),
        {ok, ASock} = ssl:transport_accept(LSock),
        {ok, CSock} = ssl:handshake(ASock).
```

_Exporting the secrets_

```erlang
      {ok, [{keylog, KeylogItems}]} = ssl:connection_information(CSock, [keylog]).
      file:write_file("key.log", [[KeylogItem,$\n] || KeylogItem <- KeylogItems]).
```

## Session Reuse pre TLS 1.3

Clients can request to reuse a session established by a previous full handshake
between that client and server by sending the id of the session in the initial
handshake message. The server may or may not agree to reuse it. If agreed the
server will send back the id and if not it will send a new id. The ssl
application has several options for handling session reuse.

On the client side the ssl application will save session data to try to automate
session reuse on behalf of the client processes on the Erlang node. Note that
only verified sessions will be saved for security reasons, that is session
resumption relies on the certificate validation to have been run in the original
handshake. To minimize memory consumption only unique sessions will be saved
unless the special `save` value is specified for the following option
`{reuse_sessions, boolean() | save}` in which case a full handshake will be
performed and that specific session will have been saved before the handshake
returns. The session id and even an opaque binary containing the session data
can be retrieved using `ssl:connection_information/1` function. A saved session
(guaranteed by the save option) can be explicitly reused using
`{reuse_session, SessionId}`. Also it is possible for the client to reuse a
session that is not saved by the ssl application using
`{reuse_session, {SessionId, SessionData}}`.

> #### Note {: .info }
>
> When using explicit session reuse, it is up to the client to make sure that
> the session being reused is for the correct server and has been verified.

Here follows a client side example, divide into several steps for readability.

Step 1 - Automated Session Reuse

```erlang
1> ssl:start().
ok

2> {ok, C1} = ssl:connect("localhost", 9999, [{verify, verify_peer},
					      {versions, ['tlsv1.2']},
					      {cacertfile, "cacerts.pem"}]).
{ok,{sslsocket,{gen_tcp,#Port<0.7>,tls_connection,undefined}, ...}}

3> ssl:connection_information(C1, [session_id]).
{ok,[{session_id,<<95,32,43,22,35,63,249,22,26,36,106,
                   152,49,52,124,56,130,192,137,161,
                   146,145,164,232,...>>}]}

%% Reuse session if possible, note that if C2 is really fast the session
%% data might not be available for reuse.
4> {ok, C2} = ssl:connect("localhost", 9999, [{verify, verify_peer},
					      {versions, ['tlsv1.2']},
					      {cacertfile, "cacerts.pem"},
					      {reuse_sessions, true}]).
{ok,{sslsocket,{gen_tcp,#Port<0.8>,tls_connection,undefined}, ...]}}

%% C2 got same session ID as client one, session was automatically reused.
5> ssl:connection_information(C2, [session_id]).
{ok,[{session_id,<<95,32,43,22,35,63,249,22,26,36,106,
                   152,49,52,124,56,130,192,137,161,
                   146,145,164,232,...>>}]}
```

Step 2- Using `save` Option

```erlang
%% We want save this particular session for
%% reuse although it has the same basis as C1
6> {ok, C3} = ssl:connect("localhost", 9999, [{verify, verify_peer},
					      {versions, ['tlsv1.2']},
					      {cacertfile, "cacerts.pem"},
					      {reuse_sessions, save}]).
{ok,{sslsocket,{gen_tcp,#Port<0.9>,tls_connection,undefined}, ...]}}

%% A full handshake is performed and we get a new session ID
7> {ok, [{session_id, ID}]} = ssl:connection_information(C3, [session_id]).
{ok,[{session_id,<<91,84,27,151,183,39,84,90,143,141,
                   121,190,66,192,10,1,27,192,33,95,78,
                   8,34,180,...>>}]}

%% Use automatic session reuse
8> {ok, C4} = ssl:connect("localhost", 9999, [{verify, verify_peer},
					      {versions, ['tlsv1.2']},
					      {cacertfile, "cacerts.pem"},
					      {reuse_sessions, true}]).
{ok,{sslsocket,{gen_tcp,#Port<0.10>,tls_connection,
                        undefined}, ...]}}

%% The "saved" one happened to be selected, but this is not a guarantee
9> ssl:connection_information(C4, [session_id]).
{ok,[{session_id,<<91,84,27,151,183,39,84,90,143,141,
                   121,190,66,192,10,1,27,192,33,95,78,
                   8,34,180,...>>}]}

%% Make sure to reuse the "saved" session
10> {ok, C5} = ssl:connect("localhost", 9999, [{verify, verify_peer},
					       {versions, ['tlsv1.2']},
					       {cacertfile, "cacerts.pem"},
					       {reuse_session, ID}]).
{ok,{sslsocket,{gen_tcp,#Port<0.11>,tls_connection,
                        undefined}, ...]}}

11> ssl:connection_information(C5, [session_id]).
{ok,[{session_id,<<91,84,27,151,183,39,84,90,143,141,
                   121,190,66,192,10,1,27,192,33,95,78,
                   8,34,180,...>>}]}
```

Step 3 - Explicit Session Reuse

```erlang
%% Perform a full handshake and the session will not be saved for reuse
12> {ok, C9} =
ssl:connect("localhost", 9999, [{verify, verify_peer},
				{versions, ['tlsv1.2']},
		                {cacertfile, "cacerts.pem"},
                                {reuse_sessions, false},
	                        {server_name_indication, disable}]).
{ok,{sslsocket,{gen_tcp,#Port<0.14>,tls_connection, ...}}

%% Fetch session ID and data for C9 connection
12> {ok, [{session_id, ID1}, {session_data, SessData}]} =
	ssl:connection_information(C9, [session_id, session_data]).
{ok,[{session_id,<<9,233,4,54,170,88,170,180,17,96,202,
                   85,85,99,119,47,9,68,195,50,120,52,
                   130,239,...>>},
     {session_data,<<131,104,13,100,0,7,115,101,115,115,105,
                     111,110,109,0,0,0,32,9,233,4,54,170,...>>}]}

%% Explicitly reuse the session from C9
13> {ok, C10} = ssl:connect("localhost", 9999, [{verify, verify_peer},
						{versions, ['tlsv1.2']},
						{cacertfile, "cacerts.pem"},
						{reuse_session, {ID1, SessData}}]).
{ok,{sslsocket,{gen_tcp,#Port<0.15>,tls_connection,
                        undefined}, ...}}

14> ssl:connection_information(C10, [session_id]).
{ok,[{session_id,<<9,233,4,54,170,88,170,180,17,96,202,
                   85,85,99,119,47,9,68,195,50,120,52,
                   130,239,...>>}]}
```

Step 4 - Not Possible to Reuse Explicit Session by ID Only

```erlang
%% Try to reuse the session from C9 using only the id
15> {ok, E} = ssl:connect("localhost", 9999, [{verify, verify_peer},
				              {versions, ['tlsv1.2']},
				              {cacertfile, "cacerts.pem"},
					      {reuse_session, ID1}]).
{ok,{sslsocket,{gen_tcp,#Port<0.18>,tls_connection,
                        undefined}, ...}}

%% This will fail (as it is not saved for reuse)
%% and a full handshake will be performed, we get a new id.
16>  ssl:connection_information(E, [session_id]).
{ok,[{session_id,<<87,46,43,126,175,68,160,153,37,29,
                   196,240,65,160,254,88,65,224,18,63,
                   18,17,174,39,...>>}]}
```

On the server side the the `{reuse_sessions, boolean()}` option determines if
the server will save session data and allow session reuse or not. This can be
further customized by the option `{reuse_session, fun()}` that may introduce a
local policy for session reuse.

## Session Tickets and Session Resumption in TLS 1.3

TLS 1.3 introduces a new secure way of resuming sessions by using session
tickets. A session ticket is an opaque data structure that is sent in the
pre_shared_key extension of a ClientHello, when a client attempts to resume a
session with keying material from a previous successful handshake.

Session tickets can be stateful or stateless. A stateful session ticket is a
database reference (session ticket store) and used with stateful servers, while
a stateless ticket is a self-encrypted and self-authenticated data structure
with cryptographic keying material and state data, enabling session resumption
with stateless servers.

The choice between stateful or stateless depends on the server requirements as
the session tickets are opaque for the clients. Generally, stateful tickets are
smaller and the server can guarantee that tickets are only used once. Stateless
tickets contain additional data, require less storage on the server side, but
they offer different guarantees against anti-replay. See also
[Anti-Replay Protection in TLS 1.3](using_ssl.md#anti-replay-protection-in-tls-1-3)

Session tickets are sent by servers on newly established TLS connections. The
number of tickets sent and their lifetime are configurable by application
variables. See also [SSL's configuration](ssl_app.md#configuration).

Session tickets are protected by application traffic keys, and in stateless
tickets, the opaque data structure itself is self-encrypted.

An example with automatic and manual session resumption:

```erlang
      {ok, _} = application:ensure_all_started(ssl).
      LOpts = [{certs_keys, [#{certfile => "cert.pem",
                               keyfile => "key.pem"}]},
               {versions, ['tlsv1.2','tlsv1.3']},
               {session_tickets, stateless}].
      {ok, LSock} = ssl:listen(8001, LOpts).
      {ok, ASock} = ssl:transport_accept(LSock).
```

_Step 2 (client):_ Start the client and connect to server:

```erlang
      {ok, _} = application:ensure_all_started(ssl).
      COpts = [{cacertfile, "cert.pem"},
               {versions, ['tlsv1.2','tlsv1.3']},
               {log_level, debug},
               {session_tickets, auto}].
      ssl:connect("localhost", 8001, COpts).
```

_Step 3 (server):_ Start the TLS handshake:

```erlang
      {ok, CSocket} = ssl:handshake(ASock).
```

A connection is established using a full handshake. Below is a summary of the
exchanged messages:

```erlang
      >>> TLS 1.3 Handshake, ClientHello ...
      <<< TLS 1.3 Handshake, ServerHello ...
      <<< Handshake, EncryptedExtensions ...
      <<< Handshake, Certificate ...
      <<< Handshake, CertificateVerify ...
      <<< Handshake, Finished ...
      >>> Handshake, Finished ...
      <<< Post-Handshake, NewSessionTicket ...
```

At this point the client has stored the received session tickets and ready to
use them when establishing new connections to the same server.

_Step 4 (server):_ Accept a new connection on the server:

```erlang
      {ok, ASock2} = ssl:transport_accept(LSock).
```

_Step 5 (client):_ Make a new connection:

```erlang
      ssl:connect("localhost", 8001, COpts).
```

_Step 6 (server):_ Start the handshake:

```erlang
      {ok, CSock2} =ssl:handshake(ASock2).
```

The second connection is a session resumption using keying material from the
previous handshake:

```erlang
      >>> TLS 1.3 Handshake, ClientHello ...
      <<< TLS 1.3 Handshake, ServerHello ...
      <<< Handshake, EncryptedExtensions ...
      <<< Handshake, Finished ...
      >>> Handshake, Finished ...
      <<< Post-Handshake, NewSessionTicket ...
```

Manual handling of session tickets is also supported. In manual mode, it is the
responsibility of the client to handle received session tickets.

_Step 7 (server):_ Accept a new connection on the server:

```erlang
      {ok, ASock3} = ssl:transport_accept(LSock).
```

_Step 8 (client):_ Make a new connection to server:

```erlang
      {ok, _} = application:ensure_all_started(ssl).
      COpts2 = [{cacertfile, "cacerts.pem"},
                {versions, ['tlsv1.2','tlsv1.3']},
                {log_level, debug},
                {session_tickets, manual}].
      ssl:connect("localhost", 8001, COpts).
```

_Step 9 (server):_ Start the handshake:

```erlang
      {ok, CSock3} = ssl:handshake(ASock3).
```

After the handshake is performed, the user process receivess messages with the
tickets sent by the server.

_Step 10 (client):_ Receive a new session ticket:

```erlang
      Ticket = receive {ssl, session_ticket, {_, TicketData}} -> TicketData end.
```

_Step 11 (server):_ Accept a new connection on the server:

```erlang
      {ok, ASock4} = ssl:transport_accept(LSock).
```

_Step 12 (client):_ Initiate a new connection to the server with the session
ticket received in Step 10:

```erlang
      {ok, _} = application:ensure_all_started(ssl).
      COpts2 = [{cacertfile, "cert.pem"},
                {versions, ['tlsv1.2','tlsv1.3']},
                {log_level, debug},
                {session_tickets, manual},
                {use_ticket, [Ticket]}].
      ssl:connect("localhost", 8001, COpts).
```

_Step 13 (server):_ Start the handshake:

```erlang
      {ok, CSock4} = ssl:handshake(ASock4).
```

## Early Data in TLS-1.3

TLS 1.3 allows clients to send data on the first flight if the endpoints have a
shared crypographic secret (pre-shared key). This means that clients can send
early data if they have a valid session ticket received in a previous successful
handshake. For more information about session resumption see
[Session Tickets and Session Resumption in TLS 1.3](using_ssl.md#session-tickets-and-session-resumption-in-tls-1-3).

The security properties of Early Data are weaker than other kinds of TLS data.
This data is not forward secret, and it is vulnerable to replay attacks. For
available mitigation strategies see
[Anti-Replay Protection in TLS 1.3](using_ssl.md#anti-replay-protection-in-tls-1-3).

In normal operation, clients will not know which, if any, of the available
mitigation strategies servers actually implement, and hence must only send early
data which they deem safe to be replayed. For example, idempotent HTTP
operations, such as HEAD and GET, can usually be regarded as safe but even they
can be exploited by a large number of replays causing resource limit exhaustion
and other similar problems.

An example of sending early data with automatic and manual session ticket
handling:

_Server_

```erlang
    early_data_server() ->
        application:load(ssl),
        {ok, _} = application:ensure_all_started(ssl),
        Port = 11029,
        LOpts = [{certs_keys, [#{certfile => "cert.pem", keyfile => "key.pem"}]},
        {reuseaddr, true},
        {versions, ['tlsv1.2','tlsv1.3']},
        {session_tickets, stateless},
        {early_data, enabled},
        ],
        {ok, LSock} = ssl:listen(Port, LOpts),
        %% Accept first connection
        {ok, ASock0} = ssl:transport_accept(LSock),
        {ok, CSock0} = ssl:handshake(ASock0),
        %% Accept second connection
        {ok, ASock1} = ssl:transport_accept(LSock),
        {ok, CSock1} = ssl:handshake(ASock1),
        Sock.
```

_Client (automatic ticket handling):_

```erlang
    early_data_auto() ->
        %% First handshake 1-RTT - get session tickets
	application:load(ssl),
	{ok, _} = application:ensure_all_started(ssl),
	Port = 11029,
	Data = <<"HEAD / HTTP/1.1\r\nHost: \r\nConnection: close\r\n">>,
	COpts0 = [{cacertfile, "cacerts.pem"},
	          {versions, ['tlsv1.2', 'tlsv1.3']},
	          {session_tickets, auto}],
        {ok, Sock0} = ssl:connect("localhost", Port, COpts0),

        %% Wait for session tickets
	timer:sleep(500),
	%% Close socket if server cannot handle multiple
        %% connections e.g. openssl s_server
	ssl:close(Sock0),

        %% Second handshake 0-RTT
	COpts1 = [{cacertfile,  "cacerts.pem"},
	          {versions, ['tlsv1.2', 'tlsv1.3']},
		  {session_tickets, auto},
		  {early_data, Data}],
        {ok, Sock} = ssl:connect("localhost", Port, COpts1),
	Sock.
```

_Client (manual ticket handling):_

```erlang
    early_data_manual() ->
        %% First handshake 1-RTT - get session tickets
	application:load(ssl),
	{ok, _} = application:ensure_all_started(ssl),
	Port = 11029,
	Data = <<"HEAD / HTTP/1.1\r\nHost: \r\nConnection: close\r\n">>,
	COpts0 = [{cacertfile, "cacerts.pem"},
	          {versions, ['tlsv1.2', 'tlsv1.3']},
	          {session_tickets, manual}],
        {ok, Sock0} = ssl:connect("localhost", Port, COpts0),

        %% Wait for session tickets
	Ticket =
	    receive
	        {ssl, session_ticket, Ticket0} ->
		    Ticket0
            end,

       %% Close socket if server cannot handle multiple connections
       %% e.g. openssl s_server
       ssl:close(Sock0),

       %% Second handshake 0-RTT
       COpts1 = [{cacertfile, "cacerts.pem"},
                 {versions, ['tlsv1.2', 'tlsv1.3']},
		 {session_tickets, manual},
		 {use_ticket, [Ticket]},
		 {early_data, Data}],
       {ok, Sock} = ssl:connect("localhost", Port, COpts1),
       Sock.
```

## Anti-Replay Protection in TLS 1.3

The TLS 1.3 protocol does not provide inherent protection for replay of 0-RTT
data but describes mechanisms that SHOULD be implemented by compliant server
implementations. The implementation of TLS 1.3 in the SSL application employs
all standard methods to prevent potential threats.

_Single-use tickets_

This mechanism is available with stateful session tickets. Session tickets can
only be used once, subsequent use of the same ticket results in a full
handshake. Stateful servers enforce this rule by maintaining a database of
outstanding valid tickets.

_Client Hello Recording_

This mechanism is available with stateless session tickets. The server records a
unique value derived from the ClientHello (PSK binder) in a given time window.
The ticket's age is verified by using both the "obsfuscated_ticket_age" and an
additional timestamp encrypted in the ticket data. As the used datastore allows
false positives, apparent replays will be answered by doing a full 1-RTT
handshake.

_Freshness Checks_

This mechanism is available with the stateless session tickets. As the ticket
data has an embedded timestamp, the server can determine if a ClientHello was
sent reasonably recently and accept the 0-RTT handshake, otherwise if falls back
to a full 1-RTT handshake. This mechanism is tightly coupled with the previous
one, it prevents storing an unlimited number of ClientHellos.

The current implementation uses a pair of Bloom filters to implement the last
two mechanisms. Bloom filters are fast, memory-efficient, probabilistic data
structures that can tell if an element may be in a set or if it is definitely
not in the set.

If the option [anti_replay](`t:ssl:anti_replay/0`) is defined in the server, a
pair of Bloom filters (_current_ and _old_) are used to record incoming
ClientHello messages (it is the unique binder value that is actually stored).
The _current_ Bloom filter is used for `WindowSize` seconds to store new
elements. At the end of the time window the Bloom filters are rotated (the
_current_ Bloom filter becomes the _old_ and an empty Bloom filter is set as
_current_.

The Anti-Replay protection feature in stateless servers executes in the
following steps when a new ClientHello is received:

- Reported ticket age (obfuscated ticket age) shall be less than ticket
  lifetime.
- Actual ticket age shall be less than the ticket lifetime (stateless session
  tickets contain the servers timestamp when the ticket was issued).
- ClientHello created with the ticket shall be sent relatively recently
  (freshness checks).
- If all above checks passed both _current_ and _old_ Bloom filters are checked
  to detect if binder was already seen. Being a probabilistic data structure,
  false positives can occur and they trigger a full handshake.
- If the binder is not seen, the binder is validated. If the binder is valid,
  the server proceeds with the 0-RTT handshake.

## Using DTLS

Using DTLS has basically the same API as TLS. You need to add the option
\{protocol, dtls\} to the connect and listen functions. For example

```erlang
 client> {ok, Socket} = ssl:connect("localhost", 9999, [{protocol, dtls},
{verify, verify_peer},{cacertfile, "cacerts.pem"}], infinity).
{ok,{sslsocket, [...]}}
```
