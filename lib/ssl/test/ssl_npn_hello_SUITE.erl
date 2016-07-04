%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(ssl_npn_hello_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("tls_handshake.hrl").
-include("tls_record.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [encode_and_decode_npn_client_hello_test,
     encode_and_decode_npn_server_hello_test,
     encode_and_decode_client_hello_test,
     encode_and_decode_server_hello_test,
     create_server_hello_with_advertised_protocols_test,
     create_server_hello_with_no_advertised_protocols_test].

init_per_testcase(_TestCase, Config) ->
    ssl_test_lib:ct_log_supported_protocol_versions(Config),
    ct:timetrap({seconds, 5}),
    Config.

end_per_testcase(_TestCase, Config) ->     
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

encode_and_decode_client_hello_test(Config) ->
    HandShakeData = create_client_handshake(undefined),
    Version = ssl_test_lib:protocol_version(Config),
    {[{DecodedHandshakeMessage, _Raw}], _} =
	tls_handshake:get_tls_handshake(Version, list_to_binary(HandShakeData), <<>>, #ssl_options{}),
    NextProtocolNegotiation = (DecodedHandshakeMessage#client_hello.extensions)#hello_extensions.next_protocol_negotiation,
    NextProtocolNegotiation = undefined.
%%--------------------------------------------------------------------
encode_and_decode_npn_client_hello_test(Config) ->
    HandShakeData = create_client_handshake(#next_protocol_negotiation{extension_data = <<>>}),
    Version = ssl_test_lib:protocol_version(Config),
    {[{DecodedHandshakeMessage, _Raw}], _} =
	tls_handshake:get_tls_handshake(Version, list_to_binary(HandShakeData), <<>>,  #ssl_options{}),
    NextProtocolNegotiation = (DecodedHandshakeMessage#client_hello.extensions)#hello_extensions.next_protocol_negotiation,
    NextProtocolNegotiation = #next_protocol_negotiation{extension_data = <<>>}.
%%--------------------------------------------------------------------
encode_and_decode_server_hello_test(Config) ->
    HandShakeData = create_server_handshake(undefined),
    Version = ssl_test_lib:protocol_version(Config),
    {[{DecodedHandshakeMessage, _Raw}], _} =
	tls_handshake:get_tls_handshake(Version, list_to_binary(HandShakeData), <<>>, #ssl_options{}),
    NextProtocolNegotiation = (DecodedHandshakeMessage#server_hello.extensions)#hello_extensions.next_protocol_negotiation,
    NextProtocolNegotiation = undefined.
%%--------------------------------------------------------------------
encode_and_decode_npn_server_hello_test(Config) ->
    HandShakeData = create_server_handshake(#next_protocol_negotiation{extension_data = <<6, "spdy/2">>}),
    Version = ssl_test_lib:protocol_version(Config),
    {[{DecodedHandshakeMessage, _Raw}], _} =
	tls_handshake:get_tls_handshake(Version, list_to_binary(HandShakeData), <<>>,  #ssl_options{}),
    NextProtocolNegotiation = (DecodedHandshakeMessage#server_hello.extensions)#hello_extensions.next_protocol_negotiation,
    ct:log("~p ~n", [NextProtocolNegotiation]),
    NextProtocolNegotiation = #next_protocol_negotiation{extension_data = <<6, "spdy/2">>}.

%%--------------------------------------------------------------------
create_server_hello_with_no_advertised_protocols_test(_Config) ->
    Hello = ssl_handshake:server_hello(<<>>, {3, 0}, create_connection_states(), #hello_extensions{}),
    undefined = (Hello#server_hello.extensions)#hello_extensions.next_protocol_negotiation.
%%--------------------------------------------------------------------
create_server_hello_with_advertised_protocols_test(_Config) ->
    Hello = ssl_handshake:server_hello(<<>>, {3, 0}, create_connection_states(),
				       #hello_extensions{next_protocol_negotiation = [<<"spdy/1">>, <<"http/1.0">>, <<"http/1.1">>]}),
    [<<"spdy/1">>, <<"http/1.0">>, <<"http/1.1">>] =
	(Hello#server_hello.extensions)#hello_extensions.next_protocol_negotiation.
%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
create_client_handshake(Npn) ->
    Vsn = {1, 2},
    tls_handshake:encode_handshake(#client_hello{
				      client_version = Vsn,
				      random = <<1:256>>,
				      session_id = <<>>,
				      cipher_suites = [?TLS_DHE_DSS_WITH_DES_CBC_SHA],
				      compression_methods = "",
				      extensions = #hello_extensions{
						      next_protocol_negotiation = Npn,
						      renegotiation_info = #renegotiation_info{}}
				     }, Vsn).

create_server_handshake(Npn) ->
    Vsn = {1, 2},
    tls_handshake:encode_handshake(#server_hello{
				      server_version = Vsn,
				      random = <<1:256>>,
				      session_id = <<>>,
				      cipher_suite = ?TLS_DHE_DSS_WITH_DES_CBC_SHA,
				      compression_method = 1,
				      extensions = #hello_extensions{
						      next_protocol_negotiation = Npn,
						      renegotiation_info = #renegotiation_info{}}
				     }, Vsn).

create_connection_states() ->
    #connection_states{
       pending_read = #connection_state{
			 security_parameters = #security_parameters{
						  server_random = <<1:256>>,
						  compression_algorithm = 1,
						  cipher_suite = ?TLS_DHE_DSS_WITH_DES_CBC_SHA
						 }
			},
       current_read = #connection_state {
			 secure_renegotiation = false
			}
      }.
