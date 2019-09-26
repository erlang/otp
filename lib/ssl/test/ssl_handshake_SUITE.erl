%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(ssl_handshake_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("ssl_alert.hrl").
-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("tls_handshake.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> [decode_hello_handshake,
	  decode_single_hello_extension_correctly,
	  decode_supported_elliptic_curves_hello_extension_correctly,
	  decode_unknown_hello_extension_correctly,
	  encode_single_hello_sni_extension_correctly,
	  decode_single_hello_sni_extension_correctly,
	  decode_empty_server_sni_correctly,
	  select_proper_tls_1_2_rsa_default_hashsign,
	  ignore_hassign_extension_pre_tls_1_2,
	  unorded_chain, signature_algorithms,
	  encode_decode_srp].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.
end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_,Config) ->
    Config.

init_per_testcase(TC, Config0) when
      TC =:= ignore_hassign_extension_pre_tls_1_2 orelse
      TC =:= signature_algorithms ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    case is_supported(sha512) of
		true ->
		    ssl_test_lib:clean_start(),
		    %% make rsa certs using oppenssl
		    {ok, _} = make_certs:all(proplists:get_value(data_dir, Config0),
					     proplists:get_value(priv_dir, Config0)),
		    Config = ssl_test_lib:cert_options(Config0),
		    ct:timetrap({seconds, 5}),
		    Config;
		false ->
		    {skip, "Crypto did not support sha512"}  
	    end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end;
init_per_testcase(_, Config0) ->
    Config0.

end_per_testcase(ignore_hassign_extension_pre_tls_1_2, _) ->
    crypto:stop();
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
decode_hello_handshake(_Config) ->
    HelloPacket = <<16#02, 16#00, 16#00,
		    16#44, 16#03, 16#03, 16#4e, 16#7f, 16#c1, 16#03, 16#35,
		    16#c2, 16#07, 16#b9, 16#4a, 16#58, 16#af, 16#34, 16#07,
		    16#a6, 16#7e, 16#ef, 16#52, 16#cb, 16#e0, 16#ea, 16#b7,
		    16#aa, 16#47, 16#c8, 16#c2, 16#2c, 16#66, 16#fa, 16#f8,
		    16#09, 16#42, 16#cf, 16#00, 16#c0, 16#30, 16#00, 16#00,
		    16#1c,
		    16#00, 16#0b, 16#00, 16#04, 16#03, 16#00, 16#01, 16#02, % ec_point_formats
		    16#ff, 16#01, 16#00, 16#01, 16#00, %% renegotiate
		    16#00, 16#23,
		    16#00, 16#00, 16#33, 16#74, 16#00, 16#07, 16#06, 16#73,
		    16#70, 16#64, 16#79, 16#2f, 16#32>>,
	
    Version = {3, 0},
    {Records, _Buffer} = tls_handshake:get_tls_handshake(Version, HelloPacket, <<>>, 
                                                         default_options_map()),

    {Hello, _Data} = hd(Records),
    Extensions = Hello#server_hello.extensions,
    #{renegotiation_info := #renegotiation_info{renegotiated_connection = <<0>>}} = Extensions.

decode_single_hello_extension_correctly(_Config) -> 
    Renegotiation = <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(1), 0>>,
    Extensions = ssl_handshake:decode_extensions(Renegotiation, {3,3}, undefined),
    #{renegotiation_info := #renegotiation_info{renegotiated_connection = <<0>>}} = Extensions.

decode_supported_elliptic_curves_hello_extension_correctly(_Config) ->
    % List of supported and unsupported curves (RFC4492:S5.1.1)
    ClientEllipticCurves = [0, tls_v1:oid_to_enum(?sect233k1), 37, tls_v1:oid_to_enum(?sect193r2), 16#badc],
    % Construct extension binary - modified version of ssl_handshake:encode_hello_extensions([#elliptic_curves{}], _)
    EllipticCurveList = << <<X:16>> || X <- ClientEllipticCurves>>,
    ListLen = byte_size(EllipticCurveList),
    Len = ListLen + 2,
    Extension = <<?UINT16(?ELLIPTIC_CURVES_EXT), ?UINT16(Len), ?UINT16(ListLen), EllipticCurveList/binary>>,
    % after decoding we should see only valid curves
    Extensions = ssl_handshake:decode_hello_extensions(Extension, {3,2}, {3,2}, client),
    #{elliptic_curves := #elliptic_curves{elliptic_curve_list = [?sect233k1, ?sect193r2]}} = Extensions. 

decode_unknown_hello_extension_correctly(_Config) ->
    FourByteUnknown = <<16#CA,16#FE, ?UINT16(4), 3, 0, 1, 2>>,
    Renegotiation = <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(1), 0>>,
    Extensions = ssl_handshake:decode_hello_extensions(<<FourByteUnknown/binary, Renegotiation/binary>>, {3,2}, {3,2}, client),
    #{renegotiation_info := #renegotiation_info{renegotiated_connection = <<0>>}} = Extensions.


encode_single_hello_sni_extension_correctly(_Config) ->
    SNI = <<16#00, 16#00, 16#00, 16#0d, 16#00, 16#0b, 16#00, 16#00, 16#08,
	    $t,    $e,    $s,    $t,    $.,    $c,    $o,    $m>>,
    ExtSize = byte_size(SNI),
    HelloExt = <<ExtSize:16/unsigned-big-integer, SNI/binary>>,
    Encoded = ssl_handshake:encode_extensions([#sni{hostname = "test.com"}]),
    HelloExt = Encoded.

decode_single_hello_sni_extension_correctly(_Config) ->
    SNI = <<16#00, 16#00, 16#00, 16#0d, 16#00, 16#0b, 16#00, 16#00, 16#08,
	    $t,    $e,    $s,    $t,    $.,    $c,    $o,    $m>>,
    Decoded = ssl_handshake:decode_hello_extensions(SNI, {3,3}, {3,3}, client),
    #{sni := #sni{hostname = "test.com"}} = Decoded.

decode_empty_server_sni_correctly(_Config) ->
    SNI = <<?UINT16(?SNI_EXT),?UINT16(0)>>,
    Decoded = ssl_handshake:decode_hello_extensions(SNI, {3,3}, {3,3}, server),
    #{sni := #sni{hostname = ""}} = Decoded.


select_proper_tls_1_2_rsa_default_hashsign(_Config) ->
    % RFC 5246 section 7.4.1.4.1 tells to use {sha1,rsa} as default signature_algorithm for RSA key exchanges
    {sha, rsa} = ssl_handshake:select_hashsign_algs(undefined, ?rsaEncryption, {3,3}),
    % Older versions use MD5/SHA1 combination
    {md5sha, rsa} = ssl_handshake:select_hashsign_algs(undefined, ?rsaEncryption, {3,2}),
    {md5sha, rsa} = ssl_handshake:select_hashsign_algs(undefined, ?rsaEncryption, {3,0}).


ignore_hassign_extension_pre_tls_1_2(Config) ->
    Opts = proplists:get_value(server_opts, Config),
    CertFile = proplists:get_value(certfile, Opts),
    [{_, Cert, _}] = ssl_test_lib:pem_to_der(CertFile),
    HashSigns = #hash_sign_algos{hash_sign_algos = [{sha512, rsa}, {sha, dsa}, {sha, rsa}]},
    {sha512, rsa} = ssl_handshake:select_hashsign({HashSigns, undefined}, Cert, ecdhe_rsa, tls_v1:default_signature_algs({3,3}), {3,3}),
    %%% Ignore
    {md5sha, rsa} = ssl_handshake:select_hashsign({HashSigns, undefined}, Cert, ecdhe_rsa, tls_v1:default_signature_algs({3,2}), {3,2}),
    {md5sha, rsa} = ssl_handshake:select_hashsign({HashSigns, undefined}, Cert, ecdhe_rsa, tls_v1:default_signature_algs({3,0}), {3,0}).

unorded_chain(Config) when is_list(Config) ->
    DefConf = ssl_test_lib:default_cert_chain_conf(),
    CertChainConf = ssl_test_lib:gen_conf(rsa, rsa, DefConf, DefConf),
    #{server_config := ServerConf,
      client_config := _ClientConf} = public_key:pkix_test_data(CertChainConf),
    PeerCert = proplists:get_value(cert, ServerConf),
    CaCerts = [_, C1, C2] = proplists:get_value(cacerts, ServerConf),
    {ok,  ExtractedCerts} = ssl_pkix_db:extract_trusted_certs({der, CaCerts}),
    UnordedChain = case public_key:pkix_is_self_signed(C1) of
                       true ->
                           [C1, C2];
                       false ->
                           [C2, C1]
                   end,
    OrderedChain = [PeerCert | lists:reverse(UnordedChain)],
    {ok, _, OrderedChain} = 
        ssl_certificate:certificate_chain(PeerCert, ets:new(foo, []), ExtractedCerts, UnordedChain).

encode_decode_srp(_Config) ->
    Exts = #{srp => #srp{username = <<"foo">>},
             sni => #sni{hostname = "bar"},
             renegotiation_info => undefined,
             signature_algs => undefined,
             alpn => undefined,
             next_protocol_negotiation => undefined,
             ec_point_formats => undefined,
             elliptic_curves => undefined
            },
    EncodedExts0 = <<0,20,          % Length
                    0,12,          % SRP extension
                    0,4,           % Length
                    3,             % srp_I length
                    102,111,111, % username = "foo"
                    0,0,           % SNI extension
                    0,8,           % Length
                    0,6,           % ServerNameLength
                    0,             % NameType (host_name)
                    0,3,           % HostNameLength
                    98,97,114>>,     % hostname = "bar"
    EncodedExts0 = <<?UINT16(_),EncodedExts/binary>> =
        ssl_handshake:encode_hello_extensions(Exts, {3,3}),
    Exts = ssl_handshake:decode_hello_extensions(EncodedExts, {3,3}, {3,3}, client).

signature_algorithms(Config) ->
    Opts = proplists:get_value(server_opts, Config),
    CertFile = proplists:get_value(certfile, Opts),
    io:format("Cert = ~p~n", [CertFile]),
    [{_, Cert, _}] = ssl_test_lib:pem_to_der(CertFile),
    HashSigns0 = #hash_sign_algos{
                   hash_sign_algos = [{sha512, rsa},
                                      {sha, dsa},
                                      {sha, rsa}]},
    Schemes0 = #signature_algorithms_cert{
                 signature_scheme_list = [rsa_pkcs1_sha1,
                                          ecdsa_sha1]},
    {sha512, rsa} = ssl_handshake:select_hashsign(
                      {HashSigns0, Schemes0},
                      Cert, ecdhe_rsa,
                      tls_v1:default_signature_algs({3,3}),
                      {3,3}),
    HashSigns1 = #hash_sign_algos{
                    hash_sign_algos = [{sha, dsa},
                                      {sha, rsa}]},
    {sha, rsa} = ssl_handshake:select_hashsign(
                      {HashSigns1, Schemes0},
                      Cert, ecdhe_rsa,
                      tls_v1:default_signature_algs({3,3}),
                   {3,3}),
    Schemes1 = #signature_algorithms_cert{
                  signature_scheme_list = [rsa_pkcs1_sha256,
                                           ecdsa_sha1]},
    %% Signature not supported
    #alert{} = ssl_handshake:select_hashsign(
                 {HashSigns1, Schemes1},
                 Cert, ecdhe_rsa,
                 tls_v1:default_signature_algs({3,3}),
                 {3,3}),
    %% No scheme, hashsign is used
    {sha, rsa} = ssl_handshake:select_hashsign(
                   {HashSigns1, undefined},
                   Cert, ecdhe_rsa,
                   tls_v1:default_signature_algs({3,3}),
                   {3,3}),
    HashSigns2 = #hash_sign_algos{
                    hash_sign_algos = [{sha, dsa}]},
    %% Signature not supported
    #alert{} = ssl_handshake:select_hashsign(
                 {HashSigns2, Schemes1},
                 Cert, ecdhe_rsa,
                 tls_v1:default_signature_algs({3,3}),
                 {3,3}).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

is_supported(Hash) ->
    Algos = crypto:supports(),
    Hashs = proplists:get_value(hashs, Algos), 
    lists:member(Hash, Hashs).

default_options_map() ->
    Fun = fun (_Key, {Default, _}) -> Default end,
    maps:map(Fun, ?RULES).
