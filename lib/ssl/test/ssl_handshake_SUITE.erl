%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include("ssl_alert.hrl").
-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("tls_handshake.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Common test
-export([all/0,
         init_per_suite/1,
         init_per_group/2,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_group/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([decode_hello_handshake/1,
         decode_single_hello_extension_correctly/1,
         decode_supported_elliptic_curves_hello_extension_correctly/1,
         decode_unknown_hello_extension_correctly/1,
         encode_single_hello_sni_extension_correctly/1,
         decode_single_hello_sni_extension_correctly/1,
         decode_empty_server_sni_correctly/1,
         select_proper_tls_1_2_rsa_default_hashsign/1,
         ignore_hassign_extension_pre_tls_1_2/1,
         signature_algorithms/1]).

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
	  signature_algorithms].

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
	
    Version = ?SSL_3_0,
    DefOpts = ssl:update_options([{verify, verify_none}], client, #{}),
    {Records, _Buffer} = tls_handshake:get_tls_handshakes(Version, HelloPacket, <<>>, DefOpts),

    {Hello, _Data} = hd(Records),
    Extensions = Hello#server_hello.extensions,
    #{renegotiation_info := #renegotiation_info{renegotiated_connection = <<0>>}} = Extensions.

decode_single_hello_extension_correctly(_Config) -> 
    Renegotiation = <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(1), 0>>,
    Extensions = ssl_handshake:decode_extensions(Renegotiation, ?TLS_1_2, undefined),
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
    Extensions = ssl_handshake:decode_hello_extensions(Extension, ?TLS_1_1, ?TLS_1_1, client),
    #{elliptic_curves := #elliptic_curves{elliptic_curve_list = [?sect233k1, ?sect193r2]}} = Extensions. 

decode_unknown_hello_extension_correctly(_Config) ->
    FourByteUnknown = <<16#CA,16#FE, ?UINT16(4), 3, 0, 1, 2>>,
    Renegotiation = <<?UINT16(?RENEGOTIATION_EXT), ?UINT16(1), 0>>,
    Extensions = ssl_handshake:decode_hello_extensions(<<FourByteUnknown/binary, Renegotiation/binary>>, ?TLS_1_1, ?TLS_1_1, client),
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
    Decoded = ssl_handshake:decode_hello_extensions(SNI, ?TLS_1_2, ?TLS_1_2, client),
    #{sni := #sni{hostname = "test.com"}} = Decoded.

decode_empty_server_sni_correctly(_Config) ->
    SNI = <<?UINT16(?SNI_EXT),?UINT16(0)>>,
    Decoded = ssl_handshake:decode_hello_extensions(SNI, ?TLS_1_2, ?TLS_1_2, server),
    #{sni := #sni{hostname = ""}} = Decoded.


select_proper_tls_1_2_rsa_default_hashsign(_Config) ->
    % RFC 5246 section 7.4.1.4.1 tells to use {sha1,rsa} as default signature_algorithm for RSA key exchanges
    {sha, rsa} = ssl_handshake:select_hashsign_algs(undefined, ?rsaEncryption, ?TLS_1_2),
    % Older versions use MD5/SHA1 combination
    {md5sha, rsa} = ssl_handshake:select_hashsign_algs(undefined, ?rsaEncryption, ?TLS_1_1),
    {md5sha, rsa} = ssl_handshake:select_hashsign_algs(undefined, ?rsaEncryption, ?SSL_3_0).


ignore_hassign_extension_pre_tls_1_2(Config) ->
    Opts = proplists:get_value(server_opts, Config),
    CertFile = proplists:get_value(certfile, Opts),
    [{_, Cert, _}] = ssl_test_lib:pem_to_der(CertFile),
    HashSigns = #hash_sign_algos{hash_sign_algos = [{sha512, rsa}, {sha, dsa}, {sha256, rsa}]},
    {sha512, rsa} = ssl_handshake:select_hashsign({HashSigns, undefined}, Cert, ecdhe_rsa, tls_v1:default_signature_algs([?TLS_1_2]), ?TLS_1_2),
    %%% Ignore
    {md5sha, rsa} = ssl_handshake:select_hashsign({HashSigns, undefined}, Cert, ecdhe_rsa, tls_v1:default_signature_algs([?TLS_1_1]), ?TLS_1_1),
    {md5sha, rsa} = ssl_handshake:select_hashsign({HashSigns, undefined}, Cert, ecdhe_rsa, tls_v1:default_signature_algs([?SSL_3_0]), ?SSL_3_0).

signature_algorithms(Config) ->
    Opts = proplists:get_value(server_opts, Config),
    CertFile = proplists:get_value(certfile, Opts),
    io:format("Cert = ~p~n", [CertFile]),
    [{_, Cert, _}] = ssl_test_lib:pem_to_der(CertFile),
    HashSigns0 = #hash_sign_algos{
                   hash_sign_algos = [{sha512, rsa},
                                      {sha, dsa},
                                      {sha256, rsa}]},
    Schemes0 = #signature_algorithms_cert{
                 signature_scheme_list = [rsa_pkcs1_sha256,
                                          ecdsa_sha1]},
    {sha512, rsa} = ssl_handshake:select_hashsign(
                      {HashSigns0, Schemes0},
                      Cert, ecdhe_rsa,
                      tls_v1:default_signature_algs([?TLS_1_2]),
                      ?TLS_1_2),
    HashSigns1 = #hash_sign_algos{
                    hash_sign_algos = [{sha, dsa},
                                       {sha256, rsa}]},
    {sha256, rsa} = ssl_handshake:select_hashsign(
                      {HashSigns1, Schemes0},
                      Cert, ecdhe_rsa,
                      tls_v1:default_signature_algs([?TLS_1_2]),
                      ?TLS_1_2),
    Schemes1 = #signature_algorithms_cert{
                  signature_scheme_list = [rsa_pkcs1_sha1,
                                           ecdsa_sha1]},
    %% Signature not supported
    #alert{} = ssl_handshake:select_hashsign(
                 {HashSigns1, Schemes1},
                 Cert, ecdhe_rsa,
                 tls_v1:default_signature_algs([?TLS_1_2]),
                 ?TLS_1_2),
    %% No scheme, hashsign is used
    {sha256, rsa} = ssl_handshake:select_hashsign(
                      {HashSigns1, undefined},
                      Cert, ecdhe_rsa,
                      tls_v1:default_signature_algs([?TLS_1_2]),
                      ?TLS_1_2),
    HashSigns2 = #hash_sign_algos{
                    hash_sign_algos = [{sha, dsa}]},
    %% Signature not supported
    #alert{} = ssl_handshake:select_hashsign(
                 {HashSigns2, Schemes1},
                 Cert, ecdhe_rsa,
                 tls_v1:default_signature_algs([?TLS_1_2]),
                 ?TLS_1_2).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

is_supported(Hash) ->
    Algos = crypto:supports(),
    Hashs = proplists:get_value(hashs, Algos), 
    lists:member(Hash, Hashs).
