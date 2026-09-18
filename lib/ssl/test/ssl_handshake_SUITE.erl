%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2026. All Rights Reserved.
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
-include_lib("ssl/src/ssl_alert.hrl").
-include_lib("ssl/src/ssl_handshake.hrl").
-include_lib("ssl/src/ssl_internal.hrl").
-include_lib("ssl/src/ssl_record.hrl").
-include_lib("ssl/src/tls_handshake.hrl").
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
         signature_algorithms/1,
         server_key_exchange_signature_not_sha1_connection/1,
         drop_md5_rsa_signature_algorithm_tls_1_2/1,
         drop_unassigned_signature_algorithms/1,
         drop_undecodable_certificate_authorities/1,
         reject_truncated_certificate_entry/1,
         decode_sni_after_unknown_name_type/1]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> [
          decode_hello_handshake,
          decode_single_hello_extension_correctly,
          decode_supported_elliptic_curves_hello_extension_correctly,
          decode_unknown_hello_extension_correctly,
          encode_single_hello_sni_extension_correctly,
          decode_single_hello_sni_extension_correctly,
          decode_empty_server_sni_correctly,
          select_proper_tls_1_2_rsa_default_hashsign,
          ignore_hassign_extension_pre_tls_1_2,
          signature_algorithms,
          drop_unassigned_signature_algorithms,
          drop_undecodable_certificate_authorities,
          reject_truncated_certificate_entry,
          decode_sni_after_unknown_name_type
         ].

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
    catch application:stop(crypto),
    try application:start(crypto) of
	ok ->
	    case is_supported(sha512) of
		true ->
		    ssl_test_lib:clean_start(),
		    %% make rsa certs using openssl
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
    application:stop(crypto);
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
    ClientEllipticCurves = [0, tls_v1:oid_to_enum(?sect233k1), 37,
                            tls_v1:oid_to_enum(?sect193r2), 16#badc],
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
    Extensions = ssl_handshake:decode_hello_extensions(<<FourByteUnknown/binary,
                                                         Renegotiation/binary>>, ?TLS_1_1, ?TLS_1_1, client),
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

decode_sni_after_unknown_name_type(_Config) ->
    %% RFC 6066 §3: a ServerNameList may contain entries of name types other
    %% than host_name (0); a decoder must skip an unknown entry by its
    %% (byte) length and continue. Regression test: dec_sni/1 used `_:Len`
    %% (Len *bits*) instead of `_:Len/binary` to skip an unknown entry, so a
    %% host_name entry following an unknown-type entry was mis-located and
    %% never decoded. Put an unknown-type entry (type 1, 3 bytes) before the
    %% host_name entry and require the hostname to still decode.
    Host = <<"test.com">>,
    HostLen = byte_size(Host),
    HostEntry = <<?BYTE(?SNI_NAMETYPE_HOST_NAME), ?UINT16(HostLen), Host/binary>>,
    UnknownEntry = <<?BYTE(1), ?UINT16(3), "abc">>,
    NameList = <<UnknownEntry/binary, HostEntry/binary>>,
    NameListLen = byte_size(NameList),
    ExtData = <<?UINT16(NameListLen), NameList/binary>>,
    ExtDataLen = byte_size(ExtData),
    SNI = <<?UINT16(?SNI_EXT), ?UINT16(ExtDataLen), ExtData/binary>>,
    Decoded = ssl_handshake:decode_hello_extensions(SNI, ?TLS_1_2, ?TLS_1_2, client),
    #{sni := #sni{hostname = "test.com"}} = Decoded.


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
    {sha512, rsa} =
        ssl_handshake:select_hashsign({HashSigns, undefined}, Cert,
                                      ecdhe_rsa, tls_v1:default_signature_algs([?TLS_1_2]), ?TLS_1_2),
    %%% Ignore
    {md5sha, rsa} =
        ssl_handshake:select_hashsign({HashSigns, undefined}, Cert,
                                      ecdhe_rsa, tls_v1:default_signature_algs([?TLS_1_1]), ?TLS_1_1),
    {md5sha, rsa} =
        ssl_handshake:select_hashsign({HashSigns, undefined}, Cert,
                                      ecdhe_rsa, tls_v1:default_signature_algs([?SSL_3_0]), ?SSL_3_0).

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
                      {HashSigns0, undefined},
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

server_key_exchange_signature_not_sha1_connection(_Config) ->
    %% RFC 9155 §4: MD5 and SHA-1 MUST NOT be used as signature hashes in
    %% (D)TLS 1.2 digital signatures, which includes the ServerKeyExchange
    %% signature. Regression test for the TLS-1.2 server signature-selection
    %% defect (OTP-20390): tls_handshake:handle_client_hello passed the
    %% signature_algs_cert pool (seeded by default with the "smooth upgrade
    %% path" {sha, rsa} entry) as the SupportedHashSigns argument to
    %% ssl_handshake:select_hashsign/5, which returns the first client-offered
    %% scheme present in that pool. A peer listing SHA-1 first therefore got a
    %% SHA-1-signed ServerKeyExchange in default configuration. The fix scopes
    %% the ServerKeyExchange signature to the signature_algs pool (no SHA-1 by
    %% default); signature_algs_cert stays for certificate-chain signatures
    %% only. The DTLS sibling already passed the correct pool.
    %%
    %% This is a black-box test: it drives a real TLS-1.2 DHE_RSA handshake
    %% via the public ssl API. The client offers rsa_pkcs1_sha1 FIRST in
    %% signature_algs_cert (offering it in signature_algs is rejected by the
    %% option layer with no_supported_algorithms); signature_algs_cert is the
    %% list select_hashsign/5 iterates for the ServerKeyExchange hash. The
    %% real signing call ssl_handshake:digitally_signed/5 is traced and the
    %% hash used for the ServerKeyExchange signature is asserted not to be
    %% SHA-1.
    %%
    %% NOTE: TLS-1.2 specific (TLS-1.3 has no ServerKeyExchange); the version
    %% is set explicitly and this case must never run in a 'tlsv1.3' group.
    case is_supported(sha256) andalso
        lists:member(rsa, crypto:supports(public_keys)) andalso
        lists:member(dh, crypto:supports(public_keys)) of
        false ->
            {skip, "Crypto does not support rsa/dh/sha256"};
        true ->
            do_skx_signature_not_sha1_connection()
    end.

do_skx_signature_not_sha1_connection() ->
    ssl_test_lib:clean_start(),
    %% In-memory RSA cert chains (no external openssl / no files needed).
    #{server_config := ServerConf,
      client_config := ClientConf} =
        public_key:pkix_test_data(
          #{server_chain =>
                #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                  intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                  peer => [{key, ssl_test_lib:hardcode_rsa_key(3)}]},
            client_chain =>
                #{root => [{key, ssl_test_lib:hardcode_rsa_key(4)}],
                  intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                  peer => [{key, ssl_test_lib:hardcode_rsa_key(6)}]}}),

    %% Force a DHE_RSA cipher so a signed ServerKeyExchange is actually sent.
    Ciphers = [#{key_exchange => dhe_rsa,
                 cipher => aes_256_gcm,
                 mac => aead,
                 prf => sha384}],

    ServerOpts = [{verify, verify_none}, {versions, ['tlsv1.2']},
                  {ciphers, Ciphers}, {reuseaddr, true} | ServerConf],
    %% Client offers SHA-1 FIRST in signature_algs_cert (the pool the buggy
    %% server iterated for the ServerKeyExchange hash).
    ClientOpts = [{verify, verify_none}, {versions, ['tlsv1.2']},
                  {ciphers, Ciphers},
                  {signature_algs_cert, [rsa_pkcs1_sha1, rsa_pkcs1_sha256]}
                  | ClientConf],

    %% Trace the actual signing call and collect the hash algorithm argument.
    Self = self(),
    Tracer = spawn_link(fun() -> skx_hash_collector(Self, []) end),
    {ok, _} = dbg:tracer(process, {fun(Msg, _) ->
                                           Tracer ! {trace_msg, Msg}, []
                                   end, []}),
    {ok, _} = dbg:p(all, [c]),
    {ok, _} = dbg:tpl(ssl_handshake, digitally_signed, 5, []),

    try
        {ok, LSock} = ssl:listen(0, ServerOpts),
        {ok, {_, Port}} = ssl:sockname(LSock),
        ServerPid =
            spawn(fun() ->
                          {ok, ASock} = ssl:transport_accept(LSock),
                          case ssl:handshake(ASock) of
                              {ok, SSock} -> ssl:close(SSock);
                              _ -> ok
                          end
                  end),
        {ok, CSock} = ssl:connect("localhost", Port, ClientOpts, 5000),
        ssl:close(CSock),
        catch exit(ServerPid, kill),
        catch ssl:close(LSock),

        %% Give the tracer a moment to deliver, then collect.
        ct:sleep(200),
        Tracer ! {get, self()},
        Hashes = receive {hashes, Hs} -> Hs after 2000 -> timeout end,
        ct:log("ServerKeyExchange signing hash algorithms observed: ~p", [Hashes]),
        %% A signed ServerKeyExchange must have been produced ...
        true = Hashes =/= timeout andalso Hashes =/= [],
        %% ... and it must NOT use SHA-1, despite the client listing it first.
        false = lists:member(sha, Hashes),
        true = lists:member(sha256, Hashes) orelse
               lists:member(sha384, Hashes) orelse
               lists:member(sha512, Hashes),
        ok
    after
        dbg:stop()
    end.

skx_hash_collector(Owner, Acc) ->
    receive
        {trace_msg, {trace, _Pid, call,
                     {ssl_handshake, digitally_signed,
                      [_Version, _Msg, HashAlgo, _Key, _SignAlgo]}}} ->
            skx_hash_collector(Owner, [HashAlgo | Acc]);
        {trace_msg, _Other} ->
            skx_hash_collector(Owner, Acc);
        {get, From} ->
            From ! {hashes, lists:reverse(Acc)},
            skx_hash_collector(Owner, Acc)
    end.

drop_md5_rsa_signature_algorithm_tls_1_2(_Config) ->
    %% MD5 is not a supported standalone signature hash for (D)TLS 1.2
    %% signature_algorithms; it only ever appeared as the combined md5sha
    %% construct in pre-TLS-1.2 RSA signatures. A client that (deliberately)
    %% configures {md5, rsa} in signature_algs for TLS 1.2 must NOT end up
    %% offering an MD5 signature scheme: option processing filters it out
    %% (tls_v1:is_pair/3), consistent with the ecdsa case which already
    %% excluded MD5. SHA-1 ({sha, rsa}) remains a valid deprecated opt-in for
    %% TLS 1.2 and must be preserved.
    %%
    %% Assert on the effective signature_algs produced by the
    %% public option-processing entry point ssl_config:handle_options/3.
    Host = net_adm:localhost(),
    Opts = [{verify, verify_none},
            {versions, ['tlsv1.2']},
            {signature_algs, [{md5, rsa}, {sha, rsa}, {sha256, rsa}]}],
    {ok, #config{ssl = #{signature_algs := SigAlgs}}} =
        ssl_config:handle_options(Opts, client, Host),
    ct:log("effective signature_algs = ~p", [SigAlgs]),

    %% {md5, rsa} must be dropped ...
    false = lists:member({md5, rsa}, SigAlgs),
    %% ... while the deprecated-but-supported {sha, rsa} opt-in is kept ...
    true = lists:member({sha, rsa}, SigAlgs),
    %% ... and the compliant {sha256, rsa} is of course present.
    true = lists:member({sha256, rsa}, SigAlgs),
    ok.


drop_unassigned_signature_algorithms(_Config) ->
    %% Be sure the algo is unsupported
    unassigned = ssl_cipher:hash_algorithm(223),
    %% TLS client_hello handshake with unsupported signature algorithm
    HelloBin0 = <<1,0,1,213,3,3,224,80,22,53,173,24,195,236,126,90,97,19,120,89,229,186,70,120,73,252,215,184,142,50,134,16,84,4,
                  60,7,89,231,32,129,11,71,132,248,183,203,23,252,145,42,154,69,82,123,172,213,137,7,235,105,178,140,163,11,186,
                  106,97,230,22,179,162,0,24,19,2,19,3,19,1,192,44,192,43,192,48,192,47,192,36,192,35,192,40,192,39,0,255,1,0,1,
                  116,0,0,0,26,0,24,0,0,21,119,119,119,46,120,120,120,120,120,120,120,120,120,120,120,120,120,46,99,111,109,0,11,
                  0,4,3,0,1,2,0,10,0,22,0,20,0,29,0,23,0,30,0,25,0,24,1,0,1,1,1,2,1,3,1,4,0,35,0,0,0,5,0,5,1,0,0,0,0,0,22,0,0,0,
                  23,0,0,0,13,0,48,0,46,

                  %% Supported signature algorithms:
                  %% 4,3,5,3,6,3,8,7,8,8,8,26,8,27,8,28,8,9,8,10,8,11,8,4,8,5,8,6,4,1,5,1,6,1,3,3,3,1,3,2,4,2,5,2,6,2,

                  %% Set unsupported signature algorithms (223,223):
                  4,3,5,3,6,3,8,7,8,8,8,26,8,27,8,28,8,9,8,10,8,11,8,4,8,5,8,6,4,1,5,1,6,1,3,3,3,1,3,2,4,2,5,2,223,223,

                  0,43,0,5,4,3,4,3,3,0,45,0,2,1,1,0,51,0,38,0,36,0,29,0,32,47,17,161,47,68,184,145,148,24,172,153,151,195,
                  110,139,12,220,63,236,88,142,36,222,42,38,251,239,157,84,148,59,72,0,41,0,174,0,121,0,115,155,62,93,115,44,106,
                  248,45,157,98,128,178,116,82,6,153,40,143,250,26,61,154,21,37,97,52,44,76,181,32,9,130,18,163,173,131,135,62,34,
                  125,9,104,15,168,70,134,222,96,240,76,224,24,171,110,210,0,100,181,11,26,114,24,20,67,59,24,77,88,26,204,134,155,
                  215,203,165,155,208,45,62,191,254,6,93,167,80,22,127,195,83,180,179,88,215,195,34,30,75,189,239,50,178,76,124,235,
                  131,68,99,57,184,107,52,232,202,165,172,75,222,53,218,0,49,48,6,136,165,215,98,30,34,60,138,162,178,39,219,246,245,
                  246,13,234,49,176,137,24,44,148,232,172,43,211,254,1,240,203,195,248,114,78,172,157,19,100,239,81,106,115,231,255,
                  168,20>>,
    <<?BYTE(Type), ?UINT24(_Length), Body/binary>> = HelloBin0,
    #client_hello{
        extensions = #{ signature_algs := #signature_algorithms{signature_scheme_list = SigAlgs} }
    } = tls_handshake:decode_handshake(?TLS_1_3, Type, Body),
    false = lists:any(
        fun
            (unassigned) -> true;
            ({unassigned, _}) -> true;
            ({_, unassigned}) -> true;
            (_) -> false
        end,
        SigAlgs).

drop_undecodable_certificate_authorities(_Config) ->
    GoodAuth0 = {rdnSequence,
                 [[{'AttributeTypeAndValue', ?'id-at-commonName',
                    {utf8String, <<"Good CA">>}}]]},
    GoodDN = public_key:pkix_encode('Name', GoodAuth0, otp),
    BadDN = base64:decode(<<"MHAxCzAJBgNVBAYMAkJSMRMwEQYDVQQKDApJQ1AtQnJhc2lsMTQwMgY",
                            "DVQQLDCtBdXRvcmlkYWRlIENlcnRpZmljYWRvcmEgUmFpeiBCcmFz",
                            "aWxlaXJhIHY1MRYwFAYDVQQDDA1BQyBTeW5ndWxhcklE">>),
    CertAuths = cert_auths_vector([BadDN, GoodDN]),
    HashSigns = <<(ssl_cipher:signature_scheme({sha256, rsa})):16>>,
    HashSignsLen = byte_size(HashSigns),
    CertAuthsLen = byte_size(CertAuths),
    CertReq = <<?BYTE(1), ?BYTE(?RSA_SIGN),
                ?UINT16(HashSignsLen), HashSigns/binary,
                ?UINT16(CertAuthsLen), CertAuths/binary>>,
    GoodAuth = public_key:pkix_normalize_name(GoodAuth0),

    #certificate_request{certificate_authorities = [GoodAuth]} =
        ssl_handshake:decode_handshake(?TLS_1_2, ?CERTIFICATE_REQUEST, CertReq).

reject_truncated_certificate_entry(_Config) ->
    %% RFC 8446 §4.4.2: a truncated/malformed CertificateEntry in a TLS-1.3
    %% Certificate message must abort with decode_error, not be silently
    %% dropped. Build a Certificate whose single entry declares a cert length
    %% larger than the bytes actually present.
    CertData = <<1,2,3,4>>,                    %% 4 bytes of "certificate"
    ClaimedLen = byte_size(CertData) + 10,     %% overrun: claim 10 more bytes
    Entries = <<?UINT24(ClaimedLen), CertData/binary, ?UINT16(0)>>,
    EntriesLen = byte_size(Entries),
    %% certificate_request_context = <<>> (BYTE 0), then the entry list.
    Body = <<?BYTE(0), ?UINT24(EntriesLen), Entries/binary>>,
    try tls_handshake:decode_handshake(?TLS_1_3, ?CERTIFICATE, Body) of
        Decoded ->
            ct:fail("Expected decode_error for truncated certificate entry, "
                    "decoded ~p", [Decoded])
    catch
        throw:#alert{description = ?DECODE_ERROR} ->
            ok;
        throw:#alert{description = Desc} ->
            ct:fail("Expected decode_error, got alert ~p", [Desc])
    end.


%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------

is_supported(Hash) ->
    Hashs = crypto:supports(hashs),
    lists:member(Hash, Hashs).

cert_auths_vector(Auths) ->
    list_to_binary([begin
                        Len = byte_size(Auth),
                        <<?UINT16(Len), Auth/binary>>
                    end || Auth <- Auths]).
