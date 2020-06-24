%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
-module(pubkey_ssh_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         ssh_rsa_public_key/1,
         ssh_dsa_public_key/1,
         ssh_ecdsa_public_key/1,
         ssh_rfc4716_rsa_comment/1,
         ssh_rfc4716_dsa_comment/1,
         ssh_rfc4716_rsa_subject/1,
         ssh_known_hosts/1,
         ssh1_known_hosts/1,
         ssh_auth_keys/1,
         ssh1_auth_keys/1,
         ssh_openssh_public_key_with_comment/1,
         ssh_openssh_public_key_long_header/1,

         ssh_hostkey_fingerprint_md5_implicit/1,
         ssh_hostkey_fingerprint_md5/1,
         ssh_hostkey_fingerprint_sha/1,
         ssh_hostkey_fingerprint_sha256/1,
         ssh_hostkey_fingerprint_sha384/1,
         ssh_hostkey_fingerprint_sha512/1,
         ssh_hostkey_fingerprint_list/1
        ]).


-define(TIMEOUT, 120000). % 2 min


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> 
    [].

all() -> 
    [{group, ssh_public_key_decode_encode},
     {group, ssh_hostkey_fingerprint}
    ].

groups() -> 
    [{ssh_public_key_decode_encode, [],
      [ssh_rsa_public_key, ssh_dsa_public_key, ssh_ecdsa_public_key,
       ssh_rfc4716_rsa_comment, ssh_rfc4716_dsa_comment,
       ssh_rfc4716_rsa_subject,
       ssh_known_hosts,
       ssh_auth_keys, ssh1_known_hosts, ssh1_auth_keys, ssh_openssh_public_key_with_comment,
       ssh_openssh_public_key_long_header]},

     {ssh_hostkey_fingerprint, [],
      [ssh_hostkey_fingerprint_md5_implicit,
       ssh_hostkey_fingerprint_md5,
       ssh_hostkey_fingerprint_sha,
       ssh_hostkey_fingerprint_sha256,
       ssh_hostkey_fingerprint_sha384,
       ssh_hostkey_fingerprint_sha512,
       ssh_hostkey_fingerprint_list]}
    ].
%%-------------------------------------------------------------------
init_per_suite(Config) ->
    application:stop(crypto),
    try crypto:start() of
	ok ->
	    application:start(asn1),
	    Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(asn1),
    application:stop(crypto).

%%-------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.
%%-------------------------------------------------------------------
init_per_testcase(ssh_hostkey_fingerprint_md5_implicit, Config) ->
    init_fingerprint_testcase([md5], Config);

init_per_testcase(ssh_hostkey_fingerprint_md5, Config) ->
    init_fingerprint_testcase([md5], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha, Config) ->
    init_fingerprint_testcase([sha], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha256, Config) ->
    init_fingerprint_testcase([sha256], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha384, Config) ->
    init_fingerprint_testcase([sha384], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha512, Config) ->
    init_fingerprint_testcase([sha512], Config);

init_per_testcase(ssh_hostkey_fingerprint_list  , Config) ->
    init_fingerprint_testcase([sha,md5], Config);

init_per_testcase(_, Config) ->
    init_common_per_testcase(Config).

	
init_fingerprint_testcase(Algs, Config) ->
    Hashs = proplists:get_value(hashs, crypto:supports(), []),
    case Algs -- Hashs of
        [] -> init_common_per_testcase(Config);
        UnsupportedAlgs ->  {skip,{UnsupportedAlgs,not_supported}}
    end.

init_common_per_testcase(Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].


end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
ssh_rsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(RSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(RSARawSsh2, rfc4716_public_key),

    {ok, RSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_rsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(RSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(RSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == RSARawSsh2 and EncodedOpenSsh
    %% = RSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_dsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(DSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(DSARawSsh2, rfc4716_public_key),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(DSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == DSARawSsh2 and EncodedOpenSsh
    %% = DSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_ecdsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, ECDSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_ecdsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(ECDSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(ECDSARawSsh2, rfc4716_public_key),

    {ok, ECDSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_ecdsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(ECDSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(ECDSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == ECDSARawSsh2 and EncodedOpenSsh
    %% = ECDSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_comment_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,
    RSARawSsh2 = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_dsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_comment_pub")),
    [{{_, #'Dss-Parms'{}} = PubKey, Attributes}] =
        public_key:ssh_decode(DSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == DSARawSsh2 as line continuation breakpoints may differ
    Encoded  = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key),
    [{PubKey, Attributes}] =
        public_key:ssh_decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_subject(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_subject_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Subject", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == RSARawSsh2 as line continuation breakpoints may differ
    Encoded  = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key),
    [{PubKey, Attributes}] =
        public_key:ssh_decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {#'RSAPublicKey'{}, Attributes4}] = Decoded =
        public_key:ssh_decode(SshKnownHosts, known_hosts),

    Comment1 = undefined,
    Comment2 = "foo@bar.com",
    Comment3 = "Comment with whitespaces",
    Comment4 = "foo@bar.com Comment with whitespaces",
    	
    Comment1 = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),	

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh1_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "ssh1_known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},{#'RSAPublicKey'{}, Attributes3}] 
	= Decoded = public_key:ssh_decode(SshKnownHosts, known_hosts),

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment ="dhopson@VMUbuntu-DSH comment with whitespaces",
    Comment = proplists:get_value(comment, Attributes3),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1}, {{_, #'Dss-Parms'{}}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {{_, #'Dss-Parms'{}}, Attributes4}
    ] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(options, Attributes1, undefined),
    true = Value1 =/= undefined,

    Comment1 = Comment2 = "dhopson@VMUbuntu-DSH",
    Comment3 = Comment4 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    Comment1 = proplists:get_value(comment, Attributes1),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh1_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "ssh1_auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1},
     {#'RSAPublicKey'{}, Attributes2}, {#'RSAPublicKey'{}, Attributes3},
     {#'RSAPublicKey'{}, Attributes4}, {#'RSAPublicKey'{}, Attributes5}] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(bits, Attributes2, undefined),
    Value2 = proplists:get_value(bits, Attributes3, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment2 = Comment3 = "dhopson@VMUbuntu-DSH",
    Comment4 = Comment5 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    undefined = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),
    Comment5 = proplists:get_value(comment, Attributes5),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh_openssh_public_key_with_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_with_comment_pub")),
    [{{_, #'Dss-Parms'{}}, _}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key).

%%--------------------------------------------------------------------
ssh_openssh_public_key_long_header(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok,RSARawOpenSsh} = file:read_file(filename:join(Datadir, "ssh_rsa_long_header_pub")),
    [{#'RSAPublicKey'{}, _}] = Decoded = public_key:ssh_decode(RSARawOpenSsh, public_key),

    Encoded = public_key:ssh_encode(Decoded, rfc4716_public_key),
    Decoded = public_key:ssh_decode(Encoded, rfc4716_public_key).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5_implicit(_Config) ->
    Expected = "4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = public_key:ssh_hostkey_fingerprint(ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5(_Config) ->
    Expected = "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = public_key:ssh_hostkey_fingerprint(md5, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead. The Expected is generated with:
%%       $ openssh-7.3p1/ssh-keygen -E sha1 -lf <file>
%%       2048 SHA1:Soammnaqg06jrm2jivMSnzQGlmk none@example.org (RSA)
ssh_hostkey_fingerprint_sha(_Config) ->
    Expected = "SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
    Expected = public_key:ssh_hostkey_fingerprint(sha, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha256(_Config) ->
    Expected = "SHA256:T7F1BahkJWR7iJO8+rpzWOPbp7LZP4MlNrDExdNYOvY",
    Expected = public_key:ssh_hostkey_fingerprint(sha256, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha384(_Config) ->
    Expected = "SHA384:QhkLoGNI4KXdPvC//HxxSCP3uTQVADqxdajbgm+Gkx9zqz8N94HyP1JmH8C4/aEl",
    Expected = public_key:ssh_hostkey_fingerprint(sha384, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha512(_Config) ->
    Expected = "SHA512:ezUismvm3ADQQb6Nm0c1DwQ6ydInlJNfsnSQejFkXNmABg1Aenk9oi45CXeBOoTnlfTsGG8nFDm0smP10PBEeA",
    Expected = public_key:ssh_hostkey_fingerprint(sha512, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_list(_Config) ->
    Expected = ["SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
                "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a"],
    Expected = public_key:ssh_hostkey_fingerprint([sha,md5], ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
ssh_hostkey(rsa) ->
    [{PKdecoded,_}] =
	public_key:ssh_decode(
	  <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYXcYmsyJBstl4EfFYzfQJmSiUE162zvSGSoMYybShYOI6rnnyvvihfw8Aml+2gZ716F2tqG48FQ/yPZEGWNPMrCejPpJctaPWhpNdNMJ8KFXSEgr5bY2mEpa19DHmuDeXKzeJJ+X7s3fVdYc4FMk5731KIW6Huf019ZnTxbx0VKG6b1KAJBg3vpNsDxEMwQ4LFMB0JHVklOTzbxmpaeULuIxvl65A+eGeFVeo2Q+YI9UnwY1vSgmc9Azwy8Ie9Z0HpQBN5I7Uc5xnknT8V6xDhgNfXEfzsgsRdDfZLECt1WO/1gP9wkosvAGZWt5oG8pbNQWiQdFq536ck8WQD9WD none@example.org">>,
	  public_key),
    PKdecoded.

