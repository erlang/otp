%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
-module(public_key_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("test_server/include/test_server_line.hrl").

-include_lib("public_key/include/public_key.hrl").

-define(TIMEOUT, 120000). % 2 min

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initialization before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    try crypto:start() of
	ok ->
	    Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(crypto).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initialization before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initialization before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = test_server:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Dog = ?config(watchdog, Config),
    case Dog of 
	undefined ->
	    ok;
	_ ->
	    test_server:timetrap_cancel(Dog)
    end.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app,
     {group, pem_decode_encode},
     {group, ssh_public_key_decode_encode},
     encrypt_decrypt,
     {group, sign_verify},
     pkix, pkix_path_validation, deprecated].

groups() -> 
    [{pem_decode_encode, [], [dsa_pem, rsa_pem, encrypted_pem,
			      dh_pem, cert_pem]},
     {ssh_public_key_decode_encode, [],
      [ssh_rsa_public_key, ssh_dsa_public_key, ssh_rfc4716_rsa_comment,
       ssh_rfc4716_dsa_comment, ssh_rfc4716_rsa_subject, ssh_known_hosts,
       ssh_auth_keys, ssh1_known_hosts, ssh1_auth_keys, ssh_openssh_public_key_with_comment,
       ssh_openssh_public_key_long_header]},
     {sign_verify, [], [rsa_sign_verify, dsa_sign_verify]}
    ].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test cases starts here.
%%--------------------------------------------------------------------

app(doc) ->
    "Test that the public_key app file is ok";
app(suite) ->
    [];
app(Config) when is_list(Config) ->
    ok = test_server:app_test(public_key).

%%--------------------------------------------------------------------

dsa_pem(doc) ->
    [""];
dsa_pem(suite) ->
    [];
dsa_pem(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

     [{'DSAPrivateKey', DerDSAKey, not_encrypted} = Entry0 ] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "dsa.pem")),

    DSAKey = public_key:der_decode('DSAPrivateKey', DerDSAKey),

    DSAKey = public_key:pem_entry_decode(Entry0),

    {ok, DSAPubPem} = file:read_file(filename:join(Datadir, "dsa_pub.pem")),
    [{'SubjectPublicKeyInfo', _, _} = PubEntry0] =
        public_key:pem_decode(DSAPubPem),
    DSAPubKey = public_key:pem_entry_decode(PubEntry0),
    true = check_entry_type(DSAPubKey, 'DSAPublicKey'),
    PubEntry0 = public_key:pem_entry_encode('SubjectPublicKeyInfo', DSAPubKey),
    DSAPubPemNoEndNewLines = strip_ending_newlines(DSAPubPem),
    DSAPubPemNoEndNewLines = strip_ending_newlines(public_key:pem_encode([PubEntry0])).

%%--------------------------------------------------------------------

rsa_pem(doc) ->
    [""];
rsa_pem(suite) ->
    [];
rsa_pem(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    [{'RSAPrivateKey', DerRSAKey, not_encrypted} =  Entry0 ] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_key.pem")),

    RSAKey0 = public_key:der_decode('RSAPrivateKey', DerRSAKey),

    RSAKey0 = public_key:pem_entry_decode(Entry0),
    
    [{'RSAPrivateKey', _, {_,_}} = Entry1] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "rsa.pem")),

    true = check_entry_type(public_key:pem_entry_decode(Entry1, "abcd1234"),
			    'RSAPrivateKey'),

    {ok, RSAPubPem} = file:read_file(filename:join(Datadir, "rsa_pub.pem")),
    [{'SubjectPublicKeyInfo', _, _} = PubEntry0] =
        public_key:pem_decode(RSAPubPem),
    RSAPubKey = public_key:pem_entry_decode(PubEntry0),
    true = check_entry_type(RSAPubKey, 'RSAPublicKey'),
    PubEntry0 = public_key:pem_entry_encode('SubjectPublicKeyInfo', RSAPubKey),
    RSAPubPemNoEndNewLines = strip_ending_newlines(RSAPubPem),
    RSAPubPemNoEndNewLines = strip_ending_newlines(public_key:pem_encode([PubEntry0])),

    {ok, RSARawPem} = file:read_file(filename:join(Datadir, "rsa_pub_key.pem")),
    [{'RSAPublicKey', _, _} = PubEntry1] =
        public_key:pem_decode(RSARawPem),
    RSAPubKey = public_key:pem_entry_decode(PubEntry1),
    RSARawPemNoEndNewLines = strip_ending_newlines(RSARawPem),
    RSARawPemNoEndNewLines = strip_ending_newlines(public_key:pem_encode([PubEntry1])).

%%--------------------------------------------------------------------

encrypted_pem(doc) ->
    [""];
encrypted_pem(suite) ->
    [];
encrypted_pem(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    [{'RSAPrivateKey', DerRSAKey, not_encrypted}] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_key.pem")),

    RSAKey = public_key:der_decode('RSAPrivateKey', DerRSAKey),

    Salt0 = crypto:rand_bytes(8),
    Entry0 = public_key:pem_entry_encode('RSAPrivateKey', RSAKey,
					 {{"DES-EDE3-CBC", Salt0}, "1234abcd"}),
    RSAKey = public_key:pem_entry_decode(Entry0,"1234abcd"),
    Des3KeyFile = filename:join(Datadir, "des3_client_key.pem"),
    erl_make_certs:der_to_pem(Des3KeyFile, [Entry0]),
    [{'RSAPrivateKey', _, {"DES-EDE3-CBC", Salt0}}] =
	erl_make_certs:pem_to_der(Des3KeyFile),

    Salt1 = crypto:rand_bytes(8),
    Entry1 = public_key:pem_entry_encode('RSAPrivateKey', RSAKey,
					   {{"DES-CBC", Salt1}, "4567efgh"}),
    DesKeyFile = filename:join(Datadir, "des_client_key.pem"),
    erl_make_certs:der_to_pem(DesKeyFile, [Entry1]),
    [{'RSAPrivateKey', _, {"DES-CBC", Salt1}} =Entry2] =
	erl_make_certs:pem_to_der(DesKeyFile),
    true = check_entry_type(public_key:pem_entry_decode(Entry2, "4567efgh"),
			     'RSAPrivateKey').
    
%%--------------------------------------------------------------------

dh_pem(doc) ->
    [""];
dh_pem(suite) ->
    [];
dh_pem(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    [{'DHParameter', DerDH, not_encrypted} = Entry] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "dh.pem")),

    erl_make_certs:der_to_pem(filename:join(Datadir, "new_dh.pem"), [Entry]),

    DHParameter = public_key:der_decode('DHParameter', DerDH),
    DHParameter = public_key:pem_entry_decode(Entry),

    Entry = public_key:pem_entry_encode('DHParameter', DHParameter).
   
%%--------------------------------------------------------------------
cert_pem(doc) ->
    [""];
cert_pem(suite) ->
    [];
cert_pem(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    [Entry0] =
	erl_make_certs:pem_to_der(filename:join(Datadir, "dsa.pem")),

    [{'Certificate', DerCert, not_encrypted} = Entry7] =  
	erl_make_certs:pem_to_der(filename:join(Datadir, "client_cert.pem")),
    
    Cert = public_key:der_decode('Certificate', DerCert),
    Cert = public_key:pem_entry_decode(Entry7),
    
    CertEntries = [{'Certificate', _, not_encrypted} = CertEntry0, 
		   {'Certificate', _, not_encrypted} = CertEntry1] = 
        erl_make_certs:pem_to_der(filename:join(Datadir, "cacerts.pem")),

    ok = erl_make_certs:der_to_pem(filename:join(Datadir, "wcacerts.pem"), CertEntries), 
    ok = erl_make_certs:der_to_pem(filename:join(Datadir, "wdsa.pem"), [Entry0]), 
    
     NewCertEntries = erl_make_certs:pem_to_der(filename:join(Datadir, "wcacerts.pem")),
     true = lists:member(CertEntry0, NewCertEntries),
     true = lists:member(CertEntry1, NewCertEntries),
    [Entry0] = erl_make_certs:pem_to_der(filename:join(Datadir, "wdsa.pem")).

%%--------------------------------------------------------------------
ssh_rsa_public_key(doc) ->
    "";
ssh_rsa_public_key(suite) ->
    [];
ssh_rsa_public_key(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

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

ssh_dsa_public_key(doc) ->
    "";
ssh_dsa_public_key(suite) ->
    [];
ssh_dsa_public_key(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

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
ssh_rfc4716_rsa_comment(doc) ->
       "Test comment header and rsa key";
ssh_rfc4716_rsa_comment(suite) ->
    [];
ssh_rfc4716_rsa_comment(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_comment_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,
    RSARawSsh2 = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_dsa_comment(doc) ->
       "Test comment header and dsa key";
ssh_rfc4716_dsa_comment(suite) ->
    [];
ssh_rfc4716_dsa_comment(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

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
ssh_rfc4716_rsa_subject(doc) ->
       "Test another header value than comment";
ssh_rfc4716_rsa_subject(suite) ->
    [];
ssh_rfc4716_rsa_subject(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

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
ssh_known_hosts(doc) ->
    "";
ssh_known_hosts(suite) ->
    [];
ssh_known_hosts(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2}] = Decoded =
        public_key:ssh_decode(SshKnownHosts, known_hosts),

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------

ssh1_known_hosts(doc) ->
    "";
ssh1_known_hosts(suite) ->
    [];
ssh1_known_hosts(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "ssh1_known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2}] = Decoded =
        public_key:ssh_decode(SshKnownHosts, known_hosts),

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh_auth_keys(doc) ->
    "";
ssh_auth_keys(suite) ->
    [];
ssh_auth_keys(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1}, {{_, #'Dss-Parms'{}}, _Attributes2}] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(options, Attributes1, undefined),
    true = Value1 =/= undefined,

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh1_auth_keys(doc) ->
    "";
ssh1_auth_keys(suite) ->
    [];
ssh1_auth_keys(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "ssh1_auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2}] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(bits, Attributes1, undefined),
    Value2 = proplists:get_value(bits, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh_openssh_public_key_with_comment(doc) ->
    "Test that emty lines and lines starting with # are ignored";
ssh_openssh_public_key_with_comment(suite) ->
    [];
ssh_openssh_public_key_with_comment(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_with_comment_pub")),
    [{{_, #'Dss-Parms'{}}, _}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key).

%%--------------------------------------------------------------------
ssh_openssh_public_key_long_header(doc) ->
    "Test that long headers are handled";
ssh_openssh_public_key_long_header(suite) ->
    [];
ssh_openssh_public_key_long_header(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),

    {ok,RSARawOpenSsh} = file:read_file(filename:join(Datadir, "ssh_rsa_long_header_pub")),
    [{#'RSAPublicKey'{}, _}] = Decoded = public_key:ssh_decode(RSARawOpenSsh, public_key),

    Encoded = public_key:ssh_encode(Decoded, rfc4716_public_key),
    Decoded = public_key:ssh_decode(Encoded, rfc4716_public_key).

%%--------------------------------------------------------------------
encrypt_decrypt(doc) -> 
    [""];
encrypt_decrypt(suite) -> 
    [];
encrypt_decrypt(Config) when is_list(Config) -> 
    {PrivateKey, _DerKey} = erl_make_certs:gen_rsa(64),
    #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} = PrivateKey,
    PublicKey = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    RsaEncrypted = public_key:encrypt_private(Msg, PrivateKey),
    Msg = public_key:decrypt_public(RsaEncrypted, PublicKey),
    Msg = public_key:decrypt_public(RsaEncrypted, PrivateKey),
    RsaEncrypted2 = public_key:encrypt_public(Msg, PublicKey),
    RsaEncrypted3 = public_key:encrypt_public(Msg, PrivateKey),
    Msg = public_key:decrypt_private(RsaEncrypted2, PrivateKey),
    Msg = public_key:decrypt_private(RsaEncrypted3, PrivateKey),

    ok.
       
%%--------------------------------------------------------------------
rsa_sign_verify(doc) ->
    ["Checks that we can sign and verify rsa signatures."];
rsa_sign_verify(suite) ->
    [];
rsa_sign_verify(Config) when is_list(Config) ->
    Ca = {_, CaKey} = erl_make_certs:make_cert([]),
    {Cert1, _} = erl_make_certs:make_cert([{key, dsa}, {issuer, Ca}]),
    PrivateRSA = #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp} = 
	public_key:pem_entry_decode(CaKey),
    PublicRSA = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    true = public_key:pkix_verify(Cert1, PublicRSA),

    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    RSASign = public_key:sign(Msg, sha, PrivateRSA),
    true = public_key:verify(Msg, sha, RSASign, PublicRSA), 
    false = public_key:verify(<<1:8, Msg/binary>>, sha, RSASign, PublicRSA), 
    false = public_key:verify(Msg, sha, <<1:8, RSASign/binary>>, PublicRSA), 

    RSASign1 = public_key:sign(Msg, md5, PrivateRSA),
    true = public_key:verify(Msg, md5, RSASign1, PublicRSA).
    
%%--------------------------------------------------------------------

dsa_sign_verify(doc) ->
    ["Checks that we can sign and verify dsa signatures."];
dsa_sign_verify(suite) ->
    [];
dsa_sign_verify(Config) when is_list(Config) ->
    Ca = erl_make_certs:make_cert([]),
    CertInfo = {_,CertKey1} = erl_make_certs:make_cert([{key, dsa}, {issuer, Ca}]),
    {Cert2,_CertKey} = erl_make_certs:make_cert([{issuer, CertInfo}]),

    #'DSAPrivateKey'{p=P, q=Q, g=G, y=Y, x=_X} =
	public_key:pem_entry_decode(CertKey1),
    true = public_key:pkix_verify(Cert2, {Y, #'Dss-Parms'{p=P, q=Q, g=G}}),

    Datadir = ?config(data_dir, Config),
    [DsaKey = {'DSAPrivateKey', _, _}] = 
	erl_make_certs:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    DSAPrivateKey = public_key:pem_entry_decode(DsaKey),
    #'DSAPrivateKey'{p=P1, q=Q1, g=G1, y=Y1, x=_X1} = DSAPrivateKey,

    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    DSASign = public_key:sign(Msg, sha, DSAPrivateKey),
    DSAPublicKey = Y1,
    DSAParams = #'Dss-Parms'{p=P1, q=Q1, g=G1},
    true = public_key:verify(Msg, sha, DSASign, {DSAPublicKey, DSAParams}), 
    false = public_key:verify(<<1:8, Msg/binary>>, sha, DSASign, 
			      {DSAPublicKey, DSAParams}), 
    false = public_key:verify(Msg, sha, <<1:8, DSASign/binary>>, 
			      {DSAPublicKey, DSAParams}), 
    
    Digest = crypto:sha(Msg),
    DigestSign = public_key:sign(Digest, none, DSAPrivateKey),
    true = public_key:verify(Digest, none, DigestSign, {DSAPublicKey, DSAParams}), 
    <<_:8, RestDigest/binary>> = Digest,
    false = public_key:verify(<<1:8, RestDigest/binary>>, none, DigestSign, 
			      {DSAPublicKey, DSAParams}), 
    false = public_key:verify(Digest, none, <<1:8, DigestSign/binary>>, 
			      {DSAPublicKey, DSAParams}).

%%--------------------------------------------------------------------
pkix(doc) ->
    "Misc pkix tests not covered elsewhere";
pkix(suite) ->
    [];
pkix(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    Certs0 = erl_make_certs:pem_to_der(filename:join(Datadir, "cacerts.pem")),
    Certs1 = erl_make_certs:pem_to_der(filename:join(Datadir, "client_cert.pem")),
    TestTransform = fun({'Certificate', CertDer, not_encrypted}) ->
			    PlainCert = public_key:pkix_decode_cert(CertDer, plain),
			    OtpCert = public_key:pkix_decode_cert(CertDer, otp),
			    CertDer = 
				public_key:pkix_encode('OTPCertificate', OtpCert, otp),
			    CertDer = 
				public_key:pkix_encode('Certificate', PlainCert, plain),
			    OTPTBS = OtpCert#'OTPCertificate'.tbsCertificate,
			    OTPSubj = OTPTBS#'OTPTBSCertificate'.subject, 
			    DNEncoded = public_key:pkix_encode('Name', OTPSubj, otp),
			    PlainTBS = PlainCert#'Certificate'.tbsCertificate,
			    Subj2 = PlainTBS#'TBSCertificate'.subject,
			    DNEncoded = public_key:pkix_encode('Name', Subj2, plain),

			    false = public_key:pkix_is_fixed_dh_cert(CertDer)
		    end,
    [TestTransform(Cert) || Cert <- Certs0 ++ Certs1],

    true = public_key:pkix_is_self_signed(element(2,hd(Certs0))),
    false = public_key:pkix_is_self_signed(element(2,hd(Certs1))),

    CaIds = [element(2, public_key:pkix_issuer_id(Cert, self)) || 
		{'Certificate', Cert, _} <- Certs0],
    {ok, IssuerId = {_, _IssuerName}} = 
	public_key:pkix_issuer_id(element(2,hd(Certs1)), other),

    true = lists:member(IssuerId, CaIds),

    %% Should be normalized allready
    TestStr   = {rdnSequence, 
		 [[{'AttributeTypeAndValue', {2,5,4,3},{printableString,"ERLANGCA"}}],
		  [{'AttributeTypeAndValue', {2,5,4,3},{printableString," erlang  ca "}}]]},
    VerifyStr = {rdnSequence, 
		 [[{'AttributeTypeAndValue', {2,5,4,3},{printableString,"erlang ca"}}],
		  [{'AttributeTypeAndValue', {2,5,4,3},{printableString,"erlangca"}}]]},
    VerifyStr = public_key:pkix_normalize_name(TestStr),

    ok.
%%--------------------------------------------------------------------
pkix_path_validation(doc) ->
    "Misc pkix tests not covered elsewhere";
pkix_path_validation(suite) ->
    [];
pkix_path_validation(Config) when is_list(Config) ->
    CaK = {Trusted,_} = 
	erl_make_certs:make_cert([{key, dsa},
			     {subject, [
					{name, "Public Key"},
					{?'id-at-name', {printableString, "public_key"}},
					{?'id-at-pseudonym', {printableString, "pubkey"}},
					{city, "Stockholm"},
					{country, "SE"},
					{org, "erlang"},
					{org_unit, "testing dep"}
				       ]}
			    ]),
    ok = erl_make_certs:write_pem("./", "public_key_cacert", CaK),

    CertK1 = {Cert1, _} = erl_make_certs:make_cert([{issuer, CaK}]),
    CertK2 = {Cert2,_} = erl_make_certs:make_cert([{issuer, CertK1}, 
					      {digest, md5}, {extensions, false}]),
    ok = erl_make_certs:write_pem("./", "public_key_cert", CertK2),
    
    {ok, _} = public_key:pkix_path_validation(Trusted, [Cert1], []),
    
    {error, {bad_cert,invalid_issuer}} = 
	public_key:pkix_path_validation(Trusted, [Cert2], []),
    
    {ok, _} = public_key:pkix_path_validation(Trusted, [Cert1, Cert2], []),    
    {error, issuer_not_found} = public_key:pkix_issuer_id(Cert2, other),

    CertK3 = {Cert3,_}  = erl_make_certs:make_cert([{issuer, CertK1}, 
					       {extensions, [{basic_constraints, false}]}]),
    {Cert4,_}  = erl_make_certs:make_cert([{issuer, CertK3}]),
    {error, {bad_cert,missing_basic_constraint}} =
	public_key:pkix_path_validation(Trusted, [Cert1, Cert3,Cert4], []),

    VerifyFunAndState0  = {fun(_,{bad_cert, missing_basic_constraint}, UserState) ->
				   {valid, UserState};
			      (_,{bad_cert, _} = Reason, _) ->
				   {fail, Reason};
			      (_,{extension, _}, UserState) ->
				   {unknown, UserState};
			      (_, valid, UserState) ->
				   {valid, UserState};
			      (_, valid_peer, UserState) ->
				   {valid, UserState}
			   end, []},
    {ok, _} =
	public_key:pkix_path_validation(Trusted, [Cert1, Cert3,Cert4],
					[{verify_fun, VerifyFunAndState0}]),

    {error, {bad_cert, unknown_ca}} =
	public_key:pkix_path_validation(unknown_ca, [Cert1, Cert3, Cert4], []),

    VerifyFunAndState1 =
	{fun(_,{bad_cert, unknown_ca}, UserState) ->
		 {valid, UserState};
	    (_,{bad_cert, _} = Reason, _) ->
		 {fail, Reason};
	    (_,{extension, _}, UserState) ->
		 {unknown, UserState};
	    (_, valid, UserState) ->
		 {valid, UserState}
	 end, []},

    {ok, _} =
	public_key:pkix_path_validation(unknown_ca, [Cert1], [{verify_fun,
							      VerifyFunAndState1}]),
    ok.

%%--------------------------------------------------------------------
deprecated(doc) -> 
    ["Check deprecated functions."];
deprecated(suite) -> 
    [];
deprecated(Config) when is_list(Config) -> 
    Datadir = ?config(data_dir, Config),
    {ok, [DsaKey = {'DSAPrivateKey', _DsaKey, _}]} =
	public_key:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    {ok, [RsaKey = {'RSAPrivateKey', _RsaKey,_}]} =
	public_key:pem_to_der(filename:join(Datadir, "client_key.pem")),
    {ok, [ProtectedRsaKey = {'RSAPrivateKey', _ProtectedRsaKey,_}]} =
	public_key:pem_to_der(filename:join(Datadir, "rsa.pem")),

    {ok, #'DSAPrivateKey'{}} = public_key:decode_private_key(DsaKey),
    {ok, #'RSAPrivateKey'{}} = public_key:decode_private_key(RsaKey),
    {ok, #'RSAPrivateKey'{}} = public_key:decode_private_key(ProtectedRsaKey, "abcd1234"),
    ok.

%%--------------------------------------------------------------------

check_entry_type(#'DSAPrivateKey'{}, 'DSAPrivateKey') ->
    true;
check_entry_type(#'RSAPrivateKey'{}, 'RSAPrivateKey') ->
    true;
check_entry_type(#'RSAPublicKey'{}, 'RSAPublicKey') ->
    true;
check_entry_type({_Int, #'Dss-Parms'{}}, 'DSAPublicKey') when is_integer(_Int) ->
    true;
check_entry_type(#'DHParameter'{}, 'DHParameter') ->
    true;
check_entry_type(#'Certificate'{}, 'Certificate') ->
    true;
check_entry_type(_,_) ->
    false.

strip_ending_newlines(Bin) ->
    string:strip(binary_to_list(Bin), right, 10).
