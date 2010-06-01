%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").
-include_lib("test_server/include/test_server_line.hrl").

-include("public_key.hrl").

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
    crypto:start(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    crypto:stop().

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
all(doc) -> 
    ["Test the public_key rsa functionality"];

all(suite) -> 
    [app, 
     dh, 
     pem_to_der, 
     decode_private_key,
     encrypt_decrypt, 
     sign_verify,
     pkix,
     pkix_path_validation
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------

app(doc) ->
    "Test that the public_key app file is ok";
app(suite) ->
    [];
app(Config) when is_list(Config) ->
    ok = test_server:app_test(public_key).

dh(doc) ->
    "Test diffie-hellman functions file is ok";
dh(suite) ->
    [];
dh(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    {ok,[DerDHparams = {dh_params, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "dh.pem")),
    {ok, DHps = #'DHParameter'{prime=P,base=G}} = public_key:decode_dhparams(DerDHparams),
    DHKeys = {Private,_Public} = public_key:gen_key(DHps),
    test_server:format("DHparams = ~p~nDH Keys~p~n", [DHps, DHKeys]),
    {_Private,_Public2} = pubkey_crypto:gen_key(diffie_hellman, [crypto:erlint(Private), P, G]),
    ok.
    

pem_to_der(doc) -> 
    ["Check that supported PEM files are decoded into the expected entry type"];
pem_to_der(suite) -> 
    [];
pem_to_der(Config) when is_list(Config) -> 
    Datadir = ?config(data_dir, Config),
    {ok,DSAKey =[{dsa_private_key, _, not_encrypted}]} = 
	public_key:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    {ok,[{rsa_private_key, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "client_key.pem")),
    {ok, [{rsa_private_key, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "rsa.pem")),
    {ok,[{rsa_private_key, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "rsa.pem"), "abcd1234"),
    {ok, Bin0} = file:read_file(filename:join(Datadir, "rsa.pem")), 
    {ok, [{rsa_private_key, _, _}]} = public_key:pem_to_der(Bin0, "abcd1234"),

    {ok,[{dh_params, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "dh.pem")),
    {ok,[{cert, _, not_encrypted}]} = 
	public_key:pem_to_der(filename:join(Datadir, "client_cert.pem")),
    {ok,[{cert_req, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "req.pem")),
    {ok, Certs = [{cert, _, _}, {cert, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "cacerts.pem")),

    {ok, Bin1} = file:read_file(filename:join(Datadir, "cacerts.pem")),
    {ok, [{cert, _, _}, {cert, _, _}]} = public_key:pem_to_der(Bin1),

    ok = public_key:der_to_pem(filename:join(Datadir, "wcacerts.pem"), Certs), 
    ok = public_key:der_to_pem(filename:join(Datadir, "wdsa.pem"), DSAKey), 
    
    {ok, Certs} = public_key:pem_to_der(filename:join(Datadir, "wcacerts.pem")),
    {ok, DSAKey} = public_key:pem_to_der(filename:join(Datadir, "wdsa.pem")),

    ok.
%%--------------------------------------------------------------------
decode_private_key(doc) -> 
    ["Check that private keys are decode to the expected key type."];
decode_private_key(suite) -> 
    [];
decode_private_key(Config) when is_list(Config) -> 
    Datadir = ?config(data_dir, Config),
    {ok,[DsaKey = {dsa_private_key, _DsaKey, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    {ok,[RsaKey = {rsa_private_key, _RsaKey,_}]} = 
	public_key:pem_to_der(filename:join(Datadir, "client_key.pem")),
    {ok,[ProtectedRsaKey1 = {rsa_private_key, _ProtectedRsaKey1,_}]} = 
	public_key:pem_to_der(filename:join(Datadir, "rsa.pem"), "abcd1234"),
    {ok,[ProtectedRsaKey2 = {rsa_private_key, _ProtectedRsaKey2,_}]} = 
	public_key:pem_to_der(filename:join(Datadir, "rsa.pem")),

    {ok, #'DSAPrivateKey'{}} = public_key:decode_private_key(DsaKey),
    {ok, #'RSAPrivateKey'{}} = public_key:decode_private_key(RsaKey),
    {ok, #'RSAPrivateKey'{}} = public_key:decode_private_key(ProtectedRsaKey1),
    {ok, #'RSAPrivateKey'{}} = public_key:decode_private_key(ProtectedRsaKey2, "abcd1234"),
    ok.
%%--------------------------------------------------------------------
encrypt_decrypt(doc) -> 
    [""];
encrypt_decrypt(suite) -> 
    [];
encrypt_decrypt(Config) when is_list(Config) -> 
    {PrivateKey, _DerKey} = pkey_test:gen_rsa(64),
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
sign_verify(doc) -> 
    ["Checks that we can sign and verify signatures."];
sign_verify(suite) -> 
    [];
sign_verify(Config) when is_list(Config) -> 
    %% Make cert signs and validates the signature using RSA and DSA
    Ca = {_, CaKey} = pkey_test:make_cert([]),
    {ok, PrivateRSA = #'RSAPrivateKey'{modulus=Mod, publicExponent=Exp}} = 
	public_key:decode_private_key(CaKey),

    CertInfo = {Cert1,CertKey1} = pkey_test:make_cert([{key, dsa}, {issuer, Ca}]),

    PublicRSA = #'RSAPublicKey'{modulus=Mod, publicExponent=Exp},
    true = public_key:verify_signature(Cert1, PublicRSA, undefined),

    {Cert2,_CertKey} = pkey_test:make_cert([{issuer, CertInfo}]),

    {ok, #'DSAPrivateKey'{p=P, q=Q, g=G, y=Y, x=_X}} = 
	public_key:decode_private_key(CertKey1),
    true = public_key:verify_signature(Cert2, Y, #'Dss-Parms'{p=P, q=Q, g=G}),
    
    %% RSA sign
    Msg0 = lists:duplicate(5, "Foo bar 100"),
    Msg  = list_to_binary(Msg0),
    RSASign = public_key:sign(sha, Msg0, PrivateRSA),
    RSASign = public_key:sign(Msg, PrivateRSA),
    true = public_key:verify_signature(Msg, sha, RSASign, PublicRSA), 
    false = public_key:verify_signature(<<1:8, Msg/binary>>, sha, RSASign, PublicRSA), 
    false = public_key:verify_signature(Msg, sha, <<1:8, RSASign/binary>>, PublicRSA), 
    RSASign = public_key:sign(sha, Msg, PrivateRSA),

    RSASign1 = public_key:sign(md5, Msg, PrivateRSA),
    true = public_key:verify_signature(Msg, md5, RSASign1, PublicRSA), 
    
    %% DSA sign
    Datadir = ?config(data_dir, Config),
    {ok,[DsaKey = {dsa_private_key, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    {ok, DSAPrivateKey} = public_key:decode_private_key(DsaKey),
    #'DSAPrivateKey'{p=P1, q=Q1, g=G1, y=Y1, x=_X1} = DSAPrivateKey,
    DSASign = public_key:sign(Msg, DSAPrivateKey),
    DSAPublicKey = Y1,
    DSAParams = #'Dss-Parms'{p=P1, q=Q1, g=G1},
    true = public_key:verify_signature(Msg, sha, DSASign, DSAPublicKey, DSAParams), 
    false = public_key:verify_signature(<<1:8, Msg/binary>>, sha, DSASign, DSAPublicKey, DSAParams), 
    false = public_key:verify_signature(Msg, sha, <<1:8, DSASign/binary>>, DSAPublicKey, DSAParams), 
    
    ok.

pkix(doc) ->
    "Misc pkix tests not covered elsewhere";
pkix(suite) ->
    [];
pkix(Config) when is_list(Config) ->
    Datadir = ?config(data_dir, Config),
    {ok,Certs0} = public_key:pem_to_der(filename:join(Datadir, "cacerts.pem")),
    {ok,Certs1} = public_key:pem_to_der(filename:join(Datadir, "client_cert.pem")),
    TestTransform = fun({cert, CertDer, not_encrypted}) ->
			    {ok, PlainCert} = public_key:pkix_decode_cert(CertDer, plain),
			    {ok, OtpCert} = public_key:pkix_decode_cert(CertDer, otp),
			    CertDer = public_key:pkix_encode_cert(OtpCert),
			    CertDer = public_key:pkix_encode_cert(PlainCert),

			    OTPSubj = (OtpCert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subject,
			    Subj = public_key:pkix_transform(OTPSubj, encode),
			    {ok, DNEncoded} = 'OTP-PUB-KEY':encode('Name', Subj),
			    Subj2 = (PlainCert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
			    {ok, DNEncoded} = 'OTP-PUB-KEY':encode('Name', Subj2),
			    OTPSubj = public_key:pkix_transform(Subj2, decode),

			    false = public_key:pkix_is_fixed_dh_cert(CertDer)
		    end,
    [TestTransform(Cert) || Cert <- Certs0 ++ Certs1],

    true = public_key:pkix_is_self_signed(element(2,hd(Certs0))),
    false = public_key:pkix_is_self_signed(element(2,hd(Certs1))),

    CaIds = [element(2, public_key:pkix_issuer_id(Cert, self)) || {cert, Cert, _} <- Certs0],
    {ok, IssuerId = {_, IssuerName}} = public_key:pkix_issuer_id(element(2,hd(Certs1)), other),
    true = lists:member(IssuerId, CaIds),

    %% Should be normalized allready
    TestStr   = {rdnSequence, [[{'AttributeTypeAndValue', {2,5,4,3},{printableString,"ERLANGCA"}}],
			       [{'AttributeTypeAndValue', {2,5,4,3},{printableString," erlang  ca "}}]]},
    VerifyStr = {rdnSequence, [[{'AttributeTypeAndValue', {2,5,4,3},{printableString,"erlang ca"}}],
			       [{'AttributeTypeAndValue', {2,5,4,3},{printableString,"erlangca"}}]]},
    VerifyStr = public_key:pkix_normalize_general_name(TestStr),

    ok.

pkix_path_validation(doc) ->
    "Misc pkix tests not covered elsewhere";
pkix_path_validation(suite) ->
    [];
pkix_path_validation(Config) when is_list(Config) ->
    CaK = {Trusted,_} = 
	pkey_test:make_cert([{key, dsa},
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
    ok = pkey_test:write_pem("/tmp", "cacert", CaK),

    CertK1 = {Cert1, _} = pkey_test:make_cert([{issuer, CaK}]),
    CertK2 = {Cert2,_} = pkey_test:make_cert([{issuer, CertK1}, {digest, md5}, {extensions, false}]),
    ok = pkey_test:write_pem("/tmp", "cert", CertK2),

    {ok, _} = public_key:pkix_path_validation(Trusted, [Cert1], []),
    
    {error, {bad_cert,invalid_issuer}} = public_key:pkix_path_validation(Trusted, [Cert2], []),
    %%{error, {bad_cert,invalid_issuer}} = public_key:pkix_path_validation(Trusted, [Cert2], [{verify,false}]), 
    
    {ok, _} = public_key:pkix_path_validation(Trusted, [Cert1, Cert2], []),    
    {error, issuer_not_found} = public_key:pkix_issuer_id(Cert2, other),

    CertK3 = {Cert3,_}  = pkey_test:make_cert([{issuer, CertK1}, {extensions, [{basic_constraints, false}]}]),
    {Cert4,_}  = pkey_test:make_cert([{issuer, CertK3}]),
    {error, E={bad_cert,missing_basic_constraint}} = 
	public_key:pkix_path_validation(Trusted, [Cert1, Cert3,Cert4], []),
    
    {ok, {_,_,[E]}} = public_key:pkix_path_validation(Trusted, [Cert1, Cert3,Cert4], [{verify,false}]),

    % test_server:format("PV ~p ~n", [Result]),
    ok.
