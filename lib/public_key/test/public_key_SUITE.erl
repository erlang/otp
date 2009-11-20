%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

-include("test_server.hrl").
-include("test_server_line.hrl").
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
     pem_to_der, 
     decode_private_key
%%    encrypt_decrypt, 
%%     rsa_verify
%%      dsa_verify_sign,
%%      pkix_encode_decode,
%%      pkix_verify_sign, 
%%      pkix_path_validation
    ].

%% Test cases starts here.
%%--------------------------------------------------------------------

app(doc) ->
    "Test that the public_key app file is ok";
app(suite) ->
    [];
app(Config) when list(Config) ->
    ok = test_server:app_test(public_key).

pem_to_der(doc) -> 
    ["Check that supported PEM files are decoded into the expected entry type"];
pem_to_der(suite) -> 
    [];
pem_to_der(Config) when is_list(Config) -> 
    Datadir = ?config(data_dir, Config),
    {ok,[{dsa_private_key, _, not_encrypted}]} = 
	public_key:pem_to_der(filename:join(Datadir, "dsa.pem")), 
    {ok,[{rsa_private_key, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "client_key.pem")),
    {ok,[{rsa_private_key, _, _}]} = 
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
    {ok,[{cert, _, _}, {cert, _, _}]} = 
	public_key:pem_to_der(filename:join(Datadir, "cacerts.pem")),

    {ok, Bin1} = file:read_file(filename:join(Datadir, "cacerts.pem")),    
    {ok, [{cert, _, _}, {cert, _, _}]} = public_key:pem_to_der(Bin1),
    
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
    RSAPrivateKey = #'RSAPrivateKey'{publicExponent = 17,	
				     modulus = 3233,
				     privateExponent = 2753,
				     prime1 = 61,
				     prime2 = 53,
				     version = 'two-prime'},
    Msg = <<0,123>>,   
    {ok, Encrypted} = public_key:encrypt(Msg, RSAPrivateKey, [{block_type, 2}]),
    test_server:format("Expected 855, Encrypted  ~p ~n", [Encrypted]),
    ok.
    








%%     Datadir = ?config(data_dir, Config),
%%      {ok,[{rsa_private_key, EncKey}]} = 
%% 	public_key:pem_to_der(filename:join(Datadir, "server_key.pem")), 
%%     {ok, Key} = public_key:decode_private_key(EncKey, rsa),
%%     RSAPublicKey = #'RSAPublicKey'{publicExponent =
%% 				   Key#'RSAPrivateKey'.publicExponent,
%% 				   modulus = Key#'RSAPrivateKey'.modulus},
%%     {ok, Msg} = file:read_file(filename:join(Datadir, "msg.txt")),
%%     Hash = crypto:sha(Msg),
%%     {ok, Encrypted} = public_key:encrypt(Hash, Key, [{block_type, 2}]),
%%     test_server:format("Encrypted ~p", [Encrypted]),
%%     {ok, Decrypted} = public_key:decrypt(Encrypted, 
%% 					 RSAPublicKey, [{block_type, 1}]),
%%     test_server:format("Encrypted ~p", [Decrypted]),
%%     true = Encrypted == Decrypted. 
    
%%--------------------------------------------------------------------
rsa_verify(doc) -> 
    ["Cheks that we can verify an rsa signature."];
rsa_verify(suite) -> 
    [];
rsa_verify(Config) when is_list(Config) -> 
    Datadir = ?config(data_dir, Config),
    
    {ok,[{cert, DerCert}]} = 
	public_key:pem_to_der(filename:join(Datadir, "server_cert.pem")),
    
    {ok, OTPCert} = public_key:pkix_decode_cert(DerCert, otp),
    
    {0, Signature} = OTPCert#'Certificate'.signature,
    TBSCert =  OTPCert#'Certificate'.tbsCertificate,

    #'TBSCertificate'{subjectPublicKeyInfo = Info} = TBSCert,
    
    #'SubjectPublicKeyInfo'{subjectPublicKey = RSAPublicKey} = Info,
    
    EncTBSCert = encoded_tbs_cert(DerCert),
    Digest = crypto:sha(EncTBSCert),

    public_key:verify_signature(Digest, Signature, RSAPublicKey).


%% Signature is generated in the following way (in datadir):
%% openssl dgst -sha1 -binary -out rsa_signature -sign server_key.pem msg.txt
%%{ok, Signature} = file:read_file(filename:join(Datadir, "rsa_signature")),
%%{ok, Signature} = file:read_file(filename:join(Datadir, "rsa_signature")),
%% {ok, Msg} = file:read_file(filename:join(Datadir, "msg.txt")),
%% Digest = crypto:sha(Msg),
%% {ok,[{rsa_private_key, EncKey}]} = 
%%	public_key:pem_to_der(filename:join(Datadir, "server_key.pem")), 
%%    {ok, Key} = public_key:decode_private_key(EncKey, rsa),
%%    RSAPublicKey = #'RSAPublicKey'{publicExponent =
%%				   Key#'RSAPrivateKey'.publicExponent,
%%				   modulus = Key#'RSAPrivateKey'.modulus},

encoded_tbs_cert(Cert) ->
    {ok, PKIXCert} = 
	'OTP-PUB-KEY':decode_TBSCert_exclusive(Cert),
    {'Certificate',
     {'Certificate_tbsCertificate', EncodedTBSCert}, _, _} = PKIXCert,
    EncodedTBSCert.

