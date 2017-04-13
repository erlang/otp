%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2017. All Rights Reserved.
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

 -module(x509_test).

 -include_lib("public_key/include/public_key.hrl").

 -export([gen_test_certs/1, gen_pem_config_files/3]).

 gen_test_certs(Opts) ->
     SRootKey = gen_key(proplists:get_value(server_key_gen, Opts)),
     CRootKey = gen_key(proplists:get_value(client_key_gen, Opts)),
     ServerRoot = root_cert("server", SRootKey, Opts),
     ClientRoot = root_cert("client", CRootKey, Opts),
     [{ServerCert, ServerKey} | ServerCAsKeys] = config(server, ServerRoot, SRootKey, Opts),
     [{ClientCert, ClientKey} | ClientCAsKeys] = config(client, ClientRoot, CRootKey, Opts),
     ServerCAs = ca_config(ClientRoot, ServerCAsKeys),
     ClientCAs = ca_config(ServerRoot, ClientCAsKeys),
     [{server_config, [{cert, ServerCert}, {key, ServerKey}, {cacerts, ServerCAs}]}, 
      {client_config, [{cert, ClientCert}, {key, ClientKey}, {cacerts, ClientCAs}]}].

gen_pem_config_files(GenCertData, ClientBase, ServerBase) ->
    ServerConf = proplists:get_value(server_config, GenCertData),
    ClientConf = proplists:get_value(client_config, GenCertData),
    
    ServerCaCertFile = ServerBase ++ "_server_cacerts.pem",
    ServerCertFile = ServerBase ++ "_server_cert.pem",
    ServerKeyFile = ServerBase ++ "_server_key.pem",
    
    ClientCaCertFile = ClientBase ++ "_client_cacerts.pem",
    ClientCertFile =  ClientBase ++ "_client_cert.pem",
    ClientKeyFile = ClientBase ++ "_client_key.pem",

    do_gen_pem_config_files(ServerConf,
                            ServerCertFile,
                            ServerKeyFile,
                            ServerCaCertFile),        
    do_gen_pem_config_files(ClientConf,
                            ClientCertFile,
                            ClientKeyFile,
                            ClientCaCertFile),
    [{server_config, [{certfile, ServerCertFile}, 
                      {keyfile, ServerKeyFile}, {cacertfile, ServerCaCertFile}]}, 
     {client_config, [{certfile, ClientCertFile}, 
                      {keyfile, ClientKeyFile}, {cacertfile, ClientCaCertFile}]}].


 do_gen_pem_config_files(Config, CertFile, KeyFile, CAFile) ->
     CAs = proplists:get_value(cacerts, Config),
     Cert = proplists:get_value(cert, Config),
     Key = proplists:get_value(key, Config),
     der_to_pem(CertFile, [cert_entry(Cert)]),
     der_to_pem(KeyFile, [key_entry(Key)]),
     der_to_pem(CAFile, ca_entries(CAs)).

 cert_entry(Cert) ->
     {'Certificate', Cert, not_encrypted}.

 key_entry(Key = #'RSAPrivateKey'{}) ->
     Der = public_key:der_encode('RSAPrivateKey', Key),
     {'RSAPrivateKey', Der, not_encrypted};
 key_entry(Key = #'DSAPrivateKey'{}) ->
     Der =  public_key:der_encode('DSAPrivateKey', Key),
     {'DSAPrivateKey', Der, not_encrypted};
 key_entry(Key = #'ECPrivateKey'{}) ->
     Der =  public_key:der_encode('ECPrivateKey', Key),
     {'ECPrivateKey', Der, not_encrypted}.

 ca_entries(CAs) ->
     [{'Certificate', CACert, not_encrypted} || CACert <- CAs].

 gen_key(KeyGen) ->
     case is_key(KeyGen) of
         true ->
             KeyGen;
         false ->
             public_key:generate_key(KeyGen)
     end.

root_cert(Role, PrivKey, Opts) ->
     TBS = cert_template(),
     Issuer = issuer("root", Role, " ROOT CA"),
     OTPTBS = TBS#'OTPTBSCertificate'{
                signature = sign_algorithm(PrivKey, Opts),
                issuer = Issuer,
                validity = validity(Opts),  
                subject = Issuer,
                subjectPublicKeyInfo = public_key(PrivKey),
                extensions = extensions(ca, Opts)
               },
     public_key:pkix_sign(OTPTBS, PrivKey).

config(Role, Root, Key, Opts) ->
    KeyGenOpt = list_to_atom(atom_to_list(Role) ++ "_key_gen_chain"),
    KeyGens = proplists:get_value(KeyGenOpt, Opts, default_key_gen()),
    Keys = lists:map(fun gen_key/1, KeyGens),
    cert_chain(Role, Root, Key, Opts, Keys).

cert_template() ->
    #'OTPTBSCertificate'{
       version = v3,              
       serialNumber = trunc(rand:uniform()*100000000)*10000 + 1,
       issuerUniqueID = asn1_NOVALUE,       
       subjectUniqueID = asn1_NOVALUE
      }.

issuer(Contact, Role, Name) ->
  subject(Contact, Role ++ Name).

subject(Contact, Name) ->
    Opts = [{email, Contact ++ "@erlang.org"},
	    {name,  Name},
	    {city, "Stockholm"},
	    {country, "SE"},
	    {org, "erlang"},
	    {org_unit, "automated testing"}],
    subject(Opts).

subject(SubjectOpts) when is_list(SubjectOpts) ->
    Encode = fun(Opt) ->
		     {Type,Value} = subject_enc(Opt),
		     [#'AttributeTypeAndValue'{type=Type, value=Value}]
	     end,
    {rdnSequence, [Encode(Opt) || Opt <- SubjectOpts]}.

subject_enc({name,  Name}) ->       
    {?'id-at-commonName', {printableString, Name}};
subject_enc({email, Email}) ->      
    {?'id-emailAddress', Email};
subject_enc({city,  City}) ->       
    {?'id-at-localityName', {printableString, City}};
subject_enc({state, State}) ->      
    {?'id-at-stateOrProvinceName', {printableString, State}};
subject_enc({org, Org}) ->          
    {?'id-at-organizationName', {printableString, Org}};
subject_enc({org_unit, OrgUnit}) -> 
    {?'id-at-organizationalUnitName', {printableString, OrgUnit}};
subject_enc({country, Country}) ->  
    {?'id-at-countryName', Country};
subject_enc({serial, Serial}) ->    
    {?'id-at-serialNumber', Serial};
subject_enc({title, Title}) ->      
    {?'id-at-title', {printableString, Title}};
subject_enc({dnQualifer, DnQ}) ->   
    {?'id-at-dnQualifier', DnQ};
subject_enc(Other) -> 
    Other.

validity(Opts) ->
    DefFrom0 = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())-1),
    DefTo0   = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())+7),
    {DefFrom, DefTo} = proplists:get_value(validity, Opts, {DefFrom0, DefTo0}),
    Format = fun({Y,M,D}) -> 
                     lists:flatten(io_lib:format("~w~2..0w~2..0w000000Z",[Y,M,D])) 
             end,
    #'Validity'{notBefore={generalTime, Format(DefFrom)},
		notAfter ={generalTime, Format(DefTo)}}.

extensions(Type, Opts) ->
    Exts  = proplists:get_value(extensions, Opts, []),
    lists:flatten([extension(Ext) || Ext <- default_extensions(Type, Exts)]).

%% Common extension: name_constraints, policy_constraints, ext_key_usage, inhibit_any, 
%% auth_key_id, subject_key_id, policy_mapping,

default_extensions(ca, Exts) ->
    Def = [{key_usage,  [keyCertSign, cRLSign]}, 
	   {basic_constraints, default}],
    add_default_extensions(Def, Exts);

default_extensions(peer, Exts) ->
    Def = [{key_usage, [digitalSignature, keyAgreement]}],
    add_default_extensions(Def, Exts).
    
add_default_extensions(Def, Exts) ->
    Filter = fun({Key, _}, D) -> 
                     lists:keydelete(Key, 1, D); 
                ({Key, _, _}, D) -> 
                     lists:keydelete(Key, 1, D)
             end,
    Exts ++ lists:foldl(Filter, Def, Exts).

extension({_, undefined}) ->
    [];
extension({basic_constraints, Data}) ->
    case Data of
	default ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = #'BasicConstraints'{cA=true},
			 critical=true};
	false -> 
	    [];
	Len when is_integer(Len) ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = #'BasicConstraints'{cA=true, pathLenConstraint = Len},
			 critical = true};
	_ ->
	    #'Extension'{extnID = ?'id-ce-basicConstraints',
			 extnValue = Data}
    end;
extension({auth_key_id, {Oid, Issuer, SNr}}) ->
    #'Extension'{extnID = ?'id-ce-authorityKeyIdentifier',
                 extnValue = #'AuthorityKeyIdentifier'{
                                keyIdentifier = Oid,	    
                                authorityCertIssuer = Issuer,     
                                authorityCertSerialNumber = SNr},
                 critical = false};
extension({key_usage, Value}) ->
    #'Extension'{extnID = ?'id-ce-keyUsage',
                 extnValue = Value,
                 critical = false};
extension({Id, Data, Critical}) ->
    #'Extension'{extnID = Id, extnValue = Data, critical = Critical}.

public_key(#'RSAPrivateKey'{modulus=N, publicExponent=E}) ->
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?rsaEncryption, parameters='NULL'},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = Public};
public_key(#'DSAPrivateKey'{p=P, q=Q, g=G, y=Y}) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-dsa', 
				 parameters={params, #'Dss-Parms'{p=P, q=Q, g=G}}},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo, subjectPublicKey = Y};
public_key(#'ECPrivateKey'{version = _Version,
			  privateKey = _PrivKey,
			  parameters = Params,
			  publicKey = PubKey}) ->
    Algo = #'PublicKeyAlgorithm'{algorithm= ?'id-ecPublicKey', parameters=Params},
    #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
			       subjectPublicKey = #'ECPoint'{point = PubKey}}.

sign_algorithm(#'RSAPrivateKey'{}, Opts) ->
    Type = rsa_digest_oid(proplists:get_value(digest, Opts, sha1)),
    #'SignatureAlgorithm'{algorithm  = Type,
                          parameters = 'NULL'};
sign_algorithm(#'DSAPrivateKey'{p=P, q=Q, g=G}, _Opts) ->
    #'SignatureAlgorithm'{algorithm  = ?'id-dsa-with-sha1',
                          parameters = {params,#'Dss-Parms'{p=P, q=Q, g=G}}};
sign_algorithm(#'ECPrivateKey'{parameters = Parms}, Opts) ->
    Type = ecdsa_digest_oid(proplists:get_value(digest, Opts, sha1)),
    #'SignatureAlgorithm'{algorithm  = Type,
                          parameters = Parms}.

rsa_digest_oid(sha1) ->
    ?'sha1WithRSAEncryption';
rsa_digest_oid(sha512) ->
    ?'sha512WithRSAEncryption';
rsa_digest_oid(sha384) ->
    ?'sha384WithRSAEncryption';
rsa_digest_oid(sha256) ->
    ?'sha256WithRSAEncryption';
rsa_digest_oid(md5) ->
   ?'md5WithRSAEncryption'.

ecdsa_digest_oid(sha1) ->
    ?'ecdsa-with-SHA1';
ecdsa_digest_oid(sha512) ->
    ?'ecdsa-with-SHA512';
ecdsa_digest_oid(sha384) ->
    ?'ecdsa-with-SHA384';
ecdsa_digest_oid(sha256) ->
    ?'ecdsa-with-SHA256'.

ca_config(Root, CAsKeys) ->
    [Root | [CA || {CA, _}  <- CAsKeys]].

cert_chain(Role, Root, RootKey, Opts, Keys) ->
    cert_chain(Role, Root, RootKey, Opts, Keys, 0, []).

cert_chain(Role, IssuerCert, IssuerKey, Opts, [Key], _, Acc) ->
    PeerOpts = list_to_atom(atom_to_list(Role) ++ "_peer_opts"),
    Cert = cert(Role, public_key:pkix_decode_cert(IssuerCert, otp), 
                IssuerKey, Key, "admin", " Peer cert", Opts, PeerOpts, peer),
    [{Cert, Key}, {IssuerCert, IssuerKey} | Acc];
cert_chain(Role, IssuerCert, IssuerKey, Opts, [Key | Keys], N, Acc) ->
    CAOpts = list_to_atom(atom_to_list(Role) ++ "_ca_" ++ integer_to_list(N)),
    Cert = cert(Role, public_key:pkix_decode_cert(IssuerCert, otp), IssuerKey, Key, "webadmin", 
                " Intermidiate CA " ++ integer_to_list(N), Opts, CAOpts, ca),
    cert_chain(Role, Cert, Key, Opts, Keys, N+1, [{IssuerCert, IssuerKey} | Acc]).
        
cert(Role, #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = Issuer,
                                                                   serialNumber = SNr
                                                                  }}, 
     PrivKey, Key, Contact, Name, Opts, CertOptsName, Type) ->
    CertOpts = proplists:get_value(CertOptsName, Opts, []),
    TBS = cert_template(),         
    OTPTBS = TBS#'OTPTBSCertificate'{
               signature = sign_algorithm(PrivKey, Opts),
               issuer =  Issuer,
               validity = validity(CertOpts),  
               subject = subject(Contact, atom_to_list(Role) ++ Name),
               subjectPublicKeyInfo = public_key(Key),
               extensions = extensions(Type, 
                                       add_default_extensions([{auth_key_id, {auth_key_oid(Role), Issuer, SNr}}],
                                                              CertOpts))
              },
    public_key:pkix_sign(OTPTBS, PrivKey).

is_key(#'DSAPrivateKey'{}) ->
    true;
is_key(#'RSAPrivateKey'{}) ->
    true;
is_key(#'ECPrivateKey'{}) ->
    true;
is_key(_) ->
    false.

der_to_pem(File, Entries) ->
    PemBin = public_key:pem_encode(Entries),
    file:write_file(File, PemBin).

default_key_gen() ->
    case tls_v1:ecc_curves(0) of
        [] ->
            [{rsa, 2048, 17}, {rsa, 2048, 17}];
        [_|_] ->
            [{namedCurve, hd(tls_v1:ecc_curves(0))},
             {namedCurve, hd(tls_v1:ecc_curves(0))}]
    end.

auth_key_oid(server) ->
    ?'id-kp-serverAuth';
auth_key_oid(client) ->
    ?'id-kp-clientAuth'.
