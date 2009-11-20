%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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

%%% Purpose : API module for decoding of certificates.

-module(ssl_pkix).

-include("ssl_pkix.hrl").

-export([decode_cert_file/1, decode_cert_file/2, 
	 decode_cert/1, decode_cert/2, encode_cert/1, encoded_tbs_cert/1,
	 signature_digest/1, decode_rsa_keyfile/2]).

%% The public API is dprecated by public_key and
%% the internal application API is no longer used ssl.
%% So this file can be compleatly removed in  R14. 
-deprecated({decode_cert_file, 1, next_major_release}).
-deprecated({decode_cert_file, 2, next_major_release}).
-deprecated({decode_cert, 1, next_major_release}).
-deprecated({decode_cert, 2, next_major_release}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: decode_cert_file(File, <Opts>) -> {ok, Cert} | {ok, [Cert]}
%%           
%%  File = string() 
%%  Opts = [Opt]    
%%  Opt = pem | ssl | pkix - ssl and pkix are mutual exclusive
%%  Cert = term()
%%
%% Description: Decodes certificats found in file <File>.
%% If the options list is empty the certificate is
%% returned as a DER encoded binary, i.e. {ok, Bin} is returned, where
%% Bin> is the provided input. The options pkix and ssl imply that the
%% certificate is returned as a parsed ASN.1 structure in the form of
%% an Erlang term.  The ssl option gives a more elaborate return
%% structure, with more explicit information. In particular object
%% identifiers are replaced by atoms. The option subject implies that
%% only the subject's distinguished name part of the certificate is
%% returned. It can only be used together with the option pkix or the
%% option ssl.
%%--------------------------------------------------------------------
decode_cert_file(File) ->
    decode_cert_file(File, []).

decode_cert_file(File, Opts) ->
    case lists:member(pem, Opts) of
	true -> 
	    {ok, List} = ssl_pem:read_file(File),
	    Certs = [Bin || {cert, Bin} <- List],
	    NewOpts =  lists:delete(pem, Opts),
	    Fun = fun(Cert) ->
			  {ok, Decoded} = decode_cert(Cert, NewOpts),
			  Decoded
		  end,
	    case lists:map(Fun, Certs) of
		[DecodedCert] ->
		    {ok, DecodedCert};
		DecodedCerts ->
		    {ok, DecodedCerts}
	    end;
	false  ->
	    {ok, Bin} = file:read_file(File),
	    decode_cert(Bin, Opts)
    end.
%%--------------------------------------------------------------------
%% Function: decode_cert(Bin, <Opts>) -> {ok, Cert}
%% Bin - binary()           
%% Opts = [Opt]    
%% Opt = ssl | pkix | subject - ssl and pkix are mutual exclusive      
%% Cert = term()
%% 
%% Description: If the options list is empty the certificate is
%% returned as a DER encoded binary, i.e. {ok, Bin} is returned, where
%% Bin> is the provided input. The options pkix and ssl imply that the
%% certificate is returned as a parsed ASN.1 structure in the form of
%% an Erlang term.  The ssl option gives a more elaborate return
%% structure, with more explicit information. In particular object
%% identifiers are replaced by atoms. The option subject implies that
%% only the subject's distinguished name part of the certificate is
%% returned. It can only be used together with the option pkix or the
%% option ssl.
%%--------------------------------------------------------------------
decode_cert(Bin) ->
    decode_cert(Bin, []).

decode_cert(Bin, []) when is_binary(Bin) ->
    {ok, Bin};
decode_cert(Bin, Opts) when is_binary(Bin) ->

    {ok, Cert} = 'OTP-PKIX':decode('Certificate', Bin), 

    case {lists:member(ssl, Opts), lists:member(pkix, Opts)} of
	{true, false} ->
	    cert_return(transform(Cert, ssl), Opts);
	{false, true} ->
	    cert_return(transform(Cert, pkix), Opts);
	_ ->
	    {error, eoptions} 
    end.

encode_cert(#'Certificate'{} = Cert) ->
    {ok, List} = 'OTP-PKIX':encode('Certificate', Cert),
    list_to_binary(List).

decode_rsa_keyfile(KeyFile, Password) ->
    {ok, List} = ssl_pem:read_file(KeyFile, Password),
    [PrivatKey] = [Bin || {rsa_private_key, Bin} <- List],
    'OTP-PKIX':decode('RSAPrivateKey', PrivatKey).

%%====================================================================
%%  Application internal API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: encoded_tbs_cert(Cert) -> PKXCert 
%%           
%% Cert = binary() - Der encoded
%% PKXCert = binary() - Der encoded
%%      
%% Description: Extracts the binary TBSCert from the binary Certificate.
%%--------------------------------------------------------------------
encoded_tbs_cert(Cert) ->
    {ok, PKIXCert} = 
	'OTP-PKIX':decode_TBSCert_exclusive(Cert),
    {'Certificate',
     {'Certificate_tbsCertificate', EncodedTBSCert}, _, _} = PKIXCert,
    EncodedTBSCert.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

cert_return(Cert, Opts) ->
    case lists:member(subject, Opts) of
	true ->
	    {ok, get_subj(Cert)};
	false  ->
	    {ok, Cert}
    end.  


%% Transfrom from PKIX1-Explicit88 to SSL-PKIX. 

transform(#'Certificate'{signature = Signature,
			 signatureAlgorithm = SignatureAlgorithm,
			 tbsCertificate = TbsCertificate} = Cert, Type) ->
    Cert#'Certificate'{tbsCertificate = transform(TbsCertificate, Type),
		       signatureAlgorithm = transform(SignatureAlgorithm, Type),
		       signature = transform(Signature, Type)};

%% -record('TBSCertificate',{
%% version = asn1_DEFAULT, serialNumber, signature, issuer, validity, subject,
%% subjectPublicKeyInfo, issuerUniqueID = asn1_NOVALUE, 
%% subjectUniqueID = asn1_NOVALUE, extensions = asn1_NOVALUE}).

transform(#'TBSCertificate'{signature = Signature, issuer = Issuer,
			    subject = Subject, extensions = Extensions,
			    subjectPublicKeyInfo = SPKInfo} = TBSCert, Type) ->
    TBSCert#'TBSCertificate'{signature = transform(Signature, Type),
			     issuer = transform(Issuer, Type),
			     subject = transform(Subject, Type),
			     subjectPublicKeyInfo = transform(SPKInfo, Type),
			     extensions = transform_extensions(Extensions, Type)
			    };

transform(#'AlgorithmIdentifier'{algorithm = Algorithm, 
				 parameters = Params}, ssl) ->
    SignAlgAny = 
	#'SignatureAlgorithm-Any'{algorithm = Algorithm, parameters = Params},
    {ok, AnyEnc} = 'OTP-PKIX':encode('SignatureAlgorithm-Any', SignAlgAny),
    {ok, SignAlgCd} =  'OTP-PKIX':decode('SignatureAlgorithm', 
					list_to_binary(AnyEnc)),
    NAlgo = ssl_pkix_oid:id2atom(SignAlgCd#'SignatureAlgorithm'.algorithm),
    SignAlgCd#'SignatureAlgorithm'{algorithm = NAlgo};

transform({rdnSequence, Lss}, Type) when is_list(Lss) ->
    {rdnSequence, [[transform(L, Type) || L <- Ls] || Ls <- Lss]};
transform({rdnSequence, Lss}, _) ->
    {rdnSequence, Lss}; 

transform(#'AttributeTypeAndValue'{} = ATAV, ssl) ->
    {ok, ATAVEnc} = 
	'OTP-PKIX':encode('AttributeTypeAndValue', ATAV),
    {ok, ATAVDec} = 'OTP-PKIX':decode('SSLAttributeTypeAndValue', 
				      list_to_binary(ATAVEnc)),
    AttrType = ATAVDec#'SSLAttributeTypeAndValue'.type,
    #'AttributeTypeAndValue'{type = ssl_pkix_oid:id2atom(AttrType),
			     value =  
			     ATAVDec#'SSLAttributeTypeAndValue'.value};

transform(#'AttributeTypeAndValue'{} = Att, pkix) ->
    Att;

%% -record('SubjectPublicKeyInfo',{
%% algorithm, subjectPublicKey}).
%%
%% -record('SubjectPublicKeyInfo_algorithm',{
%% algo, parameters = asn1_NOVALUE}).
%%
%% -record('SubjectPublicKeyInfo-Any',{
%% algorithm, subjectPublicKey}).
%%
%% -record('PublicKeyAlgorithm',{
%% algorithm, parameters = asn1_NOVALUE}).

transform(#'SubjectPublicKeyInfo'{subjectPublicKey = SubjectPublicKey,
				  algorithm = Algorithm}, ssl)  ->
    %% Transform from SubjectPublicKeyInfo (PKIX1Explicit88) 
    %% to SubjectPublicKeyInfo-Any (SSL-PKIX). 
    Algo = Algorithm#'AlgorithmIdentifier'.algorithm,
    Parameters = Algorithm#'AlgorithmIdentifier'.parameters,
    AlgorithmAny = #'PublicKeyAlgorithm'{algorithm = Algo, 
					 parameters = Parameters},
    {0, Bin} = SubjectPublicKey,
    SInfoAny = #'SSLSubjectPublicKeyInfo-Any'{algorithm = AlgorithmAny,
					    subjectPublicKey = Bin},

    %% Encode according to SubjectPublicKeyInfo-Any, and decode according
    %% to SubjectPublicKeyInfo. 
    {ok, AnyEnc} = 
	'OTP-PKIX':encode('SSLSubjectPublicKeyInfo-Any', SInfoAny),
    {ok, SInfoCd} = 'OTP-PKIX':decode('SSLSubjectPublicKeyInfo', 
				      list_to_binary(AnyEnc)),
    %% Replace object identifier by atom
    AlgorithmCd = SInfoCd#'SSLSubjectPublicKeyInfo'.algorithm,
    AlgoCd = AlgorithmCd#'SSLSubjectPublicKeyInfo_algorithm'.algo, 
    Params = AlgorithmCd#'SSLSubjectPublicKeyInfo_algorithm'.parameters,
    Key = SInfoCd#'SSLSubjectPublicKeyInfo'.subjectPublicKey,
    NAlgoCd = ssl_pkix_oid:id2atom(AlgoCd),
    NAlgorithmCd = 
	#'SubjectPublicKeyInfo_algorithm'{algorithm = NAlgoCd, 
					  parameters = Params},
    #'SubjectPublicKeyInfo'{algorithm = NAlgorithmCd,
			    subjectPublicKey = Key
			   };
transform(#'SubjectPublicKeyInfo'{} = SInfo, pkix) ->
    SInfo;

transform(#'Extension'{extnID = ExtnID} = Ext, ssl) ->
    NewExtID = ssl_pkix_oid:id2atom(ExtnID),
    ExtAny = setelement(1, Ext, 'Extension-Any'),
    {ok, AnyEnc} = 'OTP-PKIX':encode('Extension-Any', ExtAny),
    {ok, ExtCd} =  'OTP-PKIX':decode('SSLExtension', list_to_binary(AnyEnc)),
    
    ExtValue = transform_extension_value(NewExtID, 
					 ExtCd#'SSLExtension'.extnValue,
					 ssl),
    #'Extension'{extnID = NewExtID,
		 critical = ExtCd#'SSLExtension'.critical,
		 extnValue = ExtValue};

transform(#'Extension'{extnID = ExtnID, extnValue = ExtnValue} = Ext, pkix) -> 
    NewExtID = ssl_pkix_oid:id2atom(ExtnID),
    ExtValue = transform_extension_value(NewExtID, ExtnValue, pkix),
    Ext#'Extension'{extnValue = ExtValue};

transform(#'AuthorityKeyIdentifier'{authorityCertIssuer = CertIssuer} = Ext,
	  Type) ->
    Ext#'AuthorityKeyIdentifier'{authorityCertIssuer = 
				 transform(CertIssuer, Type)};

transform([{directoryName, Value}], Type) ->
     [{directoryName, transform(Value, Type)}];

transform(X, _) ->
    X.

transform_extension_value('ce-authorityKeyIdentifier', Value, Type) ->
    transform(Value, Type);
transform_extension_value(_, Value, _) ->
    Value.

transform_extensions(Exts, Type) when is_list(Exts) ->
    [transform(Ext, Type) || Ext <- Exts];
transform_extensions(Exts, _) ->
    Exts.

get_subj(Cert) ->
    (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject.

signature_digest(BinSignature) ->
    case (catch 'OTP-PKIX':decode('DigestInfo', BinSignature)) of
	{ok, DigestInfo} ->
	    list_to_binary(DigestInfo#'DigestInfo'.digest);
	_ ->
	    {error, decode_error}
    end.
