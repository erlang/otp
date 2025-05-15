%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-ifndef(public_key_internal).
-define(public_key_internal, true).

-include("AlgorithmInformation-2009.hrl").
-include("DSS.hrl").
-include("ECPrivateKey.hrl").

-include("OCSP-2024-08.hrl").
-undef('id-kp-OCSPSigning').

-include("OTP-PKIX.hrl").
-include("PKCS-1.hrl").

%%  Bug in ASN.1 compiler  (hardcode the correct value)
-undef('rSASSA-PSS-Default-Identifier').
-define('rSASSA-PSS-Default-Identifier',
        {'RSASSA-AlgorithmIdentifier',{1,2,840,113549,1,1,10},
         {'RSASSA-PSS-params',{'HashAlgorithm',{1,3,14,3,2,26},'NULL'},
          {'MaskGenAlgorithm',{1,2,840,113549,1,1,8},
           {'HashAlgorithm',{1,3,14,3,2,26},'NULL'}},20,1}}).


-include("PKCS-3.hrl").
-include("PKIX-CommonTypes-2009.hrl").
-include("PKIX1Explicit-2009.hrl").

-include("PKIX1Implicit-2009.hrl").

-undef('id-md2').
-undef('id-md5').
-undef('id-sha1').
-undef('rsaEncryption').
-undef('md2WithRSAEncryption').
-undef('md5WithRSAEncryption').
-undef('sha1WithRSAEncryption').
-include("PKIXAlgs-2009.hrl").

-include("Safecurves-pkix-18.hrl").

-include("RFC5639.hrl").


-define(DEFAULT_VERIFYFUN,
	{fun(_,{bad_cert, _} = Reason, _) ->
		 {fail, Reason};
	    (_,{extension, _}, UserState) ->
		 {unknown, UserState};
	    (_, valid, UserState) ->
		 {valid, UserState};
	    (_, valid_peer, UserState) ->
		 {valid, UserState}
	 end, []}).

-record(path_validation_state,
        {
         valid_policy_tree,
         user_initial_policy_set,
         explicit_policy,
         inhibit_any_policy,
         inhibit_policy_mapping,
         policy_mapping_ext,
         policy_constraint_ext,
         policy_inhibitany_ext,
         policy_ext_present,
         policy_ext_any,
         current_any_policy_qualifiers,
         cert_num,
         last_cert = false,
         permitted_subtrees = no_constraints, %% Name constraints
         excluded_subtrees = [],      %% Name constraints
         working_public_key_algorithm,
         working_public_key,
         working_public_key_parameters,
         working_issuer_name,
         max_path_length,
         verify_fun,
         user_state
        }).

-record(revoke_state,
        {
         reasons_mask,
         cert_status,
         interim_reasons_mask,
         valid_ext,
         details
        }).

-record('ECPoint',
        {
         point
        }).

-record(cert,
        {
         der :: public_key:der_encoded(),
         otp :: #'OTPCertificate'{}
        }).

-define(unspecified, 0).
-define(keyCompromise, 1).
-define(cACompromise, 2).
-define(affiliationChanged, 3).
-define(superseded, 4).
-define(cessationOfOperation, 5).
-define(certificateHold, 6).
-define(removeFromCRL, 8).
-define(privilegeWithdrawn, 9).
-define(aACompromise, 10).

-define('anyPolicy', {2,5,29,32,0}).

-record('ECParameters',
        {
         version,
         fieldID,
         curve,
         base,
         order,
         cofactor = asn1_NOVALUE
        }).

-record('Curve',
        {
         a,
         b,
         seed = asn1_NOVALUE
        }).

-record('FieldID',
        {
         fieldType,
         parameters
        }).

-record('Dss-Parms',
        {
         p,         % pos_integer()
         q,         % pos_integer()
         g          % pos_integer()
        }).

-record('SignatureAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PublicKeyAlgorithm',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

%% Superseded by SingleAttribute.
-record('AttributeTypeAndValue',
        {
         type,
         value
        }).

-record('PBEParameter',
        {
         salt,
         iterationCount
        }).

-record('PBES2-params',
        {
         keyDerivationFunc,
         encryptionScheme
        }).


-record('PrivateKeyInfo',
        {
         version,
         privateKeyAlgorithm,
         privateKey,
         attributes = asn1_NOVALUE,
         %% with extensions
         publicKey = asn1_NOVALUE
         %% end of extensions
        }).

-record('OneAsymmetricKey',
        {
         version,
         privateKeyAlgorithm,
         privateKey,
         attributes = asn1_NOVALUE,
         %% with extensions
         publicKey = asn1_NOVALUE
         %% end of extensions
        }).

-record('PrivateKeyAlgorithmIdentifier',
        {
         algorithm,
         parameters = asn1_NOVALUE
        }).

-record('PrivateKeyInfo_privateKeyAlgorithm',
       {
        algorithm,
        parameters = asn1_NOVALUE
       }).

-record('OTPSubjectPublicKeyInfo',
        {
         algorithm,       % #'PublicKeyAlgorithm'{}
         subjectPublicKey % binary()
        }).

-record('AnotherName',
        {
         'type-id',
         value
        }).

%% Object identifiers not present in modern specs.

-define('characteristic-two-field', {1,2,840,10045,1,2}).
-define('prime-field', {1,2,840,10045,1,1}).

-define('secp224k1', {1,3,132,0,32}).
-define('secp192k1', {1,3,132,0,31}).
-define('secp160r2', {1,3,132,0,30}).
-define('secp128r2', {1,3,132,0,29}).
-define('secp128r1', {1,3,132,0,28}).
-define('sect193r2', {1,3,132,0,25}).
-define('sect193r1', {1,3,132,0,24}).
-define('sect131r2', {1,3,132,0,23}).
-define('sect131r1', {1,3,132,0,22}).
-define('secp256k1', {1,3,132,0,10}).
-define('secp160k1', {1,3,132,0,9}).
-define('secp160r1', {1,3,132,0,8}).
-define('secp112r2', {1,3,132,0,7}).
-define('secp112r1', {1,3,132,0,6}).
-define('sect113r2', {1,3,132,0,5}).
-define('sect113r1', {1,3,132,0,4}).
-define('sect239k1', {1,3,132,0,3}).
-define('sect163r1', {1,3,132,0,2}).

-define('id-dsa-with-sha1', {1,2,840,10040,4,3}).

-define('ppBasis', {1,2,840,10045,1,2,3,3}).
-define('tpBasis', {1,2,840,10045,1,2,3,2}).
-define('gnBasis', {1,2,840,10045,1,2,3,1}).

-endif. % -ifdef(public_key_internal).
