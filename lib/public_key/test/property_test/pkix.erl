%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(pkix).

-compile(export_all).

-include_lib("common_test/include/ct_property_test.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(EMPTY_PARAM, {asn1_OPENTYPE, <<5,0>>}).

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

implicit_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, implicit_type(), {Type, implicit_value(Type)}),
            encode_decode_check(PkixType, Decoded)).
explicit_encode_decode() ->
    ?FORALL({PkixType, Decoded}, ?LET(Type, explicit_type(), {Type, explicit_value(Type)}),
           encode_decode_check(PkixType, Decoded)).
ocsp_encode_decode() ->
     ?FORALL({PkixType, Decoded}, ?LET(Type, ocsp_type(), {Type, ocsp_value(Type)}),
           encode_decode_check(PkixType, Decoded)).

encode_decode_check(PkixType, Decoded) ->
     try
         Encoded = public_key:der_encode(PkixType, Decoded),
         NewDecoded = public_key:der_decode(PkixType, Encoded),
         Decoded == NewDecoded
     catch
         _:_  ->
             false
     end.

%%--------------------------------------------------------------------
%% Generators --------------------------------------------------------
%%--------------------------------------------------------------------
implicit_type() ->
    elements(['BasicConstraints',
               'ExtKeyUsageSyntax',
               'KeyUsage',
               'KeyIdentifier']).

implicit_value('BasicConstraints') ->
    #'BasicConstraints'{cA = ?LET(CA, bool(), CA),
                        pathLenConstraint = ?LET(Len, choose(1, 10), Len)
                       };
implicit_value('ExtKeyUsageSyntax') ->
    ?LET(Usages, list(ext_key_usages()), lists:usort(Usages));
implicit_value('KeyUsage') ->
    ?LET(Usages, list(choose(0,8)),
         [key_usages_enum(Usage) || Usage <- lists:usort(Usages)]);
implicit_value('KeyIdentifier') ->
     ?LET(Bin, binary(), Bin).

ext_key_usages() ->
    elements([?'id-kp-serverAuth',
              ?'id-kp-clientAuth',
              ?'id-kp-codeSigning',
              ?'id-kp-emailProtection',
              ?'id-kp-timeStamping',
              ?'id-kp-OCSPSigning'
             ]).

key_usages_enum(0) ->
    digitalSignature;
key_usages_enum(1) ->
    nonRepudiation;
key_usages_enum(2) ->
    keyEncipherment;
key_usages_enum(3) ->
    dataEncipherment;
key_usages_enum(4) ->
    keyAgreement;
key_usages_enum(5) ->
    keyCertSign;
key_usages_enum(6) ->
    cRLSign;
key_usages_enum(7) ->
    encipherOnly;
key_usages_enum(8) ->
    decipherOnly.


explicit_type() ->
    elements(['Validity'
             ]).

explicit_value('Validity') ->
    #'Validity'{
       notBefore =
           {utcTime,"080109082930Z"},
       notAfter =
           {utcTime,"171117082930Z"}
      }.

ocsp_type() ->
    elements(['OCSPRequest'
             ]).

ocsp_value('OCSPRequest') ->
    TBSRequest = #'TBSRequest'{
              version = 0,
              requestorName = directoryName(),
              requestList = requestor_list(),
              requestExtensions = extensions()
             },
    #'OCSPRequest'{
       tbsRequest = TBSRequest,
       optionalSignature = asn1_NOVALUE
      }.

directoryName() ->
    ?LET(Ids, ?SUCHTHAT(X, list(id_attrs()), X =/= []),
         {directoryName,
          {rdnSequence, [[atter_value(Id)] || Id <- Ids]}}).

requestor_list() ->
    ?LET(HashAlgo, hash_algorithm(), requestor_list(HashAlgo)).

requestor_list({HashAlgo, Params}) ->
    [#'Request'{
        reqCert =
             #'CertID'{hashAlgorithm =
                           #'CertID_hashAlgorithm'{algorithm = HashAlgo,
                                                   parameters = Params},
                       issuerNameHash = hash(HashAlgo),
                       issuerKeyHash = hash(HashAlgo),
                       serialNumber = choose(1, 6553)},
        singleRequestExtensions = asn1_NOVALUE}].

hash_algorithm() ->
 ?LET(HashAlgo, hash_algo(), HashAlgo).

hash_algo() ->
    %% Extend to support more hashes
    elements([{?'id-sha1', ?EMPTY_PARAM}]).

hash(?'id-sha1') ->
    %% Dummy hash could be extended later to have another argument and
    %% generate real hash values for more advanced tests.
    ?LET(Value, binary(20), Value).

extensions() ->
    [#'Extension'{
        extnID = ?'id-pkix-ocsp-nonce',
        critical = false,
        extnValue = ocsp_nonce()}].

ocsp_nonce() ->
    ?LET(Len, choose(1, 128), binary(Len)).

id_attrs()->
    elements([?'id-at-surname',
              ?'id-at-givenName',
              ?'id-at-initials',
              ?'id-at-generationQualifier',
              ?'id-at-commonName',
              ?'id-at-localityName',
              ?'id-at-stateOrProvinceName',
              ?'id-at-organizationName',
              ?'id-at-title',
              ?'id-at-dnQualifier',
              ?'id-at-countryName',
              ?'id-at-serialNumber',
              ?'id-at-pseudonym',
              ?'id-domainComponent',
              ?'id-emailAddress',
              ?'id-at-organizationalUnitName'
             ]).

atter_value(?'id-at-countryName' = Name) ->
    ?LET({Capital1, Capital2}, {upper_case(), upper_case()},
         #'AttributeTypeAndValue'{

                        type = Name,
                        value = [Capital1, Capital2]
                       });
atter_value(?'id-emailAddress' = Email) ->
    End = "@example.com",
    GenLen = ?'ub-emailaddress-length' - length(End),
    ?LET(Len, choose(1, GenLen),
         #'AttributeTypeAndValue'{
            type = Email,
            value = printable_string(Len, End)
           });
atter_value(Name) when Name == ?'id-at-surname';
                       Name == ?'id-at-givenName';
                       Name == ?'id-at-initials';
                       Name == ?'id-at-organizationName';
                       Name == ?'id-at-organizationalUnitName';
                       Name == ?'id-at-title' ->
    ?LET(Size, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = {utf8String, utf8(Size)}
           });
atter_value(Name) when Name == ?'id-at-localityName';
                       Name == ?'id-at-stateOrProvinceName';
                       Name == ?'id-at-pseudonym';
                       Name == ?'id-at-generationQualifier';
                       Name == ?'id-at-commonName' ->
    ?LET(Len, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = {printableString, printable_string(Len)}
           });

atter_value(Name) when Name == ?'id-domainComponent' ->
    ?LET(Len, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = ia5_string(Len)
           });
atter_value(Name) when Name == ?'id-at-serialNumber';
                       Name == ?'id-at-dnQualifier'->
    ?LET(Num, choose(1, upper_bound(Name)),
         #'AttributeTypeAndValue'{
            type = Name,
            value = integer_to_list(Num)
           }).

upper_bound(?'id-at-surname') ->
    ?'ub-surname-length';
upper_bound(?'id-at-givenName') ->
    ?'ub-given-name-length';
upper_bound(?'id-at-initials') ->
    ?'ub-initials-length';
upper_bound(?'id-at-organizationalUnitName') ->
    ?'ub-organizational-unit-name';
upper_bound(?'id-at-organizationName') ->
    ?'ub-organization-name-length';
upper_bound(?'id-at-title') ->
    ?'ub-title';
upper_bound(?'id-at-localityName') ->
    ?'ub-locality-name';
upper_bound(?'id-at-stateOrProvinceName') ->
    ?'ub-state-name';
upper_bound(?'id-at-pseudonym') ->
    ?'ub-pseudonym';
upper_bound(?'id-at-generationQualifier') ->
    ?'ub-generation-qualifier-length';
upper_bound(?'id-at-commonName') ->
    ?'ub-common-name';
upper_bound(?'id-at-dnQualifier') ->
    ?'ub-domain-defined-attribute-value-length';
upper_bound(?'id-at-serialNumber') ->
    ?'ub-serial-number';
upper_bound(?'id-domainComponent') ->
    ?'ub-domain-name-length'.

upper_case() ->
    ?LET(ASCII, choose($A, $Z), ASCII).

lower_case() ->
    ?LET(ASCII, choose($a, $z), ASCII).

digits() ->
    ?LET(ASCII, choose($0, $9), ASCII).

misc_values() ->
    ?LET(ASCII, elements([$\r, $.,$,]), ASCII).

printable_string(Len)->
    ?LET(Value,
         ?SUCHTHAT(X, list(oneof([lower_case(), upper_case(), digits(), misc_values()])), X =/= []),
         Value).

printable_string(Len, Suffix)->
    ?LET(Value, ?SUCHTHAT(X, list(lower_case()), X =/= []), Value ++ Suffix).

ia5_string(Len)->
    ?LET(Value, ?SUCHTHAT(X, list(oneof([upper_case(), lower_case()])), X =/= []),
         Value).
