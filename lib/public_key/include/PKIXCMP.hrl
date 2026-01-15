%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-ifndef(_PKIXCMP_HRL_).
-define(_PKIXCMP_HRL_, true).

-record('PKIMessage', {
  header,
  body,
  protection = asn1_NOVALUE,
  extraCerts = asn1_NOVALUE
}).

-record('PKIHeader', {
  pvno,
  sender,
  recipient,
  messageTime = asn1_NOVALUE,
  protectionAlg = asn1_NOVALUE,
  senderKID = asn1_NOVALUE,
  recipKID = asn1_NOVALUE,
  transactionID = asn1_NOVALUE,
  senderNonce = asn1_NOVALUE,
  recipNonce = asn1_NOVALUE,
  freeText = asn1_NOVALUE,
  generalInfo = asn1_NOVALUE
}).

-record('PKIHeader_protectionAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('ProtectedPart', {
  header,
  body
}).

-ifndef(_PBMParameter_).
-define(_PBMParameter_, true).

-record('PBMParameter', {
  salt,
  owf,
  iterationCount,
  mac
}).

-record('PBMParameter_owf', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('PBMParameter_mac', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-endif.  %% _PBMParameter_

-record('DHBMParameter', {
  owf,
  mac
}).

-record('DHBMParameter_owf', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('DHBMParameter_mac', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('KemBMParameter', {
  kdf,
  kemContext = asn1_NOVALUE,
  len,
  mac
}).

-record('KemBMParameter_kdf', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('KemBMParameter_mac', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('PKIStatusInfo', {
  status,
  statusString = asn1_NOVALUE,
  failInfo = asn1_NOVALUE
}).

-record('OOBCertHash', {
  hashAlg = asn1_NOVALUE,
  certId = asn1_NOVALUE,
  hashVal
}).

-record('OOBCertHash_hashAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('Challenge', {
  owf = asn1_NOVALUE,
  witness,
  challenge,
  encryptedRand = asn1_NOVALUE
}).

-record('Challenge_owf', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('Rand', {
  int,
  sender
}).

-record('CertRepMessage', {
  caPubs = asn1_NOVALUE,
  response
}).

-record('CertResponse', {
  certReqId,
  status,
  certifiedKeyPair = asn1_NOVALUE,
  rspInfo = asn1_NOVALUE
}).

-record('CertifiedKeyPair', {
  certOrEncCert,
  privateKey = asn1_NOVALUE,
  publicationInfo = asn1_NOVALUE
}).

-record('KeyRecRepContent', {
  status,
  newSigCert = asn1_NOVALUE,
  caCerts = asn1_NOVALUE,
  keyPairHist = asn1_NOVALUE
}).

-record('RevDetails', {
  certDetails,
  crlEntryDetails = asn1_NOVALUE
}).

-record('RevDetails_crlEntryDetails_SEQOF', {
  extnID,
  critical = asn1_DEFAULT,
  extnValue
}).

-record('RevRepContent', {
  status,
  revCerts = asn1_NOVALUE,
  crls = asn1_NOVALUE
}).

-record('CAKeyUpdAnnContent', {
  oldWithNew,
  newWithOld,
  newWithNew
}).

-record('RevAnnContent', {
  status,
  certId,
  willBeRevokedAt,
  badSinceDate,
  crlDetails = asn1_NOVALUE
}).

-record('RevAnnContent_crlDetails_SEQOF', {
  extnID,
  critical = asn1_DEFAULT,
  extnValue
}).

-record('CertReqTemplateContent', {
  certTemplate,
  keySpec = asn1_NOVALUE
}).

%% Include public_key.hrl for this
%%
%% -record('AttributeTypeAndValue', {
%%   type,
%%   value
%% }).

-record('AlgIdCtrl', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('RootCaKeyUpdateContent', {
  newWithNew,
  newWithOld = asn1_NOVALUE,
  oldWithNew = asn1_NOVALUE
}).

-record('CRLStatus', {
  source,
  thisUpdate = asn1_NOVALUE
}).

-record('KemCiphertextInfo', {
  kem,
  ct
}).

-record('KemCiphertextInfo_kem', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('KemOtherInfo', {
  staticString,
  transactionID,
  kemContext = asn1_NOVALUE
}).

-record('InfoTypeAndValue', {
  infoType,
  infoValue
}).

-record('ErrorMsgContent', {
  pKIStatusInfo,
  errorCode = asn1_NOVALUE,
  errorDetails = asn1_NOVALUE
}).

-record('CertStatus', {
  certHash,
  certReqId,
  statusInfo = asn1_NOVALUE,
  hashAlg = asn1_NOVALUE
}).

-record('CertStatus_hashAlg', {
  algorithm,
  parameters = asn1_NOVALUE
}).

-record('PollReqContent_SEQOF', {
  certReqId
}).

-record('PollRepContent_SEQOF', {
  certReqId,
  checkAfter,
  reason = asn1_NOVALUE
}).

-define('id-PasswordBasedMac', {1,2,840,113533,7,66,13}).
-define('id-DHBasedMac', {1,2,840,113533,7,66,30}).
-define('id-KemBasedMac', {1,2,840,113533,7,66,16}).
-define('id-regCtrl-altCertTemplate', {1,3,6,1,5,5,7,5,1,7}).
-define('id-regCtrl-algId', {1,3,6,1,5,5,7,5,1,11}).
-define('id-regCtrl-rsaKeyLen', {1,3,6,1,5,5,7,5,1,12}).
-define('id-kp-cmKGA', {1,3,6,1,5,5,7,3,32}).
-endif. %% _PKIXCMP_HRL_
