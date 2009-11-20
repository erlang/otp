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

%%

-ifndef(ssl_pkix).
-define(ssl_pkix, true).

-include("OTP-PKIX.hrl").

%% The following commented out records are currently defined in OTP-PKIX.hrl
%% and are considered a public interface through ssl_pkix.hrl.
%% NOTE do not include OTP-PKIX.hrl it is an generated file
%% and may change but the following records will still be
%% availanble from this file.  

% -record('Certificate', {
% 	  tbsCertificate, 
% 	  signatureAlgorithm, 
% 	  signature}).

% -record('TBSCertificate', {
% 	  version = asn1_DEFAULT, 
% 	  serialNumber, 
% 	  signature, 
% 	  issuer, 
% 	  validity, 
% 	  subject, 
% 	  subjectPublicKeyInfo, 
% 	  issuerUniqueID = asn1_NOVALUE, 
% 	  subjectUniqueID = asn1_NOVALUE, 
% 	  extensions = asn1_NOVALUE}).

% -record('AttributeTypeAndValue', {
% 	  type, 
% 	  value}).

% -record('SubjectPublicKeyInfo', {
% 	  algorithm, 
% 	  subjectPublicKey}).

-record('SubjectPublicKeyInfo_algorithm', {
 	  algorithm, 
 	  parameters = asn1_NOVALUE}).

% -record('FieldID', {
% 	  fieldType,
% 	  parameters}).

% -record('Characteristic-two', {
% 	  m, 
% 	  basis, 
% 	  parameters}).

% -record('ExtensionAttribute', {
% 	  extensionAttributeType, 
% 	  extensionAttributeValue}).

% -record('Extension', {
% 	  extnID, 
% 	  critical = asn1_DEFAULT, 
% 	  extnValue}).

-endif. % -ifdef(ssl_pkix).

