%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
%%-------------------------------------------------------------------

-module(testX420).

-export([compile/3, ticket7759/2]).

-include_lib("test_server/include/test_server.hrl").


compile(Erule, Options, Config) ->
    Specs0 = specs(),
    99 = length(Specs0),
    CaseDir = ?config(case_dir, Config),
    Specs = [filename:join(x420, Spec) || Spec <- Specs0],
    asn1_test_lib:compile_all(Specs, Config, [Erule,{i,CaseDir}|Options]).

specs() ->
    ["ACSE-1", "AuthenticationFramework", "BasicAccessControl",
     "CertificateExtensions", "Character-Coding-Attributes",
     "Character-Presentation-Attributes", "Character-Profile-Attributes",
     "Colour-Attributes", "DOR-definition", "DSAOperationalAttributeTypes",
     "Default-Value-Lists", "DirectoryAbstractService",
     "DirectoryAccessProtocol", "DirectoryInformationShadowProtocol",
     "DirectoryOperationalBindingManagementProtocol",
     "DirectoryOperationalBindingTypes", "DirectoryProtectionMappings",
     "DirectoryShadowAbstractService", "DirectorySystemProtocol",
     "DistributedOperations", "Document-Profile-Descriptor",
     "EnhancedSecurity", "External-References", "GULSProtectionMappings",
     "GenericProtectingTransferSyntax", "Geo-Gr-Coding-Attributes",
     "Geo-Gr-Presentation-Attributes", "Geo-Gr-Profile-Attributes",
     "GulsSecurityExchanges", "GulsSecurityTransformations",
     "HierarchicalOperationalBindings", "IPMSAbstractService",
     "IPMSAutoActionTypes", "IPMSExtendedBodyPartTypes",
     "IPMSExtendedBodyPartTypes2", "IPMSExtendedVoiceBodyPartType",
     "IPMSFileTransferBodyPartType", "IPMSForwardedContentBodyPartType",
     "IPMSForwardedReportBodyPartType", "IPMSFunctionalObjects",
     "IPMSHeadingExtensions", "IPMSInformationObjects",
     "IPMSMessageStoreAttributes", "IPMSObjectIdentifiers",
     "IPMSObjectIdentifiers2", "IPMSSecurityExtensions", "IPMSUpperBounds",
     "ISO-STANDARD-9541-FONT-ATTRIBUTE-SET", "ISO8571-FTAM", "ISO9541-SN",
     "Identifiers-and-Expressions", "InformationFramework",
     "Interchange-Data-Elements", "Layout-Descriptors", "Link-Descriptors",
     "Location-Expressions", "Logical-Descriptors", "MHSObjectIdentifiers",
     "MHSProtocolObjectIdentifiers", "MSAbstractService",
     "MSAccessProtocol", "MSGeneralAttributeTypes",
     "MSGeneralAutoActionTypes", "MSMatchingRules", "MSObjectIdentifiers",
     "MSUpperBounds", "MTAAbstractService", "MTSAbstractService",
     "MTSAbstractService88", "MTSAccessProtocol", "MTSObjectIdentifiers",
     "MTSUpperBounds", "Notation", "ObjectIdentifiers",
     "OperationalBindingManagement", "PKCS7", "PKCS7BodyPartType",
     "Protected-Part-Descriptors", "ProtocolObjectIdentifiers",
     "Raster-Gr-Coding-Attributes", "Raster-Gr-Presentation-Attributes",
     "Raster-Gr-Profile-Attributes", "Reliable-Transfer-APDU",
     "Remote-Operations-Abstract-Syntaxes",
     "Remote-Operations-Generic-ROS-PDUs",
     "Remote-Operations-Information-Objects-extensions",
     "Remote-Operations-Information-Objects",
     "Remote-Operations-Realizations",
     "Remote-Operations-Useful-Definitions", "SelectedAttributeTypes",
     "SeseAPDUs", "SpkmGssTokens", "Style-Descriptors", "Subprofiles",
     "Temporal-Relationships", "Text-Units", "UpperBounds",
     "UsefulDefinitions", "Videotex-Coding-Attributes"].

ticket7759(_Erule,_Config) ->
    Encoded = encoded_msg(),
    io:format("Testing ticket7759 ...~n",[]),
    {ok, ContentInfo} = 'PKCS7':decode('ContentInfo',Encoded),
    {'ContentInfo',_Id,PKCS7_content} = ContentInfo,
    {ok,_} = 'PKCS7':decode('SignedData',PKCS7_content),
    ok.


encoded_msg() ->
    <<48,128,6,9,42,134,72,134,247,13,1,7,2,160,128,48,128,2,1,1,49,11,48,9,6,5,43,14,3,2,26,5,0,48,128,6,9,42,134,72,134,247,13,1,7,1,160,128,36,128,0,0,0,0,0,0,  49,130,1,192,48,130,1,188,2,1,1,48,50,48,38,49,17,48,15,6,3,85,4,3,12,8,65,100,109,105,110,67,65,49,49,17,48,15,6,3,85,4,10,12,8,69,82,73,67,83,83,79,78,2,8,15,151,245,186,21,23,240,96,48,9,6,5,43,14,3,2,26,5,0,160,129,229,48,17,6,10,96,134,72,1,134,248,69,1,9,2,49,3,19,1,51,48,17,6,10,96,134,72,1,134,248,69,1,9,3,49,3,19,1,51,48,24,6,9,42,134,72,134,247,13,1,9,3,49,11,6,9,42,134,72,134,247,13,1,7,1,48,28,6,9,42,134,72,134,247,13,1,9,5,49,15,23,13,48,56,49,50,49,48,48,57,53,52,50,51,90,48,28,6,10,96,134,72,1,134,248,69,1,9,7,49,14,19,12,49,53,50,56,49,52,50,52,48,57,53,53,48,32,6,10,96,134,72,1,134,248,69,1,9,5,49,18,4,16,165,115,177,71,78,88,239,113,78,56,98,98,18,202,217,235,48,32,6,10,96,134,72,1,134,248,69,1,9,6,49,18,4,16,227,174,230,251,43,153,252,65,11,93,231,83,34,18,55,46,48,35,6,9,42,134,72,134,247,13,1,9,4,49,22,4,20,218,57,163,238,94,107,75,13,50,85,191,239,149,96,24,144,175,216,7,9,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,4,129,128,106,233,116,125,140,51,133,173,63,41,54,138,214,211,89,215,169,125,98,77,16,222,216,240,211,79,125,111,87,186,73,63,253,204,107,102,177,63,174,197,224,212,231,172,149,246,33,68,223,67,102,93,64,152,152,5,216,102,247,134,36,197,150,236,57,77,56,138,95,71,204,31,23,149,241,213,78,172,165,249,100,187,12,45,19,57,67,120,54,63,15,239,41,217,127,61,254,60,201,104,68,3,135,214,206,93,253,255,192,94,56,107,68,210,57,61,41,249,47,156,130,244,52,12,163,216,236,69,0,0,0,0,0,0>>.
