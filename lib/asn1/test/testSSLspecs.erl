%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(testSSLspecs).

-export([compile/2,run/1,compile_combined/2,run_combined/1]).

-include_lib("common_test/include/ct.hrl").

compile(Config, Options) ->
    DataDir = proplists:get_value(data_dir, Config),
    CaseDir = proplists:get_value(case_dir, Config),
    NewOptions = [{i, DataDir}, {i, CaseDir}|Options],

    asn1_test_lib:compile_all(["SSL-PKIX", "PKIXAttributeCertificate"],
                              Config, NewOptions),

    %% test case for OTP-4792 optional open type
    asn1_test_lib:compile_all(["PKIX1Algorithms88", "PKIX1Explicit88",
                               "PKIX1Implicit88"],
                              Config, NewOptions),

    %% OTP-6698, OTP-6702
    ok = remove_db_files(CaseDir),
    asn1_test_lib:compile_all(["PKIX1Explicit93", "PKIX1Implicit93"],
                              Config, NewOptions).

compile_combined(Config, ber=Rule) ->
    DataDir = proplists:get_value(data_dir, Config),
    CaseDir = proplists:get_value(case_dir, Config),
    Options = [{i, CaseDir}, {i, DataDir}, Rule,
               der, compact_bit_string, asn1config],
    ok = remove_db_files_combined(CaseDir),
    asn1_test_lib:compile("OTP-PKIX.set.asn", Config, Options).

remove_db_files(Dir) ->
    ok = remove_db_file(Dir ++ "PKIX1Explicit93.asn1db"),
    ok = remove_db_file(Dir ++ "PKIX1Implicit93.asn1db").
remove_db_file(File) ->
    case file:delete(File) of
	ok ->
	    ok;
	{error,enoent} ->
	    ok;
	Err ->
	    Err
    end.

remove_db_files_combined(Dir) ->
    ok = remove_db_file(Dir ++ "OTP-PKIX.asn1db"),
    ok = remove_db_file(Dir ++ "SSL-PKIX.asn1db"),
    ok = remove_db_file(Dir ++ "PKIXAttributeCertificate.asn1db"),
    ok = remove_db_file(Dir ++ "PKIX1Algorithms88.asn1db"),
    ok = remove_db_file(Dir ++ "PKIX1Explicit88.asn1db"),
    ok = remove_db_file(Dir ++ "PKIX1Implicit88.asn1db").

run(ber) ->
    run1(1).

run1(6) ->
    f1(6),
    f2(6),
    transform4(ex(7));
run1(N) ->
    f1(N),
    f2(N),
    run1(N+1).


f1(N) ->
    transform1(ex(N)).

f2(N) ->
    transform2(ex(N)).


transform1(ATAV) ->
    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('AttributeTypeAndValue',
ATAV),
    {ok, _ATAVDec} = 'SSL-PKIX':decode('AttributeTypeAndValue',
                                      ATAVEnc).

transform2(ATAV) ->
    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('AttributeTypeAndValue',
ATAV),
    {ok, _ATAVDec} = 'PKIX1Explicit88':decode('AttributeTypeAndValue',
                                             ATAVEnc).


transform4(ATAV) ->
    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('Attribute',
ATAV),
    {ok, _ATAVDec} = 'PKIX1Explicit88':decode('Attribute',
                                             ATAVEnc).


ex(1) ->
    {'AttributeTypeAndValue',
     {2,5,4,3},
     <<19,5,111,116,112,67,65>>};
ex(2) ->
    {'AttributeTypeAndValue',
     {2,5,4,11},
     <<19,10,69,114,108,97,110,103,32,79,84,80>>};
ex(3) ->
    {'AttributeTypeAndValue',
     {2,5,4,10},
     <<19,11,69,114,105,99,115,115,111,110,32,65,66>>};
ex(4) ->
    {'AttributeTypeAndValue',
     {2,5,4,6},
     <<19,2,83,69>>};
ex(5) ->
    {'AttributeTypeAndValue',
     {2,5,4,7},
     <<19,9,83,116,111,99,107,104,111,108,109>>};
ex(6) ->
    {'AttributeTypeAndValue',
     {1,2,840,113549,1,9,1},
     <<22,22,112,101,116,101,114,64,101,114,105,120,
      46,101,114,105,99,115,115,111,110,46,115,101>>};
ex(7) ->
    {'Attribute',
     {1,2,840,113549,1,9,1},
     [[19,5,111,116,112,67,65]]}.

run_combined(ber) ->
    Cert = cert(),
    {ok,{'CertificatePKIX1Explicit88',{Type,UnDec},_,_}} = 'OTP-PKIX':decode_TBSCert_exclusive(Cert),
    {ok,_} = 'OTP-PKIX':decode_part(Type,UnDec),
    ok.

cert() ->
    <<48,130,3,200,48,130,3,49,160,3,2,1,2,2,1,1,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,48,129,134,49,17,48,15,6,3,85,4,3,19,8,101,114,108,97,110,103,67,65,49,19,48,17,6,3,85,4,11,19,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,19,11,69,114,105,99,115,115,111,110,32,65,66,49,18,48,16,6,3,85,4,7,19,9,83,116,111,99,107,104,111,108,109,49,11,48,9,6,3,85,4,6,19,2,83,69,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,30,23,13,48,55,48,53,48,57,49,50,51,52,48,57,90,23,13,49,55,48,51,49,55,49,50,51,52,48,57,90,48,129,131,49,14,48,12,6,3,85,4,3,19,5,111,116,112,67,65,49,19,48,17,6,3,85,4,11,19,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,19,11,69,114,105,99,115,115,111,110,32,65,66,49,11,48,9,6,3,85,4,6,19,2,83,69,49,18,48,16,6,3,85,4,7,19,9,83,116,111,99,107,104,111,108,109,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,129,159,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,3,129,141,0,48,129,137,2,129,129,0,157,223,214,78,61,218,253,253,143,253,54,171,133,60,170,135,118,35,37,238,208,160,209,6,115,228,99,139,202,32,226,243,181,251,216,20,154,56,18,225,158,17,46,210,88,80,0,60,121,125,159,116,98,206,192,243,95,213,5,117,140,217,211,69,105,70,208,53,132,33,120,233,97,86,136,39,245,36,220,204,187,171,125,193,169,250,123,3,63,220,240,216,136,151,176,255,7,53,55,204,234,104,24,222,232,46,29,88,7,239,183,95,210,202,244,125,106,169,225,181,128,13,22,216,15,150,203,147,2,3,1,0,1,163,130,1,69,48,130,1,65,48,15,6,3,85,29,19,1,1,255,4,5,48,3,1,1,255,48,11,6,3,85,29,15,4,4,3,2,1,6,48,29,6,3,85,29,14,4,22,4,20,59,42,225,69,198,245,160,224,205,23,100,154,109,139,92,188,255,177,162,7,48,129,187,6,3,85,29,35,4,129,179,48,129,176,128,20,138,222,66,214,242,39,254,232,217,84,224,143,206,167,0,167,118,166,219,135,161,129,140,164,129,137,48,129,134,49,17,48,15,6,3,85,4,3,19,8,101,114,108,97,110,103,67,65,49,19,48,17,6,3,85,4,11,19,10,69,114,108,97,110,103,32,79,84,80,49,20,48,18,6,3,85,4,10,19,11,69,114,105,99,115,115,111,110,32,65,66,49,18,48,16,6,3,85,4,7,19,9,83,116,111,99,107,104,111,108,109,49,11,48,9,6,3,85,4,6,19,2,83,69,49,37,48,35,6,9,42,134,72,134,247,13,1,9,1,22,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,130,9,0,217,241,90,207,3,129,188,250,48,33,6,3,85,29,17,4,26,48,24,129,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,33,6,3,85,29,18,4,26,48,24,129,22,112,101,116,101,114,64,101,114,105,120,46,101,114,105,99,115,115,111,110,46,115,101,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,3,129,129,0,0,193,166,44,175,85,39,236,34,138,64,208,1,244,50,147,73,0,236,172,244,6,127,14,70,113,79,176,212,170,65,159,232,153,234,253,158,17,92,249,196,102,83,116,186,67,168,95,132,65,228,190,78,11,64,75,215,73,156,223,193,46,166,11,227,106,39,133,232,175,223,75,14,232,178,210,122,70,245,18,164,65,151,44,129,3,10,20,92,251,69,230,208,30,203,65,238,140,95,48,130,202,173,28,84,253,42,18,56,47,248,152,156,171,214,37,41,155,205,230,251,98,118,164,239,246,216,66>>.
