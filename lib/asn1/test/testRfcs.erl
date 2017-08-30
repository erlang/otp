%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

-module(testRfcs).

-export([compile/3,test/0]).

-include_lib("common_test/include/ct.hrl").

compile(Config, Erules, Options0) ->
    Options = [no_ok_wrapper|Options0],
    DataDir = proplists:get_value(data_dir, Config),
    Specs0 = filelib:wildcard("*.asn1", filename:join(DataDir, rfcs)),
    Specs = [filename:join(rfcs, Spec) || Spec <- Specs0],
    122 = length(Specs),
    CaseDir = proplists:get_value(case_dir, Config),
    asn1_test_lib:compile_all(Specs, Config, [Erules,{i,CaseDir}|Options]).

test() ->
    {1,3,6,1,5,5,7,48,1,2} =
	IdPkixOcspNonce =
	'OCSP-2009':'id-pkix-ocsp-nonce'(),
    roundtrip('OCSP-2009', 'OCSPRequest',
	      {'OCSPRequest',
	       {'TBSRequest',
		0,
		{rfc822Name,"name string"},
		[{'Request',
		  {'CertID',{'_',{2,9,3,4,5},asn1_NOVALUE},
		   <<"POTATOHASH">>,<<"HASHBROWN">>,42},
		  [{'_',IdPkixOcspNonce,true,<<34,159,16,57,199>>}]}],
		asn1_NOVALUE},
	       asn1_NOVALUE}),
    otp_7759(),
    ok.

roundtrip(Module, Type, Value0) ->
    Enc = Module:encode(Type, Value0),
    Value1 = Module:decode(Type, Enc),
    asn1_test_lib:match_value(Value0, Value1),
    ok.

otp_7759() ->
    %% The release note for asn-1.6.6 says:
    %%   Decode of an open_type when the value was empty tagged
    %%   type encoded with indefinite length failed.
    Mod = 'OLD-PKCS7',
    Encoded = encoded_msg(),
    ContentInfo = Mod:decode('ContentInfo', Encoded),
    io:format("~p\n", [ContentInfo]),
    {'ContentInfo',_Id,PKCS7_content} = ContentInfo,
    X = Mod:decode('SignedData', PKCS7_content),
    io:format("~p\n", [X]),
    io:nl(),
    ok.

encoded_msg() ->
    <<48,128,6,9,42,134,72,134,247,13,1,7,2,160,128,48,128,2,1,1,49,11,48,9,6,5,43,14,3,2,26,5,0,48,128,6,9,42,134,72,134,247,13,1,7,1,160,128,36,128,0,0,0,0,0,0,  49,130,1,192,48,130,1,188,2,1,1,48,50,48,38,49,17,48,15,6,3,85,4,3,12,8,65,100,109,105,110,67,65,49,49,17,48,15,6,3,85,4,10,12,8,69,82,73,67,83,83,79,78,2,8,15,151,245,186,21,23,240,96,48,9,6,5,43,14,3,2,26,5,0,160,129,229,48,17,6,10,96,134,72,1,134,248,69,1,9,2,49,3,19,1,51,48,17,6,10,96,134,72,1,134,248,69,1,9,3,49,3,19,1,51,48,24,6,9,42,134,72,134,247,13,1,9,3,49,11,6,9,42,134,72,134,247,13,1,7,1,48,28,6,9,42,134,72,134,247,13,1,9,5,49,15,23,13,48,56,49,50,49,48,48,57,53,52,50,51,90,48,28,6,10,96,134,72,1,134,248,69,1,9,7,49,14,19,12,49,53,50,56,49,52,50,52,48,57,53,53,48,32,6,10,96,134,72,1,134,248,69,1,9,5,49,18,4,16,165,115,177,71,78,88,239,113,78,56,98,98,18,202,217,235,48,32,6,10,96,134,72,1,134,248,69,1,9,6,49,18,4,16,227,174,230,251,43,153,252,65,11,93,231,83,34,18,55,46,48,35,6,9,42,134,72,134,247,13,1,9,4,49,22,4,20,218,57,163,238,94,107,75,13,50,85,191,239,149,96,24,144,175,216,7,9,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,4,129,128,106,233,116,125,140,51,133,173,63,41,54,138,214,211,89,215,169,125,98,77,16,222,216,240,211,79,125,111,87,186,73,63,253,204,107,102,177,63,174,197,224,212,231,172,149,246,33,68,223,67,102,93,64,152,152,5,216,102,247,134,36,197,150,236,57,77,56,138,95,71,204,31,23,149,241,213,78,172,165,249,100,187,12,45,19,57,67,120,54,63,15,239,41,217,127,61,254,60,201,104,68,3,135,214,206,93,253,255,192,94,56,107,68,210,57,61,41,249,47,156,130,244,52,12,163,216,236,69,0,0,0,0,0,0>>.
