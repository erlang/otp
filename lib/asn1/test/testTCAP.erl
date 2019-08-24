%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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
-module(testTCAP).

-export([compile/2,test/2,compile_asn1config/2,test_asn1config/0]).

-include_lib("common_test/include/ct.hrl").

compile(Config, Options) ->
    Files = ["Remote-Operations-Information-Objects",
             "TCAPMessages",
             "TCAPMessages-simple",
             "TCAPPackage"],
    asn1_test_lib:compile_all(Files, Config, Options),
    asn1_test_lib:compile_erlang("TCAPPackage_msg", Config, []).

compile_asn1config(Config, Options) ->
    Files = ["Remote-Operations-Information-Objects",
             "TCAPPackage"],
    asn1_test_lib:compile_all(Files, Config, Options),
    asn1_test_lib:compile_erlang("TCAPPackage_msg", Config, []).

test(Erule,_Config) ->
    %% testing OTP-4798, open type encoded with indefinite length
    {ok,_Res} = 'TCAPMessages-simple':decode('MessageType',
					     val_OTP_4798(Erule)),

    %% testing OTP-4799, absent optional open type
    {ok,_Res2} = 'TCAPMessages-simple':decode('MessageType',
					      val_OTP_4799(Erule)),

    %% testing vance shipley's problems. Parameterized object sets.
    Val3 = 'TCAPPackage_msg':val('PackageType',unidirectional),
    Res3 = enc_dec('PackageType', Val3),
    ok = 'TCAPPackage_msg':check_result('PackageType',unidirectional,Res3),
    
    Val4 = 'TCAPPackage_msg':val('PackageType',abort),
    Res4 = enc_dec('PackageType', Val4),
    ok = 'TCAPPackage_msg':check_result('PackageType',abort,Res4),

    Val5 = 'TCAPPackage_msg':val('PackageType',response),
    Res5 = enc_dec('PackageType', Val5),
    ok = 'TCAPPackage_msg':check_result('PackageType',response,Res5).

val_OTP_4798(ber) ->
    [100,129,176,73,4,57,3,17,80,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,14,2,162,3,2,1,0,163,5,161,3,2,1,0,108,128,162,120,2,1,0,48,115,2,1,56,48,128,48,34,4,16,203,87,215,196,217,93,235,90,64,131,106,145,39,26,25,236,4,4,197,241,81,112,4,8,78,225,34,196,215,212,200,0,48,34,4,16,145,125,27,67,42,144,6,161,207,112,55,75,200,191,191,28,4,4,226,219,242,123,4,8,72,46,130,28,206,178,168,0,48,34,4,16,1,8,20,29,70,160,218,160,125,188,244,174,113,115,253,245,4,4,26,5,90,160,4,8,252,75,149,98,153,224,140,0,0,0,0,0];
val_OTP_4798(_) ->
    <<100,129,176,73,4,57,3,17,80,107,42,40,40,6,7,0,17,134,5,1,1,1,160,29,97,27,128,2,7,128,161,9,6,7,4,0,0,1,0,14,2,162,3,2,1,0,163,5,161,3,2,1,0,108,128,162,120,2,1,0,48,115,2,1,56,48,128,48,34,4,16,203,87,215,196,217,93,235,90,64,131,106,145,39,26,25,236,4,4,197,241,81,112,4,8,78,225,34,196,215,212,200,0,48,34,4,16,145,125,27,67,42,144,6,161,207,112,55,75,200,191,191,28,4,4,226,219,242,123,4,8,72,46,130,28,206,178,168,0,48,34,4,16,1,8,20,29,70,160,218,160,125,188,244,174,113,115,253,245,4,4,26,5,90,160,4,8,252,75,149,98,153,224,140,0,0,0,0,0>>.

val_OTP_4799(ber) ->
    [100,16,73,4,41,182,36,0,108,8,163,6,2,1,29,2,1,27];
val_OTP_4799(_) ->
    <<100,16,73,4,41,182,36,0,108,8,163,6,2,1,29,2,1,27>>.

test_asn1config() ->
    Val = 'TCAPPackage_msg':val('PackageType', queryWithPerm),
    {ok,B} = 'TCAPPackage':encode('PackageType', Val),
    {ok,ExMsg}='TCAPPackage':decode_PackageType(B),
    {_,{_,_,_,{Key,ExVal}}} = ExMsg,
    {ok,_Parts} = 'TCAPPackage':decode_part(Key, ExVal),
    
    Val2 = 'TCAPPackage_msg':val('TransactionPDU'),
    {ok,B2} = 'TCAPPackage':encode('TransactionPDU', Val2),
    {ok,ExMsg2} = 'TCAPPackage':decode_TransactionPDU(B2),
    {_,_,_,{Key2,ExVal2}} = ExMsg2,
    {ok,_Parts2} = 'TCAPPackage':decode_part(Key2, ExVal2),

    Val3 = 'TCAPPackage_msg':val('PackageType', response),
    {ok,B3} = 'TCAPPackage':encode('PackageType', Val3),
    {ok,ExMsg3} = 'TCAPPackage':decode_PackageType(B3),
    {_,{_,_,_,{Key3,ExVal3}}} = ExMsg3,
    {ok,_Parts3}='TCAPPackage':decode_part(Key3, ExVal3).
    
enc_dec(T, V0) ->
    M = 'TCAPPackage',
    {ok,Enc} = M:encode(T, V0),
    asn1_test_lib:map_roundtrip(M, T, Enc),
    {ok,V} = M:decode(T, Enc),
    V.
