%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-module(testDER).

-export([test/0]).

-include_lib("common_test/include/ct.hrl").

test() ->
    Val = {'Set',12,{version,214},true},
    roundtrip_enc('Set', Val, <<49,12,1,1,255,2,2,0,214,161,3,2,1,12>>),
    
    ValSof = [{version,12},{message,"PrintableString"},
	      {message,"Print"},{version,11}],
    ValSofSorted = [{version,11},{version,12},
		    {message,"Print"},{message,"PrintableString"}],
    roundtrip_enc('SetOf', ValSof, ValSofSorted,
		  <<49,30,2,1,11,2,1,12,19,5,80,114,105,110,116,19,15,80,
		    114,105,110,116,97,98,108,101,83,116,114,105,110,103>>),

    ValSO = [{'Seq2',1,true},{'Seq2',120000,false},{'Seq2',3,true}],
    roundtrip('SO', ValSO).

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('DERSpec', T, V).

roundtrip_enc(T, V, Enc) ->
    Enc = asn1_test_lib:roundtrip_enc('DERSpec', T, V).

roundtrip_enc(T, V, Expected, Enc) ->
    Enc = asn1_test_lib:roundtrip_enc('DERSpec', T, V, Expected).
