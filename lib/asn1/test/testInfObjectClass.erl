%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(testInfObjectClass).


-export([main/1]).

-include_lib("common_test/include/ct.hrl").

main(Rule) ->
    %% this test is added for OTP-4591, to test that elements in decoded
    %% value has terms in right order.
    Val = {'Seq',12,13,2},
    roundtrip('Seq', Val),
    
    %% OTP-5783
    {error,{asn1,{'Type not compatible with table constraint',
		  {component,'ArgumentType'},
		  {value,_},_}}} = 'InfClass':encode('Seq', {'Seq',12,13,1}),
    Bytes2 = case Rule of
		 ber ->
		     <<48,9,2,1,12,2,1,11,2,1,1>>;
		 _ ->
		     <<1,12,1,11,1,1>>
	     end,
    {error,{asn1,{'Type not compatible with table constraint',
		  {{component,_},
		   {value,_B},_}}}} = 'InfClass':decode('Seq', Bytes2),
    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('InfClass', T, V).
