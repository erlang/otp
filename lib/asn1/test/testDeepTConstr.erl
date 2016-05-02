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


-module(testDeepTConstr).

-export([main/1]).

-include_lib("common_test/include/ct.hrl").

main(_Erule) ->
    Val1 = {substrings,
	    {'FilterItem_substrings',
	     {2,6},
	     [{initial,"SE"},
	      {any,"DK"},
	      {final,"N"}]}},

    Val2 = {substrings,
	    {'FilterItem_substrings',
	     {2,6},
	     [{initial,"SE"},
	      {any,"DK"},
	      {final,"NO"}]}},

    Reason = must_fail('TConstrChoice', 'FilterItem', Val1),
    io:format("Reason: ~p~n~n",[Reason]),
    {ok,Bytes2} = 'TConstrChoice':encode('FilterItem', Val2),
    {ok,Res} = 'TConstrChoice':decode('FilterItem', Bytes2),

    %% test of OTP-4248.
    {ok,Bytes3} = 'TConstrChoice':encode('Seq', {'Seq',3,Bytes2}),
    {ok,{'Seq',3,Bytes4}} = 'TConstrChoice':decode('Seq', Bytes3),
    {ok,Res} = 'TConstrChoice':decode('FilterItem', Bytes4),
    
    %% test of TConstr

    Seq1Val = {'Seq1',{'Seq1_a',12,{2,4}},
	       {'Seq1_b',13,{'Type-object1',14,true}}},
    roundtrip('TConstr', 'Seq1', Seq1Val),
    

    Seq2Val = {'Seq2',123,{'Seq2_content',{2,6,7},
			   {first,{'Type-object3_first',false,47}},
			   false}},
    roundtrip('TConstr', 'Seq2', Seq2Val),

    roundtrip('TConstr', 'Info', {'Info',{'Info_xyz',{1,2}},1234}),

    roundtrip('TConstr', 'Deeper',
	      {'Deeper',
	       {'Deeper_a',12,
		{'Deeper_a_s',{2,4},42}},
	       {'Deeper_b',13,{'Type-object1',14,true}}}),

    roundtrip('TConstr', 'Seq3',
	      {'Seq3',
	       {'Seq3_a',42,'TConstr':'id-object1'()},
	       {'Seq3_b',
		{'Type-object1',-777,true},
		12345,
		{'Seq3_b_bc',12345789,{'Type-object1',-999,true}}}}),
    roundtrip('TConstr', 'Seq3-Opt',
	      {'Seq3-Opt',
	       {'Seq3-Opt_a',42,'TConstr':'id-object1'()},
	       {'Seq3-Opt_b',
		{'Type-object1',-777,true},
		12345,
		{'Seq3-Opt_b_bc',12345789,{'Type-object1',-999,true}}}}),
    ok.


roundtrip(M, T, V) ->
    asn1_test_lib:roundtrip(M, T, V).

%% Either encoding or decoding must fail.
must_fail(M, T, V) ->
    case M:encode(T, V) of
	{ok,E} ->
	    {error,Reason} = M:decode(T, E),
	    Reason;
	{error,Reason} ->
	    Reason
    end.
