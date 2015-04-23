%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2013. All Rights Reserved.
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
-module(testParamBasic).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('T11',{number, string=asn1_DEFAULT}).
-record('T12',{number, string=asn1_DEFAULT}).
-record('T21',{number, string}).
-record('T22',{number, string}).

main(Rules) ->
    roundtrip('T11', #'T11'{number=11,string="hello"}),
    roundtrip('T12', #'T12'{number=11,string = <<21:5>>}),
    roundtrip('T21', #'T21'{number=11,string="hello"}),
    roundtrip('T22', #'T22'{number=11,string = <<21:5>>}),
    case Rules of
	der ->
	    <<48,3,128,1,11>> =
		roundtrip_enc('T11', #'T11'{number=11,string="hej"}),
	    <<48,3,128,1,11>> =
		roundtrip_enc('T12',
			      #'T12'{number=11,string=[1,0,1,0]},
			      #'T12'{number=11,string = <<10:4>>});
	_ -> ok
    end,
    roundtrip('AnAlgorithm', {'AnAlgorithm',1,42}),
    roundtrip('AnAlgorithm', {'AnAlgorithm',2,true}),
    roundtrip('AnAlgorithm', {'AnAlgorithm',2,false}),
    {'AnAlgorithm',1,42} = 'ParamBasic':'alg-seq-1'(),
    {'AnAlgorithm',2,true} = 'ParamBasic':'alg-seq-2'(),

    roundtrip('Seq', {'Seq',
		      {'Seq_c1',{2,1,1},42},
		      {'Seq_c2',{2,1,1,1},asn1_NOVALUE}}),

    {_,{2,9,9,9,7},'NULL'} = 'ParamBasic':'algid-hmacWithSHA1'(),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('ParamBasic', Type, Value).

roundtrip_enc(Type, Value) ->
    asn1_test_lib:roundtrip_enc('ParamBasic', Type, Value).

roundtrip_enc(Type, Value, Expected) ->
    asn1_test_lib:roundtrip_enc('ParamBasic', Type, Value, Expected).
