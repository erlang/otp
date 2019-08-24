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

-module(testInfObjExtract).

-export([main/0]).

main() ->
    roundtrip_data_object_13('DataSeq-1'),

    roundtrip_data_object_1('DataSeq-2'),
    roundtrip_data_object_1('DataSeq-3'),
    roundtrip_data_object_1('DataSeq-4'),

    roundtrip_data_object_13('DataSeq-5'),
    roundtrip_data_object_13('DataSeq-6'),

    roundtrip_data_object_1('DataSeqSingleSet-1'),
    roundtrip_data_object_1('DataSeqSingleSet-2'),

    roundtrip('ObjClassSeq-1', {'ObjClassSeq-1',1,true}),
    roundtrip('ObjClassSeq-1', {'ObjClassSeq-1',2,true}),

    roundtrip_error('ObjClassSeq-1', {'ObjClassSeq-1',0,false}),
    roundtrip_error('ObjClassSeq-1', {'ObjClassSeq-1',3,true}),
    roundtrip_error('ObjClassSeq-1', {'ObjClassSeq-1',4,false}),
    roundtrip_error('ObjClassSeq-1', {'ObjClassSeq-1',5,true}),

    ok.

roundtrip_data_object_13(SeqType) ->
    roundtrip(SeqType, {SeqType,1,true}),
    roundtrip(SeqType, {SeqType,2,<<"abc">>}),
    roundtrip(SeqType, {SeqType,3,<<42:5>>}),
    roundtrip_error(SeqType, {SeqType,4,42}).

roundtrip_data_object_1(SeqType) ->
    roundtrip(SeqType, {SeqType,1,false}),
    roundtrip(SeqType, {SeqType,1,true}),
    roundtrip_error(SeqType, {SeqType,1,42}),
    roundtrip_error(SeqType, {SeqType,2,<<"abc">>}),
    roundtrip_error(SeqType, {SeqType,3,<<42:5>>}),
    roundtrip_error(SeqType, {SeqType,999,42}).

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('InfObjExtract', T, V).

roundtrip_error(T, V) ->
    try asn1_test_lib:roundtrip('InfObjExtract', T, V) of
	ok ->
	    test_server:fail()
    catch
	_:_ ->
	    ok
    end.
