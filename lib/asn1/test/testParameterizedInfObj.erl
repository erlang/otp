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

-module(testParameterizedInfObj).

-export([main/2,ranap/1]).

-include_lib("test_server/include/test_server.hrl").

-record('AllocationOrRetentionPriority',{priorityLevel,iE_Extensions}).
-record('ProtocolExtensionField',{id,criticality,extensionValue}).

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{protocolIEs,protocolExtensions}).


main(Config, Erule) ->
    param(Erule),
    param2(Config, Erule).

param(Erule) ->
    PERVal = #'AllocationOrRetentionPriority'
      {priorityLevel = true,
       iE_Extensions = 
       [#'ProtocolExtensionField'{id=14,
				  criticality=reject,
				  extensionValue=open_type(Erule,[0])},
	#'ProtocolExtensionField'{id=2,
				  criticality=ignore,
				  extensionValue=open_type(Erule,[1])}]},
    BERVal = #'AllocationOrRetentionPriority'
      {priorityLevel = true,
       iE_Extensions = 
       [#'ProtocolExtensionField'{id=14,
				  criticality=reject,
				  extensionValue=[2,1,0]},
	#'ProtocolExtensionField'{id=2,
				  criticality=ignore,
				  extensionValue=[2,1,1]}]},
    ?line {ok,Bytes1} = 
	case asn1_wrapper:erule(Erule) of
	    per ->
		asn1_wrapper:encode('Param','AllocationOrRetentionPriority',
				    PERVal);
	    _ ->
		asn1_wrapper:encode('Param','AllocationOrRetentionPriority',
				    BERVal)
	end,

    ?line {ok,{'AllocationOrRetentionPriority',true,[_R1,_R2]}} =
	asn1_wrapper:decode('Param','AllocationOrRetentionPriority',Bytes1),

    %% test code for OTP-4242, ValueFromObject

    case asn1_wrapper:erule(Erule) of
	ber ->
	    ?line {ok,_Val3} = asn1_wrapper:decode('Param','OS1',[4,2,1,2]),
	    ?line {error,_Reason1} = 
		asn1_wrapper:decode('Param','OS1',[4,4,1,2,3,4]),
	    ?line {error,_Reason2} = 
		asn1_wrapper:decode('Param','OS2',[4,4,1,2,3,4]),
	    ?line {ok,_Val4} = asn1_wrapper:decode('Param','OS1',[4,2,1,2]);
	per ->
	    ?line {ok,Bytes3} =
		asn1_wrapper:encode('Param','OS1',[1,2]),
	    ?line {ok,[1,2]} =
		asn1_wrapper:decode('Param','OS1',Bytes3),
	    ?line {error,_Reason3} =
		asn1_wrapper:encode('Param','OS1',[1,2,3,4])
    end,

    roundtrip('Scl', {'Scl',42,{a,9738654}}),
    roundtrip('Scl', {'Scl',42,{b,false}}),
    roundtrip('Scl', {'Scl',42,{b,true}}),

    ok.

roundtrip(T, V) ->
    {ok,Enc} = 'Param':encode(T, V),
    {ok,V} = 'Param':decode(T, Enc),
    ok.


ranap(_Erule) ->    
    PIEVal2 = [{'ProtocolIE-Field',4,ignore,{radioNetwork,'rab-pre-empted'}}],
    ?line Val2 = 
	#'InitiatingMessage'{procedureCode=1,
			     criticality=ignore,
			     value=#'Iu-ReleaseCommand'{protocolIEs=PIEVal2,
							protocolExtensions=asn1_NOVALUE}},
    
    ?line {ok,Bytes2} = asn1_wrapper:encode('RANAP','InitiatingMessage',Val2),
    ?line {ok,_Ret2} = asn1_wrapper:decode('RANAP','InitiatingMessage',Bytes2),
    
    ok.

open_type(uper,Val) when is_list(Val) ->
    list_to_binary(Val);
open_type(_,Val) ->
    Val.

param2(Config, Erule) ->
    roundtrip2('HandoverRequired',
	       {'HandoverRequired',
		[{'ProtocolIE-Field',1,"ABC"},
		 {'ProtocolIE-Field',2,577799}]}),
    Enc = roundtrip2('HandoverRequired',
		     {'HandoverRequired',
		      [{'ProtocolIE-Field',1,"ABC"},
		       {'ProtocolIE-Field',2,-42},
		       {'ProtocolIE-Field',100,533},
		       {'ProtocolIE-Field',101,true}]}),

    %% Now remove the data after the extension mark in the object set.
    DataDir = ?config(data_dir, Config),
    CaseDir = ?config(case_dir, Config),
    Asn1SrcBase = "Param2.asn1",
    Asn1SrcFile0 = filename:join(DataDir, Asn1SrcBase),
    {ok,Src0} = file:read_file(Asn1SrcFile0),
    Src = re:replace(Src0, "--Delete-start.*?--Delete-end", "...\n",
		     [dotall,global,{return,binary}]),
    io:format("~s\n\n", [Src]),

    Asn1SrcFile = filename:join(CaseDir, Asn1SrcBase),
    ok = file:write_file(Asn1SrcFile, Src),
    ok = asn1ct:compile(Asn1SrcFile,
			[{i,DataDir},{outdir,CaseDir},Erule]),

    %% Decompile extended data.
    {ok,{'HandoverRequired',[{'ProtocolIE-Field',1,"ABC"},
			     {'ProtocolIE-Field',2,-42},
			     {'ProtocolIE-Field',100,Open100},
			     {'ProtocolIE-Field',101,Open101}]}} =
	asn1_wrapper:decode('Param2', 'HandoverRequired', Enc),
    true = is_binary(Open100),
    true = is_binary(Open101),

    %% Test single root.
    roundtrip2('SingleRoot',
	       {'SingleRoot',[{'ProtocolIE-Field',1,"ABC"},
			      {'ProtocolIE-Field',2,9999}]}),
    ok.


roundtrip2(T, V) ->
    {ok,Enc} = asn1_wrapper:encode('Param2', T, V),
    {ok,V} = asn1_wrapper:decode('Param2', T, Enc),
    Enc.
