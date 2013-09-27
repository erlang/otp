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
				  extensionValue= <<0>>},
	#'ProtocolExtensionField'{id=2,
				  criticality=ignore,
				  extensionValue= <<1>>}]},
    BERVal = #'AllocationOrRetentionPriority'
      {priorityLevel = true,
       iE_Extensions = 
       [#'ProtocolExtensionField'{id=14,
				  criticality=reject,
				  extensionValue= <<2,1,0>>},
	#'ProtocolExtensionField'{id=2,
				  criticality=ignore,
				  extensionValue= <<2,1,1>>}]},
    case Erule of
	ber ->
	    roundtrip('AllocationOrRetentionPriority', BERVal);
	per ->
	    roundtrip('AllocationOrRetentionPriority', PERVal);
	uper ->
	    roundtrip('AllocationOrRetentionPriority', PERVal)
    end,

    %% test code for OTP-4242, ValueFromObject

    case Erule of
	ber ->
	    {ok,_Val3} = 'Param':decode('OS1', [4,2,1,2]),
	    {error,_Reason1} = 'Param':decode('OS1',[4,4,1,2,3,4]),
	    {error,_Reason2} = 'Param':decode('OS2',[4,4,1,2,3,4]),
	    {ok,_Val4} = 'Param':decode('OS1',[4,2,1,2]);
	_ ->					%per/uper
	    roundtrip('OS1', [1,2]),
	    {error,_Reason3} = 'Param':encode('OS1', [1,2,3,4])
    end,

    roundtrip('Scl', {'Scl',42,{a,9738654}}),
    roundtrip('Scl', {'Scl',42,{b,false}}),
    roundtrip('Scl', {'Scl',42,{b,true}}),

    ok.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('Param', T, V).


ranap(_Erule) ->    
    PIEVal2 = [{'ProtocolIE-Field',4,ignore,{radioNetwork,'rab-pre-empted'}}],
    ?line Val2 = 
	#'InitiatingMessage'{procedureCode=1,
			     criticality=ignore,
			     value=#'Iu-ReleaseCommand'{protocolIEs=PIEVal2,
							protocolExtensions=asn1_NOVALUE}},
    
    {ok,Bytes2} = 'RANAP':encode('InitiatingMessage', Val2),
    {ok,_Ret2} = 'RANAP':decode('InitiatingMessage', Bytes2),
    
    ok.

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
	'Param2':decode('HandoverRequired', Enc),
    true = is_binary(Open100),
    true = is_binary(Open101),

    %% Test single root.
    roundtrip2('SingleRoot',
	       {'SingleRoot',[{'ProtocolIE-Field',1,"ABC"},
			      {'ProtocolIE-Field',2,9999}]}),
    ok.


roundtrip2(T, V) ->
    asn1_test_lib:roundtrip_enc('Param2', T, V).
