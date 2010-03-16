%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

-export([compile/3,main/1,ranap/1]).

-include_lib("test_server/include/test_server.hrl").

-record('AllocationOrRetentionPriority',{priorityLevel,iE_Extensions}).
-record('ProtocolExtensionField',{id,criticality,extensionValue}).

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{protocolIEs,protocolExtensions}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(DataDir ++ "Param",[Rules,{outdir,OutDir}]++Options).
%    ?line ok = asn1ct:compile(DataDir ++ "RANAP-CommonDataTypes",[Rules,{outdir,OutDir}]++Options).


main(Erule) ->
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

    ok.


ranap(_Erule) ->    
    ?line PIEVal2 = [{'ProtocolIE-Field',4,ignore,{'Cause',{radioNetwork,'rab-pre-empted'}}}],
    ?line Val2 = 
	#'InitiatingMessage'{procedureCode=1,
			     criticality=ignore,
			     value=#'Iu-ReleaseCommand'{protocolIEs=PIEVal2,
							protocolExtensions=asn1_NOVALUE}},
    
    ?line {ok,Bytes2} = asn1_wrapper:encode('RANAP','InitiatingMessage',Val2),
    ?line {ok,_Ret2} = asn1_wrapper:decode('RANAP','InitiatingMessage',Bytes2),
    
    ok.

open_type(uper_bin,Val) when is_list(Val) ->
    list_to_binary(Val);
open_type(_,Val) ->
    Val.
