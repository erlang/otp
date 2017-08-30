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
-module('TCAPPackage_msg').

-compile(export_all).

-include("TCAPPackage.hrl").

val('PackageType',unidirectional) ->
    {unidirectional,val('UniTransactionPDU')};
val('PackageType',abort) ->
    {abort,val('Abort')};
val('PackageType',Component) ->
    {Component,val('TransactionPDU')};
val('ComponentPDU',1) ->
    {invokeLast,val('Invoke')};
val('ComponentPDU',2) ->
    {returnResultLast,val('ReturnResult')};
val('ComponentPDU',3) ->
    {returnError,val('ReturnError')}.



val('UniTransactionPDU') ->
    #'UniTransactionPDU'{identifier=val('TransactionID'),
			 dialoguePortion=val('DialoguePortion'),
			 componentPortion=val('ComponentSequence')};
val('TransactionPDU') ->
    #'TransactionPDU'{identifier=val('TransactionID'),
		      dialoguePortion=val('DialoguePortion'),
		      componentPortion=val('ComponentSequence')};
val('TransactionID') ->
    <<"OCTET STRING">>;
val('DialoguePortion') ->
    #'DialoguePortion'{version=val('ProtocolVersion'),
		       applicationContext={integerApplicationId,12},
		       userInformation=val('UserInformation'),
		       securityContext={integerSecurityId,13},
		       confidentiality=val('Confidentiality')};
val('Confidentiality') ->
    #'Confidentiality'{confidentialityId={integerConfidentialityId,14}};
val('ProtocolVersion') ->
    <<"K">>;
val('UserInformation') ->
    [val('EXTERNAL'),val('EXTERNAL')];
val('EXTERNAL') ->
    #'EXTERNAL'{'direct-reference'={0,1,2},
		encoding={'single-ASN1-type',<<1,2,3,4>>}};
val('ComponentSequence') ->
    [val('ComponentPDU',1),val('ComponentPDU',2),val('ComponentPDU',3)];
val('Invoke') ->
    #'Invoke'{componentIDs = <<"AB">>,
	      opcode={local,-2},
	      parameter=running};
val('ReturnResult') ->
    #'ReturnResult'{componentID = <<"C">>,
		    parameter=[1,2,3,4]};
val('ReturnError') ->
    #'ReturnError'{componentID = <<"D">>,
		   errorCode={local,21},
		   parameter=true};
val('Abort') ->
    #'Abort'{identifier=val('TransactionID'),
	     dialoguePortion=val('DialoguePortion'),
	     causeInformation={abortCause,unrecognizedPackageType}};
val(Type) ->
    io:format("Missing type: ~p~n",[Type]).


check_result('PackageType',unidirectional,Res) ->
    {unidirectional,
     {'UniTransactionPDU',
      <<"OCTET STRING">>,
      {'DialoguePortion',<<"K">>,
       {integerApplicationId,12},
       [_,%{'EXTERNAL',{syntax,{0,1,2}},asn1_NOVALUE,OTVal},
	_],%{'EXTERNAL',{syntax,{0,1,2}},asn1_NOVALUE,OTVal}],
       {integerSecurityId,13},
       {'Confidentiality',
	{integerConfidentialityId,14}}},
      [{invokeLast,
	{_,<<"AB">>,{local,-2},running}},
       {returnResultLast,{_,<<"C">>,_}},
       {returnError,{_,<<"D">>,{local,21},true}}]}} = Res,
    ok;
%%    check_OT_val(OTVal);
check_result('PackageType',abort,Res)->
    {abort,{'Abort',<<"OCTET STRING">>,
	    {'DialoguePortion',<<"K">>,
	     {integerApplicationId,12},
	     [_,%{'EXTERNAL',{syntax,{0,1,2}},asn1_NOVALUE,OTVal},
	      _],%{'EXTERNAL',{syntax,{0,1,2}},asn1_NOVALUE,OTVal}],
	     {integerSecurityId,13},
	     {'Confidentiality',
	      {integerConfidentialityId,14}}},
	    {abortCause,unrecognizedPackageType}}} = Res,
    ok;
%%    check_OT_val(OTVal);
check_result('PackageType',response,Res) ->
    {response,{'TransactionPDU',<<"OCTET STRING">>,
	       {'DialoguePortion',
		<<"K">>,
		{integerApplicationId,12},
		[_,%{'EXTERNAL',{syntax,{0,1,2}},asn1_NOVALUE,OTVal},
		 _],%{'EXTERNAL',{syntax,{0,1,2}},asn1_NOVALUE,OTVal}],
		{integerSecurityId,13},
		{'Confidentiality',
		 {integerConfidentialityId,14}}},
	       [{invokeLast,
		 {_,<<"AB">>,{local,-2},running}},
		{returnResultLast,
		 {_,<<"C">>,_}},
		{returnError,
		 {_,<<"D">>,{local,21},true}}]}} = Res,
    ok.
%%    check_OT_val(OTVal).

check_OT_val([160,4,1,2,3,4]) ->
    ok;
check_OT_val(<<160,4,1,2,3,4>>) ->
    ok;
check_OT_val(_) ->
    error.
					       
