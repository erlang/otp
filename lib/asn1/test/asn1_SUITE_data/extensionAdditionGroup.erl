%%%-------------------------------------------------------------------
%%% File    : extensionAdditionGroup.erl
%%% Author  : Kenneth Lundin
%%% Description :
%%%
%%% Created : 18 May 2010 by kenneth
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

%%%-------------------------------------------------------------------
-module(extensionAdditionGroup).
-include("Extension-Addition-Group.hrl").


-compile(export_all).

run(Erule) ->
    Val = #'Ax'{a=253, b = true, c= {e,true}, g="123", h = true},
    io:format("~p:~p~n",[Erule,Val]),
    {ok,List}= asn1rt:encode('Extension-Addition-Group','Ax',Val),
    Enc = iolist_to_binary(List),
    io:format("~p~n",[Enc]),
    {ok,Val2} = asn1rt:decode('Extension-Addition-Group','Ax',Enc),
    io:format("~p~n",[Val2]),
    case Val2 of
	Val -> ok;
	_ -> exit({expected,Val, got, Val2})
    end.

run2(Erule) ->
    Val = #'Ax3'{a=253, b = true, s = #'Ax3_s'{sa = 11, sb = true, sextaddgroup = 17}},
    io:format("~p:~p~n",[Erule,Val]),
    {ok,List}= asn1rt:encode('Extension-Addition-Group','Ax3',Val),
    Enc = iolist_to_binary(List),
    io:format("~p~n",[Enc]),
    {ok,Val2} = asn1rt:decode('Extension-Addition-Group','Ax3',Enc),
    io:format("~p~n",[Val2]),
    case Val2 of
	Val -> ok;
	_ -> exit({expected,Val, got, Val2})
    end.

run3(Erule) ->
    Val = 
{'RRC-DL-DCCH-Message',
 {c1,
  {rrcConnectionReconfiguration,
   {'RRC-RRCConnectionReconfiguration',0,
    {c1,
     {'rrcConnectionReconfiguration-r8',
      {'RRC-RRCConnectionReconfiguration-r8-IEs',
       {'RRC-MeasConfig',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
        asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
        asn1_NOVALUE,asn1_NOVALUE},
       asn1_NOVALUE,
       [[80,66,0,5,10,0,5,0,24,11,7,84,54,33,0,1,1,0,0,0,1,39,5,66,127,0,0,1],
        []],
       {'RRC-RadioResourceConfigDedicated',
        [{'RRC-SRB-ToAddMod',1,
          {explicitValue,
           {am,
            {'RRC-RLC-Config_am',
             {'RRC-UL-AM-RLC',ms45,pInfinity,kBinfinity,t4},
             {'RRC-DL-AM-RLC',ms35,ms0}}}},
          {explicitValue,
           {'RRC-LogicalChannelConfig',
            {'RRC-LogicalChannelConfig_ul-SpecificParameters',3,infinity,
             ms50,0},
            asn1_NOVALUE}}}],
        [{'RRC-DRB-ToAddMod',3,3,
          {'RRC-PDCP-Config',infinity,
           {'RRC-PDCP-Config_rlc-AM',false},
           asn1_NOVALUE,
           {notUsed,'NULL'}},
          {am,
           {'RRC-RLC-Config_am',
            {'RRC-UL-AM-RLC',ms70,p256,kBinfinity,t4},
            {'RRC-DL-AM-RLC',ms35,ms40}}},
          3,
          {'RRC-LogicalChannelConfig',
           {'RRC-LogicalChannelConfig_ul-SpecificParameters',5,infinity,ms50,
            1},
           asn1_NOVALUE}},
         {'RRC-DRB-ToAddMod',4,4,
          {'RRC-PDCP-Config',infinity,
           {'RRC-PDCP-Config_rlc-AM',false},
           asn1_NOVALUE,
           {notUsed,'NULL'}},
          {am,
           {'RRC-RLC-Config_am',
            {'RRC-UL-AM-RLC',ms70,p256,kBinfinity,t4},
            {'RRC-DL-AM-RLC',ms35,ms40}}},
          4,
          {'RRC-LogicalChannelConfig',
           {'RRC-LogicalChannelConfig_ul-SpecificParameters',5,infinity,ms50,
            1},
           asn1_NOVALUE}}],
        asn1_NOVALUE,
        {explicitValue,
         {'RRC-MAC-MainConfig',
          {'RRC-MAC-MainConfig_ul-SCH-Config',n4,sf10,sf10240,false},
          asn1_NOVALUE,sf500,
          {setup,{'RRC-MAC-MainConfig_phr-Config_setup',sf200,sf200,dB3}},
          asn1_NOVALUE}},
        asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE},
       asn1_NOVALUE,asn1_NOVALUE}}}}}}},
        io:format("~p:~p~n",[Erule,Val]),
    {ok,List}= asn1rt:encode('EUTRA-RRC-Definitions','DL-DCCH-Message',Val),
    Enc = iolist_to_binary(List),
    io:format("Result from encode:~n~p~n",[Enc]),
    {ok,Val2} = asn1rt:decode('EUTRA-RRC-Definitions','DL-DCCH-Message',Enc),
    io:format("Result from decode:~n~p~n",[Val2]),
    case Val2 of
	Val -> ok;
	_ -> exit({expected,Val, got, Val2})
    end.

