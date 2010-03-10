%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
-module(testSeqOfIndefinite).

-export([compile/3]).
-export([main/0]).

-include_lib("test_server/include/test_server.hrl").

%-record('Seq1',{bool1, int1, seq1 = asn1_DEFAULT}).
%-record('Seq2',{seq2 = asn1_DEFAULT, bool2, int2}).
%-record('Seq3',{bool3, seq3 = asn1_DEFAULT, int3}).
%-record('Seq4',{seq41 = asn1_DEFAULT, seq42 = asn1_DEFAULT, seq43 = asn1_DEFAULT}).
%-record('SeqIn',{boolIn, intIn}).
%-record('SeqCho',{bool1, int1, seq1 = asn1_DEFAULT}).
%-record('SeqChoInline',{bool1, int1, seq1 = asn1_DEFAULT}).
%-record('SeqChoOfInline_SEQOF',{bool1, int1, seq1 = asn1_DEFAULT}).
%-record('SeqEmp',{seq1}).
%-record('Empty',{}).



compile(Config,Rules,Opts) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line Options = [Rules,{outdir,OutDir}]++Opts,
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-Constants-1",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-DataTypes-1",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-21-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-20-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-19-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-18-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-17-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-15-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-14-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "Mvrasn-11-4",Options),
    ?line ok = asn1ct:compile(DataDir ++ "SeqOf",Options).
    
main() ->
    ?line ok = test(isd),
    ?line ok = test(isd2),
    ?line ok = test(dsd),
    ?line ok = test(ul_res),
    ?line ok = test(prim),
    ?line ok = test(seqofseq),
    ?line ok = test('InsertSubscriberDataArg'). % OTP-4232

test(isd)->
    EncPdu = [48,128,129,7,145,148,113,50,1,0,241,131,1,0,176,128,5,0,161,128,48,22,2,1,1,144,2,241,33,145,4,0,1,2,3,146,3,36,131,16,148,2,1,42,48,35,2,1,2,144,2,241,33,145,4,255,255,255,255,146,3,37,147,18,147,0,148,13,7,67,79,77,80,65,78,89,4,67,79,77,53,48,28,2,1,3,144,2,241,33,146,3,26,98,31,148,14,9,67,79,77,80,65,78,89,49,50,3,67,79,77,0,0,0,0,152,1,2,0,0],
    
    ?line {ok,_} = asn1_wrapper:decode('Mvrasn-11-4',
				'InsertSubscriberDataArg',
				EncPdu),
    ok;

% 
% Problems with indefinite length encoding !!!
% 
test(isd2)->
    EncPdu = [48, 128, 128, 8, 98, 2, 50, 1, 0, 0, 0, 241, 176, 128, 161, 128, 48, 128, 2, 1, 1, 144, 2, 241, 33, 145, 4, 255, 23, 12, 1, 146, 3, 9, 17, 1, 147, 0, 148, 13, 7, 67, 79, 77, 80, 65, 78, 89, 4, 67, 79, 77, 53, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    
    ?line {ok,_DecPdu} = asn1_wrapper:decode('Mvrasn-11-4',
				'InsertSubscriberDataArg',
				EncPdu),

    ok;

% 
% Is doing fine, although there is indefinite encoding used... !!!
% 
test(dsd)->
    EncPdu = [48, 128, 128, 8, 98, 2, 50, 1, 0, 0, 0, 241, 170, 2, 5, 0, 0, 0, 0, 0],
    
    ?line {ok,_DecPdu} = asn1_wrapper:decode('Mvrasn-11-4',
				'DeleteSubscriberDataArg',
				EncPdu),

    ok;

%
% Is doing fine !!!
% 
test(ul_res)->
    EncPdu = [48, 9, 4, 7, 145, 148, 113, 66, 16, 17, 241],
    
    ?line {ok,_DecPdu} = asn1_wrapper:decode('Mvrasn-11-4',
				'UpdateGprsLocationRes',
				EncPdu),

    ok;

test(prim) ->
    ?line {ok,Bytes} = asn1_wrapper:encode('SeqOf','SeqOfInt',[10,20,30]),
    ?line [Tag,_Len|Ints] = lists:flatten(Bytes),
    ?line {ok,[10,20,30]} = 
	asn1_wrapper:decode('SeqOf','SeqOfInt',[Tag,128|Ints] ++ [0,0]),
    ok;

test(seqofseq) ->
    {ok,_V} = asn1_wrapper:decode('Mvrasn-DataTypes-1',
                 'SentParameters',
                 [48,
                  129,
                  190,
                  161,
                  128,
                  4,
                  16,
                  176,
                  197,
                  182,
                  68,
                  41,
                  243,
                  188,
                  205,
                  123,
                  13,
                  9,
                  145,
                  206,
                  200,
                  144,
                  102,
                  4,
                  4,
                  176,
                  197,
                  182,
                  68,
                  4,
                  8,
                  41,
                  243,
                  188,
                  205,
                  123,
                  13,
                  9,
                  145,
                  0,
                  0,
                  161,
                  128,
                  4,
                  16,
                  39,
                  0,
                  3,
                  117,
                  35,
                  189,
                  130,
                  21,
                  42,
                  104,
                  49,
                  194,
                  212,
                  24,
                  151,
                  234,
                  4,
                  4,
                  39,
                  0,
                  3,
                  117,
                  4,
                  8,
                  35,
                  189,
                  130,
                  21,
                  42,
                  104,
                  49,
                  194,
                  0,
                  0,
                  161,
                  128,
                  4,
                  16,
                  62,
                  207,
                  166,
                  59,
                  71,
                  29,
                  37,
                  97,
                  120,
                  25,
                  132,
                  80,
                  144,
                  251,
                  161,
                  123,
                  4,
                  4,
                  62,
                  207,
                  166,
                  59,
                  4,
                  8,
                  71,
                  29,
                  37,
                  97,
                  120,
                  25,
                  132,
                  80,
                  0,
                  0,
                  161,
                  128,
                  4,
                  16,
                  95,
                  183,
                  173,
                  151,
                  17,
                  76,
                  148,
                  146,
                  248,
                  102,
                  127,
                  215,
                  102,
                  224,
                  39,
                  60,
                  4,
                  4,
                  95,
                  183,
                  173,
                  151,
                  4,
                  8,
                  17,
                  76,
                  148,
                  146,
                  248,
                  102,
                  127,
                  215,
                  0,
                  0,
                  161,
                  128,
                  4,
                  16,
                  41,
                  198,
                  247,
                  157,
                  117,
                  190,
                  203,
                  170,
                  91,
                  146,
                  88,
                  91,
                  223,
                  220,
                  188,
                  16,
                  4,
                  4,
                  41,
                  198,
                  247,
                  157,
                  4,
                  8,
                  117,
                  190,
                  203,
                  170,
                  91,
                  146,
                  88,
                  91,
                  0,
                  0]),
    ok;
test('InsertSubscriberDataArg') ->
    {ok,_V} =
	asn1_wrapper:decode('Mvrasn-11-4','InsertSubscriberDataArg',
			    [16#30,16#80,16#81,16#07,16#91,16#94,
			     16#71,16#92,16#00,16#35,16#80,16#83,
			     16#01,16#00,16#A6,16#06,16#04,16#01,
			     16#21,16#04,16#01,16#22,16#B0,16#80,
			     16#05,16#00,16#A1,16#80,16#30,16#1A,
			     16#02,16#01,16#01,16#90,16#02,16#F1,
			     16#21,16#92,16#03,16#0D,16#92,16#1F,
			     16#94,16#0C,16#03,16#53,16#49,16#4D,
			     16#03,16#47,16#53,16#4E,16#03,16#4C,
			     16#4B,16#50,16#00,16#00,16#00,16#00,
			     16#98,16#01,16#00,16#00,16#00]),
    ok.
