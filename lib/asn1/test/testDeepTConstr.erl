%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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


-module(testDeepTConstr).

-export([compile/3,main/1]).

-include_lib("test_server/include/test_server.hrl").

compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),

    ?line ok = asn1ct:compile(DataDir ++ 
			      "TConstrChoice",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++
			      "TConstr",[Rules,{outdir,OutDir}]++Options).



main(_Erule) ->
    Val1 = {'FilterItem',
	    {substrings,
	     {'FilterItem_substrings',
	      {2,6},
	      [{initial,"SE"},
	       {any,"DK"},
	       {final,"N"}]}}},

    Val2 = {'FilterItem',
	    {substrings,
	     {'FilterItem_substrings',
	      {2,6},
	      [{initial,"SE"},
	       {any,"DK"},
	       {final,"NO"}]}}},

    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('TConstrChoice','FilterItem',Val1),
    
    ?line {error,Reason} = asn1_wrapper:decode('TConstrChoice','FilterItem',Bytes1),
    
    io:format("Reason: ~p~n~n",[Reason]),
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('TConstrChoice','FilterItem',Val2),
    
    ?line {ok,Res} = asn1_wrapper:decode('TConstrChoice','FilterItem',Bytes2),
    


    %% test of OTP-4248.
    ?line {ok,Bytes3} =
	asn1_wrapper:encode('TConstrChoice','Seq',{'Seq',3,Bytes2}),

    ?line {ok,{'Seq',3,Bytes4}} =
	asn1_wrapper:decode('TConstrChoice','Seq',Bytes3),
    
    ?line {ok,Res} = asn1_wrapper:decode('TConstrChoice','FilterItem',Bytes4),
    
    %% test of TConstr

    Seq1Val = {'Seq1',{'Seq1_a',12,{2,4}},{'Seq1_b',13,{'Type-object1',14,true}}},
    ?line {ok,Bytes5} =
	asn1_wrapper:encode('TConstr','Seq1',Seq1Val),
    
    ?line {ok,Seq1Val} =
	asn1_wrapper:decode('TConstr','Seq1',Bytes5),
    

    Seq2Val = {'Seq2',123,{'Seq2_content',{2,6,7},
			   {first,{'Type-object3_first',false,47}},
			   false}},
    
    ?line {ok,Bytes6} =
	asn1_wrapper:encode('TConstr','Seq2',Seq2Val),

    ?line {ok,Seq2Val} =
	asn1_wrapper:decode('TConstr','Seq2',Bytes6),

    InfoVal = {'Info',{'Info_xyz',{1,2}},1234},
    
    ?line {ok,Bytes7} =
	asn1_wrapper:encode('TConstr','Info',InfoVal),

    ?line {ok,InfoVal} =
	asn1_wrapper:decode('TConstr','Info',Bytes7).
