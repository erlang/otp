%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(testSeqPrim).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq',{bool, boolCon, boolPri, boolApp, boolExpCon, boolExpPri, boolExpApp}).
-record('Empty',{}).

compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqPrim",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqPrim','Seq',#'Seq'{bool = true,
					       boolCon = true,
					       boolPri = true,
					       boolApp = true,
					       boolExpCon = true,
					       boolExpPri = true,
					       boolExpApp = true}),
    ?line {ok,{'Seq',true,true,true,true,true,true,true}} = 
	asn1_wrapper:decode('SeqPrim','Seq',lists:flatten(Bytes11)),
    
    
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SeqPrim','Seq',#'Seq'{bool = false,
					       boolCon = false,
					       boolPri = false,
					       boolApp = false,
					       boolExpCon = false,
					       boolExpPri = false,
					       boolExpApp = false}),
    ?line {ok,{'Seq',false,false,false,false,false,false,false}} = 
	asn1_wrapper:decode('SeqPrim','Seq',lists:flatten(Bytes12)),
    
    
    
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('SeqPrim','Seq',#'Seq'{bool = false,
					       boolCon = true,
					       boolPri = false,
					       boolApp = true,
					       boolExpCon = false,
					       boolExpPri = true,
					       boolExpApp = false}),
    ?line {ok,{'Seq',false,true,false,true,false,true,false}} = 
	asn1_wrapper:decode('SeqPrim','Seq',lists:flatten(Bytes13)),
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqPrim','Empty',#'Empty'{}),
    ?line {ok,{'Empty'}} = 
	asn1_wrapper:decode('SeqPrim','Empty',lists:flatten(Bytes21)),



    ok.

