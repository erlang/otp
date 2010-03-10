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
-module(testDef).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Def1',{bool0, 
		bool1 = asn1_DEFAULT, 
		bool2 = asn1_DEFAULT, 
		bool3 = asn1_DEFAULT}).
-record('Def2',{bool10, 
		bool11 = asn1_DEFAULT, 
		bool12 = asn1_DEFAULT, 
		bool13}).
-record('Def3',{bool30 = asn1_DEFAULT, 
		bool31 = asn1_DEFAULT, 
		bool32 = asn1_DEFAULT, 
		bool33 = asn1_DEFAULT}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "Def",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('Def','Def1',#'Def1'{bool0 = true,
							    bool1 = true,
							    bool2 = true,
							    bool3 = true}),
    ?line {ok,{'Def1',true,true,true,true}} = 
	asn1_wrapper:decode('Def','Def1',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('Def','Def1',#'Def1'{bool0 = true}),
    ?line {ok,{'Def1',true,false,false,false}} = 
	asn1_wrapper:decode('Def','Def1',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('Def','Def1',#'Def1'{bool0 = true,
							    bool2 = false}),
    ?line {ok,{'Def1',true,false,false,false}} = 
	asn1_wrapper:decode('Def','Def1',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = asn1_wrapper:encode('Def','Def1',#'Def1'{bool0 = false,
							    bool3 = false}),
    ?line {ok,{'Def1',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def1',lists:flatten(Bytes14)),
    
    
    
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('Def','Def2',#'Def2'{bool10 = false,
							    bool11 = false,
							    bool12 = false,
							    bool13 = false}),
    ?line {ok,{'Def2',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('Def','Def2',#'Def2'{bool10 = true,
							    bool13 = false}),
    ?line {ok,{'Def2',true,false,false,false}} = 
	asn1_wrapper:decode('Def','Def2',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = asn1_wrapper:encode('Def','Def2',#'Def2'{bool10 = true,
							    bool11 = false,
							    bool13 = false}),
    ?line {ok,{'Def2',true,false,false,false}} = 
	asn1_wrapper:decode('Def','Def2',lists:flatten(Bytes23)),
    
    ?line {ok,Bytes24} = asn1_wrapper:encode('Def','Def2',#'Def2'{bool10 = false,
							    bool12 = false,
							    bool13 = false}),
    ?line {ok,{'Def2',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def2',lists:flatten(Bytes24)),
    
    
    
    
    ?line {ok,Bytes31} = asn1_wrapper:encode('Def','Def3',#'Def3'{bool30 = false,
							    bool31 = false,
							    bool32 = false,
							    bool33 = false}),
    ?line {ok,{'Def3',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def3',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('Def','Def3',#'Def3'{}),
    ?line {ok,{'Def3',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def3',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = asn1_wrapper:encode('Def','Def3',#'Def3'{bool30 = true}),
    ?line {ok,{'Def3',true,false,false,false}} = 
	asn1_wrapper:decode('Def','Def3',lists:flatten(Bytes33)),
    
    ?line {ok,Bytes34} = asn1_wrapper:encode('Def','Def3',#'Def3'{bool32 = false}),
    ?line {ok,{'Def3',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def3',lists:flatten(Bytes34)),
    
    ?line {ok,Bytes35} = asn1_wrapper:encode('Def','Def3',#'Def3'{bool33 = false}),
    ?line {ok,{'Def3',false,false,false,false}} = 
	asn1_wrapper:decode('Def','Def3',lists:flatten(Bytes35)),
    
    
    
    ok.
