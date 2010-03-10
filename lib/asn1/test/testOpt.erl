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
-module(testOpt).

-export([compile/2]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Opt1',{bool0, 
		bool1 = asn1_NOVALUE, 
		bool2 = asn1_NOVALUE, 
		bool3 = asn1_NOVALUE}).

-record('Opt2',{bool10, 
		bool11 = asn1_NOVALUE, 
		bool12 = asn1_NOVALUE, 
		bool13}).

-record('Opt3',{bool30 = asn1_NOVALUE, 
		bool31 = asn1_NOVALUE, 
		bool32 = asn1_NOVALUE, 
		bool33 = asn1_NOVALUE}).


compile(Config,Rules) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "Opt",[Rules,{outdir,OutDir}]).



main(_Rules) ->
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('Opt','Opt1',#'Opt1'{bool0 = true,
							 bool1 = true,
							 bool2 = true,
							 bool3 = true}),    
    ?line {ok,{'Opt1',true,true,true,true}} = 
	asn1_wrapper:decode('Opt','Opt1',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = asn1_wrapper:encode('Opt','Opt1',#'Opt1'{bool0 = true}),
    ?line {ok,{'Opt1',true,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}} = 
	asn1_wrapper:decode('Opt','Opt1',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('Opt','Opt1',#'Opt1'{bool0 = true,
							 bool2 = false}),
    ?line {ok,{'Opt1',true,asn1_NOVALUE,false,asn1_NOVALUE}} = 
	asn1_wrapper:decode('Opt','Opt1',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = asn1_wrapper:encode('Opt','Opt1',#'Opt1'{bool0 = false,
							  bool3 = false}),
    ?line {ok,{'Opt1',false,asn1_NOVALUE,asn1_NOVALUE,false}} = 
	asn1_wrapper:decode('Opt','Opt1',lists:flatten(Bytes14)),
    
    
    
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('Opt','Opt2',#'Opt2'{bool10 = false,
							  bool11 = false,
							  bool12 = false,
							  bool13 = false}),
    ?line {ok,{'Opt2',false,false,false,false}} = 
	asn1_wrapper:decode('Opt','Opt2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('Opt','Opt2',#'Opt2'{bool10 = true,
							  bool13 = false}),
    ?line {ok,{'Opt2',true,asn1_NOVALUE,asn1_NOVALUE,false}} = 
	asn1_wrapper:decode('Opt','Opt2',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = asn1_wrapper:encode('Opt','Opt2',#'Opt2'{bool10 = true,
							  bool11 = false,
							  bool13 = false}),
    ?line {ok,{'Opt2',true,false,asn1_NOVALUE,false}} = 
	asn1_wrapper:decode('Opt','Opt2',lists:flatten(Bytes23)),
    
    ?line {ok,Bytes24} = asn1_wrapper:encode('Opt','Opt2',#'Opt2'{bool10 = false,
							  bool12 = false,
							  bool13 = false}),
    ?line {ok,{'Opt2',false,asn1_NOVALUE,false,false}} = 
	asn1_wrapper:decode('Opt','Opt2',lists:flatten(Bytes24)),
    
    
    
    
    ?line {ok,Bytes31} = asn1_wrapper:encode('Opt','Opt3',#'Opt3'{bool30 = false,
							  bool31 = false,
							  bool32 = false,
							  bool33 = false}),
    ?line {ok,{'Opt3',false,false,false,false}} = 
	asn1_wrapper:decode('Opt','Opt3',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('Opt','Opt3',#'Opt3'{}),
    ?line {ok,{'Opt3',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}} = 
	asn1_wrapper:decode('Opt','Opt3',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = asn1_wrapper:encode('Opt','Opt3',#'Opt3'{bool30 = true}),
    ?line {ok,{'Opt3',true,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}} = 
	asn1_wrapper:decode('Opt','Opt3',lists:flatten(Bytes33)),
    
    ?line {ok,Bytes34} = asn1_wrapper:encode('Opt','Opt3',#'Opt3'{bool32 = false}),
    ?line {ok,{'Opt3',asn1_NOVALUE,asn1_NOVALUE,false,asn1_NOVALUE}} = 
	asn1_wrapper:decode('Opt','Opt3',lists:flatten(Bytes34)),
    
    ?line {ok,Bytes35} = asn1_wrapper:encode('Opt','Opt3',#'Opt3'{bool33 = false}),
    ?line {ok,{'Opt3',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,false}} = 
	asn1_wrapper:decode('Opt','Opt3',lists:flatten(Bytes35)),
    
    
    
    ok.
