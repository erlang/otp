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
-module(testSetOfCho).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SetChoDef',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetChoOpt',{bool1, int1, set1 = asn1_NOVALUE}).
-record('SetChoEmbDef',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetChoEmbOpt',{bool1, int1, set1 = asn1_NOVALUE}).
-record('SetOfChoEmbDef_SETOF',{bool1, int1, set1 = asn1_DEFAULT}).
-record('SetOfChoEmbOpt_SETOF',{bool1, int1, set1 = asn1_NOVALUE}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetOfCho",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetOfCho','SetChoDef',#'SetChoDef'{bool1 = true,
							  int1 = 17}),
    ?line {ok,{'SetChoDef',true,17,[]}} = 
	asn1_wrapper:decode('SetOfCho','SetChoDef',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('SetOfCho','SetChoDef',#'SetChoDef'{bool1 = true,
							  int1 = 17,
							  set1 = [{boolIn,true},
								  {intIn,25}]}),
    ?line {ok,{'SetChoDef',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SetOfCho','SetChoDef',lists:flatten(Bytes12)),
    
    
    
    ?line {ok,Bytes15} = 
	asn1_wrapper:encode('SetOfCho','SetChoOpt',#'SetChoOpt'{bool1 = true,
							  int1 = 17}),
    ?line {ok,{'SetChoOpt',true,17,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SetOfCho','SetChoOpt',lists:flatten(Bytes15)),
    
    
    ?line {ok,Bytes16} = 
	asn1_wrapper:encode('SetOfCho','SetChoOpt',#'SetChoOpt'{bool1 = true,
							  int1 = 17,
							  set1 = [{boolIn,true},
								  {intIn,25}]}),
    ?line {ok,{'SetChoOpt',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SetOfCho','SetChoOpt',lists:flatten(Bytes16)),
    
    
    
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetOfCho','SetChoEmbDef',#'SetChoEmbDef'{bool1 = true,
								int1 = 17}),
    ?line {ok,{'SetChoEmbDef',true,17,[]}} = 
	asn1_wrapper:decode('SetOfCho','SetChoEmbDef',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SetOfCho','SetChoEmbDef',#'SetChoEmbDef'{bool1 = true,
								int1 = 17,
								set1 = [{boolIn,true},
									{intIn,25}]}),
    ?line {ok,{'SetChoEmbDef',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SetOfCho','SetChoEmbDef',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes25} = 
	asn1_wrapper:encode('SetOfCho','SetChoEmbOpt',#'SetChoEmbOpt'{bool1 = true,
								int1 = 17}),
    ?line {ok,{'SetChoEmbOpt',true,17,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SetOfCho','SetChoEmbOpt',lists:flatten(Bytes25)),
    
    
    ?line {ok,Bytes26} = 
	asn1_wrapper:encode('SetOfCho','SetChoEmbOpt',#'SetChoEmbOpt'{bool1 = true,
								int1 = 17,
								set1 = [{boolIn,true},
									{intIn,25}]}),
    ?line {ok,{'SetChoEmbOpt',true,17,[{boolIn,true},{intIn,25}]}} = 
	asn1_wrapper:decode('SetOfCho','SetChoEmbOpt',lists:flatten(Bytes26)),
    
    
    
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetOfCho','SetOfChoEmbDef',[#'SetOfChoEmbDef_SETOF'{bool1 = true,
									   int1 = 17}]),
    ?line {ok,[{'SetOfChoEmbDef_SETOF',true,17,[]}]} = 
	asn1_wrapper:decode('SetOfCho','SetOfChoEmbDef',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SetOfCho','SetOfChoEmbDef',
		      [#'SetOfChoEmbDef_SETOF'{bool1 = true,
					       int1 = 17,
					       set1 = [{boolIn,true},
						       {intIn,25}]}]),
    ?line {ok,[{'SetOfChoEmbDef_SETOF',true,17,[{boolIn,true},{intIn,25}]}]} = 
	asn1_wrapper:decode('SetOfCho','SetOfChoEmbDef',lists:flatten(Bytes32)),
    
    
    
    ?line {ok,Bytes35} = 
	asn1_wrapper:encode('SetOfCho','SetOfChoEmbOpt',[#'SetOfChoEmbOpt_SETOF'{bool1 = true,
									   int1 = 17}]),
    ?line {ok,[{'SetOfChoEmbOpt_SETOF',true,17,asn1_NOVALUE}]} = 
	asn1_wrapper:decode('SetOfCho','SetOfChoEmbOpt',lists:flatten(Bytes35)),
    
    
    ?line {ok,Bytes36} = 
	asn1_wrapper:encode('SetOfCho','SetOfChoEmbOpt',
		      [#'SetOfChoEmbOpt_SETOF'{bool1 = true,
					       int1 = 17,
					       set1 = [{boolIn,true},
						       {intIn,25}]}]),
    ?line {ok,[{'SetOfChoEmbOpt_SETOF',true,17,[{boolIn,true},{intIn,25}]}]} = 
	asn1_wrapper:decode('SetOfCho','SetOfChoEmbOpt',lists:flatten(Bytes36)),
    
    
    
    
    ok.


