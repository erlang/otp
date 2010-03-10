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
-module(testChoTypeRefSet).

-export([compile/3]).
-export([set/1]).

-include_lib("test_server/include/test_server.hrl").

-record('ChoSet',{setInt, setOs}).
-record('ChoSetImp',{setInt, setOs}).
-record('ChoSetExp',{setInt, setOs}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoTypeRefSet",[Rules,{outdir,OutDir}]++Options).



set(_Rules) ->

    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {choSet,#'ChoSet'{setInt = 88,
					setOs = "A string"}}),
    ?line {ok,{choSet,{'ChoSet',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes1)),
    
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {choSetI,#'ChoSet'{setInt = 88,
					 setOs = "A string"}}),
    ?line {ok,{choSetI,{'ChoSet',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes2)),
    
    
    ?line {ok,Bytes3} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {choSetE,#'ChoSet'{setInt = 88,
					 setOs = "A string"}}),
    ?line {ok,{choSetE,{'ChoSet',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes3)),
    
    
    ?line {ok,Bytes4} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {'choSet-I',#'ChoSetImp'{setInt = 88,
					       setOs = "A string"}}),
    ?line {ok,{'choSet-I',{'ChoSetImp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes4)),
    
    
    ?line {ok,Bytes5} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {'choSetI-I',#'ChoSetImp'{setInt = 88,
						setOs = "A string"}}),
    ?line {ok,{'choSetI-I',{'ChoSetImp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes5)),
    
    
    ?line {ok,Bytes6} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {'choSetE-I',#'ChoSetImp'{setInt = 88,
						setOs = "A string"}}),
    ?line {ok,{'choSetE-I',{'ChoSetImp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes6)),
    
    
    ?line {ok,Bytes7} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {'choSet-E',#'ChoSetExp'{setInt = 88,
					       setOs = "A string"}}),
    ?line {ok,{'choSet-E',{'ChoSetExp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes7)),
    
    
    ?line {ok,Bytes8} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {'choSetI-E',#'ChoSetExp'{setInt = 88,
						setOs = "A string"}}),
    ?line {ok,{'choSetI-E',{'ChoSetExp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes8)),
    
    
    ?line {ok,Bytes9} = 
	asn1_wrapper:encode('ChoTypeRefSet','ChoTRset',
		      {'choSetE-E',#'ChoSetExp'{setInt = 88,
						setOs = "A string"}}),
    ?line {ok,{'choSetE-E',{'ChoSetExp',88,"A string"}}} = 
	asn1_wrapper:decode('ChoTypeRefSet','ChoTRset',lists:flatten(Bytes9)),
    
    
    
    ok.
