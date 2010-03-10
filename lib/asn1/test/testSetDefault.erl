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
-module(testSetDefault).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SetDef1',{bool1 = asn1_DEFAULT, int1, set1 = asn1_DEFAULT}).
-record('SetDef2',{set2 = asn1_DEFAULT, bool2, int2}).
-record('SetDef3',{bool3 = asn1_DEFAULT, set3 = asn1_DEFAULT, int3 = asn1_DEFAULT}).
-record('SetIn',{boolIn, intIn}).



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetDefault",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetDefault','SetDef1',#'SetDef1'{bool1 = true,
							int1 = 15,
							set1 = #'SetIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SetDef1',true,15,{'SetIn',true,66}}} = 
	asn1_wrapper:decode('SetDefault','SetDef1',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('SetDefault','SetDef1',#'SetDef1'{int1 = 15}), 
    ?line {ok,{'SetDef1',true,15,{'SetIn',asn1_NOVALUE,12}}} = 
	asn1_wrapper:decode('SetDefault','SetDef1',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetDefault','SetDef2',#'SetDef2'{bool2 = true,
							int2 = 15,
							set2 = #'SetIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SetDef2',{'SetIn',true,66},true,15}} = 
	asn1_wrapper:decode('SetDefault','SetDef2',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('SetDefault','SetDef2',#'SetDef2'{bool2 = true,
									 int2 = 15}), 
    ?line {ok,{'SetDef2',{'SetIn',asn1_NOVALUE,12},true,15}} = 
	asn1_wrapper:decode('SetDefault','SetDef2',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetDefault','SetDef3',#'SetDef3'{bool3 = true,
							int3 = 15,
							set3 = #'SetIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SetDef3',true,{'SetIn',true,66},15}} = 
	asn1_wrapper:decode('SetDefault','SetDef3',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('SetDefault','SetDef3',#'SetDef3'{int3 = 15}), 
    ?line {ok,{'SetDef3',true,{'SetIn',asn1_NOVALUE,12},15}} = 
	asn1_wrapper:decode('SetDefault','SetDef3',lists:flatten(Bytes32)),
    
    
    
    

    
    
    ok.
