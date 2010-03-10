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
-module(testSeqExtension).

-include("External.hrl").
-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqExt1',{}).
-record('SeqExt2',{bool, int}).
-record('SeqExt3',{bool, int}).
-record('SeqExt4',{bool, int}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqExtension",
			      [Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SeqExtension','SeqExt1',#'SeqExt1'{}), 
    ?line {ok,{'SeqExt1'}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt1',lists:flatten(Bytes11)),

    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SeqExtension','SeqExt2',#'SeqExt2'{bool = true,int = 99}), 
    ?line {ok,{'SeqExt2',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt2',lists:flatten(Bytes21)),

    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('SeqExtension','SeqExt2',#'SeqExt2'{int = 99,bool = true}), 
    ?line {ok,{'SeqExt2',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt2',lists:flatten(Bytes22)),

    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SeqExtension','SeqExt3',#'SeqExt3'{bool = true,int = 99}), 
    ?line {ok,{'SeqExt3',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt3',lists:flatten(Bytes31)),

    ?line {ok,Bytes32} = 
	asn1_wrapper:encode('SeqExtension','SeqExt3',#'SeqExt3'{int = 99,bool = true}), 
    ?line {ok,{'SeqExt3',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt3',lists:flatten(Bytes32)),

    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SeqExtension','SeqExt4',#'SeqExt4'{bool = true,int = 99}), 
    ?line {ok,{'SeqExt4',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt4',lists:flatten(Bytes41)),

    ?line {ok,Bytes42} = 
	asn1_wrapper:encode('SeqExtension','SeqExt4',#'SeqExt4'{int = 99,bool = true}), 
    ?line {ok,{'SeqExt4',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt4',lists:flatten(Bytes42)),


    % test of extension , not ready
    
     ?line {ok,BytesX11} = 
	asn1_wrapper:encode('SeqExtension','SeqExt1',#'SeqExt1'{}), 
    ?line {ok,{'SeqExt1'}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt1',lists:flatten(BytesX11)),

    ?line {ok,BytesX21} = 
	asn1_wrapper:encode('SeqExtension','SeqExt2',#'SeqExt2'{bool = true,int = 99}), 
    ?line {ok,{'SeqExt2',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt2',lists:flatten(BytesX21)),

    ?line {ok,BytesX22} = 
	asn1_wrapper:encode('SeqExtension','SeqExt2',#'SeqExt2'{int = 99,bool = true}), 
    ?line {ok,{'SeqExt2',true,99}} = 
	asn1_wrapper:decode('SeqExtension','SeqExt2',lists:flatten(BytesX22)),

   
    
     

    ok.





