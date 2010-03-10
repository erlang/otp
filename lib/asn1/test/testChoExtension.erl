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
-module(testChoExtension).

-export([compile/3]).
-export([extension/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoExtension",[Rules,{outdir,OutDir}] ++ Options).



extension(_Rules) ->

    ?line {ok,Bytes1} = asn1_wrapper:encode('ChoExtension','ChoExt1',{'ChoExt1',{bool,true}}),
    ?line {ok,{bool,true}} = 
	asn1_wrapper:decode('ChoExtension','ChoExt1',lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = asn1_wrapper:encode('ChoExtension','ChoExt1',{'ChoExt1',{int,33}}),
    ?line {ok,{int,33}} = 
	asn1_wrapper:decode('ChoExtension','ChoExt1',lists:flatten(Bytes2)),

    %% A trick to encode with another compatible CHOICE type to test reception
    %% extension alternative

    ?line {ok,Bytes2x} = asn1_wrapper:encode('ChoExtension','ChoExt1x',{str,"abc"}),
    ?line {ok,Val2x} = 
	asn1_wrapper:decode('ChoExtension','ChoExt1',lists:flatten(Bytes2x)),
    io:format("Choice extension alternative = ~p~n",[Val2x]),

    ?line {ok,Bytes3} = asn1_wrapper:encode('ChoExtension','ChoExt2',{'ChoExt2',{bool,true}}),
    ?line {ok,{bool,true}} =
	asn1_wrapper:decode('ChoExtension','ChoExt2',lists:flatten(Bytes3)),

    ?line {ok,Bytes4} = asn1_wrapper:encode('ChoExtension','ChoExt2',{'ChoExt2',{int,33}}),
    ?line {ok,{int,33}} = 
	asn1_wrapper:decode('ChoExtension','ChoExt2',lists:flatten(Bytes4)),

    ?line {ok,Bytes5} = asn1_wrapper:encode('ChoExtension','ChoExt3',{'ChoExt3',{bool,true}}),
    ?line {ok,{bool,true}} = 
	asn1_wrapper:decode('ChoExtension','ChoExt3',lists:flatten(Bytes5)),

    ?line {ok,Bytes6} = asn1_wrapper:encode('ChoExtension','ChoExt3',{'ChoExt3',{int,33}}),
    ?line {ok,{int,33}} = 
	asn1_wrapper:decode('ChoExtension','ChoExt3',lists:flatten(Bytes6)),

    Val7 = {str,"abc"},
    ?line {ok,Bytes7} = asn1_wrapper:encode('ChoExtension','ChoExt4',Val7),
    ?line {ok,Val7} = asn1_wrapper:decode('ChoExtension','ChoExt4',lists:flatten(Bytes7)),


    ok.
