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

-module(testInfObjectClass).


-export([compile/3,main/1]).

-include_lib("test_server/include/test_server.hrl").




compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ErrorClass",[Rules,{outdir,OutDir}]++Options),
    ?line ok = asn1ct:compile(DataDir ++ "InfClass",[Rules,{outdir,OutDir}]++Options).


main(Rule) ->
    %% this test is added for OTP-4591, to test that elements in decoded
    %% value has terms in right order.
    Val = {'Seq',12,13,2},
    ?line {ok,Bytes}= asn1_wrapper:encode('InfClass','Seq',Val),
    ?line {ok,Val} = asn1_wrapper:decode('InfClass','Seq',Bytes),
    
    %% OTP-5783
    ?line {error,{asn1,{'Type not compatible with table constraint',
			{component,'ArgumentType'},
			{value,_},_}}} = asn1_wrapper:encode('InfClass','Seq',
						      {'Seq',12,13,1}),
    Bytes2 =
	if
	    Rule==per;Rule==per_bin ->
		[1,12,1,11,1,1];
	    Rule == uper_bin ->
		<<1,12,1,11,1,1>>;
	    true ->
		[48,9,2,1,12,2,1,11,2,1,1]
	end,
    ?line {error,{asn1,{'Type not compatible with table constraint',
			{{component,_},
			 {value,_B},_}}}} = 
	asn1_wrapper:decode('InfClass','Seq',Bytes2).


    
