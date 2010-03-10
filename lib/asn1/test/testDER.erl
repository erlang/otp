%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
-module(testDER).

-export([compile/3]).
-export([test/0]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rule,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "DERSpec",
			      [Rule,der,{outdir,OutDir}]++Options).

test() ->
    Val = {'Set',12,{version,214},true},
    ?line {ok,Bin}=asn1_wrapper:encode('DERSpec','Set',Val),          
    ?line ok = match_value('Set',Bin),
    ?line {ok,{'Set',12,{version,214},true}} =
	asn1_wrapper:decode('DERSpec','Set',Bin),
    
    ValSof = [{version,12},{message,"PrintableString"},{message,"Print"},{version,11}],
    ?line {ok,BSof} = asn1_wrapper:encode('DERSpec','SetOf',ValSof),
    ?line ok = match_value('SetOf',BSof),
    ?line {ok,[{version,11},{version,12},{message,"Print"},{message,"PrintableString"}]} = asn1_wrapper:decode('DERSpec','SetOf',BSof),

    ValSO = [{'Seq2',1,true},{'Seq2',120000,false},{'Seq2',3,true}],
    ?line {ok,SOB} =  asn1_wrapper:encode('DERSpec','SO',ValSO),
    ?line {ok,ValSO} = asn1_wrapper:decode('DERSpec','SO',SOB).


match_value('Set',<<49,12,1,1,255,2,2,0,214,161,3,2,1,12>>) ->
    ok;
match_value('Set',[49,12,1,1,255,2,2,0,214,161,3,2,1,12]) ->
    ok;
match_value('SetOf',<<49,30,2,1,11,2,1,12,19,5,80,114,105,110,116,19,15,80,114,105,110,116,97,98,108,101,83,116,114,105,110,103>>) -> ok;
match_value('SetOf',[49,30,2,1,11,2,1,12,19,5,80,114,105,110,116,19,15,80,114,105,110,116,97,98,108,101,83,116,114,105,110,103]) -> ok;
match_value(_,B) ->
    {error,B}.
