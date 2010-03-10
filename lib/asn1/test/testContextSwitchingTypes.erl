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
-module(testContextSwitchingTypes).

-export([compile/3]).
-export([test/0]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ContextSwitchingTypes",
			      [Rules,{outdir,OutDir}]++Options).


test() ->
    ?line ValT = 'ContextSwitchingTypes':'val1-T'(),
    ?line {ok,Bytes1} =
	asn1_wrapper:encode('ContextSwitchingTypes','T',ValT),
    ?line {ok,Result1} = 
	asn1_wrapper:decode('ContextSwitchingTypes','T',Bytes1),
    ?line ok = check_EXTERNAL(Result1),
    ?line {ok,ValT2} = asn1ct:value('ContextSwitchingTypes','T'),
    ?line {ok,Bytes1_2} =
	asn1_wrapper:encode('ContextSwitchingTypes','T',ValT2),
    ?line {ok,Result1_2} = 
	asn1_wrapper:decode('ContextSwitchingTypes','T',Bytes1_2),
    ?line ok = check_EXTERNAL(Result1_2),

    ?line ValEP = 'ContextSwitchingTypes':'val1-EP'(),
    ?line {ok,Bytes2} =
	asn1_wrapper:encode('ContextSwitchingTypes','EP',ValEP),
    ?line {ok,_Result2} = 
	asn1_wrapper:decode('ContextSwitchingTypes','EP',Bytes2),

    ?line ValCS = 'ContextSwitchingTypes':'val1-CS'(),
    ?line {ok,Bytes3} =
	asn1_wrapper:encode('ContextSwitchingTypes','CS',ValCS),
    ?line {ok,_Result3} = 
	asn1_wrapper:decode('ContextSwitchingTypes','CS',Bytes3).


check_EXTERNAL({'EXTERNAL',Identif,DVD,DV})->
    ?line ok=check_EXTERNAL_Idef(Identif),
    ?line ok = check_EXTERNAL_DVD(DVD),
    ?line ok = check_EXTERNAL_DV(DV).
check_EXTERNAL_Idef({Alt,_}) when Alt=='context-negotiation';
				  Alt=='presentation-context-id';
				  Alt==syntax ->
    ok;
check_EXTERNAL_Idef(I) ->
    {error,"failed on identification alternative",I}.
check_EXTERNAL_DVD(DVD) when list(DVD) ->
    ok;
check_EXTERNAL_DVD(asn1_NOVALUE) ->
    ok;
check_EXTERNAL_DVD(DVD) ->
    {error,"failed on data-value-descriptor alternative",DVD}.
check_EXTERNAL_DV(DV) when list(DV) ->
    ok;
check_EXTERNAL_DV(DV) ->
    {error,"failed on data-value alternative",DV}.
