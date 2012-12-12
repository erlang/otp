%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
-module(testChoExternal).


-export([compile/3]).
-export([external/1]).

-include_lib("test_server/include/test_server.hrl").
-include("External.hrl").



compile(Config, Rules, Optimize) ->
    DataDir = ?config(data_dir, Config),
    CaseDir = ?config(case_dir, Config),
    true = code:add_patha(CaseDir),
    ok = asn1ct:compile(DataDir ++ "ChoExternal",
                        [Rules, {outdir, CaseDir}] ++ Optimize).



external(_Rules) ->
    roundtrip('ChoXCho', {boolCho,true}),
    roundtrip('ChoXCho', {intCho,77}),

    roundtrip('ChoXBool', {xbool,true}),
    roundtrip('ChoXBool', {xboolImp,true}),
    roundtrip('ChoXBool', {xboolExp,true}),

    roundtrip('NT', {os,"kalle"}),
    roundtrip('Exp', {os,"kalle"}),
    roundtrip('NTNT', {os,"kalle"}),
    roundtrip('NTExp', {os,"kalle"}),
    roundtrip('ExpNT', {os,"kalle"}),
    roundtrip('ExpExp', {os,"kalle"}),
    roundtrip('XNTNT', {os,"kalle"}),
    roundtrip('XNTExp', {os,"kalle"}),
    roundtrip('XExpNT', {os,"kalle"}),
    roundtrip('XExpExp', {os,"kalle"}),
    
    ok.

roundtrip(Type, Value) ->
    {ok,Encoded} = 'ChoExternal':encode(Type, Value),
    {ok,Value} = 'ChoExternal':decode(Type, Encoded),
    ok.
