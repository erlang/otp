%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-module(asn1_test_lib).

-export([compile/3,compile_all/3,compile_erlang/3,
	 hex_to_bin/1,
	 roundtrip/3,roundtrip/4,roundtrip_enc/3,roundtrip_enc/4]).

-include_lib("test_server/include/test_server.hrl").

compile(File, Config, Options) -> compile_all([File], Config, Options).

compile_all(Files, Config, Options) ->
    DataDir = ?config(data_dir, Config),
    CaseDir = ?config(case_dir, Config),
    [compile_file(filename:join(DataDir, F), [{outdir, CaseDir}|Options])
         || F <- Files],
    ok.

compile_file(File, Options) ->
    try
        ok = asn1ct:compile(File, [warnings_as_errors|Options])
    catch
        Class:Reason ->
	    ct:print("Failed to compile ~s\n", [File]),
            erlang:error({compile_failed, {File, Options}, {Class, Reason}})
    end.

compile_erlang(Mod, Config, Options) ->
    DataDir = ?config(data_dir, Config),
    CaseDir = ?config(case_dir, Config),
    M = list_to_atom(Mod),
    {ok, M} = compile:file(filename:join(DataDir, Mod),
                           [report,{i,CaseDir},{outdir,CaseDir}|Options]).

hex_to_bin(S) ->
    << <<(hex2num(C)):4>> || C <- S, C =/= $\s >>.

roundtrip(Mod, Type, Value) ->
    roundtrip(Mod, Type, Value, Value).

roundtrip(Mod, Type, Value, ExpectedValue) ->
    {ok,Encoded} = Mod:encode(Type, Value),
    {ok,ExpectedValue} = Mod:decode(Type, Encoded),
    ok.

roundtrip_enc(Mod, Type, Value) ->
    roundtrip_enc(Mod, Type, Value, Value).

roundtrip_enc(Mod, Type, Value, ExpectedValue) ->
    {ok,Encoded} = Mod:encode(Type, Value),
    {ok,ExpectedValue} = Mod:decode(Type, Encoded),
    Encoded.

%%%
%%% Internal functions.
%%%

hex2num(C) when $0 =< C, C =< $9 -> C - $0;
hex2num(C) when $A =< C, C =< $F -> C - $A + 10;
hex2num(C) when $a =< C, C =< $f -> C - $a + 10.
