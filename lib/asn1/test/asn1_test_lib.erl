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
	 parallel/0,
	 roundtrip/3,roundtrip/4,roundtrip_enc/3,roundtrip_enc/4]).

-include_lib("test_server/include/test_server.hrl").

run_dialyzer() ->
    false.

compile(File, Config, Options) -> compile_all([File], Config, Options).

compile_all(Files, Config, Options) ->
    DataDir = ?config(data_dir, Config),
    CaseDir = ?config(case_dir, Config),
    [compile_file(filename:join(DataDir, F), [{outdir, CaseDir},
					      debug_info|Options])
         || F <- Files],
    dialyze(Files, Options),
    ok.

parallel() ->
    case erlang:system_info(schedulers) > 1 andalso not run_dialyzer() of
        true  -> [parallel];
        false -> []
    end.

dialyze(Files, Options) ->
    case not run_dialyzer() orelse lists:member(abs, Options) of
	true -> ok;
	false -> dialyze(Files)
    end.

dialyze(Files) ->
    Beams0 = [code:which(module(F)) || F <- Files],
    Beams = [code:which(asn1rt_nif)|Beams0],
    case dialyzer:run([{files,Beams},
		       {warnings,[no_improper_lists]},
		       {get_warnings,true}]) of
	[] ->
	    ok;
	[_|_]=Ws ->
	    io:put_chars([[B,$\n] || B <- Beams]),
	    io:put_chars([dialyzer:format_warning(W) || W <- Ws]),
	    error(dialyzer_warnings)
    end.

module(F0) ->
    F1 = filename:basename(F0),
    F2 = case filename:extension(F1) of
	     ".asn" ->
		 filename:rootname(F1);
	     ".asn1" ->
		 filename:rootname(F1);
	     ".py" ->
		 filename:rootname(F1);
	     "" ->
		 F1
	 end,
    F = case filename:extension(F2) of
	    ".set" ->
		filename:rootname(F2);
	    "" ->
		F2
	end,
    list_to_atom(F).
%%    filename:join(CaseDir, F ++ ".beam").

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
    test_ber_indefinite(Mod, Type, Encoded, ExpectedValue),
    ok.

roundtrip_enc(Mod, Type, Value) ->
    roundtrip_enc(Mod, Type, Value, Value).

roundtrip_enc(Mod, Type, Value, ExpectedValue) ->
    {ok,Encoded} = Mod:encode(Type, Value),
    {ok,ExpectedValue} = Mod:decode(Type, Encoded),
    test_ber_indefinite(Mod, Type, Encoded, ExpectedValue),
    Encoded.

%%%
%%% Internal functions.
%%%

hex2num(C) when $0 =< C, C =< $9 -> C - $0;
hex2num(C) when $A =< C, C =< $F -> C - $A + 10;
hex2num(C) when $a =< C, C =< $f -> C - $a + 10.

test_ber_indefinite(Mod, Type, Encoded, ExpectedValue) ->
    case Mod:encoding_rule() of
	ber ->
	    Indefinite = iolist_to_binary(ber_indefinite(Encoded)),
	    {ok,ExpectedValue} = Mod:decode(Type, Indefinite);
	_ ->
	    ok
    end.

%% Rewrite all definite lengths for constructed values to an
%% indefinite length.
ber_indefinite(Bin0) ->
    case ber_get_tag(Bin0) of
	done ->
	    [];
	primitive ->
	    Bin0;
	{constructed,Tag,Bin1} ->
	    {Len,Bin2} = ber_get_len(Bin1),
	    <<Val0:Len/binary,Bin/binary>> = Bin2,
	    Val = iolist_to_binary(ber_indefinite(Val0)),
	    [<<Tag/binary,16#80,Val/binary,0,0>>|ber_indefinite(Bin)]
    end.

ber_get_tag(<<>>) ->
    done;
ber_get_tag(<<_:2,0:1,_:5,_/binary>>) ->
    primitive;
ber_get_tag(<<_:2,1:1,_:5,_/binary>>=Bin0) ->
    TagLen = ber_tag_length(Bin0),
    <<Tag:TagLen/binary,Bin/binary>> = Bin0,
    {constructed,Tag,Bin}.

ber_tag_length(<<_:3,2#11111:5,T/binary>>) ->
    ber_tag_length_1(T, 1);
ber_tag_length(_) ->
    1.

ber_tag_length_1(<<1:1,_:7,T/binary>>, N) ->
    ber_tag_length_1(T, N+1);
ber_tag_length_1(<<0:1,_:7,_/binary>>, N) ->
    N+1.

ber_get_len(<<0:1,L:7,T/binary>>) ->
    {L,T};
ber_get_len(<<1:1,Octets:7,T0/binary>>) ->
    <<L:Octets/unit:8,T/binary>> = T0,
    {L,T}.
