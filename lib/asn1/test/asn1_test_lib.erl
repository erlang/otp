%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(asn1_test_lib).

-export([compile/3,compile_all/3,compile_erlang/3,
	 rm_dirs/1,
	 hex_to_bin/1,
	 match_value/2,
	 parallel/0,
	 roundtrip/3,roundtrip/4,roundtrip_enc/3,roundtrip_enc/4,
         map_roundtrip/3]).

-include_lib("common_test/include/ct.hrl").

run_dialyzer() ->
    false.

compile(File, Config, Options) -> compile_all([File], Config, Options).

compile_all(Files, Config, Options0) ->
    DataDir = proplists:get_value(data_dir, Config),
    CaseDir = proplists:get_value(case_dir, Config),
    Options = [{outdir,CaseDir},debug_info|Options0],

    Comp = fun(F) ->
		   compile_file(filename:join(DataDir, F), Options)
	   end,
    p_run(Comp, Files),

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
		       {warnings,[]},
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

compile_file(File, Options0) ->
    Options = [warnings_as_errors|Options0],
    try
        ok = asn1ct:compile(File, Options),
        ok = compile_maps(File, Options)
    catch
        _:Reason ->
	    ct:print("Failed to compile ~s\n~p", [File,Reason]),
            error
    end.

compile_maps(File, Options) ->
    unload_map_mod(File),
    Incompat = [abs,compact_bit_string,legacy_bit_string,
                legacy_erlang_types,maps,asn1_test_lib_no_maps],
    case lists:any(fun(E) -> lists:member(E, Incompat) end, Options) of
        true ->
            ok;
        false ->
            compile_maps_1(File, Options)
    end.

compile_maps_1(File, Options) ->
    ok = asn1ct:compile(File, [maps,no_ok_wrapper,noobj|Options]),
    OutDir = proplists:get_value(outdir, Options),
    Base0 = filename:rootname(filename:basename(File)),
    Base = case filename:extension(Base0) of
               ".set" ->
                   filename:rootname(Base0);
               _ ->
                   Base0
           end,
    ErlBase = Base ++ ".erl",
    ErlFile = filename:join(OutDir, ErlBase),
    {ok,Erl0} = file:read_file(ErlFile),
    Erl = re:replace(Erl0, <<"-module\\('">>, "&maps_"),
    MapsErlFile = filename:join(OutDir, "maps_" ++ ErlBase),
    ok = file:write_file(MapsErlFile, Erl),
    {ok,_} = compile:file(MapsErlFile, [report,{outdir,OutDir},{i,OutDir}]),
    ok.

unload_map_mod(File0) ->
    File1 = filename:basename(File0),
    File2 = filename:rootname(File1, ".asn"),
    File3 = filename:rootname(File2, ".asn1"),
    File4 = filename:rootname(File3, ".py"),
    File = filename:rootname(File4, ".set"),
    MapMod = list_to_atom("maps_"++File),
    code:delete(MapMod),
    code:purge(MapMod),
    ok.

compile_erlang(Mod, Config, Options) ->
    DataDir = proplists:get_value(data_dir, Config),
    CaseDir = proplists:get_value(case_dir, Config),
    M = list_to_atom(Mod),
    {ok, M} = compile:file(filename:join(DataDir, Mod),
                           [report,{i,CaseDir},{outdir,CaseDir}|Options]).

rm_dirs([Dir|Dirs]) ->
    {ok,L0} = file:list_dir(Dir),
    L = [filename:join(Dir, F) || F <- L0],
    IsDir = fun(F) -> filelib:is_dir(F) end,
    {Subdirs,Files} = lists:partition(IsDir, L),
    _ = [ok = file:delete(F) || F <- Files],
    rm_dirs(Subdirs),
    ok = file:del_dir(Dir),
    rm_dirs(Dirs);
rm_dirs([]) ->
    ok.

hex_to_bin(S) ->
    << <<(hex2num(C)):4>> || C <- S, C =/= $\s >>.

%% match_value(Pattern, Value) -> ok.
%%  Match Pattern against Value. If the Pattern contains in any
%%  position, the corresponding position in the Value can be
%%  anything. Generate an exception if the Pattern and Value don't
%%  match.

match_value('_', _) ->
    ok;
match_value([H1|T1], [H2|T2]) ->
    match_value(H1, H2),
    match_value(T1, T2);
match_value(T1, T2) when tuple_size(T1) =:= tuple_size(T2) ->
    match_value_tuple(1, T1, T2);
match_value(Same, Same) ->
    ok;
match_value(V1, V2) ->
    error({nomatch,V1,V2}).

roundtrip(Mod, Type, Value) ->
    roundtrip(Mod, Type, Value, Value).

roundtrip(Mod, Type, Value, ExpectedValue) ->
    roundtrip_enc(Mod, Type, Value, ExpectedValue).

roundtrip_enc(Mod, Type, Value) ->
    roundtrip_enc(Mod, Type, Value, Value).

roundtrip_enc(Mod, Type, Value, ExpectedValue) ->
    case Mod:encode(Type, Value) of
        {ok,Encoded} ->
            {ok,ExpectedValue} = Mod:decode(Type, Encoded);
        Encoded when is_binary(Encoded) ->
            ExpectedValue = Mod:decode(Type, Encoded)
    end,
    map_roundtrip(Mod, Type, Encoded),
    test_ber_indefinite(Mod, Type, Encoded, ExpectedValue),
    Encoded.

map_roundtrip(Mod, Type, Encoded) ->
    MapMod = list_to_atom("maps_"++atom_to_list(Mod)),
    try MapMod:maps() of
        true ->
            map_roundtrip_1(MapMod, Type, Encoded)
    catch
        error:undef ->
            ok
    end.

%%%
%%% Internal functions.
%%%

map_roundtrip_1(Mod, Type, Encoded) ->
    Decoded = Mod:decode(Type, Encoded),
    case Mod:encode(Type, Decoded) of
        Encoded ->
            ok;
        OtherEncoding ->
            case is_named_bitstring(Decoded) of
                true ->
                    %% In BER, named BIT STRINGs with different number of
                    %% trailing zeroes decode to the same value.
                    ok;
                false ->
                    error({encode_mismatch,Decoded,Encoded,OtherEncoding})
            end
    end,
    ok.

is_named_bitstring([H|T]) ->
    is_atom(H) andalso is_named_bitstring(T);
is_named_bitstring([]) ->
    true;
is_named_bitstring(_) ->
    false.

hex2num(C) when $0 =< C, C =< $9 -> C - $0;
hex2num(C) when $A =< C, C =< $F -> C - $A + 10;
hex2num(C) when $a =< C, C =< $f -> C - $a + 10.

match_value_tuple(I, T1, T2) when I =< tuple_size(T1) ->
    match_value(element(I, T1), element(I, T2)),
    match_value_tuple(I+1, T1, T2);
match_value_tuple(_, _, _) ->
    ok.

test_ber_indefinite(Mod, Type, Encoded, ExpectedValue) ->
    case Mod:encoding_rule() of
	ber ->
	    Indefinite = iolist_to_binary(ber_indefinite(Encoded)),
            case Mod:decode(Type, Indefinite) of
                {ok,ExpectedValue} ->
                    ok;
                ExpectedValue ->
                    ok
            end;
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

%% p_run(fun(Data) -> ok|error, List) -> ok
%%  Will fail the test case if there were any errors.

p_run(Test, List) ->
    S = erlang:system_info(schedulers),
    N = case test_server:is_cover() of
	    false ->
		S + 1;
	    true ->
		%% Cover is running. Using too many processes
		%% could slow us down.
		min(S, 4)
	end,
    %%io:format("p_run: ~p parallel processes\n", [N]),
    p_run_loop(Test, List, N, [], 0).

p_run_loop(_, [], _, [], Errors) ->
    case Errors of
	0 -> ok;
	N -> ct:fail({N,errors})
    end;
p_run_loop(Test, [H|T], N, Refs, Errors) when length(Refs) < N ->
    {_,Ref} = erlang:spawn_monitor(fun() -> exit(Test(H)) end),
    p_run_loop(Test, T, N, [Ref|Refs], Errors);
p_run_loop(Test, List, N, Refs0, Errors0) ->
    receive
	{'DOWN',Ref,process,_,Res} ->
	    Errors = case Res of
			 ok -> Errors0;
			 error -> Errors0+1
		     end,
	    Refs = Refs0 -- [Ref],
	    p_run_loop(Test, List, N, Refs, Errors)
    end.
