%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(beam_lib_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), "./log_dir/").
-define(t,test_server).
-define(privdir, "beam_lib_SUITE_priv").
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(privdir, proplists:get_value(priv_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 normal/1, error/1, cmp/1, cmp_literals/1, strip/1, otp_6711/1,
         building/1, md5/1, encrypted_abstr/1, encrypted_abstr_file/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [error, normal, cmp, cmp_literals, strip, otp_6711,
     building, md5, encrypted_abstr, encrypted_abstr_file].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Read correct beam file.
normal(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,
    Simple = filename:join(PrivDir, "simple"),
    Source = Simple ++ ".erl",
    BeamFile = Simple ++ ".beam",
    simple_file(Source),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    do_normal(Source, PrivDir, BeamFile, []),
    do_normal(Source, PrivDir, BeamFile, [no_utf8_atoms]),

    {ok,_} = compile:file(Source, [{outdir,PrivDir}, no_debug_info]),
    {ok, {simple, [{debug_info, {debug_info_v1, erl_abstract_code, {none, _}}}]}} =
	beam_lib:chunks(BeamFile, [debug_info]),
    {ok, {simple, [{abstract_code, no_abstract_code}]}} =
	beam_lib:chunks(BeamFile, [abstract_code]),

    %% {ok,_} = compile:file(Source, [compressed | CompileFlags]),
    %% do_normal(BeamFile),

    file:delete(BeamFile),
    file:delete(Source),
    NoOfTables = length(ets:all()),
    true = (P0 == pps()),
    ok.

do_normal(Source, PrivDir, BeamFile, Opts) ->
    CompileFlags = [{outdir,PrivDir}, debug_info | Opts],
    {ok,_} = compile:file(Source, CompileFlags),
    {ok, Binary} = file:read_file(BeamFile),

    do_normal(BeamFile, Opts),
    do_normal(Binary, Opts).

do_normal(BeamFile, Opts) ->
    Imports = {imports, [{erlang, get_module_info, 1},
			 {erlang, get_module_info, 2},
			 {lists, member, 2}]},
    Exports = {exports, [{module_info, 0}, {module_info, 1}, {t, 0}]},
    Local = {locals, [{t, 1}]},
    {ok, {simple, [Imports]}} = beam_lib:chunks(BeamFile, [imports]),
    {ok, {simple, [{"ImpT",_Bin}]}} =
	beam_lib:chunks(BeamFile, ["ImpT"]),
    {ok, {simple, [Exports]}} = beam_lib:chunks(BeamFile, [exports]),
    {ok, {simple, [{attributes, [{vsn, [_]}]}]}} =
	beam_lib:chunks(BeamFile, [attributes]),
    {ok, {simple, [{compile_info, _}=CompileInfo]}} =
	beam_lib:chunks(BeamFile, [compile_info]),
    {ok, {simple, [Local]}} = beam_lib:chunks(BeamFile, [locals]),
    {ok, {simple, [{attributes, [{vsn, [_]}]}, CompileInfo,
		   Exports, Imports, Local]}} =
	beam_lib:chunks(BeamFile, [attributes, compile_info, exports, imports, locals]),
    {ok, {simple, [{atoms, _Atoms}]}} =
	beam_lib:chunks(BeamFile, [atoms]),
    {ok, {simple, [{labeled_exports, _LExports}]}} =
	beam_lib:chunks(BeamFile, [labeled_exports]),
    {ok, {simple, [{labeled_locals, _LLocals}]}} =
	beam_lib:chunks(BeamFile, [labeled_locals]),
    {ok, {simple, [_Vsn]}} = beam_lib:version(BeamFile),
    {ok, {simple, [{abstract_code, {_, _}}]}} =
	beam_lib:chunks(BeamFile, [abstract_code]),
    {ok, {simple, [{debug_info, {debug_info_v1, erl_abstract_code, _}}]}} =
	beam_lib:chunks(BeamFile, [debug_info]),

    %% Test reading optional chunks.
    All = ["Atom", "Code", "StrT", "ImpT", "ExpT", "FunT", "LitT", "AtU8"],
    {ok,{simple,Chunks}} = beam_lib:chunks(BeamFile, All, [allow_missing_chunks]),
    case {verify_simple(Chunks),Opts} of
	{{missing_chunk, AtomBin}, []} when is_binary(AtomBin) -> ok;
	{{AtomBin, missing_chunk}, [no_utf8_atoms]} when is_binary(AtomBin) -> ok
    end,

    %% Make sure that reading the atom chunk works when the 'allow_missing_chunks'
    %% option is used.
    Some = ["Code",atoms,"ExpT","LitT"],
    {ok,{simple,SomeChunks}} = beam_lib:chunks(BeamFile, Some, [allow_missing_chunks]),
    [{"Code",<<_/binary>>},{atoms,[_|_]},{"ExpT",<<_/binary>>},{"LitT",missing_chunk}] =
	SomeChunks.

verify_simple([{"Atom", PlainAtomChunk},
	       {"Code", CodeBin},
	       {"StrT", StrBin},
	       {"ImpT", ImpBin},
	       {"ExpT", ExpBin},
	       {"FunT", missing_chunk},
	       {"LitT", missing_chunk},
	       {"AtU8", AtU8Chunk}])
  when is_binary(CodeBin), is_binary(StrBin),
       is_binary(ImpBin), is_binary(ExpBin) ->
    {PlainAtomChunk, AtU8Chunk}.

%% Read invalid beam files.
error(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,
    Simple = filename:join(PrivDir, "simple"),
    Source = Simple ++ ".erl",
    BeamFile = Simple ++ ".beam",
    WrongFile = Simple ++ "foo.beam",
    simple_file(Source),

    NoOfTables = length(ets:all()),
    P0 = pps(),
    {ok,_} = compile:file(Source, [{outdir,PrivDir},debug_info]),
    ACopy = filename:join(PrivDir, "a_copy.beam"),
    copy_file(BeamFile, ACopy),

    {ok, Binary} = file:read_file(BeamFile),

    copy_file(ACopy, WrongFile),
    verify(file_error, beam_lib:info("./does_simply_not_exist")),

    do_error(BeamFile, ACopy),
    do_error(Binary, ACopy),

    copy_file(ACopy, BeamFile),
    verify(unknown_chunk, beam_lib:chunks(BeamFile, [not_a_chunk])),

    ok = file:write_file(BeamFile, <<>>),
    verify(not_a_beam_file, beam_lib:info(BeamFile)),
    verify(not_a_beam_file, beam_lib:info(<<>>)),
    ok = file:write_file(BeamFile, <<"short">>),
    verify(not_a_beam_file, beam_lib:info(BeamFile)),
    verify(not_a_beam_file, beam_lib:info(<<"short">>)),

    {Binary1, _} = split_binary(Binary, byte_size(Binary)-10),
    LastChunk = last_chunk(Binary),
    verify(chunk_too_big, beam_lib:chunks(Binary1, [LastChunk])),
    Chunks = chunk_info(Binary),
    {value, {_, DebugInfoStart, _}} = lists:keysearch("Dbgi", 1, Chunks),
    {Binary2, _} = split_binary(Binary, DebugInfoStart),
    verify(chunk_too_big, beam_lib:chunks(Binary2, ["Dbgi"])),
    {Binary3, _} = split_binary(Binary, DebugInfoStart-4),
    verify(invalid_beam_file, beam_lib:chunks(Binary3, ["Dbgi"])),

    %% Instead of the 5:32 field below, there used to be control characters
    %% (including zero bytes) directly in the string. Because inferior programs
    %% such as sed and clearcasediff don't like zero bytes in text files,
    %% we have eliminated them.
    ok = file:write_file(BeamFile, <<"FOR1",5:32,"BEAMfel">>),

    NoOfTables = length(ets:all()),
    true = (P0 == pps()),
    file:delete(Source),
    file:delete(WrongFile),
    file:delete(BeamFile),
    file:delete(ACopy),
    ok.

last_chunk(Bin) ->
    L = beam_lib:info(Bin),
    {chunks,Chunks} = lists:keyfind(chunks, 1, L),
    {Last,_,_} = lists:last(Chunks),
    Last.

do_error(BeamFile, ACopy) ->
    %% evil tests
    Chunks = chunk_info(BeamFile),
    {value, {_, AtomStart, _}} = lists:keysearch("AtU8", 1, Chunks),
    {value, {_, ImportStart, _}} = lists:keysearch("ImpT", 1, Chunks),
    {value, {_, DebugInfoStart, _}} = lists:keysearch("Dbgi", 1, Chunks),
    {value, {_, AttributesStart, _}} =
	lists:keysearch("Attr", 1, Chunks),
    {value, {_, CompileInfoStart, _}} =
	lists:keysearch("CInf", 1, Chunks),
    verify(missing_chunk, beam_lib:chunks(BeamFile, ["__"])),
    BF2 = set_byte(ACopy, BeamFile, ImportStart+4, 17),
    verify(invalid_chunk, beam_lib:chunks(BF2, [imports])),
    BF3 = set_byte(ACopy, BeamFile, AtomStart-6, 17),
    verify(missing_chunk, beam_lib:chunks(BF3, [imports])),
    BF4 = set_byte(ACopy, BeamFile, DebugInfoStart+10, 17),
    verify(invalid_chunk, beam_lib:chunks(BF4, [debug_info])),
    BF5 = set_byte(ACopy, BeamFile, AttributesStart+8, 17),
    verify(invalid_chunk, beam_lib:chunks(BF5, [attributes])),

    BF6 = set_byte(ACopy, BeamFile, 1, 17),
    verify(not_a_beam_file, beam_lib:info(BF6)),
    BF7 = set_byte(ACopy, BeamFile, 9, 17),
    verify(not_a_beam_file, beam_lib:info(BF7)),

    BF8 = set_byte(ACopy, BeamFile, 13, 17),
    verify(missing_chunk, beam_lib:chunks(BF8, ["AtU8"])),

    BF9 = set_byte(ACopy, BeamFile, CompileInfoStart+8, 17),
    verify(invalid_chunk, beam_lib:chunks(BF9, [compile_info])).


%% Compare contents of BEAM files and directories.
cmp(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,

    Dir1 = filename:join(PrivDir, "dir1"),
    Dir2 = filename:join(PrivDir, "dir2"),

    ok = file:make_dir(Dir1),
    ok = file:make_dir(Dir2),

    {SourceD1, BeamFileD1} = make_beam(Dir1, simple, member),
    {Source2D1, BeamFile2D1} = make_beam(Dir1, simple2, concat),
    {SourceD2, BeamFileD2} = make_beam(Dir2, simple, concat),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    %% cmp
    ok = beam_lib:cmp(BeamFileD1, BeamFileD1),
    ver(modules_different, beam_lib:cmp(BeamFileD1, BeamFile2D1)),
    ver(chunks_different, beam_lib:cmp(BeamFileD1, BeamFileD2)),
    verify(file_error, beam_lib:cmp(foo, bar)),

    {ok, B1} = file:read_file(BeamFileD1),
    ok = beam_lib:cmp(B1, BeamFileD1),
    {ok, B2} = file:read_file(BeamFileD2),
    ver(chunks_different, beam_lib:cmp(B1, B2)),

    %% cmp_dirs
    {[],[],[]} = beam_lib:cmp_dirs(Dir1, Dir1),
    true = {[BeamFile2D1], [], [{BeamFileD1,BeamFileD2}]} ==
	beam_lib:cmp_dirs(Dir1, Dir2),
    true = {[], [BeamFile2D1], [{BeamFileD2,BeamFileD1}]} ==
	beam_lib:cmp_dirs(Dir2, Dir1),
    ver(not_a_directory, beam_lib:cmp_dirs(foo, bar)),

    %% diff_dirs
    ok = beam_lib:diff_dirs(Dir1, Dir1),
    ver(not_a_directory, beam_lib:diff_dirs(foo, bar)),

    true = (P0 == pps()),
    NoOfTables = length(ets:all()),
    delete_files([SourceD1, BeamFileD1, Source2D1,
		  BeamFile2D1, SourceD2, BeamFileD2]),

    file:del_dir(Dir1),
    file:del_dir(Dir2),
    ok.

%% Compare contents of BEAM files having literals.
cmp_literals(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,

    Dir1 = filename:join(PrivDir, "dir1"),
    Dir2 = filename:join(PrivDir, "dir2"),

    ok = file:make_dir(Dir1),
    ok = file:make_dir(Dir2),

    {SourceD1, BeamFileD1} = make_beam(Dir1, simple, constant),
    {SourceD2, BeamFileD2} = make_beam(Dir2, simple, constant2),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    %% cmp
    ok = beam_lib:cmp(BeamFileD1, BeamFileD1),
    ver(chunks_different, beam_lib:cmp(BeamFileD1, BeamFileD2)),

    {ok, B1} = file:read_file(BeamFileD1),
    ok = beam_lib:cmp(B1, BeamFileD1),
    {ok, B2} = file:read_file(BeamFileD2),
    ver(chunks_different, beam_lib:cmp(B1, B2)),

    true = (P0 == pps()),
    NoOfTables = length(ets:all()),

    delete_files([SourceD1, BeamFileD1, SourceD2, BeamFileD2]),

    file:del_dir(Dir1),
    file:del_dir(Dir2),
    ok.

%% Strip BEAM files.
strip(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,
    {SourceD1, BeamFileD1} = make_beam(PrivDir, simple, member),
    {Source2D1, BeamFile2D1} = make_beam(PrivDir, simple2, concat),
    {Source3D1, BeamFile3D1} = make_beam(PrivDir, make_fun, make_fun),
    {Source4D1, BeamFile4D1} = make_beam(PrivDir, constant, constant),
    {Source5D1, BeamFile5D1} = make_beam(PrivDir, lines, lines),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    %% strip binary
    verify(not_a_beam_file, beam_lib:strip(<<>>)),
    {ok, B1} = file:read_file(BeamFileD1),
    {ok, {simple, NB1}} = beam_lib:strip(B1),
    BId1 = chunk_ids(B1),
    NBId1 = chunk_ids(NB1),
    true = length(BId1) > length(NBId1),
    compare_chunks(B1, NB1, NBId1),

    %% strip file
    verify(file_error, beam_lib:strip(foo)),
    {ok, {simple, _}} = beam_lib:strip(BeamFileD1),
    compare_chunks(NB1, BeamFileD1, NBId1),

    %% strip_files
    {ok, B2} = file:read_file(BeamFile2D1),
    {ok, [{simple,_},{simple2,_}]} = beam_lib:strip_files([B1, B2]),
    {ok, [{simple,_},{simple2,_},{make_fun,_},{constant,_}]} =
	beam_lib:strip_files([BeamFileD1, BeamFile2D1, BeamFile3D1, BeamFile4D1]),

    %% check that each module can be loaded.
    {module, simple} = code:load_abs(filename:rootname(BeamFileD1)),
    {module, simple2} = code:load_abs(filename:rootname(BeamFile2D1)),
    {module, make_fun} = code:load_abs(filename:rootname(BeamFile3D1)),
    {module, constant} = code:load_abs(filename:rootname(BeamFile4D1)),

    %% check that line number information is still present after stripping
    {module, lines} = code:load_abs(filename:rootname(BeamFile5D1)),
    {'EXIT',{badarith,[{lines,t,1,Info}|_]}} =
	(catch lines:t(atom)),
    true = code:delete(lines),
    false = code:purge(lines),
    {ok, {lines,BeamFile5D1}} = beam_lib:strip(BeamFile5D1),
    {module, lines} = code:load_abs(filename:rootname(BeamFile5D1)),
    {'EXIT',{badarith,[{lines,t,1,Info}|_]}} =
	(catch lines:t(atom)),

    true = (P0 == pps()),
    NoOfTables = length(ets:all()),

    delete_files([SourceD1, BeamFileD1,
		  Source2D1, BeamFile2D1,
		  Source3D1, BeamFile3D1,
		  Source4D1, BeamFile4D1,
		  Source5D1, BeamFile5D1]),
    ok.


otp_6711(Conf) when is_list(Conf) ->
    {'EXIT',{function_clause,_}} = (catch {a, beam_lib:info(3)}),
    {'EXIT',{function_clause,_}} = (catch {a, beam_lib:chunks(a, b)}),
    {'EXIT',{function_clause,_}} = (catch {a, beam_lib:chunks(a,b,c)}),
    {'EXIT',{function_clause,_}} = (catch {a, beam_lib:all_chunks(3)}),
    {'EXIT',{function_clause,_}} = (catch {a, beam_lib:cmp(3,4)}),
    {'EXIT',{function_clause,_}} = (catch {a, beam_lib:strip(3)}),
    {'EXIT',{function_clause,_}} =
        (catch {a, beam_lib:strip_files([3])}),

    PrivDir = ?privdir,
    Dir = filename:join(PrivDir, "dir"),
    Lib = filename:join(Dir, "lib"),
    App = filename:join(Lib, "app"),
    EBin = filename:join(App, "ebin"),

    ok = file:make_dir(Dir),
    ok = file:make_dir(Lib),
    ok = file:make_dir(App),
    ok = file:make_dir(EBin),

    {SourceD, BeamFileD} = make_beam(EBin, simple, member),

    unwritable(BeamFileD),

    %% There is no way that strip_release can fail with
    %% function_clause or something like that...
    {error,_,{file_error,_,_}} = beam_lib:strip_release(Dir),

    delete_files([SourceD, BeamFileD]),
    file:del_dir(EBin),
    file:del_dir(App),
    file:del_dir(Lib),
    file:del_dir(Dir),
    ok.

-include_lib("kernel/include/file.hrl").

unwritable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

%% Testing building of BEAM files.
building(Conf) when is_list(Conf) ->
    PrivDir = ?privdir,

    Dir1 = filename:join(PrivDir, "b_dir1"),
    Dir2 = filename:join(PrivDir, "b_dir2"),

    ok = file:make_dir(Dir1),
    ok = file:make_dir(Dir2),

    {SourceD1, BeamFileD1} = make_beam(Dir1, building, member),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    %% read all chunks
    ChunkIds = chunk_ids(BeamFileD1),
    {ok, _Mod, Chunks} = beam_lib:all_chunks(BeamFileD1),
    ChunkIds = lists:map(fun ({Id, Data}) when is_binary(Data) -> Id
			 end, Chunks),

    %% write a new beam file, with reversed chunk order
    BeamFileD2 = filename:join(Dir2, "building.beam"),
    {ok,RevBeam} = beam_lib:build_module(lists:reverse(Chunks)),
    file:write_file(BeamFileD2, RevBeam),

    %% compare files
    compare_chunks(BeamFileD1, BeamFileD2, ChunkIds),

    %% test that we can retrieve a chunk before the atom table
    %% (actually, try to retrieve all chunks)

    lists:foreach(fun(Id) ->
			  {ok, {building, [{Id, _Data}]}} =
			      beam_lib:chunks(BeamFileD1, [Id])
		  end, ChunkIds),
    lists:foreach(fun(Id) ->
			  {ok, {building, [{Id, _Data}]}} =
			      beam_lib:chunks(BeamFileD2, [Id])
		  end, ChunkIds),

    true = (P0 == pps()),
    NoOfTables = length(ets:all()),

    delete_files([SourceD1, BeamFileD1, BeamFileD2]),
    file:del_dir(Dir1),
    file:del_dir(Dir2),
    ok.

%% Compare beam_lib:md5/1 and code:module_md5/1.
md5(Conf) when is_list(Conf) ->
    Beams = collect_beams(),
    io:format("Found ~w beam files", [length(Beams)]),
    md5_1(Beams).

md5_1([N|Ns]) ->
    {ok,Beam0} = file:read_file(N),
    Beam = maybe_uncompress(Beam0),
    {ok,{Mod,MD5}} = beam_lib:md5(Beam),
    {Mod,MD5} = {Mod,code:module_md5(Beam)},
    md5_1(Ns);
md5_1([]) -> ok.

collect_beams() ->
    SuperDir = filename:dirname(filename:dirname(code:which(?MODULE))),
    TestDirs = filelib:wildcard(filename:join([SuperDir,"*_test"])),
    AbsDirs = [filename:absname(X) || X <- code:get_path()],
    collect_beams_1(AbsDirs ++ TestDirs).

collect_beams_1([Dir|Dirs]) ->
    filelib:wildcard(filename:join(Dir, "*.beam")) ++ collect_beams_1(Dirs);
collect_beams_1([]) -> [].

maybe_uncompress(<<"FOR1",_/binary>>=Beam) -> Beam;
maybe_uncompress(Beam) -> zlib:gunzip(Beam).

%% Test encrypted abstract format.
encrypted_abstr(Conf) when is_list(Conf) ->
    run_if_crypto_works(fun() -> encrypted_abstr_1(Conf) end).

encrypted_abstr_1(Conf) ->
    PrivDir = ?privdir,
    Simple = filename:join(PrivDir, "simple"),
    Source = Simple ++ ".erl",
    BeamFile = Simple ++ ".beam",
    simple_file(Source),

    %% Avoid getting an extra port when crypto starts erl_ddll.
    erl_ddll:start(),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    Key = "#a_crypto_key",
    CompileFlags = [{outdir,PrivDir}, debug_info, {debug_info_key,Key}],
    {ok,_} = compile:file(Source, CompileFlags),
    {ok, Binary} = file:read_file(BeamFile),

    do_encrypted_abstr(BeamFile, Key),
    do_encrypted_abstr(Binary, Key),

    ok = crypto:stop(),			%To get rid of extra ets tables.
    file:delete(BeamFile),
    file:delete(Source),
    NoOfTables = length(ets:all()),
    true = (P0 == pps()),
    ok.

do_encrypted_abstr(Beam, Key) ->
    verify(key_missing_or_invalid, beam_lib:chunks(Beam, [debug_info])),

    %% The raw chunk "Dbgi" can still be read even without a key.
    {ok,{simple,[{"Dbgi",Dbgi}]}} = beam_lib:chunks(Beam, ["Dbgi"]),
    <<0:8,8:8,"des3_cbc",_/binary>> = Dbgi,

    %% Try som invalid funs.
    bad_fun(badfun, fun() -> ok end),
    bad_fun(badfun, {a,b}),
    bad_fun(blurf),
    {function_clause,_} = bad_fun(fun(glurf) -> ok end),

    %% Funs that return something strange.
    bad_fun(badfun, fun(init) -> {ok,fun() -> ok end} end),
    glurf = bad_fun(fun(init) -> {error,glurf} end),

    %% Try clearing (non-existing fun).
    undefined = beam_lib:clear_crypto_key_fun(),

    %% Install a fun which cannot retrieve a key.
    ok = beam_lib:crypto_key_fun(fun(init) -> ok end),
    {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Install a fun which returns an incorrect key.
    {ok,_} = beam_lib:clear_crypto_key_fun(),
    ok = beam_lib:crypto_key_fun(simple_crypto_fun("wrong key...")),
    {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Installing a new key fun is not possible without clearing the old.
    verify(exists, beam_lib:crypto_key_fun(simple_crypto_fun(Key))),

    %% Install the simplest possible working key fun.
    {ok,_} = beam_lib:clear_crypto_key_fun(),
    ok = beam_lib:crypto_key_fun(simple_crypto_fun(Key)),
    verify_abstract(Beam),
    {ok,{simple,[{"Dbgi",Dbgi}]}} = beam_lib:chunks(Beam, ["Dbgi"]),

    %% Installing a new key fun is not possible without clearing the old.
    verify(exists, beam_lib:crypto_key_fun(ets_crypto_fun(Key))),

    %% Install a key using an ets table.
    {ok,_} = beam_lib:clear_crypto_key_fun(),
    ok = beam_lib:crypto_key_fun(ets_crypto_fun(Key)),
    verify_abstract(Beam),
    {ok,{simple,[{"Dbgi",Dbgi}]}} = beam_lib:chunks(Beam, ["Dbgi"]),

    {ok,cleared} = beam_lib:clear_crypto_key_fun(),

    %% Try to force a stop/start race.
    start_stop_race(10000),

    ok.

start_stop_race(0) ->
    ok;
start_stop_race(N) ->
    {error,_} = beam_lib:crypto_key_fun(bad_fun),
    undefined = beam_lib:clear_crypto_key_fun(),
    start_stop_race(N-1).

bad_fun(F) ->
    {error,E} = beam_lib:crypto_key_fun(F),
    E.

bad_fun(S, F) ->
    verify(S, beam_lib:crypto_key_fun(F)).

verify_abstract(Beam) ->
    {ok,{simple,[Abst, Dbgi]}} = beam_lib:chunks(Beam, [abstract_code, debug_info]),
    {abstract_code,{raw_abstract_v1,_}} = Abst,
    {debug_info,{debug_info_v1,erl_abstract_code,_}} = Dbgi.

simple_crypto_fun(Key) ->
    fun(init) -> ok;
       ({debug_info, des3_cbc, simple, _}) -> Key
    end.

ets_crypto_fun(Key) ->
    fun(init) ->
	    T = ets:new(beam_lib_SUITE_keys, [private, set]),
	    true = ets:insert(T, {key,Key}),
	    {ok,fun({debug_info, des3_cbc, simple, _}) ->
			[{key,Val}] = ets:lookup(T, key),
			Val;
		   (clear) ->
			ets:delete(T),
			cleared
		end}
    end.

%% Test encrypted abstract format with the key in .erlang.crypt.
encrypted_abstr_file(Conf) when is_list(Conf) ->
    run_if_crypto_works(fun() -> encrypted_abstr_file_1(Conf) end).

encrypted_abstr_file_1(Conf) ->
    PrivDir = ?privdir,
    Simple = filename:join(PrivDir, "simple"),
    Source = Simple ++ ".erl",
    BeamFile = Simple ++ ".beam",
    simple_file(Source),

    %% Avoid getting an extra port when crypto starts erl_ddll.
    erl_ddll:start(),

    NoOfTables = length(ets:all()),
    P0 = pps(),

    Key = "Long And niCe 99Krypto Key",
    CompileFlags = [{outdir,PrivDir}, debug_info, {debug_info_key,Key}],
    {ok,_} = compile:file(Source, CompileFlags),
    {ok, Binary} = file:read_file(BeamFile),

    {ok,OldCwd} = file:get_cwd(),
    ok = file:set_cwd(PrivDir),
    do_encrypted_abstr_file(BeamFile, Key),
    do_encrypted_abstr_file(Binary, Key),
    ok = file:set_cwd(OldCwd),

    ok = crypto:stop(),			%To get rid of extra ets tables.
    file:delete(filename:join(PrivDir, ".erlang.crypt")),
    file:delete(BeamFile),
    file:delete(Source),
    NoOfTables = length(ets:all()),
    true = (P0 == pps()),
    ok.

do_encrypted_abstr_file(Beam, Key) ->
    %% No key.
    write_crypt_file(""),
    {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% A wrong key.
    write_crypt_file(["[{debug_info,des3_cbc,simple,\"A Wrong Key\"}].\n"]),
    {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Write correct key...
    write_crypt_file(["[{debug_info,des3_cbc,simple,\"",Key,"\"}].\n"]),

    %% ... but the fun with the wrong key is still there.
    {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Clear the fun. Now it should work.
    {ok,_} = beam_lib:clear_crypto_key_fun(),
    verify_abstract(Beam),
    verify_abstract(Beam),
    ok = file:delete(".erlang.crypt"),
    verify_abstract(Beam),

    %% Clear, otherwise the second pass will fail.
    {ok,_} = beam_lib:clear_crypto_key_fun(),
    {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),
    ok.

write_crypt_file(Contents0) ->
    Contents = list_to_binary([Contents0]),
    io:format("~s\n", [binary_to_list(Contents)]),
    ok = file:write_file(".erlang.crypt", Contents).

compare_chunks(File1, File2, ChunkIds) ->
    {ok, {_, Chunks1}} = beam_lib:chunks(File1, ChunkIds),
    {ok, {_, Chunks2}} = beam_lib:chunks(File2, ChunkIds),
    true = Chunks1 == Chunks2.

chunk_ids(File) ->
    lists:map(fun({Id,_Start,_Size}) -> Id end, chunk_info(File)).

chunk_info(File) ->
    {value, {chunks, Chunks}} =
	lists:keysearch(chunks, 1, beam_lib:info(File)),
    Chunks.

make_beam(Dir, Module, F) ->
    FileBase = filename:join(Dir, atom_to_list(Module)),
    Source = FileBase ++ ".erl",
    BeamFile = FileBase ++ ".beam",
    simple_file(Source, Module, F),
    {ok, _} = compile:file(Source, [{outdir,Dir}, debug_info, report]),
    {Source, BeamFile}.

set_byte(_Backup, Binary, Pos, Byte) when is_binary(Binary) ->
    <<B1:Pos/binary, _:1/binary, B2/binary>> = Binary,
    NB = <<B1/binary, Byte:8, B2/binary>>,
    NB;
set_byte(Backup, File, Pos, Byte) ->
    copy_file(Backup, File),
    set_byte(File, Pos, Byte),
    File.

set_byte(File, Pos, Byte) ->
    {ok, Fd} = file:open(File, [read, write]),
    {ok, _} = file:position(Fd, Pos),
    ok = file:write(Fd, [Byte]),
    file:close(Fd).

copy_file(Src, Dest) ->
    {ok, _} = file:copy(Src, Dest),
    ok = file:change_mode(Dest, 8#0666).

delete_files(Files) ->
    lists:foreach(fun(F) -> file:delete(F) end, Files).

verify(S, {error, beam_lib, R}) ->
    verify_error(S, R);
verify(S, {error, R}) ->
    verify_error(S, R).

verify_error(S, R) ->
    if
	S =:= R -> ok;
	true -> [S|_] = tuple_to_list(R)
    end,

    %% Most formatted messages begin with "./simple.beam:" or "<<...".
    FM = string:str(lists:flatten(beam_lib:format_error(R)), "simpl") > 0,
    BM = string:str(lists:flatten(beam_lib:format_error(R)), "<<") > 0,

    %% Also make sure that formatted message is not just the term printed.
    Handled = beam_lib:format_error(R) =/= io_lib:format("~p~n", [R]),
    true = ((FM > 0) or (BM > 0)) and Handled.

ver(S, {error, beam_lib, R}) ->
    [S|_] = tuple_to_list(R),
    case lists:flatten(beam_lib:format_error(R)) of
	[${ | _] ->
	    ct:fail({bad_format_error, R});
	_ ->
	    ok
    end.

pps() ->
    {erlang:ports()}.

simple_file(File) ->
    simple_file(File, simple).

simple_file(File, Module) ->
    simple_file(File, Module, member).

simple_file(File, Module, make_fun) ->
    B = list_to_binary(["-module(", atom_to_list(Module), "). "
			"-export([t/1]). "
			"t(A) -> "
			"    fun(X) -> A+X end. "]),
    ok = file:write_file(File, B);
simple_file(File, Module, constant) ->
    B = list_to_binary(["-module(", atom_to_list(Module), "). "
			"-export([t/1]). "
			"t(A) -> "
			"    {a,b,[2,3],c,d}. "]),
    ok = file:write_file(File, B);
simple_file(File, Module, constant2) ->
    B = list_to_binary(["-module(", atom_to_list(Module), "). "
			"-export([t/1]). "
			"t(A) -> "
			"    {a,b,[2,3],x,y}. "]),
    ok = file:write_file(File, B);
simple_file(File, Module, lines) ->
    B = list_to_binary(["-module(", atom_to_list(Module), ").\n"
			"-export([t/1]).\n"
			"t(A) ->\n"
			"    A+1.\n"]),
    ok = file:write_file(File, B);
simple_file(File, Module, F) ->
    B = list_to_binary(["-module(", atom_to_list(Module), "). "
			"-export([t/0]). "
			"t() -> "
			"    t([]). "
			"t(L) -> "
			"    lists:", 
			atom_to_list(F), "(a, L). "]),
    ok = file:write_file(File, B).

run_if_crypto_works(Test) ->
    try	begin crypto:start(), crypto:info(), crypto:stop(), ok end of
	ok ->
	    Test()
    catch
	error:_ ->
	    {skip,"The crypto application is missing or broken"}
    end.

