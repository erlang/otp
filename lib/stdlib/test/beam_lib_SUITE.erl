%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
-module(beam_lib_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), "./log_dir/").
-define(t,test_server).
-define(privdir, "beam_lib_SUITE_priv").
-else.
-include_lib("test_server/include/test_server.hrl").
-define(format(S, A), ok).
-define(privdir, ?config(priv_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 normal/1, error/1, cmp/1, cmp_literals/1, strip/1, otp_6711/1,
         building/1, md5/1, encrypted_abstr/1, encrypted_abstr_file/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

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
    Dog=?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

normal(suite) -> [];
normal(doc) -> ["Read correct beam file"];
normal(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir,
    ?line Simple = filename:join(PrivDir, "simple"),
    ?line Source = Simple ++ ".erl",
    ?line BeamFile = Simple ++ ".beam",
    ?line simple_file(Source),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    CompileFlags = [{outdir,PrivDir}, debug_info],
    ?line {ok,_} = compile:file(Source, CompileFlags),
    ?line {ok, Binary} = file:read_file(BeamFile),

    ?line do_normal(BeamFile),
    ?line do_normal(Binary),

    ?line {ok,_} = compile:file(Source, [{outdir,PrivDir}, no_debug_info]),
    ?line {ok, {simple, [{abstract_code, no_abstract_code}]}} = 
	beam_lib:chunks(BeamFile, [abstract_code]),

    %% ?line {ok,_} = compile:file(Source, [compressed | CompileFlags]),
    %% ?line do_normal(BeamFile),

    ?line file:delete(BeamFile),
    ?line file:delete(Source),
    ?line NoOfTables = length(ets:all()),
    ?line true = (P0 == pps()),    
    ok.

do_normal(BeamFile) ->
    ?line Imports = {imports, [{erlang, get_module_info, 1},
			       {erlang, get_module_info, 2},
			       {lists, member, 2}]},
    ?line Exports = {exports, [{module_info, 0}, {module_info, 1}, {t, 0}]},
    ?line Local = {locals, [{t, 1}]},
    ?line {ok, {simple, [Imports]}} = beam_lib:chunks(BeamFile, [imports]),
    ?line {ok, {simple, [{"ImpT",_Bin}]}} = 
	beam_lib:chunks(BeamFile, ["ImpT"]),
    ?line {ok, {simple, [Exports]}} = beam_lib:chunks(BeamFile, [exports]),
    ?line {ok, {simple, [{attributes, [{vsn, [_]}]}]}} = 
	beam_lib:chunks(BeamFile, [attributes]),
    ?line {ok, {simple, [{compile_info, _}=CompileInfo]}} = 
	beam_lib:chunks(BeamFile, [compile_info]),
    ?line {ok, {simple, [Local]}} = beam_lib:chunks(BeamFile, [locals]),
    ?line {ok, {simple, [{attributes, [{vsn, [_]}]}, CompileInfo,
			 Exports, Imports, Local]}} =
	beam_lib:chunks(BeamFile, [attributes, compile_info, exports, imports, locals]),
    ?line {ok, {simple, [{atoms, _Atoms}]}} = 
	beam_lib:chunks(BeamFile, [atoms]),
    ?line {ok, {simple, [{labeled_exports, _LExports}]}} =
	beam_lib:chunks(BeamFile, [labeled_exports]),
    ?line {ok, {simple, [{labeled_locals, _LLocals}]}} =
	beam_lib:chunks(BeamFile, [labeled_locals]),
    ?line {ok, {simple, [_Vsn]}} = beam_lib:version(BeamFile),
    ?line {ok, {simple, [{abstract_code, _}]}} = 
	beam_lib:chunks(BeamFile, [abstract_code]),
    
    %% Test reading optional chunks.
    All = ["Atom", "Code", "StrT", "ImpT", "ExpT", "FunT", "LitT"],
    ?line {ok,{simple,Chunks}} = beam_lib:chunks(BeamFile, All, [allow_missing_chunks]),
    ?line verify_simple(Chunks).

verify_simple([{"Atom", AtomBin},
	       {"Code", CodeBin},
	       {"StrT", StrBin},
	       {"ImpT", ImpBin},
	       {"ExpT", ExpBin},
	       {"FunT", missing_chunk},
	       {"LitT", missing_chunk}])
  when is_binary(AtomBin), is_binary(CodeBin), is_binary(StrBin),
       is_binary(ImpBin), is_binary(ExpBin) ->
    ok.

error(suite) -> [];
error(doc) -> ["Read invalid beam files"];
error(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir,
    ?line Simple = filename:join(PrivDir, "simple"),
    ?line Source = Simple ++ ".erl",
    ?line BeamFile = Simple ++ ".beam",
    ?line WrongFile = Simple ++ "foo.beam",
    ?line simple_file(Source),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),
    ?line {ok,_} = compile:file(Source, [{outdir,PrivDir},debug_info]),
    ?line ACopy = filename:join(PrivDir, "a_copy.beam"),
    ?line copy_file(BeamFile, ACopy),

    ?line {ok, Binary} = file:read_file(BeamFile),

    ?line copy_file(ACopy, WrongFile),
    ?line verify(file_error, beam_lib:info("./does_simply_not_exist")),

    ?line do_error(BeamFile, ACopy),
    ?line do_error(Binary, ACopy),

    ?line copy_file(ACopy, BeamFile),
    ?line verify(unknown_chunk, beam_lib:chunks(BeamFile, [not_a_chunk])),

    ?line ok = file:write_file(BeamFile, <<>>),
    ?line verify(not_a_beam_file, beam_lib:info(BeamFile)),
    ?line verify(not_a_beam_file, beam_lib:info(<<>>)),
    ?line ok = file:write_file(BeamFile, <<"short">>),
    ?line verify(not_a_beam_file, beam_lib:info(BeamFile)),
    ?line verify(not_a_beam_file, beam_lib:info(<<"short">>)),

    ?line {Binary1, _} = split_binary(Binary, byte_size(Binary)-10),
    LastChunk = last_chunk(Binary),
    ?line verify(chunk_too_big, beam_lib:chunks(Binary1, [LastChunk])),
    ?line Chunks = chunk_info(Binary),
    ?line {value, {_, AbstractStart, _}} = lists:keysearch("Abst", 1, Chunks),
    ?line {Binary2, _} = split_binary(Binary, AbstractStart),
    ?line verify(chunk_too_big, beam_lib:chunks(Binary2, ["Abst"])),
    ?line {Binary3, _} = split_binary(Binary, AbstractStart-4),
    ?line verify(invalid_beam_file, beam_lib:chunks(Binary3, ["Abst"])),

    %% Instead of the 5:32 field below, there used to be control characters
    %% (including zero bytes) directly in the string. Because inferior programs
    %% such as sed and clearcasediff don't like zero bytes in text files,
    %% we have eliminated them.
    ?line ok = file:write_file(BeamFile, <<"FOR1",5:32,"BEAMfel">>),
%   ?line verify(invalid_beam_file, beam_lib:info(BeamFile)),
%   ?line verify(invalid_beam_file, beam_lib:info(<<"FOR1",5:32,"BEAMfel">>)),

    ?line NoOfTables = length(ets:all()),
    ?line true = (P0 == pps()),    
    ?line file:delete(Source),
    ?line file:delete(WrongFile),
    ?line file:delete(BeamFile),
    ?line file:delete(ACopy),
    ok.

last_chunk(Bin) ->
    L = beam_lib:info(Bin),
    {chunks,Chunks} = lists:keyfind(chunks, 1, L),
    {Last,_,_} = lists:last(Chunks),
    Last.

do_error(BeamFile, ACopy) ->
    % evil tests
    ?line Chunks = chunk_info(BeamFile),
    ?line {value, {_, AtomStart, _}} = lists:keysearch("Atom", 1, Chunks),
    ?line {value, {_, ImportStart, _}} = lists:keysearch("ImpT", 1, Chunks),
    ?line {value, {_, AbstractStart, _}} = lists:keysearch("Abst", 1, Chunks),
    ?line {value, {_, AttributesStart, _}} = 
	lists:keysearch("Attr", 1, Chunks),
    ?line {value, {_, CompileInfoStart, _}} = 
	lists:keysearch("CInf", 1, Chunks),
    ?line verify(missing_chunk, beam_lib:chunks(BeamFile, ["__"])),
    ?line BF2 = set_byte(ACopy, BeamFile, ImportStart+4, 17),
    ?line verify(invalid_chunk, beam_lib:chunks(BF2, [imports])),
    ?line BF3 = set_byte(ACopy, BeamFile, AtomStart-6, 17),
    ?line verify(missing_chunk, beam_lib:chunks(BF3, [imports])),
    ?line BF4 = set_byte(ACopy, BeamFile, AbstractStart+10, 17),
    ?line verify(invalid_chunk, beam_lib:chunks(BF4, [abstract_code])),
    ?line BF5 = set_byte(ACopy, BeamFile, AttributesStart+10, 17),
    ?line verify(invalid_chunk, beam_lib:chunks(BF5, [attributes])),

    ?line BF6 = set_byte(ACopy, BeamFile, 1, 17),
    ?line verify(not_a_beam_file, beam_lib:info(BF6)),
    ?line BF7 = set_byte(ACopy, BeamFile, 9, 17),
    ?line verify(not_a_beam_file, beam_lib:info(BF7)),

    ?line BF8 = set_byte(ACopy, BeamFile, 13, 17),
    ?line verify(missing_chunk, beam_lib:chunks(BF8, ["Atom"])),
    
    ?line BF9 = set_byte(ACopy, BeamFile, CompileInfoStart+10, 17),
    ?line verify(invalid_chunk, beam_lib:chunks(BF9, [compile_info])).
    

cmp(suite) -> [];
cmp(doc) -> ["Compare contents of BEAM files and directories"];
cmp(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir,

    ?line Dir1 = filename:join(PrivDir, "dir1"),
    ?line Dir2 = filename:join(PrivDir, "dir2"),

    ok = file:make_dir(Dir1),
    ok = file:make_dir(Dir2),

    ?line {SourceD1, BeamFileD1} = make_beam(Dir1, simple, member),
    ?line {Source2D1, BeamFile2D1} = make_beam(Dir1, simple2, concat),
    ?line {SourceD2, BeamFileD2} = make_beam(Dir2, simple, concat),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    %% cmp
    ?line ok = beam_lib:cmp(BeamFileD1, BeamFileD1),
    ?line ver(modules_different, beam_lib:cmp(BeamFileD1, BeamFile2D1)),
    ?line ver(chunks_different, beam_lib:cmp(BeamFileD1, BeamFileD2)),
    ?line verify(file_error, beam_lib:cmp(foo, bar)),
    
    ?line {ok, B1} = file:read_file(BeamFileD1),
    ?line ok = beam_lib:cmp(B1, BeamFileD1),
    ?line {ok, B2} = file:read_file(BeamFileD2),
    ?line ver(chunks_different, beam_lib:cmp(B1, B2)),

    %% cmp_dirs
    ?line {[],[],[]} = beam_lib:cmp_dirs(Dir1, Dir1),
    ?line true = {[BeamFile2D1], [], [{BeamFileD1,BeamFileD2}]} ==
	         beam_lib:cmp_dirs(Dir1, Dir2),
    ?line true = {[], [BeamFile2D1], [{BeamFileD2,BeamFileD1}]} ==
                 beam_lib:cmp_dirs(Dir2, Dir1),
    ?line ver(not_a_directory, beam_lib:cmp_dirs(foo, bar)),
    
    %% diff_dirs
    ?line ok = beam_lib:diff_dirs(Dir1, Dir1),
    ?line ver(not_a_directory, beam_lib:diff_dirs(foo, bar)),

    ?line true = (P0 == pps()),
    ?line NoOfTables = length(ets:all()),
    ?line delete_files([SourceD1, BeamFileD1, Source2D1, 
			BeamFile2D1, SourceD2, BeamFileD2]),

    file:del_dir(Dir1),
    file:del_dir(Dir2),
    ok.

cmp_literals(suite) -> [];
cmp_literals(doc) -> ["Compare contents of BEAM files having literals"];
cmp_literals(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir,

    ?line Dir1 = filename:join(PrivDir, "dir1"),
    ?line Dir2 = filename:join(PrivDir, "dir2"),

    ok = file:make_dir(Dir1),
    ok = file:make_dir(Dir2),

    ?line {SourceD1, BeamFileD1} = make_beam(Dir1, simple, constant),
    ?line {SourceD2, BeamFileD2} = make_beam(Dir2, simple, constant2),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    %% cmp
    ?line ok = beam_lib:cmp(BeamFileD1, BeamFileD1),
    ?line ver(chunks_different, beam_lib:cmp(BeamFileD1, BeamFileD2)),
    
    ?line {ok, B1} = file:read_file(BeamFileD1),
    ?line ok = beam_lib:cmp(B1, BeamFileD1),
    ?line {ok, B2} = file:read_file(BeamFileD2),
    ?line ver(chunks_different, beam_lib:cmp(B1, B2)),

    ?line true = (P0 == pps()),
    ?line NoOfTables = length(ets:all()),

    ?line delete_files([SourceD1, BeamFileD1, SourceD2, BeamFileD2]),

    file:del_dir(Dir1),
    file:del_dir(Dir2),
    ok.

strip(suite) -> [];
strip(doc) -> ["Strip BEAM files"];
strip(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir,
    ?line {SourceD1, BeamFileD1} = make_beam(PrivDir, simple, member),
    ?line {Source2D1, BeamFile2D1} = make_beam(PrivDir, simple2, concat),
    ?line {Source3D1, BeamFile3D1} = make_beam(PrivDir, make_fun, make_fun),
    ?line {Source4D1, BeamFile4D1} = make_beam(PrivDir, constant, constant),
    ?line {Source5D1, BeamFile5D1} = make_beam(PrivDir, lines, lines),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    %% strip binary
    ?line verify(not_a_beam_file, beam_lib:strip(<<>>)),
    ?line {ok, B1} = file:read_file(BeamFileD1),
    ?line {ok, {simple, NB1}} = beam_lib:strip(B1),
    ?line BId1 = chunk_ids(B1),
    ?line NBId1 = chunk_ids(NB1),
    ?line true = length(BId1) > length(NBId1),
    ?line compare_chunks(B1, NB1, NBId1),

    %% strip file
    ?line verify(file_error, beam_lib:strip(foo)),
    ?line {ok, {simple, _}} = beam_lib:strip(BeamFileD1),
    ?line compare_chunks(NB1, BeamFileD1, NBId1),

    %% strip_files
    ?line {ok, B2} = file:read_file(BeamFile2D1),
    ?line {ok, [{simple,_},{simple2,_}]} = beam_lib:strip_files([B1, B2]),
    ?line {ok, [{simple,_},{simple2,_},{make_fun,_},{constant,_}]} = 
	beam_lib:strip_files([BeamFileD1, BeamFile2D1, BeamFile3D1, BeamFile4D1]),

    %% check that each module can be loaded.
    ?line {module, simple} = code:load_abs(filename:rootname(BeamFileD1)),
    ?line {module, simple2} = code:load_abs(filename:rootname(BeamFile2D1)),
    ?line {module, make_fun} = code:load_abs(filename:rootname(BeamFile3D1)),
    ?line {module, constant} = code:load_abs(filename:rootname(BeamFile4D1)),

    %% check that line number information is still present after stripping
    ?line {module, lines} = code:load_abs(filename:rootname(BeamFile5D1)),
    ?line {'EXIT',{badarith,[{lines,t,1,Info}|_]}} =
	(catch lines:t(atom)),
    ?line true = code:delete(lines),
    ?line false = code:purge(lines),
    ?line {ok, {lines,BeamFile5D1}} = beam_lib:strip(BeamFile5D1),
    ?line {module, lines} = code:load_abs(filename:rootname(BeamFile5D1)),
    ?line {'EXIT',{badarith,[{lines,t,1,Info}|_]}} =
	(catch lines:t(atom)),

    ?line true = (P0 == pps()),
    ?line NoOfTables = length(ets:all()),

    ?line delete_files([SourceD1, BeamFileD1, 
			Source2D1, BeamFile2D1, 
			Source3D1, BeamFile3D1,
			Source4D1, BeamFile4D1,
			Source5D1, BeamFile5D1]),
    ok.


otp_6711(Conf) when is_list(Conf) ->
    ?line {'EXIT',{function_clause,_}} = (catch {a, beam_lib:info(3)}),
    ?line {'EXIT',{function_clause,_}} = (catch {a, beam_lib:chunks(a, b)}),
    ?line {'EXIT',{function_clause,_}} = (catch {a, beam_lib:chunks(a,b,c)}),
    ?line {'EXIT',{function_clause,_}} = (catch {a, beam_lib:all_chunks(3)}),
    ?line {'EXIT',{function_clause,_}} = (catch {a, beam_lib:cmp(3,4)}),
    ?line {'EXIT',{function_clause,_}} = (catch {a, beam_lib:strip(3)}),
    ?line {'EXIT',{function_clause,_}} = 
        (catch {a, beam_lib:strip_files([3])}),

    ?line PrivDir = ?privdir,
    ?line Dir = filename:join(PrivDir, "dir"),
    ?line Lib = filename:join(Dir, "lib"),
    ?line App = filename:join(Lib, "app"),
    ?line EBin = filename:join(App, "ebin"),

    ok = file:make_dir(Dir),
    ok = file:make_dir(Lib),
    ok = file:make_dir(App),
    ok = file:make_dir(EBin),
    
    ?line {SourceD, BeamFileD} = make_beam(EBin, simple, member),

    unwritable(BeamFileD),

    %% There is no way that strip_release can fail with
    %% function_clause or something like that...
    ?line {error,_,{file_error,_,_}} = beam_lib:strip_release(Dir),

    ?line delete_files([SourceD, BeamFileD]),
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

building(doc) -> "Testing building of BEAM files.";
building(Conf) when is_list(Conf) ->
    ?line PrivDir = ?privdir,

    ?line Dir1 = filename:join(PrivDir, "b_dir1"),
    ?line Dir2 = filename:join(PrivDir, "b_dir2"),

    ok = file:make_dir(Dir1),
    ok = file:make_dir(Dir2),

    ?line {SourceD1, BeamFileD1} = make_beam(Dir1, building, member),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    %% read all chunks
    ?line ChunkIds = chunk_ids(BeamFileD1),
    ?line {ok, _Mod, Chunks} = beam_lib:all_chunks(BeamFileD1),
    ?line ChunkIds = lists:map(fun ({Id, Data}) when is_binary(Data) -> Id 
                               end, Chunks),

    %% write a new beam file, with reversed chunk order
    ?line BeamFileD2 = filename:join(Dir2, "building.beam"),
    ?line {ok,RevBeam} = beam_lib:build_module(lists:reverse(Chunks)),
    ?line file:write_file(BeamFileD2, RevBeam),

    %% compare files
    ?line compare_chunks(BeamFileD1, BeamFileD2, ChunkIds),

    %% test that we can retrieve a chunk before the atom table
    %% (actually, try to retrieve all chunks)

    ?line lists:foreach(fun(Id) ->
				{ok, {building, [{Id, _Data}]}} =
				    beam_lib:chunks(BeamFileD1, [Id])
			end, ChunkIds),
    ?line lists:foreach(fun(Id) ->
				{ok, {building, [{Id, _Data}]}} =
				    beam_lib:chunks(BeamFileD2, [Id])
			end, ChunkIds),

    ?line true = (P0 == pps()),
    ?line NoOfTables = length(ets:all()),

    ?line delete_files([SourceD1, BeamFileD1, BeamFileD2]),
    file:del_dir(Dir1),
    file:del_dir(Dir2),
    ok.

md5(suite) -> [];
md5(doc) -> ["Compare beam_lib:md5/1 and code:module_md5/1."];
md5(Conf) when is_list(Conf) ->
    ?line Beams = collect_beams(),
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

encrypted_abstr(suite) -> [];
encrypted_abstr(doc) -> ["Test encrypted abstract format"];
encrypted_abstr(Conf) when is_list(Conf) ->
    run_if_crypto_works(fun() -> encrypted_abstr_1(Conf) end).

encrypted_abstr_1(Conf) ->
    ?line PrivDir = ?privdir,
    ?line Simple = filename:join(PrivDir, "simple"),
    ?line Source = Simple ++ ".erl",
    ?line BeamFile = Simple ++ ".beam",
    ?line simple_file(Source),

    %% Avoid getting an extra port when crypto starts erl_ddll.
    ?line erl_ddll:start(),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    Key = "#a_crypto_key",
    CompileFlags = [{outdir,PrivDir}, debug_info, {debug_info_key,Key}],
    ?line {ok,_} = compile:file(Source, CompileFlags),
    ?line {ok, Binary} = file:read_file(BeamFile),

    ?line do_encrypted_abstr(BeamFile, Key),
    ?line do_encrypted_abstr(Binary, Key),

    ?line ok = crypto:stop(),			%To get rid of extra ets tables.
    ?line file:delete(BeamFile),
    ?line file:delete(Source),
    ?line NoOfTables = length(ets:all()),
    ?line true = (P0 == pps()),    
    ok.

do_encrypted_abstr(Beam, Key) ->
    ?line verify(key_missing_or_invalid, beam_lib:chunks(Beam, [abstract_code])),

    %% The raw chunk "Abst" can still be read even without a key.
    ?line {ok,{simple,[{"Abst",Abst}]}} = beam_lib:chunks(Beam, ["Abst"]),
    ?line <<0:8,8:8,"des3_cbc",_/binary>> = Abst,

    %% Try som invalid funs.
    ?line bad_fun(badfun, fun() -> ok end),
    ?line bad_fun(badfun, {a,b}),
    ?line bad_fun(blurf),
    ?line {function_clause,_} = bad_fun(fun(glurf) -> ok end),

    %% Funs that return something strange.
    ?line bad_fun(badfun, fun(init) -> {ok,fun() -> ok end} end),
    ?line glurf = bad_fun(fun(init) -> {error,glurf} end),

    %% Try clearing (non-existing fun).
    ?line undefined = beam_lib:clear_crypto_key_fun(),

    %% Install a fun which cannot retrieve a key.
    ?line ok = beam_lib:crypto_key_fun(fun(init) -> ok end),
    ?line {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Install a fun which returns an incorrect key.
    ?line {ok,_} = beam_lib:clear_crypto_key_fun(),
    ?line ok = beam_lib:crypto_key_fun(simple_crypto_fun("wrong key...")),
    ?line {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),
    
    %% Installing a new key fun is not possible without clearing the old.
    ?line verify(exists, beam_lib:crypto_key_fun(simple_crypto_fun(Key))),

    %% Install the simplest possible working key fun.
    ?line {ok,_} = beam_lib:clear_crypto_key_fun(),
    ?line ok = beam_lib:crypto_key_fun(simple_crypto_fun(Key)),
    ?line verify_abstract(Beam),
    ?line {ok,{simple,[{"Abst",Abst}]}} = beam_lib:chunks(Beam, ["Abst"]),

    %% Installing a new key fun is not possible without clearing the old.
    verify(exists, beam_lib:crypto_key_fun(ets_crypto_fun(Key))),

    %% Install a key using an ets table.
    ?line {ok,_} = beam_lib:clear_crypto_key_fun(),
    ?line ok = beam_lib:crypto_key_fun(ets_crypto_fun(Key)),
    ?line verify_abstract(Beam),
    ?line {ok,{simple,[{"Abst",Abst}]}} = beam_lib:chunks(Beam, ["Abst"]),

    ?line {ok,cleared} = beam_lib:clear_crypto_key_fun(),

    %% Try to force a stop/start race.
    ?line start_stop_race(10000),

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
    {ok,{simple,[Chunk]}} = beam_lib:chunks(Beam, [abstract_code]),
    {abstract_code,{raw_abstract_v1,_}} = Chunk.

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

encrypted_abstr_file(suite) -> [];
encrypted_abstr_file(doc) ->
    ["Test encrypted abstract format with the key in .erlang.crypt"];
encrypted_abstr_file(Conf) when is_list(Conf) ->
    run_if_crypto_works(fun() -> encrypted_abstr_file_1(Conf) end).

encrypted_abstr_file_1(Conf) ->
    ?line PrivDir = ?privdir,
    ?line Simple = filename:join(PrivDir, "simple"),
    ?line Source = Simple ++ ".erl",
    ?line BeamFile = Simple ++ ".beam",
    ?line simple_file(Source),

    %% Avoid getting an extra port when crypto starts erl_ddll.
    ?line erl_ddll:start(),

    ?line NoOfTables = length(ets:all()),
    ?line P0 = pps(),

    Key = "Long And niCe 99Krypto Key",
    CompileFlags = [{outdir,PrivDir}, debug_info, {debug_info_key,Key}],
    ?line {ok,_} = compile:file(Source, CompileFlags),
    ?line {ok, Binary} = file:read_file(BeamFile),

    ?line {ok,OldCwd} = file:get_cwd(),
    ?line ok = file:set_cwd(PrivDir),
    ?line do_encrypted_abstr_file(BeamFile, Key),
    ?line do_encrypted_abstr_file(Binary, Key),
    ?line ok = file:set_cwd(OldCwd),

    ?line ok = crypto:stop(),			%To get rid of extra ets tables.
    ?line file:delete(filename:join(PrivDir, ".erlang.crypt")),
    ?line file:delete(BeamFile),
    ?line file:delete(Source),
    ?line NoOfTables = length(ets:all()),
    ?line true = (P0 == pps()),    
    ok.

do_encrypted_abstr_file(Beam, Key) ->
    %% No key.
    ?line write_crypt_file(""),
    ?line {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% A wrong key.
    ?line write_crypt_file(["[{debug_info,des3_cbc,simple,\"A Wrong Key\"}].\n"]),
    ?line {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Write correct key...
    ?line write_crypt_file(["[{debug_info,des3_cbc,simple,\"",Key,"\"}].\n"]),

    %% ... but the fun with the wrong key is still there.
    ?line {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),

    %% Clear the fun. Now it should work.
    ?line {ok,_} = beam_lib:clear_crypto_key_fun(),
    ?line verify_abstract(Beam),
    ?line verify_abstract(Beam),
    ?line ok = file:delete(".erlang.crypt"),
    ?line verify_abstract(Beam),

    %% Clear, otherwise the second pass will fail.
    ?line {ok,_} = beam_lib:clear_crypto_key_fun(),
    ?line {error,beam_lib,Error} = beam_lib:chunks(Beam, [abstract_code]),
    ok.

write_crypt_file(Contents0) ->
    Contents = list_to_binary([Contents0]),
    io:format("~s\n", [binary_to_list(Contents)]),
    ok = file:write_file(".erlang.crypt", Contents).

compare_chunks(File1, File2, ChunkIds) ->
    ?line {ok, {_, Chunks1}} = beam_lib:chunks(File1, ChunkIds),
    ?line {ok, {_, Chunks2}} = beam_lib:chunks(File2, ChunkIds),
    ?line true = Chunks1 == Chunks2.

chunk_ids(File) ->
    ?line lists:map(fun({Id,_Start,_Size}) -> Id end, chunk_info(File)).
    
chunk_info(File) ->
    ?line {value, {chunks, Chunks}} = 
	lists:keysearch(chunks, 1, beam_lib:info(File)),
    Chunks.
    
make_beam(Dir, Module, F) ->
    ?line FileBase = filename:join(Dir, atom_to_list(Module)),
    ?line Source = FileBase ++ ".erl",
    ?line BeamFile = FileBase ++ ".beam",
    ?line simple_file(Source, Module, F),
    ?line {ok, _} = compile:file(Source, [{outdir,Dir}, debug_info, report]),
    {Source, BeamFile}.

set_byte(_Backup, Binary, Pos, Byte) when is_binary(Binary) ->
    ?line <<B1:Pos/binary, _:1/binary, B2/binary>> = Binary,
    NB = <<B1/binary, Byte:8, B2/binary>>,
    NB;
set_byte(Backup, File, Pos, Byte) ->
    ?line copy_file(Backup, File),
    ?line set_byte(File, Pos, Byte),
    File.

set_byte(File, Pos, Byte) ->
    ?line {ok, Fd} = file:open(File, [read, write]),
    ?line {ok, _} = file:position(Fd, Pos),
    ?line ok = file:write(Fd, [Byte]),
    ?line file:close(Fd).

copy_file(Src, Dest) ->
    % ?t:format("copying from ~p to ~p~n", [Src, Dest]),
    ?line {ok, _} = file:copy(Src, Dest),
    ?line ok = file:change_mode(Dest, 8#0666).

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
	    test_server:fail({bad_format_error, R});
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

