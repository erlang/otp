%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
-module(beam_lib).
-behaviour(gen_server).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([info/1,
	 cmp/2,
	 cmp_dirs/2,
	 chunks/2,
	 chunks/3,
	 all_chunks/1,
	 diff_dirs/2,
	 strip/1,
	 strip/2,
	 strip_files/1,
	 strip_files/2,
	 strip_release/1,
	 strip_release/2,
	 significant_chunks/0,
	 build_module/1,
	 version/1,
	 md5/1,
	 format_error/1]).

%% The following functions implement encrypted debug info.

-export([crypto_key_fun/1, clear_crypto_key_fun/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
	 terminate/2,code_change/3]).
-export([make_crypto_key/2, get_crypto_key/1]).	%Utilities used by compiler

-export_type([attrib_entry/0, compinfo_entry/0, labeled_entry/0, label/0]).

-import(lists, [append/1, delete/2, foreach/2, keysort/2, 
		member/2, reverse/1, sort/1, splitwith/2]).

%%-------------------------------------------------------------------------

-type beam() :: file:filename() | binary().
-type debug_info() :: {DbgiVersion :: atom(), Backend :: module(), Data :: term()} | 'no_debug_info'.

-type forms()     :: [erl_parse:abstract_form() | erl_parse:form_info()].

-type abst_code() :: {AbstVersion :: atom(), forms()} | 'no_abstract_code'.
-type dataB()     :: binary().
-type index()     :: non_neg_integer().
-type label()     :: integer().

-type chunkid()   :: nonempty_string(). % approximation of the strings below
%% "Abst" | "Dbgi" | "Attr" | "CInf" | "ExpT" | "ImpT" | "LocT" | "Atom" | "AtU8".
-type chunkname() :: 'abstract_code' | 'debug_info'
                   | 'attributes' | 'compile_info'
                   | 'exports' | 'labeled_exports'
                   | 'imports' | 'indexed_imports'
                   | 'locals' | 'labeled_locals'
                   | 'atoms'.
-type chunkref()  :: chunkname() | chunkid().

-type attrib_entry()   :: {Attribute :: atom(), [AttributeValue :: term()]}.
-type compinfo_entry() :: {InfoKey :: atom(), term()}.
-type labeled_entry()  :: {Function :: atom(), arity(), label()}.

-type chunkdata() :: {chunkid(), dataB()}
                   | {'abstract_code', abst_code()}
                   | {'debug_info', debug_info()}
                   | {'attributes', [attrib_entry()]}
                   | {'compile_info', [compinfo_entry()]}
                   | {'exports', [{atom(), arity()}]}
                   | {'labeled_exports', [labeled_entry()]}
                   | {'imports', [mfa()]}
                   | {'indexed_imports', [{index(), module(), Function :: atom(), arity()}]}
                   | {'locals', [{atom(), arity()}]}
                   | {'labeled_locals', [labeled_entry()]}
                   | {'atoms', [{integer(), atom()}]}.

%% Error reasons
-type info_rsn()  :: {'chunk_too_big', file:filename(),
		      chunkid(), ChunkSize :: non_neg_integer(),
                      FileSize :: non_neg_integer()}
                   | {'invalid_beam_file', file:filename(),
                      Position :: non_neg_integer()}
                   | {'invalid_chunk', file:filename(), chunkid()}
                   | {'missing_chunk', file:filename(), chunkid()}
                   | {'not_a_beam_file', file:filename()}
                   | {'file_error', file:filename(), file:posix()}.
-type chnk_rsn()  :: {'unknown_chunk', file:filename(), atom()}
                   | {'key_missing_or_invalid', file:filename(),
		      'abstract_code' | 'debug_info'}
                   | info_rsn().
-type cmp_rsn()   :: {'modules_different', module(), module()}
                   | {'chunks_different', chunkid()}
                   | 'different_chunks'
                   | info_rsn().

%%-------------------------------------------------------------------------

%%
%%  Exported functions
%%

-spec info(Beam) -> [InfoPair] | {'error', 'beam_lib', info_rsn()} when
      Beam :: beam(),
      InfoPair :: {'file', Filename :: file:filename()}
                | {'binary', Binary :: binary()}
                | {'module', Module :: module()}
                | {'chunks', [{ChunkId :: chunkid(),
                               Pos :: non_neg_integer(),
                               Size :: non_neg_integer()}]}.

info(File) ->
    read_info(beam_filename(File)).

-spec chunks(Beam, ChunkRefs) ->
                    {'ok', {module(), [chunkdata()]}} |
                    {'error', 'beam_lib', chnk_rsn()} when
      Beam :: beam(),
      ChunkRefs :: [chunkref()].

chunks(File, Chunks) ->
    read_chunk_data(File, Chunks).

-spec chunks(Beam, ChunkRefs, Options) ->
                    {'ok', {module(), [ChunkResult]}} |
                    {'error', 'beam_lib', chnk_rsn()} when
      Beam :: beam(),
      ChunkRefs :: [chunkref()],
      Options :: ['allow_missing_chunks'],
      ChunkResult :: chunkdata() | {ChunkRef :: chunkref(), 'missing_chunk'}.

chunks(File, Chunks, Options) ->
    try read_chunk_data(File, Chunks, Options)
    catch Error -> Error end.

-spec all_chunks(beam()) ->
           {'ok', 'beam_lib', [{chunkid(), dataB()}]} | {'error', 'beam_lib', info_rsn()}.

all_chunks(File) ->
    read_all_chunks(File).

-spec cmp(Beam1, Beam2) -> 'ok' | {'error', 'beam_lib', cmp_rsn()} when
      Beam1 :: beam(),
      Beam2 :: beam().

cmp(File1, File2) ->
    try cmp_files(File1, File2)
    catch Error -> Error end.

-spec cmp_dirs(Dir1, Dir2) ->
           {Only1, Only2, Different} | {'error', 'beam_lib', Reason} when
      Dir1 :: atom() | file:filename(),
      Dir2 :: atom() | file:filename(),
      Only1 :: [file:filename()],
      Only2 :: [file:filename()],
      Different :: [{Filename1 :: file:filename(), Filename2 :: file:filename()}],
      Reason :: {'not_a_directory', term()} | info_rsn().

cmp_dirs(Dir1, Dir2) ->
    catch compare_dirs(Dir1, Dir2).

-spec diff_dirs(Dir1, Dir2) -> 'ok' | {'error', 'beam_lib', Reason} when
      Dir1 :: atom() | file:filename(),
      Dir2 :: atom() | file:filename(),
      Reason :: {'not_a_directory', term()} | info_rsn().

diff_dirs(Dir1, Dir2) ->
    catch diff_directories(Dir1, Dir2).

-spec strip(Beam1) ->
        {'ok', {module(), Beam2}} | {'error', 'beam_lib', info_rsn()} when
      Beam1 :: beam(),
      Beam2 :: beam().

strip(FileName) ->
    strip(FileName, []).

-spec strip(Beam1, AdditionalChunks) ->
        {'ok', {module(), Beam2}} | {'error', 'beam_lib', info_rsn()} when
      Beam1 :: beam(),
      AdditionalChunks :: [chunkid()],
      Beam2 :: beam().

strip(FileName, AdditionalChunks) ->
    try strip_file(FileName, AdditionalChunks)
    catch Error -> Error end.
    
-spec strip_files(Files) ->
        {'ok', [{module(), Beam}]} | {'error', 'beam_lib', info_rsn()} when
      Files :: [beam()],
      Beam :: beam().

strip_files(Files) ->
    strip_files(Files, []).

-spec strip_files(Files, AdditionalChunks) ->
        {'ok', [{module(), Beam}]} | {'error', 'beam_lib', info_rsn()} when
      Files :: [beam()],
      AdditionalChunks :: [chunkid()],
      Beam :: beam().

strip_files(Files, AdditionalChunks) when is_list(Files) ->
    try strip_fils(Files, AdditionalChunks)
    catch Error -> Error end.

-spec strip_release(Dir) ->
        {'ok', [{module(), file:filename()}]}
      | {'error', 'beam_lib', Reason} when
      Dir :: atom() | file:filename(),
      Reason :: {'not_a_directory', term()} | info_rsn().

strip_release(Root) ->
    strip_release(Root, []).

-spec strip_release(Dir, AdditionalChunks) ->
        {'ok', [{module(), file:filename()}]}
      | {'error', 'beam_lib', Reason} when
      Dir :: atom() | file:filename(),
      AdditionalChunks :: [chunkid()],
      Reason :: {'not_a_directory', term()} | info_rsn().

strip_release(Root, AdditionalChunks) ->
    catch strip_rel(Root, AdditionalChunks).

-spec version(Beam) ->
                     {'ok', {module(), [Version :: term()]}} |
                     {'error', 'beam_lib', chnk_rsn()} when
      Beam :: beam().

version(File) ->
    case catch read_chunk_data(File, [attributes]) of
	{ok, {Module, [{attributes, Attrs}]}} ->
	    {vsn, Version} = lists:keyfind(vsn, 1, Attrs),
	    {ok, {Module, Version}};
	Error ->
	    Error
    end.

-spec md5(Beam) ->
        {'ok', {module(), MD5}} | {'error', 'beam_lib', chnk_rsn()} when
      Beam :: beam(),
      MD5 :: binary().

md5(File) ->
    case catch read_significant_chunks(File, md5_chunks()) of
	{ok, {Module, Chunks0}} ->
	    Chunks = filter_funtab(Chunks0),
	    {ok, {Module, erlang:md5([C || {_Id, C} <- Chunks])}};
	Error ->
	    Error
    end.

-spec format_error(Reason) -> io_lib:chars() when
      Reason :: term().

format_error({error, Error}) ->
    format_error(Error);
format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({unknown_chunk, File, ChunkName}) ->
    io_lib:format("~tp: Cannot find chunk ~p~n", [File, ChunkName]);
format_error({invalid_chunk, File, ChunkId}) ->
    io_lib:format("~tp: Invalid contents of chunk ~p~n", [File, ChunkId]);
format_error({not_a_beam_file, File}) ->
    io_lib:format("~tp: Not a BEAM file~n", [File]);
format_error({file_error, File, Reason}) ->
    io_lib:format("~tp: ~tp~n", [File, file:format_error(Reason)]);
format_error({missing_chunk, File, ChunkId}) ->
    io_lib:format("~tp: Not a BEAM file: no IFF \"~s\" chunk~n", 
		  [File, ChunkId]);
format_error({invalid_beam_file, File, Pos}) ->
    io_lib:format("~tp: Invalid format of BEAM file near byte number ~p~n", 
		  [File, Pos]);
format_error({chunk_too_big, File, ChunkId, Size, Len}) ->
    io_lib:format("~tp: Size of chunk \"~s\" is ~p bytes, "
		  "but only ~p bytes could be read~n",
		  [File, ChunkId, Size, Len]);
format_error({chunks_different, Id}) ->
    io_lib:format("Chunk \"~s\" differs in the two files~n", [Id]);
format_error(different_chunks) ->
    "The two files have different chunks\n";
format_error({modules_different, Module1, Module2}) ->
    io_lib:format("Module names ~p and ~p differ in the two files~n", 
		  [Module1, Module2]);
format_error({not_a_directory, Name}) ->
    io_lib:format("~tp: Not a directory~n", [Name]);
format_error({key_missing_or_invalid, File, ChunkId}) ->
    io_lib:format("~tp: Cannot decrypt ~ts because key is missing or invalid",
		  [File, ChunkId]);
format_error(badfun) ->
    "not a fun or the fun has the wrong arity";
format_error(exists) ->
    "a fun has already been installed";
format_error(E) ->
    io_lib:format("~tp~n", [E]).

%% 
%% Exported functions for encrypted debug info.
%%

-type mode()           :: 'des3_cbc'.
-type crypto_fun_arg() :: 'init'
                        | 'clear'
                        | {'debug_info', mode(), module(), file:filename()}.
-type crypto_fun()     :: fun((crypto_fun_arg()) -> term()).

-spec crypto_key_fun(CryptoKeyFun) -> 'ok' | {'error', Reason} when
      CryptoKeyFun :: crypto_fun(),
      Reason :: badfun | exists | term().

crypto_key_fun(F) ->
    call_crypto_server({crypto_key_fun, F}).

-spec clear_crypto_key_fun() -> 'undefined' | {'ok', Result} when
      Result :: 'undefined' | term().

clear_crypto_key_fun() ->
    call_crypto_server(clear_crypto_key_fun).

-spec make_crypto_key(mode(), string()) ->
        {mode(), [binary()], binary(), integer()}.

make_crypto_key(des3_cbc=Type, String) ->
    <<K1:8/binary,K2:8/binary>> = First = erlang:md5(String),
    <<K3:8/binary,IVec:8/binary>> = erlang:md5([First|reverse(String)]),
    {Type,[K1,K2,K3],IVec,8}.

-spec build_module(Chunks) -> {'ok', Binary} when
      Chunks :: [{chunkid(), dataB()}],
      Binary :: binary().

build_module(Chunks0) ->
    Chunks = list_to_binary(build_chunks(Chunks0)),
    Size = byte_size(Chunks),
    0 = Size rem 4, % Assertion: correct padding?
    {ok, <<"FOR1", (Size+4):32, "BEAM", Chunks/binary>>}.


%%
%%  Local functions
%%

read_info(File) ->
    try
        {ok, Module, Data} = scan_beam(File, info),
        [if
             is_binary(File) -> {binary, File};
             true -> {file, File}
         end, {module, Module}, {chunks, Data}]
    catch Error -> Error end.

diff_directories(Dir1, Dir2) ->
    {OnlyDir1, OnlyDir2, Diff} = compare_dirs(Dir1, Dir2),
    diff_only(Dir1, OnlyDir1),
    diff_only(Dir2, OnlyDir2),
    foreach(fun(D) -> io:format("** different: ~tp~n", [D]) end, Diff),
    ok.

diff_only(_Dir, []) -> 
    ok;
diff_only(Dir, Only) ->
    io:format("Only in ~tp: ~tp~n", [Dir, Only]).

%% -> {OnlyInDir1, OnlyInDir2, Different} | throw(Error)
compare_dirs(Dir1, Dir2) ->
    R1 = sofs:relation(beam_files(Dir1)),
    R2 = sofs:relation(beam_files(Dir2)),
    F1 = sofs:domain(R1),
    F2 = sofs:domain(R2),
    {O1, Both, O2} = sofs:symmetric_partition(F1, F2),
    OnlyL1 = sofs:image(R1, O1),
    OnlyL2 = sofs:image(R2, O2),
    B1 = sofs:to_external(sofs:restriction(R1, Both)),
    B2 = sofs:to_external(sofs:restriction(R2, Both)),
    Diff = compare_files(B1, B2, []),
    {sofs:to_external(OnlyL1), sofs:to_external(OnlyL2), Diff}.

compare_files([], [], Acc) ->
    lists:reverse(Acc);
compare_files([{_,F1} | R1], [{_,F2} | R2], Acc) ->
    NAcc = case catch cmp_files(F1, F2) of
	       {error, _Mod, _Reason} ->
		   [{F1, F2} | Acc];
	       ok ->
		   Acc
	   end,
    compare_files(R1, R2, NAcc).

beam_files(Dir) ->
    ok = assert_directory(Dir),
    L = filelib:wildcard(filename:join(Dir, "*.beam")),
    [{filename:basename(Path), Path} || Path <- L].

%% -> ok | throw(Error)
cmp_files(File1, File2) ->
    {ok, {M1, L1}} = read_all_but_useless_chunks(File1),
    {ok, {M2, L2}} = read_all_but_useless_chunks(File2),
    if
	M1 =:= M2 ->
	    cmp_lists(L1, L2);
	true ->
	    error({modules_different, M1, M2})
    end.

cmp_lists([], []) ->
    ok;
cmp_lists([{Id, C1} | R1], [{Id, C2} | R2]) ->
    if
	C1 =:= C2 ->
	    cmp_lists(R1, R2);
	true ->
	    error({chunks_different, Id})
    end;
cmp_lists(_, _) ->
    error(different_chunks).
    
strip_rel(Root, AdditionalChunks) ->
    ok = assert_directory(Root),
    strip_fils(filelib:wildcard(filename:join(Root, "lib/*/ebin/*.beam")), AdditionalChunks).

%% -> {ok, [{Mod, BinaryOrFileName}]} | throw(Error)
strip_fils(Files, AdditionalChunks) ->
    {ok, [begin {ok, Reply} = strip_file(F, AdditionalChunks), Reply end || F <- Files]}.

%% -> {ok, {Mod, FileName}} | {ok, {Mod, binary()}} | throw(Error)
strip_file(File, AdditionalChunks) ->
    {ok, {Mod, Chunks}} = read_significant_chunks(File, AdditionalChunks ++ significant_chunks()),
    {ok, Stripped0} = build_module(Chunks),
    Stripped = compress(Stripped0),
    case File of
	_ when is_binary(File) ->
	    {ok, {Mod, Stripped}};
	_ ->
	    FileName = beam_filename(File),
	    case file:open(FileName, [raw, binary, write]) of
		{ok, Fd} ->
		    case file:write(Fd, Stripped) of
			ok ->
			    ok = file:close(Fd),
			    {ok, {Mod, FileName}};
			Error ->
			    ok = file:close(Fd),
			    file_error(FileName, Error)
		    end;
		Error ->
		    file_error(FileName, Error)
	    end
    end.

build_chunks([{Id, Data} | Chunks]) ->
    BId = list_to_binary(Id),
    Size = byte_size(Data),
    Chunk = [<<BId/binary, Size:32>>, Data | pad(Size)],
    [Chunk | build_chunks(Chunks)];
build_chunks([]) -> 
    [].

pad(Size) ->
    case Size rem 4 of
	0 -> [];
	Rem -> lists:duplicate(4 - Rem, 0)
    end.

%% -> {ok, {Module, Chunks}} | throw(Error)
read_all_but_useless_chunks(File0) when is_atom(File0);
					is_list(File0);
					is_binary(File0) ->
    File = beam_filename(File0),
    {ok, Module, ChunkIds0} = scan_beam(File, info),
    ChunkIds = [Name || {Name,_,_} <- ChunkIds0,
			not is_useless_chunk(Name)],
    {ok, Module, Chunks} = scan_beam(File, ChunkIds),
    {ok, {Module, lists:reverse(Chunks)}}.

is_useless_chunk("CInf") -> true;
is_useless_chunk(_) -> false.

%% -> {ok, {Module, Chunks}} | throw(Error)
read_significant_chunks(File, ChunkList) ->
    case read_chunk_data(File, ChunkList, [allow_missing_chunks]) of
	{ok, {Module, Chunks0}} ->
	    Mandatory = mandatory_chunks(),
	    Chunks = filter_significant_chunks(Chunks0, Mandatory, File, Module),
	    {ok, {Module, Chunks}}
    end.

filter_significant_chunks([{_, Data}=Pair|Cs], Mandatory, File, Mod)
  when is_binary(Data) ->
    [Pair|filter_significant_chunks(Cs, Mandatory, File, Mod)];
filter_significant_chunks([{Id, missing_chunk}|Cs], Mandatory, File, Mod) ->
    case member(Id, Mandatory) of
	false ->
	    filter_significant_chunks(Cs, Mandatory, File, Mod);
	true ->
	    error({missing_chunk, File, Id})
    end;
filter_significant_chunks([], _, _, _) -> [].

filter_funtab([{"FunT"=Tag, <<L:4/binary, Data0/binary>>}|Cs]) ->
    Data = filter_funtab_1(Data0, <<0:32>>),
    Funtab = <<L/binary, (iolist_to_binary(Data))/binary>>,
    [{Tag, Funtab}|filter_funtab(Cs)];
filter_funtab([H|T]) ->
    [H|filter_funtab(T)];
filter_funtab([]) -> [].

filter_funtab_1(<<Important:20/binary,_OldUniq:4/binary,T/binary>>, Zero) ->
    [Important,Zero|filter_funtab_1(T, Zero)];
filter_funtab_1(Tail, _) when is_binary(Tail) -> [Tail].

read_all_chunks(File0) when is_atom(File0);
			    is_list(File0); 
			    is_binary(File0) ->
    try
        File = beam_filename(File0),
        {ok, Module, ChunkIds0} = scan_beam(File, info),
        ChunkIds = [Name || {Name,_,_} <- ChunkIds0],
        {ok, Module, Chunks} = scan_beam(File, ChunkIds),
        {ok, Module, lists:reverse(Chunks)}
    catch Error -> Error end.

read_chunk_data(File0, ChunkNames) ->
    try read_chunk_data(File0, ChunkNames, [])
    catch Error -> Error end.

%% -> {ok, {Module, Symbols}} | throw(Error)
read_chunk_data(File0, ChunkNames0, Options)
  when is_atom(File0); is_list(File0); is_binary(File0) ->
    File = beam_filename(File0),
    {ChunkIds, Names, Optional} = check_chunks(ChunkNames0, File, [], [], []),
    AllowMissingChunks = member(allow_missing_chunks, Options),
    {ok, Module, Chunks} = scan_beam(File, ChunkIds, AllowMissingChunks, Optional),
    AT = ets:new(beam_symbols, []),
    T = {empty, AT},
    try chunks_to_data(Names, Chunks, File, Chunks, Module, T, [])
    after ets:delete(AT) 
    end.

%% -> {ok, list()} | throw(Error)
check_chunks([atoms | Ids], File, IL, L, O) ->
    check_chunks(Ids, File, ["Atom", "AtU8" | IL],
		 [{atom_chunk, atoms} | L], ["Atom", "AtU8" | O]);
check_chunks([abstract_code | Ids], File, IL, L, O) ->
    check_chunks(Ids, File, ["Abst", "Dbgi" | IL],
		 [{abst_chunk, abstract_code} | L], ["Abst", "Dbgi" | O]);
check_chunks([ChunkName | Ids], File, IL, L, O) when is_atom(ChunkName) ->
    ChunkId = chunk_name_to_id(ChunkName, File),
    check_chunks(Ids, File, [ChunkId | IL], [{ChunkId, ChunkName} | L], O);
check_chunks([ChunkId | Ids], File, IL, L, O) -> % when is_list(ChunkId)
    check_chunks(Ids, File, [ChunkId | IL], [{ChunkId, ChunkId} | L], O);
check_chunks([], _File, IL, L, O) ->
    {lists:usort(IL), reverse(L), O}.

%% -> {ok, Module, Data} | throw(Error)
scan_beam(File, What) ->
    scan_beam(File, What, false, []).

%% -> {ok, Module, Data} | throw(Error)
scan_beam(File, What0, AllowMissingChunks, OptionalChunks) ->
    case scan_beam1(File, What0) of
	{missing, _FD, Mod, Data, What} when AllowMissingChunks ->
	    {ok, Mod, [{Id, missing_chunk} || Id <- What] ++ Data};
	{missing, FD, Mod, Data, What} ->
	    case What -- OptionalChunks of
		[] -> {ok, Mod, Data};
		[Missing | _] -> error({missing_chunk, filename(FD), Missing})
	    end;
	R ->
	    R
    end.

%% -> {ok, Module, Data} | throw(Error)
scan_beam1(File, What) ->
    FD = open_file(File),
    case catch scan_beam2(FD, What) of
	Error when error =:= element(1, Error) ->
	    throw(Error);
	R ->
	    R
    end.

scan_beam2(FD, What) ->
    case pread(FD, 0, 12) of
	{NFD, {ok, <<"FOR1", _Size:32, "BEAM">>}} ->
	    Start = 12,
	    scan_beam(NFD, Start, What, 17, []);
	_Error -> 
	    error({not_a_beam_file, filename(FD)})
    end.

scan_beam(_FD, _Pos, [], Mod, Data) when Mod =/= 17 ->
    {ok, Mod, Data};    
scan_beam(FD, Pos, What, Mod, Data) ->
    case pread(FD, Pos, 8) of
	{_NFD, eof} when Mod =:= 17 ->
	    error({missing_chunk, filename(FD), "Atom"});	    
	{_NFD, eof} when What =:= info ->
	    {ok, Mod, reverse(Data)};
	{NFD, eof} ->
	    {missing, NFD, Mod, Data, What};
	{NFD, {ok, <<IdL:4/binary, Sz:32>>}} ->
	    Id = binary_to_list(IdL),
	    Pos1 = Pos + 8,
	    Pos2 = (4 * trunc((Sz+3) / 4)) + Pos1,
	    get_data(What, Id, NFD, Sz, Pos1, Pos2, Mod, Data);
	{_NFD, {ok, _ChunkHead}} ->
	    error({invalid_beam_file, filename(FD), Pos})
    end.

get_atom_data(Cs, Id, FD, Size, Pos, Pos2, Data, Encoding) ->
    NewCs = del_chunk(Id, Cs),
    {NFD, Chunk} = get_chunk(Id, Pos, Size, FD),
    <<_Num:32, Chunk2/binary>> = Chunk,
    {Module, _} = extract_atom(Chunk2, Encoding),
    C = case Cs of
	    info -> 
		{Id, Pos, Size};
	    _ -> 
		{Id, Chunk}
	end,
    scan_beam(NFD, Pos2, NewCs, Module, [C | Data]).

get_data(Cs, "Atom" = Id, FD, Size, Pos, Pos2, _Mod, Data) ->
    get_atom_data(Cs, Id, FD, Size, Pos, Pos2, Data, latin1);
get_data(Cs, "AtU8" = Id, FD, Size, Pos, Pos2, _Mod, Data) ->
    get_atom_data(Cs, Id, FD, Size, Pos, Pos2, Data, utf8);
get_data(info, Id, FD, Size, Pos, Pos2, Mod, Data) ->
    scan_beam(FD, Pos2, info, Mod, [{Id, Pos, Size} | Data]);
get_data(Chunks, Id, FD, Size, Pos, Pos2, Mod, Data) ->
    {NFD, NewData} = case member(Id, Chunks) of
			 true ->
			     {FD1, Chunk} = get_chunk(Id, Pos, Size, FD),
			     {FD1, [{Id, Chunk} | Data]};
			 false ->
			     {FD, Data}
	      end,
    NewChunks = del_chunk(Id, Chunks),
    scan_beam(NFD, Pos2, NewChunks, Mod, NewData).
     
del_chunk(_Id, info) ->
    info;
del_chunk(Id, Chunks) ->
    delete(Id, Chunks).

%% -> {NFD, binary()} | throw(Error)
get_chunk(Id, Pos, Size, FD) ->
    case pread(FD, Pos, Size) of
	{NFD, eof} when Size =:= 0 -> % cannot happen
	    {NFD, <<>>};
	{_NFD, eof} when Size > 0 ->
	    error({chunk_too_big, filename(FD), Id, Size, 0});
	{_NFD, {ok, Chunk}} when Size > byte_size(Chunk) ->
	    error({chunk_too_big, filename(FD), Id, Size, byte_size(Chunk)});
	{NFD, {ok, Chunk}} -> % when Size =:= size(Chunk)
	    {NFD, Chunk}
    end.

chunks_to_data([{atom_chunk, Name} | CNs], Chunks, File, Cs, Module, Atoms, L) ->
    {NewAtoms, Ret} = chunk_to_data(Name, <<"">>, File, Cs, Atoms, Module),
    chunks_to_data(CNs, Chunks, File, Cs, Module, NewAtoms, [Ret | L]);
chunks_to_data([{abst_chunk, Name} | CNs], Chunks, File, Cs, Module, Atoms, L) ->
    DbgiChunk = proplists:get_value("Dbgi", Chunks, <<"">>),
    {NewAtoms, Ret} =
	case catch chunk_to_data(debug_info, DbgiChunk, File, Cs, Atoms, Module) of
	    {DbgiAtoms, {debug_info, {debug_info_v1, Backend, Metadata}}} ->
		case Backend:debug_info(erlang_v1, Module, Metadata, []) of
		    {ok, Code} -> {DbgiAtoms, {abstract_code, {raw_abstract_v1, Code}}};
		    {error, _} -> {DbgiAtoms, {abstract_code, no_abstract_code}}
		end;
            {error,beam_lib,{key_missing_or_invalid,Path,debug_info}} ->
                error({key_missing_or_invalid,Path,abstract_code});
	    _ ->
		AbstChunk = proplists:get_value("Abst", Chunks, <<"">>),
		chunk_to_data(Name, AbstChunk, File, Cs, Atoms, Module)
	end,
    chunks_to_data(CNs, Chunks, File, Cs, Module, NewAtoms, [Ret | L]);
chunks_to_data([{Id, Name} | CNs], Chunks, File, Cs, Module, Atoms, L) ->
    {_Id, Chunk} = lists:keyfind(Id, 1, Chunks),
    {NewAtoms, Ret} = chunk_to_data(Name, Chunk, File, Cs, Atoms, Module),
    chunks_to_data(CNs, Chunks, File, Cs, Module, NewAtoms, [Ret | L]);
chunks_to_data([], _Chunks, _File, _Cs, Module, _Atoms, L) ->
    {ok, {Module, reverse(L)}}.

chunk_to_data(attributes=Id, Chunk, File, _Cs, AtomTable, _Mod) ->
    try
	Term = binary_to_term(Chunk),
	{AtomTable, {Id, attributes(Term)}}
    catch
	error:badarg ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)})
    end;
chunk_to_data(compile_info=Id, Chunk, File, _Cs, AtomTable, _Mod) ->
    try
	{AtomTable, {Id, binary_to_term(Chunk)}}
    catch
	error:badarg ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)})
    end;
chunk_to_data(debug_info=Id, Chunk, File, _Cs, AtomTable, Mod) ->
    case Chunk of
	<<>> ->
	    {AtomTable, {Id, no_debug_info}};
	<<0:8,N:8,Mode0:N/binary,Rest/binary>> ->
	    Mode = binary_to_atom(Mode0, utf8),
	    Term = decrypt_chunk(Mode, Mod, File, Id, Rest),
	    {AtomTable, {Id, anno_from_term(Term)}};
	_ ->
	    case catch binary_to_term(Chunk) of
		{'EXIT', _} ->
		    error({invalid_chunk, File, chunk_name_to_id(Id, File)});
		Term ->
                    {AtomTable, {Id, anno_from_term(Term)}}
	    end
    end;
chunk_to_data(abstract_code=Id, Chunk, File, _Cs, AtomTable, Mod) ->
    %% Before Erlang/OTP 20.0.
    case Chunk of
	<<>> ->
	    {AtomTable, {Id, no_abstract_code}};
	<<0:8,N:8,Mode0:N/binary,Rest/binary>> ->
	    Mode = binary_to_atom(Mode0, utf8),
	    Term = decrypt_chunk(Mode, Mod, File, Id, Rest),
	    {AtomTable, {Id, old_anno_from_term(Term)}};
	_ ->
	    case catch binary_to_term(Chunk) of
		{'EXIT', _} ->
		    error({invalid_chunk, File, chunk_name_to_id(Id, File)});
		Term ->
                    try
                        {AtomTable, {Id, old_anno_from_term(Term)}}
                    catch
                        _:_ ->
                            error({invalid_chunk, File,
                                   chunk_name_to_id(Id, File)})
                    end
	    end
    end;
chunk_to_data(atoms=Id, _Chunk, _File, Cs, AtomTable0, _Mod) ->
    AtomTable = ensure_atoms(AtomTable0, Cs),
    Atoms = ets:tab2list(AtomTable),
    {AtomTable, {Id, lists:sort(Atoms)}};
chunk_to_data(ChunkName, Chunk, File,
	      Cs, AtomTable, _Mod) when is_atom(ChunkName) ->
    case catch symbols(Chunk, AtomTable, Cs, ChunkName) of
	{ok, NewAtomTable, S} ->
	    {NewAtomTable, {ChunkName, S}};
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(ChunkName, File)})
    end;
chunk_to_data(ChunkId, Chunk, _File, 
	      _Cs, AtomTable, _Module) when is_list(ChunkId) ->
    {AtomTable, {ChunkId, Chunk}}. % Chunk is a binary

chunk_name_to_id(indexed_imports, _) -> "ImpT";
chunk_name_to_id(imports, _)         -> "ImpT";
chunk_name_to_id(exports, _)         -> "ExpT";
chunk_name_to_id(labeled_exports, _) -> "ExpT";
chunk_name_to_id(locals, _)          -> "LocT";
chunk_name_to_id(labeled_locals, _)  -> "LocT";
chunk_name_to_id(attributes, _)      -> "Attr";
chunk_name_to_id(abstract_code, _)   -> "Abst";
chunk_name_to_id(debug_info, _)      -> "Dbgi";
chunk_name_to_id(compile_info, _)    -> "CInf";
chunk_name_to_id(Other, File) -> 
    error({unknown_chunk, File, Other}).

%% Extract attributes

attributes(Attrs) ->
    attributes(keysort(1, Attrs), []).

attributes([], R) ->
    reverse(R);
attributes(L, R) ->
    K = element(1, hd(L)),
    {L1, L2} = splitwith(fun(T) -> element(1, T) =:= K end, L),
    V = append([A || {_, A} <- L1]),
    attributes(L2, [{K, V} | R]).

%% Extract symbols

symbols(<<_Num:32, B/binary>>, AT0, Cs, Name) ->
    AT = ensure_atoms(AT0, Cs),
    symbols1(B, AT, Name, [], 1).

symbols1(<<I1:32, I2:32, I3:32, B/binary>>, AT, Name, S, Cnt) ->
    Symbol = symbol(Name, AT, I1, I2, I3, Cnt),
    symbols1(B, AT, Name, [Symbol|S], Cnt+1);
symbols1(<<>>, AT, _Name, S, _Cnt) ->
    {ok, AT, sort(S)}.

symbol(indexed_imports, AT, I1, I2, I3, Cnt) ->
    {Cnt, atm(AT, I1), atm(AT, I2), I3};
symbol(imports, AT, I1, I2, I3, _Cnt) ->
    {atm(AT, I1), atm(AT, I2), I3};
symbol(labeled_exports, AT, I1, I2, I3, _Cnt) ->
    {atm(AT, I1), I2, I3};
symbol(labeled_locals, AT, I1, I2, I3, _Cnt) ->
    {atm(AT, I1), I2, I3};
symbol(_, AT, I1, I2, _I3, _Cnt) ->
    {atm(AT, I1), I2}.

atm(AT, N) ->
    [{_N, S}] = ets:lookup(AT, N),
    S.

%% AT is updated.
ensure_atoms({empty, AT}, Cs) ->
    case lists:keyfind("AtU8", 1, Cs) of
	{_Id, AtomChunk} when is_binary(AtomChunk) ->
	    extract_atoms(AtomChunk, AT, utf8);
	_ ->
	    {_Id, AtomChunk} = lists:keyfind("Atom", 1, Cs),
	    extract_atoms(AtomChunk, AT, latin1)
    end,
    AT;
ensure_atoms(AT, _Cs) ->
    AT.

extract_atoms(<<_Num:32, B/binary>>, AT, Encoding) ->
    extract_atoms(B, 1, AT, Encoding).

extract_atoms(<<>>, _I, _AT, _Encoding) ->
    true;
extract_atoms(B, I, AT, Encoding) ->
    {Atom, B1} = extract_atom(B, Encoding),
    true = ets:insert(AT, {I, Atom}),
    extract_atoms(B1, I+1, AT, Encoding).

extract_atom(<<Len, B/binary>>, Encoding) ->
    <<SB:Len/binary, Tail/binary>> = B,
    {binary_to_atom(SB, Encoding), Tail}.

%%% Utils.

-record(bb, {pos = 0 :: integer(),
	     bin :: binary(),
	     source :: binary() | string()}).

open_file(<<"FOR1",_/binary>>=Binary) ->
    #bb{bin = Binary, source = Binary};
open_file(Binary0) when is_binary(Binary0) ->
    Binary = uncompress(Binary0),
    #bb{bin = Binary, source = Binary};
open_file(FileName) ->
    case file:open(FileName, [read, raw, binary]) of
	{ok, Fd} ->
	    read_all(Fd, FileName, []);
	Error ->
	    file_error(FileName, Error)
    end.

read_all(Fd, FileName, Bins) ->
    case file:read(Fd, 1 bsl 18) of
	{ok, Bin} ->
	    read_all(Fd, FileName, [Bin | Bins]);
	eof ->
	    ok = file:close(Fd),
	    #bb{bin = uncompress(reverse(Bins)), source = FileName};
	Error ->
	    ok = file:close(Fd),
	    file_error(FileName, Error)
    end.

pread(FD, AtPos, Size) ->
    #bb{pos = Pos, bin = Binary} = FD,
    Skip = AtPos-Pos,
    case Binary of
	<<_:Skip/binary, B:Size/binary, Bin/binary>> ->
	    NFD = FD#bb{pos = AtPos+Size, bin = Bin},
	    {NFD, {ok, B}};
	<<_:Skip/binary, Bin/binary>> when byte_size(Bin) > 0 ->
	    NFD = FD#bb{pos = AtPos+byte_size(Bin), bin = <<>>},
	    {NFD, {ok, Bin}};
        _ ->
            {FD, eof}
    end.

filename(BB) when is_binary(BB#bb.source) ->
    BB#bb.source;
filename(BB) -> 
    list_to_atom(BB#bb.source).    

beam_filename(Bin) when is_binary(Bin) ->
    Bin;
beam_filename(File) ->
    filename:rootname(File, ".beam") ++ ".beam".


uncompress(Binary0) ->
    {ok, Fd} = ram_file:open(Binary0, [write, binary]),
    {ok, _} = ram_file:uncompress(Fd),
    {ok, Binary} = ram_file:get_file(Fd),
    ok = ram_file:close(Fd),
    Binary.

compress(Binary0) ->
    {ok, Fd} = ram_file:open(Binary0, [write, binary]),
    {ok, _} = ram_file:compress(Fd),
    {ok, Binary} = ram_file:get_file(Fd),
    ok = ram_file:close(Fd),
    Binary.

%% -> ok | throw(Error)
assert_directory(FileName) ->
    case filelib:is_dir(FileName) of
	true ->
	    ok;
	false ->
	    error({not_a_directory, FileName})
    end.

-spec file_error(file:filename(), {'error',atom()}) -> no_return().

file_error(FileName, {error, Reason}) ->
    error({file_error, FileName, Reason}).

-spec error(term()) -> no_return().

error(Reason) ->
    throw({error, ?MODULE, Reason}).

%% The following chunks must be kept when stripping a BEAM file.

significant_chunks() ->
    ["Line" | md5_chunks()].

%% The following chunks are significant when calculating the MD5
%% for a module. They are listed in the order that they should be MD5:ed.

md5_chunks() ->
    ["Atom", "AtU8", "Code", "StrT", "ImpT", "ExpT", "FunT", "LitT"].

%% The following chunks are mandatory in every Beam file.

mandatory_chunks() ->
    ["Code", "ExpT", "ImpT", "StrT"].

%%% ====================================================================
%%% The rest of the file handles encrypted debug info.
%%%
%%% Encrypting the debug info is only useful if you want to
%%% have the debug info available all the time (maybe even in a live
%%% system), but don't want to risk that anyone else but yourself
%%% can use it.
%%% ====================================================================

-record(state, {crypto_key_f :: crypto_fun() | 'undefined'}).

-define(CRYPTO_KEY_SERVER, beam_lib__crypto_key_server).

decrypt_chunk(Type, Module, File, Id, Bin) ->
    try
	KeyString = get_crypto_key({debug_info, Type, Module, File}),
	{Type,Key,IVec,_BlockSize} = make_crypto_key(Type, KeyString),
	ok = start_crypto(),
	NewBin = crypto:block_decrypt(Type, Key, IVec, Bin),
	binary_to_term(NewBin)
    catch
	_:_ ->
	    error({key_missing_or_invalid, File, Id})
    end.

old_anno_from_term({raw_abstract_v1, Forms}) ->
    {raw_abstract_v1, anno_from_forms(Forms)};
old_anno_from_term({Tag, Forms}) when Tag =:= abstract_v1;
                                      Tag =:= abstract_v2 ->
    try {Tag, anno_from_forms(Forms)}
    catch
        _:_ ->
            {Tag, Forms}
    end;
old_anno_from_term(T) ->
    T.

anno_from_term({debug_info_v1=Tag1, erl_abstract_code=Tag2, {Forms, Opts}}) ->
    try {Tag1, Tag2, {anno_from_forms(Forms), Opts}}
    catch
        _:_ ->
            {Tag1, Tag2, {Forms, Opts}}
    end;
anno_from_term(T) ->
    T.

anno_from_forms(Forms0) ->
    %% Forms with record field types created before OTP 19.0 are
    %% replaced by well-formed record forms holding the type
    %% information.
    Forms = epp:restore_typed_record_fields(Forms0),
    [erl_parse:anno_from_term(Form) || Form <- Forms].

start_crypto() ->
    case crypto:start() of
	{error, {already_started, _}} ->
	    ok;
	ok ->
	    ok
    end.

get_crypto_key(What) ->
    call_crypto_server({get_crypto_key, What}).

call_crypto_server(Req) ->
    try 
	gen_server:call(?CRYPTO_KEY_SERVER, Req, infinity)
    catch
	exit:{noproc,_} ->
	    %% Not started.
	    call_crypto_server_1(Req);
	exit:{normal,_} ->
	    %% The process finished just as we called it.
	    call_crypto_server_1(Req)
    end.

call_crypto_server_1(Req) ->
    case gen_server:start({local,?CRYPTO_KEY_SERVER}, ?MODULE, [], []) of
	{ok, _} -> ok;
	{error, {already_started, _}} -> ok
    end,
    erlang:yield(),
    call_crypto_server(Req).

-spec init([]) -> {'ok', #state{}}.

init([]) ->
    {ok, #state{}}.

-type calls() :: 'clear_crypto_key_fun'
               | {'crypto_key_fun', _}
               | {'get_crypto_key', _}.

-spec handle_call(calls(), {pid(), term()}, #state{}) ->
        {'noreply', #state{}} |
	{'reply', 'error' | {'error','badfun' | 'exists'}, #state{}} |
	{'stop', 'normal', 'undefined' | {'ok', term()}, #state{}}.

handle_call({get_crypto_key, _}=R, From, #state{crypto_key_f=undefined}=S) ->
    case crypto_key_fun_from_file() of
	error ->
	    {reply, error, S};
	F when is_function(F) ->
	    %% The init function for the fun has already been called.
	    handle_call(R, From, S#state{crypto_key_f=F})
    end;
handle_call({get_crypto_key, What}, From, #state{crypto_key_f=F}=S) ->
    try
	Result = F(What),
	%% The result may hold information that we don't want 
	%% lying around. Reply first, then GC, then noreply.
	gen_server:reply(From, Result),
	erlang:garbage_collect(),
	{noreply, S}
    catch
	_:_ ->
	    {reply, error, S}
    end;
handle_call({crypto_key_fun, F}, {_,_} = From, S) ->
    case S#state.crypto_key_f of
	undefined ->
	    if is_function(F, 1) ->
		    {Result, Fun, Reply} = 
			case catch F(init) of
			    ok ->
				{true, F, ok};
			    {ok, F1} when is_function(F1) ->
				if
				    is_function(F1, 1) ->
					{true, F1, ok};
				    true ->
					{false, undefined, 
					 {error, badfun}}
				end;
			    {error, Reason} ->
				{false, undefined, {error, Reason}};
			    {'EXIT', Reason} ->
				{false, undefined, {error, Reason}}
			end,
		    gen_server:reply(From, Reply),
		    erlang:garbage_collect(),
		    NewS = case Result of
			       true ->
				   S#state{crypto_key_f = Fun};
			       false ->
				   S
			   end,
		    {noreply, NewS};
	       true ->
		    {reply, {error, badfun}, S}
	    end;
	OtherF when is_function(OtherF) ->
	    {reply, {error, exists}, S}
    end;
handle_call(clear_crypto_key_fun, _From, S) ->
    case S#state.crypto_key_f of
	undefined ->
	    {stop,normal,undefined,S};
	F ->
	    Result = (catch F(clear)),
	    {stop,normal,{ok,Result},S}
    end.

-spec handle_cast(term(), #state{}) -> {'noreply', #state{}}.

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {'noreply', #state{}}.

handle_info(_, State) ->
    {noreply, State}.

-spec code_change(term(), #state{}, term()) -> {'ok', #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(term(), #state{}) -> 'ok'.

terminate(_Reason, _State) ->
    ok.

crypto_key_fun_from_file() ->
    case init:get_argument(home) of
	{ok,[[Home]]} ->
	    crypto_key_fun_from_file_1([".",Home]);
	_ ->
	    crypto_key_fun_from_file_1(["."])
    end.

crypto_key_fun_from_file_1(Path) ->
    case f_p_s(Path, ".erlang.crypt") of
	{ok, KeyInfo, _} ->
	    try_load_crypto_fun(KeyInfo);
	_ ->
	    error
    end.

f_p_s(P, F) ->
    case file:path_script(P, F) of
	{error, enoent} ->
	    {error, enoent};
	{error, {Line, _Mod, _Term}=E} ->
	    error("file:path_script(~tp,~tp): error on line ~p: ~ts~n",
		  [P, F, Line, file:format_error(E)]),
	    ok;
	{error, E} when is_atom(E) ->
	    error("file:path_script(~tp,~tp): ~ts~n",
		  [P, F, file:format_error(E)]),
	    ok;
	Other ->
	    Other
    end.

try_load_crypto_fun(KeyInfo) when is_list(KeyInfo) ->
    T = ets:new(keys, [private, set]),
    foreach(
      fun({debug_info, Mode, M, Key}) when is_atom(M) ->
	      ets:insert(T, {{debug_info,Mode,M,[]}, Key});
	 ({debug_info, Mode, [], Key}) ->
	      ets:insert(T, {{debug_info, Mode, [], []}, Key});
	 (Other) ->
	      error("unknown key: ~p~n", [Other])
      end, KeyInfo),
    fun({debug_info, Mode, M, F}) ->
	    alt_lookup_key(
	      [{debug_info,Mode,M,F},
	       {debug_info,Mode,M,[]},
	       {debug_info,Mode,[],[]}], T);
       (clear) ->
	    ets:delete(T);
       (_) ->
	    error
    end;
try_load_crypto_fun(KeyInfo) ->
    error("unrecognized crypto key info: ~p\n", [KeyInfo]).

alt_lookup_key([H|T], Tab) ->
    case ets:lookup(Tab, H) of
	[] ->
	    alt_lookup_key(T, Tab);
	[{_, Val}] ->
	    Val
    end;
alt_lookup_key([], _) ->
    error.

error(Fmt, Args) ->
    error_logger:error_msg(Fmt, Args),
    error.
