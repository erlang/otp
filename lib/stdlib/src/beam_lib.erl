%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2024. All Rights Reserved.
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
-moduledoc """
This module provides an interface to files created by the BEAM Compiler ("BEAM
files").

The format used, a variant of "EA IFF 1985" Standard for Interchange Format Files,
divides data into chunks.

Chunk data can be returned as binaries or as compound terms. Compound terms are
returned when chunks are referenced by names (atoms) rather than identifiers
(strings). The recognized names and the corresponding identifiers are as
follows:

- `atoms ("Atom")`
- `attributes ("Attr")`
- `compile_info ("CInf")`
- `debug_info ("Dbgi")`
- `exports ("ExpT")`
- `imports ("ImpT")`
- `indexed_imports ("ImpT")`
- `labeled_exports ("ExpT")`
- `labeled_locals ("LocT")`
- `locals ("LocT")`
- `documentation ("Docs")`

[](){: #debug_info }

## Debug Information/Abstract Code

Option `debug_info` can be specified to the Compiler (see
[`compile`](`m:compile#debug_info`)) to have debug information, such as
[Erlang Abstract Format](`e:erts:absform.md`), stored in the `debug_info` chunk.
Tools such as Debugger and Xref require the debug information to be included.

> #### Warning {: .warning }
>
> Source code can be reconstructed from the debug information. To prevent this,
> use encrypted debug information (see below).

The debug information can also be removed from BEAM files using `strip/1`,
`strip_files/1`, and/or `strip_release/1`.

## Reconstruct Source Code

The following example shows how to reconstruct Erlang source code from the debug
information in a BEAM file `Beam`:

```erlang
{ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Beam,[abstract_code]).
io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).
```

## Encrypted Debug Information

The debug information can be encrypted to keep the source code secret, but still
be able to use tools such as Debugger or Xref.

To use encrypted debug information, a key must be provided to the compiler and
`beam_lib`. The key is specified as a string. It is recommended that the string
contains at least 32 characters and that both upper and lower case letters as
well as digits and special characters are used.

The default type (and currently the only type) of crypto algorithm is
`des3_cbc`, three rounds of DES. The key string is scrambled using
`erlang:md5/1` to generate the keys used for `des3_cbc`.

> #### Note {: .info }
>
> As far as we know by the time of writing, it is infeasible to break `des3_cbc`
> encryption without any knowledge of the key. Therefore, as long as the key is
> kept safe and is unguessable, the encrypted debug information _should_ be safe
> from intruders.

The key can be provided in the following two ways:

1. Use Compiler option `{debug_info_key,Key}`, see
   [`compile`](`m:compile#debug_info_key`) and function `crypto_key_fun/1` to
   register a fun that returns the key whenever `beam_lib` must decrypt the
   debug information.

If no such fun is registered, `beam_lib` instead searches for an `.erlang.crypt`
file, see the next section.

1. Store the key in a text file named `.erlang.crypt`.

In this case, Compiler option `encrypt_debug_info` can be used, see
[`compile`](`m:compile#encrypt_debug_info`).

## .erlang.crypt

`beam_lib` searches for `.erlang.crypt` in the current directory, then the
[user's home directory](`m:init#home`) and then
[`filename:basedir(user_config, "erlang")`](`m:filename#user_config`). If the
file is found and contains a key, `beam_lib` implicitly creates a crypto key fun
and registers it.

File `.erlang.crypt` is to contain a single list of tuples:

```erlang
{debug_info, Mode, Module, Key}
```

`Mode` is the type of crypto algorithm; currently, the only allowed value is
`des3_cbc`. `Module` is either an atom, in which case `Key` is only used for the
module `Module`, or `[]`, in which case `Key` is used for all modules. `Key` is
the non-empty key string.

`Key` in the first tuple where both `Mode` and `Module` match is used.

The following is an example of an `.erlang.crypt` file that returns the same key
for all modules:

```erlang
[{debug_info, des3_cbc, [], "%>7}|pc/DM6Cga*68$Mw]L#&_Gejr]G^"}].
```

The following is a slightly more complicated example of an `.erlang.crypt`
providing one key for module `t` and another key for all other modules:

```erlang
[{debug_info, des3_cbc, t, "My KEY"},
 {debug_info, des3_cbc, [], "%>7}|pc/DM6Cga*68$Mw]L#&_Gejr]G^"}].
```

> #### Note {: .info }
>
> Do not use any of the keys in these examples. Use your own keys.
""".
-behaviour(gen_server).

-include_lib("kernel/include/eep48.hrl").

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
-export_type([chunkid/0]).
-export_type([chnk_rsn/0]).
-export_type([beam/0]).

-import(lists, [append/1, delete/2, foreach/2, keysort/2, 
		member/2, reverse/1, sort/1, splitwith/2]).

%%-------------------------------------------------------------------------

-doc """
Each of the functions described below accept either the filename (as a string)
or a binary containing the BEAM module.
""".
-type beam() :: file:filename() | binary().
-doc """
The format stored in the `debug_info` chunk.

To retrieve particular code representation from the backend,
`Backend:debug_info(Format, Module, Data, Opts)` must be invoked. `Format` is an
atom, such as `erlang_v1` for the Erlang Abstract Format or `core_v1` for Core
Erlang. `Module` is the module represented by the beam file and `Data` is the
value stored in the debug info chunk. `Opts` is any list of values supported by
the `Backend`. `Backend:debug_info/4` must return `{ok, Code}` or
`{error, Term}`.

Developers must always invoke the `debug_info/4` function and never rely on the
`Data` stored in the `debug_info` chunk, as it is opaque and may change at any
moment. `no_debug_info` means that chunk `"Dbgi"` is present, but empty.
""".
-type debug_info() :: {DbgiVersion :: atom(), Backend :: module(), Data :: term()} | 'no_debug_info'.

-type forms()     :: [erl_parse:abstract_form() | erl_parse:form_info()].

-doc """
It is not checked that the forms conform to the abstract format indicated by
`AbstVersion`. `no_abstract_code` means that chunk `"Abst"` is present, but
empty.

For modules compiled with OTP 20 onwards, the `abst_code` chunk is automatically
computed from the `debug_info` chunk.
""".
-type abst_code() :: {AbstVersion :: atom(), forms()} | 'no_abstract_code'.
-type dataB()     :: binary().
-type index()     :: non_neg_integer().
-type label()     :: integer().

-doc """
`"Attr" | "CInf" | "Dbgi" | "ExpT" | "ImpT" | "LocT" | "AtU8" | "Docs"`
""".
-type chunkid()   :: nonempty_string(). % approximation of the strings below
%% "Abst" | "Dbgi" | "Attr" | "CInf" | "ExpT" | "ImpT" | "LocT" | "Atom" | "AtU8" | "Docs"
-type chunkname() :: 'abstract_code' | 'debug_info'
                   | 'attributes' | 'compile_info'
                   | 'exports' | 'labeled_exports'
                   | 'imports' | 'indexed_imports'
                   | 'locals' | 'labeled_locals'
                   | 'atoms' | 'documentation'.
-type chunkref()  :: chunkname() | chunkid().

-type attrib_entry()   :: {Attribute :: atom(), [AttributeValue :: term()]}.
-type compinfo_entry() :: {InfoKey :: atom(), term()}.
-type labeled_entry()  :: {Function :: atom(), arity(), label()}.

-doc "[EEP-48 documentation format](`e:kernel:eep48_chapter.md#the-docs-format`)".
-type docs() :: #docs_v1{}.

-doc """
The list of attributes is sorted on `Attribute` (in `t:attrib_entry/0`) and each
attribute name occurs once in the list. The attribute values occur in the same
order as in the file. The lists of functions are also sorted.
""".
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
                   | {'atoms', [{integer(), atom()}]}
                   | {'documentation', docs()}.

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
                   | {'missing_backend', file:filename(), module()}
                   | info_rsn().
-type cmp_rsn()   :: {'modules_different', module(), module()}
                   | {'chunks_different', chunkid()}
                   | 'different_chunks'
                   | info_rsn().

%%-------------------------------------------------------------------------

%%
%%  Exported functions
%%

-doc """
Returns a list containing some information about a BEAM file as tuples
`{Item, Info}`:

- **`{file, Filename} | {binary, Binary}`** - The name (string) of the BEAM
  file, or the binary from which the information was extracted.

- **`{module, Module}`** - The name (atom) of the module.

- **`{chunks, [{ChunkId, Pos, Size}]}`** - For each chunk, the identifier
  (string) and the position and size of the chunk data, in bytes.
""".
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

-doc """
Reads chunk data for selected chunks references. The order of the returned list
of chunk data is determined by the order of the list of chunks references.
""".
-spec chunks(Beam, ChunkRefs) ->
                    {'ok', {module(), [chunkdata()]}} |
                    {'error', 'beam_lib', chnk_rsn()} when
      Beam :: beam(),
      ChunkRefs :: [chunkref()].

chunks(File, Chunks) ->
    read_chunk_data(File, Chunks).

-doc """
Reads chunk data for selected chunks references. The order of the returned list
of chunk data is determined by the order of the list of chunks references.

By default, if any requested chunk is missing in `Beam`, an `error` tuple is
returned. However, if option `allow_missing_chunks` is specified, a result is
returned even if chunks are missing. In the result list, any missing chunks are
represented as `{ChunkRef,missing_chunk}`. Notice however that if chunk `"Atom"`
is missing, that is considered a fatal error and the return value is an `error`
tuple.
""".
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

-doc "Reads chunk data for all chunks.".
-doc(#{since => <<"OTP 18.2">>}).
-spec all_chunks(beam()) ->
           {'ok', module(), [{chunkid(), dataB()}]} | {'error', 'beam_lib', info_rsn()}.

all_chunks(File) ->
    read_all_chunks(File).

-doc """
Compares the contents of two BEAM files.

If the module names are the same, and all chunks except for chunk `"CInf"`
(the chunk containing the compilation information that is returned by
`Module:module_info(compile)`) have the same contents in both files, `ok` is
returned. Otherwise an error message is returned.
""".
-spec cmp(Beam1, Beam2) -> 'ok' | {'error', 'beam_lib', cmp_rsn()} when
      Beam1 :: beam(),
      Beam2 :: beam().

cmp(File1, File2) ->
    try cmp_files(File1, File2)
    catch Error -> Error end.

-doc """
Compares the BEAM files in two directories.

Only files with extension `".beam"` are compared. BEAM files that exist only in
directory `Dir1` (`Dir2`) are returned in `Only1` (`Only2`). BEAM files that
exist in both directories but are considered different by [`cmp/2`](`cmp/2`) are
 returned as pairs \{`Filename1`, `Filename2`\}, where `Filename1` (`Filename2`)
exists in directory `Dir1` (`Dir2`).
""".
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

-doc """
Compares the BEAM files in two directories as `cmp_dirs/2`, but the names of
files that exist in only one directory or are different are presented on
standard output.
""".
-spec diff_dirs(Dir1, Dir2) -> 'ok' | {'error', 'beam_lib', Reason} when
      Dir1 :: atom() | file:filename(),
      Dir2 :: atom() | file:filename(),
      Reason :: {'not_a_directory', term()} | info_rsn().

diff_dirs(Dir1, Dir2) ->
    catch diff_directories(Dir1, Dir2).

-doc """
Removes all chunks from a BEAM file except those used by the loader.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is
removed.
""".
-spec strip(Beam1) ->
        {'ok', {module(), Beam2}} | {'error', 'beam_lib', info_rsn()} when
      Beam1 :: beam(),
      Beam2 :: beam().

strip(FileName) ->
    strip(FileName, []).

-doc """
Removes all chunks from a BEAM file except those used by the loader or mentioned
in `AdditionalChunks`.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is removed.
""".
-doc(#{since => <<"OTP 22.0">>}).
-spec strip(Beam1, AdditionalChunks) ->
        {'ok', {module(), Beam2}} | {'error', 'beam_lib', info_rsn()} when
      Beam1 :: beam(),
      AdditionalChunks :: [chunkid()],
      Beam2 :: beam().

strip(FileName, AdditionalChunks) ->
    try strip_file(FileName, AdditionalChunks)
    catch Error -> Error end.
    
-doc """
Removes all chunks except those used by the loader from `Files`.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is
removed. The returned list contains one element for each specified filename, in
the same order as in `Files`.
""".
-spec strip_files(Files) ->
        {'ok', [{module(), Beam}]} | {'error', 'beam_lib', info_rsn()} when
      Files :: [beam()],
      Beam :: beam().

strip_files(Files) ->
    strip_files(Files, []).

-doc """
Removes all chunks except those used by the loader or mentioned in
`AdditionalChunks` from `Files`.

In particular, the debug information (chunk `debug_info` and `abstract_code`) is
removed. The returned list contains one element for each specified filename,
in the same order as in `Files`.
""".
-doc(#{since => <<"OTP 22.0">>}).
-spec strip_files(Files, AdditionalChunks) ->
        {'ok', [{module(), Beam}]} | {'error', 'beam_lib', info_rsn()} when
      Files :: [beam()],
      AdditionalChunks :: [chunkid()],
      Beam :: beam().

strip_files(Files, AdditionalChunks) when is_list(Files) ->
    try strip_fils(Files, AdditionalChunks)
    catch Error -> Error end.

-doc """
Removes all chunks except those used by the loader from the BEAM files of a
release.

`Dir` is to be the installation root directory. For example, the current OTP
release can be stripped with the call `beam_lib:strip_release(code:root_dir())`.
""".
-spec strip_release(Dir) ->
        {'ok', [{module(), file:filename()}]}
      | {'error', 'beam_lib', Reason} when
      Dir :: atom() | file:filename(),
      Reason :: {'not_a_directory', term()} | info_rsn().

strip_release(Root) ->
    strip_release(Root, []).

-doc """
Removes all chunks except those used by the loader or mentioned in
`AdditionalChunks`.

`Dir` is to be the installation root directory. For example, the current OTP
release can be stripped with the call `beam_lib:strip_release(code:root_dir(),[documentation])`.
""".
-doc(#{since => <<"OTP 22.0">>}).
-spec strip_release(Dir, AdditionalChunks) ->
        {'ok', [{module(), file:filename()}]}
      | {'error', 'beam_lib', Reason} when
      Dir :: atom() | file:filename(),
      AdditionalChunks :: [chunkid()],
      Reason :: {'not_a_directory', term()} | info_rsn().

strip_release(Root, AdditionalChunks) ->
    catch strip_rel(Root, AdditionalChunks).

-doc """
Returns the module version or versions. A version is defined by module attribute
`-vsn(Vsn)`.

If this attribute is not specified, the version defaults to the
checksum of the module. Notice that if version `Vsn` is not a list, it is made
into one, that is `{ok,{Module,[Vsn]}}` is returned. If there are many `-vsn`
module attributes, the result is the concatenated list of versions.

_Examples:_

```erlang
1> beam_lib:version(a). % -vsn(1).
{ok,{a,[1]}}
2> beam_lib:version(b). % -vsn([1]).
{ok,{b,[1]}}
3> beam_lib:version(c). % -vsn([1]). -vsn(2).
{ok,{c,[1,2]}}
4> beam_lib:version(d). % no -vsn attribute
{ok,{d,[275613208176997377698094100858909383631]}}
```
""".
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

-doc """
Calculates an MD5 redundancy check for the code of the module (compilation date
and other attributes are not included).
""".
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

-doc """
For a specified error returned by any function in this module, this function
returns a descriptive string of the error in English. For file errors, function
[`file:format_error(Posix)`](`file:format_error/1`) is to be called.
""".
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
format_error({missing_backend, File, Backend}) ->
    io_lib:format("~tp: Cannot retrieve abstract code because the backend ~p is missing",
		  [File, Backend]);
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

-doc """
Registers an unary fun that is called if `beam_lib` must read an `debug_info`
chunk that has been encrypted. The fun is held in a process that is started by
the function.

If a fun is already registered when attempting to register a fun,
`{error, exists}` is returned.

The fun must handle the following arguments:

```erlang
CryptoKeyFun(init) -> ok | {ok, NewCryptoKeyFun} | {error, Term}
```

Called when the fun is registered, in the process that holds the fun. Here the
crypto key fun can do any necessary initializations. If `{ok, NewCryptoKeyFun}`
is returned, `NewCryptoKeyFun` is registered instead of `CryptoKeyFun`. If
`{error, Term}` is returned, the registration is aborted and
[`crypto_key_fun/1`](`crypto_key_fun/1`) also returns `{error, Term}`.

```erlang
CryptoKeyFun({debug_info, Mode, Module, Filename}) -> Key
```

Called when the key is needed for module `Module` in the file named `Filename`.
`Mode` is the type of crypto algorithm; currently, the only possible value is
`des3_cbc`. The call is to fail (raise an exception) if no key is available.

```text
CryptoKeyFun(clear) -> term()
```

Called before the fun is unregistered. Here any cleaning up can be done. The
return value is not important, but is passed back to the caller of
`clear_crypto_key_fun/0` as part of its return value.
""".
-spec crypto_key_fun(CryptoKeyFun) -> 'ok' | {'error', Reason} when
      CryptoKeyFun :: crypto_fun(),
      Reason :: badfun | exists | term().

crypto_key_fun(F) ->
    call_crypto_server({crypto_key_fun, F}).

-doc """
Unregisters the crypto key fun and terminates the process holding it, started by
`crypto_key_fun/1`.

Returns either `{ok, undefined}` if no crypto key fun is registered, or
`{ok, Term}`, where `Term` is the return value from `CryptoKeyFun(clear)`, see
[`crypto_key_fun/1`](`crypto_key_fun/1`).
""".
-spec clear_crypto_key_fun() -> 'undefined' | {'ok', Result} when
      Result :: 'undefined' | term().

clear_crypto_key_fun() ->
    call_crypto_server(clear_crypto_key_fun).

-doc false.
-spec make_crypto_key(mode(), string()) ->
        {mode(), [binary()], binary(), integer()}.

make_crypto_key(des3_cbc=Type, String) ->
    <<K1:8/binary,K2:8/binary>> = First = erlang:md5(String),
    <<K3:8/binary,IVec:8/binary>> = erlang:md5([First|reverse(String)]),
    {Type,[K1,K2,K3],IVec,8}.

-doc "Builds a BEAM module (as a binary) from a list of chunks.".
-doc(#{since => <<"OTP 18.2">>}).
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
		try Backend:debug_info(erlang_v1, Module, Metadata, []) of
		    {ok, Code} -> {DbgiAtoms, {abstract_code, {raw_abstract_v1, Code}}};
		    {error, _} -> {DbgiAtoms, {abstract_code, no_abstract_code}}
                catch
                    error:undef ->
                        error({missing_backend,File,Backend})
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

chunk_to_data(Id, missing_chunk, _File, _Cs, AtomTable, _Mod) ->
    %% Missing chunk, only happens when 'allow_missing_chunks' is on.
    {AtomTable, {Id, missing_chunk}};
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
chunk_to_data(documentation=Id, Chunk, File, _Cs, AtomTable, _Mod) ->
    try
        case binary_to_term(Chunk) of
            #docs_v1{} = Term ->
                {AtomTable, {Id, Term}};
            _ ->
                error({invalid_chunk, File, chunk_name_to_id(Id, File)})
        end
    catch
	error:badarg ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)})
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
chunk_name_to_id(documentation, _)   -> "Docs";
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
    ets:lookup_element(AT, N, 2).

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

open_file(Binary0) when is_binary(Binary0) ->
    Binary = maybe_uncompress(Binary0),
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
	    #bb{bin = maybe_uncompress(reverse(Bins)), source = FileName};
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

%% Do not attempt to uncompress if we have the proper .beam format.
%% This clause matches binaries given as input.
maybe_uncompress(<<"FOR1",_/binary>>=Binary) ->
    Binary;
%% This clause matches the iolist read from files.
maybe_uncompress([<<"FOR1",_/binary>>|_]=IOData) ->
    iolist_to_binary(IOData);
maybe_uncompress(IOData) ->
    try
	zlib:gunzip(IOData)
    catch
	_:_ -> iolist_to_binary(IOData)
    end.

compress(IOData) ->
    zlib:gzip(IOData).

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

-doc false.
significant_chunks() ->
    ["Line", "Type" | md5_chunks()].

%% The following chunks are significant when calculating the MD5
%% for a module. They are listed in the order that they should be MD5:ed.

md5_chunks() ->
    ["Atom", "AtU8", "Code", "StrT", "ImpT", "ExpT", "FunT", "LitT", "Meta"].

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
	NewBin = crypto:crypto_one_time(des_ede3_cbc, Key, IVec, Bin, false),
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

-doc false.
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

-doc false.
-spec init([]) -> {'ok', #state{}}.

init([]) ->
    {ok, #state{}}.

-type calls() :: 'clear_crypto_key_fun'
               | {'crypto_key_fun', _}
               | {'get_crypto_key', _}.

-doc false.
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

-doc false.
-spec handle_cast(term(), #state{}) -> {'noreply', #state{}}.

handle_cast(_, State) ->
    {noreply, State}.

-doc false.
-spec handle_info(term(), #state{}) -> {'noreply', #state{}}.

handle_info(_, State) ->
    {noreply, State}.

-doc false.
-spec code_change(term(), #state{}, term()) -> {'ok', #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-doc false.
-spec terminate(term(), #state{}) -> 'ok'.

terminate(_Reason, _State) ->
    ok.

crypto_key_fun_from_file() ->
    UserConfig = filename:basedir(user_config,"erlang"),
    case init:get_argument(home) of
	{ok,[[Home]]} ->
	    crypto_key_fun_from_file_1([".", Home, UserConfig]);
	_ ->
	    crypto_key_fun_from_file_1([".", UserConfig])
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
