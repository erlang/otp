%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(code).
-moduledoc """
Interface to the Erlang code server process.

This module contains the interface to the Erlang _code server_, which deals with
the loading of compiled code into a running Erlang runtime system.

The runtime system can be started in _interactive_ or _embedded_ mode. Which one
is decided by the command-line flag `-mode`:

```bash
% erl -mode embedded
```

The modes are as follows:

- In _interactive_ mode, which is default, only the modules needed by
  the runtime system are loaded during system startup. Other code is
  dynamically loaded when first referenced. When a call to a function
  in a certain module is made, and that module is not loaded, the code
  server searches for and tries to load that module.

- In _embedded_ mode, modules are not auto-loaded. Trying to use a
  module that has not been loaded results in an error. This mode is
  recommended when the boot script loads all modules, as it is
  typically done in OTP releases. (Code can still be loaded later by
  explicitly ordering the code server to do so).

To prevent accidentally reloading of modules affecting the Erlang runtime
system, directories `kernel`, `stdlib`, and `compiler` are considered _sticky_.
This means that the system issues a warning and rejects the request if a user
tries to reload a module residing in any of them. The feature can be disabled by
using command-line flag `-nostick`.

## Code Path

In interactive mode, the code server maintains a _code path_,
consisting of a list of directories, which it searches sequentially
when trying to load a module.

Initially, the code path consists of the current working directory and all
Erlang object code directories under library directory `$OTPROOT/lib`, where
`$OTPROOT` is the installation directory of Erlang/OTP, `code:root_dir()`.
Directories can be named `Name[-Vsn]` and the code server, by default, chooses
the directory with the highest version number among those having the same
`Name`. Suffix `-Vsn` is optional. If an `ebin` directory exists under
`Name[-Vsn]`, this directory is added to the code path.

Environment variable `ERL_LIBS` (defined in the operating system) can be used to
define more library directories to be handled in the same way as the standard
OTP library directory described above, except that directories without an `ebin`
directory are ignored.

All application directories found in the additional directories appear before
the standard OTP applications, except for the Kernel and STDLIB applications,
which are placed before any additional applications. In other words, modules
found in any of the additional library directories override modules with the
same name in OTP, except for modules in Kernel and STDLIB.

Environment variable `ERL_LIBS` (if defined) is to contain a colon-separated
(for Unix-like systems) or semicolon-separated (for Windows) list of additional
libraries.

_Example:_

On a Unix-like system, `ERL_LIBS` can be set to the following:

```text
/usr/local/jungerl:/home/some_user/my_erlang_lib
```

The code paths specified by `$OTPROOT`, `ERL_LIBS`, and boot scripts have their
listings cached by default (except for `"."`) The code server will
lookup the contents in their directories once and avoid future file system
traversals. Therefore, modules added to such directories after the Erlang VM
boots will not be picked up. This behaviour can be disabled by setting
`-cache_boot_paths false` or by calling `code:set_path(code:get_path())`.

> #### Change {: .info }
>
> The support for caching directories in the code path was added
> in Erlang/OTP 26.

Directories given by the command line options `-pa` and `-pz` are not
cached by default. Many of the functions that manipulate the code path
accept the `cache` atom as an optional argument to enable caching
selectively.

## Loading of Code From Archive Files

> #### Change {: .info }
>
> The existing experimental support for archive files will be changed
> in a future release. As of Erlang/OTP 27, the function `code:lib_dir/2`,
> the `-code_path_choice` flag, and using `m:erl_prim_loader` for
> reading files from an archive are deprecated.
>
> `escript` scripts that use archive files should use
> `escript:extract/2` to read data files from its archive instead of using
> `code:lib_dir/2` and `m:erl_prim_loader`.

The Erlang archives are `ZIP` files with extension `.ez`. Erlang archives can
also be [enclosed in `escript`](`m:escript`) files whose file extension is arbitrary.

Erlang archive files can contain entire Erlang applications or parts of
applications. The structure in an archive file is the same as the directory
structure for an application. If you, for example, create an archive of
`mnesia-4.4.7`, the archive file must be named `mnesia-4.4.7.ez` and it must
contain a top directory named `mnesia-4.4.7`. If the version part of the name is
omitted, it must also be omitted in the archive. That is, a `mnesia.ez` archive
must contain a `mnesia` top directory.

An archive file for an application can, for example, be created like this:

```erlang
zip:create("mnesia-4.4.7.ez",
	["mnesia-4.4.7"],
	[{cwd, code:lib_dir()},
	 {compress, all},
	 {uncompress,[".beam",".app"]}]).
```

Any file in the archive can be compressed, but to speed up the access of
frequently read files, it can be a good idea to store `beam` and `app` files
uncompressed in the archive.

Normally the top directory of an application is located in library directory
`$OTPROOT/lib` or in a directory referred to by environment variable `ERL_LIBS`.
At startup, when the initial code path is computed, the code server also looks
for archive files in these directories and possibly adds `ebin` directories in
archives to the code path. The code path then contains paths to directories that
look like `$OTPROOT/lib/mnesia.ez/mnesia/ebin` or
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin`.

The code server uses module `erl_prim_loader` in ERTS (possibly through
`erl_boot_server`) to read code files from archives. However, the functions in
`erl_prim_loader` can also be used by other applications to read files from
archives. For example, the call
`erl_prim_loader:list_dir( "/otp/root/lib/mnesia-4.4.7.ez/mnesia-4.4.7/examples/bench)"`
would list the contents of a directory inside an archive. See
`m:erl_prim_loader`.

An application archive file and a regular application directory can coexist.
This can be useful when it is needed to have parts of the application as regular
files. A typical case is the `priv` directory, which must reside as a regular
directory to link in drivers dynamically and start port programs. For other
applications that do not need this, directory `priv` can reside in the archive
and the files under the directory `priv` can be read through `erl_prim_loader`.

When a directory is added to the code path and when the entire code path is
(re)set, the code server decides which subdirectories in an application that are
to be read from the archive and which that are to be read as regular files. If
directories are added or removed afterwards, the file access can fail if the
code path is not updated (possibly to the same path as before, to trigger the
directory resolution update).

For each directory on the second level in the application archive (`ebin`,
`priv`, `src`, and so on), the code server first chooses the regular directory
if it exists and second from the archive. Function `code:lib_dir/2` returns the
path to the subdirectory. For example, `code:lib_dir(megaco, ebin)` can return
`/otp/root/lib/megaco-3.9.1.1.ez/megaco-3.9.1.1/ebin` while
`code:lib_dir(megaco, priv)` can return `/otp/root/lib/megaco-3.9.1.1/priv`.

When an `escript` file contains an archive, there are no restrictions on the
name of the `escript` and no restrictions on how many applications that can be
stored in the embedded archive. Single Beam files can also reside on the top
level in the archive. At startup, the top directory in the embedded archive and
all (second level) `ebin` directories in the embedded archive are added to the
code path. See [`escript`](`e:erts:escript_cmd.md`).

A future-proof way for `escript` scripts to read data files from the archive is
to use the `escript:extract/2` function.

When the choice of directories in the code path is `strict` (which is
the default as of Erlang/OTP 27), the directory that ends up in the
code path is exactly the stated one. This means that if, for example,
the directory `$OTPROOT/lib/mnesia-4.4.7/ebin` is explicitly added to
the code path, the code server does not load files from
`$OTPROOT/lib/mnesia-4.4.7.ez/mnesia-4.4.7/ebin`.

This behavior can be controlled through command-line flag
`-code_path_choice Choice`. If the flag is set to `relaxed`, the code server
instead chooses a suitable directory depending on the actual file structure. If
a regular application `ebin` directory exists, it is chosen. Otherwise, the
directory `ebin` in the archive is chosen if it exists. If neither of them
exists, the original directory is chosen.

Command-line flag `-code_path_choice Choice` also affects how module `init`
interprets the `boot script`. The interpretation of the explicit code paths in
the `boot script` can be `strict` or `relaxed`. It is particularly useful to set
the flag to `relaxed` when elaborating with code loading from archives without
editing the `boot script`. The default has changed to `strict` in OTP 27 and the
option is scheduled for removal in OTP 28. See module `m:init` in the
Erts application.

## Current and Old Code

The code for a module can exist in two variants in a system: _current code_ and
_old code_. When a module is loaded into the system for the first time, the
module code becomes *current* and the global _export table_ is updated with
references to all functions exported from the module.

When a new instance of the module is loaded, the code of the previous
instance becomes *old*, and all export entries referring to the
previous instance are removed. After that, the new instance is loaded
as for the first time, and becomes current.

Both old and current code for a module are valid, and can even be executed
concurrently. The difference is that exported functions in old code are
unavailable. Hence, a global call cannot be made to an exported function in old
code, but old code can still be executed because of processes lingering in it.

If a third instance of the module is loaded, the code server removes (purges)
the old code and any processes lingering in it are terminated. Then the third
instance becomes current and the previously current code becomes old.

For more information about old and current code, and how to make a process
switch from old to current code, see section Compilation and Code Loading in the
[Erlang Reference Manual](`e:system:code_loading.md`).

## Native Coverage Support

In runtime systems that use the JIT, native coverage is a light-weight
way to find out which functions or lines that have been executed, or
how many times each function or line has been executed.

> #### Change {: .info }
>
> The support for native coverage was added in Erlang/OTP 27.

Native coverage works by instrumenting code at load-time. When a
module has been instrumented for native coverage collection it is not
possible to later disable the coverage collection, except by reloading
the module. However, the overhead for keeping coverage collection
running is often neligible, especially for [coverage
mode](`t:coverage_mode/0`) `function` that only keeps track of which
functions that have been executed.

The `m:cover` tool in the Tools application will automatically use the
native coverage support if the runtime system supports it.

It is only necessary to use the functionality described next if
`m:cover` is not sufficient, for example:

* If one wants to collect coverage information for the code that runs
  when the runtime system is starting (module `m:init` and so on).
  `m:cover` can only be used when the Erlang system has started, and
  it will reload every module that is to be analyzed.

* If it is necessary to collect coverage information with the absolute
  minimum disturbance of the test system. `m:cover` always counts how
  many times each line is executed (coverage mode `line_counters`),
  but by using native coverage one can use a less expensive coverage
  mode such as `function`, which has almost negligible overhead.

### Short summary of using native coverage

If the `line` or `line_counters` coverage mode is to be used,
the code to be tested must be compiled with option
[`line_coverage`](`e:compiler:compile#line_coverage`).

Use [set_coverage_mode(Mode)](`set_coverage_mode/1`) to set a
[coverage mode](`t:coverage_mode/0`) for all code subsequently
loaded, or set it with option [\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`)
for `erl`.

Optionally reset coverage information for all
modules that are to be tested by calling
[reset_coverage(Module)](`reset_coverage/1`).

Run the code whose coverage information is to be collected.

Read out the counters for all interesting modules by calling
[get_coverage(Level, Module)](`get_coverage/2`), where `Level`
is either `function` or `line`.

### The other native coverage BIFs

The following BIFs are sometimes useful, for example to fail gracefully
if the runtime system does not support native coverage:

* [coverage_support()](`coverage_support/0`) - check whether
  the runtime system supports native coverage

* [get_coverage_mode()](`get_coverage_mode/0`) - get the current
  coverage mode

* [get_coverage_mode(Module)](`get_coverage_mode/1`) - get the coverage
  mode for module `Module`

## Argument Types and Invalid Arguments

Module and application names are atoms, while file and directory names are
strings. For backward compatibility reasons, some functions accept both strings
and atoms, but a future release will probably only allow the arguments that are
documented.

Functions in this module generally fail with an exception if they are passed an
incorrect type (for example, an integer or a tuple where an atom is expected).
An error tuple is returned if the argument type is correct, but there are some
other errors (for example, a non-existing directory is specified to
[`set_path/1`](`set_path/1`)).

[](){: #error_reasons }

## Error Reasons for Code-Loading Functions

Functions that load code (such as [`load_file/1`](`load_file/1`)) will return
`{error,Reason}` if the load operation fails. Here follows a description of the
common reasons.

- **`badfile`** - The object code has an incorrect format or the module name in
  the object code is not the expected module name.

- **`nofile`** - No file with object code was found.

- **`not_purged`** - The object code could not be loaded because an old version
  of the code already existed.

- **`on_load_failure`** - The module has an
  [\-on_load function](`e:system:code_loading.md#on_load`) that failed when it
  was called.

- **`sticky_directory`** - The object code resides in a sticky directory.
""".

-include_lib("kernel/include/logger.hrl").
-include("eep48.hrl").

%% This is the interface module to the code server. It also contains
%% some implementation details.  See also related modules: code_*.erl
%% in this directory.

-export([objfile_extension/0,
	 set_path/1, set_path/2,
	 get_path/0,
	 load_file/1,
	 ensure_loaded/1,
	 ensure_modules_loaded/1,
	 load_abs/1,
	 load_abs/2,
	 load_binary/3,
	 atomic_load/1,
	 prepare_loading/1,
	 finish_loading/1,
	 delete/1,
	 purge/1,
	 soft_purge/1,
	 is_loaded/1,
	 all_loaded/0,
         all_available/0,
	 stop/0,
	 root_dir/0,
	 lib_dir/0,
	 lib_dir/1,
	 lib_dir/2,
	 compiler_dir/0,
	 priv_dir/1,
	 stick_dir/1,
	 unstick_dir/1,
	 stick_mod/1,
	 unstick_mod/1,
	 is_sticky/1,
	 get_object_code/1,
	 add_paths/1, add_paths/2,
         add_path/1, add_path/2,
	 add_pathsa/1, add_pathsa/2,
         add_pathsz/1, add_pathsz/2,
	 add_patha/1, add_patha/2,
	 add_pathz/1, add_pathz/2,
	 del_path/1,
         del_paths/1,
         clear_cache/0,
	 replace_path/2,replace_path/3,
	 start_link/0,
	 which/1,
         get_doc/1,
         get_doc/2,
	 where_is_file/1,
	 where_is_file/2,
	 clash/0,
         module_status/0,
         module_status/1,
         modified_modules/0,
         get_mode/0]).

-removed({rehash,0,"the code path cache feature has been removed"}).
-removed({is_module_native,1,"HiPE has been removed"}).

-deprecated([{lib_dir,2,"this functionality will be removed in a future release"}]).

-export_type([load_error_rsn/0, load_ret/0]).
-export_type([prepared_code/0]).
-export_type([module_status/0]).
-include_lib("kernel/include/file.hrl").

-export_type([coverage_mode/0]).
-type coverage_mode() :: 'none' | 'function' | 'function_counters' |
                         'line_coverage' | 'line_counters'.

-export([coverage_support/0,
         get_coverage/2,
         get_coverage_mode/0,
         get_coverage_mode/1,
         reset_coverage/1,
         set_coverage_mode/1]).

%%----------------------------------------------------------------------------
%% Some types for basic exported functions of this module
%%----------------------------------------------------------------------------

-define(is_cache(T), T =:= cache orelse T =:= nocache).
-type cache() :: cache | nocache.
-type load_error_rsn() :: 'badfile'
                        | 'nofile'
                        | 'not_purged'
                        | 'on_load_failure'
                        | 'sticky_directory'.
-type load_ret() :: {'error', What :: load_error_rsn()}
                  | {'module', Module :: module()}.
-type loaded_ret_atoms() :: 'cover_compiled' | 'preloaded'.
-type loaded_filename() :: (Filename :: file:filename()) | loaded_ret_atoms().

-define(PREPARED, '$prepared$').
-doc "An opaque term holding prepared code.".
-opaque prepared_code() ::
	  {?PREPARED,[{module(),{binary(),string(),_}}]}.


%%% BIFs

-export([get_chunk/2, module_md5/1]).

-doc false.
-spec get_chunk(Bin, Chunk) ->
                       binary() | undefined when
      Bin :: binary(),
      Chunk :: string().

get_chunk(<<"FOR1", _/bits>>=Beam, Chunk) ->
    get_chunk_1(Beam, Chunk);
get_chunk(Beam, Chunk) ->
    %% Corrupt header or compressed module, decompress it before passing it to
    %% the loader and let the BIF signal any errors.
    get_chunk_1(try_decompress(Beam), Chunk).

get_chunk_1(Beam, Chunk) ->
    try
        erts_internal:beamfile_chunk(Beam, Chunk)
    catch
        error:Reason ->
            {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
                (catch erlang:error(new_stacktrace, [Beam, Chunk])),
            erlang:raise(error, Reason, [{Mod,get_chunk,L,Loc}|Rest])
    end.

-doc false.
-spec module_md5(binary()) -> binary() | undefined.

module_md5(<<"FOR1", _/bits>>=Beam) ->
    module_md5_1(Beam);
module_md5(Beam) ->
    %% Corrupt header or compressed module, decompress it before passing it to
    %% the loader and let the BIF signal any errors.
    module_md5_1(try_decompress(Beam)).

module_md5_1(Beam) ->
    try
        erts_internal:beamfile_module_md5(Beam)
    catch
        error:Reason ->
            {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
                (catch erlang:error(new_stacktrace, [Beam])),
            erlang:raise(error, Reason, [{Mod,module_md5,L,Loc}|Rest])
    end.

try_decompress(Bin0) ->
    try zlib:gunzip(Bin0) of
        Decompressed -> Decompressed
    catch
        _:_ -> Bin0
    end.

%%% End of BIFs

%%----------------------------------------------------------------------------
%% User interface
%%----------------------------------------------------------------------------

-doc """
Returns the object code file extension corresponding to the Erlang machine used.

For the official Erlang/OTP release, the return value is always `.beam`.
""".
-spec objfile_extension() -> nonempty_string().
objfile_extension() ->
    init:objfile_extension().

-doc """
Tries to load the Erlang module `Module` using the code path.

It looks for the object code file with an extension corresponding to
the Erlang machine used, for example, `Module.beam`. The loading fails
if the module name found in the object code differs from the name
`Module`. Use `load_binary/3` to load object code with a module name
that is different from the file name.

Returns `{module, Module}` if successful, or `{error, Reason}` if loading fails.
See [Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of the possible error reasons.
""".
-spec load_file(Module) -> load_ret() when
      Module :: module().
load_file(Mod) when is_atom(Mod) ->
    case get_object_code(Mod) of
        error -> {error,nofile};
        {Mod,Binary,File} -> load_module(Mod, File, Binary, false)
    end.

-doc """
Tries to load a module in the same way as `load_file/1`, unless the module is
already loaded.

If called concurrently, this function ensures that only one process
attempts to load said module at a given time.

In embedded mode, it does not load a module that is not already loaded, but
returns `{error, embedded}` instead. See
[Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of other possible error reasons.
""".
-spec ensure_loaded(Module) -> {module, Module} | {error, What} when
      Module :: module(),
      What :: embedded | badfile | nofile | on_load_failure.
ensure_loaded(Mod) when is_atom(Mod) ->
    case erlang:module_loaded(Mod) of
        true -> {module, Mod};
        false ->
            case get_mode() of
                interactive ->
                    case call({get_object_code_for_loading, Mod}) of
                        {module, Mod} -> {module, Mod};
                        {error, What} -> {error, What};
                        {Binary,File,Ref} ->
                            case ensure_prepare_loading(Mod, Binary, File) of
                                {error,_}=Error ->
                                    call({load_error, Ref, Mod, Error});
                                Prepared ->
                                    call({load_module, Prepared, Mod, File, false, Ref})
                            end
                    end;
                embedded ->
                    {error, embedded}
            end
    end.

ensure_prepare_loading(Mod, missing, File) ->
    case erl_prim_loader:read_file(File) of
        {ok, Binary} -> erlang:prepare_loading(Mod, Binary);
        error -> {error, nofile}
    end;
ensure_prepare_loading(Mod, Binary, _File) ->
    erlang:prepare_loading(Mod, Binary).

%% XXX File as an atom is allowed only for backwards compatibility.
-doc """
Equivalent to [`load_file(Module)`](`load_file/1`), except that `Filename` is
an absolute or relative filename.

The code path is not searched. It returns a value in the same way as
`load_file/1`. Notice that `Filename` must not contain the extension
(for example, `.beam`) because [`load_abs/1`](`load_abs/1`) adds the
correct extension.
""".
-spec load_abs(Filename) -> load_ret() when
      Filename :: file:filename().
load_abs(File) when is_list(File); is_atom(File) ->
    load_abs(File, list_to_atom(filename:basename(File))).

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-doc false.
-spec load_abs(Filename :: loaded_filename(), Module :: module()) -> load_ret().
load_abs(File, M) when (is_list(File) orelse is_atom(File)), is_atom(M) ->
    case modp(File) of
        true ->
            FileName0 = lists:concat([File, objfile_extension()]),
            FileName = code_server:absname(FileName0),
            case erl_prim_loader:read_file(FileName) of
                {ok,Bin} ->
                    load_module(M, FileName, Bin, false);
                error ->
                    {error, nofile}
            end;
        false ->
            {error,badarg}
    end.

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-doc """
Loads object code from a binary.

This function can be used to load object code on remote Erlang nodes. Argument
`Binary` must contain object code for `Module`. `Filename` is only used by the
code server to keep a record of from which file the object code for `Module`
originates. Thus, `Filename` is not opened and read by the code server.

Returns `{module, Module}` if successful, or `{error, Reason}` if loading fails.
See [Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of the possible error reasons.
""".
-spec load_binary(Module, Filename, Binary) ->
                         {module, Module} | {error, What} when
      Module :: module(),
      Filename :: loaded_filename(),
      Binary :: binary(),
      What :: badarg | load_error_rsn().
load_binary(Mod, File, Bin)
  when is_atom(Mod), (is_list(File) orelse is_atom(File)), is_binary(Bin) ->
    case modp(File) of
        true -> load_module(Mod, File, Bin, true);
        false -> {error,badarg}
    end.

load_module(Mod, File, Bin, Purge) ->
    case erlang:prepare_loading(Mod, Bin) of
        {error,_}=Error ->
            Error;
        Prepared ->
            call({load_module, Prepared, Mod, File, Purge, false})
    end.

modp(Atom) when is_atom(Atom) -> true;
modp(List) when is_list(List) -> int_list(List);
modp(_)                       -> false.

int_list([H|T]) when is_integer(H) -> int_list(T);
int_list([_|_])                    -> false;
int_list([])                       -> true.

-doc """
Removes the current code for `Module`, that is, the current code for `Module` is
made old.

This means that processes can continue to execute the code in the
module, but no external function calls can be made to it.

Returns `true` if successful, or `false` if there is old code for `Module` that
must be purged first, or if `Module` is not a (loaded) module.
""".
-spec delete(Module) -> boolean() when
      Module :: module().
delete(Mod) when is_atom(Mod) -> call({delete,Mod}).

-doc """
Purges the code for `Module`, that is, removes code marked as old.

If some processes still linger in the old code, these processes are
killed before the code is removed.

> #### Change {: .info }
>
> As of Erlang/OTP 20.0, a process is only considered to be lingering in the
> code if it has direct references to the code. For more information see
> documentation of `erlang:check_process_code/3`, which is used in order to
> determine whether a process is lingering.

Returns `true` if successful and any process is needed to be killed, otherwise
`false`.
""".
-spec purge(Module) -> boolean() when
      Module :: module().
purge(Mod) when is_atom(Mod) -> call({purge,Mod}).

-doc """
Purges the code for `Module`, that is, removes code marked as old, but only if
no processes linger in it.

> #### Change {: .info }
>
> As of Erlang/OTP 20.0, a process is only considered to be lingering in the
> code if it has direct references to the code. For more information see
> documentation of `erlang:check_process_code/3`, which is used in order to
> determine whether a process is lingering.

Returns `false` if the module cannot be purged because of processes lingering in
old code, otherwise `true`.
""".
-spec soft_purge(Module) -> boolean() when
      Module :: module().
soft_purge(Mod) when is_atom(Mod) -> call({soft_purge,Mod}).

-doc """
Checks whether `Module` is loaded.

If it is, `{file, Loaded}` is returned, otherwise `false`.

Normally, `Loaded` is the absolute filename `Filename` from which the code is
obtained. If the module is preloaded (see [`script(4)`](`e:sasl:script.md`)),
`Loaded =:= preloaded`. If the module is Cover-compiled (see `m:cover`),
`Loaded =:= cover_compiled`.
""".
-spec is_loaded(Module) -> {'file', Loaded} | false when
      Module :: module(),
      Loaded :: loaded_filename().
is_loaded(Mod) when is_atom(Mod) ->
    code_server:is_loaded(Mod).

-doc """
Returns the object code for module `Module` if found in the code path.

Returns `{Module, Binary, Filename}` if successful, otherwise
`error`. `Binary` is a binary data object, which contains the object
code for the module. This is useful if code is to be loaded on a
remote node in a distributed system. For example, loading module
`Module` on a node `Node` is done as follows:

```erlang
...
{_Module, Binary, Filename} = code:get_object_code(Module),
erpc:call(Node, code, load_binary, [Module, Filename, Binary]),
...
```
""".
-spec get_object_code(Module) -> {Module, Binary, Filename} | error when
      Module :: module(),
      Binary :: binary(),
      Filename :: file:filename().
get_object_code(Mod) when is_atom(Mod) ->
    case call({get_object_code, Mod}) of
        {Module, missing, File} ->
            case erl_prim_loader:read_file(File) of
                {ok, Binary} -> {Module, Binary, File};
                error -> error
            end;
        {_, _, _} = MBF -> MBF;
        error -> error
    end.

-doc """
Returns a list of tuples `{Module, Loaded}` for all loaded modules.

`Loaded` is normally the absolute filename, as described for `is_loaded/1`.
""".
-spec all_loaded() -> [{Module, Loaded}] when
      Module :: module(),
      Loaded :: loaded_filename().
all_loaded() -> call(all_loaded).

-doc """
Returns a list of tuples `{Module, Filename, Loaded}` for all available modules.

A module is considered to be available if it either is loaded or would be loaded
if called. `Filename` is normally the absolute filename, as described for
`is_loaded/1`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec all_available() -> [{Module, Filename, Loaded}] when
      Module :: string(),
      Filename :: loaded_filename(),
      Loaded :: boolean().
all_available() ->
    case get_mode() of
        interactive ->
            all_available(get_path(), #{});
        embedded ->
            all_available([], #{})
    end.

all_available([Path|Tail], Acc) ->
    case erl_prim_loader:list_dir(Path) of
        {ok, Files} ->
            all_available(Tail, all_available(Path, Files, Acc));
        _Error ->
            all_available(Tail, Acc)
    end;
all_available([], AllModules) ->
    AllLoaded = [{atom_to_list(M),Path,true} || {M,Path} <- all_loaded()],
    AllAvailable =
        maps:fold(
          fun(File, Path, Acc) ->
                  [{filename:rootname(File), filename:append(Path, File), false} | Acc]
          end, [], AllModules),
    OrderFun = fun F({A,_,_},{B,_,_}) ->
                       F(A,B);
                   F(A,B) ->
                       A =< B
               end,
    lists:umerge(OrderFun, lists:sort(OrderFun, AllLoaded), lists:sort(OrderFun, AllAvailable)).

all_available(Path, [File | T], Acc) ->
    case filename:extension(File) of
        ".beam" ->
            case maps:is_key(File, Acc) of
                false ->
                    all_available(Path, T, Acc#{ File => Path });
                true ->
                    all_available(Path, T, Acc)
            end;
        _Else ->
                    all_available(Path, T, Acc)
    end;
all_available(_Path, [], Acc) ->
    Acc.

-doc false.
-spec stop() -> no_return().
stop() -> call(stop).

-doc """
Returns the root directory of Erlang/OTP, which is the directory where it is
installed.

_Example:_

```erlang
1> code:root_dir().
"/usr/local/otp"
```
""".
-spec root_dir() -> file:filename().
root_dir() -> call({dir,root_dir}).

-doc """
Returns the library directory, `$OTPROOT/lib`, where `$OTPROOT` is the root
directory of Erlang/OTP.

_Example:_

```erlang
1> code:lib_dir().
"/usr/local/otp/lib"
```
""".
-spec lib_dir() -> file:filename().
lib_dir() -> call({dir,lib_dir}).

%% XXX is_list() is for backwards compatibility -- take out in future version
-doc """
Returns the path for the *library directory*, the top directory, for an
application `Name` located under `$OTPROOT/lib` or in a directory referred to
with environment variable `ERL_LIBS`.

If a regular directory called `Name` or `Name-Vsn` exists in the code path with
an `ebin` subdirectory, the path to this directory is returned (not the `ebin`
directory).

If the directory refers to a directory in an archive, the archive name is
stripped away before the path is returned. For example, if directory
`/usr/local/otp/lib/mnesia-4.2.2.ez/mnesia-4.2.2/ebin` is in the path,
`/usr/local/otp/lib/mnesia-4.2.2/ebin` is returned. This means that the library
directory for an application is the same, regardless if the application resides
in an archive or not.

> #### Warning {: .info }
>
> Archives are experimental. In a future release, they can be removed or
> their behavior can change.

_Example:_

```erlang
> code:lib_dir(mnesia).
"/usr/local/otp/lib/mnesia-4.23"
```

Returns `{error, bad_name}` if `Name` is not the name of an application under
`$OTPROOT/lib` or on a directory referred to through environment variable
`ERL_LIBS`. Fails with an exception if `Name` has the wrong type.

> #### Warning {: .warning }
>
> For backward compatibility, `Name` is also allowed to be a string. That will
> probably change in a future release.
""".
-spec lib_dir(Name) -> file:filename() | {'error', 'bad_name'} when
      Name :: atom().
lib_dir(App) when is_atom(App) ; is_list(App) -> call({dir,{lib_dir,App}}).

-doc """
Returns the path to a subdirectory directly under the top directory of an
application.

> #### Change {: .info }
>
> This function is part of the archive support, which is an experimental
> feature that will be changed or removed in a future release.

Normally the subdirectories reside under the top directory for the
application, but when applications at least partly reside in an archive, the
situation is different. Some of the subdirectories can reside as regular
directories while others reside in an archive file. It is not checked whether
this directory exists.

Instead of using this function, use [`code:lib_dir/1`](`code:lib_dir/1`)
and `filename:join/2`.

_Example:_

```erlang
1> filename:join(code:lib_dir(megaco), "priv").
"/usr/local/otp/lib/megaco-3.9.1.1/priv"
```

Fails with an exception if `Name` or `SubDir` has the wrong type.
""".
-spec lib_dir(Name, SubDir) -> file:filename() | {'error', 'bad_name'} when
      Name :: atom(),
      SubDir :: atom().
lib_dir(App, SubDir) when is_atom(App), is_atom(SubDir) -> call({dir,{lib_dir,App,SubDir}}).

-doc """
Returns the compiler library directory.

Equivalent to [`code:lib_dir(compiler)`](`code:lib_dir/1`).
""".
-spec compiler_dir() -> file:filename().
compiler_dir() -> call({dir,compiler_dir}).

%% XXX is_list() is for backwards compatibility -- take out in future version
-doc """
Returns the path to the `priv` directory in an application.

> #### Warning {: .warning }
>
> For backward compatibility, `Name` is also allowed to be a string. That will
> probably change in a future release.
""".
-spec priv_dir(Name) -> file:filename() | {'error', 'bad_name'} when
      Name :: atom().
priv_dir(App) when is_atom(App); is_list(App) -> call({dir,{priv_dir,App}}).

-doc """
Marks `Dir` as sticky.

Returns `ok` if successful, otherwise `error`.
""".
-spec stick_dir(Dir) -> 'ok' | 'error' when
      Dir :: file:filename().
stick_dir(Dir) when is_list(Dir) -> call({stick_dir,Dir}).

-doc """
Unsticks a directory that is marked as sticky.

Returns `ok` if successful, otherwise `error`.
""".
-spec unstick_dir(Dir) -> 'ok' | 'error' when
      Dir :: file:filename().
unstick_dir(Dir) when is_list(Dir) -> call({unstick_dir,Dir}).

-doc false.
-spec stick_mod(Module :: module()) -> 'true'.
stick_mod(Mod) when is_atom(Mod) -> call({stick_mod,Mod}).

-doc false.
-spec unstick_mod(Module :: module()) -> 'true'.
unstick_mod(Mod) when is_atom(Mod) -> call({unstick_mod,Mod}).

-doc """
Returns `true` if `Module` is the name of a module that has been loaded from a
sticky directory (in other words: an attempt to reload the module will fail), or
`false` if `Module` is not a loaded module or is not sticky.
""".
-spec is_sticky(Module) -> boolean() when
      Module :: module().
is_sticky(Mod) when is_atom(Mod) ->
    code_server:is_sticky(Mod).

-type set_path_ret() :: 'true' | {'error', 'bad_directory'}.
-doc #{equiv => set_path(PathList, nocache)}.
-spec set_path(Path) -> set_path_ret() when
      Path :: [Dir :: file:filename()].
set_path(PathList) -> set_path(PathList, nocache).

-doc """
Sets the code path to the list of directories `Path`.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns:

- **`true`** - If successful

- **`{error, bad_directory}`** - If any `Dir` is not a directory name
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec set_path(Path, cache()) -> set_path_ret() when
      Path :: [Dir :: file:filename()].
set_path(PathList, Cache) when is_list(PathList), ?is_cache(Cache) ->
    case normalize_paths(PathList, [], ok) of
        {ok, Normalized} ->
            call({set_path,Normalized,Cache});
        {error, _} ->
            {error, bad_directory}
    end.

%% Atoms are supported only for backwards compatibility purposes.
%% They are not part of the typespec.
normalize_paths([P|Path], Acc, Status) when is_atom(P) ->
    normalize_paths(Path, [atom_to_list(P)|Acc], Status);
normalize_paths([P|Path], Acc, Status) when is_list(P) ->
    case int_list(P) of
        true  -> normalize_paths(Path, [filename:join([P]) | Acc], Status);
        false -> normalize_paths(Path, Acc, error)
    end;
normalize_paths([_|Path], Acc, _Status) ->
    normalize_paths(Path, Acc, error);
normalize_paths([], Acc, Status) ->
    {Status, lists:reverse(Acc)}.

-doc "Returns the code path.".
-spec get_path() -> Path when
      Path :: [Dir :: file:filename()].
get_path() -> call(get_path).

-type add_path_ret() :: 'true' | {'error', 'bad_directory'}.
-doc #{equiv => add_pathz(Dir, nocache)}.
-spec add_path(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_path(Dir) -> add_path(Dir, nocache).

-doc #{equiv => add_pathz(Dir, Cache)}.
-doc(#{since => <<"OTP 26.0">>}).
-spec add_path(Dir, cache()) -> add_path_ret() when
      Dir :: file:filename().
add_path(Dir, Cache) when is_list(Dir), ?is_cache(Cache) -> add_pathz(Dir, Cache).

-doc #{equiv => add_pathz(Dir, nocache)}.
-spec add_pathz(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_pathz(Dir) -> add_pathz(Dir, nocache).

-doc """
Adds `Dir` as the directory last in the code path.

If `Dir` already exists in the path, it is not added.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns `true` if successful, or `{error, bad_directory}` if `Dir` is
not the name of a directory.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec add_pathz(Dir, cache()) -> add_path_ret() when
      Dir :: file:filename().
add_pathz(Dir, Cache) when is_list(Dir), ?is_cache(Cache) ->
    {_, [Normalized]} = normalize_paths([Dir], [], ok),
    call({add_path,last,Normalized,Cache}).

-doc #{equiv => add_patha(Dir, nocache)}.
-spec add_patha(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_patha(Dir) -> add_patha(Dir, nocache).

-doc """
Adds `Dir` to the beginning of the code path.

If `Dir` exists, it is removed from the old position in the code path.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns `true` if successful, or `{error, bad_directory}` if `Dir` is
not the name of a directory.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec add_patha(Dir, cache()) -> add_path_ret() when
      Dir :: file:filename().
add_patha(Dir, Cache) when is_list(Dir), ?is_cache(Cache) ->
    {_, [Normalized]} = normalize_paths([Dir], [], ok),
    call({add_path,first,Normalized,Cache}).

-doc #{equiv => add_pathsz(Dirs, nocache)}.
-spec add_paths(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_paths(Dirs) -> add_paths(Dirs, nocache).

-doc #{equiv => add_pathsz(Dirs, Cache)}.
-doc(#{since => <<"OTP 26.0">>}).
-spec add_paths(Dirs, cache()) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_paths(Dirs, Cache) when is_list(Dirs), ?is_cache(Cache) ->
    add_pathsz(Dirs, Cache).

-doc #{equiv => add_pathsz(Dirs, nocache)}.
-spec add_pathsz(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsz(Dirs) -> add_pathsz(Dirs, nocache).

-doc """
Adds the directories in `Dirs` to the end of the code path.

Directories that are already present in the path will not be added.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Always returns `ok`, regardless of the validity of each individual `Dir`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec add_pathsz(Dirs, cache()) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsz(Dirs, Cache) when is_list(Dirs), ?is_cache(Cache) ->
    {_, Normalized} = normalize_paths(Dirs, [], ok),
    call({add_paths,last,Normalized,Cache}).

-doc #{equiv => add_pathsa(Dirs, nocache)}.
-spec add_pathsa(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsa(Dirs) -> add_pathsa(Dirs, nocache).

-doc """
Traverses `Dirs` and adds each `Dir` to the beginning of the code path.

This means that the order of `Dirs` is reversed in the resulting code
path. For example, if `Dirs` is `[Dir1,Dir2]`, the resulting path will
be `[Dir2,Dir1|OldCodePath]`.

If a `Dir` already exists in the code path, it is removed from the old position.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Always returns `ok`, regardless of the validity of each individual `Dir`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec add_pathsa(Dirs, cache()) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsa(Dirs, Cache) when is_list(Dirs), ?is_cache(Cache) ->
    {_, Normalized} = normalize_paths(Dirs, [], ok),
    call({add_paths,first,Normalized,Cache}).

-doc """
Deletes a directory from the code path.

The argument can be an atom `Name`, in which case the directory with
the name `.../Name[-Vsn][/ebin]` is deleted from the code path. Also,
the complete directory name `Dir` can be specified as argument.

Returns:

- **`true`** - If successful

- **`false`** - If the directory is not found

- **`{error, bad_name}`** - If the argument is invalid
""".
-spec del_path(NameOrDir) -> boolean() | {'error', What} when
      NameOrDir :: Name | Dir,
      Name :: atom(),
      Dir :: file:filename(),
      What :: 'bad_name'.
del_path(Name) when is_list(Name); is_atom(Name) -> call({del_path,Name}).

-doc """
Deletes directories from the code path.

The argument is a list of either atoms or complete directory names. If
`Name` is an atom, the directory with the name `.../Name[-Vsn][/ebin]` is
deleted from the code path.

Always returns `ok`, regardless of the validity of each individual
`NamesOrDirs`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec del_paths(NamesOrDirs) -> 'ok' when
      NamesOrDirs :: [Name | Dir],
      Name :: atom(),
      Dir :: file:filename().
del_paths(Dirs) when is_list(Dirs) -> call({del_paths,Dirs}).

-type replace_path_ret() :: 'true' |
                            {'error', 'bad_directory' | 'bad_name' | {'badarg',_}}.
-doc #{equiv => replace_path(Name, Dir, nocache)}.
-spec replace_path(Name, Dir) -> replace_path_ret() when
      Name:: atom(),
      Dir :: file:filename().
replace_path(Name, Dir) ->
    replace_path(Name, Dir, nocache).

-doc """
Replaces an old occurrence of a directory named `.../Name[-Vsn][/ebin]` in the
code path, with `Dir`.

If `Name` does not exist, it adds the new directory `Dir` last in the
code path. The new directory must also be named
`.../Name[-Vsn][/ebin]`. This function is to be used if a new version
of the directory (library) is added to a running system.

Argument `Cache` controls whether the content of the directory
should be cached on first traversal. If `Cache` is `cache` the directory
contents will be cached; if `Cache` is `nocache` it will not be cached.

Returns:

- **`true`** - If successful

- **`{error, bad_name}`** - If `Name` is not found

- **`{error, bad_directory}`** - If `Dir` does not exist

- **`{error, {badarg, [Name, Dir]}}`** - If `Name` or `Dir` is invalid
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec replace_path(Name, Dir, cache()) -> replace_path_ret() when
      Name:: atom(),
      Dir :: file:filename().
replace_path(Name, Dir, Cache) when (is_atom(Name) orelse is_list(Name)),
                 (is_atom(Dir) orelse is_list(Dir)), ?is_cache(Cache) ->
    call({replace_path,Name,Dir,Cache}).

-doc """
Returns an atom describing the mode of the code server: `interactive` or
`embedded`.

This information is useful when an external entity (for example, an IDE)
provides additional code for a running node. If the code server is in
interactive mode, it only has to add the path to the code. If the code server is
in embedded mode, the code must be loaded with `load_binary/3`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec get_mode() -> 'embedded' | 'interactive'.
get_mode() -> code_server:get_mode().

-doc """
Clears the code path cache.

If a directory is cached, its cache is cleared once and then it will
be recalculated and cached once more in a future traversal.

To clear the cache for a single path, either re-add it to the code
path (with [`add_path/2`](`add_path/2`)) or replace it (with
[`replace_path/3`](`replace_path/3`)). To disable all caching, reset
the code path with `code:set_path(code:get_path())`.

Always returns `ok`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec clear_cache() -> ok.
clear_cache() -> call(clear_cache).

%%%
%%% Loading of several modules in parallel.
%%%

-doc """
Tries to load any modules not already loaded in the list `Modules` in the same
way as `load_file/1`.

Unlike `ensure_loaded/1`, modules are loaded even in `embedded` mode.

Returns `ok` if successful, or `{error,[{Module,Reason}]}` if loading of some
modules fails. See
[Error Reasons for Code-Loading Functions](`m:code#error_reasons`) for a
description of other possible error reasons.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec ensure_modules_loaded([Module]) ->
   'ok' | {'error',[{Module,What}]} when
      Module :: module(),
      What :: badfile | nofile | on_load_failure.

ensure_modules_loaded(Modules) when is_list(Modules) ->
    case prepare_ensure(Modules, []) of
	Ms when is_list(Ms) ->
	    ensure_modules_loaded_1(Ms);
	error ->
	    error(function_clause, [Modules])
    end.

ensure_modules_loaded_1(Ms0) ->
    Ms = lists:usort(Ms0),
    {Prep,Error0} = load_mods(Ms),
    {OnLoad,Normal} = partition_on_load(Prep),
    Error1 = case finish_loading(Normal, true) of
		 ok -> Error0;
		 {error,Err} -> Err ++ Error0
	     end,
    ensure_modules_loaded_2(OnLoad, Error1).

ensure_modules_loaded_2([{M,{Prepared,File}}|Ms], Errors) ->
    case call({load_module, Prepared, M, File, false, true}) of
	{module,M} ->
	    ensure_modules_loaded_2(Ms, Errors);
	{error,Err} ->
	    ensure_modules_loaded_2(Ms, [{M,Err}|Errors])
    end;
ensure_modules_loaded_2([], []) ->
    ok;
ensure_modules_loaded_2([], [_|_]=Errors) ->
    {error,Errors}.

prepare_ensure([M|Ms], Acc) when is_atom(M) ->
    case erlang:module_loaded(M) of
	true ->
	    prepare_ensure(Ms, Acc);
	false ->
	    prepare_ensure(Ms, [M|Acc])
    end;
prepare_ensure([], Acc) ->
    Acc;
prepare_ensure(_, _) ->
    error.

-doc """
Tries to load all of the modules in the list `Modules` atomically.

That means that either all modules are loaded at the same time, or
none of the modules are loaded if there is a problem with any of the
modules.

Loading can fail for one the following reasons:

- **`badfile`** - The object code has an incorrect format or the module name in
  the object code is not the expected module name.

- **`nofile`** - No file with object code exists.

- **`on_load_not_allowed`** - A module contains an
  [\-on_load function](`e:system:code_loading.md#on_load`).

- **`duplicated`** - A module is included more than once in `Modules`.

- **`not_purged`** - The object code cannot be loaded because an old version of
  the code already exists.

- **`sticky_directory`** - The object code resides in a sticky directory.

- **`pending_on_load`** - A previously loaded module contains an `-on_load`
  function that never finished.

If it is important to minimize the time that an application is inactive while
changing code, use `prepare_loading/1` and `finish_loading/1` instead of
[`atomic_load/1`](`atomic_load/1`). Here is an example:

```erlang
{ok,Prepared} = code:prepare_loading(Modules),
%% Put the application into an inactive state or do any
%% other preparation needed before changing the code.
ok = code:finish_loading(Prepared),
%% Resume the application.
```
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec atomic_load(Modules) -> 'ok' | {'error',[{Module,What}]} when
      Modules :: [Module | {Module, Filename, Binary}],
      Module :: module(),
      Filename :: file:filename(),
      Binary :: binary(),
      What :: 'badfile' | 'nofile' | 'on_load_not_allowed' | 'duplicated' |
	      'not_purged' | 'sticky_directory' | 'pending_on_load'.

atomic_load(Modules) ->
    case do_prepare_loading(Modules) of
	{ok,Prep} ->
	    finish_loading(Prep, false);
	{error,_}=Error ->
	    Error;
	badarg ->
	    error(function_clause, [Modules])
    end.

-doc """
Prepares to load the modules in the list `Modules`.

Finish the loading by calling
[finish_loading(Prepared)](`finish_loading/1`).

This function can fail with one of the following error reasons:

- **`badfile`** - The object code has an incorrect format or the module name in
  the object code is not the expected module name.

- **`nofile`** - No file with object code exists.

- **`on_load_not_allowed`** - A module contains an
  [\-on_load function](`e:system:code_loading.md#on_load`).

- **`duplicated`** - A module is included more than once in `Modules`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec prepare_loading(Modules) ->
           {'ok',Prepared} | {'error',[{Module,What}]} when
      Modules :: [Module | {Module, Filename, Binary}],
      Module :: module(),
      Filename :: file:filename(),
      Binary :: binary(),
      Prepared :: prepared_code(),
      What :: 'badfile' | 'nofile' | 'on_load_not_allowed' | 'duplicated'.

prepare_loading(Modules) ->
    case do_prepare_loading(Modules) of
	{ok,Prep} ->
	    {ok,{?PREPARED,Prep}};
	{error,_}=Error ->
	    Error;
	badarg ->
	    error(function_clause, [Modules])
    end.

-doc """
Tries to load code for all modules that have been previously prepared by
`prepare_loading/1`.

The loading occurs atomically, meaning that either all modules are
loaded at the same time, or none of the modules are loaded.

This function can fail with one of the following error reasons:

- **`not_purged`** - The object code cannot be loaded because an old version of
  the code already exists.

- **`sticky_directory`** - The object code resides in a sticky directory.

- **`pending_on_load`** - A previously loaded module contains an `-on_load`
  function that never finished.
""".
-doc(#{since => <<"OTP 19.0">>}).
-spec finish_loading(Prepared) -> 'ok' | {'error',[{Module,What}]} when
      Prepared :: prepared_code(),
      Module :: module(),
      What :: 'not_purged' | 'sticky_directory' | 'pending_on_load'.

finish_loading({?PREPARED,Prepared}=Arg) when is_list(Prepared) ->
    case verify_prepared(Prepared) of
	ok ->
	    finish_loading(Prepared, false);
	error ->
	    error(function_clause, [Arg])
    end.

partition_load([Item|T], Bs, Ms) ->
    case Item of
	{M,File,Bin} when is_atom(M) andalso
			  is_list(File) andalso
			  is_binary(Bin) ->
	    partition_load(T, [Item|Bs], Ms);
	M when is_atom(M) ->
	    partition_load(T, Bs, [Item|Ms]);
	_ ->
	    error
    end;
partition_load([], Bs, Ms) ->
    {Bs,Ms}.

do_prepare_loading(Modules) ->
    case partition_load(Modules, [], []) of
	{ModBins,Ms} ->
	    case prepare_loading_1(ModBins, Ms) of
		{error,_}=Error ->
		    Error;
		Prep when is_list(Prep) ->
		    {ok,Prep}
	    end;
	error ->
	    badarg
    end.

prepare_loading_1(ModBins, Ms) ->
    %% erlang:finish_loading/1 *will* detect duplicates.
    %% However, we want to detect all errors that can be detected
    %% by only examining the input data before call the LastAction
    %% fun.
    case prepare_check_uniq(ModBins, Ms) of
	ok ->
	    prepare_loading_2(ModBins, Ms);
	Error ->
	    Error
    end.

prepare_loading_2(ModBins, Ms) ->
    {Prep0,Error0} = load_bins(ModBins),
    {Prep1,Error1} = load_mods(Ms),
    case Error0 ++ Error1 of
	[] ->
	    prepare_loading_3(Prep0 ++ Prep1);
	[_|_]=Error ->
	    {error,Error}
    end.

prepare_loading_3(Prep) ->
    case partition_on_load(Prep) of
	{[_|_]=OnLoad,_} ->
	    Error = [{M,on_load_not_allowed} || {M,_} <- OnLoad],
	    {error,Error};
	{[],_} ->
	    Prep
    end.

prepare_check_uniq([{M,_,_}|T], Ms) ->
    prepare_check_uniq(T, [M|Ms]);
prepare_check_uniq([], Ms) ->
    prepare_check_uniq_1(lists:sort(Ms), []).

prepare_check_uniq_1([M|[M|_]=Ms], Acc) ->
    prepare_check_uniq_1(Ms, [{M,duplicated}|Acc]);
prepare_check_uniq_1([_|Ms], Acc) ->
    prepare_check_uniq_1(Ms, Acc);
prepare_check_uniq_1([], []) ->
    ok;
prepare_check_uniq_1([], [_|_]=Errors) ->
    {error,Errors}.

partition_on_load(Prep) ->
    P = fun({_,{PC,_}}) ->
		erlang:has_prepared_code_on_load(PC)
	end,
    lists:partition(P, Prep).

verify_prepared([{M,{Prep,Name}}|T])
  when is_atom(M), is_list(Name) ->
    try erlang:has_prepared_code_on_load(Prep) of
	false ->
	    verify_prepared(T);
	_ ->
	    error
    catch
	error:_ ->
	    error
    end;
verify_prepared([]) ->
    ok;
verify_prepared(_) ->
    error.

finish_loading(Prepared, EnsureLoaded) ->
    call({finish_loading,Prepared,EnsureLoaded}).

load_mods([]) ->
    {[],[]};
load_mods(Mods) ->
    F = fun(Mod) ->
        case get_object_code(Mod) of
            {Mod, Beam, File} -> prepare_loading(Mod, File, Beam);
            error -> {error, nofile}
        end
    end,
    do_par(F, Mods).

load_bins([]) ->
    {[],[]};
load_bins(BinItems) ->
    F = fun({Mod, File, Beam}) -> prepare_loading(Mod, File, Beam) end,
    do_par(F, BinItems).

-spec prepare_loading(module(), file:filename(), binary()) ->
                     {ok,_} | {error,_}.

prepare_loading(Mod, FullName, Beam) ->
    case erlang:prepare_loading(Mod, Beam) of
	{error,_}=Error ->
	    Error;
	Prepared ->
	    {ok,{Prepared,FullName}}
    end.

do_par(Fun, L) ->
    {_,Ref} = spawn_monitor(do_par_fun(Fun, L)),
    receive
	{'DOWN',Ref,process,_,Res} ->
	    Res
    end.

-type par_fun_type() :: fun((module() | {module(), file:filename(), binary()}) ->
                            {ok,_} | {error,_}).

-spec do_par_fun(par_fun_type(), list()) -> fun(() -> no_return()).

do_par_fun(Fun, L) ->
    fun() ->
	_ = [spawn_monitor(do_par_fun_each(Fun, Item)) || Item <- L],
	exit(do_par_recv(length(L), [], []))
    end.

-spec do_par_fun_each(par_fun_type(), term()) ->
			  fun(() -> no_return()).

do_par_fun_each(Fun, Mod) when is_atom(Mod) ->
    do_par_fun_each(Fun, Mod, Mod);
do_par_fun_each(Fun, {Mod, _, _} = Item) ->
    do_par_fun_each(Fun, Mod, Item).

do_par_fun_each(Fun, Mod, Item) ->
    fun() ->
        try Fun(Item) of
            {ok,Res} ->
                exit({good,{Mod,Res}});
            {error,Error} ->
                exit({bad,{Mod,Error}})
        catch
            _:Error ->
                exit({bad,{Mod,Error}})
        end
    end.

do_par_recv(0, Good, Bad) ->
    {Good,Bad};
do_par_recv(N, Good, Bad) ->
    receive
	{'DOWN',_,process,_,{good,Res}} ->
	    do_par_recv(N-1, [Res|Good], Bad);
	{'DOWN',_,process,_,{bad,Res}} ->
	    do_par_recv(N-1, Good, [Res|Bad])
    end.

%%-----------------------------------------------------------------

call(Req) ->
    code_server:call(Req).

-doc false.
-spec start_link() -> {'ok', pid()}.
start_link() ->
    do_start().
    
%%-----------------------------------------------------------------
%% In the init phase, code must not use any modules not yet loaded,
%% either pre_loaded (e.g. init) or first in the script (e.g.
%% erlang).  Therefore, keep the modules used in init phase to a
%% minimum, and make sure they are loaded before init is called.
%% Try to call these modules from do_start instead.
%% file is used in init - this is ok; file has been started before
%% us, so the module is loaded.
%%-----------------------------------------------------------------

do_start() ->
    maybe_warn_for_cache(),
    load_code_server_prerequisites(),

    {ok,[[Root0]]} = init:get_argument(root),
    Mode = start_get_mode(),
    Root = filename:join([Root0]),	    % Normalize.
    Res = code_server:start_link([Root,Mode]),

    maybe_stick_dirs(Mode),

    Res.

%% Make sure that all modules that the code_server process calls
%% (directly or indirectly) are loaded. Otherwise the code_server
%% process will deadlock.

load_code_server_prerequisites() ->
    %% Please keep the alphabetical order.
    Needed = [beam_lib,
              binary,
	      ets,
	      filename,
	      gb_sets,
	      gb_trees,
	      lists,
	      os,
	      unicode],
    _ = [M = M:module_info(module) || M <- Needed],
    _ = erl_features:enabled(),
    ok.

maybe_stick_dirs(interactive) ->
    case init:get_argument(nostick) of
	{ok,[[]]} ->
	    ok;
	_ ->
	    do_stick_dirs()
    end;
maybe_stick_dirs(_) ->
    ok.

do_stick_dirs() ->
    do_s(compiler),
    do_s(stdlib),
    do_s(kernel).

do_s(Lib) ->
    case lib_dir(Lib) of
	{error, _} ->
	    ok;
	Dir ->
	    %% The return value is intentionally ignored. Missing
	    %% directories is not a fatal error. (In embedded systems,
	    %% there is usually no compiler directory.)
	    _ = stick_dir(filename:append(Dir, "ebin")),
	    ok
    end.

start_get_mode() ->
    case init:get_argument(mode) of
	{ok, [FirstMode | Rest]} ->
	    case Rest of
		[] ->
		    ok;
		_ ->
		    ?LOG_WARNING("Multiple -mode given to erl, using the first, ~p",
				 [FirstMode])
	    end,
	    case FirstMode of
		["embedded"] ->
		    embedded;
		_ ->
		    interactive
	    end;
	_ ->
	    interactive
    end.

%% Find out which version of a particular module we would
%% load if we tried to load it, unless it's already loaded.
%% In that case return the name of the file which contains
%% the loaded object code

-doc """
If the module is not loaded, this function searches the code path for the first
file containing object code for `Module` and returns the absolute filename.

- If the module is loaded, it returns the name of the file containing the loaded
  object code.

- If the module is preloaded, `preloaded` is returned.

- If the module is Cover-compiled, `cover_compiled` is returned.

- If the module cannot be found, `non_existing` is returned.
""".
-spec which(Module) -> Which when
      Module :: module(),
      Which :: loaded_filename() | non_existing.
which(Module) when is_atom(Module) ->
    case is_loaded(Module) of
	false ->
            which(Module, get_path());
	{file, File} ->
	    File
    end.

which(Module, Path) when is_atom(Module) ->
    File = atom_to_list(Module) ++ objfile_extension(),
    where_is_file(Path, File).

%% Search the code path for a specific file.

-doc """
Searches the code path for `Filename`, which is a file of arbitrary type.

If found, the full name is returned. `non_existing` is returned if the
file cannot be found.  The function can be useful, for example, to
locate application resource files.
""".
-spec where_is_file(Filename) -> non_existing | Absname when
      Filename :: file:filename(),
      Absname :: file:filename().
where_is_file(File) when is_list(File) ->
    Path = get_path(),
    where_is_file(Path, File).

%% To avoid unnecessary work when looking at many modules, this also
%% accepts pairs of directories and pre-fetched contents in the path
-doc false.
-spec where_is_file(Path :: [Dir|{Dir,Files}], Filename :: file:filename()) ->
          'non_existing' | file:filename() when
      Dir :: file:filename(), Files :: [file:filename()].

where_is_file([], _) ->
    non_existing;
where_is_file([{Path, Files}|Tail], File) ->
    where_is_file(Tail, File, Path, Files);
where_is_file([Path|Tail], File) ->
    Full = filename:append(Path, File),
    case erl_prim_loader:read_file_info(Full) of
	{ok,_} ->
            Full;
	_Error ->
	    where_is_file(Tail, File)
    end.

where_is_file(Tail, File, Path, Files) ->
    case lists:member(File, Files) of
        true ->
            filename:append(Path, File);
        false ->
            where_is_file(Tail, File)
    end.

-doc """
get_doc(Module)

Returns [EEP 48](https://www.erlang.org/eeps/eep-0048.html) style
documentation for `Module` if available.

If `Module` is not found in the code path, this function returns
`{error,non_existing}`.

If no documentation can be found this function attempts to generate
documentation from the debug information in the module. If no debug
information is available, this function returns `{error,missing}`.

For more information about the documentation chunk see
[Documentation Storage and Format](eep48_chapter.md) in
Kernel's User's Guide.
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec get_doc(Mod) -> {ok, Res} | {error, Reason} when
      Mod :: module(),
      Res :: #docs_v1{},
      Reason :: non_existing | missing | file:posix().
get_doc(Mod) when is_atom(Mod) ->
    get_doc(Mod, #{sources => [eep48, debug_info]}).

-doc false.
get_doc(Mod, #{sources:=[Source|Sources]}=Options) ->
    GetDoc = fun(Fn) -> R = case Source of
            debug_info -> get_doc_chunk_from_ast(Fn);
            eep48 -> get_doc_chunk(Fn, Mod)
        end,
        case R of
            {error, missing} -> get_doc(Mod, Options#{sources=>Sources});
            _ -> R
        end
    end,
    case which(Mod) of
        preloaded ->
            case code:lib_dir(erts) of
                {error, _} -> {error, missing};
                ErtsDir ->
                    GetDoc(filename:join([ErtsDir, "ebin", atom_to_list(Mod) ++ ".beam"]))
            end;
        Error when is_atom(Error) ->
            {error, Error};
        Fn ->
            GetDoc(Fn)
    end;
get_doc(_, #{sources:=[]}) ->
    {error, missing}.

get_doc_chunk(Filename, Mod) when is_atom(Mod) ->
    case beam_lib:chunks(Filename, ["Docs"]) of
        {error,beam_lib,{missing_chunk,_,_}} ->
            get_doc_chunk(Filename, atom_to_list(Mod));
        {error,beam_lib,{file_error,_Filename,_Err}} ->
            get_doc_chunk(Filename, atom_to_list(Mod));
        {ok, {Mod, [{"Docs",Bin}]}} ->
            {ok,binary_to_term(Bin)}
    end;
get_doc_chunk(Filename, Mod) ->
    RootDir = code:root_dir(),
    case filename:dirname(Filename) of
        Filename ->
            {error,missing};
        RootDir ->
            {error,missing};
        Dir ->
            ChunkFile = filename:join([Dir,"doc","chunks",Mod ++ ".chunk"]),
            case file:read_file(ChunkFile) of
                {ok, Bin} ->
                    {ok, binary_to_term(Bin)};
                {error,enoent} ->
                    get_doc_chunk(Dir, Mod);
                {error,Reason} ->
                    {error,Reason}
            end
    end.

get_doc_chunk_from_ast(Filename) ->
    case beam_lib:chunks(Filename, [abstract_code]) of
        {error,beam_lib,{missing_chunk,_,_}} ->
            {error,missing};
        {error,beam_lib,{file_error,_,_}} ->
            {error, missing};
        {ok, {_Mod, [{abstract_code,
                      {raw_abstract_v1, AST}}]}} ->
            Docs = get_function_docs_from_ast(AST),
            Types = get_type_docs_from_ast(AST),
            {ok, #docs_v1{ anno = 0, beam_language = erlang,
                           module_doc = none,
                           metadata = #{ generated => true, otp_doc_vsn => ?CURR_DOC_VERSION},
                           docs = Docs++Types }};
        {ok, {_Mod, [{abstract_code,no_abstract_code}]}} ->
            {error,missing};
        Error ->
            Error
    end.

get_type_docs_from_ast(AST) ->
    lists:flatmap(fun(E) -> get_type_docs_from_ast(E, AST) end, AST).
get_type_docs_from_ast({attribute, Anno, type, {TypeName, _, Ps}}=Meta, _) ->
    Arity = length(Ps),
    Signature = io_lib:format("~p/~p",[TypeName,Arity]),
    [{{type, TypeName, Arity},Anno,[unicode:characters_to_binary(Signature)],none,#{signature => [Meta]}}];
get_type_docs_from_ast(_, _) ->
    [].

get_function_docs_from_ast(AST) ->
    lists:flatmap(fun(E) -> get_function_docs_from_ast(E, AST) end, AST).
get_function_docs_from_ast({function,Anno,Name,Arity,_Code}, AST) ->
    Signature = io_lib:format("~p/~p",[Name,Arity]),
    Specs = lists:filter(
               fun({attribute,_Ln,spec,{FA,_}}) ->
                       case FA of
                           {F,A} ->
                               F =:= Name andalso A =:= Arity;
                           {_, F, A} ->
                               F =:= Name andalso A =:= Arity
                       end;
                  (_) -> false
               end, AST),
    SpecMd = case Specs of
                 [S] -> #{ signature => [S] };
                 [] -> #{}
             end,
    [{{function, Name, Arity}, Anno,
      [unicode:characters_to_binary(Signature)], none, SpecMd}];
get_function_docs_from_ast(_, _) ->
    [].

%% Search the entire path system looking for name clashes

-doc """
Searches all directories in the code path for module names with identical names
and writes a report to `stdout`.
""".
-spec clash() -> 'ok'.

clash() ->
    Path = get_path(),
    Struct = lists:flatten(build(Path)),
    Len = length(search(Struct)),
    io:format("** Found ~w name clashes in code paths ~n", [Len]).

%% Internal for clash/0

search([]) -> [];
search([{Dir, File} | Tail]) ->
    case lists:keyfind(File, 2, Tail) of
	false -> 
	    search(Tail);
	{Dir2, File} ->
	    io:format("** ~ts hides ~ts~n",
		      [filename:join(Dir, File),
		       filename:join(Dir2, File)]),
	    [clash | search(Tail)]
    end.

build([]) -> [];
build([Dir|Tail]) ->
    Files = filter(objfile_extension(), Dir,
		   erl_prim_loader:list_dir(Dir)),
    [decorate(Files, Dir) | build(Tail)].

decorate([], _) -> [];
decorate([File|Tail], Dir) ->
    [{Dir, File} | decorate(Tail, Dir)].

filter(_Ext, Dir, error) ->
    io:format("** Bad path can't read ~ts~n", [Dir]), [];
filter(Ext, _, {ok,Files}) -> 
    filter2(Ext, length(Ext), Files).

filter2(_Ext, _Extlen, []) -> [];
filter2(Ext, Extlen, [File|Tail]) ->
    case has_ext(Ext, Extlen, File) of
	true -> [File | filter2(Ext, Extlen, Tail)];
	false -> filter2(Ext, Extlen, Tail)
    end.

has_ext(Ext, Extlen, File) ->
    L = length(File),
    case catch lists:nthtail(L - Extlen, File) of
	Ext -> true;
	_ -> false
    end.

%%%
%%% Warning for deprecated code path cache.
%%%

maybe_warn_for_cache() ->
    case init:get_argument(code_path_cache) of
	{ok, _} ->
	    cache_warning();
	error ->
	    ok
    end.

cache_warning() ->
    W = "The code path cache functionality has been removed",
    error_logger:warning_report(W).

-type module_status() :: not_loaded | loaded | modified | removed.

%% Returns the list of all loaded modules and their current status
-doc "See `module_status/1` and `all_loaded/0` for details.".
-doc(#{since => <<"OTP 23.0">>}).
-spec module_status() -> [{module(), module_status()}].
module_status() ->
    module_status([M || {M, _} <- all_loaded()]).

-doc """
Returns the status of `Module` in relation to object file on disk.

The status of a module can be one of:

- **`not_loaded`** - If `Module` is not currently loaded.

- **`loaded`** - If `Module` is loaded, and the object file exists and contains
  the same code.

- **`removed`** - If `Module` is loaded, but no corresponding object file can be
  found in the code path.

- **`modified`** - If `Module` is loaded, but the object file contains code with
  a different MD5 checksum.

Preloaded modules are always reported as `loaded`, without inspecting the
contents on disk. Cover-compiled modules will always be reported as `modified`
if an object file exists, or as `removed` otherwise. Modules whose load path is
an empty string (which is the convention for auto-generated code) will only be
reported as `loaded` or `not_loaded`.

See also `modified_modules/0`.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec module_status (Module :: module() | [module()]) ->
          module_status() | [{module(), module_status()}].
module_status(Modules) when is_list(Modules) ->
    PathFiles = path_files(),
    [{M, module_status(M, PathFiles)} || M <- Modules];
module_status(Module) ->
    module_status(Module, code:get_path()).

%% Note that we don't want to go via which/1, since it doesn't look at the
%% disk contents at all if the module is already loaded.
module_status(Module, PathFiles) ->
    case is_loaded(Module) of
        false -> not_loaded;
        {file, preloaded} -> loaded;
        {file, cover_compiled} ->
            %% Cover compilation loads directly to memory and does not
            %% create a beam file, so report 'modified' if a file exists.
            case which(Module, PathFiles) of
                non_existing -> removed;
                _File -> modified
            end;
        {file, []} -> loaded;  % no beam file - generated code
        {file, [_|_]} ->
            %% We don't care whether or not the file is in the same location
            %% as when last loaded, as long as it can be found in the path.
            case which(Module, PathFiles) of
                non_existing -> removed;
                Path ->
                    case module_changed_on_disk(Module, Path) of
                        true -> modified;
                        false -> loaded
                    end
            end
    end.

%% Detects actual code changes only, e.g. to decide whether a module should
%% be reloaded; does not care about file timestamps or compilation time
module_changed_on_disk(Module, Path) ->
    MD5 = erlang:get_module_info(Module, md5),
    MD5 =/= beam_file_md5(Module, Path).

beam_file_md5(Module, Path) ->
    case do_beam_file_md5(Path) of
        MD5 when is_binary(MD5) ->
            MD5;
        undefined ->
            %% This module is probably embedded in an archive.
            case get_object_code(Module) of
                {Module, Code, _Path} ->
                    do_beam_file_md5(Code);
                error ->
                    undefined
            end
    end.

do_beam_file_md5(PathOrCode) ->
    case beam_lib:md5(PathOrCode) of
        {ok,{_Mod,MD5}} -> MD5;
        _ -> undefined
    end.

%% Returns a list of all modules modified on disk.
-doc """
Returns the list of all currently loaded modules for which `module_status/1`
returns `modified`.

See also `all_loaded/0`.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec modified_modules() -> [module()].
modified_modules() ->
    [M || {M, modified} <- module_status()].

%% prefetch the directory contents of code path directories
path_files() ->
    path_files(code:get_path()).

path_files([]) ->
    [];
path_files([Path|Tail]) ->
    case erl_prim_loader:list_dir(Path) of
        {ok, Files} ->
            [{Path,Files} | path_files(Tail)];
        _Error ->
            path_files(Tail)
    end.

-doc """
Return either `function` or `line` coverage data for module `Module`.

If Level is `function`, returns function coverage for the given module
according to its [coverage mode](`t:coverage_mode/0`):

- **`function`** - For each function in module Module, a boolean indicating
  whether that function has been executed at least once is returned.

- **`function_counters`** - For each function in module Module, an integer
  giving the number of times that line has been executed is returned.

- **`line`** - For each function in module Module, a boolean indicating whether
  that function has been executed at least once is returned.

- **`line_counters`** - For each function in module Module, a boolean indicating
  whether that function has been executed at least once is returned (note that
  in this mode, counters for the number of times each function has been executed
  **cannot** be retrieved).

If Level is `line`, returns line coverage for the given module according to its
coverage mode:

- **`line`** - For each executable line in the module, a boolean indicating
  whether that line has been executed at least once is returned.

- **`line_counters`** - For each executable line in the module, an integer
  giving the number of times that line was executed is returned.

Level `cover_id_line` is used by the `m:cover` tool.

Failures:

- **`badarg`** - If `Level` is not `function` or `line`.

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `Module` does not refer to a loaded module.

- **`badarg`** - If `Module` was not loaded in another coverage mode than
  `none`.

- **`badarg`** - If Level is `line` and `Module` has not been loaded with either
  `line` or `line_counters` enabled.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec get_coverage(Level, module()) -> Result when
      Level :: 'function' | 'line' | 'cover_id_line',
      Result :: [{Entity, CoverageInfo}],
      Entity :: {Function, Arity} | Line | CoverId,
      CoverageInfo :: Covered | Counter,
      Function :: atom(),
      Arity :: arity(),
      Line :: non_neg_integer(),
      CoverId :: pos_integer(),
      Covered :: boolean(),
      Counter :: non_neg_integer().
get_coverage(_Level, _Module) ->
    erlang:nif_error(undefined).

-doc """
Returns the coverage mode as set by option
[\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`) for `erl` or `set_coverage_mode/1`.

Failure:

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec get_coverage_mode() -> Mode when
      Mode :: coverage_mode().
get_coverage_mode() ->
    erlang:nif_error(undefined).

-doc """
Get coverage mode for the given module.

Failures:

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `Module` does not refer to a loaded module.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec get_coverage_mode(Module) -> Mode when
      Module :: module(),
      Mode :: coverage_mode().
get_coverage_mode(_Module) ->
    erlang:nif_error(undefined).

-doc """
Sets the coverage mode for modules that are subsequently loaded, similar to
option [\+JPcover](`e:erts:erl_cmd.md#%2BJPcover`) for `erl`.

The coverage mode will have the following effect on code that is
loaded following this call:

- **`function`** - All modules that are loaded will be instrumented to keep
  track of which functions are executed. Information about which functions that
  have been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`).

- **`function_counters`** - All modules that are loaded will be instrumented to
  count how many times each function is executed. Information about how many
  times each function has been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`).

- **`line`** - When modules that have been compiled with the
  [`line_coverage`](`m:compile#line_coverage`) option are loaded, they will be
  instrumented to keep track of which lines have been executed. Information
  about which lines have been executed can be retrieved by calling
  [`get_coverage(line, Module)`](`get_coverage/2`), and information about which
  functions that have been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`).

- **`line_counters`** - When modules that have been compiled with the
  [`line_coverage`](`m:compile#line_coverage`) option are loaded, they will be
  instrumented to count the number of times each line is executed. Information
  about how many times each line has been executed can be retrieved by calling
  [`get_coverage(line, Module)`](`get_coverage/2`), and information about which
  functions that have been executed can be retrieved by calling
  [`get_coverage(function, Module)`](`get_coverage/2`) (note that in this mode,
  counters for the number of times each function has been executed **cannot** be
  retrieved).

- **`none`** - Modules will be loaded without coverage instrumentation.

Returns the previous coverage mode.

Failures:

- **`badarg`** - If `Mode` is not a valid coverage mode.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec set_coverage_mode(Mode) -> OldMode when
      Mode :: coverage_mode(),
      OldMode :: coverage_mode().
set_coverage_mode(_Mode) ->
    erlang:nif_error(undefined).

-doc """
Resets coverage information for module `Module`.

If the [coverage mode](`t:coverage_mode/0`) is either `function` or
`line`, all booleans for `Module` keeping track of executed functions
or lines are set to `false`.

If the coverage mode is either `function_counters` or
`line_counters`, all counters for `Module` are reset to zero.

Failures:

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `Module` does not refer to a loaded module.

- **`badarg`** - If `Module` was not loaded with coverage enabled.

- **`badarg`** - If the runtime system does not support coverage.

_See also:_ [Native Coverage Support](#module-native-coverage-support)
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec reset_coverage(Module) -> 'ok' when
      Module :: module().
reset_coverage(_Module) ->
    erlang:nif_error(undefined).

-doc """
Returns `true` if the system supports coverage and `false` otherwise.

_See also:_ [Native Coverage Support](#module-native-coverage-support)
""".
-doc(#{since => <<"OTP 27.0">>}).
-spec coverage_support() -> Supported when
      Supported :: boolean().
coverage_support() ->
    erlang:nif_error(undefined).
