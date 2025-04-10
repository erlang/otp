%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
-module(zip).
-moduledoc """
Utility for reading and creating 'zip' archives.

This module archives and extracts files to and from a zip archive. The zip
format is specified by the "ZIP Appnote.txt" file, available on the PKWARE web
site [www.pkware.com](http://www.pkware.com).

The zip module supports zip archive versions up to 6.1. However,
password-protection is not supported.

By convention, the name of a zip file is to end with `.zip`. To abide to the
convention, add `.zip` to the filename.

- To create zip archives, use function `zip/2` or `zip/3`. They are
  also available as [`create/2,3`](`create/3`), to resemble the `m:erl_tar` module.
- To extract files from a zip archive, use function `unzip/1` or `unzip/2`. They
  are also available as [`extract/1,2`](`extract/1`), to resemble the `m:erl_tar` module.
- To fold a function over all files in a zip archive, use function `foldl/3`.
- To return a list of the files in a zip archive, use function `list_dir/1` or
  `list_dir/2`. They are also available as [`table/1,2`](`table/1`), to resemble the
  `m:erl_tar` module.
- To print a list of files to the Erlang shell, use function `t/1` or `tt/1`.
- Sometimes it is desirable to open a zip archive, and to unzip files from it
  file by file, without having to reopen the archive. This can be done by
  functions [`zip_open/1,2`](`zip_open/1`), [`zip_get/1,2`](`zip_get/1`),
  `zip_list_dir/1`, and `zip_close/1`.
- The ZIP extensions 0x5355 "extended timestamps" and 0x7875 "UID+GID handling"
  are supported. Both extensions are by default enabled when creating an archive,
  but only "extended timestamps" are enabled when extracting. Use the `t:extra/0`
  option to change how these extensions are used.

## Limitations

- Password-protected and encrypted archives are not supported.
- Only the DEFLATE (zlib-compression) and the STORE (uncompressed data) zip
  methods are supported.
- Comments for individual files are not supported when creating zip archives.
  The zip archive comment for the whole zip archive is supported.
- Changing a zip archive is not supported. To add or remove a file from an
  archive, the whole archive must be recreated.
""".
-define(ERL_TAR_COMPATIBILITY, ~"erl_tar compatibility functions").

-compile(nowarn_deprecated_catch).

%% Basic api
-export([unzip/1, unzip/2, extract/1, extract/2,
	 zip/2, zip/3, create/2, create/3, foldl/3,
	 list_dir/1, list_dir/2, table/1, table/2,
	 t/1, tt/1]).

%% zip server
-export([zip_open/1, zip_open/2,
	 zip_get/1, zip_get/2, zip_get_crc32/2,
	 zip_list_dir/1, zip_close/1]).

%% includes
-include("file.hrl").		 % #file_info
-include("zip.hrl").	         % #zip_file, #zip_comment

%% max bytes fed to zlib
-define(WRITE_BLOCK_SIZE, 8*1024).

%% for debugging, to turn off catch
-define(CATCH(Expr), (catch (Expr))).

%% option sets
-record(unzip_opts, {
	  output,      % output object (fun)
	  input,       % input object (fun)
	  file_filter, % file filter (boolean fun)
	  open_opts,   % options passed to file:open
	  feedback,    % feeback (fun)
	  cwd,         % directory to relate paths to
          skip_dirs,   % skip creating empty directories
          extra        % The extra fields to include
	 }).

-record(zip_opts, {
	  output,      % output object (fun)
	  input,       % input object (fun)
	  comment,     % zip-file comment
	  open_opts,   % options passed to file:open
	  feedback,    % feeback (fun)
	  cwd,         % directory to relate paths to
	  compress,    % compress files with these suffixes
	  uncompress,  % uncompress files with these suffixes
          extra        % The extra fields to include
	 }).

-record(list_dir_opts, {
	  input,       % input object (fun)
	  raw_iterator,% applied to each dir entry
	  open_opts,   % options passed to file:open
          skip_dirs,   % skip creating empty directories
          extra        % The extra fields to include
	 }).

-record(openzip_opts, {
	  output,      % output object (fun)
	  open_opts,   % file:open options
	  cwd,	       % directory to relate paths to
          skip_dirs,   % skip creating empty directories
          extra        % The extra fields to include
	 }).

% openzip record, state for an open zip-file
-record(openzip, {
	  zip_comment, % zip archive comment
	  files,       % filenames, infos, comments and offsets
	  in,          % archive handle
	  input,       % archive io object (fun)
	  output,      % output io object (fun)
	  zlib,	       % handle to open zlib
	  cwd,	       % directory to relate paths to
          skip_dirs,   % skip creating empty directories
          extra        % The extra fields to include
	 }).

% Things that I would like to add to the public record #zip_file,
% but can't as it would make things fail at upgrade.
% Instead we use {#zip_file,#zip_file_extra} internally.
-record(zip_file_extra, {
	  crc32        % checksum
	 }).

%% max bytes read from files and archives (and fed to zlib)
-define(READ_BLOCK_SIZE, 16*1024).

%% ZIP-file format records and defines

%% compression methods
-define(STORED, 0).
-define(UNCOMPRESSED, 0).
-define(SHRUNK, 1).
-define(REDUCED_1, 2).
-define(REDUCED_2, 3).
-define(REDUCED_3, 4).
-define(REDUCED_4, 5).
-define(IMPLODED, 6).
-define(TOKENIZED, 7).
-define(DEFLATED, 8).
-define(DEFLATED_64, 9).
-define(PKWARE_IMPLODED, 10).
-define(PKWARE_RESERVED, 11).
-define(BZIP2_COMPRESSED, 12).

%% Version 2.6, attribute compatibility type 3 (Unix)
-define(OS_MADE_BY_UNIX, 3).
-define(VERSION_NEEDED_STORE, 10).
-define(VERSION_NEEDED_DEFLATE, 20).
-define(VERSION_NEEDED_ZIP64, 45).
-define(VERSION_MADE_BY, 61).
-define(GP_BIT_11, 16#800). % Filename and file comment UTF-8 encoded.

%% zip-file records
-define(LOCAL_FILE_MAGIC,16#04034b50).
-define(LOCAL_FILE_HEADER_SZ,(4+2+2+2+2+2+4+4+4+2+2)).
-define(LOCAL_FILE_HEADER_CRC32_OFFSET, 4+2+2+2+2+2).
-record(local_file_header,
        {
         %% Common with cd_file_header
         version_needed,
         gp_flag,
         comp_method,
         last_mod_time,
         last_mod_date,
         crc32,
         comp_size,
         uncomp_size,
         %% X5455_EXTENDED_TIMESTAMP extension
         mtime,
         atime,
         ctime,
         %% X7875_UNIX3 extension
         uid = 0,
         gid = 0,
         %% local_file_header specific
         file_name_length,
         extra_field_length,
         %% extra data needed to create cd_file_header
         info :: undefined | file:file_info()
        }).
-define(EXTRA_OPTIONS, [extended_timestamp, uid_gid]).
-define(X0001_ZIP64, 16#0001).
-define(X5455_EXTENDED_TIMESTAMP, 16#5455).
-define(X7875_UNIX3, 16#7875).

-define(CENTRAL_FILE_HEADER_SZ,(4+2+2+2+2+2+2+4+4+4+2+2+2+2+2+4+4)).

-define(CENTRAL_DIR_MAGIC, 16#06054b50).
-define(CENTRAL_DIR_SZ, (4+2+2+2+2+4+4+2)).
-define(CENTRAL_DIR_DIGITAL_SIG_MAGIC, 16#05054b50).
-define(CENTRAL_DIR_DIGITAL_SIG_SZ, (4+2)).
-define(CENTRAL_FILE_MAGIC, 16#02014b50).

-define(DEFAULT_REGULAR_FILE_MODE, 8#644).
-define(DEFAULT_DIRECTORY_FILE_MODE, 8#744).

-record(cd_file_header,
        {
         %% Common with local_file_header
         version_needed,
         gp_flag,
         comp_method,
         last_mod_time,
         last_mod_date,
         crc32,
         comp_size,
         uncomp_size,
         %% X5455_EXTENDED_TIMESTAMP extension
         mtime,
         atime,
         ctime,
         %% X7875_UNIX3 extension
         uid = 0,
         gid = 0,
         %% cd_file_header specific
         version_made_by,
         os_made_by,
         file_name_length,
         extra_field_length,
         file_comment_length,
         disk_num_start,
         internal_attr,
         external_attr,
         local_header_offset
        }).

-define(END_OF_CENTRAL_DIR_64_LOCATOR_MAGIC, 16#07064b50).
-define(END_OF_CENTRAL_DIR_64_LOCATOR_SZ, (4+8+4)).
-define(END_OF_CENTRAL_DIR_64_MAGIC, 16#06064b50).
-define(END_OF_CENTRAL_DIR_64_SZ, (2+2+4+4+8+8+8+8)).
-define(END_OF_CENTRAL_DIR_MAGIC, 16#06054b50).
-define(END_OF_CENTRAL_DIR_SZ, (4+2+2+2+2+4+4+2)).
-define(MAX_INT32, 16#FFFF_FFFF).
-define(MAX_INT16, 16#FFFF).

%% 1.0 default version
%% 2.0 Deflate version
%% 4.5 File used ZIP64 format extension
%% 6.1 Version made by
-type zip_versions() :: 10 | 20 | 45 | 61.

-record(eocd, {eocd :: undefined | #eocd{},
               version_made_by = 10 :: zip_versions(),
               os_made_by = ~"UNIX" :: unicode:chardata() | 0..255,
               extract_version = 10 :: zip_versions(),
               disk_num,
	       start_disk_num,
	       entries_on_disk,
	       entries,
	       size,
	       offset,
	       zip_comment_length,
               extra}).

-doc """
The possible extra extension that can be used.

- **`extended_timestamp`** - enables the 0x5455 "extended timestamps" zip extension
  that embeds POSIX timestamps for access and modification times for each file in the
  archive. This makes the timestamps to be in UTC instead of local time and also increases
  the time resolution from 2 seconds to 1 second.
- **`uid_gid`** - enables 0x7875 "UNIX 3rd generation" zip extension that embeds the
  UID and GID for each file into the archive.
""".
-type extra() :: [extended_timestamp | uid_gid].

-doc "These options are described in [`create/3`](`m:zip#zip_options`).".
-type create_option() :: memory | cooked | verbose
                       | {comment, Comment ::string()}
                       | {cwd, CWD :: file:filename()}
                       | {compress, What :: extension_spec()}
                       | {uncompress, What :: extension_spec()}
                       | {extra, extra()}.
-doc ~'A filename extension, for example ".txt".'.
-type extension() :: string().
-type extension_spec() :: all
                        | [Extension :: extension()]
                        | {add, [Extension :: extension()]}
                        | {del, [Extension :: extension()]}.
-doc "The name of a zip file.".
-type filename() :: file:filename().

-doc "The record `zip_comment` only contains the archive comment for a zip archive.".
-type zip_comment() :: #zip_comment{}.
-doc """
The record `zip_file` contains the following fields:

- **`name`** - The filename

- **`info`** - File information as in `file:read_file_info/1` in Kernel.
  `mtime`, `atime` and `ctime` are expected to be
  in [`local time`](`erlang:localtime/0`) if represented using `t:calendar:datetime/0`,
  or in [OS system time](`e:erts:time_correction.md#os-system-time`) if represented by an integer.

- **`comment`** - The comment for the file in the zip archive

- **`offset`** - The file offset in the zip archive (used internally)

- **`comp_size`** - The size of the compressed file (the size of the
  uncompressed file is found in `info`)
""".
-type zip_file() :: #zip_file{}.

-doc "As returned by `zip_open/2`.".
-opaque handle() :: pid().

-export_type([create_option/0, filename/0, handle/0]).

%% Extract from a zip archive with options
%%
%% Accepted options:
%% verbose, cooked, file_list, keep_old_files, file_filter, memory

-doc(#{equiv => unzip(Archive, [])}).
-spec(unzip(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}]).

unzip(F) -> unzip(F, []).

-doc """
Extracts all files from a zip archive.

If argument `Archive` is specified as a `t:binary/0`, the contents of the binary is
assumed to be a zip archive, otherwise a filename.

Options:

- **`{file_list, FileList}`** - By default, all files are extracted from the zip
  archive. With option `{file_list, FileList}`, function [`unzip/2`](`unzip/2`)
  only extracts the files whose names are included in `FileList`. The full
  paths, including the names of all subdirectories within the zip archive, must
  be specified.

- **`cooked`** - By default, this function opens the zip file in `raw` mode,
  which is faster but does not allow a remote (Erlang) file server to be used.
  Adding `cooked` to the mode list overrides the default and opens the zip file
  without option `raw`. The same applies for the files extracted.

- **`keep_old_files`** - By default, all files with the same name as files in
  the zip archive are overwritten. With option `keep_old_files` set, function
  [`unzip/2`](`unzip/2`) does not overwrite existing files. Notice that even
  with option `memory` specified, which means that no files are overwritten,
  existing files are excluded from the result.

- **`skip_directories`** - By default empty directories within zip archives are
  extracted. With option `skip_directories` set, empty directories are no longer
  created.

- **`{extra, Extras}`** - The zip "extra" features to respect. The supported
  "extra" features are "extended timestamps" and "UID and GID" handling.
  By default only "extended timestamps" is enabled when unzipping.
  See `t:extra/0` for more details.

- **`verbose`** - Prints an informational message for each extracted file.

- **`memory`** - Instead of extracting to the current directory, the result is
  given as a list of tuples `{Filename, Binary}`, where `Binary` is a binary
  containing the extracted data of file `Filename` in the zip archive.

- **`{cwd, CWD}`** - Uses the specified directory as current directory. It is
  prepended to filenames when extracting them from the zip archive. (Acting like
  `file:set_cwd/1` in Kernel, but without changing the global `cwd` property.)
""".
-spec(unzip(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      Options :: [Option],
      Option  :: {file_list, FileList} | cooked
               | keep_old_files | verbose | memory | skip_directories |
                 {file_filter, FileFilter} | {cwd, CWD} |
                 {extra, extra()},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}],
      FileFilter :: fun((ZipFile) -> boolean()),
      CWD :: file:filename(),
      ZipFile :: zip_file(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}}).

unzip(F, Options) ->
    case ?CATCH(do_unzip(F, Options)) of
	{ok, R} -> {ok, R};
	Error -> {error, Error}
    end.

do_unzip(F, Options) ->
    Opts = get_unzip_options(F, Options),
    #unzip_opts{input = Input, open_opts = OpO,
                extra = ExtraOpts} = Opts,
    In0 = Input({open, F, OpO -- [write]}, []),
    RawIterator = fun raw_file_info_etc/5,
    {Info, In1} = get_central_dir(In0, RawIterator, Input, ExtraOpts),
    %% get rid of zip-comment
    Z = zlib:open(),
    Files = try
                get_z_files(Info, Z, In1, Opts, [])
            after
                zlib:close(Z),
                Input(close, In1)
            end,
    {ok, Files}.

%% Iterate over all files in a zip archive
-doc """
Calls `Fun(FileInArchive, GetInfo, GetBin, AccIn)` on successive files in the
`Archive`, starting with `AccIn == Acc0`.

`FileInArchive` is the name that the file has in the archive.

`GetInfo` is a fun that returns information about the file.

`GetBin` returns the file contents.

Both `GetInfo` and `GetBin` must be called within the `Fun`. Their behavior is
undefined if they are called outside the context of `Fun`.

The `Fun` must return a new accumulator, which is passed to the next call.
[`foldl/3`](`foldl/3`) returns the final accumulator value. `Acc0` is returned
if the archive is empty. It is not necessary to iterate over all files in the
archive. The iteration can be ended prematurely in a controlled manner by
throwing an exception.

_Example:_

```erlang
> Name = "dummy.zip".
"dummy.zip"
> {ok, {Name, Bin}} = zip:create(Name, [{"foo", <<"FOO">>}, {"bar", <<"BAR">>}], [memory]).
{ok,{"dummy.zip",
     <<80,75,3,4,20,0,0,0,0,0,74,152,97,60,171,39,212,26,3,0,
       0,0,3,0,0,...>>}}
> {ok, FileSpec} = zip:foldl(fun(N, I, B, Acc) -> [{N, B(), I()} | Acc] end, [], {Name, Bin}).
{ok,[{"bar",<<"BAR">>,
      {file_info,3,regular,read_write,
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 54,1,0,0,0,0,0}},
     {"foo",<<"FOO">>,
      {file_info,3,regular,read_write,
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 {{2010,3,1},{19,2,10}},
                 54,1,0,0,0,0,0}}]}
> {ok, {Name, Bin}} = zip:create(Name, lists:reverse(FileSpec), [memory]).
{ok,{"dummy.zip",
     <<80,75,3,4,20,0,0,0,0,0,74,152,97,60,171,39,212,26,3,0,
       0,0,3,0,0,...>>}}
> catch zip:foldl(fun("foo", _, B, _) -> throw(B()); (_,_,_,Acc) -> Acc end, [], {Name, Bin}).
<<"FOO">>
```
""".
-doc(#{since => <<"OTP R14B">>}).
-spec(foldl(Fun, Acc0, Archive) -> {ok, Acc1} | {error, Reason} when
      Fun :: fun((FileInArchive, GetInfo, GetBin, AccIn) -> AccOut),
      FileInArchive :: file:name(),
      GetInfo :: fun(() -> file:file_info()),
      GetBin :: fun(() -> binary()),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Archive :: file:name() | {file:name(), binary()},
      Reason :: term()).

foldl(Fun, Acc0, {_Filename, Binary}) ->
    foldl(Fun, Acc0, Binary);
foldl(Fun, Acc0, Archive) when is_function(Fun, 4) ->
    case zip_open(Archive,[memory]) of
	{ok, Handle} ->
            {ok, Files} = zip_list_dir(Handle),
            Acc1 =
                lists:foldl(
                  fun(#zip_comment{}, Acc) ->
                          Acc;
                     (#zip_file{ name = Name, info = Info }, Acc) ->
                          GetInfo = fun() -> Info end,
                          GetBin = case lists:last(Name) of
                                       $/ -> fun() -> <<>> end;
                                       _ ->
                                           fun() ->
                                                   case zip_get(Name, Handle) of
                                                       {ok, {Name, Data}} -> Data;
                                                       {error, Error} -> throw({Name, Error})
                                                   end
                                           end
                                   end,
                          Fun(Name, GetInfo, GetBin, Acc)
                  end, Acc0, Files),
	    ok = zip_close(Handle),
	    {ok, Acc1};
	{error, bad_eocd} ->
	    {error, "Not an archive file"};
	{error, Reason} ->
	    {error, Reason}
    end;
foldl(_,_, _) ->
    {error, einval}.

%% Create zip archive name F from Files or binaries
%%
%% Accepted options:
%% verbose, cooked, memory, comment

-doc(#{equiv => zip(Name, FileList, [])}).
-spec(zip(Name, FileList) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      RetValue :: {ok, FileName :: file:name()}
                | {ok, {FileName :: file:name(), binary()}}
                | {error, Reason :: term()}).

zip(F, Files) -> zip(F, Files, []).

-doc """
Creates a zip archive containing the files specified in `FileList`.

`FileList` is a list of files, with paths relative to the current directory,
which are stored with this path in the archive. File system operations are
performed to read the file metadata and, when compression is enabled, to stream
the file contents without loading whole files into memory. Files can also be
specified as binaries to create an archive directly from data. In such cases, no
metadata or file system reads are performed.

Files are compressed using the DEFLATE compression, as described in the
"Appnote.txt" file. However, files are stored without compression if they are
already compressed. [`zip/2`](`zip/2`) and [`zip/3`](`zip/3`) check the file
extension to determine if the file is to be stored without compression. Files
with the following extensions are not compressed: `.Z`, `.zip`, `.zoo`, `.arc`,
`.lzh`, `.arj`.

It is possible to override the default behavior and control what types of files
that are to be compressed by using options `{compress, What}` and
`{uncompress, What}`. It is also possible to use many `compress` and
`uncompress` options.

To trigger file compression, its extension must match with the `compress`
condition and must not match the `uncompress` condition. For example, if
`compress` is set to `["gif", "jpg"]` and `uncompress` is set to `["jpg"]`, only
files with extension `"gif"` are compressed.

[](){: #zip_options }

Options:

- **`cooked`** - By default, this function opens the zip file in mode `raw`,
  which is faster but does not allow a remote (Erlang) file server to be used.
  Adding `cooked` to the mode list overrides the default and opens the zip file
  without the `raw` option. The same applies for the files added.

- **`verbose`** - Prints an informational message about each added file.

- **`memory`** - The output is not to a file, but instead as a tuple
  `{FileName, binary()}`. The binary is a full zip archive with header and can
  be extracted with, for example, `unzip/2`.

- **`{comment, Comment}`** - Adds a comment to the zip archive.

- **`{cwd, CWD}`** - Uses the specified directory as current work directory
  (`cwd`). This is prepended to filenames when adding them, although not in the
  zip archive (acting like `file:set_cwd/1` in Kernel, but without changing the
  global `cwd` property.).

- **`{extra, Extras}`** - The zip "extra" features to respect. The supported
  "extra" features are "extended timestamps" and "UID and GID" handling.
  By default both these "extra" features are enabled.
  See `t:extra/0` for more details.

- **`{compress, What}`** - Controls what types of files to be compressed.
  Defaults to `all`. The following values of `What` are allowed:

  - **`all`** - All files are compressed (as long as they pass the `uncompress`
    condition).

  - **`[Extension]`** - Only files with exactly these extensions are compressed.

  - **`{add,[Extension]}`** - Adds these extensions to the list of compress
    extensions.

  - **`{del,[Extension]}`** - Deletes these extensions from the list of compress
    extensions.

- **`{uncompress, What}`** - Controls what types of files to be uncompressed.
  Defaults to `[".Z", ".zip", ".zoo", ".arc", ".lzh", ".arj"]`. The following
  values of `What` are allowed:

  - **`all`** - No files are compressed.

  - **`[Extension]`** - Files with these extensions are uncompressed.

  - **`{add,[Extension]}`** - Adds these extensions to the list of uncompress
    extensions.

  - **`{del,[Extension]}`** - Deletes these extensions from the list of
    uncompress extensions.
""".
-spec(zip(Name, FileList, Options) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      Options  :: [Option],
      Option   :: create_option(),
      RetValue :: {ok, FileName :: file:name()}
                | {ok, {FileName :: file:name(), binary()}}
                | {error, Reason :: term()}).

zip(F, Files, Options) ->
    case ?CATCH(do_zip(F, Files, Options)) of
	{ok, R} -> {ok, R};
	Error -> {error, Error}
    end.

do_zip(F, Files, Options) ->
    Opts = get_zip_options(Files, Options),
    #zip_opts{output = Output, open_opts = OpO} = Opts,
    Out0 = Output({open, F, OpO}, []),
    Z = zlib:open(),
    try
        {Out1, LHS, Pos} = put_z_files(Files, Z, Out0, 0, Opts, []),
        zlib:close(Z),
        Out2 = put_central_dir(LHS, Pos, Out1, Opts),
        Out3 = Output(flush, Output({close, F}, Out2)),
        {ok, Out3}
    catch
        C:R:Stk ->
            ?CATCH(zlib:close(Z)),
            Output(flush, Output({close, F}, Out0)),
            erlang:raise(C, R, Stk)
    end.


%% List zip directory contents
%%
%% Accepted options:
%% cooked, file_filter, file_output (latter 2 undocumented)

-doc(#{equiv => list_dir(Archive, [])}).
-spec(list_dir(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()]).

list_dir(F) -> list_dir(F, []).

-doc """
Retrieves all filenames in the zip archive `Archive`.

The result value is the tuple `{ok, List}`, where `List` contains the zip
archive comment as the first element.

One option is available:

- **`cooked`** - By default, this function opens the zip file in `raw` mode,
  which is faster but does not allow a remote (Erlang) file server to be used.
  Adding `cooked` to the mode list overrides the default and opens the zip file
  without option `raw`.

- **`skip_directories`** - By default empty directories within zip archives are
  listed. With option `skip_directories` set, empty directories are no longer
  listed.

- **`{extra, Extras}`** - The zip "extra" features to respect. The supported
  "extra" features are "extended timestamps" and "UID and GID" handling.
  By default only "extended timestamps" is enabled when listing files.
  See `t:extra/0` for more details.
""".
-spec(list_dir(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()],
      Options :: [Option],
      Option :: cooked | skip_directories | {extra, extra()}).

list_dir(F, Options) ->
    case ?CATCH(do_list_dir(F, Options)) of
	{ok, R} -> {ok, R};
	Error -> {error, Error}
    end.

do_list_dir(F, Options) ->
    Opts = get_list_dir_options(F, Options),
    #list_dir_opts{input = Input, open_opts = OpO,
		   raw_iterator = RawIterator,
                   skip_dirs = SkipDirs,
                   extra = ExtraOpts} = Opts,
    In0 = Input({open, F, OpO}, []),
    {Info, In1} = get_central_dir(In0, RawIterator, Input, ExtraOpts),
    Input(close, In1),
    if SkipDirs ->
            {ok,
             lists:filter(
               fun(#zip_file{ name = Name }) ->
                       lists:last(Name) =/= $/;
                  (#zip_comment{}) ->
                       true
               end, Info)};
       true ->
            {ok, Info}
    end.

-doc(#{equiv => zip_open/2}).
-spec(zip_open(Archive) -> {ok, ZipHandle} | {error, Reason} when
      Archive :: file:name() | binary(),
      ZipHandle :: handle(),
      Reason :: term()).

zip_open(Archive) -> zip_open(Archive, []).

-doc """
Opens a zip archive, and reads and saves its directory. This means that later
reading files from the archive is faster than unzipping files one at a time with
[`unzip/1,2`](`unzip/1`).

The options are equivalent to those in `unzip/2`.

The archive must be closed with `zip_close/1`.

The `ZipHandle` is closed if the process that originally opened the archive
dies.
""".
-spec(zip_open(Archive, Options) -> {ok, ZipHandle} | {error, Reason} when
      Archive :: file:name() | binary(),
      ZipHandle :: handle(),
      Options :: [Option],
      Option :: cooked | memory | {cwd, CWD :: file:filename()} | {extra, extra()},
      Reason :: term()).

zip_open(Archive, Options) ->
    Self = self(),
    Pid = spawn_link(fun() -> server_init(Self) end),
    request(Self, Pid, {open, Archive, Options}).

-doc(#{equiv => zip_get/2}).
-spec(zip_get(ZipHandle) -> {ok, [Result]} | {error, Reason} when
      ZipHandle :: handle(),
      Result :: file:name() | {file:name(), binary()},
      Reason :: term()).

zip_get(Pid) when is_pid(Pid) ->
    request(self(), Pid, get).

-doc """
Closes a zip archive, previously opened with [`zip_open/1,2`](`zip_open/1`). All
resources are closed, and the handle is not to be used after closing.
""".
-spec(zip_close(ZipHandle) -> ok | {error, einval} when
      ZipHandle :: handle()).

zip_close(Pid) when is_pid(Pid) ->
    request(self(), Pid, close).

-doc """
Extracts one or all files from an open archive.

The files are unzipped to memory or to file, depending on the options specified
to function [`zip_open/1,2`](`zip_open/1`) when opening the archive.
""".
-spec(zip_get(FileName, ZipHandle) -> {ok, Result} | {error, Reason} when
      FileName :: file:name(),
      ZipHandle :: handle(),
      Result :: file:name() | {file:name(), binary()},
      Reason :: term()).

zip_get(FileName, Pid) when is_pid(Pid) ->
    request(self(), Pid, {get, FileName}).

-doc "Extracts one crc32 checksum from an open archive.".
-doc(#{since => <<"OTP 26.0">>}).
-spec(zip_get_crc32(FileName, ZipHandle) -> {ok, CRC} | {error, Reason} when
      FileName :: file:name(),
      ZipHandle :: handle(),
      CRC :: non_neg_integer(),
      Reason :: term()).

zip_get_crc32(FileName, Pid) when is_pid(Pid) ->
    request(self(), Pid, {get_crc32, FileName}).

-doc """
Returns the file list of an open zip archive. The first returned element is the
zip archive comment.
""".
-spec(zip_list_dir(ZipHandle) -> {ok, Result} | {error, Reason} when
      Result :: [zip_comment() | zip_file()],
      ZipHandle :: handle(),
      Reason :: term()).

zip_list_dir(Pid) when is_pid(Pid) ->
    request(self(), Pid, list_dir).

request(Self, Pid, Req) ->
    Pid ! {Self, Req},
    receive
	{Pid, R} -> R
    end.

zip_t(Pid) when is_pid(Pid) ->
    Openzip = request(self(), Pid, get_state),
    openzip_t(Openzip).

zip_tt(Pid) when is_pid(Pid) ->
    Openzip = request(self(), Pid, get_state),
    openzip_tt(Openzip).

%% Print zip directory in short form

-doc """
Prints all filenames in the zip archive `Archive` to the Erlang shell. (Similar
to `tar t`.)
""".
-spec(t(Archive) -> ok when
      Archive :: file:name() | binary() | ZipHandle,
      ZipHandle :: handle()).

t(F) when is_pid(F) -> zip_t(F);
t(F) -> t(F, fun raw_short_print_info_etc/5).

t(F, RawPrint) ->
    case ?CATCH(do_t(F, RawPrint)) of
	ok -> ok;
	Error -> {error, Error}
    end.

do_t(F, RawPrint) ->
    Input = get_input(F),
    OpO = [raw],
    In0 = Input({open, F, OpO}, []),
    {_Info, In1} = get_central_dir(In0, RawPrint, Input, ?EXTRA_OPTIONS),
    Input(close, In1),
    ok.

%% Print zip directory in long form (like ls -l)

-doc """
Prints filenames and information about all files in the zip archive `Archive` to
the Erlang shell. (Similar to `tar tv`.)
""".
-spec(tt(Archive) -> ok when
      Archive :: file:name() | binary() | ZipHandle,
      ZipHandle :: handle()).

tt(F) when is_pid(F) -> zip_tt(F);
tt(F) -> t(F, fun raw_long_print_info_etc/5).


%% option utils
get_unzip_opt([], Opts) ->
    Opts;
get_unzip_opt([verbose | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{feedback = fun verbose_unzip/1});
get_unzip_opt([cooked | Rest], #unzip_opts{open_opts = OpO} = Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{open_opts = OpO -- [raw]});
get_unzip_opt([memory | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{output = fun binary_io/2});
get_unzip_opt([{cwd, CWD} | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{cwd = CWD});
get_unzip_opt([{file_filter, F} | Rest], Opts) ->
    Filter1 = fun({ZipFile,_Extra, _CWD}) -> F(ZipFile) end,
    Filter2 = fun_and_1(Filter1, Opts#unzip_opts.file_filter),
    get_unzip_opt(Rest, Opts#unzip_opts{file_filter = Filter2});
get_unzip_opt([{file_list, L} | Rest], Opts) ->
    FileInList = fun({ZipFile, _Extra, _CWD}) -> file_in_list(ZipFile, L) end,
    Filter = fun_and_1(FileInList, Opts#unzip_opts.file_filter),
    get_unzip_opt(Rest, Opts#unzip_opts{file_filter = Filter});
get_unzip_opt([keep_old_files | Rest], Opts) ->
    Keep = fun({ZipFile, _Extra, CWD}) -> keep_old_file(ZipFile, CWD) end,
    Filter = fun_and_1(Keep, Opts#unzip_opts.file_filter),
    get_unzip_opt(Rest, Opts#unzip_opts{file_filter = Filter});
get_unzip_opt([skip_directories | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{skip_dirs = true});
get_unzip_opt([{extra, What} = O| Rest], Opts) when is_list(What) ->
    case lists:all(fun(E) -> lists:member(E, ?EXTRA_OPTIONS) end, What) of
        true ->
            get_zip_opt(Rest, Opts#unzip_opts{extra = What});
        false ->
            throw({bad_option, O})
    end;
get_unzip_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

get_list_dir_opt([], Opts) ->
    Opts;
get_list_dir_opt([cooked | Rest], #list_dir_opts{open_opts = OpO} = Opts) ->
    get_list_dir_opt(Rest, Opts#list_dir_opts{open_opts = OpO -- [raw]});
get_list_dir_opt([names_only | Rest], Opts) ->
    get_list_dir_opt(Rest, Opts#list_dir_opts{
			     raw_iterator = fun(A, B, C, D, E) -> raw_name_only(A, B, C, D, E) end});
get_list_dir_opt([skip_directories | Rest], Opts) ->
    get_list_dir_opt(Rest, Opts#list_dir_opts{skip_dirs = true});
get_list_dir_opt([{extra, What} = O| Rest], Opts) when is_list(What) ->
    case lists:all(fun(E) -> lists:member(E, ?EXTRA_OPTIONS) end, What) of
        true ->
            get_zip_opt(Rest, Opts#list_dir_opts{extra = What});
        false ->
            throw({bad_option, O})
    end;
%% get_list_dir_opt([{file_output, F} | Rest], Opts) ->
%%     get_list_dir_opt(Rest, Opts#list_dir_opts{file_output = F});
%% get_list_dir_opt([{file_filter, F} | Rest], Opts) ->
%%     get_list_dir_opt(Rest, Opts#list_dir_opts{file_filter = F});
get_list_dir_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

get_zip_opt([], Opts) ->
    Opts;
get_zip_opt([verbose | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{feedback = fun verbose_zip/1});
get_zip_opt([cooked | Rest], #zip_opts{open_opts = OpO} = Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{open_opts = OpO -- [raw]});
get_zip_opt([memory | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{output = fun binary_io/2});
get_zip_opt([{cwd, CWD} | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{cwd = CWD});
get_zip_opt([{comment, C} | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{comment = C});
get_zip_opt([{compress, Which} = O| Rest], Opts) ->
    Which2 =
	case Which of
	    all ->
		all;
	    Suffixes when is_list(Suffixes) ->
		lists:usort(Suffixes);
	    {add, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.compress ++ Suffixes);
	    {del, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.compress -- Suffixes);
	    _ ->
		throw({bad_option, O})
	end,
    get_zip_opt(Rest, Opts#zip_opts{compress = Which2});
get_zip_opt([{uncompress, Which} = O| Rest], Opts) ->
    Which2 =
	case Which of
	    all ->
		all;
	    Suffixes when is_list(Suffixes) ->
		lists:usort(Suffixes);
	    {add, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.uncompress ++ Suffixes);
	    {del, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.uncompress -- Suffixes);
	    _ ->
		throw({bad_option, O})
	end,
    get_zip_opt(Rest, Opts#zip_opts{uncompress = Which2});
get_zip_opt([{extra, What} = O| Rest], Opts) when is_list(What) ->
    case lists:all(fun(E) -> lists:member(E, ?EXTRA_OPTIONS) end, What) of
        true ->
            get_zip_opt(Rest, Opts#zip_opts{extra = What});
        false ->
            throw({bad_option, O})
    end;
get_zip_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).


%% feedback funs
silent(_) -> ok.

verbose_unzip(FN) ->
    io:format("extracting: ~ts\n", [io_lib:write_string(FN)]).

verbose_zip(FN) ->
    io:format("adding: ~ts\n", [io_lib:write_string(FN)]).

%% file filter funs
all(_) -> true.

file_in_list(#zip_file{name = FileName}, List) ->
    lists:member(FileName, List);
file_in_list(_, _) ->
    false.

keep_old_file(#zip_file{name = FileName}, CWD) ->
    FileName1 = add_cwd(CWD, FileName),
    not (filelib:is_file(FileName1) orelse filelib:is_dir(FileName1));
keep_old_file(_, _) ->
    false.

%% fun combiner
fun_and_1(Fun1, Fun2) ->
    fun(A) -> Fun1(A) andalso Fun2(A) end.

%% getting options
get_zip_options(Files, Options) ->
    Suffixes = [".Z", ".zip", ".zoo", ".arc", ".lzh", ".arj"],
    Opts = #zip_opts{output = fun file_io/2,
		     input = get_zip_input({files, Files}),
		     open_opts = [raw, write],
		     comment = "",
		     feedback = fun silent/1,
		     cwd = "",
		     compress = all,
		     uncompress = Suffixes,
                     extra = ?EXTRA_OPTIONS
		    },
    Opts1 = #zip_opts{comment = Comment} = get_zip_opt(Options, Opts),
    %% UTF-8 encode characters in the interval from 127 to 255.
    {Comment1, _} = encode_string(Comment),
    Opts1#zip_opts{comment = Comment1}.

get_unzip_options(F, Options) ->
    Opts = #unzip_opts{file_filter = fun all/1,
		       output = fun file_io/2,
		       input = get_input(F),
		       open_opts = [raw],
		       feedback = fun silent/1,
                       skip_dirs = false,
		       cwd = "",
                       extra = [extended_timestamp]
		      },
    get_unzip_opt(Options, Opts).

get_openzip_options(Options) ->
    Opts = #openzip_opts{open_opts = [raw, read],
			 output = fun file_io/2,
			 cwd = "",
                         skip_dirs = false,
                         extra = ?EXTRA_OPTIONS},
    get_openzip_opt(Options, Opts).

get_input(F) when is_binary(F) ->
    fun binary_io/2;
get_input(F) when is_list(F) ->
    fun file_io/2;
get_input(_) ->
    throw(einval).

get_zip_input({F, B}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input({F, B, #file_info{}}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input({F, #file_info{}, B}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input(F) when is_list(F) ->
    fun file_io/2;
get_zip_input({files, []}) ->
    fun binary_io/2;
get_zip_input({files, [File | _]}) ->
    get_zip_input(File);
get_zip_input(_) ->
    throw(einval).

get_list_dir_options(F, Options) ->
    Opts = #list_dir_opts{raw_iterator = fun raw_file_info_public/5,
			  input = get_input(F),
			  open_opts = [raw],
                          skip_dirs = false,
                          extra = [extended_timestamp]},
    get_list_dir_opt(Options, Opts).

%% aliases for erl_tar compatibility
-doc(#{group => ?ERL_TAR_COMPATIBILITY }).
-doc #{ equiv => list_dir(Archive, []) }.
-spec(table(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()]).

table(F) -> list_dir(F).

-doc(#{group => ?ERL_TAR_COMPATIBILITY }).
-doc #{ equiv => list_dir(Archive, Options) }.
-spec(table(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()],

      Options :: [Option],
      Option :: cooked).

table(F, O) -> list_dir(F, O).

-doc(#{group => ?ERL_TAR_COMPATIBILITY }).
-doc(#{ equiv => zip(Name, FileList)} ).
-spec(create(Name, FileList) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      RetValue :: {ok, FileName :: filename()}
                | {ok, {FileName :: filename(), binary()}}
                | {error, Reason :: term()}).

create(F, Fs) -> zip(F, Fs).

-doc(#{group => ?ERL_TAR_COMPATIBILITY }).
-doc(#{ equiv => zip(Name, FileList, Options) }).
-spec(create(Name, FileList, Options) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      Options  :: [Option],
      Option   :: create_option(),
      RetValue :: {ok, FileName :: filename()}
                | {ok, {FileName :: filename(), binary()}}
                | {error, Reason :: term()}).
create(F, Fs, O) -> zip(F, Fs, O).

-doc(#{group => ?ERL_TAR_COMPATIBILITY }).
-doc(#{ equiv => unzip(Archive)} ).
-spec(extract(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}]).

extract(F) -> unzip(F).

-doc(#{group => ?ERL_TAR_COMPATIBILITY }).
-doc(#{ equiv => unzip(Archive, Options) }).
-spec(extract(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      Options :: [Option],
      Option  :: {file_list, FileList}
               | keep_old_files | verbose | memory |
                 {file_filter, FileFilter} | {cwd, CWD},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}],
      FileFilter :: fun((ZipFile) -> boolean()),
      CWD :: file:filename(),
      ZipFile :: zip_file(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}}).

extract(F, O) -> unzip(F, O).


%% put the central directory, at the end of the zip archive
put_central_dir(LHS, Pos, Out0,
		#zip_opts{output = Output, comment = Comment, extra = ExtraOpts}) ->
    {Out1, Sz} = put_cd_files_loop(LHS, Output, ExtraOpts, Out0, 0),
    put_eocd(length(LHS), Pos, Sz, Comment, Output, Out1).

put_cd_files_loop([], _Output, _ExtraOpts, Out, Sz) ->
    {Out, Sz};
put_cd_files_loop([{LH, Name, Pos} | LHRest], Output, ExtraOpts, Out0, Sz0) ->
    Extra = cd_file_header_extra_from_lh_and_pos(LH, Pos, ExtraOpts),
    CDFH = cd_file_header_from_lh_pos_and_extra(LH, Pos, Extra),
    BCDFH = cd_file_header_to_bin(CDFH),
    B = [<<?CENTRAL_FILE_MAGIC:32/little>>, BCDFH, Name, Extra],
    Out1 = Output({write, B}, Out0),
    Sz1 = Sz0 + ?CENTRAL_FILE_HEADER_SZ +
	CDFH#cd_file_header.file_name_length + CDFH#cd_file_header.extra_field_length,
    put_cd_files_loop(LHRest, Output, ExtraOpts, Out1, Sz1).

cd_file_header_extra_from_lh_and_pos(
  #local_file_header{ comp_size = CompSize,
                      uncomp_size = UnCompSize,
                      info = FI }, Pos, ExtraOpts) ->
    encode_extra(UnCompSize, CompSize, Pos,
                 FI#file_info{ atime = undefined }, ExtraOpts).

%% put end marker of central directory, the last record in the archive
put_eocd(N, Pos, Sz, Comment, Output, Out0) when
      Pos < ?MAX_INT32, N < ?MAX_INT16, Sz < ?MAX_INT32 ->
    CommentSz = length(Comment),
    EOCD = #eocd{disk_num = 0,
		 start_disk_num = 0,
		 entries_on_disk = N,
		 entries = N,
		 size = Sz,
		 offset = Pos,
		 zip_comment_length = CommentSz},
    BEOCD = eocd_to_bin(EOCD),
    B = [<<?END_OF_CENTRAL_DIR_MAGIC:32/little>>, BEOCD, Comment],
    Output({write, B}, Out0);
put_eocd(N, Pos, Sz, Comment, Output, Out0) ->
    %% Zip64 eocd
    EOCD64 = #eocd{os_made_by = ?OS_MADE_BY_UNIX,
                   version_made_by = ?VERSION_MADE_BY,
                   extract_version = ?VERSION_NEEDED_ZIP64,
                   disk_num = 0,
                   start_disk_num = 0,
                   entries_on_disk = N,
                   entries = N,
                   size = Sz,
                   offset = Pos,
                   extra = <<>> },
    BEOCD64 = eocd64_to_bin(EOCD64),
    B = [<<?END_OF_CENTRAL_DIR_64_MAGIC:32/little, (iolist_size(BEOCD64)):64/little>>, BEOCD64],
    Out1 = Output({write, B}, Out0),
    Out2 = Output({write, <<?END_OF_CENTRAL_DIR_64_LOCATOR_MAGIC:32/little,
                            0:32/little, %% Start diskdum
                            (Pos+Sz):64/little,
                            1:32/little>> %% Total disks
                  }, Out1),
    CommentSz = length(Comment),
    EOCD = #eocd{disk_num = 0,
		 start_disk_num = 0,
		 entries_on_disk = min(N,?MAX_INT16),
		 entries = min(N,?MAX_INT16),
		 size = min(Sz,?MAX_INT32),
		 offset = min(Pos, ?MAX_INT32),
		 zip_comment_length = CommentSz},
    Output({write, [<<?END_OF_CENTRAL_DIR_MAGIC:32/little>>, eocd_to_bin(EOCD), Comment]}, Out2).

get_filename({Name, _}, Type) ->
    get_filename(Name, Type);
get_filename({Name, _, _}, Type) ->
    get_filename(Name, Type);
get_filename(Name, regular) ->
    Name;
get_filename(Name, directory) ->
    %% Ensure trailing slash
    case lists:reverse(Name) of
	[$/ | _Rev] -> Name;
	Rev         -> lists:reverse([$/ | Rev])
    end.

add_cwd(_CWD, {_Name, _} = F) -> F;
add_cwd("", F) -> F;
add_cwd(CWD, F) ->
    TrailingSlash = case lists:last(F) of
                        $/ -> "/";
                        _ -> ""
                    end,
    string:trim(filename:join(CWD, F), trailing, "/") ++ TrailingSlash.

%% already compressed data should be stored as is in archive,
%% a simple name-match is used to check for this
%% files smaller than 10 bytes are also stored, not compressed
get_comp_method(_, N, _, _) when is_integer(N), N < 10 ->
    ?STORED;
get_comp_method(_, _, _, directory) ->
    ?STORED;
get_comp_method(F, _, #zip_opts{compress = Compress, uncompress = Uncompress}, _) ->
    Ext = filename:extension(F),
    Test = fun(Which) -> (Which =:= all) orelse lists:member(Ext, Which) end,
    case Test(Compress) andalso not Test(Uncompress) of
	true  -> ?DEFLATED;
	false -> ?STORED
    end.

put_z_files([], _Z, Out, Pos, _Opts, Acc) ->
    {Out, lists:reverse(Acc), Pos};
put_z_files([F | Rest], Z, Out0, Pos0,
	    #zip_opts{input = Input, output = Output, open_opts = OpO,
		      feedback = FB, cwd = CWD, extra = ExtraOpts} = Opts, Acc) ->

    %% {Pos0, _} = Output({position, cur, 0}, Out0), %% Assert correct Pos0

    In0 = [],
    F1 = add_cwd(CWD, F),
    FileInfo = Input({file_info, F1, [{time, posix}]}, In0),
    Type = FileInfo#file_info.type,
    UncompSize =
	case Type of
	    regular -> FileInfo#file_info.size;
	    directory -> 0
	end,
    FileName0 = get_filename(F, Type),
    %% UTF-8 encode characters in the interval from 127 to 255.
    {FileName, GPFlag} = encode_string(FileName0),
    CompMethod = get_comp_method(FileName, UncompSize, Opts, Type),

    %% Add any extra data needed and patch
    Extra = encode_extra(UncompSize, FileInfo, ExtraOpts),

    LH = local_file_header_from_info_method_name(FileInfo, UncompSize, CompMethod,
                                                 FileName, GPFlag, Extra),
    BLH = local_file_header_to_bin(LH),
    B = [<<?LOCAL_FILE_MAGIC:32/little>>, BLH],
    Out1 = Output({write, B}, Out0),
    Out2 = Output({write, FileName}, Out1),

    %% Start of extra data
    Pos1 = Pos0 + ?LOCAL_FILE_HEADER_SZ	+ LH#local_file_header.file_name_length,

    Out3 = Output({write, Extra}, Out2),

    {Out4, CompSize, CRC} = put_z_file(CompMethod, UncompSize, Out3, F1,
				       0, Input, Output, OpO, Z, Type),

    Pos2 = Pos1 + LH#local_file_header.extra_field_length + CompSize,
    FB(FileName0),

    %% Patch the CRC
    Patch = <<CRC:32/little>>,
    Out5 = Output({pwrite, Pos0 + ?LOCAL_FILE_HEADER_CRC32_OFFSET, Patch}, Out4),

    Out6 =
        %% If UncompSize > 4GB we always put the CompSize in the extra field
        if UncompSize >= ?MAX_INT32 ->
                %% 4 bytes for extra header + size and 8 bytes for UnComp:64
                Output({pwrite, Pos1 + 2 + 2 + 8, <<CompSize:64/little>>}, Out5);
           true ->
                %% Patch comp size if not zip64
                Output({pwrite, Pos0 + ?LOCAL_FILE_HEADER_CRC32_OFFSET + 4, <<CompSize:32/little>>}, Out5)
        end,

    Out7 = Output({seek, eof, 0}, Out6),

    %% {Pos2, _} = Output({position, cur, 0}, Out7), %% Assert correct Pos2

    LH2 = LH#local_file_header{uncomp_size = UncompSize, comp_size = CompSize, crc32 = CRC},
    ThisAcc = [{LH2, FileName, Pos0}],
    {Out8, SubAcc, Pos3} =
	case Type of
	    regular ->
		{Out7, ThisAcc, Pos2};
	    directory ->
		Files = Input({list_dir, F1}, []),
		RevFiles = reverse_join_files(F, Files, []),
		put_z_files(RevFiles, Z, Out7, Pos2, Opts, ThisAcc)
	end,
    Acc2 = lists:reverse(SubAcc) ++ Acc,
    put_z_files(Rest, Z, Out8, Pos3, Opts, Acc2).

reverse_join_files(Dir, [File | Files], Acc) ->
    reverse_join_files(Dir, Files, [filename:join([Dir, File]) | Acc]);
reverse_join_files(_Dir, [], Acc) ->
    Acc.

encode_extra(UnCompSize, FileInfo, ExtraOpts) ->
    encode_extra(UnCompSize, 0, 0, FileInfo, ExtraOpts).
encode_extra(UnCompSize, CompSize, Pos, FileInfo, ExtraOpts) ->
    %% zip64 needs to be first so that we can patch the CompSize
    [encode_extra_zip64(UnCompSize, CompSize, Pos),
     [encode_extra_extended_timestamp(FileInfo)  || lists:member(extended_timestamp, ExtraOpts)],
     [encode_extra_uid_gid(FileInfo)  || lists:member(uid_gid, ExtraOpts)]].

encode_extra_header(Header, Value) ->
    [<<Header:16/little, (iolist_size(Value)):16/little>>, Value].

encode_extra_zip64(UncompSize, CompSize, Pos) when UncompSize >= ?MAX_INT32 ->
    encode_extra_header(?X0001_ZIP64, [<<UncompSize:64/little,CompSize:64/little>>,
                                       [<<Pos:64/little>> || Pos >= ?MAX_INT32]]);
encode_extra_zip64(_UncompSize, _CompSize, Pos) when Pos >= ?MAX_INT32 ->
    encode_extra_header(?X0001_ZIP64, <<Pos:64/little>>);
encode_extra_zip64(_, _, _) ->
    <<>>.

encode_extra_extended_timestamp(FI) ->
    {Mbit, MSystemTime} =
        case datetime_to_system_time(FI#file_info.mtime) of
            undefined -> {0, <<>>};
            Mtime ->
                {1, <<(datetime_to_system_time(Mtime)):32/little>>}
        end,

    {Abit, ASystemTime} =
        case datetime_to_system_time(FI#file_info.atime) of
            undefined -> {0, <<>>};
            Atime ->
                {2, <<(datetime_to_system_time(Atime)):32/little>>}
        end,

    encode_extra_header(?X5455_EXTENDED_TIMESTAMP, [Abit bor Mbit, MSystemTime, ASystemTime]).

encode_extra_uid_gid(#file_info{ uid = Uid, gid = Gid })
  when Uid =/= undefined, Gid =/= undefined ->
    encode_extra_header(?X7875_UNIX3,<<1, 4, Uid:32/little,
                                       4, Gid:32/little>>);
encode_extra_uid_gid(_) ->
    <<>>.

%% flag for zlib
-define(MAX_WBITS, 15).

%% compress a file
put_z_file(_Method, Sz, Out, _F, Pos, _Input, _Output, _OpO, _Z, directory) ->
    {Out, Pos + Sz, 0};
put_z_file(_Method, 0, Out, _F, Pos, _Input, _Output, _OpO, _Z, regular) ->
    {Out, Pos, 0};
put_z_file(?STORED, UncompSize, Out0, F, Pos0, Input, Output, OpO, _Z, regular) ->
    In0 = [],
    In1 = Input({open, F, OpO -- [write]}, In0),
    CRC0 = 0,
    {Out1, Pos1, In2, CRC} =
        put_z_data_loop(UncompSize, In1, Out0, Pos0, Input, Output, CRC0, fun(Data, _Sync) -> Data end),
    Input(close, In2),
    {Out1, Pos1, CRC};
put_z_file(?DEFLATED, UncompSize, Out0, F, Pos0, Input, Output, OpO, Z, regular) ->
    In0 = [],
    In1 = Input({open, F, OpO -- [write]}, In0),
    ok = zlib:deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    CRC0 = 0,
    {Out1, Pos1, In2, CRC} =
        put_z_data_loop(UncompSize, In1, Out0, Pos0, Input, Output, CRC0,
        fun(Data, Sync) -> zlib:deflate(Z, Data, Sync) end),
    ok = zlib:deflateEnd(Z),
    Input(close, In2),
    {Out1, Pos1, CRC}.

%% compress data
put_z_data_loop(0, In, Out, Pos, _Input, _Output, CRC0, _DeflateFun) ->
    {Out, Pos, In, CRC0};
put_z_data_loop(UncompSize, In0, Out0, Pos0, Input, Output, CRC0, DeflateFun) ->
    N = erlang:min(?WRITE_BLOCK_SIZE, UncompSize),
    case Input({read, N}, In0) of
        {eof, _In1} ->
            {Out0, Pos0};
        {Uncompressed, In1} ->
            CRC1 = erlang:crc32(CRC0, Uncompressed),
            Compressed = DeflateFun(Uncompressed, get_sync(N, UncompSize)),
            Sz = erlang:iolist_size(Compressed),
            Out1 = Output({write, Compressed}, Out0),
            put_z_data_loop(UncompSize - N, In1, Out1, Pos0 + Sz,
                Input, Output, CRC1, DeflateFun)
    end.

%%  zlib is finished with the last chunk compressed
get_sync(N, N) -> finish;
get_sync(_, _) -> full.

%% raw iterators over central dir

%% name only
raw_name_only(CD, FileName, _FileComment, _BExtraField, Acc)
  when is_record(CD, cd_file_header) ->
    [FileName | Acc];
raw_name_only(EOCD, _, _Comment, _, Acc) when is_record(EOCD, eocd) ->
    Acc.

%% for printing directory (t/1)
raw_short_print_info_etc(CD, FileName, _FileComment, _BExtraField, Acc)
  when is_record(CD, cd_file_header) ->
    print_file_name(FileName),
    Acc;
raw_short_print_info_etc(EOCD, X, Comment, Y, Acc) when is_record(EOCD, eocd) ->
    raw_long_print_info_etc(EOCD, X, Comment, Y, Acc).

print_file_name(FileName) ->
    io:format("~ts\n", [FileName]).


%% for printing directory (tt/1)
raw_long_print_info_etc(#cd_file_header{comp_size = CompSize,
					uncomp_size = UncompSize} = CDFH,
			FileName, FileComment, _BExtraField, Acc) ->
    MTime = file_header_mtime_to_datetime(CDFH),
    print_header(CompSize, MTime, UncompSize, FileName, FileComment),
    Acc;
raw_long_print_info_etc(EOCD, _, Comment, _, Acc) when is_record(EOCD, eocd) ->
    print_comment(Comment),
    Acc.

print_header(CompSize, MTime, UncompSize, FileName, FileComment) ->
    io:format("~10w ~s ~10w ~3w% ~ts ~ts\n",
	      [CompSize, time_to_string(MTime), UncompSize,
	       get_percent(CompSize, UncompSize), FileName, FileComment]).

print_comment("") ->
    ok;
print_comment(Comment) ->
    io:format("Archive comment: ~ts\n", [Comment]).

get_percent(_, 0) -> 100;
get_percent(CompSize, Size) -> round(CompSize * 100 / Size).

%% time formatting ("borrowed" from erl_tar.erl)
time_to_string({{Y, Mon, Day}, {H, Min, _}}) ->
    io_lib:format("~s ~2w ~s:~s ~w",
		  [month(Mon), Day, two_d(H), two_d(Min), Y]).

two_d(N) ->
    tl(integer_to_list(N + 100)).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% zip header functions
cd_file_header_from_lh_pos_and_extra(LH, Pos, Extra) ->
    #local_file_header{version_needed = LHVersionNeeded,
		       gp_flag = GPFlag,
		       comp_method = CompMethod,
		       last_mod_time = LastModTime,
		       last_mod_date = LastModDate,
		       crc32 = CRC32,
		       comp_size = CompSize,
		       uncomp_size = UncompSize,
		       file_name_length = FileNameLength,
		       extra_field_length = _ExtraFieldLength,
                       info = #file_info{ type = Type, mode = Mode }} = LH,

    VersionNeeded =
        if Pos >= ?MAX_INT32 ->
                ?VERSION_NEEDED_ZIP64;
           true ->
                LHVersionNeeded
        end,

    #cd_file_header{os_made_by = ?OS_MADE_BY_UNIX,
                    version_made_by = ?VERSION_MADE_BY,
		    version_needed = VersionNeeded,
		    gp_flag = GPFlag,
		    comp_method = CompMethod,
		    last_mod_time = LastModTime,
		    last_mod_date = LastModDate,
		    crc32 = CRC32,
		    comp_size =
                        if UncompSize >= ?MAX_INT32 ->
                                ?MAX_INT32;
                           true ->
                                CompSize
                        end,
		    uncomp_size = min(UncompSize, ?MAX_INT32),
		    file_name_length = FileNameLength,
		    extra_field_length = iolist_size(Extra),
		    file_comment_length = 0, % FileCommentLength,
		    disk_num_start = 0, % DiskNumStart,
		    internal_attr = 0, % InternalAttr,
		    external_attr = % ExternalAttr
                        if Mode =:= undefined ->
                                case Type of
                                    regular -> ?DEFAULT_REGULAR_FILE_MODE;
                                    directory -> ?DEFAULT_DIRECTORY_FILE_MODE
                                end;
                           true -> Mode band 8#777
                        end bsl 16,
		    local_header_offset = min(Pos, ?MAX_INT32)}.

cd_file_header_to_bin(
  #cd_file_header{os_made_by = OsMadeBy,
                  version_made_by = VersionMadeBy,
		  version_needed = VersionNeeded,
		  gp_flag = GPFlag,
		  comp_method = CompMethod,
		  last_mod_time = LastModTime,
		  last_mod_date = LastModDate,
		  crc32 = CRC32,
		  comp_size = CompSize,
		  uncomp_size = UncompSize,
		  file_name_length = FileNameLength,
		  extra_field_length = ExtraFieldLength,
		  file_comment_length = FileCommentLength,
		  disk_num_start = DiskNumStart,
		  internal_attr = InternalAttr,
		  external_attr = ExternalAttr,
		  local_header_offset = LocalHeaderOffset}) ->
    <<VersionMadeBy:8,OsMadeBy:8,
     VersionNeeded:16/little,
     GPFlag:16/little,
     CompMethod:16/little,
     LastModTime:16/little,
     LastModDate:16/little,
     CRC32:32/little,
     CompSize:32/little,
     UncompSize:32/little,
     FileNameLength:16/little,
     ExtraFieldLength:16/little,
     FileCommentLength:16/little,
     DiskNumStart:16/little,
     InternalAttr:16/little,
     ExternalAttr:32/little,
     LocalHeaderOffset:32/little>>.

local_file_header_to_bin(
  #local_file_header{version_needed = VersionNeeded,
		     gp_flag = GPFlag,
		     comp_method = CompMethod,
		     last_mod_time = LastModTime,
		     last_mod_date = LastModDate,
		     crc32 = CRC32,
		     comp_size = CompSize,
		     uncomp_size = UncompSize,
		     file_name_length = FileNameLength,
		     extra_field_length = ExtraFieldLength}) ->
    <<VersionNeeded:16/little,
     GPFlag:16/little,
     CompMethod:16/little,
     LastModTime:16/little,
     LastModDate:16/little,
     CRC32:32/little,
     CompSize:32/little,
     UncompSize:32/little,
     FileNameLength:16/little,
     ExtraFieldLength:16/little>>.

eocd_to_bin(#eocd{disk_num = DiskNum,
	   start_disk_num = StartDiskNum,
	   entries_on_disk = EntriesOnDisk,
	   entries = Entries,
	   size = Size,
	   offset = Offset,
	   zip_comment_length = ZipCommentLength}) ->
    <<DiskNum:16/little,
      StartDiskNum:16/little,
      EntriesOnDisk:16/little,
      Entries:16/little,
      Size:32/little,
      Offset:32/little,
      ZipCommentLength:16/little>>.

eocd64_to_bin(
  #eocd{os_made_by = OsMadeBy,
        version_made_by = VersionMadeBy,
        extract_version = ExtractVersion,
        disk_num = DiskNum,
        start_disk_num = StartDiskNum,
        entries_on_disk = EntriesOnDisk,
        entries = Entries,
        size = Size,
        offset = Offset,
        extra = Extra}) ->
    <<VersionMadeBy:8,OsMadeBy:8,
      ExtractVersion:16/little,
      DiskNum:32/little,
      StartDiskNum:32/little,
      EntriesOnDisk:64/little,
      Entries:64/little,
      Size:64/little,
      Offset:64/little,
      Extra/binary>>.

%% put together a local file header
local_file_header_from_info_method_name(#file_info{mtime = MTime,
                                                   atime = ATime,
                                                   uid = Uid,
                                                   gid = Gid} = Info,
					UncompSize, CompMethod,
                                        Name, GPFlag, Extra ) ->
    CreationTime = os:system_time(second),
    {ModDate, ModTime} = dos_date_time_from_datetime(
                           calendar:system_time_to_local_time(
                             datetime_to_system_time(MTime), second)),
    VersionNeeded = if UncompSize >= ?MAX_INT32 ->
                            ?VERSION_NEEDED_ZIP64;
                       true ->
                            case CompMethod of
                                ?STORED -> ?VERSION_NEEDED_STORE;
                                ?DEFLATED -> ?VERSION_NEEDED_DEFLATE
                            end
                    end,
    #local_file_header{version_needed = VersionNeeded,
		       gp_flag = GPFlag,
		       comp_method = CompMethod,
		       last_mod_time = ModTime,
		       last_mod_date = ModDate,
                       mtime = datetime_to_system_time(MTime),
                       atime = datetime_to_system_time(ATime),
                       ctime = datetime_to_system_time(CreationTime),
                       uid = Uid,
                       gid = Gid,
		       crc32 = -1,
		       comp_size = ?MAX_INT32,
		       uncomp_size = min(UncompSize, ?MAX_INT32),
		       file_name_length = length(Name),
		       extra_field_length = iolist_size(Extra),
                       info = Info}.

%%
%% Functions used by zip server to work with archives.
%%
openzip_open(F, Options) ->
    case ?CATCH(do_openzip_open(F, Options)) of
	{ok, OpenZip} ->
	    {ok, OpenZip};
	Error ->
	    {error, Error}
    end.

do_openzip_open(F, Options) ->
    Opts = get_openzip_options(Options),
    #openzip_opts{output = Output, open_opts = OpO, cwd = CWD,
                  skip_dirs = SkipDirs, extra = ExtraOpts} = Opts,
    Input = get_input(F),
    In0 = Input({open, F, OpO -- [write]}, []),
    {[#zip_comment{comment = C} | Files], In1} =
	get_central_dir(In0, fun raw_file_info_etc/5, Input, ExtraOpts),
    Z = zlib:open(),
    {ok, #openzip{zip_comment = C,
		  files = Files,
		  in = In1,
		  input = Input,
		  output = Output,
		  zlib = Z,
		  cwd = CWD,
                  skip_dirs = SkipDirs,
                  extra = ExtraOpts}}.

%% retrieve all files from an open archive
openzip_get(OpenZip) ->
    case ?CATCH(do_openzip_get(OpenZip)) of
	{ok, Result} -> {ok, Result};
	Error -> {error, Error}
    end.

do_openzip_get(#openzip{files = Files, in = In0, input = Input,
			output = Output, zlib = Z, cwd = CWD, skip_dirs = SkipDirs,
                        extra = ExtraOpts}) ->
    ZipOpts = #unzip_opts{output = Output, input = Input,
			  file_filter = fun all/1, open_opts = [],
			  feedback = fun silent/1, cwd = CWD, skip_dirs = SkipDirs,
                          extra = ExtraOpts},
    R = get_z_files(Files, Z, In0, ZipOpts, []),
    {ok, R};
do_openzip_get(_) ->
    throw(einval).

%% retrieve the crc32 checksum from an open archive
openzip_get_crc32(FileName, #openzip{files = Files}) ->
    case file_name_search(FileName, Files) of
	{_,#zip_file_extra{crc32=CRC}} -> {ok, CRC};
	_ -> throw(file_not_found)
    end.

%% retrieve a file from an open archive
openzip_get(FileName, OpenZip) ->
    case ?CATCH(do_openzip_get(FileName, OpenZip)) of
	{ok, Result} -> {ok, Result};
	Error -> {error, Error}
    end.

do_openzip_get(F, #openzip{files = Files, in = In0, input = Input,
			   output = Output, zlib = Z, cwd = CWD, extra = ExtraOpts}) ->
    %%case lists:keysearch(F, #zip_file.name, Files) of
    case file_name_search(F, Files) of
	{#zip_file{offset = Offset},_}=ZFile ->
	    In1 = Input({seek, bof, Offset}, In0),
	    case get_z_file(In1, Z, Input, Output, [], fun silent/1,
			    CWD, ZFile, fun all/1, false, ExtraOpts) of
		{file, R, _In2} -> {ok, R};
		_ -> throw(file_not_found)
	    end;
	_ -> throw(file_not_found)
    end;
do_openzip_get(_, _) ->
    throw(einval).

file_name_search(Name,Files) ->
    Fun = fun({ZipFile,_}) ->
                  not string:equal(ZipFile#zip_file.name, Name,
                                   _IgnoreCase = false, _Norm = nfc)
          end,
    case lists:dropwhile(Fun, Files) of
	[ZFile|_] -> ZFile;
	[] -> false
    end.

%% get file list from open archive
openzip_list_dir(#openzip{zip_comment = Comment,
			  files = Files}) ->
    {ZipFiles,_Extras} = lists:unzip(Files),
    {ok, [#zip_comment{comment = Comment} | ZipFiles]};
openzip_list_dir(_) ->
    {error, einval}.

%% close an open archive
openzip_close(#openzip{in = In0, input = Input, zlib = Z}) ->
    Input(close, In0),
    zlib:close(Z);
openzip_close(_) ->
    {error, einval}.

%% small, simple, stupid zip-archive server
server_init(Parent) ->
    %% we want to know if our parent dies
    process_flag(trap_exit, true),
    server_loop(Parent, not_open).

server_loop(Parent, OpenZip) ->
    receive
	{From, {open, Archive, Options}} ->
	    case openzip_open(Archive, Options) of
		{ok, NewOpenZip} ->
		    From ! {self(), {ok, self()}},
		    server_loop(Parent, NewOpenZip);
		Error ->
		    From ! {self(), Error}
	    end;
	{From, close} ->
	    From ! {self(), openzip_close(OpenZip)};
	{From, get} ->
	    From ! {self(), openzip_get(OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, {get, FileName}} ->
	    From ! {self(), openzip_get(FileName, OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, {get_crc32, FileName}} ->
	    From ! {self(), openzip_get_crc32(FileName, OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, list_dir} ->
	    From ! {self(), openzip_list_dir(OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, get_state} ->
	    From ! {self(), OpenZip},
	    server_loop(Parent, OpenZip);
        {'EXIT', Parent, Reason} ->
            _ = openzip_close(OpenZip),
            exit({parent_died, Reason});
	_ ->
	    {error, bad_msg}
    end.

openzip_tt(#openzip{zip_comment = ZipComment, files = Files}) ->
    print_comment(ZipComment),
    lists_foreach(fun({#zip_file{comp_size = CompSize,
				name = FileName,
				comment = FileComment,
				info = FI},_}) ->
			  #file_info{size = UncompSize, mtime = MTime} = FI,
			  print_header(CompSize, MTime, UncompSize,
				       FileName, FileComment)
		  end, Files),
    ok.

openzip_t(#openzip{zip_comment = ZipComment, files = Files}) ->
    print_comment(ZipComment),
    lists_foreach(fun({#zip_file{name = FileName},_}) ->
			  print_file_name(FileName)
		  end, Files),
    ok.

lists_foreach(_, []) ->
    ok;
lists_foreach(F, [Hd|Tl]) ->
    F(Hd),
    lists_foreach(F, Tl).

%% option utils
get_openzip_opt([], Opts) ->
    Opts;
get_openzip_opt([cooked | Rest], #openzip_opts{open_opts = OO} = Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{open_opts = OO -- [raw]});
get_openzip_opt([memory | Rest], Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{output = fun binary_io/2});
get_openzip_opt([{cwd, CWD} | Rest], Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{cwd = CWD});
get_openzip_opt([skip_directories | Rest], Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{skip_dirs = true});
get_openzip_opt([{extra, What} = O| Rest], Opts) when is_list(What) ->
    case lists:all(fun(E) -> lists:member(E, ?EXTRA_OPTIONS) end, What) of
        true ->
            get_zip_opt(Rest, Opts#openzip_opts{extra = What});
        false ->
            throw({bad_option, O})
    end;
get_openzip_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

%% get the central directory from the archive
get_central_dir(In0, RawIterator, Input, ExtraOpts) ->
    {Size, In1} = Input({position, eof, 0}, In0),
    {{EOCD, BComment}, In2} =
        get_end_of_central_dir(
          In1, ?END_OF_CENTRAL_DIR_SZ,
          min(16#ffff + ?END_OF_CENTRAL_DIR_SZ + ?END_OF_CENTRAL_DIR_64_LOCATOR_SZ, Size),
          Input),
    EOCD#eocd.disk_num == 0 orelse throw(multiple_disks_not_supported),
    In3 = Input({seek, bof, EOCD#eocd.offset}, In2),
    N = EOCD#eocd.entries,
    Acc0 = [],
    %% There is no encoding flag for the archive comment.
    Comment = heuristic_to_string(BComment),
    Out0 = RawIterator(EOCD, "", Comment, <<>>, Acc0),
    get_cd_loop(N, In3, RawIterator, Input, ExtraOpts, Out0).

get_cd_loop(0, In, _RawIterator, _Input, _ExtraOpts, Acc) ->
    {lists:reverse(Acc), In};
get_cd_loop(N, In0, RawIterator, Input, ExtraOpts, Acc0) ->
    {BCD, In1} = case Input({read, ?CENTRAL_FILE_HEADER_SZ}, In0) of
	      {<<?CENTRAL_FILE_MAGIC:32/little, XBCD/binary>>, In} -> {XBCD, In};
	      _ -> throw(bad_central_directory)
	  end,
    CD = cd_file_header_from_bin(BCD),
    FileNameLen = CD#cd_file_header.file_name_length,
    ExtraLen = CD#cd_file_header.extra_field_length,
    CommentLen = CD#cd_file_header.file_comment_length,
    ToRead = FileNameLen + ExtraLen + CommentLen,
    GPFlag = CD#cd_file_header.gp_flag,
    {B2, In2} = Input({read, ToRead}, In1),
    {FileName, BExtra, Comment} =
	get_filename_extra_comment(B2, FileNameLen, ExtraLen, CommentLen, GPFlag),

    ExtraCD =
        update_extra_fields(CD, BExtra, ExtraOpts),

    Acc1 = RawIterator(ExtraCD, FileName, Comment, BExtra, Acc0),
    get_cd_loop(N-1, In2, RawIterator, Input, ExtraOpts, Acc1).

%% We parse and apply some extra fields defined by Info-ZIP. For details see:
%% proginfo/extrafld.txt in unzip. https://fossies.org/linux/unzip/proginfo/extrafld.txt
-spec update_extra_fields(#local_file_header{} | #cd_file_header{}, binary(), extra()) ->
          #local_file_header{} | #cd_file_header{}.
update_extra_fields(FileHeader, BExtra, ExtraOpts) ->
    %% We depend on some fields in the records to be at the same position
    #local_file_header.comp_size = #cd_file_header.comp_size,
    #local_file_header.uncomp_size = #cd_file_header.uncomp_size,
    #local_file_header.mtime = #cd_file_header.mtime,
    #local_file_header.atime = #cd_file_header.atime,
    #local_file_header.ctime = #cd_file_header.ctime,
    #local_file_header.uid = #cd_file_header.uid,
    #local_file_header.gid = #cd_file_header.gid,

    ExtendedTimestamp = lists:member(extended_timestamp, ExtraOpts),
    UidGid = lists:member(uid_gid, ExtraOpts),

    lists:foldl(
      fun({?X0001_ZIP64, Data}, Acc) ->
              update_zip64(Acc, Data);
         ({?X5455_EXTENDED_TIMESTAMP, Data}, Acc) when ExtendedTimestamp ->
              update_extended_timestamp(Acc, Data);
         ({?X7875_UNIX3, Data}, Acc) when UidGid ->
              update_unix3(Acc, Data);
         (_, Acc) ->
              Acc
      end, FileHeader, parse_extra(BExtra)).

update_zip64(FH, <<UnComp:64/little, Rest/binary>>) when element(#cd_file_header.uncomp_size, FH) == ?MAX_INT32 ->
    update_zip64(setelement(#cd_file_header.uncomp_size, FH, UnComp), Rest);
update_zip64(FH, <<Comp:64/little, Rest/binary>>) when element(#cd_file_header.comp_size, FH) == ?MAX_INT32 ->
    update_zip64(setelement(#cd_file_header.comp_size, FH, Comp), Rest);
update_zip64(FH, <<LocalHeaderOffset:64/little, Rest/binary>>) when element(#cd_file_header.local_header_offset, FH) == ?MAX_INT32 ->
    update_zip64(setelement(#cd_file_header.local_header_offset, FH, LocalHeaderOffset), Rest);
update_zip64(FH, <<DiskNumStart:32/little, Rest/binary>>) when element(#cd_file_header.disk_num_start, FH) == ?MAX_INT32 ->
    update_zip64(setelement(#cd_file_header.disk_num_start, FH, DiskNumStart), Rest);
update_zip64(FH, _) ->
    %% Some zip files (I'm looking at you Excel) places data here even if it is not used...
    %%  so we have to skip any non-used data instead of mathing on <<>>
    FH.

update_extended_timestamp(FileHeader, <<_:5,HasCre:1,HasAcc:1,HasMod:1,Data/binary>> ) ->
    {FHMod, DataMod} = update_extended_timestamp(FileHeader, HasMod, Data, #cd_file_header.mtime),
    {FHAcc, DataAcc} = update_extended_timestamp(FHMod, HasAcc, DataMod, #cd_file_header.atime),
    {FHCre, <<>>} = update_extended_timestamp(FHAcc, HasCre, DataAcc, #cd_file_header.ctime),
    FHCre.

update_extended_timestamp(FH, 1, <<Value:32/little,Rest/binary>>, Field) ->
    {setelement(Field, FH, Value), Rest};
%% It seems like sometimes bits are set, but the data does not include any payload
update_extended_timestamp(FH, 1, <<>>, _Field) ->
    {FH, <<>>};
update_extended_timestamp(FH, 0, Data, _Field) ->
    {FH, Data}.

update_unix3(FH, <<1, UidSize, Uid:(UidSize*8)/little, GidSize, Gid:(GidSize*8)/little>>) ->
    setelement(#cd_file_header.gid, setelement(#cd_file_header.uid, FH, Uid), Gid);
update_unix3(FH, <<Vsn,_/binary>>) when Vsn =/= 1 ->
    FH.

parse_extra(<<Tag:16/little,Sz:16/little,Data:Sz/binary,Rest/binary>>) ->
    [{Tag, Data} | parse_extra(Rest)];
parse_extra(<<>>) ->
    [].

get_filename_extra_comment(B, FileNameLen, ExtraLen, CommentLen, GPFlag) ->
    try
        <<BFileName:FileNameLen/binary,
          BExtra:ExtraLen/binary,
          BComment:CommentLen/binary>> = B,
        {binary_to_chars(BFileName, GPFlag),
         BExtra,
         %% Appendix D says: "If general purpose bit 11 is unset, the
         %% file name and comment should conform to the original ZIP
         %% character encoding." However, it seems that at least Linux
         %% zip(1) encodes the comment without setting bit 11 if the
         %% filename is 7-bit ASCII. If bit 11 is set,
         %% binary_to_chars/1 could (should?) be called (it can fail),
         %% but the choice is to employ heuristics in this case too
         %% (it does not fail).
         heuristic_to_string(BComment)}
    catch
        _:_ ->
            throw(bad_central_directory)
    end.

%% get end record, containing the offset to the central directory
%% the end record is always at the end of the file BUT alas it is
%% of variable size (yes that's dumb!)
get_end_of_central_dir(In0, Sz, MaxCentralDirSize, Input) ->
    In1 = Input({seek, eof, -Sz}, In0),
    {B, In2} = Input({read, Sz}, In1),
    case find_eocd(B) of
        none when Sz =:= MaxCentralDirSize ->
            throw(bad_eocd);
	none ->
	    get_end_of_central_dir(In2, min(Sz+Sz, MaxCentralDirSize), MaxCentralDirSize, Input);
        {EOCD64Location, EOCD, Comment} ->
            case find_eocd64(In2, EOCD64Location, EOCD, Comment, Input) of
                none ->
                    throw(bad_eocd64);
                {EOCD64, In3} ->
                    {EOCD64, In3}
            end;
	Header ->
	    {Header, In2}
    end.

%% find the end record by matching for it
%% The ?END_OF_CENTRAL_DIR_MAGIC could be in the comment,
%% so we need to match for the entire structure and make sure
%% the comment size consumes all of the binary.
find_eocd(<<?END_OF_CENTRAL_DIR_64_LOCATOR_MAGIC:32/little,
            EOCD64StartDiskNum:32/little,
            EOCD64Offset:64/little,
            EOCD64TotalDisk:32/little,
            ?END_OF_CENTRAL_DIR_MAGIC:32/little,
            DiskNum:16/little,
            StartDiskNum:16/little,
            EntriesOnDisk:16/little,
            Entries:16/little,
            Size:32/little,
            Offset:32/little,
            ZipCommentLength:16/little,
            Comment:ZipCommentLength/binary>>) ->
    if DiskNum =:= ?MAX_INT16;
       StartDiskNum =:= ?MAX_INT16;
       EntriesOnDisk =:= ?MAX_INT16,
       Entries =:= ?MAX_INT16;
       Size =:= ?MAX_INT32;
       Offset =:= ?MAX_INT32 ->
            {{EOCD64StartDiskNum, EOCD64Offset, EOCD64TotalDisk},
             #eocd{disk_num = DiskNum,
                   start_disk_num = StartDiskNum,
                   entries_on_disk = EntriesOnDisk,
                   entries = Entries,
                   size = Size,
                   offset = Offset,
                   zip_comment_length = ZipCommentLength},
             Comment};
       true ->
            none
    end;
find_eocd(<<?END_OF_CENTRAL_DIR_MAGIC:32/little,
            DiskNum:16/little,
            StartDiskNum:16/little,
            EntriesOnDisk:16/little,
            Entries:16/little,
            Size:32/little,
            Offset:32/little,
            ZipCommentLength:16/little,
            Comment:ZipCommentLength/binary>>) ->
    if DiskNum =:= ?MAX_INT16;
       StartDiskNum =:= ?MAX_INT16;
       EntriesOnDisk =:= ?MAX_INT16;
       Entries =:= ?MAX_INT16;
       Size =:= ?MAX_INT32;
       Offset =:= ?MAX_INT32 ->
            %% There should be a eocd64 locator before this entry
            none;
       true ->
            {#eocd{disk_num = DiskNum,
                   start_disk_num = StartDiskNum,
                   entries_on_disk = EntriesOnDisk,
                   entries = Entries,
                   size = Size,
                   offset = Offset,
                   zip_comment_length = ZipCommentLength},
             Comment}
       end;
find_eocd(<<_:8, Rest/binary>>) when byte_size(Rest) > ?END_OF_CENTRAL_DIR_SZ-4 ->
    find_eocd(Rest);
find_eocd(_) ->
    none.

find_eocd64(In0,{_EOCD64StartDiskNum, EOCD64Offset, _EOCD64TotalDisk}, EOCD, Comment, Input) ->
    maybe
        In1 = Input({seek, bof, EOCD64Offset}, In0),

        {<<?END_OF_CENTRAL_DIR_64_MAGIC:32/little,
           EOCDSize:64/little>>, In2}
            ?= Input({read, 4 + 8}, In1),

        {<<VersionMadeBy:8,OsMadeBy:8,
           ExtractVersion:16/little,
           DiskNum:32/little,
           StartDiskNum:32/little,
           EntriesOnDisk:64/little,
           Entries:64/little,
           Size:64/little,
           Offset:64/little,
           Extra:(EOCDSize-?END_OF_CENTRAL_DIR_64_SZ)/binary>>, In3}
            ?= Input({read, EOCDSize}, In2),

        {{EOCD#eocd{
            eocd = EOCD,
            version_made_by = VersionMadeBy,
            os_made_by = os_id_to_atom(OsMadeBy),
            extract_version = ExtractVersion,
            disk_num = DiskNum,
            start_disk_num = StartDiskNum,
            entries_on_disk = EntriesOnDisk,
            entries = Entries,
            size = Size,
            offset = Offset,
            extra = parse_extra(Extra)}, Comment}, In3}
    else
        {eof, InEOF} ->
            {eof, InEOF};
        _ ->
            none
    end.


%% Taken from APPNOTE.TXT version 6.3.10 section 4.4.2.2
os_id_to_atom(0) -> ~"MS-DOS and OS/2";
os_id_to_atom(1) -> ~"Amiga";
os_id_to_atom(2) -> ~"OpenVMS";
os_id_to_atom(3) -> ~"UNIX";
os_id_to_atom(4) -> ~"VM/CMS";
os_id_to_atom(5) -> ~"Atari ST";
os_id_to_atom(6) -> ~"OS/2 H.P.F.S";
os_id_to_atom(7) -> ~"Macintosh";
os_id_to_atom(8) -> ~"Z-System";
os_id_to_atom(9) -> ~"CP/M";
os_id_to_atom(10) -> ~"Windows NTFS";
os_id_to_atom(11) -> ~"MVS";
os_id_to_atom(12) -> ~"VSE";
os_id_to_atom(13) -> ~"Acorn Risc";
os_id_to_atom(14) -> ~"VFAT";
os_id_to_atom(15) -> ~"alternate MVS";
os_id_to_atom(16) -> ~"BeOS";
os_id_to_atom(17) -> ~"Tandem";
os_id_to_atom(18) -> ~"OS/400";
os_id_to_atom(19) -> ~"OS X (Darwin)";
os_id_to_atom(No) -> No.

%% from a central directory record, filter and accumulate what we need

%% with zip_file_extra
raw_file_info_etc(CD, FileName, FileComment, BExtraField, Acc)
  when is_record(CD, cd_file_header) ->
    #cd_file_header{comp_size = CompSize,
		    local_header_offset = Offset,
		    crc32 = CRC} = CD,
    FileInfo = cd_file_header_to_file_info(FileName, CD, BExtraField),
    [{#zip_file{name = FileName, info = FileInfo, comment = FileComment,
		offset = Offset, comp_size = CompSize}, #zip_file_extra{crc32 = CRC}} | Acc];
raw_file_info_etc(EOCD, _, Comment, _, Acc) when is_record(EOCD, eocd) ->
    [#zip_comment{comment = Comment} | Acc].

%% without zip_file_extra
raw_file_info_public(CD, FileName, FileComment, BExtraField, Acc0) ->
    [H1|T] = raw_file_info_etc(CD,FileName,FileComment,BExtraField,Acc0),
    H2 = case H1 of
	     {ZF,Extra} when is_record(Extra,zip_file_extra) -> ZF;
	     Other -> Other
	 end,
    [H2|T].


%% make a file_info from a central directory header
cd_file_header_to_file_info(FileName,
			    #cd_file_header{uncomp_size = UncompSize} = CDFH,
			    _ExtraField) ->
    M = file_header_mtime_to_datetime(CDFH),
    A = file_header_atime_to_datetime(CDFH),
    C = file_header_ctime_to_datetime(CDFH),
    Type =
	case lists:last(FileName) of
	    $/ -> directory;
	    _  -> regular
	end,
    Mode =
        if CDFH#cd_file_header.os_made_by =:= ~"UNIX" ->
                (CDFH#cd_file_header.external_attr bsr 16) band 8#777;
           true ->
                if Type =:= directory ->
                        ?DEFAULT_DIRECTORY_FILE_MODE;
                   true ->
                        ?DEFAULT_REGULAR_FILE_MODE
                end
        end,
    #file_info{size = UncompSize,
               type = Type,
               access = read_write,
               atime = A,
               mtime = M,
               ctime = C,
               mode = Mode,
               links = 1,
               major_device = 0,
               minor_device = 0,
               inode = 0,
               uid = CDFH#cd_file_header.uid,
               gid = CDFH#cd_file_header.gid}.

%% get all files using file list
%% (the offset list is already filtered on which file to get... isn't it?)
get_z_files([], _Z, _In, #unzip_opts{ output = Output }, Acc) ->
    flush_and_reverse(Output, Acc, []);
get_z_files([#zip_comment{comment = _} | Rest], Z, In, Opts, Acc) ->
    get_z_files(Rest, Z, In, Opts, Acc);
get_z_files([{#zip_file{offset = Offset} = ZipFile, ZipExtra} | Rest], Z, In0,
	    #unzip_opts{input = Input, output = Output, open_opts = OpO,
			file_filter = Filter, feedback = FB,
			cwd = CWD, skip_dirs = SkipDirs, extra = ExtraOpts} = Opts, Acc0) ->
    case Filter({ZipFile, ZipExtra, CWD}) of
	true ->
	    In1 = Input({seek, bof, Offset}, In0),
	    {In2, Acc1} =
		case get_z_file(In1, Z, Input, Output, OpO, FB,
				CWD, {ZipFile, ZipExtra}, Filter, SkipDirs, ExtraOpts) of
		    {Type, GZD, Inx} when Type =:= file; Type =:= dir ->
                        {Inx, [GZD | Acc0]};
		    {_, Inx}       -> {Inx, Acc0}
		end,
	    get_z_files(Rest, Z, In2, Opts, Acc1);
	_ ->
	    get_z_files(Rest, Z, In0, Opts, Acc0)
    end.

flush_and_reverse(Output, [H|T], Acc) ->
    flush_and_reverse(Output, T, [Output(flush, H) | Acc]);
flush_and_reverse(_Output, [], Acc) ->
    Acc.

%% get a file from the archive, reading chunks
get_z_file(In0, Z, Input, Output, OpO, FB,
	   CWD, {ZipFile,ZipExtra}, Filter, SkipDirs, ExtraOpts) ->
    case Input({read, ?LOCAL_FILE_HEADER_SZ}, In0) of
	{eof, In1} ->
	    {eof, In1};
	%% Local File Header
	{<<?LOCAL_FILE_MAGIC:32/little, B/binary>>, In1} ->
	    LH = local_file_header_from_bin(B),
	    #local_file_header{gp_flag = GPFlag,
			       comp_method = CompMethod,
			       file_name_length = FileNameLen,
			       extra_field_length = ExtraLen} = LH,

	    {BFileN, In3} = Input({read, FileNameLen + ExtraLen}, In1),
	    {FileName, BLHExtra} =
                get_filename_extra(FileNameLen, ExtraLen, BFileN, GPFlag),
            LHExtra =
                update_extra_fields(LH, BLHExtra, ExtraOpts),

	    {CompSize,CRC32} = case GPFlag band 8 =:= 8 of
				   true -> {ZipFile#zip_file.comp_size,
					    ZipExtra#zip_file_extra.crc32};
				   false -> {LHExtra#local_file_header.comp_size,
					     LHExtra#local_file_header.crc32}
			       end,

	    ReadAndWrite =
		case check_valid_location(CWD, FileName) of
		    {true,FileName1} ->
			true;
		    {false,FileName1} ->
			Filter({ZipFile#zip_file{name = FileName1},ZipExtra, CWD})
		end,

            FileNameWithCwd = add_cwd(CWD, FileName1),

            IsDir = lists:last(FileName) =:= $/,

            case ReadAndWrite andalso not (IsDir andalso SkipDirs) of
                true ->
                    {Type, Out, In} =
                        case lists:last(FileName) of
                            $/ ->
                                Out1 = Output({ensure_path,FileNameWithCwd},[]),
                                {dir, Out1, In3};
                            _ ->
                                {Out1, In4, CRC, _UncompSize} =
                                    get_z_data(CompMethod, In3, FileNameWithCwd,
                                               CompSize, Input, Output, OpO, Z),
                                In5 = skip_z_data_descriptor(GPFlag, Input, In4),

                                FB(FileName),
                                CRC =:= CRC32 orelse throw({bad_crc, FileName}),
                                {file, Out1, In5}
                        end,

                    FileInfo = local_file_header_to_file_info(
                                 Output({file_info, FileNameWithCwd}, Out),
                                 LHExtra, ZipFile),

                    SetFileInfo =
                        fun(O) -> Output({set_file_info, FileNameWithCwd, FileInfo, [{time, local}]}, O) end,

                    Out2 =
                        if Type =:= dir ->
                                Output({delay, SetFileInfo}, Out);
                           Type =:= file ->
                                SetFileInfo(Out)
                        end,

                    {Type, Out2, In};
                false ->
                    {ignore, In3}
            end;
	Else ->
	    throw({bad_local_file_header, Else})
    end.

local_file_header_to_file_info(FI, LFH, ZipFile) ->
    %% Validate that local_file_header mtime is the same as cd_file_header
    FI#file_info{ mode = ZipFile#zip_file.info#file_info.mode,
                  mtime = file_header_mtime_to_datetime(LFH),
                  atime = file_header_atime_to_datetime(LFH),
                  ctime = file_header_ctime_to_datetime(LFH)
                }.


%% make sure FileName doesn't have relative path that points over CWD
check_valid_location(CWD, FileName) ->
    TrailingSlash = case lists:last(FileName) of
                        $/ -> "/";
                        _ -> ""
                    end,
    %% check for directory traversal exploit
    {IsValid, Name} =
        case check_dir_level(filename:split(FileName), 0) of
            {FileOrDir,Level} when Level < 0 ->
                CWD1 = if CWD == "" -> "./";
                          true      -> CWD
                       end,
                error_logger:format("Illegal path: ~ts, extracting in ~ts~n",
                                    [add_cwd(CWD,FileName),CWD1]),
                {false, FileOrDir};
            _ ->
                {true, FileName}
        end,
    {IsValid, string:trim(Name, trailing, "/") ++ TrailingSlash}.

check_dir_level([FileOrDir], Level) ->
    {FileOrDir,Level};
check_dir_level(["." | Parts], Level) ->
    check_dir_level(Parts, Level);
check_dir_level([".." | Parts], Level) ->
    check_dir_level(Parts, Level-1);
check_dir_level([_Dir | Parts], Level) ->
    check_dir_level(Parts, Level+1).

get_filename_extra(FileNameLen, ExtraLen, B, GPFlag) ->
    try
        <<BFileName:FileNameLen/binary, BExtra:ExtraLen/binary>> = B,
        {binary_to_chars(BFileName, GPFlag), BExtra}
    catch
        _:_ ->
            throw(bad_file_header)
    end.

%% get compressed or stored data
get_z_data(?DEFLATED, In0, FileName, CompSize, Input, Output, OpO, Z) ->
    ok = zlib:inflateInit(Z, -?MAX_WBITS),
    Out0 = Output({open, FileName, [write | OpO]}, []),
    CRC0 = 0,
    {In1, Out1, UncompSize, CRC} = get_z_data_loop(CompSize, 0, In0, Out0, Input, Output, CRC0, Z),
    _ = ?CATCH(zlib:inflateEnd(Z)),
    Out2 = Output({close, FileName}, Out1),
    {Out2, In1, CRC, UncompSize};
get_z_data(?STORED, In0, FileName, CompSize, Input, Output, OpO, _Z) ->
    Out0 = Output({open, FileName, [write | OpO]}, []),
    CRC0 = 0,
    {In1, Out1, CRC} = copy_data_loop(CompSize, In0, Out0, Input, Output, CRC0),
    Out2 = Output({close, FileName}, Out1),
    {Out2, In1, CRC, CompSize};
get_z_data(_, _, _, _, _, _, _, _) ->
    throw(bad_file_header).

copy_data_loop(0, In, Out, _Input, _Output, CRC) ->
    {In, Out, CRC};
copy_data_loop(CompSize, In0, Out0, Input, Output, CRC0) ->
    N = erlang:min(?READ_BLOCK_SIZE, CompSize),
    case Input({read, N}, In0) of
        {eof, In1} ->
            {Out0, In1};
        {Uncompressed, In1} ->
            CRC1 = erlang:crc32(CRC0, Uncompressed),
            Out1 = Output({write, Uncompressed}, Out0),
            copy_data_loop(CompSize-N, In1, Out1, Input, Output, CRC1)
    end.

get_z_data_loop(0, UncompSize, In, Out, _Input, _Output, CRC0, _Z) ->
    {In, Out, UncompSize, CRC0};
get_z_data_loop(CompSize, UncompSize, In0, Out0, Input, Output, CRC0, Z) ->
    N = erlang:min(?READ_BLOCK_SIZE, CompSize),
    case Input({read, N}, In0) of
        {eof, In1} ->
            {Out0, In1};
        {Compressed, In1} ->
            Uncompressed = zlib:inflate(Z, Compressed),
            CRC1 = erlang:crc32(CRC0, Uncompressed),
            Out1 = Output({write, Uncompressed}, Out0),
            get_z_data_loop(CompSize-N, UncompSize + iolist_size(Uncompressed),
                In1, Out1, Input, Output, CRC1, Z)
    end.


%% skip data descriptor if any
skip_z_data_descriptor(GPFlag, Input, In0) when GPFlag band 8 =:= 8 ->
    Input({seek, cur, 12}, In0);
skip_z_data_descriptor(_GPFlag, _Input, In0) ->
    In0.

%% If we have mtime we use that, otherwise use dos time
file_header_mtime_to_datetime(FH) ->
    #cd_file_header.mtime = #local_file_header.mtime,
    case element(#cd_file_header.mtime, FH) of
        undefined ->
            dos_date_time_to_datetime(
              element(#cd_file_header.last_mod_date, FH),
              element(#cd_file_header.last_mod_time, FH));
        MTime ->
            calendar:system_time_to_local_time(MTime, second)
    end.

%% If we have atime we use that, otherwise use dos time
file_header_atime_to_datetime(FH) ->
    #cd_file_header.atime = #local_file_header.atime,
    case element(#cd_file_header.atime, FH) of
        undefined ->
            dos_date_time_to_datetime(
              element(#cd_file_header.last_mod_date, FH),
              element(#cd_file_header.last_mod_time, FH));
        Atime ->
            calendar:system_time_to_local_time(Atime, second)
    end.

%% Normally ctime will not be set, but if it is we use that. If it is not set
%% we return undefined so that when we later do write_file_info ctime will remain
%% the time that the file was created when extracted from the archive.
file_header_ctime_to_datetime(FH) ->
    #cd_file_header.ctime = #local_file_header.ctime,
    case element(#cd_file_header.ctime, FH) of
        undefined -> undefined;
        Ctime ->
            calendar:system_time_to_local_time(Ctime, second)
    end.

%% convert between erlang datetime and the MSDOS date and time
%% that's stored in the zip archive
%%    	 MSDOS Time  	           MSDOS Date
%% bit   0 - 4 	 5 - 10 11 - 15    16 - 20      21 - 24        25 - 31
%% value second  minute hour 	   day (1 - 31) month (1 - 12) years from 1980
dos_date_time_to_datetime(DosDate, DosTime) ->
    <<Hour:5, Min:6, DoubleSec:5>> = <<DosTime:16>>,
    <<YearFrom1980:7, Month:4, Day:5>> = <<DosDate:16>>,

    Datetime = {{YearFrom1980+1980, Month, Day},
                {Hour, Min, DoubleSec * 2}},
    if DoubleSec > 29 ->
            %% If DoubleSec * 2 > 59, something is broken
            %% with this archive, but unzip wraps the value
            %% so we do the same by converting to greg seconds
            %% and then back again.
            calendar:gregorian_seconds_to_datetime(
              calendar:datetime_to_gregorian_seconds(Datetime));
       true ->
            Datetime
    end.

dos_date_time_from_datetime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    YearFrom1980 = Year-1980,
    <<DosTime:16>> = <<Hour:5, Min:6, (Sec div 2):5>>,
    <<DosDate:16>> = <<YearFrom1980:7, Month:4, Day:5>>,
    {DosDate, DosTime}.

%% Convert a local datetime or universal time seconds to
%% system time (aka POSIX time, aka Unix time)
datetime_to_system_time(undefined) ->
    undefined;
datetime_to_system_time(PosixTime) when is_integer(PosixTime) ->
    PosixTime;
datetime_to_system_time(DateTime) ->
    erlang:universaltime_to_posixtime(
      erlang:localtime_to_universaltime(DateTime)).

%% A pwrite-like function for iolists (used by memory-option)

pwrite_binary(B, Pos, Bin) when byte_size(B) =:= Pos ->
    append_bins(Bin, B);
pwrite_binary(B, Pos, Bin) ->
    erlang:iolist_to_binary(pwrite_iolist(B, Pos, Bin)).

append_bins([Bin|Bins], B) when is_binary(Bin) ->
    append_bins(Bins, <<B/binary, Bin/binary>>);
append_bins([List|Bins], B) when is_list(List) ->
    append_bins(Bins, append_bins(List, B));
append_bins(Bin, B) when is_binary(Bin) ->
    <<B/binary, Bin/binary>>;
append_bins([_|_]=List, B) ->
    <<B/binary, (iolist_to_binary(List))/binary>>;
append_bins([], B) ->
    B.

-dialyzer({no_improper_lists, pwrite_iolist/3}).

pwrite_iolist(B, Pos, Bin) ->
    {Left, Right} = split_binary(B, Pos),
    Sz = erlang:iolist_size(Bin),
    R = skip_bin(Right, Sz),
    [Left, Bin | R].

skip_bin(B, Pos) when is_binary(B) ->
    case B of
	<<_:Pos/binary, Bin/binary>> -> Bin;
	_ -> <<>>
    end.

binary_to_chars(B, GPFlag) ->
    case GPFlag band ?GP_BIT_11 of
        0 ->
            binary_to_list(B);
        ?GP_BIT_11 ->
            case unicode:characters_to_list(B) of
                List when is_list(List) ->
                    List
            end
    end.

heuristic_to_string(B) when is_binary(B) ->
    case unicode:characters_to_binary(B) of
	B ->
            unicode:characters_to_list(B);
	_ ->
            binary_to_list(B)
    end.

encode_string(String) ->
    case lists:any(fun(C) -> C > 127 end, String) of
        true ->
            case unicode:characters_to_binary(String) of
                B when is_binary(B) ->
                    {binary_to_list(B), ?GP_BIT_11};
                _ ->
                    throw({bad_unicode, String})
            end;
        false ->
            {String, 0}
    end.

cd_file_header_from_bin(<<VersionMadeBy:8,OsMadeBy:8,
			 VersionNeeded:16/little,
			 GPFlag:16/little,
			 CompMethod:16/little,
			 LastModTime:16/little,
			 LastModDate:16/little,
			 CRC32:32/little,
			 CompSize:32/little,
			 UncompSize:32/little,
			 FileNameLength:16/little,
			 ExtraFieldLength:16/little,
			 FileCommentLength:16/little,
			 DiskNumStart:16/little,
			 InternalAttr:16/little,
			 ExternalAttr:32/little,
			 LocalHeaderOffset:32/little>>) ->
    #cd_file_header{version_made_by = VersionMadeBy,
                    os_made_by = os_id_to_atom(OsMadeBy),
		    version_needed = VersionNeeded,
		    gp_flag = GPFlag,
		    comp_method = CompMethod,
		    last_mod_time = LastModTime,
		    last_mod_date = LastModDate,
		    crc32 = CRC32,
		    comp_size = CompSize,
		    uncomp_size = UncompSize,
		    file_name_length = FileNameLength,
		    extra_field_length = ExtraFieldLength,
		    file_comment_length = FileCommentLength,
		    disk_num_start = DiskNumStart,
		    internal_attr = InternalAttr,
		    external_attr = ExternalAttr,
		    local_header_offset = LocalHeaderOffset};
cd_file_header_from_bin(_) ->
    throw(bad_cd_file_header).

local_file_header_from_bin(<<VersionNeeded:16/little,
			    GPFlag:16/little,
			    CompMethod:16/little,
			    LastModTime:16/little,
			    LastModDate:16/little,
			    CRC32:32/little,
			    CompSize:32/little,
			    UncompSize:32/little,
			    FileNameLength:16/little,
			    ExtraFieldLength:16/little>>) ->
    #local_file_header{version_needed = VersionNeeded,
		       gp_flag = GPFlag,
		       comp_method = CompMethod,
		       last_mod_time = LastModTime,
		       last_mod_date = LastModDate,
		       crc32 = CRC32,
		       comp_size = CompSize,
		       uncomp_size = UncompSize,
		       file_name_length = FileNameLength,
		       extra_field_length = ExtraFieldLength};
local_file_header_from_bin(_) ->
    throw(bad_local_file_header).

%% io functions
binary_io({file_info, FN, Opts}, A) ->
    FI = binary_io({file_info, FN}, A),
    case proplists:get_value(time, Opts, local) of
        local -> FI;
        posix -> FI#file_info{ atime = datetime_to_system_time(FI#file_info.atime),
                               mtime = datetime_to_system_time(FI#file_info.mtime),
                               ctime = datetime_to_system_time(FI#file_info.ctime) }
    end;
binary_io({file_info, {_Filename, _B, #file_info{} = FI}}, _A) ->
    FI;
binary_io({file_info, {_Filename, #file_info{} = FI, _B}}, _A) ->
    FI;
binary_io({file_info, {_Filename, B}}, A) ->
    binary_io({file_info, B}, A);
binary_io({file_info, Filename}, A) when is_list(Filename) ->
    binary_io({file_info, {Filename, <<>>}}, A);
binary_io({file_info, B}, _) ->
    {Type, Size} =
	if
	    is_binary(B) -> {regular, byte_size(B)};
	    B =:= directory -> {directory, 0}
	end,
    Now = calendar:local_time(),
    #file_info{size = Size, type = Type,
	       access = read_write, atime = Now,
	       mtime = Now, ctime = Now, mode =
                   if
                       Type =:= directory -> ?DEFAULT_DIRECTORY_FILE_MODE;
                       true -> ?DEFAULT_REGULAR_FILE_MODE
                   end,
	       links = 1, major_device = 0,
	       minor_device = 0, inode = 0,
	       uid = 0, gid = 0};
binary_io({open, {_Filename, B, _FI}, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, {_Filename, _FI, B}, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, {_Filename, B}, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, B, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, Filename, _Opts}, _) when is_list(Filename) ->
    {0, <<>>};
binary_io({read, N}, {Pos, B}) when Pos >= byte_size(B) ->
    {eof, {Pos+N, B}};
binary_io({read, N}, {Pos, B}) when Pos + N > byte_size(B) ->
    <<_:Pos/binary, Read/binary>> = B,
    {Read, {byte_size(B), B}};
binary_io({pread, Pos, N}, {OldPos, B}) ->
    case B of
	<<_:Pos/binary, Read:N/binary, _Rest/binary>> ->
	    {Read, {Pos+N, B}};
	_ ->
	    {eof, {OldPos, B}}
    end;
binary_io({read, N}, {Pos, B}) ->
    <<_:Pos/binary, Read:N/binary, _/binary>> = B,
    {Read, {Pos+N, B}};
binary_io({seek, bof, Pos}, {_OldPos, B}) ->
    {Pos, B};
binary_io({seek, cur, Pos}, {OldPos, B}) ->
    {OldPos + Pos, B};
binary_io({seek, eof, Pos}, {_OldPos, B}) ->
    {byte_size(B) + Pos, B};
binary_io({position, Loc, Adj}, File) ->
    {Pos, _} = NewFile = binary_io({seek, Loc, Adj}, File),
    {Pos, NewFile};
binary_io({pwrite, Pos, Data}, {OldPos, B}) ->
    {OldPos, pwrite_binary(B, Pos, Data)};
binary_io({write, Data}, {Pos, B}) ->
    {Pos + erlang:iolist_size(Data), pwrite_binary(B, Pos, Data)};
binary_io(close, {_Pos, B}) ->
    B;
binary_io({close, FN}, {_Pos, B}) ->
    {FN, B};
binary_io({list_dir, _F}, _B) ->
    [];
binary_io({set_file_info, _F, _FI}, B) ->
    B;
binary_io({set_file_info, _F, _FI, _O}, B) ->
    B;
binary_io({ensure_path, Dir}, _B) ->
    {Dir, <<>>};
binary_io({delay, Fun}, B) ->
    %% We don't delay things in binary_io
    Fun(B);
binary_io(flush, FN) ->
    FN.

file_io({file_info, F}, _) ->
    case file:read_file_info(F) of
	{ok, Info} -> Info;
	{error, E} -> throw(E)
    end;
file_io({file_info, F, Opts}, _) ->
    case file:read_file_info(F, Opts) of
	{ok, Info} -> Info;
	{error, E} -> throw(E)
    end;
file_io({open, FN, Opts}, _) ->
    case lists:member(write, Opts) of
	true -> ok = filelib:ensure_dir(FN);
	_ -> ok
    end,
    case file:open(FN, Opts++[binary]) of
	{ok, H} -> H;
	{error, E} -> throw(E)
    end;
file_io({read, N}, H) ->
    case file:read(H, N) of
	{ok, B} -> {B, H};
	eof -> {eof, H};
	{error, E} -> throw(E)
    end;
file_io({pread, Pos, N}, H) ->
    case file:pread(H, Pos, N) of
	{ok, B} -> {B, H};
	eof -> {eof, H};
	{error, E} -> throw(E)
    end;
file_io({seek, S, Pos}, H) ->
    case file:position(H, {S, Pos}) of
	{ok, _NewPos} -> H;
	{error, Error} -> throw(Error)
    end;
file_io({position, S, Pos}, H) ->
    case file:position(H, {S, Pos}) of
	{ok, NewPos} -> {NewPos, H};
	{error, Error} -> throw(Error)
    end;
file_io({write, Data}, H) ->
    case file:write(H, Data) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({pwrite, Pos, Data}, H) ->
    case file:pwrite(H, Pos, Data) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({close, FN}, H) ->
    case file:close(H) of
	ok -> #{ name => FN, flush => []};
	{error, Error} -> throw(Error)
    end;
file_io(close, H) ->
    file_io({close, ok}, H);
file_io({list_dir, F}, _H) ->
    case file:list_dir(F) of
	{ok, Files} -> Files;
	{error, Error} -> throw(Error)
    end;
file_io({set_file_info, F, FI}, H) ->
    case file:write_file_info(F, FI) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({set_file_info, F, FI, O}, H) ->
    case file:write_file_info(F, FI, O) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({ensure_path, Dir}, _H) ->
    ok = filelib:ensure_path(Dir),
    #{ name => Dir, flush => []};
file_io({delay, Fun}, #{flush := Flush} = H) ->
    H#{flush := [Fun | Flush] };
file_io(flush, #{ name := Name, flush := Flush }) ->
    _ = [F(Name) || F <- Flush],
    Name.
