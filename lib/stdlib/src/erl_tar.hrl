%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

%% Options used when adding files to a tar archive.
-record(add_opts, {
	 read_info,          %% Fun to use for read file/link info.
	 chunk_size = 0,     %% For file reading when sending to sftp. 0=do not chunk
         verbose = false,    %% Verbose on/off.
         atime = undefined,
         mtime = undefined,
         ctime = undefined,
         uid = 0,
         gid = 0}).
-type add_opts() :: #add_opts{}.

%% Options used when reading a tar archive.
-record(read_opts, {
          cwd                    :: string(),  %% Current working directory.
          keep_old_files = false :: boolean(), %% Owerwrite or not.
          files = all,                         %% Set of files to extract (or all)
          output = file :: 'file' | 'memory',
          open_mode = [],                      %% Open mode options.
          verbose = false :: boolean()}).      %% Verbose on/off.
-type read_opts() :: #read_opts{}.

-type add_opt() :: dereference |
                   verbose |
                   {chunks, pos_integer()} |
                   {atime, non_neg_integer()} |
                   {mtime, non_neg_integer()} |
                   {ctime, non_neg_integer()} |
                   {uid, non_neg_integer()} |
                   {gid, non_neg_integer()}.

-type name_in_archive() :: string().

-type extract_opt() :: {cwd, string()} |
                       {files, [name_in_archive()]} |
                       compressed |
                       cooked |
                       memory |
                       keep_old_files |
                       verbose.

-type create_opt() :: compressed |
                      cooked |
                      dereference |
                      verbose.

-type filelist() :: [file:filename() |
                     {name_in_archive(), file:filename_all()}].

-type tar_time() :: non_neg_integer().

%% The tar header, once fully parsed.
-record(tar_header, {
          name = "" :: name_in_archive(),       %% name of header file entry
          mode = 8#100644 :: non_neg_integer(), %% permission and mode bits
          uid = 0 :: non_neg_integer(),         %% user id of owner
          gid = 0 :: non_neg_integer(),         %% group id of owner
          size = 0 :: non_neg_integer(),        %% length in bytes
          mtime :: tar_time(),                  %% modified time
          typeflag :: char(),                   %% type of header entry
          linkname = "" :: name_in_archive(),   %% target name of link
          uname = "" :: string(),               %% user name of owner
          gname = "" :: string(),               %% group name of owner
          devmajor = 0 :: non_neg_integer(),    %% major number of character or block device
          devminor = 0 :: non_neg_integer(),    %% minor number of character or block device
          atime :: tar_time(),                  %% access time
          ctime :: tar_time()                   %% status change time
         }).
-type tar_header() :: #tar_header{}.

%% Metadata for a sparse file fragment
-record(sparse_entry, {
         offset = 0 :: non_neg_integer(),
         num_bytes = 0 :: non_neg_integer()}).
-type sparse_entry() :: #sparse_entry{}.
%% Contains metadata about fragments of a sparse file
-record(sparse_array, {
          entries = [] :: [sparse_entry()],
          is_extended = false :: boolean(),
          max_entries = 0 :: non_neg_integer()}).
-type sparse_array() :: #sparse_array{}.
%% A subset of tar header fields common to all tar implementations
-record(header_v7, {
          name :: binary(),
          mode :: binary(), %% octal
          uid :: binary(), %% integer
          gid :: binary(), %% integer
          size :: binary(), %% integer
          mtime :: binary(), %% integer
          checksum :: binary(), %% integer
          typeflag :: byte(), %% char
          linkname :: binary()}).
-type header_v7() :: #header_v7{}.
%% The set of fields specific to GNU tar formatted archives
-record(header_gnu, {
          header_v7 :: header_v7(),
          magic :: binary(),
          version :: binary(),
          uname :: binary(),
          gname :: binary(),
          devmajor :: binary(), %% integer
          devminor :: binary(), %% integer
          atime :: binary(), %% integer
          ctime :: binary(), %% integer
          sparse :: sparse_array(),
          real_size :: binary()}). %% integer
-type header_gnu() :: #header_gnu{}.
%% The set of fields specific to STAR-formatted archives
-record(header_star, {
          header_v7 :: header_v7(),
          magic :: binary(),
          version :: binary(),
          uname :: binary(),
          gname :: binary(),
          devmajor :: binary(), %% integer
          devminor :: binary(), %% integer
          prefix :: binary(),
          atime :: binary(), %% integer
          ctime :: binary(), %% integer
          trailer :: binary()}).
-type header_star() :: #header_star{}.
%% The set of fields specific to USTAR-formatted archives
-record(header_ustar, {
          header_v7 :: header_v7(),
          magic :: binary(),
          version :: binary(),
          uname :: binary(),
          gname :: binary(),
          devmajor :: binary(), %% integer
          devminor :: binary(), %% integer
          prefix :: binary()}).
-type header_ustar() :: #header_ustar{}.

-type header_fields() :: header_v7() |
                         header_gnu() |
                         header_star() |
                         header_ustar().

%% The overall tar reader, it holds the low-level file handle,
%% its access, position, and the I/O primitives wrapper.
-record(reader, {
          handle :: user_data(),
          access :: read | write | ram,
          pos = 0 :: non_neg_integer(),
          func :: file_op()
         }).
-opaque tar_descriptor() :: #reader{}.
-export_type([tar_descriptor/0]).

%% A reader for a regular file within the tar archive,
%% It tracks its current state relative to that file.
-record(reg_file_reader, {
          handle :: tar_descriptor(),
          num_bytes = 0,
          pos = 0,
          size = 0
         }).
-type reg_file_reader() :: #reg_file_reader{}.
%% A reader for a sparse file within the tar archive,
%% It tracks its current state relative to that file.
-record(sparse_file_reader, {
          handle :: tar_descriptor(),
          num_bytes = 0, %% bytes remaining
          pos = 0, %% pos
          size = 0, %% total size of file
          sparse_map = #sparse_array{}
         }).
-type sparse_file_reader() :: #sparse_file_reader{}.

%% Types for the readers
-type descriptor_type() :: tar_descriptor() | reg_file_reader() | sparse_file_reader().
-type user_data() :: term().

%% Type for the I/O primitive wrapper function
-type file_op() :: fun((write | close | read2 | position,
                       {user_data(), iodata()} | user_data() | {user_data(), non_neg_integer()}
                        | {user_data(), non_neg_integer()}) ->
                              ok | eof | {ok, string() | binary()} | {ok, non_neg_integer()}
                                 | {error, term()}).

%% These constants (except S_IFMT) are
%% used to determine what type of device
%% a file is. Namely, `S_IFMT band file_info.mode`
%% will equal one of these contants, and tells us
%% which type it is. The stdlib file_info record
%% does not differentiate between device types, and
%% will not allow us to differentiate between sockets
%% and named pipes. These constants are pulled from libc.
-define(S_IFMT, 61440).
-define(S_IFSOCK, 49152). %% socket
-define(S_FIFO, 4096).    %% fifo/named pipe
-define(S_IFBLK, 24576).  %% block device
-define(S_IFCHR, 8192).   %% character device

%% Typeflag constants for the tar header
-define(TYPE_REGULAR, $0).         %% regular file
-define(TYPE_REGULAR_A, 0).        %% regular file
-define(TYPE_LINK, $1).            %% hard link
-define(TYPE_SYMLINK, $2).         %% symbolic link
-define(TYPE_CHAR, $3).            %% character device node
-define(TYPE_BLOCK, $4).           %% block device node
-define(TYPE_DIR, $5).             %% directory
-define(TYPE_FIFO, $6).            %% fifo node
-define(TYPE_CONT, $7).            %% reserved
-define(TYPE_X_HEADER, $x).        %% extended header
-define(TYPE_X_GLOBAL_HEADER, $g). %% global extended header
-define(TYPE_GNU_LONGNAME, $L).    %% next file has a long name
-define(TYPE_GNU_LONGLINK, $K).    %% next file symlinks to a file with a long name
-define(TYPE_GNU_SPARSE, $S).      %% sparse file

%% Mode constants from tar spec
-define(MODE_ISUID, 4000).    %% set uid
-define(MODE_ISGID, 2000).    %% set gid
-define(MODE_ISVTX, 1000).    %% save text (sticky bit)
-define(MODE_ISDIR, 40000).   %% directory
-define(MODE_ISFIFO, 10000).  %% fifo
-define(MODE_ISREG, 100000).  %% regular file
-define(MODE_ISLNK, 120000).  %% symbolic link
-define(MODE_ISBLK, 60000).   %% block special file
-define(MODE_ISCHR, 20000).   %% character special file
-define(MODE_ISSOCK, 140000). %% socket

%% Keywords for PAX extended header
-define(PAX_ATIME, <<"atime">>).
-define(PAX_CHARSET, <<"charset">>).
-define(PAX_COMMENT, <<"comment">>).
-define(PAX_CTIME, <<"ctime">>). %% ctime is not a valid pax header
-define(PAX_GID, <<"gid">>).
-define(PAX_GNAME, <<"gname">>).
-define(PAX_LINKPATH, <<"linkpath">>).
-define(PAX_MTIME, <<"mtime">>).
-define(PAX_PATH, <<"path">>).
-define(PAX_SIZE, <<"size">>).
-define(PAX_UID, <<"uid">>).
-define(PAX_UNAME, <<"uname">>).
-define(PAX_XATTR, <<"SCHILY.xattr.">>).
-define(PAX_XATTR_STR, "SCHILY.xattr.").
-define(PAX_NONE, <<"">>).

%% Tar format constants
%% Unknown format
-define(FORMAT_UNKNOWN, 0).
%% The format of the original Unix V7 tar tool prior to standardization
-define(FORMAT_V7, 1).
%% The old and new GNU formats, incompatible with USTAR.
%% This covers the old GNU sparse extension, but it does
%% not cover the GNU sparse extensions using PAX headers,
%% versions 0.0, 0.1, and 1.0; these fall under the PAX format.
-define(FORMAT_GNU, 2).
%% Schily's tar format, which is incompatible with USTAR.
%% This does not cover STAR extensions to the PAX format; these
%% fall under the PAX format.
-define(FORMAT_STAR, 3).
%% USTAR is the former standardization of tar defined in POSIX.1-1988,
%% it is incompatible with the GNU and STAR formats.
-define(FORMAT_USTAR, 4).
%% PAX is the latest standardization of tar defined in POSIX.1-2001.
%% This is an extension of USTAR and is "backwards compatible" with it.
%%
%% Some newer formats add their own extensions to PAX, such as GNU sparse
%% files and SCHILY extended attributes. Since they are backwards compatible
%% with PAX, they will be labelled as "PAX".
-define(FORMAT_PAX, 5).

%% Magic constants
-define(MAGIC_GNU, <<"ustar ">>).
-define(VERSION_GNU, <<" \x00">>).
-define(MAGIC_USTAR, <<"ustar\x00">>).
-define(VERSION_USTAR, <<"00">>).
-define(TRAILER_STAR, <<"tar\x00">>).

%% Size constants
-define(BLOCK_SIZE, 512). %% size of each block in a tar stream
-define(NAME_SIZE, 100). %% max length of the name field in USTAR format
-define(PREFIX_SIZE, 155). %% max length of the prefix field in USTAR format

%% Maximum size of a nanosecond value as an integer
-define(MAX_NANO_INT_SIZE, 9).
%% Maximum size of a 64-bit signed integer
-define(MAX_INT64, (1 bsl 63 - 1)).

-define(PAX_GNU_SPARSE_NUMBLOCKS, <<"GNU.sparse.numblocks">>).
-define(PAX_GNU_SPARSE_OFFSET, <<"GNU.sparse.offset">>).
-define(PAX_GNU_SPARSE_NUMBYTES, <<"GNU.sparse.numbytes">>).
-define(PAX_GNU_SPARSE_MAP, <<"GNU.sparse.map">>).
-define(PAX_GNU_SPARSE_NAME, <<"GNU.sparse.name">>).
-define(PAX_GNU_SPARSE_MAJOR, <<"GNU.sparse.major">>).
-define(PAX_GNU_SPARSE_MINOR, <<"GNU.sparse.minor">>).
-define(PAX_GNU_SPARSE_SIZE, <<"GNU.sparse.size">>).
-define(PAX_GNU_SPARSE_REALSIZE, <<"GNU.sparse.realsize">>).

-define(V7_NAME, 0).
-define(V7_NAME_LEN, 100).
-define(V7_MODE, 100).
-define(V7_MODE_LEN, 8).
-define(V7_UID, 108).
-define(V7_UID_LEN, 8).
-define(V7_GID, 116).
-define(V7_GID_LEN, 8).
-define(V7_SIZE, 124).
-define(V7_SIZE_LEN, 12).
-define(V7_MTIME, 136).
-define(V7_MTIME_LEN, 12).
-define(V7_CHKSUM, 148).
-define(V7_CHKSUM_LEN, 8).
-define(V7_TYPE, 156).
-define(V7_TYPE_LEN, 1).
-define(V7_LINKNAME, 157).
-define(V7_LINKNAME_LEN, 100).

-define(STAR_TRAILER, 508).
-define(STAR_TRAILER_LEN, 4).

-define(USTAR_MAGIC, 257).
-define(USTAR_MAGIC_LEN, 6).
-define(USTAR_VERSION, 263).
-define(USTAR_VERSION_LEN, 2).
-define(USTAR_UNAME, 265).
-define(USTAR_UNAME_LEN, 32).
-define(USTAR_GNAME, 297).
-define(USTAR_GNAME_LEN, 32).
-define(USTAR_DEVMAJ, 329).
-define(USTAR_DEVMAJ_LEN, 8).
-define(USTAR_DEVMIN, 337).
-define(USTAR_DEVMIN_LEN, 8).
-define(USTAR_PREFIX, 345).
-define(USTAR_PREFIX_LEN, 155).

-define(GNU_MAGIC, 257).
-define(GNU_MAGIC_LEN, 6).
-define(GNU_VERSION, 263).
-define(GNU_VERSION_LEN, 2).

%% ?BLOCK_SIZE of zero-bytes.
%% Two of these in a row mark the end of an archive.
-define(ZERO_BLOCK, <<0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,0,0,0,0>>).

-define(BILLION, 1000000000).

-define(EPOCH, {{1970,1,1}, {0,0,0}}).
