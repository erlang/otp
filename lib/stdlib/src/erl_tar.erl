%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%% This module implements extraction/creation of tar archives.
%% It supports reading most common tar formats, namely V7, STAR,
%% USTAR, GNU, BSD/libarchive, and PAX. It produces archives in USTAR
%% format, unless it must use PAX headers, in which case it produces PAX
%% format.
%%
%% The following references where used:
%%   http://www.freebsd.org/cgi/man.cgi?query=tar&sektion=5
%%   http://www.gnu.org/software/tar/manual/html_node/Standard.html
%%   http://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html
-module(erl_tar).

-export([init/3,
         create/2, create/3,
         extract/1, extract/2,
         table/1, table/2, t/1, tt/1,
         open/2, close/1,
         parse_octal/1,
         add/3, add/4,
         format_error/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("erl_tar.hrl").

%% Converts the short error reason to a descriptive string.
-spec format_error(term()) -> string().
format_error(invalid_tar_checksum) ->
    "Checksum failed";
format_error(bad_header) ->
    "Unrecognized tar header format";
format_error({invalid_header, negative_size}) ->
    "Invalid header: negative size";
format_error(invalid_sparse_header_size) ->
    "Invalid sparse header: negative size";
format_error(invalid_sparse_map_entry) ->
    "Invalid sparse map entry";
format_error({invalid_sparse_map_entry, Reason}) ->
    lists:flatten(io_lib:format("Invalid sparse map entry: ~p", [Reason]));
format_error(invalid_end_of_archive) ->
    "Invalid end of archive";
format_error(eof) ->
    "Unexpected end of file";
format_error(integer_overflow) ->
    "Failed to parse numeric: integer overflow";
format_error({misaligned_read, Pos}) ->
    lists:flatten(io_lib:format("Read a block which was misaligned: block_size=~p pos=~p",
                                [?BLOCK_SIZE, Pos]));
format_error(invalid_gnu_1_0_sparsemap) ->
    "Invalid GNU sparse map (version 1.0)";
format_error({invalid_gnu_0_1_sparsemap, Format}) ->
    lists:flatten(io_lib:format("Invalid GNU sparse map (version ~s)", [Format]));
format_error({Name,Reason}) ->
    lists:flatten(io_lib:format("~ts: ~ts", [Name,format_error(Reason)]));
format_error(Atom) when is_atom(Atom) ->
    file:format_error(Atom);
format_error(Term) ->
    lists:flatten(io_lib:format("~tp", [Term])).

%% Initializes a new reader given a custom file handle and I/O wrappers
-spec init(handle(), write | read, file_op()) -> {ok, reader()}.
init(Handle, AccessMode, Fun) when is_function(Fun, 2) ->
    Reader = #reader{handle=Handle,access=AccessMode,func=Fun},
    {ok, Pos, Reader2} = do_position(Reader, {cur, 0}),
    {ok, Reader2#reader{pos=Pos}}.

%%%================================================================
%% Extracts all files from the tar file Name.
-spec extract(string()) -> ok | {error, term()}.
extract(Name) ->
    extract(Name, []).

%% Extracts (all) files from the tar file Name.
%% Options accepted:
%%  - cooked: Opens the tar file without mode `raw`
%%  - compressed: Uncompresses the tar file when reading
%%  - memory: Returns the tar contents as a list of tuples {Name, Bin}
%%  - keep_old_files: Extracted files will not overwrite the destination
%%  - {files, ListOfFilesToExtract}: Only extract ListOfFilesToExtract
%%  - verbose: Prints verbose information about the extraction,
%%  - {cwd, AbsoluteDir}: Sets the current working directory for the extraction
-spec extract(string(), [extract_opt()]) ->
  ok | {ok, [{string(), binary()}]} | {error, term()}.
extract(Name, Opts) ->
    Opts2 = extract_opts(Opts),
    Acc = if Opts2#read_opts.output =:= memory -> []; true -> ok end,
    foldl_read(Name, fun extract1/4, Acc, Opts2).

-spec extract1(eof | tar_header(), reader_type(), read_opts(), ok | [{string(), binary()}]) ->
                      {ok, {ok, [{string(), binary()}]}, reader_type()} |
                      {ok, ok, reader_type()} |
                      {error, term()}.
extract1(eof, Reader, _, Acc) when is_list(Acc) ->
    {ok, {ok, lists:reverse(Acc)}, Reader};
extract1(eof, Reader, _, Acc) ->
    {ok, Acc, Reader};
extract1(Header = #tar_header{name=Name,size=Size}, Reader, Opts, Acc) ->
    case check_extract(Name, Opts) of
        true ->
            case do_read(Reader, Size) of
                {ok, Bin, Reader2} ->
                    case write_extracted_element(Header, Bin, Opts) of
                        ok ->
                            {ok, Acc, Reader2};
                        {ok, NameBin} when is_list(Acc) ->
                            {ok, [NameBin | Acc], Reader2};
                        {error, _} = Err ->
                            throw(Err)
                    end;
                Err ->
                    throw(Err)
            end;
        false ->
            {ok, Acc, skip_file(Reader)}
    end.

%% Checks if the file Name should be extracted.
-spec check_extract(string(), read_opts()) -> boolean().
check_extract(_, #read_opts{files=all}) ->
    true;
check_extract(Name, #read_opts{files=Files}) ->
    ordsets:is_element(Name, Files).

%%%================================================================
%% The following table functions produce a list of information about
%% the files contained in the archive.
-type filename() :: string().
-type typeflag() :: regular | link | symlink |
                    char | block | directory |
                    fifo | reserved | unknown.
-type mode() :: non_neg_integer().
-type uid() :: non_neg_integer().
-type gid() :: non_neg_integer().

-type tar_entry() :: {filename(),
                      typeflag(),
                      non_neg_integer(),
                      calendar:datetime(),
                      mode(),
                      uid(),
                      gid()}.

%% Returns a list of names of the files in the tar file Name.
-spec table(string()) -> {ok, [string()]} | {error, term()}.
table(Name) ->
    table(Name, []).

%% Returns a list of names of the files in the tar file Name.
%% Options accepted: compressed, verbose, cooked.
-spec table(string(), [compressed | verbose | cooked]) ->
                   {ok, [tar_entry()]} | {error, term()}.
table(Name, Opts) ->
    foldl_read(Name, fun table1/4, [], table_opts(Opts)).

-spec table1(eof | tar_header(), reader_type(), read_opts(), [string() | tuple()]) ->
                    {ok, {ok, [string() | tuple()]}, reader_type()} |
                    {error, term()}.
table1(eof, Reader, _, Result) ->
    {ok, {ok, lists:reverse(Result)}, Reader};
table1(Header = #tar_header{}, Reader, #read_opts{verbose=Verbose}, Result) ->
    Attrs = table1_attrs(Header, Verbose),
    Reader2 = skip_file(Reader),
    {ok, [Attrs|Result], Reader2}.

%% Extracts attributes relevant to table1's output
table1_attrs(Header = #tar_header{typeflag=Typeflag,mode=Mode}, true) ->
    Type = typeflag(Typeflag),
    Name = Header#tar_header.name,
    Mtime = Header#tar_header.mtime,
    Uid = Header#tar_header.uid,
    Gid = Header#tar_header.gid,
    Size = Header#tar_header.size,
    {Name, Type, Size, Mtime, Mode, Uid, Gid};
table1_attrs(#tar_header{name=Name}, _Verbose) ->
    Name.

typeflag(?TYPE_REGULAR) -> regular;
typeflag(?TYPE_REGULAR_A) -> regular;
typeflag(?TYPE_LINK) -> link;
typeflag(?TYPE_SYMLINK) -> symlink;
typeflag(?TYPE_CHAR) -> char;
typeflag(?TYPE_BLOCK) -> block;
typeflag(?TYPE_DIR) -> directory;
typeflag(?TYPE_FIFO) -> fifo;
typeflag(?TYPE_CONT) -> reserved;
typeflag(_) -> unknown.

%%%================================================================
%% Comments for printing the contents of a tape archive,
%% meant to be invoked from the shell.

%% Prints each filename in the archive
-spec t(string()) -> ok | {error, term()}.
t(Name) ->
    case table(Name) of
	{ok, List} ->
	    lists:foreach(fun(N) -> ok = io:format("~ts\n", [N]) end, List);
	Error ->
	    Error
    end.

%% Prints verbose information about each file in the archive
-spec tt(string()) -> ok | {error, term()}.
tt(Name) ->
    case table(Name, [verbose]) of
	{ok, List} ->
	    lists:foreach(fun print_header/1, List);
	Error ->
	    Error
    end.

%% Used by tt/1 to print a tar_entry tuple
-spec print_header(tar_entry()) -> ok.
print_header({Name, Type, Size, Mtime, Mode, Uid, Gid}) ->
    io:format("~s~s ~4w/~-4w ~7w ~s ~s\n",
	      [type_to_string(Type), mode_to_string(Mode),
	       Uid, Gid, Size, time_to_string(Mtime), Name]).

type_to_string(regular)   -> "-";
type_to_string(directory) -> "d";
type_to_string(link)      -> "l";
type_to_string(symlink)   -> "s";
type_to_string(char)      -> "c";
type_to_string(block)     -> "b";
type_to_string(fifo)      -> "f";
type_to_string(unknown)   -> "?".

%% Converts a numeric mode to it's human-readable representation
mode_to_string(Mode) ->
    mode_to_string(Mode, "xwrxwrxwr", []).
mode_to_string(Mode, [C|T], Acc) when Mode band 1 =:= 1 ->
    mode_to_string(Mode bsr 1, T, [C|Acc]);
mode_to_string(Mode, [_|T], Acc) ->
    mode_to_string(Mode bsr 1, T, [$-|Acc]);
mode_to_string(_, [], Acc) ->
    Acc.

%% Converts a datetime tuple to a readable string
time_to_string({{Y, Mon, Day}, {H, Min, _}}) ->
    io_lib:format("~s ~2w ~s:~s ~w", [month(Mon), Day, two_d(H), two_d(Min), Y]).

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

%%%================================================================
%%% The open function with friends is to keep the file and binary api of this module
-spec open(file:filename(), [write | compressed | cooked]) ->
                  {ok, reader()} | {error, term()}.
open(Name, Mode) ->
    case open_mode(Mode) of
	{ok, Access, Raw, Opts} ->
	    open1(Name, Access, Raw, Opts);
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

-spec open1({binary, binary()} | {file, term()} | file:filename(),
            read | write,
            true | false,
            [compressed | cooked]) -> {ok, reader()} | {error, term()}.
open1({binary,Bin}, read, _Raw, Opts) ->
    case file:open(Bin, [ram,binary,read]) of
	{ok,File} ->
            _ = [ram_file:uncompress(File) || Opts =:= [compressed]],
            {ok, #reader{handle=File,access=read,func=fun file_op/2}};
	Error ->
	    Error
    end;
open1({file, Fd}, read, _Raw, _Opts) ->
    Reader = #reader{handle=Fd,access=read,func=fun file_op/2},
    {ok, Pos, Reader2} = do_position(Reader, {cur, 0}),
    {ok, Reader2#reader{pos=Pos}};
open1(Name, Access, Raw, Opts) ->
    case file:open(Name, Raw ++ [binary, Access|Opts]) of
	{ok, File} ->
            {ok, #reader{handle=File,access=Access,func=fun file_op/2}};
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

open_mode(Mode) ->
    open_mode(Mode, false, [raw], []).

open_mode(read, _, Raw, _) ->
    {ok, read, Raw, []};
open_mode(write, _, Raw, _) ->
    {ok, write, Raw, []};
open_mode([read|Rest], false, Raw, Opts) ->
    open_mode(Rest, read, Raw, Opts);
open_mode([write|Rest], false, Raw, Opts) ->
    open_mode(Rest, write, Raw, Opts);
open_mode([compressed|Rest], Access, Raw, Opts) ->
    open_mode(Rest, Access, Raw, [compressed|Opts]);
open_mode([cooked|Rest], Access, _Raw, Opts) ->
    open_mode(Rest, Access, [], Opts);
open_mode([], Access, Raw, Opts) ->
    {ok, Access, Raw, Opts};
open_mode(_, _, _, _) ->
    {error, einval}.

file_op(write, {Fd, Data}) ->
    file:write(Fd, Data);
file_op(position, {Fd, Pos}) ->
    file:position(Fd, Pos);
file_op(read2, {Fd, Size}) ->
    file:read(Fd, Size);
file_op(close, Fd) ->
    file:close(Fd).

%% Closes a tar archive.
-spec close(reader()) -> ok | {error, term()}.
close(Reader=#reader{access=read}) ->
    ok = do_close(Reader);
close(Reader=#reader{access=write}) ->
    {ok, Reader2} = pad_file(Reader),
    ok = do_close(Reader2),
    ok;
close(_) ->
    {error, einval}.

pad_file(Reader = #reader{pos=Pos}) ->
    %% There must be at least two zero blocks at the end.
    PadCurrent = skip_padding(Pos+?BLOCK_SIZE),
    Padding = lists:duplicate(PadCurrent, 0),
    do_write(Reader, [Padding, ?ZERO_BLOCK, ?ZERO_BLOCK]).


%%%================================================================
%% Creation/modification of tar archives

%% Creates a tar file Name containing the given files.
-spec create(file:filename(), filelist()) -> ok | {error, {string(), term()}}.
create(Name, FileList) ->
    create(Name, FileList, []).

%% Creates a tar archive Name containing the given files.
%% Accepted options: verbose, compressed, cooked
-spec create(file:filename(), filelist(), [create_opt()]) ->
                    ok | {error, {string(), term()}}.
create(Name, FileList, Options) ->
    Mode = lists:filter(fun(X) -> (X=:=compressed) or (X=:=cooked)
                        end, Options),
    case open(Name, [write|Mode]) of
	{ok, TarFile} ->
            do_create(TarFile, FileList, Options);
	Reason ->
	    Reason
    end.

do_create(TarFile, [], _Opts) ->
    close(TarFile);
do_create(TarFile, [{NameInArchive, NameOrBin}|Rest], Opts) ->
    case add(TarFile, NameOrBin, NameInArchive, Opts) of
        ok ->
            do_create(TarFile, Rest, Opts);
        {error, _} = Err ->
            close(TarFile),
            Err
    end;
do_create(TarFile, [Name|Rest], Opts) ->
    case add(TarFile, Name, Name, Opts) of
        ok ->
            do_create(TarFile, Rest, Opts);
        {error, _} = Err ->
            close(TarFile),
            Err
    end.

%% Adds a file to a tape archive.
-spec add(reader(), string() | {string(), string() | binary()}, [add_opt()]) ->
                 ok | {error, {string(), term()}}.
add(Reader, Name, Options) ->
    add(Reader, Name, Name, Options).
add(Reader=#reader{access=write}, Name, NameInArchive, Options) ->
    Opts = #add_opts{read_info=fun(F) -> file:read_link_info(F) end},
    add1(Reader, Name, NameInArchive, add_opts(Options, Opts));
add(#reader{access=read},_,_,_) ->
    {error, eacces};
add(Reader,_,_,_) ->
    {error, {badarg, Reader}}.

add_opts([dereference|T], Opts) ->
    add_opts(T, Opts#add_opts{read_info=fun(F) -> file:read_file_info(F) end});
add_opts([verbose|T], Opts) ->
    add_opts(T, Opts#add_opts{verbose=true});
add_opts([{chunks,N}|T], Opts) ->
    add_opts(T, Opts#add_opts{chunk_size=N});
add_opts([_|T], Opts) ->
    add_opts(T, Opts);
add_opts([], Opts) ->
    Opts.

add1(Reader = #reader{}, Name, NameInArchive, Opts=#add_opts{read_info=ReadInfo})
  when is_list(Name) ->
    Res = case ReadInfo(Name) of
        {error, Reason0} ->
            {error, {Name, Reason0}};
        {ok, Fi = #file_info{type=symlink}} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            {ok, Linkname} = file:read_link(Name),
            Header = fileinfo_to_header(NameInArchive, Fi, Linkname),
            add_header(Reader, Header, Opts);
        {ok, Fi = #file_info{type=regular}} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            Header = fileinfo_to_header(NameInArchive, Fi, false),
            {ok, Reader2} = add_header(Reader, Header, Opts),
            {ok, File} = file:open(Name, [read, binary]),
            FileSize = Header#tar_header.size,
            {ok, FileSize, Reader3} = do_copy(Reader2, File),
            Padding = skip_padding(FileSize),
            Pad = binary:copy(<<0>>, Padding),
            do_write(Reader3, Pad);
        {ok, Fi = #file_info{type=directory}} ->
            add_directory(Reader, Name, NameInArchive, Fi, Opts);
        {ok, Fi = #file_info{}} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            Header = fileinfo_to_header(NameInArchive, Fi, false),
            add_header(Reader, Header, Opts)
    end,
    case Res of
        ok -> ok;
        {ok, _Reader} -> ok;
        {error, {_Name, _Reason}} = Err -> Err;
        {error, Reason} -> {error, {Name, Reason}}
    end;
add1(Reader, Bin, NameInArchive, Opts) when is_binary(Bin) ->
    add_verbose(Opts, "a ~ts~n", [NameInArchive]),
    Now = calendar:now_to_local_time(erlang:timestamp()),
    Header = #tar_header{
              name = NameInArchive,
              size = byte_size(Bin),
              typeflag = ?TYPE_REGULAR,
              atime = Now,
              mtime = Now,
              ctime = Now,
              mode = 8#100644},
    {ok, Reader2} = add_header(Reader, Header, Opts),
    Padding = skip_padding(byte_size(Bin)),
    Data = [Bin, binary:copy(<<0>>, Padding)],
    case do_write(Reader2, Data) of
        {ok, _Reader3} -> ok;
        {error, Reason} -> {error, {NameInArchive, Reason}}
    end.

add_directory(Reader, DirName, NameInArchive, Info, Opts) ->
    case file:list_dir(DirName) of
	{ok, []} ->
	    add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            Header = fileinfo_to_header(NameInArchive, Info, false),
            add_header(Reader, Header, Opts);
	{ok, Files} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            case catch add_files(Reader, Files, DirName, NameInArchive, Opts) of
                ok -> ok;
                {ok, Reader2} -> {ok, Reader2};
                {error, {_Name, _Reason}} = Err -> Err;
                {error, Reason} -> {error, {DirName, Reason}}
            end;
	{error, Reason} ->
	    {error, {DirName, Reason}}
    end.

add_files(_Reader, [], _Dir, _DirInArchive, _Opts) ->
    ok;
add_files(Reader, [Name|Rest], Dir, DirInArchive, Opts=#add_opts{read_info=Info}) ->
    FullName = filename:join(Dir, Name),
    NameInArchive = filename:join(DirInArchive, Name),
    Res = case Info(FullName) of
        {error, Reason} ->
            {error, {FullName, Reason}};
        {ok, Fi = #file_info{type=directory}} ->
            add_directory(Reader, FullName, NameInArchive, Fi, Opts);
        {ok, Fi = #file_info{type=symlink}} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            {ok, Linkname} = file:read_link(FullName),
            Header = fileinfo_to_header(NameInArchive, Fi, Linkname),
            add_header(Reader, Header, Opts);
        {ok, Fi = #file_info{type=regular}} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            Header = fileinfo_to_header(NameInArchive, Fi, false),
            {ok, Reader2} = add_header(Reader, Header, Opts),
            {ok, File} = file:open(FullName, [read,binary]),
            FileSize = Header#tar_header.size,
            {ok, FileSize, Reader3} = do_copy(Reader2, File),
            Padding = skip_padding(FileSize),
            Pad = binary:copy(<<0>>, Padding),
            do_write(Reader3, Pad);
        {ok, Fi = #file_info{}} ->
            add_verbose(Opts, "a ~ts~n", [NameInArchive]),
            Header = fileinfo_to_header(NameInArchive, Fi, false),
            add_header(Reader, Header, Opts)
    end,
    case Res of
        ok -> add_files(Reader, Rest, Dir, DirInArchive, Opts);
        {ok, ReaderNext} -> add_files(ReaderNext, Rest, Dir, DirInArchive, Opts);
        {error, _} = Err -> Err
    end.

format_string(String, Size) when length(String) > Size ->
    throw({error, {write_string, field_too_long}});
format_string(String, Size) ->
    Ascii = to_ascii(String),
    if length(Ascii) < Size ->
            [Ascii, 0];
       true -> Ascii
    end.

format_octal(Octal) ->
    iolist_to_binary(io_lib:fwrite("~.8B", [Octal])).

add_header(Reader=#reader{}, Header=#tar_header{}, Opts) ->
    {ok, Iodata} = build_header(Header, Opts),
    do_write(Reader, Iodata).

build_header(Header = #tar_header{}, Opts) ->
    #tar_header{
       name=Name,
       mode=Mode,
       uid=Uid,
       gid=Gid,
       size=Size,
       typeflag=Type,
       linkname=Linkname,
       uname=Uname,
       gname=Gname,
       devmajor=Devmaj,
       devminor=Devmin
      } = Header,
    {ok, Block} = file:open(?ZERO_BLOCK, [ram,read,write]),
    Mtime = datetime_to_posix(Header#tar_header.mtime),
    Pax = write_string(Block, ?V7_NAME, ?V7_NAME_LEN, Name, ?PAX_PATH, #{}),
    ok = write_octal(Block, ?V7_MODE, ?V7_MODE_LEN, Mode),
    Pax2 = write_numeric(Block, ?V7_UID, ?V7_UID_LEN, Uid, ?PAX_UID, Pax),
    Pax3 = write_numeric(Block, ?V7_GID, ?V7_GID_LEN, Gid, ?PAX_GID, Pax2),
    Pax4 = write_numeric(Block, ?V7_SIZE, ?V7_SIZE_LEN, Size, ?PAX_SIZE, Pax3),
    Pax5 = write_numeric(Block, ?V7_MTIME, ?V7_MTIME_LEN, Mtime, ?PAX_NONE, Pax4),
    Pax6 = write_string(Block, ?V7_TYPE, ?V7_TYPE_LEN, <<Type>>, ?PAX_NONE, Pax5),
    Pax7 = write_string(Block, ?V7_LINKNAME, ?V7_LINKNAME_LEN,
                        Linkname, ?PAX_LINKPATH, Pax6),
    Pax8 = write_string(Block, ?USTAR_UNAME, ?USTAR_UNAME_LEN,
                        Uname, ?PAX_UNAME, Pax7),
    Pax9 = write_string(Block, ?USTAR_GNAME, ?USTAR_GNAME_LEN,
                        Gname, ?PAX_GNAME, Pax8),
    Pax10 = write_numeric(Block, ?USTAR_DEVMAJ, ?USTAR_DEVMAJ_LEN,
                          Devmaj, ?PAX_NONE, Pax9),
    Pax11 = write_numeric(Block, ?USTAR_DEVMIN, ?USTAR_DEVMIN_LEN,
                          Devmin, ?PAX_NONE, Pax10),
    % only use ustar header when name is too long
    Pax12 = case maps:get(?PAX_PATH, Pax11, nil) of
        nil ->
            Pax11;
        PaxPath ->
            case split_ustar_path(PaxPath) of
                {ok, UstarName, UstarPrefix} ->
                    _ = write_string(Block, ?V7_NAME, ?V7_NAME_LEN,
                                     UstarName, ?PAX_NONE, #{}),
                    _ = write_string(Block, ?USTAR_PREFIX, ?USTAR_PREFIX_LEN,
                                     UstarPrefix, ?PAX_NONE, #{}),
                    maps:remove(?PAX_PATH, Pax11);
                false ->
                    Pax11
            end
    end,
    Xattrs = maps:to_list(Header#tar_header.xattrs),
    Pax13 = lists:foldl(fun ({K,V}, Acc) ->
                                maps:put(<<?PAX_XATTR_STR,K>>, V, Acc)
                        end, Pax12, Xattrs),
    PaxEntry = case maps:size(Pax13) of
        0 -> [];
        _ -> build_pax_entry(Header, Pax13, Opts)
    end,
    set_format(Block, ?FORMAT_USTAR),
    set_checksum(Block),
    {ok,_} = file:position(Block, 0),
    {ok, HeaderBytes} = file:read(Block, ?BLOCK_SIZE),
    ok = file:close(Block),
    {ok, [PaxEntry, HeaderBytes]}.

set_format(_Block, ?FORMAT_V7) ->
    ok;
set_format(Block, ?FORMAT_GNU) ->
    {ok, _} = file:position(Block, ?GNU_MAGIC),
    ok = file:write(Block, ?MAGIC_GNU),
    {ok, _} = file:position(Block, ?GNU_VERSION),
    ok = file:write(Block, ?VERSION_GNU),
    ok;
set_format(Block, ?FORMAT_STAR) ->
    {ok, _} = file:position(Block, ?USTAR_MAGIC),
    ok = file:write(Block, ?MAGIC_USTAR),
    {ok, _} = file:position(Block, ?USTAR_VERSION),
    ok = file:write(Block, ?VERSION_USTAR),
    {ok, _} = file:position(Block, ?STAR_TRAILER),
    ok = file:write(Block, ?TRAILER_STAR),
    ok;
set_format(Block, Format)
  when Format =:= ?FORMAT_USTAR orelse Format =:= ?FORMAT_PAX ->
    {ok,_} = file:position(Block, ?USTAR_MAGIC),
    ok = file:write(Block, ?MAGIC_USTAR),
    {ok,_} = file:position(Block, ?USTAR_VERSION),
    ok = file:write(Block, ?VERSION_USTAR),
    ok;
set_format(_Block, Format) ->
    throw({error, {invalid_format, Format}}).

set_checksum(Block) ->
    {ok,_} = file:position(Block, 0),
    {ok,Bytes} = file:read(Block, ?BLOCK_SIZE),
    Checksum = compute_checksum(iolist_to_binary(Bytes)),
    {ok,_} = file:position(Block, ?V7_CHKSUM),
    ok = write_octal(Block, ?V7_CHKSUM, ?V7_CHKSUM_LEN, Checksum),
    ok.

build_pax_entry(Header, PaxAttrs, Opts) ->
    Path = Header#tar_header.name,
    Filename = filename:basename(Path),
    Dir = filename:dirname(Path),
    Path2 = filename:join([Dir, "PaxHeaders.0", Filename]),
    AsciiPath = to_ascii(Path2),
    Path3 = if byte_size(AsciiPath) > ?V7_NAME_LEN ->
                    binary_part(AsciiPath, 0, ?V7_NAME_LEN);
               true ->
                    AsciiPath
            end,
    Keys = maps:keys(PaxAttrs),
    SortedKeys = lists:sort(Keys),
    PaxFile = build_pax_file(SortedKeys, PaxAttrs),
    Size = byte_size(PaxFile),
    Padding = (?BLOCK_SIZE -
                   (byte_size(PaxFile) rem ?BLOCK_SIZE)) rem ?BLOCK_SIZE,
    Pad = binary:copy(<<0>>, Padding),
    PaxHeader = #tar_header{
                   name=Path3,
                   size=Size,
                   mtime=Header#tar_header.mtime,
                   atime=Header#tar_header.atime,
                   ctime=Header#tar_header.ctime,
                   typeflag=?TYPE_X_HEADER
                  },
    {ok, PaxHeaderData} = build_header(PaxHeader, Opts),
    [PaxHeaderData, PaxFile, Pad].

build_pax_file(Keys, PaxAttrs) ->
    build_pax_file(Keys, PaxAttrs, []).
build_pax_file([], _, Acc) ->
    unicode:characters_to_binary(Acc);
build_pax_file([K|Rest], Attrs, Acc) ->
    V = maps:get(K, Attrs),
    Size = sizeof(K) + sizeof(V) + 3,
    Size2 = sizeof(Size) + Size,
    Key = to_string(K),
    Value = to_string(V),
    Record = unicode:characters_to_binary(io_lib:format("~B ~ts=~ts\n", [Size2, Key, Value])),
    if byte_size(Record) =/= Size2 ->
            Size3 = byte_size(Record),
            Record2 = io_lib:format("~B ~ts=~ts\n", [Size3, Key, Value]),
            build_pax_file(Rest, Attrs, [Acc, Record2]);
       true ->
            build_pax_file(Rest, Attrs, [Acc, Record])
    end.

sizeof(Bin) when is_binary(Bin) ->
    byte_size(Bin);
sizeof(List) when is_list(List) ->
    length(List);
sizeof(N) when is_integer(N) ->
    byte_size(integer_to_binary(N));
sizeof(N) when is_float(N) ->
    byte_size(float_to_binary(N)).

to_string(Bin) when is_binary(Bin) ->
    unicode:characters_to_list(Bin);
to_string(List) when is_list(List) ->
    List;
to_string(N) when is_integer(N) ->
    integer_to_list(N);
to_string(N) when is_float(N) ->
    float_to_list(N).

split_ustar_path(Path) ->
    Len = length(Path),
    NotAscii = not is_ascii(Path),
    if Len =< ?V7_NAME_LEN orelse NotAscii ->
         false;
       true ->
            PathBin = binary:list_to_bin(Path),
            case binary:split(PathBin, [<<$/>>], [global, trim_all]) of
                [Part] when byte_size(Part) >= ?V7_NAME_LEN ->
                    false;
                Parts ->
                    case lists:last(Parts) of
                        Name when byte_size(Name) >= ?V7_NAME_LEN ->
                            false;
                        Name ->
                            Parts2 = lists:sublist(Parts, length(Parts) - 1),
                            lists:foldl(fun
                              (_, false) ->
                                  false;
                              (P, {ok, _, nil}) when byte_size(P) > ?USTAR_PREFIX_LEN ->
                                  false;
                              (P, {ok, _, Acc}) when
                                   (byte_size(P)+byte_size(Acc)) > ?USTAR_PREFIX_LEN ->
                                  false;
                              (P, {ok, N, nil}) ->
                                  {ok, N, P};
                              (P, {ok, N, Acc}) ->
                                  {ok, N, <<Acc/binary,$/,P/binary>>}
                            end, {ok, Name, nil}, Parts2)
                    end
            end
    end.

datetime_to_posix(DateTime) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Secs = calendar:datetime_to_gregorian_seconds(DateTime),
    case Secs - Epoch of
        N when N < 0 -> 0;
        N -> N
    end.

write_octal(Block, Pos, Size, X) ->
    Octal = zero_pad(format_octal(X), Size-1),
    if byte_size(Octal) < Size ->
            {ok, _} = file:position(Block, Pos),
            ok = file:write(Block, Octal),
            {ok, _} = file:position(Block, Pos+Size),
            ok;
       true ->
            throw({error, {write_failed, octal_field_too_long}})
    end.

write_string(Block, Pos, Size, Str, PaxAttr, Pax) ->
    {ok, _} = file:position(Block, Pos),
    NotAscii = not is_ascii(Str),
    if PaxAttr =/= ?PAX_NONE andalso (length(Str) > Size orelse NotAscii) ->
            Pax2 = maps:put(PaxAttr, Str, Pax),
            {ok, _} = file:position(Block, Pos+Size),
            Pax2;
       true ->
            Formatted = format_string(Str, Size),
            ok = file:write(Block, Formatted),
            {ok, _} = file:position(Block, Pos+Size),
            Pax
    end.
write_numeric(Block, Pos, Size, X, PaxAttr, Pax) ->
    % attempt octal
    Octal = zero_pad(format_octal(X), Size-1),
    if byte_size(Octal) < Size ->
            ok = file:write(Block, [Octal, 0]),
            {ok, _} = file:position(Block, Pos+Size),
            Pax;
       PaxAttr =/= ?PAX_NONE ->
            Pax2 = maps:put(PaxAttr, X, Pax),
            {ok, _} = file:position(Block, Pos+Size),
            Pax2;
       true ->
            throw({error, {write_failed, numeric_field_too_long}})
    end.

zero_pad(Str, Size) when byte_size(Str) >= Size ->
    Str;
zero_pad(Str, Size) ->
    Padding = Size - byte_size(Str),
    Pad = binary:copy(<<$0>>, Padding),
    <<Pad/binary, Str/binary>>.


%%%================================================================
%% Functions for creating or modifying tar archives

read_block(Reader) ->
    case do_read(Reader, ?BLOCK_SIZE) of
        eof ->
            throw({error, eof});
            %eof;
        %% Two zero blocks mark the end of the archive
        {ok, ?ZERO_BLOCK, Reader1} ->
            case do_read(Reader1, ?BLOCK_SIZE) of
                eof ->
                    throw({error, eof});
                {ok, ?ZERO_BLOCK, _Reader2} ->
                    eof;
                {ok, _Block, _Reader2} ->
                    throw({error, invalid_end_of_archive});
                {error,_} = Err ->
                    throw(Err)
            end;
        {ok, Block, Reader1} when is_binary(Block) ->
            {ok, Block, Reader1};
        {error, _} = Err ->
            throw(Err)
    end.

get_header(Reader=#reader{}) ->
    case read_block(Reader) of
        eof ->
            eof;
        {ok, Block, Reader1} ->
            convert_header(Block, Reader1)
    end.

%% Converts the tar header to a record.
to_v7(Bin) when is_binary(Bin), byte_size(Bin) =:= ?BLOCK_SIZE ->
    #header_v7{
       name=binary_part(Bin, ?V7_NAME, ?V7_NAME_LEN),
       mode=binary_part(Bin, ?V7_MODE, ?V7_MODE_LEN),
       uid=binary_part(Bin, ?V7_UID, ?V7_UID_LEN),
       gid=binary_part(Bin, ?V7_GID, ?V7_GID_LEN),
       size=binary_part(Bin, ?V7_SIZE, ?V7_SIZE_LEN),
       mtime=binary_part(Bin, ?V7_MTIME, ?V7_MTIME_LEN),
       checksum=binary_part(Bin, ?V7_CHKSUM, ?V7_CHKSUM_LEN),
       typeflag=binary:at(Bin, ?V7_TYPE),
       linkname=binary_part(Bin, ?V7_LINKNAME, ?V7_LINKNAME_LEN)
      }.

to_gnu(V7=#header_v7{}, Bin)
  when is_binary(Bin), byte_size(Bin) =:= ?BLOCK_SIZE ->
    #header_gnu{
       header_v7=V7,
       magic=binary_part(Bin, ?GNU_MAGIC, ?GNU_MAGIC_LEN),
       version=binary_part(Bin, ?GNU_VERSION, ?GNU_VERSION_LEN),
       uname=binary_part(Bin, 265, 32),
       gname=binary_part(Bin, 297, 32),
       devmajor=binary_part(Bin, 329, 8),
       devminor=binary_part(Bin, 337, 8),
       atime=binary_part(Bin, 345, 12),
       ctime=binary_part(Bin, 357, 12),
       sparse=to_sparse_array(binary_part(Bin, 386, 24*4+1)),
       real_size=binary_part(Bin, 483, 12)
      }.

to_star(V7=#header_v7{}, Bin)
  when is_binary(Bin), byte_size(Bin) =:= ?BLOCK_SIZE ->
    #header_star{
       header_v7=V7,
       magic=binary_part(Bin, ?USTAR_MAGIC, ?USTAR_MAGIC_LEN),
       version=binary_part(Bin, ?USTAR_VERSION, ?USTAR_VERSION_LEN),
       uname=binary_part(Bin, ?USTAR_UNAME, ?USTAR_UNAME_LEN),
       gname=binary_part(Bin, ?USTAR_GNAME, ?USTAR_GNAME_LEN),
       devmajor=binary_part(Bin, ?USTAR_DEVMAJ, ?USTAR_DEVMAJ_LEN),
       devminor=binary_part(Bin, ?USTAR_DEVMIN, ?USTAR_DEVMIN_LEN),
       prefix=binary_part(Bin, 345, 131),
       atime=binary_part(Bin, 476, 12),
       ctime=binary_part(Bin, 488, 12),
       trailer=binary_part(Bin, ?STAR_TRAILER, ?STAR_TRAILER_LEN)
      }.

to_ustar(V7=#header_v7{}, Bin)
  when is_binary(Bin), byte_size(Bin) =:= ?BLOCK_SIZE ->
    #header_ustar{
       header_v7=V7,
       magic=binary_part(Bin, ?USTAR_MAGIC, ?USTAR_MAGIC_LEN),
       version=binary_part(Bin, ?USTAR_VERSION, ?USTAR_VERSION_LEN),
       uname=binary_part(Bin, ?USTAR_UNAME, ?USTAR_UNAME_LEN),
       gname=binary_part(Bin, ?USTAR_GNAME, ?USTAR_GNAME_LEN),
       devmajor=binary_part(Bin, ?USTAR_DEVMAJ, ?USTAR_DEVMAJ_LEN),
       devminor=binary_part(Bin, ?USTAR_DEVMIN, ?USTAR_DEVMIN_LEN),
       prefix=binary_part(Bin, 345, 155)
      }.

to_sparse_array(Bin) when is_binary(Bin) ->
    MaxEntries = trunc(byte_size(Bin) / 24),
    IsExtended = not (<<0>> == binary_part(Bin, 24*MaxEntries, 1)),
    Entries = [to_sparse_entry(binary_part(Bin, N, 24)) ||
                  N <- lists:seq(0, MaxEntries-1)],
    #sparse_array{
       entries=Entries,
       max_entries=MaxEntries,
       is_extended=IsExtended
      }.

to_sparse_entry(Bin) when is_binary(Bin), byte_size(Bin) =:= 24 ->
    #sparse_entry{
       offset=binary_to_integer(binary_part(Bin, 0, 12)),
       num_bytes=binary_to_integer(binary_part(Bin, 12, 12))
      }.

-spec get_format(binary()) -> {ok, pos_integer(), header_v7()} | ?FORMAT_UNKNOWN.
get_format(Bin) when is_binary(Bin), byte_size(Bin) =:= ?BLOCK_SIZE ->
    V7 = to_v7(Bin),
    Checksum = case parse_octal(V7#header_v7.checksum) of
                   {error, _} ->
                       throw({error, invalid_tar_checksum});
                   Octal ->
                       Octal
               end,
    Chk1 = compute_checksum(Bin),
    Chk2 = compute_signed_checksum(Bin),
    if Checksum =/= Chk1 andalso Checksum =/= Chk2 ->
            ?FORMAT_UNKNOWN;
       true ->
            % guess magic
            Ustar = to_ustar(V7, Bin),
            Star = to_star(V7, Bin),
            Magic = Ustar#header_ustar.magic,
            Version = Ustar#header_ustar.version,
            Trailer = Star#header_star.trailer,
            Format = if
               Magic =:= ?MAGIC_USTAR andalso Trailer =:= ?TRAILER_STAR ->
                 ?FORMAT_STAR;
               Magic =:= ?MAGIC_USTAR ->
                 ?FORMAT_USTAR;
               Magic =:= ?MAGIC_GNU andalso Version =:= ?VERSION_GNU ->
                 ?FORMAT_GNU;
               true ->
                 ?FORMAT_V7
            end,
            {ok, Format, V7}
     end.

unpack_format(Format, V7 = #header_v7{}, Bin, Reader)
  when is_binary(Bin), byte_size(Bin) =:= ?BLOCK_SIZE ->
    Header0 = #tar_header{
                 name=parse_string(V7#header_v7.name),
                 mode=parse_numeric(V7#header_v7.mode),
                 uid=parse_numeric(V7#header_v7.uid),
                 gid=parse_numeric(V7#header_v7.gid),
                 size=parse_numeric(V7#header_v7.size),
                 mtime=posix_to_erlang_time(parse_numeric(V7#header_v7.mtime)),
                 typeflag=V7#header_v7.typeflag,
                 linkname=parse_string(V7#header_v7.linkname)
                },
    Typeflag = Header0#tar_header.typeflag,
    Header1 = if Format > ?FORMAT_V7 ->
                      Ustar = to_ustar(V7, Bin),
                      H0 = Header0#tar_header{
                        uname=parse_string(Ustar#header_ustar.uname),
                        gname=parse_string(Ustar#header_ustar.gname)},
                      H1 = if Typeflag =:= ?TYPE_CHAR
                              orelse Typeflag =:= ?TYPE_BLOCK ->
                                   Ma = parse_numeric(Ustar#header_ustar.devmajor),
                                   Mi = parse_numeric(Ustar#header_ustar.devminor),
                                   H0#tar_header{
                                     devmajor=Ma,
                                     devminor=Mi
                                    };
                              true ->
                                   H0
                            end,
                      {Prefix, H2} = case Format of
                          ?FORMAT_USTAR ->
                              {parse_string(Ustar#header_ustar.prefix), H1};
                          ?FORMAT_STAR ->
                              Star = to_star(V7, Bin),
                              Prefix0=parse_string(Star#header_star.prefix),
                              Atime0 = Star#header_star.atime,
                              Atime =posix_to_erlang_time(parse_numeric(Atime0)),
                              Ctime0 = Star#header_star.ctime,
                              Ctime =posix_to_erlang_time(parse_numeric(Ctime0)),
                              {Prefix0, H1#tar_header{
                                atime=Atime,
                                ctime=Ctime
                               }};
                          _ ->
                              {"", H1}
                      end,
                      Name = H2#tar_header.name,
                      H2#tar_header{name=safe_join_path(Prefix, Name)};
                 true ->
                      Name = Header0#tar_header.name,
                      Header0#tar_header{name=safe_join_path("", Name)}
              end,
    HeaderOnly = is_header_only_type(Typeflag),
    Header2 = if HeaderOnly ->
                      Header1#tar_header{size=0};
                 Header1#tar_header.size < 0 ->
                      throw({error, {invalid_header, negative_size}});
                 true ->
                      Header1
              end,
    if Typeflag =:= ?TYPE_GNU_SPARSE ->
            Gnu = to_gnu(V7, Bin),
            RealSize = parse_numeric(Gnu#header_gnu.real_size),
            {Sparsemap, Reader2} = parse_sparse_map(Gnu, Reader),
            {Header2, new_sparse_file_reader(Reader2, Sparsemap, RealSize)};
       true ->
            FileReader = #reg_file_reader{
                            handle = Reader,
                            num_bytes=Header2#tar_header.size,
                            size=Header2#tar_header.size,
                            pos = 0
                           },
            {Header2, FileReader}
    end.

safe_join_path([], Name) ->
    strip_slashes(Name, both);
safe_join_path(Prefix, []) ->
    strip_slashes(Prefix, right);
safe_join_path(Prefix, Name) ->
    filename:join(strip_slashes(Prefix, right), strip_slashes(Name, both)).

strip_slashes(Str, Direction) ->
    string:strip(Str, Direction, $/).

new_sparse_file_reader(_Reader, _Sparsemap, RealSize) when RealSize < 0 ->
    throw({error, invalid_sparse_header_size});
new_sparse_file_reader(Reader, Sparsemap, RealSize) ->
    true = validate_sparse_entries(Sparsemap, RealSize),
    #sparse_file_reader{
       handle = Reader,
       num_bytes = RealSize,
       pos = 0,
       size = RealSize,
       sparse_map = Sparsemap}.

validate_sparse_entries(Entries, RealSize) ->
    validate_sparse_entries(Entries, RealSize, 0, 0).
validate_sparse_entries([], _RealSize, _I, _LastOffset) ->
    true;
validate_sparse_entries([Entry=#sparse_entry{}|Rest], RealSize, I, LastOffset) ->
    Offset = Entry#sparse_entry.offset,
    NumBytes = Entry#sparse_entry.num_bytes,
    if
        Offset < 0 orelse NumBytes < 0 ->
            throw({error, invalid_sparse_map_entry});
        Offset > ?MAX_INT64-NumBytes ->
            throw({error, {invalid_sparse_map_entry, offset_too_large}});
        Offset+NumBytes > RealSize ->
            throw({error, {invalid_sparse_map_entry, offset_too_large}});
        I > 0 andalso LastOffset > Offset ->
            throw({error, {invalid_sparse_map_entry, overlapping_offsets}});
        true ->
            ok
    end,
    validate_sparse_entries(Rest, RealSize, I+1, Offset+NumBytes).


parse_sparse_map(Gnu=#header_gnu{sparse=Sparse}, Reader)
  when Sparse#sparse_array.is_extended ->
    parse_sparse_map(Gnu, Reader, []);
parse_sparse_map(#header_gnu{sparse=Sparse}, Reader) ->
    {Sparse#sparse_array.entries, Reader}.
parse_sparse_map(#sparse_array{is_extended=true,entries=Entries}, Reader, Acc) ->
    case read_block(Reader) of
        eof ->
            throw({error, eof});
        {ok, Block, Reader2} ->
            Sparse2 = to_sparse_array(Block),
            parse_sparse_map(Sparse2, Reader2, Entries++Acc)
    end;
parse_sparse_map(#sparse_array{entries=Entries}, Reader, Acc) ->
    {Entries ++ Acc, Reader}.

% Defined by taking the sum of the unsigned byte values of the
% entire header record, treating the checksum bytes to as ASCII spaces
compute_checksum(<<H1:?V7_CHKSUM/binary,
                   H2:?V7_CHKSUM_LEN/binary,
                   Rest:(?BLOCK_SIZE - ?V7_CHKSUM - ?V7_CHKSUM_LEN)/binary,
                   _/binary>>) ->
    C0 = checksum(H1) + (byte_size(H2) * $\s),
    C1 = checksum(Rest),
    C0 + C1.

compute_signed_checksum(<<H1:?V7_CHKSUM/binary,
                          H2:?V7_CHKSUM_LEN/binary,
                          Rest:(?BLOCK_SIZE - ?V7_CHKSUM - ?V7_CHKSUM_LEN)/binary,
                          _/binary>>) ->
    C0 = signed_checksum(H1) + (byte_size(H2) * $\s),
    C1 = signed_checksum(Rest),
    C0 + C1.

%% Returns the checksum of a binary.
checksum(Bin) -> checksum(Bin, 0).
checksum(<<A/unsigned,Rest/binary>>, Sum) ->
    checksum(Rest, Sum+A);
checksum(<<>>, Sum) -> Sum.

signed_checksum(Bin) -> signed_checksum(Bin, 0).
signed_checksum(<<A/signed,Rest/binary>>, Sum) ->
    signed_checksum(Rest, Sum+A);
signed_checksum(<<>>, Sum) -> Sum.

parse_numeric(<<>>) ->
    0;
parse_numeric(<<First, _/binary>> = Bin) ->
    % check for base-256 format first
    % if the bit is set, then all following bits constitute a two's
    % complement encoded number in big-endian byte order
    if
        First band 16#80 =/= 0 ->
            % Handling negative numbers relies on the following identity:
            %     -a-1 == ^a
            % If the number is negative, we use an inversion mask to invert
            % the data bytes and treat the value as an unsigned number
            Inv = if First band 16#40 =/= 0 -> 16#00; true -> 16#FF end,
            Bytes = binary:bin_to_list(Bin),
            {_, N} = lists:foldl(fun (C, {I, X}) ->
                                C1 = C bxor Inv,
                                C2 = if I =:= 0 -> C1 band 16#7F; true -> C1 end,
                                if (X bsr 56) > 0 ->
                                     throw({error,integer_overflow});
                                   true ->
                                     {I+1, (X bsl 8) bor C2}
                                end
                        end, {0, 0}, Bytes),
            if (N bsr 63) > 0 ->
                  throw({error, integer_overflow});
               true ->
                  if Inv == 16#FF ->
                    -1 bxor N;
                  true ->
                    N
                  end
             end;
        true ->
            % normal case is an octal number
            parse_octal(Bin)
    end.

parse_octal(Bin) when is_binary(Bin) ->
    % skip leading/trailing zero bytes and spaces
    do_parse_octal(Bin, <<>>).
do_parse_octal(<<>>, <<>>) ->
    0;
do_parse_octal(<<>>, Acc) ->
    case io_lib:fread("~8u", binary:bin_to_list(Acc)) of
        {error, _} = Err -> Err;
        {ok, [Octal], []} -> Octal;
        {ok, _, _} -> throw({error, bad_header})
    end;
do_parse_octal(<<$\s,Rest/binary>>, Acc) ->
    do_parse_octal(Rest, Acc);
do_parse_octal(<<0, Rest/binary>>, Acc) ->
    do_parse_octal(Rest, Acc);
do_parse_octal(<<C, Rest/binary>>, Acc) ->
    do_parse_octal(Rest, <<Acc/binary, C>>).

parse_string(Bin) when is_binary(Bin) ->
    do_parse_string(Bin, <<>>).
do_parse_string(<<>>, Acc) ->
    unicode:characters_to_list(Acc);
do_parse_string(<<0, _/binary>>, Acc) ->
    unicode:characters_to_list(Acc);
do_parse_string(<<C, Rest/binary>>, Acc) ->
    do_parse_string(Rest, <<Acc/binary, C>>).


convert_header(Bin, Reader = #reader{pos=Pos})
  when byte_size(Bin) =:= ?BLOCK_SIZE, (Pos rem ?BLOCK_SIZE) =:= 0 ->
    case get_format(Bin) of
        ?FORMAT_UNKNOWN ->
            throw({error, bad_header});
        {ok, Format, V7} ->
            unpack_format(Format, V7, Bin, Reader)
    end;
convert_header(Bin, #reader{pos=Pos}) when byte_size(Bin) =:= ?BLOCK_SIZE ->
    throw({error, misaligned_read, Pos});
convert_header(Bin, _Reader) when byte_size(Bin) =:= 0 ->
    eof;
convert_header(_Bin, _Reader) ->
    throw({error, eof}).

%% Creates a partially-populated header record based
%% on the provided file_info record. If the file is
%% a symlink, then `link` is used as the link target.
%% If the file is a directory, a slash is appended to the name.
-spec fileinfo_to_header(string(), file:file_info(), string()) ->
                                tar_header() | {error, term()}.
fileinfo_to_header(Name, Fi = #file_info{}, Link) ->
    BaseHeader = #tar_header{name=Name,
                         mtime=Fi#file_info.mtime,
                         atime=Fi#file_info.atime,
                         ctime=Fi#file_info.ctime,
                         mode=Fi#file_info.mode,
                         uid=Fi#file_info.uid,
                         gid=Fi#file_info.gid},
    do_fileinfo_to_header(BaseHeader, Fi, Link).

do_fileinfo_to_header(Header, #file_info{size=Size,type=regular}, _Link) ->
    Header#tar_header{size=Size,typeflag=?TYPE_REGULAR};
do_fileinfo_to_header(Header = #tar_header{name=Name},
                      #file_info{type=directory}, _Link) ->
    Header#tar_header{name=Name++"/",typeflag=?TYPE_DIR};
do_fileinfo_to_header(Header, #file_info{type=symlink}, Link) ->
    Header#tar_header{typeflag=?TYPE_SYMLINK,linkname=Link};
do_fileinfo_to_header(Header, Fi = #file_info{type=device,mode=Mode}, _Link)
  when (Mode band ?S_IFMT) =:= ?S_IFCHR ->
    Header#tar_header{typeflag=?TYPE_CHAR,
            devmajor=Fi#file_info.major_device,
            devminor=Fi#file_info.minor_device};
do_fileinfo_to_header(Header, Fi = #file_info{type=device,mode=Mode}, _Link)
  when (Mode band ?S_IFMT) =:= ?S_IFBLK ->
    Header#tar_header{typeflag=?TYPE_BLOCK,
            devmajor=Fi#file_info.major_device,
            devminor=Fi#file_info.minor_device};
do_fileinfo_to_header(Header, Fi = #file_info{type=other,mode=Mode}, _Link)
  when (Mode band ?S_IFMT) =:= ?S_FIFO ->
    Header#tar_header{typeflag=?TYPE_FIFO,
            devmajor=Fi#file_info.major_device,
            devminor=Fi#file_info.minor_device};
do_fileinfo_to_header(Header, Fi, _Link) ->
    {error, {invalid_file_type, Header#tar_header.name, Fi}}.

is_ascii(Str) when is_list(Str) ->
    not lists:any(fun (Char) -> Char >= 16#80 end, Str);
is_ascii(<<>>) ->
    true;
is_ascii(<<C,_Rest/binary>>) when C >= 16#80 ->
    false;
is_ascii(<<_, Rest/binary>>) ->
    is_ascii(Rest).

to_ascii(Str) when is_list(Str) ->
    case is_ascii(Str) of
        true ->
            Str;
        false ->
            lists:filter(fun (Char) -> Char < 16#80 end, Str)
    end;
to_ascii(Bin) when is_binary(Bin) ->
    to_ascii(Bin, <<>>).
to_ascii(<<>>, Acc) ->
    Acc;
to_ascii(<<C, Rest/binary>>, Acc) when C < 16#80 ->
    to_ascii(Rest, <<Acc/binary,C>>);
to_ascii(<<_, Rest/binary>>, Acc) ->
    to_ascii(Rest, Acc).

is_header_only_type(?TYPE_SYMLINK) -> true;
is_header_only_type(?TYPE_LINK)    -> true;
is_header_only_type(?TYPE_CHAR)    -> true;
is_header_only_type(?TYPE_BLOCK)   -> true;
is_header_only_type(?TYPE_DIR)     -> true;
is_header_only_type(?TYPE_FIFO)    -> true;
is_header_only_type(_) -> false.

posix_to_erlang_time(Sec) ->
    OneMillion = 1000000,
    Time = calendar:now_to_datetime({Sec div OneMillion, Sec rem OneMillion, 0}),
    erlang:universaltime_to_localtime(Time).

foldl_read(Reader=#reader{access=read}, Fun, Accu, Opts) ->
    foldl_read0(Reader, Fun, Accu, Opts);
foldl_read(#reader{access=AccessMode}, _Fun, _Accu, _Opts) ->
    {error,{read_mode_expected,AccessMode}};
foldl_read(TarName, Fun, Accu, Opts) ->
    case open(TarName, [read|Opts#read_opts.open_mode]) of
	{ok, Reader=#reader{access=read}} ->
	    case foldl_read0(Reader, Fun, Accu, Opts) of
                {ok, Result, Reader2} ->
                    ok = do_close(Reader2),
                    Result;
                 {error, _} = Err ->
                    Err
            end;
	Error ->
	    Error
    end.

foldl_read0(Reader, Fun, Accu, Opts) ->
    case catch foldl_read1(Fun, Accu, Reader, Opts, #{}) of
	{'EXIT', Reason} ->
	    exit(Reason);
	{error, {Reason, Format, Args}} ->
	    read_verbose(Opts, Format, Args),
	    {error, Reason};
	{error, Reason} ->
	    {error, Reason};
	Ok ->
	    Ok
    end.

foldl_read1(Fun, Accu0, Reader, Opts, ExtraHeaders) ->
    {ok, Reader1} = skip_unread(Reader),
    case get_header(Reader1) of
        eof ->
            Fun(eof, Reader1, Opts, Accu0);
        {Header, Reader2} ->
            case Header#tar_header.typeflag of
                ?TYPE_X_HEADER ->
                    {ExtraHeaders2, Reader3} = parse_pax(Reader2),
                    ExtraHeaders3 = maps:merge(ExtraHeaders, ExtraHeaders2),
                    foldl_read1(Fun, Accu0, Reader3, Opts, ExtraHeaders3);
                ?TYPE_GNU_LONGNAME ->
                    {RealName, Reader3} = get_real_name(Reader2),
                    ExtraHeaders2 = maps:put(?PAX_PATH,
                                             parse_string(RealName), ExtraHeaders),
                    foldl_read1(Fun, Accu0, Reader3, Opts, ExtraHeaders2);
                ?TYPE_GNU_LONGLINK ->
                    {RealName, Reader3} = get_real_name(Reader2),
                    ExtraHeaders2 = maps:put(?PAX_LINKPATH,
                                             parse_string(RealName), ExtraHeaders),
                    foldl_read1(Fun, Accu0, Reader3, Opts, ExtraHeaders2);
                _ ->
                    Header1 = merge_pax(Header, ExtraHeaders),
                    {Reader3,Header2} = get_file_reader(Reader2,Header1,ExtraHeaders),
                    {ok, NewAccu, Reader4} = Fun(Header2, Reader3, Opts, Accu0),
                    foldl_read1(Fun, NewAccu, Reader4, Opts, #{})
                end
    end.

% Checks for PAX format sparse headers and uses a
% sparse_file_reader if this is a PAX format sparse file, otherwise it
% uses the plain reader.
get_file_reader(Reader, Header,
      #{?PAX_GNU_SPARSE_MAJOR:=Major,?PAX_GNU_SPARSE_MINOR:=Minor} = Extra) ->
    SparseFormat = <<Major/binary,$.,Minor/binary>>,
    do_get_file_reader(Reader, Header, SparseFormat, Extra);
get_file_reader(Reader, Header,
      #{?PAX_GNU_SPARSE_NAME:=_SparseName,?PAX_GNU_SPARSE_MAP:=_SparseMap}=Extra) ->
    SparseFormat = <<"0.1">>,
    do_get_file_reader(Reader, Header, SparseFormat, Extra);
get_file_reader(Reader, Header, #{?PAX_GNU_SPARSE_SIZE:=_SparseSize} = Extra) ->
    SparseFormat = <<"0.0">>,
    do_get_file_reader(Reader, Header, SparseFormat, Extra);
get_file_reader(Reader, Header, _ExtraHeaders) ->
    do_get_file_reader(Reader, Header, false, false).
do_get_file_reader(Reader=#reg_file_reader{}, Header, false, false) ->
    {Reader, Header};
do_get_file_reader(Reader=#sparse_file_reader{handle=Reader}, Header,false,false) ->
    % unknown sparse format, so treat as a regular file
    NumBytes = Reader#sparse_file_reader.num_bytes,
    Size = Reader#sparse_file_reader.size,
    Pos = Reader#sparse_file_reader.pos,
    {#reg_file_reader{handle=Reader,pos=Pos,size=Size,num_bytes=NumBytes}, Header};
do_get_file_reader(Reader, Header, <<"1.0">>, Extra) ->
    SparseName = get_sparse_name(Extra, Header#tar_header.name),
    SparseSize = get_sparse_size(Extra, Header#tar_header.size),
    Header1 = Header#tar_header{name=SparseName,size=SparseSize},
    {SparseArray, Reader2} = read_gnu_sparsemap_1_0(Reader),
    {Header1, to_sparse_file_reader(Reader2, SparseSize, SparseArray)};
do_get_file_reader(Reader, Header, Format, Extra)
  when Format =:= <<"0.0">> orelse Format =:= <<"0.1">> ->
    SparseName = get_sparse_name(Extra, Header#tar_header.name),
    SparseSize = get_sparse_size(Extra, Header#tar_header.size),
    Header1 = Header#tar_header{name=SparseName,size=SparseSize},
    SparseArray = read_gnu_sparsemap_0_1(Extra, Format),
    {Header1, to_sparse_file_reader(Reader, SparseSize, SparseArray)}.

%% Reads the sparse map as stored in GNU's PAX sparse format version 1.0.
%% The format of the sparse map consists of a series of newline-terminated numeric
%% fields. The first field is the number of entries and is always present. Following
%% this are the entries, consisting of two fields (offset, num_bytes). This function must
%% stop reading at the end boundary of the block containing the last newline.
%%
%% NOTE: The GNU manual says that numeric values should be encoded in octal format.
%% However, the GNU tar utility itself outputs these values in decimal. As such, we
%% treat values as being encoded in decimal.
-spec read_gnu_sparsemap_1_0(reader_type()) -> {sparse_array(), reader_type()}.
read_gnu_sparsemap_1_0(Reader) ->
    case feed_tokens(Reader, 1) of
        eof ->
            throw({error, eof});
        {ok, Reader2, Bin} ->
            case binary:split(Bin, [<<$\n>>]) of
                [Token,Bin2] ->
                    NEntries = binary_to_integer(Token),
                    if NEntries < 0 ->
                            throw({error, invalid_gnu_1_0_sparsemap});
                       true ->
                            ok
                    end,
                    % parse all member entries
                    case feed_tokens(Reader2, 2*NEntries) of
                        {ok, Reader3, Bin3} ->
                            Bin4 = <<Bin2/binary,Bin3/binary>>,
                            read_gnu_sparsemap_1_0_entries(Reader3, NEntries, Bin4);
                        _ ->
                            throw({error, invalid_gnu_1_0_sparsemap})
                    end;
                _ ->
                    throw({error, invalid_gnu_1_0_sparsemap})
             end
    end.
read_gnu_sparsemap_1_0_entries(Reader, NumEntries, Bin) ->
    read_gnu_sparsemap_1_0_entries(Reader, NumEntries, Bin, #sparse_array{}).
read_gnu_sparsemap_1_0_entries(Reader, 0, _Bin, Acc) ->
    {Acc, Reader};
read_gnu_sparsemap_1_0_entries(Reader, NumEntries, Bin,
                               Acc=#sparse_array{entries=Entries}) ->
    case binary:split(Bin, [<<$\n>>]) of
        [OffsetToken, Bin2] ->
            case binary:split(Bin2, [<<$\n>>]) of
                [NumBytesToken, Bin3] ->
                    Offset = binary_to_integer(OffsetToken),
                    NumBytes = binary_to_integer(NumBytesToken),
                    Entry = #sparse_entry{offset=Offset,num_bytes=NumBytes},
                    Acc2 = Acc#sparse_array{entries=[Entry|Entries]},
                    read_gnu_sparsemap_1_0_entries(Reader, NumEntries-1,Bin3,Acc2);
                _ ->
                    throw({error, invalid_gnu_1_0_sparsemap})
            end;
        _ ->
            throw({error, invalid_gnu_1_0_sparsemap})
    end.


% Copies data in ?BLOCK_SIZE chunks from Reader into a
% buffer until there are at least Count newlines in the buffer.
% It will not read more blocks than needed.
feed_tokens(Reader, Count) ->
    feed_tokens(Reader, Count, <<>>).
feed_tokens(Reader, 0, Buffer) ->
    {Reader, Buffer};
feed_tokens(Reader, Count, Buffer) ->
    case do_read(Reader, ?BLOCK_SIZE) of
        {ok, Bin, Reader2} ->
            Buffer2 = <<Buffer/binary,Bin/binary>>,
            Newlines = count_newlines(Buffer2),
            feed_tokens(Reader2, Count-Newlines, Buffer2);
        Err ->
            throw(Err)
    end.

count_newlines(<<>>) -> 0;
count_newlines(Bin)  -> count_newlines(Bin, 0).
count_newlines(<<>>, Count) ->
    Count;
count_newlines(<<$\n, Bin/binary>>, Count) ->
    count_newlines(Bin, Count+1);
count_newlines(<<_C, Bin/binary>>, Count) ->
    count_newlines(Bin, Count).


%% Reads the sparse map as stored in GNU's PAX sparse format version 0.1.
%% The sparse map is stored in the PAX headers.
-spec read_gnu_sparsemap_0_1(map(), binary()) -> sparse_array().
read_gnu_sparsemap_0_1(#{?PAX_GNU_SPARSE_NUMBLOCKS:=NumEntriesStr,
                         ?PAX_GNU_SPARSE_MAP:=SparseMap}, Format) ->
    NumEntries = binary_to_integer(NumEntriesStr),
    if NumEntries < 0 orelse (2*NumEntries) < NumEntries ->
       throw({error, {malformed_gnu_0_1_sparsemap, Format}});
       true -> ok
    end,
    case binary:split(SparseMap, [<<",">>], [global]) of
        Entries when length(Entries) =:= (2*NumEntries) ->
            parse_gnu_sparsemap_0_1(Entries);
        _ ->
            throw({error, {malformed_gnu_0_1_sparsemap, Format}})
    end.

parse_gnu_sparsemap_0_1([]) ->
    #sparse_array{};
parse_gnu_sparsemap_0_1(Entries) ->
    Entries = parse_gnu_sparsemap_0_1(Entries, []),
    #sparse_array{entries=Entries}.
parse_gnu_sparsemap_0_1([], Acc) ->
    lists:reverse(Acc);
parse_gnu_sparsemap_0_1([OffsetStr, NumBytesStr|Rest], Acc) ->
    Offset=binary_to_integer(OffsetStr),
    NumBytes=binary_to_integer(NumBytesStr),
    Entry = #sparse_entry{offset=Offset,num_bytes=NumBytes},
    parse_gnu_sparsemap_0_1(Rest, [Entry|Acc]).

to_sparse_file_reader(Reader=#sparse_file_reader{}, Size, SparseEntries) ->
    Reader#sparse_file_reader{
      num_bytes = Size,
      size = Size,
      sparse_map = SparseEntries};
to_sparse_file_reader(#reg_file_reader{handle=Reader}, Size, SparseEntries) ->
    #sparse_file_reader{
       handle=Reader,
       num_bytes=Size,
       sparse_map=SparseEntries}.


get_sparse_name(#{?PAX_GNU_SPARSE_NAME:=SparseName}, _Default) ->
    parse_string(SparseName);
get_sparse_name(_, Default) ->
    Default.
get_sparse_size(#{?PAX_GNU_SPARSE_SIZE:=SparseSize}, _Default) ->
    binary_to_integer(SparseSize);
get_sparse_size(#{?PAX_GNU_SPARSE_REALSIZE:=SparseSize}, _Default) ->
    binary_to_integer(SparseSize);
get_sparse_size(_, Default) ->
    Default.


%% Applies all known PAX attributes to the current tar header
-spec merge_pax(tar_header(), map()) -> tar_header().
merge_pax(Header, ExtraHeaders) when is_map(ExtraHeaders) ->
    merge_pax(Header, maps:to_list(ExtraHeaders));
merge_pax(Header, []) ->
    Header;
merge_pax(Header, [{?PAX_PATH, Path}|Rest]) ->
    merge_pax(Header#tar_header{name=unicode:characters_to_list(Path)}, Rest);
merge_pax(Header, [{?PAX_LINKPATH, LinkPath}|Rest]) ->
    merge_pax(Header#tar_header{linkname=unicode:characters_to_list(LinkPath)}, Rest);
merge_pax(Header, [{?PAX_GNAME, Gname}|Rest]) ->
    merge_pax(Header#tar_header{gname=unicode:characters_to_list(Gname)}, Rest);
merge_pax(Header, [{?PAX_UNAME, Uname}|Rest]) ->
    merge_pax(Header#tar_header{uname=unicode:characters_to_list(Uname)}, Rest);
merge_pax(Header, [{?PAX_UID, Uid}|Rest]) ->
    Uid2 = binary_to_integer(Uid),
    merge_pax(Header#tar_header{uid=Uid2}, Rest);
merge_pax(Header, [{?PAX_GID, Gid}|Rest]) ->
    Gid2 = binary_to_integer(Gid),
    merge_pax(Header#tar_header{gid=Gid2}, Rest);
merge_pax(Header, [{?PAX_ATIME, Atime}|Rest]) ->
    Atime2 = parse_pax_time(Atime),
    merge_pax(Header#tar_header{atime=Atime2}, Rest);
merge_pax(Header, [{?PAX_MTIME, Mtime}|Rest]) ->
    Mtime2 = parse_pax_time(Mtime),
    merge_pax(Header#tar_header{mtime=Mtime2}, Rest);
merge_pax(Header, [{?PAX_CTIME, Ctime}|Rest]) ->
    Ctime2 = parse_pax_time(Ctime),
    merge_pax(Header#tar_header{ctime=Ctime2}, Rest);
merge_pax(Header, [{?PAX_SIZE, Size}|Rest]) ->
    Size2 = binary_to_integer(Size),
    merge_pax(Header#tar_header{size=Size2}, Rest);
merge_pax(Header, [{<<?PAX_XATTR_STR, Key/binary>>, Value}|Rest]) ->
    Xattrs2 = maps:put(Key, Value, Header#tar_header.xattrs),
    merge_pax(Header#tar_header{xattrs=Xattrs2}, Rest);
merge_pax(Header, [_Ignore|Rest]) ->
    merge_pax(Header, Rest).

%% Returns the time since UNIX epoch as a datetime
-spec parse_pax_time(binary()) -> calendar:datetime().
parse_pax_time(Bin) when is_binary(Bin) ->
    TotalNano = case binary:split(Bin, [<<$.>>]) of
        [SecondsStr, NanoStr] ->
            Seconds = binary_to_integer(SecondsStr),
            if byte_size(NanoStr) < ?MAX_NANO_INT_SIZE ->
                    % right pad
                    PaddingN = ?MAX_NANO_INT_SIZE-byte_size(NanoStr),
                    Padding = binary:copy(<<0>>, PaddingN),
                    NanoStr2 = <<NanoStr/binary,Padding/binary>>,
                    Nano = binary_to_integer(NanoStr2),
                    (Seconds*?BILLION)+Nano;
               byte_size(NanoStr) > ?MAX_NANO_INT_SIZE ->
                    % right truncate
                    NanoStr2 = binary_part(NanoStr, 0, ?MAX_NANO_INT_SIZE),
                    Nano = binary_to_integer(NanoStr2),
                    (Seconds*?BILLION)+Nano;
               true ->
                    (Seconds*?BILLION)+binary_to_integer(NanoStr)
            end;
        [SecondsStr] ->
            binary_to_integer(SecondsStr)*?BILLION
    end,
    % truncate to microseconds
    Micro = TotalNano div 1000,
    Mega = Micro div 1000000000000,
    Secs = Micro div 1000000 - (Mega*1000000),
    Micro2 = Micro rem 1000000,
    calendar:now_to_datetime({Mega, Secs, Micro2}).

%% Given a sparse or regular file reader, reads the whole file and
%% parses all extended attributes it contains.
-spec parse_pax(sparse_file_reader() | reg_file_reader()) ->
                       {tar_header(), reader()}.
parse_pax(#sparse_file_reader{handle=Handle,num_bytes=0}) ->
    {#{}, Handle};
parse_pax(#sparse_file_reader{handle=Handle,num_bytes=NumBytes}) ->
    case do_read(Handle, NumBytes) of
        {ok, Bytes, Handle2} ->
            % for GNU PAX sparse format 0.0 support
            % this function transforms the sparse format 0.0 headers
            % into sparse format 0.1 headers
            do_parse_pax(Handle2, Bytes, <<>>, #{});
        {error, _} = Err ->
            throw(Err)
    end;
parse_pax(#reg_file_reader{handle=Handle,num_bytes=0}) ->
    {#{}, Handle};
parse_pax(#reg_file_reader{handle=Handle,num_bytes=NumBytes}) ->
    case do_read(Handle, NumBytes) of
        {ok, Bytes, Handle2} ->
            % for GNU PAX sparse format 0.0 support
            % this function transforms the sparse format 0.0 headers
            % into sparse format 0.1 headers
            do_parse_pax(Handle2, Bytes, <<>>, #{});
        {error, _} = Err ->
            throw(Err)
    end.

do_parse_pax(Reader, <<>>, Sparsemap, Headers) when byte_size(Sparsemap) > 0 ->
    % truncate comma
    Sparsemap2 = binary_part(Sparsemap, 0, byte_size(Sparsemap) - 1),
    Headers2 = maps:put(?PAX_GNU_SPARSE_MAP, Sparsemap2, Headers),
    {Headers2, Reader};
do_parse_pax(Reader, <<>>, _Sparsemap, Headers) ->
    {Headers, Reader};
do_parse_pax(Reader, Bin, Sparsemap, Headers) ->
    {Key, Value, Residual} = parse_pax_record(Bin),
    if Key =:= ?PAX_GNU_SPARSE_OFFSET orelse Key =:= ?PAX_GNU_SPARSE_NUMBYTES ->
        % GNU sparse format 0.0 special key, write to sparse map instead of headers map
        Sparsemap2 = <<Sparsemap/binary,Value/binary,$,>>,
        do_parse_pax(Reader, Residual, Sparsemap2, Headers);
       true ->
        NewHeaders = maps:put(Key, Value, Headers),
        do_parse_pax(Reader, Residual, Sparsemap, NewHeaders)
    end.

%% Parse an extended attribute
parse_pax_record(Bin) when is_binary(Bin) ->
    case binary:split(Bin, [<<$\n>>]) of
        [Record, Residual] ->
            case binary:split(Record, [<<$\s>>], [trim_all]) of
                [_Len, Record1] ->
                    case binary:split(Record1, [<<$=>>], [trim_all]) of
                        [AttrName, AttrValue] ->
                            {AttrName, AttrValue, Residual};
                        _Other ->
                            throw({error, malformed_pax_record})
                    end;
                _Other ->
                    throw({error, malformed_pax_record})
            end;
        _Other ->
            throw({error, malformed_pax_record})
    end.

get_real_name(#reg_file_reader{handle=Handle,num_bytes=0}) ->
    {"", Handle};
get_real_name(#reg_file_reader{handle=Handle,num_bytes=NumBytes}) ->
    case do_read(Handle, NumBytes) of
        {ok, RealName, Handle2} ->
            {RealName, Handle2};
        {error, _} = Err ->
            throw(Err)
    end;
get_real_name(Reader = #sparse_file_reader{num_bytes=NumBytes}) ->
    case do_read(Reader, NumBytes) of
        {ok, RealName, Reader2} ->
            {RealName, Reader2};
        {error, _} = Err ->
            throw(Err)
    end.

%% Skip the remaining bytes for the current file entry
skip_file(Reader=#reg_file_reader{handle=Handle,pos=Pos,size=Size}) ->
    Padding = skip_padding(Size),
    AbsPos = Handle#reader.pos + (Size-Pos) + Padding,
    case do_position(Handle, AbsPos) of
        {ok, _, Handle2} ->
            Reader#reg_file_reader{handle=Handle2,num_bytes=0,pos=Size};
         Err ->
            throw(Err)
    end;
skip_file(Reader=#sparse_file_reader{handle=Handle,pos=Pos,size=Size}) ->
    Padding = skip_padding(Size),
    AbsPos = Handle#reader.pos + (Size-Pos) + Padding,
    case do_position(Handle, AbsPos) of
        {ok, _, Handle2} ->
            Reader#sparse_file_reader{handle=Handle2,num_bytes=0,pos=Size};
        Err ->
            throw(Err)
    end.

skip_padding(0) ->
    0;
skip_padding(Size) when (Size rem ?BLOCK_SIZE) =:= 0 ->
    0;
skip_padding(Size) when Size =< ?BLOCK_SIZE ->
    ?BLOCK_SIZE - Size;
skip_padding(Size) ->
    ?BLOCK_SIZE - (Size rem ?BLOCK_SIZE).

skip_unread(Reader=#reader{pos=Pos}) when (Pos rem ?BLOCK_SIZE) > 0 ->
    Padding = skip_padding(Pos + ?BLOCK_SIZE),
    AbsPos = Pos + Padding,
    case do_position(Reader, AbsPos) of
        {ok, _, Reader2} ->
            {ok, Reader2};
        Err ->
            throw(Err)
    end;
skip_unread(Reader=#reader{}) ->
    {ok, Reader};
skip_unread(#reg_file_reader{handle=Handle,num_bytes=0}) ->
    skip_unread(Handle);
skip_unread(Reader = #reg_file_reader{}) ->
    #reg_file_reader{handle=Handle} = skip_file(Reader),
    {ok, Handle};
skip_unread(#sparse_file_reader{handle=Handle,num_bytes=0}) ->
    skip_unread(Handle);
skip_unread(Reader = #sparse_file_reader{}) ->
    #sparse_file_reader{handle=Handle} = skip_file(Reader),
    {ok, Handle}.

write_extracted_element(#tar_header{name=Name,typeflag=Type},
                        Bin,
                        Opts=#read_opts{output=memory}) ->
    case typeflag(Type) of
        regular ->
            read_verbose(Opts, "x ~ts~n", [Name]),
            {ok, {Name, Bin}};
        _ ->
            ok
    end;
write_extracted_element(Header=#tar_header{name=Name}, Bin, Opts) ->
    Name1 = filename:absname(Name, Opts#read_opts.cwd),
    Created =
	case typeflag(Header#tar_header.typeflag) of
	    regular ->
		case write_extracted_file(Name1, Bin, Opts) of
                    not_written ->
                        read_verbose(Opts, "x ~ts - exists, not created~n", [Name]),
                        not_written;
                    Ok ->
                        read_verbose(Opts, "x ~ts~n", [Name]),
                        Ok
                end;
	    directory ->
                read_verbose(Opts, "x ~ts~n", [Name]),
		create_extracted_dir(Name1, Opts);
	    symlink ->
                read_verbose(Opts, "x ~ts~n", [Name]),
		create_symlink(Name1, Header#tar_header.linkname, Opts);
	    Other -> % Ignore.
		read_verbose(Opts, "x ~ts - unsupported type ~p~n",
			     [Name, Other]),
		not_written
	end,
    case Created of
	ok  -> set_extracted_file_info(Name1, Header);
	not_written -> ok
    end.

create_extracted_dir(Name, _Opts) ->
    case file:make_dir(Name) of
	ok -> ok;
	{error,enotsup} -> not_written;
	{error,eexist} -> not_written;
	{error,enoent} -> make_dirs(Name, dir);
	{error,Reason} -> throw({error, Reason})
    end.

create_symlink(Name, Linkname, Opts) ->
    case file:make_symlink(Linkname, Name) of
	ok -> ok;
	{error,enoent} ->
	    ok = make_dirs(Name, file),
	    create_symlink(Name, Linkname, Opts);
	{error,eexist} -> not_written;
	{error,enotsup} ->
	    read_verbose(Opts, "x ~ts - symbolic links not supported~n", [Name]),
	    not_written;
	{error,Reason} -> throw({error, Reason})
    end.

write_extracted_file(Name, Bin, Opts) ->
    Write =
	case Opts#read_opts.keep_old_files of
	    true ->
		case file:read_file_info(Name) of
		    {ok, _} -> false;
		    _ -> true
		end;
	    false -> true
	end,
    case Write of
	true  -> write_file(Name, Bin);
	false -> not_written
    end.

write_file(Name, Bin) ->
    case file:write_file(Name, Bin) of
	ok -> ok;
	{error,enoent} ->
	    ok = make_dirs(Name, file),
	    write_file(Name, Bin);
	{error,Reason} ->
	    throw({error, Reason})
    end.

set_extracted_file_info(_, #tar_header{typeflag = ?TYPE_SYMLINK}) -> ok;
set_extracted_file_info(_, #tar_header{typeflag = ?TYPE_LINK}) -> ok;
set_extracted_file_info(Name, #tar_header{mtime=Mtime,mode=Mode}) ->
    Info = #file_info{mode=Mode, mtime=Mtime},
    file:write_file_info(Name, Info).

%% Makes all directories leading up to the file.

make_dirs(Name, file) ->
	filelib:ensure_dir(Name);
make_dirs(Name, dir) ->
	filelib:ensure_dir(filename:join(Name,"*")).

%% Prints the message on if the verbose option is given (for reading).
read_verbose(#read_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args);
read_verbose(_, _, _) ->
    ok.

%% Prints the message on if the verbose option is given.
add_verbose(#add_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args);
add_verbose(_, _, _) ->
    ok.

%%%%%%%%%%%%%%%%%%
%% I/O primitives
%%%%%%%%%%%%%%%%%%

do_write(Reader = #reader{handle=Handle,func=Fun}, Data)
  when is_function(Fun,2) ->
    case Fun(write,{Handle,Data}) of
        ok ->
            {ok, Pos, Reader2} = do_position(Reader, {cur,0}),
            {ok, Reader2#reader{pos=Pos}};
        {error, _} = Err ->
            Err
    end.

do_copy(Reader = #reader{handle=Handle,pos=Pos,func=Fun}, Source)
  when is_function(Fun, 2) ->
    % TODO: Make this use Fun
    {ok, BytesCopied} = file:copy(Source, Handle),
    {ok, BytesCopied, Reader#reader{pos=Pos+BytesCopied}}.

do_position(Reader = #reader{handle=Handle,func=Fun}, Pos)
  when is_function(Fun,2)->
    case Fun(position, {Handle,Pos}) of
        {ok, NewPos} ->
            % since Pos may not always be an absolute seek,
            % make sure we update the reader with the new absolute position
            {ok, AbsPos} = Fun(position, {Handle, {cur, 0}}),
            {ok, NewPos, Reader#reader{pos=AbsPos}};
        Other ->
            Other
    end.

do_read(Reader = #reg_file_reader{handle=Handle,pos=Pos,size=Size}, Len) ->
    NumBytes = Size - Pos,
    ActualLen = if NumBytes - Len < 0 -> NumBytes; true -> Len end,
    case do_read(Handle, ActualLen) of
        {ok, Bin, Handle2} ->
            NewPos = Pos + ActualLen,
            NumBytes2 = Size - NewPos,
            {ok, Bin, Reader#reg_file_reader{handle=Handle2,pos=NewPos,num_bytes=NumBytes2}};
        Other ->
            Other
    end;
do_read(Reader = #sparse_file_reader{}, Len) ->
    do_sparse_read(Reader, Len);
do_read(Reader = #reader{pos=Pos,handle=Handle,func=Fun}, Len)
  when is_function(Fun,2)->
    %% Always convert to binary internally
    case Fun(read2,{Handle,Len}) of
        {ok, List} when is_list(List) ->
            Bin = list_to_binary(List),
            NewPos = Pos+byte_size(Bin),
            {ok, Bin, Reader#reader{pos=NewPos}};
        {ok, Bin} when is_binary(Bin) ->
            NewPos = Pos+byte_size(Bin),
            {ok, Bin, Reader#reader{pos=NewPos}};
        Other ->
            Other
    end.

do_sparse_read(Reader=#sparse_file_reader{sparse_map=#sparse_array{
                entries=[#sparse_entry{num_bytes=0}|Entries]}}, Len) ->
    % skip all empty fragments
    SparseMap = Reader#sparse_file_reader.sparse_map#sparse_array{entries=Entries},
    Reader2 = Reader#sparse_file_reader{sparse_map=SparseMap},
    do_sparse_read(Reader2, Len);
do_sparse_read(Reader = #sparse_file_reader{pos=Pos,sparse_map=#sparse_array{
                entries=[]},size=Size}, Len)
  when Pos < Size ->
    % if there are no more fragments, it is possible that there is one last sparse hole
    % this behaviour matches the BSD tar utility
    % however, GNU tar stops returning data even if we haven't reached the end
    read_sparse_hole(Reader, Size, Len);
do_sparse_read(#sparse_file_reader{sparse_map=#sparse_array{entries=[]}}, _Len) ->
    eof;
do_sparse_read(Reader=#sparse_file_reader{pos=Pos,sparse_map=#sparse_array{
                 entries=[#sparse_entry{offset=Offset}|Entries]}}, Len)
  when Pos < Offset ->
    SparseMap = Reader#sparse_file_reader.sparse_map#sparse_array{entries=Entries},
    Reader2 = Reader#sparse_file_reader{sparse_map=SparseMap},
    read_sparse_hole(Reader2, Offset, Len);
do_sparse_read(Reader=#sparse_file_reader{pos=Pos,sparse_map=#sparse_array{
                 entries=[Entry|Entries]}}, Len) ->
    % we're in a data fragment, so read from it
    % end offset of fragment
    EndPos = Entry#sparse_entry.offset + Entry#sparse_entry.num_bytes,
    % bytes left in fragment
    NumBytes = EndPos - Pos,
    ActualLen = if Len > NumBytes -> NumBytes; true -> Len end,
    case do_read(Reader#sparse_file_reader.handle, ActualLen) of
        {ok, Bin, Handle} ->
            BytesRead = byte_size(Bin),
            ActualEndPos = Pos+BytesRead,
            Sparsemap = if ActualEndPos =:= EndPos ->
                    Reader#sparse_file_reader.sparse_map#sparse_array{
                      entries=Entries};
               true ->
                    Reader#sparse_file_reader.sparse_map
            end,
            Size = Reader#sparse_file_reader.size,
            NumBytes2 = Size - ActualEndPos,
            Reader2 = Reader#sparse_file_reader{
                        handle=Handle,
                        pos=ActualEndPos,
                        num_bytes=NumBytes2,
                        sparse_map=Sparsemap},
            {ok, Bin, Reader2};
        Other ->
            Other
    end.

% Reads a sparse hole ending at Offset
read_sparse_hole(Reader = #sparse_file_reader{handle=Handle,pos=Pos}, Offset, Len) ->
    N = Offset - Pos,
    N2 = if N > Len ->
            Len;
       true ->
            N
    end,
    Bin = binary:copy(<<0>>, N2),
    case do_position(Handle, Handle#reader.pos + (Pos+N2)) of
        {ok, _, Handle2} ->
            NumBytes = Reader#sparse_file_reader.size - (Pos+N2),
            {ok, Bin, Reader#sparse_file_reader{
                        handle=Handle2,
                        num_bytes=NumBytes,
                        pos=Pos+N2}};
        Other ->
            Other
    end.

do_close(#reg_file_reader{handle=Handle}) -> do_close(Handle);
do_close(#sparse_file_reader{handle=Handle}) -> do_close(Handle);
do_close(#reader{handle=Handle,func=Fun}) when is_function(Fun,2) ->
    Fun(close,Handle).

%%%%%%%%%%%%%%%%%%
%% Option parsing
%%%%%%%%%%%%%%%%%%

extract_opts(List) ->
    extract_opts(List, default_options()).

table_opts(List) ->
    read_opts(List, default_options()).

default_options() ->
    {ok, Cwd} = file:get_cwd(),
    #read_opts{cwd=Cwd}.

extract_opts([keep_old_files|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{keep_old_files=true});
extract_opts([{cwd, Cwd}|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{cwd=Cwd});
extract_opts([{files, Files}|Rest], Opts) ->
    Set = ordsets:from_list(Files),
    extract_opts(Rest, Opts#read_opts{files=Set});
extract_opts([memory|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{output=memory});
extract_opts([compressed|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    extract_opts(Rest, Opts#read_opts{open_mode=[compressed|OpenMode]});
extract_opts([cooked|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    extract_opts(Rest, Opts#read_opts{open_mode=[cooked|OpenMode]});
extract_opts([verbose|Rest], Opts) ->
    extract_opts(Rest, Opts#read_opts{verbose=true});
extract_opts([Other|Rest], Opts) ->
    extract_opts(Rest, read_opts([Other], Opts));
extract_opts([], Opts) ->
    Opts.

read_opts([compressed|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    read_opts(Rest, Opts#read_opts{open_mode=[compressed|OpenMode]});
read_opts([cooked|Rest], Opts=#read_opts{open_mode=OpenMode}) ->
    read_opts(Rest, Opts#read_opts{open_mode=[cooked|OpenMode]});
read_opts([verbose|Rest], Opts) ->
    read_opts(Rest, Opts#read_opts{verbose=true});
read_opts([_|Rest], Opts) ->
    read_opts(Rest, Opts);
read_opts([], Opts) ->
    Opts.
