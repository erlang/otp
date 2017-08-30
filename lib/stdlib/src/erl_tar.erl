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
-module(erl_tar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Purpose: Unix tar (tape archive) utility.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/3, create/2, create/3, extract/1, extract/2, table/1, table/2,
	 open/2, close/1, add/3, add/4,
	 t/1, tt/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-record(add_opts,
	{read_info,				% Fun to use for read file/link info.
	 chunk_size = 0,  % For file reading when sending to sftp. 0=do not chunk
	 verbose = false :: boolean()}).	% Verbose on/off.

%% Opens a tar archive.

init(UsrHandle, AccessMode, Fun) when is_function(Fun,2) ->   
    {ok, {AccessMode,{tar_descriptor,UsrHandle,Fun}}}.

%%%================================================================		   
%%% The open function with friends is to keep the file and binary api of this module
open(Name, Mode) ->
    case open_mode(Mode) of
	{ok, Access, Raw, Opts} ->
	    open1(Name, Access, Raw, Opts);
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

open1({binary,Bin}, read, _Raw, Opts) ->
    case file:open(Bin, [ram,binary,read]) of
	{ok,File} ->
            _ = [ram_file:uncompress(File) || Opts =:= [compressed]],
	    init(File,read,file_fun());
	Error ->
	    Error
    end;
open1({file, Fd}, read, _Raw, _Opts) ->
    init(Fd, read, file_fun());
open1(Name, Access, Raw, Opts) ->
    case file:open(Name, Raw ++ [binary, Access|Opts]) of
	{ok, File} ->
	    init(File, Access, file_fun());
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

file_fun() ->
    fun(write, {Fd,Data}) ->  file:write(Fd, Data);
       (position, {Fd,Pos}) -> file:position(Fd, Pos);
       (read2, {Fd,Size}) -> file:read(Fd,Size);
       (close, Fd) -> file:close(Fd)
    end.

%%% End of file and binary api (except for open_mode/1 downwards
%%%================================================================		   

%% Closes a tar archive.

close({read, File}) ->
    ok = do_close(File);
close({write, File}) ->
    PadResult = pad_file(File),
    ok = do_close(File),
    PadResult;
close(_) ->
    {error, einval}.

%% Adds a file to a tape archive.

add(File, Name, Options) ->
    add(File, Name, Name, Options).
add({write, File}, Name, NameInArchive, Options) ->
    Opts = #add_opts{read_info=fun(F) -> file:read_link_info(F) end},
    add1(File, Name, NameInArchive, add_opts(Options, Opts));
add({read, _File}, _, _, _) ->
    {error, eacces};
add(_, _, _, _) ->
    {error, einval}.

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

%% Creates a tar file Name containing the given files.

create(Name, Filenames) ->
    create(Name, Filenames, []).

%% Creates a tar archive Name containing the given files.
%% Accepted options: verbose, compressed, cooked

create(Name, FileList, Options) ->
    Mode = lists:filter(fun(X) -> (X=:=compressed) or (X=:=cooked) 
                        end, Options),
    case open(Name, [write|Mode]) of
	{ok, TarFile} ->
	    Add = fun({NmInA, NmOrBin}) -> 
			  add(TarFile, NmOrBin, NmInA, Options);
		     (Nm) -> 
			  add(TarFile, Nm, Nm, Options)
		  end,
	    Result = foreach_while_ok(Add, FileList),
	    case {Result, close(TarFile)} of
		{ok, Res} -> Res;
		{Res, _} -> Res
	    end;
	Reason ->
	    Reason
    end.

%% Extracts all files from the tar file Name.

extract(Name) ->
    extract(Name, []).

%% Extracts (all) files from the tar file Name.
%% Options accepted: keep_old_files, {files, ListOfFilesToExtract}, verbose,
%%		{cwd, AbsoluteDirectory}

extract(Name, Opts) ->
    foldl_read(Name, fun extract1/4, ok, extract_opts(Opts)).

%% Returns a list of names of the files in the tar file Name.
%% Options accepted: verbose

table(Name) ->
    table(Name, []).

%% Returns a list of names of the files in the tar file Name.
%% Options accepted: compressed, verbose, cooked.

table(Name, Opts) ->
    foldl_read(Name, fun table1/4, [], table_opts(Opts)).


%% Comments for printing the contents of a tape archive,
%% meant to be invoked from the shell.

t(Name) ->
    case table(Name) of
	{ok, List} ->
	    lists:foreach(fun(N) -> ok = io:format("~ts\n", [N]) end, List);
	Error ->
	    Error
    end.

tt(Name) ->
    case table(Name, [verbose]) of
	{ok, List} ->
	    lists:foreach(fun print_header/1, List);
	Error ->
	    Error
    end.

print_header({Name, Type, Size, Mtime, Mode, Uid, Gid}) ->
    io:format("~s~s ~4w/~-4w ~7w ~s ~s\n",
	      [type_to_string(Type), mode_to_string(Mode),
	       Uid, Gid, Size, time_to_string(Mtime), Name]).

type_to_string(regular) -> "-";
type_to_string(directory) -> "d";
type_to_string(link) -> "l";
type_to_string(symlink) -> "s";
type_to_string(char) -> "c";
type_to_string(block) -> "b";
type_to_string(fifo) -> "f";
type_to_string(_) -> "?".

mode_to_string(Mode) ->
    mode_to_string(Mode, "xwrxwrxwr", []).

mode_to_string(Mode, [C|T], Acc) when Mode band 1 =:= 1 ->
    mode_to_string(Mode bsr 1, T, [C|Acc]);
mode_to_string(Mode, [_|T], Acc) ->
    mode_to_string(Mode bsr 1, T, [$-|Acc]);
mode_to_string(_, [], Acc) ->
    Acc.

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

%% Converts the short error reason to a descriptive string.

format_error(bad_header) -> "Bad directory header";
format_error(eof) -> "Unexpected end of file";
format_error(symbolic_link_too_long) -> "Symbolic link too long";
format_error({Name,Reason}) ->
    lists:flatten(io_lib:format("~ts: ~ts", [Name,format_error(Reason)]));
format_error(Atom) when is_atom(Atom) ->
    file:format_error(Atom);
format_error(Term) ->
    lists:flatten(io_lib:format("~tp", [Term])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%	Useful definitions (also start of implementation).
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Offset for fields in the tar header.
%% Note that these offsets are ZERO-based as in the POSIX standard
%% document, while binaries use ONE-base offset.  Caveat Programmer.

-define(th_name, 0).
-define(th_mode, 100).
-define(th_uid, 108).
-define(th_gid, 116).
-define(th_size, 124).
-define(th_mtime, 136).
-define(th_chksum, 148).
-define(th_typeflag, 156).
-define(th_linkname, 157).
-define(th_magic, 257).
-define(th_version, 263).
-define(th_prefix, 345).

%% Length of these fields.

-define(th_name_len, 100).
-define(th_mode_len, 8).
-define(th_uid_len, 8).
-define(th_gid_len, 8).
-define(th_size_len, 12).
-define(th_mtime_len, 12).
-define(th_chksum_len, 8).
-define(th_linkname_len, 100).
-define(th_magic_len, 6).
-define(th_version_len, 2).
-define(th_prefix_len, 167).

-record(tar_header,
	{name,					% Name of file.
	 mode,					% Mode bits.
	 uid,					% User id.
	 gid,					% Group id.
	 size,					% Size of file
	 mtime,					% Last modified (seconds since
						% Jan 1, 1970).
	 chksum,				% Checksum of header.
	 typeflag = [],				% Type of file.
	 linkname = [],				% Name of link.
	 filler = [],
	 prefix}).				% Filename prefix.

-define(record_size, 512).
-define(block_size, (512*20)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Adding members to a tar archive.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add1(TarFile, Bin, NameInArchive, Opts) when is_binary(Bin) ->
    Now = calendar:now_to_local_time(erlang:timestamp()),
    Info = #file_info{size = byte_size(Bin),
		      type = regular,
		      access = read_write,
		      atime = Now,
		      mtime = Now,
		      ctime = Now,
		      mode  = 8#100644,
		      links = 1,
		      major_device = 0,
		      minor_device = 0,
		      inode = 0,
		      uid = 0,
		      gid = 0},
    Header = create_header(NameInArchive, Info),
    add1(TarFile, NameInArchive, Header, Bin, Opts);
add1(TarFile, Name, NameInArchive, Opts) ->
    case read_file_and_info(Name, Opts) of
	{ok, Bin, Info} when Info#file_info.type =:= regular ->
	    Header = create_header(NameInArchive, Info),
	    add1(TarFile, Name, Header, Bin, Opts);
	{ok, PointsTo, Info} when Info#file_info.type =:= symlink ->
	    if
		length(PointsTo) > 100 ->
		    {error,{PointsTo,symbolic_link_too_long}};
		true ->
		    Info2 = Info#file_info{size=0},
		    Header = create_header(NameInArchive, Info2, PointsTo),
		    add1(TarFile, Name, Header, list_to_binary([]), Opts)
	    end;
	{ok, _, Info} when Info#file_info.type =:= directory ->
	    add_directory(TarFile, Name, NameInArchive, Info, Opts);
	{ok, _, #file_info{type=Type}} ->
	    {error, {bad_file_type, Name, Type}};
	{error, Reason} ->
	    {error, {Name, Reason}}
    end.

add1(Tar, Name, Header, chunked, Options) ->
    add_verbose(Options, "a ~ts [chunked ", [Name]),
    try
	ok = do_write(Tar, Header),
	{ok,D} = file:open(Name, [read,binary]),
	{ok,NumBytes} = add_read_write_chunks(D, Tar, Options#add_opts.chunk_size, 0, Options),
	_ = file:close(D),
	ok = do_write(Tar, padding(NumBytes,?record_size))
    of 
	ok ->
	    add_verbose(Options, "~n", []),
	    ok
    catch
	error:{badmatch,{error,Error}} ->
	    add_verbose(Options, "~n", []),
	    {error,{Name,Error}}
    end;
add1(Tar, Name, Header, Bin, Options) ->
    add_verbose(Options, "a ~ts~n", [Name]),
    do_write(Tar, [Header, Bin, padding(byte_size(Bin), ?record_size)]).

add_read_write_chunks(D, Tar, ChunkSize, SumNumBytes, Options) ->
    case file:read(D, ChunkSize) of
	{ok,Bin} -> 
	    ok = do_write(Tar, Bin),
	    add_verbose(Options, ".", []),
	    add_read_write_chunks(D, Tar, ChunkSize, SumNumBytes+byte_size(Bin), Options);
	eof ->
	    add_verbose(Options, "]", []),
	    {ok,SumNumBytes};
	Other ->
	    Other
    end.

add_directory(TarFile, DirName, NameInArchive, Info, Options) ->
    case file:list_dir(DirName) of
	{ok, []} ->
	    add_verbose(Options, "a ~ts~n", [DirName]),
	    Header = create_header(NameInArchive, Info),
	    do_write(TarFile, Header);
	{ok, Files} ->
	    Add = fun (File) ->
			  add1(TarFile,
			       filename:join(DirName, File),
			       filename:join(NameInArchive, File),
			       Options) end,
	    foreach_while_ok(Add, Files);
	{error, Reason} ->
	    {error, {DirName, Reason}}
    end.
    
%% Creates a header for file in a tar file.

create_header(Name, Info) ->
    create_header(Name, Info, []).
create_header(Name, #file_info {mode=Mode, uid=Uid, gid=Gid,
				size=Size, mtime=Mtime0, type=Type}, Linkname) ->
    Mtime = posix_time(erlang:localtime_to_universaltime(Mtime0)),
    {Prefix,Suffix} = split_filename(Name),
    H0 = [to_string(Suffix, 100),
	  to_octal(Mode, 8),
	  to_octal(Uid, 8),
	  to_octal(Gid, 8),
	  to_octal(Size, ?th_size_len),
	  to_octal(Mtime, ?th_mtime_len),
	  <<"        ">>,
	  file_type(Type),
	  to_string(Linkname, ?th_linkname_len),
	  "ustar",0,
	  "00",
	  zeroes(?th_prefix-?th_version-?th_version_len),
	  to_string(Prefix, ?th_prefix_len)],
    H = list_to_binary(H0),
    512 = byte_size(H),				%Assertion.
    ChksumString = to_octal(checksum(H), 6, [0,$\s]),
    <<Before:?th_chksum/binary,_:?th_chksum_len/binary,After/binary>> = H,
    [Before,ChksumString,After].

file_type(regular) -> $0;
file_type(symlink) -> $2;
file_type(directory) -> $5.
    
to_octal(Int, Count) when Count > 1 ->
    to_octal(Int, Count-1, [0]).

to_octal(_, 0, Result) -> Result;
to_octal(Int, Count, Result) ->
    to_octal(Int div 8, Count-1, [Int rem 8 + $0|Result]).

to_string(Str0, Count) ->
    Str = case file:native_name_encoding() of
	      utf8 ->
		  unicode:characters_to_binary(Str0);
	      latin1 ->
		  list_to_binary(Str0)
	  end,
    case byte_size(Str) of
	Size when Size < Count ->
	    [Str|zeroes(Count-Size)];
	_ -> Str
    end.

%% Pads out end of file.

pad_file(File) ->
    {ok,Position} = do_position(File, {cur,0}),
    %% There must be at least two zero records at the end.
    Fill = case ?block_size - (Position rem ?block_size) of
	       Fill0 when Fill0 < 2*?record_size ->
		   %% We need to another block here to ensure that there
		   %% are at least two zero records at the end.
		   Fill0 + ?block_size;
	       Fill0 ->
		   %% Large enough.
		   Fill0
	   end,
    do_write(File, zeroes(Fill)).

split_filename(Name) when length(Name) =< ?th_name_len ->
    {"", Name};
split_filename(Name0) ->
    split_filename(lists:reverse(filename:split(Name0)), [], [], 0).

split_filename([Comp|Rest], Prefix, Suffix, Len) 
  when Len+length(Comp) < ?th_name_len ->
    split_filename(Rest, Prefix, [Comp|Suffix], Len+length(Comp)+1);
split_filename([Comp|Rest], Prefix, Suffix, Len) ->
    split_filename(Rest, [Comp|Prefix], Suffix, Len+length(Comp)+1);
split_filename([], Prefix, Suffix, _) ->
    {filename:join(Prefix),filename:join(Suffix)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Retrieving files from a tape archive.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Options used when reading a tar archive.

-record(read_opts,
	{cwd             :: string(),		% Current working directory.
	 keep_old_files = false :: boolean(),	% Owerwrite or not.
	 files = all,				% Set of files to extract
						% (or all).
	 output = file   :: 'file' | 'memory',
	 open_mode = [],			% Open mode options.
	 verbose = false :: boolean()}).	% Verbose on/off.

extract_opts(List) ->
    extract_opts(List, default_options()).

table_opts(List) ->
    read_opts(List, default_options()).

default_options() ->
    {ok, Cwd} = file:get_cwd(),
    #read_opts{cwd=Cwd}.

%% Parse options for extract.

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

%% Common options for all read operations.

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

foldl_read({AccessMode,TD={tar_descriptor,_UsrHandle,_AccessFun}}, Fun, Accu, Opts) ->
    case AccessMode of
	read ->
	    foldl_read0(TD, Fun, Accu, Opts);
	_ ->
	    {error,{read_mode_expected,AccessMode}}
    end;
foldl_read(TarName, Fun, Accu, Opts) ->
    case open(TarName, [read|Opts#read_opts.open_mode]) of
	{ok, {read, File}} ->
	    Result = foldl_read0(File, Fun, Accu, Opts),
	    ok = do_close(File),
	    Result;
	Error ->
	    Error
    end.

foldl_read0(File, Fun, Accu, Opts) ->
    case catch foldl_read1(Fun, Accu, File, Opts) of
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

foldl_read1(Fun, Accu0, File, Opts) ->
    case get_header(File) of
	eof ->
	    Fun(eof, File, Opts, Accu0);
	Header ->
	    {ok, NewAccu} = Fun(Header, File, Opts, Accu0),
	    foldl_read1(Fun, NewAccu, File, Opts)
    end.

table1(eof, _, _, Result) ->
    {ok, lists:reverse(Result)};
table1(Header = #tar_header{}, File, #read_opts{verbose=true}, Result) ->
    #tar_header{name=Name, size=Size, mtime=Mtime, typeflag=Type,
		mode=Mode, uid=Uid, gid=Gid} = Header,
    skip(File, Size),
    {ok, [{Name, Type, Size, posix_to_erlang_time(Mtime), Mode, Uid, Gid}|Result]};
table1(#tar_header{name=Name, size=Size}, File, _, Result) ->
    skip(File, Size),
    {ok, [Name|Result]}.

extract1(eof, _, _, Acc) ->
    if
	is_list(Acc) ->
	    {ok, lists:reverse(Acc)};
	true ->
	    Acc
    end;
extract1(Header, File, Opts, Acc) ->
    Name = Header#tar_header.name,
    case check_extract(Name, Opts) of
	true ->
	    {ok, Bin} = get_element(File, Header),
	    case write_extracted_element(Header, Bin, Opts) of
		ok ->
		    {ok, Acc};
		{ok, NameBin} when is_list(Acc) ->
		    {ok, [NameBin | Acc]};
		{ok, NameBin} when Acc =:= ok ->
		    {ok, [NameBin]}
	    end;
	false ->
	    ok = skip(File, Header#tar_header.size),
	    {ok, Acc}
    end.

%% Checks if the file Name should be extracted.

check_extract(_, #read_opts{files=all}) ->
    true;
check_extract(Name, #read_opts{files=Files}) ->
    ordsets:is_element(Name, Files).

get_header(File) ->
    case do_read(File, ?record_size) of
	eof ->
	    throw({error,eof});
	{ok, Bin} when is_binary(Bin) ->
	    convert_header(Bin);
	{ok, List} ->
	    convert_header(list_to_binary(List));
	{error, Reason} ->
	    throw({error, Reason})
    end.

%% Converts the tar header to a record.

convert_header(Bin) when byte_size(Bin) =:= ?record_size ->
    case verify_checksum(Bin) of
	ok ->
	    Hd = #tar_header{name=get_name(Bin),
			     mode=from_octal(Bin, ?th_mode, ?th_mode_len),
			     uid=from_octal(Bin, ?th_uid, ?th_uid_len),
			     gid=from_octal(Bin, ?th_gid, ?th_gid_len),
			     size=from_octal(Bin, ?th_size, ?th_size_len),
			     mtime=from_octal(Bin, ?th_mtime, ?th_mtime_len),
			     linkname=from_string(Bin,
						  ?th_linkname, ?th_linkname_len),
			     typeflag=typeflag(Bin)},
	    convert_header1(Hd);
	eof ->
	    eof
    end;
convert_header(Bin) when byte_size(Bin) =:= 0 ->
    eof;
convert_header(_Bin) ->
    throw({error, eof}).

%% Basic sanity.  Better set the element size to zero here if the type
%% always is of zero length.

convert_header1(H) when H#tar_header.typeflag =:= symlink, H#tar_header.size =/= 0 ->
    convert_header1(H#tar_header{size=0});
convert_header1(H) when H#tar_header.typeflag =:= directory, H#tar_header.size =/= 0 ->
    convert_header1(H#tar_header{size=0});
convert_header1(Header) ->
    Header.

typeflag(Bin) ->
    [T] = binary_to_list(Bin, ?th_typeflag+1, ?th_typeflag+1),
    case T of
	0  -> regular;
	$0 -> regular;
	$1 -> link;
	$2 -> symlink;
	$3 -> char;
	$4 -> block;
	$5 -> directory;
	$6 -> fifo;
	$7 -> regular;
	_  -> unknown
    end.

%% Get the name of the file from the prefix and name fields of the
%% tar header.

get_name(Bin0) ->
    List0 = get_name_raw(Bin0),
    case file:native_name_encoding() of
	utf8 ->
	    Bin = list_to_binary(List0),
	    case unicode:characters_to_list(Bin) of
		{error,_,_} ->
		    List0;
		List when is_list(List) ->
		    List
	    end;
	latin1 ->
	    List0
    end.

get_name_raw(Bin) ->
    Name = from_string(Bin, ?th_name, ?th_name_len),
    case binary_to_list(Bin, ?th_prefix+1, ?th_prefix+1) of
	[0] ->
	    Name;
	[_] ->
	    Prefix = binary_to_list(Bin, ?th_prefix+1, byte_size(Bin)),
	    lists:reverse(remove_nulls(Prefix), [$/|Name])
    end.

from_string(Bin, Pos, Len) ->
    lists:reverse(remove_nulls(binary_to_list(Bin, Pos+1, Pos+Len))).
    
%% Returns all characters up to (but not including) the first null
%% character, in REVERSE order.

remove_nulls(List) ->
    remove_nulls(List, []).

remove_nulls([0|_], Result) ->
    remove_nulls([], Result);
remove_nulls([C|Rest], Result) ->
    remove_nulls(Rest, [C|Result]);
remove_nulls([], Result) ->
    Result.

from_octal(Bin, Pos, Len) ->
    from_octal(binary_to_list(Bin, Pos+1, Pos+Len)).

from_octal([$\s|Rest]) ->
    from_octal(Rest);
from_octal([Digit|Rest]) when $0 =< Digit, Digit =< $7 ->
    from_octal(Rest, Digit-$0);
from_octal(Bin) when is_binary(Bin) ->
    from_octal(binary_to_list(Bin));
from_octal(Other) ->
    throw({error, {bad_header, "Bad octal number: ~p", [Other]}}).

from_octal([Digit|Rest], Result) when $0 =< Digit, Digit =< $7 ->
    from_octal(Rest, Result*8+Digit-$0);
from_octal([$\s|_], Result) ->
    Result;
from_octal([0|_], Result) ->
    Result;
from_octal(Other, _) ->
    throw({error, {bad_header, "Bad contents in octal field: ~p", [Other]}}).

%% Retrieves the next element from the archive.
%% Returns {ok, Bin} | eof | {error, Reason}

get_element(File, #tar_header{size = 0}) ->
    skip_to_next(File),
    {ok,<<>>};
get_element(File, #tar_header{size = Size}) ->
    case do_read(File, Size) of
	{ok,Bin}=Res when byte_size(Bin) =:= Size ->
	    skip_to_next(File),
	    Res;
	{ok,List} when length(List) =:= Size ->
	    skip_to_next(File),
	    {ok,list_to_binary(List)};
	{ok,_} -> throw({error,eof});
	{error, Reason} -> throw({error, Reason});
	eof -> throw({error,eof})
    end.

%% Verify the checksum in the header.  First try an unsigned addition
%% of all bytes in the header (as it should be according to Posix).

verify_checksum(Bin) ->
    <<H1:?th_chksum/binary,CheckStr:?th_chksum_len/binary,H2/binary>> = Bin,
    case checksum(H1) + checksum(H2) of
	0 -> eof;
	Checksum0 ->
	    Csum = from_octal(CheckStr),
	    CsumInit = ?th_chksum_len * $\s,
	    case Checksum0 + CsumInit of 
		Csum -> ok;
		Unsigned ->
		    verify_checksum(H1, H2, CsumInit, Csum, Unsigned)
	    end
    end.

%% The checksums didn't match.  Now try a signed addition.

verify_checksum(H1, H2, Csum, ShouldBe, Unsigned) ->
    case signed_sum(binary_to_list(H1), signed_sum(binary_to_list(H2), Csum)) of
	ShouldBe -> ok;
	Signed ->
	    throw({error,
		   {bad_header, 
		    "Incorrect directory checksum ~w (~w), should be ~w",
		    [Signed, Unsigned, ShouldBe]}})
    end.

signed_sum([C|Rest], Sum) when C < 128 ->
    signed_sum(Rest, Sum+C);
signed_sum([C|Rest], Sum) ->
    signed_sum(Rest, Sum+C-256);
signed_sum([], Sum) -> Sum.

write_extracted_element(Header, Bin, Opts)
  when Opts#read_opts.output =:= memory ->
    case Header#tar_header.typeflag of
	regular ->
	    {ok, {Header#tar_header.name, Bin}};
	_ ->
	    ok
    end;
write_extracted_element(Header, Bin, Opts) ->
    Name = filename:absname(Header#tar_header.name, Opts#read_opts.cwd),
    Created = 
	case Header#tar_header.typeflag of
	    regular ->
		write_extracted_file(Name, Bin, Opts);
	    directory ->
		create_extracted_dir(Name, Opts);
	    symlink ->
		create_symlink(Name, Header, Opts);
	    Other ->				% Ignore.
		read_verbose(Opts, "x ~ts - unsupported type ~p~n",
			     [Name, Other]),
		not_written
	end,
    case Created of
	ok  -> set_extracted_file_info(Name, Header);
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

create_symlink(Name, #tar_header{linkname=Linkname}=Header, Opts) ->
    case file:make_symlink(Linkname, Name) of
	ok -> ok;
	{error,enoent} ->
	    ok = make_dirs(Name, file),
	    create_symlink(Name, Header, Opts);
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
	true ->
	    read_verbose(Opts, "x ~ts~n", [Name]),
	    write_file(Name, Bin);
	false ->
	    read_verbose(Opts, "x ~ts - exists, not created~n", [Name]),
	    not_written
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

set_extracted_file_info(_, #tar_header{typeflag = symlink}) -> ok;
set_extracted_file_info(Name, #tar_header{mode=Mode, mtime=Mtime}) ->
    Info = #file_info{mode=Mode, mtime=posix_to_erlang_time(Mtime)},
    file:write_file_info(Name, Info).

%% Makes all directories leading up to the file.

make_dirs(Name, file) ->
	filelib:ensure_dir(Name);
make_dirs(Name, dir) ->
	filelib:ensure_dir(filename:join(Name,"*")).

%% Prints the message on if the verbose option is given (for reading).

read_verbose(#read_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args),
    io:nl();
read_verbose(_, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Utility functions.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Returns the checksum of a binary.

checksum(Bin) -> checksum(Bin, 0).

checksum(<<A,B,C,D,E,F,G,H,T/binary>>, Sum) ->
    checksum(T, Sum+A+B+C+D+E+F+G+H);
checksum(<<A,T/binary>>, Sum) ->
    checksum(T, Sum+A);
checksum(<<>>, Sum) -> Sum.

%% Returns a list of zeroes to pad out to the given block size.

padding(Size, BlockSize) ->
    zeroes(pad_size(Size, BlockSize)).

pad_size(Size, BlockSize) ->
    case Size rem BlockSize of
	0 -> 0;
	Rem -> BlockSize-Rem
    end.

zeroes(0) -> [];
zeroes(1) -> [0];
zeroes(2) -> [0,0];
zeroes(Number) ->
    Half = zeroes(Number div 2),
    case Number rem 2 of
	0 -> [Half|Half];
	1 -> [Half|[0|Half]]
    end.

%% Skips the given number of bytes rounded up to an even record.

skip(File, Size) ->
    %% Note: There is no point in handling failure to get the current position
    %% in the file.  If it doesn't work, something serious is wrong.
    Amount = ((Size + ?record_size - 1) div ?record_size) * ?record_size,
    {ok,_} = do_position(File, {cur, Amount}),
    ok.

%% Skips to the next record in the file.

skip_to_next(File) ->
    %% Note: There is no point in handling failure to get the current position
    %% in the file.  If it doesn't work, something serious is wrong.
    {ok, Position} = do_position(File, {cur, 0}),
    NewPosition = ((Position + ?record_size - 1) div ?record_size) * ?record_size,
    {ok,NewPosition} = do_position(File, NewPosition),
    ok.

%% Prints the message on if the verbose option is given.

add_verbose(#add_opts{verbose=true}, Format, Args) ->
    io:format(Format, Args);
add_verbose(_, _, _) ->
    ok.

%% Converts a tuple containing the time to a Posix time (seconds
%% since Jan 1, 1970).

posix_time(Time) ->
    EpochStart = {{1970,1,1},{0,0,0}},
    {Days,{Hour,Min,Sec}} = calendar:time_difference(EpochStart, Time),
    86400*Days + 3600*Hour + 60*Min + Sec.

posix_to_erlang_time(Sec) ->
    OneMillion = 1000000,
    Time = calendar:now_to_datetime({Sec div OneMillion, Sec rem OneMillion, 0}),
    erlang:universaltime_to_localtime(Time).

read_file_and_info(Name, Opts) ->
    ReadInfo = Opts#add_opts.read_info,
    case ReadInfo(Name) of
	{ok,Info} when Info#file_info.type =:= regular,
		       Opts#add_opts.chunk_size>0 ->
	    {ok,chunked,Info};
	{ok,Info} when Info#file_info.type =:= regular ->
	    case file:read_file(Name) of
		{ok,Bin} ->
		    {ok,Bin,Info};
		Error ->
		    Error
	    end;
	{ok,Info} when Info#file_info.type =:= symlink ->
	    case file:read_link(Name) of
		{ok,PointsTo} ->
		    {ok,PointsTo,Info};
		Error ->
		    Error
	    end;
	{ok, Info} ->
	    {ok,[],Info};
	Error ->
	    Error
    end.

foreach_while_ok(Fun, [First|Rest]) ->
    case Fun(First) of
	ok -> foreach_while_ok(Fun, Rest);
	Other -> Other
    end;
foreach_while_ok(_, []) -> ok.
    
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

%%%================================================================
do_write({tar_descriptor,UsrHandle,Fun}, Data) -> Fun(write,{UsrHandle,Data}).

do_position({tar_descriptor,UsrHandle,Fun}, Pos) -> Fun(position,{UsrHandle,Pos}).

do_read({tar_descriptor,UsrHandle,Fun}, Len) -> Fun(read2,{UsrHandle,Len}).

do_close({tar_descriptor,UsrHandle,Fun}) -> Fun(close,UsrHandle).
