%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2013. All Rights Reserved.
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
-module(prim_file).

%% Interface module to the file driver.



%%% Interface towards a single file's contents. Uses ?FD_DRV.

%% Generic file contents operations
-export([open/2, close/1, datasync/1, sync/1, advise/4, position/2, truncate/1,
	 write/2, pwrite/2, pwrite/3, read/2, read_line/1, pread/2, pread/3,
	 copy/3, sendfile/8, allocate/3]).

%% Specialized file operations
-export([open/1, open/3]).
-export([read_file/1, read_file/2, write_file/2]).
-export([ipread_s32bu_p32bu/3]).



%%% Interface towards file system and metadata. Uses ?DRV.

%% Takes an optional port (opens a ?DRV port per default) as first argument.
-export([get_cwd/0, get_cwd/1, get_cwd/2, 
	 set_cwd/1, set_cwd/2,
	 delete/1, delete/2, 
	 rename/2, rename/3, 
	 make_dir/1, make_dir/2,
	 del_dir/1, del_dir/2,
	 read_file_info/1, read_file_info/2, read_file_info/3,
	 altname/1, altname/2,
	 write_file_info/2, write_file_info/3, write_file_info/4,
	 make_link/2, make_link/3,
	 make_symlink/2, make_symlink/3,
	 read_link/1, read_link/2, read_link_all/1, read_link_all/2,
	 read_link_info/1, read_link_info/2, read_link_info/3,
	 list_dir/1, list_dir/2, list_dir_all/1, list_dir_all/2]).
%% How to start and stop the ?DRV port.
-export([start/0, stop/1]).

%% Debug exports
-export([open_int/4, open_mode/1, open_mode/4]).

%%%-----------------------------------------------------------------
%%% Includes and defines

-include("file.hrl").

-define(DRV,    "efile").
-define(FD_DRV, "efile").

-define(LARGEFILESIZE, (1 bsl 63)).

%% Driver commands
-define(FILE_OPEN,             1).
-define(FILE_READ,             2).
-define(FILE_LSEEK,            3).
-define(FILE_WRITE,            4).
-define(FILE_FSTAT,            5).
-define(FILE_PWD,              6).
-define(FILE_READDIR,          7).
-define(FILE_CHDIR,            8).
-define(FILE_FSYNC,            9).
-define(FILE_MKDIR,            10).
-define(FILE_DELETE,           11).
-define(FILE_RENAME,           12).
-define(FILE_RMDIR,            13).
-define(FILE_TRUNCATE,         14).
-define(FILE_READ_FILE,        15).
-define(FILE_WRITE_INFO,       16).
-define(FILE_LSTAT,            19).
-define(FILE_READLINK,         20).
-define(FILE_LINK,             21).
-define(FILE_SYMLINK,          22).
-define(FILE_CLOSE,            23).
-define(FILE_PWRITEV,          24).
-define(FILE_PREADV,           25).
-define(FILE_SETOPT,           26).
-define(FILE_IPREAD,           27).
-define(FILE_ALTNAME,          28).
-define(FILE_READ_LINE,        29).
-define(FILE_FDATASYNC,        30).
-define(FILE_ADVISE,           31).
-define(FILE_SENDFILE,         32).
-define(FILE_ALLOCATE,         33).

%% Driver responses
-define(FILE_RESP_OK,          0).
-define(FILE_RESP_ERROR,       1).
-define(FILE_RESP_DATA,        2).
-define(FILE_RESP_NUMBER,      3).
-define(FILE_RESP_INFO,        4).
-define(FILE_RESP_NUMERR,      5).
-define(FILE_RESP_LDATA,       6).
-define(FILE_RESP_N2DATA,      7).
-define(FILE_RESP_EOF,         8).
-define(FILE_RESP_FNAME,       9).
-define(FILE_RESP_ALL_DATA,   10).
-define(FILE_RESP_LFNAME,     11).

%% Open modes for the driver's open function.
-define(EFILE_MODE_READ,       1).
-define(EFILE_MODE_WRITE,      2).
-define(EFILE_MODE_READ_WRITE, 3).  
-define(EFILE_MODE_APPEND,     4).
-define(EFILE_COMPRESSED,      8).
-define(EFILE_MODE_EXCL,       16).
%% Note: bit 5 (32) is used internally for VxWorks
-define(EFILE_MODE_SYNC,       64).

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 127).

%% Seek modes for the driver's seek function.
-define(EFILE_SEEK_SET, 0).
-define(EFILE_SEEK_CUR, 1).
-define(EFILE_SEEK_END, 2).

%% Options
-define(FILE_OPT_DELAYED_WRITE, 0).
-define(FILE_OPT_READ_AHEAD,    1).

%% IPREAD variants
-define(IPREAD_S32BU_P32BU, 0).

%% POSIX file advises
-define(POSIX_FADV_NORMAL,     0).
-define(POSIX_FADV_RANDOM,     1).
-define(POSIX_FADV_SEQUENTIAL, 2).
-define(POSIX_FADV_WILLNEED,   3).
-define(POSIX_FADV_DONTNEED,   4).
-define(POSIX_FADV_NOREUSE,    5).

%% Sendfile flags
-define(EFILE_SENDFILE_USE_THREADS, 1).


%%% BIFs

-export([internal_name2native/1,
         internal_native2name/1,
         internal_normalize_utf8/1,
	 is_translatable/1]).

-type prim_file_name() :: string() | unicode:unicode_binary().
-type prim_file_name_error() :: 'error' | 'ignore' | 'warning'.

-spec internal_name2native(prim_file_name()) -> binary().

internal_name2native(_) ->
    erlang:nif_error(undefined).

-spec internal_native2name(binary()) ->
        prim_file_name() | {'error',prim_file_name_error()}.

internal_native2name(_) ->
    erlang:nif_error(undefined).

-spec internal_normalize_utf8(unicode:unicode_binary()) -> string().

internal_normalize_utf8(_) ->
    erlang:nif_error(undefined).

-spec is_translatable(prim_file_name()) -> boolean().

is_translatable(_) ->
    erlang:nif_error(undefined).

%%% End of BIFs

%%%-----------------------------------------------------------------
%%% Functions operating on a file through a handle. ?FD_DRV.
%%%
%%% Generic file contents operations.
%%%
%%% Supposed to be called by applications through module file.


%% Opens a file using the driver port Port. Returns {error, Reason}
%% | {ok, FileDescriptor}
open(Port, File, ModeList) when is_port(Port), 
                                (is_list(File) orelse is_binary(File)), 
                                is_list(ModeList) ->
    case open_mode(ModeList) of
	{Mode, _Portopts, _Setopts} ->
	    open_int(Port, File, Mode, []);
	Reason ->
	    {error, Reason}
    end;
open(_,_,_) ->
    {error, badarg}.

%% Opens a file. Returns {error, Reason} | {ok, FileDescriptor}.
open(File, ModeList) when (is_list(File) orelse is_binary(File)), 
			  is_list(ModeList) ->
    case open_mode(ModeList) of
	{Mode, Portopts, Setopts} ->
	    open_int({?FD_DRV, Portopts},File, Mode, Setopts);
	Reason ->
	    {error, Reason}
    end;
open(_, _) ->
    {error, badarg}.

%% Opens a port that can be used for open/3 or read_file/2.
%% Returns {ok, Port} | {error, Reason}.
open(Portopts) when is_list(Portopts) ->
    drv_open(?FD_DRV, [binary|Portopts]);
open(_) ->
    {error, badarg}.

open_int({Driver, Portopts}, File, Mode, Setopts) ->
    case drv_open(Driver, Portopts) of
	{ok, Port} ->
	    open_int(Port, File, Mode, Setopts);
	{error, _} = Error ->
	    Error
    end;
open_int(Port, File, Mode, Setopts) ->
    M = Mode band ?EFILE_MODE_MASK,
    case drv_command(Port, [<<?FILE_OPEN, M:32>>, pathname(File)]) of
	{ok, Number} ->
	    open_int_setopts(Port, Number, Setopts);
	Error ->
	    drv_close(Port),
	    Error
    end.

open_int_setopts(Port, Number, []) ->
    {ok, #file_descriptor{module = ?MODULE, data = {Port, Number}}};    
open_int_setopts(Port, Number, [Cmd | Tail]) ->
    case drv_command(Port, Cmd) of
	ok ->
	    open_int_setopts(Port, Number, Tail);
	Error ->
	    drv_close(Port),
	    Error
    end.



%% Returns ok.

close(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    case drv_command(Port, <<?FILE_CLOSE>>) of
	ok ->
	    drv_close(Port);
	Error ->
	    Error
    end;
%% Closes a port opened with open/1.
close(Port) when is_port(Port) ->
    drv_close(Port).

-define(ADVISE(Offs, Len, Adv),
	<<?FILE_ADVISE, Offs:64/signed, Len:64/signed,
	  Adv:32/signed>>).

%% Returns {error, Reason} | ok.
advise(#file_descriptor{module = ?MODULE, data = {Port, _}},
       Offset, Length, Advise) ->
    case Advise of
	normal ->
	    Cmd = ?ADVISE(Offset, Length, ?POSIX_FADV_NORMAL),
	    drv_command(Port, Cmd);
	random ->
	    Cmd = ?ADVISE(Offset, Length, ?POSIX_FADV_RANDOM),
	    drv_command(Port, Cmd);
	sequential ->
	    Cmd = ?ADVISE(Offset, Length, ?POSIX_FADV_SEQUENTIAL),
	    drv_command(Port, Cmd);
	will_need ->
	    Cmd = ?ADVISE(Offset, Length, ?POSIX_FADV_WILLNEED),
	    drv_command(Port, Cmd);
	dont_need ->
	    Cmd = ?ADVISE(Offset, Length, ?POSIX_FADV_DONTNEED),
	    drv_command(Port, Cmd);
	no_reuse ->
	    Cmd = ?ADVISE(Offset, Length, ?POSIX_FADV_NOREUSE),
	    drv_command(Port, Cmd);
	_ ->
	    {error, einval}
    end.

%% Returns {error, Reason} | ok.
allocate(#file_descriptor{module = ?MODULE, data = {Port, _}}, Offset, Length) ->
    Cmd = <<?FILE_ALLOCATE, Offset:64/signed, Length:64/signed>>,
    drv_command(Port, Cmd).

%% Returns {error, Reason} | ok.
write(#file_descriptor{module = ?MODULE, data = {Port, _}}, Bytes) ->
    case drv_command_nt(Port, [?FILE_WRITE,erlang:dt_prepend_vm_tag_data(Bytes)],undefined) of
	{ok, _Size} ->
	    ok;
	Error ->
	    Error
    end.

%% Returns ok | {error, {WrittenCount, Reason}}
pwrite(#file_descriptor{module = ?MODULE, data = {Port, _}}, L)
  when is_list(L) ->
    pwrite_int(Port, L, 0, [], []).

pwrite_int(_, [], 0, [], []) ->
    ok;
pwrite_int(Port, [], N, Spec, Data) ->
    Header = list_to_binary([?FILE_PWRITEV, erlang:dt_prepend_vm_tag_data(<<N:32>>) | reverse(Spec)]),
    case drv_command_nt(Port, [Header | reverse(Data)], undefined) of
	{ok, _Size} ->
	    ok;
	Error ->
	    Error
    end;
pwrite_int(Port, [{Offs, Bytes} | T], N, Spec, Data)
  when is_integer(Offs) ->
    if
	-(?LARGEFILESIZE) =< Offs, Offs < ?LARGEFILESIZE ->
	    pwrite_int(Port, T, N, Spec, Data, Offs, Bytes);
	true ->
	    {error, einval}
    end;
pwrite_int(_, [_|_], _N, _Spec, _Data) ->
    {error, badarg}.

pwrite_int(Port, T, N, Spec, Data, Offs, Bin)
  when is_binary(Bin) ->
    Size = byte_size(Bin),
    pwrite_int(Port, T, N+1, 
	       [<<Offs:64/signed, Size:64>> | Spec], 
	       [Bin | Data]);
pwrite_int(Port, T, N, Spec, Data, Offs, Bytes) ->
    try list_to_binary(Bytes) of
	Bin ->
	    pwrite_int(Port, T, N, Spec, Data, Offs, Bin)
    catch
	error:Reason ->
	    {error, Reason}
    end.



%% Returns {error, Reason} | ok.
pwrite(#file_descriptor{module = ?MODULE, data = {Port, _}}, Offs, Bytes) 
  when is_integer(Offs) ->
    if
	-(?LARGEFILESIZE) =< Offs, Offs < ?LARGEFILESIZE ->
	    case pwrite_int(Port, [], 0, [], [], Offs, Bytes) of
		{error, {_, Reason}} ->
		    {error, Reason};
		Result ->
		    Result
	    end;
	true ->
	    {error, einval}
    end;
pwrite(#file_descriptor{module = ?MODULE}, _, _) ->
    {error, badarg}.


%% Returns {error, Reason} | ok.
datasync(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    drv_command(Port, [?FILE_FDATASYNC]).

%% Returns {error, Reason} | ok.
sync(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    drv_command(Port, [?FILE_FSYNC]).

%% Returns {ok, Data} | eof | {error, Reason}.
read_line(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    case drv_command(Port, <<?FILE_READ_LINE>>) of
	{ok, {0, _Data}} ->
	    eof;
	{ok, {_Size, Data}} ->
	    {ok, Data};
	{error, enomem} ->
	    erlang:garbage_collect(),
	    case drv_command(Port, <<?FILE_READ_LINE>>) of
		{ok, {0, _Data}} ->
		    eof;
		{ok, {_Size, Data}} ->
		    {ok, Data};
		Other ->
		    Other
	    end;
	Error ->
	    Error
    end.
	
%% Returns {ok, Data} | eof | {error, Reason}.
read(#file_descriptor{module = ?MODULE, data = {Port, _}}, Size)
  when is_integer(Size), 0 =< Size ->
    if
	Size < ?LARGEFILESIZE ->
	    case drv_command(Port, <<?FILE_READ, Size:64>>) of
		{ok, {0, _Data}} when Size =/= 0 ->
		    eof;
		{ok, {_Size, Data}} ->
		    {ok, Data};
		{error, enomem} ->
		    %% Garbage collecting here might help if
		    %% the current processes have some old binaries left.
		    erlang:garbage_collect(),
		    case drv_command(Port, <<?FILE_READ, Size:64>>) of
			{ok, {0, _Data}} when Size =/= 0 ->
			    eof;
			{ok, {_Size, Data}} ->
			    {ok, Data};
			Other ->
			    Other
		    end;
		Error ->
		    Error
	    end;
	true ->
	    {error, einval}
    end.

%% Returns {ok, [Data|eof, ...]} | {error, Reason}
pread(#file_descriptor{module = ?MODULE, data = {Port, _}}, L)
  when is_list(L) ->
    pread_int(Port, L, 0, []).

pread_int(_, [], 0, []) ->
    {ok, []};
pread_int(Port, [], N, Spec) ->
    drv_command_nt(Port, [?FILE_PREADV, erlang:dt_prepend_vm_tag_data(<<0:32, N:32>>) | reverse(Spec)],undefined);
pread_int(Port, [{Offs, Size} | T], N, Spec)
  when is_integer(Offs), is_integer(Size), 0 =< Size ->
    if
	-(?LARGEFILESIZE) =< Offs, Offs < ?LARGEFILESIZE,
	Size < ?LARGEFILESIZE ->
	    pread_int(Port, T, N+1, [<<Offs:64/signed, Size:64>> | Spec]);
	true ->
	    {error, einval}
    end;
pread_int(_, [_|_], _N, _Spec) ->
    {error, badarg}.



%% Returns {ok, Data} | eof | {error, Reason}.
pread(#file_descriptor{module = ?MODULE, data = {Port, _}}, Offs, Size) 
  when is_integer(Offs), is_integer(Size), 0 =< Size ->
    if
	-(?LARGEFILESIZE) =< Offs, Offs < ?LARGEFILESIZE,
	Size < ?LARGEFILESIZE ->
	    case drv_command_nt(Port, 
				[?FILE_PREADV, erlang:dt_prepend_vm_tag_data(<<0:32, 1:32,
									     Offs:64/signed, Size:64>>)], undefined) of
		{ok, [eof]} ->
		    eof;
		{ok, [Data]} ->
		    {ok, Data};
		Error ->
		    Error
	    end;
	true ->
	    {error, einval}
    end;
pread(#file_descriptor{module = ?MODULE, data = {_, _}}, _, _) ->
    {error, badarg}.



%% Returns {ok, Position} | {error, Reason}.
position(#file_descriptor{module = ?MODULE, data = {Port, _}}, At) ->
    case lseek_position(At) of
	{Offs, Whence}
	when -(?LARGEFILESIZE) =< Offs, Offs < ?LARGEFILESIZE ->
	    drv_command(Port, <<?FILE_LSEEK, Offs:64/signed, Whence:32>>);
	{_, _} ->
	    {error, einval};
	Reason ->
	    {error, Reason}
    end.

%% Returns {error, Reason} | ok.
truncate(#file_descriptor{module = ?MODULE, data = {Port, _}}) ->
    drv_command(Port, <<?FILE_TRUNCATE>>).



%% Returns {error, Reason} | {ok, BytesCopied}
copy(#file_descriptor{module = ?MODULE} = Source,
     #file_descriptor{module = ?MODULE} = Dest,
     Length)
  when is_integer(Length), Length >= 0;
       is_atom(Length) ->
    %% XXX Should be moved down to the driver for optimization.
    file:copy_opened(Source, Dest, Length).



ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE,
				    data = {_, _}} = Handle,
		   Offs,
		   Infinity) when is_atom(Infinity) ->
    ipread_s32bu_p32bu(Handle, Offs, (1 bsl 31)-1);
ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE, data = {Port, _}},
		   Offs,
		   MaxSize)
  when is_integer(Offs), is_integer(MaxSize) ->
    if
	-(?LARGEFILESIZE) =< Offs, Offs < ?LARGEFILESIZE,
	0 =< MaxSize, MaxSize < (1 bsl 31) ->
	    drv_command(Port, <<?FILE_IPREAD, ?IPREAD_S32BU_P32BU,
			       Offs:64, MaxSize:32>>);
	true ->
	    {error, einval}
    end;
ipread_s32bu_p32bu(#file_descriptor{module = ?MODULE, data = {_, _}},
		   _Offs,
		   _MaxSize) ->
    {error, badarg}.



%% Returns {ok, Contents} | {error, Reason}
read_file(File) when (is_list(File) orelse is_binary(File)) ->
    case drv_open(?FD_DRV, [binary]) of
	{ok, Port} ->
	    Result = read_file(Port, File),
	    close(Port),
	    Result;
	{error, _} = Error ->
	    Error
    end;
read_file(_) ->
    {error, badarg}.

%% Takes a Port opened with open/1.
read_file(Port, File) when is_port(Port),
			   (is_list(File) orelse is_binary(File)) ->
    Cmd = [?FILE_READ_FILE | pathname(File)],
    case drv_command(Port, Cmd) of
	{error, enomem} ->
	    %% It could possibly help to do a 
	    %% garbage collection here, 
	    %% if the file server has some references
	    %% to binaries read earlier.
	    erlang:garbage_collect(),
	    drv_command(Port, Cmd);
	Result ->
	    Result
    end;
read_file(_,_) ->
    {error, badarg}.

    

%% Returns {error, Reason} | ok.
write_file(File, Bin) when (is_list(File) orelse is_binary(File)) ->
    case open(File, [binary, write]) of
	{ok, Handle} ->
	    Result = write(Handle, Bin),
	    close(Handle),
	    Result;
	Error ->
	    Error
    end;
write_file(_, _) -> 
    {error, badarg}.


%% Returns {error, Reason} | {ok, BytesCopied}
%sendfile(_,_,_,_,_,_,_,_,_,_) ->
%    {error, enotsup};
sendfile(#file_descriptor{module = ?MODULE, data = {Port, _}},
	 Dest, Offset, Bytes, _ChunkSize, Headers, Trailers,
	 Flags) ->
    case erlang:port_get_data(Dest) of
	Data when Data == inet_tcp; Data == inet6_tcp ->
	    ok = inet:lock_socket(Dest,true),
	    {ok, DestFD} = prim_inet:getfd(Dest),
	    IntFlags = translate_sendfile_flags(Flags),
	    try drv_command(Port, [<<?FILE_SENDFILE, DestFD:32,
				     IntFlags:8,
				     Offset:64/unsigned,
				     Bytes:64/unsigned,
				     (iolist_size(Headers)):32/unsigned,
				     (iolist_size(Trailers)):32/unsigned>>,
				   Headers,Trailers])
	    after
		ok = inet:lock_socket(Dest,false)
	    end;
	_Else ->
	    {error,badarg}
    end.

translate_sendfile_flags([{use_threads,true}|T]) ->
    ?EFILE_SENDFILE_USE_THREADS bor translate_sendfile_flags(T);
translate_sendfile_flags([_|T]) ->
    translate_sendfile_flags(T);
translate_sendfile_flags([]) ->
    0.


%%%-----------------------------------------------------------------
%%% Functions operating on files without handle to the file. ?DRV.
%%%
%%% Supposed to be called by applications through module file.



%% Returns {ok, Port}, the Port should be used as first argument in all
%% the following functions. Returns {error, Reason} upon failure.
start() ->
    drv_open(?DRV, [binary]).

stop(Port) when is_port(Port) ->
    try erlang:port_close(Port) of
	_ ->
	    ok
    catch
	_:_ ->
	    ok
    end.



%%% The following functions take an optional Port as first argument.
%%% If the port is not supplied, a temporary one is opened and then
%%% closed after the request has been performed.



%% get_cwd/{0,1,2}

get_cwd() ->
    get_cwd_int(0).

get_cwd(Port) when is_port(Port) ->
    get_cwd_int(Port, 0);
get_cwd([]) ->
    get_cwd_int(0);
get_cwd([Letter, $: | _]) when $a =< Letter, Letter =< $z ->
    get_cwd_int(Letter - $a + 1);
get_cwd([Letter, $: | _]) when $A =< Letter, Letter =< $Z ->
    get_cwd_int(Letter - $A + 1);
get_cwd([_|_]) ->
    {error, einval};
get_cwd(_) ->
    {error, badarg}.

get_cwd(Port, []) when is_port(Port) ->
    get_cwd_int(Port, 0);
get_cwd(Port, [Letter, $: | _])
  when is_port(Port), $a =< Letter, Letter =< $z ->
    get_cwd_int(Port, Letter - $a + 1);
get_cwd(Port, [Letter, $: | _])
  when is_port(Port), $A =< Letter, Letter =< $Z ->
    get_cwd_int(Port, Letter - $A + 1);
get_cwd(Port, [_|_]) when is_port(Port) ->
    {error, einval};
get_cwd(_, _) ->
    {error, badarg}.

get_cwd_int(Drive) ->
    get_cwd_int({?DRV, [binary]}, Drive).

get_cwd_int(Port, Drive) ->
    drv_command(Port, <<?FILE_PWD, Drive>>,
		fun handle_fname_response/1).



%% set_cwd/{1,2}

set_cwd(Dir) ->
    set_cwd_int({?DRV, [binary]}, Dir).

set_cwd(Port, Dir) when is_port(Port) ->
    set_cwd_int(Port, Dir).

set_cwd_int(Port, Dir) when is_binary(Dir) ->
    case prim_file:is_translatable(Dir) of
	false ->
	    {error, no_translation};
	true ->
	    drv_command(Port, [?FILE_CHDIR, pathname(Dir)])
    end;
set_cwd_int(Port, Dir) when is_list(Dir) ->
    drv_command(Port, [?FILE_CHDIR, pathname(Dir)]);
set_cwd_int(_, _) ->
    {error, badarg}.



%% delete/{1,2}

delete(File) ->
    delete_int({?DRV, [binary]}, File).

delete(Port, File) when is_port(Port) ->
    delete_int(Port, File).

delete_int(Port, File) ->
    drv_command(Port, [?FILE_DELETE, pathname(File)]).



%% rename/{2,3}

rename(From, To) ->
    rename_int({?DRV, [binary]}, From, To).

rename(Port, From, To) when is_port(Port) ->
    rename_int(Port, From, To).

rename_int(Port, From, To) ->
    drv_command(Port, [?FILE_RENAME, pathname(From), pathname(To)]).



%% make_dir/{1,2}

make_dir(Dir) ->
    make_dir_int({?DRV, [binary]}, Dir).

make_dir(Port, Dir) when is_port(Port) ->
    make_dir_int(Port, Dir).

make_dir_int(Port, Dir) ->
    drv_command(Port, [?FILE_MKDIR, pathname(Dir)]).



%% del_dir/{1,2}

del_dir(Dir) ->
    del_dir_int({?DRV, [binary]}, Dir).

del_dir(Port, Dir) when is_port(Port) ->
    del_dir_int(Port, Dir).

del_dir_int(Port, Dir) ->
    drv_command(Port, [?FILE_RMDIR, pathname(Dir)]).



%% read_file_info/{1,2,3}

read_file_info(File) ->
    read_file_info_int({?DRV, [binary]}, File, local).

read_file_info(Port, File) when is_port(Port) ->
    read_file_info_int(Port, File, local);
read_file_info(File, Opts) ->
    read_file_info_int({?DRV, [binary]}, File, plgv(time, Opts, local)).

read_file_info(Port, File, Opts) when is_port(Port) ->
    read_file_info_int(Port, File, plgv(time, Opts, local)).

read_file_info_int(Port, File, TimeType) ->
    try
	case drv_command(Port, [?FILE_FSTAT, pathname(File)]) of
	    {ok, FI} -> {ok, FI#file_info{
			ctime = from_seconds(FI#file_info.ctime, TimeType),
			mtime = from_seconds(FI#file_info.mtime, TimeType),
			atime = from_seconds(FI#file_info.atime, TimeType)
		    }};
	    Error    -> Error
	end
    catch
	error:_ -> {error, badarg}
    end.


%% altname/{1,2}

altname(File) ->
    altname_int({?DRV, [binary]}, File).

altname(Port, File) when is_port(Port) ->
    altname_int(Port, File).

altname_int(Port, File) ->
    drv_command(Port, [?FILE_ALTNAME, pathname(File)],
		fun handle_fname_response/1).

%% write_file_info/{2,3,4}

write_file_info(File, Info) ->
    write_file_info_int({?DRV, [binary]}, File, Info, local).

write_file_info(Port, File, Info) when is_port(Port) ->
    write_file_info_int(Port, File, Info, local);
write_file_info(File, Info, Opts) ->
    write_file_info_int({?DRV, [binary]}, File, Info, plgv(time, Opts, local)).

write_file_info(Port, File, Info, Opts) when is_port(Port) ->
    write_file_info_int(Port, File, Info, plgv(time, Opts, local)).

write_file_info_int(Port, File, 
		    #file_info{mode=Mode, 
			       uid=Uid, 
			       gid=Gid,
			       atime=Atime0, 
			       mtime=Mtime0, 
			       ctime=Ctime0},
		       TimeType) ->

    % Atime and/or Mtime might be undefined
    %  - use localtime() for atime, if atime is undefined
    %  - use atime as mtime if mtime is undefined
    %  - use mtime as ctime if ctime is undefined

    try
	Atime = file_info_validate_atime(Atime0, TimeType),
	Mtime = file_info_validate_mtime(Mtime0, Atime),
	Ctime = file_info_validate_ctime(Ctime0, Mtime),

	drv_command(Port, [?FILE_WRITE_INFO, 
		int_to_int32bytes(Mode), 
		int_to_int32bytes(Uid), 
		int_to_int32bytes(Gid),
		int_to_int64bytes(to_seconds(Atime, TimeType)), 
		int_to_int64bytes(to_seconds(Mtime, TimeType)), 
		int_to_int64bytes(to_seconds(Ctime, TimeType)),
		pathname(File)])
    catch
	error:_ -> {error, badarg}
    end.


file_info_validate_atime(Atime, _) when Atime =/= undefined -> Atime;
file_info_validate_atime(undefined, local)     -> erlang:localtime();
file_info_validate_atime(undefined, universal) -> erlang:universaltime();
file_info_validate_atime(undefined, posix)     -> erlang:universaltime_to_posixtime(erlang:universaltime()).

file_info_validate_mtime(undefined, Atime) -> Atime;
file_info_validate_mtime(Mtime, _)         -> Mtime.

file_info_validate_ctime(undefined, Mtime) -> Mtime;
file_info_validate_ctime(Ctime, _)         -> Ctime.

%% make_link/{2,3}

make_link(Old, New) ->
    make_link_int({?DRV, [binary]}, Old, New).

make_link(Port, Old, New) when is_port(Port) ->
    make_link_int(Port, Old, New).

make_link_int(Port, Old, New) ->
    drv_command(Port, [?FILE_LINK, pathname(Old), pathname(New)]).



%% make_symlink/{2,3}

make_symlink(Old, New) ->
    make_symlink_int({?DRV, [binary]}, Old, New).

make_symlink(Port, Old, New) when is_port(Port) ->
    make_symlink_int(Port, Old, New).

make_symlink_int(Port, Old, New) ->
    drv_command(Port, [?FILE_SYMLINK, pathname(Old), pathname(New)]).



%% read_link/{2,3}

read_link(Link) ->
    read_link_int({?DRV, [binary]}, Link).

read_link(Port, Link) when is_port(Port) ->
    read_link_int(Port, Link).

read_link_int(Port, Link) ->
    drv_command(Port, [?FILE_READLINK, pathname(Link)],
		fun handle_fname_response/1).

%% read_link_all/{2,3}

read_link_all(Link) ->
    read_link_all_int({?DRV, [binary]}, Link).

read_link_all(Port, Link) when is_port(Port) ->
    read_link_all_int(Port, Link).

read_link_all_int(Port, Link) ->
    drv_command(Port, [?FILE_READLINK, pathname(Link)],
		fun handle_fname_response_all/1).



%% read_link_info/{2,3}

read_link_info(Link) ->
    read_link_info_int({?DRV, [binary]}, Link, local).

read_link_info(Port, Link) when is_port(Port) ->
    read_link_info_int(Port, Link, local);

read_link_info(Link, Opts) ->
    read_link_info_int({?DRV, [binary]}, Link, plgv(time, Opts, local)).

read_link_info(Port, Link, Opts) when is_port(Port) ->
    read_link_info_int(Port, Link, plgv(time, Opts, local)).


read_link_info_int(Port, Link, TimeType) ->
    try
	case drv_command(Port, [?FILE_LSTAT, pathname(Link)]) of
	    {ok, FI} -> {ok, FI#file_info{
			ctime = from_seconds(FI#file_info.ctime, TimeType),
			mtime = from_seconds(FI#file_info.mtime, TimeType),
			atime = from_seconds(FI#file_info.atime, TimeType)
		    }};
	    Error    -> Error
	end
    catch
	error:_ -> {error, badarg}
    end.

%% list_dir/{1,2}

list_dir(Dir) ->
    list_dir_int({?DRV, [binary]}, Dir).

list_dir(Port, Dir) when is_port(Port) ->
    list_dir_int(Port, Dir).

list_dir_int(Port, Dir) ->
    drv_command(Port, [?FILE_READDIR, pathname(Dir)],
		fun(P) ->
			case list_dir_response(P, []) of
			    {ok, RawNames} ->
				try
				    {ok, list_dir_convert(RawNames)}
				catch
				    throw:Reason ->
					Reason
				end;
			    Error ->
				Error
			end
		end).

list_dir_all(Dir) ->
    list_dir_all_int({?DRV, [binary]}, Dir).

list_dir_all(Port, Dir) when is_port(Port) ->
    list_dir_all_int(Port, Dir).

list_dir_all_int(Port, Dir) ->
    drv_command(Port, [?FILE_READDIR, pathname(Dir)],
		fun(P) ->
			case list_dir_response(P, []) of
			    {ok, RawNames} ->
				{ok, list_dir_convert_all(RawNames)};
			    Error ->
				Error
			end
		end).

list_dir_response(Port, Acc0) ->
    case drv_get_response(Port) of
	{lfname, []} ->
	    {ok, Acc0};
	{lfname, Names} ->
	    Acc = [Name || <<L:16,Name:L/binary>> <= Names] ++ Acc0,
	    list_dir_response(Port, Acc);
	Error ->
	    Error
    end.

list_dir_convert([Name|Names]) ->
    %% If the filename cannot be converted, return error or ignore
    %% with optional error logger warning, depending on +fn{u|a}{i|e|w}
    %% emulator switches.
    case prim_file:internal_native2name(Name) of
	{error, warning} ->
	    error_logger:warning_msg("Non-unicode filename ~p ignored\n",
				     [Name]),
	    list_dir_convert(Names);
	{error, ignore} ->
	    list_dir_convert(Names);
	{error, error} ->
	    throw({error, {no_translation, Name}});
	Converted when is_list(Converted) ->
	    [Converted|list_dir_convert(Names)]
    end;
list_dir_convert([]) -> [].

list_dir_convert_all([Name|Names]) ->
    %% If the filename cannot be converted, retain the filename as
    %% a binary.
    case prim_file:internal_native2name(Name) of
	{error, _} ->
	    [Name|list_dir_convert_all(Names)];
	Converted when is_list(Converted) ->
	    [Converted|list_dir_convert_all(Names)]
    end;
list_dir_convert_all([]) -> [].

%%%-----------------------------------------------------------------
%%% Functions to communicate with the driver

handle_fname_response(Port) ->
    case drv_get_response(Port) of
	{fname, Name} ->
	    case prim_file:internal_native2name(Name) of
		{error, warning} ->
		    error_logger:warning_msg("Non-unicode filename ~p "
					     "ignored when reading link\n",
					     [Name]),
		    {error, einval};
		{error, _} ->
		    {error, einval};
		Converted when is_list(Converted) ->
		    {ok, Converted}
	    end;
	Error ->
	    Error
    end.

handle_fname_response_all(Port) ->
    case drv_get_response(Port) of
	{fname, Name} ->
	    case prim_file:internal_native2name(Name) of
		{error, _} ->
		    {ok, Name};
		Converted when is_list(Converted) ->
		    {ok, Converted}
	    end;
	Error ->
	    Error
    end.

%% Opens a driver port and converts any problems into {error, emfile}.
%% Returns {ok, Port} when successful.

drv_open(Driver, Portopts) ->
    try erlang:open_port({spawn_driver, Driver}, Portopts) of
	Port ->
	    {ok, Port}
    catch
	error:Reason ->
	    {error, Reason}
    end.



%% Closes a port in a safe way. Returns ok.

drv_close(Port) ->
    Save = erlang:dt_spread_tag(false),
    try
	try erlang:port_close(Port) catch error:_ -> ok end,
	receive %% Ugly workaround in case the caller==owner traps exits
	    {'EXIT', Port, _Reason} -> 
		ok
	after 0 -> 
		ok
	end
    after
	erlang:dt_restore_tag(Save)
    end.



%% Issues a command to a port and gets the response.
%% If Port is {Driver, Portopts} a port is first opened and 
%% then closed after the result has been received.
%% Returns {ok, Result} or {error, Reason}.

drv_command(Port, Command) ->
    drv_command(Port, Command, undefined).

drv_command(Port, Command, R) when is_binary(Command) ->
    drv_command(Port, Command, true, R);
drv_command(Port, Command, R) ->
    try erlang:iolist_size(Command) of
	_ ->
	    drv_command(Port, Command, true, R)
    catch
	error:Reason ->
	    {error, Reason}
    end.

drv_command(Port, Command, Validated, R) when is_port(Port) ->
    Save = erlang:dt_spread_tag(false),
    try erlang:port_command(Port, erlang:dt_append_vm_tag_data(Command)) of
	true ->
	    drv_get_response(Port, R)
    catch
	%% If the Command is valid, knowing that the port is a port,
	%% a badarg error must mean it is a dead port, that is:
	%% a currently invalid filehandle, -> einval, not badarg.
	error:badarg when Validated ->
	    {error, einval};
	error:badarg ->
	    try erlang:iolist_size(Command) of
		_ -> % Valid
		    {error, einval}
	    catch
		error:_ ->
		    {error, badarg}
	    end;
	error:Reason ->
	    {error, Reason}
    after
	erlang:dt_restore_tag(Save)
    end;
drv_command({Driver, Portopts}, Command, Validated, R) ->
    case drv_open(Driver, Portopts) of
	{ok, Port} ->
	    Result = drv_command(Port, Command, Validated, R),
	    drv_close(Port),
	    Result;
	Error ->
	    Error
    end.
drv_command_nt(Port, Command, R) when is_port(Port) ->
    Save = erlang:dt_spread_tag(false),
    try erlang:port_command(Port, Command) of
	true ->
	    drv_get_response(Port, R)
    catch
	error:badarg ->
	    try erlang:iolist_size(Command) of
		_ -> % Valid
		    {error, einval}
	    catch
		error:_ ->
		    {error, badarg}
	    end;
	error:Reason ->
	    {error, Reason}
    after
	erlang:dt_restore_tag(Save)
    end.


    
%% Receives the response from a driver port.
%% Returns: {ok, ListOrBinary}|{error, Reason}

drv_get_response(Port, undefined) ->
    drv_get_response(Port);
drv_get_response(Port, Fun) when is_function(Fun, 1) ->
    Fun(Port).

drv_get_response(Port) ->
    erlang:bump_reductions(100),
    receive
	{Port, {data, [Response|Rest] = Data}} ->
	    try translate_response(Response, Rest)
	    catch
		error:Reason ->
		    {error, {bad_response_from_port, Data, 
			     {Reason, erlang:get_stacktrace()}}}
	    end;
	{'EXIT', Port, Reason} ->
	    {error, {port_died, Reason}}
    end.


%%%-----------------------------------------------------------------
%%% Utility functions.

%% Converts a list of mode atoms into a mode word for the driver.
%% Returns {Mode, Portopts, Setopts} where Portopts is a list of 
%% options for erlang:open_port/2 and Setopts is a list of 
%% setopt commands to send to the port, or error Reason upon failure.

open_mode(List) when is_list(List) ->
    case open_mode(List, 0, [], []) of
	{Mode, Portopts, Setopts} when Mode band 
			  (?EFILE_MODE_READ bor ?EFILE_MODE_WRITE) 
			  =:= 0 ->
	    {Mode bor ?EFILE_MODE_READ, Portopts, Setopts};
	Other ->
	    Other
    end.

open_mode([raw|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode, Portopts, Setopts);
open_mode([read|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_READ, Portopts, Setopts);
open_mode([write|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_WRITE, Portopts, Setopts);
open_mode([binary|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode, [binary | Portopts], Setopts);
open_mode([compressed|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_COMPRESSED, Portopts, Setopts);
open_mode([append|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_APPEND bor ?EFILE_MODE_WRITE, 
	      Portopts, Setopts);
open_mode([exclusive|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_EXCL, Portopts, Setopts);
open_mode([sync|Rest], Mode, Portopts, Setopts) ->
    open_mode(Rest, Mode bor ?EFILE_MODE_SYNC, Portopts, Setopts);
open_mode([delayed_write|Rest], Mode, Portopts, Setopts) ->
    open_mode([{delayed_write, 64*1024, 2000}|Rest], Mode,
	      Portopts, Setopts);
open_mode([{delayed_write, Size, Delay}|Rest], Mode, Portopts, Setopts) 
  when is_integer(Size), 0 =< Size, is_integer(Delay), 0 =< Delay ->
    if
	Size < ?LARGEFILESIZE, Delay < 1 bsl 64 ->
	    open_mode(Rest, Mode, Portopts, 
		      [<<?FILE_SETOPT, ?FILE_OPT_DELAYED_WRITE,
			Size:64, Delay:64>> 
		       | Setopts]);
	true ->
	    einval
    end;
open_mode([read_ahead|Rest], Mode, Portopts, Setopts) ->
    open_mode([{read_ahead, 64*1024}|Rest], Mode, Portopts, Setopts);
open_mode([{read_ahead, Size}|Rest], Mode, Portopts, Setopts)
  when is_integer(Size), 0 =< Size ->
    if
	Size < ?LARGEFILESIZE ->
	    open_mode(Rest, Mode, Portopts,
		      [<<?FILE_SETOPT, ?FILE_OPT_READ_AHEAD,
			Size:64>> | Setopts]);
	true ->
	    einval
    end;
open_mode([], Mode, Portopts, Setopts) ->
    {Mode, reverse(Portopts), reverse(Setopts)};
open_mode(_, _Mode, _Portopts, _Setopts) ->
    badarg.



%% Converts a position tuple {bof, X} | {cur, X} | {eof, X} into
%% {Offset, OriginCode} for the driver.
%% Returns badarg upon failure.

lseek_position(Pos)
  when is_integer(Pos) ->
    lseek_position({bof, Pos});
lseek_position(bof) ->
    lseek_position({bof, 0});
lseek_position(cur) ->
    lseek_position({cur, 0});
lseek_position(eof) ->
    lseek_position({eof, 0});
lseek_position({bof, Offset})
  when is_integer(Offset) ->
    {Offset, ?EFILE_SEEK_SET};
lseek_position({cur, Offset})
  when is_integer(Offset) ->
    {Offset, ?EFILE_SEEK_CUR};
lseek_position({eof, Offset})
  when is_integer(Offset) ->
    {Offset, ?EFILE_SEEK_END};
lseek_position(_) ->
    badarg.



%% Translates the response from the driver into 
%% {ok, Result} or {error, Reason}.

translate_response(?FILE_RESP_OK, []) ->
    ok;
translate_response(?FILE_RESP_ERROR, List) when is_list(List) ->
    {error, list_to_atom(List)};
translate_response(?FILE_RESP_NUMBER, List) ->
    {N, []} = get_uint64(List),
    {ok, N};
translate_response(?FILE_RESP_DATA, List) ->
    {_N, _Data} = ND = get_uint64(List),
    {ok, ND};
translate_response(?FILE_RESP_INFO, List) when is_list(List) ->
    {ok, transform_info(List)};
translate_response(?FILE_RESP_NUMERR, L0) ->
    {N, L1} = get_uint64(L0),
    {error, {N, list_to_atom(L1)}};
translate_response(?FILE_RESP_LDATA, List) ->
    {ok, transform_ldata(List)};
translate_response(?FILE_RESP_N2DATA, 
		   <<Offset:64, 0:64, Size:64>>) ->
    {ok, {Size, Offset, eof}};
translate_response(?FILE_RESP_N2DATA, 
		   [<<Offset:64, 0:64, Size:64>> | <<>>]) ->
    {ok, {Size, Offset, eof}};
translate_response(?FILE_RESP_N2DATA = X, 
		   [<<_:64, 0:64, _:64>> | _] = Data) ->
    {error, {bad_response_from_port, [X | Data]}};
translate_response(?FILE_RESP_N2DATA = X, 
		   [<<_:64, _:64, _:64>> | <<>>] = Data) ->
    {error, {bad_response_from_port, [X | Data]}};
translate_response(?FILE_RESP_N2DATA, 
		   [<<Offset:64, _ReadSize:64, Size:64>> | D]) ->
    {ok, {Size, Offset, D}};
translate_response(?FILE_RESP_N2DATA = X, L0) when is_list(L0) ->
    {Offset, L1}    = get_uint64(L0),
    {ReadSize, L2}  = get_uint64(L1),
    {Size, L3}      = get_uint64(L2),
    case {ReadSize, L3} of
	{0, []} ->
	    {ok, {Size, Offset, eof}};
	{0, _} ->
	    {error, {bad_response_from_port, [X | L0]}};
	{_, []} ->
	    {error, {bad_response_from_port, [X | L0]}};
	_ ->
	    {ok, {Size, Offset, L3}}
    end;
translate_response(?FILE_RESP_EOF, []) ->
    eof;
translate_response(?FILE_RESP_FNAME, Data) ->
    {fname, Data};
translate_response(?FILE_RESP_LFNAME, Data) ->
    {lfname, Data};
translate_response(?FILE_RESP_ALL_DATA, Data) ->
    {ok, Data};
translate_response(X, Data) ->
    {error, {bad_response_from_port, [X | Data]}}.

transform_info([
    Hsize1, Hsize2, Hsize3, Hsize4, 
    Lsize1, Lsize2, Lsize3, Lsize4,
    Type1,  Type2,  Type3,  Type4,
    Atime1, Atime2, Atime3, Atime4, Atime5, Atime6, Atime7, Atime8,
    Mtime1, Mtime2, Mtime3, Mtime4, Mtime5, Mtime6, Mtime7, Mtime8,
    Ctime1, Ctime2, Ctime3, Ctime4, Ctime5, Ctime6, Ctime7, Ctime8,
    Mode1,  Mode2,  Mode3,  Mode4,
    Links1, Links2, Links3, Links4,
    Major1, Major2, Major3, Major4,
    Minor1, Minor2, Minor3, Minor4,
    Inode1, Inode2, Inode3, Inode4,
    Uid1,   Uid2,   Uid3,   Uid4,
    Gid1,   Gid2,   Gid3,   Gid4,
    Access1,Access2,Access3,Access4]) ->
    #file_info {
		size   = uint32(Hsize1,Hsize2,Hsize3,Hsize4)*16#100000000 + uint32(Lsize1,Lsize2,Lsize3,Lsize4),
		type   = file_type(uint32(Type1,Type2,Type3,Type4)),
		access = file_access(uint32(Access1,Access2,Access3,Access4)),
		atime  = sint64(Atime1, Atime2, Atime3, Atime4, Atime5, Atime6, Atime7, Atime8),
		mtime  = sint64(Mtime1, Mtime2, Mtime3, Mtime4, Mtime5, Mtime6, Mtime7, Mtime8),
		ctime  = sint64(Ctime1, Ctime2, Ctime3, Ctime4, Ctime5, Ctime6, Ctime7, Ctime8),
		mode   = uint32(Mode1,Mode2,Mode3,Mode4),
		links  = uint32(Links1,Links2,Links3,Links4),
		major_device = uint32(Major1,Major2,Major3,Major4),
		minor_device = uint32(Minor1,Minor2,Minor3,Minor4),
		inode = uint32(Inode1,Inode2,Inode3,Inode4),
		uid   = uint32(Uid1,Uid2,Uid3,Uid4),
		gid   = uint32(Gid1,Gid2,Gid3,Gid4)
	    }.
    
    
file_type(1) -> device;
file_type(2) -> directory;
file_type(3) -> regular;
file_type(4) -> symlink;
file_type(_) -> other.

file_access(0) -> none;   
file_access(1) -> write;
file_access(2) -> read;
file_access(3) -> read_write.

int_to_int32bytes(Int) when is_integer(Int) ->
    <<Int:32>>;
int_to_int32bytes(undefined) ->
    <<-1:32>>.

int_to_int64bytes(Int) when is_integer(Int) ->
    <<Int:64/signed>>.


sint64(I1,I2,I3,I4,I5,I6,I7,I8) when I1 > 127 ->
    ((I1 bsl 56) bor (I2 bsl 48) bor (I3 bsl 40) bor (I4 bsl 32) bor
	(I5 bsl 24) bor (I6 bsl 16) bor (I7 bsl  8) bor I8) - (1 bsl 64);
sint64(I1,I2,I3,I4,I5,I6,I7,I8) ->
    ((I1 bsl 56) bor (I2 bsl 48) bor (I3 bsl 40) bor (I4 bsl 32) bor
	(I5 bsl 24) bor (I6 bsl 16) bor (I7 bsl  8) bor I8).


uint32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

get_uint64(L0) ->
    {X1, L1} = get_uint32(L0),
    {X2, L2} = get_uint32(L1),
    {(X1 bsl 32) bor X2, L2}.

get_uint32([X1,X2,X3,X4|List]) ->
    {(((((X1 bsl 8) bor X2) bsl 8) bor X3) bsl 8) bor X4, List}.


%% Binary mode
transform_ldata(<<0:32, 0:32>>) ->
    [];
transform_ldata([<<0:32, N:32, Sizes/binary>> | Datas]) ->
    transform_ldata(N, Sizes, Datas, []);
%% List mode
transform_ldata([_,_,_,_,_,_,_,_|_] = L0) ->
    {0, L1} = get_uint32(L0),
    {N, L2} = get_uint32(L1),
    transform_ldata(N, L2, []).

%% List mode
transform_ldata(0, List, Sizes) ->
    transform_ldata(0, List, reverse(Sizes), []);
transform_ldata(N, L0, Sizes) ->
    {Size, L1} = get_uint64(L0),
    transform_ldata(N-1, L1, [Size | Sizes]).

%% Binary mode
transform_ldata(1, <<0:64>>, <<>>, R) ->
    reverse(R, [eof]);
transform_ldata(1, <<Size:64>>, Data, R) 
  when byte_size(Data) =:= Size ->
    reverse(R, [Data]);
transform_ldata(N, <<0:64, Sizes/binary>>, [<<>> | Datas], R) ->
    transform_ldata(N-1, Sizes, Datas, [eof | R]);
transform_ldata(N, <<Size:64, Sizes/binary>>, [Data | Datas], R) 
  when byte_size(Data) =:= Size ->
    transform_ldata(N-1, Sizes, Datas, [Data | R]);
%% List mode
transform_ldata(0, [], [], R) ->
    reverse(R);
transform_ldata(0, List, [0 | Sizes], R) ->
    transform_ldata(0, List, Sizes, [eof | R]);
transform_ldata(0, List, [Size | Sizes], R) ->
    {Front, Rear} = lists_split(List, Size),
    transform_ldata(0, Rear, Sizes, [Front | R]).

lists_split(List, 0) when is_list(List) ->
    {[], List};
lists_split(List, N) when is_list(List), is_integer(N), N < 0 ->
    erlang:error(badarg, [List, N]);
lists_split(List, N) when is_list(List), is_integer(N) ->
    case lists_split(List, N, []) of
	premature_end_of_list ->
	    erlang:error(badarg, [List, N]);
	Result ->
	    Result
    end.

lists_split(List, 0, Rev) ->
    {reverse(Rev), List};
lists_split([], _, _) ->
    premature_end_of_list;
lists_split([Hd | Tl], N, Rev) ->
    lists_split(Tl, N-1, [Hd | Rev]).

%% We KNOW that lists:reverse/2 is a BIF.

reverse(X) -> lists:reverse(X, []).
reverse(L, T) -> lists:reverse(L, T).

% Will add zero termination too
% The 'EXIT' tuple from a bad argument will eventually generate an error
% in list_to_binary, which is caught and generates the {error,badarg} return
pathname(File) ->
    (catch prim_file:internal_name2native(File)).


%% proplist:get_value/3
plgv(K, [{K, V}|_], _) -> V;
plgv(K, [_|KVs], D)    -> plgv(K, KVs, D);
plgv(_, [], D)         -> D.

%%
%% We don't actually want this here
%% We want to use posix time in all prim but erl_prim_loader makes that tricky
%% It is probably needed to redo the whole erl_prim_loader

from_seconds(Seconds, posix) when is_integer(Seconds) ->
    Seconds;
from_seconds(Seconds, universal) when is_integer(Seconds) ->
    erlang:posixtime_to_universaltime(Seconds);
from_seconds(Seconds, local) when is_integer(Seconds) ->
    erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(Seconds)).

to_seconds(Seconds, posix) when is_integer(Seconds) ->
    Seconds;
to_seconds({_,_} = Datetime, universal) ->
    erlang:universaltime_to_posixtime(Datetime);
to_seconds({_,_} = Datetime, local) ->
    erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(Datetime)).
