%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
	 write/2, pwrite/2, pwrite/3, read/2, read_line/1, pread/2, pread/3, copy/3]).

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
	 read_file_info/1, read_file_info/2,
	 altname/1, altname/2,
	 write_file_info/2, write_file_info/3,
	 make_link/2, make_link/3,
	 make_symlink/2, make_symlink/3,
	 read_link/1, read_link/2,
	 read_link_info/1, read_link_info/2,
	 list_dir/1, list_dir/2]).
%% How to start and stop the ?DRV port.
-export([start/0, stop/1]).

%% Debug exports
-export([open_int/4, open_mode/1, open_mode/4]).

%%%-----------------------------------------------------------------
%%% Includes and defines

-include("file.hrl").

-define(DRV,    efile).
-define(FD_DRV, efile).

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

%% Open modes for the driver's open function.
-define(EFILE_MODE_READ,       1).
-define(EFILE_MODE_WRITE,      2).
-define(EFILE_MODE_READ_WRITE, 3).  
-define(EFILE_MODE_APPEND,     4).
-define(EFILE_COMPRESSED,      8).
-define(EFILE_MODE_EXCL,       16).

%% Use this mask to get just the mode bits to be passed to the driver.
-define(EFILE_MODE_MASK, 31).

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


%%%-----------------------------------------------------------------
%%% Functions operating on a file through a handle. ?FD_DRV.
%%%
%%% Generic file contents operations.
%%%
%%% Supposed to be called by applications through module file.


%% Opens a file using the driver port Port. Returns {error, Reason}
%% | {ok, FileDescriptor}
open(Port, File, ModeList) when is_port(Port), 
                                is_list(File), 
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
open(File, ModeList) when is_list(File), is_list(ModeList) ->
    case open_mode(ModeList) of
	{Mode, Portopts, Setopts} ->
	    open_int({?FD_DRV, Portopts}, File, Mode, Setopts);
	Reason ->
	    {error, Reason}
    end;
open(_, _) ->
    {error, badarg}.

%% Opens a port that can be used for open/3 or read_file/2.
%% Returns {ok, Port} | {error, Reason}.
open(Portopts) when is_list(Portopts) ->
    case drv_open(?FD_DRV, Portopts) of
	{error, _} = Error ->
	    Error;
	Other ->
	    Other
    end;
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
    case drv_command(Port, [<<?FILE_OPEN, M:32>>, File, 0]) of
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
write(#file_descriptor{module = ?MODULE, data = {Port, _}}, Bytes) ->
    case drv_command(Port, [?FILE_WRITE,Bytes]) of
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
    Header = list_to_binary([<<?FILE_PWRITEV, N:32>> | reverse(Spec)]),
    case drv_command_raw(Port, [Header | reverse(Data)]) of
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
		    %% the current processes has some old binaries left.
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
    drv_command(Port, [<<?FILE_PREADV, 0:32, N:32>> | reverse(Spec)]);
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
	    case drv_command(Port, 
			     <<?FILE_PREADV, 0:32, 1:32,
			      Offs:64/signed, Size:64>>) of
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

%% Returns {error, Reaseon} | ok.
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
read_file(File) ->
    case drv_open(?FD_DRV, [binary]) of
	{ok, Port} ->
	    Result = read_file(Port, File),
	    close(Port),
	    Result;
	{error, _} = Error ->
	    Error
    end.

%% Takes a Port opened with open/1.
read_file(Port, File) when is_port(Port) ->
    Cmd = [?FILE_READ_FILE | File],
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
    end.

    

%% Returns {error, Reason} | ok.
write_file(File, Bin) ->
    case open(File, [binary, write]) of
	{ok, Handle} ->
	    Result = write(Handle, Bin),
	    close(Handle),
	    Result;
	Error ->
	    Error
    end.



%%%-----------------------------------------------------------------
%%% Functions operating on files without handle to the file. ?DRV.
%%%
%%% Supposed to be called by applications through module file.



%% Returns {ok, Port}, the Port should be used as first argument in all
%% the following functions. Returns {error, Reason} upon failure.
start() ->
    try erlang:open_port({spawn, atom_to_list(?DRV)}, []) of
	Port ->
	    {ok, Port}
    catch
	error:Reason ->
	    {error, Reason}
    end.

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
    get_cwd_int({?DRV, []}, Drive).

get_cwd_int(Port, Drive) ->
    drv_command(Port, <<?FILE_PWD, Drive>>).



%% set_cwd/{1,2}

set_cwd(Dir) ->
    set_cwd_int({?DRV, []}, Dir).

set_cwd(Port, Dir) when is_port(Port) ->
    set_cwd_int(Port, Dir).

set_cwd_int(Port, Dir0) ->
    Dir = 
	(catch
	 case os:type() of
	     vxworks -> 
		 %% chdir on vxworks doesn't support
		 %% relative paths
		 %% must call get_cwd from here and use
		 %% absname/2, since
		 %% absname/1 uses file:get_cwd ...
		 case get_cwd_int(Port, 0) of
		     {ok, AbsPath} ->
			 filename:absname(Dir0, AbsPath);
		     _Badcwd ->
			 Dir0
		 end;
	     _Else ->
		 Dir0
	 end),
    %% Dir is now either a string or an EXIT tuple.
    %% An EXIT tuple will fail in the following catch.
    drv_command(Port, [?FILE_CHDIR, Dir, 0]).



%% delete/{1,2}

delete(File) ->
    delete_int({?DRV, []}, File).

delete(Port, File) when is_port(Port) ->
    delete_int(Port, File).

delete_int(Port, File) ->
    drv_command(Port, [?FILE_DELETE, File, 0]).



%% rename/{2,3}

rename(From, To) ->
    rename_int({?DRV, []}, From, To).

rename(Port, From, To) when is_port(Port) ->
    rename_int(Port, From, To).

rename_int(Port, From, To) ->
    drv_command(Port, [?FILE_RENAME, From, 0, To, 0]).



%% make_dir/{1,2}

make_dir(Dir) ->
    make_dir_int({?DRV, []}, Dir).

make_dir(Port, Dir) when is_port(Port) ->
    make_dir_int(Port, Dir).

make_dir_int(Port, Dir) ->
    drv_command(Port, [?FILE_MKDIR, Dir, 0]).



%% del_dir/{1,2}

del_dir(Dir) ->
    del_dir_int({?DRV, []}, Dir).

del_dir(Port, Dir) when is_port(Port) ->
    del_dir_int(Port, Dir).

del_dir_int(Port, Dir) ->
    drv_command(Port, [?FILE_RMDIR, Dir, 0]).



%% read_file_info/{1,2}

read_file_info(File) ->
    read_file_info_int({?DRV, []}, File).

read_file_info(Port, File) when is_port(Port) ->
    read_file_info_int(Port, File).

read_file_info_int(Port, File) ->
    drv_command(Port, [?FILE_FSTAT, File, 0]).

%% altname/{1,2}

altname(File) ->
    altname_int({?DRV, []}, File).

altname(Port, File) when is_port(Port) ->
    altname_int(Port, File).

altname_int(Port, File) ->
    drv_command(Port, [?FILE_ALTNAME, File, 0]).


%% write_file_info/{2,3}

write_file_info(File, Info) ->
    write_file_info_int({?DRV, []}, File, Info).

write_file_info(Port, File, Info) when is_port(Port) ->
    write_file_info_int(Port, File, Info).

write_file_info_int(Port, 
		    File, 
		    #file_info{mode=Mode, 
			       uid=Uid, 
			       gid=Gid,
			       atime=Atime0, 
			       mtime=Mtime0, 
			       ctime=Ctime}) ->
    {Atime, Mtime} =
	case {Atime0, Mtime0} of
	    {undefined, Mtime0} -> {erlang:localtime(), Mtime0};
	    {Atime0, undefined} -> {Atime0, Atime0};
	    Complete -> Complete
	end,
    drv_command(Port, [?FILE_WRITE_INFO, 
			int_to_bytes(Mode), 
			int_to_bytes(Uid), 
			int_to_bytes(Gid),
			date_to_bytes(Atime), 
			date_to_bytes(Mtime), 
			date_to_bytes(Ctime),
			File, 0]).



%% make_link/{2,3}

make_link(Old, New) ->
    make_link_int({?DRV, []}, Old, New).

make_link(Port, Old, New) when is_port(Port) ->
    make_link_int(Port, Old, New).

make_link_int(Port, Old, New) ->
    drv_command(Port, [?FILE_LINK, Old, 0, New, 0]).



%% make_symlink/{2,3}

make_symlink(Old, New) ->
    make_symlink_int({?DRV, []}, Old, New).

make_symlink(Port, Old, New) when is_port(Port) ->
    make_symlink_int(Port, Old, New).

make_symlink_int(Port, Old, New) ->
    drv_command(Port, [?FILE_SYMLINK, Old, 0, New, 0]).



%% read_link/{2,3}

read_link(Link) ->
    read_link_int({?DRV, []}, Link).

read_link(Port, Link) when is_port(Port) ->
    read_link_int(Port, Link).

read_link_int(Port, Link) ->
    drv_command(Port, [?FILE_READLINK, Link, 0]).



%% read_link_info/{2,3}

read_link_info(Link) ->
    read_link_info_int({?DRV, []}, Link).

read_link_info(Port, Link) when is_port(Port) ->
    read_link_info_int(Port, Link).

read_link_info_int(Port, Link) ->
    drv_command(Port, [?FILE_LSTAT, Link, 0]).



%% list_dir/{1,2}

list_dir(Dir) ->
    list_dir_int({?DRV, []}, Dir).

list_dir(Port, Dir) when is_port(Port) ->
    list_dir_int(Port, Dir).

list_dir_int(Port, Dir) ->
    drv_command(Port, [?FILE_READDIR, Dir, 0], []).



%%%-----------------------------------------------------------------
%%% Functions to communicate with the driver



%% Opens a driver port and converts any problems into {error, emfile}.
%% Returns {ok, Port} when succesful.

drv_open(Driver, Portopts) ->
    try erlang:open_port({spawn, Driver}, Portopts) of
	Port ->
	    {ok, Port}
    catch
	error:Reason ->
	    {error,Reason}
    end.



%% Closes a port in a safe way. Returns ok.

drv_close(Port) ->
    try erlang:port_close(Port) catch error:_ -> ok end,
    receive %% Ugly workaround in case the caller==owner traps exits
	{'EXIT', Port, _Reason} -> 
	    ok
    after 0 -> 
	    ok
    end.



%% Issues a command to a port and gets the response.
%% If Port is {Driver, Portopts} a port is first opened and 
%% then closed after the result has been received.
%% Returns {ok, Result} or {error, Reason}.

drv_command_raw(Port, Command) ->
    drv_command(Port, Command, false, undefined).

drv_command(Port, Command) ->
    drv_command(Port, Command, undefined).

drv_command(Port, Command, R) when is_binary(Command) ->
    drv_command(Port, Command, true, R);
drv_command(Port, Command, R) ->
    try erlang:iolist_to_binary(Command) of
	Bin ->
	    drv_command(Port, Bin, true, R)
    catch
	error:Reason ->
	    {error, Reason}
    end.

drv_command(Port, Command, Validated, R) when is_port(Port) ->
    try erlang:port_command(Port, Command) of
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


    
%% Receives the response from a driver port.
%% Returns: {ok, ListOrBinary}|{error, Reason}

drv_get_response(Port, R) when is_list(R) ->
    case drv_get_response(Port) of
	ok ->
	    {ok, R};
	{ok, Name} ->
	    drv_get_response(Port, [Name|R]);
	Error ->
	    Error
    end;
drv_get_response(Port, _) ->
    drv_get_response(Port).

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



%% Converts a list of mode atoms into an mode word for the driver.
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
translate_response(?FILE_RESP_OK, Data) ->
    {ok, Data};
translate_response(?FILE_RESP_ERROR, List) when is_list(List) ->
    {error, list_to_atom(List)};
translate_response(?FILE_RESP_NUMBER, List) ->
    {N, []} = get_uint64(List),
    {ok, N};
translate_response(?FILE_RESP_DATA, List) ->
    {N, Data} = get_uint64(List),
    {ok, {N, Data}};
translate_response(?FILE_RESP_INFO, List) when is_list(List) ->
    {ok, transform_info_ints(get_uint32s(List))};
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
    {ReadSize, L2} = get_uint64(L1),
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
translate_response(X, Data) ->
    {error, {bad_response_from_port, [X | Data]}}.

transform_info_ints(Ints) ->
    [HighSize, LowSize, Type|Tail0] = Ints,
    Size = HighSize * 16#100000000 + LowSize,
    [Ay, Am, Ad, Ah, Ami, As|Tail1]  = Tail0,
    [My, Mm, Md, Mh, Mmi, Ms|Tail2] = Tail1,
    [Cy, Cm, Cd, Ch, Cmi, Cs|Tail3] = Tail2,
    [Mode, Links, Major, Minor, Inode, Uid, Gid, Access] = Tail3,
    #file_info {
		size = Size,
		type = file_type(Type),
		access = file_access(Access),
		atime = {{Ay, Am, Ad}, {Ah, Ami, As}},
		mtime = {{My, Mm, Md}, {Mh, Mmi, Ms}},
		ctime = {{Cy, Cm, Cd}, {Ch, Cmi, Cs}},
		mode = Mode,
		links = Links,
		major_device = Major,
		minor_device = Minor,
		inode = Inode,
		uid = Uid,
		gid = Gid}.
    
file_type(1) -> device;
file_type(2) -> directory;
file_type(3) -> regular;
file_type(4) -> symlink;
file_type(_) -> other.

file_access(0) -> none;   
file_access(1) -> write;
file_access(2) -> read;
file_access(3) -> read_write.

int_to_bytes(Int) when is_integer(Int) ->
    <<Int:32>>;
int_to_bytes(undefined) ->
    <<-1:32>>.

date_to_bytes(undefined) ->
    <<-1:32, -1:32, -1:32, -1:32, -1:32, -1:32>>;
date_to_bytes({{Y, Mon, D}, {H, Min, S}}) ->
    <<Y:32, Mon:32, D:32, H:32, Min:32, S:32>>.

% uint64([[X1, X2, X3, X4] = Y1 | [X5, X6, X7, X8] = Y2]) ->
%     (uint32(Y1) bsl 32) bor uint32(Y2).

% uint64(X1, X2, X3, X4, X5, X6, X7, X8) ->
%     (uint32(X1, X2, X3, X4) bsl 32) bor uint32(X5, X6, X7, X8).

% uint32([X1,X2,X3,X4]) ->
%     (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

uint32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

get_uint64(L0) ->
    {X1, L1} = get_uint32(L0),
    {X2, L2} = get_uint32(L1),
    {(X1 bsl 32) bor X2, L2}.

get_uint32([X1,X2,X3,X4|List]) ->
    {(((((X1 bsl 8) bor X2) bsl 8) bor X3) bsl 8) bor X4, List}.

get_uint32s([X1,X2,X3,X4|Tail]) ->
    [uint32(X1,X2,X3,X4) | get_uint32s(Tail)];
get_uint32s([]) -> [].



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
