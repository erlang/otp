%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(file).

%% Interface module for the file server and the file io servers.



%%% External exports

-export([format_error/1]).
%% File system and metadata.
-export([get_cwd/0, get_cwd/1, set_cwd/1, delete/1, rename/2,
	 make_dir/1, del_dir/1, list_dir/1, list_dir_all/1,
	 read_file_info/1, read_file_info/2,
	 write_file_info/2, write_file_info/3,
	 altname/1,
	 read_link_info/1, read_link_info/2,
	 read_link/1, read_link_all/1,
	 make_link/2, make_symlink/2,
	 read_file/1, write_file/2, write_file/3]).
%% Specialized
-export([ipread_s32bu_p32bu/3]).
%% Generic file contents.
-export([open/2, close/1, advise/4, allocate/3,
	 read/2, write/2, 
	 pread/2, pread/3, pwrite/2, pwrite/3,
	 read_line/1,
	 position/2, truncate/1, datasync/1, sync/1,
	 copy/2, copy/3]).
%% High level operations
-export([consult/1, path_consult/2]).
-export([eval/1, eval/2, path_eval/2, path_eval/3, path_open/3]).
-export([script/1, script/2, path_script/2, path_script/3]).
-export([change_owner/2, change_owner/3, change_group/2,
	 change_mode/2, change_time/2, change_time/3]).

-export([pid2name/1]).

%% Sendfile functions
-export([sendfile/2,sendfile/5]).

%%% Obsolete exported functions

-export([raw_read_file_info/1, raw_write_file_info/2]).

%% Internal export to prim_file and ram_file until they implement
%% an efficient copy themselves.
-export([copy_opened/3]).

-export([ipread_s32bu_p32bu_int/3]).

%% Types that can be used from other modules -- alphabetically ordered.
-export_type([date_time/0, fd/0, file_info/0, filename/0, filename_all/0,
              io_device/0, mode/0, name/0, name_all/0, posix/0]).

%%% Includes and defines
-include("file_int.hrl").

-define(FILE_IO_SERVER_TABLE, file_io_servers).

-define(FILE_SERVER, file_server_2).   % Registered name
-define(PRIM_FILE, prim_file).         % Module
-define(RAM_FILE, ram_file).           % Module

%% data types
-type filename()  :: string().
-type filename_all() :: string() | binary().
-type file_info() :: #file_info{}.
-type fd()        :: #file_descriptor{}.
-type io_device() :: pid() | fd().
-type location()  :: integer() | {'bof', Offset :: integer()}
                   | {'cur', Offset :: integer()}
		   | {'eof', Offset :: integer()} | 'bof' | 'cur' | 'eof'.
-type mode()      :: 'read' | 'write' | 'append'
                   | 'exclusive' | 'raw' | 'binary'
		   | {'delayed_write',
                      Size :: non_neg_integer(),
                      Delay :: non_neg_integer()}
		   | 'delayed_write' | {'read_ahead', Size :: pos_integer()}
		   | 'read_ahead' | 'compressed'
		   | {'encoding', unicode:encoding()}
		   | sync.
-type deep_list() :: [char() | atom() | deep_list()].
-type name()      :: string() | atom() | deep_list().
-type name_all()  :: string() | atom() | deep_list() | (RawFilename :: binary()).
-type posix() ::
        'eacces' | 'eagain' |
        'ebadf' | 'ebadmsg' | 'ebusy' |
        'edeadlk' | 'edeadlock' | 'edquot' |
        'eexist' |
        'efault' | 'efbig' | 'eftype' |
        'eintr' | 'einval' | 'eio' | 'eisdir' |
        'eloop' |
        'emfile' | 'emlink' | 'emultihop' |
        'enametoolong' | 'enfile' |
        'enobufs' | 'enodev' | 'enolck' | 'enolink' | 'enoent' |
        'enomem' | 'enospc' | 'enosr' | 'enostr' | 'enosys' |
        'enotblk' | 'enotdir' | 'enotsup' | 'enxio' |
        'eopnotsupp' | 'eoverflow' |
        'eperm' | 'epipe' |
        'erange' | 'erofs' |
        'espipe'  | 'esrch'  | 'estale' |
        'etxtbsy' |
        'exdev'.
-type date_time() :: calendar:datetime().
-type posix_file_advise() :: 'normal' | 'sequential' | 'random'
                           | 'no_reuse' | 'will_need' | 'dont_need'.
-type sendfile_option() :: {chunk_size, non_neg_integer()}
			 | {use_threads, boolean()}.
-type file_info_option() :: {'time', 'local'} | {'time', 'universal'} 
			  | {'time', 'posix'} | raw.
%%% BIFs

-export([native_name_encoding/0]).

-spec native_name_encoding() -> latin1 | utf8.

native_name_encoding() ->
    erlang:nif_error(undef).

%%% End of BIFs


%%%-----------------------------------------------------------------
%%% General functions

-spec format_error(Reason) -> Chars when
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()},
      Chars :: string().

format_error({_Line, ?MODULE, undefined_script}) ->
    "no value returned from script";
format_error({Line, ?MODULE, {Class, Reason, Stacktrace}}) ->
    io_lib:format("~w: evaluation failed with reason ~w:~w and stacktrace ~w", 
                  [Line, Class, Reason, Stacktrace]);
format_error({Line, ?MODULE, {Reason, Stacktrace}}) ->
    io_lib:format("~w: evaluation failed with reason ~w and stacktrace ~w", 
                  [Line, Reason, Stacktrace]);
format_error({Line, Mod, Reason}) ->
    io_lib:format("~w: ~ts", [Line, Mod:format_error(Reason)]);
format_error(badarg) ->
    "bad argument";
format_error(system_limit) ->
    "a system limit was hit, probably not enough ports";
format_error(terminated) ->
    "the file server process is terminated";
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

-spec pid2name(Pid) -> {ok, Filename} | undefined when
      Filename :: filename_all(),
      Pid :: pid().

pid2name(Pid) when is_pid(Pid) ->
    case whereis(?FILE_SERVER) of
	undefined ->
	    undefined;
	_ ->
	    case ets:lookup(?FILE_IO_SERVER_TABLE, Pid) of
		[{_, Name} | _] ->
		    {ok, Name};
		_ ->
		    undefined
	    end
    end.

%%%-----------------------------------------------------------------
%%% File server functions.
%%% Functions that do not operate on a single open file.
%%% Stateless.
-spec get_cwd() -> {ok, Dir} | {error, Reason} when
      Dir :: filename(),
      Reason :: posix().

get_cwd() ->
    call(get_cwd, []).

-spec get_cwd(Drive) -> {ok, Dir} | {error, Reason} when
      Drive :: string(),
      Dir :: filename(),
      Reason :: posix() | badarg.

get_cwd(Drive) ->
    check_and_call(get_cwd, [file_name(Drive)]).

-spec set_cwd(Dir) -> ok | {error, Reason} when
      Dir :: name() | EncodedBinary,
      EncodedBinary :: binary(),
      Reason :: posix() | badarg | no_translation.

set_cwd(Dirname) -> 
    check_and_call(set_cwd, [file_name(Dirname)]).

-spec delete(Filename) -> ok | {error, Reason} when
      Filename :: name_all(),
      Reason :: posix() | badarg.

delete(Name) ->
    check_and_call(delete, [file_name(Name)]).

-spec rename(Source, Destination) -> ok | {error, Reason} when
      Source :: name_all(),
      Destination :: name_all(),
      Reason :: posix() | badarg.

rename(From, To) ->
    check_and_call(rename, [file_name(From), file_name(To)]).

-spec make_dir(Dir) -> ok | {error, Reason} when
      Dir :: name_all(),
      Reason :: posix() | badarg.

make_dir(Name) ->
    check_and_call(make_dir, [file_name(Name)]).

-spec del_dir(Dir) -> ok | {error, Reason} when
      Dir :: name_all(),
      Reason :: posix() | badarg.

del_dir(Name) ->
    check_and_call(del_dir, [file_name(Name)]).

-spec read_file_info(Filename) -> {ok, FileInfo} | {error, Reason} when
      Filename :: name_all(),
      FileInfo :: file_info(),
      Reason :: posix() | badarg.

read_file_info(Name) ->
    check_and_call(read_file_info, [file_name(Name)]).

-spec read_file_info(Filename, Opts) -> {ok, FileInfo} | {error, Reason} when
      Filename :: name_all(),
      Opts :: [file_info_option()],
      FileInfo :: file_info(),
      Reason :: posix() | badarg.

read_file_info(Name, Opts) when is_list(Opts) ->
    Args = [file_name(Name), Opts],
    case check_args(Args) of
        ok ->
            case lists:member(raw, Opts) of
                true ->
                    [FileName|_] = Args,
                    ?PRIM_FILE:read_file_info(FileName, Opts);
                false ->
                    call(read_file_info, Args)
            end;
        Error ->
            Error
    end.

-spec altname(Name :: name_all()) -> any().

altname(Name) ->
    check_and_call(altname, [file_name(Name)]).

-spec read_link_info(Name) -> {ok, FileInfo} | {error, Reason} when
      Name :: name_all(),
      FileInfo :: file_info(),
      Reason :: posix() | badarg.

read_link_info(Name) ->
    check_and_call(read_link_info, [file_name(Name)]).

-spec read_link_info(Name, Opts) -> {ok, FileInfo} | {error, Reason} when
      Name :: name_all(),
      Opts :: [file_info_option()],
      FileInfo :: file_info(),
      Reason :: posix() | badarg.

read_link_info(Name, Opts) when is_list(Opts) ->
    Args = [file_name(Name), Opts],
    case check_args(Args) of
        ok ->
            case lists:member(raw, Opts) of
                true ->
                    [FileName|_] = Args,
                    ?PRIM_FILE:read_link_info(FileName, Opts);
                false ->
                    call(read_link_info, Args)
            end;
        Error ->
            Error
    end.


-spec read_link(Name) -> {ok, Filename} | {error, Reason} when
      Name :: name_all(),
      Filename :: filename(),
      Reason :: posix() | badarg.

read_link(Name) ->
    check_and_call(read_link, [file_name(Name)]).

-spec read_link_all(Name) -> {ok, Filename} | {error, Reason} when
      Name :: name_all(),
      Filename :: filename_all(),
      Reason :: posix() | badarg.

read_link_all(Name) ->
    check_and_call(read_link_all, [file_name(Name)]).

-spec write_file_info(Filename, FileInfo) -> ok | {error, Reason} when
      Filename :: name_all(),
      FileInfo :: file_info(),
      Reason :: posix() | badarg.

write_file_info(Name, Info = #file_info{}) ->
    check_and_call(write_file_info, [file_name(Name), Info]).

-spec write_file_info(Filename, FileInfo, Opts) -> ok | {error, Reason} when
      Filename :: name_all(),
      Opts :: [file_info_option()],
      FileInfo :: file_info(),
      Reason :: posix() | badarg.

write_file_info(Name, Info = #file_info{}, Opts) when is_list(Opts) ->
    Args = [file_name(Name), Info, Opts],
    case check_args(Args) of
        ok ->
            case lists:member(raw, Opts) of
                true ->
                    [FileName|_] = Args,
                    ?PRIM_FILE:write_file_info(FileName, Info, Opts);
                false ->
                    call(write_file_info, Args)
            end;
        Error ->
            Error
    end.

-spec list_dir(Dir) -> {ok, Filenames} | {error, Reason} when
      Dir :: name_all(),
      Filenames :: [filename()],
      Reason :: posix()
              | badarg
              | {no_translation, Filename :: unicode:latin1_binary()}.

list_dir(Name) ->
    check_and_call(list_dir, [file_name(Name)]).

-spec list_dir_all(Dir) -> {ok, Filenames} | {error, Reason} when
      Dir :: name_all(),
      Filenames :: [filename_all()],
      Reason :: posix() | badarg.

list_dir_all(Name) ->
    check_and_call(list_dir_all, [file_name(Name)]).

-spec read_file(Filename) -> {ok, Binary} | {error, Reason} when
      Filename :: name_all(),
      Binary :: binary(),
      Reason :: posix() | badarg | terminated | system_limit.

read_file(Name) ->
    check_and_call(read_file, [file_name(Name)]).

-spec make_link(Existing, New) -> ok | {error, Reason} when
      Existing :: name_all(),
      New :: name_all(),
      Reason :: posix() | badarg.

make_link(Old, New) ->
    check_and_call(make_link, [file_name(Old), file_name(New)]).

-spec make_symlink(Existing, New) -> ok | {error, Reason} when
      Existing :: name_all(),
      New :: name_all(),
      Reason :: posix() | badarg.

make_symlink(Old, New) ->
    check_and_call(make_symlink, [file_name(Old), file_name(New)]).

-spec write_file(Filename, Bytes) -> ok | {error, Reason} when
      Filename :: name_all(),
      Bytes :: iodata(),
      Reason :: posix() | badarg | terminated | system_limit.

write_file(Name, Bin) ->
    check_and_call(write_file, [file_name(Name), make_binary(Bin)]).

%% This whole operation should be moved to the file_server and prim_file
%% when it is time to change file server protocol again.
%% Meanwhile, it is implemented here, slightly less efficient.

-spec write_file(Filename, Bytes, Modes) -> ok | {error, Reason} when
      Filename :: name_all(),
      Bytes :: iodata(),
      Modes :: [mode()],
      Reason :: posix() | badarg | terminated | system_limit.

write_file(Name, IOData, ModeList) when is_list(ModeList) ->
    case lists:member(raw, ModeList) of
        true ->
          %% For backwards compatibility of error messages
            try iolist_size(IOData) of
                _Size -> do_write_file(Name, IOData, ModeList)
            catch
                error:Error -> {error, Error}
            end;
        false ->
            case make_binary(IOData) of
                Bin when is_binary(Bin) ->
                    do_write_file(Name, Bin, ModeList);
                Error ->
                    Error
            end
    end.

do_write_file(Name, IOData, ModeList) ->
    case open(Name, [binary, write | ModeList]) of
        {ok, Handle} ->
            case write(Handle, IOData) of
                ok ->
                    close(Handle);
                E1 ->
                    _ = close(Handle),
                    E1
            end;
        E2 ->
            E2
    end.

%% Obsolete, undocumented, local node only, don't use!.
%% XXX to be removed.
raw_read_file_info(Name) ->
    read_file_info(Name, [raw]).

%% Obsolete, undocumented, local node only, don't use!.
%% XXX to be removed.
raw_write_file_info(Name, #file_info{} = Info) ->
    write_file_info(Name, Info, [raw]).

%%%-----------------------------------------------------------------
%%% File io server functions.
%%% They operate on a single open file.
%%% Stateful.

%% Contemporary mode specification - list of options

-spec open(File, Modes) -> {ok, IoDevice} | {error, Reason} when
      File :: Filename | iodata(),
      Filename :: name_all(),
      Modes :: [mode() | ram],
      IoDevice :: io_device(),
      Reason :: posix() | badarg | system_limit.

open(Item, ModeList) when is_list(ModeList) ->
    case {lists:member(raw, ModeList), lists:member(ram, ModeList)} of
        {false, false} ->
            %% File server file
            Args = [file_name(Item) | ModeList],
            case check_args(Args) of
                ok ->
                    [FileName | _] = Args,
                    call(open, [FileName, ModeList]);
                Error ->
                    Error
            end;
        {true, _Either} ->
            raw_file_io:open(file_name(Item), ModeList);
        {false, true} ->
            ram_file:open(Item, ModeList)
    end;

%% Old obsolete mode specification in atom or 2-tuple format
open(Item, Mode) ->
    open(Item, mode_list(Mode)).

%%%-----------------------------------------------------------------
%%% The following interface functions operate on open files.
%%% The File argument must be either a Pid or a handle 
%%% returned from ?PRIM_FILE:open.

-spec close(IoDevice) -> ok | {error, Reason} when
      IoDevice :: io_device(),
      Reason :: posix() | badarg | terminated.

close(File) when is_pid(File) ->
    case file_request(File, close) of
	{error, terminated} ->
	    ok;
	Other ->
	    Other
    end;
%%    unlink(File),
%%    exit(File, close),
%%    ok;
close(#file_descriptor{module = Module} = Handle) ->
    Module:close(Handle);
close(_) ->
    {error, badarg}.

-spec advise(IoDevice, Offset, Length, Advise) -> ok | {error, Reason} when
      IoDevice :: io_device(),
      Offset :: integer(),
      Length :: integer(),
      Advise :: posix_file_advise(),
      Reason :: posix() | badarg.

advise(File, Offset, Length, Advise) when is_pid(File) ->
    file_request(File, {advise, Offset, Length, Advise});
advise(#file_descriptor{module = Module} = Handle, Offset, Length, Advise) ->
    Module:advise(Handle, Offset, Length, Advise);
advise(_, _, _, _) ->
    {error, badarg}.

-spec allocate(File, Offset, Length) ->
	'ok' | {'error', posix()} when
      File :: io_device(),
      Offset :: non_neg_integer(),
      Length :: non_neg_integer().

allocate(File, Offset, Length) when is_pid(File) ->
    file_request(File, {allocate, Offset, Length});
allocate(#file_descriptor{module = Module} = Handle, Offset, Length) ->
    Module:allocate(Handle, Offset, Length).

-spec read(IoDevice, Number) -> {ok, Data} | eof | {error, Reason} when
      IoDevice :: io_device() | atom(),
      Number :: non_neg_integer(),
      Data :: string() | binary(),
      Reason :: posix()
              | badarg
              | terminated
              | {no_translation, unicode, latin1}.

read(File, Sz) when (is_pid(File) orelse is_atom(File)), is_integer(Sz), Sz >= 0 ->
    case io:request(File, {get_chars, '', Sz}) of
	Data when is_list(Data); is_binary(Data) ->
	    {ok, Data};
	Other ->
	    Other
    end;
read(#file_descriptor{module = Module} = Handle, Sz) 
  when is_integer(Sz), Sz >= 0 ->
    Module:read(Handle, Sz);
read(_, _) ->
    {error, badarg}.

-spec read_line(IoDevice) -> {ok, Data} | eof | {error, Reason} when
      IoDevice :: io_device() | atom(),
      Data :: string() | binary(),
      Reason :: posix()
              | badarg
              | terminated
              | {no_translation, unicode, latin1}.

read_line(File) when (is_pid(File) orelse is_atom(File)) ->
    case io:request(File, {get_line, ''}) of
	Data when is_list(Data); is_binary(Data) ->
	    {ok, Data};
	Other ->
	    Other
    end;
read_line(#file_descriptor{module = Module} = Handle) ->
    Module:read_line(Handle);
read_line(_) ->
    {error, badarg}.

-spec pread(IoDevice, LocNums) -> {ok, DataL} | eof | {error, Reason} when
      IoDevice :: io_device(),
      LocNums :: [{Location :: location(), Number :: non_neg_integer()}],
      DataL :: [Data],
      Data :: string() | binary() | eof,
      Reason :: posix() | badarg | terminated.

pread(File, L) when is_pid(File), is_list(L) ->
    pread_int(File, L, []);
pread(#file_descriptor{module = Module} = Handle, L) when is_list(L) ->
    Module:pread(Handle, L);
pread(_, _) ->
    {error, badarg}.

pread_int(_File, [], R) ->
    {ok, lists:reverse(R)};
pread_int(File, [{At, Sz} | T], R) when is_integer(Sz), Sz >= 0 ->
    case pread(File, At, Sz) of
	{ok, Data} ->
	    pread_int(File, T, [Data | R]);
	eof ->
	    pread_int(File, T, [eof | R]);
	{error, _} = Error ->
	    Error
    end;
pread_int(_, _, _) ->
    {error, badarg}.

-spec pread(IoDevice, Location, Number) ->
             {ok, Data} | eof | {error, Reason} when
      IoDevice :: io_device(),
      Location :: location(),
      Number :: non_neg_integer(),
      Data :: string() | binary(),
      Reason :: posix() | badarg | terminated.

pread(File, At, Sz) when is_pid(File), is_integer(Sz), Sz >= 0 ->
    file_request(File, {pread, At, Sz});
pread(#file_descriptor{module = Module} = Handle, Offs, Sz) 
  when is_integer(Sz), Sz >= 0 ->
    Module:pread(Handle, Offs, Sz);
pread(_, _, _) ->
    {error, badarg}.

-spec write(IoDevice, Bytes) -> ok | {error, Reason} when
      IoDevice :: io_device() | atom(),
      Bytes :: iodata(),
      Reason :: posix() | badarg | terminated.

write(File, Bytes) when (is_pid(File) orelse is_atom(File)) ->
    case make_binary(Bytes) of
	Bin when is_binary(Bin) ->
	    io:request(File, {put_chars,latin1,Bin});
	Error ->
	    Error
    end;
write(#file_descriptor{module = Module} = Handle, Bytes) ->
    Module:write(Handle, Bytes);
write(_, _) ->
    {error, badarg}.

-spec pwrite(IoDevice, LocBytes) -> ok | {error, {N, Reason}} when
      IoDevice :: io_device(),
      LocBytes :: [{Location :: location(), Bytes :: iodata()}],
      N :: non_neg_integer(),
      Reason :: posix() | badarg | terminated.

pwrite(File, L) when is_pid(File), is_list(L) ->
    pwrite_int(File, L, 0);
pwrite(#file_descriptor{module = Module} = Handle, L) when is_list(L) ->
    Module:pwrite(Handle, L);
pwrite(_, _) ->
    {error, badarg}.

pwrite_int(_File, [], _R) ->
    ok;
pwrite_int(File, [{At, Bytes} | T], R) ->
    case pwrite(File, At, Bytes) of
	ok ->
	    pwrite_int(File, T, R+1);
	{error, Reason} ->
	    {error, {R, Reason}}
    end;
pwrite_int(_, _, _) ->
    {error, badarg}.

-spec pwrite(IoDevice, Location, Bytes) -> ok | {error, Reason} when
      IoDevice :: io_device(),
      Location :: location(),
      Bytes :: iodata(),
      Reason :: posix() | badarg | terminated.

pwrite(File, At, Bytes) when is_pid(File) ->
    file_request(File, {pwrite, At, Bytes});
pwrite(#file_descriptor{module = Module} = Handle, Offs, Bytes) ->
    Module:pwrite(Handle, Offs, Bytes);
pwrite(_, _, _) ->
    {error, badarg}.

-spec datasync(IoDevice) -> ok | {error, Reason} when
      IoDevice :: io_device(),
      Reason :: posix() | badarg | terminated.

datasync(File) when is_pid(File) ->
    file_request(File, datasync);
datasync(#file_descriptor{module = Module} = Handle) ->
    Module:datasync(Handle);
datasync(_) ->
    {error, badarg}.

-spec sync(IoDevice) -> ok | {error, Reason} when
      IoDevice :: io_device(),
      Reason :: posix() | badarg | terminated.

sync(File) when is_pid(File) ->
    file_request(File, sync);
sync(#file_descriptor{module = Module} = Handle) ->
    Module:sync(Handle);
sync(_) ->
    {error, badarg}.

-spec position(IoDevice, Location) -> {ok, NewPosition} | {error, Reason} when
      IoDevice :: io_device(),
      Location :: location(),
      NewPosition :: integer(),
      Reason :: posix() | badarg | terminated.

position(File, At) when is_pid(File) ->
    file_request(File, {position,At});
position(#file_descriptor{module = Module} = Handle, At) ->
    Module:position(Handle, At);
position(_, _) ->
    {error, badarg}.

-spec truncate(IoDevice) -> ok | {error, Reason} when
      IoDevice :: io_device(),
      Reason :: posix() | badarg | terminated.

truncate(File) when is_pid(File) ->
    file_request(File, truncate);
truncate(#file_descriptor{module = Module} = Handle) ->
    Module:truncate(Handle);
truncate(_) ->
    {error, badarg}.

-spec copy(Source, Destination) -> {ok, BytesCopied} | {error, Reason} when
      Source :: io_device() | Filename | {Filename, Modes},
      Destination :: io_device() | Filename | {Filename, Modes},
      Filename :: name_all(),
      Modes :: [mode()],
      BytesCopied :: non_neg_integer(),
      Reason :: posix() | badarg | terminated.

copy(Source, Dest) ->
    copy_int(Source, Dest, infinity).

-spec copy(Source, Destination, ByteCount) ->
             {ok, BytesCopied} | {error, Reason} when
      Source :: io_device() | Filename | {Filename, Modes},
      Destination :: io_device() | Filename | {Filename, Modes},
      Filename :: name_all(),
      Modes :: [mode()],
      ByteCount :: non_neg_integer() | infinity,
      BytesCopied :: non_neg_integer(),
      Reason :: posix() | badarg | terminated.

copy(Source, Dest, Length) 
  when is_integer(Length), Length >= 0;
       is_atom(Length) ->
    copy_int(Source, Dest, Length);
copy(_, _, _) ->
    {error, badarg}.

%% Here we know that Length is either an atom or an integer >= 0
%% (by the way, atoms > integers)
%%
%% Copy between open files. 
copy_int(Source, Dest, Length) 
  when is_pid(Source), is_pid(Dest);
       is_pid(Source), is_record(Dest, file_descriptor);
       is_record(Source, file_descriptor), is_pid(Dest) ->
    copy_opened_int(Source, Dest, Length, 0);
%% Copy between open raw files, both handled by the same module
copy_int(#file_descriptor{module = Module} = Source,
	 #file_descriptor{module = Module} = Dest,
	 Length) ->
    Module:copy(Source, Dest, Length);
%% Copy between open raw files of different modules
copy_int(#file_descriptor{} = Source, 
	 #file_descriptor{} = Dest, Length) ->
    copy_opened_int(Source, Dest, Length, 0);
%% Copy between filenames, let the server do the copy
copy_int({SourceName, SourceOpts}, {DestName, DestOpts}, Length) 
  when is_list(SourceOpts), is_list(DestOpts) ->
    check_and_call(copy, 
		   [file_name(SourceName), SourceOpts,
		    file_name(DestName), DestOpts,
		    Length]);
%% Filename -> open file; must open Source and do client copy
copy_int({SourceName, SourceOpts}, Dest, Length) 
  when is_list(SourceOpts), is_pid(Dest);
       is_list(SourceOpts), is_record(Dest, file_descriptor) ->
    case file_name(SourceName) of
	{error, _} = Error ->
	    Error;
	Source ->
	    case open(Source, [read | SourceOpts]) of
		{ok, Handle} ->
		    Result = copy_opened_int(Handle, Dest, Length, 0),
		    _ = close(Handle),
		    Result;
		{error, _} = Error ->
		    Error
	    end
    end;
%% Open file -> filename; must open Dest and do client copy
copy_int(Source, {DestName, DestOpts}, Length)
  when is_pid(Source), is_list(DestOpts);
       is_record(Source, file_descriptor), is_list(DestOpts) ->
    case file_name(DestName) of
	{error, _} = Error ->
	    Error;
	Dest ->
	    case open(Dest, [write | DestOpts]) of
		{ok, Handle} ->
                    case copy_opened_int(Source, Handle, Length, 0) of
                        {ok, _} = OK ->
                            case close(Handle) of
                                ok -> OK;
                                Error -> Error
                            end;
                        Error ->
                            _ = close(Handle),
                            Error
                    end;
		{error, _} = Error ->
		    Error
	    end
    end;
%%
%% That was all combinations of {Name, Opts} tuples
%% and open files. At least one of Source and Dest has
%% to be a bare filename.
%%
%% If Source is not a bare filename; Dest must be
copy_int(Source, Dest, Length) 
  when is_pid(Source);
       is_record(Source, file_descriptor) ->
    copy_int(Source, {Dest, []}, Length);
copy_int({_SourceName, SourceOpts} = Source, Dest, Length) 
  when is_list(SourceOpts) ->
    copy_int(Source, {Dest, []}, Length);
%% If Dest is not a bare filename; Source must be
copy_int(Source, Dest, Length) 
  when is_pid(Dest);
       is_record(Dest, file_descriptor) ->
    copy_int({Source, []}, Dest, Length);
copy_int(Source, {_DestName, DestOpts} = Dest, Length) 
  when is_list(DestOpts) ->
    copy_int({Source, []}, Dest, Length);
%% Both must be bare filenames. If they are not,
%% the filename check in the copy operation will yell.
copy_int(Source, Dest, Length) ->
    copy_int({Source, []}, {Dest, []}, Length).



copy_opened(Source, Dest, Length)
  when is_integer(Length), Length >= 0;
       is_atom(Length) ->
    copy_opened_int(Source, Dest, Length);
copy_opened(_, _, _) ->
    {error, badarg}.

%% Here we know that Length is either an atom or an integer >= 0
%% (by the way, atoms > integers)

copy_opened_int(Source, Dest, Length)
  when is_pid(Source), is_pid(Dest) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(Source, Dest, Length)
  when is_pid(Source), is_record(Dest, file_descriptor) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(Source, Dest, Length)
  when is_record(Source, file_descriptor), is_pid(Dest) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(Source, Dest, Length)
  when is_record(Source, file_descriptor), is_record(Dest, file_descriptor) ->
    copy_opened_int(Source, Dest, Length, 0);
copy_opened_int(_, _, _) ->
    {error, badarg}.

%% Here we know that Source and Dest are handles to open files, Length is
%% as above, and Copied is an integer >= 0

%% Copy loop in client process
copy_opened_int(_, _, Length, Copied) when Length =< 0 -> % atom() > integer()
    {ok, Copied};
copy_opened_int(Source, Dest, Length, Copied) ->
    N = if Length > 65536 -> 65536; true -> Length end, % atom() > integer() !
    case read(Source, N) of
	{ok, Data} ->
	    M = if is_binary(Data) -> byte_size(Data);
		   is_list(Data)   -> length(Data)
		end,
	    case write(Dest, Data) of
		ok ->
		    if M < N ->
			    %% Got less than asked for - must be end of file
			    {ok, Copied+M};
		       true ->
			    %% Decrement Length (might be an atom (infinity))
			    NewLength = if is_atom(Length) -> Length;
					   true         -> Length-M
					end,
			    copy_opened_int(Source, Dest, NewLength, Copied+M)
		    end;
		{error, _} = Error ->
		    Error
	    end;
	eof ->
	    {ok, Copied};
	{error, _} = Error ->
	    Error
    end.


%% Special indirect pread function. Introduced for Dets.
%% Reads a header from pos 'Pos', the header is first a size encoded as
%% 32 bit big endian unsigned and then a position also encoded as
%% 32 bit big endian. Finally it preads the data from that pos and size 
%% in the file.

ipread_s32bu_p32bu(File, Pos, MaxSize) when is_pid(File) ->
    ipread_s32bu_p32bu_int(File, Pos, MaxSize);
ipread_s32bu_p32bu(#file_descriptor{module = Module} = Handle, Pos, MaxSize) ->
    Module:ipread_s32bu_p32bu(Handle, Pos, MaxSize);
ipread_s32bu_p32bu(_, _, _) ->
    {error, badarg}.

ipread_s32bu_p32bu_int(File, Pos, Infinity) when is_atom(Infinity) ->
    ipread_s32bu_p32bu_int(File, Pos, (1 bsl 31)-1);
ipread_s32bu_p32bu_int(File, Pos, MaxSize) 
  when is_integer(MaxSize), MaxSize >= 0 ->
    if
	MaxSize < (1 bsl 31) ->
	    case pread(File, Pos, 8) of
		{ok, Header} ->
		    ipread_s32bu_p32bu_2(File, Header, MaxSize);
		Error ->
		    Error
	    end;
	true ->
	    {error, einval}
    end;
ipread_s32bu_p32bu_int(_File, _Pos, _MaxSize) ->
    {error, badarg}.

ipread_s32bu_p32bu_2(_File, 
		     <<0:32/big-unsigned, Pos:32/big-unsigned>>,
		     _MaxSize) ->
    {ok, {0, Pos, eof}};
ipread_s32bu_p32bu_2(File, 
		     <<Size:32/big-unsigned, Pos:32/big-unsigned>>,
		     MaxSize) 
  when Size =< MaxSize ->
    case pread(File, Pos, Size) of
	{ok, Data} ->
	    {ok, {Size, Pos, Data}};
	eof ->
	    {ok, {Size, Pos, eof}};
	Error ->
	    Error
    end;
ipread_s32bu_p32bu_2(_File, 
		     <<_:8/binary>>,
		     _MaxSize) ->
    eof;
ipread_s32bu_p32bu_2(_File,
		     <<_/binary>>,
		     _MaxSize) ->
    eof;
ipread_s32bu_p32bu_2(File,
		    Header,
		    MaxSize) when is_list(Header) ->
    ipread_s32bu_p32bu_2(File, list_to_binary(Header), MaxSize).



%%%-----------------------------------------------------------------
%%% The following functions, built upon the other interface functions,
%%% provide a higher-lever interface to files.

-spec consult(Filename) -> {ok, Terms} | {error, Reason} when
      Filename :: name_all(),
      Terms :: [term()],
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

consult(File) ->
    case open(File, [read]) of
	{ok, Fd} ->
	    R = consult_stream(Fd),
	    _ = close(Fd),
	    R;
	Error ->
	    Error
    end.

-spec path_consult(Path, Filename) -> {ok, Terms, FullName} | {error, Reason} when
      Path :: [Dir],
      Dir :: name_all(),
      Filename :: name_all(),
      Terms :: [term()],
      FullName :: filename_all(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

path_consult(Path, File) ->
    case path_open(Path, File, [read]) of
	{ok, Fd, Full} ->
	    case consult_stream(Fd) of
		{ok, List} ->
		    _ = close(Fd),
		    {ok, List, Full};
		E1 ->
		    _ = close(Fd),
		    E1
	    end;
	E2 ->
	    E2
    end.

-spec eval(Filename) -> ok | {error, Reason} when
      Filename :: name_all(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

eval(File) ->
    eval(File, erl_eval:new_bindings()).

-spec eval(Filename, Bindings) -> ok | {error, Reason} when
      Filename :: name_all(),
      Bindings :: erl_eval:binding_struct(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

eval(File, Bs) ->
    case open(File, [read]) of
	{ok, Fd} ->
	    R = eval_stream(Fd, ignore, Bs),
	    _ = close(Fd),
	    R;
	Error ->
	    Error
    end.

-spec path_eval(Path, Filename) -> {ok, FullName} | {error, Reason} when
      Path :: [Dir :: name_all()],
      Filename :: name_all(),
      FullName :: filename_all(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

path_eval(Path, File) ->
    path_eval(Path, File, erl_eval:new_bindings()).

-spec path_eval(Path, Filename, Bindings) ->
             {ok, FullName} | {error, Reason} when
      Path :: [Dir :: name_all()],
      Filename :: name_all(),
      Bindings :: erl_eval:binding_struct(),
      FullName :: filename_all(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

path_eval(Path, File, Bs) ->
    case path_open(Path, File, [read]) of
	{ok, Fd, Full} ->
	    case eval_stream(Fd, ignore, Bs) of
		ok ->
		    _ = close(Fd),
		    {ok, Full};
		E1 ->
		    _ = close(Fd),
		    E1
	    end;
	E2 ->
	    E2
    end.

-spec script(Filename) -> {ok, Value} | {error, Reason} when
      Filename :: name_all(),
      Value :: term(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

script(File) ->
    script(File, erl_eval:new_bindings()).

-spec script(Filename, Bindings) -> {ok, Value} | {error, Reason} when
      Filename :: name_all(),
      Bindings :: erl_eval:binding_struct(),
      Value :: term(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

script(File, Bs) ->
    case open(File, [read]) of
	{ok, Fd} ->
	    R = eval_stream(Fd, return, Bs),
	    _ = close(Fd),
	    R;
	Error ->
	    Error
    end.

-spec path_script(Path, Filename) ->
             {ok, Value, FullName} | {error, Reason} when
      Path :: [Dir :: name_all()],
      Filename :: name_all(),
      Value :: term(),
      FullName :: filename_all(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

path_script(Path, File) ->
    path_script(Path, File, erl_eval:new_bindings()).

-spec path_script(Path, Filename, Bindings) ->
          {ok, Value, FullName} | {error, Reason} when
      Path :: [Dir :: name_all()],
      Filename :: name_all(),
      Bindings :: erl_eval:binding_struct(),
      Value :: term(),
      FullName :: filename_all(),
      Reason :: posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.

path_script(Path, File, Bs) ->
    case path_open(Path, File, [read]) of
	{ok,Fd,Full} ->
	    case eval_stream(Fd, return, Bs) of
		{ok,R} ->
		    _ = close(Fd),
		    {ok, R, Full};
		E1 ->
		    _ = close(Fd),
		    E1
	    end;
	E2 ->
	    E2
    end.
    

%% path_open(Paths, Filename, Mode) ->
%%	{ok,FileDescriptor,FullName}
%%	{error,Reason}
%%
%% Searches the Paths for file Filename which can be opened with Mode.
%% The path list is ignored if Filename contains an absolute path.

-spec path_open(Path, Filename, Modes) ->
             {ok, IoDevice, FullName} | {error, Reason} when
      Path :: [Dir :: name_all()],
      Filename :: name_all(),
      Modes :: [mode()],
      IoDevice :: io_device(),
      FullName :: filename_all(),
      Reason :: posix() | badarg | system_limit.

path_open(PathList, Name, Mode) ->
    case file_name(Name) of
	{error, _} = Error ->
	    Error;
	FileName ->
	    case filename:pathtype(FileName) of
		relative ->
		    path_open_first(PathList, FileName, Mode, enoent);
		_ ->
		    case open(Name, Mode) of
			{ok, Fd} ->
			    {ok, Fd, Name};
			Error ->
			    Error
		    end
	    end
    end.

-spec change_mode(Filename, Mode) -> ok | {error, Reason} when
      Filename :: name_all(),
      Mode :: integer(),
      Reason :: posix() | badarg.

change_mode(Name, Mode) 
  when is_integer(Mode) ->
    write_file_info(Name, #file_info{mode=Mode}).

-spec change_owner(Filename, Uid) -> ok | {error, Reason} when
      Filename :: name_all(),
      Uid :: integer(),
      Reason :: posix() | badarg.

change_owner(Name, OwnerId) 
  when is_integer(OwnerId) ->
    write_file_info(Name, #file_info{uid=OwnerId}).

-spec change_owner(Filename, Uid, Gid) -> ok | {error, Reason} when
      Filename :: name_all(),
      Uid :: integer(),
      Gid :: integer(),
      Reason :: posix() | badarg.

change_owner(Name, OwnerId, GroupId) 
  when is_integer(OwnerId), is_integer(GroupId) ->
    write_file_info(Name, #file_info{uid=OwnerId, gid=GroupId}).

-spec change_group(Filename, Gid) -> ok | {error, Reason} when
      Filename :: name_all(),
      Gid :: integer(),
      Reason :: posix() | badarg.

change_group(Name, GroupId) 
  when is_integer(GroupId) ->
    write_file_info(Name, #file_info{gid=GroupId}).

-spec change_time(Filename, Mtime) -> ok | {error, Reason} when
      Filename :: name_all(),
      Mtime :: date_time(),
      Reason :: posix() | badarg.

change_time(Name, {{Y, M, D}, {H, Min, Sec}}=Time)
  when is_integer(Y), is_integer(M), is_integer(D),
       is_integer(H), is_integer(Min), is_integer(Sec)->
    write_file_info(Name, #file_info{mtime=Time}).

-spec change_time(Filename, Atime, Mtime) -> ok | {error, Reason} when
      Filename :: name_all(),
      Atime :: date_time(),
      Mtime :: date_time(),
      Reason :: posix() | badarg.

change_time(Name, {{AY, AM, AD}, {AH, AMin, ASec}}=Atime,
         {{MY, MM, MD}, {MH, MMin, MSec}}=Mtime)
  when is_integer(AY), is_integer(AM), is_integer(AD),
       is_integer(AH), is_integer(AMin), is_integer(ASec),
       is_integer(MY), is_integer(MM), is_integer(MD),
       is_integer(MH), is_integer(MMin), is_integer(MSec)->
    write_file_info(Name, #file_info{atime=Atime, mtime=Mtime}).

%%
%% Send data using sendfile
%%

%% 1 MB, Windows seems to behave badly if it is much larger then this
-define(MAX_CHUNK_SIZE, (1 bsl 20)).

-spec sendfile(RawFile, Socket, Offset, Bytes, Opts) ->
   {'ok', non_neg_integer()} | {'error', inet:posix() | 
				closed | badarg | not_owner} when
      RawFile :: fd(),
      Socket :: inet:socket(),
      Offset :: non_neg_integer(),
      Bytes :: non_neg_integer(),
      Opts :: [sendfile_option()].
sendfile(File, _Sock, _Offet, _Bytes, _Opts) when is_pid(File) ->
    {error, badarg};
sendfile(File, Sock, Offset, Bytes, []) ->
    sendfile(File, Sock, Offset, Bytes, ?MAX_CHUNK_SIZE, [], [], []);
sendfile(File, Sock, Offset, Bytes, Opts) ->
    try proplists:get_value(chunk_size, Opts, ?MAX_CHUNK_SIZE) of
        ChunkSize0 when is_integer(ChunkSize0) ->
            ChunkSize = erlang:min(ChunkSize0, ?MAX_CHUNK_SIZE),
            %% Support for headers, trailers and options has been removed
            %% because the Darwin and BSD API for using it does not play nice
            %% with non-blocking sockets. See unix_efile.c for more info.
            sendfile(File, Sock, Offset, Bytes, ChunkSize, [], [], Opts);
        _Other ->
            {error, badarg}
    catch
        error:_ -> {error, badarg}
    end.

%% sendfile/2
-spec sendfile(Filename, Socket) ->
   {'ok', non_neg_integer()} | {'error', inet:posix() | 
				closed | badarg | not_owner}
      when Filename :: name_all(),
	   Socket :: inet:socket().
sendfile(Filename, Sock)  ->
    case file:open(Filename, [read, raw, binary]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Fd} ->
	    Res = sendfile(Fd, Sock, 0, 0, []),
	    _ = file:close(Fd),
	    Res
    end.

%% Internal sendfile functions
sendfile(#file_descriptor{ module = Mod } = Fd, Sock, Offset, Bytes,
	 ChunkSize, Headers, Trailers, Opts)
  when is_port(Sock) ->
    case Mod:sendfile(Fd, Sock, Offset, Bytes, ChunkSize, Headers, Trailers,
		      Opts) of
	{error, enotsup} ->
	    sendfile_fallback(Fd, Sock, Offset, Bytes, ChunkSize,
			      Headers, Trailers);
	Else ->
	    Else
    end;
sendfile(_,_,_,_,_,_,_,_) ->
    {error, badarg}.

%%%
%% Sendfile Fallback
%%%
sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize,
		  Headers, Trailers)
  when Headers == []; is_integer(Headers) ->
    case sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize) of
	{ok, BytesSent} when is_list(Trailers),
			     Trailers =/= [],
			     is_integer(Headers) ->
	    sendfile_send(Sock, Trailers, BytesSent+Headers);
	{ok, BytesSent} when is_list(Trailers), Trailers =/= [] ->
	    sendfile_send(Sock, Trailers, BytesSent);
	{ok, BytesSent} when is_integer(Headers) ->
	    {ok, BytesSent + Headers};
	Else ->
	    Else
    end;
sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize, Headers, Trailers) ->
    case sendfile_send(Sock, Headers, 0) of
	{ok, BytesSent} ->
	    sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize, BytesSent,
			      Trailers);
	Else ->
	    Else
    end.


sendfile_fallback(File, Sock, Offset, Bytes, ChunkSize) ->
    {ok, CurrPos} = file:position(File, {cur, 0}),
    {ok, _NewPos} = file:position(File, {bof, Offset}),
    Res = sendfile_fallback_int(File, Sock, Bytes, ChunkSize, 0),
    _ = file:position(File, {bof, CurrPos}),
    Res.


sendfile_fallback_int(File, Sock, Bytes, ChunkSize, BytesSent)
  when Bytes > BytesSent; Bytes == 0 ->
    Size = if Bytes == 0 ->
		   ChunkSize;
	       (Bytes - BytesSent) < ChunkSize ->
		   Bytes - BytesSent;
	      true ->
		   ChunkSize
	   end,
    case file:read(File, Size) of
	{ok, Data} ->
	    case sendfile_send(Sock, Data, BytesSent) of
		{ok,NewBytesSent} ->
		    sendfile_fallback_int(
		      File, Sock, Bytes, ChunkSize,
		      NewBytesSent);
		Error ->
		    Error
	    end;
	eof ->
	    {ok, BytesSent};
	Error ->
	    Error
    end;
sendfile_fallback_int(_File, _Sock, BytesSent, _ChunkSize, BytesSent) ->
    {ok, BytesSent}.

sendfile_send(Sock, Data, Old) ->
    Len = iolist_size(Data),
    case gen_tcp:send(Sock, Data) of
	ok ->
	    {ok, Len+Old};
	Else ->
	    Else
    end.



%%%-----------------------------------------------------------------
%%% Helpers

consult_stream(Fd) ->
    _ = epp:set_encoding(Fd),
    consult_stream(Fd, 1, []).

consult_stream(Fd, Line, Acc) ->
    case io:read(Fd, '', Line) of
	{ok,Term,EndLine} ->
	    consult_stream(Fd, EndLine, [Term|Acc]);
	{error,Error,_Line} ->
	    {error,Error};
	{eof,_Line} ->
	    {ok,lists:reverse(Acc)}
    end.

eval_stream(Fd, Handling, Bs) ->
    _ = epp:set_encoding(Fd),
    eval_stream(Fd, Handling, 1, undefined, [], Bs).

eval_stream(Fd, H, Line, Last, E, Bs) ->
    eval_stream2(io:parse_erl_exprs(Fd, '', Line), Fd, H, Last, E, Bs).

eval_stream2({ok,Form,EndLine}, Fd, H, Last, E, Bs0) ->
    try erl_eval:exprs(Form, Bs0) of
	{value,V,Bs} ->
	    eval_stream(Fd, H, EndLine, {V}, E, Bs)
    catch Class:Reason:StackTrace ->
            Error = {EndLine,?MODULE,{Class,Reason,StackTrace}},
	    eval_stream(Fd, H, EndLine, Last, [Error|E], Bs0)
    end;
eval_stream2({error,What,EndLine}, Fd, H, Last, E, Bs) ->
    eval_stream(Fd, H, EndLine, Last, [What | E], Bs);
eval_stream2({eof,EndLine}, _Fd, H, Last, E, _Bs) ->
    case {H, Last, E} of
	{return, {Val}, []} ->
	    {ok, Val};
	{return, undefined, E} ->
	    {error, hd(lists:reverse(E, [{EndLine,?MODULE,undefined_script}]))};
	{ignore, _, []} ->
	    ok;
	{_, _, [_|_] = E} ->
	    {error, hd(lists:reverse(E))}
    end.

path_open_first([Path|Rest], Name, Mode, LastError) ->
    case file_name(Path) of
	{error, _} = Error ->
	    Error;
	FilePath ->
	    FileName = fname_join(FilePath, Name),
	    case open(FileName, Mode) of
		{ok, Fd} ->
		    {ok, Fd, FileName};
		{error, Reason} when Reason =:= enoent; Reason =:= enotdir ->
		    path_open_first(Rest, Name, Mode, LastError);
		Error ->
		    Error
	    end
    end;
path_open_first([], _Name, _Mode, LastError) ->
    {error, LastError}.

fname_join(".", Name) ->
    Name;
fname_join(Dir, Name) ->
    filename:join(Dir, Name).

%%%-----------------------------------------------------------------
%%% Utility functions.

%% file_name(FileName)
%% 	Generates a flat file name from a deep list of atoms and 
%% 	characters (integers).

file_name(N) when is_binary(N) ->
    N;
file_name(N) ->
    try 
        file_name_1(N,file:native_name_encoding())
    catch Reason ->
        {error, Reason}
    end.

file_name_1([C|T],latin1) when is_integer(C), C < 256->
    [C|file_name_1(T,latin1)];
file_name_1([C|T],utf8) when is_integer(C) ->
    [C|file_name_1(T,utf8)];
file_name_1([H|T],E) ->
    file_name_1(H,E) ++ file_name_1(T,E);
file_name_1([],_) ->
    [];
file_name_1(N,_) when is_atom(N) ->
    atom_to_list(N);
file_name_1(_,_) ->
    throw(badarg).

make_binary(Bin) when is_binary(Bin) ->
    Bin;
make_binary(List) ->
    %% Convert the list to a binary in order to avoid copying a list
    %% to the file server.
    try 
        erlang:iolist_to_binary(List)
    catch error:Reason ->
        {error, Reason}
    end.

mode_list(read) ->
    [read];
mode_list(write) ->
    [write];
mode_list(read_write) ->
    [read, write];
mode_list({binary, Mode}) when is_atom(Mode) ->
    [binary | mode_list(Mode)];
mode_list({character, Mode}) when is_atom(Mode) ->
    mode_list(Mode);
mode_list(_) ->
    [{error, badarg}].

%%-----------------------------------------------------------------
%% Functions for communicating with the file server

call(Command, Args) when is_list(Args) ->
    X = erlang:dt_spread_tag(true),
    Y = gen_server:call(?FILE_SERVER, list_to_tuple([Command | Args]), 
			infinity),
    erlang:dt_restore_tag(X),
    Y.

check_and_call(Command, Args) when is_list(Args) ->
    case check_args(Args) of
	ok ->
	    call(Command, Args);
	Error ->
	    Error
    end.

check_args([{error, _}=Error|_Rest]) ->
    Error;
check_args([_Name|Rest]) ->
    check_args(Rest);
check_args([]) ->
    ok.

%%-----------------------------------------------------------------
%% Function for communicating with a file io server.
%% The messages sent have the following formats:
%%
%%	{file_request,From,ReplyAs,Request}
%%	{file_reply,ReplyAs,Reply}

file_request(Io, Request) ->
    Ref = erlang:monitor(process, Io),
    Io ! {file_request,self(),Ref,Request},
    receive
	{file_reply,Ref,Reply} ->
	    erlang:demonitor(Ref, [flush]),
	    Reply;
	{'DOWN', Ref, _, _, _} ->
	    {error, terminated}
    end.
