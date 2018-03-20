%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
-module(prim_file).

-export([on_load/0]).

-export([open/2, close/1,
         sync/1, datasync/1, truncate/1, advise/4, allocate/3,
         read_line/1, read/2, write/2, position/2,
         pread/2, pread/3, pwrite/2, pwrite/3]).

%% OTP internal.
-export([ipread_s32bu_p32bu/3, sendfile/8, altname/1, get_handle/1]).

-export([read_file/1, write_file/2]).

-export([read_link/1, read_link_all/1,
         read_link_info/1, read_link_info/2,
         read_file_info/1, read_file_info/2,
         write_file_info/2, write_file_info/3]).

-export([list_dir/1, list_dir_all/1]).

-export([get_cwd/0, get_cwd/1, set_cwd/1,
         delete/1, rename/2,
         make_dir/1, del_dir/1,
         make_link/2, make_symlink/2]).

-define(MIN_READLINE_SIZE, 256).
-define(LARGEFILESIZE, (1 bsl 63)).

-export([copy/3]).

-include("file_int.hrl").

-type prim_file_ref() :: term().

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

%%

%% Returns {error, Reason} | {ok, BytesCopied}
copy(#file_descriptor{module = ?MODULE} = Source,
     #file_descriptor{module = ?MODULE} = Dest,
     Length)
  when is_integer(Length), Length >= 0;
       is_atom(Length) ->
    %% XXX Should be moved down to the driver for optimization.
    file:copy_opened(Source, Dest, Length).

on_load() ->
    ok = erlang:load_nif(atom_to_list(?MODULE), 0).

open(Name, Modes) ->
    %% The try/catch pattern seen here is used throughout the file to adhere to
    %% the public file interface, which has leaked through for ages because of
    %% "raw files."
    try open_nif(encode_path(Name), Modes) of
        {ok, Ref} -> {ok, make_fd(Ref, Modes)};
        {error, Reason} -> {error, Reason}
    catch
        error:badarg -> {error, badarg}
    end.

make_fd(FRef, Modes) ->
    #file_descriptor{module = ?MODULE, data = build_fd_data(FRef, Modes) }.

close(Fd) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        close_nif(FRef)
    catch
        error:badarg -> {error, badarg}
    end.

read(Fd, Size) ->
    try
        #{ handle := FRef,
           r_ahead_size := RASz,
           r_buffer := RBuf } = get_fd_data(Fd),
        read_1(FRef, RBuf, prim_buffer:size(RBuf), RASz, Size)
    catch
        error:badarg -> {error, badarg}
    end.

-spec read_1(FRef, RBuf, RBufSz, RASz, RSz) -> Result when
      FRef :: prim_file_ref(),
      RBuf :: term(),
      RBufSz :: non_neg_integer(),
      RASz :: non_neg_integer(),
      RSz :: non_neg_integer(),
      Result :: eof | {ok, binary()} | {error, Reason :: atom()}.
read_1(_FRef, RBuf, RBufSz, _RASz, RSz) when RBufSz >= RSz ->
    {ok, prim_buffer:read(RBuf, RSz)};
read_1(FRef, RBuf, RBufSz, RASz, RSz) when RBufSz > 0 ->
    Buffered = prim_buffer:read(RBuf, RBufSz),
    case read_1(FRef, RBuf, 0, RASz, RSz - RBufSz) of
        {ok, Data} ->
            {ok, <<Buffered/binary, Data/binary>>};
        eof ->
            {ok, Buffered};
        {error, Reason} ->
            {error, Reason}
    end;
read_1(FRef, RBuf, RBufSz, RASz, RSz) when RBufSz =:= 0 ->
    case read_nif(FRef, RASz + RSz) of
        {ok, Data} when byte_size(Data) > RSz ->
            {First, Rest} = split_binary(Data, RSz),
            prim_buffer:write(RBuf, [Rest]),
            {ok, First};
        {ok, Data} when byte_size(Data) =< RSz ->
            {ok, Data};
        eof ->
            eof;
        {error, Reason} ->
            {error, Reason}
    end.

read_line(Fd) ->
    try
        #{ handle := FRef,
           r_ahead_size := RASz,
           r_buffer := RBuf } = get_fd_data(Fd),
        SearchResult = prim_buffer:find_byte_index(RBuf, $\n),
        LineSize = max(?MIN_READLINE_SIZE, RASz),
        read_line_1(FRef, RBuf, SearchResult, LineSize)
    catch
        error:badarg -> {error, badarg}
    end.

-spec read_line_1(FRef, RBuf, SearchResult, LineSize) -> Result when
      FRef :: prim_file_ref(),
      RBuf :: term(),
      SearchResult :: not_found | {ok, non_neg_integer()},
      LineSize :: non_neg_integer(),
      Result :: eof | {ok, binary()} | {error, Reason :: atom()}.
read_line_1(FRef, RBuf, not_found, LineSize) ->
    case read_nif(FRef, LineSize) of
        {ok, Data} ->
            prim_buffer:write(RBuf, [Data]),
            SearchResult = prim_buffer:find_byte_index(RBuf, $\n),
            read_line_1(FRef, RBuf, SearchResult, LineSize);
        eof ->
            case prim_buffer:size(RBuf) of
                Size when Size > 0 -> {ok, prim_buffer:read(RBuf, Size)};
                Size when Size =:= 0 -> eof
            end;
        {error, Reason} ->
            {error, Reason}
    end;
read_line_1(_FRef, RBuf, {ok, LFIndex}, _LineSize) ->
    %% Translate CRLF into just LF, completely ignoring which encoding is used.
    CRIndex = (LFIndex - 1),
    case prim_buffer:read(RBuf, LFIndex + 1) of
        <<Line:CRIndex/binary, "\r\n">> -> {ok, <<Line/binary, "\n">>};
        Line -> {ok, Line}
    end.

write(Fd, IOData) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        reset_write_position(Fd),
        write_1(FRef, erlang:iolist_to_iovec(IOData))
    catch
        error:badarg -> {error, badarg}
    end.
write_1(FRef, IOVec) ->
    case write_nif(FRef, IOVec) of
        {continue, Remainder} ->
            write_1(FRef, Remainder);
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

truncate(Fd) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        reset_write_position(Fd),
        truncate_nif(FRef)
    catch
        error:badarg -> {error, badarg}
    end.

advise(Fd, Offset, Length, Advise) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        advise_nif(FRef, Offset, Length, Advise)
    catch
        error:badarg -> {error, badarg}
    end.

allocate(Fd, Offset, Length) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        allocate_nif(FRef, Offset, Length)
    catch
        error:badarg -> {error, badarg}
    end.

sync(Fd) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        sync_nif(FRef, 0)
    catch
        error:badarg -> {error, badarg}
    end.

datasync(Fd) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        sync_nif(FRef, 1)
    catch
        error:badarg -> {error, badarg}
    end.

position(Fd, {cur, Offset}) ->
    try
        %% Adjust our current position according to how much we've read ahead.
        #{ r_buffer := RBuf } = get_fd_data(Fd),
        position_1(Fd, cur, Offset - prim_buffer:size(RBuf))
    catch
        error:badarg -> {error, badarg}
    end;
position(Fd, {Mark, Offset}) ->
    try
        position_1(Fd, Mark, Offset)
    catch
        error:badarg -> {error, badarg}
    end;
position(Fd, cur) -> position(Fd, {cur, 0});
position(Fd, bof) -> position(Fd, {bof, 0});
position(Fd, eof) -> position(Fd, {eof, 0});
position(Fd, Offset) -> position(Fd, {bof, Offset}).

position_1(Fd, Mark, Offset) ->
    #{ handle := FRef, r_buffer := RBuf } = get_fd_data(Fd),
    prim_buffer:wipe(RBuf),
    seek_nif(FRef, Mark, Offset).

pread(Fd, Offset, Size) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        pread_nif(FRef, Offset, Size)
    catch
        error:badarg -> {error, badarg}
    end.

pread(Fd, LocNums) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        pread_list(FRef, LocNums, [])
    catch
        error:badarg -> {error, badarg}
    end.

-spec pread_list(FRef, LocNums, ResultList) -> Result when
      FRef :: prim_file_ref(),
      LocNums :: list({Offset :: non_neg_integer(),
                       Size :: non_neg_integer()}),
      ResultList :: list(eof | binary()),
      Result :: {ok, ResultList} | {error, Reason :: atom()}.
pread_list(_FRef, [], ResultList) ->
    {ok, reverse_list(ResultList)};
pread_list(FRef, [{Offset, Size} | Rest], ResultList) ->
    case pread_nif(FRef, Offset, Size) of
        {ok, Data} ->
            pread_list(FRef, Rest, [Data | ResultList]);
        eof ->
            pread_list(FRef, Rest, [eof | ResultList]);
        {error, Reason} ->
            {error, Reason}
    end.

pwrite(Fd, Offset, IOData) ->
    try
        #{ handle := FRef, r_buffer := RBuf } = get_fd_data(Fd),
        prim_buffer:wipe(RBuf),
        pwrite_plain(FRef, Offset, erlang:iolist_to_iovec(IOData))
    catch
        error:badarg -> {error, badarg}
    end.
pwrite_plain(FRef, Offset, IOVec) ->
    case pwrite_nif(FRef, Offset, IOVec) of
        {continue, BytesWritten, Remainder} ->
            pwrite_plain(FRef, Offset + BytesWritten, Remainder);
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

pwrite(Fd, LocBytes) ->
    try
        #{ handle := FRef, r_buffer := RBuf } = get_fd_data(Fd),
        prim_buffer:wipe(RBuf),
        pwrite_list(FRef, LocBytes, 0)
    catch
        error:badarg -> {error, badarg}
    end.

-spec pwrite_list(FRef, LocBytes, Successes) -> Result when
      FRef :: prim_file_ref(),
      LocBytes :: list({Offset :: non_neg_integer(),
                        IOData :: iodata()}),
      Successes :: non_neg_integer(),
      Result :: ok | {error, {Successes, Reason :: atom()}}.
pwrite_list(_FRef, [], _Successes) ->
    ok;
pwrite_list(FRef, [{Offset, IOData} | Rest], Successes) ->
    case pwrite_plain(FRef, Offset, erlang:iolist_to_iovec(IOData)) of
        {error, Reason} -> {error, {Successes, Reason}};
        ok -> pwrite_list(FRef, Rest, Successes + 1)
    end.

sendfile(Fd, Socket, Offset, Bytes, _ChunkSize, [], [], _Flags) ->
    %% There's a very nasty race in here; if we die just prior to duplicating
    %% the handle down in the sendfile call, it might get reused by something
    %% entirely different and we'll leak unknown data to the socket until it
    %% dies soon after.
    %%
    %% This bug was inherited from the old driver, except it was vulnerable to
    %% the bug at any point and not just during setup.
    %%
    %% We'll have to live with this until we have a way to unambiguously
    %% transfer things between drivers or NIFs. Current ideas all fall afoul
    %% of the Two Generals problem.
    try
        advise(Fd, Offset, Bytes, sequential),
        prim_inet:sendfile(Socket, get_handle(Fd), Offset, Bytes)
    catch
        error:badarg -> {error, badarg}
    end;
sendfile(_Fd, _Socket, _Offset, _Bytes, _ChunkSize, _Headers, _Trailers, _Flags) ->
    {error, enotsup}.

%% Undocumented internal function that reads a data block with indirection.
%%
%% This is only used once in DETS and can easily be emulated with pread/2, but
%% it's pretty performance-sensitive so we've implemented it down in the NIF to
%% avoid excessive rescheduling.
-spec ipread_s32bu_p32bu(Fd, Offset, MaxSize) -> Result when
      Fd :: #file_descriptor{},
      Offset :: non_neg_integer(),
      MaxSize :: non_neg_integer() | infinity,
      Result :: {ok, Size :: non_neg_integer(),
                     Pointer :: non_neg_integer(),
                     Data :: iodata() | eof} |
                eof |
                {error, Reason :: atom()}.
ipread_s32bu_p32bu(Fd, Offset, Infinity) when is_atom(Infinity) ->
    ipread_s32bu_p32bu(Fd, Offset, (1 bsl 31) - 1);
ipread_s32bu_p32bu(Fd, Offset, MaxSize)
        when is_integer(Offset), is_integer(MaxSize) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        ipread_s32bu_p32bu_nif(FRef, Offset, MaxSize)
    catch
        error:badarg -> {error, badarg}
    end;
ipread_s32bu_p32bu(_Fd, _Offset, _MaxSize) ->
    {error, badarg}.
ipread_s32bu_p32bu_nif(_FRef, _Offset, _MaxSize) ->
    erlang:nif_error(undef).

%% Returns the binary representation of the underlying handle, for use in
%% tricky operations like sendfile/8.
-spec get_handle(Fd) -> Result when
      Fd :: #file_descriptor{},
      Result :: binary() | {error, Reason :: atom()}.
get_handle(Fd) ->
    try
        #{ handle := FRef } = get_fd_data(Fd),
        get_handle_nif(FRef)
    catch
        error:badarg -> {error, badarg}
    end.

%% Resets the write head to the position the user believes we're at, which may
%% not be the same as the real one when read caching is in effect.
reset_write_position(Fd) ->
    #{ r_buffer := RBuf } = Fd#file_descriptor.data,
    case prim_buffer:size(RBuf) of
        Size when Size > 0 -> position(Fd, cur);
        Size when Size =:= 0 -> ok
    end.

get_fd_data(#file_descriptor{ data = Data }) ->
    #{ owner := Owner } = Data,
    case self() of
        Owner -> Data;
        _ -> error(not_on_controlling_process)
    end.

build_fd_data(FRef, Modes) ->
    Defaults =
        #{ owner => self(),
           handle => FRef,
           r_ahead_size => 0,
           r_buffer => prim_buffer:new() },
    fill_fd_option_map(Modes, Defaults).

fill_fd_option_map([], Map) ->
    Map;

fill_fd_option_map([read_ahead | Modes], Map) ->
    fill_fd_option_map([{read_ahead, 64 bsl 10} | Modes], Map);
fill_fd_option_map([{read_ahead, Size} | Modes], Map) ->
    fill_fd_option_map(Modes, Map#{ r_ahead_size => Size });

fill_fd_option_map([_Ignored | Modes], Map) ->
    fill_fd_option_map(Modes, Map).

open_nif(_Name, _Modes) ->
    erlang:nif_error(undef).
close_nif(_FileRef) ->
    erlang:nif_error(undef).
read_nif(_FileRef, _Size) ->
    erlang:nif_error(undef).
write_nif(_FileRef, _IOVec) ->
    erlang:nif_error(undef).
pread_nif(_FileRef, _Offset, _Size) ->
    erlang:nif_error(undef).
pwrite_nif(_FileRef, _Offset, _IOVec) ->
    erlang:nif_error(undef).
seek_nif(_FileRef, _Mark, _Offset) ->
    erlang:nif_error(undef).
sync_nif(_FileRef, _DataOnly) ->
    erlang:nif_error(undef).
advise_nif(_FileRef, _Offset, _Length, _Advise) ->
    erlang:nif_error(undef).
allocate_nif(_FileRef, _Offset, _Length) ->
    erlang:nif_error(undef).
truncate_nif(_FileRef) ->
    erlang:nif_error(undef).
get_handle_nif(_FileRef) ->
    erlang:nif_error(undef).

%%
%% Quality-of-life helpers
%%

read_file(Filename) ->
    %% We're doing this operation in the NIF to avoid excessive rescheduling.
    try
        read_file_nif(encode_path(Filename))
    catch
        error:badarg -> {error, badarg}
    end.
read_file_nif(_Filename) ->
    erlang:nif_error(undef).

write_file(Filename, Bytes) ->
    write_file(Filename, Bytes, []).
write_file(Filename, Bytes, Modes) ->
    case open(Filename, [write, binary | Modes]) of
        {ok, Fd} ->
            Result = write(Fd, Bytes),
            close(Fd),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Filesystem operations
%%

read_link(Name) -> read_link_1(Name, false).
read_link_all(Name) -> read_link_1(Name, true).

read_link_1(Name, AcceptRawNames) ->
    try read_link_nif(encode_path(Name)) of
        {ok, RawName} -> translate_raw_name(RawName, AcceptRawNames);
        {error, Reason} -> {error, Reason}
    catch
        error:badarg -> {error, badarg}
    end.

translate_raw_name(RawName, SilentFailure) ->
    case decode_path(RawName) of
        Converted when is_list(Converted) -> {ok, Converted};
        {error, _Reason} when SilentFailure =:= false -> {error, einval};
        {error, _Reason} when SilentFailure =:= true -> {ok, RawName}
    end.

list_dir(Name) -> list_dir_1(Name, true).
list_dir_all(Name) -> list_dir_1(Name, false).

list_dir_1(Name, SkipInvalid) ->
    try list_dir_nif(encode_path(Name)) of
        {ok, RawNames} -> list_dir_convert(RawNames, SkipInvalid, []);
        {error, Reason} -> {error, Reason}
    catch
        error:badarg -> {error, badarg}
    end.

list_dir_convert([], _SkipInvalid, Result) ->
    {ok, Result};
list_dir_convert([RawName | Rest], SkipInvalid, Result) ->
    case decode_path(RawName) of
        Converted when is_list(Converted) ->
            list_dir_convert(Rest, SkipInvalid, [Converted | Result]);
        {error, _} when SkipInvalid =:= false ->
            list_dir_convert(Rest, SkipInvalid, [RawName | Result]);

        %% If the filename cannot be converted, return error or ignore with
        %% optional error logger warning depending on +fn{u|a}{i|e|w} emulator
        %% switches.
        {error, ignore} ->
            list_dir_convert(Rest, SkipInvalid, Result);
        {error, warning} ->
            error_logger:warning_msg(
                "Non-unicode filename ~p ignored\n", [RawName]),
            list_dir_convert(Rest, SkipInvalid, Result);
        {error, _} ->
            {error, {no_translation, RawName}}
    end.

read_file_info(Filename) ->
    read_info_1(Filename, 1, local).
read_file_info(Filename, Opts) ->
    read_info_1(Filename, 1, proplist_get_value(time, Opts, local)).

read_link_info(Name) ->
    read_info_1(Name, 0, local).
read_link_info(Name, Opts) ->
    read_info_1(Name, 0, proplist_get_value(time, Opts, local)).

read_info_1(Name, FollowLinks, TimeType) ->
    try read_info_nif(encode_path(Name), FollowLinks) of
        {error, Reason} -> {error, Reason};
        FileInfo ->
            CTime = from_posix_seconds(FileInfo#file_info.ctime, TimeType),
            MTime = from_posix_seconds(FileInfo#file_info.mtime, TimeType),
            ATime = from_posix_seconds(FileInfo#file_info.atime, TimeType),
            {ok, FileInfo#file_info{ ctime = CTime, mtime = MTime, atime = ATime }}
    catch
        error:badarg -> {error, badarg}
    end.

write_file_info(Filename, Info) ->
    write_file_info_1(Filename, Info, local).
write_file_info(Filename, Info, Opts) ->
    write_file_info_1(Filename, Info, proplist_get_value(time, Opts, local)).

write_file_info_1(Filename, Info, TimeType) ->
    #file_info{ mode = Modes,
                uid = Uid,
                gid = Gid,
                atime = ATime0,
                mtime = MTime0,
                ctime = CTime0} = Info,
    try
        % ATime and/or MTime might be undefined
        %  - use localtime() for atime, if atime is undefined
        %  - use atime as mtime if mtime is undefined
        %  - use mtime as ctime if ctime is undefined
        ATime = file_info_convert_atime(ATime0, TimeType),
        MTime = file_info_convert_mtime(MTime0, ATime, TimeType),
        CTime = file_info_convert_ctime(CTime0, MTime, TimeType),
        EncodedName = encode_path(Filename),

        %% This is a bit ugly but we need to handle partial failures the same
        %% way the old driver did.
        throw_on_error(set_owner(EncodedName, Uid, Gid)),
        throw_on_error(set_permissions(EncodedName, Modes)),
        throw_on_error(set_time(EncodedName, ATime, MTime, CTime))
    catch
        throw:Reason -> {error, Reason};
        error:_ -> {error, badarg}
    end.

set_owner(EncodedName, Uid, undefined) ->
    set_owner(EncodedName, Uid, -1);
set_owner(EncodedName, undefined, Gid) ->
    set_owner(EncodedName, -1, Gid);
set_owner(EncodedName, Uid, Gid) ->
    set_owner_nif(EncodedName, Uid, Gid).
set_owner_nif(_Path, _Uid, _Gid) ->
    erlang:nif_error(undef).

set_permissions(_EncodedName, undefined) ->
    ok;
set_permissions(EncodedName, Permissions) ->
    set_permissions_nif(EncodedName, Permissions).
set_permissions_nif(_Path, _Permissions) ->
    erlang:nif_error(undef).

set_time(EncodedName, ATime, MTime, CTime) ->
    set_time_nif(EncodedName, ATime, MTime, CTime).
set_time_nif(_Path, _ATime, _MTime, _CTime) ->
    erlang:nif_error(undef).

throw_on_error(ok) -> ok;
throw_on_error({error, enotsup}) -> ok;
throw_on_error({error, Reason}) -> throw(Reason).

file_info_convert_atime(ATime, TimeType) when ATime =/= undefined ->
    to_posix_seconds(ATime, TimeType);
file_info_convert_atime(undefined, local) ->
    to_posix_seconds(erlang:localtime(), local);
file_info_convert_atime(undefined, universal) ->
    to_posix_seconds(erlang:universaltime(), universal);
file_info_convert_atime(undefined, posix) ->
    erlang:universaltime_to_posixtime(erlang:universaltime()).

file_info_convert_mtime(undefined, ATime, _TimeType) ->
    ATime;
file_info_convert_mtime(MTime, _ATime, TimeType) ->
    to_posix_seconds(MTime, TimeType).

file_info_convert_ctime(undefined, MTime, _TimeType) ->
    MTime;
file_info_convert_ctime(CTime, _MTime, TimeType) ->
    to_posix_seconds(CTime, TimeType).

%% This is only relevant on Windows, so we assume that format to simplify the
%% internals.
get_cwd([Letter, $:]) when Letter >= $A, Letter =< $Z ->
    get_dcwd(Letter - $A + 1);
get_cwd([Letter, $:]) when Letter >= $a, Letter =< $z ->
    get_dcwd(Letter - $a + 1);
get_cwd([_|_]) ->
    {error, einval};
get_cwd(_) ->
    {error, badarg}.
get_dcwd(Index) ->
    try get_device_cwd_nif(Index) of
        {ok, RawPath} -> {ok, decode_path(RawPath)};
        {error, Reason} -> {error, Reason}
    catch
        error:badarg -> {error, badarg}
    end.

get_cwd() ->
    try get_cwd_nif() of
        {ok, RawPath} -> {ok, decode_path(RawPath)};
        {error, Reason} -> {error, Reason}
    catch
        error:badarg -> {error, badarg}
    end.
set_cwd(Path) ->
    try
        case is_path_translatable(Path) of
            true -> set_cwd_nif(encode_path(Path));
            false -> {error, no_translation}
        end
    catch
        error:badarg -> {error, badarg}
    end.

delete(Path) ->
    try
        del_file_nif(encode_path(Path))
    catch
        error:badarg -> {error, badarg}
    end.

rename(Source, Destination) ->
    try
        rename_nif(encode_path(Source), encode_path(Destination))
    catch
        error:badarg -> {error, badarg}
    end.
make_dir(Path) ->
    try
        make_dir_nif(encode_path(Path))
    catch
        error:badarg -> {error, badarg}
    end.
del_dir(Path) ->
    try
        del_dir_nif(encode_path(Path))
    catch
        error:badarg -> {error, badarg}
    end.
make_link(Existing, New) ->
    try
        make_hard_link_nif(encode_path(Existing), encode_path(New))
    catch
        error:badarg -> {error, badarg}
    end.
make_symlink(Existing, New) ->
    try
        make_soft_link_nif(encode_path(Existing), encode_path(New))
    catch
        error:badarg -> {error, badarg}
    end.

altname(Path) ->
    try altname_nif(encode_path(Path)) of
        {ok, RawPath} -> {ok, decode_path(RawPath)};
        Other -> Other
    catch
        error:badarg -> {error, badarg}
    end.

list_dir_nif(_Path) ->
    erlang:nif_error(undef).
read_link_nif(_Path) ->
    erlang:nif_error(undef).
read_info_nif(_Path, _FollowLinks) ->
    erlang:nif_error(undef).
make_hard_link_nif(_Existing, _New) ->
    erlang:nif_error(undef).
make_soft_link_nif(_Existing, _New) ->
    erlang:nif_error(undef).
rename_nif(_Source, _Destination) ->
    erlang:nif_error(undef).
make_dir_nif(_Path) ->
    erlang:nif_error(undef).
del_file_nif(_Path) ->
    erlang:nif_error(undef).
del_dir_nif(_Path) ->
    erlang:nif_error(undef).
get_device_cwd_nif(_DevicePath) ->
    erlang:nif_error(undef).
set_cwd_nif(_Path) ->
    erlang:nif_error(undef).
get_cwd_nif() ->
    erlang:nif_error(undef).
altname_nif(_Path) ->
    erlang:nif_error(undef).

%%
%% General helper functions.
%%

%% We know for certain that lists:reverse/2 is a BIF, so it's safe to use it
%% even though this module is preloaded.
reverse_list(List) -> lists:reverse(List).

proplist_get_value(_Key, [], Default) ->
    Default;
proplist_get_value(Key, [{Key, Value} | _Rest], _Default) ->
    Value;
proplist_get_value(Key, [Key | _Rest], _Default) ->
    true;
proplist_get_value(Key, [_Other | Rest], Default) ->
    proplist_get_value(Key, Rest, Default).

encode_path(Path) ->
    prim_file:internal_name2native(Path).
decode_path(NativePath) when is_binary(NativePath) ->
    prim_file:internal_native2name(NativePath).

is_path_translatable(Path) when is_list(Path) ->
    true;
is_path_translatable(Path) ->
    prim_file:is_translatable(Path).

%% We don't actually want this here
%%
%% We want to use posix time in all prim but erl_prim_loader makes that tricky
%% It is probably needed to redo the whole erl_prim_loader

from_posix_seconds(Seconds, posix) when is_integer(Seconds) ->
    Seconds;
from_posix_seconds(Seconds, universal) when is_integer(Seconds) ->
    erlang:posixtime_to_universaltime(Seconds);
from_posix_seconds(Seconds, local) when is_integer(Seconds) ->
    erlang:universaltime_to_localtime(erlang:posixtime_to_universaltime(Seconds)).

to_posix_seconds(Seconds, posix) when is_integer(Seconds) ->
    Seconds;
to_posix_seconds({_,_} = Datetime, universal) ->
    erlang:universaltime_to_posixtime(Datetime);
to_posix_seconds({_,_} = Datetime, local) ->
    erlang:universaltime_to_posixtime(erlang:localtime_to_universaltime(Datetime)).
