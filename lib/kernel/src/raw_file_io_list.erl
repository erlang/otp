%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(raw_file_io_list).

-export([close/1, sync/1, datasync/1, truncate/1, advise/4, allocate/3,
         position/2, write/2, pwrite/2, pwrite/3,
         read_line/1, read/2, pread/2, pread/3]).

%% OTP internal.
-export([ipread_s32bu_p32bu/3, sendfile/8]).

-export([open_layer/3]).

-include("file_int.hrl").

open_layer(Filename, Modes, [list]) ->
    case raw_file_io:open(Filename, [binary | Modes]) of
        {ok, PrivateFd} -> {ok, make_public_fd(PrivateFd, Modes)};
        Other -> Other
    end.

%% We can skip wrapping the file if it's write-only since only read operations
%% are affected by list mode. Since raw_file_io fills in all implicit options
%% for us, all we need to do is check whether 'read' is among them.
make_public_fd(PrivateFd, Modes) ->
    case lists:member(read, Modes) of
        true -> #file_descriptor{ module = ?MODULE, data = PrivateFd };
        false -> PrivateFd
    end.

close(Fd) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, close, []).

sync(Fd) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, sync, []).
datasync(Fd) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, datasync, []).

truncate(Fd) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, truncate, []).

advise(Fd, Offset, Length, Advise) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, advise, [Offset, Length, Advise]).
allocate(Fd, Offset, Length) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, allocate, [Offset, Length]).

position(Fd, Mark) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, position, [Mark]).

write(Fd, IOData) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, write, [IOData]).

pwrite(Fd, Offset, IOData) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, pwrite, [Offset, IOData]).
pwrite(Fd, LocBytes) ->
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, pwrite, [LocBytes]).

read_line(Fd) ->
    PrivateFd = Fd#file_descriptor.data,
    case ?CALL_FD(PrivateFd, read_line, []) of
        {ok, Binary} -> {ok, binary_to_list(Binary)};
        Other -> Other
    end.
read(Fd, Size) ->
    PrivateFd = Fd#file_descriptor.data,
    case ?CALL_FD(PrivateFd, read, [Size]) of
        {ok, Binary} -> {ok, binary_to_list(Binary)};
        Other -> Other
    end.
pread(Fd, Offset, Size) ->
    PrivateFd = Fd#file_descriptor.data,
    case ?CALL_FD(PrivateFd, pread, [Offset, Size]) of
        {ok, Binary} -> {ok, binary_to_list(Binary)};
        Other -> Other
    end.
pread(Fd, LocNums) ->
    PrivateFd = Fd#file_descriptor.data,
    case ?CALL_FD(PrivateFd, pread, [LocNums]) of
        {ok, LocResults} ->
            TranslatedResults =
                [ case Result of
                      Result when is_binary(Result) -> binary_to_list(Result);
                      eof -> eof
                  end || Result <- LocResults ],
            {ok, TranslatedResults};
        Other -> Other
    end.

ipread_s32bu_p32bu(Fd, Offset, MaxSize) ->
    PrivateFd = Fd#file_descriptor.data,
    case ?CALL_FD(PrivateFd, ipread_s32bu_p32bu, [Offset, MaxSize]) of
        {ok, {Size, Pointer, Binary}} when is_binary(Binary) ->
            {ok, {Size, Pointer, binary_to_list(Binary)}};
        Other ->
            Other
    end.

sendfile(Fd, Dest, Offset, Bytes, ChunkSize, Headers, Trailers, Flags) ->
    Args = [Dest, Offset, Bytes, ChunkSize, Headers, Trailers, Flags],
    PrivateFd = Fd#file_descriptor.data,
    ?CALL_FD(PrivateFd, sendfile, Args).
