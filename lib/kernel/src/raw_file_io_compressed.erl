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
-module(raw_file_io_compressed).

-export([close/1, sync/1, datasync/1, truncate/1, advise/4, allocate/3,
         position/2, write/2, pwrite/2, pwrite/3,
         read_line/1, read/2, pread/2, pread/3]).

%% OTP internal.
-export([ipread_s32bu_p32bu/3, sendfile/8]).

-export([open_layer/3]).

-include("file_int.hrl").

open_layer(Filename, Modes, Options) ->
    IsAppend = lists:member(append, Modes),
    IsDeflate = lists:member(write, Modes),
    IsInflate = lists:member(read, Modes),
    if
        IsDeflate, IsInflate; IsAppend ->
            {error, einval};
        IsDeflate, not IsInflate ->
            start_server_module(raw_file_io_deflate, Filename, Modes, Options);
        IsInflate ->
            start_server_module(raw_file_io_inflate, Filename, Modes, Options)
    end.

start_server_module(Module, Filename, Modes, Options) ->
    Secret = make_ref(),
    case gen_statem:start(Module, {self(), Secret, Options}, []) of
        {ok, Pid} -> open_next_layer(Pid, Secret, Filename, Modes);
        Other -> Other
    end.

open_next_layer(Pid, Secret, Filename, Modes) ->
    case gen_statem:call(Pid, {'$open', Secret, Filename, Modes}, infinity) of
        ok ->
            PublicFd = #file_descriptor{
                module = raw_file_io_compressed, data = {self(), Pid} },
            {ok, PublicFd};
        Other -> Other
    end.

close(Fd) ->
    wrap_call(Fd, [close]).

sync(Fd) ->
    wrap_call(Fd, [sync]).
datasync(Fd) ->
    wrap_call(Fd, [datasync]).

truncate(Fd) ->
    wrap_call(Fd, [truncate]).

advise(Fd, Offset, Length, Advise) ->
    wrap_call(Fd, [advise, Offset, Length, Advise]).
allocate(Fd, Offset, Length) ->
    wrap_call(Fd, [allocate, Offset, Length]).

position(Fd, Mark) ->
    wrap_call(Fd, [position, Mark]).

write(Fd, IOData) ->
    try
        CompactedData = erlang:iolist_to_iovec(IOData),
        wrap_call(Fd, [write, CompactedData])
    catch
        error:badarg -> {error, badarg}
    end.

pwrite(Fd, Offset, IOData) ->
    try
        CompactedData = erlang:iolist_to_iovec(IOData),
        wrap_call(Fd, [pwrite, Offset, CompactedData])
    catch
        error:badarg -> {error, badarg}
    end.
pwrite(Fd, LocBytes) ->
    try
        CompactedLocBytes =
            [ {Offset, erlang:iolist_to_iovec(IOData)} ||
              {Offset, IOData} <- LocBytes ],
        wrap_call(Fd, [pwrite, CompactedLocBytes])
    catch
        error:badarg -> {error, badarg}
    end.

read_line(Fd) ->
    wrap_call(Fd, [read_line]).
read(Fd, Size) ->
    wrap_call(Fd, [read, Size]).
pread(Fd, Offset, Size) ->
    wrap_call(Fd, [pread, Offset, Size]).
pread(Fd, LocNums) ->
    wrap_call(Fd, [pread, LocNums]).

ipread_s32bu_p32bu(Fd, Offset, MaxSize) ->
    wrap_call(Fd, [ipread_s32bu_p32bu, Offset, MaxSize]).

sendfile(_,_,_,_,_,_,_,_) ->
    {error, enotsup}.

wrap_call(Fd, Command) ->
    {_Owner, Pid} = get_fd_data(Fd),
    try gen_statem:call(Pid, Command, infinity) of
        Result -> Result
    catch
        exit:{normal, _StackTrace} -> {error, einval};
        exit:{noproc, _StackTrace} -> {error, einval}
    end.

get_fd_data(#file_descriptor{ data = Data }) ->
    {Owner, _ServerPid} = Data,
    case self() of
        Owner -> Data;
        _ -> error(not_on_controlling_process)
    end.
