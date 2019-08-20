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
-module(raw_file_io_delayed).

-behavior(gen_statem).

-export([close/1, sync/1, datasync/1, truncate/1, advise/4, allocate/3,
         position/2, write/2, pwrite/2, pwrite/3,
         read_line/1, read/2, pread/2, pread/3]).

%% OTP internal.
-export([ipread_s32bu_p32bu/3, sendfile/8]).

-export([open_layer/3]).

-export([init/1, callback_mode/0, terminate/3]).
-export([opening/3, opened/3]).

-include("file_int.hrl").

open_layer(Filename, Modes, Options) ->
    Secret = make_ref(),
    case gen_statem:start(?MODULE, {self(), Secret, Options}, []) of
        {ok, Pid} ->
            gen_statem:call(Pid, {'$open', Secret, Filename, Modes}, infinity);
        Other ->
            Other
    end.

callback_mode() -> state_functions.

init({Owner, Secret, Options}) ->
    Monitor = monitor(process, Owner),
    Defaults =
        #{ owner => Owner,
           monitor => Monitor,
           secret => Secret,
           timer => none,
           pid => self(),
           buffer => prim_buffer:new(),
           delay_size => 64 bsl 10,
           delay_time => 2000 },
    Data = fill_delay_values(Defaults, Options),
    {ok, opening, Data}.

fill_delay_values(Data, []) ->
    Data;
fill_delay_values(Data, [{delayed_write, Size, Time} | Options]) ->
    fill_delay_values(Data#{ delay_size => Size, delay_time => Time }, Options);
fill_delay_values(Data, [_ | Options]) ->
    fill_delay_values(Data, Options).

opening({call, From}, {'$open', Secret, Filename, Modes}, #{ secret := Secret } = Data) ->
    case raw_file_io:open(Filename, Modes) of
        {ok, PrivateFd} ->
            PublicData = maps:with([owner, buffer, delay_size, pid], Data),
            PublicFd = #file_descriptor{ module = ?MODULE, data = PublicData },

            NewData = Data#{ handle => PrivateFd },
            Response = {ok, PublicFd},
            {next_state, opened, NewData, [{reply, From, Response}]};
        Other ->
            {stop_and_reply, normal, [{reply, From, Other}]}
    end;
opening(_Event, _Contents, _Data) ->
    {keep_state_and_data, [postpone]}.

%%

opened(info, {'$timed_out', Secret}, #{ secret := Secret } = Data) ->
    %% If the user writes something at this exact moment, the flush will fail
    %% and the timer won't reset on the next write since the buffer won't be
    %% empty (Unless we collided on a flush). We therefore reset the timeout to
    %% ensure that data won't sit idle for extended periods of time.
    case try_flush_write_buffer(Data) of
        busy -> gen_statem:cast(self(), '$reset_timeout');
        ok -> ok
    end,
    {keep_state, Data#{ timer => none }, []};

opened(info, {'DOWN', Monitor, process, _Owner, Reason}, #{ monitor := Monitor } = Data) ->
    if
        Reason =/= kill -> try_flush_write_buffer(Data);
        Reason =:= kill -> ignored
    end,
    {stop, shutdown};

opened(info, _Message, _Data) ->
    keep_state_and_data;

opened({call, {Owner, _Tag} = From}, [close], #{ owner := Owner } = Data) ->
    case flush_write_buffer(Data) of
        ok ->
            #{ handle := PrivateFd } = Data,
            Response = ?CALL_FD(PrivateFd, close, []),
            {stop_and_reply, normal, [{reply, From, Response}]};
        Other ->
            {stop_and_reply, normal, [{reply, From, Other}]}
    end;

opened({call, {Owner, _Tag} = From}, '$wait', #{ owner := Owner }) ->
    %% Used in write/2 to synchronize writes on lock conflicts.
    {keep_state_and_data, [{reply, From, ok}]};

opened({call, {Owner, _Tag} = From}, '$synchronous_flush', #{ owner := Owner } = Data) ->
    cancel_flush_timeout(Data),
    Response = flush_write_buffer(Data),
    {keep_state_and_data, [{reply, From, Response}]};

opened({call, {Owner, _Tag} = From}, Command, #{ owner := Owner } = Data) ->
    Response =
        case flush_write_buffer(Data) of
            ok -> dispatch_command(Data, Command);
            Other -> Other
        end,
    {keep_state_and_data, [{reply, From, Response}]};

opened({call, _From}, _Command, _Data) ->
    %% The client functions filter this out, so we'll crash if the user does
    %% anything stupid on purpose.
    {shutdown, protocol_violation};

opened(cast, '$reset_timeout', #{ delay_time := Timeout, secret := Secret } = Data) ->
    cancel_flush_timeout(Data),
    Timer = erlang:send_after(Timeout, self(), {'$timed_out', Secret}),
    {keep_state, Data#{ timer => Timer }, []};

opened(cast, _Message, _Data) ->
    {keep_state_and_data, []}.

dispatch_command(Data, [Function | Args]) ->
    #{ handle := Handle } = Data,
    Module = Handle#file_descriptor.module,
    apply(Module, Function, [Handle | Args]).

cancel_flush_timeout(#{ timer := none }) ->
    ok;
cancel_flush_timeout(#{ timer := Timer }) ->
    _ = erlang:cancel_timer(Timer, [{async, true}]),
    ok.

try_flush_write_buffer(#{ buffer := Buffer, handle := PrivateFd }) ->
    case prim_buffer:try_lock(Buffer) of
        acquired ->
            flush_write_buffer_1(Buffer, PrivateFd),
            prim_buffer:unlock(Buffer),
            ok;
        busy ->
            busy
    end.

%% This is only safe to use when there is no chance of conflict with the owner
%% process, or in other words, "during synchronous calls outside of the locked
%% section of write/2"
flush_write_buffer(#{ buffer := Buffer, handle := PrivateFd }) ->
    acquired = prim_buffer:try_lock(Buffer),
    Result = flush_write_buffer_1(Buffer, PrivateFd),
    prim_buffer:unlock(Buffer),
    Result.

flush_write_buffer_1(Buffer, PrivateFd) ->
    case prim_buffer:size(Buffer) of
        Size when Size > 0 ->
            ?CALL_FD(PrivateFd, write, [prim_buffer:read_iovec(Buffer, Size)]);
        0 ->
            ok
    end.

terminate(_Reason, _State, _Data) ->
    ok.

%% Client functions

write(Fd, IOData) ->
    try
        enqueue_write(Fd, erlang:iolist_to_iovec(IOData))
    catch
        error:badarg -> {error, badarg}
    end.
enqueue_write(_Fd, []) ->
    ok;
enqueue_write(Fd, IOVec) ->
    %% get_fd_data will reject everyone except the process that opened the Fd,
    %% so we can't race with anyone except the wrapper process.
    #{ delay_size := DelaySize,
       buffer := Buffer,
       pid := Pid } = get_fd_data(Fd),
    case prim_buffer:try_lock(Buffer) of
        acquired ->
            %% (The wrapper process will exit without flushing if we're killed
            %% while holding the lock).
            enqueue_write_locked(Pid, Buffer, DelaySize, IOVec);
        busy ->
            %% This can only happen while we're processing a timeout in the
            %% wrapper process, so we perform a bogus call to get a completion
            %% notification before trying again.
            gen_statem:call(Pid, '$wait'),
            enqueue_write(Fd, IOVec)
    end.
enqueue_write_locked(Pid, Buffer, DelaySize, IOVec) ->
    %% The synchronous operations (write, forced flush) are safe since we're
    %% running on the only process that can fill the buffer; a timeout being
    %% processed just before $synchronous_flush will cause the flush to nop,
    %% and a timeout sneaking in just before a synchronous write won't do
    %% anything since the buffer is guaranteed to be empty at that point.
    BufSize = prim_buffer:size(Buffer),
    case is_iovec_smaller_than(IOVec, DelaySize - BufSize) of
        true when BufSize > 0 ->
            prim_buffer:write(Buffer, IOVec),
            prim_buffer:unlock(Buffer);
        true ->
            prim_buffer:write(Buffer, IOVec),
            prim_buffer:unlock(Buffer),
            gen_statem:cast(Pid, '$reset_timeout');
        false when BufSize > 0 ->
            prim_buffer:write(Buffer, IOVec),
            prim_buffer:unlock(Buffer),
            gen_statem:call(Pid, '$synchronous_flush');
        false ->
            prim_buffer:unlock(Buffer),
            gen_statem:call(Pid, [write, IOVec])
    end.

%% iolist_size/1 will always look through the entire list to get a precise
%% amount, which is pretty inefficient since we only need to know whether we've
%% hit the buffer threshold or not.
%%
%% We only handle the binary case since write/2 forcibly translates input to
%% erlang:iovec().
is_iovec_smaller_than(IOVec, Max) ->
    is_iovec_smaller_than_1(IOVec, Max, 0).
is_iovec_smaller_than_1(_IOVec, Max, Acc) when Acc >= Max ->
    false;
is_iovec_smaller_than_1([], _Max, _Acc) ->
    true;
is_iovec_smaller_than_1([Binary | Rest], Max, Acc) when is_binary(Binary) ->
    is_iovec_smaller_than_1(Rest, Max, Acc + byte_size(Binary)).

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
    #{ pid := Pid } = get_fd_data(Fd),
    try gen_statem:call(Pid, Command, infinity) of
        Result -> Result
    catch
        exit:{normal, _StackTrace} -> {error, einval};
        exit:{noproc, _StackTrace} -> {error, einval}
    end.

get_fd_data(#file_descriptor{ data = Data }) ->
    #{ owner := Owner } = Data,
    case self() of
        Owner -> Data;
        _ -> error(not_on_controlling_process)
    end.
