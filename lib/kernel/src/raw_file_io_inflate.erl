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
-module(raw_file_io_inflate).

-behavior(gen_statem).

-export([init/1, callback_mode/0, terminate/3]).
-export([opening/3, opened_gzip/3, opened_passthrough/3]).

-include("file_int.hrl").

-define(INFLATE_CHUNK_SIZE, (8 bsl 10)).
-define(GZIP_WBITS, (16 + 15)).

callback_mode() -> state_functions.

init({Owner, Secret, [compressed]}) ->
    Monitor = monitor(process, Owner),
    %% We're using the undocumented inflateInit/3 to open the stream in
    %% 'reset mode', which resets the inflate state at the end of every stream,
    %% allowing us to read concatenated gzip files.
    Z = zlib:open(),
    ok = zlib:inflateInit(Z, ?GZIP_WBITS, reset),
    Data =
        #{ owner => Owner,
           monitor => Monitor,
           secret => Secret,
           position => 0,
           buffer => prim_buffer:new(),
           zlib => Z },
    {ok, opening, Data}.

%% The old driver fell back to plain reads if the file didn't start with the
%% magic gzip bytes.
choose_decompression_state(PrivateFd) ->
    State =
        case ?CALL_FD(PrivateFd, read, [2]) of
            {ok, <<16#1F, 16#8B>>} -> opened_gzip;
            _Other -> opened_passthrough
        end,
    {ok, 0} = ?CALL_FD(PrivateFd, position, [0]),
    State.

opening({call, From}, {'$open', Secret, Filename, Modes}, #{ secret := Secret } = Data) ->
    case raw_file_io:open(Filename, Modes) of
        {ok, PrivateFd} ->
            NextState = choose_decompression_state(PrivateFd),
            NewData = Data#{ handle => PrivateFd },
            {next_state, NextState, NewData, [{reply, From, ok}]};
        Other ->
            {stop_and_reply, normal, [{reply, From, Other}]}
    end;
opening(_Event, _Contents, _Data) ->
    {keep_state_and_data, [postpone]}.

internal_close(From, Data) ->
    #{ handle := PrivateFd } = Data,
    Response = ?CALL_FD(PrivateFd, close, []),
    {stop_and_reply, normal, [{reply, From, Response}]}.

opened_passthrough(info, {'DOWN', Monitor, process, _Owner, _Reason}, #{ monitor := Monitor }) ->
    {stop, shutdown};

opened_passthrough(info, _Message, _Data) ->
    keep_state_and_data;

opened_passthrough({call, {Owner, _Tag} = From}, [close], #{ owner := Owner } = Data) ->
    internal_close(From, Data);

opened_passthrough({call, {Owner, _Tag} = From}, [Method | Args], #{ owner := Owner } = Data) ->
    #{ handle := PrivateFd } = Data,
    Response = ?CALL_FD(PrivateFd, Method, Args),
    {keep_state_and_data, [{reply, From, Response}]};

opened_passthrough({call, _From}, _Command, _Data) ->
    %% The client functions filter this out, so we'll crash if the user does
    %% anything stupid on purpose.
    {shutdown, protocol_violation};

opened_passthrough(_Event, _Request, _Data) ->
    keep_state_and_data.

%%

opened_gzip(info, {'DOWN', Monitor, process, _Owner, _Reason}, #{ monitor := Monitor }) ->
    {stop, shutdown};

opened_gzip(info, _Message, _Data) ->
    keep_state_and_data;

opened_gzip({call, {Owner, _Tag} = From}, [close], #{ owner := Owner } = Data) ->
    internal_close(From, Data);

opened_gzip({call, {Owner, _Tag} = From}, [position, Mark], #{ owner := Owner } = Data) ->
    case position(Data, Mark) of
        {ok, NewData, Result} ->
            Response = {ok, Result},
            {keep_state, NewData, [{reply, From, Response}]};
        Other ->
            {keep_state_and_data, [{reply, From, Other}]}
    end;

opened_gzip({call, {Owner, _Tag} = From}, [read, Size], #{ owner := Owner } = Data) ->
    case read(Data, Size) of
        {ok, NewData, Result} ->
            Response = {ok, Result},
            {keep_state, NewData, [{reply, From, Response}]};
        Other ->
            {keep_state_and_data, [{reply, From, Other}]}
    end;

opened_gzip({call, {Owner, _Tag} = From}, [read_line], #{ owner := Owner } = Data) ->
    case read_line(Data) of
        {ok, NewData, Result} ->
            Response = {ok, Result},
            {keep_state, NewData, [{reply, From, Response}]};
        Other ->
            {keep_state_and_data, [{reply, From, Other}]}
    end;

opened_gzip({call, {Owner, _Tag} = From}, [write, _IOData], #{ owner := Owner }) ->
    Response = {error, ebadf},
    {keep_state_and_data, [{reply, From, Response}]};

opened_gzip({call, {Owner, _Tag} = From}, _Request, #{ owner := Owner }) ->
    Response = {error, enotsup},
    {keep_state_and_data, [{reply, From, Response}]};

opened_gzip({call, _From}, _Request, _Data) ->
    %% The client functions filter this out, so we'll crash if the user does
    %% anything stupid on purpose.
    {shutdown, protocol_violation};

opened_gzip(_Event, _Request, _Data) ->
    keep_state_and_data.

%%

read(#{ buffer := Buffer } = Data, Size) ->
    try read_1(Data, Buffer, prim_buffer:size(Buffer), Size) of
        Result -> Result
    catch
        error:badarg -> {error, badarg};
        error:_ -> {error, eio}
    end.
read_1(Data, Buffer, BufferSize, ReadSize) when BufferSize >= ReadSize ->
    #{ position := Position } = Data,
    Decompressed = prim_buffer:read(Buffer, ReadSize),
    {ok, Data#{ position => (Position + ReadSize) }, Decompressed};
read_1(Data, Buffer, BufferSize, ReadSize) when BufferSize < ReadSize ->
    #{ handle := PrivateFd } = Data,
    case ?CALL_FD(PrivateFd, read, [?INFLATE_CHUNK_SIZE]) of
        {ok, Compressed} ->
            #{ zlib := Z } = Data,
            Uncompressed = erlang:iolist_to_iovec(zlib:inflate(Z, Compressed)),
            prim_buffer:write(Buffer, Uncompressed),
            read_1(Data, Buffer, prim_buffer:size(Buffer), ReadSize);
        eof when BufferSize > 0 ->
            read_1(Data, Buffer, BufferSize, BufferSize);
        Other ->
            Other
    end.

read_line(#{ buffer := Buffer } = Data) ->
    try read_line_1(Data, Buffer, prim_buffer:find_byte_index(Buffer, $\n)) of
        {ok, NewData, Decompressed} -> {ok, NewData, Decompressed};
        Other -> Other
    catch
        error:badarg -> {error, badarg};
        error:_ -> {error, eio}
    end.

read_line_1(Data, Buffer, not_found) ->
    #{ handle := PrivateFd, zlib := Z } = Data,
    case ?CALL_FD(PrivateFd, read, [?INFLATE_CHUNK_SIZE]) of
        {ok, Compressed} ->
            Uncompressed = erlang:iolist_to_iovec(zlib:inflate(Z, Compressed)),
            prim_buffer:write(Buffer, Uncompressed),
            read_line_1(Data, Buffer, prim_buffer:find_byte_index(Buffer, $\n));
        eof ->
            case prim_buffer:size(Buffer) of
                Size when Size > 0 -> {ok, prim_buffer:read(Buffer, Size)};
                Size when Size =:= 0 -> eof
            end;
        Error ->
            Error
    end;
read_line_1(Data, Buffer, {ok, LFIndex}) ->
    %% Translate CRLF into just LF, completely ignoring which encoding is used,
    %% but treat the file position as including CR.
    #{ position := Position } = Data,
    NewData = Data#{ position => (Position + LFIndex + 1) },
    CRIndex = (LFIndex - 1),
    TranslatedLine =
        case prim_buffer:read(Buffer, LFIndex + 1) of
            <<Line:CRIndex/binary, "\r\n">> -> <<Line/binary, "\n">>;
            Line -> Line
        end,
    {ok, NewData, TranslatedLine}.

%%
%% We support seeking in both directions as long as it isn't relative to EOF.
%%
%% Seeking backwards is extremely inefficient since we have to seek to the very
%% beginning and then decompress up to the desired point.
%%

position(Data, Mark) when is_atom(Mark) ->
    position(Data, {Mark, 0});
position(Data, Offset) when is_integer(Offset) ->
    position(Data, {bof, Offset});
position(Data, {bof, Offset}) when is_integer(Offset) ->
    position_1(Data, Offset);
position(Data, {cur, Offset}) when is_integer(Offset) ->
    #{ position := Position } = Data,
    position_1(Data, Position + Offset);
position(_Data, {eof, Offset}) when is_integer(Offset) ->
    {error, einval};
position(_Data, _Other) ->
    {error, badarg}.

position_1(_Data, Desired) when Desired < 0 ->
    {error, einval};
position_1(#{ position := Desired } = Data, Desired) ->
    {ok, Data, Desired};
position_1(#{ position := Current } = Data, Desired) when Current < Desired ->
    case read(Data, min(Desired - Current, ?INFLATE_CHUNK_SIZE)) of
        {ok, NewData, _Data} -> position_1(NewData, Desired);
        eof -> {ok, Data, Current};
        Other -> Other
    end;
position_1(#{ position := Current } = Data, Desired) when Current > Desired ->
    #{ handle := PrivateFd, buffer := Buffer, zlib := Z } = Data,
    case ?CALL_FD(PrivateFd, position, [bof]) of
        {ok, 0} ->
            ok = zlib:inflateReset(Z),
            prim_buffer:wipe(Buffer),
            position_1(Data#{ position => 0 }, Desired);
        Other ->
            Other
    end.

terminate(_Reason, _State, _Data) ->
    ok.
