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
-module(raw_file_io_deflate).

-behavior(gen_statem).

-export([init/1, callback_mode/0, terminate/3]).
-export([opening/3, opened/3]).

-include("file_int.hrl").

-define(GZIP_WBITS, 16 + 15).

callback_mode() -> state_functions.

init({Owner, Secret, [compressed]}) ->
    Monitor = monitor(process, Owner),
    Z = zlib:open(),
    ok = zlib:deflateInit(Z, default, deflated, ?GZIP_WBITS, 8, default),
    Data =
        #{ owner => Owner,
           monitor => Monitor,
           secret => Secret,
           position => 0,
           zlib => Z },
    {ok, opening, Data}.

opening({call, From}, {'$open', Secret, Filename, Modes}, #{ secret := Secret } = Data) ->
    case raw_file_io:open(Filename, Modes) of
        {ok, PrivateFd} ->
            NewData = Data#{ handle => PrivateFd },
            {next_state, opened, NewData, [{reply, From, ok}]};
        Other ->
            {stop_and_reply, normal, [{reply, From, Other}]}
    end;
opening(_Event, _Contents, _Data) ->
    {keep_state_and_data, [postpone]}.

%%

opened(info, {'DOWN', Monitor, process, _Owner, Reason}, #{ monitor := Monitor } = Data) ->
    if
        Reason =/= kill -> flush_deflate_state(Data);
        Reason =:= kill -> ignored
    end,
    {stop, shutdown};

opened(info, _Message, _Data) ->
    keep_state_and_data;

opened({call, {Owner, _Tag} = From}, [close], #{ owner := Owner } = Data) ->
    #{ handle := PrivateFd } = Data,
    Response =
        case flush_deflate_state(Data) of
            ok -> ?CALL_FD(PrivateFd, close, []);
            Other -> Other
        end,
    {stop_and_reply, normal, [{reply, From, Response}]};

opened({call, {Owner, _Tag} = From}, [position, Mark], #{ owner := Owner } = Data) ->
    case position(Data, Mark) of
        {ok, NewData, Result} ->
            Response = {ok, Result},
            {keep_state, NewData, [{reply, From, Response}]};
        Other ->
            {keep_state_and_data, [{reply, From, Other}]}
    end;

opened({call, {Owner, _Tag} = From}, [write, IOVec], #{ owner := Owner } = Data) ->
    case write(Data, IOVec) of
        {ok, NewData} -> {keep_state, NewData, [{reply, From, ok}]};
        Other -> {keep_state_and_data, [{reply, From, Other}]}
    end;

opened({call, {Owner, _Tag} = From}, [read, _Size], #{ owner := Owner }) ->
    Response = {error, ebadf},
    {keep_state_and_data, [{reply, From, Response}]};

opened({call, {Owner, _Tag} = From}, [read_line], #{ owner := Owner }) ->
    Response = {error, ebadf},
    {keep_state_and_data, [{reply, From, Response}]};

opened({call, {Owner, _Tag} = From}, _Command, #{ owner := Owner }) ->
    Response = {error, enotsup},
    {keep_state_and_data, [{reply, From, Response}]};

opened({call, _From}, _Command, _Data) ->
    %% The client functions filter this out, so we'll crash if the user does
    %% anything stupid on purpose.
    {shutdown, protocol_violation};

opened(_Event, _Request, _Data) ->
    keep_state_and_data.

write(Data, IOVec) ->
    #{ handle := PrivateFd, position := Position, zlib := Z } = Data,
    UncompressedSize = iolist_size(IOVec),
    case ?CALL_FD(PrivateFd, write, [zlib:deflate(Z, IOVec)]) of
        ok -> {ok, Data#{ position := (Position + UncompressedSize) }};
        Other -> Other
    end.

%%
%% We support "seeking" forward as long as it isn't relative to EOF.
%%
%% Seeking is a bit of a misnomer as it's really just compressing zeroes until
%% we reach the desired point, but it has always behaved like this.
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
position(_Data, _Any) ->
    {error, badarg}.

position_1(#{ position := Desired } = Data, Desired) ->
    {ok, Data, Desired};
position_1(#{ position := Current } = Data, Desired) when Current < Desired ->
    BytesToWrite = min(Desired - Current, 4 bsl 20),
    case write(Data, <<0:(BytesToWrite)/unit:8>>) of
        {ok, NewData} -> position_1(NewData, Desired);
        Other -> Other
    end;
position_1(#{ position := Current }, Desired) when Current > Desired ->
    {error, einval}.

flush_deflate_state(#{ handle := PrivateFd, zlib := Z }) ->
    case ?CALL_FD(PrivateFd, write, [zlib:deflate(Z, [], finish)]) of
        ok -> ok;
        Other -> Other
    end.

terminate(_Reason, _State, _Data) ->
    ok.
