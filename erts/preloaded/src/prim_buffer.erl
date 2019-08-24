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
-module(prim_buffer).

-export([on_load/0]).

%% This is a mutable binary buffer that helps break out buffering logic from
%% NIFs/drivers, which is often the only thing that prevents the C code from
%% being reduced to bare system call wrappers.
%%
%% All operations in this file are thread-unsafe and risk crashing the emulator
%% if you're not careful.

-export([new/0, size/1, wipe/1, read/2, read_iovec/2, write/2, skip/2]).

-export([find_byte_index/2]).

-export([try_lock/1, unlock/1]).

-type prim_buffer() :: term().

%% Controls when to copy rather than extract sub-binaries from the buffer,
%% reducing the risk of small reads keeping a large binary alive.
-define(COPYING_READ_LIMIT, 512).

%% Reads that fit into heap binaries are always copied since the cost of
%% peeking binaries that short is largely equivalent to copying.
-define(ERL_ONHEAP_BIN_LIMIT, 64).

on_load() ->
    case erlang:load_nif(atom_to_list(?MODULE), 0) of
        ok -> ok
    end.

-spec new() -> prim_buffer().
new() ->
    erlang:nif_error(undef).

-spec size(Buffer :: prim_buffer()) -> non_neg_integer().
size(_Buffer) ->
    erlang:nif_error(undef).

%% Reads data as a binary from the front of the buffer. This will almost always
%% result in copying so it should be avoided unless you absolutely must have a
%% binary.
-spec read(Buffer :: prim_buffer(), Size :: non_neg_integer()) -> binary().
read(Buffer, Size) when Size =< ?ERL_ONHEAP_BIN_LIMIT ->
    copying_read(Buffer, Size);
read(Buffer, Size) when Size > ?ERL_ONHEAP_BIN_LIMIT ->
    iolist_to_binary(read_iovec(Buffer, Size)).

%% Reads data as an erlang:iovec() binary from the front of the buffer,
%% avoiding copying if reasonable.
-spec read_iovec(Buffer, Size) -> IOVec when
      Buffer :: prim_buffer(),
      Size :: non_neg_integer(),
      IOVec :: erlang:iovec().
read_iovec(Buffer, Size) when Size =< ?ERL_ONHEAP_BIN_LIMIT ->
    [copying_read(Buffer, Size)];
read_iovec(Buffer, Size) when Size > ?ERL_ONHEAP_BIN_LIMIT ->
    Head = peek_head(Buffer),
    HeadSize = byte_size(Head),
    if
        (HeadSize - Size) > ?COPYING_READ_LIMIT, Size =< ?COPYING_READ_LIMIT ->
            [copying_read(Buffer, Size)];
        HeadSize > Size ->
            skip(Buffer, Size),
            {First, _Rest} = split_binary(Head, Size),
            [First];
        HeadSize < Size ->
            skip(Buffer, HeadSize),
            [Head | read_iovec(Buffer, Size - HeadSize)];
        HeadSize =:= Size ->
            skip(Buffer, Size),
            [Head]
    end.

%% Writes an erlang:iovec() to the back of the buffer.
-spec write(Buffer :: prim_buffer(), IOVec :: erlang:iovec()) -> ok.
write(_Buffer, _IOVec) ->
    erlang:nif_error(undef).

%% Removes data from the front of the buffer without reading it.
-spec skip(Buffer :: prim_buffer(), Size :: non_neg_integer()) -> ok.
skip(_Buffer, _Size) ->
    erlang:nif_error(undef).

-spec wipe(Buffer :: prim_buffer()) -> ok.
wipe(Buffer) ->
    skip(Buffer, prim_buffer:size(Buffer)).

%% Finds the start-index of the first occurence of Needle, for implementing
%% read_line and similar.
-spec find_byte_index(Buffer, Needle) -> Result when
      Buffer :: prim_buffer(),
      Needle :: non_neg_integer(),
      Result :: {ok, non_neg_integer()} |
                not_found.
find_byte_index(_Buffer, _Needle) ->
    erlang:nif_error(undef).

%% Attempts to take a unique lock on the buffer. Failure handling is left to
%% the user.
-spec try_lock(Buffer :: prim_buffer()) -> acquired | busy.
try_lock(_Buffer) ->
    erlang:nif_error(undef).

-spec unlock(Buffer :: prim_buffer()) -> ok.
unlock(_Buffer) ->
    erlang:nif_error(undef).

%% Unexported helper functions:

%% Reads data from the front of the buffer, returning a copy of the data to
%% avoid holding references.
-spec copying_read(Buffer :: prim_buffer(), Size :: non_neg_integer()) -> binary().
copying_read(_Buffer, _Size) ->
    erlang:nif_error(undef).

%% Returns the binary at the front of the buffer without modifying the buffer.
-spec peek_head(Buffer :: prim_buffer()) -> binary().
peek_head(_Buffer) ->
    erlang:nif_error(undef).
