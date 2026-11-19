%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2026 Zaiming Shi <zmstone@gmail.com>
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
%%
%%----------------------------------------------------------------------
%% Purpose: Socket options that more than one process must read without
%% a message round trip: the connection process writes them on setopts,
%% ssl:send/2 reads the packet header length in the caller's process to encode
%% the packet header, and the sender process reads the watermarks when it
%% sets up asynchronous sending.
%%
%% They are kept in an atomics array, one per connection. The array is a
%% lock-free, process-shared, refcounted ERTS resource of a few words; it
%% costs less than a hundred bytes and lives as long as the #sslsocket{}
%% records that refer to it.
%%
%% Note that the array outlives the connection process. A reader cannot
%% use it to detect a closed socket; the message to the sender process
%% fails with noproc instead, which tls_sender:call/2 turns into
%% {error, closed}.
%%----------------------------------------------------------------------

-module(ssl_shared_opts).
-moduledoc false.

-export([new/1,
         set_packet/2,
         get_send_header/1,
         is_valid_watermark/1,
         set_high_watermark/2,
         get_high_watermark/1,
         set_low_watermark/2,
         get_low_watermark/1]).

-export_type([t/0]).

-opaque t() :: atomics:atomics_ref().

-define(SEND_HEADER, 1).
-define(HIGH_WATERMARK, 2).
-define(LOW_WATERMARK, 3).

-spec new(list()) -> t().
new(EmOpts) ->
    Ref = atomics:new(3, [{signed, true}]),
    lists:foreach(
      fun({packet, Packet}) -> set_packet(Ref, Packet);
         ({high_watermark, Size}) -> set_high_watermark(Ref, Size);
         ({low_watermark, Size}) -> set_low_watermark(Ref, Size);
         (_) -> ok
      end, EmOpts),
    Ref.

%% Only length-prefixed packets need a header when sending. The full
%% packet type remains in the connection process for decoding and getopts.
-spec set_packet(t(), atom() | integer()) -> ok.
set_packet(Ref, Packet) ->
    atomics:put(Ref, ?SEND_HEADER, send_header(Packet)).

-spec get_send_header(t()) -> 0 | 1 | 2 | 4.
get_send_header(Ref) ->
    atomics:get(Ref, ?SEND_HEADER).

-spec is_valid_watermark(term()) -> boolean().
is_valid_watermark(Size) ->
    is_integer(Size) andalso Size >= -(1 bsl 63) andalso Size < (1 bsl 63).

-spec set_high_watermark(t(), integer()) -> ok.
set_high_watermark(Ref, Size) ->
    atomics:put(Ref, ?HIGH_WATERMARK, Size).

-spec get_high_watermark(t()) -> integer().
get_high_watermark(Ref) ->
    atomics:get(Ref, ?HIGH_WATERMARK).

-spec set_low_watermark(t(), integer()) -> ok.
set_low_watermark(Ref, Size) ->
    atomics:put(Ref, ?LOW_WATERMARK, Size).

-spec get_low_watermark(t()) -> integer().
get_low_watermark(Ref) ->
    atomics:get(Ref, ?LOW_WATERMARK).

send_header(1) -> 1;
send_header(2) -> 2;
send_header(4) -> 4;
send_header(_) -> 0.
