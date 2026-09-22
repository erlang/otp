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
%% ssl:send/2 reads the packet type in the caller's process to encode
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

-export([new/0,
         set_packet/2,
         get_packet/1,
         set_high_watermark/2,
         get_high_watermark/2,
         set_low_watermark/2,
         get_low_watermark/2]).

-export_type([t/0]).

-opaque t() :: atomics:atomics_ref().

-define(PACKET, 1).
-define(HIGH_WATERMARK, 2).
-define(LOW_WATERMARK, 3).

%% Watermarks are stored as Value + 1 so that 0 can mean "not set":
%% 0 is a legal watermark.
-define(UNSET, 0).

-spec new() -> t().
new() ->
    atomics:new(3, [{signed, false}]).

%% The packet type is the closed set inet accepts. raw and 0 are the
%% same type and share one code.
-spec set_packet(t(), atom() | 0 | 1 | 2 | 4) -> ok.
set_packet(Ref, Packet) ->
    atomics:put(Ref, ?PACKET, packet_to_int(Packet)).

-spec get_packet(t()) -> atom() | 0 | 1 | 2 | 4.
get_packet(Ref) ->
    int_to_packet(atomics:get(Ref, ?PACKET)).

-spec set_high_watermark(t(), non_neg_integer()) -> ok.
set_high_watermark(Ref, Size) when is_integer(Size), Size >= 0 ->
    atomics:put(Ref, ?HIGH_WATERMARK, Size + 1).

%% Default is returned when no watermark has been set.
-spec get_high_watermark(t(), non_neg_integer()) -> non_neg_integer().
get_high_watermark(Ref, Default) ->
    watermark(atomics:get(Ref, ?HIGH_WATERMARK), Default).

-spec set_low_watermark(t(), non_neg_integer()) -> ok.
set_low_watermark(Ref, Size) when is_integer(Size), Size >= 0 ->
    atomics:put(Ref, ?LOW_WATERMARK, Size + 1).

-spec get_low_watermark(t(), non_neg_integer()) -> non_neg_integer().
get_low_watermark(Ref, Default) ->
    watermark(atomics:get(Ref, ?LOW_WATERMARK), Default).

watermark(?UNSET, Default) -> Default;
watermark(Stored, _Default) -> Stored - 1.

packet_to_int(raw) -> 0;
packet_to_int(0) -> 0;
packet_to_int(1) -> 1;
packet_to_int(2) -> 2;
packet_to_int(4) -> 4;
packet_to_int(asn1) -> 5;
packet_to_int(cdr) -> 6;
packet_to_int(sunrm) -> 7;
packet_to_int(fcgi) -> 8;
packet_to_int(tpkt) -> 9;
packet_to_int(line) -> 10;
packet_to_int(http) -> 11;
packet_to_int(httph) -> 12;
packet_to_int(http_bin) -> 13;
packet_to_int(httph_bin) -> 14.

int_to_packet(0) -> 0;
int_to_packet(1) -> 1;
int_to_packet(2) -> 2;
int_to_packet(4) -> 4;
int_to_packet(5) -> asn1;
int_to_packet(6) -> cdr;
int_to_packet(7) -> sunrm;
int_to_packet(8) -> fcgi;
int_to_packet(9) -> tpkt;
int_to_packet(10) -> line;
int_to_packet(11) -> http;
int_to_packet(12) -> httph;
int_to_packet(13) -> http_bin;
int_to_packet(14) -> httph_bin.
