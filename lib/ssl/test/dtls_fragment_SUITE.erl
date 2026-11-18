%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
%% Purpose: Unit tests for DTLS handshake fragment reassembly in
%% dtls_handshake:get_dtls_handshake/4. This exercises the pure
%% fragment-buffering/merging logic directly, without a live handshake,
%% so edge cases (reordering, duplication, overlap, gaps between
%% message_seq, retransmission) can be tested deterministically.
%%
%% Messages use handshake type 20 (finished) because its body may be an
%% arbitrary binary, which keeps assembly assertions simple: the emitted
%% message body must equal exactly the bytes fed for that message_seq.
%%
-module(dtls_fragment_SUITE).

-behaviour(ct_suite).

%% Common Test callbacks
-export([all/0,
         suite/0,
         groups/0]).

%% Test cases
-export([single_complete/1,
         single_fragmented_in_order/1,
         fragments_out_of_order_within_message/1,
         duplicate_fragment/1,
         overlapping_fragments/1,
         multiple_messages_one_record/1,
         later_message_buffered_then_assembled/1,
         gap_does_not_merge_into_next/1,
         cross_message_seq_no_leak/1,
         retransmitted_complete_message/1,
         old_message_seq_discarded/1,
         malformed_fragment/1]).

-define(FINISHED, 20).
-define(CLIENT_HELLO_TYPE, 1).
-define(DTLS_1_2, {254, 253}).

%%--------------------------------------------------------------------
%% Common Test API
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [single_complete,
     single_fragmented_in_order,
     fragments_out_of_order_within_message,
     duplicate_fragment,
     overlapping_fragments,
     multiple_messages_one_record,
     later_message_buffered_then_assembled,
     gap_does_not_merge_into_next,
     cross_message_seq_no_leak,
     retransmitted_complete_message,
     old_message_seq_discarded,
     malformed_fragment].

groups() ->
    [].

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

single_complete(_Config) ->
    %% A single, unfragmented message assembles immediately.
    B0 = buffers(),
    {Msgs, _B1} = feed(frag(0, 3, 0, <<"abc">>), B0),
    [{0, <<"abc">>}] = bodies(Msgs, [0]).

single_fragmented_in_order(_Config) ->
    %% One message delivered as in-order fragments.
    B0 = buffers(),
    {M0, B1} = feed(frag(0, 4, 0, <<"ab">>), B0),
    [] = bodies(M0, []),               %% not complete yet
    {M1, _B2} = feed(frag(0, 4, 2, <<"cd">>), B1),
    [{0, <<"abcd">>}] = bodies(M1, [0]).

fragments_out_of_order_within_message(_Config) ->
    %% Fragments of the SAME message arriving out of order still assemble
    %% to the correct body ordered by fragment_offset.
    B0 = buffers(),
    {M0, B1} = feed(frag(0, 4, 2, <<"cd">>), B0),
    [] = bodies(M0, []),
    {M1, _B2} = feed(frag(0, 4, 0, <<"ab">>), B1),
    [{0, <<"abcd">>}] = bodies(M1, [0]).

duplicate_fragment(_Config) ->
    %% A duplicated fragment must not corrupt or double-count data.
    B0 = buffers(),
    {M0, B1} = feed(frag(0, 4, 0, <<"ab">>), B0),
    [] = bodies(M0, []),
    {M1, B2} = feed(frag(0, 4, 0, <<"ab">>), B1),   %% exact duplicate
    [] = bodies(M1, []),
    {M2, _B3} = feed(frag(0, 4, 2, <<"cd">>), B2),
    [{0, <<"abcd">>}] = bodies(M2, [0]).

overlapping_fragments(_Config) ->
    %% Overlapping fragments (same message) must assemble consistently.
    %% offset 0 len 3 "abc", then offset 2 len 2 "cd" overlapping at 2.
    B0 = buffers(),
    {M0, B1} = feed(frag(0, 4, 0, <<"abc">>), B0),
    [] = bodies(M0, []),
    {M1, _B2} = feed(frag(0, 4, 2, <<"cd">>), B1),
    [{0, <<"abcd">>}] = bodies(M1, [0]).

multiple_messages_one_record(_Config) ->
    %% Several complete messages concatenated in one record are all
    %% returned in order.
    B0 = buffers(),
    Record = <<(frag(0, 1, 0, <<"x">>))/binary,
               (frag(1, 1, 0, <<"y">>))/binary,
               (frag(2, 1, 0, <<"z">>))/binary>>,
    {Msgs, _B1} = feed(Record, B0),
    [{0, <<"x">>}, {1, <<"y">>}, {2, <<"z">>}] = bodies(Msgs, [0,1,2]).

later_message_buffered_then_assembled(_Config) ->
    %% A fully-buffered later message is assembled correctly once its own
    %% turn comes (normal out-of-order delivery).
    %%
    %% NOTE on a pre-existing behavioural subtlety (NOT the cross-seq bug):
    %% when seq0 completes, a complete seq1 already sitting in the buffer is
    %% promoted to the next-fragments buffer but is not *eagerly* emitted in
    %% the same call. It is emitted when a subsequent fragment of seq1
    %% (here a retransmit) re-triggers reassembly. This test pins the
    %% current behaviour; it is documented here so a future eager-emit
    %% improvement updates the expectation deliberately.
    B0 = buffers(),
    {M0, B1} = feed(frag(1, 2, 0, <<"YY">>), B0),   %% seq1 complete, buffered
    [] = bodies(M0, []),
    {M1, B2} = feed(frag(0, 2, 0, <<"xx">>), B1),   %% seq0 completes...
    [{0, <<"xx">>}] = bodies(M1, [0]),              %% ...only seq0 emitted now
    {M2, _B3} = feed(frag(1, 2, 0, <<"YY">>), B2),  %% re-trigger seq1
    [{1, <<"YY">>}] = bodies(M2, [1]).

gap_does_not_merge_into_next(_Config) ->
    %% REGRESSION: a buffered fragment of message_seq 2 must not be merged
    %% into message_seq 1 when seq0 completes and next_seq advances to 1.
    %% (Previously emitted seq1 body "abXY" — leaking seq2 bytes.)
    B0 = buffers(),
    {M0, B1} = feed(frag(2, 4, 2, <<"XY">>), B0),   %% seq2 partial, buffered
    [] = bodies(M0, []),
    {M1, B2} = feed(frag(0, 1, 0, <<"x">>), B1),    %% seq0 completes
    [{0, <<"x">>}] = bodies(M1, [0]),
    {M2, _B3} = feed(frag(1, 4, 0, <<"ab">>), B2),  %% seq1 first half
    %% MUST NOT complete as "abXY": seq1 is still missing its own rest.
    [] = bodies(M2, []).

cross_message_seq_no_leak(_Config) ->
    %% Full version of the regression: after the gap scenario, feeding the
    %% real rest of seq1 and seq2 must yield each message's OWN bytes.
    B0 = buffers(),
    {_, B1} = feed(frag(2, 4, 2, <<"XY">>), B0),
    {_, B2} = feed(frag(0, 1, 0, <<"x">>), B1),
    {_, B3} = feed(frag(1, 4, 0, <<"ab">>), B2),
    {M3, B4} = feed(frag(1, 4, 2, <<"cd">>), B3),   %% real rest of seq1
    [{1, <<"abcd">>}] = bodies(M3, [1]),
    {M4, _B5} = feed(frag(2, 4, 0, <<"AB">>), B4),  %% rest of seq2
    [{2, <<"ABXY">>}] = bodies(M4, [2]).

retransmitted_complete_message(_Config) ->
    %% Re-receiving an already-processed ClientHello (retransmit) is
    %% not re-emitted or merged, but reported as a peer retransmission.
    B0 = buffers(),
    {[{Hello, _}], B1} = feed(client_hello(0), B0),
    client_hello = element(1, Hello),
    {[peer_retransmission], B1} = feed(client_hello(0), B1),
    %% Several copies in one record are reported once.
    {[peer_retransmission], B1} =
        feed(<<(client_hello(0))/binary, (client_hello(0))/binary>>, B1).

old_message_seq_discarded(_Config) ->
    %% A fragment with message_seq < next_seq is discarded. Only a
    %% complete ClientHello of at least the minimal valid length is
    %% reported as a peer retransmission, so that tiny records cannot
    %% trigger a larger answer (amplification).
    B0 = buffers(),
    {M0, B1} = feed(frag(0, 1, 0, <<"x">>), B0),
    [{0, <<"x">>}] = bodies(M0, [0]),
    {M1, B2} = feed(frag(1, 1, 0, <<"y">>), B1),
    [{1, <<"y">>}] = bodies(M1, [1]),
    %% next_seq is now 2; old messages that are not ClientHellos
    {[], B2} = feed(frag(0, 1, 0, <<"x">>), B2),
    {[], B2} = feed(frag(0, 0, 0, <<>>), B2),
    %% Partial, empty or too short old ClientHello
    {[], B2} = feed(ch_frag(0, 42, 2, binary:copy(<<0>>, 40)), B2),
    {[], B2} = feed(ch_frag(0, 0, 0, <<>>), B2),
    {[], B2} = feed(ch_frag(0, 41, 0, binary:copy(<<0>>, 41)), B2).

malformed_fragment(_Config) ->
    %% A fragment whose declared fragment_length does not match the data
    %% must raise a fatal decode_error alert, not crash.
    B0 = buffers(),
    %% Claim fragment_length 10 but supply 2 bytes.
    Bad = <<?FINISHED, 4:24, 0:16, 0:24, 10:24, "ab">>,
    try feed(Bad, B0) of
        _ ->
            ct:fail(expected_alert)
    catch
        throw:{error, {tls_alert, _}} ->
            ok;
        throw:Alert when element(1, Alert) == alert ->
            ok
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Fresh, empty protocol_buffers with dtls_handshake_next_seq = 0.
%% Built positionally to avoid including dtls_connection.hrl here:
%%   {protocol_buffers, RecordBuffer, NextSeq, FlightLast,
%%                      NextFragments, LaterFragments, CipherTexts}
buffers() ->
    {protocol_buffers, <<>>, 0, undefined, [], [], []}.

%% One DTLS handshake fragment on the wire:
%%   Type:8 Length:24 MessageSeq:16 FragmentOffset:24 FragmentLength:24 Fragment
frag(Seq, TotalLen, Off, Data) ->
    <<?FINISHED, TotalLen:24, Seq:16, Off:24, (byte_size(Data)):24, Data/binary>>.

%% A ClientHello handshake fragment
ch_frag(Seq, TotalLen, Off, Data) ->
    <<?CLIENT_HELLO_TYPE, TotalLen:24, Seq:16, Off:24, (byte_size(Data)):24,
      Data/binary>>.

%% Complete minimal (42 byte) DTLS 1.2 ClientHello: version, random,
%% empty session_id and cookie, one cipher suite, null compression.
client_hello(Seq) ->
    Body = <<254, 253, 0:256, 0, 0, 2:16, 16#c0, 16#2b, 1, 0>>,
    42 = byte_size(Body),
    ch_frag(Seq, byte_size(Body), 0, Body).

feed(Wire, Buffers) ->
    dtls_handshake:get_dtls_handshake(?DTLS_1_2, Wire, Buffers,
                                      #{log_level => none}).

%% Extract {message_seq, body} pairs from the emitted raw handshake
%% binaries and assert the expected message_seqs (in order).
bodies(Msgs, ExpectedSeqs) ->
    Pairs = [parse_raw(Raw) || {_Decoded, Raw} <- Msgs],
    Seqs = [Seq || {Seq, _} <- Pairs],
    case Seqs =:= ExpectedSeqs of
        true -> Pairs;
        false -> {unexpected_seqs, Seqs, expected, ExpectedSeqs}
    end.

%% A reassembled message is emitted as:
%%   Type:8 Length:24 Seq:16 Offset:24(=0) FragLen:24(=Length) Body
parse_raw(<<_Type, Len:24, Seq:16, 0:24, Len:24, Body:Len/binary>>) ->
    {Seq, Body}.
