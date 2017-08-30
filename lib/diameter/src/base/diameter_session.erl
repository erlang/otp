%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(diameter_session).

-export([sequence/0,
         sequence/1,
         session_id/1,
         origin_state_id/0]).

%% towards diameter_sup
-export([init/0]).

-define(INT64, 16#FFFFFFFFFFFFFFFF).
-define(INT32, 16#FFFFFFFF).

%% ---------------------------------------------------------------------------
%% # sequence/0-1
%%
%% Output: 32-bit
%% ---------------------------------------------------------------------------

%% 3588, 3:
%%
%%    Hop-by-Hop Identifier
%%       The Hop-by-Hop Identifier is an unsigned 32-bit integer field (in
%%       network byte order) and aids in matching requests and replies.
%%       The sender MUST ensure that the Hop-by-Hop identifier in a request
%%       is unique on a given connection at any given time, and MAY attempt
%%       to ensure that the number is unique across reboots.  The sender of
%%       an Answer message MUST ensure that the Hop-by-Hop Identifier field
%%       contains the same value that was found in the corresponding
%%       request.  The Hop-by-Hop identifier is normally a monotonically
%%       increasing number, whose start value was randomly generated.  An
%%       answer message that is received with an unknown Hop-by-Hop
%%       Identifier MUST be discarded.
%%
%%    End-to-End Identifier
%%       The End-to-End Identifier is an unsigned 32-bit integer field (in
%%       network byte order) and is used to detect duplicate messages.
%%       Upon reboot implementations MAY set the high order 12 bits to
%%       contain the low order 12 bits of current time, and the low order
%%       20 bits to a random value.  Senders of request messages MUST
%%       insert a unique identifier on each message.  The identifier MUST
%%       remain locally unique for a period of at least 4 minutes, even
%%       across reboots.  The originator of an Answer message MUST ensure
%%       that the End-to-End Identifier field contains the same value that
%%       was found in the corresponding request.  The End-to-End Identifier
%%       MUST NOT be modified by Diameter agents of any kind.  The
%%       combination of the Origin-Host (see Section 6.3) and this field is
%%       used to detect duplicates.  Duplicate requests SHOULD cause the
%%       same answer to be transmitted (modulo the hop-by-hop Identifier
%%       field and any routing AVPs that may be present), and MUST NOT
%%       affect any state that was set when the original request was
%%       processed.  Duplicate answer messages that are to be locally
%%       consumed (see Section 6.2) SHOULD be silently discarded.

-spec sequence()
   -> diameter:'Unsigned32'().

sequence() ->
    Instr = {_Pos = 2, _Incr = 1, _Threshold = ?INT32, _SetVal = 0},
    ets:update_counter(diameter_sequence, sequence, Instr).

-spec sequence(diameter:sequence())
   -> diameter:'Unsigned32'().

sequence({_,32}) ->
    sequence();

sequence({H,N}) ->
    (H bsl N) bor (sequence() band (1 bsl N - 1)).

%% ---------------------------------------------------------------------------
%% # origin_state_id/0
%% ---------------------------------------------------------------------------

%% 3588, 8.16:
%%
%%    The Origin-State-Id AVP (AVP Code 278), of type Unsigned32, is a
%%    monotonically increasing value that is advanced whenever a Diameter
%%    entity restarts with loss of previous state, for example upon reboot.
%%    Origin-State-Id MAY be included in any Diameter message, including
%%    CER.
%%
%%    A Diameter entity issuing this AVP MUST create a higher value for
%%    this AVP each time its state is reset.  A Diameter entity MAY set
%%    Origin-State-Id to the time of startup, or it MAY use an incrementing
%%    counter retained in non-volatile memory across restarts.

-spec origin_state_id()
   -> diameter:'Unsigned32'().

origin_state_id() ->
    ets:lookup_element(diameter_sequence, origin_state_id, 2).

%% ---------------------------------------------------------------------------
%% # session_id/1
%% ---------------------------------------------------------------------------

%% 3588, 8.8:
%%
%%    The Session-Id MUST begin with the sender's identity encoded in the
%%    DiameterIdentity type (see Section 4.4).  The remainder of the
%%    Session-Id is delimited by a ";" character, and MAY be any sequence
%%    that the client can guarantee to be eternally unique; however, the
%%    following format is recommended, (square brackets [] indicate an
%%    optional element):
%%
%%    <DiameterIdentity>;<high 32 bits>;<low 32 bits>[;<optional value>]
%%
%%    <high 32 bits> and <low 32 bits> are decimal representations of the
%%    high and low 32 bits of a monotonically increasing 64-bit value.  The
%%    64-bit value is rendered in two part to simplify formatting by 32-bit
%%    processors.  At startup, the high 32 bits of the 64-bit value MAY be
%%    initialized to the time, and the low 32 bits MAY be initialized to
%%    zero.  This will for practical purposes eliminate the possibility of
%%    overlapping Session-Ids after a reboot, assuming the reboot process
%%    takes longer than a second.  Alternatively, an implementation MAY
%%    keep track of the increasing value in non-volatile memory.
%%
%%    <optional value> is implementation specific but may include a modem's
%%    device Id, a layer 2 address, timestamp, etc.

-spec session_id(diameter:'DiameterIdentity'())
   -> diameter:'OctetString'().
%% Note that Session-Id has type UTF8String and that any OctetString
%% is a UTF8String.

session_id(Host) ->
    Instr = {_Pos = 2, _Incr = 1, _Threshold = ?INT64, _Set = 0},
    N = ets:update_counter(diameter_sequence, session_base, Instr),
    Hi = N bsr 32,
    Lo = N band ?INT32,
    [Host, ";", integer_to_list(Hi),
           ";", integer_to_list(Lo),
           ";", atom_to_list(node())].

%% ---------------------------------------------------------------------------
%% # init/0
%% ---------------------------------------------------------------------------

init() ->
    Now = diameter_lib:timestamp(),
    Time = time32(Now),
    Seq  = (?INT32 band (Time bsl 20)) bor (rand:uniform(1 bsl 20) - 1),
    ets:insert(diameter_sequence, [{origin_state_id, Time},
				   {session_base, Time bsl 32},
				   {sequence, Seq}]),
    Time.

%% ---------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ---------------------------------------------------------

%% The minimum value represented by a Time value. (See diameter_types.)
%% 32 bits extends to 2104.
-define(TIME0, 62105714048).  %% {{1968,1,20},{3,14,8}}

time32(Now) ->
    Time = calendar:now_to_universal_time(Now),
    Diff = calendar:datetime_to_gregorian_seconds(Time) - ?TIME0,
    Diff band ?INT32.
