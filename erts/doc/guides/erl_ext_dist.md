<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# External Term Format

## Introduction

The external term format is mainly used in the distribution mechanism of Erlang.

As Erlang has a fixed number of types, there is no need for a programmer to
define a specification for the external format used within some application. All
Erlang terms have an external representation and the interpretation of the
different terms is application-specific.

In Erlang the BIF [`erlang:term_to_binary/1,2`](`erlang:term_to_binary/1`) is
used to convert a term into the external format. To convert binary data encoding
to a term, the BIF `erlang:binary_to_term/1` is used.

The distribution does this implicitly when sending messages across node
boundaries.

[](){: #overall_format }

The overall format of the term format is as follows:

| 1     | 1     | N      |
| ----- | ----- | ------ |
| `131` | `Tag` | `Data` |

> #### Note {: .info }
>
> When messages are
> [passed between connected nodes](erl_dist_protocol.md#connected_nodes) and a
> [distribution header](erl_ext_dist.md#distribution-header) is used, the first
> byte containing the version number (131) is omitted from the terms that follow
> the distribution header. This is because the version number is implied by the
> version number in the distribution header.

The compressed term format is as follows:

| 1     | 1    | 4                  | N                     |
| ----- | ---- | ------------------ | --------------------- |
| `131` | `80` | `UncompressedSize` | `Zlib-compressedData` |

Uncompressed size (unsigned 32-bit integer in big-endian byte order) is the size
of the data before it was compressed. The compressed data has the following
format when it has been expanded:

| 1     | Uncompressed Size |
| ----- | ----------------- |
| `Tag` | `Data`            |

[](){: #utf8_atoms }

## Encoding atoms

As from ERTS 9.0 (OTP 20), atoms may contain any Unicode characters.

Atoms sent over node distribution are always encoded in UTF-8 using either
[`ATOM_UTF8_EXT`](erl_ext_dist.md#atom_utf8_ext),
[`SMALL_ATOM_UTF8_EXT`](erl_ext_dist.md#small_atom_utf8_ext) or
[`ATOM_CACHE_REF`](erl_ext_dist.md#atom_cache_ref).

Atoms encoded with [`erlang:term_to_binary/1,2`](`erlang:term_to_binary/1`) or
[`erlang:term_to_iovec/1,2`](`erlang:term_to_iovec/1`) are by default still
using the old deprecated Latin-1 format [`ATOM_EXT`](erl_ext_dist.md#atom_ext)
for atoms that only contain Latin-1 characters (Unicode code points 0-255).
Atoms with higher code points will be encoded in UTF-8 using either
[`ATOM_UTF8_EXT`](erl_ext_dist.md#atom_utf8_ext) or
[`SMALL_ATOM_UTF8_EXT`](erl_ext_dist.md#small_atom_utf8_ext).

The maximum number of allowed characters in an atom is 255. In the UTF-8 case,
each character can need 4 bytes to be encoded.

## Distribution Header

The distribution header is sent by the erlang distribution to carry metadata
about the coming [control message](erl_dist_protocol.md#control_message) and
potential payload. It is primarily used to handle the atom cache in the Erlang
distribution. Since OTP-22 it is also used to fragment large distribution
messages into multiple smaller fragments. For more information about how the
distribution uses the distribution header, see the documentation of the
[protocol between connected nodes](erl_dist_protocol.md#connected_nodes) in the
[distribution protocol](erl_dist_protocol.md) documentation.

Any [ATOM_CACHE_REF](erl_ext_dist.md#atom_cache_ref) entries with corresponding
`AtomCacheReferenceIndex` in terms encoded on the external format following a
distribution header refer to the atom cache references made in the distribution
header. The range is 0 <= `AtomCacheReferenceIndex` < 255, that is, at most 255
different atom cache references from the following terms can be made.

### Normal Distribution Header

The non-fragmented distribution header format is as follows:

| 1     | 1    | 1                       | NumberOfAtomCacheRefs/2+1 \| 0 | N \| 0          |
| ----- | ---- | ----------------------- | ------------------------------ | --------------- |
| `131` | `68` | `NumberOfAtomCacheRefs` | `Flags`                        | `AtomCacheRefs` |

`Flags` consist of `NumberOfAtomCacheRefs/2+1` bytes, unless
`NumberOfAtomCacheRefs` is `0`. If `NumberOfAtomCacheRefs` is `0`, `Flags` and
`AtomCacheRefs` are omitted. Each atom cache reference has a half byte flag
field. Flags corresponding to a specific `AtomCacheReferenceIndex` are located
in flag byte number `AtomCacheReferenceIndex/2`. Flag byte 0 is the first byte
after the `NumberOfAtomCacheRefs` byte. Flags for an even
`AtomCacheReferenceIndex` are located in the least significant half byte and
flags for an odd `AtomCacheReferenceIndex` are located in the most significant
half byte.

The flag field of an atom cache reference has the following format:

| 1 bit               | 3 bits         |
| ------------------- | -------------- |
| `NewCacheEntryFlag` | `SegmentIndex` |

The most significant bit is the `NewCacheEntryFlag`. If set, the corresponding
cache reference is new. The three least significant bits are the `SegmentIndex`
of the corresponding atom cache entry. An atom cache consists of 8 segments,
each of size 256, that is, an atom cache can contain 2048 entries.

Another half byte flag field is located along with flag fields for atom cache
references. When `NumberOfAtomCacheRefs` is even, this half byte is the least
significant half byte of the byte that follows the atom cache references. When
`NumberOfAtomCacheRefs` is odd, this half byte is the most significant half byte
of the last byte of the atom cache references (on the wire, it will appear
before the last cache reference). It has the following format:

| 3 bits            | 1 bit       |
| ----------------- | ----------- |
| `CurrentlyUnused` | `LongAtoms` |

The least significant bit in that half byte is flag `LongAtoms`. If it is set, 2
bytes are used for atom lengths instead of 1 byte in the distribution header.

After the `Flags` field follow the `AtomCacheRefs`. The first `AtomCacheRef` is
the one corresponding to `AtomCacheReferenceIndex` 0\. Higher indices follow in
sequence up to index `NumberOfAtomCacheRefs - 1`.

If the `NewCacheEntryFlag` for the next `AtomCacheRef` has been set, a
`NewAtomCacheRef` on the following format follows:

| 1                      | 1 \| 2   | Length     |
| ---------------------- | -------- | ---------- |
| `InternalSegmentIndex` | `Length` | `AtomText` |

`InternalSegmentIndex` together with the `SegmentIndex` completely identify the
location of an atom cache entry in the atom cache. `Length` is the number of
bytes that `AtomText` consists of. Length is a 2 byte big-endian integer if flag
`LongAtoms` has been set, otherwise a 1 byte integer. When distribution flag
[`DFLAG_UTF8_ATOMS`](erl_dist_protocol.md#DFLAG_UTF8_ATOMS) has been exchanged
between both nodes in the
[distribution handshake](erl_dist_protocol.md#distribution_handshake),
characters in `AtomText` are encoded in UTF-8, otherwise in Latin-1. The
following `CachedAtomRef`s with the same `SegmentIndex` and
`InternalSegmentIndex` as this `NewAtomCacheRef` refer to this atom until a new
`NewAtomCacheRef` with the same `SegmentIndex` and `InternalSegmentIndex`
appear.

For more information on encoding of atoms, see the
[section on UTF-8 encoded atoms](erl_ext_dist.md#utf8_atoms) above.

If the `NewCacheEntryFlag` for the next `AtomCacheRef` has not been set, a
`CachedAtomRef` on the following format follows:

| 1                      |
| ---------------------- |
| `InternalSegmentIndex` |

`InternalSegmentIndex` together with the `SegmentIndex` identify the location of
the atom cache entry in the atom cache. The atom corresponding to this
`CachedAtomRef` is the latest `NewAtomCacheRef` preceding this `CachedAtomRef`
in another previously passed distribution header.

[](){: #fragments }

### Distribution Header for fragmented messages

Messages sent between Erlang nodes can sometimes be quite large. Since OTP-22 it
is possible to split large messages into smaller fragments in order to allow
smaller messages to be interleaved between larges messages. It is only the
`message` part of each
[distributed message](erl_dist_protocol.md#connected_nodes) that may be split
using fragmentation. Therefore it is recommended to use the
[PAYLOAD control messages](erl_dist_protocol.md#new-ctrlmessages-for-erlang-otp-22)
introduced in OTP-22.

Fragmented distribution messages are only used if the receiving node signals
that it supports them via the
[DFLAG_FRAGMENTS](erl_dist_protocol.md#DFLAG_FRAGMENTS) distribution flag.

A process must complete the sending of a fragmented message before it can start
sending any other message on the same distribution channel.

The start of a sequence of fragmented messages looks like this:

| 1     | 1    | 8            | 8            | 1                       | NumberOfAtomCacheRefs/2+1 \| 0 | N \| 0          |
| ----- | ---- | ------------ | ------------ | ----------------------- | ------------------------------ | --------------- |
| `131` | `69` | `SequenceId` | `FragmentId` | `NumberOfAtomCacheRefs` | `Flags`                        | `AtomCacheRefs` |

The continuation of a sequence of fragmented messages looks like this:

| 1     | 1    | 8            | 8            |
| ----- | ---- | ------------ | ------------ |
| `131` | `70` | `SequenceId` | `FragmentId` |

The starting distribution header is very similar to a non-fragmented
distribution header. The atom cache works the same as for normal distribution
header and is the same for the entire sequence. The additional fields added are
the sequence id and fragment id.

- **Sequence ID** - The sequence id is used to uniquely identify a fragmented
  message sent from one process to another on the same distributed connection.
  This is used to identify which sequence a fragment is a part of as the same
  process can be in the process of receiving multiple sequences at the same
  time.

  As one process can only be sending one fragmented message at once, it can be
  convenient to use the local PID as the sequence id.

- **Fragments ID** - The Fragment ID is used to number the fragments in a
  sequence. The id starts at the total number of fragments and then decrements
  to 1 (which is the final fragment). So if a sequence consists of 3 fragments
  the fragment id in the starting header will be 3, and then fragments 2 and 1
  are sent.

  The fragments must be delivered in the correct order, so if an unordered
  distribution carrier is used, they must be ordered before delivered to the
  Erlang run-time.

#### Example:

As an example, let say that we want to send
`{call, <0.245.2>, {set_get_state, <<0:1024>>}}` to registered process `reg`
using a fragment size of 128. To send this message we need a distribution
header, atom cache updates, the control message (which would be
`{6, <0.245.2>, [], reg}` in this case) and finally the actual message. This
would all be encoded into:

```text
131,69,0,0,2,168,0,0,5,83,0,0,0,0,0,0,0,2,               %% Header with seq and frag id
5,4,137,9,10,5,236,3,114,101,103,9,4,99,97,108,108,      %% Atom cache updates
238,13,115,101,116,95,103,101,116,95,115,116,97,116,101,
104,4,97,6,103,82,0,0,0,0,85,0,0,0,0,2,82,1,82,2,        %% Control message
104,3,82,3,103,82,0,0,0,0,245,0,0,0,2,2,                 %% Actual message using cached atoms
104,2,82,4,109,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

131,70,0,0,2,168,0,0,5,83,0,0,0,0,0,0,0,1,               %% Cont Header with seq and frag id
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,               %% Rest of payload
0,0,0,0
```

Let us break that apart into its components. First we have the distribution
header tags together with the sequence id and a fragment id of 2.

```text
131,69,                   %% Start fragment header
0,0,2,168,0,0,5,83,       %% The sequence ID
0,0,0,0,0,0,0,2,           %% The fragment ID
```

Then we have the updates to the atom cache:

```text
5,4,137,9,  %% 5 atoms and their flags
10,5,       %% The already cached atom ids
236,3,114,101,103,  %% The atom 'reg'
9,4,99,97,108,108,  %% The atom 'call'
238,13,115,101,116,95,103,101,116,95,115,116,97,116,101, %% The atom 'set_get_state'
```

The first byte says that we have 5 atoms that are part of the cache. Then
follows three bytes that are the atom cache ref flags. Each of the flags uses 4
bits so they are a bit hard to read in decimal byte form. In binary half-byte
form they look like this:

```text
0000, 0100, 1000, 1001, 1001
```

As the high bit of the first two atoms in the cache are not set we know that
they are already in the cache, so they do not have to be sent again (this is the
node name of the receiving and sending node). Then follows the atoms that have
to be sent, together with their segment ids.

Then the listing of the atoms comes, starting with 10 and 5 which are the atom
refs of the already cached atoms. Then the new atoms are sent.

When the atom cache is setup correctly the control message is sent.

```text
104,4,97,6,103,82,0,0,0,0,85,0,0,0,0,2,82,1,82,2,
```

Note that up until here it is not allowed to fragments the message. The entire
atom cache and control message has to be part of the starting fragment. After
the control message the payload of the message is sent using 128 bytes:

```text
104,3,82,3,103,82,0,0,0,0,245,0,0,0,2,2,
104,2,82,4,109,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
```

Since the payload is larger than 128-bytes it is split into two fragments. The
second fragment does not have any atom cache update instructions so it is a lot
simpler:

```text
131,70,0,0,2,168,0,0,5,83,0,0,0,0,0,0,0,1, %% Continuation dist header 70 with seq and frag id
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, %% remaining payload
0,0,0,0
```

> #### Note {: .info }
>
> The fragment size of 128 is only used as an example. Any fragments size may be
> used when sending fragmented messages.

## ATOM_CACHE_REF

| 1    | 1                         |
| ---- | ------------------------- |
| `82` | `AtomCacheReferenceIndex` |

Refers to the atom with `AtomCacheReferenceIndex` in the
[distribution header](erl_ext_dist.md#distribution-header).

## SMALL_INTEGER_EXT

| 1    | 1     |
| ---- | ----- |
| `97` | `Int` |

Unsigned 8-bit integer.

## INTEGER_EXT

| 1    | 4     |
| ---- | ----- |
| `98` | `Int` |

Signed 32-bit integer in big-endian format.

## FLOAT_EXT

| 1    | 31             |
| ---- | -------------- |
| `99` | `Float string` |

A finite float (i.e. not inf, -inf or NaN) is stored in string format. The
format used in sprintf to format the float is "%.20e" (there are more bytes
allocated than necessary). To unpack the float, use sscanf with format "%lf".

This term is used in minor version 0 of the external format; it has been
superseded by [`NEW_FLOAT_EXT`](erl_ext_dist.md#new_float_ext).

## PORT_EXT

| 1     | N      | 4    | 1          |
| ----- | ------ | ---- | ---------- |
| `102` | `Node` | `ID` | `Creation` |

Same as [`NEW_PORT_EXT`](erl_ext_dist.md#new_port_ext) except the `Creation`
field is only one byte and only two bits are significant, the rest are to be 0.

## NEW_PORT_EXT

| 1    | N      | 4    | 4          |
| ---- | ------ | ---- | ---------- |
| `89` | `Node` | `ID` | `Creation` |

Same as [`V4_PORT_EXT`](erl_ext_dist.md#v4_port_ext) except the `ID` field is
only four bytes. Only 28 bits are significant; the rest are to be 0.

`NEW_PORT_EXT` was introduced in OTP 19, but only to be decoded and echoed back.
Not encoded for local ports.

In OTP 23 distribution flag
[`DFLAG_BIG_CREATION`](erl_dist_protocol.md#DFLAG_BIG_CREATION) became
mandatory. All ports are now encoded using `NEW_PORT_EXT`, even external ports
received as [`PORT_EXT`](erl_ext_dist.md#port_ext) from older nodes.

## V4_PORT_EXT

| 1     | N      | 8    | 4          |
| ----- | ------ | ---- | ---------- |
| `120` | `Node` | `ID` | `Creation` |

Encodes a port identifier (obtained from `erlang:open_port/2`). `Node` is the
originating node, [encoded as an atom](erl_ext_dist.md#utf8_atoms). `ID` is a
64-bit big endian unsigned integer. The `Creation` works just like in
[`NEW_PID_EXT`](erl_ext_dist.md#new_pid_ext). Port operations are not allowed
across node boundaries.

In OTP 26 distribution flag [`DFLAG_V4_NC`](erl_dist_protocol.md#DFLAG_V4_NC) as
well as `V4_PORT_EXT` became mandatory accepting full 64-bit ports to be decoded
and echoed back.

## PID_EXT

| 1     | N      | 4    | 4        | 1          |
| ----- | ------ | ---- | -------- | ---------- |
| `103` | `Node` | `ID` | `Serial` | `Creation` |

Same as [`NEW_PID_EXT`](erl_ext_dist.md#new_pid_ext) except the `Creation` field
is only one byte and only two bits are significant, the rest are to be 0.

## NEW_PID_EXT

| 1    | N      | 4    | 4        | 4          |
| ---- | ------ | ---- | -------- | ---------- |
| `88` | `Node` | `ID` | `Serial` | `Creation` |

Encodes an Erlang process identifier object.

- **`Node`** - The name of the originating node,
  [encoded as an atom](erl_ext_dist.md#utf8_atoms).

- **`ID`** - A 32-bit big endian unsigned integer.

- **`Serial`** - A 32-bit big endian unsigned integer.

- **`Creation`** - A 32-bit big endian unsigned integer. All identifiers
  originating from the same node incarnation must have identical `Creation`
  values. This makes it possible to separate identifiers from old (crashed)
  nodes from a new one. The value zero is reserved and must be avoided for
  normal operations.

`NEW_PID_EXT` was introduced in OTP 19, but only to be decoded and echoed back.
Not encoded for local processes.

In OTP 23 distribution flag
[`DFLAG_BIG_CREATION`](erl_dist_protocol.md#DFLAG_BIG_CREATION) became
mandatory. All pids are now encoded using `NEW_PID_EXT`, even external pids
received as [`PID_EXT`](erl_ext_dist.md#pid_ext) from older nodes.

In OTP 26 distribution flag [`DFLAG_V4_NC`](erl_dist_protocol.md#DFLAG_V4_NC)
became mandatory accepting full 64-bit pids to be decoded and echoed back.

## SMALL_TUPLE_EXT

| 1     | 1       | N          |
| ----- | ------- | ---------- |
| `104` | `Arity` | `Elements` |

Encodes a tuple. The `Arity` field is an unsigned byte that determines how many
elements that follows in section `Elements`.

## LARGE_TUPLE_EXT

| 1     | 4       | N          |
| ----- | ------- | ---------- |
| `105` | `Arity` | `Elements` |

Same as [`SMALL_TUPLE_EXT`](erl_ext_dist.md#small_tuple_ext) except that `Arity`
is an unsigned 4 byte integer in big-endian format.

## MAP_EXT

| 1     | 4       | N       |
| ----- | ------- | ------- |
| `116` | `Arity` | `Pairs` |

Encodes a map. The `Arity` field is an unsigned 4 byte integer in big-endian
format that determines the number of key-value pairs in the map. Key and value
pairs (`Ki => Vi`) are encoded in section `Pairs` in the following order:
`K1, V1, K2, V2,..., Kn, Vn`. Duplicate keys are _not allowed_ within the same
map.

*As from *Erlang/OTP 17.0

## NIL_EXT

| 1     |
| ----- |
| `106` |

The representation for an empty list, that is, the Erlang syntax `[]`.

## STRING_EXT

| 1     | 2        | Len          |
| ----- | -------- | ------------ |
| `107` | `Length` | `Characters` |

String does _not_ have a corresponding Erlang representation, but is an
optimization for sending lists of bytes (integer in the range 0-255) more
efficiently over the distribution. As field `Length` is an unsigned 2 byte
integer (big-endian), implementations must ensure that lists longer than 65535
elements are encoded as [`LIST_EXT`](erl_ext_dist.md#list_ext).

## LIST_EXT

| 1     | 4        |            |        |
| ----- | -------- | ---------- | ------ |
| `108` | `Length` | `Elements` | `Tail` |

`Length` is the number of elements that follows in section `Elements`. `Tail` is
the final tail of the list; it is [`NIL_EXT`](erl_ext_dist.md#nil_ext) for a
proper list, but can be any type if the list is improper (for example, `[a|b]`).

## BINARY_EXT

| 1     | 4     | Len    |
| ----- | ----- | ------ |
| `109` | `Len` | `Data` |

Binaries are generated with bit syntax expression or with
`erlang:list_to_binary/1`, `erlang:term_to_binary/1`, or as input from binary
ports. The `Len` length field is an unsigned 4 byte integer (big-endian).

## SMALL_BIG_EXT

| 1     | 1   | 1      | n                   |
| ----- | --- | ------ | ------------------- |
| `110` | `n` | `Sign` | `d(0)` ... `d(n-1)` |

Bignums are stored in unary form with a `Sign` byte, that is, 0 if the bignum is
positive and 1 if it is negative. The digits are stored with the least
significant byte stored first. To calculate the integer, the following formula
can be used:

```
B = 256
(d0*B^0 + d1*B^1 + d2*B^2 + ... d(N-1)*B^(n-1))
```


## LARGE_BIG_EXT

| 1     | 4   | 1      | n                   |
| ----- | --- | ------ | ------------------- |
| `111` | `n` | `Sign` | `d(0)` ... `d(n-1)` |

Same as [`SMALL_BIG_EXT`](erl_ext_dist.md#small_big_ext) except that the length
field is an unsigned 4 byte integer.

## REFERENCE_EXT (deprecated)

| 1     | N      | 4    | 1          |
| ----- | ------ | ---- | ---------- |
| `101` | `Node` | `ID` | `Creation` |

The same as [`NEW_REFERENCE_EXT`](erl_ext_dist.md#new_reference_ext) except `ID`
is only one word (`Len` = 1).

## NEW_REFERENCE_EXT

| 1     | 2     | N      | 1          | N'       |
| ----- | ----- | ------ | ---------- | -------- |
| `114` | `Len` | `Node` | `Creation` | `ID ...` |

The same as [`NEWER_REFERENCE_EXT`](erl_ext_dist.md#newer_reference_ext)
_except_:

- **`ID`** - In the first word (4 bytes) of `ID`, only 18 bits are significant,
  the rest must be 0.

- **`Creation`** - Only one byte long and only two bits are significant, the
  rest must be 0.

## NEWER_REFERENCE_EXT

| 1    | 2     | N      | 4          | N'       |
| ---- | ----- | ------ | ---------- | -------- |
| `90` | `Len` | `Node` | `Creation` | `ID ...` |

Encodes a reference term generated with `erlang:make_ref/0`.

- **`Node`** - The name of the originating node,
  [encoded as an atom](erl_ext_dist.md#utf8_atoms).

- **`Len`** - A 16-bit big endian unsigned integer not larger than 5.

- **`ID`** - A sequence of `Len` big-endian unsigned integers (4 bytes each, so
  `N'` = 4 \* `Len`), but is to be regarded as uninterpreted data.

- **`Creation`** - Works just like in
  [`NEW_PID_EXT`](erl_ext_dist.md#new_pid_ext).

`NEWER_REFERENCE_EXT` was introduced in OTP 19, but only to be decoded and
echoed back. Not encoded for local references.

In OTP 23 distribution flag
[`DFLAG_BIG_CREATION`](erl_dist_protocol.md#DFLAG_BIG_CREATION) became
mandatory. All references are now encoded using `NEWER_REFERENCE_EXT`, even
external references received as
[`NEW_REFERENCE_EXT`](erl_ext_dist.md#new_reference_ext) from older nodes.

In OTP 26 distribution flag [`DFLAG_V4_NC`](erl_dist_protocol.md#DFLAG_V4_NC)
became mandatory. References now can contain up to 5 `ID` words.

## FUN_EXT (removed)

| 1     | 4         | N1    | N2       | N3      | N4     | N5              |
| ----- | --------- | ----- | -------- | ------- | ------ | --------------- |
| `117` | `NumFree` | `Pid` | `Module` | `Index` | `Uniq` | `Free vars ...` |

Not emitted since OTP R8, and not decoded since OTP 23.

## NEW_FUN_EXT

| 1     | 4      | 1       | 16     | 4       | 4         | N1       | N2         | N3        | N4    | N5          |
| ----- | ------ | ------- | ------ | ------- | --------- | -------- | ---------- | --------- | ----- | ----------- |
| `112` | `Size` | `Arity` | `Uniq` | `Index` | `NumFree` | `Module` | `OldIndex` | `OldUniq` | `Pid` | `Free Vars` |

This is the encoding of internal funs: `fun F/A` and `fun(Arg1,..) -> ... end`.

- **`Size`** - The total number of bytes, including field `Size`.

- **`Arity`** - The arity of the function implementing the fun.

- **`Uniq`** - The 16 bytes MD5 of the significant parts of the Beam file.

- **`Index`** - An index number. Each fun within a module has an unique index.
  `Index` is stored in big-endian byte order.

- **`NumFree`** - The number of free variables.

- **`Module`** - The module that the fun is implemented in,
  [encoded as an atom](erl_ext_dist.md#utf8_atoms).

- **`OldIndex`** - An integer encoded using
  [`SMALL_INTEGER_EXT`](erl_ext_dist.md#small_integer_ext) or
  [`INTEGER_EXT`](erl_ext_dist.md#integer_ext). Is typically a small index into
  the module's fun table.

- **`OldUniq`** - An integer encoded using
  [`SMALL_INTEGER_EXT`](erl_ext_dist.md#small_integer_ext) or
  [`INTEGER_EXT`](erl_ext_dist.md#integer_ext). `Uniq` is the hash value of the
  parse tree for the fun.

- **`Pid`** - A process identifier as in [`PID_EXT`](erl_ext_dist.md#pid_ext).
  Represents the process in which the fun was created.

- **`Free vars`** - `NumFree` number of terms, each one encoded according to its
  type.

## EXPORT_EXT

| 1     | N1       | N2         | N3      |
| ----- | -------- | ---------- | ------- |
| `113` | `Module` | `Function` | `Arity` |

This term is the encoding for external funs: `fun M:F/A`.

`Module` and `Function` are [encoded as atoms](erl_ext_dist.md#utf8_atoms).

`Arity` is an integer encoded using
[`SMALL_INTEGER_EXT`](erl_ext_dist.md#small_integer_ext).

## BIT_BINARY_EXT

| 1    | 4     | 1      | Len    |
| ---- | ----- | ------ | ------ |
| `77` | `Len` | `Bits` | `Data` |

This term represents a bitstring whose length in bits does not have to be a
multiple of 8. The `Len` field is an unsigned 4 byte integer (big-endian). The
`Bits` field is the number of bits (1-8) that are used in the last byte in the
data field, counting from the most significant bit to the least significant.

## NEW_FLOAT_EXT

| 1    | 8            |
| ---- | ------------ |
| `70` | `IEEE float` |

A finite float (i.e. not inf, -inf or NaN) is stored as 8 bytes in big-endian
IEEE format.

This term is used in minor version 1 of the external format.

## ATOM_UTF8_EXT

| 1     | 2     | Len        |
| ----- | ----- | ---------- |
| `118` | `Len` | `AtomName` |

An atom is stored with a 2 byte unsigned length in big-endian order, followed by
`Len` bytes containing the `AtomName` encoded in UTF-8.

For more information, see the
[section on encoding atoms](erl_ext_dist.md#utf8_atoms) in the beginning of this
page.

## SMALL_ATOM_UTF8_EXT

| 1     | 1     | Len        |
| ----- | ----- | ---------- |
| `119` | `Len` | `AtomName` |

An atom is stored with a 1 byte unsigned length, followed by `Len` bytes
containing the `AtomName` encoded in UTF-8. Longer atoms encoded in UTF-8 can be
represented using [`ATOM_UTF8_EXT`](erl_ext_dist.md#atom_utf8_ext).

For more information, see the
[section on encoding atoms](erl_ext_dist.md#utf8_atoms) in the beginning of this
page.

[](){: #atom_ext }

## ATOM_EXT (deprecated)

| 1     | 2     | Len        |
| ----- | ----- | ---------- |
| `100` | `Len` | `AtomName` |

An atom is stored with a 2 byte unsigned length in big-endian order, followed by
`Len` numbers of 8-bit Latin-1 characters that forms the `AtomName`. The maximum
allowed value for `Len` is 255.

[](){: #small_atom_ext }

## SMALL_ATOM_EXT (deprecated)

| 1     | 1     | Len        |
| ----- | ----- | ---------- |
| `115` | `Len` | `AtomName` |

An atom is stored with a 1 byte unsigned length, followed by `Len` numbers of
8-bit Latin-1 characters that forms the `AtomName`.

> #### Note {: .info }
>
> `SMALL_ATOM_EXT` was introduced in ERTS 5.7.2 and require an exchange of
> distribution flag
> [`DFLAG_SMALL_ATOM_TAGS`](erl_dist_protocol.md#DFLAG_SMALL_ATOM_TAGS) in the
> [distribution handshake](erl_dist_protocol.md#distribution_handshake).

## LOCAL_EXT

| 1     | ... |
| ----- | --- |
| `121` | ... |

Marks that this is encoded on an alternative local external term format intended
to only be decoded by a specific local decoder. The bytes following from here on
may contain any unspecified type of encoding of terms. It is the responsibility
of the user to only attempt to decode terms on the local external term format
which has been produced by a matching encoder.

This tag is used by the Erlang runtime system upon encoding the local external
term format when the [`local`](`m:erlang#term_to_binary_local`) option is passed
to [`term_to_binary/2`](`erlang:term_to_binary/2`), but can be used by other
encoders as well providing similar functionality. The Erlang runtime system adds
a hash immediately following the `LOCAL_EXT` tag which is verified on decoding
in order to verify that encoder and decoder match which might be a good
practice. This will very likely catch mistakes made by users, but is not
guaranteed to, and is not intended to, prevent decoding of an intentionally
forged encoding on the local external term format.

`LOCAL_EXT` was introduced in OTP 26.0.
