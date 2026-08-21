%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Nelson Vides 2026. All Rights Reserved.
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

%% SWAR (SIMD Within A Register) helpers for validating 7 bytes at once.
%%
%% We use 56 bits (7 bytes) because it is the largest value that fits
%% in a BEAM small integer (59-bit on 64-bit). This ensures bitwise and
%% arithmetic guard operations (band, bor, bxor, +, -) compile to fast
%% native code without bignum fallback.
%%
%% Call sites must provide a byte-by-byte fallback when SWAR checks fail.

-define(SWAR_MASK80, 16#80808080808080).
-define(SWAR_MASK01, 16#01010101010101).

%% Detect if any byte in a 56-bit word is zero (Mycroft's trick).
%%
%% This is a simplified variant that omits the standard (bnot V) term.
%% The full formula is: ((V - 0x01..01) band (bnot V) band 0x80..80).
%% The (bnot V) term filters out false positives from bytes >= 0x80,
%% where subtracting 0x01 does not clear the high bit. This term is
%% unnecessary when all bytes of the input word are already known to be
%% < 128 before no_zero_byte is reached (andalso short-circuit), and
%% XOR of two 7-bit values is still 7-bit, so no byte in V can have bit 7 set.
%%
%% We also avoid bnot because the JIT lacks an always_small fast path
%% for it, emitting runtime type checks and bignum fallback calls even
%% when the result provably fits in a small.
%%
%% Borrow propagation between bytes may cause rare false positives
%% (a non-zero byte adjacent to a zero byte detected as zero), but
%% these are harmless: callers fall through to a correct byte-by-byte path.
-define(no_zero_byte(V),
    ((V) - ?SWAR_MASK01) band ?SWAR_MASK80 =:= 0
).

%% SWAR check for `string:length/1` binary fast path: 7 bytes that are
%% UTF-8 single-byte code units (< 128) and not carriage return ($\r, 0x0D).
%% Other C0 controls (e.g. $\n, $\t) are allowed.
-define(are_all_ascii_len_swar(W),
    (W) band ?SWAR_MASK80 =:= 0 andalso
        ?no_zero_byte((W) bxor 16#0D0D0D0D0D0D0D)
).
