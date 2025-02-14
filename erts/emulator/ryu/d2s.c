// Copyright 2018 Ulf Adams
//
// The contents of this file may be used under the terms of the Apache License,
// Version 2.0.
//
//    (See accompanying file LICENSE-Apache or copy at
//     http://www.apache.org/licenses/LICENSE-2.0)
//
// Alternatively, the contents of this file may be used under the terms of
// the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE-Boost or copy at
//     https://www.boost.org/LICENSE_1_0.txt)
//
// Unless required by applicable law or agreed to in writing, this software
// is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.

// Runtime compiler options:
// -DRYU_DEBUG Generate verbose debugging output to stdout.
//
// -DRYU_ONLY_64_BIT_OPS Avoid using uint128_t or 64-bit intrinsics. Slower,
//     depending on your compiler.
//

// CHANGE_FOR_ERLANG: "ryu/ryu.h" -> "ryu.h"
#include "ryu.h"
// END CHANGE_FOR_ERLANG

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef RYU_DEBUG
#include <inttypes.h>
#include <stdio.h>
#endif

// CHANGE_FOR_ERLANG: "ryu/*.h" -> "*.h"
#include "common.h"
#include "digit_table.h"
#include "d2s_intrinsics.h"
// END CHANGE_FOR_ERLANG

// CHANGE_FOR_ERLANG we got rid of the small_table. Also namespace as above
#include "d2s_full_table.h"
// END CHANGE_FOR_ERLANG

#define DOUBLE_MANTISSA_BITS 52
#define DOUBLE_EXPONENT_BITS 11
#define DOUBLE_BIAS 1023

static inline uint32_t decimalLength17(const uint64_t v) {
  // This is slightly faster than a loop.
  // The average output length is 16.38 digits, so we check high-to-low.
  // Function precondition: v is not an 18, 19, or 20-digit number.
  // (17 digits are sufficient for round-tripping.)
  assert(v < 100000000000000000L);
  if (v >= 10000000000000000L) { return 17; }
  if (v >= 1000000000000000L) { return 16; }
  if (v >= 100000000000000L) { return 15; }
  if (v >= 10000000000000L) { return 14; }
  if (v >= 1000000000000L) { return 13; }
  if (v >= 100000000000L) { return 12; }
  if (v >= 10000000000L) { return 11; }
  if (v >= 1000000000L) { return 10; }
  if (v >= 100000000L) { return 9; }
  if (v >= 10000000L) { return 8; }
  if (v >= 1000000L) { return 7; }
  if (v >= 100000L) { return 6; }
  if (v >= 10000L) { return 5; }
  if (v >= 1000L) { return 4; }
  if (v >= 100L) { return 3; }
  if (v >= 10L) { return 2; }
  return 1;
}

// A floating decimal representing m * 10^e.
typedef struct floating_decimal_64 {
  uint64_t mantissa;
  // Decimal exponent's range is -324 to 308
  // inclusive, and can fit in a short if needed.
  int32_t exponent;
} floating_decimal_64;

static inline floating_decimal_64 d2d(const uint64_t ieeeMantissa, const uint32_t ieeeExponent) {
  int32_t e2;
  uint64_t m2;
  if (ieeeExponent == 0) {
    // We subtract 2 so that the bounds computation has 2 additional bits.
    e2 = 1 - DOUBLE_BIAS - DOUBLE_MANTISSA_BITS - 2;
    m2 = ieeeMantissa;
  } else {
    e2 = (int32_t) ieeeExponent - DOUBLE_BIAS - DOUBLE_MANTISSA_BITS - 2;
    m2 = (1ull << DOUBLE_MANTISSA_BITS) | ieeeMantissa;
  }
  const bool even = (m2 & 1) == 0;
  const bool acceptBounds = even;

#ifdef RYU_DEBUG
  printf("-> %" PRIu64 " * 2^%d\n", m2, e2 + 2);
#endif

  // Step 2: Determine the interval of valid decimal representations.
  const uint64_t mv = 4 * m2;
  // Implicit bool -> int conversion. True is 1, false is 0.
  const uint32_t mmShift = ieeeMantissa != 0 || ieeeExponent <= 1;
  // We would compute mp and mm like this:
  // uint64_t mp = 4 * m2 + 2;
  // uint64_t mm = mv - 1 - mmShift;

  // Step 3: Convert to a decimal power base using 128-bit arithmetic.
  uint64_t vr, vp, vm;
  int32_t e10;
  bool vmIsTrailingZeros = false;
  bool vrIsTrailingZeros = false;
  if (e2 >= 0) {
    // I tried special-casing q == 0, but there was no effect on performance.
    // This expression is slightly faster than max(0, log10Pow2(e2) - 1).
    const uint32_t q = log10Pow2(e2) - (e2 > 3);
    e10 = (int32_t) q;
    const int32_t k = DOUBLE_POW5_INV_BITCOUNT + pow5bits((int32_t) q) - 1;
    const int32_t i = -e2 + (int32_t) q + k;
    vr = mulShiftAll64(m2, DOUBLE_POW5_INV_SPLIT[q], i, &vp, &vm, mmShift);
#ifdef RYU_DEBUG
    printf("%" PRIu64 " * 2^%d / 10^%u\n", mv, e2, q);
    printf("V+=%" PRIu64 "\nV =%" PRIu64 "\nV-=%" PRIu64 "\n", vp, vr, vm);
#endif
    if (q <= 21) {
      // This should use q <= 22, but I think 21 is also safe. Smaller values
      // may still be safe, but it's more difficult to reason about them.
      // Only one of mp, mv, and mm can be a multiple of 5, if any.
      const uint32_t mvMod5 = ((uint32_t) mv) - 5 * ((uint32_t) div5(mv));
      if (mvMod5 == 0) {
        vrIsTrailingZeros = multipleOfPowerOf5(mv, q);
      } else if (acceptBounds) {
        // Same as min(e2 + (~mm & 1), pow5Factor(mm)) >= q
        // <=> e2 + (~mm & 1) >= q && pow5Factor(mm) >= q
        // <=> true && pow5Factor(mm) >= q, since e2 >= q.
        vmIsTrailingZeros = multipleOfPowerOf5(mv - 1 - mmShift, q);
      } else {
        // Same as min(e2 + 1, pow5Factor(mp)) >= q.
        vp -= multipleOfPowerOf5(mv + 2, q);
      }
    }
  } else {
    // This expression is slightly faster than max(0, log10Pow5(-e2) - 1).
    const uint32_t q = log10Pow5(-e2) - (-e2 > 1);
    e10 = (int32_t) q + e2;
    const int32_t i = -e2 - (int32_t) q;
    const int32_t k = pow5bits(i) - DOUBLE_POW5_BITCOUNT;
    const int32_t j = (int32_t) q - k;
    vr = mulShiftAll64(m2, DOUBLE_POW5_SPLIT[i], j, &vp, &vm, mmShift);
#ifdef RYU_DEBUG
    printf("%" PRIu64 " * 5^%d / 10^%u\n", mv, -e2, q);
    printf("%u %d %d %d\n", q, i, k, j);
    printf("V+=%" PRIu64 "\nV =%" PRIu64 "\nV-=%" PRIu64 "\n", vp, vr, vm);
#endif
    if (q <= 1) {
      // {vr,vp,vm} is trailing zeros if {mv,mp,mm} has at least q trailing 0 bits.
      // mv = 4 * m2, so it always has at least two trailing 0 bits.
      vrIsTrailingZeros = true;
      if (acceptBounds) {
        // mm = mv - 1 - mmShift, so it has 1 trailing 0 bit iff mmShift == 1.
        vmIsTrailingZeros = mmShift == 1;
      } else {
        // mp = mv + 2, so it always has at least one trailing 0 bit.
        --vp;
      }
    } else if (q < 63) { // TODO(ulfjack): Use a tighter bound here.
      // We want to know if the full product has at least q trailing zeros.
      // We need to compute min(p2(mv), p5(mv) - e2) >= q
      // <=> p2(mv) >= q && p5(mv) - e2 >= q
      // <=> p2(mv) >= q (because -e2 >= q)
      vrIsTrailingZeros = multipleOfPowerOf2(mv, q);
#ifdef RYU_DEBUG
      printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    }
  }
#ifdef RYU_DEBUG
  printf("e10=%d\n", e10);
  printf("V+=%" PRIu64 "\nV =%" PRIu64 "\nV-=%" PRIu64 "\n", vp, vr, vm);
  printf("vm is trailing zeros=%s\n", vmIsTrailingZeros ? "true" : "false");
  printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif

  // Step 4: Find the shortest decimal representation in the interval of valid representations.
  int32_t removed = 0;
  uint8_t lastRemovedDigit = 0;
  uint64_t output;
  // On average, we remove ~2 digits.
  if (vmIsTrailingZeros || vrIsTrailingZeros) {
    // General case, which happens rarely (~0.7%).
    for (;;) {
      const uint64_t vpDiv10 = div10(vp);
      const uint64_t vmDiv10 = div10(vm);
      if (vpDiv10 <= vmDiv10) {
        break;
      }
      const uint32_t vmMod10 = ((uint32_t) vm) - 10 * ((uint32_t) vmDiv10);
      const uint64_t vrDiv10 = div10(vr);
      const uint32_t vrMod10 = ((uint32_t) vr) - 10 * ((uint32_t) vrDiv10);
      vmIsTrailingZeros &= vmMod10 == 0;
      vrIsTrailingZeros &= lastRemovedDigit == 0;
      lastRemovedDigit = (uint8_t) vrMod10;
      vr = vrDiv10;
      vp = vpDiv10;
      vm = vmDiv10;
      ++removed;
    }
#ifdef RYU_DEBUG
    printf("V+=%" PRIu64 "\nV =%" PRIu64 "\nV-=%" PRIu64 "\n", vp, vr, vm);
    printf("d-10=%s\n", vmIsTrailingZeros ? "true" : "false");
#endif
    if (vmIsTrailingZeros) {
      for (;;) {
        const uint64_t vmDiv10 = div10(vm);
        const uint32_t vmMod10 = ((uint32_t) vm) - 10 * ((uint32_t) vmDiv10);
        if (vmMod10 != 0) {
          break;
        }
        const uint64_t vpDiv10 = div10(vp);
        const uint64_t vrDiv10 = div10(vr);
        const uint32_t vrMod10 = ((uint32_t) vr) - 10 * ((uint32_t) vrDiv10);
        vrIsTrailingZeros &= lastRemovedDigit == 0;
        lastRemovedDigit = (uint8_t) vrMod10;
        vr = vrDiv10;
        vp = vpDiv10;
        vm = vmDiv10;
        ++removed;
      }
    }
#ifdef RYU_DEBUG
    printf("%" PRIu64 " %d\n", vr, lastRemovedDigit);
    printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    if (vrIsTrailingZeros && lastRemovedDigit == 5 && vr % 2 == 0) {
      // Round even if the exact number is .....50..0.
      lastRemovedDigit = 4;
    }
    // We need to take vr + 1 if vr is outside bounds or we need to round up.
    output = vr + ((vr == vm && (!acceptBounds || !vmIsTrailingZeros)) || lastRemovedDigit >= 5);
  } else {
    // Specialized for the common case (~99.3%). Percentages below are relative to this.
    bool roundUp = false;
    const uint64_t vpDiv100 = div100(vp);
    const uint64_t vmDiv100 = div100(vm);
    if (vpDiv100 > vmDiv100) { // Optimization: remove two digits at a time (~86.2%).
      const uint64_t vrDiv100 = div100(vr);
      const uint32_t vrMod100 = ((uint32_t) vr) - 100 * ((uint32_t) vrDiv100);
      roundUp = vrMod100 >= 50;
      vr = vrDiv100;
      vp = vpDiv100;
      vm = vmDiv100;
      removed += 2;
    }
    // Loop iterations below (approximately), without optimization above:
    // 0: 0.03%, 1: 13.8%, 2: 70.6%, 3: 14.0%, 4: 1.40%, 5: 0.14%, 6+: 0.02%
    // Loop iterations below (approximately), with optimization above:
    // 0: 70.6%, 1: 27.8%, 2: 1.40%, 3: 0.14%, 4+: 0.02%
    for (;;) {
      const uint64_t vpDiv10 = div10(vp);
      const uint64_t vmDiv10 = div10(vm);
      if (vpDiv10 <= vmDiv10) {
        break;
      }
      const uint64_t vrDiv10 = div10(vr);
      const uint32_t vrMod10 = ((uint32_t) vr) - 10 * ((uint32_t) vrDiv10);
      roundUp = vrMod10 >= 5;
      vr = vrDiv10;
      vp = vpDiv10;
      vm = vmDiv10;
      ++removed;
    }
#ifdef RYU_DEBUG
    printf("%" PRIu64 " roundUp=%s\n", vr, roundUp ? "true" : "false");
    printf("vr is trailing zeros=%s\n", vrIsTrailingZeros ? "true" : "false");
#endif
    // We need to take vr + 1 if vr is outside bounds or we need to round up.
    output = vr + (vr == vm || roundUp);
  }
  const int32_t exp = e10 + removed;

#ifdef RYU_DEBUG
  printf("V+=%" PRIu64 "\nV =%" PRIu64 "\nV-=%" PRIu64 "\n", vp, vr, vm);
  printf("O=%" PRIu64 "\n", output);
  printf("EXP=%d\n", exp);
#endif

  floating_decimal_64 fd;
  fd.exponent = exp;
  fd.mantissa = output;
  return fd;
}

//CHANGE_FOR_ERLANG: This format is new, it is here to handle the different format switch used in the STL code
enum chars_format {
    FMT_SCIENTIFIC,
    FMT_FIXED,
    FMT_GENERAL
};

// SPDX-SnippetBegin
// SPDX-SnippetCopyrightText: Copyright (c) Microsoft Corporation.
// SPDX-SnippetCopyrightText: Copyright 2018 Ulf Adams
// SPDX-SnippetCopyrightText: Copyright (c) Microsoft Corporation. All rights reserved.
// SPDX-License-Identifier: (Apache-2.0 WITH LLVM-exception) AND BSL-1.0
//
// The license information in the original file is not
// clear on whether it should be AND or OR between
// "Apache 2.0 with LLVM-exception" and "Boost Software License 1.0".
// Therefore, just to be safe, an AND was chosen in the SPDX license
// identifier expression above.
// Library: STL
// Git repository: https://github.com/microsoft/STL
// Commit: e745bad3b1d05b5b19ec652d68abb37865ffa454
// Original function: https://github.com/microsoft/STL/blob/e745bad3b1d05b5b19ec652d68abb37865ffa454/stl/inc/xcharconv_ryu.h#L1926
  

// This is inspired from the MS STL Charconv, under Apache with LLVM exception licence
// see https://github.com/microsoft/STL/blob/main/LICENSE.txt
// The inspiration is at https://github.com/microsoft/STL/blob/e745bad3b1d05b5b19ec652d68abb37865ffa454/stl/inc/xcharconv_ryu.h#L1926
// CHANGE_FOR_ERLANG all the types and typecast have been adapted to C types from Cpp.
// I have also kept the Ryu original function head as it allows to not impact the rest of the code
// __v and __mantissa and __exponent have lost their double underscore over the whole function
// all the test on the lenght of the buffer have been dropped too. This could need change, but
// we always pass a 256 bytes buffer when we only need 26 bytes maximum.
static inline int to_chars(const floating_decimal_64 v, const bool sign, char* const result) {
  // Step 5: Print the decimal representation.
  uint64_t __output = v.mantissa;
  int32_t _Ryu_exponent = v.exponent;
  const uint32_t __olength = decimalLength17(__output);
  int32_t _Scientific_exponent = _Ryu_exponent + ((int32_t) __olength) - 1;

  // CHANGE_FOR_ERLANG: we use our chars_format instead of the STL one
  enum chars_format _Fmt;

  int32_t _Lower;
  int32_t _Upper;

  if (__olength == 1) {
    // CHANGE_FOR_ERLANG the format and examples have been adapted to the erlang format
    // as the original would have not shown a change in format
    // (erlang always add ".0" to scientific format) and omit the + in the exponent
    // Value | Fixed    | Scientific
    // 1e-4  | "0.0001" | "1.0e-4"
    // 1e2   | "100.0"  | "1.0e2"
    // CHANGE_FOR_ERLANG the values for a switch, as seen in the example above, for erlang
    // are different than for STL format.
    _Lower = -4;
    _Upper = 2;
  } else if (_Scientific_exponent >= 10) {
    // CHANGE_FOR_ERLANG This case does not exist for the STL and is due to the 
    // negative sign in the exponent.
    // Value        | Fixed           | Scientific
    // 123456789e1  | "1234567890.0"  | "1.23456789e9"
    // 123456789e2  | "12345678900.0" | "1.23456789e10"

    _Lower = - (int32_t) (__olength + 2);
    _Upper = 2;
  } else {
    // CHANGE_FOR_ERLANG the format and examples have been adapted to the erlang format
    // as the original would have not shown a change in format
    // (erlang always add ".0" to scientific format) and omit the + in the exponent
    // Value   | Fixed      | Scientific
    // 1234e-6 | "0.001234" | "1.234e-4"
    // 1234e1  | "12340.0"  | "1.234e4"
    // CHANGE_FOR_ERLANG the values for a switch, as seen in the example above, for erlang
    // are different than for STL format.
    _Lower = - (int32_t) (__olength + 2);
    _Upper = 1;
  }

  if (_Lower <= _Ryu_exponent && _Ryu_exponent <= _Upper) {
    // CHANGE_FOR_ERLANG this is added to handle the -2**53, 2**53 range special case
    // These are edge cases not captured above, all the other are naturally handled
    // by _Lower nad _Upper
    if ((__output >= (1ull << 53) && _Ryu_exponent == 0) 
          || (__output > ((1ull << 52) / 5) && _Ryu_exponent == 1)
          || (__output > ((1ull << 51) / 25) && _Ryu_exponent == 2)) {
      _Fmt = FMT_SCIENTIFIC;
    } else {
      _Fmt = FMT_FIXED;
    }
  } else {
    // CHANGE_FOR_ERLANG we do not need to handle the %g case here.
    _Fmt = FMT_SCIENTIFIC;
  }

  // CHANGE_FOR_ERLANG we handle the sign here as it is handled outside of this in the STL case
  // and we need it to compute the start of the buffer for the characters after
  if (sign) {
    result[0] = '-';
  }

  // CHANGE_FOR_ERLANG we compute the start of the usable buffer. It is done here
  // in order to be fixed for both branches of formatting.
  char* const __result = result + sign;

  if (_Fmt == FMT_FIXED) {
    // CHANGE_FOR_ERLANG this whole table has been adapted to erlang examples to help
    // debug and evolve the edge cases
    // Example: __output == 1729, __olength == 4

    // _Ryu_exponent | Printed  | _Whole_digits | _Total_fixed_length  | Notes
    // --------------|----------|---------------|----------------------|---------------------------------------
    //             1 | 17290.0  |  5            |  _Whole_digits + 2   | Unified length cases.
    //             0 | 1729.0   |  4            |                      |
    // --------------|----------|---------------|----------------------|---------------------------------------
    //            -1 | 172.9    |  3            | __olength + 1        | This case can't happen for
    //            -2 | 17.29    |  2            |                      | __olength == 1, but no additional
    //            -3 | 1.729    |  1            |                      | code is needed to avoid it.
    // --------------|----------|---------------|----------------------|---------------------------------------
    //            -4 | 0.1729   |  0            | 2 - _Ryu_exponent    | If the decimal point appears, we need
    //            -5 | 0.01729  | -1            |                      | to put the "0" in front
    //            -6 | 0.001729 | -2            |                      |

    const int32_t _Whole_digits = (int32_t) (__olength) + _Ryu_exponent;

    uint32_t _Total_fixed_length;
    if (_Ryu_exponent >= 0) { 
      // CHANGE_FOR_ERLANG the examples and values have been adapted to erlang format one
      // CHANGE_FOR_ERLANG we also dropped the whole adjustement, as it is only of value
      // for %f which we do not handle
      // cases "17290.0" and "1729.0"
      _Total_fixed_length = (uint32_t) (_Whole_digits) + 2;
    } else if (_Whole_digits > 0) { // case "17.29"
      _Total_fixed_length = __olength + 1;
    } else { // case "0.001729"
      _Total_fixed_length = (uint32_t) (2 - _Ryu_exponent);
    }

    char* _Mid;
    if (_Ryu_exponent >= 0) { // case "172900.0"
      // CHANGE_FOR_ERLANG we do not need the can_use_ryu, as we are not doing %f
      // but always shortest round_trip. The whole complexity here is dropped
      // Print the decimal digits, left-aligned within [result, result + _Total_fixed_length).
      _Mid = __result + __olength;
    } else { // cases "1729.0", "17.29", and "0.001729"
      // Print the decimal digits, right-aligned within [result, result + _Total_fixed_length).
      _Mid = __result + _Total_fixed_length;
    }

    // We prefer 32-bit operations, even on 64-bit platforms.
    // We have at most 17 digits, and uint32_t can store 9 digits.
    // If __output doesn't fit into uint32_t, we cut off 8 digits,
    // so the rest will fit into uint32_t.
    // CHANGE_FOR_ERLANG we consider in this whole thing that memcopy use the same
    // char has defined in the DIGIT_TABLE
    // CHANGE_FOR_ERLANG __DIGIT_TABLE became DIGIT_TABLE
    if ((__output >> 32) != 0) {
      // Expensive 64-bit division.
      const uint64_t __q = div1e8(__output);
      uint32_t __output2 = (uint32_t) (__output - 100000000 * __q);
      __output = __q;

      const uint32_t __c = __output2 % 10000;
      __output2 /= 10000;
      const uint32_t __d = __output2 % 10000;
      const uint32_t __c0 = (__c % 100) << 1;
      const uint32_t __c1 = (__c / 100) << 1;
      const uint32_t __d0 = (__d % 100) << 1;
      const uint32_t __d1 = (__d / 100) << 1;

      memcpy(_Mid -= 2, DIGIT_TABLE + __c0, 2);
      memcpy(_Mid -= 2, DIGIT_TABLE + __c1, 2);
      memcpy(_Mid -= 2, DIGIT_TABLE + __d0, 2);
      memcpy(_Mid -= 2, DIGIT_TABLE + __d1, 2);
    }
    uint32_t __output2 = (uint32_t) __output;
    while (__output2 >= 10000) {
#ifdef __clang__ // TRANSITION, LLVM-38217
      const uint32_t __c = __output2 - 10000 * (__output2 / 10000);
#else
      const uint32_t __c = __output2 % 10000;
#endif
      __output2 /= 10000;
      const uint32_t __c0 = (__c % 100) << 1;
      const uint32_t __c1 = (__c / 100) << 1;
      memcpy(_Mid -= 2, DIGIT_TABLE + __c0, 2);
      memcpy(_Mid -= 2, DIGIT_TABLE + __c1, 2);
    }
    if (__output2 >= 100) {
      const uint32_t __c = (__output2 % 100) << 1;
      __output2 /= 100;
      memcpy(_Mid -= 2, DIGIT_TABLE + __c, 2);
    }
    if (__output2 >= 10) {
      const uint32_t __c = __output2 << 1;
      memcpy(_Mid -= 2, DIGIT_TABLE + __c, 2);
    } else {
      *--_Mid = (char) ('0' + __output2);
    }

    if (_Ryu_exponent > 0) { // case "172900.0"
      // Performance note: it might be more efficient to do this immediately after setting _Mid.
      // CHANGE_FOR_ERLANG we have different case here, so we have to add the ".0" here
      // we use memset as we do not have access to fill_n
      memset(__result + __olength, '0', (size_t) _Ryu_exponent);
      __result[__olength + (size_t) _Ryu_exponent] = '.';
      __result[__olength + (size_t) _Ryu_exponent + 1] = '0';
    } else if (_Ryu_exponent == 0) { // case "1729.0"
      // CHANGE_FOR_ERLANG we have different case here, so we have to add the ".0" here
      __result[__olength] = '.';
      __result[__olength + 1] = '0';
    } else if (_Whole_digits > 0) { // case "17.29"
      // Performance note: moving digits might not be optimal.
      memmove(__result, __result + 1, (size_t) _Whole_digits);
      __result[_Whole_digits] = '.';
    } else { // case "0.001729"
      // CHANGE_FOR_ERLANG we use the memset here as we do not have access to fill_n
      // Performance note: a larger memset() followed by overwriting '.' might be more efficient.
      __result[0] = '0';
      __result[1] = '.';
      memset(__result + 2, '0', (size_t) (-_Whole_digits));
    }

    // CHANGE_FOR_ERLANG we do not need the errc and we are only interested in
    // returning the length, as it is what Ryu and erlang expect. We do add the
    // sign as we did it here instead of adding it by default as in the STL
    return _Total_fixed_length + sign;
  }

  uint32_t _Scientific_exponent_length;
  // CHANGE_FOR_ERLANG we have to do a little bit more complex logic here because we do not always
  // print the exponent sign, only if it is negative
  if (_Scientific_exponent <= -100) { // "e-100"
    _Scientific_exponent_length = 5;
  } else if (_Scientific_exponent <= -10 || _Scientific_exponent >= 100) { // "e-10" or "e100"
    _Scientific_exponent_length = 4;
  } else if ((_Scientific_exponent > -10 && _Scientific_exponent < 0) || _Scientific_exponent >= 10) { // "e-9" or "e10"
    _Scientific_exponent_length = 3;
  } else { // "e1"
    _Scientific_exponent_length = 2;
  }

  // CHANGE_FOR_ERLANG we do not need the ternary as we did all the logic above
  const uint32_t _Total_scientific_length = __olength + 1 +(__olength == 1) // digits + decimal point + possible 0 after decimal point
    + _Scientific_exponent_length; // + scientific exponent

  // Print the decimal digits.
  uint32_t __i = 0;
  // We prefer 32-bit operations, even on 64-bit platforms.
  // We have at most 17 digits, and uint32_t can store 9 digits.
  // If __output doesn't fit into uint32_t, we cut off 8 digits,
  // so the rest will fit into uint32_t.
    // CHANGE_FOR_ERLANG we consider in this whole thing that memcopy use the same
    // char has defined in the DIGIT_TABLE
    // CHANGE_FOR_ERLANG __DIGIT_TABLE became DIGIT_TABLE
  if ((__output >> 32) != 0) {
    // Expensive 64-bit division.
    const uint64_t __q = div1e8(__output);
    uint32_t __output2 = (uint32_t) (__output) - 100000000 * (uint32_t) (__q);
    __output = __q;

    const uint32_t __c = __output2 % 10000;
    __output2 /= 10000;
    const uint32_t __d = __output2 % 10000;
    const uint32_t __c0 = (__c % 100) << 1;
    const uint32_t __c1 = (__c / 100) << 1;
    const uint32_t __d0 = (__d % 100) << 1;
    const uint32_t __d1 = (__d / 100) << 1;
    memcpy(__result + __olength - __i - 1, DIGIT_TABLE + __c0, 2);
    memcpy(__result + __olength - __i - 3, DIGIT_TABLE + __c1, 2);
    memcpy(__result + __olength - __i - 5, DIGIT_TABLE + __d0, 2);
    memcpy(__result + __olength - __i - 7, DIGIT_TABLE + __d1, 2);
    __i += 8;
  }
  uint32_t __output2 = (uint32_t) (__output);
  while (__output2 >= 10000) {
#ifdef __clang__ // TRANSITION, LLVM-38217
    const uint32_t __c = __output2 - 10000 * (__output2 / 10000);
#else
    const uint32_t __c = __output2 % 10000;
#endif
    __output2 /= 10000;
    const uint32_t __c0 = (__c % 100) << 1;
    const uint32_t __c1 = (__c / 100) << 1;
    memcpy(__result + __olength - __i - 1, DIGIT_TABLE + __c0, 2);
    memcpy(__result + __olength - __i - 3, DIGIT_TABLE + __c1, 2);
    __i += 4;
  }
  if (__output2 >= 100) {
    const uint32_t __c = (__output2 % 100) << 1;
    __output2 /= 100;
    memcpy(__result + __olength - __i - 1, DIGIT_TABLE + __c, 2);
    __i += 2;
  }
  if (__output2 >= 10) {
    const uint32_t __c = __output2 << 1;
    // We can't use memcpy here: the decimal dot goes between these two digits.
    __result[2] = DIGIT_TABLE[__c + 1];
    __result[0] = DIGIT_TABLE[__c];
  } else {
    __result[0] = (char) ('0' + __output2);
  }

  // Print decimal point if needed.
  uint32_t __index;
  if (__olength > 1) {
    __result[1] = '.';
    __index = __olength + 1;
  } else {
    // In erlang we _have_ to print the ".0" in the case this is an integer
    __result[1] = '.';
    __result[2] = '0';
    __index = __olength + 2;
  }

  // Print the exponent.
  __result[__index++] = 'e';
  if (_Scientific_exponent < 0) {
    __result[__index++] = '-';
    _Scientific_exponent = -_Scientific_exponent;
  }
  // CHANGE_FOR_ERLANG no else, as we do not print the positive sign on the exponent

  if (_Scientific_exponent >= 100) {
    const int32_t __c = _Scientific_exponent % 10;
    memcpy(__result + __index, DIGIT_TABLE + 2 * (_Scientific_exponent / 10), 2);
    __result[__index + 2] = (char) ('0' + __c);
    __index += 3;
  } else if (_Scientific_exponent >= 10) {
    // CHANGE_FOR_ERLANG we have to do this only if the exponent is larger than 10
    memcpy(__result + __index, DIGIT_TABLE + 2 * _Scientific_exponent, 2);
    __index += 2;
  } else { 
  // CHANGE_FOR_ERLANG we can have an exponent under 10, which is not handled by the table
  // so we handle it here
    __result[__index++] = (char) ('0' + _Scientific_exponent);
  }

    // CHANGE_FOR_ERLANG we do not need the errc and we are only interested in
    // returning the length, as it is what Ryu and erlang expect. We do add the
    // sign as we did it here instead of adding it by default as in the STL
  return _Total_scientific_length + sign;
}
// end of STL code, back to ryu
// SPDX-SnippetEnd


static inline bool d2d_small_int(const uint64_t ieeeMantissa, const uint32_t ieeeExponent,
  floating_decimal_64* const v) {
  const uint64_t m2 = (1ull << DOUBLE_MANTISSA_BITS) | ieeeMantissa;
  const int32_t e2 = (int32_t) ieeeExponent - DOUBLE_BIAS - DOUBLE_MANTISSA_BITS;

  if (e2 > 0) {
    // f = m2 * 2^e2 >= 2^53 is an integer.
    // Ignore this case for now.
    return false;
  }

  if (e2 < -52) {
    // f < 1.
    return false;
  }

  // Since 2^52 <= m2 < 2^53 and 0 <= -e2 <= 52: 1 <= f = m2 / 2^-e2 < 2^53.
  // Test if the lower -e2 bits of the significand are 0, i.e. whether the fraction is 0.
  const uint64_t mask = (1ull << -e2) - 1;
  const uint64_t fraction = m2 & mask;
  if (fraction != 0) {
    return false;
  }

  // f is an integer in the range [1, 2^53).
  // Note: mantissa might contain trailing (decimal) 0's.
  // Note: since 2^53 < 10^16, there is no need to adjust decimalLength17().
  v->mantissa = m2 >> -e2;
  v->exponent = 0;
  return true;
}

int d2s_buffered_n(double f, char* result) {
  // Step 1: Decode the floating-point number, and unify normalized and subnormal cases.
  const uint64_t bits = double_to_bits(f);

#ifdef RYU_DEBUG
  printf("IN=");
  for (int32_t bit = 63; bit >= 0; --bit) {
    printf("%d", (int) ((bits >> bit) & 1));
  }
  printf("\n");
#endif

  // Decode bits into sign, mantissa, and exponent.
  const bool ieeeSign = ((bits >> (DOUBLE_MANTISSA_BITS + DOUBLE_EXPONENT_BITS)) & 1) != 0;
  const uint64_t ieeeMantissa = bits & ((1ull << DOUBLE_MANTISSA_BITS) - 1);
  const uint32_t ieeeExponent = (uint32_t) ((bits >> DOUBLE_MANTISSA_BITS) & ((1u << DOUBLE_EXPONENT_BITS) - 1));
  // Case distinction; exit early for the easy cases.
  if (ieeeExponent == ((1u << DOUBLE_EXPONENT_BITS) - 1u) || (ieeeExponent == 0 && ieeeMantissa == 0)) {
    return copy_special_str(result, ieeeSign, ieeeExponent, ieeeMantissa);
  }

  floating_decimal_64 v;
  const bool isSmallInt = d2d_small_int(ieeeMantissa, ieeeExponent, &v);
  if (isSmallInt) {
    // For small integers in the range [1, 2^53), v.mantissa might contain trailing (decimal) zeros.
    // For scientific notation we need to move these zeros into the exponent.
    // (This is not needed for fixed-point notation, so it might be beneficial to trim
    // trailing zeros in to_chars only if needed - once fixed-point notation output is implemented.)
    for (;;) {
      const uint64_t q = div10(v.mantissa);
      const uint32_t r = ((uint32_t) v.mantissa) - 10 * ((uint32_t) q);
      if (r != 0) {
        break;
      }
      v.mantissa = q;
      ++v.exponent;
    }
  } else {
    v = d2d(ieeeMantissa, ieeeExponent);
  }

  return to_chars(v, ieeeSign, result);
}

void d2s_buffered(double f, char* result) {
  const int index = d2s_buffered_n(f, result);

  // Terminate the string.
  result[index] = '\0';
}

char* d2s(double f) {
  char* const result = (char*) malloc(25);
  d2s_buffered(f, result);
  return result;
}
