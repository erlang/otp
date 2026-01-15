// SPDX-CopyrightText: Copyright (c) Microsoft Corporation.
// SPDX-CopyrightText: Copyright 2018 Ulf Adams
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception AND BSL-1.0
//h
// The license information in the original file is not super clear.
// Microsoft seems to have changed it from Apache-2.0 to Apache-2.0 WITH LLVM-exception
// which they most likely are not allowed to do as the LLVM exception is less
// restrictive than Apache-2.0 plain. There is not much we can do about that
// though so we use their license.
//
// Library: STL
// Git repository: https://github.com/microsoft/STL
// Commit: e745bad3b1d05b5b19ec652d68abb37865ffa454
// Original function: https://github.com/microsoft/STL/blob/e745bad3b1d05b5b19ec652d68abb37865ffa454/stl/inc/xcharconv_ryu.h#L1926

//CHANGE_FOR_ERLANG: This format is new, it is here to handle the different format switch used in the STL code
enum chars_format {
    FMT_SCIENTIFIC,
    FMT_FIXED,
    FMT_GENERAL
};  

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
