/*
 * C-implementation of Float16 - https://github.com/Maratyszcza/FP16
 *
 * Version: 0a92994d729ff76a58f692d3028ca1b64b145d91
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Facebook Inc.
 * Copyright (c) 2017 Georgia Institute of Technology
 * Copyright 2019 Google LLC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this
 * software and associated documentation files (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
 * FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/*
 * The following changes were done to adapt this to Erlang/OTP conventions:
 *
 * 1. uint16_t and uint32_t have been rewritten to Uint16 and Uint32
 * 2. Mixed declarations were moved to the top to avoid warnings
 * 3. inline was rewritten as ERTS_INLINE
 * 4. UINT16_C(x) and UINT32_C(x) were rewritten to xU as we don't support 16bits platform
 */

static ERTS_INLINE float fp32_from_bits(Uint32 w) {
    union {
	Uint32 as_bits;
	float as_value;
    } fp32 = { w };
    return fp32.as_value;
}

static ERTS_INLINE Uint32 fp32_to_bits(float f) {
    union {
	float as_value;
	Uint32 as_bits;
    } fp32 = { f };
    return fp32.as_bits;
}

/*
 * Convert a 32-bit floating-point number in IEEE single-precision format to a 16-bit floating-point number in
 * IEEE half-precision format, in bit representation.
 *
 * @note The implementation relies on IEEE-like (no assumption about rounding mode and no operations on denormals)
 * floating-point operations and bitcasts between integer and floating-point variables.
 */
static ERTS_INLINE Uint16 fp16_ieee_from_fp32_value(float f) {
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) || defined(__GNUC__) && !defined(__STRICT_ANSI__)
    const float scale_to_inf = 0x1.0p+112f;
    const float scale_to_zero = 0x1.0p-110f;
#else
    const float scale_to_inf = fp32_from_bits(0x77800000U);
    const float scale_to_zero = fp32_from_bits(0x08800000U);
#endif
    float base = (fabsf(f) * scale_to_inf) * scale_to_zero;

    const Uint32 w = fp32_to_bits(f);
    const Uint32 shl1_w = w + w;
    const Uint32 sign = w & 0x80000000U;

    Uint32 bits, exp_bits, mantissa_bits, nonsign, bias;

    bias = shl1_w & 0xFF000000U;

    if (bias < 0x71000000U) {
	bias = 0x71000000U;
    }

    base = fp32_from_bits((bias >> 1) + 0x07800000U) + base;
    bits = fp32_to_bits(base);
    exp_bits = (bits >> 13) & 0x00007C00U;
    mantissa_bits = bits & 0x00000FFFU;
    nonsign = exp_bits + mantissa_bits;
    return (sign >> 16) | (shl1_w > 0xFF000000U ? 0x7E00U : nonsign);
}

/*
 * Convert a 16-bit floating-point number in IEEE half-precision format, in bit representation, to
 * a 32-bit floating-point number in IEEE single-precision format.
 *
 * @note The implementation relies on IEEE-like (no assumption about rounding mode and no operations on denormals)
 * floating-point operations and bitcasts between integer and floating-point variables.
 */
static ERTS_INLINE float fp16_ieee_to_fp32_value(Uint16 h) {
    /*
     * Extend the half-precision floating-point number to 32 bits and shift to the upper part of the 32-bit word:
     *      +---+-----+------------+-------------------+
     *      | S |EEEEE|MM MMMM MMMM|0000 0000 0000 0000|
     *      +---+-----+------------+-------------------+
     * Bits  31  26-30    16-25            0-15
     *
     * S - sign bit, E - bits of the biased exponent, M - bits of the mantissa, 0 - zero bits.
     */
    const Uint32 w = (Uint32) h << 16;
    /*
     * Extract the sign of the input number into the high bit of the 32-bit word:
     *
     *      +---+----------------------------------+
     *      | S |0000000 00000000 00000000 00000000|
     *      +---+----------------------------------+
     * Bits  31                 0-31
     */
    const Uint32 sign = w & 0x80000000U;
    /*
     * Extract mantissa and biased exponent of the input number into the high bits of the 32-bit word:
     *
     *      +-----+------------+---------------------+
     *      |EEEEE|MM MMMM MMMM|0 0000 0000 0000 0000|
     *      +-----+------------+---------------------+
     * Bits  27-31    17-26            0-16
     */
    const Uint32 two_w = w + w;

    /*
     * Shift mantissa and exponent into bits 23-28 and bits 13-22 so they become mantissa and exponent
     * of a single-precision floating-point number:
     *
     *       S|Exponent |          Mantissa
     *      +-+---+-----+------------+----------------+
     *      |0|000|EEEEE|MM MMMM MMMM|0 0000 0000 0000|
     *      +-+---+-----+------------+----------------+
     * Bits   | 23-31   |           0-22
     *
     * Next, there are some adjustments to the exponent:
     * - The exponent needs to be corrected by the difference in exponent bias between single-precision and half-precision
     *   formats (0x7F - 0xF = 0x70)
     * - Inf and NaN values in the inputs should become Inf and NaN values after conversion to the single-precision number.
     *   Therefore, if the biased exponent of the half-precision input was 0x1F (max possible value), the biased exponent
     *   of the single-precision output must be 0xFF (max possible value). We do this correction in two steps:
     *   - First, we adjust the exponent by (0xFF - 0x1F) = 0xE0 (see exp_offset below) rather than by 0x70 suggested
     *     by the difference in the exponent bias (see above).
     *   - Then we multiply the single-precision result of exponent adjustment by 2**(-112) to reverse the effect of
     *     exponent adjustment by 0xE0 less the necessary exponent adjustment by 0x70 due to difference in exponent bias.
     *     The floating-point multiplication hardware would ensure than Inf and NaN would retain their value on at least
     *     partially IEEE754-compliant implementations.
     *
     * Note that the above operations do not handle denormal inputs (where biased exponent == 0). However, they also do not
     * operate on denormal inputs, and do not produce denormal results.
     */
    const Uint32 exp_offset = 0xE0U << 23;
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) || defined(__GNUC__) && !defined(__STRICT_ANSI__)
    const float exp_scale = 0x1.0p-112f;
#else
    const float exp_scale = fp32_from_bits(0x7800000U);
#endif
    const float normalized_value = fp32_from_bits((two_w >> 4) + exp_offset) * exp_scale;

    /*
     * Convert denormalized half-precision inputs into single-precision results (always normalized).
     * Zero inputs are also handled here.
     *
     * In a denormalized number the biased exponent is zero, and mantissa has on-zero bits.
     * First, we shift mantissa into bits 0-9 of the 32-bit word.
     *
     *                  zeros           |  mantissa
     *      +---------------------------+------------+
     *      |0000 0000 0000 0000 0000 00|MM MMMM MMMM|
     *      +---------------------------+------------+
     * Bits             10-31                0-9
     *
     * Now, remember that denormalized half-precision numbers are represented as:
     *    FP16 = mantissa * 2**(-24).
     * The trick is to construct a normalized single-precision number with the same mantissa and thehalf-precision input
     * and with an exponent which would scale the corresponding mantissa bits to 2**(-24).
     * A normalized single-precision floating-point number is represented as:
     *    FP32 = (1 + mantissa * 2**(-23)) * 2**(exponent - 127)
     * Therefore, when the biased exponent is 126, a unit change in the mantissa of the input denormalized half-precision
     * number causes a change of the constructud single-precision number by 2**(-24), i.e. the same amount.
     *
     * The last step is to adjust the bias of the constructed single-precision number. When the input half-precision number
     * is zero, the constructed single-precision number has the value of
     *    FP32 = 1 * 2**(126 - 127) = 2**(-1) = 0.5
     * Therefore, we need to subtract 0.5 from the constructed single-precision number to get the numerical equivalent of
     * the input half-precision number.
     */
    const Uint32 magic_mask = 126U << 23;
    const float magic_bias = 0.5f;
    const float denormalized_value = fp32_from_bits((two_w >> 17) | magic_mask) - magic_bias;

    /*
     * - Choose either results of conversion of input as a normalized number, or as a denormalized number, depending on the
     *   input exponent. The variable two_w contains input exponent in bits 27-31, therefore if its smaller than 2**27, the
     *   input is either a denormal number, or zero.
     * - Combine the result of conversion of exponent and mantissa with the sign of the input number.
     */
    const Uint32 denormalized_cutoff = 1U << 27;
    const Uint32 result = sign |
	(two_w < denormalized_cutoff ? fp32_to_bits(denormalized_value) : fp32_to_bits(normalized_value));
    return fp32_from_bits(result);
}
