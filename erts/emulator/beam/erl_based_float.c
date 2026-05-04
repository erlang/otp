/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * For based float conversion:
 *   BIFs: list_to_float/2, binary_to_float/2
 *   Helper functions for float_to_list/2, float_to_binary/2
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <math.h>
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "bif.h"
#include "erl_binary.h"
#include "erl_based_float.h"

/* Convert a char to its numeric value in the given base.
 * Return -1 if the value is invalid in the given base.
 */
static int digit_val(char c, int base)
{
    int d;
    if (c >= '0' && c <= '9')
        d = c - '0';
    else if (c >= 'a' && c <= 'z')
        d = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z')
        d = c - 'A' + 10;
    else
        return -1;
    return (d < base) ? d : -1;
}

/*
 * Given a string representation of a float and its base,
 * convert it into a C double.
 * The string must contain a dot, can optionally contain a sign
 * and an exponent.
 * Return true on success and false on failure.
 */
static bool based_charbuf_to_float(char *buf, int len, int base, double *result)
{
    int i = 0;
    int negative = 0;
    int d;
    int frac_len = 0;
    int ndigits = 0;
    int exp = 0;
    double N = 0.0;
    double F = 0.0;

    /* Optional sign */
    if (buf[i] == '-') {
        negative = 1;
        i++;
    } else if (buf[i] == '+') {
        i++;
    }

    /* Integer part. Skip underscore. Stop at dot. */
    while (i < len && buf[i] != '.') {
        if (buf[i] == '_') {
            i++;
            continue;
        }
        d = digit_val(buf[i], base);
        if (d < 0)
            return false;
        N = N * base + d;
        i++, ndigits++;
    }

    if (!erts_isfinite(N))
        return false;

    /* Error if no dot */
    if (i >= len || buf[i] != '.')
        return false;
    i++;

    /* Fractional part. Skip underscore. Stop at '#' or end */
    while (i < len && buf[i] != '#') {
        if (buf[i] == '_') {
            i++;
            continue;
        }
        d = digit_val(buf[i], base);
        if (d < 0)
            return false;
        if (frac_len < 60) {
            F = F * base + d;
            frac_len++;
        }
        i++;
    }

    /* Optional exponent: #e±decimal or #E±decimal */
    if (i < len && buf[i] == '#') {
        int negative_exp = 0;
        i++;
        if (i >= len || (buf[i] != 'e' && buf[i] != 'E'))
            return false;
        i++;
        if (i < len && buf[i] == '+') {
            i++;
        } else if (i < len && buf[i] == '-') {
            negative_exp = 1;
            i++;
        }
        if (i >= len || buf[i] < '0' || buf[i] > '9')
            return false;
        while (i < len) {
            if (buf[i] == '_') {
                i++;
                continue;
            }
            if (buf[i] < '0' || buf[i] > '9')
                return false;
            exp = exp * 10 + (buf[i] - '0');
            i++;
        }
        if (negative_exp) exp = -exp;
    }


    if (i != len)
        return false;

    /* Convert to base 10 using the same algorithm as erl_scan:
     * Collect all digits to create an integer N, then scale according
     * to dot position, exponent, and sign.
     */
    {
        double int_part;
        double frac_part;
        double val;

        int_part = N * pow((double)base, (double)exp);
        frac_part = F * pow((double)base, (double)exp-frac_len);
        val = int_part + frac_part;

        if (negative)
            val = -val;

        /* Reject infinity or NaN */
        if (!erts_isfinite(val))
            return false;

        *result = val;
    }

    return true;
}

/*
 * Convert a C double into an Erlang float and return as an Eterm.
 */
static Eterm make_float_term(Process *c_p, double val)
{
    FloatDef f;
    Eterm res;
    Eterm* hp;
    f.fd = val;
    hp = HAlloc(c_p, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    return res;
}

static int do_based_float_to_charbuf(Eterm efloat, struct erl_float_opts *opts,
                                     char *fbuf, int sizeof_fbuf)
{
    FloatDef f;
    double num, frac;

    enum erl_fmt_type fmt_type = opts->fmt_type;
    int base = opts->base;
    int decimals = opts->decimals;
    bool compact = opts->compact;

    char *p = fbuf;
    char int_digits[128];
    int int_len = 0;
    int i, d;

    GET_DOUBLE(efloat, f);

    if (!erts_isfinite(f.fd))
        return -1;

    if (signbit(f.fd)) {
        *p++ = '-';
        f.fd = -f.fd;
    }

    frac = modf(f.fd, &num);
    if (fmt_type == FMT_LEGACY || fmt_type == FMT_SCIENTIFIC) {
        /* One digit before dot */
        int exp = 0;
        double val = num + frac;

        if (val >= (double)base) {
            while (val >= (double)base) {
                val /= (double)base;
                exp++;
            }
        } else if (val > 0.0 && val < 1.0) {
            while (val < 1.0) {
                val *= (double)base;
                exp--;
            }
        }

        d = (int)val;
        if (d >= base) d = base - 1;
        *p++ = (d < 10) ? '0' + d : 'a' + d - 10;
        val -= d;

        *p++ = '.';
        for (i = 0; i < decimals; i++) {
            val *= base;
            d = (int)val;
            if (d >= base) d = base - 1;
            *p++ = (d < 10) ? '0' + d : 'a' + d - 10;
            val -= d;
        }

        if (compact) {
            while (p > fbuf + 2 && *(p - 1) == '0' && *(p - 2) != '.')
                p--;
        }

        /* Exponent in decimal */
        *p++ = '#';
        *p++ = 'e';
        if (exp < 0) {
            *p++ = '-';
            exp = -exp;
        }
        {
            char exp_buf[16];
            int elen = 0;
            if (exp == 0) {
                exp_buf[elen++] = '0';
            } else {
                while (exp > 0) {
                    exp_buf[elen++] = '0' + (exp % 10);
                    exp /= 10;
                }
            }
            for (i = elen - 1; i >= 0; i--)
                *p++ = exp_buf[i];
        }
    } else if (fmt_type == FMT_FIXED) {
        /* Normal decimal format */
        if (num == 0.0) {
            int_digits[int_len++] = '0';
        } else {
            unsigned long ip = (unsigned long)num;
            while (ip > 0) {
                d = ip % base;
                int_digits[int_len++] = (d < 10) ? '0' + d : 'a' + d - 10;
                ip /= base;
            }
        }
        for (i = int_len - 1; i >= 0; i--)
            *p++ = int_digits[i];

        *p++ = '.';
        for (i = 0; i < decimals; i++) {
            frac *= base;
            d = (int)frac;
            if (d >= base) d = base - 1;
            *p++ = (d < 10) ? '0' + d : 'a' + d - 10;
            frac -= d;
        }

        if (compact) {
            while (p > fbuf + 2 && *(p - 1) == '0' && *(p - 2) != '.')
                p--;
        }
    } else if (fmt_type == FMT_SHORT) {
        double val = num + frac;
        int max_digits;
        int exp = 0;
        char trial[256];
        int trial_len;
        double roundtrip;

        if (val == 0.0) {
            *p++ = '0';
            *p++ = '.';
            *p++ = '0';
            return (int)(p - fbuf);
        }

        /* Max significant digits: ceil(53 / log2(base)) */
        {
            static const int max_digits_table[] = {
                0, 0,       /* invalid bases: 0, 1 */
                54,         /* 2 */
                35, 28, 24, 22, 20, 19, 18, /* 3-9 */
                17, 17, 16, 16, 15, 15, 15, /* 10-16 */
                14, 14, 14, 14, 13, 13, 13, /* 17-23 */
                13, 13, 13, 12, 12, 12, 12, /* 24-30 */
                12, 12, 12, 12, 12, 12      /* 31-36 */
            };
            max_digits = max_digits_table[base];
        }

        /* Convert to scientific */
        if (val >= (double)base) {
            while (val >= (double)base) {
                val /= (double)base;
                exp++;
            }
        } else if (val > 0.0 && val < 1.0) {
            while (val < 1.0) {
                val *= (double)base;
                exp--;
            }
        }

        /* Generate max_digits significant digits */
        {
            char sci_buf[256];
            char *sp = sci_buf;
            double sv = val;
            int d, i;
            int sig_digits;
            int use_scientific;

            d = (int)sv;
            if (d >= base) d = base - 1;
            *sp++ = (d < 10) ? '0' + d : 'a' + d - 10;
            sv -= d;

            *sp++ = '.';
            for (i = 1; i < max_digits; i++) {
                sv *= base;
                d = (int)sv;
                if (d >= base) d = base - 1;
                *sp++ = (d < 10) ? '0' + d : 'a' + d - 10;
                sv -= d;
            }

            /* Trim digits to find shortest round-tripping representation */
            sig_digits = max_digits;
            while (sig_digits > 1) {
                /* Build trial with (sig_digits - 1) significant digits */
                char *tp = trial;
                int ti;

                trial[0] = sci_buf[0];
                tp = trial + 1;
                *tp++ = '.';

                /* fractional digits: at most (sig_digits - 2) */
                for (ti = 0; ti < sig_digits - 2; ti++) {
                    *tp++ = sci_buf[2 + ti];
                }
                if (sig_digits - 2 <= 0)
                    *tp++ = '0'; /* need at least one digit after dot */

                /* exponent */
                *tp++ = '#';
                *tp++ = 'e';
                if (exp < 0) {
                    *tp++ = '-';
                    ti = -exp;
                } else {
                    ti = exp;
                }
                if (ti == 0) {
                    *tp++ = '0';
                } else {
                    char ebuf[16];
                    int elen = 0;
                    while (ti > 0) {
                        ebuf[elen++] = '0' + (ti % 10);
                        ti /= 10;
                    }
                    for (ti = elen - 1; ti >= 0; ti--)
                        *tp++ = ebuf[ti];
                }
                trial_len = (int)(tp - trial);

                /* Round-trip test: try one fewer digit */
                if (based_charbuf_to_float(trial, trial_len, base, &roundtrip)
                    && roundtrip == (num + frac)) {
                    sig_digits--;
                } else {
                    break;
                }
            }


            if (sig_digits < 1) sig_digits = 1;

            /* Decide between scientific or normal format */
            use_scientific = 1;

            if ((num + frac) < 9007199254740992.0) { /* < 2^53 */
                /* Compare lengths of fixed vs scientific */
                int sci_len, fix_len;
                int fix_int_digits;

                /* Scientific length = sig_digits + 1(dot) + 2(#e) + exp_digits */
                sci_len = sig_digits + 1 + 2;
                {
                    int e = (exp >= 0) ? exp : -exp;
                    if (e == 0) sci_len += 1;
                    else { while (e > 0) { sci_len++; e /= 10; } }
                    if (exp < 0) sci_len++;
                }

                /* Normal length = int_digits + 1(dot) + frac_digits */
                if (exp >= 0) {
                    int fix_frac_digits;
                    fix_int_digits = exp + 1;
                    fix_frac_digits = sig_digits - fix_int_digits;
                    if (fix_frac_digits < 1) fix_frac_digits = 1;
                    fix_len = fix_int_digits + 1 + fix_frac_digits;
                } else {
                    /* 0.000...digits */
                    int leading_zeros;
                    fix_int_digits = 1; /* "0" */
                    leading_zeros = -exp - 1;
                    fix_len = 1 + 1 + leading_zeros + sig_digits; /* 0 . zeros digits */
                }

                if (fix_len <= sci_len)
                    use_scientific = 0;
            }

            /* Write output */
            if (use_scientific) {
                *p++ = sci_buf[0];
                *p++ = '.';
                for (i = 0; i < sig_digits - 1; i++)
                    *p++ = sci_buf[2 + i];
                if (sig_digits == 1)
                    *p++ = '0'; /* at least one digit after dot */
                *p++ = '#';
                *p++ = 'e';
                if (exp < 0) {
                    *p++ = '-';
                    exp = -exp;
                }
                if (exp == 0) {
                    *p++ = '0';
                } else {
                    char ebuf[16];
                    int elen = 0;
                    while (exp > 0) {
                        ebuf[elen++] = '0' + (exp % 10);
                        exp /= 10;
                    }
                    for (i = elen - 1; i >= 0; i--)
                        *p++ = ebuf[i];
                }
            } else {
                /* Normal decimal */
                if (exp >= 0) {
                    int written;
                    /* Integer part: first (exp+1) significant digits */
                    *p++ = sci_buf[0];
                    for (i = 0; i < exp && i < sig_digits - 1; i++)
                        *p++ = sci_buf[2 + i];
                    /* Pad with zeros if needed */
                    for (; i < exp; i++)
                        *p++ = '0';
                    *p++ = '.';
                    /* Fractional part: remaining significant digits */
                    written = 0;
                    for (; i < sig_digits - 1; i++) {
                        *p++ = sci_buf[2 + i];
                        written++;
                    }
                    if (written == 0)
                        *p++ = '0'; /* at least one digit after dot */
                } else {
                    /* Value < 1: "0." + 0s + significant digits */
                    *p++ = '0';
                    *p++ = '.';
                    for (i = 0; i < -exp - 1; i++)
                        *p++ = '0';
                    *p++ = sci_buf[0];
                    for (i = 0; i < sig_digits - 1; i++)
                        *p++ = sci_buf[2 + i];
                }
            }
        }

        return (int)(p - fbuf);
    }

    return (int)(p - fbuf);
}

BIF_RETTYPE erl_based_float_to_list(Process *BIF_P, Eterm efloat,
                                    struct erl_float_opts *opts)
{
    char fbuf[256];
    int used;
    Eterm* hp;

    used = do_based_float_to_charbuf(efloat, opts, fbuf, sizeof(fbuf));
    if (used <= 0)
        BIF_ERROR(BIF_P, BADARG);

    hp = HAlloc(BIF_P, (Uint)used * 2);
    BIF_RET(buf_to_intlist(&hp, fbuf, (Uint)used, NIL));
}

BIF_RETTYPE erl_based_float_to_binary(Process *BIF_P, Eterm efloat,
                                      struct erl_float_opts *opts)
{
    char fbuf[256];
    int used;

    used = do_based_float_to_charbuf(efloat, opts, fbuf, sizeof(fbuf));
    if (used <= 0)
        BIF_ERROR(BIF_P, BADARG);

    BIF_RET(erts_new_binary_from_data(BIF_P, (Uint)used, (byte*)fbuf));
}

BIF_RETTYPE list_to_float_2(BIF_ALIST_2)
{
    Sint len;
    SWord base;
    char *buf = NULL;
    double result;

    len = erts_list_length(BIF_ARG_1);
    if (len < 0)
        BIF_ERROR(BIF_P, BADARG);

    if (is_not_small(BIF_ARG_2))
        BIF_ERROR(BIF_P, BADARG);

    base = signed_val(BIF_ARG_2);
    if (base < 2 || base > 36)
        BIF_ERROR(BIF_P, BADARG);

    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, len + 1);

    if (intlist_to_buf(BIF_ARG_1, buf, len) < 0)
        goto badarg;
    buf[len] = '\0';

    if (base == 10) {
        Eterm res = do_charbuf_to_float(BIF_P, buf);
        erts_free(ERTS_ALC_T_TMP, (void *) buf);
        BIF_RET(res);
    }

    if (based_charbuf_to_float(buf, len, base, &result)) {
        erts_free(ERTS_ALC_T_TMP, (void *) buf);
        BIF_RET(make_float_term(BIF_P, result));
    }

badarg:
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE binary_to_float_2(BIF_ALIST_2)
{
    SWord base;
    Uint offset, size;
    byte *bin_base;

    if (is_not_bitstring(BIF_ARG_1) || is_not_small(BIF_ARG_2))
        BIF_ERROR(BIF_P, BADARG);

    base = signed_val(BIF_ARG_2);
    if (base < 2 || base > 36)
        BIF_ERROR(BIF_P, BADARG);

    ERTS_GET_BITSTRING(BIF_ARG_1, bin_base, offset, size);

    if (size > 0 && TAIL_BITS(size) == 0) {
        double result;

        if (base == 10) {
            byte *char_buf;
            Eterm res;

            char_buf = erts_alloc(ERTS_ALC_T_TMP, NBYTES(size) + 1);
            copy_binary_to_buffer(char_buf, 0, bin_base, offset, size);
            char_buf[NBYTES(size)] = '\0';

            res = do_charbuf_to_float(BIF_P, (char*)char_buf);
            erts_free(ERTS_ALC_T_TMP, (void*)char_buf);
            BIF_RET(res);
        } else if (based_charbuf_to_float((char*)bin_base + offset,
                                          NBYTES(size),
                                          base, &result)) {
            BIF_RET(make_float_term(BIF_P, result));
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}
