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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef ERTS_USE_BUILTIN_RYU
# error "ERTS_USE_BUILTIN_RYU should not be defined here"
#endif

#include <charconv>
#include <string.h>
#include <math.h>
#include <ctype.h>
extern "C"
{
#include "sys.h"
}

extern "C"
int sys_double_to_chars_short(double f, char* fbuf, int sizeof_fbuf)
{
    std::to_chars_result tcr;

    if (fabs(f) < 9007199254740992.0) {
        tcr = std::to_chars(fbuf, fbuf + sizeof_fbuf -1, f);
    }
    else {
        tcr = std::to_chars(fbuf, fbuf + sizeof_fbuf -1, f,
                            std::chars_format::scientific);
    }

    if (tcr.ec != std::errc()) {
        return 0;
    }


    /* The output from std::to_chars() differ from what we want:
     *
     * 1. Integer fixed floats are missing trailing ".0"          Ex: "12345"
     * 2. Single digit significands are missing ".0" before "e"   Ex: "1e-10"
     * 3. Positive exponents have a redundant '+'                 Ex: "1.2e+10"
     * 4. Single digit exponents have a redundant '0'             Ex: "1.2e-09"
     *
     * Any combinations of 2-4 are possible.
     *
     * To make things more complicated, the above differences in output can
     * change which format is the shortest. So we sometimes have to change
     * from fixed to scientific format or vice versa.
     *
     * The conversion logic that follows is pure string manipulation.
     * We rely heavily on to_chars() giving us the right amount of significant
     * digits AND in the shortest output format as described above.
     */

    char* end = tcr.ptr;
    char* p = fbuf;

    if (*p == '-') {
        p++;
    }
    ASSERT(isdigit(*p));

    if (end - p > 4) {
        char *e = end - 4;

        if (*e != 'e') {
            --e;
            if (*e != 'e') {
                goto fixed;
            }
        }
        /*
         * Scientific form
         */
        ASSERT(*e == 'e');
        if (e == p+1) {
            if (memcmp(e+1, "-04", 3) == 0) {
                // Convert from Xe-04 to 0.000X
                p[5] = p[0];
                memcpy(p, "0.000", 5);
                end++;
                goto done;
            }
            // Insert ".0" before 'e'
            memmove(e+2, e, end - e);
            *e++ = '.';
            *e++ = '0';
            end += 2;
        }

        /*
         * Simplify X.XXe+0Y to X.XXeY
         */
        e++;
        if (*e == '-') {
            e++;
            p = e;
        }
        else {
            ASSERT(*e == '+');
            p = e + 1;
        }
        ASSERT(isdigit(*p));
        if (*p == '0') {
            p++;
        }
        if (p != e) {
            memmove(e, p, end - p);
            end -= p - e;
        }
    }
    else {
    fixed:
        /*
         * Fixed form
         */
        if (end - p > 3 && end[-1] == '0' && end[-2] == '0'
            && (end - p < 11 || end[-3] == '0')) {
            /*
             * Convert from XXXXXX00 to X.XXXXXeYY
             */
            const int exp = (end - p) - 1;

            for (end -= 3; *end == '0'; end--) {
                ASSERT(end > p);
            }
            if (end == p) {
                end++; // Keep one zero as decimal
            }
            memmove(p + 2, p + 1, (end - p));
            p[1] = '.';
            end += 2;
            end[0] = 'e';
            if (exp < 10) {
                end[1] = '0' + exp;
                end += 2;
            }
            else {
                end[1] = '0' + (exp / 10);
                end[2] = '0' + exp % 10;
                end += 3;
            }
        }
        else if (memcmp(p, "0.000", 5) == 0) {
            /*
             * Convert from 0.000XXX to X.XXe-4
             */
            ASSERT(end - p >= 7);
            ASSERT(p[5] != '0');
            p[0] = p[5];
            p[1] = '.';
            memmove(p+2, p+6, (end - p) - 6);
            end--;
            memcpy(end-3, "e-4", 3);
        }
        else if (!memchr(p, '.', end - p)) {
            /*
             * Append ".0" to integers
             */
            end[0] = '.';
            end[1] = '0';
            end += 2;
        }
    }

done:
    *end = '\0';
    return end - fbuf;
}
