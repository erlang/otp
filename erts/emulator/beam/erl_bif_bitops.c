/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2018-2026. All Rights Reserved.
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

#include "sys.h"
#include "export.h"
#include "bif.h"
#include "big.h"
#include <string.h>
#include <stdio.h>



/* Bit operations exposed as BIFs: erlang:ctz/1, erlang:popcount/1
 *
 * For 64-bit inputs (small/immediate integers): deterministic 64-bit semantics.
 * For bignums: operate on the arbitrary-precision bit representation.
 *
 * Performance optimization opportunities for bignums:
 * - popcount/1: can use SIMD (e.g., AVX-512 VPOPCNTQ) to process multiple limbs
 * - ctz/1: requires sequential scan for lowest non-zero limb, then scalar ctz
 */

/* ctz/1: count trailing zeros in integer (small integers or bignums) */
BIF_RETTYPE ctz_1(BIF_ALIST_1)
{
    Eterm result = erts_ctz(BIF_P, BIF_ARG_1);
    if (!is_value(result)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(result);
}

/* popcount/1: count set bits in integer (small integers or bignums) */
BIF_RETTYPE popcount_1(BIF_ALIST_1)
{
    Eterm result = erts_popcount(BIF_P, BIF_ARG_1);
    if (!is_value(result)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(result);
}
