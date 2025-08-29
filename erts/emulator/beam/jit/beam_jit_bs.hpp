/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2025. All Rights Reserved.
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

#ifndef __BEAM_JIT_BS_HPP__
#define __BEAM_JIT_BS_HPP__

#include "beam_jit_common.hpp"

/*
 * Constructing binaries.
 */

struct BscSegment {
    BscSegment()
            : type(am_false), unit(1), flags(0), src(ArgNil()), size(ArgNil()),
              error_info(0), offsetInAccumulator(0), effectiveSize(-1),
              action(action::DIRECT) {
    }

    Eterm type;
    Uint unit;
    Uint flags;
    ArgVal src;
    ArgVal size;

    Uint error_info;
    Uint offsetInAccumulator;
    Sint effectiveSize;

    /* Here are sub actions for storing integer segments.
     *
     * We use the ACCUMULATE_FIRST and ACCUMULATE actions to shift the
     * values of segments with known, small sizes (no more than 64 bits)
     * into an accumulator register.
     *
     * When no more segments can be accumulated, the STORE action is
     * used to store the value of the accumulator into the binary.
     *
     * The DIRECT action is used when it is not possible to use the
     * accumulator (for unknown or too large sizes).
     */
    enum class action { DIRECT, ACCUMULATE_FIRST, ACCUMULATE, STORE } action;
};

std::vector<BscSegment> beam_jit_bsc_init(const Span<ArgVal> &args);

std::vector<BscSegment> beam_jit_bsc_combine_segments(
        const std::vector<BscSegment> segments);

/*
 * Matching binaries.
 */

struct BsmSegment {
    BsmSegment()
            : action(action::TEST_HEAP), live(ArgNil()), size(0), unit(1),
              flags(0), dst(ArgXRegister(0)){};

    enum class action {
        TEST_HEAP,
        ENSURE_AT_LEAST,
        ENSURE_EXACTLY,
        READ,
        EXTRACT_BITSTRING,
        EXTRACT_INTEGER,
        READ_INTEGER,
        GET_INTEGER,
        GET_BITSTRING,
        SKIP,
        DROP,
        GET_TAIL,
        EQ
    } action;
    ArgVal live;
    Uint size;
    Uint unit;
    Uint flags;
    ArgRegister dst;
};

std::vector<BsmSegment> beam_jit_bsm_init(const BeamFile *beam,
                                          Span<ArgVal> const &List);

std::vector<BsmSegment> beam_jit_opt_bsm_segments(
        const std::vector<BsmSegment> segments,
        const ArgWord &Need,
        const ArgWord &Live);
#endif
