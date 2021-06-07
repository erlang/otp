/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021. All Rights Reserved.
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

#include <string>
#include <vector>
#include <unordered_map>
#include <map>

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"

#include "beam_jit_common.h"
}

class ArgVal {
    BeamOpArg gen_op;

public:
    enum TYPE {
        u = TAG_u,
        i = TAG_i,
        x = TAG_x,
        y = TAG_y,
        f = TAG_f,
        q = TAG_q,
        e = TAG_r,
        l = TAG_l /* float register */
    };

    ArgVal(const BeamOpArg &arg) {
        gen_op = arg;
    }

    ArgVal(enum TYPE t, BeamInstr val) {
        gen_op.type = t;
        gen_op.val = val;
    }

    ArgVal(unsigned t, BeamInstr val) {
#ifdef DEBUG
        switch (t) {
        case TAG_u:
            break;
        case TAG_i:
            break;
        case TAG_x:
            break;
        case TAG_y:
            break;
        case TAG_f:
            break;
        case TAG_q:
            break;
        case TAG_r:
            break;
        case TAG_l:
            break;
        default:
            ASSERT(0);
        }
#endif

        gen_op.type = t;
        gen_op.val = val;
    }

    constexpr enum TYPE getType() const {
        return (enum TYPE)gen_op.type;
    }

    constexpr uint64_t getValue() const {
        return gen_op.val;
    }

    constexpr bool isMem() const {
        return gen_op.type == x || gen_op.type == y;
    }

    constexpr bool isLiteral() const {
        return gen_op.type == q;
    }

    constexpr bool isImmed() const {
        return gen_op.type == i;
    }

    template<typename T>
    ArgVal operator+(T val) const {
        return ArgVal(gen_op.type, val + gen_op.val);
    }

    template<typename T>
    ArgVal operator*(T val) const {
        return ArgVal(gen_op.type, val * gen_op.val);
    }

    enum Relation { none, consecutive, reverse_consecutive };

    static Relation register_relation(const ArgVal &arg1, const ArgVal &arg2) {
        TYPE type = arg1.getType();
        bool same_reg_types =
                type == arg2.getType() && (type == TYPE::x || type == TYPE::y);
        if (!same_reg_types) {
            return none;
        } else if (arg1.getValue() + 1 == arg2.getValue()) {
            return consecutive;
        } else if (arg1.getValue() == arg2.getValue() + 1) {
            return reverse_consecutive;
        } else {
            return none;
        }
    };
};
