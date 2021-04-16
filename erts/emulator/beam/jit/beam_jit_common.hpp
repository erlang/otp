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

#ifndef __BEAM_JIT_COMMON_HPP__
#define __BEAM_JIT_COMMON_HPP__

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
#include "bif.h"
#include "erl_vm.h"
#include "global.h"
#include "beam_file.h"
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

/* ** */

#if defined(DEBUG) && defined(JIT_HARD_DEBUG)
void beam_jit_validate_term(Eterm term);
#endif

typedef Eterm BeamJitNifF(struct enif_environment_t *, int argc, Eterm argv[]);
enum beam_jit_nif_load_ret { RET_NIF_success, RET_NIF_error, RET_NIF_yield };

Eterm beam_jit_call_bif(Process *c_p,
                        Eterm *reg,
                        ErtsCodePtr I,
                        ErtsBifFunc vbf,
                        Uint arity);
Eterm beam_jit_call_nif(Process *c_p,
                        ErtsCodePtr I,
                        Eterm *reg,
                        BeamJitNifF *fp,
                        struct erl_module_nif *NifMod);
enum beam_jit_nif_load_ret beam_jit_load_nif(Process *c_p,
                                             ErtsCodePtr I,
                                             Eterm *reg);

Uint beam_jit_get_map_elements(Eterm map,
                               Eterm *reg,
                               Eterm *E,
                               Uint n,
                               Eterm *fs);

void beam_jit_bs_field_size_argument_error(Process *c_p, Eterm size);
void beam_jit_bs_add_argument_error(Process *c_p, Eterm A, Eterm B);
Eterm beam_jit_bs_start_match2(Eterm context,
                               Uint live,
                               Uint slots,
                               Process *c_p,
                               Eterm *reg);
Eterm beam_jit_bs_init(Process *c_p,
                       Eterm *reg,
                       ERL_BITS_DECLARE_STATEP,
                       Eterm BsOp1,
                       Eterm BsOp2,
                       unsigned Live);
Eterm beam_jit_bs_init_bits(Process *c_p,
                            Eterm *reg,
                            ERL_BITS_DECLARE_STATEP,
                            Uint num_bits,
                            Uint alloc,
                            unsigned Live);
Eterm beam_jit_bs_get_integer(Process *c_p,
                              Eterm *reg,
                              Eterm context,
                              Uint flags,
                              Uint size,
                              Uint Live);

ErtsMessage *beam_jit_decode_dist(Process *c_p, ErtsMessage *msgp);
Sint beam_jit_remove_message(Process *c_p,
                             Sint FCALLS,
                             Eterm *HTOP,
                             Eterm *E,
                             Uint32 active_code_ix);
void beam_jit_bs_context_to_binary(Eterm context);

void beam_jit_take_receive_lock(Process *c_p);
void beam_jit_wait_locked(Process *c_p, ErtsCodePtr cp);
void beam_jit_wait_unlocked(Process *c_p, ErtsCodePtr cp);

enum beam_jit_tmo_ret { RET_next = 0, RET_wait = 1, RET_badarg = 2 };

enum beam_jit_tmo_ret beam_jit_wait_timeout(Process *c_p,
                                            Eterm timeout_value,
                                            ErtsCodePtr next);

void beam_jit_timeout(Process *c_p);
void beam_jit_timeout_locked(Process *c_p);

void beam_jit_return_to_trace(Process *c_p);

#endif
