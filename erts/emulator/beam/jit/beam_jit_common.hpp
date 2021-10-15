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

struct ArgVal : public BeamOpArg {
    enum TYPE : int {
        Word = TAG_u,
        XReg = TAG_x,
        YReg = TAG_y,
        FReg = TAG_l,
        Label = TAG_f,
        Literal = TAG_q,

        BytePtr = 'M',
        Catch = 'H',
        Export = 'E',
        FunEntry = 'F',
        Immediate = 'I'
    };

    ArgVal(UWord t, UWord value) {
#ifdef DEBUG
        switch (t) {
        case TAG_f:
            break;
        case TAG_l:
            break;
        case TAG_q:
            break;
        case TAG_u:
            break;
        case TAG_x:
            break;
        case TAG_y:
            break;
        case 'I':
            break;
        case 'E':
            break;
        case 'F':
            break;
        case 'M':
            break;
        case 'H':
            break;
        default:
            ASSERT(0);
        }
#endif
        type = (enum TYPE)t;
        val = value;
    }

    constexpr int typeIndex() const {
        switch (getType()) {
        case TYPE::XReg:
        case TYPE::YReg:
            return (int)(val >> 10);
        default:
            /* Type index 0 always points to the "any type," making it a safe
             * fallback whenever we lack type information. */
            return 0;
        }
    }

    constexpr enum TYPE getType() const {
        return (enum TYPE)type;
    }

    constexpr uint64_t getValue() const {
        switch (getType()) {
        case TYPE::XReg:
        case TYPE::YReg:
            return val & REG_MASK;
        default:
            return val;
        }
    }

    constexpr bool isRegister() const {
        switch (getType()) {
        case TYPE::FReg:
        case TYPE::XReg:
        case TYPE::YReg:
            return true;
        default:
            return false;
        }
    }

    constexpr bool isLiteral() const {
        return getType() == TYPE::Literal;
    }

    constexpr bool isImmed() const {
        return getType() == TYPE::Immediate;
    }

    constexpr bool isWord() const {
        return getType() == TYPE::Word;
    }

    constexpr bool isLabel() const {
        return getType() == TYPE::Label;
    }

    constexpr bool isConstant() const {
        switch (getType()) {
        case TYPE::BytePtr:
        case TYPE::Catch:
        case TYPE::Export:
        case TYPE::FunEntry:
        case TYPE::Immediate:
        case TYPE::Label:
        case TYPE::Literal:
        case TYPE::Word:
            return true;
        default:
            return false;
        }
    }

    struct Hash {
        size_t operator()(const ArgVal &key) const {
            return ((size_t)key.getType() << 48) ^ (size_t)key.getValue();
        }
    };

    bool operator==(const ArgVal &other) const {
        return getType() == other.getType() && getValue() == other.getValue();
    }

    template<typename T>
    ArgVal operator+(T val) const {
        return ArgVal(getType(), getValue() + val);
    }

    enum Relation { none, consecutive, reverse_consecutive };

    static Relation register_relation(const ArgVal &lhs, const ArgVal &rhs) {
        bool same_reg_types = lhs.getType() == rhs.getType();

        if (!same_reg_types) {
            return none;
        } else if (!lhs.isRegister()) {
            return none;
        } else if (lhs.getValue() + 1 == rhs.getValue()) {
            return consecutive;
        } else if (lhs.getValue() == rhs.getValue() + 1) {
            return reverse_consecutive;
        } else {
            return none;
        }
    };
};

/* This is a view into a contiguous container (like an array or `std::vector`),
 * letting us reuse the existing argument array in `beamasm_emit` while keeping
 * our interfaces convenient.
 *
 * Needless to say, spans must not live longer than the container they wrap, so
 * you must be careful not to return a span of an rvalue or stack-allocated
 * container.
 *
 * We can replace this with std::span once we require C++20. */
template<typename T>
class Span {
    const T *_data;
    size_t _size;

public:
    template<typename Container>
    Span(const Container &other) : Span(other.data(), other.size()) {
    }

    template<typename Container>
    Span(Container &other) : Span(other.data(), other.size()) {
    }

    Span(const T *begin, const T *end) : Span(begin, end - begin) {
    }

    Span(const T *data, size_t size) : _data(data), _size(size) {
    }

    Span<T> subspan(size_t index, size_t count) const {
        ASSERT(index <= size() && count <= (size() - index));
        return Span<T>(begin() + index, count);
    }

    const auto size() const {
        return _size;
    }

    const auto begin() const {
        return &_data[0];
    }

    const auto end() const {
        return &_data[size()];
    }

    const T &operator[](size_t index) const {
#ifdef DEBUG
        ASSERT(index < _size);
#endif
        return _data[index];
    }

    const T &front() const {
        return operator[](0);
    }

    const T &back() const {
        return operator[](size() - 1);
    }
};

static const Uint BSC_SEGMENT_OFFSET = 10;

typedef enum : Uint {
    BSC_OP_BINARY = 0,
    BSC_OP_FLOAT = 1,
    BSC_OP_INTEGER = 2,
    BSC_OP_UTF8 = 3,
    BSC_OP_UTF16 = 4,
    BSC_OP_UTF32 = 5,
    BSC_OP_LAST = 5
} JitBSCOp;
static const Uint BSC_OP_MASK = 0x07;
static const Uint BSC_OP_OFFSET = 7;

typedef enum : Uint {
    BSC_INFO_FVALUE = 0,
    BSC_INFO_TYPE = 1,
    BSC_INFO_SIZE = 2,
    BSC_INFO_UNIT = 3,
    BSC_INFO_DEPENDS = 4,
    BSC_INFO_LAST = 4,
} JitBSCInfo;
static const Uint BSC_INFO_MASK = 0x07;
static const Uint BSC_INFO_OFFSET = 4;

typedef enum : Uint {
    BSC_REASON_BADARG = 0,
    BSC_REASON_SYSTEM_LIMIT = 1,
    BSC_REASON_DEPENDS = 2,
    BSC_REASON_LAST = 2,
} JitBSCReason;
static const Uint BSC_REASON_MASK = 0x03;

typedef enum : Uint {
    BSC_VALUE_ARG1 = 0,
    BSC_VALUE_ARG3 = 1,
    BSC_VALUE_FVALUE = 2,
    BSC_VALUE_LAST = 2
} JitBSCValue;
static const Uint BSC_VALUE_MASK = 0x03;
static const Uint BSC_VALUE_OFFSET = 2;

static constexpr Uint beam_jit_set_bsc_segment_op(Uint segment, JitBSCOp op) {
    return (segment << BSC_SEGMENT_OFFSET) | (op << BSC_OP_OFFSET);
}

static constexpr Uint beam_jit_update_bsc_reason_info(Uint packed_info,
                                                      JitBSCReason reason,
                                                      JitBSCInfo info,
                                                      JitBSCValue value) {
    return packed_info | (value << BSC_VALUE_OFFSET) |
           (info << BSC_INFO_OFFSET) | reason;
}

static constexpr Uint beam_jit_get_bsc_segment(Uint packed_info) {
    return packed_info >> BSC_SEGMENT_OFFSET;
}

static constexpr JitBSCOp beam_jit_get_bsc_op(Uint packed_info) {
    ERTS_CT_ASSERT((BSC_OP_LAST & ~BSC_OP_MASK) == 0);
    return (JitBSCOp)((packed_info >> BSC_OP_OFFSET) & BSC_OP_MASK);
}

static constexpr JitBSCInfo beam_jit_get_bsc_info(Uint packed_info) {
    ERTS_CT_ASSERT((BSC_INFO_LAST & ~BSC_INFO_MASK) == 0);
    return (JitBSCInfo)((packed_info >> BSC_INFO_OFFSET) & BSC_INFO_MASK);
}

static constexpr JitBSCReason beam_jit_get_bsc_reason(Uint packed_info) {
    ERTS_CT_ASSERT((BSC_REASON_LAST & ~BSC_REASON_MASK) == 0);
    return (JitBSCReason)(packed_info & BSC_REASON_MASK);
}

static constexpr JitBSCValue beam_jit_get_bsc_value(Uint packed_info) {
    ERTS_CT_ASSERT((BSC_VALUE_LAST & ~BSC_VALUE_MASK) == 0);
    return (JitBSCValue)((packed_info >> BSC_VALUE_OFFSET) & BSC_VALUE_MASK);
}

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
Eterm beam_jit_bs_init(Process *c_p,
                       Eterm *reg,
                       ERL_BITS_DECLARE_STATEP,
                       Eterm num_bytes,
                       Uint alloc,
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

void beam_jit_bs_construct_fail_info(Process *c_p,
                                     Uint packed_error_info,
                                     Eterm arg3,
                                     Eterm arg1);
Sint beam_jit_bs_bit_size(Eterm term);

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

Eterm beam_jit_build_argument_list(Process *c_p, const Eterm *regs, int arity);

Export *beam_jit_handle_unloaded_fun(Process *c_p,
                                     Eterm *reg,
                                     int arity,
                                     Eterm fun_thing);

#endif
