/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2023. All Rights Reserved.
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

#ifndef ASMJIT_ASMJIT_H_INCLUDED
#    include <asmjit/asmjit.hpp>
#endif

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
#include "big.h"
#include "beam_file.h"
#include "beam_types.h"
}

/* On Windows, the min and max macros may be defined. */
#undef min
#undef max

#include "beam_jit_args.hpp"
#include "beam_jit_types.hpp"
#include "beam_jit_register_cache.hpp"

using namespace asmjit;

struct AsmRange {
    ErtsCodePtr start;
    ErtsCodePtr stop;
    const std::string name;

    struct LineData {
        ErtsCodePtr start;
        const std::string file;
        unsigned line;
    };

    const std::vector<LineData> lines;
};

/* This is a partial class for `BeamAssembler`, containing various fields and
 * helper functions that are common to all architectures so that we won't have
 * to redefine them all the time. */
class BeamAssemblerCommon : public ErrorHandler {
    BaseAssembler &assembler;

protected:
    CodeHolder code;
    FileLogger logger;
    Section *rodata;

    static bool hasCpuFeature(uint32_t featureId);

    BeamAssemblerCommon(BaseAssembler &assembler);
    ~BeamAssemblerCommon();

    void codegen(JitAllocator *allocator,
                 const void **executable_ptr,
                 void **writable_ptr);

    void comment(const char *format) {
        if (logger.file()) {
            assembler.commentf("# %s", format);
        }
    }

    template<typename... Ts>
    void comment(const char *format, Ts... args) {
        if (logger.file()) {
            char buff[1024];
            erts_snprintf(buff, sizeof(buff), format, args...);
            assembler.commentf("# %s", buff);
        }
    }

    void *getCode(Label label);
    byte *getCode(char *labelName);

    void embed(void *data, uint32_t size) {
        assembler.embed((char *)data, size);
    }

    void handleError(Error err, const char *message, BaseEmitter *origin);

    void setLogger(const std::string &log);
    void setLogger(FILE *log);

public:
    void embed_rodata(const char *labelName, const char *buff, size_t size);
    void embed_bss(const char *labelName, size_t size);
    void embed_zeros(size_t size);

    void *getBaseAddress();
    size_t getOffset();
};

struct BeamModuleAssemblerCommon {
    typedef unsigned BeamLabel;

    /* The BEAM file we've been loaded from, if any. */
    const BeamFile *beam;
    Eterm mod;

    /* Map of label number to asmjit Label */
    typedef std::unordered_map<BeamLabel, const Label> LabelMap;
    LabelMap rawLabels;

    struct patch {
        Label where;
        size_t ptr_offs;
        size_t val_offs;
    };

    struct patch_catch {
        struct patch patch;
        Label handler;
    };
    std::vector<struct patch_catch> catches;

    /* Map of import entry to patch labels and mfa */
    struct patch_import {
        std::vector<struct patch> patches;
        ErtsCodeMFA mfa;
    };
    typedef std::unordered_map<unsigned, struct patch_import> ImportMap;
    ImportMap imports;

    /* Map of fun entry to trampoline labels and patches */
    struct patch_lambda {
        std::vector<struct patch> patches;
        Label trampoline;
    };
    typedef std::unordered_map<unsigned, struct patch_lambda> LambdaMap;
    LambdaMap lambdas;

    /* Map of literals to patch labels */
    struct patch_literal {
        std::vector<struct patch> patches;
    };
    typedef std::unordered_map<unsigned, struct patch_literal> LiteralMap;
    LiteralMap literals;

    /* All string patches */
    std::vector<struct patch> strings;

    /* All functions that have been seen so far */
    std::vector<BeamLabel> functions;

    /* Used by emit to populate the labelToMFA map */
    Label current_label;

    /* The offset of our BeamCodeHeader, if any. */
    Label code_header;

    /* The module's on_load function, if any. */
    Label on_load;

    /* The end of the last function. Note that the dispatch table, constants,
     * and veneers may follow. */
    Label code_end;

    BeamModuleAssemblerCommon(const BeamFile *beam_, Eterm mod_)
            : beam(beam_), mod(mod_) {
    }

    /* Helpers */
    const auto &getTypeEntry(const ArgSource &arg) const {
        auto typeIndex =
                arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;
        ASSERT(typeIndex < beam->types.count);
        return static_cast<BeamArgType &>(beam->types.entries[typeIndex]);
    }

    auto getTypeUnion(const ArgSource &arg) const {
        if (arg.isRegister()) {
            return getTypeEntry(arg).type();
        }

        Eterm constant =
                arg.isImmed()
                        ? arg.as<ArgImmed>().get()
                        : beamfile_get_literal(beam,
                                               arg.as<ArgLiteral>().get());

        switch (tag_val_def(constant)) {
        case ATOM_DEF:
            return BeamTypeId::Atom;
        case BITSTRING_DEF:
            return BeamTypeId::Bitstring;
        case FLOAT_DEF:
            return BeamTypeId::Float;
        case FUN_DEF:
            return BeamTypeId::Fun;
        case BIG_DEF:
        case SMALL_DEF:
            return BeamTypeId::Integer;
        case LIST_DEF:
            return BeamTypeId::Cons;
        case MAP_DEF:
            return BeamTypeId::Map;
        case NIL_DEF:
            return BeamTypeId::Nil;
        case EXTERNAL_PID_DEF:
        case PID_DEF:
            return BeamTypeId::Pid;
        case EXTERNAL_PORT_DEF:
        case PORT_DEF:
            return BeamTypeId::Port;
        case EXTERNAL_REF_DEF:
        case REF_DEF:
            return BeamTypeId::Reference;
        case TUPLE_DEF:
            return BeamTypeId::Tuple;
        default:
            ERTS_ASSERT(!"tag_val_def error");
            return BeamTypeId::None; /* Avoid warning */
        }
    }

    auto getClampedRange(const ArgSource &arg) const {
        if (arg.isSmall()) {
            Sint value = arg.as<ArgSmall>().getSigned();
            return std::make_pair(value, value);
        } else {
            const auto &entry = getTypeEntry(arg);

            auto min = entry.hasLowerBound() ? entry.min() : MIN_SMALL;
            auto max = entry.hasUpperBound() ? entry.max() : MAX_SMALL;

            return std::make_pair(min, max);
        }
    }

    bool always_small(const ArgSource &arg) const {
        if (arg.isSmall()) {
            return true;
        } else if (!exact_type<BeamTypeId::Integer>(arg)) {
            return false;
        }

        const auto &entry = getTypeEntry(arg);

        if (entry.hasLowerBound() && entry.hasUpperBound()) {
            return IS_SSMALL(entry.min()) && IS_SSMALL(entry.max());
        }

        return false;
    }

    bool always_immediate(const ArgSource &arg) const {
        return always_one_of<BeamTypeId::AlwaysImmediate>(arg) ||
               always_small(arg);
    }

    bool always_same_types(const ArgSource &lhs, const ArgSource &rhs) const {
        using integral = std::underlying_type_t<BeamTypeId>;
        auto lhs_types = static_cast<integral>(getTypeUnion(lhs));
        auto rhs_types = static_cast<integral>(getTypeUnion(rhs));

        /* We can only be certain that the types are the same when there's
         * one possible type. For example, if one is a number and the other
         * is an integer, they could differ if the former is a float. */
        if ((lhs_types & (lhs_types - 1)) == 0) {
            return lhs_types == rhs_types;
        }

        return false;
    }

    template<BeamTypeId... Types>
    bool never_one_of(const ArgSource &arg) const {
        using integral = std::underlying_type_t<BeamTypeId>;
        auto types = static_cast<integral>(BeamTypeIdUnion<Types...>::value());
        auto type_union = static_cast<integral>(getTypeUnion(arg));
        return static_cast<BeamTypeId>(type_union & types) == BeamTypeId::None;
    }

    template<BeamTypeId... Types>
    bool maybe_one_of(const ArgSource &arg) const {
        return !never_one_of<Types...>(arg);
    }

    template<BeamTypeId... Types>
    bool always_one_of(const ArgSource &arg) const {
        /* Providing a single type to this function is not an error per se, but
         * `exact_type` provides a bit more error checking for that use-case,
         * so we want to encourage its use. */
        static_assert(!BeamTypeIdUnion<Types...>::is_single_typed(),
                      "always_one_of expects a union of several primitive "
                      "types, use exact_type instead");

        using integral = std::underlying_type_t<BeamTypeId>;
        auto types = static_cast<integral>(BeamTypeIdUnion<Types...>::value());
        auto type_union = static_cast<integral>(getTypeUnion(arg));
        return type_union == (type_union & types);
    }

    template<BeamTypeId Type>
    bool exact_type(const ArgSource &arg) const {
        /* Rejects `exact_type<BeamTypeId::List>(...)` and similar, as it's
         * almost always an error to exactly match a union of several types.
         *
         * On the off chance that you _do_ need to match a union exactly, use
         * `masked_types<T>(arg) == T` instead. */
        static_assert(BeamTypeIdUnion<Type>::is_single_typed(),
                      "exact_type expects exactly one primitive type, use "
                      "always_one_of instead");

        using integral = std::underlying_type_t<BeamTypeId>;
        auto type_union = static_cast<integral>(getTypeUnion(arg));
        return type_union == (type_union & static_cast<integral>(Type));
    }

    template<BeamTypeId Mask>
    BeamTypeId masked_types(const ArgSource &arg) const {
        static_assert((Mask != BeamTypeId::AlwaysBoxed &&
                       Mask != BeamTypeId::AlwaysImmediate),
                      "using masked_types with AlwaysBoxed or AlwaysImmediate "
                      "is almost always an error, use exact_type, "
                      "maybe_one_of, or never_one_of instead");
        static_assert((Mask == BeamTypeId::MaybeBoxed ||
                       Mask == BeamTypeId::MaybeImmediate ||
                       Mask == BeamTypeId::AlwaysBoxed ||
                       Mask == BeamTypeId::AlwaysImmediate),
                      "masked_types expects a mask type like MaybeBoxed or "
                      "MaybeImmediate, use exact_type, maybe_one_of, or"
                      "never_one_of instead");

        using integral = std::underlying_type_t<BeamTypeId>;
        auto mask = static_cast<integral>(Mask);
        auto type_union = static_cast<integral>(getTypeUnion(arg));
        return static_cast<BeamTypeId>(type_union & mask);
    }

    bool is_sum_small_if_args_are_small(const ArgSource &LHS,
                                        const ArgSource &RHS) const {
        Sint min, max;
        auto [min1, max1] = getClampedRange(LHS);
        auto [min2, max2] = getClampedRange(RHS);
        min = min1 + min2;
        max = max1 + max2;
        return IS_SSMALL(min) && IS_SSMALL(max);
    }

    bool is_diff_small_if_args_are_small(const ArgSource &LHS,
                                         const ArgSource &RHS) const {
        Sint min, max;
        auto [min1, max1] = getClampedRange(LHS);
        auto [min2, max2] = getClampedRange(RHS);
        min = min1 - max2;
        max = max1 - min2;
        return IS_SSMALL(min) && IS_SSMALL(max);
    }

    bool is_product_small_if_args_are_small(const ArgSource &LHS,
                                            const ArgSource &RHS) const {
        auto [min1, max1] = getClampedRange(LHS);
        auto [min2, max2] = getClampedRange(RHS);
        auto mag1 = std::max(std::abs(min1), std::abs(max1));
        auto mag2 = std::max(std::abs(min2), std::abs(max2));

        /*
         * mag1 * mag2 <= MAX_SMALL
         * mag1 <= MAX_SMALL / mag2   (when mag2 != 0)
         */
        ERTS_CT_ASSERT(MAX_SMALL < -MIN_SMALL);
        return mag2 == 0 || mag1 <= MAX_SMALL / mag2;
    }

    bool is_bsl_small(const ArgSource &LHS, const ArgSource &RHS) const {
        if (!(always_small(LHS) && always_small(RHS))) {
            return false;
        } else {
            auto [min1, max1] = getClampedRange(LHS);
            auto [min2, max2] = getClampedRange(RHS);

            if (min1 < 0 || max1 == 0 || min2 < 0) {
                return false;
            }

            return max2 < asmjit::Support::clz(max1) - _TAG_IMMED1_SIZE;
        }
    }

    int getSizeUnit(const ArgSource &arg) const {
        auto entry = getTypeEntry(arg);
        return entry.hasUnit() ? entry.unit() : 1;
    }

    bool hasLowerBound(const ArgSource &arg) const {
        return getTypeEntry(arg).hasLowerBound();
    }

    bool hasUpperBound(const ArgSource &arg) const {
        return getTypeEntry(arg).hasUpperBound();
    }
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
    BSC_OP_BITSTRING = 0,
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
Sint32 beam_jit_remove_message(Process *c_p,
                               Sint32 FCALLS,
                               Eterm *HTOP,
                               Eterm *E,
                               Uint32 active_code_ix);

void beam_jit_bs_construct_fail_info(Process *c_p,
                                     Uint packed_error_info,
                                     Eterm arg3,
                                     Eterm arg1);
Sint beam_jit_bs_bit_size(Eterm term);

Eterm beam_jit_int128_to_big(Process *p, Uint sign, Uint low, Uint high);

void beam_jit_take_receive_lock(Process *c_p);
void beam_jit_wait_locked(Process *c_p, ErtsCodePtr cp);
void beam_jit_wait_unlocked(Process *c_p, ErtsCodePtr cp);

enum beam_jit_tmo_ret { RET_next = 0, RET_wait = 1, RET_badarg = 2 };

enum beam_jit_tmo_ret beam_jit_wait_timeout(Process *c_p,
                                            Eterm timeout_value,
                                            ErtsCodePtr next);

void beam_jit_timeout(Process *c_p);
void beam_jit_timeout_locked(Process *c_p);

void beam_jit_return_to_trace(Process *c_p,
                              Eterm session_weak_id,
                              Eterm *frame);

Eterm beam_jit_build_argument_list(Process *c_p, const Eterm *regs, int arity);

Export *beam_jit_handle_unloaded_fun(Process *c_p,
                                     Eterm *reg,
                                     int arity,
                                     Eterm fun_thing);

bool beam_jit_is_list_of_immediates(Eterm term);
bool beam_jit_is_shallow_boxed(Eterm term);

#endif
