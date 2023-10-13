/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
#include <algorithm>
#include "beam_asm.hpp"

using namespace asmjit;

extern "C"
{
#include "erl_map.h"
#include "erl_term_hashing.h"
#include "beam_common.h"
}

/* ARG2 = term
 *
 * Helper for calculating the internal hash of keys before looking them up in a
 * map. This is a manual expansion of `erts_internal_hash`, and all changes to
 * that function must be mirrored here.
 *
 * Result in ARG3. */
void BeamGlobalAssembler::emit_internal_hash_helper() {
    x86::Gp key = ARG2, key_hash = ARG3;

    /* Unsigned multiplication instructions on x86 either use RDX as an
     * implicit source or clobber it. Sigh. */
    if (key == x86::rdx) {
        a.mov(TMP_MEM1q, x86::rdx);
    } else {
        ASSERT(key_hash == x86::rdx);
    }

    /* key_hash = key ^ (key >> 33); */
    a.mov(ARG4, ARG2);
    a.shr(ARG4, imm(33));
    a.mov(x86::rdx, ARG2);
    a.xor_(x86::rdx, ARG4);

    /* `RDX * ARG6` storing a 128 bit result in ARG4:RDX. We only want the
     * lower 64 bits in RDX.
     *
     * key_hash *= 0xFF51AFD7ED558CCDull */
    mov_imm(ARG6, 0xFF51AFD7ED558CCDull);
    a.mulx(ARG4, x86::rdx, ARG6);

    /* key_hash ^= key_hash >> 33; */
    a.mov(ARG4, x86::rdx);
    a.shr(ARG4, imm(33));
    a.xor_(x86::rdx, ARG4);

    /* key_hash *= 0xC4CEB9FE1A85EC53ull */
    mov_imm(ARG6, 0xC4CEB9FE1A85EC53ull);
    a.mulx(ARG4, x86::rdx, ARG6);

    /* key_hash ^= key_hash >> 33; */
    a.mov(ARG4, x86::rdx);
    a.shr(ARG4, imm(33));
    a.xor_(x86::rdx, ARG4);

    if (key == x86::rdx) {
        a.mov(key_hash, x86::rdx);
        a.mov(key, TMP_MEM1q);
    }

#ifdef DBG_HASHMAP_COLLISION_BONANZA
    a.mov(TMP_MEM1q, ARG1);
    a.mov(TMP_MEM2q, ARG2);
    a.mov(TMP_MEM3q, RET);

    a.mov(ARG1, ARG3);
    emit_enter_runtime();
    runtime_call<2>(erts_dbg_hashmap_collision_bonanza);
    emit_leave_runtime();

    a.mov(ARG3, RET);

    a.mov(ARG1, TMP_MEM1q);
    a.mov(ARG2, TMP_MEM2q);
    a.mov(RET, TMP_MEM3q);
#endif
}

/* ARG1 = hash map root, ARG2 = key, ARG3 = key hash, RETd = node header
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_hashmap_get_element() {
    Label node_loop = a.newLabel();

    x86::Gp node = ARG1, key = ARG2, key_hash = ARG3, header_val = RETd,
            index = ARG4d, depth = ARG5d;

    const int header_shift =
            (_HEADER_ARITY_OFFS + MAP_HEADER_TAG_SZ + MAP_HEADER_ARITY_SZ);

    /* Skip the root header. This is not required for child nodes. */
    a.add(node, imm(sizeof(Eterm)));
    mov_imm(depth, 0);

    a.bind(node_loop);
    {
        Label done = a.newLabel(), leaf_node = a.newLabel(),
              skip_index_adjustment = a.newLabel(),
              collision_node = a.newLabel();

        /* Find out which child we should follow, and shift the hash for the
         * next round. */
        a.mov(index, key_hash.r32());
        a.and_(index, imm(0xF));
        a.shr(key_hash, imm(4));
        a.inc(depth);

        /* The entry offset is always equal to the index on fully populated
         * nodes, so we'll skip adjusting them. */
        ERTS_CT_ASSERT(header_shift == 16);
        a.sar(header_val, imm(header_shift));
        a.cmp(header_val, imm(-1));
        a.short_().je(skip_index_adjustment);
        {
            /* If our bit isn't set on this node, the key can't be found.
             *
             * Note that we jump directly to a `RET` instruction, as `BT` only
             * affects CF, and ZF ("not found") is clear at this point. */
            a.bt(header_val, index);
            a.short_().jnc(done);

            /* The actual offset of our entry is the number of bits set (in
             * essence "entries present") before our index in the bitmap. */
            a.bzhi(header_val, header_val, index);
            a.popcnt(index, header_val);
        }
        a.bind(skip_index_adjustment);

        a.mov(node,
              x86::qword_ptr(node,
                             index.r64(),
                             3,
                             sizeof(Eterm) - TAG_PRIMARY_BOXED));
        emit_ptr_val(node, node);

        /* Have we found our leaf? */
        a.test(node.r8(), imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
        a.short_().je(leaf_node);

        /* Nope, we have to search another node. */
        a.mov(header_val, emit_boxed_val(node, 0, sizeof(Uint32)));

        /* After 8/16 nodes we've run out of the hash bits we've started with
         * and we end up in a collision node. */
        a.test(depth, imm(HAMT_MAX_LEVEL - 1));
        a.short_().jnz(node_loop);
        a.short_().jmp(collision_node);

        a.bind(leaf_node);
        {
            /* We've arrived at a leaf, set ZF according to whether its key
             * matches ours and speculatively place the element in RET. */
            a.cmp(getCARRef(node), key);
            a.mov(RET, getCDRRef(node));

            /* See comment at the jump. */
            a.bind(done);
            a.ret();
        }

        /* A collision node is a tuple of leafs where we do linear search.*/
        a.bind(collision_node);
        {
            Label linear_loop = a.newLabel();

            a.shr(header_val, imm(_HEADER_ARITY_OFFS));
            a.lea(ARG6d, x86::qword_ptr(header_val, -1));

            a.bind(linear_loop);
            {
                a.mov(ARG3,
                      x86::qword_ptr(node, ARG6, 3, 8 - TAG_PRIMARY_BOXED));

                emit_ptr_val(ARG3, ARG3);
                a.cmp(key, getCARRef(ARG3));
                a.mov(RET, getCDRRef(ARG3));
                a.short_().jz(done);

                a.dec(ARG6d);
                a.short_().jns(linear_loop);
            }

            a.ret();
        }
    }
}

/* ARG1 = flat map, ARG2 = key
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_flatmap_get_element() {
    Label fail = a.newLabel(), loop = a.newLabel();

    a.mov(RETd, emit_boxed_val(ARG1, offsetof(flatmap_t, size), 4));
    a.mov(ARG4, emit_boxed_val(ARG1, offsetof(flatmap_t, keys)));

    emit_ptr_val(ARG4, ARG4);

    a.bind(loop);
    {
        a.dec(RETd);
        a.short_().jl(fail);

        a.cmp(ARG2,
              x86::qword_ptr(ARG4, RET, 3, sizeof(Eterm) - TAG_PRIMARY_BOXED));
        a.short_().jne(loop);
    }

    int value_offset = sizeof(flatmap_t) - TAG_PRIMARY_BOXED;
    a.mov(RET, x86::qword_ptr(ARG1, RET, 3, value_offset));

    a.bind(fail);
    a.ret();
}

void BeamGlobalAssembler::emit_new_map_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_new_map);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    emit_leave_frame();

    a.ret();
}

void BeamModuleAssembler::emit_new_map(const ArgRegister &Dst,
                                       const ArgWord &Live,
                                       const ArgWord &Size,
                                       const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, CP_SIZE);

    ASSERT(Size.get() == args.size());

    mov_imm(ARG3, Live.get());
    mov_imm(ARG4, args.size());
    a.lea(ARG5, x86::qword_ptr(data));
    fragment_call(ga->get_new_map_shared());

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_new_small_map_lit(const ArgRegister &Dst,
                                                   const ArgWord &Live,
                                                   const ArgLiteral &Keys,
                                                   const ArgWord &Size,
                                                   const Span<ArgVal> &args) {
    ASSERT(Size.get() == args.size());

    emit_gc_test(ArgWord(0),
                 ArgWord(args.size() + MAP_HEADER_FLATMAP_SZ + 1),
                 Live);

    std::vector<ArgVal> data;
    data.reserve(args.size() + MAP_HEADER_FLATMAP_SZ + 1);
    data.push_back(ArgWord(MAP_HEADER_FLATMAP));
    data.push_back(Size);
    data.push_back(Keys);

    for (auto arg : args) {
        data.push_back(arg);
    }

    size_t size = data.size();
    unsigned i;

    mov_arg(x86::qword_ptr(HTOP), data[0]);

    /* Starting from 1 instead of 0 gives more opportunities for
     * applying the MMX optimizations. */
    for (i = 1; i < size - 1; i += 2) {
        x86::Mem dst_ptr0 = x86::qword_ptr(HTOP, i * sizeof(Eterm));
        x86::Mem dst_ptr1 = x86::qword_ptr(HTOP, (i + 1) * sizeof(Eterm));
        auto first = data[i];
        auto second = data[i + 1];

        switch (ArgVal::memory_relation(first, second)) {
        case ArgVal::consecutive: {
            x86::Mem src_ptr = getArgRef(first, 16);

            comment("(initializing two elements at once)");
            dst_ptr0.setSize(16);
            vmovups(x86::xmm0, src_ptr);
            vmovups(dst_ptr0, x86::xmm0);
            break;
        }
        case ArgVal::reverse_consecutive: {
            if (!hasCpuFeature(CpuFeatures::X86::kAVX)) {
                mov_arg(dst_ptr0, first);
                mov_arg(dst_ptr1, second);
            } else {
                x86::Mem src_ptr = getArgRef(second, 16);

                comment("(initializing with two swapped elements at once)");
                dst_ptr0.setSize(16);
                a.vpermilpd(x86::xmm0, src_ptr, 1); /* Load and swap */
                a.vmovups(dst_ptr0, x86::xmm0);
            }
            break;
        }
        case ArgVal::none:
            mov_arg(dst_ptr0, first);
            mov_arg(dst_ptr1, second);
            break;
        }
    }

    if (i < size) {
        x86::Mem dst_ptr = x86::qword_ptr(HTOP, i * sizeof(Eterm));
        mov_arg(dst_ptr, data[i]);
    }

    a.lea(ARG1, x86::byte_ptr(HTOP, TAG_PRIMARY_BOXED));
    a.add(HTOP, imm(size * sizeof(Eterm)));

    mov_arg(Dst, ARG1);
}

/* ARG1 = map, ARG2 = key
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_shared() {
    Label generic = a.newLabel(), hashmap = a.newLabel();

    a.mov(RETd, ARG2d);

    a.and_(RETb, imm(_TAG_PRIMARY_MASK));
    a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
    a.short_().jne(generic);

    emit_ptr_val(ARG1, ARG1);

    a.mov(RETd, emit_boxed_val(ARG1, 0, sizeof(Uint32)));
    a.mov(ARG4d, RETd);

    a.and_(ARG4d, imm(_HEADER_MAP_SUBTAG_MASK));
    a.cmp(ARG4d, imm(HAMT_SUBTAG_HEAD_FLATMAP));
    a.short_().jne(hashmap);

    emit_flatmap_get_element();

    a.bind(generic);
    {
        emit_enter_frame();
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();
        emit_leave_frame();

        emit_test_the_non_value(RET);

        /* Invert ZF, we want it to be set when RET is a value. */
        a.setnz(ARG1.r8());
        a.dec(ARG1.r8());

        a.ret();
    }

    a.bind(hashmap);
    {
        /* Calculate the internal hash of the key before diving into the
         * HAMT. */
        emit_internal_hash_helper();
        emit_hashmap_get_element();
    }
}

void BeamModuleAssembler::emit_i_get_map_element(const ArgLabel &Fail,
                                                 const ArgRegister &Src,
                                                 const ArgRegister &Key,
                                                 const ArgRegister &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    if (maybe_one_of<BeamTypeId::MaybeImmediate>(Key) &&
        hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        safe_fragment_call(ga->get_i_get_map_element_shared());
        a.jne(resolve_beam_label(Fail));
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        emit_test_the_non_value(RET);
        a.je(resolve_beam_label(Fail));
    }

    /* Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.) */
    if (!(Dst.isXRegister() && Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_get_map_elements(const ArgLabel &Fail,
                                                  const ArgSource &Src,
                                                  const ArgWord &Size,
                                                  const Span<ArgVal> &args) {
    Label generic = a.newLabel(), next = a.newLabel();
    Label data = embed_vararg_rodata(args, 0);

    /* We're not likely to gain much from inlining huge extractions, and the
     * resulting code is quite large, so we'll cut it off after a handful
     * elements.
     *
     * Note that the arguments come in flattened triplets of
     * `{Key, Dst, KeyHash}` */
    bool can_inline = args.size() < (8 * 3);

    ASSERT(Size.get() == args.size());
    ASSERT((Size.get() % 3) == 0);

    for (size_t i = 0; i < args.size(); i += 3) {
        can_inline &= args[i].isImmed();
    }

    mov_arg(ARG1, Src);

    if (can_inline) {
        comment("simplified multi-element lookup");

        emit_ptr_val(ARG1, ARG1);

        a.mov(RETd, emit_boxed_val(ARG1, 0, sizeof(Uint32)));
        a.and_(RETb, imm(_HEADER_MAP_SUBTAG_MASK));
        a.cmp(RETb, imm(HAMT_SUBTAG_HEAD_FLATMAP));
        a.jne(generic);

        ERTS_CT_ASSERT(MAP_SMALL_MAP_LIMIT <= ERTS_UINT32_MAX);
        a.mov(RETd,
              emit_boxed_val(ARG1, offsetof(flatmap_t, size), sizeof(Uint32)));
        a.mov(ARG2, emit_boxed_val(ARG1, offsetof(flatmap_t, keys)));

        emit_ptr_val(ARG2, ARG2);

        for (ssize_t i = args.size() - 3; i >= 0; i -= 3) {
            Label loop = a.newLabel();

            a.bind(loop);
            {
                x86::Mem candidate =
                        x86::qword_ptr(ARG2,
                                       RET,
                                       3,
                                       sizeof(Eterm) - TAG_PRIMARY_BOXED);

                a.dec(RETd);
                a.jl(resolve_beam_label(Fail));

                const auto &Comparand = args[i];
                cmp_arg(candidate, Comparand, ARG3);
                a.short_().jne(loop);
            }

            /* Don't store the result if the destination is the scratch X
             * register. (This instruction was originally a has_map_fields
             * instruction.) */
            const auto &Dst = args[i + 1];
            if (!(Dst.isXRegister() &&
                  Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
                const int value_offset = sizeof(flatmap_t) - TAG_PRIMARY_BOXED;
                mov_arg(Dst, x86::qword_ptr(ARG1, RET, 3, value_offset), ARG3);
            }
        }

        a.short_().jmp(next);
    }

    a.bind(generic);
    {
        mov_imm(ARG4, args.size() / 3);
        a.lea(ARG5, x86::qword_ptr(data));
        a.mov(ARG3, E);

        emit_enter_runtime();

        load_x_reg_array(ARG2);
        runtime_call<5>(beam_jit_get_map_elements);

        emit_leave_runtime();

        a.test(RET, RET);
        a.je(resolve_beam_label(Fail));
    }

    a.bind(next);
}

/* ARG1 = map, ARG2 = key, ARG3 = key hash
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_hash_shared() {
    Label hashmap = a.newLabel();

    emit_ptr_val(ARG1, ARG1);

    a.mov(RETd, emit_boxed_val(ARG1, 0, sizeof(Uint32)));
    a.mov(ARG4d, RETd);

    a.and_(ARG4d, imm(_HEADER_MAP_SUBTAG_MASK));
    a.cmp(ARG4d, imm(HAMT_SUBTAG_HEAD_FLATMAP));
    a.short_().jne(hashmap);

    emit_flatmap_get_element();

    a.bind(hashmap);
    emit_hashmap_get_element();
}

void BeamModuleAssembler::emit_i_get_map_element_hash(const ArgLabel &Fail,
                                                      const ArgRegister &Src,
                                                      const ArgConstant &Key,
                                                      const ArgWord &Hx,
                                                      const ArgRegister &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);
    mov_arg(ARG3, Hx);

    if (Key.isImmed() && hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        safe_fragment_call(ga->get_i_get_map_element_hash_shared());
        a.jne(resolve_beam_label(Fail));
    } else {
        emit_enter_runtime();
        runtime_call<3>(get_map_element_hash);
        emit_leave_runtime();

        emit_test_the_non_value(RET);
        a.je(resolve_beam_label(Fail));
    }

    /* Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.) */
    if (!(Dst.isXRegister() && Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
        mov_arg(Dst, RET);
    }
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_assoc);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    emit_leave_frame();

    a.ret();
}

/* ARG2 = key
 * ARG3 = value
 * ARG4 = map
 */
void BeamGlobalAssembler::emit_update_map_single_assoc_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_maps_put);

    emit_leave_runtime<Update::eHeapAlloc>();
    emit_leave_frame();

    a.ret();
}

void BeamModuleAssembler::emit_update_map_assoc(const ArgSource &Src,
                                                const ArgRegister &Dst,
                                                const ArgWord &Live,
                                                const ArgWord &Size,
                                                const Span<ArgVal> &args) {
    ASSERT(Size.get() == args.size());

    if (args.size() == 2) {
        mov_arg(ARG2, args[0]);
        mov_arg(ARG3, args[1]);
        mov_arg(ARG4, Src);
        safe_fragment_call(ga->get_update_map_single_assoc_shared());
    } else {
        Label data = embed_vararg_rodata(args, CP_SIZE);

        mov_arg(getXRef(Live.get()), Src);

        mov_imm(ARG3, Live.get());
        mov_imm(ARG4, args.size());
        a.lea(ARG5, x86::qword_ptr(data));
        fragment_call(ga->get_update_map_assoc_shared());
    }

    mov_arg(Dst, RET);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.ret();
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    Label error = a.newLabel();

    emit_enter_frame();
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.short_().je(error);

    a.ret();

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        a.jmp(labels[raise_exception]);
    }
}

/* ARG2 = key
 * ARG3 = value
 * ARG4 = map
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_single_exact_body_shared() {
    Label error = a.newLabel();

    a.mov(TMP_MEM2q, ARG2);

    emit_enter_frame();
    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    a.lea(ARG5, TMP_MEM1q);
    runtime_call<5>(erts_maps_update);

    emit_leave_runtime<Update::eHeapAlloc>();
    emit_leave_frame();

    a.test(RETd, RETd);
    a.short_().je(error);

    a.mov(RET, TMP_MEM1q);
    a.ret();

    a.bind(error);
    {
        a.mov(RET, TMP_MEM2q);
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADKEY));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), RET);
        mov_imm(ARG4, 0);
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_update_map_exact(const ArgSource &Src,
                                                const ArgLabel &Fail,
                                                const ArgRegister &Dst,
                                                const ArgWord &Live,
                                                const ArgWord &Size,
                                                const Span<ArgVal> &args) {
    ASSERT(Size.get() == args.size());

    if (args.size() == 2 && Fail.get() == 0) {
        mov_arg(ARG2, args[0]);
        mov_arg(ARG3, args[1]);
        mov_arg(ARG4, Src);
        fragment_call(ga->get_update_map_single_exact_body_shared());
    } else {
        Label data = embed_vararg_rodata(args, CP_SIZE);

        mov_arg(getXRef(Live.get()), Src);

        mov_imm(ARG3, Live.get());
        mov_imm(ARG4, args.size());
        a.lea(ARG5, x86::qword_ptr(data));

        if (Fail.get() != 0) {
            fragment_call(ga->get_update_map_exact_guard_shared());
            a.je(resolve_beam_label(Fail));
        } else {
            fragment_call(ga->get_update_map_exact_body_shared());
        }
    }

    mov_arg(Dst, RET);
}
