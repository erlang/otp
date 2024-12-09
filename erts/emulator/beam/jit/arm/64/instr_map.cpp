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
 * Result in ARG3. Clobbers TMP1. */
void BeamGlobalAssembler::emit_internal_hash_helper() {
    a64::Gp key = ARG2, key_hash = ARG3;

    /* key_hash = key ^ (key >> 33); */
    a.eor(key_hash, key, key, arm::lsr(33));

    /* key_hash *= 0xFF51AFD7ED558CCDull */
    mov_imm(TMP1, 0xFF51AFD7ED558CCDull);
    a.mul(key_hash, key_hash, TMP1);

    /* key_hash ^= key_hash >> 33; */
    a.eor(key_hash, key_hash, key_hash, arm::lsr(33));

    /* key_hash *= 0xC4CEB9FE1A85EC53ull */
    mov_imm(TMP1, 0xC4CEB9FE1A85EC53ull);
    a.mul(key_hash, key_hash, TMP1);

    /* key_hash ^= key_hash >> 33; */
    a.eor(key_hash, key_hash, key_hash, arm::lsr(33));

#ifdef DBG_HASHMAP_COLLISION_BONANZA
    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.stp(ARG1, ARG2, TMP_MEM1q);
    a.str(ARG4, TMP_MEM3q);

    a.mov(ARG1, ARG3);
    runtime_call<2>(erts_dbg_hashmap_collision_bonanza);
    a.mov(ARG3, ARG1);

    a.ldp(ARG1, ARG2, TMP_MEM1q);
    a.ldr(ARG4, TMP_MEM3q);

    emit_leave_runtime();
    emit_leave_runtime_frame();
#endif
}

/* ARG1 = untagged hash map root
 * ARG2 = key
 * ARG3 = key hash
 * ARG4 = node header
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_hashmap_get_element() {
    Label node_loop = a.newLabel();

    a64::Gp node = ARG1, key = ARG2, key_hash = ARG3, header_val = ARG4,
            depth = TMP5, index = TMP6;

    const int header_shift =
            (_HEADER_ARITY_OFFS + MAP_HEADER_TAG_SZ + MAP_HEADER_ARITY_SZ);

    /* Skip the root header, which is two words long (child headers are one
     * word). */
    a.add(node, node, imm(sizeof(Eterm[2])));
    mov_imm(depth, 0);

    a.bind(node_loop);
    {
        Label done = a.newLabel(), leaf_node = a.newLabel(),
              skip_index_adjustment = a.newLabel(),
              collision_node = a.newLabel();

        /* Find out which child we should follow, and shift the hash for the
         * next round. */
        a.and_(index, key_hash, imm(0xF));
        a.lsr(key_hash, key_hash, imm(4));
        a.add(depth, depth, imm(1));

        /* The entry offset is always equal to the index on fully populated
         * nodes, so we'll skip adjusting them. */
        ERTS_CT_ASSERT(header_shift == 16);
        a.asr(header_val.w(), header_val.w(), imm(header_shift));
        a.cmn(header_val.w(), imm(1));
        a.b_eq(skip_index_adjustment);
        {
            /* If our bit isn't set on this node, the key can't be found.
             *
             * Note that we jump directly to the return sequence as ZF is clear
             * at this point. */
            a.lsr(TMP1, header_val, index);
            a.tbz(TMP1, imm(0), done);

            /* The actual offset of our entry is the number of bits set (in
             * essence "entries present") before our index in the bitmap. Clear
             * the upper bits and count the remainder. */
            a.lsl(TMP2, TMP1, index);
            a.eor(TMP1, TMP2, header_val);
            a.fmov(a64::d0, TMP1);
            a.cnt(a64::v0.b8(), a64::v0.b8());
            a.addv(a64::b0, a64::v0.b8());
            a.fmov(index, a64::d0);
        }
        a.bind(skip_index_adjustment);

        a.ldr(TMP1, arm::Mem(node, index, arm::lsl(3)));
        emit_untag_ptr(node, TMP1);

        /* Have we found our leaf? */
        emit_is_boxed(leaf_node, TMP1);

        /* Nope, we have to search another node. Read and skip past the header
         * word. */
        a.ldr(header_val, arm::Mem(node).post(sizeof(Eterm)));

        /* After 8/16 nodes we've run out of the hash bits we've started with
         * and we end up in a collision node. */
        a.cmp(depth, imm(HAMT_MAX_LEVEL));
        a.b_ne(node_loop);
        a.b(collision_node);

        a.bind(leaf_node);
        {
            /* We've arrived at a leaf, set ZF according to whether its key
             * matches ours and speculatively place the element in ARG1. */
            a.ldp(TMP1, ARG1, arm::Mem(node));
            a.cmp(TMP1, key);

            /* See comment at the jump. */
            a.bind(done);
            a.ret(a64::x30);
        }

        /* A collision node is a tuple of leafs where we do linear search.*/
        a.bind(collision_node);
        {
            Label linear_loop = a.newLabel();

            a.lsr(TMP1, header_val, imm(_HEADER_ARITY_OFFS - 3));

            a.bind(linear_loop);
            {
                a.sub(TMP1, TMP1, imm(8));

                a.ldr(TMP2, arm::Mem(node, TMP1));

                emit_untag_ptr(TMP2, TMP2);
                a.ldp(TMP3, TMP4, arm::Mem(TMP2));
                a.cmp(key, TMP3);
                a.csel(ARG1, node, TMP4, imm(arm::CondCode::kNE));
                a.b_eq(done);

                a.cbnz(TMP1, linear_loop);
            }

            a.ret(a64::x30);
        }
    }
}

/* ARG1 = untagged flat map
 * ARG2 = key
 * ARG5 = size
 *
 * Result is returned in ARG1. ZF is set on success. */
void BeamGlobalAssembler::emit_flatmap_get_element() {
    Label fail = a.newLabel(), loop = a.newLabel();

    /* Bump size by 1 to slide past the `keys` field in the map, and the header
     * word in the key array. Also set flags to ensure ZF == 0 when entering
     * the loop. */
    a.adds(ARG5, ARG5, imm(1));

    /* Adjust our map pointer to the `keys` field before loading it. This
     * saves us from having to bump it to point at the values later on. */
    a.ldr(TMP1, arm::Mem(ARG1).pre(offsetof(flatmap_t, keys)));
    emit_untag_ptr(TMP1, TMP1);

    a.bind(loop);
    {
        a.sub(ARG5, ARG5, imm(1));
        a.cbz(ARG5, fail);

        a.ldr(TMP4, arm::Mem(TMP1, ARG5, arm::lsl(3)));
        a.cmp(ARG2, TMP4);
        a.b_ne(loop);
    }

    a.ldr(ARG1, arm::Mem(ARG1, ARG5, arm::lsl(3)));

    a.bind(fail);
    a.ret(a64::x30);
}

void BeamGlobalAssembler::emit_new_map_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_new_map);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_new_map(const ArgRegister &Dst,
                                       const ArgWord &Live,
                                       const ArgWord &Size,
                                       const Span<ArgVal> &args) {
    embed_vararg_rodata(args, ARG5);

    mov_arg(ARG3, Live);
    mov_imm(ARG4, args.size());
    fragment_call(ga->get_new_map_shared());

    mov_arg(Dst, ARG1);
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

    bool dst_is_src = false;
    for (auto arg : args) {
        data.push_back(arg);
        dst_is_src |= (arg == Dst);
    }

    if (dst_is_src) {
        a.add(TMP1, HTOP, TAG_PRIMARY_BOXED);
    } else {
        auto ptr = init_destination(Dst, TMP1);
        a.add(ptr.reg, HTOP, TAG_PRIMARY_BOXED);
        flush_var(ptr);
    }

    size_t size = data.size();
    unsigned i;
    for (i = 0; i < size - 1; i += 2) {
        if ((i % 128) == 0) {
            check_pending_stubs();
        }

        auto [first, second] = load_sources(data[i], TMP2, data[i + 1], TMP3);
        a.stp(first.reg, second.reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
    }

    if (i < size) {
        mov_arg(arm::Mem(HTOP).post(sizeof(Eterm)), data[i]);
    }

    if (dst_is_src) {
        auto ptr = init_destination(Dst, TMP1);
        mov_var(ptr, TMP1);
        flush_var(ptr);
    }
}

/* ARG1 = map
 * ARG2 = key
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_shared() {
    Label generic = a.newLabel(), hashmap = a.newLabel();

    a.and_(TMP1, ARG2, imm(_TAG_PRIMARY_MASK));
    a.cmp(TMP1, imm(TAG_PRIMARY_IMMED1));
    a.b_ne(generic);

    emit_untag_ptr(ARG1, ARG1);

    /* hashmap_get_element expects node header in ARG4, flatmap_get_element
     * expects size in ARG5 */
    ERTS_CT_ASSERT_FIELD_PAIR(flatmap_t, thing_word, size);
    a.ldp(ARG4, ARG5, arm::Mem(ARG1));
    a.and_(TMP1, ARG4, imm(_HEADER_MAP_SUBTAG_MASK));
    a.cmp(TMP1, imm(HAMT_SUBTAG_HEAD_FLATMAP));
    a.b_ne(hashmap);

    emit_flatmap_get_element();

    a.bind(generic);
    {
        emit_enter_runtime_frame();

        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        a.cmp(ARG1, imm(THE_NON_VALUE));

        /* Invert ZF, we want it to be set when RET is a value. */
        a.cset(TMP1, arm::CondCode::kEQ);
        a.tst(TMP1, TMP1);

        emit_leave_runtime_frame();
        a.ret(a64::x30);
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
    auto [src, key] = load_sources(Src, ARG1, Key, ARG2);

    mov_var(ARG1, src);
    mov_var(ARG2, key);

    if (maybe_one_of<BeamTypeId::MaybeImmediate>(Key)) {
        fragment_call(ga->get_i_get_map_element_shared());
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    }

    /*
     * Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.)
     */
    if (!(Dst.isXRegister() && Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_get_map_elements(const ArgLabel &Fail,
                                                  const ArgSource &Src,
                                                  const ArgWord &Size,
                                                  const Span<ArgVal> &args) {
    Label generic = a.newLabel(), next = a.newLabel();

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

        emit_untag_ptr(TMP1, ARG1);

        ERTS_CT_ASSERT_FIELD_PAIR(flatmap_t, thing_word, size);
        a.ldp(TMP2, TMP3, arm::Mem(TMP1, offsetof(flatmap_t, thing_word)));
        a.and_(TMP2, TMP2, imm(_HEADER_MAP_SUBTAG_MASK));
        a.cmp(TMP2, imm(HAMT_SUBTAG_HEAD_FLATMAP));
        a.b_ne(generic);

        check_pending_stubs();

        /* Bump size by 1 to slide past the `keys` field in the map, and the
         * header word in the key array. */
        a.add(TMP3, TMP3, imm(1));

        /* Adjust our map pointer to the `keys` field before loading it. This
         * saves us from having to bump it to point at the values later on. */
        ERTS_CT_ASSERT(sizeof(flatmap_t) ==
                       offsetof(flatmap_t, keys) + sizeof(Eterm));
        a.ldr(TMP2, arm::Mem(TMP1).pre(offsetof(flatmap_t, keys)));
        emit_untag_ptr(TMP2, TMP2);

        for (ssize_t i = args.size() - 3; i >= 0; i -= 3) {
            Label loop = a.newLabel();

            a.bind(loop);
            {
                a.subs(TMP3, TMP3, imm(1));
                a.b_eq(resolve_beam_label(Fail, disp1MB));

                a.ldr(TMP4, arm::Mem(TMP2, TMP3, arm::lsl(3)));

                const auto &Comparand = args[i];
                cmp_arg(TMP4, Comparand);
                a.b_ne(loop);
            }

            /* Don't store the result if the destination is the scratch X
             * register. (This instruction was originally a has_map_fields
             * instruction.) */
            const auto &Dst = args[i + 1];
            if (!(Dst.isXRegister() &&
                  Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
                mov_arg(Dst, arm::Mem(TMP1, TMP3, arm::lsl(3)));
            }
        }

        a.b(next);
    }

    a.bind(generic);
    {
        embed_vararg_rodata(args, ARG5);

        a.mov(ARG3, E);

        emit_enter_runtime<Update::eXRegs>();

        mov_imm(ARG4, args.size() / 3);
        load_x_reg_array(ARG2);
        runtime_call<5>(beam_jit_get_map_elements);

        emit_leave_runtime<Update::eXRegs>();

        a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

/* ARG1 = map
 * ARG2 = key
 * ARG3 = key hash
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_hash_shared() {
    Label hashmap = a.newLabel();

    emit_untag_ptr(ARG1, ARG1);

    /* hashmap_get_element expects node header in ARG4, flatmap_get_element
     * expects size in ARG5 */
    ERTS_CT_ASSERT_FIELD_PAIR(flatmap_t, thing_word, size);
    a.ldp(ARG4, ARG5, arm::Mem(ARG1));
    a.and_(TMP1, ARG4, imm(_HEADER_MAP_SUBTAG_MASK));
    a.cmp(TMP1, imm(HAMT_SUBTAG_HEAD_FLATMAP));
    a.b_ne(hashmap);
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

    if (Key.isImmed()) {
        fragment_call(ga->get_i_get_map_element_hash_shared());
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        emit_enter_runtime();
        runtime_call<3>(get_map_element_hash);
        emit_leave_runtime();

        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    }

    /*
     * Don't store the result if the destination is the scratch X register.
     * (This instruction was originally a has_map_fields instruction.)
     */
    if (!(Dst.isXRegister() && Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
        mov_arg(Dst, ARG1);
    }
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_assoc);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/* ARG2 = key
 * ARG3 = value
 * ARG4 = map
 */
void BeamGlobalAssembler::emit_update_map_single_assoc_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_maps_put);

    emit_leave_runtime<Update::eHeapAlloc>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_update_map_assoc(const ArgSource &Src,
                                                const ArgRegister &Dst,
                                                const ArgWord &Live,
                                                const ArgWord &Size,
                                                const Span<ArgVal> &args) {
    ASSERT(Size.get() == args.size());

    if (args.size() == 2) {
        emit_load_args(args[0], ARG2, args[1], ARG3, Src, ARG4);
        fragment_call(ga->get_update_map_single_assoc_shared());
    } else {
        auto src = load_source(Src, TMP1);

        embed_vararg_rodata(args, ARG5);

        mov_arg(ArgXRegister(Live.get()), src.reg);
        mov_arg(ARG3, Live);
        mov_imm(ARG4, args.size());

        fragment_call(ga->get_update_map_assoc_shared());
    }
    mov_arg(Dst, ARG1);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    Label error = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<5>(erts_gc_update_map_exact);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.ret(a64::x30);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        a.b(labels[raise_exception]);
    }
}

/* ARG2 = key
 * ARG3 = value
 * ARG4 = map
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_single_exact_body_shared() {
    Label error = a.newLabel();

    a.str(ARG2, TMP_MEM2q);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    lea(ARG5, TMP_MEM1q);
    runtime_call<5>(erts_maps_update);

    emit_leave_runtime<Update::eHeapAlloc>();
    emit_leave_runtime_frame();

    a.cbz(ARG1.w(), error);

    a.ldr(ARG1, TMP_MEM1q);
    a.ret(a64::x30);

    a.bind(error);
    {
        a.ldr(TMP2, TMP_MEM2q);
        mov_imm(TMP1, BADKEY);
        ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
        a.stp(TMP1, TMP2, arm::Mem(c_p, offsetof(Process, freason)));
        mov_imm(ARG4, 0);
        a.b(labels[raise_exception]);
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
        emit_load_args(args[0], ARG2, args[1], ARG3, Src, ARG4);
        fragment_call(ga->get_update_map_single_exact_body_shared());
    } else {
        auto src = load_source(Src, ARG4);
        embed_vararg_rodata(args, ARG5);
        mov_arg(ArgXRegister(Live.get()), src.reg);
        mov_arg(ARG3, Live);
        mov_imm(ARG4, args.size());

        if (Fail.get() != 0) {
            fragment_call(ga->get_update_map_exact_guard_shared());
            emit_branch_if_not_value(ARG1,
                                     resolve_beam_label(Fail, dispUnknown));
        } else {
            fragment_call(ga->get_update_map_exact_body_shared());
        }
    }

    mov_arg(Dst, ARG1);
}
