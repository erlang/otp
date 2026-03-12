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
    emit_enter_runtime_frame();
    emit_enter_runtime();
    a.mov(ARG1, ARG2);
    runtime_call<1>(erts_internal_hash);
    emit_leave_runtime();
    emit_leave_runtime_frame();
    a.mov(ARG3, ARG1);
    a.bx(a32::lr);
}

/* ARG1 = untagged hash map root
 * ARG2 = key
 * ARG3 = key hash
 * ARG4 = node header
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_hashmap_get_element() {
    Label fail = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime();
    add(ARG1, ARG1, TAG_PRIMARY_BOXED);
    runtime_call<3>(get_map_element_hash);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, fail);
    mov_imm(TMP, 0);
    a.tst(TMP, TMP); /* Z = 1 on success */
    a.bx(a32::lr);

    a.bind(fail);
    mov_imm(TMP, 1);
    a.tst(TMP, TMP); /* Z = 0 on failure */
    a.bx(a32::lr);
}

/* ARG1 = untagged flat map
 * ARG2 = key
 * ARG5 = size
 *
 * Result is returned in ARG1. ZF is set on success. */
void BeamGlobalAssembler::emit_flatmap_get_element() {
    Label fail = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime();
    add(ARG1, ARG1, TAG_PRIMARY_BOXED);
    runtime_call<2>(get_map_element);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, fail);
    mov_imm(TMP, 0);
    a.tst(TMP, TMP); /* Z = 1 on success */
    a.bx(a32::lr);

    a.bind(fail);
    mov_imm(TMP, 1);
    a.tst(TMP, TMP); /* Z = 0 on failure */
    a.bx(a32::lr);
}

void BeamGlobalAssembler::emit_new_map_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    a.sub(a32::sp, a32::sp, imm(8));
    a.ldr(TMP, TMP_MEM5q);
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    runtime_call<5>(erts_gc_new_map);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();
    emit_leave_runtime_frame();
    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_new_map(const ArgRegister &Dst,
                                       const ArgWord &Live,
                                       const ArgWord &Size,
                                       const Span<ArgVal> &args) {
    embed_vararg_rodata(args, TMP);
    a.str(TMP, TMP_MEM5q);
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
        add(TMP, HTOP, TAG_PRIMARY_BOXED);
        a.str(TMP, TMP_MEM5q);
    } else {
        auto ptr = init_destination(Dst, TMP);
        add(ptr.reg, HTOP, TAG_PRIMARY_BOXED);
        flush_var(ptr);
    }

    for (const auto &value : data) {
        auto src = load_source(value, TMP);
        a.str(src.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
    }

    if (dst_is_src) {
        auto ptr = init_destination(Dst, TMP);
        a.ldr(TMP, TMP_MEM5q);
        mov_var(ptr, TMP);
        flush_var(ptr);
    }
}

/* ARG1 = map
 * ARG2 = key
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_shared() {
    Label fail = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime();
    runtime_call<2>(get_map_element);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, fail);
    mov_imm(TMP, 0);
    a.tst(TMP, TMP); /* Z = 1 on success */
    a.bx(a32::lr);

    a.bind(fail);
    mov_imm(TMP, 1);
    a.tst(TMP, TMP); /* Z = 0 on failure */
    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_i_get_map_element(const ArgLabel &Fail,
                                                 const ArgRegister &Src,
                                                 const ArgRegister &Key,
                                                 const ArgRegister &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    emit_enter_runtime();
    runtime_call<2>(get_map_element);
    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));

    if (!(Dst.isXRegister() && Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_get_map_elements(const ArgLabel &Fail,
                                                  const ArgSource &Src,
                                                  const ArgWord &Size,
                                                  const Span<ArgVal> &args) {
    ASSERT(Size.get() == args.size());
    ASSERT((Size.get() % 3) == 0);

    embed_vararg_rodata(args, TMP);

    mov_arg(ARG1, Src);
    load_x_reg_array(ARG2);
    a.mov(ARG3, E);
    mov_imm(ARG4, args.size() / 3);

    emit_enter_runtime();
    a.sub(a32::sp, a32::sp, imm(8)); /* keep AAPCS alignment */
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5: fs */
    runtime_call<5>(beam_jit_get_map_elements);
    a.add(a32::sp, a32::sp, imm(8));
    emit_leave_runtime();

    a.tst(ARG1, ARG1);
    a.b_eq(resolve_beam_label(Fail, dispUnknown));
}

/* ARG1 = map
 * ARG2 = key
 * ARG3 = key hash
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_hash_shared() {
    Label fail = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime();
    runtime_call<3>(get_map_element_hash);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, fail);
    mov_imm(TMP, 0);
    a.tst(TMP, TMP); /* Z = 1 on success */
    a.bx(a32::lr);

    a.bind(fail);
    mov_imm(TMP, 1);
    a.tst(TMP, TMP); /* Z = 0 on failure */
    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_i_get_map_element_hash(const ArgLabel &Fail,
                                                      const ArgRegister &Src,
                                                      const ArgConstant &Key,
                                                      const ArgWord &Hx,
                                                      const ArgRegister &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);
    mov_arg(ARG3, Hx);

    emit_enter_runtime();
    runtime_call<3>(get_map_element_hash);
    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    if (!(Dst.isXRegister() && Dst.as<ArgXRegister>().get() == SCRATCH_X_REG)) {
        mov_arg(Dst, ARG1);
    }
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    a.sub(a32::sp, a32::sp, imm(8));
    a.ldr(TMP, TMP_MEM5q);
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    runtime_call<5>(erts_gc_update_map_assoc);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();
    emit_leave_runtime_frame();
    a.bx(a32::lr);
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
    a.bx(a32::lr);
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
        fragment_call(ga->get_update_map_single_assoc_shared());
    } else {
        auto src = load_source(Src, TMP);
        embed_vararg_rodata(args, VAR);

        mov_arg(ArgXRegister(Live.get()), src.reg);
        mov_arg(ARG3, Live);
        mov_imm(ARG4, args.size());
        a.str(VAR, TMP_MEM5q);
        fragment_call(ga->get_update_map_assoc_shared());
    }

    mov_arg(Dst, ARG1);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    a.sub(a32::sp, a32::sp, imm(8));
    a.ldr(TMP, TMP_MEM5q);
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    runtime_call<5>(erts_gc_update_map_exact);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();
    emit_leave_runtime_frame();
    a.bx(a32::lr);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    Label error = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    a.sub(a32::sp, a32::sp, imm(8));
    a.ldr(TMP, TMP_MEM5q);
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    runtime_call<5>(erts_gc_update_map_exact);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();
    emit_leave_runtime_frame();

    emit_branch_if_not_value(ARG1, error);
    a.bx(a32::lr);

    a.bind(error);
    mov_imm(ARG4, 0);
    a.b(labels[raise_exception]);
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
    lea(TMP, TMP_MEM1q);
    a.sub(a32::sp, a32::sp, imm(8));
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    runtime_call<5>(erts_maps_update);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eHeapAlloc>();
    emit_leave_runtime_frame();

    a.tst(ARG1, ARG1);
    a.b_eq(error);

    a.ldr(ARG1, TMP_MEM1q);
    a.bx(a32::lr);

    a.bind(error);
    a.ldr(TMP, TMP_MEM2q);
    mov_imm(ARG1, BADKEY);
    a.str(ARG1, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(TMP, arm::Mem(c_p, offsetof(Process, fvalue)));
    mov_imm(ARG4, 0);
    a.b(labels[raise_exception]);
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
        auto src = load_source(Src, ARG4);
        embed_vararg_rodata(args, TMP);
        mov_arg(ArgXRegister(Live.get()), src.reg);
        mov_arg(ARG3, Live);
        mov_imm(ARG4, args.size());
        a.str(TMP, TMP_MEM5q);

        if (Fail.get() != 0) {
            fragment_call(ga->get_update_map_exact_guard_shared());
            emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
        } else {
            fragment_call(ga->get_update_map_exact_body_shared());
        }
    }

    mov_arg(Dst, ARG1);
}
