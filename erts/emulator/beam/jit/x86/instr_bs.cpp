/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2021. All Rights Reserved.
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

#include "beam_asm.hpp"

extern "C"
{
#include "erl_binary.h"
#include "erl_bits.h"
#include "beam_common.h"
}

/* Clobbers RET + ARG3
 *
 * If max_size > 0, we jump to the fail label when Size > max_size
 *
 * Returns -1 when the field check always fails, 1 if it may fail, and 0 if it
 * never fails. */
int BeamModuleAssembler::emit_bs_get_field_size(const ArgVal &Size,
                                                int unit,
                                                Label fail,
                                                const x86::Gp &out,
                                                unsigned max_size) {
    if (Size.isImmed()) {
        if (is_small(Size.getValue())) {
            Sint sval = signed_val(Size.getValue());

            if (sval < 0) {
                /* badarg */
            } else if (max_size && sval > max_size) {
                /* badarg */
            } else if (sval > (MAX_SMALL / unit)) {
                /* system_limit */
            } else {
                mov_imm(out, sval * unit);
                return 0;
            }
        }

        a.jmp(fail);
        return -1;
    } else {
        mov_arg(RET, Size);

        a.mov(ARG3d, RETd);
        a.and_(ARG3d, imm(_TAG_IMMED1_MASK));
        a.cmp(ARG3d, imm(_TAG_IMMED1_SMALL));
        a.jne(fail);

        if (max_size) {
            ASSERT(Support::isInt32((Sint)make_small(max_size)));
            a.cmp(RET, imm(make_small(max_size)));
            a.ja(fail);
        }

        if (unit == 1) {
            a.sar(RET, imm(_TAG_IMMED1_SIZE));
            a.js(fail);
        } else {
            /* Untag the size but don't shift it just yet, we want to fail on
             * overflow if the final result doesn't fit into a small. */
            a.and_(RET, imm(~_TAG_IMMED1_MASK));
            a.js(fail);

            /* Size = (Size) * (Unit) */
            mov_imm(ARG3, unit);
            a.mul(ARG3); /* CLOBBERS ARG3! */
            a.jo(fail);

            a.sar(RET, imm(_TAG_IMMED1_SIZE));
        }

        if (out != RET) {
            a.mov(out, RET);
        }

        return 1;
    }
}

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgVal &Size,
                                              const ArgVal &Heap,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    mov_arg(ARG4, Size);
    mov_arg(ARG5, Heap);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    /* Must be last since mov_arg() may clobber ARG1 */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    mov_arg(Dst, RET);
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    emit_enter_runtime();
    a.mov(ARG1, c_p);
    runtime_call<2>(beam_jit_bs_field_size_argument_error);
    emit_leave_runtime();
    a.ret();
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgVal &Size,
                                                   const ArgVal &Heap,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    Label fail;

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Size, 1, fail, ARG4) >= 0) {
        mov_arg(ARG5, Heap);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        mov_arg(Dst, RET);
    }

    if (Fail.getValue() == 0) {
        Label next = a.newLabel();
        a.short_().jmp(next);

        a.bind(fail);
        {
            mov_arg(ARG2, Size);
            safe_fragment_call(ga->get_bs_size_check_shared());
            emit_raise_exception();
        }

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_init(const ArgVal &Size,
                                         const ArgVal &Live,
                                         const ArgVal &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_heap(Size, Heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_fail(const ArgVal &Size,
                                              const ArgVal &Fail,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_fail_heap(Size, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits(const ArgVal &NumBits,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    const ArgVal heap(ArgVal::Word, 0);
    emit_i_bs_init_bits_heap(NumBits, heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_heap(const ArgVal &NumBits,
                                                   const ArgVal &Alloc,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    mov_arg(ARG4, NumBits);
    mov_arg(ARG5, Alloc);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    /* Must be last since mov_arg() may clobber ARG1 */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init_bits);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail(const ArgVal &NumBits,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_bits_fail_heap(NumBits, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(const ArgVal &NumBits,
                                                        const ArgVal &Alloc,
                                                        const ArgVal &Fail,
                                                        const ArgVal &Live,
                                                        const ArgVal &Dst) {
    Label fail;

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(NumBits, 1, fail, ARG4) >= 0) {
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        /* Must be last since mov_arg() may clobber ARG1 */
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init_bits);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        mov_arg(Dst, RET);
    }

    if (Fail.getValue() == 0) {
        Label next = a.newLabel();
        a.short_().jmp(next);

        a.bind(fail);
        {
            mov_arg(ARG2, NumBits);
            safe_fragment_call(ga->get_bs_size_check_shared());
            emit_raise_exception();
        }

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_put_string(const ArgVal &Size,
                                             const ArgVal &Ptr) {
    mov_arg(ARG3, Size);

    emit_enter_runtime();

    make_move_patch(ARG2, strings, Ptr.getValue());
    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_new_bs_put_string);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgVal &Src,
                                                        const ArgVal &Fail,
                                                        const ArgVal &Sz,
                                                        const ArgVal &Flags) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);
    mov_arg(ARG4, Flags);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<4>(erts_new_bs_put_integer);

    emit_leave_runtime();

    a.test(RET, RET);

    if (Fail.getValue() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgVal &Fail,
                                                    const ArgVal &Sz,
                                                    const ArgVal &Flags,
                                                    const ArgVal &Src) {
    int unit = Flags.getValue() >> 3;
    Label next, fail;

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);
        mov_arg(ARG4, Flags);

        emit_enter_runtime();

        load_erl_bits_state(ARG1);
        runtime_call<4>(erts_new_bs_put_integer);

        emit_leave_runtime();

        a.test(RET, RET);

        if (Fail.getValue() != 0) {
            a.je(fail);
        } else {
            a.short_().jne(next);
        }
    }

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgVal &Fail,
                                                   const ArgVal &Sz,
                                                   const ArgVal &Flags,
                                                   const ArgVal &Src) {
    int unit = Flags.getValue() >> 3;
    Label next, fail;

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);

        emit_enter_runtime<Update::eReductions>();

        a.mov(ARG1, c_p);
        runtime_call<3>(erts_new_bs_put_binary);

        emit_leave_runtime<Update::eReductions>();

        a.test(RET, RET);

        if (Fail.getValue() != 0) {
            a.je(fail);
        } else {
            a.short_().jne(next);
        }
    }

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgVal &Src,
                                                       const ArgVal &Fail,
                                                       const ArgVal &Unit) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Unit);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary_all);

    emit_leave_runtime<Update::eReductions>();

    a.test(RET, RET);

    if (Fail.getValue() == 0) {
        a.jne(next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.je(resolve_beam_label(Fail));
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgVal &Fail,
                                                       const ArgVal &Sz,
                                                       const ArgVal &Src) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary);

    emit_leave_runtime<Update::eReductions>();

    a.test(RET, RET);

    if (Fail.getValue() == 0) {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.je(resolve_beam_label(Fail));
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgVal &Fail,
                                                  const ArgVal &Sz,
                                                  const ArgVal &Flags,
                                                  const ArgVal &Src) {
    int unit = Flags.getValue() >> 3;
    Label next, fail;

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);
        mov_arg(ARG4, Flags);

        emit_enter_runtime();

        a.mov(ARG1, c_p);
        runtime_call<4>(erts_new_bs_put_float);

        emit_leave_runtime();

        emit_test_the_non_value(RET);

        if (Fail.getValue() != 0) {
            a.jne(fail);
        } else {
            a.short_().je(next);
        }
    }

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgVal &Fail,
                                                      const ArgVal &Sz,
                                                      const ArgVal &Flags,
                                                      const ArgVal &Src) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);
    mov_arg(ARG4, Flags);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_new_bs_put_float);

    emit_leave_runtime();

    emit_test_the_non_value(RET);

    if (Fail.getValue() != 0) {
        a.jne(resolve_beam_label(Fail));
    } else {
        a.short_().je(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgVal &Src,
                                                 const ArgVal &Live,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Dst) {
    Label is_binary = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, Src);

    if (Fail.getValue() != 0) {
        emit_is_boxed(resolve_beam_label(Fail), Src, ARG2);
    } else {
        /* bs_start_match3 may not throw, and the compiler will only emit {f,0}
         * when it knows that the source is a match state or binary, so we're
         * free to skip the binary tests. */
    }

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG2);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));

    a.and_(RETb, imm(_HEADER_SUBTAG_MASK));
    a.cmp(RETb, imm(BIN_MATCHSTATE_SUBTAG));
#ifdef JIT_HARD_DEBUG
    a.je(next);
#else
    a.short_().je(next);
#endif

    if (Fail.getValue() != 0) {
        comment("is_binary_header");
        a.cmp(RETb, _TAG_HEADER_SUB_BIN);
        a.short_().je(is_binary);
        ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
        a.and_(RETb, imm(~4));
        a.cmp(RETb, imm(_TAG_HEADER_REFC_BIN));
        a.jne(resolve_beam_label(Fail));
    }

    a.bind(is_binary);
    {
        /* Src is not guaranteed to be inside the live range, so we need to
         * stash it during GC. */
        emit_gc_test_preserve(
                ArgVal(ArgVal::Immediate, ERL_BIN_MATCHSTATE_SIZE(0)),
                Live,
                ARG2);

        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.mov(ARG1, c_p);
        /* ARG2 was set above */
        runtime_call<2>(erts_bs_start_match_3);

        emit_leave_runtime<Update::eStack | Update::eHeap>();

        a.lea(ARG2, x86::qword_ptr(RET, TAG_PRIMARY_BOXED));
    }

    a.bind(next);
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgVal &Ctx,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Bits,
                                                 const ArgVal &Ptr) {
    const UWord size = Bits.getValue();
    Label fail = resolve_beam_label(Fail);

    mov_arg(ARG1, Ctx);

    a.mov(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));
    a.lea(ARG3, x86::qword_ptr(ARG2, size));
    a.cmp(ARG3, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.ja(fail);

    a.mov(TMP_MEM1q, ARG1);

    /* ARG4 = mb->offset & 7 */
    a.mov(ARG4, ARG2);
    a.and_(ARG4, imm(7));

    /* ARG3 = mb->base + (mb->offset >> 3) */
    a.shr(ARG2, imm(3));
    a.mov(ARG3, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.base)));
    a.add(ARG3, ARG2);

    emit_enter_runtime();

    make_move_patch(ARG1, strings, Ptr.getValue());
    mov_imm(ARG2, 0);
    mov_imm(ARG5, size);
    runtime_call<5>(erts_cmp_bits);

    emit_leave_runtime();

    a.test(RET, RET);
    a.jne(fail);

    a.mov(ARG1, TMP_MEM1q);
    a.add(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)),
          imm(size));
}

void BeamModuleAssembler::emit_i_bs_get_position(const ArgVal &Ctx,
                                                 const ArgVal &Dst) {
    mov_arg(ARG1, Ctx);

    /* Match contexts can never be literals, so we can skip clearing literal
     * tags. */
    a.mov(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));
    a.sal(ARG1, imm(_TAG_IMMED1_SIZE));
    a.or_(ARG1, imm(_TAG_IMMED1_SMALL));

    mov_arg(Dst, ARG1);
}

/* ARG3 = flags | (size << 3),
 * ARG4 = tagged match context */
void BeamGlobalAssembler::emit_bs_fixed_integer_shared() {
    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    /* Unpack size ... */
    a.mov(ARG2, ARG3);
    a.shr(ARG2, imm(3));
    /* ... flags. */
    a.and_(ARG3, imm(BSF_ALIGNED | BSF_LITTLE | BSF_SIGNED));
    a.lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));
    runtime_call<4>(erts_bs_get_integer_2);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    a.ret();
}

x86::Mem BeamModuleAssembler::emit_bs_get_integer_prologue(Label next,
                                                           Label fail,
                                                           int flags,
                                                           int size) {
    Label aligned = a.newLabel();

    a.mov(ARG2, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb.offset)));
    a.lea(ARG3, x86::qword_ptr(ARG2, size));
    a.cmp(ARG3, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb.size)));
    a.ja(fail);

    a.test(ARG2.r8(), imm(CHAR_BIT - 1));
    a.short_().je(aligned);

    /* Actually unaligned reads are quite rare, so we handle everything in a
     * shared fragment. */
    mov_imm(ARG3, flags | (size << 3));
    safe_fragment_call(ga->get_bs_fixed_integer_shared());

    /* The above call can't fail since we work on small numbers and
     * bounds-tested above. */
#ifdef JIT_HARD_DEBUG
    a.jmp(next);
#else
    a.short_().jmp(next);
#endif

    a.bind(aligned);
    {
        /* Read base address and convert offset to bytes. */
        a.mov(ARG1, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb.base)));
        a.shr(ARG2, imm(3));

        /* We cannot fail from here on; bump the match context's position. */
        a.mov(emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb.offset)),
              ARG3);

        return x86::Mem(ARG1, ARG2, 0, 0, size / 8);
    }
}

void BeamModuleAssembler::emit_i_bs_get_integer_8(const ArgVal &Ctx,
                                                  const ArgVal &Flags,
                                                  const ArgVal &Fail,
                                                  const ArgVal &Dst) {
    int flags = Flags.getValue();
    Label next = a.newLabel();
    x86::Mem address;

    mov_arg(ARG4, Ctx);

    address = emit_bs_get_integer_prologue(next,
                                           resolve_beam_label(Fail),
                                           flags,
                                           8);

    if (flags & BSF_SIGNED) {
        a.movsx(RET, address);
    } else {
        a.movzx(RET, address);
    }

    a.shl(RET, imm(_TAG_IMMED1_SIZE));
    a.or_(RET, imm(_TAG_IMMED1_SMALL));

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_get_integer_16(const ArgVal &Ctx,
                                                   const ArgVal &Flags,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Dst) {
    int flags = Flags.getValue();
    Label next = a.newLabel();
    x86::Mem address;

    mov_arg(ARG4, Ctx);

    address = emit_bs_get_integer_prologue(next,
                                           resolve_beam_label(Fail),
                                           flags,
                                           16);

    if (flags & BSF_LITTLE) {
        if (flags & BSF_SIGNED) {
            a.movsx(RET, address);
        } else {
            a.movzx(RET, address);
        }
    } else {
        if (hasCpuFeature(x86::Features::kMOVBE)) {
            a.movbe(x86::ax, address);
        } else {
            a.mov(x86::ax, address);
            a.xchg(x86::al, x86::ah);
        }

        if (flags & BSF_SIGNED) {
            a.movsx(RET, x86::ax);
        } else {
            a.movzx(RET, x86::ax);
        }
    }

    a.shl(RET, imm(_TAG_IMMED1_SIZE));
    a.or_(RET, imm(_TAG_IMMED1_SMALL));

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_get_integer_32(const ArgVal &Ctx,
                                                   const ArgVal &Flags,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Dst) {
    int flags = Flags.getValue();
    Label next = a.newLabel();
    x86::Mem address;

    mov_arg(ARG4, Ctx);

    address = emit_bs_get_integer_prologue(next,
                                           resolve_beam_label(Fail),
                                           flags,
                                           32);

    if (flags & BSF_LITTLE) {
        if (flags & BSF_SIGNED) {
            a.movsxd(RET, address);
        } else {
            /* Implicitly zero-extends to 64 bits */
            a.mov(RETd, address);
        }
    } else {
        if (hasCpuFeature(x86::Features::kMOVBE)) {
            a.movbe(RETd, address);
        } else {
            a.mov(RETd, address);
            a.bswap(RETd);
        }

        if (flags & BSF_SIGNED) {
            a.movsxd(RET, RETd);
        }
    }

    a.shl(RET, imm(_TAG_IMMED1_SIZE));
    a.or_(RET, imm(_TAG_IMMED1_SMALL));

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_get_integer_64(const ArgVal &Ctx,
                                                   const ArgVal &Flags,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    int flags = Flags.getValue();
    Label next = a.newLabel();
    x86::Mem address;

    mov_arg(ARG4, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::Immediate, BIG_UINT_HEAP_SIZE),
                          Live,
                          ARG4);

    address = emit_bs_get_integer_prologue(next,
                                           resolve_beam_label(Fail),
                                           flags,
                                           64);

    if (flags & BSF_LITTLE) {
        a.mov(RET, address);
    } else {
        if (hasCpuFeature(x86::Features::kMOVBE)) {
            a.movbe(RET, address);
        } else {
            a.mov(RET, address);
            a.bswap(RET);
        }
    }

    a.mov(ARG1, RET);
    a.mov(ARG2, RET);

    /* Speculatively make a small out of the result even though it might not
     * be one, and jump to the next instruction if it is. */
    a.shl(RET, imm(_TAG_IMMED1_SIZE));
    a.or_(RET, imm(_TAG_IMMED1_SMALL));

    if (flags & BSF_SIGNED) {
        a.sar(ARG2, imm(SMALL_BITS - 1));
        a.add(ARG2, imm(1));
        a.cmp(ARG2, imm(1));
        a.jbe(next);
    } else {
        a.shr(ARG2, imm(SMALL_BITS - 1));
        a.jz(next);
    }

    emit_enter_runtime();

    a.mov(ARG2, HTOP);
    if (flags & BSF_SIGNED) {
        runtime_call<2>(small_to_big);
    } else {
        runtime_call<2>(uword_to_big);
    }
    a.add(HTOP, imm(sizeof(Eterm) * BIG_UINT_HEAP_SIZE));

    emit_leave_runtime();

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_get_integer(const ArgVal &Ctx,
                                                const ArgVal &Fail,
                                                const ArgVal &Live,
                                                const ArgVal &FlagsAndUnit,
                                                const ArgVal &Sz,
                                                const ArgVal &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail);
    unit = FlagsAndUnit.getValue() >> 3;

    /* Clobbers RET + ARG3, returns a negative result if we always fail and
     * further work is redundant. */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG5) >= 0) {
        mov_arg(ARG3, Ctx);
        mov_arg(ARG4, FlagsAndUnit);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<6>(beam_jit_bs_get_integer);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgVal &Fail,
                                             const ArgVal &Ctx,
                                             const ArgVal &Offset) {
    ASSERT(Offset.isWord());

    mov_arg(ARG1, Ctx);

    a.mov(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.sub(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    if (Offset.getValue() != 0) {
        a.cmp(ARG2, imm(Offset.getValue()));
    }

    a.jne(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_bs_set_position(const ArgVal &Ctx,
                                               const ArgVal &Pos) {
    mov_arg(ARG1, Ctx);
    mov_arg(ARG2, Pos);

    a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
    a.mov(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)), ARG2);
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgVal &Ctx,
                                                    const ArgVal &Fail,
                                                    const ArgVal &Live,
                                                    const ArgVal &Unit,
                                                    const ArgVal &Dst) {
    unsigned unit = Unit.getValue();

    mov_arg(ARG1, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::Immediate, EXTRACT_SUB_BIN_HEAP_NEED),
                          Live,
                          ARG1);

    a.mov(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.sub(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    if ((unit & (unit - 1))) {
        /* Clobbers ARG3 */
        a.cqo();
        mov_imm(ARG4, unit);
        a.div(ARG4);
        a.test(x86::rdx, x86::rdx);
    } else {
        a.test(RETb, imm(unit - 1));
    }

    a.jne(resolve_beam_label(Fail));

    emit_enter_runtime<Update::eHeap>();

    a.lea(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));
    a.mov(ARG1, c_p);
    runtime_call<2>(erts_bs_get_binary_all_2);

    emit_leave_runtime<Update::eHeap>();

    mov_arg(Dst, RET);
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    emit_enter_runtime<Update::eHeap>();

    a.mov(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.orig)));
    a.mov(ARG3, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.base)));
    a.mov(ARG4, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    /* Extracted size = mb->size - mb->offset */
    a.mov(ARG5, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.sub(ARG5, ARG4);

    a.lea(ARG1, x86::qword_ptr(c_p, offsetof(Process, htop)));
    runtime_call<5>(erts_extract_sub_binary);

    emit_leave_runtime<Update::eHeap>();

    a.ret();
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgVal &Ctx,
                                           const ArgVal &Dst,
                                           const ArgVal &Live) {
    mov_arg(ARG1, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::Immediate, EXTRACT_SUB_BIN_HEAP_NEED),
                          Live,
                          ARG1);

    safe_fragment_call(ga->get_bs_get_tail_shared());

    mov_arg(Dst, RET);
}

/* Bits to skip are passed in RET */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgVal &Fail,
                                            const ArgVal &Ctx) {
    mov_arg(ARG1, Ctx);

    a.add(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));
    a.cmp(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.ja(resolve_beam_label(Fail));

    a.mov(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)), RET);
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgVal &Ctx,
                                               const ArgVal &Bits,
                                               const ArgVal &Fail,
                                               const ArgVal &Unit) {
    Label fail;

    fail = resolve_beam_label(Fail);

    if (emit_bs_get_field_size(Bits, Unit.getValue(), fail, RET) >= 0) {
        emit_bs_skip_bits(Fail, Ctx);
    }
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgVal &Fail,
                                                   const ArgVal &Ctx,
                                                   const ArgVal &Bits) {
    mov_arg(RET, Bits);

    emit_bs_skip_bits(Fail, Ctx);
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgVal &Ctx,
                                                const ArgVal &Fail,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const ArgVal &Flags,
                                                const ArgVal &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail);
    unit = Flags.getValue() >> 3;

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Size, unit, fail, ARG2) >= 0) {
        a.mov(TMP_MEM1q, ARG2);

        mov_arg(ARG4, Ctx);

        /* Ctx is not guaranteed to be inside the live range, so we need to
         * stash it during GC. */
        emit_gc_test_preserve(
                ArgVal(ArgVal::Immediate, EXTRACT_SUB_BIN_HEAP_NEED),
                Live,
                ARG4);

        emit_enter_runtime<Update::eHeap>();

        a.mov(ARG1, c_p);
        a.mov(ARG2, TMP_MEM1q);
        mov_imm(ARG3, Flags.getValue());
        a.lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));
        runtime_call<4>(erts_bs_get_binary_2);

        emit_leave_runtime<Update::eHeap>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgVal &Ctx,
                                               const ArgVal &Fail,
                                               const ArgVal &Live,
                                               const ArgVal &Sz,
                                               const ArgVal &Flags,
                                               const ArgVal &Dst) {
    Label fail;
    Sint unit;

    fail = resolve_beam_label(Fail);
    unit = Flags.getValue() >> 3;

    mov_arg(ARG4, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::Immediate, FLOAT_SIZE_OBJECT),
                          Live,
                          ARG4);

    if (emit_bs_get_field_size(Sz, unit, fail, ARG2, 64) >= 0) {
        emit_enter_runtime<Update::eHeap>();

        a.mov(ARG1, c_p);
        mov_imm(ARG3, Flags.getValue());
        a.lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));
        runtime_call<4>(erts_bs_get_float_2);

        emit_leave_runtime<Update::eHeap>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgVal &Src,
                                              const ArgVal &Dst) {
    Label next = a.newLabel();

    mov_arg(ARG1, Src);

    mov_imm(RET, make_small(1));
    a.cmp(ARG1, imm(make_small(0x80UL)));
    a.short_().jl(next);
    mov_imm(RET, make_small(2));
    a.cmp(ARG1, imm(make_small(0x800UL)));
    a.short_().jl(next);
    mov_imm(RET, make_small(3));
    a.cmp(ARG1, imm(make_small(0x10000UL)));
    a.short_().jl(next);
    mov_imm(RET, make_small(4));

    a.bind(next);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgVal &Fail,
                                             const ArgVal &Src) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<2>(erts_bs_put_utf8);

    emit_leave_runtime();

    a.test(RET, RET);

    if (Fail.getValue() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgVal &Ctx,
                                           const ArgVal &Fail) {
    mov_arg(ARG1, Ctx);

    emit_enter_runtime();

    a.lea(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));
    runtime_call<1>(erts_bs_get_utf8);

    emit_leave_runtime();

    emit_test_the_non_value(RET);
    a.je(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgVal &Ctx,
                                             const ArgVal &Fail,
                                             const ArgVal &Dst) {
    emit_bs_get_utf8(Ctx, Fail);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgVal &Ctx,
                                              const ArgVal &Fail) {
    emit_bs_get_utf8(Ctx, Fail);
}

void BeamModuleAssembler::emit_i_bs_utf16_size(const ArgVal &Src,
                                               const ArgVal &Dst) {
    mov_arg(ARG1, Src);

    mov_imm(RET, make_small(2));
    mov_imm(ARG2, make_small(4));
    a.cmp(ARG1, imm(make_small(0x10000UL)));
    a.cmovae(RET, ARG2);

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_put_utf16(const ArgVal &Fail,
                                              const ArgVal &Flags,
                                              const ArgVal &Src) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    /* mov_arg may clobber ARG1 */
    mov_arg(ARG3, Flags);
    mov_arg(ARG2, Src);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_bs_put_utf16);

    emit_leave_runtime();

    a.test(RET, RET);

    if (Fail.getValue() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgVal &Ctx,
                                            const ArgVal &Fail,
                                            const ArgVal &Flags) {
    mov_arg(ARG1, Ctx);

    emit_enter_runtime();

    a.lea(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));
    mov_imm(ARG2, Flags.getValue());
    runtime_call<2>(erts_bs_get_utf16);

    emit_leave_runtime();

    emit_test_the_non_value(RET);
    a.je(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgVal &Ctx,
                                              const ArgVal &Fail,
                                              const ArgVal &Flags,
                                              const ArgVal &Dst) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgVal &Ctx,
                                               const ArgVal &Fail,
                                               const ArgVal &Flags) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                x86::Gp value) {
    a.mov(ARG3d, value.r32());
    a.and_(ARG3d, imm(_TAG_IMMED1_MASK));
    a.cmp(ARG3d, imm(_TAG_IMMED1_SMALL));
    a.jne(fail);

    a.cmp(value, imm(make_small(0xD800UL)));
    a.jb(next);
    a.cmp(value, imm(make_small(0xDFFFUL)));
    a.jbe(fail);
    a.cmp(value, imm(make_small(0x10FFFFUL)));
    a.ja(fail);

    a.jmp(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgVal &Fail,
                                                     const ArgVal &Src) {
    Label fail, next = a.newLabel();

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
    }

    mov_arg(ARG1, Src);
    emit_validate_unicode(next, fail, ARG1);

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(const ArgVal &Fail,
                                                             const ArgVal &Src,
                                                             const ArgVal &Ms) {
    Label fail = a.newLabel(), next = a.newLabel();

    mov_arg(ARG1, Src);

    emit_validate_unicode(next, fail, ARG1);

    a.bind(fail);
    {
        mov_arg(ARG1, Ms);

        a.sub(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)),
              imm(32));

        if (Fail.getValue() != 0) {
            a.jmp(resolve_beam_label(Fail));
        } else {
            emit_error(BADARG);
        }
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgVal &Fail,
                                            const ArgVal &Ctx,
                                            const ArgVal &Unit) {
    unsigned int unit = Unit.getValue();

    mov_arg(ARG1, Ctx);

    a.mov(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.sub(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    if ((unit & (unit - 1))) {
        /* Clobbers ARG3 */
        a.cqo();
        mov_imm(ARG1, unit);
        a.div(ARG1);
        a.test(x86::rdx, x86::rdx);
    } else {
        a.test(RETb, imm(unit - 1));
    }

    a.jnz(resolve_beam_label(Fail));
}

/* Set the error reason when bs_add has failed. */
void BeamGlobalAssembler::emit_bs_add_shared() {
    emit_enter_runtime();
    a.mov(ARG1, c_p);
    runtime_call<3>(beam_jit_bs_add_argument_error);
    emit_leave_runtime();
    a.ret();
}

void BeamModuleAssembler::emit_bs_add(const ArgVal &Fail,
                                      const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Unit,
                                      const ArgVal &Dst) {
    Label fail;

    if (Fail.getValue() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
    }

    /* Both arguments must be immediates on x64. */
    mov_arg(ARG1, Src1);
    if (Src2.getType() == ArgVal::Immediate) {
        a.mov(RETd, ARG1d);
    } else {
        mov_arg(ARG2, Src2);
        a.mov(RETd, ARG2d);
        if (Src1.getType() != ArgVal::Immediate) {
            a.and_(RETd, ARG1d);
        }
    }
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.jne(fail);

    /* Verify that ARG2 >= 0 and multiply ARG2 by the unit. The
     * result will be untagged but not shifted and stored in RET. */
    if (Src2.getType() == ArgVal::Immediate) {
        Uint val = unsigned_val(Src2.getValue());
        if ((val >> (sizeof(Eterm) - 1) * 8) != 0) {
            /* Protect against negative or huge literal size. */
            a.jmp(fail);
            return;
        } else {
            val = (Unit.getValue() * val) << _TAG_IMMED1_SIZE;
            mov_imm(RET, val);
        }
    } else {
        a.and_(ARG2, imm(~_TAG_IMMED1_MASK));
        a.js(fail);
        /* Multiply ARG2 by unit. */
        if (Unit.getValue() == 1) {
            a.mov(RET, ARG2);
        } else {
            mov_imm(RET, Unit.getValue());
            a.mul(ARG2); /* CLOBBERS RDX = ARG3! */
            a.jo(fail);
        }
    }

    /* Verify that ARG1 >= 0. */
    a.test(ARG1, ARG1);
    a.js(fail);

    /* RET is untagged but shifted, so adding ARG1 tags it and sets the overflow
     * flag when the result won't fit an immediate. */
    a.add(RET, ARG1);

    if (Fail.getValue() != 0) {
        a.jo(fail);
    } else {
        Label next = a.newLabel();

        a.short_().jno(next);

        a.bind(fail);
        {
            mov_arg(ARG2, Src1);
            mov_arg(ARG3, Src2);
            safe_fragment_call(ga->get_bs_add_shared());
            emit_raise_exception();
        }

        a.bind(next);
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_append(const ArgVal &Fail,
                                           const ArgVal &ExtraHeap,
                                           const ArgVal &Live,
                                           const ArgVal &Unit,
                                           const ArgVal &Size,
                                           const ArgVal &Bin,
                                           const ArgVal &Dst) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG3, Live);
    mov_arg(ARG4, Size);
    mov_arg(ARG5, ExtraHeap);
    mov_arg(ARG6, Unit);

    mov_arg(ArgVal(ArgVal::XReg, Live.getValue()), Bin);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<6>(erts_bs_append);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    emit_test_the_non_value(RET);

    if (Fail.getValue() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        /* The error has been prepared in `erts_bs_append` */
        emit_raise_exception();
        a.bind(next);
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgVal &Fail,
                                                   const ArgVal &Unit,
                                                   const ArgVal &Size,
                                                   const ArgVal &Src,
                                                   const ArgVal &Dst) {
    Label next;

    if (Fail.getValue() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Size);
    mov_arg(ARG4, Unit);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_bs_private_append);

    emit_leave_runtime();

    emit_test_the_non_value(RET);

    if (Fail.getValue() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        /* The error has been prepared in `erts_bs_private_append` */
        emit_raise_exception();
        a.bind(next);
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_bs_init_writable() {
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    a.mov(ARG2, getXRef(0));
    runtime_call<2>(erts_bs_init_writable);
    a.mov(getXRef(0), RET);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    emit_enter_runtime<Update::eStack | Update::eHeap>();

    /* ARG3 is already set by the caller */
    a.mov(ARG2, ARG4);
    a.mov(ARG4, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<4>(beam_jit_bs_construct_fail_info);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    /* We must align the return address to make it a proper tagged CP, in case
     * we were called with `safe_fragment_call`. This is safe because we will
     * never actually return to the return address. */
    a.pop(ARG2);
    a.and_(ARG2, imm(-8));

#ifdef NATIVE_ERLANG_STACK
    a.push(ARG2);

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
#    ifdef ERLANG_FRAME_POINTERS
        a.push(frame_pointer);
#    endif
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
    }
#endif

    mov_imm(ARG4, nullptr);
    a.jmp(labels[raise_exception_shared]);
}

struct BscSegment {
    BscSegment()
            : src(ArgVal(ArgVal::Immediate, NIL)),
              size(ArgVal(ArgVal::Immediate, NIL)), effectiveSize(-1){};

    Eterm type;
    Uint unit;
    Uint flags;
    ArgVal src;
    ArgVal size;

    Uint error_info;
    Sint effectiveSize;
};

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgVal &Fail,
                                               const ArgVal &Alloc,
                                               const ArgVal &Live0,
                                               const ArgVal &Dst,
                                               const Span<ArgVal> &args) {
    Uint num_bits = 0;
    std::size_t n = args.size();
    std::vector<BscSegment> segments;
    Label error = a.newLabel();
    Label past_error = a.newLabel();
    ArgVal Live = Live0;
    x86::Gp sizeReg;

    /*
     * Collect information about each segment and calculate sizes of
     * fixed segments.
     */
    for (std::size_t i = 0; i < n; i += 6) {
        BscSegment seg;
        JitBSCOp bsc_op;
        Uint bsc_segment;

        ASSERT(args[i].isImmed());
        ASSERT(args[i + 1].getType() == TAG_u);
        ASSERT(args[i + 2].getType() == TAG_u);
        ASSERT(args[i + 3].getType() == TAG_u);
        seg.type = args[i].getValue();
        bsc_segment = args[i + 1].getValue();
        seg.unit = args[i + 2].getValue();
        seg.flags = args[i + 3].getValue();
        seg.src = args[i + 4];
        seg.size = args[i + 5];

        switch (seg.type) {
        case am_float:
            bsc_op = BSC_OP_FLOAT;
            break;
        case am_integer:
            bsc_op = BSC_OP_INTEGER;
            break;
        case am_utf8:
            bsc_op = BSC_OP_UTF8;
            break;
        case am_utf16:
            bsc_op = BSC_OP_UTF16;
            break;
        case am_utf32:
            bsc_op = BSC_OP_UTF32;
            break;
        default:
            bsc_op = BSC_OP_BINARY;
            break;
        }

        /*
         * Save segment number and operation for use in extended
         * error information.
         */
        seg.error_info = beam_jit_set_bsc_segment_op(bsc_segment, bsc_op);

        /*
         * As soon as we have entered runtime mode, Y registers can no
         * longer be accessed in the usual way. Therefore, if the source
         * and/or size are in Y register, copy them to X registers.
         */
        if (seg.src.getType() == TAG_y) {
            ArgVal reg = ArgVal(TAG_x, Live.getValue());
            mov_arg(reg, seg.src);
            Live.val = Live.getValue() + 1;
            seg.src = reg;
        }

        if (seg.size.getType() == TAG_y) {
            ArgVal reg = ArgVal(TAG_x, Live.getValue());
            mov_arg(reg, seg.size);
            Live.val = Live.getValue() + 1;
            seg.size = reg;
        }

        if (seg.size.isImmed() && seg.unit != 0) {
            Eterm size = seg.size.getValue();
            if (is_small(size)) {
                Uint unsigned_size = unsigned_val(size);
                if ((unsigned_size >> (sizeof(Eterm) - 1) * 8) == 0) {
                    /* This multiplication cannot overflow. */
                    Uint seg_size = seg.unit * unsigned_size;
                    seg.effectiveSize = seg_size;
                    num_bits += seg_size;
                }
            }
        }

        if (seg.effectiveSize < 0 && seg.type != am_append &&
            seg.type != am_private_append) {
            sizeReg = FCALLS;
        }

        segments.insert(segments.end(), seg);
    }

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.short_().jmp(past_error);
    a.bind(error);
    {
        /*
         * ARG1 = optional bad size value; valid if BSC_VALUE_ARG1 is set in
         * ARG4
         *
         * ARG3 = optional bad size value; valid if BSC_VALUE_ARG3 is set
         * in ARG4
         *
         * ARG4 = packed error information
         */
        comment("handle error");
        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();
        if (Fail.getValue() != 0) {
            a.jmp(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_bs_create_bin_error_shared());
        }
    }

    a.bind(past_error);

    /* We count the total number of bits in an unsigned integer. To
     * avoid having to check for overflow when adding to the counter,
     * we ensure that the signed size of each segment fits in a
     * word. */
    if (sizeReg.isValid()) {
        comment("calculate sizes");
        mov_imm(sizeReg, num_bits);
    }

    /* Generate code for calculating the size of the binary to be
     * created. */
    for (auto seg : segments) {
        if (seg.effectiveSize >= 0) {
            continue;
        }

        if (seg.type == am_append || seg.type == am_private_append) {
            continue;
        }

        if (seg.size.isImmed() && seg.size.getValue() == am_all &&
            seg.type == am_binary) {
            comment("size of an entire binary");
            mov_arg(ARG1, seg.src);
            runtime_call<1>(beam_jit_bs_bit_size);
            if (Fail.getValue() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.test(RET, RET);
            a.js(error);
            a.add(sizeReg, RET);
        } else if (seg.unit != 0) {
            comment("size binary/integer/float/string");
            mov_arg(ARG1, seg.size);
            a.mov(RETd, ARG1d);
            a.and_(RETb, imm(_TAG_IMMED1_MASK));
            a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
            if (Fail.getValue() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_DEPENDS,
                                                        BSC_INFO_SIZE,
                                                        BSC_VALUE_ARG1));
            }
            a.jne(error);
            a.mov(RET, ARG1);
            a.sar(RET, imm(_TAG_IMMED1_SIZE));
            a.js(error);
            if (seg.unit != 1) {
                if (Fail.getValue() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(
                                    seg.error_info,
                                    BSC_REASON_SYSTEM_LIMIT,
                                    BSC_INFO_SIZE,
                                    BSC_VALUE_ARG1));
                }
                a.imul(RET, RET, imm(seg.unit));
                a.jo(error);
            }
            a.add(sizeReg, RET);
        } else {
            switch (seg.type) {
            case am_utf8: {
                Label next = a.newLabel();

                comment("size utf8");
                mov_arg(ARG1, seg.src);

                mov_imm(RET, 0);
                a.mov(RETb, imm(1 * 8));
                a.cmp(ARG1, imm(make_small(0x80UL)));
                a.short_().jl(next);

                a.mov(RETb, imm(2 * 8));
                a.cmp(ARG1, imm(make_small(0x800UL)));
                a.short_().jl(next);

                a.mov(RETb, imm(3 * 8));
                a.cmp(ARG1, imm(make_small(0x10000UL)));
                a.short_().jl(next);

                a.mov(RETb, imm(4 * 8));

                a.bind(next);
                a.add(sizeReg, RET);
                break;
            }
            case am_utf16: {
                mov_arg(ARG1, seg.src);
                mov_imm(RET, 2 * 8);
                mov_imm(ARG2, 4 * 8);
                a.cmp(ARG1, imm(make_small(0x10000UL)));
                a.cmovae(RET, ARG2);
                a.add(sizeReg, RET);
                break;
            }
            case am_utf32: {
                Label next = a.newLabel();

                mov_arg(ARG1, seg.src);

                if (Fail.getValue() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                }

                a.add(sizeReg, imm(4 * 8));

                a.mov(RETd, ARG1d);
                a.and_(RETb, imm(_TAG_IMMED1_MASK));
                a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
                a.jne(error);

                a.cmp(ARG1, imm(make_small(0xD800UL)));
                a.short_().jb(next);
                a.cmp(ARG1, imm(make_small(0xDFFFUL)));
                a.jbe(error);
                a.cmp(ARG1, imm(make_small(0x10FFFFUL)));
                a.ja(error);

                a.bind(next);
                break;
            }
            default:
                ASSERT(0);
            }
        }
    }

    /* Allocate the binary. */
    if (segments[0].type == am_append) {
        BscSegment seg = segments[0];
        comment("append to binary");
        mov_arg(ARG3, Live);
        if (sizeReg.isValid()) {
            a.mov(ARG4, sizeReg);
        } else {
            mov_imm(ARG4, num_bits);
        }
        mov_arg(ARG5, Alloc);
        mov_imm(ARG6, seg.unit);
        mov_arg(ArgVal(ArgVal::XReg, Live.getValue()), seg.src);
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<6>(erts_bs_append_checked);
        if (Fail.getValue() == 0) {
            mov_arg(ARG1, ArgVal(ArgVal::XReg, Live.getValue()));
            mov_imm(ARG4,
                    beam_jit_update_bsc_reason_info(seg.error_info,
                                                    BSC_REASON_BADARG,
                                                    BSC_INFO_FVALUE,
                                                    BSC_VALUE_ARG1));
        }
        emit_test_the_non_value(RET);
        a.je(error);
    } else if (segments[0].type == am_private_append) {
        BscSegment seg = segments[0];
        comment("private append to binary");
        ASSERT(Alloc.getValue() == 0);
        mov_arg(ARG2, seg.src);
        if (sizeReg.isValid()) {
            a.mov(ARG3, sizeReg);
        } else {
            mov_imm(ARG3, num_bits);
        }
        a.mov(ARG4, seg.unit);
        a.mov(ARG1, c_p);
        runtime_call<4>(erts_bs_private_append_checked);
        /* There is no way the call can fail on a 64-bit architecture. */
    } else {
        comment("allocate binary");
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);
        load_erl_bits_state(ARG3);
        load_x_reg_array(ARG2);
        a.mov(ARG1, c_p);
        if (sizeReg.isValid()) {
            comment("(size in bits)");
            a.mov(ARG4, sizeReg);
            runtime_call<6>(beam_jit_bs_init_bits);
        } else if (num_bits % 8 == 0) {
            comment("(size in bytes)");
            mov_imm(ARG4, num_bits / 8);
            runtime_call<6>(beam_jit_bs_init);
        } else {
            mov_imm(ARG4, num_bits);
            runtime_call<6>(beam_jit_bs_init_bits);
        }
    }
    a.mov(TMP_MEM1q, RET);

    /* Build each segment of the binary. */
    for (auto seg : segments) {
        switch (seg.type) {
        case am_append:
        case am_private_append:
            break;
        case am_binary: {
            Uint error_info;

            comment("construct a binary segment");
            if (seg.effectiveSize >= 0) {
                /* The segment has a literal size. */
                mov_imm(ARG3, seg.effectiveSize);
                mov_arg(ARG2, seg.src);
                a.mov(ARG1, c_p);
                runtime_call<3>(erts_new_bs_put_binary);
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_DEPENDS,
                                                             BSC_VALUE_FVALUE);
            } else if (seg.size.isImmed() && seg.size.getValue() == am_all) {
                /* Include the entire binary/bitstring in the
                 * resulting binary. */
                a.mov(ARG3, seg.unit);
                mov_arg(ARG2, seg.src);
                a.mov(ARG1, c_p);
                runtime_call<3>(erts_new_bs_put_binary_all);
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_UNIT,
                                                             BSC_VALUE_FVALUE);
            } else {
                /* The size is a variable. We have verified that
                 * the value is a non-negative small in the
                 * appropriate range. Multiply the size with the
                 * unit. */
                mov_arg(ARG3, seg.size);
                a.sar(ARG3, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(RET, seg.unit);
                    a.mul(ARG3); /* CLOBBERS RDX = ARG3! */
                    a.mov(ARG3, RET);
                }
                mov_arg(ARG2, seg.src);
                a.mov(ARG1, c_p);
                runtime_call<3>(erts_new_bs_put_binary);
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_DEPENDS,
                                                             BSC_VALUE_FVALUE);
            }
            if (Fail.getValue() == 0) {
                mov_imm(ARG4, error_info);
            }
            a.test(RETd, RETd);
            a.je(error);
            break;
        }
        case am_float:
            comment("construct float segment");
            if (seg.effectiveSize >= 0) {
                mov_imm(ARG3, seg.effectiveSize);
            } else {
                mov_arg(ARG3, seg.size);
                a.sar(ARG3, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(RET, seg.unit);
                    a.mul(ARG3); /* CLOBBERS RDX = ARG3! */
                    a.mov(ARG3, RET);
                }
            }
            mov_arg(ARG2, seg.src);
            mov_imm(ARG4, seg.flags);
            a.mov(ARG1, c_p);
            runtime_call<4>(erts_new_bs_put_float);
            if (Fail.getValue() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_FVALUE,
                                                        BSC_VALUE_ARG1));
            }
            a.mov(ARG1, RET);
            emit_test_the_non_value(RET);
            a.jne(error);
            break;
        case am_integer:
            comment("construct integer segment");
            if (seg.effectiveSize >= 0) {
                mov_imm(ARG3, seg.effectiveSize);
            } else {
                mov_arg(ARG3, seg.size);
                a.sar(ARG3, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(RET, seg.unit);
                    a.mul(ARG3); /* CLOBBERS RDX = ARG3! */
                    a.mov(ARG3, RET);
                }
            }
            mov_arg(ARG2, seg.src);
            mov_imm(ARG4, seg.flags);
            load_erl_bits_state(ARG1);
            runtime_call<4>(erts_new_bs_put_integer);
            a.test(RETd, RETd);
            if (Fail.getValue() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.je(error);
            break;
        case am_string:
            comment("insert string");
            ASSERT(seg.effectiveSize >= 0);
            mov_imm(ARG3, seg.effectiveSize / 8);
            make_move_patch(ARG2, strings, seg.src.getValue());
            load_erl_bits_state(ARG1);
            runtime_call<3>(erts_new_bs_put_string);
            break;
        case am_utf8:
            mov_arg(ARG2, seg.src);
            load_erl_bits_state(ARG1);
            runtime_call<2>(erts_bs_put_utf8);
            if (Fail.getValue() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.test(RET, RET);
            a.je(error);
            break;
        case am_utf16:
            mov_arg(ARG2, seg.src);
            a.mov(ARG3, seg.flags);
            load_erl_bits_state(ARG1);
            runtime_call<3>(erts_bs_put_utf16);
            if (Fail.getValue() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.test(RETd, RETd);
            a.je(error);
            break;
        case am_utf32:
            mov_arg(ARG2, seg.src);
            mov_imm(ARG3, 4 * 8);
            a.mov(ARG4, seg.flags);
            load_erl_bits_state(ARG1);
            runtime_call<4>(erts_new_bs_put_integer);
            if (Fail.getValue() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.test(RETd, RETd);
            a.je(error);
            break;
        default:
            ASSERT(0);
            break;
        }
    }

    comment("done");
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    a.mov(RET, TMP_MEM1q);
    mov_arg(Dst, RET);
}
