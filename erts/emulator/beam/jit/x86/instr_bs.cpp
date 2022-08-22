/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2022. All Rights Reserved.
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
#include <numeric>

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
int BeamModuleAssembler::emit_bs_get_field_size(const ArgSource &Size,
                                                int unit,
                                                Label fail,
                                                const x86::Gp &out,
                                                unsigned max_size) {
    if (Size.isImmed()) {
        if (Size.isSmall()) {
            Sint sval = Size.as<ArgSmall>().getSigned();

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

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgWord &Size,
                                              const ArgWord &Heap,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
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

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgSource &Size,
                                                   const ArgWord &Heap,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    Label fail;

    if (Fail.get() != 0) {
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

    if (Fail.get() == 0) {
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

void BeamModuleAssembler::emit_i_bs_init(const ArgWord &Size,
                                         const ArgWord &Live,
                                         const ArgRegister &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_heap(Size, Heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_fail(const ArgRegister &Size,
                                              const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_fail_heap(Size, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits(const ArgWord &NumBits,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    const ArgVal heap(ArgVal::Word, 0);
    emit_i_bs_init_bits_heap(NumBits, heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_heap(const ArgWord &NumBits,
                                                   const ArgWord &Alloc,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
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

void BeamModuleAssembler::emit_i_bs_init_bits_fail(const ArgRegister &NumBits,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_bits_fail_heap(NumBits, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(
        const ArgSource &NumBits,
        const ArgWord &Alloc,
        const ArgLabel &Fail,
        const ArgWord &Live,
        const ArgRegister &Dst) {
    Label fail;

    if (Fail.get() != 0) {
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

    if (Fail.get() == 0) {
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

void BeamModuleAssembler::emit_bs_put_string(const ArgWord &Size,
                                             const ArgBytePtr &Ptr) {
    mov_arg(ARG3, Size);

    emit_enter_runtime();

    mov_arg(ARG2, Ptr);
    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_new_bs_put_string);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgSource &Src,
                                                        const ArgLabel &Fail,
                                                        const ArgWord &Sz,
                                                        const ArgWord &Flags) {
    Label next;

    if (Fail.get() == 0) {
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

    if (Fail.get() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgLabel &Fail,
                                                    const ArgRegister &Sz,
                                                    const ArgWord &Flags,
                                                    const ArgSource &Src) {
    int unit = Flags.get() >> 3;
    Label next, fail;

    if (Fail.get() != 0) {
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

        if (Fail.get() != 0) {
            a.je(fail);
        } else {
            a.short_().jne(next);
        }
    }

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgLabel &Fail,
                                                   const ArgSource &Sz,
                                                   const ArgWord &Flags,
                                                   const ArgSource &Src) {
    int unit = Flags.get() >> 3;
    Label next, fail;

    if (Fail.get() != 0) {
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

        if (Fail.get() != 0) {
            a.je(fail);
        } else {
            a.short_().jne(next);
        }
    }

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgSource &Src,
                                                       const ArgLabel &Fail,
                                                       const ArgWord &Unit) {
    Label next;

    if (Fail.get() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Unit);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary_all);

    emit_leave_runtime<Update::eReductions>();

    a.test(RET, RET);

    if (Fail.get() == 0) {
        a.jne(next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.je(resolve_beam_label(Fail));
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgLabel &Fail,
                                                       const ArgWord &Sz,
                                                       const ArgSource &Src) {
    Label next;

    if (Fail.get() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary);

    emit_leave_runtime<Update::eReductions>();

    a.test(RET, RET);

    if (Fail.get() == 0) {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.je(resolve_beam_label(Fail));
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgLabel &Fail,
                                                  const ArgRegister &Sz,
                                                  const ArgWord &Flags,
                                                  const ArgSource &Src) {
    int unit = Flags.get() >> 3;
    Label next, fail;

    if (Fail.get() != 0) {
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

        if (Fail.get() != 0) {
            a.jne(fail);
        } else {
            a.short_().je(next);
        }
    }

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgLabel &Fail,
                                                      const ArgWord &Sz,
                                                      const ArgWord &Flags,
                                                      const ArgSource &Src) {
    Label next;

    if (Fail.get() == 0) {
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

    if (Fail.get() != 0) {
        a.jne(resolve_beam_label(Fail));
    } else {
        a.short_().je(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgRegister &Src,
                                                 const ArgWord &Live,
                                                 const ArgLabel &Fail,
                                                 const ArgRegister &Dst) {
    Label is_binary = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, Src);

    if (Fail.get() != 0) {
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

    if (Fail.get() != 0) {
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
        emit_gc_test_preserve(ArgWord(ERL_BIN_MATCHSTATE_SIZE(0)),
                              Live,
                              Src,
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

void BeamModuleAssembler::emit_i_bs_match_string(const ArgRegister &Ctx,
                                                 const ArgLabel &Fail,
                                                 const ArgWord &Bits,
                                                 const ArgBytePtr &Ptr) {
    const UWord size = Bits.get();
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

    mov_arg(ARG1, Ptr);
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

void BeamModuleAssembler::emit_i_bs_get_position(const ArgRegister &Ctx,
                                                 const ArgRegister &Dst) {
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

void BeamModuleAssembler::emit_i_bs_get_integer_8(const ArgRegister &Ctx,
                                                  const ArgWord &Flags,
                                                  const ArgLabel &Fail,
                                                  const ArgRegister &Dst) {
    int flags = Flags.get();
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

void BeamModuleAssembler::emit_i_bs_get_integer_16(const ArgRegister &Ctx,
                                                   const ArgWord &Flags,
                                                   const ArgLabel &Fail,
                                                   const ArgRegister &Dst) {
    int flags = Flags.get();
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
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
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

void BeamModuleAssembler::emit_i_bs_get_integer_32(const ArgRegister &Ctx,
                                                   const ArgWord &Flags,
                                                   const ArgLabel &Fail,
                                                   const ArgRegister &Dst) {
    int flags = Flags.get();
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
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
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

void BeamModuleAssembler::emit_i_bs_get_integer_64(const ArgRegister &Ctx,
                                                   const ArgWord &Flags,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    int flags = Flags.get();
    Label next = a.newLabel();
    x86::Mem address;

    mov_arg(ARG4, Ctx);

    emit_gc_test_preserve(ArgWord(BIG_UINT_HEAP_SIZE), Live, Ctx, ARG4);

    address = emit_bs_get_integer_prologue(next,
                                           resolve_beam_label(Fail),
                                           flags,
                                           64);

    if (flags & BSF_LITTLE) {
        a.mov(RET, address);
    } else {
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
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

void BeamModuleAssembler::emit_i_bs_get_integer(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgWord &FlagsAndUnit,
                                                const ArgSource &Sz,
                                                const ArgRegister &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail);
    unit = FlagsAndUnit.get() >> 3;

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

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    mov_arg(ARG1, Ctx);

    a.mov(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.sub(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    if (Offset.get() != 0) {
        if (Support::isInt32(Offset.get())) {
            a.cmp(ARG2, imm(Offset.get()));
        } else {
            mov_imm(RET, Offset.get());
            a.cmp(ARG2, RET);
        }
    }

    a.jne(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    mov_arg(ARG1, Ctx);
    mov_arg(ARG2, Pos);

    a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
    a.mov(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)), ARG2);
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgRegister &Ctx,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Live,
                                                    const ArgWord &Unit,
                                                    const ArgRegister &Dst) {
    unsigned unit = Unit.get();

    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(EXTRACT_SUB_BIN_HEAP_NEED), Live, Ctx, ARG1);

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

void BeamModuleAssembler::emit_bs_get_tail(const ArgRegister &Ctx,
                                           const ArgRegister &Dst,
                                           const ArgWord &Live) {
    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(EXTRACT_SUB_BIN_HEAP_NEED), Live, Ctx, ARG1);

    safe_fragment_call(ga->get_bs_get_tail_shared());

    mov_arg(Dst, RET);
}

/* Bits to skip are passed in RET */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgLabel &Fail,
                                            const ArgRegister &Ctx) {
    mov_arg(ARG1, Ctx);

    a.add(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));
    a.cmp(RET, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.ja(resolve_beam_label(Fail));

    a.mov(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)), RET);
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgRegister &Ctx,
                                               const ArgRegister &Bits,
                                               const ArgLabel &Fail,
                                               const ArgWord &Unit) {
    Label fail;

    fail = resolve_beam_label(Fail);

    if (emit_bs_get_field_size(Bits, Unit.get(), fail, RET) >= 0) {
        emit_bs_skip_bits(Fail, Ctx);
    }
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgLabel &Fail,
                                                   const ArgRegister &Ctx,
                                                   const ArgWord &Bits) {
    mov_arg(RET, Bits);

    emit_bs_skip_bits(Fail, Ctx);
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgSource &Size,
                                                const ArgWord &Flags,
                                                const ArgRegister &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail);
    unit = Flags.get() >> 3;

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Size, unit, fail, ARG2) >= 0) {
        a.mov(TMP_MEM1q, ARG2);

        mov_arg(ARG4, Ctx);

        emit_gc_test_preserve(ArgWord(EXTRACT_SUB_BIN_HEAP_NEED),
                              Live,
                              Ctx,
                              ARG4);

        emit_enter_runtime<Update::eHeap>();

        a.mov(ARG1, c_p);
        a.mov(ARG2, TMP_MEM1q);
        mov_imm(ARG3, Flags.get());
        a.lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));
        runtime_call<4>(erts_bs_get_binary_2);

        emit_leave_runtime<Update::eHeap>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    Label fail;
    Sint unit;

    fail = resolve_beam_label(Fail);
    unit = Flags.get() >> 3;

    mov_arg(ARG4, Ctx);

    emit_gc_test_preserve(ArgWord(FLOAT_SIZE_OBJECT), Live, Ctx, ARG4);

    if (emit_bs_get_field_size(Sz, unit, fail, ARG2, 64) >= 0) {
        emit_enter_runtime<Update::eHeap>();

        a.mov(ARG1, c_p);
        mov_imm(ARG3, Flags.get());
        a.lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));
        runtime_call<4>(erts_bs_get_float_2);

        emit_leave_runtime<Update::eHeap>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgSource &Src,
                                              const ArgXRegister &Dst) {
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

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgLabel &Fail,
                                             const ArgSource &Src) {
    Label next;

    if (Fail.get() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG2, Src);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<2>(erts_bs_put_utf8);

    emit_leave_runtime();

    a.test(RET, RET);

    if (Fail.get() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgRegister &Ctx,
                                           const ArgLabel &Fail) {
    mov_arg(ARG1, Ctx);

    emit_enter_runtime();

    a.lea(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));
    runtime_call<1>(erts_bs_get_utf8);

    emit_leave_runtime();

    emit_test_the_non_value(RET);
    a.je(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgRegister &Ctx,
                                             const ArgLabel &Fail,
                                             const ArgRegister &Dst) {
    emit_bs_get_utf8(Ctx, Fail);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgRegister &Ctx,
                                              const ArgLabel &Fail) {
    emit_bs_get_utf8(Ctx, Fail);
}

void BeamModuleAssembler::emit_i_bs_utf16_size(const ArgSource &Src,
                                               const ArgXRegister &Dst) {
    mov_arg(ARG1, Src);

    mov_imm(RET, make_small(2));
    mov_imm(ARG2, make_small(4));
    a.cmp(ARG1, imm(make_small(0x10000UL)));
    a.cmovae(RET, ARG2);

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_put_utf16(const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgSource &Src) {
    Label next;

    if (Fail.get() == 0) {
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

    if (Fail.get() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgRegister &Ctx,
                                            const ArgLabel &Fail,
                                            const ArgWord &Flags) {
    mov_arg(ARG1, Ctx);

    emit_enter_runtime();

    a.lea(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));
    mov_imm(ARG2, Flags.get());
    runtime_call<2>(erts_bs_get_utf16);

    emit_leave_runtime();

    emit_test_the_non_value(RET);
    a.je(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgRegister &Ctx,
                                              const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgRegister &Dst) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Flags) {
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

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgLabel &Fail,
                                                     const ArgSource &Src) {
    Label fail, next = a.newLabel();

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
    }

    mov_arg(ARG1, Src);
    emit_validate_unicode(next, fail, ARG1);

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(
        const ArgLabel &Fail,
        const ArgSource &Src,
        const ArgRegister &Ms) {
    Label fail = a.newLabel(), next = a.newLabel();

    mov_arg(ARG1, Src);

    emit_validate_unicode(next, fail, ARG1);

    a.bind(fail);
    {
        mov_arg(ARG1, Ms);

        a.sub(emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)),
              imm(32));

        if (Fail.get() != 0) {
            a.jmp(resolve_beam_label(Fail));
        } else {
            emit_error(BADARG);
        }
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgLabel &Fail,
                                            const ArgRegister &Ctx,
                                            const ArgWord &Unit) {
    unsigned int unit = Unit.get();

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

void BeamModuleAssembler::emit_bs_add(const ArgLabel &Fail,
                                      const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgWord &Unit,
                                      const ArgXRegister &Dst) {
    Label fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail);
    } else {
        fail = a.newLabel();
    }

    /* Both arguments must be immediates on x64. */
    mov_arg(ARG1, Src1);

    if (Src2.isImmed()) {
        a.mov(RETd, ARG1d);
    } else {
        mov_arg(ARG2, Src2);
        a.mov(RETd, ARG2d);
        if (Src1.isImmed()) {
            a.and_(RETd, ARG1d);
        }
    }

    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.jne(fail);

    /* Verify that ARG2 >= 0 and multiply ARG2 by the unit. The
     * result will be untagged but not shifted and stored in RET. */
    if (Src2.isSmall()) {
        Uint val = Src2.as<ArgSmall>().getUnsigned();

        if ((val >> (sizeof(Eterm) - 1) * 8) != 0) {
            /* Protect against negative or huge literal size. */
            a.jmp(fail);
            return;
        } else {
            val = (Unit.get() * val) << _TAG_IMMED1_SIZE;
            mov_imm(RET, val);
        }
    } else {
        a.and_(ARG2, imm(~_TAG_IMMED1_MASK));
        a.js(fail);
        /* Multiply ARG2 by unit. */
        if (Unit.get() == 1) {
            a.mov(RET, ARG2);
        } else {
            mov_imm(RET, Unit.get());
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

    if (Fail.get() != 0) {
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

void BeamModuleAssembler::emit_i_bs_append(const ArgLabel &Fail,
                                           const ArgWord &ExtraHeap,
                                           const ArgWord &Live,
                                           const ArgWord &Unit,
                                           const ArgSource &Size,
                                           const ArgSource &Bin,
                                           const ArgRegister &Dst) {
    Label next;

    if (Fail.get() == 0) {
        next = a.newLabel();
    }

    mov_arg(ARG3, Live);
    mov_arg(ARG4, Size);
    mov_arg(ARG5, ExtraHeap);
    mov_arg(ARG6, Unit);

    mov_arg(ArgXRegister(Live.get()), Bin);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<6>(erts_bs_append);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    emit_test_the_non_value(RET);

    if (Fail.get() != 0) {
        a.je(resolve_beam_label(Fail));
    } else {
        a.short_().jne(next);
        /* The error has been prepared in `erts_bs_append` */
        emit_raise_exception();
        a.bind(next);
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgLabel &Fail,
                                                   const ArgWord &Unit,
                                                   const ArgSource &Size,
                                                   const ArgRegister &Src,
                                                   const ArgXRegister &Dst) {
    Label next;

    if (Fail.get() == 0) {
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

    if (Fail.get() != 0) {
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

/*
 * ARG1 = tagged bignum term
 *
 * On return, Z is set if ARG1 is not a bignum. Otherwise, Z is clear and
 * ARG1 is the 64 least significant bits of the bignum.
 */
void BeamGlobalAssembler::emit_get_sint64_shared() {
    Label success = a.newLabel();
    Label fail = a.newLabel();

    emit_is_boxed(fail, ARG1);
    x86::Gp boxed_ptr = emit_ptr_val(ARG4, ARG1);
    a.mov(ARG2, emit_boxed_val(boxed_ptr));
    a.mov(ARG3, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.and_(ARG2, imm(_TAG_HEADER_MASK));
    a.cmp(ARG2, imm(POS_BIG_SUBTAG));
    a.je(success);

    a.cmp(ARG2, imm(NEG_BIG_SUBTAG));
    a.jne(fail);

    a.neg(ARG3);

    a.bind(success);
    {
        a.mov(ARG1, ARG3);
        /* Clear Z flag.
         *
         * ARG2 is known to be POS_BIG_SUBTAG or NEG_BIG_SUBTAG at this point.
         */
        ERTS_CT_ASSERT(POS_BIG_SUBTAG != 0 && NEG_BIG_SUBTAG != 0);
        a.test(ARG2, ARG2);
        a.ret();
    }

    a.bind(fail);
    {
        a.xor_(ARG2, ARG2); /* Set Z flag */
        a.ret();
    }
}

struct BscSegment {
    BscSegment()
            : type(am_false), unit(1), flags(0), src(ArgNil()), size(ArgNil()),
              error_info(0), effectiveSize(-1), action(action::DIRECT) {
    }

    Eterm type;
    Uint unit;
    Uint flags;
    ArgVal src;
    ArgVal size;

    Uint error_info;
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

static std::vector<BscSegment> bs_combine_segments(
        const std::vector<BscSegment> segments) {
    std::vector<BscSegment> segs;

    for (auto seg : segments) {
        switch (seg.type) {
        case am_integer: {
            if (!(0 < seg.effectiveSize && seg.effectiveSize <= 64)) {
                /* Unknown or too large size. Handle using the default
                 * DIRECT action. */
                segs.push_back(seg);
                continue;
            }

            if (seg.flags & BSF_LITTLE || segs.size() == 0 ||
                segs.back().action == BscSegment::action::DIRECT) {
                /* There are no previous compatible ACCUMULATE / STORE
                 * actions. Create the first ones. */
                seg.action = BscSegment::action::ACCUMULATE_FIRST;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            auto prev = segs.back();
            if (prev.flags & BSF_LITTLE) {
                /* Little-endian segments cannot be combined with other
                 * segments. Create new ACCUMULATE_FIRST / STORE actions. */
                seg.action = BscSegment::action::ACCUMULATE_FIRST;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            /* The current segment is compatible with the previous
             * segment. Try combining them. */
            if (prev.effectiveSize + seg.effectiveSize <= 64) {
                /* The combined values of the segments fits in the
                 * accumulator. Insert an ACCUMULATE action for the
                 * current segment before the pre-existing STORE
                 * action. */
                segs.pop_back();
                prev.effectiveSize += seg.effectiveSize;
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                segs.push_back(prev);
            } else {
                /* The size exceeds 64 bits. Can't combine. */
                seg.action = BscSegment::action::ACCUMULATE_FIRST;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
            }
            break;
        }
        default:
            segs.push_back(seg);
            break;
        }
    }
    return segs;
}

void BeamModuleAssembler::update_bin_state(x86::Gp bin_base,
                                           x86::Gp bin_offset,
                                           Sint bit_offset,
                                           Sint size,
                                           x86::Gp size_reg) {
    const int x_reg_offset = offsetof(ErtsSchedulerRegisters, x_reg_array.d);
    const int cur_bin_base =
            offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state) +
            offsetof(struct erl_bits_state, erts_current_bin_);
    const int cur_bin_offset =
            offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state) +
            offsetof(struct erl_bits_state, erts_bin_offset_);

    x86::Mem mem_bin_base =
            x86::Mem(registers, cur_bin_base - x_reg_offset, sizeof(UWord));
    x86::Mem mem_bin_offset =
            x86::Mem(registers, cur_bin_offset - x_reg_offset, sizeof(UWord));

    if (bit_offset % 8 != 0 || !Support::isInt32(bit_offset + size)) {
        /* The bit offset is unknown or not byte-aligned. Alternatively,
         * the sum of bit_offset and size does not fit in an immediate. */
        a.mov(bin_base, mem_bin_base);
        a.mov(bin_offset, mem_bin_offset);

        a.mov(ARG1, bin_offset);
        if (size_reg.isValid()) {
            a.add(mem_bin_offset, size_reg);
        } else {
            a.add(mem_bin_offset, imm(size));
        }
        a.shr(ARG1, imm(3));
        a.add(ARG1, bin_base);
    } else {
        ASSERT(size >= 0);
        ASSERT(bit_offset % 8 == 0);

        comment("optimized updating of binary construction state");
        a.mov(ARG1, mem_bin_base);
        if (bit_offset) {
            a.add(ARG1, imm(bit_offset >> 3));
        }
        a.mov(mem_bin_offset, imm(bit_offset + size));
    }
}

bool BeamModuleAssembler::need_mask(const ArgVal Val, Sint size) {
    if (size == 64) {
        return false;
    } else {
        auto [min, max] = getClampedRange(Val);
        return !(0 <= min && max >> size == 0);
    }
}

/*
 * The size of the segment is assumed to be in ARG3.
 */
void BeamModuleAssembler::set_zero(Sint effectiveSize) {
    update_bin_state(RET, ARG2, -1, -1, ARG3);

    mov_imm(RET, 0);

    if (effectiveSize < 0 || effectiveSize > 128) {
        /* Size is unknown or greater than 128. Modern CPUs have an
         * enhanced "rep stosb" instruction that in most circumstances
         * is the fastest way to clear blocks of more than 128
         * bytes. */
        Label done = a.newLabel();

        if (effectiveSize < 0) {
            a.test(ARG3, ARG3);
            a.short_().jz(done);
        }

        if (ARG1 != x86::rdi) {
            a.mov(x86::rdi, ARG1);
        }
        a.mov(x86::rcx, ARG3);
        a.add(x86::rcx, imm(7));
        a.shr(x86::rcx, imm(3));
        a.rep().stosb();

        a.bind(done);
    } else {
        /* The size is known and it is at most 128 bits. */
        Uint offset = 0;

        ASSERT(0 <= effectiveSize && effectiveSize <= 128);

        if (effectiveSize == 128) {
            a.mov(x86::Mem(ARG1, offset, 8), RET);
            offset += 8;
        }

        if (effectiveSize >= 64) {
            a.mov(x86::Mem(ARG1, offset, 8), RET);
            offset += 8;
        }

        if ((effectiveSize & 63) >= 32) {
            a.mov(x86::Mem(ARG1, offset, 4), RETd);
            offset += 4;
        }

        if ((effectiveSize & 31) >= 16) {
            a.mov(x86::Mem(ARG1, offset, 2), RET.r16());
            offset += 2;
        }

        if ((effectiveSize & 15) >= 8) {
            a.mov(x86::Mem(ARG1, offset, 1), RET.r8());
            offset += 1;
        }

        if ((effectiveSize & 7) > 0) {
            a.mov(x86::Mem(ARG1, offset, 1), RET.r8());
        }
    }
}

bool BeamModuleAssembler::bs_maybe_enter_runtime(bool entered) {
    if (!entered) {
        comment("enter runtime");
        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();
    }
    return true;
}

void BeamModuleAssembler::bs_maybe_leave_runtime(bool entered) {
    if (entered) {
        comment("leave runtime");
        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();
    }
}

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgLabel &Fail,
                                               const ArgWord &Alloc,
                                               const ArgWord &Live0,
                                               const ArgRegister &Dst,
                                               const Span<ArgVal> &args) {
    Uint num_bits = 0;
    std::size_t n = args.size();
    std::vector<BscSegment> segments;
    Label error; /* Intentionally uninitialized */
    ArgWord Live = Live0;
    x86::Gp sizeReg;
    Sint allocated_size = -1;
    bool need_error_handler = false;
    bool runtime_entered = false;

    /*
     * Collect information about each segment and calculate sizes of
     * fixed segments.
     */
    for (std::size_t i = 0; i < n; i += 6) {
        BscSegment seg;
        JitBSCOp bsc_op;
        Uint bsc_segment;

        seg.type = args[i].as<ArgImmed>().get();
        bsc_segment = args[i + 1].as<ArgWord>().get();
        seg.unit = args[i + 2].as<ArgWord>().get();
        seg.flags = args[i + 3].as<ArgWord>().get();
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
         * Test whether we can omit the code for the error handler.
         */
        switch (seg.type) {
        case am_append:
            if (std::gcd(seg.unit, getSizeUnit(seg.src)) != seg.unit) {
                need_error_handler = true;
            }
            break;
        case am_binary:
            if (!(seg.size.isAtom() && seg.size.as<ArgAtom>().get() == am_all &&
                  std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit)) {
                need_error_handler = true;
            }
            break;
        case am_integer:
            if (!always_one_of(seg.src, BEAM_TYPE_INTEGER)) {
                need_error_handler = true;
            }
            break;
        case am_private_append:
        case am_string:
            break;
        default:
            need_error_handler = true;
            break;
        }

        /*
         * As soon as we have entered runtime mode, Y registers can no
         * longer be accessed in the usual way. Therefore, if the source
         * and/or size are in Y registers, copy them to X registers. Be
         * careful to preserve any associated type information.
         */
        if (seg.src.isYRegister()) {
            auto reg =
                    seg.src.as<ArgYRegister>().copy<ArgXRegister>(Live.get());
            ASSERT(reg.typeIndex() == seg.src.as<ArgYRegister>().typeIndex());
            mov_arg(reg, seg.src);

            Live = Live + 1;
            seg.src = reg;
        }

        if (seg.size.isYRegister()) {
            auto reg =
                    seg.size.as<ArgYRegister>().copy<ArgXRegister>(Live.get());
            ASSERT(reg.typeIndex() == seg.size.as<ArgYRegister>().typeIndex());
            mov_arg(reg, seg.size);

            Live = Live + 1;
            seg.size = reg;
        }

        if (seg.size.isSmall() && seg.unit != 0) {
            Uint unsigned_size = seg.size.as<ArgSmall>().getUnsigned();

            if ((unsigned_size >> (sizeof(Eterm) - 1) * 8) == 0) {
                /* This multiplication cannot overflow. */
                Uint seg_size = seg.unit * unsigned_size;
                seg.effectiveSize = seg_size;
                num_bits += seg_size;
            }
        }

        if (seg.effectiveSize < 0 && seg.type != am_append &&
            seg.type != am_private_append) {
            sizeReg = FCALLS;
            need_error_handler = true;
        }

        segments.insert(segments.end(), seg);
    }

    /*
     * Test whether a heap binary of fixed size will result from the
     * construction. If so, allocate and construct the binary now
     * before entering the runtime mode.
     */
    if (!sizeReg.isValid() && num_bits % 8 == 0 &&
        num_bits / 8 <= ERL_ONHEAP_BIN_LIMIT && segments[0].type != am_append &&
        segments[0].type != am_private_append) {
        const int x_reg_offset =
                offsetof(ErtsSchedulerRegisters, x_reg_array.d);
        const int cur_bin_base =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state) +
                offsetof(struct erl_bits_state, erts_current_bin_);
        const int cur_bin_offset =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state) +
                offsetof(struct erl_bits_state, erts_bin_offset_);
        x86::Mem mem_bin_base =
                x86::qword_ptr(registers, cur_bin_base - x_reg_offset);
        x86::Mem mem_bin_offset =
                x86::qword_ptr(registers, cur_bin_offset - x_reg_offset);
        Uint num_bytes = num_bits / 8;

        comment("allocate heap binary");
        allocated_size = (num_bytes + 7) & (-8);

        /* Ensure that there is enough room on the heap. */
        Uint need = heap_bin_size(num_bytes) + Alloc.get();
        emit_gc_test(ArgWord(0), ArgWord(need), Live);

        /* Create the heap binary. */
        a.lea(RET, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
        a.mov(TMP_MEM1q, RET);
        a.mov(x86::qword_ptr(HTOP, 0), imm(header_heap_bin(num_bytes)));
        a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), imm(num_bytes));

        /* Initialize the erl_bin_state struct. */
        a.add(HTOP, imm(sizeof(Eterm[2])));
        a.mov(mem_bin_base, HTOP);
        a.mov(mem_bin_offset, imm(0));

        /* Update HTOP. */
        a.add(HTOP, imm(allocated_size));
    }

    if (!need_error_handler) {
        comment("(cannot fail)");
    } else {
        Label past_error = a.newLabel();

        runtime_entered = bs_maybe_enter_runtime(false);
        a.short_().jmp(past_error);

        /*
         * ARG1 = optional bad size value; valid if BSC_VALUE_ARG1 is set in
         * ARG4
         *
         * ARG3 = optional bad size value; valid if BSC_VALUE_ARG3 is set
         * in ARG4
         *
         * ARG4 = packed error information
         */
        error = a.newLabel();
        a.bind(error);
        bs_maybe_leave_runtime(runtime_entered);
        comment("handle error");
        if (Fail.get() != 0) {
            a.jmp(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_bs_create_bin_error_shared());
        }

        a.bind(past_error);
    }

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

        if (seg.size.isAtom() && seg.size.as<ArgAtom>().get() == am_all &&
            seg.type == am_binary) {
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            comment("size of an entire binary");
            mov_arg(ARG1, seg.src);
            runtime_call<1>(beam_jit_bs_bit_size);
            if (exact_type(seg.src, BEAM_TYPE_BITSTRING)) {
                comment("skipped check for success since the source "
                        "is always a bit string");
            } else {
                if (Fail.get() == 0) {
                    mov_arg(ARG1, seg.src);
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                }
                a.test(RET, RET);
                a.js(error);
            }
            a.add(sizeReg, RET);
        } else if (seg.unit != 0) {
            bool can_fail = true;
            comment("size binary/integer/float/string");

            if (std::get<0>(getClampedRange(seg.size)) >= 0) {
                /* Can't fail if size is always positive. */
                can_fail = false;
            }

            if (can_fail && Fail.get() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_DEPENDS,
                                                        BSC_INFO_SIZE,
                                                        BSC_VALUE_ARG1));
            }

            mov_arg(ARG1, seg.size);

            if (always_small(seg.size)) {
                comment("skipped test for small size since it is always small");
            } else if (always_one_of(seg.size,
                                     BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER)) {
                comment("simplified test for small size since it is a number");
                a.test(ARG1d, imm(TAG_PRIMARY_LIST));
                a.je(error);
            } else {
                a.mov(RETd, ARG1d);
                a.and_(RETb, imm(_TAG_IMMED1_MASK));
                a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
                a.jne(error);
            }

            a.mov(RET, ARG1);
            a.sar(RET, imm(_TAG_IMMED1_SIZE));
            if (can_fail) {
                a.js(error);
            }
            if (seg.unit != 1) {
                if (Fail.get() == 0) {
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

                if (Fail.get() == 0) {
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

    segments = bs_combine_segments(segments);

    /* Allocate the binary. */
    if (segments[0].type == am_append) {
        BscSegment seg = segments[0];
        runtime_entered = bs_maybe_enter_runtime(runtime_entered);
        comment("append to binary");
        mov_arg(ARG3, Live);
        if (sizeReg.isValid()) {
            a.mov(ARG4, sizeReg);
        } else {
            mov_imm(ARG4, num_bits);
        }
        mov_arg(ARG5, Alloc);
        mov_imm(ARG6, seg.unit);
        mov_arg(ArgXRegister(Live.get()), seg.src);
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<6>(erts_bs_append_checked);

        if (std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit) {
            /* There is no way the call can fail with a system_limit
             * exception on a 64-bit architecture. */
            comment("skipped test for success because units are compatible");
        } else {
            if (Fail.get() == 0) {
                mov_arg(ARG1, ArgXRegister(Live.get()));
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_FVALUE,
                                                        BSC_VALUE_ARG1));
            }
            emit_test_the_non_value(RET);
            a.je(error);
        }
        a.mov(TMP_MEM1q, RET);
    } else if (segments[0].type == am_private_append) {
        BscSegment seg = segments[0];
        runtime_entered = bs_maybe_enter_runtime(runtime_entered);
        comment("private append to binary");
        ASSERT(Alloc.get() == 0);
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
        a.mov(TMP_MEM1q, RET);
    } else if (allocated_size >= 0) {
        /* The binary has already been allocated. */
    } else {
        comment("allocate binary");
        runtime_entered = bs_maybe_enter_runtime(runtime_entered);
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);
        load_erl_bits_state(ARG3);
        load_x_reg_array(ARG2);
        a.mov(ARG1, c_p);
        if (sizeReg.isValid()) {
            a.mov(ARG4, sizeReg);
            runtime_call<6>(beam_jit_bs_init_bits);
        } else {
            allocated_size = (num_bits + 7) / 8;
            if (allocated_size <= ERL_ONHEAP_BIN_LIMIT) {
                allocated_size = (allocated_size + 7) & (-8);
            }
            mov_imm(ARG4, num_bits);
            runtime_call<6>(beam_jit_bs_init_bits);
        }
        a.mov(TMP_MEM1q, RET);
    }

    /* Keep track of the bit offset from the being of the binary.
     * Set to -1 if offset is not known (when a segment of unknown
     * size has been seen). */
    Sint bit_offset = 0;

    /* Keep track of whether the current segment is byte-aligned.  (A
     * segment can be known to be byte-aligned even if the bit offset
     * is unknown.) */
    bool is_byte_aligned = true;

    /* Build each segment of the binary. */
    for (auto seg : segments) {
        switch (seg.type) {
        case am_append:
        case am_private_append:
            bit_offset = -1;
            break;
        case am_binary: {
            Uint error_info;
            bool can_fail = true;

            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
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
            } else if (seg.size.isAtom() &&
                       seg.size.as<ArgAtom>().get() == am_all) {
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
                if (std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit) {
                    comment("skipped test for success because units are "
                            "compatible");
                    can_fail = false;
                }
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

            if (can_fail) {
                if (Fail.get() == 0) {
                    mov_imm(ARG4, error_info);
                }
                a.test(RETd, RETd);
                a.je(error);
            }
            break;
        }
        case am_float:
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
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
            if (Fail.get() == 0) {
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
            switch (seg.action) {
            case BscSegment::action::ACCUMULATE_FIRST:
            case BscSegment::action::ACCUMULATE: {
                /* Shift an integer of known size (no more than 64 bits)
                 * into a word-size accumulator. */
                Label accumulate = a.newLabel();
                Label value_is_small = a.newLabel();
                x86::Gp tmp = ARG4;
                x86::Gp bin_data = ARG5;

                comment("accumulate value for integer segment");
                if (seg.action == BscSegment::action::ACCUMULATE_FIRST) {
                    mov_imm(bin_data, 0);
                } else if (seg.effectiveSize < 64) {
                    a.shl(bin_data, imm(seg.effectiveSize));
                }
                mov_arg(ARG1, seg.src);

                if (!always_small(seg.src)) {
                    if (always_one_of(seg.src,
                                      BEAM_TYPE_INTEGER |
                                              BEAM_TYPE_MASK_ALWAYS_BOXED)) {
                        comment("simplified small test since all other types "
                                "are boxed");
                        emit_is_boxed(value_is_small, seg.src, ARG1);
                    } else {
                        a.mov(ARG2d, ARG1d);
                        a.and_(ARG2d, imm(_TAG_IMMED1_MASK));
                        a.cmp(ARG2d, imm(_TAG_IMMED1_SMALL));
                        a.short_().je(value_is_small);
                    }

                    /* The value is boxed. If it is a bignum, extract the
                     * least significant 64 bits. */
                    safe_fragment_call(ga->get_get_sint64_shared());
                    if (always_one_of(seg.src, BEAM_TYPE_INTEGER)) {
                        a.short_().jmp(accumulate);
                    } else {
                        a.short_().jne(accumulate);

                        /* Not a bignum. Signal error. */
                        if (Fail.get() == 0) {
                            mov_imm(ARG4,
                                    beam_jit_update_bsc_reason_info(
                                            seg.error_info,
                                            BSC_REASON_BADARG,
                                            BSC_INFO_TYPE,
                                            BSC_VALUE_ARG1));
                        }
                        a.jmp(error);
                    }
                }

                a.bind(value_is_small);
                a.sar(ARG1, imm(_TAG_IMMED1_SIZE));

                /* Mask (if needed) and accumulate. */
                a.bind(accumulate);
                if (seg.effectiveSize == 64) {
                    a.mov(bin_data, ARG1);
                } else if (!need_mask(seg.src, seg.effectiveSize)) {
                    comment("skipped masking because the value always fits");
                    a.or_(bin_data, ARG1);
                } else if (seg.effectiveSize == 32) {
                    a.mov(ARG1d, ARG1d);
                    a.or_(bin_data, ARG1);
                } else if (seg.effectiveSize < 32) {
                    a.and_(ARG1, (1ULL << seg.effectiveSize) - 1);
                    a.or_(bin_data, ARG1);
                } else {
                    mov_imm(tmp, (1ULL << seg.effectiveSize) - 1);
                    a.and_(ARG1, tmp);
                    a.or_(bin_data, ARG1);
                }
                break;
            }
            case BscSegment::action::STORE: {
                /* The accumulator is now full or the next segment is
                 * not possible to accumulate, so it's time to store
                 * the accumulator to the current position in the
                 * binary. */
                Label store = a.newLabel();
                Label done = a.newLabel();
                x86::Gp bin_ptr = ARG1;
                x86::Gp bin_base = ARG2;
                x86::Gp bin_offset = ARG3;
                x86::Gp tmp = ARG4;
                x86::Gp bin_data = ARG5;

                comment("construct integer segment from accumulator");

                /* First we'll need to ensure that the value in the
                 * accumulator is in little endian format. */
                if (seg.effectiveSize % 8 != 0) {
                    Uint complete_bytes = 8 * (seg.effectiveSize / 8);
                    Uint num_partial = seg.effectiveSize % 8;
                    if ((seg.flags & BSF_LITTLE) == 0) {
                        a.shl(bin_data, imm(64 - seg.effectiveSize));
                        a.bswap(bin_data);
                    } else {
                        Sint mask = (1ll << complete_bytes) - 1;
                        a.mov(RET, bin_data);
                        a.shr(RET, imm(complete_bytes));
                        a.and_(RETd, imm((1ull << num_partial) - 1));
                        a.shl(RET, imm(complete_bytes + 8 - num_partial));
                        if (Support::isInt32(mask)) {
                            a.and_(bin_data, imm(mask));
                        } else {
                            mov_imm(tmp, mask);
                            a.and_(bin_data, tmp);
                        }
                        a.or_(bin_data, RET);
                    }
                } else if ((seg.flags & BSF_LITTLE) == 0) {
                    switch (seg.effectiveSize) {
                    case 8:
                        break;
                    case 32:
                        a.bswap(bin_data.r32());
                        break;
                    case 64:
                        a.bswap(bin_data);
                        break;
                    default:
                        a.bswap(bin_data);
                        a.shr(bin_data, imm(64 - seg.effectiveSize));
                        break;
                    }
                }

                update_bin_state(bin_base,
                                 bin_offset,
                                 bit_offset,
                                 seg.effectiveSize,
                                 x86::Gp());

                if (bit_offset < 0 && !is_byte_aligned) {
                    /* Bit offset is unknown and is not known to be
                     * byte aligned. Must test alignment. */
                    a.test(bin_offset, imm(7));
                    a.short_().je(store);
                }

                if (!is_byte_aligned) {
                    /* Bit offset is unknown or known to be unaligned. */
                    runtime_entered = bs_maybe_enter_runtime(runtime_entered);
                    a.mov(TMP_MEM2q, bin_data); /* MEM1q is already in use. */
                    a.lea(ARG1, TMP_MEM2q);
                    mov_imm(tmp, seg.effectiveSize);

                    runtime_call<4>(erts_copy_bits_restricted);

                    if (bit_offset < 0) {
                        /* The bit offset is unknown, which implies
                         * that there exists store code that we will
                         * need to branch past. */
                        a.short_().jmp(done);
                    }
                }

                a.bind(store);

                if (bit_offset <= 0 || is_byte_aligned) {
                    /* Bit offset is tested or known to be
                     * byte-aligned. Emit inline code to store the
                     * value of the accumulator into the binary. */
                    int num_bytes = (seg.effectiveSize + 7) / 8;

                    /* If more than one instruction is required for
                     * doing the store, test whether it would be safe
                     * to do a single 32 or 64 bit store. */
                    switch (num_bytes) {
                    case 3:
                        if (bit_offset >= 0 &&
                            allocated_size * 8 - bit_offset >= 32) {
                            comment("simplified complicated store");
                            num_bytes = 4;
                        }
                        break;
                    case 5:
                    case 6:
                    case 7:
                        if (bit_offset >= 0 &&
                            allocated_size * 8 - bit_offset >= 64) {
                            comment("simplified complicated store");
                            num_bytes = 8;
                        }
                        break;
                    }

                    do {
                        switch (num_bytes) {
                        case 1:
                            a.mov(x86::Mem(bin_ptr, 0, 1), bin_data.r8());
                            break;
                        case 2:
                            a.mov(x86::Mem(bin_ptr, 0, 2), bin_data.r16());
                            break;
                        case 3:
                            a.mov(x86::Mem(bin_ptr, 0, 2), bin_data.r16());
                            a.shr(bin_data, imm(16));
                            a.mov(x86::Mem(bin_ptr, 2, 1), bin_data.r8());
                            break;
                        case 4:
                            a.mov(x86::Mem(bin_ptr, 0, 4), bin_data.r32());
                            break;
                        case 5:
                        case 6:
                        case 7:
                            a.mov(x86::Mem(bin_ptr, 0, 4), bin_data.r32());
                            a.add(bin_ptr, imm(4));
                            a.shr(bin_data, imm(32));
                            break;
                        case 8:
                            a.mov(x86::Mem(bin_ptr, 0, 8), bin_data);
                            num_bytes = 0;
                            break;
                        default:
                            ASSERT(0);
                        }
                        num_bytes -= 4;
                    } while (num_bytes > 0);
                }

                a.bind(done);
                break;
            }
            case BscSegment::action::DIRECT:
                /* This segment either has a size exceeding the maximum
                 * accumulator size of 64 bits or has a variable size.
                 *
                 * First load the effective size (size * unit) into ARG3.
                 */
                comment("construct integer segment");
                if (seg.effectiveSize >= 0) {
                    mov_imm(ARG3, seg.effectiveSize);
                } else {
                    mov_arg(ARG3, seg.size);
                    a.sar(ARG3, imm(_TAG_IMMED1_SIZE));
                    if (Support::isPowerOf2(seg.unit)) {
                        Uint trailing_bits = Support::ctz<Eterm>(seg.unit);
                        if (trailing_bits) {
                            a.shl(ARG3, imm(trailing_bits));
                        }
                    } else {
                        mov_imm(RET, seg.unit);
                        a.mul(ARG3); /* CLOBBERS RDX = ARG3! */
                        a.mov(ARG3, RET);
                    }
                }

                if (is_byte_aligned && seg.src.isSmall() &&
                    seg.src.as<ArgSmall>().getSigned() == 0) {
                    /* Optimize the special case of setting a known
                     * byte-aligned segment to zero. */
                    comment("optimized setting segment to 0");
                    set_zero(seg.effectiveSize);
                } else {
                    /* Call the helper function to fetch and store the
                     * integer into the binary. */
                    runtime_entered = bs_maybe_enter_runtime(runtime_entered);
                    mov_arg(ARG2, seg.src);
                    mov_imm(ARG4, seg.flags);
                    load_erl_bits_state(ARG1);
                    runtime_call<4>(erts_new_bs_put_integer);
                    if (exact_type(seg.src, BEAM_TYPE_INTEGER)) {
                        comment("skipped test for success because construction "
                                "can't fail");
                    } else {
                        if (Fail.get() == 0) {
                            mov_arg(ARG1, seg.src);
                            mov_imm(ARG4,
                                    beam_jit_update_bsc_reason_info(
                                            seg.error_info,
                                            BSC_REASON_BADARG,
                                            BSC_INFO_TYPE,
                                            BSC_VALUE_ARG1));
                        }
                        a.test(RETd, RETd);
                        a.je(error);
                    }
                }
                break;
            }
            break;
        case am_string: {
            ArgBytePtr string_ptr(
                    ArgVal(ArgVal::BytePtr, seg.src.as<ArgWord>().get()));

            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            comment("insert string");
            ASSERT(seg.effectiveSize >= 0);
            mov_imm(ARG3, seg.effectiveSize / 8);
            mov_arg(ARG2, string_ptr);
            load_erl_bits_state(ARG1);
            runtime_call<3>(erts_new_bs_put_string);
        } break;
        case am_utf8:
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            mov_arg(ARG2, seg.src);
            load_erl_bits_state(ARG1);
            runtime_call<2>(erts_bs_put_utf8);
            if (Fail.get() == 0) {
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
        case am_utf16:
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            mov_arg(ARG2, seg.src);
            a.mov(ARG3, seg.flags);
            load_erl_bits_state(ARG1);
            runtime_call<3>(erts_bs_put_utf16);
            if (Fail.get() == 0) {
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
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            mov_arg(ARG2, seg.src);
            mov_imm(ARG3, 4 * 8);
            a.mov(ARG4, seg.flags);
            load_erl_bits_state(ARG1);
            runtime_call<4>(erts_new_bs_put_integer);
            if (Fail.get() == 0) {
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

        /* Try to keep track of the bit offset. */
        if (bit_offset >= 0 && (seg.action == BscSegment::action::DIRECT ||
                                seg.action == BscSegment::action::STORE)) {
            if (seg.effectiveSize >= 0) {
                bit_offset += seg.effectiveSize;
            } else {
                bit_offset = -1;
            }
        }

        /* Try to keep track whether the next segment is byte
         * aligned. */
        if (seg.type == am_append || seg.type == am_private_append) {
            if (std::gcd(getSizeUnit(seg.src), 8) != 8) {
                is_byte_aligned = false;
            }
        } else if (bit_offset % 8 == 0) {
            is_byte_aligned = true;
        } else if (seg.effectiveSize >= 0) {
            if (seg.effectiveSize % 8 != 0) {
                is_byte_aligned = false;
            }
        } else if (std::gcd(seg.unit, 8) != 8) {
            is_byte_aligned = false;
        }
    }

    bs_maybe_leave_runtime(runtime_entered);
    comment("done");
    a.mov(RET, TMP_MEM1q);
    mov_arg(Dst, RET);
}
