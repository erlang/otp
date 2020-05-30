/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

void BeamModuleAssembler::emit_linear_search(x86::Gp comparand,
                                             Label fail,
                                             const std::vector<ArgVal> &args) {
    int count = args.size() / 2;

    for (int i = 0; i < count; i++) {
        const ArgVal &value = args[i];
        const ArgVal &label = args[i + count];

        cmp_arg(comparand, value, ARG1);
        a.je(labels[label.getValue()]);
    }
    a.jmp(fail);
}

void BeamModuleAssembler::emit_i_select_tuple_arity(
        const ArgVal &Src,
        const ArgVal &Fail,
        const std::vector<ArgVal> &args) {
    mov_arg(ARG1, Src);
    emit_is_boxed(labels[Fail.getValue()], ARG1);
    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(ARG1, emit_boxed_val(boxed_ptr));
    a.mov(ARG2, ARG1);
    a.and_(ARG1, imm(_TAG_HEADER_MASK));

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    /* a.cmp(ARG1, _TAG_HEADER_ARITYVAL); is redundant */

    a.jne(labels[Fail.getValue()]);

    emit_linear_search(ARG2, labels[Fail.getValue()], args);
}

void BeamModuleAssembler::emit_i_select_val_lins(
        const ArgVal &Src,
        const ArgVal &Fail,
        const std::vector<ArgVal> &args) {
    mov_arg(ARG2, Src);
    emit_linear_search(ARG2, labels[Fail.getValue()], args);
}

static const BeamInstr select_val_bins(const Eterm select_val,
                                       const uint64_t count,
                                       const BeamInstr fail,
                                       const BeamInstr *data) {
    Eterm *low;
    Eterm *high;
    Eterm *mid;
    Sint bdiff;

    low = (Eterm *)data;
    high = low + count;

    while ((bdiff = (int)((char *)high - (char *)low)) > 0) {
        unsigned int boffset;
        Eterm mid_val;

        boffset = ((unsigned int)bdiff >> 1) & ~(sizeof(Eterm) - 1);
        mid = (Eterm *)((char *)low + boffset);
        mid_val = *mid;

        if (select_val < mid_val) {
            high = mid;
        } else if (select_val > mid_val) {
            low = mid + 1;
        } else {
            const BeamInstr *jump_tab = &data[count];
            return jump_tab[mid - data];
        }
    }

    return fail;
}

void BeamModuleAssembler::emit_i_select_val_bins(
        const ArgVal &Src,
        const ArgVal &Fail,
        const std::vector<ArgVal> &args) {
    /* FIXME: Emit this as a jump tree if it's relatively small. */
    Label data = embed_vararg_rodata(args, 0);

    mov_arg(ARG1, Src);

    emit_enter_runtime();

    a.mov(ARG2, imm(args.size() / 2));
    a.lea(ARG3, x86::qword_ptr(labels[Fail.getValue()]));
    a.lea(ARG4, x86::qword_ptr(data));
    runtime_call<4>(select_val_bins);

    emit_leave_runtime();

    a.jmp(RET);
}

void BeamModuleAssembler::emit_i_jump_on_val(const ArgVal &Src,
                                             const ArgVal &Fail,
                                             const ArgVal &Base,
                                             const std::vector<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, 0);

    mov_arg(ARG1, Src);

    a.mov(ARG2, ARG1);
    a.and_(ARG2, imm(_TAG_IMMED1_MASK));
    a.cmp(ARG2, imm(_TAG_IMMED1_SMALL));
    a.jne(labels[Fail.getValue()]);
    a.sar(ARG1, imm(_TAG_IMMED1_SIZE));

    if (Base.getValue() != 0) {
        if (Support::isInt32((Sint)Base.getValue())) {
            a.sub(ARG1, imm(Base.getValue()));
        } else {
            a.mov(ARG2, imm(Base.getValue()));
            a.sub(ARG1, ARG2);
        }
    }

    a.cmp(ARG1, imm(args.size()));
    a.jae(labels[Fail.getValue()]);

    a.lea(RET, x86::qword_ptr(data));
    a.jmp(x86::qword_ptr(RET, ARG1, 3));
}
