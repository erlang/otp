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

#ifdef BEAMASM

#    include "sys.h"
#    include "bif.h"
#    include "erl_process.h"
#    include "beam_code.h"
#    include "beam_file.h"
#    include "beam_common.h"

/* Global configuration variables */
#    ifdef HAVE_LINUX_PERF_SUPPORT
#        define BEAMASM_PERF_DUMP (1 << 0)
#        define BEAMASM_PERF_MAP (1 << 1)
extern int erts_jit_perf_support;
#    endif

void beamasm_init(void);
void beamasm_init_perf(void);
void *beamasm_new_assembler(Eterm mod, int num_labels, int num_functions);
void beamasm_codegen(void *ba,
                     const void **native_module_exec,
                     void **native_module_rw,
                     const BeamCodeHeader *in_hdr,
                     const BeamCodeHeader **out_exec_hdr,
                     BeamCodeHeader **out_rw_hdr);
void beamasm_purge_module(const void *native_module_exec,
                          void *native_module_rw);
void beamasm_delete_assembler(void *ba);
int beamasm_emit(void *ba, unsigned specific_op, BeamOp *op);
ErtsCodePtr beamasm_get_code(void *ba, int label);
const byte *beamasm_get_rodata(void *ba, char *label);
void beamasm_embed_rodata(void *ba,
                          const char *labelName,
                          const char *buff,
                          size_t size);
void beamasm_embed_bss(void *ba, char *labelName, size_t size);

unsigned int beamasm_patch_catches(void *ba, char *rw_base);
void beamasm_patch_import(void *ba, char *rw_base, int index, BeamInstr import);
void beamasm_patch_literal(void *ba, char *rw_base, int index, Eterm lit);
void beamasm_patch_lambda(void *ba, char *rw_base, int index, BeamInstr fe);
void beamasm_patch_strings(void *ba, char *rw_base, const byte *strtab);

void beamasm_emit_call_nif(const ErtsCodeInfo *info,
                           void *normal_fptr,
                           void *lib,
                           void *dirty_fptr,
                           char *buff,
                           unsigned buff_len);
Uint beamasm_get_header(void *ba, const BeamCodeHeader **);
const ErtsCodeInfo *beamasm_get_on_load(void *ba);

/* Return the module base, for line information. */
char *beamasm_get_base(void *instance);

/* Return current instruction offset, for line information. */
size_t beamasm_get_offset(void *ba);

// Number of bytes emitted at first label in order to support trace and nif load
#    define BEAM_ASM_FUNC_PROLOGUE_SIZE 8

/*
 * The code below is used to deal with intercepting the execution of
 * a process at the start of a function. It is used by tracing and nif loading.
 *
 * In the interpreter this is solved by simply writing a new instruction as
 * the first instruction in a function. In asm mode it is not as simple as
 * our code changes have to be to executing native code.
 *
 * The solution is as follows:
 *
 *   When emitting a function the first word (or function prologue) is:
 *      0x0: jmp 6
 *      0x2: ERTS_ASM_BP_FLAG_NONE
 *      0x3: relative near call
 *      0x4: &genericBPTramp
 *      0x8: actual code for function
 *
 *   When code starts to execute it will simply see the "jmp 6" instruction
 *   which skips the prologue and starts to execute the code directly.
 *
 *   When we want to enable a certain breakpoint we set the jmp target to
 *   be 1 (which means it will land on the call instruction) and will call
 *   genericBPTramp. genericBPTramp is a label at the top of each module
 *   that contains trampolines for all flag combinations.
 *
 *   genericBPTramp:
 *      0x0: ret
 *      0x10: jmp call_nif_early
 *      0x20: call generic_bp_local
 *      0x30: call generic_bp_local
 *      0x35: jmp call_nif_early
 *
 *   Note that each target is 16 byte aligned. This is because the call target
 *   in the function prologue is updated to target the correct place when a flag
 *   is updated. So if CALL_NIF_EARLY is set, then it is updated to be
 *   genericBPTramp + 0x10. If BP is set, it is updated to genericBPTramp + 0x20
 *   and the combination makes it be genericBPTramp + 0x30.
 */

enum erts_asm_bp_flag {
    ERTS_ASM_BP_FLAG_NONE = 0,
    ERTS_ASM_BP_FLAG_CALL_NIF_EARLY = 1 << 0,
    ERTS_ASM_BP_FLAG_BP = 1 << 1,
    ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY =
            ERTS_ASM_BP_FLAG_CALL_NIF_EARLY | ERTS_ASM_BP_FLAG_BP
};

static inline void erts_asm_bp_set_flag(ErtsCodeInfo *ci,
                                        enum erts_asm_bp_flag flag) {
    BeamInstr volatile *code_ptr = (BeamInstr *)erts_codeinfo_to_code(ci);
    BeamInstr code = *code_ptr;
    byte *codebytes = (byte *)&code;
    Uint32 *code32 = (Uint32 *)(codebytes + 4);
    /* 0xEB = relative jmp, 0xE8 = relative call */
    ASSERT(codebytes[0] == 0xEB && codebytes[3] == 0xE8);
    codebytes[1] = 1;
    codebytes[2] |= flag;
    *code32 += flag * 16;
    code_ptr[0] = code;
}

static inline enum erts_asm_bp_flag erts_asm_bp_get_flags(ErtsCodeInfo *ci) {
    byte *codebytes = (byte *)erts_codeinfo_to_code(ci);
    return (enum erts_asm_bp_flag)codebytes[2];
}

static inline void erts_asm_bp_unset_flag(ErtsCodeInfo *ci,
                                          enum erts_asm_bp_flag flag) {
    BeamInstr volatile *code_ptr = (BeamInstr *)erts_codeinfo_to_code(ci);
    BeamInstr code = *code_ptr;
    byte *codebytes = (byte *)&code;
    Uint32 *code32 = (Uint32 *)(codebytes + 4);
    /* 0xEB = relative jmp, 0xE8 = relative call */
    ASSERT(codebytes[0] == 0xEB && codebytes[3] == 0xE8);
    codebytes[2] &= ~flag;
    *code32 -= flag * 16;
    if (codebytes[2] == ERTS_ASM_BP_FLAG_NONE) {
        codebytes[1] = 6;
    }
    code_ptr[0] = code;
}

#endif
