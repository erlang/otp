/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

#if defined(BEAMASM) && !defined(__BEAM_ASM_H__)
#    define __BEAM_ASM_H__

#    include "sys.h"
#    include "bif.h"
#    include "erl_fun.h"
#    include "erl_process.h"
#    include "beam_code.h"
#    include "beam_file.h"
#    include "beam_common.h"

#    if defined(__APPLE__)
#        include <libkern/OSCacheControl.h>
#    endif

/* Global configuration variables */
#    ifdef HAVE_LINUX_PERF_SUPPORT
enum beamasm_perf_flags {
    BEAMASM_PERF_DUMP = (1 << 0),
    BEAMASM_PERF_MAP = (1 << 1),
    BEAMASM_PERF_FP = (1 << 2),

    BEAMASM_PERF_ENABLED =
            BEAMASM_PERF_DUMP | BEAMASM_PERF_MAP | BEAMASM_PERF_FP,
    BEAMASM_PERF_DISABLED = 0,
};
extern enum beamasm_perf_flags erts_jit_perf_support;
#    endif
extern int erts_jit_single_map;

void beamasm_init(void);
void *beamasm_new_assembler(Eterm mod,
                            int num_labels,
                            int num_functions,
                            BeamFile *beam);
void beamasm_codegen(void *ba,
                     const void **executable_region,
                     void **writable_region,
                     const BeamCodeHeader *in_hdr,
                     const BeamCodeHeader **out_exec_hdr,
                     BeamCodeHeader **out_rw_hdr);
void *beamasm_register_metadata(void *ba, const BeamCodeHeader *header);
void beamasm_unregister_metadata(void *handle);
void beamasm_purge_module(const void *executable_region,
                          void *writable_region,
                          size_t size);
void beamasm_delete_assembler(void *ba);
int beamasm_emit(void *ba, unsigned specific_op, BeamOp *op);
void beamasm_emit_coverage(void *instance,
                           void *coverage,
                           Uint index,
                           Uint size);
ErtsCodePtr beamasm_get_code(void *ba, int label);
ErtsCodePtr beamasm_get_lambda(void *ba, int index);
const byte *beamasm_get_rodata(void *ba, char *label);
void beamasm_embed_rodata(void *ba,
                          const char *labelName,
                          const char *buff,
                          size_t size);
void beamasm_embed_bss(void *ba, char *labelName, size_t size);

unsigned int beamasm_patch_catches(void *ba, char *rw_base);
void beamasm_patch_import(void *ba,
                          char *rw_base,
                          int index,
                          const Export *import);
void beamasm_patch_literal(void *ba, char *rw_base, int index, Eterm lit);
void beamasm_patch_lambda(void *ba,
                          char *rw_base,
                          int index,
                          const ErlFunEntry *fe);
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

void beamasm_unseal_module(const void *executable_region,
                           void *writable_region,
                           size_t size);
void beamasm_seal_module(const void *executable_region,
                         void *writable_region,
                         size_t size);
void beamasm_flush_icache(const void *address, size_t size);

/* Number of bytes emitted at first label in order to support trace and nif
 * load. */
#    if defined(__aarch64__)
#        define BEAM_ASM_FUNC_PROLOGUE_SIZE 12
#    else
#        define BEAM_ASM_FUNC_PROLOGUE_SIZE 8
#    endif

/* Size (in bytes) of a ErtsNativeFunc/call_nif prologue. */
#    if defined(__aarch64__)
#        define BEAM_ASM_NFUNC_SIZE (BEAM_ASM_FUNC_PROLOGUE_SIZE + 4)
#    else
#        define BEAM_ASM_NFUNC_SIZE (BEAM_ASM_FUNC_PROLOGUE_SIZE + 8)
#    endif

/*
 * The code below is used to deal with intercepting the execution of a process
 * at the start of a function. It is used by tracing and nif loading.
 *
 * In the interpreter this is solved by simply writing a new instruction as
 * the first instruction in a function. In asm mode it is not as simple as
 * our code changes have to be to executing native code.
 *
 * On x86, the solution is as follows:
 *
 *   When emitting a function the first word (or function prologue) is:
 *      0x0: short jmp 6
 *      0x2: nop
 *      0x3: relative near call to shared breakpoint fragment
 *      0x8: actual code for function
 *
 *   When code starts to execute it will simply see the `short jmp 6`
 *   instruction which skips the prologue and starts to execute the code
 *   directly.
 *
 *   When we want to enable a certain breakpoint we set the jmp target to be 1,
 *   which means it will land on the call to the shared breakpoint fragment.
 *   This fragment checks the current `breakpoint_flag` stored in the
 *   ErtsCodeInfo of this function, and then calls `erts_call_nif_early` and
 *   `erts_generic_breakpoint` accordingly.
 *
 *   Note that the update of the branch and `breakpoint_flag` does not need to
 *   be atomic: it's fine if a process only sees one of these being updated, as
 *   the code that sets breakpoints/loads NIFs doesn't rely on the trampoline
 *   being active until thread progress has been made.
 *
 * The solution for AArch64 is similar. */

enum erts_asm_bp_flag {
    ERTS_ASM_BP_FLAG_NONE = 0,
    ERTS_ASM_BP_FLAG_CALL_NIF_EARLY = 1 << 0,
    ERTS_ASM_BP_FLAG_BP = 1 << 1,
    ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY =
            ERTS_ASM_BP_FLAG_CALL_NIF_EARLY | ERTS_ASM_BP_FLAG_BP
};

static inline enum erts_asm_bp_flag erts_asm_bp_get_flags(
        const ErtsCodeInfo *ci_exec) {
    return (enum erts_asm_bp_flag)ci_exec->u.metadata.breakpoint_flag;
}

static inline void erts_asm_bp_set_flag(ErtsCodeInfo *ci_rw,
                                        const ErtsCodeInfo *ci_exec,
                                        enum erts_asm_bp_flag flag) {
    ASSERT(flag != ERTS_ASM_BP_FLAG_NONE);
    (void)ci_exec;

    if (ci_rw->u.metadata.breakpoint_flag == ERTS_ASM_BP_FLAG_NONE) {
#    if defined(__aarch64__)
        Uint32 volatile *rw_code = (Uint32 *)erts_codeinfo_to_code(ci_rw);

        /* B .next, .enabled: BL breakpoint_handler, .next: */
        ASSERT(rw_code[1] == 0x14000002);

        /* Reroute the initial jump instruction to `.enabled`. */
        rw_code[1] = 0x14000001;
#    else /* x86_64 */
        byte volatile *rw_code = (byte *)erts_codeinfo_to_code(ci_rw);

        /* SHORT JMP .next, NOP, .enabled: CALL breakpoint_handler, .next: */
        ASSERT(rw_code[0] == 0xEB && rw_code[1] == 0x06 && rw_code[2] == 0x90 &&
               rw_code[3] == 0xE8);

        /* Reroute the initial jump instruction to `.enabled`. */
        rw_code[1] = 1;
#    endif
    }

    ci_rw->u.metadata.breakpoint_flag |= flag;
}

static inline void erts_asm_bp_unset_flag(ErtsCodeInfo *ci_rw,
                                          const ErtsCodeInfo *ci_exec,
                                          enum erts_asm_bp_flag flag) {
    ASSERT(flag != ERTS_ASM_BP_FLAG_NONE);
    (void)ci_exec;

    ci_rw->u.metadata.breakpoint_flag &= ~flag;

    if (ci_rw->u.metadata.breakpoint_flag == ERTS_ASM_BP_FLAG_NONE) {
        /* We've removed the last flag, route the branch instruction back
         * past the prologue. */

#    if defined(__aarch64__)
        Uint32 volatile *rw_code = (Uint32 *)erts_codeinfo_to_code(ci_rw);

        /* B .enabled, .enabled: BL breakpoint_handler, .next: */
        ASSERT(rw_code[1] == 0x14000001);

        /* Reroute the initial jump instruction back to `.next`. */
        ERTS_CT_ASSERT(BEAM_ASM_FUNC_PROLOGUE_SIZE == sizeof(Uint32[3]));
        rw_code[1] = 0x14000002;
#    else /* x86_64 */
        byte volatile *rw_code = (byte *)erts_codeinfo_to_code(ci_rw);

        /* SHORT JMP .enabled, NOP, .enabled: CALL breakpoint_handler, .next: */
        ASSERT(rw_code[0] == 0xEB && rw_code[1] == 0x01 && rw_code[2] == 0x90 &&
               rw_code[3] == 0xE8);

        /* Reroute the initial jump instruction back to `.next`. */
        rw_code[1] = BEAM_ASM_FUNC_PROLOGUE_SIZE - 2;
#    endif
    }
}

#endif
