/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2024. All Rights Reserved.
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

/* Description:
 *	This is the interface that facilitates changing the beam code
 *      (load,upgrade,delete) while allowing executing Erlang processes to
 *      access the code without any locks or other expensive memory barriers.
 *
 *      The basic idea is to maintain several "logical copies" of the code. These
 *      copies are identified by a global 'code index', an integer of 0, 1 or 2.
 *      The code index is used as argument to code access structures like
 *      export, module, beam_catches, beam_ranges.
 *
 *      The current 'active' code index is used to access the current running
 *      code. The 'staging' code index is used by the process that performs
 *      a code change operation. When a code change operation completes
 *      successfully, the staging code index becomes the new active code index.
 *
 *      The third code index is not explicitly used. It can be thought of as
 *      the "previous active" or the "next staging" index. It is needed to make
 *      sure that we do not reuse a code index for staging until we are sure
 *      that no executing BIFs are still referencing it.
 *      We could get by with only two (0 and 1), but that would require that we
 *      must wait for all schedulers to re-schedule before each code change
 *      operation can start staging.
 *
 *      Note that the 'code index' is very loosely coupled to the concept of
 *      'current' and 'old' module versions. You can almost say that they are
 *      orthogonal to each other. Code index is an emulator global concept while
 *      'current' and 'old' is specific for each module.
 */

#ifndef __CODE_IX_H__
#define __CODE_IX_H__

#ifndef __SYS_H__
#  ifdef HAVE_CONFIG_H
#    include "config.h"
#  endif
#  include "sys.h"
#endif

#include "beam_opcodes.h"

#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY

struct process;


#define ERTS_NUM_CODE_IX 3

#ifdef BEAMASM
#define ERTS_ADDRESSV_SIZE (ERTS_NUM_CODE_IX + 1)
#define ERTS_SAVE_CALLS_CODE_IX (ERTS_ADDRESSV_SIZE - 1)
#else
#define ERTS_ADDRESSV_SIZE ERTS_NUM_CODE_IX
#endif

/* This structure lets `Export` entries and `ErlFunEntry` share dispatch code,
 * which greatly improves the performance of fun calls. */
typedef struct ErtsDispatchable_ {
    ErtsCodePtr addresses[ERTS_ADDRESSV_SIZE];
} ErtsDispatchable;

typedef unsigned ErtsCodeIndex;

typedef struct ErtsCodeMFA_ {
    Eterm module;
    Eterm function;

    /* This is technically a byte, but the interpreter needs this to be a word
     * for argument packing to work properly, and declaring it as a byte won't
     * save any space due to tail padding. */
    Uint arity;
} ErtsCodeMFA;

/*
 * The ErtsCodeInfo structure is used both in the Export entry
 * and in the code as the function header.
 */

/* If you change the size of this, you also have to update the code
   in ops.tab to reflect the new func_info size */
typedef struct ErtsCodeInfo_ {
    /* In both the JIT and interpreter, we may jump here to raise a
     * function_clause error.
     *
     * In addition, the JIT also stores the current breakpoint flags here. */
    struct {
#ifndef BEAMASM
        BeamInstr op;
#else
        struct {
            char raise_function_clause[sizeof(BeamInstr) - 1];
            char breakpoint_flag;
        } metadata;
#endif
    } u;

    /* Trace breakpoint */
    struct GenericBp *gen_bp;
    ErtsCodeMFA mfa;
} ErtsCodeInfo;

typedef struct {
    erts_refc_t pending_schedulers;
    ErtsThrPrgrLaterOp later_op;
    UWord size;

    void (*later_function)(void *);
    void *later_data;
} ErtsCodeBarrier;

/* Get the code associated with a ErtsCodeInfo ptr. */
ERTS_GLB_INLINE
ErtsCodePtr erts_codeinfo_to_code(const ErtsCodeInfo *ci);

/* Get the ErtsCodeInfo for from a code ptr. */
ERTS_GLB_INLINE
const ErtsCodeInfo *erts_code_to_codeinfo(ErtsCodePtr I);

/* Get the code associated with a ErtsCodeMFA ptr. */
ERTS_GLB_INLINE
ErtsCodePtr erts_codemfa_to_code(const ErtsCodeMFA *mfa);

/* Get the ErtsCodeMFA from a code ptr. */
ERTS_GLB_INLINE
const ErtsCodeMFA *erts_code_to_codemfa(ErtsCodePtr I);

/* Called once at emulator initialization.
 */
void erts_code_ix_init(void);

/* Return active code index.
 * Is guaranteed to be valid until the calling BIF returns.
 * To get a consistent view of the code, only one call to erts_active_code_ix()
 * should be made and the returned ix reused within the same BIF call.
 */
ERTS_GLB_INLINE
ErtsCodeIndex erts_active_code_ix(void);

/* Return staging code ix.
 * Only used by a process performing code loading/upgrading/deleting/purging.
 * Code staging permission must be seized.
 */
ERTS_GLB_INLINE
ErtsCodeIndex erts_staging_code_ix(void);

/** @brief Try to seize exclusive code loading permission. That is, both
 * staging and modification permission.
 *
 * Main process lock (only) must be held.
 * System thread progress must not be blocked.
 * Caller must not already have the code modification or staging permissions.
 * Caller is suspended and *must* yield if 0 is returned. */
int erts_try_seize_code_load_permission(struct process* c_p);

/** @brief Release code loading permission. Resumes any suspended waiters. */
void erts_release_code_load_permission(void);

/** @brief Try to seize exclusive code staging permission. Needed for code
 * loading and purging.
 *
 * This is kept separate from code modification permission to allow tracing and
 * similar during long-running purge operations.
 *
 * * Main process lock (only) must be held.
 * * System thread progress must not be blocked.
 * * Caller is suspended and *must* yield if 0 is returned.
 * * Caller must not already have the code modification or staging permissions.
 *   
 *   That is, it is _NOT_ possible to add code modification permission when you
 *   already have staging permission. The other way around is fine however.
 */
int erts_try_seize_code_stage_permission(struct process* c_p);

/** @brief Release code stage permission. Resumes any suspended waiters. */
void erts_release_code_stage_permission(void);

/** @brief Try to seize exclusive code modification permission. Needed for
 * tracing, breakpoints, and so on.
 *
 * This used to be called code_write_permission, but was renamed to break
 * merges of code that uses the old locking paradigm.
 *
 * * Main process lock (only) must be held.
 * * System thread progress must not be blocked.
 * * Caller is suspended and *must* yield if 0 is returned.
 * * Caller must not already have the code modification permission, but may
 *   have staging permission.
 */
int erts_try_seize_code_mod_permission(struct process* c_p);

/** @brief As \c erts_try_seize_code_mod_permission but for aux work.
 *
 * System thread progress must not be blocked.
 * On success return true.
 * On failure return false and aux work func(arg) will be scheduled when
 * permission is released.
 */
int erts_try_seize_code_mod_permission_aux(void (*func)(void *),
                                           void *arg);

#ifdef ERTS_ENABLE_LOCK_CHECK
void erts_lc_soften_code_mod_permission_check(void);
#else
# define erts_lc_soften_code_mod_permission_check() ((void)0)
#endif

/** @brief Release code modification permission. Resumes any suspended
 * waiters. */
void erts_release_code_mod_permission(void);

/* Prepare the "staging area" to be a complete copy of the active code.
 *
 * Code staging permission must have been seized.
 *
 * Must be followed by calls to either "end" and "commit" or "abort" before
 * code staging permission can be released.
 */
void erts_start_staging_code_ix(int num_new);

/* End the staging.
 * Preceded by "start" and must be followed by "commit".
 */
void erts_end_staging_code_ix(void);

/* Set staging code index as new active code index.
 * Preceded by "end".
 */
void erts_commit_staging_code_ix(void);

/* Abort the staging.
 * Preceded by "start".
 */
void erts_abort_staging_code_ix(void);

#ifdef DEBUG
void erts_debug_require_code_barrier(void);
void erts_debug_check_code_barrier(void);
#endif

/* Schedules an operation to run after thread progress _and_ all schedulers
 * have issued an instruction barrier. */
void erts_schedule_code_barrier(ErtsCodeBarrier *barrier,
                                void (*later_function)(void *),
                                void *later_data);

void erts_schedule_code_barrier_cleanup(ErtsCodeBarrier *barrier,
                                        void (*later_function)(void *),
                                        void *later_data,
                                        UWord size);

/* Issues a code barrier on the current thread, as well as all managed threads
 * when they wake up after thread progress is unblocked.
 *
 * Requires that thread progress is blocked. */
void erts_blocking_code_barrier(void);

/* Helper function for the above: all managed threads should call this as soon
 * as thread progress is unblocked, _BEFORE_ updating thread progress. */
void erts_code_ix_finalize_wait(void);

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_has_code_load_permission(void);
int erts_has_code_stage_permission(void);
int erts_has_code_mod_permission(void);
#endif

#define ASSERT_MFA(MFA)                                                 \
    ASSERT(is_atom((MFA)->module) && is_atom((MFA)->function))

extern erts_atomic32_t the_active_code_index;
extern erts_atomic32_t the_staging_code_index;

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
ErtsCodePtr erts_codeinfo_to_code(const ErtsCodeInfo *ci)
{
#ifndef BEAMASM
    ASSERT(BeamIsOpCode(ci->u.op, op_i_func_info_IaaI) || !ci->u.op);
#endif
    ASSERT_MFA(&ci->mfa);
    return (ErtsCodePtr)&ci[1];
}

ERTS_GLB_INLINE
const ErtsCodeInfo *erts_code_to_codeinfo(ErtsCodePtr I)
{
    const ErtsCodeInfo *ci = &((const ErtsCodeInfo *)I)[-1];

#ifndef BEAMASM
    ASSERT(BeamIsOpCode(ci->u.op, op_i_func_info_IaaI) || !ci->u.op);
#endif
    ASSERT_MFA(&ci->mfa);

    return ci;
}

ERTS_GLB_INLINE
ErtsCodePtr erts_codemfa_to_code(const ErtsCodeMFA *mfa)
{
    ASSERT_MFA(mfa);
    return (ErtsCodePtr)&mfa[1];
}

ERTS_GLB_INLINE
const ErtsCodeMFA *erts_code_to_codemfa(ErtsCodePtr I)
{
    const ErtsCodeMFA *mfa = &((const ErtsCodeMFA *)I)[-1];

    ASSERT_MFA(mfa);

    return mfa;
}

ERTS_GLB_INLINE ErtsCodeIndex erts_active_code_ix(void)
{
    return erts_atomic32_read_nob(&the_active_code_index);
}
ERTS_GLB_INLINE ErtsCodeIndex erts_staging_code_ix(void)
{
    return erts_atomic32_read_nob(&the_staging_code_index);
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__CODE_IX_H__ */

