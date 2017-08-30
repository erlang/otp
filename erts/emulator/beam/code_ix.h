/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
 *      succesfully, the staging code index becomes the new active code index.
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
struct process;


#define ERTS_NUM_CODE_IX 3
typedef unsigned ErtsCodeIndex;


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
 * Code write permission must be seized.
 */
ERTS_GLB_INLINE
ErtsCodeIndex erts_staging_code_ix(void);

/* Try seize exclusive code write permission. Needed for code staging.
 * Main process lock (only) must be held.
 * System thread progress must not be blocked.
 * Caller must not already hold the code write permission.
 * Caller is suspended and *must* yield if 0 is returned. 
 */
int erts_try_seize_code_write_permission(struct process* c_p);

/* Release code write permission.
 * Will resume any suspended waiters.
 */
void erts_release_code_write_permission(void);

/* Prepare the "staging area" to be a complete copy of the active code.
 * Code write permission must have been seized.
 * Must be followed by calls to either "end" and "commit" or "abort" before
 * code write permission can be released.
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

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_has_code_write_permission(void);
#endif



#if ERTS_GLB_INLINE_INCL_FUNC_DEF

extern erts_smp_atomic32_t the_active_code_index;
extern erts_smp_atomic32_t the_staging_code_index;

ERTS_GLB_INLINE ErtsCodeIndex erts_active_code_ix(void)
{
    return erts_smp_atomic32_read_nob(&the_active_code_index);
}
ERTS_GLB_INLINE ErtsCodeIndex erts_staging_code_ix(void)
{
    return erts_smp_atomic32_read_nob(&the_staging_code_index);
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__CODE_IX_H__ */

