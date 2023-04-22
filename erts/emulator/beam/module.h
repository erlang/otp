/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2023. All Rights Reserved.
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

#ifndef __MODULE_H__
#define __MODULE_H__

#include "index.h"
#include "beam_code.h"

struct erl_module_instance {
    const BeamCodeHeader* code_hdr;
    int code_length;		/* Length of loaded code in bytes. */
    unsigned catches;
    struct erl_module_nif* nif;
    int num_breakpoints;
    int num_traced_exports;

    const void *executable_region;
    void *writable_region;

    /* Protected by code modification permission. */
    int unsealed;
};

typedef struct erl_module {
    IndexSlot slot;		/* Must be located at top of struct! */
    int module;			/* Atom index for module (not tagged). */
    int seen;			/* Used by finish_loading() */

    struct erl_module_instance curr;
    struct erl_module_instance old; /* active protected by "old_code" rwlock */
    struct erl_module_instance* on_load;
} Module; 

void erts_module_instance_init(struct erl_module_instance* modi);
Module* erts_get_module(Eterm mod, ErtsCodeIndex code_ix);
Module* erts_put_module(Eterm mod);

/** @brief Converts the given code pointer into a writable one. The module must
 * have been made writable through \ref erts_unseal_module.
 *
 * @param[in] ptr Pointer to convert. Must point within the given module. */
void *erts_writable_code_ptr(struct erl_module_instance* modi,
                             const void *ptr);

/** @brief Opens a module for modification.
 *
 * This may only be used for one module at a time. Remember to call
 * \ref erts_seal_module before returning to Erlang code or unsealing another
 * module. */
void erts_unseal_module(struct erl_module_instance *modi);

/** @brief Seals a previously unsealed module, changing page permissions,
 * flushing code cache, et cetera as needed. The caller is responsible for
 * setting up a code barrier. */
void erts_seal_module(struct erl_module_instance *modi);

void init_module_table(void);
void module_start_staging(void);
void module_end_staging(int commit);
void module_info(fmtfn_t, void *);

Module *module_code(int, ErtsCodeIndex);
int module_code_size(ErtsCodeIndex);
int module_table_sz(void);

ERTS_GLB_INLINE void erts_rwlock_old_code(ErtsCodeIndex);
ERTS_GLB_INLINE void erts_rwunlock_old_code(ErtsCodeIndex);
ERTS_GLB_INLINE void erts_rlock_old_code(ErtsCodeIndex);
ERTS_GLB_INLINE void erts_runlock_old_code(ErtsCodeIndex);
#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_is_old_code_rlocked(ErtsCodeIndex);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

extern erts_rwmtx_t the_old_code_rwlocks[ERTS_NUM_CODE_IX];

ERTS_GLB_INLINE void erts_rwlock_old_code(ErtsCodeIndex code_ix)
{
    erts_rwmtx_rwlock(&the_old_code_rwlocks[code_ix]);
}
ERTS_GLB_INLINE void erts_rwunlock_old_code(ErtsCodeIndex code_ix)
{
    erts_rwmtx_rwunlock(&the_old_code_rwlocks[code_ix]);
}
ERTS_GLB_INLINE void erts_rlock_old_code(ErtsCodeIndex code_ix)
{
    erts_rwmtx_rlock(&the_old_code_rwlocks[code_ix]);
}
ERTS_GLB_INLINE void erts_runlock_old_code(ErtsCodeIndex code_ix)
{
    erts_rwmtx_runlock(&the_old_code_rwlocks[code_ix]);
}

#ifdef ERTS_ENABLE_LOCK_CHECK
ERTS_GLB_INLINE int erts_is_old_code_rlocked(ErtsCodeIndex code_ix)
{
    return erts_lc_rwmtx_is_rlocked(&the_old_code_rwlocks[code_ix]);
}
#endif

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */


#endif /* !__MODULE_H__ */
