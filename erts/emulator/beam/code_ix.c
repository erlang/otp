/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "code_ix.h"
#include "global.h"
#include "beam_catches.h"



#if 0
# define CIX_TRACE(text) erts_fprintf(stderr, "CIX_TRACE: " text " act=%u load=%u\r\n", erts_active_code_ix(), erts_staging_code_ix())
#else
# define CIX_TRACE(text)
#endif

erts_smp_atomic32_t the_active_code_index;
erts_smp_atomic32_t the_staging_code_index;

static erts_smp_mtx_t sverk_code_ix_lock; /*SVERK FIXME */

void erts_code_ix_init(void)
{
    /* We start emulator by initializing preloaded modules
     * single threaded with active and staging set both to zero.
     * Preloading is finished by a commit that will set things straight.
     */
    erts_smp_atomic32_init_nob(&the_active_code_index, 0);
    erts_smp_atomic32_init_nob(&the_staging_code_index, 0);
    erts_smp_mtx_init_x(&sverk_code_ix_lock, "sverk_code_ix_lock", NIL); /*SVERK FIXME */
    CIX_TRACE("init");
}

void erts_start_staging_code_ix(void)
{
    beam_catches_start_staging();
    export_start_staging();
    module_start_staging();
    erts_start_staging_ranges();
    CIX_TRACE("start");
}


void erts_commit_staging_code_ix(void)
{
    beam_catches_end_staging(1);
    export_end_staging(1);
    module_end_staging(1);
    erts_end_staging_ranges(1);
    {
	ErtsCodeIndex ix;
	export_write_lock();
	ix = erts_staging_code_ix();
	erts_smp_atomic32_set_nob(&the_active_code_index, ix);
	ix = (ix + 1) % ERTS_NUM_CODE_IX;
	erts_smp_atomic32_set_nob(&the_staging_code_index, ix);
	export_write_unlock();
    }
    CIX_TRACE("commit");
}

void erts_abort_staging_code_ix(void)
{
    beam_catches_end_staging(0);
    export_end_staging(0);
    module_end_staging(0);
    erts_end_staging_ranges(0);
    CIX_TRACE("abort");
}


/* Lock code_ix (enqueue and suspend until we get it)
*/
void erts_lock_code_ix(void)
{
    erts_smp_mtx_lock(&sverk_code_ix_lock); /*SVERK FIXME */
}

/* Unlock code_ix (resume first waiter)
*/
void erts_unlock_code_ix(void)
{
    erts_smp_mtx_unlock(&sverk_code_ix_lock); /*SVERK FIXME */
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_is_code_ix_locked(void)
{
    return erts_smp_lc_mtx_is_locked(&sverk_code_ix_lock);
}
#endif
