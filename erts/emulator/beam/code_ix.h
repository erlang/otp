/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2012. All Rights Reserved.
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
#ifndef __CODE_IX_H__
#define __CODE_IX_H__


#define ERTS_NUM_CODE_IX 3
typedef unsigned ErtsCodeIndex;

void erts_code_ix_init(void);
ErtsCodeIndex erts_active_code_ix(void);
ErtsCodeIndex erts_loader_code_ix(void);

/* Lock code_ix (enqueue and suspend until we get it)
*/
void erts_lock_code_ix(void);

/*SVERK
    erts_lock_code_ix();
    wait for thread progress
	   (we don't want any retarded threads with pointers to inactive Module)
    Copy active -> loader
	rlock old_code while copying Modules
    Update loader
    wait for thread progress
	   (we need a membarrier for everybody to "see" the new code)
    Set loader as new active
    erts_unlock_code_ix();
}*/


/* Unlock code_ix (resume first waiter)
*/
void erts_unlock_code_ix(void);
void erts_start_loader_code_ix(void);
void erts_commit_loader_code_ix(void);
void erts_abort_loader_code_ix(void);

void erts_rwlock_old_code(ErtsCodeIndex);
void erts_rwunlock_old_code(ErtsCodeIndex);
void erts_rlock_old_code(ErtsCodeIndex);
void erts_runlock_old_code(ErtsCodeIndex);

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_is_old_code_rlocked(void);
int erts_is_code_ix_locked(void);
#endif

#endif /* !__CODE_IX_H__ */
