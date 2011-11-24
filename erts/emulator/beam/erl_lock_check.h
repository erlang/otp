/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

/*
 * Description: A lock checker that checks that each thread acquires
 *              locks according to a predefined global lock order. The
 *              global lock order is used to prevent deadlocks. If the
 *              lock order is violated, an error message is printed
 *              and the emulator aborts. The lock checker is only
 *              intended to be enabled when debugging.
 *
 * Author: Rickard Green
 */

#include "sys.h"

#ifndef ERTS_LOCK_CHECK_H__
#define ERTS_LOCK_CHECK_H__

#ifdef ERTS_ENABLE_LOCK_CHECK

typedef struct {
    int inited;
    Sint16 id;
    Uint16 flags;
    UWord extra;
} erts_lc_lock_t;

#define ERTS_LC_INITITALIZED 0x7f7f7f7f


#define ERTS_LC_FLG_LT_SPINLOCK		(((Uint16) 1) << 0)
#define ERTS_LC_FLG_LT_RWSPINLOCK	(((Uint16) 1) << 1)
#define ERTS_LC_FLG_LT_MUTEX		(((Uint16) 1) << 2)
#define ERTS_LC_FLG_LT_RWMUTEX		(((Uint16) 1) << 3)
#define ERTS_LC_FLG_LT_PROCLOCK		(((Uint16) 1) << 4)

#define ERTS_LC_FLG_LO_READ		(((Uint16) 1) << 5)
#define ERTS_LC_FLG_LO_WRITE		(((Uint16) 1) << 6)

#define ERTS_LC_FLG_LO_READ_WRITE	(ERTS_LC_FLG_LO_READ		\
					 | ERTS_LC_FLG_LO_WRITE)

#define ERTS_LC_FLG_LT_ALL		(ERTS_LC_FLG_LT_SPINLOCK	\
					 | ERTS_LC_FLG_LT_RWSPINLOCK	\
					 | ERTS_LC_FLG_LT_MUTEX		\
					 | ERTS_LC_FLG_LT_RWMUTEX	\
					 | ERTS_LC_FLG_LT_PROCLOCK)

#define ERTS_LC_FLG_LO_ALL		(ERTS_LC_FLG_LO_READ		\
					 | ERTS_LC_FLG_LO_WRITE)


#define ERTS_LC_LOCK_INIT(ID, X, F) {ERTS_LC_INITITALIZED, (ID), (F), (X)}

void erts_lc_init(void);
void erts_lc_late_init(void);
Sint16 erts_lc_get_lock_order_id(char *name);
void erts_lc_check(erts_lc_lock_t *have, int have_len,
		   erts_lc_lock_t *have_not, int have_not_len);
void erts_lc_check_exact(erts_lc_lock_t *have, int have_len);
void erts_lc_have_locks(int *resv, erts_lc_lock_t *lcks, int len);
void erts_lc_have_lock_ids(int *resv, int *ids, int len);
void erts_lc_check_no_locked_of_type(Uint16 flags);
int erts_lc_trylock_force_busy_flg(erts_lc_lock_t *lck, Uint16 op_flags);
void erts_lc_trylock_flg(int locked, erts_lc_lock_t *lck, Uint16 op_flags);
void erts_lc_lock_flg(erts_lc_lock_t *lck, Uint16 op_flags);
void erts_lc_unlock_flg(erts_lc_lock_t *lck, Uint16 op_flags);
void erts_lc_might_unlock_flg(erts_lc_lock_t *lck, Uint16 op_flags);
int erts_lc_trylock_force_busy(erts_lc_lock_t *lck);
void erts_lc_trylock(int locked, erts_lc_lock_t *lck);
void erts_lc_lock(erts_lc_lock_t *lck);
void erts_lc_unlock(erts_lc_lock_t *lck);
void erts_lc_might_unlock(erts_lc_lock_t *lck);
void erts_lc_init_lock(erts_lc_lock_t *lck, char *name, Uint16 flags);
void erts_lc_init_lock_x(erts_lc_lock_t *lck, char *name, Uint16 flags, Eterm extra);
void erts_lc_destroy_lock(erts_lc_lock_t *lck);
void erts_lc_fail(char *fmt, ...);
int erts_lc_assert_failed(char *file, int line, char *assertion);
void erts_lc_set_thread_name(char *thread_name);
void erts_lc_pll(void);

void erts_lc_require_lock_flg(erts_lc_lock_t *lck, Uint16 op_flags);
void erts_lc_unrequire_lock_flg(erts_lc_lock_t *lck, Uint16 op_flags);

void erts_lc_require_lock(erts_lc_lock_t *lck);
void erts_lc_unrequire_lock(erts_lc_lock_t *lck);


#define ERTS_LC_ASSERT(A) \
  ((void) ((A) ? 1 : erts_lc_assert_failed(__FILE__, __LINE__, #A)))
#ifdef ERTS_SMP
#define ERTS_SMP_LC_ASSERT(A) ERTS_LC_ASSERT(A)
#else
#define ERTS_SMP_LC_ASSERT(A) ((void) 1)
#endif
#else /* #ifdef ERTS_ENABLE_LOCK_CHECK */
#define ERTS_SMP_LC_ASSERT(A) ((void) 1)
#define ERTS_LC_ASSERT(A) ((void) 1)
#endif /* #ifdef ERTS_ENABLE_LOCK_CHECK */

#endif /* #ifndef ERTS_LOCK_CHECK_H__ */
