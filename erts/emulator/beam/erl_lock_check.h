/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

#include "erl_lock_flags.h"

#ifndef ERTS_ENABLE_LOCK_POSITION
/* Enable in order for _x variants of mtx functions to be used. */
#define ERTS_ENABLE_LOCK_POSITION 1
#endif

typedef struct {
    int inited;
    Sint16 id;
    erts_lock_flags_t flags;
    erts_lock_options_t taken_options;
    UWord extra;
} erts_lc_lock_t;

#define ERTS_LC_INITITALIZED 0x7f7f7f7f

#define ERTS_LC_LOCK_INIT(ID, X, F) {ERTS_LC_INITITALIZED, (ID), (F), 0, (X)}

void erts_lc_init(void);
void erts_lc_late_init(void);
Sint16 erts_lc_get_lock_order_id(char *name);
void erts_lc_check(erts_lc_lock_t *have, int have_len,
		   erts_lc_lock_t *have_not, int have_not_len);
void erts_lc_check_exact(erts_lc_lock_t *have, int have_len);
void erts_lc_have_locks(int *resv, erts_lc_lock_t *lcks, int len);
void erts_lc_have_lock_ids(int *resv, int *ids, int len);
void erts_lc_check_no_locked_of_type(erts_lock_flags_t flags);
int erts_lc_trylock_force_busy_flg(erts_lc_lock_t *lck, erts_lock_options_t options);
void erts_lc_trylock_flg_x(int locked, erts_lc_lock_t *lck, erts_lock_options_t options,
			   char *file, unsigned int line);
void erts_lc_lock_flg_x(erts_lc_lock_t *lck, erts_lock_options_t options,
			char *file, unsigned int line);
void erts_lc_unlock_flg(erts_lc_lock_t *lck, erts_lock_options_t options);
void erts_lc_might_unlock_flg(erts_lc_lock_t *lck, erts_lock_options_t options);
int erts_lc_trylock_force_busy(erts_lc_lock_t *lck);
void erts_lc_trylock_x(int locked, erts_lc_lock_t *lck,
		     char* file, unsigned int line);
void erts_lc_lock_x(erts_lc_lock_t *lck, char* file, unsigned int line);
void erts_lc_unlock(erts_lc_lock_t *lck);
void erts_lc_might_unlock(erts_lc_lock_t *lck);
void erts_lc_init_lock(erts_lc_lock_t *lck, char *name, erts_lock_flags_t flags);
void erts_lc_init_lock_x(erts_lc_lock_t *lck, char *name, erts_lock_flags_t flags, Eterm extra);
void erts_lc_destroy_lock(erts_lc_lock_t *lck);
void erts_lc_fail(char *fmt, ...);
int erts_lc_assert_failed(char *file, int line, char *assertion);
void erts_lc_set_thread_name(char *thread_name);
void erts_lc_pll(void);

void erts_lc_require_lock_flg(erts_lc_lock_t *lck, erts_lock_options_t options,
			      char *file, unsigned int line);
void erts_lc_unrequire_lock_flg(erts_lc_lock_t *lck, erts_lock_options_t options);

void erts_lc_require_lock(erts_lc_lock_t *lck, char *file, unsigned int line);
void erts_lc_unrequire_lock(erts_lc_lock_t *lck);

int erts_lc_is_emu_thr(void);

Eterm erts_lc_dump_graph(void);

#define ERTS_LC_ASSERT(A) \
    ((void) (((A) || ERTS_SOMEONE_IS_CRASH_DUMPING) ? 1 : erts_lc_assert_failed(__FILE__, __LINE__, #A)))
#else /* #ifdef ERTS_ENABLE_LOCK_CHECK */
#define ERTS_LC_ASSERT(A) ((void) 1)
#endif /* #ifdef ERTS_ENABLE_LOCK_CHECK */

#define erts_lc_lock(lck) erts_lc_lock_x(lck,__FILE__,__LINE__)
#define erts_lc_trylock(res,lck) erts_lc_trylock_x(res,lck,__FILE__,__LINE__)
#define erts_lc_lock_flg(lck,flg) erts_lc_lock_flg_x(lck,flg,__FILE__,__LINE__)
#define erts_lc_trylock_flg(res,lck,flg) erts_lc_trylock_flg_x(res,lck,flg,__FILE__,__LINE__)

#endif /* #ifndef ERTS_LOCK_CHECK_H__ */
