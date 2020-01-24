/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2017. All Rights Reserved.
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

/* Description: A dynamic lock order checker.
 *              A global locking order is recorded during runtime
 *              and continuously checked against itself.
 *
 * Author: Sverker Eriksson
 */

#ifndef ERL_DYN_LOCK_CHECK_H__
#define ERL_DYN_LOCK_CHECK_H__

#ifdef ERTS_ENABLE_LOCK_CHECK
/*
 * ERTS_DYN_LOCK_CHECK enables dynamic lock checker
 * on NIF and driver mutexes and rwlocks.
 */
# define ERTS_DYN_LOCK_CHECK
#endif

#ifdef ERTS_DYN_LOCK_CHECK

/*
 * ERTS_DYN_LOCK_CHECK_INTERNAL will also enable dynamic lock checker
 * on ERTS internal mutexes and rwlocks.
 */
/*#define ERTS_DYN_LOCK_CHECK_INTERNAL*/


/* The struct to put in each lock instances */
typedef struct {
    unsigned ix;
} erts_dlc_t;

void erts_dlc_create_lock(erts_dlc_t* dlc, const char* name);
int erts_dlc_lock(erts_dlc_t* dlc);
void erts_dlc_trylock(erts_dlc_t* dlc, int locked);
void erts_dlc_unlock(erts_dlc_t* dlc);
void erts_dlc_init(void);

#endif /* ERTS_DYN_LOCK_CHECK */

#endif /* !ERL_DYN_LOCK_CHECK_H__ */
