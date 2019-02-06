/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017. All Rights Reserved.
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

#ifndef ERTS_LOCK_FLAGS_H__
#define ERTS_LOCK_FLAGS_H__

#define ERTS_LOCK_OPTIONS_READ  (1 << 1)
#define ERTS_LOCK_OPTIONS_WRITE (1 << 2)

#define ERTS_LOCK_OPTIONS_RDWR (ERTS_LOCK_OPTIONS_READ | ERTS_LOCK_OPTIONS_WRITE)

/* Property/category are bitfields to simplify their use in masks. */
#define ERTS_LOCK_FLAGS_MASK_CATEGORY (0xFFC0)
#define ERTS_LOCK_FLAGS_MASK_PROPERTY (0x0030)

/* Type is a plain number. */
#define ERTS_LOCK_FLAGS_MASK_TYPE     (0x000F)

#define ERTS_LOCK_FLAGS_TYPE_SPINLOCK (1)
#define ERTS_LOCK_FLAGS_TYPE_MUTEX    (2)
#define ERTS_LOCK_FLAGS_TYPE_PROCLOCK (3)

/* "Static" guarantees that the lock will never be destroyed once created. */
#define ERTS_LOCK_FLAGS_PROPERTY_STATIC     (1 << 4)
#define ERTS_LOCK_FLAGS_PROPERTY_READ_WRITE (1 << 5)

#define ERTS_LOCK_FLAGS_CATEGORY_ALLOCATOR    (1 << 6)
#define ERTS_LOCK_FLAGS_CATEGORY_PROCESS      (1 << 7)
#define ERTS_LOCK_FLAGS_CATEGORY_IO           (1 << 8)
#define ERTS_LOCK_FLAGS_CATEGORY_DB           (1 << 9)
#define ERTS_LOCK_FLAGS_CATEGORY_DEBUG        (1 << 10)
#define ERTS_LOCK_FLAGS_CATEGORY_SCHEDULER    (1 << 11)
#define ERTS_LOCK_FLAGS_CATEGORY_GENERIC      (1 << 12)
#define ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION (1 << 13)

#define ERTS_LOCK_TYPE_SPINLOCK \
    (ERTS_LOCK_FLAGS_TYPE_SPINLOCK)
#define ERTS_LOCK_TYPE_RWSPINLOCK \
    (ERTS_LOCK_TYPE_SPINLOCK | \
     ERTS_LOCK_FLAGS_PROPERTY_READ_WRITE)
#define ERTS_LOCK_TYPE_MUTEX \
    (ERTS_LOCK_FLAGS_TYPE_MUTEX)
#define ERTS_LOCK_TYPE_RWMUTEX \
    (ERTS_LOCK_TYPE_MUTEX | \
     ERTS_LOCK_FLAGS_PROPERTY_READ_WRITE)
#define ERTS_LOCK_TYPE_PROCLOCK \
    (ERTS_LOCK_FLAGS_CATEGORY_PROCESS | \
     ERTS_LOCK_FLAGS_TYPE_PROCLOCK)

/* -- -- */

typedef unsigned short erts_lock_flags_t;
typedef unsigned short erts_lock_options_t;

/* @brief Gets the type name of the lock, honoring the RW flag if supplied. */
const char *erts_lock_flags_get_type_name(erts_lock_flags_t flags);

/* @brief Gets a short-form description of the given lock options. (rw/r/w) */
const char *erts_lock_options_get_short_desc(erts_lock_options_t options);

#endif /* ERTS_LOCK_FLAGS_H__ */
