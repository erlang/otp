/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010. All Rights Reserved.
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
 * Description: The ethread atomic API
 * Author: Rickard Green
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHR_ATOMIC_IMPL__

#include "ethread.h"
#include "ethr_internal.h"

#ifndef ETHR_HAVE_NATIVE_ATOMICS
ethr_atomic_protection_t ethr_atomic_protection__[1 << ETHR_ATOMIC_ADDR_BITS];
#endif

int
ethr_init_atomics(void)
{
#ifndef ETHR_HAVE_NATIVE_ATOMICS
    {
	int i;
	for (i = 0; i < (1 << ETHR_ATOMIC_ADDR_BITS); i++) {
	    int res = ethr_spinlock_init(&ethr_atomic_protection__[i].u.lck);
	    if (res != 0)
		return res;
	}
    }
#endif
    return 0;
}

/*
 * --- Pointer size atomics ---------------------------------------------------
 */

ethr_sint_t *
ethr_atomic_addr(ethr_atomic_t *var)
{
    ETHR_ASSERT(var);
    return ethr_atomic_addr__(var);
}

void
ethr_atomic_init(ethr_atomic_t *var, ethr_sint_t i)
{
    ETHR_ASSERT(var);
    ethr_atomic_init__(var, i);
}

void
ethr_atomic_set(ethr_atomic_t *var, ethr_sint_t i)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_set__(var, i);
}

ethr_sint_t
ethr_atomic_read(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read__(var);
}

ethr_sint_t
ethr_atomic_add_read(ethr_atomic_t *var, ethr_sint_t incr)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_add_read__(var, incr);
}   

ethr_sint_t
ethr_atomic_inc_read(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_inc_read__(var);
}

ethr_sint_t
ethr_atomic_dec_read(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_dec_read__(var);
}

void
ethr_atomic_add(ethr_atomic_t *var, ethr_sint_t incr)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_add__(var, incr);
}   
    
void
ethr_atomic_inc(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_inc__(var);
}

void
ethr_atomic_dec(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_dec__(var);
}

ethr_sint_t
ethr_atomic_read_band(ethr_atomic_t *var, ethr_sint_t mask)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read_band__(var, mask);
}

ethr_sint_t
ethr_atomic_read_bor(ethr_atomic_t *var, ethr_sint_t mask)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read_bor__(var, mask);
}

ethr_sint_t
ethr_atomic_xchg(ethr_atomic_t *var, ethr_sint_t new)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_xchg__(var, new);
}   

ethr_sint_t
ethr_atomic_cmpxchg(ethr_atomic_t *var, ethr_sint_t new, ethr_sint_t expected)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_cmpxchg__(var, new, expected);
}

ethr_sint_t
ethr_atomic_read_acqb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_read_acqb__(var);
}

ethr_sint_t
ethr_atomic_inc_read_acqb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_inc_read_acqb__(var);
}

void
ethr_atomic_set_relb(ethr_atomic_t *var, ethr_sint_t i)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_set_relb__(var, i);
}

void
ethr_atomic_dec_relb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic_dec_relb__(var);
}

ethr_sint_t
ethr_atomic_dec_read_relb(ethr_atomic_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_dec_read_relb__(var);
}

ethr_sint_t
ethr_atomic_cmpxchg_acqb(ethr_atomic_t *var, ethr_sint_t new, ethr_sint_t exp)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_cmpxchg_acqb__(var, new, exp);
}

ethr_sint_t
ethr_atomic_cmpxchg_relb(ethr_atomic_t *var, ethr_sint_t new, ethr_sint_t exp)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic_cmpxchg_relb__(var, new, exp);
}


/*
 * --- 32-bit atomics ---------------------------------------------------------
 */

ethr_sint32_t *
ethr_atomic32_addr(ethr_atomic32_t *var)
{
    ETHR_ASSERT(var);
    return ethr_atomic32_addr__(var);
}

void
ethr_atomic32_init(ethr_atomic32_t *var, ethr_sint32_t i)
{
    ETHR_ASSERT(var);
    ethr_atomic32_init__(var, i);
}

void
ethr_atomic32_set(ethr_atomic32_t *var, ethr_sint32_t i)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic32_set__(var, i);
}

ethr_sint32_t
ethr_atomic32_read(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_read__(var);
}


ethr_sint32_t
ethr_atomic32_add_read(ethr_atomic32_t *var, ethr_sint32_t incr)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_add_read__(var, incr);
}   

ethr_sint32_t
ethr_atomic32_inc_read(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_inc_read__(var);
}

ethr_sint32_t
ethr_atomic32_dec_read(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_dec_read__(var);
}

void
ethr_atomic32_add(ethr_atomic32_t *var, ethr_sint32_t incr)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic32_add__(var, incr);
}   
    
void
ethr_atomic32_inc(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic32_inc__(var);
}

void
ethr_atomic32_dec(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic32_dec__(var);
}

ethr_sint32_t
ethr_atomic32_read_band(ethr_atomic32_t *var, ethr_sint32_t mask)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_read_band__(var, mask);
}

ethr_sint32_t
ethr_atomic32_read_bor(ethr_atomic32_t *var, ethr_sint32_t mask)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_read_bor__(var, mask);
}

ethr_sint32_t
ethr_atomic32_xchg(ethr_atomic32_t *var, ethr_sint32_t new)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_xchg__(var, new);
}   

ethr_sint32_t
ethr_atomic32_cmpxchg(ethr_atomic32_t *var,
		      ethr_sint32_t new,
		      ethr_sint32_t expected)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_cmpxchg__(var, new, expected);
}

ethr_sint32_t
ethr_atomic32_read_acqb(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_read_acqb__(var);
}

ethr_sint32_t
ethr_atomic32_inc_read_acqb(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_inc_read_acqb__(var);
}

void
ethr_atomic32_set_relb(ethr_atomic32_t *var, ethr_sint32_t i)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic32_set_relb__(var, i);
}

void
ethr_atomic32_dec_relb(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    ethr_atomic32_dec_relb__(var);
}

ethr_sint32_t
ethr_atomic32_dec_read_relb(ethr_atomic32_t *var)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_dec_read_relb__(var);
}

ethr_sint32_t
ethr_atomic32_cmpxchg_acqb(ethr_atomic32_t *var,
			   ethr_sint32_t new,
			   ethr_sint32_t exp)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_cmpxchg_acqb__(var, new, exp);
}

ethr_sint32_t
ethr_atomic32_cmpxchg_relb(ethr_atomic32_t *var,
			   ethr_sint32_t new,
			   ethr_sint32_t exp)
{
    ETHR_ASSERT(!ethr_not_inited__);
    ETHR_ASSERT(var);
    return ethr_atomic32_cmpxchg_relb__(var, new, exp);
}

