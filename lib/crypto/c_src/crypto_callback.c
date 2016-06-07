/* 
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

#include <stdio.h>
#include <string.h>
#include <openssl/opensslconf.h>

#include "erl_nif.h"
#include "crypto_callback.h"

#ifdef DEBUG
    #  define ASSERT(e) \
    ((void) ((e) ? 1 : (fprintf(stderr,"Assert '%s' failed at %s:%d\n",\
				#e, __FILE__, __LINE__), abort(), 0)))
#else
    #  define ASSERT(e) ((void) 1)
#endif

#ifdef __GNUC__
    #  define INLINE __inline__
#elif defined(__WIN32__)
    #  define INLINE __forceinline
#else
    #  define INLINE
#endif

#ifdef __WIN32__
#  define DLLEXPORT __declspec(dllexport)
#elif defined(__GNUC__) && __GNUC__ >= 4
#  define DLLEXPORT __attribute__ ((visibility("default")))
#elif defined (__SUNPRO_C) && (__SUNPRO_C >= 0x550)
#  define DLLEXPORT __global
#else
#  define DLLEXPORT
#endif

/* to be dlsym'ed */
DLLEXPORT struct crypto_callbacks* get_crypto_callbacks(int nlocks);


static void nomem(size_t size, const char* op)
{
    fprintf(stderr, "Out of memory abort. Crypto failed to %s %zu bytes.\r\n",
	    op, size);
    abort();
}

static void* crypto_alloc(size_t size)
{
    void *ret = enif_alloc(size);

    if (!ret && size)
	nomem(size, "allocate");
    return ret;
}
static void* crypto_realloc(void* ptr, size_t size)
{
    void* ret = enif_realloc(ptr, size);

    if (!ret && size)
	nomem(size, "reallocate");
    return ret;
}
static void crypto_free(void* ptr)
{
    enif_free(ptr);
}


#ifdef OPENSSL_THREADS /* vvvvvvvvvvvvvvv OPENSSL_THREADS vvvvvvvvvvvvvvvv */

static ErlNifRWLock** lock_vec = NULL; /* Static locks used by openssl */

#include <openssl/crypto.h>

static INLINE void locking(int mode, ErlNifRWLock* lock)
{
    switch (mode) {
    case CRYPTO_LOCK|CRYPTO_READ:
	enif_rwlock_rlock(lock);
	break;
    case CRYPTO_LOCK|CRYPTO_WRITE:
	enif_rwlock_rwlock(lock);
	break;
    case CRYPTO_UNLOCK|CRYPTO_READ:
	enif_rwlock_runlock(lock);
	break;
    case CRYPTO_UNLOCK|CRYPTO_WRITE:
	enif_rwlock_rwunlock(lock);
	break;
    default:
	ASSERT(!"Invalid lock mode");
    }
}

static void locking_function(int mode, int n, const char *file, int line)
{
    locking(mode, lock_vec[n]);
}

static unsigned long id_function(void)
{
    return (unsigned long) enif_thread_self();
}

/* Dynamic locking, not used by current openssl version (0.9.8)
 */
static struct CRYPTO_dynlock_value* dyn_create_function(const char *file, int line)
{
    return (struct CRYPTO_dynlock_value*) enif_rwlock_create("crypto_dyn");
}
static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value* ptr,const char *file, int line)
{
    locking(mode, (ErlNifRWLock*)ptr);
}
static void dyn_destroy_function(struct CRYPTO_dynlock_value *ptr, const char *file, int line)
{
    enif_rwlock_destroy((ErlNifRWLock*)ptr);
}

#endif /* ^^^^^^^^^^^^^^^^^^^^^^ OPENSSL_THREADS ^^^^^^^^^^^^^^^^^^^^^^ */

DLLEXPORT struct crypto_callbacks* get_crypto_callbacks(int nlocks)
{
    static int is_initialized = 0;
    static struct crypto_callbacks the_struct = {
	sizeof(struct crypto_callbacks),

	&crypto_alloc,
	&crypto_realloc,
	&crypto_free,
        
#ifdef OPENSSL_THREADS
	&locking_function,
	&id_function,
	&dyn_create_function,
	&dyn_lock_function,
	&dyn_destroy_function
#endif /* OPENSSL_THREADS */
    };

    if (!is_initialized) {
#ifdef OPENSSL_THREADS
	if (nlocks > 0) {
	    int i;
	    lock_vec = enif_alloc(nlocks*sizeof(*lock_vec));
	    if (lock_vec==NULL) return NULL;
	    memset(lock_vec, 0, nlocks*sizeof(*lock_vec));
	    
	    for (i=nlocks-1; i>=0; --i) {
		lock_vec[i] = enif_rwlock_create("crypto_stat");
		if (lock_vec[i]==NULL) return NULL;
	    }
	}
#endif
	is_initialized = 1;
    }
    return &the_struct;
}

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
/* This is not really a NIF library, but we use ERL_NIF_INIT in order to
 * get access to the erl_nif API (on Windows).
 */
ERL_NIF_INIT(dummy, (ErlNifFunc*)NULL , NULL, NULL, NULL, NULL)
#endif

