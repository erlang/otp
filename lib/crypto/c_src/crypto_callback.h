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

struct crypto_callbacks
{
    size_t sizeof_me;

    void* (*crypto_alloc)(size_t size);
    void* (*crypto_realloc)(void* ptr, size_t size);
    void (*crypto_free)(void* ptr);

    /* openssl callbacks */
  #ifdef OPENSSL_THREADS
    void (*locking_function)(int mode, int n, const char *file, int line);
    unsigned long (*id_function)(void);
    struct CRYPTO_dynlock_value* (*dyn_create_function)(const char *file,
							int line);
    void (*dyn_lock_function)(int mode, struct CRYPTO_dynlock_value* ptr,
			      const char *file, int line);
    void (*dyn_destroy_function)(struct CRYPTO_dynlock_value *ptr,
				 const char *file, int line);
  #endif /* OPENSSL_THREADS */
};

typedef struct crypto_callbacks* get_crypto_callbacks_t(int nlocks);

#ifndef HAVE_DYNAMIC_CRYPTO_LIB
struct crypto_callbacks* get_crypto_callbacks(int nlocks);
#endif

