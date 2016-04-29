/* 
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

