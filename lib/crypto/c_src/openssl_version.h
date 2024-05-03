/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2023. All Rights Reserved.
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

#ifndef E_OPENSSL_VERSION_H__
#define E_OPENSSL_VERSION_H__ 1

#include <openssl/opensslv.h>

#ifdef LIBRESSL_VERSION_NUMBER
# define HAS_LIBRESSL
# define HAS_LIBRESSL_VSN LIBRESSL_VERSION_NUMBER
#else
# define HAS_LIBRESSL_VSN 0
#endif

/* Helper macros to construct a OPENSSL_VERSION_NUMBER.
 * See openssl/opensslv.h
 */

#if !defined(HAS_LIBRESSL) && \
    defined(OPENSSL_VERSION_MAJOR) && \
    (OPENSSL_VERSION_MAJOR >= 3)

# define PACKED_OPENSSL_VERSION(MAJ, MIN, PATCH, VOID)   \
         (((((MAJ << 8) | MIN) << 16 ) | PATCH) << 4)
#else
/* Pre 3.0.0 */
#  define PACKED_OPENSSL_VERSION(MAJ, MIN, FIX, P)                        \
          ((((((((MAJ << 8) | MIN) << 8 ) | FIX) << 8) | (P-'a'+1)) << 4) | 0xf)

/* End Pre 3.0.0 */
#endif

#define PACKED_OPENSSL_VERSION_PLAIN(MAJ, MIN, FIX) \
    PACKED_OPENSSL_VERSION(MAJ,MIN,FIX,('a'-1))


#endif /* E_OPENSSL_VERSION_H__ */
