/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023. All Rights Reserved.
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
 * This module contains methods for dealing with io-/bitstring lists,
 * which follow this structure:
 *
 * head ::= Bitstring
 *        |   Byte (i.e integer in range [0..255]
 *        |   list
 *        ;
 *
 * tail ::= []
 *        |   Bitstring
 *        |   list
 *        ;
 *
 * list ::= []
 *        |   Bitstring
 *        |   [ head | tail]
 *        ;
 */

#if !defined(ERL_IOLIST_H)
#    define ERL_IOLIST_H

#include "sys.h"
#include "global.h"

#define ERTS_IOLIST_TO_BUF_OVERFLOW	(~((ErlDrvSizeT) 0))
#define ERTS_IOLIST_TO_BUF_TYPE_ERROR	(~((ErlDrvSizeT) 1))
#define ERTS_IOLIST_TO_BUF_YIELD	(~((ErlDrvSizeT) 2))
#define ERTS_IOLIST_TO_BUF_FAILED(R) \
    (((R) & (~((ErlDrvSizeT) 3))) == (~((ErlDrvSizeT) 3)))
#define ERTS_IOLIST_TO_BUF_SUCCEEDED(R) \
    (!ERTS_IOLIST_TO_BUF_FAILED((R)))

void erts_init_iolist(void);

/** @brief Copies the contents of the `iodata` \c obj into the \c data
 *
 * @param size the size of the buffer pointed to by \c data
 *
 * @return On success, returns the number of bytes remaining in the given
 * buffer.
 *
 * On failure, ERTS_IOLIST_TO_BUF_OVERFLOW signals overflow, and
 * ERTS_IOLIST_TO_BUF_TYPE_ERROR signals a type error (including that the
 * result would not be a whole number of bytes).
 */
ErlDrvSizeT erts_iolist_to_buf(Eterm obj, char *data, ErlDrvSizeT size);
int erts_iolist_size(Eterm, ErlDrvSizeT *);

#endif
