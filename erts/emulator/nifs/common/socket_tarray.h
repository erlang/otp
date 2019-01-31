/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2019. All Rights Reserved.
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
 *
 * ----------------------------------------------------------------------
 *  Purpose : Build and "maintain" a (erlang) term array of 
 *            variable length.
 * ----------------------------------------------------------------------
 *
 */

#ifndef SOCKET_TARRAY_H__
#define SOCKET_TARRAY_H__

typedef void* SocketTArray;

extern SocketTArray esock_tarray_create(Uint32 sz);
extern void         esock_tarray_delete(SocketTArray ta);
extern Uint32       esock_tarray_sz(SocketTArray ta);
extern void         esock_tarray_add(SocketTArray ta, ERL_NIF_TERM t);
extern void         esock_tarray_tolist(SocketTArray  ta,
                                        ErlNifEnv*    env,
                                        ERL_NIF_TERM* list);

#define TARRAY_CREATE(SZ)        esock_tarray_create((SZ))
#define TARRAY_DELETE(TA)        esock_tarray_delete((TA))
#define TARRAY_SZ(TA)            esock_tarray_sz((TA))
#define TARRAY_ADD(TA, T)        esock_tarray_add((TA), (T))
#define TARRAY_TOLIST(TA, E, L)  esock_tarray_tolist((TA), (E), (L))


#endif // SOCKET_TARRAY_H__
