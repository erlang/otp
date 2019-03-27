/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

#ifndef E_COMMON_H__
#define E_COMMON_H__ 1

#ifdef __WIN32__
#  include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>

#include <erl_nif.h>
#include "openssl_config.h"
#include "atoms.h"


/* All nif functions return a valid value or throws an exception */
#define EXCP(Env, Id, Str)  enif_raise_exception((Env), \
                                enif_make_tuple3((Env), \
                                                 (Id),  \
                                                 enif_make_tuple2((Env), \
                                                                  enif_make_string((Env),__FILE__,(ERL_NIF_LATIN1)), \
                                                                  enif_make_int((Env), __LINE__)), \
                                                 enif_make_string((Env),(Str),(ERL_NIF_LATIN1)) ))

#define EXCP_NOTSUP(Env, Str) EXCP((Env), atom_notsup, (Str))
#define EXCP_BADARG(Env, Str) EXCP((Env), atom_badarg, (Str))
#define EXCP_ERROR(Env, Str)  EXCP((Env), atom_error, (Str))

#endif /* E_COMMON_H__ */
