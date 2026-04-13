/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

#pragma once

#ifdef __WIN32__
#include <windows.h>
#endif

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <erl_nif.h>
#include "atoms.h"
#include "openssl_config.h"

// All nif functions return a valid value or throws an exception
ERL_NIF_TERM raise_exception(ErlNifEnv *env, ERL_NIF_TERM id, int arg_num, const char *explanation, const char *file,
                             int line);

static constexpr ERL_NIF_TERM ERL_CRYPTO_BAD_ATOM_VALUE = 0;

// Macros pass the FILE and LINE of the call site to the exception contents and are used in tests
// see crypto_SUITE:?chk_api_name etc.
#define EXCP_ERROR(Env, Str) raise_exception(Env, atom_error, -1, Str, __FILE__, __LINE__)
#define EXCP_NOTSUP(Env, Str) raise_exception(Env, atom_notsup, -1, Str, __FILE__, __LINE__)
#define EXCP_ERROR_N(Env, ArgNum, Str) raise_exception(Env, atom_error, ArgNum, Str, __FILE__, __LINE__)
#define EXCP_NOTSUP_N(Env, ArgNum, Str) raise_exception(Env, atom_notsup, ArgNum, Str, __FILE__, __LINE__)
#define EXCP_BADARG_N(Env, ArgNum, Str) raise_exception(Env, atom_badarg, ArgNum, Str, __FILE__, __LINE__)
#define RAISE_NOTSUP(Env) enif_raise_exception(Env, atom_notsup)

#define ASSIGN_GOTO(Var, Goto, CALL)                                                                                   \
    {                                                                                                                  \
        Var = (CALL);                                                                                                  \
        goto Goto;                                                                                                     \
    }
