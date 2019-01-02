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

#ifndef E_EC_H__
#define E_EC_H__ 1

#include "common.h"

#if defined(HAVE_EC)
int get_ec_key(ErlNifEnv* env, ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
               EC_KEY** res);
int term2point(ErlNifEnv* env, ERL_NIF_TERM term, EC_GROUP *group, EC_POINT **pptr);
ERL_NIF_TERM make_badarg_maybe(ErlNifEnv* env);
#endif

ERL_NIF_TERM ec_key_generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_EC_H__ */
