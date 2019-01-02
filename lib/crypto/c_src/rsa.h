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

#ifndef E_RSA_H__
#define E_RSA_H__ 1

#include "common.h"

int get_rsa_public_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa);
int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa);

ERL_NIF_TERM rsa_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_RSA_H__ */
