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

#ifndef E_INFO_H__
#define E_INFO_H__ 1

#include "common.h"

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
extern char *crypto_callback_name;

int change_basename(ErlNifBinary* bin, char* buf, size_t bufsz, const char* newfile);
void error_handler(void* null, const char* errstr);
#endif

ERL_NIF_TERM info_lib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_INFO_H__ */
