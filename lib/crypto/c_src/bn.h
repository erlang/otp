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

#ifndef E_BN_H__
#define E_BN_H__ 1

#include "common.h"

ERL_NIF_TERM bin_from_bn(ErlNifEnv* env, const BIGNUM *bn);
ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef HAVE_EC
ERL_NIF_TERM bn2term(ErlNifEnv* env, size_t size, const BIGNUM *bn);
#endif

int get_bn_from_mpint(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp);
int get_bn_from_bin(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp);
int get_bn_from_bin_sz(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp, size_t* binsize);

#ifdef HAS_3_0_API
int get_ossl_octet_string_param_from_bin(ErlNifEnv* env, const char* key, ERL_NIF_TERM bin, OSSL_PARAM *dest);
int get_ossl_BN_param_from_bin_x(ErlNifEnv* env, char* key, ERL_NIF_TERM bin, OSSL_PARAM *dest, BIGNUM** bn_out);
#define get_ossl_BN_param_from_bin(ENV,KEY,BIN,DEST) get_ossl_BN_param_from_bin_x(ENV,KEY,BIN,DEST,NULL)

int get_ossl_BN_param_from_bin_sz_x(ErlNifEnv* env, char* key, ERL_NIF_TERM bin, OSSL_PARAM *dest, size_t *size, BIGNUM** bn_out);
#define get_ossl_BN_param_from_bin_sz(ENV,KEY,BIN,DEST,SIZE) get_ossl_BN_param_from_bin_sz_x(ENV,KEY,BIN,DEST,SIZE,NULL)

int get_ossl_BN_param_from_bn(ErlNifEnv* env, char* key, const BIGNUM* bn, OSSL_PARAM *dest);

int get_ossl_param_from_bin_in_list_x(ErlNifEnv* env, char* key, ERL_NIF_TERM *listcell, OSSL_PARAM *dest, BIGNUM** bn_out);
#define get_ossl_param_from_bin_in_list(ENV,KEY,CELL,DEST) get_ossl_param_from_bin_in_list_x(ENV,KEY,CELL,DEST,NULL)

#endif

#endif /* E_BN_H__ */

