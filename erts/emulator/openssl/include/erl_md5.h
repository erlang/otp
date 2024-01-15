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

#ifndef ERL_MD5_H__
#define ERL_MD5_H__

#undef ERLANG_OPENSSL_INTEGRATION
#define ERLANG_OPENSSL_INTEGRATION

#define MD5_INIT_FUNCTION_NAME                  MD5Init
#define MD5_UPDATE_FUNCTION_NAME                MD5Update
#define MD5_FINAL_FUNCTION_NAME                 MD5Final
#define MD5_TRANSFORM_FUNCTION_NAME             MD5Transform
#define MD5_BLOCK_DATA_ORDER_FUNCTION_NAME      MD5BlockDataOrder

#include "openssl_local/md5.h"

#endif
