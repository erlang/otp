/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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

#ifndef ERL_BASED_FLOAT_H__
#define ERL_BASED_FLOAT_H__

enum erl_fmt_type {
    FMT_LEGACY,
    FMT_SHORT,
    FMT_FIXED,
    FMT_SCIENTIFIC
};

struct erl_float_opts {
    int base;
    enum erl_fmt_type fmt_type;
    bool compact;
    int decimals;
};


BIF_RETTYPE erl_based_float_to_list(Process *BIF_P, Eterm arg, struct erl_float_opts *opts);
BIF_RETTYPE erl_based_float_to_binary(Process *BIF_P, Eterm arg, struct erl_float_opts *opts);

/* Defined in bif.c, used for base-10 fallback */
BIF_RETTYPE do_charbuf_to_float(Process *BIF_P, char *buf);

#endif /* ERL_BASED_FLOAT_H__ */
