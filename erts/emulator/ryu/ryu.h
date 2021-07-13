// Copyright 2018 Ulf Adams
//
// The contents of this file may be used under the terms of the Apache License,
// Version 2.0.
//
//    (See accompanying file LICENSE-Apache or copy at
//     http://www.apache.org/licenses/LICENSE-2.0)
//
// Alternatively, the contents of this file may be used under the terms of
// the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE-Boost or copy at
//     https://www.boost.org/LICENSE_1_0.txt)
//
// Unless required by applicable law or agreed to in writing, this software
// is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.
#ifndef RYU_H
#define RYU_H

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>

int d2s_buffered_n(double f, char* result);
void d2s_buffered(double f, char* result);
char* d2s(double f);

//CHANGE_FOR_ERLANG we dropped all the other functions as not used by us

#ifdef __cplusplus
}
#endif

#endif // RYU_H
