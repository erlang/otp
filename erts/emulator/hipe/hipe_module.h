/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2016. All Rights Reserved.
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
/*
 * hipe_module.h
 *
 *
 */
#ifndef HIPE_MODULE_H
#define HIPE_MODULE_H

/* Forward-declare type to resolve circular dependency with module.h */
typedef struct hipe_module HipeModule;

#include "global.h"

struct hipe_module {
    void *text_segment;
    Uint text_segment_size;
    void *data_segment;

    struct hipe_ref* first_hipe_ref;  /* all external hipe calls from this module */
    struct hipe_sdesc* first_hipe_sdesc;  /* all stack descriptors for this module */
};

extern void hipe_free_module(HipeModule *mod);

#endif /* HIPE_MODULE_H */
