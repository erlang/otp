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
 * hipe_load.h
 *
 * HiPE atomic code loader
 */
#ifndef HIPE_LOAD_H
#define HIPE_LOAD_H

#include "global.h"

typedef struct hipe_loader_state {
    Eterm module;                   /* Module name, atom */

    void *text_segment;
    Uint text_segment_size;

    void *data_segment;
    Uint data_segment_size;

    struct hipe_ref* new_hipe_refs;
    struct hipe_sdesc* new_hipe_sdesc;

} HipeLoaderState;

extern Binary *hipe_alloc_loader_state(Eterm module);
extern void hipe_free_loader_state(HipeLoaderState*);
extern HipeLoaderState *hipe_get_loader_state(Binary *binary);

#endif /* HIPE_LOAD_H */
