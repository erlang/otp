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
 * hipe_load.c
 *
 * HiPE atomic code loader
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "global.h"
#include "erl_binary.h"
#include "hipe_load.h"
#include "hipe_bif0.h"

void hipe_free_loader_state(HipeLoaderState *stp)
{
    if (stp->module == NIL) return;

    // TODO: Needs to be freed separately. We'd like have a unified executable
    // code allocator, so postpone this for now.
    /* if (stp->text_segment) */
    /* 	erts_free(ERTS_ALC_T_HIPE, stp->text_segment); */
    stp->text_segment = NULL;
    stp->text_segment_size = 0;

    if (stp->data_segment)
	erts_free(ERTS_ALC_T_HIPE_LL, stp->data_segment);
    stp->data_segment = NULL;
    stp->data_segment_size = 0;

    if (stp->new_hipe_refs) {
        hipe_purge_refs(stp->new_hipe_refs, stp->module, 0);
        stp->new_hipe_refs = NULL;
    }
    if (stp->new_hipe_sdesc) {
        hipe_purge_sdescs(stp->new_hipe_sdesc, stp->module, 0);
        stp->new_hipe_sdesc = NULL;
    }

    stp->module = NIL;
}

static int
hipe_loader_state_dtor(Binary* magic)
{
    HipeLoaderState* stp = ERTS_MAGIC_BIN_DATA(magic);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(magic) == hipe_loader_state_dtor);

    hipe_free_loader_state(stp);
    return 1;
}

Binary *hipe_alloc_loader_state(Eterm module)
{
    HipeLoaderState *stp;
    Binary *magic;

    if (is_not_atom(module)) return NULL;

    magic = erts_create_magic_binary(sizeof(HipeLoaderState),
				     hipe_loader_state_dtor);
    erts_refc_inc(&magic->intern.refc, 1);
    stp = ERTS_MAGIC_BIN_DATA(magic);

    stp->module = module;
    stp->text_segment = NULL;
    stp->text_segment_size = 0;
    stp->data_segment = NULL;
    stp->data_segment_size = 0;

    stp->new_hipe_refs = NULL;
    stp->new_hipe_sdesc = NULL;

    return magic;
}

HipeLoaderState *
hipe_get_loader_state(Binary *magic)
{
    if (ERTS_MAGIC_BIN_DESTRUCTOR(magic) != hipe_loader_state_dtor)
	return NULL;

    return (HipeLoaderState*) ERTS_MAGIC_BIN_DATA(magic);
}
