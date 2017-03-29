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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "hipe_arch.h"
#include "hipe_module.h"

void hipe_free_module(HipeModule *mod)
{
    hipe_free_code(mod->text_segment, mod->text_segment_size);
    if (mod->data_segment) /* Some modules lack data segments */
	erts_free(ERTS_ALC_T_HIPE_LL, mod->data_segment);

    erts_free(ERTS_ALC_T_HIPE_LL, mod);
}
