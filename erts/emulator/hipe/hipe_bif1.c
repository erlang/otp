/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
 * hipe_bif1.c
 *
 * Performance analysis support.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "global.h"
#include "bif.h"
#include "big.h"
#include "error.h"
#include "beam_load.h"
#include "erl_vm.h"
#include "hipe_bif0.h"
#include "hipe_bif1.h"

BIF_RETTYPE hipe_bifs_call_count_on_1(BIF_ALIST_1)
{
    ErtsCodeInfo *ci;
    Eterm *pc;
    struct hipe_call_count *hcc;

    ci = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!ci)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
    pc = erts_codeinfo_to_code(ci);
    if (BeamIsOpCode(pc[0], op_hipe_trap_call))
	BIF_ERROR(BIF_P, BADARG);
    if (BeamIsOpCode(pc[0], op_hipe_call_count))
	BIF_RET(NIL);
    hcc = erts_alloc(ERTS_ALC_T_HIPE_SL, sizeof(*hcc));
    hcc->count = 0;
    hcc->opcode = pc[0];
    ci->u.hcc = hcc;
    pc[0] = BeamOpCodeAddr(op_hipe_call_count);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_call_count_off_1(BIF_ALIST_1)
{
    ErtsCodeInfo* ci;
    Eterm *pc;
    struct hipe_call_count *hcc;
    unsigned count;

    ci = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!ci)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
    pc = erts_codeinfo_to_code(ci);
    if (! BeamIsOpCode(pc[0], op_hipe_call_count))
	BIF_RET(am_false);
    hcc = ci->u.hcc;
    count = hcc->count;
    pc[0] = hcc->opcode;
    ci->u.hcc = NULL;
    erts_free(ERTS_ALC_T_HIPE_SL, hcc);
    BIF_RET(make_small(count));
}

BIF_RETTYPE hipe_bifs_call_count_get_1(BIF_ALIST_1)
{
    ErtsCodeInfo* ci;
    Eterm *pc;
    struct hipe_call_count *hcc;

    ci = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!ci)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
    pc = erts_codeinfo_to_code(ci);
    if (! BeamIsOpCode(pc[0], op_hipe_call_count))
	BIF_RET(am_false);
    hcc = ci->u.hcc;
    BIF_RET(make_small(hcc->count));
}

BIF_RETTYPE hipe_bifs_call_count_clear_1(BIF_ALIST_1)
{
    ErtsCodeInfo* ci;
    Eterm *pc;
    struct hipe_call_count *hcc;
    unsigned count;

    ci = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!ci)
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
    pc = erts_codeinfo_to_code(ci);
    if (! BeamIsOpCode(pc[0], op_hipe_call_count))
	BIF_RET(am_false);
    hcc = ci->u.hcc;
    count = hcc->count;
    hcc->count = 0;
    BIF_RET(make_small(count));
}

unsigned int hipe_trap_count;

BIF_RETTYPE hipe_bifs_trap_count_get_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_trap_count));
}

BIF_RETTYPE hipe_bifs_trap_count_clear_0(BIF_ALIST_0)
{
    unsigned int count = hipe_trap_count;
    hipe_trap_count = 0;
    BIF_RET(make_small(count));
}
