/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2024. All Rights Reserved.
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

#ifndef _BEAM_LOAD_H
#  define _BEAM_LOAD_H

#include "beam_opcodes.h"
#include "beam_file.h"

#include "erl_process.h"
#include "erl_fun.h"
#include "export.h"
#include "bif.h"

#include "erl_bif_table.h"
#include "beam_code.h"

Eterm beam_make_current_old(Process *c_p, ErtsProcLocks c_p_locks,
                            Eterm module);

typedef struct gen_op_entry {
   char* name;
   int arity;
   int specific;
   int num_specific;
   int transform;
} GenOpEntry;

extern const GenOpEntry gen_opc[];

typedef struct LoaderState_ LoaderState;

#ifdef BEAMASM
#include "jit/load.h"
#else
#include "emu/load.h"
#endif

int beam_load_prepared_dtor(Binary *magic);
void beam_load_prepared_free(Binary *magic);

int beam_load_prepare_emit(LoaderState *stp);
int beam_load_emit_op(LoaderState *stp, BeamOp *op);
int beam_load_finish_emit(LoaderState *stp);

struct erl_module_instance;
void beam_load_finalize_code(LoaderState *stp,
                             struct erl_module_instance* inst_p);

void beam_load_purge_aux(const BeamCodeHeader *hdr);

void beam_load_new_genop(LoaderState* stp);

#ifndef BEAMASM
int beam_load_new_label(LoaderState* stp);
#endif

#define BeamLoadError0(Stp, Fmt) \
    do { \
        beam_load_report_error(__LINE__, Stp, Fmt); \
        goto load_error; \
    } while (0)

#define BeamLoadError1(Stp, Fmt, Arg1) \
    do { \
        beam_load_report_error(__LINE__, stp, Fmt, Arg1); \
        goto load_error; \
    } while (0)

#define BeamLoadError2(Stp, Fmt, Arg1, Arg2) \
    do { \
        beam_load_report_error(__LINE__, Stp, Fmt, Arg1, Arg2); \
        goto load_error; \
    } while (0)

#define BeamLoadError3(Stp, Fmt, Arg1, Arg2, Arg3) \
    do { \
        beam_load_report_error(__LINE__, stp, Fmt, Arg1, Arg2, Arg3); \
        goto load_error; \
    } while (0)

#define BeamLoadVerifyTag(Stp, Actual, Expected) \
    if (Actual != Expected) { \
       BeamLoadError2(Stp, "bad tag %d; expected %d", Actual, Expected); \
    } else {}

void beam_load_report_error(int line, LoaderState* context, char *fmt,...);

/*
 * The transform engine.
 */

int erts_transform_engine(LoaderState* st);

#define TE_OK 0
#define TE_FAIL (-1)
#define TE_SHORT_WINDOW (-2)

int erts_beam_eval_predicate(unsigned int op, LoaderState* st,
                             BeamOpArg var[], BeamOpArg* rest_args);
BeamOp* erts_beam_execute_transform(unsigned int op, LoaderState* st,
                                    BeamOpArg var[], BeamOpArg* rest_args);

void erts_beam_bif_load_init(Uint);
Uint erts_get_outstanding_system_requests_limit(void);
Uint erts_set_outstanding_system_requests_limit(Uint new_val);

#endif /* _BEAM_LOAD_H */
