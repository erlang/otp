/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020. All Rights Reserved.
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
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "export.h"
#include "bif.h"
#include "beam_load.h"

int
erts_transform_engine(LoaderState* st)
{
    Uint op;
    int ap;			/* Current argument. */
    const Uint* restart; /* Where to restart if current match fails. */
    GenOpArg var[TE_MAX_VARS];	/* Buffer for variables. */
    GenOpArg* rest_args = NULL;
    int num_rest_args = 0;
    int i;			/* General index. */
    Uint mask;
    GenOp* instr;
    GenOp* first = st->genop;
    GenOp* keep = NULL;
    const Uint* pc;
    static Uint restart_fail[1] = {TOP_fail};

    ASSERT(gen_opc[first->op].transform != -1);
    restart = op_transform + gen_opc[first->op].transform;

 restart:
    ASSERT(restart != NULL);
    pc = restart;
    ASSERT(*pc < NUM_TOPS);	/* Valid instruction? */
    instr = first;

#ifdef DEBUG
    restart = NULL;
#endif
    ap = 0;
    for (;;) {
	op = *pc++;

	switch (op) {
	case TOP_next_instr:
	    instr = instr->next;
	    ap = 0;
	    if (instr == NULL) {
		/*
		 * We'll need at least one more instruction to decide whether
		 * this combination matches or not.
		 */
		return TE_SHORT_WINDOW;
	    }
	    if (*pc++ != instr->op)
		goto restart;
	    break;
	case TOP_is_type:
	    mask = *pc++;

	    ASSERT(ap < instr->arity);
	    ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
	    if (((1 << instr->a[ap].type) & mask) == 0)
		goto restart;
	    break;
#if defined(TOP_is_type_next_arg)
	case TOP_is_type_next_arg:
	    mask = *pc++;
	    ASSERT(ap < instr->arity);
	    ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
	    if (((1 << instr->a[ap].type) & mask) == 0)
		goto restart;
            ap++;
	    break;
#endif
	case TOP_pred:
	    i = *pc++;
            i = erts_beam_eval_predicate((unsigned) i, st, var, rest_args);
	    if (i == 0)
		goto restart;
	    break;
#if defined(TOP_is_eq)
	case TOP_is_eq:
	    ASSERT(ap < instr->arity);
	    if (*pc++ != instr->a[ap].val)
		goto restart;
	    break;
#endif
	case TOP_is_type_eq:
	    mask = *pc++;

	    ASSERT(ap < instr->arity);
	    ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
	    if (((1 << instr->a[ap].type) & mask) == 0)
		goto restart;
	    if (*pc++ != instr->a[ap].val)
		goto restart;
	    break;
#if defined(TOP_is_type_eq_next_arg)
	case TOP_is_type_eq_next_arg:
	    mask = *pc++;
            ASSERT(ap < instr->arity);
            ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
            if (((1 << instr->a[ap].type) & mask) == 0)
                goto restart;
            if (*pc++ != instr->a[ap].val)
                goto restart;
            ap++;
            break;
#endif
	case TOP_is_same_var:
	    ASSERT(ap < instr->arity);
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    if (var[i].type != instr->a[ap].type)
		goto restart;
	    switch (var[i].type) {
	    case TAG_n:
		break;
	    default:
		if (var[i].val != instr->a[ap].val)
		    goto restart;
	    }
	    break;
#if defined(TOP_is_bif)
	case TOP_is_bif:
	    {
		int bif_number = *pc++;

		/*
		 * In debug build, the type must be 'u'.
		 * In a real build, don't match.  (I.e. retain the original
		 * call instruction, this will work, but it will be a
		 * slight performance loss.)
		 */

		ASSERT(instr->a[ap].type == TAG_u);
		if (instr->a[ap].type != TAG_u)
		    goto restart;

		/*
		 * In debug build, the assertion will catch invalid indexes
		 * immediately.  In a real build, the loader will issue
		 * an diagnostic later when the instruction is loaded.
		 */

		i = instr->a[ap].val;
		ASSERT(i < st->num_imports);
		if (i >= st->num_imports || st->import[i].bif == NULL)
		    goto restart;
                if (bif_number != -1) {
                    Export *bif = st->import[i].bif;
                    if (bif->bif_number != bif_number) {
                        goto restart;
                    }
                }
	    }
	    break;
#endif
#if defined(TOP_is_not_bif)
	case TOP_is_not_bif:
	    {
		pc++;

		/*
		 * In debug build, the type must be 'u'.
		 */

		ASSERT(instr->a[ap].type == TAG_u);
		if (instr->a[ap].type != TAG_u) {
		    goto restart;
		}
		i = instr->a[ap].val;

		/*
		 * erlang:apply/2,3 are strange. They exist as (dummy) BIFs
		 * so that they are included in the export table before
		 * the erlang module is loaded. They also exist in the erlang
		 * module as functions. When used in code, a special Beam
		 * instruction is used.
		 *
		 * Below we specially recognize erlang:apply/2,3 as special.
		 * This is necessary because after setting a trace pattern on
		 * them, you cannot no longer see from the export entry that
		 * they are special.
		 */
		if (i < st->num_imports) {
		    if (st->import[i].bif != NULL ||
			(st->import[i].module == am_erlang &&
			 st->import[i].function == am_apply &&
			 (st->import[i].arity == 2 || st->import[i].arity == 3))) {
			goto restart;
		    }
		}
	    }
	    break;

#endif
#if defined(TOP_is_func)
	case TOP_is_func:
	    {
		Eterm mod = *pc++;
		Eterm func = *pc++;
		int arity = *pc++;

		ASSERT(instr->a[ap].type == TAG_u);
		if (instr->a[ap].type != TAG_u) {
		    goto restart;
		}
		i = instr->a[ap].val;
		ASSERT(i < st->num_imports);
		if (i >= st->num_imports || st->import[i].module != mod ||
		    st->import[i].function != func ||
		    (arity < MAX_ARG && st->import[i].arity != arity)) {
		    goto restart;
		}
	    }
	    break;
#endif
	case TOP_set_var_next_arg:
	    ASSERT(ap < instr->arity);
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    var[i].type = instr->a[ap].type;
	    var[i].val = instr->a[ap].val;
	    ap++;
	    break;
#if defined(TOP_is_type_set_var_next_arg)
	case TOP_is_type_set_var_next_arg:
            mask = pc[0];
            i = pc[1];
	    ASSERT(i < TE_MAX_VARS);
            ASSERT(ap < instr->arity);
	    ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
	    if (((1 << instr->a[ap].type) & mask) == 0)
		goto restart;
	    ASSERT(i < TE_MAX_VARS);
	    var[i] = instr->a[ap];
	    ap++;
            pc += 2;
	    break;
#endif
#if defined(TOP_is_type_eq_set_var_next_arg)
	case TOP_is_type_eq_set_var_next_arg:
            {
                Eterm val;
                mask = pc[0];
                val = pc[1];
                i = pc[2];
                ASSERT(i < TE_MAX_VARS);
                ASSERT(ap < instr->arity);
                ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
                if (((1 << instr->a[ap].type) & mask) == 0)
                    goto restart;
                if (val != instr->a[ap].val)
                    goto restart;
                ASSERT(i < TE_MAX_VARS);
                var[i] = instr->a[ap];
                ap++;
                pc += 3;
            }
	    break;
#endif
#if defined(TOP_rest_args)
	case TOP_rest_args:
	    {
		int formal_arity = gen_opc[instr->op].arity;
		num_rest_args = instr->arity - formal_arity;
		rest_args = instr->a + formal_arity;
	    }
	    break;
#endif
	case TOP_next_arg:
	    ap++;
	    break;
	case TOP_commit:
	    instr = instr->next; /* The next_instr was optimized away. */
	    keep = instr;
	    break;
#if defined(TOP_commit_new_instr)
	case TOP_commit_new_instr:
            /*
             * Reuse the last instruction on the left side instead of
             * allocating a new instruction. Note that this is not
             * safe if TOP_rest_args has been executed; therefore,
             * this combined instruction is never used when that is
             * the case.
             */
            ASSERT(instr->a == instr->def_args);
            keep = instr;
            instr->op = op = *pc++;
            instr->arity = gen_opc[op].arity;
            ap = 0;
            break;
#endif
#if defined(TOP_keep)
	case TOP_keep:
	    /* Keep the current instruction unchanged. */
	    keep = instr;
	    break;
#endif
#if defined(TOP_call_end)
	case TOP_call_end:
	    {
		GenOp** lastp;
		GenOp* new_instr;

		i = *pc++;
                new_instr = erts_beam_execute_transform((unsigned) i, st, var, rest_args);
		if (new_instr == NULL) {
		    goto restart;
		}

		lastp = &new_instr;
		while (*lastp != NULL) {
		    lastp = &((*lastp)->next);
		}

		keep = instr->next; /* The next_instr was optimized away. */
		*lastp = keep;
                instr = new_instr;
	    }
	    /* FALLTHROUGH */
#endif
	case TOP_end:
            st->genop = instr;
	    while (first != keep) {
		GenOp* next = first->next;
		FREE_GENOP(st, first);
		first = next;
	    }
	    return TE_OK;
	case TOP_new_instr:
	    /*
	     * Note that the instructions are generated in reverse order.
	     */
            {
                GenOp* new_instr;
                NEW_GENOP(st, new_instr);
                new_instr->next = instr;
                instr = new_instr;
                instr->op = op = *pc++;
                instr->arity = gen_opc[op].arity;
                ap = 0;
            }
            break;
#ifdef TOP_rename
	case TOP_rename:
	    instr->op = op = *pc++;
	    instr->arity = gen_opc[op].arity;
	    return TE_OK;
#endif
	case TOP_store_val_next_arg:
            instr->a[ap].type = pc[0];
            instr->a[ap].val = pc[1];
            ap++;
            pc += 2;
            break;
	case TOP_store_var_next_arg:
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    instr->a[ap].type = var[i].type;
	    instr->a[ap].val = var[i].val;
	    ap++;
	    break;
#if defined(TOP_store_rest_args)
	case TOP_store_rest_args:
	    {
		GENOP_ARITY(instr, instr->arity+num_rest_args);
		sys_memcpy(instr->a, instr->def_args, ap*sizeof(GenOpArg));
		sys_memcpy(instr->a+ap, rest_args, num_rest_args*sizeof(GenOpArg));
		ap += num_rest_args;
	    }
	    break;
#endif
	case TOP_try_me_else:
	    restart = pc + 1;
	    restart += *pc++;
	    ASSERT(*pc < NUM_TOPS); /* Valid instruction? */
	    break;
	case TOP_try_me_else_fail:
	    restart = restart_fail;
	    break;
	case TOP_fail:
	    return TE_FAIL;
#if defined(TOP_skip_unless)
	case TOP_skip_unless:
            /*
             * Note that the caller of transform_engine() guarantees that
             * there is always a second instruction available.
             */
            ASSERT(instr);
            if (instr->next->op != pc[0]) {
                /* The second instruction is wrong. Skip ahead. */
                pc += pc[1] + 2;
                ASSERT(*pc < NUM_TOPS); /* Valid instruction? */
            } else {
                /* Correct second instruction. */
                pc += 2;
            }
	    break;
#endif
	default:
	    ASSERT(0);
	}
    }
}
