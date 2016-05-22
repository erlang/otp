/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
 * Operator BIFs.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "erl_map.h"

BIF_RETTYPE and_2(BIF_ALIST_2)
{
    if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_true)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE or_2(BIF_ALIST_2)
{
    if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_false)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE xor_2(BIF_ALIST_2)
{
    if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_true)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_false)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE not_1(BIF_ALIST_1)
{
    if (BIF_ARG_1 == am_true)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_false)
	BIF_RET(am_true);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE sgt_2(BIF_ALIST_2)
{
    BIF_RET(CMP_GT(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE sge_2(BIF_ALIST_2)
{
    BIF_RET(CMP_GE(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE slt_2(BIF_ALIST_2)
{
    BIF_RET(CMP_LT(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE sle_2(BIF_ALIST_2)
{
    BIF_RET(CMP_LE(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE seq_2(BIF_ALIST_2)
{
    BIF_RET(eq(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE seqeq_2(BIF_ALIST_2)
{
    BIF_RET(CMP_EQ(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE sneq_2(BIF_ALIST_2)
{
    BIF_RET(eq(BIF_ARG_1, BIF_ARG_2) ? am_false : am_true);
}

BIF_RETTYPE sneqeq_2(BIF_ALIST_2)
{
    BIF_RET(CMP_NE(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE is_atom_1(BIF_ALIST_1)
{
    if (is_atom(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}
	
BIF_RETTYPE is_float_1(BIF_ALIST_1)
{
    if (is_float(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_integer_1(BIF_ALIST_1)
{
    if (is_integer(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_list_1(BIF_ALIST_1)
{
    if (is_list(BIF_ARG_1) || is_nil(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_number_1(BIF_ALIST_1)
{
    if (is_number(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}


BIF_RETTYPE is_pid_1(BIF_ALIST_1)
{
    if (is_pid(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_port_1(BIF_ALIST_1)
{
    if (is_port(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_reference_1(BIF_ALIST_1)
{
    if (is_ref(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_tuple_1(BIF_ALIST_1)
{
    if (is_tuple(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_binary_1(BIF_ALIST_1)
{
    if (is_binary(BIF_ARG_1) && binary_bitsize(BIF_ARG_1) == 0) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_bitstring_1(BIF_ALIST_1)
{
    if (is_binary(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_function_1(BIF_ALIST_1)
{
    if (is_any_fun(BIF_ARG_1)) {
	BIF_RET(am_true);
    } else {
	BIF_RET(am_false);
    }
}

BIF_RETTYPE is_function_2(BIF_ALIST_2)
{
    BIF_RET(erl_is_function(BIF_P, BIF_ARG_1, BIF_ARG_2));
}

Eterm erl_is_function(Process* p, Eterm arg1, Eterm arg2)
{
    Sint arity;

    /*
     * Verify argument 2 (arity); arity must be >= 0.
     */ 
    if (is_small(arg2)) {
	arity = signed_val(arg2);
	if (arity < 0) {
	error:
	    BIF_ERROR(p, BADARG);
	}
    } else if (is_big(arg2) && !bignum_header_is_neg(*big_val(arg2))) {
	/* A positive bignum is OK, but can't possibly match. */
	arity = -1;
    } else {
	/* Everything else (including negative bignum) is an error. */
	goto error;
    }

    if (is_fun(arg1)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(arg1);

	if (funp->arity == (Uint) arity) {
	    BIF_RET(am_true);
	}
    } else if (is_export(arg1)) {
	Export* exp = (Export *) (export_val(arg1)[1]);

	if (exp->code[2] == (Uint) arity) {
	    BIF_RET(am_true);
	}
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_boolean_1(BIF_ALIST_1)
{
    if (BIF_ARG_1 == am_true || BIF_ARG_1 == am_false) {
	BIF_RET(am_true);
    } else {
	BIF_RET(am_false);
    }
}



/*
 * The compiler usually translates calls to is_record/2 to more primitive
 * operations. In some cases this is not possible. We'll need to implement
 * a weak version of is_record/2 as BIF (the size of the record cannot
 * be verified).
 */
BIF_RETTYPE is_record_2(BIF_ALIST_2) 
{
    Eterm *t;

    if (is_not_atom(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_tuple(BIF_ARG_1) &&
	arityval(*(t = tuple_val(BIF_ARG_1))) >= 1 &&
	t[1] == BIF_ARG_2) {
 	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}


/*
 * Record test cannot actually be a bif. The epp processor is involved in
 * the real guard test, we have to add one more parameter, the 
 * return value of record_info(size, Rec), which is the arity of the TUPLE.
 * his may seem awkward when applied from the shell, where the plain
 * tuple test is more understandable, I think...
 */
BIF_RETTYPE is_record_3(BIF_ALIST_3) 
{
    Eterm *t;
    if (is_not_atom(BIF_ARG_2) || is_not_small(BIF_ARG_3)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_tuple(BIF_ARG_1) && 
	arityval(*(t = tuple_val(BIF_ARG_1))) == signed_val(BIF_ARG_3)
	&& t[1] == BIF_ARG_2) {
 	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}
	
BIF_RETTYPE is_map_1(BIF_ALIST_1)
{
    if (is_map(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}
