/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include <stdlib.h>
#include <stdio.h>

__decl_noreturn static void __noreturn
et_abort(const char *expr, const char *file, unsigned line)
{
#ifdef EXIT_ON_ET_ABORT
    static int have_been_called = 0;

    if (have_been_called) {
	abort();
    } else {
	/*
	 * Prevent infinite loop.
	 */
	have_been_called = 1;
	erl_exit(1, "TYPE ASSERTION FAILED, file %s, line %u: %s\n", file, line, expr);
    }
#else
    erts_fprintf(stderr, "TYPE ASSERTION FAILED, file %s, line %u: %s\n", file, line, expr);
    abort();
#endif
}

#if ET_DEBUG
#define ET_ASSERT(expr,file,line) \
do { \
    if (!(expr)) \
	et_abort(#expr, file, line); \
} while(0)
#else
#define ET_ASSERT(expr,file,line)	do { } while(0)
#endif

#if ET_DEBUG
unsigned tag_val_def_debug(Eterm x, const char *file, unsigned line)
#else
unsigned tag_val_def(Eterm x)
#define file __FILE__
#define line __LINE__
#endif
{
    static char msg[32];

    switch (x & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	ET_ASSERT(_list_precond(x),file,line);
	return LIST_DEF;
      case TAG_PRIMARY_BOXED: {
	  Eterm hdr = *boxed_val(x);
	  ET_ASSERT(is_header(hdr),file,line);
	  switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	    case (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE):	return TUPLE_DEF;
	    case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):	return BIG_DEF;
	    case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):	return BIG_DEF;
	    case (_TAG_HEADER_REF >> _TAG_PRIMARY_SIZE):	return REF_DEF;
	    case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):	return FLOAT_DEF;
	    case (_TAG_HEADER_EXPORT >> _TAG_PRIMARY_SIZE):     return EXPORT_DEF;
	    case (_TAG_HEADER_FUN >> _TAG_PRIMARY_SIZE):	return FUN_DEF;
	    case (_TAG_HEADER_EXTERNAL_PID >> _TAG_PRIMARY_SIZE):	return EXTERNAL_PID_DEF;
	    case (_TAG_HEADER_EXTERNAL_PORT >> _TAG_PRIMARY_SIZE):	return EXTERNAL_PORT_DEF;
	    case (_TAG_HEADER_EXTERNAL_REF >> _TAG_PRIMARY_SIZE):	return EXTERNAL_REF_DEF;
	    default:						return BINARY_DEF;
	  }
	  break;
      }
      case TAG_PRIMARY_IMMED1: {
	  switch ((x & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	    case (_TAG_IMMED1_PID >> _TAG_PRIMARY_SIZE):	return PID_DEF;
	    case (_TAG_IMMED1_PORT >> _TAG_PRIMARY_SIZE):	return PORT_DEF;
	    case (_TAG_IMMED1_IMMED2 >> _TAG_PRIMARY_SIZE): {
		switch ((x & _TAG_IMMED2_MASK) >> _TAG_IMMED1_SIZE) {
		  case (_TAG_IMMED2_ATOM >> _TAG_IMMED1_SIZE):	return ATOM_DEF;
		  case (_TAG_IMMED2_NIL >> _TAG_IMMED1_SIZE):	return NIL_DEF;
		}
		break;
	    }
	    case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):	return SMALL_DEF;
	  }
	  break;
      }
    }
    sprintf(msg, "tag_val_def: %#lx", (unsigned long) x);
    et_abort(msg, file, line);
#undef file
#undef line
}

/*
 * XXX: define NUMBER_CODE() here when new representation is used
 */

#if ET_DEBUG
#define ET_DEFINE_CHECKED(FUNTY,FUN,ARGTY,PRECOND) \
FUNTY checked_##FUN(ARGTY x, const char *file, unsigned line) \
{ \
    ET_ASSERT(PRECOND(x),file,line); \
    return _unchecked_##FUN(x); \
}

ET_DEFINE_CHECKED(Eterm,make_boxed,Eterm*,_is_taggable_pointer);
ET_DEFINE_CHECKED(int,is_boxed,Eterm,!is_header);
ET_DEFINE_CHECKED(Eterm*,boxed_val,Eterm,_boxed_precond);
ET_DEFINE_CHECKED(Eterm,make_list,Eterm*,_is_taggable_pointer);
ET_DEFINE_CHECKED(int,is_not_list,Eterm,!is_header);
ET_DEFINE_CHECKED(Eterm*,list_val,Eterm,_list_precond);
ET_DEFINE_CHECKED(Uint,unsigned_val,Eterm,is_small);
ET_DEFINE_CHECKED(Sint,signed_val,Eterm,is_small);
ET_DEFINE_CHECKED(Uint,atom_val,Eterm,is_atom);
ET_DEFINE_CHECKED(Uint,header_arity,Eterm,is_header);
ET_DEFINE_CHECKED(Uint,arityval,Eterm,is_arity_value);
ET_DEFINE_CHECKED(Uint,thing_arityval,Eterm,is_thing);
ET_DEFINE_CHECKED(Uint,thing_subtag,Eterm,is_thing);
ET_DEFINE_CHECKED(Eterm*,binary_val,Eterm,is_binary);
ET_DEFINE_CHECKED(Eterm*,fun_val,Eterm,is_fun);
ET_DEFINE_CHECKED(int,bignum_header_is_neg,Eterm,_is_bignum_header);
ET_DEFINE_CHECKED(Eterm,bignum_header_neg,Eterm,_is_bignum_header);
ET_DEFINE_CHECKED(Uint,bignum_header_arity,Eterm,_is_bignum_header);
ET_DEFINE_CHECKED(Eterm*,big_val,Eterm,is_big);
ET_DEFINE_CHECKED(Eterm*,float_val,Eterm,is_float);
ET_DEFINE_CHECKED(Eterm*,tuple_val,Eterm,is_tuple);
ET_DEFINE_CHECKED(Uint,internal_pid_data,Eterm,is_internal_pid);
ET_DEFINE_CHECKED(struct erl_node_*,internal_pid_node,Eterm,is_internal_pid);
ET_DEFINE_CHECKED(Uint,internal_port_data,Eterm,is_internal_port);
ET_DEFINE_CHECKED(struct erl_node_*,internal_port_node,Eterm,is_internal_port);
ET_DEFINE_CHECKED(Eterm*,internal_ref_val,Eterm,is_internal_ref);
ET_DEFINE_CHECKED(Uint,internal_ref_data_words,Eterm,is_internal_ref);
ET_DEFINE_CHECKED(Uint32*,internal_ref_data,Eterm,is_internal_ref);
ET_DEFINE_CHECKED(struct erl_node_*,internal_ref_node,Eterm,is_internal_ref);
ET_DEFINE_CHECKED(Eterm*,external_val,Eterm,is_external);
ET_DEFINE_CHECKED(Uint,external_data_words,Eterm,is_external);
ET_DEFINE_CHECKED(Uint,external_pid_data_words,Eterm,is_external_pid);
ET_DEFINE_CHECKED(Uint,external_pid_data,Eterm,is_external_pid);
ET_DEFINE_CHECKED(struct erl_node_*,external_pid_node,Eterm,is_external_pid);
ET_DEFINE_CHECKED(Uint,external_port_data_words,Eterm,is_external_port);
ET_DEFINE_CHECKED(Uint,external_port_data,Eterm,is_external_port);
ET_DEFINE_CHECKED(struct erl_node_*,external_port_node,Eterm,is_external_port);
ET_DEFINE_CHECKED(Uint,external_ref_data_words,Eterm,is_external_ref);
ET_DEFINE_CHECKED(Uint32*,external_ref_data,Eterm,is_external_ref);
ET_DEFINE_CHECKED(struct erl_node_*,external_ref_node,Eterm,is_external_ref);
ET_DEFINE_CHECKED(Eterm*,export_val,Eterm,is_export);

ET_DEFINE_CHECKED(Eterm,make_cp,UWord *,_is_taggable_pointer);
ET_DEFINE_CHECKED(UWord *,cp_val,Eterm,is_CP);
ET_DEFINE_CHECKED(Uint,catch_val,Eterm,is_catch);
ET_DEFINE_CHECKED(Uint,x_reg_offset,Uint,_is_xreg);
ET_DEFINE_CHECKED(Uint,y_reg_offset,Uint,_is_yreg);
ET_DEFINE_CHECKED(Uint,x_reg_index,Uint,_is_xreg);
ET_DEFINE_CHECKED(Uint,y_reg_index,Uint,_is_yreg);

#endif	/* ET_DEBUG */
