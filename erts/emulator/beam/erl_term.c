/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

#if HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_map.h"
#include <stdlib.h>
#include <stdio.h>

void
erts_set_literal_tag(Eterm *term, Eterm *hp_start, Eterm hsz)
{
#ifdef TAG_LITERAL_PTR
    Eterm *hp_end, *hp;
    
    hp_end = hp_start + hsz;
    hp = hp_start;

    while (hp < hp_end) {
	switch (primary_tag(*hp)) {
	case TAG_PRIMARY_BOXED:
	case TAG_PRIMARY_LIST:
	    *hp |= TAG_LITERAL_PTR;
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(*hp)) {
		hp += thing_arityval(*hp);
	    }
	    break;
	default:
	    break;
	}
	
	hp++;
    }
    if (is_boxed(*term) || is_list(*term))
	*term |= TAG_LITERAL_PTR;
#endif
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

ET_DEFINE_CHECKED(Eterm,make_boxed,const Eterm*,_is_taggable_pointer);
ET_DEFINE_CHECKED(int,is_boxed,Eterm,!is_header);
ET_DEFINE_CHECKED(Eterm*,boxed_val,Wterm,_boxed_precond);
ET_DEFINE_CHECKED(Eterm,make_list,const Eterm*,_is_taggable_pointer);
ET_DEFINE_CHECKED(int,is_not_list,Eterm,!is_header);
ET_DEFINE_CHECKED(Eterm*,list_val,Wterm,_list_precond);
ET_DEFINE_CHECKED(Uint,unsigned_val,Eterm,is_small);
ET_DEFINE_CHECKED(Sint,signed_val,Eterm,is_small);
ET_DEFINE_CHECKED(Uint,atom_val,Eterm,is_atom);
ET_DEFINE_CHECKED(Uint,header_arity,Eterm,is_header);
ET_DEFINE_CHECKED(Uint,arityval,Eterm,is_sane_arity_value);
ET_DEFINE_CHECKED(Uint,thing_arityval,Eterm,is_thing);
ET_DEFINE_CHECKED(Uint,thing_subtag,Eterm,is_thing);
ET_DEFINE_CHECKED(Eterm*,binary_val,Wterm,is_binary);
ET_DEFINE_CHECKED(Eterm*,fun_val,Wterm,is_fun);
ET_DEFINE_CHECKED(int,bignum_header_is_neg,Eterm,_is_bignum_header);
ET_DEFINE_CHECKED(Eterm,bignum_header_neg,Eterm,_is_bignum_header);
ET_DEFINE_CHECKED(Uint,bignum_header_arity,Eterm,_is_bignum_header);
ET_DEFINE_CHECKED(Eterm*,big_val,Wterm,is_big);
ET_DEFINE_CHECKED(Eterm*,float_val,Wterm,is_float);
ET_DEFINE_CHECKED(Eterm*,tuple_val,Wterm,is_tuple);
ET_DEFINE_CHECKED(struct erl_node_*,internal_pid_node,Eterm,is_internal_pid);
ET_DEFINE_CHECKED(struct erl_node_*,internal_port_node,Eterm,is_internal_port);
ET_DEFINE_CHECKED(Eterm*,internal_ref_val,Wterm,is_internal_ref);
ET_DEFINE_CHECKED(Uint,internal_ref_data_words,Wterm,is_internal_ref);
ET_DEFINE_CHECKED(Uint32*,internal_ref_data,Wterm,is_internal_ref);
ET_DEFINE_CHECKED(struct erl_node_*,internal_ref_node,Eterm,is_internal_ref);
ET_DEFINE_CHECKED(Eterm*,external_val,Wterm,is_external);
ET_DEFINE_CHECKED(Uint,external_data_words,Wterm,is_external);
ET_DEFINE_CHECKED(Uint,external_pid_data_words,Wterm,is_external_pid);
ET_DEFINE_CHECKED(Uint,external_pid_data,Wterm,is_external_pid);
ET_DEFINE_CHECKED(struct erl_node_*,external_pid_node,Wterm,is_external_pid);
ET_DEFINE_CHECKED(Uint,external_port_data_words,Wterm,is_external_port);
ET_DEFINE_CHECKED(Uint,external_port_data,Wterm,is_external_port);
ET_DEFINE_CHECKED(struct erl_node_*,external_port_node,Wterm,is_external_port);
ET_DEFINE_CHECKED(Uint,external_ref_data_words,Wterm,is_external_ref);
ET_DEFINE_CHECKED(Uint32*,external_ref_data,Wterm,is_external_ref);
ET_DEFINE_CHECKED(struct erl_node_*,external_ref_node,Eterm,is_external_ref);
ET_DEFINE_CHECKED(Eterm*,export_val,Wterm,is_export);
ET_DEFINE_CHECKED(Uint,external_thing_data_words,ExternalThing*,is_thing_ptr);

ET_DEFINE_CHECKED(Eterm,make_cp,UWord *,_is_taggable_pointer);
ET_DEFINE_CHECKED(UWord *,cp_val,Eterm,is_CP);
ET_DEFINE_CHECKED(Uint,catch_val,Eterm,is_catch);
ET_DEFINE_CHECKED(Uint,loader_x_reg_index,Uint,_is_loader_x_reg);
ET_DEFINE_CHECKED(Uint,loader_y_reg_index,Uint,_is_loader_y_reg);

#endif	/* ET_DEBUG */
