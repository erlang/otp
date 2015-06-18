/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2012. All Rights Reserved.
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

#define ERL_WANT_GC_INTERNALS__

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_gc.h"
#include "big.h"
#include "erl_map.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "dtrace-wrapper.h"

static void move_one_frag(Eterm** hpp, ErlHeapFragment*, ErlOffHeap*);

/*
 *  Copy object "obj" to process p.
 */
Eterm
copy_object(Eterm obj, Process* to)
{
    Uint size = size_object(obj);
    Eterm* hp = HAlloc(to, size);
    Eterm res;

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(copy_object)) {
        DTRACE_CHARBUF(proc_name, 64);

        erts_snprintf(proc_name, sizeof(DTRACE_CHARBUF_NAME(proc_name)),
                      "%T", to->common.id);
        DTRACE2(copy_object, proc_name, size);
    }
#endif
    res = copy_struct(obj, size, &hp, &to->off_heap);
#ifdef DEBUG
    if (eq(obj, res) == 0) {
	erl_exit(ERTS_ABORT_EXIT, "copy not equal to source\n");
    }
#endif
    return res;
}

/*
 * Return the "flat" size of the object.
 */

#if HALFWORD_HEAP
Uint size_object_rel(Eterm obj, Eterm* base)
#else
Uint size_object(Eterm obj)
#endif
{
    Uint sum = 0;
    Eterm* ptr;
    int arity;

    DECLARE_ESTACK(s);
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
	    sum += 2;
	    ptr = list_val_rel(obj,base);
	    obj = *ptr++;
	    if (!IS_CONST(obj)) {
		ESTACK_PUSH(s, obj);
	    }	    
	    obj = *ptr;
	    break;
	case TAG_PRIMARY_BOXED:
	    {
		Eterm hdr = *boxed_val_rel(obj,base);
		ASSERT(is_header(hdr));
		switch (hdr & _TAG_HEADER_MASK) {
		case ARITYVAL_SUBTAG:
		    ptr = tuple_val_rel(obj,base);
		    arity = header_arity(hdr);
		    sum += arity + 1;
		    if (arity == 0) { /* Empty tuple -- unusual. */
			goto pop_next;
		    }
		    while (arity-- > 1) {
			obj = *++ptr;
			if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			}
		    }
		    obj = *++ptr;
		    break;
		case FUN_SUBTAG:
		    {
			Eterm* bptr = fun_val_rel(obj,base);
			ErlFunThing* funp = (ErlFunThing *) bptr;
			unsigned eterms = 1 /* creator */ + funp->num_free;
			unsigned sz = thing_arityval(hdr);
			sum += 1 /* header */ + sz + eterms;
			bptr += 1 /* header */ + sz;
			while (eterms-- > 1) {
			  obj = *bptr++;
			  if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			  }
			}
			obj = *bptr;
			break;
		    }
		case MAP_SUBTAG:
		    switch (MAP_HEADER_TYPE(hdr)) {
			case MAP_HEADER_TAG_FLATMAP_HEAD :
                            {
                                Uint n;
                                flatmap_t *mp;
                                mp  = (flatmap_t*)flatmap_val_rel(obj,base);
                                ptr = (Eterm *)mp;
                                n   = flatmap_get_size(mp) + 1;
                                sum += n + 2;
                                ptr += 2; /* hdr + size words */
                                while (n--) {
                                    obj = *ptr++;
                                    if (!IS_CONST(obj)) {
                                        ESTACK_PUSH(s, obj);
                                    }
                                }
                                goto pop_next;
                            }
			case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
			case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
			case MAP_HEADER_TAG_HAMT_NODE_BITMAP :
			    {
				Eterm *head;
				Uint sz;
				head  = hashmap_val_rel(obj, base);
				sz    = hashmap_bitcount(MAP_HEADER_VAL(hdr));
				sum  += 1 + sz + header_arity(hdr);
				head += 1 + header_arity(hdr);

				if (sz == 0) {
				    goto pop_next;
				}
				while(sz-- > 1) {
				    obj = head[sz];
				    if (!IS_CONST(obj)) {
					ESTACK_PUSH(s, obj);
				    }
				}
				obj = head[0];
			    }
			    break;
			default:
			    erl_exit(ERTS_ABORT_EXIT, "size_object: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
		    }
		    break;
		case SUB_BINARY_SUBTAG:
		    {
			Eterm real_bin;
			ERTS_DECLARE_DUMMY(Uint offset); /* Not used. */
			Uint bitsize;
			Uint bitoffs;
			Uint extra_bytes;
			Eterm hdr;
			ERTS_GET_REAL_BIN_REL(obj, real_bin, offset, bitoffs, bitsize, base);
			if ((bitsize + bitoffs) > 8) {
			    sum += ERL_SUB_BIN_SIZE;
			    extra_bytes = 2;
			} else if ((bitsize + bitoffs) > 0) {
			    sum += ERL_SUB_BIN_SIZE;
			    extra_bytes = 1;
			} else {
			    extra_bytes = 0;
			}
			hdr = *binary_val_rel(real_bin,base);
			if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
			    sum += PROC_BIN_SIZE;
			} else {
			    sum += heap_bin_size(binary_size_rel(obj,base)+extra_bytes);
			}
			goto pop_next;
		    }
		    break;
                case BIN_MATCHSTATE_SUBTAG:
		    erl_exit(ERTS_ABORT_EXIT,
			     "size_object: matchstate term not allowed");
		default:
		    sum += thing_arityval(hdr) + 1;
		    goto pop_next;
		}
	    }
	    break;
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);
		return sum;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT, "size_object: bad tag for %#x\n", obj);
	}
    }
}

/*
 *  Copy a structure to a heap.
 */
#if HALFWORD_HEAP
Eterm copy_struct_rel(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap,
                      Eterm* src_base, Eterm* dst_base)
#else
Eterm copy_struct(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
#endif
{
    char* hstart;
    Uint hsize;
    Eterm* htop;
    Eterm* hbot;
    Eterm* hp;
    Eterm* objp;
    Eterm* tp;
    Eterm  res;
    Eterm  elem;
    Eterm* tailp;
    Eterm* argp;
    Eterm* const_tuple;
    Eterm hdr;
    int i;
#ifdef DEBUG
    Eterm org_obj = obj;
    Uint org_sz = sz;
#endif

    if (IS_CONST(obj))
	return obj;

    DTRACE1(copy_struct, (int32_t)sz);

    hp = htop = *hpp;
    hbot   = htop + sz;
    hstart = (char *)htop;
    hsize = (char*) hbot - hstart;
    const_tuple = 0;

    /* Copy the object onto the heap */
    switch (primary_tag(obj)) {
    case TAG_PRIMARY_LIST:
	argp = &res;
	objp = list_val_rel(obj,src_base);
	goto L_copy_list;
    case TAG_PRIMARY_BOXED: argp = &res; goto L_copy_boxed;
    default:
	erl_exit(ERTS_ABORT_EXIT,
		 "%s, line %d: Internal error in copy_struct: 0x%08x\n",
		 __FILE__, __LINE__,obj);
    }

 L_copy:
    while (hp != htop) {
	obj = *hp;

	switch (primary_tag(obj)) {
	case TAG_PRIMARY_IMMED1:
	    hp++;
	    break;
	case TAG_PRIMARY_LIST:
	    objp = list_val_rel(obj,src_base);
	#if !HALFWORD_HEAP || defined(DEBUG)
	    if (in_area(objp,hstart,hsize)) {
		ASSERT(!HALFWORD_HEAP);
		hp++;
		break;
	    }
	#endif
	    argp = hp++;
	    /* Fall through */

	L_copy_list:
	    tailp = argp;
	    for (;;) {
		tp = tailp;
		elem = CAR(objp);
		if (IS_CONST(elem)) {
		    hbot -= 2;
		    CAR(hbot) = elem;
		    tailp = &CDR(hbot);
		}
		else {
		    CAR(htop) = elem;
		#if HALFWORD_HEAP
		    CDR(htop) = CDR(objp);
		    *tailp = make_list_rel(htop,dst_base);
		    htop += 2;
		    goto L_copy;
		#else
		    tailp = &CDR(htop);
		    htop += 2;
		#endif
		}
		ASSERT(!HALFWORD_HEAP || tp < hp || tp >= hbot);
		*tp = make_list_rel(tailp - 1, dst_base);
		obj = CDR(objp);
		if (!is_list(obj)) {
		    break;
		}
		objp = list_val_rel(obj,src_base);
	    }
	    switch (primary_tag(obj)) {
	    case TAG_PRIMARY_IMMED1: *tailp = obj; goto L_copy;
	    case TAG_PRIMARY_BOXED: argp = tailp; goto L_copy_boxed;
	    default:
		erl_exit(ERTS_ABORT_EXIT,
			 "%s, line %d: Internal error in copy_struct: 0x%08x\n",
			 __FILE__, __LINE__,obj);
	    }
	    
	case TAG_PRIMARY_BOXED:
	#if !HALFWORD_HEAP || defined(DEBUG)
	    if (in_area(boxed_val_rel(obj,src_base),hstart,hsize)) {
		ASSERT(!HALFWORD_HEAP);
		hp++;
		break;
	    }
	#endif
	    argp = hp++;

	L_copy_boxed:
	    objp = boxed_val_rel(obj, src_base);
	    hdr = *objp;
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    int const_flag = 1; /* assume constant tuple */
		    i = arityval(hdr);
		    *argp = make_tuple_rel(htop, dst_base);
		    tp = htop;	/* tp is pointer to new arity value */
		    *htop++ = *objp++; /* copy arity value */
		    while (i--) {
			elem = *objp++;
			if (!IS_CONST(elem)) {
			    const_flag = 0;
			}
			*htop++ = elem;
		    }
		    if (const_flag) {
			const_tuple = tp; /* this is the latest const_tuple */
		    }
		}
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb;

		    pb = (ProcBin *) objp;
		    if (pb->flags) {
			erts_emasculate_writable_binary(pb);
		    }
		    i = thing_arityval(*objp) + 1;
		    hbot -= i;
		    tp = hbot;
		    while (i--)  {
			*tp++ = *objp++;
		    }
		    *argp = make_binary_rel(hbot, dst_base);
		    pb = (ProcBin*) hbot;
		    erts_refc_inc(&pb->val->refc, 2);
		    pb->next = off_heap->first;
		    pb->flags = 0;
		    off_heap->first = (struct erl_off_heap_header*) pb;
		    OH_OVERHEAD(off_heap, pb->size / sizeof(Eterm));
		}
		break;
	    case SUB_BINARY_SUBTAG:
		{
		    ErlSubBin* sb = (ErlSubBin *) objp;
		    Eterm real_bin = sb->orig;
		    Uint bit_offset = sb->bitoffs;
		    Uint bit_size = sb -> bitsize;
		    Uint offset = sb->offs;
		    size_t size = sb->size;
		    Uint extra_bytes;
		    Uint real_size;
		    if ((bit_size + bit_offset) > 8) {
			extra_bytes = 2;
		    } else if ((bit_size + bit_offset) > 0) {
			extra_bytes = 1;
		    } else {
			extra_bytes = 0;
		    } 
		    real_size = size+extra_bytes;
		    objp = binary_val_rel(real_bin,src_base);
		    if (thing_subtag(*objp) == HEAP_BINARY_SUBTAG) {
			ErlHeapBin* from = (ErlHeapBin *) objp;
			ErlHeapBin* to;
			i = heap_bin_size(real_size);
			hbot -= i;
			to = (ErlHeapBin *) hbot;
			to->thing_word = header_heap_bin(real_size);
			to->size = real_size;
			sys_memcpy(to->data, ((byte *)from->data)+offset, real_size);
		    } else {
			ProcBin* from = (ProcBin *) objp;
			ProcBin* to;
			
			ASSERT(thing_subtag(*objp) == REFC_BINARY_SUBTAG);
			if (from->flags) {
			    erts_emasculate_writable_binary(from);
			}
			hbot -= PROC_BIN_SIZE;
			to = (ProcBin *) hbot;
			to->thing_word = HEADER_PROC_BIN;
			to->size = real_size;
			to->val = from->val;
			erts_refc_inc(&to->val->refc, 2);
			to->bytes = from->bytes + offset;
			to->next = off_heap->first;
			to->flags = 0;
			off_heap->first = (struct erl_off_heap_header*) to;
			OH_OVERHEAD(off_heap, to->size / sizeof(Eterm));
		    }
		    *argp = make_binary_rel(hbot, dst_base);
		    if (extra_bytes != 0) {
			ErlSubBin* res;
			hbot -= ERL_SUB_BIN_SIZE;
			res = (ErlSubBin *) hbot;
			res->thing_word = HEADER_SUB_BIN;
			res->size = size;
			res->bitsize = bit_size;
			res->bitoffs = bit_offset;
			res->offs = 0;
			res->is_writable = 0;
			res->orig = *argp;
			*argp = make_binary_rel(hbot, dst_base);
		    }
		    break;
		}
		break;
	    case FUN_SUBTAG:
		{
		    ErlFunThing* funp = (ErlFunThing *) objp;

		    i =  thing_arityval(hdr) + 2 + funp->num_free;
		    tp = htop;
		    while (i--)  {
			*htop++ = *objp++;
		    }
		    funp = (ErlFunThing *) tp;
		    funp->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*) funp;
		    erts_refc_inc(&funp->fe->refc, 2);
		    *argp = make_fun_rel(tp, dst_base);
		}
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		  ExternalThing *etp = (ExternalThing *) htop;

		  i  = thing_arityval(hdr) + 1;
		  tp = htop;

		  while (i--)  {
		    *htop++ = *objp++;
		  }

		  etp->next = off_heap->first;
		  off_heap->first = (struct erl_off_heap_header*)etp;
		  erts_refc_inc(&etp->node->refc, 2);

		  *argp = make_external_rel(tp, dst_base);
		}
		break;
	    case MAP_SUBTAG:
		tp = htop;
		switch (MAP_HEADER_TYPE(hdr)) {
		    case MAP_HEADER_TAG_FLATMAP_HEAD :
                        i = flatmap_get_size(objp) + 3;
                        *argp = make_flatmap_rel(htop, dst_base);
                        while (i--) {
                            *htop++ = *objp++;
                        }
			break;
		    case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
		    case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
			*htop++ = *objp++;
		    case MAP_HEADER_TAG_HAMT_NODE_BITMAP :
			i = 1 + hashmap_bitcount(MAP_HEADER_VAL(hdr));
			while (i--)  { *htop++ = *objp++; }
			*argp = make_hashmap_rel(tp, dst_base);
			break;
		    default:
			erl_exit(ERTS_ABORT_EXIT, "copy_struct: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
		}
		break;
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "copy_struct: matchstate term not allowed");
	    default:
		i = thing_arityval(hdr)+1;
		hbot -= i;
		tp = hbot;
		*argp = make_boxed_rel(hbot, dst_base);
		while (i--) {
		    *tp++ = *objp++;
		}
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(obj) || hp == const_tuple) {
		hp += header_arity(obj) + 1;
	    } else {
		hp++;
	    }
	    break;
	}
    }

#ifdef DEBUG
    if (htop != hbot)
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error in copy_struct() when copying %T:"
		 " htop=%p != hbot=%p (sz=%beu)\n",
		 org_obj, htop, hbot, org_sz); 
#else
    if (htop > hbot) {
	erl_exit(ERTS_ABORT_EXIT,
		 "Internal error in copy_struct(): htop, hbot overrun\n");
    }
#endif
    *hpp = (Eterm *) (hstart+hsize);
    return res;
}

/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 *
 * NOTE: Assumes that term is a tuple (ptr is an untagged tuple ptr).
 */
#if HALFWORD_HEAP
Eterm copy_shallow_rel(Eterm* ptr, Uint sz, Eterm** hpp, ErlOffHeap* off_heap,
		       Eterm* src_base)
#else
Eterm copy_shallow(Eterm* ptr, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
#endif
{
    Eterm* tp = ptr;
    Eterm* hp = *hpp;
    const Eterm res = make_tuple(hp);
#if HALFWORD_HEAP
    const Sint offs = COMPRESS_POINTER(hp - (tp - src_base));
#else
    const Sint offs = (hp - tp) * sizeof(Eterm);
#endif

    while (sz--) {
	Eterm val = *tp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    *hp++ = byte_offset_ptr(val, offs);
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb = (ProcBin *) (tp-1);
		    erts_refc_inc(&pb->val->refc, 2);
		    OH_OVERHEAD(off_heap, pb->size / sizeof(Eterm));
		}
		goto off_heap_common;

	    case FUN_SUBTAG:
		{
		    ErlFunThing* funp = (ErlFunThing *) (tp-1);
		    erts_refc_inc(&funp->fe->refc, 2);
		}
		goto off_heap_common;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		    ExternalThing* etp = (ExternalThing *) (tp-1);
		    erts_refc_inc(&etp->node->refc, 2);
		}
	    off_heap_common:
		{
		    struct erl_off_heap_header* ohh = (struct erl_off_heap_header*)(hp-1);
		    int tari = thing_arityval(val);
		    
		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    ohh->next = off_heap->first;
		    off_heap->first = ohh;
		}
		break;
	    default:
		{
		    int tari = header_arity(val);
    
		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		}
		break;
	    }
	    break;
	}
    }
    *hpp = hp;
    return res;
}

/* Move all terms in heap fragments into heap. The terms must be guaranteed to 
 * be contained within the fragments. The source terms are destructed with
 * move markers.
 * Typically used to copy a multi-fragmented message (from NIF).
 */
void move_multi_frags(Eterm** hpp, ErlOffHeap* off_heap, ErlHeapFragment* first,
		      Eterm* refs, unsigned nrefs)
{
    ErlHeapFragment* bp;
    Eterm* hp_start = *hpp;
    Eterm* hp_end;
    Eterm* hp;
    unsigned i;

    for (bp=first; bp!=NULL; bp=bp->next) {
	move_one_frag(hpp, bp, off_heap);
    }
    hp_end = *hpp;
    for (hp=hp_start; hp<hp_end; ++hp) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *hp;
	switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED:
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED_BOXED(val)) {
		ASSERT(is_boxed(val));
		*hp = val;
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = list_val(gval);
	    val = *ptr;
	    if (IS_MOVED_CONS(val)) {
		*hp = ptr[1];
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(gval)) {
		hp += thing_arityval(gval);
	    }
	    break;
	}
    }
    for (i=0; i<nrefs; ++i) {
	refs[i] = follow_moved(refs[i]);
    }
}

static void
move_one_frag(Eterm** hpp, ErlHeapFragment* frag, ErlOffHeap* off_heap)
{
    Eterm* ptr = frag->mem;
    Eterm* end = ptr + frag->used_size;
    Eterm dummy_ref;
    Eterm* hp = *hpp;

    while (ptr != end) {
	Eterm val;
	ASSERT(ptr < end);
	val = *ptr;
	ASSERT(val != ERTS_HOLE_MARKER);
	if (is_header(val)) {
	    struct erl_off_heap_header* hdr = (struct erl_off_heap_header*)hp;
	    ASSERT(ptr + header_arity(val) < end);
	    MOVE_BOXED(ptr, val, hp, &dummy_ref);	    
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case REFC_BINARY_SUBTAG:
	    case FUN_SUBTAG:
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		hdr->next = off_heap->first;
		off_heap->first = hdr;
		break;
	    }
	}
	else { /* must be a cons cell */
	    ASSERT(ptr+1 < end);
	    MOVE_CONS(ptr, val, hp, &dummy_ref);
	    ptr += 2;
	}
    }
    *hpp = hp;
    OH_OVERHEAD(off_heap, frag->off_heap.overhead);
    frag->off_heap.first = NULL;
}

