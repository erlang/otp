/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_gc.h"
#include "erl_nmgc.h"
#include "big.h"
#include "erl_binary.h"
#include "erl_bits.h"

#ifdef HYBRID
MA_STACK_DECLARE(src);
MA_STACK_DECLARE(dst);
MA_STACK_DECLARE(offset);
#endif

static void move_one_frag(Eterm** hpp, Eterm* src, Uint src_sz, ErlOffHeap*);

void
init_copy(void)
{
#ifdef HYBRID
    MA_STACK_ALLOC(src);
    MA_STACK_ALLOC(dst);
    MA_STACK_ALLOC(offset);
#endif
}

/*
 *  Copy object "obj" to process p.
 */
Eterm
copy_object(Eterm obj, Process* to)
{
    Uint size = size_object(obj);
    Eterm* hp = HAlloc(to, size);
    Eterm res;

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

Uint
size_object(Eterm obj)
{
    Uint sum = 0;
    Eterm* ptr;
    int arity;

    DECLARE_ESTACK(s);
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
	    sum += 2;
	    ptr = list_val(obj);
	    obj = *ptr++;
	    if (!IS_CONST(obj)) {
		ESTACK_PUSH(s, obj);
	    }	    
	    obj = *ptr;
	    break;
	case TAG_PRIMARY_BOXED:
	    {
		Eterm hdr = *boxed_val(obj);
		ASSERT(is_header(hdr));
		switch (hdr & _TAG_HEADER_MASK) {
		case ARITYVAL_SUBTAG:
		    ptr = tuple_val(obj);
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
			Eterm* bptr = fun_val(obj);
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
		case SUB_BINARY_SUBTAG:
		    {
			Eterm real_bin;
			Uint offset; /* Not used. */
			Uint bitsize;
			Uint bitoffs;
			Uint extra_bytes;
			Eterm hdr;
			ERTS_GET_REAL_BIN(obj, real_bin, offset, bitoffs, bitsize);
			if ((bitsize + bitoffs) > 8) {
			    sum += ERL_SUB_BIN_SIZE;
			    extra_bytes = 2;
			} else if ((bitsize + bitoffs) > 0) {
			    sum += ERL_SUB_BIN_SIZE;
			    extra_bytes = 1;
			} else {
			    extra_bytes = 0;
			}
			hdr = *binary_val(real_bin);
			if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
			    sum += PROC_BIN_SIZE;
			} else {
			    sum += heap_bin_size(binary_size(obj)+extra_bytes);
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
Eterm
copy_struct(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
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

    hp = htop = *hpp;
    hbot   = htop + sz;
    hstart = (char *)htop;
    hsize = (char*) hbot - hstart;
    const_tuple = 0;

    /* Copy the object onto the heap */
    switch (primary_tag(obj)) {
    case TAG_PRIMARY_LIST: argp = &res; goto L_copy_list;
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
	    objp = list_val(obj);
	    if (in_area(objp,hstart,hsize)) {
		hp++;
		break;
	    }
	    argp = hp++;
	    /* Fall through */

	L_copy_list:
	    tailp = argp;
	    while (is_list(obj)) {
		objp = list_val(obj);
		tp = tailp;
		elem = *objp;
		if (IS_CONST(elem)) {
		    *(hbot-2) = elem;
		    tailp = hbot-1;
		    hbot -= 2;
		}
		else {
		    *htop = elem;
		    tailp = htop+1;
		    htop += 2;
		}
		*tp = make_list(tailp - 1);
		obj = *(objp+1);
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
	    if (in_area(boxed_val(obj),hstart,hsize)) {
		hp++;
		break;
	    }
	    argp = hp++;

	L_copy_boxed:
	    objp = boxed_val(obj);
	    hdr = *objp;
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    int const_flag = 1; /* assume constant tuple */
		    i = arityval(hdr);
		    *argp = make_tuple(htop);
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
		    *argp = make_binary(hbot);
		    pb = (ProcBin*) hbot;
		    erts_refc_inc(&pb->val->refc, 2);
		    pb->next = off_heap->mso;
		    pb->flags = 0;
		    off_heap->mso = pb;
		    off_heap->overhead += pb->size / sizeof(Eterm);
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
		    objp = binary_val(real_bin);
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
			to->next = off_heap->mso;
			to->flags = 0;
			off_heap->mso = to;
			off_heap->overhead += to->size / sizeof(Eterm);
		    }
		    *argp = make_binary(hbot);
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
			*argp = make_binary(hbot);
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
#ifndef HYBRID /* FIND ME! */
		    funp = (ErlFunThing *) tp;
		    funp->next = off_heap->funs;
		    off_heap->funs = funp;
		    erts_refc_inc(&funp->fe->refc, 2);
#endif
		    *argp = make_fun(tp);
		}
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		  ExternalThing *etp = (ExternalThing *) htop;

		  i =  thing_arityval(hdr) + 1;
		  tp = htop;

		  while (i--)  {
		    *htop++ = *objp++;
		  }

		  etp->next = off_heap->externals;
		  off_heap->externals = etp;
		  erts_refc_inc(&etp->node->refc, 2);

		  *argp = make_external(tp);
		}
		break;
	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "copy_struct: matchstate term not allowed");
	    default:
		i = thing_arityval(hdr)+1;
		hbot -= i;
		tp = hbot;
		*argp = make_boxed(hbot);
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
		 " htop=%p != hbot=%p (sz=%bpu)\n",
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

#ifdef HYBRID

#ifdef BM_MESSAGE_SIZES
#  define BM_ADD(var,val) (var) += (val);
#else
#  define BM_ADD(var,val)
#endif

#ifdef DEBUG
#  define CLEARMEM(PTR,SIZE) memset(PTR,0,SIZE*sizeof(Eterm))
#else
#  define CLEARMEM(PTR,SIZE)
#endif

#ifdef INCREMENTAL
#define GlobalAlloc(p, need, hp)                                        \
do {                                                                    \
    Uint n = (need);                                                    \
    BM_ADD(words_copied,n);                                             \
    BM_SWAP_TIMER(copy,system);                                         \
    /* If a new collection cycle is started during copy, the message *  \
     * will end up in the old generation and all allocations         *  \
     * thereafter must go directly into the old generation.          */ \
    if (alloc_old) {                                                    \
        erts_incremental_gc((p),n,&dest,1);                             \
        (hp) = erts_inc_alloc(n);                                       \
    } else {                                                            \
        (hp) = IncAlloc((p),n,&dest,1);                                 \
        if (ma_gc_flags & GC_CYCLE_START) {                             \
            alloc_old = 1;                                              \
            global_htop = global_heap;                                  \
            (hp) = erts_inc_alloc(n);                                   \
        }                                                               \
    }                                                                   \
    CLEARMEM((hp),(n));                                                 \
    BM_SWAP_TIMER(system,copy);                                         \
} while(0)

#else /* no INCREMELNTAL */

#define GlobalAlloc(p, need, hp)                                        \
do {                                                                    \
    Uint n = (need);                                                    \
    total_need += n;                                                    \
    if (total_need >= global_heap_sz)                                   \
        erl_exit(ERTS_ABORT_EXIT, "Copying a message (%d words) larger than the nursery simply won't work...\n", total_need); \
    if (global_hend - n < global_htop) {                                \
        BM_SWAP_TIMER(copy,system);                                     \
        erts_global_garbage_collect((p),total_need,NULL,0);             \
        BM_SWAP_TIMER(system,copy);                                     \
        total_need = 0;                                                 \
        ma_src_top = 0;                                                 \
        ma_dst_top = 0;                                                 \
        ma_offset_top = 0;                                              \
        goto copy_start;                                                \
    }                                                                   \
    (hp) = global_htop;                                                 \
    global_htop += n;                                                   \
    BM_ADD(words_copied,n);                                             \
} while(0)
#endif /* INCREMENTAL */

/* Copy a message to the message area. */
Eterm copy_struct_lazy(Process *from, Eterm orig, Uint offs)
{
    Eterm obj;
    Eterm dest;
#ifdef INCREMENTAL
    int alloc_old = 0;
#else
    int total_need = 0;
#endif

    VERBOSE(DEBUG_MESSAGES,
            ("COPY START; %T is sending a message @ 0x%016x\n%T\n",
             from->id, orig, orig));

#ifndef INCREMENTAL
 copy_start:
#endif
    MA_STACK_PUSH(src,orig);
    MA_STACK_PUSH(dst,&dest);
    MA_STACK_PUSH(offset,offs);

    while (ma_src_top > 0) {
        obj = MA_STACK_POP(src);

        /* copy_struct_lazy should never be called with something that
         * do not need to be copied. Within the loop, nothing that do
         * not need copying should be placed in the src-stack.
         */
        ASSERT(!NO_COPY(obj));

        switch (primary_tag(obj)) {
        case TAG_PRIMARY_LIST: {
            Eterm *hp;
            Eterm *objp;

            GlobalAlloc(from,2,hp);
            objp = list_val(obj);

            MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_list(hp));
            MA_STACK_POP(dst);

            /* TODO: Byt ordningen nedan så att CDR pushas först. */

            if (NO_COPY(*objp)) {
                hp[0] = *objp;
#ifdef INCREMENTAL
                if (ptr_within(ptr_val(*objp),inc_fromspc,inc_fromend))
                    INC_STORE(gray,hp,2);
#endif
            } else {
                MA_STACK_PUSH(src,*objp);
                MA_STACK_PUSH(dst,hp);
                MA_STACK_PUSH(offset,0);
            }

            objp++;

            if (NO_COPY(*objp)) {
                hp[1] = *objp;
#ifdef INCREMENTAL
                if (ptr_within(ptr_val(*objp),inc_fromspc,inc_fromend))
                    INC_STORE(gray,hp,2);
#endif
            }
            else {
                MA_STACK_PUSH(src,*objp);
                MA_STACK_PUSH(dst,hp);
                MA_STACK_PUSH(offset,1);
            }
            continue;
        }

        case TAG_PRIMARY_BOXED: {
            Eterm *objp = boxed_val(obj);

            switch (*objp & _TAG_HEADER_MASK) {
            case ARITYVAL_SUBTAG: {
                Uint ari = arityval(*objp);
                Uint i;
                Eterm *hp;
                GlobalAlloc(from,ari + 1,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_tuple(hp));
                MA_STACK_POP(dst);
                *hp = *objp++;
                for (i = 1; i <= ari; i++) {
                    switch (primary_tag(*objp)) {
                    case TAG_PRIMARY_LIST:
                    case TAG_PRIMARY_BOXED:
                        if (NO_COPY(*objp)) {
                            hp[i] = *objp;
#ifdef INCREMENTAL
                            if (ptr_within(ptr_val(*objp),
                                           inc_fromspc,inc_fromend))
                                INC_STORE(gray,hp,BOXED_NEED(hp,*hp));
#endif
                            objp++;
                        } else {
                            MA_STACK_PUSH(src,*objp++);
                            MA_STACK_PUSH(dst,hp);
                            MA_STACK_PUSH(offset,i);
                        }
                        break;
                    default:
                        hp[i] = *objp++;
                    }
                }
                continue;
            }

            case REFC_BINARY_SUBTAG: {
                ProcBin *pb;
                Uint i = thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,i,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_binary(hp));
                MA_STACK_POP(dst);
                pb = (ProcBin*) hp;
                while (i--) {
                    *hp++ = *objp++;
                }
                erts_refc_inc(&pb->val->refc, 2);
                pb->next = erts_global_offheap.mso;
                erts_global_offheap.mso = pb;
                erts_global_offheap.overhead += pb->size / sizeof(Eterm);
                continue;
            }

            case FUN_SUBTAG: {
                ErlFunThing *funp = (ErlFunThing*) objp;
                Uint i = thing_arityval(*objp) + 1;
                Uint j = i + 1 + funp->num_free;
                Uint k = i;
                Eterm *hp, *hp_start;
                GlobalAlloc(from,j,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                hp_start = hp;
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_fun(hp));
                MA_STACK_POP(dst);
                funp = (ErlFunThing*) hp;
                while (i--) {
                    *hp++ = *objp++;
                }
#ifndef HYBRID // FIND ME!
                funp->next = erts_global_offheap.funs;
                erts_global_offheap.funs = funp;
                erts_refc_inc(&funp->fe->refc, 2);
#endif
                for (i = k; i < j; i++) {
                    switch (primary_tag(*objp)) {
                    case TAG_PRIMARY_LIST:
                    case TAG_PRIMARY_BOXED:
                        if (NO_COPY(*objp)) {
#ifdef INCREMENTAL
                            if (ptr_within(ptr_val(*objp),
                                           inc_fromspc,inc_fromend))
                                INC_STORE(gray,hp,BOXED_NEED(hp,*hp));
#endif
                            *hp++ = *objp++;
                        } else {
                            MA_STACK_PUSH(src,*objp++);
                            MA_STACK_PUSH(dst,hp_start);
                            MA_STACK_PUSH(offset,i);
                            hp++;
                        }
                        break;
                    default:
                        *hp++ = *objp++;
                    }
                }
                continue;
            }

            case EXTERNAL_PID_SUBTAG:
            case EXTERNAL_PORT_SUBTAG:
            case EXTERNAL_REF_SUBTAG: {
                ExternalThing *etp;
                Uint i =  thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,i,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_external(hp));
                MA_STACK_POP(dst);
                etp = (ExternalThing*) hp;
                while (i--)  {
                    *hp++ = *objp++;
                }

                etp->next = erts_global_offheap.externals;
                erts_global_offheap.externals = etp;
		erts_refc_inc(&etp->node->refc, 2);
                continue;
            }

            case SUB_BINARY_SUBTAG: {
                ErlSubBin *sb = (ErlSubBin *) objp;
		Eterm *hp;
		Eterm res_binary;
                Eterm real_bin = sb->orig;
                Uint bit_offset = sb->bitoffs;
		Uint bit_size = sb -> bitsize;
		Uint sub_offset = sb->offs;
                size_t size = sb->size;
		Uint extra_bytes;
		Uint real_size;
		Uint sub_binary_heapneed;
		if ((bit_size + bit_offset) > 8) {
		    extra_bytes = 2;
		    sub_binary_heapneed = ERL_SUB_BIN_SIZE;
		} else if ((bit_size + bit_offset) > 0) {
		    extra_bytes = 1;
		    sub_binary_heapneed = ERL_SUB_BIN_SIZE;
		} else {
		    extra_bytes = 0;
		    sub_binary_heapneed = 0;
		}
		
		real_size = size+extra_bytes;
                objp = binary_val(real_bin);
                if (thing_subtag(*objp) == HEAP_BINARY_SUBTAG) {
                    ErlHeapBin *from_bin;
                    ErlHeapBin *to_bin;
                    Uint i = heap_bin_size(real_size);
                    GlobalAlloc(from,i+sub_binary_heapneed,hp);
                    from_bin = (ErlHeapBin *) objp;
                    to_bin = (ErlHeapBin *) hp;
                    to_bin->thing_word = header_heap_bin(real_size);
                    to_bin->size = real_size;
                    sys_memcpy(to_bin->data, ((byte *)from_bin->data) +
                               sub_offset, real_size);
		    res_binary = make_binary(to_bin);
		    hp += i;
                } else {
                    ProcBin *from_bin;
                    ProcBin *to_bin;
                    
                    ASSERT(thing_subtag(*objp) == REFC_BINARY_SUBTAG);
		    from_bin = (ProcBin *) objp;
		    erts_refc_inc(&from_bin->val->refc, 2);
                    GlobalAlloc(from,PROC_BIN_SIZE+sub_binary_heapneed,hp);
                    to_bin = (ProcBin *) hp;
                    to_bin->thing_word = HEADER_PROC_BIN;
                    to_bin->size = real_size;
                    to_bin->val = from_bin->val;
                    to_bin->bytes = from_bin->bytes + sub_offset;
                    to_bin->next = erts_global_offheap.mso;
                    erts_global_offheap.mso = to_bin;
                    erts_global_offheap.overhead += to_bin->size / sizeof(Eterm);
		    res_binary=make_binary(to_bin);
		    hp += PROC_BIN_SIZE;
                }
		if (extra_bytes != 0) {
		    ErlSubBin* res;
		    res = (ErlSubBin *) hp;
		    res->thing_word = HEADER_SUB_BIN;
		    res->size = size;
		    res->bitsize = bit_size;
		    res->bitoffs = bit_offset;
		    res->offs = 0;
		    res->is_writable = 0;
		    res->orig = res_binary;
		    res_binary = make_binary(hp);
		}
		MA_STACK_UPDATE(dst,MA_STACK_POP(offset),res_binary);
		MA_STACK_POP(dst);
                continue;
            }

	    case BIN_MATCHSTATE_SUBTAG:
		erl_exit(ERTS_ABORT_EXIT,
			 "copy_struct_lazy: matchstate term not allowed");

            default: {
                Uint size = thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,size,hp);
                /* A GC above might invalidate the value of objp */
                objp = boxed_val(obj);
                MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_boxed(hp));
                MA_STACK_POP(dst);
                while (size--) {
                    *hp++ = *objp++;
                }
                continue;
            }
            }
            continue;
        }

        case TAG_PRIMARY_HEADER:
        ASSERT((obj & _TAG_HEADER_MASK) == ARITYVAL_SUBTAG);
        {
            Eterm *objp = &obj;
            Uint ari = arityval(obj);
            Uint i;
            Eterm *hp;
            GlobalAlloc(from,ari + 1,hp);
            MA_STACK_UPDATE(dst,MA_STACK_POP(offset),make_tuple(hp));
            MA_STACK_POP(dst);
            *hp = *objp++;
            for (i = 1; i <= ari; i++) {
                switch (primary_tag(*objp)) {
                case TAG_PRIMARY_LIST:
                case TAG_PRIMARY_BOXED:
                    if (NO_COPY(*objp)) {
#ifdef INCREMENTAL
                        if (ptr_within(ptr_val(*objp),inc_fromspc,inc_fromend))
                            INC_STORE(gray,hp,ari + 1);
#endif
                        hp[i] = *objp++;
                    } else {
                        MA_STACK_PUSH(src,*objp++);
                        MA_STACK_PUSH(dst,hp);
                        MA_STACK_PUSH(offset,i);
                    }
                    break;
                default:
                    hp[i] = *objp++;
                }
            }
            continue;
        }

        default:
            erl_exit(ERTS_ABORT_EXIT,
		     "%s, line %d: Internal error in copy_struct_lazy: 0x%08x\n",
                     __FILE__, __LINE__,obj);
        }
    }

    VERBOSE(DEBUG_MESSAGES,
            ("Copy allocated @ 0x%08lx:\n%T\n",
             (unsigned long)ptr_val(dest),dest));

    ma_gc_flags &= ~GC_CYCLE_START;

    ASSERT(eq(orig, dest));
    ASSERT(ma_src_top == 0);
    ASSERT(ma_dst_top == 0);
    ASSERT(ma_offset_top == 0);
    return dest;
}

#undef NO_COPY
#endif /* HYBRID */

/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 *
 * NOTE: Assumes that term is a tuple (ptr is an untagged tuple ptr).
 */
Eterm
copy_shallow(Eterm* ptr, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
{
    Eterm* tp = ptr;
    Eterm* hp = *hpp;
    Sint offs = hp - tp;

    while (sz--) {
	Eterm val = *tp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    *hp++ = offset_ptr(val, offs);
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb = (ProcBin *) (hp-1);
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    erts_refc_inc(&pb->val->refc, 2);
		    pb->next = off_heap->mso;
		    off_heap->mso = pb;
		    off_heap->overhead += pb->size / sizeof(Eterm);
		}
		break;
	    case FUN_SUBTAG:
		{
#ifndef HYBRID /* FIND ME! */
		    ErlFunThing* funp = (ErlFunThing *) (hp-1);
#endif
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
#ifndef HYBRID /* FIND ME! */
		    funp->next = off_heap->funs;
		    off_heap->funs = funp;
		    erts_refc_inc(&funp->fe->refc, 2);
#endif
		}
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		    ExternalThing* etp = (ExternalThing *) (hp-1);
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    etp->next = off_heap->externals;
		    off_heap->externals = etp;
		    erts_refc_inc(&etp->node->refc, 2);
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
    return make_tuple(ptr + offs); 
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
	move_one_frag(hpp, bp->mem, bp->used_size, off_heap);
	off_heap->overhead += bp->off_heap.overhead;
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
move_one_frag(Eterm** hpp, Eterm* src, Uint src_sz, ErlOffHeap* off_heap)
{
    union {
	Uint *up;
	ProcBin *pbp;
	ErlFunThing *efp;
	ExternalThing *etp;
    } ohe;
    Eterm* ptr = src;
    Eterm* end = ptr + src_sz;
    Eterm dummy_ref;
    Eterm* hp = *hpp;

    while (ptr != end) {
	Eterm val;
	ASSERT(ptr < end);
	val = *ptr;
	ASSERT(val != ERTS_HOLE_MARKER);
	if (is_header(val)) {
	    ASSERT(ptr + header_arity(val) < end);
	    ohe.up = hp;
	    MOVE_BOXED(ptr, val, hp, &dummy_ref);	    
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case REFC_BINARY_SUBTAG:
		ohe.pbp->next = off_heap->mso;
		off_heap->mso = ohe.pbp;
		break;
	    case FUN_SUBTAG:
		ohe.efp->next = off_heap->funs;
		off_heap->funs = ohe.efp;
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		ohe.etp->next = off_heap->externals;
		off_heap->externals = ohe.etp;
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
}

