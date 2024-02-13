/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
#include "erl_global_literals.h"

/* The shared_xyz functions temporarily use the primary tag bits of the header
 * word to store whether the term has been visited/processed before, preventing
 * us from directly comparing the header to constants (e.g. HEADER_BIN_REF) and
 * breaking an assertion in * `thing_subtag`. */
#define shared_thing_subtag(Header) _unchecked_thing_subtag(Header)

static void move_one_frag(Eterm** hpp, ErlHeapFragment*, ErlOffHeap*, int);

/*
 *  Copy object "obj" to process p.
 */
Eterm copy_object_x(Eterm obj, Process* to, Uint extra)
{
    if (!is_immed(obj)) {
        Uint size = size_object(obj);
        Eterm* hp = HAllocX(to, size, extra);
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
            erts_exit(ERTS_ABORT_EXIT, "copy not equal to source\n");
        }
#endif
        return res;
    }
    return obj;
}

/*
 * Return the "flat" size of the object.
 */

#define in_literal_purge_area(PTR)                   \
    (lit_purge_ptr && (                              \
        (lit_purge_ptr <= (PTR) &&                   \
        (PTR) < (lit_purge_ptr + lit_purge_sz))))

Uint size_object_x(Eterm obj, erts_literal_area_t *litopt)
{
    Uint sum = 0;
    Eterm* ptr;
    int arity;
    Eterm *lit_purge_ptr = litopt ? litopt->lit_purge_ptr : NULL;
    Uint   lit_purge_sz  = litopt ? litopt->lit_purge_sz  : 0;
#ifdef DEBUG
    Eterm mypid = erts_get_current_pid();
#endif
    DECLARE_ESTACK(s);
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] size_object %p\n", mypid, obj));

    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
	    ptr = list_val(obj);
            if (litopt && erts_is_literal(obj,ptr) && !in_literal_purge_area(ptr)) {
                goto pop_next;
            }
	    sum += 2;
	    obj = *ptr++;
	    if (!IS_CONST(obj)) {
		ESTACK_PUSH(s, obj);
	    }
	    obj = *ptr;
	    break;
	case TAG_PRIMARY_BOXED:
	    {
		Eterm hdr;
                ptr = boxed_val(obj);
                if (litopt && erts_is_literal(obj,ptr) && !in_literal_purge_area(ptr)) {
                    goto pop_next;
                }
                hdr = *ptr;
		ASSERT(is_header(hdr));
		switch (hdr & _TAG_HEADER_MASK) {
		case ARITYVAL_SUBTAG:
		    arity = header_arity(hdr);
		    if (arity == 0) { /* Empty tuple -- unusual. */
                        ASSERT(!litopt &&
                               erts_is_literal(obj,ptr) &&
                               obj == ERTS_GLOBAL_LIT_EMPTY_TUPLE);
                        /*
                          The empty tuple is always a global literal
                          constant so it does not take up any extra
                          space.
                        */
			goto pop_next;
		    }
                    ptr = tuple_val(obj);
                    sum += arity + 1;
		    while (arity-- > 1) {
			obj = *++ptr;
			if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			}
		    }
		    obj = *++ptr;
		    break;
		case FUN_REF_SUBTAG:
                    sum += ERL_FUN_REF_SIZE;
                    goto pop_next;
                case FUN_SUBTAG:
                    {
                        const ErlFunThing* funp = (ErlFunThing*)fun_val(obj);

                        ASSERT(ERL_FUN_SIZE == (1 + thing_arityval(hdr)));
                        sum += ERL_FUN_SIZE + fun_env_size(funp);

                        for (int i = 1; i < fun_env_size(funp); i++) {
                            obj = funp->env[i];
                            if (!IS_CONST(obj)) {
                                ESTACK_PUSH(s, obj);
                            }
                        }

                        if (fun_env_size(funp) > 0) {
                            obj = funp->env[0];
                            break;
                        }

                        goto pop_next;
		    }
		case MAP_SUBTAG:
		    switch (MAP_HEADER_TYPE(hdr)) {
			case MAP_HEADER_TAG_FLATMAP_HEAD :
                            {
                                Uint n;
                                flatmap_t *mp;
                                mp  = (flatmap_t*)flatmap_val(obj);
                                ptr = (Eterm *)mp;
                                n   = flatmap_get_size(mp) + 1;
                                ASSERT(flatmap_get_size(mp) <= MAP_SMALL_MAP_LIMIT);
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
				head  = hashmap_val(obj);
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
			    erts_exit(ERTS_ABORT_EXIT, "size_object: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
		    }
		    break;
		case SUB_BITS_SUBTAG:
		    {
                        const ErlSubBits *sb = (ErlSubBits*)boxed_val(obj);
                        const Eterm *underlying = boxed_val(sb->orig);

                        if (*underlying == HEADER_BIN_REF) {
                            /* Note that we copy the structure verbatim even if
                             * the size is lower than the off-heap limit as it
                             * was most likely created that way on purpose.
                             *
                             * We also include the size of the attached BinRef
                             * to save us another lap through the main loop. */
                            sum += ERL_REFC_BITS_SIZE;
                        } else {
                            /* When an ErlSubBits is used as a match context it
                             * may refer to an on-heap bitstring instead of a
                             * BinRef, in which case we need to copy the
                             * underlying data instead of the match context. */
                            ASSERT(erl_sub_bits_is_match_context(sb));
                            sum += heap_bits_size(sb->end - sb->start);
                        }

			goto pop_next;
		    }
		    break;
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
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] size was: %u\n", mypid, sum));
		return sum;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT, "size_object: bad tag for %#x\n", obj);
	}
    }
}

/*
 *  Machinery for sharing preserving information
 *  Using a WSTACK but not very transparently; consider refactoring
 */

#define DECLARE_BITSTORE(s)						\
    DECLARE_WSTACK(s);							\
    int WSTK_CONCAT(s,_bitoffs) = 0;					\
    int WSTK_CONCAT(s,_offset) = 0;					\
    UWord WSTK_CONCAT(s,_buffer) = 0

#define DESTROY_BITSTORE(s) DESTROY_WSTACK(s)
#define BITSTORE_PUT(s,i)						\
do {									\
    WSTK_CONCAT(s,_buffer) |= i << WSTK_CONCAT(s,_bitoffs);		\
    WSTK_CONCAT(s,_bitoffs) += 2;					\
    if (WSTK_CONCAT(s,_bitoffs) >= 8*sizeof(UWord)) {			\
	WSTACK_PUSH(s, WSTK_CONCAT(s,_buffer));				\
	WSTK_CONCAT(s,_bitoffs) = 0;					\
	WSTK_CONCAT(s,_buffer) = 0;					\
    }									\
} while(0)
#define BITSTORE_CLOSE(s)						\
do {									\
    if (WSTK_CONCAT(s,_bitoffs) > 0) {					\
	WSTACK_PUSH(s, WSTK_CONCAT(s,_buffer));				\
	WSTK_CONCAT(s,_bitoffs) = 0;					\
    }									\
} while(0)

#define BITSTORE_FETCH(s,dst)                                           \
do {                                                                    \
    UWord result;                                                       \
    if (WSTK_CONCAT(s,_bitoffs) <= 0) {                                 \
        ASSERT(WSTK_CONCAT(s,_offset) < (s.wsp - s.wstart));            \
        WSTK_CONCAT(s,_buffer) = s.wstart[WSTK_CONCAT(s,_offset)];      \
        WSTK_CONCAT(s,_offset)++;                                       \
        WSTK_CONCAT(s,_bitoffs) = 8*sizeof(UWord);                      \
    }                                                                   \
    WSTK_CONCAT(s,_bitoffs) -= 2;                                       \
    result = WSTK_CONCAT(s,_buffer) & 3;                                \
    WSTK_CONCAT(s,_buffer) >>= 2;                                       \
    (dst) = result;                                                     \
} while(0)

#define COUNT_OFF_HEAP (0)

/*
 *  Return the real size of an object and find sharing information
 *  This currently returns the same as erts_debug:size/1.
 *  It is argued whether the size of subterms in constant pools
 *  should be counted or not.
 */

Uint size_shared(Eterm obj)
{
    Eterm saved_obj = obj;
    Uint sum = 0;
    Eterm* ptr;

    DECLARE_EQUEUE(s);
    DECLARE_BITSTORE(b);

    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val(obj);
	    /* we're not counting anything that's outside our heap */
	    if (!COUNT_OFF_HEAP && erts_is_literal(obj,ptr)) {
		goto pop_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it's visited, don't count it */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER ||
		primary_tag(head) == TAG_PRIMARY_HEADER) {
		goto pop_next;
	    }
	    /* else make it visited now */
	    switch (primary_tag(tail)) {
	    case TAG_PRIMARY_LIST:
		ptr[1] = (tail - TAG_PRIMARY_LIST) | TAG_PRIMARY_HEADER;
		break;
	    case TAG_PRIMARY_IMMED1:
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_IMMED1) | primary_tag(head);
		break;
	    case TAG_PRIMARY_BOXED:
		BITSTORE_PUT(b, primary_tag(head));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_BOXED) | TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and count it */
	    sum += 2;
	    if (!IS_CONST(head)) {
		EQUEUE_PUT(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val(obj);
	    /* we're not counting anything that's outside our heap */
	    if (!COUNT_OFF_HEAP && erts_is_literal(obj,ptr)) {
		goto pop_next;
	    }
	    hdr = *ptr;
	    /* if it's visited, don't count it */
	    if (primary_tag(hdr) != TAG_PRIMARY_HEADER) {
		goto pop_next;
	    }
	    /* else make it visited now */
	    *ptr = (hdr - primary_tag(hdr)) + BOXED_VISITED;
	    /* and count it */
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		sum += arity + 1;
		if (arity == 0) { /* Empty tuple -- unusual. */
                    ASSERT(COUNT_OFF_HEAP &&
                           erts_is_literal(obj,ptr) &&
                           obj == ERTS_GLOBAL_LIT_EMPTY_TUPLE);
                    goto pop_next;
		}
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT(s, obj);
		    }
		}
		goto pop_next;
	    }
            case FUN_REF_SUBTAG:
                sum += ERL_FUN_REF_SIZE;
                goto pop_next;
            case FUN_SUBTAG: {
                const ErlFunThing* funp = (ErlFunThing *) ptr;

                ASSERT(ERL_FUN_SIZE == (1 + thing_arityval(hdr)));
                sum += ERL_FUN_SIZE + fun_env_size(funp);

                for (int i = 0; i < fun_env_size(funp); i++) {
                    obj = funp->env[i];
                    if (!IS_CONST(obj)) {
                        EQUEUE_PUT(s, obj);
                    }
                }

                goto pop_next;
            }
            case BIN_REF_SUBTAG: {
                sum += ERL_BIN_REF_SIZE;
                goto pop_next;
            }
            case SUB_BITS_SUBTAG: {
                const ErlSubBits *sb = (ErlSubBits*)ptr;
                const Eterm *underlying = boxed_val(sb->orig);

                if (shared_thing_subtag(*underlying) == BIN_REF_SUBTAG) {
                    EQUEUE_PUT(s, sb->orig);
                    sum += ERL_SUB_BITS_SIZE;
                } else {
                    /* This is a match context referring to a heap bitstring.
                     * As this is fairly rare and sharing multiple instances of
                     * the same match context is rarer still, we'll ignore any
                     * potential sharing here and assume that a blunt copy will
                     * be made. */
                    ASSERT(erl_sub_bits_is_match_context(sb));
                    sum += heap_bits_size(sb->end - sb->start);
                }

                goto pop_next;
            }
            case MAP_SUBTAG:
                switch (MAP_HEADER_TYPE(hdr)) {
                    case MAP_HEADER_TAG_FLATMAP_HEAD : {
                        flatmap_t *mp  = (flatmap_t*)flatmap_val(obj);
                        Uint n = flatmap_get_size(mp) + 1;
                        ASSERT(flatmap_get_size(mp) <= MAP_SMALL_MAP_LIMIT);
                        ptr  = (Eterm *)mp;
                        sum += n + 2;
                        ptr += 2; /* hdr + size words */
                        while (n--) {
                            obj = *ptr++;
                            if (!IS_CONST(obj)) {
                                EQUEUE_PUT(s, obj);
                            }
                        }
                        goto pop_next;
                    }
                    case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
                    case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
                    case MAP_HEADER_TAG_HAMT_NODE_BITMAP : {
                        Uint n = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                        sum += 1 + n + header_arity(hdr);
                        ptr += 1 + header_arity(hdr);
                        while (n--) {
                            obj = *ptr++;
                            if (!IS_CONST(obj)) {
                                EQUEUE_PUT(s, obj);
                            }
                        }
                        goto pop_next;
                    }
                    default:
                        erts_exit(ERTS_ABORT_EXIT, "size_shared: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
                }
	    default:
		sum += thing_arityval(hdr) + 1;
		goto pop_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (EQUEUE_ISEMPTY(s)) {
		goto cleanup;
	    }
	    obj = EQUEUE_GET(s);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
    }

cleanup:
    obj = saved_obj;
    BITSTORE_CLOSE(b);
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val(obj);
	    if (!COUNT_OFF_HEAP && erts_is_literal(obj,ptr)) {
		goto cleanup_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if not already clean, clean it up */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER) {
		if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		    Eterm saved;
                    BITSTORE_FETCH(b, saved);
		    CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | saved;
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) | TAG_PRIMARY_BOXED;
		} else {
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) | TAG_PRIMARY_LIST;
		}
	    } else if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | primary_tag(tail);
		CDR(ptr) = tail = (tail - primary_tag(tail)) | TAG_PRIMARY_IMMED1;
	    } else {
		goto cleanup_next;
	    }
	    /* and its children too */
	    if (!IS_CONST(head)) {
		EQUEUE_PUT_UNCHECKED(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val(obj);
	    if (!COUNT_OFF_HEAP && erts_is_literal(obj,ptr)) {
		goto cleanup_next;
	    }
	    hdr = *ptr;
	    /* if not already clean, clean it up */
	    if (primary_tag(hdr) == TAG_PRIMARY_HEADER) {
		goto cleanup_next;
	    }
	    else {
		ASSERT(primary_tag(hdr) == BOXED_VISITED);
		*ptr = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
	    }
	    /* and its children too */
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		if (arity == 0) { /* Empty tuple -- unusual. */
                    ASSERT(COUNT_OFF_HEAP &&
                           erts_is_literal(obj,ptr) &&
                           obj == ERTS_GLOBAL_LIT_EMPTY_TUPLE);
		    goto cleanup_next;
		}
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT_UNCHECKED(s, obj);
		    }
		}
		goto cleanup_next;
	    }
            case FUN_SUBTAG: {
                const ErlFunThing *funp = (ErlFunThing *) ptr;

                for (int i = 0; i < fun_env_size(funp); i++) {
                    obj = funp->env[i];
                    if (!IS_CONST(obj)) {
                        EQUEUE_PUT_UNCHECKED(s, obj);
                    }
                }
                goto cleanup_next;
            }
            case SUB_BITS_SUBTAG: {
                const ErlSubBits *sb = (ErlSubBits*)ptr;
                const Eterm *underlying = boxed_val(sb->orig);

                /* We only visit BinRefs: see comment above on match
                 * contexts. */
                if (shared_thing_subtag(*underlying) == BIN_REF_SUBTAG) {
                    EQUEUE_PUT_UNCHECKED(s, sb->orig);
                }
                goto cleanup_next;
            }
            case MAP_SUBTAG:
                switch (MAP_HEADER_TYPE(hdr)) {
                    case MAP_HEADER_TAG_FLATMAP_HEAD : {
                        flatmap_t *mp = (flatmap_t *) ptr;
                        Uint n = flatmap_get_size(mp) + 1;
                        ASSERT(flatmap_get_size(mp) <= MAP_SMALL_MAP_LIMIT);
                        ptr += 2; /* hdr + size words */
                        while (n--) {
                            obj = *ptr++;
                            if (!IS_CONST(obj)) {
                                EQUEUE_PUT_UNCHECKED(s, obj);
                            }
                        }
                        goto cleanup_next;
                    }
                    case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
                    case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
                    case MAP_HEADER_TAG_HAMT_NODE_BITMAP : {
                        Uint n = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                        sum += 1 + n + header_arity(hdr);
                        ptr += 1 + header_arity(hdr);
                        while (n--) {
                            obj = *ptr++;
                            if (!IS_CONST(obj)) {
                                EQUEUE_PUT_UNCHECKED(s, obj);
                            }
                        }
                        goto cleanup_next;
                    }
                    default:
                        erts_exit(ERTS_ABORT_EXIT, "size_shared: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
                }
	    default:
		goto cleanup_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	cleanup_next:
	    if (EQUEUE_ISEMPTY(s)) {
		goto all_clean;
	    }
	    obj = EQUEUE_GET(s);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
    }

 all_clean:
    /* Return the result */
    DESTROY_EQUEUE(s);
    DESTROY_BITSTORE(b);
    return sum;
}

/*
 *  Copy a structure to a heap.
 */
Eterm copy_struct_x(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap,
                    Uint *bsz, erts_literal_area_t *litopt
#ifdef ERTS_COPY_REGISTER_LOCATION
                    , char *file, int line
#endif
    )
{
    char* hstart;
    Uint hsize;
    Eterm* htop;
    Eterm* hbot;
    Eterm* hp;
    Eterm* ERTS_RESTRICT objp;
    Eterm* tp;
    Eterm  res;
    Eterm  elem;
    Eterm* tailp;
    Eterm* argp;
    Eterm* const_tuple;
    Eterm hdr;
    Eterm *hend;
    int i;
    Eterm *lit_purge_ptr = litopt ? litopt->lit_purge_ptr : NULL;
    Uint   lit_purge_sz  = litopt ? litopt->lit_purge_sz  : 0;
#ifdef DEBUG
    Eterm org_obj = obj;
    Uint org_sz = sz;
    Eterm mypid = erts_get_current_pid();
#endif

    if (IS_CONST(obj))
	return obj;

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy_struct %p\n", mypid, obj));

    DTRACE1(copy_struct, (int32_t)sz);

    hp = htop = *hpp;
    hbot = hend = htop + sz;
    hstart = (char *)htop;
    hsize = (char*) hbot - hstart;
    const_tuple = 0;

    /* Copy the object onto the heap */
    switch (primary_tag(obj)) {
    case TAG_PRIMARY_LIST:
	argp = &res;
	objp = list_val(obj);
	goto L_copy_list;
    case TAG_PRIMARY_BOXED: argp = &res; goto L_copy_boxed;
    default:
	erts_exit(ERTS_ABORT_EXIT,
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
	    if (ErtsInArea(objp,hstart,hsize)) {
		hp++;
		break;
	    }
	    argp = hp++;
	    /* Fall through */

	L_copy_list:
	    tailp = argp;
            if (litopt && erts_is_literal(obj,objp) && !in_literal_purge_area(objp)) {
                *tailp = obj;
                goto L_copy;
            }
	    for (;;) {
		tp = tailp;
		elem = CAR(objp);
		if (IS_CONST(elem)) {
		    hbot -= 2;
		    CAR(hbot) = elem;
		    tailp = &CDR(hbot);
		} else {
		    CAR(htop) = elem;
		    tailp = &CDR(htop);
		    htop += 2;
		}
		*tp = make_list(tailp - 1);
		obj = CDR(objp);

		if (!is_list(obj)) {
		    break;
		}
		objp = list_val(obj);

                if (litopt && erts_is_literal(obj,objp) && !in_literal_purge_area(objp)) {
                    *tailp = obj;
                    goto L_copy;
                }
	    }
	    switch (primary_tag(obj)) {
	    case TAG_PRIMARY_IMMED1: *tailp = obj; goto L_copy;
	    case TAG_PRIMARY_BOXED: argp = tailp; goto L_copy_boxed;
	    default:
		erts_exit(ERTS_ABORT_EXIT,
			 "%s, line %d: Internal error in copy_struct: 0x%08x\n",
			 __FILE__, __LINE__,obj);
	    }

	case TAG_PRIMARY_BOXED:
	    if (ErtsInArea(boxed_val(obj),hstart,hsize)) {
		hp++;
		break;
	    }
	    argp = hp++;

	L_copy_boxed:
	    objp = boxed_val(obj);
            if (litopt && erts_is_literal(obj,objp) && !in_literal_purge_area(objp)) {
                *argp = obj;
                break;
            }
	    hdr = *objp;
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    int const_flag = 1; /* assume constant tuple */
		    i = arityval(hdr);
                    if (i == 0) {
                        ASSERT(!litopt &&
                               erts_is_literal(obj,objp) &&
                               obj == ERTS_GLOBAL_LIT_EMPTY_TUPLE);
                        *argp = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
                        break;
                    }
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
            case SUB_BITS_SUBTAG:
                {
                    ErlSubBits *from_sb = (ErlSubBits*)objp;
                    Eterm *underlying = boxed_val(from_sb->orig);

                    if (*underlying == HEADER_BIN_REF) {
                        BinRef *from_br, *to_br;
                        ErlSubBits *to_sb;

                        from_br = (BinRef*)underlying;

                        /* As BinRefs are only reachable through ErlSubBits and
                         * we aren't doing a shared copy, and certain functions
                         * like copy_ets_element assume that outer objects
                         * appear before inner ones on the heap, we'll handle
                         * them together here. */
                        hbot -= ERL_BIN_REF_SIZE;
                        to_br = (BinRef*)hbot;
                        hbot -= ERL_SUB_BITS_SIZE;
                        to_sb = (ErlSubBits*)hbot;

                        ASSERT(from_br->thing_word == HEADER_BIN_REF);
                        erts_pin_writable_binary(from_sb, from_br);

                        erts_refc_inc(&(from_br->val)->intern.refc, 2);

                        *to_sb = *from_sb;
                        *to_br = *from_br;

                        ASSERT(erl_sub_bits_is_normal(to_sb));
                        to_sb->orig = make_boxed((Eterm*)to_br);

                        to_br->next = off_heap->first;
                        off_heap->first = (struct erl_off_heap_header*)to_br;
                        ERTS_BR_OVERHEAD(off_heap, to_br);

                        *argp = make_bitstring(to_sb);
                    } else {
                        /* When an ErlSubBits is used as a match context it may
                         * refer to an on-heap bitstring instead of a BinRef,
                         * in which case we need to copy the underlying data
                         * instead of the match context. */
                        Uint size = from_sb->end - from_sb->start;

                        ASSERT(erl_sub_bits_is_match_context(from_sb));

                        hbot -= heap_bits_size(size);
                        *argp = HEAP_BITSTRING(hbot,
                                               erl_sub_bits_get_base(from_sb),
                                               from_sb->start,
                                               size);
                    }
                }
                break;
            case FUN_REF_SUBTAG:
                {
                    const FunRef *src_ref = (const FunRef *)objp;
                    FunRef *dst_ref;

                    hbot -= ERL_FUN_REF_SIZE;
                    dst_ref = (FunRef *)hbot;

                    dst_ref->thing_word = HEADER_FUN_REF;
                    dst_ref->entry = src_ref->entry;

                    dst_ref->next = off_heap->first;
                    off_heap->first = (struct erl_off_heap_header*)dst_ref;

                    /* All fun entries are NULL during module loading, before
                     * the code is finalized.
                     *
                     * Strictly speaking it would be nice to crash when we see
                     * this outside of loading, but it's too complicated to
                     * keep track of whether we are. */
                    if (dst_ref->entry != NULL) {
                        erts_refc_inc(&(dst_ref->entry)->refc, 2);
                    }

                    *argp = make_boxed((Eterm*)dst_ref);
                }
                break;
            case FUN_SUBTAG:
                {
                    const ErlFunThing *src_fun = (const ErlFunThing *)objp;
                    ErlFunThing *dst_fun = (ErlFunThing *)htop;

                    *dst_fun = *src_fun;

                    for (int i = 0; i < fun_env_size(dst_fun); i++) {
                        dst_fun->env[i] = src_fun->env[i];
                    }

                    ASSERT(&htop[ERL_FUN_SIZE] == &dst_fun->env[0]);
                    htop = &dst_fun->env[fun_env_size(dst_fun)];
                    *argp = make_fun(dst_fun);
                }
                break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		  ExternalThing *etp = (ExternalThing *) objp;
#if defined(ERTS_COPY_REGISTER_LOCATION) && defined(ERL_NODE_BOOKKEEP)
		  erts_ref_node_entry__(etp->node, 2, make_boxed(htop), file, line);
#else
		  erts_ref_node_entry(etp->node, 2, make_boxed(htop));
#endif
		}
	    L_off_heap_node_container_common:
		{
		  struct erl_off_heap_header *ohhp;
		  ohhp = (struct erl_off_heap_header *) htop;
		  i  = thing_arityval(hdr) + 1;
		  *argp = make_boxed(htop);
		  tp = htop;

		  while (i--)  {
		    *htop++ = *objp++;
		  }

		  ohhp->next = off_heap->first;
		  off_heap->first = ohhp;

		}
		break;
	    case MAP_SUBTAG:
		tp = htop;
		switch (MAP_HEADER_TYPE(hdr)) {
		    case MAP_HEADER_TAG_FLATMAP_HEAD :
                        i = flatmap_get_size(objp) + 3;
                        ASSERT(flatmap_get_size(objp) <= MAP_SMALL_MAP_LIMIT);
                        *argp = make_flatmap(htop);
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
			*argp = make_hashmap(tp);
			break;
		    default:
			erts_exit(ERTS_ABORT_EXIT, "copy_struct: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
		}
		break;
	    case REF_SUBTAG:
		if (is_magic_ref_thing(objp)) {
		    ErtsMRefThing *mreft = (ErtsMRefThing *) objp;
		    erts_refc_inc(&mreft->mb->intern.refc, 2);
		    goto L_off_heap_node_container_common;
		}
		/* Fall through... */
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

    if (bsz) {
        *hpp = htop;
        *bsz = hend - hbot;
    } else {
#ifdef DEBUG
        if (!eq(org_obj, res)) {
            erts_exit(ERTS_ABORT_EXIT,
                    "Internal error in copy_struct() when copying %T:"
                    " not equal to copy %T\n",
                    org_obj, res);
        }
        if (htop != hbot) {
            erts_exit(ERTS_ABORT_EXIT,
                    "Internal error in copy_struct() when copying %T:"
                    " htop=%p != hbot=%p (sz=%beu)\n",
                    org_obj, htop, hbot, org_sz);
        }
#else
        if (htop > hbot) {
            erts_exit(ERTS_ABORT_EXIT,
                    "Internal error in copy_struct(): htop, hbot overrun\n");
        }
#endif
        *hpp = (Eterm *) (hstart+hsize);
    }
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] result is at %p\n", mypid, res));
    return res;
}


/*
 *  Machinery for the table used by the sharing preserving copier
 *  Using an ESTACK but not very transparently; consider refactoring
 */

#define DECLARE_SHTABLE(s)					\
    DECLARE_ESTACK(s);						\
    Uint ESTK_CONCAT(s,_offset) = 0
#define DESTROY_SHTABLE(s) DESTROY_ESTACK(s)
#define SHTABLE_INCR 4
#define SHTABLE_NEXT(s)	ESTK_CONCAT(s,_offset)
#define SHTABLE_PUSH(s,x,y,b)					\
do {								\
    if (s.sp > s.end - SHTABLE_INCR) {				\
        erl_grow_estack(&(s), SHTABLE_INCR);	       		\
    }								\
    *s.sp++ = (x);						\
    *s.sp++ = (y);						\
    *s.sp++ = (Eterm) NULL;					\
    *s.sp++ = (Eterm) (b);		                	\
    ESTK_CONCAT(s,_offset) += SHTABLE_INCR;			\
} while(0)
#define SHTABLE_X(s,e) (s.start[e])
#define SHTABLE_Y(s,e) (s.start[(e)+1])
#define SHTABLE_FWD(s,e) ((Eterm *) (s.start[(e)+2]))
#define SHTABLE_FWD_UPD(s,e,p) (s.start[(e)+2] = (Eterm) (p))
#define SHTABLE_REV(s,e) ((Eterm *) (s.start[(e)+3]))

#define LIST_SHARED_UNPROCESSED ((Eterm) 0)
#define LIST_SHARED_PROCESSED	((Eterm) 1)

#define HEAP_ELEM_TO_BE_FILLED	_unchecked_make_list(NULL)


/*
 *  Specialized macros for using/reusing the persistent state
 */

#define DECLARE_EQUEUE_INIT_INFO(q, info)		\
    UWord* EQUE_DEF_QUEUE(q) = info->queue_default;	\
    ErtsEQueue q = {					\
        EQUE_DEF_QUEUE(q), /* start */			\
        EQUE_DEF_QUEUE(q), /* front */			\
        EQUE_DEF_QUEUE(q), /* back */			\
        1,                 /* possibly_empty */		\
        EQUE_DEF_QUEUE(q) + DEF_EQUEUE_SIZE, /* end */	\
        ERTS_ALC_T_ESTACK  /* alloc_type */		\
    }

#define DECLARE_EQUEUE_FROM_INFO(q, info)		\
    /* no EQUE_DEF_QUEUE(q), read-only */		\
    ErtsEQueue q = {					\
        info->queue_start,      /* start */		\
        info->queue_start,      /* front */		\
        info->queue_start,      /* back */		\
        1,                      /* possibly_empty */	\
        info->queue_end,        /* end */		\
        info->queue_alloc_type  /* alloc_type */	\
    }

#define DECLARE_BITSTORE_INIT_INFO(s, info)		\
    UWord* WSTK_DEF_STACK(s) = info->bitstore_default;	\
    ErtsWStack s = {					\
        WSTK_DEF_STACK(s),  /* wstart */		\
        WSTK_DEF_STACK(s),  /* wsp */			\
        WSTK_DEF_STACK(s) + DEF_WSTACK_SIZE, /* wend */	\
        WSTK_DEF_STACK(s),  /* wdflt */ 		\
        ERTS_ALC_T_ESTACK /* alloc_type */		\
    };							\
    int WSTK_CONCAT(s,_bitoffs) = 0;			\
    /* no WSTK_CONCAT(s,_offset), write-only */		\
    UWord WSTK_CONCAT(s,_buffer) = 0

#ifdef DEBUG
# define DEBUG_COND(D,E) D
#else
# define DEBUG_COND(D,E) E
#endif

#define DECLARE_BITSTORE_FROM_INFO(s, info)		\
    /* no WSTK_DEF_STACK(s), read-only */		\
    ErtsWStack s = {					\
        info->bitstore_start,  /* wstart */		\
        DEBUG_COND(info->bitstore_stop, NULL), /* wsp,  read-only */ \
        NULL,                  /* wend, read-only */	\
        NULL,                  /* wdef, read-only */	\
        info->bitstore_alloc_type /* alloc_type */	\
    };							\
    int WSTK_CONCAT(s,_bitoffs) = 0;			\
    int WSTK_CONCAT(s,_offset) = 0;			\
    UWord WSTK_CONCAT(s,_buffer) = 0

#define DECLARE_SHTABLE_INIT_INFO(s, info)		\
    Eterm* ESTK_DEF_STACK(s) = info->shtable_default;	\
    ErtsEStack s = {					\
        ESTK_DEF_STACK(s),  /* start */			\
        ESTK_DEF_STACK(s),  /* sp */			\
        ESTK_DEF_STACK(s) + DEF_ESTACK_SIZE, /* end */	\
        ESTK_DEF_STACK(s),  /* default */		\
        ERTS_ALC_T_ESTACK /* alloc_type */		\
    };							\
    Uint ESTK_CONCAT(s,_offset) = 0

#define DECLARE_SHTABLE_FROM_INFO(s, info)		\
    /* no ESTK_DEF_STACK(s), read-only */		\
    ErtsEStack s = {					\
        info->shtable_start,     /* start */		\
        NULL,                    /* sp,  read-only */	\
        NULL,                    /* end, read-only */	\
        NULL,                    /* def, read-only */	\
        info->shtable_alloc_type /* alloc_type */	\
    };							\
    /* no ESTK_CONCAT(s,_offset), read-only */

/*
 *  Copy object "obj" preserving sharing.
 *  First half: count size and calculate sharing.
 */
Uint copy_shared_calculate(Eterm obj, erts_shcopy_t *info)
{
    Uint sum;
    Uint e;
    Eterm* ptr;
    Eterm *lit_purge_ptr = info->lit_purge_ptr;
    Uint lit_purge_sz = info->lit_purge_sz;
    int copy_literals = info->copy_literals;
#ifdef DEBUG
    Eterm mypid = erts_get_current_pid();
#endif
    const Eterm empty_tuple_literal =
        ERTS_GLOBAL_LIT_EMPTY_TUPLE;
    
    DECLARE_EQUEUE_INIT_INFO(s, info);
    DECLARE_BITSTORE_INIT_INFO(b, info);
    DECLARE_SHTABLE_INIT_INFO(t, info);

    /* step #0:
       -------------------------------------------------------
       get rid of the easy cases first:
       - copying constants
       - if not a proper process, do flat copy
    */

    if (IS_CONST(obj))
	return 0;

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy_shared_calculate %p\n", mypid, obj));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] message is %T\n", mypid, obj));

    /* step #1:
       -------------------------------------------------------
       traverse the term and calculate the size;
       when traversing, transform as you do in size_shared
       but when you find shared objects:

       a. add entry in the table, indexed by i
       b. mark them:
	  b1. boxed terms, set header to (i | 11)
	      store (old header, NONV, NULL, backptr) in the entry
	  b2. cons cells, set CDR to NONV, set CAR to i
	      store (old CAR, old CDR, NULL, backptr) in the entry
    */

    sum = 0;

    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val(obj);
	    /* off heap list pointers are copied verbatim */
	    if (erts_is_literal(obj,ptr)) {
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] bypassed copying %p is %T\n", mypid, ptr, obj));
                if (copy_literals || in_literal_purge_area(ptr))
                    info->literal_size += size_object(obj);
		goto pop_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it's visited, don't count it;
	       if not already shared, make it shared and store it in the table */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER ||
		primary_tag(head) == TAG_PRIMARY_HEADER) {
		if (tail != THE_NON_VALUE) {
		    e = SHTABLE_NEXT(t);
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabling L %p\n", mypid, ptr));
		    SHTABLE_PUSH(t, head, tail, ptr);
		    CAR(ptr) = (e << _TAG_PRIMARY_SIZE) | LIST_SHARED_UNPROCESSED;
		    CDR(ptr) = THE_NON_VALUE;
		}
		goto pop_next;
	    }
	    /* else make it visited now */
	    switch (primary_tag(tail)) {
	    case TAG_PRIMARY_LIST:
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling L/L %p\n", mypid, ptr));
		CDR(ptr) = (tail - TAG_PRIMARY_LIST) | TAG_PRIMARY_HEADER;
		break;
	    case TAG_PRIMARY_IMMED1:
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling L/I %p\n", mypid, ptr));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_IMMED1) | primary_tag(head);
		break;
	    case TAG_PRIMARY_BOXED:
		BITSTORE_PUT(b, primary_tag(head));
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling L/B %p\n", mypid, ptr));
		CAR(ptr) = (head - primary_tag(head)) | TAG_PRIMARY_HEADER;
		CDR(ptr) = (tail - TAG_PRIMARY_BOXED) | TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and count it */
	    sum += 2;
	    if (!IS_CONST(head)) {
		EQUEUE_PUT(s, head);
	    }
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val(obj);
	    /* off heap pointers to boxes (except pointers to the
               empty tuple) are copied verbatim */
	    if (erts_is_literal(obj,ptr)) {
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] bypassed copying %p is %T\n", mypid, ptr, obj));
                if (obj != empty_tuple_literal &&
                    (copy_literals || in_literal_purge_area(ptr)))
                    info->literal_size += size_object(obj);
		goto pop_next;
	    }
	    hdr = *ptr;
	    /* if it's visited, don't count it;
	       if not already shared, make it shared and store it in the table */
	    if (primary_tag(hdr) != TAG_PRIMARY_HEADER) {
		if (primary_tag(hdr) == BOXED_VISITED) {
		    e = SHTABLE_NEXT(t);
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabling B %p\n", mypid, ptr));
		    SHTABLE_PUSH(t, hdr, THE_NON_VALUE, ptr);
		    *ptr = (e << _TAG_PRIMARY_SIZE) | BOXED_SHARED_UNPROCESSED;
		}
		goto pop_next;
	    }
	    /* else make it visited now */
	    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] mangling B %p\n", mypid, ptr));
	    *ptr = (hdr - primary_tag(hdr)) + BOXED_VISITED;
	    /* and count it */
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
                /* arity cannot be 0 as the empty tuple is always a
                   global constant literal which is handled above */
                ASSERT(arity != 0);
		sum += arity + 1;
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (!IS_CONST(obj)) {
			EQUEUE_PUT(s, obj);
		    }
		}
		goto pop_next;
	    }
	    case FUN_SUBTAG: {
                const ErlFunThing* funp = (ErlFunThing *) ptr;

                ASSERT(ERL_FUN_SIZE == (1 + thing_arityval(hdr)));
                sum += ERL_FUN_SIZE + fun_env_size(funp);

                for (int i = 0; i < fun_env_size(funp); i++) {
                    obj = funp->env[i];
                    if (!IS_CONST(obj)) {
                        EQUEUE_PUT(s, obj);
                    }
                }

                goto pop_next;
            }
            case BIN_REF_SUBTAG: {
                sum += ERL_BIN_REF_SIZE;
                goto pop_next;
            }
            case SUB_BITS_SUBTAG: {
                ErlSubBits *sb = (ErlSubBits*)ptr;
                Eterm *underlying = boxed_val(sb->orig);
                Eterm orig_hdr = *underlying;

                switch (primary_tag(orig_hdr)) {
                case BOXED_SHARED_UNPROCESSED:
                case BOXED_SHARED_PROCESSED:
                    orig_hdr = SHTABLE_X(t, orig_hdr >> _TAG_PRIMARY_SIZE);
                    break;
                }

                if (shared_thing_subtag(orig_hdr) == BIN_REF_SUBTAG) {
                    erts_pin_writable_binary(sb, (BinRef*)underlying);

                    EQUEUE_PUT(s, sb->orig);
                    sum += ERL_SUB_BITS_SIZE;
                } else {
                    /* This is a match context referring to an on-heap
                     * bitstring.
                     *
                     * As this is fairly rare and sharing multiple instances of
                     * the same match context is rarer still, we'll make a
                     * blunt copy that ignores sharing. */
                    ASSERT(erl_sub_bits_is_match_context(sb));
                    sum += heap_bits_size(sb->end - sb->start);
                }

                goto pop_next;
            }
            case MAP_SUBTAG:
                switch (MAP_HEADER_TYPE(hdr)) {
                    case MAP_HEADER_TAG_FLATMAP_HEAD : {
                        flatmap_t *mp = (flatmap_t *) ptr;
                        Uint n = flatmap_get_size(mp) + 1;
                        ASSERT(flatmap_get_size(mp) <= MAP_SMALL_MAP_LIMIT);
                        sum += n + 2;
                        ptr += 2; /* hdr + size words */
                        while (n--) {
                            obj = *ptr++;
                            if (!IS_CONST(obj)) {
                                EQUEUE_PUT(s, obj);
                            }
                        }
                        goto pop_next;
                    }
                    case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
                    case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
                    case MAP_HEADER_TAG_HAMT_NODE_BITMAP : {
                        Uint n = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                        sum += 1 + n + header_arity(hdr);
                        ptr += 1 + header_arity(hdr);

                        if (n == 0) {
                            goto pop_next;
                        }
                        while(n--) {
                            obj = *ptr++;
                            if (!IS_CONST(obj)) {
                                EQUEUE_PUT(s, obj);
                            }
                        }
                        goto pop_next;
                    }
                    default:
                        erts_exit(ERTS_ABORT_EXIT, "copy_shared_calculate: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
                }
	    default:
		sum += thing_arityval(hdr) + 1;
		goto pop_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	pop_next:
	    if (EQUEUE_ISEMPTY(s)) {
                /* add sentinel to the table */
                SHTABLE_PUSH(t, THE_NON_VALUE, THE_NON_VALUE, NULL);
                /* store persistent info */
                BITSTORE_CLOSE(b);
                info->queue_start = s.start;
                info->queue_end = s.end;
                info->queue_alloc_type = s.alloc_type;
                info->bitstore_start = b.wstart;
#ifdef DEBUG
                info->bitstore_stop = b.wsp;
#endif
                info->bitstore_alloc_type = b.alloc_type;
                info->shtable_start = t.start;
                info->shtable_alloc_type = t.alloc_type;
                /* single point of return: the size of the object */
                VERBOSE(DEBUG_SHCOPY, ("[pid=%T] size was: %u\n", mypid, sum));
                return sum + info->literal_size;
	    }
	    obj = EQUEUE_GET(s);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT, "[pid=%T] size_shared: bad tag for %#x\n", obj);
	}
    }
}

/*
 *  Copy object "obj" preserving sharing.
 *  Second half: copy and restore the object.
 */
Uint copy_shared_perform_x(Eterm obj, Uint size, erts_shcopy_t *info,
                           Eterm** hpp, ErlOffHeap* off_heap
#ifdef ERTS_COPY_REGISTER_LOCATION
                           , char *file, int line
#endif
    )
{
    Uint e;
    unsigned sz;
    Eterm* ptr;
    Eterm* hp;
    Eterm* hscan;
    Eterm result;
    Eterm* resp;
    Eterm *hbot, *hend;
    unsigned remaining;
    Eterm *lit_purge_ptr = info->lit_purge_ptr;
    Uint lit_purge_sz = info->lit_purge_sz;
    int copy_literals = info->copy_literals;
#ifdef DEBUG
    Eterm mypid = erts_get_current_pid();
    Eterm saved_obj = obj;
#endif
    const Eterm empty_tuple_literal =
        ERTS_GLOBAL_LIT_EMPTY_TUPLE;
    DECLARE_EQUEUE_FROM_INFO(s, info);
    DECLARE_BITSTORE_FROM_INFO(b, info);
    DECLARE_SHTABLE_FROM_INFO(t, info);

    /* step #0:
       -------------------------------------------------------
       get rid of the easy cases first:
       - copying constants
       - if not a proper process, do flat copy
    */

    if (IS_CONST(obj))
	return obj;

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy_shared_perform %p\n", mypid, obj));

    /* step #2: was performed before this function was called
       -------------------------------------------------------
       allocate new space
    */

    hscan = hp = *hpp;
    hbot  = hend = hp + size;

    /* step #3:
       -------------------------------------------------------
       traverse the term a second time and when traversing:
       a. if the object is marked as shared
	  a1. if the entry contains a forwarding ptr, use that
	  a2. otherwise, copy it to the new space and store the
	      forwarding ptr to the entry
      b. otherwise, reverse-transform as you do in size_shared
	 and copy to the new space
    */

    resp = &result;
    remaining = 0;
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST: {
	    Eterm head, tail;
	    ptr = list_val(obj);
	    /* off heap list pointers are copied verbatim */
	    if (erts_is_literal(obj,ptr)) {
                if (!(copy_literals || in_literal_purge_area(ptr))) {
                    *resp = obj;
                } else {
                    Uint bsz = 0;
                    *resp = copy_struct_x(obj, hbot - hp, &hp, off_heap, &bsz, NULL
#ifdef ERTS_COPY_REGISTER_LOCATION
                                          , file, line
#endif
                        ); /* copy literal */
                    hbot -= bsz;
                }
		goto cleanup_next;
	    }
	    head = CAR(ptr);
	    tail = CDR(ptr);
	    /* if it is shared */
	    if (tail == THE_NON_VALUE) {
		e = head >> _TAG_PRIMARY_SIZE;
		/* if it has been processed, just use the forwarding pointer */
		if (primary_tag(head) == LIST_SHARED_PROCESSED) {
		    *resp = make_list(SHTABLE_FWD(t, e));
		    goto cleanup_next;
		}
		/* else, let's process it now,
		   copy it and keep the forwarding pointer */
		else {
		    CAR(ptr) = (head - primary_tag(head)) + LIST_SHARED_PROCESSED;
		    head = SHTABLE_X(t, e);
		    tail = SHTABLE_Y(t, e);
		    ptr = &(SHTABLE_X(t, e));
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabled L %p is %p\n", mypid, ptr, SHTABLE_REV(t, e)));
		    SHTABLE_FWD_UPD(t, e, hp);
		}
	    }
	    /* if not already clean, clean it up and copy it */
	    if (primary_tag(tail) == TAG_PRIMARY_HEADER) {
		if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		    Eterm saved;
                    BITSTORE_FETCH(b, saved);
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling L/B %p\n", mypid, ptr));
		    CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) + saved;
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) + TAG_PRIMARY_BOXED;
		} else {
		    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling L/L %p\n", mypid, ptr));
		    CDR(ptr) = tail = (tail - TAG_PRIMARY_HEADER) + TAG_PRIMARY_LIST;
		}
	    } else if (primary_tag(head) == TAG_PRIMARY_HEADER) {
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling L/I %p\n", mypid, ptr));
		CAR(ptr) = head = (head - TAG_PRIMARY_HEADER) | primary_tag(tail);
		CDR(ptr) = tail = (tail - primary_tag(tail)) | TAG_PRIMARY_IMMED1;
	    } else {
		ASSERT(0 && "cannot come here");
		goto cleanup_next;
	    }
	    /* and its children too */
	    if (IS_CONST(head)) {
		CAR(hp) = head;
	    } else {
		EQUEUE_PUT_UNCHECKED(s, head);
		CAR(hp) = HEAP_ELEM_TO_BE_FILLED;
	    }
	    *resp = make_list(hp);
	    resp = &(CDR(hp));
	    hp += 2;
	    obj = tail;
	    break;
	}
	case TAG_PRIMARY_BOXED: {
	    Eterm hdr;
	    ptr = boxed_val(obj);
	    /* off heap pointers to boxes are copied verbatim */
	    if (erts_is_literal(obj,ptr)) {
                if (obj == empty_tuple_literal ||
                    !(copy_literals || in_literal_purge_area(ptr))) {
                    *resp = obj;
                } else {
                    Uint bsz = 0;
                    *resp = copy_struct_x(obj, hbot - hp, &hp, off_heap, &bsz, NULL
#ifdef ERTS_COPY_REGISTER_LOCATION
                                          , file, line
#endif
                        ); /* copy literal */
                    hbot -= bsz;
                }
		goto cleanup_next;
	    }
	    hdr = *ptr;
	    /* clean it up, unless it's already clean or shared and processed */
	    switch (primary_tag(hdr)) {
	    case TAG_PRIMARY_HEADER:
		ASSERT(0 && "cannot come here");
	    /* if it is shared and has been processed,
	       just use the forwarding pointer */
	    case BOXED_SHARED_PROCESSED:
		e = hdr >> _TAG_PRIMARY_SIZE;
		*resp = make_boxed(SHTABLE_FWD(t, e));
		goto cleanup_next;
	    /* if it is shared but has not been processed yet, let's process
	       it now: copy it and keep the forwarding pointer */
	    case BOXED_SHARED_UNPROCESSED:
		e = hdr >> _TAG_PRIMARY_SIZE;
		*ptr = (hdr - primary_tag(hdr)) + BOXED_SHARED_PROCESSED;
		hdr = SHTABLE_X(t, e);
		ASSERT(primary_tag(hdr) == BOXED_VISITED);
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] tabled B %p is %p\n", mypid, ptr, SHTABLE_REV(t, e)));
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling B %p\n", mypid, ptr));
		SHTABLE_X(t, e) = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
		SHTABLE_FWD_UPD(t, e, hp);
		break;
	    case BOXED_VISITED:
		VERBOSE(DEBUG_SHCOPY, ("[pid=%T] unmangling B %p\n", mypid, ptr));
		*ptr = hdr = (hdr - BOXED_VISITED) + TAG_PRIMARY_HEADER;
		break;
	    }
	    /* and its children too */
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG: {
		int arity = header_arity(hdr);
		*resp = make_boxed(hp);
		*hp++ = hdr;
		while (arity-- > 0) {
		    obj = *++ptr;
		    if (IS_CONST(obj)) {
			*hp++ = obj;
		    } else {
			EQUEUE_PUT_UNCHECKED(s, obj);
			*hp++ = HEAP_ELEM_TO_BE_FILLED;
		    }
		}
		goto cleanup_next;
	    }
            case FUN_REF_SUBTAG:
                {
                    const FunRef *src_ref = (const FunRef *)ptr;
                    FunRef *dst_ref = (FunRef *)hp;

                    dst_ref->thing_word = HEADER_FUN_REF;
                    dst_ref->entry = src_ref->entry;

                    dst_ref->next = off_heap->first;
                    off_heap->first = (struct erl_off_heap_header*)dst_ref;
                    erts_refc_inc(&(dst_ref->entry)->refc, 2);

                    *resp = make_boxed((Eterm*)dst_ref);
                    hp += ERL_FUN_REF_SIZE;
                }
                goto cleanup_next;
            case FUN_SUBTAG: {
                const ErlFunThing *src_fun = (const ErlFunThing *)ptr;
                ErlFunThing *dst_fun = (ErlFunThing *)hp;

                *dst_fun = *src_fun;

                /* The header of the source fun may have been clobbered,
                 * restore it. */
                dst_fun->thing_word = hdr;

                for (int i = 0; i < fun_env_size(dst_fun); i++) {
                    obj = src_fun->env[i];

                    if (!IS_CONST(obj)) {
                        EQUEUE_PUT_UNCHECKED(s, obj);
                        obj = HEAP_ELEM_TO_BE_FILLED;
                    }

                    dst_fun->env[i] = obj;
                }

                ASSERT(&hp[ERL_FUN_SIZE] == &dst_fun->env[0]);
                hp = &dst_fun->env[fun_env_size(dst_fun)];
                *resp = make_fun(dst_fun);

                goto cleanup_next;
            }
            case MAP_SUBTAG:
                *resp  = make_flatmap(hp);
                *hp++  = hdr;
                switch (MAP_HEADER_TYPE(hdr)) {
                    case MAP_HEADER_TAG_FLATMAP_HEAD : {
                        flatmap_t *mp = (flatmap_t *) ptr;
                        Uint n = flatmap_get_size(mp) + 1;
                        ASSERT(flatmap_get_size(mp) <= MAP_SMALL_MAP_LIMIT);
                        *hp++  = *++ptr; /* keys */
                        while (n--) {
                            obj = *++ptr;
                            if (IS_CONST(obj)) {
                                *hp++ = obj;
                            } else {
                                EQUEUE_PUT_UNCHECKED(s, obj);
                                *hp++ = HEAP_ELEM_TO_BE_FILLED;
                            }
                        }
                        goto cleanup_next;
                    }
                    case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
                    case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
			*hp++ = *++ptr; /* total map size */
                    case MAP_HEADER_TAG_HAMT_NODE_BITMAP : {
                         Uint n = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                         while (n--)  {
                             obj = *++ptr;
                             if (IS_CONST(obj)) {
                                 *hp++ = obj;
                             } else {
                                 EQUEUE_PUT_UNCHECKED(s, obj);
                                 *hp++ = HEAP_ELEM_TO_BE_FILLED;
                             }
                         }
                        goto cleanup_next;
                    }
                    default:
                        erts_exit(ERTS_ABORT_EXIT, "copy_shared_perform: bad hashmap type %d\n", MAP_HEADER_TYPE(hdr));
                }
            case BIN_REF_SUBTAG: {
                const BinRef *from_br;
                BinRef *to_br;

                from_br = (BinRef*)ptr;
                to_br = (BinRef*)hp;

                *to_br = *from_br;
                to_br->thing_word = hdr;

                /* Note that we don't need to pin the binary as that was done
                 * earlier during copy_shared_calculate */
                ASSERT(!((from_br->val)->intern.flags &
                         (BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)));
                erts_refc_inc(&(from_br->val)->intern.refc, 2);

                to_br->next = off_heap->first;
                off_heap->first = (struct erl_off_heap_header*)to_br;
                ERTS_BR_OVERHEAD(off_heap, to_br);

                *resp = make_boxed((Eterm*)to_br);
                hp += ERL_BIN_REF_SIZE;

                goto cleanup_next;
            }
            case SUB_BITS_SUBTAG: {
                const ErlSubBits *from_sb = (ErlSubBits*)ptr;
                const Eterm *underlying = boxed_val(from_sb->orig);
                Eterm orig_hdr = *underlying;

                switch (primary_tag(orig_hdr)) {
                case BOXED_SHARED_UNPROCESSED:
                case BOXED_SHARED_PROCESSED:
                    orig_hdr = SHTABLE_X(t, orig_hdr >> _TAG_PRIMARY_SIZE);
                    break;
                }

                if (shared_thing_subtag(orig_hdr) == BIN_REF_SUBTAG) {
                    ErlSubBits *to_sb;

                    to_sb = (ErlSubBits*)hp;

                    EQUEUE_PUT_UNCHECKED(s, from_sb->orig);

                    *to_sb = *from_sb;
                    to_sb->thing_word = hdr;
                    to_sb->orig = HEAP_ELEM_TO_BE_FILLED;

                    *resp = make_bitstring(to_sb);
                    hp += ERL_SUB_BITS_SIZE;
                } else {
                    /* This is a match context referring to an on-heap
                     * bitstring.
                     *
                     * As this is fairly rare and sharing multiple instances of
                     * the same match context is rarer still, we'll make a
                     * blunt copy that ignores sharing. */
                    Uint size = from_sb->end - from_sb->start;
                    ASSERT(erl_sub_bits_is_match_context(from_sb));

                    hbot -= heap_bits_size(size);
                    *resp = HEAP_BITSTRING(hbot,
                                           erl_sub_bits_get_base(from_sb),
                                           from_sb->start,
                                           size);
                }

                goto cleanup_next;
            }
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
	    {
		ExternalThing *etp = (ExternalThing *) ptr;
                
#if defined(ERTS_COPY_REGISTER_LOCATION) && defined(ERL_NODE_BOOKKEEP)
                erts_ref_node_entry__(etp->node, 2, make_boxed(hp), file, line);
#else
                erts_ref_node_entry(etp->node, 2, make_boxed(hp));
#endif
	    }
	  off_heap_node_container_common:
	    {
		struct erl_off_heap_header *ohhp;
		ohhp = (struct erl_off_heap_header *) hp;
		sz = thing_arityval(hdr);
		*resp = make_boxed(hp);
		*hp++ = hdr;
		ptr++;
		while (sz-- > 0) {
		    *hp++ = *ptr++;
		}
		ohhp->next = off_heap->first;
		off_heap->first = ohhp;
		goto cleanup_next;
	    }
	    case REF_SUBTAG:
		if (is_magic_ref_thing_with_hdr(ptr,hdr)) {
		    ErtsMRefThing *mreft = (ErtsMRefThing *) ptr;
		    erts_refc_inc(&mreft->mb->intern.refc, 2);
		    goto off_heap_node_container_common;
		}
		/* Fall through... */
	    default:
		sz = thing_arityval(hdr);
		*resp = make_boxed(hp);
		*hp++ = hdr;
		ptr++;
		while (sz-- > 0) {
		    *hp++ = *ptr++;
		}
		goto cleanup_next;
	    }
	    break;
	}
	case TAG_PRIMARY_IMMED1:
	    *resp = obj;
	cleanup_next:
	    if (EQUEUE_ISEMPTY(s)) {
		goto all_clean;
	    }
	    obj = EQUEUE_GET(s);
	    for (;;) {
		ASSERT(hscan < hp);
		if (remaining == 0) {
		    if (*hscan == HEAP_ELEM_TO_BE_FILLED) {
			resp = hscan;
			hscan += 2;
			break; /* scanning loop */
		    } else if (primary_tag(*hscan) == TAG_PRIMARY_HEADER) {
			switch (*hscan & _TAG_HEADER_MASK) {
			case ARITYVAL_SUBTAG:
			    remaining = header_arity(*hscan);
			    hscan++;
			    break;
			case FUN_SUBTAG: {
                            const ErlFunThing* funp = (ErlFunThing *) hscan;
                            ASSERT(ERL_FUN_SIZE == (1 + thing_arityval(*hscan)));
                            hscan += ERL_FUN_SIZE;
                            remaining = fun_env_size(funp);
                            break;
			}
			case MAP_SUBTAG:
                            switch (MAP_HEADER_TYPE(*hscan)) {
                                case MAP_HEADER_TAG_FLATMAP_HEAD : {
                                    flatmap_t *mp = (flatmap_t *) hscan;
                                    remaining = flatmap_get_size(mp) + 1;
                                    ASSERT(flatmap_get_size(mp) <= MAP_SMALL_MAP_LIMIT);
                                    hscan += 2;
                                    break;
                                }
                                case MAP_HEADER_TAG_HAMT_HEAD_BITMAP :
                                case MAP_HEADER_TAG_HAMT_HEAD_ARRAY :
                                case MAP_HEADER_TAG_HAMT_NODE_BITMAP :
                                    remaining = hashmap_bitcount(MAP_HEADER_VAL(*hscan));
                                    hscan += MAP_HEADER_ARITY(*hscan) + 1;
                                    break;
                                default:
                                    erts_exit(ERTS_ABORT_EXIT,
                                            "copy_shared_perform: bad hashmap type %d\n",
                                            MAP_HEADER_TYPE(*hscan));
                            }
                            break;
                        case SUB_BITS_SUBTAG: {
                            const ErlSubBits *sb = (ErlSubBits*)hscan;
                            if (sb->orig == HEAP_ELEM_TO_BE_FILLED) {
                                hscan += offsetof(ErlSubBits, orig) / sizeof(Eterm);
                                remaining = 1;
                            } else {
                                ASSERT(erl_sub_bits_is_match_context(sb));
                                hscan += ERL_SUB_BITS_SIZE;
                            }
                            break;
                        }
			default:
			    hscan += 1 + thing_arityval(*hscan);
			    break;
			}
		    } else {
			hscan++;
		    }
		} else if (*hscan == HEAP_ELEM_TO_BE_FILLED) {
		    resp = hscan++;
		    remaining--;
		    break; /* scanning loop */
		} else {
		    hscan++;
		    remaining--;
		}
	    }
	    ASSERT(resp < hp);
	    break;
	default:
	    erts_exit(ERTS_ABORT_EXIT, "size_shared: bad tag for %#x\n", obj);
	}
    }

    /* step #4:
       -------------------------------------------------------
       traverse the table and reverse-transform all stored entries
    */

all_clean:
    for (e = 0; ; e += SHTABLE_INCR) {
	ptr = SHTABLE_REV(t, e);
	if (ptr == NULL)
	    break;
	VERBOSE(DEBUG_SHCOPY, ("[copy] restoring shared: %x\n", ptr));
	/* entry was a list */
	if (SHTABLE_Y(t, e) != THE_NON_VALUE) {
	    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] untabling L %p\n", mypid, ptr));
	    CAR(ptr) = SHTABLE_X(t, e);
	    CDR(ptr) = SHTABLE_Y(t, e);
	}
	/* entry was boxed */
	else {
	    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] untabling B %p\n", mypid, ptr));
	    *ptr = SHTABLE_X(t, e);
	    ASSERT(primary_tag(*ptr) == TAG_PRIMARY_HEADER);
	}
    }

#ifdef DEBUG
    if (eq(saved_obj, result) == 0) {
	erts_fprintf(stderr, "original = %T\n", saved_obj);
	erts_fprintf(stderr, "copy = %T\n", result);
	erts_exit(ERTS_ABORT_EXIT, "copy (shared) not equal to source\n");
    }
#endif

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] original was %T\n", mypid, saved_obj));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] copy is %T\n", mypid, result));
    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] result is at %p\n", mypid, result));

    ASSERT(hbot == hp);
    ASSERT(size == ((hp - *hpp) + (hend - hbot)));
    *hpp = hend;
    return result;
}


/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 */
Eterm copy_shallow_obj_x(Eterm obj, Uint sz, Eterm **hpp, ErlOffHeap *off_heap
#ifdef ERTS_COPY_REGISTER_LOCATION
                         ,
                         char *file, int line
#endif
) {
    Eterm *source_ptr;
    Eterm *target_ptr;

    if (sz == 0) {
        ASSERT(is_zero_sized(obj));
        return obj;
    }

    ASSERT(is_boxed(obj) || is_list(obj));
    ASSERT(!is_zero_sized(obj));

    source_ptr = ptr_val(obj);
#ifdef ERTS_COPY_REGISTER_LOCATION
    target_ptr = copy_shallow_x(source_ptr, sz, hpp, off_heap, file, line);
#else
    target_ptr = copy_shallow_x(source_ptr, sz, hpp, off_heap);
#endif

    return is_boxed(obj) ? make_boxed(target_ptr) : make_list(target_ptr);
}


/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 */
Eterm* copy_shallow_x(Eterm *ERTS_RESTRICT ptr, Uint sz, Eterm **hpp,
                     ErlOffHeap *off_heap
#ifdef ERTS_COPY_REGISTER_LOCATION
                     ,
                     char *file, int line
#endif
) {
    Eterm *tp = ptr;
    Eterm *hp = *hpp;
    Eterm* res = hp;
    const Sint offs = (hp - tp) * sizeof(Eterm);
    const Eterm empty_tuple_literal =
        ERTS_GLOBAL_LIT_EMPTY_TUPLE;
    while (sz--) {
	Eterm val = *tp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
            if (val == empty_tuple_literal) {
                *hp++ = empty_tuple_literal;
            } else {
                *hp++ = byte_offset_ptr(val, offs);
            }
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
            case SUB_BITS_SUBTAG:
                {
                    const ErlSubBits *sb = (ErlSubBits*)(&tp[-1]);
                    const Uint orig_offset = ERL_SUB_BITS_SIZE - 2;

                    ASSERT(erl_sub_bits_is_normal(sb));

                    sys_memcpy(hp, tp, orig_offset * sizeof(Eterm));
                    hp[orig_offset] = byte_offset_ptr(sb->orig, offs);

                    hp += orig_offset + 1;
                    tp += orig_offset + 1;

                    sz -= ERL_SUB_BITS_SIZE - 1;
                }
                break;
            case BIN_REF_SUBTAG:
                {
                    BinRef *br = (BinRef*)(&tp[-1]);
                    erts_refc_inc(&(br->val)->intern.refc, 2);
                    ERTS_BR_OVERHEAD(off_heap, br);
                    goto off_heap_common;
                }
            case FUN_REF_SUBTAG:
                {
                    FunRef *refp = (FunRef *) (tp-1);
                    erts_refc_inc(&(refp->entry)->refc, 2);
                    goto off_heap_common;
                }
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		    ExternalThing* etp = (ExternalThing *) (tp-1);
#if defined(ERTS_COPY_REGISTER_LOCATION) && defined(ERL_NODE_BOOKKEEP)
                    erts_ref_node_entry__(etp->node, 2, make_boxed(hp-1), file, line);
#else
                    erts_ref_node_entry(etp->node, 2, make_boxed(hp-1));
#endif
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
	    case REF_SUBTAG: {
		ErtsRefThing *rtp = (ErtsRefThing *) (tp - 1);
		if (is_magic_ref_thing(rtp)) {
		    ErtsMRefThing *mreft = (ErtsMRefThing *) rtp;
		    erts_refc_inc(&mreft->mb->intern.refc, 2);
		    goto off_heap_common;
		}
		/* Fall through... */
	    }
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
void erts_move_multi_frags(Eterm** hpp, ErlOffHeap* off_heap, ErlHeapFragment* first,
			   Eterm* refs, unsigned nrefs, int literals)
{
    ErlHeapFragment* bp;
    Eterm* hp_start = *hpp;
    Eterm* hp_end;
    Eterm* hp;
    unsigned i;
    Eterm literal_tag;

#ifdef TAG_LITERAL_PTR
    literal_tag = (Eterm) literals ? TAG_LITERAL_PTR : 0;
#else
    literal_tag = (Eterm) 0;
#endif

    for (bp=first; bp!=NULL; bp=bp->next) {
	move_one_frag(hpp, bp, off_heap, literals);
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
#ifdef TAG_LITERAL_PTR
		val |= literal_tag;
#endif
		*hp = val;
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = list_val(gval);
	    val = *ptr;
	    if (IS_MOVED_CONS(val)) {
		val = ptr[1];
#ifdef TAG_LITERAL_PTR
		val |= literal_tag;
#endif
		*hp = val;
	    }
	    break;
	case TAG_PRIMARY_HEADER:
            if (gval == HEADER_SUB_BITS) {
                /* Tag the `orig` field as a literal. It's the last field
                 * inside the thing structure so we can handle it by pretending
                 * it's not part of the thing. */
                hp += thing_arityval(gval) - 1;
            } else if (header_is_thing(gval)) {
                hp += thing_arityval(gval);
            }
            break;
        }
    }
    for (i=0; i<nrefs; ++i) {
	refs[i] = follow_moved(refs[i], literal_tag);
    }
}

static void
move_one_frag(Eterm** hpp, ErlHeapFragment* frag, ErlOffHeap* off_heap, int literals)
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
            ptr = move_boxed(ptr, val, &hp, &dummy_ref);

            switch (val & _HEADER_SUBTAG_MASK) {
            case REF_SUBTAG:
                if (!is_magic_ref_thing(hdr)) {
                    break;
                }
            case BIN_REF_SUBTAG:
            case EXTERNAL_PID_SUBTAG:
            case EXTERNAL_PORT_SUBTAG:
            case EXTERNAL_REF_SUBTAG:
            case FUN_REF_SUBTAG:
                hdr->next = off_heap->first;
                off_heap->first = hdr;
                break;
            }
        } else { /* must be a cons cell */
            ASSERT(ptr+1 < end);
            move_cons(ptr, val, &hp, &dummy_ref);
            ptr += 2;
        }
    }

    *hpp = hp;
    OH_OVERHEAD(off_heap, frag->off_heap.overhead);
    frag->off_heap.first = NULL;
}
