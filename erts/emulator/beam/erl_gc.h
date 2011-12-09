/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

#ifndef __ERL_GC_H__
#define __ERL_GC_H__

/* GC declarations shared by beam/erl_gc.c and hipe/hipe_gc.c */

#if defined(DEBUG) && !ERTS_GLB_INLINE_INCL_FUNC_DEF
#  define HARDDEBUG 1
#endif

#define IS_MOVED_BOXED(x)	(!is_header((x)))
#define IS_MOVED_CONS(x)	(is_non_value((x)))

#define MOVE_CONS(PTR,CAR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
									\
    HTOP[0] = CAR;		/* copy car */				\
    HTOP[1] = PTR[1];		/* copy cdr */				\
    gval = make_list(HTOP);	/* new location */			\
    *ORIG = gval;		/* redirect original reference */	\
    PTR[0] = THE_NON_VALUE;	/* store forwarding indicator */	\
    PTR[1] = gval;		/* store forwarding address */		\
    HTOP += 2;			/* update tospace htop */		\
} while(0)

#define MOVE_BOXED(PTR,HDR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
    Sint nelts;								\
									\
    ASSERT(is_header(HDR));						\
    gval = make_boxed(HTOP);						\
    *ORIG = gval;							\
    *HTOP++ = HDR;							\
    *PTR++ = gval;							\
    nelts = header_arity(HDR);						\
    switch ((HDR) & _HEADER_SUBTAG_MASK) {				\
    case SUB_BINARY_SUBTAG: nelts++; break;				\
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR-1))->num_free+1; break;	\
    }									\
    while (nelts--)							\
	*HTOP++ = *PTR++;						\
} while(0)

#define in_area(ptr,start,nbytes) \
 ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

extern Uint erts_test_long_gc_sleep;

#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)
int within(Eterm *ptr, Process *p);
#endif

ERTS_GLB_INLINE Eterm follow_moved(Eterm term);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm follow_moved(Eterm term)
{
    Eterm* ptr;
    switch (primary_tag(term)) {
    case TAG_PRIMARY_IMMED1:
	break;
    case TAG_PRIMARY_BOXED:
	ptr = boxed_val(term);
	if (IS_MOVED_BOXED(*ptr)) term = *ptr;
	break;
    case TAG_PRIMARY_LIST:
	ptr = list_val(term);
	if (IS_MOVED_CONS(ptr[0])) term = ptr[1];
	break;
    default:
	ASSERT(!"strange tag in follow_moved");
    }
    return term;
}
#endif

#endif /* __ERL_GC_H__ */
