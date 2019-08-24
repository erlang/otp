/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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


#ifndef HIPE_STACK_H
#define HIPE_STACK_H

#include "hipe_arch.h"

/*
 * Stack descriptors.
 */

#include <stddef.h>	/* offsetof() */

struct hipe_sdesc {
    struct {
	unsigned long hvalue;	/* return address */
	struct hipe_sdesc *next;	/* hash collision chain */
    } bucket;
    unsigned int fsize : 23;    /* frame size */
    unsigned int has_exnra : 1; /* exn handler presence flag */
    unsigned int stk_nargs : 8; /* arguments on stack */
    Uint32 m_aix;
    Uint32 f_aix;
    Uint32 a;
    struct hipe_sdesc* next_in_modi;
    Uint32 livebits[1]; /* size depends on arch & data in summary field */
};

struct hipe_sdesc_with_exnra {
    unsigned long exnra;
    struct hipe_sdesc sdesc;
};

static __inline__ unsigned int sdesc_fsize(const struct hipe_sdesc *sdesc)
{
    return sdesc->fsize;
}

/* Nr of arguments pushed on stack */
static __inline__ unsigned int sdesc_arity(const struct hipe_sdesc *sdesc)
{
    return sdesc->stk_nargs;
}

static __inline__ unsigned long sdesc_exnra(const struct hipe_sdesc *sdesc)
{
    if (sdesc->has_exnra) {
	const char *tmp;
	tmp = (const char*)sdesc - offsetof(struct hipe_sdesc_with_exnra, sdesc);
	return ((const struct hipe_sdesc_with_exnra*)tmp)->exnra;
    }
    return 0;
}

struct hipe_sdesc_table {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size)-1 */
    unsigned int used;
    struct hipe_sdesc **bucket;
};
extern struct hipe_sdesc_table hipe_sdesc_table;

extern struct hipe_sdesc *hipe_put_sdesc(struct hipe_sdesc*);
extern void hipe_destruct_sdesc(struct hipe_sdesc*);
extern void hipe_init_sdesc_table(struct hipe_sdesc*);
extern struct hipe_sdesc *hipe_decode_sdesc(Eterm);

#if !defined(__GNUC__) || (__GNUC__ < 2) || (__GNUC__ == 2 && __GNUC_MINOR__ < 96)
#define __builtin_expect(x, expected_value) (x)
#endif
#define likely(x)	__builtin_expect((x),1)
#define unlikely(x)	__builtin_expect((x),0)

static __inline__ const struct hipe_sdesc *hipe_find_sdesc(unsigned long ra)
{
    unsigned int i = (ra >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    const struct hipe_sdesc *sdesc = hipe_sdesc_table.bucket[i];
    if (likely(sdesc->bucket.hvalue == ra))
	return sdesc;
    do {
	sdesc = sdesc->bucket.next;
    } while (sdesc->bucket.hvalue != ra);
    return sdesc;
}

AEXTERN(void,nbif_stack_trap_ra,(void));

extern void hipe_print_nstack(Process*);
extern void hipe_find_handler(Process*);
extern void (*hipe_handle_stack_trap(Process*))(void);
extern void hipe_update_stack_trap(Process*, const struct hipe_sdesc*);
extern int hipe_fill_stacktrace(Process*, int, Eterm**);

#if 0 && defined(HIPE_NSTACK_GROWS_UP)
#define hipe_nstack_start(p)	((p)->hipe.nstack)
#define hipe_nstack_used(p)	((p)->hipe.nsp - (p)->hipe.nstack)
#define hipe_nstack_avail(p)	((p)->hipe.nstend - (p)->hipe.nsp)
#endif
#if defined(HIPE_NSTACK_GROWS_DOWN)
#define hipe_nstack_start(p)	((p)->hipe.nsp)
#define hipe_nstack_used(p)	((p)->hipe.nstend - (p)->hipe.nsp)
#define hipe_nstack_avail(p)	((unsigned)((p)->hipe.nsp - (p)->hipe.nstack))
#endif

/* ensure that at least nwords words are available on the native stack */
static __inline__ void hipe_check_nstack(Process *p, unsigned nwords)
{
    extern void hipe_inc_nstack(Process *p);

    while (hipe_nstack_avail(p) < nwords)
	hipe_inc_nstack(p);
}

/*
 * GC support procedures
 */
extern Eterm *fullsweep_nstack(Process *p, Eterm *n_htop);
extern void gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop);
extern Eterm *sweep_literals_nstack(Process *p, Eterm *n_htop, char *area,
				    Uint area_size);
extern int nstack_any_heap_ref_ptrs(Process *, char* mod_start, Uint mod_size);
extern int nstack_any_cps_in_segment(Process *, char* seg_start, Uint seg_size);


#endif /* HIPE_STACK_H */
