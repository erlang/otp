/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2017. All Rights Reserved.
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
#include "config.h"
#endif
#include "global.h"

#include "hipe_stack.h"

/*
 * Native-code stack descriptor hash table.
 *
 * This uses a specialised version of BEAM's hash table code:
 * - Hash table size is always a power of two.
 *   Permits replacing an expensive integer division operation
 *   with a cheap bitwise 'and' in the hash index calculation
 * - Lookups assume the key is in the table.
 *   Permits removing NULL checks.
 * - Switched order of the hash bucket next and hvalue fields.
 *   The hvalue field, which must always be checked, gets a zero
 *   structure offset, which is faster on some architectures;
 *   the next field is only referenced if hvalue didn't match.
 * These changes yield a much more efficient lookup operation.
 */
struct hipe_sdesc_table hipe_sdesc_table;

static struct hipe_sdesc **alloc_bucket(unsigned int size)
{
    unsigned long nbytes = size * sizeof(struct hipe_sdesc*);
    struct hipe_sdesc **bucket = erts_alloc(ERTS_ALC_T_HIPE_LL, nbytes);
    sys_memzero(bucket, nbytes);
    return bucket;
}

static void hipe_grow_sdesc_table(void)
{
    unsigned int old_size, new_size, new_mask;
    struct hipe_sdesc **old_bucket, **new_bucket;
    unsigned int i;

    old_size = 1 << hipe_sdesc_table.log2size;
    hipe_sdesc_table.log2size += 1;
    new_size = 1 << hipe_sdesc_table.log2size;
    new_mask = new_size - 1;
    hipe_sdesc_table.mask = new_mask;
    old_bucket = hipe_sdesc_table.bucket;
    new_bucket = alloc_bucket(new_size);
    hipe_sdesc_table.bucket = new_bucket;
    for (i = 0; i < old_size; ++i) {
	struct hipe_sdesc *b = old_bucket[i];
	while (b != NULL) {
	    struct hipe_sdesc *next = b->bucket.next;
	    unsigned int j = (b->bucket.hvalue >> HIPE_RA_LSR_COUNT) & new_mask;
	    b->bucket.next = new_bucket[j];
	    new_bucket[j] = b;
	    b = next;
	}
    }
    erts_free(ERTS_ALC_T_HIPE_LL, old_bucket);
}

struct hipe_sdesc *hipe_put_sdesc(struct hipe_sdesc *sdesc)
{
    unsigned long ra;
    unsigned int i;
    struct hipe_sdesc *chain;
    unsigned int size;

    ra = sdesc->bucket.hvalue;
    i = (ra >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    chain = hipe_sdesc_table.bucket[i];

    for (; chain != NULL; chain = chain->bucket.next)
	if (chain->bucket.hvalue == ra)
	    return chain;	/* collision! (shouldn't happen) */

    sdesc->bucket.next = hipe_sdesc_table.bucket[i];
    hipe_sdesc_table.bucket[i] = sdesc;
    hipe_sdesc_table.used += 1;
    size = 1 << hipe_sdesc_table.log2size;
    if (hipe_sdesc_table.used > (4*size)/5)	/* rehash at 80% */
	hipe_grow_sdesc_table();
    return sdesc;
}

void hipe_destruct_sdesc(struct hipe_sdesc *sdesc)
{
    unsigned int i;
    struct hipe_sdesc** prevp;
    void* free_me;

    i = (sdesc->bucket.hvalue >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    prevp = &hipe_sdesc_table.bucket[i];

    for (; *prevp != sdesc; prevp = &(*prevp)->bucket.next)
        ASSERT(*prevp);

    *prevp = sdesc->bucket.next;
    hipe_sdesc_table.used -= 1;

    if (sdesc->has_exnra)
        free_me = ErtsContainerStruct(sdesc, struct hipe_sdesc_with_exnra, sdesc);
    else
        free_me = sdesc;
    erts_free(ERTS_ALC_T_HIPE_LL, free_me);
}

void hipe_init_sdesc_table(struct hipe_sdesc *sdesc)
{
    unsigned int log2size, size;

    log2size = 10;
    size = 1 << log2size;
    hipe_sdesc_table.log2size = log2size;
    hipe_sdesc_table.mask = size - 1;
    hipe_sdesc_table.used = 0;
    hipe_sdesc_table.bucket = alloc_bucket(size);

    hipe_put_sdesc(sdesc);
}

/*
 * XXX: x86 and SPARC currently use the same stack descriptor
 * representation. If different representations are needed in
 * the future, this code has to be made target dependent.
 */
struct hipe_sdesc *hipe_decode_sdesc(Eterm arg)
{
    Uint ra, exnra;
    Eterm *live;
    Uint fsize, nargs, stk_nargs, nlive, i, nslots, off;
    Uint livebitswords, sdescbytes;
    void *p;
    struct hipe_sdesc *sdesc;
    Eterm* mfa_tpl;
    Eterm* tp;

    if (is_not_tuple(arg))
        return 0;

    tp = tuple_val(arg);
    if (tp[0] != make_arityval(6) ||
        term_to_Uint(tp[1], &ra) == 0 ||
	term_to_Uint(tp[2], &exnra) == 0 ||
	is_not_small(tp[3]) ||
	(fsize = unsigned_val(tp[3])) > 65535 ||
	is_not_small(tp[4]) ||
	(stk_nargs = unsigned_val(tp[4])) > 255 ||
	is_not_tuple(tp[5]) ||
        is_not_tuple(tp[6]) ||
        (mfa_tpl = tuple_val(tp[6]))[0] != make_arityval(3) ||
        is_not_atom(mfa_tpl[1]) ||
        is_not_atom(mfa_tpl[2]) ||
        is_not_small(mfa_tpl[3]) ||
        (nargs = unsigned_val(mfa_tpl[3])) > 255)
	return 0;

    if (stk_nargs > nargs)
        return 0;

    /* Get tuple with live slots */
    live = tuple_val(tp[5]) + 1;
    /* Get number of live slots */
    nlive = arityval(live[-1]);
    /* Calculate size of frame = locals + ra + stack arguments */
    nslots = fsize + 1 + stk_nargs;
    /* Check that only valid slots are given. */
    for (i = 0; i < nlive; ++i) {
	if (is_not_small(live[i]) ||
	    (off = unsigned_val(live[i]), off >= nslots) ||
	    off == fsize)
	    return 0;
    }

    /* Calculate number of words for the live bitmap. */
    livebitswords = (fsize + stk_nargs + 1 + 31) / 32;
    /* Calculate number of bytes needed for the stack descriptor. */
    sdescbytes =
	(exnra
	 ? offsetof(struct hipe_sdesc_with_exnra, sdesc.livebits)
	 : offsetof(struct hipe_sdesc, livebits))
	+ livebitswords * sizeof(int);
    p = erts_alloc(ERTS_ALC_T_HIPE_LL, sdescbytes);
    /* If we have an exception handler use the
       special sdesc_with_exnra structure. */
    if (exnra) {
	struct hipe_sdesc_with_exnra *sdesc_we = p;
	sdesc_we->exnra = exnra;
	sdesc = &(sdesc_we->sdesc);
    } else
	sdesc = p;

    sdesc->m_aix = atom_val(mfa_tpl[1]);
    sdesc->f_aix = atom_val(mfa_tpl[2]);
    sdesc->a = nargs;


    /* Initialise head of sdesc. */
    sdesc->bucket.next = 0;
    sdesc->bucket.hvalue = ra;
    sdesc->fsize = fsize;
    sdesc->has_exnra = (exnra ? 1 : 0);
    sdesc->stk_nargs = stk_nargs;
    /* Clear all live-bits */
    for (i = 0; i < livebitswords; ++i)
	sdesc->livebits[i] = 0;
    /* Set live-bits given by caller. */
    for (i = 0; i < nlive; ++i) {
	off = unsigned_val(live[i]);
	sdesc->livebits[off / 32] |= (1 << (off & 31));
    }
    return sdesc;
}
