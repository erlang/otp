/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
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

/*  Implementation of the erlang external format 
 *
 *  And a nice cache mechanism which is used just to send a
 *  index indicating a specific atom to a remote node instead of the
 *  entire atom.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_WANT_EXTERNAL_TAGS

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "external.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_zlib.h"
#include "erl_map.h"

#ifdef HIPE
#include "hipe_mode_switch.h"
#endif
#define in_area(ptr,start,nbytes) ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

#define MAX_STRING_LEN 0xffff

#define is_valid_creation(Cre) ((unsigned)(Cre) < MAX_CREATION || (Cre) == INTERNAL_CREATION)

#undef ERTS_DEBUG_USE_DIST_SEP
#ifdef DEBUG
#  if 0
/*
 * Enabling ERTS_DEBUG_USE_DIST_SEP can be useful when debugging, but the
 * result refuses to talk to nodes without it!
 */
#    define ERTS_DEBUG_USE_DIST_SEP
#  endif
#  define IF_DEBUG(X) X
#else
#  define IF_DEBUG(X)
#endif

/* Does Sint fit in Sint32?
 */
#define IS_SSMALL32(x) (((Uint) (((x) >> (32-1)) + 1)) < 2)

/*
 *   Valid creations for nodes are 1, 2, or 3. 0 can also be sent
 *   as creation, though. When 0 is used as creation, the real creation
 *   is unknown. Creation 0 on data will be changed to current
 *   creation of the node which it belongs to when it enters
 *   that node.
 *       This typically happens when a remote pid is created with
 *   list_to_pid/1 and then sent to the remote node. This behavior 
 *   has the undesirable effect that a pid can be passed between nodes,
 *   and as a result of that not being equal to itself (the pid that
 *   comes back isn't equal to the original pid).
 *
 */

static Export term_to_binary_trap_export;

static byte* enc_term(ErtsAtomCacheMap *, Eterm, byte*, Uint32, struct erl_off_heap_header** off_heap);
struct TTBEncodeContext_;
static int enc_term_int(struct TTBEncodeContext_*,ErtsAtomCacheMap *acmp, Eterm obj, byte* ep, Uint32 dflags,
			struct erl_off_heap_header** off_heap, Sint *reds, byte **res);
static Uint is_external_string(Eterm obj, int* p_is_string);
static byte* enc_atom(ErtsAtomCacheMap *, Eterm, byte*, Uint32);
static byte* enc_pid(ErtsAtomCacheMap *, Eterm, byte*, Uint32);
struct B2TContext_t;
static byte* dec_term(ErtsDistExternal *, Eterm**, byte*, ErlOffHeap*, Eterm*, struct B2TContext_t*);
static byte* dec_atom(ErtsDistExternal *, byte*, Eterm*);
static byte* dec_pid(ErtsDistExternal *, Eterm**, byte*, ErlOffHeap*, Eterm*);
static Sint decoded_size(byte *ep, byte* endp, int internal_tags, struct B2TContext_t*);
static BIF_RETTYPE term_to_binary_trap_1(BIF_ALIST_1);

static Eterm erts_term_to_binary_int(Process* p, Eterm Term, int level, Uint flags, 
				     Binary *context_b);

static Uint encode_size_struct2(ErtsAtomCacheMap *, Eterm, unsigned);
struct TTBSizeContext_;
static int encode_size_struct_int(struct TTBSizeContext_*, ErtsAtomCacheMap *acmp, Eterm obj,
				  unsigned dflags, Sint *reds, Uint *res);

static Export binary_to_term_trap_export;
static BIF_RETTYPE binary_to_term_trap_1(BIF_ALIST_1);
static Eterm binary_to_term_int(Process* p, Uint32 flags, Eterm bin, Binary* context_b);

void erts_init_external(void) {
#if 1 /* In R16 */
    erts_init_trap_export(&term_to_binary_trap_export,
			  am_erlang, am_term_to_binary_trap, 1,
			  &term_to_binary_trap_1);

    erts_init_trap_export(&binary_to_term_trap_export,
			  am_erlang, am_binary_to_term_trap, 1,
			  &binary_to_term_trap_1);
#else
    sys_memset((void *) &term_to_binary_trap_export, 0, sizeof(Export));
    term_to_binary_trap_export.address = &term_to_binary_trap_export.code[3];
    term_to_binary_trap_export.code[0] = am_erlang;
    term_to_binary_trap_export.code[1] = am_term_to_binary_trap;
    term_to_binary_trap_export.code[2] = 1;
    term_to_binary_trap_export.code[3] = (BeamInstr) em_apply_bif;
    term_to_binary_trap_export.code[4] = (BeamInstr) &term_to_binary_trap_1;
#endif    
    return;
}

#define ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES 255

#define ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(IIX) \
  (((((Uint32) (IIX)) >> 1) & 0x7fffffff))
#define ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(IIX) \
  (((IIX) << 2) & 7)
#define ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(NO_ATOMS) \
  (((((Uint32) (NO_ATOMS)) >> 1) & 0x7fffffff)+1)

#define ERTS_DIST_HDR_LONG_ATOMS_FLG (1 << 0)

/* #define ERTS_ATOM_CACHE_HASH */
#define ERTS_USE_ATOM_CACHE_SIZE 2039
#if ERTS_ATOM_CACHE_SIZE < ERTS_USE_ATOM_CACHE_SIZE
#error "ERTS_USE_ATOM_CACHE_SIZE too large"
#endif

static ERTS_INLINE int
atom2cix(Eterm atom)
{
    Uint val;
    ASSERT(is_atom(atom));
    val = atom_val(atom);
#ifdef ERTS_ATOM_CACHE_HASH
    val = atom_tab(val)->slot.bucket.hvalue;
#endif
#if ERTS_USE_ATOM_CACHE_SIZE == 256
    return (int) (val & ((Uint) 0xff));
#else
    return (int) (val % ERTS_USE_ATOM_CACHE_SIZE);
#endif
}

int erts_debug_max_atom_out_cache_index(void)
{
    return ERTS_USE_ATOM_CACHE_SIZE-1;
}

int
erts_debug_atom_to_out_cache_index(Eterm atom)
{
    return atom2cix(atom);
}

void
erts_init_atom_cache_map(ErtsAtomCacheMap *acmp)
{
    if (acmp) {
	int ix;
	acmp->long_atoms = 0;
	for (ix = 0; ix < ERTS_ATOM_CACHE_SIZE; ix++)
	    acmp->cache[ix].iix = -1;
	acmp->sz = 0;
	acmp->hdr_sz = -1;
    }
}

void
erts_reset_atom_cache_map(ErtsAtomCacheMap *acmp)
{
    if (acmp) {
	int i;
	acmp->long_atoms = 0;
	for (i = 0; i < acmp->sz; i++) {
	    ASSERT(0 <= acmp->cix[i] && acmp->cix[i] < ERTS_ATOM_CACHE_SIZE);
	    acmp->cache[acmp->cix[i]].iix = -1;
	}
	acmp->sz = 0;
	acmp->hdr_sz = -1;
#ifdef DEBUG
	for (i = 0; i < ERTS_ATOM_CACHE_SIZE; i++) {
	    ASSERT(acmp->cache[i].iix < 0);
	}
#endif
    }
}

void
erts_destroy_atom_cache_map(ErtsAtomCacheMap *acmp)
{

}

static ERTS_INLINE void
insert_acache_map(ErtsAtomCacheMap *acmp, Eterm atom, Uint32 dflags)
{
    /*
     * If the receiver do not understand utf8 atoms
     * and this atom cannot be represented in latin1,
     * we are not allowed to cache it.
     *
     * In this case all atoms are assumed to have
     * latin1 encoding in the cache. By refusing it
     * in the cache we will instead encode it using
     * ATOM_UTF8_EXT/SMALL_ATOM_UTF8_EXT which the
     * receiver do not recognize and tear down the
     * connection.
     */
    if (acmp && acmp->sz < ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES
	&& ((dflags & DFLAG_UTF8_ATOMS)
	    || atom_tab(atom_val(atom))->latin1_chars >= 0)) {
	int ix;
	ASSERT(acmp->hdr_sz < 0);
	ix = atom2cix(atom);
	if (acmp->cache[ix].iix < 0) {
	    acmp->cache[ix].iix = acmp->sz;
	    acmp->cix[acmp->sz++] = ix;
	    acmp->cache[ix].atom = atom;
	}
    }
}

static ERTS_INLINE int
get_iix_acache_map(ErtsAtomCacheMap *acmp, Eterm atom, Uint32 dflags)
{
    if (!acmp)
	return -1;
    else {
	int ix;
	ASSERT(is_atom(atom));
	ix = atom2cix(atom);
	if (acmp->cache[ix].iix < 0) {
	    ASSERT(acmp->sz == ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES
		   || (!(dflags & DFLAG_UTF8_ATOMS)
		       && atom_tab(atom_val(atom))->latin1_chars < 0));
	    return -1;
	}
	else {
	    ASSERT(acmp->cache[ix].iix < ERTS_ATOM_CACHE_SIZE);
	    return acmp->cache[ix].atom == atom ? acmp->cache[ix].iix : -1;
	}
    }
}

void
erts_finalize_atom_cache_map(ErtsAtomCacheMap *acmp, Uint32 dflags)
{
    if (acmp) {
	int utf8_atoms = (int) (dflags & DFLAG_UTF8_ATOMS);
	int long_atoms = 0; /* !0 if one or more atoms are longer than 255. */
	int i;
	int sz;
	int fix_sz
	    = 1 /* VERSION_MAGIC */
	    + 1 /* DIST_HEADER */
	    + 1 /* dist header flags */
	    + 1 /* number of internal cache entries */
	    ;
	int min_sz;
	ASSERT(acmp->hdr_sz < 0);
	/* Make sure cache update instructions fit */
	min_sz = fix_sz+(2+4)*acmp->sz;
	sz = fix_sz;
	for (i = 0; i < acmp->sz; i++) {
	    Atom *a;
	    Eterm atom;
	    int len;
	    atom = acmp->cache[acmp->cix[i]].atom;
	    ASSERT(is_atom(atom));
	    a = atom_tab(atom_val(atom));
	    len = (int) (utf8_atoms ? a->len : a->latin1_chars);
	    ASSERT(len >= 0);
	    if (!long_atoms && len > 255)
		long_atoms = 1;
	    /* Enough for a new atom cache value */
	    sz += 1 /* cix */ + 1 /* length */ + len /* text */;
	}
	if (long_atoms) {
	    acmp->long_atoms = 1;
	    sz += acmp->sz; /* we need 2 bytes per atom for length */
	}
	/* Dynamically sized flag field */
	sz += ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(acmp->sz);
	if (sz < min_sz)
	    sz = min_sz;
	acmp->hdr_sz = sz;
    }
}

Uint
erts_encode_ext_dist_header_size(ErtsAtomCacheMap *acmp)
{
    if (!acmp)
	return 0;
    else {
	ASSERT(acmp->hdr_sz >= 0);
	return acmp->hdr_sz;
    }
}

byte *erts_encode_ext_dist_header_setup(byte *ctl_ext, ErtsAtomCacheMap *acmp)
{
    /* Maximum number of atom must be less than the maximum of a 32 bits
       unsigned integer. Check is done in erl_init.c, erl_start function. */
    if (!acmp)
	return ctl_ext;
    else {
	int i;
	byte *ep = ctl_ext;
	byte dist_hdr_flags = acmp->long_atoms ? ERTS_DIST_HDR_LONG_ATOMS_FLG : 0;
	ASSERT(acmp->hdr_sz >= 0);
	/*
	 * Write cache update instructions. Note that this is a purely
	 * internal format, never seen on the wire. This section is later
	 * rewritten by erts_encode_ext_dist_header_finalize() while updating
	 * the cache. We write the header backwards just before the
	 * actual term(s).
	 */
	for (i = acmp->sz-1; i >= 0; i--) {
	    Uint32 aval;
	    ASSERT(0 <= acmp->cix[i] && acmp->cix[i] < ERTS_ATOM_CACHE_SIZE);
	    ASSERT(i == acmp->cache[acmp->cix[i]].iix);
	    ASSERT(is_atom(acmp->cache[acmp->cix[i]].atom));

	    aval = (Uint32) atom_val(acmp->cache[acmp->cix[i]].atom);
	    ep -= 4;
	    put_int32(aval, ep);
	    ep -= 2;
	    put_int16(acmp->cix[i], ep);
	}
	--ep;
	put_int8(acmp->sz, ep);
	--ep;
	put_int8(dist_hdr_flags, ep);
	*--ep = DIST_HEADER;
	*--ep = VERSION_MAGIC;
	return ep;
    }
}

byte *erts_encode_ext_dist_header_finalize(byte *ext, ErtsAtomCache *cache, Uint32 dflags)
{
    byte *ip;
    byte instr_buf[(2+4)*ERTS_ATOM_CACHE_SIZE];
    int ci, sz;
    byte dist_hdr_flags;
    int long_atoms;
    int utf8_atoms = (int) (dflags & DFLAG_UTF8_ATOMS);
    register byte *ep = ext;
    ASSERT(ep[0] == VERSION_MAGIC);
    if (ep[1] != DIST_HEADER)
	return ext;

    dist_hdr_flags = ep[2];
    long_atoms = ERTS_DIST_HDR_LONG_ATOMS_FLG & ((int) dist_hdr_flags);

    /*
     * Update output atom cache and write the external version of
     * the dist header. We write the header backwards just
     * before the actual term(s).
     */
    ep += 3;
    ci = (int) get_int8(ep);
    ASSERT(0 <= ci && ci < ERTS_ATOM_CACHE_SIZE);
    ep += 1;
    sz = (2+4)*ci;
    ip = &instr_buf[0];
    sys_memcpy((void *) ip, (void *) ep, sz);
    ep += sz;
    /* ep now points to the beginning of the control message term */
#ifdef ERTS_DEBUG_USE_DIST_SEP
    ASSERT(*ep == VERSION_MAGIC);
#endif
    if (ci > 0) {
	Uint32 flgs_buf[((ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(
			      ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES)-1)
			 / sizeof(Uint32))+1];
	register Uint32 flgs;
	int iix, flgs_bytes, flgs_buf_ix, used_half_bytes;
#ifdef DEBUG
	int tot_used_half_bytes;
#endif

	flgs_bytes = ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(ci);

	ASSERT(flgs_bytes <= sizeof(flgs_buf));
	flgs = (Uint32) dist_hdr_flags;
	flgs_buf_ix = 0;
	if ((ci & 1) == 0)
	    used_half_bytes = 2;
	else
	    used_half_bytes = 1;
#ifdef DEBUG
	tot_used_half_bytes = used_half_bytes;
#endif
	iix = ci-1;
	while (iix >= 0) {
	    int cix;
	    Eterm atom;

	    if (used_half_bytes != 8)
		flgs <<= 4;
	    else {
		flgs_buf[flgs_buf_ix++] = flgs;
		flgs = 0;
		used_half_bytes = 0;
	    }

	    ip = &instr_buf[0] + (2+4)*iix;
	    cix = (int) get_int16(&ip[0]);
	    ASSERT(0 <= cix && cix < ERTS_ATOM_CACHE_SIZE);
	    atom = make_atom((Uint) get_int32(&ip[2]));
	    if (cache->out_arr[cix] == atom) {
		--ep;
		put_int8(cix, ep);
		flgs |= ((cix >> 8) & 7);
	    }
	    else {
		Atom *a;
		cache->out_arr[cix] = atom;
		a = atom_tab(atom_val(atom));
		if (utf8_atoms) {
		    sz = a->len;
		    ep -= sz;
		    sys_memcpy((void *) ep, (void *) a->name, sz);
		}
		else {
		    ASSERT(0 <= a->latin1_chars && a->latin1_chars <= MAX_ATOM_CHARACTERS);
		    ep -= a->latin1_chars;
		    sz = erts_utf8_to_latin1(ep, a->name, a->len);
		    ASSERT(a->latin1_chars == sz);
		}
		if (long_atoms) {
		    ep -= 2;
		    put_int16(sz, ep);
		}
		else {
		    ASSERT(0 <= sz && sz <= 255);
		    --ep;
		    put_int8(sz, ep);
		}
		--ep;
		put_int8(cix, ep);
		flgs |= (8 | ((cix >> 8) & 7));
	    }
	    iix--;
	    used_half_bytes++;
#ifdef DEBUG
	    tot_used_half_bytes++;
#endif
	}
	ASSERT(tot_used_half_bytes == 2*flgs_bytes);
	flgs_buf[flgs_buf_ix] = flgs;
	flgs_buf_ix = 0;
	while (1) {
	    flgs = flgs_buf[flgs_buf_ix];
	    if (flgs_bytes > 4) {
		*--ep = (byte) ((flgs >> 24) & 0xff);
		*--ep = (byte) ((flgs >> 16) & 0xff);
		*--ep = (byte) ((flgs >> 8) & 0xff);
		*--ep = (byte) (flgs & 0xff);
		flgs_buf_ix++;
		flgs_bytes -= 4;
	    }
	    else {
		switch (flgs_bytes) {
		case 4:
		    *--ep = (byte) ((flgs >> 24) & 0xff);
		case 3:
		    *--ep = (byte) ((flgs >> 16) & 0xff);
		case 2:
		    *--ep = (byte) ((flgs >> 8) & 0xff);
		case 1:
		    *--ep = (byte) (flgs & 0xff);
		}
		break;
	    }
	}
    }
    --ep;
    put_int8(ci, ep);
    *--ep = DIST_HEADER;
    *--ep = VERSION_MAGIC;
    return ep;
}

Uint erts_encode_dist_ext_size(Eterm term, Uint32 flags, ErtsAtomCacheMap *acmp)
{
    Uint sz = 0;
#ifndef ERTS_DEBUG_USE_DIST_SEP
    if (!(flags & DFLAG_DIST_HDR_ATOM_CACHE))
#endif
	sz++ /* VERSION_MAGIC */;
    sz += encode_size_struct2(acmp, term, flags);
    return sz;
}

Uint erts_encode_ext_size(Eterm term)
{
    return encode_size_struct2(NULL, term, TERM_TO_BINARY_DFLAGS)
	+ 1 /* VERSION_MAGIC */;
}

Uint erts_encode_ext_size_2(Eterm term, unsigned dflags)
{
    return encode_size_struct2(NULL, term, TERM_TO_BINARY_DFLAGS|dflags)
        + 1 /* VERSION_MAGIC */;
}

Uint erts_encode_ext_size_ets(Eterm term)
{
    return encode_size_struct2(NULL, term, TERM_TO_BINARY_DFLAGS|DFLAG_INTERNAL_TAGS);
}


void erts_encode_dist_ext(Eterm term, byte **ext, Uint32 flags, ErtsAtomCacheMap *acmp)
{
    byte *ep = *ext;
#ifndef ERTS_DEBUG_USE_DIST_SEP
    if (!(flags & DFLAG_DIST_HDR_ATOM_CACHE))
#endif
	*ep++ = VERSION_MAGIC;
    ep = enc_term(acmp, term, ep, flags, NULL);
    if (!ep)
	erl_exit(ERTS_ABORT_EXIT,
		 "%s:%d:erts_encode_dist_ext(): Internal data structure error\n",
		 __FILE__, __LINE__);
    *ext = ep;
}

void erts_encode_ext(Eterm term, byte **ext)
{
    byte *ep = *ext;
    *ep++ = VERSION_MAGIC;
    ep = enc_term(NULL, term, ep, TERM_TO_BINARY_DFLAGS, NULL);
    if (!ep)
	erl_exit(ERTS_ABORT_EXIT,
		 "%s:%d:erts_encode_ext(): Internal data structure error\n",
		 __FILE__, __LINE__);
    *ext = ep;
}

byte* erts_encode_ext_ets(Eterm term, byte *ep, struct erl_off_heap_header** off_heap)
{
    return enc_term(NULL, term, ep, TERM_TO_BINARY_DFLAGS|DFLAG_INTERNAL_TAGS,
		    off_heap);
}

ErtsDistExternal *
erts_make_dist_ext_copy(ErtsDistExternal *edep, Uint xsize)
{
    size_t align_sz;
    size_t dist_ext_sz;
    size_t ext_sz;
    byte *ep;
    ErtsDistExternal *new_edep;

    dist_ext_sz = ERTS_DIST_EXT_SIZE(edep);
    ASSERT(edep->ext_endp && edep->extp);
    ASSERT(edep->ext_endp >= edep->extp);
    ext_sz = edep->ext_endp - edep->extp;

    align_sz = ERTS_EXTRA_DATA_ALIGN_SZ(dist_ext_sz + ext_sz);

    new_edep = erts_alloc(ERTS_ALC_T_EXT_TERM_DATA,
			  dist_ext_sz + ext_sz + align_sz + xsize);

    ep = (byte *) new_edep;
    sys_memcpy((void *) ep, (void *) edep, dist_ext_sz);
    ep += dist_ext_sz;
    if (new_edep->dep)
	erts_refc_inc(&new_edep->dep->refc, 1);
    new_edep->extp = ep;
    new_edep->ext_endp = ep + ext_sz;
    new_edep->heap_size = -1;
    sys_memcpy((void *) ep, (void *) edep->extp, ext_sz);
    return new_edep;
}

int
erts_prepare_dist_ext(ErtsDistExternal *edep,
		      byte *ext,
		      Uint size,
		      DistEntry *dep,
		      ErtsAtomCache *cache)
{
#undef ERTS_EXT_FAIL
#undef ERTS_EXT_HDR_FAIL
#if 1
#define ERTS_EXT_FAIL goto fail
#define ERTS_EXT_HDR_FAIL goto bad_hdr
#else
#define ERTS_EXT_FAIL abort()
#define ERTS_EXT_HDR_FAIL abort()
#endif

    register byte *ep = ext;
    int utf8_atoms = (int) (dep->flags & DFLAG_UTF8_ATOMS);

    edep->heap_size = -1;
    edep->ext_endp = ext+size;

    if (size < 2)
	ERTS_EXT_FAIL;

    if (ep[0] != VERSION_MAGIC) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	if (dep)
	    erts_dsprintf(dsbufp,
			  "** Got message from incompatible erlang on "
			  "channel %d\n",
			  dist_entry_channel_no(dep));
	else
	    erts_dsprintf(dsbufp,
			  "** Attempt to convert old incompatible "
			  "binary %d\n",
			  *ep);
	erts_send_error_to_logger_nogl(dsbufp);
	ERTS_EXT_FAIL;
    }

    edep->flags = 0;
    edep->dep = dep;
    if (dep) {
	erts_smp_de_rlock(dep);
	if (dep->flags & DFLAG_DIST_HDR_ATOM_CACHE)
	    edep->flags |= ERTS_DIST_EXT_DFLAG_HDR;
	    
	edep->flags |= (dep->connection_id & ERTS_DIST_EXT_CON_ID_MASK);
	erts_smp_de_runlock(dep);
    }

    if (ep[1] != DIST_HEADER) {
	if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR)
	    ERTS_EXT_HDR_FAIL;
	edep->attab.size = 0;
	edep->extp = ext;
    }
    else {
	int tix;
	int no_atoms;

	if (!(edep->flags & ERTS_DIST_EXT_DFLAG_HDR))
	    ERTS_EXT_HDR_FAIL;

#undef CHKSIZE
#define CHKSIZE(SZ) \
	do { if ((SZ) > edep->ext_endp - ep) ERTS_EXT_HDR_FAIL; } while(0)

	CHKSIZE(1+1+1);
	ep += 2;
	no_atoms = (int) get_int8(ep);
	if (no_atoms < 0 || ERTS_ATOM_CACHE_SIZE < no_atoms)
	    ERTS_EXT_HDR_FAIL;
	ep++;
	if (no_atoms) {
	    int long_atoms = 0;
#ifdef DEBUG
	    byte *flgs_buf = ep;
#endif
	    byte *flgsp = ep;
	    int flgs_size = ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(no_atoms);
	    int byte_ix;
	    int bit_ix;
	    int got_flgs;
	    register Uint32 flgs = 0;

	    CHKSIZE(flgs_size);
	    ep += flgs_size;

	    /*
	     * Check long atoms flag
	     */
	    byte_ix = ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(no_atoms);
	    bit_ix = ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(no_atoms);
	    if (flgsp[byte_ix] & (((byte) ERTS_DIST_HDR_LONG_ATOMS_FLG) << bit_ix))
		long_atoms = 1;

#ifdef DEBUG
	    byte_ix = 0;
	    bit_ix = 0;
#endif
	    got_flgs = 0;
	    /*
	     * Setup the atom translation table.
	     */
	    edep->flags |= ERTS_DIST_EXT_ATOM_TRANS_TAB;
	    edep->attab.size = no_atoms;
	    for (tix = 0; tix < no_atoms; tix++) {
		Eterm atom;
		int cix;
		int len;

		if (!got_flgs) {
		    int left = no_atoms - tix;
		    if (left > 6) {
			flgs = ((((Uint32) flgsp[3]) << 24)
		        	| (((Uint32) flgsp[2]) << 16)
				| (((Uint32) flgsp[1]) << 8)
				| ((Uint32) flgsp[0]));
			flgsp += 4;
		    }
		    else {
			flgs = 0;
			switch (left) {
			case 6:
			case 5:
			    flgs |= (((Uint32) flgsp[2]) << 16);
			case 4:
			case 3:
			    flgs |= (((Uint32) flgsp[1]) << 8);
			case 2:
			case 1:
			    flgs |= ((Uint32) flgsp[0]);
			}
		    }
		    got_flgs = 8;
		}

		ASSERT(byte_ix == ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(tix));
		ASSERT(bit_ix == ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(tix));
		ASSERT((flgs & 3)
		       == (((flgs_buf[byte_ix]
			     & (((byte) 3) << bit_ix)) >> bit_ix) & 3));

		CHKSIZE(1);
		cix = (int) ((flgs & 7) << 8);
		if ((flgs & 8) == 0) {
		    /* atom already cached */
		    cix += (int) get_int8(ep);
		    if (cix >= ERTS_ATOM_CACHE_SIZE)
			ERTS_EXT_HDR_FAIL;
		    ep++;
		    atom = cache->in_arr[cix];
		    if (!is_atom(atom))
			ERTS_EXT_HDR_FAIL;
		    edep->attab.atom[tix] = atom;
		}
		else {
		    /* new cached atom */
		    cix += (int) get_int8(ep);
		    if (cix >= ERTS_ATOM_CACHE_SIZE)
			ERTS_EXT_HDR_FAIL;
		    ep++;
		    if (long_atoms) {
			CHKSIZE(2);
			len = get_int16(ep);
			ep += 2;
		    }
		    else {
			CHKSIZE(1);
			len = get_int8(ep);
			ep++;
		    }
		    CHKSIZE(len);
		    atom = erts_atom_put((byte *) ep,
					 len,
					 (utf8_atoms
					  ? ERTS_ATOM_ENC_UTF8
					  : ERTS_ATOM_ENC_LATIN1),
					 0);
		    if (is_non_value(atom))
			ERTS_EXT_HDR_FAIL;
		    ep += len;
		    cache->in_arr[cix] = atom;
		    edep->attab.atom[tix] = atom;
		}
		flgs >>= 4;
		got_flgs--;
#ifdef DEBUG
		bit_ix += 4;
		if (bit_ix >= 8) {
		    bit_ix = 0;
		    flgs = (int) flgs_buf[++byte_ix];
		    ASSERT(byte_ix < flgs_size);
		}
#endif
	    }
	}
	edep->extp = ep;
#ifdef ERTS_DEBUG_USE_DIST_SEP
	if (*ep != VERSION_MAGIC)
	    ERTS_EXT_HDR_FAIL;
#endif
    }
#ifdef ERTS_DEBUG_USE_DIST_SEP
    if (*ep != VERSION_MAGIC)
	ERTS_EXT_FAIL;
#endif

    return 0;

#undef CHKSIZE
#undef ERTS_EXT_FAIL
#undef ERTS_EXT_HDR_FAIL

 bad_hdr:
    if (dep) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "%T got a corrupted distribution header from %T "
		      "on distribution channel %d\n",
		      erts_this_node->sysname,
		      edep->dep->sysname,
		      dist_entry_channel_no(edep->dep));
	for (ep = ext; ep < edep->ext_endp; ep++)
	    erts_dsprintf(dsbufp, ep != ext ? ",%b8u" : "<<%b8u", *ep);
	erts_dsprintf(dsbufp, ">>");
	erts_send_warning_to_logger_nogl(dsbufp);
    }
 fail:
    if (dep)
	erts_kill_dist_connection(dep, dep->connection_id);
    return -1;
}

static void
bad_dist_ext(ErtsDistExternal *edep)
{
    if (edep->dep) {
	DistEntry *dep = edep->dep;
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	byte *ep;
	erts_dsprintf(dsbufp,
		      "%T got a corrupted external term from %T "
		      "on distribution channel %d\n",
		      erts_this_node->sysname,
		      dep->sysname,
		      dist_entry_channel_no(dep));
	for (ep = edep->extp; ep < edep->ext_endp; ep++)
	    erts_dsprintf(dsbufp,
			  ep != edep->extp ? ",%b8u" : "<<...,%b8u",
			  *ep);
	erts_dsprintf(dsbufp, ">>\n");
	erts_dsprintf(dsbufp, "ATOM_CACHE_REF translations: ");
	if (!(edep->flags & ERTS_DIST_EXT_ATOM_TRANS_TAB) || !edep->attab.size)
	    erts_dsprintf(dsbufp, "none");
	else {
	    int i;
	    erts_dsprintf(dsbufp, "0=%T", edep->attab.atom[0]);
	    for (i = 1; i < edep->attab.size; i++)
		erts_dsprintf(dsbufp, ", %d=%T", i, edep->attab.atom[i]);
	}
	erts_send_warning_to_logger_nogl(dsbufp);
	erts_kill_dist_connection(dep, ERTS_DIST_EXT_CON_ID(edep));
    }
}

Sint
erts_decode_dist_ext_size(ErtsDistExternal *edep)
{
    Sint res;
    byte *ep;
    if (edep->extp >= edep->ext_endp)
	goto fail;
#ifndef ERTS_DEBUG_USE_DIST_SEP
    if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR) {
	if (*edep->extp == VERSION_MAGIC)
	    goto fail;
	ep = edep->extp;
    }
    else
#endif
    {
	if (*edep->extp != VERSION_MAGIC)
	    goto fail;
	ep = edep->extp+1;
    }
    res = decoded_size(ep, edep->ext_endp, 0, NULL);
    if (res >= 0)
	return res;
 fail:
    bad_dist_ext(edep);
    return -1;
}

Sint erts_decode_ext_size(byte *ext, Uint size)
{
    if (size == 0 || *ext != VERSION_MAGIC)
	return -1;
    return decoded_size(ext+1, ext+size, 0, NULL);
}

Sint erts_decode_ext_size_ets(byte *ext, Uint size)
{
    Sint sz = decoded_size(ext, ext+size, 1, NULL);
    ASSERT(sz >= 0);
    return sz;
}


/*
** hpp is set to either a &p->htop or
** a pointer to a memory pointer (form message buffers)
** on return hpp is updated to point after allocated data
*/
Eterm
erts_decode_dist_ext(Eterm** hpp,
		     ErlOffHeap* off_heap,
		     ErtsDistExternal *edep)
{
    Eterm obj;
    byte* ep = edep->extp;

    if (ep >= edep->ext_endp)
	goto error;
#ifndef ERTS_DEBUG_USE_DIST_SEP
    if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR) {
	if (*ep == VERSION_MAGIC)
	    goto error;
    }
    else
#endif
    {
	if (*ep != VERSION_MAGIC)
	    goto error;
	ep++;
    }
    ep = dec_term(edep, hpp, ep, off_heap, &obj, NULL);
    if (!ep)
	goto error;

    edep->extp = ep;

    return obj;

 error:

    bad_dist_ext(edep);

    return THE_NON_VALUE;
}

Eterm erts_decode_ext(Eterm **hpp, ErlOffHeap *off_heap, byte **ext)
{
    Eterm obj;
    byte *ep = *ext;
    if (*ep++ != VERSION_MAGIC)
	return THE_NON_VALUE;
    ep = dec_term(NULL, hpp, ep, off_heap, &obj, NULL);
    if (!ep) {
#ifdef DEBUG
	bin_write(ERTS_PRINT_STDERR,NULL,*ext,500);
#endif
	return THE_NON_VALUE;
    }
    *ext = ep;
    return obj;
}

Eterm erts_decode_ext_ets(Eterm **hpp, ErlOffHeap *off_heap, byte *ext)
{
    Eterm obj;
    ext = dec_term(NULL, hpp, ext, off_heap, &obj, NULL);
    ASSERT(ext);
    return obj;
}

/**********************************************************************/

BIF_RETTYPE erts_debug_dist_ext_to_term_2(BIF_ALIST_2)
{
    Eterm res;
    Eterm *hp;
    Eterm *hendp;
    Sint hsz;
    ErtsDistExternal ede;
    Eterm *tp;
    Eterm real_bin;
    Uint offset;
    Uint size;
    Uint bitsize;
    Uint bitoffs;
    Uint arity;
    int i;

    ede.flags = ERTS_DIST_EXT_ATOM_TRANS_TAB;
    ede.dep = NULL;
    ede.heap_size = -1;
    
    if (is_not_tuple(BIF_ARG_1))
	goto badarg;
    tp = tuple_val(BIF_ARG_1);
    arity = arityval(tp[0]);
    if (arity > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES)
	goto badarg;

    ede.attab.size = arity;
    for (i = 1; i <= arity; i++) {
	if (is_not_atom(tp[i]))
	    goto badarg;
	ede.attab.atom[i-1] = tp[i];
    }

    if (is_not_binary(BIF_ARG_2))
	goto badarg;

    size = binary_size(BIF_ARG_2);
    if (size == 0)
	goto badarg;
    ERTS_GET_REAL_BIN(BIF_ARG_2, real_bin, offset, bitoffs, bitsize);
    if (bitsize != 0)
	goto badarg;

    ede.extp = binary_bytes(real_bin)+offset;
    ede.ext_endp = ede.extp + size;

    hsz = erts_decode_dist_ext_size(&ede);
    if (hsz < 0)
	goto badarg;

    hp = HAlloc(BIF_P, (Uint) hsz);
    hendp = hp + hsz;

    res = erts_decode_dist_ext(&hp, &MSO(BIF_P), &ede);

    HRelease(BIF_P, hendp, hp);

    if (is_value(res))
	BIF_RET(res);

 badarg:

    BIF_ERROR(BIF_P, BADARG);
}

static BIF_RETTYPE term_to_binary_trap_1(BIF_ALIST_1)
{
    Eterm *tp = tuple_val(BIF_ARG_1);
    Eterm Term = tp[1];
    Eterm bt = tp[2];
    Binary *bin = ((ProcBin *) binary_val(bt))->val;
    Eterm res = erts_term_to_binary_int(BIF_P, Term, 0, 0,bin);
    if (is_tuple(res)) {
	ASSERT(BIF_P->flags & F_DISABLE_GC);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
        if (erts_set_gc_state(BIF_P, 1)
            || MSO(BIF_P).overhead > BIN_VHEAP_SZ(BIF_P))
            ERTS_BIF_YIELD_RETURN(BIF_P, res);
        else
            BIF_RET(res);
    }
}

BIF_RETTYPE term_to_binary_1(BIF_ALIST_1)
{
    Eterm res = erts_term_to_binary_int(BIF_P, BIF_ARG_1, 0, TERM_TO_BINARY_DFLAGS, NULL);
    if (is_tuple(res)) {
	erts_set_gc_state(BIF_P, 0);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
	BIF_RET(res);
    }
}

BIF_RETTYPE term_to_binary_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm Term = BIF_ARG_1;
    Eterm Flags = BIF_ARG_2;
    int level = 0;
    Uint flags = TERM_TO_BINARY_DFLAGS;
    Eterm res;

    while (is_list(Flags)) {
	Eterm arg = CAR(list_val(Flags));
	Eterm* tp;
	if (arg == am_compressed) {
	    level = Z_DEFAULT_COMPRESSION;
	} else if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
	    if (tp[1] == am_minor_version && is_small(tp[2])) {
		switch (signed_val(tp[2])) {
		case 0:
		    flags = TERM_TO_BINARY_DFLAGS;
		    break;
		case 1:
		    flags = TERM_TO_BINARY_DFLAGS|DFLAG_NEW_FLOATS;
		    break;
		default:
		    goto error;
		}
	    } else if (tp[1] == am_compressed && is_small(tp[2])) {
		level = signed_val(tp[2]);
		if (!(0 <= level && level < 10)) {
		    goto error;
		}
	    } else {
		goto error;
	    }
	} else {
	error:
	    BIF_ERROR(p, BADARG);
	}
	Flags = CDR(list_val(Flags));
    }
    if (is_not_nil(Flags)) {
	goto error;
    }

    res = erts_term_to_binary_int(p, Term, level, flags, NULL);
    if (is_tuple(res)) {
	erts_set_gc_state(p, 0);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
	BIF_RET(res);
    }
}


enum B2TState { /* order is somewhat significant */
    B2TPrepare,
    B2TUncompressChunk,
    B2TSizeInit,
    B2TSize,
    B2TDecodeInit,
    B2TDecode,
    B2TDecodeList,
    B2TDecodeTuple,
    B2TDecodeString,
    B2TDecodeBinary,

    B2TDone,
    B2TDecodeFail,
    B2TBadArg
};

typedef struct {
    int heap_size;
    int terms;
    byte* ep;
    int atom_extra_skip;
} B2TSizeContext;

typedef struct {
    byte*  ep;
    Eterm  res;
    Eterm* next;
    Eterm* hp_start;
    Eterm* hp;
    Eterm* hp_end;
    int remaining_n;
    char* remaining_bytes;
    Eterm* maps_head;
} B2TDecodeContext;

typedef struct {
    z_stream stream;
    byte* dbytes;
    Uint dleft;
} B2TUncompressContext;

typedef struct B2TContext_t {
    Sint heap_size;
    byte* aligned_alloc;
    ErtsBinary2TermState b2ts;
    Uint32 flags;
    SWord reds;
    Eterm trap_bin;
    enum B2TState state;
    union {
	B2TSizeContext sc;
	B2TDecodeContext dc;
	B2TUncompressContext uc;
    } u;
} B2TContext;


static uLongf binary2term_uncomp_size(byte* data, Sint size)
{
    z_stream stream;
    int err;
    const uInt chunk_size = 64*1024;  /* Ask tmp-alloc about a suitable size? */
    void* tmp_buf = erts_alloc(ERTS_ALC_T_TMP, chunk_size);
    uLongf uncomp_size = 0;

    stream.next_in = (Bytef*)data;
    stream.avail_in = (uInt)size;
    stream.next_out = tmp_buf;
    stream.avail_out = (uInt)chunk_size;

    erl_zlib_alloc_init(&stream);

    err = inflateInit(&stream);
    if (err == Z_OK) {
	do {
	    stream.next_out = tmp_buf;
	    stream.avail_out = chunk_size;	   
	    err = inflate(&stream, Z_NO_FLUSH);
	    uncomp_size += chunk_size - stream.avail_out;
	}while (err == Z_OK);
	inflateEnd(&stream);
    }
    erts_free(ERTS_ALC_T_TMP, tmp_buf);
    return err == Z_STREAM_END ? uncomp_size : 0;
}

static ERTS_INLINE int
binary2term_prepare(ErtsBinary2TermState *state, byte *data, Sint data_size,
		    B2TContext* ctx)
{
    byte *bytes = data;
    Sint size = data_size;

    state->exttmp = 0;

    if (size < 1 || *bytes != VERSION_MAGIC) {
	return -1;
    }
    bytes++;
    size--;
    if (size < 5 || *bytes != COMPRESSED) {
	state->extp = bytes;
        if (ctx)
	    ctx->state = B2TSizeInit;
    }
    else  {
	uLongf dest_len = (Uint32) get_int32(bytes+1);
	bytes += 5;
	size -= 5;	
	if (dest_len > 32*1024*1024
	    || (state->extp = erts_alloc_fnf(ERTS_ALC_T_EXT_TERM_DATA, dest_len)) == NULL) {
            /*
             * Try avoid out-of-memory crash due to corrupted 'dest_len'
             * by checking the actual length of the uncompressed data.
             * The only way to do that is to uncompress it. Sad but true.
             */
	    if (dest_len != binary2term_uncomp_size(bytes, size)) {
                return -1;
	    }
	    state->extp = erts_alloc(ERTS_ALC_T_EXT_TERM_DATA, dest_len);
            ctx->reds -= dest_len;
	}
	state->exttmp = 1;
        if (ctx) {
	    if (erl_zlib_inflate_start(&ctx->u.uc.stream, bytes, size) != Z_OK)
		return -1;

	    ctx->u.uc.dbytes = state->extp;
	    ctx->u.uc.dleft = dest_len;
	    ctx->state = B2TUncompressChunk;
        }
	else {
	    uLongf dlen = dest_len;
	    if (erl_zlib_uncompress(state->extp, &dlen, bytes, size) != Z_OK
		|| dlen != dest_len) {
		return -1;
	    }
        }
	size = (Sint) dest_len;
    }
    state->extsize = size;
    return 0;
}

static ERTS_INLINE void
binary2term_abort(ErtsBinary2TermState *state)
{
    if (state->exttmp) {
	state->exttmp = 0;
	erts_free(ERTS_ALC_T_EXT_TERM_DATA, state->extp);
    }
}

static ERTS_INLINE Eterm
binary2term_create(ErtsDistExternal *edep, ErtsBinary2TermState *state, Eterm **hpp, ErlOffHeap *ohp)
{
    Eterm res;
    if (!dec_term(edep, hpp, state->extp, ohp, &res, NULL))
	res = THE_NON_VALUE;
    if (state->exttmp) {
	state->exttmp = 0;
	erts_free(ERTS_ALC_T_EXT_TERM_DATA, state->extp);
    }
    return res;
}

Sint
erts_binary2term_prepare(ErtsBinary2TermState *state, byte *data, Sint data_size)
{
    Sint res;

    if (binary2term_prepare(state, data, data_size, NULL) < 0 ||
        (res=decoded_size(state->extp, state->extp + state->extsize, 0, NULL)) < 0) {

        if (state->exttmp)
            erts_free(ERTS_ALC_T_EXT_TERM_DATA, state->extp);
        state->extp = NULL;
	state->exttmp = 0;
	return -1;
    }
    return res;
}

void
erts_binary2term_abort(ErtsBinary2TermState *state)
{
    binary2term_abort(state);
}

Eterm
erts_binary2term_create(ErtsBinary2TermState *state, Eterm **hpp, ErlOffHeap *ohp)
{
    return binary2term_create(NULL,state, hpp, ohp);
}

static void b2t_destroy_context(B2TContext* context)
{
    erts_free_aligned_binary_bytes_extra(context->aligned_alloc,
                                         ERTS_ALC_T_EXT_TERM_DATA);
    context->aligned_alloc = NULL;
    binary2term_abort(&context->b2ts);
    if (context->state == B2TUncompressChunk) {
	erl_zlib_inflate_finish(&context->u.uc.stream);
    }
}

static void b2t_context_destructor(Binary *context_bin)
{
    B2TContext* ctx = (B2TContext*) ERTS_MAGIC_BIN_DATA(context_bin);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(context_bin) == b2t_context_destructor);

    b2t_destroy_context(ctx);
}

static BIF_RETTYPE binary_to_term_trap_1(BIF_ALIST_1)
{
    Binary *context_bin = ((ProcBin *) binary_val(BIF_ARG_1))->val;
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(context_bin) == b2t_context_destructor);

    return binary_to_term_int(BIF_P, 0, THE_NON_VALUE, context_bin);
}


#define B2T_BYTES_PER_REDUCTION 128
#define B2T_MEMCPY_FACTOR 8

/* Define for testing */
/*#define EXTREME_B2T_TRAPPING 1*/

#ifdef EXTREME_B2T_TRAPPING
static unsigned b2t_rand(void)
{
    static unsigned prev = 17;
    prev = (prev * 214013 + 2531011);
    return prev;
}
#endif


static B2TContext* b2t_export_context(Process* p, B2TContext* src)
{
    Binary* context_b = erts_create_magic_binary(sizeof(B2TContext),
                                                 b2t_context_destructor);
    B2TContext* ctx = ERTS_MAGIC_BIN_DATA(context_b);
    Eterm* hp;
    sys_memcpy(ctx, src, sizeof(B2TContext));
    if (ctx->state >= B2TDecode && ctx->u.dc.next == &src->u.dc.res) {
        ctx->u.dc.next = &ctx->u.dc.res;
    }
    hp = HAlloc(p, PROC_BIN_SIZE);
    ctx->trap_bin = erts_mk_magic_binary_term(&hp, &MSO(p), context_b);
    return ctx;
}

static Eterm binary_to_term_int(Process* p, Uint32 flags, Eterm bin, Binary* context_b)
{
#ifdef EXTREME_B2T_TRAPPING
    SWord initial_reds = 1 + b2t_rand() % 4;
#else
    SWord initial_reds = (Uint)(ERTS_BIF_REDS_LEFT(p) * B2T_BYTES_PER_REDUCTION);
#endif
    B2TContext c_buff;
    B2TContext *ctx;
    int is_first_call;

    if (context_b == NULL) {
	/* Setup enough to get started */
        is_first_call = 1;
        ctx = &c_buff;
	ctx->state = B2TPrepare;
        ctx->aligned_alloc = NULL;
        ctx->flags = flags;
        IF_DEBUG(ctx->trap_bin = THE_NON_VALUE;)
    } else {
        is_first_call = 0;
	ctx = ERTS_MAGIC_BIN_DATA(context_b);
        ASSERT(ctx->state != B2TPrepare);
    }
    ctx->reds = initial_reds;

    do {
        switch (ctx->state) {
        case B2TPrepare: {
	    byte* bytes;
            Uint bin_size;
            bytes = erts_get_aligned_binary_bytes_extra(bin,
                                                        &ctx->aligned_alloc,
                                                        ERTS_ALC_T_EXT_TERM_DATA,
                                                        0);
            if (bytes == NULL) {
                ctx->b2ts.exttmp = 0;
                ctx->state = B2TBadArg;
                break;
            }
            bin_size = binary_size(bin);
            if (ctx->aligned_alloc) {
                ctx->reds -= bin_size / 8;
            }
            if (binary2term_prepare(&ctx->b2ts, bytes, bin_size, ctx) < 0) {
		ctx->state = B2TBadArg;
	    }
            break;
        }
	case B2TUncompressChunk: {
            uLongf chunk = ctx->reds;
            int zret;

            if (chunk > ctx->u.uc.dleft)
                chunk = ctx->u.uc.dleft;
            zret = erl_zlib_inflate_chunk(&ctx->u.uc.stream,
                                          ctx->u.uc.dbytes, &chunk);
            ctx->u.uc.dbytes += chunk;
            ctx->u.uc.dleft  -= chunk;
            if (zret == Z_OK && ctx->u.uc.dleft > 0) {
                ctx->reds = 0;
            }
            else if (erl_zlib_inflate_finish(&ctx->u.uc.stream) == Z_OK
                     && zret == Z_STREAM_END
                     && ctx->u.uc.dleft == 0) {
                ctx->reds -= chunk;
                ctx->state = B2TSizeInit;
            }
            else {
                ctx->state = B2TBadArg;
            }
            break;
        }
	case B2TSizeInit:
	    ctx->u.sc.ep = NULL;
	    ctx->state = B2TSize;
	    /*fall through*/
        case B2TSize:
            ctx->heap_size = decoded_size(ctx->b2ts.extp,
					  ctx->b2ts.extp + ctx->b2ts.extsize,
                                          0, ctx);
            break;

        case B2TDecodeInit:
            if (ctx == &c_buff && ctx->b2ts.extsize > ctx->reds) {
                /* dec_term will maybe trap, allocate space for magic bin
                   before result term to make it easy to trim with HRelease.
                 */
                ctx = b2t_export_context(p, &c_buff);
            }
            ctx->u.dc.ep = ctx->b2ts.extp;
            ctx->u.dc.res = (Eterm) (UWord) NULL;
            ctx->u.dc.next = &ctx->u.dc.res;
            ctx->u.dc.hp_start = HAlloc(p, ctx->heap_size);
            ctx->u.dc.hp       = ctx->u.dc.hp_start;
            ctx->u.dc.hp_end   = ctx->u.dc.hp_start + ctx->heap_size;
	    ctx->u.dc.maps_head = NULL;
            ctx->state = B2TDecode;
            /*fall through*/
	case B2TDecode:
        case B2TDecodeList:
        case B2TDecodeTuple:
        case B2TDecodeString:
        case B2TDecodeBinary: {
	    ErtsDistExternal fakedep;
            fakedep.flags = ctx->flags;
            dec_term(&fakedep, NULL, NULL, &MSO(p), NULL, ctx);
            break;
	}
        case B2TDecodeFail:
            HRelease(p, ctx->u.dc.hp_end, ctx->u.dc.hp_start);
            /*fall through*/
        case B2TBadArg:
            b2t_destroy_context(ctx);
            if (!is_first_call) {
                erts_set_gc_state(p, 1);
            }
            BUMP_REDS(p, (initial_reds - ctx->reds) / B2T_BYTES_PER_REDUCTION);
            BIF_ERROR(p, BADARG & ~EXF_SAVETRACE);

        case B2TDone:
            b2t_destroy_context(ctx);

            if (ctx->u.dc.hp > ctx->u.dc.hp_end) {
                erl_exit(1, ":%s, line %d: heap overrun by %d words(s)\n",
                         __FILE__, __LINE__, ctx->u.dc.hp - ctx->u.dc.hp_end);
            }
            HRelease(p, ctx->u.dc.hp_end, ctx->u.dc.hp);

            if (!is_first_call) {
                erts_set_gc_state(p, 1);
            }
            BUMP_REDS(p, (initial_reds - ctx->reds) / B2T_BYTES_PER_REDUCTION);
            return ctx->u.dc.res;

        default:
            ASSERT(!"Unknown state in binary_to_term");
        }
    }while (ctx->reds > 0 || ctx->state >= B2TDone);

    if (ctx == &c_buff) {
        ASSERT(ctx->trap_bin == THE_NON_VALUE);
        ctx = b2t_export_context(p, &c_buff);
    }
    ASSERT(ctx->trap_bin != THE_NON_VALUE);

    if (is_first_call) {
        erts_set_gc_state(p, 0);
    }
    BUMP_ALL_REDS(p);
    BIF_TRAP1(&binary_to_term_trap_export, p, ctx->trap_bin);
}

BIF_RETTYPE erts_internal_binary_to_term_1(BIF_ALIST_1)
{
    return binary_to_term_int(BIF_P, 0, BIF_ARG_1, NULL);
}

BIF_RETTYPE erts_internal_binary_to_term_2(BIF_ALIST_2)
{
    Eterm opts;
    Eterm opt;
    Uint32 flags = 0;

    opts = BIF_ARG_2;
    while (is_list(opts)) {
        opt = CAR(list_val(opts));
        if (opt == am_safe) {
            flags |= ERTS_DIST_EXT_BTT_SAFE;
        }
	else {
            goto error;
        }
        opts = CDR(list_val(opts));
    }

    if (is_not_nil(opts))
        goto error;

    return binary_to_term_int(BIF_P, flags, BIF_ARG_1, NULL);

error:
    BIF_ERROR(BIF_P, BADARG);
}

Eterm
external_size_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm Term = BIF_ARG_1;

    Uint size = erts_encode_ext_size(Term);
    if (IS_USMALL(0, size)) {
	BIF_RET(make_small(size));
    } else {
	Eterm* hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(size, hp));
    }
}

Eterm
external_size_2(BIF_ALIST_2)
{
    Uint size;
    Uint flags = TERM_TO_BINARY_DFLAGS;

    while (is_list(BIF_ARG_2)) {
        Eterm arg = CAR(list_val(BIF_ARG_2));
        Eterm* tp;

        if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
            if (tp[1] == am_minor_version && is_small(tp[2])) {
                switch (signed_val(tp[2])) {
                case 0:
                    break;
                case 1:
                    flags |= DFLAG_NEW_FLOATS;
                    break;
                default:
                    goto error;
                }
            } else {
                goto error;
            }
        } else {
        error:
            BIF_ERROR(BIF_P, BADARG);
        }
        BIF_ARG_2 = CDR(list_val(BIF_ARG_2));
    }
    if (is_not_nil(BIF_ARG_2)) {
        goto error;
    }

    size = erts_encode_ext_size_2(BIF_ARG_1, flags);
    if (IS_USMALL(0, size)) {
        BIF_RET(make_small(size));
    } else {
        Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
        BIF_RET(uint_to_big(size, hp));
    }
}

static Eterm
erts_term_to_binary_simple(Process* p, Eterm Term, Uint size, int level, Uint flags)
{
    Eterm bin;
    size_t real_size;
    byte* endp;

    if (level != 0) {
	byte buf[256];
	byte* bytes = buf;
	byte* out_bytes;
	uLongf dest_len;

	if (sizeof(buf) < size) {
	    bytes = erts_alloc(ERTS_ALC_T_TMP, size);
	}

	if ((endp = enc_term(NULL, Term, bytes, flags, NULL))
	    == NULL) {
	    erl_exit(1, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		     __FILE__, __LINE__, real_size - size);
	}

	/*
	 * We don't want to compress if compression actually increases the size.
	 * Therefore, don't give zlib more out buffer than the size of the
	 * uncompressed external format (minus the 5 bytes needed for the
	 * COMPRESSED tag). If zlib returns any error, we'll revert to using
	 * the original uncompressed external term format.
	 */

	if (real_size < 5) {
	    dest_len = 0;
	} else {
	    dest_len = real_size - 5;
	}
	bin = new_binary(p, NULL, real_size+1);
	out_bytes = binary_bytes(bin);
	out_bytes[0] = VERSION_MAGIC;
	if (erl_zlib_compress2(out_bytes+6, &dest_len, bytes, real_size, level) != Z_OK) {
	    sys_memcpy(out_bytes+1, bytes, real_size);
	    bin = erts_realloc_binary(bin, real_size+1);
	} else {
	    out_bytes[1] = COMPRESSED;
	    put_int32(real_size, out_bytes+2);
	    bin = erts_realloc_binary(bin, dest_len+6);
	}
	if (bytes != buf) {
	    erts_free(ERTS_ALC_T_TMP, bytes);
	}
	return bin;
    } else {
	byte* bytes;

	bin = new_binary(p, (byte *)NULL, size);
	bytes = binary_bytes(bin);
	bytes[0] = VERSION_MAGIC;
	if ((endp = enc_term(NULL, Term, bytes+1, flags, NULL))
	    == NULL) {
	    erl_exit(1, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		     __FILE__, __LINE__, endp - (bytes + size));
	}
	return erts_realloc_binary(bin, real_size);
    }
}

Eterm
erts_term_to_binary(Process* p, Eterm Term, int level, Uint flags) {
    Uint size;
    size = encode_size_struct2(NULL, Term, flags) + 1 /* VERSION_MAGIC */;
    return erts_term_to_binary_simple(p, Term, size, level, flags);
}

/* Define for testing */
/* #define EXTREME_TTB_TRAPPING 1 */

#ifndef EXTREME_TTB_TRAPPING
#define TERM_TO_BINARY_LOOP_FACTOR 32
#define TERM_TO_BINARY_COMPRESS_CHUNK (1 << 18)
#else
#define TERM_TO_BINARY_LOOP_FACTOR 1
#define TERM_TO_BINARY_COMPRESS_CHUNK 10
#endif


typedef enum { TTBSize, TTBEncode, TTBCompress } TTBState;
typedef struct TTBSizeContext_ {
    Uint flags;
    int level;
    Uint result;
    Eterm obj;
    ErtsEStack estack;
} TTBSizeContext;

typedef struct TTBEncodeContext_ {
    Uint flags;
    int level;
    byte* ep;
    Eterm obj;
    ErtsWStack wstack;
    Binary *result_bin;
} TTBEncodeContext;

typedef struct {
    Uint real_size;
    Uint dest_len;
    byte *dbytes;
    Binary *result_bin;
    Binary *destination_bin;
    z_stream stream;
} TTBCompressContext;

typedef struct {
    int alive;
    TTBState state;
    union {
	TTBSizeContext sc;
	TTBEncodeContext ec;
	TTBCompressContext cc;
    } s;
} TTBContext;

static void ttb_context_destructor(Binary *context_bin)
{
    TTBContext *context = ERTS_MAGIC_BIN_DATA(context_bin);
    if (context->alive) {
	context->alive = 0;
	switch (context->state) {
	case TTBSize:
	    DESTROY_SAVED_ESTACK(&context->s.sc.estack);
	    break;
	case TTBEncode:
	    DESTROY_SAVED_WSTACK(&context->s.ec.wstack);
	    if (context->s.ec.result_bin != NULL) { /* Set to NULL if ever made alive! */
		ASSERT(erts_refc_read(&(context->s.ec.result_bin->refc),0) == 0);
		erts_bin_free(context->s.ec.result_bin);
		context->s.ec.result_bin = NULL;
	    }
	    break;
	case TTBCompress:
	    erl_zlib_deflate_finish(&(context->s.cc.stream));

	    if (context->s.cc.destination_bin != NULL) { /* Set to NULL if ever made alive! */
		ASSERT(erts_refc_read(&(context->s.cc.destination_bin->refc),0) == 0);
		erts_bin_free(context->s.cc.destination_bin);
		context->s.cc.destination_bin = NULL;
	    }
	    
	    if (context->s.cc.result_bin != NULL) { /* Set to NULL if ever made alive! */
		ASSERT(erts_refc_read(&(context->s.cc.result_bin->refc),0) == 0);
		erts_bin_free(context->s.cc.result_bin);
		context->s.cc.result_bin = NULL;
	    }
	    break;
	}
    }
}

static Eterm erts_term_to_binary_int(Process* p, Eterm Term, int level, Uint flags, 
				     Binary *context_b) 
{
    Eterm *hp;
    Eterm res;
    Eterm c_term;
#ifndef EXTREME_TTB_TRAPPING
    Sint reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * TERM_TO_BINARY_LOOP_FACTOR);
#else
    Sint reds = 20; /* For testing */
#endif
    Sint initial_reds = reds; 
    TTBContext c_buff;
    TTBContext *context = &c_buff;

#define EXPORT_CONTEXT()						\
    do {								\
	if (context_b == NULL) {					\
	    context_b = erts_create_magic_binary(sizeof(TTBContext),    \
                                                 ttb_context_destructor);   \
	    context =  ERTS_MAGIC_BIN_DATA(context_b);			\
	    memcpy(context,&c_buff,sizeof(TTBContext));			\
	}								\
    } while (0)

#define RETURN_STATE()							\
    do {								\
	hp = HAlloc(p, PROC_BIN_SIZE+3);				\
	c_term = erts_mk_magic_binary_term(&hp, &MSO(p), context_b);	\
	res = TUPLE2(hp, Term, c_term);					\
	BUMP_ALL_REDS(p);                                               \
	return res;							\
    } while (0);


    if (context_b == NULL) {
	/* Setup enough to get started */
	context->state = TTBSize;
	context->alive = 1;
	context->s.sc.estack.start = NULL;
	context->s.sc.flags = flags;
	context->s.sc.level = level;
    } else {
	context = ERTS_MAGIC_BIN_DATA(context_b);
    }	    
    /* Initialization done, now we will go through the states */
    for (;;) {
	switch (context->state) {
	case TTBSize:
	    {
		Uint size;
		Binary *result_bin;
		int level;
		Uint flags;
		/* Try for fast path */
		if (encode_size_struct_int(&context->s.sc, NULL, Term,
					   context->s.sc.flags, &reds, &size) < 0) {
		    EXPORT_CONTEXT();
		    /* Same state */
		    RETURN_STATE();
		}
		++size; /* VERSION_MAGIC */
		/* Move these to next state */
		flags = context->s.sc.flags;
		level = context->s.sc.level;
		if (size <=  ERL_ONHEAP_BIN_LIMIT) {
		    /* Finish in one go */
		    res = erts_term_to_binary_simple(p, Term, size, 
						     level, flags);
		    BUMP_REDS(p, 1);
		    return res;
		}

		result_bin = erts_bin_nrml_alloc(size);
		result_bin->flags = 0;
		result_bin->orig_size = size;
		erts_refc_init(&result_bin->refc, 0);
		result_bin->orig_bytes[0] = VERSION_MAGIC;
		/* Next state immediately, no need to export context */
		context->state = TTBEncode;
		context->s.ec.flags = flags;
		context->s.ec.level = level;
		context->s.ec.wstack.wstart = NULL;
		context->s.ec.result_bin = result_bin;
		break;
	    }
	case TTBEncode:
	    {
		byte *endp;
		byte *bytes = (byte *) context->s.ec.result_bin->orig_bytes;
		size_t real_size;
		Binary *result_bin;

		flags = context->s.ec.flags;
		if (enc_term_int(&context->s.ec, NULL,Term, bytes+1, flags, NULL, &reds, &endp) < 0) {
		    EXPORT_CONTEXT();
		    RETURN_STATE();
		}
		real_size = endp - bytes;
		result_bin = erts_bin_realloc(context->s.ec.result_bin,real_size);
		level = context->s.ec.level;
		BUMP_REDS(p, (initial_reds - reds) / TERM_TO_BINARY_LOOP_FACTOR);
		if (level == 0 || real_size < 6) { /* We are done */
		    ProcBin* pb;
		return_normal:
		    context->s.ec.result_bin = NULL;
		    context->alive = 0;
		    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = real_size;
		    pb->next = MSO(p).first;
		    MSO(p).first = (struct erl_off_heap_header*)pb;
		    pb->val = result_bin;
		    pb->bytes = (byte*) result_bin->orig_bytes;
		    pb->flags = 0;
		    OH_OVERHEAD(&(MSO(p)), pb->size / sizeof(Eterm));
		    erts_refc_inc(&result_bin->refc, 1);
		    if (context_b && erts_refc_read(&context_b->refc,0) == 0) {
			erts_bin_free(context_b);
		    }
		    return make_binary(pb);
		}
		/* Continue with compression... */
		/* To make absolutely sure that zlib does not barf on a reallocated context, 
		   we make sure it's "exported" before doing anything compession-like */
		EXPORT_CONTEXT();
		bytes = (byte *) result_bin->orig_bytes; /* result_bin is reallocated */
		if (erl_zlib_deflate_start(&(context->s.cc.stream),bytes+1,real_size-1,level) 
		    != Z_OK) {
		    goto return_normal;
		}
		context->state = TTBCompress;
		context->s.cc.real_size = real_size;
		context->s.cc.result_bin = result_bin;

		result_bin = erts_bin_nrml_alloc(real_size);
		result_bin->flags = 0;
		result_bin->orig_size = real_size;
		erts_refc_init(&result_bin->refc, 0);
		result_bin->orig_bytes[0] = VERSION_MAGIC;

		context->s.cc.destination_bin = result_bin;
		context->s.cc.dest_len = 0;
		context->s.cc.dbytes = (byte *) result_bin->orig_bytes+6;
		break;
	    }
	case TTBCompress:
	    {
		uLongf tot_dest_len = context->s.cc.real_size - 6;
		uLongf left = (tot_dest_len - context->s.cc.dest_len);
		uLongf this_time = (left > TERM_TO_BINARY_COMPRESS_CHUNK) ?  
		    TERM_TO_BINARY_COMPRESS_CHUNK : 
		    left;
		Binary *result_bin;
		ProcBin *pb;
		Uint max = (ERTS_BIF_REDS_LEFT(p) *  TERM_TO_BINARY_COMPRESS_CHUNK) / CONTEXT_REDS;

		if (max < this_time) {
		    this_time = max + 1; /* do not set this_time to 0 */
		}

		res = erl_zlib_deflate_chunk(&(context->s.cc.stream), context->s.cc.dbytes, &this_time);
		context->s.cc.dbytes += this_time;
		context->s.cc.dest_len += this_time;
		switch (res) {
		case Z_OK:
		    if (context->s.cc.dest_len >= tot_dest_len) {
			goto no_use_compressing;
		    }
		    RETURN_STATE();
		case Z_STREAM_END:
		    {
			byte *dbytes = (byte *) context->s.cc.destination_bin->orig_bytes + 1;

			dbytes[0] = COMPRESSED;
			put_int32(context->s.cc.real_size-1,dbytes+1);
			erl_zlib_deflate_finish(&(context->s.cc.stream));
			result_bin = erts_bin_realloc(context->s.cc.destination_bin,
						      context->s.cc.dest_len+6);
			context->s.cc.destination_bin = NULL;
			pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
			pb->thing_word = HEADER_PROC_BIN;
			pb->size = context->s.cc.dest_len+6;
			pb->next = MSO(p).first;
			MSO(p).first = (struct erl_off_heap_header*)pb;
			pb->val = result_bin;
			pb->bytes = (byte*) result_bin->orig_bytes;
			pb->flags = 0;
			OH_OVERHEAD(&(MSO(p)), pb->size / sizeof(Eterm));
			erts_refc_inc(&result_bin->refc, 1);
			erts_bin_free(context->s.cc.result_bin);
			context->s.cc.result_bin = NULL;
			context->alive = 0;
			BUMP_REDS(p, (this_time * CONTEXT_REDS) / TERM_TO_BINARY_COMPRESS_CHUNK);
			if (context_b && erts_refc_read(&context_b->refc,0) == 0) {
			    erts_bin_free(context_b);
			}
			return make_binary(pb);
		    }
		default: /* Compression error, revert to uncompressed binary (still in 
			    context) */
		no_use_compressing:
		    result_bin = context->s.cc.result_bin;
		    context->s.cc.result_bin = NULL;
		    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = context->s.cc.real_size;
		    pb->next = MSO(p).first;
		    MSO(p).first = (struct erl_off_heap_header*)pb;
		    pb->val = result_bin;
		    pb->bytes = (byte*) result_bin->orig_bytes;
		    pb->flags = 0;
		    OH_OVERHEAD(&(MSO(p)), pb->size / sizeof(Eterm));
		    erts_refc_inc(&result_bin->refc, 1);
		    erl_zlib_deflate_finish(&(context->s.cc.stream));
		    erts_bin_free(context->s.cc.destination_bin);
		    context->s.cc.destination_bin = NULL;
		    context->alive = 0;
		    BUMP_REDS(p, (this_time * CONTEXT_REDS) / TERM_TO_BINARY_COMPRESS_CHUNK);
		    if (context_b && erts_refc_read(&context_b->refc,0) == 0) {
			erts_bin_free(context_b);
		    }
		    return make_binary(pb);
		}
	    }
	}
    }
#undef EXPORT_CONTEXT
#undef RETURN_STATE
}			








/*
 * This function fills ext with the external format of atom.
 * If it's an old atom we just supply an index, otherwise
 * we insert the index _and_ the entire atom. This way the receiving side
 * does not have to perform an hash on the etom to locate it, and
 * we save a lot of space on the wire.
 */

static byte*
enc_atom(ErtsAtomCacheMap *acmp, Eterm atom, byte *ep, Uint32 dflags)
{
    int iix;
    int len;
    int utf8_atoms = (int) (dflags & DFLAG_UTF8_ATOMS);

    ASSERT(is_atom(atom));

    if (dflags & DFLAG_INTERNAL_TAGS) {
	Uint aval = atom_val(atom);
	ASSERT(aval < (1<<24));
	if (aval >= (1 << 16)) {
	    *ep++ = ATOM_INTERNAL_REF3;
	    put_int24(aval, ep);
	    ep += 3;
	}
	else {
	    *ep++ = ATOM_INTERNAL_REF2;
	    put_int16(aval, ep);
	    ep += 2;
	}
	return ep;
    }

    /*
     * term_to_binary/1,2 and the initial distribution message
     * don't use the cache.
     */

    iix = get_iix_acache_map(acmp, atom, dflags);
    if (iix < 0) {
	Atom *a = atom_tab(atom_val(atom));
	len = a->len;
	if (utf8_atoms || a->latin1_chars < 0) {
	    if (len > 255) {
		*ep++ = ATOM_UTF8_EXT;
		put_int16(len, ep);
		ep += 2;
	    }
	    else {
		*ep++ = SMALL_ATOM_UTF8_EXT;
		put_int8(len, ep);
		ep += 1;
	    }
	    sys_memcpy((char *) ep, (char *) a->name, len);
	}
	else {
	    if (a->latin1_chars <= 255 && (dflags & DFLAG_SMALL_ATOM_TAGS)) {
		*ep++ = SMALL_ATOM_EXT;
		if (len == a->latin1_chars) {
		    sys_memcpy(ep+1, a->name, len);
		}
		else {
		    len = erts_utf8_to_latin1(ep+1, a->name, len);
		    ASSERT(len == a->latin1_chars);
		}
		put_int8(len, ep);
		ep++;
	    }
	    else {
		*ep++ = ATOM_EXT;
		if (len == a->latin1_chars) {
		    sys_memcpy(ep+2, a->name, len);
		}
		else {
		    len = erts_utf8_to_latin1(ep+2, a->name, len);
		    ASSERT(len == a->latin1_chars);
		}
		put_int16(len, ep);
		ep += 2;
	    }	    
	}
	ep += len;
	return ep;
    }

    /* The atom is referenced in the cache. */
    *ep++ = ATOM_CACHE_REF;
    put_int8(iix, ep);
    ep++;
    return ep;
}

static byte*
enc_pid(ErtsAtomCacheMap *acmp, Eterm pid, byte* ep, Uint32 dflags)
{
    Uint on, os;

    *ep++ = PID_EXT;
    /* insert  atom here containing host and sysname  */
    ep = enc_atom(acmp, pid_node_name(pid), ep, dflags);

    /* two bytes for each number and serial */

    on = pid_number(pid);
    os = pid_serial(pid);

    put_int32(on, ep);
    ep += 4;
    put_int32(os, ep);
    ep += 4;
    *ep++ = (is_internal_pid(pid) && (dflags & DFLAG_INTERNAL_TAGS)) ?
	INTERNAL_CREATION : pid_creation(pid);
    return ep;
}

/* Expect an atom in plain text or cached */
static byte*
dec_atom(ErtsDistExternal *edep, byte* ep, Eterm* objp)
{
    Uint len;
    int n;
    ErtsAtomEncoding char_enc;

    switch (*ep++) {
    case ATOM_CACHE_REF:
	if (!(edep && (edep->flags & ERTS_DIST_EXT_ATOM_TRANS_TAB)))
	    goto error;
	n = get_int8(ep);
	ep++;
	if (n >= edep->attab.size)
	    goto error;
	ASSERT(is_atom(edep->attab.atom[n]));
	*objp = edep->attab.atom[n];
	break;
    case ATOM_EXT:
	len = get_int16(ep),
	ep += 2;
	char_enc = ERTS_ATOM_ENC_LATIN1;
        goto dec_atom_common;
    case SMALL_ATOM_EXT:
	len = get_int8(ep);
	ep++;
	char_enc = ERTS_ATOM_ENC_LATIN1;
	goto dec_atom_common;
    case ATOM_UTF8_EXT:
	len = get_int16(ep),
	ep += 2;
	char_enc = ERTS_ATOM_ENC_UTF8;
	goto dec_atom_common;
    case SMALL_ATOM_UTF8_EXT:
	len = get_int8(ep),
	ep++;
	char_enc = ERTS_ATOM_ENC_UTF8;
    dec_atom_common:
        if (edep && (edep->flags & ERTS_DIST_EXT_BTT_SAFE)) {
	    if (!erts_atom_get((char*)ep, len, objp, char_enc)) {
                goto error;
	    }
        } else {
	    Eterm atom = erts_atom_put(ep, len, char_enc, 0);
	    if (is_non_value(atom))
		goto error;
            *objp = atom;
        }
	ep += len;
	break;
    case ATOM_INTERNAL_REF2:
	n = get_int16(ep);
	ep += 2;
	if (n >= atom_table_size()) {
	    goto error;
	}
	*objp = make_atom(n);
	break;
    case ATOM_INTERNAL_REF3:
	n = get_int24(ep);
	ep += 3;
	if (n >= atom_table_size()) {
	    goto error;
	}
	*objp = make_atom(n);
	break;

    default:
    error:
	*objp = NIL;	/* Don't leave a hole in the heap */
	return NULL;
    }
    return ep;
}

static ERTS_INLINE ErlNode* dec_get_node(Eterm sysname, Uint creation)
{
    switch (creation) {
    case INTERNAL_CREATION:
	return erts_this_node;
    case ORIG_CREATION:
	if (sysname == erts_this_node->sysname) {
	    creation = erts_this_node->creation;
	}
    }
    return erts_find_or_insert_node(sysname,creation);
}

static byte*
dec_pid(ErtsDistExternal *edep, Eterm** hpp, byte* ep, ErlOffHeap* off_heap, Eterm* objp)
{
    Eterm sysname;
    Uint data;
    Uint num;
    Uint ser;
    Uint cre;
    ErlNode *node;

    *objp = NIL;		/* In case we fail, don't leave a hole in the heap */

    /* eat first atom */
    if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
	return NULL;
    num = get_int32(ep);
    ep += 4;
    if (num > ERTS_MAX_PID_NUMBER)
	return NULL;
    ser = get_int32(ep);
    ep += 4;
    if (ser > ERTS_MAX_PID_SERIAL)
	return NULL;
    cre = get_int8(ep);
    ep += 1;

    if (!is_valid_creation(cre)) {
	return NULL;
    }
    data = make_pid_data(ser, num);

    /*
     * We are careful to create the node entry only after all
     * validity tests are done.
     */
    node = dec_get_node(sysname, cre);

    if(node == erts_this_node) {
	*objp = make_internal_pid(data);
    } else {
	ExternalThing *etp = (ExternalThing *) *hpp;
	*hpp += EXTERNAL_THING_HEAD_SIZE + 1;

	etp->header = make_external_pid_header(1);
	etp->next = off_heap->first;
	etp->node = node;
	etp->data.ui[0] = data;

	off_heap->first = (struct erl_off_heap_header*) etp;
	*objp = make_external_pid(etp);
    }
    return ep;
}


#define ENC_TERM ((Eterm) 0)
#define ENC_ONE_CONS ((Eterm) 1)
#define ENC_PATCH_FUN_SIZE ((Eterm) 2)
#define ENC_LAST_ARRAY_ELEMENT ((Eterm) 3)


static byte*
enc_term(ErtsAtomCacheMap *acmp, Eterm obj, byte* ep, Uint32 dflags,
	 struct erl_off_heap_header** off_heap)
{
    byte *res;
    (void) enc_term_int(NULL, acmp, obj, ep, dflags, off_heap, NULL, &res);
    return res;
}

static int
enc_term_int(TTBEncodeContext* ctx, ErtsAtomCacheMap *acmp, Eterm obj, byte* ep, Uint32 dflags,
	     struct erl_off_heap_header** off_heap, Sint *reds, byte **res)
{
    DECLARE_WSTACK(s);
    Uint n;
    Uint i;
    Uint j;
    Uint* ptr;
    Eterm val;
    FloatDef f;
    Sint r = 0;
#if HALFWORD_HEAP
    UWord wobj;
#endif


    if (ctx) {
	WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	r = *reds;

	if (ctx->wstack.wstart) { /* restore saved stacks and byte pointer */
	    WSTACK_RESTORE(s, &ctx->wstack);
	    ep = ctx->ep;
	    obj = ctx->obj;
	}
    }

    goto L_jump_start;

 outer_loop:
    while (!WSTACK_ISEMPTY(s)) {
#if HALFWORD_HEAP
	obj = (Eterm) (wobj = WSTACK_POP(s));
#else
	obj = WSTACK_POP(s);
#endif
	switch (val = WSTACK_POP(s)) {
	case ENC_TERM:
	    break;
	case ENC_ONE_CONS:
	encode_one_cons:
	    {
		Eterm* cons = list_val(obj);
		Eterm tl;

		obj = CAR(cons);
		tl = CDR(cons);
		WSTACK_PUSH(s, is_list(tl) ? ENC_ONE_CONS : ENC_TERM);
		WSTACK_PUSH(s, tl);
	    }
	    break;
	case ENC_PATCH_FUN_SIZE:
	    {
#if HALFWORD_HEAP
		byte* size_p = (byte *) wobj;
#else
		byte* size_p = (byte *) obj;
#endif
		put_int32(ep - size_p, size_p);
	    }
	    goto outer_loop;
	case ENC_LAST_ARRAY_ELEMENT:
	    /* obj is the tuple */
	    {
#if HALFWORD_HEAP
		Eterm* ptr = (Eterm *) wobj;
#else
		Eterm* ptr = (Eterm *) obj;
#endif
		obj = *ptr;
	    }
	    break;
	default:		/* ENC_LAST_ARRAY_ELEMENT+1 and upwards */
	    {
#if HALFWORD_HEAP
		Eterm* ptr = (Eterm *) wobj;
#else
		Eterm* ptr = (Eterm *) obj;
#endif
		WSTACK_PUSH(s, val-1);
		obj = *ptr++;
		WSTACK_PUSH(s, (UWord)ptr);
	    }
	    break;
	}

    L_jump_start:

	if (ctx && --r == 0) {
	    *reds = r;
	    ctx->obj = obj;
	    ctx->ep = ep;
	    WSTACK_SAVE(s, &ctx->wstack);
	    return -1;
	}
	switch(tag_val_def(obj)) {
	case NIL_DEF:
	    *ep++ = NIL_EXT;
	    break;

	case ATOM_DEF:
	    ep = enc_atom(acmp,obj,ep,dflags);
	    break;

	case SMALL_DEF:
	    {
		/* From R14B we no longer restrict INTEGER_EXT to 28 bits,
		 * as done earlier for backward compatibility reasons. */
		Sint val = signed_val(obj);

		if ((Uint)val < 256) {
		    *ep++ = SMALL_INTEGER_EXT;
		    put_int8(val, ep);
		    ep++;
		} else if (sizeof(Sint) == 4 || IS_SSMALL32(val)) {
		    *ep++ = INTEGER_EXT;
		    put_int32(val, ep);
		    ep += 4;
		} else {
		    DeclareTmpHeapNoproc(tmp_big,2);
		    Eterm big;
		    UseTmpHeapNoproc(2);
		    big = small_to_big(val, tmp_big);
		    *ep++ = SMALL_BIG_EXT;
		    n = big_bytes(big);
		    ASSERT(n < 256);
		    put_int8(n, ep);
		    ep += 1;
		    *ep++ = big_sign(big);
		    ep = big_to_bytes(big, ep);
		    UnUseTmpHeapNoproc(2);
		}
	    }
	    break;

	case BIG_DEF:
	    {
		int sign = big_sign(obj);
		n = big_bytes(obj);
		if (sizeof(Sint)==4 && n<=4) {
		    Uint dig = big_digit(obj,0);		   
		    Sint val = sign ? -dig : dig;
		    if ((val<0) == sign) {
			*ep++ = INTEGER_EXT;
			put_int32(val, ep);
			ep += 4;
			break;
		    }
		}
		if (n < 256) {
		    *ep++ = SMALL_BIG_EXT;
		    put_int8(n, ep);
		    ep += 1;
		}
		else {
		    *ep++ = LARGE_BIG_EXT;
		    put_int32(n, ep);
		    ep += 4;
		}
		*ep++ = sign;
		ep = big_to_bytes(obj, ep);
	    }
	    break;

	case PID_DEF:
	case EXTERNAL_PID_DEF:
	    ep = enc_pid(acmp, obj, ep, dflags);
	    break;

	case REF_DEF:
	case EXTERNAL_REF_DEF: {
	    Uint32 *ref_num;

	    ASSERT(dflags & DFLAG_EXTENDED_REFERENCES);

	    *ep++ = NEW_REFERENCE_EXT;
	    i = ref_no_of_numbers(obj);
	    put_int16(i, ep);
	    ep += 2;
	    ep = enc_atom(acmp,ref_node_name(obj),ep,dflags);
	    *ep++ = ((dflags & DFLAG_INTERNAL_TAGS) && is_internal_ref(obj)) ?
		INTERNAL_CREATION : ref_creation(obj);
	    ref_num = ref_numbers(obj);
	    for (j = 0; j < i; j++) {
		put_int32(ref_num[j], ep);
		ep += 4;
	    }
	    break;
	}
	case PORT_DEF:
	case EXTERNAL_PORT_DEF:

	    *ep++ = PORT_EXT;
	    ep = enc_atom(acmp,port_node_name(obj),ep,dflags);
	    j = port_number(obj);
	    put_int32(j, ep);
	    ep += 4;
	    *ep++ = ((dflags & DFLAG_INTERNAL_TAGS) && is_internal_port(obj)) ?
		INTERNAL_CREATION : port_creation(obj);
	    break;

	case LIST_DEF:
	    {
		int is_str;

		i = is_external_string(obj, &is_str);
		if (is_str) {
		    *ep++ = STRING_EXT;
		    put_int16(i, ep);
		    ep += 2;
		    while (is_list(obj)) {
			Eterm* cons = list_val(obj);
			*ep++ = unsigned_val(CAR(cons));
			obj = CDR(cons);
		    }
		} else {
		    *ep++ = LIST_EXT;
		    put_int32(i, ep);
		    ep += 4;
		    goto encode_one_cons;
		}
	    }
	    break;

	case TUPLE_DEF:
	    ptr = tuple_val(obj);
	    i = arityval(*ptr);
	    ptr++;
	    if (i <= 0xff) {
		*ep++ = SMALL_TUPLE_EXT;
		put_int8(i, ep);
		ep += 1;
	    } else  {
		*ep++ = LARGE_TUPLE_EXT;
		put_int32(i, ep);
		ep += 4;
	    }
	    if (i > 0) {
		WSTACK_PUSH(s, ENC_LAST_ARRAY_ELEMENT+i-1);
		WSTACK_PUSH(s, (UWord)ptr);
	    }
	    break;

	case MAP_DEF:
	    {
		map_t *mp = (map_t*)map_val(obj);
		Uint size = map_get_size(mp);
		Eterm *mptr;

		*ep++ = MAP_EXT;
		put_int32(size, ep); ep += 4;

		/* Push values first */
		if (size > 0) {
		    mptr = map_get_values(mp);
		    for (i = size-1; i >= 1; i--) {
			WSTACK_PUSH(s, ENC_TERM);
			WSTACK_PUSH(s, (UWord) mptr[i]);
		    }

		    WSTACK_PUSH(s, ENC_TERM);
		    WSTACK_PUSH(s, (UWord) mptr[0]);

		    mptr = map_get_keys(mp);
		    for (i = size-1; i >= 1; i--) {
			WSTACK_PUSH(s, ENC_TERM);
			WSTACK_PUSH(s, (UWord) mptr[i]);
		    }

		    obj = mptr[0];
		    goto L_jump_start;
		}
	    }
	    break;

	case FLOAT_DEF:
	    GET_DOUBLE(obj, f);
	    if (dflags & DFLAG_NEW_FLOATS) {
		*ep++ = NEW_FLOAT_EXT;
#if defined(WORDS_BIGENDIAN) || defined(DOUBLE_MIDDLE_ENDIAN)
		put_int32(f.fw[0], ep);
		ep += 4;
		put_int32(f.fw[1], ep);
#else
		put_int32(f.fw[1], ep);
		ep += 4;
		put_int32(f.fw[0], ep);
#endif		
		ep += 4;
	    } else {
		*ep++ = FLOAT_EXT;

		/* now the erts_snprintf which does the work */
		i = sys_double_to_chars(f.fd, (char*) ep, (size_t)31);

		/* Don't leave garbage after the float!  (Bad practice in general,
		 * and Purify complains.)
		 */
		sys_memset(ep+i, 0, 31-i);
		ep += 31;
	    }
	    break;

	case BINARY_DEF:
	    {
		Uint bitoffs;
		Uint bitsize;
		byte* bytes;

		ERTS_GET_BINARY_BYTES(obj, bytes, bitoffs, bitsize);
		if (dflags & DFLAG_INTERNAL_TAGS) {
		    ProcBin* pb = (ProcBin*) binary_val(obj);
		    Uint bytesize = pb->size;
		    if (pb->thing_word == HEADER_SUB_BIN) {
			ErlSubBin* sub = (ErlSubBin*)pb;
			pb = (ProcBin*) binary_val(sub->orig);
			ASSERT(bytesize == sub->size);
			bytesize += (bitoffs + bitsize + 7) / 8;
		    }
		    if (pb->thing_word == HEADER_PROC_BIN
			&& heap_bin_size(bytesize) > PROC_BIN_SIZE) {
			ProcBin tmp;
			if (bitoffs || bitsize) {
			    *ep++ = BIT_BINARY_INTERNAL_REF;
			    *ep++ = bitoffs;
			    *ep++ = bitsize;
			}
			else {
			    *ep++ = BINARY_INTERNAL_REF;
			}
			if (pb->flags) {
			    char* before_realloc = pb->val->orig_bytes; 
			    erts_emasculate_writable_binary(pb);
			    bytes += (pb->val->orig_bytes - before_realloc);
			}
			erts_refc_inc(&pb->val->refc, 2);

			sys_memcpy(&tmp, pb, sizeof(ProcBin));
			tmp.next = *off_heap;
			tmp.bytes = bytes;
			tmp.size = bytesize;
			sys_memcpy(ep, &tmp, sizeof(ProcBin));
			*off_heap = (struct erl_off_heap_header*) ep;
			ep += sizeof(ProcBin);
			break;
		    }
		}
		if (bitsize == 0) {
		    /* Plain old byte-sized binary. */
		    *ep++ = BINARY_EXT;
		    j = binary_size(obj);
		    put_int32(j, ep);
		    ep += 4;
		    copy_binary_to_buffer(ep, 0, bytes, bitoffs, 8*j);
		    ep += j;
		} else if (dflags & DFLAG_BIT_BINARIES) {
		    /* Bit-level binary. */
		    *ep++ = BIT_BINARY_EXT;
		    j = binary_size(obj);
		    put_int32((j+1), ep);
		    ep += 4;
		    *ep++ = bitsize;
		    ep[j] = 0;	/* Zero unused bits at end of binary */
		    copy_binary_to_buffer(ep, 0, bytes, bitoffs, 8*j+bitsize);
		    ep += j + 1;
		} else {
		    /*
		     * Bit-level binary, but the receiver doesn't support it.
		     * Build a tuple instead.
		     */
		    *ep++ = SMALL_TUPLE_EXT;
		    *ep++ = 2;
		    *ep++ = BINARY_EXT;
		    j = binary_size(obj);
		    put_int32((j+1), ep);
		    ep += 4;
		    ep[j] = 0;	/* Zero unused bits at end of binary */
		    copy_binary_to_buffer(ep, 0, bytes, bitoffs, 8*j+bitsize);
		    ep += j+1;
		    *ep++ = SMALL_INTEGER_EXT;
		    *ep++ = bitsize;
		}
	    }
	    break;
	case EXPORT_DEF:
	    {
		Export* exp = *((Export **) (export_val(obj) + 1));
		if ((dflags & DFLAG_EXPORT_PTR_TAG) != 0) {
		    *ep++ = EXPORT_EXT;
		    ep = enc_atom(acmp, exp->code[0], ep, dflags);
		    ep = enc_atom(acmp, exp->code[1], ep, dflags);
		    ep = enc_term(acmp, make_small(exp->code[2]), ep, dflags, off_heap);
		} else {
		    /* Tag, arity */
		    *ep++ = SMALL_TUPLE_EXT;
		    put_int8(2, ep);
		    ep += 1;

		    /* Module name */
		    ep = enc_atom(acmp, exp->code[0], ep, dflags);

		    /* Function name */
		    ep = enc_atom(acmp, exp->code[1], ep, dflags);
		}
		break;
	    }
	    break;
	case FUN_DEF:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(obj);

		if ((dflags & DFLAG_NEW_FUN_TAGS) != 0) {
		    int ei;

		    *ep++ = NEW_FUN_EXT;
		    WSTACK_PUSH(s, ENC_PATCH_FUN_SIZE);
		    WSTACK_PUSH(s, (UWord) ep); /* Position for patching in size */
		    ep += 4;
		    *ep = funp->arity;
		    ep += 1;
		    sys_memcpy(ep, funp->fe->uniq, 16);
		    ep += 16;
		    put_int32(funp->fe->index, ep);
		    ep += 4;
		    put_int32(funp->num_free, ep);
		    ep += 4;
		    ep = enc_atom(acmp, funp->fe->module, ep, dflags);
		    ep = enc_term(acmp, make_small(funp->fe->old_index), ep, dflags, off_heap);
		    ep = enc_term(acmp, make_small(funp->fe->old_uniq), ep, dflags, off_heap);
		    ep = enc_pid(acmp, funp->creator, ep, dflags);

		fun_env:
		    for (ei = funp->num_free-1; ei > 0; ei--) {
			WSTACK_PUSH(s, ENC_TERM);
			WSTACK_PUSH(s, (UWord) funp->env[ei]);
		    }
		    if (funp->num_free != 0) {
			obj = funp->env[0];
			goto L_jump_start;
		    }
		} else {
		    /*
		     * Communicating with an obsolete erl_interface or
		     * jinterface node. Convert the fun to a tuple to
		     * avoid crasching.
		     */
		
		    /* Tag, arity */
		    *ep++ = SMALL_TUPLE_EXT;
		    put_int8(5, ep);
		    ep += 1;
		
		    /* 'fun' */
		    ep = enc_atom(acmp, am_fun, ep, dflags);
		
		    /* Module name */
		    ep = enc_atom(acmp, funp->fe->module, ep, dflags);
		
		    /* Index, Uniq */
		    *ep++ = INTEGER_EXT;
		    put_int32(funp->fe->old_index, ep);
		    ep += 4;
		    *ep++ = INTEGER_EXT;
		    put_int32(funp->fe->old_uniq, ep);
		    ep += 4;
		
		    /* Environment sub-tuple arity */
		    ASSERT(funp->num_free < MAX_ARG);
		    *ep++ = SMALL_TUPLE_EXT;
		    put_int8(funp->num_free, ep);
		    ep += 1;
		    goto fun_env;
		}
	    }
	    break;
	}
    }
    DESTROY_WSTACK(s);
    if (ctx) {
	ASSERT(ctx->wstack.wstart == NULL);
	*reds = r;
    }
    *res = ep;
    return 0;
}

static
Uint
is_external_string(Eterm list, int* p_is_string)
{
    Uint len = 0;

    /*
     * Calculate the length of the list as long as all characters
     * are integers from 0 through 255.
     */
    while (is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd)) {
	    break;
	}
	len++;
	list = CDR(consp);
    }

    /*
     * If we have reached the end of the list, and we have
     * not exceeded the maximum length of a string, this
     * is a string.
     */
    *p_is_string = is_nil(list) && len < MAX_STRING_LEN;

    /*
     * Continue to calculate the length.
     */
    while (is_list(list)) {
	Eterm* consp = list_val(list);
	len++;
	list = CDR(consp);
    }
    return len;
}

/* Assumes that the ones to undo are preluding the list. */ 
static void
undo_offheap_in_area(ErlOffHeap* off_heap, Eterm* start, Eterm* end)
{
    const Uint area_sz = (end - start) * sizeof(Eterm);
    struct erl_off_heap_header* hdr;
    struct erl_off_heap_header** hdr_nextp = NULL;

    for (hdr = off_heap->first; ; hdr=hdr->next) {
	if (!in_area(hdr, start, area_sz)) {
	    if (hdr_nextp != NULL) {
		*hdr_nextp = NULL;
		erts_cleanup_offheap(off_heap);
		off_heap->first = hdr;
	    }
	    break;
	}
	hdr_nextp = &hdr->next;
    }    

    /* Assert that the ones to undo were indeed preluding the list. */ 
#ifdef DEBUG
    for (hdr = off_heap->first; hdr != NULL; hdr = hdr->next) {
	ASSERT(!in_area(hdr, start, area_sz));
    }    
#endif /* DEBUG */
}


/* Decode term from external format into *objp.
** On failure return NULL and (R13B04) *hpp will be unchanged.
*/
static byte*
dec_term(ErtsDistExternal *edep, Eterm** hpp, byte* ep, ErlOffHeap* off_heap,
         Eterm* objp, B2TContext* ctx)
{
    Eterm* hp_saved;
    int n;
    ErtsAtomEncoding char_enc;
    register Eterm* hp;        /* Please don't take the address of hp */
    Eterm *maps_head;   /* for validation of maps */
    Eterm* next;
    SWord reds;

    if (ctx) {
        hp_saved = ctx->u.dc.hp_start;
        reds     = ctx->reds;
        next     = ctx->u.dc.next;
        ep       = ctx->u.dc.ep;
        hpp      = &ctx->u.dc.hp;
	maps_head = ctx->u.dc.maps_head;

        if (ctx->state != B2TDecode) {
            int n_limit = reds;

	    n = ctx->u.dc.remaining_n;
            if (ctx->state == B2TDecodeBinary) {
                n_limit *= B2T_MEMCPY_FACTOR;
                ASSERT(n_limit >= reds);
		reds -= n / B2T_MEMCPY_FACTOR;
            }
	    else
		reds -= n;

            if (n > n_limit) {
                ctx->u.dc.remaining_n -= n_limit;
                n = n_limit;
                reds = 0;
            }
            else {
                ctx->u.dc.remaining_n = 0;
            }

            switch (ctx->state) {
            case B2TDecodeList:
                objp = next - 2;
                while (n > 0) {
                    objp[0] = (Eterm) COMPRESS_POINTER(next);
                    objp[1] = make_list(next);
                    next = objp;
                    objp -= 2;
                    n--;
                }
                break;

            case B2TDecodeTuple:
                objp = next - 1;
                while (n-- > 0) {
                    objp[0] = (Eterm) COMPRESS_POINTER(next);
                    next = objp;
                    objp--;
                }
                break;

            case B2TDecodeString:
                hp = *hpp;
                hp[-1] = make_list(hp);  /* overwrite the premature NIL */
                while (n-- > 0) {
                    hp[0] = make_small(*ep++);
                    hp[1] = make_list(hp+2);
                    hp += 2;
                }
                hp[-1] = NIL;
                *hpp = hp;
                break;

            case B2TDecodeBinary:
                sys_memcpy(ctx->u.dc.remaining_bytes, ep, n);
                ctx->u.dc.remaining_bytes += n;
                ep += n;
                break;

            default:
                ASSERT(!"Unknown state");
            }
            if (!ctx->u.dc.remaining_n) {
                ctx->state = B2TDecode;
            }
            if (reds <= 0) {
                ctx->u.dc.next = next;
                ctx->u.dc.ep = ep;
                ctx->reds = 0;
                return NULL;
            }
        }
    }
    else {
        hp_saved = *hpp;
        reds = ERTS_SWORD_MAX;
        next = objp;
        *next = (Eterm) (UWord) NULL;
	maps_head = NULL;
    }
    hp = *hpp;

    while (next != NULL) {

	objp = next;
	next = (Eterm *) EXPAND_POINTER(*objp);

	switch (*ep++) {
	case INTEGER_EXT:
	    {
		Sint sn = get_int32(ep);

		ep += 4;
#if defined(ARCH_64) && !HALFWORD_HEAP
		*objp = make_small(sn);
#else
		if (MY_IS_SSMALL(sn)) {
		    *objp = make_small(sn);
		} else {
		    *objp = small_to_big(sn, hp);
		    hp += BIG_UINT_HEAP_SIZE;
		}
#endif
		break;
	    }
	case SMALL_INTEGER_EXT:
	    n = get_int8(ep);
	    ep++;
	    *objp = make_small(n);
	    break;
	case SMALL_BIG_EXT:
	    n = get_int8(ep);
	    ep++;
	    goto big_loop;
	case LARGE_BIG_EXT:
	    n = get_int32(ep);
	    ep += 4;
	big_loop:
	    {
		Eterm big;
		byte* first;
		byte* last;
		Uint neg;

		neg = get_int8(ep); /* Sign bit */
		ep++;

		/*
		 * Strip away leading zeroes to avoid creating illegal bignums.
		 */
		first = ep;
		last = ep + n;
		ep += n;
		do {
		    --last;
		} while (first <= last && *last == 0);

		if ((n = last - first + 1) == 0) {
		    /* Zero width bignum defaults to zero */
		    big = make_small(0);
		} else {
		    big = bytes_to_big(first, n, neg, hp);
		    if (is_big(big)) {
			hp += big_arity(big) + 1;
		    }
		}
		*objp = big;
		break;
	    }
	case ATOM_CACHE_REF:
	    if (edep == 0 || (edep->flags & ERTS_DIST_EXT_ATOM_TRANS_TAB) == 0) {
		goto error;
	    }
	    n = get_int8(ep);
	    ep++;
	    if (n >= edep->attab.size)
		goto error;
	    ASSERT(is_atom(edep->attab.atom[n]));
	    *objp = edep->attab.atom[n];
	    break;
	case ATOM_EXT:
	    n = get_int16(ep);
	    ep += 2;
	    char_enc = ERTS_ATOM_ENC_LATIN1;
	    goto dec_term_atom_common;
	case SMALL_ATOM_EXT:
	    n = get_int8(ep);
	    ep++;
	    char_enc = ERTS_ATOM_ENC_LATIN1;
	    goto dec_term_atom_common;
	case ATOM_UTF8_EXT:
	    n = get_int16(ep);
	    ep += 2;
	    char_enc = ERTS_ATOM_ENC_UTF8;
	    goto dec_term_atom_common;
	case SMALL_ATOM_UTF8_EXT:
	    n = get_int8(ep);
	    ep++;
	    char_enc = ERTS_ATOM_ENC_UTF8;
dec_term_atom_common:
	    if (edep && (edep->flags & ERTS_DIST_EXT_BTT_SAFE)) {
		if (!erts_atom_get((char*)ep, n, objp, char_enc)) {
		    goto error;
		}
	    } else {
		Eterm atom = erts_atom_put(ep, n, char_enc, 0);
		if (is_non_value(atom))
		    goto error;
	        *objp = atom;
	    }
	    ep += n;
	    break;
	case LARGE_TUPLE_EXT:
	    n = get_int32(ep);
	    ep += 4;
	    goto tuple_loop;
	case SMALL_TUPLE_EXT:
	    n = get_int8(ep);
	    ep++;
	tuple_loop:
	    *objp = make_tuple(hp);
	    *hp++ = make_arityval(n);
	    hp += n;
            objp = hp - 1;
            if (ctx) {
                if (reds < n) {
                    ASSERT(reds > 0);
                    ctx->state = B2TDecodeTuple;
                    ctx->u.dc.remaining_n = n - reds;
                    n = reds;
                }
		reds -= n;
	    }
	    while (n-- > 0) {
		objp[0] = (Eterm) COMPRESS_POINTER(next);
		next = objp;
		objp--;
	    }
	    break;
	case NIL_EXT:
	    *objp = NIL;
	    break;
	case LIST_EXT:
	    n = get_int32(ep);
	    ep += 4;
	    if (n == 0) {
		next = objp;
		break;
	    }
	    *objp = make_list(hp);
            hp += 2 * n;
	    objp = hp - 2;
	    objp[0] = (Eterm) COMPRESS_POINTER((objp+1));
	    objp[1] = (Eterm) COMPRESS_POINTER(next);
	    next = objp;
	    objp -= 2;
            n--;
	    if (ctx) {
                if (reds < n) {
		    ctx->state = B2TDecodeList;
		    ctx->u.dc.remaining_n = n - reds;
		    n = reds;
		}
		reds -= n;
	    }
            while (n > 0) {
		objp[0] = (Eterm) COMPRESS_POINTER(next);
		objp[1] = make_list(next);
		next = objp;
		objp -= 2;
                n--;
	    }
	    break;
	case STRING_EXT:
	    n = get_int16(ep);
	    ep += 2;
	    if (n == 0) {
		*objp = NIL;
		break;
	    }
	    *objp = make_list(hp);
            if (ctx) {
                if (reds < n) {
                    ctx->state = B2TDecodeString;
                    ctx->u.dc.remaining_n = n - reds;
                    n = reds;
		}
                reds -= n;
            }
	    while (n-- > 0) {
		hp[0] = make_small(*ep++);
		hp[1] = make_list(hp+2);
		hp += 2;
	    }
	    hp[-1] = NIL;
	    break;
	case FLOAT_EXT:
	    {
		FloatDef ff;

		if (sys_chars_to_double((char*)ep, &ff.fd) != 0) {
		    goto error;
		}
		ep += 31;
		*objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
	case NEW_FLOAT_EXT:
	    {
		FloatDef ff;
#ifndef NO_FPE_SIGNALS
		volatile unsigned long *fpexnp = erts_get_current_fp_exception();
#endif

#if defined(WORDS_BIGENDIAN) || defined(DOUBLE_MIDDLE_ENDIAN)
		ff.fw[0] = get_int32(ep);
		ep += 4;
		ff.fw[1] = get_int32(ep);
		ep += 4;
#else
		ff.fw[1] = get_int32(ep);
		ep += 4;
		ff.fw[0] = get_int32(ep);
		ep += 4;
#endif		
		__ERTS_FP_CHECK_INIT(fpexnp);
		__ERTS_FP_ERROR_THOROUGH(fpexnp, ff.fd, goto error);
		*objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
	case PID_EXT:
	    *hpp = hp;
	    ep = dec_pid(edep, hpp, ep, off_heap, objp);
	    hp = *hpp;
	    if (ep == NULL) {
		goto error;
	    }
	    break;
	case PORT_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		Uint num;
		Uint cre;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL) {
		    goto error;
		}
		if ((num = get_int32(ep)) > ERTS_MAX_PORT_NUMBER) {
		    goto error;
		}
		ep += 4;
		cre = get_int8(ep);
		ep++;
		if (!is_valid_creation(cre)) {
		    goto error;
		}

		node = dec_get_node(sysname, cre);
		if(node == erts_this_node) {
		    *objp = make_internal_port(num);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
		    hp += EXTERNAL_THING_HEAD_SIZE + 1;
		    
		    etp->header = make_external_port_header(1);
		    etp->next = off_heap->first;
		    etp->node = node;
		    etp->data.ui[0] = num;

		    off_heap->first = (struct erl_off_heap_header*)etp;
		    *objp = make_external_port(etp);
		}

		break;
	    }
	case REFERENCE_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		int i;
		Uint cre;
		Uint32 *ref_num;
		Uint32 r0;
		Uint ref_words;

		ref_words = 1;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
		    goto error;
		if ((r0 = get_int32(ep)) >= MAX_REFERENCE )
		    goto error;
		ep += 4;

		cre = get_int8(ep);
		ep += 1;
		if (!is_valid_creation(cre)) {
		    goto error;
		}
		goto ref_ext_common;

	    case NEW_REFERENCE_EXT:
		ref_words = get_int16(ep);
		ep += 2;

		if (ref_words > ERTS_MAX_REF_NUMBERS)
		    goto error;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
		    goto error;

		cre = get_int8(ep);
		ep += 1;
		if (!is_valid_creation(cre)) {
		    goto error;
		}
		r0 = get_int32(ep);
		ep += 4;
		if (r0 >= MAX_REFERENCE)
		    goto error;

	    ref_ext_common:

		node = dec_get_node(sysname, cre);
		if(node == erts_this_node) {
		    RefThing *rtp = (RefThing *) hp;
		    ref_num = (Uint32 *) (hp + REF_THING_HEAD_SIZE);

#if defined(ARCH_64) && !HALFWORD_HEAP
		    hp += REF_THING_HEAD_SIZE + ref_words/2 + 1;
		    rtp->header = make_ref_thing_header(ref_words/2 + 1);
#else
		    hp += REF_THING_HEAD_SIZE + ref_words;
		    rtp->header = make_ref_thing_header(ref_words);
#endif
		    *objp = make_internal_ref(rtp);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
#if defined(ARCH_64) && !HALFWORD_HEAP
		    hp += EXTERNAL_THING_HEAD_SIZE + ref_words/2 + 1;
#else
		    hp += EXTERNAL_THING_HEAD_SIZE + ref_words;
#endif

#if defined(ARCH_64) && !HALFWORD_HEAP
		    etp->header = make_external_ref_header(ref_words/2 + 1);
#else
		    etp->header = make_external_ref_header(ref_words);
#endif
		    etp->next = off_heap->first;
		    etp->node = node;

		    off_heap->first = (struct erl_off_heap_header*)etp;
		    *objp = make_external_ref(etp);
		    ref_num = &(etp->data.ui32[0]);
		}

#if defined(ARCH_64) && !HALFWORD_HEAP
		*(ref_num++) = ref_words /* 32-bit arity */;
#endif
		ref_num[0] = r0;
		for(i = 1; i < ref_words; i++) {
		    ref_num[i] = get_int32(ep);
		    ep += 4;
		}
#if defined(ARCH_64) && !HALFWORD_HEAP
		if ((1 + ref_words) % 2)
		    ref_num[ref_words] = 0;
#endif
		break;
	    }
	case BINARY_EXT:
	    {
		n = get_int32(ep);
		ep += 4;
	    
		if ((unsigned)n <= ERL_ONHEAP_BIN_LIMIT) {
		    ErlHeapBin* hb = (ErlHeapBin *) hp;

		    hb->thing_word = header_heap_bin(n);
		    hb->size = n;
		    hp += heap_bin_size(n);
		    sys_memcpy(hb->data, ep, n);
		    *objp = make_binary(hb);
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);
		    ProcBin* pb;
		    dbin->flags = 0;
		    dbin->orig_size = n;
		    erts_refc_init(&dbin->refc, 1);
		    pb = (ProcBin *) hp;
		    hp += PROC_BIN_SIZE;
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = n;
		    pb->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*)pb;
		    pb->val = dbin;
		    pb->bytes = (byte*) dbin->orig_bytes;
		    pb->flags = 0;
		    *objp = make_binary(pb);
                    if (ctx) {
                        int n_limit = reds * B2T_MEMCPY_FACTOR;
                        if (n > n_limit) {
                            ctx->state = B2TDecodeBinary;
                            ctx->u.dc.remaining_n = n - n_limit;
                            ctx->u.dc.remaining_bytes = dbin->orig_bytes + n_limit;
                            n = n_limit;
                            reds = 0;
                        }
                        else
                            reds -= n / B2T_MEMCPY_FACTOR;
                    }
                    sys_memcpy(dbin->orig_bytes, ep, n);
                }
		ep += n;
		break;
	    }
	case BIT_BINARY_EXT:
	    {
		Eterm bin;
		ErlSubBin* sb;
		Uint bitsize;

		n = get_int32(ep);
		bitsize = ep[4];
                if (((bitsize==0) != (n==0)) || bitsize > 8)
                    goto error;
                ep += 5;
		if ((unsigned)n <= ERL_ONHEAP_BIN_LIMIT) {
		    ErlHeapBin* hb = (ErlHeapBin *) hp;

		    hb->thing_word = header_heap_bin(n);
		    hb->size = n;
		    sys_memcpy(hb->data, ep, n);
		    bin = make_binary(hb);
		    hp += heap_bin_size(n);
                    ep += n;
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);
		    ProcBin* pb;

		    dbin->flags = 0;
		    dbin->orig_size = n;
		    erts_refc_init(&dbin->refc, 1);
		    pb = (ProcBin *) hp;
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = n;
		    pb->next = off_heap->first;
		    off_heap->first = (struct erl_off_heap_header*)pb;
		    pb->val = dbin;
		    pb->bytes = (byte*) dbin->orig_bytes;
		    pb->flags = 0;
		    bin = make_binary(pb);
		    hp += PROC_BIN_SIZE;
                    if (ctx) {
                        int n_limit = reds * B2T_MEMCPY_FACTOR;
                        if (n > n_limit) {
                            ctx->state = B2TDecodeBinary;
                            ctx->u.dc.remaining_n = n - n_limit;
                            ctx->u.dc.remaining_bytes = dbin->orig_bytes + n_limit;
                            n = n_limit;
                            reds = 0;
                        }
                        else
                            reds -= n / B2T_MEMCPY_FACTOR;
                    }
                    sys_memcpy(dbin->orig_bytes, ep, n);
                    ep += n;
                    n = pb->size;
                }

		if (bitsize == 8 || n == 0) {
		    *objp = bin;
		} else {
                    sb = (ErlSubBin *)hp;
		    sb->thing_word = HEADER_SUB_BIN;
		    sb->orig = bin;
		    sb->size = n - 1;
		    sb->bitsize = bitsize;
		    sb->bitoffs = 0;
		    sb->offs = 0;
		    sb->is_writable = 0;
		    *objp = make_binary(sb);
		    hp += ERL_SUB_BIN_SIZE;
		}
		break;
	    }
	case EXPORT_EXT:
	    {
		Eterm mod;
		Eterm name;
		Eterm temp;
		Sint arity;

		if ((ep = dec_atom(edep, ep, &mod)) == NULL) {
		    goto error;
		}
		if ((ep = dec_atom(edep, ep, &name)) == NULL) {
		    goto error;
		}
		*hpp = hp;
		ep = dec_term(edep, hpp, ep, off_heap, &temp, NULL);
		hp = *hpp;
		if (ep == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		arity = signed_val(temp);
		if (arity < 0) {
		    goto error;
		}
		if (edep && (edep->flags & ERTS_DIST_EXT_BTT_SAFE)) {
		    if (!erts_active_export_entry(mod, name, arity))
			goto error;
                }
		*objp = make_export(hp);
		*hp++ = HEADER_EXPORT;
#if HALFWORD_HEAP
		*((UWord *) (UWord) hp) =  (UWord) erts_export_get_or_make_stub(mod, name, arity);
		hp += 2;
#else
		*hp++ = (Eterm) erts_export_get_or_make_stub(mod, name, arity);
#endif
		break;
	    }
	    break;
	case MAP_EXT:
	    {
		map_t *mp;
		Uint32 size,n;
		Eterm *kptr,*vptr;
		Eterm keys;

		size = get_int32(ep); ep += 4;

		keys  = make_tuple(hp);
		*hp++ = make_arityval(size);
		kptr  = hp;
		hp   += size;

		mp    = (map_t*)hp;
		hp   += MAP_HEADER_SIZE;
		vptr  = hp;
		hp   += size;

		/* kptr, first word for keys
		 * vptr, first word for values
		 */

		/*
		 * Use thing_word to link through decoded maps.
		 * The list of maps is for later validation.
		 */

		mp->thing_word = (Eterm) COMPRESS_POINTER(maps_head);
		maps_head      = (Eterm *) mp;

		mp->size       = size;
		mp->keys       = keys;
		*objp          = make_map(mp);

		/* We assume the map is wellformed, meaning:
		 * - ascending key order
		 * - unique keys
		 */

		objp  = vptr + size - 1;
		n     = size;

		while (n-- > 0) {
		    *objp = (Eterm) COMPRESS_POINTER(next);
		    next  = objp;
		    objp--;
		}

		objp  = kptr + size - 1;
		n     = size;

		while (n-- > 0) {
		    *objp = (Eterm) COMPRESS_POINTER(next);
		    next  = objp;
		    objp--;
		}
	    }
	    break;
	case NEW_FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Uint arity;
		Eterm module;
		byte* uniq;
		int index;
		Sint old_uniq;
		Sint old_index;
		unsigned num_free;
		int i;
		Eterm temp;

		ep += 4;	/* Skip total size in bytes */
		arity = *ep++;
		uniq = ep;
		ep += 16;
		index = get_int32(ep);
		ep += 4;
		num_free = get_int32(ep);
		ep += 4;
		hp += ERL_FUN_SIZE;
		hp += num_free;
		funp->thing_word = HEADER_FUN;
		funp->num_free = num_free;
		*objp = make_fun(funp);

		/* Module */
		if ((ep = dec_atom(edep, ep, &module)) == NULL) {
		    goto error;
		}
		*hpp = hp;
		/* Index */
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp, NULL)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		/* Uniq */
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp, NULL)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_uniq = unsigned_val(temp);

		/*
		 * It is safe to link the fun into the fun list only when
		 * no more validity tests can fail.
		 */
		funp->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)funp;

		funp->fe = erts_put_fun_entry2(module, old_uniq, old_index,
					       uniq, index, arity);
		funp->arity = arity;
#ifdef HIPE
		if (funp->fe->native_address == NULL) {
		    hipe_set_closure_stub(funp->fe, num_free);
		}
		funp->native_address = funp->fe->native_address;
#endif
		hp = *hpp;

		/* Environment */
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) COMPRESS_POINTER(next);
		    next = funp->env + i;
		}
		/* Creator */
		funp->creator = (Eterm) COMPRESS_POINTER(next);
		next = &(funp->creator);
		break;
	    }
	case FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Eterm module;
		Sint old_uniq;
		Sint old_index;
		unsigned num_free;
		int i;
		Eterm temp;

		num_free = get_int32(ep);
		ep += 4;
		hp += ERL_FUN_SIZE;
		hp += num_free;
		*hpp = hp;
		funp->thing_word = HEADER_FUN;
		funp->num_free = num_free;
		*objp = make_fun(funp);

		/* Creator pid */
		if (*ep != PID_EXT 
		    || (ep = dec_pid(edep, hpp, ++ep, off_heap,
				     &funp->creator))==NULL) { 
		    goto error;
		}

		/* Module */
		if ((ep = dec_atom(edep, ep, &module)) == NULL) {
		    goto error;
		}

		/* Index */
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp, NULL)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		/* Uniq */
		if ((ep = dec_term(edep, hpp, ep, off_heap, &temp, NULL)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		
		/*
		 * It is safe to link the fun into the fun list only when
		 * no more validity tests can fail.
		 */
		funp->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)funp;
		old_uniq = unsigned_val(temp);

		funp->fe = erts_put_fun_entry(module, old_uniq, old_index);
		funp->arity = funp->fe->address[-1] - num_free;
#ifdef HIPE
		funp->native_address = funp->fe->native_address;
#endif
		hp = *hpp;

		/* Environment */
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) COMPRESS_POINTER(next);
		    next = funp->env + i;
		}
		break;
	    }
	case ATOM_INTERNAL_REF2:
	    n = get_int16(ep);
	    ep += 2;
	    if (n >= atom_table_size()) {
		goto error;
	    }
	    *objp = make_atom(n);
	    break;
	case ATOM_INTERNAL_REF3:
	    n = get_int24(ep);
	    ep += 3;
	    if (n >= atom_table_size()) {
		goto error;
	    }
	    *objp = make_atom(n);
	    break;

	case BINARY_INTERNAL_REF:
	    {
		ProcBin* pb = (ProcBin*) hp;
		sys_memcpy(pb, ep, sizeof(ProcBin));
		ep += sizeof(ProcBin);

		erts_refc_inc(&pb->val->refc, 1);
		hp += PROC_BIN_SIZE;
		pb->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)pb;
		pb->flags = 0;
		*objp = make_binary(pb);
		break;
	    }
	case BIT_BINARY_INTERNAL_REF:
	    {
		Sint bitoffs = *ep++;
		Sint bitsize = *ep++;
		ProcBin* pb = (ProcBin*) hp;
		ErlSubBin* sub;
		sys_memcpy(pb, ep, sizeof(ProcBin));
		ep += sizeof(ProcBin);

		erts_refc_inc(&pb->val->refc, 1);
		hp += PROC_BIN_SIZE;
		pb->next = off_heap->first;
		off_heap->first = (struct erl_off_heap_header*)pb;
		pb->flags = 0;

		sub = (ErlSubBin*)hp;
		sub->thing_word = HEADER_SUB_BIN;
		sub->size = pb->size - (bitoffs + bitsize + 7)/8;
		sub->offs = 0;
		sub->bitoffs = bitoffs;
		sub->bitsize = bitsize;
		sub->is_writable = 0;
		sub->orig = make_binary(pb);

		hp += ERL_SUB_BIN_SIZE;
		*objp = make_binary(sub);
		break;
	    }

	default:
	    goto error;
	}

        if (--reds <= 0) {
            if (ctx) {
                if (next || ctx->state != B2TDecode) {
                    ctx->u.dc.ep = ep;
                    ctx->u.dc.next = next;
                    ctx->u.dc.hp = hp;
		    ctx->u.dc.maps_head = maps_head;
                    ctx->reds = 0;
                    return NULL;
                }
            }
            else {
                reds = ERTS_SWORD_MAX;
            }
        }
    }

    /* Iterate through all the maps and check for validity and sort keys
     * - done here for when we know it is complete.
     */

    while (maps_head) {
	next  = (Eterm *)(EXPAND_POINTER(*maps_head));
	*maps_head = MAP_HEADER;
	if (!erts_validate_and_sort_map((map_t*)maps_head))
	    goto error;
	maps_head  = next;
    }

    if (ctx) {
        ctx->state = B2TDone;
	ctx->reds = reds;
    }

    *hpp = hp;
    return ep;

error:
    /* UNDO:
     * Must unlink all off-heap objects that may have been
     * linked into the process. 
     */
    if (hp < *hpp) { /* Sometimes we used hp and sometimes *hpp */
	hp = *hpp;   /* the largest must be the freshest */
    }
    undo_offheap_in_area(off_heap, hp_saved, hp);
    *hpp = hp_saved;
    if (ctx) {
	ctx->state = B2TDecodeFail;
	ctx->reds = reds;
    }
        
    return NULL;
}

/* returns the number of bytes needed to encode an object
   to a sequence of bytes
   N.B. That this must agree with to_external2() above!!!
   (except for cached atoms) */
static Uint encode_size_struct2(ErtsAtomCacheMap *acmp, Eterm obj, unsigned dflags) {
    Uint res;
    (void) encode_size_struct_int(NULL, acmp, obj, dflags, NULL, &res);
    return res;
}

static int
encode_size_struct_int(TTBSizeContext* ctx, ErtsAtomCacheMap *acmp, Eterm obj,
		       unsigned dflags, Sint *reds, Uint *res)
{
    DECLARE_ESTACK(s);
    Uint m, i, arity;
    Uint result = 0;
    Sint r = 0;

    if (ctx) {
	ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	r = *reds;

	if (ctx->estack.start) { /* restore saved stack */
	    ESTACK_RESTORE(s, &ctx->estack);
	    result = ctx->result;
	    obj = ctx->obj;
	}
    }

    goto L_jump_start;

 outer_loop:
    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    handle_popped_obj:
	if (is_list(obj)) {
	    Eterm* cons = list_val(obj);
	    Eterm tl;

	    tl = CDR(cons);
	    obj = CAR(cons);
	    ESTACK_PUSH(s, tl);
	} else if (is_nil(obj)) {
	    result++;
	    goto outer_loop;
	} else {
	    /*
	     * Other term (in the tail of a non-proper list or
	     * in a fun's environment).
	     */
	}
    
    L_jump_start:
	if (ctx && --r == 0) {
	    *reds = r;
	    ctx->obj = obj;
	    ctx->result = result;
	    ESTACK_SAVE(s, &ctx->estack);
	    return -1;
	}
	switch (tag_val_def(obj)) {
	case NIL_DEF:
	    result++;
	    break;
	case ATOM_DEF:
	    if (dflags & DFLAG_INTERNAL_TAGS) {
		if (atom_val(obj) >= (1<<16)) {
		    result += 1 + 3;
		}
		else {
		    result += 1 + 2;
		}
	    }
	    else {
		Atom *a = atom_tab(atom_val(obj));
		int alen;
		if ((dflags & DFLAG_UTF8_ATOMS) || a->latin1_chars < 0) {
		    alen = a->len;
		    result += 1 + 1 + alen;
		    if (alen > 255) {
			result++; /* ATOM_UTF8_EXT (not small) */
		    }
		}
		else {
		    alen = a->latin1_chars;
		    result += 1 + 1 + alen;
		    if (alen > 255 || !(dflags & DFLAG_SMALL_ATOM_TAGS))
			result++; /* ATOM_EXT (not small) */
		}
		insert_acache_map(acmp, obj, dflags);
	    }
	    break;
	case SMALL_DEF:
	    {
		Sint val = signed_val(obj);

		if ((Uint)val < 256)
		    result += 1 + 1;		/* SMALL_INTEGER_EXT */
		else if (sizeof(Sint) == 4 || IS_SSMALL32(val))
		    result += 1 + 4;		/* INTEGER_EXT */
		else {
		    DeclareTmpHeapNoproc(tmp_big,2);
		    UseTmpHeapNoproc(2);
		    i = big_bytes(small_to_big(val, tmp_big));
		    result += 1 + 1 + 1 + i;	/* SMALL_BIG_EXT */
		    UnUseTmpHeapNoproc(2);
		}
	    }
	    break;
	case BIG_DEF:
	    i = big_bytes(obj);
	    if (sizeof(Sint)==4 && i <= 4 && (big_digit(obj,0)-big_sign(obj)) < (1<<31))
		result += 1 + 4;          /* INTEGER_EXT */
	    else if (i < 256)
		result += 1 + 1 + 1 + i;  /* tag,size,sign,digits */
	    else
		result += 1 + 4 + 1 + i;  /* tag,size,sign,digits */
	    break;
	case PID_DEF:
	case EXTERNAL_PID_DEF:
	    result += (1 + encode_size_struct2(acmp, pid_node_name(obj), dflags) +
		       4 + 4 + 1);
	    break;
	case REF_DEF:
	case EXTERNAL_REF_DEF:
	    ASSERT(dflags & DFLAG_EXTENDED_REFERENCES);
	    i = ref_no_of_numbers(obj);
	    result += (1 + 2 + encode_size_struct2(acmp, ref_node_name(obj), dflags) +
		       1 + 4*i);
	    break;
	case PORT_DEF:
	case EXTERNAL_PORT_DEF:
	    result += (1 + encode_size_struct2(acmp, port_node_name(obj), dflags) +
		      4 + 1);
	    break;
	case LIST_DEF:
	    if ((m = is_string(obj)) && (m < MAX_STRING_LEN)) {
		result += m + 2 + 1;
	    } else {
		result += 5;
		goto handle_popped_obj;
	    }
	    break;
	case TUPLE_DEF:
	    {
		Eterm* ptr = tuple_val(obj);
		Uint i;
		arity = arityval(*ptr);
		if (arity <= 0xff) {
		    result += 1 + 1;
		} else {
		    result += 1 + 4;
		}
		for (i = 1; i <= arity; ++i) {
		    if (is_list(ptr[i])) {
			if ((m = is_string(obj)) && (m < MAX_STRING_LEN)) {
			    result += m + 2 + 1;
			} else {
			    result += 5;
			}
		    }
		    ESTACK_PUSH(s,ptr[i]);
		}
		goto outer_loop;
	    }
	    break;
	case MAP_DEF:
	    {
		map_t *mp = (map_t*)map_val(obj);
		Uint size = map_get_size(mp);
		Uint i;
		Eterm *ptr;

		result += 1 + 4; /* tag + 4 bytes size */

		/* push values first */
		ptr = map_get_values(mp);
		i   = size;
		while(i--) {
		    if (is_list(*ptr)) {
			if ((m = is_string(*ptr)) && (m < MAX_STRING_LEN)) {
			    result += m + 2 + 1;
			} else {
			    result += 5;
			}
		    }
		    ESTACK_PUSH(s,*ptr);
		    ++ptr;
		}

		ptr = map_get_keys(mp);
		i   = size;
		while(i--) {
		    if (is_list(*ptr)) {
			if ((m = is_string(*ptr)) && (m < MAX_STRING_LEN)) {
			    result += m + 2 + 1;
			} else {
			    result += 5;
			}
		    }
		    ESTACK_PUSH(s,*ptr);
		    ++ptr;
		}
		goto outer_loop;
	    }
	    break;
	case FLOAT_DEF:
	    if (dflags & DFLAG_NEW_FLOATS) {
		result += 9;
	    } else {
		result += 32;   /* Yes, including the tag */
	    }
	    break;
	case BINARY_DEF:
	    if (dflags & DFLAG_INTERNAL_TAGS) {
		ProcBin* pb = (ProcBin*) binary_val(obj);
		Uint sub_extra = 0;
		Uint tot_bytes = pb->size;
		if (pb->thing_word == HEADER_SUB_BIN) {
		    ErlSubBin* sub = (ErlSubBin*) pb;
		    pb = (ProcBin*) binary_val(sub->orig);
		    sub_extra = 2;  /* bitoffs and bitsize */
		    tot_bytes += (sub->bitoffs + sub->bitsize+ 7) / 8;
		}
		if (pb->thing_word == HEADER_PROC_BIN
		    && heap_bin_size(tot_bytes) > PROC_BIN_SIZE) {

		    result += 1 + sub_extra + sizeof(ProcBin);
		    break;
		}
	    }
	    result += 1 + 4 + binary_size(obj) +
		    5;			/* For unaligned binary */
	    break;
	case FUN_DEF:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
		
		if ((dflags & DFLAG_NEW_FUN_TAGS) != 0) {
		    result += 20+1+1+4;	/* New ID + Tag */
		    result += 4; /* Length field (number of free variables */
		    result += encode_size_struct2(acmp, funp->creator, dflags);
		    result += encode_size_struct2(acmp, funp->fe->module, dflags);
		    result += 2 * (1+4);	/* Index, Uniq */
		} else {
		    /*
		     * Size when fun is mapped to a tuple.
		     */
		    result += 1 + 1; /* Tuple tag, arity */
		    result += 1 + 1 + 2 +
			atom_tab(atom_val(am_fun))->len; /* 'fun' */
		    result += 1 + 1 + 2 +
			atom_tab(atom_val(funp->fe->module))->len; /* Module name */
		    result += 2 * (1 + 4); /* Index + Uniq */
		    result += 1 + (funp->num_free < 0x100 ? 1 : 4);
		}
		for (i = 1; i < funp->num_free; i++) {
		    obj = funp->env[i];

		    if (is_not_list(obj)) {
			/* Push any non-list terms on the stack */
			ESTACK_PUSH(s, obj);
		    } else {
			/* Lists must be handled specially. */
			if ((m = is_string(obj)) && (m < MAX_STRING_LEN)) {
			    result += m + 2 + 1;
			} else {
			    result += 5;
			    ESTACK_PUSH(s, obj);
			}
		    }
		}
		if (funp->num_free != 0) {
		    obj = funp->env[0];
		    goto L_jump_start;
		}
		break;
	    }

	case EXPORT_DEF:
	    {
		Export* ep = *((Export **) (export_val(obj) + 1));
#if HALFWORD_HEAP
		result += 2;
#else
		result += 1;
#endif
		result += encode_size_struct2(acmp, ep->code[0], dflags);
		result += encode_size_struct2(acmp, ep->code[1], dflags);
		result += encode_size_struct2(acmp, make_small(ep->code[2]), dflags);
	    }
	    break;

	default:
	    erl_exit(1,"Internal data structure error (in encode_size_struct2)%x\n",
		     obj);
	}
    }

    DESTROY_ESTACK(s);
    if (ctx) {
	ASSERT(ctx->estack.start == NULL);
	*reds = r;
    }
    *res = result;
    return 0;
}

static Sint
decoded_size(byte *ep, byte* endp, int internal_tags, B2TContext* ctx)
{
    int heap_size;
    int terms;
    int atom_extra_skip;
    Uint n;
    SWord reds;

    if (ctx) {
        reds = ctx->reds;
        if (ctx->u.sc.ep) {
            heap_size = ctx->u.sc.heap_size;
            terms = ctx->u.sc.terms;
            ep = ctx->u.sc.ep;
            atom_extra_skip = ctx->u.sc.atom_extra_skip;
            goto init_done;
        }
    }
    else
        reds = 0; /* not used but compiler warns anyway */

    heap_size = 0;
    terms = 1;
    atom_extra_skip = 0;
init_done:

#define SKIP(sz)				\
    do {					\
	if ((sz) <= endp-ep) {			\
	    ep += (sz);				\
        } else { goto error; };			\
    } while (0)

#define SKIP2(sz1, sz2)				\
    do {					\
	Uint sz = (sz1) + (sz2);		\
	if (sz1 < sz && (sz) <= endp-ep) {	\
	    ep += (sz);				\
        } else { goto error; }			\
    } while (0)

#define CHKSIZE(sz)				\
    do {					\
	 if ((sz) > endp-ep) { goto error; }	\
    } while (0)

#define ADDTERMS(n)				\
    do {					\
        int before = terms;		        \
	terms += (n);                           \
	if (terms < before) goto error;     	\
    } while (0)

    ASSERT(terms > 0);
    do {
        int tag;
	CHKSIZE(1);
	tag = ep++[0];
	switch (tag) {
	case INTEGER_EXT:
	    SKIP(4);
#if !defined(ARCH_64) || HALFWORD_HEAP
	    heap_size += BIG_UINT_HEAP_SIZE;
#endif
	    break;
	case SMALL_INTEGER_EXT:
	    SKIP(1);
	    break;
	case SMALL_BIG_EXT:
	    CHKSIZE(1);
	    n = ep[0];		/* number of bytes */
	    SKIP2(n, 1+1);		/* skip size,sign,digits */
	    heap_size += 1+(n+sizeof(Eterm)-1)/sizeof(Eterm); /* XXX: 1 too much? */
	    break;
	case LARGE_BIG_EXT:
	    CHKSIZE(4);
	    n = get_int32(ep);
	    if (n > BIG_ARITY_MAX*sizeof(ErtsDigit)) {
		goto error;
	    }
	    SKIP2(n,4+1);		/* skip, size,sign,digits */
	    heap_size += 1+1+(n+sizeof(Eterm)-1)/sizeof(Eterm); /* XXX: 1 too much? */
	    break;
	case ATOM_EXT:
	    CHKSIZE(2);
	    n = get_int16(ep);
	    if (n > MAX_ATOM_CHARACTERS) {
		goto error;
	    }
	    SKIP(n+2+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;
	case ATOM_UTF8_EXT:
	    CHKSIZE(2);
	    n = get_int16(ep);
	    ep += 2;
	    if (n > MAX_ATOM_SZ_LIMIT) {
		goto error;
	    }
	    SKIP(n+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;
	case SMALL_ATOM_EXT:
	    CHKSIZE(1);
	    n = get_int8(ep);
	    if (n > MAX_ATOM_CHARACTERS) {
		goto error;
	    }
	    SKIP(n+1+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;
	case SMALL_ATOM_UTF8_EXT:
	    CHKSIZE(1);
	    n = get_int8(ep);
	    ep++;
	    if (n > MAX_ATOM_SZ_LIMIT) {
		goto error;
	    }
	    SKIP(n+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;
	case ATOM_CACHE_REF:
	    SKIP(1+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;
	case PID_EXT:
	    atom_extra_skip = 9;
	    /* In case it is an external pid */
	    heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
	    terms++;
	    break;
	case PORT_EXT:
	    atom_extra_skip = 5;
	    /* In case it is an external port */
	    heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
	    terms++;
	    break;
	case NEW_REFERENCE_EXT:
	    {
		int id_words;

		CHKSIZE(2);
		id_words = get_int16(ep);
		    
		if (id_words > ERTS_MAX_REF_NUMBERS)
		    goto error;

		ep += 2;
		atom_extra_skip = 1 + 4*id_words;
		/* In case it is an external ref */
#if defined(ARCH_64) && !HALFWORD_HEAP
		heap_size += EXTERNAL_THING_HEAD_SIZE + id_words/2 + 1;
#else
		heap_size += EXTERNAL_THING_HEAD_SIZE + id_words;
#endif
		terms++;
		break;
	    }
	case REFERENCE_EXT:
	    /* In case it is an external ref */
	    heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
	    atom_extra_skip = 5;
	    terms++;
	    break;
	case NIL_EXT:
	    break;
	case LIST_EXT:
	    CHKSIZE(4);
	    n = get_int32(ep);
	    ep += 4;
	    ADDTERMS(n);
	    terms++;
	    heap_size += 2 * n;
	    break;
	case SMALL_TUPLE_EXT:
	    CHKSIZE(1);
	    n = *ep++;
	    terms += n;
	    heap_size += n + 1;
	    break;
	case LARGE_TUPLE_EXT:
	    CHKSIZE(4);
	    n = get_int32(ep);
	    ep += 4;
	    ADDTERMS(n);
	    heap_size += n + 1;
	    break;
	case MAP_EXT:
	    CHKSIZE(4);
	    n = get_int32(ep);
	    ep += 4;
	    ADDTERMS(2*n);
	    heap_size += 3 + n + 1 + n;
	    break;
	case STRING_EXT:
	    CHKSIZE(2);
	    n = get_int16(ep);
	    SKIP(n+2);
	    heap_size += 2 * n;
	    break;
	case FLOAT_EXT:
	    SKIP(31);
	    heap_size += FLOAT_SIZE_OBJECT;
	    break;
	case NEW_FLOAT_EXT:
	    SKIP(8);
	    heap_size += FLOAT_SIZE_OBJECT;
	    break;
	case BINARY_EXT:
	    CHKSIZE(4);
	    n = get_int32(ep);
	    SKIP2(n, 4);
	    if (n <= ERL_ONHEAP_BIN_LIMIT) {
		heap_size += heap_bin_size(n);
	    } else {
		heap_size += PROC_BIN_SIZE;
	    }
	    break;
	case BIT_BINARY_EXT:
	    {
		CHKSIZE(5);
		n = get_int32(ep);
		SKIP2(n, 5);
		if (n <= ERL_ONHEAP_BIN_LIMIT) {
		    heap_size += heap_bin_size(n) + ERL_SUB_BIN_SIZE;
		} else {
		    heap_size += PROC_BIN_SIZE + ERL_SUB_BIN_SIZE;
		}
	    }
	    break;
	case EXPORT_EXT:
	    terms += 3;
#if HALFWORD_HEAP
	    heap_size += 3;
#else
	    heap_size += 2;
#endif
	    break;
	case NEW_FUN_EXT:
	    {
		unsigned num_free;
		Uint total_size;

		CHKSIZE(1+16+4+4);
		total_size = get_int32(ep);
		CHKSIZE(total_size);		
		ep += 1+16+4+4;
		/*FALLTHROUGH*/

	    case FUN_EXT:
		CHKSIZE(4);
		num_free = get_int32(ep);
		ep += 4;
		if (num_free > MAX_ARG) {
		    goto error;
		}
		terms += 4 + num_free;
		heap_size += ERL_FUN_SIZE + num_free;
		break;
	    }
	case ATOM_INTERNAL_REF2:
	    SKIP(2+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;
	case ATOM_INTERNAL_REF3:
	    SKIP(3+atom_extra_skip);
	    atom_extra_skip = 0;
	    break;

	case BINARY_INTERNAL_REF:
	    if (!internal_tags) {
		goto error;
	    }
	    SKIP(sizeof(ProcBin));
	    heap_size += PROC_BIN_SIZE;
	    break;
	case BIT_BINARY_INTERNAL_REF:
	    if (!internal_tags) {
		goto error;
	    }
	    SKIP(2+sizeof(ProcBin));
	    heap_size += PROC_BIN_SIZE + ERL_SUB_BIN_SIZE;
	    break;
	default:
	    goto error;
	}
        terms--;

        if (ctx && --reds <= 0 && terms > 0) {
            ctx->u.sc.heap_size = heap_size;
            ctx->u.sc.terms = terms;
            ctx->u.sc.ep = ep;
            ctx->u.sc.atom_extra_skip = atom_extra_skip;
            ctx->reds = 0;
            return 0;
        }
    }while (terms > 0);

    /* 'terms' may be non-zero if it has wrapped around */
    if (terms == 0) {
        if (ctx) {
            ctx->state = B2TDecodeInit;
            ctx->reds = reds;
        }
        return heap_size;
    }

error:
    if (ctx) {
        ctx->state = B2TBadArg;
    }
    return -1;
#undef SKIP
#undef SKIP2
#undef CHKSIZE
}


#ifdef HIPE
BIF_RETTYPE hipe_wrapper_term_to_binary_1(BIF_ALIST_1);
BIF_RETTYPE hipe_wrapper_term_to_binary_2(BIF_ALIST_2);
BIF_RETTYPE hipe_wrapper_erts_internal_binary_to_term_1(BIF_ALIST_1);
BIF_RETTYPE hipe_wrapper_erts_internal_binary_to_term_2(BIF_ALIST_2);

/* Hipe wrappers used by native code for BIFs that disable GC while trapping.
 *
 * Problem:
 * When native code calls a BIF that traps, hipe_mode_switch will push a
 * "trap frame" on the Erlang stack in order to find its way back from beam_emu
 * back to native caller when finally done. If GC is disabled and stack/heap
 * is full there is no place to push the "trap frame".
 *
 * Solution:
 * We reserve space on stack for the "trap frame" here before the BIF is called.
 * If the BIF does not trap, the space is reclaimed here before returning.
 * If the BIF traps, hipe_push_beam_trap_frame() will detect that a "trap frame"
 * already is reserved and use it.
 */
BIF_RETTYPE hipe_wrapper_term_to_binary_1(BIF_ALIST_1)
{
    Eterm res;
    hipe_reserve_beam_trap_frame(BIF_P, BIF__ARGS, 1);
    res = term_to_binary_1(BIF_P, BIF__ARGS);
    if (is_value(res) || BIF_P->freason != TRAP) {
	hipe_unreserve_beam_trap_frame(BIF_P);
    }
    return res;
}
BIF_RETTYPE hipe_wrapper_term_to_binary_2(BIF_ALIST_2)
{
    Eterm res;
    hipe_reserve_beam_trap_frame(BIF_P, BIF__ARGS, 2);
    res = term_to_binary_2(BIF_P, BIF__ARGS);
    if (is_value(res) || BIF_P->freason != TRAP) {
	hipe_unreserve_beam_trap_frame(BIF_P);
    }
    return res;
}
BIF_RETTYPE hipe_wrapper_erts_internal_binary_to_term_1(BIF_ALIST_1)
{
    Eterm res;
    hipe_reserve_beam_trap_frame(BIF_P, BIF__ARGS, 1);
    res = erts_internal_binary_to_term_1(BIF_P, BIF__ARGS);
    if (is_value(res) || BIF_P->freason != TRAP) {
	hipe_unreserve_beam_trap_frame(BIF_P);
    }
    return res;
}
BIF_RETTYPE hipe_wrapper_erts_internal_binary_to_term_2(BIF_ALIST_2)
{
    Eterm res;
    hipe_reserve_beam_trap_frame(BIF_P, BIF__ARGS, 2);
    res = erts_internal_binary_to_term_2(BIF_P, BIF__ARGS);
    if (is_value(res) || BIF_P->freason != TRAP) {
	hipe_unreserve_beam_trap_frame(BIF_P);
    }
    return res;
}
#endif /*HIPE*/
