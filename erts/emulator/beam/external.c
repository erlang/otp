/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
#include "erl_proc_sig_queue.h"
#include "erl_trace.h"

#define PASS_THROUGH 'p'

#define in_area(ptr,start,nbytes) ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

#define MAX_STRING_LEN 0xffff

/*
 * MAX value for the creation field in pid, port and reference
 * for the old PID_EXT, PORT_EXT, REFERENCE_EXT and NEW_REFERENCE_EXT.
 * Older nodes (OTP 19-22) will send us these so we must be able to decode them.
 *
 * From OTP 23 DFLAG_BIG_CREATION is mandatory so this node will always
 * encode with new big 32-bit creations using NEW_PID_EXT, NEW_PORT_EXT
 * and NEWER_REFERENCE_EXT.
*/
#define ERTS_MAX_TINY_CREATION (3)
#define is_tiny_creation(Cre) ((unsigned)(Cre) <= ERTS_MAX_TINY_CREATION)

/*
 *   When 0 is used as creation, the real creation
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

static Export term_to_binary_trap_export;

static byte* enc_term(ErtsAtomCacheMap *, Eterm, byte*, Uint64, struct erl_off_heap_header** off_heap);
struct TTBEncodeContext_;
static int enc_term_int(struct TTBEncodeContext_*,ErtsAtomCacheMap *acmp, Eterm obj, byte* ep, Uint64 dflags,
			struct erl_off_heap_header** off_heap, Sint *reds, byte **res);
static int is_external_string(Eterm obj, Uint* lenp);
static byte* enc_atom(ErtsAtomCacheMap *, Eterm, byte*, Uint64);
static byte* enc_pid(ErtsAtomCacheMap *, Eterm, byte*, Uint64);
struct B2TContext_t;
static const byte* dec_term(ErtsDistExternal*, ErtsHeapFactory*, const byte*, Eterm*, struct B2TContext_t*, int);
static const byte* dec_atom(ErtsDistExternal *, const byte*, Eterm*);
static const byte* dec_pid(ErtsDistExternal *, ErtsHeapFactory*, const byte*, Eterm*, byte tag);
static Sint decoded_size(const byte *ep, const byte* endp, int internal_tags, struct B2TContext_t*);
static BIF_RETTYPE term_to_binary_trap_1(BIF_ALIST_1);

static Eterm erts_term_to_binary_int(Process* p, Sint bif_ix, Eterm Term, Eterm opts, int level,
                                     Uint64 dflags, Binary *context_b, int iovec,
                                     Uint fragment_size);

static Uint encode_size_struct2(ErtsAtomCacheMap *, Eterm, Uint64);
static ErtsExtSzRes encode_size_struct_int(TTBSizeContext*, ErtsAtomCacheMap *acmp,
                                           Eterm obj, Uint64 dflags, Sint *reds, Uint *res);

static Export binary_to_term_trap_export;
static BIF_RETTYPE binary_to_term_trap_1(BIF_ALIST_1);
static Sint transcode_dist_obuf(ErtsDistOutputBuf*, DistEntry*, Uint64 dflags, Sint reds);
static void store_in_vec(TTBEncodeContext *ctx, byte *ep, Binary *ohbin, Eterm ohpb,
                         byte *ohp, Uint ohsz);

void erts_init_external(void) {
    erts_init_trap_export(&term_to_binary_trap_export,
			  am_erts_internal, am_term_to_binary_trap, 1,
			  &term_to_binary_trap_1);

    erts_init_trap_export(&binary_to_term_trap_export,
			  am_erts_internal, am_binary_to_term_trap, 1,
			  &binary_to_term_trap_1);
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
insert_acache_map(ErtsAtomCacheMap *acmp, Eterm atom, Uint64 dflags)
{
    if (acmp && acmp->sz < ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) {
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
get_iix_acache_map(ErtsAtomCacheMap *acmp, Eterm atom, Uint64 dflags)
{
    if (!acmp)
	return -1;
    else {
	int ix;
	ASSERT(is_atom(atom));
	ix = atom2cix(atom);
	if (acmp->cache[ix].iix < 0) {
	    ASSERT(acmp->sz == ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES);
	    return -1;
	}
	else {
	    ASSERT(acmp->cache[ix].iix < ERTS_ATOM_CACHE_SIZE);
	    return acmp->cache[ix].atom == atom ? acmp->cache[ix].iix : -1;
	}
    }
}

void
erts_finalize_atom_cache_map(ErtsAtomCacheMap *acmp, Uint64 dflags)
{
    if (acmp) {
	int long_atoms = 0; /* !0 if one or more atoms are longer than 255. */
	int i;
	int sz = 0;
	int min_sz;
	ASSERT(acmp->hdr_sz < 0);
	/* Make sure cache update instructions fit */
	min_sz = (2+4)*acmp->sz;
	for (i = 0; i < acmp->sz; i++) {
	    Atom *a;
	    Eterm atom;
	    int len;
	    atom = acmp->cache[acmp->cix[i]].atom;
	    ASSERT(is_atom(atom));
	    a = atom_tab(atom_val(atom));
	    len = (int) a->len;
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
erts_encode_ext_dist_header_size(TTBEncodeContext *ctx,
                                 ErtsAtomCacheMap *acmp,
                                 Uint fragments)
{
    if (ctx->dflags & DFLAG_PENDING_CONNECT) {
        /* HOPEFUL_DATA + hopefull flags + hopefull ix + payload ix */
        return 1 + 8 + 4 + 4;
    }
    else if (!acmp && !(ctx->dflags & DFLAG_FRAGMENTS))
	return 1; /* pass through */
    else {
        int fix_sz
            = 1 /* VERSION_MAGIC */
            + 1 /* DIST_HEADER */
            + 1 /* dist header flags */
            + 1 /* number of internal cache entries */
            ;

        if (fragments > 1)
            fix_sz += 8 /* sequence id */
                + 8 /* number of fragments */
                ;
        if (acmp) {
            ASSERT(acmp->hdr_sz >= 0);
            fix_sz += acmp->hdr_sz;
        } else {
            ASSERT(ctx->dflags & DFLAG_FRAGMENTS);
        }

        return fix_sz;
    }
}

byte *erts_encode_ext_dist_header_setup(TTBEncodeContext *ctx,
                                        byte *ctl_ext, ErtsAtomCacheMap *acmp,
                                        Uint fragments, Eterm from)
{
    /* Maximum number of atom must be less than the maximum of a 32 bits
       unsigned integer. Check is done in erl_init.c, erl_start function. */
    if (ctx->dflags & DFLAG_PENDING_CONNECT) {
        byte *ep = ctl_ext;
        ep -= 4;
        ctx->payload_ixp = ep;
        put_int32(0, ep);
        ep -= 4;
        ctx->hopefull_ixp = ep;
        put_int32(ERTS_NO_HIX, ep);
        ep -= 8;
        ctx->hopefull_flagsp = ep;
        put_int64(0, ep);
        *--ep = HOPEFUL_DATA;
        return ep;
    }
    else if (!acmp && !(ctx->dflags & DFLAG_FRAGMENTS)) {
        byte *ep = ctl_ext;
        *--ep = PASS_THROUGH;
	return ep;
    }
    else {
	int i;
	byte *ep = ctl_ext;
	byte dist_hdr_flags = acmp && acmp->long_atoms ? ERTS_DIST_HDR_LONG_ATOMS_FLG : 0;
	ASSERT(!acmp || acmp->hdr_sz >= 0);

        if (acmp) {
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
        } else {
            ASSERT(ctx->dflags & DFLAG_FRAGMENTS);
            /* If we don't have an atom cache but are using a dist header we just put 0
               in the atom cache size slot */
            --ep;
            put_int8(0, ep);
        }
	--ep;
	put_int8(dist_hdr_flags, ep);
        if (fragments > 1) {
            ASSERT(is_pid(from));
            ep -= 8;
            put_int64(fragments, ep);
            ep -= 8;
            put_int64(from, ep);
            *--ep = DIST_FRAG_HEADER;
        } else {
            *--ep = DIST_HEADER;
        }
        *--ep = VERSION_MAGIC;
	return ep;
    }
}

byte *erts_encode_ext_dist_header_fragment(byte **hdrpp,
                                           Uint fragment,
                                           Eterm from)
{
    byte *ep = *hdrpp, *start = ep;
    ASSERT(is_pid(from));
    *ep++ = VERSION_MAGIC;
    *ep++ = DIST_FRAG_CONT;
    put_int64(from, ep);
    ep += 8;
    put_int64(fragment, ep);
    ep += 8;
    *hdrpp = ep;
    return start;
}


Sint erts_encode_ext_dist_header_finalize(ErtsDistOutputBuf* ob,
                                          DistEntry* dep,
                                          Uint64 dflags,
                                          Sint reds)
{
    byte *ip;
    byte instr_buf[(2+4)*ERTS_ATOM_CACHE_SIZE];
    int ci, sz;
    byte dist_hdr_flags;
    int long_atoms;
    Uint64 seq_id = 0, frag_id = 0;
    register byte *ep = ob->eiov->iov[1].iov_base;

    /*
     * The buffer can have different layouts at this point depending on
     * what was known when encoded:
     *
     * Pending connection: HOPEFUL_DATA, HFlgs, HIX, PIX, CtrlTerm [, MsgTerm]
     * With atom cache   : VERSION_MAGIC, DIST_HEADER, ..., CtrlTerm [, MsgTerm]
     * No atom cache     : VERSION_MAGIC, CtrlTerm [, VERSION_MAGIC, MsgTerm]
     */

    if (ep[0] == HOPEFUL_DATA)
        return transcode_dist_obuf(ob, dep, dflags, reds);

    if (ep[0] == PASS_THROUGH) {
        ASSERT(!(dflags & (DFLAG_DIST_HDR_ATOM_CACHE|DFLAG_FRAGMENTS)));
        ASSERT(ob->eiov->iov[1].iov_len == 1);
        return reds;
    }

    if (ep[1] == DIST_FRAG_CONT) {
        ASSERT(ep[0] == VERSION_MAGIC);
        ASSERT(ob->eiov->iov[1].iov_len == 18);
        return reds;
    }

    if (ep[1] == DIST_FRAG_HEADER) {
        /* skip the seq id and frag id */
        seq_id = get_int64(&ep[2]);
        ep += 8;
        frag_id = get_int64(&ep[2]);
        ep += 8;
    }

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
    ASSERT(ep == &((byte *)ob->eiov->iov[1].iov_base)[ob->eiov->iov[1].iov_len]);
    if (ci > 0) {
	Uint32 flgs_buf[((ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(
			      ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES)-1)
			 / sizeof(Uint32))+1];
	register Uint32 flgs;
	int iix, flgs_bytes, flgs_buf_ix, used_half_bytes;
        ErtsAtomCache* cache = dep->cache;
#ifdef DEBUG
	int tot_used_half_bytes, top_buf_ix;
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
	    atom = make_atom((Uint) get_uint32(&ip[2]));
	    if (cache->out_arr[cix] == atom) {
		--ep;
		put_int8(cix, ep);
		flgs |= ((cix >> 8) & 7);
	    }
	    else {
		Atom *a;
		cache->out_arr[cix] = atom;
		a = atom_tab(atom_val(atom));
                sz = a->len;
                ep -= sz;
                sys_memcpy((void *) ep, (void *) a->name, sz);
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
#ifdef DEBUG
        top_buf_ix = flgs_buf_ix;
#endif
	flgs_buf_ix = 0;
	while (1) {
            ASSERT(flgs_buf_ix <= top_buf_ix);
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
                ASSERT(flgs_buf_ix == top_buf_ix);
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
        reds -= 3; /*was ERTS_PORT_REDS_DIST_CMD_FINALIZE*/
    }
    --ep;
    put_int8(ci, ep);
    if (seq_id) {
        ep -= 8;
        put_int64(frag_id, ep);
        ep -= 8;
        put_int64(seq_id, ep);
        *--ep = DIST_FRAG_HEADER;
    } else {
        *--ep = DIST_HEADER;
    }
    *--ep = VERSION_MAGIC;

    sz = ((byte *) ob->eiov->iov[1].iov_base) - ep;
    ob->eiov->size += sz;
    ob->eiov->iov[1].iov_len += sz;
    ob->eiov->iov[1].iov_base = ep;

    return reds < 0 ? 0 : reds;
}

ErtsExtSzRes
erts_encode_dist_ext_size(Eterm term,
                          ErtsAtomCacheMap *acmp,
                          TTBSizeContext* ctx,
                          Uint* szp, Sint *redsp,
                          Sint *vlenp, Uint *fragmentsp)
{
    Uint sz;
    ErtsExtSzRes res;

    ASSERT(ctx);
    ASSERT(szp);
    ASSERT(vlenp);
    ASSERT(fragmentsp);

    sz = *szp;

    if (!ctx->wstack.wstart) {
        /*
         * First call for this 'term'. We might however encode
         * multiple terms and this might not be the first term
         * in the sequence. 'ctx' should contain valid info about
         * about previous terms regarding fragments, and vlen.
         * 'szp' should contain valid info about the total size
         * of previous terms.
         */
        if (ctx->vlen < 0) {
            /* First term as well */
            ctx->vlen = 0;
            if (ctx->dflags & DFLAG_FRAGMENTS)
                ctx->fragment_size = ERTS_DIST_FRAGMENT_SIZE;
        }

#ifndef ERTS_DEBUG_USE_DIST_SEP
	if (!(ctx->dflags & (DFLAG_DIST_HDR_ATOM_CACHE|DFLAG_FRAGMENTS)))
#endif
	    sz++ /* VERSION_MAGIC */;

    }

    res = encode_size_struct_int(ctx, acmp, term, ctx->dflags, redsp, &sz);

    if (res == ERTS_EXT_SZ_OK) {
        Uint total_size, fragments;

        /*
         * Each fragment use
         * - one element for driver header
         * - one element for fragment header
         * - and (at least) one for data
         */
        total_size = sz + ctx->extra_size;
        fragments = (total_size - 1)/ctx->fragment_size + 1;

	*szp = sz;
        *fragmentsp = fragments;
        *vlenp = ctx->vlen + 3*fragments;
    }

    return res;
}

ErtsExtSzRes erts_encode_ext_size_2(Eterm term, unsigned dflags, Uint *szp)
{
    ErtsExtSzRes res;
    *szp = 0;
    res = encode_size_struct_int(NULL, NULL, term, dflags, NULL, szp);
    (*szp)++ /* VERSION_MAGIC */;
    return res;
}

ErtsExtSzRes erts_encode_ext_size(Eterm term, Uint *szp)
{
    return erts_encode_ext_size_2(term, TERM_TO_BINARY_DFLAGS, szp);
}

Uint erts_encode_ext_size_ets(Eterm term)
{
    return encode_size_struct2(NULL, term,
                               TERM_TO_BINARY_DFLAGS|DFLAG_ETS_COMPRESSED);
}


int erts_encode_dist_ext(Eterm term, byte **ext, Uint64 flags, ErtsAtomCacheMap *acmp,
                         TTBEncodeContext* ctx, Uint *fragmentsp, Sint* reds)
{
    int res;
    ASSERT(ctx);
    
    if (!ctx->wstack.wstart) {
        ctx->cptr = *ext;
#ifndef ERTS_DEBUG_USE_DIST_SEP
	if (!(flags & (DFLAG_DIST_HDR_ATOM_CACHE|DFLAG_PENDING_CONNECT|DFLAG_FRAGMENTS)))
#endif
	    *(*ext)++ = VERSION_MAGIC;
#ifndef ERTS_DEBUG_USE_DIST_SEP
        if (flags & DFLAG_PENDING_CONNECT) {
            Sint payload_ix = ctx->vlen;
            ASSERT(ctx->payload_ixp);
            if (payload_ix) {
                /* we potentially need a version magic on the payload... */
                (*ext)++;
                ctx->cptr = *ext;
                put_int32(payload_ix, ctx->payload_ixp);
            }
        }
#endif
    }
    res = enc_term_int(ctx, acmp, term, *ext, flags, NULL, reds, ext);
    if (fragmentsp)
        *fragmentsp = res == 0 ? ctx->frag_ix + 1 : ctx->frag_ix;
    if (flags & DFLAG_PENDING_CONNECT) {
        ASSERT(ctx->hopefull_flagsp);
        put_int64(ctx->hopefull_flags, ctx->hopefull_flagsp);
    }
    return res;
}

void erts_encode_ext(Eterm term, byte **ext)
{
    byte *ep = *ext;
    *ep++ = VERSION_MAGIC;
    ep = enc_term(NULL, term, ep, TERM_TO_BINARY_DFLAGS, NULL);
    if (!ep)
	erts_exit(ERTS_ABORT_EXIT,
		 "%s:%d:erts_encode_ext(): Internal data structure error\n",
		 __FILE__, __LINE__);
    *ext = ep;
}

byte* erts_encode_ext_ets(Eterm term, byte *ep, struct erl_off_heap_header** off_heap)
{
    return enc_term(NULL, term, ep, TERM_TO_BINARY_DFLAGS|DFLAG_ETS_COMPRESSED,
		    off_heap);
}


static Uint
dist_ext_size(ErtsDistExternal *edep)
{
    Uint sz = sizeof(ErtsDistExternal);

    ASSERT(edep->data->ext_endp && edep->data->extp);
    ASSERT(edep->data->ext_endp >= edep->data->extp);

    if (edep->flags & ERTS_DIST_EXT_ATOM_TRANS_TAB) {
        ASSERT(0 <= edep->attab.size \
               && edep->attab.size <= ERTS_ATOM_CACHE_SIZE);
        sz -= sizeof(Eterm)*(ERTS_ATOM_CACHE_SIZE - edep->attab.size);
    } else {
        sz -= sizeof(ErtsAtomTranslationTable);
    }
    ASSERT(sz % 4 == 0);
    return sz;
}

Uint
erts_dist_ext_size(ErtsDistExternal *edep)
{
    Uint sz = dist_ext_size(edep);
    sz += 4;  /* may need to pad to 8-byte-align ErtsDistExternalData */
    sz += edep->data[0].frag_id * sizeof(ErtsDistExternalData);
    return sz;
}

Uint
erts_dist_ext_data_size(ErtsDistExternal *edep)
{
    Uint sz = 0, i;
    for (i = 0; i < edep->data->frag_id; i++)
        sz += edep->data[i].ext_endp - edep->data[i].extp;
    return sz;
}

void
erts_dist_ext_frag(ErtsDistExternalData *ede_datap, ErtsDistExternal *edep)
{
    ErtsDistExternalData *new_ede_datap = &edep->data[edep->data->frag_id - ede_datap->frag_id];
    sys_memcpy(new_ede_datap, ede_datap, sizeof(ErtsDistExternalData));

    /* If the data is not backed by a binary, we create one here to keep
       things simple. Only custom distribution drivers should use lists. */
    if (new_ede_datap->binp == NULL) {
        size_t ext_sz = ede_datap->ext_endp - ede_datap->extp;
        new_ede_datap->binp = erts_bin_nrml_alloc(ext_sz);
        sys_memcpy(new_ede_datap->binp->orig_bytes, (void *) ede_datap->extp, ext_sz);
        new_ede_datap->extp = (byte*)new_ede_datap->binp->orig_bytes;
        new_ede_datap->ext_endp = (byte*)new_ede_datap->binp->orig_bytes + ext_sz;
    } else {
        erts_refc_inc(&new_ede_datap->binp->intern.refc, 2);
    }
}

void
erts_make_dist_ext_copy(ErtsDistExternal *edep, ErtsDistExternal *new_edep)
{
    size_t dist_ext_sz = dist_ext_size(edep);
    byte *ep;

    ep = (byte *) new_edep;
    sys_memcpy((void *) ep, (void *) edep, dist_ext_sz);
    erts_ref_dist_entry(new_edep->dep);

    ep += dist_ext_sz;
    ep += (UWord)ep & 4; /* 8-byte alignment for ErtsDistExternalData */
    ASSERT((UWord)ep % 8 == 0);

    new_edep->data = (ErtsDistExternalData*)ep;
    sys_memzero(new_edep->data, sizeof(ErtsDistExternalData) * edep->data->frag_id);
    new_edep->data->frag_id = edep->data->frag_id;
    erts_dist_ext_frag(edep->data, new_edep);
}

void
erts_free_dist_ext_copy(ErtsDistExternal *edep)
{
    int i;
    erts_deref_dist_entry(edep->dep);
    for (i = 0; i < edep->data->frag_id; i++)
        if (edep->data[i].binp)
            erts_bin_release(edep->data[i].binp);
}

ErtsPrepDistExtRes
erts_prepare_dist_ext(ErtsDistExternal *edep,
		      byte *ext,
		      Uint size,
                      Binary *binp,
		      DistEntry *dep,
                      Uint32 conn_id,
		      ErtsAtomCache *cache)
{
    register byte *ep;

    ASSERT(dep);
    erts_de_rlock(dep);

    if ((dep->state != ERTS_DE_STATE_CONNECTED &&
         dep->state != ERTS_DE_STATE_PENDING)
        || dep->connection_id != conn_id) {
        erts_de_runlock(dep);
        return ERTS_PREP_DIST_EXT_CLOSED;
    }

    if (!(dep->dflags & (DFLAG_DIST_HDR_ATOM_CACHE|DFLAG_FRAGMENTS))) {
        /* Skip PASS_THROUGH */
        ext++;
        size--;
    }

    ep = ext;

    if (size < 2)
        goto fail;

    if (ep[0] != VERSION_MAGIC) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
        erts_dsprintf(dsbufp,
                      "** Got message from incompatible erlang on "
                      "channel %d\n",
                      dist_entry_channel_no(dep));
	erts_send_error_to_logger_nogl(dsbufp);
	goto fail;
    }

    edep->heap_size = -1;
    edep->flags = 0;
    edep->dep = dep;
    edep->mld = dep->mld;
    edep->connection_id = conn_id;
    edep->data->ext_endp = ext+size;
    edep->data->binp = binp;
    edep->data->seq_id = 0;
    edep->data->frag_id = 1;

    if (dep->dflags & (DFLAG_DIST_HDR_ATOM_CACHE|DFLAG_FRAGMENTS))
        edep->flags |= ERTS_DIST_EXT_DFLAG_HDR;

    if (ep[1] != DIST_HEADER && ep[1] != DIST_FRAG_HEADER && ep[1] != DIST_FRAG_CONT) {
	if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR)
	    goto bad_hdr;
	edep->attab.size = 0;
	edep->data->extp = ext;
    }
    else if (ep[1] == DIST_FRAG_CONT) {
        if (!(dep->dflags & DFLAG_FRAGMENTS))
            goto bad_hdr;
        edep->attab.size = 0;
	edep->data->extp = ext + 1 + 1 + 8 + 8;
        edep->data->seq_id = get_int64(&ep[2]);
        edep->data->frag_id = get_int64(&ep[2+8]);
        erts_de_runlock(dep);
        return ERTS_PREP_DIST_EXT_FRAG_CONT;
    }
    else {
	int tix;
	int no_atoms;

	if (!(edep->flags & ERTS_DIST_EXT_DFLAG_HDR))
	    goto bad_hdr;

        if (ep[1] == DIST_FRAG_HEADER) {
            if (!(dep->dflags & DFLAG_FRAGMENTS))
                goto bad_hdr;
            edep->data->seq_id = get_int64(&ep[2]);
            edep->data->frag_id = get_int64(&ep[2+8]);
            ep += 16;
        }

#undef CHKSIZE
#define CHKSIZE(SZ) \
	do { if ((SZ) > edep->data->ext_endp - ep) goto bad_hdr; } while(0)

	CHKSIZE(1+1+1);
	ep += 2;
	no_atoms = (int) get_int8(ep);
	if (no_atoms < 0 || ERTS_ATOM_CACHE_SIZE < no_atoms)
	    goto bad_hdr;
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
			goto bad_hdr;
		    ep++;
		    atom = cache->in_arr[cix];
		    if (!is_atom(atom))
			goto bad_hdr;
		    edep->attab.atom[tix] = atom;
		}
		else {
		    /* new cached atom */
		    cix += (int) get_int8(ep);
		    if (cix >= ERTS_ATOM_CACHE_SIZE)
			goto bad_hdr;
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
                                         ERTS_ATOM_ENC_UTF8,
					 0);
		    if (is_non_value(atom))
			goto bad_hdr;
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
	edep->data->extp = ep;
#ifdef ERTS_DEBUG_USE_DIST_SEP
	if (*ep != VERSION_MAGIC)
	    goto bad_hdr;
#endif
    }
#ifdef ERTS_DEBUG_USE_DIST_SEP
    if (*ep != VERSION_MAGIC)
	goto fail;
#endif

    erts_de_runlock(dep);

    return ERTS_PREP_DIST_EXT_SUCCESS;

#undef CHKSIZE

 bad_hdr: {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "%T got a corrupted distribution header from %T "
		      "on distribution channel %d\n",
		      erts_this_node->sysname,
		      edep->dep->sysname,
		      dist_entry_channel_no(edep->dep));
	for (ep = ext; ep < edep->data->ext_endp; ep++)
	    erts_dsprintf(dsbufp, ep != ext ? ",%b8u" : "<<%b8u", *ep);
	erts_dsprintf(dsbufp, ">>");
	erts_send_warning_to_logger_nogl(dsbufp);
    }
 fail: {
	erts_de_runlock(dep);
	erts_kill_dist_connection(dep, conn_id);
    }
    return ERTS_PREP_DIST_EXT_FAILED;
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
	for (ep = edep->data->extp; ep < edep->data->ext_endp; ep++)
	    erts_dsprintf(dsbufp,
			  ep != edep->data->extp ? ",%b8u" : "<<...,%b8u",
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
	erts_kill_dist_connection(dep, edep->connection_id);
    }
}

Sint
erts_decode_dist_ext_size(ErtsDistExternal *edep, int kill_connection, int payload)
{
    Sint res;
    byte *ep;

    if (edep->data->frag_id > 1 && payload) {
        Uint sz = 0;
        Binary *bin;
        int i;
        byte *ep;

        for (i = 0; i < edep->data->frag_id; i++)
            sz += edep->data[i].ext_endp - edep->data[i].extp;

        bin = erts_bin_nrml_alloc(sz);
        ep = (byte*)bin->orig_bytes;

        for (i = 0; i < edep->data->frag_id; i++) {
            sys_memcpy(ep, edep->data[i].extp, edep->data[i].ext_endp - edep->data[i].extp);
            ep += edep->data[i].ext_endp - edep->data[i].extp;
            erts_bin_release(edep->data[i].binp);
            edep->data[i].binp = NULL;
            edep->data[i].extp = NULL;
            edep->data[i].ext_endp = NULL;
        }

        edep->data->frag_id = 1;
        edep->data->extp = (byte*)bin->orig_bytes;
        edep->data->ext_endp = ep;
        edep->data->binp = bin;
    }

    if (edep->data->extp >= edep->data->ext_endp)
	goto fail;
#ifndef ERTS_DEBUG_USE_DIST_SEP
    if (edep->flags & ERTS_DIST_EXT_DFLAG_HDR) {
	if (*edep->data->extp == VERSION_MAGIC)
	    goto fail;
	ep = edep->data->extp;
    }
    else
#endif
    {
	if (*edep->data->extp != VERSION_MAGIC)
	    goto fail;
	ep = edep->data->extp+1;
    }
    res = decoded_size(ep, edep->data->ext_endp, 0, NULL);
    if (res >= 0)
	return res;
 fail:
    if (kill_connection)
        bad_dist_ext(edep);
    return -1;
}

Sint erts_decode_ext_size(const byte *ext, Uint size)
{
    if (size == 0 || *ext != VERSION_MAGIC)
	return -1;
    return decoded_size(ext+1, ext+size, 0, NULL);
}

Sint erts_decode_ext_size_ets(const byte *ext, Uint size)
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
erts_decode_dist_ext(ErtsHeapFactory* factory,
		     ErtsDistExternal *edep,
                     int kill_connection)
{
    Eterm obj;
    const byte* ep;

    ep = edep->data->extp;

    if (ep >= edep->data->ext_endp)
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
    ep = dec_term(edep, factory, ep, &obj, NULL, 0);
    if (!ep)
	goto error;

    edep->data->extp = (byte*)ep;

    return obj;

 error:
    erts_factory_undo(factory);

    if (kill_connection)
        bad_dist_ext(edep);

    return THE_NON_VALUE;
}

Eterm erts_decode_ext(ErtsHeapFactory* factory, const byte **ext, Uint32 flags)
{
    ErtsDistExternal ede, *edep;
    Eterm obj;
    const byte *ep = *ext;
    if (*ep++ != VERSION_MAGIC) {
        erts_factory_undo(factory);
	return THE_NON_VALUE;
    }
    if (flags) {
        ASSERT(flags == ERTS_DIST_EXT_BTT_SAFE);
        ede.flags = flags; /* a dummy struct just for the flags */
        ede.data = NULL;
        edep = &ede;
    } else {
        edep = NULL;
    }
    ep = dec_term(edep, factory, ep, &obj, NULL, 0);
    if (!ep) {
	return THE_NON_VALUE;
    }
    *ext = ep;
    return obj;
}

Eterm erts_decode_ext_ets(ErtsHeapFactory* factory, const byte *ext)
{
    Eterm obj;
    ext = dec_term(NULL, factory, ext, &obj, NULL, 1);
    ASSERT(ext);
    return obj;
}

/**********************************************************************/

BIF_RETTYPE erts_debug_dist_ext_to_term_2(BIF_ALIST_2)
{
    ErtsHeapFactory factory;
    Eterm res;
    Sint hsz;
    ErtsDistExternal ede;
    ErtsDistExternalData ede_data;
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
    ede.data = &ede_data;

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

    ede.data->extp = binary_bytes(real_bin)+offset;
    ede.data->ext_endp = ede.data->extp + size;
    ede.data->frag_id = 1;
    ede.data->binp = NULL;

    hsz = erts_decode_dist_ext_size(&ede, 1, 1);
    if (hsz < 0)
	goto badarg;

    erts_factory_proc_prealloc_init(&factory, BIF_P, hsz);
    res = erts_decode_dist_ext(&factory, &ede, 1);
    erts_factory_close(&factory);

    if (is_value(res))
	BIF_RET(res);

 badarg:

    BIF_ERROR(BIF_P, BADARG);
}

static BIF_RETTYPE term_to_binary_trap_1(BIF_ALIST_1)
{
    Eterm *tp = tuple_val(BIF_ARG_1);
    Eterm Term = tp[1];
    Eterm Opts = tp[2];
    Eterm bt = tp[3];
    Eterm bix = tp[4];
    Sint bif_ix = signed_val(bix);
    Binary *bin = erts_magic_ref2bin(bt);
    Eterm res = erts_term_to_binary_int(BIF_P, bif_ix, Term, Opts,
                                        0, 0,bin, 0, ~((Uint) 0));
    if (is_non_value(res)) {
        if (erts_set_gc_state(BIF_P, 1)
            || MSO(BIF_P).overhead > BIN_VHEAP_SZ(BIF_P)) {
            ERTS_VBUMP_ALL_REDS(BIF_P);
        }
        if (Opts == am_undefined)
            ERTS_BIF_ERROR_TRAPPED1(BIF_P, SYSTEM_LIMIT,
                                    BIF_TRAP_EXPORT(bif_ix), Term);
        else
            ERTS_BIF_ERROR_TRAPPED2(BIF_P, SYSTEM_LIMIT,
                                    BIF_TRAP_EXPORT(bif_ix), Term, Opts);
    }
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
    Eterm res = erts_term_to_binary_int(BIF_P, BIF_term_to_binary_1,
                                        BIF_ARG_1, am_undefined,
                                        0, TERM_TO_BINARY_DFLAGS, NULL, 0,
                                        ~((Uint) 0));
    if (is_non_value(res)) {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }
    if (is_tuple(res)) {
	erts_set_gc_state(BIF_P, 0);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
	BIF_RET(res);
    }
}

BIF_RETTYPE term_to_iovec_1(BIF_ALIST_1)
{
    Eterm res = erts_term_to_binary_int(BIF_P, BIF_term_to_iovec_1,
                                        BIF_ARG_1, am_undefined,
                                        0, TERM_TO_BINARY_DFLAGS, NULL, !0,
                                        ~((Uint) 0));
    if (is_non_value(res)) {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }
    if (is_tuple(res)) {
	erts_set_gc_state(BIF_P, 0);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
	BIF_RET(res);
    }
}

static ERTS_INLINE int
parse_t2b_opts(Eterm opts, Uint *flagsp, int *levelp, int *iovecp, Uint *fsizep)
{
    int level = 0;
    int iovec = 0;
    Uint flags = TERM_TO_BINARY_DFLAGS;
    Uint fsize = ~((Uint) 0); /* one fragment */

    while (is_list(opts)) {
	Eterm arg = CAR(list_val(opts));
	Eterm* tp;
	if (arg == am_compressed) {
	    level = Z_DEFAULT_COMPRESSION;
        }
        else if (iovecp && arg == am_iovec) {
            iovec = !0;
	} else if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
	    if (tp[1] == am_minor_version && is_small(tp[2])) {
		switch (signed_val(tp[2])) {
		case 0:
		    flags = TERM_TO_BINARY_DFLAGS & ~DFLAG_NEW_FLOATS;
		    break;
		case 1: /* Current default... */
		    flags = TERM_TO_BINARY_DFLAGS;
                    break;
                case 2:
                    flags = TERM_TO_BINARY_DFLAGS | DFLAG_UTF8_ATOMS;
		    break;
		default:
                    return 0; /* badarg */
		}
	    } else if (tp[1] == am_compressed && is_small(tp[2])) {
		level = signed_val(tp[2]);
		if (!(0 <= level && level < 10)) {
                    return 0; /* badarg */
		}
	    } else if (fsizep) {
                if (ERTS_IS_ATOM_STR("fragment", tp[1])) {
                    if (!term_to_Uint(tp[2], &fsize))
                        return 0; /* badarg */
                }
                else {
                    return 0; /* badarg */
                }
            }
            else {
                return 0; /* badarg */
	    }
	} else {
            return 0; /* badarg */
	}
	opts = CDR(list_val(opts));
    }
    if (is_not_nil(opts)) {
        return 0; /* badarg */
    }

    *flagsp = flags;
    *levelp = level;
    if (iovecp)
        *iovecp = iovec;
    if (fsizep)
        *fsizep = fsize;

    return !0; /* ok */
}

BIF_RETTYPE term_to_binary_2(BIF_ALIST_2)
{
    int level;
    Uint flags;
    Eterm res;

    if (!parse_t2b_opts(BIF_ARG_2, &flags, &level, NULL, NULL)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    res = erts_term_to_binary_int(BIF_P, BIF_term_to_binary_2,
                                  BIF_ARG_1, BIF_ARG_2,
                                  level, flags, NULL, 0,
                                  ~((Uint) 0));
    if (is_non_value(res)) {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }
    if (is_tuple(res)) {
	erts_set_gc_state(BIF_P, 0);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
	BIF_RET(res);
    }
}

BIF_RETTYPE term_to_iovec_2(BIF_ALIST_2)
{
    int level;
    Uint flags;
    Eterm res;

    if (!parse_t2b_opts(BIF_ARG_2, &flags, &level, NULL, NULL)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    
    res = erts_term_to_binary_int(BIF_P, BIF_term_to_iovec_2,
                                  BIF_ARG_1, BIF_ARG_2,
                                  level, flags, NULL, !0,
                                  ~((Uint) 0));
    if (is_non_value(res)) {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }
    if (is_tuple(res)) {
	erts_set_gc_state(BIF_P, 0);
	BIF_TRAP1(&term_to_binary_trap_export,BIF_P,res);
    } else {
	ASSERT(!(BIF_P->flags & F_DISABLE_GC));
	BIF_RET(res);
    }
}

Eterm
erts_debug_term_to_binary(Process *p, Eterm term, Eterm opts)
{
    Eterm ret;
    int level, iovec;
    Uint flags;
    Uint fsize;
    
    if (!parse_t2b_opts(opts, &flags, &level, &iovec, &fsize)) {
        ERTS_BIF_PREP_ERROR(ret, p, BADARG);
    }
    else {
        Eterm res = erts_term_to_binary_int(p, BIF_term_to_binary_2,
                                            term, opts, level, flags,
                                            NULL, iovec, fsize);
    
        if (is_non_value(res)) {
            ASSERT(!(p->flags & F_DISABLE_GC));
            ERTS_BIF_PREP_ERROR(ret, p, SYSTEM_LIMIT);
        }
        else if (is_tuple(res)) {
            erts_set_gc_state(p, 0);
            ERTS_BIF_PREP_TRAP1(ret, &term_to_binary_trap_export,p,res);
        }
        else {
            ASSERT(!(p->flags & F_DISABLE_GC));
            ERTS_BIF_PREP_RET(ret, res);
        }
    }
    return ret;
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
    Sint heap_size;
    int terms;
    const byte* ep;
    int atom_extra_skip;
} B2TSizeContext;

typedef struct {
    const byte* ep;
    Eterm  res;
    Eterm* next;
    ErtsHeapFactory factory;
    int remaining_n;
    char* remaining_bytes;
    ErtsWStack flat_maps;
    ErtsPStack hamt_array;
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
    Uint used_bytes; /* In: boolean, Out: bytes */
    Eterm trap_bin;  /* THE_NON_VALUE if not exported */
    Export *bif;
    Eterm arg[2];
    enum B2TState state;
    union {
	B2TSizeContext sc;
	B2TDecodeContext dc;
	B2TUncompressContext uc;
    } u;
} B2TContext;

static B2TContext* b2t_export_context(Process*, B2TContext* src);

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
		    B2TContext** ctxp, Process* p)
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
        if (ctxp)
	    (*ctxp)->state = B2TSizeInit;
    }
    else  {
	uLongf dest_len = get_uint32(bytes+1);
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
            if (ctxp)
                (*ctxp)->reds -= dest_len;
	}
	state->exttmp = 1;
        if (ctxp) {
            /*
             * Start decompression by exporting trap context
             * so we don't have to deal with deep-copying z_stream.
             */
            B2TContext* ctx = b2t_export_context(p, *ctxp);
            ASSERT(state = &(*ctxp)->b2ts);
            state = &ctx->b2ts;

	    if (erl_zlib_inflate_start(&ctx->u.uc.stream, bytes, size) != Z_OK)
		return -1;

	    ctx->u.uc.dbytes = state->extp;
	    ctx->u.uc.dleft = dest_len;
            if (ctx->used_bytes) {
                ASSERT(ctx->used_bytes == 1);
                 /* to be subtracted by stream.avail_in when done */
                ctx->used_bytes = data_size;
            }
	    ctx->state = B2TUncompressChunk;
            *ctxp = ctx;
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
binary2term_create(ErtsDistExternal *edep, ErtsBinary2TermState *state,
		   ErtsHeapFactory* factory)
{
    Eterm res;

    if (!dec_term(edep, factory, state->extp, &res, NULL, 0))
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

    if (binary2term_prepare(state, data, data_size, NULL, NULL) < 0 ||
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
erts_binary2term_create(ErtsBinary2TermState *state, ErtsHeapFactory* factory)
{
    return binary2term_create(NULL,state, factory);
}

static void b2t_destroy_context(B2TContext* context)
{
    erts_free_aligned_binary_bytes_extra(context->aligned_alloc,
                                         ERTS_ALC_T_EXT_TERM_DATA);
    context->aligned_alloc = NULL;
    binary2term_abort(&context->b2ts);
    switch (context->state) {
    case B2TUncompressChunk:
	erl_zlib_inflate_finish(&context->u.uc.stream);
	break;
    case B2TDecode:
    case B2TDecodeList:
    case B2TDecodeTuple:
    case B2TDecodeString:
    case B2TDecodeBinary:
	if (context->u.dc.hamt_array.pstart) {
	    erts_free(context->u.dc.hamt_array.alloc_type,
		      context->u.dc.hamt_array.pstart);
	}
	break;
    default:;
    }
}

static int b2t_context_destructor(Binary *context_bin)
{
    B2TContext* ctx = (B2TContext*) ERTS_MAGIC_BIN_DATA(context_bin);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(context_bin) == b2t_context_destructor);

    b2t_destroy_context(ctx);
    return 1;
}

static BIF_RETTYPE binary_to_term_int(Process*, Eterm bin, B2TContext*);


static BIF_RETTYPE binary_to_term_trap_1(BIF_ALIST_1)
{
    Binary *context_bin = erts_magic_ref2bin(BIF_ARG_1);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(context_bin) == b2t_context_destructor);

    return binary_to_term_int(BIF_P, THE_NON_VALUE, ERTS_MAGIC_BIN_DATA(context_bin));
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

    ASSERT(is_non_value(src->trap_bin));
    sys_memcpy(ctx, src, sizeof(B2TContext));
    if (ctx->state >= B2TDecode && ctx->u.dc.next == &src->u.dc.res) {
        ctx->u.dc.next = &ctx->u.dc.res;
    }
    hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
    ctx->trap_bin = erts_mk_magic_ref(&hp, &MSO(p), context_b);
    return ctx;
}

static BIF_RETTYPE binary_to_term_int(Process* p, Eterm bin, B2TContext *ctx)
{
    BIF_RETTYPE ret_val;
#ifdef EXTREME_B2T_TRAPPING
    SWord initial_reds = 1 + b2t_rand() % 4;
#else
    SWord initial_reds = (Uint)(ERTS_BIF_REDS_LEFT(p) * B2T_BYTES_PER_REDUCTION);
#endif
    int is_first_call;

    if (is_value(bin)) {
	/* Setup enough to get started */
        is_first_call = 1;
	ctx->state = B2TPrepare;
        ctx->aligned_alloc = NULL;
    } else {
        ASSERT(is_value(ctx->trap_bin));
        ASSERT(ctx->state != B2TPrepare);
        is_first_call = 0;
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
            if (binary2term_prepare(&ctx->b2ts, bytes, bin_size, &ctx, p) < 0) {
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
                if (ctx->used_bytes) {
                    ASSERT(ctx->used_bytes > 5 + ctx->u.uc.stream.avail_in);
                    ctx->used_bytes -= ctx->u.uc.stream.avail_in;
                }
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
            if (is_non_value(ctx->trap_bin) && ctx->b2ts.extsize > ctx->reds) {
                /* dec_term will maybe trap, allocate space for magic bin
                   before result term to make it easy to trim with HRelease.
                 */
                ctx = b2t_export_context(p, ctx);
            }
            ctx->u.dc.ep = ctx->b2ts.extp;
            ctx->u.dc.res = (Eterm) (UWord) NULL;
            ctx->u.dc.next = &ctx->u.dc.res;
	    erts_factory_proc_prealloc_init(&ctx->u.dc.factory, p, ctx->heap_size);
	    ctx->u.dc.flat_maps.wstart = NULL;
	    ctx->u.dc.hamt_array.pstart = NULL;
            ctx->state = B2TDecode;
            /*fall through*/
	case B2TDecode:
        case B2TDecodeList:
        case B2TDecodeTuple:
        case B2TDecodeString:
        case B2TDecodeBinary: {
	    ErtsDistExternal fakedep;
            fakedep.flags = ctx->flags;
            fakedep.data = NULL;
            dec_term(&fakedep, NULL, NULL, NULL, ctx, 0);
            break;
	}
        case B2TDecodeFail:
            /*fall through*/
        case B2TBadArg:
            BUMP_REDS(p, (initial_reds - ctx->reds) / B2T_BYTES_PER_REDUCTION);

	    ASSERT(ctx->bif == BIF_TRAP_EXPORT(BIF_binary_to_term_1)
		   || ctx->bif == BIF_TRAP_EXPORT(BIF_binary_to_term_2));

	    if (is_first_call)
		ERTS_BIF_PREP_ERROR(ret_val, p, BADARG);
	    else {
                erts_set_gc_state(p, 1);
		if (is_non_value(ctx->arg[1]))
		    ERTS_BIF_PREP_ERROR_TRAPPED1(ret_val, p, BADARG, ctx->bif,
						 ctx->arg[0]);
		else
		    ERTS_BIF_PREP_ERROR_TRAPPED2(ret_val, p, BADARG, ctx->bif,
						 ctx->arg[0], ctx->arg[1]);
	    }
            b2t_destroy_context(ctx);
	    return ret_val;

        case B2TDone:
            if (ctx->used_bytes) {
                Eterm *hp;
                Eterm used;
                if (!ctx->b2ts.exttmp) {
                    ASSERT(ctx->used_bytes == 1);
                    ctx->used_bytes = (ctx->u.dc.ep - ctx->b2ts.extp
                                       +1); /* VERSION_MAGIC */
                }
                if (IS_USMALL(0, ctx->used_bytes)) {
                    hp = erts_produce_heap(&ctx->u.dc.factory, 3, 0);
                    used = make_small(ctx->used_bytes);
                }
                else {
                    hp = erts_produce_heap(&ctx->u.dc.factory, 3+BIG_UINT_HEAP_SIZE, 0);
                    used = uint_to_big(ctx->used_bytes, hp);
                    hp += BIG_UINT_HEAP_SIZE;
                }
                ctx->u.dc.res = TUPLE2(hp, ctx->u.dc.res, used);
            }
            b2t_destroy_context(ctx);

            if (ctx->u.dc.factory.hp > ctx->u.dc.factory.hp_end) {
                erts_exit(ERTS_ERROR_EXIT, ":%s, line %d: heap overrun by %d words(s)\n",
                         __FILE__, __LINE__, ctx->u.dc.factory.hp - ctx->u.dc.factory.hp_end);
            }
	    erts_factory_close(&ctx->u.dc.factory);

            if (!is_first_call) {
                erts_set_gc_state(p, 1);
            }
            BUMP_REDS(p, (initial_reds - ctx->reds) / B2T_BYTES_PER_REDUCTION);
	    ERTS_BIF_PREP_RET(ret_val, ctx->u.dc.res);
	    return ret_val;

        default:
            ASSERT(!"Unknown state in binary_to_term");
        }
    }while (ctx->reds > 0 || ctx->state >= B2TDone);

    if (is_non_value(ctx->trap_bin)) {
        ctx = b2t_export_context(p, ctx);
        ASSERT(is_value(ctx->trap_bin));
    }

    if (is_first_call) {
        erts_set_gc_state(p, 0);
    }
    BUMP_ALL_REDS(p);

    ERTS_BIF_PREP_TRAP1(ret_val, &binary_to_term_trap_export,
			p, ctx->trap_bin);

    return ret_val;
}

BIF_RETTYPE binary_to_term_1(BIF_ALIST_1)
{
    B2TContext ctx;

    ctx.flags = 0;
    ctx.used_bytes = 0;
    ctx.trap_bin = THE_NON_VALUE;
    ctx.bif = BIF_TRAP_EXPORT(BIF_binary_to_term_1);
    ctx.arg[0] = BIF_ARG_1;
    ctx.arg[1] = THE_NON_VALUE;
    return binary_to_term_int(BIF_P, BIF_ARG_1, &ctx);
}

BIF_RETTYPE binary_to_term_2(BIF_ALIST_2)
{
    B2TContext ctx;
    Eterm opts;
    Eterm opt;

    ctx.flags = 0;
    ctx.used_bytes = 0;
    opts = BIF_ARG_2;
    while (is_list(opts)) {
        opt = CAR(list_val(opts));
        if (opt == am_safe) {
            ctx.flags |= ERTS_DIST_EXT_BTT_SAFE;
        }
        else if (opt == am_used) {
            ctx.used_bytes = 1;
        }
	else {
            goto error;
        }
        opts = CDR(list_val(opts));
    }

    if (is_not_nil(opts))
        goto error;

    ctx.trap_bin = THE_NON_VALUE;
    ctx.bif = BIF_TRAP_EXPORT(BIF_binary_to_term_2);
    ctx.arg[0] = BIF_ARG_1;
    ctx.arg[1] = BIF_ARG_2;
    return binary_to_term_int(BIF_P, BIF_ARG_1, &ctx);

error:
    BIF_ERROR(BIF_P, BADARG);
}

Eterm
external_size_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm Term = BIF_ARG_1;
    Uint size = 0;

    switch (erts_encode_ext_size(Term, &size)) {
    case ERTS_EXT_SZ_SYSTEM_LIMIT:
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    case ERTS_EXT_SZ_YIELD:
        ERTS_INTERNAL_ERROR("Unexpected yield");
    case ERTS_EXT_SZ_OK:
        break;
    }

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
    Uint size = 0;
    Uint flags = TERM_TO_BINARY_DFLAGS;

    while (is_list(BIF_ARG_2)) {
        Eterm arg = CAR(list_val(BIF_ARG_2));
        Eterm* tp;

        if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
            if (tp[1] == am_minor_version && is_small(tp[2])) {
                switch (signed_val(tp[2])) {
                case 0:
                    flags &= ~DFLAG_NEW_FLOATS;
                    break;
                case 1:
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

    switch (erts_encode_ext_size_2(BIF_ARG_1, flags, &size)) {
    case ERTS_EXT_SZ_SYSTEM_LIMIT:
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    case ERTS_EXT_SZ_YIELD:
        ERTS_INTERNAL_ERROR("Unexpected yield");
    case ERTS_EXT_SZ_OK:
        break;
    }

    if (IS_USMALL(0, size)) {
        BIF_RET(make_small(size));
    } else {
        Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
        BIF_RET(uint_to_big(size, hp));
    }
}

static Eterm
erts_term_to_binary_simple(Process* p, Eterm Term, Uint size, int level, Uint64 dflags)
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

	if ((endp = enc_term(NULL, Term, bytes, dflags, NULL))
	    == NULL) {
	    erts_exit(ERTS_ERROR_EXIT, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erts_exit(ERTS_ERROR_EXIT, "%s, line %d: buffer overflow: %d word(s)\n",
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
	if ((endp = enc_term(NULL, Term, bytes+1, dflags, NULL))
	    == NULL) {
	    erts_exit(ERTS_ERROR_EXIT, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erts_exit(ERTS_ERROR_EXIT, "%s, line %d: buffer overflow: %d word(s)\n",
		     __FILE__, __LINE__, endp - (bytes + size));
	}
	return erts_realloc_binary(bin, real_size);
    }
}

Eterm
erts_term_to_binary(Process* p, Eterm Term, int level, Uint64 flags) {
    Uint size = 0;
    switch (encode_size_struct_int(NULL, NULL, Term, flags, NULL, &size)) {
    case ERTS_EXT_SZ_SYSTEM_LIMIT:
        return THE_NON_VALUE;
    case ERTS_EXT_SZ_YIELD:
        ERTS_INTERNAL_ERROR("Unexpected yield");
    case ERTS_EXT_SZ_OK:
        break;
    }
    size++; /* VERSION_MAGIC */;
    return erts_term_to_binary_simple(p, Term, size, level, flags);
}

/* Define EXTREME_TTB_TRAPPING for testing in dist.h */

#ifndef EXTREME_TTB_TRAPPING
#define TERM_TO_BINARY_COMPRESS_CHUNK (1 << 18)
#else
#define TERM_TO_BINARY_COMPRESS_CHUNK 10
#endif
#define TERM_TO_BINARY_MEMCPY_FACTOR 8

static int ttb_context_destructor(Binary *context_bin)
{
    TTBContext *context = ERTS_MAGIC_BIN_DATA(context_bin);
    if (context->alive) {
	context->alive = 0;
	switch (context->state) {
	case TTBSize:
	    DESTROY_SAVED_WSTACK(&context->s.sc.wstack);
	    break;
	case TTBEncode:
	    DESTROY_SAVED_WSTACK(&context->s.ec.wstack);
	    if (context->s.ec.result_bin != NULL) { /* Set to NULL if ever made alive! */
		ASSERT(erts_refc_read(&(context->s.ec.result_bin->intern.refc),1));
		erts_bin_free(context->s.ec.result_bin);
		context->s.ec.result_bin = NULL;
	    }
            if (context->s.ec.iov)
                erts_free(ERTS_ALC_T_T2B_VEC, context->s.ec.iov);
	    break;
	case TTBCompress:
	    erl_zlib_deflate_finish(&(context->s.cc.stream));

	    if (context->s.cc.destination_bin != NULL) { /* Set to NULL if ever made alive! */
		ASSERT(erts_refc_read(&(context->s.cc.destination_bin->intern.refc),1));
		erts_bin_free(context->s.cc.destination_bin);
		context->s.cc.destination_bin = NULL;
	    }
	    
	    if (context->s.cc.result_bin != NULL) { /* Set to NULL if ever made alive! */
		ASSERT(erts_refc_read(&(context->s.cc.result_bin->intern.refc),1));
		erts_bin_free(context->s.cc.result_bin);
		context->s.cc.result_bin = NULL;
	    }
	    break;
	}
    }
    return 1;
}

Uint
erts_ttb_iov_size(int use_termv, Sint vlen, Uint fragments)
{
    Uint sz;
    ASSERT(vlen > 0);
    ASSERT(fragments > 0);
    sz = sizeof(SysIOVec)*vlen;
    sz += sizeof(ErlDrvBinary *)*vlen;
    if (use_termv)
        sz += sizeof(Eterm)*vlen;
    sz += sizeof(ErlIOVec *)*fragments;
    sz += sizeof(ErlIOVec)*fragments;
    ASSERT(sz % sizeof(void*) == 0);
    return sz;
}

void
erts_ttb_iov_init(TTBEncodeContext *ctx, int use_termv, char *ptr,
                  Sint vlen, Uint fragments, Uint fragment_size)
{
    ctx->vlen = 0;
    ctx->size = 0;
    
    ctx->iov = (SysIOVec *) ptr;
    ptr += sizeof(SysIOVec)*vlen;
    ASSERT(((UWord) ptr) % sizeof(void *) == 0);
    
    ctx->binv = (ErlDrvBinary **) ptr;
    ptr += sizeof(ErlDrvBinary *)*vlen;

    if (!use_termv)
        ctx->termv = NULL;
    else {
        ctx->termv = (Eterm *) ptr;
        ptr += sizeof(Eterm)*vlen;
    }
    
    ctx->fragment_eiovs = (ErlIOVec *) ptr;
    ptr += sizeof(ErlIOVec)*fragments;
    ASSERT(((UWord) ptr) % sizeof(void *) == 0);
    
    ctx->frag_ix = -1;
    ctx->fragment_size = fragment_size;

#ifdef DEBUG
    ctx->cptr = NULL;
    ctx->debug_fragments = fragments;
    ctx->debug_vlen = vlen;
#endif
}

static Eterm erts_term_to_binary_int(Process* p, Sint bif_ix, Eterm Term, Eterm opts,
                                     int level, Uint64 dflags, Binary *context_b,
                                     int iovec, Uint fragment_size)
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

    ASSERT(bif_ix > 0 && IS_USMALL(!0, bif_ix));
    ASSERT(bif_ix == BIF_term_to_binary_1 || bif_ix == BIF_term_to_binary_2
           || bif_ix == BIF_term_to_iovec_1 || bif_ix == BIF_term_to_iovec_2);
    
#define EXPORT_CONTEXT()						\
    do {								\
	if (context_b == NULL) {					\
	    context_b = erts_create_magic_binary(sizeof(TTBContext),    \
                                                 ttb_context_destructor);\
	    context =  ERTS_MAGIC_BIN_DATA(context_b);			\
	    sys_memcpy(context,&c_buff,sizeof(TTBContext));		\
	}								\
    } while (0)

#define RETURN_STATE()							\
    do {								\
	hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + 1 + 4);              \
	c_term = erts_mk_magic_ref(&hp, &MSO(p), context_b);            \
	res = TUPLE4(hp, Term, opts, c_term, make_small(bif_ix));       \
	BUMP_ALL_REDS(p);                                               \
	return res;							\
    } while (0);

    if (context_b == NULL) {
	/* Setup enough to get started */
	context->state = TTBSize;
	context->alive = 1;
        ERTS_INIT_TTBSizeContext(&context->s.sc, dflags);
	context->s.sc.level = level;
        context->s.sc.fragment_size = fragment_size;
        if (!level) {
            context->s.sc.vlen = iovec ? 0 : -1;
            context->s.sc.iovec = iovec;
        }
        else {
            context->s.sc.vlen = -1;
            context->s.sc.iovec = 0;
        }
    } else {
	context = ERTS_MAGIC_BIN_DATA(context_b);
    }

    /* Initialization done, now we will go through the states */
    for (;;) {
	switch (context->state) {
	case TTBSize:
	    {
		Uint size, fragments = 1;
		Binary *result_bin;
		int level = context->s.sc.level;
                Sint vlen;
                iovec = context->s.sc.iovec;
                fragment_size = context->s.sc.fragment_size;
		size = 1; /* VERSION_MAGIC */
                switch (encode_size_struct_int(&context->s.sc, NULL, Term,
                                               context->s.sc.dflags, &reds,
                                               &size)) {
                case ERTS_EXT_SZ_SYSTEM_LIMIT:
                    BUMP_REDS(p, (initial_reds - reds) / TERM_TO_BINARY_LOOP_FACTOR);
                    return THE_NON_VALUE;
                case ERTS_EXT_SZ_YIELD:
		    EXPORT_CONTEXT();
		    /* Same state */
		    RETURN_STATE();
                case ERTS_EXT_SZ_OK:
                    break;
		}
		/* Move these to next state */
		dflags = context->s.sc.dflags;
                vlen = context->s.sc.vlen;
		if (vlen >= 0) {
                    Uint total_size = size + context->s.sc.extra_size;
                    fragments = (total_size - 1)/fragment_size + 1;
                    vlen += 3*fragments;
                    ASSERT(vlen);
                }
                else if (size <= ERL_ONHEAP_BIN_LIMIT) {
		    /* Finish in one go */
		    res = erts_term_to_binary_simple(p, Term, size, 
						     level, dflags);
                    if (iovec) {
                        Eterm *hp = HAlloc(p, 2);
                        res = CONS(hp, res, NIL);
                    }
		    BUMP_REDS(p, 1);
		    return res;
		}

		result_bin = erts_bin_nrml_alloc(size);
		result_bin->orig_bytes[0] = (byte)VERSION_MAGIC;
		/* Next state immediately, no need to export context */
		context->state = TTBEncode;
                ERTS_INIT_TTBEncodeContext(&context->s.ec, dflags);
		context->s.ec.level = level;
		context->s.ec.result_bin = result_bin;
                context->s.ec.iovec = iovec;
                if (vlen >= 0) {
                    Uint sz = erts_ttb_iov_size(!0, vlen, fragments);
                    char *ptr = (char *) erts_alloc(ERTS_ALC_T_T2B_VEC, sz);
                    erts_ttb_iov_init(&context->s.ec, !0, ptr, vlen,
                                      fragments, fragment_size);
                    context->s.ec.cptr = (byte *) &result_bin->orig_bytes[0];
                }
		break;
	    }
	case TTBEncode:
	    {
		byte *endp, *tmp;
		byte *bytes = (byte *) context->s.ec.result_bin->orig_bytes;
		size_t real_size;
		Binary *result_bin;
                Sint realloc_offset;
                Uint fragments;

		dflags = context->s.ec.dflags;
		if (enc_term_int(&context->s.ec, NULL,Term, bytes+1, dflags,
                                 NULL, &reds, &endp) < 0) {
		    EXPORT_CONTEXT();
		    RETURN_STATE();
		}
		real_size = endp - bytes;
                tmp = (byte *) &context->s.ec.result_bin->orig_bytes[0];
		result_bin = erts_bin_realloc(context->s.ec.result_bin,real_size);
                realloc_offset = (byte *) &result_bin->orig_bytes[0] - tmp;
		level = context->s.ec.level;
		BUMP_REDS(p, (initial_reds - reds) / TERM_TO_BINARY_LOOP_FACTOR);
		if (level == 0 || real_size < 6) { /* We are done */
                    Sint cbin_refc_diff;
                    Eterm result, rb_term, *hp, *hp_end;
                    Uint hsz;
                    int ix;
                    SysIOVec *iov;
                    Eterm *termv;
		return_normal:
                    fragments = context->s.ec.frag_ix + 1;
		    context->s.ec.result_bin = NULL;
		    context->alive = 0;
		    if (context_b && erts_refc_read(&context_b->intern.refc,0) == 0) {
			erts_bin_free(context_b);
		    }
                    if (!context->s.ec.iov) {
                        hsz = PROC_BIN_SIZE + (iovec ? 2 : 0);
                        hp = HAlloc(p, hsz);
                        result = erts_build_proc_bin(&MSO(p), hp, result_bin);
                        if (iovec) {
                            hp += PROC_BIN_SIZE;
                            result = CONS(hp, result, NIL);
                        }
                        return result;
                    }
                    iovec = context->s.ec.iovec;
                    ASSERT(iovec);
                    iov = context->s.ec.iov;
                    termv = context->s.ec.termv;
                    ASSERT(context->s.ec.vlen <= context->s.ec.debug_vlen);
                    ASSERT(fragments <= context->s.ec.debug_fragments);                    
                    /* first two elements should be unused */
                    ASSERT(context->s.ec.vlen >= 3*fragments);
                    ASSERT(!iov[0].iov_base && !iov[0].iov_len);
                    ASSERT(!iov[1].iov_base && !iov[1].iov_len);

                    hsz = (2 /* cons */
                           + (PROC_BIN_SIZE > ERL_SUB_BIN_SIZE
                              ? PROC_BIN_SIZE
                              : ERL_SUB_BIN_SIZE)); /* max size per vec */
                    hsz *= context->s.ec.vlen - 2*fragments; /* number of vecs */
                    hp = HAlloc(p, hsz);
                    hp_end = hp + hsz;
                    rb_term = THE_NON_VALUE;
                    result = NIL;
                    ASSERT(erts_refc_read(&result_bin->intern.refc, 1) == 1);
                    cbin_refc_diff = -1;
                    for (ix = context->s.ec.vlen - 1; ix > 1; ix--) {
                        Eterm bin_term, pb_term;
                        Uint pb_size;
                        ProcBin *pb;
                        SysIOVec *iovp = &iov[ix];
                        if (!iovp->iov_base)
                            continue; /* empty slot for header */
                        pb_term = termv[ix];
                        if (is_value(pb_term)) {
                            pb_size = binary_size(pb_term);
                            pb = (ProcBin *) binary_val(pb_term);
                        }
                        else {
                            iovp->iov_base = (void *) (((byte *) iovp->iov_base)
                                                       + realloc_offset);
                            pb_size = result_bin->orig_size;
                            if (is_non_value(rb_term))
                                pb = NULL;
                            else {
                                pb = (ProcBin *) binary_val(rb_term);
                                pb_term = rb_term;
                            }
                        }
                        /*
                         * We intentionally avoid using sub binaries
                         * since the GC might convert those to heap
                         * binaries and by this ruin the nice preparation
                         * for usage of this data as I/O vector in
                         * nifs/drivers.
                         */
                        if (is_value(pb_term) && iovp->iov_len == pb_size)
                            bin_term = pb_term;
                        else {
                            Binary *bin;
                            if (is_value(pb_term)) {
                                bin = ((ProcBin *) binary_val(pb_term))->val;
                                erts_refc_inc(&bin->intern.refc, 2);
                            }
                            else {
                                bin = result_bin;
                                cbin_refc_diff++;
                            }
                            pb = (ProcBin *) (char *) hp;
                            hp += PROC_BIN_SIZE;
                            pb->thing_word = HEADER_PROC_BIN;
                            pb->size = (Uint) iovp->iov_len;
                            pb->next = MSO(p).first;
                            MSO(p).first = (struct erl_off_heap_header*) pb;
                            pb->val = bin;
                            pb->bytes = (byte*) iovp->iov_base;
                            pb->flags = 0;
                            OH_OVERHEAD(&MSO(p), pb->size / sizeof(Eterm));
                            bin_term = make_binary(pb);
                        }
                        result = CONS(hp, bin_term, result);
                        hp += 2;
                    }
                    ASSERT(hp <= hp_end);
                    HRelease(p, hp_end, hp);
                    context->s.ec.iov = NULL;
                    erts_free(ERTS_ALC_T_T2B_VEC, iov);
                    if (cbin_refc_diff) {
                        ASSERT(cbin_refc_diff >= -1);
                        if (cbin_refc_diff > 0)
                            erts_refc_add(&result_bin->intern.refc,
                                          cbin_refc_diff, 1);
                        else
                            erts_bin_free(result_bin);
                    }
                    return result;
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
		result_bin->orig_bytes[0] = (byte) VERSION_MAGIC;

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
			ASSERT(erts_refc_read(&result_bin->intern.refc, 1));
			erts_bin_free(context->s.cc.result_bin);
			context->s.cc.result_bin = NULL;
			context->alive = 0;
			BUMP_REDS(p, (this_time * CONTEXT_REDS) / TERM_TO_BINARY_COMPRESS_CHUNK);
			if (context_b && erts_refc_read(&context_b->intern.refc,0) == 0) {
			    erts_bin_free(context_b);
			}
			return erts_build_proc_bin(&MSO(p),
						   HAlloc(p, PROC_BIN_SIZE),
                                                   result_bin);
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
		    ASSERT(erts_refc_read(&result_bin->intern.refc, 1));
		    erl_zlib_deflate_finish(&(context->s.cc.stream));
		    erts_bin_free(context->s.cc.destination_bin);
		    context->s.cc.destination_bin = NULL;
		    context->alive = 0;
		    BUMP_REDS(p, (this_time * CONTEXT_REDS) / TERM_TO_BINARY_COMPRESS_CHUNK);
		    if (context_b && erts_refc_read(&context_b->intern.refc,0) == 0) {
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
enc_atom(ErtsAtomCacheMap *acmp, Eterm atom, byte *ep, Uint64 dflags)
{
    int iix;
    int len;
    const int utf8_atoms = (int) (dflags & DFLAG_UTF8_ATOMS);

    ASSERT(is_atom(atom));

    if (dflags & DFLAG_ETS_COMPRESSED) {
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

/*
 * We use this atom as sysname in local pid/port/refs
 * for the ETS compressed format
 *
 */
#define INTERNAL_LOCAL_SYSNAME am_ErtsSecretAtom

static byte*
enc_pid(ErtsAtomCacheMap *acmp, Eterm pid, byte* ep, Uint64 dflags)
{
    Uint on, os;
    Eterm sysname = ((is_internal_pid(pid) && (dflags & DFLAG_ETS_COMPRESSED))
		      ? INTERNAL_LOCAL_SYSNAME : pid_node_name(pid));
    Uint32 creation = pid_creation(pid);

    *ep++ = NEW_PID_EXT;

    ep = enc_atom(acmp, sysname, ep, dflags);

    if (is_internal_pid(pid)) {
        on = internal_pid_number(pid);
        os = internal_pid_serial(pid);
    }
    else {
        on = external_pid_number(pid);
        os = external_pid_serial(pid);
    }

    put_int32(on, ep);
    ep += 4;
    put_int32(os, ep);
    ep += 4;
    put_int32(creation, ep);
    ep += 4;
    return ep;
}

/* Expect an atom in plain text or cached */
static const byte*
dec_atom(ErtsDistExternal *edep, const byte* ep, Eterm* objp)
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

static ERTS_INLINE int dec_is_this_node(Eterm sysname, Uint32 creation)
{
    return (sysname == INTERNAL_LOCAL_SYSNAME
            ||
            (sysname == erts_this_node->sysname
             && (creation == erts_this_node->creation
                 || creation == ORIG_CREATION)));
}


static ERTS_INLINE ErlNode* dec_get_node(Eterm sysname, Uint32 creation, Eterm book)
{
    if (dec_is_this_node(sysname, creation))
	return erts_this_node;
    else
        return erts_find_or_insert_node(sysname,creation,book);
}

static const byte*
dec_pid(ErtsDistExternal *edep, ErtsHeapFactory* factory, const byte* ep,
        Eterm* objp, byte tag)
{
    Eterm sysname;
    Uint data;
    Uint num;
    Uint ser;
    Uint32 cre;

    *objp = NIL;		/* In case we fail, don't leave a hole in the heap */

    /* eat first atom */
    if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
	return NULL;
    num = get_uint32(ep);
    ep += 4;
    ser = get_uint32(ep);
    ep += 4;
    if (tag == PID_EXT) {
        cre = get_int8(ep);
        ep += 1;
        if (!is_tiny_creation(cre)) {
            return NULL;
        }
    } else {
        ASSERT(tag == NEW_PID_EXT);
        cre = get_uint32(ep);
        ep += 4;
    }

    /*
     * We are careful to create the node entry only after all
     * validity tests are done.
     */
    if (dec_is_this_node(sysname, cre)) {
        if (num > ERTS_MAX_INTERNAL_PID_NUMBER ||
            ser > ERTS_MAX_INTERNAL_PID_SERIAL) {
            return NULL;
        }

        data = make_pid_data(ser, num);
	*objp = make_internal_pid(data);
    } else {
	ExternalThing *etp = (ExternalThing *) factory->hp;
        factory->hp += EXTERNAL_PID_HEAP_SIZE;

	etp->header = make_external_pid_header();
	etp->next = factory->off_heap->first;
        etp->node = erts_find_or_insert_node(sysname, cre, make_boxed(&etp->header));
        etp->data.pid.num = num;
        etp->data.pid.ser = ser;

	factory->off_heap->first = (struct erl_off_heap_header*) etp;
	*objp = make_external_pid(etp);
    }
    return ep;
}


#define ENC_TERM ((Eterm) 0)
#define ENC_ONE_CONS ((Eterm) 1)
#define ENC_PATCH_FUN_SIZE ((Eterm) 2)
#define ENC_BIN_COPY ((Eterm) 3)
#define ENC_MAP_PAIR ((Eterm) 4)
#define ENC_HASHMAP_NODE ((Eterm) 5)
#define ENC_LAST_ARRAY_ELEMENT ((Eterm) 6)

static byte*
enc_term(ErtsAtomCacheMap *acmp, Eterm obj, byte* ep, Uint64 dflags,
	 struct erl_off_heap_header** off_heap)
{
    byte *res;
    (void) enc_term_int(NULL, acmp, obj, ep, dflags, off_heap, NULL, &res);
    return res;
}

static int
enc_term_int(TTBEncodeContext* ctx, ErtsAtomCacheMap *acmp, Eterm obj, byte* ep,
             Uint64 dflags,
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
    int use_iov = 0;

    if (ctx) {
	WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	r = *reds;
        use_iov = !!ctx->iov;

	if (ctx->wstack.wstart) { /* restore saved stacks and byte pointer */
	    WSTACK_RESTORE(s, &ctx->wstack);
	    ep = ctx->ep;
	    obj = ctx->obj;
	    if (is_non_value(obj)) {
		goto outer_loop;
	    }
	}
    }

    goto L_jump_start;

 outer_loop:
    while (!WSTACK_ISEMPTY(s)) {
	obj = WSTACK_POP(s);

	switch (val = WSTACK_POP(s)) {
	case ENC_TERM:
	    break;
	case ENC_ONE_CONS:
	encode_one_cons:
	    {
		Eterm* cons = list_val(obj);
		Eterm tl;
                Uint len_cnt = WSTACK_POP(s);

		obj = CAR(cons);
		tl = CDR(cons);
                if (is_list(tl)) {
                    len_cnt++;
                    WSTACK_PUSH3(s, len_cnt, ENC_ONE_CONS, tl);
                }
                else {
                    byte* list_lenp = (byte*) WSTACK_POP(s);
		    ASSERT(list_lenp[-1] == LIST_EXT);
                    put_int32(len_cnt, list_lenp);

                    WSTACK_PUSH2(s, ENC_TERM, tl);
                }
	    }
	    break;
	case ENC_PATCH_FUN_SIZE:
	    {
		byte* size_p = (byte *) obj;
                Sint32 sz = ep - size_p;
		put_int32(sz, size_p);
	    }
	    goto outer_loop;
	case ENC_BIN_COPY: {
	    Uint bits = (Uint)obj;
	    Uint bitoffs = WSTACK_POP(s);
	    byte* bytes = (byte*) WSTACK_POP(s);
	    byte* dst = (byte*) WSTACK_POP(s);
	    if (bits > r * (TERM_TO_BINARY_MEMCPY_FACTOR * 8)) {
		Uint n = r * TERM_TO_BINARY_MEMCPY_FACTOR;
		WSTACK_PUSH5(s, (UWord)(dst + n), (UWord)(bytes + n), bitoffs,
			     ENC_BIN_COPY, bits - 8*n);
		bits = 8*n;
		copy_binary_to_buffer(dst, 0, bytes, bitoffs, bits);
		obj = THE_NON_VALUE;
		r = 0; /* yield */
		break;
	    } else {
		copy_binary_to_buffer(dst, 0, bytes, bitoffs, bits);
		r -= bits / (TERM_TO_BINARY_MEMCPY_FACTOR * 8);
		goto outer_loop;
	    }
	}
	case ENC_MAP_PAIR: {
	    Uint pairs_left = obj;
	    Eterm *vptr = (Eterm*) WSTACK_POP(s);
	    Eterm *kptr = (Eterm*) WSTACK_POP(s);

	    obj = *kptr;
	    if (--pairs_left > 0) {
		WSTACK_PUSH4(s, (UWord)(kptr+1), (UWord)(vptr+1),
			     ENC_MAP_PAIR, pairs_left);
	    }
	    WSTACK_PUSH2(s, ENC_TERM, *vptr);
	    break;
	}
	case ENC_HASHMAP_NODE:
	    if (is_list(obj)) { /* leaf node [K|V] */
		ptr = list_val(obj);
		WSTACK_PUSH2(s, ENC_TERM, CDR(ptr));
		obj = CAR(ptr);
	    }
	    break;
	case ENC_LAST_ARRAY_ELEMENT:
	    /* obj is the tuple */
	    {
		Eterm* ptr = (Eterm *) obj;
		obj = *ptr;
	    }
	    break;
	default:		/* ENC_LAST_ARRAY_ELEMENT+1 and upwards */
	    {
		Eterm* ptr = (Eterm *) obj;
		obj = *ptr++;
		WSTACK_PUSH2(s, val-1, (UWord)ptr);
	    }
	    break;
	}

	if (ctx && --r <= 0) {
	    *reds = 0;
	    ctx->obj = obj;
	    ctx->ep = ep;
	    WSTACK_SAVE(s, &ctx->wstack);
	    return -1;
	}

    L_jump_start:
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
	    Eterm sysname = (((dflags & DFLAG_ETS_COMPRESSED) && is_internal_ref(obj))
			     ? INTERNAL_LOCAL_SYSNAME : ref_node_name(obj));
            Uint32 creation = ref_creation(obj);

	    erts_magic_ref_save_bin(obj);

            *ep++ = NEWER_REFERENCE_EXT;
	    i = ref_no_numbers(obj);
	    put_int16(i, ep);
	    ep += 2;
	    ep = enc_atom(acmp, sysname, ep, dflags);
            put_int32(creation, ep);
            ep += 4;
	    ref_num = ref_numbers(obj);
	    for (j = 0; j < i; j++) {
		put_int32(ref_num[j], ep);
		ep += 4;
	    }
	    break;
	}
	case PORT_DEF:
	case EXTERNAL_PORT_DEF: {
	    Eterm sysname = (((dflags & DFLAG_ETS_COMPRESSED) && is_internal_port(obj))
			     ? INTERNAL_LOCAL_SYSNAME : port_node_name(obj));
            Uint32 creation = port_creation(obj);
	    byte *tagp = ep++;
	    Uint64 num;

	    ep = enc_atom(acmp, sysname, ep, dflags);
	    num = port_number(obj);
	    if (num > ERTS_MAX_V3_PORT_NUMBER) {
		*tagp = V4_PORT_EXT;
		put_int64(num, ep);
		ep += 8;
	    }
	    else {
		*tagp = NEW_PORT_EXT;
		put_int32(num, ep);
		ep += 4;
	    }
            put_int32(creation, ep);
            ep += 4;
	    break;
	}
	case LIST_DEF:
	    {
		if (is_external_string(obj, &i)) {
		    *ep++ = STRING_EXT;
		    put_int16(i, ep);
		    ep += 2;
		    while (is_list(obj)) {
			Eterm* cons = list_val(obj);
			*ep++ = unsigned_val(CAR(cons));
			obj = CDR(cons);
		    }
		    r -= i;
		} else {
		    r -= i/2;
		    *ep++ = LIST_EXT;
                    /* Patch list length when we find end of list */
                    WSTACK_PUSH2(s, (UWord)ep, 1);
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
		WSTACK_PUSH2(s, ENC_LAST_ARRAY_ELEMENT+i-1, (UWord)ptr);
	    }
	    break;

	case MAP_DEF:
	    if (is_flatmap(obj)) {
		flatmap_t *mp = (flatmap_t*)flatmap_val(obj);
		Uint size = flatmap_get_size(mp);

		*ep++ = MAP_EXT;
		put_int32(size, ep); ep += 4;

		if (size > 0) {
		    Eterm *kptr = flatmap_get_keys(mp);
		    Eterm *vptr = flatmap_get_values(mp);

		    WSTACK_PUSH4(s, (UWord)kptr, (UWord)vptr,
				 ENC_MAP_PAIR, size);
		}
	    } else {
		Eterm hdr;
		Uint node_sz;
		ptr = boxed_val(obj);
		hdr = *ptr;
		ASSERT(is_header(hdr));
		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		case HAMT_SUBTAG_HEAD_ARRAY:
		    *ep++ = MAP_EXT;
		    ptr++;
		    put_int32(*ptr, ep); ep += 4;
		    node_sz = 16;
		    break;
		case HAMT_SUBTAG_HEAD_BITMAP:
		    *ep++ = MAP_EXT;
		    ptr++;
		    put_int32(*ptr, ep); ep += 4;
		    /*fall through*/
		case HAMT_SUBTAG_NODE_BITMAP:
		    node_sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
		    ASSERT(node_sz < 17);
		    break;
		default:
		    erts_exit(ERTS_ERROR_EXIT, "bad header\r\n");
		}

		ptr++;
		WSTACK_RESERVE(s, node_sz*2);
		while(node_sz--) {
		    WSTACK_FAST_PUSH(s, ENC_HASHMAP_NODE);
		    WSTACK_FAST_PUSH(s, *ptr++);
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

		/* Don't leave garbage after the float */
		sys_memset(ep+i, 0, 31-i);
		ep += 31;
	    }
	    break;

	case BINARY_DEF:
	    {
		Uint bitoffs;
		Uint bitsize;
		byte* bytes;
		byte* data_dst;
                Uint off_heap_bytesize = 0;
                Uint off_heap_tail;
                Eterm pb_term;
                Binary *pb_val;

                ASSERT(!(dflags & DFLAG_PENDING_CONNECT) || (ctx && ctx->iov));
    
		ERTS_GET_BINARY_BYTES(obj, bytes, bitoffs, bitsize);
                if (use_iov) {
                    if (bitoffs == 0) {
                        ProcBin* pb = (ProcBin*) binary_val(obj);
                        off_heap_bytesize = pb->size;
                        if (off_heap_bytesize <= ERL_ONHEAP_BIN_LIMIT)
                            off_heap_bytesize = 0;
                        else {
                            pb_term = obj;
                            if (pb->thing_word == HEADER_SUB_BIN) {
                                ErlSubBin* sub = (ErlSubBin*)pb;
                                pb_term = sub->orig;
                                pb = (ProcBin*) binary_val(pb_term);
                            }
                            if (pb->thing_word != HEADER_PROC_BIN)
                                off_heap_bytesize = 0;
                            else {
                                if (pb->flags) {
                                    char* before_realloc = pb->val->orig_bytes; 
                                    erts_emasculate_writable_binary(pb);
                                    bytes += (pb->val->orig_bytes - before_realloc);
                                    ASSERT((byte *) &pb->val->orig_bytes[0] <= bytes
                                           && bytes < ((byte *) &pb->val->orig_bytes[0]
                                                       + pb->val->orig_size));
                                }
                                pb_val = pb->val;
                            }
                        }
                    }
                }
		else if (dflags & DFLAG_ETS_COMPRESSED) {
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
			erts_refc_inc(&pb->val->intern.refc, 2);

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
                    if (off_heap_bytesize)
                        off_heap_tail = 0;
                    else {
                        data_dst = ep;
                        ep += j;
                    }
		} else {
		    /* Bit-level binary. */
                    *ep++ = BIT_BINARY_EXT;
                    j = binary_size(obj);
                    put_int32((j+1), ep);
                    ep += 4;
                    *ep++ = bitsize;
                    if (off_heap_bytesize) {
                        /* trailing bits */
                        ep[0] = 0;
                        copy_binary_to_buffer(ep, 0, bytes + j, 0, bitsize);
                        off_heap_tail = 1;
                    }
                    else {
                        ep[j] = 0;	/* Zero unused bits at end of binary */
                        data_dst = ep;
                        ep += j + 1;
                    }
		}
                if (off_heap_bytesize) {
                    ASSERT(pb_val);
                    store_in_vec(ctx, ep, pb_val, pb_term,
                                 bytes, off_heap_bytesize);
                    ep += off_heap_tail;
                }
                else if (ctx && j > r * TERM_TO_BINARY_MEMCPY_FACTOR) {
		    WSTACK_PUSH5(s, (UWord)data_dst, (UWord)bytes, bitoffs,
				 ENC_BIN_COPY, 8*j + bitsize);
		} else {
		    copy_binary_to_buffer(data_dst, 0, bytes, bitoffs,
					  8 * j + bitsize);
		}
	    }
	    break;
	case EXPORT_DEF:
	    {
		Export* exp = *((Export **) (export_val(obj) + 1));
                *ep++ = EXPORT_EXT;
                ep = enc_atom(acmp, exp->info.mfa.module, ep, dflags);
                ep = enc_atom(acmp, exp->info.mfa.function, ep, dflags);
                ep = enc_term(acmp, make_small(exp->info.mfa.arity),
                              ep, dflags, off_heap);
		break;
	    }
	    break;
	case FUN_DEF:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
		int ei;

                *ep++ = NEW_FUN_EXT;
                WSTACK_PUSH2(s, ENC_PATCH_FUN_SIZE,
                             (UWord) ep); /* Position for patching in size */
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

		for (ei = funp->num_free-1; ei >= 0; ei--) {
		    WSTACK_PUSH2(s, ENC_TERM, (UWord) funp->env[ei]);
		}
	    }
	    break;
	}
    }
    DESTROY_WSTACK(s);
    if (ctx) {
	ASSERT(ctx->wstack.wstart == NULL);
	*reds = r;
        if (use_iov)
            store_in_vec(ctx, ep, NULL, THE_NON_VALUE, NULL, 0);
    }
    *res = ep;
    return 0;
}

static ERTS_INLINE void
store_in_vec_aux(TTBEncodeContext *ctx,
                 Binary *bin,
                 Eterm term,
                 byte *ptr,
                 Uint len)
{
    ErlDrvBinary *dbin = Binary2ErlDrvBinary(bin);
    int vlen = ctx->vlen;
    Uint iov_len;
    ErlIOVec *feiovp;

    ASSERT(((byte *) &bin->orig_bytes[0]) <= ptr);
    ASSERT(ptr + len <= ((byte *) &bin->orig_bytes[0]) + bin->orig_size);

    if (ctx->frag_ix >= 0) {
        feiovp = &ctx->fragment_eiovs[ctx->frag_ix];
        ASSERT(0 < feiovp->size);
        ASSERT(feiovp->size <= ctx->fragment_size);
        if (feiovp->size != ctx->fragment_size) {
            /* current fragment not full yet... */
            iov_len = ctx->fragment_size - feiovp->size;
            if (len < iov_len)
                iov_len = len;
            goto store_iov_data;
        }
    }

    while (len) {
        /* Start new fragment... */
        ctx->frag_ix++;
        feiovp = &ctx->fragment_eiovs[ctx->frag_ix];
        ASSERT(ctx->frag_ix >= 0);

        if (ctx->termv) {
            ctx->termv[vlen] = THE_NON_VALUE;
            ctx->termv[vlen+1] = THE_NON_VALUE;
        }

        feiovp->vsize = 2;
        feiovp->size = 0;
        feiovp->iov = &ctx->iov[vlen];
        feiovp->binv = &ctx->binv[vlen];

        /* entry for driver header */
        ctx->iov[vlen].iov_base = NULL;
        ctx->iov[vlen].iov_len = 0;
        ctx->binv[vlen] = NULL;
        vlen++;

        /* entry for dist header */
        ctx->iov[vlen].iov_base = NULL;
        ctx->iov[vlen].iov_len = 0;
        ctx->binv[vlen] = NULL;
        vlen++;

        iov_len = len < ctx->fragment_size ? len : ctx->fragment_size;

    store_iov_data:

        ASSERT(iov_len);
        
        do {
            Uint iov_len_left;
                
            if (iov_len <= MAX_SYSIOVEC_IOVLEN)
                iov_len_left = 0;
            else {
                iov_len_left = iov_len - MAX_SYSIOVEC_IOVLEN;
                iov_len = MAX_SYSIOVEC_IOVLEN;
            }

            ctx->iov[vlen].iov_base = ptr;
            ctx->iov[vlen].iov_len = iov_len;
            ctx->binv[vlen] = dbin;
            if (ctx->termv)
                ctx->termv[vlen] = term;
            else
                erts_refc_inc(&bin->intern.refc, 2);
            ctx->size += iov_len;
            len -= iov_len;
            ptr += iov_len;
            vlen++;
            feiovp->size += iov_len;
            feiovp->vsize++;

            iov_len = iov_len_left;
        } while (iov_len);
    }

    ctx->vlen = vlen;
}

static void
store_in_vec(TTBEncodeContext *ctx,
             byte *ep,
             Binary *ohbin,
             Eterm ohpb,
             byte *ohp,
             Uint ohsz)
{
    byte *cp = ctx->cptr;
    if (cp != ep) {
        /* save data in common binary... */
        store_in_vec_aux(ctx,
                         ctx->result_bin,
                         THE_NON_VALUE,
                         cp,
                         ep - cp);
        ASSERT(ctx->vlen <= ctx->debug_vlen);
        ASSERT(ctx->frag_ix <= ctx->debug_fragments);
        ctx->cptr = ep;
    }
    if (ohbin) {
        /* save off-heap binary... */
        store_in_vec_aux(ctx,
                         ohbin,
                         ohpb,
                         ohp,
                         ohsz);
        ASSERT(ctx->vlen <= ctx->debug_vlen);
        ASSERT(ctx->frag_ix <= ctx->debug_fragments);
    }
}

/** @brief Is it a list of bytes not longer than MAX_STRING_LEN?
 * @param lenp out: string length or number of list cells traversed
 * @return true/false
 */
static
int
is_external_string(Eterm list, Uint* lenp)
{
    Uint len = 0;

    /*
     * Calculate the length of the list as long as all characters
     * are integers from 0 through 255.
     */
    while (is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd) || ++len > MAX_STRING_LEN) {
	    *lenp = len;
            return 0;
	}
	list = CDR(consp);
    }

    *lenp = len;
    return is_nil(list);
}


struct dec_term_hamt
{
    Eterm* objp; /* write result here */
    Uint size;   /* nr of leafs */
    Eterm* leaf_array;
};


/* Decode term from external format into *objp.
** On failure calls erts_factory_undo() and returns NULL
*/
static const byte*
dec_term(ErtsDistExternal *edep,
	 ErtsHeapFactory* factory,
	 const byte* ep,
         Eterm* objp,
	 B2TContext* ctx,
         int ets_decode)
{
#define PSTACK_TYPE struct dec_term_hamt
    PSTACK_DECLARE(hamt_array, 5);
    int n;
    ErtsAtomEncoding char_enc;
    register Eterm* hp;        /* Please don't take the address of hp */
    DECLARE_WSTACK(flat_maps); /* for preprocessing of small maps */
    Eterm* next;
    SWord reds;
#ifdef DEBUG
    Eterm* dbg_resultp = ctx ? &ctx->u.dc.res : objp;
#endif

    if (ctx) {
        reds     = ctx->reds;
        next     = ctx->u.dc.next;
        ep       = ctx->u.dc.ep;
	factory  = &ctx->u.dc.factory;

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
                    objp[0] = (Eterm) next;
                    objp[1] = make_list(next);
                    next = objp;
                    objp -= 2;
                    n--;
                }
                break;

            case B2TDecodeTuple:
                objp = next - 1;
                while (n-- > 0) {
                    objp[0] = (Eterm) next;
                    next = objp;
                    objp--;
                }
                break;

            case B2TDecodeString:
                hp = factory->hp;
                hp[-1] = make_list(hp);  /* overwrite the premature NIL */
                while (n-- > 0) {
                    hp[0] = make_small(*ep++);
                    hp[1] = make_list(hp+2);
                    hp += 2;
                }
                hp[-1] = NIL;
		factory->hp = hp;
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
	PSTACK_CHANGE_ALLOCATOR(hamt_array, ERTS_ALC_T_SAVED_ESTACK);
        WSTACK_CHANGE_ALLOCATOR(flat_maps, ERTS_ALC_T_SAVED_ESTACK);
	if (ctx->u.dc.hamt_array.pstart) {
	    PSTACK_RESTORE(hamt_array, &ctx->u.dc.hamt_array);
	}
	if (ctx->u.dc.flat_maps.wstart) {
	    WSTACK_RESTORE(flat_maps, &ctx->u.dc.flat_maps);
	}
    }
    else {
        reds = ERTS_SWORD_MAX;
        next = objp;
        *next = (Eterm) (UWord) NULL;
    }
    hp = factory->hp;

    while (next != NULL) {

	objp = next;
	next = (Eterm *) *objp;

	switch (*ep++) {
	case INTEGER_EXT:
	    {
		Sint sn = get_int32(ep);

		ep += 4;
#if defined(ARCH_64)
		*objp = make_small(sn);
#else
		if (IS_SSMALL(sn)) {
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
		const byte* first;
		const byte* last;
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
		    if (is_nil(big))
			goto error;
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
		objp[0] = (Eterm) next;
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
	    objp[0] = (Eterm) (objp+1);
	    objp[1] = (Eterm) next;
	    next = objp;
	    objp -= 2;
            n--;
	    if (ctx) {
                if (reds < n) {
                    ASSERT(reds > 0);
		    ctx->state = B2TDecodeList;
		    ctx->u.dc.remaining_n = n - reds;
		    n = reds;
		}
		reds -= n;
	    }
            while (n > 0) {
		objp[0] = (Eterm) next;
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

        if (!erts_isfinite(ff.fd)) {
            goto error;
        }

		*objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
        case PID_EXT:
        case NEW_PID_EXT:
	    factory->hp = hp;
	    ep = dec_pid(edep, factory, ep, objp, ep[-1]);
	    hp = factory->hp;
	    if (ep == NULL) {
		goto error;
	    }
	    break;
        case PORT_EXT:
        case NEW_PORT_EXT:
        case V4_PORT_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		Uint64 num;
		Uint32 cre;
                byte tag = ep[-1];

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL) {
		    goto error;
		}
		if (tag == V4_PORT_EXT) {
		    num = get_int64(ep);
		    ep += 8;
		}
		else {
		    num = get_uint32(ep);
		    ep += 4;
		}
                if (tag == PORT_EXT) {
                    cre = get_int8(ep);
                    ep++;
                    if (!is_tiny_creation(cre)) {
                        goto error;
                    }
                }
                else {
                    cre = get_int32(ep);
                    ep += 4;
                }
		node = dec_get_node(sysname, cre, make_boxed(hp));
		if(node == erts_this_node) {
		    if (num > ERTS_MAX_INTERNAL_PORT_NUMBER)
			goto error;
		    *objp = make_internal_port((Uint) num);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
		    hp += EXTERNAL_PORT_HEAP_SIZE;
		    
		    etp->header = make_external_port_header();
		    etp->next = factory->off_heap->first;
		    etp->node = node;
#ifdef ARCH_64
		    etp->data.port.id = num;
#else
		    etp->data.port.low = (Uint32) (num & 0xffffffff);
		    etp->data.port.high = (Uint32) ((num >> 32) & 0xffffffff);
#endif

		    factory->off_heap->first = (struct erl_off_heap_header*)etp;
		    *objp = make_external_port(etp);
		}

		break;
	    }
	case REFERENCE_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		int i;
		Uint32 cre;
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
		if (!is_tiny_creation(cre)) {
		    goto error;
		}
		goto ref_ext_common;

	    case NEW_REFERENCE_EXT:
		ref_words = get_int16(ep);
		ep += 2;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
		    goto error;

		cre = get_int8(ep);
		ep += 1;
		if (!is_tiny_creation(cre)) {
		    goto error;
		}
		r0 = get_int32(ep);
		ep += 4;
		if (r0 >= MAX_REFERENCE)
		    goto error;
		goto ref_ext_common;

            case NEWER_REFERENCE_EXT:
		ref_words = get_int16(ep);
		ep += 2;

		if ((ep = dec_atom(edep, ep, &sysname)) == NULL)
		    goto error;

		cre = get_int32(ep);
		ep += 4;
		r0 = get_int32(ep);
		ep += 4;

	    ref_ext_common:

		if (ref_words > ERTS_MAX_REF_NUMBERS)
		    goto error;

		node = dec_get_node(sysname, cre, make_boxed(hp));
		if(node == erts_this_node) {
                    Eterm *rtp = hp;
                    Uint32 ref_num_buf[ERTS_MAX_REF_NUMBERS];
                    if (r0 >= MAX_REFERENCE) {
                          /*
                           * Must reject local refs with more than 18 bits
                           * in first word as magic ref table relies on it.
                           */
                        goto error;
                    }

                    ref_num = &ref_num_buf[0];
                    ref_num[0] = r0;
                    for(i = 1; i < ref_words; i++) {
                        ref_num[i] = get_int32(ep);
                        ep += 4;
                    }
		    if (ref_words != ERTS_REF_NUMBERS) {
                        int i;
                        if (ref_words > ERTS_REF_NUMBERS)
                            goto error; /* Not a ref that we created... */
                        for (i = ref_words; i < ERTS_REF_NUMBERS; i++)
                            ref_num[i] = 0;
		    }
                    if (erts_is_ordinary_ref_numbers(ref_num)) {
                    make_ordinary_internal_ref:
                        write_ref_thing(hp, ref_num[0], ref_num[1], ref_num[2]);
                        hp += ERTS_REF_THING_SIZE;
                    }
                    else {
                        /* Check if it is a pid reference... */
                        Eterm pid = erts_pid_ref_lookup(ref_num);
                        if (is_internal_pid(pid)) {
                            write_pid_ref_thing(hp, ref_num[0], ref_num[1],
                                                ref_num[2], pid);
                            hp += ERTS_PID_REF_THING_SIZE;
                        }
                        else {
                            /* Check if it is a magic reference... */
                            ErtsMagicBinary *mb = erts_magic_ref_lookup_bin(ref_num);
                            if (!mb)
                                goto make_ordinary_internal_ref;
                            /* Refc on binary was increased by lookup above... */
                            ASSERT(rtp);
                            write_magic_ref_thing(hp, factory->off_heap, mb);
                            OH_OVERHEAD(factory->off_heap,
                                        mb->orig_size / sizeof(Eterm));
                            hp += ERTS_MAGIC_REF_THING_SIZE;
                        }
                    }
		    *objp = make_internal_ref(rtp);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
#if defined(ARCH_64)
		    hp += EXTERNAL_THING_HEAD_SIZE + ref_words/2 + 1;
#else
		    hp += EXTERNAL_THING_HEAD_SIZE + ref_words;
#endif

#if defined(ARCH_64)
		    etp->header = make_external_ref_header(ref_words/2 + 1);
#else
		    etp->header = make_external_ref_header(ref_words);
#endif
		    etp->next = factory->off_heap->first;
		    etp->node = node;

		    factory->off_heap->first = (struct erl_off_heap_header*)etp;
		    *objp = make_external_ref(etp);
		    ref_num = &(etp->data.ui32[0]);
#if defined(ARCH_64)
		    *(ref_num++) = ref_words /* 32-bit arity */;
#endif

                    ref_num[0] = r0;

                    for(i = 1; i < ref_words; i++) {
                        ref_num[i] = get_int32(ep);
                        ep += 4;
                    }
#if defined(ARCH_64)
                    if ((1 + ref_words) % 2)
                        ref_num[ref_words] = 0;
#endif
	    }
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
		} else if (edep && edep->data && edep->data->binp &&
                           n > (edep->data->binp->orig_size / 4)) {
                    /* If we decode a refc binary from a distribution data
                       entry we know that it is a refc binary to begin with
                       so we just increment it and use the reference. This
                       means that the entire distribution data entry will
                       remain until this binary is de-allocated so we only
                       do it if a substantial part (> 25%) of the data
                       is a binary. */
                    ProcBin* pb = (ProcBin *) hp;
                    Binary* bptr = edep->data->binp;
                    erts_refc_inc(&bptr->intern.refc, 1);
                    pb->thing_word = HEADER_PROC_BIN;
                    pb->size = n;
                    pb->next = factory->off_heap->first;
                    factory->off_heap->first = (struct erl_off_heap_header*)pb;
                    pb->val = bptr;
                    pb->bytes = (byte*) ep;
                    ERTS_ASSERT((byte*)(bptr->orig_bytes) < ep &&
                                ep+n <= (byte*)(bptr->orig_bytes+bptr->orig_size));
                    pb->flags = 0;
                    OH_OVERHEAD(factory->off_heap, pb->size / sizeof(Eterm));
                    hp += PROC_BIN_SIZE;
                    *objp = make_binary(pb);
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);

		    *objp = erts_build_proc_bin(factory->off_heap, hp, dbin);
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
		    Uint n_copy = n;

		    bin = erts_build_proc_bin(factory->off_heap, hp, dbin);
		    hp += PROC_BIN_SIZE;
                    if (ctx) {
                        int n_limit = reds * B2T_MEMCPY_FACTOR;
                        if (n > n_limit) {
                            ctx->state = B2TDecodeBinary;
                            ctx->u.dc.remaining_n = n - n_limit;
                            ctx->u.dc.remaining_bytes = dbin->orig_bytes + n_limit;
                            n_copy = n_limit;
                            reds = 0;
                        }
                        else {
                            reds -= n / B2T_MEMCPY_FACTOR;
			}
                    }
                    sys_memcpy(dbin->orig_bytes, ep, n_copy);
                    ep += n_copy;
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
		factory->hp = hp;
		ep = dec_term(edep, factory, ep, &temp, NULL, 0);
		hp = factory->hp;
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
		*hp++ = (Eterm) erts_export_get_or_make_stub(mod, name, arity);
		break;
	    }
	    break;
	case MAP_EXT:
	    {
		Uint32 size,n;
		Eterm *kptr,*vptr;
		Eterm keys;

		size = get_int32(ep); ep += 4;

                if (size <= MAP_SMALL_MAP_LIMIT) {
                    flatmap_t *mp;

                    keys  = make_tuple(hp);
                    *hp++ = make_arityval(size);
                    hp   += size;
                    kptr = hp - 1;

                    mp    = (flatmap_t*)hp;
                    hp   += MAP_HEADER_FLATMAP_SZ;
                    hp   += size;
                    vptr = hp - 1;

                    /* kptr, last word for keys
                     * vptr, last word for values
                     */

                    WSTACK_PUSH(flat_maps, (UWord)mp);
                    mp->thing_word = MAP_HEADER_FLATMAP;
                    mp->size       = size;
                    mp->keys       = keys;
                    *objp          = make_flatmap(mp);

                    for (n = size; n; n--) {
                        *vptr = (Eterm) next;
                        *kptr = (Eterm) vptr;
                        next  = kptr;
                        vptr--;
                        kptr--;
                    }
                }
                else {  /* Make hamt */
                    struct dec_term_hamt* hamt = PSTACK_PUSH(hamt_array);

                    hamt->objp = objp;
                    hamt->size = size;
                    hamt->leaf_array = hp;

                    for (n = size; n; n--) {
                        CDR(hp) = (Eterm) next;
                        CAR(hp) = (Eterm) &CDR(hp);
                        next = &CAR(hp);
                        hp += 2;
                    }
                }
	    }
	    break;
	case NEW_FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Uint arity;
		Eterm module;
		const byte* uniq;
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
		factory->hp = hp;
		/* Index */
		if ((ep = dec_term(edep, factory, ep, &temp, NULL, 0)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		/* Uniq */
		if ((ep = dec_term(edep, factory, ep, &temp, NULL, 0)) == NULL) {
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
		funp->next = factory->off_heap->first;
		factory->off_heap->first = (struct erl_off_heap_header*)funp;

		funp->fe = erts_put_fun_entry2(module, old_uniq, old_index,
					       uniq, index, arity);
		funp->arity = arity;
		hp = factory->hp;

		/* Environment */
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) next;
		    next = funp->env + i;
		}
		/* Creator */
		funp->creator = (Eterm) next;
		next = &(funp->creator);
		break;
	    }
	case ATOM_INTERNAL_REF2:
	    n = get_int16(ep);
	    ep += 2;
            /* If this is an ets_decode we know that
               the atom is valid, so we can skip the
               validation check */
	    if (!ets_decode && n >= atom_table_size()) {
		goto error;
	    }
	    *objp = make_atom(n);
	    break;
	case ATOM_INTERNAL_REF3:
	    n = get_int24(ep);
	    ep += 3;
            /* If this is an ets_decode we know that
               the atom is valid, so we can skip the
               validation check */
	    if (!ets_decode && n >= atom_table_size()) {
		goto error;
	    }
	    *objp = make_atom(n);
	    break;

	case BINARY_INTERNAL_REF:
	    {
		ProcBin* pb = (ProcBin*) hp;
		sys_memcpy(pb, ep, sizeof(ProcBin));
		ep += sizeof(ProcBin);

		erts_refc_inc(&pb->val->intern.refc, 1);
		hp += PROC_BIN_SIZE;
		pb->next = factory->off_heap->first;
		factory->off_heap->first = (struct erl_off_heap_header*)pb;
		OH_OVERHEAD(factory->off_heap, pb->size / sizeof(Eterm));
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

		erts_refc_inc(&pb->val->intern.refc, 1);
		hp += PROC_BIN_SIZE;
		pb->next = factory->off_heap->first;
		factory->off_heap->first = (struct erl_off_heap_header*)pb;
                OH_OVERHEAD(factory->off_heap, pb->size / sizeof(Eterm));
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
                    ctx->u.dc.factory.hp = hp;
		    if (!WSTACK_ISEMPTY(flat_maps)) {
			WSTACK_SAVE(flat_maps, &ctx->u.dc.flat_maps);
		    }
		    if (!PSTACK_IS_EMPTY(hamt_array)) {
			PSTACK_SAVE(hamt_array, &ctx->u.dc.hamt_array);
		    }
                    ctx->reds = 0;
                    return NULL;
                }
            }
            else {
                reds = ERTS_SWORD_MAX;
            }
        }
    }

    ASSERT(hp <= factory->hp_end
           || (factory->mode == FACTORY_CLOSED && is_immed(*dbg_resultp)));
    factory->hp = hp;
    /*
     * From here on factory may produce (more) heap fragments
     */

    if (!PSTACK_IS_EMPTY(hamt_array)) {
        do {
            struct dec_term_hamt* hamt = PSTACK_TOP(hamt_array);

            *hamt->objp = erts_hashmap_from_array(factory,
                                                  hamt->leaf_array,
                                                  hamt->size,
                                                  1);
            if (is_non_value(*hamt->objp))
                goto error_hamt;

            (void) PSTACK_POP(hamt_array);
        } while (!PSTACK_IS_EMPTY(hamt_array));
        PSTACK_DESTROY(hamt_array);
    }

    /* Iterate through all the (flat)maps and check for validity and sort keys
     * - done here for when we know it is complete.
     */

    while(!WSTACK_ISEMPTY(flat_maps)) {
        next = (Eterm *)WSTACK_POP(flat_maps);
        if (!erts_validate_and_sort_flatmap((flatmap_t*)next))
            goto error;
    }
    WSTACK_DESTROY(flat_maps);

    ASSERT((Eterm*)*dbg_resultp != NULL);

    if (ctx) {
        ctx->state = B2TDone;
	ctx->reds = reds;
        ctx->u.dc.ep = ep;
    }

    return ep;

error:
    /* UNDO:
     * Must unlink all off-heap objects that may have been
     * linked into the process. 
     */
    if (factory->mode != FACTORY_CLOSED) {
	if (factory->hp < hp) { /* Sometimes we used hp and sometimes factory->hp */
	    factory->hp = hp;   /* the largest must be the freshest */
	}
    }
    else ASSERT(!factory->hp || factory->hp == hp);

error_hamt:
    erts_factory_undo(factory);
    PSTACK_DESTROY(hamt_array);
    if (ctx) {
	ctx->state = B2TDecodeFail;
	ctx->reds = reds;
    }
    WSTACK_DESTROY(flat_maps);
        
    return NULL;
}

/* returns the number of bytes needed to encode an object
   to a sequence of bytes
   N.B. That this must agree with to_external2() above!!!
   (except for cached atoms) */
static Uint encode_size_struct2(ErtsAtomCacheMap *acmp,
                                Eterm obj,
                                Uint64 dflags) {
    Uint size = 0;
    ErtsExtSzRes res = encode_size_struct_int(NULL, acmp, obj,
                                              dflags, NULL,
                                              &size);
    /*
     * encode_size_struct2() only allowed when
     * we know the result will always be OK!
     */ 
    ASSERT(res == ERTS_EXT_SZ_OK); (void) res;
    return (Uint) size;
}

static ErtsExtSzRes
encode_size_struct_int(TTBSizeContext* ctx, ErtsAtomCacheMap *acmp, Eterm obj,
		       Uint64 dflags, Sint *reds, Uint *res)
{
    DECLARE_WSTACK(s);
    Uint m, i, arity;
    Uint result = *res;
    Sint r = 0;
    int vlen = -1;

    if (ctx) {
	WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	r = *reds;

        vlen = ctx->vlen;
        
	if (!ctx->wstack.wstart)
            ctx->last_result = result;
        else { /* restore saved stack */
	    WSTACK_RESTORE(s, &ctx->wstack);
	    result = ctx->result;
	    obj = ctx->obj;
	}
    }

#define LIST_TAIL_OP ((0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER)
#define TERM_ARRAY_OP(N) (((N) << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER)
#define TERM_ARRAY_OP_DEC(OP) ((OP) - (1 << _TAG_PRIMARY_SIZE))


    for (;;) {
	ASSERT(!is_header(obj));

	if (ctx && --r <= 0) {
	    *reds = 0;
	    ctx->obj = obj;
	    ctx->result = result;
            ctx->vlen = vlen;
	    WSTACK_SAVE(s, &ctx->wstack);
	    return ERTS_EXT_SZ_YIELD;
	}
	switch (tag_val_def(obj)) {
	case NIL_DEF:
	    result++;
	    break;
	case ATOM_DEF:
	    if (dflags & DFLAG_ETS_COMPRESSED) {
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
        case EXTERNAL_PID_DEF:
	case PID_DEF:
	    result += (1 + encode_size_struct2(acmp, pid_node_name(obj), dflags) +
		       4 + 4 + 4);
	    break;
        case EXTERNAL_REF_DEF:
	case REF_DEF:
	    i = ref_no_numbers(obj);
	    result += (1 + 2 + encode_size_struct2(acmp, ref_node_name(obj), dflags) +
		       4 + 4*i);
	    break;
        case EXTERNAL_PORT_DEF:
        case PORT_DEF: {
	    Uint64 num = port_number(obj);
	    result += (num > ERTS_MAX_V3_PORT_NUMBER) ? 8 : 4;
	    result += (1 + encode_size_struct2(acmp, port_node_name(obj), dflags)
		       /* num */ + 4);
	    break;
	}
	case LIST_DEF: {
	    int is_str = is_external_string(obj, &m);
	    r -= m/2;
	    if (is_str) {
		result += m + 2 + 1;
	    } else {
		result += 5;
		WSTACK_PUSH2(s, (UWord)CDR(list_val(obj)), (UWord)LIST_TAIL_OP);
		obj = CAR(list_val(obj));
		continue; /* big loop */
	    }
	    break;
	}
	case TUPLE_DEF:
	    {
		Eterm* ptr = tuple_val(obj);
		arity = arityval(*ptr);
		if (arity <= 0xff) {
		    result += 1 + 1;
		} else {
		    result += 1 + 4;
		}
		if (arity > 1) {
		    WSTACK_PUSH2(s, (UWord) (ptr + 2),
				    (UWord) TERM_ARRAY_OP(arity-1));
		}
                else if (arity == 0) {
		    break;
                }
		obj = ptr[1];
		continue; /* big loop */
	    }
	case MAP_DEF:
	    if (is_flatmap(obj)) {
		flatmap_t *mp = (flatmap_t*)flatmap_val(obj);
		Uint size = flatmap_get_size(mp);

		result += 1 + 4; /* tag + 4 bytes size */

                if (size) {
		    WSTACK_PUSH4(s, (UWord) flatmap_get_values(mp),
				    (UWord) TERM_ARRAY_OP(size),
		                    (UWord) flatmap_get_keys(mp),
				    (UWord) TERM_ARRAY_OP(size));
		}
	    } else {
		Eterm *ptr;
		Eterm hdr;
		Uint node_sz;
		ptr = boxed_val(obj);
		hdr = *ptr;
		ASSERT(is_header(hdr));
		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		case HAMT_SUBTAG_HEAD_ARRAY:
		    ptr++;
		    node_sz = 16;
		    result += 1 + 4; /* tag + 4 bytes size */
		    break;
		case HAMT_SUBTAG_HEAD_BITMAP:
		    ptr++;
		    result += 1 + 4; /* tag + 4 bytes size */
		    /*fall through*/
		case HAMT_SUBTAG_NODE_BITMAP:
		    node_sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
		    ASSERT(node_sz < 17);
		    break;
		default:
		    erts_exit(ERTS_ERROR_EXIT, "bad header\r\n");
		}

		ptr++;
		WSTACK_RESERVE(s, node_sz*2);
		while(node_sz--) {
                    if (is_list(*ptr)) {
			WSTACK_FAST_PUSH(s, CAR(list_val(*ptr)));
			WSTACK_FAST_PUSH(s, CDR(list_val(*ptr)));
                    } else {
			WSTACK_FAST_PUSH(s, *ptr);
		    }
		    ptr++;
		}
	    }
	    break;
	case FLOAT_DEF:
	    if (dflags & DFLAG_NEW_FLOATS) {
		result += 9;
	    } else {
		result += 32;   /* Yes, including the tag */
	    }
	    break;
	case BINARY_DEF: {
            ProcBin* pb = (ProcBin*) binary_val(obj);
            Uint bin_size = pb->size;
            byte bitoffs = 0;
            byte bitsize = 0;
            if (dflags & DFLAG_ETS_COMPRESSED) {
		ProcBin* pb = (ProcBin*) binary_val(obj);
		Uint sub_extra = 0;
		if (pb->thing_word == HEADER_SUB_BIN) {
		    ErlSubBin* sub = (ErlSubBin*) pb;
                    bitoffs = sub->bitoffs;
                    bitsize = sub->bitsize;
		    pb = (ProcBin*) binary_val(sub->orig);
		    sub_extra = 2;  /* bitoffs and bitsize */
		    bin_size += (bitoffs + bitsize + 7) / 8;
		}
		if (pb->thing_word == HEADER_PROC_BIN
		    && heap_bin_size(bin_size) > PROC_BIN_SIZE) {

		    result += 1 + sub_extra + sizeof(ProcBin);
		    break;
		}
            }
            else {
#ifdef ARCH_64
                if (bin_size >= (Uint) 0xffffffff) {
                    if (pb->thing_word == HEADER_SUB_BIN) {
                        ErlSubBin* sub = (ErlSubBin*) pb;
                        bin_size += (sub->bitoffs + sub->bitsize+ 7) / 8;
                    }
                    if (bin_size > (Uint) 0xffffffff) {
                        WSTACK_DESTROY(s);
                        return ERTS_EXT_SZ_SYSTEM_LIMIT;
                    }
                }
#endif
                if (pb->thing_word == HEADER_SUB_BIN) {
                    ErlSubBin* sub = (ErlSubBin*) pb;
                    bitoffs = sub->bitoffs;
                    bitsize = sub->bitsize;
                    pb = (ProcBin*) binary_val(sub->orig);
                }
                if (vlen >= 0) {
                    Uint csz;
                    if (pb->thing_word == HEADER_PROC_BIN
                        && bitoffs == 0
                        && bin_size > ERL_ONHEAP_BIN_LIMIT) {
                        Uint trailing_result;
                        if (bitsize == 0) {
                            result += (1 /* BIT_BINARY_EXT */
                                       + 4 /* size */);
                            trailing_result = 0;
                        }
                        else {
                            result += (1 /* BIT_BINARY_EXT */
                                       + 4 /* size */
                                       + 1 /* trailing bitsize */);
                            trailing_result = 1 /* trailing bits */;
                        }
                        csz = result - ctx->last_result;
                        ctx->last_result = result;
                        result += trailing_result;
                        vlen += 2; /* data leading up to binary and binary */

                        /* potentially multiple elements leading up to binary */
                        vlen += csz/MAX_SYSIOVEC_IOVLEN;
                        /* potentially multiple elements for binary */
                        vlen += bin_size/MAX_SYSIOVEC_IOVLEN;
                        ctx->extra_size += bin_size;

                        if (dflags & DFLAG_PENDING_CONNECT) {
			    ASSERT(ctx);
                            vlen += 2; /* for hopefull prolog and epilog */
                            result += (4 /* for hopefull prolog (see below) */
                                       + 4); /* for hopefull epilog (see below) */
                            ctx->last_result = result;
                        }
                        break;
                    }
                }
	    }

            if (bitsize == 0) {
                result += (1 /* BIT_BINARY_EXT */
                           + 4 /* size */
                           + bin_size);
            }
            else if (dflags & DFLAG_PENDING_CONNECT) {
                /* This is the odd case when we have an un-aligned bit-string
                   during a pending connect. */
                Uint csz;
		ASSERT(ctx);
                csz = result - ctx->last_result;
                /* potentially multiple elements leading up to binary */
                vlen += (csz + MAX_SYSIOVEC_IOVLEN - 1)/MAX_SYSIOVEC_IOVLEN;

                vlen++; /* hopefull prolog */
                /*
                 * Size for hopefull prolog is max of
                 * - fallback: 1 + 1 + 1 + 4
                 * - hopfull index + bit binary prolog: 4 + 1 + 4 + 1
                 */
                result += 4 + 1 + 4 + 1;
                /* potentially multiple elements for binary */
                vlen += bin_size/MAX_SYSIOVEC_IOVLEN + 1;
                result += bin_size;
                vlen++; /* hopefull epiolog */
                /*
                 * Size for hopefull epiolog is max of
                 * - fallback: 1 + 1 + 1
                 * - hopfull index + bit binary epilog: 4 + 1
                 */
                result += 4 + 1;
                ctx->last_result = result;
            }
            else {
                result += 1 + 4 + 1 + bin_size + 1;
            }
	    break;
        }
	case FUN_DEF:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(obj);

                result += 20+1+1+4;	/* New ID + Tag */
                result += 4; /* Length field (number of free variables */
                result += encode_size_struct2(acmp, funp->creator, dflags);
                result += encode_size_struct2(acmp, funp->fe->module, dflags);
                result += 2 * (1+4);	/* Index, Uniq */
		if (funp->num_free > 1) {
		    WSTACK_PUSH2(s, (UWord) (funp->env + 1),
				    (UWord) TERM_ARRAY_OP(funp->num_free-1));
		}
		if (funp->num_free != 0) {
		    obj = funp->env[0];
		    continue; /* big loop */
		}
		break;
	    }

	case EXPORT_DEF:
	    {
		Export* ep = *((Export **) (export_val(obj) + 1));
                Uint tmp_result = result;
		result += 1;
		result += encode_size_struct2(acmp, ep->info.mfa.module, dflags);
		result += encode_size_struct2(acmp, ep->info.mfa.function, dflags);
		result += encode_size_struct2(acmp, make_small(ep->info.mfa.arity), dflags);
                if (dflags & DFLAG_PENDING_CONNECT) {
                    Uint csz;
		    ASSERT(ctx);

                    /*
                     * Fallback is 1 + 1 + Module size + Function size, that is,
                     * the hopefull index + hopefull encoding is larger...
                     */
                    ASSERT(dflags & DFLAG_EXPORT_PTR_TAG);
                    csz = tmp_result - ctx->last_result;
                    /* potentially multiple elements leading up to hopefull entry */
                    vlen += (csz/MAX_SYSIOVEC_IOVLEN + 1
			     + 1); /* hopefull entry */
                    result += 4; /* hopefull index */
                    ctx->last_result = result;
                }
	    }
	    break;

	default:
	    erts_exit(ERTS_ERROR_EXIT,"Internal data structure error (in encode_size_struct_int) %x\n",
		     obj);
	}

	if (WSTACK_ISEMPTY(s)) {
	    break;
	}
	obj = (Eterm) WSTACK_POP(s);

        if (is_header(obj)) {
            switch (obj) {
            case LIST_TAIL_OP:
		obj = (Eterm) WSTACK_POP(s);
		if (is_list(obj)) {
		    Eterm* cons = list_val(obj);

		    WSTACK_PUSH2(s, (UWord)CDR(cons), (UWord)LIST_TAIL_OP);
		    obj = CAR(cons);
		}
		break;

	    case TERM_ARRAY_OP(1):
		obj = *(Eterm*)WSTACK_POP(s);
		break;
	    default: { /* TERM_ARRAY_OP(N) when N > 1 */
		Eterm* ptr = (Eterm*) WSTACK_POP(s);
		WSTACK_PUSH2(s, (UWord) (ptr+1),
			        (UWord) TERM_ARRAY_OP_DEC(obj));
		obj = *ptr;
	    }
	    }
	}
    }

    WSTACK_DESTROY(s);
    if (ctx) {
	ASSERT(ctx->wstack.wstart == NULL);
	*reds = r < 0 ? 0 : r;

        if (vlen >= 0) {
            Uint csz;
            csz = result - ctx->last_result;
            if (csz)
                vlen += csz/MAX_SYSIOVEC_IOVLEN + 1;
            ctx->vlen = vlen;
        }
    }
    *res = result;
    return ERTS_EXT_SZ_OK;
}



static Sint
decoded_size(const byte *ep, const byte* endp, int internal_tags, B2TContext* ctx)
{
    Sint heap_size;
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
        ERTS_UNDEF(reds, 0);

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
#if !defined(ARCH_64)
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
	    n = get_uint32(ep);
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
        case NEW_PID_EXT:
	    atom_extra_skip = 12;
	    goto case_PID;
        case PID_EXT:
	    atom_extra_skip = 9;
	case_PID:
	    /* In case it is an external pid */
	    heap_size += EXTERNAL_PID_HEAP_SIZE;
	    terms++;
	    break;
        case V4_PORT_EXT:
	    atom_extra_skip = 12;
	    goto case_PORT;
        case NEW_PORT_EXT:
	    atom_extra_skip = 8;
	    goto case_PORT;
        case PORT_EXT:
	    atom_extra_skip = 5;
	case_PORT:
	    /* In case it is an external port */
	    heap_size += EXTERNAL_PORT_HEAP_SIZE;
	    terms++;
	    break;
	case NEWER_REFERENCE_EXT:
	    atom_extra_skip = 4;
	    goto case_NEW_REFERENCE;
        case NEW_REFERENCE_EXT:
	    atom_extra_skip = 1;
	case_NEW_REFERENCE:
	    {
		int id_words;

		CHKSIZE(2);
		id_words = get_int16(ep);
		    
		if (id_words > ERTS_MAX_REF_NUMBERS)
		    goto error;

		ep += 2;
		atom_extra_skip += 4*id_words;
		/* In case it is an external ref */
#if defined(ARCH_64)
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
	    n = get_uint32(ep);
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
	    n = get_uint32(ep);
	    ep += 4;
	    ADDTERMS(n);
	    heap_size += n + 1;
	    break;
	case MAP_EXT:
	    CHKSIZE(4);
	    n = get_uint32(ep);
	    ep += 4;
	    ADDTERMS(2*n);
            if (n <= MAP_SMALL_MAP_LIMIT) {
                heap_size += 3 + n + 1 + n;
            } else {
                heap_size += HASHMAP_ESTIMATED_HEAP_SIZE(n);
            }
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
	    n = get_uint32(ep);
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
		n = get_uint32(ep);
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
	    heap_size += 2;
	    break;
	case NEW_FUN_EXT:
	    {
		unsigned num_free;
		Uint total_size;

		CHKSIZE(1+16+4+4);
		total_size = get_uint32(ep);
		CHKSIZE(total_size);		
		ep += 1+16+4+4;
		CHKSIZE(4);
		num_free = get_uint32(ep);
		ep += 4;
		if (num_free > MAX_ARG) {
		    goto error;
		}
		terms += 4 + num_free;
		heap_size += ERL_FUN_SIZE + num_free;
		break;
	    }
	case FUN_EXT:
            /*
             * OTP 23: No longer support decoding the old fun
             * representation.
             */
            goto error;
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

#define ERTS_TRANSCODE_REDS_FACT 4
typedef struct {
    ErtsHeapFactory factory;
    Eterm *hp;
} ErtsTranscodeDecodeState;

static Eterm
transcode_decode_ctl_msg(ErtsTranscodeDecodeState *state,
                         SysIOVec *iov,
                         int end_ix)
{
    Eterm ctl_msg, *hp;
    Uint buf_sz;
    byte *buf_start, *buf_end;
    const byte *ptr;
    Uint hsz;

    if (end_ix == 3) {
        /* The whole control message is in iov[2].iov_base */
        buf_sz = (Uint) iov[2].iov_len;
        buf_start = (byte *) iov[2].iov_base;
        buf_end = buf_start + buf_sz;
    }
    else {
        /* Control message over multiple buffers... */
        int ix;
        buf_sz = 0;
        for (ix = 2; ix < end_ix; ix++)
            buf_sz += iov[ix].iov_len;
        ptr = buf_start = erts_alloc(ERTS_ALC_T_TMP, buf_sz);
        buf_end = buf_start + buf_sz;
        for (ix = 2; ix < end_ix; ix++) {
            sys_memcpy((void *) ptr,
                       (void *) iov[ix].iov_base,
                       iov[ix].iov_len);
            ptr += iov[ix].iov_len;
        }
    }

    hsz = decoded_size(buf_start, buf_end, 0, NULL);
    state->hp = hp = erts_alloc(ERTS_ALC_T_TMP, hsz*sizeof(Eterm));
    erts_factory_tmp_init(&state->factory, hp, hsz, ERTS_ALC_T_TMP);
            
    ptr = dec_term(NULL, &state->factory, buf_start, &ctl_msg, NULL, 0);
    ASSERT(ptr); (void)ptr;
    ASSERT(is_tuple(ctl_msg));

    if (buf_start != (byte *) iov[2].iov_base)
        erts_free(ERTS_ALC_T_TMP, buf_start);
    
    return ctl_msg;
}

static void
transcode_decode_state_destroy(ErtsTranscodeDecodeState *state)
{
    erts_factory_close(&state->factory);
    erts_free(ERTS_ALC_T_TMP, state->hp);    
}

static
Sint transcode_dist_obuf(ErtsDistOutputBuf* ob,
                         DistEntry* dep,
                         Uint64 dflags,
                         Sint reds)
{
    ErlIOVec* eiov = ob->eiov;
    SysIOVec* iov = eiov->iov;
    byte *hdr;
    Uint32 payload_ix;
    Sint start_r, r;
    byte *ep;

    if (reds < 0)
        return reds;

    /*
     * HOPEFUL_DATA header always present in io vector
     * element 1:
     *
     * +---+--------------+-----------+----------+
     * |'H'|Hopefull Flags|Hopefull IX|Payload IX|
     * +---+--------------+-----------+----------+
     *   1         8            4          4
     *
     * Hopefull flags: Flags corresponding to actual
     *                 hopefull encodings in this
     *                 buffer.
     * Hopefull IX:    Vector index of first hopefull
     *                 encoding. Each hopefull encoding
     *                 is preceeded by 4 bytes containing
     *                 next vector index of hopefull
     *                 encoding. ERTS_NO_HIX marks the
     *                 end.
     * Payload IX:     Vector index of the beginning
     *                 of the payload if there is
     *                 one; otherwise, zero.
     */
    hdr = (byte *) iov[1].iov_base;

    ASSERT(HOPEFUL_DATA == *((byte *)iov[1].iov_base));
    ASSERT(iov[1].iov_len == 1+8+4+4);
    
    /* Control message always begin in vector element 2 */
    ep = iov[2].iov_base;
    ASSERT(ep[0] == SMALL_TUPLE_EXT || ep[0] == LARGE_TUPLE_EXT);

    if (((~dflags & (DFLAG_DIST_MONITOR | DFLAG_DIST_MONITOR_NAME))
         && ep[0] == SMALL_TUPLE_EXT
         && ep[1] == 4
         && ep[2] == SMALL_INTEGER_EXT
         && (ep[3] == DOP_MONITOR_P ||
             ep[3] == DOP_MONITOR_P_EXIT ||
             ep[3] == DOP_DEMONITOR_P)
         /* The receiver does not support process monitoring.
            Suppress monitor control msg (see erts_dsig_send_monitor). */)
        || (!(dflags & DFLAG_ALIAS)
            && ep[0] == SMALL_TUPLE_EXT
            && (ep[1] == 3 || ep[1] == 4)
            && ep[2] == SMALL_INTEGER_EXT
            && ((ep[3] == DOP_ALIAS_SEND) || (ep[3] == DOP_ALIAS_SEND_TT))
        /* The receiver does not support alias, so the alias
               is obviously not present at the receiver. */)) {
        /*
         * Drop packet by converting it to an empty (tick) packet...
         */
        int i;
        for (i = 1; i < ob->eiov->vsize; i++) {
            if (ob->eiov->binv[i])
                driver_free_binary(ob->eiov->binv[i]);
        }
        ob->eiov->vsize = 1;
        ob->eiov->size = 0;
        return reds;
    }

    /* Currently, the hopefull flags and IX are not used. */
    hdr++;
    hdr += 8;

    if ((~dflags & DFLAG_SPAWN)
        && ep[0] == SMALL_TUPLE_EXT
        && ((ep[1] == 6
             && ep[2] == SMALL_INTEGER_EXT
             && ep[3] == DOP_SPAWN_REQUEST)
            || (ep[1] == 8
                && ep[2] == SMALL_INTEGER_EXT
                && ep[3] == DOP_SPAWN_REQUEST_TT))) {
        /*
         * Receiver does not support distributed spawn. Convert
         * this packet to an empty (tick) packet, and inform
         * spawning process that this is not supported...
         */
        ErtsTranscodeDecodeState tds;
        Eterm ctl_msg, ref, pid, token, *tp;
        int i;

        hdr += 4;
        payload_ix = get_int32(hdr);
        ASSERT(payload_ix >= 3);

        ctl_msg = transcode_decode_ctl_msg(&tds, iov, payload_ix);

        ASSERT(is_tuple_arity(ctl_msg, 6)
               || is_tuple_arity(ctl_msg, 8));
        tp = tuple_val(ctl_msg);
        ASSERT(tp[1] == make_small(DOP_SPAWN_REQUEST)
               || tp[1] == make_small(DOP_SPAWN_REQUEST_TT));

        ref = tp[2];
        pid = tp[3];
        if (tp[1] == make_small(DOP_SPAWN_REQUEST))
            token = NIL;
        else {
            token = tp[8];
            erts_seq_trace_update_node_token(token);
        }
        ASSERT(is_internal_ordinary_ref(tp[2]));
        ASSERT(is_internal_pid(tp[3]));
        
        (void) erts_proc_sig_send_dist_spawn_reply(dep->sysname,
                                                   ref, pid,
                                                   NULL, am_notsup,
                                                   token);

        transcode_decode_state_destroy(&tds);

        for (i = 1; i < ob->eiov->vsize; i++) {
            if (ob->eiov->binv[i])
                driver_free_binary(ob->eiov->binv[i]);
        }
        ob->eiov->vsize = 1;
        ob->eiov->size = 0;
        
        reds -= 4;
        
        if (reds < 0)
            return 0;
        return reds;
    }

    if ((~dflags & DFLAG_UNLINK_ID)
        && ep[0] == SMALL_TUPLE_EXT
        && ep[1] == 4
        && ep[2] == SMALL_INTEGER_EXT
        && (ep[3] == DOP_UNLINK_ID_ACK || ep[3] == DOP_UNLINK_ID)) {

        if (ep[3] == DOP_UNLINK_ID_ACK) {
            /* Drop DOP_UNLINK_ID_ACK signal... */
            int i;
            for (i = 1; i < ob->eiov->vsize; i++) {
                if (ob->eiov->binv[i])
                    driver_free_binary(ob->eiov->binv[i]);
            }
            ob->eiov->vsize = 1;
            ob->eiov->size = 0;
        }
        else {
            Eterm ctl_msg, remote, local, *tp;
            ErtsTranscodeDecodeState tds;
            Uint64 id;
            byte *ptr;
            ASSERT(ep[3] == DOP_UNLINK_ID);
            /*
             * Rewrite the DOP_UNLINK_ID signal into a
             * DOP_UNLINK signal and send an unlink ack
             * to the local sender.
             */

            /*
             * decode control message so we get info
             * needed for unlink ack signal to send...
             */
            ASSERT(get_int32(hdr + 4) == 0); /* No payload */
            ctl_msg = transcode_decode_ctl_msg(&tds, iov, eiov->vsize);

            ASSERT(is_tuple_arity(ctl_msg, 4));
            
            tp = tuple_val(ctl_msg);
            ASSERT(tp[1] == make_small(DOP_UNLINK_ID));

            if (!term_to_Uint64(tp[2], &id))
                ERTS_INTERNAL_ERROR("Invalid encoding of DOP_UNLINK_ID signal");
            
            local = tp[3];
            remote = tp[4];

            ASSERT(is_internal_pid(local));
            ASSERT(is_external_pid(remote));

            /*
             * Rewrite buffer to an unlink signal by removing
             * second element and change first element to
             * DOP_UNLINK. That is, to: {DOP_UNLINK, local, remote}
             */

            ptr = &ep[4];
            switch (*ptr) {
            case SMALL_INTEGER_EXT:
                ptr += 1;
                break;
            case INTEGER_EXT:
                ptr += 4;
                break;
            case SMALL_BIG_EXT:
                ptr += 1;
                ASSERT(*ptr <= 8);
                ptr += *ptr + 1;
                break;
            default:
                ERTS_INTERNAL_ERROR("Invalid encoding of DOP_UNLINK_ID signal");
                break;
            }

            ASSERT((ptr - ep) <= 16);
            ASSERT((ptr - ep) <= iov[2].iov_len);
            
            *(ptr--) = DOP_UNLINK;
            *(ptr--) = SMALL_INTEGER_EXT;
            *(ptr--) = 3;
            *ptr = SMALL_TUPLE_EXT;

            iov[2].iov_base = ptr;
            iov[2].iov_len -= (ptr - ep);

#ifdef DEBUG
            {
                ErtsTranscodeDecodeState dbg_tds;
                Eterm new_ctl_msg = transcode_decode_ctl_msg(&dbg_tds,
                                                             iov,
                                                             eiov->vsize);
                ASSERT(is_tuple_arity(new_ctl_msg, 3));
                tp = tuple_val(new_ctl_msg);
                ASSERT(tp[1] == make_small(DOP_UNLINK));
                ASSERT(tp[2] == local);
                ASSERT(eq(tp[3], remote));
                transcode_decode_state_destroy(&dbg_tds);
            }
#endif

            /* Send unlink ack to local sender... */
            erts_proc_sig_send_dist_unlink_ack(NULL, dep,
                                               dep->connection_id,
                                               remote, local, id);

            transcode_decode_state_destroy(&tds);

            reds -= 5;
        }
        if (reds < 0)
            return 0;
        return reds;
    }
    
    start_r = r = reds*ERTS_TRANSCODE_REDS_FACT;

    /*
     * Replace hopefull data header with actual header...
     */
    ep = (byte *) iov[1].iov_base;
    eiov->size -= iov[1].iov_len;

    if (dflags & (DFLAG_DIST_HDR_ATOM_CACHE|DFLAG_FRAGMENTS)) {
        /*
         * Encoding was done without atom caching but receiver expects
         * a dist header, so we prepend an empty one.
         */
        *ep++ = VERSION_MAGIC;
        *ep++ = DIST_HEADER;
        *ep++ = 0; /* NumberOfAtomCacheRefs */
    }
    else {
        hdr += 4;
        payload_ix = get_int32(hdr);

        if (payload_ix) {
            ASSERT(0 < payload_ix && payload_ix < eiov->vsize);
            /* Prepend version magic on payload. */
            iov[payload_ix].iov_base = &((byte*)iov[payload_ix].iov_base)[-1];
            *((byte *) iov[payload_ix].iov_base) = VERSION_MAGIC;
            iov[payload_ix].iov_len++;
            eiov->size++;
            r--;
        }

        *ep++ = PASS_THROUGH;
        *ep++ = VERSION_MAGIC;
    }

    iov[1].iov_len = ep - (byte *) iov[1].iov_base;
    eiov->size += iov[1].iov_len;

    r--;

    /* done... */

    reds -= (start_r - r)/ERTS_TRANSCODE_REDS_FACT + 1;
    if (reds < 0)
        return 0;
    return reds;
}
