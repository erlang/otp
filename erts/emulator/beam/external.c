/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
#include "erl_global_literals.h"
#include "erl_term_hashing.h"


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
static const byte* dec_atom(ErtsDistExternal *, const byte*, Eterm*, int);
static const byte* dec_pid(ErtsDistExternal *, ErtsHeapFactory*, const byte*, Eterm*, byte tag, int);
static Sint decoded_size(const byte *ep, const byte* endp, int internal_tags, struct B2TContext_t*);
static BIF_RETTYPE term_to_binary_trap_1(BIF_ALIST_1);

static Eterm erts_term_to_binary_int(Process* p, Sint bif_ix, Eterm Term, Eterm opts, int level,
                                     Uint64 dflags, Binary *context_b, int iovec,
                                     Uint fragment_size);

static ErtsExtSzRes encode_size_struct_int(TTBSizeContext*, ErtsAtomCacheMap *acmp,
                                           Eterm obj, Uint64 dflags, Sint *reds, Uint *res);

static Export binary_to_term_trap_export;
static BIF_RETTYPE binary_to_term_trap_1(BIF_ALIST_1);
static Sint transcode_dist_obuf(ErtsDistOutputBuf*, DistEntry*, Uint64 dflags, Sint reds);
static void store_in_vec(TTBEncodeContext *ctx, byte *ep, Binary *ohbin, Eterm ohpb,
                         byte *ohp, Uint ohsz);
static Uint32 calc_iovec_fun_size(SysIOVec* iov, Uint32 fun_high_ix, byte* size_p);

void erts_init_external(void) {
    erts_init_trap_export(&term_to_binary_trap_export,
			  am_erts_internal, am_term_to_binary_trap, 1,
			  &term_to_binary_trap_1);

    erts_init_trap_export(&binary_to_term_trap_export,
			  am_erts_internal, am_binary_to_term_trap, 1,
			  &binary_to_term_trap_1);
    return;
}

static Uint32 local_node_hash;

void erts_late_init_external(void)
{
    char hname[256], pidstr[21];
    size_t hname_len, pidstr_len;
    ErtsMonotonicTime mtime, toffs;
    ErtsBlockHashState hstate;
    byte *lnid;
    Uint lnid_ix, chunk_size;
    int res;

    res = sys_get_hostname(&hname[0], sizeof(hname));
    if (res == 0) {
        hname_len = strlen(hname);
    }
    else {
        hname[0] = '\0';
        hname_len = 0;
    }

    sys_get_pid(&pidstr[0], sizeof(pidstr));
    pidstr[20] = '\0';

    pidstr_len = strlen(pidstr);

    toffs = erts_get_time_offset();
    mtime = erts_get_monotonic_time(NULL);

    lnid = (byte *) erts_alloc(ERTS_ALC_T_TMP, 8 + hname_len + pidstr_len);

    lnid_ix = 0;

    /* time offset... */
    lnid[lnid_ix++] = (byte) toffs & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 8) & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 16) & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 24) & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 32) & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 40) & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 48) & 0xff;
    lnid[lnid_ix++] = (byte) (toffs >> 56) & 0xff;

    /* hostname... */

    sys_memcpy(&lnid[lnid_ix], &hname[0], hname_len);

    lnid_ix += hname_len;

    /* pid... */
    memcpy(&lnid[lnid_ix], &pidstr[0], pidstr_len);

    lnid_ix += pidstr_len;

    /*
     * Use least significant 32 bits of monotonic time as initial
     * value to hash...
     */
    erts_block_hash_init(&hstate, &lnid[0], lnid_ix,
                         (Uint32) (mtime & 0xffffffff));
    chunk_size = ERTS_UINT_MAX;
    res = erts_block_hash(&local_node_hash, &chunk_size, &hstate);
    ASSERT(res); (void) res;
    ASSERT(chunk_size == lnid_ix);

    erts_free(ERTS_ALC_T_TMP, lnid);
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
        /* HOPEFUL_DATA + hopeful flags + hopeful ix + payload ix */
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
        ctx->hopeful_ixp = ep;
        put_int32(ERTS_NO_HIX, ep);
        ep -= 8;
        ctx->hopeful_flagsp = ep;
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
                sys_memcpy((void *) ep, (void *) erts_atom_get_name(a), sz);
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
                    ERTS_FALLTHROUGH();
		case 3:
		    *--ep = (byte) ((flgs >> 16) & 0xff);
                    ERTS_FALLTHROUGH();
		case 2:
		    *--ep = (byte) ((flgs >> 8) & 0xff);
                    ERTS_FALLTHROUGH();
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

ErtsExtSzRes erts_encode_ext_size_2(Eterm term, Uint64 dflags, Uint *szp)
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
    ErtsExtSzRes res;
    Uint sz = 0;

    res = encode_size_struct_int(NULL, NULL, term,
                                 TERM_TO_BINARY_DFLAGS|DFLAG_ETS_COMPRESSED,
                                 NULL, &sz);
    ASSERT(res == ERTS_EXT_SZ_OK); (void) res;
    return sz;
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
        ASSERT(ctx->hopeful_flagsp);
        put_int64(ctx->hopeful_flags, ctx->hopeful_flagsp);
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
		      const byte *ext,
		      Uint size,
                      Binary *binp,
		      DistEntry *dep,
                      Uint32 conn_id,
		      ErtsAtomCache *cache)
{
    register const byte *ep;

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
	    const byte *flgs_buf = ep;
#endif
	    const byte *flgsp = ep;
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
                            ERTS_FALLTHROUGH();
			case 4:
			case 3:
			    flgs |= (((Uint32) flgsp[1]) << 8);
                            ERTS_FALLTHROUGH();
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
	const byte *ep;
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
    const byte *ep;
    Sint res;

    if (edep->data->frag_id > 1 && payload) {
        Uint sz = 0;
        Binary *bin;
        byte *buf;
        int i;

        for (i = 0; i < edep->data->frag_id; i++)
            sz += edep->data[i].ext_endp - edep->data[i].extp;

        bin = erts_bin_nrml_alloc(sz);
        buf = (byte*)bin->orig_bytes;

        for (i = 0; i < edep->data->frag_id; i++) {
            sys_memcpy(buf,
                       edep->data[i].extp,
                       edep->data[i].ext_endp - edep->data[i].extp);

            buf += edep->data[i].ext_endp - edep->data[i].extp;
            erts_bin_release(edep->data[i].binp);
            edep->data[i].binp = NULL;
            edep->data[i].extp = NULL;
            edep->data[i].ext_endp = NULL;
        }

        edep->data->frag_id = 1;
        edep->data->extp = (byte*)bin->orig_bytes;
        edep->data->ext_endp = buf;
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
    Uint offset, size;
    byte *base;
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

    if (is_not_bitstring(BIF_ARG_2)) {
        goto badarg;
    }

    ERTS_GET_BITSTRING(BIF_ARG_2, base, offset, size);

    if (size == 0 || TAIL_BITS(size) != 0) {
        goto badarg;
    }

    ERTS_ASSERT(BIT_OFFSET(offset) == 0);
    offset = BYTE_OFFSET(offset);
    size = BYTE_SIZE(size);

    ede.data->extp = &base[offset];
    ede.data->ext_endp = &ede.data->extp[size];
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
            || MSO(BIF_P).overhead > BIF_P->bin_vheap_sz) {
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
            || MSO(BIF_P).overhead > BIF_P->bin_vheap_sz)
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
parse_t2b_opts(Eterm opts, Uint64 *flagsp, int *levelp, int *iovecp, Uint *fsizep)
{
    int level = 0;
    int iovec = 0;
    Uint64 flags = TERM_TO_BINARY_DFLAGS;
    int deterministic = 0, local = 0;
    Uint fsize = ~((Uint) 0); /* one fragment */

    while (is_list(opts)) {
	Eterm arg = CAR(list_val(opts));
	Eterm* tp;
	if (arg == am_compressed) {
	    level = Z_DEFAULT_COMPRESSION;
        }
        else if (iovecp && arg == am_iovec) {
            iovec = !0;
        } else if (arg == am_deterministic) {
            deterministic = 1;
        } else if (arg == am_local) {
            local = !0;
	} else if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
	    if (tp[1] == am_minor_version && is_small(tp[2])) {
		switch (signed_val(tp[2])) {
		case 0:
		    flags = (TERM_TO_BINARY_DFLAGS
                             & ~(DFLAG_NEW_FLOATS | DFLAG_UTF8_ATOMS));
		    break;
		case 1: /* Current default... */
		    flags = TERM_TO_BINARY_DFLAGS & ~DFLAG_UTF8_ATOMS;
                    break;
                case 2:
                    flags = TERM_TO_BINARY_DFLAGS;
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

    if (deterministic && local) {
        return 0; /* badarg */
    }

    if (local) {
        flags |= DFLAG_LOCAL_EXT;
    }

    if (deterministic) {
        flags |= DFLAG_DETERMINISTIC;
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
    Uint64 flags;
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
    Uint64 flags;
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
    Uint64 flags;
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
    Uint32 terms;
    int atom_extra_skip;
    const byte* ep;
    ErtsBlockHashState lext_state;
    const byte *lext_hash;
    Uint32 lext_term_end;
} B2TSizeContext;

typedef struct {
    const byte* ep;
    Eterm  res;
    Eterm* next;
    ErtsHeapFactory factory;
    int remaining_n;
    int internal_nc;
    char* remaining_bytes;
    ErtsPStack map_array;
} B2TDecodeContext;

typedef struct {
    z_stream stream;
    byte* dbytes;
    Uint dleft;
} B2TUncompressContext;

typedef struct B2TContext_t {
    Sint heap_size;
    const byte *aligned_alloc;
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

static uLongf binary2term_uncomp_size(const byte* data, Sint size)
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
binary2term_prepare(ErtsBinary2TermState *state,
                    const byte *data, Sint data_size,
                    B2TContext** ctxp, Process* p)
{
    const byte *bytes = data;
    Sint size = data_size;

    state->exttmp = 0;

    if (size < 1 || *bytes != VERSION_MAGIC) {
	return -1;
    }
    bytes++;
    size--;
    if (size < 5 || *bytes != COMPRESSED) {
	state->extp = (byte*)bytes;
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
	if (context->u.dc.map_array.pstart) {
	    erts_free(context->u.dc.map_array.alloc_type,
		      context->u.dc.map_array.pstart);
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
            const byte *bytes;
            Uint bin_size;

            bytes = erts_get_aligned_binary_bytes_extra(bin,
                                                        &bin_size,
                                                        &ctx->aligned_alloc,
                                                        ERTS_ALC_T_EXT_TERM_DATA,
                                                        0);
            if (bytes == NULL) {
                ctx->b2ts.exttmp = 0;
                ctx->state = B2TBadArg;
                break;
            }
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
            ctx->u.dc.internal_nc = 0;
	    erts_factory_proc_prealloc_init(&ctx->u.dc.factory, p, ctx->heap_size);
	    ctx->u.dc.map_array.pstart = NULL;
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
    BIF_P->fvalue = am_badopt;
    BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
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
    int level;
    Uint64 flags;

    if (!parse_t2b_opts(BIF_ARG_2, &flags, &level, NULL, NULL)) {
        BIF_ERROR(BIF_P, BADARG);
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
        bin = erts_new_binary(p, real_size+1, &out_bytes);
        out_bytes[0] = VERSION_MAGIC;
	if (erl_zlib_compress2(out_bytes+6, &dest_len, bytes, real_size, level) != Z_OK) {
	    sys_memcpy(out_bytes+1, bytes, real_size);
	    bin = erts_shrink_binary_term(bin, real_size+1);
	} else {
	    out_bytes[1] = COMPRESSED;
	    put_int32(real_size, out_bytes+2);
	    bin = erts_shrink_binary_term(bin, dest_len+6);
	}
	if (bytes != buf) {
	    erts_free(ERTS_ALC_T_TMP, bytes);
	}
	return bin;
    } else {
        byte* bytes;

        bin = erts_new_binary(p, size, &bytes);
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
	return erts_shrink_binary_term(bin, real_size);
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
            /* Set to NULL if ever made alive! */
            if (context->s.ec.result_bin != NULL) {
                ASSERT(erts_refc_read(&(context->s.ec.result_bin->intern.refc),
                        1) == 1);
                erts_bin_free(context->s.ec.result_bin);
                context->s.ec.result_bin = NULL;
            }
            if (context->s.ec.map_array)
                erts_free(ERTS_ALC_T_T2B_DETERMINISTIC, context->s.ec.map_array);
            if (context->s.ec.ycf_yield_state)
                erts_qsort_ycf_gen_destroy(context->s.ec.ycf_yield_state);
            if (context->s.ec.iov)
                erts_free(ERTS_ALC_T_T2B_VEC, context->s.ec.iov);
	    break;
	case TTBCompress:
	    erl_zlib_deflate_finish(&(context->s.cc.stream));

            /* Set to NULL if ever made alive! */
            if (context->s.cc.destination_bin != NULL) {
                ASSERT(
                    erts_refc_read(&(context->s.cc.destination_bin->intern.refc),
                                   1) == 1);
                erts_bin_free(context->s.cc.destination_bin);
                context->s.cc.destination_bin = NULL;
            }

            /* Set to NULL if ever made alive! */
            if (context->s.cc.result_bin != NULL) {
                ASSERT(
                    erts_refc_read(&(context->s.cc.result_bin->intern.refc),
                                   1) == 1);
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
                else if (size <= ERL_ONHEAP_BINARY_LIMIT) {
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
                    Eterm result, *hp, *hp_end;
                    int referenced_cbin;
                    BinRef *result_ref;
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
                        hp = HAlloc(p, ERL_REFC_BITS_SIZE + (iovec ? 2 : 0));

                        result = erts_wrap_refc_bitstring(&MSO(p).first,
                                                          &MSO(p).overhead,
                                                          &hp,
                                                          result_bin,
                                                          (byte*)result_bin->orig_bytes,
                                                          0,
                                                          NBITS(result_bin->orig_size));

                        if (iovec) {
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

                    /* Max per vec entry, cons + sub binary. */
                    hsz = (2 + ERL_SUB_BITS_SIZE);
                     /* Number of vecs */
                    hsz *= context->s.ec.vlen - 2*fragments;
                     /* BinRef for result bin. */
                    hsz += ERL_BIN_REF_SIZE;
                    hp = HAlloc(p, hsz);
                    hp_end = hp + hsz;
                    result = NIL;

                    /* Speculatively create a BinRef to hold any direct
                     * references into the result_bin.
                     *
                     * Note that we do not link it into the off-heap list
                     * until we know it will be used. */
                    ASSERT(erts_refc_read(&result_bin->intern.refc, 1) == 1);
                    result_ref = (BinRef*)hp;
                    result_ref->thing_word = HEADER_BIN_REF;
                    result_ref->val = result_bin;
                    hp += ERL_BIN_REF_SIZE;
                    referenced_cbin = 0;

                    for (ix = context->s.ec.vlen - 1; ix > 1; ix--) {
                        SysIOVec *iovp = &iov[ix];
                        Eterm segment;

                        if (iovp->iov_base == NULL) {
                            continue; /* empty slot for header */
                        }

                        ASSERT(IS_BINARY_SIZE_OK(iovp->iov_len));
                        segment = termv[ix];

                        if (is_value(segment)) {
                            ErlSubBits *from_sb;

                            from_sb = (ErlSubBits*)bitstring_val(segment);
                            ASSERT(from_sb->thing_word == HEADER_SUB_BITS);

                            /* If the term refers to the entire segment, we can
                             * use it as is. Otherwise we need to return a
                             * shrunken copy. */
                            if (NBITS(iovp->iov_len) != (from_sb->end -
                                                         from_sb->start)) {
                                ErlSubBits *to_sb = (ErlSubBits*)hp;

                                segment = make_bitstring(to_sb);
                                hp += ERL_SUB_BITS_SIZE;

                                *to_sb = *from_sb;
                                to_sb->end = to_sb->start + NBITS(iovp->iov_len);

                                ASSERT(to_sb->end < from_sb->end);
                            }
                        } else {
                            /* We don't have a term and need to create one now,
                             * note that we intentionally avoid using heap
                             * binaries since they will (most likely) need to
                             * be converted to off-heap form when the result is
                             * actually used.
                             *
                             * This wastes a bit of heap space for small
                             * binaries, but that trade-off seems to be worth
                             * it in most cases. */
                            ErlSubBits *sb = (ErlSubBits*)hp;
                            Uint iov_offset;

                            iovp->iov_base =
                                (void*)(((byte *)iovp->iov_base) + realloc_offset);
                            iov_offset =
                                (char*)iovp->iov_base - (char*)result_bin->orig_bytes;

                            ASSERT(IS_BINARY_SIZE_OK(iov_offset));

                            erl_sub_bits_init(sb,
                                              0,
                                              make_boxed((Eterm*)result_ref),
                                              result_bin->orig_bytes,
                                              NBITS(iov_offset),
                                              NBITS(iovp->iov_len));

                            segment = make_bitstring(sb);
                            hp += ERL_SUB_BITS_SIZE;

                            referenced_cbin = 1;
                        }

                        result = CONS(hp, segment, result);
                        hp += 2;
                    }

                    ASSERT(hp <= hp_end);
                    HRelease(p, hp_end, hp);
                    context->s.ec.iov = NULL;
                    erts_free(ERTS_ALC_T_T2B_VEC, iov);

                    if (referenced_cbin) {
                        result_ref->next = MSO(p).first;
                        MSO(p).first = (struct erl_off_heap_header*)result_ref;
                        OH_OVERHEAD(&MSO(p), result_bin->orig_size);

                        /* Ownership has been transferred to the result_ref, so
                         * we do not need to bump refc. */
                    } else {
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

                        hp = HAlloc(p, ERL_REFC_BITS_SIZE);
                        return erts_wrap_refc_bitstring(&MSO(p).first,
                                                        &MSO(p).overhead,
                                                        &hp,
                                                        result_bin,
                                                        (byte*)result_bin->orig_bytes,
                                                        0,
                                                        NBITS(result_bin->orig_size));
                    }
                default:
                /* Revert to uncompressed binary (still in context) on
                 * compression errors, and when compression grows the result.*/
                no_use_compressing:
                    {
                        Uint result_size;

                        ASSERT(IS_BINARY_SIZE_OK(context->s.cc.real_size));
                        result_size = NBITS(context->s.cc.real_size);

                        result_bin = context->s.cc.result_bin;
                        context->s.cc.result_bin = NULL;

                        ASSERT(erts_refc_read(&result_bin->intern.refc, 1));

                        erl_zlib_deflate_finish(&(context->s.cc.stream));
                        erts_bin_free(context->s.cc.destination_bin);
                        context->s.cc.destination_bin = NULL;
                        context->alive = 0;

                        if (context_b &&
                            erts_refc_read(&context_b->intern.refc,0) == 0) {
                            erts_bin_free(context_b);
                        }

                        BUMP_REDS(p,
                                  ((this_time * CONTEXT_REDS) /
                                   TERM_TO_BINARY_COMPRESS_CHUNK));

                        hp = HAlloc(p, ERL_REFC_BITS_SIZE);
                        return erts_wrap_refc_bitstring(&MSO(p).first,
                                                        &MSO(p).overhead,
                                                        &hp,
                                                        result_bin,
                                                        (byte*)result_bin->orig_bytes,
                                                        0,
                                                        result_size);
                    }
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
	    sys_memcpy((char *) ep, (char *) erts_atom_get_name(a), len);
	}
	else {
	    if (a->latin1_chars <= 255 && (dflags & DFLAG_SMALL_ATOM_TAGS)) {
		*ep++ = SMALL_ATOM_EXT;
		if (len == a->latin1_chars) {
		    sys_memcpy(ep+1, erts_atom_get_name(a), len);
		}
		else {
		    len = erts_utf8_to_latin1(ep+1, erts_atom_get_name(a), len);
		    ASSERT(len == a->latin1_chars);
		}
		put_int8(len, ep);
		ep++;
	    }
	    else {
		*ep++ = ATOM_EXT;
		if (len == a->latin1_chars) {
		    sys_memcpy(ep+2, erts_atom_get_name(a), len);
		}
		else {
		    len = erts_utf8_to_latin1(ep+2, erts_atom_get_name(a), len);
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
 * We use INTERNAL_LOCAL_SYSNAME to mark internal node for pid/port/refs
 * in the ETS compressed format and local format.
 *
 */
#define INTERNAL_LOCAL_SYSNAME NIL

static ERTS_INLINE byte *
enc_internal_pid(ErtsAtomCacheMap *acmp, Eterm pid, byte* ep, Uint64 dflags)
{
    Uint32 number, serial, creation;

    ASSERT(is_internal_pid(pid));

    *ep++ = NEW_PID_EXT;

    number = internal_pid_number(pid);
    serial = internal_pid_serial(pid);
    if (dflags & (DFLAG_ETS_COMPRESSED|DFLAG_LOCAL_EXT)) {
        *ep++ = NIL_EXT; /* INTERNAL_LOCAL_NODE */
        creation = 0;
    }
    else {
        Eterm sysname = internal_pid_node_name(pid);
        creation = internal_pid_creation(pid);
        ep = enc_atom(acmp, sysname, ep, dflags);
    }

    put_int32(number, ep);
    ep += 4;
    put_int32(serial, ep);
    ep += 4;
    put_int32(creation, ep);
    ep += 4;
    return ep;

}

static ERTS_INLINE byte *
enc_external_pid(ErtsAtomCacheMap *acmp, Eterm pid, byte* ep, Uint64 dflags)
{
    Uint32 number, serial, creation;
    Eterm sysname;

    ASSERT(is_external_pid(pid));

    *ep++ = NEW_PID_EXT;

    ASSERT(is_external_pid(pid));
    number = external_pid_number(pid);
    serial = external_pid_serial(pid);
    sysname = external_pid_node_name(pid);
    creation = external_pid_creation(pid);
    ep = enc_atom(acmp, sysname, ep, dflags);

    put_int32(number, ep);
    ep += 4;
    put_int32(serial, ep);
    ep += 4;
    put_int32(creation, ep);
    ep += 4;
    return ep;
}

static byte*
enc_pid(ErtsAtomCacheMap *acmp, Eterm pid, byte* ep, Uint64 dflags)
{
    if (is_internal_pid(pid))
        return enc_internal_pid(acmp, pid, ep, dflags);
    return enc_external_pid(acmp, pid, ep, dflags);
}

/* Expect an atom in plain text or cached */
static const byte*
dec_atom(ErtsDistExternal *edep, const byte* ep, Eterm* objp, int internal_nc)
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
    case NIL_EXT:
        if (!internal_nc) {
            goto error;
        }
        *objp = INTERNAL_LOCAL_SYSNAME;
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
            (sysname == erts_this_node->sysname &&
             creation == erts_this_node->creation));
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
        Eterm* objp, byte tag, int internal_nc)
{
    Eterm sysname;
    Uint data;
    Uint num;
    Uint ser;
    Uint32 cre;

    *objp = NIL;		/* In case we fail, don't leave a hole in the heap */

    /* eat first atom */
    if ((ep = dec_atom(edep, ep, &sysname, internal_nc)) == NULL)
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
#define ENC_START_SORTING_MAP ((Eterm) 7)
#define ENC_CONTINUE_SORTING_MAP ((Eterm) 8)
#define ENC_PUSH_SORTED_MAP ((Eterm) 9)
#define ENC_LAST_ARRAY_ELEMENT ((Eterm) 10) /* must be the largest one */

static Eterm* alloc_map_array(Uint size)
{
    return (Eterm *) erts_alloc(ERTS_ALC_T_T2B_DETERMINISTIC,
                                size * 2 * sizeof(Eterm));
}

static int map_key_compare(Eterm *a, Eterm *b)
{
    Sint c = CMP_TERM(*a, *b);
    if (c < 0) {
        return -1;
    } else if (c > 0) {
        return 1;
    } else {
        return 0;
    }
}

static void*
ycf_yield_alloc(size_t size, void* context)
{
    (void) context;
    return (void *) erts_alloc(ERTS_ALC_T_T2B_DETERMINISTIC, size);
}

static void
ycf_yield_free(void* block, void* context)
{
    (void) context;
    erts_free(ERTS_ALC_T_T2B_DETERMINISTIC, block);
}

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
    register Sint r = 0;
    int use_iov = 0;
    byte *lext_hash = NULL; /* initialize to avoid faulty warning... */

    /* The following variables are only used during encoding of
     * a map when the `deterministic` option is active. */
    Eterm* map_array = NULL;
    Eterm* next_map_element = NULL;

    if (ctx) {
	WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	r = *reds;
        use_iov = !!ctx->iov;

	if (ctx->wstack.wstart) { /* restore saved stacks and byte pointer */
	    WSTACK_RESTORE(s, &ctx->wstack);
	    ep = ctx->ep;
	    obj = ctx->obj;
            map_array = ctx->map_array;
            next_map_element = ctx->next_map_element;
            lext_hash = ctx->lext_hash;
	    if (is_non_value(obj)) {
		goto outer_loop;
	    }
            else {
                goto L_jump_start;
            }
	}
        if (ctx->continue_make_lext_hash) {
            lext_hash = ctx->lext_hash;
	    ep = ctx->ep;
            if (use_iov) {
                goto continue_make_lext_hash_iov;
            }
            else {
                goto continue_make_lext_hash_bin;
            }
        }
    }

    /* We only pass here once at the start of the encoding... */
    if (dflags & DFLAG_LOCAL_EXT) {
        *ep++ = LOCAL_EXT;
        lext_hash = ep;
        if (ctx)
            ctx->lext_hash = ep;
        ep += 4; /* 32-bit hash */
        if (use_iov) {
            ASSERT(ctx);
            store_in_vec(ctx, ep, NULL, THE_NON_VALUE, NULL, 0);
            /* current vlen is now where to start calculating the hash */
            ctx->lext_vlen = ctx->vlen;
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
                Sint32 fun_sz;

                if (use_iov && !ErtsInArea(size_p, ctx->cptr, ep - ctx->cptr)) {
                    ASSERT(ctx->vlen > 0);
                    fun_sz = (ep - ctx->cptr)
                        + calc_iovec_fun_size(ctx->iov, ctx->vlen-1, size_p);
                }
                else {
                    /* No iovec encoding or still in same iovec buffer as start
                     * of fun. Easy to calculate fun size. */
                    fun_sz = ep - size_p;
                }
                put_int32(fun_sz, size_p);
	    }
	    goto outer_loop;
	case ENC_BIN_COPY: {
            Uint size = (Uint)obj;
            Uint src_offset = WSTACK_POP(s);
            Uint dst_offset = WSTACK_POP(s);
            byte* src = (byte*) WSTACK_POP(s);
            byte* dst = (byte*) WSTACK_POP(s);
            if (size > (r * TERM_TO_BINARY_MEMCPY_FACTOR)) {
                Uint n = r * TERM_TO_BINARY_MEMCPY_FACTOR;
                copy_binary_to_buffer(dst, dst_offset, src, src_offset, n);
                src_offset += n;
                dst_offset += n;
                size -= n;
                WSTACK_PUSH6(s, (UWord)dst, (UWord)src, dst_offset, src_offset,
                             ENC_BIN_COPY, size);
                obj = THE_NON_VALUE;
                r = 0; /* yield */
                break;
            } else {
                copy_binary_to_buffer(dst, dst_offset, src, src_offset, size);
                r -= size / (TERM_TO_BINARY_MEMCPY_FACTOR);
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
                if (dflags & DFLAG_DETERMINISTIC) {
                    *next_map_element++ = CAR(ptr);
                    *next_map_element++ = CDR(ptr);
                    goto outer_loop;
                }
		WSTACK_PUSH2(s, ENC_TERM, CDR(ptr));
		obj = CAR(ptr);
	    }
            else if (is_tuple(obj)) { /* collision node */
                Uint tpl_sz;
                ptr = tuple_val(obj);
                tpl_sz = arityval(*ptr);
                ASSERT(tpl_sz >= 2);
                ptr++;
                WSTACK_RESERVE(s, tpl_sz * 2);
                while(tpl_sz--) {
                    ASSERT(is_list(*ptr));
                    WSTACK_FAST_PUSH(s, ENC_HASHMAP_NODE);
                    WSTACK_FAST_PUSH(s, *ptr++);
                }
                goto outer_loop;
            }
            else
                ASSERT((*boxed_val(obj) & _HEADER_MAP_SUBTAG_MASK)
                       == HAMT_SUBTAG_NODE_BITMAP);
            break;
	case ENC_START_SORTING_MAP: /* option `deterministic` */
            {
                long num_reductions = r;

                n = next_map_element - map_array;
                if (ctx == NULL) {
                    /* No context means that the external representation of term
                     * being encoded will fit in a heap binary (64 bytes). This
                     * can only happen in the DEBUG build of the runtime system
                     * where maps with more than 3 elements are large maps. */
                    ASSERT(n < 64); /* Conservative assertion. */
                    qsort(map_array, n/2, 2*sizeof(Eterm),
                          (int (*)(const void *, const void *)) map_key_compare);
                    WSTACK_PUSH2(s, ENC_PUSH_SORTED_MAP, THE_NON_VALUE);
                    goto outer_loop;
                } else {
                    /* Use yieldable qsort since the number of elements
                     * in the map could be huge. */
                    num_reductions = r;
                    ctx->ycf_yield_state = NULL;
                    erts_qsort_ycf_gen_yielding(&num_reductions,
                                                &ctx->ycf_yield_state,
                                                NULL,
                                                ycf_yield_alloc,
                                                ycf_yield_free,
                                                NULL,
                                                0,
                                                NULL,
                                                map_array, n/2, 2*sizeof(Eterm),
                                                (int (*)(const void *, const void *)) map_key_compare);
                    if (ctx->ycf_yield_state) {
                        r = 0;
                        WSTACK_PUSH2(s, ENC_CONTINUE_SORTING_MAP, THE_NON_VALUE);
                        break;
                    } else {
                        WSTACK_PUSH2(s, ENC_PUSH_SORTED_MAP, THE_NON_VALUE);
                        r = num_reductions;
                        goto outer_loop;
                    }
                }
            }
        case ENC_CONTINUE_SORTING_MAP: /* option `deterministic` */
            {
                long num_reductions = r;

                erts_qsort_ycf_gen_continue(&num_reductions,
                                            &ctx->ycf_yield_state,
                                            NULL);
                if (ctx->ycf_yield_state) {
                    r = 0;
                    WSTACK_PUSH2(s, ENC_CONTINUE_SORTING_MAP, THE_NON_VALUE);
                    break;
                } else {
                    WSTACK_PUSH2(s, ENC_PUSH_SORTED_MAP, THE_NON_VALUE);
                    r = num_reductions;
                    goto outer_loop;
                }
            }
        case ENC_PUSH_SORTED_MAP: /* option `deterministic` */
            {
                n = next_map_element - map_array;
                WSTACK_RESERVE(s, 2*n);
                ptr = next_map_element - 1;
                do {
                    WSTACK_FAST_PUSH(s, ENC_TERM);
                    WSTACK_FAST_PUSH(s, *ptr);
                    ptr--;
                } while (ptr > map_array);
                obj = *ptr;
                erts_free(ERTS_ALC_T_T2B_DETERMINISTIC, map_array);
                map_array = next_map_element = NULL;
                break;
            }
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
            ctx->map_array = map_array;
            ctx->next_map_element = next_map_element;
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

	case EXTERNAL_PID_DEF:
	    ep = enc_external_pid(acmp, obj, ep, dflags);
	    break;

	case PID_DEF:
	    ep = enc_internal_pid(acmp, obj, ep, dflags);
	    break;

	case EXTERNAL_REF_DEF: {
            Eterm sysname;
	    Uint32 *ref_num;
            Uint32 creation;

            *ep++ = NEWER_REFERENCE_EXT;

            ref_num = external_ref_numbers(obj);
            i = external_ref_no_numbers(obj);
            put_int16(i, ep);
            ep += 2;

            sysname = external_ref_node_name(obj);
            creation = external_ref_creation(obj);
            goto ref_common;

        case REF_DEF:
            if ((dflags & DFLAG_ETS_COMPRESSED) && is_internal_magic_ref(obj)) {
                ErtsMRefThing tmp;
                ErtsMRefThing *mrtp = (ErtsMRefThing *) internal_ref_val(obj);

                erts_refc_inc(&mrtp->mb->intern.refc, 2);

                *ep++ = MAGIC_REF_INTERNAL_REF;
                sys_memcpy(&tmp, mrtp, sizeof(ErtsMRefThing));
                tmp.next = *off_heap;
                sys_memcpy(ep, &tmp, sizeof(ErtsMRefThing));
                *off_heap = (struct erl_off_heap_header*) ep;
                ep += sizeof(ErtsMRefThing);
                break;
            }

            *ep++ = NEWER_REFERENCE_EXT;

            erts_magic_ref_save_bin(obj);

            ref_num = internal_ref_numbers(obj);
            i = internal_ref_no_numbers(obj);
            put_int16(i, ep);
            ep += 2;

            if (dflags & (DFLAG_ETS_COMPRESSED|DFLAG_LOCAL_EXT)) {
                *ep++ = NIL_EXT; /* INTERNAL_LOCAL_NODE */
                creation = 0;
            }
            else {
                sysname = internal_ref_node_name(obj);
                creation = internal_ref_creation(obj);
            ref_common:
                ep = enc_atom(acmp, sysname, ep, dflags);
            }

            put_int32(creation, ep);
            ep += 4;
	    for (j = 0; j < i; j++) {
		put_int32(ref_num[j], ep);
		ep += 4;
	    }
	    break;
	}
	case EXTERNAL_PORT_DEF: {
            Eterm sysname;
            Uint64 number;
            Uint32 creation;

            *ep++ = V4_PORT_EXT;
            number = external_port_number(obj);
            sysname = external_port_node_name(obj);
            creation = external_port_creation(obj);

            goto port_common;

        case PORT_DEF:

            *ep++ = V4_PORT_EXT;
            number = internal_port_number(obj);
            if (dflags & (DFLAG_ETS_COMPRESSED|DFLAG_LOCAL_EXT)) {
                *ep++ = NIL_EXT; /* INTERNAL_LOCAL_NODE */
                creation = 0;
            }
            else {
                sysname = internal_port_node_name(obj);
                creation = internal_port_creation(obj);
            port_common:
                ep = enc_atom(acmp, sysname, ep, dflags);
            }

            put_int64(number, ep);
            ep += 8;
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
                ASSERT(ENC_LAST_ARRAY_ELEMENT+i-1 >= ENC_LAST_ARRAY_ELEMENT);
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

                    if (dflags & DFLAG_DETERMINISTIC) {
                        ASSERT(map_array == NULL);
                        next_map_element = map_array = alloc_map_array(size);
                        while (size--) {
                            *next_map_element++ = *kptr++;
                            *next_map_element++ = *vptr++;
                        }
                        WSTACK_PUSH2(s, ENC_START_SORTING_MAP, THE_NON_VALUE);
                    }
                    else {
                        WSTACK_PUSH4(s, (UWord)kptr, (UWord)vptr,
                                     ENC_MAP_PAIR, size);
                    }
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
                    if (dflags & DFLAG_DETERMINISTIC) {
                        /* Option `deterministic`: Note that we
                         * process large maps in a breadth-first
                         * order, that is, we push all keys and values
                         * to the stack and deallocate the map array
                         * before encoding any of the keys and
                         * values. That means that when we find a
                         * large map in key or value of an outer map,
                         * the map array for the outer map has already
                         * been deallocated. */

                        ASSERT(map_array == NULL);
                        next_map_element = map_array = alloc_map_array(*ptr);
                        WSTACK_PUSH2(s, ENC_START_SORTING_MAP, THE_NON_VALUE);
                    }
		    node_sz = 16;
		    break;
		case HAMT_SUBTAG_HEAD_BITMAP:
		    *ep++ = MAP_EXT;
		    ptr++;
		    put_int32(*ptr, ep); ep += 4;
                    if (dflags & DFLAG_DETERMINISTIC) {
                        ASSERT(map_array == NULL);
                        next_map_element = map_array = alloc_map_array(*ptr);
                        WSTACK_PUSH2(s, ENC_START_SORTING_MAP, THE_NON_VALUE);
                    }
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

	case BITSTRING_DEF:
            {
                ERTS_DECLARE_DUMMY(Eterm br_flags);
                int encoding, copy_payload;
                Uint offset, size;
                Uint wire_size;
                BinRef *br;
                byte *base;

                ERTS_PIN_BITSTRING(obj, br_flags, br, base, offset, size);
                encoding = TAIL_BITS(size) ? BIT_BINARY_EXT : BINARY_EXT;
                wire_size = NBYTES(size);

                copy_payload =
                    (size <= ERL_ONHEAP_BITS_LIMIT) ||
                    (BIT_OFFSET(offset) != 0) ||
                    !use_iov;

                if ((dflags & DFLAG_ETS_COMPRESSED) && (br != NULL)) {
                    const Binary *refc_binary = br->val;

                    ASSERT(!use_iov);

                    /* Always use [BIT_]BINARY_INTERNAL_REF: this may lead to a
                     * larger result than copying the payload, but ensures that
                     * the decoded object is exactly the same as the encoded
                     * one, simplifying the decompression logic in ETS. */
                    if ((encoding == BINARY_EXT) &&
                        (base == (byte*)refc_binary->orig_bytes) &&
                        (size == refc_binary->orig_size * 8) &&
                        (offset == 0)) {
                        encoding = BINARY_INTERNAL_REF;
                        copy_payload = 0;
                    } else {
                        encoding = BITSTRING_INTERNAL_REF;
                        copy_payload = 0;
                    }
                }

                ASSERT((copy_payload || use_iov) ^
                       (encoding == BITSTRING_INTERNAL_REF ||
                        encoding == BINARY_INTERNAL_REF));

                *ep++ = encoding;
                switch (encoding) {
                case BITSTRING_INTERNAL_REF:
                    sys_memcpy(ep, boxed_val(obj), sizeof(ErlSubBits));
                    ep += sizeof(ErlSubBits);
                    /* Fall through! */
                case BINARY_INTERNAL_REF:
                    {
                        BinRef tmp_ref;

                        erts_refc_inc(&(br->val)->intern.refc, 2);

                        sys_memcpy(&tmp_ref, br, sizeof(BinRef));
                        /* NOTE: this is only used by db_cleanup_offheap_comp
                         * which handles potentially unaligned pointers. */
                        tmp_ref.next = *off_heap;
                        *off_heap = (struct erl_off_heap_header*)ep;
                        sys_memcpy(ep, &tmp_ref, sizeof(BinRef));

                        ep += sizeof(BinRef);
                    }
                    break;
                case BIT_BINARY_EXT:
                    put_int32(wire_size, ep);
                    ep[4] = TAIL_BITS(size);
                    ep += 4 + 1;

                    if (copy_payload) {
                        /* To avoid information leakage, we have to clear the
                         * unused bits at the end of the binary. The used bits
                         * will be copied in later. */
                        ep[wire_size - 1] = 0;
                    } else {
                        /* The bulk of the payload will be referenced directly
                         * in the resulting iov, but the trailing bits will be
                         * copied here since the iov can't address bits. */
                        Uint trailing_bits = TAIL_BITS(size);
                        ep[0] = 0;
                        copy_binary_to_buffer(ep, 0, base,
                                              offset + size - trailing_bits,
                                              trailing_bits);
                    }
                    break;
                case BINARY_EXT:
                    put_int32(wire_size, ep);
                    ep += 4;
                    break;
                }

                if (copy_payload) {
                    byte* data_dst = ep;
                    ep += wire_size;

                    if (ctx && wire_size > r * TERM_TO_BINARY_MEMCPY_FACTOR) {
                        WSTACK_PUSH6(s, (UWord)data_dst, (UWord)base,
                                     0, offset, ENC_BIN_COPY, size);
                    } else {
                        copy_binary_to_buffer(data_dst, 0, base, offset, size);
                    }
                } else if (use_iov) {
                    /* Reference the stored data directly, omitting the
                     * trailing bits which have been copied separately. */
                    ASSERT(br != NULL && BIT_OFFSET(offset) == 0);
                    base = &base[BYTE_OFFSET(offset)];
                    store_in_vec(ctx, ep, br->val, obj, base, BYTE_SIZE(size));
                    ep += (encoding == BIT_BINARY_EXT);
                }

                ASSERT(ctx == NULL || !use_iov ||
                       (ep - ctx->cptr) <= (ctx->result_bin)->orig_size);
            }
            break;
        case FUN_DEF:
            {
                ErlFunThing* funp = (ErlFunThing *) fun_val(obj);

                if (is_local_fun(funp)) {
                    const ErlFunEntry *fe = funp->entry.fun;
                    int ei;

                    *ep++ = NEW_FUN_EXT;
                    WSTACK_PUSH2(s, ENC_PATCH_FUN_SIZE,
                                (UWord) ep); /* Position for patching in size */
                    ep += 4;
                    *ep = fun_arity(funp);
                    ep += 1;
                    sys_memcpy(ep, fe->uniq, 16);
                    ep += 16;
                    put_int32(fe->index, ep);
                    ep += 4;
                    put_int32((Uint32)fun_num_free(funp), ep);
                    ep += 4;
                    ep = enc_atom(acmp, fe->module, ep, dflags);
                    ep = enc_term(acmp, make_small(fe->old_index), ep, dflags, off_heap);
                    ep = enc_term(acmp, make_small(fe->old_uniq), ep, dflags, off_heap);
                    ep = enc_pid(acmp, erts_init_process_id, ep, dflags);

                    for (ei = fun_num_free(funp)-1; ei >= 0; ei--) {
                        WSTACK_PUSH2(s, ENC_TERM, (UWord) funp->env[ei]);
                    }
                } else {
                    const Export *exp = funp->entry.exp;

                    *ep++ = EXPORT_EXT;
                    ep = enc_atom(acmp, exp->info.mfa.module, ep, dflags);
                    ep = enc_atom(acmp, exp->info.mfa.function, ep, dflags);
                    ep = enc_term(acmp, make_small(exp->info.mfa.arity),
                                 ep, dflags, off_heap);

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
    if (dflags & DFLAG_LOCAL_EXT) {
        int done;
        Uint32 hash;
        Uint chunk_len;

        if (use_iov) {
            ASSERT(ctx);
            erts_iov_block_hash_init(&ctx->lext_state.iov_block,
                                     &ctx->iov[ctx->lext_vlen],
                                     ctx->vlen - ctx->lext_vlen,
                                     local_node_hash);

        continue_make_lext_hash_iov:
            /* Do 128 bytes per reduction... */
            chunk_len = (Uint) r*128;

            done = erts_iov_block_hash(&hash, &chunk_len,
                                       &ctx->lext_state.iov_block);
        }
        else {
            ErtsBlockHashState lext_state_buf, *lext_state;
            byte *ep_start = lext_hash + 4 /* 32 bit hash value */;
            Sint len = ep - ep_start;

            ASSERT(len >= 0);

            lext_state = ctx ? &ctx->lext_state.block : &lext_state_buf;

            erts_block_hash_init(lext_state, ep_start, len, local_node_hash);

            if (!ctx) {
                /* Do it all at once... */
                chunk_len = ERTS_UINT_MAX;
            }
            else {
            continue_make_lext_hash_bin:
                lext_state = &ctx->lext_state.block;
                /* Do 128 bytes per reduction... */
                chunk_len = (Uint) r*128;
            }

            done = erts_block_hash(&hash, &chunk_len, lext_state);
        }

        if (!ctx) {
            ASSERT(done);
        }
        else {
            if (!done) {
                /* yield; more work calculating hash... */
                ctx->ep = ep;
                ctx->continue_make_lext_hash = !0;
                *reds = 0;
                return -1;
            }
            r -= chunk_len/128;
            *reds = r;
        }

        put_int32(hash, lext_hash);
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

#ifdef DEBUG
    if (!(bin->intern.flags & BIN_FLAG_MAGIC)) {
        ASSERT(((byte *) &bin->orig_bytes[0]) <= ptr);
        ASSERT(ptr + len <= ((byte *) &bin->orig_bytes[0]) + bin->orig_size);
    }
#endif

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
             Binary *refc_binary,
             Eterm binary,
             byte *data,
             Uint size)
{
    byte *cp;

    ASSERT((refc_binary == NULL) ^ is_value(binary));

    cp = ctx->cptr;
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


    if (refc_binary) {
        /* save off-heap binary... */
        store_in_vec_aux(ctx,
                         refc_binary,
                         binary,
                         data,
                         size);
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


struct dec_term_map
{
    Eterm* objp; /* hashmap: write result here, flatmap: NULL  */
    Uint size;   /* hashmap: nr of leafs, flatmap: unused */
    union {
        Eterm* leaf_array;  /* hashmap */
        flatmap_t* flatmap;
    } u;
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
#define PSTACK_TYPE struct dec_term_map
    PSTACK_DECLARE(map_array, 10);
    int n, internal_nc = ets_decode;
    ErtsAtomEncoding char_enc;
    register Eterm* hp;        /* Please don't take the address of hp */
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
        internal_nc = ctx->u.dc.internal_nc;

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
	PSTACK_CHANGE_ALLOCATOR(map_array, ERTS_ALC_T_SAVED_ESTACK);
	if (ctx->u.dc.map_array.pstart) {
	    PSTACK_RESTORE(map_array, &ctx->u.dc.map_array);
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

    continue_this_obj:

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
            if (n == 0) {
                *objp = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
                break;
            }
	    goto tuple_loop;
	case SMALL_TUPLE_EXT:
	    n = get_int8(ep);
	    ep++;
            if (n == 0) {
                *objp = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
                break;
            }
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
	{
	    Uint32 nu = get_uint32(ep);
	    ep += 4;
	    if (nu == 0) {
		next = objp;
		break;
	    }
	    *objp = make_list(hp);
            hp += 2 * (Uint) nu;
	    objp = hp - 2;
	    objp[0] = (Eterm) (objp+1);
	    objp[1] = (Eterm) next;
	    next = objp;
	    objp -= 2;
            nu--;
	    if (ctx) {
                if ((Uint) reds < nu) {
                    ASSERT(reds > 0);
		    ctx->state = B2TDecodeList;
		    ctx->u.dc.remaining_n = nu - reds;
		    nu = reds;
		}
		reds -= nu;
	    }
            while (nu > 0) {
		objp[0] = (Eterm) next;
		objp[1] = make_list(next);
		next = objp;
		objp -= 2;
                nu--;
	    }
	    break;
	}
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
	    ep = dec_pid(edep, factory, ep, objp, ep[-1], internal_nc);
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

		if ((ep = dec_atom(edep, ep, &sysname, internal_nc)) == NULL) {
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

		if ((ep = dec_atom(edep, ep, &sysname, internal_nc)) == NULL)
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

		if ((ep = dec_atom(edep, ep, &sysname, internal_nc)) == NULL)
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

		if ((ep = dec_atom(edep, ep, &sysname, internal_nc)) == NULL)
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
                        if (ref_words > ERTS_MAX_REF_NUMBERS)
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
                        Eterm pid = erts_pid_ref_lookup(ref_num, ref_words);
                        if (is_internal_pid(pid)) {
                            write_pid_ref_thing(hp, ref_num[0], ref_num[1],
                                                ref_num[2], pid);
                            hp += ERTS_PID_REF_THING_SIZE;
                        }
                        else if (is_non_value(pid)) {
                            goto error; /* invalid reference... */
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
        case BIT_BINARY_EXT:
        case BINARY_EXT:
            {
                Uint size_in_bits, nu;

                nu = get_uint32(ep);
                if (!IS_BINARY_SIZE_OK(nu)) {
                    goto error;
                }

                size_in_bits = NBITS(nu);

                if (ep[-1] == BIT_BINARY_EXT) {
                    Uint trailing_bits = ep[4];

                    if (((trailing_bits == 0) != (nu == 0)) ||
                        trailing_bits > 8) {
                        goto error;
                    }

                    size_in_bits -= 8 - trailing_bits;
                    ep++;
                }

                ep += 4;

                if (size_in_bits > ERL_ONHEAP_BITS_LIMIT &&
                    edep &&
                    edep->data &&
                    (edep->data)->binp &&
                    (nu > ((edep->data)->binp)->orig_size / 4)) {
                    /* If we decode a refc binary from a distribution data
                     * entry we know that it is a refc binary to begin with so
                     * we just increment it and use the reference. This means
                     * that the entire distribution data entry will remain
                     * until this binary is de-allocated, so we'll only do it
                     * when a substantial part (> 25%) of the data is a
                     * binary. */
                    Binary *refc_binary = (edep->data)->binp;
                    byte *data;

                    erts_refc_inc(&refc_binary->intern.refc, 1);

                    data = (byte*)refc_binary->orig_bytes;
                    ERTS_ASSERT(ErtsInArea(ep, data, refc_binary->orig_size));

                    factory->hp = hp;
                    *objp =
                        erts_wrap_refc_bitstring(&(factory->off_heap)->first,
                                                 &(factory->off_heap)->overhead,
                                                 &factory->hp,
                                                 refc_binary,
                                                 data,
                                                 NBITS(ep - data),
                                                 size_in_bits);
                    hp = factory->hp;
                } else {
                    byte *data;

                    factory->hp = hp;
                    *objp = erts_hfact_new_bitstring(factory,
                                                     0,
                                                     size_in_bits,
                                                     &data);
                    hp = factory->hp;

                    if (ctx) {
                        unsigned int n_limit = reds * B2T_MEMCPY_FACTOR;

                        if (nu > n_limit) {
                            ctx->state = B2TDecodeBinary;
                            ctx->u.dc.remaining_n = nu - n_limit;
                            ctx->u.dc.remaining_bytes = (char*)&data[n_limit];
                            nu = n_limit;
                            reds = 0;
                        } else {
                            reds -= nu / B2T_MEMCPY_FACTOR;
                        }
                    }

                    sys_memcpy(data, ep, nu);
                }

                ep += nu;
                break;
            }
        case EXPORT_EXT:
            {
                Export *export;
                Eterm mod;
                Eterm name;
                Eterm temp;
                Sint arity;

                if ((ep = dec_atom(edep, ep, &mod, 0)) == NULL) {
                    goto error;
                }
                if ((ep = dec_atom(edep, ep, &name, 0)) == NULL) {
                    goto error;
                }
                factory->hp = hp;
                ep = dec_term(edep, factory, ep, &temp, NULL, 0);
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
                    if (!erts_active_export_entry(mod, name, arity)) {
                        goto error;
                    }
                }

                export = erts_export_get_or_make_stub(mod, name, arity);
                *objp = export->lambda;
            }
            break;
	case MAP_EXT:
	    {
		Uint32 size,n;
		Eterm *kptr,*vptr;
		Eterm keys;
                struct dec_term_map* map = PSTACK_PUSH(map_array);

		size = get_int32(ep); ep += 4;

                if (size <= MAP_SMALL_MAP_LIMIT) {
                    flatmap_t *mp;
                    if (size == 0) {
                        keys = ERTS_GLOBAL_LIT_EMPTY_TUPLE;
                    } else {
                        keys  = make_tuple(hp);
                        *hp++ = make_arityval(size);
                        hp   += size;
                    }
                    kptr = hp - 1;

                    mp    = (flatmap_t*)hp;
                    hp   += MAP_HEADER_FLATMAP_SZ;
                    hp   += size;
                    vptr = hp - 1;

                    /* kptr, last word for keys
                     * vptr, last word for values
                     */

                    map->objp = NULL;
                    map->u.flatmap = mp;

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
                    ASSERT(objp != NULL);
                    map->objp = objp;
                    map->size = size;
                    map->u.leaf_array = hp;

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
		ErlFunThing *funp;
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

                funp = (ErlFunThing*)hp;
                funp->thing_word = MAKE_FUN_HEADER(arity, num_free, 0);
                *objp = make_fun(funp);

                hp += ERL_FUN_SIZE + num_free;

		/* Module */
		if ((ep = dec_atom(edep, ep, &module, 0)) == NULL) {
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

                /* Creator pid, discarded */
                if ((ep = dec_term(edep, factory, ep, &temp, NULL,
                                   internal_nc)) == NULL) {
                    goto error;
                }
                if (!is_pid(temp)) {
                    goto error;
                }

                funp->entry.fun = erts_fun_entry_get_or_make_stub(module,
                                                                  old_uniq,
                                                                  old_index,
                                                                  uniq,
                                                                  index,
                                                                  arity);
                hp = factory->hp;

		/* Environment */
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) next;
		    next = funp->env + i;
		}
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

        case BITSTRING_INTERNAL_REF:
        case BINARY_INTERNAL_REF:
            {
                const byte tag = ep[-1];
                ErlSubBits *sb;
                BinRef *br;

                sb = (ErlSubBits*)hp;
                hp += ERL_SUB_BITS_SIZE;
                br = (BinRef*)hp;
                hp += ERL_BIN_REF_SIZE;

                if (tag == BITSTRING_INTERNAL_REF) {
                    ASSERT(br == (BinRef*)&sb[1]);

                    sys_memcpy(sb, ep, sizeof(ErlSubBits) + sizeof(BinRef));
                    ep += sizeof(ErlSubBits) + sizeof(BinRef);

                    sb->orig = make_boxed((Eterm*)br);
                } else {
                    /* The encoded bitstring can be described entirely from the
                     * wrapped Binary* object, so we've skipped encoding an
                     * ErlSubBits here. We still need to create one however.*/
                    sys_memcpy(br, ep, sizeof(BinRef));
                    ep += sizeof(BinRef);

                    erl_sub_bits_init(sb,
                                      0,
                                      make_boxed((Eterm*)br),
                                      &(br->val)->orig_bytes[0],
                                      0,
                                      NBITS((br->val)->orig_size));
                }

                erts_refc_inc(&(br->val)->intern.refc, 1);

                br->next = (factory->off_heap)->first;
                (factory->off_heap)->first = (struct erl_off_heap_header*)br;
                ERTS_BR_OVERHEAD(factory->off_heap, br);
                *objp = make_bitstring(sb);
                break;
            }
        case MAGIC_REF_INTERNAL_REF:
            {
                ErtsMRefThing* mrtp = (ErtsMRefThing*) hp;
                sys_memcpy(mrtp, ep, sizeof(ErtsMRefThing));
                ep += sizeof(ErtsMRefThing);
                erts_refc_inc(&mrtp->mb->intern.refc, 2);
                hp += ERTS_MAGIC_REF_THING_SIZE;
                mrtp->next = factory->off_heap->first;
                factory->off_heap->first = (struct erl_off_heap_header*)mrtp;
                *objp = make_internal_ref(mrtp);
                ASSERT(is_internal_magic_ref(*objp));
                break;
            }

        case LOCAL_EXT:
            internal_nc = !0;
            if (ctx)
                ctx->u.dc.internal_nc = !0;
            ep += 4; /* 32-bit hash (verified in decoded_size()) */
            goto continue_this_obj;

	default:
	    goto error;
	}

        if (--reds <= 0) {
            if (ctx) {
                if (next || ctx->state != B2TDecode) {
                    ctx->u.dc.ep = ep;
                    ctx->u.dc.next = next;
                    ctx->u.dc.factory.hp = hp;
		    if (!PSTACK_IS_EMPTY(map_array)) {
			PSTACK_SAVE(map_array, &ctx->u.dc.map_array);
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

#ifdef DEBUG
    if (factory->mode == FACTORY_CLOSED) {
        erts_literal_area_t purge_area;
        INITIALIZE_LITERAL_PURGE_AREA(purge_area);

        /* When we've got a dummy factory we should only be able to produce
         * global literals and immediates. */
        ASSERT(size_object_litopt(*dbg_resultp, &purge_area) == 0);
    } else {
        ASSERT(hp <= factory->hp_end);
    }
#endif

    factory->hp = hp;
    /*
     * From here on factory may produce (more) heap fragments
     * and we don't use local variable 'hp' anymore.
     */

    /*
     * Iterate through all the maps and for
     *   + hashmaps: hash keys and generate all inner hamt nodes
     *   + flatmaps: check for duplicate keys and sort keys if needed
     *
     * We do this at the end because the size of the preallocated heap is only
     * guaranteed to include everything except hamt nodes. If unlucky with hash
     * collisions the factory may need to create new heap fragments.
     *
     * As maps may include each other as keys, it's important the iteration
     * below is done bottom-up. Sub maps are completed before potential
     * container maps.
     */
    if (!PSTACK_IS_EMPTY(map_array)) {
        do {
            struct dec_term_map* map = PSTACK_TOP(map_array);

            if (map->objp) {
                *map->objp = erts_hashmap_from_array(factory,
                                                     map->u.leaf_array,
                                                     map->size,
                                                     1);
                if (is_non_value(*map->objp))
                    goto error_map_fixup;
            }
            else {
                if (!erts_validate_and_sort_flatmap(map->u.flatmap))
                    goto error_map_fixup;
            }

            (void) PSTACK_POP(map_array);
        } while (!PSTACK_IS_EMPTY(map_array));
    }

    /* Now that no more errors can occur, the stack can be destroyed safely. */
    PSTACK_DESTROY(map_array);

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

error_map_fixup:
    erts_factory_undo(factory);
    PSTACK_DESTROY(map_array);
    if (ctx) {
	ctx->state = B2TDecodeFail;
	ctx->reds = reds;
    }
    return NULL;
}

static Uint
encode_atom_size(ErtsAtomCacheMap *acmp, Eterm atom, Uint64 dflags)
{
    ASSERT(is_atom(atom));
    if (dflags & DFLAG_ETS_COMPRESSED) {
        if (atom_val(atom) >= (1<<16)) {
            return (Uint) 1 + 3;
        }
        else {
            return (Uint) 1 + 2;
        }
    }
    else {
        Atom *a = atom_tab(atom_val(atom));
        int alen;
        Uint result;
        if ((dflags & DFLAG_UTF8_ATOMS) || a->latin1_chars < 0) {
            alen = a->len;
            result = (Uint) 1 + 1 + alen;
            if (alen > 255) {
                result++; /* ATOM_UTF8_EXT (not small) */
            }
        }
        else {
            alen = a->latin1_chars;
            result = (Uint) 1 + 1 + alen;
            if (alen > 255 || !(dflags & DFLAG_SMALL_ATOM_TAGS))
                result++; /* ATOM_EXT (not small) */
        }
        insert_acache_map(acmp, atom, dflags);
        return result;
    }
}

static Uint
encode_internal_pid_size(ErtsAtomCacheMap *acmp, Eterm pid, Uint64 dflags)
{
    int nlen;
    ASSERT(is_internal_pid(pid));
    nlen = ((dflags & (DFLAG_ETS_COMPRESSED|DFLAG_LOCAL_EXT))
            ? 1
            : encode_atom_size(acmp, internal_pid_node_name(pid), dflags));
    return (Uint) 1 + nlen + 4 + 4 + 4;
}

static Uint
encode_external_pid_size(ErtsAtomCacheMap *acmp, Eterm pid, Uint64 dflags)
{
    int nlen;
    ASSERT(is_external_pid(pid));
    nlen = encode_atom_size(acmp, external_pid_node_name(pid), dflags);
    return (Uint) 1 + nlen + 4 + 4 + 4;
}

static Uint
encode_pid_size(ErtsAtomCacheMap *acmp, Eterm pid, Uint64 dflags)
{
    if (is_internal_pid(pid))
        return encode_internal_pid_size(acmp, pid, dflags);
    ASSERT(is_external_pid(pid));
    return encode_external_pid_size(acmp, pid, dflags);
}

static Uint
encode_small_size(ErtsAtomCacheMap *acmp, Eterm pid, Uint64 dflags)
{
    Sint val = signed_val(pid);
    Uint result;

    if ((Uint)val < 256)
        result = (Uint) 1 + 1;		/* SMALL_INTEGER_EXT */
    else if (sizeof(Sint) == 4 || IS_SSMALL32(val))
        result = (Uint) 1 + 4;		/* INTEGER_EXT */
    else {
        int i;
        DeclareTmpHeapNoproc(tmp_big,2);
        UseTmpHeapNoproc(2);
        i = big_bytes(small_to_big(val, tmp_big));
        result = (Uint) 1 + 1 + 1 + i;	/* SMALL_BIG_EXT */
        UnUseTmpHeapNoproc(2);
    }
    return result;
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

    if (!ctx) {
        if (dflags & DFLAG_LOCAL_EXT)
            result += 5;
    }
    else {
	WSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	r = *reds;

        vlen = ctx->vlen;
        
	if (!ctx->wstack.wstart) {
            ctx->last_result = result;
            if (dflags & DFLAG_LOCAL_EXT) {
                result += 5;
                if (vlen >= 0)
                    vlen++;
            }
        }
        else { /* restore saved stack */
	    WSTACK_RESTORE(s, &ctx->wstack);
	    result = ctx->result;
	    obj = ctx->obj;
	}
    }

#define LIST_TAIL_OP ((0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER)
#define HASHMAP_NODE_OP ((1 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER)
#define TERM_ARRAY_OP(N) (((N) << _HEADER_ARITY_OFFS) | TAG_PRIMARY_HEADER)
#define TERM_ARRAY_OP_DEC(OP) ((OP) - (1 << _HEADER_ARITY_OFFS))

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
            result += encode_atom_size(acmp, obj, dflags);
	    break;
	case SMALL_DEF:
            result += encode_small_size(acmp, obj, dflags);
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
	    result += encode_external_pid_size(acmp, obj, dflags);
	    break;
	case PID_DEF:
            result += encode_internal_pid_size(acmp, obj, dflags);
	    break;
        case EXTERNAL_REF_DEF: {
            int nlen = encode_atom_size(acmp,
                                        external_ref_node_name(obj),
                                        dflags);
            i = external_ref_no_numbers(obj);
	    result += (1 + 2 + nlen + 4 + 4*i);
	    break;
        }
	case REF_DEF:
            if ((dflags & DFLAG_ETS_COMPRESSED) && is_internal_magic_ref(obj)) {
                result += 1 + sizeof(ErtsMRefThing);
            }
            else {
                int nlen;
                i = internal_ref_no_numbers(obj);
                if (dflags & (DFLAG_ETS_COMPRESSED|DFLAG_LOCAL_EXT)) {
                    nlen = 1;
                }
                else {
                    nlen = encode_atom_size(acmp,
                                            internal_ref_node_name(obj),
                                            dflags);
                }
                result += (1 + 2 + nlen + 4 + 4*i);
            }
            break;
        case EXTERNAL_PORT_DEF: {
            int nlen = encode_atom_size(acmp,
                                        external_port_node_name(obj),
                                        dflags);
	    result += (1 + nlen + 8 + 4);
	    break;
	}
        case PORT_DEF: {
            int nlen = ((dflags & (DFLAG_ETS_COMPRESSED|DFLAG_LOCAL_EXT))
                        ? 1
                        : encode_atom_size(acmp,
                                           internal_port_node_name(obj),
                                           dflags));
	    result += (1 + nlen + 8 + 4);
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
                WSTACK_RESERVE(s, node_sz * 2);
                while(node_sz--) {
                    Eterm node = *ptr++;

                    if (is_list(node) || is_tuple(node)) {
                        WSTACK_FAST_PUSH(s, (UWord)node);
                        WSTACK_FAST_PUSH(s, (UWord)HASHMAP_NODE_OP);
                    } else {
                        ASSERT(is_map(node));
                        WSTACK_FAST_PUSH(s, node);
                    }
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
	case BITSTRING_DEF: {
            ERTS_DECLARE_DUMMY(Eterm br_flags);
            int encoding, copy_payload;
            Uint offset, size;
            Uint wire_size;
            byte *base;
            BinRef *br;

            ERTS_PIN_BITSTRING(obj, br_flags, br, base, offset, size);
            encoding = TAIL_BITS(size) ? BIT_BINARY_EXT : BINARY_EXT;
            wire_size = NBYTES(size);

            if (wire_size >= ERTS_UINT32_MAX) {
                WSTACK_DESTROY(s);
                return ERTS_EXT_SZ_SYSTEM_LIMIT;
            }

            copy_payload =
                (size <= ERL_ONHEAP_BITS_LIMIT) ||
                (BIT_OFFSET(offset) != 0) ||
                (vlen < 0);

            if ((dflags & DFLAG_ETS_COMPRESSED) && (br != NULL)) {
                const Binary *refc_binary = br->val;

                ASSERT(vlen < 0);

                /* Always use [BIT_]BINARY_INTERNAL_REF: see matching comment
                 * in enc_term_int. */
                if ((encoding == BINARY_EXT) &&
                    (base == (byte*)refc_binary->orig_bytes) &&
                    (size == refc_binary->orig_size * 8) &&
                    (offset == 0)) {
                    encoding = BINARY_INTERNAL_REF;
                } else {
                    encoding = BITSTRING_INTERNAL_REF;
                }
            }

            switch (encoding) {
            case BITSTRING_INTERNAL_REF:
                result += sizeof(ErlSubBits);
                /* !! FALL THROUGH !! */
            case BINARY_INTERNAL_REF:
                result += 1 /* [BIT_]BINARY_INTERNAL_REF */
                          + sizeof(BinRef);
                break;
            case BIT_BINARY_EXT:
                result += 1; /* Trailing bit count. */
                /* !! FALL THROUGH !! */
            case BINARY_EXT:
                result += 1   /* [BIT_]BINARY_EXT */
                          + 4 /* Size in bytes */;

                if (copy_payload) {
                    result += wire_size;
                } else {
                    Uint csz;

                    /* The encode routine will set up an iovec pointing at the
                     * actual data at this offset.
                     *
                     * In case of bit binaries, we grab the offset prior to
                     * adding the trailing bits, as we want the iovec to
                     * include the data prior to those. */
                    csz = result - ctx->last_result;
                    ctx->last_result = result;

                    /* Trailing bits if any. */
                    result += (encoding == BIT_BINARY_EXT);

                    /* Data leading up to the binary, and the binary itself */
                    vlen += 2;

                    /* Potentially multiple elements leading up to binary */
                    vlen += csz / MAX_SYSIOVEC_IOVLEN;

                    /* Potentially multiple elements for binary.
                     *
                     * Note that we do not include the trailing bits in this
                     * calculation as those have already been counted above. */
                    size = BYTE_SIZE(size);
                    vlen += size / MAX_SYSIOVEC_IOVLEN;

                    ctx->extra_size += size;
                }
                break;
            }

            break;
        }
        case FUN_DEF:
            {
                ErlFunThing *funp = (ErlFunThing *) fun_val(obj);

                if (is_local_fun(funp)) {
                    const ErlFunEntry *fe = funp->entry.fun;

                    result += 1 /* tag */
                            + 4 /* length field (size of free variables) */
                            + 1 /* arity */
                            + 16 /* uniq */
                            + 4 /* index */
                            + 4; /* free variables */
                    result += encode_atom_size(acmp, fe->module, dflags);
                    result += encode_small_size(acmp, make_small(fe->old_index), dflags);
                    result += encode_small_size(acmp, make_small(fe->old_uniq), dflags);
                    result += encode_pid_size(acmp, erts_init_process_id, dflags);

                    if (fun_num_free(funp) > 1) {
                        WSTACK_PUSH2(s, (UWord) (funp->env + 1),
                                    (UWord) TERM_ARRAY_OP(fun_num_free(funp)-1));
                    }

                    if (fun_num_free(funp) != 0) {
                        obj = funp->env[0];
                        continue; /* big loop */
                    }
                } else {
                    const Export *ep = funp->entry.exp;

                    result += 1;
                    result += encode_atom_size(acmp, ep->info.mfa.module, dflags);
                    result += encode_atom_size(acmp, ep->info.mfa.function, dflags);
                    result += encode_small_size(acmp, make_small(ep->info.mfa.arity), dflags);
                }
                break;
        }

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
	    case HASHMAP_NODE_OP: {
                Eterm *cons;

                obj = (Eterm)WSTACK_POP(s);

                if (is_tuple(obj)) {
                    /* Collision node */
                    Eterm *node_terms;
                    Uint node_size;

                    node_terms = tuple_val(obj);
                    node_size = arityval(*node_terms);
                    ASSERT(node_size >= 2);

                    WSTACK_RESERVE(s, node_size * 2);
                    for (Uint i = 1; i < node_size; i++) {
                         ASSERT(is_list(node_terms[i]));
                         WSTACK_FAST_PUSH(s, (UWord)node_terms[i]);
                         WSTACK_FAST_PUSH(s, (UWord)HASHMAP_NODE_OP);
                    }

                    /* The last collision leaf must be handled below, or it
                     * will be wrongly treated as a normal cons cell by the
                     * main loop. */
                    obj = node_terms[node_size];
                    ASSERT(is_list(obj));
                }

                if (is_list(obj)) {
                    /* Leaf node */
                    cons = list_val(obj);

                    WSTACK_PUSH(s, CDR(cons));
                    obj = CAR(cons);
                }
                break;
            }
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
    int atom_extra_skip;
    Uint n;
    SWord reds;
    const byte *lext_hash;
    Uint32 lext_term_end;

    /* Keep track of the current number of sub terms remaining to be decoded.
     *
     * We limit the number of sub terms to 2^32-1, even on 64-bit
     * machines, because a term that has many sub-terms must be truly
     * ginormous and is proably a mistake.
     *
     * This means that a map with 2^31 or more elements cannot be decoded,
     * even on a 64-bit machine.
     */
    Uint32 terms;

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

/* Increment the number of terms that remain to decode
 * and check for the term counter wrapping around. */
#define ADDTERMS(n)				\
    do {					\
        Uint32 before = terms;		        \
	terms += (n);                           \
	if (terms < before) goto error;     	\
    } while (0)


    if (ctx) {
        reds = ctx->reds;
        if (ctx->u.sc.ep) {
            ep = ctx->u.sc.ep;
            atom_extra_skip = ctx->u.sc.atom_extra_skip;
            heap_size = ctx->u.sc.heap_size;
            lext_hash = ctx->u.sc.lext_hash;
            lext_term_end = ctx->u.sc.lext_term_end;
            terms = ctx->u.sc.terms;
            if (terms == lext_term_end) {
                ASSERT(lext_hash);
                goto continue_check_lext;
            }
            goto init_done;
        }
    }
    else
        ERTS_UNDEF(reds, 0);

    heap_size = 0;
    terms = 1;
    atom_extra_skip = 0;
    /*
     * lext_hash != NULL and local_term_end != ~0 when decoding local external
     * term format...
     */
    lext_hash = NULL;
    lext_term_end = ~0;

init_done:

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
            ADDTERMS(1);
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
            ADDTERMS(1);
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
                ADDTERMS(1);
		break;
	    }
	case REFERENCE_EXT:
	    /* In case it is an external ref */
	    heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
	    atom_extra_skip = 5;
	    terms++;
	    break;
	case NIL_EXT:
            if (atom_extra_skip) {
                /*
                 * atom_extra_skip != 0 should only appear due to local encoding,
                 * or compressed ets encoding, of a node name in internal
                 * pids/ports/refs. If not currently inside a local encoding,
                 * this is an error...
                 */
                if (!lext_hash && !internal_tags)
                    goto error;
                SKIP(atom_extra_skip);
                atom_extra_skip = 0;
            }
	    break;
	case LIST_EXT:
	    CHKSIZE(4);
	    n = get_uint32(ep);
	    ep += 4;
            CHKSIZE(n); /* Fail faster if the binary is too short. */
            /* Count terms in two operations to avoid overflow. */
            ADDTERMS(n);
            ADDTERMS(1);
	    heap_size += 2 * n;
	    break;
	case SMALL_TUPLE_EXT:
	    CHKSIZE(1);
	    n = *ep++;
            ADDTERMS(n);
            /* When decoding the empty tuple we always use the canonical
             * global literal, so it won't occupy any heap space in the block
             * we're decoding to. */
            if (n > 0) {
                heap_size += n + 1;
            }
	    break;
	case LARGE_TUPLE_EXT:
	    CHKSIZE(4);
	    n = get_uint32(ep);
	    ep += 4;
            CHKSIZE(n); /* Fail faster if the binary is too short. */
	    ADDTERMS(n);
            /* See SMALL_TUPLE_EXT. */
            if (n > 0) {
                heap_size += n + 1;
            }
	    break;
	case MAP_EXT:
	    CHKSIZE(4);
	    n = get_uint32(ep);
	    ep += 4;
            if (n <= MAP_SMALL_MAP_LIMIT) {
                heap_size += 3 + n;

                /* When decoding the empty tuple we always use the canonical
                 * global literal, so it won't occupy any heap space in the
                 * block we're decoding to. */
                if (n > 0) {
                    heap_size += 1 + n;
                }
#if defined(ARCH_64)
            } else if ((n >> 31) != 0) {
                /* Avoid overflow by limiting the number of elements in
                 * a map to 2^31-1 (about 2 billions). */
                goto error;
#else
            } else if ((n >> 30) != 0) {
                /* Can't possibly fit in memory on 32-bit machine. */
                goto error;
#endif
            } else {
                CHKSIZE(2*(Uint)n); /* Fail faster if the binary is too short. */
                heap_size += HASHMAP_ESTIMATED_HEAP_SIZE(n);
            }
            /* Count terms in two operations to avoid overflow. */
            ADDTERMS(n);
            ADDTERMS(n);
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
#if defined(ARCH_32)
            if (!IS_BINARY_SIZE_OK(n))
                goto error;
#endif
            SKIP2(n, 4);
            if (n <= ERL_ONHEAP_BINARY_LIMIT) {
                heap_size += heap_bits_size(NBITS(n));
            } else {
                heap_size += ERL_REFC_BITS_SIZE;
            }
            break;
	case BIT_BINARY_EXT:
	    {
		CHKSIZE(5);
		n = get_uint32(ep);
#if defined(ARCH_32)
                if (!IS_BINARY_SIZE_OK(n))
                    goto error;
#endif
                SKIP2(n, 5);
                if (n <= ERL_ONHEAP_BINARY_LIMIT) {
                    heap_size += heap_bits_size(NBITS(n));
                } else {
                    heap_size += ERL_REFC_BITS_SIZE;
                }
	    }
	    break;
	case EXPORT_EXT:
            /* When decoding these we always use the canonical fun from the
             * export entry, so they won't occupy any heap space in the block
             * we're decoding to. */
            ADDTERMS(3);
	    break;
	case NEW_FUN_EXT:
	    {
		unsigned num_free;

		CHKSIZE(1+16+4+4);
                /* Ignore faulty Size field as we have bugs since OTP 23.0
                 * that can encode it too large if fun contains EXPORT_EXT or
                 * BIT_BINARY_EXT hopefully encoded for pending connection.
                 */
		ep += 1+16+4+4;
		CHKSIZE(4);
		num_free = get_uint32(ep);
		ep += 4;
		if (num_free > MAX_ARG) {
		    goto error;
		}
		ADDTERMS(4 + num_free);
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
        case BITSTRING_INTERNAL_REF:
            if (!internal_tags) {
                goto error;
            }
            SKIP(sizeof(ErlSubBits));
            /* !!! FALL THROUGH !!! */
        case BINARY_INTERNAL_REF:
            if (!internal_tags) {
                goto error;
            }
            SKIP(sizeof(BinRef));

            /* While BITSTRING_INTERNAL_REF and BINARY_INTERNAL_REF are
             * encoded differently, their on-heap size is the same since
             * BinRefs are not valid terms and must be wrapped by an
             * ErlSubBits. */
            heap_size += ERL_REFC_BITS_SIZE;
            break;
        case MAGIC_REF_INTERNAL_REF:
            if (!internal_tags)
                goto error;
            SKIP(sizeof(ErtsMRefThing));
            heap_size += ERTS_MAGIC_REF_THING_SIZE;
            break;
        case LOCAL_EXT:
            /*
             * Currently the hash is 4 bytes large...
             */
            CHKSIZE(4);
            lext_hash = ep;
            lext_term_end = terms - 1;
            terms++;
            ep += 4;
            break;
	default:
	    goto error;
	}
        terms--;

        if (terms == lext_term_end) {
            ErtsBlockHashState lext_state_buf, *lext_state;
            const byte *ep_start = lext_hash + 4 /* 32 bit hash value */;
            Sint len = ep - ep_start;
            Uint chunk_len;
            Uint32 hash;

            ASSERT(lext_hash);

            if (len <= 0) {
                /* no terms */
                goto error;
            }

            lext_state = ctx ? &ctx->u.sc.lext_state : &lext_state_buf;

            erts_block_hash_init(lext_state, ep_start, len, local_node_hash);

            if (!ctx) {
                /* Do it all at once... */
                chunk_len = ERTS_UINT_MAX;
            }
            else {
            continue_check_lext:
                lext_state = &ctx->u.sc.lext_state;
                /* 'reds' has been scaled up to "bytes per reds"... */
                chunk_len = (Uint) reds;
            }

            if (!erts_block_hash(&hash, &chunk_len, lext_state)) {
                /* yield; more work calculating hash... */
                ASSERT(ctx);
                reds = 0;
            }
            else {
                reds -= chunk_len;
                if (hash != get_int32(lext_hash)) {
                    /*
                     * Hash presented in external format did not match the
                     * calculated hash...
                     */
                    goto error;
                }
                lext_hash = NULL;
                lext_term_end = ~0;
            }

        }

        ASSERT(terms != ~0);

        if (ctx && --reds <= 0 && terms != 0) {
            ctx->u.sc.heap_size = heap_size;
            ctx->u.sc.terms = terms;
            ctx->u.sc.ep = ep;
            ctx->u.sc.atom_extra_skip = atom_extra_skip;
            ctx->u.sc.lext_hash = lext_hash;
            ctx->u.sc.lext_term_end = lext_term_end;
            ctx->reds = 0;
            return 0;
        }
    } while (terms != 0);

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

static Uint32
calc_iovec_fun_size(SysIOVec* iov, Uint32 fun_high_ix, byte* size_p)
{
    Sint32 ix;
    Uint32 fun_size = 0;

    ASSERT(size_p[-1] == NEW_FUN_EXT);

    /*
     * Search backwards for start of fun while adding up its total byte size.
     */
    for (ix = fun_high_ix; ix >= 0; ix--) {
        fun_size += iov[ix].iov_len;

        if (ErtsInArea(size_p, iov[ix].iov_base, iov[ix].iov_len)) {
            fun_size -= (size_p - (byte*)iov[ix].iov_base);
            break;
        }
    }
    ERTS_ASSERT(ix >= 0);
    return fun_size;
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
    int payload_moved_to_control = 0;

    if (reds < 0)
        return reds;

    /*
     * HOPEFUL_DATA header always present in io vector
     * element 1:
     *
     * +---+--------------+-----------+----------+
     * |'H'|Hopeful Flags|Hopeful IX|Payload IX|
     * +---+--------------+-----------+----------+
     *   1         8            4          4
     *
     * Hopeful flags:  Flags corresponding to actual
     *                 hopeful encodings in this
     *                 buffer.
     * Hopeful IX:     Vector index of first hopeful
     *                 encoding. Each hopeful encoding
     *                 is preceeded by 4 bytes containing
     *                 next vector index of hopeful
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

    /* Currently, the hopeful flags and IX are not used. */
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

    if ((~dflags & DFLAG_ALTACT_SIG)
        && ep[0] == SMALL_TUPLE_EXT
        && (ep[1] == 4 || ep[1] == 5)
        && ep[2] == SMALL_INTEGER_EXT
        && ep[3] == DOP_ALTACT_SIG_SEND) {
        byte *ptr;
        int dist_op;
        Uint32 flags;
        int tuple_size = ep[1];
        ptr = &ep[4];

        /*
         * The receiver does not understand alternate action signals...
         *
         * This control message has the following layouts, depending on
         * whether it contains a token or not:
         * - {DOP_ALTACT_SIG_SEND, Flags, SenderPid, To}
         * - {DOP_ALTACT_SIG_SEND, Flags, SenderPid, To, Token}
         * The control message is always followed by a payload term which
         * is the actual message to deliver.
         *
         * The Flags element is an integer which currently have these bit
         * flags defined in the least significant bits:
         * - ERTS_DOP_ALTACT_SIG_FLG_PRIO
         *   This is a priority message
         * - ERTS_DOP_ALTACT_SIG_FLG_TOKEN
         *   Control message also contains a token (control message is
         *   a 5-tuple instead of a 4-tuple).
         * - ERTS_DOP_ALTACT_SIG_FLG_ALIAS
         *   An alias message (To is a reference)
         * - ERTS_DOP_ALTACT_SIG_FLG_NAME
         *   Send to a registered name (To is an atom)
         * - ERTS_DOP_ALTACT_SIG_FLG_EXIT
         *   The signal is an exit signal; otherwise, a message signal
         *
         * If neither ERTS_DOP_ALTACT_SIG_FLG_ALIAS nor
         * ERTS_DOP_ALTACT_SIG_FLG_NAME is set, 'To' is a process
         * identifier.
         *
         * Currently the only alternate action messages supported are:
         * * Alias messages ('To' is a reference)
         * * Priority messages ('To' is a reference, atom, or pid)
         *
         * The receiving node does not support priority messages. By this we
         * know that the receiver process have not enabled reception of messages
         * as priority messages, so we can safely send the message as an
         * ordinary message.
         *
         * If we are sending an alias message, we need to check if it supports
         * DOP_ALIAS_SEND/DOP_ALIAS_SEND_TT control messages and send it as such
         * if supported; otherwise, we can safely drop the message since we know
         * that the receiver process will not have an alias registered.
         */

        /*
         * First read the flags field so we know what kind of altact message
         * this is.
         *
         * Currently we only have 4 flags defined, so we know that
         * the flags field has been encoded using SMALL_INTEGER_EXT.
         */
        switch (*(ptr++)) {
        case SMALL_INTEGER_EXT:
            flags = (Uint32) *ptr;
            break;
        default:
            ERTS_INTERNAL_ERROR(!"Invalid altact msg flags");
            break;
        }

        /* 'ptr' points to last byte before SenderPid... */

        /*
         * Determine what kind of control message we want to rewrite
         * this control message as...
         */
        if (flags & ERTS_DOP_ALTACT_SIG_FLG_EXIT) {
            dist_op = ((flags & ERTS_DOP_ALTACT_SIG_FLG_ALIAS)
                       ? 0 /* drop it... */
                       : ((dflags & DFLAG_EXIT_PAYLOAD)
                          ? ((flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)
                             ? DOP_PAYLOAD_EXIT2_TT
                             : DOP_PAYLOAD_EXIT2)
                          : ((flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)
                             ? DOP_EXIT2_TT
                             : DOP_EXIT2)));
        }
        else if (flags & ERTS_DOP_ALTACT_SIG_FLG_ALIAS) {
            dist_op = ((~dflags & DFLAG_ALIAS)
                       ? 0 /* drop it... */
                       : ((flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)
                          ? DOP_ALIAS_SEND_TT
                          : DOP_ALIAS_SEND));
        }
        else if (flags & ERTS_DOP_ALTACT_SIG_FLG_NAME) {
            dist_op = ((flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)
                       ? DOP_REG_SEND_TT
                       : DOP_REG_SEND);
        }
        else if (dflags & DFLAG_SEND_SENDER) {
            dist_op = ((flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)
                       ? DOP_SEND_SENDER_TT
                       : DOP_SEND_SENDER);
        }
        else {
            dist_op = ((flags & ERTS_DOP_ALTACT_SIG_FLG_TOKEN)
                       ? DOP_SEND_TT
                       : DOP_SEND);
        }

        switch (dist_op) {
        case DOP_ALIAS_SEND:
            /* {DOP_ALIAS_SEND, SenderPid, Alias} */
        case DOP_ALIAS_SEND_TT:
            /* {DOP_ALIAS_SEND_TT, SenderPid, Alias, Token} */
        case DOP_SEND_SENDER:
            /* {DOP_SEND_SENDER, SenderPid, ToPid} */
        case DOP_SEND_SENDER_TT:
            /* {DOP_SEND_SENDER_TT, SenderPid, ToPid, Token} */
        case DOP_PAYLOAD_EXIT2:
            /* {DOP_PAYLOAD_EXIT2, SenderPid, ToPid} */
        case DOP_PAYLOAD_EXIT2_TT:
            /* {DOP_PAYLOAD_EXIT2_TT, SenderPid, ToPid, Token} */

            /*
             * We want to drop the 'flags' element, add new 'dist_op',
             * and decrease tuple size by one.
             *
             * 'ptr' points the the byte before SenderPid, so we can
             * just write 'dist_op' and tuple info backwards from here.
             */

            tuple_size--;

        altact_sig_fallback_common:

            *(ptr--) = dist_op;
            *(ptr--) = SMALL_INTEGER_EXT;
            *(ptr--) = tuple_size;
            *ptr = SMALL_TUPLE_EXT;

            reds--;
            break;

        case DOP_EXIT2:
            /* {DOP_EXIT2, SenderPid, ToPid, Reason} */
        case DOP_EXIT2_TT:
            /* {DOP_EXIT2_TT, SenderPid, ToPid, Token, Reason} */

            /*
             * In the DOP_ALTACT_SIG_SEND signal, Reason follows *after* the
             * control message instead of *in* the control message as in the
             * DOP_EXIT2 and DOP_EXIT2_TT signals.
             *
             * We want to:
             * - leave the tuple size as it is, since we add a Reason element
             *   to the tuple but remove the Flags element from the tuple
             * - remove the encoding of the Flags element
             * - if the distribution header isn't used, we also want to
             *   prevent the magic number being placed before Reason
             *
             */

            /* Prevent magic number being placed before Reason */
            payload_moved_to_control = !0;

            /*
             * 'ptr' points the the byte before SenderPid and 'tuple_size'
             * equals the tuple size we want, so we can just write 'dist_op'
             * and tuple info backwards from here.
             */
            goto altact_sig_fallback_common;

        case DOP_REG_SEND:
            /* {DOP_REG_SEND, SenderPid, '', ToName} */
        case DOP_REG_SEND_TT:
            /* {DOP_REG_SEND_TT, SenderPid, '', ToName, TraceToken} */
        case DOP_SEND:
            /* {DOP_SEND, '', ToPid} */
        case DOP_SEND_TT:
            /* {DOP_SEND_TT, '', ToPid, TraceToken} */

            /*
             * These require a bit more rewriting...
             *
             * In the regsend case we want to insert an empty atom
             * between the SenderPid and the ToName, remove the
             * flags and change the 'dist_op'.
             *
             * In the send case we want to drop the flags field,
             * replace the SenderPid with an empty atom, change the
             * 'dist_op', and decrease the size of the tuple by one.
             */
        {
            byte *sender_start = ++ptr;
            int n;
            ASSERT(*sender_start == NEW_PID_EXT);
            ptr++;
            switch (*(ptr++)) {
            case ATOM_UTF8_EXT:
                n = (int) get_int16(ptr);
                ptr += 2;
                break;
            case SMALL_ATOM_UTF8_EXT:
                n = (int) *(ptr++);
                break;
            default:
                ERTS_INTERNAL_ERROR("Unexpected pid encoding");
                break;
            }

            ptr += n;

            ptr += 4 /* ID */ + 4 /* serial */ + 4 /* creation */;

            /* 'ptr' now points to the first byte in 'ToPid'/'ToName' */

            if (dist_op == DOP_SEND_TT || dist_op == DOP_SEND) {
                /* Write the empty atom */
                *(--ptr) = 0;
                *(--ptr) = SMALL_ATOM_UTF8_EXT;
                /* Tuple size one element smaller than for the altact msg... */
                tuple_size--;
                ptr--;
                /*
                 * 'ptr' now points to the last byte before the empty atom so we
                 * can continue writing 'dist_op' and tuple info backwards from
                 * here.
                 */
                reds--;
            }
            else {
                /*
                 * Move SenderId two bytes earlier in order to make room for
                 * the empty atom that we want to write between SenderId and
                 * ToName...
                 */
                size_t sender_sz = ptr - sender_start;
                byte *new_sender_start = sender_start - 2;
                ASSERT(dist_op == DOP_REG_SEND_TT || dist_op == DOP_REG_SEND);
                memmove((void *) new_sender_start,
                        (void *) sender_start,
                        sender_sz);
                /* Write the empty atom between SenderId and ToName... */
                *(--ptr) = 0;
                *(--ptr) = SMALL_ATOM_UTF8_EXT;
                /*
                 * We do not change tuple size since reg-send has the same tuple
                 * size as for the altact msg
                 */
                ptr = new_sender_start - 1;
                /*
                 * 'ptr' now points to the last byte before the SenderId so we
                 * can continue writing 'dist_op' and tuple info backwards from
                 * here.
                 */
                reds -= 2;
            }
            goto altact_sig_fallback_common;
        }

        case 0: {
            int i;
            /* Drop signal and send a tick instead... */
            for (i = 1; i < ob->eiov->vsize; i++) {
                if (ob->eiov->binv[i])
                    driver_free_binary(ob->eiov->binv[i]);
            }
            ob->eiov->vsize = 1;
            ob->eiov->size = 0;
            reds--;
            if (reds < 0)
                return 0;
            return reds;
        }

        default:
            ERTS_INTERNAL_ERROR("Unexpected fallback dist operation");
            break;
        }

        ASSERT(ptr >= ep);

        iov[2].iov_base = ptr;
        iov[2].iov_len -= (ptr - ep);
        eiov->size -= (ptr - ep);
    }

    start_r = r = reds*ERTS_TRANSCODE_REDS_FACT;

    /*
     * Replace hopeful data header with actual header...
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

        if (payload_ix && !payload_moved_to_control) {
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
