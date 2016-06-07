/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

/* Since there are 255 different External tag values to choose from
   There is no reason to not be extravagant.
   Hence, the different tags for large/small tuple e.t.c
*/


#ifdef ERTS_WANT_EXTERNAL_TAGS
#ifndef ERTS_EXTERNAL_TAGS
#define ERTS_EXTERNAL_TAGS

#define SMALL_INTEGER_EXT 'a'
#define INTEGER_EXT       'b'
#define FLOAT_EXT         'c'
#define ATOM_EXT          'd'
#define SMALL_ATOM_EXT    's'
#define REFERENCE_EXT     'e'
#define NEW_REFERENCE_EXT 'r'
#define NEWER_REFERENCE_EXT 'Z'
#define PORT_EXT          'f'
#define NEW_PORT_EXT      'Y'
#define NEW_FLOAT_EXT     'F'
#define PID_EXT           'g'
#define NEW_PID_EXT       'X'
#define SMALL_TUPLE_EXT   'h'
#define LARGE_TUPLE_EXT   'i'
#define NIL_EXT           'j'
#define STRING_EXT        'k'
#define LIST_EXT          'l'
#define BINARY_EXT        'm'
#define BIT_BINARY_EXT    'M'
#define SMALL_BIG_EXT     'n'
#define LARGE_BIG_EXT     'o'
#define NEW_FUN_EXT       'p'
#define EXPORT_EXT        'q'
#define MAP_EXT           't'
#define FUN_EXT           'u'
#define ATOM_UTF8_EXT     'v'
#define SMALL_ATOM_UTF8_EXT 'w'

#define DIST_HEADER       'D'
#define ATOM_CACHE_REF    'R'
#define ATOM_INTERNAL_REF2 'I'
#define ATOM_INTERNAL_REF3 'K'
#define BINARY_INTERNAL_REF 'J'
#define BIT_BINARY_INTERNAL_REF 'L'
#define COMPRESSED        'P'

#if 0
/* Not used anymore */
#define CACHED_ATOM       'C'
#define NEW_CACHE         'N'
#endif


#define VERSION_MAGIC 131   /* 130 in erlang 4.2 */
                /* Increment this when changing the external format. */
                /* ON the other hand, don't change the external format */
                /* since that breaks other people's code! */

#endif /* ERTS_EXTERNAL_TAGS */
#endif /* ERTS_WANT_EXTERNAL_TAGS */

#ifndef ERL_EXTERNAL_H__
#define ERL_EXTERNAL_H__

#include "erl_node_tables.h"

#define ERTS_ATOM_CACHE_SIZE 2048

typedef struct cache {
    Eterm in_arr[ERTS_ATOM_CACHE_SIZE];
    Eterm out_arr[ERTS_ATOM_CACHE_SIZE];
} ErtsAtomCache;

typedef struct {
    int hdr_sz;
    int sz;
    int long_atoms;
    int cix[ERTS_ATOM_CACHE_SIZE];
    struct {
	Eterm atom;
	int iix;
    } cache[ERTS_ATOM_CACHE_SIZE];
} ErtsAtomCacheMap;

typedef struct {
    Uint32 size;
    Eterm atom[ERTS_ATOM_CACHE_SIZE];
} ErtsAtomTranslationTable;

/*
 * These flags are tagged onto the high bits of a connection ID and stored in
 * the ErtsDistExternal structure's flags field.  They are used to indicate
 * various bits of state necessary to decode binaries in a variety of
 * scenarios. The mask ERTS_DIST_EXT_CON_ID_MASK is used later to separate the
 * connection ID from the flags. Be careful to ensure that the mask does not
 * overlap any of the bits used for flags, or ERTS will leak flags bits into
 * connection IDs and leak connection ID bits into the flags.
 */
#define ERTS_DIST_EXT_DFLAG_HDR      ((Uint32) 0x80000000)
#define ERTS_DIST_EXT_ATOM_TRANS_TAB ((Uint32) 0x40000000)
#define ERTS_DIST_EXT_BTT_SAFE       ((Uint32) 0x20000000)
#define ERTS_DIST_EXT_CON_ID_MASK    ((Uint32) 0x1fffffff)

#define ERTS_DIST_EXT_CON_ID(DIST_EXTP) \
  ((DIST_EXTP)->flags & ERTS_DIST_EXT_CON_ID_MASK)
typedef struct {
    DistEntry *dep;
    byte *extp;
    byte *ext_endp;
    Sint heap_size;
    Uint32 flags;
    ErtsAtomTranslationTable attab;
} ErtsDistExternal;

typedef struct {
    int have_header;
    int cache_entries;
} ErtsDistHeaderPeek;

#define ERTS_DIST_EXT_SIZE(EDEP) \
  (sizeof(ErtsDistExternal) \
   - (((EDEP)->flags & ERTS_DIST_EXT_ATOM_TRANS_TAB) \
      ? (ASSERT(0 <= (EDEP)->attab.size \
		&& (EDEP)->attab.size <= ERTS_ATOM_CACHE_SIZE), \
	 sizeof(Eterm)*(ERTS_ATOM_CACHE_SIZE - (EDEP)->attab.size)) \
      : sizeof(ErtsAtomTranslationTable)))

typedef struct {
    byte *extp;
    int exttmp;
    Uint extsize;
    Uint heap_size;
} ErtsBinary2TermState;


/* -------------------------------------------------------------------------- */

void erts_init_atom_cache_map(ErtsAtomCacheMap *);
void erts_reset_atom_cache_map(ErtsAtomCacheMap *);
void erts_destroy_atom_cache_map(ErtsAtomCacheMap *);
void erts_finalize_atom_cache_map(ErtsAtomCacheMap *, Uint32);

Uint erts_encode_ext_dist_header_size(ErtsAtomCacheMap *);
byte *erts_encode_ext_dist_header_setup(byte *, ErtsAtomCacheMap *);
byte *erts_encode_ext_dist_header_finalize(byte *, ErtsAtomCache *, Uint32);
struct erts_dsig_send_context;
int erts_encode_dist_ext_size(Eterm, Uint32, ErtsAtomCacheMap*, Uint* szp);
int erts_encode_dist_ext_size_int(Eterm term, struct erts_dsig_send_context* ctx, Uint* szp);
struct TTBEncodeContext_;
int erts_encode_dist_ext(Eterm, byte **, Uint32, ErtsAtomCacheMap *,
			  struct TTBEncodeContext_ *, Sint* reds);

Uint erts_encode_ext_size(Eterm);
Uint erts_encode_ext_size_2(Eterm, unsigned);
Uint erts_encode_ext_size_ets(Eterm);
void erts_encode_ext(Eterm, byte **);
byte* erts_encode_ext_ets(Eterm, byte *, struct erl_off_heap_header** ext_off_heap);

#ifdef ERTS_WANT_EXTERNAL_TAGS
ERTS_GLB_INLINE void erts_peek_dist_header(ErtsDistHeaderPeek *, byte *, Uint);
#endif
ERTS_GLB_INLINE void erts_free_dist_ext_copy(ErtsDistExternal *);
ERTS_GLB_INLINE void *erts_dist_ext_trailer(ErtsDistExternal *);
ErtsDistExternal *erts_make_dist_ext_copy(ErtsDistExternal *, Uint);
void *erts_dist_ext_trailer(ErtsDistExternal *);
void erts_destroy_dist_ext_copy(ErtsDistExternal *);
int erts_prepare_dist_ext(ErtsDistExternal *, byte *, Uint,
			  DistEntry *, ErtsAtomCache *);
Sint erts_decode_dist_ext_size(ErtsDistExternal *);
Eterm erts_decode_dist_ext(ErtsHeapFactory* factory, ErtsDistExternal *);

Sint erts_decode_ext_size(byte*, Uint);
Sint erts_decode_ext_size_ets(byte*, Uint);
Eterm erts_decode_ext(ErtsHeapFactory*, byte**, Uint32 flags);
Eterm erts_decode_ext_ets(ErtsHeapFactory*, byte*);

Eterm erts_term_to_binary(Process* p, Eterm Term, int level, Uint flags);

Sint erts_binary2term_prepare(ErtsBinary2TermState *, byte *, Sint);
void erts_binary2term_abort(ErtsBinary2TermState *);
Eterm erts_binary2term_create(ErtsBinary2TermState *, ErtsHeapFactory*);
int erts_debug_max_atom_out_cache_index(void);
int erts_debug_atom_to_out_cache_index(Eterm);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF
#ifdef ERTS_WANT_EXTERNAL_TAGS
ERTS_GLB_INLINE void
erts_peek_dist_header(ErtsDistHeaderPeek *dhpp, byte *ext, Uint sz)
{
    if (ext[0] == VERSION_MAGIC
	|| ext[1] != DIST_HEADER
	|| sz < (1+1+1))
	dhpp->have_header = 0;
    else {
	dhpp->have_header = 1;
	dhpp->cache_entries = (int) get_int8(&ext[2]);
    }
}
#endif

ERTS_GLB_INLINE void
erts_free_dist_ext_copy(ErtsDistExternal *edep)
{
    if (edep->dep)
	erts_deref_dist_entry(edep->dep);
    erts_free(ERTS_ALC_T_EXT_TERM_DATA, edep);
}

ERTS_GLB_INLINE void *
erts_dist_ext_trailer(ErtsDistExternal *edep)
{
    void *res = (void *) (edep->ext_endp
			  + ERTS_EXTRA_DATA_ALIGN_SZ(edep->ext_endp));
    ASSERT((((UWord) res) % sizeof(Uint)) == 0);
    return res;
}

#endif

#endif /* ERL_EXTERNAL_H__ */
