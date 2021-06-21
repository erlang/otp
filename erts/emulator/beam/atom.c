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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_sys_driver.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"
#include "atom.h"


#define ATOM_SIZE  3000

/*
 * Atomic operations on generic pointers.
 */

static ERTS_INLINE void *atomic_cmpxchg_ptr_mb(void **pptr, void *new, void *exp)
{
    return (void *) erts_atomic_cmpxchg_mb((erts_atomic_t *) pptr, (erts_aint_t) new, (erts_aint_t) exp);
}

/*
 * Atom entry.
 * Internal view, including hash/index table details.
 */

typedef struct atom_int {
    HashBucket bucket;		/* MUST BE LOCATED AT TOP OF STRUCT!!! */
    erts_atomic_t index;
    Atom atom;			/* this is what client code gets to see */
} AtomInt;

static ERTS_INLINE AtomInt **atomic_cmpxchg_atomintpp_mb(AtomInt ***ptr, AtomInt **new, AtomInt **exp)
{
    return (AtomInt **) atomic_cmpxchg_ptr_mb((void **) ptr, new, exp);
}

static ERTS_INLINE AtomInt *atomic_cmpxchg_atomintp_mb(AtomInt **ptr, AtomInt *new, AtomInt *exp)
{
    return (AtomInt *) atomic_cmpxchg_ptr_mb((void **) ptr, new, exp);
}

/*
 * Atom Index Table
 */

typedef struct atom_index_table
{
    Hash htable;		/* Mapping obj -> index */
    erts_atomic_t size;		/* Allocated size */
    int limit;			/* Max size */
    erts_atomic_t entries;	/* Number of entries */
    AtomInt ***seg_table;	/* Mapping index -> obj */
} AtomIndexTable;

#define ATOM_INDEX_PAGE_SHIFT 10
#define ATOM_INDEX_PAGE_SIZE (1 << ATOM_INDEX_PAGE_SHIFT)
#define ATOM_INDEX_PAGE_MASK ((1 << ATOM_INDEX_PAGE_SHIFT) - 1)

static AtomIndexTable erts_atom_table;	/* The index table */

static erts_rwmtx_t atom_table_lock;

#define atom_read_lock()	erts_rwmtx_rlock(&atom_table_lock)
#define atom_read_unlock()	erts_rwmtx_runlock(&atom_table_lock)
#define atom_write_lock()	erts_rwmtx_rwlock(&atom_table_lock)
#define atom_write_unlock()	erts_rwmtx_rwunlock(&atom_table_lock)

#if 0
#define ERTS_ATOM_PUT_OPS_STAT
#endif
#ifdef ERTS_ATOM_PUT_OPS_STAT
static erts_atomic_t atom_put_ops;
#endif

/* Functions for allocating space for the ext of atoms. We do not
 * use malloc for each atom to prevent excessive memory fragmentation
 */

typedef struct _atom_text {
    struct _atom_text* next;
    unsigned char text[ATOM_TEXT_SIZE];
} AtomText;

static AtomText* text_list;	/* List of text buffers */
static byte *atom_text_pos;
static byte *atom_text_end;
static Uint reserved_atom_space;	/* Total amount of atom text space */
static Uint atom_space;		/* Amount of atom text space used */

static void atom_index_info(fmtfn_t to, void *arg)
{
    AtomIndexTable *t = &erts_atom_table;
    hash_info(to, arg, &t->htable);
    erts_print(to, arg, "=index_table:%s\n", t->htable.name);
    erts_print(to, arg, "size: %d\n", (int) erts_atomic_read_mb(&t->size));
    erts_print(to, arg, "limit: %d\n", t->limit);
    erts_print(to, arg, "entries: %d\n", (int) erts_atomic_read_mb(&t->entries));
}

/*
 * Returns size of table in bytes. Stored objects not included.
 */
static int atom_index_table_sz(void)
{
    AtomIndexTable *t = &erts_atom_table;
    return (sizeof(AtomIndexTable)
	    - sizeof(Hash)
	    + (int) erts_atomic_read_mb(&t->size) * sizeof(AtomInt *)
	    + hash_table_sz(&t->htable));
}

/*
 * init a pre allocated or static hash structure
 * and allocate buckets.
 */
static void atom_index_init(HashFunctions fun)
{
    AtomIndexTable *t = &erts_atom_table;
    int size = ATOM_SIZE;
    int limit = erts_atom_table_size;
    Uint base_size = (((Uint) limit + ATOM_INDEX_PAGE_SIZE - 1) / ATOM_INDEX_PAGE_SIZE) * sizeof(AtomInt *);

    hash_init(ERTS_ALC_T_ATOM_TABLE, &t->htable, "atom_tab", 3*size/4, fun);

    erts_atomic_set_mb(&t->size, 0);
    t->limit = limit;
    erts_atomic_set_mb(&t->entries, 0);
    t->seg_table = (AtomInt ***) erts_alloc(ERTS_ALC_T_ATOM_TABLE, base_size);
    memzero(t->seg_table, base_size);
}

static int atom_index_put(AtomInt *tmpl)
{
    AtomIndexTable *t = &erts_atom_table;
    AtomInt *p;
    int ix;
    int entries;
    int size, size0;
    AtomInt **segment;

    p = (AtomInt *) hash_put(&t->htable, tmpl);
    ix = (int) erts_atomic_read_mb(&p->index);
    if (ix >= 0) {
	return ix;
    }

    for (;;) {
	size = (int) erts_atomic_read_mb(&t->size);
	do {
	    size0 = size;
	    entries = (int) erts_atomic_read_mb(&t->entries);
	    size = (int) erts_atomic_read_mb(&t->size);
	} while (size != size0);
	ix = entries;
	if (ix >= size) {
	    Uint sz;
	    if (ix >= t->limit) {
		/* A core dump is unnecessary */
		erts_exit(ERTS_DUMP_EXIT, "no more index entries in %s (max=%d)\n",
			  t->htable.name, t->limit);
	    }
	    sz = ATOM_INDEX_PAGE_SIZE * sizeof(AtomInt *);
	    segment = erts_alloc(ERTS_ALC_T_ATOM_TABLE, sz);
	    memzero(segment, sz);
	    if (atomic_cmpxchg_atomintpp_mb(&t->seg_table[ix >> ATOM_INDEX_PAGE_SHIFT], segment, NULL) != NULL) {
		/* someone concurrently expanded the seg_table, retry */
		erts_free(ERTS_ALC_T_ATOM_TABLE, segment);
		continue;
	    }
	    erts_atomic_add_mb(&t->size, ATOM_INDEX_PAGE_SIZE);
	} else {
	    segment = t->seg_table[ix >> ATOM_INDEX_PAGE_SHIFT];
	}
	if (atomic_cmpxchg_atomintp_mb(&segment[ix & ATOM_INDEX_PAGE_MASK], p, NULL) != NULL) {
	    /* someone concurrently used the index we wanted, retry */
	    continue;
	}
	break;
    }
    erts_atomic_set_mb(&p->index, ix);
    erts_atomic_inc_mb(&t->entries);
    return ix;
}

static int atom_index_get(AtomInt *tmpl)
{
    AtomIndexTable *t = &erts_atom_table;
    AtomInt *p = (AtomInt *) hash_get(&t->htable, tmpl);
    return p ? (int) erts_atomic_read_mb(&p->index) : -1;
}

static AtomInt *atom_index_lookup(Uint ix)
{
    AtomIndexTable *t = &erts_atom_table;
    return t->seg_table[ix >> ATOM_INDEX_PAGE_SHIFT][ix & ATOM_INDEX_PAGE_MASK];
}

/*
 * Print info about atom tables
 */
void atom_info(fmtfn_t to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    atom_index_info(to, to_arg);
#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_print(to, to_arg, "atom_put_ops: %ld\n",
	       erts_atomic_read_nob(&atom_put_ops));
#endif

    if (lock)
	atom_read_unlock();
}

/*
 * Allocate an atom text segment.
 */
static void
more_atom_space(void)
{
    AtomText* ptr;

    ptr = (AtomText*) erts_alloc(ERTS_ALC_T_ATOM_TXT, sizeof(AtomText));

    ptr->next = text_list;
    text_list = ptr;

    atom_text_pos = ptr->text;
    atom_text_end = atom_text_pos + ATOM_TEXT_SIZE;
    reserved_atom_space += sizeof(AtomText);

    VERBOSE(DEBUG_SYSTEM,("Allocated %d atom space\n",ATOM_TEXT_SIZE));
}

/*
 * Allocate string space within an atom text segment.
 */

static byte*
atom_text_alloc(int bytes)
{
    byte *res;

    ASSERT(bytes <= MAX_ATOM_SZ_LIMIT);
    if (atom_text_pos + bytes >= atom_text_end) {
	more_atom_space();
    }
    res = atom_text_pos;
    atom_text_pos += bytes;
    atom_space    += bytes;
    return res;
}

/*
 * Calculate atom hash value (using the hash algorithm
 * hashpjw from the Dragon Book).
 */

static HashValue
atom_hash(AtomInt* obj)
{
    byte* p = obj->atom.name;
    int len = obj->atom.len;
    HashValue h = 0, g;
    byte v;

    while(len--) {
	v = *p++;
	/* latin1 clutch for r16 */
	if (len && (v & 0xFE) == 0xC2 && (*p & 0xC0) == 0x80) {
	    v = (v << 6) | (*p & 0x3F);
	    p++; len--;
	}
	/* normal hashpjw follows for v */
	h = (h << 4) + v;
	if ((g = h & 0xf0000000)) {
	    h ^= (g >> 24);
	    h ^= g;
	}
    }
    return h;
}


static int 
atom_cmp(AtomInt* tmpl, AtomInt* obj)
{
    if (tmpl->atom.len == obj->atom.len &&
	sys_memcmp(tmpl->atom.name, obj->atom.name, tmpl->atom.len) == 0)
	return 0;
    return 1;
}


static AtomInt*
atom_alloc(AtomInt* tmpl)
{
    AtomInt* obj = (AtomInt*) erts_alloc(ERTS_ALC_T_ATOM, sizeof(AtomInt));

    obj->atom.name = atom_text_alloc(tmpl->atom.len);
    sys_memcpy(obj->atom.name, tmpl->atom.name, tmpl->atom.len);
    obj->atom.len = tmpl->atom.len;
    obj->atom.latin1_chars = tmpl->atom.latin1_chars;
    erts_atomic_set_mb(&obj->index, -1);

    /*
     * Precompute ordinal value of first 3 bytes + 7 bits.
     * This is used by erl_utils.h:erts_cmp_atoms().
     * We cannot use the full 32 bits of the first 4 bytes,
     * since we use the sign of the difference between two
     * ordinal values to represent their relative order.
     */
    {
	unsigned char c[4];
	int i;
	int j;

	j = (tmpl->atom.len < 4) ? tmpl->atom.len : 4;
	for(i = 0; i < j; ++i)
	    c[i] = tmpl->atom.name[i];
	for(; i < 4; ++i)
	    c[i] = '\0';
	obj->atom.ord0 = (c[0] << 23) + (c[1] << 15) + (c[2] << 7) + (c[3] >> 1);
    }
    return obj;
}

static void
atom_free(AtomInt* obj)
{
    ASSERT((int) erts_atomic_read_mb(&obj->index) == atom_val(am_ErtsSecretAtom));
}

static void latin1_to_utf8(byte* conv_buf, Uint buf_sz,
                           const byte** srcp, Uint* lenp)
{
    byte* dst;
    const byte* src = *srcp;
    Uint i, len = *lenp;

    ASSERT(len <= MAX_ATOM_CHARACTERS);
    ASSERT(buf_sz >= MAX_ATOM_SZ_FROM_LATIN1);

    for (i=0 ; i < len; ++i) {
	if (src[i] & 0x80) {
	    goto need_convertion;
	}
    }
    return;

need_convertion:
    sys_memcpy(conv_buf, src, i);
    dst = conv_buf + i;
    for ( ; i < len; ++i) {
	unsigned char chr = src[i];
	if (!(chr & 0x80)) {
	    *dst++ = chr;
	}
	else {
	    *dst++ = 0xC0 | (chr >> 6);
	    *dst++ = 0x80 | (chr & 0x3F);
	}
    }
    *srcp = conv_buf;	
    *lenp = dst - conv_buf;
}

/*
 * erts_atom_put_index() may fail. Returns negative indexes for errors.
 */
int
erts_atom_put_index(const byte *name, Sint len, ErtsAtomEncoding enc, int trunc)
{
    byte utf8_copy[MAX_ATOM_SZ_FROM_LATIN1];
    const byte *text = name;
    Uint tlen;
    Sint no_latin1_chars;
    AtomInt a;
    int aix;

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_atomic_inc_nob(&atom_put_ops);
#endif

    if (len < 0) {
        if (trunc) {
            len = 0;
        } else {
            return ATOM_MAX_CHARS_ERROR;
        }
    }

    tlen = len;

    switch (enc) {
    case ERTS_ATOM_ENC_7BIT_ASCII:
	if (tlen > MAX_ATOM_CHARACTERS) {
	    if (trunc)
		tlen = MAX_ATOM_CHARACTERS;
	    else
		return ATOM_MAX_CHARS_ERROR;
	}
#ifdef DEBUG
	for (aix = 0; aix < len; aix++) {
	    ASSERT((name[aix] & 0x80) == 0);
	}
#endif
	no_latin1_chars = tlen;
	break;
    case ERTS_ATOM_ENC_LATIN1:
	if (tlen > MAX_ATOM_CHARACTERS) {
	    if (trunc)
		tlen = MAX_ATOM_CHARACTERS;
	    else
		return ATOM_MAX_CHARS_ERROR;
	}
	no_latin1_chars = tlen;
	latin1_to_utf8(utf8_copy, sizeof(utf8_copy), &text, &tlen);
	break;
    case ERTS_ATOM_ENC_UTF8:
	/* First sanity check; need to verify later */
	if (tlen > MAX_ATOM_SZ_LIMIT && !trunc)
	    return ATOM_MAX_CHARS_ERROR;
	break;
    }

    a.atom.len = tlen;
    a.atom.name = (byte *) text;
    atom_read_lock();
    aix = atom_index_get(&a);
    atom_read_unlock();
    if (aix >= 0) {
	/* Already in table no need to verify it */
	return aix;
    }

    if (enc == ERTS_ATOM_ENC_UTF8) {
	/* Need to verify encoding and length */
	byte *err_pos;
	Uint no_chars;
	switch (erts_analyze_utf8_x((byte *) text,
				    (Uint) tlen,
				    &err_pos,
				    &no_chars, NULL,
				    &no_latin1_chars,
				    MAX_ATOM_CHARACTERS)) {
	case ERTS_UTF8_OK:
	    ASSERT(no_chars <= MAX_ATOM_CHARACTERS);
	    break;
	case ERTS_UTF8_OK_MAX_CHARS:
	    /* Truncated... */
	    if (!trunc)
		return ATOM_MAX_CHARS_ERROR;
	    ASSERT(no_chars == MAX_ATOM_CHARACTERS);
	    tlen = err_pos - text;
	    break;
	default:
	    /* Bad utf8... */
	    return ATOM_BAD_ENCODING_ERROR;
	}
    }

    ASSERT(tlen <= MAX_ATOM_SZ_LIMIT);
    ASSERT(-1 <= no_latin1_chars && no_latin1_chars <= MAX_ATOM_CHARACTERS);

    a.atom.len = tlen;
    a.atom.latin1_chars = (Sint16) no_latin1_chars;
    a.atom.name = (byte *) text;
    atom_write_lock();
    aix = atom_index_put(&a);
    atom_write_unlock();
    return aix;
}

/*
 * erts_atom_put() may fail. If it fails THE_NON_VALUE is returned!
 */
Eterm
erts_atom_put(const byte *name, Sint len, ErtsAtomEncoding enc, int trunc)
{
    int aix = erts_atom_put_index(name, len, enc, trunc);
    if (aix >= 0)
	return make_atom(aix);
    else
	return THE_NON_VALUE;
}

Eterm
am_atom_put(const char* name, Sint len)
{
    /* Assumes 7-bit ascii; use erts_atom_put() for other encodings... */
    return erts_atom_put((byte *) name, len, ERTS_ATOM_ENC_7BIT_ASCII, 1);
}

int atom_table_size(void)
{
    int ret;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    ret = (int) erts_atomic_read_mb(&erts_atom_table.entries);
    if (lock)
	atom_read_unlock();
    return ret;
}

int atom_table_sz(void)
{
    int ret;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    ret = atom_index_table_sz();
    if (lock)
	atom_read_unlock();
    return ret;
}

Eterm
erts_atom_get(const char *name, Uint len, ErtsAtomEncoding enc)
{
    byte utf8_copy[MAX_ATOM_SZ_FROM_LATIN1];
    AtomInt a;
    int i;

    switch (enc) {
    case ERTS_ATOM_ENC_LATIN1:
        if (len > MAX_ATOM_CHARACTERS) {
            return THE_NON_VALUE;
        }

        latin1_to_utf8(utf8_copy, sizeof(utf8_copy), (const byte**)&name, &len);

        a.atom.name = (byte*)name;
        a.atom.len = (Sint16)len;
        break;
    case ERTS_ATOM_ENC_7BIT_ASCII:
        if (len > MAX_ATOM_CHARACTERS) {
            return THE_NON_VALUE;
        }

        for (i = 0; i < len; i++) {
            if (name[i] & 0x80) {
                return THE_NON_VALUE;
            }
        }

        a.atom.len = (Sint16)len;
        a.atom.name = (byte*)name;
        break;
    case ERTS_ATOM_ENC_UTF8:
        if (len > MAX_ATOM_SZ_LIMIT) {
            return THE_NON_VALUE;
        }

        /* We don't need to check whether the encoding is legal as all atom
         * names are stored as UTF-8 and we know a lookup with a badly encoded
         * name will fail. */

        a.atom.len = (Sint16)len;
        a.atom.name = (byte*)name;
        break;
    }

    atom_read_lock();
    i = atom_index_get(&a);
    atom_read_unlock();

    return (i >= 0) ? make_atom(i) : THE_NON_VALUE;
}

void
erts_atom_get_text_space_sizes(Uint *reserved, Uint *used)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    if (reserved)
	*reserved = reserved_atom_space;
    if (used)
	*used = atom_space;
    if (lock)
	atom_read_unlock();
}

static AtomInt *atom_tab_int(Uint i)
{
    return atom_index_lookup(i);
}

void
init_atom_table(void)
{
    HashFunctions f;
    int i;
    AtomInt a;
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;

    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_atomic_init_nob(&atom_put_ops, 0);
#endif

    erts_rwmtx_init_opt(&atom_table_lock, &rwmtx_opt, "atom_tab", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    f.hash = (H_FUN) atom_hash;
    f.cmp  = (HCMP_FUN) atom_cmp;
    f.alloc = (HALLOC_FUN) atom_alloc;
    f.free = (HFREE_FUN) atom_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    atom_text_pos = NULL;
    atom_text_end = NULL;
    reserved_atom_space = 0;
    atom_space = 0;
    text_list = NULL;

    atom_index_init(f);
    more_atom_space();

    /* Ordinary atoms */
    for (i = 0; erl_atom_names[i] != 0; i++) {
	int ix;
	a.atom.len = sys_strlen(erl_atom_names[i]);
	a.atom.latin1_chars = a.atom.len;
	a.atom.name = (byte*)erl_atom_names[i];
	erts_atomic_set_mb(&a.index, i);
#ifdef DEBUG
	/* Verify 7-bit ascii */
	for (ix = 0; ix < a.atom.len; ix++) {
	    ASSERT((a.atom.name[ix] & 0x80) == 0);
	}
#endif
	ix = atom_index_put(&a);
	atom_text_pos -= a.atom.len;
	atom_space -= a.atom.len;
	atom_tab_int(ix)->atom.name = (byte*)erl_atom_names[i];
    }

    /* Hide am_ErtsSecretAtom */
    hash_erase(&erts_atom_table.htable, atom_tab_int(atom_val(am_ErtsSecretAtom)));
}

void
dump_atoms(fmtfn_t to, void *to_arg)
{
    int i = (int) erts_atomic_read_mb(&erts_atom_table.entries);

    /*
     * Print out the atom table starting from the end.
     */
    while (--i >= 0) {
	if (atom_index_lookup(i)) {
	    erts_print(to, to_arg, "%T\n", make_atom(i));
	}
    }
}

Uint
erts_get_atom_limit(void)
{
    return erts_atom_table.limit;
}

UWord atom_hvalue(Eterm atom)
{
    return atom_tab_int(atom_val(atom))->bucket.hvalue;
}

Atom *atom_tab(Uint i)
{
    return &atom_tab_int(i)->atom;
}
