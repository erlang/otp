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
#include "erl_vm.h"
#include "global.h"
#include "atom.h"


/*
 * Atomic operations on generic pointers.
 */

static ERTS_INLINE void *atomic_cmpxchg_ptr_mb(void **pptr, void *new, void *exp)
{
    return (void *) erts_atomic_cmpxchg_mb((erts_atomic_t *) pptr, (erts_aint_t) new, (erts_aint_t) exp);
}

static ERTS_INLINE void *atomic_read_ptr_mb(void **pptr)
{
    return (void *) erts_atomic_read_mb((erts_atomic_t *) pptr);
}

static ERTS_INLINE void atomic_set_ptr_mb(void **pptr, void *val)
{
    erts_atomic_set_mb((erts_atomic_t *) pptr, (erts_aint_t) val);
}

/*
 * Atom entry.
 * Internal view, including hash/index table details.
 */

typedef uint32_t sokey_t;	/* split-ordered list key, must have known width */

typedef struct atom_int {
    struct atom_int *next;
    sokey_t key;
    erts_atomic_t index;
    UWord hvalue;
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

static ERTS_INLINE AtomInt *atomic_read_atomintp_mb(AtomInt **ptr)
{
    return (AtomInt *) atomic_read_ptr_mb((void **) ptr);
}

/*
 * Lock-free operations on AtomInt lists sorted on key (transformed hvalue)
 */

static ERTS_INLINE byte *atomic_read_bytep_mb(byte **ptr)
{
    return (byte *) atomic_read_ptr_mb((void **) ptr);
}

static ERTS_INLINE void atomic_set_bytep_mb(byte **ptr, byte *val)
{
    atomic_set_ptr_mb((void **) ptr, (void *) val);
}

/* Find atom by key and name in list sorted on increasing key.
 * Returns atom if found, NULL if not found.
 */
static AtomInt *list_find(AtomInt *cur, AtomInt *tmpl)
{
    while (cur) {
	if (cur->key == tmpl->key &&
	    cur->atom.len == tmpl->atom.len &&
	    sys_memcmp(atomic_read_bytep_mb(&cur->atom.name), tmpl->atom.name, tmpl->atom.len) == 0) /* found */
	    return cur;
	if (cur->key > tmpl->key) /* key not in list */
	    break;
	/* cur->key <= tmpl->key, keep looking */
	cur = atomic_read_atomintp_mb(&cur->next);
    }
    return NULL;
}

/* Insert new atom in list sorted on increasing key.
 * If another atom with the same key already is present, returns
 * that other atom, otherwise NULL signalling successful insertion.
 */
static AtomInt *list_insert(AtomInt **head, AtomInt *new)
{
    AtomInt **prev;
    AtomInt *cur;
    sokey_t key;

    key = new->key;
    for (;;) {
	prev = head;
	for (;;) {
	    cur = atomic_read_atomintp_mb(prev);
	    if (!cur)			/* end of list, we insert here */
		break;
	    if (cur->key == key &&
		cur->atom.len == new->atom.len &&
		sys_memcmp(atomic_read_bytep_mb(&cur->atom.name), new->atom.name, new->atom.len) == 0) /* already there, return other */
		return cur;
	    if (cur->key > key) /* key not in list, we insert here */
		break;
	    /* cur->key <= key, keep looking */
	    prev = &cur->next;
	}
	new->next = cur;
	if (atomic_cmpxchg_atomintp_mb(prev, new, cur) == cur)
	    return NULL;
	/* someone concurrently updated *prev, retry */
    }
}

static ERTS_INLINE int key_is_regular(sokey_t key)
{
    return (key & 1) != 0; /* see so_regularkey() */
}

/* Compute the length of a bucket.
 * Stops upon finding a dummy node, which implies a different bucket.
 */
static int list_length(AtomInt *cur)
{
    int len;

    len = 0;
    while (cur && key_is_regular(cur->key)) {
	++len;
	cur = atomic_read_atomintp_mb(&cur->next);
    }
    return len;
}

/*
 * Lock-free Atom Hash Table using Split-Ordered Lists, based on:
 *
 * Ori Shalev and Nir Shavit, "Split-Ordered Lists: Lock-Free Extensible
 * Hash Tables", Journal of the ACM, Vol. 53, No. 3, May 2006, pp. 379--405.
 */

typedef struct {
    const char *name;		/* Table name (static string, for debugging) */
    erts_atomic_t size;		/* Number of buckets, always a power of 2 */
    erts_atomic_t nrobjs;	/* Number of objects in table */
    erts_atomic_t grow_limit;	/* Expand when nrobjs > grow_limit */
    AtomInt **seg_table;	/* Array of segments of dummy objects */
} AtomHashTable;

#define ATOM_HASH_PAGE_SHIFT	10
#define ATOM_HASH_PAGE_SIZE	(1 << ATOM_HASH_PAGE_SHIFT)
#define ATOM_HASH_PAGE_MASK	((1 << ATOM_HASH_PAGE_SHIFT) - 1)

/* reverse the bits in a 32-bit word */
static uint32_t reverse(uint32_t w)
{
    w = ((w & 0x0000FFFF) << 16) |  (w >> 16);
    w = ((w & 0x00FF00FF) << 8)  | ((w >> 8) & 0x00FF00FF);
    w = ((w & 0x0F0F0F0F) << 4)  | ((w >> 4) & 0x0F0F0F0F);
    w = ((w & 0x33333333) << 2)  | ((w >> 2) & 0x33333333);
    w = ((w & 0x55555555) << 1)  | ((w >> 1) & 0x55555555);
    return w;
}

static sokey_t so_dummykey(sokey_t key)
{
    return reverse(key);
}

static sokey_t so_regularkey(sokey_t key)
{
    return reverse(key | (1U << 31));
}

/* given a bucket b = p + (1 << i), where p < (1 << i) and i < 32, return p */
static uint32_t bucket_parent(uint32_t bucket)
{
    uint32_t mask;

    /* given bucket 0...01x...x
       compute mask 0...011...1 */
    mask = bucket;
    mask |= (mask >> 1);
    mask |= (mask >> 2);
    mask |= (mask >> 4);
    mask |= (mask >> 8);
    mask |= (mask >> 16);

    /* now clear highest set bit */
    return bucket & (mask >> 1);
}

/*
 * Every bucket neeeds a dummy element, so we make the segments
 * arrays of elements (AtomInt:s) not pointers to elements;
 * this saves one level of indirection when inspecting a bucket.
 *
 * To distinguish an uninitialized from an initialized dummy, we
 * place a marker in its index field to indicate its state.
 */

#define ATOM_HASH_INDEX_UNINITIALIZED	(-1)
#define ATOM_HASH_INDEX_INITIALIZED	(-2)

static ERTS_INLINE void atom_hash_mark_dummy_uninitialized(AtomInt *dummy)
{
    erts_atomic_set_mb(&dummy->index, ATOM_HASH_INDEX_UNINITIALIZED);
}

static ERTS_INLINE void atom_hash_mark_dummy_initialized(AtomInt *dummy)
{
    erts_atomic_set_mb(&dummy->index, ATOM_HASH_INDEX_INITIALIZED);
}

static ERTS_INLINE int atom_hash_dummy_is_uninitialized(AtomInt *dummy)
{
    return erts_atomic_read_mb(&dummy->index) == ATOM_HASH_INDEX_UNINITIALIZED;
}

static AtomInt *atom_hash_new_segment(void)
{
    Uint sz;
    AtomInt *segment;
    int i;

    sz = ATOM_HASH_PAGE_SIZE * sizeof(AtomInt);
    segment = erts_alloc(ERTS_ALC_T_ATOM_TABLE, sz);
    memzero(segment, sz);

    for (i = 0; i < ATOM_HASH_PAGE_SIZE; ++i)
	atom_hash_mark_dummy_uninitialized(&segment[i]);

    return segment;
}

static ERTS_INLINE Uint atom_hash_grow_limit(Uint size)
{
    /* This cannot overflow since size is bounded by the maximum number
       of atoms, which is (UINT_MAX >> 6) [_TAG_IMMED2_SIZE == 6]. */
    return (8 * size) / 5;	/* grow at 160% load */
}

static void atom_hash_init(AtomHashTable *t, const char *name)
{
    int limit = erts_atom_table_size;
    Uint sz;
    AtomInt *seg0;

    t->name = name;
    erts_atomic_set_mb(&t->size, 2);
    erts_atomic_set_mb(&t->nrobjs, 0);
    erts_atomic_set_mb(&t->grow_limit, UWORD_CONSTANT(1) << 1);

    sz = (((Uint) limit + ATOM_HASH_PAGE_SIZE - 1) / ATOM_HASH_PAGE_SIZE) * sizeof(AtomInt *);
    t->seg_table = (AtomInt **) erts_alloc(ERTS_ALC_T_ATOM_TABLE, sz);
    memzero(t->seg_table, sz);

    seg0 = atom_hash_new_segment();
    seg0[0].key = so_dummykey(0);
    atom_hash_mark_dummy_initialized(&seg0[0]);
    t->seg_table[0] = seg0;
}

static AtomInt *atom_hash_dummy_raw(AtomHashTable *t, sokey_t bucket)
{
    AtomInt *segment, *tmp;

    segment = atomic_read_atomintp_mb(&t->seg_table[bucket >> ATOM_HASH_PAGE_SHIFT]);
    if (!segment) {
	segment = atom_hash_new_segment();
	tmp = atomic_cmpxchg_atomintp_mb(&t->seg_table[bucket >> ATOM_HASH_PAGE_SHIFT], segment, NULL);
	if (tmp != NULL) {
	    erts_free(ERTS_ALC_T_ATOM_TABLE, segment);
	    segment = tmp;
	}
    }
    return &segment[bucket & ATOM_HASH_PAGE_MASK];
}

static void atom_hash_initialize_bucket(AtomHashTable *t, sokey_t bucket, AtomInt *dummy)
{
    sokey_t parent_bucket;
    AtomInt *parent_dummy;

    parent_bucket = bucket_parent(bucket);
    parent_dummy = atom_hash_dummy_raw(t, parent_bucket);
    if (atom_hash_dummy_is_uninitialized(parent_dummy))
	atom_hash_initialize_bucket(t, parent_bucket, parent_dummy);

    dummy->key = so_dummykey(bucket);
    (void) list_insert(&parent_dummy->next, dummy);
    atom_hash_mark_dummy_initialized(dummy);
}

static AtomInt *atom_hash_key_dummy(AtomHashTable *t, sokey_t key, Uint size)
{
    sokey_t bucket = key & (size - 1); /* size is always a power of two */
    AtomInt *dummy = atom_hash_dummy_raw(t, bucket);
    if (atom_hash_dummy_is_uninitialized(dummy))
	atom_hash_initialize_bucket(t, bucket, dummy);
    return dummy;
}

static AtomInt *atom_hash_find(AtomHashTable *t, AtomInt *tmpl)
{
    Uint hvalue;
    sokey_t key;
    Uint size;
    AtomInt *dummy;

    hvalue = tmpl->hvalue;
    size = erts_atomic_read_mb(&t->size);
    key = (sokey_t) hvalue & ((1U << 31) - 1);
    dummy = atom_hash_key_dummy(t, key, size);
    tmpl->key = so_regularkey(key);
    return list_find(atomic_read_atomintp_mb(&dummy->next), tmpl);
}

static byte *atom_text_alloc(int bytes); /* forward */

static AtomInt *atom_hash_insert(AtomHashTable *t, AtomInt *new)
{
    Uint hvalue;
    sokey_t key;
    Uint size;
    AtomInt *dummy;
    AtomInt *other;
    Uint nrobjs;
    Uint grow_limit;
    byte *name;

    hvalue = new->hvalue;
    key = (sokey_t) hvalue & ((1U << 31) - 1);
    size = erts_atomic_read_mb(&t->size);
    dummy = atom_hash_key_dummy(t, key, size);
    new->key = so_regularkey(key);

    other = list_insert(&dummy->next, new);
    if (other)
	return other;

    name = atom_text_alloc(new->atom.len);
    sys_memcpy(name, atomic_read_bytep_mb(&new->atom.name), new->atom.len);
    atomic_set_bytep_mb(&new->atom.name, name);

    nrobjs = erts_atomic_inc_read_mb(&t->nrobjs);
    /* grow at 160% load, as long as size does not exceed 1<<31 */
    grow_limit = erts_atomic_read_mb(&t->grow_limit);
    if (nrobjs > grow_limit && size < (1 << 31))
	if (erts_atomic_cmpxchg_mb(&t->size, 2 * size, size) == size)
	    (void) erts_atomic_cmpxchg_mb(&t->grow_limit, atom_hash_grow_limit(size * 2), grow_limit);

    return NULL;
}

static void atom_hash_info(fmtfn_t to, void *arg, AtomHashTable *t)
{
    const char *name;
    int size;
    int used;
    int nrobjs;
    int objects;
    int max_depth;
    int nrseg;
    int i;

    name = t->name;
    size = erts_atomic_read_mb(&t->size);

    max_depth = 0;
    objects = 0;
    used = 0;

    nrseg = (size + ATOM_HASH_PAGE_SIZE - 1) / ATOM_HASH_PAGE_SIZE;
    for (i = 0; i < nrseg; ++i) {
	AtomInt *segment;
	int j;

	segment = atomic_read_atomintp_mb(&t->seg_table[i]);
	if (!segment)
	    continue;
	for (j = 0; j < ATOM_HASH_PAGE_SIZE; ++j) {
	    AtomInt *dummy;
	    int depth;

	    dummy = &segment[j];
	    if (atom_hash_dummy_is_uninitialized(dummy))
		continue;
	    depth = list_length(atomic_read_atomintp_mb(&dummy->next));
	    if (depth) {
		objects += depth;
		++used;
		if (depth > max_depth)
		    max_depth = depth;
	    }
	}
    }

    nrobjs = erts_atomic_read_mb(&t->nrobjs);
    ASSERT(objects <= nrobjs);

    erts_print(to, arg, "=hash_table:%s\n", name);
    erts_print(to, arg, "size: %d\n",       size);
    erts_print(to, arg, "used: %d\n",       used);
    erts_print(to, arg, "objs: %d\n",       nrobjs);
    erts_print(to, arg, "depth: %d\n",      max_depth);
}

static int atom_hash_table_sz(AtomHashTable *t)
{
    unsigned int maxseg;
    unsigned int nrseg;
    unsigned int i;

    maxseg = ((Uint) erts_atom_table_size + ATOM_HASH_PAGE_SIZE - 1) / ATOM_HASH_PAGE_SIZE;

    nrseg = 0;
    for (i = 0; i < maxseg; ++i)
	if (t->seg_table[i] != NULL)
	    ++nrseg;

    return
	(sizeof(AtomHashTable) +
	 strlen(t->name) + 1 +
	 maxseg * sizeof(AtomInt *) +
	 nrseg * ATOM_HASH_PAGE_SIZE * sizeof(AtomInt));
}

/*
 * Atom Index Table
 */

typedef struct atom_index_table
{
    AtomHashTable htable;	/* Mapping obj -> index */
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
static Uint reserved_atom_space;	/* Total amount of atom text space */
static Uint atom_space;		/* Amount of atom text space used */

static void atom_index_info(fmtfn_t to, void *arg)
{
    AtomIndexTable *t = &erts_atom_table;
    atom_hash_info(to, arg, &t->htable);
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
    int hsz;

    hsz = atom_hash_table_sz(&t->htable);
    return (sizeof(AtomIndexTable)
	    - sizeof(Hash)
	    + (int) erts_atomic_read_mb(&t->size) * sizeof(AtomInt *)
	    + hsz);
}

/*
 * init a pre allocated or static hash structure
 * and allocate buckets.
 */
static void atom_index_init(void)
{
    AtomIndexTable *t = &erts_atom_table;
    int limit = erts_atom_table_size;
    Uint base_size = (((Uint) limit + ATOM_INDEX_PAGE_SIZE - 1) / ATOM_INDEX_PAGE_SIZE) * sizeof(AtomInt *);

    atom_hash_init(&t->htable, "atom_tab");

    erts_atomic_set_mb(&t->size, 0);
    t->limit = limit;
    erts_atomic_set_mb(&t->entries, 0);
    t->seg_table = (AtomInt ***) erts_alloc(ERTS_ALC_T_ATOM_TABLE, base_size);
    memzero(t->seg_table, base_size);
}

static int atom_index_put_raw(AtomInt *p)
{
    AtomIndexTable *t = &erts_atom_table;
    int ix;
    int entries;
    int size, size0;
    AtomInt **segment;

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

static AtomInt *atom_alloc(AtomInt *); /* forward */

static int atom_index_put(AtomInt *tmpl)
{
    AtomIndexTable *t = &erts_atom_table;
    AtomInt *p;
    int ix;

    tmpl = atom_alloc(tmpl);
    p = atom_hash_insert(&t->htable, tmpl);

    if (p)
	erts_free(ERTS_ALC_T_ATOM, tmpl);
    else
	p = tmpl;

    ix = (int) erts_atomic_read_mb(&p->index);
    if (ix >= 0)
	return ix;

    return atom_index_put_raw(p);
}

static int atom_index_get(AtomInt *tmpl)
{
    AtomIndexTable *t = &erts_atom_table;
    AtomInt *p;

    p = atom_hash_find(&t->htable, tmpl);
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
    atom_index_info(to, to_arg);
#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_print(to, to_arg, "atom_put_ops: %ld\n",
	       erts_atomic_read_nob(&atom_put_ops));
#endif
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
    byte *text_end;

    atom_write_lock();
    ASSERT(bytes <= MAX_ATOM_SZ_LIMIT);
    text_end = text_list->text + ATOM_TEXT_SIZE;
    if (bytes > text_end - atom_text_pos) {
	more_atom_space();
    }
    res = atom_text_pos;
    atom_text_pos += bytes;
    atom_space    += bytes;
    atom_write_unlock();
    return res;
}

/*
 * Calculate atom hash value (using the hash algorithm
 * hashpjw from the Dragon Book).
 *
 * This cannot be changed, as phash2/[12] depend on it.
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

static AtomInt *atom_alloc(AtomInt *tmpl)
{
    AtomInt *obj = (AtomInt *) erts_alloc(ERTS_ALC_T_ATOM, sizeof(AtomInt));

    obj->hvalue = tmpl->hvalue; /* precomputed */
    obj->atom.name = tmpl->atom.name; /* name is reallocated by atom_hash_insert() */
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
    a.hvalue = atom_hash(&a);
    aix = atom_index_get(&a);
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
	    a.atom.len = tlen;
	    a.hvalue = atom_hash(&a);
	    break;
	default:
	    /* Bad utf8... */
	    return ATOM_BAD_ENCODING_ERROR;
	}
    }

    ASSERT(tlen <= MAX_ATOM_SZ_LIMIT);
    ASSERT(-1 <= no_latin1_chars && no_latin1_chars <= MAX_ATOM_CHARACTERS);

    a.atom.latin1_chars = (Sint16) no_latin1_chars;
    aix = atom_index_put(&a);
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
    return (int) erts_atomic_read_mb(&erts_atom_table.entries);
}

int atom_table_sz(void)
{
    return atom_index_table_sz();
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

        break;
    case ERTS_ATOM_ENC_UTF8:
        if (len > MAX_ATOM_SZ_LIMIT) {
            return THE_NON_VALUE;
        }

        /* We don't need to check whether the encoding is legal as all atom
         * names are stored as UTF-8 and we know a lookup with a badly encoded
         * name will fail. */

        break;
    }

    a.atom.len = (Sint16) len;
    a.atom.name = (byte *) name;
    a.hvalue = atom_hash(&a);
    i = atom_index_get(&a);

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

    atom_text_pos = NULL;
    reserved_atom_space = 0;
    atom_space = 0;
    text_list = NULL;

    atom_index_init();
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
	/* am_ErtsSecretAtom should be entered in the index table but not the hash table */
	a.hvalue = atom_hash(&a);
	{
	    AtomInt *p = atom_alloc(&a);
	    ix = atom_index_put_raw(p);
	    if (ix != atom_val(am_ErtsSecretAtom)) {
		(void) atom_hash_insert(&erts_atom_table.htable, p);
		atom_text_pos -= a.atom.len;
		atom_space -= a.atom.len;
		p->atom.name = (byte *) erl_atom_names[i];
	    }
	}
    }
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
    return atom_tab_int(atom_val(atom))->hvalue;
}

Atom *atom_tab(Uint i)
{
    return &atom_tab_int(i)->atom;
}
