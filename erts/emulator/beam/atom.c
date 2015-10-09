/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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

IndexTable erts_atom_table;	/* The index table */

#include "erl_smp.h"

static erts_smp_rwmtx_t atom_table_lock;

#define atom_read_lock()	erts_smp_rwmtx_rlock(&atom_table_lock)
#define atom_read_unlock()	erts_smp_rwmtx_runlock(&atom_table_lock)
#define atom_write_lock()	erts_smp_rwmtx_rwlock(&atom_table_lock)
#define atom_write_unlock()	erts_smp_rwmtx_rwunlock(&atom_table_lock)

#if 0
#define ERTS_ATOM_PUT_OPS_STAT
#endif
#ifdef ERTS_ATOM_PUT_OPS_STAT
static erts_smp_atomic_t atom_put_ops;
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

/*
 * Print info about atom tables
 */
void atom_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    index_info(to, to_arg, &erts_atom_table);
#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_print(to, to_arg, "atom_put_ops: %ld\n",
	       erts_smp_atomic_read_nob(&atom_put_ops));
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
atom_hash(Atom* obj)
{
    byte* p = obj->name;
    int len = obj->len;
    HashValue h = 0, g;
    byte v;

    while(len--) {
	v = *p++;
	/* latin1 clutch for r16 */
	if ((v & 0xFE) == 0xC2 && (*p & 0xC0) == 0x80) {
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
atom_cmp(Atom* tmpl, Atom* obj)
{
    if (tmpl->len == obj->len &&
	sys_memcmp(tmpl->name, obj->name, tmpl->len) == 0)
	return 0;
    return 1;
}


static Atom*
atom_alloc(Atom* tmpl)
{
    Atom* obj = (Atom*) erts_alloc(ERTS_ALC_T_ATOM, sizeof(Atom));

    obj->name = atom_text_alloc(tmpl->len);
    sys_memcpy(obj->name, tmpl->name, tmpl->len);
    obj->len = tmpl->len;
    obj->latin1_chars = tmpl->latin1_chars;
    obj->slot.index = -1;

    /*
     * Precompute ordinal value of first 3 bytes + 7 bits.
     * This is used by utils.c:cmp_atoms().
     * We cannot use the full 32 bits of the first 4 bytes,
     * since we use the sign of the difference between two
     * ordinal values to represent their relative order.
     */
    {
	unsigned char c[4];
	int i;
	int j;

	j = (tmpl->len < 4) ? tmpl->len : 4;
	for(i = 0; i < j; ++i)
	    c[i] = tmpl->name[i];
	for(; i < 4; ++i)
	    c[i] = '\0';
	obj->ord0 = (c[0] << 23) + (c[1] << 15) + (c[2] << 7) + (c[3] >> 1);
    }
    return obj;
}

static void
atom_free(Atom* obj)
{
    erts_free(ERTS_ALC_T_ATOM, (void*) obj);
}

static void latin1_to_utf8(byte* conv_buf, const byte** srcp, int* lenp)
{
    byte* dst;
    const byte* src = *srcp;
    int i, len = *lenp;

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
 * erts_atom_put() may fail. If it fails THE_NON_VALUE is returned!
 */
Eterm
erts_atom_put(const byte *name, int len, ErtsAtomEncoding enc, int trunc)
{
    byte utf8_copy[MAX_ATOM_SZ_FROM_LATIN1];
    const byte *text = name;
    int tlen = len;
    Sint no_latin1_chars;
    Atom a;
    int aix;

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_smp_atomic_inc_nob(&atom_put_ops);
#endif

    if (tlen < 0) {
	if (trunc)
	    tlen = 0;
	else
	    return THE_NON_VALUE;
    }

    switch (enc) {
    case ERTS_ATOM_ENC_7BIT_ASCII:
	if (tlen > MAX_ATOM_CHARACTERS) {
	    if (trunc)
		tlen = MAX_ATOM_CHARACTERS;
	    else
		return THE_NON_VALUE;
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
		return THE_NON_VALUE;
	}
	no_latin1_chars = tlen;
	latin1_to_utf8(utf8_copy, &text, &tlen);
	break;
    case ERTS_ATOM_ENC_UTF8:
	/* First sanity check; need to verify later */
	if (tlen > MAX_ATOM_SZ_LIMIT && !trunc)
	    return THE_NON_VALUE;
	break;
    }

    a.len = tlen;
    a.name = (byte *) text;
    atom_read_lock();
    aix = index_get(&erts_atom_table, (void*) &a);
    atom_read_unlock();
    if (aix >= 0) {
	/* Already in table no need to verify it */
	return make_atom(aix);
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
		return THE_NON_VALUE;
	    ASSERT(no_chars == MAX_ATOM_CHARACTERS);
	    tlen = err_pos - text;
	    break;
	default:
	    /* Bad utf8... */
	    return THE_NON_VALUE;
	}
    }

    ASSERT(tlen <= MAX_ATOM_SZ_LIMIT);
    ASSERT(-1 <= no_latin1_chars && no_latin1_chars <= MAX_ATOM_CHARACTERS);

    a.len = tlen;
    a.latin1_chars = (Sint16) no_latin1_chars;
    a.name = (byte *) text;
    atom_write_lock();
    aix = index_put(&erts_atom_table, (void*) &a);
    atom_write_unlock();
    return make_atom(aix);
}

Eterm
am_atom_put(const char* name, int len)
{
    /* Assumes 7-bit ascii; use erts_atom_put() for other encodings... */
    return erts_atom_put((byte *) name, len, ERTS_ATOM_ENC_7BIT_ASCII, 1);
}

int atom_table_size(void)
{
    int ret;
#ifdef ERTS_SMP
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
#endif
    ret = erts_atom_table.entries;
#ifdef ERTS_SMP
    if (lock)
	atom_read_unlock();
#endif
    return ret;
}

int atom_table_sz(void)
{
    int ret;
#ifdef ERTS_SMP
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
#endif
    ret = index_table_sz(&erts_atom_table);
#ifdef ERTS_SMP
    if (lock)
	atom_read_unlock();
#endif
    return ret;
}

int
erts_atom_get(const char *name, int len, Eterm* ap, ErtsAtomEncoding enc)
{
    byte utf8_copy[MAX_ATOM_SZ_FROM_LATIN1];
    Atom a;
    int i;
    int res;

    a.len = (Sint16) len;
    a.name = (byte *)name;
    if (enc == ERTS_ATOM_ENC_LATIN1) {
	latin1_to_utf8(utf8_copy, (const byte**)&a.name, &len);
	a.len = (Sint16) len;
    }
    atom_read_lock();
    i = index_get(&erts_atom_table, (void*) &a);
    res = i < 0 ? 0 : (*ap = make_atom(i), 1);
    atom_read_unlock();
    return res;
}

void
erts_atom_get_text_space_sizes(Uint *reserved, Uint *used)
{
#ifdef ERTS_SMP
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
#endif
    if (reserved)
	*reserved = reserved_atom_space;
    if (used)
	*used = atom_space;
#ifdef ERTS_SMP
    if (lock)
	atom_read_unlock();
#endif
}

void
init_atom_table(void)
{
    HashFunctions f;
    int i;
    Atom a;
    erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;

    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_SMP_RWMTX_LONG_LIVED;

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_smp_atomic_init_nob(&atom_put_ops, 0);
#endif

    erts_smp_rwmtx_init_opt(&atom_table_lock, &rwmtx_opt, "atom_tab");

    f.hash = (H_FUN) atom_hash;
    f.cmp  = (HCMP_FUN) atom_cmp;
    f.alloc = (HALLOC_FUN) atom_alloc;
    f.free = (HFREE_FUN) atom_free;

    atom_text_pos = NULL;
    atom_text_end = NULL;
    reserved_atom_space = 0;
    atom_space = 0;
    text_list = NULL;

    erts_index_init(ERTS_ALC_T_ATOM_TABLE, &erts_atom_table,
		    "atom_tab", ATOM_SIZE, erts_atom_table_size, f);
    more_atom_space();

    /* Ordinary atoms */
    for (i = 0; erl_atom_names[i] != 0; i++) {
	int ix;
	a.len = strlen(erl_atom_names[i]);
	a.latin1_chars = a.len;
	a.name = (byte*)erl_atom_names[i];
	a.slot.index = i;
#ifdef DEBUG
	/* Verify 7-bit ascii */
	for (ix = 0; ix < a.len; ix++) {
	    ASSERT((a.name[ix] & 0x80) == 0);
	}
#endif
	ix = index_put(&erts_atom_table, (void*) &a);
	atom_text_pos -= a.len;
	atom_space -= a.len;
	atom_tab(ix)->name = (byte*)erl_atom_names[i];
    }
}

void
dump_atoms(int to, void *to_arg)
{
    int i = erts_atom_table.entries;

    /*
     * Print out the atom table starting from the end.
     */
    while (--i >= 0) {
	if (erts_index_lookup(&erts_atom_table, i)) {
	    erts_print(to, to_arg, "%T\n", make_atom(i));
	}
    }
}
