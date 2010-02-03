/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
#define atom_init_lock()	erts_smp_rwmtx_init(&atom_table_lock, \
						    "atom_tab")
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
	       erts_smp_atomic_read(&atom_put_ops));
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

    ASSERT(bytes <= MAX_ATOM_LENGTH);
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

    while(len--) {
	h = (h << 4) + *p++;
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

Eterm
am_atom_put(const char* name, int len)
{
    Atom a;
    Eterm ret;
    int aix;

    /*
     * Silently truncate the atom if it is too long. Overlong atoms
     * could occur in situations where we have no good way to return
     * an error, such as in the I/O system. (Unfortunately, many
     * drivers don't check for errors.)
     *
     * If an error should be produced for overlong atoms (such in
     * list_to_atom/1), the caller should check the length before
     * calling this function.
     */
    if (len > MAX_ATOM_LENGTH) {
	len = MAX_ATOM_LENGTH;
    }
#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_smp_atomic_inc(&atom_put_ops);
#endif
    a.len = len;
    a.name = (byte*)name;
    atom_read_lock();
    aix = index_get(&erts_atom_table, (void*) &a);
    atom_read_unlock();
    if (aix >= 0)
	ret = make_atom(aix);
    else {
	atom_write_lock();
	ret = make_atom(index_put(&erts_atom_table, (void*) &a));
	atom_write_unlock();
    }
    return ret;
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
erts_atom_get(const char *name, int len, Eterm* ap)
{
    Atom a;
    int i;
    int res;

    a.len = len;
    a.name = (byte *)name;
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

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_smp_atomic_init(&atom_put_ops, 0);
#endif

    atom_init_lock();
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
	a.name = (byte*)erl_atom_names[i];
	a.slot.index = i;
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
