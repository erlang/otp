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

#ifndef __ATOM_H__
#define __ATOM_H__

#ifndef __INDEX_H__
#include "index.h"
#endif

#include "erl_atom_table.h"

#define MAX_ATOM_LENGTH 255
#define ATOM_LIMIT (1024*1024)
#define MIN_ATOM_TABLE_SIZE 8192

#ifndef ARCH_32
/* Internal atom cache needs MAX_ATOM_TABLE_SIZE to be less than an
   unsigned 32 bit integer. See external.c(erts_encode_ext_dist_header_setup)
   for more details. */
#define MAX_ATOM_TABLE_SIZE ((MAX_ATOM_INDEX + 1 < (1UL << 32)) ? MAX_ATOM_INDEX + 1 : (1UL << 32))
#else
#define MAX_ATOM_TABLE_SIZE (MAX_ATOM_INDEX + 1)
#endif


/*
 * Atom entry.
 */
typedef struct atom {
    IndexSlot slot;  /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    int len;         /* length of atom name */
    int ord0;        /* ordinal value of first 3 bytes + 7 bits */
    byte* name;      /* name of atom */
} Atom;

extern IndexTable erts_atom_table;

ERTS_GLB_INLINE Atom* atom_tab(Uint i);
ERTS_GLB_INLINE int erts_is_atom_bytes(byte *text, size_t len, Eterm term);
ERTS_GLB_INLINE int erts_is_atom_str(char *str, Eterm term);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Atom*
atom_tab(Uint i)
{
    return (Atom *) erts_index_lookup(&erts_atom_table, i);
}

ERTS_GLB_INLINE int erts_is_atom_bytes(byte *text, size_t len, Eterm term)
{
    Atom *a;
    if (!is_atom(term))
	return 0;
    a = atom_tab(atom_val(term));
    return (len == (size_t) a->len
	    && sys_memcmp((void *) a->name, (void *) text, len) == 0);
}

ERTS_GLB_INLINE int erts_is_atom_str(char *str, Eterm term)
{
    Atom *a;
    int i, len;
    char *aname;
    if (!is_atom(term))
	return 0;
    a = atom_tab(atom_val(term));
    len = a->len;
    aname = (char *) a->name;
    for (i = 0; i < len; i++)
	if (aname[i] != str[i] || str[i] == '\0')
	    return 0;
    return str[len] == '\0';
}

#endif

/*
 * Note, ERTS_IS_ATOM_STR() expects the first argument to be a
 * string literal.
 */
#define ERTS_IS_ATOM_STR(LSTR, TERM) \
  (erts_is_atom_bytes((byte *) LSTR, sizeof(LSTR) - 1, (TERM)))
#define ERTS_DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#define ERTS_INIT_AM(S) AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

int atom_table_size(void);	/* number of elements */
int atom_table_sz(void);	/* table size in bytes, excluding stored objects */

Eterm am_atom_put(const char*, int); /* most callers pass plain char*'s */
int atom_erase(byte*, int);
int atom_static_put(byte*, int);
void init_atom_table(void);
void atom_info(int, void *);
void dump_atoms(int, void *);
int erts_atom_get(const char* name, int len, Eterm* ap);
void erts_atom_get_text_space_sizes(Uint *reserved, Uint *used);
#endif

