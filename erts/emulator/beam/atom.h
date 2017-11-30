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

#ifndef __ATOM_H__
#define __ATOM_H__

#include "index.h"
#include "erl_atom_table.h"

#define MAX_ATOM_CHARACTERS 255
#define MAX_ATOM_SZ_FROM_LATIN1 (2*MAX_ATOM_CHARACTERS)
#define MAX_ATOM_SZ_LIMIT (4*MAX_ATOM_CHARACTERS) /* theoretical byte limit */
#define ATOM_LIMIT (1024*1024)
#define MIN_ATOM_TABLE_SIZE 8192
#define ATOM_BAD_ENCODING_ERROR -1
#define ATOM_MAX_CHARS_ERROR -2

#ifndef ARCH_32
/* Internal atom cache needs MAX_ATOM_TABLE_SIZE to be less than an
   unsigned 32 bit integer. See external.c(erts_encode_ext_dist_header_setup)
   for more details. */
#define MAX_ATOM_TABLE_SIZE ((MAX_ATOM_INDEX + 1 < (UWORD_CONSTANT(1) << 32)) ? MAX_ATOM_INDEX + 1 : ((UWORD_CONSTANT(1) << 31) - 1)) /* Here we use maximum signed interger value to avoid integer overflow */
#else
#define MAX_ATOM_TABLE_SIZE (MAX_ATOM_INDEX + 1)
#endif


/*
 * Atom entry.
 */
typedef struct atom {
    IndexSlot slot;  /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    Sint16 len;      /* length of atom name (UTF-8 encoded) */
    Sint16 latin1_chars; /* 0-255 if atom can be encoded in latin1; otherwise, -1 */
    int ord0;        /* ordinal value of first 3 bytes + 7 bits */
    byte* name;      /* name of atom */
} Atom;

extern IndexTable erts_atom_table;

ERTS_GLB_INLINE Atom* atom_tab(Uint i);
ERTS_GLB_INLINE int erts_is_atom_utf8_bytes(byte *text, size_t len, Eterm term);
ERTS_GLB_INLINE int erts_is_atom_str(const char *str, Eterm term, int is_latin1);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Atom*
atom_tab(Uint i)
{
    return (Atom *) erts_index_lookup(&erts_atom_table, i);
}

ERTS_GLB_INLINE int erts_is_atom_utf8_bytes(byte *text, size_t len, Eterm term)
{
    Atom *a;
    if (!is_atom(term))
	return 0;
    a = atom_tab(atom_val(term));
    return (len == (size_t) a->len
	    && sys_memcmp((void *) a->name, (void *) text, len) == 0);
}

ERTS_GLB_INLINE int erts_is_atom_str(const char *str, Eterm term, int is_latin1)
{
    Atom *a;
    int i, len;
    const byte* aname;
    const byte* s = (const byte*) str;

    if (!is_atom(term))
	return 0;
    a = atom_tab(atom_val(term));
    len = a->len;
    aname = a->name;
    if (is_latin1) {
	for (i = 0; i < len; s++) {
	    if (aname[i] < 0x80) {
		if (aname[i] != *s || *s == '\0')
		    return 0;
		i++;
	    }
	    else {
		if (aname[i]   != (0xC0 | (*s >> 6)) || 
		    aname[i+1] != (0x80 | (*s & 0x3F))) {
		    return 0;
		}
		i += 2;
	    }
	}
    }
    else {
	for (i = 0; i < len; i++, s++)
	    if (aname[i] != *s || *s == '\0')
		return 0;
    }
    return *s == '\0';
}

#endif

typedef enum {
    ERTS_ATOM_ENC_7BIT_ASCII,
    ERTS_ATOM_ENC_LATIN1,
    ERTS_ATOM_ENC_UTF8
} ErtsAtomEncoding;

/*
 * Note, ERTS_IS_ATOM_STR() expects the first argument to be a
 * 7-bit ASCII string literal.
 */
#define ERTS_IS_ATOM_STR(LSTR, TERM) \
  (erts_is_atom_utf8_bytes((byte *) LSTR, sizeof(LSTR) - 1, (TERM)))
#define ERTS_DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#define ERTS_INIT_AM(S) AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#define ERTS_MAKE_AM(Str) am_atom_put(Str, sizeof(Str) - 1)

int atom_table_size(void);	/* number of elements */
int atom_table_sz(void);	/* table size in bytes, excluding stored objects */

Eterm am_atom_put(const char*, int); /* ONLY 7-bit ascii! */
Eterm erts_atom_put(const byte *name, int len, ErtsAtomEncoding enc, int trunc);
int erts_atom_put_index(const byte *name, int len, ErtsAtomEncoding enc, int trunc);
void init_atom_table(void);
void atom_info(fmtfn_t, void *);
void dump_atoms(fmtfn_t, void *);
Uint erts_get_atom_limit(void);
int erts_atom_get(const char* name, int len, Eterm* ap, ErtsAtomEncoding enc);
void erts_atom_get_text_space_sizes(Uint *reserved, Uint *used);
#endif

