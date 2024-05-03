/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

#ifndef _BEAM_FILE_H
#define _BEAM_FILE_H

#define ERTS_BEAM_MAX_OPARGS 8

#include "sys.h"
#include "atom.h"
#include "beam_types.h"

#define CHECKSUM_SIZE 16

#define MakeIffId(a, b, c, d) \
  (((Uint) (a) << 24) | ((Uint) (b) << 16) | ((Uint) (c) << 8) | (Uint) (d))

typedef struct {
    Sint32 form_id;

    Sint32 size;
    const byte *data;
} IFF_File;

typedef struct {
    Uint32 iff_id;

    Sint32 size;
    const byte *data;
} IFF_Chunk;

int iff_init(const byte *data, size_t size, IFF_File *iff);

/** @brief Reads a specific chunk from the given IFF file, skipping all
 * validation.
 *
 * @return 1 on success, 0 on failure */
int iff_read_chunk(IFF_File *iff, Uint id, IFF_Chunk *chunk);

typedef struct {
    Sint32 count;
    Eterm *entries;
} BeamFile_AtomTable;

typedef struct {
    Sint32 function_count;
    Sint32 label_count;
    Sint32 max_opcode;

    const byte *data;
    Sint32 size;
} BeamFile_Code;

typedef struct {
    Eterm function;
    int arity;

    /* Entry point */
    Sint32 label;
} BeamFile_ExportEntry;

typedef struct {
    Sint32 count;
    BeamFile_ExportEntry *entries;
} BeamFile_ExportTable;

typedef struct {
    Eterm module;
    Eterm function;
    int arity;
} BeamFile_ImportEntry;

typedef struct {
    Sint32 count;
    BeamFile_ImportEntry *entries;
} BeamFile_ImportTable;

typedef struct {
    Sint32 index;
    Sint32 old_uniq;

    Eterm function;
    Sint32 num_free;
    Sint32 arity;
    Sint32 label;
} BeamFile_LambdaEntry;

typedef struct {
    Sint count;
    BeamFile_LambdaEntry *entries;
} BeamFile_LambdaTable;

typedef struct {
    Sint32 location;
    Sint32 name_index;
} BeamFile_LineEntry;

typedef struct {
    Sint32 instruction_count;
    Sint32 flags;

    Sint32 name_count;
    Sint *names;

    Sint32 location_size;
    Sint32 item_count;
    BeamFile_LineEntry *items;
} BeamFile_LineTable;

enum beamfile_line_flags {
    BEAMFILE_EXECUTABLE_LINE = 1, /* The executable_line instruction is used. */
    BEAMFILE_FORCE_LINE_COUNTERS = 2 /* Force emission of line counters. */
};

typedef struct {
    struct erl_heap_fragment *heap_fragments;
    Eterm value;
} BeamFile_LiteralEntry;

typedef struct {
    Sint heap_size;

    Sint32 allocated;
    Sint32 count;
    BeamFile_LiteralEntry *entries;
} BeamFile_LiteralTable;

typedef struct {
    /* To simplify code that queries types, the first entry (which must be
     * present) is always the "any type." */
    Sint32 count;
    char fallback; /* If this is a fallback type table */
    BeamType *entries;
} BeamFile_TypeTable;

typedef struct {
    IFF_File iff;

    Eterm module;

    byte checksum[CHECKSUM_SIZE];

    BeamFile_AtomTable atoms;
    BeamFile_ImportTable imports;
    BeamFile_ExportTable exports;
#ifdef BEAMASM
    BeamFile_ExportTable locals;
#endif
    BeamFile_LambdaTable lambdas;
    BeamFile_LineTable lines;
    BeamFile_TypeTable types;

    /* Static literals are those defined in the file, and dynamic literals are
     * those created when loading. The former is positively indexed starting
     * from 0, while the latter is negatively indexed starting at -1.
     *
     * These are kept separate to enhance validation, erroring out if any TAG_q
     * read from the file refers to a literal outside the static table. */
    BeamFile_LiteralTable static_literals;
    BeamFile_LiteralTable dynamic_literals;

    BeamFile_Code code;

    /* Data-only chunks */
    struct {
        const byte *data;
        Sint32 size;
    } attributes, compile_info, strings;
} BeamFile;

enum beamfile_read_result {
    BEAMFILE_READ_SUCCESS,

    BEAMFILE_READ_CORRUPT_FILE_HEADER,

    /* Mandatory chunks */
    BEAMFILE_READ_MISSING_ATOM_TABLE,
    BEAMFILE_READ_OBSOLETE_ATOM_TABLE,
    BEAMFILE_READ_CORRUPT_ATOM_TABLE,
    BEAMFILE_READ_MISSING_CODE_CHUNK,
    BEAMFILE_READ_CORRUPT_CODE_CHUNK,
    BEAMFILE_READ_MISSING_EXPORT_TABLE,
    BEAMFILE_READ_CORRUPT_EXPORT_TABLE,
    BEAMFILE_READ_MISSING_IMPORT_TABLE,
    BEAMFILE_READ_CORRUPT_IMPORT_TABLE,
    BEAMFILE_READ_CORRUPT_LOCALS_TABLE,

    /* Optional chunks */
    BEAMFILE_READ_CORRUPT_LAMBDA_TABLE,
    BEAMFILE_READ_CORRUPT_LINE_TABLE,
    BEAMFILE_READ_CORRUPT_LITERAL_TABLE,
    BEAMFILE_READ_CORRUPT_TYPE_TABLE
};

typedef struct {
    /* TAG_xyz */
    UWord type;
    UWord val;
} BeamOpArg;

typedef struct beamop {
    /* Opcode */
    int op;

    /* Number of arguments */
    int arity;

    /* Default buffer for arguments */
    BeamOpArg def_args[ERTS_BEAM_MAX_OPARGS];

    /* Actual arguments */
    BeamOpArg *a;

    struct beamop *next;
} BeamOp;

typedef struct beamop_block {
    BeamOp ops[32];
    struct beamop_block* next;
} BeamOpBlock;

typedef struct {
    BeamOpBlock *beamop_blocks;
    BeamOp *free;
} BeamOpAllocator;

/*
 * !! These are included here to break a circular dependency !!
 */
#include "erl_process.h"
#include "erl_message.h"

void beamfile_init(void);

/** @brief Reads the given module binary into \p beam and validates its
 * structural integrity. */
enum beamfile_read_result
beamfile_read(const byte *data, size_t size, BeamFile *beam);

/** @brief Releases all resources associated with \p beam, but does not free
 * it. */
void beamfile_free(BeamFile *beam);

/** @brief Copies a term into the dynamic literal table
 *
 * @param[in] deduplicate Whether to try to deduplicate the term before
 * insertion. Set to zero if you require a new unique literal, for example if
 * it needs to be modified in the late stages of loading.
 *
 * @return A literal index that can be used in beamfile_get_literal */
Sint beamfile_add_literal(BeamFile *beam, Eterm term, int deduplicate);

/** @brief Gets a term from the literal table.
 *
 * The returned term is valid until the BeamFile is freed or the literals are
 * moved through \c beamfile_move_literals , which is destructive and makes
 * all previously returned terms invalid.
 *
 * As such, the return value should only be used briefly in pattern matching or
 * similar until they're permanently moved to the literal area. */
Eterm beamfile_get_literal(const BeamFile *beam, Sint index);

/** @brief Destructively moves all literals to hpp/oh.
 *
 * It's safe to use \c beamfile_get_literal both before and after this call,
 * but previously retrieved terms will be destroyed and need to be retrieved
 * again. */
void beamfile_move_literals(BeamFile *beam, Eterm **hpp, ErlOffHeap *oh);

void beamopallocator_init(BeamOpAllocator *allocator);
void beamopallocator_dtor(BeamOpAllocator *allocator);

ERTS_GLB_INLINE
BeamOp *beamopallocator_new_op(BeamOpAllocator *allocator);

ERTS_GLB_INLINE
void beamopallocator_free_op(BeamOpAllocator *allocator, BeamOp *op);

/** @brief Internal helper for \c beamopallocator_new_op */
void beamopallocator_expand__(BeamOpAllocator *allocator);

/**/

typedef struct BeamCodeReader__ BeamCodeReader;

BeamCodeReader *beamfile_get_code(BeamFile *beam, BeamOpAllocator *op_alloc);
int beamcodereader_next(BeamCodeReader *code_reader, BeamOp **op);
void beamcodereader_close(BeamCodeReader *code_reader);

/**/

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef DEBUG
# define GARBAGE__ 0xCC
# define DEBUG_INIT_BEAMOP(Dst) sys_memset(Dst, GARBAGE__, sizeof(BeamOp))
#else
# define DEBUG_INIT_BEAMOP(Dst)
#endif

ERTS_GLB_INLINE
BeamOp *beamopallocator_new_op(BeamOpAllocator *allocator) {
    BeamOp *res;

    if (allocator->free == NULL) {
        beamopallocator_expand__(allocator);
    }

    res = allocator->free;
    allocator->free = res->next;

    DEBUG_INIT_BEAMOP(res);
    res->a = res->def_args;

    return res;
}

ERTS_GLB_INLINE
void beamopallocator_free_op(BeamOpAllocator *allocator, BeamOp *op) {
    if (op->a != op->def_args) {
        erts_free(ERTS_ALC_T_LOADER_TMP, op->a);
    }

    op->next = allocator->free;
    allocator->free = op;
}

#endif

#endif
