/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
#include "config.h"
#endif

#include "beam_file.h"
#include "beam_load.h"
#include "erl_zlib.h"
#include "big.h"
#include "erl_unicode.h"
#include "erl_binary.h"
#include "erl_global_literals.h"

#define LoadError(Expr)      \
    do {                     \
        (void)(Expr);        \
        return 0;            \
    } while(0)

#define LoadAssert(Expr)     \
    do {                     \
        if (!(Expr)) {       \
            return 0;        \
        }                    \
    } while(0)

/* Quick sanity check for item counts; if the resulting array can't fit into
 * 1GB it's most likely wonky. */
#define CHECK_ITEM_COUNT(Count, Minimum, ItemSize) \
    ((Count) >= (Minimum) && ((Count) < (1 << 30) / ItemSize))

typedef struct {
    const byte *head;
    const byte *end;
} BeamReader;

struct BeamCodeReader__ {
    BeamReader reader;

    BeamOpAllocator *allocator;
    BeamFile *file;

    BeamOp *pending;

    BeamOpArg current_func_label;
    BeamOpArg current_entry_label;
    int first;
};

typedef struct {
    int tag;

    /* Size of the number pointed to by `ptr_value`, or zero if it's small
     * enough to fit into `word_value`. */
    Sint32 size;

    union {
        const byte *ptr_value;
        SWord word_value;
    };
} TaggedNumber;

static void beamreader_init(const byte *data, size_t size, BeamReader *reader) {
    reader->head = data;
    reader->end = &data[size];
}

static int beamreader_test(const BeamReader *reader, size_t size) {
    ASSERT(reader->end >= reader->head);
    return (reader->end - reader->head) >= size;
}

static int beamreader_eof(const BeamReader *reader) {
    ASSERT(reader->end >= reader->head);
    return reader->end == reader->head;
}

static int beamreader_read_u8(BeamReader *reader, byte *val) {
    if (!beamreader_test(reader, 1)) {
        return 0;
    }

    *val = reader->head[0];
    reader->head++;

    return 1;
}

static int beamreader_read_bytes(BeamReader *reader, size_t size, const byte **val) {
    if (!beamreader_test(reader, size)) {
        return 0;
    }

    *val = reader->head;
    reader->head += size;

    return 1;
}

static int beamreader_read_i16(BeamReader *reader, Sint16 *val) {
    LoadAssert(beamreader_test(reader, sizeof(Sint16)));

    *val = (Sint16)reader->head[0] << 0x08 |
           (Sint16)reader->head[1] << 0x00;

    reader->head += sizeof(Sint16);

    return 1;
}

static int beamreader_read_i32(BeamReader *reader, Sint32 *val) {
    LoadAssert(beamreader_test(reader, sizeof(Sint32)));

    *val = (Sint32)reader->head[0] << 0x18 |
           (Sint32)reader->head[1] << 0x10 |
           (Sint32)reader->head[2] << 0x08 |
           (Sint32)reader->head[3] << 0x00;

    reader->head += sizeof(Sint32);

    return 1;
}

/* Internal helper function for reading U-tagged values and similar.
 *
 * Returns 0 on success, and 1 if the result doesn't fit into a signed word. */
static int unpack_varint(int size, const byte *data, SWord *val) {
    if (size <= sizeof(SWord)) {
        UWord res;
        int i;

        res = 0;
        for (i = 0; i < size; i++) {
            res = ((UWord)data[i] << ((size - i - 1) * CHAR_BIT)) | res;
        }

        *val = (SWord)res;
        return 1;
    }

    return 0;
}

static int beamreader_read_tagged(BeamReader *reader, TaggedNumber *val) {
    byte len_code;

    LoadAssert(beamreader_read_u8(reader, &len_code));

    val->tag = len_code & 0x07;

    if ((len_code & 0x08) == 0) {
        val->word_value = len_code >> 4;
        val->size = 0;
    } else if ((len_code & 0x10) == 0) {
        byte extra_byte;

        LoadAssert(beamreader_read_u8(reader, &extra_byte));

        val->word_value = ((len_code >> 5) << 8) | extra_byte;
        val->size = 0;
    } else {
        const byte *data;
        size_t count;

        len_code >>= 5;
        if (len_code < 7) {
            static const int size_base = 2;
            count = len_code + size_base;
        } else {
            static const int size_base = 9;
            TaggedNumber size_prefix;
            Sint unpacked_size;

            LoadAssert(beamreader_read_tagged(reader, &size_prefix));
            LoadAssert(size_prefix.tag == TAG_u);

            unpacked_size = size_prefix.word_value;
            LoadAssert(unpacked_size < (ERTS_SINT32_MAX - size_base));

            count = unpacked_size + size_base;
        }

        LoadAssert(beamreader_read_bytes(reader, count, &data));

        /* Try to unpack the value into a word if possible. */
        if (unpack_varint(count, data, &val->word_value)) {
            if (val->tag == TAG_i) {
                SWord sign_extended_value;
                int shift;

                shift = CHAR_BIT * (sizeof(SWord) - count);
                sign_extended_value = (val->word_value << shift) >> shift;

                if (IS_SSMALL(sign_extended_value)) {
                    val->word_value = sign_extended_value;
                    val->size = 0;
                    return 1;
                }
            } else if (val->word_value >= 0) {
                val->size = 0;
                return 1;
            }
        }

        if (val->tag != TAG_i) {
            /* Integers (TAG_i) are the only values allowed to be negative or
             * larger than a word. Indicate overflow by changing the tag to
             * TAG_o. */
            val->tag = TAG_o;
        }

        val->ptr_value = data;
        val->size = count;
    }

    return 1;
}

static int parse_atom_chunk(BeamFile *beam,
                            IFF_Chunk *chunk) {
    BeamFile_AtomTable *atoms;
    BeamReader reader;
    Sint32 count;
    int i;

    ASSERT(beam->atoms.entries == NULL);
    atoms = &beam->atoms;

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &count));
    LoadAssert(CHECK_ITEM_COUNT(count, 1, sizeof(atoms->entries[0])));

    /* Reserve a slot for the empty list, which is encoded as atom 0 as we
     * didn't want to reserve a tag for this back in the day.
     *
     * It's always converted to TAG_n on loading so we'll fill its slot with
     * THE_NON_VALUE to catch tag errors. */
    count++;

    atoms->entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                count * sizeof(atoms->entries[0]));
    atoms->entries[0] = THE_NON_VALUE;
    atoms->count = count;

    for (i = 1; i < count; i++) {
        const byte *string;
        byte length;
        Eterm atom;

        LoadAssert(beamreader_read_u8(&reader, &length));
        LoadAssert(beamreader_read_bytes(&reader, length, &string));

        atom = erts_atom_put(string, length, ERTS_ATOM_ENC_UTF8, 1);
        LoadAssert(atom != THE_NON_VALUE);

        atoms->entries[i] = atom;
    }

    beam->module = atoms->entries[1];

    return 1;
}

static int parse_import_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    BeamFile_ImportTable *imports;
    BeamFile_AtomTable *atoms;
    BeamReader reader;
    Sint32 count;
    int i;

    imports = &beam->imports;
    ASSERT(imports->entries == NULL);

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &count));
    LoadAssert(CHECK_ITEM_COUNT(count, 0, sizeof(imports->entries[0])));

    imports->entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                  count * sizeof(imports->entries[0]));
    imports->count = count;

    atoms = &beam->atoms;

    for (i = 0; i < count; i++) {
        Sint32 module_atom_index;
        Sint32 function_atom_index;
        Sint32 arity;

        LoadAssert(beamreader_read_i32(&reader, &module_atom_index));
        LoadAssert(beamreader_read_i32(&reader, &function_atom_index));
        LoadAssert(beamreader_read_i32(&reader, &arity));

        LoadAssert(module_atom_index >= 0 && function_atom_index >= 0);
        LoadAssert(module_atom_index < atoms->count);
        LoadAssert(function_atom_index < atoms->count);
        LoadAssert(arity >= 0 && arity <= MAX_ARG);

        imports->entries[i].module = atoms->entries[module_atom_index];
        imports->entries[i].function = atoms->entries[function_atom_index];
        imports->entries[i].arity = arity;
    }

    return 1;
}

static int parse_export_table(BeamFile_ExportTable *dest,
			      BeamFile *beam, IFF_Chunk *chunk) {
    BeamFile_AtomTable *atoms;
    BeamReader reader;
    Sint32 count;
    int i;

    ASSERT(dest->entries == NULL);

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &count));
    LoadAssert(CHECK_ITEM_COUNT(count, 0, sizeof(dest->entries[0])));

    dest->entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                  count * sizeof(dest->entries[0]));
    dest->count = count;

    atoms = &beam->atoms;

    for (i = 0; i < count; i++) {
        Sint32 atom_index, arity, label;

        LoadAssert(beamreader_read_i32(&reader, &atom_index));
        LoadAssert(beamreader_read_i32(&reader, &arity));
        LoadAssert(beamreader_read_i32(&reader, &label));

        LoadAssert(atom_index >= 0 && atom_index < atoms->count);
        LoadAssert(arity >= 0 && arity <= MAX_ARG);
        LoadAssert(label >= 0);

        dest->entries[i].function = atoms->entries[atom_index];
        dest->entries[i].arity = arity;
        dest->entries[i].label = label;
    }

    return 1;
}

static int parse_export_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    return parse_export_table(&beam->exports, beam, chunk);
}

#ifdef BEAMASM
static int parse_locals_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    return parse_export_table(&beam->locals, beam, chunk);
}
#endif

static int parse_lambda_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    BeamFile_LambdaTable *lambdas;
    BeamFile_AtomTable *atoms;
    BeamReader reader;
    Sint32 count;
    int i;

    lambdas = &beam->lambdas;
    ASSERT(lambdas->entries == NULL);

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &count));
    LoadAssert(CHECK_ITEM_COUNT(count, 0, sizeof(lambdas->entries[0])));

    lambdas->entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                  count * sizeof(lambdas->entries[0]));
    lambdas->count = count;

    atoms = &beam->atoms;

    for (i = 0; i < count; i++) {
        Sint32 atom_index, arity, label, fun_index, num_free, old_uniq;

        LoadAssert(beamreader_read_i32(&reader, &atom_index));
        LoadAssert(beamreader_read_i32(&reader, &arity));
        LoadAssert(beamreader_read_i32(&reader, &label));
        LoadAssert(beamreader_read_i32(&reader, &fun_index));
        LoadAssert(beamreader_read_i32(&reader, &num_free));
        LoadAssert(beamreader_read_i32(&reader, &old_uniq));

        LoadAssert(atom_index >= 0 && atom_index < atoms->count);
        LoadAssert(label >= 0);

        lambdas->entries[i].function = atoms->entries[atom_index];
        lambdas->entries[i].num_free = num_free;
        lambdas->entries[i].arity = arity;
        lambdas->entries[i].label = label;

        lambdas->entries[i].index = fun_index;
        lambdas->entries[i].old_uniq = old_uniq;
    }

    return 1;
}

static int parse_line_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    BeamFile_LineTable *lines;
    BeamReader reader;

    Sint32 version, flags, instr_count, item_count, name_count, name_index;
    int i;

    lines = &beam->lines;
    ASSERT(lines->items == NULL && lines->names == NULL);

    if (erts_no_line_info) {
        return 1;
    }

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &version));

    if (version != 0) {
        return 1;
    }

    LoadAssert(beamreader_read_i32(&reader, &flags));
    LoadAssert(beamreader_read_i32(&reader, &instr_count));
    LoadAssert(beamreader_read_i32(&reader, &item_count));
    LoadAssert(beamreader_read_i32(&reader, &name_count));

    LoadAssert(CHECK_ITEM_COUNT(item_count, 0, sizeof(lines->items[0])));
    LoadAssert(CHECK_ITEM_COUNT(name_count, 0, sizeof(lines->names[0])));

    /* Include the implicit "module name with .erl suffix" entry so we don't
     * have to special-case it anywhere else. */
    name_count++;

    /* Save flags. */
    lines->flags = flags;

    /* Reserve space for the "undefined location" entry. */
    item_count++;

    lines->instruction_count = instr_count;

    lines->items = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                              item_count * sizeof(lines->items[0]));
    lines->item_count = item_count;

    /* The zeroth entry in the line item table is always present and contains
     * the "undefined location." */
    lines->items[0].name_index = 0;
    lines->items[0].location = 0;

    lines->names = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                              name_count * sizeof(lines->names[0]));
    lines->name_count = name_count;

    /* We have to use the 32-bit representation if there's any names other than
     * the implicit "module_name.erl" in the table, as we can't fit LOC_FILE in
     * 16 bits. */
    lines->location_size = name_count > 1 ? sizeof(Sint32) : sizeof(Sint16);

    name_index = 0;
    i = 1;

    while (i < item_count) {
        TaggedNumber val;

        LoadAssert(beamreader_read_tagged(&reader, &val));

        switch(val.tag) {
        case TAG_a:
            name_index = val.word_value;

            /* The name table is off by one as 0 refers to the module, so we're
             * testing with '<=' instead of '<'. */
            LoadAssert(name_index <= name_count);
            break;
        case TAG_i:
            /* Line numbers must fit into a positive word, anything requiring a
             * bignum won't fit into memory anyway. */
            LoadAssert(val.size == 0 && val.word_value >= 0);

            if (val.word_value > 0xFFFF) {
                lines->location_size = sizeof(Sint32);
            }

            lines->items[i].location = val.word_value;
            lines->items[i].name_index = name_index;
            i++;

            break;
        default:
            LoadError("Illegal line item tag");
        }
    }

    /* Add the implicit "module_name.erl" entry, followed by the rest of the
     * name table. */
    {
        Eterm default_name_buf[MAX_ATOM_CHARACTERS * 2];
        Eterm *name_heap = default_name_buf;
        Eterm name, suffix;
        Eterm *hp;

        suffix = erts_get_global_literal(ERTS_LIT_ERL_FILE_SUFFIX);

        hp = name_heap;
        name = erts_atom_to_string(&hp, beam->module, suffix);

        lines->names[0] = beamfile_add_literal(beam, name, 1);

        for (i = 1; i < name_count; i++) {
            Uint num_chars, num_built, num_eaten;
            const byte *name_data, *err_pos;
            Sint16 name_length;
            Eterm *hp;

            LoadAssert(beamreader_read_i16(&reader, &name_length));
            LoadAssert(name_length >= 0);

            LoadAssert(beamreader_read_bytes(&reader, name_length, &name_data));

            if (name_length > 0) {
                LoadAssert(erts_analyze_utf8(name_data, name_length,
                                             &err_pos, &num_chars,
                                             NULL) == ERTS_UTF8_OK);

                if (num_chars < MAX_ATOM_CHARACTERS) {
                    name_heap = default_name_buf;
                } else {
                    name_heap = erts_alloc(ERTS_ALC_T_LOADER_TMP,
                                           num_chars * sizeof(Eterm[2]));
                }

                hp = name_heap;
                name = erts_make_list_from_utf8_buf(&hp, num_chars,
                                                    name_data,
                                                    name_length,
                                                    &num_built,
                                                    &num_eaten,
                                                    NIL);

                ASSERT(num_built == num_chars);
                ASSERT(num_eaten == name_length);

                lines->names[i] = beamfile_add_literal(beam, name, 1);

                if (name_heap != default_name_buf) {
                    erts_free(ERTS_ALC_T_LOADER_TMP, name_heap);
                }
            } else {
                /* Empty file names are rather unusual and annoying to deal
                 * with since NIL isn't a valid literal, so we'll fake it with
                 * our module name instead. */
                lines->names[i] = lines->names[0];
            }
        }
    }

    return 1;
}

/* We assume the presence of a type table to simplify loading, so we'll need to
 * create a dummy table (with single entry for the "any type") when we don't
 * have one. */
static void init_fallback_type_table(BeamFile *beam) {
    BeamFile_TypeTable *types;

    types = &beam->types;
    ASSERT(types->entries == NULL);

    types->entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                sizeof(types->entries[0]));
    types->count = 1;
    types->fallback = 1;

    types->entries[0].type_union = BEAM_TYPE_ANY;
    types->entries[0].metadata_flags = 0;
    types->entries[0].size_unit = 1;
    types->entries[0].min = MAX_SMALL + 1;
    types->entries[0].max = MIN_SMALL - 1;
}

static int parse_type_chunk_data(BeamFile *beam, BeamReader *p_reader) {
    BeamFile_TypeTable *types;

    Sint32 count;
    int i;

    types = &beam->types;
    ASSERT(types->entries == NULL);

    LoadAssert(beamreader_read_i32(p_reader, &count));
    LoadAssert(CHECK_ITEM_COUNT(count, 0, sizeof(types->entries[0])));
    LoadAssert(count >= 1);

    types->entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                count * sizeof(types->entries[0]));
    types->count = count;
    types->fallback = 0;

    for (i = 0; i < count; i++) {
        const byte *type_data;
        int extra;

        LoadAssert(beamreader_read_bytes(p_reader, 2, &type_data));
        extra = beam_types_decode_type(type_data, &types->entries[i]);
        LoadAssert(extra >= 0);
        LoadAssert(beamreader_read_bytes(p_reader, extra, &type_data));
        beam_types_decode_extra(type_data, &types->entries[i]);
    }

    /* The first entry MUST be the "any type." */
    LoadAssert(types->entries[0].type_union == BEAM_TYPE_ANY);
    LoadAssert(types->entries[0].min > types->entries[0].max);

    return 1;
}

static int parse_type_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    BeamReader reader;
    Sint32 version;

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &version));

    if (version == BEAM_TYPES_VERSION) {
        return parse_type_chunk_data(beam, &reader);
    } else {
        /* Incompatible type format. */
        init_fallback_type_table(beam);
        return 1;
    }
}

static ErlHeapFragment *new_literal_fragment(Uint size)
{
    ErlHeapFragment *bp;

    bp = (ErlHeapFragment*) ERTS_HEAP_ALLOC(ERTS_ALC_T_PREPARED_CODE,
                                            ERTS_HEAP_FRAG_SIZE(size));
    ERTS_INIT_HEAP_FRAG(bp, size, size);

    return bp;
}

static void free_literal_fragment(ErlHeapFragment *fragment) {
    while (fragment != NULL) {
        ErlHeapFragment *next_fragment = fragment->next;

        erts_cleanup_offheap(&fragment->off_heap);
        ERTS_HEAP_FREE(ERTS_ALC_T_PREPARED_CODE, (void *) fragment,
                        ERTS_HEAP_FRAG_SIZE(fragment->size));

        fragment = next_fragment;
    }
}

static int parse_decompressed_literals(BeamFile *beam,
                                       byte *data,
                                       uLongf size) {
    BeamFile_LiteralTable *literals;
    BeamFile_LiteralEntry *entries;
    BeamReader reader;

    Sint heap_size;
    Sint32 count;
    int i;

    reader.head = data;
    reader.end = &reader.head[size];

    ASSERT(beam->static_literals.entries == NULL);

    LoadAssert(beamreader_read_i32(&reader, &count));
    LoadAssert(CHECK_ITEM_COUNT(count, 0, sizeof(literals->entries[0])));

    if (count == 0) {
        /* Empty chunk; nothing to do. */
        return 1;
    }

    entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE, count * sizeof(*entries));

    literals = &beam->static_literals;
    literals->allocated = count;
    literals->count = count;
    literals->entries = entries;

    /* Clear all entries to make sure the destructor doesn't crash if we error
     * out in the loop below. */
    for (i = 0; i < count; i++) {
        entries[i].heap_fragments = NULL;
    }

    heap_size = 0;

    for (i = 0; i < count; i++) {
        const byte *ext_data;
        Sint32 ext_size;
        Sint term_size;

        ErtsHeapFactory factory;
        ErlHeapFragment *fragments;
        Eterm value;

#ifdef DEBUG
        erts_literal_area_t purge_area;
        INITIALIZE_LITERAL_PURGE_AREA(purge_area);
#endif

        LoadAssert(beamreader_read_i32(&reader, &ext_size));
        LoadAssert(beamreader_read_bytes(&reader, ext_size, &ext_data));
        term_size = erts_decode_ext_size(ext_data, ext_size);
        LoadAssert(term_size >= 0);

        if (term_size > 0) {
            ErlHeapFragment *literal_frag = new_literal_fragment(term_size);

            erts_factory_heap_frag_init(&factory, literal_frag);
            factory.alloc_type = ERTS_ALC_T_PREPARED_CODE;

            value = erts_decode_ext(&factory, &ext_data, 0);
            erts_factory_close(&factory);

            LoadAssert(!is_non_value(value));
            ASSERT(size_object_litopt(value, &purge_area) > 0);

            heap_size += erts_used_frag_sz(factory.heap_frags);
            fragments = factory.heap_frags;
        } else {
            erts_factory_dummy_init(&factory);
            value = erts_decode_ext(&factory, &ext_data, 0);

            /* erts_decode_ext may return terms that are (or contain) global
             * literals, for instance export funs or the empty tuple. As these
             * are singleton values that belong to everyone, they can safely be
             * returned without being copied into a fragment.
             *
             * (Note that erts_decode_ext_size does not include said term in
             * the decoded size) */
            LoadAssert(!is_non_value(value));
            ASSERT(size_object_litopt(value, &purge_area) == 0);

            fragments = NULL;
        }

        entries[i].heap_fragments = fragments;
        entries[i].value = value;
    }

    literals->heap_size = heap_size;

    return 1;
}

static int parse_literal_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    BeamReader reader;

    Sint32 compressed_size, uncompressed_size;
    uLongf uncompressed_size_z;
    byte *uncompressed_data;
    int success;

    beamreader_init(chunk->data, chunk->size, &reader);
    compressed_size = chunk->size;

    LoadAssert(beamreader_read_i32(&reader, &uncompressed_size));
    LoadAssert(compressed_size >= 0);

    uncompressed_size_z = uncompressed_size;
    uncompressed_data = erts_alloc(ERTS_ALC_T_TMP, uncompressed_size);
    success = 0;

    if (erl_zlib_uncompress(uncompressed_data,
                            &uncompressed_size_z,
                            reader.head,
                            compressed_size) == Z_OK) {
        success = parse_decompressed_literals(beam,
                                              uncompressed_data,
                                              uncompressed_size_z);
    }

    erts_free(ERTS_ALC_T_TMP, (void*)uncompressed_data);
    return success;
}

static int parse_code_chunk(BeamFile *beam, IFF_Chunk *chunk) {
    Sint32 head_size, version;
    BeamReader reader;

    beamreader_init(chunk->data, chunk->size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &head_size));
    LoadAssert(beamreader_test(&reader, head_size));

    /* Everything after the code header is code. */
    beam->code.data = &reader.head[head_size];
    beam->code.size = (reader.end - reader.head) - head_size;

    reader.end = beam->code.data;

    LoadAssert(beamreader_read_i32(&reader, &version));
    LoadAssert(version == BEAM_FORMAT_NUMBER);

    LoadAssert(beamreader_read_i32(&reader, &beam->code.max_opcode));
    LoadAssert(beamreader_read_i32(&reader, &beam->code.label_count));
    LoadAssert(beamreader_read_i32(&reader, &beam->code.function_count));

    return 1;
}

static int read_beam_chunks(const IFF_File *file,
                            int count, const Uint *ids,
                            IFF_Chunk *chunks) {
    BeamReader reader;
    Sint32 beam_id;
    int i;

    for (i = 0; i < count; i++) {
        chunks[i].iff_id = 0;
        chunks[i].data = NULL;
        chunks[i].size = 0;
    }

    beamreader_init(file->data, file->size, &reader);
    LoadAssert(beamreader_read_i32(&reader, &beam_id));

    if (beam_id != MakeIffId('B', 'E', 'A', 'M')) {
        LoadError("not a BEAM file: IFF form type is not 'BEAM'");
    }

    while (!beamreader_eof(&reader)) {
        Sint32 chunk_id, chunk_size;
        const byte *chunk_data;
        Sint32 storage_size;

        LoadAssert(beamreader_read_i32(&reader, &chunk_id));
        LoadAssert(beamreader_read_i32(&reader, &chunk_size));

        /* Chunk storage is aligned to the next 32-bit boundary, so we'll need
         * to skip (but not read) a few extra bytes if we come up short. */
        storage_size = (chunk_size + 3) & ~3;
        LoadAssert(beamreader_test(&reader, storage_size));

        LoadAssert(beamreader_read_bytes(&reader, chunk_size, &chunk_data));
        reader.head += storage_size - chunk_size;

        for (i = 0; i < count; i++) {
            if (ids[i] == chunk_id) {
                chunks[i].iff_id = chunk_id;
                chunks[i].data = chunk_data;
                chunks[i].size = chunk_size;

                break;
            }
        }
    }

    return 1;
}

enum beamfile_read_result
beamfile_read(const byte *data, size_t size, BeamFile *beam) {
    static const Uint chunk_iffs[] = {
        MakeIffId('A', 't', 'U', '8'), /* 0 */
        MakeIffId('C', 'o', 'd', 'e'), /* 1 */
        MakeIffId('S', 't', 'r', 'T'), /* 2 */
        MakeIffId('I', 'm', 'p', 'T'), /* 3 */
        MakeIffId('E', 'x', 'p', 'T'), /* 4 */
        MakeIffId('F', 'u', 'n', 'T'), /* 5 */
        MakeIffId('L', 'i', 't', 'T'), /* 6 */
        MakeIffId('A', 't', 't', 'r'), /* 7 */
        MakeIffId('C', 'I', 'n', 'f'), /* 8 */
        MakeIffId('L', 'i', 'n', 'e'), /* 9 */
        MakeIffId('L', 'o', 'c', 'T'), /* 10 */
        MakeIffId('A', 't', 'o', 'm'), /* 11 */
        MakeIffId('T', 'y', 'p', 'e'), /* 12 */
        MakeIffId('M', 'e', 't', 'a'), /* 13 */
    };

    static const int UTF8_ATOM_CHUNK = 0;
    static const int CODE_CHUNK = 1;
    static const int STR_CHUNK = 2;
    static const int IMP_CHUNK = 3;
    static const int EXP_CHUNK = 4;
    static const int LAMBDA_CHUNK = 5;
    static const int LITERAL_CHUNK = 6;
    static const int ATTR_CHUNK = 7;
    static const int COMPILE_CHUNK = 8;
    static const int LINE_CHUNK = 9;
#ifdef BEAMASM
    static const int LOC_CHUNK = 10;
#endif
    static const int OBSOLETE_ATOM_CHUNK = 11;
    static const int TYPE_CHUNK = 12;
    static const int META_CHUNK = 13;

    static const int NUM_CHUNKS = sizeof(chunk_iffs) / sizeof(chunk_iffs[0]);

    enum beamfile_read_result error;

    /* MSVC doesn't like the use of NUM_CHUNKS here */
    IFF_Chunk chunks[sizeof(chunk_iffs) / sizeof(chunk_iffs[0])];

    sys_memset(beam, 0, sizeof(*beam));

    if (!iff_init(data, size, &beam->iff)) {
        error = BEAMFILE_READ_CORRUPT_FILE_HEADER;
        goto error;
    }

    if (!read_beam_chunks(&beam->iff, NUM_CHUNKS, chunk_iffs, chunks)) {
        error = BEAMFILE_READ_CORRUPT_FILE_HEADER;
        goto error;
    }

    if (chunks[CODE_CHUNK].size == 0) {
        error = BEAMFILE_READ_MISSING_CODE_CHUNK;
        goto error;
    } else if (!parse_code_chunk(beam, &chunks[CODE_CHUNK])) {
        error = BEAMFILE_READ_CORRUPT_CODE_CHUNK;
        goto error;
    }

    if (chunks[UTF8_ATOM_CHUNK].size == 0) {
        if (chunks[OBSOLETE_ATOM_CHUNK].size == 0) {
            /* Old atom table chunk is also missing. */
            error = BEAMFILE_READ_MISSING_ATOM_TABLE;
        } else {
            /* Old atom table chunk table exists. (OTP 20 or earlier.) */
            error = BEAMFILE_READ_OBSOLETE_ATOM_TABLE;
        }
        goto error;
    } else if (!parse_atom_chunk(beam, &chunks[UTF8_ATOM_CHUNK])) {
        error = BEAMFILE_READ_CORRUPT_ATOM_TABLE;
        goto error;
    }

    if (chunks[IMP_CHUNK].size == 0) {
        error = BEAMFILE_READ_MISSING_IMPORT_TABLE;
        goto error;
    } else if (!parse_import_chunk(beam, &chunks[IMP_CHUNK])) {
        error = BEAMFILE_READ_CORRUPT_IMPORT_TABLE;
        goto error;
    }

    if (chunks[EXP_CHUNK].size == 0) {
        error = BEAMFILE_READ_MISSING_EXPORT_TABLE;
        goto error;
    } else if (!parse_export_chunk(beam, &chunks[EXP_CHUNK])) {
        error = BEAMFILE_READ_CORRUPT_EXPORT_TABLE;
        goto error;
    }

#ifdef BEAMASM
    if (erts_jit_asm_dump && chunks[LOC_CHUNK].size > 0) {
        if (!parse_locals_chunk(beam, &chunks[LOC_CHUNK])) {
            error = BEAMFILE_READ_CORRUPT_LOCALS_TABLE;
            goto error;
        }
    }
#endif

    if (chunks[LAMBDA_CHUNK].size > 0) {
        if (!parse_lambda_chunk(beam, &chunks[LAMBDA_CHUNK])) {
            error = BEAMFILE_READ_CORRUPT_LAMBDA_TABLE;
            goto error;
        }
    }

    if (chunks[LITERAL_CHUNK].size > 0) {
        if (!parse_literal_chunk(beam, &chunks[LITERAL_CHUNK])) {
            error = BEAMFILE_READ_CORRUPT_LITERAL_TABLE;
            goto error;
        }
    }

    if (chunks[LINE_CHUNK].size > 0) {
        if (!parse_line_chunk(beam, &chunks[LINE_CHUNK])) {
            error = BEAMFILE_READ_CORRUPT_LINE_TABLE;
            goto error;
        }
    }

    if (chunks[TYPE_CHUNK].size > 0) {
        if (!parse_type_chunk(beam, &chunks[TYPE_CHUNK])) {
            error = BEAMFILE_READ_CORRUPT_TYPE_TABLE;
            goto error;
        }
    } else {
        init_fallback_type_table(beam);
    }

    beam->strings.data = chunks[STR_CHUNK].data;
    beam->strings.size = chunks[STR_CHUNK].size;

    beam->attributes.data = chunks[ATTR_CHUNK].data;
    beam->attributes.size = chunks[ATTR_CHUNK].size;

    beam->compile_info.data = chunks[COMPILE_CHUNK].data;
    beam->compile_info.size = chunks[COMPILE_CHUNK].size;

    /* Compute module checksum. Please keep all parsing above this section */
    {
        MD5_CTX md5;

        MD5Init(&md5);

        MD5Update(&md5,
                  (byte*)chunks[UTF8_ATOM_CHUNK].data,
                  chunks[UTF8_ATOM_CHUNK].size);
        MD5Update(&md5,
                  (byte*)chunks[CODE_CHUNK].data,
                  chunks[CODE_CHUNK].size);
        MD5Update(&md5,
                  (byte*)chunks[STR_CHUNK].data,
                  chunks[STR_CHUNK].size);
        MD5Update(&md5,
                  (byte*)chunks[IMP_CHUNK].data,
                  chunks[IMP_CHUNK].size);
        MD5Update(&md5,
                  (byte*)chunks[EXP_CHUNK].data,
                  chunks[EXP_CHUNK].size);

        if (chunks[LAMBDA_CHUNK].size > 0) {
            const byte* start = chunks[LAMBDA_CHUNK].data;
            Sint32 left = chunks[LAMBDA_CHUNK].size;

            /* The `OldUniq` field must be ignored when computing the module
            * checksum hash, as it's derived using a (broken and superseded)
            * endian-dependent hash function. */
            if (left >= 4) {
                MD5Update(&md5, (byte*)start, 4);

                start += 4;
                left -= 4;

                while (left >= 24) {
                    static byte zero[4] = {0, 0, 0, 0};

                    /* Include: Function Arity Index NumFree */
                    MD5Update(&md5, (byte*)start, 20);
                    /* Set to zero: OldUniq */
                    MD5Update(&md5, (byte*)zero, 4);

                    start += 24;
                    left -= 24;
                }
            }

            if (left > 0) {
                error = BEAMFILE_READ_CORRUPT_LAMBDA_TABLE;
                goto error;
            }
        }

        if (chunks[LITERAL_CHUNK].size > 0) {
            MD5Update(&md5,
                      (byte*)chunks[LITERAL_CHUNK].data,
                      chunks[LITERAL_CHUNK].size);
        }

        if (chunks[META_CHUNK].size > 0) {
            MD5Update(&md5,
                      (byte*)chunks[META_CHUNK].data,
                      chunks[META_CHUNK].size);
        }

        MD5Final(beam->checksum, &md5);
    }

    return BEAMFILE_READ_SUCCESS;

error:
    ASSERT(error != BEAMFILE_READ_SUCCESS);
    beamfile_free(beam);
    return error;
}

static void beamfile_literal_dtor(BeamFile_LiteralTable *literals) {
    int i;

    for (i = 0; i < literals->count; i++) {
        free_literal_fragment(literals->entries[i].heap_fragments);
    }

    erts_free(ERTS_ALC_T_PREPARED_CODE, literals->entries);
    literals->entries = NULL;
}

void beamfile_free(BeamFile *beam) {
    if (beam->atoms.entries) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->atoms.entries);
        beam->atoms.entries = NULL;
    }

    if (beam->imports.entries) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->imports.entries);
        beam->imports.entries = NULL;
    }

    if (beam->exports.entries) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->exports.entries);
        beam->exports.entries = NULL;
    }

#ifdef BEAMASM
    if (beam->locals.entries) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->locals.entries);
        beam->locals.entries = NULL;
    }
#endif

    if (beam->lambdas.entries) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->lambdas.entries);
        beam->lambdas.entries = NULL;
    }

    if (beam->lines.items) {
        ASSERT(beam->lines.names);

        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->lines.items);
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->lines.names);

        beam->lines.items = NULL;
        beam->lines.names = NULL;
    }

    if (beam->types.entries) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, beam->types.entries);
        beam->types.entries = NULL;
    }

    if (beam->static_literals.entries) {
        beamfile_literal_dtor(&beam->static_literals);
    }

    if (beam->dynamic_literals.entries) {
        beamfile_literal_dtor(&beam->dynamic_literals);
    }
}

Sint beamfile_add_literal(BeamFile *beam, Eterm term, int deduplicate) {
    BeamFile_LiteralTable *literals;
    BeamFile_LiteralEntry *entries;

    ErlHeapFragment *fragment;
    Uint term_size;
    Eterm *hp;
    Sint i;

    literals = &beam->dynamic_literals;
    entries = literals->entries;

    if (entries != NULL) {
        if (deduplicate) {
            /* Return a matching index if this literal already exists.
             * We search backwards since duplicates tend to be used close to
             * one another, and skip searching static literals as the chances
             * of overlap are pretty slim. */
            for (i = literals->count - 1; i >= 0; i--) {
                if (EQ(term, entries[i].value)) {
                    /* Dynamic literal indexes are negative, starting at -1 */
                    return ~i;
                }
            }
        }

        if (literals->count == literals->allocated) {
            literals->allocated *= 2;

            entries = erts_realloc(ERTS_ALC_T_PREPARED_CODE, entries,
                                   literals->allocated * sizeof(*entries));

            literals->entries = entries;
        }
    } else {
        literals->allocated = 32;

        entries = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                             literals->allocated * sizeof(*entries));

        literals->entries = entries;
    }

    term_size = size_object(term);
    literals->heap_size += term_size;

    fragment = new_literal_fragment(term_size);
    hp = fragment->mem;

    i = literals->count;
    literals->count++;

    entries[i].heap_fragments = fragment;
    entries[i].value = copy_struct(term, term_size, &hp, &fragment->off_heap);

    /* Dynamic literal indexes are negative, starting at -1 */
    return ~i;
}

Eterm beamfile_get_literal(const BeamFile *beam, Sint index) {
    const BeamFile_LiteralTable *literals;

    if (index >= 0) {
        literals = &beam->static_literals;
        if (index < literals->count) {
            return literals->entries[index].value;
        }
    } else {
        /* Dynamic literal indexes are negative to more easily keep them apart
         * from static ones. */
        index = ~index;

        literals = &beam->dynamic_literals;
        if (index < literals->count) {
            return literals->entries[index].value;
        }
    }

    /* Static literal indexes are checked when parsing operations, and dynamic
     * literals are managed by us, so we can only land here through a bug. */
    ERTS_INTERNAL_ERROR("illegal literal index");
}

static void move_literal_entries(BeamFile_LiteralEntry *entries, int count,
                                 Eterm **hpp, ErlOffHeap *oh) {
    int i;

    for (i = 0; i < count; i++) {
        if (entries[i].heap_fragments != NULL) {
            Eterm value = entries[i].value;

#ifdef DEBUG
            erts_literal_area_t purge_area;
            INITIALIZE_LITERAL_PURGE_AREA(purge_area);
#endif

            ASSERT(size_object_litopt(value, &purge_area) > 0);

            erts_move_multi_frags(hpp, oh,
                                  entries[i].heap_fragments, &value,
                                  1, 1);
            ASSERT(erts_is_literal(value, ptr_val(value)));

            free_literal_fragment(entries[i].heap_fragments);
            entries[i].heap_fragments = NULL;
            entries[i].value = value;
        }

        ASSERT(entries[i].heap_fragments == NULL);
    }
}

void beamfile_move_literals(BeamFile *beam, Eterm **hpp, ErlOffHeap *oh) {
    BeamFile_LiteralTable *literals;

    literals = &beam->static_literals;
    move_literal_entries(literals->entries, literals->count, hpp, oh);

    literals = &beam->dynamic_literals;
    move_literal_entries(literals->entries, literals->count, hpp, oh);
}

int iff_init(const byte *data, size_t size, IFF_File *iff) {
    Sint32 form_size, form_id;
    const byte *real_data;
    BeamReader reader;

    sys_memset(iff, 0, sizeof(*iff));
    beamreader_init(data, size, &reader);

    LoadAssert(beamreader_read_i32(&reader, &form_id));
    LoadAssert(form_id == MakeIffId('F', 'O', 'R', '1'));

    LoadAssert(beamreader_read_i32(&reader, &form_size));
    LoadAssert(beamreader_read_bytes(&reader, form_size, &real_data));

    iff->data = real_data;
    iff->size = form_size;

    return 1;
}

int iff_read_chunk(IFF_File *iff, Uint id, IFF_Chunk *chunk)
{
    return read_beam_chunks(iff, 1, &id, chunk);
}

void beamfile_init(void) {
    Eterm suffix;
    Eterm *hp;

    hp = erts_alloc_global_literal(ERTS_LIT_ERL_FILE_SUFFIX, 8);
    suffix = erts_bin_bytes_to_list(NIL, hp, (byte*)".erl", 4, 0);

    erts_register_global_literal(ERTS_LIT_ERL_FILE_SUFFIX, suffix);
}

/* * * * * * * */

void beamopallocator_init(BeamOpAllocator *allocator) {
    sys_memset(allocator, 0, sizeof(*allocator));
}

void beamopallocator_expand__(BeamOpAllocator *allocator) {
    BeamOpBlock *block;
    int count, i;

    block = erts_alloc(ERTS_ALC_T_LOADER_TMP, sizeof(*block));
    count = sizeof(block->ops) / sizeof(block->ops[0]);

    block->next = allocator->beamop_blocks;
    allocator->beamop_blocks = block;

    for (i = 0; i < count - 1; i++) {
        block->ops[i].next = &block->ops[i + 1];
    }

    block->ops[i].next = NULL;
    allocator->free = block->ops;
}

void beamopallocator_dtor(BeamOpAllocator *allocator) {
    BeamOpBlock *block = allocator->beamop_blocks;

    while (block != NULL) {
        BeamOpBlock *next_block = block->next;
        erts_free(ERTS_ALC_T_LOADER_TMP, block);
        block = next_block;
    }

    allocator->beamop_blocks = 0;
}

BeamCodeReader *beamfile_get_code(BeamFile *beam, BeamOpAllocator *op_alloc) {
    BeamCodeReader *res;

    ASSERT(beam->code.data != NULL && beam->code.size > 0);

    res = erts_alloc(ERTS_ALC_T_PREPARED_CODE, sizeof(BeamCodeReader));

    res->allocator = op_alloc;
    res->file = beam;
    res->pending = NULL;
    res->first = 1;

    beamreader_init(beam->code.data,
                    beam->code.size,
                    &res->reader);

    return res;
}

/* Converts TAG_i to untagged smalls (TAG_i), bignums (TAG_q with literal), or
 * TAG_o if the result can't fit into a bignum. */
static int marshal_integer(BeamCodeReader *code_reader, TaggedNumber *value) {
    ASSERT(value->tag == TAG_i);

    if (value->size == 0) {
        /* Ideally we'd want TAG_i to be tagged integers, just like TAG_a are
         * tagged atoms rather than table indexes, but it would make the
         * transformation engine far more complicated so we'll tag them when
         * emitting instructions instead. */
        ASSERT(IS_SSMALL(value->word_value));
        return 1;
    } else {
        Eterm default_res_buf[128/sizeof(Eterm)];
        byte default_conv_buf[128];
        byte* conv_buf;
        Eterm* res_buf;
        Eterm term;

        int size, is_negative;
        const byte *source;
        int i;

        source = value->ptr_value;
        size = value->size;

        if (size < sizeof(default_conv_buf) - sizeof(Eterm)) {
            res_buf = default_res_buf;
            conv_buf = default_conv_buf;
        } else {
            conv_buf = erts_alloc(ERTS_ALC_T_LOADER_TMP,
                                  size + sizeof(Eterm));
            res_buf = erts_alloc(ERTS_ALC_T_LOADER_TMP,
                                 size + sizeof(Eterm));
        }

        /* Copy the number in reverse order to the conversion buffer */
        for (i = 0; i < size; i++) {
            conv_buf[size - i - 1] = *source++;
        }

        /* Check if the number is negative, and negate it if so.*/
        is_negative = (conv_buf[size - 1] & 0x80) != 0;
        if (is_negative) {
            unsigned carry = 1;

            for (i = 0; i < size; i++) {
                conv_buf[i] = ~conv_buf[i] + carry;
                carry = (conv_buf[i] == 0 && carry == 1);
            }

            ASSERT(carry == 0);
        }

        /* Cut away leading zero, if any. */
        if (conv_buf[size - 1] == 0) {
            size--;

            if (conv_buf[size - 1] == 0) {
                /* Number's not normalized; the file is most likely corrupt. */

                if (conv_buf != default_conv_buf) {
                    erts_free(ERTS_ALC_T_LOADER_TMP, conv_buf);
                }

                if (res_buf != default_res_buf) {
                    erts_free(ERTS_ALC_T_LOADER_TMP, res_buf);
                }

                return 0;
            }
        }

        /* Align to nearest word boundary. */
        while (size % sizeof(Eterm) != 0) {
            conv_buf[size++] = 0;
        }

        term = bytes_to_big(conv_buf, size, is_negative, res_buf);
        if (is_not_nil(term)) {
            value->tag = TAG_q;
            value->size = 0;

            value->word_value = beamfile_add_literal(code_reader->file,
                                                     term, 1);
        } else {
            /* Result doesn't fit into a bignum. */
            value->tag = TAG_o;
        }

        if (conv_buf != default_conv_buf) {
            erts_free(ERTS_ALC_T_LOADER_TMP, conv_buf);
        }

        if (res_buf != default_res_buf) {
            erts_free(ERTS_ALC_T_LOADER_TMP, res_buf);
        }

        return 1;
    }
}

static int marshal_allocation_list(BeamReader *reader, Sint *res) {
    TaggedNumber count;
    Sint sum;
    int i;

    LoadAssert(beamreader_read_tagged(reader, &count));
    LoadAssert(count.tag == TAG_u);
    LoadAssert(count.word_value <= 3);

    sum = 0;
    for (i = 0; i < count.word_value; i++) {
        int kind, number;
        TaggedNumber val;

        LoadAssert(beamreader_read_tagged(reader, &val));
        LoadAssert(val.tag == TAG_u);
        LoadAssert(val.word_value <= ERTS_SINT32_MAX);
        kind = val.word_value;

        LoadAssert(beamreader_read_tagged(reader, &val));
        LoadAssert(val.tag == TAG_u);
        LoadAssert(val.word_value <= ERTS_SINT32_MAX /
                   MAX(FLOAT_SIZE_OBJECT, ERL_FUN_SIZE + 1));
        number = val.word_value;

        switch(kind) {
        case 0:
            LoadAssert(sum <= (ERTS_SINT32_MAX - number));
            sum += number;
            break;
        case 1:
            LoadAssert(sum <= (ERTS_SINT32_MAX - FLOAT_SIZE_OBJECT * number));
            sum += FLOAT_SIZE_OBJECT * number;
            break;
        case 2:
            LoadAssert(sum <= (ERTS_SINT32_MAX - (ERL_FUN_SIZE + 1) * number));

            /* This is always a local fun, so we need to add one word to
             * reserve space for its `FunRef`. */
            sum += (ERL_FUN_SIZE + 1) * number;
            break;
        default:
            LoadError("Invalid allocation tag");
        }
    }

    *res = sum;

    return 1;
}

static int beamcodereader_read_next(BeamCodeReader *code_reader, BeamOp **out) {
    BeamOp *op;
    int arity, i;
    byte opcode;
    BeamReader *reader;

    reader = &code_reader->reader;

    LoadAssert(beamreader_read_u8(reader, &opcode));
    LoadAssert(opcode > 0 && opcode <= MAX_GENERIC_OPCODE);

    arity = gen_opc[opcode].arity;
    ASSERT(arity <= ERTS_BEAM_MAX_OPARGS);

    op = beamopallocator_new_op(code_reader->allocator);
    op->op = opcode;
    op->arity = arity;

    for (i = 0; i < arity; i++) {
        TaggedNumber raw_arg;

        LoadAssert(beamreader_read_tagged(reader, &raw_arg));

        switch(raw_arg.tag) {
        case TAG_u:
        case TAG_q:
        case TAG_o:
            break;
        case TAG_a:
            if (raw_arg.word_value == 0) {
                raw_arg.tag = TAG_n;
                raw_arg.word_value = NIL;
            } else {
                BeamFile_AtomTable *atoms = &(code_reader->file)->atoms;
                LoadAssert(raw_arg.word_value < atoms->count);
                raw_arg.word_value = atoms->entries[raw_arg.word_value];
            }
            break;
        case TAG_f:
            if (raw_arg.word_value == 0) {
                raw_arg.tag = TAG_p;
            } else {
                BeamFile_Code *code = &(code_reader->file)->code;
                LoadAssert(raw_arg.word_value < code->label_count);
            }
            break;
        case TAG_i:
            LoadAssert(marshal_integer(code_reader, &raw_arg));
            break;
        case TAG_x:
        case TAG_y:
            LoadAssert(raw_arg.word_value < MAX_REG);
            break;
        case TAG_z:
            /* Extended tags */

            switch (raw_arg.word_value) {
            case 0:
                LoadError("Floating point number superseded in OTP R16B");
                break;
            case 1:
                /* Argument list */
                {
                    TaggedNumber extra_args;

                    /* Lists may only be the last argument. */
                    LoadAssert((i + 1) == arity);

                    LoadAssert(beamreader_read_tagged(reader, &extra_args));
                    LoadAssert(extra_args.tag == TAG_u);

                    arity += extra_args.word_value;
                    op->arity = arity;

                    /* Expand the argument array if necessary. */
                    if (arity > ERTS_BEAM_MAX_OPARGS) {
                        LoadAssert(op->a == op->def_args);

                        op->a = erts_alloc(ERTS_ALC_T_LOADER_TMP,
                                           arity * sizeof(op->a[0]));
                        sys_memcpy(op->a, op->def_args, i * sizeof(op->a[0]));
                    }

                    raw_arg.tag = TAG_u;
                    raw_arg.word_value = extra_args.word_value;

                    break;
                }
            case 2:
                /* Float register */
                {
                    TaggedNumber index;

                    LoadAssert(beamreader_read_tagged(reader, &index));
                    LoadAssert(index.tag == TAG_u);
                    LoadAssert(index.word_value < MAX_REG);

                    raw_arg.tag = TAG_l;
                    raw_arg.word_value = index.word_value;

                    break;
                }
            case 3:
                /* Allocation list */
                {
                    Sint heap_words;

                    LoadAssert(marshal_allocation_list(reader, &heap_words));

                    raw_arg.tag = TAG_u;
                    raw_arg.word_value = heap_words;

                    break;
                }
            case 4:
                /* Literal */
                {
                    const BeamFile_LiteralTable *literals;
                    TaggedNumber index;

                    LoadAssert(beamreader_read_tagged(reader, &index));
                    LoadAssert(index.tag == TAG_u);

                    /* The referenced literal must be defined in the file, it
                     * must not have been created at runtime. */
                    literals = &(code_reader->file)->static_literals;
                    LoadAssert(index.word_value < literals->count);

                    raw_arg.tag = TAG_q;
                    raw_arg.word_value = index.word_value;

                    break;
                }
            case 5:
                /* Register with type hint */
                {
                    const BeamFile_TypeTable *types;
                    TaggedNumber index;

                    LoadAssert(beamreader_read_tagged(reader, &raw_arg));
                    LoadAssert(raw_arg.tag == TAG_x || raw_arg.tag == TAG_y);

                    LoadAssert(beamreader_read_tagged(reader, &index));
                    LoadAssert(index.tag == TAG_u);

                    types = &(code_reader->file)->types;

                    /* We may land here without a table if it was stripped
                     * after compilation, in which case we want to treat these
                     * as ordinary registers. */
                    if (!types->fallback) {
                        LoadAssert(index.word_value < types->count);

                        ERTS_CT_ASSERT(REG_MASK < (1 << 10));
                        raw_arg.word_value |= index.word_value << 10;
                    }
                    break;
                }
            default:
                LoadError("Unrecognized extended tag");
            }
            break;
        default:
            LoadError("Unrecognized tag");
        }

        /* All arguments must be words except for TAG_o, which is valueless. */
        LoadAssert(raw_arg.size == 0 || raw_arg.tag == TAG_o);

        /* Fill in current argument. */
        op->a[i].type = raw_arg.tag;
        op->a[i].val = raw_arg.word_value;
    }

    *out = op;

    return 1;
}

static void synthesize_func_end(BeamCodeReader *code_reader) {
    BeamOp *func_end;

    func_end = beamopallocator_new_op(code_reader->allocator);
    func_end->op = genop_int_func_end_2;
    func_end->arity = 2;

    ASSERT(code_reader->current_func_label.type == TAG_u);
    func_end->a[0].val = code_reader->current_func_label.val;
    func_end->a[0].type = TAG_f;

    ASSERT(code_reader->current_entry_label.type == TAG_u);
    func_end->a[1].val = code_reader->current_entry_label.val;
    func_end->a[1].type = TAG_f;

    func_end->next = code_reader->pending;
    code_reader->pending = func_end;
}

int beamcodereader_next(BeamCodeReader *code_reader, BeamOp **out) {
    BeamOp *op;

    if (code_reader->pending) {
        op = code_reader->pending;
        code_reader->pending = op->next;

        *out = op;
        return 1;
    }

    LoadAssert(beamcodereader_read_next(code_reader, &op));

    switch (op->op) {
    case genop_label_1:
        /* To simplify the rest of the loading process, attempt to synthesize
         * int_func_start/5 and int_func_end/2 instructions.
         *
         * We look for the following instruction sequence to
         * find function boundaries: label Lbl | line Loc | func_info M F A.
         * (Where the line instruction is optional.)
         *
         * So far we have seen a label/0 instruction. Put this instruction into
         * the pending queue and decode the next instruction. */
        code_reader->pending = op;
        LoadAssert(beamcodereader_read_next(code_reader, &op->next));
        op = op->next;

        /* If the current instruction is a line instruction, append it to
         * the pending queue and decode the following instruction. */
        if (op->op == genop_line_1) {
            LoadAssert(beamcodereader_read_next(code_reader, &op->next));
            op = op->next;
        }

        /* The code must not end abruptly after a label. */
        LoadAssert(op->op != genop_int_code_end_0);

        /* If the current instruction is a func_info instruction, we
         * have found a function boundary. */
        if (ERTS_LIKELY(op->op != genop_func_info_3)) {
            op->next = NULL;
        } else {
            BeamOpArg func_label, entry_label;
            BeamOp *func_start;
            BeamOp *entry_op;
            BeamOp *next;

            /* The func_info/3 instruction must be followed by its entry
             * label. */
            LoadAssert(beamcodereader_read_next(code_reader, &entry_op));
            LoadAssert(entry_op->op == genop_label_1);
            entry_label = entry_op->a[0];
            LoadAssert(entry_label.type == TAG_u);
            entry_op->next = NULL;

            /* Prepare to walk through the queue of pending instructions. */
            op = code_reader->pending;

            /* Pick up the label from the first label/1 instruction. */
            ASSERT(op->op == genop_label_1);
            func_label = op->a[0];
            LoadAssert(func_label.type == TAG_u);

            next = op->next;
            beamopallocator_free_op(code_reader->allocator, op);
            op = next;

            /* Allocate the int_func_start/0 function. */
            func_start = beamopallocator_new_op(code_reader->allocator);
            func_start->op = genop_int_func_start_5;
            func_start->arity = 5;
            func_start->a[0] = func_label;

            /* If the current instruction is a line/1 instruction, pick up the
             * location from that instruction. Otherwise use NIL. */
            func_start->a[1].type = TAG_n;
            if (op->op == genop_line_1) {
                func_start->a[1] = op->a[0];
                next = op->next;
                beamopallocator_free_op(code_reader->allocator, op);
                op = next;
            }

            /* Pick up the MFA from the func_info/3 instruction. */
            ASSERT(op->op == genop_func_info_3);
            func_start->a[2] = op->a[0];
            func_start->a[3] = op->a[1];
            func_start->a[4] = op->a[2];
            beamopallocator_free_op(code_reader->allocator, op);

            /* Put the int_func_start/5 instruction into the pending queue,
             * and link the entry label after it. */
            code_reader->pending = func_start;
            func_start->next = entry_op;

            /* Unless this is the first function in the module, synthesize an
             * int_func_end/2 instruction and prepend it to the pending
             * queue. */
            if (!code_reader->first) {
                synthesize_func_end(code_reader);
            }

            code_reader->current_func_label = func_label;
            code_reader->current_entry_label = entry_label;
            code_reader->first = 0;
        }

        /* At this point, there is at least one instruction in the pending
         * queue, and the op variable points to the last instruction in the
         * queue. */
        return beamcodereader_next(code_reader, out);
    case genop_int_code_end_0:
        code_reader->pending = op;

        if (!code_reader->first) {
            synthesize_func_end(code_reader);
        }

        op->next = NULL;
        return beamcodereader_next(code_reader, out);
    default:
        *out = op;
        return 1;
    }
}

void beamcodereader_close(BeamCodeReader *reader) {
    erts_free(ERTS_ALC_T_PREPARED_CODE, reader);
}
