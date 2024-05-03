#include "jit-reader.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

/* Useful links
 * - https://pwparchive.wordpress.com/2011/11/20/new-jit-interface-for-gdb/
 * - https://sourceware.org/gdb/current/onlinedocs/gdb/Custom-Debug-Info.html
 * - https://github.com/tetzank/asmjit-utilities
 * - https://github.com/bminor/binutils-gdb/blob/master/gdb/testsuite/gdb.base/jitreader.c
 */

GDB_DECLARE_GPL_COMPATIBLE_READER

#if 0
#define HARD_DEBUG
static FILE *log = NULL;
#define LOG(...) do { fprintf(log, ##__VA_ARGS__); fflush(log); } while(0)
#else
#define LOG(...)
#endif

typedef enum {
    X64_RBP = 6,  /* Frame pointer iff native frames are enabled */
    X64_RSP = 7,  /* Stack pointer when using native stack */
    X64_R12 = 12, /* Stack pointer when using non-native stack */
    X64_R13 = 13, /* Current process */
    X64_RIP = 16
} X64Register;

typedef enum {
    /* Return address only */
    ERTS_FRAME_LAYOUT_RA,
    /* Frame pointer, return address */
    ERTS_FRAME_LAYOUT_FP_RA
} ErtsFrameLayout;

struct emulator_info {
    /* 0 = regular, 1 = frame pointers */
    int frame_layout;
    const void *normal_exit;
};

struct erlang_module_info {
    uint64_t base_address;
    uint32_t range_count;
    uint32_t code_size;
    /* Module name, including null terminator. */
    uint16_t name_length;
    char name[];
    /* array of range_info structures */
};

struct range_info {
    uint32_t start_offset;
    uint32_t end_offset;
    uint32_t line_count;
    /* Range name, including null terminator. */
    uint16_t name_length;
    char name[];
    /* array of line_info structures */
};

struct line_info {
    uint32_t start_offset;
    uint32_t line_number;
    /* File name, including null terminator. */
    uint16_t file_length;
    char file[];
};

enum debug_info_header {
    DEBUG_INFO_HEADER_EMULATOR = 0,
    DEBUG_INFO_HEADER_MODULE = 1,
};

struct debug_info {
    enum debug_info_header header;
    union {
        struct emulator_info emu;
        struct erlang_module_info mod;
    } payload;
};

typedef struct range {
    GDB_CORE_ADDR start;
    GDB_CORE_ADDR end;
#ifdef HARD_DEBUG
    char *name;
#endif
} range;

typedef struct priv {
    range *ranges;
    int num_ranges;
    ErtsFrameLayout frame_layout;
    const void *normal_exit;
} priv;

static enum gdb_status read_module_info(struct gdb_reader_funcs *self,
                                        struct gdb_symbol_callbacks *cb,
                                        struct erlang_module_info *module_info) {
    struct gdb_object *obj = cb->object_open(cb);
    GDB_CORE_ADDR mod_start, mod_end;
    char *symfile = (char*)module_info;
    priv *priv = self->priv_data;

    symfile += sizeof(*module_info) + module_info->name_length;

    mod_start = module_info->base_address;
    mod_end = mod_start + module_info->code_size;

    priv->ranges = realloc(priv->ranges, (priv->num_ranges + 1) * sizeof(range));
    priv->ranges[priv->num_ranges].start = mod_start;
    priv->ranges[priv->num_ranges].end = mod_end;
#ifdef HARD_DEBUG
    priv->ranges[priv->num_ranges].name = strdup(module_info->name);
#endif
    priv->num_ranges += 1;

    LOG("Add module `%s` (0x%lx, 0x%lx)\r\n",
        module_info->name, mod_start, mod_end);

    for (int range = 0; range < module_info->range_count; range++) {
        struct range_info *range_info;
        struct gdb_symtab *symtab;
        GDB_CORE_ADDR begin, end;

        range_info = (struct range_info *)symfile;
        symfile += sizeof(*range_info) + range_info->name_length;

        begin = mod_start + range_info->start_offset;
        end = mod_start + range_info->end_offset;

        LOG("Add range `%s` (0x%lx, 0x%lx), %u lines\r\n",
            range_info->name,
            begin, end,
            range_info->line_count);

        /* A bug in GDB < 9 forces us to open and close the symtab for each
         * iteration. */
        symtab = cb->symtab_open(cb, obj, module_info->name);
        cb->block_open(cb, symtab, NULL, begin, end, range_info->name);
        cb->symtab_close(cb, symtab);

        for (int line = 0; line < range_info->line_count; line++) {
            struct gdb_line_mapping line_mapping;
            struct line_info *line_info;

            line_info = (struct line_info *)symfile;
            symfile += sizeof(*line_info) + line_info->file_length;

            line_mapping.pc = mod_start + line_info->start_offset;
            line_mapping.line = line_info->line_number;

            LOG("\t%s:%u\r\n", line_info->file, line_info->line_number);

            /* The symbol table must be opened and closed on every single line
             * for file names to work properly, as there is no other way to
             * tell GDB that a certain line belongs to a different file than
             * the rest of the table. Sigh. */
            symtab = cb->symtab_open(cb, obj, line_info->file);

            cb->block_open(cb, symtab, NULL, line_mapping.pc, end,
                           range_info->name);
            cb->line_mapping_add(cb, symtab, 1, &line_mapping);
            cb->symtab_close(cb, symtab);
        }
    }

    cb->object_close(cb, obj);

    return GDB_SUCCESS;
}

static enum gdb_status read_emulator_info(struct gdb_reader_funcs *self,
                                          struct gdb_symbol_callbacks *cb,
                                          struct emulator_info *emulator_info) {
    priv *priv = self->priv_data;

    priv->frame_layout = emulator_info->frame_layout;
    priv->normal_exit = emulator_info->normal_exit;

    LOG("initialize: frame layout = %i\r\n", priv->frame_layout);

    return GDB_SUCCESS;
}

static enum gdb_status read_debug_info(struct gdb_reader_funcs *self,
                                       struct gdb_symbol_callbacks *cb,
                                       void *memory, long memory_sz) {
    struct debug_info *debug_info = memory;

    (void)memory_sz;

    switch (debug_info->header) {
    case DEBUG_INFO_HEADER_EMULATOR:
        return read_emulator_info(self, cb, &debug_info->payload.emu);
    case DEBUG_INFO_HEADER_MODULE:
        return read_module_info(self, cb, &debug_info->payload.mod);
    }

    return GDB_FAIL;
}

static void regfree(struct gdb_reg_value *reg) {
    free(reg);
}

static struct range *get_range(priv *priv, GDB_CORE_ADDR rip) {
    for (int i = 0; i < priv->num_ranges; i++) {
        if (rip >= priv->ranges[i].start && rip < priv->ranges[i].end) {
            return &priv->ranges[i];
        }
    }

    return NULL;
}

static enum gdb_status unwind(struct gdb_reader_funcs *self,
                              struct gdb_unwind_callbacks *cb) {
    GDB_CORE_ADDR rbp, rsp, rip;
    struct range *range;
    priv *priv;

    rbp = *(GDB_CORE_ADDR*)cb->reg_get(cb, X64_RBP)->value;
    rsp = *(GDB_CORE_ADDR*)cb->reg_get(cb, X64_RSP)->value;
    rip = *(GDB_CORE_ADDR*)cb->reg_get(cb, X64_RIP)->value;

    priv = self->priv_data;
    range = get_range(priv, rip);

    /* Check that rip points to one of the addresses that we handle */
    if (range) {
        struct gdb_reg_value *prev_rbp, *prev_rsp, *prev_rip;

        prev_rbp = malloc(sizeof(struct gdb_reg_value) + sizeof(char*));
        prev_rsp = malloc(sizeof(struct gdb_reg_value) + sizeof(char*));
        prev_rip = malloc(sizeof(struct gdb_reg_value) + sizeof(char*));

        LOG("UNWIND match %s: rbp: 0x%lx rsp: 0x%lx rip: 0x%lx  \r\n",
            range->name, rbp, rsp, rip);

        prev_rbp->free = &regfree;
        prev_rbp->defined = 1;
        prev_rbp->size = sizeof(char*);
        prev_rsp->free = &regfree;
        prev_rsp->defined = 1;
        prev_rsp->size = sizeof(char*);
        prev_rip->free = &regfree;
        prev_rip->defined = 1;
        prev_rip->size = sizeof(char*);

        if (priv->frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
            /* Frame pointers are enabled, which means that rbp will point to
             * where we stored the previous frames rbp. Also the previous
             * frames address will be at rbp + 8 and the previous frames rsp
             * will be rbp + 16.
             *
             *  0x00:                <- prev_rsp
             *  0x08: prev call addr
             *  0x10: prev rbp       <- curr rbp
             *  0x18: current frame
             *  0x20:                <- curr rip */
            cb->target_read(rbp + 1 * sizeof(char*), &prev_rip->value,
                            sizeof(char*));
            cb->target_read(rbp + 0 * sizeof(char*), &prev_rbp->value,
                            sizeof(char*));
            *(GDB_CORE_ADDR*)prev_rsp->value = rbp + sizeof(char*[2]);
        } else {
            /* Normal frame layout, we need to scan the stack. */
            cb->target_read(rsp, &prev_rip->value, sizeof(char*));

            for (rsp += sizeof(char*); ; rsp += sizeof(char*)) {
                cb->target_read(rsp, &prev_rip->value, sizeof(char*));

                LOG("rsp: 0x%lx rip: 0x%lx\r\n",
                    rsp, *(GDB_CORE_ADDR*)prev_rip->value);

                /* Check if it is a cp */
                if ((*(GDB_CORE_ADDR*)prev_rip->value & 0x3) == 0) {
                    break;
                }
            }

            *(GDB_CORE_ADDR*)prev_rsp->value = rsp;
            *(GDB_CORE_ADDR*)prev_rbp->value = rsp - sizeof(char*);
        }

        if (*(GDB_CORE_ADDR*)prev_rip->value == (uintptr_t)priv->normal_exit) {
            LOG("Normal exit\r\n");
            *(GDB_CORE_ADDR*)prev_rsp->value = 0;
            *(GDB_CORE_ADDR*)prev_rbp->value = 0;
        } else {
            LOG("UNWIND prev: rbp: 0x%lx rsp: 0x%lx rip: 0x%lx\r\n",
                *(GDB_CORE_ADDR*)prev_rbp->value,
                *(GDB_CORE_ADDR*)prev_rsp->value,
                *(GDB_CORE_ADDR*)prev_rip->value);
        }

        cb->reg_set(cb, X64_RIP, prev_rip);
        cb->reg_set(cb, X64_RSP, prev_rsp);
        cb->reg_set(cb, X64_RBP, prev_rbp);

        return GDB_SUCCESS;
    }

    LOG("UNWIND no match: rbp: 0x%lx rsp: 0x%lx rip: 0x%lx\r\n", rbp, rsp, rip);
    return GDB_FAIL;
}

static struct gdb_frame_id get_frame_id(struct gdb_reader_funcs *self,
                                        struct gdb_unwind_callbacks *cb){
    struct gdb_frame_id frame = {0, 0};
    GDB_CORE_ADDR rbp, rsp, rip;
    struct range *range;
    priv *priv;

    rbp = *(GDB_CORE_ADDR*)cb->reg_get(cb, X64_RBP)->value;
    rsp = *(GDB_CORE_ADDR*)cb->reg_get(cb, X64_RSP)->value;
    rip = *(GDB_CORE_ADDR*)cb->reg_get(cb, X64_RIP)->value;

    priv = self->priv_data;
    range = get_range(priv, rip);

    LOG("FRAME: rip: 0x%lx rsp: 0x%lx rbp: 0x%lx \r\n", rip, rsp, rbp);

    if (range) {
        frame.code_address = rip;

        if (priv->frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
            frame.stack_address = rbp + sizeof(char*);
        } else {
            GDB_CORE_ADDR prev_rip;

            for (rsp += sizeof(char*); ; rsp += sizeof(char*)) {
                cb->target_read(rsp, &prev_rip, sizeof(char*));

                LOG("rsp: 0x%lx rip: 0x%lx\r\n", rsp, prev_rip);

                /* Check if it is a cp */
                if ((prev_rip & 0x3) == 0) {
                    break;
                }
            }

            frame.stack_address = rsp;
        }
    }

    LOG("FRAME: code_address: 0x%lx stack_address: 0x%lx\r\n",
        frame.code_address, frame.stack_address);

    return frame;
}

static void destroy(struct gdb_reader_funcs *self){
    free(self);
}

struct gdb_reader_funcs *gdb_init_reader(void){
    struct gdb_reader_funcs *funcs = malloc(sizeof(struct gdb_reader_funcs));
    priv *priv_data = malloc(sizeof(priv));

    priv_data->num_ranges = 1;
    priv_data->ranges = malloc(sizeof(range));
    priv_data->ranges[0].start = 0;
    priv_data->ranges[0].end = 0;

    funcs->reader_version = GDB_READER_INTERFACE_VERSION;
    funcs->priv_data = priv_data;

    funcs->read = read_debug_info;
    funcs->unwind = unwind;
    funcs->get_frame_id = get_frame_id;
    funcs->destroy = destroy;

#ifdef HARD_DEBUG
    log = fopen("/tmp/jit-reader.log","w+");
    if (!log) fprintf(stderr,"Could not open /tmp/jit-reader.log");
#endif

    return funcs;
}
