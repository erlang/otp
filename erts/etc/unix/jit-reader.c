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

/* #define HARD_DEBUG */

typedef struct range {
    GDB_CORE_ADDR start;
    GDB_CORE_ADDR end;
} range;

typedef struct priv {
    range *ranges;
    int num_ranges;
} priv;

static enum gdb_status read_debug_info(struct gdb_reader_funcs *self,
                                       struct gdb_symbol_callbacks *cb,
                                       void *memory, long memory_sz) {
    priv *priv = self->priv_data;
    uint64_t num_functions = *(uint64_t*)memory;
    GDB_CORE_ADDR mod_start  = *(GDB_CORE_ADDR *)(memory + sizeof(uint64_t));
    GDB_CORE_ADDR mod_end  = mod_start + *(uint64_t*)(memory + sizeof(uint64_t)*2);
    char* module = memory + sizeof(uint64_t)*3;
    char* curr = module + strlen(module) + 1;
    int i;
    struct gdb_object *obj = cb->object_open(cb);

    priv->ranges = realloc(priv->ranges, sizeof(range) * ++priv->num_ranges);
    priv->ranges[priv->num_ranges-1].start = mod_start;
    priv->ranges[priv->num_ranges-1].end = mod_end;

#ifdef HARD_DEBUG
   fprintf(stderr,"Add module %s (%lx, %lx)\r\n", module, mod_start, mod_end);
#endif

    for (i = 0; i < num_functions; i++) {
        // get begin and end of code segment
        // A bug in GDB < 9 forces us to open and close the symtab for each iteration
        struct gdb_symtab *symtab = cb->symtab_open(cb, obj, module);
        GDB_CORE_ADDR begin = *(GDB_CORE_ADDR*)curr;
        GDB_CORE_ADDR end = *(GDB_CORE_ADDR*)(curr + sizeof(GDB_CORE_ADDR));
        // get name of function
        const char *name = (const char*)(curr + sizeof(GDB_CORE_ADDR) * 2);
        curr += strlen(name) + 1 + sizeof(GDB_CORE_ADDR) * 2;

#ifdef HARD_DEBUG
        fprintf(stderr,"Add %s (%lx, %lx)\r\n", name, begin, end);
#endif

        // returned value has no use
        cb->block_open(cb, symtab, NULL, begin, end, name);
        cb->symtab_close(cb, symtab);
    }

    cb->object_close(cb, obj);
    return GDB_SUCCESS;
}

static void regfree(struct gdb_reg_value *reg) {
    free(reg);
}

static enum gdb_status unwind(struct gdb_reader_funcs *self, struct gdb_unwind_callbacks *cb) {
    int i;
    priv *priv = self->priv_data;
    GDB_CORE_ADDR rip = *(GDB_CORE_ADDR*)cb->reg_get(cb,16)->value;
#ifdef HARD_DEBUG
    GDB_CORE_ADDR rsp = *(GDB_CORE_ADDR*)cb->reg_get(cb,7)->value;
#endif
    GDB_CORE_ADDR rbp = *(GDB_CORE_ADDR*)cb->reg_get(cb,6)->value;

    /* Check that rip points to one of the addresses that we handle */
    for (i = 0; i < priv->num_ranges; i++) {
        if (rip >= priv->ranges[i].start && rip < priv->ranges[i].end) {
            struct gdb_reg_value *prev_rsp = malloc(sizeof(struct gdb_reg_value) + sizeof(char*)),
                *prev_rip = malloc(sizeof(struct gdb_reg_value) + sizeof(char*)),
                *prev_rbp = malloc(sizeof(struct gdb_reg_value) + sizeof(char*));

#ifdef HARD_DEBUG
            fprintf(stderr,"UNWIND match: rip: %lx rsp: %lx rbp: %lx \r\n", rip, rsp, rbp);
#endif
            /* We use the normal frame-pointer logic to unwind the stack, which means
               that rbp will point to where we stored the previous frames rbp. Also
               the previous frames address will be at rbp + 1 and the previous frames
               rsp will be rbp + 2.

               0x00:                <- prev_rsp
               0x80: prev call addr
               0x10: prev rbp       <- curr rbp
               0x18: current frame
               0x20:                <- curr rip

            */

            prev_rsp->free = &regfree;
            prev_rsp->defined = 1;
            prev_rsp->size = sizeof(char*);
            ((GDB_CORE_ADDR*)prev_rsp->value)[0] = rbp + sizeof(char*) * 2;

            cb->target_read(rbp + 0 * sizeof(char*), &prev_rbp->value, sizeof(char*));
            prev_rbp->free = &regfree;
            prev_rbp->defined = 1;
            prev_rbp->size = sizeof(char*);

            cb->target_read(rbp + 1 * sizeof(char*), &prev_rip->value, sizeof(char*));
            prev_rip->free = &regfree;
            prev_rip->defined = 1;
            prev_rip->size = sizeof(char*);

#ifdef HARD_DEBUG
            fprintf(stderr,"UNWIND prev: rip: %lx rsp: %lx rbp: %lx\r\n",
                    *(GDB_CORE_ADDR*)prev_rip->value,
                    *(GDB_CORE_ADDR*)prev_rsp->value,
                    *(GDB_CORE_ADDR*)prev_rbp->value);
#endif

            cb->reg_set(cb, 16, prev_rip);
            cb->reg_set(cb, 7, prev_rsp);
            cb->reg_set(cb, 6, prev_rbp);
            return GDB_SUCCESS;
        }
    }
#ifdef HARD_DEBUG
    fprintf(stderr,"UNWIND no match: rip: %lx rsp: %lx rbp: %lx\r\n", rip, rsp, rbp);
#endif
    return GDB_FAIL;
}

static struct gdb_frame_id get_frame_id(struct gdb_reader_funcs *self, struct gdb_unwind_callbacks *cb){
    int i;
    priv *priv = self->priv_data;
    struct gdb_frame_id frame = {0, 0};
    GDB_CORE_ADDR rip = *(GDB_CORE_ADDR*)cb->reg_get(cb,16)->value;
    GDB_CORE_ADDR rbp = *(GDB_CORE_ADDR*)cb->reg_get(cb,6)->value;
#ifdef HARD_DEBUG
    GDB_CORE_ADDR rsp = *(GDB_CORE_ADDR*)cb->reg_get(cb,7)->value;
    fprintf(stderr,"FRAME: rip: %lx rsp: %lx rbp: %lx \r\n", rip, rsp, rbp);
#endif
    for (i = 0; i < priv->num_ranges; i++) {
        if (rip >= priv->ranges[i].start && rip < priv->ranges[i].end) {
            frame.code_address = priv->ranges[i].start;
            frame.stack_address = rbp + sizeof(char*);
        }
    }
    return frame;
}

static void destroy(struct gdb_reader_funcs *self){
    free(self);
}

struct gdb_reader_funcs *gdb_init_reader(void){
    struct gdb_reader_funcs *funcs = malloc(sizeof(struct gdb_reader_funcs));
    priv *priv = malloc(sizeof(priv));
    priv->num_ranges = 1;
    priv->ranges = malloc(sizeof(range));
    priv->ranges[0].start = 0;
    priv->ranges[0].end = 0;

    funcs->reader_version = GDB_READER_INTERFACE_VERSION;
    funcs->priv_data = priv;

    funcs->read = read_debug_info;
    funcs->unwind = unwind;
    funcs->get_frame_id = get_frame_id;
    funcs->destroy = destroy;

    return funcs;
}
