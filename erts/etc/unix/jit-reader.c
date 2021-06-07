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


typedef struct range {
    GDB_CORE_ADDR start;
    GDB_CORE_ADDR end;
    int global;
    char *name;
} range;

typedef struct priv {
    range *ranges;
    int num_ranges;
    uint64_t normal_exit;
} priv;


static enum gdb_status read_debug_info(struct gdb_reader_funcs *self,
                                       struct gdb_symbol_callbacks *cb,
                                       void *memory, long memory_sz) {
    priv *priv = self->priv_data;
    uint64_t num_functions = *(uint64_t*)memory;
    if (num_functions == 0) {
        /* Initialize */
        priv->normal_exit = *(uint64_t*)(memory + sizeof(uint64_t));
        LOG("initialize: normal_exit=%p\r\n",(void*)priv->normal_exit);
    } else {
        GDB_CORE_ADDR mod_start  = *(GDB_CORE_ADDR *)(memory + sizeof(uint64_t));
        GDB_CORE_ADDR mod_end  = mod_start + *(uint64_t*)(memory + sizeof(uint64_t)*2);
        char* module = memory + sizeof(uint64_t)*3;
        char* curr = module + strlen(module) + 1;
        int i;
        struct gdb_object *obj = cb->object_open(cb);

        priv->ranges = realloc(priv->ranges, sizeof(range) * ++priv->num_ranges);
        priv->ranges[priv->num_ranges-1].start = mod_start;
        priv->ranges[priv->num_ranges-1].end = mod_end;
        priv->ranges[priv->num_ranges-1].name = strdup(module);
        priv->ranges[priv->num_ranges-1].global = !strcmp(module, "global");

        LOG("Add module %s (0x%lx, 0x%lx)\r\n", module, mod_start, mod_end);

        for (i = 0; i < num_functions; i++) {
            // get begin and end of code segment
            // A bug in GDB < 9 forces us to open and close the symtab for each iteration
            struct gdb_symtab *symtab = cb->symtab_open(cb, obj, module);
            GDB_CORE_ADDR begin = *(GDB_CORE_ADDR*)curr;
            GDB_CORE_ADDR end = *(GDB_CORE_ADDR*)(curr + sizeof(GDB_CORE_ADDR));
            // get name of function
            const char *name = (const char*)(curr + sizeof(GDB_CORE_ADDR) * 2);
            curr += strlen(name) + 1 + sizeof(GDB_CORE_ADDR) * 2;

            LOG("Add %s (0x%lx, 0x%lx)\r\n", name, begin, end);

            // returned value has no use
            cb->block_open(cb, symtab, NULL, begin, end, name);
            cb->symtab_close(cb, symtab);
        }

        cb->object_close(cb, obj);
    }
    return GDB_SUCCESS;
}

static void regfree(struct gdb_reg_value *reg) {
    free(reg);
}

static struct range *get_range(priv *priv, GDB_CORE_ADDR rip) {
    int i;
    for (i = 0; i < priv->num_ranges; i++)
        if (rip >= priv->ranges[i].start && rip < priv->ranges[i].end)
            return &priv->ranges[i];
    return NULL;
}

static enum gdb_status unwind(struct gdb_reader_funcs *self, struct gdb_unwind_callbacks *cb) {
    int i;
    priv *priv = self->priv_data;
    GDB_CORE_ADDR rip = *(GDB_CORE_ADDR*)cb->reg_get(cb,16)->value;
    GDB_CORE_ADDR rsp = *(GDB_CORE_ADDR*)cb->reg_get(cb,7)->value;
    GDB_CORE_ADDR rbp = *(GDB_CORE_ADDR*)cb->reg_get(cb,6)->value;
    struct range *range = get_range(priv, rip);

    /* Check that rip points to one of the addresses that we handle */
    if (range) {
        struct gdb_reg_value *prev_rsp = malloc(sizeof(struct gdb_reg_value) + sizeof(char*)),
            *prev_rip = malloc(sizeof(struct gdb_reg_value) + sizeof(char*)),
            *prev_rbp = malloc(sizeof(struct gdb_reg_value) + sizeof(char*));

        LOG("UNWIND match %s: rip: 0x%lx rsp: 0x%lx rbp: 0x%lx \r\n", range->name, rip, rsp, rbp);
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

        for (i = 0; i < 16; i++) {
            if (i != 16 && i != 7 && i != 6) {
                struct gdb_reg_value *reg = malloc(sizeof(struct gdb_reg_value) + sizeof(char*));
                reg->free = &regfree;
                reg->defined = 1;
                reg->size = sizeof(char*);
                *(GDB_CORE_ADDR*)reg->value = *(GDB_CORE_ADDR*)cb->reg_get(cb,i)->value;
                cb->reg_set(cb, i, reg);
            }
        }

        prev_rsp->free = &regfree;
        prev_rsp->defined = 1;
        prev_rsp->size = sizeof(char*);
        prev_rbp->free = &regfree;
        prev_rbp->defined = 1;
        prev_rbp->size = sizeof(char*);
        prev_rip->free = &regfree;
        prev_rip->defined = 1;
        prev_rip->size = sizeof(char*);

        if (range->global) {

            cb->target_read(rbp + 1 * sizeof(char*), &prev_rip->value, sizeof(char*));
            ((GDB_CORE_ADDR*)prev_rsp->value)[0] = rbp + sizeof(char*) * 2;
            cb->target_read(rbp + 0 * sizeof(char*), &prev_rbp->value, sizeof(char*));

        } else {

            if (rip == priv->normal_exit) {
                LOG("Normal exit\r\n");
                ((GDB_CORE_ADDR*)prev_rsp->value)[0] = rsp;
                ((GDB_CORE_ADDR*)prev_rbp->value)[0] = rbp;
                ((GDB_CORE_ADDR*)prev_rip->value)[0] = rip;
            } else {
                cb->target_read(rsp, &prev_rip->value, sizeof(char*));

                for (rsp += sizeof(char*); ; rsp += sizeof(char*)) {
                    cb->target_read(rsp, &prev_rip->value, sizeof(char*));
                    LOG("rsp: 0x%lx rip: 0x%lx\r\n", rsp, *(GDB_CORE_ADDR*)prev_rip->value);
                    /* Check if it is a cp */
                    if ((*(GDB_CORE_ADDR*)prev_rip->value & 0x3) == 0) {
                        break;
                    }
                }
                ((GDB_CORE_ADDR*)prev_rsp->value)[0] = rsp;
                ((GDB_CORE_ADDR*)prev_rbp->value)[0] = rsp - sizeof(char*);
            }
        }

        LOG("UNWIND prev: rip: 0x%lx rsp: 0x%lx rbp: 0x%lx\r\n",
            *(GDB_CORE_ADDR*)prev_rip->value,
            *(GDB_CORE_ADDR*)prev_rsp->value,
            *(GDB_CORE_ADDR*)prev_rbp->value);

        cb->reg_set(cb, 16, prev_rip);
        cb->reg_set(cb, 7, prev_rsp);
        cb->reg_set(cb, 6, prev_rbp);
        return GDB_SUCCESS;
    }
    LOG("UNWIND no match: rip: 0x%lx rsp: 0x%lx rbp: 0x%lx\r\n", rip, rsp, rbp);
    return GDB_FAIL;
}

static struct gdb_frame_id get_frame_id(struct gdb_reader_funcs *self, struct gdb_unwind_callbacks *cb){
    priv *priv = self->priv_data;
    struct gdb_frame_id frame = {0, 0};
    GDB_CORE_ADDR rip = *(GDB_CORE_ADDR*)cb->reg_get(cb,16)->value;
    GDB_CORE_ADDR rbp = *(GDB_CORE_ADDR*)cb->reg_get(cb,6)->value;
    GDB_CORE_ADDR rsp = *(GDB_CORE_ADDR*)cb->reg_get(cb,7)->value;
    struct range *range = get_range(priv, rip);
    LOG("FRAME: rip: 0x%lx rsp: 0x%lx rbp: 0x%lx \r\n", rip, rsp, rbp);
    if (range) {
        frame.code_address = rip;
        if (range->global) {
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
