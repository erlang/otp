/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

#ifndef __GLOBAL_H__
#define __GLOBAL_H__

#include "sys.h"
#include <stddef.h> /* offsetof() */
#include "erl_alloc.h"
#include "erl_vm.h"
#include "erl_node_container_utils.h"
#include "hash.h"
#include "index.h"
#include "atom.h"
#include "code_ix.h"
#include "export.h"
#include "module.h"
#include "register.h"
#include "erl_fun.h"
#include "erl_node_tables.h"
#include "erl_process.h"
#include "erl_sys_driver.h"
#include "erl_debug.h"
#include "error.h"
#include "erl_utils.h"
#include "erl_port.h"
#include "erl_gc.h"
#include "erl_nif.h"
#define ERTS_BINARY_TYPES_ONLY__
#include "erl_binary.h"
#undef ERTS_BINARY_TYPES_ONLY__

struct enif_func_t;

#ifdef  DEBUG
#  define ERTS_NIF_ASSERT_IN_ENV
#endif
struct enif_environment_t /* ErlNifEnv */
{
    struct erl_module_nif* mod_nif;
    Process* proc;
    Eterm* hp;
    Eterm* hp_end;
    ErlHeapFragment* heap_frag;
    int fpe_was_unmasked;
    struct enif_tmp_obj_t* tmp_obj_list;
    int exception_thrown; /* boolean */
    Process *tracee;
    int exiting; /* boolean (dirty nifs might return in exiting state) */

#ifdef ERTS_NIF_ASSERT_IN_ENV
    int dbg_disable_assert_in_env;
#endif
};
struct enif_resource_type_t
{
    struct enif_resource_type_t* next;   /* list of all resource types */
    struct enif_resource_type_t* prev;
    struct erl_module_nif* owner;  /* that created this type and thus implements the destructor*/
    ErlNifResourceDtor* dtor;      /* user destructor function */
    ErlNifResourceStop* stop;
    ErlNifResourceDown* down;
    erts_refc_t refc;  /* num of resources of this type (HOTSPOT warning)
                          +1 for active erl_module_nif */
    Eterm module;
    Eterm name;
};

typedef struct
{
    erts_mtx_t lock;
    ErtsMonitor* root;
    Uint refc;
    size_t user_data_sz;
} ErtsResourceMonitors;

typedef struct ErtsResource_
{
    struct enif_resource_type_t* type;
    ErtsResourceMonitors* monitors;
#ifdef DEBUG
    erts_refc_t nif_refc;
#else
# ifdef ARCH_32
    byte align__[4];
# endif
#endif
    char data[1];
}ErtsResource;

#define DATA_TO_RESOURCE(PTR) ErtsContainerStruct(PTR, ErtsResource, data)
#define erts_resource_ref_size(P) ERTS_MAGIC_REF_THING_SIZE

extern Eterm erts_bld_resource_ref(Eterm** hp, ErlOffHeap*, ErtsResource*);

extern void erts_pre_nif(struct enif_environment_t*, Process*,
			 struct erl_module_nif*, Process* tracee);
extern void erts_post_nif(struct enif_environment_t* env);
#ifdef DEBUG
int erts_dbg_is_resource_dying(ErtsResource*);
#endif
extern void erts_resource_stop(ErtsResource*, ErlNifEvent, int is_direct_call);
void erts_fire_nif_monitor(ErtsMonitor *tmon);
void erts_nif_demonitored(ErtsResource* resource);
extern void erts_add_taint(Eterm mod_atom);
extern Eterm erts_nif_taints(Process* p);
extern void erts_print_nif_taints(fmtfn_t to, void* to_arg);
void erts_unload_nif(struct erl_module_nif* nif);
extern void erl_nif_init(void);
extern int erts_nif_get_funcs(struct erl_module_nif*,
                              struct enif_func_t **funcs);
extern Module *erts_nif_get_module(struct erl_module_nif*);
extern Eterm erts_nif_call_function(Process *p, Process *tracee,
                                    struct erl_module_nif*,
                                    struct enif_func_t *,
                                    int argc, Eterm *argv);

int erts_call_dirty_nif(ErtsSchedulerData *esdp, Process *c_p,
			BeamInstr *I, Eterm *reg);
ErtsMessage* erts_create_message_from_nif_env(ErlNifEnv* msg_env);


/* Driver handle (wrapper for old plain handle) */
#define ERL_DE_OK      0
#define ERL_DE_UNLOAD  1
#define ERL_DE_FORCE_UNLOAD 2 
#define ERL_DE_RELOAD  3
#define ERL_DE_FORCE_RELOAD  4
#define ERL_DE_PERMANENT 5

#define ERL_DE_PROC_LOADED 0
#define ERL_DE_PROC_AWAIT_UNLOAD 1
#define ERL_DE_PROC_AWAIT_UNLOAD_ONLY 2
#define ERL_DE_PROC_AWAIT_LOAD 3

/* Flags for process entries */
#define ERL_DE_FL_DEREFERENCED 1

/* Flags for drivers, put locking policy here /PaN */
#define ERL_DE_FL_KILL_PORTS 1

#define ERL_FL_CONSISTENT_MASK ( ERL_DE_FL_KILL_PORTS )

/* System specific load errors are returned as positive values */
#define ERL_DE_NO_ERROR 0
#define ERL_DE_LOAD_ERROR_NO_INIT -1
#define ERL_DE_LOAD_ERROR_FAILED_INIT -2
#define ERL_DE_LOAD_ERROR_BAD_NAME -3
#define ERL_DE_LOAD_ERROR_NAME_TO_LONG -4
#define ERL_DE_LOAD_ERROR_INCORRECT_VERSION -5
#define ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY -6
#define ERL_DE_ERROR_UNSPECIFIED -7
#define ERL_DE_LOOKUP_ERROR_NOT_FOUND -8
#define ERL_DE_DYNAMIC_ERROR_OFFSET -10

typedef struct de_proc_entry {
    Process *proc;                   /* The process... */
    Uint    awaiting_status;         /* PROC_LOADED == Have loaded the driver
			                PROC_AWAIT_UNLOAD == Wants to be notified 
			                when we have unloaded the driver (was locked)
			                PROC_AWAIT_LOAD == Wants to be notified when we
			                reloaded the driver (old was locked) */
    Uint    flags;                   /* ERL_FL_DE_DEREFERENCED when reload in progress */
    Eterm   heap[ERTS_REF_THING_SIZE];    /* "ref heap" */
    struct  de_proc_entry *next;
} DE_ProcEntry;

typedef struct {
    void         *handle;             /* Handle for DLL or SO (for dyn. drivers). */
    DE_ProcEntry *procs;              /* List of pids that have loaded this driver,
				         or that wait for it to change state */
    erts_refc_t  refc;                /* Number of ports/processes having
					 references to the driver */
    erts_atomic32_t port_count;   /* Number of ports using the driver */
    Uint         flags;               /* ERL_DE_FL_KILL_PORTS */
    int          status;              /* ERL_DE_xxx */
    char         *full_path;          /* Full path of the driver */
    char         *reload_full_path;   /* If status == ERL_DE_RELOAD, this contains
				         full name of driver (path) */
    char         *reload_driver_name; /* ... and this contains the driver name */
    Uint         reload_flags;        /* flags for reloaded driver */
} DE_Handle;

/*
 * This structure represents a link to the next driver.
 */

struct erts_driver_t_ {
    erts_driver_t *next;
    erts_driver_t *prev;
    Eterm name_atom;
    char *name;
    struct {
	int major;
	int minor;
    } version;
    int flags;
    DE_Handle *handle;
    erts_mtx_t *lock;
    ErlDrvEntry *entry;
    ErlDrvData (*start)(ErlDrvPort port, char *command, SysDriverOpts* opts);
    void (*stop)(ErlDrvData drv_data);
    void (*finish)(void);
    void (*flush)(ErlDrvData drv_data);
    void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev); /* Might be NULL */
    ErlDrvSSizeT (*control)(ErlDrvData drv_data, unsigned int command,
			    char *buf, ErlDrvSizeT len,
			    char **rbuf, ErlDrvSizeT rlen); /* Might be NULL */
    ErlDrvSSizeT (*call)(ErlDrvData drv_data, unsigned int command,
			 char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen, /* Might be NULL */
			 unsigned int *flags);
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
    void (*timeout)(ErlDrvData drv_data);
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data); /* Might be NULL */ 
    void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
    void (*stop_select)(ErlDrvEvent event, void*); /* Might be NULL */
    void (*emergency_close)(ErlDrvData drv_data);  /* Might be NULL */
};

extern erts_driver_t *driver_list;
extern erts_rwmtx_t erts_driver_list_lock;

extern void erts_ddll_init(void);
extern void erts_ddll_lock_driver(DE_Handle *dh, char *name);

/* These are for bookkeeping */
extern void erts_ddll_increment_port_count(DE_Handle *dh);
extern void erts_ddll_decrement_port_count(DE_Handle *dh);

/* These makes things happen, drivers may be scheduled for unload etc */
extern void erts_ddll_reference_driver(DE_Handle *dh);
extern void erts_ddll_reference_referenced_driver(DE_Handle *dh);
extern void erts_ddll_dereference_driver(DE_Handle *dh);

extern char *erts_ddll_error(int code);
extern void erts_ddll_proc_dead(Process *p, ErtsProcLocks plocks);
extern int erts_ddll_driver_ok(DE_Handle *dh);
extern void erts_ddll_remove_monitor(Process *p,
				     Eterm ref,
				     ErtsProcLocks plocks);
extern Eterm erts_ddll_monitor_driver(Process *p,
				      Eterm description,
				      ErtsProcLocks plocks);

/*
 * This structure represents one type of a binary in a process.
 */

typedef struct proc_bin {
    Eterm thing_word;		/* Subtag REFC_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    struct erl_off_heap_header *next;
    Binary *val;		/* Pointer to Binary structure. */
    byte *bytes;		/* Pointer to the actual data bytes. */
    Uint flags;			/* Flag word. */
} ProcBin;

#define PB_IS_WRITABLE 1	/* Writable (only one reference to ProcBin) */
#define PB_ACTIVE_WRITER 2	/* There is an active writer */

/*
 * ProcBin size in Eterm words.
 */
#define PROC_BIN_SIZE (sizeof(ProcBin)/sizeof(Eterm))

union erl_off_heap_ptr {
    struct erl_off_heap_header* hdr;
    ProcBin *pb;
    struct erl_fun_thing* fun;
    struct external_thing_* ext;
    ErtsMRefThing *mref;
    Eterm* ep;
    void* voidp;
};

/* controls warning mapping in error_logger */

extern Eterm node_cookie;

extern int erts_backtrace_depth;
extern erts_atomic32_t erts_max_gen_gcs;

extern int bif_reductions;      /* reductions + fcalls (when doing call_bif) */
extern int stackdump_on_exit;

/*
 * Here is an implementation of a lightweight stack.
 *
 * Use it like this:
 *
 * DECLARE_ESTACK(Stack)	(At the start of a block)
 * ...
 * ESTACK_PUSH(Stack, Term)
 * ...
 * if (ESTACK_ISEMPTY(Stack)) {
 *    Stack is empty
 * } else {
 *    Term = ESTACK_POP(Stack);
 *    Process popped Term here
 * }
 * ...
 * DESTROY_ESTACK(Stack)
 */

typedef struct ErtsEStack_ {
    Eterm* start;
    Eterm* sp;
    Eterm* end;
    Eterm* edefault;
    ErtsAlcType_t alloc_type;
}ErtsEStack;

#define DEF_ESTACK_SIZE (16)

void erl_grow_estack(ErtsEStack*, Uint need);
#define ESTK_CONCAT(a,b) a##b
#define ESTK_DEF_STACK(s) ESTK_CONCAT(s,_default_estack)

#define DECLARE_ESTACK(s)				\
    Eterm ESTK_DEF_STACK(s)[DEF_ESTACK_SIZE];		\
    ErtsEStack s = {					\
        ESTK_DEF_STACK(s),  /* start */ 		\
        ESTK_DEF_STACK(s),  /* sp */			\
        ESTK_DEF_STACK(s) + DEF_ESTACK_SIZE, /* end */	\
        ESTK_DEF_STACK(s),  /* default */		\
        ERTS_ALC_T_ESTACK /* alloc_type */		\
    }

#define ESTACK_CHANGE_ALLOCATOR(s,t)					\
do {									\
    if ((s).start != ESTK_DEF_STACK(s)) {				\
	erts_exit(ERTS_ERROR_EXIT, "Internal error - trying to change allocator "	\
		 "type of active estack\n");				\
    }									\
    (s).alloc_type = (t);						\
 } while (0)

#define DESTROY_ESTACK(s)				\
do {							\
    if ((s).start != ESTK_DEF_STACK(s)) {		\
	erts_free((s).alloc_type, (s).start); 		\
    }							\
} while(0)


/*
 * Do not free the stack after this, it may have pointers into what
 * was saved in 'dst'.
 */
#define ESTACK_SAVE(s,dst)\
do {\
    if ((s).start == ESTK_DEF_STACK(s)) {\
	UWord _wsz = ESTACK_COUNT(s);\
	(dst)->start = erts_alloc((s).alloc_type,\
				  DEF_ESTACK_SIZE * sizeof(Eterm));\
	sys_memcpy((dst)->start, (s).start,_wsz*sizeof(Eterm));\
	(dst)->sp = (dst)->start + _wsz;\
	(dst)->end = (dst)->start + DEF_ESTACK_SIZE;\
        (dst)->edefault = NULL;\
	(dst)->alloc_type = (s).alloc_type;\
    } else\
        *(dst) = (s);\
 } while (0)

#define DESTROY_SAVED_ESTACK(estack)\
do {\
    if ((estack)->start) {\
	erts_free((estack)->alloc_type, (estack)->start);\
	(estack)->start = NULL;\
    }\
} while(0)

#define CLEAR_SAVED_ESTACK(estack) ((void) ((estack)->start = NULL))

/*
 * Use on empty stack, only the allocator can be changed before this.
 * The src stack is reset to NULL.
 */
#define ESTACK_RESTORE(s, src)			\
do {						\
    ASSERT((s).start == ESTK_DEF_STACK(s));	\
    (s) = *(src);  /* struct copy */		\
    (src)->start = NULL;			\
    ASSERT((s).sp >= (s).start);		\
    ASSERT((s).sp <= (s).end);			\
} while (0)

#define ESTACK_IS_STATIC(s) ((s).start == ESTK_DEF_STACK(s))

#define ESTACK_PUSH(s, x)			\
do {						\
    if ((s).sp == (s).end) {			\
	erl_grow_estack(&(s), 1); 		\
    }						\
    *(s).sp++ = (x);				\
} while(0)

#define ESTACK_PUSH2(s, x, y)			\
do {						\
    if ((s).sp > (s).end - 2) {			\
	erl_grow_estack(&(s), 2);		\
    }						\
    *(s).sp++ = (x);				\
    *(s).sp++ = (y);				\
} while(0)

#define ESTACK_PUSH3(s, x, y, z)		\
do {						\
    if ((s).sp > (s).end - 3) {			\
	erl_grow_estack(&s, 3); 		\
    }						\
    *(s).sp++ = (x);				\
    *(s).sp++ = (y);				\
    *(s).sp++ = (z);				\
} while(0)

#define ESTACK_PUSH4(s, E1, E2, E3, E4)		\
do {						\
    if ((s).sp > (s).end - 4) {			\
	erl_grow_estack(&s, 4);                 \
    }						\
    *(s).sp++ = (E1);				\
    *(s).sp++ = (E2);				\
    *(s).sp++ = (E3);				\
    *(s).sp++ = (E4);				\
} while(0)

#define ESTACK_RESERVE(s, push_cnt)             \
do {					        \
    if ((s).sp > (s).end - (push_cnt)) {	\
	erl_grow_estack(&(s), (push_cnt));	\
    }					        \
} while(0)

/* Must be preceded by ESTACK_RESERVE */
#define ESTACK_FAST_PUSH(s, x)				\
do {							\
    ASSERT((s).sp < (s).end);                           \
    *s.sp++ = (x);					\
} while(0)

#define ESTACK_COUNT(s) ((s).sp - (s).start)
#define ESTACK_ISEMPTY(s) ((s).sp == (s).start)
#define ESTACK_POP(s) (*(--(s).sp))


/*
 * WSTACK: same as ESTACK but with UWord instead of Eterm
 */

typedef struct ErtsWStack_ {
    UWord* wstart;
    UWord* wsp;
    UWord* wend;
    UWord* wdefault;
    ErtsAlcType_t alloc_type;
}ErtsWStack;

#define DEF_WSTACK_SIZE (16)

void erl_grow_wstack(ErtsWStack*, Uint need);
#define WSTK_CONCAT(a,b) a##b
#define WSTK_DEF_STACK(s) WSTK_CONCAT(s,_default_wstack)

#define WSTACK_DECLARE(s)				\
    UWord WSTK_DEF_STACK(s)[DEF_WSTACK_SIZE];		\
    ErtsWStack s = {					\
        WSTK_DEF_STACK(s),  /* wstart */ 		\
        WSTK_DEF_STACK(s),  /* wsp */			\
        WSTK_DEF_STACK(s) + DEF_WSTACK_SIZE, /* wend */	\
        WSTK_DEF_STACK(s),  /* wdflt */ 		\
        ERTS_ALC_T_ESTACK /* alloc_type */		\
    }
#define DECLARE_WSTACK WSTACK_DECLARE

typedef struct ErtsDynamicWStack_ {
    UWord default_stack[DEF_WSTACK_SIZE];
    ErtsWStack ws;
}ErtsDynamicWStack;

#define WSTACK_INIT(dwsp, ALC_TYPE)                               \
do {	 	                                                  \
    (dwsp)->ws.wstart   = (dwsp)->default_stack;                  \
    (dwsp)->ws.wsp      = (dwsp)->default_stack;                  \
    (dwsp)->ws.wend     = (dwsp)->default_stack + DEF_WSTACK_SIZE;\
    (dwsp)->ws.wdefault = (dwsp)->default_stack;                  \
    (dwsp)->ws.alloc_type = ALC_TYPE;                             \
} while (0)

#define WSTACK_CHANGE_ALLOCATOR(s,t)					\
do {									\
    if (s.wstart != WSTK_DEF_STACK(s)) {				\
	erts_exit(ERTS_ERROR_EXIT, "Internal error - trying to change allocator "	\
		 "type of active wstack\n");				\
    }									\
    s.alloc_type = (t);							\
 } while (0)

#define WSTACK_DESTROY(s)				\
do {							\
    if (s.wstart != s.wdefault) {		        \
	erts_free(s.alloc_type, s.wstart); 		\
    }							\
} while(0)
#define DESTROY_WSTACK WSTACK_DESTROY

#define WSTACK_DEBUG(s) \
    do { \
	fprintf(stderr, "wstack size   = %ld\r\n", s.wsp - s.wstart); \
	fprintf(stderr, "wstack wstart = %p\r\n", s.wstart); \
	fprintf(stderr, "wstack wsp    = %p\r\n", s.wsp); \
    } while(0)

/*
 * Do not free the stack after this, it may have pointers into what
 * was saved in 'dst'.
 */
#define WSTACK_SAVE(s,dst)\
do {\
    if (s.wstart == WSTK_DEF_STACK(s)) {\
	UWord _wsz = WSTACK_COUNT(s);\
	(dst)->wstart = erts_alloc(s.alloc_type,\
				  DEF_WSTACK_SIZE * sizeof(UWord));\
	sys_memcpy((dst)->wstart, s.wstart,_wsz*sizeof(UWord));\
	(dst)->wsp = (dst)->wstart + _wsz;\
	(dst)->wend = (dst)->wstart + DEF_WSTACK_SIZE;\
        (dst)->wdefault = NULL;\
	(dst)->alloc_type = s.alloc_type;\
    } else\
        *(dst) = s;\
 } while (0)

#define DESTROY_SAVED_WSTACK(wstack)\
do {\
    if ((wstack)->wstart) {\
	erts_free((wstack)->alloc_type, (wstack)->wstart);\
	(wstack)->wstart = NULL;\
    }\
} while(0)

#define CLEAR_SAVED_WSTACK(wstack) ((void) ((wstack)->wstart = NULL))

/*
 * Use on empty stack, only the allocator can be changed before this.
 * The src stack is reset to NULL.
 */
#define WSTACK_RESTORE(s, src)			\
do {						\
    ASSERT(s.wstart == WSTK_DEF_STACK(s));	\
    s = *(src);  /* struct copy */		\
    (src)->wstart = NULL;			\
    ASSERT(s.wsp >= s.wstart);			\
    ASSERT(s.wsp <= s.wend);			\
} while (0)

#define WSTACK_IS_STATIC(s) (s.wstart == WSTK_DEF_STACK(s))

#define WSTACK_PUSH(s, x)				\
do {							\
    if (s.wsp == s.wend) {				\
	erl_grow_wstack(&s, 1); 	                \
    }							\
    *s.wsp++ = (x);					\
} while(0)

#define WSTACK_PUSH2(s, x, y)			\
do {						\
    if (s.wsp > s.wend - 2) {			\
	erl_grow_wstack(&s, 2);                 \
    }						\
    *s.wsp++ = (x);				\
    *s.wsp++ = (y);				\
} while(0)

#define WSTACK_PUSH3(s, x, y, z)		\
do {						\
    if (s.wsp > s.wend - 3) {	                \
	erl_grow_wstack(&s, 3);                 \
    }						\
    *s.wsp++ = (x);				\
    *s.wsp++ = (y);				\
    *s.wsp++ = (z);				\
} while(0)

#define WSTACK_PUSH4(s, A1, A2, A3, A4)		\
do {						\
    if (s.wsp > s.wend - 4) {	                \
	erl_grow_wstack(&s, 4);                 \
    }						\
    *s.wsp++ = (A1);				\
    *s.wsp++ = (A2);				\
    *s.wsp++ = (A3);				\
    *s.wsp++ = (A4);				\
} while(0)

#define WSTACK_PUSH5(s, A1, A2, A3, A4, A5)     \
do {						\
    if (s.wsp > s.wend - 5) {	                \
	erl_grow_wstack(&s, 5);                 \
    }						\
    *s.wsp++ = (A1);				\
    *s.wsp++ = (A2);				\
    *s.wsp++ = (A3);				\
    *s.wsp++ = (A4);				\
    *s.wsp++ = (A5);				\
} while(0)

#define WSTACK_PUSH6(s, A1, A2, A3, A4, A5, A6) \
do {						\
    if (s.wsp > s.wend - 6) {	                \
	erl_grow_wstack(&s, 6);                 \
    }						\
    *s.wsp++ = (A1);				\
    *s.wsp++ = (A2);				\
    *s.wsp++ = (A3);				\
    *s.wsp++ = (A4);				\
    *s.wsp++ = (A5);				\
    *s.wsp++ = (A6);				\
} while(0)

#define WSTACK_RESERVE(s, push_cnt)             \
do {						\
    if (s.wsp > s.wend - (push_cnt)) { 	        \
	erl_grow_wstack(&s, (push_cnt));        \
    }                                           \
} while(0)

/* Must be preceded by WSTACK_RESERVE */
#define WSTACK_FAST_PUSH(s, x)                  \
do {						\
    ASSERT(s.wsp < s.wend);                     \
    *s.wsp++ = (x);                             \
} while(0)

#define WSTACK_COUNT(s) (s.wsp - s.wstart)
#define WSTACK_ISEMPTY(s) (s.wsp == s.wstart)
#define WSTACK_POP(s) ((ASSERT(s.wsp > s.wstart)),*(--s.wsp))

#define WSTACK_ROLLBACK(s, count) (ASSERT(WSTACK_COUNT(s) >= (count)), \
                                   s.wsp = s.wstart + (count))

/* PSTACK - Stack of any type.
 * Usage:
 * {
 * #define PSTACK_TYPE MyType
 *    PSTACK_DECLARE(s,16);
 *    MyType *sp = PSTACK_PUSH(s);
 *
 *    sp->x = ....
 *    sp->y = ....
 *    sp = PSTACK_PUSH(s);
 *    ...
 *    sp = PSTACK_POP(s);
 *    if (PSTACK_IS_EMPTY(s)) {
 *        // sp is invalid when stack is empty after pop
 *    }
 *
 *    PSTACK_DESTROY(s);
 * }
 */


typedef struct ErtsPStack_ {
    byte* pstart;
    int offs;   /* "stack pointer" as byte offset from pstart */
    int size;   /* allocated size in bytes */
    ErtsAlcType_t alloc_type;
}ErtsPStack;

void erl_grow_pstack(ErtsPStack* s, void* default_pstack, unsigned need_bytes);
#define PSTK_CONCAT(a,b) a##b
#define PSTK_DEF_STACK(s) PSTK_CONCAT(s,_default_pstack)

#define PSTACK_DECLARE(s, DEF_PSTACK_SIZE) \
PSTACK_TYPE PSTK_DEF_STACK(s)[DEF_PSTACK_SIZE];                            \
ErtsPStack s = { (byte*)PSTK_DEF_STACK(s), /* pstart */                    \
                 -(int)sizeof(PSTACK_TYPE), /* offs */                     \
                 DEF_PSTACK_SIZE*sizeof(PSTACK_TYPE), /* size */           \
                 ERTS_ALC_T_ESTACK   /* alloc_type */                      \
}

#define PSTACK_CHANGE_ALLOCATOR(s,t)					\
do {									\
    if (s.pstart != (byte*)PSTK_DEF_STACK(s)) {				\
	erts_exit(ERTS_ERROR_EXIT, "Internal error - trying to change allocator "	\
		 "type of active pstack\n");				\
    }									\
    s.alloc_type = (t);							\
 } while (0)

#define PSTACK_DESTROY(s)				\
do {							\
    if (s.pstart != (byte*)PSTK_DEF_STACK(s)) {		\
	erts_free(s.alloc_type, s.pstart); 		\
    }							\
} while(0)

#define PSTACK_IS_EMPTY(s) (s.offs < 0)

#define PSTACK_COUNT(s) ((s.offs + sizeof(PSTACK_TYPE)) / sizeof(PSTACK_TYPE))

#define PSTACK_TOP(s) (ASSERT(!PSTACK_IS_EMPTY(s)), \
                       (PSTACK_TYPE*)(s.pstart + s.offs))

#define PSTACK_PUSH(s) 		                                            \
    (s.offs += sizeof(PSTACK_TYPE),                                         \
     ((s.offs == s.size) ? erl_grow_pstack(&s, PSTK_DEF_STACK(s),           \
                                          sizeof(PSTACK_TYPE)) : (void)0),  \
     ((PSTACK_TYPE*) (s.pstart + s.offs)))

#define PSTACK_POP(s) ((s.offs -= sizeof(PSTACK_TYPE)), \
                       (PSTACK_TYPE*)(s.pstart + s.offs))

/*
 * Do not free the stack after this, it may have pointers into what
 * was saved in 'dst'.
 */
#define PSTACK_SAVE(s,dst)\
do {\
    if (s.pstart == (byte*)PSTK_DEF_STACK(s)) {\
	UWord _pbytes = PSTACK_COUNT(s) * sizeof(PSTACK_TYPE);\
	(dst)->pstart = erts_alloc(s.alloc_type,\
				   sizeof(PSTK_DEF_STACK(s)));\
	sys_memcpy((dst)->pstart, s.pstart, _pbytes);\
	(dst)->offs = s.offs;\
	(dst)->size = s.size;\
	(dst)->alloc_type = s.alloc_type;\
    } else\
        *(dst) = s;\
 } while (0)

/*
 * Use on empty stack, only the allocator can be changed before this.
 * The src stack is reset to NULL.
 */
#define PSTACK_RESTORE(s, src)			        \
do {						        \
    ASSERT(s.pstart == (byte*)PSTK_DEF_STACK(s));	\
    s = *(src);  /* struct copy */		        \
    (src)->pstart = NULL;			        \
    ASSERT(s.offs >= -(int)sizeof(PSTACK_TYPE));        \
    ASSERT(s.offs < s.size);			        \
} while (0)

#define PSTACK_DESTROY_SAVED(pstack)\
do {\
    if ((pstack)->pstart) {\
	erts_free((pstack)->alloc_type, (pstack)->pstart);\
	(pstack)->pstart = NULL;\
    }\
} while(0)


/*
 *  An implementation of lightweight unbounded queues,
 *  using a circular dynamic array.
 *  It does not include support for change_allocator.
 *
 *  Use it like this:
 *
 *  DECLARE_EQUEUE(Queue)	(At the start of a block)
 *  ...
 *  EQUEUE_PUT(Queue, Term)
 *  ...
 *  if (EQUEUE_ISEMPTY(Queue)) {
 *     Queue is empty
 *  } else {
 *     Term = EQUEUE_GET(Stack);
 *     Process popped Term here
 *  }
 *  ...
 *  DESTROY_EQUEUE(Queue)
 */

typedef struct {
    Eterm* start;
    Eterm* front;
    Eterm* back;
    int possibly_empty;
    Eterm* end;
    ErtsAlcType_t alloc_type;
} ErtsEQueue;

#define DEF_EQUEUE_SIZE (16)

void erl_grow_equeue(ErtsEQueue*, Eterm* def_queue);
#define EQUE_CONCAT(a,b) a##b
#define EQUE_DEF_QUEUE(q) EQUE_CONCAT(q,_default_equeue)

#define DECLARE_EQUEUE(q)				\
    UWord EQUE_DEF_QUEUE(q)[DEF_EQUEUE_SIZE];     	\
    ErtsEQueue q = {					\
        EQUE_DEF_QUEUE(q), /* start */			\
        EQUE_DEF_QUEUE(q), /* front */			\
        EQUE_DEF_QUEUE(q), /* back */			\
        1,                 /* possibly_empty */		\
        EQUE_DEF_QUEUE(q) + DEF_EQUEUE_SIZE, /* end */	\
        ERTS_ALC_T_ESTACK  /* alloc_type */		\
    }

#define DESTROY_EQUEUE(q)				\
do {							\
    if (q.start != EQUE_DEF_QUEUE(q)) {			\
      erts_free(q.alloc_type, q.start);			\
    }							\
} while(0)

#define EQUEUE_PUT_UNCHECKED(q, x)			\
do {							\
    q.possibly_empty = 0;				\
    *(q.back) = (x);                    		\
    if (++(q.back) == q.end) {				\
	q.back = q.start;				\
    }							\
} while(0)

#define EQUEUE_PUT(q, x)				\
do {							\
    if (q.back == q.front && !q.possibly_empty) {	\
        erl_grow_equeue(&q, EQUE_DEF_QUEUE(q));		\
    }							\
    EQUEUE_PUT_UNCHECKED(q, x);				\
} while(0)

#define EQUEUE_ISEMPTY(q) (q.back == q.front && q.possibly_empty)

ERTS_GLB_INLINE Eterm erts_equeue_get(ErtsEQueue *q);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm erts_equeue_get(ErtsEQueue *q) {
    Eterm x;
    q->possibly_empty = 1;
    x = *(q->front);
    if (++(q->front) == q->end) {
        q->front = q->start;
    }
    return x;
}
#endif
#define EQUEUE_GET(q) erts_equeue_get(&(q));

/* binary.c */

void erts_emasculate_writable_binary(ProcBin* pb);
Eterm erts_new_heap_binary(Process *p, byte *buf, int len, byte** datap);
Eterm erts_new_mso_binary(Process*, byte*, Uint);
Eterm new_binary(Process*, byte*, Uint);
Eterm erts_heap_factory_new_binary(ErtsHeapFactory *hfact, byte *buf,
                                   Uint len, Uint reserve_size);
Eterm erts_realloc_binary(Eterm bin, size_t size);
Eterm erts_build_proc_bin(ErlOffHeap*, Eterm*, Binary*);

/* erl_bif_info.c */

Eterm
erts_bld_port_info(Eterm **hpp,
		   ErlOffHeap *ohp,
		   Uint *szp,
		   Port *prt,
		   Eterm item); 
Eterm erts_bld_bin_list(Uint **hpp, Uint *szp, ErlOffHeap* oh, Eterm tail);


void erts_bif_info_init(void);

/* bif.c */

void erts_queue_monitor_message(Process *,
				ErtsProcLocks*,
				Eterm,
				Eterm,
				Eterm,
				Eterm);
void erts_init_trap_export(Export* ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(Process*, Eterm*, BeamInstr*));
void erts_init_bif(void);
Eterm erl_send(Process *p, Eterm to, Eterm msg);
int erts_set_group_leader(Process *proc, Eterm new_gl);

/* erl_bif_guard.c */

void erts_init_bif_guard(void);
Eterm erts_trapping_length_1(Process* p, Eterm* args);

/* erl_bif_op.c */

Eterm erl_is_function(Process* p, Eterm arg1, Eterm arg2);

/* beam_bif_load.c */
Eterm erts_check_process_code(Process *c_p, Eterm module, int *redsp, int fcalls);
Eterm erts_proc_copy_literal_area(Process *c_p, int *redsp, int fcalls, int gc_allowed);

Uint32 erts_block_release_literal_area(void);
void erts_unblock_release_literal_area(Uint32);

void erts_debug_foreach_release_literal_area_off_heap(void (*func)(ErlOffHeap *, void *),
                                                      void *arg);

typedef struct ErtsLiteralArea_ {
    struct erl_off_heap_header *off_heap;
    Eterm *end;
    Eterm start[1]; /* beginning of area */
} ErtsLiteralArea;

void erts_queue_release_literals(Process *c_p, ErtsLiteralArea* literals);

#define ERTS_LITERAL_AREA_ALLOC_SIZE(N) \
    (sizeof(ErtsLiteralArea) + sizeof(Eterm)*((N) - 1))

extern erts_atomic_t erts_copy_literal_area__;
#define ERTS_COPY_LITERAL_AREA()					\
    ((ErtsLiteralArea *) erts_atomic_read_nob(&erts_copy_literal_area__))
extern Process *erts_literal_area_collector;

extern Process *erts_code_purger;

/* beam_load.c */
typedef struct {
    ErtsCodeMFA* mfa;		/* Pointer to: Mod, Name, Arity */
    Uint needed;		/* Heap space needed for entire tuple */
    Uint32 loc;			/* Location in source code */
    Eterm* fname_ptr;		/* Pointer to fname table */
} FunctionInfo;

Binary* erts_alloc_loader_state(void);
Eterm erts_module_for_prepared_code(Binary* magic);
Eterm erts_has_code_on_load(Binary* magic);
Eterm erts_prepare_loading(Binary* loader_state,  Process *c_p,
			   Eterm group_leader, Eterm* modp,
			   byte* code, Uint size);
Eterm erts_finish_loading(Binary* loader_state, Process* c_p,
			  ErtsProcLocks c_p_locks, Eterm* modp);
Eterm erts_preload_module(Process *c_p, ErtsProcLocks c_p_locks,
			  Eterm group_leader, Eterm* mod, byte* code, Uint size);
void init_load(void);
ErtsCodeMFA* find_function_from_pc(BeamInstr* pc);
Eterm* erts_build_mfa_item(FunctionInfo* fi, Eterm* hp,
			   Eterm args, Eterm* mfa_p);
void erts_set_current_function(FunctionInfo* fi, ErtsCodeMFA* mfa);
Eterm erts_module_info_0(Process* p, Eterm module);
Eterm erts_module_info_1(Process* p, Eterm module, Eterm what);
Eterm erts_make_stub_module(Process* p, Eterm Mod, Eterm Beam, Eterm Info);
int erts_commit_hipe_patch_load(Eterm hipe_magic_bin);

/* beam_ranges.c */
void erts_init_ranges(void);
void erts_start_staging_ranges(int num_new);
void erts_end_staging_ranges(int commit);
void erts_update_ranges(BeamInstr* code, Uint size);
void erts_remove_from_ranges(BeamInstr* code);
UWord erts_ranges_sz(void);
void erts_lookup_function_info(FunctionInfo* fi, BeamInstr* pc, int full_info);
extern ErtsLiteralArea** erts_dump_lit_areas;
extern Uint erts_dump_num_lit_areas;

/* break.c */
void init_break_handler(void);
void erts_set_ignore_break(void);
void erts_replace_intr(void);
void process_info(fmtfn_t, void *);
void print_process_info(fmtfn_t, void *, Process*, ErtsProcLocks);
void info(fmtfn_t, void *);
void loaded(fmtfn_t, void *);
void erts_print_base64(fmtfn_t to, void *to_arg, byte* src, Uint size);

/* sighandler sys.c */
int erts_set_signal(Eterm signal, Eterm type);

/* erl_arith.c */
double erts_get_positive_zero_float(void);

/* config.c */

__decl_noreturn void __noreturn erts_exit_epilogue(void);
__decl_noreturn void __noreturn erts_exit(int n, char*, ...);
__decl_noreturn void __noreturn erts_flush_async_exit(int n, char*, ...);
void erl_error(char*, va_list);

/* This controls whether sharing-preserving copy is used by Erlang */

#ifdef SHCOPY
#define SHCOPY_SEND
#define SHCOPY_SPAWN
#endif

/* The persistent state while the sharing-preserving copier works */

typedef struct {
    Eterm  queue_default[DEF_EQUEUE_SIZE];
    Eterm* queue_start;
    Eterm* queue_end;
    ErtsAlcType_t queue_alloc_type;
    UWord  bitstore_default[DEF_WSTACK_SIZE];
    UWord* bitstore_start;
    ErtsAlcType_t bitstore_alloc_type;
    Eterm  shtable_default[DEF_ESTACK_SIZE];
    Eterm* shtable_start;
    ErtsAlcType_t shtable_alloc_type;
    Uint literal_size;
    Eterm *lit_purge_ptr;
    Uint lit_purge_sz;
    int copy_literals;
} erts_shcopy_t;

#define INITIALIZE_SHCOPY(info)						\
    do {								\
	ErtsLiteralArea *larea__ = ERTS_COPY_LITERAL_AREA();		\
	info.queue_start = info.queue_default;				\
	info.bitstore_start = info.bitstore_default;			\
	info.shtable_start = info.shtable_default;			\
	info.literal_size = 0;						\
	info.copy_literals = 0;						\
	if (larea__) {							\
	    info.lit_purge_ptr = &larea__->start[0];			\
	    info.lit_purge_sz = larea__->end - info.lit_purge_ptr;	\
	}								\
	else {								\
	    info.lit_purge_ptr = NULL;					\
	    info.lit_purge_sz = 0;					\
	}								\
    } while(0)

#define DESTROY_SHCOPY(info)                                            \
do {                                                                    \
    if (info.queue_start != info.queue_default) {                       \
        erts_free(info.queue_alloc_type, info.queue_start);             \
    }                                                                   \
    if (info.bitstore_start != info.bitstore_default) {                 \
        erts_free(info.bitstore_alloc_type, info.bitstore_start);       \
    }                                                                   \
    if (info.shtable_start != info.shtable_default) {                   \
        erts_free(info.shtable_alloc_type, info.shtable_start);         \
    }                                                                   \
} while(0)

/* copy.c */
typedef struct {
    Eterm *lit_purge_ptr;
    Uint lit_purge_sz;
} erts_literal_area_t;

#define INITIALIZE_LITERAL_PURGE_AREA(Area)				\
    do {								\
	ErtsLiteralArea *larea__ = ERTS_COPY_LITERAL_AREA();		\
	if (larea__) {							\
	    (Area).lit_purge_ptr = &larea__->start[0];			\
	    (Area).lit_purge_sz = larea__->end - (Area).lit_purge_ptr;	\
	}								\
	else {								\
	    (Area).lit_purge_ptr = NULL;				\
	    (Area).lit_purge_sz = 0;					\
	}								\
    } while(0)

Eterm copy_object_x(Eterm, Process*, Uint);
#define copy_object(Term, Proc) copy_object_x(Term,Proc,0)

Uint size_object_x(Eterm, erts_literal_area_t*);
#define size_object(Term) size_object_x(Term,NULL)
#define size_object_litopt(Term,LitArea) size_object_x(Term,LitArea)

Uint copy_shared_calculate(Eterm, erts_shcopy_t*);
Uint size_shared(Eterm);

/* #define ERTS_COPY_REGISTER_LOCATION */

#ifdef ERTS_COPY_REGISTER_LOCATION

#define copy_shared_perform(U, V, X, Y, Z) \
    copy_shared_perform_x((U), (V), (X), (Y), (Z), __FILE__, __LINE__)
Eterm copy_shared_perform_x(Eterm, Uint, erts_shcopy_t*, Eterm**, ErlOffHeap*,
                            char *file, int line);

Eterm copy_struct_x(Eterm, Uint, Eterm**, ErlOffHeap*, Uint*, erts_literal_area_t*,
                    char *file, int line);
#define copy_struct(Obj,Sz,HPP,OH) \
    copy_struct_x(Obj,Sz,HPP,OH,NULL,NULL,__FILE__,__LINE__)
#define copy_struct_litopt(Obj,Sz,HPP,OH,LitArea) \
    copy_struct_x(Obj,Sz,HPP,OH,NULL,LitArea,__FILE__,__LINE__)

#define copy_shallow(R, SZ, HPP, OH) \
    copy_shallow_x((R), (SZ), (HPP), (OH), __FILE__, __LINE__)
Eterm copy_shallow_x(Eterm* ERTS_RESTRICT, Uint, Eterm**, ErlOffHeap*,
                     char *file, int line);

#else

#define copy_shared_perform(U, V, X, Y, Z) \
    copy_shared_perform_x((U), (V), (X), (Y), (Z))
Eterm copy_shared_perform_x(Eterm, Uint, erts_shcopy_t*, Eterm**, ErlOffHeap*);

Eterm copy_struct_x(Eterm, Uint, Eterm**, ErlOffHeap*, Uint*, erts_literal_area_t*);
#define copy_struct(Obj,Sz,HPP,OH) \
    copy_struct_x(Obj,Sz,HPP,OH,NULL,NULL)
#define copy_struct_litopt(Obj,Sz,HPP,OH,LitArea) \
    copy_struct_x(Obj,Sz,HPP,OH,NULL,LitArea)

#define copy_shallow(R, SZ, HPP, OH) \
    copy_shallow_x((R), (SZ), (HPP), (OH))
Eterm copy_shallow_x(Eterm* ERTS_RESTRICT, Uint, Eterm**, ErlOffHeap*);

#endif

void erts_move_multi_frags(Eterm** hpp, ErlOffHeap*, ErlHeapFragment* first,
			   Eterm* refs, unsigned nrefs, int literals);

/* Utilities */
void erts_monitor_nodes_delete(ErtsMonitor *);
extern Eterm erts_monitor_nodes(Process *, Eterm, Eterm);
extern Eterm erts_processes_monitoring_nodes(Process *);
extern int erts_do_net_exits(DistEntry*, Eterm);
extern int distribution_info(fmtfn_t, void *);
extern int is_node_name_atom(Eterm a);

extern int erts_net_message(Port *, DistEntry *, Uint32 conn_id,
			    byte *, ErlDrvSizeT, Binary *, byte *, ErlDrvSizeT);

extern void init_dist(void);
extern int stop_dist(void);

void erl_progressf(char* format, ...);

#ifdef MESS_DEBUG
void print_pass_through(int, byte*, int);
#endif

/* beam_emu.c */
int catchlevel(Process*);
void init_emulator(void);
void process_main(Eterm* x_reg_array, FloatDef* f_reg_array);
void erts_dirty_process_main(ErtsSchedulerData *);
Eterm build_stacktrace(Process* c_p, Eterm exc);
Eterm expand_error_value(Process* c_p, Uint freason, Eterm Value);
void erts_save_stacktrace(Process* p, struct StackTrace* s, int depth);

/* erl_init.c */

typedef struct {
    Eterm delay_time;
    int context_reds;
} ErtsModifiedTimings;

extern Export *erts_delay_trap;
extern int erts_modified_timing_level;
extern ErtsModifiedTimings erts_modified_timings[];
#define ERTS_USE_MODIFIED_TIMING() \
  (erts_modified_timing_level >= 0)
#define ERTS_MODIFIED_TIMING_DELAY \
  (erts_modified_timings[erts_modified_timing_level].delay_time)
#define ERTS_MODIFIED_TIMING_CONTEXT_REDS \
  (erts_modified_timings[erts_modified_timing_level].context_reds)
#define ERTS_MODIFIED_TIMING_INPUT_REDS \
  (erts_modified_timings[erts_modified_timing_level].input_reds)

extern int erts_no_line_info;
extern Eterm erts_error_logger_warnings;
extern int erts_initialized;
extern int erts_compat_rel;
void erl_start(int, char**);
void erts_usage(void);
Eterm erts_preloaded(Process* p);


/* erl_md5.c */

typedef struct {
    Uint32 state[4];		/* state (ABCD) */
    Uint32 count[2];		/* number of bits, modulo 2^64 (lsb first) */
    unsigned char buffer[64];	/* input buffer */
} MD5_CTX;

void MD5Init(MD5_CTX *);
void MD5Update(MD5_CTX *, unsigned char *, unsigned int);
void MD5Final(unsigned char [16], MD5_CTX *);


/* io.c */

typedef struct {
    char *name;
    char *driver_name;
} ErtsPortNames;

#define ERTS_SPAWN_DRIVER 1
#define ERTS_SPAWN_EXECUTABLE 2
#define ERTS_SPAWN_ANY (ERTS_SPAWN_DRIVER | ERTS_SPAWN_EXECUTABLE)
int erts_add_driver_entry(ErlDrvEntry *drv, DE_Handle *handle, int driver_list_locked, int taint);
void erts_destroy_driver(erts_driver_t *drv);
int erts_save_suspend_process_on_port(Port*, Process*);
Port *erts_open_driver(erts_driver_t*, Eterm, char*, SysDriverOpts*, int *, int *);
void erts_init_io(int, int, int);
void erts_raw_port_command(Port*, byte*, Uint);
void driver_report_exit(ErlDrvPort, int);
LineBuf* allocate_linebuf(int);
int async_ready(Port *, void*);
ErtsPortNames *erts_get_port_names(Eterm, ErlDrvPort);
void erts_free_port_names(ErtsPortNames *);
Uint erts_port_ioq_size(Port *pp);
void erts_stale_drv_select(Eterm, ErlDrvPort, ErlDrvEvent, int, int);

Port *erts_get_heart_port(void);
void erts_emergency_close_ports(void);
void erts_ref_to_driver_monitor(Eterm ref, ErlDrvMonitor *mon);
Eterm erts_driver_monitor_to_ref(Eterm* hp, const ErlDrvMonitor *mon);

#if defined(ERTS_ENABLE_LOCK_COUNT)
void erts_lcnt_update_driver_locks(int enable);
void erts_lcnt_update_port_locks(int enable);
#endif

/* driver_tab.c */
typedef struct {
    ErlDrvEntry* de;
    int taint;
} ErtsStaticDriver;
typedef void *(*ErtsStaticNifInitFPtr)(void);
typedef struct ErtsStaticNifEntry_ {
    const char *nif_name;
    ErtsStaticNifInitFPtr nif_init;
    int taint;
} ErtsStaticNifEntry;
ErtsStaticNifEntry* erts_static_nif_get_nif_init(const char *name, int len);
int erts_is_static_nif(void *handle);
void erts_init_static_drivers(void);

/* erl_drv_thread.c */
void erl_drv_thr_init(void);

/* utils.c */
void erts_cleanup_offheap(ErlOffHeap *offheap);

Uint64 erts_timestamp_millis(void);

Export* erts_find_function(Eterm, Eterm, unsigned int, ErtsCodeIndex);

/* ERTS_NOINLINE prevents link-time optimization across modules */
void *erts_calc_stacklimit(char *prev_c, UWord stacksize) ERTS_NOINLINE;
int erts_check_below_limit(char *ptr, char *limit) ERTS_NOINLINE;
int erts_check_above_limit(char *ptr, char *limit) ERTS_NOINLINE;
void *erts_ptr_id(void *ptr) ERTS_NOINLINE;
int erts_check_if_stack_grows_downwards(char *ptr) ERTS_NOINLINE;

Eterm store_external_or_ref_in_proc_(Process *, Eterm);
Eterm store_external_or_ref_(Uint **, ErlOffHeap*, Eterm);

#define NC_HEAP_SIZE(NC) \
 (ASSERT(is_node_container((NC))), \
  IS_CONST((NC)) ? 0 : (thing_arityval(*boxed_val((NC))) + 1))
#define STORE_NC(Hpp, ETpp, NC) \
 (ASSERT(is_node_container((NC))), \
  IS_CONST((NC)) ? (NC) : store_external_or_ref_((Hpp), (ETpp), (NC)))
#define STORE_NC_IN_PROC(Pp, NC) \
 (ASSERT(is_node_container((NC))), \
  IS_CONST((NC)) ? (NC) : store_external_or_ref_in_proc_((Pp), (NC)))

/* duplicates from big.h */
int term_to_Uint(Eterm term, Uint *up);
int term_to_UWord(Eterm, UWord*);

#ifdef HAVE_ERTS_NOW_CPU
extern int erts_cpu_timestamp;
#endif
/* erl_bif_chksum.c */
void erts_init_bif_chksum(void);
/* erl_bif_re.c */
void erts_init_bif_re(void);
Sint erts_re_set_loop_limit(Sint limit);
/* erl_bif_binary.c */
void erts_init_bif_binary(void);
Sint erts_binary_set_loop_limit(Sint limit);

/* erl_bif_persistent.c */
void erts_init_bif_persistent_term(void);
Uint erts_persistent_term_count(void);
void erts_init_persistent_dumping(void);
extern ErtsLiteralArea** erts_persistent_areas;
extern Uint erts_num_persistent_areas;
void erts_debug_foreach_persistent_term_off_heap(void (*func)(ErlOffHeap *, void *),
                                                 void *arg);
int erts_debug_have_accessed_literal_area(ErtsLiteralArea *lap);
void erts_debug_save_accessed_literal_area(ErtsLiteralArea *lap);

/* external.c */
void erts_init_external(void);

/* erl_map.c */
void erts_init_map(void);

/* beam_debug.c */
UWord erts_check_stack_recursion_downwards(char *start_c);
UWord erts_check_stack_recursion_upwards(char *start_c);
int erts_is_above_stack_limit(char *ptr);

/* erl_unicode.c */
void erts_init_unicode(void);
Sint erts_unicode_set_loop_limit(Sint limit);

void erts_native_filename_put(Eterm ioterm, int encoding, byte *p) ;
Sint erts_native_filename_need(Eterm ioterm, int encoding);
void erts_copy_utf8_to_utf16_little(byte *target, byte *bytes, int num_chars);
int erts_analyze_utf8(byte *source, Uint size, 
			byte **err_pos, Uint *num_chars, int *left);
int erts_analyze_utf8_x(byte *source, Uint size, 
			byte **err_pos, Uint *num_chars, int *left,
			Sint *num_latin1_chars, Uint max_chars);
char *erts_convert_filename_to_native(Eterm name, char *statbuf, 
				      size_t statbuf_size, 
				      ErtsAlcType_t alloc_type, 
				      int allow_empty, int allow_atom,
				      Sint *used /* out */);
char *erts_convert_filename_to_encoding(Eterm name, char *statbuf,
					size_t statbuf_size,
					ErtsAlcType_t alloc_type,
					int allow_empty, int allow_atom,
					int encoding,
					Sint *used /* out */,
					Uint extra);
char* erts_convert_filename_to_wchar(byte* bytes, Uint size,
                                     char *statbuf, size_t statbuf_size,
                                     ErtsAlcType_t alloc_type, Sint* used,
                                     Uint extra_wchars);
Eterm erts_convert_native_to_filename(Process *p, size_t size, byte *bytes);
Eterm erts_utf8_to_list(Process *p, Uint num, byte *bytes, Uint sz, Uint left,
			Uint *num_built, Uint *num_eaten, Eterm tail);
int erts_utf8_to_latin1(byte* dest, const byte* source, int slen);
#define ERTS_UTF8_OK 0
#define ERTS_UTF8_INCOMPLETE 1
#define ERTS_UTF8_ERROR 2
#define ERTS_UTF8_ANALYZE_MORE 3
#define ERTS_UTF8_OK_MAX_CHARS 4

void bin_write(fmtfn_t, void*, byte*, size_t);
Sint intlist_to_buf(Eterm, char*, Sint); /* most callers pass plain char*'s */
int erts_unicode_list_to_buf(Eterm list, byte *buf, Sint len, Sint* written);
Sint erts_unicode_list_to_buf_len(Eterm list);

int Sint_to_buf(Sint num, int base, char **buf_p, size_t buf_size);

#define ERTS_IOLIST_STATE_INITER(C_P, OBJ)	\
    {(C_P), 0, 0, (OBJ), {NULL, NULL, NULL, ERTS_ALC_T_INVALID}, 0, 0}

#define ERTS_IOLIST_STATE_MOVE(TO, FROM)	\
    sys_memcpy((void *) (TO), (void *) (FROM), sizeof(ErtsIOListState))

#define ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED 8

typedef struct {
    Process *c_p;
    ErlDrvSizeT size;
    Uint offs;
    Eterm obj;
    ErtsEStack estack;
    int reds_left;
    int have_size;
} ErtsIOListState;

#define ERTS_IOLIST2BUF_STATE_INITER(C_P, OBJ)	\
    {ERTS_IOLIST_STATE_INITER((C_P), (OBJ)), {NULL, 0, 0, 0}, NULL, 0, NULL, 0}

#define ERTS_IOLIST2BUF_STATE_MOVE(TO, FROM)	\
    sys_memcpy((void *) (TO), (void *) (FROM), sizeof(ErtsIOList2BufState))

#define ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT 32
#define ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED 8
#define ERTS_IOLIST_TO_BUF_BYTES_PER_RED \
    (ERTS_IOLIST_TO_BUF_YIELD_COUNT_PER_RED*ERTS_IOLIST_TO_BUF_BYTES_PER_YIELD_COUNT)

typedef struct {
    ErtsIOListState iolist;
    struct {
	byte *bptr;
	size_t size;
	Uint bitoffs;
	Uint bitsize;
    } bcopy;
    char *buf;
    ErlDrvSizeT len;
    Eterm *objp;
    int offset;
} ErtsIOList2BufState;

#define ERTS_IOLIST_OK 0
#define ERTS_IOLIST_OVERFLOW 1
#define ERTS_IOLIST_TYPE 2
#define ERTS_IOLIST_YIELD 3

Eterm buf_to_intlist(Eterm**, const char*, size_t, Eterm); /* most callers pass plain char*'s */

#define ERTS_IOLIST_TO_BUF_OVERFLOW	(~((ErlDrvSizeT) 0))
#define ERTS_IOLIST_TO_BUF_TYPE_ERROR	(~((ErlDrvSizeT) 1))
#define ERTS_IOLIST_TO_BUF_YIELD	(~((ErlDrvSizeT) 2))
#define ERTS_IOLIST_TO_BUF_FAILED(R) \
    (((R) & (~((ErlDrvSizeT) 3))) == (~((ErlDrvSizeT) 3)))
#define ERTS_IOLIST_TO_BUF_SUCCEEDED(R) \
    (!ERTS_IOLIST_TO_BUF_FAILED((R)))

ErlDrvSizeT erts_iolist_to_buf(Eterm, char*, ErlDrvSizeT);
ErlDrvSizeT erts_iolist_to_buf_yielding(ErtsIOList2BufState *);
int erts_iolist_size_yielding(ErtsIOListState *state);
int erts_iolist_size(Eterm, ErlDrvSizeT *);
Sint is_string(Eterm);
void erl_at_exit(void (*) (void*), void*);
Eterm collect_memory(Process *);
void dump_memory_to_fd(int);
int dump_memory_data(const char *);

Eterm erts_mixed_plus(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_minus(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_times(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_div(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_int_div(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_int_rem(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_band(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bor(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bxor(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bnot(Process* p, Eterm arg);

Eterm erts_gc_mixed_plus(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_minus(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_times(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_div(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_int_div(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_int_rem(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_band(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bor(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bxor(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bnot(Process* p, Eterm* reg, Uint live);

Uint erts_current_reductions(Process* current, Process *p);

int erts_print_system_version(fmtfn_t to, void *arg, Process *c_p);

int erts_hibernate(Process* c_p, Eterm* reg);

ERTS_GLB_FORCE_INLINE int erts_is_literal(Eterm tptr, Eterm *ptr);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_FORCE_INLINE int erts_is_literal(Eterm tptr, Eterm *ptr)
{
    ASSERT(is_boxed(tptr) || is_list(tptr));
    ASSERT(ptr == ptr_val(tptr));

#if defined(ERTS_HAVE_IS_IN_LITERAL_RANGE)
    return erts_is_in_literal_range(ptr);
#elif defined(TAG_LITERAL_PTR)
    return is_literal_ptr(tptr);
#else
#  error Not able to detect literals...
#endif

}

#endif

Eterm erts_msacc_request(Process *c_p, int action, Eterm *threads);

/*
** Call_trace uses this API for the parameter matching functions
*/

#define MatchSetRef(MPSP) 			\
do {						\
    if ((MPSP) != NULL) {			\
	erts_refc_inc(&(MPSP)->intern.refc, 1);	\
    }						\
} while (0)

#define MatchSetUnref(MPSP)					\
do {								\
    if (((MPSP) != NULL)) {                                     \
	erts_bin_release(MPSP);					\
    }								\
} while(0)

#define MatchSetGetSource(MPSP) erts_match_set_get_source(MPSP)

extern Binary *erts_match_set_compile(Process *p, Eterm matchexpr, Eterm MFA);
extern void erts_match_set_release_result(Process* p);
ERTS_GLB_INLINE void erts_match_set_release_result_trace(Process* p, Eterm);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE
void erts_match_set_release_result_trace(Process* p, Eterm pam_result)
{
    if (is_not_immed(pam_result))
        erts_match_set_release_result(p);
}
#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

enum erts_pam_run_flags {
    ERTS_PAM_TMP_RESULT=1,
    ERTS_PAM_COPY_RESULT=2,
    ERTS_PAM_CONTIGUOUS_TUPLE=4,
    ERTS_PAM_IGNORE_TRACE_SILENT=8
};
extern Eterm erts_match_set_run_trace(Process *p,
                                      Process *self,
                                      Binary *mpsp,
                                      Eterm *args, int num_args,
                                      enum erts_pam_run_flags in_flags,
                                      Uint32 *return_flags);
extern Eterm erts_match_set_get_source(Binary *mpsp);
extern void erts_match_prog_foreach_offheap(Binary *b,
					    void (*)(ErlOffHeap *, void *),
					    void *);

#define MATCH_SET_RETURN_TRACE    (0x1) /* return trace requested */
#define MATCH_SET_RETURN_TO_TRACE (0x2) /* Misleading name, it is not actually
					   set by the match program, but by the
					   breakpoint functions */
#define MATCH_SET_EXCEPTION_TRACE (0x4) /* exception trace requested */
#define MATCH_SET_RX_TRACE (MATCH_SET_RETURN_TRACE|MATCH_SET_EXCEPTION_TRACE)

extern erts_driver_t vanilla_driver;
extern erts_driver_t spawn_driver;
extern erts_driver_t forker_driver;
extern erts_driver_t fd_driver;

int erts_beam_jump_table(void);

#define DeclareTmpHeap(VariableName,Size,Process) \
     Eterm VariableName[Size]
#define DeclareTypedTmpHeap(Type,VariableName,Process)	\
     Type VariableName[1]
#define DeclareTmpHeapNoproc(VariableName,Size) \
     Eterm VariableName[Size]
#define UseTmpHeap(Size,Proc) /* Nothing */
#define UnUseTmpHeap(Size,Proc) /* Nothing */
#define UseTmpHeapNoproc(Size) /* Nothing */
#define UnUseTmpHeapNoproc(Size) /* Nothing */

ERTS_GLB_INLINE void dtrace_pid_str(Eterm pid, char *process_buf);
ERTS_GLB_INLINE void dtrace_proc_str(Process *process, char *process_buf);
ERTS_GLB_INLINE void dtrace_port_str(Port *port, char *port_buf);
ERTS_GLB_INLINE void dtrace_fun_decode(Process *process, ErtsCodeMFA *mfa,
				       char *process_buf, char *mfa_buf);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#include "dtrace-wrapper.h"

ERTS_GLB_INLINE void
dtrace_pid_str(Eterm pid, char *process_buf)
{
    if (is_pid(pid))
        erts_snprintf(process_buf, DTRACE_TERM_BUF_SIZE, "<%lu.%lu.%lu>",
                      pid_channel_no(pid),
                      pid_number(pid),
                      pid_serial(pid));
    else if (is_port(pid))
        erts_snprintf(process_buf, DTRACE_TERM_BUF_SIZE, "#Port<%lu.%lu>",
                      port_channel_no(pid),
                      port_number(pid));
}

ERTS_GLB_INLINE void
dtrace_proc_str(Process *process, char *process_buf)
{
    dtrace_pid_str(process->common.id, process_buf);
}

ERTS_GLB_INLINE void
dtrace_port_str(Port *port, char *port_buf)
{
    dtrace_pid_str(port->common.id, port_buf);
}

ERTS_GLB_INLINE void
dtrace_fun_decode(Process *process, ErtsCodeMFA *mfa,
                  char *process_buf, char *mfa_buf)
{
    if (process_buf) {
        dtrace_proc_str(process, process_buf);
    }

    erts_snprintf(mfa_buf, DTRACE_TERM_BUF_SIZE, "%T:%T/%d",
                  mfa->module, mfa->function, mfa->arity);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__GLOBAL_H__ */
