/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
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
#include "benchmark.h"
#include "erl_process.h"
#include "erl_sys_driver.h"
#include "erl_debug.h"
#include "error.h"
#include "erl_utils.h"
#include "erl_port.h"

struct enif_environment_t /* ErlNifEnv */
{
    struct erl_module_nif* mod_nif;
    Process* proc;
    Eterm* hp;
    Eterm* hp_end;
    ErlHeapFragment* heap_frag;
    int fpe_was_unmasked;
    struct enif_tmp_obj_t* tmp_obj_list;
};
extern void erts_pre_nif(struct enif_environment_t*, Process*,
			 struct erl_module_nif*);
extern void erts_post_nif(struct enif_environment_t* env);
extern Eterm erts_nif_taints(Process* p);
extern void erts_print_nif_taints(int to, void* to_arg);
void erts_unload_nif(struct erl_module_nif* nif);
extern void erl_nif_init(void);

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
    Eterm   heap[REF_THING_SIZE];    /* "ref heap" */
    struct  de_proc_entry *next;
} DE_ProcEntry;

typedef struct {
    void         *handle;             /* Handle for DLL or SO (for dyn. drivers). */
    DE_ProcEntry *procs;              /* List of pids that have loaded this driver,
				         or that wait for it to change state */
    erts_refc_t  refc;                /* Number of ports/processes having
					 references to the driver */
    erts_smp_atomic32_t port_count;   /* Number of ports using the driver */
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
    char *name;
    struct {
	int major;
	int minor;
    } version;
    int flags;
    DE_Handle *handle;
#ifdef ERTS_SMP
    erts_smp_mtx_t *lock;
#endif
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
    void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
		  ErlDrvEventData event_data);
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
    void (*timeout)(ErlDrvData drv_data);
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data); /* Might be NULL */ 
    void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
    void (*stop_select)(ErlDrvEvent event, void*); /* Might be NULL */
};

extern erts_driver_t *driver_list;
extern erts_smp_rwmtx_t erts_driver_list_lock;

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
** Just like the driver binary but with initial flags
** Note that the two structures Binary and ErlDrvBinary HAVE to
** be equal except for extra fields in the beginning of the struct.
** ErlDrvBinary is defined in erl_driver.h.
** When driver_alloc_binary is called, a Binary is allocated, but 
** the pointer returned is to the address of the first element that
** also occurs in the ErlDrvBinary struct (driver.*binary takes care if this).
** The driver need never know about additions to the internal Binary of the
** emulator. One should however NEVER be sloppy when mixing ErlDrvBinary
** and Binary, the macros below can convert one type to the other, as they both
** in reality are equal.
*/

#ifdef ARCH_32
 /* *DO NOT USE* only for alignment. */
#define ERTS_BINARY_STRUCT_ALIGNMENT Uint32 align__;
#else
#define ERTS_BINARY_STRUCT_ALIGNMENT
#endif

/* Add fields in ERTS_INTERNAL_BINARY_FIELDS, otherwise the drivers crash */
#define ERTS_INTERNAL_BINARY_FIELDS				\
    UWord flags;							\
    erts_refc_t refc;						\
    ERTS_BINARY_STRUCT_ALIGNMENT

typedef struct binary {
    ERTS_INTERNAL_BINARY_FIELDS
    SWord orig_size;
    char orig_bytes[1]; /* to be continued */
} Binary;

#define ERTS_SIZEOF_Binary(Sz) \
    (offsetof(Binary,orig_bytes) + (Sz))

typedef struct {
    ERTS_INTERNAL_BINARY_FIELDS
    SWord orig_size;
    void (*destructor)(Binary *);
    char magic_bin_data[1];
} ErtsMagicBinary;

typedef union {
    Binary binary;
    ErtsMagicBinary magic_binary;
    struct {
	ERTS_INTERNAL_BINARY_FIELDS
	ErlDrvBinary binary;
    } driver;
} ErtsBinary;

/*
 * 'Binary' alignment:
 *   Address of orig_bytes[0] of a Binary should always be 8-byte aligned.
 * It is assumed that the flags, refc, and orig_size fields are 4 bytes on
 * 32-bits architectures and 8 bytes on 64-bits architectures.
 */

#define ERTS_MAGIC_BIN_DESTRUCTOR(BP) \
  ((ErtsBinary *) (BP))->magic_binary.destructor
#define ERTS_MAGIC_BIN_DATA(BP) \
  ((void *) ((ErtsBinary *) (BP))->magic_binary.magic_bin_data)
#define ERTS_MAGIC_BIN_DATA_SIZE(BP) \
  ((BP)->orig_size - sizeof(void (*)(Binary *)))
#define ERTS_MAGIC_BIN_ORIG_SIZE(Sz) \
  (sizeof(void (*)(Binary *)) + (Sz))
#define ERTS_MAGIC_BIN_SIZE(Sz) \
  (offsetof(ErtsMagicBinary,magic_bin_data) + (Sz))
#define ERTS_MAGIC_BIN_FROM_DATA(DATA) \
  ((ErtsBinary*)((char*)(DATA) - offsetof(ErtsMagicBinary,magic_bin_data)))

#define Binary2ErlDrvBinary(B) (&((ErtsBinary *) (B))->driver.binary)
#define ErlDrvBinary2Binary(D) ((Binary *) \
				(((char *) (D)) \
				 - offsetof(ErtsBinary, driver.binary)))

/* A "magic" binary flag */
#define BIN_FLAG_MAGIC      1
#define BIN_FLAG_USR1       2 /* Reserved for use by different modules too mark */
#define BIN_FLAG_USR2       4 /*  certain binaries as special (used by ets) */
#define BIN_FLAG_DRV        8

/*
 * This structure represents one type of a binary in a process.
 */

typedef struct proc_bin {
    Eterm thing_word;		/* Subtag REFC_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
#if HALFWORD_HEAP
    void* dummy_ptr_padding__;
#endif
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

ERTS_GLB_INLINE Eterm erts_mk_magic_binary_term(Eterm **hpp,
						ErlOffHeap *ohp,
						Binary *mbp);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Eterm
erts_mk_magic_binary_term(Eterm **hpp, ErlOffHeap *ohp, Binary *mbp)
{
    ProcBin *pb = (ProcBin *) *hpp;
    *hpp += PROC_BIN_SIZE;

    ASSERT(mbp->flags & BIN_FLAG_MAGIC);

    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = ohp->first;
    ohp->first = (struct erl_off_heap_header*) pb;
    pb->val = mbp;
    pb->bytes = (byte *) mbp->orig_bytes;
    pb->flags = 0;

    erts_refc_inc(&mbp->refc, 1);

    return make_binary(pb);    
}

#endif

#define ERTS_TERM_IS_MAGIC_BINARY(T) \
  (is_binary((T)) \
   && (thing_subtag(*binary_val((T))) == REFC_BINARY_SUBTAG) \
   && (((ProcBin *) binary_val((T)))->val->flags & BIN_FLAG_MAGIC))


union erl_off_heap_ptr {
    struct erl_off_heap_header* hdr;
    ProcBin *pb;
    struct erl_fun_thing* fun;
    struct external_thing_* ext;
    Eterm* ep;
    void* voidp;
};

/* controls warning mapping in error_logger */

extern Eterm node_cookie;
extern Uint display_items;	/* no of items to display in traces etc */

extern int erts_backtrace_depth;
extern erts_smp_atomic32_t erts_max_gen_gcs;

extern int erts_disable_tolerant_timeofday;

extern int bif_reductions;      /* reductions + fcalls (when doing call_bif) */
extern int stackdump_on_exit;

/*
 * Here is an implementation of a lightweiht stack.
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

typedef struct {
    Eterm* start;
    Eterm* sp;
    Eterm* end;
    ErtsAlcType_t alloc_type;
}ErtsEStack;

#define DEF_ESTACK_SIZE (16)

void erl_grow_estack(ErtsEStack*, Eterm* def_stack);
#define ESTK_CONCAT(a,b) a##b
#define ESTK_DEF_STACK(s) ESTK_CONCAT(s,_default_estack)

#define DECLARE_ESTACK(s)				\
    Eterm ESTK_DEF_STACK(s)[DEF_ESTACK_SIZE];		\
    ErtsEStack s = {					\
        ESTK_DEF_STACK(s),  /* start */ 		\
        ESTK_DEF_STACK(s),  /* sp */			\
        ESTK_DEF_STACK(s) + DEF_ESTACK_SIZE, /* end */	\
        ERTS_ALC_T_ESTACK /* alloc_type */		\
    }

#define ESTACK_CHANGE_ALLOCATOR(s,t)					\
do {									\
    if (s.start != ESTK_DEF_STACK(s)) {					\
	erl_exit(1, "Internal error - trying to change allocator "	\
		 "type of active estack\n");				\
    }									\
    s.alloc_type = (t);							\
 } while (0)

#define DESTROY_ESTACK(s)				\
do {							\
    if (s.start != ESTK_DEF_STACK(s)) {			\
	erts_free(s.alloc_type, s.start); 		\
    }							\
} while(0)


/*
 * Do not free the stack after this, it may have pointers into what
 * was saved in 'dst'.
 */
#define ESTACK_SAVE(s,dst)\
do {\
    if (s.start == ESTK_DEF_STACK(s)) {\
	UWord _wsz = ESTACK_COUNT(s);\
	(dst)->start = erts_alloc(s.alloc_type,\
				  DEF_ESTACK_SIZE * sizeof(Eterm));\
	memcpy((dst)->start, s.start,_wsz*sizeof(Eterm));\
	(dst)->sp = (dst)->start + _wsz;\
	(dst)->end = (dst)->start + DEF_ESTACK_SIZE;\
	(dst)->alloc_type = s.alloc_type;\
    } else\
        *(dst) = s;\
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
    ASSERT(s.start == ESTK_DEF_STACK(s));	\
    s = *(src);  /* struct copy */		\
    (src)->start = NULL;			\
    ASSERT(s.sp >= s.start);			\
    ASSERT(s.sp <= s.end);			\
} while (0)

#define ESTACK_IS_STATIC(s) (s.start == ESTK_DEF_STACK(s)))

#define ESTACK_PUSH(s, x)				\
do {							\
    if (s.sp == s.end) {				\
	erl_grow_estack(&s, ESTK_DEF_STACK(s)); 	\
    }							\
    *s.sp++ = (x);					\
} while(0)

#define ESTACK_PUSH2(s, x, y)			\
do {						\
    if (s.sp > s.end - 2) {			\
	erl_grow_estack(&s, ESTK_DEF_STACK(s)); \
    }						\
    *s.sp++ = (x);				\
    *s.sp++ = (y);				\
} while(0)

#define ESTACK_PUSH3(s, x, y, z)		\
do {						\
    if (s.sp > s.end - 3) {			\
	erl_grow_estack(&s, ESTK_DEF_STACK(s)); \
    }						\
    *s.sp++ = (x);				\
    *s.sp++ = (y);				\
    *s.sp++ = (z);				\
} while(0)

#define ESTACK_COUNT(s) (s.sp - s.start)
#define ESTACK_ISEMPTY(s) (s.sp == s.start)
#define ESTACK_POP(s) (*(--s.sp))


/*
 * WSTACK: same as ESTACK but with UWord instead of Eterm
 */

typedef struct {
    UWord* wstart;
    UWord* wsp;
    UWord* wend;
    ErtsAlcType_t alloc_type;
}ErtsWStack;

#define DEF_WSTACK_SIZE (16)

void erl_grow_wstack(ErtsWStack*, UWord* def_stack);
#define WSTK_CONCAT(a,b) a##b
#define WSTK_DEF_STACK(s) WSTK_CONCAT(s,_default_wstack)

#define DECLARE_WSTACK(s)				\
    UWord WSTK_DEF_STACK(s)[DEF_WSTACK_SIZE];		\
    ErtsWStack s = {					\
        WSTK_DEF_STACK(s),  /* wstart */ 		\
        WSTK_DEF_STACK(s),  /* wsp */			\
        WSTK_DEF_STACK(s) + DEF_WSTACK_SIZE, /* wend */	\
        ERTS_ALC_T_ESTACK /* alloc_type */		\
    }

#define WSTACK_CHANGE_ALLOCATOR(s,t)					\
do {									\
    if (s.wstart != WSTK_DEF_STACK(s)) {				\
	erl_exit(1, "Internal error - trying to change allocator "	\
		 "type of active wstack\n");				\
    }									\
    s.alloc_type = (t);							\
 } while (0)

#define DESTROY_WSTACK(s)				\
do {							\
    if (s.wstart != WSTK_DEF_STACK(s)) {		\
	erts_free(s.alloc_type, s.wstart); 		\
    }							\
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
	memcpy((dst)->wstart, s.wstart,_wsz*sizeof(UWord));\
	(dst)->wsp = (dst)->wstart + _wsz;\
	(dst)->wend = (dst)->wstart + DEF_WSTACK_SIZE;\
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

#define WSTACK_IS_STATIC(s) (s.wstart == WSTK_DEF_STACK(s)))

#define WSTACK_PUSH(s, x)				\
do {							\
    if (s.wsp == s.wend) {				\
	erl_grow_wstack(&s, WSTK_DEF_STACK(s)); 	\
    }							\
    *s.wsp++ = (x);					\
} while(0)

#define WSTACK_PUSH2(s, x, y)			\
do {						\
    if (s.wsp > s.wend - 2) {			\
	erl_grow_wstack(&s, WSTK_DEF_STACK(s)); \
    }						\
    *s.wsp++ = (x);				\
    *s.wsp++ = (y);				\
} while(0)

#define WSTACK_PUSH3(s, x, y, z)		\
do {						\
    if (s.wsp > s.wend - 3) {	\
	erl_grow_wstack(&s, WSTK_DEF_STACK(s)); \
    }						\
    *s.wsp++ = (x);				\
    *s.wsp++ = (y);				\
    *s.wsp++ = (z);				\
} while(0)

#define WSTACK_COUNT(s) (s.wsp - s.wstart)
#define WSTACK_ISEMPTY(s) (s.wsp == s.wstart)
#define WSTACK_POP(s) (*(--s.wsp))


/* binary.c */

void erts_emasculate_writable_binary(ProcBin* pb);
Eterm erts_new_heap_binary(Process *p, byte *buf, int len, byte** datap);
Eterm erts_new_mso_binary(Process*, byte*, int);
Eterm new_binary(Process*, byte*, Uint);
Eterm erts_realloc_binary(Eterm bin, size_t size);

/* erl_bif_info.c */

Eterm
erts_bld_port_info(Eterm **hpp,
		   ErlOffHeap *ohp,
		   Uint *szp,
		   Port *prt,
		   Eterm item); 

void erts_bif_info_init(void);

/* bif.c */
Eterm erts_make_ref(Process *);
Eterm erts_make_ref_in_buffer(Eterm buffer[REF_THING_SIZE]);
void erts_make_ref_in_array(Uint32 ref[ERTS_MAX_REF_NUMBERS]);

ERTS_GLB_INLINE Eterm
erts_proc_store_ref(Process *c_p, Uint32 ref[ERTS_MAX_REF_NUMBERS]);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Eterm
erts_proc_store_ref(Process *c_p, Uint32 ref[ERTS_MAX_REF_NUMBERS])
{
    Eterm *hp = HAlloc(c_p, REF_THING_SIZE);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);
    return make_internal_ref(hp);
}

#endif

void erts_queue_monitor_message(Process *,
				ErtsProcLocks*,
				Eterm,
				Eterm,
				Eterm,
				Eterm);
void erts_init_trap_export(Export* ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(Process*,Eterm*));
void erts_init_bif(void);
Eterm erl_send(Process *p, Eterm to, Eterm msg);

/* erl_bif_op.c */

Eterm erl_is_function(Process* p, Eterm arg1, Eterm arg2);

/* beam_bif_load.c */
Eterm erts_check_process_code(Process *c_p, Eterm module, int allow_gc, int *redsp);


/* beam_load.c */
typedef struct {
    BeamInstr* current;		/* Pointer to: Mod, Name, Arity */
    Uint needed;		/* Heap space needed for entire tuple */
    Uint32 loc;			/* Location in source code */
    Eterm* fname_ptr;		/* Pointer to fname table */
} FunctionInfo;

Binary* erts_alloc_loader_state(void);
Eterm erts_module_for_prepared_code(Binary* magic);
Eterm erts_prepare_loading(Binary* loader_state,  Process *c_p,
			   Eterm group_leader, Eterm* modp,
			   byte* code, Uint size);
Eterm erts_finish_loading(Binary* loader_state, Process* c_p,
			  ErtsProcLocks c_p_locks, Eterm* modp);
Eterm erts_preload_module(Process *c_p, ErtsProcLocks c_p_locks,
			  Eterm group_leader, Eterm* mod, byte* code, Uint size);
void init_load(void);
BeamInstr* find_function_from_pc(BeamInstr* pc);
Eterm* erts_build_mfa_item(FunctionInfo* fi, Eterm* hp,
			   Eterm args, Eterm* mfa_p);
void erts_set_current_function(FunctionInfo* fi, BeamInstr* current);
Eterm erts_module_info_0(Process* p, Eterm module);
Eterm erts_module_info_1(Process* p, Eterm module, Eterm what);
Eterm erts_make_stub_module(Process* p, Eterm Mod, Eterm Beam, Eterm Info);

/* beam_ranges.c */
void erts_init_ranges(void);
void erts_start_staging_ranges(void);
void erts_end_staging_ranges(int commit);
void erts_update_ranges(BeamInstr* code, Uint size);
void erts_remove_from_ranges(BeamInstr* code);
UWord erts_ranges_sz(void);
void erts_lookup_function_info(FunctionInfo* fi, BeamInstr* pc, int full_info);

/* break.c */
void init_break_handler(void);
void erts_set_ignore_break(void);
void erts_replace_intr(void);
void process_info(int, void *);
void print_process_info(int, void *, Process*);
void info(int, void *);
void loaded(int, void *);

/* config.c */

__decl_noreturn void __noreturn erl_exit(int n, char*, ...);
__decl_noreturn void __noreturn erl_exit_flush_async(int n, char*, ...);
void erl_error(char*, va_list);

/* copy.c */
Eterm copy_object(Eterm, Process*);

#if HALFWORD_HEAP
Uint size_object_rel(Eterm, Eterm*);
#  define size_object(A) size_object_rel(A,NULL)

Eterm copy_struct_rel(Eterm, Uint, Eterm**, ErlOffHeap*, Eterm* src_base, Eterm* dst_base);
#  define copy_struct(OBJ,SZ,HPP,OH) copy_struct_rel(OBJ,SZ,HPP,OH, NULL,NULL)

Eterm copy_shallow_rel(Eterm*, Uint, Eterm**, ErlOffHeap*, Eterm* src_base);
#  define copy_shallow(A,B,C,D) copy_shallow_rel(A,B,C,D,NULL)

#else /* !HALFWORD_HEAP */

Uint size_object(Eterm);
#  define size_object_rel(A,B) size_object(A)

Eterm copy_struct(Eterm, Uint, Eterm**, ErlOffHeap*);
#  define copy_struct_rel(OBJ,SZ,HPP,OH, SB,DB) copy_struct(OBJ,SZ,HPP,OH)

Eterm copy_shallow(Eterm*, Uint, Eterm**, ErlOffHeap*);
#  define copy_shallow_rel(A,B,C,D, BASE) copy_shallow(A,B,C,D)

#endif


void move_multi_frags(Eterm** hpp, ErlOffHeap*, ErlHeapFragment* first,
		      Eterm* refs, unsigned nrefs);

/* Utilities */
extern void erts_delete_nodes_monitors(Process *, ErtsProcLocks);
extern Eterm erts_monitor_nodes(Process *, Eterm, Eterm);
extern Eterm erts_processes_monitoring_nodes(Process *);
extern int erts_do_net_exits(DistEntry*, Eterm);
extern int distribution_info(int, void *);
extern int is_node_name_atom(Eterm a);

extern int erts_net_message(Port *, DistEntry *,
			    byte *, ErlDrvSizeT, byte *, ErlDrvSizeT);

extern void init_dist(void);
extern int stop_dist(void);

void erl_progressf(char* format, ...);

#ifdef MESS_DEBUG
void print_pass_through(int, byte*, int);
#endif

/* beam_emu.c */
int catchlevel(Process*);
void init_emulator(void);
void process_main(void);
Eterm build_stacktrace(Process* c_p, Eterm exc);
Eterm expand_error_value(Process* c_p, Uint freason, Eterm Value);
void erts_save_stacktrace(Process* p, struct StackTrace* s, int depth);

/* erl_init.c */

typedef struct {
    Eterm delay_time;
    int context_reds;
    int input_reds;
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
extern int erts_use_sender_punish;
void erts_short_init(void);
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

/* ggc.c */

void erts_gc_info(ErtsGCInfo *gcip);
void erts_init_gc(void);
int erts_garbage_collect(Process*, int, Eterm*, int);
void erts_garbage_collect_hibernate(Process* p);
Eterm erts_gc_after_bif_call(Process* p, Eterm result, Eterm* regs, Uint arity);
void erts_garbage_collect_literals(Process* p, Eterm* literals,
				   Uint lit_size,
				   struct erl_off_heap_header* oh);
Uint erts_next_heap_size(Uint, Uint);
Eterm erts_heap_sizes(Process* p);

void erts_offset_off_heap(ErlOffHeap *, Sint, Eterm*, Eterm*);
void erts_offset_heap_ptr(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_offset_heap(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_free_heap_frags(Process* p);

/* io.c */

typedef struct {
    char *name;
    char *driver_name;
} ErtsPortNames;

#define ERTS_SPAWN_DRIVER 1
#define ERTS_SPAWN_EXECUTABLE 2
#define ERTS_SPAWN_ANY (ERTS_SPAWN_DRIVER | ERTS_SPAWN_EXECUTABLE)
int erts_add_driver_entry(ErlDrvEntry *drv, DE_Handle *handle, int driver_list_locked);
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

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_COUNT)
void erts_lcnt_enable_io_lock_count(int enable);
#endif

/* driver_tab.c */
typedef void *(*ErtsStaticNifInitFPtr)(void);
ErtsStaticNifInitFPtr erts_static_nif_get_nif_init(const char *name, int len);
int erts_is_static_nif(void *handle);
void erts_init_static_drivers(void);

/* erl_drv_thread.c */
void erl_drv_thr_init(void);

/* utils.c */
void erts_cleanup_offheap(ErlOffHeap *offheap);

Uint64 erts_timestamp_millis(void);

Export* erts_find_function(Eterm, Eterm, unsigned int, ErtsCodeIndex);

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

/* external.c */
void erts_init_external(void);

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
Eterm erts_convert_native_to_filename(Process *p, byte *bytes);
Eterm erts_utf8_to_list(Process *p, Uint num, byte *bytes, Uint sz, Uint left,
			Uint *num_built, Uint *num_eaten, Eterm tail);
int erts_utf8_to_latin1(byte* dest, const byte* source, int slen);
#define ERTS_UTF8_OK 0
#define ERTS_UTF8_INCOMPLETE 1
#define ERTS_UTF8_ERROR 2
#define ERTS_UTF8_ANALYZE_MORE 3
#define ERTS_UTF8_OK_MAX_CHARS 4

void bin_write(int, void*, byte*, size_t);
int intlist_to_buf(Eterm, char*, int); /* most callers pass plain char*'s */

struct Sint_buf {
#if defined(ARCH_64) && !HALFWORD_HEAP
    char s[22];
#else
    char s[12];
#endif
};	
char* Sint_to_buf(Sint, struct Sint_buf*);

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
int is_string(Eterm);
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

Eterm erts_gc_length_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bit_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_byte_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_map_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_abs_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_float_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_round_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_trunc_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_binary_part_3(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_binary_part_2(Process* p, Eterm* reg, Uint live);

Uint erts_current_reductions(Process* current, Process *p);

int erts_print_system_version(int to, void *arg, Process *c_p);

int erts_hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg);

/*
** Call_trace uses this API for the parameter matching functions
*/

#define MatchSetRef(MPSP) 			\
do {						\
    if ((MPSP) != NULL) {			\
	erts_refc_inc(&(MPSP)->refc, 1);	\
    }						\
} while (0)

#define MatchSetUnref(MPSP)					\
do {								\
    if (((MPSP) != NULL) && erts_refc_dectest(&(MPSP)->refc, 0) <= 0) { \
	erts_bin_free(MPSP);					\
    }								\
} while(0)

#define MatchSetGetSource(MPSP) erts_match_set_get_source(MPSP)

extern Binary *erts_match_set_compile(Process *p, Eterm matchexpr);
Eterm erts_match_set_lint(Process *p, Eterm matchexpr); 
extern void erts_match_set_release_result(Process* p);

enum erts_pam_run_flags {
    ERTS_PAM_TMP_RESULT=1,
    ERTS_PAM_COPY_RESULT=2,
    ERTS_PAM_CONTIGUOUS_TUPLE=4,
    ERTS_PAM_IGNORE_TRACE_SILENT=8
};
extern Eterm erts_match_set_run(Process *p, Binary *mpsp, 
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
extern erts_driver_t fd_driver;

int erts_beam_jump_table(void);

/* Should maybe be placed in erl_message.h, but then we get an include mess. */
ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap_state(Uint size,
			      ErlHeapFragment **bpp,
			      ErlOffHeap **ohpp,
			      Process *receiver,
			      ErtsProcLocks *receiver_locks,
			      erts_aint32_t *statep);

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap(Uint size,
			ErlHeapFragment **bpp,
			ErlOffHeap **ohpp,
			Process *receiver,
			ErtsProcLocks *receiver_locks);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

/*
 * NOTE: erts_alloc_message_heap() releases msg q and status
 *       lock on receiver without ensuring that other locks are
 *       held. User is responsible to ensure that the receiver
 *       pointer cannot become invalid until after message has
 *       been passed. This is normal done either by increasing
 *       reference count on process (preferred) or by holding
 *       main or link lock over the whole message passing
 *       operation.
 */

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap_state(Uint size,
			      ErlHeapFragment **bpp,
			      ErlOffHeap **ohpp,
			      Process *receiver,
			      ErtsProcLocks *receiver_locks,
			      erts_aint32_t *statep)
{
    Eterm *hp;
    erts_aint32_t state;
#ifdef ERTS_SMP
    int locked_main = 0;
    state = erts_smp_atomic32_read_acqb(&receiver->state);
    if (statep)
	*statep = state;
    if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT))
	goto allocate_in_mbuf;
#endif

    if (size > (Uint) INT_MAX)
	erl_exit(ERTS_ABORT_EXIT, "HUGE size (%beu)\n", size);

    if (
#if defined(ERTS_SMP)
	*receiver_locks & ERTS_PROC_LOCK_MAIN
#else
	1
#endif
	) {
#ifdef ERTS_SMP
    try_allocate_on_heap:
#endif
	state = erts_smp_atomic32_read_nob(&receiver->state);
	if (statep)
	    *statep = state;
	if ((state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT))
	    || (receiver->flags & F_DISABLE_GC)
	    || HEAP_LIMIT(receiver) - HEAP_TOP(receiver) <= size) {
	    /*
	     * The heap is either potentially in an inconsistent
	     * state, or not large enough.
	     */
#ifdef ERTS_SMP
	    if (locked_main) {
		*receiver_locks &= ~ERTS_PROC_LOCK_MAIN;
		erts_smp_proc_unlock(receiver, ERTS_PROC_LOCK_MAIN);
	    }
#endif
	    goto allocate_in_mbuf;
	}
	hp = HEAP_TOP(receiver);
	HEAP_TOP(receiver) = hp + size;
	*bpp = NULL;
	*ohpp = &MSO(receiver);
    }
#ifdef ERTS_SMP
    else if (erts_smp_proc_trylock(receiver, ERTS_PROC_LOCK_MAIN) == 0) {
	locked_main = 1;
	*receiver_locks |= ERTS_PROC_LOCK_MAIN;
	goto try_allocate_on_heap;
    }
#endif
    else {
	ErlHeapFragment *bp;
    allocate_in_mbuf:
	bp = new_message_buffer(size);
	hp = bp->mem;
	*bpp = bp;
	*ohpp = &bp->off_heap;
    }

    return hp;
}

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap(Uint size,
			ErlHeapFragment **bpp,
			ErlOffHeap **ohpp,
			Process *receiver,
			ErtsProcLocks *receiver_locks)
{
    return erts_alloc_message_heap_state(size, bpp, ohpp, receiver,
					 receiver_locks, NULL);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#if !HEAP_ON_C_STACK
#  if defined(DEBUG)
#    define DeclareTmpHeap(VariableName,Size,Process) \
       Eterm *VariableName = erts_debug_allocate_tmp_heap(Size,Process)
#    define DeclareTypedTmpHeap(Type,VariableName,Process)		\
      Type *VariableName = (Type *) erts_debug_allocate_tmp_heap(sizeof(Type)/sizeof(Eterm),Process)
#    define DeclareTmpHeapNoproc(VariableName,Size) \
       Eterm *VariableName = erts_debug_allocate_tmp_heap(Size,NULL)
#    define UseTmpHeap(Size,Proc) \
       do { \
         erts_debug_use_tmp_heap((Size),(Proc)); \
       } while (0)
#    define UnUseTmpHeap(Size,Proc) \
       do { \
         erts_debug_unuse_tmp_heap((Size),(Proc)); \
       } while (0)
#    define UseTmpHeapNoproc(Size) \
       do { \
         erts_debug_use_tmp_heap(Size,NULL); \
       } while (0)
#    define UnUseTmpHeapNoproc(Size) \
       do { \
         erts_debug_unuse_tmp_heap(Size,NULL); \
       } while (0)
#  else
#    define DeclareTmpHeap(VariableName,Size,Process) \
       Eterm *VariableName = (ERTS_PROC_GET_SCHDATA(Process)->tmp_heap)+(ERTS_PROC_GET_SCHDATA(Process)->num_tmp_heap_used)
#    define DeclareTypedTmpHeap(Type,VariableName,Process)		\
      Type *VariableName = (Type *) (ERTS_PROC_GET_SCHDATA(Process)->tmp_heap)+(ERTS_PROC_GET_SCHDATA(Process)->num_tmp_heap_used)
#    define DeclareTmpHeapNoproc(VariableName,Size) \
       Eterm *VariableName = (erts_get_scheduler_data()->tmp_heap)+(erts_get_scheduler_data()->num_tmp_heap_used)
#    define UseTmpHeap(Size,Proc) \
       do { \
         ERTS_PROC_GET_SCHDATA(Proc)->num_tmp_heap_used += (Size); \
       } while (0)
#    define UnUseTmpHeap(Size,Proc) \
       do { \
         ERTS_PROC_GET_SCHDATA(Proc)->num_tmp_heap_used -= (Size); \
       } while (0)
#    define UseTmpHeapNoproc(Size) \
       do { \
         erts_get_scheduler_data()->num_tmp_heap_used += (Size); \
       } while (0)
#    define UnUseTmpHeapNoproc(Size) \
       do { \
         erts_get_scheduler_data()->num_tmp_heap_used -= (Size); \
       } while (0)


#  endif

#else
#  define DeclareTmpHeap(VariableName,Size,Process) \
     Eterm VariableName[Size]
#  define DeclareTypedTmpHeap(Type,VariableName,Process)	\
     Type VariableName[1]
#  define DeclareTmpHeapNoproc(VariableName,Size) \
     Eterm VariableName[Size]
#  define UseTmpHeap(Size,Proc) /* Nothing */
#  define UnUseTmpHeap(Size,Proc) /* Nothing */
#  define UseTmpHeapNoproc(Size) /* Nothing */
#  define UnUseTmpHeapNoproc(Size) /* Nothing */
#endif /* HEAP_ON_C_STACK */

ERTS_GLB_INLINE void dtrace_pid_str(Eterm pid, char *process_buf);
ERTS_GLB_INLINE void dtrace_proc_str(Process *process, char *process_buf);
ERTS_GLB_INLINE void dtrace_port_str(Port *port, char *port_buf);
ERTS_GLB_INLINE void dtrace_fun_decode(Process *process,
				       Eterm module, Eterm function, int arity,
				       char *process_buf, char *mfa_buf);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#include "dtrace-wrapper.h"

ERTS_GLB_INLINE void
dtrace_pid_str(Eterm pid, char *process_buf)
{
    erts_snprintf(process_buf, DTRACE_TERM_BUF_SIZE, "<%lu.%lu.%lu>",
                  pid_channel_no(pid),
                  pid_number(pid),
                  pid_serial(pid));
}

ERTS_GLB_INLINE void
dtrace_proc_str(Process *process, char *process_buf)
{
    dtrace_pid_str(process->common.id, process_buf);
}

ERTS_GLB_INLINE void
dtrace_port_str(Port *port, char *port_buf)
{
    erts_snprintf(port_buf, DTRACE_TERM_BUF_SIZE, "#Port<%lu.%lu>",
                  port_channel_no(port->common.id),
                  port_number(port->common.id));
}

ERTS_GLB_INLINE void
dtrace_fun_decode(Process *process,
                  Eterm module, Eterm function, int arity,
                  char *process_buf, char *mfa_buf)
{
    if (process_buf) {
        dtrace_proc_str(process, process_buf);
    }

    erts_snprintf(mfa_buf, DTRACE_TERM_BUF_SIZE, "%T:%T/%d",
                  module, function, arity);
}
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__GLOBAL_H__ */
