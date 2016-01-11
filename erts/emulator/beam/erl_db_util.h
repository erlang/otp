/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2013. All Rights Reserved.
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

#ifndef _DB_UTIL_H
#define _DB_UTIL_H

#include "global.h"
#include "erl_message.h"

/*#define HARDDEBUG 1*/

#ifdef DEBUG
/*
** DMC_DEBUG does NOT need DEBUG, but DEBUG needs DMC_DEBUG
*/
#define DMC_DEBUG 1
#endif

/*
 * These values can be returned from the functions performing the 
 * BIF operation for different types of tables. When the
 * actual operations have been performed, the BIF function
 * checks for negative returns and issues BIF_ERRORS based 
 * upon these values.
 */
#define DB_ERROR_NONE      0     /* No error */
#define DB_ERROR_BADITEM  -1     /* The item was malformed ie no 
				   tuple or to small*/
#define DB_ERROR_BADTABLE -2     /* The Table is inconsisitent */
#define DB_ERROR_SYSRES   -3     /* Out of system resources */
#define DB_ERROR_BADKEY   -4     /* Returned if a key that should
				    exist does not. */
#define DB_ERROR_BADPARAM  -5     /* Returned if a specified slot does 
				     not exist (hash table only) or
				     the state parameter in db_match_object
				     is broken.*/
#define DB_ERROR_UNSPEC   -10    /* Unspecified error */

/*#define DEBUG_CLONE*/

/*
 * A datatype for a database entry stored out of a process heap
 */
typedef struct db_term {
    struct erl_off_heap_header* first_oh; /* Off heap data for term. */
    Uint size;		   /* Heap size of term in "words" */
#ifdef DEBUG_CLONE
    Eterm* debug_clone;    /* An uncompressed copy */
#endif
    Eterm tpl[1];          /* Term data. Top tuple always first */

    /* Compression: is_immed and key element are uncompressed.
       Compressed elements are stored in external format after each other
       last in dbterm. The top tuple elements contains byte offsets, to
       the start of the data, tagged as headers.
       The allocated size of the dbterm in bytes is stored at tpl[arity+1].
     */
} DbTerm;

union db_table;
typedef union db_table DbTable;

#define DB_MUST_RESIZE 1
#define DB_NEW_OBJECT 2

/* Info about a database entry while it's being updated
 * (by update_counter or update_element)
 */
typedef struct {
    DbTable* tb;
    DbTerm* dbterm;
    void** bp;         /* {Hash|Tree}DbTerm** */
    Uint new_size;
    int flags;
    void* lck;
} DbUpdateHandle;


typedef struct db_table_method
{
    int (*db_create)(Process *p, DbTable* tb);
    int (*db_first)(Process* p, 
		    DbTable* tb, /* [in out] */ 
		    Eterm* ret   /* [out] */);
    int (*db_next)(Process* p, 
		   DbTable* tb, /* [in out] */
		   Eterm key,   /* [in] */
		   Eterm* ret /* [out] */);
    int (*db_last)(Process* p, 
		   DbTable* tb, /* [in out] */
		   Eterm* ret   /* [out] */);
    int (*db_prev)(Process* p, 
		   DbTable* tb, /* [in out] */
		   Eterm key, 
		   Eterm* ret);
    int (*db_put)(DbTable* tb, /* [in out] */ 
		  Eterm obj,
		  int key_clash_fail); /* DB_ERROR_BADKEY if key exists */ 
    int (*db_get)(Process* p, 
		  DbTable* tb, /* [in out] */ 
		  Eterm key, 
		  Eterm* ret);
    int (*db_get_element)(Process* p, 
			  DbTable* tb, /* [in out] */ 
			  Eterm key, 
			  int index, 
			  Eterm* ret);
    int (*db_member)(DbTable* tb, /* [in out] */ 
		     Eterm key, 
		     Eterm* ret);
    int (*db_erase)(DbTable* tb,  /* [in out] */ 
		    Eterm key, 
		    Eterm* ret);
    int (*db_erase_object)(DbTable* tb, /* [in out] */ 
			   Eterm obj,
			   Eterm* ret);
    int (*db_slot)(Process* p, 
		   DbTable* tb, /* [in out] */ 
		   Eterm slot, 
		   Eterm* ret);
    int (*db_select_chunk)(Process* p, 
			   DbTable* tb, /* [in out] */ 
			   Eterm pattern,
			   Sint chunk_size,
			   int reverse,
			   Eterm* ret);
    int (*db_select)(Process* p, 
		     DbTable* tb, /* [in out] */ 
		     Eterm pattern,
		     int reverse,
		     Eterm* ret);
    int (*db_select_delete)(Process* p, 
			    DbTable* tb, /* [in out] */ 
			    Eterm pattern,
			    Eterm* ret);
    int (*db_select_continue)(Process* p, 
			      DbTable* tb, /* [in out] */ 
			      Eterm continuation,
			      Eterm* ret);
    int (*db_select_delete_continue)(Process* p, 
				     DbTable* tb, /* [in out] */ 
				     Eterm continuation,
				     Eterm* ret);
    int (*db_select_count)(Process* p, 
			   DbTable* tb, /* [in out] */ 
			   Eterm pattern, 
			   Eterm* ret);
    int (*db_select_count_continue)(Process* p, 
				    DbTable* tb, /* [in out] */ 
				    Eterm continuation, 
				    Eterm* ret);
    int (*db_take)(Process *, DbTable *, Eterm, Eterm *);

    int (*db_delete_all_objects)(Process* p,
				 DbTable* db /* [in out] */ );

    int (*db_free_table)(DbTable* db /* [in out] */ );
    int (*db_free_table_continue)(DbTable* db); /* [in out] */  
    
    void (*db_print)(int to, 
		     void* to_arg, 
		     int show, 
		     DbTable* tb /* [in out] */ );

    void (*db_foreach_offheap)(DbTable* db,  /* [in out] */ 
			       void (*func)(ErlOffHeap *, void *),
			       void *arg);
    void (*db_check_table)(DbTable* tb);

    /* Lookup a dbterm for updating. Return false if not found. */
    int (*db_lookup_dbterm)(Process *, DbTable *, Eterm key, Eterm obj,
                            DbUpdateHandle* handle);

    /* Must be called for each db_lookup_dbterm that returned true, even if
    ** dbterm was not updated. If the handle was of a new object and cret is
    ** not DB_ERROR_NONE, the object is removed from the table. */
    void (*db_finalize_dbterm)(int cret, DbUpdateHandle* handle);

} DbTableMethod;

typedef struct db_fixation {
    Eterm pid;
    Uint counter;
    struct db_fixation *next;
} DbFixation;

/*
 * This structure contains data for all different types of database
 * tables. Note that these fields must match the same fields
 * in the table-type specific structures.
 * The reason it is placed here and not in db.h is that some table 
 * operations may be the same on different types of tables.
 */

typedef struct db_table_common {
    erts_refc_t ref;          /* fixation counter */
#ifdef ERTS_SMP
    erts_smp_rwmtx_t rwlock;  /* rw lock on table */
    erts_smp_mtx_t fixlock;   /* Protects fixations,megasec,sec,microsec */
    int is_thread_safe;       /* No fine locking inside table needed */
    Uint32 type;              /* table type, *read only* after creation */
#endif
    Eterm owner;              /* Pid of the creator */
    Eterm heir;               /* Pid of the heir */
    UWord heir_data;          /* To send in ETS-TRANSFER (is_immed or (DbTerm*) */
    Uint64 heir_started_interval;  /* To further identify the heir */
    Eterm the_name;           /* an atom */
    Eterm id;                 /* atom | integer */
    DbTableMethod* meth;      /* table methods */
    erts_smp_atomic_t nitems; /* Total number of items in table */
    erts_smp_atomic_t memory_size;/* Total memory size. NOTE: in bytes! */
    Uint megasec,sec,microsec; /* Last fixation time */
    DbFixation* fixations;    /* List of processes who have done safe_fixtable,
                                 "local" fixations not included. */ 
    /* All 32-bit fields */
    Uint32 status;            /* bit masks defined  below */
    int slot;                 /* slot index in meta_main_tab */
    int keypos;               /* defaults to 1 */
    int compress;
} DbTableCommon;

/* These are status bit patterns */
#define DB_NORMAL        (1 << 0)
#define DB_PRIVATE       (1 << 1)
#define DB_PROTECTED     (1 << 2)
#define DB_PUBLIC        (1 << 3)
#define DB_BAG           (1 << 4)
#define DB_SET           (1 << 5)
/*#define DB_LHASH         (1 << 6)*/
#define DB_FINE_LOCKED   (1 << 7)  /* fine grained locking enabled */
#define DB_DUPLICATE_BAG (1 << 8)
#define DB_ORDERED_SET   (1 << 9)
#define DB_DELETE        (1 << 10) /* table is being deleted */
#define DB_FREQ_READ     (1 << 11)

#define ERTS_ETS_TABLE_TYPES (DB_BAG|DB_SET|DB_DUPLICATE_BAG|DB_ORDERED_SET|DB_FINE_LOCKED|DB_FREQ_READ)

#define IS_HASH_TABLE(Status) (!!((Status) & \
				  (DB_BAG | DB_SET | DB_DUPLICATE_BAG)))
#define IS_TREE_TABLE(Status) (!!((Status) & \
				  DB_ORDERED_SET))
#define NFIXED(T) (erts_refc_read(&(T)->common.ref,0))
#define IS_FIXED(T) (NFIXED(T) != 0) 

/*
 * tplp is an untagged pointer to a tuple we know is large enough
 * and dth is a pointer to a DbTableHash.
 */
#define GETKEY(dth, tplp)   (*((tplp) + ((DbTableCommon*)(dth))->keypos))


ERTS_GLB_INLINE Eterm db_copy_key(Process* p, DbTable* tb, DbTerm* obj);
Eterm db_copy_from_comp(DbTableCommon* tb, DbTerm* bp, Eterm** hpp,
			ErlOffHeap* off_heap);
int db_eq_comp(DbTableCommon* tb, Eterm a, DbTerm* b);
DbTerm* db_alloc_tmp_uncompressed(DbTableCommon* tb, DbTerm* org);

ERTS_GLB_INLINE Eterm db_copy_object_from_ets(DbTableCommon* tb, DbTerm* bp,
					      Eterm** hpp, ErlOffHeap* off_heap);
ERTS_GLB_INLINE int db_eq(DbTableCommon* tb, Eterm a, DbTerm* b);
Wterm db_do_read_element(DbUpdateHandle* handle, Sint position);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Eterm db_copy_key(Process* p, DbTable* tb, DbTerm* obj)
{
    Eterm key = GETKEY(tb, obj->tpl);
    if IS_CONST(key) return key;
    else {
	Uint size = size_object(key);
	Eterm* hp = HAlloc(p, size);
	Eterm res = copy_struct(key, size, &hp, &MSO(p));
	ASSERT(EQ(res,key));
	return res;
    }
}

ERTS_GLB_INLINE Eterm db_copy_object_from_ets(DbTableCommon* tb, DbTerm* bp,
					      Eterm** hpp, ErlOffHeap* off_heap)
{
    if (tb->compress) {
	return db_copy_from_comp(tb, bp, hpp, off_heap);
    }
    else {
	return copy_shallow(bp->tpl, bp->size, hpp, off_heap);
    }
}

ERTS_GLB_INLINE int db_eq(DbTableCommon* tb, Eterm a, DbTerm* b)
{
    if (!tb->compress) {
	return EQ(a, make_tuple(b->tpl));
    }
    else {
	return db_eq_comp(tb, a, b);
    }
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */


#define DB_READ  (DB_PROTECTED|DB_PUBLIC)
#define DB_WRITE DB_PUBLIC
#define DB_INFO  (DB_PROTECTED|DB_PUBLIC|DB_PRIVATE)

#define ONLY_WRITER(P,T) (((T)->common.status & (DB_PRIVATE|DB_PROTECTED)) \
			  && (T)->common.owner == (P)->common.id)

#define ONLY_READER(P,T) (((T)->common.status & DB_PRIVATE) && \
(T)->common.owner == (P)->common.id)

/* Function prototypes */
BIF_RETTYPE db_get_trace_control_word(Process* p);
BIF_RETTYPE db_set_trace_control_word(Process* p, Eterm tcw);
BIF_RETTYPE db_get_trace_control_word_0(BIF_ALIST_0);
BIF_RETTYPE db_set_trace_control_word_1(BIF_ALIST_1);

void db_initialize_util(void);
Eterm db_getkey(int keypos, Eterm obj);
void db_cleanup_offheap_comp(DbTerm* p);
void db_free_term(DbTable *tb, void* basep, Uint offset);
void* db_store_term(DbTableCommon *tb, DbTerm* old, Uint offset, Eterm obj);
void* db_store_term_comp(DbTableCommon *tb, DbTerm* old, Uint offset, Eterm obj);
Eterm db_copy_element_from_ets(DbTableCommon* tb, Process* p, DbTerm* obj,
			       Uint pos, Eterm** hpp, Uint extra);
int db_has_map(Eterm obj);
int db_has_variable(Eterm obj);
int db_is_variable(Eterm obj);
void db_do_update_element(DbUpdateHandle* handle,
			  Sint position,
			  Eterm newval);
void db_finalize_resize(DbUpdateHandle* handle, Uint offset);
Eterm db_add_counter(Eterm** hpp, Wterm counter, Eterm incr);
Eterm db_match_set_lint(Process *p, Eterm matchexpr, Uint flags);
Binary *db_match_set_compile(Process *p, Eterm matchexpr, 
			     Uint flags);
void erts_db_match_prog_destructor(Binary *);

typedef struct match_prog {
    ErlHeapFragment *term_save; /* Only if needed, a list of message 
				    buffers for off heap copies 
				    (i.e. binaries)*/
    int single_variable;     /* ets:match needs to know this. */
    int num_bindings;        /* Size of heap */
    /* The following two are only filled in when match specs 
       are used for tracing */
    struct erl_heap_fragment *saved_program_buf;
    Eterm saved_program;
    Uint heap_size;          /* size of: heap + eheap + stack */
    Uint stack_offset;
#ifdef DMC_DEBUG
    UWord* prog_end;		/* End of program */
#endif
    UWord text[1];		/* Beginning of program */
} MatchProg;

/*
 * The heap-eheap-stack block of a MatchProg is nowadays allocated
 * when the match program is run.
 * - heap: variable bindings
 * - eheap: erlang heap storage
 * - eheap: a "large enough" stack
 */

#define DMC_ERR_STR_LEN 100

typedef enum { dmcWarning, dmcError} DMCErrorSeverity;

typedef struct dmc_error {
    char error_string[DMC_ERR_STR_LEN + 1]; /* printf format string
					       with %d for the variable
					       number (if applicable) */
    int variable;                           /* -1 if no variable is referenced
					       in error string */
    struct dmc_error *next;
    DMCErrorSeverity severity;              /* Error or warning */
} DMCError;

typedef struct dmc_err_info {
    unsigned int *var_trans; /* Translations of variable names, 
				initiated to NULL
				and free'd with sys_free if != NULL 
				after compilation */
    int num_trans;
    int error_added;         /* indicates if the error list contains
				any fatal errors (dmcError severity) */
    DMCError *first;         /* List of errors */
} DMCErrInfo;

/*
** Compilation flags
**
** The dialect is in the 3 least significant bits and are to be interspaced by
** by at least 2 (decimal), thats why ((Uint) 2) isn't used. This is to be 
** able to add DBIF_GUARD or DBIF BODY to it to use in the match_spec bif
** table. The rest of the word is used like ordinary flags, one bit for each 
** flag. Note that DCOMP_TABLE and DCOMP_TRACE are mutually exclusive.
*/
#define DCOMP_TABLE ((Uint) 1) /* Ets and dets. The body returns a value, 
		       * and the parameter to the execution is a tuple. */
#define DCOMP_TRACE ((Uint) 4) /* Trace. More functions are allowed, and the 
		       * parameter to the execution will be an array. */
#define DCOMP_DIALECT_MASK ((Uint) 0x7) /* To mask out the bits marking 
					   dialect */
#define DCOMP_FAKE_DESTRUCTIVE ((Uint) 8) /* When this is active, no setting of
					     trace control words or seq_trace tokens will be done. */


Binary *db_match_compile(Eterm *matchexpr, Eterm *guards,
			 Eterm *body, int num_matches, 
			 Uint flags, 
			 DMCErrInfo *err_info);
/* Returns newly allocated MatchProg binary with refc == 0*/

Eterm db_match_dbterm(DbTableCommon* tb, Process* c_p, Binary* bprog,
		      int all, DbTerm* obj, Eterm** hpp, Uint extra);

Eterm db_prog_match(Process *p, Binary *prog, Eterm term,
		    Eterm *termp, int arity,
		    enum erts_pam_run_flags in_flags,
		    Uint32 *return_flags /* Zeroed on enter */);

/* returns DB_ERROR_NONE if matches, 1 if not matches and some db error on 
   error. */
DMCErrInfo *db_new_dmc_err_info(void);
/* Returns allocated error info, where errors are collected for lint. */
Eterm db_format_dmc_err_info(Process *p, DMCErrInfo *ei);
/* Formats an error info structure into a list of tuples. */
void db_free_dmc_err_info(DMCErrInfo *ei);
/* Completely free's an error info structure, including all recorded 
   errors */
Eterm db_make_mp_binary(Process *p, Binary *mp, Eterm **hpp);
/* Convert a match program to a erlang "magic" binary to be returned to userspace,
   increments the reference counter. */
int erts_db_is_compiled_ms(Eterm term);

/*
** Convenience when compiling into Binary structures
*/
#define IsMatchProgBinary(BP) \
  (((BP)->flags & BIN_FLAG_MAGIC) \
   && ERTS_MAGIC_BIN_DESTRUCTOR((BP)) == erts_db_match_prog_destructor)

#define Binary2MatchProg(BP) \
  (ASSERT(IsMatchProgBinary((BP))), \
   ((MatchProg *) ERTS_MAGIC_BIN_DATA((BP))))
/*
** Debugging 
*/
#ifdef HARDDEBUG
void db_check_tables(void); /* in db.c */
#define CHECK_TABLES() db_check_tables()
#else 
#define CHECK_TABLES()
#endif

#endif /* _DB_UTIL_H */
