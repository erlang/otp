/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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

#ifndef __ERL_NMGC_H__
#define __ERL_NMGC_H__

#ifdef INCREMENTAL
#include <stddef.h>      /* offsetof() */
#include "erl_process.h"

#define INC_FULLPAGE (INC_PAGESIZE + offsetof(INC_Page,start) / sizeof(void*))

#define BOXED_NEED(PTR,HDR)                                             \
  (((HDR) & _HEADER_SUBTAG_MASK) == SUB_BINARY_SUBTAG ?                 \
    header_arity(HDR) + 2 :                                             \
   ((HDR) & _HEADER_SUBTAG_MASK) == FUN_SUBTAG ?                        \
    header_arity(HDR) + ((ErlFunThing*)(PTR))->num_free + 2 :           \
   header_arity(HDR) + 1)


#define INC_DECREASE_WORK(n) inc_words_to_go -= (n);

#define INC_COPY_CONS(FROM,TO,PTR)					\
do {									\
    TO[0] = FROM[0];							\
    TO[1] = FROM[1];							\
    INC_MARK_FORWARD(FROM,TO);						\
    *(PTR) = make_list(TO);						\
    INC_DECREASE_WORK(2);						\
    (TO) += 2;								\
} while(0)

#define INC_COPY_BOXED(FROM,TO,PTR)					\
do {									\
    Sint nelts;								\
    Eterm hdr = *(FROM);						\
									\
    ASSERT(is_header(hdr));						\
    INC_MARK_FORWARD(FROM,TO);						\
    *(PTR) = make_boxed(TO);						\
    *(TO)++ = *(FROM)++;       						\
    nelts = header_arity(hdr);						\
    switch ((hdr) & _HEADER_SUBTAG_MASK) {				\
    case SUB_BINARY_SUBTAG: nelts++; break;				\
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(FROM-1))->num_free+1; break;\
    }									\
    INC_DECREASE_WORK(nelts + 1);					\
    while (nelts--)							\
	*(TO)++ = *(FROM)++;						\
} while(0)


/* Things copied to the old generation are not marked in the blackmap. 
 * This is ok since the page they are copied to (aging) is not part of
 * the sweep.
 */
#define COPYMARK_CONS(FROM,TO,PTR,LIMIT)                                \
do {							                \
    if (ptr_within(FROM,inc_fromspc,inc_fromend)) {                     \
        if (INC_IS_FORWARDED(FROM)) {                                   \
            *PTR = make_list(INC_FORWARD_VALUE(FROM));                  \
        } else if (TO + 2 <= LIMIT) {                                   \
            INC_STORE(gray,TO,2);                                       \
            INC_COPY_CONS(FROM,TO,PTR);                                 \
        } else {                                                        \
            Eterm *hp = erts_inc_alloc(2);                              \
            INC_STORE(gray,hp,2);                                       \
            INC_COPY_CONS(FROM,hp,PTR);                                 \
        }                                                               \
    } else if (ptr_within(FROM,global_old_heap,global_old_hend) &&     	\
               (blackmap[FROM - global_old_heap] == 0)) {               \
        blackmap[FROM - global_old_heap] = 2;                           \
        INC_DECREASE_WORK(2);                                           \
        INC_STORE(gray,FROM,2);                                         \
    }                                                                   \
} while(0)

#define COPYMARK_BOXED(FROM,TO,PTR,LIMIT)                               \
do {							                \
    if (ptr_within(FROM,inc_fromspc,inc_fromend)) {                     \
        int size = BOXED_NEED(FROM,*FROM);                              \
        if (INC_IS_FORWARDED(FROM)) {                                   \
            *PTR = make_boxed(INC_FORWARD_VALUE(FROM));                 \
        } else if (TO + size <= LIMIT) {                                \
            INC_STORE(gray,TO,size);                                    \
            INC_COPY_BOXED(FROM,TO,PTR);                                \
        } else {                                                        \
            Eterm *hp = erts_inc_alloc(size);                           \
            INC_STORE(gray,hp,size);                                    \
            INC_COPY_BOXED(FROM,hp,PTR);                                \
        }                                                               \
    } else if (ptr_within(FROM,global_old_heap,global_old_hend) &&     	\
               (blackmap[FROM - global_old_heap] == 0)) {               \
        int size = BOXED_NEED(FROM,*FROM);                              \
        if (size > 254) {                                               \
            blackmap[FROM - global_old_heap] = 255;                     \
            *(int*)((long)(&blackmap[FROM -                             \
                                     global_old_heap] + 4) & ~3) = size; \
        } else                                                          \
            blackmap[FROM - global_old_heap] = size;                    \
        INC_DECREASE_WORK(size);                                        \
        INC_STORE(gray,FROM,size);                                      \
    }                                                                   \
} while(0)

#define INC_MARK_FORWARD(ptr,dst) fwdptrs[(ptr) - inc_fromspc] = (dst);
#define INC_IS_FORWARDED(ptr) (fwdptrs[(ptr) - inc_fromspc] != 0)
#define INC_FORWARD_VALUE(ptr) fwdptrs[(ptr) - inc_fromspc]

/* Note for BM_TIMER: Active timer should always be 'system' when IncAlloc
 * is called!
 */
#define IncAlloc(p, sz, objv, nobj)                                     \
    (ASSERT_EXPR((sz) >= 0),                                            \
     (((inc_alloc_limit - global_htop) <= (sz)) ?                       \
      erts_incremental_gc((p),(sz),(objv),(nobj)) : 0),                 \
     ASSERT_EXPR(global_hend - global_htop > (sz)),                     \
     global_htop += (sz), global_htop - (sz))


/************************************************************************
 * INC_STORAGE, a dynamic circular storage for objects (INC_Object).    *
 * Use INC_STORE to add objects to the storage. The storage can then    *
 * be used either as a queue, using INC_STORAGE_GET to retreive         *
 * values, or as a stack, using INC_STORAGE_POP. It is OK to mix calls  *
 * to GET and POP if that is desired.                                   *
 * An iterator can be declared to traverse the storage without removing *
 * any elements, and INC_STORAGE_STEP will then return each element in  *
 * turn, oldest first.                                                  *
 ***********************************************************************/

/* Declare a new storage; must be in the beginning of a block. Give
 * the storage a name that is used in all later calls to the storage. 
 * If this is an external declaration of the storage, pass the keyword
 * external as the first argument, otherwise leave it empty.
 */
#define INC_STORAGE_DECLARATION(ext,name)                               \
    ext INC_Storage *name##head;                                        \
    ext INC_Storage *name##tail;                                        \
    ext INC_Object *name##free;                                         \
    ext INC_Object *name##last_free;                                    \
    ext int name##size;


/* Initialize the storage. Note that memory allocation is involved -
 * don't forget to erase the storage when you are done.
 */
#define INC_STORAGE_INIT(name) do {                                     \
    name##head = (INC_Storage*)erts_alloc(ERTS_ALC_T_OBJECT_STACK,      \
                                          sizeof(INC_Storage));         \
    name##head->next = name##head;                                      \
    name##head->prev = name##head;                                      \
    name##tail = name##head;                                            \
    name##free = name##head->data;                                      \
    name##last_free = name##free + INC_STORAGE_SIZE - 1;                \
    name##size = 0;                                                     \
} while(0)


/*
#define INC_STORAGE_SWAP(s1,s2) do {                                    \
    INC_Storage *tmphead = s1##head;                                    \
    INC_Storage *tmptail = s1##tail;                                    \
    INC_Object *tmpfree = s1##free;                                     \
    INC_Object *tmplast = s1##last_free;                                \
    int tmpsize = s1##size;                                             \
    s1##head = s2##head;                                                \
    s1##tail = s2##tail;                                                \
    s1##free = s2##free;                                                \
    s1##last_free = s2##last_free;                                      \
    s1##size = s2##size;                                                \
    s2##head = tmphead;                                                 \
    s2##tail = tmptail;                                                 \
    s2##free = tmpfree;                                                 \
    s2##last_free = tmplast;                                            \
    s2##size = tmpsize;                                                 \
} while(0)
*/


/* Return and remove the youngest element - treat the storage as a
 * stack. Always check that there are elements in the queue before
 * using INC_STORAGE_POP!
 */
#define INC_STORAGE_POP(name) (ASSERT_EXPR(name##size != 0),            \
    name##size--,                                                       \
    (--name##free != name##head->data - 1) ?                            \
    name##free : (name##head = name##head->prev,                        \
                  name##free = name##head->data + INC_STORAGE_SIZE - 1))


/* Return and remove the oldest element - treat the storage as a
 * queue. Always check that there are elements in the queue before
 * using INC_STORAGE_GET!
 */
#define INC_STORAGE_GET(name) (ASSERT_EXPR(name##size != 0),            \
    name##size--,                                                       \
    (++name##last_free != name##tail->data + INC_STORAGE_SIZE) ?        \
     name##last_free : (name##tail = name##tail->next,                  \
                        name##last_free = name##tail->data))


/* Advance the head to the next free location. If the storage is full,
 * a new storage is allocated and linked into the list.
 */
#define INC_STORAGE_NEXT(name) do {                                     \
    if (name##free == name##last_free) {                                \
        name##tail = (INC_Storage*)erts_alloc(ERTS_ALC_T_OBJECT_STACK,  \
                                              sizeof(INC_Storage));     \
        memcpy(name##tail->data,name##head->data,                       \
               INC_STORAGE_SIZE * sizeof(INC_Object));                  \
        name##tail->next = name##head->next;                            \
        name##head->next = name##tail;                                  \
        name##tail->prev = name##tail->next->prev;                      \
        name##tail->next->prev = name##tail;                            \
        name##last_free = ((void*)name##tail +                          \
                         ((void*)name##last_free - (void*)name##head)); \
    }                                                                   \
    name##free++;                                                       \
    name##size++;                                                       \
    if (name##free == name##head->data + INC_STORAGE_SIZE) {            \
        name##head = name##head->next;                                  \
        name##free = name##head->data;                                  \
    }                                                                   \
} while(0)


/* The head of this storage is the next free location. This is where
 * the next element will be stored.
 */
#define INC_STORAGE_HEAD(name) (name##free)


/* Return the top - the youngest element in the storage. */
/* #define INC_STORAGE_TOP(name) (name##free - 1 with some magic..) */


/* True if the storage is empty, false otherwise */
#define INC_STORAGE_EMPTY(name) (name##size == 0)


/* Store a new element in the head of the storage and advance the head
 * to the next free location.
 */
#define INC_STORE(name,ptr,sz) do {                                      \
    INC_STORAGE_HEAD(name)->this = ptr;                                  \
    INC_STORAGE_HEAD(name)->size = sz;                                   \
    INC_STORAGE_NEXT(name);                                              \
} while(0)


/* An iterator. Use it together with INC_STORAGE_STEP to browse throuh
 * the storage. Please note that it is not possible to remove an entry
 * in the middle of the storage, use GET or POP to remove enties.
 */
#define INC_STORAGE_ITERATOR(name)                                      \
    INC_Storage *name##iterator_head = name##tail;                      \
    INC_Object *name##iterator_current = name##last_free;               \
    int name##iterator_left = name##size;


/* Return the next element in the storage (sorted by age, oldest
 * first) or NULL if the storage is empty or the last element has been
 * returned already.
 */
#define INC_STORAGE_STEP(name) (name##iterator_left == 0 ? NULL :       \
    (name##iterator_left--,                                             \
     (++name##iterator_current != name##iterator_head->data +           \
      INC_STORAGE_SIZE) ? name##iterator_current :                      \
     (name##iterator_head = name##iterator_head->next,                  \
      name##iterator_current = name##iterator_head->data)))


/* Erase the storage. */
#define INC_STORAGE_ERASE(name)do {                             \
    name##head->prev->next = NULL;                              \
    while (name##head != NULL) {                                \
        name##tail = name##head;                                \
        name##head = name##head->next;                          \
        erts_free(ERTS_ALC_T_OBJECT_STACK,(void*)name##tail);   \
    }                                                           \
    name##tail = NULL;                                          \
    name##free = NULL;                                          \
    name##last_free = NULL;                                     \
    name##size = 0;                                             \
} while(0)

/*
 * Structures used by the non-moving memory manager
 */

typedef struct
{
  Eterm *this;
  unsigned long size;
} INC_Object;

typedef struct inc_storage {
  struct inc_storage *next;
  struct inc_storage *prev;
  INC_Object data[INC_STORAGE_SIZE];
} INC_Storage;

typedef struct inc_mem_block
{
  unsigned long size;
  struct inc_mem_block *prev;
  struct inc_mem_block *next;
} INC_MemBlock;

typedef struct inc_page
{
  struct inc_page *next;
  Eterm start[1]; /* Has to be last in struct, this is where the data start */
} INC_Page;


/*
 * Heap pointers for the non-moving memory area.
 */
extern INC_Page *inc_used_mem;
extern INC_MemBlock *inc_free_list;
extern unsigned char *blackmap;

extern Eterm **fwdptrs;
extern Eterm *inc_fromspc;
extern Eterm *inc_fromend;
extern Process *inc_active_proc;
extern Process *inc_active_last;
extern Eterm *inc_alloc_limit;
extern int   inc_words_to_go;

INC_STORAGE_DECLARATION(extern,gray);
INC_STORAGE_DECLARATION(extern,root);

void erts_init_incgc(void);
void erts_cleanup_incgc(void);
void erts_incremental_gc(Process *p, int sz, Eterm* objv, int nobj);
Eterm *erts_inc_alloc(int need);

#else
#  define INC_STORE(lst,ptr,sz)
#  define INC_MARK_FORWARD(ptr)
#  define INC_IS_FORWARDED(ptr)
#  define INC_FORWARD_VALUE(ptr)
#endif /* INCREMENTAL */

#endif /* _ERL_NMGC_H_ */
