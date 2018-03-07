/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

#ifndef __ERL_MESSAGE_H__
#define __ERL_MESSAGE_H__

#include "sys.h"
#define ERTS_PROC_SIG_QUEUE_TYPE_ONLY
#include "erl_proc_sig_queue.h"
#undef ERTS_PROC_SIG_QUEUE_TYPE_ONLY

struct proc_bin;
struct external_thing_;

typedef struct erl_mesg ErtsMessage;

/*
 * This struct represents data that must be updated by structure copy,
 * but is stored outside of any heap.
 */

struct erl_off_heap_header {
    Eterm thing_word;
    Uint size;
    struct erl_off_heap_header* next;
};

#define OH_OVERHEAD(oh, size) do { \
    (oh)->overhead += size;        \
} while(0)

typedef struct erl_off_heap {
    struct erl_off_heap_header* first;
    Uint64 overhead;     /* Administrative overhead (used to force GC). */
} ErlOffHeap;

#define ERTS_INIT_OFF_HEAP(OHP)			\
    do {					\
	(OHP)->first = NULL;			\
	(OHP)->overhead = 0;			\
    } while (0)

typedef struct {
    enum {
        FACTORY_CLOSED = 0,
        FACTORY_HALLOC,
        FACTORY_MESSAGE,
        FACTORY_HEAP_FRAGS,
        FACTORY_STATIC,
        FACTORY_TMP
    } mode;
    Process* p;
    Eterm* hp_start;
    Eterm* hp;
    Eterm* hp_end;
    ErtsMessage *message;
    struct erl_heap_fragment* heap_frags;
    struct erl_heap_fragment* heap_frags_saved;
    Uint heap_frags_saved_used;
    ErlOffHeap* off_heap;
    ErlOffHeap off_heap_saved;
    Uint32 alloc_type;
} ErtsHeapFactory;

void erts_factory_proc_init(ErtsHeapFactory*, Process*);
void erts_factory_proc_prealloc_init(ErtsHeapFactory*, Process*, Sint size);
void erts_factory_heap_frag_init(ErtsHeapFactory*, struct erl_heap_fragment*);
ErtsMessage *erts_factory_message_create(ErtsHeapFactory *, Process *,
					  ErtsProcLocks *, Uint sz);
void erts_factory_selfcontained_message_init(ErtsHeapFactory*, ErtsMessage *, Eterm *);
void erts_factory_static_init(ErtsHeapFactory*, Eterm* hp, Uint size, ErlOffHeap*);
void erts_factory_tmp_init(ErtsHeapFactory*, Eterm* hp, Uint size, Uint32 atype);
void erts_factory_dummy_init(ErtsHeapFactory*);

Eterm* erts_produce_heap(ErtsHeapFactory*, Uint need, Uint xtra);
Eterm* erts_reserve_heap(ErtsHeapFactory*, Uint need);
void erts_factory_close(ErtsHeapFactory*);
void erts_factory_trim_and_close(ErtsHeapFactory*,Eterm *brefs, Uint brefs_size);
void erts_factory_undo(ErtsHeapFactory*);

#ifdef CHECK_FOR_HOLES
# define ERTS_FACTORY_HOLE_CHECK(f) do {    \
        /*if ((f)->p) erts_check_for_holes((f)->p);*/ \
    } while (0)
#else
# define ERTS_FACTORY_HOLE_CHECK(p)
#endif

#include "external.h"
#include "erl_process.h"

#define ERTS_INVALID_HFRAG_PTR ((ErlHeapFragment *) ~((UWord) 7))

/*
 * This struct represents a heap fragment, which is used when there
 * isn't sufficient room in the process heap and we can't do a GC.
 */

typedef struct erl_heap_fragment ErlHeapFragment;
struct erl_heap_fragment {
    ErlHeapFragment* next;	/* Next heap fragment */
    ErlOffHeap off_heap;	/* Offset heap data. */
    Uint alloc_size;		/* Size in (half)words of mem */
    Uint used_size;		/* With terms to be moved to heap by GC */
    Eterm mem[1];		/* Data */
};

/* m[0] = message, m[1] = seq trace token */
#define ERL_MESSAGE_REF_ARRAY_SZ 3
#define ERL_MESSAGE_TERM(mp) ((mp)->m[0])
#define ERL_MESSAGE_TOKEN(mp) ((mp)->m[1])
#define ERL_MESSAGE_FROM(mp) ((mp)->m[2])

#ifdef USE_VM_PROBES
/* m[2] = dynamic trace user tag */
#undef ERL_MESSAGE_REF_ARRAY_SZ
#define ERL_MESSAGE_REF_ARRAY_SZ 4
#define ERL_MESSAGE_DT_UTAG(mp) ((mp)->m[3])
#else
#endif

#ifdef USE_VM_PROBES
#define have_no_seqtrace(T) ((T) == NIL || (T) == am_have_dt_utag)
#else
#define have_no_seqtrace(T) ((T) == NIL)
#endif
#define have_seqtrace(T)    (!have_no_seqtrace(T))

#define ERL_MESSAGE_REF_FIELDS__			\
    ErtsMessage *next;	/* Next message */		\
    union {						\
	ErtsDistExternal *dist_ext;			\
	ErlHeapFragment *heap_frag;			\
	void *attached;					\
    } data;						\
    Eterm m[ERL_MESSAGE_REF_ARRAY_SZ]


typedef struct erl_msg_ref__ {
    ERL_MESSAGE_REF_FIELDS__;
} ErtsMessageRef;

struct erl_mesg {
    ERL_MESSAGE_REF_FIELDS__;

    ErlHeapFragment hfrag;
};

/*
 * The ErtsMessage struct is only one special type
 * of signal. All signal structs have a common
 * begining and can be differentiated by looking
 * at the ErtsSignal 'common.tag' field.
 *
 * - An ordinary message will have a value
 * - A distribution message that has not been
 *   decoded yet will have the non-value.
 * - Other signals will have an external pid
 *   header tag. In order to differentiate
 *   between those signals one needs to look
 *   at the arity part of the header (see
 *   erts_proc_sig_queue.h).
 */

#define ERTS_SIG_IS_NON_MSG_TAG(Tag) \
    (is_external_pid_header((Tag)))

#define ERTS_SIG_IS_NON_MSG(Sig) \
    ERTS_SIG_IS_NON_MSG_TAG(((ErtsSignal *) (Sig))->common.tag)

#define ERTS_SIG_IS_INTERNAL_MSG_TAG(Tag) \
    (!is_header((Tag)))
#define ERTS_SIG_IS_INTERNAL_MSG(Sig) \
    ERTS_SIG_IS_INTERNAL_MSG_TAG(((ErtsSignal *) (Sig))->common.tag)

#define ERTS_SIG_IS_EXTERNAL_MSG_TAG(Tag) \
    ((Tag) == THE_NON_VALUE)
#define ERTS_SIG_IS_EXTERNAL_MSG(Sig) \
    ERTS_SIG_IS_EXTERNAL_MSG_TAG(((ErtsSignal *) (Sig))->common.tag)

#define ERTS_SIG_IS_MSG_TAG(Tag) \
    (!ERTS_SIG_IS_NON_MSG_TAG(Tag))
#define ERTS_SIG_IS_MSG(Sig) \
    ERTS_SIG_IS_MSG_TAG(((ErtsSignal *) (Sig))->common.tag)

typedef union {
    ErtsSignalCommon common;
    ErtsMessageRef msg;
} ErtsSignal;

typedef struct {
    /* pointers to next pointers pointing to... */
    ErtsMessage **next; /* ... next (non-message) signal */
    ErtsMessage **last; /* ... next (non-message) signal */
} ErtsMsgQNMSigs;

/* Size of default message buffer (erl_message.c) */
#define ERL_MESSAGE_BUF_SZ 500

typedef struct {
    /* inner queue */
    ErtsMessage *first;
    ErtsMessage **last;  /* point to the last next pointer */
    ErtsMessage **save;

    /* middle queue */
    ErtsMessage *cont;
    ErtsMessage **cont_last;
    ErtsMsgQNMSigs nmsigs;

    /* Common for inner and middle queue */
    ErtsMessage **saved_last;	/* saved last pointer */
    Sint len; /* message queue length (inner+middle) */
} ErtsSignalPrivQueues;

typedef struct {
    ErtsMessage* first;
    ErtsMessage** last;  /* point to the last next pointer */
    Sint len;            /* queue length */
    ErtsMsgQNMSigs nmsigs;
} ErtsSignalInQueue;

typedef struct erl_trace_message_queue__ {
    struct erl_trace_message_queue__ *next; /* point to the next receiver */
    Eterm receiver;
    ErtsMessage* first;
    ErtsMessage** last;  /* point to the last next pointer */
    Sint len;            /* queue length */
} ErlTraceMessageQueue;

#define ERTS_RECV_MARK_SAVE(P)                                          \
    do {                                                                \
        erts_proc_lock((P), ERTS_PROC_LOCK_MSGQ);                       \
        if ((P)->sig_inq.first)                                         \
            erts_proc_sig_fetch((P));                                   \
        erts_proc_unlock((P), ERTS_PROC_LOCK_MSGQ);                     \
        if ((P)->sig_qs.cont) {                                         \
            (P)->sig_qs.saved_last = (P)->sig_qs.cont_last;             \
            (P)->flags |= F_DEFERRED_SAVED_LAST;                        \
        }                                                               \
        else {                                                          \
            (P)->sig_qs.saved_last = (P)->sig_qs.last;                  \
            (P)->flags &= ~F_DEFERRED_SAVED_LAST;                       \
        }                                                               \
    } while (0)

#define ERTS_RECV_MARK_SET(P)                                           \
    do {                                                                \
        if ((P)->sig_qs.saved_last) {                                   \
            if ((P)->flags & F_DEFERRED_SAVED_LAST) {                   \
                /* Points to middle queue; use end of inner */          \
                (P)->sig_qs.save = (P)->sig_qs.last;                    \
                ASSERT(!PEEK_MESSAGE((P)));                             \
            }                                                           \
            else {                                                      \
                /* Points to inner queue; safe to use */                \
                (P)->sig_qs.save = (P)->sig_qs.saved_last;              \
            }                                                           \
        }                                                               \
    } while (0)

#define ERTS_RECV_MARK_CLEAR(P)                                         \
    do {                                                                \
        (P)->sig_qs.saved_last = NULL;                                  \
        (P)->flags &= ~F_DEFERRED_SAVED_LAST;                           \
    } while (0)


/* Get "current" message */
#define PEEK_MESSAGE(p)  (*(p)->sig_qs.save)

#ifdef USE_VM_PROBES
#define LINK_MESSAGE_DTAG(mp, dt) ERL_MESSAGE_DT_UTAG(mp) = dt
#else
#define LINK_MESSAGE_DTAG(mp, dt)
#endif

#ifdef USE_VM_PROBES
#  define ERTS_MSG_RECV_TRACED(P)                                       \
    ((ERTS_TRACE_FLAGS((P)) & F_TRACE_RECEIVE)                          \
     || DTRACE_ENABLED(message_queued))
#else
#  define ERTS_MSG_RECV_TRACED(P)                                       \
    (ERTS_TRACE_FLAGS((P)) & F_TRACE_RECEIVE)

#endif


/* Add message last in private message queue */
#define LINK_MESSAGE_PRIVQ(p, first_msg, last_msg, num_msgs)            \
    do {                                                                \
        ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((p), "before");             \
        if ((p)->sig_qs.cont || ERTS_MSG_RECV_TRACED((p))) {            \
            *(p)->sig_qs.cont_last = (first_msg);                       \
            (p)->sig_qs.cont_last = (last_msg);                         \
        }                                                               \
        else {                                                          \
            *(p)->sig_qs.last = (first_msg);                            \
            (p)->sig_qs.last = (last_msg);                              \
        }                                                               \
        (p)->sig_qs.len += (num_msgs);                                  \
        ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((p), "after");              \
    } while (0)

/* Add message last_msg in message queue */
#define LINK_MESSAGE(p, first_msg, last_msg, num_msgs) \
    do {                                                                \
        ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__((p), "before");               \
        *(p)->sig_inq.last = (first_msg);                               \
        (p)->sig_inq.last = (last_msg);                                 \
        (p)->sig_inq.len += (num_msgs);                                 \
        ERTS_HDBG_CHECK_SIGNAL_IN_QUEUE__((p), "before");               \
    } while(0)

/* Unlink current message */
#define UNLINK_MESSAGE(p,msgp)                                          \
    do {                                                                \
        ErtsMessage *mp__ = (msgp)->next;                               \
        ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((p), "before");             \
        *(p)->sig_qs.save = mp__;                                       \
        (p)->sig_qs.len--;                                              \
        if (mp__ == NULL)                                               \
            (p)->sig_qs.last = (p)->sig_qs.save;                        \
        ERTS_HDBG_CHECK_SIGNAL_PRIV_QUEUE__((p), "after");              \
    } while(0)

/*
 * Reset message save point (after receive match).
 * Also invalidate the saved position since it may no
 * longer be safe to use.
 */
#define JOIN_MESSAGE(p)                                                 \
   do {                                                                 \
       (p)->sig_qs.save = &(p)->sig_qs.first;                           \
       ERTS_RECV_MARK_CLEAR((p));                                       \
   } while(0)

/* Save current message */
#define SAVE_MESSAGE(p) \
     (p)->sig_qs.save = &(*(p)->sig_qs.save)->next

#define ERTS_SND_FLG_NO_SEQ_TRACE		(((unsigned) 1) << 0)

#define ERTS_HEAP_FRAG_SIZE(DATA_WORDS) \
   (sizeof(ErlHeapFragment) - sizeof(Eterm) + (DATA_WORDS)*sizeof(Eterm))

#define ERTS_INIT_HEAP_FRAG(HEAP_FRAG_P, USED_WORDS, DATA_WORDS)	\
    do {								\
	(HEAP_FRAG_P)->next = NULL;					\
	(HEAP_FRAG_P)->alloc_size = (DATA_WORDS);			\
	(HEAP_FRAG_P)->used_size = (USED_WORDS);			\
	(HEAP_FRAG_P)->off_heap.first = NULL;				\
	(HEAP_FRAG_P)->off_heap.overhead = 0;				\
    } while (0)

#ifdef USE_VM_PROBES
#define ERL_MESSAGE_DT_UTAG_INIT(MP) ERL_MESSAGE_DT_UTAG(MP) = NIL
#else
#define ERL_MESSAGE_DT_UTAG_INIT(MP) do{ } while (0)
#endif

#define ERTS_INIT_MESSAGE(MP)                           \
    do {                                                \
        (MP)->next = NULL;                              \
        ERL_MESSAGE_TERM(MP) = THE_NON_VALUE;           \
        ERL_MESSAGE_TOKEN(MP) = NIL;                    \
        ERL_MESSAGE_FROM(MP) = NIL;                     \
        ERL_MESSAGE_DT_UTAG_INIT(MP);                   \
        MP->data.attached = NULL;                       \
    } while (0)

void init_message(void);
ErlHeapFragment* new_message_buffer(Uint);
ErlHeapFragment* erts_resize_message_buffer(ErlHeapFragment *, Uint,
					    Eterm *, Uint);
void free_message_buffer(ErlHeapFragment *);
void erts_queue_dist_message(Process*, ErtsProcLocks, ErtsDistExternal *, Eterm, Eterm);
Sint erts_queue_message(Process*, ErtsProcLocks,ErtsMessage*, Eterm, Eterm);
Sint erts_queue_messages(Process*, ErtsProcLocks,
                         ErtsMessage*, ErtsMessage**, Uint);
void erts_deliver_exit_message(Eterm, Process*, ErtsProcLocks *, Eterm, Eterm);
Sint erts_send_message(Process*, Process*, ErtsProcLocks*, Eterm, unsigned);
void erts_link_mbuf_to_proc(Process *proc, ErlHeapFragment *bp);

Uint erts_msg_attached_data_size_aux(ErtsMessage *msg);

void erts_cleanup_offheap(ErlOffHeap *offheap);
void erts_save_message_in_proc(Process *p, ErtsMessage *msg);
Sint erts_move_messages_off_heap(Process *c_p);
Sint erts_complete_off_heap_message_queue_change(Process *c_p);
Eterm erts_change_message_queue_management(Process *c_p, Eterm new_state);

int erts_decode_dist_message(Process *, ErtsProcLocks, ErtsMessage *, int);

void erts_cleanup_messages(ErtsMessage *mp);

void *erts_alloc_message_ref(void);
void erts_free_message_ref(void *);

#define ERTS_SMALL_FIX_MSG_SZ 10
#define ERTS_MEDIUM_FIX_MSG_SZ 20
#define ERTS_LARGE_FIX_MSG_SZ 30

void *erts_alloc_small_message(void);
void erts_free_small_message(void *mp);

typedef struct {
    ErtsMessage m;
    Eterm data[ERTS_SMALL_FIX_MSG_SZ-1];
} ErtsSmallFixSzMessage;

typedef struct {
    ErtsMessage m;
    Eterm data[ERTS_MEDIUM_FIX_MSG_SZ-1];
} ErtsMediumFixSzMessage;

typedef struct {
    ErtsMessage m;
    Eterm data[ERTS_LARGE_FIX_MSG_SZ-1];
} ErtsLargeFixSzMessage;

ErtsMessage *erts_try_alloc_message_on_heap(Process *pp,
					    erts_aint32_t *psp,
					    ErtsProcLocks *plp,
					    Uint sz,
					    Eterm **hpp,
					    ErlOffHeap **ohpp,
					    int *on_heap_p);
ErtsMessage *erts_realloc_shrink_message(ErtsMessage *mp, Uint sz,
					 Eterm *brefs, Uint brefs_size);

ERTS_GLB_FORCE_INLINE ErtsMessage *erts_alloc_message(Uint sz, Eterm **hpp);
ERTS_GLB_FORCE_INLINE ErtsMessage *erts_shrink_message(ErtsMessage *mp, Uint sz,
						       Eterm *brefs, Uint brefs_size);
ERTS_GLB_FORCE_INLINE void erts_free_message(ErtsMessage *mp);
ERTS_GLB_INLINE Uint erts_used_frag_sz(const ErlHeapFragment*);
ERTS_GLB_INLINE Uint erts_msg_attached_data_size(ErtsMessage *msg);

#define ERTS_MSG_COMBINED_HFRAG ((void *) 0x1)

#define erts_message_to_heap_frag(MP)                   \
    (((MP)->data.attached == ERTS_MSG_COMBINED_HFRAG) ? \
        &(MP)->hfrag : (MP)->data.heap_frag)

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_FORCE_INLINE ErtsMessage *erts_alloc_message(Uint sz, Eterm **hpp)
{
    ErtsMessage *mp;

    if (sz == 0) {
	mp = erts_alloc_message_ref();
        ERTS_INIT_MESSAGE(mp);
	if (hpp)
	    *hpp = NULL;
	return mp;
    }

    mp = erts_alloc(ERTS_ALC_T_MSG,
		    sizeof(ErtsMessage) + (sz - 1)*sizeof(Eterm));

    ERTS_INIT_MESSAGE(mp);
    mp->data.attached = ERTS_MSG_COMBINED_HFRAG;
    ERTS_INIT_HEAP_FRAG(&mp->hfrag, sz, sz);

    if (hpp)
	*hpp = &mp->hfrag.mem[0];

    return mp;
}

ERTS_GLB_FORCE_INLINE ErtsMessage *
erts_shrink_message(ErtsMessage *mp, Uint sz, Eterm *brefs, Uint brefs_size)
{
    if (sz == 0) {
	ErtsMessage *nmp;
	if (!mp->data.attached)
	    return mp;
	ASSERT(mp->data.attached == ERTS_MSG_COMBINED_HFRAG);
	nmp = erts_alloc_message_ref();
#ifdef DEBUG
	if (brefs && brefs_size) {
	    int i;
	    for (i = 0; i < brefs_size; i++)
		ASSERT(is_non_value(brefs[i]) || is_immed(brefs[i]));
	}
#endif
	erts_free(ERTS_ALC_T_MSG, mp);
	return nmp;
    }

    ASSERT(mp->data.attached == ERTS_MSG_COMBINED_HFRAG);
    ASSERT(mp->hfrag.used_size >= sz);

    if (sz >= (mp->hfrag.alloc_size - mp->hfrag.alloc_size / 16)) {
	mp->hfrag.used_size = sz;
	return mp;
    }

    return erts_realloc_shrink_message(mp, sz, brefs, brefs_size);
}

ERTS_GLB_FORCE_INLINE void erts_free_message(ErtsMessage *mp)
{
    if (mp->data.attached != ERTS_MSG_COMBINED_HFRAG)
	erts_free_message_ref(mp);
    else
	erts_free(ERTS_ALC_T_MSG, mp);
}

ERTS_GLB_INLINE Uint erts_used_frag_sz(const ErlHeapFragment* bp)
{
    Uint sz = 0;
    for ( ; bp!=NULL; bp=bp->next) {
	sz += bp->used_size;
    }
    return sz;
}

ERTS_GLB_INLINE Uint erts_msg_attached_data_size(ErtsMessage *msg)
{
    ASSERT(msg->data.attached);
    if (is_value(ERL_MESSAGE_TERM(msg))) {
	ErlHeapFragment *bp;
        bp = erts_message_to_heap_frag(msg);
	return erts_used_frag_sz(bp);
    }
    else if (msg->data.dist_ext->heap_size < 0)
	return erts_msg_attached_data_size_aux(msg);
    else {
	Uint sz = msg->data.dist_ext->heap_size;
	if (is_not_nil(ERL_MESSAGE_TOKEN(msg))) {
	    ErlHeapFragment *heap_frag;
	    heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
	    sz += heap_frag->used_size;
	}
	return sz;
    }
}

#endif

Uint erts_mbuf_size(Process *p);
#if defined(DEBUG) || 0
#  define ERTS_CHK_MBUF_SZ(P)				\
    do {						\
	Uint actual_mbuf_sz__ = erts_mbuf_size((P));	\
	ERTS_ASSERT((P)->mbuf_sz >= actual_mbuf_sz__);	\
    } while (0)
#else
#  define ERTS_CHK_MBUF_SZ(P) ((void) 1)
#endif

#define ERTS_FOREACH_SIG_PRIVQS(PROC, MVAR, CODE)                       \
    do {                                                                \
        int i__;                                                        \
        ErtsMessage *msgs__[] = {(PROC)->sig_qs.first,                  \
                                 (PROC)->sig_qs.cont};                  \
        for (i__ = 0; i__ < sizeof(msgs__)/sizeof(msgs__[0]); i__++) {  \
            ErtsMessage *MVAR;                                          \
            for (MVAR = msgs__[i__]; MVAR; MVAR = MVAR->next) {         \
                CODE;                                                   \
            }                                                           \
        }                                                               \
    } while (0)

#endif
