/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2012. All Rights Reserved.
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

#ifndef __ERL_MESSAGE_H__
#define __ERL_MESSAGE_H__

struct proc_bin;
struct external_thing_;

/*
 * This struct represents data that must be updated by structure copy,
 * but is stored outside of any heap.
 */

struct erl_off_heap_header {
    Eterm thing_word;
    Uint size;
#if HALFWORD_HEAP
    void* dummy_ptr_padding__;
#endif
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
#include "external.h"
#include "erl_process.h"

/*
 * This struct represents a heap fragment, which is used when there
 * isn't sufficient room in the process heap and we can't do a GC.
 */

typedef struct erl_heap_fragment ErlHeapFragment;
struct erl_heap_fragment {
    ErlHeapFragment* next;	/* Next heap fragment */
    ErlOffHeap off_heap;	/* Offset heap data. */
    unsigned alloc_size;	/* Size in (half)words of mem */
    unsigned used_size;         /* With terms to be moved to heap by GC */
    Eterm mem[1];		/* Data */
};

typedef struct erl_mesg {
    struct erl_mesg* next;	/* Next message */
    union {
	ErtsDistExternal *dist_ext;
	ErlHeapFragment *heap_frag;
	void *attached;
    } data;
#ifdef USE_VM_PROBES
    Eterm m[3];                 /* m[0] = message, m[1] = seq trace token, m[3] = dynamic trace user tag */
#else
    Eterm m[2];			/* m[0] = message, m[1] = seq trace token */
#endif
} ErlMessage;

#define ERL_MESSAGE_TERM(mp) ((mp)->m[0])
#define ERL_MESSAGE_TOKEN(mp) ((mp)->m[1])
#ifdef USE_VM_PROBES
#define ERL_MESSAGE_DT_UTAG(mp) ((mp)->m[2])
#endif

/* Size of default message buffer (erl_message.c) */
#define ERL_MESSAGE_BUF_SZ 500

typedef struct {
    ErlMessage* first;
    ErlMessage** last;  /* point to the last next pointer */
    ErlMessage** save;
    Sint len;            /* queue length */

    /*
     * The following two fields are used by the recv_mark/1 and
     * recv_set/1 instructions.
     */
    BeamInstr* mark;		/* address to rec_loop/2 instruction */
    ErlMessage** saved_last;	/* saved last pointer */
} ErlMessageQueue;

#ifdef ERTS_SMP

typedef struct {
    ErlMessage* first;
    ErlMessage** last;  /* point to the last next pointer */
    Sint len;            /* queue length */
} ErlMessageInQueue;

#endif

/* Get "current" message */
#define PEEK_MESSAGE(p)  (*(p)->msg.save)


/* Add message last in private message queue */
#define LINK_MESSAGE_PRIVQ(p, mp) do { \
    *(p)->msg.last = (mp); \
    (p)->msg.last = &(mp)->next; \
    (p)->msg.len++; \
} while(0)


#ifdef ERTS_SMP

/* Move in message queue to end of private message queue */
#define ERTS_SMP_MSGQ_MV_INQ2PRIVQ(P)					\
do {									\
    if ((P)->msg_inq.first) {						\
	*(P)->msg.last = (P)->msg_inq.first;				\
	(P)->msg.last = (P)->msg_inq.last;				\
	(P)->msg.len += (P)->msg_inq.len;				\
	(P)->msg_inq.first = NULL;					\
	(P)->msg_inq.last = &(P)->msg_inq.first;			\
	(P)->msg_inq.len = 0;						\
    }									\
} while (0)

/* Add message last in message queue */
#define LINK_MESSAGE(p, mp) do { \
    *(p)->msg_inq.last = (mp); \
    (p)->msg_inq.last = &(mp)->next; \
    (p)->msg_inq.len++; \
} while(0)

#else

#define ERTS_SMP_MSGQ_MV_INQ2PRIVQ(P)

/* Add message last in message queue */
#define LINK_MESSAGE(p, mp) LINK_MESSAGE_PRIVQ((p), (mp))

#endif

/* Unlink current message */
#define UNLINK_MESSAGE(p,msgp) do { \
     ErlMessage* __mp = (msgp)->next; \
     *(p)->msg.save = __mp; \
     (p)->msg.len--; \
     if (__mp == NULL) \
         (p)->msg.last = (p)->msg.save; \
     (p)->msg.mark = 0; \
} while(0)

/* Reset message save point (after receive match) */
#define JOIN_MESSAGE(p) \
     (p)->msg.save = &(p)->msg.first

/* Save current message */
#define SAVE_MESSAGE(p) \
     (p)->msg.save = &(*(p)->msg.save)->next

/*
 * ErtsMoveMsgAttachmentIntoProc() moves data attached to a message
 * onto the heap of a process. The attached data is the content of
 * the the message either on the internal format or on the external
 * format, and also possibly a seq trace token on the internal format.
 * If the message content is on the external format, the decode might
 * fail. If the decoding fails, ERL_MESSAGE_TERM(M) will contain
 * THE_NON_VALUE. That is, ERL_MESSAGE_TERM(M) *has* to be checked
 * afterwards and taken care of appropriately.
 *
 * ErtsMoveMsgAttachmentIntoProc() will shallow copy to heap if
 * possible; otherwise, move to heap via garbage collection.
 *
 * ErtsMoveMsgAttachmentIntoProc() is used when receiveing messages
 * in process_main() and in hipe_check_get_msg().
 */

#define ErtsMoveMsgAttachmentIntoProc(M, P, ST, HT, FC, SWPO, SWPI)	\
do {									\
    if ((M)->data.attached) {						\
	Uint need__ = erts_msg_attached_data_size((M));			\
 	if ((ST) - (HT) >= need__) {					\
	    Uint *htop__ = (HT);					\
	    erts_move_msg_attached_data_to_heap(&htop__, &MSO((P)), (M));\
	    ASSERT(htop__ - (HT) <= need__);				\
	    (HT) = htop__;						\
	}								\
	else {								\
	    { SWPO ; }							\
	    (FC) -= erts_garbage_collect((P), 0, NULL, 0);		\
	    { SWPI ; }							\
	}								\
	ASSERT(!(M)->data.attached);					\
    }									\
} while (0)

#define ERTS_SND_FLG_NO_SEQ_TRACE		(((unsigned) 1) << 0)

#define ERTS_HEAP_FRAG_SIZE(DATA_WORDS) \
   (sizeof(ErlHeapFragment) - sizeof(Eterm) + (DATA_WORDS)*sizeof(Eterm))

#define ERTS_INIT_HEAP_FRAG(HEAP_FRAG_P, DATA_WORDS)	\
do {							\
    (HEAP_FRAG_P)->next = NULL;				\
    (HEAP_FRAG_P)->alloc_size = (DATA_WORDS);		\
    (HEAP_FRAG_P)->used_size = (DATA_WORDS);            \
    (HEAP_FRAG_P)->off_heap.first = NULL; 	        \
    (HEAP_FRAG_P)->off_heap.overhead = 0;		\
} while (0)

void init_message(void);
void free_message(ErlMessage *);
ErlHeapFragment* new_message_buffer(Uint);
ErlHeapFragment* erts_resize_message_buffer(ErlHeapFragment *, Uint,
					    Eterm *, Uint);
void free_message_buffer(ErlHeapFragment *);
void erts_queue_dist_message(Process*, ErtsProcLocks*, ErtsDistExternal *, Eterm);
void erts_queue_message(Process*, ErtsProcLocks*, ErlHeapFragment*, Eterm, Eterm
#ifdef USE_VM_PROBES
		   , Eterm dt_utag
#endif
);
void erts_deliver_exit_message(Eterm, Process*, ErtsProcLocks *, Eterm, Eterm);
Sint erts_send_message(Process*, Process*, ErtsProcLocks*, Eterm, unsigned);
void erts_link_mbuf_to_proc(Process *proc, ErlHeapFragment *bp);

void erts_move_msg_mbuf_to_heap(Eterm**, ErlOffHeap*, ErlMessage *);

Uint erts_msg_attached_data_size_aux(ErlMessage *msg);
void erts_move_msg_attached_data_to_heap(Eterm **, ErlOffHeap *, ErlMessage *);

Eterm erts_msg_distext2heap(Process *, ErtsProcLocks *, ErlHeapFragment **,
			    Eterm *, ErtsDistExternal *);

void erts_cleanup_offheap(ErlOffHeap *offheap);


ERTS_GLB_INLINE Uint erts_msg_used_frag_sz(const ErlMessage *msg);
ERTS_GLB_INLINE Uint erts_msg_attached_data_size(ErlMessage *msg);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Uint erts_msg_used_frag_sz(const ErlMessage *msg)
{
    const ErlHeapFragment *bp;
    Uint sz = 0;
    for (bp = msg->data.heap_frag; bp!=NULL; bp=bp->next) {
	sz += bp->used_size;
    }
    return sz;
}

ERTS_GLB_INLINE Uint erts_msg_attached_data_size(ErlMessage *msg)
{
    ASSERT(msg->data.attached);
    if (is_value(ERL_MESSAGE_TERM(msg)))
	return erts_msg_used_frag_sz(msg);
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

#endif
