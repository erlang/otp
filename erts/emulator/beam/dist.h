/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

#ifndef __DIST_H__
#define __DIST_H__

#include "erl_process.h"
#include "erl_node_tables.h"
#include "zlib.h"

#define DFLAG_PUBLISHED                ((Uint64)0x01)
#define DFLAG_ATOM_CACHE               ((Uint64)0x02)
#define DFLAG_EXTENDED_REFERENCES      ((Uint64)0x04)
#define DFLAG_DIST_MONITOR             ((Uint64)0x08)
#define DFLAG_FUN_TAGS                 ((Uint64)0x10)
#define DFLAG_DIST_MONITOR_NAME        ((Uint64)0x20)
#define DFLAG_HIDDEN_ATOM_CACHE        ((Uint64)0x40)
#define DFLAG_NEW_FUN_TAGS             ((Uint64)0x80)
#define DFLAG_EXTENDED_PIDS_PORTS     ((Uint64)0x100)
#define DFLAG_EXPORT_PTR_TAG          ((Uint64)0x200)
#define DFLAG_BIT_BINARIES            ((Uint64)0x400)
#define DFLAG_NEW_FLOATS              ((Uint64)0x800)
#define DFLAG_UNICODE_IO             ((Uint64)0x1000)
#define DFLAG_DIST_HDR_ATOM_CACHE    ((Uint64)0x2000)
#define DFLAG_SMALL_ATOM_TAGS        ((Uint64)0x4000)
#define DFLAG_ETS_COMPRESSED         ((Uint64)0x8000) /* internal */
#define DFLAG_UTF8_ATOMS            ((Uint64)0x10000)
#define DFLAG_MAP_TAG               ((Uint64)0x20000)
#define DFLAG_BIG_CREATION          ((Uint64)0x40000)
#define DFLAG_SEND_SENDER           ((Uint64)0x80000)
#define DFLAG_BIG_SEQTRACE_LABELS  ((Uint64)0x100000)
#define DFLAG_PENDING_CONNECT      ((Uint64)0x200000) /* internal */
#define DFLAG_EXIT_PAYLOAD         ((Uint64)0x400000)
#define DFLAG_FRAGMENTS            ((Uint64)0x800000)
#define DFLAG_HANDSHAKE_23        ((Uint64)0x1000000)
#define DFLAG_UNLINK_ID           ((Uint64)0x2000000)
#define DFLAG_MANDATORY_25_DIGEST ((Uint64)0x4000000)
#define DFLAG_RESERVED           ((Uint64)0xf8000000)

/*
 * As the old handshake only support 32 flag bits, we reserve the remaining
 * bits in the lower 32 for changes in the handshake protocol or potentially
 * new capabilities that we also want to backport to OTP-22 or older.
 */
#define DFLAG_SPAWN            (((Uint64)0x1) << 32)
#define DFLAG_NAME_ME          (((Uint64)0x2) << 32)
#define DFLAG_V4_NC            (((Uint64)0x4) << 32)
#define DFLAG_ALIAS            (((Uint64)0x8) << 32)
#define DFLAG_LOCAL_EXT        (((Uint64)0x10) << 32) /* internal */
#define DFLAG_ALTACT_SIG       (((Uint64)0x20) << 32)

/*
 * In term_to_binary/2, we will use DFLAG_ATOM_CACHE to mean
 * DFLAG_DETERMINISTIC.
 */

#define DFLAG_DETERMINISTIC            DFLAG_ATOM_CACHE

/* Mandatory flags for distribution in OTP 25. */
#define DFLAG_DIST_MANDATORY_25 (DFLAG_EXTENDED_REFERENCES        \
                                | DFLAG_FUN_TAGS                  \
                                | DFLAG_EXTENDED_PIDS_PORTS       \
                                | DFLAG_UTF8_ATOMS                \
                                | DFLAG_NEW_FUN_TAGS              \
                                | DFLAG_BIG_CREATION              \
                                | DFLAG_NEW_FLOATS                \
                                | DFLAG_MAP_TAG                   \
                                | DFLAG_EXPORT_PTR_TAG            \
                                | DFLAG_BIT_BINARIES              \
                                | DFLAG_HANDSHAKE_23)

/* New mandatory flags for distribution in OTP 26 */
#define DFLAG_DIST_MANDATORY_26 (DFLAG_V4_NC                      \
                                 | DFLAG_UNLINK_ID)

/* Mandatory flags for distribution. */
#define DFLAG_DIST_MANDATORY (DFLAG_DIST_MANDATORY_25             \
                              | DFLAG_DIST_MANDATORY_26)

/*
 * Additional optimistic flags when encoding toward pending connection.
 * If remote node (erl_interface) does not support these then we may need
 * to transcode messages enqueued before connection setup was finished.
 */
#define DFLAG_DIST_HOPEFULLY (DFLAG_DIST_MONITOR                \
                              | DFLAG_DIST_MONITOR_NAME         \
                              | DFLAG_SPAWN                     \
                              | DFLAG_ALTACT_SIG                \
			      | DFLAG_ALIAS)

/* Our preferred set of flags. Used for connection setup handshake */
#define DFLAG_DIST_DEFAULT (DFLAG_DIST_MANDATORY | DFLAG_DIST_HOPEFULLY \
                            | DFLAG_UNICODE_IO                \
                            | DFLAG_DIST_HDR_ATOM_CACHE       \
                            | DFLAG_SMALL_ATOM_TAGS           \
                            | DFLAG_SEND_SENDER               \
                            | DFLAG_BIG_SEQTRACE_LABELS       \
                            | DFLAG_EXIT_PAYLOAD              \
                            | DFLAG_FRAGMENTS                 \
                            | DFLAG_SPAWN                     \
                            | DFLAG_ALIAS		      \
                            | DFLAG_MANDATORY_25_DIGEST)

/* Flags addable by local distr implementations */
#define DFLAG_DIST_ADDABLE    DFLAG_DIST_DEFAULT

/* Flags rejectable by local distr implementation */
#define DFLAG_DIST_REJECTABLE (DFLAG_DIST_HDR_ATOM_CACHE         \
                               | DFLAG_HIDDEN_ATOM_CACHE         \
                               | DFLAG_FRAGMENTS                 \
                               | DFLAG_ATOM_CACHE)

/* Flags for all features needing strict order delivery */
#define DFLAG_DIST_STRICT_ORDER DFLAG_DIST_HDR_ATOM_CACHE

/* All flags that should be enabled when term_to_binary/1 is used. */
#define TERM_TO_BINARY_DFLAGS (DFLAG_NEW_FLOATS                  \
                               | DFLAG_UTF8_ATOMS)

/* opcodes used in distribution messages */
enum dop {
    DOP_LINK                = 1,
    DOP_SEND                = 2,
    DOP_EXIT                = 3,
    DOP_UNLINK              = 4,
/* Ancient DOP_NODE_LINK (5) was here, can be reused */
    DOP_REG_SEND            = 6,
    DOP_GROUP_LEADER        = 7,
    DOP_EXIT2               = 8,

    DOP_SEND_TT             = 12,
    DOP_EXIT_TT             = 13,
    DOP_REG_SEND_TT         = 16,
    DOP_EXIT2_TT            = 18,

    DOP_MONITOR_P           = 19,
    DOP_DEMONITOR_P         = 20,
    DOP_MONITOR_P_EXIT      = 21,

    DOP_SEND_SENDER         = 22,
    DOP_SEND_SENDER_TT      = 23,

    /* These are used when DFLAG_EXIT_PAYLOAD is detected */
    DOP_PAYLOAD_EXIT           = 24,
    DOP_PAYLOAD_EXIT_TT        = 25,
    DOP_PAYLOAD_EXIT2          = 26,
    DOP_PAYLOAD_EXIT2_TT       = 27,
    DOP_PAYLOAD_MONITOR_P_EXIT = 28,

    DOP_SPAWN_REQUEST       = 29,
    DOP_SPAWN_REQUEST_TT    = 30,
    DOP_SPAWN_REPLY         = 31,
    DOP_SPAWN_REPLY_TT      = 32,

    DOP_ALIAS_SEND          = 33,
    DOP_ALIAS_SEND_TT       = 34,

    DOP_UNLINK_ID           = 35,
    DOP_UNLINK_ID_ACK       = 36,

    DOP_ALTACT_SIG_SEND     = 37
};

#define ERTS_DIST_SPAWN_FLAG_LINK       (1 << 0)
#define ERTS_DIST_SPAWN_FLAG_MONITOR    (1 << 1)

#define ERTS_DOP_ALTACT_SIG_FLG_PRIO    (1 << 0)
#define ERTS_DOP_ALTACT_SIG_FLG_TOKEN   (1 << 1)
#define ERTS_DOP_ALTACT_SIG_FLG_ALIAS   (1 << 2)
#define ERTS_DOP_ALTACT_SIG_FLG_NAME    (1 << 3)
#define ERTS_DOP_ALTACT_SIG_FLG_EXIT    (1 << 4)

/* distribution trap functions */
extern Export* dmonitor_node_trap;

typedef enum {
    ERTS_DSP_NO_LOCK,
    ERTS_DSP_RLOCK
} ErtsDSigPrepLock;


/* Must be larger or equal to 16 */
#ifdef DEBUG
#define ERTS_DIST_FRAGMENT_SIZE 1024
#else
/* This should be made configurable */
#define ERTS_DIST_FRAGMENT_SIZE (64 * 1024)
#endif

#define ERTS_DIST_FRAGMENT_HEADER_SIZE (1 + 1 + 8 + 8) /* magic, header, seq id, frag id*/

#define ERTS_DE_BUSY_LIMIT (1024*1024)
extern int erts_dist_buf_busy_limit;
extern int erts_is_alive;

/*
 * erts_dsig_prepare() prepares a send of a distributed signal.
 * One of the values defined below are returned.
 */

/* Connected; signals can be enqueued and sent. */
#define ERTS_DSIG_PREP_CONNECTED	0
/* Not connected; connection needs to be set up. */
#define ERTS_DSIG_PREP_NOT_CONNECTED	1
/* Caller would be suspended on send operation. */
#define ERTS_DSIG_PREP_WOULD_SUSPEND	2
/* System not alive (distributed) */
#define ERTS_DSIG_PREP_NOT_ALIVE	3
/* Pending connection; signals can be enqueued */
#define ERTS_DSIG_PREP_PENDING	        4

/* dist_ctrl_{g,s}et_option/2 */
#define ERTS_DIST_CTRL_OPT_GET_SIZE     ((Uint32) (1 << 0))

/* for emulator internal testing... */
extern Uint64 erts_dflags_test_remove;

#ifdef DEBUG
#define ERTS_DBG_CHK_NO_DIST_LNK(D, R, L) \
    erts_dbg_chk_no_dist_proc_link((D), (R), (L))
#else
#define ERTS_DBG_CHK_NO_DIST_LNK(D, R, L)
#endif

/* Define for testing */
/* #define EXTREME_TTB_TRAPPING 1 */

#ifndef EXTREME_TTB_TRAPPING
#define TERM_TO_BINARY_LOOP_FACTOR 32
#else
#define TERM_TO_BINARY_LOOP_FACTOR 1
#endif

typedef enum { TTBSize, TTBEncode, TTBCompress } TTBState;
typedef struct TTBSizeContext_ {
    Uint64 dflags;
    int level;
    Sint vlen;
    int iovec;
    Uint fragment_size;
    Uint last_result;
    Uint extra_size;
    Uint result;
    Eterm obj;
    ErtsWStack wstack;
} TTBSizeContext;

#define ERTS_INIT_TTBSizeContext(Ctx, Flags)                    \
    do {                                                        \
        (Ctx)->wstack.wstart = NULL;                            \
        (Ctx)->dflags = (Flags);                                 \
        (Ctx)->level = 0;                                       \
        (Ctx)->vlen = -1;                                       \
        (Ctx)->fragment_size = ~((Uint) 0);                     \
        (Ctx)->extra_size = 0;                                  \
        (Ctx)->last_result = 0;                                 \
    } while (0)

typedef struct TTBEncodeContext_ {
    Uint64 dflags;
    Uint64 hopeful_flags;
    byte *hopeful_flagsp;
    int level;
    byte* ep;
    Eterm obj;
    ErtsWStack wstack;
    Eterm* map_array;
    Eterm* next_map_element;
    void* ycf_yield_state;
    Binary *result_bin;
    byte *cptr;
    Sint vlen;
    Uint size;
    byte *payload_ixp;
    byte *hopeful_ixp;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    Eterm *termv;
    Uint fragment_size;
    Sint frag_ix;
    ErlIOVec *fragment_eiovs;
    int iovec;
    int continue_make_lext_hash;
    int lext_vlen;
    byte *lext_hash;
    union {
        ErtsBlockHashState block;
        ErtsIovBlockHashState iov_block;
    } lext_state;
#ifdef DEBUG
    int debug_fragments;
    int debug_vlen;
#endif
} TTBEncodeContext;

#define ERTS_INIT_TTBEncodeContext(Ctx, Flags)                  \
    do {                                                        \
        (Ctx)->wstack.wstart = NULL;                            \
        (Ctx)->dflags = (Flags);                                \
        (Ctx)->level = 0;                                       \
        (Ctx)->map_array = 0;                                   \
        (Ctx)->ycf_yield_state = 0;                             \
        (Ctx)->vlen = 0;                                        \
        (Ctx)->size = 0;                                        \
        (Ctx)->termv = NULL;                                    \
        (Ctx)->iov = NULL;                                      \
        (Ctx)->binv = NULL;                                     \
        (Ctx)->fragment_size = ~((Uint) 0);                     \
        (Ctx)->continue_make_lext_hash = 0;                     \
        (Ctx)->lext_vlen = -1;                                  \
        if ((Flags) & DFLAG_PENDING_CONNECT) {                  \
            (Ctx)->hopeful_flags = 0;                           \
            (Ctx)->hopeful_flagsp = NULL;                       \
            (Ctx)->hopeful_ixp = NULL;                          \
            (Ctx)->payload_ixp = NULL;                          \
        }                                                       \
    } while (0)

typedef struct {
    Uint real_size;
    Uint dest_len;
    byte *dbytes;
    Binary *result_bin;
    Binary *destination_bin;
    z_stream stream;
} TTBCompressContext;

typedef struct {
    int alive;
    TTBState state;
    union {
	TTBSizeContext sc;
	TTBEncodeContext ec;
	TTBCompressContext cc;
    } s;
} TTBContext;

enum erts_dsig_send_phase {
    ERTS_DSIG_SEND_PHASE_INIT,
    ERTS_DSIG_SEND_PHASE_MSG_SIZE,
    ERTS_DSIG_SEND_PHASE_ALLOC,
    ERTS_DSIG_SEND_PHASE_MSG_ENCODE,
    ERTS_DSIG_SEND_PHASE_FIN,
    ERTS_DSIG_SEND_PHASE_SEND
};

typedef struct erts_dsig_send_context {
    int connect;
    int no_suspend;
    int no_trap;
    int ignore_busy;

    Eterm ctl;
    Eterm msg;
    Eterm from;
    Eterm ctl_heap[8]; /* 7-tuple (SPAWN_REQUEST_TT) */
    Eterm return_term;

    DistEntry *dep;
    Eterm node;   /* used if dep == NULL */
    Eterm cid;
    Eterm connection_id;
    int deref_dep;

    enum erts_dsig_send_phase phase;
    Sint reds;

    Uint data_size, dhdr_ext_size;
    byte *dhdrp, *extp;
    ErtsAtomCacheMap *acmp;
    ErtsDistOutputBuf *obuf;
    Uint alloced_fragments, fragments;
    Sint vlen;
    Uint64 dflags;
    Process *c_p;
    union {
	TTBSizeContext sc;
	TTBEncodeContext ec;
    }u;

} ErtsDSigSendContext;

typedef struct dist_sequences DistSeqNode;

struct dist_sequences {
    ErlHeapFragment hfrag;
    struct dist_sequences *parent;
    struct dist_sequences *left;
    struct dist_sequences *right;
    char is_red;

    Uint64 seq_id;
    int cnt;
    Sint ctl_len;
};

/*
 * erts_dsig_send_* return values.
 */
#define ERTS_DSIG_SEND_OK	0
#define ERTS_DSIG_SEND_YIELD	1
#define ERTS_DSIG_SEND_CONTINUE 2
#define ERTS_DSIG_SEND_TOO_LRG  3

extern int erts_dsig_send_msg(ErtsDSigSendContext*, Eterm, Eterm, Eterm, int);
extern int erts_dsig_send_link(ErtsDSigSendContext *, Eterm, Eterm);
extern int erts_dsig_send_exit_tt(ErtsDSigSendContext *, Process *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_unlink(ErtsDSigSendContext *, Eterm, Eterm, Uint64);
extern int erts_dsig_send_unlink_ack(ErtsDSigSendContext *, Eterm, Eterm, Uint64);
extern int erts_dsig_send_group_leader(ErtsDSigSendContext *, Eterm, Eterm);
extern int erts_dsig_send_exit(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_exit2(ErtsDSigSendContext *, Eterm, Eterm, Eterm, int);
extern int erts_dsig_send_demonitor(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_monitor(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_m_exit(ErtsDSigSendContext *, Eterm, Eterm, Eterm, Eterm);
extern int erts_dsig_send_spawn_reply(ErtsDSigSendContext *, Eterm, Eterm, Eterm, Eterm, Eterm);

extern int erts_dsig_send(ErtsDSigSendContext *dsdp);
extern int erts_dsend_context_dtor(Binary*);
extern Eterm erts_dsend_export_trap_context(Process* p, ErtsDSigSendContext* ctx);

extern int erts_dist_command(Port *prt, int reds);
extern void erts_dist_port_not_busy(Port *prt);
extern void erts_kill_dist_connection(DistEntry *dep, Uint32);

extern Uint erts_dist_cache_size(void);

extern Sint erts_abort_pending_connection_rwunlock(DistEntry *dep, int *);

extern void erts_debug_dist_seq_tree_foreach(
    DistEntry *dep,
    int (*func)(DistSeqNode *, void*, Sint), void *args);

extern int erts_dsig_prepare(ErtsDSigSendContext *,
                             DistEntry*,
                             Process *,
                             ErtsProcLocks,
                             ErtsDSigPrepLock,
                             int,
                             int,
                             int);

void erts_dist_print_procs_suspended_on_de(fmtfn_t to, void *to_arg);
int erts_auto_connect(DistEntry* dep, Process *proc, ErtsProcLocks proc_locks);

Uint erts_ttb_iov_size(int use_termv, Sint vlen, Uint fragments);
void erts_ttb_iov_init(TTBEncodeContext *ctx, int use_termv, char *ptr,
                       Sint vlen, Uint fragments, Uint fragments_size);

int erts_is_this_node_alive(void);

#endif
