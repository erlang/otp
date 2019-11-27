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

#ifndef __DIST_H__
#define __DIST_H__

#include "erl_process.h"
#include "erl_node_tables.h"
#include "zlib.h"

#define DFLAG_PUBLISHED           0x01
#define DFLAG_ATOM_CACHE          0x02
#define DFLAG_EXTENDED_REFERENCES 0x04
#define DFLAG_DIST_MONITOR        0x08
#define DFLAG_FUN_TAGS            0x10
#define DFLAG_DIST_MONITOR_NAME   0x20
#define DFLAG_HIDDEN_ATOM_CACHE   0x40
#define DFLAG_NEW_FUN_TAGS        0x80
#define DFLAG_EXTENDED_PIDS_PORTS 0x100
#define DFLAG_EXPORT_PTR_TAG      0x200
#define DFLAG_BIT_BINARIES        0x400
#define DFLAG_NEW_FLOATS          0x800
#define DFLAG_UNICODE_IO          0x1000
#define DFLAG_DIST_HDR_ATOM_CACHE 0x2000
#define DFLAG_SMALL_ATOM_TAGS     0x4000
#define DFLAG_INTERNAL_TAGS       0x8000   /* used by ETS 'compressed' option */
#define DFLAG_UTF8_ATOMS          0x10000
#define DFLAG_MAP_TAG             0x20000
#define DFLAG_BIG_CREATION        0x40000
#define DFLAG_SEND_SENDER         0x80000
#define DFLAG_BIG_SEQTRACE_LABELS 0x100000
#define DFLAG_NO_MAGIC            0x200000 /* internal for pending connection */
#define DFLAG_EXIT_PAYLOAD        0x400000
#define DFLAG_FRAGMENTS           0x800000

/* Mandatory flags for distribution */
#define DFLAG_DIST_MANDATORY (DFLAG_EXTENDED_REFERENCES         \
                              | DFLAG_EXTENDED_PIDS_PORTS       \
			      | DFLAG_UTF8_ATOMS                \
			      | DFLAG_NEW_FUN_TAGS)

/*
 * Additional optimistic flags when encoding toward pending connection.
 * If remote node (erl_interface) does not supporting these then we may need
 * to transcode messages enqueued before connection setup was finished.
 */
#define DFLAG_DIST_HOPEFULLY (DFLAG_EXPORT_PTR_TAG              \
                              | DFLAG_BIT_BINARIES              \
                              | DFLAG_DIST_MONITOR              \
                              | DFLAG_DIST_MONITOR_NAME)

/* Our preferred set of flags. Used for connection setup handshake */
#define DFLAG_DIST_DEFAULT (DFLAG_DIST_MANDATORY | DFLAG_DIST_HOPEFULLY \
                            | DFLAG_FUN_TAGS                  \
                            | DFLAG_NEW_FLOATS                \
                            | DFLAG_UNICODE_IO                \
                            | DFLAG_DIST_HDR_ATOM_CACHE       \
                            | DFLAG_SMALL_ATOM_TAGS           \
                            | DFLAG_UTF8_ATOMS                \
                            | DFLAG_MAP_TAG                   \
                            | DFLAG_BIG_CREATION              \
                            | DFLAG_SEND_SENDER               \
                            | DFLAG_BIG_SEQTRACE_LABELS       \
                            | DFLAG_EXIT_PAYLOAD              \
                            | DFLAG_FRAGMENTS)

/* Flags addable by local distr implementations */
#define DFLAG_DIST_ADDABLE    DFLAG_DIST_DEFAULT

/* Flags rejectable by local distr implementation */
#define DFLAG_DIST_REJECTABLE (DFLAG_DIST_HDR_ATOM_CACHE         \
                               | DFLAG_HIDDEN_ATOM_CACHE         \
                               | DFLAG_ATOM_CACHE)

/* Flags for all features needing strict order delivery */
#define DFLAG_DIST_STRICT_ORDER DFLAG_DIST_HDR_ATOM_CACHE

/* All flags that should be enabled when term_to_binary/1 is used. */
#define TERM_TO_BINARY_DFLAGS (DFLAG_EXTENDED_REFERENCES	\
			       | DFLAG_NEW_FUN_TAGS		\
			       | DFLAG_NEW_FLOATS		\
			       | DFLAG_EXTENDED_PIDS_PORTS	\
			       | DFLAG_EXPORT_PTR_TAG		\
			       | DFLAG_BIT_BINARIES             \
			       | DFLAG_MAP_TAG                  \
                               | DFLAG_BIG_CREATION)

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
    DOP_PAYLOAD_MONITOR_P_EXIT = 28
};

/* distribution trap functions */
extern Export* dmonitor_node_trap;

typedef enum {
    ERTS_DSP_NO_LOCK,
    ERTS_DSP_RLOCK
} ErtsDSigPrepLock;


/* Must be larger or equal to 16 */
#ifdef DEBUG
#define ERTS_DIST_FRAGMENT_SIZE 16
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
    Uint flags;
    int level;
    Uint result;
    Eterm obj;
    ErtsWStack wstack;
} TTBSizeContext;

typedef struct TTBEncodeContext_ {
    Uint flags;
    Uint hopefull_flags;
    int level;
    byte* ep;
    Eterm obj;
    ErtsWStack wstack;
    Binary *result_bin;
} TTBEncodeContext;

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

    Eterm ctl;
    Eterm msg;
    Eterm from;
    Eterm ctl_heap[6];
    Eterm return_term;

    DistEntry *dep;
    Eterm node;   /* used if dep == NULL */
    Eterm cid;
    Eterm connection_id;
    int deref_dep;

    enum erts_dsig_send_phase phase;
    Sint reds;

    Uint32 max_finalize_prepend;
    Uint data_size, dhdr_ext_size;
    ErtsAtomCacheMap *acmp;
    ErtsDistOutputBuf *obuf;
    Uint fragments;
    Uint32 flags;
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

extern int erts_dsig_send_msg(ErtsDSigSendContext*, Eterm, Eterm);
extern int erts_dsig_send_reg_msg(ErtsDSigSendContext*, Eterm, Eterm, Eterm);
extern int erts_dsig_send_link(ErtsDSigSendContext *, Eterm, Eterm);
extern int erts_dsig_send_exit_tt(ErtsDSigSendContext *, Eterm, Eterm, Eterm, Eterm);
extern int erts_dsig_send_unlink(ErtsDSigSendContext *, Eterm, Eterm);
extern int erts_dsig_send_group_leader(ErtsDSigSendContext *, Eterm, Eterm);
extern int erts_dsig_send_exit(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_exit2(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_demonitor(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_monitor(ErtsDSigSendContext *, Eterm, Eterm, Eterm);
extern int erts_dsig_send_m_exit(ErtsDSigSendContext *, Eterm, Eterm, Eterm, Eterm);

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
#endif
