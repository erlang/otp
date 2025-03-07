/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2025. All Rights Reserved.
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
 *
 * ----------------------------------------------------------------------
 *  Purpose : "Global" Types and stuff for socket.
 * ----------------------------------------------------------------------
 *
 */

#ifndef PRIM_SOCKET_INT_H__
#define PRIM_SOCKET_INT_H__

#include <erl_nif.h>
#include <sys.h>

#include "socket_int.h"
#include "socket_dbg.h"


/* ********************************************************************* *
 *                   Socket state defs and macros                        *
 * ********************************************************************* *
 */

#define ESOCK_STATE_BOUND        0x0001 /* readState */
#define ESOCK_STATE_LISTENING    0x0002 /* readState */
#define ESOCK_STATE_ACCEPTING    0x0004 /* readState */
#define ESOCK_STATE_CONNECTING   0x0010 /* writeState */
#define ESOCK_STATE_CONNECTED    0x0020 /* writeState */

/* This is set in either readState or writeState
 * so it has to be read from both.
 * Means that the socket has been used in select,
 * so select_stop is required. */
#define ESOCK_STATE_SELECTED     0x0100 /* readState or writeState */

/* These are set in both readState and writeState
 * so they can be read from either. */
#define ESOCK_STATE_CLOSING      0x0200 /* readState and writeState */

#define ESOCK_STATE_CLOSED       0x0400 /* readState and writeState */
//
#define ESOCK_STATE_DTOR         0x8000

#define IS_BOUND(st)                           \
    (((st) & ESOCK_STATE_BOUND) != 0)

#define IS_CLOSED(st)                           \
    (((st) & ESOCK_STATE_CLOSED) != 0)

#define IS_CLOSING(st)                          \
    (((st) & ESOCK_STATE_CLOSING) != 0)

#define IS_ACCEPTING(st)                        \
    (((st) & ESOCK_STATE_ACCEPTING) != 0)

#define IS_OPEN(st)                                             \
    (((st) & (ESOCK_STATE_CLOSED | ESOCK_STATE_CLOSING)) == 0)

#define IS_SELECTED(d)                                                  \
    ((((d)->readState | (d)->writeState) & ESOCK_STATE_SELECTED) != 0)


#define ESOCK_DESC_PATTERN_CREATED 0x03030303
#define ESOCK_DESC_PATTERN_DTOR    0xC0C0C0C0


/* ==========================================================================
 * The ESOCK_IS_ERROR macro below is used for portability reasons.
 * While POSIX specifies that errors from socket-related system calls
 * should be indicated with a -1 return value, some users have experienced
 * non-Windows OS kernels that return negative values other than -1.
 * While one can argue that such kernels are technically broken, comparing
 * against values less than 0 covers their out-of-spec return values without
 * imposing incorrect semantics on systems that manage to correctly return -1
 * for errors, thus increasing Erlang's portability.
 */
#ifdef __WIN32__
#define ESOCK_IS_ERROR(val) ((val) == INVALID_SOCKET)
#else
#define ESOCK_IS_ERROR(val) ((val) < 0)
#endif


/* ********************************************************************* *
 *                                 Misc                                  *
 * ********************************************************************* *
 */

#define ESOCK_GET_RESOURCE(ENV, REF, RES) \
    enif_get_resource((ENV), (REF), esocks, (RES))

#define ESOCK_MON2TERM(E, M) \
    esock_make_monitor_term((E), (M))


/* ********************************************************************* *
 *                       Counter type and related "things"               *
 * ********************************************************************* *
 */

#if ESOCK_COUNTER_SIZE == 16

typedef Uint16                   ESockCounter;
#define ESOCK_COUNTER_MAX        ((ESockCounter) 0xFFFF)
#define MKCNT(ENV, CNT)          MKUI((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%u"

#elif ESOCK_COUNTER_SIZE == 24

typedef Uint32                   ESockCounter;
#define ESOCK_COUNTER_MAX        ((ESockCounter) 0xFFFFFF)
#define MKCNT(ENV, CNT)          MKUI((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%lu"

#elif ESOCK_COUNTER_SIZE == 32

typedef Uint32 ESockCounter;
#define ESOCK_COUNTER_MAX        (~((ESockCounter) 0))
#define MKCNT(ENV, CNT)          MKUI((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%lu"

#elif ESOCK_COUNTER_SIZE == 48

typedef Uint64                   ESockCounter;
#define ESOCK_COUNTER_MAX        ((ESockCounter) 0xFFFFFFFFFFFF)
#define MKCNT(ENV, CNT)          MKUI64((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%llu"

#elif ESOCK_COUNTER_SIZE == 64

typedef Uint64                   ESockCounter;
#define ESOCK_COUNTER_MAX        (~((ESockCounter) 0))
#define MKCNT(ENV, CNT)          MKUI64((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%llu"

#else

#error "Invalid counter size"

#endif

#define ESOCK_CNT_INC( __E__, __D__, SF, ACNT, CNT, INC)                \
    do {                                                                \
        if (esock_cnt_inc((CNT), (INC))) {                              \
	  esock_send_wrap_msg((__E__), (__D__), (SF), (ACNT));		\
	}								\
    } while (0)



/* ********************************************************************* *
 *                         (Socket) Debug macros                         *
 * ********************************************************************* *
 */

#define SSDBG( __D__ , proto )    ESOCK_DBG_PRINTF( (__D__)->dbg , proto )
#define SSDBG2( __DBG__ , proto ) ESOCK_DBG_PRINTF( (__DBG__) , proto )


/* ********************************************************************* *
 *                           Sendfile stuff                              *
 * ********************************************************************* *
 */

#if defined(HAVE_SENDFILE)

typedef struct {
    ESockCounter cnt;     // Calls to OS sendfile()
    ESockCounter byteCnt; // Bytes sent with sendfile
    ESockCounter fails;   // Failed sendfile operations
    ESockCounter max;     // Largest sendfile operation
    ESockCounter maxCnt;  // Counter for ="=
    ESockCounter pkg;     // Sendfile chunks
    ESockCounter pkgMax;  // Largest sendfile chunk
    ESockCounter tries;   // Started sendfile operations
    ESockCounter waits;   // Select's during sendfile
} ESockSendfileCounters;

#endif


/* ********************************************************************* *
 *                       Monitor wrapper type                            *
 * ********************************************************************* *
 */

typedef struct {
    ErlNifMonitor mon;
    BOOLEAN_T     isActive;
} ESockMonitor;




/* ********************************************************************* *
 *                 The 'request' data structure                          *
 * Each I/O request (write, read, accept, connect, ...) that cannot be   *
 * completed directly, is scheduled for later or asynchronous.           *
 * These requests are "pushed" into a request "queue".                   *
 * In the case of the classic (unix) implementation, this is an actual   *
 * queue which also takes care of the order of the requests.             *
 * In case of the I/O Completion Port (windows), it is only used as an   *
 * database (we "push" into the list but we never "pop" from the list,   *
 * we search and delete).                                                *
 * ********************************************************************* *
 */

typedef struct {
    ErlNifPid    pid; // PID of the requesting process
    ESockMonitor mon; // Monitor to the requesting process

    /* We need an environment for the copy of the ref we store here.
     * We will also use this environment for any messages we send
     * (with the ref in it). Such as the select message (used in the 
     * select call) or the abort message.
     */
    ErlNifEnv*   env;
    ERL_NIF_TERM ref; // The (unique) reference (ID) of the request

    /* The socket for which this request is made.
     * A pointer to an *optional* data structure. This is intended for
     * the 'overlapped' structure used by I/O Completion Port.
     * A request scheduled by the I/O Completion Port can be 'cancelled'
     * using the socket and the 'overlapped' data (provided when the
     * operation was scheduled).
     */
    SOCKET       sock;
    void*        dataP;
    
} ESockRequestor;

typedef struct esock_request_queue_element {
    struct esock_request_queue_element* nextP;
    ESockRequestor                      data;
} ESockRequestQueueElement;

typedef struct {
    ESockRequestQueueElement* first;
    ESockRequestQueueElement* last;
} ESockRequestQueue;



/* ********************************************************************* *
 *           Holding the socket level 'otp' option 'meta' term           *
 * ********************************************************************* *
 */

typedef struct{
    ErlNifEnv*   env;
    ERL_NIF_TERM ref;
} ESockMeta;



/* ********************************************************************* *
 *                     Control Message spec type                         *
 * ********************************************************************* *
 */

typedef struct {
    int type; // Message type

    // Function to encode into erlang term
    BOOLEAN_T (* encode)(ErlNifEnv*     env,
                         unsigned char* data,
                         size_t         dataLen,
                         ERL_NIF_TERM*  eResult);

    // Function to decode from erlang term
    BOOLEAN_T (* decode)(ErlNifEnv*      env,
                         ERL_NIF_TERM    eValue,
                         struct cmsghdr* cmsgP,
                         size_t          rem,
                         size_t*         usedP);
    
    ERL_NIF_TERM *nameP; // Pointer to option name atom
} ESockCmsgSpec;
    

/*----------------------------------------------------------------------------
 * Interface types and constants.
 *
 * The set of elements should be the same as for the type
 * msg_flag() in socket.erl.
 */
typedef struct {
    int           flag;
    ERL_NIF_TERM* name;
} ESockFlag;

extern const ESockFlag esock_msg_flags[];
extern const int       esock_msg_flags_length;
extern const ESockFlag esock_ioctl_flags[];
extern const int       esock_ioctl_flags_length;


/* ********************************************************************* *
 *                     The socket nif global info                        *
 * ********************************************************************* *
 */

typedef struct {
    /* These are for debugging, testing and the like */
    // ERL_NIF_TERM version;
    // ERL_NIF_TERM buildDate;

    /* XXX Should be locked but too awkward and small gain */
    BOOLEAN_T    dbg;
    BOOLEAN_T    useReg;
    BOOLEAN_T    eei;

    /* Registry stuff */
    ErlNifPid    regPid; /* Constant - not locked */

    /* IOV_MAX. Constant - not locked */
    int          iov_max;

    /* XXX
     * Should be locked but too awkward for no gain since it is not used yet
     */
    BOOLEAN_T    iow; // Where do we send this? Subscription?

    ErlNifMutex* protocolsMtx;

    ErlNifMutex* cntMtx; /* Locks the below */
    /* Its extreme overkill to have these counters be 64-bit,
     * but since the other counters are, it's much simpler to
     * let these be 64-bit also.
     */
    ESockCounter numSockets;
    ESockCounter numTypeStreams;
    ESockCounter numTypeDGrams;
    ESockCounter numTypeSeqPkgs;
    ESockCounter numDomainInet;
    ESockCounter numDomainInet6;
    ESockCounter numDomainLocal;
    ESockCounter numProtoIP;
    ESockCounter numProtoTCP;
    ESockCounter numProtoUDP;
    ESockCounter numProtoSCTP;
    //
    BOOLEAN_T    sockDbg;
} ESockData;



/* ********************************************************************* *
 *                       The socket descriptor                           *
 * ********************************************************************* *
 */

typedef struct {
    /* 
     * +++ This is a way to, possibly, detect memory overrides "and stuff" +++
     *
     * We have two patterns. One is set when the descriptor is created
     * (allocated) and one is set when the descriptor is dtor'ed.
     */
    Uint32             pattern;

    /* +++ Stuff "about" the socket +++ */

    /* "Constant" - set when socket is created and never changed */
    int                domain;
    int                type;
    int                protocol;

    /* The state is partly for debugging, decisions are made often
     * based on other variables.  The state is divided in
     * a readState half and a writeState half that can be
     * OR:ed together to create the complete state.
     * The halves are locked by their corresponding lock.
     */

    /* +++ Write stuff +++ */
    ErlNifMutex*       writeMtx;
    /**/
    unsigned int       writeState; // For debugging
#ifndef __WIN32__
    /*
     * On *none* Windows:
     * This is intended for the *current* writer.
     * The queue is intended for *waiting* writers.
     *
     * *On* Windows:
     * We let the I/O Completion Ports handle the queue'ing
     * so we do not need to keep track which request is active
     * and which are waiting.
     * We only use the *queue* as a database.
     */
    ESockRequestor     currentWriter;
    ESockRequestor*    currentWriterP; // NULL or &currentWriter
#endif
    ESockRequestQueue  writersQ;
    ESockCounter       writePkgCnt;
    ESockCounter       writePkgMax;
    ESockCounter       writePkgMaxCnt;
    ESockCounter       writeByteCnt;
    ESockCounter       writeTries;
    ESockCounter       writeWaits;
    ESockCounter       writeFails;
#ifdef HAVE_SENDFILE
    HANDLE                 sendfileHandle;
    ESockSendfileCounters* sendfileCountersP;
#endif

    /* +++ Connector +++ */
    ESockRequestor     connector;
    ESockRequestor*    connectorP; // NULL or &connector
    /* +++ Config stuff +++ */
    size_t             wCtrlSz; // Write control buffer size
    ESockMeta          meta;    // Level 'otp' option 'meta' term

    /* +++ Read stuff +++ */
    ErlNifMutex*       readMtx;
    /**/
    unsigned int       readState; // For debugging
#ifndef __WIN32__
    /*
     * On *none* Windows:
     * This is intended for the *current* reader.
     * The queue is intended for *waiting* readers.
     *
     * *On* Windows:
     * We let the I/O Completion Ports handle the queue'ing
     * so we do not need to keep track which request is active
     * and which are waiting.
     * We only use the *queue* as a database.
     */
    ESockRequestor     currentReader;
    ESockRequestor*    currentReaderP; // NULL or &currentReader
    ErlNifBinary       buf;
#endif
    ESockRequestQueue  readersQ;

    ESockCounter       readPkgCnt;
    ESockCounter       readPkgMax;
    ESockCounter       readPkgMaxCnt;
    ESockCounter       readByteCnt;
    ESockCounter       readTries;
    ESockCounter       readWaits;
    ESockCounter       readFails;

    /* +++ Accept stuff +++ */
#ifndef __WIN32__
    /*
     * On *none* Windows:
     * This is intended for the *current* acceptor.
     * The queue is intended for *waiting* acceptors.
     *
     * *On* Windows:
     * We let the I/O Completion Ports handle the queue'ing
     * so we do not need to keep track which request is active
     * and which are waiting.
     * We only use the *queue* as a database.
     */
    ESockRequestor     currentAcceptor;
    ESockRequestor*    currentAcceptorP; // NULL or &currentAcceptor
#endif
    ESockRequestQueue  acceptorsQ;
    ESockCounter       accSuccess;
    ESockCounter       accTries;
    ESockCounter       accWaits;
    ESockCounter       accFails;
    /* +++ Config stuff +++ */
    size_t             rBufSz;  // Read buffer size (when data length = 0)
    /* rNum and rNumCnt are used (together with rBufSz) when calling the recv 
     * function with the Length argument set to 0 (zero).
     * If rNum is 0 (zero), then rNumCnt is not used and only *one* read will
     * be done. Also, when get'ing the value of the option (rcvbuf) with 
     * getopt, the value will be reported as an integer. If the rNum has a 
     * value greater then 0 (zero), then it will instead be reported as
     * {N, BufSz}.
     * On Windows, rNum and rNumCnt is *not* used!
     */
#ifndef __WIN32__
    unsigned int       rNum;    // recv: Number of reads using rBufSz
    unsigned int       rNumCnt; // recv: Current number of reads (so far)
#endif
    size_t             rCtrlSz; // Read control buffer size

    /* Locked by readMtx and writeMtx combined for writing,
     * which means only one of them is required for reading
     */
    /* +++ Close stuff +++ */
    ErlNifPid          closerPid;
    ESockMonitor       closerMon;
    ErlNifEnv*         closeEnv;
    ERL_NIF_TERM       closeRef;
    /* +++ Inform On (counter) Wrap +++ */
    BOOLEAN_T          iow;
    /* +++ Controller (owner) process +++ */
    ErlNifPid          ctrlPid;
    ESockMonitor       ctrlMon;
    /* +++ The actual socket +++ */
    SOCKET             sock;
    SOCKET             origFD; // A 'socket' created from this FD
    BOOLEAN_T          closeOnClose; // Have we dup'ed or not
    BOOLEAN_T          selectRead; // Try to have read select active
    /* +++ The dbg flag for SSDBG +++ */
    BOOLEAN_T          dbg;
    BOOLEAN_T          useReg;

    /* Lock order: readMtx, writeMtx, cntMtx
     */

#if defined(ESOCK_DESCRIPTOR_FILLER)
    char               filler[1024];
#endif

} ESockDescriptor;



/* ======================================================================== *
 *                          What to do about this?                          *
 * ======================================================================== *
 */

extern char* erl_errno_id(int error); /* THIS IS JUST TEMPORARY??? */


/* ======================================================================== *
 *                            Functions                                     *
 * ======================================================================== *
 */

extern ESockDescriptor* esock_alloc_descriptor(SOCKET sock);
extern void esock_dealloc_descriptor(ErlNifEnv*       env,
                                     ESockDescriptor* descP);

extern BOOLEAN_T esock_open_is_debug(ErlNifEnv*   env,
                                     ERL_NIF_TERM eopts,
                                     BOOLEAN_T    def);
extern BOOLEAN_T esock_open_use_registry(ErlNifEnv*   env,
                                         ERL_NIF_TERM eopts,
                                         BOOLEAN_T    def);
extern BOOLEAN_T esock_open_which_protocol(SOCKET sock, int* proto);

extern BOOLEAN_T esock_getopt_int(SOCKET sock,
                                  int    level,
                                  int    opt,
                                  int*   valP);


/* ** Socket Registry functions *** */
extern void esock_send_reg_add_msg(ErlNifEnv*   env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM sockRef);
extern void esock_send_reg_del_msg(ErlNifEnv*   env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM sockRef);


/* *** Message sending functions *** */
extern void esock_send_simple_abort_msg(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ErlNifPid*       pid,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     reason);
extern void esock_send_abort_msg(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ESockRequestor*  reqP,
                                 ERL_NIF_TERM     reason);
extern void esock_send_close_msg(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ErlNifPid*       pid);
extern void esock_send_wrap_msg(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     cnt);


/* ** Monitor functions *** */
extern int esock_monitor(const char*      slogan,
                         ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         const ErlNifPid* pid,
                         ESockMonitor*    mon);
extern int esock_demonitor(const char*      slogan,
                           ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ESockMonitor*    monP);
extern void esock_monitor_init(ESockMonitor* mon);
extern ERL_NIF_TERM esock_make_monitor_term(ErlNifEnv*          env,
                                            const ESockMonitor* monP);
extern BOOLEAN_T esock_monitor_eq(const ESockMonitor* monP,
                                  const ErlNifMonitor* mon);


/* *** Counter functions *** */
extern BOOLEAN_T esock_cnt_inc(ESockCounter* cnt, ESockCounter inc);
extern void      esock_cnt_dec(ESockCounter* cnt, ESockCounter dec);
extern void      esock_inc_socket(int domain, int type, int protocol);
extern void      esock_dec_socket(int domain, int type, int protocol);


/* *** Select functions *** */
extern int esock_select_read(ErlNifEnv*       env,
                             ErlNifEvent      event,
                             void*            obj,
                             const ErlNifPid* pidP,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     selectRef);
extern int esock_select_write(ErlNifEnv*       env,
                              ErlNifEvent      event,
                              void*            obj,
                              const ErlNifPid* pidP,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     selectRef);
extern int esock_select_stop(ErlNifEnv*  env,
                             ErlNifEvent event,
                             void*       obj);
extern int esock_select_cancel(ErlNifEnv*             env,
                               ErlNifEvent            event,
                               enum ErlNifSelectFlags mode,
                               void*                  obj);
extern ERL_NIF_TERM esock_cancel_write_select(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     opRef);
extern ERL_NIF_TERM esock_cancel_read_select(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     opRef);
extern ERL_NIF_TERM esock_cancel_mode_select(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     opRef,
                                             int              smode,
                                             int              rmode);


/* *** Request queue functions *** */
extern void esock_free_request_queue(ESockRequestQueue* q);
extern BOOLEAN_T esock_requestor_pop(ESockRequestQueue* q,
                                     ESockRequestor*    reqP);

extern void esock_requestor_init(ESockRequestor* reqP);
extern void esock_requestor_release(const char*      slogan,
                                    ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ESockRequestor*  reqP);


/* *** esock_activate_next_acceptor ***
 * *** esock_activate_next_writer   ***
 * *** esock_activate_next_reader   ***
 *
 * All the activate-next functions for acceptor, writer and reader
 * have exactly the same API, so we apply some macro magic to simplify.
 * They simply operates on dufferent data structures.
 *
 */

#define ACTIVATE_NEXT_FUNCS_DEFS     \
    ACTIVATE_NEXT_FUNC_DEF(acceptor) \
    ACTIVATE_NEXT_FUNC_DEF(writer)   \
    ACTIVATE_NEXT_FUNC_DEF(reader)

#define ACTIVATE_NEXT_FUNC_DEF(F)                                       \
    extern BOOLEAN_T esock_activate_next_##F(ErlNifEnv*       env,      \
                                             ESockDescriptor* descP,    \
                                             ERL_NIF_TERM     sockRef);
ACTIVATE_NEXT_FUNCS_DEFS
#undef ACTIVATE_NEXT_FUNC_DEF

/* esock_acceptor_search4pid | esock_writer_search4pid | esock_reader_search4pid
 * esock_acceptor_push       | esock_writer_push       | esock_reader_push
 * esock_acceptor_pop        | esock_writer_pop        | esock_reader_pop
 * esock_acceptor_unqueue    | esock_writer_unqueue    | esock_reader_unqueue
 *
 * All the queue operator functions (search4pid, push, pop
 * and unqueue) for acceptor, writer and reader has exactly
 * the same API, so we apply some macro magic to simplify.
 */

#define ESOCK_OPERATOR_FUNCS_DEFS      \
    ESOCK_OPERATOR_FUNCS_DEF(acceptor) \
    ESOCK_OPERATOR_FUNCS_DEF(writer)   \
    ESOCK_OPERATOR_FUNCS_DEF(reader)

#define ESOCK_OPERATOR_FUNCS_DEF(O)                                    \
    extern BOOLEAN_T esock_##O##_search4pid(ErlNifEnv*       env,      \
                                            ESockDescriptor* descP,    \
                                            ErlNifPid*       pid);     \
    extern void esock_##O##_push(ErlNifEnv*       env,                 \
                                 ESockDescriptor* descP,               \
                                 ErlNifPid        pid,                 \
                                 ERL_NIF_TERM     ref,                 \
                                 void*            dataP);              \
    extern BOOLEAN_T esock_##O##_pop(ErlNifEnv*       env,     \
                                     ESockDescriptor* descP,   \
                                     ESockRequestor*  reqP);   \
    extern BOOLEAN_T esock_##O##_unqueue(ErlNifEnv*       env,          \
                                         ESockDescriptor* descP,        \
                                         ERL_NIF_TERM*    refP,         \
                                         const ErlNifPid* pidP);
ESOCK_OPERATOR_FUNCS_DEFS
#undef ESOCK_OPERATOR_FUNCS_DEF


/* *** Environment wrapper functions ***
 * These hould really be inline, but for now...
 */
extern void       esock_clear_env(const char* slogan, ErlNifEnv* env);
extern void       esock_free_env(const char* slogan, ErlNifEnv* env);
extern ErlNifEnv* esock_alloc_env(const char* slogan);


/* *** Control Message utility functions ***
 */
#ifndef __WIN32__
extern void* esock_init_cmsghdr(struct cmsghdr* cmsgP,
                                size_t          rem,
                                size_t          size,
                                size_t*         usedP);
#if defined(IP_TTL) || \
    defined(IPV6_HOPLIMIT) || \
    defined(IPV6_TCLASS) || defined(IPV6_RECVTCLASS)
extern BOOLEAN_T esock_cmsg_decode_int(ErlNifEnv*      env,
                                       ERL_NIF_TERM    eValue,
                                       struct cmsghdr* cmsgP,
                                       size_t          rem,
                                       size_t*         usedP);
#endif
extern BOOLEAN_T esock_cmsg_decode_bool(ErlNifEnv*      env,
                                        ERL_NIF_TERM    eValue,
                                        struct cmsghdr* cmsgP,
                                        size_t          rem,
                                        size_t*         usedP);
extern ESockCmsgSpec* esock_lookup_cmsg_table(int level, size_t *num);
extern ESockCmsgSpec* esock_lookup_cmsg_spec(ESockCmsgSpec* table,
                                             size_t         num,
                                             ERL_NIF_TERM   eType);

extern BOOLEAN_T esock_encode_cmsg(ErlNifEnv*     env,
                                   int            level,
                                   int            type,
                                   unsigned char* dataP,
                                   size_t         dataLen,
                                   ERL_NIF_TERM*  eType,
                                   ERL_NIF_TERM*  eData);
extern void esock_encode_msg_flags(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              msgFlags,
                                   ERL_NIF_TERM*    flags);
#endif


extern void esock_stop_handle_current(ErlNifEnv*       env,
                                      const char*      role,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ESockRequestor*  reqP);
extern void esock_inform_waiting_procs(ErlNifEnv*         env,
                                       const char*        role,
                                       ESockDescriptor*   descP,
                                       ERL_NIF_TERM       sockRef,
                                       ESockRequestQueue* q,
                                       ERL_NIF_TERM       reason);


/* *** Control Message 'stuff' ***
 */
extern void* esock_init_cmsghdr(struct cmsghdr* cmsgP,
                                size_t          rem,  // Remaining space
                                size_t          size, // Size of data
                                size_t*         usedP);
extern ESockCmsgSpec* esock_lookup_cmsg_table(int level, size_t *num);
extern ESockCmsgSpec* esock_lookup_cmsg_spec(ESockCmsgSpec* table,
                                             size_t         num,
                                             ERL_NIF_TERM   eType);



/* *** Sendfile 'stuff' ***
 */
#ifdef HAVE_SENDFILE

extern ESockSendfileCounters initESockSendfileCounters;

#endif

/* *** message functions ****
 */
extern void esock_send_wrap_msg(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     cnt);
extern BOOLEAN_T esock_send_msg(ErlNifEnv*   env,
                                ErlNifPid*   pid,
                                ERL_NIF_TERM msg,
                                ErlNifEnv*   msgEnv);
extern ERL_NIF_TERM esock_mk_socket_msg(ErlNifEnv*   env,
                                        ERL_NIF_TERM sockRef,
                                        ERL_NIF_TERM tag,
                                        ERL_NIF_TERM info);
extern ERL_NIF_TERM esock_mk_socket(ErlNifEnv*   env,
                                    ERL_NIF_TERM sockRef);
#ifdef HAVE_SENDFILE
extern void esock_send_sendfile_deferred_close_msg(ErlNifEnv*       env,
                                                   ESockDescriptor* descP);
#endif


/* *** 'close' functions ***
 */
extern int esock_close_socket(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              BOOLEAN_T        unlock);


/* *** 'ioctl' functions ***
 */
extern ERL_NIF_TERM esock_encode_ioctl_ivalue(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              ivalue);
extern ERL_NIF_TERM esock_encode_ioctl_bvalue(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              bvalue);

#endif // PRIM_SOCKET_INT_H__
