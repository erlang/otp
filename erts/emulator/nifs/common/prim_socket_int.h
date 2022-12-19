/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2022. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"

#include <erl_nif.h>

#include "socket_int.h"


/* ********************************************************************* *
 *                              SOCKET and HANDLE                        *
 * ********************************************************************* *
 */

#if defined(__WIN32__)

#define INVALID_EVENT NULL

#else

#define INVALID_HANDLE (-1)
typedef int HANDLE;
#define INVALID_SOCKET (-1)
typedef int SOCKET; /* A subset of HANDLE */
#define INVALID_EVENT INVALID_HANDLE

#endif


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
    ESockRequestor     currentWriter;
    ESockRequestor*    currentWriterP; // NULL or &currentWriter
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
    ESockRequestor     currentReader;
    ESockRequestor*    currentReaderP; // NULL or &currentReader
    ESockRequestQueue  readersQ;
    ErlNifBinary       rbuffer;      // DO WE NEED THIS
    Uint32             readCapacity; // DO WE NEED THIS
    ESockCounter       readPkgCnt;
    ESockCounter       readPkgMax;
    ESockCounter       readPkgMaxCnt;
    ESockCounter       readByteCnt;
    ESockCounter       readTries;
    ESockCounter       readWaits;
    ESockCounter       readFails;
    /* +++ Accept stuff +++ */
    ESockRequestor     currentAcceptor;
    ESockRequestor*    currentAcceptorP; // NULL or &currentAcceptor
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
     * value greater then 0 (zero), then it will instead be reported as {N, BufSz}.
     */
    unsigned int       rNum;    // recv: Number of reads using rBufSz
    unsigned int       rNumCnt; // recv: Current number of reads (so far)
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
    ErlNifEvent        event;
    SOCKET             origFD; // A 'socket' created from this FD
    BOOLEAN_T          closeOnClose; // Have we dup'ed or not
    /* +++ The dbg flag for SSDBG +++ */
    BOOLEAN_T          dbg;
    BOOLEAN_T          useReg;

    /* Lock order: readMtx, writeMtx, cntMtx
     */
} ESockDescriptor;


#endif // PRIM_SOCKET_INT_H__
