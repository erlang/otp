/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
 *  Purpose : Windows version of the asyncronous I/O backend.
 * ----------------------------------------------------------------------
 *
 * Misc:
 * This is based of the Windows concept: I/O Completion Port.
 * This feature works on "all" kinds of I/O, even file I/O. But,
 * our implementation only deals with socket I/O.
 * The "asynchronous" access functions (that "can" block) that we
 * use are as following:
 *
 *   * WSASend, WSASendTo, WSASendMsg, WSARecv, WSARecvFrom, WSARecvMsg
 *   * AccepxEx
 *     (this is actually a function pointer, obtained at run time by
 *      making a call to the WSAIoctl function with the
 *      SIO_GET_EXTENSION_FUNCTION_POINTER opcode specified.
 *      The input buffer passed to the WSAIoctl function must contain
 *      WSAID_ACCEPTEX).
 *      To get the local and remote addresses, the GetAcceptExSockaddrs
 *      must be called. This function is *also* a function pointer
 *      obtained at run time by making a call to the WSAIoctl function.
 *   * ConnectEx:
 *     (this is actually a function pointer, obtained at run time by
 *      The function pointer for the ConnectEx function must be
 *      making a call to the WSAIoctl function with the
 *      SIO_GET_EXTENSION_FUNCTION_POINTER opcode specified.
 *      The input buffer passed to the WSAIoctl function must contain
 *      WSAID_CONNECTEX).
 *   * WSASendMsg & WSARecvMsg are actually *also* function pointers!!
 *
 * But since we want them to "behave" the same way, we need to add 
 * some wrapper code to simulate the "completion behaviour".
 *
 * These functions (in erlang) should return simething *like* this:
 *
 *    ok | completion | {error, Reason}
 *
 * And if the return value was 'completion', the caller shall expect
 * the following message, when the "operation" has "completed" (success
 * or failure):
 * 
 *    {'$socket', Socket, completion, {Ref, CompletionStatus}}
 *
 * Where 'Socket' is the socket on which the call was made (for example,
 * 'socket:send(Socket, ...)), and CompletionStatus is the result of
 * actual operation: ok | {error, Reason}
 *
 * Examples:
 * * https://www.winsocketdotnetworkprogramming.com/winsock2programming/winsock2advancediomethod5i.html
 * * https://www.codeproject.com/Articles/13382/A-simple-application-using-I-O-Completion-Ports-an
 * * https://www.winsocketdotnetworkprogramming.com/winsock2programming/winsock2advancedscalableapp6b.html
 *
 * More useful links:
 * * https://learn.microsoft.com/en-us/windows/win32/api/mswsock/nf-mswsock-acceptex
 * * https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsaioctl
 * * https://learn.microsoft.com/en-us/windows/win32/winsock/socket-options-and-ioctls-2
 *
 * Note:
 * -
 */


#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#ifdef ESOCK_ENABLE

// #include <Ws2def.h>
// #include <winsock2.h>
// #include <windows.h>
#include <ws2tcpip.h>
#include <mswsock.h>
#include <stdio.h>

#include <sys.h>

#include "socket_int.h"
#include "socket_io.h"
#include "socket_asyncio.h"
#include "socket_util.h"
#include "socket_tarray.h"
#include "socket_dbg.h"


/* =================================================================== *
 *                                                                     *
 *                          Local Constants                            *
 *                                                                     *
 * =================================================================== */

#define ESAIO_OK                     ESOCK_IO_OK
#define ESAIO_ERR_WINSOCK_INIT       0x0001
#define ESAIO_ERR_IOCPORT_CREATE     0x0002
#define ESAIO_ERR_FSOCK_CREATE       0x0003
#define ESAIO_ERR_IOCTL_ACCEPT_GET   0x0004
#define ESAIO_ERR_IOCTL_CONNECT_GET  0x0005
#define ESAIO_ERR_IOCTL_SENDMSG_GET  0x0006
#define ESAIO_ERR_IOCTL_RECVMSG_GET  0x0007
#define ESAIO_ERR_THREAD_OPTS_CREATE 0x0011
#define ESAIO_ERR_THREAD_CREATE      0x0012

#define ERRNO_BLOCK                  WSAEWOULDBLOCK

#define ESAIO_RECVFROM_MIN_BUFSZ     0x8000


/* ======================================================================== *
 *                               Socket wrappers                            *
 * ======================================================================== *
 */

#define sock_accept_O(s, as, b, al, rb, o)              \
    ctrl.accept((s), (as), (b), 0, (al), (al), (rb), (o))
#define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_close(s)                   closesocket((s))
#define sock_connect(s, a, al)          connect((s), (a), (al))
#define sock_connect_O(s, a, al, sent, o)                               \
    ctrl.connect((s), (struct sockaddr*) (a), (al), NULL, 0, (sent), (o))
#define sock_errno()                    WSAGetLastError()
#define sock_ioctl1(s, cc, b)                    \
    ioctlsocket((s), (cc), (b))
#define sock_ioctl2(s, cc, ib, ibs, ob, obs, br) \
    WSAIoctl((s), (cc), (ib), (ibs), (ob), (obs), (br), NULL, NULL)
// #define sock_listen(s, b)               listen((s), (b))
// #define sock_name(s, addr, len)        getsockname((s), (addr), (len))
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_open_O(domain, type, proto) \
    WSASocket((domain), (type), (proto), NULL, 0, WSA_FLAG_OVERLAPPED)
#define sock_recv_O(s,buf,flag,ol)                      \
    WSARecv((s), (buf), 1, NULL, (flag), (ol), NULL)
#define sock_recvfrom_O(s,buf,flag,fa,fal,ol)                           \
    WSARecvFrom((s), (buf), 1, NULL, (flag), (fa), (fal), (ol), NULL)
#define sock_recvmsg_O(s,msg,o)     \
    ctrl.recvmsg((s), (msg), NULL, (o), NULL)
#define sock_send_O(s,buf,flag,o)                       \
    WSASend((s), (buf), 1, NULL, (flag), (o), NULL)
/* #define sock_sendmsg_O(s,buf,flag,ol)                   \ */
/*     WSASendMsg((s), (buf), (flag), NULL, (ol), NULL) */
#define sock_sendmsg_O(s,buf,flag,ol)                   \
    ctrl.sendmsg((s), (buf), (flag), NULL, (ol), NULL)
#define sock_sendto_O(s,buf,flag,ta,tal,o)              \
    WSASendTo((s), (buf), 1, NULL, (flag), (ta), (tal), (o), NULL)
#define sock_sendv_O(s,iov,iovcnt,o)                  \
    WSASend((s), (iov), iovcnt, NULL, 0, (o), NULL)
#define sock_setopt(s,l,o,v,ln)        setsockopt((s),(l),(o),(v),(ln))


#define ESAIO_UPDATE_ACCEPT_CONTEXT(AS, LS)                 \
    sock_setopt( (AS), SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, \
                 (char*) &(LS), sizeof( (LS) ))
#define ESAIO_UPDATE_CONNECT_CONTEXT(S)                                 \
    sock_setopt((S), SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT, NULL, 0)


#define ESOCK_CMSG_FIRSTHDR(M) WSA_CMSG_FIRSTHDR((M))
#define ESOCK_CMSG_NXTHDR(M,C) WSA_CMSG_NXTHDR((M), (C))
#define ESOCK_CMSG_DATA(C)     WSA_CMSG_DATA((C))


/* =================================================================== *
 *                                                                     *
 *                            Local types                              *
 *                                                                     *
 * =================================================================== */

typedef struct {
    Uint16     id;      /* Thread id: mainly used for debugging,
                         * and name creation */

    /* Thread state(s) */
#define ESAIO_THREAD_STATE_UNDEF        0xFFFF
#define ESAIO_THREAD_STATE_INITIATING   0x0000
#define ESAIO_THREAD_STATE_OPERATIONAL  0x0001
#define ESAIO_THREAD_STATE_TERMINATING  0x0002
#define ESAIO_THREAD_STATE_TERMINATED   0x0003

    Uint16     state;   /* State of the thread:
                         * undefined, initiating, operational, terminating */

    /* Thread error 'state(s)'.
     * If the thread is "not running", this value tells why.
     */
#define ESAIO_THREAD_ERROR_UNDEF        0xFFFF
#define ESAIO_THREAD_ERROR_OK           0x0000
#define ESAIO_THREAD_ERROR_TOCREATE     0x0001
#define ESAIO_THREAD_ERROR_TCREATE      0x0002
#define ESAIO_THREAD_ERROR_GET          0x0003
#define ESAIO_THREAD_ERROR_CMD          0x0004
    
    Uint32       error; /* In case the thread exits, 
                         * this is where the (error) reason is stored.
                         * Not sure i f we would ever be able to
                         * read this (if things are bad enough that the
                         * threads terminate)...
                         */

    /* Do we need this?
     * "The environment of the calling thread (process bound or
     *  callback environment) or NULL if calling from a custom
     *  thread not spawned by ERTS."
     */
    ErlNifEnv*   env;   /* Used when sending messages */

#define ESAIO_THREAD_CNT_MAX 0xFFFFFFFF

    Uint32       cnt;   /* Run-counter: mainly used for debugging */

    unsigned int latest; /* Latest request (tag) */
} ESAIOThreadData;

typedef struct {
    ErlNifThreadOpts* optsP;
    ErlNifTid         tid;
    ESAIOThreadData   data;
} ESAIOThread;

typedef struct {
    WSADATA         wsaData;
    HANDLE          cport;

    /* Used during initiation
     * for extracting AcceptEx and ConnectEx
     */
    SOCKET          srvInit;

    /* Extension functions */
    LPFN_ACCEPTEX   accept;
    LPFN_CONNECTEX  connect;
    LPFN_WSASENDMSG sendmsg;
    LPFN_WSARECVMSG recvmsg;

    /* Thread pool stuff.
     * The size of the pool is configurable. */
    DWORD           numThreads;
    ESAIOThread*    threads;

    /* Misc stuff */
    BOOLEAN_T       dbg;
    BOOLEAN_T       sockDbg;

    /* Counter stuff */
    ErlNifMutex*    cntMtx;
    ESockCounter    unexpectedConnects;
    ESockCounter    unexpectedAccepts;
    ESockCounter    unexpectedWrites;
    ESockCounter    unexpectedReads;
    ESockCounter    genErrs;
    ESockCounter    unknownCmds;

} ESAIOControl;


typedef struct __ESAIOOpDataAccept {
    /* AcceptEx; ; lookup with WSAID_ACCEPTEX */

    /* The socket, sock, is created empty and then provided as an
     * argumented to AcceptEx (together with the listen socket
     * and the other arguments).
     * When AcceptEx has completed successfully, the socket, s, is
     * usable.
     * But in order for the functions sockname and peername to work,
     * the SO_UPDATE_ACCEPT_CONTEXT option must be set on the
     * accepted socket, sock. */
    SOCKET       lsock;    /* The listen socket */
    SOCKET       asock;    /* The "accepted" socket.
                            * This is created "in advance"
                            * and then sent to AcceptEx as an argument.
                            */
    char*        buf;      /* Size depends on domain.
                            * This is used for 'initial data', 
                            * 'local address' and 'remote address'.
                            * We use neither of these, but the 
                            * AcceptEx function requires this argument!
                            */
    ERL_NIF_TERM lSockRef; /* The listen socket */
    ERL_NIF_TERM accRef;   /* The (unique) reference (ID) of the accept */
} ESAIOOpDataAccept;

typedef struct __ESAIOOpDataConnect {
    /* ConnectEx; ; lookup with WSAID_CONNECTEX */

    /* When ConnectEx has completed successfully, 
     * the socket is usable.
     * *But*, in order for the functions sockname and peername to work,
     * the SO_UPDATE_CONNECT_CONTEXT option must be set on the socket.
     */
    ERL_NIF_TERM sockRef; /* The socket */
    ERL_NIF_TERM connRef; /* The (unique) reference (ID)
                            * of the connect request */
} ESAIOOpDataConnect;

typedef struct __ESAIOOpDataSend {
    /* WSASend */
    WSABUF        wbuf;    /* During ongoing sending, this buffer cannot
                            * be de-allocated. */
    ERL_NIF_TERM  sockRef; /* The socket */
    ERL_NIF_TERM  sendRef; /* The (unique) reference (ID)
                            * of the send request */
} ESAIOOpDataSend;

typedef struct __ESAIOOpDataSendTo {
    /* WSASendTo */
    WSABUF       wbuf;     /* During ongoing sending, this buffer cannot
                            * be de-allocated. */

    /* Do we actually need these (remote address)?
     * Debugging/logging?
     */
    ESockAddress remoteAddr;
    SOCKLEN_T    remoteAddrLen;

    ERL_NIF_TERM sockRef; /* The socket */
    ERL_NIF_TERM sendRef; /* The (unique) reference (ID)
                           * of the send request */
} ESAIOOpDataSendTo;

typedef struct __ESAIOOpDataSendMsg {
    /* WSASendMsg; lookup with WSAID_WSASENDMSG */
    WSAMSG       msg;
    ErlNifIOVec* iovec;
    char*        ctrlBuf;
    ESockAddress addr;
    ERL_NIF_TERM sockRef; /* The socket */
    ERL_NIF_TERM sendRef; /* The (unique) reference (ID)
                           * of the send request */
} ESAIOOpDataSendMsg;

typedef struct __ESAIOOpDataSendv {
    /* WSASend (used with an io-vector) */
    ErlNifIOVec*  iovec;

    ERL_NIF_TERM  sockRef; /* The socket */
    ERL_NIF_TERM  sendRef; /* The (unique) reference (ID)
                            * of the send request */
} ESAIOOpDataSendv;

typedef struct __ESAIOOpDataRecv {
    /* WSARecv */
    DWORD         toRead;  /* Can be 0 (= zero)
                            * "just to indicate: give me what you got"
                            */
    ErlNifBinary buf;
    ERL_NIF_TERM sockRef; /* The socket */
    ERL_NIF_TERM recvRef; /* The (unique) reference (ID)
                           * of the recv request */
} ESAIOOpDataRecv;

typedef struct __ESAIOOpDataRecvFrom {
    /* WSARecvFrom */
    DWORD         toRead;  /* Can be 0 (= zero)
                            * "just to indicate: give me what you got"
                            */
    ErlNifBinary  buf;

    ESockAddress  fromAddr;
    INT           addrLen; // SOCKLEN_T

    ERL_NIF_TERM  sockRef; /* The socket */
    ERL_NIF_TERM  recvRef; /* The (unique) reference (ID)
                            * of the recv request */
} ESAIOOpDataRecvFrom;

typedef struct __ESAIOOpDataRecvMsg {
    /* WSARecvMsg; lookup with WSAID_WSARECVMSG */

    /* If we used I/O - vectors of different size(s),
     * we would (maybe) need to malloc them, and simply
     * have a pointer here.
     */

    WSAMSG        msg;
    WSABUF        wbufs[1];
    ErlNifBinary  data[1];
    ErlNifBinary  ctrl;
    ESockAddress  addr;

    ERL_NIF_TERM  sockRef; /* The socket */
    ERL_NIF_TERM  recvRef; /* The (unique) reference (ID)
                            * of the recv request */
} ESAIOOpDataRecvMsg;

/* An 'operation', recv/recvfrom/recvmsg and send/sendto/sendmsg,
 * accept or connect, is 'encoded' into this structure, which is
 * "passed around".
 */
typedef struct __ESAIOOperation {
    /* Has to be first and is *only* used by I/O Completion Port framework */
    WSAOVERLAPPED        ol;

    /* *** Commands (=tags) *** */
#define ESAIO_OP_NONE         0x0000  // None
    /* "system" commands */
#define ESAIO_OP_TERMINATE    0x0001  // Terminate
#define ESAIO_OP_DEBUG        0x0002  // Change debug level for thread(s)
    /* Commands for establishing connections; connect and accept */
#define ESAIO_OP_CONNECT      0x0011  // ConnectEx (function pointer)
#define ESAIO_OP_ACCEPT       0x0012  // AcceptEx (function pointer)
    /* Commands for sending */
#define ESAIO_OP_SEND         0x0021  // WSASend
#define ESAIO_OP_SENDTO       0x0022  // WSASendTo
#define ESAIO_OP_SENDMSG      0x0023  // WSASendMsg
#define ESAIO_OP_SENDV        0x0024  // WSASend (with vector)
    /* Commands for receiving */
#define ESAIO_OP_RECV         0x0031  // WSARecv
#define ESAIO_OP_RECVFROM     0x0032  // WSARecvFrom
#define ESAIO_OP_RECVMSG      0x0033  // WSARecvMsg

    unsigned int          tag;    /* The 'tag' of the operation */

    ErlNifPid             caller; /* *Almost* every request (not connect)
                                   * operations require a caller */
    ErlNifEnv*            env;    /* *Almost* every request
                                   * needs an environment */

    /* Generic "data" field.
     * This is different for each 'operation'!
     * Also; not all opererations have this!
     */

    union {
        /* +++ accept +++ */
        ESAIOOpDataAccept accept;

         /* +++ connect +++ */
        ESAIOOpDataConnect connect;

        /* +++ send +++ */
        ESAIOOpDataSend send;

        /* +++ sendto +++ */
        ESAIOOpDataSendTo sendto;

        /* +++ sendmsg +++ */
        ESAIOOpDataSendMsg sendmsg;

        /* +++ sendv +++ */
        ESAIOOpDataSendv sendv;

        /* +++ recv +++ */
        ESAIOOpDataRecv recv;

        /* +++ recvfrom +++ */
        ESAIOOpDataRecvFrom recvfrom;

        /* +++ recvmsg +++ */
        ESAIOOpDataRecvMsg recvmsg;

    } data;

} ESAIOOperation;



/* =================================================================== *
 *                                                                     *
 *                        Function Forwards                            *
 *                                                                     *
 * =================================================================== */

static
BOOLEAN_T init_srv_init_socket(int* savedErrno);

static ERL_NIF_TERM esaio_connect_stream(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     connRef,
                                         ESockAddress*    addrP,
                                         SOCKLEN_T        addrLen);
static ERL_NIF_TERM connect_stream_check_result(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ESAIOOperation*  opP,
                                                BOOL             cres);
static ERL_NIF_TERM esaio_connect_dgram(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     connRef,
                                        ESockAddress*    addrP,
                                        SOCKLEN_T        addrLen);

static ERL_NIF_TERM accept_check_result(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ESAIOOperation*  opP,
                                        BOOL             ares,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     accRef,
                                        SOCKET           accSock,
                                        ErlNifPid        caller);
static ERL_NIF_TERM accept_check_pending(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ESAIOOperation*  opP,
                                         ErlNifPid        caller,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     accRef);
static ERL_NIF_TERM accept_check_fail(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ESAIOOperation*  opP,
                                      int              saveErrno,
                                      SOCKET           accSock,
                                      ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM esaio_accept_accepted(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ErlNifPid        pid,
                                          ERL_NIF_TERM     sockRef,
                                          SOCKET           accSock);

static ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ESAIOOperation*  opP,
                                      ErlNifPid        caller,
                                      int              send_result,
                                      ssize_t          dataSize,
                                      BOOLEAN_T        dataInTail,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     sendRef,
                                      BOOLEAN_T*       cleanup);
static ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  DWORD            written,
                                  ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM send_check_pending(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       ErlNifPid        caller,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     sendRef);
static ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              saveErrno,
                                    ERL_NIF_TERM     sockRef);

static BOOLEAN_T init_sendmsg_sockaddr(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eMsg,
                                       WSAMSG*          msgP,
                                       ESockAddress*    addrP);
static BOOLEAN_T verify_sendmsg_iovec_size(const ESockData* dataP,
                                           ESockDescriptor* descP,
                                           ErlNifIOVec*     iovec);
static BOOLEAN_T verify_sendmsg_iovec_tail(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM*    tail);
static BOOLEAN_T check_sendmsg_iovec_overflow(ESockDescriptor* descP,
                                              ErlNifIOVec*     iovec,
                                              ssize_t*         dataSize);
static BOOLEAN_T decode_cmsghdrs(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     eCMsg,
                                 char*            cmsgHdrBufP,
                                 size_t           cmsgHdrBufLen,
                                 size_t*          cmsgHdrBufUsed);
static BOOLEAN_T decode_cmsghdr(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eCMsg,
                                char*            bufP,
                                size_t           rem,
                                size_t*          used);
static BOOLEAN_T decode_cmsghdr_value(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      ERL_NIF_TERM     eType,
                                      ERL_NIF_TERM     eValue,
                                      char*            dataP,
                                      size_t           dataLen,
                                      size_t*          dataUsedP);
static BOOLEAN_T decode_cmsghdr_data(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     ERL_NIF_TERM     eType,
                                     ERL_NIF_TERM     eData,
                                     char*            dataP,
                                     size_t           dataLen,
                                     size_t*          dataUsedP);
static void encode_msg(ErlNifEnv*       env,
                       ESockDescriptor* descP,
                       ssize_t          read,
                       WSAMSG*          msgP,
                       ErlNifBinary*    dataBufP,
                       ErlNifBinary*    ctrlBufP,
                       ERL_NIF_TERM*    eMsg);
static void encode_cmsgs(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ErlNifBinary*    cmsgBinP,
                         WSAMSG*          msgP,
                         ERL_NIF_TERM*    eCMsg);
static ERL_NIF_TERM recv_check_ok(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ESAIOOperation*  opP,
                                  ssize_t          toRead,
                                  ErlNifPid        caller,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef);

static ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ssize_t          toRead,
                                      ESAIOOperation*  opP,
                                      ErlNifPid        caller,
                                      int              recv_result,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_pending(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       ErlNifPid        caller,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ESAIOOperation*  opP,
                                    int              saveErrno,
                                    ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recv_check_failure(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       int              saveErrno,
                                       ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ESAIOOperation*  opP,
                                          ErlNifPid        caller,
                                          int              recv_result,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvfrom_check_ok(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ESAIOOperation*  opP,
                                      ErlNifPid        caller,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvfrom_check_fail(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ESAIOOperation*  opP,
                                        int              saveErrno,
                                        ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recvmsg_check_result(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ESAIOOperation*  opP,
                                         ErlNifPid        caller,
                                         int              recv_result,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvmsg_check_ok(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ESAIOOperation*  opP,
                                     ErlNifPid        caller,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvmsg_check_fail(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       int              saveErrno,
                                       ERL_NIF_TERM     sockRef);

#if defined(FIONREAD)
static ERL_NIF_TERM esaio_ioctl_fionread(ErlNifEnv*       env,
                                         ESockDescriptor* descP);
#endif
#if defined(SIOCATMARK)
static ERL_NIF_TERM esaio_ioctl_siocatmark(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif

#if defined(SIO_TCP_INFO)
static ERL_NIF_TERM esaio_ioctl_tcp_info(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eversion);
static ERL_NIF_TERM encode_tcp_info_v0(ErlNifEnv*   env,
                                       TCP_INFO_v0* infoP);
#if defined(HAVE_TCP_INFO_V1)
static ERL_NIF_TERM encode_tcp_info_v1(ErlNifEnv*   env,
                                       TCP_INFO_v1* infoP);
#endif
static ERL_NIF_TERM encode_tcp_state(ErlNifEnv* env,
                                     TCPSTATE   state);
#endif

#if defined(SIO_RCVALL)
static ERL_NIF_TERM esaio_ioctl_rcvall(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     evalue);
#endif

#if defined(SIO_RCVALL_IGMPMCAST)
static ERL_NIF_TERM esaio_ioctl_rcvall_igmpmcast(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     evalue);
#endif

#if defined(SIO_RCVALL_MCAST)
static ERL_NIF_TERM esaio_ioctl_rcvall_mcast(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     evalue);
#endif


static void* esaio_completion_main(void* threadDataP);
static BOOLEAN_T esaio_completion_terminate(ESAIOThreadData* dataP,
                                            OVERLAPPED*      ovl);
static BOOLEAN_T esaio_completion_unknown(ESAIOThreadData* dataP,
                                          ESockDescriptor* descP,
                                          OVERLAPPED*      ovl,
                                          DWORD            numBytes,
                                          int              error);
static void esaio_completion_fail(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  const char*      opStr,
                                  int              error,
                                  BOOLEAN_T        inform);

static BOOLEAN_T esaio_completion_connect(ESAIOThreadData*    dataP,
                                          ESockDescriptor*    descP,
                                          OVERLAPPED*         ovl,
                                          ErlNifEnv*          opEnv,
                                          ErlNifPid*          opCaller,
                                          ESAIOOpDataConnect* opDataP,
                                          int                 error);
static void esaio_completion_connect_success(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ESAIOOpDataConnect* opDataP);
static void esaio_completion_connect_aborted(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ESAIOOpDataConnect* opDataP);
static void esaio_completion_connect_failure(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ESAIOOpDataConnect* opDataP,
                                             int                 error);
static void esaio_completion_connect_completed(ErlNifEnv*          env,
                                               ESockDescriptor*    descP,
                                               ESAIOOpDataConnect* opDataPP);
static void esaio_completion_connect_not_active(ESockDescriptor* descP);
static void esaio_completion_connect_fail(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              error,
                                          BOOLEAN_T        inform);

static BOOLEAN_T esaio_completion_accept(ESAIOThreadData*   dataP,
                                         ESockDescriptor*   descP,
                                         OVERLAPPED*        ovl,
                                         ErlNifEnv*         opEnv,
                                         ErlNifPid*         opCaller,
                                         ESAIOOpDataAccept* opDataP,
                                         int                error);
static void esaio_completion_accept_success(ErlNifEnv*         env,
                                            ESockDescriptor*   descP,
                                            ErlNifEnv*         opEnv,
                                            ErlNifPid*         opCaller,
                                            ESAIOOpDataAccept* opDataP);
static void esaio_completion_accept_aborted(ErlNifEnv*         env,
                                            ESockDescriptor*   descP,
                                            ErlNifPid*         opCaller,
                                            ESAIOOpDataAccept* opDataP);
static void esaio_completion_accept_failure(ErlNifEnv*         env,
                                            ESockDescriptor*   descP,
                                            ErlNifPid*         opCaller,
                                            ESAIOOpDataAccept* opDataP,
                                            int                error);
static void esaio_completion_accept_completed(ErlNifEnv*         env,
                                              ESockDescriptor*   descP,
                                              ErlNifEnv*         opEnv,
                                              ErlNifPid*         opCaller,
                                              ESAIOOpDataAccept* opDataP,
                                              ESockRequestor*    reqP);
static void esaio_completion_accept_not_active(ESockDescriptor* descP);
static void esaio_completion_accept_fail(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              error,
                                         BOOLEAN_T        inform);

static BOOLEAN_T esaio_completion_send(ESAIOThreadData* dataP,
                                       ESockDescriptor* descP,
                                       OVERLAPPED*      ovl,
                                       ErlNifEnv*       opEnv,
                                       ErlNifPid*       opCaller,
                                       ESAIOOpDataSend* opDataP,
                                       int              error);
static void esaio_completion_send_success(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          OVERLAPPED*      ovl,
                                          ErlNifEnv*       opEnv,
                                          ErlNifPid*       opCaller,
                                          ESAIOOpDataSend* opDataP);
static void esaio_completion_send_aborted(ErlNifEnv*         env,
                                          ESockDescriptor* descP,
                                          ErlNifPid*       opCaller,
                                          ESAIOOpDataSend* opDataP);
static void esaio_completion_send_failure(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ErlNifPid*       opCaller,
                                          ESAIOOpDataSend* opDataP,
                                          int              error);
static void esaio_completion_send_completed(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            OVERLAPPED*      ovl,
                                            ErlNifEnv*       opEnv,
                                            ErlNifPid*       sender,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     sendRef,
                                            DWORD            toWrite,
                                            ESockRequestor*  reqP);
static ERL_NIF_TERM esaio_completion_send_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     sockRef,
                                               DWORD            written);
static ERL_NIF_TERM esaio_completion_send_partial(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  DWORD            written);
static void esaio_completion_send_fail(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              error,
                                       BOOLEAN_T        inform);
static void esaio_completion_send_not_active(ESockDescriptor* descP);
static BOOLEAN_T esaio_completion_sendto(ESAIOThreadData*   dataP,
                                         ESockDescriptor*   descP,
                                         OVERLAPPED*        ovl,
                                         ErlNifEnv*         opEnv,
                                         ErlNifPid*         opCaller,
                                         ESAIOOpDataSendTo* opDataP,
                                         int                error);
static void esaio_completion_sendto_success(ErlNifEnv*         env,
                                            ESockDescriptor*   descP,
                                            OVERLAPPED*        ovl,
                                            ErlNifEnv*         opEnv,
                                            ErlNifPid*         opCaller,
                                            ESAIOOpDataSendTo* opDataP);
static void esaio_completion_sendto_aborted(ErlNifEnv*         env,
                                            ESockDescriptor*   descP,
                                            ErlNifPid*         opCaller,
                                            ESAIOOpDataSendTo* opDataP);
static void esaio_completion_sendto_failure(ErlNifEnv*         env,
                                            ESockDescriptor*   descP,
                                            ErlNifPid*         opCaller,
                                            ESAIOOpDataSendTo* opDataP,
                                            int                error);
static void esaio_completion_sendto_fail(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              error,
                                         BOOLEAN_T        inform);
static BOOLEAN_T esaio_completion_sendmsg(ESAIOThreadData*    dataP,
                                          ESockDescriptor*    descP,
                                          OVERLAPPED*         ovl,
                                          ErlNifEnv*          opEnv,
                                          ErlNifPid*          opCaller,
                                          ESAIOOpDataSendMsg* opDataP,
                                          int                 error);
static void esaio_completion_sendmsg_success(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             OVERLAPPED*         ovl,
                                             ErlNifEnv*          opEnv,
                                             ErlNifPid*          opCaller,
                                             ESAIOOpDataSendMsg* opDataP);
static void esaio_completion_sendmsg_aborted(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ErlNifPid*          opCaller,
                                             ESAIOOpDataSendMsg* opDataP);
static void esaio_completion_sendmsg_failure(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ErlNifPid*          opCaller,
                                             ESAIOOpDataSendMsg* opDataP,
                                             int                 error);
static void esaio_completion_sendmsg_fail(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              error,
                                          BOOLEAN_T        inform);
static BOOLEAN_T esaio_completion_recv(ESAIOThreadData* dataP,
                                       ESockDescriptor* descP,
                                       OVERLAPPED*      ovl,
                                       ErlNifEnv*       opEnv,
                                       ErlNifPid*       opCaller,
                                       ESAIOOpDataRecv* opDataP,
                                       int              error);
static void esaio_completion_recv_success(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          OVERLAPPED*      ovl,
                                          ErlNifEnv*       opEnv,
                                          ErlNifPid*       opCaller,
                                          ESAIOOpDataRecv* opDataP);
static void esaio_completion_recv_aborted(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ErlNifPid*       opCaller,
                                          ESAIOOpDataRecv* opDataP);
static void esaio_completion_recv_failure(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ErlNifPid*       opCaller,
                                          ESAIOOpDataRecv* opDataP,
                                          int              error);
static void esaio_completion_recv_completed(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            OVERLAPPED*      ovl,
                                            ErlNifEnv*       opEnv,
                                            ErlNifPid*       opCaller,
                                            ESAIOOpDataRecv* opDataP,
                                            ESockRequestor*  reqP);
static ERL_NIF_TERM esaio_completion_recv_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ErlNifEnv*       opEnv,
                                               ESAIOOpDataRecv* opDataP,
                                               DWORD            flags);
static ERL_NIF_TERM esaio_completion_recv_partial(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ErlNifEnv*       opEnv,
                                                  ESAIOOpDataRecv* opDataP,
                                                  ESockRequestor*  reqP,
                                                  DWORD            read,
                                                  DWORD            flags);
static ERL_NIF_TERM esaio_completion_recv_partial_done(ErlNifEnv*       env,
                                                       ESockDescriptor* descP,
                                                       ErlNifEnv*       opEnv,
                                                       ESAIOOpDataRecv* opDataP,
                                                       ssize_t          read,
                                                       DWORD            flags);
static ERL_NIF_TERM esaio_completion_recv_partial_part(ErlNifEnv*       env,
                                                       ESockDescriptor* descP,
                                                       ErlNifEnv*       opEnv,
                                                       ESAIOOpDataRecv* opDataP,
                                                       ssize_t          read,
                                                       DWORD            flags);
static void esaio_completion_recv_not_active(ESockDescriptor* descP);
static void esaio_completion_recv_closed(ESockDescriptor* descP,
                                         int              error);
static void esaio_completion_recv_fail(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              error,
                                       BOOLEAN_T        inform);
static BOOLEAN_T esaio_completion_recvfrom(ESAIOThreadData*     dataP,
                                           ESockDescriptor*     descP,
                                           OVERLAPPED*          ovl,
                                           ErlNifEnv*           opEnv,
                                           ErlNifPid*           opCaller,
                                           ESAIOOpDataRecvFrom* opDataP,
                                           int                  error);
static void esaio_completion_recvfrom_success(ErlNifEnv*           env,
                                              ESockDescriptor*     descP,
                                              OVERLAPPED*          ovl,
                                              ErlNifEnv*           opEnv,
                                              ErlNifPid*           opCaller,
                                              ESAIOOpDataRecvFrom* opDataP);
static void esaio_completion_recvfrom_more_data(ErlNifEnv*           env,
                                                ESockDescriptor*     descP,
                                                ErlNifEnv*           opEnv,
                                                ErlNifPid*           opCaller,
                                                ESAIOOpDataRecvFrom* opDataP,
                                                int                  error);
static void esaio_completion_recvfrom_aborted(ErlNifEnv*           env,
                                              ESockDescriptor*     descP,
                                              ErlNifPid*           opCaller,
                                              ESAIOOpDataRecvFrom* opDataP);
static void esaio_completion_recvfrom_failure(ErlNifEnv*           env,
                                              ESockDescriptor*     descP,
                                              ErlNifPid*           opCaller,
                                              ESAIOOpDataRecvFrom* opDataP,
                                              int                  error);
static void esaio_completion_recvfrom_completed(ErlNifEnv*           env,
                                                ESockDescriptor*     descP,
                                                OVERLAPPED*          ovl,
                                                ErlNifEnv*           opEnv,
                                                ErlNifPid*           opCaller,
                                                ESAIOOpDataRecvFrom* opDataP,
                                                ESockRequestor*      reqP);
static ERL_NIF_TERM esaio_completion_recvfrom_done(ErlNifEnv*           env,
                                                   ESockDescriptor*     descP,
                                                   ErlNifEnv*           opEnv,
                                                   ESAIOOpDataRecvFrom* opDataP,
                                                   DWORD                flags);
static ERL_NIF_TERM esaio_completion_recvfrom_partial(ErlNifEnv*           env,
                                                      ESockDescriptor*     descP,
                                                      ErlNifEnv*           opEnv,
                                                      ESAIOOpDataRecvFrom* opDataP,
                                                      ESockRequestor*      reqP,
                                                      DWORD                read,
                                                      DWORD                flags);
static void esaio_completion_recvfrom_fail(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           int              error,
                                           BOOLEAN_T        inform);
static BOOLEAN_T esaio_completion_recvmsg(ESAIOThreadData*    dataP,
                                          ESockDescriptor*    descP,
                                          OVERLAPPED*         ovl,
                                          ErlNifEnv*          opEnv,
                                          ErlNifPid*          opCaller,
                                          ESAIOOpDataRecvMsg* opDataP,
                                          int                error);
static void esaio_completion_recvmsg_success(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             OVERLAPPED*         ovl,
                                             ErlNifEnv*          opEnv,
                                             ErlNifPid*          opCaller,
                                             ESAIOOpDataRecvMsg* opDataP);
static void esaio_completion_recvmsg_aborted(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ErlNifPid*          opCaller,
                                             ESAIOOpDataRecvMsg* opDataP);
static void esaio_completion_recvmsg_failure(ErlNifEnv*          env,
                                             ESockDescriptor*    descP,
                                             ErlNifPid*          opCaller,
                                             ESAIOOpDataRecvMsg* opDataP,
                                             int                 error);
static void esaio_completion_recvmsg_completed(ErlNifEnv*          env,
                                               ESockDescriptor*    descP,
                                               OVERLAPPED*         ovl,
                                               ErlNifEnv*          opEnv,
                                               ErlNifPid*          opCaller,
                                               ESAIOOpDataRecvMsg* opDataP,
                                               ESockRequestor*     reqP);
static ERL_NIF_TERM esaio_completion_recvmsg_done(ErlNifEnv*          env,
                                                  ESockDescriptor*    descP,
                                                  ErlNifEnv*          opEnv,
                                                  ESAIOOpDataRecvMsg* opDataP,
                                                  DWORD               flags);
static ERL_NIF_TERM esaio_completion_recvmsg_partial(ErlNifEnv*          env,
                                                     ESockDescriptor*    descP,
                                                     ErlNifEnv*          opEnv,
                                                     ESAIOOpDataRecvMsg* opDataP,
                                                     ESockRequestor*     reqP,
                                                     DWORD               read,
                                                     DWORD               flags);
static void esaio_completion_recvmsg_fail(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              error,
                                          BOOLEAN_T        inform);

static ERL_NIF_TERM esaio_completion_get_ovl_result_fail(ErlNifEnv*       env,
                                                         ESockDescriptor* descP,
                                                         int              error);

static BOOL get_send_ovl_result(SOCKET      sock,
                                OVERLAPPED* ovl,
                                DWORD*      written);
static BOOL get_recv_ovl_result(SOCKET      sock,
                                OVERLAPPED* ovl,
                                DWORD*      read,
                                DWORD*      flags);
static BOOL get_recvmsg_ovl_result(SOCKET      sock,
                                   OVERLAPPED* ovl,
                                   DWORD*      read);
static BOOL get_ovl_result(SOCKET      sock,
                           OVERLAPPED* ovl,
                           DWORD*      transfer,
                           DWORD*      flags);

static void esaio_completion_inc(ESAIOThreadData* dataP);

static int esaio_add_socket(ESockDescriptor* descP);

static void esaio_send_completion_msg(ErlNifEnv*       sendEnv,
                                      ESockDescriptor* descP,
                                      ErlNifPid*       pid,
                                      ErlNifEnv*       msgEnv,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     connRef);
static ERL_NIF_TERM mk_completion_msg(ErlNifEnv*   env,
                                      ERL_NIF_TERM sockRef,
                                      ERL_NIF_TERM completionRef);

static void esaio_down_acceptor(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                const ErlNifPid* pidP,
                                const ErlNifMonitor* monP);
static void esaio_down_writer(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              const ErlNifPid* pidP,
                              const ErlNifMonitor* monP);
static void esaio_down_reader(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              const ErlNifPid* pidP,
                              const ErlNifMonitor* monP);

static BOOLEAN_T do_stop(ErlNifEnv*       env,
                         ESockDescriptor* descP);


/* =================================================================== *
 *                                                                     *
 *                      Local (global) variables                       *
 *                                                                     *
 * =================================================================== */

static ESAIOControl ctrl = {0};



/* =================================================================== *
 *                                                                     *
 *                        Various esaio macros                         *
 *                                                                     *
 * =================================================================== */

/* Global socket debug */
#define SGDBG( proto )            ESOCK_DBG_PRINTF( ctrl.dbg , proto )

/* These are just wrapper macros for the I/O Completion Port */
#define ESAIO_IOCP_CREATE(NT)                           \
    CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, (u_long) 0, (NT))
#define ESAIO_IOCP_ADD(SOCK, DP)                                \
    CreateIoCompletionPort((SOCK), ctrl.cport, (ULONG*) (DP), 0)

/* These are just wrapper macros for the I/O Completion Port queue */
#define ESAIO_IOCQ_POP(NB, DP, OLP)                                     \
    GetQueuedCompletionStatus(ctrl.cport, (DP), (DP), (OLP), INFINITE)
#define ESAIO_IOCQ_PUSH(OP)                                             \
    PostQueuedCompletionStatus(ctrl.cport, 0, 0, (OVERLAPPED*) (OP))
#define ESAIO_IOCQ_CANCEL(H, OP)                                        \
    CancelIoEx((H), (OVERLAPPED*) (OP))


/* =================================================================== *
 *                                                                     *
 *                         I/O Backend exports                         *
 *                                                                     *
 * =================================================================== */


/* *******************************************************************
 * This function is called during (esock) nif loading
 * The only argument that we actually *need* is the 'numThreads'.
 * 'dataP' is just for convenience (dbg and stuff).
 */
extern
int esaio_init(unsigned int     numThreads,
               const ESockData* dataP)
{
    int          ires, save_errno;
    unsigned int i;
    DWORD        dummy;
    GUID         guidAcceptEx  = WSAID_ACCEPTEX;
    GUID         guidConnectEx = WSAID_CONNECTEX;
    GUID         guidSendMsg   = WSAID_WSASENDMSG;
    GUID         guidRecvMsg   = WSAID_WSARECVMSG;

    /* Enabling this results in a core dump when calling socket:accept
     * multiple times (the second call fails in env alloc).!
     */
    // ctrl.dbg               = TRUE;
    ctrl.dbg               = dataP->dbg;
    ctrl.sockDbg           = dataP->sockDbg;

    SGDBG( ("WIN-ESAIO", "esaio_init -> entry\r\n") );

    ctrl.cntMtx             = MCREATE("win-esaio.cnt");
    ctrl.unexpectedConnects = 0;
    ctrl.unexpectedAccepts  = 0;
    ctrl.unexpectedWrites   = 0;
    ctrl.unexpectedReads    = 0;
    ctrl.genErrs            = 0;
    ctrl.unknownCmds        = 0;

    /* We should actually check the value of 'numThreads'
     * Since if its zero (the default), we should instead
     * assign: 2 * 'number of schedulers'
     * Or shall we trust the 'prim_socket' preloaded to 
     * select the proper value?
     */
    ctrl.numThreads = (DWORD) numThreads;

    // Initialize Winsock
    SGDBG( ("WIN-ESAIO", "esaio_init -> try initialize winsock\r\n") );
    ires = WSAStartup(MAKEWORD(2, 2), &ctrl.wsaData);
    if (ires != NO_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed initialize winsock: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        return ESAIO_ERR_WINSOCK_INIT;
    }    

    // Create a handle for the completion port
    SGDBG( ("WIN-ESAIO", "esaio_init -> try create I/O completion port\r\n") );
    ctrl.cport = CreateIoCompletionPort(INVALID_HANDLE_VALUE,
                                        NULL, (u_long) 0, ctrl.numThreads);
    if (ctrl.cport == NULL) {
        save_errno = sock_errno();
        
        esock_error_msg("Failed create I/O Completion Port:"
                        "\r\n   %s (%d)"
                        "\r\n", erl_errno_id(save_errno), save_errno);

        WSACleanup();

        return ESAIO_ERR_IOCPORT_CREATE;
    }


    /* Create the "service init" socket and then
     * extract the AcceptEx and ConnectEx functions.
     */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try create 'service init' socket\r\n") );
    if ( !init_srv_init_socket(&save_errno) ) {

        esock_error_msg("Failed create 'service init' socket: "
                        "\r\n   %s (%d)"
                        "\r\n",
                        erl_errno_id(save_errno), save_errno);

        WSACleanup();

        return ESAIO_ERR_FSOCK_CREATE;
    }


    /* Load the AcceptEx function into memory using WSAIoctl.
     * The WSAIoctl function is an extension of the ioctlsocket()
     * function that can use overlapped I/O.
     * The function's 3rd through 6th parameters are input and output
     * buffers where we pass the pointer to our AcceptEx function.
     * This is used so that we can call the AcceptEx function directly,
     * rather than refer to the Mswsock.lib library.
     */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try extract 'accept' function\r\n") );
    ires = WSAIoctl(ctrl.srvInit, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidAcceptEx, sizeof (guidAcceptEx), 
                    &ctrl.accept, sizeof (ctrl.accept), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'accept' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.srvInit);
        ctrl.srvInit  = INVALID_SOCKET;
        ctrl.accept = NULL;

        WSACleanup();

        return ESAIO_ERR_IOCTL_ACCEPT_GET;
    }


    /* Basically the same as for AcceptEx above */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try extract 'connect' function\r\n") );
    ires = WSAIoctl(ctrl.srvInit, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidConnectEx, sizeof (guidConnectEx), 
                    &ctrl.connect, sizeof (ctrl.connect), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'connect' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.srvInit);
        ctrl.srvInit  = INVALID_SOCKET;
        ctrl.accept = NULL;

        WSACleanup();

        return ESAIO_ERR_IOCTL_CONNECT_GET;
    }
    

    /* Basically the same as for AcceptEx above */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try extract 'sendmsg' function\r\n") );
    ires = WSAIoctl(ctrl.srvInit, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidSendMsg, sizeof (guidSendMsg), 
                    &ctrl.sendmsg, sizeof (ctrl.sendmsg), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'sendmsg' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.srvInit);
        ctrl.srvInit   = INVALID_SOCKET;
        ctrl.accept  = NULL;
        ctrl.connect = NULL;

        WSACleanup();

        return ESAIO_ERR_IOCTL_SENDMSG_GET;
    }
    

    /* Basically the same as for AcceptEx above */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try extract 'recvmsg' function\r\n") );
    ires = WSAIoctl(ctrl.srvInit, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidRecvMsg, sizeof (guidRecvMsg), 
                    &ctrl.recvmsg, sizeof (ctrl.recvmsg), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'recvmsg' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.srvInit);
        ctrl.srvInit   = INVALID_SOCKET;
        ctrl.accept  = NULL;
        ctrl.connect = NULL;
        ctrl.sendmsg = NULL;

        WSACleanup();

        return ESAIO_ERR_IOCTL_RECVMSG_GET;
    }
    

    /*
     * Create the completion port thread pool.
     */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try alloc thread pool memory\r\n") );
    ctrl.threads = MALLOC(numThreads * sizeof(ESAIOThread));
    ESOCK_ASSERT( ctrl.threads != NULL );

    SGDBG( ("WIN-ESAIO", "esaio_init -> basic init of thread data\r\n") );
    for (i = 0; i < numThreads; i++) {
        ctrl.threads[i].data.id    = i;
        ctrl.threads[i].data.state = ESAIO_THREAD_STATE_UNDEF;
        ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_UNDEF;
        ctrl.threads[i].data.env   = NULL;
        ctrl.threads[i].data.cnt   = 0;
    }

    SGDBG( ("WIN-ESAIO", "esaio_init -> try create thread(s)\r\n") );
    for (i = 0; i < numThreads; i++) {
        char buf[64]; /* Buffer used for building the names */
        int  j;

        /* We set these here to avoid raise with the thread.
         * *If* we fail in the creation, then we set an error */

        ctrl.threads[i].data.state = ESAIO_THREAD_STATE_INITIATING;
        ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_OK;

        SGDBG( ("WIN-ESAIO",
                "esaio_init -> try create %d thread opts\r\n", i) );
        sprintf(buf, "esaio-opts[%d]", i);
        ctrl.threads[i].optsP      = TOCREATE(buf);
        if (ctrl.threads[i].optsP == NULL) {

            esock_error_msg("Failed create thread opts %d\r\n", i);

            ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_TOCREATE;

            for (j = 0; j < i; j++) {
                SGDBG( ("WIN-ESAIO",
                        "esaio_init -> destroy thread opts %d\r\n", j) );
                TODESTROY(ctrl.threads[j].optsP);
            }

            WSACleanup();

            return ESAIO_ERR_THREAD_OPTS_CREATE;
        }

        SGDBG( ("WIN-ESAIO",
                "esaio_init -> try create thread %d\r\n", i) );
        sprintf(buf, "esaio[%d]", i);
        if (0 != TCREATE(buf, 
                         &ctrl.threads[i].tid, 
                         esaio_completion_main, 
                         (void*) &ctrl.threads[i].data, 
                         ctrl.threads[i].optsP)) {

            esock_error_msg("Failed create thread %d\r\n", i);

            ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_TCREATE;

            for (j = 0; j <= i; j++) {
                SGDBG( ("WIN-ESAIO",
                        "esaio_init -> destroy thread opts %d\r\n", j) );
                TODESTROY(ctrl.threads[j].optsP);
            }

            WSACleanup();

            return ESAIO_ERR_THREAD_CREATE;
        }

    }


    SGDBG( ("WIN-ESAIO", "esaio_init -> done\r\n") );

    return ESAIO_OK;
}


static
BOOLEAN_T init_srv_init_socket(int* savedErrno)
{
    SOCKET sock;
    int    save_errno = 0;

    sock = sock_open(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock == INVALID_SOCKET) {
        save_errno = sock_errno();

        /* This *could* be because we are on a 'IPv6 only' machine
         * => So try with that (AF_INET6) domain also.
         */

        if (save_errno == WSAEAFNOSUPPORT) { 
            sock = sock_open(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
            if (sock == INVALID_SOCKET) {
                /* Ouch, still failing, so we will keep the original error */
                ctrl.srvInit = INVALID_SOCKET;
                *savedErrno  = save_errno;
                return FALSE;
            } else {
                ctrl.srvInit  = sock;
                *savedErrno = 0;
                return TRUE;
            }
        } else {
            ctrl.srvInit = INVALID_SOCKET;
            *savedErrno  = save_errno;
            return FALSE;            
        }
    } else {
        ctrl.srvInit = sock;
        *savedErrno  = 0;
        return TRUE;
    }
}


/* *******************************************************************
 * Finish, terminate, the ESock Async I/O backend.
 * This means principally to terminate the threads of the thread pool.
 * Issue a "message" via PostQueuedCompletionStatus
 * instructing every thread (of the pool) to terminate.
 */
extern
void esaio_finish()
{
    int t, lastThread;

    SGDBG( ("WIN-ESAIO", "esaio_finish -> entry\r\n") );

    if (ctrl.srvInit != INVALID_SOCKET) {
        SGDBG( ("WIN-ESAIO", "esaio_finish -> close 'dummy' socket\r\n") );
        (void) sock_close(ctrl.srvInit);
        ctrl.srvInit = INVALID_SOCKET;
    }

    SGDBG( ("WIN-ESAIO",
            "esaio_finish -> try terminate %d worker threads\r\n",
            ctrl.numThreads) );
    for (t = 0, lastThread = -1, lastThread; t < ctrl.numThreads; t++) {
        ESAIOOperation* opP;
        BOOL            qres;

        SGDBG( ("WIN-ESAIO",
                "esaio_finish -> "
                "[%d] try allocate (terminate-) operation\r\n", t) );

        /* Where is this FREE'ed??
         * By the thread after it has been received?
         */

        opP = MALLOC( sizeof(ESAIOOperation) );

        /* We should actually check that the alloc was successful
         * and if not ... 
         * Note that this function is only called when we are terminating
         * the VM. So, is there actually any point in "doing" something?
         * Or should we solve this another way? Instead of allocating
         * a memory block; Send in a constant, ESAIO_OP_TERMINATE,
         * *instead of* the overlapped pointer!
         */

        /* If this dows not work, there is not much we can do!
         * And since this is *only* done when we terminate the VM...
         */

        if (opP != NULL) {
            sys_memzero((char *) opP, sizeof(ESAIOOperation));

            opP->tag = ESAIO_OP_TERMINATE;
            /* This should never be accessed for this command, *
             * but just to be on the safe side...              */
            enif_set_pid_undefined(&opP->caller);
            opP->env = NULL;

            SGDBG( ("WIN-ESAIO",
                    "esaio_finish -> "
                    "try post (terminate-) package %d\r\n", t) );

            qres = PostQueuedCompletionStatus(ctrl.cport,
                                              0, 0, (OVERLAPPED*) opP);

            if (!qres) {
                int save_errno = sock_errno();
                esock_error_msg("Failed posting 'terminate' command: "
                                "\r\n   %s (%d)"
                                "\r\n", erl_errno_id(save_errno), save_errno);
                break;
            } else {
                lastThread = t;
            }

        } else {

            SGDBG( ("WIN-ESAIO",
                    "esaio_finish -> "
                    "failed allocate (terminate-) operation %d\r\n", t) );

        }
    }

    if (lastThread >= 0) {
        SGDBG( ("WIN-ESAIO",
                "esaio_finish -> await (worker) thread(s) termination\r\n") );
        for (t = 0; t < (lastThread+1); t++) {

            SGDBG( ("WIN-ESAIO",
                    "esaio_finish -> try join with thread %d\r\n", t) );

            (void) TJOIN(ctrl.threads[t].tid, NULL);

            SGDBG( ("WIN-ESAIO", "esaio_finish -> joined with %d\r\n", t) );

        }
    }

    SGDBG( ("WIN-ESAIO", "esaio_finish -> cleanup\r\n") );
    WSACleanup();

    /* This is overkill,
     * since this function, esaio_finish, is called when the VM is halt'ing...
     * ...but just to be a good citizen...
     */
    SGDBG( ("WIN-ESAIO", "esaio_finish -> free the thread pool data\r\n") );
    FREE( ctrl.threads );

    SGDBG( ("WIN-ESAIO",
            "esaio_finish -> invalidate (extension) functions\r\n") );
    ctrl.accept  = NULL;
    ctrl.connect = NULL;
    ctrl.sendmsg = NULL;
    ctrl.recvmsg = NULL;
    
    SGDBG( ("WIN-ESAIO", "esaio_finish -> done\r\n") );

    return;
}



/* *******************************************************************
 * esaio_info - Return info "about" this I/O backend.
 */

extern
ERL_NIF_TERM esaio_info(ErlNifEnv* env)
{
    ERL_NIF_TERM info, numThreads,
        numUnexpAccs, numUnexpConns, numUnexpWs, numUnexpRs,
        numGenErrs,
        numUnknownCmds;
    
    numThreads     = MKUI(env, ctrl.numThreads);

    MLOCK(ctrl.cntMtx);

    numUnexpConns  = MKUI(env, ctrl.unexpectedConnects);
    numUnexpAccs   = MKUI(env, ctrl.unexpectedAccepts);
    numUnexpWs     = MKUI(env, ctrl.unexpectedWrites);
    numUnexpRs     = MKUI(env, ctrl.unexpectedReads);
    numGenErrs     = MKUI(env, ctrl.genErrs);
    numUnknownCmds = MKUI(env, ctrl.unknownCmds);

    MUNLOCK(ctrl.cntMtx);

    {
        ERL_NIF_TERM cntKeys[] = {esock_atom_num_unexpected_connects,
                                  esock_atom_num_unexpected_accepts,
                                  esock_atom_num_unexpected_writes,
                                  esock_atom_num_unexpected_reads,
                                  esock_atom_num_general_errors,
                                  esock_atom_num_unknown_cmds};
        ERL_NIF_TERM cntVals[] = {numUnexpConns, numUnexpAccs,
                                  numUnexpWs, numUnexpRs,
                                  numGenErrs,
                                  numUnknownCmds};
        unsigned int numCntKeys = NUM(cntKeys);
        unsigned int numCntVals = NUM(cntVals);
        ERL_NIF_TERM counters;

        ESOCK_ASSERT( numCntKeys == numCntVals );
        ESOCK_ASSERT( MKMA(env, cntKeys, cntVals, numCntKeys, &counters) );

        {
            ERL_NIF_TERM keys[]  = {esock_atom_name,
                                    esock_atom_num_threads,
                                    esock_atom_counters};
            ERL_NIF_TERM vals[]  = {MKA(env, "win_esaio"),
                                    numThreads,
                                    counters};
            unsigned int numKeys = NUM(keys);
            unsigned int numVals = NUM(vals);

            ESOCK_ASSERT( numKeys == numVals );
            ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );
        }
    }

    return info;
}



/* *******************************************************************
 * esaio_open_plain - create an endpoint (from an existing fd) for
 *                    communication.
 *
 * Create an *overlapped* socket, then add it to the I/O
 * completion port.
 */

extern
ERL_NIF_TERM esaio_open_plain(ErlNifEnv*       env,
                              int              domain,
                              int              type,
                              int              protocol,
                              ERL_NIF_TERM     eopts,
                              const ESockData* dataP)
{
    /* We do not actually need the dataP since we already have the dbg... */
    BOOLEAN_T        dbg    = esock_open_is_debug(env, eopts, dataP->sockDbg);
    BOOLEAN_T        useReg = esock_open_use_registry(env, eopts, dataP->useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              proto   = protocol;
    DWORD            dwFlags = WSA_FLAG_OVERLAPPED;
    SOCKET           sock    = INVALID_SOCKET;
    ErlNifPid        self;
    int              res, save_errno;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("WIN-ESAIO", "esaio_open_plain -> entry with"
             "\r\n   domain:   %d"
             "\r\n   type:     %d"
             "\r\n   protocol: %d"
             "\r\n   eopts:    %T"
             "\r\n", domain, type, protocol, eopts) );

    sock = sock_open_O(domain, type, proto);

    if (sock == INVALID_SOCKET) {
        save_errno = sock_errno();
        return esock_make_error_errno(env, save_errno);
    }

    SSDBG2( dbg, ("WIN-ESAIO",
                  "esaio_open_plain -> open success: %d\r\n", sock) );

    /* NOTE that if the protocol = 0 (default) and the domain is not
     * local (AF_LOCAL) we need to explicitly get the protocol here!
     */
    
    if (proto == 0)
        (void) esock_open_which_protocol(sock, &proto);

    /* Create and initiate the socket "descriptor" */
    descP           = esock_alloc_descriptor(sock);
    descP->ctrlPid  = self;
    descP->domain   = domain;
    descP->type     = type;
    descP->protocol = proto;

    SSDBG2( dbg, ("WIN-ESAIO",
                  "esaio_open_plain -> add to completion port\r\n") );

    if (ESAIO_OK != (save_errno = esaio_add_socket(descP))) {
        // See esock_dtor for what needs done!
        ERL_NIF_TERM tag    = esock_atom_add_socket;
        ERL_NIF_TERM reason = MKA(env, erl_errno_id(save_errno));

        SSDBG2( dbg, ("WIN-ESAIO",
                      "esaio_open_plain -> "
                      "failed adding socket to completion port: "
                      "%T (%d)\r\n", reason, save_errno) );

        esock_dealloc_descriptor(env, descP);
        sock_close(sock);

        /* This should really be:
         *     {error, {invalid, {add_to_completion_port, Reason}}}
         */

        return esock_make_error_t2r(env, tag, reason);
    }

    SSDBG2( dbg, ("WIN-ESAIO",
                  "esaio_open_plain -> create socket ref\r\n") );

    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    SSDBG2( dbg, ("WIN-ESAIO",
                  "esaio_open_plain -> monitor owner %T\r\n", descP->ctrlPid) );

    ESOCK_ASSERT( MONP("esaio_open -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    esock_inc_socket(domain, type, proto);

    SSDBG2( dbg, ("WIN-ESAIO",
                  "esaio_open_plain -> maybe update registry\r\n") );

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    SSDBG2( dbg, ("WIN-ESAIO",
                  "esaio_open_plain -> done\r\n") );

    return esock_make_ok2(env, sockRef);
}



/* *******************************************************************
 * esaio_bind - Bind a name to a socket.
 *
 */
extern
ERL_NIF_TERM esaio_bind(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ESockAddress*    sockAddrP,
                        SOCKLEN_T        addrLen)
{
    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    if (sock_bind(descP->sock, &sockAddrP->sa, addrLen) < 0) {
        return esock_make_error_errno(env, sock_errno());
    }

    descP->writeState |= ESOCK_STATE_BOUND;

    return esock_atom_ok;
}



/* *******************************************************************
 * esaio_connect - Connect the socket to a specified address
 *
 * The function we use here, ConnectEx, is intended for STREAM/TCP.
 * But (traditional) connect can be used with DGRAM/UDP.
 * So, does ConnectEx work with DGRAM/UDP sockets? I think not.
 * So we may need to test what kind of socket we have, and for
 * DGRAM/UDP use the "old" connect:
 *
 *    if (type == DGRAM) && (protocol == UDP)
 *       connect(...);
 *    else
 *       ConnectEx(...);
 *
 * Quote from the Microsoft documentation:
 * "The ConnectEx function can only be used with connection-oriented sockets.
 *  The socket passed in the s parameter must be created with a socket type
 *  of SOCK_STREAM, SOCK_RDM, or SOCK_SEQPACKET."
 *
 * Quote from the Microsoft documentation:
 * When the ConnectEx function successfully completes, the "connected socket"
 * can be passed to only the following functions:
 *
 *   * ReadFile         (not provided by our API)
 *   * WriteFile        (not provided by our API)
 *   * send or WSASend  (send not used by us)
 *   * recv or WSARecv  (recv not used by us)
 *   * TransmitFile     (not provided by our API)
 *   * closesocket
 *
 * Socket used *must* be *bound* before calling ConnectEx!
 */

extern
ERL_NIF_TERM esaio_connect(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     connRef,
                           ESockAddress*    addrP,
                           SOCKLEN_T        addrLen)
{
    /*
     * Verify that we are in the proper state
     */

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_connect(%T, %d) -> verify open\r\n",
            sockRef, descP->sock) );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, esock_atom_closed);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_connect(%T, %d) -> verify type: %s\r\n",
            sockRef, descP->sock, TYPE2STR(descP->type)) );

    switch (descP->type) {
    case SOCK_STREAM:
        return esaio_connect_stream(env,
                                    descP, sockRef, connRef,
                                    addrP, addrLen);
        break;

    case SOCK_DGRAM:
        return esaio_connect_dgram(env,
                                   descP, sockRef, connRef,
                                   addrP, addrLen);
        break;

    default:
        return enif_make_badarg(env);
    }
}



/* *******************************************************************
 * esaio_connect_stream - Connect the (stream) socket
 */
static
ERL_NIF_TERM esaio_connect_stream(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     connRef,
                                  ESockAddress*    addrP,
                                  SOCKLEN_T        addrLen)
{
    int             save_errno;
    BOOL            cres;
    ESAIOOperation* opP;
    ERL_NIF_TERM    eres;
    ErlNifPid       self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );
    

    /* ConnectEx *requires* the socket to be bound */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_connect_stream(%T) -> verify bound\r\n", sockRef) );
    if (! IS_BOUND(descP->writeState))
        return esock_make_error(env, esock_atom_not_bound);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_connect_stream(%T) -> check if ongoing\r\n",
            sockRef) );
    if (descP->connectorP != NULL) {

        /* Connect already in progress, check if its us */

        if (COMPARE_PIDS(&self, &descP->connector.pid) != 0) {
            /* *Other* process has connect in progress */
            if (addrP != NULL) {
                eres = esock_make_error(env, esock_atom_already);
            } else {
                /* This is a bad call sequence
                 * - connect without an address is only allowed
                 *   for the connecting process
                 */
                eres = esock_raise_invalid(env, esock_atom_state);
            }
        } else {

            /* TRHIS IS NOT HOW IT WORKS ON WINDOWS!
             * This function should never be called *again*
             * The completion message contains the full and final answer.
             * No need to call again!
             */

            eres = esock_raise_invalid(env, esock_atom_state);
        }

    } else if (addrP == NULL) {

        /* This is a bad call sequence
         * - connect without an address is not valid on Windows.
         */
        eres = esock_raise_invalid(env, esock_atom_state);

    } else {

        DWORD sentDummy = 0;

        /* No connect in progress */

        /* Initial connect call, with address */
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_connect_stream(%T) -> allocate (connect) operation\r\n",
                sockRef) );

        opP = MALLOC( sizeof(ESAIOOperation) );
        ESOCK_ASSERT( opP != NULL);
        sys_memzero((char*) opP, sizeof(ESAIOOperation));

        opP->tag = ESAIO_OP_CONNECT;

        /* Its a bit annoying that we have to alloc an env and then
         * copy the ref *before* we know that we actually need it.
         * How much does this cost?
         */
        
        /* Initiate connector */
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_connect_stream(%T) -> initiate connector\r\n",
                sockRef) );

        descP->connector.pid   = self;
        ESOCK_ASSERT( MONP("esaio_connect_stream -> conn",
                           env, descP,
                           &self, &descP->connector.mon) == 0 );
        descP->connector.dataP = (void*) opP;
        descP->connector.env   = esock_alloc_env("connector");
        descP->connector.ref   = CP_TERM(descP->connector.env, connRef);
        descP->connectorP      = &descP->connector;
        descP->writeState |=
            (ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

        opP->env                  = esock_alloc_env("esaio-connect-stream");
        opP->caller               = self;
        opP->data.connect.sockRef = CP_TERM(opP->env, sockRef);
        opP->data.connect.connRef = CP_TERM(opP->env, connRef);

        SSDBG( descP,
               ("WIN-ESAIO", "esaio_connect_stream {%d} -> try connect\r\n",
                descP->sock) );

        /*
         * BOOL LpfnConnectex(
         *      [in]           SOCKET s,
         *      [in]           const sockaddr *name,
         *      [in]           int namelen,
         *      [in, optional] PVOID lpSendBuffer,
         *      [in]           DWORD dwSendDataLength,
         *      [out]          LPDWORD lpdwBytesSent,
         *      [in]           LPOVERLAPPED lpOverlapped
         * )
         */

        cres = sock_connect_O(descP->sock,
                              addrP, addrLen,
                              &sentDummy, (OVERLAPPED*) opP);

        /* 
         * We need to keep using the requestor "queues"!
         * That is the "only" way to handle the monitoring of
         * the requestor.
         * That is if the request (for instance a connect) is
         * is scheduled, WSA_IO_PENDING, then we need to store
         * the info about the requestor somewhere we can access it,
         * in case the requestor for example dies (and we need to
         * clean up).
         */

        eres = connect_stream_check_result(env, descP, opP, cres);

    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_connect {%d} -> done with"
                   "\r\n   eres: %T"
                   "\r\n",
                   descP->sock, eres) );

    return eres;
}


static
ERL_NIF_TERM connect_stream_check_result(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ESAIOOperation*  opP,
                                         BOOL             cres)
{
    ERL_NIF_TERM eres, tag, reason;
    int          save_errno;

    if (cres) {

        /* Success already! */

        int err;

        SSDBG( descP,
               ("WIN-ESAIO",
                "connect_stream_check_result(%d) -> connected\r\n",
                descP->sock) );

        /* Clean up the connector stuff, no need for that anymore */
        esock_requestor_release("connect_stream_check_result -> success",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;

        /* We need to make sure peername and sockname works! */

        SSDBG( descP,
               ("WIN-ESAIO",
                "connect_stream_check_result {%d} -> "
                "update connect context\r\n",
                descP->sock) );

        err = ESAIO_UPDATE_CONNECT_CONTEXT( descP->sock );

        if (err == 0) {

            descP->writeState &=
                ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
            descP->writeState |= ESOCK_STATE_CONNECTED;

            eres = esock_atom_ok;

        } else {

            save_errno = sock_errno();
            tag        = esock_atom_update_connect_context;
            reason     = ENO2T(env, save_errno);

            SSDBG( descP, ("WIN-ESAIO",
                           "connect_stream_check_result(%d) -> "
                           "connect context update failed: %T\r\n",
                           descP->sock, reason) );

            sock_close(descP->sock);
            descP->writeState = ESOCK_STATE_CLOSED;

            eres = esock_make_error_t2r(env, tag, reason);
        }

    } else {

        /* Connect returned error, check which */

        save_errno = sock_errno();

        if (save_errno == WSA_IO_PENDING) {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "connect_stream_check_result(%d) -> connect scheduled\r\n",
                    descP->sock) );

            eres = esock_atom_completion;

        } else {
            ERL_NIF_TERM ereason = ENO2T(env, save_errno);

            SSDBG( descP, ("WIN-ESAIO",
                           "connect_stream_check_result(%d) -> "
                           "connect attempt failed: %T\r\n",
                           descP->sock, ereason) );

            /* Clean up the connector stuff, no need for that anymore */
            esock_requestor_release("connect_stream_check_result -> failure",
                                    env, descP, &descP->connector);
            descP->connectorP = NULL;

            /* Will an event be generetade in this case?
             * Assume not => We need to clean up here!
             */
            esock_clear_env("connect_stream_check_result", opP->env);
            esock_free_env("connect_stream_check_result", opP->env);
            FREE( opP );

            sock_close(descP->sock);
            descP->writeState = ESOCK_STATE_CLOSED;

            eres = esock_make_error(env, ereason);
        }
    }

    return eres;
}



/* *** esaio_connect_dgram ***
 * Handle the "fake" connect of a DGRAM socket ("bind" to 
 * a remote address, so user can use send/recv instead of
 * sendto/recvfrom). Should use sock_connect(...)
 * This is corrently just a placeholder!
 */
static
ERL_NIF_TERM esaio_connect_dgram(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     connRef,
                                 ESockAddress*    addrP,
                                 SOCKLEN_T        addrLen)
{
    int       save_errno;
    ErlNifPid self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    if (descP->connectorP != NULL) {
        /* Connect in progress */

        return esock_make_error(env, esock_atom_already);
    }

    /* No connect in progress */

    if (addrP == NULL) {
        /* Connect without an address is not allowed
         */
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* Initial connect call, with address */

    if (sock_connect(descP->sock, (struct sockaddr*) addrP, addrLen) == 0) {
        /* Success! */
        SSDBG( descP, ("WIN-ESAIO",
                       "essio_connect_dgram {%d} -> connected\r\n",
                       descP->sock) );

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* Connect returned error */
    save_errno = sock_errno();
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_connect_dgram {%d} -> error: %d\r\n",
            descP->sock, save_errno) );

    return esock_make_error_errno(env, save_errno);

}



/* *** esaio_listen *** */


/* ========================================================================
 */
extern
ERL_NIF_TERM esaio_accept(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     accRef)
{
    ErlNifPid       caller;
    ERL_NIF_TERM    eres;
    SOCKET          accSock;
    ESAIOOperation* opP;
    BOOLEAN_T       ares;
    unsigned int    addrSz, bufSz;
    DWORD           recvBytes;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    /* Ensure that this caller does not already have a
     * (accept) request waiting */
    if (esock_acceptor_search4pid(env, descP, &caller)) {
        /* Acceptor already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* Accept and Read uses the same flag so they can not be simultaneous.
     */
    SSDBG( descP, ("WIN-ESAIO", "esaio_accept {%d} -> verify not reading\r\n",
                   descP->sock) );
    if (descP->readersQ.first != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Should we verify domain, type and protocol? */

    /* Allocate 'operation' */
    SSDBG( descP, ("WIN-ESAIO",
                   "esaio_accept {%d} -> allocate 'operation'\r\n",
                   descP->sock) );
    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_ACCEPT;

    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * How much does this cost?
     */    
    SSDBG( descP, ("WIN-ESAIO",
                   "esaio_accept {%d} -> initiate 'operation'\r\n",
                   descP->sock) );
    opP->env                  = esock_alloc_env("esaio_accept - operation");
    opP->data.accept.lSockRef = CP_TERM(opP->env, sockRef);
    opP->data.accept.accRef   = CP_TERM(opP->env, accRef);
    opP->data.accept.lsock    = descP->sock;
    opP->caller               = caller;

    /* Create the accepting socket
     * domain   - should be AF_INET | AF_INET6 | AF_LOCAL (sould we make sure?)
     * type     - should be SOCK_STREAM | SOCK_SEQPACKET (should we make sure?)
     * protocol - should be IPPROTO_TCP | IPPROTO_SCTP (should we make sure?)
     * See check above!
     */
    SSDBG( descP, ("WIN-ESAIO",
                   "esaio_accept {%d} -> try create 'accepting' socket\r\n",
                   descP->sock) );
    accSock = sock_open(descP->domain, descP->type, descP->protocol);
    if (accSock == INVALID_SOCKET) {
        int          save_errno = sock_errno();
        ERL_NIF_TERM reason     = MKA(env, erl_errno_id(save_errno));
        ERL_NIF_TERM tag        = esock_atom_create_accept_socket;

        esock_clear_env("esaio_accept - invalid accept socket", opP->env);
        esock_free_env("esaio_accept - invalid accept socket", opP->env);
        FREE( opP );

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_accept {%d} -> failed create 'accepting' socket:"
                "\r\n   %T (%d)"
                "\r\n",
                descP->sock, reason, save_errno) );

        return esock_make_error_t2r(env, tag, reason);
    }

    opP->data.accept.asock = accSock;

    /* According to the (Microsoft) documentation, the buffer size of the 
     * local and remote address must be 16 bytes *more* than the size of
     * the sockaddr structure for the transport protocol in use.
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_accept {%d} -> "
            "try calculate address and address buffer size(s)\r\n",
            descP->sock) );
    switch (descP->domain) {
    case AF_INET:
        addrSz = sizeof(struct sockaddr_in) + 16;
        break;
    case AF_INET6:
        addrSz = sizeof(struct sockaddr_in6) + 16;
        break;
    case AF_LOCAL:
        addrSz = sizeof(struct sockaddr_un) + 16;
        break;
    default:
        return esock_make_error_invalid(env, esock_atom_domain);
        break;
    }
    bufSz  = 2 * addrSz;

    opP->data.accept.buf = MALLOC( bufSz );
    ESOCK_ASSERT( opP->data.accept.buf != NULL);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_accept(%T, %d) -> try accept\r\n",
            sockRef, descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_tries, &descP->accTries, 1);

    ares = sock_accept_O(descP->sock, accSock,
                         opP->data.accept.buf,
                         addrSz,
                         &recvBytes,
                         (OVERLAPPED*) opP);

    eres = accept_check_result(env, descP, opP, ares,
                               sockRef, accRef, accSock, caller);

    SSDBG( descP, ("WIN-ESAIO", "esaio_accept(%T, %d) -> done when"
                   "\r\n   eres: %T"
                   "\r\n", sockRef, descP->sock, eres) );

    return eres;
}



static
ERL_NIF_TERM accept_check_result(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ESAIOOperation*  opP,
                                 BOOL             ares,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     accRef,
                                 SOCKET           accSock,
                                 ErlNifPid        caller)
{
    ERL_NIF_TERM eres;

   if (ares) {

        /* Success already!
         * So, no need to store the data (in the "queue").
         * And then allocate and initiate the new descriptor.
         */

       eres = esaio_accept_accepted(env, descP, caller, sockRef, accSock);

    } else {

        /* Accept returned error, check which */

        int save_errno = sock_errno();

        /* As pointed out above, there are basically two kinds of errors: 
         * 1) Pending:
         *    An overlapped operation was successfully initiated.
         *    Completion will be "indicated" at a later time.
         * 2) An actual error
         */

        if (save_errno == WSA_IO_PENDING) {

            /* We need to store the data in the queue! */

            eres = accept_check_pending(env, descP, opP, caller,
                                        sockRef, accRef);

        } else {

            eres = accept_check_fail(env, descP, opP, save_errno,
                                     accSock, sockRef);

        }
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "accept_check_result(%T, %d) -> done with"
            "\r\n  result: %T"
            "\r\n", sockRef, descP->sock, eres) );

    return eres;
}



static
ERL_NIF_TERM accept_check_pending(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ESAIOOperation*  opP,
                                  ErlNifPid        caller,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     accRef)
{
    SSDBG( descP,
           ("WIN-ESAIO",
            "accept_check_pending(%T, %d) -> entry with"
            "\r\n   accRef: %T"
            "\r\n", sockRef, descP->sock, accRef) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_waits, &descP->accWaits, 1);

    if (descP->acceptorsQ.first == NULL)
        descP->readState |= (ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);

    /* Will be picked up by the (worker) threads when the event comes */
    esock_acceptor_push(env, descP, caller, accRef, opP);

    return esock_atom_completion;

}


static
ERL_NIF_TERM accept_check_fail(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ESAIOOperation*  opP,
                               int              saveErrno,
                               SOCKET           accSock,
                               ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason;

    SSDBG( descP,
           ("WIN-ESAIO",
            "accept_check_fail(%T, %d) -> entry with"
            "\r\n   errno:        %d"
            "\r\n   (acc) socket: %d"
            "\r\n", sockRef, descP->sock, saveErrno, accSock) );

    reason = MKA(env, erl_errno_id(saveErrno));

    /* Will an event be generetade in this case?
     * Assume not => We need to clean up here!
     */
    esock_clear_env("esaio_accept", opP->env);
    esock_free_env("esaio_accept", opP->env);
    FREE( opP->data.accept.buf );
    FREE( opP );

    sock_close(accSock);

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_fails, &descP->accFails, 1);

    return esock_make_error(env, reason);
}



/* *** esaio_accept_accepted ***
 *
 * Generic function handling a successful accept.
 */
static
ERL_NIF_TERM esaio_accept_accepted(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ErlNifPid        pid,
                                   ERL_NIF_TERM     sockRef,
                                   SOCKET           accSock)
{
    ESockDescriptor* accDescP;
    ERL_NIF_TERM     accRef;
    int              save_errno;

    /*
     * We got one
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_accept_accepted(%T, %d) -> entry with"
            "\r\n   (accept) socket: %T"
            "\r\n", sockRef, descP->sock, accSock) );

    // Allocate the descriptor
    accDescP = esock_alloc_descriptor(accSock);
    
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_accept_accepted(%T, %d) -> add to completion port\r\n",
            sockRef, descP->sock) );

    if (ESAIO_OK != (save_errno = esaio_add_socket(accDescP))) {
        // See esock_dtor for what needs done!
        ERL_NIF_TERM tag    = esock_atom_add_socket;
        ERL_NIF_TERM reason = MKA(env, erl_errno_id(save_errno));

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_fails, &descP->accFails, 1);

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_accept_accepted(%T, %d) -> "
                "failed adding (accepted) socket to completion port: "
                "%T (%d)\r\n", sockRef, descP->sock, reason, save_errno) );

        esock_dealloc_descriptor(env, accDescP);
        sock_close(accSock);

        /* This should really be:
         *     {error, {invalid, {add_to_completion_port, Reason}}}
         */

        return esock_make_error_t2r(env, tag, reason);
    }

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_success, &descP->accSuccess, 1);

    accDescP->domain   = descP->domain;
    accDescP->type     = descP->type;
    accDescP->protocol = descP->protocol;

    MLOCK(descP->writeMtx);

    accDescP->rBufSz   = descP->rBufSz;  // Inherit buffer size
    accDescP->rCtrlSz  = descP->rCtrlSz; // Inherit buffer size
    accDescP->wCtrlSz  = descP->wCtrlSz; // Inherit buffer size
    accDescP->iow      = descP->iow;     // Inherit iow
    accDescP->dbg      = descP->dbg;     // Inherit debug flag
    accDescP->useReg   = descP->useReg;  // Inherit useReg flag
    esock_inc_socket(accDescP->domain, accDescP->type, accDescP->protocol);

    accRef = enif_make_resource(env, accDescP);
    enif_release_resource(accDescP);

    accDescP->ctrlPid = pid;
    /* pid has actually been compared equal to self()
     * in this code path just a little while ago
     */
    ESOCK_ASSERT( MONP("esaio_accept_accepted -> ctrl",
                       env, accDescP,
                       &accDescP->ctrlPid,
                       &accDescP->ctrlMon) == 0 );

    accDescP->writeState |= ESOCK_STATE_CONNECTED;

    MUNLOCK(descP->writeMtx);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, accRef);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_accept_accepted(%T, %d) -> done\r\n",
            sockRef, descP->sock) );

    return esock_make_ok2(env, accRef);

}



/* ========================================================================
 * Do the actual send.
 * Do some initial writer checks, do the actual send and then
 * analyze the result.
 *
 * The following flags are "valid":
 *
 *        MSG_DONTROUTE, MSG_PARTIAL, and MSG_OOB
 *
 */
extern
ERL_NIF_TERM esaio_send(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     sendRef,
                        ErlNifBinary*    sndDataP,
                        int              flags)
{
    ErlNifPid       caller;
    ERL_NIF_TERM    eres;
    BOOLEAN_T       cleanup = FALSE;
    int             wres;
    DWORD           toWrite;
    char*           buf;
    DWORD           f = (DWORD) flags;
    ESAIOOperation* opP;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->writeState)) {
        ESOCK_EPRINTF("esaio_send(%T, %d) -> NOT OPEN\r\n",
                      sockRef, descP->sock);
        return esock_make_error_closed(env);
    }

    /* Connect and Write can not be simultaneous? */
    if (descP->connectorP != NULL) {
        ESOCK_EPRINTF("esaio_send(%T, %d) -> CONNECTING\r\n",
                      sockRef, descP->sock);
        return esock_make_error_invalid(env, esock_atom_state);
    }

    /* Ensure that this caller does not *already* have a
     * (send) request waiting */
    if (esock_writer_search4pid(env, descP, &caller)) {
        /* Sender already in queue */
        ESOCK_EPRINTF("esaio_send(%T, %d) -> ALREADY SENDING\r\n",
                      sockRef, descP->sock);
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* This is a size check,
     * to ensure we do not try to send something *to* large */
    toWrite = (DWORD) sndDataP->size;
    if ((size_t) toWrite != sndDataP->size)
        return esock_make_error_invalid(env, esock_atom_data_size);    

    /* Once the send function has been called, this memory
     * is "owned" by the system. That is, we cannot free it
     * (or do anything with it) until the *operation* has completed,
     * so the free is done by the thread(s).
     */
    buf = MALLOC( toWrite );
    ESOCK_ASSERT( buf != NULL );
    sys_memcpy(buf, sndDataP->data, toWrite);

    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_SEND;
    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * But after the call is to late, so...
     */
    opP->env                = esock_alloc_env("esaio-send - operation");
    opP->data.send.sendRef  = CP_TERM(opP->env, sendRef);
    opP->data.send.sockRef  = CP_TERM(opP->env, sockRef);
    opP->data.send.wbuf.buf = buf;
    opP->data.send.wbuf.len = toWrite;
    opP->caller             = caller;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    wres = sock_send_O(descP->sock, &opP->data.send.wbuf, f, (OVERLAPPED*) opP);

    eres = send_check_result(env, descP, opP, caller,
                             wres, toWrite, FALSE,
                             sockRef, sendRef, &cleanup);

    if (cleanup) {

        /* "Manually" allocated buffer */
        FREE( opP->data.send.wbuf.buf );

        esock_clear_env("esaio_send - cleanup", opP->env);
        esock_free_env("esaio_send - cleanup", opP->env);

        FREE( opP );

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_send {%d} -> done (%s)"
            "\r\n   %T"
            "\r\n", descP->sock, B2S(cleanup), eres) );

    return eres;
}



/* ========================================================================
 * Do the actual send.
 * Do some initial writer checks, do the actual send and then
 * analyze the result.
 *
 * The following flags are "valid":
 *
 *        MSG_DONTROUTE, MSG_PARTIAL, and MSG_OOB
 *
 * "Explicit binding is discouraged for client applications."
 */

extern
ERL_NIF_TERM esaio_sendto(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     sendRef,
                          ErlNifBinary*    sndDataP,
                          int              flags,
                          ESockAddress*    toAddrP,
                          SOCKLEN_T        toAddrLen)
{
    ErlNifPid       caller;
    ERL_NIF_TERM    eres;
    BOOLEAN_T       cleanup = FALSE;
    int             wres;
    DWORD           toWrite;
    char*           buf;
    DWORD           f = (DWORD) flags;
    ESAIOOperation* opP;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write can not be simultaneous? */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Empty address to allowed */
    if (toAddrP == NULL)
        return esock_make_invalid(env, esock_atom_sockaddr);

    /* Ensure that this caller does not *already* have a
     * (send) request waiting */
    if (esock_writer_search4pid(env, descP, &caller)) {
        /* Sender already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* This is a size check,
     * to ensure we do not try to send something *to* large */
    toWrite = (DWORD) sndDataP->size;
    if ((size_t) toWrite != sndDataP->size)
        return esock_make_error_invalid(env, esock_atom_data_size);    

    /* Once the send function has been called, this memory
     * (buf) "belongs" to the "system" (so no need to free it).
     */
    buf = MALLOC( toWrite );
    ESOCK_ASSERT( buf != NULL );
    sys_memcpy(buf, sndDataP->data, toWrite);

    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_SENDTO;
    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * But after the call is to late, so...
     */
    opP->env = esock_alloc_env("esaio-sendto - operation");
    opP->data.sendto.sendRef       = CP_TERM(opP->env, sendRef);
    opP->data.sendto.sockRef       = CP_TERM(opP->env, sockRef);
    opP->data.sendto.wbuf.buf      = buf;
    opP->data.sendto.wbuf.len      = toWrite;
    opP->data.sendto.remoteAddr    = *toAddrP; // Do we need this?
    opP->data.sendto.remoteAddrLen = toAddrLen;// Do we need this?
    opP->caller                    = caller;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    wres = sock_sendto_O(descP->sock, &opP->data.sendto.wbuf, f,
                         (struct sockaddr*) toAddrP, toAddrLen,
                         (OVERLAPPED*) opP);

    eres = send_check_result(env, descP, opP, caller,
                             wres, toWrite, FALSE,
                             sockRef, sendRef, &cleanup);

    if (cleanup) {

        /* "Manually" allocated buffer */
        FREE( opP->data.sendto.wbuf.buf );

        esock_clear_env("esaio_sendto - cleanup", opP->env);
        esock_free_env("esaio_sendto - cleanup", opP->env);

        FREE( opP );

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendto {%d} -> done (%s)"
            "\r\n   %T"
            "\r\n", descP->sock, B2S(cleanup), eres) );

    return eres;
}



/* ========================================================================
 * Do the actual sendmsg.
 * Do some initial writer checks, do the actual send and then
 * analyze the result.
 *
 * The following flags are "valid":
 *
 *        MSG_DONTROUTE, MSG_PARTIAL, and MSG_OOB
 *
 * "Explicit binding is discouraged for client applications."
 *
 * Also, according to Microsoft documentation:
 *
 *      "can only be used with datagrams and raw sockets."
 *
 * So, should we check, or let the user crash and burn?
 *
 * Note that this operation *only* works for socket
 * of types SOCK_DGRAM and SOCK_RAW! Should we check
 * and throw 'enotsup' otherwise? Would make testing
 * easier...
 */

extern
ERL_NIF_TERM esaio_sendmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     sendRef,
                           ERL_NIF_TERM     eMsg,
                           int              flags,
                           ERL_NIF_TERM     eIOV,
                           const ESockData* dataP)
{
    ErlNifPid       caller;
    ERL_NIF_TERM    eres;
    BOOLEAN_T       cleanup = FALSE;
    int             wres;
    ERL_NIF_TERM    tail;
    ERL_NIF_TERM    eAddr, eCtrl;
    ssize_t         dataSize;
    size_t          ctrlBufLen,  ctrlBufUsed;
    WSABUF*         wbufs = NULL;
    ESAIOOperation* opP   = NULL;

    SSDBG( descP, ("WIN-ESAIO", "esaio_sendmsg(%T, %d) -> entry with"
                   "\r\n", sockRef, descP->sock) );

    /* This *only* works on socket type(s) DGRAM or RAW.
     * Other socket types results in einval, which is not very
     * helpful. So, in order to, atleast, help with testing,
     * we do this...
     */
    if (! ((descP->type == SOCK_DGRAM) || (descP->type == SOCK_RAW))) {
        return enif_raise_exception(env, MKA(env, "notsup"));
    }

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write can not be simultaneous? */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Ensure that this caller does not *already* have a
     * (send) request waiting */
    if (esock_writer_search4pid(env, descP, &caller)) {
        /* Sender already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }

    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_SENDMSG;

    if (! init_sendmsg_sockaddr(env, descP, eMsg,
                                &opP->data.sendmsg.msg,
                                &opP->data.sendmsg.addr)) {

        FREE( opP );

        return esock_make_invalid(env, esock_atom_addr);
    }

    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * How much does this cost?
     */
    opP->env = esock_alloc_env("esaio_sendmsg - operation");

    /* Extract the *mandatory* 'iov', which must be an erlang:iovec(),
     * from which we take at most IOV_MAX binaries.
     * The env *cannot* be NULL because we don't actually know if 
     * the send succeeds *now*. It could be sceduled!
     */
    if ((! enif_inspect_iovec(opP->env,
                              dataP->iov_max, eIOV, &tail,
                              &opP->data.sendmsg.iovec))) {

        SSDBG( descP, ("WIN-ESAIO",
                       "essaio_sendmsg {%d} -> not an iov\r\n",
                       descP->sock) );

        esock_free_env("esaio-sendmsg - iovec failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_sendmsg {%d} ->"
                   "\r\n   iovcnt: %lu"
                   "\r\n   tail:   %s"
                   "\r\n", descP->sock,
                   (unsigned long) opP->data.sendmsg.iovec->iovcnt,
                   B2S(! enif_is_empty_list(opP->env, tail))) );


    /* We now have an allocated iovec - verify vector size */

    if (! verify_sendmsg_iovec_size(dataP, descP, opP->data.sendmsg.iovec)) {

        /* We can not send the whole packet in one sendmsg() call */
        SSDBG( descP, ("WIN-ESAIO",
                       "esaio_sendmsg {%d} -> iovcnt > iov_max\r\n",
                       descP->sock) );

        // No need - belongs to op env: FREE_IOVEC( opP->data.sendmsg.iovec );
        esock_free_env("esaio-sendmsg - iovec failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);
    }


    /* Verify that we can send the entire message.
     * On DGRAM the tail must be "empty" (= everything must fit in one message).
     */
    if (! verify_sendmsg_iovec_tail(opP->env, descP, &tail)) {

        // No need - belongs to op env: FREE_IOVEC( opP->data.sendmsg.iovec );
        esock_free_env("esaio-sendmsg - iovec tail failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);

    }
    
    if (! check_sendmsg_iovec_overflow(descP,
                                       opP->data.sendmsg.iovec, &dataSize)) {

        // No need - belongs to op env: FREE_IOVEC( opP->data.sendmsg.iovec );
        esock_free_env("esaio-sendmsg - iovec size failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);

    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_sendmsg {%d} -> iovec size verified"
            "\r\n   iov length: %lu"
            "\r\n   data size:  %u"
            "\r\n",
            descP->sock,
            (unsigned long) opP->data.sendmsg.iovec->iovcnt,
            (long) dataSize) );

    wbufs = MALLOC(opP->data.sendmsg.iovec->iovcnt * sizeof(WSABUF));
    ESOCK_ASSERT( wbufs != NULL );
    for (int i = 0; i < opP->data.sendmsg.iovec->iovcnt; i++) {
        wbufs[i].len = opP->data.sendmsg.iovec->iov[i].iov_len;
        wbufs[i].buf = opP->data.sendmsg.iovec->iov[i].iov_base;
    }
    
    opP->data.sendmsg.msg.lpBuffers     = wbufs;
    opP->data.sendmsg.msg.dwBufferCount = opP->data.sendmsg.iovec->iovcnt;

    /* And now for the control headers - some default first */
    eCtrl                     = esock_atom_undefined;
    ctrlBufLen                = 0;
    opP->data.sendmsg.ctrlBuf = NULL;

    /* Extract the *optional* 'ctrl' out of the eMsg map */
    if (GET_MAP_VAL(env, eMsg, esock_atom_ctrl, &eCtrl)) {
        ctrlBufLen                = descP->wCtrlSz;
        opP->data.sendmsg.ctrlBuf = (char*) MALLOC(ctrlBufLen);
        ESOCK_ASSERT( opP->data.sendmsg.ctrlBuf != NULL );
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_sendmsg {%d} -> optional ctrl: "
                   "\r\n   ctrlBuf:    %p"
                   "\r\n   ctrlBufLen: %lu"
                   "\r\n   eCtrl:      %T"
                   "\r\n", descP->sock,
                   opP->data.sendmsg.ctrlBuf,
                   (unsigned long) ctrlBufLen, eCtrl) );

    if (opP->data.sendmsg.ctrlBuf != NULL) {
        if (! decode_cmsghdrs(env, descP,
                              eCtrl,
                              opP->data.sendmsg.ctrlBuf, ctrlBufLen,
                              &ctrlBufUsed)) {

            FREE( opP->data.sendmsg.ctrlBuf );
            FREE( opP->data.sendmsg.msg.lpBuffers );
            // No need - belongs to op env: FREE_IOVEC( opP->data.sendmsg.iovec );
            esock_free_env("esaio-sendmsg - iovec size failure", opP->env);
            FREE( opP );

            return esock_make_invalid(env, esock_atom_ctrl);
        }
    } else {
         ctrlBufUsed = 0;
    }
    opP->data.sendmsg.msg.Control.len = ctrlBufUsed;
    opP->data.sendmsg.msg.Control.buf = opP->data.sendmsg.ctrlBuf;

    /* We do not yet handle the flags (see function header above),
     * so zero it just in case. */
    opP->data.sendmsg.msg.dwFlags = 0;

    opP->tag                  = ESAIO_OP_SENDMSG;
    opP->caller               = caller;
    opP->data.sendmsg.sockRef = CP_TERM(opP->env, sockRef);
    opP->data.sendmsg.sendRef = CP_TERM(opP->env, sendRef);

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    wres = sock_sendmsg_O(descP->sock, &opP->data.sendmsg.msg, flags,
                          (OVERLAPPED*) opP);

    eres = send_check_result(env, descP, opP, caller,
                             wres, dataSize,
                             (! enif_is_empty_list(opP->env, tail)),
                             sockRef, sendRef, &cleanup);

    if (cleanup) {
        
        /* "Manually" allocated buffers */
        FREE( opP->data.sendmsg.msg.lpBuffers );
        if (opP->data.sendmsg.ctrlBuf != NULL)
            FREE( opP->data.sendmsg.ctrlBuf );

        /* The i/o vector belongs to the op env,
         * so it goes when the env goes.
         */
        esock_clear_env("esaio_sendmsg - cleanup", opP->env);
        esock_free_env("esaio_sendmsg - cleanup", opP->env);

        FREE( opP );

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendmsg {%d} -> done (%s)"
            "\r\n   %T"
            "\r\n", descP->sock, B2S(cleanup), eres) );

    return eres;
}


static
BOOLEAN_T init_sendmsg_sockaddr(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eMsg,
                                WSAMSG*          msgP,
                                ESockAddress*    addrP)
{
    ERL_NIF_TERM eAddr;

    if (! GET_MAP_VAL(env, eMsg, esock_atom_addr, &eAddr)) {

        SSDBG( descP, ("WIN-ESAIO",
                       "init_sendmsg_sockaddr {%d} -> no address\r\n",
                       descP->sock) );

        msgP->name    = NULL;
        msgP->namelen = 0;

    } else {

        SSDBG( descP, ("WIN-ESAIO", "init_sendmsg_sockaddr {%d} ->"
                       "\r\n   address: %T"
                       "\r\n", descP->sock, eAddr) );

        msgP->name    = (void*) addrP;
        msgP->namelen = sizeof(ESockAddress);
        sys_memzero((char *) msgP->name, msgP->namelen);

        if (! esock_decode_sockaddr(env, eAddr,
                                    (ESockAddress*) msgP->name,
                                    (SOCKLEN_T*) &msgP->namelen)) {

            SSDBG( descP, ("WIN-ESAIO",
                           "init_sendmsg_sockaddr {%d} -> invalid address\r\n",
                           descP->sock) );

            return FALSE;
        }
    }

    return TRUE;

}
                             


static
BOOLEAN_T verify_sendmsg_iovec_size(const ESockData* dataP,
                                    ESockDescriptor* descP,
                                    ErlNifIOVec*     iovec)
{
    if (iovec->iovcnt > dataP->iov_max) {
        if (descP->type == SOCK_STREAM) {
            iovec->iovcnt = dataP->iov_max;
        } else {
            return FALSE;
        }
    }

    return TRUE;
}



static
BOOLEAN_T verify_sendmsg_iovec_tail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM*    tail)
{
    ERL_NIF_TERM h, t, tmp = *tail;
    ErlNifBinary bin;

    /* Find out if there is remaining data in the tail.
     * Skip empty binaries otherwise break.
     * If 'tail' after loop exit is the empty list
     * there was no more data.  Otherwise there is more
     * data or the 'iov' is invalid.
     */

    for (;;) {
        if (enif_get_list_cell(env, tmp, &h, &t) &&
            enif_inspect_binary(env, h, &bin) &&
            (bin.size == 0)) {
            tmp = t;
            continue;
        } else
            break;
    }

    *tail = tmp;

    if ((! enif_is_empty_list(env, tmp)) &&
        (descP->type != SOCK_STREAM)) {

        /* We can not send the whole packet in one sendmsg() call */
        SSDBG( descP, ("WIN-ESAIO",
                       "verify_sendmsg_iovec_tail {%d} -> invalid tail\r\n",
                       descP->sock) );

        return FALSE;
    }

    return TRUE;
    
}



static
BOOLEAN_T check_sendmsg_iovec_overflow(ESockDescriptor* descP,
                                       ErlNifIOVec*     iovec,
                                       ssize_t*         dataSize)
{
    ssize_t dsz = 0;
    size_t  i;

    for (i = 0;  i < iovec->iovcnt;  i++) {
        size_t len = iovec->iov[i].iov_len;
        dsz += len;
        if (dsz < len) {

            /* Overflow */

            SSDBG( descP, ("WIN-ESAIO",
                           "check_sendmsg_iovec_overflow {%d} -> Overflow"
                           "\r\n   i:         %lu"
                           "\r\n   len:       %lu"
                           "\r\n   dataSize:  %ld"
                           "\r\n", descP->sock, (unsigned long) i,
                           (unsigned long) len, (long) dsz) );

            *dataSize = dsz;

            return FALSE;

        }
    }

    *dataSize = dsz;

    return TRUE;
    
}



/* ========================================================================
 * Do the actual sendv.
 * Do some initial writer checks, do the actual send and then
 * analyze the result.
 */
extern
ERL_NIF_TERM esaio_sendv(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ERL_NIF_TERM     sockRef,
                         ERL_NIF_TERM     sendRef,
                         ERL_NIF_TERM     eIOV,
                         const ESockData* dataP)
{
    ErlNifPid       caller;
    ERL_NIF_TERM    eres;
    ESockAddress    addr;
    ERL_NIF_TERM    tail;
    ssize_t         dataSize, sendv_result;
    BOOLEAN_T       dataInTail, cleanup;
    ESAIOOperation* opP   = NULL;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> get caller\r\n",
            descP->sock) );

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> check state\r\n",
            descP->sock) );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> check if connecting\r\n",
            descP->sock) );

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);


    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> check if already writing\r\n",
            descP->sock) );

    /* Ensure that this caller does not *already* have a
     * (send) request waiting */
    if (esock_writer_search4pid(env, descP, &caller)) {
        /* Sender already in queue */
        ESOCK_EPRINTF("esaio_send(%T, %d) -> ALREADY SENDING\r\n",
                      sockRef, descP->sock);
        return esock_raise_invalid(env, esock_atom_state);
    }


    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> allocations\r\n",
            descP->sock) );

    /* Allocate the operation */
    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_SENDV;

    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * How much does this cost?
     */
    opP->env = esock_alloc_env("esaio_sendv - operation");

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> extract I/O vector\r\n",
            descP->sock) );

    /* Extract the 'iov', which must be an erlang:iovec(),
     * from which we take at most IOV_MAX binaries.
     * The env *cannot* be NULL because we don't actually know if 
     * the send succeeds *now*. It could be sceduled!
     */
    if (! enif_inspect_iovec(opP->env,
                             dataP->iov_max, eIOV, &tail,
                             &opP->data.sendv.iovec)) {

        SSDBG( descP, ("WIN-ESAIO",
                       "esaio_sendv {%d} -> iov inspection failed\r\n",
                       descP->sock) );

        esock_free_env("esaio-sendv - iovec inspection failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);
    }

    if (opP->data.sendv.iovec == NULL) {

        SSDBG( descP, ("UNIX-ESSIO",
                       "esaio_sendv {%d} -> not an iov\r\n",
                       descP->sock) );

        esock_free_env("esaio-sendv - iovec failure", opP->env);
        FREE( opP );

        return esock_make_invalid(env, esock_atom_iov);
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> check if data in tail\r\n",
            descP->sock) );

    dataInTail = (! enif_is_empty_list(opP->env, tail));

    SSDBG( descP, ("WIN-ESAIO", "esaio_sendv(%d) -> verify iovec size when"
                   "\r\n   iovcnt: %lu"
                   "\r\n   tail:   %s"
                   "\r\n", descP->sock,
                   (unsigned long) opP->data.sendv.iovec->iovcnt,
                   B2S(dataInTail)) );

    /* We now have an allocated iovec - verify vector size */

    if (! verify_sendmsg_iovec_size(dataP, descP, opP->data.sendv.iovec)) {

        /* We can not send the whole packet in one sendv() call */
        SSDBG( descP, ("WIN-ESAIO",
                       "esaio_sendv {%d} -> iovcnt > iov_max\r\n",
                       descP->sock) );

        // No need - belongs to op env: FREE_IOVEC( opP->data.send.iovec );
        esock_free_env("esaio-sendv - iovec failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> verify iovec tail\r\n",
            descP->sock) );

    /* Verify that we can send the entire message.
     * On DGRAM the tail must be "empty" (= everything must fit in one message).
     */
    if (! verify_sendmsg_iovec_tail(opP->env, descP, &tail)) {

        // No need - belongs to op env: FREE_IOVEC( opP->data.send.iovec );
        esock_free_env("esaio-sendv - iovec tail failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);

    }
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> check iovec overflow\r\n",
            descP->sock) );

    if (! check_sendmsg_iovec_overflow(descP,
                                       opP->data.sendv.iovec, &dataSize)) {

        // No need - belongs to op env: FREE_IOVEC( opP->data.sendv.iovec );
        esock_free_env("esaio-sendv - iovec size failure", opP->env);
        FREE( opP );

        return esock_make_error_invalid(env, esock_atom_iov);

    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_sendv {%d} -> iovec size verified"
            "\r\n   iov length: %lu"
            "\r\n   data size:  %u"
            "\r\n",
            descP->sock,
            (unsigned long) opP->data.sendv.iovec->iovcnt,
            (long) dataSize) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    /* And now, try to send the message */
    sendv_result = sock_sendv_O(descP->sock,
                                (LPWSABUF) opP->data.sendv.iovec->iov,
                                opP->data.sendv.iovec->iovcnt,
                                (OVERLAPPED*) opP);

    eres = send_check_result(env, descP, opP, caller,
                             sendv_result, dataSize, dataInTail,
                             sockRef, sendRef, &cleanup);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv(%d) -> sent and analyzed: %d, %T\r\n",
            descP->sock, sendv_result, eres) );

    if (cleanup) {
        
        /* The i/o vector belongs to the op env,
         * so it goes when the env goes.
         */
        esock_clear_env("esaio_sendv - cleanup", opP->env);
        esock_free_env("esaio_sendv - cleanup", opP->env);

        FREE( opP );

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_sendv {%d} -> done (%s)"
            "\r\n   %T"
            "\r\n", descP->sock, B2S(cleanup), eres) );

    return eres;

}



/* *** Control message utility functions *** */

/* +++ decode_cmsghdrs +++
 *
 * Decode a list of cmsg(). There can be 0 or more "blocks".
 *
 * Each element can either be a (erlang) map that needs to be decoded,
 * or a (erlang) binary that just needs to be appended to the control
 * buffer.
 *
 * Our "problem" is that we have no idea how much memory we actually need.
 *
 */

static
BOOLEAN_T decode_cmsghdrs(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     eCMsg,
                          char*            cmsgHdrBufP,
                          size_t           cmsgHdrBufLen,
                          size_t*          cmsgHdrBufUsed)
{
    ERL_NIF_TERM elem, tail, list;
    char*        bufP;
    size_t       rem, used, totUsed = 0;
    unsigned int len;
    int          i;

    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdrs {%d} -> entry with"
                   "\r\n   eCMsg:      %T"
                   "\r\n   cmsgHdrBufP:   0x%lX"
                   "\r\n   cmsgHdrBufLen: %d"
                   "\r\n", descP->sock,
                   eCMsg, cmsgHdrBufP, cmsgHdrBufLen) );

    if (! GET_LIST_LEN(env, eCMsg, &len))
        return FALSE;

    SSDBG( descP,
           ("WIN-ESAIO",
            "decode_cmsghdrs {%d} -> list length: %d\r\n",
            descP->sock, len) );

    for (i = 0, list = eCMsg, rem  = cmsgHdrBufLen, bufP = cmsgHdrBufP;
         i < len; i++) {
            
        SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdrs {%d} -> process elem %d:"
                       "\r\n   (buffer) rem:     %u"
                       "\r\n   (buffer) totUsed: %u"
                       "\r\n", descP->sock, i, rem, totUsed) );

        /* Extract the (current) head of the (cmsg hdr) list */
        if (! GET_LIST_ELEM(env, list, &elem, &tail))
            return FALSE;
            
        used = 0; // Just in case...
        if (! decode_cmsghdr(env, descP, elem, bufP, rem, &used))
            return FALSE;

#ifdef __WIN32__
        bufP     = CHARP( bufP + used );
#else
        bufP     = CHARP( ULONG(bufP) + used );
#endif
        rem      = SZT( rem - used );
        list     = tail;
        totUsed += used;

    }

    *cmsgHdrBufUsed = totUsed;

    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdrs {%d} -> done"
                   "\r\n   all %u ctrl headers processed"
                   "\r\n   totUsed = %lu\r\n",
                   descP->sock, len, (unsigned long) totUsed) );

    return TRUE;
}



/* +++ decode_cmsghdr +++
 *
 * Decode one cmsg(). Put the "result" into the buffer and advance the
 * pointer (of the buffer) afterwards. Also update 'rem' accordingly.
 * But before the actual decode, make sure that there is enough room in 
 * the buffer for the cmsg header (sizeof(*hdr) < rem).
 *
 * The eCMsg should be a map with three fields:
 *
 *     level :: socket | protocol() | integer()
 *     type  :: atom() | integer()
 *                                What values are valid depend on the level
 *     data  :: binary() | integer() | boolean()
 *                                The type of the data depends on
 *     or                         level and type, but can be a binary,
 *                                which means that the data is already coded.
 *     value :: term()            Which is a term matching the decode function
 */

static
BOOLEAN_T decode_cmsghdr(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ERL_NIF_TERM     eCMsg,
                         char*            bufP,
                         size_t           rem,
                         size_t*          used)
{
    ERL_NIF_TERM eLevel, eType, eData, eValue;
    int          level;

    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d} -> entry with"
                   "\r\n   eCMsg: %T"
                   "\r\n", descP->sock, eCMsg) );

    // Get 'level' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_level, &eLevel))
        return FALSE;
    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d} -> eLevel: %T"
                   "\r\n", descP->sock, eLevel) );

    // Get 'type' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_type, &eType))
        return FALSE;
    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d} -> eType:  %T"
                   "\r\n", descP->sock, eType) );

    // Decode Level
    if (! esock_decode_level(env, eLevel, &level))
        return FALSE;
    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d}-> level:  %d\r\n",
                   descP->sock, level) );

    // Get 'data' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_data, &eData)) {

        // Get 'value' field
        if (! GET_MAP_VAL(env, eCMsg, esock_atom_value, &eValue))
            return FALSE;
        SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d} -> eValue:  %T"
                   "\r\n", descP->sock, eValue) );

        // Decode Value
        if (! decode_cmsghdr_value(env, descP, level, eType, eValue,
                                   bufP, rem, used))
            return FALSE;

    } else {

        // Verify no 'value' field
        if (GET_MAP_VAL(env, eCMsg, esock_atom_value, &eValue))
            return FALSE;

        SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d} -> eData:  %T"
                   "\r\n", descP->sock, eData) );

        // Decode Data
        if (! decode_cmsghdr_data(env, descP, level, eType, eData,
                                  bufP, rem, used))
            return FALSE;
    }

    SSDBG( descP, ("WIN-ESAIO", "decode_cmsghdr {%d}-> used:  %lu\r\n",
                   descP->sock, (unsigned long) *used) );

    return TRUE;
}


static
BOOLEAN_T decode_cmsghdr_value(ErlNifEnv*   env,
                               ESockDescriptor* descP,
                               int          level,
                               ERL_NIF_TERM eType,
                               ERL_NIF_TERM eValue,
                               char*        bufP,
                               size_t       rem,
                               size_t*      usedP)
{
    int type;
    struct cmsghdr* cmsgP     = (struct cmsghdr *) bufP;
    ESockCmsgSpec*  cmsgTable;
    ESockCmsgSpec*  cmsgSpecP = NULL;
    size_t          num       = 0;

    SSDBG( descP,
           ("WIN-ESAIO",
            "decode_cmsghdr_value {%d} -> entry  \r\n"
            "   eType:  %T\r\n"
            "   eValue: %T\r\n",
            descP->sock, eType, eValue) );

    // We have decode functions only for symbolic (atom) types
    if (! IS_ATOM(env, eType)) {
        SSDBG( descP,
               ("WIN-ESAIO",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   eType not an atom\r\n",
                descP->sock) );
        return FALSE;
    }

    /* Try to look up the symbolic type
     */
    if (((cmsgTable = esock_lookup_cmsg_table(level, &num)) == NULL) ||
        ((cmsgSpecP = esock_lookup_cmsg_spec(cmsgTable, num, eType)) == NULL) ||
        (cmsgSpecP->decode == NULL)) {

        /* We found no table for this level,
         * we found no symbolic type in the level table,
         * or no decode function for this type
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   cmsgTable:  %p\r\n"
                "   cmsgSpecP:  %p\r\n",
                descP->sock, cmsgTable, cmsgSpecP) );

        return FALSE;
    }

    if (! cmsgSpecP->decode(env, eValue, cmsgP, rem, usedP)) {
        // Decode function failed
        SSDBG( descP,
               ("WIN-ESAIO",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   decode function failed\r\n",
                descP->sock) );
        return FALSE;
    }

    // Successful decode

    type = cmsgSpecP->type;

    SSDBG( descP,
           ("WIN-ESAIO",
            "decode_cmsghdr_value {%d} -> TRUE:\r\n"
            "   level:   %d\r\n"
            "   type:    %d\r\n",
            "   *usedP:  %lu\r\n",
            descP->sock, level, type, (unsigned long) *usedP) );

    cmsgP->cmsg_level = level;
    cmsgP->cmsg_type = type;
    return TRUE;
}


static
BOOLEAN_T decode_cmsghdr_data(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              level,
                              ERL_NIF_TERM     eType,
                              ERL_NIF_TERM     eData,
                              char*            bufP,
                              size_t           rem,
                              size_t*          usedP)
{
    int             type;
    ErlNifBinary    bin;
    struct cmsghdr* cmsgP     = (struct cmsghdr *) bufP;
    ESockCmsgSpec*  cmsgSpecP = NULL;

    SSDBG( descP,
           ("WIN-ESAIO",
            "decode_cmsghdr_data {%d} -> entry  \r\n"
            "   eType: %T\r\n"
            "   eData: %T\r\n",
            descP->sock, eType, eData) );

    // Decode Type
    if (! GET_INT(env, eType, &type)) {
        ESockCmsgSpec* cmsgTable = NULL;
        size_t         num       = 0;

        /* Try to look up the symbolic (atom) type
         */
        if ((! IS_ATOM(env, eType)) ||
            ((cmsgTable = esock_lookup_cmsg_table(level, &num)) == NULL) ||
            ((cmsgSpecP = esock_lookup_cmsg_spec(cmsgTable, num, eType)) == NULL)) {
            /* Type was not an atom,
             * we found no table for this level,
             * or we found no symbolic type in the level table
             */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "decode_cmsghdr_data {%d} -> FALSE:\r\n"
                    "   cmsgTable:  %p\r\n"
                    "   cmsgSpecP:  %p\r\n",
                    descP->sock, cmsgTable, cmsgSpecP) );
            return FALSE;
        }

        type = cmsgSpecP->type;
    }

    // Decode Data
    if (GET_BIN(env, eData, &bin)) {
        void *p;

        p = esock_init_cmsghdr(cmsgP, rem, bin.size, usedP);
        if (p == NULL) {
            /* No room for the data
             */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "decode_cmsghdr_data {%d} -> FALSE:\r\n"
                    "   rem:      %lu\r\n"
                    "   bin.size: %lu\r\n",
                    descP->sock,
                    (unsigned long) rem,
                    (unsigned long) bin.size) );
            return FALSE;
        }

        // Copy the binary data
        sys_memcpy(p, bin.data, bin.size);

    } else if ((! esock_cmsg_decode_int(env, eData, cmsgP, rem, usedP)) &&
               (! esock_cmsg_decode_bool(env, eData, cmsgP, rem, usedP))) {
        SSDBG( descP,
               ("WIN-ESAIO",
                "decode_cmsghdr_data {%d} -> FALSE\r\n",
                descP->sock) );
        return FALSE;
    }

    // Successful decode

    SSDBG( descP,
           ("WIN-ESAIO",
            "decode_cmsghdr_data {%d} -> TRUE:\r\n"
            "   level:   %d\r\n"
            "   type:    %d\r\n"
            "   *usedP:  %lu\r\n",
            descP->sock, level, type, (unsigned long) *usedP) );

    cmsgP->cmsg_level = level;
    cmsgP->cmsg_type = type;
    return TRUE;
}




/* +++ encode_msg +++
 *
 * Encode a msg() (recvmsg). In erlang its represented as
 * a map, which has a specific set of attributes:
 *
 *     addr (source address) - sockaddr()
 *     iov                   - [binary()]
 *     ctrl                  - [cmsg()]
 *     flags                 - msg_flags()
 */

static
void encode_msg(ErlNifEnv*       env,
                ESockDescriptor* descP,
                ssize_t          read,
                WSAMSG*          msgP,
                ErlNifBinary*    dataBufP,
                ErlNifBinary*    ctrlBufP,
                ERL_NIF_TERM*    eMsg)
{
    ERL_NIF_TERM addr, iov, ctrl, flags;

    SSDBG( descP,
           ("WIN-ESAIO", "encode_msg {%d} -> entry with"
            "\r\n   read: %ld"
            "\r\n", descP->sock, (long) read) );

    /* The address is not used if we are connected (unless, maybe,
     * family is 'local'), so check (length = 0) before we try to encodel
     */
    if (msgP->namelen != 0) {
        esock_encode_sockaddr(env,
                              (ESockAddress*) msgP->name,
                              msgP->namelen,
                              &addr);
    } else {
        addr = esock_atom_undefined;
    }

    SSDBG( descP,
           ("WIN-ESAIO", "encode_msg {%d} -> encode iov"
            "\r\n   num vectors: %lu"
            "\r\n", descP->sock, (unsigned long) msgP->dwBufferCount) );

    esock_encode_iov(env, read,
                     (SysIOVec*) msgP->lpBuffers, msgP->dwBufferCount, dataBufP,
                     &iov);

    SSDBG( descP,
           ("WIN-ESAIO",
            "encode_msg {%d} -> try encode cmsgs\r\n",
            descP->sock) );

    encode_cmsgs(env, descP, ctrlBufP, msgP, &ctrl);

    SSDBG( descP,
           ("WIN-ESAIO",
            "encode_msg {%d} -> try encode flags\r\n",
            descP->sock) );

    esock_encode_msg_flags(env, descP, msgP->dwFlags, &flags);

    SSDBG( descP,
           ("WIN-ESAIO", "encode_msg {%d} -> components encoded:"
            "\r\n   addr:  %T"
            "\r\n   ctrl:  %T"
            "\r\n   flags: %T"
            "\r\n", descP->sock, addr, ctrl, flags) );

    {
        ERL_NIF_TERM keys[]  = {esock_atom_iov,
                                esock_atom_ctrl,
                                esock_atom_flags,
                                esock_atom_addr};
        ERL_NIF_TERM vals[]  = {iov, ctrl, flags, addr};
        size_t       numKeys = NUM(keys);
        
        ESOCK_ASSERT( numKeys == NUM(vals) );
        
        SSDBG( descP,
               ("WIN-ESAIO",
                "encode_msg {%d} -> create map\r\n",
                descP->sock) );

        if (msgP->namelen == 0)
            numKeys--; // No addr
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eMsg) );

        SSDBG( descP,
               ("WIN-ESAIO",
                "encode_msg {%d}-> map encoded\r\n",
                descP->sock) );
    }

    SSDBG( descP,
           ("WIN-ESAIO", "encode_msg {%d} -> done\r\n", descP->sock) );
}



/* +++ encode_cmsgs +++
 *
 * Encode a list of cmsg(). There can be 0 or more cmsghdr "blocks".
 *
 * Our "problem" is that we have no idea how many control messages
 * we have.
 *
 * The cmsgHdrP arguments points to the start of the control data buffer,
 * an actual binary. Its the only way to create sub-binaries. So, what we
 * need to continue processing this is to turn that into an binary erlang 
 * term (which can then in turn be turned into sub-binaries).
 *
 * We need the cmsgBufP (even though cmsgHdrP points to it) to be able
 * to create sub-binaries (one for each cmsg hdr).
 *
 * The TArray (term array) is created with the size of 128, which should
 * be enough. But if its not, then it will be automatically realloc'ed during
 * add. Once we are done adding hdr's to it, we convert the tarray to a list.
 */

static
void encode_cmsgs(ErlNifEnv*       env,
                  ESockDescriptor* descP,
                  ErlNifBinary*    cmsgBinP,
                  WSAMSG*          msgP,
                  ERL_NIF_TERM*    eCMsg)
{
    ERL_NIF_TERM ctrlBuf  = MKBIN(env, cmsgBinP); // The *entire* binary
    SocketTArray cmsghdrs = TARRAY_CREATE(128);
    WSACMSGHDR*  firstP   = ESOCK_CMSG_FIRSTHDR(msgP);
    WSACMSGHDR*  currentP;
    
    SSDBG( descP, ("WIN-ESAIO", "encode_cmsgs {%d} -> entry when"
                   "\r\n   msg ctrl len:  %d"
                   "\r\n   (ctrl) firstP: 0x%lX"
                   "\r\n", descP->sock, msgP->Control.len, firstP) );

    for (currentP = firstP;
         (currentP != NULL);
         /* nifs\win32\win_socket_asyncio.c(3167):
          * warning C4116: unnamed type definition in parentheses
          */
#pragma warning(disable:4116)
         currentP = ESOCK_CMSG_NXTHDR(msgP, currentP)) {
#pragma warning(default:4116)

        SSDBG( descP,
               ("WIN-ESAIO", "encode_cmsgs {%d} -> process cmsg header when"
                "\r\n   TArray Size: %d"
                "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

        /* MUST check this since on Linux the returned "cmsg" may actually
         * go too far!
         */
        if (((CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP)) >
            msgP->Control.len) {

            /* Ouch, fatal error - give up 
             * We assume we cannot trust any data if this is wrong.
             */

            SSDBG( descP,
                   ("WIN-ESAIO", "encode_cmsgs {%d} -> check failed when: "
                    "\r\n   currentP:           0x%lX"
                    "\r\n   (current) cmsg_len: %d"
                    "\r\n   firstP:             0x%lX"
                    "\r\n   =>                  %d"
                    "\r\n   msg ctrl len:       %d"
                    "\r\n", descP->sock,
                    CHARP(currentP), currentP->cmsg_len, CHARP(firstP),
                    (CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP),
                    msgP->Control.len) );

            TARRAY_ADD(cmsghdrs, esock_atom_bad_data);
            break;
            
        } else {
            unsigned char* dataP   = UCHARP(ESOCK_CMSG_DATA(currentP));
            size_t         dataPos = dataP - cmsgBinP->data;
            size_t         dataLen =
                (UCHARP(currentP) + currentP->cmsg_len) - dataP;
            ERL_NIF_TERM
                cmsgHdr,
                keys[]  =
                {esock_atom_level,
                 esock_atom_type,
                 esock_atom_data,
                 esock_atom_value},
                vals[NUM(keys)];
            size_t numKeys = NUM(keys);
            BOOLEAN_T have_value;

            SSDBG( descP,
                   ("WIN-ESAIO", "encode_cmsgs {%d} -> cmsg header data: "
                    "\r\n   dataPos: %d"
                    "\r\n   dataLen: %d"
                    "\r\n", descP->sock, dataPos, dataLen) );

            vals[0] = esock_encode_level(env, currentP->cmsg_level);
            vals[2] = MKSBIN(env, ctrlBuf, dataPos, dataLen);
            have_value = esock_encode_cmsg(env,
                                           currentP->cmsg_level,
                                           currentP->cmsg_type,
                                           dataP, dataLen, &vals[1], &vals[3]);

            SSDBG( descP,
                   ("WIN-ESAIO", "encode_cmsgs {%d} -> "
                    "\r\n   %T: %T"
                    "\r\n   %T: %T"
                    "\r\n   %T: %T"
                    "\r\n", descP->sock,
                    keys[0], vals[0], keys[1], vals[1], keys[2], vals[2]) );
            if (have_value)
                SSDBG( descP,
                       ("WIN-ESAIO", "encode_cmsgs {%d} -> "
                        "\r\n   %T: %T"
                        "\r\n", descP->sock, keys[3], vals[3]) );

            /* Guard against cut-and-paste errors */
            ESOCK_ASSERT( numKeys == NUM(vals) );
            ESOCK_ASSERT( MKMA(env, keys, vals,
                               numKeys - (have_value ? 0 : 1), &cmsgHdr) );

            /* And finally add it to the list... */
            TARRAY_ADD(cmsghdrs, cmsgHdr);
        }
    }

    SSDBG( descP,
           ("WIN-ESAIO", "encode_cmsgs {%d} -> cmsg headers processed when"
            "\r\n   TArray Size: %d"
            "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

    /* The tarray is populated - convert it to a list */
    TARRAY_TOLIST(cmsghdrs, env, eCMsg);
}



/* ======                       Receive functions                    ====== */


/* ========================================================================
 * The (read) buffer handling should be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 *
 * The following flags are supported (with overlapped):
 *    MSG_OOB            - Processes OOB data.
 *    MSG_PARTIAL        - *message-oriented* sockets only.
 *                         Both intput to *and* output from recv.
 *    MSG_PUSH_IMMEDIATE - *stream-oriented* sockets only.
 *                         Hint rather than an actual guarantee.
 *    MSG_WAITALL        - *stream-oriented* sockets only.
 *                         The request will complete only when one of
 *                         the following conditions apply:
 *                         - buffer is completely full.
 *                         - The connection has been closed.
 *                         - The request has been canceled or an error occurred.
 */
extern
ERL_NIF_TERM esaio_recv(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    ErlNifPid       caller;
    ESAIOOperation* opP;
    int             rres;
    WSABUF          wbuf;
    DWORD           f = flags;
    size_t          bufSz = (len != 0 ? len : descP->rBufSz);

    SSDBG( descP, ("WIN-ESAIO", "esaio_recv {%d} -> entry with"
                   "\r\n   length:        %ld"
                   "\r\n   (buffer) size: %lu"
                   "\r\n", descP->sock,
                   (long) len, (unsigned long) bufSz) );

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    /* Accept and Read can not be simultaneous? */
    if (descP->acceptorsQ.first != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Ensure that this caller does not *already* have a
     * (recv) request waiting */
    if (esock_reader_search4pid(env, descP, &caller)) {
        /* Reader already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* Allocate the operation */
    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_RECV;
    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * How much does this cost?
     */
    opP->env                = esock_alloc_env("esaio-recv - operation");
    opP->data.recv.recvRef  = CP_TERM(opP->env, recvRef);
    opP->data.recv.sockRef  = CP_TERM(opP->env, sockRef);
    opP->caller             = caller;

    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &opP->data.recv.buf) );

    opP->data.recv.toRead = len;
    wbuf.buf = opP->data.recv.buf.data;
    wbuf.len = opP->data.recv.buf.size;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_tries, &descP->readTries, 1);

    SSDBG( descP, ("WIN-ESAIO", "esaio_recv {%d} -> try read (%lu)\r\n",
                   descP->sock, (unsigned long) bufSz) );

    rres = sock_recv_O(descP->sock, &wbuf, &f, (OVERLAPPED*) opP);

    return recv_check_result(env, descP, len, opP, caller, rres,
                             sockRef, recvRef);
}


/* *** recv_check_result ***
 *
 * Analyze the result of a receive attempt.
 * The receive may have been completed directly or scheduled (overlapped).
 */
static
ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ssize_t          toRead,
                               ESAIOOperation*  opP,
                               ErlNifPid        caller,
                               int              recv_result,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM eres;

    if (recv_result == 0) {

        /* +++ Success +++ */

        eres = recv_check_ok(env, descP, opP, toRead, caller, sockRef, recvRef);

    } else {
        int err;

        /* +++ Failure or pending +++ */

        err = sock_errno();

        /* As pointed out above, there are basically two kinds of errors: 
         * 1) Pending:
         *    An overlapped operation was successfully initiated.
         *    Completion will be indicated at a later time.
         * 2) An actual error
         */

        if (err == WSA_IO_PENDING) {

            if (! IS_ZERO(recvRef)) {

                eres = recv_check_pending(env, descP, opP, caller,
                                          sockRef, recvRef);

            } else {

                /* We are not allowed to wait! => cancel */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recv_check_result(%T, %d) -> "
                        "pending - but we are not allowed to wait => cancel"
                        "\r\n", sockRef, descP->sock) );

                if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) opP)) {
                    int          save_errno = sock_errno();
                    ERL_NIF_TERM tag        = esock_atom_cancel;
                    ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                    SSDBG( descP,
                           ("WIN-ESAIO",
                            "recv_check_result(%T, %d) -> "
                            "failed cancel pending operation"
                            "\r\n   %T"
                            "\r\n", sockRef, descP->sock, reason) );

                    eres = esock_make_error(env, MKT2(env, tag, reason));

                } else {

                    eres = esock_atom_timeout; // Will trigger {error, timeout}

                }

            }

        } else {

            eres = recv_check_fail(env, descP, opP, err, sockRef);

        }
        
    }

    return eres;
}



/* *** recv_check_ok ***
 *
 * A successful recv. We *know* that in this case the buffer is filled!
 */

static
ERL_NIF_TERM recv_check_ok(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ESAIOOperation*  opP,
                           ssize_t          toRead,
                           ErlNifPid        caller,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM data, eres;
    DWORD        read = 0, flags = 0;

    SSDBG( descP,
           ("WIN-ESAIO",
            "recv_check_ok -> try get overlapped result\r\n") );

    if (get_recv_ovl_result(descP->sock, (OVERLAPPED*) opP, &read, &flags)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "recv_check_ok -> overlapped success result: "
                "\r\n   read:  %d"
                "\r\n   flags: 0x%X"
                "\r\n", read, flags) );

        (void) flags; // We should really do something with this...

        /* <KOLLA>
         *
         * We need to handle read = 0 for other type(s) (DGRAM) when
         * its actually valid to read 0 bytes.
         *
         * </KOLLA>
         */

        if ((read == 0) && (descP->type == SOCK_STREAM)) {

            /*
             * When a stream socket peer has performed an orderly
             * shutdown, the return value will be 0 (the traditional
             * "end-of-file" return).
             *
             * *We* do never actually try to read 0 bytes!
             */

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_read_fails, &descP->readFails, 1);

            eres = esock_make_error(env, esock_atom_closed);
            
        } else {

            if (read == opP->data.recv.buf.size) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recv_check_ok(%T, %d) -> complete success (%d)"
                        "\r\n", sockRef, descP->sock, read) );

                /* There *may* be more data available,
                 * so we could return {more, Bin}. But that requires
                 * the use of rNum and rNumCnt to work properly.
                 * (otherwise we may end up in an infinite read loop).
                 * But sine we do not (yet) have those fields on Windows,
                 * we will just return {ok, Bin} and be done with it.
                 *
                 * This transfers "ownership" of the *allocated* binary to an
                 * erlang term (no need for an explicit free).
                 */

                data = MKBIN(env, &opP->data.recv.buf);

                eres = esock_make_ok2(env, data);

            } else if ((toRead == 0) ||
                       (descP->type != SOCK_STREAM)) {

                /* On Windows, we do not (yet) use rNum and rNumCnt,
                 * so we can't loop when we do not specify a actual
                 * length (toRead = 0). Therefor, when toRead = 0 we
                 * stop reading directly and return {ok, Data}.
                 */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recv_check_ok(%T, %d) -> complete success (%d, %d)"
                        "\r\n",
                        sockRef, descP->sock, read, opP->data.recv.buf.size) );

                ESOCK_ASSERT( REALLOC_BIN(&opP->data.recv.buf, read) );

                /* This transfers "ownership" of the *allocated* binary to an
                 * erlang term (no need for an explicit free).
                 */
                data = MKBIN(env, &opP->data.recv.buf);
                data = MKSBIN(env, data, 0, read);

                eres = esock_make_ok2(env, data);

            } else {
                
                /*
                 * We did not get everything we asked for,
                 * make another attempt: {more, Data}
                 */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recv_check_ok(%T, %d) -> partial (%d) success"
                        "\r\n", sockRef, descP->sock, read) );

                ESOCK_ASSERT( REALLOC_BIN(&opP->data.recv.buf, read) );

                /*
                 * This transfers "ownership" of the *allocated* binary to an
                 * erlang term (no need for an explicit free).
                 */
                data = MKBIN(env, &opP->data.recv.buf);
                data = MKSBIN(env, data, 0, read);

                eres = MKT2(env, esock_atom_more, data);

            }

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_read_pkg, &descP->readPkgCnt, 1);
            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_read_byte, &descP->readByteCnt, read);

            /* (maybe) Update max */
            if (read > descP->readPkgMax)
                descP->readPkgMax = read;

        }

    } else {

        int save_errno = sock_errno();

        switch (save_errno) {
        case WSA_IO_INCOMPLETE:
            /*
             * WSA_IO_INCOMPLETE
             *
             * Even though it (the I/O Completion Port framework) told
             * us it was done, it was not. So we need to postpone and let
             * the (worker) threads deal with it anyway...effing framework...
             */

            if (! IS_ZERO(recvRef)) {
                
                eres = recv_check_pending(env, descP, opP, caller,
                                          sockRef, recvRef);

            } else {

                /* But we are not allowed to wait! => cancel */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recv_check_ok(%T, %d) -> "
                        "incomplete - but we are not allowed to wait => cancel"
                        "\r\n", sockRef, descP->sock) );

                if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) opP)) {
                    int          save_errno = sock_errno();
                    ERL_NIF_TERM tag        = esock_atom_cancel;
                    ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                    SSDBG( descP,
                           ("WIN-ESAIO",
                            "recv_check_ok(%T, %d) -> "
                            "failed cancel incomplete operation"
                            "\r\n   %T"
                            "\r\n", sockRef, descP->sock, reason) );

                    eres = esock_make_error(env, MKT2(env, tag, reason));

                } else {

                    eres = esock_atom_timeout; // Will trigger {error, timeout}

                }
            }
            break;

        default:
            {
                ERL_NIF_TERM eerrno = ENO2T(env, save_errno);
                ERL_NIF_TERM reason = MKT2(env,
                                           esock_atom_get_overlapped_result,
                                           eerrno);

                ESOCK_CNT_INC(env, descP, sockRef,
                              esock_atom_read_fails, &descP->readFails, 1);

                MLOCK(ctrl.cntMtx);

                esock_cnt_inc(&ctrl.genErrs, 1);

                MUNLOCK(ctrl.cntMtx);

                eres = esock_make_error(env, reason);
            }
            break;
        }
    }

    SSDBG( descP,
           ("WIN-ESAIO", "recv_check_ok(%T) {%d} -> done"
            "\r\n",
            sockRef, descP->sock) );

    return eres;
}



/* *** recv_check_pending ***
 *
 * The recv operation was scheduled, that is, its now in the hands
 * of the I/O Completion Port framework.
 */
static
ERL_NIF_TERM recv_check_pending(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ESAIOOperation*  opP,
                                ErlNifPid        caller,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     recvRef)
{

    SSDBG( descP,
           ("WIN-ESAIO",
            "recv_check_pending(%T, %d) -> entry with"
            "\r\n   recvRef: %T"
            "\r\n", sockRef, descP->sock, recvRef) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_waits, &descP->readWaits, 1);

    descP->readState |= ESOCK_STATE_SELECTED;

    esock_reader_push(env, descP, caller, recvRef, opP);
            
    return esock_atom_completion;
    
}



/* *** recv_check_fail ***
 *
 * Processing done upon failed 'recv'.
 * An actual failure.
 */
static
ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ESAIOOperation*  opP,
                             int              saveErrno,
                             ERL_NIF_TERM     sockRef)
{
    SSDBG( descP,
           ("WIN-ESAIO", "recv_check_fail(%T) {%d} -> entry with"
            "\r\n   errno: %d"
            "\r\n",
            sockRef, descP->sock, saveErrno) );

    FREE_BIN( &opP->data.recv.buf );

    return recv_check_failure(env, descP, opP, saveErrno, sockRef);
}



/* *** recv_check_failure ***
 *
 * Processing done upon failed recv.
 * An actual failure.
 */
static
ERL_NIF_TERM recv_check_failure(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ESAIOOperation*  opP,
                                int              saveErrno,
                                ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
           ("WIN-ESAIO", "recv_check_failure(%T) {%d} -> error: %d (%T)\r\n",
            sockRef, descP->sock, saveErrno, reason) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_fails, &descP->readFails, 1);

    esock_clear_env("recv_check_failure", opP->env);
    esock_free_env("recv_check_failure", opP->env);
    FREE( opP );

    return esock_make_error(env, reason);
}



/* ========================================================================
 * esaio_recvfrom - Read a "packet" from a socket
 *
 * The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throw'ing it away...
 */
extern
ERL_NIF_TERM esaio_recvfrom(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            ssize_t          len,
                            int              flags)
{
    ErlNifPid       caller;
    ESAIOOperation* opP;
    int             rres;
    WSABUF          wbuf;
    DWORD           f = flags;
    size_t          bufSz = (len != 0 ? len : descP->rBufSz);

    if (bufSz < ESAIO_RECVFROM_MIN_BUFSZ) bufSz = ESAIO_RECVFROM_MIN_BUFSZ;

    SSDBG( descP, ("WIN-ESAIO", "essio_recvfrom {%d} -> entry with"
                   "\r\n   bufSz: %d"
                   "\r\n", descP->sock, bufSz) );

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    /* Accept and Read can not be simultaneous? */
    if (descP->acceptorsQ.first != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Ensure that this caller does not *already* have a
     * (recv) request waiting */
    if (esock_reader_search4pid(env, descP, &caller)) {
        /* Reader already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* Allocate the operation */
    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_RECVFROM;
    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * How much does this cost?
     */
    opP->env                    = esock_alloc_env("esaio-recvfrom - operation");
    opP->data.recvfrom.recvRef  = CP_TERM(opP->env, recvRef);
    opP->data.recvfrom.sockRef  = CP_TERM(opP->env, sockRef);
    opP->caller                 = caller;

    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &opP->data.recv.buf) );

    opP->data.recvfrom.toRead = len;
    wbuf.buf = opP->data.recvfrom.buf.data;
    wbuf.len = opP->data.recvfrom.buf.size;

    opP->data.recvfrom.addrLen = sizeof(ESockAddress);

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_tries, &descP->readTries, 1);

    SSDBG( descP, ("WIN-ESAIO", "esaio_recvfrom {%d} -> try read (%lu)\r\n",
                   descP->sock, (unsigned long) bufSz) );

    rres = sock_recvfrom_O(descP->sock, &wbuf, &f,
                           (struct sockaddr*) &opP->data.recvfrom.fromAddr,
                           &opP->data.recvfrom.addrLen, (OVERLAPPED*) opP);

    return recvfrom_check_result(env, descP, opP, caller, rres,
                                 sockRef, recvRef);
}



/* *** recvfrom_check_result ***
 *
 * Analyze the result of a receive attempt.
 * The receive may have been completed directly or scheduled (overlapped).
 */
static
ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ESAIOOperation*  opP,
                                   ErlNifPid        caller,
                                   int              recv_result,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM eres;

    if (recv_result == 0) {

        /* +++ Success +++ */

        eres = recvfrom_check_ok(env, descP, opP, caller, sockRef, recvRef);

    } else {
        int err;

        /* +++ Failure +++ */

        err = sock_errno();

        /* As pointed out above, there are basically two kinds of errors: 
         * 1) Pending:
         *    An overlapped operation was successfully initiated.
         *    Completion will be indicated at a later time.
         * 2) An actual error
         */

        if (err == WSA_IO_PENDING) {

            if (! IS_ZERO(recvRef)) {

                eres = recv_check_pending(env, descP, opP, caller,
                                          sockRef, recvRef);
            } else {

                /* We are not allowed to wait! => cancel */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recvfrom_check_result(%T, %d) -> "
                        "pending - but we are not allowed to wait => cancel"
                        "\r\n", sockRef, descP->sock) );

                if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) opP)) {
                    int          save_errno = sock_errno();
                    ERL_NIF_TERM tag        = esock_atom_cancel;
                    ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                    SSDBG( descP,
                           ("WIN-ESAIO",
                            "recvfrom_check_result(%T, %d) -> "
                            "failed cancel pending operation"
                            "\r\n   %T"
                            "\r\n", sockRef, descP->sock, reason) );

                    eres = esock_make_error(env, MKT2(env, tag, reason));

                } else {

                    eres = esock_atom_timeout; // Will trigger {error, timeout}

                }

            }
                
        } else {

            eres = recvfrom_check_fail(env, descP, opP, err, sockRef);

        }
        
    }

    return eres;
}



/* *** recvfrom_check_ok ***
 *
 * A successful recvfrom. We *know* that in this case the buffer is filled!
 */

static
ERL_NIF_TERM recvfrom_check_ok(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ESAIOOperation*  opP,
                               ErlNifPid        caller,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM data, eres;
    DWORD        read = 0, flags = 0;

    SSDBG( descP,
           ("WIN-ESAIO",
            "recvfrom_check_ok -> try get overlapped result\r\n") );

    if (get_recv_ovl_result(descP->sock, (OVERLAPPED*) opP, &read, &flags)) {

        ERL_NIF_TERM eSockAddr;

        SSDBG( descP,
               ("WIN-ESAIO",
                "recvfrom_check_ok -> overlapped result: "
                "\r\n   read:  %d"
                "\r\n   flags: 0x%X"
                "\r\n", read, flags) );

        (void) flags; // We should really do something with this...

        esock_encode_sockaddr(env,
                              &opP->data.recvfrom.fromAddr,
                              opP->data.recvfrom.addrLen,
                              &eSockAddr);

        if (read != opP->data.recvfrom.buf.size) {
            ESOCK_ASSERT( REALLOC_BIN(&opP->data.recvfrom.buf, read) );
        }
        /* This transfers "ownership" of the *allocated* binary to an
         * erlang term (no need for an explicit free).
         */
        data = MKBIN(env, &opP->data.recvfrom.buf);

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_pkg, &descP->readPkgCnt, 1);
        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_byte, &descP->readByteCnt, read);

        /* (maybe) Update max */
        if (read > descP->readPkgMax)
            descP->readPkgMax = read;

        /*
         * This is:                 {ok, {Source, Data}}
         * But it should really be: {ok, {Source, Flags, Data}}
         */
        eres = esock_make_ok2(env, MKT2(env, eSockAddr, data));

    } else {

        int save_errno = sock_errno();

        switch (save_errno) {
        case WSA_IO_INCOMPLETE:
            /*
             * WSA_IO_INCOMPLETE
             *
             * Even though it (the I/O Completion Port framework) told
             * us it was done, it was not. So we need to postpone and let
             * the (worker) threads deal with it anyway...effing framework...
             */

            if (! IS_ZERO(recvRef)) {
                
                eres = recv_check_pending(env, descP, opP, caller,
                                          sockRef, recvRef);

            } else {

                /* But we are not allowed to wait! => cancel */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recvfrom_check_ok(%T, %d) -> "
                        "incomplete - but we are not allowed to wait => cancel"
                        "\r\n", sockRef, descP->sock) );

                if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) opP)) {
                    int          save_errno = sock_errno();
                    ERL_NIF_TERM tag        = esock_atom_cancel;
                    ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                    SSDBG( descP,
                           ("WIN-ESAIO",
                            "recvfrom_check_ok(%T, %d) -> "
                            "failed cancel incomplete operation"
                            "\r\n   %T"
                            "\r\n", sockRef, descP->sock, reason) );

                    eres = esock_make_error(env, MKT2(env, tag, reason));

                } else {

                    eres = esock_atom_timeout; // Will trigger {error, timeout}

                }
            }
            break;

        default:
            {
                ERL_NIF_TERM eerrno = ENO2T(env, save_errno);
                ERL_NIF_TERM reason = MKT2(env,
                                           esock_atom_get_overlapped_result,
                                           eerrno);

                ESOCK_CNT_INC(env, descP, sockRef,
                              esock_atom_read_fails, &descP->readFails, 1);

                MLOCK(ctrl.cntMtx);

                esock_cnt_inc(&ctrl.genErrs, 1);

                MUNLOCK(ctrl.cntMtx);

                eres = esock_make_error(env, reason);
            }
            break;
        }
    }

    SSDBG( descP,
           ("WIN-ESAIO", "recvfrom_check_ok(%T) {%d} -> done with"
            "\r\n   result: %T"
            "\r\n",
            sockRef, descP->sock, eres) );

    return eres;
}



/* *** recvfrom_check_fail ***
 *
 * Processing done upon failed 'recvfrom'.
 * An actual failure.
 */
static
ERL_NIF_TERM recvfrom_check_fail(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ESAIOOperation*  opP,
                                 int              saveErrno,
                                 ERL_NIF_TERM     sockRef)
{
    SSDBG( descP,
           ("WIN-ESAIO", "recfrom_check_fail(%T) {%d} -> entry with"
            "\r\n   errno: %d"
            "\r\n",
            sockRef, descP->sock, saveErrno) );

    FREE_BIN( &opP->data.recvfrom.buf );

    return recv_check_failure(env, descP, opP, saveErrno, sockRef);
}




/* ========================================================================
 * esaio_recvmsg - Read a "message" from a socket
 * The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 *
 * Note that this operation *only* works for socket
 * of types SOCK_DGRAM and SOCK_RAW! Should we check
 * and throw 'enotsup' otherwise? Would make testing
 * easier...
 */
extern
ERL_NIF_TERM esaio_recvmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef,
                           ssize_t          bufLen,
                           ssize_t          ctrlLen,
                           int              flags)
{
    ErlNifPid       caller;
    ESAIOOperation* opP;
    SOCKLEN_T       addrLen;
    size_t          bufSz  = (bufLen  != 0 ? bufLen  : descP->rBufSz);
    size_t          ctrlSz = (ctrlLen != 0 ? ctrlLen : descP->rCtrlSz);
    int             rres;
    ERL_NIF_TERM    eres;

    (void) flags;

    SSDBG( descP, ("WIN-ESAIO", "esaio_recvmsg(%T) {%d} -> entry with"
                   "\r\n   bufSz:  %lu (%ld)"
                   "\r\n   ctrlSz: %ld (%ld)"
                   "\r\n", sockRef, descP->sock,
                   (unsigned long) bufSz, (long) bufLen,
                   (unsigned long) ctrlSz, (long) ctrlLen) );

    /* This *only* works on socket type(s) DGRAM or RAW.
     * Other socket types results in einval, which is not very
     * helpful. So, in order to, atleast, help with testing,
     * we do this...
     */
    if (! ((descP->type == SOCK_DGRAM) || (descP->type == SOCK_RAW))) {
        return enif_raise_exception(env, MKA(env, "notsup"));
    }

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    /* Accept and Read can not be simultaneous? */
    if (descP->acceptorsQ.first != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Ensure that this caller does not *already* have a
     * (recv) request waiting */
    if (esock_reader_search4pid(env, descP, &caller)) {
        /* Reader already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }

    /* Allocate the operation */
    opP = MALLOC( sizeof(ESAIOOperation) );
    ESOCK_ASSERT( opP != NULL);
    sys_memzero((char*) opP, sizeof(ESAIOOperation));

    opP->tag = ESAIO_OP_RECVMSG;
    /* Its a bit annoying that we have to alloc an env and then
     * copy the ref *before* we know that we actually need it.
     * How much does this cost?
     */
    opP->env                   = esock_alloc_env("esaio-recvmsg - operation");
    opP->data.recvmsg.recvRef  = CP_TERM(opP->env, recvRef);
    opP->data.recvmsg.sockRef  = CP_TERM(opP->env, sockRef);
    opP->caller                = caller;

    /* Allocate the (msg) data buffer:
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &opP->data.recvmsg.data[0]) );

    /* Allocate the ctrl (buffer):
     */
    ESOCK_ASSERT( ALLOC_BIN(ctrlSz, &opP->data.recvmsg.ctrl) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_tries, &descP->readTries, 1);

    addrLen = sizeof(opP->data.recvmsg.addr);
    sys_memzero((char*) &opP->data.recvmsg.addr, addrLen);
    sys_memzero((char*) &opP->data.recvmsg.msg,  sizeof(opP->data.recvmsg.msg));

    opP->data.recvmsg.wbufs[0].buf = opP->data.recvmsg.data[0].data;
    opP->data.recvmsg.wbufs[0].len = opP->data.recvmsg.data[0].size;

    opP->data.recvmsg.msg.name          = (SOCKADDR*) &opP->data.recvmsg.addr;
    opP->data.recvmsg.msg.namelen       = addrLen;
    opP->data.recvmsg.msg.lpBuffers     = opP->data.recvmsg.wbufs;
    opP->data.recvmsg.msg.dwBufferCount = 1; // Should be calculated...
    opP->data.recvmsg.msg.Control.buf   = opP->data.recvmsg.ctrl.data;
    opP->data.recvmsg.msg.Control.len   = opP->data.recvmsg.ctrl.size;
    opP->data.recvmsg.msg.dwFlags       = 0; // TMP

    rres = sock_recvmsg_O(descP->sock,
                          &opP->data.recvmsg.msg,
                          (OVERLAPPED*) opP);

    eres = recvmsg_check_result(env, descP, opP, caller, rres,
                                sockRef, recvRef);

    SSDBG( descP, ("WIN-ESAIO", "esaio_recvmsg(%T) {%d} -> done\r\n",
                   sockRef, descP->sock) );

    return eres;
}


static
BOOLEAN_T recv_check_reader(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ErlNifPid*       caller,
                            ERL_NIF_TERM     ref,
                            ERL_NIF_TERM*    checkResult)
{
    BOOLEAN_T result;

    /* Check if already reader */
    if (! esock_reader_search4pid(env, descP, caller)) {
        /* No; check if we can wait for a result */
        if (COMPARE(ref, esock_atom_zero) == 0)
            return FALSE;
    } else {
        *checkResult = esock_raise_invalid(env, esock_atom_state);
    }

    return TRUE;
}


/* *** recvmsg_check_result ***
 *
 * The recvmsg function (maybe) delivers one (1) message. If our buffer
 * is to small, the message will be truncated. So, regardless of
 * if we filled the buffer or not, we have got what we are going
 * to get regarding this message.
 */
static
ERL_NIF_TERM recvmsg_check_result(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ESAIOOperation*  opP,
                                  ErlNifPid        caller,
                                  int              recv_result,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM eres;

    SSDBG( descP,
           ("WIN-ESAIO", "recvmsg_check_result(%T) {%d} -> entry with"
            "\r\n   recv_result: %d"
            "\r\n   recvRef:     %T"
            "\r\n", sockRef, descP->sock, recv_result, recvRef) );

    if (recv_result == 0) {

        /* +++ Success +++ */

        eres = recvmsg_check_ok(env, descP, opP, caller, sockRef, recvRef);

    } else {
        int err;

        /* +++ Failure +++ */

        err = sock_errno();

        /* As pointed out above, there are basically two kinds of errors: 
         * 1) Pending:
         *    An overlapped operation was successfully initiated.
         *    Completion will be indicated at a later time.
         * 2) An actual error
         */

        if (err == WSA_IO_PENDING) {

            if (! IS_ZERO(recvRef)) {

                eres = recv_check_pending(env, descP, opP, caller,
                                          sockRef, recvRef);

            } else {

                /* We are not allowed to wait! => cancel */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recvmsg_check_result(%T, %d) -> "
                        "pending - but we are not allowed to wait => cancel"
                        "\r\n", sockRef, descP->sock) );

                if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) opP)) {
                    int          save_errno = sock_errno();
                    ERL_NIF_TERM tag        = esock_atom_cancel;
                    ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                    SSDBG( descP,
                           ("WIN-ESAIO",
                            "recvmsg_check_result(%T, %d) -> "
                            "failed cancel pending operation"
                            "\r\n   %T"
                            "\r\n", sockRef, descP->sock, reason) );

                    eres = esock_make_error(env, MKT2(env, tag, reason));

                } else {

                    eres = esock_atom_timeout; // Will trigger {error, timeout}

                }
                
            }

        } else {

            eres = recvmsg_check_fail(env, descP, opP, err, sockRef);

        }
        
    }

    SSDBG( descP,
           ("WIN-ESAIO", "recvmsg_check_result(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return eres;
}


/* *** recvmsg_check_ok ***
 *
 * A successful recvmsg. We *know* that in this case the buffer is filled!
 */

static
ERL_NIF_TERM recvmsg_check_ok(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ESAIOOperation*  opP,
                              ErlNifPid        caller,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM eMsg, eres;
    DWORD        read = 0, flags = 0;

    SSDBG( descP,
           ("WIN-ESAIO",
            "recvmsg_check_ok(%T) {%d} -> try get overlapped result\r\n",
            sockRef, descP->sock) );

    if (get_recv_ovl_result(descP->sock, (OVERLAPPED*) opP, &read, &flags)) {

        ERL_NIF_TERM eSockAddr;

        SSDBG( descP,
               ("WIN-ESAIO",
                "recvmsg_check_ok(%T, %d) -> overlapped success result: "
                "\r\n   read:  %d"
                "\r\n   flags: 0x%X"
                "\r\n", sockRef, descP->sock, read, flags) );

        (void) flags; // We should really do something with this...

        encode_msg(env, descP, read,
                   &opP->data.recvmsg.msg,
                   opP->data.recvmsg.data,
                   &opP->data.recvmsg.ctrl,
                   &eMsg);

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_pkg, &descP->readPkgCnt, 1);
        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_byte, &descP->readByteCnt, read);

        /* (maybe) Update max */
        if (read > descP->readPkgMax)
            descP->readPkgMax = read;

        eres = esock_make_ok2(env, eMsg);

    } else {

        int save_errno = sock_errno();

        switch (save_errno) {
        case WSA_IO_INCOMPLETE:
            /*
             * WSA_IO_INCOMPLETE
             *
             * Even though it (the I/O Completion Port framework) told
             * us it was done, it was not. So we need to postpone and let
             * the (worker) threads deal with it anyway...effing framework...
             */

            if (! IS_ZERO(recvRef)) {
                
                eres = recv_check_pending(env, descP, opP, caller,
                                          sockRef, recvRef);

            } else {

                /* But we are not allowed to wait! => cancel */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "recvmsg_check_ok(%T, %d) -> "
                        "incomplete - but we are not allowed to wait => cancel"
                        "\r\n", sockRef, descP->sock) );

                if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) opP)) {
                    int          save_errno = sock_errno();
                    ERL_NIF_TERM tag        = esock_atom_cancel;
                    ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                    SSDBG( descP,
                           ("WIN-ESAIO",
                            "recvmsg_check_ok(%T, %d) -> "
                            "failed cancel incomplete operation"
                            "\r\n   %T"
                            "\r\n", sockRef, descP->sock, reason) );

                    eres = esock_make_error(env, MKT2(env, tag, reason));

                } else {

                    eres = esock_atom_timeout; // Will trigger {error, timeout}

                }
            }
            break;

        default:
            {
                ERL_NIF_TERM eerrno = ENO2T(env, save_errno);
                ERL_NIF_TERM reason = MKT2(env,
                                           esock_atom_get_overlapped_result,
                                           eerrno);

                ESOCK_CNT_INC(env, descP, sockRef,
                              esock_atom_read_fails, &descP->readFails, 1);

                MLOCK(ctrl.cntMtx);

                esock_cnt_inc(&ctrl.genErrs, 1);

                MUNLOCK(ctrl.cntMtx);

                eres = esock_make_error(env, reason);
            }
            break;
        }
    }

    SSDBG( descP,
           ("WIN-ESAIO", "recvmsg_check_ok(%T) {%d} -> done with"
            "\r\n   result: %T"
            "\r\n",
            sockRef, descP->sock, eres) );

    return eres;
}



/* *** recvmsg_check_fail ***
 *
 * Processing done upon failed 'recvmsg'.
 * An actual failure.
 */
static
ERL_NIF_TERM recvmsg_check_fail(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ESAIOOperation*  opP,
                                int              saveErrno,
                                ERL_NIF_TERM     sockRef)
{
    SSDBG( descP,
           ("WIN-ESAIO", "recvmsg_check_fail(%T) {%d} -> entry with"
            "\r\n   errno: %d"
            "\r\n",
            sockRef, descP->sock, saveErrno) );

    FREE_BIN( &opP->data.recvmsg.data[0] );
    FREE_BIN( &opP->data.recvmsg.ctrl );

    return recv_check_failure(env, descP, opP, saveErrno, sockRef);
}





/* *******************************************************************
 * esaio_close - Close a socket
 *
 * Stage 1 of the socket close
 */

extern
ERL_NIF_TERM esaio_close(ErlNifEnv*       env,
                         ESockDescriptor* descP)
{
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_close(%d) -> begin closing\r\n",
            descP->sock) );

     if (! IS_OPEN(descP->readState)) {
        /* A bit of cheeting; maybe not closed yet - do we need a queue? */
        return esock_make_error_closed(env);
    }

    /* Store the PID of the caller,
     * since we need to inform it when we
     * (that is, the stop callback function)
     * completes.
     */
    ESOCK_ASSERT( enif_self(env, &descP->closerPid) != NULL );

    /* If the caller is not the owner; monitor the caller,
     * since we should complete this operation even if the caller dies
     * (for whatever reason).
     */
    if (COMPARE_PIDS(&descP->closerPid, &descP->ctrlPid) != 0) {

        ESOCK_ASSERT( MONP("esaio_close-check -> closer",
                           env, descP,
                           &descP->closerPid,
                           &descP->closerMon) == 0 );
    }

    /* Prepare for closing the socket */
    descP->readState  |= ESOCK_STATE_CLOSING;
    descP->writeState |= ESOCK_STATE_CLOSING;
    if (do_stop(env, descP)) {

        // stop() has been scheduled - wait for it
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_close {%d} -> stop was scheduled\r\n",
                descP->sock) );

        // Create closeRef for the close msg that esock_stop() will send
        descP->closeEnv = esock_alloc_env("esock_close_do - close-env");
        descP->closeRef = MKREF(descP->closeEnv);

        return esock_make_ok2(env, CP_TERM(env, descP->closeRef));

    } else {
        // The socket may be closed - tell caller to finalize
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_close {%d} -> stop was called\r\n",
                descP->sock) );

        return esock_atom_ok;
    }
}



static
BOOLEAN_T do_stop(ErlNifEnv*       env,
                  ESockDescriptor* descP)
{
    BOOLEAN_T    ret;
    ERL_NIF_TERM sockRef;

    sockRef = enif_make_resource(env, descP);

    if (IS_SELECTED(descP)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "do_stop {%d} -> cancel outstanding I/O operations\r\n",
                descP->sock) );

        /* Cancel *all* outstanding I/O operations on the socket.
         * We have to wait for the worker threads to process these ops!
         * (will result in OPERATION_ABORTED for the threads).
         */
        if (! CancelIoEx((HANDLE) descP->sock, NULL) ) {
            int          save_errno = sock_errno();
            ERL_NIF_TERM ereason    = ENO2T(env, save_errno);

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "do_stop {%d} -> cancel I/O failed: "
                    "\r\n   %T\r\n",
                    descP->sock, ereason) );

            /* Only issue an error message for errors *other* than
             * 'not found' (since 'not found' means there is no active
             * requests = already completed => race).
             */

            if (save_errno != ERROR_NOT_FOUND)
                esock_error_msg("Failed cancel outstanding I/O operations:"
                                "\r\n   Socket: " SOCKET_FORMAT_STR
                                "\r\n   Reason: %T"
                                "\r\n",
                                descP->sock, ereason);
            
            ret = FALSE;

        } else {

            /* Cancel of all active requests (to the I/O completion port
             * machinery) has been successfully requested.
             * The requests will be aborted and handled by the worker threads.
             */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "do_stop {%d} -> successfully canceled\r\n", descP->sock) );

            ret = TRUE;
        }

    } else {

        /* No active requests in the I/O completion port machinery */

        SSDBG( descP,
               ("WIN-ESAIO",
                "do_stop {%d} -> no active I/O requests\r\n", descP->sock) );

        ret = FALSE;
    }

    /* We do nothing here with the requests in the various queues
     * They are handled by the working threads, when the abort is triggered
     * (one for each request)!
     */

    /* +++++++ Connector +++++++
     * Note that there should not be Writers and a Connector
     * at the same time so the check for if the
     * current Writer/Connecter was deselected is only correct
     * under that assumption
     */

    if (descP->connectorP != NULL) {

        /* We have a Connector;
         *
         * The Connector will not get a select message
         * - send it an abort message
         */

        esock_stop_handle_current(env,
                                  "connector",
                                  descP, sockRef, &descP->connector);

        descP->connectorP = NULL;
    }

    return ret;
}



/* ========================================================================
 * Perform the final step in the socket close.
 */
extern
ERL_NIF_TERM esaio_fin_close(ErlNifEnv*       env,
                             ESockDescriptor* descP)
{
    int       err;
    ErlNifPid self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    if (IS_CLOSED(descP->readState))
        return esock_make_error_closed(env);

    if (! IS_CLOSING(descP->readState)) {
        // esock_close() has not been called
        return esock_raise_invalid(env, esock_atom_state);
    }

    if (IS_SELECTED(descP) && (descP->closeEnv != NULL)) {
        // esock_stop() is scheduled but has not been called
        return esock_raise_invalid(env, esock_atom_state);
    }

    if (COMPARE_PIDS(&descP->closerPid, &self) != 0) {
        // This process is not the closer
        return esock_raise_invalid(env, esock_atom_state);
    }

    // Close the socket

    /* Stop monitoring the closer.
     * Demonitoring may fail since this is a dirty NIF
     * - the caller may have died already.
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_fin_close {%d} -> demonitor closer process %T\r\n",
            descP->sock, descP->closerPid) );
    enif_set_pid_undefined(&descP->closerPid);
    if (descP->closerMon.isActive) {
        (void) DEMONP("esaio_fin_close -> closer",
                      env, descP, &descP->closerMon);
    }

    /* Stop monitoring the owner */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_fin_close {%d} -> demonitor owner process %T\r\n",
            descP->sock, descP->ctrlPid) );
    enif_set_pid_undefined(&descP->ctrlPid);
    (void) DEMONP("esaio_fin_close -> ctrl",
                  env, descP, &descP->ctrlMon);
    /* Not impossible to still get a esock_down() call from a
     * just triggered owner monitor down
     */

    /* This nif-function is executed in a dirty scheduler just so
     * that it can "hang" (with minimum effect on the VM) while the
     * kernel writes our buffers. IF we have set the linger option
     * for this ({true, integer() > 0}).
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_fin_close {%d} -> (try) close the socket\r\n",
            descP->sock, descP->ctrlPid) );
    err = esock_close_socket(env, descP, TRUE);

    if (err != 0) {
        if (err == ERRNO_BLOCK) {
            /* Not all data in the buffers where sent,
             * make sure the caller gets this.
             */
            return esock_make_error(env, esock_atom_timeout);
        } else {
            return esock_make_error_errno(env, err);
        }
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_fin_close -> done\r\n") );

    return esock_atom_ok;
}



/* ========================================================================
 * Cancel a connect request.
 */
extern
ERL_NIF_TERM esaio_cancel_connect(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;
    ErlNifPid    self;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_cancel_connect {%d} -> entry with"
            "\r\n   writeState: 0x%X"
            "\r\n   opRef:      %T"
            "\r\n",
            descP->sock, descP->writeState, opRef) );

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    if (! IS_OPEN(descP->writeState)) {

        res = esock_make_error_closed(env);

    } else if ((descP->connectorP == NULL) ||
               (COMPARE_PIDS(&self, &descP->connector.pid) != 0) ||
               (COMPARE(opRef, descP->connector.ref) != 0)) {

        res = esock_make_error(env, esock_atom_not_found);

    } else {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_cancel_connect {%d} -> "
                "try cancel connect I/O request\r\n",
                descP->sock) );

       if (! CancelIoEx((HANDLE) descP->sock,
                         (OVERLAPPED*) descP->connector.dataP)) {
            /* What does this mean?
             * One of the possible reasons is that the connect succeeded.
             * In which case, one of the threads in the thread-pool will
             * be triggered (eventually).
             * Then we have to deal with a connected socket that no one wants...
             */
            int save_errno = sock_errno();
            res = esock_make_error_errno(env, save_errno);
        } else {
            res = esock_atom_ok;
        }

        esock_requestor_release("esock_cancel_connect",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_cancel_connect {%d} -> done when"
            "\r\n   res: %T"
            "\r\n",
            descP->sock, descP->writeState,
            opRef, res) );

    return res;
}



/* *** esock_cancel_accept ***
 *
 * We have three different cases:
 *   *) Socket is closed:
 *      return error: closed
 *   *) Active accept (found in the request store):
 *      Cancel the completion request!
 *      Success will trigger an event (delivered) to the
 *      (completion) worker threads.
 *   *) Not found (in the request store):
 *      This request has already completed (race):
 *      return: not_found
 *
 */
extern
ERL_NIF_TERM esaio_cancel_accept(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM   res;
    ESockRequestor req;
    ErlNifPid      caller;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_cancel_accept(%T), {%d,0x%X} ->"
            "\r\n   opRef: %T"
            "\r\n", sockRef, descP->sock, descP->readState, opRef) );

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState)) {

        res = esock_make_error_closed(env);

    } else if (esock_acceptor_get(env, descP, &opRef, &caller, &req)) {

        ESOCK_ASSERT( DEMONP("esaio_cancel_accept -> acceptor",
                             env, descP, &req.mon) == 0);

         SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_cancel_accept {%d} -> try cancel accept I/O request\r\n",
                descP->sock) );

       if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) req.dataP)) {

            /* What does this mean?
             * One of the possible reasons is that the accept succeeded.
             * In which case, one of the threads in the thread-pool will
             * be triggered (eventually).
             * Then we have to deal with a connected socket that no one wants...
             */

            int save_errno = sock_errno();
            res = esock_make_error_errno(env, save_errno);

        } else {

            res = esock_atom_ok;

        }

        /* Request cleanup (demonitor already done above) */
        esock_clear_env("esaio_cancel_accept -> req cleanup", req.env);
        esock_free_env("esaio_cancel_accept -> req cleanup", req.env);

        /* *Maybe* update listen socket (read) state
         * (depends on if the queue is now empty)
         */
        if (descP->acceptorsQ.first == NULL) {
            descP->readState &= ~ESOCK_STATE_ACCEPTING;
        }

    } else {

        res = esock_make_error(env, esock_atom_not_found);

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_cancel_accept(%T) -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, res) );

    return res;
}




/* *** esock_cancel_send ***
 *
 * We have three different cases:
 *   *) Socket is closed:
 *      return error: closed
 *   *) Active send (found in the request store):
 *      Cancel the completion request!
 *      Success will trigger an event (delivered) to the
 *      (completion) worker threads.
 *   *) Not found (in the request store):
 *      This request has already completed (race):
 *      return: not_found
 *
 */
extern
ERL_NIF_TERM esaio_cancel_send(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM   res;
    ESockRequestor req;
    ErlNifPid      caller;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_cancel_send(%T), {%d,0x%X} ->"
            "\r\n   opRef: %T"
            "\r\n", sockRef, descP->sock, descP->readState, opRef) );

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->writeState)) {

        res = esock_make_error_closed(env);

    } else if (esock_writer_get(env, descP, &opRef, &caller, &req)) {

        ESOCK_ASSERT( DEMONP("esaio_cancel_send -> sender",
                             env, descP, &req.mon) == 0);

         SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_cancel_send {%d} -> try cancel send I/O request\r\n",
                descP->sock) );

       if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) req.dataP)) {

            /* What does this mean?
             * One of the possible reasons is that the send succeeded.
             * In which case, one of the threads in the thread-pool will
             * be triggered (eventually).
             */

            int save_errno = sock_errno();
            res = esock_make_error_errno(env, save_errno);

        } else {

            res = esock_atom_ok;

        }

        /* Request cleanup (demonitor already done above) */
        esock_clear_env("esaio_cancel_send -> req cleanup", req.env);
        esock_free_env("esaio_cancel_send -> req cleanup", req.env);

    } else {

        res = esock_make_error(env, esock_atom_not_found);

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_cancel_send(%T) -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, res) );

    return res;
}




/* *** esock_cancel_recv ***
 *
 * We have three different cases:
 *   *) Socket is closed:
 *      return error: closed
 *   *) Active receive (found in the request store):
 *      Cancel the completion request!
 *      Success will trigger an event (delivered) to the
 *      (completion) worker threads.
 *   *) Not found (in the request store):
 *      This request has already completed (race):
 *      return: not_found
 *
 */
extern
ERL_NIF_TERM esaio_cancel_recv(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM   res;
    ESockRequestor req;
    ErlNifPid      caller;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_cancel_recv(%T), {%d,0x%X} ->"
            "\r\n   opRef: %T"
            "\r\n", sockRef, descP->sock, descP->readState, opRef) );

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState)) {

        res = esock_make_error_closed(env);

    } else if (esock_reader_get(env, descP, &opRef, &caller, &req)) {

        ESOCK_ASSERT( DEMONP("esaio_cancel_recv -> reader",
                             env, descP, &req.mon) == 0);

         SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_cancel_recv {%d} -> try cancel send I/O request\r\n",
                descP->sock) );

       if (! CancelIoEx((HANDLE) descP->sock, (OVERLAPPED*) req.dataP)) {

            /* What does this mean?
             * One of the possible reasons is that the recv succeeded.
             * In which case, one of the threads in the thread-pool will
             * be triggered (eventually).
             */

            int save_errno = sock_errno();
            res = esock_make_error_errno(env, save_errno);

        } else {

            res = esock_atom_ok;

        }

        /* Request cleanup (demonitor already done above) */
        esock_clear_env("esaio_cancel_recv -> req cleanup", req.env);
        esock_free_env("esaio_cancel_recv -> req cleanup", req.env);

    } else {

        res = esock_make_error(env, esock_atom_not_found);

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_cancel_recv(%T) -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, res) );

    return res;
}




/* ========================================================================
 * IOCTL with three args (socket, request "key" and one argument)
 *
 * The type and value of 'arg' depend on the request,
 * which we have not yet "analyzed".
 *
 * Request     arg       arg type
 * -------     -------   --------
 * tcp_info    version   integer()
 * rcvall      command   atom() (off | on | iplevel)
 */
extern
ERL_NIF_TERM esaio_ioctl3(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req,
			  ERL_NIF_TERM     arg)
{
  switch (req) {

      /* These are *get* requests */

#if defined(SIO_TCP_INFO)
  case SIO_TCP_INFO:
      return esaio_ioctl_tcp_info(env, descP, arg);
      break;
#endif

    /* These are *set* requests */

#if defined(SIO_RCVALL)
  case SIO_RCVALL:
      return esaio_ioctl_rcvall(env, descP, arg);
      break;
#endif

#if defined(SIO_RCVALL_IGMPMCAST)
  case SIO_RCVALL_IGMPMCAST:
      return esaio_ioctl_rcvall_igmpmcast(env, descP, arg);
      break;
#endif

#if defined(SIO_RCVALL_MCAST)
  case SIO_RCVALL_MCAST:
      return esaio_ioctl_rcvall_mcast(env, descP, arg);
      break;
#endif


  default:
      return esock_make_error(env, esock_atom_enotsup);
      break;
  }

}



#if defined(SIO_TCP_INFO)
static
ERL_NIF_TERM esaio_ioctl_tcp_info(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     eversion)
{
    DWORD        ndata = 0; // We do not actually use this
    ERL_NIF_TERM result;
    int          res;
    int          version;
  
    SSDBG( descP, ("WIN-ESAIO", "esaio_ioctl_tcp_info(%d) -> entry with"
                   "\r\n      (e)version: %T"
                   "\r\n", descP->sock, eversion) );

    if (!GET_INT(env, eversion, &version))
        return enif_make_badarg(env);

    switch (version) {
    case 0:
        {
            TCP_INFO_v0 info;

            sys_memzero((char *) &info, sizeof(info));
            res = sock_ioctl2(descP->sock, SIO_TCP_INFO,
                              &version, sizeof(version),
                              &info, sizeof(info), &ndata);
            (void) ndata;
            if (res != 0) {
                int          save_errno = sock_errno();
                ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                SSDBG( descP,
                       ("WIN-ESAIO", "esaio_ioctl_tcp_info(%d,v0) -> failure: "
                        "\r\n      reason: %T"
                        "\r\n", descP->sock, reason) );

                result = esock_make_error(env, reason);

            } else {
                ERL_NIF_TERM einfo = encode_tcp_info_v0(env, &info);

                result = esock_make_ok2(env, einfo);
            }
        }
        break;

#if defined(HAVE_TCP_INFO_V1)      
    case 1:
        {
            TCP_INFO_v1 info;

            sys_memzero((char *) &info, sizeof(info));
            res = sock_ioctl2(descP->sock, SIO_TCP_INFO,
                              &version, sizeof(version),
                              &info, sizeof(info), &ndata);
            (void) ndata;
            if (res != 0) {
                int          save_errno = sock_errno();
                ERL_NIF_TERM reason     = ENO2T(env, save_errno);

                SSDBG( descP,
                       ("WIN-ESAIO", "esaio_ioctl_tcp_info(%d,v1) -> failure: "
                        "\r\n      reason: %T"
                        "\r\n", descP->sock, reason) );

                result = esock_make_error(env, reason);

            } else {
                ERL_NIF_TERM einfo = encode_tcp_info_v1(env, &info);

                result = esock_make_ok2(env, einfo);
            }
        }
        break;
#endif

    default:
        return enif_make_badarg(env);
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_ioctl_tcp_info(%d) -> done with"
            "\r\n      result: %T"
            "\r\n",
            descP->sock, result) );
    
    return result;

}
#endif


/*
  typedef struct _TCP_INFO_v0 {
  TCPSTATE State;
  ULONG    Mss;
  ULONG64  ConnectionTimeMs;
  BOOLEAN  TimestampsEnabled;
  ULONG    RttUs;
  ULONG    MinRttUs;
  ULONG    BytesInFlight;
  ULONG    Cwnd;
  ULONG    SndWnd;
  ULONG    RcvWnd;
  ULONG    RcvBuf;
  ULONG64  BytesOut;
  ULONG64  BytesIn;
  ULONG    BytesReordered;
  ULONG    BytesRetrans;
  ULONG    FastRetrans;
  ULONG    DupAcksIn;
  ULONG    TimeoutEpisodes;
  UCHAR    SynRetrans;
  } TCP_INFO_v0, *PTCP_INFO_v0;
  *
  typedef enum _TCPSTATE {
  TCPSTATE_CLOSED,
  TCPSTATE_LISTEN,
  TCPSTATE_SYN_SENT,
  TCPSTATE_SYN_RCVD,
  TCPSTATE_ESTABLISHED,
  TCPSTATE_FIN_WAIT_1,
  TCPSTATE_FIN_WAIT_2,
  TCPSTATE_CLOSE_WAIT,
  TCPSTATE_CLOSING,
  TCPSTATE_LAST_ACK,
  TCPSTATE_TIME_WAIT,
  TCPSTATE_MAX
  } TCPSTATE;
  */
#if defined(SIO_TCP_INFO)
static
ERL_NIF_TERM encode_tcp_info_v0(ErlNifEnv* env, TCP_INFO_v0* infoP)
{
    ERL_NIF_TERM einfo;
    ERL_NIF_TERM keys[] = {esock_atom_state,
        esock_atom_mss,
        esock_atom_connection_time,
        esock_atom_timestamp_enabled,
        esock_atom_rtt,
        esock_atom_min_rtt,
        esock_atom_bytes_in_flight,
        esock_atom_cwnd,
        esock_atom_snd_wnd,
        esock_atom_rcv_wnd,
        esock_atom_rcv_buf,
        esock_atom_bytes_out,
        esock_atom_bytes_in,
        esock_atom_bytes_reordered,
        esock_atom_bytes_retrans,
        esock_atom_fast_retrans,
        esock_atom_dup_acks_in,
        esock_atom_timeout_episodes,
        esock_atom_syn_retrans};
    ERL_NIF_TERM vals[]  = {encode_tcp_state(env, infoP->State),
        MKUL(env, infoP->Mss),
        MKUI64(env, infoP->ConnectionTimeMs),
        infoP->TimestampsEnabled ? esock_atom_true : esock_atom_false,
        MKUL(env,   infoP->RttUs),
        MKUL(env,   infoP->MinRttUs),
        MKUL(env,   infoP->BytesInFlight),
        MKUL(env,   infoP->Cwnd),
        MKUL(env,   infoP->SndWnd),
        MKUL(env,   infoP->RcvWnd),
        MKUL(env,   infoP->RcvBuf),
        MKUI64(env, infoP->BytesOut),
        MKUI64(env, infoP->BytesIn),
        MKUL(env,   infoP->BytesReordered),
        MKUL(env,   infoP->BytesRetrans),
        MKUL(env,   infoP->FastRetrans),
        MKUL(env,   infoP->DupAcksIn),
        MKUL(env,   infoP->TimeoutEpisodes),
        MKUI(env,   infoP->SynRetrans)};
    unsigned int numKeys = NUM(keys);
    unsigned int numVals = NUM(vals);

    ESOCK_ASSERT( numKeys == numVals );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &einfo) );

    return einfo;
}
#endif


/*
  typedef struct _TCP_INFO_v1 {
  TCPSTATE State;
  ULONG    Mss;
  ULONG64  ConnectionTimeMs;
  BOOLEAN  TimestampsEnabled;
  ULONG    RttUs;
  ULONG    MinRttUs;
  ULONG    BytesInFlight;
  ULONG    Cwnd;
  ULONG    SndWnd;
  ULONG    RcvWnd;
  ULONG    RcvBuf;
  ULONG64  BytesOut;
  ULONG64  BytesIn;
  ULONG    BytesReordered;
  ULONG    BytesRetrans;
  ULONG    FastRetrans;
  ULONG    DupAcksIn;
  ULONG    TimeoutEpisodes;
  UCHAR    SynRetrans;
  ULONG    SndLimTransRwin;
  ULONG    SndLimTimeRwin;
  ULONG64  SndLimBytesRwin;
  ULONG    SndLimTransCwnd;
  ULONG    SndLimTimeCwnd;
  ULONG64  SndLimBytesCwnd;
  ULONG    SndLimTransSnd;
  ULONG    SndLimTimeSnd;
  ULONG64  SndLimBytesSnd;
  } TCP_INFO_v1, *PTCP_INFO_v1;
 */
#if defined(SIO_TCP_INFO) && defined(HAVE_TCP_INFO_V1)
static
ERL_NIF_TERM encode_tcp_info_v1(ErlNifEnv* env, TCP_INFO_v1* infoP)
{
    ERL_NIF_TERM einfo;
    ERL_NIF_TERM keys[] = {esock_atom_state,
        esock_atom_mss,
        esock_atom_connection_time,
        esock_atom_timestamp_enabled,
        esock_atom_rtt,
        esock_atom_min_rtt,
        esock_atom_bytes_in_flight,
        esock_atom_cwnd,
        esock_atom_snd_wnd,
        esock_atom_rcv_wnd,
        esock_atom_rcv_buf,
        esock_atom_bytes_out,
        esock_atom_bytes_in,
        esock_atom_bytes_reordered,
        esock_atom_bytes_retrans,
        esock_atom_fast_retrans,
        esock_atom_dup_acks_in,
        esock_atom_timeout_episodes,
        esock_atom_syn_retrans,
        esock_atom_syn_lim_trans_rwin,
        esock_atom_syn_lim_time_rwin,
        esock_atom_syn_lim_bytes_rwin,
        esock_atom_syn_lim_trans_cwnd,
        esock_atom_syn_lim_time_cwnd,
        esock_atom_syn_lim_bytes_cwnd,
        esock_atom_syn_lim_trans_snd,
        esock_atom_syn_lim_time_snd,
        esock_atom_syn_lim_bytes_snd};
    ERL_NIF_TERM vals[]  = {encode_tcp_state(env, infoP->State),
        MKUL(env,   infoP->Mss),
        MKUI64(end, infoP->ConnectionTimeMs),
        infoP->TimestampsEnabled ? esock_atom_true : esock_atom_false,
        MKUL(env,   infoP->RttUs),
        MKUL(env,   infoP->MinRttUs),
        MKUL(env,   infoP->BytesInFlight),
        MKUL(env,   infoP->Cwnd),
        MKUL(env,   infoP->SndWnd),
        MKUL(env,   infoP->RcvWnd),
        MKUL(env,   infoP->RcvBuf),
        MKUI64(env, infoP->BytesOut),
        MKUI64(env, infoP->BytesIn),
        MKUL(env,   infoP->BytesReordered),
        MKUL(env,   infoP->BytesRetrans),
        MKUL(env,   infoP->FastRetrans),
        MKUL(env,   infoP->DupAcksIn),
        MKUL(env,   infoP->TimeoutEpisodes),
        MKUI(env,   infoP->SynRetrans),
        MKUL(env,   infoP->SndLimTransRwin),
        MKUL(env,   infoP->SndLimTimeRwin),
        MKUI64(env, infoP->SndLimBytesRwin),
        MKUL(env,   infoP->SndLimTransCwnd),
        MKUL(env,   infoP->SndLimTimeCwnd),
        MKUI64(env, infoP->SndLimBytesCwnd),
        MKUL(env,   infoP->SndLimTransSnd),
        MKUL(env,   infoP->SndLimTimeSnd),
        MKUI64(env, infoP->SndLimBytesSnd)};
    unsigned int numKeys = NUM(keys);
    unsigned int numVals = NUM(vals);

    ESOCK_ASSERT( numKeys == numVals );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &einfo) );

    return einfo;
}
#endif



#if defined(SIO_TCP_INFO)
static
ERL_NIF_TERM encode_tcp_state(ErlNifEnv* env, TCPSTATE state)
{
    ERL_NIF_TERM estate;

    switch (state) {
    case TCPSTATE_CLOSED:
        estate = esock_atom_closed;
        break;
    case TCPSTATE_LISTEN:
        estate = esock_atom_listen;
        break;
    case TCPSTATE_SYN_SENT:
        estate = esock_atom_syn_sent;
        break;
    case TCPSTATE_SYN_RCVD:
        estate = esock_atom_syn_rcvd;
        break;
    case TCPSTATE_ESTABLISHED:
        estate = esock_atom_established;
        break;
    case TCPSTATE_FIN_WAIT_1:
        estate = esock_atom_fin_wait_1;
        break;
    case TCPSTATE_FIN_WAIT_2:
        estate = esock_atom_fin_wait_2;
        break;
    case TCPSTATE_CLOSE_WAIT:
        estate = esock_atom_close_wait;
        break;
    case TCPSTATE_CLOSING:
        estate = esock_atom_closing;
        break;
    case TCPSTATE_LAST_ACK:
        estate = esock_atom_last_ack;
        break;
    case TCPSTATE_TIME_WAIT:
        estate = esock_atom_time_wait;
        break;
    case TCPSTATE_MAX:
        estate = esock_atom_max;
        break;
    default:
        estate = MKI(env, state);
        break;
    }

    return estate;
}
#endif


#if defined(SIO_RCVALL)
static
ERL_NIF_TERM esaio_ioctl_rcvall(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     evalue)
{
    DWORD        ndata = 0; // We do not actually use this
    ERL_NIF_TERM result;
    int          value, res;
  
    SSDBG( descP, ("WIN-ESAIO", "esaio_ioctl_rcvall(%d) -> entry with"
                   "\r\n      (e)value: %T"
                   "\r\n", descP->sock, evalue) );

    if (! IS_ATOM(env, evalue))
        return enif_make_badarg(env);

    if (COMPARE(evalue, esock_atom_off) == 0) {
        value = RCVALL_OFF;
    } else if (COMPARE(evalue, esock_atom_on) == 0) {
        value = RCVALL_ON;
    } else if (COMPARE(evalue, esock_atom_iplevel) == 0) {
        value = RCVALL_IPLEVEL;
    } else {
        return enif_make_badarg(env);
    }

    res = sock_ioctl2(descP->sock, SIO_RCVALL,
                      &value, sizeof(value),
                      NULL, 0, &ndata);
    (void) ndata;

    if (res != 0) {
        int          save_errno = sock_errno();
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP,
               ("WIN-ESAIO", "esaio_ioctl_rcvall(%d) -> failure: "
                "\r\n      reason: %T"
                "\r\n", descP->sock, reason) );

        result = esock_make_error(env, reason);

    } else {

        result = esock_atom_ok;

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_ioctl_rcvall(%d) -> done with"
            "\r\n      result: %T"
            "\r\n",
            descP->sock, result) );
    
    return result;

}
#endif



#if defined(SIO_RCVALL_IGMPMCAST)
static
ERL_NIF_TERM esaio_ioctl_rcvall_igmpmcast(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     evalue)
{
    DWORD        ndata = 0; // We do not actually use this
    ERL_NIF_TERM result;
    int          value, res;
  
    SSDBG( descP, ("WIN-ESAIO", "esaio_ioctl_rcvall_igmpmcast(%d) -> entry with"
                   "\r\n      (e)value: %T"
                   "\r\n", descP->sock, evalue) );

    if (! IS_ATOM(env, evalue))
        return enif_make_badarg(env);

    if (COMPARE(evalue, esock_atom_off) == 0) {
        value = RCVALL_OFF;
    } else if (COMPARE(evalue, esock_atom_on) == 0) {
        value = RCVALL_ON;
    } else {
        return enif_make_badarg(env);
    }

    res = sock_ioctl2(descP->sock, SIO_RCVALL_IGMPMCAST,
                      &value, sizeof(value),
                      NULL, 0, &ndata);
    (void) ndata;

    if (res != 0) {
        int          save_errno = sock_errno();
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP,
               ("WIN-ESAIO", "esaio_ioctl_rcvall_igmpmcast(%d) -> failure: "
                "\r\n      reason: %T"
                "\r\n", descP->sock, reason) );

        result = esock_make_error(env, reason);

    } else {

        result = esock_atom_ok;

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_ioctl_rcvall_igmpmcast(%d) -> done with"
            "\r\n      result: %T"
            "\r\n",
            descP->sock, result) );
    
    return result;

}
#endif



#if defined(SIO_RCVALL_MCAST)
/*
 * We should really have a common function for this,
 * since igmpmcast and mcast is basically identical.
 */
static
ERL_NIF_TERM esaio_ioctl_rcvall_mcast(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     evalue)
{
    DWORD        ndata = 0; // We do not actually use this
    ERL_NIF_TERM result;
    int          value, res;
  
    SSDBG( descP, ("WIN-ESAIO", "esaio_ioctl_rcvall_mcast(%d) -> entry with"
                   "\r\n      (e)value: %T"
                   "\r\n", descP->sock, evalue) );

    if (! IS_ATOM(env, evalue))
        return enif_make_badarg(env);

    if (COMPARE(evalue, esock_atom_off) == 0) {
        value = RCVALL_OFF;
    } else if (COMPARE(evalue, esock_atom_on) == 0) {
        value = RCVALL_ON;
    } else {
        return enif_make_badarg(env);
    }

    res = sock_ioctl2(descP->sock, SIO_RCVALL_MCAST,
                      &value, sizeof(value),
                      NULL, 0, &ndata);
    (void) ndata;

    if (res != 0) {
        int          save_errno = sock_errno();
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP,
               ("WIN-ESAIO", "esaio_ioctl_rcvall_mcast(%d) -> failure: "
                "\r\n      reason: %T"
                "\r\n", descP->sock, reason) );

        result = esock_make_error(env, reason);

    } else {

        result = esock_atom_ok;

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_ioctl_rcvall_mcast(%d) -> done with"
            "\r\n      result: %T"
            "\r\n",
            descP->sock, result) );
    
    return result;

}
#endif



/* ========================================================================
 * IOCTL with two args (socket and request "key")
 *
 */
extern
ERL_NIF_TERM esaio_ioctl2(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req)
{
  switch (req) {

#if defined(FIONREAD)
  case FIONREAD:
      return esaio_ioctl_fionread(env, descP);
      break;
#endif

#if defined(SIOCATMARK)
  case SIOCATMARK:
      return esaio_ioctl_siocatmark(env, descP);
      break;
#endif

  default:
      return esock_make_error(env, esock_atom_enotsup);
      break;
  }

}


#if defined(FIONREAD)
static
ERL_NIF_TERM esaio_ioctl_fionread(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
    u_long       n     = 0;
    DWORD        ndata = 0; // We do not actually use this
    int          res;
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_ioctl_fionread(%d) -> entry\r\n", descP->sock) );

    res = sock_ioctl2(descP->sock, FIONREAD, NULL, 0, &n, sizeof(n), &ndata);
    (void) ndata;

    if (res != 0) {
        int          save_errno = sock_errno();
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP,
               ("WIN-ESAIO", "esaio_ioctl_fionread(%d) -> failure: "
                "\r\n      reason: %T"
                "\r\n", descP->sock, reason) );

        result = esock_make_error(env, reason);

    } else {

        result = esock_encode_ioctl_ivalue(env, descP, n);

    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_ioctl_fionread(%d) -> done with: "
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    return result;
}
#endif


/* For a stream socket that has been configured for inline reception of any
 * OOB data (SO_OOBINLINE), tests if there is any OOB data waiting to be read.
 * Returns TRUE if there data waiting to be read, FALSE otherwise.
 */
#if defined(SIOCATMARK)
static
ERL_NIF_TERM esaio_ioctl_siocatmark(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    int          b     = 0;
    DWORD        ndata = 0; // We do not actually use this
    int          res;
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_ioctl_siocatmark(%d) -> entry\r\n",
            descP->sock) );

    res = sock_ioctl2(descP->sock, SIOCATMARK, NULL, 0, &b, sizeof(b), &ndata);
    (void) ndata;

    if (res != 0) {
        int          save_errno = sock_errno();
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP,
               ("WIN-ESAIO", "esaio_ioctl_siocatmark(%d) -> failure: "
                "\r\n      reason: %T"
                "\r\n", descP->sock, reason) );

        result = esock_make_error(env, reason);

    } else {

        result = esock_encode_ioctl_bvalue(env, descP, b);

    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_ioctl_siocatmark(%d) -> done with: "
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    return result;
}
#endif




/* ====================================================================
 *
 * The "worker" thread of the I/O Completion Port thread pool.
 * Shall each thread have its own environment?
 *
 * ====================================================================
 */

static
void* esaio_completion_main(void* threadDataP)
{
    char             envName[64]; /* Used for building the (env-) name */
    BOOLEAN_T        done  = FALSE;
    ESAIOThreadData* dataP = (ESAIOThreadData*) threadDataP;
    ESockDescriptor* descP = NULL;
    ESAIOOperation*  opP;
    OVERLAPPED*      olP;
    BOOL             res;
    DWORD            numBytes, flags = 0;
    int              save_errno;
 
    SGDBG( ("WIN-ESAIO", "esaio_completion_main -> entry\r\n") );

    dataP->state = ESAIO_THREAD_STATE_INITIATING;

    sprintf(envName, "esaio-completion-main[%d]", dataP->id);
    dataP->env = esock_alloc_env(envName);

    dataP->state = ESAIO_THREAD_STATE_OPERATIONAL;

    SGDBG( ("WIN-ESAIO", "esaio_completion_main -> initiated\r\n") );

    while (!done) {
        /*
         * If this function *fails*, return value FALSE, the (out-) arguments:
         *   - lpNumberOfBytes (numBytes)
         *   - lpCompletionKey (descP)
         *   - lpOverlapped    (olP)
         * *can* contain particular value combinations as follows:
         *
         *   * If *lpOverlapped is NULL, the function did not dequeue a
         *     completion packet from the completion port.
         *     In this case, the function does not store information in the
         *     variables pointed to by the lpNumberOfBytes and lpCompletionKey
         *     parameters, and their values are indeterminate.
         *
         *   * If *lpOverlapped is not NULL and the function dequeues a
         *     completion packet for a failed I/O operation from the
         *     completion port, the function stores information about the
         *     failed operation in the variables pointed to by lpNumberOfBytes,
         *     lpCompletionKey, and lpOverlapped.
         *     To get extended error information, call GetLastError.
         *
         */

        SGDBG( ("WIN-ESAIO",
                "esaio_completion_main -> [%d] try dequeue packet\r\n",
                dataP->cnt) );

        res = GetQueuedCompletionStatus(ctrl.cport,
                                        &numBytes,
                                        (PULONG_PTR) &descP,
                                        &olP,
                                        INFINITE);
        save_errno = NO_ERROR;

        if (!res) {

            save_errno = sock_errno(); // Details

            if (olP == NULL) {

                /* First alt.
                 * What shall we do here? Quit? Try again?
                 */
                   
                SGDBG( ("WIN-ESAIO",
                        "esaio_completion_main -> [failure 1]"
                        "\r\n   %s (%d)"
                        "\r\n", erl_errno_id(save_errno), save_errno) );

                dataP->state = ESAIO_THREAD_STATE_TERMINATING;
                dataP->error = ESAIO_THREAD_ERROR_GET;
                opP          = NULL;
                done         = TRUE;
                break;

            } else {

                /* Second alt.
                 * Dequeued a complete packet for a *failed* I/O operation.
                 */

                SGDBG( ("WIN-ESAIO",
                        "esaio_completion_main -> [failure 2] "
                        "\r\n   %s (%d)"
                        "\r\n", erl_errno_id(save_errno), save_errno) );

                opP = CONTAINING_RECORD(olP, ESAIOOperation, ol);
                esaio_completion_inc(dataP);

            }
        } else {
            opP = CONTAINING_RECORD(olP, ESAIOOperation, ol);
            esaio_completion_inc(dataP);

            SGDBG( ("WIN-ESAIO", "esaio_completion_main -> success\r\n") );

        } /* if (!res) */

        dataP->latest = opP->tag;

        switch (opP->tag) {
        case ESAIO_OP_TERMINATE:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received terminate cmd\r\n") );
            done = esaio_completion_terminate(dataP, (OVERLAPPED*) opP);
            break;

        case ESAIO_OP_CONNECT:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received connect cmd\r\n") );
            done = esaio_completion_connect(dataP, descP, (OVERLAPPED*) opP,
                                            opP->env, &opP->caller,
                                            &opP->data.connect,
                                            save_errno);
            break;

        case ESAIO_OP_ACCEPT:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received accept cmd\r\n") );
            done = esaio_completion_accept(dataP, descP, (OVERLAPPED*) opP,
                                           opP->env, &opP->caller,
                                           &opP->data.accept,
                                           save_errno);
            break;

        case ESAIO_OP_SEND:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received send cmd\r\n") );
            done = esaio_completion_send(dataP, descP, (OVERLAPPED*) opP,
                                         opP->env, &opP->caller,
                                         &opP->data.send,
                                         save_errno);
            break;

        case ESAIO_OP_SENDTO:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received sendto cmd\r\n") );
            done = esaio_completion_sendto(dataP, descP, (OVERLAPPED*) opP,
                                           opP->env, &opP->caller,
                                           &opP->data.sendto,
                                           save_errno);
            break;

        case ESAIO_OP_SENDMSG:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received sendmsg cmd\r\n") );
            done = esaio_completion_sendmsg(dataP, descP, (OVERLAPPED*) opP,
                                            opP->env, &opP->caller,
                                            &opP->data.sendmsg,
                                            save_errno);
            break;

        case ESAIO_OP_RECV:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received recv cmd\r\n") );
            done = esaio_completion_recv(dataP, descP, (OVERLAPPED*) opP,
                                         opP->env, &opP->caller,
                                         &opP->data.recv,
                                         save_errno);
            break;

        case ESAIO_OP_RECVFROM:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received recvfrom cmd\r\n") );
            done = esaio_completion_recvfrom(dataP, descP, (OVERLAPPED*) opP,
                                             opP->env, &opP->caller,
                                             &opP->data.recvfrom,
                                             save_errno);
            break;

        case ESAIO_OP_RECVMSG:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received recvmsg cmd\r\n") );
            done = esaio_completion_recvmsg(dataP, descP, (OVERLAPPED*) opP,
                                            opP->env, &opP->caller,
                                            &opP->data.recvmsg,
                                            save_errno);
            break;

        default:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received unknown cmd: "
                    "\r\n   %d"
                    "\r\n",
                    opP->tag) );
            done = esaio_completion_unknown(dataP, descP, (OVERLAPPED*) opP,
                                            numBytes, save_errno);
            break;

        }

        FREE(opP);

    } /* while (!done) */

    SGDBG( ("WIN-ESAIO", "esaio_completion_main -> terminating\r\n") );

    TEXIT(threadDataP);

    SGDBG( ("WIN-ESAIO", "esaio_completion_main -> terminated\r\n") );

    dataP->state = ESAIO_THREAD_STATE_TERMINATED;

    SGDBG( ("WIN-ESAIO", "esaio_completion_main -> done\r\n") );

    return threadDataP;
}


/* *** esaio_completion_terminate ***
 *
 * We are done
 *
 */
static
BOOLEAN_T  esaio_completion_terminate(ESAIOThreadData* dataP,
                                      OVERLAPPED*      ovl)
{
    (void) ovl;

    dataP->state = ESAIO_THREAD_STATE_TERMINATING;
    dataP->error = ESAIO_THREAD_ERROR_CMD;

    return TRUE;
}


/* *** esaio_completion_connect ***
 *
 * Handle a completed 'connect' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:connect(Socket, ..., nowait), the connect is
 * scheduled, and then just as it has completed, but before this
 * thread has been activated to handle the 'connect completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 */
static
BOOLEAN_T esaio_completion_connect(ESAIOThreadData*    dataP,
                                   ESockDescriptor*    descP,
                                   OVERLAPPED*         ovl,
                                   ErlNifEnv*          opEnv,
                                   ErlNifPid*          opCaller,
                                   ESAIOOpDataConnect* opDataP,
                                   int                 error)
{
    ErlNifEnv*   env = dataP->env;
    ERL_NIF_TERM reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_connect(%d) -> entry\r\n",
            descP->sock, error) );

    (void) opCaller;

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_connect(%d) -> success"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_connect_success(env, descP, opDataP);

        MUNLOCK(descP->writeMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_connect(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_connect_aborted(env, descP, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        /* We do not know what this is
         * but we can "assume" that the request failed so we need to
         * remove it from the "queue" if its still there...
         * And cleanup...
         */
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_connect(%d) -> unknown failure:"
                "\r\n   %T"
                "\r\n", descP->sock, ENO2T(env, error)) );
        MLOCK(descP->writeMtx);

        esaio_completion_connect_failure(env, descP, opDataP, error);

        MUNLOCK(descP->writeMtx);
        break;
    }

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_connect -> clear and delete op env\r\n") );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_connect", opEnv);
    esock_free_env("esaio_completion_connect", opEnv);

    SGDBG( ("WIN-ESAIO", "esaio_completion_connect -> done\r\n") );

    return FALSE;
}



/* *** esaio_completion_connect_success ***
 * The 'connect' operation was successful.
 */
static
void esaio_completion_connect_success(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ESAIOOpDataConnect* opDataP)
{
    if (descP->connectorP != NULL) {
        if (IS_OPEN(descP->writeState)) {
            esaio_completion_connect_completed(env, descP, opDataP);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */

            /* Clean up the connector stuff, no need for that anymore */
            esock_requestor_release("esaio_completion_connect_success -> "
                                    "not active",
                                    env, descP, &descP->connector);
            descP->connectorP = NULL;

            descP->writeState &=
                ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

            esaio_completion_connect_not_active(descP);
        }
    } else {
        /* Connect was actually completed directly
         * (and 'connector' was therefor not initiated)
         * => Nothing to do here, other than cleanup.
         */
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }
}



/* *** esaio_completion_connect_aborted ***
 * The 'connect' operation was aborted.
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_connect_aborted(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ESAIOOpDataConnect* opDataP)
{
    if (descP->connectorP != NULL) {

        ERL_NIF_TERM reason = esock_atom_closed;

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             descP->connectorP, reason);

        /* Clean up the connector stuff, no need for that anymore */
        esock_requestor_release("connect_stream_check_result -> abort",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

        /* The socket not being open (assumed closing),
         * means we are in the closing phase...
         */
        if (! IS_OPEN(descP->writeState)) {

            esaio_stop(env, descP);

        }
    } else {
        descP->writeState &= ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
    }
}


/* *** esaio_completion_connect_failure *
 * A "general" failure happened while performing the 'connect' operation.
 */
static
void esaio_completion_connect_failure(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ESAIOOpDataConnect* opDataP,
                                      int                 error)
{
    if (descP->connectorP != NULL) {
        /* Figure out the reason */
        ERL_NIF_TERM reason = MKT2(env,
                                   esock_atom_completion_status,
                                   ENO2T(env, error));
        
        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             descP->connectorP, reason);
        esaio_completion_connect_fail(env, descP, error, FALSE);

        /* Clean up the connector stuff, no need for that anymore */
        esock_requestor_release("connect_stream_check_result -> failure",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

    } else {
        esaio_completion_connect_fail(env, descP, error, TRUE);
    }
}


/* *** esaio_completion_connect_completed ***
 * The connect request has completed.
 */
static
void esaio_completion_connect_completed(ErlNifEnv*          env,
                                        ESockDescriptor*    descP,
                                        ESAIOOpDataConnect* opDataP)
{
    ERL_NIF_TERM completionStatus, completionInfo;
    int          ucres;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_connect_completed(%d) -> "
            "success - try update context\r\n", descP->sock) );

    ucres = ESAIO_UPDATE_CONNECT_CONTEXT( descP->sock );
    
    if (ucres == 0) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_connect_completed({%d) -> success\r\n",
                descP->sock) );

        descP->writeState &= ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
        descP->writeState |= ESOCK_STATE_CONNECTED;

        completionStatus   = esock_atom_ok;

    } else {

        /* It is actually possible that this is an error "we do not know"
         * which will result in the atom 'unknown', which is not very useful...
         * So, we should really test if is 'unknown' and if so use the actual
         * value (the integer) instead.
         */
        int          save_errno = sock_errno();
        ERL_NIF_TERM tag        = esock_atom_update_connect_context;
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP, ("WIN-ESAIO",
                       "esaio_completion_connect_completed(%d) -> "
                       "failed update connect context: %T\r\n",
                       descP->sock, reason) );

        descP->writeState = ESOCK_STATE_CLOSED;

        sock_close(descP->sock);

        completionStatus = esock_make_error_t2r(descP->connector.env,
                                                tag, reason);

    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_connect_completed {%d} -> "
            "completion status: %T\r\n",
            descP->sock, completionStatus) );

    completionInfo = MKT2(descP->connector.env,
                          descP->connector.ref,
                          completionStatus);

    /* Send a 'connect' completion message */
    esaio_send_completion_msg(env,
                              descP,
                              &descP->connector.pid,
                              descP->connector.env,
                              CP_TERM(descP->connector.env, opDataP->sockRef),
                              completionInfo);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_connect_completed {%d} -> cleanup\r\n",
            descP->sock) );

    /* Clean up the connector stuff, no need for that anymore */
    esock_requestor_release("esaio_completion_connect_completed",
                            env, descP, &descP->connector);
    descP->connectorP = NULL;

}



/* *** esaio_completion_connect_not_active ***
 * A connect has completed but the operation is no longer valid.
 */
static
void esaio_completion_connect_not_active(ESockDescriptor* descP)
{
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_connect_not_active -> "
            "success for cancelled connect\r\n") );

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.unexpectedConnects, 1);

    MUNLOCK(ctrl.cntMtx);

}



/* *** esaio_completion_connect_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_connect_fail(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              error,
                                   BOOLEAN_T        inform)
{
    descP->writeState &= ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
    esaio_completion_fail(env, descP, "connect", error, inform);
}




/* === accept 'stuff' === */

/* *** esaio_completion_accept ***
 *
 * Handle a completed 'accept' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:accept(Socket, ..., nowait), the accept is
 * scheduled, and then just as it has completed, but before this
 * thread has been activated to handle the 'accept completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 */
static
BOOLEAN_T esaio_completion_accept(ESAIOThreadData*   dataP,
                                  ESockDescriptor*   descP,
                                  OVERLAPPED*        ovl,
                                  ErlNifEnv*         opEnv,
                                  ErlNifPid*         opCaller,
                                  ESAIOOpDataAccept* opDataP,
                                  int                error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_accept(%d) -> entry with"
            "\r\n   error: %s (%d)"
            "\r\n", descP->sock, erl_errno_id(error), error) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_accept(%d) -> success"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_accept_success(env, descP, opEnv, opCaller, opDataP);

        MUNLOCK(descP->readMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_accept(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_accept_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        /* We do not know what this is
         * but we can "assume" that the request failed so we need to
         * remove it from the "queue" if its still there...
         * And cleanup...
         */
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_accept(%d) -> unknown failure"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_accept_failure(env, descP, opCaller, opDataP, error);

        MUNLOCK(descP->readMtx);
        break;
    }

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_accept -> clear and delete op env\r\n") );

    /* "Manually" allocated buffer */
    FREE( opDataP->buf );    

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_accept - op cleanup", opEnv);
    esock_free_env("esaio_completion_accept - op cleanup", opEnv);
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_accept -> done\r\n") );

    return FALSE;

}


/* *** esaio_completion_accept_success ***
 * The 'accept' operation was successful.
 */
static
void esaio_completion_accept_success(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     ErlNifEnv*         opEnv,
                                     ErlNifPid*         opCaller,
                                     ESAIOOpDataAccept* opDataP)
{
    ESockRequestor req;

    if (esock_acceptor_get(env, descP,
                           &opDataP->accRef,
                           opCaller,
                           &req)) {
        if (IS_OPEN(descP->readState)) {
            esaio_completion_accept_completed(env, descP,
                                              opEnv, opCaller, opDataP,
                                              &req);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_accept_not_active(descP);
        }
    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         * => But we do not free the "buffer" since it was "used up"
         *    when we (as assumed) got the result (directly)...
         */
    }

    /* *Maybe* update socket (read) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_success(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->acceptorsQ.first == NULL)), descP->readState) );
    if (descP->acceptorsQ.first == NULL) {
        descP->readState &= ~(ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);
    }
}


/* *** esaio_completion_accept_aborted ***
 * The 'accept' operation was aborted.
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_accept_aborted(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     ErlNifPid*         opCaller,
                                     ESAIOOpDataAccept* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_acceptor_get(env, descP,
                           &opDataP->accRef,
                           opCaller,
                           &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_accept_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->lSockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (read) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->readState)))) );

    if (! IS_OPEN(descP->readState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->acceptorsQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->readersQ.first == NULL) &&
                (descP->writersQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_accept_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    /* *Maybe* update socket (read) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_aborted(%d) -> "
            "maybe (%s) update (read) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->acceptorsQ.first == NULL)), descP->readState) );
    if (descP->acceptorsQ.first == NULL) {
        descP->readState &= ~(ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);
    }

}


/* *** esaio_completion_accept_failure *
 * A "general" failure happened while performing the 'accept' operation.
 */
static
void esaio_completion_accept_failure(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     ErlNifPid*         opCaller,
                                     ESAIOOpDataAccept* opDataP,
                                     int                error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    if (esock_acceptor_get(env, descP,
                           &opDataP->accRef,
                           opCaller,
                           &req)) {
            
        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->lSockRef,
                             &req, reason);
        esaio_completion_accept_fail(env, descP, error, FALSE);
    } else {
        esaio_completion_accept_fail(env, descP, error, TRUE);
    }

    /* *Maybe* update socket (read) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_failure(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->acceptorsQ.first == NULL)), descP->readState) );
    if (descP->acceptorsQ.first == NULL) {
        descP->readState &= ~(ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);
    }

}


/* *** esaio_completion_accept_completed ***
 * The accept request has completed.
 */
static
void esaio_completion_accept_completed(ErlNifEnv*         env,
                                       ESockDescriptor*   descP,
                                       ErlNifEnv*         opEnv,
                                       ErlNifPid*         opCaller,
                                       ESAIOOpDataAccept* opDataP,
                                       ESockRequestor*    reqP)
{
    ERL_NIF_TERM     completionStatus, completionInfo;
    int              ucres;
    ESockDescriptor* accDescP;
    ERL_NIF_TERM     accRef, accSocket;

    ESOCK_ASSERT( DEMONP("esaio_completion_accept_completed - acceptor",
                         env, descP, &reqP->mon) == 0);

    /* We need to make sure peername and sockname works! */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_completed -> "
            "success - try update context\r\n") );

    ucres = ESAIO_UPDATE_ACCEPT_CONTEXT( opDataP->asock, opDataP->lsock );

    if (ucres == 0) {

        int save_errno;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_accept_completed -> "
                "create (accepted) descriptor\r\n") );

        accDescP = esock_alloc_descriptor(opDataP->asock);

        if (ESAIO_OK != (save_errno = esaio_add_socket(accDescP))) {
            // See esock_dtor for what needs done!
            ERL_NIF_TERM tag    = esock_atom_add_socket;
            ERL_NIF_TERM reason = ENO2T(opEnv, save_errno);

            ESOCK_CNT_INC(env, descP, CP_TERM(env, opDataP->lSockRef),
                          esock_atom_acc_fails, &descP->accFails, 1);

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_accept_completed -> "
                    "failed adding (accepted) socket to completion port: "
                    "%T\r\n", reason) );

            esock_dealloc_descriptor(env, accDescP);
            sock_close(opDataP->asock);

            /* This should really be:
             *     {error, {invalid, {add_to_completion_port, Reason}}}
             */

            completionStatus = esock_make_error_t2r(opEnv, tag, reason);

        } else {

            ESOCK_CNT_INC(env, descP, CP_TERM(env, opDataP->lSockRef),
                          esock_atom_acc_success, &descP->accSuccess, 1);

            accDescP->domain   = descP->domain;
            accDescP->type     = descP->type;
            accDescP->protocol = descP->protocol;

            MLOCK(descP->writeMtx);

            accDescP->rBufSz   = descP->rBufSz;  // Inherit buffer size
            accDescP->rCtrlSz  = descP->rCtrlSz; // Inherit buffer size
            accDescP->wCtrlSz  = descP->wCtrlSz; // Inherit buffer size
            accDescP->iow      = descP->iow;     // Inherit iow
            accDescP->dbg      = descP->dbg;     // Inherit debug flag
            accDescP->useReg   = descP->useReg;  // Inherit useReg flag
            esock_inc_socket(accDescP->domain, accDescP->type,
                             accDescP->protocol);

            accRef = enif_make_resource(env, accDescP);
            enif_release_resource(accDescP);
            accSocket = esock_mk_socket(opEnv, CP_TERM(opEnv, accRef));
                
            accDescP->ctrlPid = *opCaller;

            ESOCK_ASSERT( MONP("esaio_completion_accept_completed -> ctrl",
                               env, accDescP,
                               &accDescP->ctrlPid,
                               &accDescP->ctrlMon) == 0 );

            accDescP->writeState |= ESOCK_STATE_CONNECTED;

            MUNLOCK(descP->writeMtx);

            /* And finally (maybe) update the registry */
            if (descP->useReg)
                esock_send_reg_add_msg(env, descP, accRef);

            completionStatus = esock_make_ok2(opEnv, accSocket);

        }

    } else {

        /* It is actually possible that this is an error "we do not know"
         * which will result in the atom 'unknown', which is not very useful...
         * So, we should really test if is 'unknown' and if so use the actual
         * value (the integer) instead.
         */
        int          save_errno = sock_errno();
        ERL_NIF_TERM tag        = esock_atom_update_accept_context;
        ERL_NIF_TERM reason     = ENO2T(env, save_errno);

        SSDBG( descP, ("WIN-ESAIO",
                       "esaio_completion_accept_completed(%d) -> "
                       "accept context update failed: %T (%d)\r\n",
                       descP->sock, reason, save_errno) );

        sock_close(descP->sock);
        descP->writeState = ESOCK_STATE_CLOSED;

        completionStatus = esock_make_error_t2r(opEnv, tag, reason);

    }

    completionInfo = MKT2(opEnv, opDataP->accRef, completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, opCaller), completionInfo) );

    /* Send a 'accept' completion message */
    esaio_send_completion_msg(env,               // Send env
                              descP,             // Descriptor
                              opCaller,          // Msg destination
                              opEnv,             // Msg env
                              opDataP->lSockRef, // Dest socket
                              completionInfo);   // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_accept_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_accept_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_accept_completed -> req cleanup",
                   reqP->env);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_accept_completed -> done\r\n") );
}



/* *** esaio_completion_accept_not_active ***
 * A accept request has completed but the request is no longer valid.
 */
static
void esaio_completion_accept_not_active(ESockDescriptor* descP)
{
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_not_active(%d) -> "
            "success for not active accept request\r\n", descP->sock) );

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.unexpectedAccepts, 1);

    MUNLOCK(ctrl.cntMtx);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_not_active(%d) -> done\r\n",
            descP->sock) );

}


/* *** esaio_completion_accept_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_accept_fail(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              error,
                                  BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "accept", error, inform);
}




/* === send 'stuff' === */

/* *** esaio_completion_send ***
 *
 * Handle a completed 'send' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:send(Socket, ..., nowait), the send is scheduled,
 * and then just as it has completed, but before this
 * thread has been activated to handle the 'send completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 *
 * We need to use 'WSAGetOverlappedResult' to actually figure out the
 * "transfer result" (how much was sent).
 */
static
BOOLEAN_T esaio_completion_send(ESAIOThreadData* dataP,
                                ESockDescriptor* descP,
                                OVERLAPPED*      ovl,
                                ErlNifEnv*       opEnv,
                                ErlNifPid*       opCaller,
                                ESAIOOpDataSend* opDataP,
                                int              error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_send(%d) -> entry with"
            "\r\n   error: %T"
            "\r\n", descP->sock, ENO2T(env, error)) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_send(%d) -> no error"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_send_success(env, descP, ovl, opEnv,
                                      opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_send_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send(%d) -> operation unknown failure"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_send_failure(env, descP, opCaller, opDataP, error);
        
        MUNLOCK(descP->writeMtx);
        break;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send(%d) -> cleanup\r\n", descP->sock) );

    FREE( opDataP->wbuf.buf );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_send - op cleanup", opEnv);
    esock_free_env("esaio_completion_send - op cleanup", opEnv);
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_send(%d) -> done\r\n",
            descP->sock) );

    return FALSE;

}



/* *** esaio_completion_send_success ***
 * The 'send' operation was successful.
 */
static
void esaio_completion_send_success(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   OVERLAPPED*      ovl,
                                   ErlNifEnv*       opEnv,
                                   ErlNifPid*       opCaller,
                                   ESAIOOpDataSend* opDataP)
{
    ESockRequestor req;

    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->writeState)) {
            esaio_completion_send_completed(env, descP, ovl, opEnv,
                                            opCaller,
                                            opDataP->sockRef,
                                            opDataP->sendRef,
                                            opDataP->wbuf.len,
                                            &req);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_send_not_active(descP);
        }

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_success(%d) -> "
            "maybe (%s) update (write) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}



/* *** esaio_completion_send_aborted ***
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_send_aborted(ErlNifEnv*         env,
                                     ESockDescriptor* descP,
                                     ErlNifPid*       opCaller,
                                     ESAIOOpDataSend* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (write) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->writeState)))) );

    if (! IS_OPEN(descP->writeState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->writersQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->readersQ.first == NULL) &&
                (descP->acceptorsQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_send_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_aborted(%d) -> "
            "maybe (%s) update (write) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}



/* *** esaio_completion_send_failure *
 * A "general" failure happened while performing the 'send' operation.
 */
static
void esaio_completion_send_failure(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ErlNifPid*       opCaller,
                                   ESAIOOpDataSend* opDataP,
                                   int              error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    /* We do not know what this is
     * but we can "assume" that the request failed so we need to
     * remove it from the "queue" if its still there...
     * And cleanup...
     */
    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {

        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);
        esaio_completion_send_fail(env, descP, error, FALSE);

    } else {
        esaio_completion_send_fail(env, descP, error, TRUE);
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_failure(%d) -> "
            "maybe (%s) update (write) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_send_completed ***
 * The send request has completed.
 */
static
void esaio_completion_send_completed(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     OVERLAPPED*      ovl,
                                     ErlNifEnv*       opEnv,
                                     ErlNifPid*       sender,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     sendRef,
                                     DWORD            toWrite,
                                     ESockRequestor*  reqP)
{
    ERL_NIF_TERM completionStatus, completionInfo;
    DWORD        written;

    ESOCK_ASSERT( DEMONP("esaio_completion_send_completed - sender",
                         env, descP, &reqP->mon) == 0);

    /* Success, but we need to check how much we actually got.
     * Also the 'flags' (which we currentöy ignore)
     *
     * CompletionStatus = ok | {ok, RestData}
     * CompletionInfo   = {ConnRef, CompletionStatus}
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_completed ->"
            "success - try get overlapped result\r\n") );

    if (get_send_ovl_result(descP->sock, ovl, &written)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send_completed -> overlapped result: "
                "\r\n   written:     %d"
                "\r\n   buffer size: %d"
                "\r\n", written, toWrite) );

        if (written == toWrite) {

            /* Sent it all => done */

            completionStatus = esaio_completion_send_done(env,
                                                          descP, sockRef,
                                                          written);

        } else {

            /* Only send part of the data =>
             * needs splitting and (maybe) retry (its up to the caller)!
             */

            completionStatus = esaio_completion_send_partial(env,
                                                             descP,
                                                             sockRef,
                                                             written);
        }

    } else {

        int save_errno = sock_errno();

        /* Now what?
         * We know we wrote "something" but we cannot figure out
         * how much...
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send_completed -> "
                "overlapped result failure: %d\r\n", save_errno) );

        completionStatus =
            esaio_completion_get_ovl_result_fail(env, descP, save_errno);
    }

    completionInfo = MKT2(env, CP_TERM(env, sendRef), completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, sender), completionInfo) );

    /* Send a 'send' completion message */
    esaio_send_completion_msg(env,             // Send env
                              descP,           // Descriptor
                              sender,          // Msg destination
                              opEnv,           // Msg env
                              sockRef,         // Dest socket
                              completionInfo); // Info

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_send_completed -> done\r\n") );
}



/* *** esaio_completion_send_done ***
 *
 * A complete write (the entire buffer was sent).
 *
 */
static
ERL_NIF_TERM esaio_completion_send_done(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        DWORD            written)
{
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_pkg, &descP->writePkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_byte, &descP->writeByteCnt, written);

    if (written > descP->writePkgMax)
        descP->writePkgMax = written;

    return esock_atom_ok;
}



/* *** esaio_completion_send_partial ***
 *
 * A partial send, that is only part of the buffer was used.
 *
 */
static
ERL_NIF_TERM esaio_completion_send_partial(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef,
                                           DWORD            written)
{
    if (written > 0) {

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_write_pkg, &descP->writePkgCnt, 1);
        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_write_byte, &descP->writeByteCnt, written);

        if (written > descP->writePkgMax)
            descP->writePkgMax = written;

    }

    return esock_make_ok2(env, MKI64(env, written));

}



/* *** esaio_completion_send_not_active ***
 * A send request has completed but the request is no longer valid.
 */
static
void esaio_completion_send_not_active(ESockDescriptor* descP)
{
    /* This send request is *not* "active"!
     * The send (send, sendto, sendmsg) operation
     * has been (most likely) cancelled => cleanup.
     * If the op failed, its safe to assume that the error is
     * the result of the cancellation, and we do not actually
     * need to do anything here.
     * If however, the send succeeded, we need to do "some 
     * cleanup".
     * But what can we do here?
     * Send an abort message to the sender or/and owner?
     * Increment a counter (unexpected acceptssends)?
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_not_active(%d) -> "
            "success for not active send request\r\n",
            descP->sock) );

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.unexpectedWrites, 1);

    MUNLOCK(ctrl.cntMtx);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_send_not_active(%d) -> done\r\n",
            descP->sock) );

}



/* *** esaio_completion_send_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_send_fail(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              error,
                                BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "send", error, inform);
}



/* *** esaio_completion_sendto ***
 *
 * Handle a completed 'sendto' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:sendto(Socket, ..., nowait), the send is scheduled,
 * and then just as it has completed, but before this
 * thread has been activated to handle the 'send completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 *
 * We need to use 'WSAGetOverlappedResult' to actually figure out the
 * "transfer result" (how much was sent).
 */
static
BOOLEAN_T esaio_completion_sendto(ESAIOThreadData*   dataP,
                                  ESockDescriptor*   descP,
                                  OVERLAPPED*        ovl,
                                  ErlNifEnv*         opEnv,
                                  ErlNifPid*         opCaller,
                                  ESAIOOpDataSendTo* opDataP,
                                  int                error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_sendto(%d) -> entry"
            "\r\n   error: %T"
            "\r\n", descP->sock, ENO2T(env, error)) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_sendto(%d) -> no error"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_sendto_success(env, descP, ovl, opEnv,
                                        opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_sendto(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_sendto_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_sendto(%d) -> operation unknown failure"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_sendto_failure(env, descP, opCaller, opDataP, error);

        MUNLOCK(descP->writeMtx);
        break;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendto(%d) -> cleanup\r\n",
            descP->sock) );

    FREE( opDataP->wbuf.buf );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_sendto - op cleanup", opEnv);
    esock_free_env("esaio_completion_sendto - op cleanup", opEnv);
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_sendto(%d) -> done\r\n",
            descP->sock) );

    return FALSE;
}



/* *** esaio_completion_sendto_suuccess *** */
static
void esaio_completion_sendto_success(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     OVERLAPPED*        ovl,
                                     ErlNifEnv*         opEnv,
                                     ErlNifPid*         opCaller,
                                     ESAIOOpDataSendTo* opDataP)
{
    ESockRequestor req;

    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->writeState)) {
            esaio_completion_send_completed(env, descP, ovl, opEnv,
                                            opCaller,
                                            opDataP->sockRef,
                                            opDataP->sendRef,
                                            opDataP->wbuf.len,
                                            &req);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_send_not_active(descP);
        }

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendto_success(%d) -> "
            "maybe (%s) update (write) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}



/* *** esaio_completion_sendto_aborted ***
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_sendto_aborted(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     ErlNifPid*         opCaller,
                                     ESAIOOpDataSendTo* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendto_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_sendto_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendto_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (write) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->writeState)))) );

    if (! IS_OPEN(descP->writeState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->writersQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->readersQ.first == NULL) &&
                (descP->acceptorsQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_sendto_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendto_aborted(%d) -> "
            "maybe (%s) update (write) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }
}


/* *** esaio_completion_sendto_failure *
 * A "general" failure happened while performing the 'sendto' operation.
 */
static
void esaio_completion_sendto_failure(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     ErlNifPid*         opCaller,
                                     ESAIOOpDataSendTo* opDataP,
                                     int                error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    /* We do not know what this is
     * but we can "assume" that the request failed so we need to
     * remove it from the "queue" if its still there...
     * And cleanup...
     */
    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {

        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);
        esaio_completion_sendto_fail(env, descP, error, FALSE);

    } else {
        esaio_completion_sendto_fail(env, descP, error, TRUE);
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendto_failure(%d) -> "
            "maybe (%s) update (write) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}



/* *** esaio_completion_sendto_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_sendto_fail(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              error,
                                  BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "sendto", error, inform);
}



/* *** esaio_completion_sendmsg ***
 *
 * Handle a completed 'sendmsg' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:sendto(Socket, ..., nowait), the send is scheduled,
 * and then just as it has completed, but before this
 * thread has been activated to handle the 'send completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 *
 * We need to use 'WSAGetOverlappedResult' to actually figure out the
 * "transfer result" (how much was sent).
 */
static
BOOLEAN_T esaio_completion_sendmsg(ESAIOThreadData*    dataP,
                                   ESockDescriptor*    descP,
                                   OVERLAPPED*         ovl,
                                   ErlNifEnv*          opEnv,
                                   ErlNifPid*          opCaller,
                                   ESAIOOpDataSendMsg* opDataP,
                                   int                 error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_sendmsg(%d) -> entry with"
            "\r\n   error: %T"
            "\r\n", descP->sock, ENO2T(env, error)) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_sendmsg(%d) -> no error"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_sendmsg_success(env, descP, ovl, opEnv,
                                         opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_sendmsg(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_sendmsg_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_sendmsg(%d) -> operation unknown failure"
                "\r\n", descP->sock) );
        MLOCK(descP->writeMtx);

        esaio_completion_sendmsg_failure(env, descP, opCaller, opDataP, error);

        MUNLOCK(descP->writeMtx);
        break;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendmsg(%d) -> cleanup\r\n", descP->sock) );

    /* "Manually" allocated buffers */
    FREE( opDataP->msg.lpBuffers );
    if (opDataP->ctrlBuf != NULL)
        FREE( opDataP->ctrlBuf );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_sendmsg - op cleanup", opEnv);
    esock_free_env("esaio_completion_sendmsg - op cleanup", opEnv);
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_sendmsg(%d) -> done\r\n",
            descP->sock) );

    return FALSE;

}


/* *** esaio_completion_sendmsg_suuccess *** */
static
void esaio_completion_sendmsg_success(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      OVERLAPPED*         ovl,
                                      ErlNifEnv*          opEnv,
                                      ErlNifPid*          opCaller,
                                      ESAIOOpDataSendMsg* opDataP)
{
    ESockRequestor req;

    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->writeState)) {

            DWORD toWrite = 0;

            /* Calculate how much data *in total*
             * we was supposed to write */
            for (int i = 0; i < opDataP->iovec->iovcnt; i++) {
                toWrite += opDataP->iovec->iov[i].iov_len;
            }

            esaio_completion_send_completed(env, descP, ovl, opEnv,
                                            opCaller,
                                            opDataP->sockRef,
                                            opDataP->sendRef,
                                            toWrite,
                                            &req);

        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_send_not_active(descP);
        }

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendmsg_success(%d) -> "
            "maybe (%s) update (write) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_sendmsg_aborted ***
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_sendmsg_aborted(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ErlNifPid*          opCaller,
                                      ESAIOOpDataSendMsg* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendmsg_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_sendmsg_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendmsg_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (write) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->writeState)))) );

    if (! IS_OPEN(descP->writeState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->writersQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->readersQ.first == NULL) &&
                (descP->acceptorsQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_sendmsg_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendmsg_aborted(%d) -> "
            "maybe (%s) update (write) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_sendmsg_failure *
 * A "general" failure happened while performing the 'sendmsg' operation.
 */
static
void esaio_completion_sendmsg_failure(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ErlNifPid*          opCaller,
                                      ESAIOOpDataSendMsg* opDataP,
                                      int                 error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    /* We do not know what this is
     * but we can "assume" that the request failed so we need to
     * remove it from the "queue" if its still there...
     * And cleanup...
     */
    if (esock_writer_get(env, descP,
                         &opDataP->sendRef,
                         opCaller,
                         &req)) {

        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);
        esaio_completion_sendmsg_fail(env, descP, error, FALSE);

    } else {
        esaio_completion_sendmsg_fail(env, descP, error, TRUE);
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_sendmsg_success(%d) -> "
            "maybe (%s) update (write) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->writersQ.first == NULL)), descP->writeState) );
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_sendmsg_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_sendmsg_fail(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              error,
                                   BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "sendmsg", error, inform);
}



/* === receive 'stuff' === */

/* *** esaio_completion_recv ***
 *
 * Handle a completed 'recv' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:recv(Socket, ..., nowait), the recv is scheduled,
 * and then just as it has completed, but before this
 * thread has been activated to handle the 'recv completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 */
static
BOOLEAN_T esaio_completion_recv(ESAIOThreadData* dataP,
                                ESockDescriptor* descP,
                                OVERLAPPED*      ovl,
                                ErlNifEnv*       opEnv,
                                ErlNifPid*       opCaller,
                                ESAIOOpDataRecv* opDataP,
                                int              error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recv(%d) -> entry with"
            "\r\n   error: %T"
            "\r\n", descP->sock, ENO2T(env, error)) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_recv(%d) -> no error"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_recv_success(env, descP, ovl, opEnv,
                                      opCaller, opDataP);

        MUNLOCK(descP->readMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_recv_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv(%d) -> operation unknown failure"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_recv_failure(env, descP, opCaller, opDataP, error);

        MUNLOCK(descP->readMtx);
        break;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv {%d} -> clear and delete op env\r\n",
            descP->sock) );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_recv - op cleanup", opEnv);
    esock_free_env("esaio_completion_recv - op cleanup", opEnv);
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recv(%d) -> done\r\n",
            descP->sock) );

    return FALSE;
}


static
void esaio_completion_recv_success(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   OVERLAPPED*      ovl,
                                   ErlNifEnv*       opEnv,
                                   ErlNifPid*       opCaller,
                                   ESAIOOpDataRecv* opDataP)
{
    ESockRequestor req;

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->readState)) {
            esaio_completion_recv_completed(env, descP, ovl, opEnv,
                                            opCaller, opDataP,
                                            &req);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_recv_not_active(descP);
            FREE_BIN( &opDataP->buf );
        }

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         * => But we do not free the "buffer" since it was "used up"
         *    when we (as assumed) got the result (directly)...
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_success(%d) -> "
            "maybe (%s) update (read) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recv_aborted ***
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_recv_aborted(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ErlNifPid*       opCaller,
                                   ESAIOOpDataRecv* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (read) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->readState)))) );

    if (! IS_OPEN(descP->readState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->readersQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->writersQ.first == NULL) &&
                (descP->acceptorsQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_recv_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    FREE_BIN( &opDataP->buf );

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_aborted(%d) -> "
            "maybe (%s) update (read) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recv_failure *
 * A "general" failure happened while performing the 'recv' operation.
 */
static
void esaio_completion_recv_failure(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ErlNifPid*       opCaller,
                                   ESAIOOpDataRecv* opDataP,
                                   int              error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    /* We do not know what this is
     * but we can "assume" that the request failed so we need to
     * remove it from the "queue" if its still there...
     * And cleanup...
     */
    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {
        /* Figure out the reason */
        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);
        esaio_completion_recv_fail(env, descP, error, FALSE);

    } else {
        esaio_completion_recv_fail(env, descP, error, TRUE);
    }

    FREE_BIN( &opDataP->buf );

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_failure(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }
            
}


/* *** esaio_completion_recv_completed ***
 * The recv request has completed.
 */
static
void esaio_completion_recv_completed(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     OVERLAPPED*      ovl,
                                     ErlNifEnv*       opEnv,
                                     ErlNifPid*       opCaller,
                                     ESAIOOpDataRecv* opDataP,
                                     ESockRequestor*  reqP)
{
    ERL_NIF_TERM completionStatus, completionInfo;
    DWORD        read, flags;

    ESOCK_ASSERT( DEMONP("esaio_completion_recv_completed - sender",
                         env, descP, &reqP->mon) == 0);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_completed ->"
            "success - try get overlapped result\r\n") );

    if (get_recv_ovl_result(descP->sock, ovl, &read, &flags)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_completed -> overlapped result: "
                "\r\n   read:        %d"
                "\r\n   buffer size: %d"
                "\r\n   flags:       %d"
                "\r\n", read, opDataP->buf.size, flags) );

        /* *** Success! ***
         * CompletionStatus = {ok, {Flags, Bin}} (should be)
         * CompletionInfo   = {ConnRef, CompletionStatus}
         */

        if ((read == 0) && (descP->type == SOCK_STREAM)) {

            /*
             * When a stream socket peer has performed an orderly
             * shutdown, the return value will be 0 (the traditional
             * "end-of-file" return).
             *
             * *We* do never actually try to read 0 bytes!
             */

            ESOCK_CNT_INC(env, descP, opDataP->sockRef,
                          esock_atom_read_fails, &descP->readFails, 1);

            completionStatus = esock_make_error(opEnv, esock_atom_closed);
            
        } else {

            if (read == opDataP->buf.size) {
                /* We filled the buffer => done */

                completionStatus =
                    esaio_completion_recv_done(env, descP,
                                               opEnv, opDataP,
                                               flags);

            } else {

                /* Only used a part of the buffer =>
                 * needs splitting and (maybe) retry (its up to the caller)!
                 */

                completionStatus =
                    esaio_completion_recv_partial(env, descP,
                                                  opEnv, opDataP,
                                                  reqP, read, flags);
            }

        }

    } else {

        int save_errno = sock_errno();

        /* Now what?
         * We know we read "something" but we cannot figure out
         * how much...
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_completed -> "
                "overlapped result failure: %d\r\n", save_errno) );

        completionStatus =
            esaio_completion_get_ovl_result_fail(opEnv, descP, save_errno);
    }

    completionInfo = MKT2(opEnv, opDataP->recvRef, completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, opCaller), completionInfo) );

    /* Send a 'send' completion message */
    esaio_send_completion_msg(env,                 // Send env
                              descP,               // Descriptor
                              opCaller,            // Msg destination
                              opEnv,               // Msg env
                              opDataP->sockRef,    // Socket
                              completionInfo);     // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recv_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_recv_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_recv_completed -> req cleanup",
                   reqP->env);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recv_completed -> done\r\n") );
}



/* *** esaio_completion_recv_done ***
 *
 * A complete read (filled the provided buffer).
 *
 */
static
ERL_NIF_TERM esaio_completion_recv_done(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ErlNifEnv*       opEnv,
                                        ESAIOOpDataRecv* opDataP,
                                        DWORD            flags)
{
    ERL_NIF_TERM data;
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM recvRef = opDataP->recvRef;
    DWORD        read    = opDataP->buf.size;

    (void) flags;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_done(%T) {%d} -> entry with"
            "\r\n   recvRef: %T"
            "\r\n   flags:   0x%X"
            "\r\n", sockRef, descP->sock, recvRef, flags) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(opEnv, &opDataP->buf);

    /* We ignore the flags *for now*.
     * Needs to be passed up eventually!
     *
     * This should eventually be something like:
     *
     *                {ok, {Flags, Bin}}
     *
     * But for now we skip the 'flags' part:
     *
     *                {ok, Bin}
     */
    return esock_make_ok2(opEnv, data);
}


/* *** esaio_completion_recv_partial ***
 *
 * A partial read, that is only part of the buffer was used.
 *
 */
static
ERL_NIF_TERM esaio_completion_recv_partial(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ErlNifEnv*       opEnv,
                                           ESAIOOpDataRecv* opDataP,
                                           ESockRequestor*  reqP,
                                           DWORD            read,
                                           DWORD            flags)
{
    ERL_NIF_TERM res;
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM recvRef = opDataP->recvRef;
    DWORD        toRead  = opDataP->toRead;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_partial(%T) {%d} -> entry with"
            "\r\n   toRead:  %ld"
            "\r\n   recvRef: %T"
            "\r\n   read:    %ld"
            "\r\n   flags:   0x%X"
            "\r\n", sockRef, descP->sock,
            (long) toRead, recvRef, (long) read, flags) );

    /* We only got part of what we wanted, but since we let the user
     * (or possibly the read loop in socket) decide what to do,
     * do we actually need this check? Why not just call a
     * 'esaio_completion_recv_partial' function (that splits the
     * binary and deliver what we got) and let the user sort it out?
     */

    if ((toRead == 0) ||
        (descP->type != SOCK_STREAM)) {

        /* +++ We only got a partial,           ***
         * *** but we should not wait for more. +++
         * +++ Must split it into a sub-binary. +++
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_partial(%T) {%d} -> done reading\r\n",
                sockRef, descP->sock) );

        res = esaio_completion_recv_partial_done(env, descP,
                                                 opEnv, opDataP,
                                                 read, flags);

    } else {

        /* A stream socket with specified read size
         * and not a polling read, we got a partial read
         * - return a result to initiate a retry
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_partial(%T) {%d} ->"
                " only part of data - expected more"
                "\r\n", sockRef, descP->sock) );

        res = esaio_completion_recv_partial_part(env, descP,
                                                 opEnv, opDataP,
                                                 read, flags);
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_partial(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
}



/* *** esaio_completion_recv_partial_done ***
 *
 * A successful but only partial recv, which fulfilled the required read.
 */

static
ERL_NIF_TERM esaio_completion_recv_partial_done(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ErlNifEnv*       opEnv,
                                                ESAIOOpDataRecv* opDataP,
                                                ssize_t          read,
                                                DWORD            flags)
{
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM data;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    ESOCK_ASSERT( REALLOC_BIN(&opDataP->buf, read) );
    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(opEnv, &opDataP->buf);

    (void) flags;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_partial_done(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return esock_make_ok2(opEnv, data);
}



/* *** esaio_completion_recv_partial_part ***
 *
 * A successful but only partial recv, which only partly fulfilled
 * the required read.
 * We do *not* want to risk ending up in a "never ending" read loop
 * here (by trying to read more data (and yet again getting partial)).
 * [worst case, we could up with all our worker threads busy trying
 * to read more data, and no one ready to respond to new requests].
 * So we simply return what we got to the user and let the user
 * decide what to do.
 *
 * What shall we send? {ok, Bin} | {more, Bin}
 * Presumably the user knows how much to expect, so is therefor
 * able to check:
 *
 *           "Expected > byte_size(Bin)"   -> read again
 *           "Expected =:= byte_size(Bin)" -> done
 */

static
ERL_NIF_TERM esaio_completion_recv_partial_part(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ErlNifEnv*       opEnv,
                                                ESAIOOpDataRecv* opDataP,
                                                ssize_t          read,
                                                DWORD            flags)
{
    /* This is just a "placeholder". Is this really all we need to do? */
    return esaio_completion_recv_partial_done(env, descP,
                                              opEnv, opDataP,
                                              read, flags);
}



/* *** esaio_completion_recv_not_active ***
 * A recv request has completed but the request is no longer valid.
 */
static
void esaio_completion_recv_not_active(ESockDescriptor* descP)
{
    /* This receive request is *not* "active"!
     * The receive (recv,recvfrom,recvmsg) operation
     * has been (most likely) cancelled => cleanup.
     * If the op failed, its safe to assume that the error is
     * the result of the cancellation, and we do not actually
     * need to do anything here.
     * If however, the recv succeeded, we need to do "some 
     * cleanup".
     * But what can we do here?
     * Send an abort message to the reader or/and owner?
     * Increment a counter (unexpected readsa)?
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_not_active {%d} -> "
            "success for not active read request\r\n", descP->sock) );

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.unexpectedReads, 1);

    MUNLOCK(ctrl.cntMtx);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_not_active {%d} -> done\r\n",
            descP->sock) );

}



/* *** esaio_completion_recv_closed ***
 * A recv request has completed but the socket is closed.
 * When the socket is closed, all outstanding requests
 * are "flushed", so we do not actually need to "do" anything
 * here (other then maybe count unexpected writes), maybe read bytes?.
 */
static
void esaio_completion_recv_closed(ESockDescriptor* descP,
                                  int              error)
{
    if (error == NO_ERROR) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_closed -> "
                "success for closed socket (%d)\r\n",
                descP->sock) );

        MLOCK(ctrl.cntMtx);
            
        esock_cnt_inc(&ctrl.unexpectedReads, 1);

        MUNLOCK(ctrl.cntMtx);

    }
}



/* *** esaio_completion_recv_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_recv_fail(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              error,
                                BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "recv", error, inform);
}



/* *** esaio_completion_recvfrom ***
 *
 * Handle a completed 'recvfrom' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:recvfrom(Socket, ..., nowait), the receive is scheduled,
 * and then just as it has completed, but before this
 * thread has been activated to handle the 'recv completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 */
static
BOOLEAN_T esaio_completion_recvfrom(ESAIOThreadData*     dataP,
                                    ESockDescriptor*     descP,
                                    OVERLAPPED*          ovl,
                                    ErlNifEnv*           opEnv,
                                    ErlNifPid*           opCaller,
                                    ESAIOOpDataRecvFrom* opDataP,
                                    int                  error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvfrom(%d) -> entry with"
            "\r\n   error: %T, %s (%d)"
            "\r\n",
            descP->sock, ENO2T(env, error),
            erl_errno_id(error), error) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_recvfrom(%d) -> no error"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_recvfrom_success(env, descP, ovl, opEnv,
                                          opCaller, opDataP);

        MUNLOCK(descP->readMtx);
        break;

    case ERROR_MORE_DATA:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom(%d) -> more data"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_recvfrom_more_data(env, descP,
                                            opEnv, opCaller, opDataP,
                                            error);

        MUNLOCK(descP->readMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_recvfrom_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom(%d) -> operation unknown failure"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_recvfrom_failure(env, descP, opCaller, opDataP, error);

        MUNLOCK(descP->readMtx);
        break;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom {%d} -> clear and delete op env\r\n") );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_recvfrom - op cleanup", opEnv);
    esock_free_env("esaio_completion_recvfrom - op cleanup", opEnv);
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvfrom {%d} -> done\r\n") );

    return FALSE;
}


static
void esaio_completion_recvfrom_success(ErlNifEnv*           env,
                                       ESockDescriptor*     descP,
                                       OVERLAPPED*          ovl,
                                       ErlNifEnv*           opEnv,
                                       ErlNifPid*           opCaller,
                                       ESAIOOpDataRecvFrom* opDataP)
{
    ESockRequestor req;

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->readState)) {
            esaio_completion_recvfrom_completed(env, descP,
                                                ovl, opEnv, opCaller,
                                                opDataP, &req);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_recv_not_active(descP);
            FREE_BIN( &opDataP->buf );
        }

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         * => But we do not free the "buffer" since it was "used up"
         *    when we (as assumed) got the result (directly)...
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_success(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


static
void esaio_completion_recvfrom_more_data(ErlNifEnv*           env,
                                         ESockDescriptor*     descP,
                                         ErlNifEnv*           opEnv,
                                         ErlNifPid*           opCaller,
                                         ESAIOOpDataRecvFrom* opDataP,
                                         int                  error)
{
    ESockRequestor req;

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->readState)) {
            /* We do not actually need to call this function
             * since we already know its 'more_data', but just
             * to get the same format...
             */
            ERL_NIF_TERM reason           = MKT2(env,
                                                 esock_atom_completion_status,
                                                 ENO2T(env, error));
            ERL_NIF_TERM completionStatus = esock_make_error(env, reason);
            ERL_NIF_TERM completionInfo   = MKT2(opEnv,
                                                 opDataP->recvRef,
                                                 completionStatus);

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_recvfrom_more_data(%d) -> "
                    "send completion message: "
                    "\r\n   Completion Status: %T"
                    "\r\n", descP->sock, completionStatus) );

            /* Send a 'recvfrom' completion message */
            esaio_send_completion_msg(env,              // Send env
                                      descP,            // Descriptor
                                      opCaller,         // Msg destination
                                      opEnv,            // Msg env
                                      opDataP->sockRef, // Dest socket
                                      completionInfo);  // Info

        }

        FREE_BIN( &opDataP->buf );

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         * => But we do not free the "buffer" since it was "used up"
         *    when we (as assumed) got the result (directly)...
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_more_data(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recvfrom_aborted ***
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */
static
void esaio_completion_recvfrom_aborted(ErlNifEnv*           env,
                                       ESockDescriptor*     descP,
                                       ErlNifPid*           opCaller,
                                       ESAIOOpDataRecvFrom* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (read) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->readState)))) );

    if (! IS_OPEN(descP->readState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->readersQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->writersQ.first == NULL) &&
                (descP->acceptorsQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_recvfrom_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    FREE_BIN( &opDataP->buf );

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_aborted(%d) -> "
            "maybe (%s) update (read) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recvfrom_failure *
 * A "general" failure happened while performing the 'recvfrom' operation.
 */
static
void esaio_completion_recvfrom_failure(ErlNifEnv*           env,
                                       ESockDescriptor*     descP,
                                       ErlNifPid*           opCaller,
                                       ESAIOOpDataRecvFrom* opDataP,
                                       int                  error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    /* We do not know what this is
     * but we can "assume" that the request failed so we need to
     * remove it from the "queue" if its still there...
     * And cleanup...
     */
    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {

        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);
        esaio_completion_recvfrom_fail(env, descP, error, FALSE);

    } else {
        esaio_completion_recvfrom_fail(env, descP, error, TRUE);
    }

    FREE_BIN( &opDataP->buf );

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_failure(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recvfrom_completed ***
 * The recvfrom request has completed.
 */
static
void esaio_completion_recvfrom_completed(ErlNifEnv*           env,
                                         ESockDescriptor*     descP,
                                         OVERLAPPED*          ovl,
                                         ErlNifEnv*           opEnv,
                                         ErlNifPid*           opCaller,
                                         ESAIOOpDataRecvFrom* opDataP,
                                         ESockRequestor*      reqP)
{
    ERL_NIF_TERM completionStatus, completionInfo;
    DWORD        read, flags;

    ESOCK_ASSERT( DEMONP("esaio_completion_recvfrom_completed - sender",
                         env, descP, &reqP->mon) == 0);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_completed ->"
            "success - try get overlapped result\r\n") );

    if (get_recv_ovl_result(descP->sock, ovl, &read, &flags)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom_completed -> overlapped result: "
                "\r\n   read:        %d"
                "\r\n   buffer size: %d"
                "\r\n   flags:       %d"
                "\r\n", read, opDataP->buf.size, flags) );

        /* *** Success! ***
         * CompletionStatus = {ok, {Flags, Bin}} (should be)
         * CompletionInfo   = {ConnRef, CompletionStatus}
         */

        if (read == opDataP->buf.size) {
            /* We filled the buffer => done */

            completionStatus =
                esaio_completion_recvfrom_done(env, descP,
                                               opEnv, opDataP,
                                               flags);

        } else {

            /* Only used a part of the buffer =>
             * needs splitting and (maybe) retry (its up to the caller)!
             */

            completionStatus =
                esaio_completion_recvfrom_partial(env, descP,
                                                  opEnv, opDataP,
                                                  reqP, read, flags);
        }

    } else {

        int save_errno = sock_errno();

        /* Now what?
         * We know we read "something" but we cannot figure out
         * how much...
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom_completed -> "
                "overlapped result failure: %d\r\n", save_errno) );

        completionStatus =
            esaio_completion_get_ovl_result_fail(opEnv, descP, save_errno);

        FREE_BIN( &opDataP->buf );
    }

    completionInfo = MKT2(opEnv, opDataP->recvRef, completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, opCaller), completionInfo) );

    /* Send a 'recvfrom' completion message */
    esaio_send_completion_msg(env,              // Send env
                              descP,            // Descriptor
                              opCaller,         // Msg destination
                              opEnv,            // Msg env
                              opDataP->sockRef, // Dest socket
                              completionInfo);  // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_recvfrom_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_recvfrom_completed -> req cleanup",
                   reqP->env);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvfrom_completed -> done\r\n") );
}



/* *** esaio_completion_recvfrom_done ***
 *
 * A complete read (filled the provided buffer).
 *
 */
static
ERL_NIF_TERM esaio_completion_recvfrom_done(ErlNifEnv*           env,
                                            ESockDescriptor*     descP,
                                            ErlNifEnv*           opEnv,
                                            ESAIOOpDataRecvFrom* opDataP,
                                            DWORD                flags)
{
    ERL_NIF_TERM res, data, eSockAddr;
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM recvRef = opDataP->recvRef;
    DWORD        read    = opDataP->buf.size;

    (void) flags;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_done(%T) {%d} -> entry with"
            "\r\n   recvRef: %T"
            "\r\n   flags:   0x%X"
            "\r\n", sockRef, descP->sock, recvRef, flags) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    esock_encode_sockaddr(opEnv,
                          &opDataP->fromAddr,
                          opDataP->addrLen,
                          &eSockAddr);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(opEnv, &opDataP->buf);

    /* We ignore the flags *for now*.
     * Needs to be passed up eventually!
     *
     * This should eventually be something like:
     *
     *                {ok, {Source, Flags, Data}}
     *
     * But for now we skip the 'flags' part:
     *
     *                {ok, {Source, Data}}
     */
    res = esock_make_ok2(opEnv, MKT2(opEnv, eSockAddr, data));

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_done(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
}



/* *** esaio_completion_recvfrom_partial ***
 *
 * A partial read, that is only part of the buffer was used.
 *
 */
static
ERL_NIF_TERM esaio_completion_recvfrom_partial(ErlNifEnv*           env,
                                               ESockDescriptor*     descP,
                                               ErlNifEnv*           opEnv,
                                               ESAIOOpDataRecvFrom* opDataP,
                                               ESockRequestor*      reqP,
                                               DWORD                read,
                                               DWORD                flags)
{
    ERL_NIF_TERM res, data, eSockAddr;
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM recvRef = opDataP->recvRef;

    (void) flags;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_partial(%T) {%d} -> entry with"
            "\r\n   recvRef: %T"
            "\r\n   read:    %ld"
            "\r\n   flags:   0x%X"
            "\r\n", sockRef, descP->sock, recvRef, (long) read, flags) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    esock_encode_sockaddr(opEnv,
                          &opDataP->fromAddr,
                          opDataP->addrLen,
                          &eSockAddr);

    ESOCK_ASSERT( REALLOC_BIN(&opDataP->buf, read) );
    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(opEnv, &opDataP->buf);

    /* We ignore the flags *for now*.
     * Needs to be passed up eventually!
     *
     * This should eventually be something like:
     *
     *                {ok, {Source, Flags, Data}}
     *
     * But for now we skip the 'flags' part:
     *
     *                {ok, {Source, Data}}
     */

    res = esock_make_ok2(opEnv, MKT2(opEnv, eSockAddr, data));

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_partial(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
}



/* *** esaio_completion_recvfrom_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_recvfrom_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              error,
                                    BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "recvfrom", error, inform);
}



/* *** esaio_completion_recvmsg ***
 *
 * Handle a completed 'recvmsg' (completion) request.
 * Send a 'completion' message (to requestor) with the request status.
 *
 * Completion message: 
 *     {'socket tag', socket(), completion, CompletionInfo}
 *
 *     CompletionInfo:   {CompletionHandle, CompletionStatus}
 *     CompletionHandle: reference()
 *     Result:           ok | {error, Reason}
 *
 *
 * There is a possibillity of a race here. That is, if the user
 * calls socket:recvmsg(Socket, ..., nowait), the receive is scheduled,
 * and then just as it has completed, but before this
 * thread has been activated to handle the 'recv completed'
 * the user calls socket:close(Socket) (or exits).
 * Then when this function is called, the socket is closed.
 * What to do?
 */
static
BOOLEAN_T esaio_completion_recvmsg(ESAIOThreadData*    dataP,
                                   ESockDescriptor*    descP,
                                   OVERLAPPED*         ovl,
                                   ErlNifEnv*          opEnv,
                                   ErlNifPid*          opCaller,
                                   ESAIOOpDataRecvMsg* opDataP,
                                   int                 error)
{
    ErlNifEnv*     env = dataP->env;
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvmsg(%d) -> entry with"
            "\r\n   error: %T"
            "\r\n", descP->sock, ENO2T(env, error)) );

    switch (error) {
    case NO_ERROR:
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_completion_recvmsg(%d) -> no error:"
                "\r\n   try get request %T from %T"
                "\r\n",
                descP->sock,
                opDataP->recvRef, MKPID(env, opCaller)) );
        MLOCK(descP->readMtx);

        esaio_completion_recvmsg_success(env, descP, ovl, opEnv,
                                         opCaller, opDataP);

        MUNLOCK(descP->readMtx);
        break;

    case WSA_OPERATION_ABORTED:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvmsg(%d) -> operation aborted"
                "\r\n", descP->sock) );
        /* *** SAME MTX LOCK ORDER FOR ALL OPs *** */
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        esaio_completion_recvmsg_aborted(env, descP, opCaller, opDataP);

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    default:
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvmsg(%d) -> unknown operation failure"
                "\r\n", descP->sock) );
        MLOCK(descP->readMtx);

        esaio_completion_recvmsg_failure(env, descP, opCaller, opDataP, error);

        MUNLOCK(descP->readMtx);
        break;
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg {%d} -> clear and delete op env\r\n",
            descP->sock) );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_recvmsg - op cleanup", opEnv);
    esock_free_env("esaio_completion_recvmsg - op cleanup", opEnv);
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvmsg {%d} -> done\r\n",
            descP->sock) );

    return FALSE;
}


static
void esaio_completion_recvmsg_success(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      OVERLAPPED*         ovl,
                                      ErlNifEnv*          opEnv,
                                      ErlNifPid*          opCaller,
                                      ESAIOOpDataRecvMsg* opDataP)
{
    ESockRequestor req;

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {
        if (IS_OPEN(descP->readState)) {
            esaio_completion_recvmsg_completed(env, descP, ovl, opEnv,
                                               opCaller, opDataP,
                                               &req);
        } else {
            /* A completed (active) request for a socket that is not open.
             * Is this even possible?
             * A race (completed just as the socket was closed).
             */
            esaio_completion_recv_not_active(descP);
            FREE_BIN( &opDataP->data[0] );
            FREE_BIN( &opDataP->ctrl );
        }

    } else {
        /* Request was actually completed directly
         * (and was therefor not put into the "queue")
         * => Nothing to do here, other than cleanup (see below).
         * => But we do not free the "buffer" since it was "used up"
         *    when we (as assumed) got the result (directly)...
         */
    }

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_success(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recvmsg_aborted ***
 * The only thing *we* do that could cause an abort is the
 * 'CancelIoEx' call, which we do when closing the socket
 * (or cancel a request).
 * But if we have done that;
 *   - Socket state will not be 'open' and
 *   - we have also set closer (pid and ref).
 */

static
void esaio_completion_recvmsg_aborted(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ErlNifPid*          opCaller,
                                      ESAIOOpDataRecvMsg* opDataP)
{
    ESockRequestor req;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_aborted(%d) -> "
            "try get request"
            "\r\n", descP->sock) );

    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {

        ERL_NIF_TERM reason = esock_atom_closed;

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvmsg_aborted(%d) -> "
                "send abort message to %T"
                "\r\n", descP->sock, req.pid) );

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);

    }

    /* The socket not being open (assumed closing),
     * means we are in the closing phase...
     */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_aborted(%d) -> "
            "maybe send close message => "
            "\r\n   is socket (read) open: %s"
            "\r\n",
            descP->sock, B2S((IS_OPEN(descP->readState)))) );

    if (! IS_OPEN(descP->readState)) {

        /* We can only send the 'close' message to the closer
         * when all requests has been processed!
         */

        /* Check "our" queue */
        if (descP->readersQ.first == NULL) {

            /* Check "other" queue(s) and if there is a closer pid */
            if ((descP->writersQ.first == NULL) &&
                (descP->acceptorsQ.first == NULL)) {

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_recvmsg_aborted(%d) -> "
                        "all queues are empty => "
                        "\r\n   send close message"
                        "\r\n",
                        descP->sock) );

                esaio_stop(env, descP);

            }
        }
    }

    FREE_BIN( &opDataP->data[0] );
    FREE_BIN( &opDataP->ctrl );

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_aborted(%d) -> "
            "maybe (%s) update (read) state (0x%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recvmsg_failure *
 * A "general" failure happened while performing the 'recvmsg' operation.
 */
static
void esaio_completion_recvmsg_failure(ErlNifEnv*          env,
                                      ESockDescriptor*    descP,
                                      ErlNifPid*          opCaller,
                                      ESAIOOpDataRecvMsg* opDataP,
                                      int                 error)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    /* We do not know what this is
     * but we can "assume" that the request failed so we need to
     * remove it from the "queue" if its still there...
     * And cleanup...
     */
    if (esock_reader_get(env, descP,
                         &opDataP->recvRef,
                         opCaller,
                         &req)) {

        reason = MKT2(env,
                      esock_atom_completion_status,
                      ENO2T(env, error));

        /* Inform the user waiting for a reply */
        esock_send_abort_msg(env, descP, opDataP->sockRef,
                             &req, reason);
        esaio_completion_recvmsg_fail(env, descP, error, FALSE);

    } else {
        esaio_completion_recvmsg_fail(env, descP, error, TRUE);
    }

    FREE_BIN( &opDataP->data[0] );
    FREE_BIN( &opDataP->ctrl );

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_failure(%d) -> "
            "maybe (%s) update (read) state (ox%X)\r\n",
            descP->sock,
            B2S((descP->readersQ.first == NULL)), descP->readState) );
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

}


/* *** esaio_completion_recvmsg_completed ***
 * The recvmsg request has completed.
 */
static
void esaio_completion_recvmsg_completed(ErlNifEnv*          env,
                                        ESockDescriptor*    descP,
                                        OVERLAPPED*         ovl,
                                        ErlNifEnv*          opEnv,
                                        ErlNifPid*          opCaller,
                                        ESAIOOpDataRecvMsg* opDataP,
                                        ESockRequestor*     reqP)
{
    ERL_NIF_TERM completionStatus, completionInfo;
    DWORD        read, flags;

    ESOCK_ASSERT( DEMONP("esaio_completion_recvmsg_completed - sender",
                         env, descP, &reqP->mon) == 0);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_completed ->"
            "success - try get overlapped result\r\n") );

    if (get_recv_ovl_result(descP->sock, ovl, &read, &flags)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvmsg_completed -> overlapped result: "
                "\r\n   read:        %d"
                "\r\n   buffer size: %d"
                "\r\n   flags:       %d"
                "\r\n", read, opDataP->data[0].size, flags) );

        /* *** Success! ***
         * CompletionStatus = {ok, Msg} (should be)
         * CompletionInfo   = {ConnRef, CompletionStatus}
         */

        /* We should have "calculated" the entire size before,
         * but since we have a vector of size one...
         */
        if (read == opDataP->data[0].size) {
            /* We filled the buffer => done */

            completionStatus =
                esaio_completion_recvmsg_done(env, descP,
                                              opEnv, opDataP,
                                              flags);

        } else {

            /* Only used a part of the buffer =>
             * needs splitting and (maybe) retry (its up to the caller)!
             */

            completionStatus =
                esaio_completion_recvmsg_partial(env, descP,
                                                 opEnv, opDataP,
                                                 reqP, read, flags);
        }

    } else {

        int save_errno = sock_errno();

        /* Now what?
         * We know we read "something" but we cannot figure out
         * how much...
         */
        
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvmsg_completed -> "
                "overlapped result failure: %d\r\n", save_errno) );

        completionStatus =
            esaio_completion_get_ovl_result_fail(opEnv, descP, save_errno);

    }

    completionInfo = MKT2(opEnv, opDataP->recvRef, completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, opCaller), completionInfo) );

    /* Send a 'send' completion message */
    esaio_send_completion_msg(env,              // Send env
                              descP,            // Descriptor
                              opCaller,         // Msg destination
                              opEnv,            // Msg env
                              opDataP->sockRef, // Dest socket
                              completionInfo);  // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_recvmsg_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_recvmsg_completed -> req cleanup",
                   reqP->env);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvmsg_completed -> done\r\n") );
}



/* *** esaio_completion_recvmsg_done ***
 *
 * A complete read (filled the provided buffer).
 *
 */
static
ERL_NIF_TERM esaio_completion_recvmsg_done(ErlNifEnv*          env,
                                           ESockDescriptor*    descP,
                                           ErlNifEnv*          opEnv,
                                           ESAIOOpDataRecvMsg* opDataP,
                                           DWORD               flags)
{
    ERL_NIF_TERM res, eMsg;
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM recvRef = opDataP->recvRef;
    DWORD        read    = opDataP->data[0].size;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_done(%T) {%d} -> entry with"
            "\r\n   recvRef: %T"
            "\r\n   flags:   0x%X"
            "\r\n", sockRef, descP->sock, recvRef, flags) );

    (void) flags;

    encode_msg(opEnv, descP, read,
               &opDataP->msg,
               opDataP->data,
               &opDataP->ctrl,
               &eMsg);

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    res = esock_make_ok2(opEnv, eMsg);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_done(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
}



/* *** esaio_completion_recvmsg_partial ***
 *
 * A partial read, that is only part of the buffer was used.
 *
 */
static
ERL_NIF_TERM esaio_completion_recvmsg_partial(ErlNifEnv*          env,
                                              ESockDescriptor*    descP,
                                              ErlNifEnv*          opEnv,
                                              ESAIOOpDataRecvMsg* opDataP,
                                              ESockRequestor*     reqP,
                                              DWORD               read,
                                              DWORD               flags)
{
    ERL_NIF_TERM res, eMsg;
    ERL_NIF_TERM sockRef = opDataP->sockRef;
    ERL_NIF_TERM recvRef = opDataP->recvRef;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_partial(%T) {%d} -> entry with"
            "\r\n   recvRef: %T"
            "\r\n   read:    %ld"
            "\r\n   flags:   0x%X"
            "\r\n", sockRef, descP->sock, recvRef, (long) read, flags) );

    (void) flags;

    /* This function hansles splitting the binaries */
    encode_msg(opEnv, descP, read,
               &opDataP->msg,
               opDataP->data,
               &opDataP->ctrl,
               &eMsg);

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    res = esock_make_ok2(opEnv, eMsg);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvmsg_partial(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
}



/* *** esaio_completion_recvmsg_fail ***
 * Unknown operation failure.
 */
static
void esaio_completion_recvmsg_fail(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              error,
                                   BOOLEAN_T        inform)
{
    esaio_completion_fail(env, descP, "recvmsg", error, inform);
}



/* *** esaio_completion_get_ovl_result_fail ***
 * This function is called when the function 'WSAGetOverlappedResult' fails.
 * It generates a result (returns) in the form of:
 *
 *         {error, {get_overlapped_result, atom()}}
 */
static
ERL_NIF_TERM esaio_completion_get_ovl_result_fail(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  int              error)
{
    ERL_NIF_TERM eerrno = ENO2T(env, error);
    ERL_NIF_TERM reason = MKT2(env, esock_atom_get_overlapped_result, eerrno);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_get_ovl_result_fail{%d} -> entry with"
            "\r\n   Errno: %d (%T)"
            "\r\n", descP->sock, error, eerrno) );

    MLOCK(ctrl.cntMtx);

    esock_cnt_inc(&ctrl.genErrs, 1);

    MUNLOCK(ctrl.cntMtx);

    return esock_make_error(env, reason);
}




/* === Unknown command 'stuff' === */

/* *** esaio_completion_unknown ***
 * What shall we actually do here?
 * Increment counters (bytes, number of unknown packages)?
 * Send a messge with this info to "someone"?
 * Write a (error) message to stdout/stderr?
 */
static
BOOLEAN_T esaio_completion_unknown(ESAIOThreadData* dataP,
                                   ESockDescriptor* descP,
                                   OVERLAPPED*      ovl,
                                   DWORD            numBytes,
                                   int              error)
{
    (void) dataP;
    (void) descP;
    (void) ovl;
    (void) numBytes;
    (void) error;

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.unknownCmds, 1);

    MUNLOCK(ctrl.cntMtx);

    return FALSE;
}



/* *** esaio_completion_fail ***
 * Unknown operation failure (not 'unknown operation' failure,
 * but an unknown 'operation failure').
 */
static
void esaio_completion_fail(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           const char*      opStr,
                           int              error,
                           BOOLEAN_T        inform)
{
    if (inform)
        esock_warning_msg("[WIN-ESAIO] Unknown (%s) operation failure: "
                          "\r\n   Descriptor: %d"
                          "\r\n   Error:      %T"
                          "\r\n",
                          opStr, descP->sock, ENO2T(env, error));

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.genErrs, 1);

    MUNLOCK(ctrl.cntMtx);

}



static
void esaio_completion_inc(ESAIOThreadData* dataP)
{
    if (dataP->cnt == ESAIO_THREAD_CNT_MAX) {
        dataP->cnt = 0;
    } else {
        dataP->cnt++;    
    }
}



/* ====================================================================
 *
 * NIF (I/O backend) Resource callback functions: dtor, stop and down
 *
 * ====================================================================
 */

extern
void esaio_dtor(ErlNifEnv*       env,
                ESockDescriptor* descP)
{
    ERL_NIF_TERM sockRef;

    SGDBG( ("WIN-ESAIO", "esaio_dtor -> entry\r\n") );

    /*
    ESOCK_PRINTF("esaio_dtor -> entry when"
                 "\r\n   is selected:     %s"
                 "\r\n   read state:      0x%X"
                 "\r\n   is read-closed:  %s"
                 "\r\n   write state:     0x%X"
                 "\r\n   is write-closed: %s"
                 "\r\n   sock:            %d"
                 "\r\n",
                 B2S(IS_SELECTED(descP)),
                 descP->readState,
                 B2S(IS_CLOSED(descP->readState)),
                 descP->writeState,
                 B2S(IS_CLOSED(descP->writeState)),
                 descP->sock);
    */

    if (IS_SELECTED(descP)) {
        /* We have used the socket in the "I/O Completion Port" machinery,
         * so we must have closed it properly to get here
         */
        if (! IS_CLOSED(descP->readState) )
            esock_warning_msg("Socket Read State not CLOSED (0x%X) "
                              "at dtor\r\n", descP->readState);
        
        if (! IS_CLOSED(descP->writeState) )
            esock_warning_msg("Socket Write State not CLOSED (0x%X) "
                              "at dtor\r\n", descP->writeState);
        
        if ( descP->sock != INVALID_SOCKET )
            esock_warning_msg("Socket %d still valid\r\n", descP->sock);

        ESOCK_ASSERT( IS_CLOSED(descP->readState) );
        ESOCK_ASSERT( IS_CLOSED(descP->writeState) );
        ESOCK_ASSERT( descP->sock == INVALID_SOCKET );

    } else {
        /* The socket is only opened, should be safe to close nonblocking */
        (void) sock_close(descP->sock);
        descP->sock = INVALID_SOCKET;
    }

    SGDBG( ("WIN-ESAIO", "esaio_dtor -> set state and pattern\r\n") );
    descP->readState  |= (ESOCK_STATE_DTOR | ESOCK_STATE_CLOSED);
    descP->writeState |= (ESOCK_STATE_DTOR | ESOCK_STATE_CLOSED);
    descP->pattern     = (ESOCK_DESC_PATTERN_DTOR | ESOCK_STATE_CLOSED);

    SGDBG( ("WIN-ESAIO",
            "esaio_dtor -> try free readers request queue\r\n") );
    esock_free_request_queue(&descP->readersQ);

    SGDBG( ("WIN-ESAIO",
            "esaio_dtor -> try free writers request queue\r\n") );
    esock_free_request_queue(&descP->writersQ);

    SGDBG( ("WIN-ESAIO",
            "esaio_dtor -> try free acceptors request queue\r\n") );
    esock_free_request_queue(&descP->acceptorsQ);

    esock_free_env("esaio_dtor close env", descP->closeEnv);
    descP->closeEnv = NULL;

    esock_free_env("esaio_dtor meta env", descP->meta.env);
    descP->meta.env = NULL;

    SGDBG( ("WIN-ESAIO", "esaio_dtor -> done\r\n") );
}



extern
void esaio_stop(ErlNifEnv*       env,
                ESockDescriptor* descP)
{

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_stop(%d) -> entry\r\n", descP->sock) );

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Inform waiting Closer, or close socket
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if ( !IS_PID_UNDEF(&descP->closerPid) &&
        (descP->closeEnv != NULL) ) {

        /* We will only send this message if the user was made to 
         * wait (async close). In that case we have en env!
         * We have a waiting closer process after nif_close()
         * - send message to trigger nif_finalize_close()
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_stop(%d) -> send close msg to %T\r\n",
                descP->sock, MKPID(env, &descP->closerPid)) );

        esock_send_close_msg(env, descP, &descP->closerPid);
        /* Message send frees closeEnv */
        descP->closeEnv = NULL;
        descP->closeRef = esock_atom_undefined;

    } else {
        int err;

        /* We do not have a closer process
         * - have to do an unclean (non blocking) close */

        err = esock_close_socket(env, descP, FALSE);

        switch (err) {
        case NO_ERROR:
            break;
        case WSAENOTSOCK:
            if (descP->sock != INVALID_SOCKET)
                esock_warning_msg("[WIN-ESAIO] Attempt to close an "
                                  "already closed socket"
                                  "\r\n(without a closer process): "
                                  "\r\n   Controlling Process: %T"
                                  "\r\n   socket fd:           %d"
                                  "\r\n",
                                  descP->ctrlPid, descP->sock);
            break;

        default:
            esock_warning_msg("[WIN-ESAIO] Failed closing socket without "
                              "closer process: "
                              "\r\n   Controlling Process: %T"
                              "\r\n   socket fd:           %d"
                              "\r\n   Errno:               %T"
                              "\r\n",
                              descP->ctrlPid, descP->sock, ENO2T(env, err));
            break;
        }

    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_stop(%d) -> done\r\n", descP->sock) );

}




/* A 'down' has occured.
 * Check the possible processes we monitor in turn:
 * closer, controlling process (owner), connector, reader, acceptor and writer.
 *
 */
extern
void esaio_down(ErlNifEnv*           env,
                ESockDescriptor*     descP,
                const ErlNifPid*     pidP,
                const ErlNifMonitor* monP)
{
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_down {%d} -> entry with:"
            "\r\n   Pid: %T"
            "\r\n   Mon: %T"
            "\r\n", descP->sock, MKPID(env, pidP), MON2T(env, monP)) );

    if (COMPARE_PIDS(&descP->closerPid, pidP) == 0) {

        /* The closer process went down
         * - it will not call nif_finalize_close
         */

        enif_set_pid_undefined(&descP->closerPid);

        if (MON_EQ(&descP->closerMon, monP)) {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_down {%d} -> closer process exit\r\n",
                    descP->sock) );

            MON_INIT(&descP->closerMon);

        } else {
            // The owner is the closer so we used its monitor

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_down {%d} -> closer controlling process exit\r\n",
                    descP->sock) );

            ESOCK_ASSERT( MON_EQ(&descP->ctrlMon, monP) );
            MON_INIT(&descP->ctrlMon);
            enif_set_pid_undefined(&descP->ctrlPid);

        }

        /* Since the closer went down there was one,
         * hence esock_close() must have run or scheduled esock_stop(),
         * or the socket has never been "selected" upon.
         */

        if (descP->closeEnv == NULL) {
            int err;

            /* Since there is no closeEnv,
             * esock_close() did not schedule esock_stop()
             * and is about to call esock_finalize_close() but died,
             * or esock_stop() has run, sent close_msg to the closer
             * and cleared ->closeEnv but the closer died
             * - we have to do an unclean (non blocking) socket close here
             */

            err = esock_close_socket(env, descP, FALSE);
            if (err != 0)
                esock_warning_msg("[WIN-ESAIO] "
                                  "Failed closing socket for terminating "
                                  "closer process: "
                                  "\r\n   Closer Process: %T"
                                  "\r\n   Descriptor:     %d"
                                  "\r\n   Errno:          %d (%T)"
                                  "\r\n",
                                  MKPID(env, pidP), descP->sock,
                                  err, ENO2T(env, err));
        } else {
            /* Since there is a closeEnv esock_stop() has not run yet
             * - when it finds that there is no closer process
             *   it will close the socket and ignore the close_msg
             *
             * The 'stop' callback function will never be triggered on
             * Windows...It may be explicitly called...
             */
            esock_clear_env("esaio_down - close-env", descP->closeEnv);
            esock_free_env("esaio_down - close-env", descP->closeEnv);
            descP->closeEnv = NULL;
            descP->closeRef = esock_atom_undefined;
        }

    } else if (MON_EQ(&descP->ctrlMon, monP)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_down {%d} -> controller process exit\r\n",
                descP->sock) );

        MON_INIT(&descP->ctrlMon);
        /* The owner went down */
        enif_set_pid_undefined(&descP->ctrlPid);

        if (IS_OPEN(descP->readState)) {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_down {%d} -> OPEN => initiate close\r\n",
                    descP->sock) );

            esaio_down_ctrl(env, descP, pidP);

            descP->readState  |= ESOCK_STATE_CLOSING;
            descP->writeState |= ESOCK_STATE_CLOSING;

        } else {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_down {%d} -> already closed or closing\r\n",
                    descP->sock) );

        }

    } else if (descP->connectorP != NULL &&
               MON_EQ(&descP->connector.mon, monP)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_down {%d} -> connector process exit\r\n",
                descP->sock) );

        MON_INIT(&descP->connector.mon);

        /* connectorP is only set during connection.
         * Forget all about the ongoing connection.
         * We might end up connected, but the process that initiated
         * the connection has died and will never know
         */

        esock_requestor_release("esaio_down->connector",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;

    } else {
        ERL_NIF_TERM sockRef = enif_make_resource(env, descP);

        /* check all operation queue(s): acceptor, writer and reader.
         *
         * Is it really any point in doing this if the socket is closed?
         *
         */

        if (IS_CLOSED(descP->readState)) {
            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_down(%T) {%d} -> stray down: %T\r\n",
                    sockRef, descP->sock, pidP) );
        } else {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_down(%T) {%d} -> "
                    "other process - check readers, writers and acceptors\r\n",
                    sockRef, descP->sock) );

            if (descP->readersQ.first != NULL)
                esaio_down_reader(env, descP, sockRef, pidP, monP);
            if (descP->acceptorsQ.first != NULL)
                esaio_down_acceptor(env, descP, sockRef, pidP, monP);
            if (descP->writersQ.first != NULL)
                esaio_down_writer(env, descP, sockRef, pidP, monP);
        }
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_down {%d} -> done\r\n", descP->sock) );

}



/* *** esaio_down_ctrl ***
 *
 * Stop after a downed controller (controlling process = owner process)
 *
 * This is 'extern' because its currently called from prim_socket_nif
 * (esock_setopt_otp_ctrl_proc).
 */
extern
void esaio_down_ctrl(ErlNifEnv*           env,
                     ESockDescriptor*     descP,
                     const ErlNifPid*     pidP)
{
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_down_ctrl {%d} -> entry with"
            "\r\n   Pid: %T"
            "\r\n", descP->sock, MKPID(env, pidP)) );

    if (do_stop(env, descP)) {
        /* esock_stop() is scheduled
         * - it has to close the socket
         */
        SSDBG( descP,
               ("WIN-ESAIO", "esaio_down_ctrl {%d} -> stop was scheduled\r\n",
                descP->sock) );
    } else {
        int err;

        /* Socket has no *active* requests in the I/O Completion Ports machinery
         * so esock_stop() will not be called
         * - we have to do an unclean (non blocking) socket close here
         */

        err = esock_close_socket(env, descP, FALSE);
        if (err != 0)
            esock_warning_msg("[WIN-ESAIO] "
                              "Failed closing socket for terminating "
                              "owner process: "
                              "\r\n   Owner Process:  %T"
                              "\r\n   Descriptor:     %d"
                              "\r\n   Errno:          %d (%T)"
                              "\r\n",
                              MKPID(env, pidP), descP->sock,
                              err, ENO2T(env, err));
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_down_ctrl {%d} -> done\r\n", descP->sock) );

}



/* *** esaio_down_acceptor ***
 *
 * Check and then handle a downed acceptor process.
 *
 */
static
void esaio_down_acceptor(ErlNifEnv*           env,
                         ESockDescriptor*     descP,
                         ERL_NIF_TERM         sockRef,
                         const ErlNifPid*     pidP,
                         const ErlNifMonitor* monP)
{
        
    /* Maybe unqueue one of the waiting acceptors */
        
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_down_acceptor(%T) {%d} -> "
            "maybe unqueue a waiting acceptor\r\n",
            sockRef, descP->sock) );
        
    esock_acceptor_unqueue(env, descP, NULL, pidP);

}


/* *** esaio_down_writer ***
 *
 * Check and then handle a downed writer process.
 *
 */

static
void esaio_down_writer(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       ERL_NIF_TERM         sockRef,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP)
{
        
    /* Maybe unqueue one of the waiting writer(s) */
        
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_down_writer(%T) {%d} -> maybe unqueue a waiting writer\r\n",
            sockRef, descP->sock) );
        
    esock_writer_unqueue(env, descP, NULL, pidP);

}


/* *** esaio_down_reader ***
 *
 * Check and then handle a downed reader process.
 *
 */

static
void esaio_down_reader(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       ERL_NIF_TERM         sockRef,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP)
{
    /* Maybe unqueue one of the waiting reader(s) */
        
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_down_reader(%T) {%d} -> maybe unqueue a waiting reader\r\n",
            sockRef, descP->sock) );

    esock_reader_unqueue(env, descP, NULL, pidP);

}


/* ==================================================================== *
 *                                                                      *
 *                     Send Utility functions                           *
 *                                                                      *
 * ==================================================================== *
 */

/* *** send_check_result ***
 *
 * Check the result of a socket send (WSASend, WSASendTo and WSASendMsg) call.
 *
 */
static
ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ESAIOOperation*  opP,
                               ErlNifPid        caller,
                               int              send_result,
                               ssize_t          dataSize,
                               BOOLEAN_T        dataInTail,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     sendRef,
                               BOOLEAN_T*       cleanup)
{
    ERL_NIF_TERM res;
    BOOLEAN_T    send_error;
    int          err;

    if (send_result == 0) {

        /* Send success already!
         * So, no need to store the data (request, in the "queue").
         * Note that the completion threads will use the
         * precense or absence of this request 'record' to inform
         * its actions.
         */

        *cleanup = FALSE;

        res = send_check_ok(env, descP, dataSize, sockRef);

    } else {

        /* send returned error, check which */

        int save_errno = sock_errno();

        /* There are basically two kinds of errors: 
         * 1) Pending:
         *    An overlapped operation was successfully initiated.
         *    Completion will be "indicated" at a later time.
         * 2) An actual error
         */

        if (save_errno == WSA_IO_PENDING) {

            /* We need to store the data in the queue! */

            *cleanup = FALSE;

            res = send_check_pending(env, descP, opP, caller, sockRef, sendRef);

        } else {

            *cleanup = TRUE;

            res = send_check_fail(env, descP, save_errno, sockRef);

        }
    }

    SSDBG( descP,
           ("WIN-ESAIO",
            "send_check_result(%T) {%d} -> done:"
            "\r\n   res: %T"
            "\r\n", sockRef, descP->sock, res) );

    return res;
}



/* *** send_check_ok ***
 *
 * Processing done upon successful send.
 */
static
ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           DWORD            written,
                           ERL_NIF_TERM     sockRef)
{
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_pkg, &descP->writePkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_byte, &descP->writeByteCnt, written);

    /* We can *never* have a partial successs:
     * Either the entire buffer is sent, or the op is scheduled or we fail.
     * But since we have a field (writePkgMaxCnt) in the descriptor
     * we might as well use it.
     */

    descP->writePkgMaxCnt = written;
    if (descP->writePkgMaxCnt > descP->writePkgMax)
        descP->writePkgMax = descP->writePkgMaxCnt;
    descP->writePkgMaxCnt = 0;

    SSDBG( descP,
           ("WIN-ESAIO", "send_check_ok(%T) {%d} -> %ld written - done\r\n",
            sockRef, descP->sock, written) );

    return esock_atom_ok;
}


/* *** send_check_pending ***
 *
 * The send operation was scheduled, that is, its now in the handls
 * of the I/O Completion Port framework.
 */
static
ERL_NIF_TERM send_check_pending(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ESAIOOperation*  opP,
                                ErlNifPid        caller,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     sendRef)
{
    SSDBG( descP,
           ("WIN-ESAIO",
            "send_check_pending(%T, %d) -> entry with"
            "\r\n   sendRef: %T"
            "\r\n", sockRef, descP->sock, sendRef) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_waits, &descP->writeWaits, 1);

    descP->writeState |= ESOCK_STATE_SELECTED;

    esock_writer_push(env, descP, caller, sendRef, opP);
            
    return esock_atom_completion;
    
}



/* *** send_check_fail ***
 *
 * Processing done upon failed send.
 * An actual failure.
 */
static
ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              saveErrno,
                             ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_fails, &descP->writeFails, 1);

    reason = ENO2T(env, saveErrno);

    SSDBG( descP,
           ("WIN-ESAIO",
            "send_check_fail(%T, %d) -> error: "
            "\r\n   %d (%T)\r\n",
            sockRef, descP->sock, saveErrno, reason) );

    return esock_make_error(env, reason);
}



/* ==================================================================== *
 *                                                                      *
 *                          Utility functions                           *
 *                                                                      *
 * ==================================================================== *
 */

/* *** get_send_ovl_result ***
 *
 * Used for all send 'overlapped' operations; send, sendto and sendmsg.
 */
static
BOOL get_send_ovl_result(SOCKET      sock,
                         OVERLAPPED* ovl,
                         DWORD*      written)
{
    DWORD flags  = 0;
    BOOL  result = get_ovl_result(sock, ovl, written, &flags);

    (void) flags;

    return result;
}


/* *** get_recv_ovl_result ***
 *
 * Used for recv *and* recvfrom 'overlapped' operations.
 */
static
BOOL get_recv_ovl_result(SOCKET      sock,
                         OVERLAPPED* ovl,
                         DWORD*      read,
                         DWORD*      flags)
{
    return get_ovl_result(sock, ovl, read, flags);
}


/* *** get_recvmsg_ovl_result ***
 *
 * Used for all recvmsg 'overlapped' operations.
 */
static
BOOL get_recvmsg_ovl_result(SOCKET      sock,
                            OVERLAPPED* ovl,
                            DWORD*      read)
{
    DWORD flags  = 0;
    BOOL  result = get_ovl_result(sock, ovl, read, &flags);

    (void) flags;

    return result;
}


/* *** get_ovl_result ***
 *
 * Simple wrapper function for WSAGetOverlappedResult.
 */
static
BOOL get_ovl_result(SOCKET      sock,
                    OVERLAPPED* ovl,
                    DWORD*      transfer,
                    DWORD*      flags)
{
    return WSAGetOverlappedResult(sock, ovl, transfer, FALSE, flags);
}



/* *** esaio_add_socket ***
 *
 * Add socket to I/O completion port.
 */
static
int esaio_add_socket(ESockDescriptor* descP)
{
    int    res;
    HANDLE tmp = CreateIoCompletionPort((HANDLE) descP->sock, ctrl.cport,
                                        (ULONG_PTR) descP, 0);

    if (tmp != NULL) {
        res = ESAIO_OK;
    } else {
        res = sock_errno();
    }

    return res;
}


static
void esaio_send_completion_msg(ErlNifEnv*       sendEnv,
                               ESockDescriptor* descP,
                               ErlNifPid*       pid,
                               ErlNifEnv*       msgEnv,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     completionInfo)
{
    ERL_NIF_TERM msg = mk_completion_msg(msgEnv, sockRef, completionInfo);

    /* This can only fail if:
     *   - The recipient is dead.
     *     Our monitor should clear this up ... eventually.
     *     Possible race?
     *   - We (the sender) are "dead" (which we are clearly not)
     */
    if (! esock_send_msg(sendEnv, pid, msg, NULL)) {

        /*
        ESOCK_DBG_PRINTF( TRUE, ("IN-ESAIO",
                                 "esaio_send_completion_msg(%T) {%d} failed ->"
                                 "\r\n   pid: %T"
                                 "\r\n",
                                 sockRef, descP->sock, MKPID(sendEnv, pid)) );
        */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_send_completion_msg(%T) {%d} failed ->"
                "\r\n   pid: %T"
                "\r\n",
                sockRef, descP->sock, MKPID(sendEnv, pid)) );
    }
}


/* *** mk_completion_msg ***
 *
 * Construct a completion (socket) message. It has the form: 
 *
 *         {'$socket', Socket, completion, CompletionInfo}
 *
 */

static
ERL_NIF_TERM mk_completion_msg(ErlNifEnv*   env,
                               ERL_NIF_TERM sockRef,
                               ERL_NIF_TERM info)
{
    return esock_mk_socket_msg(env, sockRef,
                               esock_atom_completion, info);
}


#endif
