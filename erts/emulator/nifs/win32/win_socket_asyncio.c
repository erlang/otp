/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023-2023. All Rights Reserved.
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
#define ESAIO_ERR_THREAD_OPTS_CREATE 0x0011
#define ESAIO_ERR_THREAD_CREATE      0x0012

#define ERRNO_BLOCK                  WSAEWOULDBLOCK


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
// #define sock_listen(s, b)               listen((s), (b))
// #define sock_name(s, addr, len)        getsockname((s), (addr), (len))
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_open_O(domain, type, proto) \
    WSASocket((domain), (type), (proto), NULL, 0, WSA_FLAG_OVERLAPPED)
#define sock_recv_O(s,buf,flag,ol)                      \
    WSARecv((s), (buf), 1, NULL, (flag), (ol), NULL)
#define sock_recvfrom_O(s,buf,flag,fa,fal,ol)                           \
    WSARecvFrom((s), (buf), 1, NULL, (flag), (fa), (fal), (ol), NULL)
#define sock_send_O(s,buf,flag,o)                       \
    WSASend((s), (buf), 1, NULL, (flag), (o), NULL)
/* #define sock_sendmsg_O(s,buf,flag,ol)                   \ */
/*     WSASendMsg((s), (buf), (flag), NULL, (ol), NULL) */
#define sock_sendmsg_O(s,buf,flag,ol)                   \
    ctrl.sendmsg((s), (buf), (flag), NULL, (ol), NULL)
#define sock_sendto_O(s,buf,flag,ta,tal,o)              \
    WSASendTo((s), (buf), 1, NULL, (flag), (ta), (tal), (o), NULL)
#define sock_setopt(s,l,o,v,ln)        setsockopt((s),(l),(o),(v),(ln))


#define ESAIO_UPDATE_ACCEPT_CONTEXT(AS, LS)                 \
    sock_setopt( (AS), SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, \
                 (char*) &(LS), sizeof( (LS) ))
#define ESAIO_UPDATE_CONNECT_CONTEXT(S)                                 \
    sock_setopt((S), SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT, NULL, 0)


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

    SOCKET          dummy; // Used for extracting AcceptEx and ConnectEx

    LPFN_ACCEPTEX   accept;
    LPFN_CONNECTEX  connect;
    LPFN_WSASENDMSG sendmsg;

    /* Thread pool stuff.
     * The size of the pool is configurable. */
    DWORD           numThreads;
    ESAIOThread*    threads;

    /* Misc stuff */
    BOOLEAN_T       dbg;
    BOOLEAN_T       sockDbg;

    /* Counter stuff */
    ErlNifMutex*    cntMtx;
    ESockCounter    unexpectedAccepts;
    ESockCounter    unexpectedWrites;
    ESockCounter    unexpectedReads;
    ESockCounter    genErrs;
    ESockCounter    unknownCmds;

} ESAIOControl;


typedef struct __ESAIOOpDataAccept {
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
    /* When ConnectEx has completed successfully, 
     * the socket is usable.
     * *But*, in order for the functions sockname and peername to work,
     * the SO_UPDATE_CONNECT_CONTEXT option must be set on the socket.
     */
    ERL_NIF_TERM sockRef; /* The socket */
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
    /* WSASendMsg */
    WSAMSG       msg;
    ErlNifIOVec* iovec;
    char*        ctrlBuf;
    ESockAddress addr;
    ERL_NIF_TERM sockRef; /* The socket */
    ERL_NIF_TERM sendRef; /* The (unique) reference (ID)
                           * of the send request */
} ESAIOOpDataSendMsg;

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
    /* Commands for receiving */
#define ESAIO_OP_RECV         0x0031  // WSARecv
#define ESAIO_OP_RECVFROM     0x0032  // WSARecvFrom

    unsigned int          tag;    /* The 'tag' of the operation */

    ErlNifPid             caller; /* Almost every request (not connect)
                                   * operations require a caller */
    ErlNifEnv*            env;    /* Almost every request
                                   * needs an environment */

    /* Generic "data" field.
     * This is different for each 'operation'!
     * Also, not all opererations have this!
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

        /* +++ recv +++ */
        ESAIOOpDataRecv recv;

        /* +++ recvfrom +++ */
        ESAIOOpDataRecvFrom recvfrom;

    } data;

} ESAIOOperation;



/* =================================================================== *
 *                                                                     *
 *                        Function Forwards                            *
 *                                                                     *
 * =================================================================== */

static ERL_NIF_TERM esaio_accept_accepted(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     sockRef,
                                          SOCKET           accSock,
                                          ErlNifPid        pid);

static ERL_NIF_TERM esaio_connect_stream(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     connRef,
                                         ESockAddress*    addrP,
                                         SOCKLEN_T        addrLen);
static ERL_NIF_TERM esaio_connect_dgram(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     connRef,
                                        ESockAddress*    addrP,
                                        SOCKLEN_T        addrLen);

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

static ERL_NIF_TERM recv_check_ok(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ESAIOOperation*  opP,
                                  ERL_NIF_TERM     sockRef);

static ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
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
                                    ErlNifBinary*    buf,
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
                                      ERL_NIF_TERM     sockRef);

static void* esaio_completion_main(void* threadDataP);
static BOOLEAN_T esaio_completion_terminate(ESAIOThreadData* dataP,
                                            ESAIOOperation*  opP);
static BOOLEAN_T esaio_completion_unknown(ESAIOThreadData* dataP,
                                          ESockDescriptor* descP,
                                          ESAIOOperation*  opP,
                                          DWORD            numBytes,
                                          int              error);
static BOOLEAN_T esaio_completion_connect(ESAIOThreadData* dataP,
                                          ESockDescriptor* descP,
                                          ESAIOOperation*  opP,
                                          int              error);
static BOOLEAN_T esaio_completion_accept(ESAIOThreadData* dataP,
                                         ESockDescriptor* descP,
                                         ESAIOOperation*  opP,
                                         int              error);
static void esaio_completion_accept_completed(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ESAIOOperation*  opP,
                                              ESockRequestor*  reqP,
                                              int              error);
static void esaio_completion_accept_not_active(ESAIOOperation*  opP,
                                               int              error);
static void esaio_completion_accept_closed(ESockDescriptor* descP,
                                           ESAIOOperation*  opP,
                                           int              error);
static BOOLEAN_T esaio_completion_send(ESAIOThreadData* dataP,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       int              error);
static void esaio_completion_send_completed(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            OVERLAPPED*      ovl,
                                            ErlNifEnv*       opEnv,
                                            ErlNifPid*       sender,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     sendRef,
                                            DWORD            toWrite,
                                            ESockRequestor*  reqP,
                                            int              error);
static ERL_NIF_TERM esaio_completion_send_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     sockRef,
                                               DWORD            written);
static ERL_NIF_TERM esaio_completion_send_partial(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  DWORD            written);
static void esaio_completion_send_not_active(int error);
static void esaio_completion_send_closed(ESockDescriptor* descP,
                                         int              error);
static BOOLEAN_T esaio_completion_sendto(ESAIOThreadData* dataP,
                                         ESockDescriptor* descP,
                                         ESAIOOperation*  opP,
                                         int              error);
static BOOLEAN_T esaio_completion_sendmsg(ESAIOThreadData* dataP,
                                          ESockDescriptor* descP,
                                          ESAIOOperation*  opP,
                                          int              error);
static BOOLEAN_T esaio_completion_recv(ESAIOThreadData* dataP,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       int              error);
static void esaio_completion_recv_completed(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ESAIOOperation*  opP,
                                            ESockRequestor*  reqP,
                                            int              error);
static ERL_NIF_TERM esaio_completion_recv_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ESAIOOperation*  opP,
                                               DWORD            flags);
static ERL_NIF_TERM esaio_completion_recv_partial(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ESAIOOperation*  opP,
                                                  ESockRequestor*  reqP,
                                                  DWORD            read,
                                                  DWORD            flags);
static ERL_NIF_TERM esaio_completion_recv_partial_done(ErlNifEnv*       env,
                                                       ESockDescriptor* descP,
                                                       ESAIOOperation*  opP,
                                                       ssize_t          read,
                                                       DWORD            flags);
static ERL_NIF_TERM esaio_completion_recv_partial_part(ErlNifEnv*       env,
                                                       ESockDescriptor* descP,
                                                       ESAIOOperation*  opP,
                                                       ssize_t          read,
                                                       DWORD            flags);
static void esaio_completion_recv_not_active(ESockDescriptor* descP,
                                             int              error);
static void esaio_completion_recv_closed(ESockDescriptor* descP,
                                         int              error);
static void esaio_completion_recvfrom_completed(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ESAIOOperation*  opP,
                                                ESockRequestor*  reqP,
                                                int              error);
static ERL_NIF_TERM esaio_completion_recvfrom_done(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ESAIOOperation*  opP,
                                                   DWORD            flags);
static ERL_NIF_TERM esaio_completion_recvfrom_partial(ErlNifEnv*       env,
                                                      ESockDescriptor* descP,
                                                      ESAIOOperation*  opP,
                                                      ESockRequestor*  reqP,
                                                      DWORD            read,
                                                      DWORD            flags);
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

    /* Enabling this results in a core dump when calling socket:accept
     * multiple times (the second call fails in env alloc).!
     */
    // ctrl.dbg               = TRUE;
    ctrl.dbg               = dataP->dbg;
    ctrl.sockDbg           = dataP->sockDbg;

    SGDBG( ("WIN-ESAIO", "esaio_init -> entry\r\n") );

    ctrl.cntMtx            = MCREATE("win-esaio.cnt");
    ctrl.unexpectedAccepts = 0;
    ctrl.unexpectedWrites  = 0;
    ctrl.unexpectedReads   = 0;
    ctrl.genErrs           = 0;
    ctrl.unknownCmds       = 0;

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


    /* Create the "dummy" socket and then
     * extract the AcceptEx and ConnectEx functions.
     */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try create 'dummy' socket\r\n") );
    ctrl.dummy = sock_open(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (ctrl.dummy == INVALID_SOCKET) {
        save_errno = sock_errno();

        esock_error_msg("Failed create 'dummy' socket: "
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
    ires = WSAIoctl(ctrl.dummy, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidAcceptEx, sizeof (guidAcceptEx), 
                    &ctrl.accept, sizeof (ctrl.accept), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'accept' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.dummy);
        ctrl.dummy  = INVALID_SOCKET;
        ctrl.accept = NULL;

        WSACleanup();

        return ESAIO_ERR_IOCTL_ACCEPT_GET;
    }


    /* Basically the same as for AcceptEx above */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try extract 'connect' function\r\n") );
    ires = WSAIoctl(ctrl.dummy, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidConnectEx, sizeof (guidConnectEx), 
                    &ctrl.connect, sizeof (ctrl.connect), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'connect' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.dummy);
        ctrl.dummy  = INVALID_SOCKET;
        ctrl.accept = NULL;

        WSACleanup();
        return ESAIO_ERR_IOCTL_CONNECT_GET;
    }
    

    /* Basically the same as for AcceptEx above */
    SGDBG( ("WIN-ESAIO", "esaio_init -> try extract 'sendmsg' function\r\n") );
    ires = WSAIoctl(ctrl.dummy, SIO_GET_EXTENSION_FUNCTION_POINTER,
                    &guidSendMsg, sizeof (guidSendMsg), 
                    &ctrl.sendmsg, sizeof (ctrl.sendmsg), 
                    &dummy, NULL, NULL);
    if (ires == SOCKET_ERROR) {
        save_errno = sock_errno();

        esock_error_msg("Failed extracting 'sendmsg' function: %d"
                        "\r\n   %s (%d)"
                        "\r\n",
                        ires, erl_errno_id(save_errno), save_errno);

        (void) sock_close(ctrl.dummy);
        ctrl.dummy   = INVALID_SOCKET;
        ctrl.accept  = NULL;
        ctrl.connect = NULL;

        WSACleanup();
        return ESAIO_ERR_IOCTL_SENDMSG_GET;
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
            esock_error_msg("Failed create thread opts %d\r\n");

            ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_TOCREATE;

            for (j = 0; j < i; j++) {
                SGDBG( ("WIN-ESAIO",
                        "esaio_init -> destroy thread opts %d\r\n", j) );
                TODESTROY(ctrl.threads[j].optsP);
            }
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

            ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_TCREATE;

            for (j = 0; j <= i; j++) {
                SGDBG( ("WIN-ESAIO",
                        "esaio_init -> destroy thread opts %d\r\n", j) );
                TODESTROY(ctrl.threads[j].optsP);
            }
            return ESAIO_ERR_THREAD_CREATE;
        }

    }


    SGDBG( ("WIN-ESAIO", "esaio_init -> done\r\n") );

    return ESAIO_OK;
}



/* *******************************************************************
 * Finish, terminate, the ESock Async I/O backend.
 * This means principally to terminate (threads of) the thread pool.
 * Issue a "message" via PostQueuedCompletionStatus
 * instructing all (completion) threads to terminate.
 */
extern
void esaio_finish()
{
    int t, lastThread;

    SGDBG( ("WIN-ESAIO", "esaio_finish -> entry\r\n") );

    if (ctrl.dummy != INVALID_SOCKET) {
        SGDBG( ("WIN-ESAIO", "esaio_finish -> close 'dummy' socket\r\n") );
        (void) sock_close(ctrl.dummy);
        ctrl.dummy = INVALID_SOCKET;
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
         * the VM. So, is there actuall any pointy in "doing" something?
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

    /* This is overkill,
     * since this function, esaio_finish, is called when the VM is halt'ing...
     * ...but just to be a nice citizen...
     */
    SGDBG( ("WIN-ESAIO", "esaio_finish -> free the thread pool data\r\n") );
    FREE( ctrl.threads );

    SGDBG( ("WIN-ESAIO", "esaio_finish -> invalidate functions\r\n") );
    ctrl.accept  = NULL;
    ctrl.connect = NULL;
    
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
        numUnexpAccs, numUnexpWs, numUnexpRs,
        numGenErrs,
        numUnknownCmds;
    
    numThreads     = MKUI(env, ctrl.numThreads);

    MLOCK(ctrl.cntMtx);

    numUnexpAccs   = MKUI(env, ctrl.unexpectedAccepts);
    numUnexpWs     = MKUI(env, ctrl.unexpectedWrites);
    numUnexpRs     = MKUI(env, ctrl.unexpectedReads);
    numGenErrs     = MKUI(env, ctrl.genErrs);
    numUnknownCmds = MKUI(env, ctrl.unknownCmds);

    MUNLOCK(ctrl.cntMtx);

    {
        ERL_NIF_TERM cntKeys[] = {esock_atom_num_unexpected_accepts,
                                  esock_atom_num_unexpected_writes,
                                  esock_atom_num_unexpected_reads,
                                  esock_atom_num_general_errors,
                                  esock_atom_num_unknown_cmds};
        ERL_NIF_TERM cntVals[] = {numUnexpAccs, numUnexpWs, numUnexpRs,
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
           ("WIN-ESAIO", "esaio_connect(%T) -> verify open\r\n", sockRef) );
    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, esock_atom_closed);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_connect(%T) -> verify type: %s\r\n",
            sockRef, TYPE2STR(descP->type)) );
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
    BOOL            cret;
    // ErlNifPid       proxy;
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

    /* This is either a pid or the atom 'undefined' */
    /*
    if (IS_PID(env, eproxy)) {
        if (!GET_LPID(env, eproxy, &proxy)) {
        return enif_make_badarg(env);
        }
    }
    */
    
    SSDBG( descP,
           ("WIN-ESAIO", "esaio_connect_stream(%T) -> check if ongoing\r\n",
            sockRef) );
    if (descP->connectorP != NULL) {

        /* Connect already in progress, check if its us */

        if (COMPARE_PIDS(&self, &descP->connector.pid) != 0) {
            /* *Other* process has connect in progress */
            if (addrP != NULL) {
                return esock_make_error(env, esock_atom_already);
            } else {
                /* This is a bad call sequence
                 * - connect without an address is only allowed
                 *   for the connecting process
                 */
                return esock_raise_invalid(env, esock_atom_state);
            }
        } else {

            /* TRHIS IS NOT HOW IT WORKS ON WINDOWS!
             * This function should never be called *again*
             * The completion message contains the full and final answer.
             * No need to call again!
             */

            return esock_raise_invalid(env, esock_atom_state);
        }


        /* Finalize after received 'completion' message */

        /*
        esock_requestor_release("esaio_connect_stream finalize -> connected",
                                env, descP, &descP->connector);
        descP->connectorP  = NULL;
        descP->writeState &= ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
        descP->writeState |= ESOCK_STATE_CONNECTED;
        */

        /* Here we should send a 'completion' message indicating
         * success, that is 'ok'.
         */
        
        return esock_atom_ok;

    } else {

        DWORD sentDummy = 0;

        /* No connect already in progress */

        if (addrP == NULL)
            /* This is a bad call sequence
             * - connect without an address is only allowed when
             *   a connect is in progress,
             *   after getting the 'completion' message
             */
            return esock_raise_invalid(env, esock_atom_state);

        /* Initial connect call, with address */
        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_connect_stream(%T) -> allocate operation\r\n",
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
        opP->data.connect.sockRef = CP_TERM(opP->env, sockRef);

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

        cret = sock_connect_O(descP->sock,
                              addrP, addrLen,
                              &sentDummy, (OVERLAPPED*) opP);

        /* 
         * We need to keep using the requestor "queues"!
         * That is the "only" way to handle the mon itoring of
         * the requestor.
         * That is if the request (for instance a connect) is
         * is scheduled, WSA_IO_PENDING, then we need to store
         * the info about the requestor somewhere we can access it,
         * in case the requestor for example dies (and we need to
         * clean up).
         */

        if (cret) {

            /* Success already! */

            int err;

            SSDBG( descP,
                   ("WIN-ESAIO", "esaio_connect_stream {%d} -> connected\r\n",
                    descP->sock) );

            /* No need for this "stuff" anymore */
            esock_clear_env("esaio_connect_stream", opP->env);
            esock_free_env("esaio_connect_stream", opP->env);
            FREE( opP );

            /* We need to make sure peername and sockname works! */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_connect_stream {%d} -> update connect context\r\n",
                    descP->sock) );

            err = ESAIO_UPDATE_CONNECT_CONTEXT( descP->sock );

            if (err == 0) {

                descP->writeState &=
                    ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
                descP->writeState |= ESOCK_STATE_CONNECTED;
                eres = esock_atom_ok;

            } else {
                ERL_NIF_TERM tag, reason;

                save_errno = sock_errno();

                tag        = esock_atom_update_connect_context;
                reason     = MKA(env, erl_errno_id(save_errno));

                SSDBG( descP, ("WIN-ESAIO",
                               "esaio_connect_stream {%d} -> "
                               "connect context update failed: %T (%d)\r\n",
                               descP->sock, reason, save_errno) );

                sock_close(descP->sock);
                descP->writeState = ESOCK_STATE_CLOSED;

                WSACleanup();

                eres = esock_make_error_t2r(env, tag, reason);
            }

        } else {

            /* Connect returned error, check which */

            save_errno = sock_errno();

            if (save_errno == WSA_IO_PENDING) {
                SSDBG( descP, ("WIN-ESAIO",
                               "esaio_connect {%d} -> connect scheduled\r\n",
                               descP->sock) );
                // eres = esock_make_error(env, esock_atom_completion);
                eres = esock_atom_completion;

            } else {
                ERL_NIF_TERM ereason = MKA(env, erl_errno_id(save_errno));

                SSDBG( descP, ("WIN-ESAIO",
                               "esaio_connect {%d} -> "
                               "connect attempt failed: %T (%d)\r\n",
                               descP->sock, ereason, save_errno) );

                esock_clear_env("esaio_connect", opP->env);
                esock_free_env("esaio_connect", opP->env);
                FREE( opP );

                sock_close(descP->sock);
                descP->writeState = ESOCK_STATE_CLOSED;
                WSACleanup();

                eres = esock_make_error(env, ereason);
            }
        }
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_connect {%d} -> done with"
                   "\r\n   eres: %T"
                   "\r\n",
                   descP->sock, eres) );

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
    return enif_make_badarg(env);
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
    ERL_NIF_TERM    res, reason;
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
     * domain   - should be AF_INET | AF_INET6 (sould we make sure?)
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
        ERL_NIF_TERM tag        = esock_atom_create_accept_socket;

        reason = MKA(env, erl_errno_id(save_errno));

        esock_clear_env("esaio_accept - invalid accept socket", opP->env);
        esock_free_env("esaio_accept - invalid accept socket", opP->env);
        FREE( opP );
        WSACleanup();

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
    default:
        return esock_make_error_invalid(env, esock_atom_domain);
        break;
    }
    bufSz  = 2 * addrSz;

    opP->data.accept.buf = MALLOC( bufSz );
    ESOCK_ASSERT( opP->data.accept.buf != NULL);

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_accept {%d} -> try accept\r\n",
            descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_tries, &descP->accTries, 1);

    ares = sock_accept_O(descP->sock, accSock,
                         opP->data.accept.buf,
                         addrSz,
                         &recvBytes,
                         (OVERLAPPED*) opP);

    if (ares) {

        /* Success already!
         * So, no need to store the data (in the "queue").
         * But we do need to clean up the overlapped structure.
         * And then allocate and initiate the new descriptor.
         */

        esock_clear_env("esaio_accept - success", opP->env);
        esock_free_env("esaio_accept - success", opP->env);
        FREE( opP->data.accept.buf );
        FREE( opP );

        res = esaio_accept_accepted(env, descP, sockRef, accSock, caller);

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

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_accept {%d} -> pending\r\n", descP->sock) );

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_acc_waits, &descP->accWaits, 1);

            if (descP->acceptorsQ.first == NULL)
                descP->readState |= ESOCK_STATE_ACCEPTING;

            esock_acceptor_push(env, descP, caller, accRef, opP);
            
            res = esock_atom_completion;

        } else {

            reason = MKA(env, erl_errno_id(save_errno));

            SSDBG( descP, ("WIN-ESAIO",
                           "esaio_accept {%d} -> accept failed: %T( %d)\r\n",
                           descP->sock, reason, save_errno) );

            esock_clear_env("esaio_accept", opP->env);
            esock_free_env("esaio_accept", opP->env);
            FREE( opP->data.accept.buf );
            FREE( opP );

            sock_close(accSock);
            WSACleanup();

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_acc_fails, &descP->accFails, 1);

            res = esock_make_error(env, reason);
        }
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_accept {%d} -> done when"
                   "\r\n   res: %T"
                   "\r\n", descP->sock, res) );

    return res;
}



/* *** esaio_accept_accepted ***
 *
 * Generic function handling a successful accept.
 */
static
ERL_NIF_TERM esaio_accept_accepted(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     sockRef,
                                   SOCKET           accSock,
                                   ErlNifPid        pid)
{
    ESockDescriptor* accDescP;
    ERL_NIF_TERM     accRef;
    int              save_errno;

    /*
     * We got one
     */

    // Allocate the descriptor
    accDescP = esock_alloc_descriptor(accSock);
    
    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_accept_accepted -> add to completion port\r\n") );

    if (ESAIO_OK != (save_errno = esaio_add_socket(accDescP))) {
        // See esock_dtor for what needs done!
        ERL_NIF_TERM tag    = esock_atom_add_socket;
        ERL_NIF_TERM reason = MKA(env, erl_errno_id(save_errno));

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_fails, &descP->accFails, 1);

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_open_plain -> "
                "failed adding (accepted) socket to completion port: "
                "%T (%d)\r\n",
                reason, save_errno) );

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
    accDescP->rNum     = descP->rNum;    // Inherit buffer uses
    accDescP->rNumCnt  = 0;
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

    return esock_make_ok2(env, accRef);

}



/* ========================================================================
 * Do the actual send.
 * Do some initial writer checks, do the actual send and then
 * analyze the result.
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

    /* This is a size check,
     * to ensure we do not try to send something *to* large */
    toWrite = (DWORD) sndDataP->size;
    if ((size_t) toWrite != sndDataP->size)
        return esock_make_error_invalid(env, esock_atom_data_size);    

    /* Once the send function has been called, this memory
     * is "owned" by the system. That is, we cannot free it
     * (or do anything with it) until the operation has completed.
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
 * The following flöags are valid:
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

    /* We do not need this anymore.
     * If we succeeded "directly", the data is sent and we can free this now.
     * If the op was scheduled, the "system" copied the info in these buffers
     * before returning (from sendmsg), so we can free this now.
     * => We can free this now!
     */
    FREE( opP->data.sendmsg.msg.lpBuffers );

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
        esock_clear_env("esaio_sendto - cleanup", opP->env);
        esock_free_env("esaio_sendto - cleanup", opP->env);

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
                       "essio_sendmsg {%d} -> invalid tail\r\n",
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
                           "verify_sendmsg_iovec_size {%d} -> Overflow"
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

    return recv_check_result(env, descP, opP, caller, rres,
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
                               ESAIOOperation*  opP,
                               ErlNifPid        caller,
                               int              recv_result,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM eres;

    if (recv_result == 0) {

        /* +++ Success +++ */

        eres = recv_check_ok(env, descP, opP, sockRef);

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

            eres = recv_check_pending(env, descP, opP, caller,
                                      sockRef, recvRef);
        } else {

            eres = recv_check_fail(env, descP,
                                   opP, &opP->data.recv.buf,
                                   err, sockRef);

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
                           ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM data, result;
    DWORD        read = 0, flags = 0;

    SSDBG( descP,
           ("WIN-ESAIO",
            "recv_check_ok -> try get overlapped result\r\n") );

    if (get_recv_ovl_result(descP->sock, (OVERLAPPED*) opP, &read, &flags)) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "recv_check_ok -> overlapped result: "
                "\r\n   read:  %d"
                "\r\n   flags: 0x%X"
                "\r\n", read, flags) );

        (void) flags; // We should really do somthing with this...

        if (read == opP->data.recv.buf.size) {
            /* This transfers "ownership" of the *allocated* binary to an
             * erlang term (no need for an explicit free).
             */
            data = MKBIN(env, &opP->data.recv.buf);
        } else {
            /* This transfers "ownership" of the *allocated* binary to an
             * erlang term (no need for an explicit free).
             */
            data = MKBIN(env, &opP->data.recv.buf);
            data = MKSBIN(env, data, 0, read);
        }

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_pkg, &descP->readPkgCnt, 1);
        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_byte, &descP->readByteCnt, read);

        /* (maybe) Update max */
        if (read > descP->readPkgMax)
            descP->readPkgMax = read;

        esock_clear_env("recv_check_ok", opP->env);
        esock_free_env("recv_check_ok", opP->env);
        FREE( opP );

        result = esock_make_ok2(env, data);

    } else {
        int          save_errno = sock_errno();
        ERL_NIF_TERM eerrno = MKA(env, erl_errno_id(save_errno));
        ERL_NIF_TERM reason = MKT2(env, esock_atom_get_overlapped_result, eerrno);

        MLOCK(ctrl.cntMtx);

        esock_cnt_inc(&ctrl.genErrs, 1);

        MUNLOCK(ctrl.cntMtx);

        result = esock_make_error(env, reason);
    }

    SSDBG( descP,
           ("WIN-ESAIO", "recv_check_ok(%T) {%d} -> done with"
            "\r\n   result: %T"
            "\r\n",
            sockRef, descP->sock, result) );

    return result;
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
            "recv_check_pending {%d} -> pending\r\n", descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_waits, &descP->readWaits, 1);

    descP->readState |= ESOCK_STATE_SELECTED;

    esock_reader_push(env, descP, caller, recvRef, opP);
            
    return esock_atom_completion;
    
}



/* *** recv_check_fail* ***
 *
 * Processing done upon failed recv.
 * An actual failure.
 */
static
ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ESAIOOperation*  opP,
                             ErlNifBinary*    buf,
                             int              saveErrno,
                             ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
           ("WIN-ESAIO", "recv_check_fail(%T) {%d} -> error: %d (%T)\r\n",
            sockRef, descP->sock, saveErrno, reason) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_fails, &descP->readFails, 1);

    esock_clear_env("recv_check_fail", opP->env);
    esock_free_env("recv_check_fail", opP->env);
    FREE_BIN( buf );
    FREE( opP );

    return esock_make_error(env, reason);
}



/* ========================================================================
 * esaio_recvfrom - Read a "packet" from a socket
 *
 * The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
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

        eres = recvfrom_check_ok(env, descP, opP, sockRef);

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
            eres = recv_check_pending(env, descP, opP, caller,
                                      sockRef, recvRef);
        } else {

            eres = recv_check_fail(env, descP,
                                   opP, &opP->data.recvfrom.buf,
                                   err, sockRef);

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
                               ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM data, result;
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

        (void) flags; // We should really do somthing with this...

        esock_encode_sockaddr(env,
                              &opP->data.recvfrom.fromAddr,
                              opP->data.recvfrom.addrLen,
                              &eSockAddr);

        if (read == opP->data.recvfrom.buf.size) {
            /* This transfers "ownership" of the *allocated* binary to an
             * erlang term (no need for an explicit free).
             */
            data = MKBIN(env, &opP->data.recvfrom.buf);
        } else {
            /* This transfers "ownership" of the *allocated* binary to an
             * erlang term (no need for an explicit free).
             */
            data = MKBIN(env, &opP->data.recvfrom.buf);
            data = MKSBIN(env, data, 0, read);
        }

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_pkg, &descP->readPkgCnt, 1);
        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_byte, &descP->readByteCnt, read);

        /* (maybe) Update max */
        if (read > descP->readPkgMax)
            descP->readPkgMax = read;

        esock_clear_env("recv_check_ok", opP->env);
        esock_free_env("recv_check_ok", opP->env);
        FREE( opP );

        /*
         * This is:                 {ok, {Source, Data}}
         * But it should really be: {ok, {Source, Flags, Data}}
         */
        result = esock_make_ok2(env, MKT2(env, eSockAddr, data));

    } else {
        int          save_errno = sock_errno();
        ERL_NIF_TERM eerrno = MKA(env, erl_errno_id(save_errno));
        ERL_NIF_TERM reason = MKT2(env, esock_atom_get_overlapped_result, eerrno);

        MLOCK(ctrl.cntMtx);

        esock_cnt_inc(&ctrl.genErrs, 1);

        MUNLOCK(ctrl.cntMtx);

        result = esock_make_error(env, reason);
    }

    SSDBG( descP,
           ("WIN-ESAIO", "recvfrom_check_ok(%T) {%d} -> done with"
            "\r\n   result: %T"
            "\r\n",
            sockRef, descP->sock, result) );

    return result;
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

        if (! CancelIoEx((HANDLE) descP->sock, NULL) ) {
            int save_errno = sock_errno();

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "do_stop {%d} -> cancel I/O failed: %s (%d)\r\n",
                    descP->sock, erl_errno_id(save_errno), save_errno) );

            /* Only issue an error message for errors *other* than
             * 'not found' (since 'not found' means there is no active
             * requests = already completed => race).
             */

            if (save_errno != ERROR_NOT_FOUND)
                esock_error_msg("Failed cancel outstanding I/O operations:"
                                "\r\n   Socket: " SOCKET_FORMAT_STR
                                "\r\n   Reason: %s (%d)"
                                "\r\n",
                                descP->sock,
                                erl_errno_id(save_errno), save_errno);
            
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

    /* +++++++ Waiting Writers +++++++ */

    if (descP->writersQ.first != NULL) {

        /* Inform the waiting Writers - send it an abort message */

        SSDBG( descP,
               ("WIN-ESAIO",
                "do_stop {%d} -> handle waiting writer(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "writer",
                                   descP, sockRef, &descP->writersQ,
                                   esock_atom_closed);

    }

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

    /* +++++++ Current and waiting Readers +++++++ */

    if (descP->readersQ.first != NULL) {

        /* Inform the Readers - send it an abort message */

        SSDBG( descP,
               ("WIN-ESAIO",
                "do_stop {%d} -> handle waiting reader(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "writer",
                                   descP, sockRef, &descP->readersQ,
                                   esock_atom_closed);

    }

    /* +++++++ Waiting Acceptors +++++++
     *
     * Note that there should not be Readers and Acceptors
     * at the same time so the check for if the
     * Reader/Acceptor was deselected is only correct
     * under that assumption
     */

    if (descP->acceptorsQ.first != NULL) {

        /* Inform the waiting Acceptor - send it an abort message */

        SSDBG( descP,
               ("WIN-ESAIO",
                "do_stop {%d} -> handle waiting acceptors(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "acceptor",
                                   descP, sockRef, &descP->acceptorsQ,
                                   esock_atom_closed);

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
            if (olP == NULL) {

                /* First alt.
                 * What shall we do here? Quit? Try again?
                 */
                   
                SGDBG( ("WIN-ESAIO",
                        "esaio_completion_main -> [failure 1]\r\n") );

                dataP->state = ESAIO_THREAD_STATE_TERMINATING;
                dataP->error = ESAIO_THREAD_ERROR_GET;
                opP          = NULL;
                done         = TRUE;
                break;

            } else {

                /* Second alt.
                 * Dequeued a complete packet for a *failed* I/O operation.
                 */

                save_errno = sock_errno(); // Details

                SGDBG( ("WIN-ESAIO",
                        "esaio_completion_main -> [failure 2] "
                        "\r\n   %s (%d)"
                        "\r\n",
                        erl_errno_id(save_errno), save_errno) );

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
            done = esaio_completion_terminate(dataP, opP);
            break;

        case ESAIO_OP_CONNECT:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received connect cmd\r\n") );
            done = esaio_completion_connect(dataP, descP, opP, save_errno);
            break;

        case ESAIO_OP_ACCEPT:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received accept cmd\r\n") );
            done = esaio_completion_accept(dataP, descP, opP, save_errno);
            break;

        case ESAIO_OP_SEND:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received send cmd\r\n") );
            done = esaio_completion_send(dataP, descP, opP, save_errno);
            break;

        case ESAIO_OP_SENDTO:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received sendto cmd\r\n") );
            done = esaio_completion_sendto(dataP, descP, opP, save_errno);
            break;

        case ESAIO_OP_SENDMSG:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received sendmsg cmd\r\n") );
            done = esaio_completion_sendmsg(dataP, descP, opP, save_errno);
            break;

        case ESAIO_OP_RECV:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received recv cmd\r\n") );
            done = esaio_completion_recv(dataP, descP, opP, save_errno);
            break;

        case ESAIO_OP_RECVFROM:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received recvfrom cmd\r\n") );
            done = esaio_completion_recvfrom(dataP, descP, opP, save_errno);
            break;

        default:
            SGDBG( ("WIN-ESAIO",
                    "esaio_completion_main -> received unknown cmd: "
                    "\r\n   %d"
                    "\r\n",
                    opP->tag) );
            done = esaio_completion_unknown(dataP, descP, opP, numBytes,
                                            save_errno);
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
                                      ESAIOOperation*  opP)
{
    (void) opP;

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
BOOLEAN_T esaio_completion_connect(ESAIOThreadData* dataP,
                                   ESockDescriptor* descP,
                                   ESAIOOperation*  opP,
                                   int              error)
{
    ErlNifEnv*   env = dataP->env;
    ERL_NIF_TERM completionStatus, completionInfo;

    MLOCK(descP->writeMtx);

    /* Check if the socket is open.
     * If not we do not "need to" process this.
     * Either the connector has already closed the socket
     * or the connector has exited (and the socket has been
     * closed as a result of that.
     * Possibly the *bad user* sent the socket to some other
     * process before it was connected and that process closed
     * the socket...
     * Either way, there is nothing to do here.
     */

    SGDBG( ("WIN-ESAIO", "esaio_completion_connect -> verify open\r\n") );

    if (IS_OPEN(descP->writeState)) {

        /* WE SHOULD TEST THAT WE ARE CONNECTING,
         * SINCE IF WE ARE NOT THAT MEANS THAT THE OPERATION WAS
         * 'cancelled'!
         * But if the connect has been cancelled, what do do here?
         * shutdown?
         * So, if the connect was successful, we need to terminate
         * connection: shutdown
         * But if the if it was unsuccessful, we can assume that
         * the connection may have already been cancelled...
         *
         * When cancel:
         *      State == connected => shutdown + set state open
         *      State == connecting => se state cancel
         *
         * Here:
         *     State == cancel => shutdown + set state open
         *     State == open => do nothing
         *     State == connecting => normal processing + set state connected
         */

        /* Still connecting? */

        if (descP->connectorP != NULL) {

            /* So, did the connect attempt succeed or not? */
            if (error == NO_ERROR) {

                int err;

                /* *** Success! *** 
                 * CompletionStatus = ok
                 */

                SGDBG( ("WIN-ESAIO",
                        "esaio_completion_connect -> success\r\n") );

                /* We need to make sure peername and sockname works! */

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_completion_connect {%d} -> "
                        "update connect context\r\n",
                        descP->sock) );

                err = ESAIO_UPDATE_CONNECT_CONTEXT( descP->sock );

                if (err == 0) {
                    descP->writeState &=
                        ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
                    descP->writeState |= ESOCK_STATE_CONNECTED;
                    completionStatus = esock_atom_ok;

                } else {

                    int          save_errno = sock_errno();
                    ERL_NIF_TERM reason     = MKA(env,
                                                  erl_errno_id(save_errno));

                    SSDBG( descP, ("WIN-ESAIO",
                                   "esaio_connect_stream {%d} -> "
                                   "connect context update failed: %T (%d)\r\n",
                                   descP->sock, reason, save_errno) );

                    sock_close(descP->sock);
                    descP->writeState = ESOCK_STATE_CLOSED;

                    WSACleanup();

                    completionStatus =
                        esock_make_error_t2r(env,
                                             MKA(env, "update_connect_context"),
                                             reason);

                }

            } else {

                /* *** Failure! ***
                 * CompletionStatus = {error, Reason}
                 */

                ERL_NIF_TERM reason = MKA(env, erl_errno_id(error));

                SGDBG( ("WIN-ESAIO",
                        "esaio_completion_connect -> failure: %T\r\n",
                        reason) );

                completionStatus = esock_make_error(env, reason);

            }

            completionInfo = MKT2(env,
                                  CP_TERM(env, descP->connector.ref),
                                  completionStatus);

            /* Send a 'connect' completion message */
            esaio_send_completion_msg(env,
                                      descP,
                                      &descP->connector.pid,
                                      descP->connector.env,
                                      CP_TERM(descP->connector.env,
                                              opP->data.connect.sockRef),
                                      completionInfo);

            /* Finalize */

            ESOCK_ASSERT( (descP->connectorP != NULL) );

            esock_requestor_release("esaio_completion_connect finalize -> "
                                    "connected",
                                    env, descP, &descP->connector);
            descP->connectorP  = NULL;
            descP->writeState &=
                ~(ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);
            descP->writeState |= ESOCK_STATE_CONNECTED;

        } else {

            /* *Not* connecting! Connect has been cancelled => cleanup
             * If the op failed, its safe to assume that the error is
             * the result of the cancellation, and we do not actually
             * need to do anything here.
             * If however, the connect succeeded, we need to do "some 
             * cleanup".
             * But what can we do here? We have a connected socket,
             * that the owner actually do not want...
             * Send an abort message to the owner?
             */

            if (error == NO_ERROR) {
                ERL_NIF_TERM sockRef = enif_make_resource(env, descP);

                sock_close(descP->sock);
                descP->readState  = ESOCK_STATE_CLOSED;
                descP->writeState = ESOCK_STATE_CLOSED;

                esock_send_simple_abort_msg(env, descP, &descP->ctrlPid,
                                            sockRef, esock_atom_closed);

            }
        }
    }
    
    MUNLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_connect -> clear and delete op env\r\n") );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_connect", opP->env);
    esock_free_env("esaio_completion_connect", opP->env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_connect -> done\r\n") );

    return FALSE;
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
BOOLEAN_T esaio_completion_accept(ESAIOThreadData* dataP,
                                  ESockDescriptor* descP,
                                  ESAIOOperation*  opP,
                                  int              error)
{
    ErlNifEnv*   env = dataP->env;

    MLOCK(descP->readMtx);

    SGDBG( ("WIN-ESAIO", "esaio_completion_accept -> verify open\r\n") );

    if (IS_OPEN(descP->readState)) {

        ESockRequestor req;

        /* Is "this" accept still valid? */
        if (esock_acceptor_get(env, descP,
                               &opP->data.accept.accRef, &opP->caller,
                               &req)) {

            esaio_completion_accept_completed(env, descP, opP, &req, error);

        } else {

            esaio_completion_accept_not_active(opP, error);

        }
    } else {

        esaio_completion_accept_closed(descP, opP, error);

    }

    MUNLOCK(descP->readMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_accept -> clear and delete op env\r\n") );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_accept - op cleanup", opP->env);
    esock_free_env("esaio_completion_accept - op cleanup", opP->env);
    FREE( opP->data.accept.buf );    
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_accept -> done\r\n") );

    return FALSE;
}



static
void esaio_completion_accept_completed(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ESAIOOperation*  opP,
                                       ESockRequestor*  reqP,
                                       int              error)
{
    ERL_NIF_TERM completionStatus, completionInfo;

    ESOCK_ASSERT( DEMONP("esaio_completion_accept_completed - acceptor",
                         env, descP, &reqP->mon) == 0);

    if (error == NO_ERROR) {

        int              err;
        ESockDescriptor* accDescP;
        ERL_NIF_TERM     accRef, accSocket;

        /* *** Success! ***
         * CompletionStatus = {ok, NewSocket}
         * CompletionInfo   = {ConnRef, CompletionStatus}
         */

        /* We need to make sure peername and sockname works! */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_accept_completed {%d} -> "
                "success - update accept context\r\n",
                descP->sock) );

        err = ESAIO_UPDATE_ACCEPT_CONTEXT( opP->data.accept.asock,
                                           opP->data.accept.lsock );

        if (err == 0) {

            int save_errno;

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_accept_completed -> "
                    "create (accepted) descriptor\r\n") );

            accDescP           = esock_alloc_descriptor(opP->data.accept.asock);

            if (ESAIO_OK != (save_errno = esaio_add_socket(accDescP))) {
                // See esock_dtor for what needs done!
                ERL_NIF_TERM tag    = esock_atom_add_socket;
                ERL_NIF_TERM reason = MKA(env, erl_errno_id(save_errno));

                ESOCK_CNT_INC(env, descP, CP_TERM(env, opP->data.accept.lSockRef),
                              esock_atom_acc_fails, &descP->accFails, 1);

                SSDBG( descP,
                       ("WIN-ESAIO",
                        "esaio_open_plain -> "
                        "failed adding (accepted) socket to completion port: "
                        "%T (%d)\r\n",
                        reason, save_errno) );

                esock_dealloc_descriptor(env, accDescP);
                sock_close(opP->data.accept.asock);

                /* This should really be:
                 *     {error, {invalid, {add_to_completion_port, Reason}}}
                 */

                completionStatus = esock_make_error_t2r(env, tag, reason);

            } else {

                ESOCK_CNT_INC(env, descP, opP->data.accept.lSockRef,
                              esock_atom_acc_success,
                              &descP->accSuccess, 1);

                accDescP->domain   = descP->domain;
                accDescP->type     = descP->type;
                accDescP->protocol = descP->protocol;

                MLOCK(descP->writeMtx);

                accDescP->rBufSz   = descP->rBufSz;  // Inherit buffer size
                accDescP->rNum     = descP->rNum;    // Inherit buffer uses
                accDescP->rNumCnt  = 0;
                accDescP->rCtrlSz  = descP->rCtrlSz; // Inherit buffer size
                accDescP->wCtrlSz  = descP->wCtrlSz; // Inherit buffer size
                accDescP->iow      = descP->iow;     // Inherit iow
                accDescP->dbg      = descP->dbg;     // Inherit debug flag
                accDescP->useReg   = descP->useReg;  // Inherit useReg flag
                esock_inc_socket(accDescP->domain, accDescP->type,
                                 accDescP->protocol);

                accRef = enif_make_resource(env, accDescP);
                enif_release_resource(accDescP);
                accSocket = esock_mk_socket(env, accRef);
                
                accDescP->ctrlPid = opP->caller;

                ESOCK_ASSERT( MONP("esaio_completion_accept_completed -> ctrl",
                                   env, accDescP,
                                   &accDescP->ctrlPid,
                                   &accDescP->ctrlMon) == 0 );

                accDescP->writeState |= ESOCK_STATE_CONNECTED;

                MUNLOCK(descP->writeMtx);

                /* And finally (maybe) update the registry */
                if (descP->useReg)
                    esock_send_reg_add_msg(env, descP, accRef);

                completionStatus = esock_make_ok2(env, accSocket);

            }

        } else {

            int          save_errno = sock_errno();
            ERL_NIF_TERM tag        = esock_atom_update_accept_context;
            ERL_NIF_TERM reason     = MKA(env, erl_errno_id(save_errno));

            SSDBG( descP, ("WIN-ESAIO",
                           "esaio_completion_accept_completed {%d} -> "
                           "accept context update failed: %T (%d)\r\n",
                           descP->sock, reason, save_errno) );

            sock_close(descP->sock);
            descP->writeState = ESOCK_STATE_CLOSED;

            WSACleanup();

            completionStatus = esock_make_error_t2r(env, tag, reason);

        }

    } else {

        /* *** Failure! ***
         * CompletionStatus = {error, Reason}
         */

        ERL_NIF_TERM reason = MKA(env, erl_errno_id(error));

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_accept_completed -> failure: %T\r\n",
                reason) );

        completionStatus = esock_make_error(env, reason);

    }

    completionInfo = MKT2(env,
                          CP_TERM(env, opP->data.accept.accRef),
                          completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, &opP->caller), completionInfo) );

    /* Send a 'accept' completion message */
    esaio_send_completion_msg(env,                       // Send env
                              descP,                     // Descriptor
                              &opP->caller,              // Msg destination
                              opP->env,                  // Msg env
                              opP->data.accept.lSockRef, // Dest socket
                              completionInfo);           // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_accept_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_accept_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_accept_completed -> req cleanup",
                   reqP->env);

    /* *Maybe* update listen socket (read) state
     * (depends on if the queue is now empty)
     */
    if (descP->acceptorsQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_ACCEPTING;
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_accept_completed -> done\r\n") );
}



static
void esaio_completion_accept_not_active(ESAIOOperation*  opP,
                                        int              error)
{
    /* This accept request is *not* "active"!
     * Accept has been cancelled => cleanup
     * If the op failed, its safe to assume that the error is
     * the result of the cancellation, and we do not actually
     * need to do anything here.
     * If however, the accept succeeded, we need to do "some 
     * cleanup".
     * But what can we do here?
     * We have an *unknown* connected socket.
     * Send an abort message to the (listen socket) owner?
     * Increment a counter (unexpected accepts)?
     */

    if (error == NO_ERROR) {

        SGDBG( ("WIN-ESAIO",
                "esaio_completion_accept_not_active -> "
                "success for cancelled accept\r\n") );

        MLOCK(ctrl.cntMtx);
            
        esock_cnt_inc(&ctrl.unexpectedAccepts, 1);

        MUNLOCK(ctrl.cntMtx);

        sock_close(opP->data.accept.asock);
    }
}


static
void esaio_completion_accept_closed(ESockDescriptor* descP,
                                    ESAIOOperation*  opP,
                                    int              error)
{
    /* Even though the listen socket is not open,
     * the accept may still have succeeded (race) and
     * therefor we need to check the result!
     */

    if (error == NO_ERROR) {

        SGDBG( ("WIN-ESAIO",
                "esaio_completion_accept -> "
                "success for closed (listen) socket (%d)\r\n",
                descP->sock) );

        MLOCK(ctrl.cntMtx);
            
        esock_cnt_inc(&ctrl.unexpectedAccepts, 1);

        MUNLOCK(ctrl.cntMtx);

        sock_close(opP->data.accept.asock);
    }
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
                                ESAIOOperation*  opP,
                                int              error)
{
    ErlNifEnv* env = dataP->env;

    MLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO", "esaio_completion_send -> verify open\r\n") );

    if (IS_OPEN(descP->writeState)) {

        ESockRequestor req;

        /* Is "this" send still valid? */
        if (esock_writer_get(env, descP,
                             &opP->data.send.sendRef, &opP->caller,
                             &req)) {

            esaio_completion_send_completed(env, descP,
                                            (OVERLAPPED*) opP,
                                            opP->env,
                                            &opP->caller,
                                            opP->data.send.sockRef,
                                            opP->data.send.sendRef,
                                            opP->data.send.wbuf.len,
                                            &req, error);

        } else {

            esaio_completion_send_not_active(error);

        }
    } else {

        esaio_completion_send_closed(descP, error);

    }

    MUNLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_send -> clear and delete op env\r\n") );

    /* "Manually" allocated buffer */
    FREE( opP->data.send.wbuf.buf );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_send - op cleanup", opP->env);
    esock_free_env("esaio_completion_send - op cleanup", opP->env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_send -> done\r\n") );

    return FALSE;
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
BOOLEAN_T esaio_completion_sendto(ESAIOThreadData* dataP,
                                  ESockDescriptor* descP,
                                  ESAIOOperation*  opP,
                                  int              error)
{
    ErlNifEnv* env = dataP->env;

    MLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO", "esaio_completion_sendto -> verify open\r\n") );

    if (IS_OPEN(descP->writeState)) {

        ESockRequestor req;

        /* Is "this" send still valid? */
        if (esock_writer_get(env, descP,
                             &opP->data.sendto.sendRef, &opP->caller,
                             &req)) {

            esaio_completion_send_completed(env, descP,
                                            (OVERLAPPED*) opP,
                                            opP->env,
                                            &opP->caller,
                                            opP->data.sendto.sockRef,
                                            opP->data.sendto.sendRef,
                                            opP->data.sendto.wbuf.len,
                                            &req, error);

        } else {

            esaio_completion_send_not_active(error);

        }
    } else {

        esaio_completion_send_closed(descP, error);

    }

    MUNLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_sendto -> clear and delete op env\r\n") );

    /* "Manually" allocated buffer */
    FREE( opP->data.sendto.wbuf.buf );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_sendto - op cleanup", opP->env);
    esock_free_env("esaio_completion_sendto - op cleanup", opP->env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_sendto -> done\r\n") );

    return FALSE;
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
BOOLEAN_T esaio_completion_sendmsg(ESAIOThreadData* dataP,
                                   ESockDescriptor* descP,
                                   ESAIOOperation*  opP,
                                   int              error)
{
    ErlNifEnv* env = dataP->env;

    MLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO", "esaio_completion_sendmsg -> verify open\r\n") );

    if (IS_OPEN(descP->writeState)) {

        ESockRequestor req;

        /* Is "this" send still valid? */
        if (esock_writer_get(env, descP,
                             &opP->data.sendto.sendRef, &opP->caller,
                             &req)) {

            DWORD toWrite = 0;

            /* Calculate how much data *in total* we was supposed to write */
            for (int i = 0; i < opP->data.sendmsg.iovec->iovcnt; i++) {
                toWrite += opP->data.sendmsg.iovec->iov[i].iov_len;
            }

            esaio_completion_send_completed(env, descP,
                                            (OVERLAPPED*) opP,
                                            opP->env,
                                            &opP->caller,
                                            opP->data.sendto.sockRef,
                                            opP->data.sendto.sendRef,
                                            toWrite,
                                            &req, error);

        } else {

            esaio_completion_send_not_active(error);

        }
    } else {

        esaio_completion_send_closed(descP, error);

    }

    MUNLOCK(descP->writeMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_sendmsg -> clear and delete op env\r\n") );

    /* "Manually" allocated buffers */
    FREE( opP->data.sendmsg.msg.lpBuffers );
    if (opP->data.sendmsg.ctrlBuf != NULL)
        FREE( opP->data.sendmsg.ctrlBuf );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_sendmsg - op cleanup", opP->env);
    esock_free_env("esaio_completion_sendmsg - op cleanup", opP->env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_sendmsg -> done\r\n") );

    return FALSE;
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
                                     ESockRequestor*  reqP,
                                     int              error)
{
    ERL_NIF_TERM completionStatus, completionInfo;

    ESOCK_ASSERT( DEMONP("esaio_completion_send_completed - sender",
                         env, descP, &reqP->mon) == 0);

    if (error == NO_ERROR) {
        DWORD written = 0;

        /* Success, but we need to check how much we actually got.
         * Also the 'flags' (which we currentöy ignore)
         *
         * CompletionStatus = ok | {ok, RestData}
         * CompletionInfo   = {ConnRef, CompletionStatus}
         */

        MLOCK(descP->writeMtx);

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

            /* Now what?
             * We know we wrote "something" but we cannot figure out
             * how much...
             */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_send_completed -> "
                    "overlapped result failure\r\n") );

            completionStatus =
                esaio_completion_get_ovl_result_fail(env,
                                                     descP,
                                                     sock_errno());
        }

        MUNLOCK(descP->writeMtx);

        completionStatus = esock_atom_ok;

    } else {

        /* *** Failure! ***
         * CompletionStatus = {error, Reason}
         */

        ERL_NIF_TERM reason = MKA(env, erl_errno_id(error));

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send_completed -> failure: %T\r\n",
                reason) );

        completionStatus = esock_make_error(env, reason);

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

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_send_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_send_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_send_completed -> req cleanup",
                   reqP->env);

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    if (descP->writersQ.first == NULL) {
        descP->writeState &= ~ESOCK_STATE_SELECTED;
    }

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
void esaio_completion_send_not_active(int error)
{
    /* This send request is *not* "active"!
     * Send has been cancelled => cleanup
     * If the op failed, its safe to assume that the error is
     * the result of the cancellation, and we do not actually
     * need to do anything here.
     * If however, the send succeeded, we need to do "some 
     * cleanup".
     * But what can we do here?
     * Send an abort message to the sender or/and owner?
     * Increment a counter (unexpected acceptssends)?
     */

    if (error == NO_ERROR) {

        SGDBG( ("WIN-ESAIO",
                "esaio_completion_send_not_active -> "
                "success for cancelled send\r\n") );

        MLOCK(ctrl.cntMtx);
            
        esock_cnt_inc(&ctrl.unexpectedWrites, 1);

        MUNLOCK(ctrl.cntMtx);

    }
}


/* *** esaio_completion_send_closed ***
 * A send request has completed but the socket is closed.
 * When the socket is closed, all outstanding requests
 * are "flushed", so we do not actually need to "do" anything
 * here (other then maybe count unexpected writes).
 */
static
void esaio_completion_send_closed(ESockDescriptor* descP,
                                  int              error)
{
    if (error == NO_ERROR) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_send_closed -> "
                "success for closed socket (%d)\r\n",
                descP->sock) );

        MLOCK(ctrl.cntMtx);
            
        esock_cnt_inc(&ctrl.unexpectedWrites, 1);

        MUNLOCK(ctrl.cntMtx);

    }
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
                                ESAIOOperation*  opP,
                                int              error)
{
    ErlNifEnv* env = dataP->env;

    MLOCK(descP->readMtx);

    SGDBG( ("WIN-ESAIO", "esaio_completion_recv -> verify open\r\n") );

    if (IS_OPEN(descP->readState)) {

        ESockRequestor req;

        /* Is "this" read still valid? */
        if (esock_reader_get(env, descP,
                             &opP->data.recv.recvRef, &opP->caller,
                             &req)) {

            esaio_completion_recv_completed(env, descP, opP, &req, error);

        } else {

            esaio_completion_recv_not_active(descP, error);

        }

    } else {

        esaio_completion_recv_closed(descP, error);

    }

    MUNLOCK(descP->readMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_recv -> clear and delete op env\r\n") );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_recv - op cleanup", opP->env);
    esock_free_env("esaio_completion_recv - op cleanup", opP->env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_recv -> done\r\n") );

    return FALSE;
}



/* *** esaio_completion_recv_completed ***
 * The recv request has completed.
 */
static
void esaio_completion_recv_completed(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ESAIOOperation*  opP,
                                     ESockRequestor*  reqP,
                                     int              error)
{
    ERL_NIF_TERM completionStatus, completionInfo;

    ESOCK_ASSERT( DEMONP("esaio_completion_recv_completed - sender",
                         env, descP, &reqP->mon) == 0);

    if (error == NO_ERROR) {
        DWORD read, flags;

        /* Success, but we need to check how much we actually got.
         * Also the 'flags' (which we currentöy ignore)
         */

        MLOCK(descP->writeMtx);

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_completed ->"
                "success - try get overlapped result\r\n") );

        if (get_recv_ovl_result(descP->sock, (OVERLAPPED*) opP,
                                &read, &flags)) {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_recv_completed -> overlapped result: "
                    "\r\n   read:        %d"
                    "\r\n   buffer size: %d"
                    "\r\n   flags:       %d"
                    "\r\n", read, opP->data.recv.buf.size, flags) );

            /* *** Success! ***
             * CompletionStatus = {ok, {Flags, Bin}} (should be)
             * CompletionInfo   = {ConnRef, CompletionStatus}
             */

            if (read == opP->data.recv.buf.size) {
                /* We filled the buffer => done */

                completionStatus = esaio_completion_recv_done(env, descP, opP,
                                                              flags);

            } else {

                /* Only used a part of the buffer =>
                 * needs splitting and (maybe) retry (its up to the caller)!
                 */

                completionStatus = esaio_completion_recv_partial(env,
                                                                 descP,
                                                                 opP,
                                                                 reqP,
                                                                 read,
                                                                 flags);
            }

        } else {

            /* Now what?
             * We know we read "something" but we cannot figure out
             * how much...
             */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_recv_completed -> "
                    "overlapped result failure\r\n") );

            completionStatus =
                esaio_completion_get_ovl_result_fail(env,
                                                     descP,
                                                     sock_errno());
       }

        MUNLOCK(descP->writeMtx);

    } else {

        /* *** Failure! ***
         * CompletionStatus = {error, Reason}
         */

        ERL_NIF_TERM reason = MKA(env, erl_errno_id(error));

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_completed -> failure: %T\r\n",
                reason) );

        completionStatus = esock_make_error(env, reason);

    }

    completionInfo = MKT2(env,
                          CP_TERM(env, opP->data.recv.recvRef),
                          completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, &opP->caller), completionInfo) );

    /* Send a 'send' completion message */
    esaio_send_completion_msg(env,                    // Send env
                              descP,                  // Descriptor
                              &opP->caller,           // Msg destination
                              opP->env,               // Msg env
                              opP->data.recv.sockRef, // Dest socket
                              completionInfo);        // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recv_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_recv_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_recv_completed -> req cleanup",
                   reqP->env);

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

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
                                        ESAIOOperation*  opP,
                                        DWORD            flags)
{
    ERL_NIF_TERM data;
    ERL_NIF_TERM sockRef = opP->data.recv.sockRef;
    ERL_NIF_TERM recvRef = opP->data.recv.recvRef;
    DWORD        read    = opP->data.recv.buf.size;

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
    data = MKBIN(env, &opP->data.recv.buf);

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
    return esock_make_ok2(env, data);
}


/* *** esaio_completion_recv_partial ***
 *
 * A partial read, that is only part of the buffer was used.
 *
 */
static
ERL_NIF_TERM esaio_completion_recv_partial(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ESAIOOperation*  opP,
                                           ESockRequestor*  reqP,
                                           DWORD            read,
                                           DWORD            flags)
{
    ERL_NIF_TERM res;
    ERL_NIF_TERM sockRef = opP->data.recv.sockRef;
    ERL_NIF_TERM recvRef = opP->data.recv.recvRef;
    DWORD        toRead  = opP->data.recv.toRead;

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
        (descP->type != SOCK_STREAM) ||
        (COMPARE(recvRef, esock_atom_zero) == 0)) {

        /* +++ We only got a partial,           ***
         * *** but we should not wait for more. +++
         * +++ Must split it into a sub-binary. +++
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_partial(%T) {%d} -> done reading\r\n",
                sockRef, descP->sock) );

        res = esaio_completion_recv_partial_done(env, descP, opP,
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

        res = esaio_completion_recv_partial_part(env, descP, opP,
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
                                                ESAIOOperation*  opP,
                                                ssize_t          read,
                                                DWORD            flags)
{
    ERL_NIF_TERM sockRef = opP->data.recv.sockRef;
    ERL_NIF_TERM data;

    ESOCK_CNT_INC(env, descP, opP->data.recv.sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, opP->data.recv.sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, read);

    if (read > descP->readPkgMax)
        descP->readPkgMax = read;

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, &opP->data.recv.buf);
    data = MKSBIN(env, data, 0, read);

    (void) flags;

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_partial_done(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return esock_make_ok2(env, data);
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
                                                ESAIOOperation*  opP,
                                                ssize_t          read,
                                                DWORD            flags)
{
    /* This is just a "placeholder". Is this really all we need to do? */
    return esaio_completion_recv_partial_done(env, descP, opP, read, flags);
}



/* *** esaio_completion_recv_not_active ***
 * A recv request has completed but the request is no longer valid.
 */
static
void esaio_completion_recv_not_active(ESockDescriptor* descP,
                                      int              error)
{
    /* This receive request is *not* "active"!
     * The receive (recv) operation has been cancelled => cleanup
     * If the op failed, its safe to assume that the error is
     * the result of the cancellation, and we do not actually
     * need to do anything here.
     * If however, the recv succeeded, we need to do "some 
     * cleanup".
     * But what can we do here?
     * Send an abort message to the reader or/and owner?
     * Increment a counter (unexpected readsa)?
     */

    if (error == NO_ERROR) {

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recv_not_active -> "
                "success for cancelled recv (%d)\r\n", descP->sock) );

        MLOCK(ctrl.cntMtx);
            
        esock_cnt_inc(&ctrl.unexpectedReads, 1);

        MUNLOCK(ctrl.cntMtx);

    }
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
BOOLEAN_T esaio_completion_recvfrom(ESAIOThreadData* dataP,
                                    ESockDescriptor* descP,
                                    ESAIOOperation*  opP,
                                    int              error)
{
    ErlNifEnv* env = dataP->env;

    MLOCK(descP->readMtx);

    SGDBG( ("WIN-ESAIO", "esaio_completion_recvfrom -> verify open\r\n") );

    if (IS_OPEN(descP->readState)) {

        ESockRequestor req;

        /* Is "this" read still valid? */
        if (esock_reader_get(env, descP,
                             &opP->data.recvfrom.recvRef,
                             &opP->caller,
                             &req)) {

            esaio_completion_recvfrom_completed(env, descP, opP, &req, error);

        } else {

            esaio_completion_recv_not_active(descP, error);

        }

    } else {

        esaio_completion_recv_closed(descP, error);

    }

    MUNLOCK(descP->readMtx);

    SGDBG( ("WIN-ESAIO",
            "esaio_completion_recvfrom -> clear and delete op env\r\n") );

    /* No need for this "stuff" anymore */
    esock_clear_env("esaio_completion_recvfrom - op cleanup", opP->env);
    esock_free_env("esaio_completion_recvfrom - op cleanup", opP->env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_recvfrom -> done\r\n") );

    return FALSE;
}



/* *** esaio_completion_recvfrom_completed ***
 * The recvfrom request has completed.
 */
static
void esaio_completion_recvfrom_completed(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ESAIOOperation*  opP,
                                         ESockRequestor*  reqP,
                                         int              error)
{
    ERL_NIF_TERM completionStatus, completionInfo;

    ESOCK_ASSERT( DEMONP("esaio_completion_recvfrom_completed - sender",
                         env, descP, &reqP->mon) == 0);

    if (error == NO_ERROR) {
        DWORD read, flags;

        /* Success, but we need to check how much we actually got.
         * Also the 'flags' (which we currentöy ignore)
         */

        MLOCK(descP->writeMtx);

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom_completed ->"
                "success - try get overlapped result\r\n") );

        if (get_recv_ovl_result(descP->sock, (OVERLAPPED*) opP,
                                &read, &flags)) {

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_recvfrom_completed -> overlapped result: "
                    "\r\n   read:        %d"
                    "\r\n   buffer size: %d"
                    "\r\n   flags:       %d"
                    "\r\n", read, opP->data.recvfrom.buf.size, flags) );

            /* *** Success! ***
             * CompletionStatus = {ok, {Flags, Bin}} (should be)
             * CompletionInfo   = {ConnRef, CompletionStatus}
             */

            if (read == opP->data.recvfrom.buf.size) {
                /* We filled the buffer => done */

                completionStatus = esaio_completion_recvfrom_done(env,
                                                                  descP, opP,
                                                                  flags);

            } else {

                /* Only used a part of the buffer =>
                 * needs splitting and (maybe) retry (its up to the caller)!
                 */

                completionStatus = esaio_completion_recvfrom_partial(env,
                                                                     descP,
                                                                     opP,
                                                                     reqP,
                                                                     read,
                                                                     flags);
            }

        } else {

            /* Now what?
             * We know we read "something" but we cannot figure out
             * how much...
             */

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_recvfrom_completed -> "
                    "overlapped result failure\r\n") );

            completionStatus =
                esaio_completion_get_ovl_result_fail(env,
                                                     descP,
                                                     sock_errno());
       }

        MUNLOCK(descP->writeMtx);

    } else {

        /* *** Failure! ***
         * CompletionStatus = {error, Reason}
         */

        ERL_NIF_TERM reason = MKA(env, erl_errno_id(error));

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_completion_recvfrom_completed -> failure: %T\r\n",
                reason) );

        completionStatus = esock_make_error(env, reason);

    }

    completionInfo = MKT2(env,
                          CP_TERM(env, opP->data.recvfrom.recvRef),
                          completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_completed -> "
            "send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, &opP->caller), completionInfo) );

    /* Send a 'send' completion message */
    esaio_send_completion_msg(env,                        // Send env
                              descP,                      // Descriptor
                              &opP->caller,    // Msg destination
                              opP->env,                   // Msg env
                              opP->data.recvfrom.sockRef, // Dest socket
                              completionInfo);            // Info

    /* *** Finalize *** */

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_completed -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_recvfrom_completed -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_recvfrom_completed -> req cleanup",
                   reqP->env);

    /* *Maybe* update socket (write) state
     * (depends on if the queue is now empty)
     */
    if (descP->readersQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_SELECTED;
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_completion_recvfrom_completed -> done\r\n") );
}



/* *** esaio_completion_recvfrom_done ***
 *
 * A complete read (filled the provided buffer).
 *
 */
static
ERL_NIF_TERM esaio_completion_recvfrom_done(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ESAIOOperation*  opP,
                                            DWORD            flags)
{
    ERL_NIF_TERM res, data, eSockAddr;
    ERL_NIF_TERM sockRef = opP->data.recvfrom.sockRef;
    ERL_NIF_TERM recvRef = opP->data.recvfrom.recvRef;
    DWORD        read    = opP->data.recvfrom.buf.size;

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

    esock_encode_sockaddr(env,
                          &opP->data.recvfrom.fromAddr,
                          opP->data.recvfrom.addrLen,
                          &eSockAddr);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, &opP->data.recvfrom.buf);

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
    res = esock_make_ok2(env, MKT2(env, eSockAddr, data));

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recv_done(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
}



/* *** esaio_completion_recvfrom_partial ***
 *
 * A partial read, that is only part of the buffer was used.
 *
 */
static
ERL_NIF_TERM esaio_completion_recvfrom_partial(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ESAIOOperation*  opP,
                                               ESockRequestor*  reqP,
                                               DWORD            read,
                                               DWORD            flags)
{
    ERL_NIF_TERM res, data, eSockAddr;
    ERL_NIF_TERM sockRef = opP->data.recvfrom.sockRef;
    ERL_NIF_TERM recvRef = opP->data.recvfrom.recvRef;

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

    esock_encode_sockaddr(env,
                          &opP->data.recvfrom.fromAddr,
                          opP->data.recvfrom.addrLen,
                          &eSockAddr);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, &opP->data.recvfrom.buf);
    data = MKSBIN(env, data, 0, read);

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

    res = esock_make_ok2(env, MKT2(env, eSockAddr, data));

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_recvfrom_partial(%T) {%d} -> done\r\n",
            sockRef, descP->sock) );

    return res;
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
    ERL_NIF_TERM eerrno = MKA(env, erl_errno_id(error));
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
                                   ESAIOOperation*  opP,
                                   DWORD            numBytes,
                                   int              error)
{
    (void) dataP;
    (void) descP;
    (void) opP;
    (void) numBytes;
    (void) error;

    MLOCK(ctrl.cntMtx);
            
    esock_cnt_inc(&ctrl.unknownCmds, 1);

    MUNLOCK(ctrl.cntMtx);

    return FALSE;
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
    SGDBG( ("WIN-ESAIO", "esaio_dtor -> entry\r\n") );

    if (IS_SELECTED(descP)) {
        /* We have used the socket in the select machinery,
         * so we must have closed it properly to get here
         */
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
                ESockDescriptor* descP,
                ErlNifEvent      fd)
{

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_stop {%d/%d} -> entry\r\n", descP->sock, fd) );

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Inform waiting Closer, or close socket
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (! enif_is_pid_undefined(&descP->closerPid)) {
        /* We have a waiting closer process after nif_close()
         * - send message to trigger nif_finalize_close()
         */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esaio_stop {%d/%d} -> send close msg to %T\r\n",
                descP->sock, fd, MKPID(env, &descP->closerPid)) );

        esock_send_close_msg(env, descP, &descP->closerPid);
        /* Message send frees closeEnv */
        descP->closeEnv = NULL;
        descP->closeRef = esock_atom_undefined;

    } else {
        int err;

        /* We do not have a closer process
         * - have to do an unclean (non blocking) close */

        err = esock_close_socket(env, descP, FALSE);

        if (err != 0)
            esock_warning_msg("[WIN-ESAIO] Failed closing socket without "
                              "closer process: "
                              "\r\n   Controlling Process: %T"
                              "\r\n   Descriptor:          %d"
                              "\r\n   Errno:               %d (%T)"
                              "\r\n",
                              descP->ctrlPid, descP->sock,
                              err, MKA(env, erl_errno_id(err)));
    }

    SSDBG( descP,
           ("WIN-ESAIO", "esaio_stop {%d/%d} -> done\r\n", descP->sock, fd) );

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
                                  err, MKA(env, erl_errno_id(err)));
        } else {
            /* Since there is a closeEnv esock_stop() has not run yet
             * - when it finds that there is no closer process
             *   it will close the socket and ignore the close_msg
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
                              err, MKA(env, erl_errno_id(err)));
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
 * If a "complete" send has been made, the next (waiting) writer will be 
 * scheduled (if there is one).
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
         * So, no need to store the data (in the "queue").
         * But we do need to clean up the overlapped structure.
         */

        *cleanup = TRUE;

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
            "send_check_result {%d} -> pending\r\n", descP->sock) );

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

    reason = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
           ("WIN-ESAIO", "send_check_fail(%T) {%d} -> error: %d (%T)\r\n",
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


/* *** get_send_ovl_result ***
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
