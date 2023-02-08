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
 *
 *
 * Note:
 * We should have a "general" function that returns an info term
 * about this implementation: esaio_info()
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
#define ESAIO_ERR_THREAD_OPTS_CREATE 0x0006
#define ESAIO_ERR_THREAD_CREATE      0x0007

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
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_open_O(domain, type, proto) \
    WSASocket((domain), (type), (proto), NULL, 0, WSA_FLAG_OVERLAPPED)
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
    WSADATA        wsaData;
    HANDLE         cport;

    SOCKET         dummy; // Used for extracting AcceptEx and ConnectEx
    LPFN_ACCEPTEX  accept;
    LPFN_CONNECTEX connect;

    /* Thread pool stuff.
     * The size of the pool is configurable. */
    DWORD          numThreads;
    ESAIOThread*   threads;

    /* Misc stuff */
    BOOLEAN_T      dbg;
    BOOLEAN_T      sockDbg;

    /* Counter stuff */
    ErlNifMutex*   cntMtx;
    ESockCounter   unexpectedAccepts;
    ESockCounter   unknownCmds;

} ESAIOControl;


/* An 'operation', recv/recvfrom/recvmsg and send/sendto/sendmsg,
 * accept or connect, is 'encoded' into this structure, which is
 * "passed around".
 */
typedef struct __ESAIOOperation {
    /* Has to be first and is *only* used by I/O Completion Port framework */
    WSAOVERLAPPED        ol;

    /* *** Commands *** */
#define ESAIO_OP_NONE         0x0000  // None
    /* "system" commands */
#define ESAIO_OP_TERMINATE    0x0001  // Terminate
#define ESAIO_OP_DEBUG        0x0002  // Change debug level for thread(s)
    /* Commands for establishing connections; connect and accept */
#define ESAIO_OP_CONNECT      0x0011  // ConnectEx (function pointer)
#define ESAIO_OP_ACCEPT       0x0012  // AcceptEx (function pointer)

    unsigned int          tag;     // The 'tag' of the operation

    /* Generic "data" field.
     * This is different for each 'operation'!
     * Also, not all opererations have this!
     */

    union {
        /* +++ accept +++ */
        struct {
            /* The socket, sock, is created empty and then provided as an
             * argumented to AcceptEx (together with the listen socket
             * and the other arguments).
             * When AcceptEx has completed successfully, the socket, s, is
             * usable.
             * But in order for the functions sockname and peername to work,
             * the SO_UPDATE_ACCEPT_CONTEXT option must be set on the
             * accepted socket, sock. */
            ErlNifPid     pid;    /* Caller of 'accept' */
            SOCKET        lsock;  /* The listen socket */
            SOCKET        asock;  /* The "accepted" socket.
                                   * This is created "in advance"
                                   * and then sent to AcceptEx as an argument.
                                   */
            char*         buf;    /* Size depends on domain.
                                   * This is used for 'initial data', 
                                   * 'local address' and 'remote address'.
                                   * We use neither of these, but the 
                                   * AcceptEx function requires this argument!
                                   */
            ErlNifEnv*    env;
            ERL_NIF_TERM  lSockRef; // The listen socket
            ERL_NIF_TERM  accRef;   // The (unique) reference (ID) of the accept
        } accept;

         /* +++ connect +++ */
        struct {
            /* When ConnectEx has completed successfully, 
             * the socket is usable.
             * *But*, in order for the functions sockname and peername to work,
             * the SO_UPDATE_CONNECT_CONTEXT option must be set on the socket.
             */

            ErlNifEnv*   env;
            ERL_NIF_TERM sockRef; // The socket
        } connect;
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
static void esaio_completion_accept_accepted(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ESAIOOperation*  opP,
                                             ESockRequestor*  reqP,
                                             int              error);
static void esaio_completion_accept_not_active(ESAIOOperation*  opP,
                                               int              error);
static void esaio_completion_accept_closed(ESockDescriptor* descP,
                                           ESAIOOperation*  opP,
                                           int              error);
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

    /* Enabling this results in a core dump when calling socket:accept
     * multiple times (the second call fails in env alloc).!
     */
    // ctrl.dbg               = TRUE;
    ctrl.dbg               = dataP->dbg;
    ctrl.sockDbg           = dataP->sockDbg;

    SGDBG( ("WIN-ESAIO", "esaio_init -> entry\r\n") );

    ctrl.cntMtx            = MCREATE("win-esaio.cnt");
    ctrl.unexpectedAccepts = 0;
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
        ctrl.dummy = INVALID_SOCKET;

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
    ERL_NIF_TERM info, numUnexpAccs, numUnknownCmds;
    
    MLOCK(ctrl.cntMtx);

    numUnexpAccs   = MKUI(env, ctrl.unexpectedAccepts);
    numUnknownCmds = MKUI(env, ctrl.unknownCmds);

    MUNLOCK(ctrl.cntMtx);

    {
        ERL_NIF_TERM keys[]  = {esock_atom_name,
                                esock_atom_num_unexpected_accepts,
                                esock_atom_num_unknown_cmds};
        ERL_NIF_TERM vals[]  = {MKA(env, "win_esaio"),
                                numUnexpAccs,
                                numUnknownCmds};
        unsigned int numKeys = NUM(keys);
        unsigned int numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );
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

        opP->data.connect.env     = esock_alloc_env("esaio-connect-stream");
        opP->data.connect.sockRef = CP_TERM(opP->data.connect.env, sockRef);

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
            esock_clear_env("esaio_connect_stream", opP->data.connect.env);
            esock_free_env("esaio_connect_stream", opP->data.connect.env);
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

                esock_clear_env("esaio_connect", opP->data.connect.env);
                esock_free_env("esaio_connect", opP->data.connect.env);
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

    /* Ensure that this caller does not already have a (accept) request waiting */
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
    opP->data.accept.lsock    = descP->sock;
    opP->data.accept.pid      = caller;
    opP->data.accept.env      = esock_alloc_env("esaio_accept - operation");
    opP->data.accept.lSockRef = CP_TERM(opP->data.accept.env, sockRef);
    opP->data.accept.accRef   = CP_TERM(opP->data.accept.env, accRef);

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

        esock_clear_env("esaio_accept - invalid accept socket", opP->data.accept.env);
        esock_free_env("esaio_accept - invalid accept socket", opP->data.accept.env);
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

        esock_clear_env("esaio_accept - success", opP->data.accept.env);
        esock_free_env("esaio_accept - success", opP->data.accept.env);
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

            esock_clear_env("esaio_accept", opP->data.accept.env);
            esock_free_env("esaio_accept", opP->data.accept.env);
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



/* *** essio_accept_accepted ***
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
                    "do_stop {%d} -> cancel failed: %s (%d)\r\n",
                    descP->sock, erl_errno_id(save_errno), save_errno) );

            /* <CHECK>
             * We may need to check what kind of error
             * before we issue this message.
             * </CHECK>
             */

            if (save_errno != ERROR_NOT_FOUND)
                esock_error_msg("Failed cancel outstanding I/O operations:"
                                "\r\n   Socket: " SOCKET_FORMAT_STR
                                "\r\n   Reason: %s (%d)"
                                "\r\n",
                                descP->sock,
                                erl_errno_id(save_errno), save_errno);
            
            ret = FALSE;

        }

    } else {

        ret = FALSE;
    }

    /* +++++++ Waiting Writers +++++++ */

    if (descP->writersQ.first != NULL) {

        /* Inform the waiting Writers - send it an abort message */

        SSDBG( descP,
               ("WIN-ESAIO",
                "esock_do_stop {%d} -> handle waiting writer(s)\r\n",
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
                "esock_do_stop {%d} -> handle waiting reader(s)\r\n",
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
                "esock_do_stop {%d} -> handle waiting acceptors(s)\r\n",
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
 * We have two different cases:
 *   *) Its the current acceptor
 *      Cancel the select!
 *      We need to activate one of the waiting acceptors.
 *   *) Its one of the acceptors ("waiting") in the queue
 *      Simply remove the acceptor from the queue.
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
    esock_clear_env("esaio_completion_connect", opP->data.connect.env);
    esock_free_env("esaio_completion_connect", opP->data.connect.env);
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_connect -> done\r\n") );

    return FALSE;
}



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
 * calls socket:accept(Socket, ..., nowait), the acceptt is
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
                               &opP->data.accept.accRef, &opP->data.accept.pid,
                               &req)) {

            esaio_completion_accept_accepted(env, descP, opP, &req, error);

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
    esock_clear_env("esaio_completion_accept - op cleanup", opP->data.accept.env);
    esock_free_env("esaio_completion_accept - op cleanup", opP->data.accept.env);
    FREE( opP->data.accept.buf );    
    FREE( opP );    
    
    SGDBG( ("WIN-ESAIO", "esaio_completion_accept -> done\r\n") );

    return FALSE;
}



static
void esaio_completion_accept_accepted(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ESAIOOperation*  opP,
                                      ESockRequestor*  reqP,
                                      int              error)
{
    ERL_NIF_TERM completionStatus, completionInfo;

    ESOCK_ASSERT( DEMONP("esaio_completion_accept_accepted - acceptor",
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
                "esaio_completion_accept_accepted {%d} -> "
                "success - update accept context\r\n",
                descP->sock) );

        err = ESAIO_UPDATE_ACCEPT_CONTEXT( opP->data.accept.asock,
                                           opP->data.accept.lsock );

        if (err == 0) {

            int save_errno;

            SSDBG( descP,
                   ("WIN-ESAIO",
                    "esaio_completion_accept_accepted -> "
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
                
                accDescP->ctrlPid = opP->data.accept.pid;

                ESOCK_ASSERT( MONP("esaio_completion_accept_accepted -> ctrl",
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
                           "esaio_completion_accept_accepted {%d} -> "
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
                "esaio_completion_accept_accepted -> failure: %T\r\n",
                reason) );

        completionStatus = esock_make_error(env, reason);

    }

    completionInfo = MKT2(env,
                          CP_TERM(env, opP->data.accept.accRef),
                          completionStatus);

    SSDBG( descP,
           ("WIN-ESAIO",
            "esaio_completion_accept_accepted -> send completion message to %T with"
            "\r\n   CompletionInfo: %T"
            "\r\n", MKPID(env, &opP->data.accept.pid), completionInfo) );

    /* Send a 'accept' completion message */
    esaio_send_completion_msg(env,                       // Send env
                              descP,                     // Descriptor
                              &opP->data.accept.pid,     // Msg destination
                              opP->data.accept.env,      // Msg env
                              opP->data.accept.lSockRef, // Dest socket
                              completionInfo);           // Info

    /* *** Finalize *** */

    SSDBG( descP, ("WIN-ESAIO", "esaio_completion_accept_accepted -> finalize\r\n") );

    /* Request cleanup (demonitor already done above) */
    esock_clear_env("esaio_completion_accept_accepted -> req cleanup",
                    reqP->env);
    esock_free_env("esaio_completion_accept_accepted -> req cleanup",
                   reqP->env);

    /* *Maybe* update listen socket (read) state
     * (depends on if the queue is now empty)
     */
    if (descP->acceptorsQ.first == NULL) {
        descP->readState &= ~ESOCK_STATE_ACCEPTING;
    }

    SSDBG( descP, ("WIN-ESAIO", "esaio_completion_accept_accepted -> done\r\n") );
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
                "esaio_completion_accept -> "
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



/* ==================================================================== *
 *                                                                      *
 *                          Utility functions                           *
 *                                                                      *
 * ==================================================================== *
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
