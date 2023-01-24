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

#define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_close(s)                   closesocket((s))
#define sock_errno()                    WSAGetLastError()
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_open_O(domain, type, proto) \
    WSASocket((domain), (type), (proto), NULL, 0, WSA_FLAG_OVERLAPPED)


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

} ESAIOControl;


/* An 'operation', recv/recvfrom/recvmsg and send/sendto/sendmsg,
 * accept or connect, is 'encoded' into this structure, which is
 * "passed around".
 */
typedef struct __ESAIOOperation {
    /* Has to be first and is *only* used by I/O Completion Port framework */
    WSAOVERLAPPED        ol;

#define ESAIO_OP_NONE         0x0000  // None
#define ESAIO_OP_TERMINATE    0x0001  // Terminate

    unsigned int          tag;     // The 'tag' of the operation

} ESAIOOperation;


/* =================================================================== *
 *                                                                     *
 *                        Function Forwards                            *
 *                                                                     *
 * =================================================================== */

static void* esaio_completion_main(void* threadDataP);
static BOOLEAN_T  esaio_completion_terminate(ESAIOThreadData* dataP,
                                             ESAIOOperation*  opP);
static BOOLEAN_T esaio_completion_unknown(ESAIOThreadData* dataP,
                                          ESockDescriptor* descP,
                                          ESAIOOperation*  opP,
                                          DWORD            numBytes,
                                          int              error);
static void esaio_completion_inc(ESAIOThreadData* dataP);

static int esaio_add_socket(ESockDescriptor* descP);

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

    ctrl.dbg        = dataP->dbg;
    ctrl.sockDbg    = dataP->sockDbg;

    SGDBG( ("WIN-ESAIO", "esaio_init -> entry\r\n") );

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
    ERL_NIF_TERM info;
    ERL_NIF_TERM keys[]  = {esock_atom_name};
    ERL_NIF_TERM vals[]  = {MKA(env, "win_esaio")};
    unsigned int numKeys = NUM(keys);
    unsigned int numVals = NUM(vals);

    ESOCK_ASSERT( numKeys == numVals );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );

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
        esock_dealloc_descriptor(env, descP);
        return esock_make_error_errno(env, save_errno);
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



/* ========================================================================
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

    descP->readState |= ESOCK_STATE_BOUND;

    return esock_atom_ok;
}


/* *******************************************************************
 * Clsoe a socket
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

    /* +++++++ Current and waiting Writers +++++++ */

    if (descP->currentWriterP != NULL) {

        /* We have a current Writer;
         *
         * The current Writer will not get a select message
         * - send it an abort message
         */

        esock_stop_handle_current(env,
                                  "writer",
                                  descP, sockRef, &descP->currentWriter);

        /* Inform the waiting Writers (in the same way) */

        SSDBG( descP,
               ("SOCKET",
                "esock_do_stop {%d} -> handle waiting writer(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "writer",
                                   descP, sockRef, &descP->writersQ,
                                   esock_atom_closed);

        descP->currentWriterP = NULL;
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

    if (descP->currentReaderP != NULL) {

        /* We have a current Reader; was it deselected?
         *
         * The current Reader will not get a select message
         * - send it an abort message
         */

        esock_stop_handle_current(env,
                                  "reader",
                                  descP, sockRef, &descP->currentReader);

        /* Inform the Readers (in the same way) */

        SSDBG( descP,
               ("SOCKET",
                "esock_do_stop {%d} -> handle waiting reader(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "writer",
                                   descP, sockRef, &descP->readersQ,
                                   esock_atom_closed);

        descP->currentReaderP = NULL;
    }

    /* +++++++ Current and waiting Acceptors +++++++
     *
     * Note that there should not be Readers and Acceptors
     * at the same time so the check for if the
     * current Reader/Acceptor was deselected is only correct
     * under that assumption
     */

    if (descP->currentAcceptorP != NULL) {

        /* We have a current Acceptor;
         *
         * The current Acceptor will not get a select message
         * - send it an abort message
         */

        esock_stop_handle_current(env,
                                  "acceptor",
                                  descP, sockRef, &descP->currentAcceptor);

        /* Inform the waiting Acceptor (in the same way) */

        SSDBG( descP,
               ("SOCKET",
                "esock_do_stop {%d} -> handle waiting acceptors(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "acceptor",
                                   descP, sockRef, &descP->acceptorsQ,
                                   esock_atom_closed);

        descP->currentAcceptorP = NULL;
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

                save_errno = WSAGetLastError(); // Details

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


static
BOOLEAN_T  esaio_completion_terminate(ESAIOThreadData* dataP,
                                      ESAIOOperation*  opP)
{
    (void) opP;

    dataP->state = ESAIO_THREAD_STATE_TERMINATING;
    dataP->error = ESAIO_THREAD_ERROR_CMD;

    return TRUE;
}

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
