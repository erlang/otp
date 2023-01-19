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
 *   * Connectex:
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


/* ======================================================================== *
 *                               Socket wrappers                            *
 * ======================================================================== *
 */

#define sock_close(s)                   closesocket((s))
#define sock_errno()                    WSAGetLastError()
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))


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


/* =================================================================== *
 *                                                                     *
 *                      Local (global) variables                       *
 *                                                                     *
 * =================================================================== */

static ESAIOControl ctrl = {0};



/* =================================================================== *
 *                                                                     *
 *                        Various esock macros                         *
 *                                                                     *
 * =================================================================== */

/* Global socket debug */
#define SGDBG( proto )            ESOCK_DBG_PRINTF( ctrl.dbg , proto )


/* =================================================================== *
 *                                                                     *
 *                         I/O Backend exports                         *
 *                                                                     *
 * =================================================================== */

/* This function is called during (esock) nif loading
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

    ctrl.dbg        = TRUE; // dataP->dbg;
    ctrl.sockDbg    = dataP->sockDbg;

    SGDBG( ("UNIX-ESAIO", "esaio_init -> entry\r\n") );

    /* We should actually check the value of 'numThreads'
     * Since if its zero (the default), we should instead
     * assign: 2 * 'number of schedulers'
     * Or shall we trust the 'prim_socket' preloaded to 
     * select the proper value?
     */
    ctrl.numThreads = (DWORD) numThreads;

    // Initialize Winsock
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try initialize winsock\r\n") );
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
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try create I/O completion port\r\n") );
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
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try create 'dummy' socket\r\n") );
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
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try extract 'accept' function\r\n") );
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
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try extract 'connect' function\r\n") );
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
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try alloc thread pool memory\r\n") );
    ctrl.threads = MALLOC(numThreads * sizeof(ESAIOThread));
    ESOCK_ASSERT( ctrl.threads != NULL );

    SGDBG( ("UNIX-ESAIO", "esaio_init -> basic init of thread data\r\n") );
    for (i = 0; i < numThreads; i++) {
        ctrl.threads[i].data.id    = i;
        ctrl.threads[i].data.state = ESAIO_THREAD_STATE_UNDEF;
        ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_UNDEF;
        ctrl.threads[i].data.env   = NULL;
        ctrl.threads[i].data.cnt   = 0;
    }

    SGDBG( ("UNIX-ESAIO", "esaio_init -> try create thread(s)\r\n") );
    for (i = 0; i < numThreads; i++) {
        char buf[64]; /* Buffer used for building the names */
        int  j;

        /* We set these here to avoid raise with the thread.
         * *If* we fail in the creation, then we set an error */

        ctrl.threads[i].data.state = ESAIO_THREAD_STATE_INITIATING;
        ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_OK;

        SGDBG( ("UNIX-ESAIO",
                "esaio_init -> try create %d thread opts\r\n", i) );
        sprintf(buf, "esaio-opts[%d]", i);
        ctrl.threads[i].optsP      = TOCREATE(buf);
        if (ctrl.threads[i].optsP == NULL) {
            esock_error_msg("Failed create thread opts %d\r\n");

            ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_TOCREATE;

            for (j = 0; j < i; j++) {
                SGDBG( ("UNIX-ESAIO",
                        "esaio_init -> destroy thread opts %d\r\n", j) );
                TODESTROY(ctrl.threads[j].optsP);
            }
            return ESAIO_ERR_THREAD_OPTS_CREATE;
        }

        SGDBG( ("UNIX-ESAIO",
                "esaio_init -> try create thread %d\r\n", i) );
        sprintf(buf, "esaio[%d]", i);
        if (0 != TCREATE(buf, 
                         &ctrl.threads[i].tid, 
                         esaio_completion_main, 
                         (void*) &ctrl.threads[i].data, 
                         ctrl.threads[i].optsP)) {

            ctrl.threads[i].data.error = ESAIO_THREAD_ERROR_TCREATE;

            for (j = 0; j <= i; j++) {
                SGDBG( ("UNIX-ESAIO",
                        "esaio_init -> destroy thread opts %d\r\n", j) );
                TODESTROY(ctrl.threads[j].optsP);
            }
            return ESAIO_ERR_THREAD_CREATE;
        }

    }


    SGDBG( ("UNIX-ESAIO", "esaio_init -> done\r\n") );

    return ESAIO_OK;
}



/* Issue a "message" via PostQueuedCompletionStatus
 * instructing all (completion) threads to terminate.
 */
extern
void esaio_finish()
{
    unsigned int t;

    SGDBG( ("UNIX-ESAIO", "esaio_finish -> entry\r\n") );

    if (ctrl.dummy != INVALID_SOCKET) {
        SGDBG( ("UNIX-ESAIO", "esaio_finish -> close 'dummy' socket\r\n") );
        (void) sock_close(ctrl.dummy);
        ctrl.dummy = INVALID_SOCKET;
    }

    for (t = 0; t < ctrl.numThreads; t++) {
        ESAIOOperation* opP;

        SGDBG( ("UNIX-ESAIO",
                "esaio_finish -> "
                "[%d] try allocate (terminate-) operation\r\n", t) );

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

            SGDBG( ("UNIX-ESAIO",
                    "esaio_finish -> "
                    "[%d] try post (terminate-) operation\r\n", t) );

            PostQueuedCompletionStatus(ctrl.cport, 0, 0, (OVERLAPPED*) opP);

        } else {

            SGDBG( ("UNIX-ESAIO",
                    "esaio_finish -> "
                    "[%d] failed allocate (terminate-) operation\r\n", t) );

        }
    }

    SGDBG( ("UNIX-ESAIO", "esaio_finish -> invalidate functions\r\n") );
    ctrl.accept  = NULL;
    ctrl.connect = NULL;
    
    SGDBG( ("UNIX-ESAIO", "esaio_finish -> done\r\n") );

    return;
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
 
    SGDBG( ("UNIX-ESAIO",
            "[%d] esaio_completion_main -> entry\r\n", dataP->id) );

    dataP->state = ESAIO_THREAD_STATE_INITIATING;

    sprintf(envName, "esaio-completion-main[%d]", dataP->id);
    dataP->env = esock_alloc_env(envName);

    dataP->state = ESAIO_THREAD_STATE_OPERATIONAL;

    SGDBG( ("UNIX-ESAIO",
            "[%d] esaio_completion_main -> initiated\r\n", dataP->id) );

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

        SGDBG( ("UNIX-ESAIO",
                "[%d] esaio_completion_main -> [%d] try dequeue packet\r\n",
                dataP->id, dataP->cnt) );

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
                   
                SGDBG( ("UNIX-ESAIO",
                        "[%d] esaio_completion_main -> [failure 1]\r\n",
                        dataP->id) );

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

                SGDBG( ("UNIX-ESAIO",
                        "[%d] esaio_completion_main -> [failure 2] "
                        "\r\n   %s (%d)"
                        "\r\n",
                        dataP->id, erl_errno_id(save_errno), save_errno) );

                opP = CONTAINING_RECORD(olP, ESAIOOperation, ol);
                esaio_completion_inc(dataP);

            }
        } else {
            opP = CONTAINING_RECORD(olP, ESAIOOperation, ol);
            esaio_completion_inc(dataP);

            SGDBG( ("UNIX-ESAIO",
                    "[%d] esaio_completion_main -> success\r\n", dataP->id) );

        }

        dataP->latest = opP->tag;

        switch (opP->tag) {
        case ESAIO_OP_TERMINATE:
            SGDBG( ("UNIX-ESAIO",
                    "[%d] esaio_completion_main -> received terminate cmd\r\n",
                    dataP->id) );
            done = esaio_completion_terminate(dataP, opP);
            break;

        default:
            SGDBG( ("UNIX-ESAIO",
                    "[%d] esaio_completion_main -> received unknown cmd: "
                    "\r\n   %d"
                    "\r\n",
                    dataP->id, opP->tag) );
            done = esaio_completion_unknown(dataP, descP, opP, numBytes,
                                            save_errno);
            break;

        }
    }

    SGDBG( ("UNIX-ESAIO",
            "[%d] esaio_completion_main -> terminating\r\n", dataP->id) );

    TEXIT(threadDataP); // What to do here?

    SGDBG( ("UNIX-ESAIO",
            "[%d] esaio_completion_main -> terminated\r\n", dataP->id) );

    dataP->state = ESAIO_THREAD_STATE_TERMINATED;

    SGDBG( ("UNIX-ESAIO",
            "[%d] esaio_completion_main -> done\r\n", dataP->id) );

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

