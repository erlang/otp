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

#define ESAIO_OK                 ESOCK_IO_OK
#define ESAIO_ERR_WINSOCK_INIT   0x0001
#define ESAIO_ERR_IOCPORT_CREATE 0x0002


/* =================================================================== *
 *                                                                     *
 *                            Local types                              *
 *                                                                     *
 * =================================================================== */

typedef struct {
    Uint16     id;    /* Thread id: mainly used for debugging,
                         * and name creation */
    Uint16     st;    /* State of the thread:
                         * undefined, initiating, operational, terminating */

    /* Do we need this?
     * "The environment of the calling thread (process bound or
     *  callback environment) or NULL if calling from a custom
     *  thread not spawned by ERTS."
     */
    ErlNifEnv*   env;   /* Used when sending messages */
    Uint32       cnt;   /* Run-counter: mainly used for debugging */
    Uint32       error; /* In case the thread exits, 
                         * this is where the reason is stored 
                         */
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
    DWORD numThreads;

    /* Misc stuff */
    BOOLEAN_T dbg;
    BOOLEAN_T sockDbg;

} ESAIOControl;



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
    int          ires, saveErrno;
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
        esock_error_msg("Failed initialize winsock: %d", ires);
        return ESAIO_ERR_WINSOCK_INIT;
    }    

    // Create a handle for the completion port
    SGDBG( ("UNIX-ESAIO", "esaio_init -> try create I/O completion port\r\n") );
    ctrl.cport = CreateIoCompletionPort(INVALID_HANDLE_VALUE,
                                        NULL, (u_long) 0, ctrl.numThreads);
    if (ctrl.cport == NULL) {
        DWORD cres = GetLastError();
        
        esock_error_msg("Failed create I/O Completion Port: %d", cres);

        WSACleanup();

        return ESAIO_ERR_IOCPORT_CREATE;
    }

    SGDBG( ("UNIX-ESAIO", "esaio_init -> done\r\n") );

    return ESOCK_IO_OK;
}



/* Issue a "message" via PostQueuedCompletionStatus
 * instructing all (completion) threads to terminate.
 */
extern
void esaio_finish()
{
    SGDBG( ("UNIX-ESAIO", "esaio_finish -> entry\r\n") );

    return;
}

