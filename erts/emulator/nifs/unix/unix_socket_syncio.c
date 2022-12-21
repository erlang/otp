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
 *  Purpose : UNIX version of synchronous I/O backend.
 * ----------------------------------------------------------------------
 *
 * essio = ESock Synchronous I/O
 *
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"

#include "prim_socket_int.h"
#include "socket_util.h"
#include "socket_io.h"
#include "socket_syncio.h"


/* ======================================================================== *
 *                               Socket wrappers                            *
 * ======================================================================== *
 */

#define sock_errno()                    errno
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_peer(s, addr, len)         getpeername((s), (addr), (len))


/* ======================================================================== *
 *                               Function Forwards                          *
 * ======================================================================== *
 */
static BOOLEAN_T open_is_debug(ErlNifEnv*   env,
                               ERL_NIF_TERM eopts,
                               BOOLEAN_T    def);
static BOOLEAN_T open_use_registry(ErlNifEnv*   env,
                                   ERL_NIF_TERM eopts,
                                   BOOLEAN_T    def);
static BOOLEAN_T open_todup(ErlNifEnv*   env,
                            ERL_NIF_TERM eopts);
static BOOLEAN_T open_which_domain(SOCKET sock,   int* domain);
static BOOLEAN_T open_which_type(SOCKET sock,     int* type);
static BOOLEAN_T open_which_protocol(SOCKET sock, int* proto);
static BOOLEAN_T open_get_domain(ErlNifEnv*   env,
                                 ERL_NIF_TERM eopts,
                                 int*         domain);
static BOOLEAN_T open_get_type(ErlNifEnv*   env,
                               ERL_NIF_TERM eopts,
                               int*         type);
static BOOLEAN_T open_get_protocol(ErlNifEnv*   env,
                                   ERL_NIF_TERM eopts,
                                   int*         protocol);

/*
 * For "standard" (unix) synchronous I/O, this is just a dummy function.
 */
extern
int essio_init(unsigned int numThreads)
{
    VOID(numThreads);

    return ESOCK_IO_OK;
}


/*
 * For "standard" (unix) synchronous I/O, this is just a dummy function.
 * Also, will we ever call this?
 */
extern
void essio_finish(void)
{
    return;
}


/* ========================================================================
 * essio_open - create an endpoint (from an existing fd) for communication
 *
 * Assumes the input has been validated.
 *
 * Normally we want debugging on (individual) sockets to be controlled
 * by the sockets own debug flag. But since we don't even have a socket
 * yet, we must use the global debug flag.
 */
extern
ERL_NIF_TERM essio_open2(ErlNifEnv*       env,
                         int              fd,
                         ERL_NIF_TERM     eopts,
                         const ESockData* dataP)
{
    BOOLEAN_T        dbg    = open_is_debug(env, eopts, dataP->sockDbg);
    BOOLEAN_T        useReg = open_use_registry(env, eopts, dataP->useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              domain, type, protocol;
    int              save_errno = 0;
    BOOLEAN_T        closeOnClose;
    SOCKET           sock;
    ErlNifEvent      event;
    ErlNifPid        self;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("UNIX-SOCKET", "essio_open2 -> entry with"
             "\r\n   fd:    %d"
             "\r\n   eopts: %T"
             "\r\n", fd, eopts) );

    /*
     * Before we do anything else, we try to retrieve domain, type and protocol
     * This information is either present in the eopts map or if not we need
     * to "get" it from the system (getsockopt).
     * Note that its not possible to get all of these on all platforms,
     * and in those cases the user *must* provide us with them (eopts).
     *
     * We try the system first (since its more reliable) and if that fails
     * we check the eopts map. If neither one works, we *give up*!
     */

    if (! open_which_domain(fd, &domain)) {
        SSDBG2( dbg,
                ("UNIX-SOCKET",
                 "essio_open2 -> failed get domain from system\r\n") );

        if (! open_get_domain(env, eopts, &domain)) {
            return esock_make_invalid(env, esock_atom_domain);
        }
    }

    if (! open_which_type(fd, &type)) {
        SSDBG2( dbg,
                ("UNIX-SOCKET",
                 "essio_open2 -> failed get type from system\r\n") );

        if (! open_get_type(env, eopts, &type))
            return esock_make_invalid(env, esock_atom_type);
    }

    if (! open_which_protocol(fd, &protocol)) {
        SSDBG2( dbg,
                ("UNIX-SOCKET",
                 "essio_open2 -> failed get protocol from system\r\n") );

        if (! open_get_protocol(env, eopts, &protocol)) {
            SSDBG2( dbg,
                    ("UNIX-SOCKET",
                     "essio_open2 -> "
                     "failed get protocol => try protocol 0\r\n") );
            protocol = 0;
        }
    }


    SSDBG2( dbg,
            ("UNIX-SOCKET", "essio_open2 -> "
             "\r\n   domain:   %d"
             "\r\n   type:     %d"
             "\r\n   protocol: %d"
             "\r\n", domain, type, protocol) );


    if (open_todup(env, eopts)) {
        /* We shall dup the socket */
        if (ESOCK_IS_ERROR(sock = dup(fd))) {
            save_errno = sock_errno();

            SSDBG2( dbg,
                    ("UNIX-SOCKET",
                     "essio_open2 -> dup failed: %d\r\n",
                     save_errno) );

            return esock_make_error_errno(env, save_errno);
        }
        closeOnClose = TRUE;
    } else {
        sock         = fd;
        closeOnClose = FALSE;
    }

    event = sock;

    SET_NONBLOCKING(sock);

    /* Create and initiate the socket "descriptor" */
    descP               = esock_alloc_descriptor(sock, event);
    descP->ctrlPid      = self;
    descP->domain       = domain;
    descP->type         = type;
    descP->protocol     = protocol;
    descP->closeOnClose = closeOnClose;
    descP->origFD       = fd;

    /* Check if we are already connected, if so change state */
    {
        ESockAddress remote;
        SOCKLEN_T    addrLen = sizeof(remote);
        sys_memzero((char *) &remote, addrLen);
        if (sock_peer(descP->sock,
                      (struct sockaddr*) &remote,
                      &addrLen) == 0) {
            SSDBG2( dbg, ("UNIX-SOCKET", "essio_open2 -> connected\r\n") );
            descP->writeState |= ESOCK_STATE_CONNECTED;
        } else {
            SSDBG2( dbg, ("UNIX-SOCKET", "essio_open2 -> not connected\r\n") );
        }
    }

    /* And create the 'socket' resource */
    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    ESOCK_ASSERT( MONP("essio_open2 -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    esock_inc_socket(domain, type, protocol);

    /* And finally (maybe) update the registry.
     * Shall we keep track of the fact that this socket is created elsewhere?
     */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    SSDBG2( dbg,
            ("UNIX-SOCKET", "essio_open2 -> done: %T\r\n", sockRef) );

    return esock_make_ok2(env, sockRef);
}


static
BOOLEAN_T open_is_debug(ErlNifEnv*   env,
                        ERL_NIF_TERM eopts,
                        BOOLEAN_T    def)
{
    return esock_get_bool_from_map(env, eopts, esock_atom_debug, def);
}

static
BOOLEAN_T open_use_registry(ErlNifEnv*   env,
                            ERL_NIF_TERM eopts,
                            BOOLEAN_T    def)
{
    return esock_get_bool_from_map(env, eopts, esock_atom_use_registry, def);
}


static
BOOLEAN_T open_which_domain(SOCKET sock, int* domain)
{
#if defined(SO_DOMAIN)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_DOMAIN, domain))
        return TRUE;
#endif
    return FALSE;
}

/* The eopts contains an integer 'domain' key.
 */
static
BOOLEAN_T open_get_domain(ErlNifEnv*   env,
                          ERL_NIF_TERM eopts,
                          int*         domain)
{
    ERL_NIF_TERM edomain;
    
    if (!GET_MAP_VAL(env, eopts,
		     esock_atom_domain, &edomain))
      return FALSE;

    if (esock_decode_domain(env, edomain, domain) == 0)
      return FALSE;

    return TRUE;
}

static
BOOLEAN_T open_which_type(SOCKET sock, int* type)
{
#if defined(SO_TYPE)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_TYPE, type))
        return TRUE;
#endif
    return FALSE;
}

/* The eopts contains an integer 'type' key.
 */
static
BOOLEAN_T open_get_type(ErlNifEnv*   env,
                        ERL_NIF_TERM eopts,
                        int*         type)
{
    ERL_NIF_TERM etype;

    if (! GET_MAP_VAL(env, eopts, esock_atom_type, &etype))
        return FALSE;

    if (! esock_decode_type(env, etype, type))
        return FALSE;

    return TRUE;
}

static
BOOLEAN_T open_which_protocol(SOCKET sock, int* proto)
{
#if defined(SO_PROTOCOL)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_PROTOCOL, proto))
        return TRUE;
#endif
    return FALSE;
}

/* The eopts contains an integer 'type' key.
 */
static
BOOLEAN_T open_get_protocol(ErlNifEnv*   env,
                            ERL_NIF_TERM eopts,
                            int*         protocol)
{
    return esock_extract_int_from_map(env, eopts,
                                      esock_atom_protocol, protocol);
}


/* The eopts contains a boolean 'dup' key. Defaults to TRUE.
 */
static
BOOLEAN_T open_todup(ErlNifEnv* env, ERL_NIF_TERM eopts)
{
    return esock_get_bool_from_map(env, eopts, esock_atom_dup, TRUE);
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_open4(ErlNifEnv*       env,
                         int              domain,
                         int              type,
                         int              protocol,
                         ERL_NIF_TERM     eopts,
                         const ESockData* dataP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_bind(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ESockAddress*    sockAddrP,
                        SOCKLEN_T        addrLen)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/*  ========================================================================
 */
extern
ERL_NIF_TERM essio_connect(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     connRef,
                           ESockAddress*    addrP,
                           SOCKLEN_T        addrLen)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_listen(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              backlog)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_accept(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     accRef)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_send(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendto(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     sendRef,
                          ErlNifBinary*    dataP,
                          int              flags,
                          ESockAddress*    toAddrP,
                          SOCKLEN_T        toAddrLen)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     sendRef,
                           ERL_NIF_TERM     eMsg,
                           int              flags,
                           ERL_NIF_TERM     eIOV)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendfile(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            off_t            offset,
                            size_t*          countP,
                            int*             errP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_recv(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_recvfrom(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            ssize_t          len,
                            int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_recvmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef,
                           ssize_t          bufLen,
                           ssize_t          ctrlLen,
                           int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_close(ErlNifEnv*       env,
                         ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_fin_close(ErlNifEnv*       env,
                             ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_shutdown(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              how)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sockname(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_peername(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}
