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

#ifdef HAS_ACCEPT4
// We have to figure out what the flags are...
#define sock_accept(s, addr, len)       \
    accept4((s), (addr), (len), (SOCK_CLOEXEC))
#else
#define sock_accept(s, addr, len)       accept((s), (addr), (len))
#endif
#define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_connect(s, addr, len)      connect((s), (addr), (len))
#define sock_errno()                    errno
#define sock_listen(s, b)               listen((s), (b))
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

#ifdef HAVE_SETNS
static BOOLEAN_T open_get_netns(ErlNifEnv*   env,
                                ERL_NIF_TERM opts,
                                char**       netns);
static BOOLEAN_T change_network_namespace(BOOLEAN_T dbg,
                                          char* netns, int* cns, int* err);
static BOOLEAN_T restore_network_namespace(BOOLEAN_T dbg,
                                           int ns, SOCKET sock, int* err);
#endif

static BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err);

static ERL_NIF_TERM esock_accept_listening_error(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     sockRef,
                                                 ERL_NIF_TERM     accRef,
                                                 ErlNifPid        caller,
                                                 int              save_errno);
static ERL_NIF_TERM esock_accept_listening_accept(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  SOCKET           accSock,
                                                  ErlNifPid        caller);
static ERL_NIF_TERM esock_accept_accepting_current(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   ERL_NIF_TERM     ref);
static
ERL_NIF_TERM esock_accept_accepting_current_accept(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   SOCKET           accSock);
static
ERL_NIF_TERM esock_accept_accepting_current_error(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  ERL_NIF_TERM     opRef,
                                                  int              save_errno);
static ERL_NIF_TERM esock_accept_accepting_other(ErlNifEnv*       env,
						 ESockDescriptor* descP,
						 ERL_NIF_TERM     ref,
						 ErlNifPid        caller);
static ERL_NIF_TERM esock_accept_busy_retry(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef,
                                            ErlNifPid*       pidP);
static BOOLEAN_T esock_accept_accepted(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       SOCKET           accSock,
                                       ErlNifPid        pid,
                                       ERL_NIF_TERM*    result);


/* ======================================================================== *
 *                              ESSIO Functions                             *
 * ======================================================================== *
 */

/*
 * For "standard" (unix) synchronous I/O, in our case
 * this is just a dummy function.
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
            ("UNIX-ESSIO", "essio_open2 -> entry with"
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
                ("UNIX-ESSIO",
                 "essio_open2 -> failed get domain from system\r\n") );

        if (! open_get_domain(env, eopts, &domain)) {
            return esock_make_invalid(env, esock_atom_domain);
        }
    }

    if (! open_which_type(fd, &type)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO",
                 "essio_open2 -> failed get type from system\r\n") );

        if (! open_get_type(env, eopts, &type))
            return esock_make_invalid(env, esock_atom_type);
    }

    if (! open_which_protocol(fd, &protocol)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO",
                 "essio_open2 -> failed get protocol from system\r\n") );

        if (! open_get_protocol(env, eopts, &protocol)) {
            SSDBG2( dbg,
                    ("UNIX-ESSIO",
                     "essio_open2 -> "
                     "failed get protocol => try protocol 0\r\n") );
            protocol = 0;
        }
    }


    SSDBG2( dbg,
            ("UNIX-ESSIO", "essio_open2 -> "
             "\r\n   domain:   %d"
             "\r\n   type:     %d"
             "\r\n   protocol: %d"
             "\r\n", domain, type, protocol) );


    if (open_todup(env, eopts)) {
        /* We shall dup the socket */
        if (ESOCK_IS_ERROR(sock = dup(fd))) {
            save_errno = sock_errno();

            SSDBG2( dbg,
                    ("UNIX-ESSIO",
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
            SSDBG2( dbg, ("UNIX-ESSIO", "essio_open2 -> connected\r\n") );
            descP->writeState |= ESOCK_STATE_CONNECTED;
        } else {
            SSDBG2( dbg, ("UNIX-ESSIO", "essio_open2 -> not connected\r\n") );
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
            ("UNIX-ESSIO", "essio_open2 -> done: %T\r\n", sockRef) );

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
    BOOLEAN_T        dbg    = open_is_debug(env, eopts, dataP->sockDbg);
    BOOLEAN_T        useReg = open_use_registry(env, eopts, dataP->useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              proto = protocol, save_errno;
    SOCKET           sock;
    char*            netns;
#ifdef HAVE_SETNS
    int              current_ns = 0;
#endif
    ErlNifPid        self;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("UNIX-ESSIO", "essio_open4 -> entry with"
             "\r\n   domain:   %d"
             "\r\n   type:     %d"
             "\r\n   protocol: %d"
             "\r\n   eopts:    %T"
             "\r\n", domain, type, protocol, eopts) );


#ifdef HAVE_SETNS
    if (open_get_netns(env, eopts, &netns)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO", "essio_open4 -> namespace: %s\r\n", netns) );
    }
#else
    netns = NULL;
#endif


#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        (! change_network_namespace(dbg,
                                    netns, &current_ns, &save_errno))) {
        FREE(netns);
        return esock_make_error_errno(env, save_errno);
    }
#endif

    if (ESOCK_IS_ERROR(sock = sock_open(domain, type, proto))) {
        if (netns != NULL) FREE(netns);
        return esock_make_error_errno(env, sock_errno());
    }

    SSDBG2( dbg, ("UNIX-ESSIO", "essio_open4 -> open success: %d\r\n", sock) );


    /* NOTE that if the protocol = 0 (default) and the domain is not
     * local (AF_LOCAL) we need to explicitly get the protocol here!
     */
    
    if (proto == 0)
        (void) open_which_protocol(sock, &proto);

#ifdef HAVE_SETNS
    if (netns != NULL) {
        FREE(netns);
        if (! restore_network_namespace(dbg,
                                        current_ns, sock, &save_errno))
            return esock_make_error_errno(env, save_errno);
    }
#endif

    SET_NONBLOCKING(sock);


    /* Create and initiate the socket "descriptor" */
    descP           = esock_alloc_descriptor(sock, sock);
    descP->ctrlPid  = self;
    descP->domain   = domain;
    descP->type     = type;
    descP->protocol = proto;

    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    ESOCK_ASSERT( MONP("esock_open -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    esock_inc_socket(domain, type, protocol);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    return esock_make_ok2(env, sockRef);
}


#ifdef HAVE_SETNS
/* open_get_netns - extract the netns field from the opts map
 */
static
BOOLEAN_T open_get_netns(ErlNifEnv* env, ERL_NIF_TERM opts, char** netns)
{
    ERL_NIF_TERM val;
    ErlNifBinary bin;
    char*        buf;

    /* The currently only supported extra option is: netns */
    if (!GET_MAP_VAL(env, opts, esock_atom_netns, &val)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    /* The value should be a binary file name */
    if (! enif_inspect_binary(env, val, &bin)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    ESOCK_ASSERT( (buf = MALLOC(bin.size+1)) != NULL );

    sys_memcpy(buf, bin.data, bin.size);
    buf[bin.size] = '\0';
    *netns = buf;

    return TRUE;
}


/* We should really have another API, so that we can return errno... */

/* *** change network namespace ***
 * Retrieve the current namespace and set the new.
 * Return result and previous namespace if successful.
 */
static
BOOLEAN_T change_network_namespace(BOOLEAN_T dbg,
                                   char* netns, int* cns, int* err)
{
    int save_errno;
    int current_ns = 0;
    int new_ns     = 0;

    SSDBG2( dbg,
            ("UNIX-ESSIO", "change_network_namespace -> entry with"
             "\r\n   new ns: %s"
             "\r\n", netns) );

    current_ns = open("/proc/self/ns/net", O_RDONLY);
    if (ESOCK_IS_ERROR(current_ns)) {
        *err = sock_errno();
        return FALSE;
    }
    new_ns = open(netns, O_RDONLY);
    if (ESOCK_IS_ERROR(new_ns)) {
        save_errno = sock_errno();
        (void) close(current_ns);
        *err = save_errno;
        return FALSE;
    }
    if (setns(new_ns, CLONE_NEWNET) != 0) {
        save_errno = sock_errno();
        (void) close(new_ns);
        (void) close(current_ns);
        *err = save_errno;
        return FALSE;
    } else {
        (void) close(new_ns);
        *cns = current_ns;
        return TRUE;
    }
}


/* *** restore network namespace ***
 * Restore the previous namespace (see above).
 */
static
BOOLEAN_T restore_network_namespace(BOOLEAN_T dbg,
                                    int ns, SOCKET sock, int* err)
{
    SSDBG2( dbg,
            ("UNIX-ESSIO", "restore_network_namespace -> entry with"
             "\r\n   ns: %d"
             "\r\n", ns) );

    if (setns(ns, CLONE_NEWNET) != 0) {
        /* XXX Failed to restore network namespace.
         * What to do? Tidy up and return an error...
         * Note that the thread now might still be in the namespace.
         * Can this even happen? Should the emulator be aborted?
         */
        int save_errno = sock_errno();
        (void) close(sock);
        (void) close(ns);
        *err = save_errno;
        return FALSE;
    } else {
        (void) close(ns);
        return TRUE;
    }
}

#endif



/* ========================================================================
 */
extern
ERL_NIF_TERM essio_bind(ErlNifEnv*       env,
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
    int       save_errno;
    ErlNifPid self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    /*
     * Verify that we are in the proper state
     */

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentWriterP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    if (descP->connectorP != NULL) {
        /* Connect in progress */

        if (COMPARE_PIDS(&self, &descP->connector.pid) != 0) {
	    /* Other process has connect in progress */
	    if (addrP != NULL) {
                return esock_make_error(env, esock_atom_already);
	    } else {
	        /* This is a bad call sequence
		 * - connect without an address is only allowed
		 *   for the connecting process
		 */
	        return esock_raise_invalid(env, esock_atom_state);
	    }
        }

        /* Finalize after received select message */

        esock_requestor_release("essio_connect finalize -> connected",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;

        if (! verify_is_connected(descP, &save_errno)) {
            return esock_make_error_errno(env, save_errno);
        }

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* No connect in progress */

    if (addrP == NULL)
        /* This is a bad call sequence
         * - connect without an address is only allowed when
         *   a connect is in progress, after getting the select message
         */
        return esock_raise_invalid(env, esock_atom_state);

    /* Initial connect call, with address */

    if (sock_connect(descP->sock, (struct sockaddr*) addrP, addrLen) == 0) {
        /* Success already! */
        SSDBG( descP, ("UNIX-ESSIO", "essio_connect {%d} -> connected\r\n",
                       descP->sock) );

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* Connect returned error */
    save_errno = sock_errno();

    switch (save_errno) {

    case EINPROGRESS:   /* Unix & OSE!!        */
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_connect {%d} -> would block => select\r\n",
                descP->sock) );
        {
            int sres;

            if ((sres =
                 esock_select_write(env, descP->sock, descP, NULL,
                                    sockRef, connRef)) < 0)
                return
                    enif_raise_exception(env,
                                         MKT2(env, esock_atom_select_write,
                                              MKI(env, sres)));
            /* Initiate connector */
            descP->connector.pid = self;
            ESOCK_ASSERT( MONP("esock_connect -> conn",
                               env, descP,
                               &self, &descP->connector.mon) == 0 );
            descP->connector.env = esock_alloc_env("connector");
            descP->connector.ref = CP_TERM(descP->connector.env, connRef);
            descP->connectorP = &descP->connector;
            descP->writeState |=
                (ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

            return esock_atom_select;
        }
        break;

    default:
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_connect {%d} -> error: %d\r\n",
                descP->sock, save_errno) );

        return esock_make_error_errno(env, save_errno);

    } // switch(save_errno)
}


/* *** verify_is_connected ***
 * Check if a connection has been established.
 */
static
BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err)
{
    /*
     * *** This is strange ***
     *
     * This *should* work on Windows NT too, but doesn't.
     * An bug in Winsock 2.0 for Windows NT?
     *
     * See "Unix Netwok Programming", "The Sockets Networking API",
     * W.R.Stevens, Volume 1, third edition, 16.4 Nonblocking 'connect',
     * before Interrupted 'connect' (p 412) for a discussion about
     * Unix portability and non blocking connect.
     */

    int error = 0;

#ifdef SO_ERROR
    if (! esock_getopt_int(descP->sock, SOL_SOCKET, SO_ERROR, &error)) {
        // Solaris does it this way according to W.R.Stevens
        error = sock_errno();
    }
#elif 1
    char buf[0];
    if (ESOCK_IS_ERROR(read(descP->sock, buf, sizeof(buf)))) {
        error = sock_errno();
    }
#else
    /* This variant probably returns wrong error value
     * ENOTCONN instead of the actual connect error
     */
    ESockAddress remote;
    SOCKLEN_T    addrLen = sizeof(remote);
    sys_memzero((char *) &remote, addrLen);
    if (sock_peer(descP->sock,
                  (struct sockaddr*) &remote, &addrLen)) < 0) {
        error = sock_errno();
    }
#endif

    if (error != 0) {
        *err = error;
        return FALSE;
    }
    return TRUE;
}



/* ========================================================================
 */
extern
ERL_NIF_TERM essio_listen(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              backlog)
{
    
    /*
     * Verify that we are in the proper state
     */

    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    /*
     * And attempt to make socket listening
     */
    
    if ((sock_listen(descP->sock, backlog)) < 0)
        return esock_make_error_errno(env, sock_errno());

    descP->readState |= ESOCK_STATE_LISTENING;

    return esock_atom_ok;

}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_accept(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     accRef)
{
    ErlNifPid     caller;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentReaderP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    if (descP->currentAcceptorP == NULL) {
        SOCKET        accSock;

        /* We have no active acceptor (and therefore no acceptors in queue)
         */

        SSDBG( descP, ("SOCKET", "esock_accept {%d} -> try accept\r\n",
                       descP->sock) );

	ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_tries, &descP->accTries, 1);

        accSock = sock_accept(descP->sock, NULL, NULL);

        if (ESOCK_IS_ERROR(accSock)) {
            int           save_errno;

            save_errno = sock_errno();

            return esock_accept_listening_error(env, descP, sockRef,
                                                accRef, caller, save_errno);
        } else {
            /* We got an incoming connection */
            return esock_accept_listening_accept(env, descP, sockRef,
                                                 accSock, caller);
        }
    } else {

        /* We have an active acceptor and possibly acceptors waiting in queue.
         * If the pid of the calling process is not the pid of the
	 * "current process", push the requester onto the (acceptor) queue.
         */

        SSDBG( descP, ("SOCKET", "esock_accept_accepting -> check: "
                       "is caller current acceptor:"
                       "\r\n   Caller:      %T"
                       "\r\n   Current:     %T"
                       "\r\n   Current Mon: %T"
                       "\r\n",
                       caller,
                       descP->currentAcceptor.pid,
                       ESOCK_MON2TERM(env, &descP->currentAcceptor.mon)) );

        if (COMPARE_PIDS(&descP->currentAcceptor.pid, &caller) == 0) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting {%d} -> current acceptor"
                    "\r\n", descP->sock) );

            return esock_accept_accepting_current(env, descP, sockRef, accRef);

        } else {

            /* Not the "current acceptor", so (maybe) push onto queue */

            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting {%d} -> *not* current acceptor\r\n",
                    descP->sock) );

            return esock_accept_accepting_other(env, descP, accRef, caller);
        }
    }
}


/* *** esock_accept_listening_error ***
 *
 * The accept call resultet in an error - handle it.
 * There are only two cases:
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
static
ERL_NIF_TERM esock_accept_listening_error(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     accRef,
                                          ErlNifPid        caller,
                                          int              save_errno)
{
    ERL_NIF_TERM res;

    if (save_errno == ERRNO_BLOCK ||
        save_errno == EAGAIN) {

        /* *** Try again later *** */

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_listening_error {%d} -> would block - retry\r\n",
                descP->sock) );

	descP->currentAcceptor.pid = caller;
        ESOCK_ASSERT( MONP("esock_accept_listening -> current acceptor",
                           env, descP,
                           &descP->currentAcceptor.pid,
                           &descP->currentAcceptor.mon) == 0 );
        ESOCK_ASSERT( descP->currentAcceptor.env == NULL );
        descP->currentAcceptor.env = esock_alloc_env("current acceptor");
        descP->currentAcceptor.ref =
            CP_TERM(descP->currentAcceptor.env, accRef);
        descP->currentAcceptorP = &descP->currentAcceptor;

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_listening_error {%d} -> retry for: "
                "\r\n   Current Pid: %T"
                "\r\n   Current Mon: %T"
                "\r\n",
                descP->sock,
                descP->currentAcceptor.pid,
                ESOCK_MON2TERM(env, &descP->currentAcceptor.mon)) );

        res = esock_accept_busy_retry(env, descP, sockRef, accRef, NULL);

    } else {

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_listening {%d} -> errno: %d\r\n",
                descP->sock, save_errno) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_fails, &descP->accFails, 1);

        res = esock_make_error_errno(env, save_errno);
    }

    return res;
}


/* *** esock_accept_listening_accept ***
 *
 * The accept call was successful (accepted) - handle the new connection.
 */
static
ERL_NIF_TERM esock_accept_listening_accept(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef,
                                           SOCKET           accSock,
                                           ErlNifPid        caller)
{
    ERL_NIF_TERM res;

    esock_accept_accepted(env, descP, sockRef, accSock, caller, &res);

    return res;
}


/* *** esock_accept_accepting_current ***
 * Handles when the current acceptor makes another attempt.
 */
static
ERL_NIF_TERM esock_accept_accepting_current(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef)
{
    SOCKET        accSock;
    int           save_errno;
    ERL_NIF_TERM  res;

    SSDBG( descP,
           ("SOCKET",
            "esock_accept_accepting_current {%d} -> try accept\r\n",
            descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_tries, &descP->accTries, 1);
	
    accSock = sock_accept(descP->sock, NULL, NULL);

    if (ESOCK_IS_ERROR(accSock)) {

        save_errno = sock_errno();

        res = esock_accept_accepting_current_error(env, descP, sockRef,
                                                   accRef, save_errno);
    } else {

        res = esock_accept_accepting_current_accept(env, descP, sockRef,
                                                    accSock);
    }

    return res;
}


/* *** esock_accept_accepting_current_accept ***
 *
 * Handles when the current acceptor succeeded in its accept call -
 * handle the new connection.
 */
static
ERL_NIF_TERM esock_accept_accepting_current_accept(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   SOCKET           accSock)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET",
            "esock_accept_accepting_current_accept {%d}"
            "\r\n", descP->sock) );

    if (esock_accept_accepted(env, descP, sockRef, accSock,
                              descP->currentAcceptor.pid, &res)) {

        ESOCK_ASSERT( DEMONP("esock_accept_accepting_current_accept -> "
                             "current acceptor",
                             env, descP, &descP->currentAcceptor.mon) == 0);

        MON_INIT(&descP->currentAcceptor.mon);

        if (!esock_activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting_current_accept {%d} ->"
                    " no more acceptors"
                    "\r\n", descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

    }

    return res;
}


/* *** esock_accept_accepting_current_error ***
 * The accept call of current acceptor resultet in an error - handle it.
 * There are only two cases:
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
static
ERL_NIF_TERM esock_accept_accepting_current_error(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  ERL_NIF_TERM     opRef,
                                                  int              save_errno)
{
    ERL_NIF_TERM   res, reason;

    if (save_errno == ERRNO_BLOCK ||
        save_errno == EAGAIN) {

        /*
         * Just try again, no real error, just a ghost trigger from poll,
         */

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_accepting_current_error {%d} -> "
                "would block: try again\r\n", descP->sock) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_waits, &descP->accWaits, 1);

        res = esock_accept_busy_retry(env, descP, sockRef, opRef,
                                      &descP->currentAcceptor.pid);

    } else {
        ESockRequestor req;

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_accepting_current_error {%d} -> "
                "error: %d\r\n", descP->sock, save_errno) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_fails, &descP->accFails, 1);

        esock_requestor_release("esock_accept_accepting_current_error",
                                env, descP, &descP->currentAcceptor);

        reason = MKA(env, erl_errno_id(save_errno));
        res    = esock_make_error(env, reason);

        req.env = NULL;
        while (esock_acceptor_pop(env, descP, &req)) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting_current_error {%d} -> abort %T\r\n",
                    descP->sock, req.pid) );

            esock_send_abort_msg(env, descP, sockRef, &req, reason);

            (void) DEMONP("esock_accept_accepting_current_error -> "
                          "pop'ed writer",
                          env, descP, &req.mon);
        }
        descP->currentAcceptorP = NULL;
    }

    return res;
}


/* *** esock_accept_accepting_other ***
 * Handles when the another acceptor makes an attempt, which
 * results (maybe) in the request being pushed onto the
 * acceptor queue.
 */
ERL_NIF_TERM
esock_accept_accepting_other(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     ref,
                             ErlNifPid        caller)
{
    if (! esock_acceptor_search4pid(env, descP, &caller)) {
        esock_acceptor_push(env, descP, caller, ref);
	return esock_atom_select;
    } else {
        /* Acceptor already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }
}


/* *** esock_accept_busy_retry ***
 *
 * Perform a retry select. If successful, set nextState.
 */
static
ERL_NIF_TERM esock_accept_busy_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     accRef,
                                     ErlNifPid*       pidP)
{
    int          sres;
    ERL_NIF_TERM res;

    if ((sres = esock_select_read(env, descP->sock, descP, pidP,
                                  sockRef, accRef)) < 0) {

        ESOCK_ASSERT( DEMONP("esock_accept_busy_retry - select failed",
                             env, descP, &descP->currentAcceptor.mon) == 0);

        MON_INIT(&descP->currentAcceptor.mon);

        /* It is very unlikely that a next acceptor will be able
         * to do anything successful, but we will clean the queue
         */
        
        if (!esock_activate_next_acceptor(env, descP, sockRef)) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_busy_retry {%d} -> no more acceptors\r\n",
                    descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

        res =
            enif_raise_exception(env,
                                 MKT2(env, esock_atom_select_read,
                                      MKI(env, sres)));
    } else {
        descP->readState |=
            (ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);
        res = esock_atom_select;
    }

    return res;
}


/* *** esock_accept_accepted ***
 *
 * Generic function handling a successful accept.
 */
static
BOOLEAN_T esock_accept_accepted(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                SOCKET           accSock,
                                ErlNifPid        pid,
                                ERL_NIF_TERM*    result)
{
    ESockDescriptor* accDescP;
    ERL_NIF_TERM     accRef;

    /*
     * We got one
     */

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_success, &descP->accSuccess, 1);

    accDescP           = esock_alloc_descriptor(accSock, accSock);
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
    ESOCK_ASSERT( MONP("esock_accept_accepted -> ctrl",
                       env, accDescP,
                       &accDescP->ctrlPid,
                       &accDescP->ctrlMon) == 0 );

    SET_NONBLOCKING(accDescP->sock);

    descP->writeState |= ESOCK_STATE_CONNECTED;

    MUNLOCK(descP->writeMtx);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, accRef);

    *result = esock_make_ok2(env, accRef);

    return TRUE;
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
