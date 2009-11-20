/*<copyright>
 * <year>1999-2008</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
/*
 * Purpose: Header file for adaptions to various SSL packages.
 */

#ifndef ESOCK_SSL_H
#define ESOCK_SSL_H

#include <sys/types.h>
#include <stdio.h>
#include "esock.h"

typedef struct {
    const char *compile_version;/* version of OpenSSL when compiling esock */
    const char *lib_version;	/* version of OpenSSL in library */
} esock_version;

/* Variables to be set by certain functions (see below) */
char *esock_ssl_errstr;

/* Ephemeral RSA and DH */
int ephemeral_rsa, ephemeral_dh;

/* Protocol version (sslv2, sslv3, tlsv1) */
int protocol_version;

/* version info */
esock_version *esock_ssl_version(void);

/* ciphers info */
char *esock_ssl_ciphers(void);

/* seeding */
void esock_ssl_seed(void *buf, int len);

/* Initialization and finalization of SSL */

int esock_ssl_init(void);
void esock_ssl_finish(void);

/* Freeing of SSL resources for a connection */

void esock_ssl_free(Connection *cp);

/* Print error diagnostics to a file pointer */

void esock_ssl_print_errors_fp(FILE *fp);

/* All functions below have to return >= 0 on success, and < 0 on 
 * failure. 
 * 
 * If the return indicates a failure (return value < 0) and the failure
 * is temporary the error context (sock_errno()/sock_set_errno()) must
 * be set to ERRNO_BLOCK. 
 *
 * If the failure is permanent, the error context must be set to something
 * else than ERRNO_BLOCK, and `esock_ssl_errstr' must be set to point to
 * short diagnostic string describing the error.
 */

int esock_ssl_accept_init(Connection *cp, void *listenssl);
int esock_ssl_connect_init(Connection *cp);
int esock_ssl_listen_init(Connection *cp);

/* All functions below may involve non-blocking I/O with a temporary
 * failure.  Hence they have to have the error context set to
 * ERRNO_BLOCK, or else have esock_ssl_errstr set to point to a
 * diagnostic string, in case the return value is < 0. If the return
 * value is 0, cp->eof and cp->bp are set, if appropritate.
 */

int esock_ssl_accept(Connection *cp);
int esock_ssl_connect(Connection *cp);

int esock_ssl_read(Connection *cp, char *buf, int len);
int esock_ssl_write(Connection *cp, char *buf, int len);

int esock_ssl_shutdown(Connection *cp);

/* Peer certificate */

int esock_ssl_getpeercert(Connection *cp, unsigned char **buf);
int esock_ssl_getpeercertchain(Connection *cp, unsigned char **buf);

/* Sessions */
int esock_ssl_session_reused(Connection *cp);

/* Protocol version and cipher of established connection */
int esock_ssl_getprotocol_version(Connection *cp, char **buf);
int esock_ssl_getcipher(Connection *cp, char **buf);

#endif
