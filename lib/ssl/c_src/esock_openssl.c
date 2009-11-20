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
 * Purpose: Adaptions for the OpenSSL package.
 *
 * This file implements the functions defined in esock_ssl.h for
 * the OpenSSL package. 
 *
 * The following holds true for non-blockling I/O:
 *
 *   Function		Return values
 *   -------- 		-------------
 *   SSL_accept()       success: 1, failure: =<0
 *   SSL_connect()      success: 1, failure: =<0
 *   SSL_read()         success: >0, eof: 0, failure: <0 
 *   SSL_write()	success: > 0, failure: =<0 
 *   SSL_shutdown()	success: 1, not finished: 0
 *
 * If the return value of any of the above functions is `ret' and the
 * ssl connection is `ssl', the call
 *
 * 	ssl_error = SSL_get_error(ssl, ret);
 *
 * returns one of the following eight values:
 *
 *   SSL_ERROR_NONE			ret > 0
 *   SSL_ERROR_ZERO_RETURN		ret = 0
 *   SSL_ERROR_WANT_READ		ret < 0 and ssl wants to read
 *   SSL_ERROR_WANT_WRITE		ret < 0 and ssl wants to write
 *   SSL_ERROR_SYSCALL			ret < 0  or ret = 0
 *   SSL_ERROR_SSL			if there was an ssl internal error
 *   SSL_ERROR_WANT_X509_LOOKUP		ret < 0 and ssl wants x509 lookup 
 *   SSL_ERROR_WANT_CONNECT		ret < 0 and ssl wants connect
 *
 * It is the case that SSL_read() sometimes returns -1, even when the 
 * underlying file descriptor is ready for reading.
 * 
 * Also, sometimes we may have SSL_ERROR_SSL in SSL_accept() and SSL_connect()
 * when a retry should be done.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifndef __WIN32__
# include <fcntl.h>
# include <unistd.h>
#endif

#include "esock.h"
#include "esock_ssl.h"
#include "debuglog.h"
#include "esock_utils.h"
#include "esock_posix_str.h"

#include <openssl/crypto.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

int ephemeral_rsa = 0;
int ephemeral_dh = 0;		/* XXX Not used yet */
int protocol_version = 0;

char *esock_ssl_errstr = "";

#define FLAGSBUFSIZE		512
#define X509BUFSIZE		256
#define DEFAULT_VERIFY_DEPTH	1

#define SET_WANT(cp, ssl_error) \
    switch((ssl_error)) { \
    case SSL_ERROR_WANT_READ: \
	(cp)->ssl_want = ESOCK_SSL_WANT_READ; \
	break; \
    case SSL_ERROR_WANT_WRITE: \
	(cp)->ssl_want = ESOCK_SSL_WANT_WRITE; \
	break; \
    default: \
    (cp)->ssl_want = 0; \
	break; \
    }

#define RESET_ERRSTR() \
    esock_ssl_errstr = "";

#define MAYBE_SET_ERRSTR(s) \
    if (!esock_ssl_errstr[0]) \
	esock_ssl_errstr = (s);

typedef struct {
    int code;
    char *text;
} err_entry;

typedef struct {
    SSL_CTX *ctx;
    char *passwd;
    int verify_depth;
} callback_data;

static char *ssl_error_str(int error);
static void end_ssl_call(int ret, Connection *cp, int ssl_error);
static void check_shutdown(Connection *cp);
static int set_ssl_parameters(Connection *cp, SSL_CTX *ctx);
static int verify_callback(int ok, X509_STORE_CTX *ctx);
static int passwd_callback(char *buf, int num, int rwflag, void *userdata);
static void info_callback(const SSL *ssl, int where, int ret);
static void callback_data_free(void *parent, void *ptr, 
			       CRYPTO_EX_DATA *ad, 
			       int idx, long arg1, void *argp);
static RSA *tmp_rsa_callback(SSL *ssl, int is_export, int keylen);
static void restrict_protocols(SSL_CTX *ctx);

static err_entry errs[] = {    
    {SSL_ERROR_NONE, "SSL_ERROR_NONE"},
    {SSL_ERROR_ZERO_RETURN, "SSL_ERROR_ZERO_RETURN"}, 
    {SSL_ERROR_WANT_READ, "SSL_ERROR_WANT_READ"}, 
    {SSL_ERROR_WANT_WRITE, "SSL_ERROR_WANT_WRITE"}, 
    {SSL_ERROR_SYSCALL, "SSL_ERROR_SYSCALL"},
    {SSL_ERROR_SSL, "SSL_ERROR_SSL"},
    {SSL_ERROR_WANT_X509_LOOKUP, "SSL_ERROR_WANT_X509_LOOKUP"},
    {SSL_ERROR_WANT_CONNECT, "SSL_ERROR_WANT_CONNECT"}
};

static SSL_METHOD *method;	/* for listen and connect init */
static char x509_buf[X509BUFSIZE]; /* for verify_callback */
static int callback_data_index = -1; /* for ctx ex_data */
static unsigned char randvec[1024]; /* XXX */

#if defined(__WIN32__) || OPEN_MAX > 256
# define FOPEN_WORKAROUND(var, expr) var = (expr)
# define VOID_FOPEN_WORKAROUND(expr) expr
#else
/*
 * This is an ugly workaround. On Solaris, fopen() will return NULL if
 * it gets a file descriptor > 255. To avoid that, we'll make sure that
 * there is always one low-numbered file descriptor available when
 * fopen() is called.
 */
static int reserved_fd;		/* Reserve a low-numbered file descriptor */
# define USE_FOPEN_WORKAROUND 1

# define FOPEN_WORKAROUND(var, expr)		\
do {						\
   close(reserved_fd);				\
   var = (expr);				\
   reserved_fd = open("/dev/null", O_RDONLY);	\
} while (0)

# define VOID_FOPEN_WORKAROUND(expr)		\
do {						\
   close(reserved_fd);				\
   expr;					\
   reserved_fd = open("/dev/null", O_RDONLY);	\
} while (0)
#endif

esock_version *esock_ssl_version(void)
{
    static esock_version vsn;

    vsn.compile_version = OPENSSL_VERSION_TEXT;
    vsn.lib_version = SSLeay_version(SSLEAY_VERSION);
    return &vsn;
}

char *esock_ssl_ciphers(void)
{
    SSL_CTX *ctx;
    SSL *ssl;
    char *ciphers;
    const char *cp;
    int i = 0, used = 0, len, incr = 1024;

    if (!(ctx = SSL_CTX_new(method)))
	return NULL;
    restrict_protocols(ctx);
    if (!(ssl = SSL_new(ctx))) {
	SSL_CTX_free(ctx);
	return NULL;
    }

    ciphers = esock_malloc(incr);
    len = incr;
    *ciphers = '\0';

    while (1) {
	if (!(cp = SSL_get_cipher_list(ssl, i)))
	    break;
	if (i > 0) {
	    if (used == len) {
		len += incr;
		ciphers = esock_realloc(ciphers, len); 
	    }
	    strcat(ciphers, ":");
	    used++;
	}
	if (strlen(cp) + used >= len) {
	    len += incr;
	    ciphers = esock_realloc(ciphers, len); 
	}
	strcat(ciphers, cp);
	used += strlen(cp);
	i++;
    }
    SSL_free(ssl);
    SSL_CTX_free(ctx);
    return ciphers;
}

void  esock_ssl_seed(void *buf, int len) 
{
    RAND_seed(buf, len);

    /* XXX Maybe we should call RAND_status() and check if we have got
     * enough randomness. 
     */
}

int esock_ssl_init(void)
{
    method = SSLv23_method();	/* SSLv2, SSLv3 and TLSv1, may be restricted
				 in listen and connect */
    SSL_load_error_strings();
    SSL_library_init();
    esock_ssl_seed(randvec, sizeof(randvec));
    callback_data_index = SSL_CTX_get_ex_new_index(0, "callback_data", 
						 NULL, NULL, 
						 callback_data_free);
#ifdef USE_FOPEN_WORKAROUND
    reserved_fd = open("/dev/null", O_RDONLY);
    DEBUGF(("init: reserved_fd=%d\r\n", reserved_fd));
#endif
    return 0;
}


void esock_ssl_finish(void)
{
    /* Nothing */
}


void esock_ssl_free(Connection *cp)
{
    SSL *ssl = cp->opaque;
    SSL_CTX *ctx;

    if (ssl) {
	ctx = SSL_get_SSL_CTX(ssl);
	SSL_free(ssl);
	if (cp->origin != ORIG_ACCEPT)
	    SSL_CTX_free(ctx);
	cp->opaque = NULL;
    }
}


/*
 * Print SSL specific errors.
 */
void esock_ssl_print_errors_fp(FILE *fp)
{
    ERR_print_errors_fp(fp);
}


int esock_ssl_accept_init(Connection *cp, void *listenssl)
{
    SSL_CTX *listenctx;
    SSL *ssl;

    RESET_ERRSTR();
    MAYBE_SET_ERRSTR("esslacceptinit");

    if(!listenssl) {
	DEBUGF(("esock_ssl_accept_init: listenssl null\n"));
	return -1;
    }
    if (!(listenctx = SSL_get_SSL_CTX(listenssl))) {
	DEBUGF(("esock_ssl_accept_init: SSL_get_SSL_CTX\n"));
	return -1;
    }
    if (!(ssl = cp->opaque = SSL_new(listenctx))) {
	DEBUGF(("esock_ssl_accept_init: SSL_new(listenctx)\n"));
	return -1;
    }
    SSL_set_fd(ssl, cp->fd);
    return 0;

}


int esock_ssl_connect_init(Connection *cp)
{
    SSL_CTX *ctx;
    SSL *ssl;

    RESET_ERRSTR();
    MAYBE_SET_ERRSTR("esslconnectinit");

    if (!(ctx = SSL_CTX_new(method)))
	return -1;
    if (set_ssl_parameters(cp, ctx) < 0) {
	SSL_CTX_free(ctx);
	return -1;
    }
    restrict_protocols(ctx);
    if (!(ssl = cp->opaque = SSL_new(ctx))) {
	SSL_CTX_free(ctx);
	return -1;
    }
    SSL_set_fd(ssl, cp->fd);
    return 0;
}


int esock_ssl_listen_init(Connection *cp)
{
    SSL_CTX *ctx;
    SSL *ssl;

    RESET_ERRSTR();
    MAYBE_SET_ERRSTR("essllisteninit");

    if (!(ctx = SSL_CTX_new(method)))
	return -1;
    if (set_ssl_parameters(cp, ctx) < 0) {
	SSL_CTX_free(ctx);
	return -1;
    }
    restrict_protocols(ctx);

    /* The allocation of ctx is for setting ssl parameters, so that
     * accepts can inherit them. We allocate ssl to be able to
     * refer to it via cp->opaque, but will not be used otherwise.
     */
    if (!(ssl = cp->opaque = SSL_new(ctx))) {
	SSL_CTX_free(ctx);
	return -1;
    }
    /* Set callback for temporary ephemeral RSA key generation.
    * Note: for servers only. */
    SSL_CTX_set_tmp_rsa_callback(ctx, tmp_rsa_callback);
    return 0;
}

/* 
 * esock_ssl_accept(Connection *cp)
 *
 */
int esock_ssl_accept(Connection *cp)
{
    int ret, ssl_error;
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();

    DEBUGF(("esock_ssl_accept: calling SSL_accept fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    ret = SSL_accept(ssl);
    DEBUGF(("  sock_errno %d errno %d \n", sock_errno(), errno));
    ssl_error = SSL_get_error(ssl, ret);
    DEBUGF(("  SSL_accept = %d\n"
	    "  ssl_error: %s\n"
	    "  state after: %s\n", 
	    ret, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    DEBUGF(("    ret %d os error %s\n", ret, strerror(errno)));
    if (ret > 0)
	return ret;
    else if (ret == 0) {
	const char* f; int l; unsigned int e;
	while ((e = ERR_get_error_line(&f, &l))) {
	    DEBUGF(("    error %s:%d  %s\n", f, l, ssl_error_str(e)));
	}
	/* permanent accept error */
	sock_set_errno(ERRNO_NONE);
	MAYBE_SET_ERRSTR("esslaccept");
	return -1;
    }
    end_ssl_call(ret, cp, ssl_error);
    return ret;
}

/* 
 * esock_ssl_connect(Connection *cp)
 *
 */
int esock_ssl_connect(Connection *cp)
{
    int ret, ssl_error;
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();

    DEBUGF(("esock_ssl_connect: calling SSL_connect fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    ret = SSL_connect(ssl);
    ssl_error = SSL_get_error(ssl, ret);
    DEBUGF(("  SSL_connect() = %d\n"
	    "  ssl_error: %s\n"
	    "  state after: %s\n", 
	    ret, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (ret > 0)
	return ret;
    else if (ret == 0) {
	/* permanent connect error */
	sock_set_errno(ERRNO_NONE);
	MAYBE_SET_ERRSTR("esslconnect");
	return -1;
    }
    end_ssl_call(ret, cp, ssl_error);
    return ret;
}


int esock_ssl_session_reused(Connection *cp)
{
    SSL *ssl = cp->opaque;

    return SSL_session_reused(ssl);
}


/* esock_ssl_read(Connection *cp, char *buf, int len)
 *
 * Read at most `len' chars into `buf'. Returns number of chars
 * read ( > 0), or 0 at EOF, or -1 on error. Sets cp->eof, cp->bp if
 * appropriate. 
 */

int esock_ssl_read(Connection *cp, char *buf, int len)
{
    int ret, ssl_error;
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();
    DEBUGF(("esock_ssl_read: calling SSL_read fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));

    ret = SSL_read(ssl, buf, len);
    ssl_error = SSL_get_error(ssl, ret);

    DEBUGF(("  SSL_read = %d\n"
	    "  ssl_error: %s\n"
	    "  state after: %s\n", 
	    ret, ssl_error_str(ssl_error), SSL_state_string(ssl)));

    if (ssl_error == SSL_ERROR_NONE) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", ret, buf));
	DEBUGMSGF(("message (char): [%3.*b]\n", ret, buf));
    }
    if (ret > 0)
	return ret;
    if (ret == 0) {
	check_shutdown(cp);
	return ret;
    } 
    end_ssl_call(ret, cp, ssl_error);
    return ret;
}

/* 
 * esock_ssl_write(Connection *cp, char *buf, int len)
 *
 * Writes at most `len' chars from `buf'. Returns number of chars
 * written, or -1 on error.
 */
int esock_ssl_write(Connection *cp, char *buf, int len)
{
    int ret, ssl_error;
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();
    DEBUGF(("esock_ssl_write: calling SSL_write fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    ret = SSL_write(ssl, buf, len);
    ssl_error = SSL_get_error(ssl, ret);
    DEBUGF(("  SSL_write = %d\n"
	    "  ssl_error: %s\n"
	    "  state after: %s\n", 
	    ret, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (ssl_error == SSL_ERROR_NONE) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", ret, buf));
	DEBUGMSGF(("message (char): [%3.*b]\n", ret, buf));
    }
    if (ret > 0)
	return ret;
    if (ret == 0) {
	check_shutdown(cp);
	return ret;
    } 
    end_ssl_call(ret, cp, ssl_error);
    return ret;
}


int esock_ssl_shutdown(Connection *cp)
{
    int ret, ssl_error;
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();
    DEBUGF(("esock_ssl_shutdown: calling SSL_shutdown fd = %d\n"
    "  state before: %s\n",  cp->fd, SSL_state_string(ssl)));
    ret = SSL_shutdown(ssl);
    ssl_error = SSL_get_error(ssl, ret);
    DEBUGF(("  SSL_shutdown = %d\n"
	    "  ssl_error: %s\n"
	    "  state after: %s\n",
	    ret, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (ret >= 0) {
	check_shutdown(cp);
	return ret;
    }
    end_ssl_call(ret, cp, ssl_error);
    return ret;
}


/* Returns total number of bytes in DER encoded cert pointed to by
 * *buf, which is allocated by this function, unless return < 0.  
 * XXX X509_free ??
 */
int esock_ssl_getpeercert(Connection *cp, unsigned char **buf)
{
    int len;
    SSL *ssl = cp->opaque;
    X509 *x509;
    unsigned char *tmp;

    RESET_ERRSTR();
    if((x509 = SSL_get_peer_certificate(ssl)) == NULL) {
	MAYBE_SET_ERRSTR("enopeercert"); /* XXX doc */
	return -1;
    }
    
    if ((len = i2d_X509(x509, NULL)) <= 0) {
	MAYBE_SET_ERRSTR("epeercert");
	return -1;
    }

    tmp = *buf = esock_malloc(len);

    /* We must use a temporary value here, since i2d_X509(X509 *x,
     * unsigned char **out) increments *out.  
     */
    if (i2d_X509(x509, &tmp) < 0) {
	esock_free(tmp);
	MAYBE_SET_ERRSTR("epeercert");
	return -1;
    }
    return len;
}

/* Returns total number of bytes in chain of certs. Each cert begins
 * with a 4-bytes length. The last cert is ended with 4-bytes of
 * zeros. The result is returned in *buf, which is allocated unless
 * the return value is < 0.  
 * XXX X509_free ? sk_X509_free ? 
 * XXX X509_free is reference counting.
 */
int esock_ssl_getpeercertchain(Connection *cp, unsigned char **buf)
{
    SSL *ssl = cp->opaque;
    STACK_OF(X509) *x509_stack;
    X509 *x509;
    int num, i, totlen, pos, *der_len;
    unsigned char *vbuf;

    RESET_ERRSTR();
    if((x509_stack = SSL_get_peer_cert_chain(ssl)) == NULL) {
	MAYBE_SET_ERRSTR("enopeercertchain"); /* XXX doc */
	return -1;
    }
    
    num = sk_X509_num(x509_stack);
    der_len = esock_malloc(num * sizeof(int));
    totlen = 0;

    for (i = 0; i < num; i++) {
	x509 = sk_X509_value(x509_stack, i);
	totlen += 4;
	if ((der_len[i] = i2d_X509(x509, NULL)) < 0) {
	    MAYBE_SET_ERRSTR("epeercertchain");
	    esock_free(der_len);
	    return -1;
	}
	totlen += der_len[i];
    }
    totlen += 4;

    vbuf = *buf = esock_malloc(totlen);
    pos = 0;

    for (i = 0; i < num; i++) {
	x509 = sk_X509_value(x509_stack, i);
	PUT_INT32(der_len[i], vbuf);
	vbuf += 4;
	/* Note: i2d_X509 increments vbuf */
	if (i2d_X509(x509, &vbuf) < 0) {
	    MAYBE_SET_ERRSTR("epeercertchain");
	    esock_free(*buf);
	    esock_free(der_len);
	    return -1;
	}
    }
    esock_free(der_len);
    return totlen;
}


int esock_ssl_getprotocol_version(Connection *cp, char **buf)
{
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();
    if (!ssl) {
	MAYBE_SET_ERRSTR("enoent");
	return -1;
    }
    *buf = (char *) SSL_get_version(ssl);

    return 0;
}


int esock_ssl_getcipher(Connection *cp, char **buf)
{
    SSL *ssl = cp->opaque;

    RESET_ERRSTR();
    if (!ssl) {
	MAYBE_SET_ERRSTR("enoent");
	return -1;
    }
    *buf = (char *) SSL_get_cipher(ssl);

    return 0;
}

/* Local functions */

static char *ssl_error_str(int ssl_error)
{
    int i;
    static char buf[128];

    for (i = 0; i < sizeof(errs)/sizeof(err_entry); i ++) {
	if (ssl_error == errs[i].code)
	    return errs[i].text;
    }
    sprintf(buf, "esock_openssl: SSL_error unknown: %d", ssl_error);
    return buf;
}

void end_ssl_call(int ret, Connection *cp, int ssl_error)
{
    SET_WANT(cp, ssl_error);
    switch (ssl_error) {
    case SSL_ERROR_SYSCALL:
	/* Typically sock_errno() is equal to ERRNO_BLOCK */
	MAYBE_SET_ERRSTR(esock_posix_str(sock_errno()));
	break;
    case SSL_ERROR_SSL:
	sock_set_errno(ERRNO_NONE);
	MAYBE_SET_ERRSTR("esslerrssl");
	break;
    case SSL_ERROR_WANT_X509_LOOKUP:
	SSLDEBUGF();
	sock_set_errno(ERRNO_NONE);
	MAYBE_SET_ERRSTR("ex509lookup");
	break;
    case SSL_ERROR_WANT_CONNECT:
	SSLDEBUGF();
	sock_set_errno(ERRNO_NONE);
	MAYBE_SET_ERRSTR("ewantconnect");
	break;
    default:
	break;
    }
}

void check_shutdown(Connection *cp) 
{
    int sd_mode;
    SSL *ssl = cp->opaque;

    sd_mode = SSL_get_shutdown(ssl);
    if (sd_mode & SSL_RECEIVED_SHUTDOWN)
	cp->eof = 1;
    if (sd_mode & SSL_SENT_SHUTDOWN) {
	DEBUGF(("check_shutdown SSL_SENT_SHUTDOWN\n"));
	cp->bp = 1;
    }
}

/* 
 * set_ssl_parameters
 *
 * Set ssl parameters from connection structure. Only called for
 * listen and connect. 
 *
 * Note: The -cacertdir option is not documented.
 */
static int set_ssl_parameters(Connection *cp, SSL_CTX *ctx)
{
    char *cacertfile = NULL, *cacertdir = NULL, *certfile = NULL;
    char *keyfile = NULL, *ciphers = NULL, *password = NULL;
    int verify = 0, verify_depth = DEFAULT_VERIFY_DEPTH, verify_mode;
    int i, argc;
    char **argv;
    callback_data *cb_data;

    RESET_ERRSTR();

    argc = esock_build_argv(cp->flags, &argv);

    DEBUGF(("Argv:\n"));
    for (i = 0; i < argc; i++) {
	DEBUGF(("%d:  %s\n", i, argv[i]));
    }

    for (i = 0; i < argc; i++) {
	if (strcmp(argv[i], "-verify") == 0) {
	    verify = atoi(argv[++i]);
	} else if (strcmp(argv[i], "-depth") == 0) {
	    verify_depth = atoi(argv[++i]);
	} else if (strcmp(argv[i], "-log") == 0) {
	    /* XXX  ignored: logging per connection not supported */
	    i++;
	} else if (strcmp(argv[i], "-certfile") == 0) {
	    certfile = argv[++i];
	} else if (strcmp(argv[i], "-keyfile") == 0) {
	    keyfile = argv[++i];
	} else if (strcmp(argv[i], "-password") == 0) {
	    password = argv[++i];
	} else if (strcmp(argv[i], "-cacertfile") == 0) {
	    cacertfile = argv[++i];
	} else if (strcmp(argv[i], "-cacertdir") == 0) {
	    cacertdir = argv[++i];
	} else if (strcmp(argv[i], "-d") == 0) {
	    /* XXX  ignored: debug per connection not supported */
	    i++;
	} else if (strcmp(argv[i], "-ciphers") == 0) {
	    ciphers = argv[++i];
	} else {
	    /* XXX Error: now ignored */
	}
    }
    DEBUGF(("set_ssl_parameters: all arguments read\n"));

    if (cp->origin == ORIG_LISTEN && !certfile) {
	DEBUGF(("ERROR: Server must have certificate\n"));
	MAYBE_SET_ERRSTR("enoservercert");
	goto err_end;
    }

    /* Define callback data */
    /* XXX Check for NULL */
    cb_data = esock_malloc(sizeof(callback_data));
    cb_data->ctx = ctx;
    if (password) {
	cb_data->passwd = esock_malloc(strlen(password) + 1);
	strcpy(cb_data->passwd, password);
    } else
	cb_data->passwd = NULL;
    cb_data->verify_depth = verify_depth;
    SSL_CTX_set_ex_data(ctx, callback_data_index, cb_data);

    /* password callback */
    SSL_CTX_set_default_passwd_cb(ctx, passwd_callback);
    SSL_CTX_set_default_passwd_cb_userdata(ctx, cb_data);

    /* Set location for "trusted" certificates */
    if (cacertfile || cacertdir) {
	int res;
	DEBUGF(("set_ssl_parameters: SSL_CTX_load_verify_locations\n"));
	FOPEN_WORKAROUND(res, SSL_CTX_load_verify_locations(ctx, cacertfile,
							    cacertdir));
	if (!res) {
	    DEBUGF(("ERROR: Cannot load verify locations\n"));
	    MAYBE_SET_ERRSTR("ecacertfile");
	    goto err_end;
	}
    } else {
	int res;
	DEBUGF(("set_ssl_parameters: SSL_CTX_set_default_verify_paths\n"));
	FOPEN_WORKAROUND(res, SSL_CTX_set_default_verify_paths(ctx));
	if (!res) {
	    DEBUGF(("ERROR: Cannot set default verify paths\n"));
	    MAYBE_SET_ERRSTR("ecacertfile");
	    goto err_end;
	}
    }

    /* For a server the following sets the list of CA distinguished
     * names that it sends to its client when it requests the
     * certificate from the client.  
     * XXX The names of certs in cacertdir ignored.  
     */
    if (cp->origin == ORIG_LISTEN && cacertfile) {
	DEBUGF(("set_ssl_parameters: SSL_CTX_set_client_CA_list\n"));
	VOID_FOPEN_WORKAROUND(SSL_CTX_set_client_CA_list(ctx,
			           SSL_load_client_CA_file(cacertfile)));
	if (!SSL_CTX_get_client_CA_list(ctx)) {
	    DEBUGF(("ERROR: Cannot set client CA list\n"));
	    MAYBE_SET_ERRSTR("ecacertfile");
	    goto err_end;
	}
    }

    /* Use certificate file if key file has not been set. */
    if (!keyfile)
	keyfile = certfile;

    if (certfile) {
	int res;
	DEBUGF(("set_ssl_parameters: SSL_CTX_use_certificate_file\n"));
	FOPEN_WORKAROUND(res, SSL_CTX_use_certificate_file(ctx, certfile,
							   SSL_FILETYPE_PEM));
	if (res <= 0) {
	    DEBUGF(("ERROR: Cannot set certificate file\n"));
	    MAYBE_SET_ERRSTR("ecertfile");
	    goto err_end;
	}
    }
    if (keyfile) { 
	int res;
	DEBUGF(("set_ssl_parameters: SSL_CTX_use_PrivateKey_file\n"));
	FOPEN_WORKAROUND(res, SSL_CTX_use_PrivateKey_file(ctx, keyfile, 
					SSL_FILETYPE_PEM));
	if (res <= 0) {
	    DEBUGF(("ERROR: Cannot set private key file\n"));
	    MAYBE_SET_ERRSTR("ekeyfile");
	    goto err_end;
	}
    }
    if(certfile && keyfile) {
	DEBUGF(("set_ssl_parameters: SSL_CTX_check_private_key\n"));
	if (!SSL_CTX_check_private_key(ctx)) {
	    DEBUGF(("ERROR: Private key does not match the certificate\n")); 
	    MAYBE_SET_ERRSTR("ekeymismatch");
	    goto err_end;
	}
    }    

    /* Ciphers */
    if (ciphers) {
	DEBUGF(("set_ssl_parameters: SSL_CTX_set_cipher_list\n"));
	if (!SSL_CTX_set_cipher_list(ctx, ciphers)) {
	    DEBUGF(("ERROR: Cannot set cipher list\n"));
	    MAYBE_SET_ERRSTR("ecipher");
	    goto err_end;
	}
    }

    /* Verify depth */
    DEBUGF(("set_ssl_parameters: SSL_CTX_set_verify_depth (depth = %d)\n", 
	    verify_depth)); 
    SSL_CTX_set_verify_depth(ctx, verify_depth);

    /* Verify mode and callback */
    /* XXX Why precisely these modes? */
    switch (verify) {
    case 0:
	verify_mode = SSL_VERIFY_NONE;
	break;
    case 1:
	verify_mode = SSL_VERIFY_PEER|SSL_VERIFY_CLIENT_ONCE;
	break;
    case 2:
	verify_mode = SSL_VERIFY_PEER|SSL_VERIFY_CLIENT_ONCE|
	    SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
	break;
    default:
	verify_mode = SSL_VERIFY_NONE;
    }
    DEBUGF(("set_ssl_parameters: SSL_CTX_set_verify (verify = %d)\n", 
	    verify)); 
    SSL_CTX_set_verify(ctx, verify_mode, verify_callback);

    /* Session id context. Should be an option really. */
    if (cp->origin == ORIG_LISTEN) {
	unsigned char *sid = "Erlang/OTP/ssl";
	SSL_CTX_set_session_id_context(ctx, sid, strlen(sid));
    }

    /* info callback */
    if (debug) 
	SSL_CTX_set_info_callback(ctx, info_callback);

    DEBUGF(("set_ssl_parameters: done\n"));
    /* Free arg list */
    for (i = 0; argv[i]; i++)
	esock_free(argv[i]);
    esock_free(argv);
    return 0;

 err_end:
    DEBUGF(("set_ssl_parameters: error\n"));
    /* Free arg list */
    for (i = 0; argv[i]; i++)
	esock_free(argv[i]);
    esock_free(argv);
    return -1;
}

/* Call back functions */

static int verify_callback(int ok, X509_STORE_CTX *x509_ctx)
{
    X509 *cert;
    int cert_err, depth;
    SSL *ssl;
    SSL_CTX *ctx;
    callback_data *cb_data;

    cert = X509_STORE_CTX_get_current_cert(x509_ctx);
    cert_err = X509_STORE_CTX_get_error(x509_ctx);
    depth = X509_STORE_CTX_get_error_depth(x509_ctx);

    ssl = X509_STORE_CTX_get_ex_data(x509_ctx, 
				     SSL_get_ex_data_X509_STORE_CTX_idx());
    ctx = SSL_get_SSL_CTX(ssl);
    cb_data = SSL_CTX_get_ex_data(ctx, callback_data_index);

    X509_NAME_oneline(X509_get_subject_name(cert), x509_buf, sizeof(x509_buf));
    DEBUGF(("  +vfy: depth = %d\n", depth));
    DEBUGF(("        subject = %s\n", x509_buf));
    X509_NAME_oneline(X509_get_issuer_name(cert),  x509_buf, sizeof(x509_buf));
    DEBUGF(("        issuer = %s\n", x509_buf));

    if (!ok) {
	DEBUGF(("  +vfy: error = %d [%s]\n", cert_err,
		X509_verify_cert_error_string(cert_err)));
	if (depth >= cb_data->verify_depth) 
	    ok = 1;
    }

    switch (cert_err) {
    case X509_V_OK:
    case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
	ok = 1;
	break;
    case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
    case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
	MAYBE_SET_ERRSTR("enoissuercert");
	break;
    case X509_V_ERR_CERT_HAS_EXPIRED:
	MAYBE_SET_ERRSTR("epeercertexpired");
	break;
    case X509_V_ERR_CERT_NOT_YET_VALID:
    case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
    case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
	MAYBE_SET_ERRSTR("epeercertinvalid");
	break;
    case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
	MAYBE_SET_ERRSTR("eselfsignedcert");
	break;
    case X509_V_ERR_CERT_CHAIN_TOO_LONG:
	MAYBE_SET_ERRSTR("echaintoolong");
	break;
    default:
	MAYBE_SET_ERRSTR("epeercert");
	break;
    }
    DEBUGF(("  +vfy: return = %d\n",ok));
    return ok;
}

static int passwd_callback(char *buf, int num, int rwflag, void *userdata)
{
    callback_data *cb_data = userdata;
    int len;

    if (cb_data && cb_data->passwd) {
	DEBUGF(("  +passwd: %s\n", cb_data->passwd));
	strncpy(buf, cb_data->passwd, num);
	len = strlen(cb_data->passwd);
	return len;
    }
    DEBUGF(("  +passwd: ERROR: No password set.\n"));
    return 0;
}

static void info_callback(const SSL *ssl, int where, int ret)
{
    char *str;

    if (where & SSL_CB_LOOP) {
	DEBUGF(("  info: %s\n",SSL_state_string_long(ssl)));
    } else if (where & SSL_CB_ALERT) {
	str = (where & SSL_CB_READ) ? "read" : "write";
	DEBUGF(("  info: SSL3 alert %s:%s:%s\n", str, 
		SSL_alert_type_string_long(ret),
		SSL_alert_desc_string_long(ret)));
    } else if (where & SSL_CB_EXIT) {
	if (ret == 0) {
	    DEBUGF(("  info: failed in %s\n", SSL_state_string_long(ssl)));
	} else if (ret < 0) {
	    DEBUGF(("  info: error in %s\n", SSL_state_string_long(ssl)));
	}
    }
}

/* This function is called whenever a SSL_CTX *ctx structure is
 * freed. 
*/
static void callback_data_free(void *parent, void *ptr, CRYPTO_EX_DATA *ad, 
			       int idx, long arg1, void *argp)
{
    callback_data *cb_data = ptr;
    
    if (cb_data) {
	if (cb_data->passwd) 
	    esock_free(cb_data->passwd);
	esock_free(cb_data);
    }
}

static RSA *tmp_rsa_callback(SSL *ssl, int is_export, int keylen)
{
    static RSA *rsa512 = NULL;
    static RSA *rsa1024 = NULL;

    switch (keylen) {
    case 512:
	if (!rsa512)
	    rsa512 = RSA_generate_key(keylen, RSA_F4, NULL, NULL);
	return rsa512;
	break;
    case 1024:
	if (!rsa1024)
	    rsa1024 = RSA_generate_key(keylen, RSA_F4, NULL, NULL);
	return rsa1024;
	break;
    default:
	if (rsa1024)
	    return rsa1024;
	if (rsa512)
	    return rsa512;
	rsa512 = RSA_generate_key(keylen, RSA_F4, NULL, NULL);
	return rsa512;
    }
}

/* Restrict protocols (SSLv2, SSLv3, TLSv1) */
static void restrict_protocols(SSL_CTX *ctx)
{
    long options = 0;

    if (protocol_version) {
	if ((protocol_version & ESOCK_SSLv2) == 0) 
	    options |= SSL_OP_NO_SSLv2;
	if ((protocol_version & ESOCK_SSLv3) == 0) 
	    options |= SSL_OP_NO_SSLv3;
	if ((protocol_version & ESOCK_TLSv1) == 0) 
	    options |= SSL_OP_NO_TLSv1;
	SSL_CTX_set_options(ctx, options);
    }
}


static unsigned char randvec [] = {
    181, 177, 237, 240, 107, 24, 43, 148, 
    105, 4, 248, 13, 199, 255, 23, 58, 
    71, 181, 57, 151, 156, 25, 165, 7, 
    73, 80, 80, 231, 70, 110, 96, 162, 
    24, 205, 178, 178, 67, 122, 210, 180, 
    92, 6, 156, 182, 84, 159, 85, 6, 
    175, 66, 165, 167, 137, 34, 179, 237, 
    77, 90, 87, 185, 21, 106, 92, 115, 
    137, 65, 233, 42, 164, 153, 208, 133, 
    160, 172, 129, 202, 46, 220, 98, 66, 
    115, 66, 46, 28, 226, 200, 140, 145, 
    207, 194, 58, 71, 56, 203, 113, 34, 
    221, 116, 63, 114, 188, 210, 45, 238, 
    200, 123, 35, 150, 2, 78, 160, 22, 
    226, 167, 162, 10, 182, 75, 109, 97, 
    86, 252, 93, 125, 117, 214, 220, 37, 
    105, 160, 56, 158, 97, 57, 22, 14, 
    73, 169, 111, 190, 222, 176, 14, 82, 
    111, 42, 87, 90, 136, 236, 22, 209, 
    156, 207, 40, 251, 88, 141, 51, 211, 
    31, 158, 153, 91, 119, 83, 255, 60, 
    55, 94, 5, 115, 119, 210, 224, 185, 
    163, 163, 5, 3, 197, 106, 110, 206, 
    109, 132, 50, 190, 177, 133, 175, 129, 
    225, 161, 156, 244, 77, 150, 99, 38, 
    17, 111, 46, 230, 152, 64, 50, 164, 
    19, 78, 3, 164, 169, 175, 104, 97, 
    103, 158, 91, 168, 186, 191, 73, 88, 
    118, 112, 41, 188, 219, 0, 198, 209, 
    206, 7, 5, 169, 127, 180, 80, 74, 
    124, 4, 4, 108, 197, 67, 204, 29, 
    101, 95, 174, 147, 64, 163, 89, 160, 
    10, 5, 56, 134, 209, 69, 209, 55, 
    214, 136, 45, 212, 113, 85, 159, 133, 
    141, 249, 75, 40, 175, 91, 142, 13, 
    179, 179, 51, 0, 136, 63, 148, 175, 
    103, 162, 8, 214, 4, 24, 59, 71, 
    9, 185, 48, 127, 159, 165, 8, 8, 
    135, 151, 92, 214, 132, 151, 204, 169, 
    24, 112, 229, 59, 236, 81, 238, 64, 
    150, 196, 97, 213, 140, 159, 20, 24, 
    79, 210, 191, 53, 130, 33, 157, 87, 
    16, 180, 175, 217, 56, 123, 115, 196, 
    130, 6, 155, 37, 220, 80, 232, 129, 
    240, 57, 199, 249, 196, 152, 28, 111, 
    124, 192, 59, 46, 29, 21, 178, 51, 
    156, 17, 248, 61, 254, 80, 201, 131, 
    203, 59, 227, 191, 71, 121, 134, 181, 
    55, 79, 130, 225, 246, 36, 179, 224, 
    189, 243, 200, 75, 73, 41, 251, 41, 
    71, 251, 78, 146, 99, 101, 104, 69, 
    18, 122, 65, 24, 232, 84, 246, 242, 
    209, 18, 241, 114, 3, 65, 177, 99, 
    49, 99, 215, 59, 9, 175, 195, 11, 
    25, 46, 43, 120, 109, 179, 159, 250, 
    239, 246, 135, 78, 2, 238, 214, 237, 
    64, 170, 50, 44, 68, 67, 111, 232, 
    225, 230, 224, 124, 76, 32, 52, 158, 
    151, 54, 184, 135, 122, 66, 211, 215, 
    121, 90, 124, 158, 55, 73, 116, 137, 
    240, 15, 38, 31, 183, 86, 93, 49, 
    148, 184, 125, 250, 155, 216, 84, 246, 
    27, 172, 141, 54, 80, 158, 227, 254, 
    189, 164, 238, 229, 68, 26, 231, 11, 
    198, 222, 15, 141, 98, 8, 124, 219, 
    60, 125, 170, 213, 114, 24, 189, 65, 
    80, 186, 71, 126, 223, 153, 20, 141, 
    110, 73, 173, 218, 214, 63, 205, 177, 
    132, 115, 184, 28, 122, 232, 210, 72, 
    237, 41, 93, 17, 152, 95, 242, 138, 
    79, 98, 47, 197, 36, 17, 137, 230, 
    15, 73, 193, 1, 181, 123, 0, 186, 
    185, 135, 142, 200, 139, 78, 57, 145, 
    191, 32, 98, 250, 113, 188, 71, 32, 
    205, 81, 219, 99, 60, 87, 42, 95, 
    249, 252, 121, 125, 246, 230, 74, 162, 
    73, 59, 179, 142, 178, 47, 163, 161, 
    236, 14, 123, 219, 18, 6, 102, 140, 
    215, 210, 76, 9, 119, 147, 252, 63, 
    13, 51, 161, 172, 180, 116, 212, 129, 
    116, 237, 38, 64, 213, 222, 35, 14, 
    183, 237, 78, 204, 250, 250, 5, 41, 
    142, 5, 207, 154, 65, 183, 108, 82, 
    1, 43, 149, 233, 89, 195, 25, 233, 
    4, 34, 19, 122, 16, 58, 121, 5, 
    118, 168, 22, 213, 49, 226, 163, 169, 
    21, 78, 179, 232, 125, 216, 198, 147, 
    245, 196, 199, 138, 185, 167, 179, 82, 
    175, 53, 6, 162, 5, 141, 180, 212, 
    95, 201, 234, 169, 111, 175, 138, 197, 
    177, 246, 154, 41, 185, 201, 134, 187, 
    88, 99, 231, 23, 190, 36, 72, 174, 
    244, 185, 205, 50, 230, 226, 210, 119, 
    175, 107, 109, 244, 12, 122, 84, 51, 
    146, 95, 68, 74, 76, 212, 221, 103, 
    244, 71, 63, 133, 149, 233, 48, 3, 
    176, 168, 6, 98, 88, 226, 120, 190, 
    205, 249, 38, 157, 205, 148, 250, 203, 
    147, 62, 195, 229, 219, 109, 177, 119, 
    120, 43, 165, 99, 253, 210, 180, 32, 
    227, 180, 174, 64, 156, 139, 251, 53, 
    205, 132, 210, 208, 3, 199, 115, 64, 
    59, 27, 249, 164, 224, 191, 124, 241, 
    142, 10, 19, 120, 227, 46, 174, 231, 
    48, 65, 41, 56, 51, 38, 185, 95, 
    250, 182, 100, 40, 196, 124, 173, 119, 
    162, 148, 170, 34, 51, 68, 175, 60, 
    242, 201, 225, 34, 146, 157, 159, 0, 
    144, 148, 82, 72, 149, 53, 201, 10, 
    248, 206, 154, 126, 33, 153, 56, 48, 
    5, 90, 194, 22, 251, 173, 211, 202, 
    203, 253, 112, 147, 188, 200, 142, 206, 
    206, 175, 233, 76, 93, 104, 125, 41, 
    64, 145, 202, 53, 130, 251, 23, 90, 
    28, 199, 13, 128, 185, 154, 53, 194, 
    195, 55, 80, 56, 151, 216, 195, 138, 
    7, 170, 143, 236, 74, 141, 229, 174, 
    32, 165, 131, 68, 174, 104, 35, 143, 
    183, 41, 80, 191, 120, 79, 166, 240, 
    123, 55, 60, 2, 128, 56, 4, 199, 
    122, 85, 90, 76, 246, 29, 13, 6, 
    126, 229, 14, 203, 244, 73, 121, 42, 
    169, 35, 44, 202, 18, 69, 153, 120, 
    141, 77, 124, 191, 215, 18, 115, 187, 
    108, 246, 135, 151, 225, 192, 50, 89, 
    128, 45, 39, 253, 149, 234, 203, 84, 
    51, 174, 15, 237, 17, 57, 76, 81, 
    39, 107, 40, 36, 22, 52, 92, 39};
