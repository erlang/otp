/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
 *
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
 * %CopyrightEnd%
 */
/*
 * Purpose: Provides file and directory operations.
 *
 * This file is generic, and does the work of decoding the commands
 * and encoding the responses.  System-specific functions are found in
 * the unix_efile.c and win_efile.c files.
 */

/* Operations */

#define FILE_OPEN		 1 /* Essential for startup */
#define FILE_READ		 2
#define FILE_LSEEK		 3
#define FILE_WRITE		 4
#define FILE_FSTAT		 5 /* Essential for startup */
#define FILE_PWD                 6 /* Essential for startup */
#define FILE_READDIR             7 /* Essential for startup */
#define FILE_CHDIR               8
#define FILE_FSYNC               9
#define FILE_MKDIR              10
#define FILE_DELETE             11
#define FILE_RENAME             12
#define FILE_RMDIR              13
#define FILE_TRUNCATE           14
#define FILE_READ_FILE          15 /* Essential for startup */
#define FILE_WRITE_INFO		16
#define FILE_LSTAT            	19
#define FILE_READLINK        	20
#define FILE_LINK             	21
#define FILE_SYMLINK          	22
#define FILE_CLOSE		23
#define FILE_PWRITEV		24
#define FILE_PREADV		25
#define FILE_SETOPT		26
#define FILE_IPREAD             27
#define FILE_ALTNAME            28
#define FILE_READ_LINE          29
#define FILE_FDATASYNC          30
#define FILE_FADVISE            31

/* Return codes */

#define FILE_RESP_OK         0
#define FILE_RESP_ERROR      1
#define FILE_RESP_DATA       2
#define FILE_RESP_NUMBER     3
#define FILE_RESP_INFO       4
#define FILE_RESP_NUMERR     5
#define FILE_RESP_LDATA      6
#define FILE_RESP_N2DATA     7
#define FILE_RESP_EOF        8

/* Options */

#define FILE_OPT_DELAYED_WRITE 0
#define FILE_OPT_READ_AHEAD    1

/* IPREAD variants */

#define IPREAD_S32BU_P32BU 0

/* Limits */

#define FILE_SEGMENT_READ  (256*1024)
#define FILE_SEGMENT_WRITE (256*1024)

/* Internal */

/* Set to 1 to test having read_ahead implicitly for read_line */ 
#define ALWAYS_READ_LINE_AHEAD 0


/* Must not be possible to get from malloc()! */
#define FILE_FD_INVALID ((Sint)(-1))

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdlib.h>
#include "sys.h"
#include "erl_driver.h"
#include "erl_efile.h"
#include "erl_threads.h"
#include "zlib.h"
#include "gzio.h"
#include <ctype.h>
#include <sys/types.h>

extern void erl_exit(int n, char *fmt, _DOTS_);

static ErlDrvSysInfo sys_info;


/*#define TRACE 1*/
#ifdef TRACE
#    define TRACE_C(c) (putchar(c))
#    define TRACE_S(s) (fputs((s), stdout))
#    define TRACE_F(args) (printf args)
#else
#    define TRACE_C(c) ((void)(0))
#    define TRACE_S(s) ((void)(0))
#    define TRACE_F(args) ((void)(0))
#endif


#ifdef USE_THREADS
#define IF_THRDS if (sys_info.async_threads > 0)
#ifdef HARDDEBUG /* HARDDEBUG in io.c is expected too */
#define TRACE_DRIVER fprintf(stderr, "Efile: ")
#else
#define TRACE_DRIVER
#endif
#define MUTEX_INIT(m, p) do { IF_THRDS { TRACE_DRIVER; (m = driver_pdl_create(p)); } } while (0)
#define MUTEX_LOCK(m)    do { IF_THRDS { TRACE_DRIVER; driver_pdl_lock(m);   } } while (0)
#define MUTEX_UNLOCK(m)  do { IF_THRDS { TRACE_DRIVER; driver_pdl_unlock(m); } } while (0)
#else
#define MUTEX_INIT(m, p)
#define MUTEX_LOCK(m)
#define MUTEX_UNLOCK(m)
#endif



#if 0
/* Experimental, for forcing all file operations to use the same thread. */
static unsigned file_fixed_key = 1;
#define KEY(desc) (&file_fixed_key)
#else
#define KEY(desc) (&(desc)->key)
#endif



#if     MAXPATHLEN >= BUFSIZ
#define    RESBUFSIZE  MAXPATHLEN+1
#else
#define    RESBUFSIZE  BUFSIZ
#endif

#define GET_TIME(i, b) \
    (i).year  = get_int32((b) + 0 * 4); \
    (i).month = get_int32((b) + 1 * 4); \
    (i).day   = get_int32((b) + 2 * 4); \
    (i).hour  = get_int32((b) + 3 * 4); \
    (i).minute = get_int32((b) + 4 * 4); \
    (i).second = get_int32((b) + 5 * 4)

#define PUT_TIME(i, b) \
  put_int32((i).year,  (b) + 0 * 4); \
  put_int32((i).month, (b) + 1 * 4); \
  put_int32((i).day,   (b) + 2 * 4); \
  put_int32((i).hour,  (b) + 3 * 4); \
  put_int32((i).minute,(b) + 4 * 4); \
  put_int32((i).second,(b) + 5 * 4)


#if ALWAYS_READ_LINE_AHEAD
#define DEFAULT_LINEBUF_SIZE 2048
#else
#define DEFAULT_LINEBUF_SIZE 512 /* Small, it's usually discarded anyway */ 
#endif

typedef unsigned char uchar;

static ErlDrvData file_start(ErlDrvPort port, char* command);
static int file_init(void);
static void file_stop(ErlDrvData);
static void file_output(ErlDrvData, char* buf, int len);
static int file_control(ErlDrvData, unsigned int command, 
			char* buf, int len, char **rbuf, int rlen);
static void file_timeout(ErlDrvData);
static void file_outputv(ErlDrvData, ErlIOVec*);
static void file_async_ready(ErlDrvData, ErlDrvThreadData);
static void file_flush(ErlDrvData);



enum e_timer {timer_idle, timer_again, timer_write};

struct t_data;

typedef struct {
    SWord           fd;
    ErlDrvPort      port;
    unsigned int    key;      /* Async queue key */
    unsigned        flags;    /* Original flags from FILE_OPEN. */
    void          (*invoke)(void *);
    struct t_data  *d;
    void          (*free)(void *);
    struct t_data  *cq_head;  /* Queue of incoming commands */
    struct t_data  *cq_tail;  /* -""- */
    enum e_timer    timer_state;
    size_t          read_bufsize;
    ErlDrvBinary   *read_binp;
    size_t          read_offset;
    size_t          read_size;
    size_t          write_bufsize;
    unsigned long   write_delay;
    int             write_error;
    Efile_error     write_errInfo;
    ErlDrvPDL       q_mtx;    /* Mutex for the driver queue, known by the emulator. Also used for
				 mutual exclusion when accessing field(s) below. */
    size_t          write_buffered;
} file_descriptor;


static int reply_error(file_descriptor*, Efile_error* errInfo);

struct erl_drv_entry efile_driver_entry = {
    file_init,
    file_start,
    file_stop,
    file_output,
    NULL,
    NULL,
    "efile",
    NULL,
    NULL,
    file_control,
    file_timeout,
    file_outputv,
    file_async_ready,
    file_flush,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL
};



static int thread_short_circuit;

#define DRIVER_ASYNC(level, desc, f_invoke, data, f_free) \
if (thread_short_circuit >= (level)) { \
    (*(f_invoke))(data); \
    file_async_ready((ErlDrvData)(desc), (data)); \
} else { \
    driver_async((desc)->port, KEY(desc), (f_invoke), (data), (f_free)); \
}



struct t_pbuf_spec {
    Sint64 offset;
    size_t size;
};

struct t_pwritev {
    ErlDrvPort         port;
    ErlDrvPDL          q_mtx;
    size_t             size;
    size_t             free_size;
    unsigned           cnt;
    unsigned           n;
    struct t_pbuf_spec specs[1];
};

struct t_preadv {
    ErlIOVec eiov;
    unsigned n;
    unsigned cnt;
    size_t   size;
    Sint64   offsets[1];
};

#define READDIR_BUFSIZE (8*1024)
#if READDIR_BUFSIZE < (2*MAXPATHLEN)
#undef READDIR_BUFSIZE
#define READDIR_BUFSIZE (2*MAXPATHLEN)
#endif

struct t_readdir_buf {
    struct t_readdir_buf *next;
    char buf[READDIR_BUFSIZE];
};

struct t_data
{
    struct t_data *next;
    int            command;
    int            level;
    void         (*invoke)(void *);
    void         (*free)(void *);
    int            again;
    int            reply;
    int            result_ok;
    Efile_error    errInfo;
    int            flags;
    SWord          fd;
    /**/
    Efile_info        info;
    EFILE_DIR_HANDLE  dir_handle; /* Handle to open directory. */
    ErlDrvBinary     *bin;
    int               drive;
    size_t            n;
    /*off_t             offset;*/
    /*size_t            bytesRead; Bytes read from the file. */
    /**/
    union {
	struct {
	    Sint64 offset;
	    int    origin;
	    Sint64 location;
	} lseek;
	struct {
	    ErlDrvPort    port;
	    ErlDrvPDL     q_mtx;
	    size_t        size;
	    size_t        free_size;
	    size_t        reply_size;
	} writev;
	struct t_pwritev pwritev;
	struct t_preadv  preadv;
	struct {
	    ErlDrvBinary *binp;
	    size_t        bin_offset;
	    size_t        bin_size;
	    size_t        size;
	} read;
	struct {
	    ErlDrvBinary *binp; /* in - out */
	    size_t        read_offset; /* in - out */
	    size_t        read_size; /* in - out */
	    size_t        nl_pos; /* out */
	    short         nl_skip; /* out, 0 or 1 */
#if !ALWAYS_READ_LINE_AHEAD
	    short         read_ahead; /* in, bool */
#endif
	} read_line;
	struct {
	    ErlDrvBinary *binp;
	    int           size;
	    int           offset;
	    char          name[1];
	} read_file;
	struct {
	    struct t_readdir_buf *first_buf;
	    struct t_readdir_buf *last_buf;
	} read_dir;
	struct {
	    Sint64 offset;
	    Sint64 length;
	    int advise;
	} fadvise;
    } c;
    char b[1];
};


#define EF_ALLOC(S)		driver_alloc((S))
#define EF_REALLOC(P, S)	driver_realloc((P), (S))
#define EF_SAFE_ALLOC(S)	ef_safe_alloc((S))
#define EF_SAFE_REALLOC(P, S)	ef_safe_realloc((P), (S))
#define EF_FREE(P)		do { if((P)) driver_free((P)); } while(0)

static void *ef_safe_alloc(Uint s)
{
    void *p = EF_ALLOC(s);
    if (!p) erl_exit(1, "efile drv: Can't allocate %d bytes of memory\n", s);
    return p;
}

#if 0 /* Currently not used */

static void *ef_safe_realloc(void *op, Uint s)
{
    void *p = EF_REALLOC(op, s);
    if (!p) erl_exit(1, "efile drv: Can't reallocate %d bytes of memory\n", s);
    return p;
}

#endif

/*********************************************************************
 * ErlIOVec manipulation functions.
 */

/* char EV_CHAR(ErlIOVec *ev, int p, int q) */
#define EV_CHAR_P(ev, p, q)                   \
    (((char *)(ev)->iov[(q)].iov_base) + (p))

/* int EV_GET_CHAR(ErlIOVec *ev, char *p, int *pp, int *qp) */
#define EV_GET_CHAR(ev, p, pp, qp)                      \
    (*(pp)+1 <= (ev)->iov[*(qp)].iov_len                \
     ? (*(p) = *EV_CHAR_P(ev, *(pp), *(qp)),            \
        *(pp) = (    *(pp)+1 < (ev)->iov[*(qp)].iov_len \
                 ?   *(pp)+1                            \
                 : ((*(qp))++, 0)),                     \
        !0)                                             \
     : 0)

/* Uint32 EV_UINT32(ErlIOVec *ev, int p, int q)*/
#define EV_UINT32(ev, p, q) \
    ((Uint32) *(((unsigned char *)(ev)->iov[(q)].iov_base) + (p)))

/* int EV_GET_UINT32(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT32(ev, p, pp, qp)                      \
    (*(pp)+4 <= (ev)->iov[*(qp)].iov_len                  \
     ? (*(p) = (EV_UINT32(ev, *(pp),   *(qp)) << 24)      \
             | (EV_UINT32(ev, *(pp)+1, *(qp)) << 16)      \
             | (EV_UINT32(ev, *(pp)+2, *(qp)) << 8)       \
             | (EV_UINT32(ev, *(pp)+3, *(qp))),           \
        *(pp) = (    *(pp)+4 < (ev)->iov[*(qp)].iov_len   \
                 ?   *(pp)+4                              \
                 : ((*(qp))++, 0)),                       \
        !0)                                               \
     : 0)

/* Uint64 EV_UINT64(ErlIOVec *ev, int p, int q)*/
#define EV_UINT64(ev, p, q) \
    ((Uint64) *(((unsigned char *)(ev)->iov[(q)].iov_base) + (p)))

/* int EV_GET_UINT64(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT64(ev, p, pp, qp)                      \
    (*(pp)+8 <= (ev)->iov[*(qp)].iov_len                  \
     ? (*(p) = (EV_UINT64(ev, *(pp),   *(qp)) << 56)      \
             | (EV_UINT64(ev, *(pp)+1, *(qp)) << 48)      \
             | (EV_UINT64(ev, *(pp)+2, *(qp)) << 40)      \
             | (EV_UINT64(ev, *(pp)+3, *(qp)) << 32)      \
             | (EV_UINT64(ev, *(pp)+4, *(qp)) << 24)      \
             | (EV_UINT64(ev, *(pp)+5, *(qp)) << 16)      \
             | (EV_UINT64(ev, *(pp)+6, *(qp)) << 8)       \
             | (EV_UINT64(ev, *(pp)+7, *(qp))),           \
        *(pp) = (    *(pp)+8 < (ev)->iov[*(qp)].iov_len   \
                 ?   *(pp)+8                              \
                 : ((*(qp))++, 0)),                       \
        !0)                                               \
     : 0)



#if 0

static void ev_clear(ErlIOVec *ev) {
    ASSERT(ev);
    ev->size = 0;
    ev->vsize = 0;
    ev->iov = NULL;
    ev->binv = NULL;
}

/* Assumes that ->iov and ->binv were allocated with sys_alloc().
 */
static void ev_free(ErlIOVec *ev) {
    if (! ev) {
	return;
    }
    if (ev->vsize > 0) {
	int i;
	ASSERT(ev->iov);
	ASSERT(ev->binv);
	for (i = 0; i < ev->vsize; i++) {
	    if (ev->binv[i]) {
		driver_free_binary(ev->binv[i]);
	    }
	}
	EF_FREE(ev->iov);
	EF_FREE(ev->binv);
    }
}

/* Copy the contents from source to dest.
 * Data in binaries is not copied, just the pointers; 
 * and refc is incremented.
 */
static ErlIOVec *ev_copy(ErlIOVec *dest, ErlIOVec *source) {
    int *ip;
    ASSERT(dest);
    ASSERT(source);
    if (source->vsize == 0) {
	/* Empty source */
	ev_clear(dest);
	return dest;
    }
    /* Allocate ->iov and ->binv */
    dest->iov = EF_ALLOC(sizeof(*dest->iov) * source->vsize);
    if (! dest->iov) {
	return NULL;
    }
    dest->binv = EF_ALLOC(sizeof(*dest->binv) * source->vsize);
    if (! dest->binv) {
	EF_FREE(dest->iov);
	return NULL;
    }
    dest->size = source->size;
    /* Copy one vector element at the time. 
     * Use *ip as an alias for dest->vsize to improve readabiliy.
     * Keep dest consistent in every iteration by using 
     * dest->vsize==*ip as loop variable.
     */
    for (ip = &dest->vsize, *ip = 0;  *ip < source->vsize;  (*ip)++) {
	if (source->iov[*ip].iov_len == 0) {
	    /* Empty vector element */
	    dest->iov[*ip].iov_len = 0;
	    dest->iov[*ip].iov_base = NULL;
	    dest->binv[*ip] = NULL;
	} else {
	    /* Non empty vector element */
	    if (source->binv[*ip]) {
		/* Contents in binary - copy pointers and increment refc */
		dest->iov[*ip] = source->iov[*ip];
		dest->binv[*ip] = source->binv[*ip];
		driver_binary_inc_refc(source->binv[*ip]);
	    } else {
		/* Contents not in binary - allocate new binary and copy data */
		if (! (dest->binv[*ip] = 
		       driver_alloc_binary(source->iov[*ip].iov_len))) {
		    goto failed;
		}
		sys_memcpy(dest->binv[*ip]->orig_bytes,
			   source->iov[*ip].iov_base,
			   source->iov[*ip].iov_len);
		dest->iov[*ip].iov_base = dest->binv[*ip]->orig_bytes;
		dest->iov[*ip].iov_len = source->iov[*ip].iov_len;
	    }
	}
    }
    return dest;
 failed:
    ev_free(dest);
    return NULL;
}

#endif



/*********************************************************************
 * Command queue functions
 */

static void cq_enq(file_descriptor *desc, struct t_data *d) {
    ASSERT(d);
    if (desc->cq_head) {
	ASSERT(desc->cq_tail);
	ASSERT(!desc->cq_tail->next);
	desc->cq_tail = desc->cq_tail->next = d;
    } else {
	ASSERT(desc->cq_tail == NULL);
	desc->cq_head = desc->cq_tail = d;
    }
    d->next = NULL;
}

static struct t_data *cq_deq(file_descriptor *desc) {
    struct t_data *d = desc->cq_head;
    ASSERT(d || (!d && !desc->cq_tail));
    if (d) {
	ASSERT(!d->next || (d->next && desc->cq_tail != d));
	if ((desc->cq_head = d->next) == NULL) {
	    ASSERT(desc->cq_tail == d);
	    desc->cq_tail = NULL;
	}
    }	
    return d;
}



/*********************************************************************
 * Driver entry point -> init
 */
static int 
file_init(void)
{
    char buf[21]; /* enough to hold any 64-bit integer */
    size_t bufsz = sizeof(buf);
    thread_short_circuit = (erl_drv_getenv("ERL_EFILE_THREAD_SHORT_CIRCUIT",
					   buf,
					   &bufsz) == 0
			    ? atoi(buf)
			    : 0);
    driver_system_info(&sys_info, sizeof(ErlDrvSysInfo));
    return 0;
}

/*********************************************************************
 * Driver entry point -> start
 */
static ErlDrvData 
file_start(ErlDrvPort port, char* command) 

{
    file_descriptor* desc;

    if ((desc = (file_descriptor*) EF_ALLOC(sizeof(file_descriptor)))
	== NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    desc->fd = FILE_FD_INVALID;
    desc->port = port;
    desc->key = (unsigned int) (UWord) port;
    desc->flags = 0;
    desc->invoke = NULL;
    desc->d = NULL;
    desc->free = NULL;
    desc->cq_head = NULL;
    desc->cq_tail = NULL;
    desc->timer_state = timer_idle;
    desc->read_bufsize = 0;
    desc->read_binp = NULL;
    desc->read_offset = 0;
    desc->read_size = 0;
    desc->write_delay = 0L;
    desc->write_bufsize = 0;
    desc->write_error = 0;
    MUTEX_INIT(desc->q_mtx, port); /* Refc is one, referenced by emulator now */
    desc->write_buffered = 0;
    return (ErlDrvData) desc;
}

static void free_data(void *data)
{
    EF_FREE(data);
}

static void do_close(int flags, SWord fd) {
    if (flags & EFILE_COMPRESSED) {
	erts_gzclose((gzFile)(fd));
    } else {
	efile_closefile((int) fd);
    }
}

static void invoke_close(void *data)
{
    struct t_data *d = (struct t_data *) data;
    d->again = 0;
    do_close(d->flags, d->fd);
}

/*********************************************************************
 * Driver entry point -> stop
 */
static void 
file_stop(ErlDrvData e)
{
    file_descriptor* desc = (file_descriptor*)e;

    TRACE_C('p');

    if (desc->fd != FILE_FD_INVALID) {
	do_close(desc->flags, desc->fd);
	desc->fd = FILE_FD_INVALID;
	desc->flags = 0;
    }
    if (desc->read_binp) {
	driver_free_binary(desc->read_binp);
    }
    EF_FREE(desc);
}


/*
 * Sends back an error reply to Erlang.
 */

static void reply_posix_error(file_descriptor *desc, int posix_errno) {
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------+
     * | FILE_RESP_ERROR | Posix error id string |
     * +-----------------------------------------+
     */

    TRACE_C('E');

    response[0] = FILE_RESP_ERROR;
    for (s = erl_errno_id(posix_errno), t = response+1; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
}

static void reply_Uint_posix_error(file_descriptor *desc, Uint num, 
				   int posix_errno) {
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +----------------------------------------------------------------------+
     * | FILE_RESP_NUMERR | 64-bit number (big-endian) | Posix error id string |
     * +----------------------------------------------------------------------+
     */

    TRACE_C('N');

    response[0] = FILE_RESP_NUMERR;
#if SIZEOF_VOID_P == 4 || HALFWORD_HEAP
    put_int32(0, response+1);
#else
    put_int32(num>>32, response+1);
#endif
    put_int32((Uint32)num, response+1+4);
    for (s = erl_errno_id(posix_errno), t = response+1+4+4; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
}



static int reply_error(file_descriptor *desc, 
		       Efile_error *errInfo) /* The error codes. */
{
    reply_posix_error(desc, errInfo->posix_errno);
    return 0;
}

static int reply_Uint_error(file_descriptor *desc, Uint num, 
			    Efile_error *errInfo) /* The error codes. */
{
    reply_Uint_posix_error(desc, num, errInfo->posix_errno);
    return 0;
}

static int reply_ok(file_descriptor *desc) {
    char c = FILE_RESP_OK;

    driver_output2(desc->port, &c, 1, NULL, 0);
    return 0;
}

static int reply(file_descriptor *desc, int ok, Efile_error *errInfo) {
    if (!ok) {
	reply_error(desc, errInfo);
    } else {
	TRACE_C('K');
	reply_ok(desc);
    }
    return 0;
}

static int reply_Uint(file_descriptor *desc, Uint result) {
    char tmp[1+4+4];

    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------------+
     * | FILE_RESP_NUMBER | 64-bit number (big-endian) |
     * +-----------------------------------------------+
     */

    TRACE_C('R');

    tmp[0] = FILE_RESP_NUMBER;
#if SIZEOF_VOID_P == 4 || HALFWORD_HEAP
    put_int32(0, tmp+1);
#else
    put_int32(result>>32, tmp+1);
#endif
    put_int32((Uint32)result, tmp+1+4);
    driver_output2(desc->port, tmp, sizeof(tmp), NULL, 0);
    return 0;
}

static int reply_Sint64(file_descriptor *desc, Sint64 result) {
    char tmp[1+4+4];

    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------------+
     * | FILE_RESP_NUMBER | 64-bit number (big-endian) |
     * +-----------------------------------------------+
     */

    TRACE_C('R');

    tmp[0] = FILE_RESP_NUMBER;
    put_int64(result, tmp+1);
    driver_output2(desc->port, tmp, sizeof(tmp), NULL, 0);
    return 0;
}

#if 0
static void reply_again(file_descriptor *desc) {
    char tmp[1];
    tmp[0] = FILE_RESP_AGAIN;
    driver_output2(desc->port, tmp, sizeof(tmp), NULL, 0);
}
#endif

static void reply_ev(file_descriptor *desc, char response, ErlIOVec *ev) {
    char tmp[1];
    /* Data arriving at the Erlang process:
     * [Response, Binary0, Binary1, .... | BinaryN-1]
     */
    tmp[0] = response;
    driver_outputv(desc->port, tmp, sizeof(tmp), ev, 0);
}

static void reply_data(file_descriptor *desc, 
		       ErlDrvBinary *binp, size_t offset, size_t len) {
    char header[1+4+4];
    /* Data arriving at the Erlang process:
     * [?FILE_RESP_DATA, 64-bit length (big-endian) | Data]
     */
    header[0] = FILE_RESP_DATA;
#if SIZEOF_SIZE_T == 4
    put_int32(0, header+1);
#else
    put_int32(len>>32, header+1);
#endif
    put_int32((Uint32)len, header+1+4);
    driver_output_binary(desc->port, header, sizeof(header),
			 binp, offset, len);
}

static void reply_buf(file_descriptor *desc, char *buf, size_t len) {
    char header[1+4+4];
    /* Data arriving at the Erlang process:
     * [?FILE_RESP_DATA, 64-bit length (big-endian) | Data]
     */
    header[0] = FILE_RESP_DATA;
#if SIZEOF_SIZE_T == 4
    put_int32(0, header+1);
#else
    put_int32(len>>32, header+1);
#endif
    put_int32((Uint32)len, header+1+4);
    driver_output2(desc->port, header, sizeof(header), buf, len);
}

static int reply_eof(file_descriptor *desc) {
    char c = FILE_RESP_EOF;

    driver_output2(desc->port, &c, 1, NULL, 0);
    return 0;
}


 
static void invoke_name(void *data, int (*f)(Efile_error *, char *))
{
    struct t_data *d = (struct t_data *) data;
    char *name = (char *) d->b;

    d->again = 0;
    d->result_ok = (*f)(&d->errInfo, name);
}

static void invoke_mkdir(void *data)
{
    invoke_name(data, efile_mkdir);
}

static void invoke_rmdir(void *data)
{
    invoke_name(data, efile_rmdir);
}

static void invoke_delete_file(void *data)
{
    invoke_name(data, efile_delete_file);
}

static void invoke_chdir(void *data)
{
    invoke_name(data, efile_chdir);
}

static void invoke_fdatasync(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;

    d->again = 0;
    d->result_ok = efile_fdatasync(&d->errInfo, fd);
}

static void invoke_fsync(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;

    d->again = 0;
    d->result_ok = efile_fsync(&d->errInfo, fd);
}

static void invoke_truncate(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;

    d->again = 0;
    d->result_ok = efile_truncate_file(&d->errInfo, &fd, d->flags);
}

static void invoke_read(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status, segment;
    size_t size, read_size;

    segment = d->again && d->c.read.bin_size >= 2*FILE_SEGMENT_READ;
    if (segment) {
	size = FILE_SEGMENT_READ;
    } else {
	size = d->c.read.bin_size;
    }
    read_size = size;
    if (d->flags & EFILE_COMPRESSED) {
	read_size = erts_gzread((gzFile)d->fd, 
				d->c.read.binp->orig_bytes + d->c.read.bin_offset,
				size);
	status = (read_size != -1);
	if (!status) {
	    d->errInfo.posix_errno = EIO;
	}
    } else {
	status = efile_read(&d->errInfo, d->flags, (int) d->fd,
			    d->c.read.binp->orig_bytes + d->c.read.bin_offset,
			    size,
			    &read_size);
    }
    if ( (d->result_ok = status)) {
	ASSERT(read_size <= size);
	d->c.read.bin_offset += read_size;
	if (read_size < size || !segment) {
	    d->c.read.bin_size = 0;
	    d->again = 0;
	} else {
	    d->c.read.bin_size -= read_size;
	}
    } else {
	d->again = 0;
    }
}

static void free_read(void *data)
{
    struct t_data *d = (struct t_data *) data;

    driver_free_binary(d->c.read.binp);
    EF_FREE(d);
}

static void invoke_read_line(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status;
    size_t read_size;
    int local_loop = (d->again == 0);

    do {
	size_t size = (d->c.read_line.binp)->orig_size - 
	    d->c.read_line.read_offset - d->c.read_line.read_size;
	if (size == 0) {
	    /* Need more place */
	    size_t need = (d->c.read_line.read_size >= DEFAULT_LINEBUF_SIZE) ? 
		d->c.read_line.read_size + DEFAULT_LINEBUF_SIZE : DEFAULT_LINEBUF_SIZE;
	    ErlDrvBinary   *newbin = driver_alloc_binary(need);
	    if (newbin == NULL) {
		d->result_ok = 0;
		d->errInfo.posix_errno = ENOMEM;
		d->again = 0;
		break;
	    }
	    memcpy(newbin->orig_bytes, (d->c.read_line.binp)->orig_bytes + d->c.read_line.read_offset,  
		   d->c.read_line.read_size);
	    driver_free_binary(d->c.read_line.binp);
	    d->c.read_line.binp = newbin;
	    d->c.read_line.read_offset = 0;
	    size = need - d->c.read_line.read_size;
	}
	if (d->flags & EFILE_COMPRESSED) {
	    read_size = erts_gzread((gzFile)d->fd, 
				    d->c.read_line.binp->orig_bytes + 
				    d->c.read_line.read_offset + d->c.read_line.read_size,
				    size);
	    status = (read_size != -1);
	    if (!status) {
		d->errInfo.posix_errno = EIO;
	    }
	} else {
	    status = efile_read(&d->errInfo, d->flags, (int) d->fd,
				d->c.read_line.binp->orig_bytes + 
				d->c.read_line.read_offset + d->c.read_line.read_size,
				size,
				&read_size);
	}
	if ( (d->result_ok = status)) {
	    void *nl_ptr = memchr((d->c.read_line.binp)->orig_bytes + 
				  d->c.read_line.read_offset + d->c.read_line.read_size,'\n',read_size);
	    ASSERT(read_size <= size);
	    d->c.read_line.read_size += read_size;
	    if (nl_ptr != NULL) {
		/* If found, we're done */
		d->c.read_line.nl_pos = ((char *) nl_ptr) - 
		    ((char *) ((d->c.read_line.binp)->orig_bytes)) + 1;
		if (d->c.read_line.nl_pos > 1 &&
		    *(((char *) nl_ptr) - 1) == '\r') {
		    --d->c.read_line.nl_pos;
		    *(((char *) nl_ptr) - 1) = '\n';
		    d->c.read_line.nl_skip = 1;
		} else {
		    d->c.read_line.nl_skip = 0;
		}
		d->again = 0;
#if !ALWAYS_READ_LINE_AHEAD
		if (!(d->c.read_line.read_ahead)) {
		    /* Ouch! Undo buffering... */
		    size_t too_much = d->c.read_line.read_size - d->c.read_line.nl_skip - 
			(d->c.read_line.nl_pos - d->c.read_line.read_offset);
		    d->c.read_line.read_size -= too_much;
		    ASSERT(d->c.read_line.read_size >= 0);
		    if (d->flags & EFILE_COMPRESSED) {
			Sint64 location = erts_gzseek((gzFile)d->fd, 
						      -((Sint64) too_much), EFILE_SEEK_CUR);
			if (location == -1) {
			    d->result_ok = 0;
			    d->errInfo.posix_errno = errno;
			}
		    } else {
			Sint64 location;
			d->result_ok = efile_seek(&d->errInfo, (int) d->fd, 
						-((Sint64) too_much), EFILE_SEEK_CUR,
						&location);
		    }
		}
#endif
		break;
	    } else if (read_size == 0) {
		d->c.read_line.nl_pos = 
		    d->c.read_line.read_offset + d->c.read_line.read_size;
		d->c.read_line.nl_skip = 0;
		d->again = 0;
		break;
	    }
	} else {
	    d->again = 0;
	    break;
	}
    } while (local_loop);
}

static void free_read_line(void *data)
{
    struct t_data *d = (struct t_data *) data;

    driver_free_binary(d->c.read_line.binp);
    EF_FREE(d);
}

static void invoke_read_file(void *data)
{
    struct t_data *d = (struct t_data *) data;
    size_t read_size;
    int chop;
    
    if (! d->c.read_file.binp) { /* First invocation only */
	int fd;
	Sint64 size;
	
	if (! (d->result_ok = 
	       efile_openfile(&d->errInfo, d->c.read_file.name, 
			      EFILE_MODE_READ, &fd, &size))) {
	    goto done;
	}
	d->fd = fd;
	d->c.read_file.size = (int) size;
	if (size < 0 || size != d->c.read_file.size ||
	    ! (d->c.read_file.binp = 
	       driver_alloc_binary(d->c.read_file.size))) {
	    d->result_ok = 0;
	    d->errInfo.posix_errno = ENOMEM;
	    goto close;
	}
	d->c.read_file.offset = 0;
    }
    /* Invariant: d->c.read_file.size >= d->c.read_file.offset */
    
    read_size = (size_t) (d->c.read_file.size - d->c.read_file.offset);
    if (! read_size) goto close;
    chop = d->again && read_size >= FILE_SEGMENT_READ*2;
    if (chop) read_size = FILE_SEGMENT_READ;
    d->result_ok = 
	efile_read(&d->errInfo, 
		   EFILE_MODE_READ, 
		   (int) d->fd, 
		   d->c.read_file.binp->orig_bytes + d->c.read_file.offset,
		   read_size, 
		   &read_size);
    if (d->result_ok) {
	d->c.read_file.offset += read_size;
	if (chop) return; /* again */
    }
 close:
    efile_closefile((int) d->fd);
 done:
    d->again = 0;
}

static void free_read_file(void *data)
{
    struct t_data *d = (struct t_data *) data;

    if (d->c.read_file.binp) driver_free_binary(d->c.read_file.binp);
    EF_FREE(d);
}



static void invoke_preadv(void *data)
{
    struct t_data   *d = (struct t_data *) data;
    struct t_preadv *c = &d->c.preadv;
    ErlIOVec        *ev = &c->eiov;
    size_t           bytes_read_so_far = 0;
    unsigned char   *p = (unsigned char *)ev->iov[0].iov_base + 4+4+8*c->cnt;

    while (c->cnt < c->n) {
	size_t read_size = ev->iov[1 + c->cnt].iov_len - c->size;
	size_t bytes_read = 0;
	int chop = d->again 
	    && bytes_read_so_far + read_size >= 2*FILE_SEGMENT_READ;
	if (chop) {
	    ASSERT(bytes_read_so_far < FILE_SEGMENT_READ);
	    read_size = FILE_SEGMENT_READ + FILE_SEGMENT_READ/2
		- bytes_read_so_far;
	}
	if ( (d->result_ok 
	      = efile_pread(&d->errInfo, 
			    (int) d->fd,
			    c->offsets[c->cnt] + c->size,
			    ev->iov[1 + c->cnt].iov_base + c->size,
			    read_size,
			    &bytes_read))) {
	    bytes_read_so_far += bytes_read;
	    if (chop && bytes_read == read_size) {
		c->size += bytes_read;
		return;
	    }
	    ASSERT(bytes_read <= read_size);
	    ev->iov[1 + c->cnt].iov_len = bytes_read + c->size;
	    ev->size += bytes_read + c->size;
	    put_int64(bytes_read + c->size, p); p += 8;
	    c->size = 0;
	    c->cnt++;
	    if (d->again 
		&& bytes_read_so_far >= FILE_SEGMENT_READ
		&& c->cnt < c->n) {
		return;
	    }
	} else {
	    /* In case of a read error, ev->size will not be correct,
	     * which does not matter since no read data is returned
	     * to Erlang.
	     */
	    break;
	}
    }					
    d->again = 0;
}

static void free_preadv(void *data) {
    struct t_data *d = data;
    int            i;
    ErlIOVec      *ev = &d->c.preadv.eiov;
    
    for(i = 0; i < ev->vsize; i++) {
	driver_free_binary(ev->binv[i]);
    }
    EF_FREE(d);
}

static void invoke_ipread(void *data)
{
    struct t_data   *d = data;
    struct t_preadv *c = &d->c.preadv;
    ErlIOVec        *ev = &c->eiov;
    size_t bytes_read = 0;
    char buf[2*sizeof(Uint32)];
    Uint32 offset, size;
    
    /* Read indirection header */
    if (! efile_pread(&d->errInfo, (int) d->fd, c->offsets[0], 
		      buf, sizeof(buf), &bytes_read)) {
	goto error;
    }
    if (bytes_read != sizeof(buf)) goto done; /* eof */
    size = get_int32(buf);
    offset = get_int32(buf+4);
    if (size > c->size) goto done; /* eof */
    c->n = 1;
    c->cnt = 0;
    c->size = 0;
    c->offsets[0] = offset;
    if (! (ev->binv[0] = driver_alloc_binary(3*8))) {
	d->errInfo.posix_errno = ENOMEM;
	goto error;
    }
    ev->vsize = 1;
    ev->iov[0].iov_len = 3*8;
    ev->iov[0].iov_base = ev->binv[0]->orig_bytes;
    ev->size = ev->iov[0].iov_len;
    put_int64(offset, ev->iov[0].iov_base);
    put_int64(size, ((char *)ev->iov[0].iov_base) + 2*8);
    if (size == 0) {
	put_int64(size, ((char *)ev->iov[0].iov_base) + 8);
	goto done;
    }
    if (! (ev->binv[1] = driver_alloc_binary(size))) {
	d->errInfo.posix_errno = ENOMEM;
	goto error;
    }
    ev->vsize = 2;
    ev->iov[1].iov_len = size;
    ev->iov[1].iov_base = ev->binv[1]->orig_bytes;
    /* Read data block */
    d->invoke = invoke_preadv;
    invoke_preadv(data);
    return;
 error:
    d->result_ok = 0;
    d->again = 0;
    return;
 done:
    d->result_ok = !0;
    d->again = 0;
}

/* invoke_writev and invoke_pwritev are the only thread functions that
 * access non-thread data i.e the port queue and a mutex in the port
 * structure that is used to lock the port queue.
 *
 * The port will normally not be terminated until the port queue is
 * empty, but if the port is killed, i.e., exit(Port, kill) is called,
 * it will terminate regardless of the port queue state. When the
 * port is invalid driver_peekq() returns NULL and set the size to -1,
 * and driver_sizeq() returns -1.
 */

static void invoke_writev(void *data) {
    struct t_data *d = (struct t_data *) data;
    SysIOVec      *iov0;
    SysIOVec      *iov;
    int            iovlen;
    int            iovcnt;
    size_t         size;
    size_t         p;
    int            segment;

    segment = d->again && d->c.writev.size >= 2*FILE_SEGMENT_WRITE;
    if (segment) {
	size = FILE_SEGMENT_WRITE;
    } else {
	size = d->c.writev.size;
    }

    /* Copy the io vector to avoid locking the port que while writing */
    MUTEX_LOCK(d->c.writev.q_mtx); /* Lock before accessing the port queue */
    iov0 = driver_peekq(d->c.writev.port, &iovlen);

    /* Calculate iovcnt */
    for (p = 0, iovcnt = 0;
	 p < size && iovcnt < iovlen;
	 p += iov0[iovcnt++].iov_len)
	;
    iov = EF_ALLOC(sizeof(SysIOVec)*iovcnt);
    memcpy(iov,iov0,iovcnt*sizeof(SysIOVec));
    MUTEX_UNLOCK(d->c.writev.q_mtx);
    /* Let go of lock until we deque from original vector */

    if (iovlen > 0) {
	ASSERT(iov[iovcnt-1].iov_len > p - size);
	iov[iovcnt-1].iov_len -= p - size;
	if (d->flags & EFILE_COMPRESSED) {
	    int i, status = 1;
	    for (i = 0; i < iovcnt; i++) {
		if (iov[i].iov_base && iov[i].iov_len > 0) {
		    /* Just in case, I do not know what gzwrite does
		     * with errno.
		     */
		    errno = EINVAL; 
		    if (! (status = 
			   erts_gzwrite((gzFile)d->fd, 
					iov[i].iov_base,
					iov[i].iov_len)) == iov[i].iov_len) {
			d->errInfo.posix_errno =
			    d->errInfo.os_errno = errno; /* XXX Correct? */
			break;
		    }
		}
	    }
	    d->result_ok = status;
	} else {
	    d->result_ok = efile_writev(&d->errInfo, 
					d->flags, (int) d->fd,
					iov, iovcnt, size);
	}
    } else if (iovlen == 0) {
	d->result_ok = 1;
    }
    else { /* Port has terminated */
	d->result_ok = 0;
	d->errInfo.posix_errno = d->errInfo.os_errno = EINVAL;
    }
    EF_FREE(iov);

    d->c.writev.free_size = size;
    d->c.writev.size -= size;
    if (! d->result_ok) {
	d->again = 0;
    } else {
	if (! segment) {
	    d->again = 0;
	}
	TRACE_F(("w%lu", (unsigned long)size));

    }
}

static void free_writev(void *data) {
    struct t_data *d = data;
    MUTEX_LOCK(d->c.writev.q_mtx);
    driver_deq(d->c.writev.port, d->c.writev.size + d->c.writev.free_size);
    MUTEX_UNLOCK(d->c.writev.q_mtx);
    EF_FREE(d);
}

static void invoke_pwd(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_getdcwd(&d->errInfo,d->drive, d->b+1,
				 RESBUFSIZE-1);
}

static void invoke_readlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char resbuf[RESBUFSIZE];	/* Result buffer. */

    d->again = 0;
    d->result_ok = efile_readlink(&d->errInfo, d->b, resbuf+1,
				  RESBUFSIZE-1);
    if (d->result_ok != 0)
	strcpy((char *) d->b + 1, resbuf+1);
}

static void invoke_altname(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char resbuf[RESBUFSIZE];	/* Result buffer. */

    d->again = 0;
    d->result_ok = efile_altname(&d->errInfo, d->b, resbuf+1,
				  RESBUFSIZE-1);
    if (d->result_ok != 0)
	strcpy((char *) d->b + 1, resbuf+1);
}

static void invoke_pwritev(void *data) {
    struct t_data    *d = (struct t_data *) data;
    SysIOVec         *iov0;
    SysIOVec         *iov;
    int               iovlen;
    int               iovcnt;
    struct t_pwritev *c = &d->c.pwritev;
    size_t            p;
    int               segment;
    size_t            size, write_size;

    segment = d->again && c->size >= 2*FILE_SEGMENT_WRITE;
    if (segment) {
	size = FILE_SEGMENT_WRITE;
    } else {
	size = c->size;
    }
    d->result_ok = !0;
    p = 0;
    /* Lock the queue just for a while, we don't want it locked during write */
    MUTEX_LOCK(c->q_mtx);
    iov0 = driver_peekq(c->port, &iovlen);
    iov = EF_ALLOC(sizeof(SysIOVec)*iovlen);
    memcpy(iov,iov0,sizeof(SysIOVec)*iovlen);
    MUTEX_UNLOCK(c->q_mtx);

    if (iovlen < 0)
	goto error; /* Port terminated */
    for (iovcnt = 0, c->free_size = 0;
	 c->cnt < c->n && iovcnt < iovlen && c->free_size < size;
	 c->cnt++) {
	int chop;
	write_size = c->specs[c->cnt].size;
	if (iov[iovcnt].iov_len - p < write_size) {
	    /* Mismatch between pos/size spec and what is queued */
	    d->errInfo.posix_errno = EINVAL;
	    d->result_ok = 0;
	    d->again = 0;
	    goto done;
	}
	chop = segment && c->free_size + write_size >= 2*FILE_SEGMENT_WRITE;
	if (chop) {
	    ASSERT(c->free_size < FILE_SEGMENT_WRITE);
	    write_size = FILE_SEGMENT_WRITE + FILE_SEGMENT_WRITE/2 
		- c->free_size;
	}
	d->result_ok = efile_pwrite(&d->errInfo, (int) d->fd,
				    iov[iovcnt].iov_base + p,
				    write_size,
				    c->specs[c->cnt].offset);
	if (! d->result_ok) {
	    d->again = 0;
	    goto done;
	}
	c->free_size += write_size; 
	c->size -= write_size;
	if (chop) { 
	    c->specs[c->cnt].offset += write_size;
	    c->specs[c->cnt].size -= write_size;
	    /* Schedule out (d->again != 0) */
	    goto done;
	}
	/* Move forward in buffer */
	p += write_size;
	ASSERT(iov[iovcnt].iov_len >= p);
	if (iov[iovcnt].iov_len == p) {
	    /* Move to next iov[], we trust that it is not a 
	     * zero length vector, and thereby depend on that
	     * such are not queued.
	     */
	    iovcnt++; p = 0;
	}
    }
    if (! segment) {
	if (c->cnt != c->n) {
	    /* Mismatch between number of 
	     * pos/size specs vs number of queued buffers .
	     */
	error:
	    d->errInfo.posix_errno = EINVAL;
	    d->result_ok = 0;
	    d->again = 0;
	} else {
	    ASSERT(c->free_size == size);
	    d->again = 0;
	}
    }
 done:
    EF_FREE(iov); /* Free our copy of the vector, nothing to restore */
}

static void free_pwritev(void *data) {
    struct t_data *d = data;

    MUTEX_LOCK(d->c.writev.q_mtx);
    driver_deq(d->c.pwritev.port, d->c.pwritev.free_size + d->c.pwritev.size);
    MUTEX_UNLOCK(d->c.writev.q_mtx);
    EF_FREE(d);
}

static void invoke_flstat(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_fileinfo(&d->errInfo, &d->info,
				  d->b, d->command == FILE_LSTAT);
}

static void invoke_link(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    d->again = 0;
    new_name = name+strlen(name)+1;
    d->result_ok = efile_link(&d->errInfo, name, new_name);
}

static void invoke_symlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    d->again = 0;
    new_name = name+strlen(name)+1;
    d->result_ok = efile_symlink(&d->errInfo, name, new_name);
}

static void invoke_rename(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    d->again = 0;
    new_name = name+strlen(name)+1;
    d->result_ok = efile_rename(&d->errInfo, name, new_name);
}

static void invoke_write_info(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_write_info(&d->errInfo, &d->info, d->b);
}

static void invoke_lseek(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status;

    d->again = 0;
    if (d->flags & EFILE_COMPRESSED) {
	int offset = (int) d->c.lseek.offset;
	
	if (offset != d->c.lseek.offset) {
	    d->errInfo.posix_errno = EINVAL;
	    status = 0;
	} else {
	    d->c.lseek.location = erts_gzseek((gzFile)d->fd, 
					      offset, d->c.lseek.origin);
	    if (d->c.lseek.location == -1) {
		d->errInfo.posix_errno = errno;
		status = 0;
	    } else {
		status = 1;
	    }
	}
    } else {
	status = efile_seek(&d->errInfo, (int) d->fd, 
			    d->c.lseek.offset, d->c.lseek.origin,
			    &d->c.lseek.location);
    }
    d->result_ok = status;
}

static void invoke_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int s;
    char *p = NULL;
    int buf_sz = 0;

    d->again = 0;
    d->errInfo.posix_errno = 0;

    while (1) {
	char *str;
	if (buf_sz < (4 /* sz */ + 1 /* cmd */ + MAXPATHLEN + 1 /* '\0' */)) {
	    struct t_readdir_buf *b;
	    if (p) {
		put_int32(0, p); /* EOB */
	    }
	    b = EF_SAFE_ALLOC(sizeof(struct t_readdir_buf));
	    b->next = NULL;
	    if (d->c.read_dir.last_buf)
		d->c.read_dir.last_buf->next = b;
	    else
		d->c.read_dir.first_buf = b;
	    d->c.read_dir.last_buf = b;
	    p = &b->buf[0];
	    buf_sz = READDIR_BUFSIZE - 4/* EOB */;
	}

	p[4] = FILE_RESP_OK;
	buf_sz -= 4 + 1;
	str = p + 4 + 1;
	ASSERT(buf_sz >= MAXPATHLEN + 1);
	s = efile_readdir(&d->errInfo, d->b, &d->dir_handle, str, buf_sz);

	if (s) {
	    int str_sz = strlen(str);
	    int sz = str_sz + 1;
	    put_int32(sz, p);
	    p += 4 + sz;
	    buf_sz -= str_sz;
	}
	else {
	    put_int32(1, p);
	    p += 4 + 1;
	    put_int32(0, p); /* EOB */
	    d->result_ok = (d->errInfo.posix_errno == 0);
	    break;
	}
    }
}

static void invoke_open(void *data)
{
    struct t_data *d = (struct t_data *) data;
    
    int status = 1;		/* Status of open call. */

    d->again = 0;
    if ((d->flags & EFILE_COMPRESSED) == 0) {
	int fd;
	status = efile_openfile(&d->errInfo, d->b, d->flags, &fd, NULL);
	d->fd = fd;
    } else {
	char* mode = NULL;

	if (((d->flags & (EFILE_MODE_READ_WRITE)) == EFILE_MODE_READ_WRITE) ||
	    (d->flags & EFILE_MODE_APPEND)) {
	    status = 0;
	    d->errInfo.posix_errno = EINVAL;
	} else {
	    status = efile_may_openfile(&d->errInfo, d->b);
	    if (status || (d->errInfo.posix_errno != EISDIR)) {
		mode = (d->flags & EFILE_MODE_READ) ? "rb" : "wb";
		d->fd = (SWord) erts_gzopen(d->b, mode);
		if ((gzFile)d->fd) {
		    status = 1;
		} else {
		    if (errno == 0) {
			errno = ENOMEM;
		    }
		    d->errInfo.posix_errno = errno;
		    status = 0;
		}
	    }
	}
    }

    d->result_ok = status;
}

static void invoke_fadvise(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;
    off_t offset = (off_t) d->c.fadvise.offset;
    off_t length = (off_t) d->c.fadvise.length;
    int advise = (int) d->c.fadvise.advise;

    d->again = 0;
    d->result_ok = efile_fadvise(&d->errInfo, fd, offset, length, advise);
}

static void free_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;
    struct t_readdir_buf *b1 = d->c.read_dir.first_buf;
    while (b1) {
	struct t_readdir_buf *b2 = b1;
	b1 = b1->next;
	EF_FREE(b2);
    }
    EF_FREE(d);
}



static void try_free_read_bin(file_descriptor *desc) {
    if ((desc->read_size == 0)
	&& (desc->read_offset >= desc->read_binp->orig_size)) {
	ASSERT(desc->read_offset == desc->read_binp->orig_size);
	driver_free_binary(desc->read_binp);
	desc->read_binp = NULL;
	desc->read_offset = 0;
	desc->read_size = 0;
    }
}



static int try_again(file_descriptor *desc, struct t_data *d) {
    if (! d->again) {
	return 0;
    }
    switch (d->command) {
    case FILE_WRITE:
	MUTEX_LOCK(d->c.writev.q_mtx);
	driver_deq(d->c.writev.port, d->c.writev.free_size);
	MUTEX_UNLOCK(d->c.writev.q_mtx);
	break;
    case FILE_PWRITEV:
	MUTEX_LOCK(d->c.writev.q_mtx);
	driver_deq(d->c.pwritev.port, d->c.pwritev.free_size);
	MUTEX_UNLOCK(d->c.writev.q_mtx);
	break;
    }
    if (desc->timer_state != timer_idle) {
	driver_cancel_timer(desc->port);
    }
    desc->timer_state = timer_again;
    desc->invoke = d->invoke;
    desc->d = d;
    desc->free = d->free;
    driver_set_timer(desc->port, 0L);
    return !0;
}



static void cq_execute(file_descriptor *desc) {
    struct t_data *d;
    register void *void_ptr; /* Soft cast variable */
    if (desc->timer_state == timer_again)
	return;
    if (! (d = cq_deq(desc)))
	return;
    TRACE_F(("x%i", (int) d->command));
    d->again = sys_info.async_threads == 0;
    DRIVER_ASYNC(d->level, desc, d->invoke, void_ptr=d, d->free);
}

static int async_write(file_descriptor *desc, int *errp,
		       int reply, Uint32 reply_size) {
    struct t_data *d;
    if (! (d = EF_ALLOC(sizeof(struct t_data) - 1))) {
	if (errp) *errp = ENOMEM;
	return -1;
    }
    TRACE_F(("w%lu", (unsigned long)desc->write_buffered));
    d->command = FILE_WRITE;
    d->fd = desc->fd;
    d->flags = desc->flags;
    d->c.writev.port = desc->port;
    d->c.writev.q_mtx = desc->q_mtx;
    d->c.writev.size = desc->write_buffered;
    d->reply = reply;
    d->c.writev.free_size = 0;
    d->c.writev.reply_size = reply_size;
    d->invoke = invoke_writev;
    d->free = free_writev;
    d->level = 1;
    cq_enq(desc, d);
    desc->write_buffered = 0;
    return 0;
}

static int flush_write(file_descriptor *desc, int *errp) {
    int    result;
    MUTEX_LOCK(desc->q_mtx);
    if (desc->write_buffered > 0) {
	result = async_write(desc, errp, 0, 0);
    } else {
	result = 0;
    }
    MUTEX_UNLOCK(desc->q_mtx);
    return result;
}

static int check_write_error(file_descriptor *desc, int *errp) {
    if (desc->write_error) {
	if (errp) *errp = desc->write_errInfo.posix_errno;
	desc->write_error = 0;
	return -1;
    }
    return 0;
}

static int flush_write_check_error(file_descriptor *desc, int *errp) {
    int r;
    if ( (r = flush_write(desc, errp)) != 0) {
	check_write_error(desc, NULL);
	return r;
    } else {
	return check_write_error(desc, errp);
    }
}

static int async_lseek(file_descriptor *desc, int *errp, int reply, 
		       Sint64 offset, int origin) {
    struct t_data *d;
    if (! (d = EF_ALLOC(sizeof(struct t_data)))) {
	*errp = ENOMEM;
	return -1;
    }
    d->flags = desc->flags;
    d->fd = desc->fd;
    d->command = FILE_LSEEK;
    d->reply = reply;
    d->c.lseek.offset = offset;
    d->c.lseek.origin = origin;
    d->invoke = invoke_lseek;
    d->free = free_data;
    d->level = 1;
    cq_enq(desc, d);
    return 0;
}

static void flush_read(file_descriptor *desc) {
    desc->read_offset = 0;
    desc->read_size = 0;
    if (desc->read_binp) {
	driver_free_binary(desc->read_binp);
	desc->read_binp = NULL;
    }
}

static int lseek_flush_read(file_descriptor *desc, int *errp) {
    int r = 0;
    size_t read_size = desc->read_size;
    if (read_size != 0) {
	flush_read(desc);
	if ((r = async_lseek(desc, errp, 0, 
			     -((ssize_t)read_size), EFILE_SEEK_CUR)) 
	    < 0) {
	    return r;
	}
    } else {
	flush_read(desc);
    }
    return r;
}



/*********************************************************************
 * Driver entry point -> ready_async
 */
static void 
file_async_ready(ErlDrvData e, ErlDrvThreadData data)
{
    file_descriptor *desc = (file_descriptor*)e;
    struct t_data *d = (struct t_data *) data;
    char header[5];		/* result code + count */
    char resbuf[RESBUFSIZE];	/* Result buffer. */
    

    TRACE_C('r');

    if (try_again(desc, d)) {
	return;
    }

    switch (d->command)
    {
    case FILE_READ:
	if (!d->result_ok) {
	    reply_error(desc, &d->errInfo);
	} else {
	    size_t available_bytes = 
		d->c.read.bin_offset + d->c.read.bin_size - desc->read_offset;
	    if (available_bytes < d->c.read.size) {
		d->c.read.size = available_bytes;
	    }
	    TRACE_C('D');
	    reply_data(desc, d->c.read.binp, 
		       desc->read_offset, d->c.read.size);
	    desc->read_offset += d->c.read.size;
	    desc->read_size = 
		d->c.read.bin_offset + d->c.read.bin_size - desc->read_offset;
	    try_free_read_bin(desc);
	}
	free_read(data);
	break;
      case FILE_READ_LINE:
	  /* The read_line stucture differs from the read structure.
	     The data->read_offset and d->c.read_line.read_offset are copies, as are 
	     data->read_size and d->c.read_line.read_size 
             The read_line function does not kniow in advance how large the binary has to be,
	     why new allocation (but not reallocation of the old binary, for obvious reasons) 
	     may happen in the worker thread. */
	if (!d->result_ok) {
	    reply_error(desc, &d->errInfo);
	} else {
	    size_t len = d->c.read_line.nl_pos - d->c.read_line.read_offset;
	    TRACE_C('L');
	    reply_data(desc, d->c.read_line.binp, 
		       d->c.read_line.read_offset, len);
	    desc->read_offset = d->c.read_line.read_offset + d->c.read_line.nl_skip + len;
	    desc->read_size = 
		d->c.read_line.read_size - d->c.read_line.nl_skip - len;
	    if (desc->read_binp != d->c.read_line.binp) { /* New binary allocated */
		driver_free_binary(desc->read_binp);
		desc->read_binp =  d->c.read_line.binp;
		driver_binary_inc_refc(desc->read_binp);
	    }
#if !ALWAYS_READ_LINE_AHEAD
	    ASSERT(desc->read_bufsize > 0 || desc->read_size == 0);
	    if (desc->read_bufsize == 0) {
		desc->read_offset = desc->read_binp->orig_size; /* triggers cleanup */
	    }
#endif
	    try_free_read_bin(desc);
	}
	free_read_line(data);
	break;
      case FILE_READ_FILE:
	if (!d->result_ok)
	    reply_error(desc, &d->errInfo);
	else {
	    header[0] = FILE_RESP_OK;
	    TRACE_C('R');
	    driver_output_binary(desc->port, header, 1,
				 d->c.read_file.binp,
				 0, d->c.read_file.offset);
	}
	free_read_file(data);
	break;
      case FILE_WRITE:
	  if (d->reply) {
	      if (! d->result_ok) {
		  reply_error(desc, &d->errInfo);
	      } else {
		  reply_Uint(desc, d->c.writev.reply_size);
	      }
	  } else {
	      if (! d->result_ok) {
		  desc->write_error = !0;
		  desc->write_errInfo = d->errInfo;
	      }
	  }
	  free_writev(data);
	  break;
      case FILE_LSEEK:
	  if (d->reply) {
	      if (d->result_ok)
		  reply_Sint64(desc, d->c.lseek.location);
	      else
		  reply_error(desc, &d->errInfo);
	  }
	  free_data(data);
	  break;
      case FILE_MKDIR:
      case FILE_RMDIR:
      case FILE_CHDIR:
      case FILE_DELETE:
      case FILE_FDATASYNC:
      case FILE_FSYNC:
      case FILE_TRUNCATE:
      case FILE_LINK:
      case FILE_SYMLINK:
      case FILE_RENAME:
      case FILE_WRITE_INFO:
      case FILE_FADVISE:
	reply(desc, d->result_ok, &d->errInfo);
	free_data(data);
	break;
      case FILE_ALTNAME:
      case FILE_PWD:
      case FILE_READLINK:
        {
	    int length;
	    char *resbuf = d->b;

	    if (!d->result_ok)
		reply_error(desc, &d->errInfo);
	    else {
		resbuf[0] = FILE_RESP_OK;
		length = 1+strlen((char*) resbuf+1);
		TRACE_C('R');
		driver_output2(desc->port, resbuf, length, NULL, 0);
	    }
	    free_data(data);
	    break;
	}
      case FILE_OPEN:
	if (!d->result_ok) {
	    reply_error(desc, &d->errInfo);
	} else {
	    desc->fd = d->fd;
	    desc->flags = d->flags;
	    reply_Uint(desc, d->fd);
	}
	free_data(data);
	break;
      case FILE_FSTAT:
      case FILE_LSTAT:
        {
	    if (d->result_ok) {
		resbuf[0] = FILE_RESP_INFO;

		put_int32(d->info.size_high,         &resbuf[1 + (0 * 4)]);
		put_int32(d->info.size_low,          &resbuf[1 + (1 * 4)]);
		put_int32(d->info.type,              &resbuf[1 + (2 * 4)]);

		PUT_TIME(d->info.accessTime, resbuf + 1 + 3*4);
		PUT_TIME(d->info.modifyTime, resbuf + 1 + 9*4);
		PUT_TIME(d->info.cTime, resbuf + 1 + 15*4);

		put_int32(d->info.mode,              &resbuf[1 + (21 * 4)]);
		put_int32(d->info.links,             &resbuf[1 + (22 * 4)]);
		put_int32(d->info.major_device,      &resbuf[1 + (23 * 4)]);
		put_int32(d->info.minor_device,      &resbuf[1 + (24 * 4)]);
		put_int32(d->info.inode,             &resbuf[1 + (25 * 4)]);
		put_int32(d->info.uid,               &resbuf[1 + (26 * 4)]);
		put_int32(d->info.gid,               &resbuf[1 + (27 * 4)]);
		put_int32(d->info.access,            &resbuf[1 + (28 * 4)]);

#define RESULT_SIZE (1 + (29 * 4))
		TRACE_C('R');
		driver_output2(desc->port, resbuf, RESULT_SIZE, NULL, 0);
#undef RESULT_SIZE
	    } else
		reply_error(desc, &d->errInfo);
	}
	free_data(data);
	break;
      case FILE_READDIR:
	if (!d->result_ok)
	    reply_error(desc, &d->errInfo);
	else {
	    struct t_readdir_buf *b1 = d->c.read_dir.first_buf;
	    TRACE_C('R');
	    ASSERT(b1);
	    while (b1) {
		struct t_readdir_buf *b2 = b1;
		char *p = &b1->buf[0];
		int sz = get_int32(p);
		while (sz) { /* 0 == EOB */
		    p += 4;
		    driver_output2(desc->port, p, sz, NULL, 0);
		    p += sz;
		    sz = get_int32(p);
		}
		b1 = b1->next;
		EF_FREE(b2);
	    }
	    d->c.read_dir.first_buf = NULL;
	    d->c.read_dir.last_buf = NULL;
	}
	free_readdir(data);
	break;
	/* See file_stop */
      case FILE_CLOSE:
	  if (d->reply) {
	      TRACE_C('K');
	      reply_ok(desc);
	  }
	  free_data(data);
	  break;
      case FILE_PWRITEV:
	  if (!d->result_ok) {
	      reply_Uint_error(desc, d->c.pwritev.cnt, &d->errInfo);
	  } else {
	      reply_Uint(desc, d->c.pwritev.n);
	  }
	  free_pwritev(data);
	  break;
      case FILE_PREADV:
	  if (!d->result_ok) {
	      reply_error(desc, &d->errInfo);
	  } else {
	      reply_ev(desc, FILE_RESP_LDATA, &d->c.preadv.eiov);
	  }
	  free_preadv(data);
	  break;
      case FILE_IPREAD:
	  if (!d->result_ok) {
	      reply_error(desc, &d->errInfo);
	  } else if (!d->c.preadv.eiov.vsize) {
	      reply_eof(desc);
	  } else {
	      reply_ev(desc, FILE_RESP_N2DATA, &d->c.preadv.eiov);
	  }
	  free_preadv(data);
	  break;
      default:
	abort();
    }
    if (desc->write_buffered != 0 && desc->timer_state == timer_idle) {
	desc->timer_state = timer_write;
	driver_set_timer(desc->port, desc->write_delay);
    }
    cq_execute(desc);
}

/*********************************************************************
 * Driver entry point -> output
 */
static void 
file_output(ErlDrvData e, char* buf, int count)
{
    file_descriptor* desc = (file_descriptor*)e;
    Efile_error errInfo;	/* The error codes for the last operation. */
    Sint fd;			/* The file descriptor for this port, if any,
				 * -1 if none.
				 */
    char* name;			/* Points to the filename in buf. */
    int command;
    struct t_data *d = NULL;


    TRACE_C('o');

    fd  = desc->fd;
    name = buf+1;
    command = *(uchar*)buf++;

    switch(command) {

    case FILE_MKDIR:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_mkdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_RMDIR:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_rmdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_DELETE:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_delete_file;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_RENAME:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + strlen(name) + 1
			      + strlen(new_name) + 1);
	
	    strcpy(d->b, name);
	    strcpy(d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_rename;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}
    case FILE_CHDIR:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_chdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_PWD:
        {
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + RESBUFSIZE + 1);
	
	    d->drive = *(uchar*)buf;
	    d->command = command;
	    d->invoke = invoke_pwd;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_READDIR: 
#ifdef USE_THREADS
	if (sys_info.async_threads > 0)
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(name) + 1);
	
	    strcpy(d->b, name);
	    d->dir_handle = NULL;
	    d->command = command;
	    d->invoke = invoke_readdir;
	    d->free = free_readdir;
	    d->level = 2;
	    d->c.read_dir.first_buf = NULL;
	    d->c.read_dir.last_buf = NULL;
	    goto done;
	}
	else   
#endif
	{
	    char resbuf[RESBUFSIZE+1];
	    EFILE_DIR_HANDLE dir_handle; /* Handle to open directory. */

	    errInfo.posix_errno = 0;
	    dir_handle = NULL;
	    resbuf[0] = FILE_RESP_OK;

	    while (efile_readdir(&errInfo, name, &dir_handle,
				 resbuf+1, RESBUFSIZE)) {
		int length = 1 + strlen(resbuf+1);
		driver_output2(desc->port, resbuf, length, NULL, 0);
	    }
	    if (errInfo.posix_errno != 0) {
		reply_error(desc, &errInfo);
		return;
	    }
	    TRACE_C('R');
	    driver_output2(desc->port, resbuf, 1, NULL, 0);
	    return;
	}
    case FILE_OPEN:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(buf+4) + 1);
	
	    d->flags = get_int32((uchar*)buf);
	    name = buf+4;
	    strcpy(d->b, name);
	    d->command = command;
	    d->invoke = invoke_open;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_FDATASYNC:
    {
	    d = EF_SAFE_ALLOC(sizeof(struct t_data));

	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_fdatasync;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
    }

    case FILE_FSYNC:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data));
	
	d->fd = fd;
	d->command = command;
	d->invoke = invoke_fsync;
	d->free = free_data;
	d->level = 2;
	goto done;
    }


    case FILE_FSTAT: 
    case FILE_LSTAT:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->fd = fd;
	d->command = command;
	d->invoke = invoke_flstat;
	d->free = free_data;
	d->level = 2;
	goto done;
    }

    case FILE_TRUNCATE:
        {
	    d = EF_SAFE_ALLOC(sizeof(struct t_data));
	    
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_truncate;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_WRITE_INFO:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + strlen(buf+21*4) + 1);
	    
	    d->info.mode = get_int32(buf + 0 * 4);
	    d->info.uid = get_int32(buf + 1 * 4);
	    d->info.gid = get_int32(buf + 2 * 4);
	    GET_TIME(d->info.accessTime, buf + 3 * 4);
	    GET_TIME(d->info.modifyTime, buf + 9 * 4);
	    GET_TIME(d->info.cTime, buf + 15 * 4);
	    strcpy(d->b, buf+21*4);
	    d->command = command;
	    d->invoke = invoke_write_info;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_READLINK:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + RESBUFSIZE + 1);
	
	    strcpy(d->b, name);
	    d->command = command;
	    d->invoke = invoke_readlink;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_ALTNAME:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + RESBUFSIZE + 1);
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_altname;
	d->free = free_data;
	d->level = 2;
	goto done;
    }


    case FILE_LINK:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + strlen(name) + 1
			      + strlen(new_name) + 1);
	
	    strcpy(d->b, name);
	    strcpy(d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_link;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_SYMLINK:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + strlen(name) + 1
			      + strlen(new_name) + 1);
	
	    strcpy(d->b, name);
	    strcpy(d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_symlink;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_FADVISE:
    {
        d = EF_SAFE_ALLOC(sizeof(struct t_data));

        d->fd = fd;
        d->command = command;
        d->invoke = invoke_fadvise;
        d->free = free_data;
        d->level = 2;
        d->c.fadvise.offset = get_int64((uchar*) buf);
        d->c.fadvise.length = get_int64(((uchar*) buf) + sizeof(Sint64));
        d->c.fadvise.advise = get_int32(((uchar*) buf) + 2 * sizeof(Sint64));
        goto done;
    }

    }

    /*
     * Ignore anything else -- let the caller hang.
     */
     
    return;

 done:
    if (d) {
	cq_enq(desc, d);
    }
}

/*********************************************************************
 * Driver entry point -> flush
 */
static void 
file_flush(ErlDrvData e) {
    file_descriptor *desc = (file_descriptor *)e;
    int r;

    TRACE_C('f');

    r = flush_write(desc, NULL);
    /* Only possible reason for bad return value is ENOMEM, and 
     * there is nobody to tell...
     */
    ASSERT(r == 0); 
    r = 0; /* Avoiding warning */
    cq_execute(desc);
}



/*********************************************************************
 * Driver entry point -> control
 */
static int 
file_control(ErlDrvData e, unsigned int command, 
			 char* buf, int len, char **rbuf, int rlen) {
    file_descriptor *desc = (file_descriptor *)e;
    switch (command) {
    default:
	return 0;
    } /* switch (command) */
    ASSERT(0);
    desc = NULL; /* XXX Avoid warning while empty switch */
    return 0;
}

/*********************************************************************
 * Driver entry point -> timeout
 */
static void 
file_timeout(ErlDrvData e) {
    file_descriptor *desc = (file_descriptor *)e;
    enum e_timer timer_state = desc->timer_state;

    TRACE_C('t');

    desc->timer_state = timer_idle;
    switch (timer_state) {
    case timer_idle:
	ASSERT(0);
	break;
    case timer_again:
	ASSERT(desc->invoke);
	ASSERT(desc->free);
	driver_async(desc->port, KEY(desc), desc->invoke, desc->d, desc->free);
	break;
    case timer_write: {
	int r = flush_write(desc, NULL);
	/* Only possible reason for bad return value is ENOMEM, and 
	 * there is nobody to tell...
	 */
	ASSERT(r == 0); 
	r = 0; /* Avoiding warning */
	cq_execute(desc);
    } break;
    } /* case */
}



/*********************************************************************
 * Driver entry point -> outputv
 */
static void 
file_outputv(ErlDrvData e, ErlIOVec *ev) {
    file_descriptor* desc = (file_descriptor*)e;
    char command;
    int p, q;
    int err;

    TRACE_C('v');

    p = 0; q = 1;
    if (! EV_GET_CHAR(ev, &command, &p, &q)) {
	/* Empty command */
	reply_posix_error(desc, EINVAL);
	goto done;
    }
    /* 'command' contains the decoded command number,
     * 'p' and 'q' point out the next byte in the command:
     * ((char *)ev->iov[q].iov_base) + p;
     */
    
    TRACE_F(("%i", (int) command));

    switch (command) {

    case FILE_CLOSE: {
	flush_read(desc);
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size != 1) {
	    /* Wrong command length */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if (desc->fd != FILE_FD_INVALID) {
	    struct t_data *d;
	    if (! (d = EF_ALLOC(sizeof(struct t_data)))) {
		reply_posix_error(desc, ENOMEM);
	    } else {
		d->command = command;
		d->reply = !0;
		d->fd = desc->fd;
		d->flags = desc->flags;
		d->invoke = invoke_close;
		d->free = free_data;
		d->level = 2;
		cq_enq(desc, d);
		desc->fd = FILE_FD_INVALID;
		desc->flags = 0;
	    }
	} else {
	    reply_posix_error(desc, EBADF);
	}
    } goto done;

    case FILE_READ: {
	Uint32 sizeH, sizeL;
	size_t size, alloc_size;
	struct t_data *d;
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
#if ALWAYS_READ_LINE_AHEAD
	if (desc->read_bufsize == 0 && desc->read_binp != NULL && desc->read_size > 0) {
	    /* We have allocated a buffer for line mode but should not really have a 
	       read-ahead buffer... */
	    if (lseek_flush_read(desc, &err) < 0) {
		reply_posix_error(desc, err);
		goto done;
	    }
	}
#endif
	if (ev->size != 1+8
	    || !EV_GET_UINT32(ev, &sizeH, &p, &q)
	    || !EV_GET_UINT32(ev, &sizeL, &p, &q)) {
	    /* Wrong buffer length to contain the read count */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
#if SIZEOF_SIZE_T == 4
	if (sizeH != 0) {
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	size = sizeL;
#else
	size = ((size_t)sizeH << 32) | sizeL;
#endif
	if ((desc->fd == FILE_FD_INVALID)
	    || (! (desc->flags & EFILE_MODE_READ)) ) {
	    reply_posix_error(desc, EBADF);
	    goto done;
	}
	if (size == 0) {
	    reply_buf(desc, &command, 0);
	    goto done;
	}
	if (desc->read_size >= size) {
	    /* We already have all data */
	    TRACE_C('D');
	    reply_data(desc, desc->read_binp, desc->read_offset, size);
	    desc->read_offset += size;
	    desc->read_size -= size;
	    try_free_read_bin(desc);
	    goto done;
	}
	/* We may have some of the data 
	 */
	/* Justification for the following strange formula:
	 * If the read request is for such a large block as more than 
	 * half the buffer size it may lead to a lot of unnecessary copying, 
	 * since the tail of the old buffer is copied to the head of the
	 * new, and if the tail is almost half the buffer it is a lot
	 * to copy. Therefore allocate the exact amount needed in 
	 * this case, giving no lingering tail. */
	alloc_size = 
	    size > (desc->read_bufsize>>1) ? 
	    size : desc->read_bufsize;
	if (! desc->read_binp) {
	    /* Need to allocate a new binary for the result */
	    if (! (desc->read_binp = driver_alloc_binary(alloc_size))) {
		reply_posix_error(desc, ENOMEM);
		goto done;
	    }
	} else {
	    /* We already have a buffer */
	    if (desc->read_binp->orig_size - desc->read_offset < size) {
		/* Need to allocate a new binary for the result */
		ErlDrvBinary *binp;
		if (! (binp = driver_alloc_binary(alloc_size))) {
		    reply_posix_error(desc, ENOMEM);
		    goto done;
		}
		/* Move data we already have to the new binary */
		sys_memcpy(binp->orig_bytes, 
			   desc->read_binp->orig_bytes + desc->read_offset,
			   desc->read_size);
		driver_free_binary(desc->read_binp);
		desc->read_offset = 0;
		desc->read_binp = binp;
	    }
	} 
	if (! (d = EF_ALLOC(sizeof(struct t_data)))) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.read.binp = desc->read_binp;
	d->c.read.bin_offset = desc->read_offset + desc->read_size;
	d->c.read.bin_size = desc->read_binp->orig_size - d->c.read.bin_offset;
	d->c.read.size = size;
	driver_binary_inc_refc(d->c.read.binp);
	d->invoke = invoke_read;
	d->free = free_read;
	d->level = 1;
	cq_enq(desc, d);
    } goto done; /* case FILE_READ: */

    case FILE_READ_LINE: {
	/*
	 * Icky little creature... We do mostly as ordinary file read, but with a few differences.
	 * 1) We have to scan for proper newline sequence if there is a buffer already, we cannot know 
	 *    in advance if the buffer contains a whole line without scanning.
	 * 2) We do not know how large the buffer needs to be in advance. We give a default buffer,
	 *    but the worker may need to allocate a new one. Freeing the old and rereferencing a newly 
	 *    allocated binary + dealing with offsets and lengts are done in file_async ready
	 *    for this OP.
	 */
	struct t_data *d;
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size != 1) {
	    /* Wrong command length */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if ((desc->fd == FILE_FD_INVALID)
	    || (! (desc->flags & EFILE_MODE_READ)) ) {
	    reply_posix_error(desc, EBADF);
	    goto done;
	}
	if (desc->read_size > 0) {
	    /* look for '\n' in what we'we already got */
	    void *nl_ptr = memchr(desc->read_binp->orig_bytes + desc->read_offset,'\n',desc->read_size);
	    if (nl_ptr != NULL) {
		/* If found, we're done */
		int skip = 0;
		size_t size = ((char *) nl_ptr) - 
		    ((char *) (desc->read_binp->orig_bytes + desc->read_offset)) + 1;
		if (size > 1 &&
		    *(((char *) nl_ptr) - 1) == '\r') {
		    *(((char *) nl_ptr) - 1) = '\n';		    
		    skip = 1;
		    --size;
		} 
		reply_data(desc, desc->read_binp, desc->read_offset, size);
		desc->read_offset += (size + skip);
		desc->read_size -= (size + skip);
		try_free_read_bin(desc);
		goto done;
	    }
	}
	/* Now, it's up to the thread to work out the need for more buffers and such, it's
	   no use doing it in this thread as we do not have the information required anyway. 
	   Even a NULL buffer could be handled by the thread, but code is simplified by us 
	   allocating it */
	if (! desc->read_binp) {
	    int alloc_size = (desc->read_bufsize > DEFAULT_LINEBUF_SIZE) ? desc->read_bufsize : 
		DEFAULT_LINEBUF_SIZE;
	    /* Allocate a new binary for the result */
	    if (! (desc->read_binp = driver_alloc_binary(alloc_size))) {
		reply_posix_error(desc, ENOMEM);
		goto done;
	    }
	}	
	if (! (d = EF_ALLOC(sizeof(struct t_data)))) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}

	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.read_line.binp = desc->read_binp;
	d->c.read_line.read_offset = desc->read_offset;
	d->c.read_line.read_size = desc->read_size;
#if !ALWAYS_READ_LINE_AHEAD
	d->c.read_line.read_ahead = (desc->read_bufsize > 0);
#endif 
	driver_binary_inc_refc(d->c.read.binp);
	d->invoke = invoke_read_line;
	d->free = free_read_line;
	d->level = 1;
	cq_enq(desc, d);
    } goto done;
    case FILE_WRITE: {
	int skip = 1;
	int size = ev->size - skip;
	if (lseek_flush_read(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (! (desc->flags & EFILE_MODE_WRITE)) {
	    reply_posix_error(desc, EBADF);
	    goto done;
	}
	if (size <= 0) {
	    reply_Uint(desc, size);
	    goto done;
	}
	MUTEX_LOCK(desc->q_mtx);
	if (driver_enqv(desc->port, ev, skip)) {
	    MUTEX_UNLOCK(desc->q_mtx);
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	desc->write_buffered += size;
	if (desc->write_buffered < desc->write_bufsize) {
	    MUTEX_UNLOCK(desc->q_mtx);
	    reply_Uint(desc, size);
	    if (desc->timer_state == timer_idle) {
		desc->timer_state = timer_write;
		driver_set_timer(desc->port, desc->write_delay);
	    }
	} else {
	    if (async_write(desc, &err, !0, size) != 0) {
		MUTEX_UNLOCK(desc->q_mtx);
		reply_posix_error(desc, err);
		goto done;
	    } else {
		MUTEX_UNLOCK(desc->q_mtx);
	    }
	}
    } goto done; /* case FILE_WRITE */

    case FILE_PWRITEV: {
	Uint32 i, j, n; 
	size_t total;
	struct t_data *d;
	if (lseek_flush_read(desc, &err) < 0) {
	    reply_Uint_posix_error(desc, 0, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_Uint_posix_error(desc, 0, err);
	    goto done;
	}
	if (ev->size < 1+4
	    || !EV_GET_UINT32(ev, &n, &p, &q)) {
	    /* Buffer too short to contain even the number of pos/size specs */
	    reply_Uint_posix_error(desc, 0, EINVAL);
	    goto done;
	}
	if (n == 0) {
	    /* Trivial case - nothing to write */
	    if (ev->size != 1+4) {
		reply_posix_error(desc, err);
	    } else {
		reply_Uint(desc, 0);
	    }
	    goto done;
	}
	if (ev->size < 1+4+8*(2*n)) {
	    /* Buffer too short to contain even the pos/size specs */
	    reply_Uint_posix_error(desc, 0, EINVAL);
	    goto done;
	}
	d = EF_ALLOC(sizeof(struct t_data) 
		     + (n * sizeof(struct t_pbuf_spec)));
	if (! d) {
	    reply_Uint_posix_error(desc, 0, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.pwritev.port = desc->port;
	d->c.pwritev.q_mtx = desc->q_mtx;
	d->c.pwritev.n = n;
	d->c.pwritev.cnt = 0;
	total = 0;
	j = 0;
	/* Create pos/size specs in the thread data structure
	 * for all non-zero size binaries. Calculate total size.
	 */
	for(i = 0; i < n; i++) {
	    Uint32 sizeH, sizeL;
	    size_t size;
	    if (   !EV_GET_UINT64(ev, &d->c.pwritev.specs[i].offset, &p, &q)
		|| !EV_GET_UINT32(ev, &sizeH, &p, &q)
		|| !EV_GET_UINT32(ev, &sizeL, &p, &q)) {
		/* Misalignment in buffer */
		reply_Uint_posix_error(desc, 0, EINVAL);
		EF_FREE(d);
		goto done;
	    }
#if SIZEOF_SIZE_T == 4
	    if (sizeH != 0) {
		reply_Uint_posix_error(desc, 0, EINVAL);
		EF_FREE(d);
		goto done;
	    }
	    size = sizeL;
#else
	    size = ((size_t)sizeH<<32) | sizeL;
#endif
	    if (size > 0) {
		total += size;
		d->c.pwritev.specs[j].size = size;
		j++;
	    }
	}
	d->c.pwritev.size = total;
	d->c.pwritev.free_size = 0;
	if (j == 0) {
	    /* Trivial case - nothing to write */
	    EF_FREE(d);
	    reply_Uint(desc, 0);
	} else {
	    size_t skip = 1 + 4 + 8*(2*n);
	    if (skip + total != ev->size) {
		/* Actual amount of data does not match 
		 * total of all pos/size specs
		 */
		EF_FREE(d);
		reply_Uint_posix_error(desc, 0, EINVAL);
	    } else {
		/* Enqueue the data */
		MUTEX_LOCK(desc->q_mtx);
		driver_enqv(desc->port, ev, skip);
		MUTEX_UNLOCK(desc->q_mtx);
		/* Execute the command */
		d->invoke = invoke_pwritev;
		d->free = free_pwritev;
		d->level = 1;
		cq_enq(desc, d);
	    }
	}
    } goto done; /* case FILE_PWRITEV: */

    case FILE_PREADV: {
	register void * void_ptr;
	Uint32 i, n;
	struct t_data *d;
	ErlIOVec *res_ev;
	if (lseek_flush_read(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size < 1+8
	    || !EV_GET_UINT32(ev, &n, &p, &q)
	    || !EV_GET_UINT32(ev, &n, &p, &q)) {
	    /* Buffer too short to contain even the number of pos/size specs */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if (ev->size != 1+8+8*(2*n)) {
	    /* Buffer wrong length to contain the pos/size specs */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	/* Create the thread data structure with the contained ErlIOVec 
	 * and corresponding binaries for the response 
	 */
	d = EF_ALLOC(sizeof(*d)
		     + (n * sizeof(*d->c.preadv.offsets))
		     + ((1+n) * (sizeof(*res_ev->iov)
				 + sizeof(*res_ev->binv))));
	if (! d) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.preadv.n = n;
	d->c.preadv.cnt = 0;
	d->c.preadv.size = 0;
	res_ev = &d->c.preadv.eiov;
	/* XXX possible alignment problems here for weird machines */
	res_ev->vsize = 1+d->c.preadv.n;
	res_ev->iov = void_ptr = &d->c.preadv.offsets[d->c.preadv.n];
	res_ev->binv = void_ptr = &res_ev->iov[res_ev->vsize];
	/* Read in the pos/size specs and allocate binaries for the results */
	for (i = 1; i < 1+n; i++) {
	    Uint32 sizeH, sizeL;
	    size_t size;
	    if (   !EV_GET_UINT64(ev, &d->c.preadv.offsets[i-1], &p, &q)
		|| !EV_GET_UINT32(ev, &sizeH, &p, &q)
		|| !EV_GET_UINT32(ev, &sizeL, &p, &q)) {
		reply_posix_error(desc, EINVAL);
		break;
	    }
#if SIZEOF_SIZE_T == 4
	    if (sizeH != 0) {
		reply_posix_error(desc, EINVAL);
		break;
	    }
	    size = sizeL;
#else
	    size = ((size_t)sizeH<<32) | sizeL;
#endif
	    if (! (res_ev->binv[i] = driver_alloc_binary(size))) {
		reply_posix_error(desc, ENOMEM);
		break;
	    } else {
		res_ev->iov[i].iov_len  = size;
		res_ev->iov[i].iov_base = res_ev->binv[i]->orig_bytes;
	    }
	}
	if (i < 1+n) {
	    for (i--; i > 0; i--) {
		driver_free_binary(res_ev->binv[i]);
	    }
	    EF_FREE(d);
	    goto done;
	}
	/* Allocate the header binary (index 0) */
	res_ev->binv[0] = driver_alloc_binary(4+4+8*n);
	if (! res_ev->binv[0]) {
	    reply_posix_error(desc, ENOMEM);
	    for (i = 1; i < 1+n; i++) {
		driver_free_binary(res_ev->binv[i]);
	    }
	    EF_FREE(d);
	    goto done;
	}
	res_ev->iov[0].iov_len = 4+4+8*n;
	res_ev->iov[0].iov_base = res_ev->binv[0]->orig_bytes;
	/* Fill in the number of buffers in the header */
	put_int32(0, res_ev->iov[0].iov_base);
	put_int32(n, res_ev->iov[0].iov_base+4);
	/**/
	res_ev->size = res_ev->iov[0].iov_len;
	if (n == 0) {
	    /* Trivial case - nothing to read */
	    reply_ev(desc, FILE_RESP_LDATA, res_ev);
	    free_preadv(d);
	    goto done;
	} else {
	    d->invoke = invoke_preadv;
	    d->free = free_preadv;
	    d->level = 1;
	    cq_enq(desc, d);
	}
    } goto done; /* case FILE_PREADV: */

    case FILE_LSEEK: {
	Sint64 offset;          /* Offset for seek */
	Uint32 origin;		/* Origin of seek. */
	if (lseek_flush_read(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size != 1+8+4
	    || !EV_GET_UINT64(ev, &offset, &p, &q)
	    || !EV_GET_UINT32(ev, &origin, &p, &q)) {
	    /* Wrong length of buffer to contain offset and origin */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if (async_lseek(desc, &err, !0, offset, origin) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
    } goto done;

    case FILE_READ_FILE: {
	struct t_data *d;
	if (ev->size < 1+1) {
	    /* Buffer contains empty name */
	    reply_posix_error(desc, ENOENT);
	    goto done;
	}
	if (ev->size-1 != ev->iov[q].iov_len-p) {
	    /* Name not in one single buffer */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	d = EF_ALLOC(sizeof(struct t_data) + ev->size);
	if (! d) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	/* Copy name */
	memcpy(d->c.read_file.name, EV_CHAR_P(ev, p, q), ev->size-1);
	d->c.read_file.name[ev->size-1] = '\0';
	d->c.read_file.binp = NULL;
	d->invoke = invoke_read_file;
	d->free = free_read_file;
	d->level = 2;
	cq_enq(desc, d);
    } goto done;

    case FILE_IPREAD: {
	/* This operation cheets by using invoke_preadv() and free_preadv()
	 * plus its own invoke_ipread. Therefore the result format is 
	 * a bit awkward - the header binary contains one extra 64 bit
	 * field that invoke_preadv() fortunately ignores,
	 * and the first 64 bit field does not contain the number of 
	 * data binaries which invoke_preadv() also ignores.
	 */
	register void * void_ptr;
	char mode;
	Sint64 hdr_offset;
	Uint32 max_size;
	struct t_data *d;
	ErlIOVec *res_ev;
	int vsize;
	if (! EV_GET_CHAR(ev, &mode, &p, &q)) {
	    /* Empty command */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if (mode != IPREAD_S32BU_P32BU) {
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if (lseek_flush_read(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size < 1+1+8+4
	    || !EV_GET_UINT64(ev, &hdr_offset, &p, &q)
	    || !EV_GET_UINT32(ev, &max_size, &p, &q)) {
	    /* Buffer too short to contain 
	     * the header offset and max size spec */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	/* Create the thread data structure with the contained ErlIOVec 
	 * and corresponding binaries for the response 
	 */
	vsize = 2;
	d = EF_ALLOC(sizeof(*d) + 
		     vsize*(sizeof(*res_ev->iov) + sizeof(*res_ev->binv)));
	if (! d) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.preadv.offsets[0] = hdr_offset;
	d->c.preadv.size = max_size;
	res_ev = &d->c.preadv.eiov;
	/* XXX possible alignment problems here for weird machines */
	res_ev->iov = void_ptr = d + 1;
	res_ev->binv = void_ptr = res_ev->iov + vsize;
	res_ev->size = 0;
	res_ev->vsize = 0;
	d->invoke = invoke_ipread;
	d->free = free_preadv;
	d->level = 1;
	cq_enq(desc, d);
    } goto done; /* case FILE_IPREAD: */

    case FILE_SETOPT: {
	char opt;
	if (ev->size < 1+1
	    || !EV_GET_CHAR(ev, &opt, &p, &q)) {
	    /* Buffer too short to contain even the option type */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	switch (opt) {
	case FILE_OPT_DELAYED_WRITE: {
	    Uint32 sizeH, sizeL, delayH, delayL;
	    if (ev->size != 1+1+4*sizeof(Uint32)
		|| !EV_GET_UINT32(ev, &sizeH, &p, &q)
		|| !EV_GET_UINT32(ev, &sizeL, &p, &q)
		|| !EV_GET_UINT32(ev, &delayH, &p, &q)
		|| !EV_GET_UINT32(ev, &delayL, &p, &q)) {
		/* Buffer has wrong length to contain the option values */
		reply_posix_error(desc, EINVAL);
		goto done;
	    }
#if SIZEOF_SIZE_T == 4
	    if (sizeH != 0) {
		reply_posix_error(desc, EINVAL);
		goto done;
	    }
	    desc->write_bufsize = sizeL;
#else
	    desc->write_bufsize = ((size_t)sizeH << 32) | sizeL;
#endif
#if SIZEOF_LONG == 4
	    if (delayH != 0) {
		reply_posix_error(desc, EINVAL);
		goto done;
	    }
	    desc->write_delay = delayL;
#else
	    desc->write_delay = ((unsigned long)delayH << 32) | delayL;
#endif
	    TRACE_C('K');
	    reply_ok(desc);
	} goto done;
	case FILE_OPT_READ_AHEAD: {
	    Uint32 sizeH, sizeL;
	    if (ev->size != 1+1+2*sizeof(Uint32)
		|| !EV_GET_UINT32(ev, &sizeH, &p, &q)
		|| !EV_GET_UINT32(ev, &sizeL, &p, &q)) {
		/* Buffer has wrong length to contain the option values */
		reply_posix_error(desc, EINVAL);
		goto done;
	    }
#if SIZEOF_SIZE_T == 4
	    if (sizeH != 0) {
		reply_posix_error(desc, EINVAL);
		goto done;
	    }
	    desc->read_bufsize = sizeL;
#else
	    desc->read_bufsize = ((size_t)sizeH << 32) | sizeL;
#endif
	    TRACE_C('K');
	    reply_ok(desc);
	} goto done;
	default:
	    reply_posix_error(desc, EINVAL);
	    goto done;
	} /* case FILE_OPT_DELAYED_WRITE: */
    } ASSERT(0); goto done; /* case FILE_SETOPT: */
    
    } /* switch(command) */
    
    if (lseek_flush_read(desc, &err) < 0) {
	reply_posix_error(desc, err);
	goto done;
    }
    if (flush_write_check_error(desc, &err) < 0) {
	reply_posix_error(desc, err);
	goto done;
    } else {
	/* Flatten buffer and send it to file_output(desc, buf, len) */
	int len = ev->size;
	char *buf = EF_ALLOC(len);
	if (! buf) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	driver_vec_to_buf(ev, buf, len);
	file_output((ErlDrvData) desc, buf, len);
	EF_FREE(buf);
	goto done;
    }

 done:
    cq_execute(desc);
}
