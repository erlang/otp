/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#define FILE_SENDFILE           32
#define FILE_FALLOCATE          33
#define FILE_CLOSE_ON_PORT_EXIT 34
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
#define FILE_RESP_FNAME      9
#define FILE_RESP_ALL_DATA  10
#define FILE_RESP_LFNAME    11

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

#include <ctype.h>
#include <sys/types.h>
#include <stdlib.h>

/* Need (NON)BLOCKING macros for sendfile */
#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif

#include "sys.h"

#include "erl_driver.h"
#include "erl_efile.h"
#include "erl_threads.h"
#include "gzio.h"
#include "dtrace-wrapper.h" 


static ErlDrvSysInfo sys_info;

/* For explanation of this var, see comment for same var in erl_async.c */
static unsigned gcc_optimizer_hack = 0;

#ifdef  USE_VM_PROBES

#define DTRACE_EFILE_BUFSIZ 128

#define DTRACE_INVOKE_SETUP(op) \
    do { DTRACE3(efile_drv_int_entry, d->sched_i1, d->sched_i2, op); } while (0)
#define DTRACE_INVOKE_SETUP_BY_NAME(op) \
    struct t_data *d = (struct t_data *) data ; \
    DTRACE_INVOKE_SETUP(op)
#define DTRACE_INVOKE_RETURN(op) \
    do { DTRACE3(efile_drv_int_return, d->sched_i1, d->sched_i2, \
                 op); } while (0) ; gcc_optimizer_hack++ ;

/* Assign human-friendlier id numbers to scheduler & I/O worker threads */
int             dt_driver_idnum = 0;
int             dt_driver_io_worker_base = 5000;
erts_mtx_t      dt_driver_mutex;
pthread_key_t   dt_driver_key;

typedef struct {
    int         thread_num;
    Uint64      tag;
} dt_private;

dt_private *get_dt_private(int);
#else  /* USE_VM_PROBES */
#define DTRACE_INVOKE_SETUP(op)            do {} while (0)
#define DTRACE_INVOKE_SETUP_BY_NAME(op)    do {} while (0)
#define DTRACE_INVOKE_RETURN(op)           do {} while (0)
#endif  /* USE_VM_PROBES */

/* #define TRACE 1 */
#ifdef TRACE
#    define TRACE_C(c) do { putchar(c); fflush(stdout); } while (0)
#    define TRACE_S(s) do { fputs((s), stdout); fflush(stdout); } while (0)
#    define TRACE_F(args) do { printf args ;fflush(stdout); } while (0)
#else
#    define TRACE_C(c) ((void)(0))
#    define TRACE_S(s) ((void)(0))
#    define TRACE_F(args) ((void)(0))
#endif


#ifdef USE_THREADS
#define THRDS_AVAILABLE (sys_info.async_threads > 0)
#ifdef HARDDEBUG /* HARDDEBUG in io.c is expected too */
#define TRACE_DRIVER fprintf(stderr, "Efile: ")
#else
#define TRACE_DRIVER
#endif
#define MUTEX_INIT(m, p) do { IF_THRDS { TRACE_DRIVER; (m = driver_pdl_create(p)); } } while (0)
#define MUTEX_LOCK(m)    do { IF_THRDS { TRACE_DRIVER; driver_pdl_lock(m);   } } while (0)
#define MUTEX_UNLOCK(m)  do { IF_THRDS { TRACE_DRIVER; driver_pdl_unlock(m); } } while (0)
#else
#define THRDS_AVAILABLE (0)
#define MUTEX_INIT(m, p)
#define MUTEX_LOCK(m)
#define MUTEX_UNLOCK(m)
#endif
#define IF_THRDS if (THRDS_AVAILABLE)


#define SENDFILE_FLGS_USE_THREADS (1 << 0)
/**
 * On DARWIN sendfile can deadlock with close if called in
 * different threads. So until Apple fixes so that sendfile
 * is not buggy we disable usage of the async pool for
 * DARWIN. The testcase t_sendfile_crashduring reproduces
 * this error when using +A 10 and enabling SENDFILE_FLGS_USE_THREADS.
 */
#if defined(__APPLE__) && defined(__MACH__)
#define USE_THRDS_FOR_SENDFILE(DATA) 0
#else
#define USE_THRDS_FOR_SENDFILE(DATA) (DATA->flags & SENDFILE_FLGS_USE_THREADS)
#endif /* defined(__APPLE__) && defined(__MACH__) */



#if 0
/* Experimental, for forcing all file operations to use the same thread. */
   static unsigned file_fixed_key = 1;
#  define KEY(desc) (&file_fixed_key)
#else
#  define KEY(desc) (&(desc)->key)
#endif

#ifndef MAX
#  define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#ifdef FILENAMES_16BIT
#ifdef USE_VM_PROBES
#error 16bit characters in filenames and dtrace in combination is not supported.
#endif
#  define FILENAME_BYTELEN(Str) filename_len_16bit(Str)
#  define FILENAME_COPY(To,From) filename_cpy_16bit((To),(From)) 
#  define FILENAME_CHARSIZE 2

   static int filename_len_16bit(char *str) 
   {
       char *p = str;
       while(*p != '\0' || p[1] != '\0') {
	   p += 2;
       }
       return (p - str);
   }

   static void filename_cpy_16bit(char *to, char *from) 
   {
       while(*from != '\0' || from[1] != '\0') {
	   *to++ = *from++;
	   *to++ = *from++;
       }
       *to++ = *from++;
       *to++ = *from++;
   }

#else
#  define FILENAME_BYTELEN(Str) strlen(Str)
#  define FILENAME_COPY(To,From) strcpy(To,From) 
#  define FILENAME_CHARSIZE 1
#endif

#if     (MAXPATHLEN+1)*FILENAME_CHARSIZE+1 > BUFSIZ
#  define    RESBUFSIZE  ((MAXPATHLEN+1)*FILENAME_CHARSIZE+1)
#else
#  define    RESBUFSIZE  BUFSIZ
#endif

#define READDIR_CHUNKS (5)



#if ALWAYS_READ_LINE_AHEAD
#define DEFAULT_LINEBUF_SIZE 2048
#else
#define DEFAULT_LINEBUF_SIZE 512 /* Small, it's usually discarded anyway */ 
#endif

typedef unsigned char uchar;

static ErlDrvData file_start(ErlDrvPort port, char* command);
static int file_init(void);
static void file_stop(ErlDrvData);
static void file_output(ErlDrvData, char* buf, ErlDrvSizeT len);
static ErlDrvSSizeT file_control(ErlDrvData, unsigned int command,
				 char* buf, ErlDrvSizeT len,
				 char **rbuf, ErlDrvSizeT rlen);
static void file_timeout(ErlDrvData);
static void file_outputv(ErlDrvData, ErlIOVec*);
static void file_async_ready(ErlDrvData, ErlDrvThreadData);
static void file_flush(ErlDrvData);

#ifdef HAVE_SENDFILE
static void file_ready_output(ErlDrvData data, ErlDrvEvent event);
static void file_stop_select(ErlDrvEvent event, void* _);
#endif /* HAVE_SENDFILE */


enum e_timer {timer_idle, timer_again, timer_write};
#ifdef HAVE_SENDFILE
enum e_sendfile {sending, not_sending};
#define SENDFILE_USE_THREADS (1 << 0)
#endif /* HAVE_SENDFILE */

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
#ifdef HAVE_SENDFILE
    enum e_sendfile sendfile_state;
#endif /* HAVE_SENDFILE */
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
#ifdef USE_VM_PROBES
    int             idnum;      /* Unique ID # for this driver thread/desc */
    char            port_str[DTRACE_TERM_BUF_SIZE];
#endif
} file_descriptor;


static int reply_error(file_descriptor*, Efile_error* errInfo);

struct erl_drv_entry efile_driver_entry = {
    file_init,
    file_start,
    file_stop,
    file_output,
    NULL,
#ifdef HAVE_SENDFILE
    file_ready_output,
#else
    NULL,
#endif /* HAVE_SENDFILE */
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
    NULL,
    NULL,
#ifdef HAVE_SENDFILE
    file_stop_select
#else
    NULL
#endif /* HAVE_SENDFILE */
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

#define READDIR_BUFSIZE (8*1024)*READDIR_CHUNKS
#if READDIR_BUFSIZE < (1 + (2 + MAXPATHLEN)*FILENAME_CHARSIZE*READDIR_CHUNKS)
#  undef READDIR_BUFSIZE
#  define READDIR_BUFSIZE (1 + (2 + MAXPATHLEN)*FILENAME_CHARSIZE*READDIR_CHUNKS)
#endif

struct t_readdir_buf {
     struct t_readdir_buf *next;
     size_t n;
     char buf[READDIR_BUFSIZE];
};

struct t_data
{
    struct t_data *next;
    int            command;
    int            level;
    void         (*invoke)(void *);
    void         (*free)(void *);
    void           *data_to_free; /* used by FILE_CLOSE_ON_PORT_EXIT only */
    int            again;
    int            reply;
#ifdef  USE_VM_PROBES
    int               sched_i1;
    Uint64            sched_i2;
    char              sched_utag[DTRACE_EFILE_BUFSIZ+1];
#endif
    int            result_ok;
    Efile_error    errInfo;
    int            flags;
    SWord          fd;
    int            is_fd_unused;
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
#ifdef HAVE_SENDFILE
	struct {
	    ErlDrvPort port;
	    ErlDrvPDL q_mtx;
	    int out_fd;
	    off_t offset;
	    Uint64 nbytes;
	    Uint64 written;
	} sendfile;
#endif /* HAVE_SENDFILE */
	struct {
	    Sint64 offset;
	    Sint64 length;
	} fallocate;
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
    if (!p) erts_exit(ERTS_ERROR_EXIT, "efile drv: Can't allocate %lu bytes of memory\n", (unsigned long)s);
    return p;
}

/*********************************************************************
 * ErlIOVec manipulation functions.
 */

/* char EV_CHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_CHAR_P(ev, p, q)			\
    (((char *)(ev)->iov[q].iov_base) + (p))

/* int EV_GET_CHAR(ErlIOVec *ev, char *p, int *pp, int *qp) */
#define EV_GET_CHAR(ev, p, pp, qp) efile_ev_get_char(ev, p ,pp, qp)
static int
efile_ev_get_char(ErlIOVec *ev, char *p, size_t *pp, size_t *qp) {
    if (*pp + 1 <= ev->iov[*qp].iov_len) {
	*p = *EV_CHAR_P(ev, *pp, *qp);
	if (*pp + 1 < ev->iov[*qp].iov_len)
	    *pp += 1;
	else {
	    *qp += 1;
	    *pp = 0;
	}
	return !0;
    }
    return 0;
}

/* Uint32 EV_UINT32(ErlIOVec *ev, int p, int q)*/
#define EV_UINT32(ev, p, q)						\
    ((Uint32) ((unsigned char *)(ev)->iov[q].iov_base)[p])

/* int EV_GET_UINT32(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT32(ev, p, pp, qp) efile_ev_get_uint32(ev, p, pp, qp)
static int
efile_ev_get_uint32(ErlIOVec *ev, Uint32 *p, size_t *pp, size_t *qp) {
    if (*pp + 4 <= ev->iov[*qp].iov_len) {
	*p = (EV_UINT32(ev, *pp,   *qp) << 24)
	    | (EV_UINT32(ev, *pp + 1, *qp) << 16)
	    | (EV_UINT32(ev, *pp + 2, *qp) << 8)
	    | (EV_UINT32(ev, *pp + 3, *qp));
	if (*pp + 4 < ev->iov[*qp].iov_len)
	    *pp += 4;
	else {
	    *qp += 1;
	    *pp = 0;
	}
	return !0;
    }
    return 0;
}

/* Uint64 EV_UINT64(ErlIOVec *ev, int p, int q)*/
#define EV_UINT64(ev, p, q)						\
    ((Uint64) ((unsigned char *)(ev)->iov[q].iov_base)[p])

/* int EV_GET_UINT64(ErlIOVec *ev, Uint64 *p, int *pp, int *qp) */
#define EV_GET_UINT64(ev, p, pp, qp) efile_ev_get_uint64(ev, p, pp, qp)
static int
efile_ev_get_uint64(ErlIOVec *ev, Uint64 *p, size_t *pp, size_t *qp) {
    if (*pp + 8 <= ev->iov[*qp].iov_len) {
	*p = (EV_UINT64(ev, *pp, *qp) << 56)
	    | (EV_UINT64(ev, *pp + 1, *qp) << 48)
	    | (EV_UINT64(ev, *pp + 2, *qp) << 40)
	    | (EV_UINT64(ev, *pp + 3, *qp) << 32)
	    | (EV_UINT64(ev, *pp + 4, *qp) << 24)
	    | (EV_UINT64(ev, *pp + 5, *qp) << 16)
	    | (EV_UINT64(ev, *pp + 6, *qp) << 8)
	    | (EV_UINT64(ev, *pp + 7, *qp));
	if (*pp + 8 < ev->iov[*qp].iov_len)
	    *pp += 8;
	else {
	    *qp += 1;
	    *pp = 0;
	}
	return !0;
    }
    return 0;
}

/* int EV_GET_SINT64(ErlIOVec *ev, Uint64 *p, int *pp, int *qp) */
#define EV_GET_SINT64(ev, p, pp, qp) efile_ev_get_sint64(ev, p, pp, qp)
static int
efile_ev_get_sint64(ErlIOVec *ev, Sint64 *p, size_t *pp, size_t *qp) {
    Uint64 *tmp = (Uint64*)p;
    return EV_GET_UINT64(ev, tmp, pp, qp);
}

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

    /* run initiation of efile_driver if needed */
    efile_init();

#ifdef  USE_VM_PROBES
    erts_mtx_init(&dt_driver_mutex, "efile_drv dtrace mutex");
    pthread_key_create(&dt_driver_key, NULL);
#endif  /* USE_VM_PROBES */

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
    desc->key = driver_async_port_key(port);
    desc->flags = 0;
    desc->invoke = NULL;
    desc->d = NULL;
    desc->free = NULL;
    desc->cq_head = NULL;
    desc->cq_tail = NULL;
    desc->timer_state = timer_idle;
#ifdef HAVE_SENDFILE
    desc->sendfile_state = not_sending;
#endif
    desc->read_bufsize = 0;
    desc->read_binp = NULL;
    desc->read_offset = 0;
    desc->read_size = 0;
    desc->write_delay = 0L;
    desc->write_bufsize = 0;
    desc->write_error = 0;
    MUTEX_INIT(desc->q_mtx, port); /* Refc is one, referenced by emulator now */
    desc->write_buffered = 0;
#ifdef  USE_VM_PROBES
    dtrace_drvport_str(port, desc->port_str);
    get_dt_private(0);           /* throw away return value */
#endif  /* USE_VM_PROBES */
    return (ErlDrvData) desc;
}

static void do_close(int flags, SWord fd) {
    if (flags & EFILE_COMPRESSED) {
	erts_gzclose((ErtsGzFile)(fd));
    } else {
	efile_closefile((int) fd);
    }
}

static void invoke_close(void *data)
{
    struct t_data *d = (struct t_data *) data;
    DTRACE_INVOKE_SETUP(FILE_CLOSE);
    d->again = 0;
    do_close(d->flags, d->fd);
    DTRACE_INVOKE_RETURN(FILE_CLOSE);
}

static void free_data(void *data)
{
    struct t_data *d = (struct t_data *) data;

    switch (d->command) {
    case FILE_OPEN:
        if (d->is_fd_unused && d->fd != FILE_FD_INVALID) {
            /* This is OK to do in scheduler thread because there can be no async op
               ongoing for this fd here, as we exited during async open.
               Ideally, this close should happen in an async thread too, but that would
               require a substantial rewrite, as we are here because of a dead port and
               cannot schedule async jobs for that port any more... */
            do_close(d->flags, d->fd);
        }
        break;
    case FILE_CLOSE_ON_PORT_EXIT:
        EF_FREE(d->data_to_free);
        break;
    }

    EF_FREE(data);
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
#if SIZEOF_VOID_P == 4
    put_int32(0, response+1);
#else
    put_int32(num>>32, response+1);
#endif
    put_int32((Uint32)num, response+1+4);
    for (s = erl_errno_id(posix_errno), t = response+1+4+4; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
}

#ifdef HAVE_SENDFILE
static void reply_string_error(file_descriptor *desc, char* str) {
    char response[256];		/* Response buffer. */
    char* s;
    char* t;

    response[0] = FILE_RESP_ERROR;
    for (s = str, t = response+1; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
}
#endif

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
#if SIZEOF_VOID_P == 4
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
    DTRACE_INVOKE_SETUP_BY_NAME(FILE_MKDIR);
    invoke_name(data, efile_mkdir);
    DTRACE_INVOKE_RETURN(FILE_MKDIR);
}

static void invoke_rmdir(void *data)
{
    DTRACE_INVOKE_SETUP_BY_NAME(FILE_RMDIR);
    invoke_name(data, efile_rmdir);
    DTRACE_INVOKE_RETURN(FILE_RMDIR);
}

static void invoke_delete_file(void *data)
{
    DTRACE_INVOKE_SETUP_BY_NAME(FILE_DELETE);
    invoke_name(data, efile_delete_file);
    DTRACE_INVOKE_RETURN(FILE_DELETE);
}

static void invoke_chdir(void *data)
{
    DTRACE_INVOKE_SETUP_BY_NAME(FILE_CHDIR);
    invoke_name(data, efile_chdir);
    DTRACE_INVOKE_RETURN(FILE_CHDIR);
}

static void invoke_fdatasync(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;
    DTRACE_INVOKE_SETUP(FILE_FDATASYNC);

    d->again = 0;
    d->result_ok = efile_fdatasync(&d->errInfo, fd);
    DTRACE_INVOKE_RETURN(FILE_FDATASYNC);
}

static void invoke_fsync(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;
    DTRACE_INVOKE_SETUP(FILE_FSYNC);

    d->again = 0;
    d->result_ok = efile_fsync(&d->errInfo, fd);
    DTRACE_INVOKE_RETURN(FILE_FSYNC);
}

static void invoke_truncate(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;
    DTRACE_INVOKE_SETUP(FILE_TRUNCATE);

    d->again = 0;
    d->result_ok = efile_truncate_file(&d->errInfo, &fd, d->flags);
    DTRACE_INVOKE_RETURN(FILE_TRUNCATE);
}

static void invoke_read(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status, segment;
    size_t size, read_size;
    DTRACE_INVOKE_SETUP(FILE_READ);

    segment = d->again && d->c.read.bin_size >= 2*FILE_SEGMENT_READ;
    if (segment) {
	size = FILE_SEGMENT_READ;
    } else {
	size = d->c.read.bin_size;
    }
    read_size = size;
    if (d->flags & EFILE_COMPRESSED) {
	read_size = erts_gzread((ErtsGzFile)d->fd,
				d->c.read.binp->orig_bytes + d->c.read.bin_offset,
				size);
	status = (read_size != (size_t) -1);
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
    DTRACE_INVOKE_RETURN(FILE_READ);
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
    size_t read_size = 0;
    int local_loop = (d->again == 0);
    DTRACE_INVOKE_SETUP(FILE_READ_LINE);

    do {
	size_t size = (d->c.read_line.binp)->orig_size - 
	    d->c.read_line.read_offset - d->c.read_line.read_size;
	if (size == 0) {
	    /* Need more place */
	    ErlDrvSizeT need = (d->c.read_line.read_size >= DEFAULT_LINEBUF_SIZE) ?
		d->c.read_line.read_size + DEFAULT_LINEBUF_SIZE : DEFAULT_LINEBUF_SIZE;
	    ErlDrvBinary   *newbin;
#if !ALWAYS_READ_LINE_AHEAD
	    /* Use read_ahead size if need does not exceed it */
	    if (need < (d->c.read_line.binp)->orig_size && 
		d->c.read_line.read_ahead)
	      need = (d->c.read_line.binp)->orig_size;
#endif
	    newbin = driver_alloc_binary(need);
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
	    read_size = erts_gzread((ErtsGzFile)d->fd,
				    d->c.read_line.binp->orig_bytes + 
				    d->c.read_line.read_offset + d->c.read_line.read_size,
				    size);
	    status = (read_size != (size_t) -1);
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
			Sint64 location = erts_gzseek((ErtsGzFile)d->fd,
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
    DTRACE_INVOKE_RETURN(FILE_READ_LINE);
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
    DTRACE_INVOKE_SETUP(FILE_READ_FILE);
    
    if (! d->c.read_file.binp) { /* First invocation only */
	int fd;
	Sint64 size;
	
	if (! (d->result_ok = 
	       efile_openfile(&d->errInfo, d->b, 
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
	if (chop) goto chop_done; /* again */
    }
 close:
    efile_closefile((int) d->fd);
 done:
    d->again = 0;
 chop_done:
    DTRACE_INVOKE_RETURN(FILE_READ_FILE);
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
    DTRACE_INVOKE_SETUP(FILE_PREADV);

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
			    ((char *)ev->iov[1 + c->cnt].iov_base) + c->size,
			    read_size,
			    &bytes_read))) {
	    bytes_read_so_far += bytes_read;
	    if (chop && bytes_read == read_size) {
		c->size += bytes_read;
		goto done;
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
		goto done;
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
 done:
    DTRACE_INVOKE_RETURN(FILE_PREADV);
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
    DTRACE_INVOKE_SETUP(FILE_IPREAD);
    
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
    DTRACE_INVOKE_RETURN(FILE_IPREAD);
    return;
 error:
    d->result_ok = 0;
    d->again = 0;
    DTRACE_INVOKE_RETURN(FILE_IPREAD);
    return;
 done:
    d->result_ok = !0;
    d->again = 0;
    DTRACE_INVOKE_RETURN(FILE_IPREAD);
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
    DTRACE_INVOKE_SETUP(FILE_WRITE);

    segment = d->again && d->c.writev.size >= 2*FILE_SEGMENT_WRITE;
    if (segment) {
	size = FILE_SEGMENT_WRITE;
    } else {
	size = d->c.writev.size;
    }

    /* Copy the io vector to avoid locking the port que while writing,
     * also, both we and efile_writev might/will change the SysIOVec
     * when segmenting or due to partial write and we do not want to
     * tamper with the actual queue that we get from driver_peekq
     */
    MUTEX_LOCK(d->c.writev.q_mtx); /* Lock before accessing the port queue */
    iov0 = driver_peekq(d->c.writev.port, &iovlen);

    /* Calculate iovcnt */
    for (p = 0, iovcnt = 0;
	 p < size && iovcnt < iovlen;
	 p += iov0[iovcnt++].iov_len)
	;
    iov = EF_SAFE_ALLOC(sizeof(SysIOVec)*iovcnt);
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
		    status = erts_gzwrite((ErtsGzFile)d->fd,
					  iov[i].iov_base,
					  iov[i].iov_len) == iov[i].iov_len;
		    if (! status) {
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
					iov, iovcnt);
	}
    } else if (iovlen == 0) {
	d->result_ok = 1;
    }
    else { /* Port has terminated */
	d->result_ok = 0;
	d->errInfo.posix_errno = d->errInfo.os_errno = EINVAL;
    }
    EF_FREE(iov);

    if (! d->result_ok) {
	d->again = 0;
	MUTEX_LOCK(d->c.writev.q_mtx);
	driver_deq(d->c.writev.port, d->c.writev.size);
	MUTEX_UNLOCK(d->c.writev.q_mtx);
    } else {
	if (! segment) {
	    d->again = 0;
	}
	d->c.writev.size -= size;
	TRACE_F(("w%lu", (unsigned long)size));
	MUTEX_LOCK(d->c.writev.q_mtx);
	driver_deq(d->c.writev.port, size);
	MUTEX_UNLOCK(d->c.writev.q_mtx);
    }


    DTRACE_INVOKE_RETURN(FILE_WRITE);
}

static void invoke_pwd(void *data)
{
    struct t_data *d = (struct t_data *) data;
    DTRACE_INVOKE_SETUP(FILE_PWD);

    d->again = 0;
    d->result_ok = efile_getdcwd(&d->errInfo,d->drive, d->b+1,
				 RESBUFSIZE-1);
    DTRACE_INVOKE_RETURN(FILE_PWD);
}

static void invoke_readlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char resbuf[RESBUFSIZE];	/* Result buffer. */
    DTRACE_INVOKE_SETUP(FILE_READLINK);

    d->again = 0;
    d->result_ok = efile_readlink(&d->errInfo, d->b, resbuf+1,
				  RESBUFSIZE-1);
    if (d->result_ok != 0)
	FILENAME_COPY((char *) d->b + 1, resbuf+1);
    DTRACE_INVOKE_RETURN(FILE_READLINK);
}

static void invoke_altname(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char resbuf[RESBUFSIZE];	/* Result buffer. */
    DTRACE_INVOKE_SETUP(FILE_ALTNAME);

    d->again = 0;
    d->result_ok = efile_altname(&d->errInfo, d->b, resbuf+1,
				  RESBUFSIZE-1);
    if (d->result_ok != 0)
	FILENAME_COPY((char *) d->b + 1, resbuf+1);
    DTRACE_INVOKE_RETURN(FILE_ALTNAME);
}

static void invoke_pwritev(void *data) {
    struct t_data* const d = (struct t_data *) data;
    struct t_pwritev * const c = &d->c.pwritev;
    SysIOVec         *iov0;
    SysIOVec         *iov;
    int               iovlen;
    int               iovcnt;
    size_t            p;
    int               segment;
    size_t            size, write_size, written;
    DTRACE_INVOKE_SETUP(FILE_PWRITEV);

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
    iov = EF_SAFE_ALLOC(sizeof(SysIOVec)*iovlen);
    memcpy(iov,iov0,sizeof(SysIOVec)*iovlen);
    MUTEX_UNLOCK(c->q_mtx);

    if (iovlen < 0)
	goto error; /* Port terminated */
    for (iovcnt = 0, written = 0;
	 c->cnt < c->n && iovcnt < iovlen && written < size;
	 c->cnt++) {
	int chop;
	write_size = c->specs[c->cnt].size;
	if (iov[iovcnt].iov_len - p < write_size) {
	    goto error;
	}
	chop = segment && written + write_size >= 2*FILE_SEGMENT_WRITE;
	if (chop) {
	    ASSERT(written < FILE_SEGMENT_WRITE);
	    write_size = FILE_SEGMENT_WRITE + FILE_SEGMENT_WRITE/2 
		- written;
	}
	d->result_ok = efile_pwrite(&d->errInfo, (int) d->fd,
				    (char *)(iov[iovcnt].iov_base) + p,
				    write_size,
				    c->specs[c->cnt].offset);
	if (! d->result_ok) {
	    d->again = 0;
	    goto deq_error;
	}
	written += write_size; 
	c->size -= write_size;
	if (chop) { 
	    c->specs[c->cnt].offset += write_size;
	    c->specs[c->cnt].size -= write_size;
	    /* Schedule out (d->again != 0) */
	    break;
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
	deq_error:
	    MUTEX_LOCK(c->q_mtx);
	    driver_deq(c->port, c->size);
	    MUTEX_UNLOCK(c->q_mtx);

	    goto done;
	} else {
	    ASSERT(written == size);
	    d->again = 0;
	}
    } else {
      ASSERT(written >= FILE_SEGMENT_WRITE);
    }
      
    MUTEX_LOCK(c->q_mtx);
    driver_deq(c->port, written);
    MUTEX_UNLOCK(c->q_mtx);
 done:
    EF_FREE(iov); /* Free our copy of the vector, nothing to restore */
    
    DTRACE_INVOKE_RETURN(FILE_PWRITEV);
}

static void invoke_flstat(void *data)
{
    struct t_data *d = (struct t_data *) data;

    DTRACE3(efile_drv_int_entry, d->sched_i1, d->sched_i2,
            d->command == FILE_LSTAT ? FILE_LSTAT : FILE_FSTAT);
    d->again = 0;
    d->result_ok = efile_fileinfo(&d->errInfo, &d->info,
				  d->b, d->command == FILE_LSTAT);
    DTRACE3(efile_drv_int_entry, d->sched_i1, d->sched_i2,
            d->command == FILE_LSTAT ? FILE_LSTAT : FILE_FSTAT);
    gcc_optimizer_hack++;
}

static void invoke_link(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;
    DTRACE_INVOKE_SETUP(FILE_LINK);

    d->again = 0;
    new_name = name+FILENAME_BYTELEN(name)+FILENAME_CHARSIZE;
    d->result_ok = efile_link(&d->errInfo, name, new_name);
    DTRACE_INVOKE_RETURN(FILE_LINK);
}

static void invoke_symlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;
    DTRACE_INVOKE_SETUP(FILE_SYMLINK);

    d->again = 0;
    new_name = name+FILENAME_BYTELEN(name)+FILENAME_CHARSIZE;
    d->result_ok = efile_symlink(&d->errInfo, name, new_name);
    DTRACE_INVOKE_RETURN(FILE_SYMLINK);
}

static void invoke_rename(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;
    DTRACE_INVOKE_SETUP(FILE_RENAME);

    d->again = 0;
    new_name = name+FILENAME_BYTELEN(name)+FILENAME_CHARSIZE;
    d->result_ok = efile_rename(&d->errInfo, name, new_name);
    DTRACE_INVOKE_RETURN(FILE_RENAME);
}

static void invoke_write_info(void *data)
{
    struct t_data *d = (struct t_data *) data;
    DTRACE_INVOKE_SETUP(FILE_WRITE_INFO);

    d->again = 0;
    d->result_ok = efile_write_info(&d->errInfo, &d->info, d->b);
    DTRACE_INVOKE_RETURN(FILE_WRITE_INFO);
}

static void invoke_lseek(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status;
    DTRACE_INVOKE_SETUP(FILE_LSEEK);

    d->again = 0;
    if (d->flags & EFILE_COMPRESSED) {
	int offset = (int) d->c.lseek.offset;
	
	if (offset != d->c.lseek.offset) {
	    d->errInfo.posix_errno = EINVAL;
	    status = 0;
	} else {
	    d->c.lseek.location = erts_gzseek((ErtsGzFile)d->fd,
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
    DTRACE_INVOKE_RETURN(FILE_LSEEK);
}

static void invoke_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *p = NULL;
    size_t file_bs;
    size_t n = 0, total = 0;
    struct t_readdir_buf *b = NULL;
    int res = 0;
    DTRACE_INVOKE_SETUP(FILE_READDIR);

    d->again = 0;
    d->errInfo.posix_errno = 0;

    do {
	total   = READDIR_BUFSIZE;
	n       = 1;
	b       = EF_SAFE_ALLOC(sizeof(struct t_readdir_buf));
	b->next = NULL;
	
	if (d->c.read_dir.last_buf) {
	    d->c.read_dir.last_buf->next = b;
	} else {
	    d->c.read_dir.first_buf = b;
	}
	d->c.read_dir.last_buf = b;

	p       = &b->buf[0];
	p[0]    = FILE_RESP_LFNAME;
	file_bs = READDIR_BUFSIZE - n;

	do {
	    res = efile_readdir(&d->errInfo, d->b, &d->dir_handle, p + n + 2, &file_bs);

	    if (res) {
		put_int16((Uint16)file_bs, p + n);
		n += 2 + file_bs;
		file_bs = READDIR_BUFSIZE - n;
	    }
	} while( res && ((total - n - 2) >= MAXPATHLEN*FILENAME_CHARSIZE));

	b->n = n;
    } while(res);

    d->result_ok = (d->errInfo.posix_errno == 0);
    DTRACE_INVOKE_RETURN(FILE_READDIR);
}

static void invoke_open(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status = 1;		/* Status of open call. */
    DTRACE_INVOKE_SETUP(FILE_OPEN);

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
		if ((ErtsGzFile)d->fd) {
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
    if (!status) {
        d->fd = FILE_FD_INVALID;
    }
    DTRACE_INVOKE_RETURN(FILE_OPEN);
}

static void invoke_fadvise(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;
    off_t offset = (off_t) d->c.fadvise.offset;
    off_t length = (off_t) d->c.fadvise.length;
    int advise = (int) d->c.fadvise.advise;
    DTRACE_INVOKE_SETUP(FILE_FADVISE);

    d->again = 0;
    d->result_ok = efile_fadvise(&d->errInfo, fd, offset, length, advise);
    DTRACE_INVOKE_RETURN(FILE_FADVISE);
}

#ifdef HAVE_SENDFILE
static void invoke_sendfile(void *data)
{
    struct t_data *d = (struct t_data *)data;
    int fd = d->fd;
    int out_fd = (int)d->c.sendfile.out_fd;
    Uint64 nbytes = d->c.sendfile.nbytes;
    int result = 0;
    d->again = 0;

    result = efile_sendfile(&d->errInfo, fd, out_fd, &d->c.sendfile.offset, &nbytes, NULL);

    d->c.sendfile.written += nbytes;

    if (result == 1 || (result == 0 && USE_THRDS_FOR_SENDFILE(d))) {
      d->result_ok = 0;
    } else if (result == 0 && (d->errInfo.posix_errno == EAGAIN
				 || d->errInfo.posix_errno == EINTR)) {
      if ((d->c.sendfile.nbytes - nbytes) != 0) {
	d->result_ok = 1;
	if (d->c.sendfile.nbytes != 0)
	  d->c.sendfile.nbytes -= nbytes;
      } else if (nbytes == 0 && d->c.sendfile.nbytes == 0) {
	d->result_ok = 1;
      } else
	d->result_ok = 0;
    } else {
	d->result_ok = -1;
    }
}

static void free_sendfile(void *data) {
    struct t_data *d = (struct t_data *)data;
    if (USE_THRDS_FOR_SENDFILE(d)) {
	SET_NONBLOCKING(d->c.sendfile.out_fd);
    } else {
	MUTEX_LOCK(d->c.sendfile.q_mtx);
	driver_deq(d->c.sendfile.port,1);
	MUTEX_UNLOCK(d->c.sendfile.q_mtx);
	driver_select(d->c.sendfile.port, (ErlDrvEvent)(long)d->c.sendfile.out_fd, ERL_DRV_USE_NO_CALLBACK|ERL_DRV_WRITE, 0);
    }
    EF_FREE(data);
}

static void file_ready_output(ErlDrvData data, ErlDrvEvent event)
{
    file_descriptor* fd = (file_descriptor*) data;

    switch (fd->d->command) {
    case FILE_SENDFILE:
	driver_select(fd->d->c.sendfile.port, event,
		      (int)ERL_DRV_WRITE,(int) 0);
	invoke_sendfile((void *)fd->d);
	file_async_ready(data, (ErlDrvThreadData)fd->d);
	break;
    default:
	break;
    }
}

static void file_stop_select(ErlDrvEvent event, void* _)
{

}

static int flush_sendfile(file_descriptor *desc,void *_) {
    if (desc->sendfile_state == sending) {
	desc->d->result_ok = -1;
	desc->d->errInfo.posix_errno = ECONNABORTED;
	file_async_ready((ErlDrvData)desc,(ErlDrvThreadData)desc->d);
    }
    return 1;
}
#endif /* HAVE_SENDFILE */


static void invoke_fallocate(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = (int) d->fd;
    Sint64 offset = d->c.fallocate.offset;
    Sint64 length = d->c.fallocate.length;

    d->again = 0;
    d->result_ok = efile_fallocate(&d->errInfo, fd, offset, length);
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
    if (! d->again)
	return 0;
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
#ifdef HAVE_SENDFILE
    if (desc->sendfile_state == sending)
	return;
#endif
    if (! (d = cq_deq(desc)))
	return;
    TRACE_F(("x%i", (int) d->command));
    d->again = sys_info.async_threads == 0;
    DRIVER_ASYNC(d->level, desc, d->invoke, void_ptr=d, d->free);
}

static struct t_data *async_write(file_descriptor *desc, int *errp,
		       int reply, Uint32 reply_size
#ifdef USE_VM_PROBES
		       ,Sint64 *dt_i1, Sint64 *dt_i2, Sint64 *dt_i3
#endif
) {
    struct t_data *d;
    if (! (d = EF_ALLOC(sizeof(struct t_data) - 1))) {
	if (errp) *errp = ENOMEM;
	return NULL;
    }
    TRACE_F(("w%lu", (unsigned long)desc->write_buffered));
    d->command = FILE_WRITE;
    d->fd = desc->fd;
    d->flags = desc->flags;
    d->c.writev.port = desc->port;
    d->c.writev.q_mtx = desc->q_mtx;
    d->c.writev.size = desc->write_buffered;
#ifdef USE_VM_PROBES
    if (dt_i1 != NULL) {
        *dt_i1 = d->fd;
        *dt_i2 = d->flags;
        *dt_i3 = d->c.writev.size;
    }
#endif
    d->reply = reply;
    d->c.writev.reply_size = reply_size;
    d->invoke = invoke_writev;
    d->free = free_data;
    d->level = 1;
    cq_enq(desc, d);
    desc->write_buffered = 0;
    return d;
}

static int flush_write(file_descriptor *desc, int *errp
#ifdef USE_VM_PROBES
                       , dt_private *dt_priv, char *dt_utag
#endif
) {
    int    result = 0;
#ifdef USE_VM_PROBES
    Sint64 dt_i1 = 0, dt_i2 = 0, dt_i3 = 0;
#endif
    struct t_data *d = NULL;

    MUTEX_LOCK(desc->q_mtx);
    if (desc->write_buffered > 0) {
	if ((d = async_write(desc, errp, 0, 0
#ifdef USE_VM_PROBES
			     ,&dt_i1, &dt_i2, &dt_i3
#endif
			     )) == NULL) {
            result = -1;
        }
    }
    MUTEX_UNLOCK(desc->q_mtx);
#ifdef USE_VM_PROBES
    if (d != NULL) {
        d->sched_i1 = dt_priv->thread_num;
        d->sched_i2 = dt_priv->tag;
        d->sched_utag[0] = '\0';
        if (dt_utag != NULL) {
            if (dt_utag[0] == '\0') {
                dt_utag = NULL;
            } else {
                strncpy(d->sched_utag, dt_utag, sizeof(d->sched_utag) - 1);
                d->sched_utag[sizeof(d->sched_utag) - 1] = '\0';
            }
        }
        DTRACE11(efile_drv_entry, dt_priv->thread_num, dt_priv->tag++,
                 dt_utag, FILE_WRITE,
                 NULL, NULL, dt_i1, dt_i2, dt_i3, 0, desc->port_str);
    }
#endif /* USE_VM_PROBES */
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

static int flush_write_check_error(file_descriptor *desc, int *errp
#ifdef USE_VM_PROBES
                                   , dt_private *dt_priv, char *dt_utag
#endif
				   ) {
    int r;
    if ( (r = flush_write(desc, errp
#ifdef USE_VM_PROBES
			  , dt_priv, dt_utag
#endif
			  )) != 0) {
	check_write_error(desc, NULL);
	return r;
    } else {
	return check_write_error(desc, errp);
    }
}

static struct t_data *async_lseek(file_descriptor *desc, int *errp, int reply,
				  Sint64 offset, int origin
#ifdef USE_VM_PROBES
				  , Sint64 *dt_i1, Sint64 *dt_i2, Sint64 *dt_i3
#endif
				  ) {
    struct t_data *d;
    if (! (d = EF_ALLOC(sizeof(struct t_data)))) {
	*errp = ENOMEM;
	return NULL;
    }
    d->flags = desc->flags;
    d->fd = desc->fd;
    d->command = FILE_LSEEK;
    d->reply = reply;
    d->c.lseek.offset = offset;
    d->c.lseek.origin = origin;
#ifdef USE_VM_PROBES
    if (dt_i1 != NULL) {
        *dt_i1 = d->fd;
        *dt_i2 = d->c.lseek.offset;
        *dt_i3 = d->c.lseek.origin;
    }
#endif
    d->invoke = invoke_lseek;
    d->free = free_data;
    d->level = 1;
    cq_enq(desc, d);
    return d;
}

static void flush_read(file_descriptor *desc) {
    desc->read_offset = 0;
    desc->read_size = 0;
    if (desc->read_binp) {
	driver_free_binary(desc->read_binp);
	desc->read_binp = NULL;
    }
}

static int lseek_flush_read(file_descriptor *desc, int *errp
#ifdef USE_VM_PROBES
			    ,dt_private *dt_priv, char *dt_utag
#endif
			    ) {
    int r = 0;
    size_t read_size = desc->read_size;
#ifdef USE_VM_PROBES
    Sint64 dt_i1 = 0, dt_i2 = 0, dt_i3 = 0;
#endif
    struct t_data *d;

    flush_read(desc);
    if (read_size != 0) {
	if ((d = async_lseek(desc, errp, 0,
                             -((ssize_t)read_size), EFILE_SEEK_CUR
#ifdef USE_VM_PROBES
			     , &dt_i1, &dt_i2, &dt_i3
#endif
			     )) == NULL) {
            r = -1;
        } else {
#ifdef USE_VM_PROBES
            d->sched_i1 = dt_priv->thread_num;
            d->sched_i2 = dt_priv->tag;
            d->sched_utag[0] = '\0';
            if (dt_utag != NULL) {
                if (dt_utag[0] == '\0') {
                    dt_utag = NULL;
                } else {
                    strncpy(d->sched_utag, dt_utag, sizeof(d->sched_utag) - 1);
                    d->sched_utag[sizeof(d->sched_utag) - 1] = '\0';
                }
            }
            DTRACE11(efile_drv_entry, dt_priv->thread_num, dt_priv->tag++,
                     dt_utag, FILE_LSEEK,
                     NULL, NULL, dt_i1, dt_i2, dt_i3, 0, desc->port_str);
#endif /* USE_VM_PROBES */
        }
    }
    return r;
}


/*********************************************************************
 * Driver entry point -> stop
 * The close has to be scheduled on async thread, so that currently active
 * async operation does not suddenly have the ground disappearing under their feet...
 */
static void 
file_stop(ErlDrvData e)
{
    file_descriptor* desc = (file_descriptor*)e;

    TRACE_C('p');

    IF_THRDS {
	flush_read(desc);
	if (desc->fd != FILE_FD_INVALID) {
	    struct t_data *d = EF_SAFE_ALLOC(sizeof(struct t_data));
	    d->command = FILE_CLOSE_ON_PORT_EXIT;
	    d->reply = !0;
	    d->fd = desc->fd;
	    d->flags = desc->flags;
	    d->invoke = invoke_close;
	    d->free = free_data;
	    d->level = 2;
	    d->data_to_free = (void *) desc;
	    cq_enq(desc, d);
	    desc->fd = FILE_FD_INVALID;
	    desc->flags = 0;
	    cq_execute(desc);
	} else {
	    EF_FREE(desc);
	}
    } else {
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
#ifdef  USE_VM_PROBES
    int sched_i1 = d->sched_i1, sched_i2 = d->sched_i2, command = d->command,
        result_ok = d->result_ok,
        posix_errno = d->result_ok ? 0 : d->errInfo.posix_errno;
    DTRACE_CHARBUF(sched_utag, DTRACE_EFILE_BUFSIZ+1);

    sched_utag[0] = '\0';
    if (DTRACE_ENABLED(efile_drv_return)) {
        strncpy(sched_utag, d->sched_utag, DTRACE_EFILE_BUFSIZ);
        sched_utag[DTRACE_EFILE_BUFSIZ] = '\0';
    }
#endif  /* USE_VM_PROBES */

    TRACE_C('r');

    if (try_again(desc, d)) {
        /* DTRACE TODO: what kind of probe makes sense here? */
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
	    header[0] = FILE_RESP_ALL_DATA;
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
	  free_data(data);
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
      case FILE_FALLOCATE:
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
		resbuf[0] = FILE_RESP_FNAME;
		length = 1+FILENAME_BYTELEN((char*) resbuf+1);
		TRACE_C('R');
		driver_output2(desc->port, resbuf, 1, resbuf+1, length-1);
	    }
	    free_data(data);
	    break;
	}
      case FILE_OPEN:
	if (!d->result_ok) {
	    reply_error(desc, &d->errInfo);
	} else {
	    ASSERT(d->is_fd_unused);
	    desc->fd = d->fd;
	    desc->flags = d->flags;
	    d->is_fd_unused = 0;
	    reply_Uint(desc, d->fd);
	}
	free_data(data);
	break;
      case FILE_FSTAT:
      case FILE_LSTAT:
        {
	    if (d->result_ok) {
		resbuf[0] = FILE_RESP_INFO;

		put_int32(d->info.size_high,         &resbuf[1 + ( 0 * 4)]);
		put_int32(d->info.size_low,          &resbuf[1 + ( 1 * 4)]);
		put_int32(d->info.type,              &resbuf[1 + ( 2 * 4)]);

		/* Note 64 bit indexing in resbuf here */
		put_int64(d->info.accessTime,        &resbuf[1 + ( 3 * 4)]);
		put_int64(d->info.modifyTime,        &resbuf[1 + ( 5 * 4)]);
		put_int64(d->info.cTime,             &resbuf[1 + ( 7 * 4)]);

		put_int32(d->info.mode,              &resbuf[1 + ( 9 * 4)]);
		put_int32(d->info.links,             &resbuf[1 + (10 * 4)]);
		put_int32(d->info.major_device,      &resbuf[1 + (11 * 4)]);
		put_int32(d->info.minor_device,      &resbuf[1 + (12 * 4)]);
		put_int32(d->info.inode,             &resbuf[1 + (13 * 4)]);
		put_int32(d->info.uid,               &resbuf[1 + (14 * 4)]);
		put_int32(d->info.gid,               &resbuf[1 + (15 * 4)]);
		put_int32(d->info.access,            &resbuf[1 + (16 * 4)]);

#define RESULT_SIZE (1 + (17 * 4))
		TRACE_C('R');
		driver_output2(desc->port, resbuf, RESULT_SIZE, NULL, 0);
#undef RESULT_SIZE
	    } else
		reply_error(desc, &d->errInfo);
	}
	free_data(data);
	break;
      case FILE_READDIR:
	if (!d->result_ok) {
	    reply_error(desc, &d->errInfo);
	} else {
	    struct t_readdir_buf *b1 = d->c.read_dir.first_buf;
	    char   op = FILE_RESP_LFNAME;

	    TRACE_C('R');
	    ASSERT(b1);

	    while (b1) {
		struct t_readdir_buf *b2 = b1;
		char *p = &b1->buf[0];
		driver_output2(desc->port, p, 1, p + 1, b1->n - 1);
		b1 = b1->next;
		EF_FREE(b2);
	    }
	    driver_output2(desc->port, &op, 1, NULL, 0);
    
	    d->c.read_dir.first_buf = NULL;
	    d->c.read_dir.last_buf = NULL;
	}
	free_readdir(data);
	break;
      case FILE_CLOSE:
	  if (d->reply) {
	      TRACE_C('K');
	      reply_ok(desc);
#ifdef USE_VM_PROBES
              result_ok = 1;
#endif
	  }
	  free_data(data);
	  break;
      case FILE_PWRITEV:
	  if (!d->result_ok) {
	      reply_Uint_error(desc, d->c.pwritev.cnt, &d->errInfo);
	  } else {
	      reply_Uint(desc, d->c.pwritev.n);
	  }
	  free_data(data);
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
#ifdef HAVE_SENDFILE
      case FILE_SENDFILE:
	  if (d->result_ok == -1) {
	      if (d->errInfo.posix_errno == ECONNRESET ||
		  d->errInfo.posix_errno == ENOTCONN ||
		  d->errInfo.posix_errno == EPIPE)
		  reply_string_error(desc,"closed");
	      else
		  reply_error(desc, &d->errInfo);
	      desc->sendfile_state = not_sending;
	      free_sendfile(data);
	  } else if (d->result_ok == 0) {
	      reply_Sint64(desc, d->c.sendfile.written);
	      desc->sendfile_state = not_sending;
	      free_sendfile(data);
	  } else if (d->result_ok == 1) { /* If we are using select to send the rest of the data */
	      desc->sendfile_state = sending;
	      desc->d = d;
	      driver_select(desc->port, (ErlDrvEvent)(long)d->c.sendfile.out_fd,
			    ERL_DRV_USE_NO_CALLBACK|ERL_DRV_WRITE, 1);
	  }
	  break;
#endif
      case FILE_CLOSE_ON_PORT_EXIT:
	  /* See file_stop. However this is never invoked after the port is killed. */
	  free_data(data);
	  desc = NULL;
	  /* This is it for this port, so just send dtrace and return, avoid doing anything to the freed data */
	  DTRACE6(efile_drv_return, sched_i1, sched_i2, sched_utag,
		  command, result_ok, posix_errno);
	  return;
      default:
	abort();
    }
    DTRACE6(efile_drv_return, sched_i1, sched_i2, sched_utag,
            command, result_ok, posix_errno);
    if (desc->write_buffered != 0 && desc->timer_state == timer_idle ) {
	desc->timer_state = timer_write;
	driver_set_timer(desc->port, desc->write_delay);
    }
    cq_execute(desc);

}


/*********************************************************************
 * Driver entry point -> output
 */
static void 
file_output(ErlDrvData e, char* buf, ErlDrvSizeT count)
{
    file_descriptor* desc = (file_descriptor*)e;
    Efile_error errInfo;	/* The error codes for the last operation. */
    Sint fd;			/* The file descriptor for this port, if any,
				 * -1 if none.
				 */
    char* name;			/* Points to the filename in buf. */
    int command;
    struct t_data *d = NULL;
#ifdef  USE_VM_PROBES
    char *dt_utag = NULL;
    char *dt_s1 = NULL, *dt_s2 = NULL;
    Sint64 dt_i1 = 0;
    Sint64 dt_i2 = 0;
    Sint64 dt_i3 = 0;
    Sint64 dt_i4 = 0;
    dt_private *dt_priv = get_dt_private(0);
#endif  /* USE_VM_PROBES */

    TRACE_C('o');

    fd  = desc->fd;
    name = buf+1;
    command = *(uchar*)buf++;

    switch(command) {

    case FILE_MKDIR:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE);
	
	FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	dt_s1 = d->b;
	dt_utag = name + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;
#endif
	d->command = command;
	d->invoke = invoke_mkdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_RMDIR:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE);
	
	FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	dt_s1 = d->b;
	dt_utag = name + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;
#endif
	d->command = command;
	d->invoke = invoke_rmdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_DELETE:
    {
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE);
	
	FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	dt_s1 = d->b;
	dt_utag = name + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;
#endif
	d->command = command;
	d->invoke = invoke_delete_file;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_RENAME:
	{
	    char* new_name;
	    int namelen = FILENAME_BYTELEN(name)+FILENAME_CHARSIZE;
	    new_name = name+namelen;
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + namelen
			      + FILENAME_BYTELEN(new_name) + FILENAME_CHARSIZE);
	
	    FILENAME_COPY(d->b, name);
	    FILENAME_COPY(d->b + namelen, new_name);
#ifdef USE_VM_PROBES
	    dt_s1 = d->b;
	    dt_s2 = d->b + namelen;
	    dt_utag = buf + namelen + FILENAME_BYTELEN(new_name) + FILENAME_CHARSIZE;
#endif
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
	d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE);
	
	FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	dt_s1 = d->b;
	dt_utag = name + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;
#endif
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
#ifdef USE_VM_PROBES
	    dt_utag = buf + 1;
#endif
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
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(name) + 
			      FILENAME_CHARSIZE);
	
	    FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	    dt_s1 = d->b;
	    dt_utag = name + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;
#endif
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
	    size_t resbufsize;
	    size_t n = 0, total = 0;
	    int res = 0;
	    char resbuf[READDIR_BUFSIZE];

	    EFILE_DIR_HANDLE dir_handle; /* Handle to open directory. */

	    total               = READDIR_BUFSIZE;
	    errInfo.posix_errno = 0;
	    dir_handle          = NULL;
	    resbuf[0]           = FILE_RESP_LFNAME;

#ifdef USE_VM_PROBES
	    dt_s1 = name;
	    dt_utag = name + FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;
#endif
	    /* Fill the buffer with multiple directory listings before sending it to the
	     * receiving process. READDIR_CHUNKS is minimum number of files sent to the
	     * receiver.
	     * Format for each driver_output2:
	     * ------------------------------------
	     * | Type   | Len     | Filename  | ...
	     * | 1 byte | 2 bytes | Len bytes | ...
	     * ------------------------------------
	     */

	    do {
		n = 1;
		resbufsize = READDIR_BUFSIZE - n;

		do {
		    res = efile_readdir(&errInfo, name, &dir_handle, resbuf + n + 2, &resbufsize);

		    if (res) {
			put_int16((Uint16)resbufsize, resbuf + n);
			n += 2 + resbufsize;
			resbufsize = READDIR_BUFSIZE - n;
		    }
		} while( res && ((total - n - 2) >= MAXPATHLEN*FILENAME_CHARSIZE));

		if (n > 1) {
		    driver_output2(desc->port, resbuf, 1, resbuf + 1, n - 1);
		}
	    } while(res);

	    if (errInfo.posix_errno != 0) {
		reply_error(desc, &errInfo);
		return;
	    }
#ifdef USE_VM_PROBES
	    if (dt_utag != NULL && dt_utag[0] == '\0') {
                dt_utag = NULL;
            } 

	    DTRACE11(efile_drv_entry, dt_priv->thread_num, dt_priv->tag,
		     dt_utag, command, name, dt_s2,
		     dt_i1, dt_i2, dt_i3, dt_i4, desc->port_str);
	    DTRACE6(efile_drv_return, dt_priv->thread_num, dt_priv->tag++, 
		    dt_utag, command, 1, 0);
#endif
	    TRACE_C('R');
	    driver_output2(desc->port, resbuf, 1, NULL, 0);
	    return;
	}
    case FILE_OPEN:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(buf+4) + 
			      FILENAME_CHARSIZE);
	
	    d->flags = get_int32((uchar*)buf);
	    name = buf+4;
	    FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	    dt_i1 = d->flags;
	    dt_s1 = d->b;
	    dt_utag = name + FILENAME_BYTELEN(d->b) + FILENAME_CHARSIZE;
#endif
	    d->command = command;
	    d->invoke = invoke_open;
	    d->free = free_data;
	    d->level = 2;
	    d->is_fd_unused = 1;
	    goto done;
	}

    case FILE_FDATASYNC:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data));
	    
	    d->fd = fd;
#ifdef USE_VM_PROBES
	    dt_utag = name;
	    dt_i1 = fd;
#endif
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
#ifdef USE_VM_PROBES
	    dt_utag = name;
	    dt_i1 = fd;
#endif
	    d->command = command;
	    d->invoke = invoke_fsync;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}


    case FILE_FSTAT: 
    case FILE_LSTAT:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + FILENAME_BYTELEN(name) + 
			      FILENAME_CHARSIZE);
	    
	    FILENAME_COPY(d->b, name);
	    d->fd = fd;
#ifdef USE_VM_PROBES
	    dt_utag = name + FILENAME_BYTELEN(d->b) + FILENAME_CHARSIZE;
	    if (command == FILE_LSTAT) {
		dt_s1 = d->b;
	    } else {
		dt_i1 = fd;
	    }
#endif
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
#ifdef USE_VM_PROBES
	    dt_utag = name;
	    dt_i1 = fd;
	    dt_i2 = d->flags;
#endif
	    d->command = command;
	    d->invoke = invoke_truncate;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_WRITE_INFO:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + FILENAME_BYTELEN(buf + 9*4) + FILENAME_CHARSIZE);
	    
	    d->info.mode       = get_int32(buf + 0 * 4);
	    d->info.uid        = get_int32(buf + 1 * 4);
	    d->info.gid        = get_int32(buf + 2 * 4);
	    d->info.accessTime = get_int64(buf + 3 * 4);
	    d->info.modifyTime = get_int64(buf + 5 * 4);
	    d->info.cTime      = get_int64(buf + 7 * 4);

	    FILENAME_COPY(d->b, buf + 9*4);
#ifdef USE_VM_PROBES
	    dt_i1              = d->info.mode;
	    dt_i2              = d->info.uid;
	    dt_i3              = d->info.gid;
	    dt_s1 = d->b;
	    dt_utag = buf + 9 * 4 + FILENAME_BYTELEN(d->b) + FILENAME_CHARSIZE;
#endif
	    d->command = command;
	    d->invoke = invoke_write_info;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_READLINK:
	{
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + 
			      MAX(RESBUFSIZE, (FILENAME_BYTELEN(name) +  
					       FILENAME_CHARSIZE))  + 1);
	    FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	    dt_s1 = d->b;
	    dt_utag = name + FILENAME_BYTELEN(d->b) + FILENAME_CHARSIZE;
#endif
	    d->command = command;
	    d->invoke = invoke_readlink;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_ALTNAME:
	{
	     d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1 + 
			       MAX(RESBUFSIZE, (FILENAME_BYTELEN(name) +  
						FILENAME_CHARSIZE))  + 1);
	    FILENAME_COPY(d->b, name);
#ifdef USE_VM_PROBES
	    dt_s1 = d->b;
	    dt_utag = name + FILENAME_BYTELEN(d->b) + FILENAME_CHARSIZE;
#endif
	    d->command = command;
	    d->invoke = invoke_altname;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}


    case FILE_LINK:
	{
	    char* new_name;
	    int namelen = FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;

	    new_name = name+namelen;
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + namelen
			      + FILENAME_BYTELEN(new_name) + FILENAME_CHARSIZE);
	
	    FILENAME_COPY(d->b, name);
	    FILENAME_COPY(d->b + namelen, new_name);
#ifdef USE_VM_PROBES
	    dt_s1 = d->b;
	    dt_s2 = d->b + namelen;
	    dt_utag = buf + namelen + FILENAME_BYTELEN(dt_s2) + FILENAME_CHARSIZE;
#endif
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
	    int namelen = FILENAME_BYTELEN(name) + FILENAME_CHARSIZE;

	    new_name = name+namelen;
	    d = EF_SAFE_ALLOC(sizeof(struct t_data) - 1
			      + namelen
			      + FILENAME_BYTELEN(new_name) + FILENAME_CHARSIZE);
	
	    FILENAME_COPY(d->b, name);
	    FILENAME_COPY(d->b + namelen, new_name);
#ifdef USE_VM_PROBES
	    dt_s1 = d->b;
	    dt_s2 = d->b + namelen;
	    dt_utag = buf + namelen + FILENAME_BYTELEN(dt_s2) + FILENAME_CHARSIZE;
#endif
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
#ifdef USE_VM_PROBES
        dt_i1 = d->fd;
        dt_i2 = d->c.fadvise.offset;
        dt_i3 = d->c.fadvise.length;
        dt_i4 = d->c.fadvise.advise;
        dt_utag = buf + 3 * sizeof(Sint64);
#endif
        goto done;
    }

    case FILE_FALLOCATE:
    {
        d = EF_SAFE_ALLOC(sizeof(struct t_data));

        d->fd = fd;
        d->command = command;
        d->invoke = invoke_fallocate;
        d->free = free_data;
        d->level = 2;
        d->c.fallocate.offset = get_int64((uchar*) buf);
        d->c.fallocate.length = get_int64(((uchar*) buf) + sizeof(Sint64));
        goto done;
    }

    }

    /*
     * Ignore anything else -- let the caller hang.
     */
     
    return;

 done:
    if (d) {
#ifdef USE_VM_PROBES
	d->sched_i1 = dt_priv->thread_num;
	d->sched_i2 = dt_priv->tag;
	d->sched_utag[0] = '\0';
	if (dt_utag != NULL) {
	    if (dt_utag[0] == '\0') {
		dt_utag = NULL;
	    } else {
		strncpy(d->sched_utag, dt_utag, sizeof(d->sched_utag) - 1);
		d->sched_utag[sizeof(d->sched_utag) - 1] = '\0';
	    }
	}
	DTRACE11(efile_drv_entry, dt_priv->thread_num, dt_priv->tag++,
		 dt_utag, command, dt_s1, dt_s2,
		 dt_i1, dt_i2, dt_i3, dt_i4, desc->port_str);
#endif
	cq_enq(desc, d);
    }
}

/*********************************************************************
 * Driver entry point -> flush
 */
static void 
file_flush(ErlDrvData e) {
    file_descriptor *desc = (file_descriptor *)e;
#ifdef DEBUG
    int r;
#endif
#ifdef  USE_VM_PROBES
    dt_private *dt_priv = get_dt_private(dt_driver_io_worker_base);
#endif

    TRACE_C('f');

#ifdef HAVE_SENDFILE
    flush_sendfile(desc, NULL);
#endif

#ifdef DEBUG
    r = 
#endif
         flush_write(desc, NULL
#ifdef USE_VM_PROBES
		     , dt_priv, (desc->d == NULL) ? NULL : desc->d->sched_utag
#endif
		     );
    /* Only possible reason for bad return value is ENOMEM, and 
     * there is nobody to tell...
     */
#ifdef DEBUG
    ASSERT(r == 0); 
#endif
    cq_execute(desc);
}



/*********************************************************************
 * Driver entry point -> control
 * Only debug functionality...
 */
static ErlDrvSSizeT
file_control(ErlDrvData e, unsigned int command, 
	     char* buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) {
    file_descriptor *desc = (file_descriptor *)e;
    switch (command) {
    case 'K' :
	if (rlen < 4) {
	    *rbuf = EF_ALLOC(4);
	}
	(*rbuf)[0] = ((desc->key) >> 24) & 0xFF;
	(*rbuf)[1] = ((desc->key) >> 16) & 0xFF;
	(*rbuf)[2] = ((desc->key) >> 8) & 0xFF;
	(*rbuf)[3] = (desc->key) & 0xFF;
	return 4;
    default:
	return 0;
    }
}

/*********************************************************************
 * Driver entry point -> timeout
 */
static void 
file_timeout(ErlDrvData e) {
    file_descriptor *desc = (file_descriptor *)e;
    enum e_timer timer_state = desc->timer_state;
#ifdef  USE_VM_PROBES
    dt_private *dt_priv = get_dt_private(dt_driver_io_worker_base);
#endif

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
#ifdef DEBUG
	int r = 
#endif
	         flush_write(desc, NULL
#ifdef USE_VM_PROBES
			     , dt_priv, (desc->d == NULL) ? NULL : desc->d->sched_utag
#endif
			     );
	/* Only possible reason for bad return value is ENOMEM, and 
	 * there is nobody to tell...
	 */
	ASSERT(r == 0); 
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
    size_t p, q;
    int err;
    struct t_data *d = NULL;
#ifdef USE_VM_PROBES
    Sint64 dt_i1 = 0, dt_i2 = 0, dt_i3 = 0;
    Sint64 dt_i4 = 0;
    char *dt_utag = NULL;
    char *dt_s1 = NULL;
    dt_private *dt_priv = get_dt_private(dt_driver_io_worker_base);
#endif

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
#ifdef USE_VM_PROBES
	dt_utag = EV_CHAR_P(ev, p, q);
#endif
	flush_read(desc);
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (desc->fd != FILE_FD_INVALID) {
	    if (! (d = EF_ALLOC(sizeof(struct t_data)))) {
		reply_posix_error(desc, ENOMEM);
	    } else {
		d->command = command;
		d->reply = !0;
		d->fd = desc->fd;
		d->flags = desc->flags;
#ifdef USE_VM_PROBES
		dt_i1 = d->fd;
		dt_i2 = d->flags;
#endif
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

	if (!EV_GET_UINT32(ev, &sizeH, &p, &q)
	    || !EV_GET_UINT32(ev, &sizeL, &p, &q)) {
	    /* Wrong buffer length to contain the read count */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
#ifdef USE_VM_PROBES
	dt_utag = EV_CHAR_P(ev, p, q);
#endif
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
#if ALWAYS_READ_LINE_AHEAD
	if (desc->read_bufsize == 0 && desc->read_binp != NULL && desc->read_size > 0) {
	    /* We have allocated a buffer for line mode but should not really have a 
	       read-ahead buffer... */
	    if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
				 , dt_priv, dt_utag
#endif
				 ) < 0) {
		reply_posix_error(desc, err);
		goto done;
	    }
	}
#endif
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
#ifdef USE_VM_PROBES
	dt_i1 = d->fd;
	dt_i2 = d->flags;
	dt_i3 = d->c.read.size;
#endif
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
#ifdef USE_VM_PROBES
	dt_utag = EV_CHAR_P(ev, p, q);
#endif
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size != 1
#ifdef USE_VM_PROBES
	    + FILENAME_BYTELEN(dt_utag) + FILENAME_CHARSIZE
#endif
	    ) {
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
#ifdef USE_VM_PROBES
	dt_i1 = d->fd;
	dt_i2 = d->flags;
	dt_i3 = d->c.read_line.read_offset;
#endif
#if !ALWAYS_READ_LINE_AHEAD
	d->c.read_line.read_ahead = (desc->read_bufsize > 0);
#ifdef USE_VM_PROBES
	dt_i4 = d->c.read_line.read_ahead;
#endif
#endif 
	driver_binary_inc_refc(d->c.read.binp);
	d->invoke = invoke_read_line;
	d->free = free_read_line;
	d->level = 1;
	cq_enq(desc, d);
    } goto done;
    case FILE_WRITE: { /* Dtrace: The dtrace user tag is not last in message, 
			  but follows the message tag directly. 
			  This is handled specially in prim_file.erl */
	ErlDrvSizeT skip = 1;
	ErlDrvSizeT size = ev->size - skip;

#ifdef USE_VM_PROBES
	dt_utag = EV_CHAR_P(ev, p, q);
	skip += FILENAME_BYTELEN(dt_utag) + FILENAME_CHARSIZE;
	size = ev->size - skip;
#endif
	if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
			     , dt_priv, dt_utag
#endif
			     ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (! (desc->flags & EFILE_MODE_WRITE)) {
	    reply_posix_error(desc, EBADF);
	    goto done;
	}
	if (size == 0) {
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
	    if ((d = async_write(desc, &err, !0, size
#ifdef USE_VM_PROBES
				 , &dt_i1, &dt_i2, &dt_i3
#endif
				 )) == NULL) {
		MUTEX_UNLOCK(desc->q_mtx);
		reply_posix_error(desc, err);
		goto done;
	    } else {
		MUTEX_UNLOCK(desc->q_mtx);
	    }
	}
    } goto done; /* case FILE_WRITE */

    case FILE_PWRITEV: { /* Dtrace: The dtrace user tag is not last in message, 
			   but follows the message tag directly. 
			   This is handled specially in prim_file.erl */
	Uint32 i, j, n; 
	size_t total;
#ifdef USE_VM_PROBES
	char dt_tmp;
	int dt_utag_bytes = 1;

	dt_utag = EV_CHAR_P(ev, p, q);
	/* This will work for UTF-8, but not for UTF-16 - extra reminder here */
#ifdef FILENAMES_16BIT 
#error 16bit characters in filenames and dtrace in combination is not supported.
#endif
	while (EV_GET_CHAR(ev, &dt_tmp, &p, &q) && dt_tmp != '\0') {
	    dt_utag_bytes++;
	}
#endif
	if (ev->size < 1+4
#ifdef USE_VM_PROBES
	    + dt_utag_bytes
#endif
	    || !EV_GET_UINT32(ev, &n, &p, &q)) {
	    /* Buffer too short to contain even the number of pos/size specs */
	    reply_Uint_posix_error(desc, 0, EINVAL);
	    goto done;
	}
	if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
			     , dt_priv, dt_utag
#endif
			     ) < 0) {
	    reply_Uint_posix_error(desc, 0, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_Uint_posix_error(desc, 0, err);
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
	if (ev->size < 1+4+8*(2*n)
#ifdef USE_VM_PROBES
	    + dt_utag_bytes
#endif
	    ) {
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
#ifdef USE_VM_PROBES
	dt_i1 = d->fd;
	dt_i2 = d->flags;
#endif
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
	    if (   !EV_GET_SINT64(ev, &d->c.pwritev.specs[i].offset, &p, &q)
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
#ifdef USE_VM_PROBES
	dt_i3 = d->c.pwritev.size;
#endif
	if (j == 0) {
	    /* Trivial case - nothing to write */
	    EF_FREE(d);
	    reply_Uint(desc, 0);
	} else {
	    ErlDrvSizeT skip = 1 + 4 + 8 * (2*n) 
#ifdef USE_VM_PROBES
		+ dt_utag_bytes
#endif
		;
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
		d->free = free_data;
		d->level = 1;
		cq_enq(desc, d);
	    }
	}
    } goto done; /* case FILE_PWRITEV: */

    case FILE_PREADV: { /* Dtrace: The dtrace user tag is not last in message, 
			   but follows the message tag directly. 
			   This is handled specially in prim_file.erl */
	register void * void_ptr;
	Uint32 i, n;
	ErlIOVec *res_ev;
#ifdef USE_VM_PROBES
	char dt_tmp;
	int dt_utag_bytes = 1;
	/* This will work for UTF-8, but not for UTF-16 - extra reminder here */
#ifdef FILENAMES_16BIT 
#error 16bit characters in filenames and dtrace in combination is not supported.
#endif
	dt_utag = EV_CHAR_P(ev, p, q);
	while (EV_GET_CHAR(ev, &dt_tmp, &p, &q) && dt_tmp != '\0') {
	    dt_utag_bytes++;
	}
#endif
	if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
			     , dt_priv, dt_utag
#endif
			     ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (ev->size < 1+8
#ifdef USE_VM_PROBES
	    + dt_utag_bytes
#endif
	    || !EV_GET_UINT32(ev, &n, &p, &q)
	    || !EV_GET_UINT32(ev, &n, &p, &q)) {
	    /* Buffer too short to contain even the number of pos/size specs */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	if (ev->size < 1+8+8*(2*n)
#ifdef USE_VM_PROBES
	    + dt_utag_bytes
#endif
	    ) {
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
#ifdef USE_VM_PROBES
	dt_i1 = d->fd;
	dt_i2 = d->flags;
#endif
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
	    if (   !EV_GET_SINT64(ev, &d->c.preadv.offsets[i-1], &p, &q)
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
#ifdef USE_VM_PROBES
	    dt_i3 += size;
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
	put_int32(n, (char *)(res_ev->iov[0].iov_base) + 4);
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
	Sint64 offset;		/* Offset for seek */
	Uint32 origin;		/* Origin of seek. */

	if (ev->size < 1+8+4
	    || !EV_GET_SINT64(ev, &offset, &p, &q)
	    || !EV_GET_UINT32(ev, &origin, &p, &q)) {
	    /* Wrong length of buffer to contain offset and origin */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
#ifdef USE_VM_PROBES
	dt_utag = EV_CHAR_P(ev, p, q);
#endif
	if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
			     , dt_priv, dt_utag
#endif
			     ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if ((d = async_lseek(desc, &err, !0, offset, origin
#ifdef USE_VM_PROBES
			     , &dt_i1, &dt_i2, &dt_i3
#endif
			     )) == NULL) {
	    reply_posix_error(desc, err);
	    goto done;
	}
    } goto done;

    case FILE_READ_FILE: {
	char *filename;
	if (ev->size < 1+1) {
	    /* Buffer contains empty name */
	    reply_posix_error(desc, ENOENT);
	    goto done;
	}
#ifndef USE_VM_PROBES
	/* In the dtrace case, the iov has an extra element, the dtrace utag - we will need 
	   another test to see that
	   the filename is in a single buffer: */
	if (ev->size-1 != ev->iov[q].iov_len-p) {
	    /* Name not in one single buffer */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
#else
	if (((byte *)ev->iov[q].iov_base)[ev->iov[q].iov_len-1] != '\0') {
	    /* Name not in one single buffer */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}	
#endif
	filename = EV_CHAR_P(ev, p, q);
	d = EF_ALLOC(sizeof(struct t_data) -1 + FILENAME_BYTELEN(filename) + FILENAME_CHARSIZE);
	if (! d) {
	    reply_posix_error(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	/* Copy name */
	FILENAME_COPY(d->b, filename);
#ifdef USE_VM_PROBES
	{
	    char dt_tmp;

	    /* This will work for UTF-8, but not for UTF-16 - extra reminder here */
#ifdef FILENAMES_16BIT 
#error 16bit characters in filenames and dtrace in combination is not supported.
#endif
	    while (EV_GET_CHAR(ev, &dt_tmp, &p, &q) && dt_tmp != '\0') 
		;
	    dt_s1 = d->b;
	    dt_utag = EV_CHAR_P(ev, p, q);
	}
#endif
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
	if (ev->size < 1+1+8+4
	    || !EV_GET_SINT64(ev, &hdr_offset, &p, &q)
	    || !EV_GET_UINT32(ev, &max_size, &p, &q)) {
	    /* Buffer too short to contain 
	     * the header offset and max size spec */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
#ifdef USE_VM_PROBES
	dt_utag = EV_CHAR_P(ev, p, q);
#endif
	if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
			     , dt_priv, dt_utag
#endif
			     ) < 0) {
	    reply_posix_error(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				    , dt_priv, dt_utag
#endif
				    ) < 0) {
	    reply_posix_error(desc, err);
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
#ifdef USE_VM_PROBES
	dt_i1 = d->fd;
	dt_i2 = d->flags;
	dt_i3 = d->c.preadv.offsets[0];
	dt_i4 = d->c.preadv.size;
#endif
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
#ifdef USE_VM_PROBES
	dt_i1 = opt;
	dt_utag = EV_CHAR_P(ev, p, q);
#endif
	switch (opt) {
	case FILE_OPT_DELAYED_WRITE: {
	    Uint32 sizeH, sizeL, delayH, delayL;
	    if (ev->size != 1+1+4*sizeof(Uint32)
#ifdef USE_VM_PROBES
		+ FILENAME_BYTELEN(dt_utag) + FILENAME_CHARSIZE
#endif
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
#ifdef USE_VM_PROBES
	    dt_i2 = desc->write_delay;
#endif
	    TRACE_C('K');
	    reply_ok(desc);
	} goto done;
	case FILE_OPT_READ_AHEAD: {
	    Uint32 sizeH, sizeL;
	    if (ev->size != 1+1+2*sizeof(Uint32)
#ifdef USE_VM_PROBES
		+ FILENAME_BYTELEN(dt_utag)+FILENAME_CHARSIZE
#endif
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
#ifdef USE_VM_PROBES
	    dt_i2 = desc->read_bufsize;
#endif
	    TRACE_C('K');
	    reply_ok(desc);
	} goto done;
	default:
	    reply_posix_error(desc, EINVAL);
	    goto done;
	} /* case FILE_OPT_DELAYED_WRITE: */
    } ASSERT(0); goto done; /* case FILE_SETOPT: */

    case FILE_SENDFILE: {

#ifdef HAVE_SENDFILE
        struct t_data *d;
	Uint32 out_fd, offsetH, offsetL, hd_len, tl_len;
	Uint64 nbytes;
	char flags;

	if (ev->size < 1 + 7 * sizeof(Uint32) + sizeof(char)
		|| !EV_GET_UINT32(ev, &out_fd, &p, &q)
		|| !EV_GET_CHAR(ev, &flags, &p, &q)
		|| !EV_GET_UINT32(ev, &offsetH, &p, &q)
		|| !EV_GET_UINT32(ev, &offsetL, &p, &q)
		|| !EV_GET_UINT64(ev, &nbytes, &p, &q)
		|| !EV_GET_UINT32(ev, &hd_len, &p, &q)
		|| !EV_GET_UINT32(ev, &tl_len, &p, &q)) {
	    /* Buffer has wrong length to contain all the needed values */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}

	if (hd_len != 0 || tl_len != 0) {
	    /* We do not allow header, trailers */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}

	
	if (flags & SENDFILE_FLGS_USE_THREADS && !THRDS_AVAILABLE) {
	    /* We do not allow use_threads flag on a system where
	       no threads are available. */
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}

	d = EF_SAFE_ALLOC(sizeof(struct t_data));
	d->fd = desc->fd;
	d->command = command;
	d->invoke = invoke_sendfile;
	d->free = free_sendfile;
	d->flags = flags;
	d->level = 2;

	d->c.sendfile.out_fd = (int) out_fd;
	d->c.sendfile.written = 0;
	d->c.sendfile.port = desc->port;
	d->c.sendfile.q_mtx = desc->q_mtx;

    #if SIZEOF_OFF_T == 4
	if (offsetH != 0) {
	    reply_posix_error(desc, EINVAL);
	    goto done;
	}
	d->c.sendfile.offset = (off_t) offsetL;
    #else
	d->c.sendfile.offset = ((off_t) offsetH << 32) | offsetL;
    #endif

	d->c.sendfile.nbytes = nbytes;

	if (USE_THRDS_FOR_SENDFILE(d)) {
	    SET_BLOCKING(d->c.sendfile.out_fd);
	} else {
	    /**
	     * Write a place holder to queue in order to force file_flush
	     * to be called before the driver is closed.
	     */
	    char tmp[1] = "";
	    MUTEX_LOCK(d->c.sendfile.q_mtx);
	    if (driver_enq(d->c.sendfile.port, tmp, 1)) {
	        MUTEX_UNLOCK(d->c.sendfile.q_mtx);
		reply_posix_error(desc, ENOMEM);
		goto done;
	    }
	    MUTEX_UNLOCK(d->c.sendfile.q_mtx);
	}

	cq_enq(desc, d);
#else
	reply_posix_error(desc, ENOTSUP);
#endif
	goto done;
        } /* case FILE_SENDFILE: */

    } /* switch(command) */

    if (lseek_flush_read(desc, &err
#ifdef USE_VM_PROBES
			 , dt_priv, dt_utag
#endif
			 ) < 0) {
	reply_posix_error(desc, err);
	goto done;
    }
    if (flush_write_check_error(desc, &err
#ifdef USE_VM_PROBES
				, dt_priv, dt_utag
#endif
				) < 0) {
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
    if (d != NULL) {
#ifdef USE_VM_PROBES
	/*
	 * If d == NULL, then either:
	 *    1). There was an error of some sort, or
	 *    2). The command given to us is actually implemented
	 *	  by file_output() instead.
	 *
	 * Case #1 is probably a TODO item, perhaps?
	 * Case #2 we definitely don't want to activate a probe.
	 */
	d->sched_i1 = dt_priv->thread_num;
	d->sched_i2 = dt_priv->tag;
	d->sched_utag[0] = '\0';
	if (dt_utag != NULL) {
	    if (dt_utag[0] == '\0') {
                dt_utag = NULL;
            } else {
		strncpy(d->sched_utag, dt_utag, sizeof(d->sched_utag) - 1);
		d->sched_utag[sizeof(d->sched_utag) - 1] = '\0';
	    }
	}
	DTRACE11(efile_drv_entry, dt_priv->thread_num, dt_priv->tag++,
		 dt_utag, command, dt_s1, NULL, dt_i1, dt_i2, dt_i3, dt_i4,
		 desc->port_str);
#endif
    }
    cq_execute(desc);
}

#ifdef  USE_VM_PROBES
dt_private *
get_dt_private(int base)
{
    dt_private *dt_priv = (dt_private *) pthread_getspecific(dt_driver_key);

    if (dt_priv == NULL) {
	dt_priv = EF_SAFE_ALLOC(sizeof(dt_private));
	erts_mtx_lock(&dt_driver_mutex);
	dt_priv->thread_num = (base + dt_driver_idnum++);
	erts_mtx_unlock(&dt_driver_mutex);
	dt_priv->tag = 0;
	pthread_setspecific(dt_driver_key, dt_priv);
    }
    return dt_priv;
}
#endif  /* USE_VM_PROBES */
