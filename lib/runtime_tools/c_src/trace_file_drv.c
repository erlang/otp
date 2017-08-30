/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
 * Purpose: Send trace messages to a file.
 */

#ifdef __WIN32__
#include <windows.h>
#endif
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __WIN32__
#  include <io.h>
#  define write _write
#  define close _close
#  define unlink _unlink
#else
#  include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

/*
 * Deduce MAXPATHLEN, which is the one to use in this file, 
 * from any available definition.
 */
#ifndef MAXPATHLEN
#  ifdef PATH_MAX /* Posix */
#    define MAXPATHLEN PATH_MAX
#  else
#    ifdef _POSIX_PATH_MAX /* Posix */
#      define MAXPATHLEN _POSIX_PATH_MAX
#    else
#      ifdef MAXPATH
#        define MAXPATHLEN MAXPATH
#      else
#        ifdef MAX_PATH
#          define MAXPATHLEN MAX_PATH
#        else
#          ifdef _MAX_PATH
#            define MAXPATHLEN _MAX_PATH
#         else
#            error Could not define MAXPATHLEN
#          endif
#        endif
#      endif
#    endif
#  endif
#endif



#ifdef DEBUG
#include <assert.h>
#define ASSERT(X) assert(X)
#else
#define ASSERT(X)
#endif



#include "erl_driver.h"



/*
** Protocol from driver:
** '\0' -> ok
** '\1' ++ String -> {error, Atom}
**
** Protocol when opening (arguments to start):
** ["w <WrapSize> <WrapCnt> <TailIndex> "] "n <Filename>"
** Where...
** <Filename>, a string ('\0' terminated):
**    The filename where the trace output is to be written.
** "w ...", if present orders a size limited wrapping log.
** <WrapSize>, an unsigned integer:
**    The size limit of each log file.
** <WrapCnt>, an unsigned integer:
**    The number of log files.
** <TailIndex>, an unsigned integer:
**    The (zero based) index of where to insert the filename
**    sequence count "a".."z","aa".."az","ba".."zz","aaa"...
**
** Port control messages handled:
** 'f' -> '\0' (ok) | '\1' ++ String (error) : Flush file.
**
** The package written to the file looks like this:
** +--+--------+-----------------------------------+
** |Op|Size NBO|Term in external format or empty   |
** +--+--------+-----------------------------------+
** Op, a char, for conformance with the IP driver:
**    0 = binary, 1 = drop
**    If Op is 1, then Size reflects the number of dropped messages. The 
**    op 1 is never used in this driver.
** Size, a 32 bit interger in network byte order:
**    Either the size of the binary term, or the number of packet's dropped.
** Term, an array of bytes:
**    An erlang term in the external format or simply empty if Op == 1, the
**    term is Size long.
*/ 

typedef int FILETYPE;

#define BUFFER_SIZE (BUFSIZ*8)

#define OP_BINARY 0
#define OP_DROP   1

/*
** State structures
*/

typedef struct trace_file_name {
    char name[MAXPATHLEN+1]; /* Incl. space for terminating '\0' */
    unsigned suffix;         /* Index of suffix start */
    unsigned tail;           /* Index of tail start */
    unsigned len;            /* Total length (strlen) */
    unsigned cnt;            /* Current file count 0 <= cnt <= n */
    unsigned n;              /* Number of files */
} TraceFileName;

typedef struct trace_file_wrap_data {
    TraceFileName cur;  /* Current trace file */
    TraceFileName del;  /* Next file to delete when wrapping */
    unsigned      size; /* File max size */
    int           cnt;  /* How many remains before starting to wrap */
    unsigned long time; /* Time to pass until starting to delete old files */
    unsigned      len;  /* Current file len */
} TraceFileWrapData;

typedef struct trace_file_data {
    FILETYPE fd;
    ErlDrvPort port;
    struct trace_file_data *next, *prev;
    TraceFileWrapData *wrap; /* == NULL => no wrap */
    int buff_siz;
    int buff_pos;
    unsigned char buff[1]; /* You guessed it, will be longer... */
} TraceFileData;

static TraceFileData *first_data; 

/*
** Interface routines
*/
static ErlDrvData trace_file_start(ErlDrvPort port, char *buff);
static void trace_file_stop(ErlDrvData handle);
static void trace_file_output(ErlDrvData handle, char *buff,
			      ErlDrvSizeT bufflen);
static void trace_file_outputv(ErlDrvData handle, ErlIOVec *ev);
static void trace_file_finish(void);
static ErlDrvSSizeT trace_file_control(ErlDrvData handle,
				      unsigned int command, 
				      char* buff, ErlDrvSizeT count, 
				      char** res, ErlDrvSizeT res_size);
static void trace_file_timeout(ErlDrvData handle);

/*
** Internal routines
*/
static unsigned digits(unsigned n);
static void next_name(TraceFileName *tfn);
static void *my_alloc(size_t size);
static int my_write(TraceFileData *data, unsigned char *buff, int siz);
static int my_flush(TraceFileData *data);
static void put_be(unsigned n, unsigned char *s);
static void close_unlink_port(TraceFileData *data); 
static int wrap_file(TraceFileData *data);
#ifdef __WIN32__
static int win_open(char *path, int flags, int mask);
#define open win_open
#else
ErlDrvEntry *driver_init(void);
#endif

/*
** The driver struct
*/
ErlDrvEntry trace_file_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    trace_file_start,      /* L_PTR start, called when port is opened */
    trace_file_stop,       /* F_PTR stop, called when port is closed */
    trace_file_output,     /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "trace_file_drv",      /* char *driver_name, the argument to open_port */
    trace_file_finish,     /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    trace_file_control,    /* F_PTR control, port_control callback */
    trace_file_timeout,    /* F_PTR timeout, driver_set_timer callback */
    trace_file_outputv,    /* F_PTR outputv, reserved */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};

/*
** Driver initialization routine
*/
DRIVER_INIT(trace_file_drv)
{
    first_data = NULL;
    return &trace_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData trace_file_start(ErlDrvPort port, char *buff)
{
    unsigned size, cnt, time, tail, len;
    char *p;
    TraceFileData     *data;
    TraceFileWrapData *wrap;
    FILETYPE fd;
    int n, w;
    static const char name[] = "trace_file_drv";


#ifdef HARDDEBUG
    fprintf(stderr,"hello (%s)\r\n", buff);
#endif
    w = 0; /* Index of where sscanf gave up */
    size = 0; /* Warning elimination */
    cnt = 0;  /* -""- */
    time = 0;  /* -""- */
    tail = 0; /* -""- */
    n = sscanf(buff, "trace_file_drv %n w %u %u %u %u %n",
	       &w, &size, &cnt, &time, &tail, &w);

    if (w < sizeof(name) || (n != 0 && n != 4))
	return ERL_DRV_ERROR_BADARG;

    /* Search for "n <Filename>" in the rest of the string */
    p = buff + w;
    for (p = buff + w; *p == ' '; p++); /* Skip space (necessary?) */
    if (*p++ != 'n')
	return ERL_DRV_ERROR_BADARG;
    if (*p++ != ' ')
	return ERL_DRV_ERROR_BADARG;
    /* Here we are at the start of the filename; p */
    len = strlen(p);
    if (tail >= len)
	/* Tail must start within filename */
	return ERL_DRV_ERROR_BADARG;

    data = my_alloc(sizeof(TraceFileData) - 1 + BUFFER_SIZE);

    /* We have to check the length in case we are running on 
     * VxWorks since too long pathnames may cause bus errors
     * instead of error return from file operations.
     */
    if (n == 4) {
	/* Size limited wrapping log */
	unsigned d = digits(cnt); /* Nof digits in filename counter */
	if (len+d >= MAXPATHLEN) {
	    errno = ENAMETOOLONG; 
	    return ERL_DRV_ERROR_ERRNO;
	}
	wrap = my_alloc(sizeof(TraceFileWrapData));
	wrap->size = size;
	wrap->cnt = cnt;
	wrap->time = time;
	wrap->len = 0;
	strcpy(wrap->cur.name, p);
	wrap->cur.suffix = tail;
	wrap->cur.tail = tail;
	wrap->cur.len = len;
	wrap->cur.cnt = cnt;
	wrap->cur.n = cnt;
	next_name(&wrap->cur); /* Incr to suffix "0" */
	wrap->del = wrap->cur; /* Struct copy! */
	p = wrap->cur.name; /* Use new name for open */
    } else {
	/* Regular log */
	if (len >= MAXPATHLEN) {
	    errno = ENAMETOOLONG; 
	    return ERL_DRV_ERROR_ERRNO;
	}
	wrap = NULL;
    }

    if ((fd = open(p, O_WRONLY | O_TRUNC | O_CREAT
#ifdef O_BINARY
		   | O_BINARY
#endif
		   , 0777)) < 0) {
	int saved_errno = errno;
	if (wrap)
	    driver_free(wrap);
	driver_free(data);
	errno = saved_errno;
	return ERL_DRV_ERROR_ERRNO;
    } 

    data->fd = fd;
    data->port = port;
    data->buff_siz = BUFFER_SIZE;
    data->buff_pos = 0;
    data->wrap = wrap;

    if (first_data) {
	data->prev = first_data->prev;
	first_data->prev = data;
    } else
	data->prev = NULL;
    data->next = first_data;
    first_data = data;

    if (wrap && wrap->time > 0)
	driver_set_timer(port, wrap->time);

    return (ErlDrvData) data;
}


/*
** Close a port
*/
static void trace_file_stop(ErlDrvData handle)
{
    close_unlink_port((TraceFileData *) handle);
}

/*
** Data sent from erlang to port.
*/
static void trace_file_outputv(ErlDrvData handle, ErlIOVec *ev)
{
    int i;
    for (i = 0; i < ev->vsize; i++) {
        if (ev->iov[i].iov_len)
            trace_file_output(handle, ev->iov[i].iov_base,
                              ev->iov[i].iov_len);
    }
}

static void trace_file_output(ErlDrvData handle, char *buff,
			      ErlDrvSizeT bufflen)
{
    int heavy = 0;
    TraceFileData *data = (TraceFileData *) handle;
    unsigned char b[5] = "";
    put_be((unsigned) bufflen, b + 1);
    switch (my_write(data, (unsigned char *) b, sizeof(b))) {
    case 1:
	heavy = !0;
    case 0:
	switch (my_write(data, (unsigned char *) buff, bufflen)) {
	case 1:
	    heavy = !0;
	case 0:
	    break;
	case -1:
	    driver_failure_posix(data->port, errno); /* XXX */
	    return;
	}
	break;
    case -1:
	driver_failure_posix(data->port, errno); /* XXX */
	return;
    }
    if (data->wrap) {
	TraceFileWrapData *wrap = data->wrap;
	/* Size limited wrapping log files */
	wrap->len += sizeof(b) + bufflen;
	if (wrap->time == 0 && wrap->len >= wrap->size) {
	    if (wrap_file(data) < 0) {
		driver_failure_posix(data->port, errno); /* XXX */
		return;
	    }
	    heavy = !0;
	}
    }
    if (heavy) {
	set_port_control_flags(data->port, PORT_CONTROL_FLAG_HEAVY);
    }
}

/*
** Control message from erlang, we handle $f, which is flush.
*/
static ErlDrvSSizeT trace_file_control(ErlDrvData handle,
				       unsigned int command, 
				       char* buff, ErlDrvSizeT count, 
				       char** res, ErlDrvSizeT res_size)
{
    if (command == 'f') {
	TraceFileData *data = (TraceFileData *) handle;
	if (my_flush(data) < 0) {
	    driver_failure_posix(data->port, errno); /* XXX */
	}
	if (res_size < 1) {
	    *res = my_alloc(1);
	}
	**res = '\0';
	return 1;
    } 
    return -1;
}

/*
** Timeout from driver_set_timer.
*/
static void trace_file_timeout(ErlDrvData handle) {
    TraceFileData *data = (TraceFileData *) handle;
    if (data->wrap) {
	if (wrap_file(data) < 0) {
	    driver_failure_posix(data->port, errno); /* XXX */
	    return;
	} else {
	    driver_set_timer(data->port, data->wrap->time);
	}
    }
}

/*
** Driver unloaded
*/
static void trace_file_finish(void)
{
    while (first_data != NULL) {
	close_unlink_port(first_data);
    }
}

/*
** Internal helpers
*/

/* Calculate needed number of digits in filename counter.
**/
static unsigned digits(unsigned n) {
    unsigned m, i;
    for (m = 10, i = 1;  n >= m;  i++, m *= 10) ;
    return i;
}

/*
** Increment filename.
**
** The filename counter counts "0"-"9","10"-"19"..."[n->n]","0"...,
** but also "","0" which is used for initialization.
*/
static void next_name(TraceFileName *n) {
    if (n->cnt >= n->n) {
	n->cnt = 0;
	/* Circular count from "[n->n]" to "0", or from "" to "0" */
	memmove(&n->name[n->suffix+1], 
		&n->name[n->tail], 
		n->len+1 - n->tail); /* Including '\0' */
	n->name[n->suffix] = '0';
	n->len -= n->tail - n->suffix - 1;
	n->tail = n->suffix + 1;
    } else {
	int i = n->tail;
	n->cnt++;
	do {
	    i--;
	    /* Increment from the end, 
	     * '0'..'1', carry propagate forward */
	    if (n->name[i] < '9') {
		n->name[i]++;
		return;
	    } else
		n->name[i] = '0';
	} while (i > n->suffix);
	/* Wrapped around from "99..99" to "00..00", 
	 * need one more character */
	memmove(&n->name[n->tail+1],
		&n->name[n->tail],
		n->len+1 - n->tail); /* Incl '\0' */
	n->name[n->tail++] = '0';
	n->name[n->suffix] = '1';
	n->len++;
    }
}

/*
** Yet another malloc wrapper
*/
static void *my_alloc(size_t size) 
{
    void *ret;
    if ((ret = (void *) driver_alloc(size)) == NULL) {
	/* May or may not work... */
	fprintf(stderr, "Could not allocate %d bytes of memory in %s.",
		(int) size, __FILE__);
	exit(1);
    }
    return ret;
}

/*
** A write wrapper that regards it as an error if not all data was written.
*/
static int do_write(FILETYPE fd, unsigned char *buff, int siz) {
    int w;
    while (1) {
	w = write(fd, buff, siz);
	if (w < 0 && errno == EINTR)
	    continue;
	else if (w != siz) {
	    if (w >= 0) {
		errno = ENOSPC;
	    }
	    return -1;
	}
	return siz;
    }
}

/*
** Returns 0 if write to cache, 1 i write to file, and -1 if write failed.
*/
static int my_write(TraceFileData *data, unsigned char *buff, int siz) 
{
    int wrote;

    if (data->buff_siz - data->buff_pos >= siz) {
	memcpy(data->buff + data->buff_pos, buff, siz);
	data->buff_pos += siz; 
	return 0;
    }
    
    wrote = data->buff_siz - data->buff_pos;
    memcpy(data->buff + data->buff_pos, buff, wrote);
    if (do_write(data->fd, data->buff, data->buff_siz) < 0) {
	return -1;
    }
    data->buff_pos = 0;
    if (siz - wrote >= data->buff_siz) {
	/* Write directly, no need to buffer... */
	if (do_write(data->fd, buff + wrote, siz - wrote) < 0) {
	    return -1;
	}
	return 1;
    } 
    memcpy(data->buff, buff + wrote, siz - wrote);
    data->buff_pos = siz - wrote;
    set_port_control_flags(data->port, PORT_CONTROL_FLAG_HEAVY);
    return 1;
}

/* 
** Returns negative if it failed to write.
 */
static int my_flush(TraceFileData *data)
{
    if (do_write(data->fd, data->buff, data->buff_pos) < 0) {
	return -1;
    }
    data->buff_pos = 0;
    return 0;
}

/*
** Write unsigned to buffer in big endian
*/
static void put_be(unsigned n, unsigned char *s)
{
    s[0] = n >> 24;
    s[1] = (n >> 16) & 0xFFFFU;
    s[2] = (n >> 8) & 0xFFFFU;
    s[3] = n & 0xFFFFU;
}

/*
** Wrapper that only closes non-negative filehandles
*/
static void do_close(FILETYPE fd) {
    if (fd != -1) {
	close(fd);
    }
}

/*
** Close the whole port and clean up
*/
static void close_unlink_port(TraceFileData *data) 
{
    my_flush(data);
    do_close(data->fd);

    if (data->next)
	data->next->prev = data->prev;
    if (data->prev)
	data->prev->next = data->next;
    else
	first_data = data->next;

    if (data->wrap)
	driver_free(data->wrap);
    driver_free(data);
}

/*
** Wrap to new file - close the current, open a new and
** perhaps delete a too old one
**
** Returns negative if something failed.
*/
static int wrap_file(TraceFileData *data) {
    if (my_flush(data) < 0) {
	int saved_errno = errno;
	close(data->fd);
	data->fd = -1;
	errno = saved_errno;
	return -1;
    }
    close(data->fd);
    data->fd = -1;
    data->buff_pos = 0;
    data->wrap->len = 0;
    /* Count down before starting to remove old files */
    if (data->wrap->cnt > 0)
	data->wrap->cnt--;
    if (data->wrap->cnt == 0) {
	/* Remove an old file */
	unlink(data->wrap->del.name);
	next_name(&data->wrap->del);
    }
    next_name(&data->wrap->cur);
try_open:
    data->fd = open(data->wrap->cur.name, O_WRONLY | O_TRUNC | O_CREAT
#ifdef O_BINARY
	      | O_BINARY
#endif
	      , 0777);
    if (data->fd < 0) {
	if (errno == EINTR)
	    goto try_open;
	data->fd = -1;
	return -1;
    }
    return 0;
}

#ifdef __WIN32__
static int win_open(char *path, int flags, int mask)
{
  DWORD access = 0;
  DWORD creation = 0;
  HANDLE fd;
  int ret;
  if (flags & O_WRONLY) {
    access =  GENERIC_WRITE;
  } else if (flags & O_RDONLY) {
    access = GENERIC_READ;
  } else {
    access = (GENERIC_READ | GENERIC_WRITE);
  } 
  
  if (flags & O_CREAT) {
    creation |= CREATE_ALWAYS;
  }  else {
     creation |= OPEN_ALWAYS;
  }

  fd = CreateFileA(path, access,  
		   FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, 
		   NULL, creation, FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd == INVALID_HANDLE_VALUE) {
    
    return -1;
  }
  
  if ((ret = _open_osfhandle((intptr_t)fd, (flags & O_RDONLY) ? O_RDONLY : 0))
      < 0) {
    CloseHandle(fd);
  }

  return ret;
}
#endif
