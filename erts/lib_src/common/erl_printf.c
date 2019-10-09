/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

/* Without this, variable argument lists break on VxWorks */
#ifdef VXWORKS
#include <vxWorks.h>
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(__sun) || defined(__sun__)
    /* For flockfile(3c), putc_unlocked(3c), etc */
    #define __EXTENSIONS__
#endif

#include <string.h>
#include "erl_errno.h"
#ifdef __WIN32__
#	include <io.h>
#else
#	include <unistd.h>
#endif
#include "erl_printf.h"
#include "erl_printf_format.h"

#ifdef DEBUG
#include <assert.h>
#define ASSERT(X) assert(X)
#else
#define ASSERT(X) 
#endif

#if defined(__WIN32__) && !defined(__GNUC__)
typedef int ssize_t;
#endif

int (*erts_printf_stdout_func)(char *, va_list) = NULL;
int (*erts_printf_stderr_func)(char *, va_list) = NULL;

int erts_printf_add_cr_to_stdout = 0;
int erts_printf_add_cr_to_stderr = 0;

int (*erts_printf_block_fpe)(void) = NULL;
void (*erts_printf_unblock_fpe)(int) = NULL;

#undef FLOCKFILE
#undef FUNLOCKFILE
#undef PUTC
#undef FWRITE
#undef PUTC_ON_SMALL_WRITES

#if defined(HAVE_FLOCKFILE)
#	define FLOCKFILE(FP)	flockfile(FP)
#	define FUNLOCKFILE(FP)	funlockfile(FP)
#	ifdef HAVE_PUTC_UNLOCKED
#		define PUTC	putc_unlocked
#		define PUTC_ON_SMALL_WRITES
#	endif
#	ifdef HAVE_FWRITE_UNLOCKED
#		define FWRITE	fwrite_unlocked
#	endif
#else
#	define FLOCKFILE(FP)
#	define FUNLOCKFILE(FP)
#endif
#ifndef PUTC
#	define PUTC putc
#endif
#ifndef FWRITE
#	define FWRITE fwrite
#endif

/* We use write for stdout and stderr as they could be
   set to non-blocking by shell drivers, and non-blocking
   FILE * functions work unpredictably as best */
static int
printf_putc(int c, FILE *stream) {
    if ((FILE*)stream == stdout || (FILE*)stream == stderr) {
        int fd = stream == stdout ? fileno(stdout) : fileno(stderr);
        /* cast to a char here, because write expects bytes. */
        unsigned char buf[1] = { c };
        int res;
        do {
            res = write(fd, buf, 1);
        } while (res == -1 && (errno == EAGAIN || errno == EINTR));
        if (res == -1) return EOF;
        return res;
    }

    return PUTC(c, stream);
}

static size_t
printf_fwrite(const void *ptr, size_t size, size_t nitems,
          FILE *stream) {
    if ((FILE*)stream == stdout || (FILE*)stream == stderr) {
        int fd = stream == stdout ? fileno(stdout) : fileno(stderr);
        int res;
        do {
            res = write(fd, ptr, size*nitems);
        } while (res == -1 && (errno == EAGAIN || errno == EINTR));
        if (res == -1) return 0;
        return res;
    }
    return FWRITE(ptr, size, nitems, stream);
}

static int
get_error_result(void)
{
    int res = errno;
    if (res <= 0)
	res = EIO;
    return -res;
}


static int
write_f_add_cr(void *vfp, char* buf, size_t len)
{
    size_t i;
    ASSERT(vfp);
    for (i = 0; i < len; i++) {
        if (buf[i] == '\n' && printf_putc('\r', (FILE *) vfp) == EOF)
            return get_error_result();
        if (printf_putc(buf[i], (FILE *) vfp) == EOF)
            return get_error_result();
    }
    return len;
}

int
erts_write_fp(void *vfp, char* buf, size_t len)
{
    ASSERT(vfp);
#ifdef PUTC_ON_SMALL_WRITES
    if (len <= 64) { /* Try to optimize writes of small bufs. */
	int i;
	for (i = 0; i < len; i++)
	    if (printf_putc(buf[i], (FILE *) vfp) == EOF)
		return get_error_result();
    }
    else
#endif
    if (printf_fwrite((void *) buf, sizeof(char), len, (FILE *) vfp) != len)
	return get_error_result();
    return len;
}

int
erts_write_fd(void *vfdp, char* buf, size_t len)
{
    ssize_t size;
    size_t res = len;
    ASSERT(vfdp);

    while (len) {
	size = write(*((int *) vfdp), (void *) buf, len);
	if (size < 0) {
#ifdef EINTR
	    if (errno == EINTR)
		continue;
#endif
	    return get_error_result();
	}
	if (size > len)
	    return -EIO;
	len -= size;
    }

    return res;
}

static int
write_s(void *vwbufpp, char* bufp, size_t len)
{
    char **wbufpp = (char **) vwbufpp;
    ASSERT(wbufpp && *wbufpp);
    ASSERT(len > 0);
    memcpy((void *) *wbufpp, (void *) bufp, len);
    *wbufpp += len;
    return len;
}


typedef struct {
    char *buf;
    size_t len;
} write_sn_arg_t;

static int
write_sn(void *vwsnap, char* buf, size_t len)
{
    int rv = 0;
    write_sn_arg_t *wsnap = (write_sn_arg_t *) vwsnap;
    ASSERT(wsnap);
    ASSERT(len > 0);
    if (wsnap->len > 0) {
	size_t sz = len;
	if (sz >= wsnap->len)
	    sz = wsnap->len;
	rv = (int)sz;
	memcpy((void *) wsnap->buf, (void *) buf, sz);
	wsnap->buf += sz;
	wsnap->len -= sz;
	return sz;
    }
    return rv;
}

int
erts_write_ds(void *vdsbufp, char* buf, size_t len)
{
    erts_dsprintf_buf_t *dsbufp = (erts_dsprintf_buf_t *) vdsbufp;
    size_t need_len = len + 1; /* Also trailing '\0' */
    ASSERT(dsbufp);
    ASSERT(len > 0);
    ASSERT(dsbufp->str_len <= dsbufp->size);
    if (need_len > dsbufp->size - dsbufp->str_len) {
	dsbufp = (*dsbufp->grow)(dsbufp, need_len);
	if (!dsbufp)
	    return -ENOMEM;
    }
    memcpy((void *) (dsbufp->str + dsbufp->str_len), (void *) buf, len);
    dsbufp->str_len += len;
    return len;
}

int
erts_printf(const char *format, ...)
{
    int res;
    va_list arglist;
    va_start(arglist, format);
    errno = 0;
    if (erts_printf_stdout_func)
	res = (*erts_printf_stdout_func)((char *) format, arglist);
    else {
	FLOCKFILE(stdout);
	res = erts_printf_format(erts_printf_add_cr_to_stdout
				 ? write_f_add_cr
				 : erts_write_fp,
				 (void *) stdout,
				 (char *) format,
				 arglist);
	FUNLOCKFILE(stdout);
    }
    va_end(arglist);
    return res;
}

int
erts_fprintf(FILE *filep, const char *format, ...)
{
    int res;
    va_list arglist;
    va_start(arglist, format);
    errno = 0;
    if (erts_printf_stdout_func && filep == stdout)
	res = (*erts_printf_stdout_func)((char *) format, arglist);
    else if (erts_printf_stderr_func && filep == stderr)
	res = (*erts_printf_stderr_func)((char *) format, arglist);
    else {
	int (*fmt_f)(void*, char*, size_t);
	if (erts_printf_add_cr_to_stdout && filep == stdout)
	    fmt_f = write_f_add_cr;
	else if (erts_printf_add_cr_to_stderr && filep == stderr)
	    fmt_f = write_f_add_cr;
	else
	    fmt_f = erts_write_fp;
	FLOCKFILE(filep);
	res = erts_printf_format(fmt_f,(void *)filep,(char *)format,arglist);
	FUNLOCKFILE(filep);
    }
    va_end(arglist);
    return res;
}

int
erts_fdprintf(int fd, const char *format, ...)
{
    int res;
    va_list arglist;
    va_start(arglist, format);
    errno = 0;
    res = erts_printf_format(erts_write_fd,(void *)&fd,(char *)format,arglist);
    va_end(arglist);
    return res;
}

int
erts_sprintf(char *buf, const char *format, ...)
{
    int res;
    char *p = buf;
    va_list arglist;
    va_start(arglist, format);
    errno = 0;
    res = erts_printf_format(write_s, (void *) &p, (char *) format, arglist);
    if (res < 0)
	buf[0] = '\0';
    else
	buf[res] = '\0';
    va_end(arglist);
    return res;
}

int
erts_snprintf(char *buf, size_t size, const char *format, ...)
{
    write_sn_arg_t wsnap;
    int res;
    va_list arglist;
    if (size < 1)
	return -EINVAL;
    wsnap.buf = buf;
    wsnap.len = size-1; /* Always need room for trailing '\0' */
    va_start(arglist, format);
    errno = 0;
    res = erts_printf_format(write_sn, (void *)&wsnap, (char *)format, arglist);
    if (res < 0)
	buf[0] = '\0';
    else if (res < size)
	buf[res] = '\0';
    else
	buf[size-1] = '\0';
    va_end(arglist);
    return res;
}
 
int
erts_dsprintf(erts_dsprintf_buf_t *dsbufp, const char *format, ...)
{
    int res;
    va_list arglist;
    if (!dsbufp)
	return -EINVAL;
    va_start(arglist, format);
    errno = 0;
    res = erts_printf_format(erts_write_ds, (void *)dsbufp, (char *)format, arglist);
    if (dsbufp->str) {
	if (res < 0)
	    dsbufp->str[0] = '\0';
	else
	    dsbufp->str[dsbufp->str_len] = '\0';
    }
    va_end(arglist);
    return res;
}

/*
 * Callback printf
 */
int erts_cbprintf(fmtfn_t cb_fn, void* cb_arg, const char* format, ...)
{
    int res;
    va_list arglist;
    va_start(arglist, format);
    errno = 0;
    res = erts_printf_format(cb_fn, cb_arg, (char *)format, arglist);
    va_end(arglist);
    return res;
}

int
erts_vprintf(const char *format, va_list arglist)
{	
    int res;
    if (erts_printf_stdout_func)
	res = (*erts_printf_stdout_func)((char *) format, arglist);
    else {
	errno = 0;
	res = erts_printf_format(erts_printf_add_cr_to_stdout
				 ? write_f_add_cr
				 : erts_write_fp,
				 (void *) stdout,
				 (char *) format,
				 arglist);
    }
    return res;
}

int
erts_vfprintf(FILE *filep, const char *format, va_list arglist)
{
    int res;
    if (erts_printf_stdout_func && filep == stdout)
	res = (*erts_printf_stdout_func)((char *) format, arglist);
    else if (erts_printf_stderr_func && filep == stderr)
	res = (*erts_printf_stderr_func)((char *) format, arglist);
    else {
	int (*fmt_f)(void*, char*, size_t);
	errno = 0;
	if (erts_printf_add_cr_to_stdout && filep == stdout)
	    fmt_f = write_f_add_cr;
	else if (erts_printf_add_cr_to_stderr && filep == stderr)
	    fmt_f = write_f_add_cr;
	else
	    fmt_f = erts_write_fp;
	FLOCKFILE(filep);
	res = erts_printf_format(fmt_f,(void *)filep,(char *)format,arglist);
	FUNLOCKFILE(filep);
    }
    return res;
}

int
erts_vfdprintf(int fd, const char *format, va_list arglist)
{
    int res;
    errno = 0;
    res = erts_printf_format(erts_write_fd,(void *)&fd,(char *)format,arglist);
    return res;
}

int
erts_vsprintf(char *buf, const char *format, va_list arglist)
{
    int res;
    char *p = buf;
    errno = 0;
    res = erts_printf_format(write_s, (void *) &p, (char *) format, arglist);
    if (res < 0)
	buf[0] = '\0';
    else
	buf[res] = '\0';
    return res;
}

int
erts_vsnprintf(char *buf, size_t size, const char *format,  va_list arglist)
{
    write_sn_arg_t wsnap;
    int res;
    if (size < 1)
	return -EINVAL;
    wsnap.buf = buf;
    wsnap.len = size-1; /* Always need room for trailing '\0' */
    errno = 0;
    res = erts_printf_format(write_sn, (void *)&wsnap, (char *)format, arglist);
    if (res < 0)
	buf[0] = '\0';
    else if (res < size)
	buf[res] = '\0';
    else
	buf[size-1] = '\0';
    return res;
}

int
erts_vdsprintf(erts_dsprintf_buf_t *dsbufp, const char *format, va_list arglist)
{
    int res;
    if (!dsbufp)
	return -EINVAL;
    errno = 0;
    res = erts_printf_format(erts_write_ds, (void *)dsbufp, (char *)format, arglist);
    if (dsbufp->str) {
	if (res < 0)
	    dsbufp->str[0] = '\0';
	else
	    dsbufp->str[dsbufp->str_len] = '\0';
    }
    return res;
}

int
erts_vcbprintf(fmtfn_t cb_fn, void* cb_arg, const char *format, va_list arglist)
{
    errno = 0;
    return erts_printf_format(cb_fn, cb_arg, (char *)format, arglist);
}
