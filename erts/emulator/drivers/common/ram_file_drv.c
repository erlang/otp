/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 * RAM File operations
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

/* Operations */

/* defined "file" functions  */
#define RAM_FILE_OPEN		1
#define RAM_FILE_READ		2
#define RAM_FILE_LSEEK		3
#define RAM_FILE_WRITE		4
#define RAM_FILE_FSYNC          9
#define RAM_FILE_TRUNCATE      14
#define RAM_FILE_PREAD         17
#define RAM_FILE_PWRITE        18
#define RAM_FILE_FDATASYNC     19

/* other operations */
#define RAM_FILE_GET           30
#define RAM_FILE_SET           31
#define RAM_FILE_GET_CLOSE     32  /* get_file/close */
#define RAM_FILE_COMPRESS      33  /* compress file */
#define RAM_FILE_UNCOMPRESS    34  /* uncompress file */
#define RAM_FILE_UUENCODE      35  /* uuencode file */
#define RAM_FILE_UUDECODE      36  /* uudecode file */
#define RAM_FILE_SIZE          37  /* get file size */
#define RAM_FILE_ADVISE        38  /* predeclare the access
                                    * pattern for file data */
#define RAM_FILE_ALLOCATE      39  /* allocate space for a file */
/* possible new operations include:
   DES_ENCRYPT
   DES_DECRYPT
   CRC-32, CRC-16, CRC-CCITT
   IP-CHECKSUM
*/

/*
 * Open modes for RAM_FILE_OPEN.
 */
#define RAM_FILE_MODE_READ       1
#define RAM_FILE_MODE_WRITE      2  /* Implies truncating file 
				     * when used alone. */
#define RAM_FILE_MODE_READ_WRITE 3

/*
 * Seek modes for RAM_FILE_LSEEK.
 */
#define	RAM_FILE_SEEK_SET	0
#define	RAM_FILE_SEEK_CUR	1
#define	RAM_FILE_SEEK_END	2

/* Return codes */

#define RAM_FILE_RESP_OK         0
#define RAM_FILE_RESP_ERROR      1
#define RAM_FILE_RESP_DATA       2
#define RAM_FILE_RESP_NUMBER     3
#define RAM_FILE_RESP_INFO       4

#include <stdio.h>
#include <ctype.h>
#include <limits.h>

#include "sys.h"
#include "erl_driver.h"
#include "zlib.h"
#include "gzio.h"

#ifndef NULL
#define NULL ((void*)0)
#endif

#define BFILE_BLOCK  1024

typedef unsigned char uchar;

static ErlDrvData rfile_start(ErlDrvPort, char*);
static int rfile_init(void);
static void rfile_stop(ErlDrvData);
static void rfile_command(ErlDrvData, char*, ErlDrvSizeT);


struct erl_drv_entry ram_file_driver_entry = {
    rfile_init,
    rfile_start,
    rfile_stop,
    rfile_command,
    NULL,
    NULL,
    "ram_file_drv",
    NULL,
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
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

/* A File is represented as a array of bytes, this array is
   reallocated when needed. A possibly better implementation
   whould be to have a vector of blocks. This may be implemented
   when we have the commandv/driver_outputv
*/
typedef struct ram_file {
    ErlDrvPort port;	/* the associcated port */
    int flags;          /* flags read/write */
    ErlDrvBinary* bin;  /* binary to hold binary file */
    char* buf;          /* buffer start (in binary) */
    ErlDrvSSizeT size;   /* buffer size (allocated) */
    ErlDrvSSizeT cur;    /* current position in buffer */
    ErlDrvSSizeT end;            /* end position in buffer */
} RamFile;

#ifdef LOADABLE
static int rfile_finish(DriverEntry* drv)
{
    return 0;
}

DriverEntry* driver_init(void *handle)
{
    ram_file_driver_entry.handle = handle;
    ram_file_driver_entry.driver_name = "ram_file_drv";
    ram_file_driver_entry.finish = rfile_finish;
    ram_file_driver_entry.init = rfile_init;
    ram_file_driver_entry.start = rfile_start;
    ram_file_driver_entry.stop = rfile_stop;
    ram_file_driver_entry.output = rfile_command;
    ram_file_driver_entry.ready_input = NULL;
    ram_file_driver_entry.ready_output = NULL;
    return &ram_file_driver_entry;
}
#endif

static int rfile_init(void)
{
    return 0;
}

static ErlDrvData rfile_start(ErlDrvPort port, char* buf)
{
    RamFile* f;

    if ((f = (RamFile*) driver_alloc(sizeof(RamFile))) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    f->port = port;
    f->flags = 0;
    f->bin = NULL;
    f->buf = NULL;
    f->size = f->cur = f->end = 0;
    return (ErlDrvData)f;
}

static void rfile_stop(ErlDrvData e)
{
    RamFile* f = (RamFile*)e;
    if (f->bin != NULL) 
	driver_free_binary(f->bin);
    driver_free(f);
}

/*
 * Sends back an error reply to Erlang.
 */

static int error_reply(RamFile *f, int err)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------+
     * | RAM_FILE_RESP_ERROR | Posix error id string |
     * +-----------------------------------------+
     */
    response[0] = RAM_FILE_RESP_ERROR;
    for (s = erl_errno_id(err), t = response+1; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(f->port, response, t-response, NULL, 0);
    return 0;
}

static int reply(RamFile *f, int ok, int err)
{
    if (!ok)
	error_reply(f, err);
    else {
	char c = RAM_FILE_RESP_OK;
        driver_output2(f->port, &c, 1, NULL, 0);
    }
    return 0;
}

static int numeric_reply(RamFile *f, ErlDrvSSizeT result)
{
    char tmp[5];

    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------------+
     * | RAM_FILE_RESP_NUMBER | 32-bit number (big-endian) |
     * +-----------------------------------------------+
     */

    tmp[0] = RAM_FILE_RESP_NUMBER;
    put_int32(result, tmp+1);
    driver_output2(f->port, tmp, sizeof(tmp), NULL, 0);
    return 0;
}

/* install bin as the new binary reset all pointer */

static void ram_file_set(RamFile *f, ErlDrvBinary *bin,
			 ErlDrvSSizeT bsize, ErlDrvSSizeT len)
{
    f->size = bsize;
    f->buf = bin->orig_bytes;
    f->cur = 0;
    f->end = len;
    f->bin = bin;
}

static int ram_file_init(RamFile *f, char *buf, ErlDrvSSizeT count, int *error)
{
    ErlDrvSSizeT bsize;
    ErlDrvBinary* bin;
    
    if (count < 0) {
	*error = EINVAL;
	return -1;
    }
    if ((bsize = (count+BFILE_BLOCK+(BFILE_BLOCK>>1)) & ~(BFILE_BLOCK-1))
	< 0) {
	bsize = INT_MAX;
    }

    if (f->bin == NULL)
	bin = driver_alloc_binary(bsize);
    else 
	bin = driver_realloc_binary(f->bin, bsize);
    if (bin == NULL) {
	*error = ENOMEM;
	return -1;
    }
    sys_memzero(bin->orig_bytes, bsize);
    sys_memcpy(bin->orig_bytes, buf, count);
    ram_file_set(f, bin, bsize, count);
    return count;
}

static ErlDrvSSizeT ram_file_expand(RamFile *f, ErlDrvSSizeT size, int *error)
{
    ErlDrvSSizeT bsize;
    ErlDrvBinary* bin;
    
    if (size < 0) {
	*error = EINVAL;
	return -1;
    }
    if ((bsize = (size+BFILE_BLOCK+(BFILE_BLOCK>>1)) & ~(BFILE_BLOCK-1))
	< 0) {
	bsize = INT_MAX;
    }
    
    if (bsize <= f->size)
	return f->size;
    else {
	if ((bin = driver_realloc_binary(f->bin, bsize)) == NULL) {
	    *error = ENOMEM;
	    return -1;
	}
	sys_memzero(bin->orig_bytes+f->size, bsize - f->size);
	f->size = bsize;
	f->buf = bin->orig_bytes;
	f->bin = bin;
	return bsize;
    }
}


static ErlDrvSSizeT ram_file_write(RamFile *f, char *buf, ErlDrvSSizeT len,
			  ErlDrvSSizeT *location, int *error)
{
    ErlDrvSSizeT cur = f->cur;
    
    if (!(f->flags & RAM_FILE_MODE_WRITE)) {
	*error = EBADF;
	return -1;
    }
    if (location) cur = *location;
    if (cur < 0 || len < 0 || cur+len < 0) {
	*error = EINVAL;
	return -1;
    }
    if (cur+len > f->size && ram_file_expand(f, cur+len, error) < 0) {
	return -1;
    }
    if (len) sys_memcpy(f->buf+cur, buf, len);
    cur += len;
    if (cur > f->end) f->end = cur;
    if (! location) f->cur = cur;
    return len;
}

static ErlDrvSSizeT ram_file_read(RamFile *f, ErlDrvSSizeT len, ErlDrvBinary **bp,
			 ErlDrvSSizeT *location, int *error)
{
    ErlDrvBinary* bin;
    ErlDrvSSizeT cur = f->cur;
    
    if (!(f->flags & RAM_FILE_MODE_READ)) {
	*error = EBADF;
	return -1;
    }
    if (location) cur = *location;
    if (cur < 0 || len < 0) {
	*error = EINVAL;
	return -1;
    }
    if (cur < f->end) {
	if (len > f->end-cur) len = f->end - cur;
    } else {
	len = 0; /* eof */
    }
    if ((bin = driver_alloc_binary(len)) == NULL) {
	*error = ENOMEM;
	return -1;
    }
    if (len) sys_memcpy(bin->orig_bytes, f->buf+cur, len);
    *bp = bin;
    if (! location) f->cur = cur + len;
    return len;
}

static ErlDrvSSizeT ram_file_seek(RamFile *f, ErlDrvSSizeT offset, int whence,
				 int *error)
{
    ErlDrvSSizeT pos;

    if (f->flags == 0) {
	*error = EBADF;
	return -1;
    }	
    switch(whence) {
    case RAM_FILE_SEEK_SET: pos = offset; break;
    case RAM_FILE_SEEK_CUR: pos = f->cur + offset; break;
    case RAM_FILE_SEEK_END: pos = f->end + offset; break;
    default: *error = EINVAL; return -1;
    }
    if (pos < 0) {
	*error = EINVAL;
	return -1;
    }
    return f->cur = pos;
}

#define UUMASK(x)     ((x)&0x3F)
#define uu_encode(x)  (UUMASK(x)+32)

/* calculate max number of quadrauple bytes given max line length */
#define UULINE(n) ( (((n)-1) / 4) * 3)

#define UNIX_LINE 61  /* 61 character lines =>  45 uncoded => 60 coded */

#define uu_pack(p, c1, c2, c3) \
        (p)[0] = uu_encode((c1) >> 2), \
        (p)[1] = uu_encode(((c1) << 4) | ((c2) >> 4)), \
        (p)[2] = uu_encode(((c2) << 2) | ((c3) >> 6)), \
        (p)[3] = uu_encode(c3)

static int ram_file_uuencode(RamFile *f)
{
    ErlDrvSSizeT code_len = UULINE(UNIX_LINE);
    ErlDrvSSizeT len = f->end;
    ErlDrvSSizeT usize = 4*((len+2)/3) + 2*((len+code_len-1)/code_len) + 2;
    ErlDrvBinary* bin;
    uchar* inp;
    uchar* outp;
    ErlDrvSSizeT count = 0;

    if ((bin = driver_alloc_binary(usize)) == NULL)
	return error_reply(f, ENOMEM);
    outp = (uchar*)bin->orig_bytes;
    inp = (uchar*)f->buf;

    while(len > 0) {
        int c1, c2, c3;
        int n = (len >= code_len) ? code_len : len;

        len -= n;
        *outp++ = uu_encode(UUMASK(n));
        count++;
        while (n >= 3) {
            c1 = inp[0];
            c2 = inp[1];
            c3 = inp[2];
	    uu_pack(outp, c1, c2, c3);
            inp += 3; n -= 3;
            outp += 4; count += 4;
        }
        if (n == 2) {
            c1 = inp[0];
            c2 = inp[1];
	    uu_pack(outp, c1, c2, 0);
	    inp += 2;
            outp += 4; count += 4;
        }
        else if (n == 1) {
            c1 = inp[0];
	    uu_pack(outp, c1, 0, 0);
	    inp += 1;
            outp += 4; count += 4;
        }
        *outp++ = '\n';
        count++;
    }
    *outp++ = ' ';   /* this end of file 0 length !!! */
    *outp++ = '\n';
    count += 2;
    ASSERT(count == usize);
    driver_free_binary(f->bin);
    ram_file_set(f, bin, usize, count);
    return numeric_reply(f, count);
}


#define uu_decode(x)  ((x)-32)

static int ram_file_uudecode(RamFile *f)
{
    ErlDrvSSizeT len = f->end;
    ErlDrvSSizeT usize = ( (len+3) / 4 ) * 3;
    ErlDrvBinary* bin;
    uchar* inp;
    uchar* outp;
    int count = 0;
    int n;

    if ((bin = driver_alloc_binary(usize)) == NULL)
	return error_reply(f, ENOMEM);
    outp = (uchar*)bin->orig_bytes;
    inp  = (uchar*)f->buf;

    while(len > 0) {
	if ((n = uu_decode(*inp++)) < 0)
	    goto error;
        len--;
	if ((n == 0) && (*inp == '\n'))
	    break;
        count += n;     /* count characters */
        while((n > 0) && (len >= 4)) {
            int c1, c2, c3, c4;
            c1 = uu_decode(inp[0]);
            c2 = uu_decode(inp[1]);
            c3 = uu_decode(inp[2]);
            c4 = uu_decode(inp[3]);
	    inp += 4;
            len -= 4;

            switch(n) {
            case 1:
                *outp++ = (c1 << 2) | (c2 >> 4);
		n = 0;
                break;
            case 2:
                *outp++ = (c1 << 2) | (c2 >> 4);
                *outp++ = (c2 << 4) | (c3 >> 2);
		n = 0;
                break;
            default:
                *outp++ = (c1 << 2) | (c2 >> 4);
                *outp++ = (c2 << 4) | (c3 >> 2);
                *outp++ = (c3 << 6) | c4;
                n -= 3;
                break;
            }
        }
	if ((n != 0) || (*inp++ != '\n'))
	    goto error;
        len--;
    }
    driver_free_binary(f->bin);
    ram_file_set(f, bin, usize, count);
    return numeric_reply(f, count);

 error:
    driver_free_binary(bin);
    return error_reply(f, EINVAL);
}


static int ram_file_compress(RamFile *f)
{
    ErlDrvSSizeT size = f->end;
    ErlDrvBinary* bin;

    if ((bin = erts_gzdeflate_buffer(f->buf, size)) == NULL) {
	return error_reply(f, EINVAL);
    }
    driver_free_binary(f->bin);
    size = bin->orig_size;
    ram_file_set(f, bin, size, size);
    return numeric_reply(f, size);
}

/* Tricky since we dont know the expanded size !!! */
/* First attempt is to double the size of input */
/* loop until we don't get Z_BUF_ERROR */

static int ram_file_uncompress(RamFile *f)
{
    ErlDrvSSizeT size = f->end;
    ErlDrvBinary* bin;

    if ((bin = erts_gzinflate_buffer(f->buf, size)) == NULL) {
	return error_reply(f, EINVAL);
    }
    driver_free_binary(f->bin);
    size = bin->orig_size;
    ram_file_set(f, bin, size, size);
    return numeric_reply(f, size);
}


static void rfile_command(ErlDrvData e, char* buf, ErlDrvSizeT count)
{
    RamFile* f = (RamFile*)e;
    int error = 0;
    ErlDrvBinary* bin;
    char header[5];     /* result code + count */
    ErlDrvSSizeT offset;
    ErlDrvSSizeT origin;		/* Origin of seek. */
    ErlDrvSSizeT n;

    count--;
    switch(*(uchar*)buf++) {
    case RAM_FILE_OPEN:  /* args is initial data */
	f->flags = get_int32(buf);
	if (ram_file_init(f, buf+4, count-4, &error) < 0)
	    error_reply(f, error);
	else
	    numeric_reply(f, 0); /* 0 is not used */
	break;

    case RAM_FILE_FDATASYNC:
	if (f->flags == 0)
	    error_reply(f, EBADF);
	else
	    reply(f, 1, 0);
	break;

    case RAM_FILE_FSYNC:
	if (f->flags == 0)
	    error_reply(f, EBADF);
	else
	    reply(f, 1, 0);
	break;

    case RAM_FILE_WRITE:
	if (ram_file_write(f, buf, count, NULL, &error) < 0)
	    error_reply(f, error);
	else
	    numeric_reply(f, count);
	break;

    case RAM_FILE_PWRITE:
        if ((offset = get_int32(buf)) < 0)
	    error_reply(f, EINVAL);
	else if (ram_file_write(f, buf+4, count-4, &offset, &error) < 0)
	    error_reply(f, error);
	else
	    numeric_reply(f, count-4);
	break;

    case RAM_FILE_LSEEK:
	offset = get_int32(buf);
	origin = get_int32(buf+4);
	if ((offset = ram_file_seek(f, offset, origin, &error)) < 0)
	    error_reply(f, error);
	else
	    numeric_reply(f, offset);
	break;

    case RAM_FILE_PREAD:
	if ((offset = get_int32(buf)) < 0) {
	    error_reply(f, EINVAL);
	    break;
	}
	
	count = get_int32(buf+4);
	if ((n = ram_file_read(f, count, &bin, &offset, &error)) < 0) {
	    error_reply(f, error);
	} else {
	    header[0] = RAM_FILE_RESP_DATA;
	    put_int32(n, header+1);
	    driver_output_binary(f->port, header, sizeof(header),
				 bin, 0, n);
	    driver_free_binary(bin);
	}
	break;

    case RAM_FILE_READ:
	count = get_int32(buf);
	if ((n = ram_file_read(f, count, &bin, NULL, &error)) < 0)
	    error_reply(f, error);
	else {
	    header[0] = RAM_FILE_RESP_DATA;
	    put_int32(n, header+1);
	    driver_output_binary(f->port, header, sizeof(header),
				 bin, 0, n);
	    driver_free_binary(bin);
	}
	break;

    case RAM_FILE_TRUNCATE:
	if (!(f->flags & RAM_FILE_MODE_WRITE)) {
	    error_reply(f, EACCES);
	    break;
	}
	if (f->end > f->cur)
	    sys_memzero(f->buf + f->cur, f->end - f->cur);
	f->end = f->cur;
	reply(f, 1, 0);
	break;

    case RAM_FILE_GET:        /* return a copy of the file */
	n = f->end;  /* length */
	if ((bin = driver_alloc_binary(n)) == NULL) {
	    error_reply(f, ENOMEM);
	    break;
	}
	sys_memcpy(bin->orig_bytes, f->buf, n);
	
	header[0] = RAM_FILE_RESP_DATA;
	put_int32(n, header+1);
	driver_output_binary(f->port, header, sizeof(header),
			     bin, 0, n);
	driver_free_binary(bin);
	break;

    case RAM_FILE_GET_CLOSE:  /* return the file and close driver */
	n = f->end;  /* length */
	bin = f->bin;
	f->bin = NULL;  /* NUKE IT */
	header[0] = RAM_FILE_RESP_DATA;
	put_int32(n, header+1);
	driver_output_binary(f->port, header, sizeof(header),
			     bin, 0, n);
	driver_free_binary(bin);
	driver_failure(f->port, 0);
	break;

    case RAM_FILE_SIZE:
	numeric_reply(f, f->end);
	break;
	
    case RAM_FILE_SET:        /* re-init file with new data */
	if ((n = ram_file_init(f, buf, count, &error)) < 0)
	    error_reply(f, error);
	else
	    numeric_reply(f, n); /* 0 is not used */
	break;
	
    case RAM_FILE_COMPRESS:   /* inline compress the file */
	ram_file_compress(f);
	break;

    case RAM_FILE_UNCOMPRESS: /* inline uncompress file */
	ram_file_uncompress(f);
	break;

    case RAM_FILE_UUENCODE:   /* uuencode file */
	ram_file_uuencode(f);
	break;
	
    case RAM_FILE_UUDECODE:   /* uudecode file */
	ram_file_uudecode(f);
	break;

    case RAM_FILE_ADVISE:
	if (f->flags == 0)
	    error_reply(f, EBADF);
	else
	    reply(f, 1, 0);
	break;

    case RAM_FILE_ALLOCATE:
	if (f->flags == 0)
	    error_reply(f, EBADF);
	else
	    reply(f, 1, 0);
	break;
    }
    /*
     * Ignore anything else -- let the caller hang.
     */
}
