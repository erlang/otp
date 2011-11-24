/*
 * Original version by Jean-loup Gailly. Modified for use by the
 * Erlang run-time system and efile_driver; names of all external
 * functions changed to avoid conflicts with the official gzio.c file.
 *
 * gzio.c -- IO on .gz files
 * Copyright (C) 1995-1996 Jean-loup Gailly.
 * For conditions of distribution and use, see copyright notice in zlib.h
 */
/* %ExternalCopyright% */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include "erl_driver.h"
#include "sys.h"

#ifdef VXWORKS
/* pull in FOPEN from zutil.h instead */
#undef F_OPEN
#endif

#ifdef __WIN32__
#define HAVE_CONFLICTING_FREAD_DECLARATION
#define FILENAMES_16BIT 1
#endif

#ifdef STDC
#  define zstrerror(errnum) strerror(errnum)
#else
#  define zstrerror(errnum) ""
#endif

#include "gzio_zutil.h"
#include "erl_zlib.h"
#include "gzio.h"

/********struct internal_state {int dummy;}; / * for buggy compilers */

#define Z_BUFSIZE 4096

#define ALLOC(size) driver_alloc(size)
#define TRYFREE(p) {if (p) driver_free(p);}

static int gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define RESERVED     0xE0 /* bits 5..7: reserved */

typedef struct gz_stream {
    z_stream stream;
    int      z_err;   /* error code for last stream operation */
    int      z_eof;   /* set if end of input file */
#ifdef UNIX
    int      file;    /* .gz file descriptor */
#else
    FILE     *file;   /* .gz file */
#endif
    Byte     *inbuf;  /* input buffer */
    Byte     *outbuf; /* output buffer */
    uLong    crc;     /* crc32 of uncompressed data */
    char     *msg;    /* error message */
    char     *path;   /* path name for debugging only */
    int      transparent; /* 1 if input file is not a .gz file */
    char     mode;    /* 'w' or 'r' */
    int      position; /* Position (for seek) */
    int (*destroy)OF((struct gz_stream*)); /* Function to destroy
					    *  this structure. */
} gz_stream;

local gzFile gz_open      OF((const char *path, const char *mode));
local int    get_byte     OF((gz_stream *s));
local void   check_header OF((gz_stream *s));
local int    destroy      OF((gz_stream *s));
local uLong  getLong      OF((gz_stream *s));

#ifdef UNIX
/*
 * In Solaris 8 and earlier, fopen() and its friends cannot handle 
 * file descriptors larger than 255. Therefore, we use read()/write()
 * on all Unix systems.
 */
# define ERTS_GZWRITE(File, Buf, Count) write((File), (Buf), (Count))
# define ERTS_GZREAD(File, Buf, Count) read((File), (Buf), (Count))
#else
/*
 * On all other operating systems, using fopen(), fread()/fwrite(), since
 * there is not guaranteed to exist any read()/write() (not part of
 * ANSI/ISO-C).
 */
# define ERTS_GZWRITE(File, Buf, Count) fwrite((Buf), 1, (Count), (File))
# define ERTS_GZREAD(File, Buf, Count) fread((Buf), 1, (Count), (File))
#endif

/*
 * Ripped from efile_drv.c
 */

#ifdef FILENAMES_16BIT
#  define FILENAME_BYTELEN(Str) filename_len_16bit(Str)
#  define FILENAME_COPY(To,From) filename_cpy_16bit((To),(From)) 
#  define FILENAME_CHARSIZE 2

   static int filename_len_16bit(const char *str) 
   {
       const char *p = str;
       while(*p != '\0' || p[1] != '\0') {
	   p += 2;
       }
       return (p - str);
   }

   static void filename_cpy_16bit(char *to, const char *from) 
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

/* ===========================================================================
     Opens a gzip (.gz) file for reading or writing. The mode parameter
   is as in fopen ("rb" or "wb"). The file is given either by file descriptor
   or path name (if fd == -1).
     gz_open return NULL if the file could not be opened or if there was
   insufficient memory to allocate the (de)compression state; errno
   can be checked to distinguish the two cases (if errno is zero, the
   zlib error is Z_MEM_ERROR).
*/
local gzFile gz_open (path, mode)
    const char *path;
    const char *mode;
{
    int err;
    int level = Z_DEFAULT_COMPRESSION; /* compression level */
    char *p = (char*)mode;
    gz_stream *s;
    char fmode[80]; /* copy of mode, without the compression level */
    char *m = fmode;

    if (!path || !mode) return Z_NULL;

    s = (gz_stream *)ALLOC(sizeof(gz_stream));
    if (!s) return Z_NULL;

    erl_zlib_alloc_init(&s->stream);
    s->stream.next_in = s->inbuf = Z_NULL;
    s->stream.next_out = s->outbuf = Z_NULL;
    s->stream.avail_in = s->stream.avail_out = 0;
#ifdef UNIX
    s->file = -1;
#else
    s->file = NULL;
#endif
    s->z_err = Z_OK;
    s->z_eof = 0;
    s->crc = crc32(0L, Z_NULL, 0);
    s->msg = NULL;
    s->transparent = 0;
    s->position = 0;
    s->destroy = destroy;

    s->path = (char*)ALLOC(FILENAME_BYTELEN(path)+FILENAME_CHARSIZE);
    if (s->path == NULL) {
        return s->destroy(s), (gzFile)Z_NULL;
    }
    FILENAME_COPY(s->path, path); /* do this early for debugging */

    s->mode = '\0';
    do {
        if (*p == 'r')
	    s->mode = 'r';
	if (*p == 'w' || *p == 'a')
	    s->mode = 'w';
	if (isdigit((int)*p)) {
	    level = *p - '0';
	} else {
	    *m++ = *p;		/* Copy the mode */
	}
    } while (*p++ && m < fmode + sizeof(fmode) - 1);
    *m = '\0';
    if (s->mode == '\0')
	return s->destroy(s), (gzFile)Z_NULL;
    
    if (s->mode == 'w') {
        err = deflateInit2(&(s->stream), level,
                           Z_DEFLATED, MAX_WBITS+16, DEF_MEM_LEVEL, 0);
        /* windowBits is passed < 0 to suppress zlib header */

        s->stream.next_out = s->outbuf = (Byte*)ALLOC(Z_BUFSIZE);

        if (err != Z_OK || s->outbuf == Z_NULL) {
            return s->destroy(s), (gzFile)Z_NULL;
        }
    } else {
	/*
	 * It is tempting to use the built-in support in zlib
	 * for handling GZIP headers, but unfortunately it
	 * cannot handle multiple GZIP headers (which occur when
	 * several GZIP files have been concatenated).
	 */

	err = inflateInit2(&(s->stream), -MAX_WBITS);
        s->stream.next_in  = s->inbuf = (Byte*)ALLOC(Z_BUFSIZE);

        if (err != Z_OK || s->inbuf == Z_NULL) {
            return s->destroy(s), (gzFile)Z_NULL;
        }
    }
    s->stream.avail_out = Z_BUFSIZE;

    errno = 0;
#if defined(FILENAMES_16BIT)
    {
	char wfmode[160];
	int i=0,j;
	for(j=0;fmode[j] != '\0';++j) {
	    wfmode[i++]=fmode[j];
	    wfmode[i++]='\0';
	}
	wfmode[i++] = '\0';
	wfmode[i++] = '\0';
	s->file = F_OPEN(path, wfmode);
	if (s->file == NULL) {
	    return s->destroy(s), (gzFile)Z_NULL;
	}
    }
#elif defined(UNIX)
    if (s->mode == 'r') {
	s->file = open(path, O_RDONLY);
    } else {
	s->file = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    }
    if (s->file == -1) {
        return s->destroy(s), (gzFile)Z_NULL;
    }
#else
    s->file = F_OPEN(path, fmode);
    if (s->file == NULL) {
        return s->destroy(s), (gzFile)Z_NULL;
    }
#endif
    if (s->mode == 'r') {
	check_header(s); /* skip the .gz header */
    }
    return (gzFile)s;
}

/* ===========================================================================
   Rewind a gzfile back to the beginning.
*/

local int gz_rewind (gz_stream *s)
{
    TRYFREE(s->msg);

#ifdef UNIX
    lseek(s->file, 0L, SEEK_SET);
#else
    fseek(s->file, 0L, SEEK_SET);
#endif
    inflateReset(&(s->stream));
    s->stream.next_in = Z_NULL;
    s->stream.next_out = Z_NULL;
    s->stream.avail_in = s->stream.avail_out = 0;
    s->z_err = Z_OK;
    s->z_eof = 0;
    s->crc = crc32(0L, Z_NULL, 0);
    s->msg = NULL;
    s->position = 0;
    s->stream.next_in = s->inbuf;

    s->stream.avail_out = Z_BUFSIZE;

    check_header(s);		/* skip the .gz header */
    return 1;
}

/* ===========================================================================
     Opens a gzip (.gz) file for reading or writing.
*/
gzFile erts_gzopen (path, mode)
    const char *path;
    const char *mode;
{
    return gz_open (path, mode);
}


/* ===========================================================================
     Read a byte from a gz_stream; update next_in and avail_in. Return EOF
   for end of file.
   IN assertion: the stream s has been sucessfully opened for reading.
*/
local int get_byte(s)
    gz_stream *s;
{
    if (s->z_eof) return EOF;
    if (s->stream.avail_in == 0) {
#ifdef UNIX
	size_t res;
	errno = 0;
	res = ERTS_GZREAD(s->file, s->inbuf, Z_BUFSIZE);
	if (res == 0) {
	    s->stream.avail_in = 0;
	    s->z_eof = 1;
	    return EOF;
	} else if (res < 0) {
	    s->stream.avail_in = 0;
	    s->z_eof = 1;
	    s->z_err = Z_ERRNO;
	    return EOF;
	} else {
	    s->stream.avail_in = (uInt) res;
	}
#else
	errno = 0;
	s->stream.avail_in = ERTS_GZREAD(s->file, s->inbuf, Z_BUFSIZE);
	if (s->stream.avail_in == 0) {
	    s->z_eof = 1;
	    if (s->file && ferror(s->file))
		s->z_err = Z_ERRNO;
	    return EOF;
	}
#endif
	s->stream.next_in = s->inbuf;
    }
    s->stream.avail_in--;
    return *(s->stream.next_in)++;
}

/* ===========================================================================
      Check the gzip header of a gz_stream opened for reading. Set the stream
    mode to transparent if the gzip magic header is not present; set s->err
    to Z_DATA_ERROR if the magic header is present but the rest of the header
    is incorrect.
    IN assertion: the stream s has already been created sucessfully;
       s->stream.avail_in is zero for the first time, but may be non-zero
       for concatenated .gz files.
*/
local void check_header(s)
    gz_stream *s;
{
    int method; /* method byte */
    int flags;  /* flags byte */
    uInt len;
    int c;

    /* Check the gzip magic header */
    for (len = 0; len < 2; len++) {
	c = get_byte(s);
	if (c != gz_magic[len]) {
	    if (len != 0) s->stream.avail_in++, s->stream.next_in--;
	    if (c != EOF) {
		s->stream.avail_in++, s->stream.next_in--;
		s->transparent = 1;
	    }
	    s->z_err = s->stream.avail_in != 0 ? Z_OK : Z_STREAM_END;
	    return;
	}
    }
    method = get_byte(s);
    flags = get_byte(s);
    if (method != Z_DEFLATED || (flags & RESERVED) != 0) {
	s->z_err = Z_DATA_ERROR;
	return;
    }

    /* Discard time, xflags and OS code: */
    for (len = 0; len < 6; len++) (void)get_byte(s);

    if ((flags & EXTRA_FIELD) != 0) { /* skip the extra field */
	len  =  (uInt)get_byte(s);
	len += ((uInt)get_byte(s))<<8;
	/* len is garbage if EOF but the loop below will quit anyway */
	while (len-- != 0 && get_byte(s) != EOF) ;
    }
    if ((flags & ORIG_NAME) != 0) { /* skip the original file name */
	while ((c = get_byte(s)) != 0 && c != EOF) ;
    }
    if ((flags & COMMENT) != 0) {   /* skip the .gz file comment */
	while ((c = get_byte(s)) != 0 && c != EOF) ;
    }
    if ((flags & HEAD_CRC) != 0) {  /* skip the header crc */
	for (len = 0; len < 2; len++) (void)get_byte(s);
    }
    s->z_err = s->z_eof ? Z_DATA_ERROR : Z_OK;
}

 /* ===========================================================================
 * Cleanup then free the given gz_stream. Return a zlib error code.
   Try freeing in the reverse order of allocations.
 */
local int destroy (s)
    gz_stream *s;
{
    int err = Z_OK;

    if (!s) return Z_STREAM_ERROR;

    TRYFREE(s->msg);

    if (s->stream.state != NULL) {
       if (s->mode == 'w') {
           err = deflateEnd(&(s->stream));
       } else if (s->mode == 'r') {
           err = inflateEnd(&(s->stream));
       }
    }
#ifdef UNIX
    if (s->file != -1 && close(s->file)) {
        err = Z_ERRNO;
    }
#else
    if (s->file != NULL && fclose(s->file)) {
        err = Z_ERRNO;
    }
#endif
    if (s->z_err < 0) err = s->z_err;

    TRYFREE(s->inbuf);
    TRYFREE(s->outbuf);
    TRYFREE(s->path);
    TRYFREE(s);
    return err;
}

/* ===========================================================================
     Reads the given number of uncompressed bytes from the compressed file.
   gzread returns the number of bytes actually read (0 for end of file).
*/
int
erts_gzread(gzFile file, voidp buf, unsigned len)
{
    gz_stream *s = (gz_stream*)file;
    Bytef *start = buf; /* starting point for crc computation */
    Byte  *next_out; /* == stream.next_out but not forced far (for MSDOS) */

    if (s == NULL || s->mode != 'r') return Z_STREAM_ERROR;

    if (s->z_err == Z_DATA_ERROR || s->z_err == Z_ERRNO) return -1;
    if (s->z_err == Z_STREAM_END) return 0;  /* EOF */

    s->stream.next_out = next_out = buf;
    s->stream.avail_out = len;

    while (s->stream.avail_out != 0) {

	if (s->transparent) {
	    /* Copy first the lookahead bytes: */
	    uInt n = s->stream.avail_in;
	    if (n > s->stream.avail_out) n = s->stream.avail_out;
	    if (n > 0) {
		zmemcpy(s->stream.next_out, s->stream.next_in, n);
		next_out += n;
		s->stream.next_out = next_out;
		s->stream.next_in   += n;
		s->stream.avail_out -= n;
		s->stream.avail_in  -= n;
	    }
	    if (s->stream.avail_out > 0) {
		s->stream.avail_out -= ERTS_GZREAD(s->file, next_out,
						   s->stream.avail_out);
	    }
	    len -= s->stream.avail_out;
	    s->stream.total_in  += (uLong)len;
	    s->stream.total_out += (uLong)len;
            if (len == 0) s->z_eof = 1;
	    s->position += (int)len;
	    return (int)len;
	}
        if (s->stream.avail_in == 0 && !s->z_eof) {
#ifdef UNIX
	    size_t res;
	    errno = 0;
	    res = ERTS_GZREAD(s->file, s->inbuf, Z_BUFSIZE);
	    if (res == 0) {
		s->stream.avail_in = 0;
		s->z_eof = 1;
		return EOF;
	    } else if (res < 0) {
		s->stream.avail_in = 0;
		s->z_eof = 1;
		s->z_err = Z_ERRNO;
		return EOF;
	    } else {
		s->stream.avail_in = (uInt) res;
	    }
#else
	    errno = 0;
            s->stream.avail_in = ERTS_GZREAD(s->file, s->inbuf, Z_BUFSIZE);
            if (s->stream.avail_in == 0) {
                s->z_eof = 1;
		if (s->file && ferror(s->file)) {
		    s->z_err = Z_ERRNO;
		    break;
		}
            }
#endif
            s->stream.next_in = s->inbuf;
        }
        s->z_err = inflate(&(s->stream), Z_NO_FLUSH);

	if (s->z_err == Z_STREAM_END) {
	    /* Check CRC and original size */
	    s->crc = crc32(s->crc, start, (uInt)(s->stream.next_out - start));
	    start = s->stream.next_out;

	    if (getLong(s) != s->crc) {
		s->z_err = Z_DATA_ERROR;
	    } else {
	        (void)getLong(s);
                /* The uncompressed length returned by above getlong() may
                 * be different from s->stream.total_out) in case of
		 * concatenated .gz files. Check for such files:
		 */
		check_header(s);
		if (s->z_err == Z_OK) {
		    uLong total_in = s->stream.total_in;
		    uLong total_out = s->stream.total_out;

		    inflateReset(&(s->stream));
		    s->stream.total_in = total_in;
		    s->stream.total_out = total_out;
		    s->crc = crc32(0L, Z_NULL, 0);
		}
	    }
	}
	if (s->z_err != Z_OK || s->z_eof) break;
    }
    s->crc = crc32(s->crc, start, (uInt)(s->stream.next_out - start));

    s->position += (int)(len - s->stream.avail_out);

    return (int)(len - s->stream.avail_out);
}

/* ===========================================================================
     Writes the given number of uncompressed bytes into the compressed file.
   gzwrite returns the number of bytes actually written (0 in case of error).
*/
int
erts_gzwrite(gzFile file, voidp buf, unsigned len)
{
    gz_stream *s = (gz_stream*)file;

    if (s == NULL || s->mode != 'w') return Z_STREAM_ERROR;

    s->stream.next_in = buf;
    s->stream.avail_in = len;

    while (s->stream.avail_in != 0) {

        if (s->stream.avail_out == 0) {

            s->stream.next_out = s->outbuf;
            if (ERTS_GZWRITE(s->file, s->outbuf, Z_BUFSIZE) != Z_BUFSIZE) {
                s->z_err = Z_ERRNO;
                break;
            }
            s->stream.avail_out = Z_BUFSIZE;
        }
        s->z_err = deflate(&(s->stream), Z_NO_FLUSH);
        if (s->z_err != Z_OK) break;
    }
    s->position += (int)(len - s->stream.avail_in);
    return (int)(len - s->stream.avail_in);
}

/*
 * For use by Erlang file driver.
 *
 * XXX Limitations:
 *  - SEEK_END is not allowed (length of file is not known).
 *  - When writing, only forward seek is supported.
 */

int
erts_gzseek(gzFile file, int offset, int whence)
{
    int pos;
    gz_stream* s = (gz_stream *) file;

    if (s == NULL) {
	errno = EINVAL;
	return -1;
    }
    if (s->z_err == Z_DATA_ERROR || s->z_err == Z_ERRNO) {
	errno = EIO;
	return -1;
    }

    switch (whence) {
    case SEEK_SET: pos = offset; break;
    case SEEK_CUR: pos = s->position+offset; break;
    case SEEK_END: 
    default:
      errno = EINVAL; return -1;
    }

    if (pos == s->position) {
	return pos;
    }

    if (pos < s->position) {
	if (s->mode == 'w') {
	    errno = EINVAL;
	    return -1;
	}
	gz_rewind(s);
    }

    while (s->position < pos) {
	char buf[512];
	int n;
	int save_pos = s->position;

	n = pos - s->position;
	if (n > sizeof(buf))
	    n = sizeof(buf);

	if (s->mode == 'r') {
	    erts_gzread(file, buf, n);
	} else {
	    memset(buf, '\0', n);
	    erts_gzwrite(file, buf, n);
	}
	if (save_pos == s->position) break;
    }

    return s->position;
}

/* ===========================================================================
     Flushes all pending output into the compressed file. The parameter
   flush is as in the deflate() function.
     gzflush should be called only when strictly necessary because it can
   degrade compression.
*/
int
erts_gzflush(gzFile file, int flush)
{
    uInt len;
    int done = 0;
    gz_stream *s = (gz_stream*)file;

    if (s == NULL || s->mode != 'w') return Z_STREAM_ERROR;

    s->stream.avail_in = 0; /* should be zero already anyway */

    for (;;) {
        len = Z_BUFSIZE - s->stream.avail_out;

        if (len != 0) {
            if ((uInt)ERTS_GZWRITE(s->file, s->outbuf, len) != len) {
                s->z_err = Z_ERRNO;
                return Z_ERRNO;
            }
            s->stream.next_out = s->outbuf;
            s->stream.avail_out = Z_BUFSIZE;
        }
        if (done) break;
        s->z_err = deflate(&(s->stream), flush);

        /* deflate has finished flushing only when it hasn't used up
         * all the available space in the output buffer: 
         */
        done = (s->stream.avail_out != 0 || s->z_err == Z_STREAM_END);
 
        if (s->z_err != Z_OK && s->z_err != Z_STREAM_END) break;
    }
#ifndef UNIX
    fflush(s->file);
#endif
    return  s->z_err == Z_STREAM_END ? Z_OK : s->z_err;
}

/* ===========================================================================
   Reads a long in LSB order from the given gz_stream. Sets 
*/
local uLong getLong (s)
    gz_stream *s;
{
    uLong x = (uLong)get_byte(s);
    int c;

    x += ((uLong)get_byte(s))<<8;
    x += ((uLong)get_byte(s))<<16;
    c = get_byte(s);
    if (c == EOF) s->z_err = Z_DATA_ERROR;
    x += ((uLong)c)<<24;
    return x;
}

/* ===========================================================================
     Flushes all pending output if necessary, closes the compressed file
   and deallocates all the (de)compression state.
*/
int
erts_gzclose(gzFile file)
{
    int err;
    gz_stream *s = (gz_stream*)file;

    if (s == NULL) return Z_STREAM_ERROR;

    if (s->mode == 'w') {
        err = erts_gzflush (file, Z_FINISH);
        if (err != Z_OK) return s->destroy(file);
    }
    return s->destroy(file);
}


/* ===========================================================================
   Uncompresses the buffer given and returns a pointer to a binary.
   If the buffer was not compressed with gzip, the buffer contents
   will be copied unchanged into the binary.

   If a `gzip' header was found, but there were subsequent errors,
   a NULL pointer is returned.
*/

ErlDrvBinary*
erts_gzinflate_buffer(char* start, uLong size)
{
    ErlDrvBinary* bin;
    ErlDrvBinary* bin2;
    z_stream zstr;
    unsigned char* bptr;

    /*
     * Check for the magic bytes beginning a GZIP header.
     */
    bptr = (unsigned char *) start;
    if (size < 2 || bptr[0] != gz_magic[0] || bptr[1] != gz_magic[1]) {
	/* No GZIP header -- just copy the data into a new binary */
	if ((bin = driver_alloc_binary(size)) == NULL) {
	    return NULL;
	}
	memcpy(bin->orig_bytes, start, size);
	return bin;
    }

    /*
     * The magic bytes for a GZIP header are there. Now try to decompress.
     * It is an error if the GZIP header is not correct.
     */

    zstr.next_in = (unsigned char*) start;
    zstr.avail_in = size;
    erl_zlib_alloc_init(&zstr);
    size *= 2;
    if ((bin = driver_alloc_binary(size)) == NULL) {
	return NULL;
    }
    if (inflateInit2(&zstr, 15+16) != Z_OK) { /* Decode GZIP format */
	driver_free(bin);
	return NULL;
    }
    for (;;) {
	int status;

	zstr.next_out = (unsigned char *) bin->orig_bytes + zstr.total_out;
	zstr.avail_out = size - zstr.total_out;
	status = inflate(&zstr, Z_NO_FLUSH);
	if (status == Z_OK) {
	    size *= 2;
	    if ((bin2 = driver_realloc_binary(bin, size)) == NULL) {
	    error:
		driver_free_binary(bin);
		inflateEnd(&zstr);
		return NULL;
	    }
	    bin = bin2;
	} else if (status == Z_STREAM_END) {
	    if ((bin2 = driver_realloc_binary(bin, zstr.total_out)) == NULL) {
		goto error;
	    }
	    inflateEnd(&zstr);
	    return bin2;
	} else {
	    goto error;
	}
    }
}

/* ===========================================================================
   Compresses the buffer given and returns a pointer to a binary.
   A NULL pointer is returned if any error occurs.
   Writes a gzip header as well.
*/

#define GZIP_HD_SIZE 10
#define GZIP_TL_SIZE 8

#define GZIP_X_SIZE (GZIP_HD_SIZE+GZIP_TL_SIZE)

ErlDrvBinary*
erts_gzdeflate_buffer(char* start, uLong size)
{
    z_stream c_stream; /* compression stream */
    ErlDrvBinary* bin;
    ErlDrvBinary* bin2;
    uLong    crc;     /* crc32 of uncompressed data */
    uLong    szIn;
    Byte* ptr;
    int comprLen = size + (size/1000) + 1 + 12; /* see zlib.h */

    crc = crc32(0L, Z_NULL, 0);
    erl_zlib_alloc_init(&c_stream);

    if (deflateInit2(&c_stream, Z_DEFAULT_COMPRESSION,
		     Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0) != Z_OK)
	return NULL;

    if ((bin = driver_alloc_binary(comprLen+GZIP_X_SIZE)) == NULL)
	return NULL;
    sprintf(bin->orig_bytes, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
	    Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/, OS_CODE);

    c_stream.next_out = ((Byte*) bin->orig_bytes)+GZIP_HD_SIZE;
    c_stream.avail_out = (uInt) bin->orig_size - GZIP_HD_SIZE;
    c_stream.next_in  = (Byte*) start;
    c_stream.avail_in = (uInt) size;

    if (deflate(&c_stream, Z_FINISH) != Z_STREAM_END) {
	driver_free_binary(bin);
	return NULL;	
    }
    crc = crc32(crc, (unsigned char*)start, size);
    ptr = c_stream.next_out;
    szIn = c_stream.total_in;

    *ptr++ = (crc & 0xff); crc >>= 8;
    *ptr++ = (crc & 0xff); crc >>= 8;
    *ptr++ = (crc & 0xff); crc >>= 8;
    *ptr++ = (crc & 0xff); crc >>= 8;

    *ptr++ = (szIn & 0xff); szIn >>= 8;
    *ptr++ = (szIn & 0xff); szIn >>= 8;
    *ptr++ = (szIn & 0xff); szIn >>= 8;
    *ptr++ = (szIn & 0xff); szIn >>= 8;

    if (deflateEnd(&c_stream) != Z_OK) {
	driver_free_binary(bin);
	return NULL;	
    }	
    size = ptr - (Byte*)bin->orig_bytes;

    if ((bin2 = driver_realloc_binary(bin, size)) == NULL)
	driver_free_binary(bin);
    return bin2;
}

