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
#include <string.h> /* ssize_t on Mac OS X */
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "erl_driver.h"
#include "sys.h"

#include "gzio_zutil.h"
#include "erl_zlib.h"
#include "gzio.h"

static int gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

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

