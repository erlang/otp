/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

/* A sparse wrapper around zlib with erts memory allocation.
 *
 * erl_zlib_compress2 and erl_zlib_uncompress are erts-adapted versions
 * of the original compress2 and uncompress from zlib-1.2.3. 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_zlib.h"

#include "sys.h"
#include "erl_alloc.h"

voidpf erl_zlib_zalloc_callback (voidpf opaque, unsigned items, unsigned size)
{
    (void) opaque; /* make compiler happy */
    return erts_alloc_fnf(ERTS_ALC_T_ZLIB, items * size);
}

void erl_zlib_zfree_callback (voidpf opaque, voidpf ptr)
{
    (void) opaque; /* make compiler happy */
    erts_free(ERTS_ALC_T_ZLIB, ptr);
}

/*
 * Initialize a z_stream with a source, to later *chunk* data into a destination 
 * Returns Z_OK or Error.
 */
int ZEXPORT erl_zlib_deflate_start(z_stream *streamp, const Bytef* source, 
				   uLong sourceLen, int level) 
{
    streamp->next_in = (Bytef*)source;
    streamp->avail_in = (uInt)sourceLen;
    streamp->total_out = streamp->avail_out = 0;
    streamp->next_out = NULL;
    erl_zlib_alloc_init(streamp);
    return deflateInit(streamp, level);
}
/* 
 * Deflate a chunk, The destination length is the limit.
 * Returns Z_OK if more to process, Z_STREAM_END if we are done.
 */
int ZEXPORT erl_zlib_deflate_chunk(z_stream *streamp, Bytef* dest, uLongf* destLen) 
{
    int err;
    uLongf last_tot = streamp->total_out;

    streamp->next_out = dest;
    streamp->avail_out = (uInt)*destLen;
    
    if ((uLong)streamp->avail_out != *destLen) return Z_BUF_ERROR;
    
    err = deflate(streamp, Z_FINISH);
    *destLen = streamp->total_out - last_tot;
    return err; 
}


/*
 * When we are done, free up the deflate structure
 * Retyurns Z_OK or Error
 */
int ZEXPORT erl_zlib_deflate_finish(z_stream *streamp)
{
    return deflateEnd(streamp);
}

int ZEXPORT erl_zlib_inflate_start(z_stream *streamp, const Bytef* source,
                                   uLong sourceLen)
{
    streamp->next_in = (Bytef*)source;
    streamp->avail_in = (uInt)sourceLen;
    streamp->total_out = streamp->avail_out = 0;
    streamp->next_out = NULL;
    erl_zlib_alloc_init(streamp);
    return inflateInit(streamp);
}
/*
 * Inflate a chunk, The destination length is the limit.
 * Returns Z_OK if more to process, Z_STREAM_END if we are done.
 */
int ZEXPORT erl_zlib_inflate_chunk(z_stream *streamp, Bytef* dest, uLongf* destLen)
{
    int err;
    uLongf last_tot = streamp->total_out;

    streamp->next_out = dest;
    streamp->avail_out = (uInt)*destLen;

    if ((uLong)streamp->avail_out != *destLen) return Z_BUF_ERROR;

    err = inflate(streamp, Z_NO_FLUSH);
    ASSERT(err != Z_STREAM_ERROR);
    *destLen = streamp->total_out - last_tot;
    return err;
}

/*
 * When we are done, free up the inflate structure
 * Retyurns Z_OK or Error
 */
int ZEXPORT erl_zlib_inflate_finish(z_stream *streamp)
{
    return inflateEnd(streamp);
}


int ZEXPORT erl_zlib_compress2 (Bytef* dest, uLongf* destLen,
				const Bytef* source, uLong sourceLen,
				int level)
{
    z_stream stream;
    int err;

    stream.next_in = (Bytef*)source;
    stream.avail_in = (uInt)sourceLen;
#ifdef MAXSEG_64K
    /* Check for source > 64K on 16-bit machine: */
    if ((uLong)stream.avail_in != sourceLen) return Z_BUF_ERROR;
#endif
    stream.next_out = dest;
    stream.avail_out = (uInt)*destLen;
    if ((uLong)stream.avail_out != *destLen) return Z_BUF_ERROR;

    erl_zlib_alloc_init(&stream);

    err = deflateInit(&stream, level);
    if (err != Z_OK) return err;

    err = deflate(&stream, Z_FINISH);
    if (err != Z_STREAM_END) {
        deflateEnd(&stream);
        return err == Z_OK ? Z_BUF_ERROR : err;
    }
    *destLen = stream.total_out;

    err = deflateEnd(&stream);
    return err;
}

int ZEXPORT erl_zlib_uncompress (Bytef* dest, uLongf* destLen,
				 const Bytef* source, uLong sourceLen)
{
    z_stream stream;
    int err;

    stream.next_in = (Bytef*)source;
    stream.avail_in = (uInt)sourceLen;
    /* Check for source > 64K on 16-bit machine: */
    if ((uLong)stream.avail_in != sourceLen) return Z_BUF_ERROR;

    stream.next_out = dest;
    stream.avail_out = (uInt)*destLen;
    if ((uLong)stream.avail_out != *destLen) return Z_BUF_ERROR;

    erl_zlib_alloc_init(&stream);

    err = inflateInit(&stream);
    if (err != Z_OK) return err;

    err = inflate(&stream, Z_FINISH);
    if (err != Z_STREAM_END) {
        inflateEnd(&stream);
        if (err == Z_NEED_DICT || (err == Z_BUF_ERROR && stream.avail_in == 0))
            return Z_DATA_ERROR;
        return err;
    }
    *destLen = stream.total_out;

    err = inflateEnd(&stream);
    return err;
}

