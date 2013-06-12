/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009-2013. All Rights Reserved.
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

/* A sparse wrapper interface around zlib with erts memory allocation. 
*/

#include <zlib.h>


/* Initialize zalloc, zfree and opaque of a z_stream
*/
#define erl_zlib_alloc_init(s)              \
  do { /* 'opaque' not used */              \
    (s)->zalloc = erl_zlib_zalloc_callback; \
    (s)->zfree = erl_zlib_zfree_callback;   \
  } while (0)

/*
 * Chunked interface, used by term_to_binary among others.
 */
int ZEXPORT erl_zlib_deflate_start(z_stream *streamp, const Bytef* source, 
				   uLong sourceLen, int level);
int ZEXPORT erl_zlib_deflate_chunk(z_stream *streamp, Bytef* dest, uLongf* destLen);
int ZEXPORT erl_zlib_deflate_finish(z_stream *streamp);

/* Use instead of compress
*/
#define erl_zlib_compress(dest,destLen,source,sourceLen) \
    erl_zlib_compress2(dest,destLen,source,sourceLen,Z_DEFAULT_COMPRESSION)

/* Use instead of compress2
*/
int ZEXPORT erl_zlib_compress2 (Bytef* dest, uLongf* destLen,
				const Bytef* source, uLong sourceLen,
				int level);
/* Use instead of uncompress
*/
int ZEXPORT erl_zlib_uncompress (Bytef* dest, uLongf* destLen,
				 const Bytef* source, uLong sourceLen);


voidpf erl_zlib_zalloc_callback (voidpf,unsigned,unsigned);
void erl_zlib_zfree_callback (voidpf,voidpf);

