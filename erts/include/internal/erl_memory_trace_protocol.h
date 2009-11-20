/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
 * Description:	
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_MEMORY_TRACE_PROTOCOL_H__
#define ERL_MEMORY_TRACE_PROTOCOL_H__

/*
 * Increase ERTS_MT_MAJOR_VSN and set ERTS_MT_MINOR_VSN to 0
 * when backward incompatible changes are made in the protocol.
 *
 * Increase ERTS_MT_MINOR_VSN when backward compatible changes are
 * made in the protocol.
 */ 
#define ERTS_MT_MAJOR_VSN		(2)
#define ERTS_MT_MINOR_VSN		(0)

/* Trace flags */

#define ERTS_MT_64_BIT_FLAG		(1 << 0)
#define ERTS_MT_CRR_INFO		(1 << 1)
#define ERTS_MT_SEG_CRR_INFO		(1 << 2)

/* Header flags */
/* Allocator flags */

#define ERTS_MT_ALLCTR_USD_CRR_INFO	(1 << 0)

/* Block type flags */



/* Entry tags */

#define ERTS_MT_V1_ALLOCATOR_TAG	(1)
#define ERTS_MT_V1_BLOCK_TYPE_TAG	(2)
#define ERTS_MT_V1_ALLOC_TAG		(3)
#define ERTS_MT_V1_REALLOC_NPB_TAG	(4)
#define ERTS_MT_V1_REALLOC_MV_TAG	(5)
#define ERTS_MT_V1_REALLOC_NMV_TAG	(6)
#define ERTS_MT_V1_FREE_TAG		(7)
#define ERTS_MT_V1_TIME_INC_TAG		(8)
#define ERTS_MT_V1_STOP_TAG		(9)
#define ERTS_MT_V1_EXIT_TAG		(10)

#define ERTS_MT_END_OF_HDR_TAG		(0)
#define ERTS_MT_ALLOCATOR_HDR_TAG	(1)
#define ERTS_MT_BLOCK_TYPE_HDR_TAG	(2)

#define ERTS_MT_EXIT_BDY_TAG		(0)
#define ERTS_MT_STOP_BDY_TAG		(1)
#define ERTS_MT_ALLOC_BDY_TAG		(2)
#define ERTS_MT_REALLOC_BDY_TAG		(3)
#define ERTS_MT_FREE_BDY_TAG		(4)
#define ERTS_MT_CRR_ALLOC_BDY_TAG	(5)
#define ERTS_MT_CRR_REALLOC_BDY_TAG	(6)
#define ERTS_MT_CRR_FREE_BDY_TAG	(7)
#define ERTS_MT_TIME_INC_BDY_TAG	(8)
#define ERTS_MT_X_BDY_TAG		(9)

/* X subtags */
#if 0
#define ERTS_MT_X_ _BDY_TAG		(0)
#endif

#define ERTS_MT_START_WORD		(0xfff04711)
/* Entry header fields */

#define ERTS_MT_UI8_MSB_EHDR_FLD_SZ	(0)
#define ERTS_MT_UI16_MSB_EHDR_FLD_SZ	(1)
#define ERTS_MT_UI32_MSB_EHDR_FLD_SZ	(2)
#define ERTS_MT_UI64_MSB_EHDR_FLD_SZ	(3)
#define ERTS_MT_UI_MSB_EHDR_FLD_SZ	ERTS_MT_UI64_MSB_EHDR_FLD_SZ
#define ERTS_MT_TAG_EHDR_FLD_SZ		(4)

#define ERTS_MT_UI8_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI8_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI16_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI16_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI32_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI32_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI64_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI64_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI_MSB_EHDR_FLD_MSK	ERTS_MT_UI64_MSB_EHDR_FLD_MSK
#define ERTS_MT_TAG_EHDR_FLD_MSK	((1 << ERTS_MT_TAG_EHDR_FLD_SZ)-1)

/* Time increment word */
#define ERTS_MT_TIME_INC_SECS_SHIFT	20
#define ERTS_MT_TIME_INC_USECS_SHIFT	0

#define ERTS_MT_TIME_INC_SECS_MASK	((1 << 12) - 1)
#define ERTS_MT_TIME_INC_USECS_MASK	((1 << 20) - 1)


#define ERTS_MT_MAX_V1_HEADER_ENTRY_SIZE (2 + 2 + 1 + 255 + 2)
/* Largest v1 header entry is block type entry (ERTS_MT_V1_BLOCK_TYPE_TAG) */
#define ERTS_MT_MAX_V1_BODY_ENTRY_SIZE (2 + 8 + 8 + 8 + 4)
/* Largest body entry is realloc moved entry (ERTS_MT_V1_REALLOC_MV_TAG) */


#define ERTS_MT_MAX_HEADER_ENTRY_SIZE (1 + 2 + 2 + 1 + 255 + 2)
/* Largest header entry is block type entry (ERTS_MT_BLOCK_TYPE_TAG) */
#define ERTS_MT_MAX_BODY_ENTRY_SIZE   ERTS_MT_MAX_CRR_REALLOC_SIZE
/* Largest body entry is carrier realloc entry (ERTS_MT_CRR_REALLOC_BDY_TAG) */

/*
 *
 * Entry header:
 *
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |                 ... |MSB2|MSB1|
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * Time inc entry field:
 *
 * 31               23                                            0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |     Seconds   |   Micro Seconds                               |
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 */

#define ERTS_MT_MAX_CRR_ALLOC_SIZE (1 + 2 + 2 + 2 + 8 + 8 + 4)

/*
 * ERTS_MT_CRR_ALLOC_BDY_TAG:
 * N                1     2        3          4          5
 * MSB             1-0   1-0     7|3-0      7|3-0       3-0
 * SZ     1   2    2-1   2-1     8|4-1      8|4-1       4-1
 * UIT   UI8 UI16  UI16  UI16  UI64|UI32  UI64|UI32    UI32
 *      +---+----+...--+...--+...-------+...-------+...-------+
 *      |Tag| Hdr|CType| Type| Out ptr  | In size  | Time inc |
 *      +---+----+...--+...--+...-------+...-------+...-------+
 *
 */

#define ERTS_MT_MAX_ALLOC_SIZE (1 + 2 + 2 + 8 + 8 + 4)
/*
 * ERTS_MT_ALLOC_BDY_TAG:
 * N                1        2          3          4
 * MSB             1-0     7|3-0      7|3-0       3-0
 * SZ     1   2    2-1     8|4-1      8|4-1       4-1
 * UIT   UI8 UI16  UI16  UI64|UI32  UI64|UI32    UI32
 *      +---+----+...--+...-------+...-------+...-------+
 *      |Tag| Hdr| Type| Out ptr  | In size  | Time inc |
 *      +---+----+...--+...-------+...-------+...-------+
 *
 */

#define ERTS_MT_MAX_CRR_REALLOC_SIZE (1 + 2 + 2 + 2 + 8 + 8 + 8 + 4)
/*
 * ERTS_MT_CRR_REALLOC_BDY_TAG:
 * N                1     2        3          4          5          6
 * MSB             1-0   1-0     7|3-0      7|3-0      7|3-0       3-0
 * SZ     1   2    2-1   2-1     8|4-1      8|4-1      8|4-1       4-1
 * UIT   UI8 UI16  UI16  UI16  UI64|UI32  UI64|UI32  UI64|UI32    UI32
 *      +---+----+...--+...--+...-------+...-------+...-------+...-------+
 *      |Tag| Hdr|CType| Type| Out ptr  | In ptr   | In size  | Time inc |
 *      +---+----+...--+...--+...-------+...-------+...-------+...-------+
 *
 */

#define ERTS_MT_MAX_REALLOC_SIZE (1 + 2 + 2 + 8 + 8 + 8 + 4)
/*
 * ERTS_MT_REALLOC_BDY_TAG:
 * N                1        2          3          4          5
 * MSB             1-0     7|3-0      7|3-0      7|3-0       3-0
 * SZ     1   2    2-1     8|4-1      8|4-1      8|4-1       4-1
 * UIT   UI8 UI16  UI16  UI64|UI32  UI64|UI32  UI64|UI32    UI32
 *      +---+----+...--+...-------+...-------+...-------+...-------+
 *      |Tag| Hdr| Type| Out ptr  | In ptr   | In size  | Time inc |
 *      +---+----+...--+...-------+...-------+...-------+...-------+
 *
 */

#define ERTS_MT_MAX_CRR_FREE_SIZE (1 + 2 + 2 + 2 + 8 + 4)
/*
 * ERTS_MT_CRR_FREE_BDY_TAG:
 * N                1     2        3          4
 * MSB             1-0   1-0     7|3-0       3-0
 * SZ     1   2    2-1   2-1     8|4-1       4-1
 * UIT   UI8 UI16  UI16  UI16  UI64|UI32    UI32
 *      +---+----+...--+...--+...-------+...-------+
 *      |Tag| Hdr|CType| Type| In ptr   | Time inc |
 *      +---+----+...--+...--+...-------+...-------+
 *
 */

#define ERTS_MT_MAX_FREE_SIZE (1 + 2 + 2 + 8 + 4)
/*
 * ERTS_MT_FREE_BDY_TAG:
 * N                1      2            3
 * MSB             1-0   7|3-0         3-0
 * SZ     1   2    2-1   8|4-1         4-1
 * UIT   UI8 UI16  UI16  UI64|UI32    UI32
 *      +---+----+...--+...-------+...-------+
 *      |Tag| Hdr| Type| In ptr   | Time inc |
 *      +---+----+...--+...-------+...-------+
 *
 */

/*
 * ERTS_MT_X_BDY_TAG:
 * N
 * MSB
 * SZ     1    2     1
 * UIT   UI8  UI16  UI8   
 *      +---+-----+------+... ...+
 *      |Tag|TotSz|SubTag|       |
 *      +---+-----+------+... ...+
 *
 *      ^                        ^
 *      |                        |
 *      +------ TotSz bytes -----+
 *
 * X for extension
 *
 * * Tag equals ERTS_MT_X_BDY_TAG.
 * * TotSz contains the total size of the entry.
 * * SubTag is used to distinguish between different sub entries
 *   passed in X entries.
 *
 */



#endif /* #ifndef ERL_MEMORY_TRACE_PROTOCOL_H__ */

