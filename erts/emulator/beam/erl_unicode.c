/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_binary.h"
#include "big.h"

#include "erl_unicode.h"

typedef struct _restart_context {
    byte *bytes;
    Uint num_processed_bytes;
    Uint num_bytes_to_process;
    Uint num_resulting_chars;
    int state;
} RestartContext;


#define LOOP_FACTOR 10
#define LOOP_FACTOR_SIMPLE 50 /* When just counting */

static Uint max_loop_limit;

static BIF_RETTYPE utf8_to_list(BIF_ALIST_1);
static BIF_RETTYPE finalize_list_to_list(Process *p, 
					 byte *bytes,
					 Eterm rest,
					 Uint num_processed_bytes,
					 Uint num_bytes_to_process, 
					 Uint num_resulting_chars, 
					 int state, int left,
					 Eterm tail);
static int analyze_utf8(byte *source, Uint size, 
			byte **err_pos, Uint *num_chars, int *left);
#define UTF8_OK 0
#define UTF8_INCOMPLETE 1
#define UTF8_ERROR 2
#define UTF8_ANALYZE_MORE 3

static BIF_RETTYPE characters_to_utf8_trap(BIF_ALIST_3);
static BIF_RETTYPE characters_to_list_trap_1(BIF_ALIST_3);
static BIF_RETTYPE characters_to_list_trap_2(BIF_ALIST_3);

static BIF_RETTYPE characters_to_list_trap_3(BIF_ALIST_3);
static BIF_RETTYPE characters_to_list_trap_4(BIF_ALIST_1);

static Export characters_to_utf8_trap_exp;
static Export characters_to_list_trap_1_exp;
static Export characters_to_list_trap_2_exp;

static Export characters_to_list_trap_3_exp;
static Export characters_to_list_trap_4_exp;

static Export *c_to_b_int_trap_exportp = NULL;
static Export *c_to_l_int_trap_exportp = NULL;

void erts_init_unicode(void)
{
    max_loop_limit = CONTEXT_REDS * LOOP_FACTOR;
    /* Non visual BIFs to trap to. */
    memset(&characters_to_utf8_trap_exp, 0, sizeof(Export));
    characters_to_utf8_trap_exp.address = 
	&characters_to_utf8_trap_exp.code[3];
    characters_to_utf8_trap_exp.code[0] = am_erlang;
    characters_to_utf8_trap_exp.code[1] = 
	am_atom_put("characters_to_utf8_trap",23);
    characters_to_utf8_trap_exp.code[2] = 3;
    characters_to_utf8_trap_exp.code[3] =
	(BeamInstr) em_apply_bif;
    characters_to_utf8_trap_exp.code[4] = 
	(BeamInstr) &characters_to_utf8_trap;

    memset(&characters_to_list_trap_1_exp, 0, sizeof(Export));
    characters_to_list_trap_1_exp.address = 
	&characters_to_list_trap_1_exp.code[3];
    characters_to_list_trap_1_exp.code[0] = am_erlang;
    characters_to_list_trap_1_exp.code[1] = 
	am_atom_put("characters_to_list_trap_1",25);
    characters_to_list_trap_1_exp.code[2] = 3;
    characters_to_list_trap_1_exp.code[3] =
	(BeamInstr) em_apply_bif;
    characters_to_list_trap_1_exp.code[4] = 
	(BeamInstr) &characters_to_list_trap_1;

    memset(&characters_to_list_trap_2_exp, 0, sizeof(Export));
    characters_to_list_trap_2_exp.address = 
	&characters_to_list_trap_2_exp.code[3];
    characters_to_list_trap_2_exp.code[0] = am_erlang;
    characters_to_list_trap_2_exp.code[1] = 
	am_atom_put("characters_to_list_trap_2",25);
    characters_to_list_trap_2_exp.code[2] = 3;
    characters_to_list_trap_2_exp.code[3] =
	(BeamInstr) em_apply_bif;
    characters_to_list_trap_2_exp.code[4] = 
	(BeamInstr) &characters_to_list_trap_2;


    memset(&characters_to_list_trap_3_exp, 0, sizeof(Export));
    characters_to_list_trap_3_exp.address = 
	&characters_to_list_trap_3_exp.code[3];
    characters_to_list_trap_3_exp.code[0] = am_erlang;
    characters_to_list_trap_3_exp.code[1] = 
	am_atom_put("characters_to_list_trap_3",25);
    characters_to_list_trap_3_exp.code[2] = 3;
    characters_to_list_trap_3_exp.code[3] =
	(BeamInstr) em_apply_bif;
    characters_to_list_trap_3_exp.code[4] = 
	(BeamInstr) &characters_to_list_trap_3;

    memset(&characters_to_list_trap_4_exp, 0, sizeof(Export));
    characters_to_list_trap_4_exp.address = 
	&characters_to_list_trap_4_exp.code[3];
    characters_to_list_trap_4_exp.code[0] = am_erlang;
    characters_to_list_trap_4_exp.code[1] = 
	am_atom_put("characters_to_list_trap_4",25);
    characters_to_list_trap_4_exp.code[2] = 1;
    characters_to_list_trap_4_exp.code[3] =
	(BeamInstr) em_apply_bif;
    characters_to_list_trap_4_exp.code[4] = 
	(BeamInstr) &characters_to_list_trap_4;

    c_to_b_int_trap_exportp =  erts_export_put(am_unicode,am_characters_to_binary_int,2);
    c_to_l_int_trap_exportp =  erts_export_put(am_unicode,am_characters_to_list_int,2);
    

}


static ERTS_INLINE void *alloc_restart(size_t size)
{
    return erts_alloc(ERTS_ALC_T_UNICODE_BUFFER,size);
}

static ERTS_INLINE void free_restart(void *ptr)
{
    erts_free(ERTS_ALC_T_UNICODE_BUFFER, ptr);
}

static void cleanup_restart_context(RestartContext *rc)
{
    if (rc->bytes != NULL) {
	free_restart(rc->bytes);
	rc->bytes = NULL;
    }
}

static void cleanup_restart_context_bin(Binary *bp)
{
    RestartContext *rc = ERTS_MAGIC_BIN_DATA(bp);
    cleanup_restart_context(rc);
}

static RestartContext *get_rc_from_bin(Eterm bin)
{
    Binary *mbp;
    ASSERT(ERTS_TERM_IS_MAGIC_BINARY(bin));

    mbp = ((ProcBin *) binary_val(bin))->val;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp)
	   == cleanup_restart_context_bin);
    return (RestartContext *) ERTS_MAGIC_BIN_DATA(mbp);    
}

static Eterm make_magic_bin_for_restart(Process *p, RestartContext *rc)
{
    Binary *mbp = erts_create_magic_binary(sizeof(RestartContext),
					   cleanup_restart_context_bin);
    RestartContext *restartp = ERTS_MAGIC_BIN_DATA(mbp);
    Eterm *hp;
    memcpy(restartp,rc,sizeof(RestartContext));
    hp = HAlloc(p, PROC_BIN_SIZE);
    return erts_mk_magic_binary_term(&hp, &MSO(p), mbp);
}

	
Sint erts_unicode_set_loop_limit(Sint limit) 
{
    Sint save = (Sint) max_loop_limit;
    if (limit <= 0) {
	max_loop_limit = CONTEXT_REDS * LOOP_FACTOR;
    } else {
	max_loop_limit = (Uint) limit;
    }
    return save;
}

static ERTS_INLINE int allowed_iterations(Process *p)
{
    int tmp = ERTS_BIF_REDS_LEFT(p) * LOOP_FACTOR;
    int tmp2 = max_loop_limit;
    if (tmp2 < tmp)
	return tmp2;
    else
	return tmp;
}
static ERTS_INLINE int cost_to_proc(Process *p, int cost)
{
    int x = (cost / LOOP_FACTOR);
    BUMP_REDS(p,x);
    return x;
}
static ERTS_INLINE int simple_loops_to_common(int cost)
{
    int factor = (LOOP_FACTOR_SIMPLE / LOOP_FACTOR);
    return (cost / factor);
}

static Sint aligned_binary_size(Eterm binary)
{
    unsigned char *bytes;
    Uint bitoffs;
    Uint bitsize;
    
    ERTS_GET_BINARY_BYTES(binary, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	return (Sint) -1;
    }
    return binary_size(binary);
}

static Sint latin1_binary_need(Eterm binary)
{
    unsigned char *bytes;
    byte *temp_alloc = NULL;
    Uint bitoffs;
    Uint bitsize;
    Uint size;
    Sint need = 0;
    Sint i;
    
    ERTS_GET_BINARY_BYTES(binary, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	return (Sint) -1;
    }
    if (bitoffs != 0) {
	bytes = erts_get_aligned_binary_bytes(binary, &temp_alloc);
	/* The call to erts_get_aligned_binary_bytes cannot fail as 
	   we'we already checked bitsize and that this is a binary */
    }
    size = binary_size(binary);
    for(i = 0; i < size; ++i) {
	if (bytes[i] & ((byte) 0x80)) {
	    need += 2;
	} else {
	    need += 1;
	}
    }
    erts_free_aligned_binary_bytes(temp_alloc);
    return need;
}

static int utf8_len(byte first) 
{
    if ((first & ((byte) 0x80)) == 0) {
	return 1;
    } else if ((first & ((byte) 0xE0)) == 0xC0) {
	return 2;
    } else if ((first & ((byte) 0xF0)) == 0xE0) {
	return 3;
    } else if ((first & ((byte) 0xF8)) == 0xF0) {
	return 4;
    } 
    return -1;
}

static int copy_utf8_bin(byte *target, byte *source, Uint size, 
			 byte *leftover, int *num_leftovers, 
			 byte **err_pos, Uint *characters) {
    int copied = 0;
    if (leftover != NULL && *num_leftovers) {
	int need = utf8_len(leftover[0]);
	int from_source = need - (*num_leftovers);
	int c;
	byte *tmp_err_pos = NULL;
	ASSERT(need > 0);
	ASSERT(from_source > 0);
	if (size < from_source) {
	    memcpy(leftover + (*num_leftovers), source, size);
	    *num_leftovers += size;
	    return 0;
	}
	/* leftover has room for four bytes (see bif) */
	memcpy(leftover + (*num_leftovers),source,from_source);
	c = copy_utf8_bin(target, leftover, need, NULL, NULL, &tmp_err_pos, characters);
	if (tmp_err_pos != 0) {
	    *err_pos = source;
	    return 0;
	}
	copied += c;
	*num_leftovers = 0;
	size -= from_source;
	target += c;
	source += from_source;
    }
    while (size) {
	if (((*source) & ((byte) 0x80)) == 0) {
	    *(target++) = *(source++);
	    --size; ++copied;
	} else if (((*source) & ((byte) 0xE0)) == 0xC0) {
	    if (leftover && size < 2) {
		*leftover = *source;
		*num_leftovers = 1;
		break;
	    }
	    if (size < 2 || ((source[1] & ((byte) 0xC0)) != 0x80) ||
		((*source) < 0xC2) /* overlong */) {
		*err_pos = source;
		return copied;
	    }
	    *(target++) = *(source++);
	    *(target++) = *(source++);
	    size -= 2; copied += 2;
	} else if (((*source) & ((byte) 0xF0)) == 0xE0) {
	    if (leftover && size < 3) {
		memcpy(leftover, source, (int) size);
		*num_leftovers = (int) size;
		break;
	    }
	    if (size < 3 || ((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xE0) && (source[1] < 0xA0)) /* overlong */ ) {
		*err_pos = source;
		return copied;
	    }
	    if ((((*source) & ((byte) 0xF)) == 0xD) && 
		((source[1] & 0x20) != 0)) {
		*err_pos = source;
		return copied;
	    }

	    if (((*source) == 0xEF) && (source[1] == 0xBF) &&
		((source[2] == 0xBE) || (source[2] == 0xBF))) {
		*err_pos = source;
		return copied;
	    }
		
	    *(target++) = *(source++);
	    *(target++) = *(source++);
	    *(target++) = *(source++);
	    size -= 3; copied += 3;
	} else if (((*source) & ((byte) 0xF8)) == 0xF0) {
	    if (leftover && size < 4) {
		memcpy(leftover, source, (int) size);
		*num_leftovers = (int) size;
		break;
	    }
	    if (size < 4 || ((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		((source[3] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xF0) && (source[1] < 0x90)) /* overlong */) {
		*err_pos = source;
		return copied;
	    }
	    if ((((*source) & ((byte)0x7)) > 0x4U) ||
		((((*source) & ((byte)0x7)) == 0x4U) && 
		 ((source[1] & ((byte)0x3F)) > 0xFU))) {
		*err_pos = source;
		return copied;
	    }
	    *(target++) = *(source++);
	    *(target++) = *(source++);
	    *(target++) = *(source++);
	    *(target++) = *(source++);
	    size -= 4; copied +=4;
	} else {
	    *err_pos = source;
	    return copied;
	}
	++(*characters);
    }
    return copied;
}
	    
	    
    
static Sint utf8_need(Eterm ioterm, int latin1, Uint *costp) 
{
    Eterm *objp;
    Eterm obj;
    DECLARE_ESTACK(stack);
    Sint need = 0;
    Uint cost = 0;

    if (is_nil(ioterm)) {
	DESTROY_ESTACK(stack);
	*costp = 0;
	return need;
    }
    if(is_binary(ioterm)) {
	DESTROY_ESTACK(stack);
	if (latin1) {
	    Sint x = latin1_binary_need(ioterm);
	    *costp = x;
	    return x;
	} else {
	    *costp = 1;
	    return aligned_binary_size(ioterm);
	}
    }
    
    if (!is_list(ioterm)) {
	DESTROY_ESTACK(stack);
	*costp = 0;
	return (Sint) -1;
    }
    /* OK a list, needs to be processed in order, handling each flat list-level
       as they occur, just like io_list_to_binary would */
    ESTACK_PUSH(stack,ioterm);
    while (!ESTACK_ISEMPTY(stack)) {
	ioterm = ESTACK_POP(stack);
	if (is_nil(ioterm)) {
	    /* ignore empty lists */
	    continue;
	}
	if(is_list(ioterm)) {
L_Again:   /* Restart with sublist, old listend was pushed on stack */
	    objp = list_val(ioterm);
	    obj = CAR(objp);
	    for(;;) { /* loop over one flat list of bytes and binaries
		         until sublist or list end is encountered */
		if (is_small(obj)) { /* Always small */
		    for(;;) {
			Uint x = unsigned_val(obj);
			if (x < 0x80)
			    need +=1;
			else if (x < 0x800)
			    need += 2;
			else if (x < 0x10000) 
			    need += 3;
			else 
			    need += 4; 
			/* everything else will give badarg later 
			   in the process, so we dont check */
			++cost;
			ioterm = CDR(objp);
			if (!is_list(ioterm)) {
			    break;
			}
			objp = list_val(ioterm);
			obj = CAR(objp);
			if (!is_byte(obj))
			    break;
		    }
		} else if (is_nil(obj)) {
		    ioterm = CDR(objp);
		    if (!is_list(ioterm)) {
			break;
		    }
		    objp = list_val(ioterm);
		    obj = CAR(objp);
		} else if (is_list(obj)) {
		    /* push rest of list for later processing, start 
		       again with sublist */
		    ESTACK_PUSH(stack,CDR(objp));
		    ioterm = obj;
		    goto L_Again;
		} else if (is_binary(obj)) {
		    Sint x;

		    if (latin1) { 
			x = latin1_binary_need(obj);
			if (x < 0) {
			    DESTROY_ESTACK(stack);
			    *costp = cost;
			    return x;
			} 
			cost += x;
		    } else {
			x = aligned_binary_size(obj);
			if (x < 0) {
			    DESTROY_ESTACK(stack);
			    *costp = cost;
			    return x;
			} 
			++cost;
		    }
		    need += x;
		    ioterm = CDR(objp);
		    if (is_list(ioterm)) {
			/* objp and obj need to be updated if 
			   loop is to continue */
			objp = list_val(ioterm);
			obj = CAR(objp);
		    }
		} else {
		    DESTROY_ESTACK(stack);
		    *costp = cost;
		    return ((Sint) -1);
		} 
		if (is_nil(ioterm) || !is_list(ioterm)) {
		    break;
		}
	    } /* for(;;) */
	} /* is_list(ioterm) */
	
	if (!is_list(ioterm) && !is_nil(ioterm)) {
	    /* inproper list end */
	    if (is_binary(ioterm)) {
		Sint x; 
		if (latin1) {
		    x = latin1_binary_need(ioterm);
		    if (x < 0) {
			DESTROY_ESTACK(stack);
			*costp = cost;
			return x;
		    } 
		    cost += x;
		} else {
		    x = aligned_binary_size(ioterm);
		    if (x < 0) {
			DESTROY_ESTACK(stack);
			*costp = cost;
			return x;
		    } 
		    ++cost;
		}
		need += x;
	    } else {
		DESTROY_ESTACK(stack);
		*costp = cost;
		return ((Sint) -1);
	    }
	}
    } /* while  not estack empty */
    DESTROY_ESTACK(stack);
    *costp = cost;
    return need;
}
    
    
static Eterm do_build_utf8(Process *p, Eterm ioterm, int *left, int latin1,
			   byte *target, int *pos, Uint *characters, int *err, 
			   byte *leftover, int *num_leftovers)
{
    int c;
    Eterm *objp;
    Eterm obj;
    DECLARE_ESTACK(stack);

    *err = 0;
    if ((*left) <= 0 || is_nil(ioterm)) {
	DESTROY_ESTACK(stack);
	return ioterm;
    }
    if(is_binary(ioterm)) {
	Uint bitoffs;
	Uint bitsize;
	Uint size;
	Uint i;
	Eterm res_term = NIL;
	unsigned char *bytes;
	byte *temp_alloc = NULL;
	Uint orig_size;
	
	ERTS_GET_BINARY_BYTES(ioterm, bytes, bitoffs, bitsize);
	if (bitsize != 0) {
	    *err = 1;
	    DESTROY_ESTACK(stack);
	    return ioterm;
	}
	if (bitoffs != 0) {
	    bytes = erts_get_aligned_binary_bytes(ioterm, &temp_alloc);
	    /* The call to erts_get_aligned_binary_bytes cannot fail as 
	       we'we already checked bitsize and that this is a binary */
	}

	orig_size = size = binary_size(ioterm);

	/* This is done to avoid splitting binaries in two 
	   and then create an unnecessary rest that eventually gives an error.
	   For cases where errors are not returned this is unnecessary */
	if (!latin1) { 
	    /* Find a valid character boundary */
	    while (size > (*left) && 
		   (((byte) bytes[(*left)]) & ((byte) 0xC0)) == ((byte) 0x80)) {
		++(*left);
	    }
	}

	if (size > (*left)) {
	    Eterm *hp;
	    ErlSubBin *sb;
	    Eterm orig;
	    Uint offset;
	    /* Split the binary in two parts, of which we 
	       only process the first */
	    hp = HAlloc(p, ERL_SUB_BIN_SIZE);
	    sb = (ErlSubBin *) hp;
	    ERTS_GET_REAL_BIN(ioterm, orig, offset, bitoffs, bitsize);
	    sb->thing_word = HEADER_SUB_BIN;
	    sb->size = size - (*left);
	    sb->offs = offset + (*left);
	    sb->orig = orig;
	    sb->bitoffs = bitoffs;
	    sb->bitsize = bitsize;
	    sb->is_writable = 0;
	    res_term = make_binary(sb);
	    size = (*left);
	}

	if (!latin1) {
	    int num;
	    byte *err_pos = NULL;
	    num = copy_utf8_bin(target + (*pos), bytes, 
				size, leftover, num_leftovers,&err_pos,characters);
	    *pos += num;
	    if (err_pos != NULL) {
		int rest_bin_offset;
		int rest_bin_size;
		Eterm *hp;
		ErlSubBin *sb;
		Eterm orig;
		Uint offset;

		*err = 1;
		/* we have no real stack, just build a list of the binaries
		   we have not decoded... */
		DESTROY_ESTACK(stack);

		rest_bin_offset = (err_pos - bytes);
		rest_bin_size = orig_size - rest_bin_offset;
		
		hp = HAlloc(p, ERL_SUB_BIN_SIZE);
		sb = (ErlSubBin *) hp;
		ERTS_GET_REAL_BIN(ioterm, orig, offset, bitoffs, bitsize);
		sb->thing_word = HEADER_SUB_BIN;
		sb->size = rest_bin_size;
		sb->offs = offset + rest_bin_offset;
		sb->orig = orig;
		sb->bitoffs = bitoffs;
		sb->bitsize = bitsize;
		sb->is_writable = 0;
		res_term = make_binary(sb);
		erts_free_aligned_binary_bytes(temp_alloc);
		return res_term;
	    }
	} else {
	    i = 0;
	    while(i < size) {
		if (bytes[i] < 0x80) {
		    target[(*pos)++] = bytes[i++];
		} else {
		    target[(*pos)++] = ((bytes[i] >> 6) | ((byte) 0xC0));
		    target[(*pos)++] = ((bytes[i] & 0x3F) | ((byte) 0x80));
		    ++i;
		}
		++(*characters);
	    }
	}
	*left -= size;
	DESTROY_ESTACK(stack);
	erts_free_aligned_binary_bytes(temp_alloc);
	return res_term;
    }
	
    if (!is_list(ioterm)) {
	*err = 1;
	goto done;
    }

    /* OK a list, needs to be processed in order, handling each flat list-level
       as they occur, just like io_list_to_binary would */
    ESTACK_PUSH(stack,ioterm);
    while (!ESTACK_ISEMPTY(stack) && (*left)) {
	ioterm = ESTACK_POP(stack);
	if (is_nil(ioterm)) {
	    /* ignore empty lists */
	    continue;
	}
	if(is_list(ioterm)) {
L_Again:   /* Restart with sublist, old listend was pushed on stack */
	    objp = list_val(ioterm);
	    obj = CAR(objp);
	    for(;;) { /* loop over one flat list of bytes and binaries
		         until sublist or list end is encountered */
		if (is_small(obj)) { /* Always small in unicode*/
		    if (*num_leftovers) {
			/* Have rest from previous bin and this is an integer, not allowed */
			*err = 1;
			goto done;
		    }
		    for(;;) {
			Uint x = unsigned_val(obj);
			if (latin1 && x > 255) {
			    *err = 1;
			    goto done;
			}
			if (x < 0x80) {
			    target[(*pos)++] = (byte) x;
			}
			else if (x < 0x800) {
			    target[(*pos)++] = (((byte) (x >> 6)) | 
						 ((byte) 0xC0));
			    target[(*pos)++] = (((byte) (x & 0x3F)) | 
						((byte) 0x80));
			} else if (x < 0x10000) {
			    if ((x >= 0xD800 && x <= 0xDFFF) ||
				(x == 0xFFFE) ||
				(x == 0xFFFF)) { /* Invalid unicode range */
				*err = 1;
				goto done;
			    }
			    target[(*pos)++] = (((byte) (x >> 12)) | 
						((byte) 0xE0));
			    target[(*pos)++] = ((((byte) (x >> 6)) & 0x3F)  | 
						((byte) 0x80));
			    target[(*pos)++] = (((byte) (x & 0x3F)) | 
						((byte) 0x80));
			} else if (x < 0x110000) { /* Standard imposed max */
			    target[(*pos)++] = (((byte) (x >> 18)) | 
						((byte) 0xF0));
			    target[(*pos)++] = ((((byte) (x >> 12)) & 0x3F)  | 
						((byte) 0x80));
			    target[(*pos)++] = ((((byte) (x >> 6)) & 0x3F)  | 
						((byte) 0x80));
			    target[(*pos)++] = (((byte) (x & 0x3F)) | 
						((byte) 0x80));
			} else {
				*err = 1;
				goto done;
			}
			++(*characters);
			--(*left);
			ioterm = CDR(objp);
			if (!is_list(ioterm) || !(*left)) {
			    break;
			}
			objp = list_val(ioterm);
			obj = CAR(objp);
			if (!is_small(obj))
			    break;
		    }
		} else if (is_nil(obj)) {
		    ioterm = CDR(objp);
		    if (!is_list(ioterm)) {
			break;
		    }
		    objp = list_val(ioterm);
		    obj = CAR(objp);
		} else if (is_list(obj)) {
		    /* push rest of list for later processing, start 
		       again with sublist */
		    ESTACK_PUSH(stack,CDR(objp));
		    ioterm = obj;
		    goto L_Again;
		} else if (is_binary(obj)) {
		    Eterm rest_term;
		    rest_term = do_build_utf8(p,obj,left,latin1,target,pos, characters, err, 
					      leftover, num_leftovers);
		    if ((*err) != 0) {
			Eterm *hp;
			hp = HAlloc(p, 2);
			obj = CDR(objp);
			ioterm = CONS(hp, rest_term, obj);
			//(*left) = 0;
			goto done;
		    }
		    if (rest_term != NIL) {
			Eterm *hp;
			hp = HAlloc(p, 2);
			obj = CDR(objp);
			ioterm = CONS(hp, rest_term, obj);
			(*left) = 0;
			break;
		    }
		    ioterm = CDR(objp);
		    if (is_list(ioterm)) {
			/* objp and obj need to be updated if 
			   loop is to continue */
			objp = list_val(ioterm);
			obj = CAR(objp);
		    }
		} else {
		    *err = 1;
		    goto done;
		} 
		if (!(*left) || is_nil(ioterm) || !is_list(ioterm)) {
		    break;
		}
	    } /* for(;;) */
	} /* is_list(ioterm) */

	if ((*left) && !is_list(ioterm) && !is_nil(ioterm)) {
	    /* inproper list end */
	    if (is_binary(ioterm)) {
		ioterm = do_build_utf8(p,ioterm,left,latin1,target,pos,characters,err,leftover,num_leftovers);
		if ((*err) != 0) {
		    goto done;
		}
	    } else {
		*err = 1;
		goto done;
	    }
	}
    } /* while left and not estack empty */
 done:
    c = ESTACK_COUNT(stack);
    if (c > 0) {
	Eterm *hp = HAlloc(p,2*c);
	while(!ESTACK_ISEMPTY(stack)) {
	    Eterm st = ESTACK_POP(stack);
	    ioterm = CONS(hp, ioterm, st);
	    hp += 2;
	}
    }
    DESTROY_ESTACK(stack);
    return ioterm;

}

static int check_leftovers(byte *source, int size) 
{
    if (((*source) & ((byte) 0xE0)) == 0xC0) {
	return 0;
    } else if (((*source) & ((byte) 0xF0)) == 0xE0) {
	if (size < 2 || 
	    (size < 3 && ((source[1] & ((byte) 0xC0)) == 0x80))) { 
	    return 0;
	}
    } else if (((*source) & ((byte) 0xF8)) == 0xF0) {
	if (size < 2 ||
	    (size < 3 && ((source[1] & ((byte) 0xC0)) == 0x80)) ||
	    (size < 4 && 
	     ((source[1] & ((byte) 0xC0)) == 0x80) &&
	     ((source[2] & ((byte) 0xC0)) == 0x80))) {
	    return 0;
	}
    }
    return -1;
}
	
	 

static BIF_RETTYPE build_utf8_return(Process *p,Eterm bin,int pos,
			       Eterm rest_term,int err,
			       byte *leftover,int num_leftovers,Eterm latin1)
{
    Eterm *hp;
    Eterm ret;

    binary_size(bin) = pos;
    if (err) {
	if (num_leftovers > 0) {
	    Eterm leftover_bin = new_binary(p, leftover, num_leftovers);
	    hp = HAlloc(p,8);
	    rest_term = CONS(hp,rest_term,NIL);
	    hp += 2;
	    rest_term = CONS(hp,leftover_bin,rest_term);
	    hp += 2;
	} else {
	   hp = HAlloc(p,4);
	} 
	ret = TUPLE3(hp,am_error,bin,rest_term);
    } else if (rest_term == NIL && num_leftovers != 0) {
	Eterm leftover_bin = new_binary(p, leftover, num_leftovers);
	if (check_leftovers(leftover,num_leftovers) != 0) {
	    hp = HAlloc(p,4);
	    ret = TUPLE3(hp,am_error,bin,leftover_bin);
	} else {
	    hp = HAlloc(p,4);
	    ret = TUPLE3(hp,am_incomplete,bin,leftover_bin);
	}
    } else { /* All OK */	    
	if (rest_term != NIL) { /* Trap */
	    if (num_leftovers > 0) {
		Eterm rest_bin = new_binary(p, leftover, num_leftovers);
		hp = HAlloc(p,2);
		rest_term = CONS(hp,rest_bin,rest_term);
	    }
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(&characters_to_utf8_trap_exp, p, bin, rest_term, latin1);
	} else { /* Success */
	    /*hp = HAlloc(p,5);
	      ret = TUPLE4(hp,bin,rest_term,make_small(pos),make_small(err));*/
	    ret = bin;
	}
    }
    BIF_RET(ret);
}


static BIF_RETTYPE characters_to_utf8_trap(BIF_ALIST_3)
{
    Eterm *real_bin;
    Sint need;
    byte* bytes;
    Eterm rest_term;
    int left, sleft;
    int pos;
    int err;
    byte leftover[4]; /* used for temp buffer too, 
			 otherwise 3 bytes would have been enough */
    int num_leftovers = 0;
    int latin1 = 0;
    Uint characters = 0;
    
    /*erts_printf("Trap %T!\r\n",BIF_ARG_2);*/
    ASSERT(is_binary(BIF_ARG_1));
    real_bin = binary_val(BIF_ARG_1);
    ASSERT(*real_bin == HEADER_PROC_BIN);
    need = ((ProcBin *) real_bin)->val->orig_size;
    pos = (int) binary_size(BIF_ARG_1);
    bytes = binary_bytes(BIF_ARG_1);
    sleft = left = allowed_iterations(BIF_P);
    err = 0;
    if (BIF_ARG_3 == am_latin1) {
	latin1 = 1;
    } 
    rest_term = do_build_utf8(BIF_P, BIF_ARG_2, &left, latin1,
			      bytes, &pos, &characters, &err, leftover, &num_leftovers); 
    cost_to_proc(BIF_P, sleft - left);
    return build_utf8_return(BIF_P,BIF_ARG_1,pos,rest_term,err,
			      leftover,num_leftovers,BIF_ARG_3);
}

BIF_RETTYPE unicode_bin_is_7bit_1(BIF_ALIST_1)
{
    Sint need;
    if(!is_binary(BIF_ARG_1)) {
	BIF_RET(am_false);
    }
    need = latin1_binary_need(BIF_ARG_1);
    if(need >= 0 && aligned_binary_size(BIF_ARG_1) == need) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

static int is_valid_utf8(Eterm orig_bin)
{
    Uint bitoffs;
    Uint bitsize;
    Uint size;
    byte *temp_alloc = NULL;
    byte *endpos;
    Uint numchar;
    byte *bytes;
    int ret;

    ERTS_GET_BINARY_BYTES(orig_bin, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	return 0;
    }
    if (bitoffs != 0) {
	bytes = erts_get_aligned_binary_bytes(orig_bin, &temp_alloc);
    }
    size = binary_size(orig_bin);
    ret = analyze_utf8(bytes,
		       size,
		       &endpos,&numchar,NULL);
    erts_free_aligned_binary_bytes(temp_alloc);
    return (ret == UTF8_OK);
}

BIF_RETTYPE unicode_characters_to_binary_2(BIF_ALIST_2)
{
    Sint need;
    Uint characters;
    int latin1;
    Eterm bin;
    byte *bytes;
    int pos;
    int err;
    int left, sleft;
    Eterm rest_term, subject;
    byte leftover[4]; /* used for temp buffer too, o
			 therwise 3 bytes would have been enough */
    int num_leftovers = 0;
    Uint cost_of_utf8_need;


    if (BIF_ARG_2 == am_latin1) {
	latin1 = 1;
    } else if (BIF_ARG_2 == am_unicode || BIF_ARG_2 == am_utf8) {
	latin1 = 0;
    } else {
	BIF_TRAP2(c_to_b_int_trap_exportp, BIF_P, BIF_ARG_1, BIF_ARG_2);
    }	
    if (is_list(BIF_ARG_1) && is_binary(CAR(list_val(BIF_ARG_1))) && 
	is_nil(CDR(list_val(BIF_ARG_1)))) {
	subject = CAR(list_val(BIF_ARG_1));
    } else {
	subject = BIF_ARG_1;
    }

    need = utf8_need(subject,latin1,&cost_of_utf8_need);
    if (need < 0) {
	BIF_ERROR(BIF_P,BADARG);
    }
    if (is_binary(subject) && need >= 0 && aligned_binary_size(subject) == need
	&& (latin1 || is_valid_utf8(subject))) {
	cost_to_proc(BIF_P, simple_loops_to_common(cost_of_utf8_need)); 
	    BIF_RET(subject);
    }
	

    bin = erts_new_mso_binary(BIF_P, (byte *)NULL, need);
    bytes = binary_bytes(bin);
    cost_to_proc(BIF_P, simple_loops_to_common(cost_of_utf8_need)); 
    left = allowed_iterations(BIF_P) - 
	simple_loops_to_common(cost_of_utf8_need);
    if (left <= 0) {
	/* simplified - let everything be setup by setting left to 1 */
	left = 1;
    }
    sleft = left;
    pos = 0;
    err = 0;


    rest_term = do_build_utf8(BIF_P, subject, &left, latin1,
			      bytes, &pos, &characters, &err, leftover, &num_leftovers); 
#ifdef HARDDEBUG
    if (left == 0) {
	Eterm bin;
	if (is_binary(subject)) {
	    bin = subject;
	} else if(is_list(subject) && is_binary(CAR(list_val(subject)))) {
	    bin = CAR(list_val(subject));
	} else {
	    bin = NIL;
	}
	if (is_binary(bin)) {
	    byte *t = NULL;
	    Uint sz = binary_size(bin);
	    byte *by = erts_get_aligned_binary_bytes(bin,&t);
	    int i;
	    erts_printf("<<");
	    for (i = 0;i < sz; ++i) {
		erts_printf((i == sz -1) ? "0x%X" : "0x%X, ", (unsigned) by[i]);
	    }
	    erts_printf(">>: ");
	    erts_free_aligned_binary_bytes(t);
	}
	erts_printf("%d - %d = %d\n",sleft,left,sleft - left);
    }
#endif
    cost_to_proc(BIF_P, sleft - left); 
    return build_utf8_return(BIF_P,bin,pos,rest_term,err,
			     leftover,num_leftovers,BIF_ARG_2);
}

static BIF_RETTYPE build_list_return(Process *p, byte *bytes, int pos, Uint characters,
				     Eterm rest_term, int err,
				     byte *leftover, int num_leftovers,
				     Eterm latin1, int left)
{
    Eterm *hp;
    
    if (left <= 0) {
	left = 1;
    }
    
    if (err) {
	if (num_leftovers > 0) {
	    Eterm leftover_bin = new_binary(p, leftover, num_leftovers);
	    hp = HAlloc(p,4);
	    rest_term = CONS(hp,rest_term,NIL);
	    hp += 2;
	    rest_term = CONS(hp,leftover_bin,rest_term);
	}
	BIF_RET(finalize_list_to_list(p, bytes, rest_term, 0U, pos, characters, UTF8_ERROR, left, NIL));
    } else if (rest_term == NIL && num_leftovers != 0) {
	Eterm leftover_bin = new_binary(p, leftover, num_leftovers);
	if (check_leftovers(leftover,num_leftovers) != 0) {
	    BIF_RET(finalize_list_to_list(p, bytes, leftover_bin, 0U, pos, characters, UTF8_ERROR, 
					  left, NIL));
	} else {
	    BIF_RET(finalize_list_to_list(p, bytes, leftover_bin, 0U, pos, characters, UTF8_INCOMPLETE, 
					  left, NIL));
	}
    } else { /* All OK */	    
	if (rest_term != NIL) { /* Trap */
	    RestartContext rc;
	    if (num_leftovers > 0) {
		Eterm rest_bin = new_binary(p, leftover, num_leftovers);
		hp = HAlloc(p,2);
		rest_term = CONS(hp,rest_bin,rest_term);
	    }
	    BUMP_ALL_REDS(p);
	    rc.bytes = bytes;
	    rc.num_processed_bytes = 0; /* not used */
	    rc.num_bytes_to_process = pos;
	    rc.num_resulting_chars = characters;
	    rc.state = UTF8_OK; /* not used */
	    BIF_TRAP3(&characters_to_list_trap_1_exp, p, make_magic_bin_for_restart(p,&rc), 
		      rest_term, latin1);
	} else { /* Success */
	    BIF_RET(finalize_list_to_list(p, bytes, NIL, 0U, pos, characters, UTF8_OK, left, NIL));
	}
    }
}

static BIF_RETTYPE characters_to_list_trap_1(BIF_ALIST_3)
{
    RestartContext *rc;
    byte* bytes;
    int pos;
    Uint characters;
    int err;
    Eterm rest_term;
    int left, sleft;

    int latin1 = 0;
    byte leftover[4]; /* used for temp buffer too, 
			 otherwise 3 bytes would have been enough */
    int num_leftovers = 0;
    

    rc = get_rc_from_bin(BIF_ARG_1);

    bytes = rc->bytes;
    rc->bytes = NULL; /* to avoid free due to later GC */
    pos = rc->num_bytes_to_process;
    characters = rc->num_resulting_chars;

    sleft = left = allowed_iterations(BIF_P);
    err = 0;
    if (BIF_ARG_3 == am_latin1) {
	latin1 = 1;
    } 
    rest_term = do_build_utf8(BIF_P, BIF_ARG_2, &left, latin1,
			      bytes, &pos, &characters, &err, leftover, &num_leftovers); 
    cost_to_proc(BIF_P, sleft - left);
    return build_list_return(BIF_P,bytes,pos,characters,rest_term,err,
			      leftover,num_leftovers,BIF_ARG_3,left);
}

BIF_RETTYPE unicode_characters_to_list_2(BIF_ALIST_2)
{
    Sint need;
    int latin1;
    Uint characters = 0;
    byte *bytes;
    int pos;
    int err;
    int left, sleft;
    Eterm rest_term;
    byte leftover[4]; /* used for temp buffer too, o
			 therwise 3 bytes would have been enough */
    int num_leftovers = 0;
    Uint cost_of_utf8_need;

    if (BIF_ARG_2 == am_latin1) {
	latin1 = 1;
    } else if (BIF_ARG_2 == am_unicode || BIF_ARG_2 == am_utf8) {
	latin1 = 0;
    } else {
	BIF_TRAP2(c_to_l_int_trap_exportp, BIF_P, BIF_ARG_1, BIF_ARG_2);
    }	
    if (is_binary(BIF_ARG_1) && !latin1) { /* Optimized behaviour for this case */
	    return utf8_to_list(BIF_P,BIF_ARG_1);
    }
    need = utf8_need(BIF_ARG_1,latin1,&cost_of_utf8_need);
    if (need < 0) {
	BIF_ERROR(BIF_P,BADARG);
    }
    bytes = alloc_restart(need);
    cost_to_proc(BIF_P, simple_loops_to_common(cost_of_utf8_need)); 
    left = allowed_iterations(BIF_P) - 
	simple_loops_to_common(cost_of_utf8_need);
    if (left <= 0) {
	/* simplified - let everything be setup by setting left to 1 */
	left = 1;
    }
    sleft = left;
    pos = 0;
    err = 0;


    rest_term = do_build_utf8(BIF_P, BIF_ARG_1, &left, latin1,
			      bytes, &pos, &characters, &err, leftover, &num_leftovers); 
    cost_to_proc(BIF_P, sleft - left); 
    return build_list_return(BIF_P,bytes,pos,characters,rest_term,err,
			     leftover,num_leftovers,BIF_ARG_2,left);
}


/*
 * When input to characters_to_list is a plain binary and the format is 'unicode', we do
 * a faster analyze and size count with this function.
 */
static int analyze_utf8(byte *source, Uint size, 
			byte **err_pos, Uint *num_chars, int *left)
{
    *err_pos = source;
    *num_chars = 0;
    while (size) {
	if (((*source) & ((byte) 0x80)) == 0) {
	    source++;
	    --size; 
	} else if (((*source) & ((byte) 0xE0)) == 0xC0) {
	    if (size < 2) {
		return UTF8_INCOMPLETE;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((*source) < 0xC2) /* overlong */) {
		return UTF8_ERROR;
	    }
	    source += 2;
	    size -= 2;
	} else if (((*source) & ((byte) 0xF0)) == 0xE0) {
	    if (size < 3) {
		return UTF8_INCOMPLETE;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xE0) && (source[1] < 0xA0)) /* overlong */ ) {
		return UTF8_ERROR;
	    }
	    if ((((*source) & ((byte) 0xF)) == 0xD) && 
		((source[1] & 0x20) != 0)) {
		return UTF8_ERROR;
	    }
	    if (((*source) == 0xEF) && (source[1] == 0xBF) &&
		((source[2] == 0xBE) || (source[2] == 0xBF))) {
		return UTF8_ERROR;
	    }
	    source += 3;
	    size -= 3;
	} else if (((*source) & ((byte) 0xF8)) == 0xF0) {
	    if (size < 4) {
		return UTF8_INCOMPLETE;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		((source[3] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xF0) && (source[1] < 0x90)) /* overlong */) {
		return UTF8_ERROR;
	    }
	    if ((((*source) & ((byte)0x7)) > 0x4U) ||
		((((*source) & ((byte)0x7)) == 0x4U) && 
		 ((source[1] & ((byte)0x3F)) > 0xFU))) {
		return UTF8_ERROR;
	    }
	    source += 4;
	    size -= 4; 
	} else {
	    return UTF8_ERROR;
	}
	++(*num_chars);
	*err_pos = source;
	if (left && --(*left) <= 0) {
	    return UTF8_ANALYZE_MORE;
	}
    }
    return UTF8_OK;
}

/*
 * No errors should be able to occur - no overlongs, no malformed, no nothing
 */    
static Eterm do_utf8_to_list(Process *p, Uint num, byte *bytes, Uint sz, 
			     Uint left,
			     Uint *num_built, Uint *num_eaten, Eterm tail)
{
    Eterm *hp;
    Eterm ret;
    byte *source, *ssource;
    Uint unipoint;

    ASSERT(num > 0);
    if (left < num) {
	if (left > 0)
	    num = left;
	else
	    num = 1;
    }
    
    *num_built = num; /* Always */

    hp = HAlloc(p,num * 2);
    ret = tail;
    source = bytes + sz;
    ssource = source;
    while(--source >= bytes) {
	if (((*source) & ((byte) 0x80)) == 0) {
	    unipoint = (Uint) *source;
	} else if (((*source) & ((byte) 0xE0)) == 0xC0) {
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0x1F))) << 6) |
		((Uint) (source[1] & ((byte) 0x3F))); 	
	} else if (((*source) & ((byte) 0xF0)) == 0xE0) {
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0xF))) << 12) |
		(((Uint) (source[1] & ((byte) 0x3F))) << 6) |
		((Uint) (source[2] & ((byte) 0x3F))); 	 	
	} else if (((*source) & ((byte) 0xF8)) == 0xF0) {
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0x7))) << 18) |
		(((Uint) (source[1] & ((byte) 0x3F))) << 12) |
		(((Uint) (source[2] & ((byte) 0x3F))) << 6) |
		((Uint) (source[3] & ((byte) 0x3F))); 	 	
	} else {
	    /* ignore 2#10XXXXXX */
	    continue;
	}
	ret = CONS(hp,make_small(unipoint),ret);
	hp += 2;
	if (--num <= 0) {
	    break;
	}
    }
    *num_eaten = (ssource - source);
    return ret;
}

/*
 * The last step of characters_to_list, build a list from the buffer 'bytes' (created in the same way
 * as for characters_to_utf8). All sizes are known in advance and most data will be held in a 
 * "magic binary" during trapping.
 */
static BIF_RETTYPE finalize_list_to_list(Process *p, 
					 byte *bytes,
					 Eterm rest,
					 Uint num_processed_bytes,
					 Uint num_bytes_to_process, 
					 Uint num_resulting_chars, 
					 int state, int left,
					 Eterm tail) 
{
    Uint num_built; /* characters */
    Uint num_eaten; /* bytes */
    Eterm *hp;
    Eterm converted,ret;

    if (!num_bytes_to_process) {
	converted = tail;
    } else {
	num_built = 0;
	num_eaten = 0;
	converted = do_utf8_to_list(p, num_resulting_chars,
				    bytes, num_bytes_to_process,
				    left, &num_built, &num_eaten, tail);
	cost_to_proc(p,num_built);
	
	if (num_built != num_resulting_chars) { /* work left to do */
	    RestartContext rc;

	    rc.num_resulting_chars = num_resulting_chars - num_built;
	    rc.num_bytes_to_process = num_bytes_to_process - num_eaten;
	    rc.num_processed_bytes = num_processed_bytes + num_eaten;
	    rc.state = state;
	    rc.bytes = bytes;
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(&characters_to_list_trap_2_exp, p, 
		       make_magic_bin_for_restart(p, &rc), rest, converted); 
	}
    }

    /* 
     * OK, no more trapping, let's get rid of the temporary array...
     */

    free_restart(bytes);
    if (state == UTF8_INCOMPLETE) {
	hp = HAlloc(p,4);
	ret = TUPLE3(hp,am_incomplete,converted,rest);
    } else if (state == UTF8_ERROR) {
	hp = HAlloc(p,4);
	ret = TUPLE3(hp,am_error,converted,rest);
    } else {
	ret = converted;
    }

    BIF_RET(ret);
}
 
static BIF_RETTYPE characters_to_list_trap_2(BIF_ALIST_3)
{
    RestartContext *rc;
    byte *bytes;

    rc = get_rc_from_bin(BIF_ARG_1);

    bytes = rc->bytes;
    rc->bytes = NULL; /* Don't want this freed just yet... */
    return finalize_list_to_list(BIF_P, bytes, BIF_ARG_2, rc->num_processed_bytes,
				 rc->num_bytes_to_process, rc->num_resulting_chars,
				 rc->state, allowed_iterations(BIF_P), BIF_ARG_3);
}


/*
 * Hooks into the process of decoding a binary depending on state.
 * If last_state is UTF8_ANALYZE_MORE, num_bytes_to_process 
 * and num_resulting_chars will grow
 * until we're done analyzing the binary. Then we'll eat 
 * the bytes to process, lowering num_bytes_to_process and num_resulting_chars,
 * while increasing num_processed_bytes until we're done. the state 
 * indicates how to return (error, incomplete or ok) in this stage.
 * note that num_processed_bytes and num_bytes_to_process will make up the 
 * length of the binary part to process, not necessarily the length of the 
 * whole binary (if there are errors or an incomplete tail).
 *
 * Analyzing happens from the beginning of the binary towards the end,
 * while result is built from the end of the analyzed/accepted part 
 * towards the beginning.
 *
 * Note that this routine is *only* called when original input was a plain utf8 binary,
 * otherwise the rest and the sizes are known in advance, so finalize_list_to_list is 
 * used to build the resulting list (no analyzing needed).
 */
static BIF_RETTYPE do_bif_utf8_to_list(Process *p, 
				       Eterm orig_bin,
				       Uint num_processed_bytes,
				       Uint num_bytes_to_process, 
				       Uint num_resulting_chars, 
				       int state,
				       Eterm tail) 
{
    int left;
    Uint bitoffs;
    Uint bitsize;
    Uint size;
    byte *bytes;
    Eterm converted = NIL;
    Eterm rest = NIL;
    Eterm *hp;
    Eterm ret;
    byte *temp_alloc = NULL;
    byte *endpos;
    Uint numchar;

    Uint b_sz; /* size of the non analyzed tail */
    Uint num_built; /* characters */
    Uint num_eaten; /* bytes */

    ERTS_GET_BINARY_BYTES(orig_bin, bytes, bitoffs, bitsize);
    if (bitsize != 0) {
	converted = NIL;
	rest = orig_bin;
	goto error_return;
    }
    if (bitoffs != 0) {
	bytes = erts_get_aligned_binary_bytes(orig_bin, &temp_alloc);
    }
    
    size = binary_size(orig_bin);

    left = allowed_iterations(p);
    
    if (state == UTF8_ANALYZE_MORE) {
	state = analyze_utf8(bytes + num_bytes_to_process,
			     size - num_bytes_to_process,
			     &endpos,&numchar,&left);
	cost_to_proc(p,numchar);
	num_resulting_chars += numchar;
	num_bytes_to_process = endpos - bytes;
	if (state == UTF8_ANALYZE_MORE) {
	    Eterm epos = erts_make_integer(num_bytes_to_process,p);
	    Eterm enumchar = erts_make_integer(num_resulting_chars,p);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(&characters_to_list_trap_3_exp, p, orig_bin, epos, 
		      enumchar);
	}
    } 

    /* 
     * If we're here, we have everything analyzed and are instead building 
     */


    if (!num_bytes_to_process) {
	converted = tail;
    } else {
	num_built = 0;
	num_eaten = 0;
	converted = do_utf8_to_list(p, num_resulting_chars,
				    bytes, num_bytes_to_process,
				    left, &num_built, &num_eaten, tail);
	cost_to_proc(p,num_built);
	
	if (num_built != num_resulting_chars) { /* work left to do */
	    Eterm newnum_resulting_chars = 
		erts_make_integer(num_resulting_chars - num_built,p);
	    Eterm newnum_bytes_to_process = 
		erts_make_integer(num_bytes_to_process - num_eaten,p);
	    Eterm newnum_processed_bytes = 
		erts_make_integer(num_processed_bytes + num_eaten,p);
	    Eterm traptuple;
	    hp = HAlloc(p,7);
	    traptuple = TUPLE6(hp,orig_bin,newnum_processed_bytes,
			       newnum_bytes_to_process, 
			       newnum_resulting_chars,
			       make_small(state),
			       converted);
	    BUMP_ALL_REDS(p);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BIF_TRAP1(&characters_to_list_trap_4_exp,p,traptuple);
	}
    }

    /* 
     * OK, no more trapping, let's build rest binary if there should
     * be one. 
     */

    b_sz = size - (num_bytes_to_process + num_processed_bytes);

    if (b_sz) {
	ErlSubBin *sb;
	Eterm orig;
	Uint offset;
	ASSERT(state != UTF8_OK);
	hp = HAlloc(p, ERL_SUB_BIN_SIZE);
	sb = (ErlSubBin *) hp;
	ERTS_GET_REAL_BIN(orig_bin, orig, offset, bitoffs, bitsize);
	sb->thing_word = HEADER_SUB_BIN;
	sb->size = b_sz;
	sb->offs = num_bytes_to_process + num_processed_bytes;
	sb->orig = orig;
	sb->bitoffs = bitoffs;
	sb->bitsize = bitsize;
	sb->is_writable = 0;
	rest = make_binary(sb);
    } 

    /* Done */

    if (state == UTF8_INCOMPLETE) {
	if (check_leftovers(bytes + num_bytes_to_process + num_processed_bytes,
			    b_sz) != 0) {
	    goto error_return;
	}
	hp = HAlloc(p,4);
	ret = TUPLE3(hp,am_incomplete,converted,rest);
    } else if (state == UTF8_ERROR) {
 error_return:
	hp = HAlloc(p,4);
	ret = TUPLE3(hp,am_error,converted,rest);
    } else {
	ret = converted;
    }

    erts_free_aligned_binary_bytes(temp_alloc);
    BIF_RET(ret);
}


/* 
 * This is called when there's still analyzing left to do,
 * we only reach this if original input was a binary.
 */

static BIF_RETTYPE characters_to_list_trap_3(BIF_ALIST_3)
{
    Uint num_bytes_to_process;
    Uint num_resulting_chars;

    term_to_Uint(BIF_ARG_2, &num_bytes_to_process); /* The number of already
						       analyzed and accepted 
						       bytes */
    term_to_Uint(BIF_ARG_3, &num_resulting_chars); /* The number of chars
						      procuced by the
						      already analyzed
						      part of the binary */

    /*erts_printf("Trap: %T, %T, %T\n",BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);*/

    return do_bif_utf8_to_list(BIF_P, 
			       BIF_ARG_1, /* the binary */ 
			       0U, /* nothing processed yet */
			       num_bytes_to_process, 
			       num_resulting_chars,
			       UTF8_ANALYZE_MORE, /* always this state here */
			       NIL); /* Nothing built -> no tail yet */
	
}

/*
 * This is called when analyzing is done and we are trapped during building,
 * we only reach this if original input was a binary.
 */
static BIF_RETTYPE characters_to_list_trap_4(BIF_ALIST_1)
{
    Uint num_processed_bytes;
    Uint num_bytes_to_process;
    Uint num_resulting_chars;
    Eterm orig_bin, tail;
    int last_state;
    Eterm *tplp = tuple_val(BIF_ARG_1);

    orig_bin = tplp[1];
    term_to_Uint(tplp[2], &num_processed_bytes);
    term_to_Uint(tplp[3], &num_bytes_to_process);
    term_to_Uint(tplp[4], &num_resulting_chars);
    last_state = (int) signed_val(tplp[5]);
    tail = tplp[6];

    /*erts_printf("Trap: {%T, %lu, %lu, %lu, %d, %T}\n",
      orig_bin, num_processed_bytes, num_bytes_to_process, 
      num_resulting_chars, last_state, tail);*/

    return do_bif_utf8_to_list(BIF_P, 
			       orig_bin, /* The whole binary */
			       num_processed_bytes,  /* Number of bytes 
							already processed */
			       num_bytes_to_process, /* Bytes left to proc. */
			       num_resulting_chars,  /* Num chars left to 
							build */
			       last_state,           /* The current state 
							(never ANALYZE_MORE)*/
			       tail);                /* The already built 
							tail */  
	
}
/*
 * This is only used when characters are a plain unicode (utf8) binary.
 * Instead of building an utf8 buffer, we analyze the binary given and use that.
 */

static BIF_RETTYPE utf8_to_list(BIF_ALIST_1)
{
    if (!is_binary(BIF_ARG_1) || aligned_binary_size(BIF_ARG_1) < 0) {
	BIF_ERROR(BIF_P,BADARG);
    }
    return do_bif_utf8_to_list(BIF_P, BIF_ARG_1, 0U, 0U, 0U, 
			       UTF8_ANALYZE_MORE,NIL);
}


BIF_RETTYPE atom_to_binary_2(BIF_ALIST_2)
{
    Atom* ap;

    if (is_not_atom(BIF_ARG_1)) {
	goto error;
    }

    ap = atom_tab(atom_val(BIF_ARG_1));

    if (BIF_ARG_2 == am_latin1) {
	BIF_RET(new_binary(BIF_P, ap->name, ap->len));
    } else if (BIF_ARG_2 == am_utf8 || BIF_ARG_2 == am_unicode) {
	int bin_size = 0;
	int i;
	Eterm bin_term;
	byte* bin_p;

	for (i = 0; i < ap->len; i++) {
	    bin_size += (ap->name[i] >= 0x80) ? 2 : 1;
	}
	if (bin_size == ap->len) {
	    BIF_RET(new_binary(BIF_P, ap->name, ap->len));
	}
	bin_term = new_binary(BIF_P, 0, bin_size);
	bin_p = binary_bytes(bin_term);
	for (i = 0; i < ap->len; i++) {
	    byte b = ap->name[i];
	    if (b < 0x80) {
		*bin_p++ = b;
	    } else {
		*bin_p++ = 0xC0 | (b >> 6);
		*bin_p++ = 0x80 | (b & 0x3F);
	    }
	}
	BIF_RET(bin_term);
    } else {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
}

static BIF_RETTYPE
binary_to_atom(Process* p, Eterm bin, Eterm enc, int must_exist)
{
    byte* bytes;
    byte *temp_alloc = NULL;
    Uint bin_size;

    if ((bytes = erts_get_aligned_binary_bytes(bin, &temp_alloc)) == 0) {
	BIF_ERROR(p, BADARG);
    }
    bin_size = binary_size(bin);
    if (enc == am_latin1) {
	Eterm a;
	if (bin_size > MAX_ATOM_LENGTH) {
	system_limit:
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BIF_ERROR(p, SYSTEM_LIMIT);
	}
	if (!must_exist) {
	    a = am_atom_put((char *)bytes, bin_size);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BIF_RET(a);
	} else if (erts_atom_get((char *)bytes, bin_size, &a)) {
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BIF_RET(a);
	} else {
	    goto badarg;
	}
    } else if (enc == am_utf8 || enc == am_unicode) {
	char *buf;
	char *dst;
	int i;
	int num_chars;
	Eterm res;

	if (bin_size > 2*MAX_ATOM_LENGTH) {
	    byte* err_pos;
	    Uint n;
	    int reds_left = bin_size+1; /* Number of reductions left. */

	    if (analyze_utf8(bytes, bin_size, &err_pos,
			     &n, &reds_left) == UTF8_OK) {
		/* 
		 * Correct UTF-8 encoding, but too many characters to
		 * fit in an atom.
		 */
		goto system_limit;
	    } else {
		/*
		 * Something wrong in the UTF-8 encoding or Unicode code
		 * points > 255.
		 */
		goto badarg;
	    }
	}

	/*
	 * Allocate a temporary buffer the same size as the binary,
	 * so that we don't need an extra overflow test.
	 */
	buf = (char *) erts_alloc(ERTS_ALC_T_TMP, bin_size);
	dst = buf;
	for (i = 0; i < bin_size; i++) {
	    int c = bytes[i];
	    if (c < 0x80) {
		*dst++ = c;
	    } else if (i < bin_size-1) {
		int c2;
		if ((c & 0xE0) != 0xC0) {
		    goto free_badarg;
		}
		i++;
		c = (c & 0x3F) << 6;
		c2 = bytes[i];
		if ((c2 & 0xC0) != 0x80) {
		    goto free_badarg;
		}
		c = c | (c2 & 0x3F);
		if (0x80 <= c && c < 256) {
		    *dst++ = c;
		} else {
		    goto free_badarg;
		}
	    } else {
	    free_badarg:
		erts_free(ERTS_ALC_T_TMP, (void *) buf);
		goto badarg;
	    }
	}
	num_chars = dst - buf;
	if (num_chars > MAX_ATOM_LENGTH) {
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    goto system_limit;
	}
	if (!must_exist) {
	    res = am_atom_put(buf, num_chars);
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BIF_RET(res);
	} else {
	    int exists = erts_atom_get(buf, num_chars, &res);
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    if (exists) {
		erts_free_aligned_binary_bytes(temp_alloc);
		BIF_RET(res);
	    } else {
		goto badarg;
	    }
	}
    } else {
    badarg:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(p, BADARG);
    }
}

BIF_RETTYPE binary_to_atom_2(BIF_ALIST_2)
{
    return binary_to_atom(BIF_P, BIF_ARG_1, BIF_ARG_2, 0);
}

BIF_RETTYPE binary_to_existing_atom_2(BIF_ALIST_2)
{
    return binary_to_atom(BIF_P, BIF_ARG_1, BIF_ARG_2, 1);
}
