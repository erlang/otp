/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
#include "erl_iolist.h"
#include "big.h"
#include "zlib.h"
#define ERLANG_INTEGRATION 1
#define PCRE2_STATIC
//#include "pcre.h"
#define PCRE2_CODE_UNIT_WIDTH 8
#include "../pcre/pcre2.h"

#define LOOP_FACTOR 16

static Uint max_loop_limit;
static Export re_match_trap_export;
static BIF_RETTYPE re_match_trap(BIF_ALIST_3);
static Export *grun_trap_exportp = NULL;
static Export *urun_trap_exportp = NULL;
static Export *ucompile_trap_exportp = NULL;

static pcre2_general_context* the_general_ctx;
static pcre2_general_context* the_binary_general_ctx;
static pcre2_general_context* the_precomp_general_ctx;

static pcre2_compile_context* the_tmp_compile_ctx;
static pcre2_compile_context* the_precompile_ctx;

static BIF_RETTYPE re_compile(Process* p, Eterm re_arg, Eterm opts_arg, bool is_import);
static BIF_RETTYPE re_run(Process *p, Eterm arg1, Eterm arg2, Eterm arg3, bool first);

static void *our_pcre2_malloc(size_t size, void* null)
{
    const ErtsAlcType_t type = erts_initialized ? ERTS_ALC_T_RE_SHORTLIVED
                                                : ERTS_ALC_T_RE_INIT;
    return erts_alloc(type, size);
}

static void our_pcre2_free(void *ptr, void* null)
{
    /* Allocations made during initialization are never freed. */
    erts_free(ERTS_ALC_T_RE_SHORTLIVED, ptr);
}

erts_tsd_key_t the_binary_malloc_tsd_key;

static void *our_pcre2_binary_malloc(size_t size, void* null)
{
    if (erts_initialized) {
        Binary* bin = erts_bin_nrml_alloc(size);

        /* Use TSD to "return" Binary back to caller.
         * We assume only one malloc call per pcre2_serialize_encode()
         */
        ASSERT(erts_tsd_get(the_binary_malloc_tsd_key) == NULL);
        erts_tsd_set(the_binary_malloc_tsd_key, bin);

        return &(bin->orig_bytes);
    }
    else {
        /* Allocation of the_binary_general_ctx itself, which is never freed. */
        return erts_alloc(ERTS_ALC_T_RE_INIT, size);
    }
}

static void our_pcre2_binary_free(void *ptr, void* null)
{
    ASSERT(!"Dead code. Exported binary should be deallocated by GC.");
}

/*
 * The magic binary for a pre-compiled regex.
 * Just an indirection to the pcre2_code allocated and managed by PCRE2.
 * The lifetime is the same as the referred pcre2_code.
 *
 * This might look like an unnecessary indirection, but to allocate
 * the pcre2_code as a magic binary would force us to break the PCRE2 API
 * abstraction somehow.

 * According to PCRE2 docs compiled pcre2_code's are thread-safe. That is,
 * several schedulers may execute re:run with the same pcre2_code instance
 * without any need for synchronization or to copy it.
 */
struct regex_magic_indirect {
    pcre2_code* regex_code;
};

static int regex_code_destructor(Binary *mbp)
{
    struct regex_magic_indirect* indirect = ERTS_MAGIC_BIN_DATA(mbp);
    pcre2_code_free(indirect->regex_code);
#ifdef DEBUG
    indirect->regex_code = NULL;
#endif
    return 1;
}

static void *our_pcre2_precompile_malloc(size_t size, void* null)
{
    return erts_alloc(ERTS_ALC_T_RE_PRECOMPILE, size);
}

static void our_pcre2_precompile_free(void *ptr, void* null)
{
    erts_free(ERTS_ALC_T_RE_PRECOMPILE, ptr);
}

static bool magic_ref_to_pcre2_code(Eterm magic_ref, pcre2_code  **code_p)
{
    Binary *bin;
    struct regex_magic_indirect* indirect;

    if (!is_internal_magic_ref(magic_ref)) {
        return false;
    }
    bin = erts_magic_ref2bin(magic_ref);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != regex_code_destructor) {
        return false;
    }
    indirect = (struct regex_magic_indirect*) ERTS_MAGIC_BIN_DATA(bin);
    ASSERT(indirect->regex_code != NULL);

    *code_p = indirect->regex_code;
    return true;
}



#define ERTS_PCRE_STACK_MARGIN (10*1024)
#define ERTS_STACK_LIMIT ((char *) erts_get_stacklimit())

static int
stack_guard_downwards(uint32_t depth, void* null)
{
    char *limit = ERTS_STACK_LIMIT;
    char c;

    ASSERT(limit);

    return erts_check_below_limit(&c, limit + ERTS_PCRE_STACK_MARGIN);
}

static int
stack_guard_upwards(uint32_t depth, void* null)
{
    char *limit = ERTS_STACK_LIMIT;
    char c;

    ASSERT(limit);

    return erts_check_above_limit(&c, limit - ERTS_PCRE_STACK_MARGIN);
}

void erts_init_bif_re(void)
{
    char c;
    int (*stack_guard)(uint32_t, void *);

    /* We use value 0 as newline/bsr option not specified */
    ERTS_CT_ASSERT(PCRE2_NEWLINE_CR && PCRE2_NEWLINE_LF && PCRE2_NEWLINE_CRLF
                   && PCRE2_NEWLINE_ANY && PCRE2_NEWLINE_ANYCRLF
                   && PCRE2_NEWLINE_NUL);
    ERTS_CT_ASSERT(PCRE2_BSR_ANYCRLF && PCRE2_BSR_UNICODE);

    if (erts_check_if_stack_grows_downwards(&c))
        stack_guard = stack_guard_downwards;
    else
        stack_guard = stack_guard_upwards;

    the_general_ctx = pcre2_general_context_create(our_pcre2_malloc,
                                                   our_pcre2_free,
                                                   NULL);
    the_tmp_compile_ctx = pcre2_compile_context_create(the_general_ctx);
    pcre2_set_compile_recursion_guard(the_tmp_compile_ctx, stack_guard, NULL);

    the_precomp_general_ctx =
        pcre2_general_context_create(our_pcre2_precompile_malloc,
                                     our_pcre2_precompile_free,
                                     NULL);
    the_precompile_ctx = pcre2_compile_context_create(the_precomp_general_ctx);
    pcre2_set_compile_recursion_guard(the_precompile_ctx, stack_guard,
                                      NULL);

    the_binary_general_ctx =
        pcre2_general_context_create(our_pcre2_binary_malloc,
                                     our_pcre2_binary_free,
                                     NULL);
    erts_tsd_key_create(&the_binary_malloc_tsd_key, "re_binary_malloc");

    max_loop_limit = CONTEXT_REDS * LOOP_FACTOR;
    erts_init_trap_export(&re_match_trap_export, am_erlang, am_re_run_trap, 3,
			  &re_match_trap);
    grun_trap_exportp =  erts_export_put(am_re,am_grun,3);
    urun_trap_exportp =  erts_export_put(am_re,am_urun,3);
    ucompile_trap_exportp =  erts_export_put(am_re,am_ucompile,2);

    return;
}

Sint erts_re_set_loop_limit(Sint limit) 
{
    Sint save = (Sint) max_loop_limit;
    if (limit <= 0) {
	max_loop_limit = CONTEXT_REDS * LOOP_FACTOR;
    } else {
	max_loop_limit = (Uint) limit;
    }
    return save;
}

/*
 * Deal with plain int's and so on for the library interface
 */

static int term_to_int(Eterm term, int *sp)
{
#if defined(ARCH_64)

    if (is_small(term)) {
	Uint x = signed_val(term);
	if (x > INT_MAX) {
	    return 0;
	}
	*sp = (int) x;
	return 1;
    } 
    return 0;

#else

    if (is_small(term)) {
	*sp = signed_val(term);
	return 1;
    } else if (is_big(term)) {
	ErtsDigit* xr = big_v(term);
	dsize_t xl = big_size(term);
	int sign = big_sign(term);
	unsigned uval = 0;
	int n = 0;

	if (xl*D_EXP > sizeof(unsigned)*8) {
	    return 0;
	}
	while (xl-- > 0) {
	    uval |= ((unsigned)(*xr++)) << n;
	    n += D_EXP;
	}
	if (sign) {
	    uval = -uval;
	    if ((int)uval > 0)
		return 0;
	} else {
	    if ((int)uval < 0)
		return 0;
	}
	*sp = uval;
	return 1;
    } else {
	return 0;
    }

#endif

}

static Eterm make_signed_integer(int x, Process *p)
{
#if defined(ARCH_64)
    return make_small(x);
#else
    Eterm* hp;
    if (IS_SSMALL(x))
	return make_small(x);
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	if (x >= 0) {
	    *hp = make_pos_bignum_header(1);
	} else {
	    x = -x;
	    *hp = make_neg_bignum_header(1);
	}
	BIG_DIGIT(hp, 0) = x;
	return make_big(hp);
    }
#endif
}

/*
 * Parse option lists
 */

#define PARSE_FLAG_UNIQUE_COMPILE_OPT 1
#define PARSE_FLAG_UNIQUE_EXEC_OPT 2
#define PARSE_FLAG_UNICODE 4
#define PARSE_FLAG_STARTOFFSET 8
#define PARSE_FLAG_CAPTURE_OPT 16
#define PARSE_FLAG_GLOBAL 32
#define PARSE_FLAG_REPORT_ERRORS 64
#define PARSE_FLAG_MATCH_LIMIT 128
#define PARSE_FLAG_MATCH_LIMIT_RECURSION 256
#define PARSE_FLAG_EXPORT 512

#define CAPSPEC_VALUES 0
#define CAPSPEC_TYPE 1
#define CAPSPEC_SIZE 2
#define CAPSPEC_INIT {0,0}


struct parsed_options {
    uint32_t compile;   // Option arg to pcre2_compile
    uint32_t newline;   // PCRE2_NEWLINE_*
    uint32_t bsr;       // PCRE2_BSR_*
    uint32_t match;     // Option arg to pcre2_match
    int flags;
    int startoffset;
    Eterm capture[CAPSPEC_SIZE];
    uint32_t match_limit;
    uint32_t match_limit_recursion;
};

static bool parse_options(Eterm listp, struct parsed_options* po)
{
    po->compile  = 0;
    po->newline = 0;
    po->bsr = 0;
    po->match = 0;
    po->flags = 0;
    po->startoffset = 0;
    po->capture[0] = 0;
    po->capture[1] = 0;
    ERTS_UNDEF(po->match_limit, 0);
    ERTS_UNDEF(po->match_limit_recursion, 0);

    for (;is_list(listp); listp = CDR(list_val(listp))) {
	    Eterm item = CAR(list_val(listp));
	    if (is_tuple(item)) {
		Eterm *tp = tuple_val(item);
		if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
		    if (arityval(*tp) == 3 && tp[1] == am_capture) {
                        po->capture[CAPSPEC_VALUES] = tp[2];
                        po->capture[CAPSPEC_TYPE] = tp[3];
			po->flags |= (PARSE_FLAG_CAPTURE_OPT |
			       PARSE_FLAG_UNIQUE_EXEC_OPT);
                        continue;
		    } else {
                        return false;
                    }
                }
		switch(tp[1]) {
		case am_capture:
                    po->capture[CAPSPEC_VALUES] = tp[2];
                    po->capture[CAPSPEC_TYPE] = am_index;
		    po->flags |= (PARSE_FLAG_CAPTURE_OPT |
			   PARSE_FLAG_UNIQUE_EXEC_OPT);
		    break;
		case am_offset:
		    { 
			int tmp;
			if (!term_to_int(tp[2],&tmp) || tmp < 0) {
			    return false;
			}
                        po->startoffset = tmp;
		    }
		    po->flags |= (PARSE_FLAG_UNIQUE_EXEC_OPT|PARSE_FLAG_STARTOFFSET);
		    break;
		case am_match_limit:
		    { 
			int tmp;
			if (!term_to_int(tp[2],&tmp) || tmp < 0) {
			    return false;
			}
                        po->match_limit = tmp;
		    }
		    po->flags |= (PARSE_FLAG_UNIQUE_EXEC_OPT|PARSE_FLAG_MATCH_LIMIT);
		    break;
		case am_match_limit_recursion:
		    { 
			int tmp;
			if (!term_to_int(tp[2],&tmp) || tmp < 0) {
			    return false;
			}
                        po->match_limit_recursion = tmp;
		    }
		    po->flags |= (PARSE_FLAG_UNIQUE_EXEC_OPT|
			   PARSE_FLAG_MATCH_LIMIT_RECURSION);
		    break;
		case am_newline:
		    switch (tp[2]) {
		    case am_cr: 
                        po->newline = PCRE2_NEWLINE_CR;
			break;
                    case am_crlf:
                        po->newline = PCRE2_NEWLINE_CRLF;
			break;
		    case am_lf: 
                        po->newline = PCRE2_NEWLINE_LF;
			break;
		    case am_nul:
			 po->newline = PCRE2_NEWLINE_NUL;
			break;
		    case am_anycrlf: 
                        po->newline = PCRE2_NEWLINE_ANYCRLF;
			break;
		    case am_any: 
                        po->newline = PCRE2_NEWLINE_ANY;
			break;
		    default:
			return false;
			break;
		    }
		    break;
		default:
		    return false;
		}
	    } else {
		switch(item) {
		case am_anchored:
		    po->compile  |= PCRE2_ANCHORED;
		    po->match |= PCRE2_ANCHORED;
		    break;
		case am_notempty:
		    po->match |= PCRE2_NOTEMPTY;
		    po->flags |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_notempty_atstart:
		    po->match |= PCRE2_NOTEMPTY_ATSTART;
		    po->flags |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_notbol:
		    po->match |= PCRE2_NOTBOL;
		    po->flags |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_noteol:
		    po->match |= PCRE2_NOTEOL;
		    po->flags |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_no_start_optimize:
		    po->compile |= PCRE2_NO_START_OPTIMIZE;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_caseless:
		    po->compile |= PCRE2_CASELESS;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_dollar_endonly:
		    po->compile |= PCRE2_DOLLAR_ENDONLY;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_dotall:
		    po->compile |= PCRE2_DOTALL;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_extended:
		    po->compile |= PCRE2_EXTENDED;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_firstline:
		    po->compile |= PCRE2_FIRSTLINE;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_multiline:
		    po->compile |= PCRE2_MULTILINE;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_no_auto_capture:
		    po->compile |= PCRE2_NO_AUTO_CAPTURE;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_dupnames:
		    po->compile |= PCRE2_DUPNAMES;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_ungreedy:
		    po->compile |= PCRE2_UNGREEDY;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_ucp:
		    po->compile |= PCRE2_UCP;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_never_utf:
		    po->compile |= PCRE2_NEVER_UTF;
		    po->flags |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_report_errors:
		    po->flags |= (PARSE_FLAG_UNIQUE_EXEC_OPT |
			   PARSE_FLAG_REPORT_ERRORS);
		    break;
		case am_unicode:
		    po->compile |= PCRE2_UTF;
		    po->flags |= (PARSE_FLAG_UNIQUE_COMPILE_OPT | PARSE_FLAG_UNICODE);
		    break;
		case am_global:
		    po->flags |= (PARSE_FLAG_UNIQUE_EXEC_OPT | PARSE_FLAG_GLOBAL);
		    break;
                case am_bsr_anycrlf:
                    po->bsr = PCRE2_BSR_ANYCRLF;
		    break;
		case am_bsr_unicode: 
                    po->bsr = PCRE2_BSR_UNICODE;
		    break;
                case am_export:
                    po->flags |= (PARSE_FLAG_EXPORT | PARSE_FLAG_UNIQUE_COMPILE_OPT);
                    break;
		default:
		    return false;
		}
	    }
    }
    if (is_not_nil(listp)) {
        return false;
    }
    return true;
}

/*
 * Regex compile helper
 */
static pcre2_code *compile(const char* expr,
                           ErlDrvSizeT slen,
			   const struct parsed_options *opts,
                           pcre2_compile_context *compile_ctx,
                           int *errcode,
                           PCRE2_SIZE *errofset)
{
    pcre2_compile_context* ctx;
    pcre2_code *result;

    if (opts->newline | opts->bsr) {
        ctx = pcre2_compile_context_copy(compile_ctx);
        if (opts->newline) {
            pcre2_set_newline(ctx, opts->newline);
        }
        if (opts->bsr) {
            pcre2_set_bsr(ctx, opts->bsr);
        }
    }
    else {
        ctx = compile_ctx;
    }
    result = pcre2_compile_8((const PCRE2_UCHAR8 *)expr, slen, opts->compile,
                             errcode, errofset, ctx);
    if (ctx != compile_ctx) {
        pcre2_compile_context_free(ctx);
    }
    return result;
}

/*
 * Build Erlang term result from successful compilation
 */
static Eterm
build_compile_result(Process *p, pcre2_code *result, byte unicode, bool with_ok)
{
    Eterm *hp;
    Eterm ret;
    uint32_t capture_count;
    uint32_t newline;
    int use_crlf;
    Binary* magic_bin;
    Eterm magic_ref;
    struct regex_magic_indirect* indirect;

    ASSERT(result);

    pcre2_pattern_info(result, PCRE2_INFO_CAPTURECOUNT, &capture_count);
    pcre2_pattern_info(result, PCRE2_INFO_NEWLINE, &newline);
    use_crlf = (newline == PCRE2_NEWLINE_ANY ||
		newline == PCRE2_NEWLINE_CRLF ||
		newline == PCRE2_NEWLINE_ANYCRLF);

    magic_bin = erts_create_magic_binary(sizeof(struct regex_magic_indirect),
					 regex_code_destructor);
    indirect = ERTS_MAGIC_BIN_DATA(magic_bin);
    indirect->regex_code = result;

    hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + 6 + (with_ok ? 3 : 0));
    magic_ref = erts_mk_magic_ref(&hp, &MSO(p), magic_bin);
    ret = TUPLE5(hp, am_re_pattern, make_small(capture_count),
		 make_small(unicode), make_small(use_crlf), magic_ref);
    if (with_ok) {
	hp += 6;
	ret = TUPLE2(hp,am_ok,ret);
    }
    return ret;
}

#define EXPORTED_HDR_TITLE_SZ 8
#define EXPORTED_HDR_CHECKSUM_SZ 4
#define EXPORTED_HDR_ENCODE_VER_SZ 1
#define EXPORTED_HDR_UNICODE_SZ 1

#define EXPORTED_HDR_TITLE_OFFS 0
#define EXPORTED_HDR_CHECKSUM_OFFS    (EXPORTED_HDR_TITLE_OFFS + EXPORTED_HDR_TITLE_SZ)
#define EXPORTED_HDR_ENCODE_VER_OFFS  (EXPORTED_HDR_CHECKSUM_OFFS + EXPORTED_HDR_CHECKSUM_SZ)
#define EXPORTED_HDR_UNICODE_OFFS     (EXPORTED_HDR_ENCODE_VER_OFFS + EXPORTED_HDR_ENCODE_VER_SZ)
#define EXPORTED_HDR_SZ               (EXPORTED_HDR_UNICODE_OFFS + EXPORTED_HDR_UNICODE_SZ)

/*
 * Bump this version if for some reason the encoded binary format need to change
 * while the PCRE version is the same. That is, if we want to force fallback to
 * compilation without even looking at the exported stuff.
 */
#define EXPORTED_ENCODE_VERSION 1

static uint32_t
calc_checksum(const byte* encoded, Uint encoded_sz)
{
    return crc32(0, encoded, encoded_sz);
}

/*
 * Build Erlang binary exported result from successful compilation
 */
static Eterm
build_compile_export(Process *p, const pcre2_code *result, byte unicode,
                     Eterm regex_bin, Eterm opts)
{
    Eterm *hp, *hp_end;
    Uint hsz;
    Eterm ret, encode_bin_term, hdr_bin_term;
    uint8_t* serialized_bytes;
    PCRE2_SIZE serialized_size;
    Binary* bin;
    int32_t encode_res;
    byte *hdr;
    uint32_t chksum;

    ASSERT(result);

#ifdef DEBUG
    erts_tsd_set(the_binary_malloc_tsd_key, NULL);
#endif
    encode_res = pcre2_serialize_encode_8(&result, 1,
                                          &serialized_bytes, &serialized_size,
                                          the_binary_general_ctx);
    ASSERT(encode_res == 1); (void)encode_res;

    bin = erts_tsd_get(the_binary_malloc_tsd_key);
    ASSERT(bin);
    ASSERT((char*)serialized_bytes >= bin->orig_bytes);
    ASSERT((char*)serialized_bytes + serialized_size <= bin->orig_bytes + bin->orig_size);

    hsz = 3 + 6 + ERL_REFC_BITS_SIZE;
    hp = HAlloc(p, hsz);
    hp_end = hp + hsz;

    encode_bin_term = erts_wrap_refc_bitstring(&MSO(p).first,
                                               &MSO(p).overhead,
                                               &hp,
                                               bin,
                                               serialized_bytes,
                                               0,
                                               NBITS(serialized_size));

    hdr_bin_term = erts_new_binary(p, EXPORTED_HDR_SZ, &hdr);

    sys_memcpy(hdr + EXPORTED_HDR_TITLE_OFFS, "re-PCRE2", EXPORTED_HDR_TITLE_SZ);
    put_int8(EXPORTED_ENCODE_VERSION, hdr + EXPORTED_HDR_ENCODE_VER_OFFS);
    put_int8(unicode, hdr + EXPORTED_HDR_UNICODE_OFFS);

    chksum = calc_checksum(serialized_bytes, serialized_size);
    put_int32(chksum, hdr + EXPORTED_HDR_CHECKSUM_OFFS);

    ret = TUPLE5(hp, am_re_exported_pattern, hdr_bin_term, regex_bin, opts, encode_bin_term);
    hp += 6;
    ret = TUPLE2(hp, am_ok, ret);
    hp += 3;
    ASSERT(hp == hp_end); (void)hp_end;

    return ret;
}

BIF_RETTYPE
re_import_1(BIF_ALIST_1)
{
    Eterm* tpl;
    pcre2_code *regex_code;
    int32_t decode_ret;
    uint32_t chksum;
    const byte* hdr;
    Uint hdr_sz;
    const byte *hdr_tmp_alloc = NULL;
    const byte *encoded_tmp_alloc = NULL;
    byte enc_ver;
    byte unicode;

    ERTS_UNDEF(regex_code, NULL);
    ERTS_UNDEF(unicode, 0);

    // {re_exported_pattern, HeaderBin, OrigBin, OrigOpts, EncodedBin}

    if (!is_tuple_arity(BIF_ARG_1, 5)) {
        goto badarg;
    }
    tpl = tuple_val(BIF_ARG_1);
    if (tpl[1] != am_re_exported_pattern) {
        goto badarg;
    }

    hdr = erts_get_aligned_binary_bytes(tpl[2], &hdr_sz, &hdr_tmp_alloc);
    if (!hdr || hdr_sz < EXPORTED_HDR_SZ
        || sys_memcmp(hdr, "re-PCRE2", EXPORTED_HDR_TITLE_SZ) != 0) {
        goto badarg;
    }
    enc_ver = get_int8(hdr + EXPORTED_HDR_ENCODE_VER_OFFS);
    if (enc_ver == EXPORTED_ENCODE_VERSION) {
        const byte *encoded;
        Uint encoded_sz;

        /*
         * Allow header to contain more unknow data that we ignore.
         * Could be new optional features (such as checksum over fallback)
         * that was added while being forward compatible.
         */
        /*if (hdr_sz != EXPORTED_HDR_SZ) {
            goto badarg;
        }*/

        encoded = erts_get_aligned_binary_bytes(tpl[5], &encoded_sz,
                                                &encoded_tmp_alloc);
        if (!encoded) {
            goto badarg;
        }

        chksum = get_uint32(hdr + EXPORTED_HDR_CHECKSUM_OFFS);
        if (chksum != calc_checksum(encoded, encoded_sz)) {
            goto badarg;
        }
        unicode = get_int8(hdr + EXPORTED_HDR_UNICODE_OFFS);

        decode_ret = pcre2_serialize_decode_8(&regex_code, 1,
                                              encoded,
                                              the_precomp_general_ctx);
    }
    else {
        /*
         * Incorrect export encode format.
         * Don't even look at tpl[5] and instead act as if the decode failed
         * and fallback to compile regex below.
         */
        decode_ret = PCRE2_ERROR_BADMODE;
    }

    erts_free_aligned_binary_bytes(hdr_tmp_alloc);
    hdr_tmp_alloc = NULL;
    erts_free_aligned_binary_bytes(encoded_tmp_alloc);
    encoded_tmp_alloc = NULL;

    switch (decode_ret) {
    case 1: // Ok
        return build_compile_result(BIF_P, regex_code, unicode, false);

    case PCRE2_ERROR_BADMODE:
    case PCRE2_ERROR_BADMAGIC:
        // Wrong architecture or PCRE version, try compile orig regex.
        if (is_bitstring(tpl[3])) {
            return re_compile(BIF_P, tpl[3], tpl[4], true);
        }
    }
    ASSERT(decode_ret < 0);

badarg:
    erts_free_aligned_binary_bytes(hdr_tmp_alloc);
    erts_free_aligned_binary_bytes(encoded_tmp_alloc);
    BIF_ERROR(BIF_P, BADARG);
}


/*
 * Build Erlang term result from FAILED compilation
 */
static Eterm 
build_compile_error(Process *p,
		    int errcode, PCRE2_SIZE errofset,
		    Eterm extra_err_tag)
{
    Eterm *hp;
    Eterm ret;
    int elen, need;
    PCRE2_UCHAR8 errstr[120];

    /* Return {error, {Code, String, Offset}} */
    if (pcre2_get_error_message(errcode, errstr, sizeof(errstr))
	== PCRE2_ERROR_BADDATA) {
	erts_snprintf((char*)errstr, sizeof(errstr), "Unknown error (%d)", errcode);
    }
    elen = sys_strlen((const char*)errstr);
    need = 3 /* tuple of 2 */ +
	3 /* tuple of 2 */ +
	(2 * elen) /* The error string list */ +
	((extra_err_tag != NIL) ? 3 : 0);
    hp = HAlloc(p, need);
    ret = buf_to_intlist(&hp, (char *) errstr, elen, NIL);
    ret = TUPLE2(hp, ret, make_small(errofset));
    hp += 3;
    if (extra_err_tag != NIL) {
	/* Return {error, {extra_tag,
	       {Code, String, Offset}}} instead */
	ret =  TUPLE2(hp, extra_err_tag, ret);
	hp += 3;
    }
    ret = TUPLE2(hp, am_error, ret);
    return ret;
}


/*
 * Compile BIFs
 */

BIF_RETTYPE
re_version_0(BIF_ALIST_0)
{
    byte version[24];
    int version_size = pcre2_config(PCRE2_CONFIG_VERSION, version) - 1;

    BIF_RET(erts_new_binary_from_data(BIF_P, version_size, version));
}

static bool get_iolist_as_bytes(Process* p,
                                Eterm iolist,
                                byte **bytes_p,
                                ErlDrvSizeT *slen_p,
                                byte** tmp_buf_p,
                                Eterm* resbin_p)
{
    int buffres;

    ASSERT(tmp_buf_p || resbin_p);

    if (is_bitstring(iolist)) {
        Uint bit_offs, bit_sz;

        ERTS_GET_BITSTRING(iolist, *bytes_p, bit_offs, bit_sz);
        if (!BIT_OFFSET(bit_offs) && !TAIL_BITS(bit_sz)) {
            *bytes_p += BYTE_OFFSET(bit_offs);
            *slen_p = BYTE_SIZE(bit_sz);
            if (resbin_p) {
                *resbin_p = iolist;
            }
            return true;
        }
    }

    if (erts_iolist_size(iolist, slen_p)) {
        return false;
    }
    if (resbin_p) {
        *resbin_p = erts_new_binary(p, *slen_p, bytes_p);
    }
    else {
        *bytes_p = *tmp_buf_p = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, *slen_p);
    }

    buffres = erts_iolist_to_buf(iolist, (char *)*bytes_p, *slen_p);
    ASSERT(buffres >= 0); (void)buffres;
    return true;
}

static BIF_RETTYPE
re_compile(Process* p, Eterm re_arg, Eterm opts_arg, bool is_import)
{
    ErlDrvSizeT slen;
    byte *expr;
    byte *tmp_expr = NULL;
    pcre2_code *result;
    int errcode = 0;
    PCRE2_SIZE errofset = 0;
    Eterm ret;
    byte unicode = 0;
    bool is_export;
    struct parsed_options opts;
    Eterm regex_bin;
    Eterm* regex_bin_p;

    ERTS_UNDEF(regex_bin, THE_NON_VALUE);

    if (!parse_options(opts_arg, &opts)) {
    opt_error:
        p->fvalue = am_badopt;
	BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
    }

    if (opts.flags & PARSE_FLAG_UNIQUE_EXEC_OPT) {
        goto opt_error;
    }

    unicode = (opts.flags & PARSE_FLAG_UNICODE) ? 1 : 0;
    is_export = !is_import && opts.flags & PARSE_FLAG_EXPORT;

    if (unicode && !is_bitstring(re_arg)) {
        BIF_TRAP2(ucompile_trap_exportp, p, re_arg, opts_arg);
    }

    regex_bin_p = is_export ? &regex_bin : NULL;

    if (!get_iolist_as_bytes(p, re_arg, &expr, &slen, &tmp_expr, regex_bin_p)) {
        BIF_ERROR(p,BADARG);
    }

    result = compile((char*)expr, slen, &opts,
                     (is_export ? the_tmp_compile_ctx : the_precompile_ctx),
                     &errcode, &errofset);

    if (!result) {
        if (is_import) {
            ERTS_BIF_PREP_ERROR(ret, p, BADARG);
        }
        else {
            ret = build_compile_error(p, errcode, errofset, NIL);
        }
    }
    else if (is_export) {
        ret = build_compile_export(p, result, unicode, regex_bin, opts_arg);
        pcre2_code_free(result);
    }
    else {
        ret = build_compile_result(p, result, unicode, !is_import);
    }

    if (tmp_expr) {
        erts_free(ERTS_ALC_T_RE_TMP_BUF, tmp_expr);
    }
    BIF_RET(ret);
}

BIF_RETTYPE
re_compile_2(BIF_ALIST_2)
{
    return re_compile(BIF_P, BIF_ARG_1, BIF_ARG_2, false);
}

BIF_RETTYPE
re_compile_1(BIF_ALIST_1)
{
    return re_compile(BIF_P, BIF_ARG_1, NIL, false);
}

/*
 * Restart contexts for the re:run bif
 */

/*
 * When erts_pcre_exec is restarted, only the actual extra-structure with
 * it's restart-data need to be kept. The match is then called with
 * watever is saved. The code is pointed out by this and cannot be
 * reallocated or GC'ed, why it's passed along as a off-heap-binary,
 * but not actually passed in the erts_pcre_exec restart calls.
 */

typedef enum { RetIndex, RetString, RetBin, RetNone } ReturnType;

typedef struct _return_info {
    ReturnType type;
    int num_spec; /* 0 == all, -1 == all_but first, > 0 specified in vector */
    int v[1];
} ReturnInfo;


#define RESTART_FLAG_SUBJECT_IN_BINARY 0x1
#define RESTART_FLAG_REPORT_MATCH_LIMIT 0x2

typedef struct _restart_context {
    void *restart_data;
    Uint32 flags;
    PCRE2_UCHAR8* subject; /* to be able to free it when done */
    pcre2_code *code_to_free;    // temp compiled regex, NULL if precompiled
    pcre2_match_data *match_data;
    pcre2_match_context *match_ctx;
    PCRE2_SIZE *ovector; /* Keep until done */
    ReturnInfo *ret_info;
} RestartContext;

#define RESTART_FLAG_SUBJECT_IN_BINARY 0x1
#define RESTART_FLAG_REPORT_MATCH_LIMIT 0x2

static void cleanup_restart_context(RestartContext *rc) 
{
    if (rc->restart_data != NULL) {
	pcre2_free_restart_data(rc->match_data);
	rc->restart_data = NULL;
    }
    if (rc->match_data != NULL) {
        pcre2_match_data_free(rc->match_data);
        rc->match_data = NULL;
        rc->ovector = NULL;
    }
    if (rc->match_ctx != NULL) {
        pcre2_match_context_free(rc->match_ctx);
        rc->match_ctx = NULL;
    }
    if (rc->subject != NULL && !(rc->flags & RESTART_FLAG_SUBJECT_IN_BINARY)) {
	erts_free(ERTS_ALC_T_RE_SUBJECT, rc->subject);    
    }
    rc->subject = NULL;
    if (rc->code_to_free != NULL) {
        pcre2_code_free(rc->code_to_free);
        rc->code_to_free = NULL;
    }
    if (rc->ret_info != NULL) {
	erts_free(ERTS_ALC_T_RE_SUBJECT, rc->ret_info);
	rc->ret_info = NULL;
    }
}

static int cleanup_restart_context_bin(Binary *bp)
{
    RestartContext *rc = ERTS_MAGIC_BIN_DATA(bp);
    cleanup_restart_context(rc);
    return 1;
}

/*
 * Build the return value for Erlang from result and restart context
 */

static Eterm build_exec_return(Process *p, int rc, RestartContext *restartp, Eterm orig_subject) 
{
    Eterm res;
    Eterm *hp;
    if (rc <= 0) {
	if (restartp->flags & RESTART_FLAG_REPORT_MATCH_LIMIT) {
	    if (rc == PCRE2_ERROR_MATCHLIMIT) {
		hp = HAlloc(p,3);
		res = TUPLE2(hp,am_error,am_match_limit);
	    } else if (rc == PCRE2_ERROR_RECURSIONLIMIT) {
		hp = HAlloc(p,3);
		res = TUPLE2(hp,am_error,am_match_limit_recursion);
	    } else {
		res = am_nomatch;
	    }
	} else {
	    res = am_nomatch;
	}
    } else {
	ReturnInfo *ri;
	ReturnInfo defri;

	if (restartp->ret_info == NULL) {
            /* OpenBSD 5.8 gcc compiler for some reason creates
               bad code if the above initialization is done
               inline with the struct. So don't do that. */
            defri.type = RetIndex;
            defri.num_spec = 0;
            defri.v[0] = 0;
	    ri = &defri;
	} else {
	    ri = restartp->ret_info;
	}

	if (ri->type == RetNone) {
	    res = am_match;
	} else if (ri->type == RetIndex){
	    Eterm *tmp_vect;
	    Eterm tpl;
	    int i;
	    if (ri->num_spec <= 0) {
		tmp_vect = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, 
				      rc * 2 * sizeof(Eterm));
		for(i = -(ri->num_spec) ;i < rc; ++i) {
                    if (restartp->ovector[i*2] == PCRE2_UNSET) {
                        tmp_vect[i*2] = make_small(-1);
                        tmp_vect[i*2+1] = make_small(0);
                    } else {
                        tmp_vect[i*2] = make_signed_integer(restartp->ovector[i * 2], p);
                        tmp_vect[i*2+1] = make_signed_integer(restartp->ovector[i*2+1]
                                                              - restartp->ovector[i*2],p);
                    }
		}
		hp = HAlloc(p, 3+(3+2)*(rc + ri->num_spec));
		res = NIL;
		for(i = rc-1 ;i >= -(ri->num_spec); --i) {
		    tpl = TUPLE2(hp,tmp_vect[i*2],tmp_vect[i*2+1]);
		    hp += 3;
		    res = CONS(hp,tpl,res);
		    hp += 2;
		}
	    } else {
		int n = 0;
		int x;
		tmp_vect = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, 
				      ri->num_spec * 2 * sizeof(Eterm));
		for (i = 0; i < ri->num_spec; ++i) {
		    x = ri->v[i];
		    if (x < -1) {
			int n = i-x+1;
			int j;
			for (j = i+1; j < ri->num_spec && j < n; ++j) {
			    if (restartp->ovector[(ri->v[j])*2] != PCRE2_UNSET) {
				x = ri->v[j];
				break;
			    }
			}
			i = n-1;
		    }
		    if (x < rc && x >= 0) {
			tmp_vect[n*2] = make_signed_integer(restartp->ovector[x*2],p);
			tmp_vect[n*2+1] = make_signed_integer(restartp->ovector[x*2+1]-restartp->ovector[x*2],p);
		    } else {
			tmp_vect[n*2] = make_small(-1);
			tmp_vect[n*2+1] = make_small(0);
		    }
		    ++n;
		}
		hp = HAlloc(p, 3+(3+2)*n);
		res = NIL;
		for(i = n-1 ;i >= 0; --i) {
		    tpl = TUPLE2(hp,tmp_vect[i*2],tmp_vect[i*2+1]);
		    hp += 3;
		    res = CONS(hp,tpl,res);
		    hp += 2;
		}
	    }
	    res = TUPLE2(hp,am_match,res);
	    erts_free(ERTS_ALC_T_RE_TMP_BUF, tmp_vect);
	} else {
	    Eterm *tmp_vect;
	    int i;
	    if (ri->num_spec <= 0) {
		tmp_vect = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, 
				      rc * sizeof(Eterm));
		for(i = -(ri->num_spec) ;i < rc; ++i) { /* XXX: Unicode */
                    PCRE2_UCHAR8* cp;
		    int len;
		    if (restartp->ovector[i*2] == PCRE2_UNSET) {
			cp = restartp->subject;
			len = 0;
		    } else {
			cp = restartp->subject + restartp->ovector[i*2];
			len = restartp->ovector[i*2+1] - restartp->ovector[i*2];
		    }
                if (ri->type == RetBin) { 
                        if (restartp->flags & RESTART_FLAG_SUBJECT_IN_BINARY) {
                            /* Optimized - if subject was binary to begin with,
                             * we can make sub-binaries. */
                            tmp_vect[i] =
                                erts_make_sub_binary(p,
                                                     orig_subject,
                                                     cp - restartp->subject,
                                                     len);
                        } else {
                            tmp_vect[i] =
                                erts_new_binary_from_data(p, len, (byte*)cp);
                        }
		    } else {
			Eterm *hp2;
			hp2 = HAlloc(p,(2*len));
			tmp_vect[i] = buf_to_intlist(&hp2, (char*)cp, len, NIL);
		    } 
		}
		hp = HAlloc(p, 3+2*(rc + ri->num_spec));
		res = NIL;
		for(i = rc-1 ;i >= -(ri->num_spec); --i) {
		    res = CONS(hp,tmp_vect[i],res);
		    hp += 2;
		}
	    } else {
		int n = 0;
		int x;
		tmp_vect = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, 
				      ri->num_spec * sizeof(Eterm));
		for (i = 0; i < ri->num_spec; ++i) {
		    x = ri->v[i];
		    if (x < -1) {
			int n = i-x+1;
			int j;
			for (j = i+1; j < ri->num_spec && j < n; ++j) {
			    if (restartp->ovector[(ri->v[j])*2] != PCRE2_UNSET) {
				x = ri->v[j];
				break;
			    }
			}
			i = n-1;
		    }
		    if (x < rc && x >= 0) {
			PCRE2_UCHAR *cp;
			int len;
			if (restartp->ovector[x*2] == PCRE2_UNSET) {
			    cp = restartp->subject;
			    len = 0;
			} else {
			    cp = restartp->subject + restartp->ovector[x*2];
			    len = restartp->ovector[x*2+1] - restartp->ovector[x*2];
			}
                        if (ri->type == RetBin) {
                            if (restartp->flags & RESTART_FLAG_SUBJECT_IN_BINARY) {
                                /* Optimized - if subject was binary to begin
                                 * with, we can make sub-binaries. */
                                tmp_vect[n] =
                                    erts_make_sub_binary(p,
                                                         orig_subject,
                                                         cp - restartp->subject,
                                                         len);
                            } else {
                                tmp_vect[n] =
                                    erts_new_binary_from_data(p, len, (byte*)cp);
                            }
			} else {
			    Eterm *hp2;
			    hp2 = HAlloc(p,(2*len));
			    tmp_vect[n] = buf_to_intlist(&hp2, (char*)cp, len, NIL);
			} 
		    } else {
                        if (ri->type == RetBin) {
                            tmp_vect[n] =
                                erts_new_binary_from_data(p, 0, (byte*)"");
			} else {
			    tmp_vect[n] = NIL;
			} 
		    }	
		    ++n;
		}
		hp = HAlloc(p, 3+2*n);
		res = NIL;
		for(i = n-1 ;i >= 0; --i) {
		    res = CONS(hp,tmp_vect[i],res);
		    hp += 2;
		}
		
	    }	    
	    res = TUPLE2(hp,am_match,res);
	    erts_free(ERTS_ALC_T_RE_TMP_BUF, tmp_vect);
	}
    }
    return res;
}

/*
 * Extra parsing function, build the ReturnInfo structure from
 * a capture specification in the option list
 */

#define RINFO_SIZ(Num) (sizeof(ReturnInfo) + (sizeof(int) * (Num - 1)))
#define PICK_INDEX(NameEntry)					        \
    ((int) ((((unsigned) ((unsigned char *) (NameEntry))[0]) << 8) +	\
	    ((unsigned) ((unsigned char *) (NameEntry))[1])))


static void build_one_capture(pcre2_code  *code, ReturnInfo **ri, int *sallocated, int has_dupnames, char *name)
{
    ReturnInfo *r = (*ri);
    if (has_dupnames) {
	/* Build a sequence of positions, starting with -size if
	   more than one, otherwise just put the index there... */
	const PCRE2_UCHAR8 *first,*last;
	int esize = pcre2_substring_nametable_scan(code, (PCRE2_UCHAR8*)name,
                                                   &first, &last);
	if (esize == PCRE2_ERROR_NOSUBSTRING) {
	    r->v[r->num_spec - 1] = -1;
	} else if(last == first) {
	    r->v[r->num_spec - 1] = PICK_INDEX(first);
	} else {
	    int num = ((last - first) / esize) + 1;
	    int i;
	    ASSERT(num > 1);
	    r->v[r->num_spec - 1] = -num; /* A value less than -1 means
					       multiple indexes for same name */
	    for (i = 0; i < num; ++i) {
		++(r->num_spec);
		if(r->num_spec > (*sallocated)) {
		    (*sallocated) += 10;
		    r = erts_realloc(ERTS_ALC_T_RE_SUBJECT, r, 
				      RINFO_SIZ((*sallocated)));
		}
		r->v[r->num_spec - 1] = PICK_INDEX(first);
		first += esize;
	    }
	}
    } else {
	/* Use the faster binary search if no duplicate names are present */  
        r->v[r->num_spec - 1] = pcre2_substring_number_from_name(code, (PCRE2_UCHAR8*)name);
	if (r->v[r->num_spec - 1] == PCRE2_ERROR_NOSUBSTRING) {
	    r->v[r->num_spec - 1] = -1;
	}
    }
    *ri = r;
}    

static ReturnInfo *
build_capture(Eterm capture_spec[CAPSPEC_SIZE], pcre2_code  *code)
{
    ReturnInfo *ri = erts_alloc(ERTS_ALC_T_RE_SUBJECT, RINFO_SIZ(0));
    int sallocated = 0;
    char *tmpb = NULL;
    int tmpbsiz = 0;
    Eterm l;

    ri->type = RetIndex;
    ri->num_spec = 0;


    switch(capture_spec[CAPSPEC_TYPE]) {
    case am_index:
	ri->type = RetIndex;
	break;
    case am_list:
	ri->type = RetString;
	break;
    case am_binary:
	ri->type = RetBin;
	break;
    default:
	goto error;
    }

    switch(capture_spec[CAPSPEC_VALUES]) {
    case am_all:
	ri->num_spec = 0;
	break;
    case am_none:
    case NIL:
	ri->num_spec = 0;
	ri->type = RetNone;
	break;
    case am_all_but_first:
	ri->num_spec = -1;
	break;
    case am_first:
	ri->num_spec = 1;
	if(ri->num_spec > sallocated) {
	    sallocated = ri->num_spec;
	    ri = erts_realloc(ERTS_ALC_T_RE_SUBJECT, ri, RINFO_SIZ(sallocated));
	}
	ri->v[ri->num_spec - 1] = 0;
	break;
    case am_all_names:
	{
	    int rc,i;
	    uint32_t top;
	    uint32_t entrysize;
	    unsigned char *nametable, *last = NULL;
	    int has_dupnames;
	    uint32_t options;

	    if (pcre2_pattern_info(code, PCRE2_INFO_ALLOPTIONS, &options) != 0)
		goto error;
	    if ((rc = pcre2_pattern_info(code, PCRE2_INFO_NAMECOUNT, &top)) != 0)
		goto error;
	    if (top <= 0) {
		ri->num_spec = 0;
		ri->type = RetNone;
		break;
	    }
	    if (pcre2_pattern_info(code, PCRE2_INFO_NAMEENTRYSIZE, &entrysize) != 0)
		goto error;
	    if (pcre2_pattern_info(code, PCRE2_INFO_NAMETABLE, &nametable) != 0)
		goto error;
	    
	    has_dupnames = ((options & PCRE2_DUPNAMES) != 0);

	    for(i=0;i<top;++i) {
		if (last == NULL || !has_dupnames || sys_strcmp((char *) last+2,(char *) nametable+2)) {
		    ASSERT(ri->num_spec >= 0);
		    ++(ri->num_spec);
		    if(ri->num_spec > sallocated) {
			sallocated += 10;
			ri = erts_realloc(ERTS_ALC_T_RE_SUBJECT, ri, RINFO_SIZ(sallocated));
		    }
		    if (has_dupnames) {
			/* This could be more effective, we actually have 
			   the names and could fill in the vector
			   immediately. Now we lookup the name again. */
			build_one_capture(code,&ri,&sallocated,has_dupnames,(char *) nametable+2);
		    } else {
			ri->v[ri->num_spec - 1] = PICK_INDEX(nametable);	
		    }
		}
		last = nametable;
		nametable += entrysize;
	    }
	    break;
	}
    default:
	if (is_list(capture_spec[CAPSPEC_VALUES])) {
	    for(l=capture_spec[CAPSPEC_VALUES];is_list(l);l = CDR(list_val(l))) {
		int x;
		Eterm val = CAR(list_val(l));
		ASSERT(ri->num_spec >= 0);
		++(ri->num_spec);
		if(ri->num_spec > sallocated) {
		    sallocated += 10;
		    ri = erts_realloc(ERTS_ALC_T_RE_SUBJECT, ri, RINFO_SIZ(sallocated));
		}
		if (term_to_int(val,&x)) {
		    ri->v[ri->num_spec - 1] = x;
		} else if (is_atom(val) || is_bitstring(val) || is_list(val)) {
		    int has_dupnames;
		    uint32_t options;
		    if (pcre2_pattern_info(code, PCRE2_INFO_ALLOPTIONS, &options) != 0)
			goto error;
		    has_dupnames = ((options & PCRE2_DUPNAMES) != 0);
		    if (is_atom(val)) {
			Atom *ap = atom_tab(atom_val(val));
			if ((ap->len + 1) > tmpbsiz) {
			    if (!tmpbsiz) {
				tmpb = erts_alloc(ERTS_ALC_T_RE_TMP_BUF,(tmpbsiz = ap->len + 1));
			    } else {
				tmpb = erts_realloc(ERTS_ALC_T_RE_TMP_BUF,tmpb,
						    (tmpbsiz = ap->len + 1));
			    }
			}
                        ASSERT(tmpb != NULL);
			sys_memcpy(tmpb,erts_atom_get_name(ap),ap->len);
			tmpb[ap->len] = '\0';
		    } else {
			ErlDrvSizeT slen;
			int buffres;

			if (erts_iolist_size(val, &slen)) {
			    goto error;
			}
			if ((slen + 1) > tmpbsiz) {
			    if (!tmpbsiz) {
				tmpb = erts_alloc(ERTS_ALC_T_RE_TMP_BUF,(tmpbsiz = slen + 1));
			    } else {
				tmpb = erts_realloc(ERTS_ALC_T_RE_TMP_BUF,tmpb,
						    (tmpbsiz = slen + 1));
			    }
			}
                        ASSERT(tmpb != NULL);
			buffres = erts_iolist_to_buf(val, tmpb, slen);
			ASSERT(buffres >= 0); (void)buffres;
			tmpb[slen] = '\0';
		    }
		    build_one_capture(code,&ri,&sallocated,has_dupnames,tmpb);
		} else {
		    goto error;
		}
	    }
	    if (l != NIL) {
		goto error;
	    }
	} else {
	    goto error;
	}
	break;
    }
    
    if(tmpb != NULL) {
	erts_free(ERTS_ALC_T_RE_TMP_BUF,tmpb);
    }
    return ri;
 error:
    if(tmpb != NULL) {
	erts_free(ERTS_ALC_T_RE_TMP_BUF,tmpb);
    }
    erts_free(ERTS_ALC_T_RE_SUBJECT, ri);
    return NULL;
}    


/*
 * The actual re:run/2,3 BIFs
 */
static BIF_RETTYPE
re_run(Process *p, Eterm arg1, Eterm arg2, Eterm arg3, bool first)
{
    RestartContext restart;
    pcre2_code  *regex_code;
    ErlDrvSizeT slength;
    int ovsize;
    Eterm *tp;
    int rc;
    Eterm res;
    Sint32 loop_limit;
    int is_list_cap;
    struct parsed_options opts;
    const Sint32 reds_initial = ERTS_BIF_REDS_LEFT(p);
    Sint32 reds_consumed;

    if (!parse_options(arg3, &opts) || (opts.flags & PARSE_FLAG_EXPORT)) {
        p->fvalue = am_badopt;
	BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
    }
    if (!first) {
        /*
         * 'first' is false when re:grun() previously has called re:internal_run()
         * with the same subject; i.e., no need to do yet another validation of
         * the subject regarding utf8 encoding...
         */
        opts.match |= PCRE2_NO_UTF_CHECK;
    }
    is_list_cap = ((opts.flags & PARSE_FLAG_CAPTURE_OPT) &&
		   (opts.capture[CAPSPEC_TYPE] == am_list));

    if (!is_tuple_arity(arg2, 5)) {
        if (!is_bitstring(arg2) && !is_list(arg2) && !is_nil(arg2)) {
            BIF_ERROR(p,BADARG);
        }
        else {
	    /* Compile from textual regex */
	    ErlDrvSizeT slen;
	    byte *expr;
            byte *tmp_expr = NULL;

	    int errcode = 0;
	    PCRE2_SIZE errofset = 0;
	    uint32_t capture_count;

	    if (opts.flags & PARSE_FLAG_UNICODE &&
		(!is_bitstring(arg2) || !is_bitstring(arg1) ||
		 (is_list_cap && !(opts.flags & PARSE_FLAG_GLOBAL)))) {
		BIF_TRAP3(urun_trap_exportp, p, arg1, arg2, arg3);
	    }
	    
            if (!get_iolist_as_bytes(p, arg2, &expr, &slen, &tmp_expr, NULL)) {
                BIF_ERROR(p,BADARG);
            }

            regex_code = compile((char*)expr, slen, &opts, the_tmp_compile_ctx,
                                 &errcode, &errofset);
            if (tmp_expr) {
                erts_free(ERTS_ALC_T_RE_TMP_BUF, tmp_expr);
            }

            if (!regex_code) {
		/* Compilation error gives badarg except in the compile 
		   function or if we have PARSE_FLAG_REPORT_ERRORS */
		if (opts.flags &  PARSE_FLAG_REPORT_ERRORS) {
		    res = build_compile_error(p, errcode,
					       errofset, am_compile);
		    BIF_RET(res);
		} else {
		    BIF_ERROR(p,BADARG);
		}
	    }
	    if (opts.flags & PARSE_FLAG_GLOBAL) {
		Eterm precompiled = 
		    build_compile_result(p,
					 regex_code,
					 (opts.flags &
					  PARSE_FLAG_UNICODE) ? 1 : 0,
					 false);
		Eterm *hp,r;
		hp = HAlloc(p,4);
		/* arg2 is in the tuple just to make exceptions right */
		r = TUPLE3(hp,arg3,
			   ((opts.flags & PARSE_FLAG_UNIQUE_COMPILE_OPT) ?
			    am_true : 
			    am_false), arg2);
		BIF_TRAP3(grun_trap_exportp, p, arg1, precompiled, r);
	    }

	    pcre2_pattern_info(regex_code, PCRE2_INFO_CAPTURECOUNT, &capture_count);
	    ovsize = capture_count + 1;
	    restart.code_to_free = regex_code;
        }
    } else {
        /* Precompiled regex */

	if (opts.flags & PARSE_FLAG_UNIQUE_COMPILE_OPT) {
	    BIF_ERROR(p,BADARG);
	}

	tp = tuple_val(arg2);
	if (tp[1] != am_re_pattern || is_not_small(tp[2]) || 
	    is_not_small(tp[3]) || is_not_small(tp[4]) || 
            !magic_ref_to_pcre2_code(tp[5], &regex_code)) {
	    BIF_ERROR(p,BADARG);
	}

	if (unsigned_val(tp[3]) && 
	    (!is_bitstring(arg1) ||
	     (is_list_cap && !(opts.flags & PARSE_FLAG_GLOBAL)))) { /* unicode */
	    BIF_TRAP3(urun_trap_exportp, p, arg1, arg2,
		      arg3);
	}

	if (opts.flags & PARSE_FLAG_GLOBAL) {
	    Eterm *hp,r;
	    hp = HAlloc(p,3);
	    r = TUPLE2(hp,arg3,am_false);
	    BIF_TRAP3(grun_trap_exportp, p, arg1, arg2,
		      r);
	}

	ovsize = unsigned_val(tp[2]) + 1;

	if (opts.newline | opts.bsr) {
            /*
	     * Old PCRE did support newline and bsr options at both "compile"
	     * and "match". PCRE2 do only support them to "compile" function.
             * To be nice we only fail with badarg if (old) user passes
             * different newline or bsr option to re:run vs re:compile.
             */
	    if (opts.newline) {
		uint32_t newline_compiled;
		if (pcre2_pattern_info(regex_code, PCRE2_INFO_NEWLINE,
				       &newline_compiled) != 0
		    || newline_compiled != opts.newline) {
		    BIF_ERROR(p, BADARG);
		}
	    }
	    if (opts.bsr) {
		uint32_t bsr_compiled;
		if (pcre2_pattern_info(regex_code, PCRE2_INFO_BSR,
				       &bsr_compiled) != 0
		    || bsr_compiled != opts.bsr) {
		    BIF_ERROR(p, BADARG);
		}
	    }
        }
        restart.code_to_free = NULL;
    }

    restart.match_data = pcre2_match_data_create(ovsize, the_general_ctx);
    restart.ovector = pcre2_get_ovector_pointer(restart.match_data);

    loop_limit = MIN(reds_initial * LOOP_FACTOR, max_loop_limit);
    pcre2_set_loops_left(restart.match_data, loop_limit);
    restart.restart_data = NULL;
    pcre2_set_restart_data(restart.match_data, &restart.restart_data);
    pcre2_set_restart_flags(restart.match_data, 0);

    restart.ret_info = NULL;
    if (opts.flags & PARSE_FLAG_CAPTURE_OPT) {
	if ((restart.ret_info = build_capture(opts.capture, regex_code)) == NULL) {
            pcre2_match_data_free(restart.match_data);
            pcre2_code_free_8(restart.code_to_free);
	    BIF_ERROR(p,BADARG);
	}
    }

    if (opts.flags & (PARSE_FLAG_MATCH_LIMIT | PARSE_FLAG_MATCH_LIMIT_RECURSION)) {
        restart.match_ctx = pcre2_match_context_create(the_general_ctx);
        if (opts.flags & PARSE_FLAG_MATCH_LIMIT) {
            pcre2_set_match_limit(restart.match_ctx, opts.match_limit);
        }
        if (opts.flags & PARSE_FLAG_MATCH_LIMIT_RECURSION) {
            pcre2_set_depth_limit(restart.match_ctx, opts.match_limit_recursion);
        }
    }
    else {
        restart.match_ctx = NULL;
    }

    /* Optimized - if already in binary off heap, keep that and avoid copying,
     * also binary returns can be sub binaries in that case. */
    restart.flags = 0;
    if (is_bitstring(arg1)) {
        ERTS_DECLARE_DUMMY(Eterm br_flags);
        ERTS_DECLARE_DUMMY(BinRef *br);
        Uint offset, size;
        byte *base;

        ERTS_PIN_BITSTRING(arg1, br_flags, br, base, offset, size);

        /* If this is an unaligned or on-heap binary, we'll make a copy of it
         * instead. */
        if (BIT_OFFSET(offset) != 0 || TAIL_BITS(size) != 0 ||
            size <= ERL_ONHEAP_BITS_LIMIT) {
            goto handle_iodata;
        }

        restart.flags |= RESTART_FLAG_SUBJECT_IN_BINARY;
        restart.subject = &base[BYTE_OFFSET(offset)];
        slength = BYTE_SIZE(size);
    } else {
	int buffres;
handle_iodata:
	if (erts_iolist_size(arg1, &slength)) {
            pcre2_match_data_free(restart.match_data);
            if (restart.match_ctx) {
                pcre2_match_context_free(restart.match_ctx);
            }
            pcre2_code_free_8(restart.code_to_free);
	    if (restart.ret_info != NULL) {
		erts_free(ERTS_ALC_T_RE_SUBJECT, restart.ret_info);
	    }
	    BIF_ERROR(p,BADARG);
	}
	restart.subject = erts_alloc(ERTS_ALC_T_RE_SUBJECT, slength);

	buffres = erts_iolist_to_buf(arg1, (char*)restart.subject, slength);
	ASSERT(buffres >= 0); (void)buffres;
    }

    if (opts.flags & PARSE_FLAG_REPORT_ERRORS) {
	restart.flags |= RESTART_FLAG_REPORT_MATCH_LIMIT;
    }

    rc = pcre2_match_8(regex_code, restart.subject,
                       slength, opts.startoffset,
                       opts.match,
                       restart.match_data,
                       restart.match_ctx);

    reds_consumed = (loop_limit - pcre2_get_loops_left(restart.match_data)) / LOOP_FACTOR;

    if (rc < 0) {
        switch (rc) {
            /* No match... */
        case PCRE2_ERROR_NOMATCH:
        case PCRE2_ERROR_MATCHLIMIT:
        case PCRE2_ERROR_DEPTHLIMIT:
        case PCRE2_ERROR_HEAPLIMIT:
            break;

            /* Yield... */
        case PCRE2_ERROR_LOOP_LIMIT: {
            /* Trap */
            Binary *mbp = erts_create_magic_binary(sizeof(RestartContext),
                                                   cleanup_restart_context_bin);
            RestartContext *restartp = ERTS_MAGIC_BIN_DATA(mbp);
            Eterm magic_ref;
            Eterm *hp;
            BUMP_REDS(p, reds_consumed);
            sys_memcpy(restartp,&restart,sizeof(RestartContext));
            pcre2_set_restart_data(restart.match_data, &restartp->restart_data);
            ERTS_VBUMP_ALL_REDS(p);
            hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
            magic_ref = erts_mk_magic_ref(&hp, &MSO(p), mbp);
            BIF_TRAP3(&re_match_trap_export,
                      p,
                      arg1,
                      arg2 /* To avoid GC of precompiled code, XXX: not utilized yet */,
                      magic_ref);
        }

            /* Recursive loop detected in pattern... */
        case PCRE2_ERROR_RECURSELOOP:
            reds_consumed = CONTEXT_REDS; /* Unknown amount of work done... */
            break; /* nomatch for backwards compatibility reasons for now... */
            
            /* Bad utf8 in subject... */
        case PCRE2_ERROR_BADUTFOFFSET:
        case PCRE2_ERROR_UTF8_ERR1:
        case PCRE2_ERROR_UTF8_ERR2:
        case PCRE2_ERROR_UTF8_ERR3:
        case PCRE2_ERROR_UTF8_ERR4:
        case PCRE2_ERROR_UTF8_ERR5:
        case PCRE2_ERROR_UTF8_ERR6:
        case PCRE2_ERROR_UTF8_ERR7:
        case PCRE2_ERROR_UTF8_ERR8:
        case PCRE2_ERROR_UTF8_ERR9:
        case PCRE2_ERROR_UTF8_ERR10:
        case PCRE2_ERROR_UTF8_ERR11:
        case PCRE2_ERROR_UTF8_ERR12:
        case PCRE2_ERROR_UTF8_ERR13:
        case PCRE2_ERROR_UTF8_ERR14:
        case PCRE2_ERROR_UTF8_ERR15:
        case PCRE2_ERROR_UTF8_ERR16:
        case PCRE2_ERROR_UTF8_ERR17:
        case PCRE2_ERROR_UTF8_ERR18:
        case PCRE2_ERROR_UTF8_ERR19:
        case PCRE2_ERROR_UTF8_ERR20:
        case PCRE2_ERROR_UTF8_ERR21:
            BUMP_ALL_REDS(p); /* Unknown amount of work done... */
            ERTS_FALLTHROUGH();

        case PCRE2_ERROR_BADOFFSET:
            /* Bad pre-compiled regexp... */
        case PCRE2_ERROR_BADMAGIC:
        case PCRE2_ERROR_BADMODE:
            cleanup_restart_context(&restart);
            BIF_ERROR(p, BADARG);

        default:
            /* Something unexpected happened... */
            ASSERT(! "Unexpected pcre2_match() result");
            cleanup_restart_context(&restart);
            BIF_ERROR(p, EXC_INTERNAL_ERROR);
        }
    }
    
    BUMP_REDS(p, reds_consumed);

    res = build_exec_return(p, rc, &restart, arg1);
 
    cleanup_restart_context(&restart);

    BIF_RET(res);
}

BIF_RETTYPE
re_internal_run_4(BIF_ALIST_4)
{
    bool first;
    if (BIF_ARG_4 == am_false)
        first = false;
    else if (BIF_ARG_4 == am_true)
        first = true;
    else
        BIF_ERROR(BIF_P,BADARG);
    return re_run(BIF_P,BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, first);
}

BIF_RETTYPE
re_run_3(BIF_ALIST_3)
{
    return re_run(BIF_P,BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, true);
}

BIF_RETTYPE
re_run_2(BIF_ALIST_2) 
{
    return re_run(BIF_P,BIF_ARG_1, BIF_ARG_2, NIL, true);
}

/*
 * The "magic" trap target, continue a re:run
 */

static BIF_RETTYPE re_match_trap(BIF_ALIST_3)
     /* XXX: Optimize - arg 1 and 2 to be utilized for keeping binary 
	code and subject */
{
    Binary *mbp;
    RestartContext *restartp;
    int rc;
    Sint32 loop_limit;
    Eterm res;
    const Sint32 reds_initial = ERTS_BIF_REDS_LEFT(BIF_P);
    Sint32 reds_consumed;

    mbp = erts_magic_ref2bin(BIF_ARG_3);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp)
	   == cleanup_restart_context_bin);

    restartp = (RestartContext *) ERTS_MAGIC_BIN_DATA(mbp);
    loop_limit = MIN(reds_initial * LOOP_FACTOR, max_loop_limit);
    pcre2_set_loops_left(restartp->match_data, loop_limit);
    pcre2_set_restart_flags(restartp->match_data,  0);
    
    rc = pcre2_match(NULL, NULL, 0, 0, 0, restartp->match_data, NULL);

    reds_consumed = (loop_limit - pcre2_get_loops_left(restartp->match_data)) / LOOP_FACTOR;

    if (rc < 0) {
        switch (rc) {
            /* No match... */
        case PCRE2_ERROR_NOMATCH:
        case PCRE2_ERROR_MATCHLIMIT:
        case PCRE2_ERROR_RECURSIONLIMIT:
            break;
        case PCRE2_ERROR_LOOP_LIMIT:
            /* Trap */
            BUMP_REDS(BIF_P, reds_consumed);
            ERTS_VBUMP_ALL_REDS(BIF_P);
            BIF_TRAP3(&re_match_trap_export, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
            /* Bad utf8 in subject... */
        case PCRE2_ERROR_BADUTFOFFSET:
        case PCRE2_ERROR_UTF8_ERR1:
        case PCRE2_ERROR_UTF8_ERR2:
        case PCRE2_ERROR_UTF8_ERR3:
        case PCRE2_ERROR_UTF8_ERR4:
        case PCRE2_ERROR_UTF8_ERR5:
        case PCRE2_ERROR_UTF8_ERR6:
        case PCRE2_ERROR_UTF8_ERR7:
        case PCRE2_ERROR_UTF8_ERR8:
        case PCRE2_ERROR_UTF8_ERR9:
        case PCRE2_ERROR_UTF8_ERR10:
        case PCRE2_ERROR_UTF8_ERR11:
        case PCRE2_ERROR_UTF8_ERR12:
        case PCRE2_ERROR_UTF8_ERR13:
        case PCRE2_ERROR_UTF8_ERR14:
        case PCRE2_ERROR_UTF8_ERR15:
        case PCRE2_ERROR_UTF8_ERR16:
        case PCRE2_ERROR_UTF8_ERR17:
        case PCRE2_ERROR_UTF8_ERR18:
        case PCRE2_ERROR_UTF8_ERR19:
        case PCRE2_ERROR_UTF8_ERR20:
        case PCRE2_ERROR_UTF8_ERR21:
            BUMP_ALL_REDS(BIF_P); /* Unknown amount of work done... */
            ERTS_FALLTHROUGH();

        case PCRE2_ERROR_BADOFFSET:
        case PCRE2_ERROR_BADMAGIC:
        case PCRE2_ERROR_BADMODE:
            cleanup_restart_context(restartp);
            BIF_ERROR(BIF_P, BADARG);
        default:
            /* Something unexpected happened... */
            ASSERT(! "Unexpected pcre2_match() result");
            cleanup_restart_context(restartp);
            BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
        }
    }
    BUMP_REDS(BIF_P, reds_consumed);

    res = build_exec_return(BIF_P, rc, restartp, BIF_ARG_1);
 
    cleanup_restart_context(restartp);

    BIF_RET(res);
}
    
BIF_RETTYPE
re_inspect_2(BIF_ALIST_2) 
{
    const byte *temp_alloc = NULL;
    Eterm *tp,*tmp_vec,*hp;
    int i,j;
    uint32_t top;
    uint32_t entrysize;
    unsigned char *nametable, *last,*name;
    int has_dupnames;
    uint32_t options;
    int num_names;
    Eterm res;
    pcre2_code  *code;
    int infores;

    if (is_not_tuple(BIF_ARG_1) || (arityval(*tuple_val(BIF_ARG_1)) != 5)) {
	goto error;
    }
    tp = tuple_val(BIF_ARG_1);
    if (tp[1] != am_re_pattern || is_not_small(tp[2])
        || is_not_small(tp[3]) || is_not_small(tp[4])
        || !magic_ref_to_pcre2_code(tp[5], &code)) {
        goto error;
    }
    if (BIF_ARG_2 != am_namelist) {
        goto error;
    }

    /* OK, so let's try to get some info */
    
    if (pcre2_pattern_info(code, PCRE2_INFO_ALLOPTIONS, &options) != 0)
	goto error;

    infores = pcre2_pattern_info(code, PCRE2_INFO_NAMECOUNT, &top);
    ASSERT(infores == 0); (void)infores;

    if (top <= 0) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp,am_namelist,NIL);
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_RET(res);
    }
    infores = pcre2_pattern_info(code, PCRE2_INFO_NAMEENTRYSIZE, &entrysize);
    ASSERT(infores == 0);

    infores = pcre2_pattern_info(code, PCRE2_INFO_NAMETABLE, &nametable);
    ASSERT(infores == 0);
    
    has_dupnames = ((options & PCRE2_DUPNAMES) != 0);
    /* First, count the names */
    num_names = 0;
    last = NULL;
    name = nametable;
    for(i=0;i<top;++i) {
	if (last == NULL || !has_dupnames || sys_strcmp((char *) last+2,
						    (char *) name+2)) {
	    ++num_names;
	}
	last = name;
	name += entrysize;
    }
    tmp_vec =  erts_alloc(ERTS_ALC_T_RE_TMP_BUF, 
			  num_names * sizeof(Eterm));
    /* Re-iterate and fill tmp_vec */
    last = NULL;
    name = nametable;
    j = 0;
    for(i=0;i<top;++i) {
        if (last == NULL ||
            !has_dupnames ||
            sys_strcmp((char *)&last[2], (char*)&name[2])) {
            Uint len = sys_strlen((char*)&name[2]);
            tmp_vec[j++] = erts_new_binary_from_data(BIF_P, len, (byte*)&name[2]);
	}
	last = name;
	name += entrysize;
    }
    ASSERT(j == num_names);
    hp = HAlloc(BIF_P, 3+2*j);
    res = NIL;
    for(i = j-1 ;i >= 0; --i) {
	res = CONS(hp,tmp_vec[i],res);
	hp += 2;
    }
    res = TUPLE2(hp,am_namelist,res);
    erts_free_aligned_binary_bytes(temp_alloc);
    erts_free(ERTS_ALC_T_RE_TMP_BUF, tmp_vec);
    BIF_RET(res);

 error:
    /* tmp_vec never allocated when we reach here */
    erts_free_aligned_binary_bytes(temp_alloc);
    BIF_ERROR(BIF_P,BADARG);
}
    

	
    

	
