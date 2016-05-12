/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
#include "big.h"
#define ERLANG_INTEGRATION 1
#define PCRE_STATIC
#include "pcre.h"

#define PCRE_DEFAULT_COMPILE_OPTS 0 
#define PCRE_DEFAULT_EXEC_OPTS 0 
#define LOOP_FACTOR 10


static const unsigned char *default_table;
static Uint max_loop_limit;
static Export re_exec_trap_export;
static Export *grun_trap_exportp = NULL;
static Export *urun_trap_exportp = NULL;
static Export *ucompile_trap_exportp = NULL;

static BIF_RETTYPE re_exec_trap(BIF_ALIST_3);
static BIF_RETTYPE re_run(Process *p, Eterm arg1, Eterm arg2, Eterm arg3);

static void *erts_erts_pcre_malloc(size_t size) {
    return erts_alloc(ERTS_ALC_T_RE_HEAP,size);
}

static void erts_erts_pcre_free(void *ptr) {
    erts_free(ERTS_ALC_T_RE_HEAP,ptr);
}

static void *erts_erts_pcre_stack_malloc(size_t size) {
    return erts_alloc(ERTS_ALC_T_RE_STACK,size);
}

static void erts_erts_pcre_stack_free(void *ptr) {
    erts_free(ERTS_ALC_T_RE_STACK,ptr);
}

void erts_init_bif_re(void)
{
    erts_pcre_malloc = &erts_erts_pcre_malloc;
    erts_pcre_free = &erts_erts_pcre_free;
    erts_pcre_stack_malloc = &erts_erts_pcre_stack_malloc;
    erts_pcre_stack_free = &erts_erts_pcre_stack_free;
    default_table = NULL; /* ISO8859-1 default, forced into pcre */
    max_loop_limit = CONTEXT_REDS * LOOP_FACTOR;

    erts_init_trap_export(&re_exec_trap_export, am_erlang, am_re_run_trap, 3,
			  &re_exec_trap);

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

#define CAPSPEC_VALUES 0
#define CAPSPEC_TYPE 1
#define CAPSPEC_SIZE 2
#define CAPSPEC_INIT {0,0}

static int /* 0 == ok, < 0 == error */ 
parse_options(Eterm listp, /* in */
	      int *compile_options, /* out */ 
	      int *exec_options, /* out */
	      int *flags,/* out */
	      int *startoffset, /* out */
	      Eterm *capture_spec, /* capture_spec[CAPSPEC_SIZE] */ /* out */
	      int *match_limit, /* out */
	      int *match_limit_recursion)  /* out */
{
    int copt,eopt,fl;
    Eterm item;

    if (listp  == NIL) {
	copt = PCRE_DEFAULT_COMPILE_OPTS;
	eopt = PCRE_DEFAULT_EXEC_OPTS;
	fl = 0;
    } else {
	copt = 0;
	eopt = 0;
	fl = 0;
	for (;is_list(listp); listp = CDR(list_val(listp))) {
	    item = CAR(list_val(listp));
	    if (is_tuple(item)) {
		Eterm *tp = tuple_val(item);
		if (arityval(*tp) != 2 || is_not_atom(tp[1])) {
		    if (arityval(*tp) == 3 && tp[1] == am_capture) {
			if (capture_spec != NULL) {
			    capture_spec[CAPSPEC_VALUES] = tp[2];
			    capture_spec[CAPSPEC_TYPE] = tp[3];
			}
			fl |= (PARSE_FLAG_CAPTURE_OPT | 
			       PARSE_FLAG_UNIQUE_EXEC_OPT);
			continue;
		    } else {
			return -1;
		    } 
		}
		switch(tp[1]) {
		case am_capture:
		    if (capture_spec != NULL) {
			capture_spec[CAPSPEC_VALUES] = tp[2];
			capture_spec[CAPSPEC_TYPE] = am_index;
		    }
		    fl |= (PARSE_FLAG_CAPTURE_OPT | 
			   PARSE_FLAG_UNIQUE_EXEC_OPT);
		    break;
		case am_offset:
		    { 
			int tmp;
			if (!term_to_int(tp[2],&tmp) || tmp < 0) {
			    return -1; 
			}
			if (startoffset != NULL) {
			    *startoffset = tmp;
			}
		    }
		    fl |= (PARSE_FLAG_UNIQUE_EXEC_OPT|PARSE_FLAG_STARTOFFSET);
		    break;
		case am_match_limit:
		    { 
			int tmp;
			if (!term_to_int(tp[2],&tmp) || tmp < 0) {
			    return -1; 
			}
			if (match_limit != NULL) {
			    *match_limit = tmp;
			}
		    }
		    fl |= (PARSE_FLAG_UNIQUE_EXEC_OPT|PARSE_FLAG_MATCH_LIMIT);
		    break;
		case am_match_limit_recursion:
		    { 
			int tmp;
			if (!term_to_int(tp[2],&tmp) || tmp < 0) {
			    return -1; 
			}
			if (match_limit_recursion != NULL) {
			    *match_limit_recursion = tmp;
			}
		    }
		    fl |= (PARSE_FLAG_UNIQUE_EXEC_OPT|
			   PARSE_FLAG_MATCH_LIMIT_RECURSION);
		    break;
		case am_newline:
		    if (!is_atom(tp[2])) {
			return -1; 
		    }
		    switch (tp[2]) {
		    case am_cr: 
			copt |= PCRE_NEWLINE_CR; 
			eopt |= PCRE_NEWLINE_CR; 
			break;
		    case am_crlf: 
			copt |= PCRE_NEWLINE_CRLF; 
			eopt |= PCRE_NEWLINE_CRLF; 
			break;
		    case am_lf: 
			copt |= PCRE_NEWLINE_LF; 
			eopt |= PCRE_NEWLINE_LF; 
			break;
		    case am_anycrlf: 
			copt |= PCRE_NEWLINE_ANYCRLF; 
			eopt |= PCRE_NEWLINE_ANYCRLF; 
			break;
		    case am_any: 
			eopt |= PCRE_NEWLINE_ANY; 
			copt |= PCRE_NEWLINE_ANY; 
			break;
		    default:
			return -1; 
			break;
		    }    
		    break;
		default:
		    return -1; 
		}
	    } else if (is_not_atom(item)) {
		return -1;
	    } else {
		switch(item) {
		case am_anchored:
		    copt |= PCRE_ANCHORED; 
		    eopt |= PCRE_ANCHORED; 
		    break;
		case am_notempty:
		    eopt |= PCRE_NOTEMPTY; 
		    fl |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_notempty_atstart:
		    eopt |= PCRE_NOTEMPTY_ATSTART; 
		    fl |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_notbol:
		    eopt |= PCRE_NOTBOL; 
		    fl |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_noteol:
		    eopt |= PCRE_NOTEOL; 
		    fl |= PARSE_FLAG_UNIQUE_EXEC_OPT;
		    break;
		case am_no_start_optimize:
		    copt |= PCRE_NO_START_OPTIMIZE; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_caseless:
		    copt |= PCRE_CASELESS; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_dollar_endonly:
		    copt |= PCRE_DOLLAR_ENDONLY; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_dotall:
		    copt |= PCRE_DOTALL; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_extended:
		    copt |= PCRE_EXTENDED; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_firstline:
		    copt |= PCRE_FIRSTLINE; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_multiline:
		    copt |= PCRE_MULTILINE; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_no_auto_capture:
		    copt |= PCRE_NO_AUTO_CAPTURE; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_dupnames:
		    copt |= PCRE_DUPNAMES; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_ungreedy:
		    copt |= PCRE_UNGREEDY; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_ucp:
		    copt |= PCRE_UCP; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_never_utf:
		    copt |= PCRE_NEVER_UTF; 
		    fl |= PARSE_FLAG_UNIQUE_COMPILE_OPT;
		    break;
		case am_report_errors:
		    fl |= (PARSE_FLAG_UNIQUE_EXEC_OPT | 
			   PARSE_FLAG_REPORT_ERRORS);
		    break;
		case am_unicode:
		    copt |= PCRE_UTF8; 
		    fl |= (PARSE_FLAG_UNIQUE_COMPILE_OPT | PARSE_FLAG_UNICODE);
		    break;
		case am_global:
		    fl |= (PARSE_FLAG_UNIQUE_EXEC_OPT | PARSE_FLAG_GLOBAL);
		    break;
		case am_bsr_anycrlf: 
		    eopt |= PCRE_BSR_ANYCRLF; 
		    copt |= PCRE_BSR_ANYCRLF; 
		    break;
		case am_bsr_unicode: 
		    eopt |= PCRE_BSR_UNICODE; 
		    copt |= PCRE_BSR_UNICODE; 
		    break;
		default:
		    return -1;
		}
	    }
	}
	if (is_not_nil(listp)) {
	    return -1;
	}
    }
    if (compile_options != NULL) {
	*compile_options = copt;
    }
   if (exec_options != NULL) {
	*exec_options = eopt;
    }
    if (flags != NULL) {
	*flags = fl;
    }
    return 0;
}

/*
 * Build Erlang term result from compilation
 */

static Eterm 
build_compile_result(Process *p, Eterm error_tag, pcre *result, int errcode, const char *errstr, int errofset, int unicode, int with_ok, Eterm extra_err_tag) 
{
    Eterm *hp;
    Eterm ret;
    size_t pattern_size;
    int capture_count;
    int use_crlf;
    unsigned long options;
    if (!result) {
	/* Return {error_tag, {Code, String, Offset}} */
	int elen = sys_strlen(errstr);
	int need = 3 /* tuple of 2 */ + 
	    3 /* tuple of 2 */ + 
	    (2 * elen) /* The error string list */ +
	    ((extra_err_tag != NIL) ? 3 : 0);
	hp = HAlloc(p, need);
	ret = buf_to_intlist(&hp, (char *) errstr, elen, NIL);
	ret = TUPLE2(hp, ret, make_small(errofset));
	hp += 3;
	if (extra_err_tag != NIL) {
	    /* Return {error_tag, {extra_tag, 
	       {Code, String, Offset}}} instead */
	    ret =  TUPLE2(hp, extra_err_tag, ret);
	    hp += 3;
	}
	ret = TUPLE2(hp, error_tag, ret);
    } else {
	erts_pcre_fullinfo(result, NULL, PCRE_INFO_SIZE, &pattern_size);
	erts_pcre_fullinfo(result, NULL, PCRE_INFO_CAPTURECOUNT, &capture_count);
	erts_pcre_fullinfo(result, NULL, PCRE_INFO_OPTIONS, &options);
	options &= PCRE_NEWLINE_CR|PCRE_NEWLINE_LF | PCRE_NEWLINE_CRLF |
               PCRE_NEWLINE_ANY | PCRE_NEWLINE_ANYCRLF;
	use_crlf = (options == PCRE_NEWLINE_ANY ||
		    options == PCRE_NEWLINE_CRLF ||
		    options == PCRE_NEWLINE_ANYCRLF);
	/* XXX: Optimize - keep in offheap binary to allow this to 
	   be kept across traps w/o need of copying */
	ret = new_binary(p, (byte *) result, pattern_size);
	erts_pcre_free(result);
	hp = HAlloc(p, (with_ok) ? (3+6) : 6);
	ret = TUPLE5(hp,am_re_pattern, make_small(capture_count), make_small(unicode),make_small(use_crlf),ret);
	if (with_ok) {
	    hp += 6;
	    ret = TUPLE2(hp,am_ok,ret);
	}	    
    }
    return ret;
}

/*
 * Compile BIFs
 */

static BIF_RETTYPE
re_compile(Process* p, Eterm arg1, Eterm arg2)
{
    ErlDrvSizeT slen;
    char *expr;
    pcre *result;
    int errcode = 0;
    const char *errstr = "";
    int errofset = 0;
    Eterm ret;
    int options = 0;
    int pflags = 0;
    int unicode = 0;
#ifdef DEBUG
    int buffres;
#endif


    if (parse_options(arg2,&options,NULL,&pflags,NULL,NULL,NULL,NULL)
	< 0) {
	BIF_ERROR(p,BADARG);
    }

    if (pflags & PARSE_FLAG_UNIQUE_EXEC_OPT) {
	BIF_ERROR(p,BADARG);
    }

    unicode = (pflags & PARSE_FLAG_UNICODE) ? 1 : 0;

    if (pflags & PARSE_FLAG_UNICODE && !is_binary(arg1)) {
	BIF_TRAP2(ucompile_trap_exportp, p, arg1, arg2);
    }

    if (erts_iolist_size(arg1, &slen)) {
        BIF_ERROR(p,BADARG);
    }
    expr = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, slen + 1);
#ifdef DEBUG
    buffres =
#endif
    erts_iolist_to_buf(arg1, expr, slen);

    ASSERT(buffres >= 0);

    expr[slen]='\0';
    result = erts_pcre_compile2(expr, options, &errcode, 
			   &errstr, &errofset, default_table);

    ret = build_compile_result(p, am_error, result, errcode,
			       errstr, errofset, unicode, 1, NIL);
    erts_free(ERTS_ALC_T_RE_TMP_BUF, expr);
    BIF_RET(ret);
}

BIF_RETTYPE
re_compile_2(BIF_ALIST_2)
{
    return re_compile(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE
re_compile_1(BIF_ALIST_1)
{
    return re_compile(BIF_P, BIF_ARG_1, NIL);
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

typedef struct _restart_context {
    erts_pcre_extra extra;
    void *restart_data;
    Uint32 flags;
    char *subject; /* to be able to free it when done */
    pcre *code; /* Keep a copy */
    int *ovector; /* Keep until done */
    ReturnInfo *ret_info;
} RestartContext;

#define RESTART_FLAG_SUBJECT_IN_BINARY 0x1
#define RESTART_FLAG_REPORT_MATCH_LIMIT 0x2

static void cleanup_restart_context(RestartContext *rc) 
{
    if (rc->restart_data != NULL) {
	erts_pcre_free_restart_data(rc->restart_data);
	rc->restart_data = NULL;
    }
    if (rc->ovector != NULL) {
	erts_free(ERTS_ALC_T_RE_SUBJECT, rc->ovector);
	rc->ovector = NULL;
    }
    if (rc->subject != NULL && !(rc->flags & RESTART_FLAG_SUBJECT_IN_BINARY)) {
	erts_free(ERTS_ALC_T_RE_SUBJECT, rc->subject);    
    }
    rc->subject = NULL;
    if (rc->code != NULL) {
	erts_free(ERTS_ALC_T_RE_SUBJECT, rc->code);
	rc->code = NULL;
    }
    if (rc->ret_info != NULL) {
	erts_free(ERTS_ALC_T_RE_SUBJECT, rc->ret_info);
	rc->ret_info = NULL;
    }
}

static void cleanup_restart_context_bin(Binary *bp)
{
    RestartContext *rc = ERTS_MAGIC_BIN_DATA(bp);
    cleanup_restart_context(rc);
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
	    if (rc == PCRE_ERROR_MATCHLIMIT) {
		hp = HAlloc(p,3);
		res = TUPLE2(hp,am_error,am_match_limit);
	    } else if (rc == PCRE_ERROR_RECURSIONLIMIT) {
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
		    tmp_vect[i*2] = make_signed_integer(restartp->ovector[i*2],p);
		    tmp_vect[i*2+1] = make_signed_integer(restartp->ovector[i*2+1] - restartp->ovector[i*2],p);
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
			    if (restartp->ovector[(ri->v[j])*2] >= 0) {
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
	    Eterm orig = NIL;
	    Uint offset = 0;
	    Uint bitoffs = 0;
	    Uint bitsize = 0;
	    if (restartp->flags & RESTART_FLAG_SUBJECT_IN_BINARY) {
		ERTS_GET_REAL_BIN(orig_subject, orig, offset, bitoffs, bitsize);
	    }
	    if (ri->num_spec <= 0) {
		tmp_vect = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, 
				      rc * sizeof(Eterm));
		for(i = -(ri->num_spec) ;i < rc; ++i) { /* XXX: Unicode */
		    char *cp;
		    int len;
		    if (restartp->ovector[i*2] < 0) {
			cp = restartp->subject;
			len = 0;
		    } else {
			cp = restartp->subject + restartp->ovector[i*2];
			len = restartp->ovector[i*2+1] - restartp->ovector[i*2];
		    }
		    if (ri->type == RetBin) { 
			if (restartp->flags & RESTART_FLAG_SUBJECT_IN_BINARY) {
			    /* Optimized - if subject was binary to begin 
			       with, we can make sub-binaries. */
			    ErlSubBin *sb;
			    Uint virtual_offset = cp - restartp->subject;
			    hp = HAlloc(p, ERL_SUB_BIN_SIZE);
			    sb = (ErlSubBin *) hp;
			    sb->thing_word = HEADER_SUB_BIN;
			    sb->size = len;
			    sb->offs = offset + virtual_offset;
			    sb->orig = orig;
			    sb->bitoffs = bitoffs;
			    sb->bitsize = bitsize;
			    sb->is_writable = 0;
			    tmp_vect[i] = make_binary(sb);
			} else {
			    tmp_vect[i] = new_binary(p, (byte *) cp, len);
			}
		    } else {
			Eterm *hp2;
			hp2 = HAlloc(p,(2*len));
			tmp_vect[i] = buf_to_intlist(&hp2, cp, len, NIL);
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
			    if (restartp->ovector[(ri->v[j])*2] >= 0) {
				x = ri->v[j];
				break;
			    }
			}
			i = n-1;
		    }
		    if (x < rc && x >= 0) {
			char *cp;
			int len;
			if (restartp->ovector[x*2] < 0) {
			    cp = restartp->subject;
			    len = 0;
			} else {
			    cp = restartp->subject + restartp->ovector[x*2];
			    len = restartp->ovector[x*2+1] - restartp->ovector[x*2];
			}
			if (ri->type == RetBin) { 
			    if (restartp->flags & RESTART_FLAG_SUBJECT_IN_BINARY) {
				/* Optimized - if subject was binary to begin 
				   with, we could make sub-binaries. */
				ErlSubBin *sb;
				Uint virtual_offset = cp - restartp->subject;
				hp = HAlloc(p, ERL_SUB_BIN_SIZE);
				sb = (ErlSubBin *) hp;
				sb->thing_word = HEADER_SUB_BIN;
				sb->size = len;
				sb->offs = offset + virtual_offset;
				sb->orig = orig;
				sb->bitoffs = bitoffs;
				sb->bitsize = bitsize;
				sb->is_writable = 0;
				tmp_vect[n] = make_binary(sb);
			    } else {
				tmp_vect[n] = new_binary(p, (byte *) cp, len);
			    }
			} else {
			    Eterm *hp2;
			    hp2 = HAlloc(p,(2*len));
			    tmp_vect[n] = buf_to_intlist(&hp2, cp, len, NIL);
			} 
		    } else {
			if (ri->type == RetBin) { 
			    tmp_vect[n] = new_binary(p, (byte *) "", 0);
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


static void build_one_capture(const pcre *code, ReturnInfo **ri, int *sallocated, int has_dupnames, char *name) 
{
    ReturnInfo *r = (*ri);
    if (has_dupnames) {
	/* Build a sequence of positions, starting with -size if
	   more than one, otherwise just put the index there... */
	char *first,*last;
	int esize = erts_pcre_get_stringtable_entries(code,name,&first,&last);
	if (esize == PCRE_ERROR_NOSUBSTRING) {
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
	if ((r->v[r->num_spec - 1] = erts_pcre_get_stringnumber(code,name)) ==
	    PCRE_ERROR_NOSUBSTRING) {
	    r->v[r->num_spec - 1] = -1;
	}
    }
    *ri = r;
}    

static ReturnInfo *
build_capture(Eterm capture_spec[CAPSPEC_SIZE], const pcre *code)
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
	    int rc,i,top;
	    int entrysize;
	    unsigned char *nametable, *last = NULL;
	    int has_dupnames;
	    unsigned long options;

	    if (erts_pcre_fullinfo(code, NULL, PCRE_INFO_OPTIONS, &options) != 0)
		goto error;
	    if ((rc = erts_pcre_fullinfo(code, NULL, PCRE_INFO_NAMECOUNT, &top)) != 0)
		goto error;
	    if (top <= 0) {
		ri->num_spec = 0;
		ri->type = RetNone;
		break;
	    }
	    if (erts_pcre_fullinfo(code, NULL, PCRE_INFO_NAMEENTRYSIZE, &entrysize) != 0)
		goto error;
	    if (erts_pcre_fullinfo(code, NULL, PCRE_INFO_NAMETABLE, &nametable) != 0)
		goto error;
	    
	    has_dupnames = ((options & PCRE_DUPNAMES) != 0);

	    for(i=0;i<top;++i) {
		if (last == NULL || !has_dupnames || strcmp((char *) last+2,(char *) nametable+2)) {
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
		} else if (is_atom(val) || is_binary(val) || is_list(val)) {
		    int has_dupnames;
		    unsigned long options;
		    if (erts_pcre_fullinfo(code, NULL, PCRE_INFO_OPTIONS, &options) != 0)
			goto error;
		    has_dupnames = ((options & PCRE_DUPNAMES) != 0);
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
			memcpy(tmpb,ap->name,ap->len);
			tmpb[ap->len] = '\0';
		    } else {
			ErlDrvSizeT slen;
#ifdef DEBUG
			int buffres;
#endif

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

#ifdef DEBUG
			buffres =
#endif
			erts_iolist_to_buf(val, tmpb, slen);
			ASSERT(buffres >= 0);
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
re_run(Process *p, Eterm arg1, Eterm arg2, Eterm arg3)
{
    const pcre *code_tmp;
    RestartContext restart;
    byte *temp_alloc = NULL;
    ErlDrvSizeT slength;
    int startoffset = 0;
    int options = 0, comp_options = 0;
    int ovsize;
    int pflags;
    Eterm *tp;
    int rc;
    Eterm res;
    size_t code_size;
    Uint loop_limit_tmp;
    unsigned long loop_count;
    Eterm capture[CAPSPEC_SIZE] = CAPSPEC_INIT;
    int is_list_cap;
    int match_limit = 0;
    int match_limit_recursion = 0;

    if (parse_options(arg3,&comp_options,&options,&pflags,&startoffset,capture,
		      &match_limit,&match_limit_recursion)
	< 0) {
	BIF_ERROR(p,BADARG);
    }
    is_list_cap = ((pflags & PARSE_FLAG_CAPTURE_OPT) && 
		   (capture[CAPSPEC_TYPE] == am_list));

    if (is_not_tuple(arg2) || (arityval(*tuple_val(arg2)) != 5)) {
	if (is_binary(arg2) || is_list(arg2) || is_nil(arg2)) {
	    /* Compile from textual RE */
	    ErlDrvSizeT slen;
	    char *expr;
	    pcre *result;
	    int errcode = 0;
	    const char *errstr = "";
	    int errofset = 0;
	    int capture_count;
#ifdef DEBUG
	    int buffres;
#endif

	    if (pflags & PARSE_FLAG_UNICODE && 
		(!is_binary(arg2) || !is_binary(arg1) ||
		 (is_list_cap && !(pflags & PARSE_FLAG_GLOBAL)))) { 
		BIF_TRAP3(urun_trap_exportp, p, arg1, arg2, arg3);
	    }
	    
	    if (erts_iolist_size(arg2, &slen)) {
		BIF_ERROR(p,BADARG);
	    }
	    
	    expr = erts_alloc(ERTS_ALC_T_RE_TMP_BUF, slen + 1);
	    
#ifdef DEBUG
	    buffres =
#endif
	    erts_iolist_to_buf(arg2, expr, slen);

	    ASSERT(buffres >= 0);

	    expr[slen]='\0';
	    result = erts_pcre_compile2(expr, comp_options, &errcode, 
				   &errstr, &errofset, default_table);
	    if (!result) {
		/* Compilation error gives badarg except in the compile 
		   function or if we have PARSE_FLAG_REPORT_ERRORS */
		if (pflags &  PARSE_FLAG_REPORT_ERRORS) {
		    res = build_compile_result(p, am_error, result, errcode,
					       errstr, errofset, 
					       (pflags & 
						PARSE_FLAG_UNICODE) ? 1 : 0, 
					       1, am_compile);
		    erts_free(ERTS_ALC_T_RE_TMP_BUF, expr);
		    BIF_RET(res);
		} else {
		    erts_free(ERTS_ALC_T_RE_TMP_BUF, expr);
		    BIF_ERROR(p,BADARG);
		}
	    }
	    if (pflags & PARSE_FLAG_GLOBAL) {
		Eterm precompiled = 
		    build_compile_result(p, am_error,
					 result, errcode, 
					 errstr, errofset, 
					 (pflags & 
					  PARSE_FLAG_UNICODE) ? 1 : 0,
					 0, NIL);
		Eterm *hp,r;
		erts_free(ERTS_ALC_T_RE_TMP_BUF, expr);
		hp = HAlloc(p,4);
		/* arg2 is in the tuple just to make exceptions right */
		r = TUPLE3(hp,arg3,
			   ((pflags & PARSE_FLAG_UNIQUE_COMPILE_OPT) ? 
			    am_true : 
			    am_false), arg2);
		BIF_TRAP3(grun_trap_exportp, p, arg1, precompiled, r);
	    }

	    erts_pcre_fullinfo(result, NULL, PCRE_INFO_SIZE, &code_size);
	    erts_pcre_fullinfo(result, NULL, PCRE_INFO_CAPTURECOUNT, &capture_count);
	    ovsize = 3*(capture_count+1);
	    restart.code = erts_alloc(ERTS_ALC_T_RE_SUBJECT, code_size);
	    memcpy(restart.code, result, code_size);
	    erts_pcre_free(result);
	    erts_free(ERTS_ALC_T_RE_TMP_BUF, expr);
	    /*unicode = (pflags & PARSE_FLAG_UNICODE) ? 1 : 0;*/
	} else {  
	    BIF_ERROR(p,BADARG);
	}
    } else {
	if (pflags & PARSE_FLAG_UNIQUE_COMPILE_OPT) {
	    BIF_ERROR(p,BADARG);
	}

	tp = tuple_val(arg2);
	if (tp[1] != am_re_pattern || is_not_small(tp[2]) || 
	    is_not_small(tp[3]) || is_not_small(tp[4]) || 
	    is_not_binary(tp[5])) {
	    BIF_ERROR(p,BADARG);
	}

	if (unsigned_val(tp[3]) && 
	    (!is_binary(arg1) ||
	     (is_list_cap && !(pflags & PARSE_FLAG_GLOBAL)))) { /* unicode */
	    BIF_TRAP3(urun_trap_exportp, p, arg1, arg2,
		      arg3);
	}

	if (pflags & PARSE_FLAG_GLOBAL) {
	    Eterm *hp,r;
	    hp = HAlloc(p,3);
	    r = TUPLE2(hp,arg3,am_false);
	    BIF_TRAP3(grun_trap_exportp, p, arg1, arg2,
		      r);
	}

	ovsize = 3*(unsigned_val(tp[2])+1);
	code_size = binary_size(tp[5]);
	code_tmp = (const pcre *) erts_get_aligned_binary_bytes(tp[5], &temp_alloc);
	if (code_tmp == NULL || code_size < 4) {
	    erts_free_aligned_binary_bytes(temp_alloc);
	    BIF_ERROR(p, BADARG);
	}
	restart.code = erts_alloc(ERTS_ALC_T_RE_SUBJECT, code_size);
	memcpy(restart.code, code_tmp, code_size);
	erts_free_aligned_binary_bytes(temp_alloc);

    }


    restart.ovector =  erts_alloc(ERTS_ALC_T_RE_SUBJECT, ovsize * sizeof(int));
    restart.extra.flags = PCRE_EXTRA_TABLES | PCRE_EXTRA_LOOP_LIMIT;
    restart.extra.tables = default_table;
    restart.extra.loop_limit = ERTS_BIF_REDS_LEFT(p) * LOOP_FACTOR;
    loop_limit_tmp = max_loop_limit; /* To lesser probability of race in debug
					situation (erts_debug) */
    if (restart.extra.loop_limit > loop_limit_tmp) {
	restart.extra.loop_limit = loop_limit_tmp;
    }
    restart.restart_data = NULL;
    restart.extra.restart_data = &restart.restart_data;
    restart.extra.restart_flags = 0;
    restart.extra.loop_counter_return = &loop_count;
    restart.ret_info = NULL;

    if (pflags & PARSE_FLAG_MATCH_LIMIT) {
	restart.extra.flags |= PCRE_EXTRA_MATCH_LIMIT;
	restart.extra.match_limit = match_limit;
    }

    if (pflags & PARSE_FLAG_MATCH_LIMIT_RECURSION) {
	restart.extra.flags |= PCRE_EXTRA_MATCH_LIMIT_RECURSION;
	restart.extra.match_limit_recursion = match_limit_recursion;
    }
    
    if (pflags & PARSE_FLAG_CAPTURE_OPT) {
	if ((restart.ret_info = build_capture(capture,restart.code)) == NULL) {
	    erts_free(ERTS_ALC_T_RE_SUBJECT, restart.ovector);
	    erts_free(ERTS_ALC_T_RE_SUBJECT, restart.code);
	    BIF_ERROR(p,BADARG);
	}
    }

    /*  Optimized - if already in binary off heap, keep that and avoid
       copying, also binary returns can be sub binaries in that case */

    restart.flags = 0;
    if (is_binary(arg1)) {
	Eterm real_bin;
	Uint offset;
	Eterm* bptr;
	int bitoffs;
	int bitsize;
	ProcBin* pb;

	ERTS_GET_REAL_BIN(arg1, real_bin, offset, bitoffs, bitsize);

	slength = binary_size(arg1);
	bptr = binary_val(real_bin);
	if (bitsize != 0 || bitoffs != 0 ||  (*bptr != HEADER_PROC_BIN)) {
	    goto handle_iolist;
	}
	pb = (ProcBin *) bptr;
	if (pb->flags) {
	    erts_emasculate_writable_binary(pb);
	}
	restart.subject = (char *) (pb->bytes+offset);
	restart.flags |= RESTART_FLAG_SUBJECT_IN_BINARY;
    } else {
#ifdef DEBUG
	int buffres;
#endif
handle_iolist:
	if (erts_iolist_size(arg1, &slength)) {
	    erts_free(ERTS_ALC_T_RE_SUBJECT, restart.ovector);
	    erts_free(ERTS_ALC_T_RE_SUBJECT, restart.code);
	    if (restart.ret_info != NULL) {
		erts_free(ERTS_ALC_T_RE_SUBJECT, restart.ret_info);
	    }
	    BIF_ERROR(p,BADARG);
	}
	restart.subject = erts_alloc(ERTS_ALC_T_RE_SUBJECT, slength);

#ifdef DEBUG
	buffres =
#endif
	erts_iolist_to_buf(arg1, restart.subject, slength);
	ASSERT(buffres >= 0);
    }

    if (pflags & PARSE_FLAG_REPORT_ERRORS) {
	restart.flags |= RESTART_FLAG_REPORT_MATCH_LIMIT;
    }

#ifdef DEBUG
    loop_count = 0xFFFFFFFF;
#endif

    rc = erts_pcre_exec(restart.code, &(restart.extra), restart.subject, 
			slength, startoffset, 
			options, restart.ovector, ovsize);

    if (rc == PCRE_ERROR_BADENDIANNESS || rc == PCRE_ERROR_BADMAGIC) {
	cleanup_restart_context(&restart);
	BIF_ERROR(p,BADARG);
    }
    
    ASSERT(loop_count != 0xFFFFFFFF);
    BUMP_REDS(p, loop_count / LOOP_FACTOR);
    if (rc == PCRE_ERROR_LOOP_LIMIT) {
	/* Trap */
	Binary *mbp = erts_create_magic_binary(sizeof(RestartContext),
					       cleanup_restart_context_bin);
	RestartContext *restartp = ERTS_MAGIC_BIN_DATA(mbp);
	Eterm magic_bin;
	Eterm *hp;
	memcpy(restartp,&restart,sizeof(RestartContext));
	BUMP_ALL_REDS(p);
	hp = HAlloc(p, PROC_BIN_SIZE);
	magic_bin = erts_mk_magic_binary_term(&hp, &MSO(p), mbp);
	BIF_TRAP3(&re_exec_trap_export, 
		  p,
		  arg1,
		  arg2 /* To avoid GC of precompiled code, XXX: not utilized yet */,
		  magic_bin);
    }

    res = build_exec_return(p, rc, &restart, arg1);
 
    cleanup_restart_context(&restart);

    BIF_RET(res);
}

BIF_RETTYPE
re_run_3(BIF_ALIST_3)
{
    return re_run(BIF_P,BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

BIF_RETTYPE
re_run_2(BIF_ALIST_2) 
{
    return re_run(BIF_P,BIF_ARG_1, BIF_ARG_2, NIL);
}

/*
 * The "magic" trap target, continue a re:run
 */

static BIF_RETTYPE re_exec_trap(BIF_ALIST_3) 
     /* XXX: Optimize - arg 1 and 2 to be utilized for keeping binary 
	code and subject */
{
    Binary *mbp;
    RestartContext *restartp;
    int rc;
    unsigned long loop_count;
    Uint loop_limit_tmp;
    Eterm res;

    ASSERT(ERTS_TERM_IS_MAGIC_BINARY(BIF_ARG_3));

    mbp = ((ProcBin *) binary_val(BIF_ARG_3))->val;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp)
	   == cleanup_restart_context_bin);

    restartp = (RestartContext *) ERTS_MAGIC_BIN_DATA(mbp);

    restartp->extra.loop_limit = ERTS_BIF_REDS_LEFT(BIF_P) * LOOP_FACTOR;
    loop_limit_tmp = max_loop_limit; /* To lesser probability of race in debug
					situation (erts_debug) */
    if (restartp->extra.loop_limit > loop_limit_tmp) {
	restartp->extra.loop_limit = loop_limit_tmp;
    }
    restartp->extra.loop_counter_return = &loop_count;
    restartp->extra.restart_data = &restartp->restart_data;
    restartp->extra.restart_flags = 0;
    
#ifdef DEBUG
    loop_count = 0xFFFFFFFF;
#endif
    rc = erts_pcre_exec(NULL, &(restartp->extra), NULL, 0, 0, 0, NULL, 0);
    ASSERT(loop_count != 0xFFFFFFFF);
    BUMP_REDS(BIF_P, loop_count / LOOP_FACTOR);
    if (rc == PCRE_ERROR_LOOP_LIMIT) {
	/* Trap */
	BUMP_ALL_REDS(BIF_P);
	BIF_TRAP3(&re_exec_trap_export, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }
    res = build_exec_return(BIF_P, rc, restartp, BIF_ARG_1);
 
    cleanup_restart_context(restartp);

    BIF_RET(res);
}
    
BIF_RETTYPE
re_inspect_2(BIF_ALIST_2) 
{
    Eterm *tp,*tmp_vec,*hp;
    int i,top,j;
    int entrysize;
    unsigned char *nametable, *last,*name;
    int has_dupnames;
    unsigned long options;
    int num_names;
    Eterm res;
    const pcre *code;
    byte *temp_alloc = NULL;
#ifdef DEBUG
    int infores;
#endif
    

    if (is_not_tuple(BIF_ARG_1) || (arityval(*tuple_val(BIF_ARG_1)) != 5)) {
	goto error;
    }
    tp = tuple_val(BIF_ARG_1);
    if (tp[1] != am_re_pattern || is_not_small(tp[2]) || 
	is_not_small(tp[3]) || is_not_small(tp[4]) || 
	is_not_binary(tp[5])) {
	goto error;
    }
    if (BIF_ARG_2 != am_namelist) {
	goto error;
    }
    if ((code = (const pcre *) 
	 erts_get_aligned_binary_bytes(tp[5], &temp_alloc)) == NULL) {
	goto error;
    }

    /* OK, so let's try to get some info */
    
    if (erts_pcre_fullinfo(code, NULL, PCRE_INFO_OPTIONS, &options) != 0)
	goto error;

#ifdef DEBUG
    infores =
#endif
    erts_pcre_fullinfo(code, NULL, PCRE_INFO_NAMECOUNT, &top);

    ASSERT(infores == 0);

    if (top <= 0) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp,am_namelist,NIL);
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_RET(res);
    }
#ifdef DEBUG
    infores =
#endif
    erts_pcre_fullinfo(code, NULL, PCRE_INFO_NAMEENTRYSIZE, &entrysize);

    ASSERT(infores == 0);

#ifdef DEBUG
    infores =
#endif
    erts_pcre_fullinfo(code, NULL, PCRE_INFO_NAMETABLE, &nametable);

    ASSERT(infores == 0);
    
    has_dupnames = ((options & PCRE_DUPNAMES) != 0);
    /* First, count the names */
    num_names = 0;
    last = NULL;
    name = nametable;
    for(i=0;i<top;++i) {
	if (last == NULL || !has_dupnames || strcmp((char *) last+2,
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
	if (last == NULL || !has_dupnames || strcmp((char *) last+2,
						    (char *) name+2)) {
	    tmp_vec[j++] = new_binary(BIF_P, (byte *) name+2, strlen((char *) name+2));
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
    

	
    

	
