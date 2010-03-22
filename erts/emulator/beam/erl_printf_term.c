/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2010. All Rights Reserved.
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

#include "erl_printf_term.h"
#include "sys.h"
#include "big.h"

#define PRINT_CHAR(CNT, FN, ARG, C)					\
do {									\
    int res__ = erts_printf_char((FN), (ARG), (C));			\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

#define PRINT_STRING(CNT, FN, ARG, STR)					\
do {									\
    int res__ = erts_printf_string((FN), (ARG), (STR));			\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

#define PRINT_BUF(CNT, FN, ARG, BUF, LEN)				\
do {									\
    int res__ = erts_printf_buf((FN), (ARG), (char*)(BUF), (LEN));	\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

#define PRINT_POINTER(CNT, FN, ARG, PTR)				\
do {									\
    int res__ = erts_printf_pointer((FN), (ARG), (void *) (PTR));	\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

#define PRINT_ULONG(CNT, FN, ARG, C, P, W, I)				\
do {									\
    int res__ = erts_printf_ulong((FN), (ARG), (C), (P), (W), (I));	\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

#define PRINT_SLONG(CNT, FN, ARG, C, P, W, I)				\
do {									\
    int res__ = erts_printf_slong((FN), (ARG), (C), (P), (W), (I));	\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

#define PRINT_DOUBLE(CNT, FN, ARG, C, P, W, I)				\
do {									\
    int res__ = erts_printf_double((FN), (ARG), (C), (P), (W), (I));	\
    if (res__ < 0)							\
	return res__;							\
    (CNT) += res__;							\
} while (0)

/* CTYPE macros */

#define LATIN1

#define IS_DIGIT(c)  ((c) >= '0' && (c) <= '9')
#ifdef LATIN1
#define IS_LOWER(c)  (((c) >= 'a' && (c) <= 'z') \
		      || ((c) >= 128+95 && (c) <= 255 && (c) != 247))
#define IS_UPPER(c)  (((c) >= 'A' && (c) <= 'Z') \
		      || ((c) >= 128+64 && (c) <= 128+94 && (c) != 247-32))
#else
#define IS_LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define IS_UPPER(c)  ((c) >= 'A' && (c) <= 'Z')
#endif

#define IS_ALNUM(c)  (IS_DIGIT(c) || IS_LOWER(c) || IS_UPPER(c))

/* We don't include 160 (non-breaking space). */
#define IS_SPACE(c)  (c == ' ' || c == '\n' || c == '\t' || c == '\r')

#ifdef LATIN1
#define IS_CNTRL(c)  ((c) < ' ' || (c) == 127 \
		      || ((c) >= 128 && (c) < 128+32))
#else
/* Treat all non-ASCII as control characters */
#define IS_CNTRL(c)  ((c) < ' ' || (c) >= 127)
#endif

#define IS_PRINT(c)  (!IS_CNTRL(c))

/* return 0 if list is not a non-empty flat list of printable characters */

static int
is_printable_string(Eterm list)
{
    int len = 0;
    int c;

    while(is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	c = signed_val(hd);
	/* IS_PRINT || IS_SPACE would be another way to put it */
	if (IS_CNTRL(c) && !IS_SPACE(c))
	   return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

/* print a atom doing what quoting is necessary */
static int print_atom_name(fmtfn_t fn, void* arg, Eterm atom, long *dcount)
{
    int n, i;
    int res;
    int need_quote;
    int pos;
    byte *s;
    byte *cpos;
    int c;

    res = 0;
    i = atom_val(atom);

    if ((i < 0) || (i >= atom_table_size()) ||  (atom_tab(i) == NULL)) {
	PRINT_STRING(res, fn, arg, "<bad atom index: ");
	PRINT_SLONG(res, fn, arg, 'd', 0, 1, (signed long) i);
	PRINT_CHAR(res, fn, arg, '>');
	return res;
    }

    s = atom_tab(i)->name;
    n = atom_tab(i)->len;

    *dcount -= atom_tab(i)->len;

    if (n == 0) {
	PRINT_STRING(res, fn, arg, "''");
	return res;
    }


    need_quote = 0;
    cpos = s;
    pos = n - 1;

    c = *cpos++;
    if (!IS_LOWER(c))
	need_quote++;
    else {
	while (pos--) {
	    c = *cpos++;
	    if (!IS_ALNUM(c) && (c != '_')) {
		need_quote++;
		break;
	    }
	}
    }
    cpos = s;
    pos = n;
    if (need_quote)
	PRINT_CHAR(res, fn, arg, '\'');
    while(pos--) {
	c = *cpos++;
	switch(c) {
	case '\'': PRINT_STRING(res, fn, arg, "\\'"); break;
	case '\\': PRINT_STRING(res, fn, arg, "\\\\"); break;
	case '\n': PRINT_STRING(res, fn, arg, "\\n"); break;
	case '\f': PRINT_STRING(res, fn, arg, "\\f"); break;
	case '\t': PRINT_STRING(res, fn, arg, "\\t"); break;
	case '\r': PRINT_STRING(res, fn, arg, "\\r"); break;
	case '\b': PRINT_STRING(res, fn, arg, "\\b"); break;
	case '\v': PRINT_STRING(res, fn, arg, "\\v"); break;
	default:
	    if (IS_CNTRL(c)) {
		PRINT_CHAR(res, fn, arg, '\\');
		PRINT_ULONG(res, fn, arg, 'o', 1, 3, (unsigned long) c);
	    }
	    else
		PRINT_CHAR(res, fn, arg, (char) c);
	    break;
	}
    }
    if (need_quote)
	PRINT_CHAR(res, fn, arg, '\'');
    return res;
}



static int
print_term(fmtfn_t fn, void* arg, Eterm obj, long *dcount)
{
    int res;
    int i;
    Uint32 *ref_num;
    Eterm* nobj;

    res = 0;

    if ((*dcount)-- <= 0)
	return res;

#ifdef HYBRID___NOT_ACTIVE
    /* Color coded output based on memory location */
    if(ptr_val(obj) >= global_heap && ptr_val(obj) < global_hend)
        PRINT_STRING(res, fn, arg, "\033[32m");
#ifdef INCREMENTAL
    else if(ptr_val(obj) >= inc_fromspc && ptr_val(obj) < inc_fromend)
        PRINT_STRING(res, fn, arg, "\033[33m");
#endif
    else if(IS_CONST(obj))
        PRINT_STRING(res, fn, arg, "\033[34m");
    else
        PRINT_STRING(res, fn, arg, "\033[31m");
#endif

    if (is_CP(obj)) {
	PRINT_STRING(res, fn, arg, "<cp/header:");
	PRINT_POINTER(res, fn, arg, cp_val(obj));
	PRINT_CHAR(res, fn, arg, '>');
	return res;
    }

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	PRINT_STRING(res, fn, arg, "[]");
	break;
    case ATOM_DEF: {
	int tres = print_atom_name(fn, arg, obj, dcount);
	if (tres < 0)
	    return tres;
	res += tres;
	if (*dcount <= 0)
	    return res;
	break;
    }
    case SMALL_DEF:
	PRINT_SLONG(res, fn, arg, 'd', 0, 1, (signed long) signed_val(obj));
	break;
    case BIG_DEF: {
	int print_res;
	char def_buf[64];
	char *buf, *big_str;
	Uint sz = (Uint) big_decimal_estimate(obj);
	sz++;
	if (sz <= 64)
	    buf = &def_buf[0];
	else
	    buf = erts_alloc(ERTS_ALC_T_TMP, sz);
	big_str = erts_big_to_string(obj, buf, sz);
	print_res = erts_printf_string(fn, arg, big_str);
	if (buf != &def_buf[0])
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	if (print_res < 0)
	    return print_res;
	res += print_res;
	break;
    }
    case REF_DEF:
    case EXTERNAL_REF_DEF:
	PRINT_STRING(res, fn, arg, "#Ref<");
	PRINT_ULONG(res, fn, arg, 'u', 0, 1,
		    (unsigned long) ref_channel_no(obj));
	ref_num = ref_numbers(obj);
	for (i = ref_no_of_numbers(obj)-1; i >= 0; i--) {
	    PRINT_CHAR(res, fn, arg, '.');
	    PRINT_ULONG(res, fn, arg, 'u', 0, 1, (unsigned long) ref_num[i]);
	}
	PRINT_CHAR(res, fn, arg, '>');
	break;
    case PID_DEF:
    case EXTERNAL_PID_DEF:
	PRINT_CHAR(res, fn, arg, '<');
	PRINT_ULONG(res, fn, arg, 'u', 0, 1,
		    (unsigned long) pid_channel_no(obj));
	PRINT_CHAR(res, fn, arg, '.');
	PRINT_ULONG(res, fn, arg, 'u', 0, 1,
		    (unsigned long) pid_number(obj));
	PRINT_CHAR(res, fn, arg, '.');
	PRINT_ULONG(res, fn, arg, 'u', 0, 1,
		    (unsigned long) pid_serial(obj));
	PRINT_CHAR(res, fn, arg, '>');
	break;
    case PORT_DEF:
    case EXTERNAL_PORT_DEF:
	PRINT_STRING(res, fn, arg, "#Port<");
	PRINT_ULONG(res, fn, arg, 'u', 0, 1,
		    (unsigned long) port_channel_no(obj));
	PRINT_CHAR(res, fn, arg, '.');
	PRINT_ULONG(res, fn, arg, 'u', 0, 1,
		    (unsigned long) port_number(obj));
	PRINT_CHAR(res, fn, arg, '>');
	break;
    case LIST_DEF:
	if (is_printable_string(obj)) {
	    int c;
	    PRINT_CHAR(res, fn, arg, '"');
	    nobj = list_val(obj);
	    while (1) {
		if ((*dcount)-- <= 0)
		    return res;
		c = signed_val(*nobj++);
		if (c == '\n')
		    PRINT_STRING(res, fn, arg, "\\n");
		else {
		    if (c == '"')
			PRINT_CHAR(res, fn, arg, '\\');
		    PRINT_CHAR(res, fn, arg, (char) c);
		}
		if (is_not_list(*nobj))
		    break;
		nobj = list_val(*nobj);
	    }
	    PRINT_CHAR(res, fn, arg, '"');
	} else {
	    PRINT_CHAR(res, fn, arg, '[');
	    nobj = list_val(obj);
	    while (1) {
		int tres = print_term(fn, arg, *nobj++, dcount);
		if (tres < 0)
		    return tres;
		res += tres;
		if (*dcount <= 0)
		    return res;
		if (is_not_list(*nobj))
		    break;
		PRINT_CHAR(res, fn, arg, ',');
		nobj = list_val(*nobj);
	    }
	    if (is_not_nil(*nobj)) {
		int tres;
		PRINT_CHAR(res, fn, arg, '|');
		tres = print_term(fn, arg, *nobj, dcount);
		if (tres < 0)
		    return tres;
		res += tres;
		if (*dcount <= 0)
		    return res;
	    }
	    PRINT_CHAR(res, fn, arg, ']');
	}
	break;
    case TUPLE_DEF:
	nobj = tuple_val(obj);	/* pointer to arity */
	i = arityval(*nobj);	/* arity */
	PRINT_CHAR(res, fn, arg, '{');
	while (i--) {
	    int tres = print_term(fn, arg, *++nobj, dcount);
	    if (tres < 0)
		return tres;
	    res += tres;
	    if (*dcount <= 0)
		return res;
	    if (i >= 1)
		PRINT_CHAR(res, fn, arg, ',');
	}
	PRINT_CHAR(res, fn, arg, '}');
	break;
    case FLOAT_DEF: {
	    FloatDef ff;
	    GET_DOUBLE(obj, ff);
	    PRINT_DOUBLE(res, fn, arg, 'e', 6, 0, ff.fd);
	}
	break;
    case BINARY_DEF:
	{
	    ProcBin* pb = (ProcBin *) binary_val(obj);
	    if (pb->size == 1)
		PRINT_STRING(res, fn, arg, "<<1 byte>>");
	    else {
		PRINT_STRING(res, fn, arg, "<<");
		PRINT_ULONG(res, fn, arg, 'u', 0, 1, (unsigned long) pb->size);
		PRINT_STRING(res, fn, arg, " bytes>>");
	    }
	}
	break;
    case EXPORT_DEF:
	{
	    Export* ep = *((Export **) (export_val(obj) + 1));
	    Atom* module = atom_tab(atom_val(ep->code[0]));
	    Atom* name = atom_tab(atom_val(ep->code[1]));

	    PRINT_STRING(res, fn, arg, "#Fun<");
	    PRINT_BUF(res, fn, arg, module->name, module->len);
	    PRINT_CHAR(res, fn, arg, '.');
	    PRINT_BUF(res, fn, arg, name->name, name->len);
	    PRINT_CHAR(res, fn, arg, '.');
	    PRINT_SLONG(res, fn, arg, 'd', 0, 1,
			(signed long) ep->code[2]);
	    PRINT_CHAR(res, fn, arg, '>');
	}
	break;
    case FUN_DEF:
	{
	    ErlFunThing *funp = (ErlFunThing *) fun_val(obj);
	    Atom *ap = atom_tab(atom_val(funp->fe->module));

	    PRINT_STRING(res, fn, arg, "#Fun<");
	    PRINT_BUF(res, fn, arg, ap->name, ap->len);
	    PRINT_CHAR(res, fn, arg, '.');
	    PRINT_SLONG(res, fn, arg, 'd', 0, 1,
			(signed long) funp->fe->old_index);
	    PRINT_CHAR(res, fn, arg, '.');
	    PRINT_SLONG(res, fn, arg, 'd', 0, 1,
			(signed long) funp->fe->old_uniq);
	    PRINT_CHAR(res, fn, arg, '>');
	}
	break;
    default:
	PRINT_STRING(res, fn, arg, "<unknown:");
	PRINT_POINTER(res, fn, arg, (UWord) obj);
	PRINT_CHAR(res, fn, arg, '>');
	break;
    }

    return res;
}

int
erts_printf_term(fmtfn_t fn, void* arg, unsigned long term, long precision)
{
    int res = print_term(fn, arg, (Uint) term, &precision);
    if (res < 0)
	return res;
    if (precision <= 0)
	PRINT_STRING(res, fn, arg, "... ");
    return res;
}
