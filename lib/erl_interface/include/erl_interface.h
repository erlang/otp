/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#ifndef _ERL_INTERFACE_H
#define _ERL_INTERFACE_H

/************************************************************************/
/*      This file defines the complete interface to erl_interface       */
/*      Note: the 'ei' interface is the prefered C API.                 */
/************************************************************************/

#include "ei.h"			/* ei is the base */

/* -------------------------------------------------------------------- */
/*                           Public defines                             */
/* -------------------------------------------------------------------- */

#define ERL_COMPOUND (1 << 7)

#define ERL_UNDEF        0
#define ERL_INTEGER      1
#define ERL_U_INTEGER    2 /* unsigned int */
#define ERL_ATOM         3
#define ERL_PID          4
#define ERL_PORT         5
#define ERL_REF          6
#define ERL_CONS         (7 | ERL_COMPOUND)
#define ERL_LIST         ERL_CONS
#define ERL_NIL          8
#define ERL_EMPTY_LIST   ERL_NIL
#define ERL_TUPLE        (9 | ERL_COMPOUND)
#define ERL_BINARY      10
#define ERL_FLOAT       11
#define ERL_VARIABLE    (12 | ERL_COMPOUND) /* used in patterns */
#define ERL_SMALL_BIG   13
#define ERL_U_SMALL_BIG 14
#define ERL_FUNCTION    (15 | ERL_COMPOUND)
#define ERL_BIG         16
#define ERL_LONGLONG	17
#define ERL_U_LONGLONG	18


#define ERL_TYPE(x)       (ERL_HEADER(x)->type)

/* FIXME some macros left in erl_eterm.h should probably be documented */

#define ERL_IS_INTEGER(x)           (ERL_TYPE(x) == ERL_INTEGER)
#define ERL_IS_UNSIGNED_INTEGER(x)  (ERL_TYPE(x) == ERL_U_INTEGER)
#define ERL_IS_LONGLONG(x)          (ERL_TYPE(x) == ERL_LONGLONG)
#define ERL_IS_UNSIGNED_LONGLONG(x) (ERL_TYPE(x) == ERL_U_LONGLONG)
#define ERL_IS_FLOAT(x)             (ERL_TYPE(x) == ERL_FLOAT)
#define ERL_IS_ATOM(x)              (ERL_TYPE(x) == ERL_ATOM)
#define ERL_IS_PID(x)               (ERL_TYPE(x) == ERL_PID)
#define ERL_IS_PORT(x)              (ERL_TYPE(x) == ERL_PORT)
#define ERL_IS_REF(x)               (ERL_TYPE(x) == ERL_REF)
#define ERL_IS_TUPLE(x)             (ERL_TYPE(x) == ERL_TUPLE)
#define ERL_IS_BINARY(x)            (ERL_TYPE(x) == ERL_BINARY)
#define ERL_IS_NIL(x)               (ERL_TYPE(x) == ERL_NIL)
#define ERL_IS_EMPTY_LIST(x)        ERL_IS_NIL(x)
#define ERL_IS_CONS(x)              (ERL_TYPE(x) == ERL_CONS)
#define ERL_IS_LIST(x)              (ERL_IS_CONS(x) || ERL_IS_EMPTY_LIST(x))

/*
 * Macros used for XXXX
 */

#define ERL_HEADER(x)     ((Erl_Header *)x)
#define ERL_COUNT(x)      (ERL_HEADER(x)->count)

/*
 * Macros used for retrieving values from Erlang terms.
 */

#define ERL_INT_VALUE(x)  ((x)->uval.ival.i)
#define ERL_INT_UVALUE(x) ((x)->uval.uival.u)
#define ERL_LL_VALUE(x)   ((x)->uval.llval.i)
#define ERL_LL_UVALUE(x)  ((x)->uval.ullval.u)

#define ERL_FLOAT_VALUE(x) ((x)->uval.fval.f)

#define ERL_ATOM_PTR(x) erl_atom_ptr_latin1((Erl_Atom_data*) &(x)->uval.aval.d)
#define ERL_ATOM_PTR_UTF8(x) erl_atom_ptr_utf8((Erl_Atom_data*) &(x)->uval.aval.d)
#define ERL_ATOM_SIZE(x) erl_atom_size_latin1((Erl_Atom_data*) &(x)->uval.aval.d)
#define ERL_ATOM_SIZE_UTF8(x) erl_atom_size_utf8((Erl_Atom_data*) &(x)->uval.aval.d)

#define ERL_PID_NODE(x) erl_atom_ptr_latin1((Erl_Atom_data*) &(x)->uval.pidval.node)
#define ERL_PID_NODE_UTF8(x) erl_atom_ptr_utf8((Erl_Atom_data*) &(x)->uval.pidval.node)
#define ERL_PID_NUMBER(x) ((x)->uval.pidval.number)
#define ERL_PID_SERIAL(x) ((x)->uval.pidval.serial)
#define ERL_PID_CREATION(x) ((x)->uval.pidval.creation)

#define ERL_PORT_NODE(x) erl_atom_ptr_latin1((Erl_Atom_data*) &(x)->uval.portval.node)
#define ERL_PORT_NODE_UTF8(x) erl_atom_ptr_utf8((Erl_Atom_data*) &(x)->uval.portval.node)
#define ERL_PORT_NUMBER(x) ((x)->uval.portval.number)
#define ERL_PORT_CREATION(x) ((x)->uval.portval.creation)

#define ERL_REF_NODE(x) erl_atom_ptr_latin1((Erl_Atom_data*) &(x)->uval.refval.node)
#define ERL_REF_NODE_UTF8(x) erl_atom_ptr_utf8((Erl_Atom_data*) &(x)->uval.refval.node)
#define ERL_REF_NUMBER(x) ((x)->uval.refval.n[0])
#define ERL_REF_NUMBERS(x) ((x)->uval.refval.n)
#define ERL_REF_LEN(x) ((x)->uval.refval.len)
#define ERL_REF_CREATION(x) ((x)->uval.refval.creation)

#define ERL_TUPLE_SIZE(x) ((x)->uval.tval.size)

/* NOTE!!! This is 0-based!! (first item is number 0)
 * Note too that element/2 (in Erlang) and
 * erl_element() are both 1-based.
 */
#define ERL_TUPLE_ELEMS(x) ((x)->uval.tval.elems)
#define ERL_TUPLE_ELEMENT(x, i) (ERL_TUPLE_ELEMS(x)[(i)])

#define ERL_BIN_SIZE(x) ((x)->uval.bval.size)
#define ERL_BIN_PTR(x) ((x)->uval.bval.b)

#define ERL_CONS_HEAD(x) ((x)->uval.lval.head)
#define ERL_CONS_TAIL(x) ((x)->uval.lval.tail)

#define ERL_VAR_LEN(x) ((x)->uval.vval.len)
#define ERL_VAR_NAME(x) ((x)->uval.vval.name)
#define ERL_VAR_VALUE(x) ((x)->uval.vval.v)

#define ERL_CLOSURE_SIZE(x)  ((x)->uval.funcval.size)
#define ERL_FUN_CREATOR(x)   ((x)->uval.funcval.creator)
#define ERL_FUN_MODULE(x)    ((x)->uval.funcval.module)
#define ERL_FUN_UNIQ(x)      ((x)->uval.funcval.uniq)
#define ERL_FUN_INDEX(x)     ((x)->uval.funcval.index)
#define ERL_FUN_ARITY(x)     ((x)->uval.funcval.arity)
#define ERL_FUN_NEW_INDEX(x) ((x)->uval.funcval.new_index)
#define ERL_FUN_MD5(x)       ((x)->uval.funcval.md5)
#define ERL_CLOSURE(x)       ((x)->uval.funcval.closure)
#define ERL_CLOSURE_ELEMENT(x,i) (ERL_CLOSURE(x)[(i)])


#ifdef __cplusplus
extern "C" {
#endif

/* -------------------------------------------------------------------- */
/*              Type definitions of Erlang terms in C                   */
/* -------------------------------------------------------------------- */

typedef struct {
  unsigned int count:24;	/* reference counter */
  unsigned int type:8;		/* type of Erlang term */
} Erl_Header;

typedef struct {
  Erl_Header h;
  int i;
} Erl_Integer;

typedef struct {
  Erl_Header h;
  unsigned int u;
} Erl_Uinteger;

typedef struct {
  Erl_Header h;
  long long i;
} Erl_LLInteger;

typedef struct {
  Erl_Header h;
  unsigned long long u;
} Erl_ULLInteger;

typedef struct {
  Erl_Header h;
  double f;
} Erl_Float;

typedef struct {
  char *utf8;
  int lenU;
  char *latin1;
  int lenL;
} Erl_Atom_data;

char* erl_atom_ptr_latin1(Erl_Atom_data*) EI_DEPRECATED_ATTR;
char* erl_atom_ptr_utf8(Erl_Atom_data*) EI_DEPRECATED_ATTR;
int erl_atom_size_latin1(Erl_Atom_data*) EI_DEPRECATED_ATTR;
int erl_atom_size_utf8(Erl_Atom_data*) EI_DEPRECATED_ATTR;
char* erl_atom_init_latin1(Erl_Atom_data*, const char*) EI_DEPRECATED_ATTR;

typedef struct {
  Erl_Header h;
  Erl_Atom_data d;
} Erl_Atom;

typedef struct {
  Erl_Header h;
  Erl_Atom_data node;
  unsigned int number;
  unsigned int serial;
  unsigned int creation;
} Erl_Pid;

typedef struct {    
  Erl_Header h;
  Erl_Atom_data node;
  unsigned int number;
  unsigned int creation;
} Erl_Port;

typedef struct {
  Erl_Header h;
  Erl_Atom_data node;
  int len;
  unsigned int n[3];
  unsigned int creation;
} Erl_Ref;

typedef struct {
  Erl_Header h;
  int arity;
  int is_neg;
  unsigned short *digits;
} Erl_Big;

struct _eterm; /* forward */    

typedef struct {
  Erl_Header h;
  struct _eterm *head;
  struct _eterm *tail;
} Erl_List;

typedef struct {
  Erl_Header h;
} Erl_EmptyList;

typedef struct {
  Erl_Header h;
  int size;
  struct _eterm **elems;
} Erl_Tuple;

typedef struct {
  Erl_Header h;
  int size;
  unsigned char *b;
} Erl_Binary;

/* Variables may only exist in patterns. 
 * Note: identical variable names in a pattern 
 * denotes the same value.
 */
typedef struct {    
  Erl_Header h;
  int len;           
  char *name;        
  struct _eterm *v;  
} Erl_Variable;


typedef struct {
    Erl_Header h;
    int size;			/* size of closure */
    int arity;			/* arity for new (post R7) external funs */
    unsigned char md5[16];	/* md5 for new funs */
    int new_index;		/* new funs */
    struct _eterm*  creator;	/* pid */
    struct _eterm*  module;	/* module */
    struct _eterm*  index;
    struct _eterm*  uniq;
    struct _eterm** closure;
} Erl_Function;

typedef struct _eterm {
  union {
    Erl_Integer    ival;
    Erl_Uinteger   uival; 
    Erl_LLInteger  llval;
    Erl_ULLInteger ullval;
    Erl_Float      fval;
    Erl_Atom       aval;
    Erl_Pid        pidval;     
    Erl_Port       portval;    
    Erl_Ref        refval;   
    Erl_List       lval;
    Erl_EmptyList  nval;
    Erl_Tuple      tval;
    Erl_Binary     bval;
    Erl_Variable   vval;
    Erl_Function   funcval;
    Erl_Big        bigval;
  } uval;
} ETERM;


#define MAXREGLEN (255*4)  /* max length of registered (atom) name */

typedef struct {
  int type;   /* one of the message type constants in eiext.h */
  ETERM *msg; /* the actual message */
  ETERM *from;
  ETERM *to;
  char to_name[MAXREGLEN+1];
} ErlMessage;

typedef unsigned char Erl_Heap;


/* -------------------------------------------------------------------- */
/*                          The functions                               */
/* -------------------------------------------------------------------- */

void   erl_init(void *x, long y) EI_DEPRECATED_ATTR;
void   erl_set_compat_rel(unsigned) EI_DEPRECATED_ATTR;
int    erl_connect_init(int, char*,short) EI_DEPRECATED_ATTR;
int    erl_connect_xinit(char*,char*,char*,struct in_addr*,char*,short) EI_DEPRECATED_ATTR;
int    erl_connect(char*) EI_DEPRECATED_ATTR; 
int    erl_xconnect(struct in_addr*,char *) EI_DEPRECATED_ATTR;
int    erl_close_connection(int) EI_DEPRECATED_ATTR;
int    erl_receive(int, unsigned char*, int) EI_DEPRECATED_ATTR;
int    erl_receive_msg(int, unsigned char*, int, ErlMessage*) EI_DEPRECATED_ATTR;
int    erl_xreceive_msg(int, unsigned char**, int*, ErlMessage*) EI_DEPRECATED_ATTR;
int    erl_send(int, ETERM*, ETERM*) EI_DEPRECATED_ATTR;
int    erl_reg_send(int, char*, ETERM*) EI_DEPRECATED_ATTR;
ETERM *erl_rpc(int,char*,char*,ETERM*) EI_DEPRECATED_ATTR;
int    erl_rpc_to(int,char*,char*,ETERM*) EI_DEPRECATED_ATTR;
int    erl_rpc_from(int,int,ErlMessage*) EI_DEPRECATED_ATTR;

/* erl_publish returns open descriptor on success, or -1 */
int    erl_publish(int port) EI_DEPRECATED_ATTR;
int    erl_accept(int,ErlConnect*) EI_DEPRECATED_ATTR;

const char *erl_thiscookie(void) EI_DEPRECATED_ATTR;
const char *erl_thisnodename(void) EI_DEPRECATED_ATTR;
const char *erl_thishostname(void) EI_DEPRECATED_ATTR;
const char *erl_thisalivename(void) EI_DEPRECATED_ATTR;
short       erl_thiscreation(void) EI_DEPRECATED_ATTR;

/* returns 0 on success, -1 if node not known to epmd or epmd not reached */
int    erl_unpublish(const char *alive) EI_DEPRECATED_ATTR;

#ifdef EI_HAVE_DEPRECATED_ATTR__
#define EI_DEPR_ATTR_EXTRA , EI_DEPRECATED_ATTR_NAME
#else
#define EI_DEPR_ATTR_EXTRA
#endif


/* Report generic error to stderr. */
void   erl_err_msg(const char * __template, ...)
       __attribute__ ((__format__ (printf, 1, 2) EI_DEPR_ATTR_EXTRA)) ;
/* Report generic error to stderr and die. */
void   erl_err_quit(const char * __template, ...)
       __attribute__ ((__format__ (printf, 1, 2), __noreturn__ EI_DEPR_ATTR_EXTRA));
/* Report system/libc error to stderr. */
void   erl_err_ret(const char * __template, ...)
       __attribute__ ((__format__ (printf, 1, 2) EI_DEPR_ATTR_EXTRA));
/* Report system/libc error to stderr and die. */
void   erl_err_sys(const char * __template, ...)
       __attribute__ ((__format__ (printf, 1, 2), __noreturn__ EI_DEPR_ATTR_EXTRA));

ETERM *erl_cons(ETERM*,ETERM*) EI_DEPRECATED_ATTR;
ETERM *erl_copy_term(const ETERM*) EI_DEPRECATED_ATTR;
ETERM *erl_element(int,const ETERM*) EI_DEPRECATED_ATTR;

ETERM *erl_hd(const ETERM*) EI_DEPRECATED_ATTR;
ETERM* erl_iolist_to_binary(const ETERM* term) EI_DEPRECATED_ATTR;
char*  erl_iolist_to_string(const ETERM* term) EI_DEPRECATED_ATTR;
int    erl_iolist_length(const ETERM*) EI_DEPRECATED_ATTR;
int    erl_length(const ETERM*) EI_DEPRECATED_ATTR;
ETERM *erl_mk_atom(const char*) EI_DEPRECATED_ATTR;
ETERM *erl_mk_binary(const char*,int) EI_DEPRECATED_ATTR;
ETERM *erl_mk_empty_list(void) EI_DEPRECATED_ATTR;
ETERM *erl_mk_estring(const char*, int) EI_DEPRECATED_ATTR;
ETERM *erl_mk_float(double) EI_DEPRECATED_ATTR;
ETERM *erl_mk_int(int) EI_DEPRECATED_ATTR;
ETERM *erl_mk_longlong(long long) EI_DEPRECATED_ATTR;
ETERM *erl_mk_list(ETERM**,int) EI_DEPRECATED_ATTR;
ETERM *erl_mk_pid(const char*,unsigned int,unsigned int,unsigned char) EI_DEPRECATED_ATTR;
ETERM *erl_mk_port(const char*,unsigned int,unsigned char) EI_DEPRECATED_ATTR;
ETERM *erl_mk_ref(const char*,unsigned int,unsigned char) EI_DEPRECATED_ATTR;
ETERM *erl_mk_long_ref(const char*,unsigned int,unsigned int,
		       unsigned int,unsigned char) EI_DEPRECATED_ATTR;
ETERM *erl_mk_string(const char*) EI_DEPRECATED_ATTR;
ETERM *erl_mk_tuple(ETERM**,int) EI_DEPRECATED_ATTR;
ETERM *erl_mk_uint(unsigned int) EI_DEPRECATED_ATTR;
ETERM *erl_mk_ulonglong(unsigned long long) EI_DEPRECATED_ATTR;
ETERM *erl_mk_var(const char*) EI_DEPRECATED_ATTR;
int    erl_print_term(FILE*,const ETERM*) EI_DEPRECATED_ATTR;
/* int    erl_sprint_term(char*,const ETERM*) EI_DEPRECATED_ATTR; */
int    erl_size(const ETERM*) EI_DEPRECATED_ATTR;
ETERM *erl_tl(const ETERM*) EI_DEPRECATED_ATTR;
ETERM *erl_var_content(const ETERM*, const char*) EI_DEPRECATED_ATTR;

ETERM *erl_format(char*, ... ) EI_DEPRECATED_ATTR;
int    erl_match(ETERM*, ETERM*) EI_DEPRECATED_ATTR;

char **erl_global_names(int fd, int *count) EI_DEPRECATED_ATTR;
int    erl_global_register(int fd, const char *name, ETERM *pid) EI_DEPRECATED_ATTR;
int    erl_global_unregister(int fd, const char *name) EI_DEPRECATED_ATTR;
ETERM *erl_global_whereis(int fd, const char *name, char *node) EI_DEPRECATED_ATTR;

void   erl_init_malloc(Erl_Heap*,long) EI_DEPRECATED_ATTR;
ETERM *erl_alloc_eterm(unsigned char) EI_DEPRECATED_ATTR;
void   erl_eterm_release(void) EI_DEPRECATED_ATTR;
void   erl_eterm_statistics(unsigned long*,unsigned long*) EI_DEPRECATED_ATTR;
void   erl_free_array(ETERM**,int) EI_DEPRECATED_ATTR;
void   erl_free_term(ETERM*) EI_DEPRECATED_ATTR;
void   erl_free_compound(ETERM*) EI_DEPRECATED_ATTR;
void  *erl_malloc(long) EI_DEPRECATED_ATTR;
void   erl_free(void*) EI_DEPRECATED_ATTR;

int    erl_compare_ext(unsigned char*, unsigned char*) EI_DEPRECATED_ATTR;
ETERM *erl_decode(unsigned char*) EI_DEPRECATED_ATTR;
ETERM *erl_decode_buf(unsigned char**) EI_DEPRECATED_ATTR;
int    erl_encode(ETERM*,unsigned char*t) EI_DEPRECATED_ATTR;
int    erl_encode_buf(ETERM*,unsigned char**) EI_DEPRECATED_ATTR;
int    erl_ext_size(unsigned char*) EI_DEPRECATED_ATTR;
unsigned char erl_ext_type(unsigned char*) EI_DEPRECATED_ATTR; /* Note: returned 'char' before R9C */
unsigned char *erl_peek_ext(unsigned char*,int) EI_DEPRECATED_ATTR;
int    erl_term_len(ETERM*) EI_DEPRECATED_ATTR;

int cmp_latin1_vs_utf8(const char* sL, int lenL, const char* sU, int lenU) EI_DEPRECATED_ATTR;

/* -------------------------------------------------------------------- */
/*                      Wrappers around ei functions                    */
/* -------------------------------------------------------------------- */

/* 
 * Undocumented before R9C, included for compatibility with old code
 */

struct hostent *erl_gethostbyname(const char *name) EI_DEPRECATED_ATTR;
struct hostent *erl_gethostbyaddr(const char *addr, int len, int type) EI_DEPRECATED_ATTR;
struct hostent *erl_gethostbyname_r(const char *name, 
				    struct hostent *hostp, 
				    char *buffer, 
				    int buflen, 
				    int *h_errnop) EI_DEPRECATED_ATTR;
struct hostent *erl_gethostbyaddr_r(const char *addr,
				    int length, 
				    int type, 
				    struct hostent *hostp,
				    char *buffer,  
				    int buflen, 
				    int *h_errnop) EI_DEPRECATED_ATTR;

/* 
 * Undocumented, included for compatibility with old code
 */

void erl_init_resolve(void) EI_DEPRECATED_ATTR;
int erl_distversion(int fd) EI_DEPRECATED_ATTR;
int erl_epmd_connect(struct in_addr *inaddr) EI_DEPRECATED_ATTR;
int erl_epmd_port(struct in_addr *inaddr, const char *alive, int *dist) EI_DEPRECATED_ATTR;

#ifdef __cplusplus
}
#endif

#endif
