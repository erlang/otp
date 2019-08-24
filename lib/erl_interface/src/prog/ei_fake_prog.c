/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

/***************************************************************************
 *
 *  This is a fake program that contains all functions, variables and
 *  defined symbols mentioned in the manual. We compile this file to see
 *  that the header files and created library is complete.
 *
 *  You can't run this program, it is for compiling and linking only.
 *
 ***************************************************************************/

/* This is a link and header file test. Including "ei.h" and linking
   with libei.a should be enough */

/* Use most of
 * CFLAGS="-I../include -g -O2
 *         -ansi -pedantic
 *         -Wall
 *         -Wshadow
 *         -Wstrict-prototypes
 *         -Wmissing-prototypes
 *         -Wmissing-declarations
 *         -Wnested-externs
 *         -Winline
 *         -Werror"
 */

/* An exception from using eidef.h, use config.h directly */
#include "config.h"

#if defined(HAVE_GMP_H) && defined(HAVE_LIBGMP)
#include <gmp.h>
#endif /* HAVE_GMP_H && HAVE_LIBGMP */

/* #include <netdb.h> now included by ei.h */
#include "ei.h"

#ifdef VXWORKS
int ei_fake_prog_main(void)
#else
int main(void)
#endif
{
  ErlConnect conp;
  Erl_IpAddr thisipaddr = (Erl_IpAddr)0;
  FILE *fp = (FILE *)0;
  char* charp = "foo";
  double *doublep = NULL;
  double doublex = 0.0;
  ei_cnode xec;
  ei_reg *ei_regp = NULL;
  ei_term eterm;
  ei_x_buff eix;
  erlang_big *bigp = NULL;
  erlang_fun efun;
  erlang_msg *msgp = NULL;
  erlang_msg emsg;
  erlang_pid *pidp = NULL;
  erlang_pid epid;
  erlang_port eport;
  erlang_ref eref;
  erlang_trace etrace;
  int *intp = NULL;
  int intx = 0;
  long *longp = NULL;
  long longx = 0;
  short creation = 0;
  struct ei_reg_stat *ei_reg_statp = NULL;
  struct ei_reg_tabstat *ei_reg_tabstatp = NULL;
  struct hostent *hostp = NULL;
  unsigned char * ucharp = (unsigned char *)"foo";
  unsigned long *ulongp = NULL;
  unsigned long ulongx = 0;
  void *voidp = NULL;
#ifndef VXWORKS
  EI_LONGLONG *longlongp = (EI_LONGLONG*)NULL;
  EI_LONGLONG longlongx = 0;
  EI_ULONGLONG *ulonglongp = (EI_ULONGLONG*)NULL;
  EI_ULONGLONG ulonglongx = 0;
#endif
  erlang_char_encoding enc;
  ei_socket_callbacks cbs;

  intx = erl_errno;

  ei_init();

  ei_close_connection(intx);
  
  ei_connect_init(&xec, charp, charp, creation);
  ei_connect_init_ussi(&xec, charp, charp, creation, &cbs, sizeof(cbs), NULL);
  ei_connect_xinit (&xec, charp, charp, charp, thisipaddr, charp, creation);
  ei_connect_xinit_ussi(&xec, charp, charp, charp, thisipaddr, charp, creation, &cbs, sizeof(cbs), NULL);

  ei_connect(&xec, charp);
  ei_xconnect (&xec, thisipaddr, charp);

  ei_receive(intx, ucharp, intx);
  ei_receive_msg(intx, &emsg, &eix);
  ei_xreceive_msg(intx, &emsg, &eix);

  ei_send(intx, &epid, charp, intx);
  ei_reg_send(&xec, intx, charp, charp, intx);

  ei_rpc(&xec, intx, charp, charp, charp, intx, &eix);
  ei_rpc_to(&xec, intx, charp, charp, charp, intx);
  ei_rpc_from(&xec, intx, intx, &emsg, &eix);

  ei_publish(&xec, intx);
  ei_accept(&xec, intx, &conp);
  ei_unpublish(&xec);
  ei_listen(&xec, intp, intx);
  ei_xlisten(&xec, thisipaddr, intp, intx);

  ei_thisnodename(&xec);
  ei_thishostname(&xec);
  ei_thisalivename(&xec);

  ei_self(&xec);

  ei_gethostbyname(charp);
  ei_gethostbyaddr(charp, intx, intx);
  ei_gethostbyname_r(charp, hostp, charp, intx, intp);
  ei_gethostbyaddr_r(charp, intx, intx, hostp, charp, intx, intp);

  ei_encode_version(charp, intp);
  ei_x_encode_version(&eix);
  ei_encode_long(charp, intp, longx);
  ei_x_encode_long(&eix, longx);
  ei_encode_ulong(charp, intp, ulongx);
  ei_x_encode_ulong(&eix, ulongx);
  ei_encode_double(charp, intp, doublex);
  ei_x_encode_double(&eix, doublex);
  ei_encode_boolean(charp, intp, intx);
  ei_x_encode_boolean(&eix, intx);
  ei_encode_char(charp, intp, 'a');
  ei_x_encode_char(&eix, 'a');
  ei_encode_string(charp, intp, charp);
  ei_encode_string_len(charp, intp, charp, intx);
  ei_x_encode_string(&eix, charp);
  ei_x_encode_string_len(&eix, charp, intx);
  ei_encode_atom(charp, intp, charp);
  ei_encode_atom_as(charp, intp, charp, ERLANG_LATIN1, ERLANG_UTF8);
  ei_encode_atom_len(charp, intp, charp, intx);
  ei_encode_atom_len_as(charp, intp, charp, intx, ERLANG_ASCII, ERLANG_LATIN1);
  ei_x_encode_atom(&eix, charp);
  ei_x_encode_atom_as(&eix, charp, ERLANG_LATIN1, ERLANG_UTF8);
  ei_x_encode_atom_len(&eix, charp, intx);
  ei_x_encode_atom_len_as(&eix, charp, intx, ERLANG_LATIN1, ERLANG_UTF8);
  ei_encode_binary(charp, intp, (void *)0, longx);
  ei_x_encode_binary(&eix, (void*)0, intx);
  ei_encode_pid(charp, intp, &epid);
  ei_x_encode_pid(&eix, &epid);
  ei_encode_fun(charp, intp, &efun);
  ei_x_encode_fun(&eix, &efun);
  ei_encode_port(charp, intp, &eport);
  ei_x_encode_port(&eix, &eport);
  ei_encode_ref(charp, intp, &eref);
  ei_x_encode_ref(&eix, &eref);
  ei_encode_trace(charp, intp, &etrace);
  ei_x_encode_trace(&eix, &etrace);
  ei_encode_tuple_header(charp, intp, intx);
  ei_x_encode_tuple_header(&eix, longx);
  ei_encode_list_header(charp, intp, intx);
  ei_x_encode_list_header(&eix, longx);
/* #define ei_encode_empty_list(buf,i) ei_encode_list_header(buf,i,0) */
  ei_x_encode_empty_list(&eix);

  ei_get_type(charp, intp, intp, intp);

  ei_decode_version(charp, intp, intp);
  ei_decode_long(charp, intp, longp);
  ei_decode_ulong(charp, intp, ulongp);
  ei_decode_double(charp, intp, doublep);
  ei_decode_boolean(charp, intp, intp);
  ei_decode_char(charp, intp, charp);
  ei_decode_string(charp, intp, charp);
  ei_decode_atom(charp, intp, charp);
  ei_decode_atom_as(charp, intp, charp, MAXATOMLEN_UTF8, ERLANG_UTF8, &enc, &enc);
  ei_decode_binary(charp, intp, (void *)0, longp);
  ei_decode_fun(charp, intp, &efun);
  free_fun(&efun);
  ei_decode_pid(charp, intp, &epid);
  ei_decode_port(charp, intp, &eport);
  ei_decode_ref(charp, intp, &eref);
  ei_decode_trace(charp, intp, &etrace);
  ei_decode_tuple_header(charp, intp, intp);
  ei_decode_list_header(charp, intp, intp);

  ei_decode_ei_term(charp, intp, &eterm);

  ei_print_term(fp, charp, intp);
  ei_s_print_term(&charp, charp, intp);

  ei_x_format(&eix, charp);
  ei_x_format_wo_ver(&eix, charp);

  ei_x_new(&eix);
  ei_x_new_with_version(&eix);
  ei_x_free(&eix);
  ei_x_append(&eix, &eix);
  ei_x_append_buf(&eix, charp, intx);
  ei_skip_term(charp, intp);

  ei_reg_open(intx);
  ei_reg_resize(ei_regp, intx);
  ei_reg_close(ei_regp);

  ei_reg_setival(ei_regp, charp, longx);
  ei_reg_setfval(ei_regp, charp, doublex);
  ei_reg_setsval(ei_regp, charp, charp);
  ei_reg_setpval(ei_regp, charp, voidp, intx);

  ei_reg_setval(ei_regp, charp, intx);

  ei_reg_getival(ei_regp, charp);
  ei_reg_getfval(ei_regp, charp);
  ei_reg_getsval(ei_regp, charp);
  ei_reg_getpval(ei_regp, charp, intp);

  ei_reg_getval(ei_regp, charp, intx);

  ei_reg_markdirty(ei_regp, charp);

  ei_reg_delete(ei_regp, charp);

  ei_reg_stat(ei_regp, charp, ei_reg_statp);

  ei_reg_tabstat(ei_regp, ei_reg_tabstatp);

  ei_reg_dump(intx, ei_regp, charp, intx);
  ei_reg_restore(intx, ei_regp, charp);
  ei_reg_purge(ei_regp);

#if defined(HAVE_GMP_H) && defined(HAVE_LIBGMP)
  {
      mpz_t obj;
      ei_decode_bignum(charp, intp, obj);
      ei_encode_bignum(charp, intp, obj);
      ei_x_encode_bignum(&eix, obj);
  }
#endif /* HAVE_GMP_H && HAVE_LIBGMP */

#ifndef VXWORKS

  ei_decode_longlong(charp, intp, longlongp);
  ei_decode_ulonglong(charp, intp, ulonglongp);
  ei_encode_longlong(charp, intp, longlongx);
  ei_encode_ulonglong(charp, intp, ulonglongx);
  ei_x_encode_longlong(&eix, longlongx);
  ei_x_encode_ulonglong(&eix, ulonglongx);

#endif

#ifdef USE_EI_UNDOCUMENTED

  ei_decode_intlist(charp, intp, longp, intp);

  ei_receive_encoded(intx, &charp, intp, msgp, intp);
  ei_send_encoded(intx, pidp, charp, intx);
  ei_send_reg_encoded(intx, pidp, charp, charp, intx);

  ei_decode_big(charp, intp, bigp);
  ei_big_comp(bigp, bigp);
  ei_big_to_double(bigp, doublep);
  ei_small_to_big(intx, bigp);
  ei_alloc_big(intx);
  ei_free_big(bigp);

#endif /* USE_EI_UNDOCUMENTED */

  return
      BUFSIZ +
      EAGAIN +
      EHOSTUNREACH +
      EIO +
      EI_BIN +
      EI_DELET +
      EI_DIRTY +
      EI_FLT +
      EI_FORCE +
      EI_INT +
      EI_NOPURGE +
      EI_STR +
      EMSGSIZE +
      ENOMEM +
      ERL_ERROR +
      ERL_EXIT +
      ERL_LINK +
      ERL_MSG +
      ERL_NO_TIMEOUT +
      ERL_REG_SEND +
      ERL_SEND +
      ERL_TICK +
      ERL_TIMEOUT +
      ERL_UNLINK +
      ETIMEDOUT +
      MAXATOMLEN;
}
