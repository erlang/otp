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

/* #include <netdb.h> now included by ei.h */
#include "erl_interface.h"

#ifdef VXWORKS
int erl_fake_prog_main(void)
#else
int main(void)
#endif
{
  ei_x_buff eix;
  int index = 0;
  ETERM **etermpp = NULL, *etermp = NULL;
  char *charp = NULL;
  unsigned char uchar, **ucharpp = NULL, *ucharp = NULL;
  void *voidp = NULL;
  Erl_Heap *erl_heapp = NULL;
  int intx = 0;
  int *intp = NULL;
  unsigned int uintx, *uintp;
  unsigned long *ulongp = NULL;
  long longx = 0;
  double doublex = 0.0;
  short shortx = 42;
  FILE *filep = NULL;
  Erl_IpAddr erl_ipaddr = NULL;
  ErlMessage *erlmessagep = NULL;
  ErlConnect *erlconnectp = NULL;
  struct hostent *hostp = NULL;
  struct in_addr *inaddrp = NULL;

  /* Converion to erl_interface format is in liberl_interface */

  intx = erl_errno;

  ei_encode_term(charp, &index, voidp);
  ei_x_encode_term(&eix, voidp);
  ei_decode_term(charp, &index, voidp);

  erl_init(voidp, longx);
  erl_connect_init(intx, charp,shortx);
  erl_connect_xinit(charp,charp,charp,erl_ipaddr,charp,shortx);
  erl_connect(charp); 
  erl_xconnect(erl_ipaddr,charp);
  erl_close_connection(intx);
  erl_receive(intx, ucharp, intx);
  erl_receive_msg(intx, ucharp, intx, erlmessagep);
  erl_xreceive_msg(intx, ucharpp, intp, erlmessagep);
  erl_send(intx, etermp, etermp);
  erl_reg_send(intx, charp, etermp);
  erl_rpc(intx,charp,charp,etermp);
  erl_rpc_to(intx,charp,charp,etermp);
  erl_rpc_from(intx,intx,erlmessagep);

  erl_publish(intx);
  erl_accept(intx,erlconnectp);

  erl_thiscookie();
  erl_thisnodename();
  erl_thishostname();
  erl_thisalivename();
  erl_thiscreation();
  erl_unpublish(charp);
  erl_err_msg(charp);
  erl_err_quit(charp);
  erl_err_ret(charp);
  erl_err_sys(charp);

  erl_cons(etermp,etermp);
  erl_copy_term(etermp);
  erl_element(intx,etermp);

  erl_hd(etermp);
  erl_iolist_to_binary(etermp);
  erl_iolist_to_string(etermp);
  erl_iolist_length(etermp);
  erl_length(etermp);
  erl_mk_atom(charp);
  erl_mk_binary(charp,intx);
  erl_mk_empty_list();
  erl_mk_estring(charp, intx);
  erl_mk_float(doublex);
  erl_mk_int(intx);
  erl_mk_list(etermpp,intx);
  erl_mk_pid(charp,uintx,uintx,uchar);
  erl_mk_port(charp,uintx,uchar);
  erl_mk_ref(charp,uintx,uchar);
  erl_mk_long_ref(charp,uintx,uintx,uintx,uchar);
  erl_mk_string(charp);
  erl_mk_tuple(etermpp,intx);
  erl_mk_uint(uintx);
  erl_mk_var(charp);
  erl_print_term(filep,etermp);
  /*  erl_sprint_term(charp,etermp); */
  erl_size(etermp);
  erl_tl(etermp);
  erl_var_content(etermp, charp);

  erl_format(charp);
  erl_match(etermp, etermp);

  erl_global_names(intx, intp);
  erl_global_register(intx, charp, etermp);
  erl_global_unregister(intx, charp);
  erl_global_whereis(intx, charp, charp);

  erl_init_malloc(erl_heapp,longx);
  erl_alloc_eterm(uchar);
  erl_eterm_release();
  erl_eterm_statistics(ulongp,ulongp);
  erl_free_array(etermpp,intx);
  erl_free_term(etermp);
  erl_free_compound(etermp);
  erl_malloc(longx);
  erl_free(voidp);

  erl_compare_ext(ucharp, ucharp);
  erl_decode(ucharp);
  erl_decode_buf(ucharpp);
  erl_encode(etermp,ucharp);
  erl_encode_buf(etermp,ucharpp);
  erl_ext_size(ucharp);
  erl_ext_type(ucharp);
  erl_peek_ext(ucharp,intx);
  erl_term_len(etermp);

  erl_gethostbyname(charp);
  erl_gethostbyaddr(charp, intx, intx);
  erl_gethostbyname_r(charp, hostp, charp, intx, intp);
  erl_gethostbyaddr_r(charp, intx, intx, hostp, charp, intx, intp);

  erl_init_resolve();
  erl_distversion(intx);

  erl_epmd_connect(inaddrp);
  erl_epmd_port(inaddrp, charp, intp);

  charp  = ERL_ATOM_PTR(etermp);
  intx   = ERL_ATOM_SIZE(etermp);
  ucharp = ERL_BIN_PTR(etermp);
  intx   = ERL_BIN_SIZE(etermp);
  etermp = ERL_CONS_HEAD(etermp);
  etermp = ERL_CONS_TAIL(etermp);
  intx   = ERL_COUNT(etermp);
  doublex= ERL_FLOAT_VALUE(etermp);
  uintx  = ERL_INT_UVALUE(etermp);
  intx   = ERL_INT_VALUE(etermp);
  intx   = ERL_IS_ATOM(etermp);
  intx   = ERL_IS_BINARY(etermp);
  intx   = ERL_IS_CONS(etermp);
  intx   = ERL_IS_EMPTY_LIST(etermp);
  intx   = ERL_IS_FLOAT(etermp);
  intx   = ERL_IS_INTEGER(etermp);
  intx   = ERL_IS_LIST(etermp);
  intx   = ERL_IS_PID(etermp);
  intx   = ERL_IS_PORT(etermp);
  intx   = ERL_IS_REF(etermp);
  intx   = ERL_IS_TUPLE(etermp);
  intx   = ERL_IS_UNSIGNED_INTEGER(etermp);
  uchar  = ERL_PID_CREATION(etermp);
  charp  = ERL_PID_NODE(etermp);
  uintx  = ERL_PID_NUMBER(etermp);
  uintx  = ERL_PID_SERIAL(etermp);
  uchar  = ERL_PORT_CREATION(etermp);
  charp  = ERL_PORT_NODE(etermp);
  uintx  = ERL_PORT_NUMBER(etermp);
  uchar  = ERL_REF_CREATION(etermp);
  intx   = ERL_REF_LEN(etermp);
  charp  = ERL_REF_NODE(etermp);
  uintx  = ERL_REF_NUMBER(etermp);
  uintp  = ERL_REF_NUMBERS(etermp);
  etermp = ERL_TUPLE_ELEMENT(etermp,intx);
  intx   = ERL_TUPLE_SIZE(etermp);

  return 
      BUFSIZ +
      EAGAIN +
      EHOSTUNREACH +
      EINVAL +
      EIO +
      EMSGSIZE +
      ENOMEM +
      ERL_ATOM +
      ERL_BINARY +
      ERL_ERROR +
      ERL_EXIT +
      ERL_FLOAT +
      ERL_INTEGER +
      ERL_LINK +
      ERL_LIST +
      ERL_MSG +
      ERL_NO_TIMEOUT +
      ERL_PID +
      ERL_PORT +
      ERL_REF +
      ERL_REG_SEND +
      ERL_SEND +
      ERL_SMALL_BIG +
      ERL_TICK +
      ERL_TIMEOUT +
      ERL_TUPLE +
      ERL_UNLINK +
      ERL_U_INTEGER +
      ERL_U_SMALL_BIG +
      ERL_VARIABLE +
      ETIMEDOUT +
      MAXNODELEN +
      MAXREGLEN;
}
