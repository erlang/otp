/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2025. All Rights Reserved.
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
 *
 * ----------------------------------------------------------------------
 *  Purpose : UNIX version of synchronous I/O backend.
 * ----------------------------------------------------------------------
 *
 * essio = ESock Synchronous I/O
 *
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#ifdef ESOCK_ENABLE

/* If we HAVE_SCTP_H and Solaris, we need to define the following in
 * order to get SCTP working:
 */
#if (defined(HAVE_SCTP_H) && defined(__sun) && defined(__SVR4))
#define SOLARIS10    1
/* WARNING: This is not quite correct, it may also be Solaris 11! */
#define _XPG4_2
#define __EXTENSIONS__
#endif

#ifdef HAVE_SENDFILE
#if defined(__linux__) || (defined(__sun) && defined(__SVR4))
    #include <sys/sendfile.h>
#elif defined(__FreeBSD__) || defined(__DragonFly__)
    /* Need to define __BSD_VISIBLE in order to expose prototype
     * of sendfile in sys/socket.h
     */
    #define __BSD_VISIBLE 1
#endif
#endif

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"

#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#include <net/if.h>

#include "prim_socket_int.h"
#include "socket_util.h"
#include "socket_io.h"
#include "socket_syncio.h"
#include "socket_tarray.h"
#include "prim_file_nif_dyncall.h"


/* ======================================================================== *
 *                               Socket wrappers                            *
 * ======================================================================== *
 */

#ifdef HAS_ACCEPT4
// We have to figure out what the flags are...
#define sock_accept(s, addr, len)       \
    accept4((s), (addr), (len), (SOCK_CLOEXEC))
#else
#define sock_accept(s, addr, len)       accept((s), (addr), (len))
#endif
#define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_close(s)                   close((s))
// #define sock_close_event(e)             /* do nothing */
#define sock_connect(s, addr, len)      connect((s), (addr), (len))
#define sock_errno()                    errno
// #define sock_listen(s, b)               listen((s), (b))
// #define sock_name(s, addr, len)         getsockname((s), (addr), (len))
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_peer(s, addr, len)         getpeername((s), (addr), (len))
#define sock_recv(s,buf,len,flag)       recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
    recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_recvmsg(s,msghdr,flag)     recvmsg((s),(msghdr),(flag))
#define sock_send(s,buf,len,flag)       send((s), (buf), (len), (flag))
#define sock_sendmsg(s,msghdr,flag)     sendmsg((s),(msghdr),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
    sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendv(s,iov,iovcnt)        writev((s), (iov), (iovcnt))
#define sock_shutdown(s, how)           shutdown((s), (how))


/* =================================================================== *
 *                                                                     *
 *                        Various essio macros                         *
 *                                                                     *
 * =================================================================== */

/* Global socket debug */
#define SGDBG( proto )            ESOCK_DBG_PRINTF( ctrl.dbg , proto )


/* =================================================================== *
 *                                                                     *
 *                            Local types                              *
 *                                                                     *
 * =================================================================== */

typedef struct {
    /* Misc stuff */
    BOOLEAN_T      dbg;
    BOOLEAN_T      sockDbg;
} ESSIOControl;



/* ======================================================================== *
 *                          Function Forwards                               *
 * ======================================================================== *
 */
static BOOLEAN_T open_todup(ErlNifEnv*   env,
                            ERL_NIF_TERM eopts);
static BOOLEAN_T open_which_domain(SOCKET sock,   int* domain);
static BOOLEAN_T open_which_type(SOCKET sock,     int* type);
static BOOLEAN_T open_get_domain(ErlNifEnv*   env,
                                 ERL_NIF_TERM eopts,
                                 int*         domain);
static BOOLEAN_T open_get_type(ErlNifEnv*   env,
                               ERL_NIF_TERM eopts,
                               int*         type);
static BOOLEAN_T open_get_protocol(ErlNifEnv*   env,
                                   ERL_NIF_TERM eopts,
                                   int*         protocol);

#ifdef HAVE_SETNS
static BOOLEAN_T open_get_netns(ErlNifEnv*   env,
                                ERL_NIF_TERM opts,
                                char**       netns);
static BOOLEAN_T change_network_namespace(BOOLEAN_T dbg,
                                          char* netns, int* cns, int* err);
static BOOLEAN_T restore_network_namespace(BOOLEAN_T dbg,
                                           int ns, SOCKET sock, int* err);
#endif

static BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err);

static ERL_NIF_TERM essio_cancel_accept_current(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM essio_cancel_accept_waiting(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     opRef,
                                                const ErlNifPid* selfP);
static ERL_NIF_TERM essio_cancel_send_current(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM essio_cancel_send_waiting(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     opRef,
                                              const ErlNifPid* selfP);
static ERL_NIF_TERM essio_cancel_recv_current(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM essio_cancel_recv_waiting(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     opRef,
                                              const ErlNifPid* selfP);

static ERL_NIF_TERM essio_accept_listening_error(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     sockRef,
                                                 ERL_NIF_TERM     accRef,
                                                 ErlNifPid        caller,
                                                 int              save_errno);
static ERL_NIF_TERM essio_accept_listening_accept(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  SOCKET           accSock,
                                                  ErlNifPid        caller);
static ERL_NIF_TERM essio_accept_accepting_current(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   ERL_NIF_TERM     ref);
static
ERL_NIF_TERM essio_accept_accepting_current_accept(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   SOCKET           accSock);
static
ERL_NIF_TERM essio_accept_accepting_current_error(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  ERL_NIF_TERM     opRef,
                                                  int              save_errno);
static ERL_NIF_TERM essio_accept_accepting_other(ErlNifEnv*       env,
						 ESockDescriptor* descP,
						 ERL_NIF_TERM     ref,
						 ErlNifPid        caller);
static ERL_NIF_TERM essio_accept_busy_retry(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef,
                                            ErlNifPid*       pidP);
static BOOLEAN_T essio_accept_accepted(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       SOCKET           accSock,
                                       ErlNifPid        pid,
                                       ERL_NIF_TERM*    result);

static BOOLEAN_T send_check_writer(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     ref,
                                   ERL_NIF_TERM*    checkResult);
static ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ssize_t          send_result,
                                      ssize_t          dataSize,
                                      BOOLEAN_T        dataInTail,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     sendRef);
static ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ssize_t          written,
                                  ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              saveErrno,
                                    ERL_NIF_TERM     sockRef);
static void send_error_waiting_writers(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     reason);
static ERL_NIF_TERM send_check_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ssize_t          written,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     sendRef);

static BOOLEAN_T decode_cmsghdrs(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     eCMsg,
                                 char*            cmsgHdrBufP,
                                 size_t           cmsgHdrBufLen,
                                 size_t*          cmsgHdrBufUsed);
static BOOLEAN_T decode_cmsghdr(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eCMsg,
                                char*            bufP,
                                size_t           rem,
                                size_t*          used);
static BOOLEAN_T decode_cmsghdr_value(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      ERL_NIF_TERM     eType,
                                      ERL_NIF_TERM     eValue,
                                      char*            dataP,
                                      size_t           dataLen,
                                      size_t*          dataUsedP);
static BOOLEAN_T decode_cmsghdr_data(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     ERL_NIF_TERM     eType,
                                     ERL_NIF_TERM     eData,
                                     char*            dataP,
                                     size_t           dataLen,
                                     size_t*          dataUsedP);

static void encode_msg(ErlNifEnv*       env,
                       ESockDescriptor* descP,
                       ssize_t          read,
                       struct msghdr*   msgHdrP,
                       ErlNifBinary*    dataBufP,
                       ErlNifBinary*    ctrlBufP,
                       ERL_NIF_TERM*    eMsg);
static void encode_cmsgs(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ErlNifBinary*    cmsgBinP,
                         struct msghdr*   msgHdrP,
                         ERL_NIF_TERM*    eCMsg);

#if defined(HAVE_SENDFILE)
static int essio_sendfile(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          off_t            offset,
                          size_t*          countP,
                          int*             errP);
static ERL_NIF_TERM essio_sendfile_errno(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef,
                                         int              err);
static ERL_NIF_TERM essio_sendfile_error(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     reason);
static ERL_NIF_TERM essio_sendfile_select(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     sendRef,
                                          size_t           count);
static ERL_NIF_TERM essio_sendfile_ok(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      size_t           count);
#endif

static BOOLEAN_T recv_check_entry(ErlNifEnv       *env,
                                  ESockDescriptor *descP,
                                  ERL_NIF_TERM     recvRef,
                                  ERL_NIF_TERM    *retP);
static BOOLEAN_T recv_check_result(ErlNifEnv       *env,
                                   ESockDescriptor *descP,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef,
                                   ssize_t          readResult,
                                   int              saveErrno,
                                   ERL_NIF_TERM    *retP);
static ERL_NIF_TERM recv_check_full(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef,
                                    ssize_t          len,
                                    ErlNifBinary    *bufP);
static ERL_NIF_TERM recv_check_full_maybe_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     sockRef,
                                               ERL_NIF_TERM     recvRef,
                                               ErlNifBinary    *bufP);
static ERL_NIF_TERM recv_check_full_done(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     recvRef,
                                         ErlNifBinary    *bufP);
static ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef,
                                    int              saveErrno);
static ERL_NIF_TERM recv_check_fail_gen(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        int              saveErrno);
static ERL_NIF_TERM recv_check_fail_econnreset(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recv_check_select(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     recvRef,
                                      ERL_NIF_TERM     msg);
static ERL_NIF_TERM recv_check_partial(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     recvRef,
                                       ssize_t          len,
                                       ErlNifBinary    *bufP);
static void recv_init_current_reader(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     recvRef);
static void recv_update_current_reader(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef);
static void recv_error_current_reader(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     reason);

static ERL_NIF_TERM essio_ioctl_gifconf(ErlNifEnv*       env,
					ESockDescriptor* descP);
/* esock_ioctl_fionread */
#if defined(FIONREAD)
#define IOCTL_FIONREAD_FUNC2_DEF IOCTL_GET_FUNC2_DEF(fionread)
#else
#define IOCTL_FIONREAD_FUNC2_DEF
#endif

/* esock_ioctl_fionwrite */
#if defined(FIONWRITE)
#define IOCTL_FIONWRITE_FUNC2_DEF IOCTL_GET_FUNC2_DEF(fionwrite)
#else
#define IOCTL_FIONWRITE_FUNC2_DEF
#endif

/* esock_ioctl_fionspace */
#if defined(FIONSPACE)
#define IOCTL_FIONSPACE_FUNC2_DEF IOCTL_GET_FUNC2_DEF(fionspace)
#else
#define IOCTL_FIONSPACE_FUNC2_DEF
#endif

/* esock_ioctl_siocatmark */
#if defined(SIOCATMARK)
#define IOCTL_SIOCATMARK_FUNC2_DEF IOCTL_GET_FUNC2_DEF(siocatmark)
#else
#define IOCTL_SIOCATMARK_FUNC2_DEF
#endif

#define IOCTL_GET_FUNCS2_DEF			\
    IOCTL_FIONREAD_FUNC2_DEF;			\
    IOCTL_FIONWRITE_FUNC2_DEF;			\
    IOCTL_FIONSPACE_FUNC2_DEF;			\
    IOCTL_SIOCATMARK_FUNC2_DEF;
#define IOCTL_GET_FUNC2_DEF(F)					\
    static ERL_NIF_TERM essio_ioctl_##F(ErlNifEnv*       env,	\
                                        ESockDescriptor* descP)
IOCTL_GET_FUNCS2_DEF
#undef IOCTL_GET_FUNC2_DEF

#if defined(SIOCGIFNAME)
static ERL_NIF_TERM essio_ioctl_gifname(ErlNifEnv*       env,
					ESockDescriptor* descP,
					ERL_NIF_TERM     eidx);
#endif

/* esock_ioctl_gifindex */
#if defined(SIOCGIFINDEX)
#define IOCTL_GIFINDEX_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifindex)
#else
#define IOCTL_GIFINDEX_FUNC3_DEF
#endif

/* esock_ioctl_gifflags */
#if defined(SIOCGIFFLAGS)
#define IOCTL_GIFFLAGS_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifflags)
#else
#define IOCTL_GIFFLAGS_FUNC3_DEF
#endif

/* esock_ioctl_gifaddr */
#if defined(SIOCGIFADDR)
#define IOCTL_GIFADDR_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifaddr)
#else
#define IOCTL_GIFADDR_FUNC3_DEF
#endif

/* esock_ioctl_gifdstaddr */
#if defined(SIOCGIFDSTADDR)
#define IOCTL_GIFDSTADDR_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifdstaddr)
#else
#define IOCTL_GIFDSTADDR_FUNC3_DEF
#endif

/* esock_ioctl_gifbrdaddr */
#if defined(SIOCGIFBRDADDR)
#define IOCTL_GIFBRDADDR_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifbrdaddr)
#else
#define IOCTL_GIFBRDADDR_FUNC3_DEF
#endif

/* esock_ioctl_gifnetmask */
#if defined(SIOCGIFNETMASK)
#define IOCTL_GIFNETMASK_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifnetmask)
#else
#define IOCTL_GIFNETMASK_FUNC3_DEF
#endif

/* esock_ioctl_gifmtu */
#if defined(SIOCGIFMTU)
#define IOCTL_GIFMTU_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifmtu)
#else
#define IOCTL_GIFMTU_FUNC3_DEF
#endif

/* esock_ioctl_gifhwaddr */
#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
#define IOCTL_GIFHWADDR_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifhwaddr)
#else
#define IOCTL_GIFHWADDR_FUNC3_DEF
#endif

/* esock_ioctl_gifenaddr */
#if defined(SIOCGENADDR) && defined(ESOCK_USE_ENADDR)
#define IOCTL_GENADDR_FUNC3_DEF IOCTL_GET_FUNC3_DEF(genaddr)
#else
#define IOCTL_GENADDR_FUNC3_DEF
#endif

/* esock_ioctl_gifmap */
#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
#define IOCTL_GIFMAP_FUNC3_DEF IOCTL_GET_FUNC3_DEF(gifmap)
#else
#define IOCTL_GIFMAP_FUNC3_DEF
#endif

/* esock_ioctl_giftxqlen */
#if defined(SIOCGIFTXQLEN)
#define IOCTL_GIFTXQLEN_FUNC3_DEF IOCTL_GET_FUNC3_DEF(giftxqlen)
#else
#define IOCTL_GIFTXQLEN_FUNC3_DEF
#endif

#define IOCTL_GET_FUNCS3_DEF			\
    IOCTL_GIFINDEX_FUNC3_DEF;			\
    IOCTL_GIFFLAGS_FUNC3_DEF;			\
    IOCTL_GIFADDR_FUNC3_DEF;			\
    IOCTL_GIFDSTADDR_FUNC3_DEF;			\
    IOCTL_GIFBRDADDR_FUNC3_DEF;			\
    IOCTL_GIFNETMASK_FUNC3_DEF;			\
    IOCTL_GIFMTU_FUNC3_DEF;			\
    IOCTL_GIFHWADDR_FUNC3_DEF;			\
    IOCTL_GENADDR_FUNC3_DEF;			\
    IOCTL_GIFMAP_FUNC3_DEF;			\
    IOCTL_GIFTXQLEN_FUNC3_DEF;
#define IOCTL_GET_FUNC3_DEF(F)					\
    static ERL_NIF_TERM essio_ioctl_##F(ErlNifEnv*       env,	\
                                        ESockDescriptor* descP,	\
                                        ERL_NIF_TERM     ename)
IOCTL_GET_FUNCS3_DEF
#undef IOCTL_GET_FUNC3_DEF

/* esock_ioctl_sifflags */
#if defined(SIOCSIFFLAGS)
#define IOCTL_SIFFLAGS_FUNC_DEF IOCTL_SET_FUNC_DEF(sifflags)
#else
#define IOCTL_SIFFLAGS_FUNC_DEF
#endif

/* esock_ioctl_sifaddr */
#if defined(SIOCSIFADDR)
#define IOCTL_SIFADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifaddr)
#else
#define IOCTL_SIFADDR_FUNC_DEF
#endif

/* esock_ioctl_sifdstaddr */
#if defined(SIOCSIFDSTADDR)
#define IOCTL_SIFDSTADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifdstaddr)
#else
#define IOCTL_SIFDSTADDR_FUNC_DEF
#endif

/* esock_ioctl_sifbrdaddr */
#if defined(SIOCSIFBRDADDR)
#define IOCTL_SIFBRDADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifbrdaddr)
#else
#define IOCTL_SIFBRDADDR_FUNC_DEF
#endif

/* esock_ioctl_sifnetmask */
#if defined(SIOCSIFNETMASK)
#define IOCTL_SIFNETMASK_FUNC_DEF IOCTL_SET_FUNC_DEF(sifnetmask)
#else
#define IOCTL_SIFNETMASK_FUNC_DEF
#endif

/* esock_ioctl_sifmtu */
#if defined(SIOCSIFMTU)
#define IOCTL_SIFMTU_FUNC_DEF IOCTL_SET_FUNC_DEF(sifmtu)
#else
#define IOCTL_SIFMTU_FUNC_DEF
#endif

/* esock_ioctl_siftxqlen */
#if defined(SIOCSIFTXQLEN)
#define IOCTL_SIFTXQLEN_FUNC_DEF IOCTL_SET_FUNC_DEF(siftxqlen)
#else
#define IOCTL_SIFTXQLEN_FUNC_DEF
#endif

/* esock_ioctl_sifhwaddr */
#if defined(SIOCSIFHWADDR)
#define IOCTL_SIFHWADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifhwaddr)
#else
#define IOCTL_SIFHWADDR_FUNC_DEF
#endif

#define IOCTL_SET_FUNCS_DEF			\
  IOCTL_SIFFLAGS_FUNC_DEF;			\
  IOCTL_SIFADDR_FUNC_DEF;			\
  IOCTL_SIFDSTADDR_FUNC_DEF;			\
  IOCTL_SIFBRDADDR_FUNC_DEF;			\
  IOCTL_SIFNETMASK_FUNC_DEF;			\
  IOCTL_SIFMTU_FUNC_DEF;			\
  IOCTL_SIFTXQLEN_FUNC_DEF;			\
  IOCTL_SIFHWADDR_FUNC_DEF;
#define IOCTL_SET_FUNC_DEF(F)					\
  static ERL_NIF_TERM essio_ioctl_##F(ErlNifEnv*       env,	\
				      ESockDescriptor* descP,	\
				      ERL_NIF_TERM     ename,   \
				      ERL_NIF_TERM     evalue)
IOCTL_SET_FUNCS_DEF
#undef IOCTL_SET_FUNC_DEF


static ERL_NIF_TERM encode_ioctl_ifconf(ErlNifEnv*       env,
					ESockDescriptor* descP,
					struct ifconf*   ifcP);
static ERL_NIF_TERM encode_ioctl_ifconf_ifreq(ErlNifEnv*       env,
					      ESockDescriptor* descP,
					      struct ifreq*    ifrP);
static ERL_NIF_TERM encode_ioctl_ifreq_name(ErlNifEnv* env,
					    char*      name);
static ERL_NIF_TERM encode_ioctl_ifreq_sockaddr(ErlNifEnv*       env,
						struct sockaddr* sa);
static ERL_NIF_TERM make_ifreq(ErlNifEnv*   env,
			       ERL_NIF_TERM name,
			       ERL_NIF_TERM key2,
			       ERL_NIF_TERM val2);
#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
static ERL_NIF_TERM encode_ioctl_ifrmap(ErlNifEnv*       env,
					ESockDescriptor* descP,
					struct ifmap*    mapP);
#endif
#if (defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR))
static ERL_NIF_TERM encode_ioctl_hwaddr(ErlNifEnv*       env,
					ESockDescriptor* descP,
					struct sockaddr* addrP);
#endif
#if (defined(SIOCGENADDR) && defined(ESOCK_USE_ENADDR))
static ERL_NIF_TERM encode_ioctl_enaddr(ErlNifEnv*       env,
					ESockDescriptor* descP,
					char*            addrP);
#endif
static ERL_NIF_TERM encode_ioctl_ifraddr(ErlNifEnv*       env,
					 ESockDescriptor* descP,
					 struct sockaddr* addrP);
static ERL_NIF_TERM encode_ioctl_flags(ErlNifEnv*       env,
				       ESockDescriptor* descP,
				       short            flags);
#if defined(SIOCSIFFLAGS)
static BOOLEAN_T decode_ioctl_flags(ErlNifEnv*       env,
				    ESockDescriptor* descP,
				    ERL_NIF_TERM     eflags,
				    short*           flags);
#endif
static BOOLEAN_T decode_ioctl_sockaddr(ErlNifEnv*       env,
				       ESockDescriptor* descP,
				       ERL_NIF_TERM     eaddr,
				       ESockAddress*    addr);
#if defined(SIOCSIFHWADDR)
static
BOOLEAN_T decode_ioctl_hwaddr(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     eaddr,
                              ESockAddress*    addr);
#endif
#if defined(SIOCSIFMTU)
static BOOLEAN_T decode_ioctl_mtu(ErlNifEnv*       env,
				  ESockDescriptor* descP,
				  ERL_NIF_TERM     emtu,
				  int*             mtu);
#endif
#if defined(SIOCSIFTXQLEN)
static BOOLEAN_T decode_ioctl_txqlen(ErlNifEnv*       env,
				     ESockDescriptor* descP,
				     ERL_NIF_TERM     etxqlen,
				     int*             txqlen);
#endif
#if defined(SIOCSIFTXQLEN)
static BOOLEAN_T decode_ioctl_ivalue(ErlNifEnv*       env,
				     ESockDescriptor* descP,
				     ERL_NIF_TERM     eivalue,
				     int*             ivalue);
#endif
static ERL_NIF_TERM encode_ioctl_ivalue(ErlNifEnv*       env,
					ESockDescriptor* descP,
					int              ivalue);
static ERL_NIF_TERM encode_ioctl_bvalue(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              bvalue);


/*
static void essio_down_ctrl(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            const ErlNifPid* pidP);
*/
static void essio_down_acceptor(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                const ErlNifPid* pidP,
                                const ErlNifMonitor* monP);
static void essio_down_writer(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              const ErlNifPid* pidP,
                              const ErlNifMonitor* monP);
static void essio_down_reader(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              const ErlNifPid* pidP,
                              const ErlNifMonitor* monP);

static BOOLEAN_T do_stop(ErlNifEnv*       env,
                         ESockDescriptor* descP);


/* =================================================================== *
 *                                                                     *
 *                      Local (global) variables                       *
 *                                                                     *
 * =================================================================== */

static ESSIOControl ctrl = {0};



/* ======================================================================== *
 *                              ESSIO Functions                             *
 * ======================================================================== *
 */

/*
 * For "standard" (unix) synchronous I/O, in our case
 * this is just a dummy function.
 */
extern
int essio_init(unsigned int     numThreads,
               const ESockData* dataP)
{
    VOID(numThreads);

    ctrl.dbg        = dataP->dbg;
    ctrl.sockDbg    = dataP->sockDbg;

    return ESOCK_IO_OK;
}


/*
 * For "standard" (unix) synchronous I/O, this is just a dummy function.
 * Also, will we ever call this?
 */
extern
void essio_finish(void)
{
    return;
}



/* *******************************************************************
 * essio_info - Return info "about" this I/O backend.
 */

extern
ERL_NIF_TERM essio_info(ErlNifEnv* env)
{
    ERL_NIF_TERM info;
    ERL_NIF_TERM keys[]  = {esock_atom_name};
    ERL_NIF_TERM vals[]  = {MKA(env, "unix_essio")};
    unsigned int numKeys = NUM(keys);
    unsigned int numVals = NUM(vals);

    ESOCK_ASSERT( numKeys == numVals );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );

    return info;
}



/* ========================================================================
 * essio_open - create an endpoint (from an existing fd) for communication
 *
 * Assumes the input has been validated.
 *
 * Normally we want debugging on (individual) sockets to be controlled
 * by the sockets own debug flag. But since we don't even have a socket
 * yet, we must use the global debug flag.
 */
extern
ERL_NIF_TERM essio_open_with_fd(ErlNifEnv*       env,
                                int              fd,
                                ERL_NIF_TERM     eopts,
                                const ESockData* dataP)
{
    BOOLEAN_T        dbg    = esock_open_is_debug(env, eopts, dataP->sockDbg);
    BOOLEAN_T        useReg = esock_open_use_registry(env, eopts, dataP->useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              domain, type, protocol;
    int              save_errno = 0;
    BOOLEAN_T        closeOnClose;
    SOCKET           sock;
    ErlNifPid        self;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("UNIX-ESSIO", "essio_open_with_fd -> entry with"
             "\r\n   fd:    %d"
             "\r\n   eopts: %T"
             "\r\n", fd, eopts) );

    /*
     * Before we do anything else, we try to retrieve domain, type and protocol
     * This information is either present in the eopts map or if not we need
     * to "get" it from the system (getsockopt).
     * Note that its not possible to get all of these on all platforms,
     * and in those cases the user *must* provide us with them (eopts).
     *
     * We try the system first (since its more reliable) and if that fails
     * we check the eopts map. If neither one works, we *give up*!
     */

    if (! open_which_domain(fd, &domain)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO",
                 "essio_open_with_fd -> failed get domain from system\r\n") );

        if (! open_get_domain(env, eopts, &domain)) {
            return esock_make_invalid(env, esock_atom_domain);
        }
    }

    if (! open_which_type(fd, &type)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO",
                 "essio_open_with_fd -> failed get type from system\r\n") );

        if (! open_get_type(env, eopts, &type))
            return esock_make_invalid(env, esock_atom_type);
    }

    if (! esock_open_which_protocol(fd, &protocol)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO",
                 "essio_open_with_fd -> failed get protocol from system\r\n") );

        if (! open_get_protocol(env, eopts, &protocol)) {
            SSDBG2( dbg,
                    ("UNIX-ESSIO",
                     "essio_open_with_fd -> "
                     "failed get protocol => try protocol 0\r\n") );
            protocol = 0;
        }
    }


    SSDBG2( dbg,
            ("UNIX-ESSIO",
             "essio_open_with_fd -> "
             "\r\n   domain:   %d (%s)"
             "\r\n   type:     %d (%s)"
             "\r\n   protocol: %d (%s)"
             "\r\n",
             domain,   DOM2STR(domain),
             type,     TYPE2STR(type),
             protocol, PROTO2STR(protocol)) );


    if (open_todup(env, eopts)) {
        /* We shall dup the socket */
        if (ESOCK_IS_ERROR(sock = dup(fd))) {
            save_errno = sock_errno();

            SSDBG2( dbg,
                    ("UNIX-ESSIO",
                     "essio_open_with_fd -> dup failed: %d\r\n",
                     save_errno) );

            return esock_make_error_errno(env, save_errno);
        }
        closeOnClose = TRUE;
    } else {
        sock         = fd;
        closeOnClose = FALSE;
    }


    SET_NONBLOCKING(sock);

    /* Create and initiate the socket "descriptor" */
    descP               = esock_alloc_descriptor(sock);
    descP->ctrlPid      = self;
    descP->domain       = domain;
    descP->type         = type;
    descP->protocol     = protocol;
    descP->closeOnClose = closeOnClose;
    descP->origFD       = fd;

    /* Check if we are already connected, if so change state */
    {
        ESockAddress remote;
        SOCKLEN_T    addrLen = sizeof(remote);
        sys_memzero((char *) &remote, addrLen);
        if (sock_peer(descP->sock,
                      (struct sockaddr*) &remote,
                      &addrLen) == 0) {
            SSDBG2( dbg, ("UNIX-ESSIO",
                          "essio_open_with_fd -> connected\r\n") );
            descP->writeState |= ESOCK_STATE_CONNECTED;
        } else {
            SSDBG2( dbg, ("UNIX-ESSIO",
                          "essio_open_with_fd -> not connected\r\n") );
        }
    }

    /* And create the 'socket' resource */
    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    ESOCK_ASSERT( MONP("essio_open_with_fd -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    esock_inc_socket(domain, type, protocol);

    /* And finally (maybe) update the registry.
     * Shall we keep track of the fact that this socket is created elsewhere?
     */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    SSDBG2( dbg,
            ("UNIX-ESSIO", "essio_open_with_fd -> done: %T\r\n", sockRef) );

    return esock_make_ok2(env, sockRef);
}


static
BOOLEAN_T open_which_domain(SOCKET sock, int* domain)
{
#if defined(SO_DOMAIN)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_DOMAIN, domain))
        return TRUE;
#endif
    return FALSE;
}

/* The eopts contains an integer 'domain' key.
 */
static
BOOLEAN_T open_get_domain(ErlNifEnv*   env,
                          ERL_NIF_TERM eopts,
                          int*         domain)
{
    ERL_NIF_TERM edomain;
    
    if (!GET_MAP_VAL(env, eopts,
		     esock_atom_domain, &edomain))
      return FALSE;

    if (esock_decode_domain(env, edomain, domain) == 0)
      return FALSE;

    return TRUE;
}

static
BOOLEAN_T open_which_type(SOCKET sock, int* type)
{
#if defined(SO_TYPE)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_TYPE, type))
        return TRUE;
#endif
    return FALSE;
}

/* The eopts contains an integer 'type' key.
 */
static
BOOLEAN_T open_get_type(ErlNifEnv*   env,
                        ERL_NIF_TERM eopts,
                        int*         type)
{
    ERL_NIF_TERM etype;

    if (! GET_MAP_VAL(env, eopts, esock_atom_type, &etype))
        return FALSE;

    if (! esock_decode_type(env, etype, type))
        return FALSE;

    return TRUE;
}

/* The eopts contains an integer 'type' key.
 */
static
BOOLEAN_T open_get_protocol(ErlNifEnv*   env,
                            ERL_NIF_TERM eopts,
                            int*         protocol)
{
    return esock_extract_int_from_map(env, eopts,
                                      esock_atom_protocol, protocol);
}


/* The eopts contains a boolean 'dup' key. Defaults to TRUE.
 */
static
BOOLEAN_T open_todup(ErlNifEnv* env, ERL_NIF_TERM eopts)
{
    return esock_get_bool_from_map(env, eopts, esock_atom_dup, TRUE);
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_open_plain(ErlNifEnv*       env,
                              int              domain,
                              int              type,
                              int              protocol,
                              ERL_NIF_TERM     eopts,
                              const ESockData* dataP)
{
    BOOLEAN_T        dbg    = esock_open_is_debug(env, eopts, dataP->sockDbg);
    BOOLEAN_T        useReg = esock_open_use_registry(env, eopts, dataP->useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              proto = protocol;
    SOCKET           sock;
    char*            netns;
#ifdef HAVE_SETNS
    int              save_errno;
    int              current_ns = 0;
#endif
    ErlNifPid        self;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("UNIX-ESSIO", "essio_open4 -> entry with"
             "\r\n   domain:   %d (%s)"
             "\r\n   type:     %d (%s)"
             "\r\n   protocol: %d (%s)"
             "\r\n   eopts:    %T"
             "\r\n",
             domain,   DOM2STR(domain),
             type,     TYPE2STR(type),
             protocol, PROTO2STR(protocol),
             eopts) );


#ifdef HAVE_SETNS
    if (open_get_netns(env, eopts, &netns)) {
        SSDBG2( dbg,
                ("UNIX-ESSIO", "essio_open4 -> namespace: %s\r\n", netns) );
    }
#else
    netns = NULL;
#endif


#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        (! change_network_namespace(dbg,
                                    netns, &current_ns, &save_errno))) {
        FREE(netns);
        return esock_make_error_errno(env, save_errno);
    }
#endif

    if (ESOCK_IS_ERROR(sock = sock_open(domain, type, proto))) {
        if (netns != NULL) FREE(netns);
        return esock_make_error_errno(env, sock_errno());
    }

    SSDBG2( dbg, ("UNIX-ESSIO", "essio_open4 -> open success: %d\r\n", sock) );


    /* NOTE that if the protocol = 0 (default) and the domain is not
     * local (AF_LOCAL) we need to explicitly get the protocol here!
     */
    
    if (proto == 0)
        (void) esock_open_which_protocol(sock, &proto);

#ifdef HAVE_SETNS
    if (netns != NULL) {
        FREE(netns);
        if (! restore_network_namespace(dbg,
                                        current_ns, sock, &save_errno))
            return esock_make_error_errno(env, save_errno);
    }
#endif

    SET_NONBLOCKING(sock);


    /* Create and initiate the socket "descriptor" */
    descP           = esock_alloc_descriptor(sock);
    descP->ctrlPid  = self;
    descP->domain   = domain;
    descP->type     = type;
    descP->protocol = proto;

    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    ESOCK_ASSERT( MONP("esock_open -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    esock_inc_socket(domain, type, proto);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    return esock_make_ok2(env, sockRef);
}


#ifdef HAVE_SETNS
/* open_get_netns - extract the netns field from the opts map
 */
static
BOOLEAN_T open_get_netns(ErlNifEnv* env, ERL_NIF_TERM opts, char** netns)
{
    ERL_NIF_TERM val;
    ErlNifBinary bin;
    char*        buf;

    /* The currently only supported extra option is: netns */
    if (!GET_MAP_VAL(env, opts, esock_atom_netns, &val)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    /* The value should be a binary file name */
    if (! enif_inspect_binary(env, val, &bin)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    ESOCK_ASSERT( (buf = MALLOC(bin.size+1)) != NULL );

    sys_memcpy(buf, bin.data, bin.size);
    buf[bin.size] = '\0';
    *netns = buf;

    return TRUE;
}


/* We should really have another API, so that we can return errno... */

/* *** change network namespace ***
 * Retrieve the current namespace and set the new.
 * Return result and previous namespace if successful.
 */
static
BOOLEAN_T change_network_namespace(BOOLEAN_T dbg,
                                   char* netns, int* cns, int* err)
{
    int save_errno;
    int current_ns = 0;
    int new_ns     = 0;

    SSDBG2( dbg,
            ("UNIX-ESSIO", "change_network_namespace -> entry with"
             "\r\n   new ns: %s"
             "\r\n", netns) );

    current_ns = open("/proc/self/ns/net", O_RDONLY);
    if (ESOCK_IS_ERROR(current_ns)) {
        *err = sock_errno();
        return FALSE;
    }
    new_ns = open(netns, O_RDONLY);
    if (ESOCK_IS_ERROR(new_ns)) {
        save_errno = sock_errno();
        (void) close(current_ns);
        *err = save_errno;
        return FALSE;
    }
    if (setns(new_ns, CLONE_NEWNET) != 0) {
        save_errno = sock_errno();
        (void) close(new_ns);
        (void) close(current_ns);
        *err = save_errno;
        return FALSE;
    } else {
        (void) close(new_ns);
        *cns = current_ns;
        return TRUE;
    }
}


/* *** restore network namespace ***
 * Restore the previous namespace (see above).
 */
static
BOOLEAN_T restore_network_namespace(BOOLEAN_T dbg,
                                    int ns, SOCKET sock, int* err)
{
    SSDBG2( dbg,
            ("UNIX-ESSIO", "restore_network_namespace -> entry with"
             "\r\n   ns: %d"
             "\r\n", ns) );

    if (setns(ns, CLONE_NEWNET) != 0) {
        /* XXX Failed to restore network namespace.
         * What to do? Tidy up and return an error...
         * Note that the thread now might still be in the namespace.
         * Can this even happen? Should the emulator be aborted?
         */
        int save_errno = sock_errno();
        (void) close(sock);
        (void) close(ns);
        *err = save_errno;
        return FALSE;
    } else {
        (void) close(ns);
        return TRUE;
    }
}

#endif



/* ========================================================================
 */
extern
ERL_NIF_TERM essio_bind(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ESockAddress*    sockAddrP,
                        SOCKLEN_T        addrLen)
{
    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    if (sock_bind(descP->sock, &sockAddrP->sa, addrLen) < 0) {
        return esock_make_error_errno(env, sock_errno());
    }

    descP->readState |= ESOCK_STATE_BOUND;

    return esock_atom_ok;
}


/*  ========================================================================
 */
extern
ERL_NIF_TERM essio_connect(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     connRef,
                           ESockAddress*    addrP,
                           SOCKLEN_T        addrLen)
{
    int       save_errno;
    ErlNifPid self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    /*
     * Verify that we are in the proper state
     */

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentWriterP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    if (descP->connectorP != NULL) {
        /* Connect in progress */

        if (COMPARE_PIDS(&self, &descP->connector.pid) != 0) {
	    /* Other process has connect in progress */
	    if (addrP != NULL) {
                return esock_make_error(env, esock_atom_already);
	    } else {
	        /* This is a bad call sequence
		 * - connect without an address is only allowed
		 *   for the connecting process
		 */
	        return esock_raise_invalid(env, esock_atom_state);
	    }
        }

        /* Finalize after received select message */

        esock_requestor_release("essio_connect finalize -> connected",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;

        if (! verify_is_connected(descP, &save_errno)) {
            return esock_make_error_errno(env, save_errno);
        }

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* No connect in progress */

    if (addrP == NULL)
        /* This is a bad call sequence
         * - connect without an address is only allowed when
         *   a connect is in progress, after getting the select message
         */
        return esock_raise_invalid(env, esock_atom_state);

    /* Initial connect call, with address */

    if (sock_connect(descP->sock, (struct sockaddr*) addrP, addrLen) == 0) {
        /* Success already! */
        SSDBG( descP, ("UNIX-ESSIO", "essio_connect {%d} -> connected\r\n",
                       descP->sock) );

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* Connect returned error */
    save_errno = sock_errno();

    switch (save_errno) {

    case EINPROGRESS:   /* Unix & OSE!!        */
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_connect {%d} -> would block => select\r\n",
                descP->sock) );
        {
            int sres;

            if ((sres =
                 esock_select_write(env, descP->sock, descP, NULL,
                                    sockRef, connRef)) < 0)
                return
                    enif_raise_exception(env,
                                         MKT2(env, esock_atom_select_write,
                                              MKI(env, sres)));
            /* Initiate connector */
            descP->connector.pid = self;
            ESOCK_ASSERT( MONP("essio_connect -> conn",
                               env, descP,
                               &self, &descP->connector.mon) == 0 );
            descP->connector.env = esock_alloc_env("connector");
            descP->connector.ref = CP_TERM(descP->connector.env, connRef);
            descP->connectorP = &descP->connector;
            descP->writeState |=
                (ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

            return esock_atom_select;
        }
        break;

    default:
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_connect {%d} -> error: %d\r\n",
                descP->sock, save_errno) );

        return esock_make_error_errno(env, save_errno);

    } // switch(save_errno)
}


/* *** verify_is_connected ***
 * Check if a connection has been established.
 */
static
BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err)
{
    /*
     * *** This is strange ***
     *
     * This *should* work on Windows NT too, but doesn't.
     * An bug in Winsock 2.0 for Windows NT?
     *
     * See "Unix Netwok Programming", "The Sockets Networking API",
     * W.R.Stevens, Volume 1, third edition, 16.4 Nonblocking 'connect',
     * before Interrupted 'connect' (p 412) for a discussion about
     * Unix portability and non blocking connect.
     */

    int error = 0;

#ifdef SO_ERROR
    if (! esock_getopt_int(descP->sock, SOL_SOCKET, SO_ERROR, &error)) {
        // Solaris does it this way according to W.R.Stevens
        error = sock_errno();
    }
#elif 1
    char buf[0];
    if (ESOCK_IS_ERROR(read(descP->sock, buf, sizeof(buf)))) {
        error = sock_errno();
    }
#else
    /* This variant probably returns wrong error value
     * ENOTCONN instead of the actual connect error
     */
    ESockAddress remote;
    SOCKLEN_T    addrLen = sizeof(remote);
    sys_memzero((char *) &remote, addrLen);
    if (sock_peer(descP->sock,
                  (struct sockaddr*) &remote, &addrLen)) < 0) {
        error = sock_errno();
    }
#endif

    if (error != 0) {
        *err = error;
        return FALSE;
    }
    return TRUE;
}



/* *** essio_listen *** */


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_accept(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     accRef)
{
    ErlNifPid caller;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState))
        return esock_make_error_closed(env);

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentReaderP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    if (descP->currentAcceptorP == NULL) {
        SOCKET accSock;

        /* We have no active acceptor (and therefore no acceptors in queue)
         */

        SSDBG( descP, ("UNIX-ESSIO", "essio_accept {%d} -> try accept\r\n",
                       descP->sock) );

	ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_tries, &descP->accTries, 1);

        accSock = sock_accept(descP->sock, NULL, NULL);

        if (ESOCK_IS_ERROR(accSock)) {
            int           save_errno;

            save_errno = sock_errno();

            return essio_accept_listening_error(env, descP, sockRef,
                                                accRef, caller, save_errno);
        } else {
            /* We got an incoming connection */
            return essio_accept_listening_accept(env, descP, sockRef,
                                                 accSock, caller);
        }
    } else {

        /* We have an active acceptor and possibly acceptors waiting in queue.
         * If the pid of the calling process is not the pid of the
	 * "current process", push the requester onto the (acceptor) queue.
         */

        SSDBG( descP, ("UNIX-ESSIO", "essio_accept_accepting -> "
                       "check: is caller current acceptor:"
                       "\r\n   Caller:      %T"
                       "\r\n   Current:     %T"
                       "\r\n   Current Mon: %T"
                       "\r\n",
                       caller,
                       descP->currentAcceptor.pid,
                       ESOCK_MON2TERM(env, &descP->currentAcceptor.mon)) );

        if (COMPARE_PIDS(&descP->currentAcceptor.pid, &caller) == 0) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_accepting {%d} -> "
                    "current acceptor - try again"
                    "\r\n", descP->sock) );

            return essio_accept_accepting_current(env, descP, sockRef, accRef);

        } else {

            /* Not the "current acceptor", so (maybe) push onto queue */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_accepting {%d} -> *not* current acceptor\r\n",
                    descP->sock) );

            return essio_accept_accepting_other(env, descP, accRef, caller);
        }
    }
}


/* *** essio_accept_listening_error ***
 *
 * The accept call resultet in an error - handle it.
 * There are only two cases:
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
static
ERL_NIF_TERM essio_accept_listening_error(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     accRef,
                                          ErlNifPid        caller,
                                          int              save_errno)
{
    ERL_NIF_TERM res;

    if (save_errno == ERRNO_BLOCK ||
        save_errno == EAGAIN) {

        /* *** Try again later *** */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_accept_listening_error {%d} -> would block - retry\r\n",
                descP->sock) );

	descP->currentAcceptor.pid = caller;
        ESOCK_ASSERT( MONP("essio_accept_listening -> current acceptor",
                           env, descP,
                           &descP->currentAcceptor.pid,
                           &descP->currentAcceptor.mon) == 0 );
        ESOCK_ASSERT( descP->currentAcceptor.env == NULL );
        descP->currentAcceptor.env = esock_alloc_env("current acceptor");
        descP->currentAcceptor.ref =
            CP_TERM(descP->currentAcceptor.env, accRef);
        descP->currentAcceptorP = &descP->currentAcceptor;

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_accept_listening_error {%d} -> retry for: "
                "\r\n   Current Pid: %T"
                "\r\n   Current Mon: %T"
                "\r\n",
                descP->sock,
                descP->currentAcceptor.pid,
                ESOCK_MON2TERM(env, &descP->currentAcceptor.mon)) );

        res = essio_accept_busy_retry(env, descP, sockRef, accRef, NULL);

    } else {

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_accept_listening {%d} -> errno: %d\r\n",
                descP->sock, save_errno) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_fails, &descP->accFails, 1);

        res = esock_make_error_errno(env, save_errno);
    }

    return res;
}


/* *** essio_accept_listening_accept ***
 *
 * The accept call was successful (accepted) - handle the new connection.
 */
static
ERL_NIF_TERM essio_accept_listening_accept(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef,
                                           SOCKET           accSock,
                                           ErlNifPid        caller)
{
    ERL_NIF_TERM res;

    essio_accept_accepted(env, descP, sockRef, accSock, caller, &res);

    return res;
}


/* *** essio_accept_accepting_current ***
 * Handles when the current acceptor makes another attempt.
 */
static
ERL_NIF_TERM essio_accept_accepting_current(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef)
{
    SOCKET        accSock;
    int           save_errno;
    ERL_NIF_TERM  res;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_accept_accepting_current {%d} -> try accept\r\n",
            descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_tries, &descP->accTries, 1);
	
    accSock = sock_accept(descP->sock, NULL, NULL);

    if (ESOCK_IS_ERROR(accSock)) {

        save_errno = sock_errno();

        res = essio_accept_accepting_current_error(env, descP, sockRef,
                                                   accRef, save_errno);
    } else {

        res = essio_accept_accepting_current_accept(env, descP, sockRef,
                                                    accSock);
    }

    return res;
}


/* *** essio_accept_accepting_current_accept ***
 *
 * Handles when the current acceptor succeeded in its accept call -
 * handle the new connection.
 */
static
ERL_NIF_TERM essio_accept_accepting_current_accept(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   SOCKET           accSock)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_accept_accepting_current_accept {%d}"
            "\r\n", descP->sock) );

    if (essio_accept_accepted(env, descP, sockRef, accSock,
                              descP->currentAcceptor.pid, &res)) {

        ESOCK_ASSERT( DEMONP("essio_accept_accepting_current_accept -> "
                             "current acceptor",
                             env, descP, &descP->currentAcceptor.mon) == 0);

        MON_INIT(&descP->currentAcceptor.mon);

        if (!esock_activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_accepting_current_accept {%d} ->"
                    " no more acceptors"
                    "\r\n", descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

    }

    return res;
}


/* *** essio_accept_accepting_current_error ***
 * The accept call of current acceptor resultet in an error - handle it.
 * There are only two cases:
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
static
ERL_NIF_TERM essio_accept_accepting_current_error(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  ERL_NIF_TERM     accRef,
                                                  int              save_errno)
{
    ERL_NIF_TERM res, reason;

    if (save_errno == ERRNO_BLOCK ||
        save_errno == EAGAIN) {

        /*
         * Just try again, no real error, just a ghost trigger from poll,
         */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_accept_accepting_current_error(%d) -> "
                "would block: try again\r\n", descP->sock) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_waits, &descP->accWaits, 1);


        /* Maybe cancel "current" select */
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_accept_accepting_current_error(%d) -> "
                "cancel current select"
                "\r\n", descP->sock) );
        res = esock_cancel_read_select(env, descP, descP->currentAcceptor.ref);

        if (IS_OK(res)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_accepting_current_error(%d) -> "
                    "send abort message"
                    "\r\n", descP->sock) );
            esock_send_abort_msg(env, descP, sockRef,
                                 &descP->currentAcceptor,
                                 esock_atom_cancelled);
            /* We need a new env,
             * since sending the abort message uses up the old */
            descP->currentAcceptor.env = esock_alloc_env("current acceptor");

            /* And update the currentr acceptor ref (handle) */
            descP->currentAcceptor.ref =
                CP_TERM(descP->currentAcceptor.env, accRef);
        
            /* And finally - retry */
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_accepting_current_error(%d) -> "
                    "try new select"
                    "\r\n", descP->sock) );
            res = essio_accept_busy_retry(env, descP, sockRef, accRef,
                                          &descP->currentAcceptor.pid);
        }

    } else {
        ESockRequestor req;

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_accept_accepting_current_error(%d) -> "
                "error: %d\r\n", descP->sock, save_errno) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_acc_fails, &descP->accFails, 1);

        esock_requestor_release("essio_accept_accepting_current_error",
                                env, descP, &descP->currentAcceptor);

        reason = MKA(env, erl_errno_id(save_errno));
        res    = esock_make_error(env, reason);

        req.env = NULL;
        while (esock_acceptor_pop(env, descP, &req)) {
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_accepting_current_error(%d) -> abort %T\r\n",
                    descP->sock, req.pid) );

            esock_send_abort_msg(env, descP, sockRef, &req, reason);

            (void) DEMONP("essio_accept_accepting_current_error -> "
                          "pop'ed writer",
                          env, descP, &req.mon);
        }
        descP->currentAcceptorP = NULL;
    }

    return res;
}


/* *** essio_accept_accepting_other ***
 * Handles when the another acceptor makes an attempt, which
 * results (maybe) in the request being pushed onto the
 * acceptor queue.
 */
static
ERL_NIF_TERM essio_accept_accepting_other(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     ref,
                                          ErlNifPid        caller)
{
    if (! esock_acceptor_search4pid(env, descP, &caller)) {
        esock_acceptor_push(env, descP, caller, ref, NULL);
	return esock_atom_select;
    } else {
        /* Acceptor already in queue */
        return esock_raise_invalid(env, esock_atom_state);
    }
}


/* *** essio_accept_busy_retry ***
 *
 * Perform a retry select. If successful, set nextState.
 */
static
ERL_NIF_TERM essio_accept_busy_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     accRef,
                                     ErlNifPid*       pidP)
{
    int          sres;
    ERL_NIF_TERM res;

    if ((sres = esock_select_read(env, descP->sock, descP, pidP,
                                  sockRef, accRef)) < 0) {

        ESOCK_ASSERT( DEMONP("essio_accept_busy_retry - select failed",
                             env, descP, &descP->currentAcceptor.mon) == 0);

        MON_INIT(&descP->currentAcceptor.mon);

        /* It is very unlikely that a next acceptor will be able
         * to do anything successful, but we will clean the queue
         */
        
        if (!esock_activate_next_acceptor(env, descP, sockRef)) {
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_accept_busy_retry {%d} -> no more acceptors\r\n",
                    descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

        res =
            enif_raise_exception(env,
                                 MKT2(env, esock_atom_select_read,
                                      MKI(env, sres)));
    } else {

        descP->readState |=
            (ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);

        res = esock_atom_select;
    }

    return res;
}


/* *** essio_accept_accepted ***
 *
 * Generic function handling a successful accept.
 */
static
BOOLEAN_T essio_accept_accepted(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                SOCKET           accSock,
                                ErlNifPid        pid,
                                ERL_NIF_TERM*    result)
{
    ESockDescriptor* accDescP;
    ERL_NIF_TERM     accRef;

    /*
     * We got one
     */

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_acc_success, &descP->accSuccess, 1);

    accDescP           = esock_alloc_descriptor(accSock);
    accDescP->domain   = descP->domain;
    accDescP->type     = descP->type;
    accDescP->protocol = descP->protocol;

    MLOCK(descP->writeMtx);

    accDescP->rBufSz   = descP->rBufSz;  // Inherit buffer size
    accDescP->rNum     = descP->rNum;    // Inherit buffer uses
    accDescP->rNumCnt  = 0;
    accDescP->rCtrlSz  = descP->rCtrlSz; // Inherit buffer size
    accDescP->wCtrlSz  = descP->wCtrlSz; // Inherit buffer size
    accDescP->iow      = descP->iow;     // Inherit iow
    accDescP->dbg      = descP->dbg;     // Inherit debug flag
    accDescP->useReg   = descP->useReg;  // Inherit useReg flag
    esock_inc_socket(accDescP->domain, accDescP->type, accDescP->protocol);

    accRef = enif_make_resource(env, accDescP);
    enif_release_resource(accDescP);

    accDescP->ctrlPid = pid;
    /* pid has actually been compared equal to self()
     * in this code path just a little while ago
     */
    ESOCK_ASSERT( MONP("essio_accept_accepted -> ctrl",
                       env, accDescP,
                       &accDescP->ctrlPid,
                       &accDescP->ctrlMon) == 0 );

    SET_NONBLOCKING(accDescP->sock);

    accDescP->writeState |= ESOCK_STATE_CONNECTED;

    MUNLOCK(descP->writeMtx);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, accRef);

    *result = esock_make_ok2(env, accRef);

    return TRUE;
}



/* ========================================================================
 * Do the actual send.
 * Do some initial writer checks, do the actual send and then
 * analyze the result. If we are done, another writer may be
 * scheduled (if there is one in the writer queue).
 */
extern
ERL_NIF_TERM essio_send(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     sendRef,
                        ErlNifBinary*    sndDataP,
                        int              flags)
{
    ssize_t      send_result;
    ERL_NIF_TERM writerCheck;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    send_result = (ssize_t) sndDataP->size;
    if ((size_t) send_result != sndDataP->size)
        return esock_make_error_invalid(env, esock_atom_data_size);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP, ("UNIX-ESSIO", "esock_send {%d} -> writer check failed: "
                       "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }
    
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    send_result = sock_send(descP->sock, sndDataP->data, sndDataP->size, flags);

    return send_check_result(env, descP,
                             send_result, sndDataP->size, FALSE,
                             sockRef, sendRef);

}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendto(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     sendRef,
                          ErlNifBinary*    dataP,
                          int              flags,
                          ESockAddress*    toAddrP,
                          SOCKLEN_T        toAddrLen)
{
    ssize_t      result;
    ERL_NIF_TERM writerCheck;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    result = (ssize_t) dataP->size;
    if ((size_t) result != dataP->size)
        return esock_make_error_invalid(env, esock_atom_data_size);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP, ("UNIX-ESSIO",
                       "essio_sendto {%d} -> writer check failed: "
                       "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }
    
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    if (toAddrP != NULL) {
        result = sock_sendto(descP->sock,
                             dataP->data, dataP->size, flags,
                             &toAddrP->sa, toAddrLen);
    } else {
        result = sock_sendto(descP->sock,
                             dataP->data, dataP->size, flags,
                             NULL, 0);
    }

    return send_check_result(env, descP, result, dataP->size, FALSE,
                             sockRef, sendRef);
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     sendRef,
                           ERL_NIF_TERM     eMsg,
                           int              flags,
                           ERL_NIF_TERM     eIOV,
                           const ESockData* dataP)
{
    ERL_NIF_TERM  res, eAddr, eCtrl;
    ESockAddress  addr;
    struct msghdr msgHdr;
    ErlNifIOVec  *iovec = NULL;
    char*         ctrlBuf;
    size_t        ctrlBufLen,  ctrlBufUsed;
    ssize_t       dataSize,    sendmsg_result;
    ERL_NIF_TERM  writerCheck, tail;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_sendmsg {%d} -> writer check failed: "
                "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }

    /* Initiate the .name and .namelen fields depending on if
     * we have an address or not
     */
    if (! GET_MAP_VAL(env, eMsg, esock_atom_addr, &eAddr)) {

        SSDBG( descP, ("UNIX-ESSIO",
                       "essio_sendmsg {%d} -> no address\r\n", descP->sock) );

        msgHdr.msg_name    = NULL;
        msgHdr.msg_namelen = 0;
    } else {
        msgHdr.msg_name    = (void*) &addr;
        msgHdr.msg_namelen = sizeof(addr);
        sys_memzero((char *) msgHdr.msg_name, msgHdr.msg_namelen);

        SSDBG( descP, ("UNIX-ESSIO", "essio_sendmsg {%d} ->"
                       "\r\n   address: %T"
                       "\r\n", descP->sock, eAddr) );

        if (! esock_decode_sockaddr(env, eAddr,
                                    msgHdr.msg_name,
                                    &msgHdr.msg_namelen)) {
            SSDBG( descP, ("UNIX-ESSIO",
                           "essio_sendmsg {%d} -> invalid address\r\n",
                           descP->sock) );
            return esock_make_invalid(env, esock_atom_addr);
        }
    }

    /* Extract the *mandatory* 'iov', which must be an erlang:iovec(),
     * from which we take at most IOV_MAX binaries
     */
    if ((! enif_inspect_iovec(NULL, dataP->iov_max, eIOV, &tail, &iovec))) {
        SSDBG( descP, ("UNIX-ESSIO",
                       "essio_sendmsg {%d} -> not an iov\r\n",
                       descP->sock) );

        return esock_make_invalid(env, esock_atom_iov);
    }

    SSDBG( descP, ("UNIX-ESSIO", "essio_sendmsg {%d} ->"
                   "\r\n   iovcnt: %lu"
                   "\r\n   tail:   %s"
                   "\r\n", descP->sock,
                   (unsigned long) iovec->iovcnt,
                   B2S(! enif_is_empty_list(env, tail))) );

    /* We now have an allocated iovec */

    eCtrl             = esock_atom_undefined;
    ctrlBufLen        = 0;
    ctrlBuf           = NULL;

    if (iovec->iovcnt > dataP->iov_max) {
        if (descP->type == SOCK_STREAM) {
            iovec->iovcnt = dataP->iov_max;
        } else {
            /* We can not send the whole packet in one sendmsg() call */
            SSDBG( descP, ("UNIX-ESSIO",
                           "essio_sendmsg {%d} -> iovcnt > iov_max\r\n",
                           descP->sock) );
            res = esock_make_invalid(env, esock_atom_iov);
            goto done_free_iovec;
        }
    }

    dataSize = 0;
    {
        ERL_NIF_TERM h,   t;
        ErlNifBinary bin;
        size_t       i;

        /* Find out if there is remaining data in the tail.
         * Skip empty binaries otherwise break.
         * If 'tail' after loop exit is the empty list
         * there was no more data.  Otherwise there is more
         * data or the 'iov' is invalid.
         */
        for (;;) {
            if (enif_get_list_cell(env, tail, &h, &t) &&
                enif_inspect_binary(env, h, &bin) &&
                (bin.size == 0)) {
                tail = t;
                continue;
            } else
                break;
        }

        if ((! enif_is_empty_list(env, tail)) &&
            (descP->type != SOCK_STREAM)) {
            /* We can not send the whole packet in one sendmsg() call */
            SSDBG( descP, ("UNIX-ESSIO",
                           "essio_sendmsg {%d} -> invalid tail\r\n",
                           descP->sock) );
            res = esock_make_invalid(env, esock_atom_iov);
            goto done_free_iovec;
        }

        /* Calculate the data size */

        for (i = 0;  i < iovec->iovcnt;  i++) {
            size_t len = iovec->iov[i].iov_len;
            dataSize += len;
            if (dataSize < len) {
                /* Overflow */
                SSDBG( descP, ("UNIX-ESSIO", "essio_sendmsg {%d} -> Overflow"
                               "\r\n   i:         %lu"
                               "\r\n   len:       %lu"
                               "\r\n   dataSize:  %ld"
                               "\r\n", descP->sock, (unsigned long) i,
                               (unsigned long) len, (long) dataSize) );
                res = esock_make_invalid(env, esock_atom_iov);
                goto done_free_iovec;
            }
        }
    }
    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_sendmsg {%d} -> iovec size verified"
            "\r\n   iov length: %lu"
            "\r\n   data size:  %u"
            "\r\n",
            descP->sock,
            (unsigned long) iovec->iovcnt, (long) dataSize) );

    msgHdr.msg_iovlen = iovec->iovcnt;
    msgHdr.msg_iov    = iovec->iov;

    /* Extract the *optional* 'ctrl' */
    if (GET_MAP_VAL(env, eMsg, esock_atom_ctrl, &eCtrl)) {
        ctrlBufLen = descP->wCtrlSz;
        ctrlBuf    = (char*) MALLOC(ctrlBufLen);
        ESOCK_ASSERT( ctrlBuf != NULL );
    }
    SSDBG( descP, ("UNIX-ESSIO", "essio_sendmsg {%d} -> optional ctrl: "
                   "\r\n   ctrlBuf:    %p"
                   "\r\n   ctrlBufLen: %lu"
                   "\r\n   eCtrl:      %T"
                   "\r\n", descP->sock,
                   ctrlBuf, (unsigned long) ctrlBufLen, eCtrl) );

    /* Decode the ctrl and initiate that part of the msghdr.
     */
    if (ctrlBuf != NULL) {
        if (! decode_cmsghdrs(env, descP,
                              eCtrl,
                              ctrlBuf, ctrlBufLen, &ctrlBufUsed)) {
            SSDBG( descP, ("UNIX-ESSIO",
                           "essio_sendmsg {%d} -> invalid ctrl\r\n",
                           descP->sock) );
            res = esock_make_invalid(env, esock_atom_ctrl);
            goto done_free_iovec;
        }
    } else {
        ctrlBufUsed = 0;
    }
    msgHdr.msg_control    = ctrlBuf;
    msgHdr.msg_controllen = ctrlBufUsed;

    /* The msg_flags field is not used when sending,
     * but zero it just in case */
    msgHdr.msg_flags      = 0;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    /* And now, try to send the message */
    sendmsg_result = sock_sendmsg(descP->sock, &msgHdr, flags);

    res = send_check_result(env, descP, sendmsg_result, dataSize,
                            (! enif_is_empty_list(env, tail)),
                            sockRef, sendRef);

 done_free_iovec:
    FREE_IOVEC( iovec );
    if (ctrlBuf != NULL) FREE(ctrlBuf);

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_sendmsg {%d} -> done"
            "\r\n   %T"
            "\r\n", descP->sock, res) );

    return res;

}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendv(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ERL_NIF_TERM     sockRef,
                         ERL_NIF_TERM     sendRef,
                         ERL_NIF_TERM     eIOV,
                         const ESockData* dataP)
{
    ERL_NIF_TERM  eres;
    ErlNifIOVec  *iovec = NULL;
    ssize_t       dataSize, sendv_result;
    ERL_NIF_TERM  writerCheck, tail;
    BOOLEAN_T     dataInTail;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_sendv {%d} -> writer check failed: "
                "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }


    /* Extract the 'iov', which must be an erlang:iovec(),
     * from which we take at most IOV_MAX binaries
     */
    if (! enif_inspect_iovec(NULL, dataP->iov_max, eIOV, &tail, &iovec)) {
        SSDBG( descP, ("UNIX-ESSIO",
                       "essio_sendv {%d} -> iov inspection failed\r\n",
                       descP->sock) );

        return esock_make_invalid(env, esock_atom_iov);
    }

    if (iovec == NULL) {
        SSDBG( descP, ("UNIX-ESSIO",
                       "essio_sendv {%d} -> not an iov\r\n",
                       descP->sock) );

        return esock_make_invalid(env, esock_atom_iov);
    }

    // ESOCK_ASSERT( iovec != NULL );

    dataInTail = (! enif_is_empty_list(env, tail));    

    SSDBG( descP, ("UNIX-ESSIO", "essio_sendv {%d} ->"
                   "\r\n   iovcnt: %lu"
                   "\r\n   tail:   %s"
                   "\r\n", descP->sock,
                   (unsigned long) iovec->iovcnt, B2S(dataInTail)) );

    /* We now have an allocated iovec - verify vector size */

    if (iovec->iovcnt > dataP->iov_max) {
        if (descP->type == SOCK_STREAM) {
            iovec->iovcnt = dataP->iov_max;
        } else {
            /* We can not send the whole packet in one sendv() call */
            SSDBG( descP, ("UNIX-ESSIO",
                           "essio_sendv {%d} -> iovcnt > iov_max\r\n",
                           descP->sock) );

            FREE_IOVEC( iovec );

            return esock_make_invalid(env, esock_atom_iov);
        }
    }

    dataSize = 0;
    {
        ERL_NIF_TERM h,   t;
        ErlNifBinary bin;
        size_t       i;

        /* Find out if there is remaining data in the tail.
         * Skip empty binaries otherwise break.
         * If 'tail' after loop exit is the empty list
         * there was no more data.  Otherwise there is more
         * data or the 'iov' is invalid.
         */
        for (;;) {
            if (enif_get_list_cell(env, tail, &h, &t) &&
                enif_inspect_binary(env, h, &bin) &&
                (bin.size == 0)) {
                tail = t;
                continue;
            } else
                break;
        }

        if (dataInTail &&
            (descP->type != SOCK_STREAM)) {
            /* We can not send the whole packet in one sendmsg() call */
            SSDBG( descP, ("UNIX-ESSIO",
                           "essio_sendv {%d} -> invalid tail\r\n",
                           descP->sock) );

            FREE_IOVEC( iovec );

            return esock_make_invalid(env, esock_atom_iov);

        }

        /* Calculate the data size */

        for (i = 0;  i < iovec->iovcnt;  i++) {
            size_t len = iovec->iov[i].iov_len;
            dataSize += len;
            if (dataSize < len) {
                /* Overflow */
                SSDBG( descP, ("UNIX-ESSIO", "essio_sendv {%d} -> Overflow"
                               "\r\n   i:         %lu"
                               "\r\n   len:       %lu"
                               "\r\n   dataSize:  %ld"
                               "\r\n", descP->sock, (unsigned long) i,
                               (unsigned long) len, (long) dataSize) );

                FREE_IOVEC( iovec );

                return esock_make_invalid(env, esock_atom_iov);

            }
        }
    }

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_sendv {%d} -> iovec size verified"
            "\r\n   iov length: %lu"
            "\r\n   data size:  %u"
            "\r\n",
            descP->sock,
            (unsigned long) iovec->iovcnt, (long) dataSize) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_tries, &descP->writeTries, 1);

    /* And now, try to send the message */
    sendv_result = sock_sendv(descP->sock, iovec->iov, iovec->iovcnt);

    eres = send_check_result(env, descP, sendv_result,
                             dataSize, dataInTail,
                             sockRef, sendRef);

    FREE_IOVEC( iovec );

    SSDBG( descP, ("UNIX-ESSIO", "essio_sendv {%d} -> done"
                   "\r\n   data size:    %lu"
                   "\r\n   data in tail: %s"
                   "\r\n", descP->sock,
                   dataSize, B2S(dataInTail)) );

    return eres;
}


/* ========================================================================
 * Start a sendfile() operation
 */
extern
ERL_NIF_TERM essio_sendfile_start(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     sendRef,
                                  off_t            offset,
                                  size_t           count,
                                  ERL_NIF_TERM     fRef)
{
#if defined(HAVE_SENDFILE)
    ERL_NIF_TERM writerCheck;
    ssize_t      res;
    int          err;

    SSDBG( descP, ("UNIX-ESSIO",
                   "essio_sendfile_start {%d} -> entry with"
                   "\r\n   sockRef: %T"
                   "\r\n   sendRef: %T"
                   "\r\n   fRef:    %T"
                   "\r\n   offset:  %lu"
                   "\r\n   count:   %lu"
                   "\r\n",
                   descP->sock, sockRef, sendRef,
                   fRef, (unsigned long) offset, (unsigned long) count) );

    if (! IS_OPEN(descP->writeState)) {
        return esock_make_error_closed(env);
    }

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL) {
        return esock_make_error_invalid(env, esock_atom_state);
    }

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer
     */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP, ("UNIX-ESSIO",
                       "essio_sendfile_start {%d} -> writer check failed: "
                       "\r\n   %T\r\n", descP->sock, writerCheck) );

        /* Returns 'select' if current process got enqueued,
         * or exception invalid state if current process already
         * was enqueued
         */
        return writerCheck;
    }

    if (descP->sendfileHandle != INVALID_HANDLE)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Get a dup:ed file handle from prim_file_nif
     * through a NIF dyncall
     */
    {
        struct prim_file_nif_dyncall_dup dc_dup;

        dc_dup.op = prim_file_nif_dyncall_dup;
        dc_dup.result = EINVAL; // should not be needed

        /* Request the handle */
        if (enif_dynamic_resource_call(env,
                                       esock_atom_prim_file,
                                       esock_atom_efile,
                                       fRef,
                                       &dc_dup)
            != 0) {
            return
                essio_sendfile_error(env, descP, sockRef,
                                     MKT2(env,
                                          esock_atom_invalid,
                                          esock_atom_efile));
        }
        if (dc_dup.result != 0) {
            return
                essio_sendfile_errno(env, descP, sockRef, dc_dup.result);
        }
        descP->sendfileHandle = dc_dup.handle;
    }

    SSDBG( descP, ("UNIX-ESSIO",
                   "essio_sendfile_start(%T) {%d} -> sendRef: %T"
                   "\r\n   sendfileHandle: %d"
                   "\r\n",
                   sockRef, descP->sock, sendRef,
                   descP->sendfileHandle) );

    if (descP->sendfileCountersP == NULL) {
        descP->sendfileCountersP = MALLOC(sizeof(ESockSendfileCounters));
        *descP->sendfileCountersP = initESockSendfileCounters;
    }

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_sendfile_tries,
                  &descP->sendfileCountersP->tries, 1);
    descP->sendfileCountersP->maxCnt = 0;

    res = essio_sendfile(env, descP, sockRef, offset, &count, &err);

    if (res < 0) { // Terminal error

        (void) close(descP->sendfileHandle);
        descP->sendfileHandle = INVALID_HANDLE;

        return essio_sendfile_errno(env, descP, sockRef, err);

    } else if (res > 0) { // Retry by select

        if (descP->currentWriterP == NULL) {
            int mon_res;

            /* Register writer as current */
            ESOCK_ASSERT( enif_self(env, &descP->currentWriter.pid) != NULL );
            mon_res =
                MONP("sendfile-start -> current writer",
                     env, descP,
                     &descP->currentWriter.pid,
                     &descP->currentWriter.mon);
            ESOCK_ASSERT( mon_res >= 0 );

            if (mon_res > 0) {
                /* Caller died already, can happen for dirty NIFs */

                (void) close(descP->sendfileHandle);
                descP->sendfileHandle = INVALID_HANDLE;

                return essio_sendfile_error(env, descP, sockRef,
                                            MKT2(env,
                                                 esock_atom_invalid,
                                                 esock_atom_not_owner));
            }
            ESOCK_ASSERT( descP->currentWriter.env == NULL );
            descP->currentWriter.env = esock_alloc_env("current-writer");
            descP->currentWriter.ref =
                CP_TERM(descP->currentWriter.env, sendRef);
            descP->currentWriterP = &descP->currentWriter;
        }
        // else current writer is already registered by esock_requestor_pop()

        return essio_sendfile_select(env, descP, sockRef, sendRef, count);

    } else { // res == 0: Done
        return essio_sendfile_ok(env, descP, sockRef, count);
    }
#else
    VOID(env);
    VOID(descP);
    VOID(sockRef);
    VOID(sendRef);
    VOID(offset);
    VOID(count);
    VOID(fRef);
    return enif_raise_exception(env, MKA(env, "notsup"));
#endif
}


/* ========================================================================
 * Continue an ongoing sendfile operation
 */

extern
ERL_NIF_TERM essio_sendfile_cont(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     sendRef,
                                 off_t            offset,
                                 size_t           count)
{
#if defined(HAVE_SENDFILE)
    ErlNifPid caller;
    ssize_t   res;
    int       err;

    SSDBG( descP, ("UNIX-ESSIO",
                   "essio_sendfile_cont {%d} -> entry"
                   "\r\n   sockRef: %T"
                   "\r\n   sendRef: %T"
                   "\r\n", descP->sock, sockRef, sendRef) );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error_closed(env);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, esock_atom_state);

    /* Verify that this process has a sendfile operation in progress */
    ESOCK_ASSERT( enif_self(env, &caller) != NULL );
    if ((descP->currentWriterP == NULL) ||
        (descP->sendfileHandle == INVALID_HANDLE) ||
        (COMPARE_PIDS(&descP->currentWriter.pid, &caller) != 0)) {
        //
        return esock_raise_invalid(env, esock_atom_state);
    }

    res = essio_sendfile(env, descP, sockRef, offset, &count, &err);

    if (res < 0) { // Terminal error

        (void) close(descP->sendfileHandle);
        descP->sendfileHandle = INVALID_HANDLE;

        return essio_sendfile_errno(env, descP, sockRef, err);

     } else if (res > 0) { // Retry by select

        /* Overwrite current writer registration */
        enif_clear_env(descP->currentWriter.env);
        descP->currentWriter.ref =
            CP_TERM(descP->currentWriter.env, sendRef);

        return essio_sendfile_select(env, descP, sockRef, sendRef, count);

    } else { // res == 0: Done
        return essio_sendfile_ok(env, descP, sockRef, count);
    }
#else
    VOID(env);
    VOID(descP);
    VOID(sockRef);
    VOID(sendRef);
    VOID(offset);
    VOID(count);
    return enif_raise_exception(env, MKA(env, "notsup"));
#endif
}


/* ========================================================================
 * Deferred close of the dup:ed file descriptor
 */

extern
ERL_NIF_TERM essio_sendfile_deferred_close(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
#if defined(HAVE_SENDFILE)
    if (descP->sendfileHandle == INVALID_HANDLE)
        return esock_make_error_invalid(env, esock_atom_state);

    (void) close(descP->sendfileHandle);
    descP->sendfileHandle = INVALID_HANDLE;

    return esock_atom_ok;
#else
    VOID(env);
    VOID(descP);
    return enif_raise_exception(env, MKA(env, "notsup"));
#endif
}



/* ========================================================================
 * The (read) buffer handling should be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
extern
ERL_NIF_TERM essio_recv(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    int          saveErrno;
    ErlNifBinary buf;
    ssize_t      readResult;
    size_t       bufSz = (len != 0 ? len : descP->rBufSz); // 0 means default
    ERL_NIF_TERM ret;

    SSDBG( descP, ("UNIX-ESSIO", "essio_recv {%d} -> entry with"
                   "\r\n   bufSz: %lu"
                   "\r\n   count: %u"
                   "\r\n   len:   %ld"
                   "\r\n", descP->sock,
                   (unsigned long) bufSz, descP->rNumCnt, (long) len) );


    /* Check basic state and current reader */
    if (! recv_check_entry(env, descP, recvRef, &ret)) {
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_recv {%d} -> entry failed: "
                "\r\n   %T\r\n", descP->sock, ret) );
        return ret;
    }

    /* Allocate the receive buffer */
    if (descP->buf.data == NULL) {
        ESOCK_ASSERT( ALLOC_BIN(bufSz, &buf) );
    }
    else {
        buf = descP->buf;
        if (buf.size != bufSz) {
            REALLOC_BIN(&buf, bufSz);
        }
    }

    SSDBG( descP, ("UNIX-ESSIO", "essio_recv {%d} -> try read (%lu)\r\n",
                   descP->sock, (unsigned long) len) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_tries, &descP->readTries, 1);

    /* recv() */
    readResult = sock_recv(descP->sock, buf.data, buf.size, flags);
    saveErrno  = ESOCK_IS_ERROR(readResult) ? sock_errno() : 0;

    SSDBG( descP, ("UNIX-ESSIO",
                   "essio_recv {%d} -> readResult: %ld (%d)\r\n",
                   descP->sock, (long) readResult, saveErrno) );

    /* Check for errors and end of stream */
    if (! recv_check_result(env, descP, sockRef, recvRef,
                            readResult, saveErrno, &ret) ) {
        /* Keep the buffer */
        descP->buf = buf;
        return ret;
    }
    /* readResult >= 0 */
    ESOCK_ASSERT( readResult <= buf.size );

    if (readResult < buf.size) {

        /* +++ We did not fill the buffer +++ */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_recv {%d} -> [%lu] "
                "did not fill the buffer (%ld)\r\n",
                descP->sock, (unsigned long) buf.size,
                (long) readResult) );

        if (// Less than 4K (1 page) wasted
            readResult >= (buf.size & ~4095) ||
            // Less than 25% wasted
            readResult >= (buf.size >> 1) + (buf.size >> 2)) {
            //
            /* Reallocate and drop buffer */
            descP->buf.data = NULL;
            ESOCK_ASSERT( REALLOC_BIN(&buf, readResult) );
        }
        else {
            /* Keep buffer, copy content to new binary*/
            descP->buf = buf;
            ESOCK_ASSERT( ALLOC_BIN(readResult, &buf) );
            sys_memcpy(buf.data, descP->buf.data, buf.size);
        }
        /* Return {ok|timeout|select|select_read, Bin} */
        return recv_check_partial(env, descP, sockRef, recvRef, len, &buf);

    } else {

        /* +++ We filled the buffer +++ */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_recv {%d} -> [%lu] filled the buffer\r\n",
                descP->sock, (unsigned long) buf.size) );

        descP->buf.data = NULL; // Drop buffer
        /* Return {more|ok|select_read, Bin} */
        return recv_check_full(env, descP, sockRef, recvRef, len, &buf);
    }
}


/* ========================================================================
 * The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
extern
ERL_NIF_TERM essio_recvfrom(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            ssize_t          len,
                            int              flags)
{
    ESockAddress  fromAddr;
    SOCKLEN_T     fromAddrLen;
    ssize_t       readResult;
    int           saveErrno;
    ErlNifBinary  buf;
    size_t        bufSz = (len != 0 ? len : descP->rBufSz); // 0 means default
    ERL_NIF_TERM  ret;

    SSDBG( descP, ("UNIX-ESSIO", "essio_recvfrom {%d} -> entry with"
                   "\r\n   bufSz: %lu (%ld)"
                   "\r\n", descP->sock,
                   (unsigned long) bufSz, (long) len) );

    /* Check basic state and current reader */
    if (! recv_check_entry(env, descP, recvRef, &ret)) {
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_recvfrom {%d} -> entry failed: "
                "\r\n   %T\r\n", descP->sock, ret) );
        return ret;
    }

    /* Allocate the receive buffer */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &buf) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_tries, &descP->readTries, 1);

    fromAddrLen = sizeof(fromAddr);
    sys_memzero((char*) &fromAddr, fromAddrLen);

    /* recvfrom() */
    readResult = sock_recvfrom(descP->sock, buf.data, buf.size, flags,
                               &fromAddr.sa, &fromAddrLen);
    saveErrno = ESOCK_IS_ERROR(readResult) ? sock_errno() : 0;

    /* Check for errors and end of stream */
    if (! recv_check_result(env, descP, sockRef, recvRef,
                            readResult, saveErrno, &ret) ) {
        FREE_BIN(&buf);
        return ret;
    }
    /* readResult >= 0 */
    ESOCK_ASSERT( readResult <= buf.size );

    /* The recvfrom function delivers one (1) message. If our buffer
     * is too small, the message will be truncated. So, regardless
     * if we filled the buffer or not, we have got what we are going
     * to get regarding this message.
     *
     * Encode the message and source address
     */

    if (readResult < buf.size) {
        ESOCK_ASSERT( REALLOC_BIN(&buf, readResult) );
    }

    descP->rNumCnt = 0;

    ESOCK_CNT_INC(env, descP, sockRef, esock_atom_read_pkg,
                  &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef, esock_atom_read_byte,
                  &descP->readByteCnt, buf.size);
    if (buf.size > descP->readPkgMax)
        descP->readPkgMax = buf.size;

    esock_encode_sockaddr(env,
                          &fromAddr, fromAddrLen,
                          &ret);

    /* MKBIN transfers "ownership" of the *allocated* binary to an
     * erlang term in env (no need to free; it will be GC:ed).
     */
    /* {FromAddr, Bin} */
    ret = MKT2(env, ret, MKBIN(env, &buf));

    if (descP->selectRead && (COMPARE(recvRef, esock_atom_zero) != 0)) {
        /* Return {select_read, {FromAddr, Bin}} */
        ret = MKT2(env, esock_atom_select_read, ret);
        return recv_check_select(env, descP, sockRef, recvRef, ret);
    }
    else {
        recv_update_current_reader(env, descP, sockRef);
        /* Return {ok, {FromAddr, Bin}} */
        return esock_make_ok2(env, ret);
    }
}


/* ========================================================================
 * The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
extern
ERL_NIF_TERM essio_recvmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef,
                           ssize_t          bufLen,
                           ssize_t          ctrlLen,
                           int              flags)
{
    SOCKLEN_T     addrLen;
    ssize_t       readResult;
    int           saveErrno;
    size_t        bufSz  = (bufLen  != 0 ? bufLen  : descP->rBufSz);
    size_t        ctrlSz = (ctrlLen != 0 ? ctrlLen : descP->rCtrlSz);
    struct msghdr msgHdr;
    SysIOVec      iov[1];  // Shall we always use 1?
    ErlNifBinary  data[1]; // Shall we always use 1?
    ErlNifBinary  ctrl;
    ERL_NIF_TERM  ret;
    ESockAddress  addr;

    SSDBG( descP, ("UNIX-ESSIO", "essio_recvmsg {%d} -> entry with"
                   "\r\n   bufSz:  %lu (%ld)"
                   "\r\n   ctrlSz: %ld (%ld)"
                   "\r\n", descP->sock,
                   (unsigned long) bufSz, (long) bufLen,
                   (unsigned long) ctrlSz, (long) ctrlLen) );

    /* Check basic state and current reader */
    if (! recv_check_entry(env, descP, recvRef, &ret)) {
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_recvmsg {%d} -> entry failed: "
                "\r\n   %T\r\n", descP->sock, ret) );
        return ret;
    }

    /* Allocate the (msg) data buffer:
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &data[0]) );

    /* Allocate the ctrl (buffer):
     */
    ESOCK_ASSERT( ALLOC_BIN(ctrlSz, &ctrl) );

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_tries, &descP->readTries, 1);

    addrLen = sizeof(addr);
    sys_memzero((char*) &addr,   addrLen);
    sys_memzero((char*) &msgHdr, sizeof(msgHdr));

    iov[0].iov_base = data[0].data;
    iov[0].iov_len  = data[0].size;

    msgHdr.msg_name       = &addr;
    msgHdr.msg_namelen    = addrLen;
    msgHdr.msg_iov        = iov;
    msgHdr.msg_iovlen     = 1; // Should use a constant or calculate...
    msgHdr.msg_control    = ctrl.data;
    msgHdr.msg_controllen = ctrl.size;

    /* recvmsg() */
    readResult = sock_recvmsg(descP->sock, &msgHdr, flags);
    saveErrno = ESOCK_IS_ERROR(readResult) ? sock_errno() : 0;

    /* Check for errors and end of stream */
    if (! recv_check_result(env, descP, sockRef, recvRef,
                            readResult, saveErrno, &ret) ) {
        FREE_BIN(&data[0]);
        FREE_BIN(&ctrl);
        return ret;
    }
    /* readResult >= 0 */

    /* The recvmsg function delivers one (1) message. If our buffer
     * is to small, the message will be truncated. So, regardless
     * if we filled the buffer or not, we have got what we are going
     * to get regarding this message.
     */

    /*
     * <KOLLA>
     *
     * The return value of recvmsg is the *total* number of bytes
     * that where successfully read. This data has been put into
     * the *IO vector*.
     *
     * </KOLLA>
     */

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_recvmsg {%d} -> ok\r\n",
            descP->sock) );

    descP->rNumCnt = 0;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef, esock_atom_read_byte,
                  &descP->readByteCnt, readResult);
    if (readResult > descP->readPkgMax)
        descP->readPkgMax = readResult;

    encode_msg(env, descP,
               readResult, &msgHdr, &data[0], &ctrl,
               &ret);

    if (descP->selectRead && (COMPARE(recvRef, esock_atom_zero) != 0)) {
        /* Return {select_read, Msg} */
        ret = MKT2(env, esock_atom_select_read, ret);
        return recv_check_select(env, descP, sockRef, recvRef, ret);
    }
    else {
        recv_update_current_reader(env, descP, sockRef);
        /* Return {ok, Msg} */
        return esock_make_ok2(env, ret);
    }
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_close(ErlNifEnv*       env,
                         ESockDescriptor* descP)
{
    if (! IS_OPEN(descP->readState)) {
        /* A bit of cheeting; maybe not closed yet - do we need a queue? */
        return esock_make_error_closed(env);
    }

    /* Store the PID of the caller,
     * since we need to inform it when we
     * (that is, the stop callback function)
     * completes.
     */
    ESOCK_ASSERT( enif_self(env, &descP->closerPid) != NULL );

    /* If the caller is not the owner; monitor the caller,
     * since we should complete this operation even if the caller dies
     * (for whatever reason).
     */
    if (COMPARE_PIDS(&descP->closerPid, &descP->ctrlPid) != 0) {

        ESOCK_ASSERT( MONP("essio_close-check -> closer",
                           env, descP,
                           &descP->closerPid,
                           &descP->closerMon) == 0 );
    }

    /* Prepare for closing the socket */
    descP->readState  |= ESOCK_STATE_CLOSING;
    descP->writeState |= ESOCK_STATE_CLOSING;
    if (do_stop(env, descP)) {
        // stop() has been scheduled - wait for it
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_close {%d} -> stop was scheduled\r\n",
                descP->sock) );

        // Create closeRef for the close msg that esock_stop() will send
        descP->closeEnv = esock_alloc_env("esock_close_do - close-env");
        descP->closeRef = MKREF(descP->closeEnv);

        return esock_make_ok2(env, CP_TERM(env, descP->closeRef));
    } else {
        // The socket may be closed - tell caller to finalize
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_close {%d} -> stop was called\r\n",
                descP->sock) );

        return esock_atom_ok;
    }
}



/* Prepare for close - return whether stop is scheduled or not
 */
static
BOOLEAN_T do_stop(ErlNifEnv*       env,
                  ESockDescriptor* descP)
{
    BOOLEAN_T    ret;
    int          sres;
    ERL_NIF_TERM sockRef;

    sockRef = enif_make_resource(env, descP);

    if (IS_SELECTED(descP)) {
        ESOCK_ASSERT( (sres = esock_select_stop(env,
                                                (ErlNifEvent) descP->sock,
                                                descP))
                      >= 0 );
        if ((sres & ERL_NIF_SELECT_STOP_CALLED) != 0) {
            /* The socket is no longer known by the select machinery
             * - it may be closed
             */
            ret = FALSE;
        } else {
            ESOCK_ASSERT( (sres & ERL_NIF_SELECT_STOP_SCHEDULED) != 0 );
            /* esock_stop() is scheduled
             * - socket may be removed by esock_stop() or later
             */
            ret = TRUE;
        }
    } else {
        sres = 0;
        /* The socket has never been used in the select machinery
         * - it may be closed
         */
        ret = FALSE;
    }

    /* +++++++ Current and waiting Writers +++++++ */

    if (descP->currentWriterP != NULL) {

        /* We have a current Writer; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_WRITE_CANCELLED) {

            /* The current Writer will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "writer",
                                      descP, sockRef, &descP->currentWriter);
        }

        /* Inform the waiting Writers (in the same way) */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "do_stop {%d} -> handle waiting writer(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "writer",
                                   descP, sockRef, &descP->writersQ,
                                   esock_atom_closed);

        descP->currentWriterP = NULL;
    }

    /* +++++++ Connector +++++++
     * Note that there should not be Writers and a Connector
     * at the same time so the check for if the
     * current Writer/Connecter was deselected is only correct
     * under that assumption
     */

    if (descP->connectorP != NULL) {

        /* We have a Connector; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_WRITE_CANCELLED) {

            /* The Connector will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "connector",
                                      descP, sockRef, &descP->connector);
        }

        descP->connectorP = NULL;
    }

    /* +++++++ Current and waiting Readers +++++++ */

    if (descP->currentReaderP != NULL) {

        /* We have a current Reader; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_READ_CANCELLED) {

            /* The current Reader will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "reader",
                                      descP, sockRef, &descP->currentReader);
        }

        /* Inform the Readers (in the same way) */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "do_stop {%d} -> handle waiting reader(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "reader",
                                   descP, sockRef, &descP->readersQ,
                                   esock_atom_closed);

        descP->currentReaderP = NULL;
    }

    /* +++++++ Current and waiting Acceptors +++++++
     *
     * Note that there should not be Readers and Acceptors
     * at the same time so the check for if the
     * current Reader/Acceptor was deselected is only correct
     * under that assumption
     */

    if (descP->currentAcceptorP != NULL) {

        /* We have a current Acceptor; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_READ_CANCELLED) {

            /* The current Acceptor will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "acceptor",
                                      descP, sockRef, &descP->currentAcceptor);
        }

        /* Inform the waiting Acceptor (in the same way) */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "do_stop {%d} -> handle waiting acceptors(s)\r\n",
                descP->sock) );

        esock_inform_waiting_procs(env, "acceptor",
                                   descP, sockRef, &descP->acceptorsQ,
                                   esock_atom_closed);

        descP->currentAcceptorP = NULL;
    }

    return ret;
}



/* ========================================================================
 * Perform the final step in the socket close.
 */
extern
ERL_NIF_TERM essio_fin_close(ErlNifEnv*       env,
                             ESockDescriptor* descP)
{
    int       err;
    ErlNifPid self;
#ifdef HAVE_SENDFILE
    HANDLE    sendfileHandle;
#endif

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    if (IS_CLOSED(descP->readState))
        return esock_make_error_closed(env);

    if (! IS_CLOSING(descP->readState)) {
        // esock_close() has not been called
        return esock_raise_invalid(env, esock_atom_state);
    }

    if (IS_SELECTED(descP) && (descP->closeEnv != NULL)) {
        // esock_stop() is scheduled but has not been called
        return esock_raise_invalid(env, esock_atom_state);
    }

    if (COMPARE_PIDS(&descP->closerPid, &self) != 0) {
        // This process is not the closer
        return esock_raise_invalid(env, esock_atom_state);
    }

    // Close the socket

    /* Stop monitoring the closer.
     * Demonitoring may fail since this is a dirty NIF
     * - the caller may have died already.
     */
    enif_set_pid_undefined(&descP->closerPid);
    if (descP->closerMon.isActive) {
        (void) DEMONP("essio_fin_close -> closer",
                      env, descP, &descP->closerMon);
    }

    /* Stop monitoring the owner */
    enif_set_pid_undefined(&descP->ctrlPid);
    (void) DEMONP("essio_fin_close -> ctrl",
                  env, descP, &descP->ctrlMon);
    /* Not impossible to still get a esock_down() call from a
     * just triggered owner monitor down
     */

#ifdef HAVE_SENDFILE
    sendfileHandle = descP->sendfileHandle;
    descP->sendfileHandle = INVALID_HANDLE;
#endif

    /* This nif-function is executed in a dirty scheduler just so
     * that it can "hang" (with minimum effect on the VM) while the
     * kernel writes our buffers. IF we have set the linger option
     * for this ({true, integer() > 0}). For this to work we must
     * be blocking...
     */
    SET_BLOCKING(descP->sock);
    err = esock_close_socket(env, descP, TRUE);

#ifdef HAVE_SENDFILE
    if (sendfileHandle != INVALID_HANDLE) {
        (void) close(descP->sendfileHandle);
    }
#endif

    if (err != 0) {
        if (err == ERRNO_BLOCK) {
            /* Not all data in the buffers where sent,
             * make sure the caller gets this.
             */
            return esock_make_error(env, esock_atom_timeout);
        } else {
            return esock_make_error_errno(env, err);
        }
    }

    return esock_atom_ok;
}


/* ========================================================================
 * *** essio_shutdown should go here - if we need one ***
 */


/* ========================================================================
 * *** essio_sockname should go here - if we need one ***
 */


/* ========================================================================
 * *** essio_peername should go here - if we need one ***
 */


/* ========================================================================
 * Cancel a connect request.
 */

extern
ERL_NIF_TERM essio_cancel_connect(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;
    ErlNifPid    self;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_cancel_connect {%d} -> entry with"
            "\r\n   writeState: 0x%X"
            "\r\n   opRef:      %T"
            "\r\n",
            descP->sock, descP->writeState, opRef) );

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    if (! IS_OPEN(descP->writeState)) {

        res = esock_make_error_closed(env);

    } else if ((descP->connectorP == NULL) ||
               (COMPARE_PIDS(&self, &descP->connector.pid) != 0) ||
               (COMPARE(opRef, descP->connector.ref) != 0)) {

        res = esock_make_error(env, esock_atom_not_found);

    } else {

        res = esock_cancel_write_select(env, descP, opRef);
        esock_requestor_release("esock_cancel_connect",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;
    }

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_cancel_connect {%d} -> done when"
            "\r\n   res: %T"
            "\r\n",
            descP->sock, descP->writeState,
            opRef, res) );

    return res;
}



/* ========================================================================
 * Cancel accept request
 *
 * We have two different cases:
 *   *) Its the current acceptor
 *      Cancel the select!
 *      We need to activate one of the waiting acceptors.
 *   *) Its one of the acceptors ("waiting") in the queue
 *      Simply remove the acceptor from the queue.
 *
 */
extern
ERL_NIF_TERM essio_cancel_accept(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_cancel_accept(%T), {%d,0x%X} ->"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n",
            sockRef,  descP->sock, descP->readState,
            opRef,
            ((descP->currentAcceptorP == NULL)
             ? "without acceptor" : "with acceptor")) );

    if (! IS_OPEN(descP->readState)) {

        res = esock_make_error_closed(env);

    } else if (descP->currentAcceptorP == NULL) {

        res = esock_atom_not_found;

    } else {
        ErlNifPid self;

        ESOCK_ASSERT( enif_self(env, &self) != NULL );

        if (COMPARE_PIDS(&self, &descP->currentAcceptor.pid) == 0) {
            if (COMPARE(opRef, descP->currentAcceptor.ref) == 0)
                res = essio_cancel_accept_current(env, descP, sockRef);
            else
                res = esock_atom_not_found;
        } else {
            res = essio_cancel_accept_waiting(env, descP, opRef, &self);
        }
    }

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_cancel_accept(%T) -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, res) );

    return res;
}


/* The current acceptor process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the acceptor queue).
 */
static
ERL_NIF_TERM essio_cancel_accept_current(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    ESOCK_ASSERT( DEMONP("essio_cancel_accept_current -> current acceptor",
                         env, descP, &descP->currentAcceptor.mon) == 0);
    MON_INIT(&descP->currentAcceptor.mon);
    res = esock_cancel_read_select(env, descP, descP->currentAcceptor.ref);

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_cancel_accept_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (!esock_activate_next_acceptor(env, descP, sockRef)) {

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_cancel_accept_current(%T) {%d} -> "
                "no more acceptors\r\n",
                sockRef, descP->sock) );

        descP->readState &= ~ESOCK_STATE_ACCEPTING;

        descP->currentAcceptorP = NULL;
    }

    return res;
}


/* These processes have not performed a select, so we can simply
 * remove them from the acceptor queue.
 */
static
ERL_NIF_TERM essio_cancel_accept_waiting(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef,
                                         const ErlNifPid* selfP)
{
    /* unqueue request from (acceptor) queue */

    if (esock_acceptor_unqueue(env, descP, &opRef, selfP)) {
        return esock_atom_ok;
    } else {
        return esock_atom_not_found;
    }
}



/* ========================================================================
 * Cancel send request
 *
 * Cancel a send operation.
 * Its either the current writer or one of the waiting writers.
 */

extern
ERL_NIF_TERM essio_cancel_send(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_cancel_send(%T), {%d,0x%X} -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n",
            sockRef,  descP->sock, descP->writeState,
            opRef,
            ((descP->currentWriterP == NULL)
             ? "without writer" : "with writer")) );

    if (! IS_OPEN(descP->writeState)) {

        res = esock_make_error_closed(env);

    } else if (descP->currentWriterP == NULL) {

        res = esock_atom_not_found;

    } else {
        ErlNifPid self;

        ESOCK_ASSERT( enif_self(env, &self) != NULL );

        if (COMPARE_PIDS(&self, &descP->currentWriter.pid) == 0) {
            if (COMPARE(opRef, descP->currentWriter.ref) == 0)
                res = essio_cancel_send_current(env, descP, sockRef);
            else
                res = esock_atom_not_found;
        } else {
            res = essio_cancel_send_waiting(env, descP, opRef, &self);
        }
    }

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_cancel_send(%T) {%d} -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, descP->sock, res) );

    return res;
}



/* The current writer process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the writer queue).
 */
static
ERL_NIF_TERM essio_cancel_send_current(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    ESOCK_ASSERT( DEMONP("essio_cancel_send_current -> current writer",
                         env, descP, &descP->currentWriter.mon) == 0);
    res = esock_cancel_write_select(env, descP, descP->currentWriter.ref);

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_cancel_send_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (!esock_activate_next_writer(env, descP, sockRef)) {
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_cancel_send_current(%T) {%d} -> no more writers"
                "\r\n", sockRef, descP->sock) );

        descP->currentWriterP = NULL;
    }

    return res;
}



/* These processes have not performed a select, so we can simply
 * remove them from the writer queue.
 */
static
ERL_NIF_TERM essio_cancel_send_waiting(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     opRef,
                                       const ErlNifPid* selfP)
{
    /* unqueue request from (writer) queue */

    if (esock_writer_unqueue(env, descP, &opRef, selfP)) {
        return esock_atom_ok;
    } else {
        return esock_atom_not_found;
    }
}



/* ========================================================================
 * Cancel receive request
 *
 * Cancel a read operation.
 * Its either the current reader or one of the waiting readers.
 */
extern
ERL_NIF_TERM essio_cancel_recv(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "essio_cancel_recv(%T), {%d,0x%X} -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n",
            sockRef,  descP->sock, descP->readState,
            opRef,
            ((descP->currentReaderP == NULL)
             ? "without reader" : "with reader")) );

    if (! IS_OPEN(descP->readState)) {

        res = esock_make_error_closed(env);

    } else if (descP->currentReaderP == NULL) {

        res =  esock_atom_not_found;

    } else {
        ErlNifPid self;

        ESOCK_ASSERT( enif_self(env, &self) != NULL );

        if (COMPARE_PIDS(&self, &descP->currentReader.pid) == 0) {
            if (COMPARE(opRef, descP->currentReader.ref) == 0)
                res = essio_cancel_recv_current(env, descP, sockRef);
            else
                res =  esock_atom_not_found;
        } else {
            res = essio_cancel_recv_waiting(env, descP, opRef, &self);
        }
    }

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_cancel_recv(%T) {%d} -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, descP->sock, res) );


    return res;

}


/* The current reader process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the reader queue).
 */
static
ERL_NIF_TERM essio_cancel_recv_current(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    ESOCK_ASSERT( DEMONP("essio_cancel_recv_current -> current reader",
                         env, descP, &descP->currentReader.mon) == 0);
    res = esock_cancel_read_select(env, descP, descP->currentReader.ref);

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_cancel_recv_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (!esock_activate_next_reader(env, descP, sockRef)) {
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_cancel_recv_current(%T) {%d} -> no more readers"
                "\r\n", sockRef, descP->sock) );

        descP->currentReaderP = NULL;
    }

    return res;
}


/* These processes have not performed a select, so we can simply
 * remove them from the reader queue.
 */
static
ERL_NIF_TERM essio_cancel_recv_waiting(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     opRef,
                                       const ErlNifPid* selfP)
{
    /* unqueue request from (reader) queue */

    if (esock_reader_unqueue(env, descP, &opRef, selfP)) {
        return esock_atom_ok;
    } else {
        return esock_atom_not_found;
    }
}



/* ========================================================================
 * IOCTL with two args (socket and request "key")
 *
 */
extern
ERL_NIF_TERM essio_ioctl2(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req)
{
  switch (req) {

#if defined(SIOCGIFCONF)
  case SIOCGIFCONF:
      return essio_ioctl_gifconf(env, descP);
      break;
#endif

#if defined(FIONREAD)
  case FIONREAD:
      return essio_ioctl_fionread(env, descP);
      break;
#endif

#if defined(FIONWRITE)
  case FIONWRITE:
      return essio_ioctl_fionwrite(env, descP);
      break;
#endif

#if defined(FIONSPACE)
  case FIONSPACE:
      return essio_ioctl_fionspace(env, descP);
      break;
#endif

#if defined(SIOCATMARK)
  case SIOCATMARK:
      return essio_ioctl_siocatmark(env, descP);
      break;
#endif

  default:
    return esock_make_error(env, esock_atom_enotsup);
    break;
  }

}



/* ========================================================================
 * IOCTL with three args (socket, request "key" and one argument)
 *
 * The type and value of 'arg' depend on the request,
 * which we have not yet "analyzed".
 *
 * Request     arg       arg type
 * -------     -------   --------
 * gifname     ifindex   integer
 * gifindex    name      string
 * gifflags    name      string
 * gifaddr     name      string
 * gifdstaddr  name      string
 * gifbdraddr  name      string
 * gifnetmask  name      string
 * gifmtu      name      string
 * gifhwaddr   name      string
 * genaddr     name      string
 * gifmap      name      string
 * giftxqlen   name      string
 */
extern
ERL_NIF_TERM essio_ioctl3(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req,
			  ERL_NIF_TERM     arg)
{
  /* This for *get* requests */

  switch (req) {

#if defined(SIOCGIFNAME)
  case SIOCGIFNAME:
    return essio_ioctl_gifname(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFINDEX)
  case SIOCGIFINDEX:
    return essio_ioctl_gifindex(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFFLAGS)
  case SIOCGIFFLAGS:
    return essio_ioctl_gifflags(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFADDR)
  case SIOCGIFADDR:
    return essio_ioctl_gifaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFDSTADDR)
  case SIOCGIFDSTADDR:
    return essio_ioctl_gifdstaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFBRDADDR)
  case SIOCGIFBRDADDR:
    return essio_ioctl_gifbrdaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFNETMASK)
  case SIOCGIFNETMASK:
    return essio_ioctl_gifnetmask(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFMTU)
  case SIOCGIFMTU:
    return essio_ioctl_gifmtu(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
  case SIOCGIFHWADDR:
      return essio_ioctl_gifhwaddr(env, descP, arg);
      break;
#endif

#if defined(SIOCGENADDR) && defined(ESOCK_USE_ENADDR)
  case SIOCGENADDR:
      return essio_ioctl_genaddr(env, descP, arg);
      break;
#endif

#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
  case SIOCGIFMAP:
    return essio_ioctl_gifmap(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFTXQLEN)
  case SIOCGIFTXQLEN:
    return essio_ioctl_giftxqlen(env, descP, arg);
    break;
#endif

  default:
    return esock_make_error(env, esock_atom_enotsup);
    break;
  }

}



/* ========================================================================
 * IOCTL with four args (socket, request "key" and two arguments)
 *
 * The type and value of arg(s) depend on the request,
 * which we have not yet "analyzed".
 *
 * Request     arg1      arg1 type    arg2     arg2 type
 * -------     -------   ---------    ------   ---------
 * sifflags    name      string       Flags    #{IntFlag := boolean()}
 *                                             IntFlag is the native flag
 * sifaddr     name      string       Addr     sockaddr()
 * sifdstaddr  name      string       DstAddr  sockaddr()
 * sifbrdaddr  name      string       BrdAddr  sockaddr()
 * sifnetmask  name      string       NetMask  sockaddr()
 * gifmtu      name      string       MTU      integer()
 * sifhwaddr   name      string       HwAddr   sockaddr()
 * giftxqlen   name      string       Len      integer()
 */
extern
ERL_NIF_TERM essio_ioctl4(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req,
			  ERL_NIF_TERM     ename,
			  ERL_NIF_TERM     eval)
{

  switch (req) {

#if defined(SIOCSIFFLAGS)
  case SIOCSIFFLAGS:
    return essio_ioctl_sifflags(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFADDR)
  case SIOCSIFADDR:
    return essio_ioctl_sifaddr(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFDSTADDR)
  case SIOCSIFDSTADDR:
    return essio_ioctl_sifdstaddr(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFBRDADDR)
  case SIOCSIFBRDADDR:
      return essio_ioctl_sifbrdaddr(env, descP, ename, eval);
      break;
#endif

#if defined(SIOCSIFNETMASK)
  case SIOCSIFNETMASK:
    return essio_ioctl_sifnetmask(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFMTU)
  case SIOCSIFMTU:
    return essio_ioctl_sifmtu(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFTXQLEN)
  case SIOCSIFTXQLEN:
    return essio_ioctl_siftxqlen(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFHWADDR)
  case SIOCSIFHWADDR:
      return essio_ioctl_sifhwaddr(env, descP, ename, eval);
      break;
#endif

  default:
    return esock_make_error(env, esock_atom_enotsup);
    break;
  }

}



/* ===========================================================================
 * The implemented (ioctl) get requests falls into three grops:
 *
 *   1) gifconf - Takes no argument other then the request
 *   2) gifname - Takes the interface index (integer) as an argument
 *   3) other   - All other (get) requests takes the interface name (string)
 *                as the argument.
 *
 * The functions defined using the macros below are all in the third (3)
 * group.
 *
 */

static
ERL_NIF_TERM essio_ioctl_gifconf(ErlNifEnv*       env,
				 ESockDescriptor* descP)
{
    struct ifconf ifc;
    int           ifc_len = 0;
    int           buflen  = 100 * sizeof(struct ifreq);
    char         *buf     = MALLOC(buflen);
    ERL_NIF_TERM  result;

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_ioctl_gifconf {%d} -> entry\r\n", descP->sock) );

    for (;;) {
        ifc.ifc_len = buflen;
        ifc.ifc_buf = buf;
        if (ioctl(descP->sock, SIOCGIFCONF, (char *) &ifc) < 0) {
            int saveErrno = sock_errno();

            SSDBG( descP,
                   ("UNIX-ESSIO", "essio_ioctl_gifconf {%d} -> failure: "
                    "\r\n      errno: %d (%s)"
                    "\r\n", descP->sock, saveErrno, erl_errno_id(saveErrno)) );

            if (saveErrno != EINVAL || ifc_len) {
                ERL_NIF_TERM reason = MKA(env, erl_errno_id(saveErrno));
                FREE(buf);
                return esock_make_error(env, reason);
            }
        } else {
            if (ifc.ifc_len == ifc_len) break; /* buf large enough */
            ifc_len = ifc.ifc_len;
        }
        buflen += 10 * sizeof(struct ifreq);
        buf     = (char *) REALLOC(buf, buflen);
    }

    result = encode_ioctl_ifconf(env, descP, &ifc);

    FREE(ifc.ifc_buf);

    return result;
}

/*
  static
  ERL_NIF_TERM essio_ioctl_fionread(ErlNifEnv*       env,
  ESockDescriptor* descP)
  {
  int          n = 0;
  ERL_NIF_TERM result;

  SSDBG( descP,
  ("UNIX-ESSIO",
  "essio_ioctl_fionread(%d) -> entry\r\n", descP->sock) );

  if (ioctl(descP->sock, FIONREAD, (char *) &n) < 0) {
  ERL_NIF_TERM reason;
  int          saveErrno = sock_errno();

  SSDBG( descP,
  ("UNIX-ESSIO", "essio_ioctl_fionread(%d) -> failure: "
  "\r\n      errno: %d (%s)"
  "\r\n", descP->sock, saveErrno, erl_errno_id(saveErrno)) );

  reason = MKA(env, erl_errno_id(saveErrno));

  result = esock_make_error(env, reason);
  } else {

  result = encode_ioctl_ivalue(env, descP, n);

  }

  SSDBG( descP,
  ("UNIX-ESSIO",
  "essio_ioctl_fionread(%d) -> done with: "
  "\r\n   result: %T"
  "\r\n", descP->sock, result) );

  return result;
  }
*/

/* *** essio_ioctl_fionread *** */
#if defined(FIONREAD)
#define IOCTL_FIONREAD_FUNC2_DECL                       \
    IOCTL_GET_REQUEST2_DECL(fionread, FIONREAD, ivalue)
#else
#define IOCTL_FIONREAD_FUNC2_DECL
#endif

/* *** essio_ioctl_fionwrite *** */
#if defined(FIONWRITE)
#define IOCTL_FIONWRITE_FUNC2_DECL                       \
    IOCTL_GET_REQUEST2_DECL(fionwrite, FIONWRITE, ivalue)
#else
#define IOCTL_FIONWRITE_FUNC2_DECL
#endif

/* *** essio_ioctl_fionspace *** */
#if defined(FIONSPACE)
#define IOCTL_FIONSPACE_FUNC2_DECL                              \
    IOCTL_GET_REQUEST2_DECL(fionspace, FIONSPACE, ivalue)
#else
#define IOCTL_FIONSPACE_FUNC2_DECL
#endif

/* *** essio_ioctl_siocatmark *** */
#if defined(SIOCATMARK)
#define IOCTL_SIOCATMARK_FUNC2_DECL                             \
    IOCTL_GET_REQUEST2_DECL(siocatmark, SIOCATMARK, bvalue)
#else
#define IOCTL_FIONSPACE_FUNC2_DECL
#endif

#define IOCTL_GET_FUNCS2                        \
  IOCTL_FIONREAD_FUNC2_DECL			\
  IOCTL_FIONWRITE_FUNC2_DECL			\
  IOCTL_FIONSPACE_FUNC2_DECL			\
  IOCTL_SIOCATMARK_FUNC2_DECL

#define IOCTL_GET_REQUEST2_DECL(OR, R, EF)				\
  static								\
  ERL_NIF_TERM essio_ioctl_##OR(ErlNifEnv*       env,			\
                                ESockDescriptor* descP)			\
  {									\
    ERL_NIF_TERM result;						\
    int          n = 0;							\
									\
    SSDBG( descP,                                                       \
           ("UNIX-ESSIO", "essio_ioctl_" #OR "(%d) -> entry"            \
            "\r\n", descP->sock) );                                     \
									\
    if (ioctl(descP->sock, R, &n) < 0) {                                \
      int          saveErrno = sock_errno();                            \
      ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));       \
									\
      SSDBG( descP,                                                     \
	     ("UNIX-ESSIO", "essio_ioctl_" #OR "(%d) -> failure: "      \
	      "\r\n      reason: %T (%d)"                               \
	      "\r\n", descP->sock, reason, saveErrno) );                \
									\
      result = esock_make_error(env, reason);                           \
									\
    } else {                                                            \
      SSDBG( descP,                                                     \
	     ("UNIX-ESSIO", "essio_ioctl_" #OR "(%d) -> encode value\r\n", \
	      descP->sock) );                                           \
      result = encode_ioctl_##EF(env, descP, n);                        \
    }									\
									\
    SSDBG( descP,                                                       \
        ("UNIX-ESSIO",                                                  \
         "essio_ioctl_" #OR "(%d) -> done with: "                       \
         "\r\n   result: %T"                                            \
         "\r\n", descP->sock, result) );                                \
                                                                        \
    return result;                                                      \
									\
  }
IOCTL_GET_FUNCS2
#undef IOCTL_GET_FUNCS2


/* *** essio_ioctl_gifindex *** */
#if defined(SIOCGIFINDEX)
#if defined(ESOCK_USE_IFINDEX)
#define IOCTL_GIFINDEX_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifindex, SIOCGIFINDEX, ivalue, ifreq.ifr_ifindex)
#elif defined(ESOCK_USE_INDEX)
#define IOCTL_GIFINDEX_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifindex, SIOCGIFINDEX, ivalue, ifreq.ifr_index)
#else
#define IOCTL_GIFINDEX_FUNC3_DECL
#endif
#else
#define IOCTL_GIFINDEX_FUNC3_DECL
#endif

/* *** essio_ioctl_gifflags *** */
#if defined(SIOCGIFFLAGS)
#define IOCTL_GIFFLAGS_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifflags, SIOCGIFFLAGS, flags,  ifreq.ifr_flags)
#else
#define IOCTL_GIFFLAGS_FUNC3_DECL
#endif

/* *** essio_ioctl_gifaddr *** */
#if defined(SIOCGIFADDR)
#define IOCTL_GIFADDR_FUNC3_DECL						\
  IOCTL_GET_REQUEST3_DECL(gifaddr, SIOCGIFADDR, ifraddr, &ifreq.ifr_addr)
#else
#define IOCTL_GIFADDR_FUNC3_DECL
#endif

/* *** essio_ioctl_gifdstaddr *** */
#if defined(SIOCGIFDSTADDR)
#define IOCTL_GIFDSTADDR_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifdstaddr, SIOCGIFDSTADDR, ifraddr, &ifreq.ifr_dstaddr)
#else
#define IOCTL_GIFDSTADDR_FUNC3_DECL
#endif

/* *** essio_ioctl_gifbrdaddr *** */
#if defined(SIOCGIFBRDADDR)
#define IOCTL_GIFBRDADDR_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifbrdaddr, SIOCGIFBRDADDR, ifraddr, &ifreq.ifr_broadaddr)
#else
#define IOCTL_GIFBRDADDR_FUNC3_DECL
#endif

/* *** essio_ioctl_gifnetmask *** */
#if defined(SIOCGIFNETMASK)
#ifdef __linux__
#define IOCTL_GIFNETMASK_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifnetmask, SIOCGIFNETMASK, ifraddr, &ifreq.ifr_netmask)
#else
#define IOCTL_GIFNETMASK_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(gifnetmask, SIOCGIFNETMASK, ifraddr, &ifreq.ifr_addr)
#endif
#else
#define IOCTL_GIFNETMASK_FUNC3_DECL
#endif

/* *** essio_ioctl_gifmtu *** */
#if defined(SIOCGIFMTU)
#define IOCTL_GIFMTU_FUNC3_DECL						\
  IOCTL_GET_REQUEST3_DECL(gifmtu, SIOCGIFMTU, ivalue,  ifreq.ifr_mtu)
#else
#define IOCTL_GIFMTU_FUNC3_DECL
#endif

/* *** essio_ioctl_gifhwaddr *** */
#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
#define IOCTL_GIFHWADDR_FUNC3_DECL					\
    IOCTL_GET_REQUEST3_DECL(gifhwaddr, SIOCGIFHWADDR, hwaddr, &ifreq.ifr_hwaddr)
#else
#define IOCTL_GIFHWADDR_FUNC3_DECL
#endif

/* *** essio_ioctl_genaddr *** */
#if defined(SIOCGENADDR) && defined(ESOCK_USE_ENADDR)
#define IOCTL_GENADDR_FUNC3_DECL					\
    IOCTL_GET_REQUEST3_DECL(genaddr, SIOCGENADDR, enaddr, ifreq.ifr_enaddr)
#else
#define IOCTL_GENADDR_FUNC3_DECL
#endif

/* *** essio_ioctl_gifmap *** */
#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
#define IOCTL_GIFMAP_FUNC3_DECL						\
  IOCTL_GET_REQUEST3_DECL(gifmap, SIOCGIFMAP, ifrmap, &ifreq.ifr_map)
#else
#define IOCTL_GIFMAP_FUNC3_DECL
#endif

/* *** essio_ioctl_giftxqlen *** */
#if defined(SIOCGIFTXQLEN)
#define IOCTL_GIFTXQLEN_FUNC3_DECL					\
  IOCTL_GET_REQUEST3_DECL(giftxqlen, SIOCGIFTXQLEN, ivalue,  ifreq.ifr_qlen)
#else
#define IOCTL_GIFTXQLEN_FUNC3_DECL
#endif

#define IOCTL_GET_FUNCS3                        \
  IOCTL_GIFINDEX_FUNC3_DECL			\
  IOCTL_GIFFLAGS_FUNC3_DECL			\
  IOCTL_GIFADDR_FUNC3_DECL			\
  IOCTL_GIFDSTADDR_FUNC3_DECL			\
  IOCTL_GIFBRDADDR_FUNC3_DECL			\
  IOCTL_GIFNETMASK_FUNC3_DECL			\
  IOCTL_GIFMTU_FUNC3_DECL			\
  IOCTL_GIFHWADDR_FUNC3_DECL			\
  IOCTL_GENADDR_FUNC3_DECL			\
  IOCTL_GIFMAP_FUNC3_DECL			\
  IOCTL_GIFTXQLEN_FUNC3_DECL

#define IOCTL_GET_REQUEST3_DECL(OR, R, EF, UV)				\
  static								\
  ERL_NIF_TERM essio_ioctl_##OR(ErlNifEnv*       env,			\
                                ESockDescriptor* descP,			\
			        ERL_NIF_TERM     ename)			\
  {									\
    ERL_NIF_TERM result;						\
    struct ifreq ifreq;							\
    char*        ifn = NULL;						\
    int          nlen;							\
									\
    SSDBG( descP, ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> entry with" \
		   "\r\n      (e)Name: %T"				\
		   "\r\n", descP->sock, ename) );                       \
									\
    if (!esock_decode_string(env, ename, &ifn))                         \
      return enif_make_badarg(env);                                     \
									\
    nlen = esock_strnlen(ifn, IFNAMSIZ);                                \
									\
    sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ);                         \
    sys_memcpy(ifreq.ifr_name, ifn,                                     \
	       (nlen >= IFNAMSIZ) ? IFNAMSIZ-1 : nlen);                 \
									\
    SSDBG( descP,                                                       \
	   ("UNIX-ESSIO",                                               \
	    "essio_ioctl_" #OR " {%d} -> try ioctl\r\n",                \
	    descP->sock) );                                             \
									\
    if (ioctl(descP->sock, R, (char *) &ifreq) < 0) {                   \
      int          saveErrno = sock_errno();                            \
      ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));       \
									\
      SSDBG( descP,                                                     \
	     ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> failure: "     \
	      "\r\n      reason: %T (%d)"                               \
	      "\r\n", descP->sock, reason, saveErrno) );                \
									\
      result = esock_make_error(env, reason);                           \
									\
    } else {                                                            \
      SSDBG( descP,                                                     \
	     ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> encode value\r\n", \
	      descP->sock) );                                           \
      result = encode_ioctl_##EF(env, descP, UV);                       \
    }									\
									\
    FREE(ifn);								\
									\
    return result;                                                      \
									\
  }
IOCTL_GET_FUNCS3
#undef IOCTL_GET_FUNCS3


/* ===========================================================================
 * The "rest" of the implemented (ioctl) get requests(3)
 *
 * These (get) requests could not be 'generated' by the (simple) macros above.
 */

#if defined(SIOCGIFNAME)
static
ERL_NIF_TERM essio_ioctl_gifname(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 ERL_NIF_TERM     eidx)
{
  ERL_NIF_TERM result;
  struct ifreq ifreq;
  int          index;
  
  SSDBG( descP, ("UNIX-ESSIO", "essio_ioctl_gifname {%d} -> entry with"
		 "\r\n      (e)Index: %T"
		 "\r\n", descP->sock, eidx) );

  if (!GET_INT(env, eidx, &index))
    return enif_make_badarg(env);

  ifreq.ifr_ifindex = index;

  SSDBG( descP,
	 ("UNIX-ESSIO",
          "essio_ioctl_gifname {%d} -> try ioctl\r\n", descP->sock) );

  if (ioctl(descP->sock, SIOCGIFNAME, (char *) &ifreq) < 0) {
    int          saveErrno = sock_errno();
    ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
	   ("UNIX-ESSIO", "essio_ioctl_gifname {%d} -> failure: "
	    "\r\n      reason: %T (%d)"
	    "\r\n", descP->sock, reason, saveErrno) );

    result = esock_make_error(env, reason);

  } else {
    SSDBG( descP,
	   ("UNIX-ESSIO", "essio_ioctl_gifname {%d} -> encode name\r\n",
	    descP->sock) );
    
    result = esock_make_ok2(env, encode_ioctl_ifreq_name(env, ifreq.ifr_name));
  }

  SSDBG( descP,
	 ("UNIX-ESSIO", "essio_ioctl_gifname {%d} -> done with"
	  "\r\n      result: %T"
	  "\r\n",
	  descP->sock, result) );
    
  return result;

}
#endif




/* ===========================================================================
 * The implemented (ioctl) set requests:
 *
 */

/* *** essio_ioctl_sifaddr *** */
#if defined(SIOCSIFADDR)
#define IOCTL_SIFADDR_FUNC_DECL					\
  IOCTL_SET_REQUEST_DECL(sifaddr, SIOCSIFADDR, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_addr))
#else
#define IOCTL_SIFADDR_FUNC_DECL
#endif

/* *** essio_ioctl_sifdstaddr *** */
#if defined(SIOCSIFDSTADDR)
#define IOCTL_SIFDSTADDR_FUNC_DECL				\
  IOCTL_SET_REQUEST_DECL(sifdstaddr, SIOCSIFDSTADDR, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_dstaddr))
#else
#define IOCTL_SIFDSTADDR_FUNC_DECL
#endif

/* *** essio_ioctl_sifbrdaddr *** */
#if defined(SIOCSIFBRDADDR)
#define IOCTL_SIFBRDADDR_FUNC_DECL					\
    IOCTL_SET_REQUEST_DECL(sifbrdaddr, SIOCSIFBRDADDR, sockaddr,        \
                           ((ESockAddress*) &ifreq.ifr_broadaddr))
#else
#define IOCTL_SIFBRDADDR_FUNC_DECL
#endif

/* *** essio_ioctl_sifnetmask *** */
#if defined(SIOCSIFNETMASK)
#ifdef __linux__
#define IOCTL_SIFNETMASK_FUNC_DECL				\
  IOCTL_SET_REQUEST_DECL(sifnetmask, SIOCSIFNETMASK, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_netmask))
#else
#define IOCTL_SIFNETMASK_FUNC_DECL				\
  IOCTL_SET_REQUEST_DECL(sifnetmask, SIOCSIFNETMASK, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_addr))
#endif
#else
#define IOCTL_SIFNETMASK_FUNC_DECL
#endif

/* *** essio_ioctl_sifmtu ***
 * On some platforms, MTU is an unsigned int
 */
#if defined(SIOCSIFMTU)
#define IOCTL_SIFMTU_FUNC_DECL						\
  IOCTL_SET_REQUEST_DECL(sifmtu, SIOCSIFMTU, mtu, (int*) &ifreq.ifr_mtu)
#else
#define IOCTL_SIFMTU_FUNC_DECL
#endif

/* *** essio_ioctl_siftxqlen *** */
#if defined(SIOCSIFTXQLEN)
#define IOCTL_SIFTXQLEN_FUNC_DECL						\
  IOCTL_SET_REQUEST_DECL(siftxqlen, SIOCSIFTXQLEN, txqlen, &ifreq.ifr_qlen)
#else
#define IOCTL_SIFTXQLEN_FUNC_DECL
#endif

/* *** essio_ioctl_sifhqaddr *** */
#if defined(SIOCSIFHWADDR)
#define IOCTL_SIFHWADDR_FUNC_DECL                                     \
    IOCTL_SET_REQUEST_DECL(sifhwaddr, SIOCSIFHWADDR, hwaddr,          \
                           ((ESockAddress*) &ifreq.ifr_hwaddr))
#else
#define IOCTL_SIFHWADDR_FUNC_DECL
#endif

#define IOCTL_SET_FUNCS				\
  IOCTL_SIFADDR_FUNC_DECL			\
  IOCTL_SIFDSTADDR_FUNC_DECL			\
  IOCTL_SIFBRDADDR_FUNC_DECL			\
  IOCTL_SIFNETMASK_FUNC_DECL			\
  IOCTL_SIFMTU_FUNC_DECL			\
  IOCTL_SIFTXQLEN_FUNC_DECL                     \
  IOCTL_SIFHWADDR_FUNC_DECL

#define IOCTL_SET_REQUEST_DECL(OR, R, DF, UVP)				\
  static								\
  ERL_NIF_TERM essio_ioctl_##OR(ErlNifEnv*       env,			\
                                ESockDescriptor* descP,			\
				ERL_NIF_TERM     ename,			\
				ERL_NIF_TERM     evalue)		\
  {									\
    ERL_NIF_TERM result;						\
    struct ifreq ifreq;							\
    char*        ifn = NULL;						\
    int          nlen;							\
                                                                        \
    SSDBG( descP, ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> entry with" \
		   "\r\n      (e)Name:  %T"				\
		   "\r\n      (e)Value: %T"				\
		   "\r\n", descP->sock, ename, evalue) );		\
									\
    if (!esock_decode_string(env, ename, &ifn)) {			\
									\
      SSDBG( descP,							\
	     ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> failed decode name" \
	      "\r\n", descP->sock) );					\
									\
      return enif_make_badarg(env);					\
    }									\
									\
    if (! decode_ioctl_##DF(env, descP, evalue, UVP)) {			\
									\
      SSDBG( descP,							\
	     ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> failed decode addr" \
	      "\r\n", descP->sock) );					\
									\
      return esock_make_invalid(env, esock_atom_##DF);			\
    }									\
									\
    nlen = esock_strnlen(ifn, IFNAMSIZ);				\
									\
    sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ);			        \
    sys_memcpy(ifreq.ifr_name, ifn,					\
	       (nlen >= IFNAMSIZ) ? IFNAMSIZ-1 : nlen);			\
									\
    SSDBG( descP,							\
	   ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> try ioctl\r\n",	\
	    descP->sock) );						\
									\
    if (ioctl(descP->sock, R, (char *) &ifreq) < 0) {			\
      int          saveErrno = sock_errno();				\
      ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));	\
									\
      SSDBG( descP,							\
	     ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> failure: "     \
	      "\r\n      reason: %T (%d)"				\
	      "\r\n", descP->sock, reason, saveErrno) );		\
									\
      result = esock_make_error(env, reason);				\
									\
    } else {								\
      SSDBG( descP,							\
	     ("UNIX-ESSIO", "essio_ioctl_" #OR " {%d} -> "              \
	      "addr successfully set\r\n",				\
	      descP->sock) );						\
      result = esock_atom_ok;						\
    }                                                                   \
                                                                        \
    FREE(ifn);                                                          \
                                                                        \
    return result;                                                      \
                                                                        \
  }

IOCTL_SET_FUNCS
#undef IOCTL_SET_FUNCS


/* ===========================================================================
 * The "rest" of the implemented (ioctl) set requests
 *
 * These (set) requests could not be 'generated' by the macros above.
 */

#if defined(SIOCSIFFLAGS)
static
ERL_NIF_TERM essio_ioctl_sifflags(ErlNifEnv*       env,
				  ESockDescriptor* descP,
				  ERL_NIF_TERM     ename,
				  ERL_NIF_TERM     eflags)
{
  ERL_NIF_TERM result;
  struct ifreq ifreq;
  char*        ifn = NULL;
  int          nlen;

  SSDBG( descP, ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> entry with"
		 "\r\n      (e)Name: %T"
		 "\r\n      (e)Flags: %T"
		 "\r\n", descP->sock, ename, eflags) );

  if (!esock_decode_string(env, ename, &ifn)) {

    SSDBG( descP,
	   ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> failed decode name"
	    "\r\n", descP->sock) );

    return enif_make_badarg(env);
  }

  // Make sure the length of the string is valid!
  nlen = esock_strnlen(ifn, IFNAMSIZ);
  
  sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ); // Just in case
  sys_memcpy(ifreq.ifr_name, ifn, 
	     (nlen >= IFNAMSIZ) ? IFNAMSIZ-1 : nlen);
  
  SSDBG( descP,
	 ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> try (get) ioctl\r\n",
	  descP->sock) );

  if (ioctl(descP->sock, SIOCGIFFLAGS, (char *) &ifreq) < 0) {
    int          saveErrno = sock_errno();
    ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
	   ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> "
	    "failure: failed reading *current* flags"
	    "\r\n      reason: %T (%d)"
	    "\r\n", descP->sock, reason, saveErrno) );

    result = esock_make_error(env, reason);

  } else {

    SSDBG( descP,
	   ("UNIX-ESSIO",
            "essio_ioctl_sifflags {%d} -> (local) update flags\r\n",
	    descP->sock) );

    if (decode_ioctl_flags(env, descP, eflags, &ifreq.ifr_flags)) {

      SSDBG( descP,
	     ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> try (set) ioctl\r\n",
	      descP->sock) );

      if (ioctl(descP->sock, SIOCSIFFLAGS, (char *) &ifreq) < 0) {
	int          saveErrno = sock_errno();
	ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));

	SSDBG( descP,
	       ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> failure: "
		"\r\n      reason: %T (%d)"
		"\r\n", descP->sock, reason, saveErrno) );

	result = esock_make_error(env, reason);

      } else {
	SSDBG( descP,
	       ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> "
		"updated flags successfully set\r\n",
		descP->sock) );
	result = esock_atom_ok;
      }

      /* We know that if esock_decode_string is successful,
       * we have "some" form of string, and therefor memory
       * has been allocated (and need to be freed)... */
      FREE(ifn);

    } else {
      result = enif_make_badarg(env);
    }
  }

  SSDBG( descP,
	 ("UNIX-ESSIO", "essio_ioctl_sifflags {%d} -> done with result: "
	  "\r\n      %T"
	  "\r\n",
	  descP->sock, result) );

  return result;

}
#endif



/* ===========================================================================
 * ioctl utility functions
 *
 */

#if defined(AF_LINK) && !defined(NO_SA_LEN)
#define SIZEA(p) (((p).sa_len > sizeof(p)) ? (p).sa_len : sizeof(p))
#else
#define SIZEA(p) (sizeof (p))
#endif

static
ERL_NIF_TERM encode_ioctl_ifconf(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 struct ifconf*   ifcP)
{
    ERL_NIF_TERM result;
    unsigned int len = (ifcP == NULL) ? 0 : ifcP->ifc_len;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "encode_ioctl_ifconf -> entry with"
            "\r\n   (total) len: %d\r\n", len) );

    if (len > 0) {
        ERL_NIF_TERM  elem, array;
        SocketTArray  tarray = TARRAY_CREATE(32);
        unsigned int  n     = 1; // Just for debugging
        unsigned int  i     = 0;
        unsigned int  sz;
        struct ifreq* ifrP;

        for (;;) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "encode_ioctl_ifconf -> encode entry %d at %d\r\n", n, i) );

            ifrP = (struct ifreq*) VOIDP(ifcP->ifc_buf + i);
            sz   = sizeof(ifrP->ifr_name) + SIZEA(ifrP->ifr_addr);
            if (sz < sizeof(*ifrP)) sz = sizeof(*ifrP);

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "encode_ioctl_ifconf -> "
                    "\r\n   size:     %d"
                    "\r\n      Name len: %d"
                    "\r\n      Addr len: %d"
                    "\r\n      Rec len:  %d"
                    "\r\n",
                    sz,
                    sizeof(ifrP->ifr_name),
                    SIZEA(ifrP->ifr_addr),
                    sizeof(*ifrP)) );

            i += sz;
            if (i > len) break;

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "encode_ioctl_ifconf -> encode new entry\r\n") );

            elem = encode_ioctl_ifconf_ifreq(env, descP, ifrP);

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "encode_ioctl_ifconf -> add new entry: "
                    "\r\n   %T\r\n", elem) );

            TARRAY_ADD(tarray, elem);

            n++;
        }

        SSDBG( descP,
               ("UNIX-ESSIO",
                "encode_ioctl_ifconf -> all entries encoded\r\n") );

        TARRAY_TOLIST(tarray, env, &array);
        result = esock_make_ok2(env, array);

    } else {

        result = esock_make_ok2(env, MKEL(env));

    }

    SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_ifconf -> done\r\n") );

    return result;
}


#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
static
ERL_NIF_TERM encode_ioctl_ifrmap(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 struct ifmap*    mapP)
{
  ERL_NIF_TERM mapKeys[] = {esock_atom_mem_start,
			    esock_atom_mem_end,
			    esock_atom_base_addr,
			    esock_atom_irq,
			    esock_atom_dma,
			    esock_atom_port};
  ERL_NIF_TERM mapVals[] = {MKUL(env, mapP->mem_start),
			    MKUL(env, mapP->mem_end),
			    MKUI(env, mapP->base_addr),
			    MKUI(env, mapP->irq),
			    MKUI(env, mapP->dma),
			    MKUI(env, mapP->port)};
  unsigned int numMapKeys = NUM(mapKeys);
  unsigned int numMapVals = NUM(mapVals);
  ERL_NIF_TERM emap;

  ESOCK_ASSERT( numMapVals == numMapKeys );
  ESOCK_ASSERT( MKMA(env, mapKeys, mapVals, numMapKeys, &emap) );

  SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_ifrmap -> done with"
		 "\r\n    Map: %T"
		 "\r\n", emap) );

  return esock_make_ok2(env, emap);;
}
#endif


#if (defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR))
static
ERL_NIF_TERM encode_ioctl_hwaddr(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 struct sockaddr* addrP)
{
    ERL_NIF_TERM eaddr;
    SOCKLEN_T    sz = sizeof(struct sockaddr);

    esock_encode_hwsockaddr(env, addrP, sz, &eaddr);

    SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_hwaddr -> done with"
                   "\r\n    Sock Addr: %T"
                   "\r\n", eaddr) );

    return esock_make_ok2(env, eaddr);;
}
#endif


#if (defined(SIOCGENADDR) && defined(ESOCK_USE_ENADDR))
static
ERL_NIF_TERM encode_ioctl_enaddr(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 char*            enaddr)
{
    ERL_NIF_TERM eenaddr;
    struct ifreq foo;
    SOCKLEN_T    len = sizeof(foo.ifr_enaddr);

    eenaddr = esock_make_new_binary(env, enaddr, len);

    SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_enaddr -> done with"
                   "\r\n    Gen Addr: %T"
                   "\r\n", eenaddr) );

    return esock_make_ok2(env, eenaddr);;
}
#endif

static
ERL_NIF_TERM encode_ioctl_ifraddr(ErlNifEnv*       env,
				  ESockDescriptor* descP,
				  struct sockaddr* addrP)
{
  ERL_NIF_TERM eaddr;

  esock_encode_sockaddr(env, (ESockAddress*) addrP, -1, &eaddr);

  SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_ifraddr -> done with"
		 "\r\n    Sock Addr: %T"
		 "\r\n", eaddr) );

  return esock_make_ok2(env, eaddr);;
}


static
ERL_NIF_TERM encode_ioctl_flags(ErlNifEnv*       env,
				ESockDescriptor* descP,
				short            flags)
{
    int          i, flag, num = esock_ioctl_flags_length; // NUM(ioctl_flags);
    ERL_NIF_TERM eflags, eflag;
    SocketTArray ta = TARRAY_CREATE(20); // Just to be on the safe side

  if (flags == 0) {
    eflags = MKEL(env);
  } else {
    for (i = 0; (i < num) && (flags != 0); i++) {
      flag = esock_ioctl_flags[i].flag;
      if ((flag != 0) && ((flags & flag) == flag)) {
	eflag  = *(esock_ioctl_flags[i].name);
	flags &= ~flag;

	SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_flags  {%d} -> "
		       "\r\n      i:               %d"
		       "\r\n      found flag:      %T (%d)"
		       "\r\n      remaining flags: %d"
		       "\r\n", descP->sock, i, eflag, flag, flags) );

	TARRAY_ADD(ta, eflag);
      }
    }
    if (flags != 0) {

      SSDBG( descP,
             ("UNIX-ESSIO", "encode_ioctl_flags  {%d} -> unknown flag(s): %d"
              "\r\n", descP->sock, flags) );

      TARRAY_ADD(ta, MKI(env, flags));
    }

    TARRAY_TOLIST(ta, env, &eflags);
  }
  

  SSDBG( descP, ("UNIX-ESSIO", "encode_ioctl_flags -> done with"
		 "\r\n    Flags: %T (%d)"
		 "\r\n", eflags, flags) );

  return esock_make_ok2(env, eflags);
}


static
BOOLEAN_T decode_ioctl_sockaddr(ErlNifEnv*       env,
				ESockDescriptor* descP,
				ERL_NIF_TERM     eaddr,
				ESockAddress*    addr)
{
    SOCKLEN_T addrLen;
    BOOLEAN_T result;

    result = esock_decode_sockaddr(env, eaddr, (ESockAddress*) addr, &addrLen);

    VOID(addrLen);

    SSDBG( descP,
           ("UNIX-ESSIO", "decode_ioctl_sockaddr {%d} -> decode result: %s"
            "\r\n", descP->sock, B2S(result)) );

    return result;
}


#if defined(SIOCSIFHWADDR)
static
BOOLEAN_T decode_ioctl_hwaddr(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     eaddr,
                              ESockAddress*    addr)
{
    SOCKLEN_T addrLen;
    BOOLEAN_T result;

    result = esock_decode_hwsockaddr(env, eaddr,
                                     (ESockAddress*) addr, &addrLen);

    VOID(addrLen);

    SSDBG( descP,
           ("UNIX-ESSIO", "decode_ioctl_hwaddr {%d} -> decode result: %s"
            "\r\n", descP->sock, B2S(result)) );

    return result;
}
#endif



#if defined(SIOCSIFMTU)
static
BOOLEAN_T decode_ioctl_mtu(ErlNifEnv*       env,
			   ESockDescriptor* descP,
			   ERL_NIF_TERM     emtu,
			   int*             mtu)
{
  BOOLEAN_T result;

  if (! GET_INT(env, emtu, mtu)) {
    result = FALSE;
  } else {
    result = TRUE;
  }

  SSDBG( descP,
	 ("UNIX-ESSIO", "decode_ioctl_mtu {%d} -> decode result: %s"
	  "\r\n", descP->sock, B2S(result)) );

  return result;
}
#endif
				 

#if defined(SIOCSIFTXQLEN)
static
BOOLEAN_T decode_ioctl_txqlen(ErlNifEnv*       env,
			      ESockDescriptor* descP,
			      ERL_NIF_TERM     etxqlen,
			      int*             txqlen)
{
  return decode_ioctl_ivalue(env, descP, etxqlen, txqlen);
}
#endif

/* All uses of the function should be added. For instance:
 * #if defined(SIOCGIFTXQLEN) || defined(FOOBAR) || defined(YXA)
 */
#if defined(SIOCGIFTXQLEN)
static
BOOLEAN_T decode_ioctl_ivalue(ErlNifEnv*       env,
			      ESockDescriptor* descP,
			      ERL_NIF_TERM     eivalue,
			      int*             ivalue)
{
  BOOLEAN_T result;

  if (! GET_INT(env, eivalue, ivalue)) {
    result = FALSE;
  } else {
    result = TRUE;
  }

  SSDBG( descP,
	 ("UNIX-ESSIO", "decode_ioctl_ivalue {%d} -> decode result: %s"
	  "\r\n", descP->sock, B2S(result)) );

  return result;
}
#endif
				 

static
BOOLEAN_T decode_ioctl_flags(ErlNifEnv*       env,
			     ESockDescriptor* descP,
			     ERL_NIF_TERM     eflags,
			     short*           flags)
{
  ERL_NIF_TERM      key, value;
  ErlNifMapIterator iter;
  int               tmpFlags = (int) *flags; // Current value
  int               flag;

  SSDBG( descP,
	 ("UNIX-ESSIO", "decode_ioctl_flags {%d} -> entry with"
	  "\r\n      flags: %d"
	  "\r\n",
	  descP->sock, tmpFlags) );

  enif_map_iterator_create(env, eflags, &iter, ERL_NIF_MAP_ITERATOR_FIRST);

  while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {

    /* Convert key (eflag) to int */
    if (! GET_INT(env, key, &flag)) {
      enif_map_iterator_destroy(env, &iter);
      return FALSE;
    }

    // Update flag
    if (COMPARE(value, esock_atom_true) == 0) {
      SSDBG( descP,
	     ("UNIX-ESSIO", "decode_ioctl_flags {%d} -> set %d\r\n",
	      descP->sock, flag) );
      tmpFlags |= flag;
    } else {
      SSDBG( descP,
	     ("UNIX-ESSIO", "decode_ioctl_flags {%d} -> reset %d\r\n",
	      descP->sock, flag) );
      tmpFlags &= ~flag;
    }

    enif_map_iterator_next(env, &iter);
  }

  enif_map_iterator_destroy(env, &iter);

  SSDBG( descP,
	 ("UNIX-ESSIO", "decode_ioctl_flags {%d} -> done with"
	  "\r\n      (new) flags: %d"
	  "\r\n",
	  descP->sock, tmpFlags) );

  *flags = (short) tmpFlags;

  return TRUE;
}


static
ERL_NIF_TERM encode_ioctl_ivalue(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 int              ivalue)
{
    return esock_encode_ioctl_ivalue(env, descP, ivalue);
}

static
ERL_NIF_TERM encode_ioctl_bvalue(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 int              bvalue)
{
    return esock_encode_ioctl_bvalue(env, descP, bvalue);
}

static
ERL_NIF_TERM encode_ioctl_ifconf_ifreq(ErlNifEnv*       env,
				       ESockDescriptor* descP,
				       struct ifreq*    ifrP)
{
  ERL_NIF_TERM ename, eaddr;

  ESOCK_ASSERT( ifrP != NULL );

  SSDBG( descP,
         ("UNIX-ESSIO", "encode_ioctl_ifconf_ifreq -> encode name\r\n") );
  ename = encode_ioctl_ifreq_name(env,     ifrP->ifr_name);

  SSDBG( descP,
         ("UNIX-ESSIO", "encode_ioctl_ifconf_ifreq -> encode sockaddr\r\n") );
  eaddr = encode_ioctl_ifreq_sockaddr(env, &ifrP->ifr_addr);

  SSDBG( descP,
         ("UNIX-ESSIO", "encode_ioctl_ifconf_ifreq -> make ifreq map with"
          "\r\n    Name:      %T"
          "\r\n    Sock Addr: %T"
          "\r\n", ename, eaddr) );
  return make_ifreq(env, ename, esock_atom_addr, eaddr);
}

static
ERL_NIF_TERM encode_ioctl_ifreq_name(ErlNifEnv* env,
				     char*      name)
{
  return ((name == NULL) ? esock_atom_undefined : MKS(env, name));
}

static
ERL_NIF_TERM encode_ioctl_ifreq_sockaddr(ErlNifEnv* env, struct sockaddr* sa)
{
  ERL_NIF_TERM esa;

  if (sa != NULL) {

    esock_encode_sockaddr(env, (ESockAddress*) sa, -1, &esa);

  } else {

      esa = esock_atom_undefined;

  }

  return esa;
}


/* The ifreq structure *always* contain a name
 * and *one* other element. The second element
 * depend on the ioctl request. 
 */
static
ERL_NIF_TERM make_ifreq(ErlNifEnv*   env,
			ERL_NIF_TERM name,
			ERL_NIF_TERM key2,
			ERL_NIF_TERM val2)
{
  ERL_NIF_TERM keys[2];
  ERL_NIF_TERM vals[2];
  ERL_NIF_TERM res;

  keys[0] = esock_atom_name;
  vals[0] = name;

  keys[1] = key2;
  vals[1] = val2;

  ESOCK_ASSERT( MKMA(env, keys, vals, NUM(keys), &res) );

  return res;
}
	   



/* ----------------------------------------------------------------------
 *  U t i l i t y   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* *** send_check_writer ***
 *
 * Checks if we have a current writer and if that is us.
 * If not (current writer), then we must be made to wait
 * for our turn. This is done by pushing us unto the writer queue.
 */
static
BOOLEAN_T send_check_writer(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     ref,
                            ERL_NIF_TERM*    checkResult)
{
    if (descP->currentWriterP != NULL) {
        ErlNifPid caller;
        
        ESOCK_ASSERT( enif_self(env, &caller) != NULL );

        if (COMPARE_PIDS(&descP->currentWriter.pid, &caller) != 0) {
            /* Not the "current writer", so (maybe) push onto queue */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "send_check_writer {%d} -> not (current) writer"
                    "\r\n   ref: %T"
                    "\r\n", descP->sock, ref) );

            if (! esock_writer_search4pid(env, descP, &caller)) {
                esock_writer_push(env, descP, caller, ref, NULL);
                *checkResult = esock_atom_select;
            } else {
                /* Writer already in queue */
                *checkResult = esock_raise_invalid(env, esock_atom_state);
            }
            
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "send_check_writer {%d} -> queue (push) result: %T\r\n"
                    "\r\n   ref: %T"
                    "\r\n", descP->sock, *checkResult, ref) );
            
            return FALSE;
        }
    }

    // Does not actually matter in this case, but ...
    *checkResult = esock_atom_ok;

    return TRUE;
}


/* *** send_check_result ***
 *
 * Check the result of a socket send (send, sendto and sendmsg) call.
 * If a "complete" send has been made, the next (waiting) writer will be 
 * scheduled (if there is one).
 * If we did not manage to send the entire package, make another select,
 * so that we can be informed when we can make another try (to send the rest),
 * and return with the amount we actually managed to send (its up to the caller
 * (that is the erlang code) to figure out hust much is left to send).
 * If the write fail, we give up and return with the appropriate error code.
 *
 * What about the remaining writers!!
 *
 */
static
ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ssize_t          send_result,
                               ssize_t          dataSize,
                               BOOLEAN_T        dataInTail,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     sendRef)
{
    ERL_NIF_TERM res;
    BOOLEAN_T    send_error;
    int          err;

    send_error = ESOCK_IS_ERROR(send_result);
    err = send_error ? sock_errno() : 0;

    SSDBG( descP,
           ("UNIX-ESSIO", "send_check_result(%T) {%d} -> entry with"
            "\r\n   send_result:  %ld"
            "\r\n   dataSize:     %ld"
            "\r\n   err:          %d"
            "\r\n   sendRef:      %T"
            "\r\n", sockRef, descP->sock,
            (long) send_result, (long) dataSize, err, sendRef) );

    if (send_error) {
        /* Some kind of send failure - check what kind */
        if ((err != EAGAIN) && (err != EINTR)) {
            res = send_check_fail(env, descP, err, sockRef);
        } else {
            /* Ok, try again later */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "send_check_result(%T) {%d} -> try again"
                    "\r\n", sockRef, descP->sock) );

            res = send_check_retry(env, descP, -1, sockRef, sendRef);
        }
    } else {
        ssize_t written = send_result;
        ESOCK_ASSERT( dataSize >= written );

        if (written < dataSize) {
            /* Not the entire package */
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "send_check_result(%T) {%d} -> "
                    "not entire package written (%d of %d)"
                    "\r\n", sockRef, descP->sock,
                    written, dataSize) );

            res = send_check_retry(env, descP, written, sockRef, sendRef);
        } else if (dataInTail) {
            /* We sent all we could, but not everything (data in tail) */
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "send_check_result(%T) {%d} -> "
                    "not entire package written (%d but data in tail)"
                    "\r\n", sockRef, descP->sock,
                    written) );

            res =
                send_check_retry(env, descP, written, sockRef,
                                 esock_atom_iov);
        } else {
            res = send_check_ok(env, descP, written, sockRef);
        }
    }

    SSDBG( descP,
           ("UNIX-ESSIO",
            "send_check_result(%T) {%d} -> done:"
            "\r\n   res: %T"
            "\r\n", sockRef, descP->sock,
            res) );

    return res;
}


/* *** send_check_ok ***
 *
 * Processing done upon successful send.
 */
static
ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ssize_t          written,
                           ERL_NIF_TERM     sockRef)
{
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_pkg, &descP->writePkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_byte, &descP->writeByteCnt, written);
    descP->writePkgMaxCnt += written;
    if (descP->writePkgMaxCnt > descP->writePkgMax)
        descP->writePkgMax = descP->writePkgMaxCnt;
    descP->writePkgMaxCnt = 0;

    SSDBG( descP,
           ("UNIX-ESSIO", "send_check_ok(%T) {%d} -> "
            "everything written (%ld) - done\r\n",
            sockRef, descP->sock, written) );

    if (descP->currentWriterP != NULL) {
        ESOCK_ASSERT( DEMONP("send_check_ok -> current writer",
                             env, descP, &descP->currentWriter.mon) == 0);
    }
    /*
     * Ok, this write is done maybe activate the next (if any)
     */
    if (!esock_activate_next_writer(env, descP, sockRef)) {

        SSDBG( descP,
               ("UNIX-ESSIO", "send_check_ok(%T) {%d} -> no more writers\r\n",
                sockRef, descP->sock) );

        descP->currentWriterP = NULL;
    }

    return esock_atom_ok;
}


/* *** send_check_fail ***
 *
 * Processing done upon failed send.
 * An actual failure - we (and everyone waiting) give up.
 */
static
ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              saveErrno,
                             ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_write_fails, &descP->writeFails, 1);

    reason = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
           ("UNIX-ESSIO", "send_check_fail(%T) {%d} -> error: %d (%T)\r\n",
            sockRef, descP->sock, saveErrno, reason) );

    if (saveErrno != EINVAL) {

        /*
         * We assume that anything other then einval (invalid input)
         * is basically fatal (=> all waiting sends are aborted)
         */

        if (descP->currentWriterP != NULL) {

            esock_requestor_release("send_check_fail",
                                    env, descP, &descP->currentWriter);

            send_error_waiting_writers(env, descP, sockRef, reason);

            descP->currentWriterP = NULL;
        }
    }

    return esock_make_error(env, reason);
}


/* *** send_error_waiting_writers ***
 *
 * Process all waiting writers when a fatal error has occurred.
 * All waiting writers will be "aborted", that is a
 * nif_abort message will be sent (with ref and reason).
 */
static
void send_error_waiting_writers(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     reason)
{
    ESockRequestor req;

    req.env = NULL; /* read by writer_pop before free */
    while (esock_writer_pop(env, descP, &req)) {
        SSDBG( descP,
               ("UNIX-ESSIO",
                "send_error_waiting_writers(%T) {%d} -> abort"
                "\r\n   pid:    %T"
                "\r\n   reason: %T"
                "\r\n",
                sockRef, descP->sock, &req.pid, reason) );

        esock_send_abort_msg(env, descP, sockRef, &req, reason);

        (void) DEMONP("send_error_waiting_writers -> pop'ed writer",
                      env, descP, &req.mon);
    }
}


/* *** send_check_retry ***
 *
 * Processing done upon incomplete or blocked send.
 *
 * We failed to write the *entire* packet (anything less
 * then size of the packet, which is 0 <= written < sizeof
 * packet, so schedule the rest for later.
 */
static
ERL_NIF_TERM send_check_retry(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ssize_t          written,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     sendRef)
{
    int          sres;
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "send_check_retry(%T) {%d} -> %ld"
            "\r\n", sockRef, descP->sock, (long) written) );

    if (written >= 0) {
        descP->writePkgMaxCnt += written;

        if (descP->type != SOCK_STREAM) {
            /* Partial write for packet oriented socket
             * - done with packet
             */
            if (descP->writePkgMaxCnt > descP->writePkgMax)
                descP->writePkgMax = descP->writePkgMaxCnt;
            descP->writePkgMaxCnt = 0;

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_write_pkg, &descP->writePkgCnt, 1);
            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_write_byte, &descP->writeByteCnt, written);

            if (descP->currentWriterP != NULL) {
                ESOCK_ASSERT( DEMONP("send_check_retry -> current writer",
                                     env, descP,
                                     &descP->currentWriter.mon) == 0);
            }
            /*
             * Ok, this write is done maybe activate the next (if any)
             */
            if (!esock_activate_next_writer(env, descP, sockRef)) {

                SSDBG( descP,
                       ("UNIX-ESSIO",
                        "send_check_retry(%T) {%d} -> no more writers\r\n",
                        sockRef, descP->sock) );

                descP->currentWriterP = NULL;
            }

            return esock_make_ok2(env, MKI64(env, written));
        } /* else partial write for stream socket */
    } /* else send would have blocked */

    /* Register this process as current writer */

    if (descP->currentWriterP == NULL) {
        /* Register writer as current */

        ESOCK_ASSERT( enif_self(env, &descP->currentWriter.pid) != NULL );
        ESOCK_ASSERT( MONP("send_check_retry -> current writer",
                           env, descP,
                           &descP->currentWriter.pid,
                           &descP->currentWriter.mon) == 0 );
        ESOCK_ASSERT( descP->currentWriter.env == NULL );

        descP->currentWriter.env = esock_alloc_env("current-writer");
        descP->currentWriter.ref =
            CP_TERM(descP->currentWriter.env, sendRef);
        descP->currentWriterP = &descP->currentWriter;
    } else {
        /* Overwrite current writer registration */
        enif_clear_env(descP->currentWriter.env);
        descP->currentWriter.ref = CP_TERM(descP->currentWriter.env, sendRef);
    }

    if (COMPARE(sendRef, esock_atom_iov) == 0) {
        ESOCK_ASSERT( written >= 0 );
        /* IOV iteration - do not select */
        return MKT2(env, esock_atom_iov, MKI64(env, written));
    }

    /* Select write for this process */

    sres = esock_select_write(env, descP->sock, descP, NULL, sockRef, sendRef);

    if (sres < 0) {
        ERL_NIF_TERM reason;

        /* Internal select error */
        ESOCK_ASSERT( DEMONP("send_check_retry - select error",
                             env, descP, &descP->currentWriter.mon) == 0);

        /* Fail all queued writers */
        reason = MKT2(env, esock_atom_select_write, MKI(env, sres));
        esock_requestor_release("send_check_retry - select error",
                                env, descP, &descP->currentWriter);
        send_error_waiting_writers(env, descP, sockRef, reason);
        descP->currentWriterP = NULL;

        res =
            enif_raise_exception(env,
                                 MKT2(env, esock_atom_select_write,
                                      MKI(env, sres)));

    } else {
        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_write_waits, &descP->writeWaits, 1);

        descP->writeState |= ESOCK_STATE_SELECTED;

        if (written >= 0) {
            /* Partial write success */
            res = MKT2(env, esock_atom_select, MKI64(env, written));
        } else {
            /* No write - try again */
            res = esock_atom_select;
        }
    }

    return res;
}


/* *** Control message utility functions *** */

/* +++ decode_cmsghdrs +++
 *
 * Decode a list of cmsg(). There can be 0 or more "blocks".
 *
 * Each element can either be a (erlang) map that needs to be decoded,
 * or a (erlang) binary that just needs to be appended to the control
 * buffer.
 *
 * Our "problem" is that we have no idea how much memory we actually need.
 *
 */

static
BOOLEAN_T decode_cmsghdrs(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     eCMsg,
                          char*            cmsgHdrBufP,
                          size_t           cmsgHdrBufLen,
                          size_t*          cmsgHdrBufUsed)
{
    ERL_NIF_TERM elem, tail, list;
    char*        bufP;
    size_t       rem, used, totUsed = 0;
    unsigned int len;
    int          i;

    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdrs {%d} -> entry with"
                   "\r\n   eCMsg:      %T"
                   "\r\n   cmsgHdrBufP:   0x%lX"
                   "\r\n   cmsgHdrBufLen: %d"
                   "\r\n", descP->sock,
                   eCMsg, cmsgHdrBufP, cmsgHdrBufLen) );

    if (! GET_LIST_LEN(env, eCMsg, &len))
        return FALSE;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "decode_cmsghdrs {%d} -> list length: %d\r\n",
            descP->sock, len) );

    for (i = 0, list = eCMsg, rem  = cmsgHdrBufLen, bufP = cmsgHdrBufP;
         i < len; i++) {
            
        SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdrs {%d} -> process elem %d:"
                       "\r\n   (buffer) rem:     %u"
                       "\r\n   (buffer) totUsed: %u"
                       "\r\n", descP->sock, i, rem, totUsed) );

        /* Extract the (current) head of the (cmsg hdr) list */
        if (! GET_LIST_ELEM(env, list, &elem, &tail))
            return FALSE;
            
        used = 0; // Just in case...
        if (! decode_cmsghdr(env, descP, elem, bufP, rem, &used))
            return FALSE;

        bufP     = CHARP( ULONG(bufP) + used );
        rem      = SZT( rem - used );
        list     = tail;
        totUsed += used;

    }

    *cmsgHdrBufUsed = totUsed;

    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdrs {%d} -> done"
                   "\r\n   all %u ctrl headers processed"
                   "\r\n   totUsed = %lu\r\n",
                   descP->sock, len, (unsigned long) totUsed) );

    return TRUE;
}


/* +++ decode_cmsghdr +++
 *
 * Decode one cmsg(). Put the "result" into the buffer and advance the
 * pointer (of the buffer) afterwards. Also update 'rem' accordingly.
 * But before the actual decode, make sure that there is enough room in 
 * the buffer for the cmsg header (sizeof(*hdr) < rem).
 *
 * The eCMsg should be a map with three fields:
 *
 *     level :: socket | protocol() | integer()
 *     type  :: atom() | integer()
 *                                What values are valid depend on the level
 *     data  :: binary() | integer() | boolean()
 *                                The type of the data depends on
 *     or                         level and type, but can be a binary,
 *                                which means that the data is already coded.
 *     value :: term()            Which is a term matching the decode function
 */

static
BOOLEAN_T decode_cmsghdr(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ERL_NIF_TERM     eCMsg,
                         char*            bufP,
                         size_t           rem,
                         size_t*          used)
{
    ERL_NIF_TERM eLevel, eType, eData, eValue;
    int          level;

    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d} -> entry with"
                   "\r\n   eCMsg: %T"
                   "\r\n", descP->sock, eCMsg) );

    // Get 'level' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_level, &eLevel))
        return FALSE;
    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d} -> eLevel: %T"
                   "\r\n", descP->sock, eLevel) );

    // Get 'type' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_type, &eType))
        return FALSE;
    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d} -> eType:  %T"
                   "\r\n", descP->sock, eType) );

    // Decode Level
    if (! esock_decode_level(env, eLevel, &level))
        return FALSE;
    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d}-> level:  %d\r\n",
                   descP->sock, level) );

    // Get 'data' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_data, &eData)) {

        // Get 'value' field
        if (! GET_MAP_VAL(env, eCMsg, esock_atom_value, &eValue))
            return FALSE;
        SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d} -> eValue:  %T"
                   "\r\n", descP->sock, eValue) );

        // Decode Value
        if (! decode_cmsghdr_value(env, descP, level, eType, eValue,
                                   bufP, rem, used))
            return FALSE;

    } else {

        // Verify no 'value' field
        if (GET_MAP_VAL(env, eCMsg, esock_atom_value, &eValue))
            return FALSE;

        SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d} -> eData:  %T"
                   "\r\n", descP->sock, eData) );

        // Decode Data
        if (! decode_cmsghdr_data(env, descP, level, eType, eData,
                                  bufP, rem, used))
            return FALSE;
    }

    SSDBG( descP, ("UNIX-ESSIO", "decode_cmsghdr {%d}-> used:  %lu\r\n",
                   descP->sock, (unsigned long) *used) );

    return TRUE;
}


static
BOOLEAN_T decode_cmsghdr_value(ErlNifEnv*   env,
                               ESockDescriptor* descP,
                               int          level,
                               ERL_NIF_TERM eType,
                               ERL_NIF_TERM eValue,
                               char*        bufP,
                               size_t       rem,
                               size_t*      usedP)
{
    int type;
    struct cmsghdr* cmsgP     = (struct cmsghdr *) bufP;
    ESockCmsgSpec*  cmsgTable;
    ESockCmsgSpec*  cmsgSpecP = NULL;
    size_t          num       = 0;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "decode_cmsghdr_value {%d} -> entry  \r\n"
            "   eType:  %T\r\n"
            "   eValue: %T\r\n",
            descP->sock, eType, eValue) );

    // We have decode functions only for symbolic (atom) types
    if (! IS_ATOM(env, eType)) {
        SSDBG( descP,
               ("UNIX-ESSIO",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   eType not an atom\r\n",
                descP->sock) );
        return FALSE;
    }

    /* Try to look up the symbolic type
     */
    if (((cmsgTable = esock_lookup_cmsg_table(level, &num)) == NULL) ||
        ((cmsgSpecP = esock_lookup_cmsg_spec(cmsgTable, num, eType)) == NULL) ||
        (cmsgSpecP->decode == NULL)) {
        /* We found no table for this level,
         * we found no symbolic type in the level table,
         * or no decode function for this type
         */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   cmsgTable:  %p\r\n"
                "   cmsgSpecP:  %p\r\n",
                descP->sock, cmsgTable, cmsgSpecP) );
        return FALSE;
    }

    if (! cmsgSpecP->decode(env, eValue, cmsgP, rem, usedP)) {
        // Decode function failed
        SSDBG( descP,
               ("UNIX-ESSIO",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   decode function failed\r\n",
                descP->sock) );
        return FALSE;
    }

    // Successful decode

    type = cmsgSpecP->type;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "decode_cmsghdr_value {%d} -> TRUE:\r\n"
            "   level:   %d\r\n"
            "   type:    %d\r\n",
            "   *usedP:  %lu\r\n",
            descP->sock, level, type, (unsigned long) *usedP) );

    cmsgP->cmsg_level = level;
    cmsgP->cmsg_type = type;
    return TRUE;
}


static
BOOLEAN_T decode_cmsghdr_data(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              level,
                              ERL_NIF_TERM     eType,
                              ERL_NIF_TERM     eData,
                              char*            bufP,
                              size_t           rem,
                              size_t*          usedP)
{
    int             type;
    ErlNifBinary    bin;
    struct cmsghdr* cmsgP     = (struct cmsghdr *) bufP;
    ESockCmsgSpec*  cmsgSpecP = NULL;

    SSDBG( descP,
           ("UNIX-ESSIO",
            "decode_cmsghdr_data {%d} -> entry  \r\n"
            "   eType: %T\r\n"
            "   eData: %T\r\n",
            descP->sock, eType, eData) );

    // Decode Type
    if (! GET_INT(env, eType, &type)) {
        ESockCmsgSpec* cmsgTable = NULL;
        size_t         num       = 0;

        /* Try to look up the symbolic (atom) type
         */
        if ((! IS_ATOM(env, eType)) ||
            ((cmsgTable = esock_lookup_cmsg_table(level, &num)) == NULL) ||
            ((cmsgSpecP = esock_lookup_cmsg_spec(cmsgTable, num, eType)) == NULL)) {
            /* Type was not an atom,
             * we found no table for this level,
             * or we found no symbolic type in the level table
             */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "decode_cmsghdr_data {%d} -> FALSE:\r\n"
                    "   cmsgTable:  %p\r\n"
                    "   cmsgSpecP:  %p\r\n",
                    descP->sock, cmsgTable, cmsgSpecP) );
            return FALSE;
        }

        type = cmsgSpecP->type;
    }

    // Decode Data
    if (GET_BIN(env, eData, &bin)) {
        void *p;

        p = esock_init_cmsghdr(cmsgP, rem, bin.size, usedP);
        if (p == NULL) {
            /* No room for the data
             */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "decode_cmsghdr_data {%d} -> FALSE:\r\n"
                    "   rem:      %lu\r\n"
                    "   bin.size: %lu\r\n",
                    descP->sock,
                    (unsigned long) rem,
                    (unsigned long) bin.size) );
            return FALSE;
        }

        // Copy the binary data
        sys_memcpy(p, bin.data, bin.size);

    } else if ((! esock_cmsg_decode_int(env, eData, cmsgP, rem, usedP)) &&
               (! esock_cmsg_decode_bool(env, eData, cmsgP, rem, usedP))) {
        SSDBG( descP,
               ("UNIX-ESSIO",
                "decode_cmsghdr_data {%d} -> FALSE\r\n",
                descP->sock) );
        return FALSE;
    }

    // Successful decode

    SSDBG( descP,
           ("UNIX-ESSIO",
            "decode_cmsghdr_data {%d} -> TRUE:\r\n"
            "   level:   %d\r\n"
            "   type:    %d\r\n"
            "   *usedP:  %lu\r\n",
            descP->sock, level, type, (unsigned long) *usedP) );

    cmsgP->cmsg_level = level;
    cmsgP->cmsg_type = type;
    return TRUE;
}


/* +++ encode_msg +++
 *
 * Encode a msg() (recvmsg). In erlang its represented as
 * a map, which has a specific set of attributes:
 *
 *     addr (source address) - sockaddr()
 *     iov                   - [binary()]
 *     ctrl                  - [cmsg()]
 *     flags                 - msg_flags()
 */

static
void encode_msg(ErlNifEnv*       env,
                ESockDescriptor* descP,
                ssize_t          read,
                struct msghdr*   msgHdrP,
                ErlNifBinary*    dataBufP,
                ErlNifBinary*    ctrlBufP,
                ERL_NIF_TERM*    eMsg)
{
    ERL_NIF_TERM addr, iov, ctrl, flags;

    SSDBG( descP,
           ("UNIX-ESSIO", "encode_msg {%d} -> entry with"
            "\r\n   read: %ld"
            "\r\n", descP->sock, (long) read) );

    /* The address is not used if we are connected (unless, maybe,
     * family is 'local'), so check (length = 0) before we try to encodel
     */
    if (msgHdrP->msg_namelen != 0) {
        esock_encode_sockaddr(env,
                              (ESockAddress*) msgHdrP->msg_name,
                              msgHdrP->msg_namelen,
                              &addr);
    } else {
        addr = esock_atom_undefined;
    }

    SSDBG( descP,
           ("UNIX-ESSIO", "encode_msg {%d} -> encode iov"
            "\r\n   msg_iovlen: %lu"
            "\r\n",
            descP->sock,
            (unsigned long) msgHdrP->msg_iovlen) );

    esock_encode_iov(env, read,
                     msgHdrP->msg_iov, msgHdrP->msg_iovlen, dataBufP,
                     &iov);

    SSDBG( descP,
           ("UNIX-ESSIO",
            "encode_msg {%d} -> try encode cmsgs\r\n",
            descP->sock) );

    encode_cmsgs(env, descP, ctrlBufP, msgHdrP, &ctrl);

    SSDBG( descP,
           ("UNIX-ESSIO",
            "encode_msg {%d} -> try encode flags\r\n",
            descP->sock) );

    esock_encode_msg_flags(env, descP, msgHdrP->msg_flags, &flags);

    SSDBG( descP,
           ("UNIX-ESSIO", "encode_msg {%d} -> components encoded:"
            "\r\n   addr:  %T"
            "\r\n   ctrl:  %T"
            "\r\n   flags: %T"
            "\r\n", descP->sock, addr, ctrl, flags) );

    {
        ERL_NIF_TERM keys[]  = {esock_atom_iov,
                                esock_atom_ctrl,
                                esock_atom_flags,
                                esock_atom_addr};
        ERL_NIF_TERM vals[]  = {iov, ctrl, flags, addr};
        size_t       numKeys = NUM(keys);
        
        ESOCK_ASSERT( numKeys == NUM(vals) );
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "encode_msg {%d} -> create map\r\n",
                descP->sock) );

        if (msgHdrP->msg_namelen == 0)
            numKeys--; // No addr
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eMsg) );

        SSDBG( descP,
               ("UNIX-ESSIO",
                "encode_msg {%d}-> map encoded\r\n",
                descP->sock) );
    }

    SSDBG( descP,
           ("UNIX-ESSIO", "encode_msg {%d} -> done\r\n", descP->sock) );
}



/* +++ encode_cmsgs +++
 *
 * Encode a list of cmsg(). There can be 0 or more cmsghdr "blocks".
 *
 * Our "problem" is that we have no idea how many control messages
 * we have.
 *
 * The cmsgHdrP arguments points to the start of the control data buffer,
 * an actual binary. Its the only way to create sub-binaries. So, what we
 * need to continue processing this is to turn that into an binary erlang 
 * term (which can then in turn be turned into sub-binaries).
 *
 * We need the cmsgBufP (even though cmsgHdrP points to it) to be able
 * to create sub-binaries (one for each cmsg hdr).
 *
 * The TArray (term array) is created with the size of 128, which should
 * be enough. But if its not, then it will be automatically realloc'ed during
 * add. Once we are done adding hdr's to it, we convert the tarray to a list.
 */

static
void encode_cmsgs(ErlNifEnv*       env,
                  ESockDescriptor* descP,
                  ErlNifBinary*    cmsgBinP,
                  struct msghdr*   msgHdrP,
                  ERL_NIF_TERM*    eCMsg)
{
    ERL_NIF_TERM    ctrlBuf  = MKBIN(env, cmsgBinP); // The *entire* binary
    SocketTArray    cmsghdrs = TARRAY_CREATE(128);
    struct cmsghdr* firstP   = CMSG_FIRSTHDR(msgHdrP);
    struct cmsghdr* currentP;
    
    SSDBG( descP, ("UNIX-ESSIO", "encode_cmsgs {%d} -> entry when"
                   "\r\n   msg ctrl len:  %d"
                   "\r\n   (ctrl) firstP: 0x%lX"
                   "\r\n", descP->sock,
                   msgHdrP->msg_controllen, firstP) );

    for (currentP = firstP;
         /*
          * In *old* versions of darwin, the CMSG_FIRSTHDR does not
          * check the msg_controllen, so we do it here.
          * We should really test this stuff during configure,
          * but for now, this will have to do.
          */
#if defined(__DARWIN__)
         (msgHdrP->msg_controllen >= sizeof(struct cmsghdr)) &&
             (currentP != NULL);
#else
         (currentP != NULL);
#endif
         currentP = CMSG_NXTHDR(msgHdrP, currentP)) {

        SSDBG( descP,
               ("UNIX-ESSIO", "encode_cmsgs {%d} -> process cmsg header when"
                "\r\n   TArray Size: %d"
                "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

        /* MUST check this since on Linux the returned "cmsg" may actually
         * go too far!
         */
        if (((CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP)) >
            msgHdrP->msg_controllen) {

            /* Ouch, fatal error - give up 
             * We assume we cannot trust any data if this is wrong.
             */

            SSDBG( descP,
                   ("UNIX-ESSIO", "encode_cmsgs {%d} -> check failed when: "
                    "\r\n   currentP:           0x%lX"
                    "\r\n   (current) cmsg_len: %d"
                    "\r\n   firstP:             0x%lX"
                    "\r\n   =>                  %d"
                    "\r\n   msg ctrl len:       %d"
                    "\r\n", descP->sock,
                    CHARP(currentP), currentP->cmsg_len, CHARP(firstP),
                    (CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP),
                    msgHdrP->msg_controllen) );

            TARRAY_ADD(cmsghdrs, esock_atom_bad_data);
            break;
            
        } else {
            unsigned char* dataP   = UCHARP(CMSG_DATA(currentP));
            size_t         dataPos = dataP - cmsgBinP->data;
            size_t         dataLen =
                (UCHARP(currentP) + currentP->cmsg_len) - dataP;
            ERL_NIF_TERM
                cmsgHdr,
                keys[]  =
                {esock_atom_level,
                 esock_atom_type,
                 esock_atom_data,
                 esock_atom_value},
                vals[NUM(keys)];
            size_t numKeys = NUM(keys);
            BOOLEAN_T have_value;

            SSDBG( descP,
                   ("UNIX-ESSIO", "encode_cmsgs {%d} -> cmsg header data: "
                    "\r\n   dataPos: %d"
                    "\r\n   dataLen: %d"
                    "\r\n", descP->sock, dataPos, dataLen) );

            vals[0] = esock_encode_level(env, currentP->cmsg_level);
            vals[2] = MKSBIN(env, ctrlBuf, dataPos, dataLen);
            have_value = esock_encode_cmsg(env,
                                           currentP->cmsg_level,
                                           currentP->cmsg_type,
                                           dataP, dataLen, &vals[1], &vals[3]);

            SSDBG( descP,
                   ("UNIX-ESSIO", "encode_cmsgs {%d} -> "
                    "\r\n   %T: %T"
                    "\r\n   %T: %T"
                    "\r\n   %T: %T"
                    "\r\n", descP->sock,
                    keys[0], vals[0], keys[1], vals[1], keys[2], vals[2]) );
            if (have_value)
                SSDBG( descP,
                       ("UNIX-ESSIO", "encode_cmsgs {%d} -> "
                        "\r\n   %T: %T"
                        "\r\n", descP->sock, keys[3], vals[3]) );

            /* Guard against cut-and-paste errors */
            ESOCK_ASSERT( numKeys == NUM(vals) );
            ESOCK_ASSERT( MKMA(env, keys, vals,
                               numKeys - (have_value ? 0 : 1), &cmsgHdr) );

            /* And finally add it to the list... */
            TARRAY_ADD(cmsghdrs, cmsgHdr);
        }
    }

    SSDBG( descP,
           ("UNIX-ESSIO", "encode_cmsgs {%d} -> cmsg headers processed when"
            "\r\n   TArray Size: %d"
            "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

    /* The tarray is populated - convert it to a list */
    TARRAY_TOLIST(cmsghdrs, env, eCMsg);
}



/* *** Sendfile utility functions *** */

/* Platform independent sendfile() function
 *
 * Return < 0  for terminal error
 *        0    for done
 *        > 0  for retry with select
 */
#if defined(HAVE_SENDFILE)
static
int essio_sendfile(ErlNifEnv*       env,
                   ESockDescriptor* descP,
                   ERL_NIF_TERM     sockRef,
                   off_t            offset,
                   size_t*          countP,
                   int*             errP)
{
    size_t pkgSize = 0; // Total sent in this call

    SSDBG( descP, ("UNIX-ESSIO",
                   "essio_sendfile {%d,%d} -> entry"
                   "\r\n   sockRef: %T"
                   "\r\n",
                   descP->sock, descP->sendfileHandle, sockRef) );

    for (;;) {
        size_t  chunk_size = (size_t) 0x20000000UL; // 0.5 GB
        size_t  bytes_sent;
        ssize_t res;
        int     error;

        /* *countP == 0 means send the whole file - use chunk size */
        if ((*countP > 0) && (*countP < chunk_size))
            chunk_size = *countP;

        {
            /* Platform dependent code:
             *   update and check offset, set and check bytes_sent, and
             *       set res to >= 0 and error to 0, or
             *       set res to < 0 and error to sock_errno()
             */
#if defined (__linux__)

            off_t prev_offset;

            prev_offset = offset;
            res =
                sendfile(descP->sock, descP->sendfileHandle,
                         &offset, chunk_size);
            error = (res < 0) ? sock_errno() : 0;

            ESOCK_ASSERT( offset >= prev_offset );
            ESOCK_ASSERT( (off_t) chunk_size >= (offset - prev_offset) );
            bytes_sent = (size_t) (offset - prev_offset);

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_sendfile(%T) {%d,%d}"
                    "\r\n   res:         %d"
                    "\r\n   bytes_sent:  %lu"
                    "\r\n   error:       %d"
                    "\r\n",
                    sockRef, descP->sock, descP->sendfileHandle,
                    res, (unsigned long) bytes_sent, error) );

#elif defined(__FreeBSD__) || defined(__DragonFly__) || defined(__DARWIN__)

            off_t sbytes;

#if defined(__DARWIN__)
            sbytes = (off_t) chunk_size;
            res = (ssize_t)
                sendfile(descP->sendfileHandle, descP->sock, offset,
                         &sbytes, NULL, 0);
#else
            sbytes = 0;
            res = (ssize_t)
                sendfile(descP->sendfileHandle, descP->sock, offset,
                         chunk_size, NULL, &sbytes, 0);
#endif
            error = (res < 0) ? sock_errno() : 0;

            /* For an error return, we do not dare trust that sbytes is set
             * unless the error is ERRNO_BLOCK or EINTR
             * - the man page is to vague
             */
            if ((res < 0) && (error != ERRNO_BLOCK) && (error != EINTR)) {
                sbytes = 0;
            } else {
                ESOCK_ASSERT( sbytes >= 0 );
                ESOCK_ASSERT( (off_t) chunk_size >= sbytes );
                ESOCK_ASSERT( offset + sbytes >= offset );
                offset += sbytes;
            }
            bytes_sent = (size_t) sbytes;

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_sendfile(%T) {%d,%d}"
                    "\r\n   res:         %d"
                    "\r\n   bytes_sent:  %lu"
                    "\r\n   error:       %d"
                    "\r\n",
                    sockRef, descP->sock, descP->sendfileHandle,
                    res, (unsigned long) bytes_sent, error) );

#elif defined(__sun) && defined(__SVR4) && defined(HAVE_SENDFILEV)

            sendfilevec_t sfvec[1];

            sfvec[0].sfv_fd = descP->sendfileHandle;
            sfvec[0].sfv_flag = 0;
            sfvec[0].sfv_off = offset;
            sfvec[0].sfv_len = chunk_size;

            res = sendfilev(descP->sock, sfvec, NUM(sfvec), &bytes_sent);
            error = (res < 0) ? sock_errno() : 0;

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_sendfile(%T) {%d,%d}"
                    "\r\n   res:         %d"
                    "\r\n   bytes_sent:  %lu"
                    "\r\n   error:       %d"
                    "\r\n",
                    sockRef, descP->sock, descP->sendfileHandle,
                    res, (unsigned long) bytes_sent, error) );

            if ((res < 0) && (error == EINVAL)) {
                /* On e.b SunOS 5.10 using sfv_len > file size
                 * lands here - we regard this as a successful send.
                 * All other causes for EINVAL are avoided,
                 * except for .sfv_fd not seekable, which would
                 * give bytes_sent == 0 that we would interpret
                 * as end of file, which is kind of true.
                 */
                res = 0;
            }
            ESOCK_ASSERT( chunk_size >= bytes_sent );
            ESOCK_ASSERT( offset + bytes_sent >= offset );
            offset += bytes_sent;

#else
#error "Unsupported sendfile syscall; update configure test."
#endif

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_sendfile,
                          &descP->sendfileCountersP->cnt, 1);

            if (bytes_sent != 0) {

                pkgSize += bytes_sent;

                ESOCK_CNT_INC(env, descP, sockRef,
                              esock_atom_sendfile_pkg,
                              &descP->sendfileCountersP->pkg,
                              1);
                ESOCK_CNT_INC(env, descP, sockRef,
                              esock_atom_sendfile_byte,
                              &descP->sendfileCountersP->byteCnt,
                              bytes_sent);

                if (pkgSize > descP->sendfileCountersP->pkgMax)
                    descP->sendfileCountersP->pkgMax = pkgSize;
                if ((descP->sendfileCountersP->maxCnt += bytes_sent)
                    > descP->sendfileCountersP->max)
                    descP->sendfileCountersP->max =
                        descP->sendfileCountersP->maxCnt;
            }

            /* *countP == 0 means send whole file */
            if (*countP > 0) {

                *countP -= bytes_sent;

                if (*countP == 0) { // All sent
                    *countP = pkgSize;
                    return 0;
                }
            }

            if (res < 0) {
                if (error == ERRNO_BLOCK) {
                    *countP = pkgSize;
                    return 1;
                }
                if (error == EINTR)
                    continue;
                *errP = error;
                return -1;
            }

            if (bytes_sent == 0) { // End of input file
                *countP = pkgSize;
                return 0;
            }
        }
    } // for (;;)
}


static
ERL_NIF_TERM essio_sendfile_errno(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  int              err)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(err));

    return essio_sendfile_error(env, descP, sockRef, reason);
}


static
ERL_NIF_TERM essio_sendfile_error(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     reason)
{
    SSDBG( descP, ("UNIX-ESSIO",
                   "essio_sendfile_error {%d} -> entry"
                   "\r\n   sockRef: %T"
                   "\r\n   reason:  %T"
                   "\r\n", descP->sock, sockRef, reason) );

    if (descP->sendfileCountersP == NULL) {
        descP->sendfileCountersP = MALLOC(sizeof(ESockSendfileCounters));
        *descP->sendfileCountersP = initESockSendfileCounters;
    }

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_sendfile_fails,
                  &descP->sendfileCountersP->fails, 1);

    /* XXX Should we have special treatment for EINVAL,
     * such as to only fail current operation and activate
     * the next from the queue?
     */

    if (descP->currentWriterP != NULL) {

        (void) DEMONP("essio_sendfile_error",
                      env, descP, &descP->currentWriter.mon);

        /* Fail all queued writers */
        esock_requestor_release("essio_sendfile_error",
                                env, descP, &descP->currentWriter);
        send_error_waiting_writers(env, descP, sockRef, reason);
        descP->currentWriterP = NULL;

    }

    return esock_make_error(env, reason);
}

static
ERL_NIF_TERM essio_sendfile_select(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     sendRef,
                                   size_t           count)
{
    int sres;

    /* Select write for this process */
    sres = esock_select_write(env, descP->sock, descP, NULL, sockRef, sendRef);
    if (sres < 0) {
        ERL_NIF_TERM reason;

        /* Internal select error */
        (void) DEMONP("essio_sendfile_select - failed",
                      env, descP, &descP->currentWriter.mon);

        /* Fail all queued writers */
        reason = MKT2(env, esock_atom_select_write, MKI(env, sres));
        esock_requestor_release("essio_sendfile_select - failed",
                                env, descP, &descP->currentWriter);
        send_error_waiting_writers(env, descP, sockRef, reason);
        descP->currentWriterP = NULL;

        (void) close(descP->sendfileHandle);
        descP->sendfileHandle = INVALID_HANDLE;

        return enif_raise_exception(env, reason);

    } else {
        ErlNifUInt64 bytes_sent;

        SSDBG( descP,
               ("UNIX-ESSIO", "essio_sendfile_select {%d} -> selected"
                "\r\n   sockRef: %T"
                "\r\n   sendRef: %T"
                "\r\n   count:  %lu"
                "\r\n", descP->sock, sockRef, sendRef, (unsigned long) count) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_sendfile_waits,
                      &descP->sendfileCountersP->waits,
                      1);

        descP->writeState |= ESOCK_STATE_SELECTED;
        bytes_sent = (ErlNifUInt64) count;

        return MKT2(env, esock_atom_select, MKUI64(env, bytes_sent));
    }
}


static
ERL_NIF_TERM essio_sendfile_ok(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               size_t           count)
{
    ErlNifUInt64 bytes_sent64u;

    SSDBG( descP,
           ("UNIX-ESSIO", "essio_sendfile_ok {%d} -> entry when done"
            "\r\n   sockRef: %T"
            "\r\n   written: %lu"
            "\r\n", descP->sock, sockRef, (unsigned long) count) );

    if (descP->currentWriterP != NULL) {

        (void) DEMONP("essio_sendfile_ok -> current writer",
                      env, descP, &descP->currentWriter.mon);

        /*
         * Ok, this write is done maybe activate the next (if any)
         */
        if (! esock_activate_next_writer(env, descP, sockRef)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_sendfile_ok {%d} -> no more writers"
                    "\r\n   sockRef: %T"
                    "\r\n",
                    descP->sock, sockRef) );

            descP->currentWriterP = NULL;
        }
    }

    descP->writePkgMaxCnt = 0;
    bytes_sent64u = (ErlNifUInt64) count;

    (void) close(descP->sendfileHandle);
    descP->sendfileHandle = INVALID_HANDLE;

    return esock_make_ok2(env, MKUI64(env, bytes_sent64u));
}

#endif // #ifdef HAVE_SENDFILE


/* ====================================================================
 *
 * NIF (I/O backend) Resource callback functions: dtor, stop and down
 *
 * ====================================================================
 */

extern
void essio_dtor(ErlNifEnv*       env,
                ESockDescriptor* descP)
{
    SGDBG( ("UNIX-ESSIO", "dtor -> entry\r\n") );

    if (IS_SELECTED(descP)) {
        /* We have used the socket in the select machinery,
         * so we must have closed it properly to get here
         */
        ESOCK_ASSERT( IS_CLOSED(descP->readState) );
        ESOCK_ASSERT( IS_CLOSED(descP->writeState) );
        ESOCK_ASSERT( descP->sock == INVALID_SOCKET );
    } else {
        /* The socket is only opened, should be safe to close nonblocking */
        (void) sock_close(descP->sock);
        descP->sock = INVALID_SOCKET;
    }

    SGDBG( ("UNIX-ESSIO", "dtor -> set state and pattern\r\n") );
    descP->readState  |= (ESOCK_STATE_DTOR | ESOCK_STATE_CLOSED);
    descP->writeState |= (ESOCK_STATE_DTOR | ESOCK_STATE_CLOSED);
    descP->pattern     = (ESOCK_DESC_PATTERN_DTOR | ESOCK_STATE_CLOSED);

    if (descP->buf.data != NULL) {
        FREE_BIN(&descP->buf);
    }

    esock_free_env("dtor reader", descP->currentReader.env);
    descP->currentReader.env = NULL;

    esock_free_env("dtor writer", descP->currentWriter.env);
    descP->currentWriter.env = NULL;

    esock_free_env("dtor acceptor", descP->currentAcceptor.env);
    descP->currentAcceptor.env = NULL;

    SGDBG( ("UNIX-ESSIO", "dtor -> try free readers request queue\r\n") );
    esock_free_request_queue(&descP->readersQ);

    SGDBG( ("UNIX-ESSIO", "dtor -> try free writers request queue\r\n") );
    esock_free_request_queue(&descP->writersQ);

    SGDBG( ("UNIX-ESSIO", "dtor -> try free acceptors request queue\r\n") );
    esock_free_request_queue(&descP->acceptorsQ);

#ifdef HAVE_SENDFILE
    ESOCK_ASSERT( descP->sendfileHandle == INVALID_HANDLE );
    if (descP->sendfileCountersP != NULL) {
        FREE(descP->sendfileCountersP);
        descP->sendfileCountersP = NULL;
  }
#endif

    esock_free_env("dtor close env", descP->closeEnv);
    descP->closeEnv = NULL;

    esock_free_env("dtor meta env", descP->meta.env);
    descP->meta.env = NULL;

    SGDBG( ("UNIX-ESSIO", "dtor -> done\r\n") );
}


extern
void essio_stop(ErlNifEnv*       env,
                ESockDescriptor* descP)
{
#ifdef HAVE_SENDFILE
    if (descP->sendfileCountersP != NULL) {
        ESockSendfileCounters* cntP = descP->sendfileCountersP;

        SSDBG( descP, ("UNIX-ESSIO", "esock_stop(%d) ->  sendfileCounters:"
                       "\r\n   cnt:      %lu"
                       "\r\n   byteCnt:  %lu"
                       "\r\n   fails:    %lu"
                       "\r\n   max:      %lu"
                       "\r\n   pkg:      %lu"
                       "\r\n   pkgMax    %lu"
                       "\r\n   tries:    %lu"
                       "\r\n   waits:    %lu"
                       "\r\n",
                       descP->sock,
                       (unsigned long) cntP->cnt,
                       (unsigned long) cntP->byteCnt,
                       (unsigned long) cntP->fails,
                       (unsigned long) cntP->max,
                       (unsigned long) cntP->pkg,
                       (unsigned long) cntP->pkgMax,
                       (unsigned long) cntP->tries,
                       (unsigned long) cntP->waits) );
    }
#endif

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Inform waiting Closer, or close socket
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (! enif_is_pid_undefined(&descP->closerPid)) {
        /* We have a waiting closer process after nif_close()
         * - send message to trigger nif_finalize_close()
         */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "esock_stop(%d) -> send close msg to %T\r\n",
                descP->sock, MKPID(env, &descP->closerPid)) );

        esock_send_close_msg(env, descP, &descP->closerPid);
        /* Message send frees closeEnv */
        descP->closeEnv = NULL;
        descP->closeRef = esock_atom_undefined;

    } else {
        int err;

        /* We do not have a closer process
         * - have to do an unclean (non blocking) close */

#ifdef HAVE_SENDFILE
        if (descP->sendfileHandle != INVALID_HANDLE)
            esock_send_sendfile_deferred_close_msg(env, descP);
#endif

        err = esock_close_socket(env, descP, FALSE);

        if (err != 0)
            esock_warning_msg("[UNIX-ESSIO] Failed closing socket without "
                              "closer process: "
                              "\r\n   Controlling Process: %T"
                              "\r\n   Descriptor:          %d"
                              "\r\n   Errno:               %d (%T)"
                              "\r\n",
                              descP->ctrlPid, descP->sock,
                              err, MKA(env, erl_errno_id(err)));
    }

}


/* A 'down' has occured.
 * Check the possible processes we monitor in turn:
 * closer, controlling process (owner), connector, reader, acceptor and writer.
 *
 */
extern
void essio_down(ErlNifEnv*           env,
                ESockDescriptor*     descP,
                const ErlNifPid*     pidP,
                const ErlNifMonitor* monP)
{
    if (COMPARE_PIDS(&descP->closerPid, pidP) == 0) {

        /* The closer process went down
         * - it will not call nif_finalize_close
         */

        enif_set_pid_undefined(&descP->closerPid);

        if (MON_EQ(&descP->closerMon, monP)) {
            MON_INIT(&descP->closerMon);

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down {%d} -> closer process exit\r\n",
                    descP->sock) );

        } else {
            // The owner is the closer so we used its monitor

            ESOCK_ASSERT( MON_EQ(&descP->ctrlMon, monP) );
            MON_INIT(&descP->ctrlMon);
            enif_set_pid_undefined(&descP->ctrlPid);

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down {%d} -> closer controlling process exit\r\n",
                    descP->sock) );
        }

        /* Since the closer went down there was one,
         * hence esock_close() must have run or scheduled esock_stop(),
         * or the socket has never been "selected" upon.
         */

        if (descP->closeEnv == NULL) {
            int err;

            /* Since there is no closeEnv,
             * esock_close() did not schedule esock_stop()
             * and is about to call esock_finalize_close() but died,
             * or esock_stop() has run, sent close_msg to the closer
             * and cleared ->closeEnv but the closer died
             * - we have to do an unclean (non blocking) socket close here
             */

#ifdef HAVE_SENDFILE
            if (descP->sendfileHandle != INVALID_HANDLE)
                esock_send_sendfile_deferred_close_msg(env, descP);
#endif

            err = esock_close_socket(env, descP, FALSE);
            if (err != 0)
                esock_warning_msg("[UNIX-ESSIO] "
                                  "Failed closing socket for terminating "
                                  "closer process: "
                                  "\r\n   Closer Process: %T"
                                  "\r\n   Descriptor:     %d"
                                  "\r\n   Errno:          %d (%T)"
                                  "\r\n",
                                  MKPID(env, pidP), descP->sock,
                                  err, MKA(env, erl_errno_id(err)));
        } else {
            /* Since there is a closeEnv esock_stop() has not run yet
             * - when it finds that there is no closer process
             *   it will close the socket and ignore the close_msg
             */
            esock_clear_env("essio_down - close-env", descP->closeEnv);
            esock_free_env("essio_down - close-env", descP->closeEnv);
            descP->closeEnv = NULL;
            descP->closeRef = esock_atom_undefined;
        }

    } else if (MON_EQ(&descP->ctrlMon, monP)) {
        MON_INIT(&descP->ctrlMon);
        /* The owner went down */
        enif_set_pid_undefined(&descP->ctrlPid);

        if (IS_OPEN(descP->readState)) {
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down {%d} -> controller process exit"
                    "\r\n   initiate close\r\n",
                    descP->sock) );

            essio_down_ctrl(env, descP, pidP);

            descP->readState  |= ESOCK_STATE_CLOSING;
            descP->writeState |= ESOCK_STATE_CLOSING;

        } else {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down {%d} -> controller process exit"
                    "\r\n   already closed or closing\r\n",
                    descP->sock) );

        }

    } else if (descP->connectorP != NULL &&
               MON_EQ(&descP->connector.mon, monP)) {
        MON_INIT(&descP->connector.mon);

        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down {%d} -> connector process exit\r\n",
                descP->sock) );

        /* connectorP is only set during connection.
         * Forget all about the ongoing connection.
         * We might end up connected, but the process that initiated
         * the connection has died and will never know
         */

        esock_requestor_release("esock_down->connector",
                                env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;

    } else {
        ERL_NIF_TERM sockRef = enif_make_resource(env, descP);

        /* check all operation queue(s): acceptor, writer and reader.
         *
         * Is it really any point in doing this if the socket is closed?
         *
         */

        if (IS_CLOSED(descP->readState)) {
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down(%T) {%d} -> stray down: %T\r\n",
                    sockRef, descP->sock, pidP) );
        } else {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down(%T) {%d} -> other process term\r\n",
                    sockRef, descP->sock) );

            if (descP->currentReaderP != NULL)
                essio_down_reader(env, descP, sockRef, pidP, monP);
            if (descP->currentAcceptorP != NULL)
                essio_down_acceptor(env, descP, sockRef, pidP, monP);
            if (descP->currentWriterP != NULL)
                essio_down_writer(env, descP, sockRef, pidP, monP);
        }
    }

}


/* ==================================================================== */

/* *** Recv/recvfrom/recvmsg utility functions *** */

static
BOOLEAN_T recv_check_entry(ErlNifEnv       *env,
                           ESockDescriptor *descP,
                           ERL_NIF_TERM     recvRef,
                           ERL_NIF_TERM    *retP)
{
    if (! IS_OPEN(descP->readState)) {
        *retP = esock_make_error_closed(env);
        return FALSE;
    }

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentAcceptorP != NULL) {
        *retP = esock_make_error_invalid(env, esock_atom_state);
        return FALSE;
    }

    /* Check if we have a current reader and if that is us. If not,
     * then we must be made to wait for our turn. This is done by pushing
     * us unto the reader queue.
     * Note that we do *not* actually initiate the currentReader structure
     * here, since we do not actually know yet if we need to! We do that in
     * the [recv|recvfrom|recvmsg]_check_result function.
     */
    if (descP->currentReaderP != NULL) {
        ErlNifPid caller;

        ESOCK_ASSERT( enif_self(env, &caller) != NULL );

        if (COMPARE_PIDS(&descP->currentReader.pid, &caller) != 0) {
            /* Not the "current reader", so (maybe) push onto queue */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "recv_check_reader {%d} -> not (current) reader"
                    "\r\n   recvRef: %T"
                    "\r\n", descP->sock, recvRef) );

            if (! esock_reader_search4pid(env, descP, &caller)) {
                if (COMPARE(recvRef, esock_atom_zero) == 0) {
                    *retP = esock_atom_timeout;
                }
                else {
                    esock_reader_push(env, descP, caller, recvRef, NULL);
                    *retP = esock_atom_select;
                }
            } else {
                /* Reader already in queue */
                *retP = esock_raise_invalid(env, esock_atom_state);
            }

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "recv_check_reader {%d} -> queue (push) result: %T\r\n",
                    descP->sock, *retP) );

            return FALSE;
        }
    }
    *retP = esock_atom_ok; /* Ignored */
    return TRUE;
}


/* *** recv_check_result ***
 *
 * Common for all recv* functions; check for end of stream
 * and recv error, set the result term and return TRUE.
 * If neither return FALSE and let the caller handle the message.
 */
static
BOOLEAN_T recv_check_result(ErlNifEnv       *env,
                            ESockDescriptor *descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            ssize_t          readResult,
                            int              saveErrno,
                            ERL_NIF_TERM    *retP)
{
    if ((readResult == 0) && (descP->type == SOCK_STREAM)) {
        ERL_NIF_TERM reason = esock_atom_closed;

        /* Stream closed from other side
         *
         * When a stream socket peer has performed an orderly shutdown,
         * the return value will be 0 (the traditional "end-of-file" return).
         *
         * *We* do never actually try to read 0 bytes!
         *
         * We must also notify any waiting readers!
         */

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_fails, &descP->readFails, 1);

        recv_error_current_reader(env, descP, sockRef, reason);
        /* Return {error, closed} */
        *retP = esock_make_error(env, reason);
        return FALSE;
    }
    else if (readResult < 0) {

        /* +++ Error handling +++ */

        /* 'timeout' | {error, SaveErrno} */
        *retP = recv_check_fail(env, descP, sockRef, recvRef, saveErrno);
        return FALSE;
    }

    return TRUE;
}


/* *** recv_check_full ***
 *
 * This function is called if we filled the allocated buffer.
 * But are we done yet?
 *
 * len = 0 means: Give me everything you have => maybe
 * len > 0 means: Yes
 *
 * Return {more|ok|select_read, Bin}
 */
static
ERL_NIF_TERM recv_check_full(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     recvRef,
                             ssize_t          len,
                             ErlNifBinary    *bufP)
{
    ERL_NIF_TERM res;

    if ((len == 0) &&
        (descP->type == SOCK_STREAM)) {

        /* +++ Give us everything you have got =>     *
         *     (maybe) needs to continue          +++ */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_full(%T) {%d} -> shall we continue reading?"
                "\r\n   bufSz:    %ld"
                "\r\n   rNum:     %u"
                "\r\n   rNumCnt:  %u"
                "\r\n", sockRef, descP->sock,
                (unsigned long) bufP->size, descP->rNum,
                descP->rNumCnt) );

        /* Res = {more|ok|select_read, Bin} */
        res = recv_check_full_maybe_done(env, descP, sockRef, recvRef, bufP);

    } else {

        /* +++ We got exactly as much as we requested => We are done +++ */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_full(%T) {%d} -> [%ld] "
                "we got exactly what we could fit\r\n",
                sockRef, descP->sock, (long) len) );

        /* Res = {ok|select_read, Bin} */
        res = recv_check_full_done(env, descP, sockRef, recvRef, bufP);
    }

    return res;

}


/* *** recv_check_full_maybe_done ***
 *
 * Increment and check rNumCnt.  If it hasn't reached its max
 * (rNum); return       {more, Bin},
 * then more reads should be done,
 * otherwise return     {ok|select_read, Bin}
 * depending on selectRead.
*/
static
ERL_NIF_TERM recv_check_full_maybe_done(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     recvRef,
                                        ErlNifBinary    *bufP)
{
    ERL_NIF_TERM ret;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt,
                  bufP->size);
    descP->readPkgMaxCnt += bufP->size;

    descP->rNumCnt++;
    if (descP->rNumCnt >= descP->rNum) {

        /* Ret = {ok|select_read, Bin} */
        ret = recv_check_full_done(env, descP, sockRef, recvRef, bufP);

    } else {

        /* Yes, we *do* need to continue reading */

        recv_init_current_reader(env, descP, recvRef);

        /* This transfers "ownership" of the *allocated* binary to an
         * erlang term (no need for an explicit free).
         */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_full_maybe_done(%T) {%d} -> [%lu] "
                "we are done for now - read more\r\n",
                sockRef, descP->sock, (unsigned long) bufP->size) );

        /* Ret =  {more, Bin} */
        ret = MKT2(env, esock_atom_more, MKBIN(env, bufP));
    }

    return ret;
}



/* *** recv_check_full_done ***
 *
 * A successful recv and we filled the buffer.
 * - return {ok|select_read, Bin}
 */
static
ERL_NIF_TERM recv_check_full_done(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef,
                                  ErlNifBinary    *bufP)
{
    ERL_NIF_TERM res;

    descP->rNumCnt = 0;

    res = MKBIN(env, bufP);

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, bufP->size);

    descP->readPkgMaxCnt += bufP->size;
    if (descP->readPkgMaxCnt > descP->readPkgMax)
        descP->readPkgMax = descP->readPkgMaxCnt;
    descP->readPkgMaxCnt = 0;

    if (descP->selectRead && (COMPARE(recvRef, esock_atom_zero) != 0)) {
        /* {select_read, Bin} */
        res = MKT2(env, esock_atom_select_read, res);
        return recv_check_select(env, descP, sockRef, recvRef, res);
    }
    else {
        recv_update_current_reader(env, descP, sockRef);

        /* This transfers "ownership" of the *allocated* binary to an
         * erlang term (no need for an explicit free).
         */

        /* Return {ok, Bin} */
        return esock_make_ok2(env, res);
    }
}


/* *** recv_check_fail ***
 *
 * Handle recv failure.
 */
static
ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     recvRef,
                             int              saveErrno)
{
    ERL_NIF_TERM res;

    descP->rNumCnt = 0;

    if (saveErrno == ECONNRESET)  {

        /* +++ Oops - closed +++ */

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_fail(%T) {%d} -> econnreset: closed"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, recvRef) );

        // This is a bit overkill (to count here), but just in case...
        ESOCK_CNT_INC(env, descP, sockRef, esock_atom_read_fails,
                      &descP->readFails, 1);

        /* Res = {error, econnreset} */
        res = recv_check_fail_econnreset(env, descP, sockRef);

    } else if ((saveErrno == ERRNO_BLOCK) ||
               (saveErrno == EAGAIN)) {

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_fail(%T) {%d} -> eagain"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, recvRef) );

        if (COMPARE(recvRef, esock_atom_zero) == 0) {

            recv_update_current_reader(env, descP, sockRef);

            /* Would block and zero time-out - this is a time-out
             * Res = 'timeout'
             */
            res = esock_atom_timeout;
        }
        else {
            /* Res = 'select' */
            res = recv_check_select(env, descP, sockRef, recvRef,
                                    esock_atom_select);
        }

    } else {

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_fail(%T) {%d} -> errno: %d\r\n"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, saveErrno, recvRef) );

        ESOCK_CNT_INC(env, descP, sockRef, esock_atom_read_fails,
                      &descP->readFails, 1);

        /* Res = {error, SaveErrno} */
        res = recv_check_fail_gen(env, descP, sockRef, saveErrno);
    }

    return res;
}


/* *** recv_check_fail_gen ***
 *
 * The recv call had a "general" failure.
 */
static
ERL_NIF_TERM recv_check_fail_gen(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 int              saveErrno)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(saveErrno));

    recv_error_current_reader(env, descP, sockRef, reason);

    /* Return {error, SaveErrno} */
    return esock_make_error(env, reason);
}


/* *** recv_check_fail_econnreset ***
 *
 * We detected that the socket was closed while reading.
 * Inform current and waiting readers.
 */
static
ERL_NIF_TERM recv_check_fail_econnreset(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(ECONNRESET));
    ERL_NIF_TERM res = esock_make_error(env, reason);

    /* <KOLLA>
     *
     * IF THE CURRENT PROCESS IS *NOT* THE CONTROLLING
     * PROCESS, WE NEED TO INFORM IT!!!
     *
     * ALL WAITING PROCESSES MUST ALSO GET THE ERROR!!
     * HANDLED BY THE STOP (CALLBACK) FUNCTION?
     *
     * SINCE THIS IS A REMOTE CLOSE, WE DON'T NEED TO WAIT
     * FOR OUTPUT TO BE WRITTEN (NO ONE WILL READ), JUST
     * ABORT THE SOCKET REGARDLESS OF LINGER???
     *
     * </KOLLA>
     */

    recv_error_current_reader(env, descP, sockRef, reason);

    /* Return {error, econnreset} */
    return res;
}


/* *** recv_check_select ***
 *
 * The recv call should be retried
 * - initiate select read and return Msg or an exception
 */
static
ERL_NIF_TERM recv_check_select(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef,
                               ERL_NIF_TERM     msg)
{
    int          sres;

    /* SELECT for more data */

    if ((sres = esock_select_read(env, descP->sock, descP, NULL,
                                  sockRef, recvRef)) < 0) {
        /* Unlikely that any next reader will have better luck,
         * but why not give them a shot - the queue will be cleared
         */
        recv_update_current_reader(env, descP, sockRef);

        /* Return error({select_read, SRes}) */
        return enif_raise_exception(env,
                                    MKT2(env, esock_atom_select_read,
                                         MKI(env, sres)));
    } else {

        recv_init_current_reader(env, descP, recvRef);

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_select(%T) {%d} -> SELECT for more"
                "\r\n   recvRef: %T"
                "\r\n   msg:     %T"
                "\r\n", sockRef, descP->sock, recvRef, msg) );

        descP->readState |= ESOCK_STATE_SELECTED;
        return msg;
    }
}


/* *** recv_check_partial ***
 *
 * Handle a successful recv which only partly filled the specified buffer.
 *
 * Return {ok|timeout|select|select_read, Bin}
 */
static
ERL_NIF_TERM recv_check_partial(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     recvRef,
                                ssize_t          len,
                                ErlNifBinary    *bufP)
{
    ERL_NIF_TERM res;

    /* Buffer not filled */

    descP->rNumCnt = 0;

    ESOCK_CNT_INC(env, descP, sockRef,
                  esock_atom_read_byte, &descP->readByteCnt, bufP->size);

    descP->readPkgMaxCnt += bufP->size;
    if (descP->readPkgMaxCnt > descP->readPkgMax)
        descP->readPkgMax = descP->readPkgMaxCnt;
    descP->readPkgMaxCnt = 0;

    res = MKBIN(env, bufP);

    if ((descP->type == SOCK_STREAM) && (len > 0)) {

        /* A stream socket with specified read size
         * - more data is needed
         */

        if (COMPARE(recvRef, esock_atom_zero) == 0) {

            /* Polling read */

            ESOCK_CNT_INC(env, descP, sockRef,
                          esock_atom_read_pkg, &descP->readPkgCnt, 1);
            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "recv_check_partial(%T) {%d} -> [%ld] split buffer time-out"
                    "\r\n   recvRef: %T"
                    "\r\n", sockRef, descP->sock, (long) len,
                    recvRef) );

            recv_update_current_reader(env, descP, sockRef);
            /* Res = {timeout, Bin} */
            res = MKT2(env, esock_atom_timeout, res);
        } else {

            /* Incomplete data */

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "recv_check_partial(%T) {%d} -> [%ld]"
                    " only part of message - expect more"
                    "\r\n   recvRef: %T"
                    "\r\n", sockRef, descP->sock, (long) len,
                    recvRef) );


            /* Res = {select, Bin} */
            res = MKT2(env, esock_atom_select, res);
            res = recv_check_select(env, descP, sockRef, recvRef, res);

        }
    } else {

        /* No more data is needed */

        ESOCK_CNT_INC(env, descP, sockRef,
                      esock_atom_read_pkg, &descP->readPkgCnt, 1);

        SSDBG( descP,
               ("UNIX-ESSIO",
                "recv_check_partial(%T) {%d} -> [%ld] split buffer"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, (long) len,
                recvRef) );

        if (descP->selectRead && (COMPARE(recvRef, esock_atom_zero) != 0)) {
            /* Res = {select_read, Bin} */
            res = MKT2(env, esock_atom_select_read, res);
            res = recv_check_select(env, descP, sockRef, recvRef, res);
        }
        else {
            /* Res = {ok, Bin} */
            recv_update_current_reader(env, descP, sockRef);
            res = esock_make_ok2(env, res);
        }
    }

    return res;
}


/* *** recv_init_current_reader ***
 *
 * Initiate (maybe) the currentReader structure of the descriptor.
 * Including monitoring the calling process.
 */
static
void recv_init_current_reader(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     recvRef)
{
    if (descP->currentReaderP == NULL) {

        ESOCK_ASSERT( enif_self(env, &descP->currentReader.pid) != NULL );

        ESOCK_ASSERT( MONP("recv_init_current_reader -> current reader",
                           env, descP,
                           &descP->currentReader.pid,
                           &descP->currentReader.mon) == 0);
        ESOCK_ASSERT(!descP->currentReader.env);

        descP->currentReader.env = esock_alloc_env("current-reader");
        descP->currentReader.ref =
            CP_TERM(descP->currentReader.env, recvRef);
        descP->currentReaderP = &descP->currentReader;
    } else {

        /*
         * This is a retry:
         * We have done, for instance, recv(Sock, X), but only received Y < X.
         * We then call recv again with size = X-Y. So, we then get a new ref.
         * 
         * Make use of the existing environment
         */

        enif_clear_env(descP->currentReader.env);
        descP->currentReader.ref = CP_TERM(descP->currentReader.env, recvRef);
    }
}


/* *** recv_update_current_reader ***
 *
 * Demonitors the current reader process and pop's the reader queue.
 * If there is a waiting (reader) process, then it will be assigned
 * as the new current reader and a new (read) select will be done.
 */

static
void recv_update_current_reader(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef)
{
    if (descP->currentReaderP != NULL) {
        
        ESOCK_ASSERT( DEMONP("recv_update_current_reader",
                             env, descP, &descP->currentReader.mon) == 0);

        if (! esock_activate_next_reader(env, descP, sockRef)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "recv_update_current_reader(%T) {%d} -> no more readers\r\n",
                    sockRef, descP->sock) );

            descP->currentReaderP = NULL;
        }
    }
}


/* *** recv_error_current_reader ***
 *
 * Process the current reader and any waiting readers
 * when a read (fatal) error has occurred.
 * All waiting readers will be "aborted", that is a 
 * nif_abort message will be sent (with ref and reason).
 */

static
void recv_error_current_reader(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     reason)
{
    if (descP->currentReaderP != NULL) {
        ESockRequestor req;

        esock_requestor_release("recv_error_current_reader",
                                env, descP, &descP->currentReader);

        req.env = NULL; /* read by reader_pop before free */
        while (esock_reader_pop(env, descP, &req)) {

            SSDBG( descP,
                   ("UNIX-ESSIO", "recv_error_current_reader(%T) {%d} -> abort"
                    "\r\n   pid:   %T"
                    "\r\n   reason %T"
                    "\r\n", sockRef, descP->sock,
                    req.pid, reason) );

            esock_send_abort_msg(env, descP, sockRef, &req, reason);

            ESOCK_ASSERT( DEMONP("recv_error_current_reader -> pop'ed reader",
                                 env, descP, &req.mon) == 0);
        }

        descP->currentReaderP = NULL;
    }
}


/* *** essio_down_ctrl ***
 *
 * Stop after a downed controller (controlling process = owner process)
 *
 * This is 'extern' because its currently called from prim_socket_nif
 * (esock_setopt_otp_ctrl_proc).
 */
extern
void essio_down_ctrl(ErlNifEnv*           env,
                     ESockDescriptor*     descP,
                     const ErlNifPid*     pidP)
{
    SSDBG( descP,
           ("UNIX-ESSIO", "essio_down_ctrl {%d} ->"
            "\r\n   Pid: %T"
            "\r\n", descP->sock, MKPID(env, pidP)) );

    if (do_stop(env, descP)) {
        /* esock_stop() is scheduled
         * - it has to close the socket
         */
        SSDBG( descP,
               ("UNIX-ESSIO", "essio_down_ctrl {%d} -> stop was scheduled\r\n",
                descP->sock) );
    } else {
        int err;

        /* Socket is not in the select machinery
         * so esock_stop() will not be called
         * - we have to do an unclean (non blocking) socket close here
         */

#ifdef HAVE_SENDFILE
        if (descP->sendfileHandle != INVALID_HANDLE)
            esock_send_sendfile_deferred_close_msg(env, descP);
#endif

        err = esock_close_socket(env, descP, FALSE);
        if (err != 0)
            esock_warning_msg("[UNIX-ESSIO] "
                              "Failed closing socket for terminating "
                              "owner process: "
                              "\r\n   Owner Process:  %T"
                              "\r\n   Descriptor:     %d"
                              "\r\n   Errno:          %d (%T)"
                              "\r\n",
                              MKPID(env, pidP), descP->sock,
                              err, MKA(env, erl_errno_id(err)));
    }
}



/* *** essio_down_acceptor ***
 *
 * Check and then handle a downed acceptor process.
 *
 */
static
void essio_down_acceptor(ErlNifEnv*           env,
                         ESockDescriptor*     descP,
                         ERL_NIF_TERM         sockRef,
                         const ErlNifPid*     pidP,
                         const ErlNifMonitor* monP)
{
    if (MON_EQ(&descP->currentAcceptor.mon, monP)) {
        MON_INIT(&descP->currentAcceptor.mon);
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down_acceptor(%T) {%d} -> "
                "current acceptor - try activate next\r\n",
                sockRef, descP->sock) );
        
        if (!esock_activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down_acceptor(%T) {%d} -> no more writers\r\n",
                    sockRef, descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

    } else {
        
        /* Maybe unqueue one of the waiting acceptors */
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down_acceptor(%T) {%d} -> "
                "not current acceptor - maybe a waiting acceptor\r\n",
                sockRef, descP->sock) );
        
        esock_acceptor_unqueue(env, descP, NULL, pidP);
    }
}


/* *** essio_down_writer ***
 *
 * Check and then handle a downed writer process.
 *
 */

static
void essio_down_writer(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       ERL_NIF_TERM         sockRef,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP)
{
    if (MON_EQ(&descP->currentWriter.mon, monP)) {
        MON_INIT(&descP->currentWriter.mon);
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down_writer(%T) {%d} -> "
                "current writer - try activate next\r\n",
                sockRef, descP->sock) );
        
        if (!esock_activate_next_writer(env, descP, sockRef)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down_writer(%T) {%d} -> no active writer\r\n",
                    sockRef, descP->sock) );

            descP->currentWriterP = NULL;
        }
        
    } else {
        
        /* Maybe unqueue one of the waiting writer(s) */
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down_writer(%T) {%d} -> "
                "not current writer - maybe a waiting writer\r\n",
                sockRef, descP->sock) );
        
        esock_writer_unqueue(env, descP, NULL, pidP);
    }
}


/* *** essio_down_reader ***
 *
 * Check and then handle a downed reader process.
 *
 */

static
void essio_down_reader(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       ERL_NIF_TERM         sockRef,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP)
{
    if (MON_EQ(&descP->currentReader.mon, monP)) {
        MON_INIT(&descP->currentReader.mon);
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down_reader(%T) {%d} -> "
                "current reader - try activate next\r\n",
                sockRef, descP->sock) );
        
        if (! esock_activate_next_reader(env, descP, sockRef)) {

            SSDBG( descP,
                   ("UNIX-ESSIO",
                    "essio_down_reader(%T) {%d} -> no more readers\r\n",
                    sockRef, descP->sock) );

            descP->currentReaderP = NULL;
        }

    } else {
        
        /* Maybe unqueue one of the waiting reader(s) */
        
        SSDBG( descP,
               ("UNIX-ESSIO",
                "essio_down_reader(%T) {%d} -> "
                "not current reader - maybe a waiting reader\r\n",
                sockRef, descP->sock) );
        
        esock_reader_unqueue(env, descP, NULL, pidP);
    }
}


#endif
