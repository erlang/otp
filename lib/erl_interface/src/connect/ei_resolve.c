/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
/* 
 * Interface functions to different versions of gethostbyname
 */

#ifdef VXWORKS
#include <vxWorks.h>
#include <stdio.h>
#include <semLib.h>
#include <hostLib.h>
#include <resolvLib.h>
#include <string.h>
#include <sys/socket.h>
#include <errno.h>
#include <symLib.h>
#include <sysSymTbl.h>

#elif __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#else /* unix of some kind */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>
#endif 

/* common to all platforms */
#include "eidef.h"
#include "ei_resolve.h"
#include "ei_locking.h"

#ifdef HAVE_GETHOSTBYNAME_R

int ei_init_resolve(void)
{
    return 0;			/* Do nothing */
}

#else /* !HAVE_GETHOSTBYNAME_R */

/* we have our own in that case */

/* Make sure this semaphore has been initialized somewhere first. This
 * should probably be done from 'erl'_init() but we do it in the first
 * call to gethostbyname_r() or gethostbyaddr_r().
 */
/* FIXME we don't want globals here, but maybe ok? */
#ifdef _REENTRANT
static ei_mutex_t *ei_gethost_sem = NULL;
#endif /* _REENTRANT */
static int ei_resolve_initialized = 0;
#ifndef __WIN32__
int h_errno;
#endif

#ifdef DEBUG
#define DEBUGF(X) fprintf X
#else
#define DEBUGF(X) /* Nothing */
#endif

#ifdef VXWORKS
/* FIXME problem for threaded ? */
static struct hostent *(*sens_gethostbyname)(const char *name,
					     char *, int) = NULL;
static struct hostent *(*sens_gethostbyaddr)(const char *addr,
					     char *, int) = NULL;
#endif

#ifdef VXWORKS
static int verify_dns_configuration(void);
#endif

/*
 * If we find SENS resolver, use the functions found there, i.e.
 * resolvGetHostByName() and resolvGetHostByAddr(). Otherwise we use
 * our own, which are just wrappers around hostGetByName() and
 * hostGetByAddr(). Here we look up the functions.
 */
int ei_init_resolve(void)
{

#ifdef VXWORKS
  void *sym;
  SYM_TYPE symtype;

  if (symFindByName(sysSymTbl,"resolvGetHostByName",
		    (char **)&sym,&symtype) == OK && 
      verify_dns_configuration()) {
      sens_gethostbyname = sym;
      DEBUGF((stderr,"found SENS resolver - using it for gethostbyname()\n"));
      if (symFindByName(sysSymTbl,"resolvGetHostByAddr",
			(char **)&sym,&symtype) == OK) {
	  sens_gethostbyaddr = sym;
	  DEBUGF((stderr,"found SENS resolver - "
		  "using it for gethostbyaddr()\n"));
      }
      else {
	  DEBUGF((stderr,"SENS resolver not found - "
		  "using default gethostbyaddr()\n"));
      }
  }
  else {
      DEBUGF((stderr,"SENS resolver not found - "
	      "using default gethostbyname()\n"));
  }
#endif /* VXWORKS */

#ifdef _REENTRANT
  ei_gethost_sem = ei_mutex_create();
  if (!ei_gethost_sem)
      return ENOMEM;
#endif /* _REENTRANT */

  ei_resolve_initialized = 1;
  return 0;
}

#ifdef VXWORKS
/*
** Function to verify the DNS configuration on VwXorks SENS.
** Actually configures to a default value if unconfigured...
*/
static int verify_dns_configuration(void) 
{
    /* FIXME problem for threaded ? */ 
    static char resolv_params[sizeof(RESOLV_PARAMS_S)];
    void (*rpg)(char *);
    STATUS (*rps)(char *);
    SYM_TYPE dummy;
    int get_result, set_result;

    get_result = symFindByName(sysSymTbl,"resolvParamsGet", (char **) &rpg, &dummy);
    set_result = symFindByName(sysSymTbl,"resolvParamsSet", (char **) &rps, &dummy);

    if (!(get_result == OK &&
	  set_result == OK))
	return -1;
    (*rpg)(resolv_params);
    if (*resolv_params == '\0') {
	/* It exists, but is not configured, ei_connect would fail
	   if we left it this way... The best we can do is to configure
	   it to use the local host database on the card, as a fallback */
	*resolv_params = (char) 1;
	fprintf(stderr,"Trying to fix up DNS configuration.\n");
	if (((*rps)(resolv_params)) != OK)
	  return -1;
    }
    return 0;
}

#endif    

#if defined(VXWORKS) || _REENTRANT

/* 
 * Copy the contents of one struct hostent to another, i.e. don't just
 * copy the pointers, copy all the data and create new pointers, etc.
 * User must provide a secondary buffer to which the host data can be copied.
 *
 * Returns 0 on success or -1 if buffer is too small for host data 
*/

/* a couple of helpers
 * align: increment buf until it is dword-aligned, reduce len by same amount.
 * advance:  increment buf by n bytes, reduce len by same amount .
 */
#if defined SIZEOF_VOID_P
#define EI_ALIGNBYTES (SIZEOF_VOID_P - 1)
#else
#define EI_ALIGNBYTES (sizeof(void*) - 1)
#endif
#define align_buf(buf,len) for (;(((unsigned)buf) & EI_ALIGNBYTES); (buf)++, len--)
#define advance_buf(buf,len,n) ((buf)+=(n),(len)-=(n))

/* "and now the tricky part..." */
static int copy_hostent(struct hostent *dest, const struct hostent *src, char *buffer, int buflen)
{
  char **pptr;
  int len;
  char **src_aliases   = NULL;
  char **src_addr_list = NULL;

  /* fix initial buffer alignment */
  align_buf(buffer, buflen);

  /* copy the data into our buffer... */
  /* first the easy ones! */
  dest->h_length = src->h_length;
  dest->h_addrtype = src->h_addrtype;

  /* h_name */
  dest->h_name = buffer;
  len = strlen(src->h_name);
  if (buflen < len+1) return -1;
  memmove((char *)dest->h_name,src->h_name,len);
  buffer[len] = (char)0;
  advance_buf(buffer,buflen,len+1);

  /* traverse alias list, collecting the pointers */
  align_buf(buffer, buflen);
  pptr = (char **)buffer;
  dest->h_aliases = pptr;          /* save head of pointer array */
  
  src_aliases = src->h_aliases;
  
  while(*(src_aliases)) {
    if (buflen < sizeof(*pptr)) return -1;
    *pptr = (char *)src_aliases;
    advance_buf(buffer,buflen,sizeof(*pptr));
    src_aliases++;
    pptr++;
  }  
  if (buflen < sizeof(*pptr)) return -1;
  *pptr = NULL;
  advance_buf(buffer,buflen,sizeof(*pptr));

  /* go back to saved position & transfer the alias data */
  pptr = dest->h_aliases;
  while (*pptr) {
    len = strlen(*pptr);
    if (buflen < len+1) return -1;
    memmove(buffer,*pptr,len);     /* copy data to local buffer */
    buffer[len] = (char)0;
    *pptr = buffer;                /* point to own copy now */
    advance_buf(buffer,buflen,len+1);
    pptr++;
  }

  /* traverse address list, collecting the pointers */
  align_buf(buffer, buflen);
  pptr = (char **)buffer;
  dest->h_addr_list = pptr;        /* save head of pointer array */
  
  src_addr_list = src->h_addr_list;
  
  while(*(src_addr_list)) {
    if (buflen < sizeof(*pptr)) return -1;
    *pptr = *src_addr_list;
    advance_buf(buffer,buflen,sizeof(*pptr));
    src_addr_list++;
    pptr++;
  }  
  if (buflen < sizeof(*pptr)) return -1;
  *pptr = NULL;
  advance_buf(buffer,buflen,sizeof(*pptr));

  /* go back to saved position & transfer the addresses */
  /* align_buf(buffer, buflen); */
  pptr = dest->h_addr_list;
  while (*pptr) {
    len = src->h_length;
    if (buflen < len+1) return -1;
    memmove(buffer,*pptr,len);     /* copy data to local buffer */
    buffer[len]=(char)0;           /* not sure if termination is necessary */
    *pptr = buffer;                /* point to own copy now */
    advance_buf(buffer,buflen,len+1);
    pptr++;
  }

  if (buflen < 0) return -1;
  return 0;
}

#undef EI_ALIGNBYTES

/* This function is a pseudo-reentrant version of gethostbyname(). It
 * uses locks to serialize the call to the regular (non-reentrant)
 * gethostbyname() and then copies the data into the user-provided
 * buffers. It's not pretty but it works. 
 *
 * name - name of host to look up
 * hostp - user-supplied structure for returning host entry
 * buffer - user-supplied buffer: storage for the copied host data
 * buflen - length of user-supplied buffer
 * h_errnop - buffer for return status
 *
 * Returns values as for gethostbyname(). Additionally, sets
 * errno=ERANGE and returns NULL if buffer is too small for host data. 
 */

static struct hostent *my_gethostbyname_r(const char *name, 
					  struct hostent *hostp, 
					  char *buffer, 
					  int buflen, 
					  int *h_errnop)
{
  struct hostent dest;
  struct hostent *src;
  struct hostent *rval = NULL;

  if (!ei_resolve_initialized) {
    *h_errnop = NO_RECOVERY;
    return NULL;
  }
  
#ifdef _REENTRANT
  /* === BEGIN critical section === */
  if (ei_mutex_lock(ei_gethost_sem,0) != 0) {
    *h_errnop = NO_RECOVERY;
    return NULL;
  }
#endif /* _REENTRANT */

  /* lookup the data */
  if ((src = ei_gethostbyname(name))) {
    /* copy to caller's buffer */
    if (!copy_hostent(&dest,src,buffer,buflen)) {
      /* success */ 
      *hostp = dest;               
      *h_errnop = 0;
      rval = hostp;
    }
    
    else {
      /* failure - buffer size */
#ifdef __WIN32__
      SetLastError(ERROR_INSUFFICIENT_BUFFER);
#else
      errno = ERANGE;
#endif
      *h_errnop = 0;
    }
  }
  
  else {
    /* failure - lookup */
#ifdef __WIN32__
    *h_errnop = WSAGetLastError();
#else
    *h_errnop = h_errno;
#endif
  }

#ifdef _REENTRANT
  /* === END critical section === */
  ei_mutex_unlock(ei_gethost_sem);
#endif /* _REENTRANT */
  return rval;
}

#endif /* defined(VXWORKS) || _REENTRANT */

#if defined(VXWORKS) || EI_THREADS != false

static struct hostent *my_gethostbyaddr_r(const char *addr,
					  int length, 
					  int type, 
					  struct hostent *hostp,
					  char *buffer,  
					  int buflen, 
					  int *h_errnop)
{
  struct hostent dest;
  struct hostent *src;
  struct hostent *rval = NULL;

  /* FIXME this should have been done in 'erl'_init()? */
  if (!ei_resolve_initialized) {
    *h_errnop = NO_RECOVERY;
    return NULL;
  }

#ifdef _REENTRANT
  /* === BEGIN critical section === */
  if (ei_mutex_lock(ei_gethost_sem,0) != 0) {
    *h_errnop = NO_RECOVERY;
    return NULL;
  }
#endif /* _REENTRANT */

  /* lookup the data */
  if ((src = ei_gethostbyaddr(addr,length,type))) {
    /* copy to caller's buffer */
    if (!copy_hostent(&dest,src,buffer,buflen)) {
      /* success */
      *hostp = dest;                
      *h_errnop = 0;
      rval = hostp;
    }

    else {
      /* failure - buffer size */
#ifdef __WIN32__
      SetLastError(ERROR_INSUFFICIENT_BUFFER);
#else
      errno = ERANGE;
#endif
      *h_errnop = 0;
    }
  }

  else {
    /* failure - lookup */
#ifdef __WIN32__
    *h_errnop = WSAGetLastError();
#else
    *h_errnop = h_errno;
#endif
  }


#ifdef _REENTRANT
  /* === END critical section === */
  ei_mutex_unlock(ei_gethost_sem);
#endif /* _REENTRANT */
  return rval;
}

#endif /* defined(VXWORKS) || EI_THREADS != false */

#endif /* !HAVE_GETHOSTBYNAME_R */


#ifdef __WIN32__
struct hostent *ei_gethostbyname(const char *name)
{
    return gethostbyname(name);
}

struct hostent *ei_gethostbyaddr(const char *addr, int len, int type)
{
    return gethostbyaddr(addr, len, type);
}

#elif VXWORKS


/* these are a couple of substitutes for the real thing when we run on
 * stock vxworks (i.e. no sens).
 *
 * len and type are ignored, but we make up some reasonable values and
 * insert them
 */
static struct hostent *my_gethostbyname(const char *name)
{
  /* FIXME problem for threaded ? */
  static struct hostent h;
  static char hostname[EI_MAXHOSTNAMELEN+1];
  static char *aliases[1] = {NULL};
  static char *addrp[2] = {NULL,NULL};
  static unsigned long addr = 0;

  strcpy(hostname,name);
  if ((addr = (unsigned long)hostGetByName(hostname)) == ERROR) {
    h_errno = HOST_NOT_FOUND;
    return NULL;
  }

  h_errno = 0;
  h.h_name = hostname;
  h.h_aliases = aliases;
  h.h_length = 4;
  h.h_addrtype = AF_INET;
  addrp[0] = (char *)&addr;
  h.h_addr_list = addrp;
  
  return &h;
}

static struct hostent *my_gethostbyaddr(const char *addr, int len, int type)
{
  /* FIXME problem for threaded ? */
  static struct hostent h;
  static char hostname[EI_MAXHOSTNAMELEN+1];
  static char *aliases[1] = { NULL };
  static unsigned long inaddr;
  static char *addrp[2] = {(char *)&inaddr, NULL};

  memmove(&inaddr,addr,sizeof(inaddr));
  
  if ((hostGetByAddr(inaddr,hostname)) == ERROR) {
    h_errno = HOST_NOT_FOUND;
    return NULL;
  }
  
  h_errno = 0;
  h.h_name = hostname;
  h.h_aliases = aliases;
  h.h_length = 4;
  h.h_addrtype = AF_INET;
  h.h_addr_list = addrp;

  return &h;
}

/* use sens functions for these, if found. */
struct hostent *ei_gethostbyname(const char *name)
{
  struct hostent *h = NULL;
  
  if (!sens_gethostbyname) {
    h = my_gethostbyname(name);
  }
  else {
    /* FIXME problem for threaded ? */
    static char buf[1024];
    h = sens_gethostbyname(name,buf,1024);
  }

  return h;
}

struct hostent *ei_gethostbyaddr(const char *addr, int len, int type)
{
  struct hostent *h = NULL;
  
  if (!sens_gethostbyaddr) { 
    h = my_gethostbyaddr(addr,len,type);
  }
  else {
    /* FIXME problem for threaded ? */
    static char buf[1024];
    h = sens_gethostbyaddr(addr,buf,1024);
  }

  return h;
}

struct hostent *ei_gethostbyaddr_r(const char *addr,
				int length, 
				int type, 
				struct hostent *hostp,
				char *buffer,  
				int buflen, 
				int *h_errnop)
{
  struct hostent *h = NULL;
  
  /* use own func if sens function not available */
  if (!sens_gethostbyaddr) {
    h = my_gethostbyaddr_r(addr,length,type,hostp,buffer,buflen,h_errnop);
  }
  else {
    if (!(h = sens_gethostbyaddr(addr,buffer,buflen))) {
      /* sens returns status via errno */
      *h_errnop = errno; 
    }
    else {
      *hostp = *h;
      *h_errnop = 0;
    }
  }
      
  return h;
}

struct hostent *ei_gethostbyname_r(const char *name, 
				    struct hostent *hostp, 
				    char *buffer, 
				    int buflen, 
				    int *h_errnop)
{
  struct hostent *h = NULL;
  
  /* use own func if sens function not available */
  if (!sens_gethostbyname) {
    h = my_gethostbyname_r(name,hostp,buffer,buflen,h_errnop);
  }
  else {
    if (!(h = sens_gethostbyname(name,buffer,buflen))) {
      /* sens returns status via errno */
      *h_errnop = errno; 
    }
    else {
      *hostp = *h;
      *h_errnop = 0;
    }
  }
      
  return h;
}

#else /* unix of some kind */

struct hostent *ei_gethostbyname(const char *name)
{
    return gethostbyname(name);
}

struct hostent *ei_gethostbyaddr(const char *addr, int len, int type)
{
    return gethostbyaddr(addr, len, type);
}

struct hostent *ei_gethostbyaddr_r(const char *addr,
				int length, 
				int type, 
				struct hostent *hostp,
				char *buffer,  
				int buflen, 
				int *h_errnop)
{
#if (EI_THREADS == false)
  /* threads disabled, no need to call reentrant function */
  return gethostbyaddr(addr, length, type); 
#else
#ifndef HAVE_GETHOSTBYNAME_R
  return my_gethostbyaddr_r(addr,length,type,hostp,buffer,buflen,h_errnop);
#else
#if (defined(__GLIBC__) || defined(__linux__) || (__FreeBSD_version >= 602000) || defined(__DragonFly__))
  struct hostent *result;

  gethostbyaddr_r(addr, length, type, hostp, buffer, buflen, &result,
		h_errnop);

  return result;
#else
  return gethostbyaddr_r(addr,length,type,hostp,buffer,buflen,h_errnop);
#endif
#endif
#endif
}

struct hostent *ei_gethostbyname_r(const char *name, 
				    struct hostent *hostp, 
				    char *buffer, 
				    int buflen, 
				    int *h_errnop)
{
#ifndef _REENTRANT
  /* threads disabled, no need to call reentrant function */
  return gethostbyname(name);
#else
#ifndef HAVE_GETHOSTBYNAME_R
  return my_gethostbyname_r(name,hostp,buffer,buflen,h_errnop);
#else
#if (defined(__GLIBC__) || defined(__linux__) || (__FreeBSD_version >= 602000) || defined(__DragonFly__) || defined(__ANDROID__))
  struct hostent *result;
  int err;

  err = gethostbyname_r(name, hostp, buffer, buflen, &result, h_errnop);
  if (err == ERANGE)
      *h_errnop = err;

  return result;
#else
  return gethostbyname_r(name,hostp,buffer,buflen,h_errnop);
#endif
#endif
#endif
}

#endif /* vxworks, win, unix */

