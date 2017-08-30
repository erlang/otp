/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

 */
#ifndef _RECLAIM_H
#define _RECLAIM_H


/* The Erlang release for VxWorks includes a simple mechanism for
   "resource reclamation" at task exit - it allows replacement of the
   functions that open/close "files" and malloc/free memory with versions
   that keep track, to be able to "reclaim" file descriptors and memory
   when a task exits (regardless of *how* it exits).

   The interface to this mechanism is made available via this file,
   with the following caveats:

   - The interface may change (or perhaps even be removed, though that
     isn't likely until VxWorks itself provides similar functionality)
     in future releases - i.e. you must always use the version of this
     file that comes with the Erlang release you are using.

   - Disaster is guaranteed if you use the mechanism incorrectly (see
     below for the correct way), e.g. allocate memory with the "tracking"
     version of malloc() and free it with the "standard" version of free().

   - The mechanism (of course) incurs some performance penalty - thus
     for a simple program you may be better off with careful programming,
     making sure that you do whatever close()/free()/etc calls that are
     appropriate at all exit points (though if you need to guard against
     taskDelete() etc, things get messy...).

   To use the mechanism, simply program your application normally, i.e.
   use open()/close()/malloc()/free() etc as usual, but #include this
   file before any usage of the relevant functions. NOTE: To avoid the
   "disaster" mentioned above, you *must* #include it in *all* (or none)
   of the files that manipulate a particular file descriptor, allocated
   memory area, etc.

   Before any task that uses this utility is loaded (which includes the 
   erlang emulator), the reclaim.o object file has to be loaded and
   the function reclaim_init() has to be called. reclaim_init should be called
   only _ONCE_ in a systems lifetime and has only a primitive guard
   against multiple calls (i.e. a global variable is checked). Therefore
   the initialization should occur either in the start script of the system
   or (even better) in the usrInit() part of system initialization. The 
   object file itself should be loaded only once, so linking it with the
   kernel is a good idea, linking with each application is an extremely bad 
   dito. Make really sure that it's loaded _before_ any application that 
   uses it if You want to load it in the startup script.

   If You dont want to have #define's for the posix/stdio names
   of the file/memory operations (i.e. no #define malloc save_malloc etc),
   #define RECLAIM_NO_ALIAS in Your source before reclaim.h is included. 
*/

#include <vxWorks.h> /* STATUS, size_t  */
#include <sockLib.h> /* struct sockaddr */ 
#include <stdio.h>   /* FILE            */

#if defined(__STDC__)
#define _RECLAIM_DECL_FUN(RetType, FunName, ParamList) \
extern RetType FunName##ParamList
#define _RECLAIM_VOID_PTR void *
#define _RECLAIM_VOID_PARAM void
#define _RECLAIM_VOID_RETURN void
#elif defined(__cplusplus)
#define _RECLAIM_DECL_FUN(RetType, FunName, ParamList) \
extern "C" RetType FunName##ParamList
#define _RECLAIM_VOID_PTR void *
#define _RECLAIM_VOID_PARAM 
#define _RECLAIM_VOID_RETURN void
#else
#define _RECLAIM_DECL_FUN(RetType, FunName, Ignore) extern RetType FunName()
#define DECLARE_FUNCTION_TYPE(RetType, Type, PList) typedef RetType (* Type)()
#define _RECLAIM_VOID_PTR char *
#define _RECLAIM_VOID_PARAM 
#define _RECLAIM_VOID_RETURN
#endif /* __STDC__ / __cplusplus */

/* Initialize the facility, on a per system basis. */
_RECLAIM_DECL_FUN(STATUS, reclaim_init, (_RECLAIM_VOID_PARAM)); 

/* File descriptor operations */
_RECLAIM_DECL_FUN(int,save_open,(char *, int, ...));
_RECLAIM_DECL_FUN(int,save_creat,(char *, int));
_RECLAIM_DECL_FUN(int,save_socket,(int, int, int));
_RECLAIM_DECL_FUN(int,save_accept,(int, struct sockaddr *, int *));
_RECLAIM_DECL_FUN(int,save_close,(int)); 
/* Interface to add an fd to what's reclaimed even though it's not open with 
   one of the above functions */
_RECLAIM_DECL_FUN(_RECLAIM_VOID_RETURN, save_fd, (int fd));
#ifndef RECLAIM_NO_ALIAS
#define open	save_open
#define creat	save_creat
#define socket	save_socket
#define accept	save_accept
#define close	save_close
#endif
/* Stdio file operations */
_RECLAIM_DECL_FUN(FILE *, save_fopen, (char *, char *));
_RECLAIM_DECL_FUN(FILE *, save_fdopen, (int, char *));
_RECLAIM_DECL_FUN(FILE *, save_freopen, (char *, char *, FILE *));
_RECLAIM_DECL_FUN(int, save_fclose, (FILE *));
/* XXX Should do opendir/closedir too... */
#ifndef RECLAIM_NO_ALIAS
#define fopen	save_fopen
#define fdopen	save_fdopen
#define freopen	save_freopen
#define fclose	save_fclose
#endif
/* Memory allocation */
_RECLAIM_DECL_FUN(_RECLAIM_VOID_PTR, save_malloc, (size_t));
_RECLAIM_DECL_FUN(_RECLAIM_VOID_PTR, save_calloc, (size_t, size_t));
_RECLAIM_DECL_FUN(_RECLAIM_VOID_PTR, save_realloc, 
		  (_RECLAIM_VOID_PTR, size_t));
_RECLAIM_DECL_FUN(void, save_free, (_RECLAIM_VOID_PTR));
_RECLAIM_DECL_FUN(void, save_cfree, (_RECLAIM_VOID_PTR));
#ifndef RECLAIM_NO_ALIAS
#define malloc	save_malloc
#define calloc	save_calloc
#define realloc	save_realloc
#define free	save_free
#define cfree	save_cfree
#endif
/* Generic interfaces to malloc etc... */
_RECLAIM_DECL_FUN(_RECLAIM_VOID_PTR, plain_malloc, (size_t));
_RECLAIM_DECL_FUN(_RECLAIM_VOID_PTR, plain_realloc, 
		  (_RECLAIM_VOID_PTR, size_t));
_RECLAIM_DECL_FUN(void, plain_free, (_RECLAIM_VOID_PTR));
#endif /* _RECLAIM_H */




