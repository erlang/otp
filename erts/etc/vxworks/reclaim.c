/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

#include <vxWorks.h>
#include <version.h>
#include <string.h>
#include <types.h>
#include <sigLib.h>
#include <ioLib.h>
#include <iosLib.h>
#include <fioLib.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <symLib.h>
#include <sysLib.h>
#include <sysSymTbl.h>
#include <loadLib.h>
#include <taskLib.h>
#include <taskVarLib.h>
#include <taskHookLib.h>
#include <tickLib.h>
#include <time.h>
#include <rngLib.h>
#include <semLib.h>
#include <selectLib.h>
#include <sockLib.h>
#include <a_out.h>
#include <wdLib.h>
#include <timers.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <stdarg.h>

#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>



#define RECLAIM_NO_ALIAS /* No #defines for open/malloc/fopen etc */
#include "reclaim.h"
#include "reclaim_private.h"

#undef open
#undef creat
#undef socket
#undef accept
#undef close
#undef fopen
#undef fdopen
#undef freopen
#undef fclose
/* XXX Should do opendir/closedir too... */
#undef malloc
#undef calloc
#undef realloc
#undef free
#undef cfree

#ifdef _ARCH_PPC 
#define MAX_FILES_SYM_NAME "maxFiles"
#else
#define MAX_FILES_SYM_NAME "_maxFiles"
#endif


/* 
 * Use another free() function upon task deletion? 
 * Note! When changing free function, the new free function will have to
 * be able to cope with ALL previously used malloced areas, that is 
 * it has to be able to find out how things are malloced
 * to free them in the right way!
 */
static FreeFunction reclaim_free_function = NULL;

/* delete hook handling (see below) */
static int hook_added = 0;	/* Initated at first reclaim_init, an extra
				 non MT-safe check that we only get 
				 initialized once */

/* Forward... */
static void save_reclaim(WIND_TCB *tcbp);

struct mall_data {
    struct mall_data *next;
    char *self;
};

struct task_data {
    FUNCPTR version;		/* To recognize when we have reloaded */
    int max_files;		/* It may change... */
    struct fd_set open_fds;
    struct mall_data *mall_data;
    FUNCPTR delete_hook;
    caddr_t hook_data;
    FILE *open_fps[1];		/* Will be max_files long */
} *task_data = NULL;

static int max_files = 50;	/* default configAll.h */

int reclaim_max_files(void)
{
    return max_files;
}

#ifdef DEBUG
#define check_hook() \
((task_data != NULL || \
  fdprintf(2,"check_hook() TID = 0x%08x, Called from line %d\n", \
	  (unsigned int)taskIdSelf(),\
	  __LINE__)) && \
(task_data != NULL || \
 (taskVarAdd(0, (int *)&task_data) == OK && \
 (task_data = (struct task_data *)\
 calloc(1, sizeof(struct task_data) + max_files*sizeof(FILE *))) != NULL && \
   (task_data->version = (FUNCPTR)save_reclaim) != NULL && \
   (task_data->max_files = max_files) != 0 && \
  fdprintf(2,"taskVar Added for 0x%08x\n",(unsigned int)taskIdSelf()))))
#else
#define check_hook() \
(task_data != NULL || \
 (taskVarAdd(0, (int *)&task_data) == OK && \
 (task_data = (struct task_data *)\
 calloc(1, sizeof(struct task_data) + max_files*sizeof(FILE *))) != NULL && \
   (task_data->version = (FUNCPTR)save_reclaim) != NULL && \
   (task_data->max_files = max_files) != 0))
#endif

/*
 * Global initialization of the reclaim data structures, mainly
 * the max_files variable. This HAS to be called by some task before 
 * the first task that utilizes this exit's, preferrably before any
 * task makes the first use of this library.
 */
STATUS reclaim_init(void)
{
    int *mp;
    SYM_TYPE type;
    struct task_data *tdp;
    int i;

    if (!hook_added) {
	/* race condition */
	++hook_added;
	/* Try to find the maxFiles value */
	if (symFindByNameAndType(sysSymTbl, MAX_FILES_SYM_NAME, (char **)&mp, 
				 &type,
				 N_EXT | N_BSS, N_EXT | N_BSS) == OK ||
	    symFindByNameAndType(sysSymTbl, MAX_FILES_SYM_NAME, (char **)&mp, 
				 &type,
				 N_EXT | N_DATA, N_EXT | N_DATA) == OK) {

#ifdef DEBUG
	    fdprintf(2, "Found maxFiles=%d\n", *mp);
#endif
	    if (*mp <= FD_SETSIZE)
		max_files = *mp;
	    else
		max_files = FD_SETSIZE;
	}
	if (task_data != NULL && task_data->max_files != max_files) {
	    /* fix our own iff we have one */
	    if ((tdp = (struct task_data *)
		 realloc(task_data, sizeof(struct task_data) +
			 max_files*sizeof(FILE *))) != NULL) {
		task_data = tdp;
		for (i = task_data->max_files; i < max_files; i++)
		    task_data->open_fps[i] = NULL;
		task_data->max_files = max_files;
	    }
	}
	/* Make sure taskVariables are deleted AFTER our hook is run. */
	taskVarInit();
	if(taskDeleteHookAdd((FUNCPTR)save_reclaim) != OK) {
	    fprintf(stderr, 
		    "Panic: taskDeleteHook cannot be added for reclaim.\n");
	    return ERROR;
	}
	return OK;
    } else 
	return ERROR;
}

/* N.B.!! We do *not* execute in the context of the dying task here,
   but rather that of tExcTask - we do get a pointer to the task's
   TCB though - this pointer is in fact also the task's ID.     */
static void save_reclaim(WIND_TCB *tcbp)
{
  int i, var, oldfd;
  struct task_data *tdp;
  struct mall_data *mdp, *mdnextp;

  if ((var = taskVarGet((int)tcbp, (int *)&task_data)) != ERROR &&
      var != 0) {
    tdp = (struct task_data *)var;
    if (tdp->version == (FUNCPTR)save_reclaim) { /* Only handle our own */
#ifdef DEBUG
      fdprintf(2, "Reclaiming for task id 0x%x:\nFiles: ", (int)tcbp);
#endif
      /* Ugh! VxWorks doesn't even flush stdout/err - we need to
	 get at those (which are task-private of course, i.e. we
	 can't just do fflush(stdout) here) - we could be really
	 pedantic and try to redefine stdout/err (which "are"
	 function calls) too, snarfing the values when they are
	 used - but besides the overhead this is problematic since
	 they are actually #defines already... We'll peek in the
	 TCB instead (no documentation of course). And of course,
	 we must also meddle with the *file descriptor* indirections,
	 or we'll just flush out on tExcTask's descriptors...   */
      for (i = 1; i <= 2; i++) {
	if (tcbp->taskStdFp[i] != NULL) {
#ifdef DEBUG
	  fdprintf(2, "fflush(%s) ", i == 1 ? "stdout" : "stderr");
#endif
	  oldfd = ioTaskStdGet(0, i);
	  ioTaskStdSet(0, i, tcbp->taskStd[i]);
	  fflush(tcbp->taskStdFp[i]);
	  ioTaskStdSet(0, i, oldfd);
	}
      }
      for (i = 3; i < tdp->max_files; i++) {
	if (FD_ISSET(i, &tdp->open_fds)) {
#ifdef DEBUG
	  fdprintf(2, "close(%d) ", i);
#endif
	  (void) close(i);
	}
	if (tdp->open_fps[i] != NULL) {
#ifdef DEBUG
	  fdprintf(2, "fclose(%0x%x) ", (int)tdp->open_fps[i]);
#endif
	  (void) fclose(tdp->open_fps[i]);
	}
      }
      i = 0;
      mdp = tdp->mall_data;
      while (mdp != NULL) {
	mdnextp = mdp->next;
	if(reclaim_free_function != NULL)
	    (*reclaim_free_function)(mdp->self);
	else
	    free(mdp->self);
	i++;
	mdp = mdnextp;
      }
#ifdef DEBUG
      fdprintf(2, "\nFreeing memory: total %d mallocs\n", i);
#endif
      
      if (tdp->delete_hook != NULL) {
#ifdef DEBUG
	  fdprintf(2, "Calling delete hook at 0x%08x\n", tdp->delete_hook);
#endif
	  (*tdp->delete_hook)(tdp->hook_data);
#ifdef DEBUG
	  fdprintf(2, "Called delete hook at 0x%08x\n", tdp->delete_hook);
#endif
      }
#ifdef DEBUG
      fdprintf(2, "Freeing own mem at 0x%08x\n", tdp);
#endif
      (void) free((char *)tdp);
#ifdef DEBUG
      fdprintf(2, "Freed own mem at 0x%08x, done (0x%08x)\n**********\n", tdp,
	       taskIdSelf());
      checkStack(0);
#endif
    }
  }
#ifdef DEBUG
  else
    fdprintf(2, "No task data found for id 0x%x, var = %d\n", (int)tcbp, var);
#endif
}

/*
 * This sets another free function to be used by the task deletion hook.
 * The free function HAS to be able to free ANY type of dynamically allocated
 * memory that can be in the task data list of allocated memory, that is
 * also memory that's allocated before the free function was set.
 * A "user-supplied" free function is GLOBAL to the system!!!
 * A race condition is present, a task delete hook may be running when this
 * function is called, that may not be especially funny...
 */ 
void set_reclaim_free_function(FreeFunction f){
    reclaim_free_function = f;
}

void save_delete_hook(FUNCPTR func, caddr_t parm)
{
  if (check_hook()) {
    task_data->delete_hook = func;
    task_data->hook_data = parm;
  }
}

/*
 * plain_malloc is only used by spawn_start; plain_free by call_proc;
 * save_fd is used by both.
 */
void *plain_malloc(size_t size){
  return(malloc(size));
}

void *plain_realloc(void *ptr, size_t size){
  return(realloc(ptr, size));
}

void plain_free(void *ptr){
  free(ptr);
}

void save_fd(int fd){
  if (fd >= 0 && check_hook() && fd < task_data->max_files)
    FD_SET(fd, &task_data->open_fds);
}

int save_open(char *path, int flags, /*mode_t mode*/ ...){
  int fd;
  mode_t mode = 0;
  if(flags & O_CREAT){
      va_list pvar;
      va_start(pvar,flags);
#ifdef __GNUC__
#warning save_open() gives three known alignment warnings.
#endif
      mode = va_arg(pvar, mode_t);
      va_end(pvar);
  }
  if ((fd = open(path, flags, mode)) >= 0 && check_hook())
    FD_SET(fd, &task_data->open_fds);
  return(fd);
}

int save_creat(char *path, int mode){
  int fd;
  if ((fd = creat(path, mode)) >= 0 && check_hook())
    FD_SET(fd, &task_data->open_fds);
  return(fd);
}

int save_socket(int domain, int type, int protocol){
  int fd;
  if ((fd = socket(domain, type, protocol)) >= 0 && check_hook())
    FD_SET(fd, &task_data->open_fds);
  return(fd);
}

int save_accept(int s, struct sockaddr *addr, int *addrlen){
  int fd;
  if ((fd = accept(s, addr, addrlen)) >= 0 && check_hook())
    FD_SET(fd, &task_data->open_fds);
  return(fd);
}

int save_close(int fd){
  if (fd >= 0 && fd <= FD_SETSIZE && check_hook())
    FD_CLR(fd, &task_data->open_fds);
  return(close(fd));
}

/* The dealing with FILE *'s below isn't strictly correct, we assume
   that one never has several pointing to the same fd - in the unlikely
   event that one does, all but the last one opened is forgotten */
FILE *save_fopen(const char *filename, char *type){
  FILE *fp;
    
  if ((fp = fopen(filename, type)) != NULL &&
      check_hook() && fileno(fp) < task_data->max_files)
    task_data->open_fps[fileno(fp)] = fp;
  return(fp);
}

FILE *save_fdopen(int fd, char *type){
  FILE *fp;
    
  if ((fp = fdopen(fd, type)) != NULL &&
      check_hook() && fileno(fp) < task_data->max_files) {
    task_data->open_fps[fileno(fp)] = fp;
    FD_CLR(fd, &task_data->open_fds);
  }
  return(fp);
}

FILE *save_freopen(char *filename, char *type, FILE *stream){
  FILE *fp;

  if (check_hook()) {
    if(fileno(stream) < task_data->max_files &&
       task_data->open_fps[fileno(stream)] == stream)
      task_data->open_fps[fileno(stream)] = NULL;
    if ((fp = freopen(filename, type, stream)) != NULL &&
	fileno(fp) < task_data->max_files)
      task_data->open_fps[fileno(fp)] = fp;
  } else
    fp = freopen(filename, type, stream);
  return(fp);
}

int save_fclose(FILE *stream){
  if (check_hook() && fileno(stream) < task_data->max_files &&
      task_data->open_fps[fileno(stream)] == stream)
    task_data->open_fps[fileno(stream)] = NULL;
  return(fclose(stream));
}

/* We link all malloc'ed segments by adding a couple of pointers
   at the *end* - that way we can return the address malloc gave
   (need to make sure we align those pointers) */

/*
  #define MALL_MARGIN   32
  #define save_mall_size(size)  save_mall_size1((size) + 2 * MALL_MARGIN)
  #define save_mall_size1(size) \
  (((size) + sizeof(char *) - 1) & (unsigned long)(-sizeof(char*)))
 
  #define save_mall_enq(ptr, mdp) save_mall_enq1((ptr), (mdp) - MALL_MARGIN)
  #define save_mall_enq1(ptr, mdp) \
  (((struct mall_data *)(mdp))->self = (ptr), \
  ((struct mall_data *)(mdp))->next = task_data->mall_data, \
  task_data->mall_data = (struct mall_data *)(mdp))
*/
#define save_mall_size(size) \
(((size) + sizeof(char *) - 1) & (unsigned long)(-sizeof(char*)))
#define save_mall_enq(ptr, mdp) \
(((struct mall_data *)(mdp))->self = (ptr), \
 ((struct mall_data *)(mdp))->next = task_data->mall_data, \
     task_data->mall_data = (struct mall_data *)(mdp))
 

#define save_mall_deq(ptr) { \
    struct mall_data *mdp = task_data->mall_data, \
    **prevnext = &task_data->mall_data; \
    while (mdp != NULL && mdp->self != (ptr)) { \
	prevnext = &mdp->next; \
	mdp = mdp->next; \
    } \
    if (mdp != NULL) *prevnext = mdp->next; \
}

void *save_malloc2(size_t size, MallocFunction mf){
  unsigned msize = save_mall_size(size);
  char *ptr;

  if ((ptr = (*mf)(msize + sizeof(struct mall_data))) != NULL &&
      check_hook())
    save_mall_enq((void *) ptr, (void *) (ptr + msize));
  return((void *) ptr);
}

void *save_malloc(size_t size){
    return save_malloc2(size, &malloc);
}

void *save_calloc2(size_t nelem, size_t elsize, CallocFunction cf){
  unsigned msize = save_mall_size(nelem * elsize);
  char *ptr;

  if ((ptr = (*cf)(1, msize + sizeof(struct mall_data))) != NULL &&
      check_hook())
    save_mall_enq((void *) ptr, (void *) (ptr + msize));
  return((void *) ptr);
}

void *save_calloc(size_t nelem, size_t elsize){
    return save_calloc2(nelem,elsize,&calloc);
}

void *save_realloc2(void *optr, size_t size, ReallocFunction rf){
  unsigned msize = save_mall_size(size);
  char *ptr;

  /* First we must dequeue the old save block, after that
     we try to realloc, if that succeeds we enqueue the new
     block, if it fails we have to enqueue the old one anew
     so we must deduce the size of that old block first. */

  struct mall_data *mdp0 = task_data->mall_data,
      **prevnext0 = &task_data->mall_data;
  while (mdp0 != NULL && mdp0->self != (((char *) optr))) { 
      prevnext0 = &mdp0->next;
      mdp0 = mdp0->next;
  }
  /* mdp0 == NULL (can) mean that the block that is realloced
   have been malloced with an (for example) ordinary malloc
   (that is not a save_malloc). This is handled like: no dequeing
   is done of that block, the new block is enqueued */ 
  if (mdp0 != NULL)
      save_mall_deq(((char *) optr));

  if ((ptr = (*rf)(optr, msize + sizeof(struct mall_data))) != NULL &&
      check_hook())
      save_mall_enq((void *) ptr, (void *) (ptr + msize));
  else if (mdp0 != NULL)
      /* re-enqueue the old block that has just been dequeued */
      save_mall_enq(((char *) optr), mdp0);
  
  return((void *) ptr);
}

void *save_realloc(void *optr, size_t size){
    return save_realloc2(optr,size,&realloc);
}

void save_free2(void *ptr, FreeFunction ff)
{
  if (check_hook())
    save_mall_deq(((char *) ptr));
  (*ff)(ptr);
}

void save_free(void *ptr){
    save_free2(ptr,&free);
}

void save_cfree2(void *ptr, CfreeFunction cf)
{
  if (check_hook())
    save_mall_deq(((char *)ptr));
  (*cf)(ptr);
}

void save_cfree(void *ptr){
    save_cfree2(ptr,&cfree);
}

