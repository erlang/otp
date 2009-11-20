/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
/*
 * OLD, OBSOLETE include file for erlang driver writers.
 * New drivers should use erl_driver.h instead.
 */

#ifndef __DRIVER_H__
#define __DRIVER_H__

#include <stdlib.h>
#include "driver_int.h"

#undef _ANSI_ARGS_
#undef CONST

#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus) || defined(USE_PROTOTYPE)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#   define CONST const
#else
#   define _ANSI_ARGS_(x)	()
#   define CONST
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

/* Values for mode arg to driver_select() */

#define DO_READ	 (1 << 0)
#define DO_WRITE (1 << 1)

/* Flags for set_port_control_flags() */
#define PORT_CONTROL_FLAG_BINARY	1
#define PORT_CONTROL_FLAG_HEAVY		2

/* This macro is used to name a dynamic driver's init function in */
/* a way that doesn't lead to conflicts. This is crucial when using */
/* operating systems that has one namespace for all symbols */
/* (e.g. VxWorks). Example: if you have an dynamic driver C source */
/* file named echo_drv.c, you use the macro like this: */
/* int DRIVER_INIT(echo_drv)(void *handle) */
#if defined(VXWORKS)
#  define DRIVER_INIT(DRIVER_NAME) DRIVER_NAME  ## _init
#elif defined(__WIN32__)
#  define DRIVER_INIT(DRIVER_NAME) __declspec(dllexport) driver_init
#else 
#  define DRIVER_INIT(DRIVER_NAME)  driver_init
#endif

typedef int (*F_PTR)();    /* a function pointer */
typedef long (*L_PTR)();   /* pointer to a function returning long */

extern int null_func();

/* This structure MUST match Binary in global.h exactly!!! */
typedef struct driver_binary {
    int orig_size;        /* total length of binary */
    char orig_bytes[1];   /* the data (char instead of byte!) */
} DriverBinary;

typedef struct {
    int vsize;     /* length of vectors */
    int size;      /* total size in bytes */
    SysIOVec* iov;
    DriverBinary**  binv;
} ErlIOVec;

/*
 * OLD, OBSOLETE driver entry structure.
 */

typedef struct driver_entry {
    F_PTR init;          /* called at system start up (no args) */
    L_PTR start;         /* called when some one does an open_port
			 args: port, command (nul-terminated),
			 additional/alternate args for fd/vanilla/spawn driver.
			 return value -1 means failure, other
			 is saved and passed to the other funcs */
    F_PTR stop;          /* called when port is closed, and when the
			    emulator is halted - arg: start_return */
    F_PTR output;	 /* called when we have output from erlang to the port
		         args: start_return, buf, buflen */
    F_PTR ready_input;   /* called when we have input from one of the driver's
			 file descriptors - args: start_return, fd */
    F_PTR ready_output;  /* called when output is possible to one of the driver's
			 file descriptors - args: start_return, fd */
    char *driver_name;   /* name supplied as {driver,Name,Args} to open_port */

    F_PTR finish;        /* called before unloading (DYNAMIC DRIVERS ONLY) */
    void *handle;        /* file handle             (DYNAMIC DRIVERS ONLY) */
    F_PTR control;	 /* "ioctl" for drivers (invoked by port_command/3) */
    F_PTR timeout;       /* Reserved */
    F_PTR outputv;       /* Reserved */
    F_PTR ready_async;   /* Completion routine for driver_async */
    F_PTR padding1[3];   /* pad to match size of modern driver struct */
    int padding2[4];     /* more pad */
    F_PTR padding3[3];   /* even more padding */
} DriverEntry;


/* These are the kernel functions available for driver writers */

EXTERN int driver_select _ANSI_ARGS_((int,int,int,int));

EXTERN int driver_output _ANSI_ARGS_((int, char*, int));
EXTERN int driver_output2 _ANSI_ARGS_((int, char*, int, char*, int));
EXTERN int driver_output_binary _ANSI_ARGS_((int, char*, int,
					     DriverBinary*, int, int));
EXTERN int driver_outputv _ANSI_ARGS_((int, char*,int,ErlIOVec*,int));

EXTERN int driver_vec_to_buf _ANSI_ARGS_((ErlIOVec*, char*, int));

EXTERN int driver_set_timer _ANSI_ARGS_((int, unsigned long));
EXTERN int driver_cancel_timer _ANSI_ARGS_((int));

/*
 * The following functions are used to initiate a close of a port
 * from a driver.
 */
EXTERN int driver_failure_eof _ANSI_ARGS_((int));
EXTERN int driver_failure_atom _ANSI_ARGS_((int, char *));
EXTERN int driver_failure_posix _ANSI_ARGS_((int, int));
EXTERN int driver_failure _ANSI_ARGS_((int, int));
EXTERN int driver_exit _ANSI_ARGS_ ((int, int));

EXTERN char* erl_errno_id _ANSI_ARGS_((int error));
EXTERN void set_busy_port _ANSI_ARGS_((int, int));
EXTERN void add_driver_entry _ANSI_ARGS_((DriverEntry *));
EXTERN int remove_driver_entry _ANSI_ARGS_((DriverEntry *));
EXTERN void set_port_control_flags _ANSI_ARGS_((int, int));

/* Binary interface */
/* NOTE: DO NOT overwrite a binary with new data (if the data is delivered);
** since the binary is a shared object it MUST be written once.
*/

EXTERN DriverBinary* driver_alloc_binary _ANSI_ARGS_((int));
EXTERN DriverBinary* driver_realloc_binary _ANSI_ARGS_((DriverBinary*, int));
EXTERN void driver_free_binary _ANSI_ARGS_((DriverBinary*));


/* Queue interface */
EXTERN int driver_enqv _ANSI_ARGS_((int, ErlIOVec*, int));
EXTERN int driver_pushqv _ANSI_ARGS_((int, ErlIOVec*, int));
EXTERN int driver_deq _ANSI_ARGS_((int, int));
EXTERN SysIOVec* driver_peekq _ANSI_ARGS_((int, int*));
EXTERN int driver_sizeq _ANSI_ARGS_((int));
EXTERN int driver_enq_bin _ANSI_ARGS_((int, DriverBinary*, int, int));
EXTERN int driver_enq _ANSI_ARGS_((int, char*, int));
EXTERN int driver_pushq_bin _ANSI_ARGS_((int, DriverBinary*, int, int));
EXTERN int driver_pushq _ANSI_ARGS_((int, char*, int));

/* Memory management */
EXTERN void *driver_alloc _ANSI_ARGS_((size_t));
EXTERN void *driver_realloc _ANSI_ARGS_((void*, size_t));
EXTERN void driver_free _ANSI_ARGS_((void*));

/* Shared / dynamic link libraries */
EXTERN void *driver_dl_open _ANSI_ARGS_((char *));
EXTERN void *driver_dl_sym _ANSI_ARGS_((void *, char *));
EXTERN int driver_dl_close _ANSI_ARGS_((void *));
EXTERN char *driver_dl_error _ANSI_ARGS_((void));

/* Async IO functions */
EXTERN long driver_async _ANSI_ARGS_((int,
				      unsigned int*,
				      void (*)(void*), 
				      void *,
				      void (*)(void*)));
EXTERN int driver_async_cancel _ANSI_ARGS_((long));

EXTERN int driver_lock_driver _ANSI_ARGS_((int));

/* Threads */
typedef void* erl_mutex_t;
typedef void* erl_cond_t;
typedef void* erl_thread_t;

EXTERN erl_mutex_t erts_mutex_create _ANSI_ARGS_((void));
EXTERN int erts_mutex_destroy _ANSI_ARGS_((erl_mutex_t));
EXTERN int erts_mutex_lock _ANSI_ARGS_((erl_mutex_t));
EXTERN int erts_mutex_unlock _ANSI_ARGS_((erl_mutex_t));

EXTERN erl_cond_t erts_cond_create _ANSI_ARGS_((void));
EXTERN int erts_cond_destroy _ANSI_ARGS_((erl_cond_t));
EXTERN int erts_cond_signal _ANSI_ARGS_((erl_cond_t));
EXTERN int erts_cond_broadcast _ANSI_ARGS_((erl_cond_t));
EXTERN int erts_cond_wait _ANSI_ARGS_((erl_cond_t, erl_mutex_t));
EXTERN int erts_cond_timedwait _ANSI_ARGS_((erl_cond_t, erl_mutex_t, long));

EXTERN int erts_thread_create _ANSI_ARGS_((erl_thread_t*,
					 void* (*func)(void*),
					 void* arg,
					 int detached));
EXTERN erl_thread_t erts_thread_self _ANSI_ARGS_((void));
EXTERN void erts_thread_exit _ANSI_ARGS_((void*));
EXTERN int  erts_thread_join _ANSI_ARGS_((erl_thread_t, void**));
EXTERN int  erts_thread_kill _ANSI_ARGS_((erl_thread_t));


typedef unsigned long DriverTermData;

#define TERM_DATA(x) ((DriverTermData) (x))

/* Possible types to send from driver          Argument type */
#define ERL_DRV_NIL         ((DriverTermData) 1)  /* None */
#define ERL_DRV_ATOM        ((DriverTermData) 2)  /* driver_mk_atom(string) */
#define ERL_DRV_INT         ((DriverTermData) 3)  /* int */
#define ERL_DRV_PORT        ((DriverTermData) 4)  /* driver_mk_port(ix) */
#define ERL_DRV_BINARY      ((DriverTermData) 5)  /* ErlDriverBinary*, int */
#define ERL_DRV_STRING      ((DriverTermData) 6)  /* char*, int */
#define ERL_DRV_TUPLE       ((DriverTermData) 7)  /* int */
#define ERL_DRV_LIST        ((DriverTermData) 8)  /* int */
#define ERL_DRV_STRING_CONS ((DriverTermData) 9)  /* char*, int */
#define ERL_DRV_PID         ((DriverTermData) 10) /* driver_connected,... */

/* DriverTermData is the type to use for casts when building 
 * terms that should be sent to connected process,
 * for instance a tuple on the form {tcp, Port, [Tag|Binary]}
 *
 * DriverTermData spec[] = {
 *    ERL_DRV_ATOM, driver_mk_atom("tcp"),
 *    ERL_DRV_PORT, driver_mk_port(drv->ix),
 *             ERL_DRV_INT, REPLY_TAG,
 *             ERL_DRV_BIN, 50, TERM_DATA(buffer),
 *             ERL_DRV_LIST, 2,
 *    ERL_DRV_TUPLE, 3,
 *  }       
 *             
 */

EXTERN DriverTermData driver_mk_atom _ANSI_ARGS_ ((char*));
EXTERN DriverTermData driver_mk_port _ANSI_ARGS_ ((int));
EXTERN DriverTermData driver_connected _ANSI_ARGS_((int));
EXTERN DriverTermData driver_caller _ANSI_ARGS_((int));

EXTERN int driver_output_term _ANSI_ARGS_((int, DriverTermData *, int));
EXTERN int driver_send_term _ANSI_ARGS_((int, DriverTermData, DriverTermData *, int));

#endif


