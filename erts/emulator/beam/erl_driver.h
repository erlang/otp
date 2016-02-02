/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2014. All Rights Reserved.
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
 * Include file for erlang driver writers.
 */

#ifndef __ERL_DRIVER_H__
#define __ERL_DRIVER_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERL_DRV_DEPRECATED_FUNC
#ifdef __GNUC__
#  if __GNUC__ >= 3
#    undef ERL_DRV_DEPRECATED_FUNC
#    define ERL_DRV_DEPRECATED_FUNC __attribute__((deprecated))
#  endif
#endif

/* This is OK to override by the NIF/driver implementor */
#if defined(HALFWORD_HEAP_EMULATOR_SAVED__) && !defined(HALFWORD_HEAP_EMULATOR)
#define HALFWORD_HEAP_EMULATOR HALFWORD_HEAP_EMULATOR_SAVED__
#endif

#include "erl_drv_nif.h"

#include <stdlib.h>
#include <sys/types.h>	/* ssize_t */

#if defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_)
#ifndef STATIC_ERLANG_DRIVER
   /* Windows dynamic drivers, everything is different... */
#define ERL_DRIVER_TYPES_ONLY
#define WIN32_DYNAMIC_ERL_DRIVER
#endif
/*
 * This structure can be cast to a WSABUF structure.
 */
typedef struct _SysIOVec {
    unsigned long iov_len;
    char* iov_base;
} SysIOVec;
#else  /* Unix */
#  ifdef HAVE_SYS_UIO_H
#    include <sys/types.h>
#    include <sys/uio.h>
typedef struct iovec SysIOVec;
#  else
typedef struct {
    char* iov_base;
    size_t iov_len;
} SysIOVec;
#  endif
#endif

#ifndef EXTERN
#  ifdef __cplusplus
#    define EXTERN extern "C"
#  else
#    define EXTERN extern
#  endif
#endif

/* Values for mode arg to driver_select() */
#define ERL_DRV_READ  (1 << 0)
#define ERL_DRV_WRITE (1 << 1)
#define ERL_DRV_USE   (1 << 2)
#define ERL_DRV_USE_NO_CALLBACK (ERL_DRV_USE | (1 << 3))

/* Old deprecated */
#define DO_READ  ERL_DRV_READ
#define DO_WRITE ERL_DRV_WRITE

#define ERL_DRV_EXTENDED_MARKER		(0xfeeeeeed)
#define ERL_DRV_EXTENDED_MAJOR_VERSION	3
#define ERL_DRV_EXTENDED_MINOR_VERSION	3

/*
 * The emulator will refuse to load a driver with a major version
 * lower than ERL_DRV_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD. The load
 * may however fail if user have not removed use of deprecated
 * symbols.
 *
 * The ERL_DRV_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD have to allow
 * loading of drivers built at least two major OTP releases
 * ago.
 *
 * Bump of major version to 3 happened in OTP 17. That is, in
 * OTP 19 we can increase ERL_DRV_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD
 * to 3.
 */
#define ERL_DRV_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD 2

/*
 * The emulator will refuse to load a driver with different major
 * version than the one used by the emulator.
 */


/* Values for set_port_control_flags() */

#define PORT_CONTROL_FLAG_BINARY	(1 << 0)
#define PORT_CONTROL_FLAG_HEAVY		(1 << 1)

/* Values for get_port_flags() */

#define PORT_FLAG_BINARY                (1 << 0)
#define PORT_FLAG_LINE                  (1 << 1)


#define ERL_DRV_FLAG_USE_PORT_LOCKING	(1 << 0)
#define ERL_DRV_FLAG_SOFT_BUSY		(1 << 1)
#define ERL_DRV_FLAG_NO_BUSY_MSGQ	(1 << 2)

/*
 * Integer types
 */

typedef ErlNapiUInt64 ErlDrvUInt64;
typedef ErlNapiSInt64 ErlDrvSInt64;
typedef ErlNapiUInt ErlDrvUInt;
typedef ErlNapiSInt ErlDrvSInt;
typedef ErlNapiUInt ErlDrvTermData;

#if defined(__WIN32__) || defined(_WIN32)
typedef ErlDrvUInt ErlDrvSizeT;
typedef ErlDrvSInt ErlDrvSSizeT;
#else
typedef size_t ErlDrvSizeT;
typedef ssize_t ErlDrvSSizeT;
#endif

/*
 * A binary as seen in a driver. Note that a binary should never be
 * altered by the driver when it has been sent to Erlang.
 */

typedef struct erl_drv_binary {
    ErlDrvSInt orig_size;        /* total length of binary */
    char orig_bytes[1];   /* the data (char instead of byte!) */
} ErlDrvBinary;


/*
 * Note: These types are incomplete to catch type errors easier.
 */

typedef struct _erl_drv_data* ErlDrvData; /* Data to be used by the driver itself. */
#ifndef ERL_SYS_DRV
typedef struct _erl_drv_event* ErlDrvEvent; /* An event to be selected on. */
#endif
typedef struct _erl_drv_port* ErlDrvPort; /* A port descriptor. */
typedef struct _erl_drv_port* ErlDrvThreadData; /* Thread data. */

#if !defined(__WIN32__) && !defined(_WIN32) && !defined(_WIN32_) && !defined(USE_SELECT)
struct erl_drv_event_data {
    short events;
    short revents;
};
#endif
typedef struct erl_drv_event_data *ErlDrvEventData; /* Event data */

/*
 * A driver monitor
 */
typedef struct {
    unsigned char data[sizeof(void *)*4];
} ErlDrvMonitor;

typedef struct {
    unsigned long megasecs;
    unsigned long secs;
    unsigned long microsecs;
} ErlDrvNowData;

typedef ErlDrvSInt64 ErlDrvTime;

#define ERL_DRV_TIME_ERROR ((ErlDrvSInt64) ERTS_NAPI_TIME_ERROR__)

typedef enum {
    ERL_DRV_SEC = ERTS_NAPI_SEC__,
    ERL_DRV_MSEC = ERTS_NAPI_MSEC__,
    ERL_DRV_USEC = ERTS_NAPI_USEC__,
    ERL_DRV_NSEC = ERTS_NAPI_NSEC__
} ErlDrvTimeUnit;

/*
 * Error codes that can be return from driver.
 */

/*
 * Exception code from open_port/2 will be {'EXIT',{einval,Where}}.
 */
#define ERL_DRV_ERROR_GENERAL ((ErlDrvData) -1)

/*
 * Exception code from open_port/2 will be {'EXIT',{Errno,Where}},
 * where Errno is a textual representation of the errno variable
 * (e.g. eacces if errno is EACCES).
 */
#define ERL_DRV_ERROR_ERRNO ((ErlDrvData) -2)

/*
 * Exception code from open_port/2 will be {'EXIT',{badarg,Where}}.
 */
#define ERL_DRV_ERROR_BADARG ((ErlDrvData) -3)

typedef struct erl_io_vec {
    int vsize;			/* length of vectors */
    ErlDrvSizeT size;		/* total size in bytes */
    SysIOVec* iov;
    ErlDrvBinary** binv;
} ErlIOVec;

/*
 * erl driver thread types
 */

typedef struct ErlDrvTid_ *ErlDrvTid;
typedef struct ErlDrvMutex_ ErlDrvMutex;
typedef struct ErlDrvCond_ ErlDrvCond;
typedef struct ErlDrvRWLock_ ErlDrvRWLock;
typedef int ErlDrvTSDKey;

/*
 * 
 */
typedef struct erl_drv_port_data_lock * ErlDrvPDL;

/*
 * This structure defines a driver.
 */

typedef struct erl_drv_entry {
    int (*init)(void);		/* called at system start up for statically
				   linked drivers, and after loading for
				   dynamically loaded drivers */ 

#ifndef ERL_SYS_DRV
    ErlDrvData (*start)(ErlDrvPort port, char *command);
				/* called when open_port/2 is invoked.
				   return value -1 means failure. */
#else
    ErlDrvData (*start)(ErlDrvPort port, char *command, SysDriverOpts* opts);
				/* special options, only for system driver */
#endif
    void (*stop)(ErlDrvData drv_data);
                                /* called when port is closed, and when the
				   emulator is halted. */
    void (*output)(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
				/* called when we have output from erlang to
				   the port */
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
				/* called when we have input from one of 
				   the driver's handles */
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
				/* called when output is possible to one of 
				   the driver's handles */
    char *driver_name;		/* name supplied as command 
				   in open_port XXX ? */
    void (*finish)(void);        /* called before unloading the driver -
				   DYNAMIC DRIVERS ONLY */
    void *handle;		/* Reserved -- Used by emulator internally */
    ErlDrvSSizeT (*control)(ErlDrvData drv_data, unsigned int command,
			    char *buf, ErlDrvSizeT len, char **rbuf,
			    ErlDrvSizeT rlen); /* "ioctl" for drivers - invoked by
						  port_control/3 */
    void (*timeout)(ErlDrvData drv_data);	/* Handling of timeout in driver */
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
				/* called when we have output from erlang
				   to the port */
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thread_data);
    void (*flush)(ErlDrvData drv_data);
                                /* called when the port is about to be 
				   closed, and there is data in the 
				   driver queue that needs to be flushed
				   before 'stop' can be called */
    ErlDrvSSizeT (*call)(ErlDrvData drv_data,
			 unsigned int command, char *buf, ErlDrvSizeT len,
			 char **rbuf, ErlDrvSizeT rlen,
			 unsigned int *flags); /* Works mostly like 'control',
						  a synchronous
						  call into the driver. */
    void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
		  ErlDrvEventData event_data);
                                /* Called when an event selected by 
				   driver_event() has occurred */
    int extended_marker;	/* ERL_DRV_EXTENDED_MARKER */
    int major_version;		/* ERL_DRV_EXTENDED_MAJOR_VERSION */
    int minor_version;		/* ERL_DRV_EXTENDED_MINOR_VERSION */
    int driver_flags;		/* ERL_DRV_FLAGs */
    void *handle2;              /* Reserved -- Used by emulator internally */
    void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
                                /* Called when a process monitor fires */
    void (*stop_select)(ErlDrvEvent event, void* reserved);
    	                        /* Called on behalf of driver_select when
				   it is safe to release 'event'. A typical
				   unix driver would call close(event) */
    void (*emergency_close)(ErlDrvData drv_data);
                                /* called when the port is closed abruptly.
				   specifically when erl_crash_dump is called. */
    /* When adding entries here, dont forget to pad in obsolete/driver.h */
} ErlDrvEntry;

/*
 * This macro is used to name a dynamic driver's init function in
 * a way that doesn't lead to conflicts. This is crucial when using
 * operating systems that has one namespace for all symbols
 * (e.g. VxWorks). Example: if you have an dynamic driver C source
 * file named echo_drv.c, you use the macro like this:
 * 
 *    DRIVER_INIT(echo_drv)
 *    {
 *	 ....
 *    }
 *
 * This function will be called by the Erlang I/O system when the driver is loaded.
 * It must initialize a ErlDrvEntry structure and return a pointer to it.
 */

#ifdef STATIC_ERLANG_DRIVER
#  define ERLANG_DRIVER_NAME(NAME) NAME ## _driver_init
#else
#  define ERLANG_DRIVER_NAME(NAME) driver_init
#endif

/* For windows dynamic drivers */
#ifndef ERL_DRIVER_TYPES_ONLY

#if defined(__WIN32__)
#  define DRIVER_INIT(DRIVER_NAME) \
  __declspec(dllexport) ErlDrvEntry* ERLANG_DRIVER_NAME(DRIVER_NAME)(void);	\
    __declspec(dllexport) ErlDrvEntry* ERLANG_DRIVER_NAME(DRIVER_NAME)(void)
#else 
#  define DRIVER_INIT(DRIVER_NAME) \
    ErlDrvEntry* ERLANG_DRIVER_NAME(DRIVER_NAME)(void); \
    ErlDrvEntry* ERLANG_DRIVER_NAME(DRIVER_NAME)(void)
#endif

#define ERL_DRV_BUSY_MSGQ_DISABLED	(~((ErlDrvSizeT) 0))
#define ERL_DRV_BUSY_MSGQ_READ_ONLY	((ErlDrvSizeT) 0)
#define ERL_DRV_BUSY_MSGQ_LIM_MAX	(ERL_DRV_BUSY_MSGQ_DISABLED - 1)
#define ERL_DRV_BUSY_MSGQ_LIM_MIN	((ErlDrvSizeT) 1)

/*
 * These are the functions available for driver writers.
 */
EXTERN void erl_drv_busy_msgq_limits(ErlDrvPort port,
				     ErlDrvSizeT *low,
				     ErlDrvSizeT *high);

EXTERN int driver_select(ErlDrvPort port, ErlDrvEvent event, int mode, int on);
EXTERN int driver_event(ErlDrvPort port, ErlDrvEvent event, 
			ErlDrvEventData event_data);

EXTERN int driver_output(ErlDrvPort port, char *buf, ErlDrvSizeT len);
EXTERN int driver_output2(ErlDrvPort port, char *hbuf, ErlDrvSizeT hlen,
			  char *buf, ErlDrvSizeT len);
EXTERN int driver_output_binary(ErlDrvPort port, char *hbuf, ErlDrvSizeT hlen,
				ErlDrvBinary* bin,
				ErlDrvSizeT offset, ErlDrvSizeT len);
EXTERN int driver_outputv(ErlDrvPort port, char* hbuf, ErlDrvSizeT hlen,
			  ErlIOVec *ev, ErlDrvSizeT skip);
EXTERN ErlDrvSizeT driver_vec_to_buf(ErlIOVec *ev, char *buf, ErlDrvSizeT len);
EXTERN int driver_set_timer(ErlDrvPort port, unsigned long time);
EXTERN int driver_cancel_timer(ErlDrvPort port);
EXTERN int driver_read_timer(ErlDrvPort port, unsigned long *time_left);

/*
 * Inform runtime system about lengthy work.
 */
EXTERN int erl_drv_consume_timeslice(ErlDrvPort port, int percent);

/*
 * Get plain-text error message from within a driver
 */
EXTERN char* erl_errno_id(int error);

/*
 * The following functions are used to initiate a close of a port
 * from a driver.
 */
EXTERN int driver_failure_eof(ErlDrvPort port);
EXTERN int driver_failure_atom(ErlDrvPort port, char *string);
EXTERN int driver_failure_posix(ErlDrvPort port, int error);
EXTERN int driver_failure(ErlDrvPort port, int error);
EXTERN int driver_exit (ErlDrvPort port, int err);


/*
 * Port Data Lock
 */

EXTERN ErlDrvPDL driver_pdl_create(ErlDrvPort);
EXTERN void driver_pdl_lock(ErlDrvPDL);
EXTERN void driver_pdl_unlock(ErlDrvPDL);
EXTERN ErlDrvSInt driver_pdl_get_refc(ErlDrvPDL);
EXTERN ErlDrvSInt driver_pdl_inc_refc(ErlDrvPDL);
EXTERN ErlDrvSInt driver_pdl_dec_refc(ErlDrvPDL);

/*
 * Process monitors
 */
EXTERN int 
driver_monitor_process(ErlDrvPort port, ErlDrvTermData process, 
		       ErlDrvMonitor *monitor);
EXTERN int 
driver_demonitor_process(ErlDrvPort port, const ErlDrvMonitor *monitor);
EXTERN ErlDrvTermData 
driver_get_monitored_process(ErlDrvPort port, const ErlDrvMonitor *monitor);
EXTERN int driver_compare_monitors(const ErlDrvMonitor *monitor1,
				   const ErlDrvMonitor *monitor2);

/*
 * Port attributes
 */
EXTERN void set_busy_port(ErlDrvPort port, int on);
EXTERN void set_port_control_flags(ErlDrvPort port, int flags);

EXTERN int  get_port_flags(ErlDrvPort port);


/* Binary interface */

/*
 * NOTE: DO NOT overwrite a binary with new data (if the data is delivered);
 * since the binary is a shared object it MUST be written once.
 */

EXTERN ErlDrvBinary* driver_alloc_binary(ErlDrvSizeT size);
EXTERN ErlDrvBinary* driver_realloc_binary(ErlDrvBinary *bin, ErlDrvSizeT size);
EXTERN void driver_free_binary(ErlDrvBinary *bin);

/* Referenc count on driver binaries */
EXTERN ErlDrvSInt driver_binary_get_refc(ErlDrvBinary *dbp);
EXTERN ErlDrvSInt driver_binary_inc_refc(ErlDrvBinary *dbp);
EXTERN ErlDrvSInt driver_binary_dec_refc(ErlDrvBinary *dbp);

/* Allocation interface */
EXTERN void *driver_alloc(ErlDrvSizeT size);
EXTERN void *driver_realloc(void *ptr, ErlDrvSizeT size);
EXTERN void driver_free(void *ptr);

/* Queue interface */
EXTERN int driver_enq(ErlDrvPort port, char* buf, ErlDrvSizeT len);
EXTERN int driver_pushq(ErlDrvPort port, char* buf, ErlDrvSizeT len);
EXTERN ErlDrvSizeT driver_deq(ErlDrvPort port, ErlDrvSizeT size);
EXTERN ErlDrvSizeT driver_sizeq(ErlDrvPort port);
EXTERN int driver_enq_bin(ErlDrvPort port, ErlDrvBinary *bin, ErlDrvSizeT offset,
			  ErlDrvSizeT len);
EXTERN int driver_pushq_bin(ErlDrvPort port, ErlDrvBinary *bin, ErlDrvSizeT offset,
			    ErlDrvSizeT len);

EXTERN ErlDrvSizeT driver_peekqv(ErlDrvPort port, ErlIOVec *ev);
EXTERN SysIOVec* driver_peekq(ErlDrvPort port, int *vlen);
EXTERN int driver_enqv(ErlDrvPort port, ErlIOVec *ev, ErlDrvSizeT skip);
EXTERN int driver_pushqv(ErlDrvPort port, ErlIOVec *ev, ErlDrvSizeT skip);

/*
 * Add and remove driver entries.
 */
EXTERN void add_driver_entry(ErlDrvEntry *de);
EXTERN int remove_driver_entry(ErlDrvEntry *de);

/*
 * System info
 */
EXTERN void driver_system_info(ErlDrvSysInfo *sip, size_t si_size);

/*
 * erl driver thread functions.
 */

EXTERN ErlDrvMutex *erl_drv_mutex_create(char *name);
EXTERN void erl_drv_mutex_destroy(ErlDrvMutex *mtx);
EXTERN int erl_drv_mutex_trylock(ErlDrvMutex *mtx);
EXTERN void erl_drv_mutex_lock(ErlDrvMutex *mtx);
EXTERN void erl_drv_mutex_unlock(ErlDrvMutex *mtx);
EXTERN ErlDrvCond *erl_drv_cond_create(char *name);
EXTERN void erl_drv_cond_destroy(ErlDrvCond *cnd);
EXTERN void erl_drv_cond_signal(ErlDrvCond *cnd);
EXTERN void erl_drv_cond_broadcast(ErlDrvCond *cnd);
EXTERN void erl_drv_cond_wait(ErlDrvCond *cnd, ErlDrvMutex *mtx);
EXTERN ErlDrvRWLock *erl_drv_rwlock_create(char *name);
EXTERN void erl_drv_rwlock_destroy(ErlDrvRWLock *rwlck);
EXTERN int erl_drv_rwlock_tryrlock(ErlDrvRWLock *rwlck);
EXTERN void erl_drv_rwlock_rlock(ErlDrvRWLock *rwlck);
EXTERN void erl_drv_rwlock_runlock(ErlDrvRWLock *rwlck);
EXTERN int erl_drv_rwlock_tryrwlock(ErlDrvRWLock *rwlck);
EXTERN void erl_drv_rwlock_rwlock(ErlDrvRWLock *rwlck);
EXTERN void erl_drv_rwlock_rwunlock(ErlDrvRWLock *rwlck);
EXTERN int erl_drv_tsd_key_create(char *name, ErlDrvTSDKey *key);
EXTERN void erl_drv_tsd_key_destroy(ErlDrvTSDKey key);
EXTERN void erl_drv_tsd_set(ErlDrvTSDKey key, void *data);
EXTERN void *erl_drv_tsd_get(ErlDrvTSDKey key);
EXTERN ErlDrvThreadOpts *erl_drv_thread_opts_create(char *name);
EXTERN void erl_drv_thread_opts_destroy(ErlDrvThreadOpts *opts);
EXTERN int erl_drv_thread_create(char *name,
				 ErlDrvTid *tid,
				 void * (*func)(void *),
				 void *args,
				 ErlDrvThreadOpts *opts);
EXTERN ErlDrvTid erl_drv_thread_self(void);
EXTERN int erl_drv_equal_tids(ErlDrvTid tid1, ErlDrvTid tid2);
EXTERN void erl_drv_thread_exit(void *resp);
EXTERN int erl_drv_thread_join(ErlDrvTid, void **respp);

EXTERN char* erl_drv_mutex_name(ErlDrvMutex *mtx);
EXTERN char* erl_drv_cond_name(ErlDrvCond *cnd);
EXTERN char* erl_drv_rwlock_name(ErlDrvRWLock *rwlck);
EXTERN char* erl_drv_thread_name(ErlDrvTid tid);

/*
 * Misc.
 */
EXTERN int null_func(void);

#endif /* !ERL_DRIVER_TYPES_ONLY */

/* Constants for return flags from the 'port_call' callback */
#define DRIVER_CALL_KEEP_BUFFER 0x1

/* ErlDrvTerm is the type to use for casts when building 
 * terms that should be sent to connected process,
 * for instance a tuple on the form {tcp, Port, [Tag|Binary]}
 *
 * ErlDrvTerm spec[] = {
 *    ERL_DRV_ATOM, driver_mk_atom("tcp"),
 *    ERL_DRV_PORT, driver_mk_port(drv->ix),
 *             ERL_DRV_INT, REPLY_TAG,
 *             ERL_DRV_BINARY, (ErlDrvTerm)bin, 50, 0,
 *             ERL_DRV_LIST, 2,
 *    ERL_DRV_TUPLE, 3,
 *  }       
 *             
 */

#define TERM_DATA(x) ((ErlDrvTermData) (x))

/* Possible types to send from driver          Argument type */
#define ERL_DRV_NIL         ((ErlDrvTermData) 1)  /* None */
#define ERL_DRV_ATOM        ((ErlDrvTermData) 2)  /* driver_mk_atom(string) */
#define ERL_DRV_INT         ((ErlDrvTermData) 3)  /* ErlDrvSInt */
#define ERL_DRV_PORT        ((ErlDrvTermData) 4)  /* driver_mk_port(ix) */
#define ERL_DRV_BINARY      ((ErlDrvTermData) 5)  /* ErlDrvBinary*, 
						   * ErlDrvUInt size,
						   * ErlDrvUInt offs */
#define ERL_DRV_STRING      ((ErlDrvTermData) 6)  /* char*, ErlDrvUInt */
#define ERL_DRV_TUPLE       ((ErlDrvTermData) 7)  /* ErlDrvUInt */
#define ERL_DRV_LIST        ((ErlDrvTermData) 8)  /* ErlDrvUInt */
#define ERL_DRV_STRING_CONS ((ErlDrvTermData) 9)  /* char*, ErlDrvUInt */
#define ERL_DRV_PID         ((ErlDrvTermData) 10) /* driver_connected,... */

#define ERL_DRV_FLOAT       ((ErlDrvTermData) 11) /* double * */
#define ERL_DRV_EXT2TERM    ((ErlDrvTermData) 12) /* char *, ErlDrvUInt */
#define ERL_DRV_UINT        ((ErlDrvTermData) 13) /* ErlDrvUInt */
#define ERL_DRV_BUF2BINARY  ((ErlDrvTermData) 14) /* char *, ErlDrvUInt */
#define ERL_DRV_INT64       ((ErlDrvTermData) 15) /* ErlDrvSInt64 * */
#define ERL_DRV_UINT64      ((ErlDrvTermData) 16) /* ErlDrvUInt64 * */

#define ERL_DRV_MAP         ((ErlDrvTermData) 17) /* ErlDrvUInt */

#ifndef ERL_DRIVER_TYPES_ONLY

/* make terms for driver_output_term and driver_send_term */
EXTERN ErlDrvTermData driver_mk_atom(char*);
EXTERN ErlDrvTermData driver_mk_port(ErlDrvPort);
EXTERN ErlDrvTermData driver_connected(ErlDrvPort);
EXTERN ErlDrvTermData driver_caller(ErlDrvPort);
extern const ErlDrvTermData driver_term_nil;
EXTERN ErlDrvTermData driver_mk_term_nil(void);
EXTERN ErlDrvPort driver_create_port(ErlDrvPort creator_port, 
				     ErlDrvTermData connected, /* pid */
				     char* name, /* driver name */
				     ErlDrvData drv_data);
					 

/*
 * driver_output_term() is deprecated, and scheduled for removal in
 * OTP-R17. Use erl_drv_output_term() instead. For more information
 * see the erl_driver(3) documentation.
 */
EXTERN int driver_output_term(ErlDrvPort ix,
			      ErlDrvTermData* data,
			      int len) ERL_DRV_DEPRECATED_FUNC;
/*
 * driver_send_term() is deprecated, and scheduled for removal in
 * OTP-R17. Use erl_drv_send_term() instead. For more information
 * see the erl_driver(3) documentation.
 */
EXTERN int driver_send_term(ErlDrvPort ix,
			    ErlDrvTermData to,
			    ErlDrvTermData* data,
			    int len) ERL_DRV_DEPRECATED_FUNC;

/* output term data to the port owner */
EXTERN int erl_drv_output_term(ErlDrvTermData port,
			       ErlDrvTermData* data,
			       int len);
/* output term data to a specific process */
EXTERN int erl_drv_send_term(ErlDrvTermData port,
			     ErlDrvTermData to,
			     ErlDrvTermData* data,
			     int len);

/* Async IO functions */
EXTERN unsigned int driver_async_port_key(ErlDrvPort port);

EXTERN long driver_async(ErlDrvPort ix,
			 unsigned int* key,
			 void (*async_invoke)(void*), 
			 void* async_data,
			 void (*async_free)(void*));

/* Locks the driver in the machine "forever", there is
   no unlock function. Note that this is almost never useful, as an open
   port towards the driver locks it until the port is closed, why unexpected
   unloading "never" happens. */
EXTERN int driver_lock_driver(ErlDrvPort ix);

/* Get the current 'now' timestamp (analogue to erlang:now()) */
EXTERN int driver_get_now(ErlDrvNowData *now) ERL_DRV_DEPRECATED_FUNC;

/* Erlang Monotonic Time */
EXTERN ErlDrvTime erl_drv_monotonic_time(ErlDrvTimeUnit time_unit);
/* Time offset between Erlang Monotonic Time and Erlang System Time */
EXTERN ErlDrvTime erl_drv_time_offset(ErlDrvTimeUnit time_unit);
/* Time unit conversion */
EXTERN ErlDrvTime erl_drv_convert_time_unit(ErlDrvTime val,
					    ErlDrvTimeUnit from,
					    ErlDrvTimeUnit to);

/* These were removed from the ANSI version, now they're back. */

EXTERN void *driver_dl_open(char *);
EXTERN void *driver_dl_sym(void *, char *);
EXTERN int driver_dl_close(void *);
EXTERN char *driver_dl_error(void);

/* environment */
EXTERN int erl_drv_putenv(const char *key, char *value);
EXTERN int erl_drv_getenv(const char *key, char *value, size_t *value_size);

#ifdef __OSE__
typedef ErlDrvUInt ErlDrvOseEventId;
EXTERN union SIGNAL *erl_drv_ose_get_signal(ErlDrvEvent ev);
EXTERN ErlDrvEvent erl_drv_ose_event_alloc(SIGSELECT sig, ErlDrvOseEventId handle,
					   ErlDrvOseEventId (*resolve_signal)(union SIGNAL *sig), void *extra);
EXTERN void erl_drv_ose_event_free(ErlDrvEvent ev);
EXTERN void erl_drv_ose_event_fetch(ErlDrvEvent ev, SIGSELECT *sig,
                  ErlDrvOseEventId *handle, void **extra);
#endif

#endif /* !ERL_DRIVER_TYPES_ONLY */

#ifdef WIN32_DYNAMIC_ERL_DRIVER
#  include "erl_win_dyn_driver.h"
#endif

#endif

/* also in global.h, but driver's can't include global.h */
void dtrace_drvport_str(ErlDrvPort port, char *port_buf);
