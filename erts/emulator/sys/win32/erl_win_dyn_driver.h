/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
 * Include file for erlang driver writers using dynamic drivers on windows.
 */

/* Maybe this should be auto generated, but I'll leave that for now... */

#ifndef _ERL_WIN_DYN_DRIVER_H 
#define _ERL_WIN_DYN_DRIVER_H 

#define WDD_FTYPE(FunctionName) TWinDynDriver##FunctionName

#define WDD_TYPEDEF(RetType, FunctionName, Params) \
  typedef RetType WDD_FTYPE(FunctionName) Params 

WDD_TYPEDEF(int, null_func,(void));
WDD_TYPEDEF(int, driver_failure_atom,(ErlDrvPort, char *));
WDD_TYPEDEF(int, driver_failure_posix,(ErlDrvPort, int));
WDD_TYPEDEF(int, driver_failure,(ErlDrvPort, int));
WDD_TYPEDEF(int, driver_exit, (ErlDrvPort, int));
WDD_TYPEDEF(int, driver_failure_eof, (ErlDrvPort));
WDD_TYPEDEF(void, erl_drv_busy_msgq_limits, (ErlDrvPort, ErlDrvSizeT *, ErlDrvSizeT *));
WDD_TYPEDEF(int, driver_select, (ErlDrvPort, ErlDrvEvent, int, int));
WDD_TYPEDEF(int, driver_event, (ErlDrvPort, ErlDrvEvent,ErlDrvEventData));
WDD_TYPEDEF(int, driver_output, (ErlDrvPort, char *, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_output2, (ErlDrvPort, char *, ErlDrvSizeT ,char *, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_output_binary, (ErlDrvPort, char *, ErlDrvSizeT, ErlDrvBinary*, ErlDrvSizeT, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_outputv, (ErlDrvPort, char*, ErlDrvSizeT, ErlIOVec *, ErlDrvSizeT));
WDD_TYPEDEF(ErlDrvSizeT, driver_vec_to_buf, (ErlIOVec *, char *, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_set_timer, (ErlDrvPort, unsigned long));
WDD_TYPEDEF(int, driver_cancel_timer, (ErlDrvPort));
WDD_TYPEDEF(int, driver_read_timer, (ErlDrvPort, unsigned long *));
WDD_TYPEDEF(int, erl_drv_consume_timeslice, (ErlDrvPort, int));
WDD_TYPEDEF(char *, erl_errno_id, (int));
WDD_TYPEDEF(void, set_busy_port, (ErlDrvPort, int));
WDD_TYPEDEF(void, set_port_control_flags, (ErlDrvPort, int));
WDD_TYPEDEF(int, get_port_flags, (ErlDrvPort));
WDD_TYPEDEF(ErlDrvBinary *, driver_alloc_binary, (ErlDrvSizeT));
WDD_TYPEDEF(ErlDrvBinary *, driver_realloc_binary, (ErlDrvBinary *, ErlDrvSizeT));
WDD_TYPEDEF(void, driver_free_binary, (ErlDrvBinary *));
WDD_TYPEDEF(void *, driver_alloc, (ErlDrvSizeT));
WDD_TYPEDEF(void *, driver_realloc, (void *, ErlDrvSizeT));
WDD_TYPEDEF(void, driver_free, (void *));
WDD_TYPEDEF(int, driver_enq, (ErlDrvPort, char*, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_pushq, (ErlDrvPort, char*, ErlDrvSizeT));
WDD_TYPEDEF(ErlDrvSizeT, driver_deq, (ErlDrvPort, ErlDrvSizeT));
WDD_TYPEDEF(ErlDrvSizeT, driver_sizeq, (ErlDrvPort));
WDD_TYPEDEF(int, driver_enq_bin, (ErlDrvPort, ErlDrvBinary *, ErlDrvSizeT, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_pushq_bin, (ErlDrvPort, ErlDrvBinary *, ErlDrvSizeT, ErlDrvSizeT));
WDD_TYPEDEF(ErlDrvSizeT, driver_peekqv, (ErlDrvPort, ErlIOVec *));
WDD_TYPEDEF(SysIOVec *, driver_peekq, (ErlDrvPort, int *));
WDD_TYPEDEF(int, driver_enqv, (ErlDrvPort, ErlIOVec *, ErlDrvSizeT));
WDD_TYPEDEF(int, driver_pushqv, (ErlDrvPort, ErlIOVec *, ErlDrvSizeT));
WDD_TYPEDEF(void, add_driver_entry, (ErlDrvEntry *));
WDD_TYPEDEF(int, remove_driver_entry, (ErlDrvEntry *));
WDD_TYPEDEF(ErlDrvTermData, driver_mk_atom, (char*));
WDD_TYPEDEF(ErlDrvTermData, driver_mk_port,(ErlDrvPort));
WDD_TYPEDEF(ErlDrvTermData, driver_connected,(ErlDrvPort));
WDD_TYPEDEF(ErlDrvTermData, driver_caller,(ErlDrvPort));
WDD_TYPEDEF(ErlDrvTermData, driver_mk_term_nil,(void));
WDD_TYPEDEF(int, erl_drv_output_term, (ErlDrvTermData, ErlDrvTermData*, int));
WDD_TYPEDEF(int, driver_output_term, (ErlDrvPort, ErlDrvTermData*, int));
WDD_TYPEDEF(int, erl_drv_send_term, (ErlDrvTermData, ErlDrvTermData, ErlDrvTermData*, int));
WDD_TYPEDEF(int, driver_send_term, (ErlDrvPort, ErlDrvTermData, ErlDrvTermData*, int));
WDD_TYPEDEF(unsigned int, driver_async_port_key, (ErlDrvPort));
WDD_TYPEDEF(long, driver_async, (ErlDrvPort,unsigned int*,void (*)(void*),void*,void (*)(void*)));
WDD_TYPEDEF(int, driver_lock_driver, (ErlDrvPort));
WDD_TYPEDEF(void *, driver_dl_open, (char *));
WDD_TYPEDEF(void *, driver_dl_sym, (void *, char *));
WDD_TYPEDEF(int, driver_dl_close, (void *));
WDD_TYPEDEF(char *, driver_dl_error, (void));
WDD_TYPEDEF(ErlDrvUInt, erts_alc_test, (ErlDrvUInt,
					ErlDrvUInt,
					ErlDrvUInt,
					ErlDrvUInt));
WDD_TYPEDEF(ErlDrvSInt, driver_binary_get_refc, (ErlDrvBinary *dbp));
WDD_TYPEDEF(ErlDrvSInt, driver_binary_inc_refc, (ErlDrvBinary *dbp));
WDD_TYPEDEF(ErlDrvSInt, driver_binary_dec_refc, (ErlDrvBinary *dbp));
WDD_TYPEDEF(ErlDrvPDL, driver_pdl_create, (ErlDrvPort));
WDD_TYPEDEF(void, driver_pdl_lock, (ErlDrvPDL));
WDD_TYPEDEF(void, driver_pdl_unlock, (ErlDrvPDL));
WDD_TYPEDEF(ErlDrvSInt, driver_pdl_get_refc, (ErlDrvPDL));
WDD_TYPEDEF(ErlDrvSInt, driver_pdl_inc_refc, (ErlDrvPDL));
WDD_TYPEDEF(ErlDrvSInt, driver_pdl_dec_refc, (ErlDrvPDL));
WDD_TYPEDEF(void, driver_system_info, (ErlDrvSysInfo *, size_t));
WDD_TYPEDEF(int, driver_get_now, (ErlDrvNowData *));
WDD_TYPEDEF(ErlDrvTime, erl_drv_monotonic_time, (ErlDrvTimeUnit));
WDD_TYPEDEF(ErlDrvTime, erl_drv_time_offset, (ErlDrvTimeUnit));
WDD_TYPEDEF(ErlDrvTime, erl_drv_convert_time_unit, (ErlDrvTime,
						    ErlDrvTimeUnit,
						    ErlDrvTimeUnit));
WDD_TYPEDEF(int, driver_monitor_process, (ErlDrvPort port, 
					  ErlDrvTermData process, 
					  ErlDrvMonitor *monitor));
WDD_TYPEDEF(int, driver_demonitor_process, (ErlDrvPort port,
					    const ErlDrvMonitor *monitor));
WDD_TYPEDEF(ErlDrvTermData,  driver_get_monitored_process, 
	    (ErlDrvPort port, const ErlDrvMonitor *monitor));
WDD_TYPEDEF(int,  driver_compare_monitors, 
	    (const ErlDrvMonitor *, const ErlDrvMonitor *));
WDD_TYPEDEF(ErlDrvMutex *, erl_drv_mutex_create, (char *name));
WDD_TYPEDEF(void, erl_drv_mutex_destroy, (ErlDrvMutex *mtx));
WDD_TYPEDEF(int, erl_drv_mutex_trylock, (ErlDrvMutex *mtx));
WDD_TYPEDEF(void, erl_drv_mutex_lock, (ErlDrvMutex *mtx));
WDD_TYPEDEF(void, erl_drv_mutex_unlock, (ErlDrvMutex *mtx));
WDD_TYPEDEF(ErlDrvCond *, erl_drv_cond_create, (char *name));
WDD_TYPEDEF(void, erl_drv_cond_destroy, (ErlDrvCond *cnd));
WDD_TYPEDEF(void, erl_drv_cond_signal, (ErlDrvCond *cnd));
WDD_TYPEDEF(void, erl_drv_cond_broadcast, (ErlDrvCond *cnd));
WDD_TYPEDEF(void, erl_drv_cond_wait, (ErlDrvCond *cnd, ErlDrvMutex *mtx));
WDD_TYPEDEF(ErlDrvRWLock *, erl_drv_rwlock_create, (char *name));
WDD_TYPEDEF(void, erl_drv_rwlock_destroy, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(int, erl_drv_rwlock_tryrlock, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(void, erl_drv_rwlock_rlock, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(void, erl_drv_rwlock_runlock, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(int, erl_drv_rwlock_tryrwlock, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(void, erl_drv_rwlock_rwlock, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(void, erl_drv_rwlock_rwunlock, (ErlDrvRWLock *rwlck));
WDD_TYPEDEF(int, erl_drv_tsd_key_create, (char *name, ErlDrvTSDKey *key));
WDD_TYPEDEF(void, erl_drv_tsd_key_destroy, (ErlDrvTSDKey key));
WDD_TYPEDEF(void, erl_drv_tsd_set, (ErlDrvTSDKey key, void *data));
WDD_TYPEDEF(void *, erl_drv_tsd_get, (ErlDrvTSDKey key));
WDD_TYPEDEF(ErlDrvThreadOpts *, erl_drv_thread_opts_create, (char *name));
WDD_TYPEDEF(void, erl_drv_thread_opts_destroy, (ErlDrvThreadOpts *opts));
WDD_TYPEDEF(int, erl_drv_thread_create, (char *name,
					 ErlDrvTid *tid,
					 void * (*func)(void *),
					 void *args,
					 ErlDrvThreadOpts *opts));
WDD_TYPEDEF(ErlDrvTid, erl_drv_thread_self, (void));
WDD_TYPEDEF(int, erl_drv_equal_tids, (ErlDrvTid tid1, ErlDrvTid tid2));
WDD_TYPEDEF(void, erl_drv_thread_exit, (void *resp));
WDD_TYPEDEF(int, erl_drv_thread_join, (ErlDrvTid, void **respp));
WDD_TYPEDEF(int, erl_drv_putenv, (const char *key, char *value));
WDD_TYPEDEF(int, erl_drv_getenv, (const char *key, char *value, size_t *value_size));

typedef struct {
    WDD_FTYPE(null_func) *null_func;
    WDD_FTYPE(driver_failure_atom) *driver_failure_atom;
    WDD_FTYPE(driver_failure_posix) *driver_failure_posix;
    WDD_FTYPE(driver_failure) *driver_failure;
    WDD_FTYPE(driver_exit) *driver_exit;
    WDD_FTYPE(driver_failure_eof) *driver_failure_eof;
    WDD_FTYPE(erl_drv_busy_msgq_limits) *erl_drv_busy_msgq_limits;
    WDD_FTYPE(driver_select) *driver_select;
    WDD_FTYPE(driver_event) *driver_event;
    WDD_FTYPE(driver_output) *driver_output;
    WDD_FTYPE(driver_output2) *driver_output2;
    WDD_FTYPE(driver_output_binary) *driver_output_binary;
    WDD_FTYPE(driver_outputv) *driver_outputv;
    WDD_FTYPE(driver_vec_to_buf) *driver_vec_to_buf;
    WDD_FTYPE(driver_set_timer) *driver_set_timer;
    WDD_FTYPE(driver_cancel_timer) *driver_cancel_timer;
    WDD_FTYPE(driver_read_timer) *driver_read_timer;
    WDD_FTYPE(erl_drv_consume_timeslice) *erl_drv_consume_timeslice;
    WDD_FTYPE(erl_errno_id) *erl_errno_id;
    WDD_FTYPE(set_busy_port)* set_busy_port;
    WDD_FTYPE(set_port_control_flags) *set_port_control_flags;
    WDD_FTYPE(get_port_flags) *get_port_flags;
    WDD_FTYPE(driver_alloc_binary) *driver_alloc_binary;
    WDD_FTYPE(driver_realloc_binary) *driver_realloc_binary;
    WDD_FTYPE(driver_free_binary) *driver_free_binary;
    WDD_FTYPE(driver_alloc) *driver_alloc;
    WDD_FTYPE(driver_realloc) *driver_realloc;
    WDD_FTYPE(driver_free) *driver_free;
    WDD_FTYPE(driver_enq) *driver_enq;
    WDD_FTYPE(driver_pushq) *driver_pushq;
    WDD_FTYPE(driver_deq) *driver_deq;
    WDD_FTYPE(driver_sizeq) *driver_sizeq;
    WDD_FTYPE(driver_enq_bin)* driver_enq_bin;
    WDD_FTYPE(driver_pushq_bin) *driver_pushq_bin;
    WDD_FTYPE(driver_peekqv) *driver_peekqv;
    WDD_FTYPE(driver_peekq) *driver_peekq;
    WDD_FTYPE(driver_enqv) *driver_enqv;
    WDD_FTYPE(driver_pushqv) *driver_pushqv;
    WDD_FTYPE(add_driver_entry) *add_driver_entry;
    WDD_FTYPE(remove_driver_entry) *remove_driver_entry;
    WDD_FTYPE(driver_mk_atom) *driver_mk_atom;
    WDD_FTYPE(driver_mk_port) *driver_mk_port;
    WDD_FTYPE(driver_connected) *driver_connected;
    WDD_FTYPE(driver_caller) *driver_caller;
    WDD_FTYPE(driver_mk_term_nil) *driver_mk_term_nil;
    WDD_FTYPE(erl_drv_output_term) *erl_drv_output_term;
    WDD_FTYPE(driver_output_term) *driver_output_term;
    WDD_FTYPE(erl_drv_send_term) *erl_drv_send_term;
    WDD_FTYPE(driver_send_term) *driver_send_term;
    WDD_FTYPE(driver_async_port_key) *driver_async_port_key;
    WDD_FTYPE(driver_async) *driver_async;
    WDD_FTYPE(driver_lock_driver) *driver_lock_driver;
    WDD_FTYPE(driver_dl_open) *driver_dl_open;
    WDD_FTYPE(driver_dl_sym) *driver_dl_sym;
    WDD_FTYPE(driver_dl_close) *driver_dl_close;
    WDD_FTYPE(driver_dl_error) *driver_dl_error;
    WDD_FTYPE(erts_alc_test) *erts_alc_test;
    WDD_FTYPE(driver_binary_get_refc) *driver_binary_get_refc;
    WDD_FTYPE(driver_binary_inc_refc) *driver_binary_inc_refc;
    WDD_FTYPE(driver_binary_dec_refc) *driver_binary_dec_refc;
    WDD_FTYPE(driver_pdl_create) *driver_pdl_create;
    WDD_FTYPE(driver_pdl_lock) *driver_pdl_lock;
    WDD_FTYPE(driver_pdl_unlock) *driver_pdl_unlock;
    WDD_FTYPE(driver_pdl_get_refc) *driver_pdl_get_refc;
    WDD_FTYPE(driver_pdl_inc_refc) *driver_pdl_inc_refc;
    WDD_FTYPE(driver_pdl_dec_refc) *driver_pdl_dec_refc;
    WDD_FTYPE(driver_system_info) *driver_system_info;
    WDD_FTYPE(driver_get_now) *driver_get_now;
    WDD_FTYPE(erl_drv_monotonic_time) *erl_drv_monotonic_time;
    WDD_FTYPE(erl_drv_time_offset) *erl_drv_time_offset;
    WDD_FTYPE(erl_drv_convert_time_unit) *erl_drv_convert_time_unit;
    WDD_FTYPE(driver_monitor_process) *driver_monitor_process;
    WDD_FTYPE(driver_demonitor_process) *driver_demonitor_process;
    WDD_FTYPE(driver_get_monitored_process) *driver_get_monitored_process;
    WDD_FTYPE(driver_compare_monitors) *driver_compare_monitors;
    WDD_FTYPE(erl_drv_mutex_create) *erl_drv_mutex_create;
    WDD_FTYPE(erl_drv_mutex_destroy) *erl_drv_mutex_destroy;
    WDD_FTYPE(erl_drv_mutex_trylock) *erl_drv_mutex_trylock;
    WDD_FTYPE(erl_drv_mutex_lock) *erl_drv_mutex_lock;
    WDD_FTYPE(erl_drv_mutex_unlock) *erl_drv_mutex_unlock;
    WDD_FTYPE(erl_drv_cond_create) *erl_drv_cond_create;
    WDD_FTYPE(erl_drv_cond_destroy) *erl_drv_cond_destroy;
    WDD_FTYPE(erl_drv_cond_signal) *erl_drv_cond_signal;
    WDD_FTYPE(erl_drv_cond_broadcast) *erl_drv_cond_broadcast;
    WDD_FTYPE(erl_drv_cond_wait) *erl_drv_cond_wait;
    WDD_FTYPE(erl_drv_rwlock_create) *erl_drv_rwlock_create;
    WDD_FTYPE(erl_drv_rwlock_destroy) *erl_drv_rwlock_destroy;
    WDD_FTYPE(erl_drv_rwlock_tryrlock) *erl_drv_rwlock_tryrlock;
    WDD_FTYPE(erl_drv_rwlock_rlock) *erl_drv_rwlock_rlock;
    WDD_FTYPE(erl_drv_rwlock_runlock) *erl_drv_rwlock_runlock;
    WDD_FTYPE(erl_drv_rwlock_tryrwlock) *erl_drv_rwlock_tryrwlock;
    WDD_FTYPE(erl_drv_rwlock_rwlock) *erl_drv_rwlock_rwlock;
    WDD_FTYPE(erl_drv_rwlock_rwunlock) *erl_drv_rwlock_rwunlock;
    WDD_FTYPE(erl_drv_tsd_key_create) *erl_drv_tsd_key_create;
    WDD_FTYPE(erl_drv_tsd_key_destroy) *erl_drv_tsd_key_destroy;
    WDD_FTYPE(erl_drv_tsd_set) *erl_drv_tsd_set;
    WDD_FTYPE(erl_drv_tsd_get) *erl_drv_tsd_get;
    WDD_FTYPE(erl_drv_thread_opts_create) *erl_drv_thread_opts_create;
    WDD_FTYPE(erl_drv_thread_opts_destroy) *erl_drv_thread_opts_destroy;
    WDD_FTYPE(erl_drv_thread_create) *erl_drv_thread_create;
    WDD_FTYPE(erl_drv_thread_self) *erl_drv_thread_self;
    WDD_FTYPE(erl_drv_equal_tids) *erl_drv_equal_tids;
    WDD_FTYPE(erl_drv_thread_exit) *erl_drv_thread_exit;
    WDD_FTYPE(erl_drv_thread_join) *erl_drv_thread_join;
    WDD_FTYPE(erl_drv_putenv) *erl_drv_putenv;
    WDD_FTYPE(erl_drv_getenv) *erl_drv_getenv;
  /* Add new calls here */
} TWinDynDriverCallbacks;   

/* This header is included explicitly by the ddll static driver, it musn't define things then */ 
#ifndef STATIC_ERLANG_DRIVER

extern TWinDynDriverCallbacks WinDynDriverCallbacks;

#define null_func (WinDynDriverCallbacks.null_func)
#define driver_failure_atom (WinDynDriverCallbacks.driver_failure_atom)
#define driver_failure_posix (WinDynDriverCallbacks.driver_failure_posix)
#define driver_failure (WinDynDriverCallbacks.driver_failure)
#define driver_exit (WinDynDriverCallbacks.driver_exit)
#define driver_failure_eof (WinDynDriverCallbacks.driver_failure_eof)
#define erl_drv_busy_msgq_limits (WinDynDriverCallbacks.erl_drv_busy_msgq_limits)
#define driver_select (WinDynDriverCallbacks.driver_select)
#define driver_event (WinDynDriverCallbacks.driver_event)
#define driver_output (WinDynDriverCallbacks.driver_output)
#define driver_output2 (WinDynDriverCallbacks.driver_output2)
#define driver_output_binary (WinDynDriverCallbacks.driver_output_binary)
#define driver_outputv (WinDynDriverCallbacks.driver_outputv)
#define driver_vec_to_buf (WinDynDriverCallbacks.driver_vec_to_buf)
#define driver_set_timer (WinDynDriverCallbacks.driver_set_timer)
#define driver_cancel_timer (WinDynDriverCallbacks.driver_cancel_timer)
#define driver_read_timer (WinDynDriverCallbacks.driver_read_timer)
#define erl_drv_consume_timeslice (WinDynDriverCallbacks.erl_drv_consume_timeslice)
#define erl_errno_id (WinDynDriverCallbacks.erl_errno_id)
#define set_busy_port (WinDynDriverCallbacks.set_busy_port)
#define set_port_control_flags (WinDynDriverCallbacks.set_port_control_flags)
#define get_port_flags (WinDynDriverCallbacks.get_port_flags)
#define driver_alloc_binary (WinDynDriverCallbacks.driver_alloc_binary)
#define driver_realloc_binary (WinDynDriverCallbacks.driver_realloc_binary)
#define driver_free_binary (WinDynDriverCallbacks.driver_free_binary)
#define driver_alloc (WinDynDriverCallbacks.driver_alloc)
#define driver_realloc (WinDynDriverCallbacks.driver_realloc)
#define driver_free (WinDynDriverCallbacks.driver_free)
#define driver_enq (WinDynDriverCallbacks.driver_enq)
#define driver_pushq (WinDynDriverCallbacks.driver_pushq)
#define driver_deq (WinDynDriverCallbacks.driver_deq)
#define driver_sizeq (WinDynDriverCallbacks.driver_sizeq)
#define driver_enq_bin (WinDynDriverCallbacks.driver_enq_bin)
#define driver_pushq_bin (WinDynDriverCallbacks.driver_pushq_bin)
#define driver_peekqv (WinDynDriverCallbacks.driver_peekqv)
#define driver_peekq (WinDynDriverCallbacks.driver_peekq)
#define driver_enqv (WinDynDriverCallbacks.driver_enqv)
#define driver_pushqv (WinDynDriverCallbacks.driver_pushqv)
#define add_driver_entry (WinDynDriverCallbacks.add_driver_entry)
#define remove_driver_entry (WinDynDriverCallbacks.remove_driver_entry)
#define driver_mk_atom (WinDynDriverCallbacks.driver_mk_atom)
#define driver_mk_port (WinDynDriverCallbacks.driver_mk_port)
#define driver_connected (WinDynDriverCallbacks.driver_connected)
#define driver_caller (WinDynDriverCallbacks.driver_caller)
#define driver_mk_term_nil (WinDynDriverCallbacks.driver_mk_term_nil)
#define erl_drv_output_term (WinDynDriverCallbacks.erl_drv_output_term)
#define driver_output_term (WinDynDriverCallbacks.driver_output_term)
#define erl_drv_send_term (WinDynDriverCallbacks.erl_drv_send_term)
#define driver_send_term (WinDynDriverCallbacks.driver_send_term)
#define driver_async_port_key (WinDynDriverCallbacks.driver_async_port_key)
#define driver_async (WinDynDriverCallbacks.driver_async)
#define driver_lock_driver (WinDynDriverCallbacks.driver_lock_driver)
#define driver_dl_open (WinDynDriverCallbacks.driver_dl_open)
#define driver_dl_sym (WinDynDriverCallbacks.driver_dl_sym)
#define driver_dl_close (WinDynDriverCallbacks.driver_dl_close)
#define driver_dl_error (WinDynDriverCallbacks.driver_dl_error)
#define erts_alc_test (WinDynDriverCallbacks.erts_alc_test)
#define driver_binary_get_refc (WinDynDriverCallbacks.driver_binary_get_refc)
#define driver_binary_inc_refc (WinDynDriverCallbacks.driver_binary_inc_refc)
#define driver_binary_dec_refc (WinDynDriverCallbacks.driver_binary_dec_refc)
#define driver_pdl_create (WinDynDriverCallbacks.driver_pdl_create)
#define driver_pdl_lock (WinDynDriverCallbacks.driver_pdl_lock)
#define driver_pdl_unlock (WinDynDriverCallbacks.driver_pdl_unlock)
#define driver_pdl_get_refc (WinDynDriverCallbacks.driver_pdl_get_refc)
#define driver_pdl_inc_refc (WinDynDriverCallbacks.driver_pdl_inc_refc)
#define driver_pdl_dec_refc (WinDynDriverCallbacks.driver_pdl_dec_refc)
#define driver_system_info (WinDynDriverCallbacks.driver_system_info)
#define driver_get_now (WinDynDriverCallbacks.driver_get_now)
#define erl_drv_monotonic_time (WinDynDriverCallbacks.erl_drv_monotonic_time)
#define erl_drv_time_offset (WinDynDriverCallbacks.erl_drv_time_offset)
#define erl_drv_convert_time_unit (WinDynDriverCallbacks.erl_drv_convert_time_unit)
#define driver_monitor_process \
(WinDynDriverCallbacks.driver_monitor_process)
#define driver_demonitor_process \
(WinDynDriverCallbacks.driver_demonitor_process)
#define driver_get_monitored_process \
(WinDynDriverCallbacks.driver_get_monitored_process)
#define driver_compare_monitors \
(WinDynDriverCallbacks.driver_compare_monitors)
#define erl_drv_mutex_create (WinDynDriverCallbacks.erl_drv_mutex_create)
#define erl_drv_mutex_destroy (WinDynDriverCallbacks.erl_drv_mutex_destroy)
#define erl_drv_mutex_trylock (WinDynDriverCallbacks.erl_drv_mutex_trylock)
#define erl_drv_mutex_lock (WinDynDriverCallbacks.erl_drv_mutex_lock)
#define erl_drv_mutex_unlock (WinDynDriverCallbacks.erl_drv_mutex_unlock)
#define erl_drv_cond_create (WinDynDriverCallbacks.erl_drv_cond_create)
#define erl_drv_cond_destroy (WinDynDriverCallbacks.erl_drv_cond_destroy)
#define erl_drv_cond_signal (WinDynDriverCallbacks.erl_drv_cond_signal)
#define erl_drv_cond_broadcast (WinDynDriverCallbacks.erl_drv_cond_broadcast)
#define erl_drv_cond_wait (WinDynDriverCallbacks.erl_drv_cond_wait)
#define erl_drv_rwlock_create (WinDynDriverCallbacks.erl_drv_rwlock_create)
#define erl_drv_rwlock_destroy (WinDynDriverCallbacks.erl_drv_rwlock_destroy)
#define erl_drv_rwlock_tryrlock (WinDynDriverCallbacks.erl_drv_rwlock_tryrlock)
#define erl_drv_rwlock_rlock (WinDynDriverCallbacks.erl_drv_rwlock_rlock)
#define erl_drv_rwlock_runlock (WinDynDriverCallbacks.erl_drv_rwlock_runlock)
#define erl_drv_rwlock_tryrwlock \
(WinDynDriverCallbacks.erl_drv_rwlock_tryrwlock)
#define erl_drv_rwlock_rwlock (WinDynDriverCallbacks.erl_drv_rwlock_rwlock)
#define erl_drv_rwlock_rwunlock (WinDynDriverCallbacks.erl_drv_rwlock_rwunlock)
#define erl_drv_tsd_key_create (WinDynDriverCallbacks.erl_drv_tsd_key_create)
#define erl_drv_tsd_key_destroy (WinDynDriverCallbacks.erl_drv_tsd_key_destroy)
#define erl_drv_tsd_set (WinDynDriverCallbacks.erl_drv_tsd_set)
#define erl_drv_tsd_get (WinDynDriverCallbacks.erl_drv_tsd_get)
#define erl_drv_thread_opts_create \
(WinDynDriverCallbacks.erl_drv_thread_opts_create)
#define erl_drv_thread_opts_destroy \
(WinDynDriverCallbacks.erl_drv_thread_opts_destroy)
#define erl_drv_thread_create (WinDynDriverCallbacks.erl_drv_thread_create)
#define erl_drv_thread_self (WinDynDriverCallbacks.erl_drv_thread_self)
#define erl_drv_equal_tids (WinDynDriverCallbacks.erl_drv_equal_tids)
#define erl_drv_thread_exit (WinDynDriverCallbacks.erl_drv_thread_exit)
#define erl_drv_thread_join (WinDynDriverCallbacks.erl_drv_thread_join)
#define erl_drv_putenv (WinDynDriverCallbacks.erl_drv_putenv)
#define erl_drv_getenv (WinDynDriverCallbacks.erl_drv_getenv)

/* The only variable in the interface... */
#define driver_term_nil (driver_mk_term_nil())

#include <stdio.h>
#include <stdlib.h> 

#define DRIVER_INIT(DriverName)									\
ErlDrvEntry *erl_dyndriver_real_driver_init(void);									\
TWinDynDriverCallbacks WinDynDriverCallbacks;							\
__declspec(dllexport) ErlDrvEntry *driver_init(TWinDynDriverCallbacks *callbacks)	        \
{												\
    memcpy(&WinDynDriverCallbacks,callbacks,sizeof(TWinDynDriverCallbacks));			\
    return erl_dyndriver_real_driver_init();									\
}												\
ErlDrvEntry *erl_dyndriver_real_driver_init(void)

/* This is to make erl_driver.h avoid changing what's done here */
#define ERL_DRIVER_TYPES_ONLY

#else /* defined(STATIC_ERLANG_DRIVER) */
/* This is for the ddll driver */

#define ERL_INIT_CALLBACK_STRUCTURE(W)			\
do {				                        \
((W).null_func) = null_func;				\
((W).driver_failure_atom) = driver_failure_atom;	\
((W).driver_failure_posix) = driver_failure_posix;	\
((W).driver_failure) = driver_failure;			\
((W).driver_exit) = driver_exit;			\
((W).driver_failure_eof) = driver_failure_eof;		\
((W).erl_drv_busy_msgq_limits) = erl_drv_busy_msgq_limits;\
((W).driver_select) = driver_select;			\
((W).driver_event) = driver_event;			\
((W).driver_output) = driver_output;			\
((W).driver_output2) = driver_output2;			\
((W).driver_output_binary) = driver_output_binary;	\
((W).driver_outputv) = driver_outputv;			\
((W).driver_vec_to_buf) = driver_vec_to_buf;		\
((W).driver_set_timer) = driver_set_timer;		\
((W).driver_cancel_timer) = driver_cancel_timer;	\
((W).driver_read_timer) = driver_read_timer;		\
((W).erl_drv_consume_timeslice) = erl_drv_consume_timeslice;\
((W).erl_errno_id) = erl_errno_id;			\
((W).set_busy_port) = set_busy_port;			\
((W).set_port_control_flags) = set_port_control_flags;	\
((W).get_port_flags) = get_port_flags;			\
((W).driver_alloc_binary) = driver_alloc_binary;	\
((W).driver_realloc_binary) = driver_realloc_binary;	\
((W).driver_free_binary) = driver_free_binary;		\
((W).driver_alloc) = driver_alloc;			\
((W).driver_realloc) = driver_realloc;			\
((W).driver_free) = driver_free;			\
((W).driver_enq) = driver_enq;				\
((W).driver_pushq) = driver_pushq;			\
((W).driver_deq) = driver_deq;				\
((W).driver_sizeq) = driver_sizeq;			\
((W).driver_enq_bin) = driver_enq_bin;			\
((W).driver_pushq_bin) = driver_pushq_bin;		\
((W).driver_peekqv) = driver_peekqv;			\
((W).driver_peekq) = driver_peekq;			\
((W).driver_enqv) = driver_enqv;			\
((W).driver_pushqv) = driver_pushqv;			\
((W).add_driver_entry) = add_driver_entry;		\
((W).remove_driver_entry) = remove_driver_entry;	\
((W).driver_mk_atom) = driver_mk_atom;			\
((W).driver_mk_port) = driver_mk_port;			\
((W).driver_connected) = driver_connected;		\
((W).driver_caller) = driver_caller;			\
((W).driver_mk_term_nil) = driver_mk_term_nil;		\
((W).erl_drv_output_term) = erl_drv_output_term;	\
((W).driver_output_term) = driver_output_term;		\
((W).erl_drv_send_term) = erl_drv_send_term;		\
((W).driver_send_term) = driver_send_term;		\
((W).driver_async_port_key) = driver_async_port_key;	\
((W).driver_async) = driver_async;			\
((W).driver_lock_driver) = driver_lock_driver;	       	\
((W).driver_dl_open) =  driver_dl_open;			\
((W).driver_dl_sym) =  driver_dl_sym;			\
((W).driver_dl_close) =  driver_dl_close;		\
((W).driver_dl_error) =  driver_dl_error;		\
((W).erts_alc_test) = erts_alc_test;			\
((W).driver_binary_get_refc) = driver_binary_get_refc;	\
((W).driver_binary_inc_refc) = driver_binary_inc_refc;	\
((W).driver_binary_dec_refc) = driver_binary_dec_refc;	\
((W).driver_pdl_create) = driver_pdl_create;		\
((W).driver_pdl_lock) = driver_pdl_lock;		\
((W).driver_pdl_unlock) = driver_pdl_unlock;		\
((W).driver_pdl_get_refc) = driver_pdl_get_refc;	\
((W).driver_pdl_inc_refc) = driver_pdl_inc_refc;	\
((W).driver_pdl_dec_refc) = driver_pdl_dec_refc;	\
((W).driver_system_info) = driver_system_info;		\
((W).driver_get_now) = driver_get_now;		        \
((W).erl_drv_monotonic_time) = erl_drv_monotonic_time;	\
((W).erl_drv_time_offset) = erl_drv_time_offset;	\
((W).erl_drv_convert_time_unit) = erl_drv_convert_time_unit; \
((W).driver_monitor_process) = driver_monitor_process;    \
((W).driver_demonitor_process) = driver_demonitor_process;  \
((W).driver_get_monitored_process) = driver_get_monitored_process;      \
((W).driver_compare_monitors) = driver_compare_monitors;\
((W).erl_drv_mutex_create) = erl_drv_mutex_create;	\
((W).erl_drv_mutex_destroy) = erl_drv_mutex_destroy;	\
((W).erl_drv_mutex_trylock) = erl_drv_mutex_trylock;	\
((W).erl_drv_mutex_lock) = erl_drv_mutex_lock;		\
((W).erl_drv_mutex_unlock) = erl_drv_mutex_unlock;	\
((W).erl_drv_cond_create) = erl_drv_cond_create;	\
((W).erl_drv_cond_destroy) = erl_drv_cond_destroy;	\
((W).erl_drv_cond_signal) = erl_drv_cond_signal;	\
((W).erl_drv_cond_broadcast) = erl_drv_cond_broadcast;	\
((W).erl_drv_cond_wait) = erl_drv_cond_wait;		\
((W).erl_drv_rwlock_create) = erl_drv_rwlock_create;	\
((W).erl_drv_rwlock_destroy) = erl_drv_rwlock_destroy;	\
((W).erl_drv_rwlock_tryrlock) = erl_drv_rwlock_tryrlock;\
((W).erl_drv_rwlock_rlock) = erl_drv_rwlock_rlock;	\
((W).erl_drv_rwlock_runlock) = erl_drv_rwlock_runlock;	\
((W).erl_drv_rwlock_tryrwlock) = erl_drv_rwlock_tryrwlock;\
((W).erl_drv_rwlock_rwlock) = erl_drv_rwlock_rwlock;	\
((W).erl_drv_rwlock_rwunlock) = erl_drv_rwlock_rwunlock;\
((W).erl_drv_tsd_key_create) = erl_drv_tsd_key_create;	\
((W).erl_drv_tsd_key_destroy) = erl_drv_tsd_key_destroy;\
((W).erl_drv_tsd_set) = erl_drv_tsd_set;		\
((W).erl_drv_tsd_get) = erl_drv_tsd_get;		\
((W).erl_drv_thread_opts_create) = erl_drv_thread_opts_create;\
((W).erl_drv_thread_opts_destroy) = erl_drv_thread_opts_destroy;\
((W).erl_drv_thread_create) = erl_drv_thread_create;	\
((W).erl_drv_thread_self) = erl_drv_thread_self;	\
((W).erl_drv_equal_tids) = erl_drv_equal_tids;		\
((W).erl_drv_thread_exit) = erl_drv_thread_exit;	\
((W).erl_drv_thread_join) = erl_drv_thread_join;	\
((W).erl_drv_putenv) = erl_drv_putenv;			\
((W).erl_drv_getenv) = erl_drv_getenv;			\
} while (0)



#endif /* STATIC_ERLANG_DRIVER */
#endif /* _ERL_WIN_DYN_DRIVER_H */
