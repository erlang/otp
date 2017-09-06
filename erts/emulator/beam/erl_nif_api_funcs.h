/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2017. All Rights Reserved.
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

#if !defined(ERL_NIF_API_FUNC_DECL) && !defined(ERL_NIF_API_FUNC_MACRO)
#  error This file should not be included directly
#endif

/*
** WARNING: Add new ERL_NIF_API_FUNC_DECL entries at the bottom of the list
** to keep compatibility on Windows!!!
**
** And don't forget to increase ERL_NIF_MINOR_VERSION in erl_nif.h
** when adding functions to the API.
*/
#ifdef ERL_NIF_API_FUNC_DECL
ERL_NIF_API_FUNC_DECL(void*,enif_priv_data,(ErlNifEnv*));
ERL_NIF_API_FUNC_DECL(void*,enif_alloc,(size_t size));
ERL_NIF_API_FUNC_DECL(void,enif_free,(void* ptr));
ERL_NIF_API_FUNC_DECL(int,enif_is_atom,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_binary,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_ref,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_inspect_binary,(ErlNifEnv*, ERL_NIF_TERM bin_term, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(int,enif_alloc_binary,(size_t size, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(int,enif_realloc_binary,(ErlNifBinary* bin, size_t size));
ERL_NIF_API_FUNC_DECL(void,enif_release_binary,(ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(int,enif_get_int,(ErlNifEnv*, ERL_NIF_TERM term, int* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_ulong,(ErlNifEnv*, ERL_NIF_TERM term, unsigned long* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_double,(ErlNifEnv*, ERL_NIF_TERM term, double* dp));
ERL_NIF_API_FUNC_DECL(int,enif_get_list_cell,(ErlNifEnv* env, ERL_NIF_TERM term, ERL_NIF_TERM* head, ERL_NIF_TERM* tail));
ERL_NIF_API_FUNC_DECL(int,enif_get_tuple,(ErlNifEnv* env, ERL_NIF_TERM tpl, int* arity, const ERL_NIF_TERM** array));
ERL_NIF_API_FUNC_DECL(int,enif_is_identical,(ERL_NIF_TERM lhs, ERL_NIF_TERM rhs));
ERL_NIF_API_FUNC_DECL(int,enif_compare,(ERL_NIF_TERM lhs, ERL_NIF_TERM rhs));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_binary,(ErlNifEnv* env, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_badarg,(ErlNifEnv* env));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_int,(ErlNifEnv* env, int i));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_ulong,(ErlNifEnv* env, unsigned long i));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_double,(ErlNifEnv* env, double d));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_atom,(ErlNifEnv* env, const char* name));
ERL_NIF_API_FUNC_DECL(int,enif_make_existing_atom,(ErlNifEnv* env, const char* name, ERL_NIF_TERM* atom, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_tuple,(ErlNifEnv* env, unsigned cnt, ...));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_list,(ErlNifEnv* env, unsigned cnt, ...));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_list_cell,(ErlNifEnv* env, ERL_NIF_TERM car, ERL_NIF_TERM cdr));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_string,(ErlNifEnv* env, const char* string, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_ref,(ErlNifEnv* env));

ERL_NIF_API_FUNC_DECL(ErlNifMutex*,enif_mutex_create,(char *name));
ERL_NIF_API_FUNC_DECL(void,enif_mutex_destroy,(ErlNifMutex *mtx));
ERL_NIF_API_FUNC_DECL(int,enif_mutex_trylock,(ErlNifMutex *mtx));
ERL_NIF_API_FUNC_DECL(void,enif_mutex_lock,(ErlNifMutex *mtx));
ERL_NIF_API_FUNC_DECL(void,enif_mutex_unlock,(ErlNifMutex *mtx));
ERL_NIF_API_FUNC_DECL(ErlNifCond*,enif_cond_create,(char *name));
ERL_NIF_API_FUNC_DECL(void,enif_cond_destroy,(ErlNifCond *cnd));
ERL_NIF_API_FUNC_DECL(void,enif_cond_signal,(ErlNifCond *cnd));
ERL_NIF_API_FUNC_DECL(void,enif_cond_broadcast,(ErlNifCond *cnd));
ERL_NIF_API_FUNC_DECL(void,enif_cond_wait,(ErlNifCond *cnd, ErlNifMutex *mtx));
ERL_NIF_API_FUNC_DECL(ErlNifRWLock*,enif_rwlock_create,(char *name));
ERL_NIF_API_FUNC_DECL(void,enif_rwlock_destroy,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(int,enif_rwlock_tryrlock,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(void,enif_rwlock_rlock,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(void,enif_rwlock_runlock,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(int,enif_rwlock_tryrwlock,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(void,enif_rwlock_rwlock,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(void,enif_rwlock_rwunlock,(ErlNifRWLock *rwlck));
ERL_NIF_API_FUNC_DECL(int,enif_tsd_key_create,(char *name, ErlNifTSDKey *key));
ERL_NIF_API_FUNC_DECL(void,enif_tsd_key_destroy,(ErlNifTSDKey key));
ERL_NIF_API_FUNC_DECL(void,enif_tsd_set,(ErlNifTSDKey key, void *data));
ERL_NIF_API_FUNC_DECL(void*,enif_tsd_get,(ErlNifTSDKey key));
ERL_NIF_API_FUNC_DECL(ErlNifThreadOpts*,enif_thread_opts_create,(char *name));
ERL_NIF_API_FUNC_DECL(void,enif_thread_opts_destroy,(ErlNifThreadOpts *opts));
ERL_NIF_API_FUNC_DECL(int,enif_thread_create,(char *name,ErlNifTid *tid,void * (*func)(void *),void *args,ErlNifThreadOpts *opts));
ERL_NIF_API_FUNC_DECL(ErlNifTid,enif_thread_self,(void));
ERL_NIF_API_FUNC_DECL(int,enif_equal_tids,(ErlNifTid tid1, ErlNifTid tid2));
ERL_NIF_API_FUNC_DECL(void,enif_thread_exit,(void *resp));
ERL_NIF_API_FUNC_DECL(int,enif_thread_join,(ErlNifTid, void **respp));

ERL_NIF_API_FUNC_DECL(void*,enif_realloc,(void* ptr, size_t size));
ERL_NIF_API_FUNC_DECL(void,enif_system_info,(ErlNifSysInfo *sip, size_t si_size));
ERL_NIF_API_FUNC_DECL(int,enif_fprintf,(void/* FILE* */ *filep, const char *format, ...));
ERL_NIF_API_FUNC_DECL(int,enif_inspect_iolist_as_binary,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_sub_binary,(ErlNifEnv*, ERL_NIF_TERM bin_term, size_t pos, size_t size));
ERL_NIF_API_FUNC_DECL(int,enif_get_string,(ErlNifEnv*, ERL_NIF_TERM list, char* buf, unsigned len, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(int,enif_get_atom,(ErlNifEnv*, ERL_NIF_TERM atom, char* buf, unsigned len, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(int,enif_is_fun,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_pid,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_port,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_get_uint,(ErlNifEnv*, ERL_NIF_TERM term, unsigned* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_long,(ErlNifEnv*, ERL_NIF_TERM term, long* ip));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_uint,(ErlNifEnv*, unsigned i));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_long,(ErlNifEnv*, long i));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_tuple_from_array,(ErlNifEnv*, const ERL_NIF_TERM arr[], unsigned cnt));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_list_from_array,(ErlNifEnv*, const ERL_NIF_TERM arr[], unsigned cnt));
ERL_NIF_API_FUNC_DECL(int,enif_is_empty_list,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(ErlNifResourceType*,enif_open_resource_type,(ErlNifEnv*, const char* module_str, const char* name_str, void (*dtor)(ErlNifEnv*,void *), ErlNifResourceFlags flags, ErlNifResourceFlags* tried));
ERL_NIF_API_FUNC_DECL(void*,enif_alloc_resource,(ErlNifResourceType* type, size_t size));
ERL_NIF_API_FUNC_DECL(void,enif_release_resource,(void* obj));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_resource,(ErlNifEnv*, void* obj));
ERL_NIF_API_FUNC_DECL(int,enif_get_resource,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifResourceType* type, void** objp));
ERL_NIF_API_FUNC_DECL(size_t,enif_sizeof_resource,(void* obj));
ERL_NIF_API_FUNC_DECL(unsigned char*,enif_make_new_binary,(ErlNifEnv*,size_t size,ERL_NIF_TERM* termp));
ERL_NIF_API_FUNC_DECL(int,enif_is_list,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_tuple,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_get_atom_length,(ErlNifEnv*, ERL_NIF_TERM atom, unsigned* len, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(int,enif_get_list_length,(ErlNifEnv* env, ERL_NIF_TERM term, unsigned* len));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM, enif_make_atom_len,(ErlNifEnv* env, const char* name, size_t len));
ERL_NIF_API_FUNC_DECL(int, enif_make_existing_atom_len,(ErlNifEnv* env, const char* name, size_t len, ERL_NIF_TERM* atom, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_string_len,(ErlNifEnv* env, const char* string, size_t len, ErlNifCharEncoding));
ERL_NIF_API_FUNC_DECL(ErlNifEnv*,enif_alloc_env,(void));
ERL_NIF_API_FUNC_DECL(void,enif_free_env,(ErlNifEnv* env));
ERL_NIF_API_FUNC_DECL(void,enif_clear_env,(ErlNifEnv* env));
ERL_NIF_API_FUNC_DECL(int,enif_send,(ErlNifEnv* env, const ErlNifPid* to_pid, ErlNifEnv* msg_env, ERL_NIF_TERM msg));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_copy,(ErlNifEnv* dst_env, ERL_NIF_TERM src_term));
ERL_NIF_API_FUNC_DECL(ErlNifPid*,enif_self,(ErlNifEnv* caller_env, ErlNifPid* pid));
ERL_NIF_API_FUNC_DECL(int,enif_get_local_pid,(ErlNifEnv* env, ERL_NIF_TERM, ErlNifPid* pid));
ERL_NIF_API_FUNC_DECL(void,enif_keep_resource,(void* obj));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_resource_binary,(ErlNifEnv*,void* obj,const void* data, size_t size));
#if SIZEOF_LONG != 8
ERL_NIF_API_FUNC_DECL(int,enif_get_int64,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifSInt64* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_uint64,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifUInt64* ip));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_int64,(ErlNifEnv*, ErlNifSInt64));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_uint64,(ErlNifEnv*, ErlNifUInt64));
#endif
ERL_NIF_API_FUNC_DECL(int,enif_is_exception,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_make_reverse_list,(ErlNifEnv*, ERL_NIF_TERM term, ERL_NIF_TERM *list));
ERL_NIF_API_FUNC_DECL(int,enif_is_number,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(void*,enif_dlopen,(const char* lib, void (*err_handler)(void*,const char*), void* err_arg));
ERL_NIF_API_FUNC_DECL(void*,enif_dlsym,(void* handle, const char* symbol, void (*err_handler)(void*,const char*), void* err_arg));
ERL_NIF_API_FUNC_DECL(int,enif_consume_timeslice,(ErlNifEnv*, int percent));
ERL_NIF_API_FUNC_DECL(int, enif_is_map, (ErlNifEnv* env, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int, enif_get_map_size, (ErlNifEnv* env, ERL_NIF_TERM term, size_t *size));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM, enif_make_new_map, (ErlNifEnv* env));
ERL_NIF_API_FUNC_DECL(int, enif_make_map_put, (ErlNifEnv* env, ERL_NIF_TERM map_in, ERL_NIF_TERM key, ERL_NIF_TERM value, ERL_NIF_TERM* map_out));
ERL_NIF_API_FUNC_DECL(int, enif_get_map_value, (ErlNifEnv* env, ERL_NIF_TERM map, ERL_NIF_TERM key, ERL_NIF_TERM* value));
ERL_NIF_API_FUNC_DECL(int, enif_make_map_update, (ErlNifEnv* env, ERL_NIF_TERM map_in, ERL_NIF_TERM key, ERL_NIF_TERM value, ERL_NIF_TERM* map_out));
ERL_NIF_API_FUNC_DECL(int, enif_make_map_remove, (ErlNifEnv* env, ERL_NIF_TERM map_in, ERL_NIF_TERM key, ERL_NIF_TERM* map_out));
ERL_NIF_API_FUNC_DECL(int, enif_map_iterator_create, (ErlNifEnv *env, ERL_NIF_TERM map, ErlNifMapIterator *iter, ErlNifMapIteratorEntry entry));
ERL_NIF_API_FUNC_DECL(void, enif_map_iterator_destroy, (ErlNifEnv *env, ErlNifMapIterator *iter));
ERL_NIF_API_FUNC_DECL(int, enif_map_iterator_is_head, (ErlNifEnv *env, ErlNifMapIterator *iter));
ERL_NIF_API_FUNC_DECL(int, enif_map_iterator_is_tail, (ErlNifEnv *env, ErlNifMapIterator *iter));
ERL_NIF_API_FUNC_DECL(int, enif_map_iterator_next, (ErlNifEnv *env, ErlNifMapIterator *iter));
ERL_NIF_API_FUNC_DECL(int, enif_map_iterator_prev, (ErlNifEnv *env, ErlNifMapIterator *iter));
ERL_NIF_API_FUNC_DECL(int, enif_map_iterator_get_pair, (ErlNifEnv *env, ErlNifMapIterator *iter, ERL_NIF_TERM *key, ERL_NIF_TERM *value));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_schedule_nif,(ErlNifEnv*,const char*,int,ERL_NIF_TERM (*)(ErlNifEnv*,int,const ERL_NIF_TERM[]),int,const ERL_NIF_TERM[]));
ERL_NIF_API_FUNC_DECL(int, enif_has_pending_exception, (ErlNifEnv *env, ERL_NIF_TERM* reason));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM, enif_raise_exception, (ErlNifEnv *env, ERL_NIF_TERM reason));
ERL_NIF_API_FUNC_DECL(int,enif_getenv,(const char* key, char* value, size_t* value_size));
ERL_NIF_API_FUNC_DECL(ErlNifTime, enif_monotonic_time, (ErlNifTimeUnit));
ERL_NIF_API_FUNC_DECL(ErlNifTime, enif_time_offset, (ErlNifTimeUnit));
ERL_NIF_API_FUNC_DECL(ErlNifTime, enif_convert_time_unit, (ErlNifTime, ErlNifTimeUnit, ErlNifTimeUnit));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM, enif_now_time, (ErlNifEnv *env));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM, enif_cpu_time, (ErlNifEnv *env));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM, enif_make_unique_integer, (ErlNifEnv *env, ErlNifUniqueInteger properties));
ERL_NIF_API_FUNC_DECL(int, enif_is_current_process_alive, (ErlNifEnv *env));
ERL_NIF_API_FUNC_DECL(int, enif_is_process_alive, (ErlNifEnv *env, ErlNifPid *pid));
ERL_NIF_API_FUNC_DECL(int, enif_is_port_alive, (ErlNifEnv *env, ErlNifPort *port_id));
ERL_NIF_API_FUNC_DECL(int, enif_get_local_port, (ErlNifEnv* env, ERL_NIF_TERM, ErlNifPort* port_id));
ERL_NIF_API_FUNC_DECL(int, enif_term_to_binary, (ErlNifEnv *env, ERL_NIF_TERM term, ErlNifBinary *bin));
ERL_NIF_API_FUNC_DECL(size_t, enif_binary_to_term, (ErlNifEnv *env, const unsigned char* data, size_t sz, ERL_NIF_TERM *term, unsigned int opts));
ERL_NIF_API_FUNC_DECL(int, enif_port_command, (ErlNifEnv *env, const ErlNifPort* to_port, ErlNifEnv *msg_env, ERL_NIF_TERM msg));
ERL_NIF_API_FUNC_DECL(int,enif_thread_type,(void));
ERL_NIF_API_FUNC_DECL(int,enif_snprintf,(char * buffer, size_t size, const char *format, ...));
ERL_NIF_API_FUNC_DECL(int,enif_select,(ErlNifEnv* env, ErlNifEvent e, enum ErlNifSelectFlags flags, void* obj, const ErlNifPid* pid, ERL_NIF_TERM ref));
ERL_NIF_API_FUNC_DECL(ErlNifResourceType*,enif_open_resource_type_x,(ErlNifEnv*, const char* name_str, const ErlNifResourceTypeInit*, ErlNifResourceFlags flags, ErlNifResourceFlags* tried));
ERL_NIF_API_FUNC_DECL(int, enif_monitor_process,(ErlNifEnv*,void* obj,const ErlNifPid*,ErlNifMonitor *monitor));
ERL_NIF_API_FUNC_DECL(int, enif_demonitor_process,(ErlNifEnv*,void* obj,const ErlNifMonitor *monitor));
ERL_NIF_API_FUNC_DECL(int, enif_compare_monitors,(const ErlNifMonitor*,const ErlNifMonitor*));
ERL_NIF_API_FUNC_DECL(ErlNifUInt64,enif_hash,(ErlNifHash type, ERL_NIF_TERM term, ErlNifUInt64 salt));
ERL_NIF_API_FUNC_DECL(int, enif_whereis_pid, (ErlNifEnv *env, ERL_NIF_TERM name, ErlNifPid *pid));
ERL_NIF_API_FUNC_DECL(int, enif_whereis_port, (ErlNifEnv *env, ERL_NIF_TERM name, ErlNifPort *port));

ERL_NIF_API_FUNC_DECL(ErlNifIOQueue *,enif_ioq_create,(ErlNifIOQueueOpts opts));
ERL_NIF_API_FUNC_DECL(void,enif_ioq_destroy,(ErlNifIOQueue *q));

ERL_NIF_API_FUNC_DECL(int,enif_ioq_enq_binary,(ErlNifIOQueue *q, ErlNifBinary *bin, size_t skip));
ERL_NIF_API_FUNC_DECL(int,enif_ioq_enqv,(ErlNifIOQueue *q, ErlNifIOVec *iov, size_t skip));

ERL_NIF_API_FUNC_DECL(size_t,enif_ioq_size,(ErlNifIOQueue *q));
ERL_NIF_API_FUNC_DECL(int,enif_ioq_deq,(ErlNifIOQueue *q, size_t count, size_t *size));

ERL_NIF_API_FUNC_DECL(SysIOVec*,enif_ioq_peek,(ErlNifIOQueue *q, int *iovlen));

ERL_NIF_API_FUNC_DECL(int,enif_inspect_iovec,(ErlNifEnv *env, size_t max_length, ERL_NIF_TERM iovec_term, ERL_NIF_TERM *tail, ErlNifIOVec **iovec));
ERL_NIF_API_FUNC_DECL(void,enif_free_iovec,(ErlNifIOVec *iov));


/*
** ADD NEW ENTRIES HERE (before this comment) !!!
*/
#endif /* ERL_NIF_API_FUNC_DECL */

/*
** Please keep the ERL_NIF_API_FUNC_MACRO list below in the same order
** as the ERL_NIF_API_FUNC_DECL list above
*/
#ifdef ERL_NIF_API_FUNC_MACRO
#  define enif_priv_data ERL_NIF_API_FUNC_MACRO(enif_priv_data)
#  define enif_alloc ERL_NIF_API_FUNC_MACRO(enif_alloc)
#  define enif_free ERL_NIF_API_FUNC_MACRO(enif_free)
#  define enif_is_atom ERL_NIF_API_FUNC_MACRO(enif_is_atom)
#  define enif_is_binary ERL_NIF_API_FUNC_MACRO(enif_is_binary)
#  define enif_is_ref ERL_NIF_API_FUNC_MACRO(enif_is_ref)
#  define enif_inspect_binary ERL_NIF_API_FUNC_MACRO(enif_inspect_binary)
#  define enif_alloc_binary ERL_NIF_API_FUNC_MACRO(enif_alloc_binary)
#  define enif_realloc_binary ERL_NIF_API_FUNC_MACRO(enif_realloc_binary)
#  define enif_release_binary ERL_NIF_API_FUNC_MACRO(enif_release_binary)
#  define enif_get_int ERL_NIF_API_FUNC_MACRO(enif_get_int)
#  define enif_get_ulong ERL_NIF_API_FUNC_MACRO(enif_get_ulong)
#  define enif_get_double ERL_NIF_API_FUNC_MACRO(enif_get_double)
#  define enif_get_tuple ERL_NIF_API_FUNC_MACRO(enif_get_tuple)
#  define enif_get_list_cell ERL_NIF_API_FUNC_MACRO(enif_get_list_cell)
#  define enif_is_identical ERL_NIF_API_FUNC_MACRO(enif_is_identical)
#  define enif_compare ERL_NIF_API_FUNC_MACRO(enif_compare)

#  define enif_make_binary ERL_NIF_API_FUNC_MACRO(enif_make_binary)
#  define enif_make_badarg ERL_NIF_API_FUNC_MACRO(enif_make_badarg)
#  define enif_make_int ERL_NIF_API_FUNC_MACRO(enif_make_int)
#  define enif_make_ulong ERL_NIF_API_FUNC_MACRO(enif_make_ulong)
#  define enif_make_double ERL_NIF_API_FUNC_MACRO(enif_make_double)
#  define enif_make_atom ERL_NIF_API_FUNC_MACRO(enif_make_atom)
#  define enif_make_existing_atom ERL_NIF_API_FUNC_MACRO(enif_make_existing_atom)
#  define enif_make_tuple ERL_NIF_API_FUNC_MACRO(enif_make_tuple)
#  define enif_make_list ERL_NIF_API_FUNC_MACRO(enif_make_list)
#  define enif_make_list_cell ERL_NIF_API_FUNC_MACRO(enif_make_list_cell)
#  define enif_make_string ERL_NIF_API_FUNC_MACRO(enif_make_string)
#  define enif_make_ref ERL_NIF_API_FUNC_MACRO(enif_make_ref)

#  define enif_mutex_create ERL_NIF_API_FUNC_MACRO(enif_mutex_create) 
#  define enif_mutex_destroy ERL_NIF_API_FUNC_MACRO(enif_mutex_destroy) 
#  define enif_mutex_trylock ERL_NIF_API_FUNC_MACRO(enif_mutex_trylock) 
#  define enif_mutex_lock ERL_NIF_API_FUNC_MACRO(enif_mutex_lock) 
#  define enif_mutex_unlock ERL_NIF_API_FUNC_MACRO(enif_mutex_unlock) 
#  define enif_cond_create ERL_NIF_API_FUNC_MACRO(enif_cond_create) 
#  define enif_cond_destroy ERL_NIF_API_FUNC_MACRO(enif_cond_destroy) 
#  define enif_cond_signal ERL_NIF_API_FUNC_MACRO(enif_cond_signal) 
#  define enif_cond_broadcast ERL_NIF_API_FUNC_MACRO(enif_cond_broadcast) 
#  define enif_cond_wait ERL_NIF_API_FUNC_MACRO(enif_cond_wait) 
#  define enif_rwlock_create ERL_NIF_API_FUNC_MACRO(enif_rwlock_create) 
#  define enif_rwlock_destroy ERL_NIF_API_FUNC_MACRO(enif_rwlock_destroy) 
#  define enif_rwlock_tryrlock ERL_NIF_API_FUNC_MACRO(enif_rwlock_tryrlock) 
#  define enif_rwlock_rlock ERL_NIF_API_FUNC_MACRO(enif_rwlock_rlock) 
#  define enif_rwlock_runlock ERL_NIF_API_FUNC_MACRO(enif_rwlock_runlock) 
#  define enif_rwlock_tryrwlock ERL_NIF_API_FUNC_MACRO(enif_rwlock_tryrwlock) 
#  define enif_rwlock_rwlock ERL_NIF_API_FUNC_MACRO(enif_rwlock_rwlock) 
#  define enif_rwlock_rwunlock ERL_NIF_API_FUNC_MACRO(enif_rwlock_rwunlock) 
#  define enif_tsd_key_create ERL_NIF_API_FUNC_MACRO(enif_tsd_key_create) 
#  define enif_tsd_key_destroy ERL_NIF_API_FUNC_MACRO(enif_tsd_key_destroy) 
#  define enif_tsd_set ERL_NIF_API_FUNC_MACRO(enif_tsd_set) 
#  define enif_tsd_get ERL_NIF_API_FUNC_MACRO(enif_tsd_get) 
#  define enif_thread_opts_create ERL_NIF_API_FUNC_MACRO(enif_thread_opts_create) 
#  define enif_thread_opts_destroy ERL_NIF_API_FUNC_MACRO(enif_thread_opts_destroy) 
#  define enif_thread_create ERL_NIF_API_FUNC_MACRO(enif_thread_create) 
#  define enif_thread_self ERL_NIF_API_FUNC_MACRO(enif_thread_self) 
#  define enif_equal_tids ERL_NIF_API_FUNC_MACRO(enif_equal_tids) 
#  define enif_thread_exit ERL_NIF_API_FUNC_MACRO(enif_thread_exit) 
#  define enif_thread_join ERL_NIF_API_FUNC_MACRO(enif_thread_join) 

#  define enif_realloc ERL_NIF_API_FUNC_MACRO(enif_realloc) 
#  define enif_system_info ERL_NIF_API_FUNC_MACRO(enif_system_info) 
#  define enif_fprintf ERL_NIF_API_FUNC_MACRO(enif_fprintf) 
#  define enif_inspect_iolist_as_binary ERL_NIF_API_FUNC_MACRO(enif_inspect_iolist_as_binary)
#  define enif_make_sub_binary ERL_NIF_API_FUNC_MACRO(enif_make_sub_binary)
#  define enif_get_string ERL_NIF_API_FUNC_MACRO(enif_get_string)
#  define enif_get_atom ERL_NIF_API_FUNC_MACRO(enif_get_atom)
#  define enif_is_fun ERL_NIF_API_FUNC_MACRO(enif_is_fun)
#  define enif_is_pid ERL_NIF_API_FUNC_MACRO(enif_is_pid)
#  define enif_is_port ERL_NIF_API_FUNC_MACRO(enif_is_port)
#  define enif_get_uint ERL_NIF_API_FUNC_MACRO(enif_get_uint)
#  define enif_get_long ERL_NIF_API_FUNC_MACRO(enif_get_long)
#  define enif_make_uint ERL_NIF_API_FUNC_MACRO(enif_make_uint)
#  define enif_make_long ERL_NIF_API_FUNC_MACRO(enif_make_long)
#  define enif_make_tuple_from_array ERL_NIF_API_FUNC_MACRO(enif_make_tuple_from_array)
#  define enif_make_list_from_array ERL_NIF_API_FUNC_MACRO(enif_make_list_from_array)
#  define enif_is_empty_list ERL_NIF_API_FUNC_MACRO(enif_is_empty_list)
#  define enif_open_resource_type ERL_NIF_API_FUNC_MACRO(enif_open_resource_type)
#  define enif_alloc_resource ERL_NIF_API_FUNC_MACRO(enif_alloc_resource)
#  define enif_release_resource ERL_NIF_API_FUNC_MACRO(enif_release_resource)
#  define enif_make_resource ERL_NIF_API_FUNC_MACRO(enif_make_resource)
#  define enif_get_resource ERL_NIF_API_FUNC_MACRO(enif_get_resource)
#  define enif_sizeof_resource ERL_NIF_API_FUNC_MACRO(enif_sizeof_resource)
#  define enif_make_new_binary ERL_NIF_API_FUNC_MACRO(enif_make_new_binary)
#  define enif_is_list ERL_NIF_API_FUNC_MACRO(enif_is_list)
#  define enif_is_tuple ERL_NIF_API_FUNC_MACRO(enif_is_tuple)
#  define enif_get_atom_length ERL_NIF_API_FUNC_MACRO(enif_get_atom_length)
#  define enif_get_list_length ERL_NIF_API_FUNC_MACRO(enif_get_list_length)
#  define enif_make_atom_len ERL_NIF_API_FUNC_MACRO(enif_make_atom_len)
#  define enif_make_existing_atom_len ERL_NIF_API_FUNC_MACRO(enif_make_existing_atom_len)
#  define enif_make_string_len ERL_NIF_API_FUNC_MACRO(enif_make_string_len)
#  define enif_alloc_env ERL_NIF_API_FUNC_MACRO(enif_alloc_env)
#  define enif_free_env ERL_NIF_API_FUNC_MACRO(enif_free_env)
#  define enif_clear_env ERL_NIF_API_FUNC_MACRO(enif_clear_env)
#  define enif_send ERL_NIF_API_FUNC_MACRO(enif_send)
#  define enif_make_copy ERL_NIF_API_FUNC_MACRO(enif_make_copy)
#  define enif_self ERL_NIF_API_FUNC_MACRO(enif_self)
#  define enif_get_local_pid ERL_NIF_API_FUNC_MACRO(enif_get_local_pid)
#  define enif_keep_resource ERL_NIF_API_FUNC_MACRO(enif_keep_resource)
#  define enif_make_resource_binary ERL_NIF_API_FUNC_MACRO(enif_make_resource_binary)
#if  SIZEOF_LONG != 8
#  define enif_get_int64 ERL_NIF_API_FUNC_MACRO(enif_get_int64)
#  define enif_get_uint64 ERL_NIF_API_FUNC_MACRO(enif_get_uint64)
#  define enif_make_int64 ERL_NIF_API_FUNC_MACRO(enif_make_int64)
#  define enif_make_uint64 ERL_NIF_API_FUNC_MACRO(enif_make_uint64)
#endif
#  define enif_is_exception ERL_NIF_API_FUNC_MACRO(enif_is_exception)
#  define enif_make_reverse_list ERL_NIF_API_FUNC_MACRO(enif_make_reverse_list)
#  define enif_is_number ERL_NIF_API_FUNC_MACRO(enif_is_number)
#  define enif_dlopen ERL_NIF_API_FUNC_MACRO(enif_dlopen)
#  define enif_dlsym ERL_NIF_API_FUNC_MACRO(enif_dlsym)
#  define enif_consume_timeslice ERL_NIF_API_FUNC_MACRO(enif_consume_timeslice)
#  define enif_is_map ERL_NIF_API_FUNC_MACRO(enif_is_map)
#  define enif_get_map_size ERL_NIF_API_FUNC_MACRO(enif_get_map_size)
#  define enif_make_new_map ERL_NIF_API_FUNC_MACRO(enif_make_new_map)
#  define enif_make_map_put ERL_NIF_API_FUNC_MACRO(enif_make_map_put)
#  define enif_get_map_value ERL_NIF_API_FUNC_MACRO(enif_get_map_value)
#  define enif_make_map_update ERL_NIF_API_FUNC_MACRO(enif_make_map_update)
#  define enif_make_map_remove ERL_NIF_API_FUNC_MACRO(enif_make_map_remove)
#  define enif_map_iterator_create ERL_NIF_API_FUNC_MACRO(enif_map_iterator_create)
#  define enif_map_iterator_destroy ERL_NIF_API_FUNC_MACRO(enif_map_iterator_destroy)
#  define enif_map_iterator_is_head ERL_NIF_API_FUNC_MACRO(enif_map_iterator_is_head)
#  define enif_map_iterator_is_tail ERL_NIF_API_FUNC_MACRO(enif_map_iterator_is_tail)
#  define enif_map_iterator_next ERL_NIF_API_FUNC_MACRO(enif_map_iterator_next)
#  define enif_map_iterator_prev ERL_NIF_API_FUNC_MACRO(enif_map_iterator_prev)
#  define enif_map_iterator_get_pair ERL_NIF_API_FUNC_MACRO(enif_map_iterator_get_pair)
#  define enif_schedule_nif ERL_NIF_API_FUNC_MACRO(enif_schedule_nif)
#  define enif_has_pending_exception ERL_NIF_API_FUNC_MACRO(enif_has_pending_exception)
#  define enif_raise_exception ERL_NIF_API_FUNC_MACRO(enif_raise_exception)
#  define enif_getenv ERL_NIF_API_FUNC_MACRO(enif_getenv)
#  define enif_monotonic_time ERL_NIF_API_FUNC_MACRO(enif_monotonic_time)
#  define enif_time_offset ERL_NIF_API_FUNC_MACRO(enif_time_offset)
#  define enif_convert_time_unit ERL_NIF_API_FUNC_MACRO(enif_convert_time_unit)
#  define enif_now_time ERL_NIF_API_FUNC_MACRO(enif_now_time)
#  define enif_cpu_time ERL_NIF_API_FUNC_MACRO(enif_cpu_time)
#  define enif_make_unique_integer ERL_NIF_API_FUNC_MACRO(enif_make_unique_integer)
#  define enif_is_current_process_alive ERL_NIF_API_FUNC_MACRO(enif_is_current_process_alive)
#  define enif_is_process_alive ERL_NIF_API_FUNC_MACRO(enif_is_process_alive)
#  define enif_is_port_alive ERL_NIF_API_FUNC_MACRO(enif_is_port_alive)
#  define enif_get_local_port ERL_NIF_API_FUNC_MACRO(enif_get_local_port)
#  define enif_term_to_binary ERL_NIF_API_FUNC_MACRO(enif_term_to_binary)
#  define enif_binary_to_term ERL_NIF_API_FUNC_MACRO(enif_binary_to_term)
#  define enif_port_command ERL_NIF_API_FUNC_MACRO(enif_port_command)
#  define enif_thread_type ERL_NIF_API_FUNC_MACRO(enif_thread_type)
#  define enif_snprintf ERL_NIF_API_FUNC_MACRO(enif_snprintf)
#  define enif_select ERL_NIF_API_FUNC_MACRO(enif_select)
#  define enif_open_resource_type_x ERL_NIF_API_FUNC_MACRO(enif_open_resource_type_x)
#  define enif_monitor_process ERL_NIF_API_FUNC_MACRO(enif_monitor_process)
#  define enif_demonitor_process ERL_NIF_API_FUNC_MACRO(enif_demonitor_process)
#  define enif_compare_monitors ERL_NIF_API_FUNC_MACRO(enif_compare_monitors)
#  define enif_hash ERL_NIF_API_FUNC_MACRO(enif_hash)
#  define enif_whereis_pid ERL_NIF_API_FUNC_MACRO(enif_whereis_pid)
#  define enif_whereis_port ERL_NIF_API_FUNC_MACRO(enif_whereis_port)
#  define enif_ioq_create ERL_NIF_API_FUNC_MACRO(enif_ioq_create)
#  define enif_ioq_destroy ERL_NIF_API_FUNC_MACRO(enif_ioq_destroy)
#  define enif_ioq_enq ERL_NIF_API_FUNC_MACRO(enif_ioq_enq)
#  define enif_ioq_enq_binary ERL_NIF_API_FUNC_MACRO(enif_ioq_enq_binary)
#  define enif_ioq_enqv ERL_NIF_API_FUNC_MACRO(enif_ioq_enqv)
#  define enif_ioq_size ERL_NIF_API_FUNC_MACRO(enif_ioq_size)
#  define enif_ioq_deq ERL_NIF_API_FUNC_MACRO(enif_ioq_deq)
#  define enif_ioq_peek ERL_NIF_API_FUNC_MACRO(enif_ioq_peek)
#  define enif_inspect_iovec ERL_NIF_API_FUNC_MACRO(enif_inspect_iovec)
#  define enif_free_iovec ERL_NIF_API_FUNC_MACRO(enif_free_iovec)

/*
** ADD NEW ENTRIES HERE (before this comment)
*/

/*
 * Conditional EXPERIMENTAL stuff always last
 * Must be moved up and made unconditional to support binary backward
 * compatibility on Windows.
 */
#endif  /* ERL_NIF_API_FUNC_MACRO */


#if defined(__GNUC__) && !(defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))

/* Inline functions for compile time type checking of arguments to
   variadic functions.
*/

#  define ERL_NIF_INLINE __inline__

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple1(ErlNifEnv* env,
						    ERL_NIF_TERM e1)
{
    return enif_make_tuple(env, 1, e1);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple2(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2)
{
    return enif_make_tuple(env, 2, e1, e2);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple3(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3)
{
    return enif_make_tuple(env, 3, e1, e2, e3);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple4(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3,
						    ERL_NIF_TERM e4)
{
    return enif_make_tuple(env, 4, e1, e2, e3, e4);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple5(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3,
						    ERL_NIF_TERM e4,
						    ERL_NIF_TERM e5)
{
    return enif_make_tuple(env, 5, e1, e2, e3, e4, e5);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple6(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3,
						    ERL_NIF_TERM e4,
						    ERL_NIF_TERM e5,
						    ERL_NIF_TERM e6)
{
    return enif_make_tuple(env, 6, e1, e2, e3, e4, e5, e6);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple7(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3,
						    ERL_NIF_TERM e4,
						    ERL_NIF_TERM e5,
						    ERL_NIF_TERM e6,
						    ERL_NIF_TERM e7)
{
    return enif_make_tuple(env, 7, e1, e2, e3, e4, e5, e6, e7);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple8(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3,
						    ERL_NIF_TERM e4,
						    ERL_NIF_TERM e5,
						    ERL_NIF_TERM e6,
						    ERL_NIF_TERM e7,
						    ERL_NIF_TERM e8)
{
    return enif_make_tuple(env, 8, e1, e2, e3, e4, e5, e6, e7, e8);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_tuple9(ErlNifEnv* env,
						    ERL_NIF_TERM e1,
						    ERL_NIF_TERM e2,
						    ERL_NIF_TERM e3,
						    ERL_NIF_TERM e4,
						    ERL_NIF_TERM e5,
						    ERL_NIF_TERM e6,
						    ERL_NIF_TERM e7,
						    ERL_NIF_TERM e8,
						    ERL_NIF_TERM e9)
{
    return enif_make_tuple(env, 9, e1, e2, e3, e4, e5, e6, e7, e8, e9);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list1(ErlNifEnv* env,
						   ERL_NIF_TERM e1)
{
    return enif_make_list(env, 1, e1);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list2(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2)
{
    return enif_make_list(env, 2, e1, e2);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list3(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3)
{
    return enif_make_list(env, 3, e1, e2, e3);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list4(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3,
						   ERL_NIF_TERM e4)
{
    return enif_make_list(env, 4, e1, e2, e3, e4);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list5(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3,
						   ERL_NIF_TERM e4,
						   ERL_NIF_TERM e5)
{
    return enif_make_list(env, 5, e1, e2, e3, e4, e5);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list6(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3,
						   ERL_NIF_TERM e4,
						   ERL_NIF_TERM e5,
						   ERL_NIF_TERM e6)
{
    return enif_make_list(env, 6, e1, e2, e3, e4, e5, e6);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list7(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3,
						   ERL_NIF_TERM e4,
						   ERL_NIF_TERM e5,
						   ERL_NIF_TERM e6,
						   ERL_NIF_TERM e7)
{
    return enif_make_list(env, 7, e1, e2, e3, e4, e5, e6, e7);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list8(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3,
						   ERL_NIF_TERM e4,
						   ERL_NIF_TERM e5,
						   ERL_NIF_TERM e6,
						   ERL_NIF_TERM e7,
						   ERL_NIF_TERM e8)
{
    return enif_make_list(env, 8, e1, e2, e3, e4, e5, e6, e7, e8);
}

static ERL_NIF_INLINE ERL_NIF_TERM enif_make_list9(ErlNifEnv* env,
						   ERL_NIF_TERM e1,
						   ERL_NIF_TERM e2,
						   ERL_NIF_TERM e3,
						   ERL_NIF_TERM e4,
						   ERL_NIF_TERM e5,
						   ERL_NIF_TERM e6,
						   ERL_NIF_TERM e7,
						   ERL_NIF_TERM e8,
						   ERL_NIF_TERM e9)
{
    return enif_make_list(env, 9, e1, e2, e3, e4, e5, e6, e7, e8, e9);
}

#  undef ERL_NIF_INLINE

#else /* fallback with macros */

#ifndef enif_make_list1
#  define enif_make_list1(ENV,E1) enif_make_list(ENV,1,E1)
#  define enif_make_list2(ENV,E1,E2) enif_make_list(ENV,2,E1,E2)
#  define enif_make_list3(ENV,E1,E2,E3) enif_make_list(ENV,3,E1,E2,E3)
#  define enif_make_list4(ENV,E1,E2,E3,E4) enif_make_list(ENV,4,E1,E2,E3,E4)
#  define enif_make_list5(ENV,E1,E2,E3,E4,E5) enif_make_list(ENV,5,E1,E2,E3,E4,E5)
#  define enif_make_list6(ENV,E1,E2,E3,E4,E5,E6) enif_make_list(ENV,6,E1,E2,E3,E4,E5,E6)
#  define enif_make_list7(ENV,E1,E2,E3,E4,E5,E6,E7) enif_make_list(ENV,7,E1,E2,E3,E4,E5,E6,E7)
#  define enif_make_list8(ENV,E1,E2,E3,E4,E5,E6,E7,E8) enif_make_list(ENV,8,E1,E2,E3,E4,E5,E6,E7,E8)
#  define enif_make_list9(ENV,E1,E2,E3,E4,E5,E6,E7,E8,E9) enif_make_list(ENV,9,E1,E2,E3,E4,E5,E6,E7,E8,E9)
#  define enif_make_tuple1(ENV,E1) enif_make_tuple(ENV,1,E1)
#  define enif_make_tuple2(ENV,E1,E2) enif_make_tuple(ENV,2,E1,E2)
#  define enif_make_tuple3(ENV,E1,E2,E3) enif_make_tuple(ENV,3,E1,E2,E3)
#  define enif_make_tuple4(ENV,E1,E2,E3,E4) enif_make_tuple(ENV,4,E1,E2,E3,E4)
#  define enif_make_tuple5(ENV,E1,E2,E3,E4,E5) enif_make_tuple(ENV,5,E1,E2,E3,E4,E5)
#  define enif_make_tuple6(ENV,E1,E2,E3,E4,E5,E6) enif_make_tuple(ENV,6,E1,E2,E3,E4,E5,E6)
#  define enif_make_tuple7(ENV,E1,E2,E3,E4,E5,E6,E7) enif_make_tuple(ENV,7,E1,E2,E3,E4,E5,E6,E7)
#  define enif_make_tuple8(ENV,E1,E2,E3,E4,E5,E6,E7,E8) enif_make_tuple(ENV,8,E1,E2,E3,E4,E5,E6,E7,E8)
#  define enif_make_tuple9(ENV,E1,E2,E3,E4,E5,E6,E7,E8,E9) enif_make_tuple(ENV,9,E1,E2,E3,E4,E5,E6,E7,E8,E9)
#endif

#endif /* __GNUC__ && !WIN32 */

#ifndef enif_make_pid

#  define enif_make_pid(ENV, PID) ((void)(ENV),(const ERL_NIF_TERM)((PID)->pid))

#if SIZEOF_LONG == 8
#  define enif_get_int64 enif_get_long
#  define enif_get_uint64 enif_get_ulong
#  define enif_make_int64 enif_make_long
#  define enif_make_uint64 enif_make_ulong
#endif

#endif

