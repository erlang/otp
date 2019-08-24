/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2017. All Rights Reserved.
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

#if !defined(ERL_NIF_API_FUNC_DECL) && !defined(ERL_NIF_API_FUNC_MACRO)
#  error This file should not be included directly
#endif

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

/*
** Add last to keep compatibility on Windows!!!
*/
#endif

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
#endif

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

#  define enif_make_pid(ENV, PID) ((const ERL_NIF_TERM)((PID)->pid))
#endif

