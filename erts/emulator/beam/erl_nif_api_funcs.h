/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009. All Rights Reserved.
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
ERL_NIF_API_FUNC_DECL(void*,enif_get_data,(ErlNifEnv*));
ERL_NIF_API_FUNC_DECL(void*,enif_alloc,(ErlNifEnv*, size_t size));
ERL_NIF_API_FUNC_DECL(void,enif_free,(ErlNifEnv*, void* ptr));
ERL_NIF_API_FUNC_DECL(int,enif_is_atom,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_binary,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_is_ref,(ErlNifEnv*, ERL_NIF_TERM term));
ERL_NIF_API_FUNC_DECL(int,enif_inspect_binary,(ErlNifEnv*, ERL_NIF_TERM bin_term, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(int,enif_alloc_binary,(ErlNifEnv*, unsigned size, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(int,enif_realloc_binary,(ErlNifEnv*, ErlNifBinary* bin, unsigned size));
ERL_NIF_API_FUNC_DECL(void,enif_release_binary,(ErlNifEnv*, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(int,enif_get_int,(ErlNifEnv*, ERL_NIF_TERM term, int* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_ulong,(ErlNifEnv*, ERL_NIF_TERM term, unsigned long* ip));
ERL_NIF_API_FUNC_DECL(int,enif_get_double,(ErlNifEnv*, ERL_NIF_TERM term, double* dp));
ERL_NIF_API_FUNC_DECL(int,enif_get_list_cell,(ErlNifEnv* env, ERL_NIF_TERM term, ERL_NIF_TERM* head, ERL_NIF_TERM* tail));
ERL_NIF_API_FUNC_DECL(int,enif_get_tuple,(ErlNifEnv* env, ERL_NIF_TERM tpl, int* arity, ERL_NIF_TERM** array));
ERL_NIF_API_FUNC_DECL(int,enif_is_identical,(ErlNifEnv* env, ERL_NIF_TERM lhs, ERL_NIF_TERM rhs));
ERL_NIF_API_FUNC_DECL(int,enif_compare,(ErlNifEnv* env, ERL_NIF_TERM lhs, ERL_NIF_TERM rhs));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_binary,(ErlNifEnv* env, ErlNifBinary* bin));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_badarg,(ErlNifEnv* env));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_int,(ErlNifEnv* env, int i));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_ulong,(ErlNifEnv* env, unsigned long i));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_double,(ErlNifEnv* env, double d));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_atom,(ErlNifEnv* env, const char* name));
ERL_NIF_API_FUNC_DECL(int,enif_make_existing_atom,(ErlNifEnv* env, const char* name, ERL_NIF_TERM* atom));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_tuple,(ErlNifEnv* env, unsigned cnt, ...));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_list,(ErlNifEnv* env, unsigned cnt, ...));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_list_cell,(ErlNifEnv* env, ERL_NIF_TERM car, ERL_NIF_TERM cdr));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_string,(ErlNifEnv* env, const char* string));
ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_ref,(ErlNifEnv* env));
/*
** Add last to keep compatibility on Windows!!!
*/
#endif

#ifdef ERL_NIF_API_FUNC_MACRO
#  define enif_get_data ERL_NIF_API_FUNC_MACRO(enif_get_data)
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

#endif

