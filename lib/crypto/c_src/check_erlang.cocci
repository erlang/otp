// %CopyrightBegin%
//
// Copyright Doug Hogan 2019. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// %CopyrightEnd%

// Coccinelle script to help verify Erlang calls.
// http://coccinelle.lip6.fr
// https://github.com/coccinelle/coccinelle
//
// These work with the Erlang code because it has a rigid coding pattern.
// $ spatch.opt --all-includes -sp_file check_erlang.cocci -dir .

// Make sure resources are cleaned up properly in all paths.
// Need 'strict' so it's also checked in error handling paths.
@enif_alloc_resource@
type T;
identifier CTX, L;
identifier virtual.enif_alloc_resource, virtual.enif_release_resource;
position p, pr;
@@

 T *CTX = NULL;

 ...
 if ((CTX = enif_alloc_resource(...)@p) == NULL)
   goto L;

 ... when strict, forall
 if (CTX)
   enif_release_resource(CTX)@pr;


// After calling enif_alloc_binary(), you must either release it with
// enif_release_binary() or transfer ownership to Erlang via enif_make_binary().
@enif_alloc_binary@
expression SZ;
identifier BIN, RET, ENV, X, L;
identifier TUPLE =~ "^enif_make_tuple[0-9]+$";
identifier virtual.enif_alloc_binary, virtual.enif_make_binary;
identifier virtual.enif_release_binary;
position pa, pm, pr;
@@

// This construct is used in engine.c
(
  if (!enif_alloc_binary(SZ, &BIN)@pa)
      goto L;

  ... when strict, forall
  return
(
  enif_make_binary(ENV, &BIN)@pm
|
  TUPLE(..., enif_make_binary(ENV, &BIN)@pm)@pm
);

|
// This is the typical way we allocate and use binaries.
  int X = 0;

  ...
  if (!enif_alloc_binary(SZ, &BIN)@pa)
    goto L;
  X = 1;

  ... when strict, forall
(
  RET = enif_make_binary(ENV, &BIN)@pm;
  X = 0;
|
  if (X)
    enif_release_binary(&BIN)@pr;
|
  return enif_make_binary(ENV, &BIN)@pm;
)
)

// TODO: These don't have single checks that handle all cases.
//
// enif_consume_timeslice      returns 1 if exhausted or else 0
// enif_has_pending_exception  returns true if exception pending

@erlang_check_void@
identifier FUNCVOID =~ "^(enif_mutex_destroy|enif_mutex_lock|enif_mutex_unlock|enif_rwlock_destroy|enif_rwlock_rlock|enif_rwlock_runlock|enif_rwlock_rwlock|enif_rwlock_rwunlock|enif_system_info)$";
position p;
@@

  FUNCVOID(...)@p;


@erlang_check_null@
expression X;
identifier L;
identifier FUNCNULL =~ "^(enif_alloc|enif_alloc_resource|enif_dlopen|enif_dlsym|enif_make_new_binary|enif_mutex_create|enif_open_resource_type|enif_realloc|enif_rwlock_create)$";
position p;
@@

(
  if ((X = FUNCNULL(...)@p) == NULL)
      goto L;
|
  X = FUNCNULL(...)@p;
  if (X == NULL)
      goto L;
|
  return FUNCNULL(...)@p;
)


@erlang_check_not@
identifier L;
identifier FUNCNOT =~ "^(enif_alloc_binary|enif_get_int|enif_get_list_cell|enif_get_list_length|enif_get_long|enif_get_map_value|enif_get_resource|enif_get_tuple|enif_get_uint|enif_get_ulong|enif_inspect_binary|enif_inspect_iolist_as_binary|enif_is_atom|enif_is_binary|enif_is_current_process_alive|enif_is_empty_list|enif_is_list|enif_is_map|enif_is_tuple|enif_realloc_binary)$";
position p;
@@

(
  if (!FUNCNOT(...)@p)
      goto L;
|
  return FUNCNOT(...)@p;
)


@erlang_check_null_free@
expression X;
identifier FUNCFREE =~ "^(enif_free|enif_free_env|enif_free_iovec|enif_release_binary|enif_release_resource)$";
position p;
@@

  if (
(
 X
|
 X != NULL
)
  )
    FUNCFREE(X)@p;


@erlang_check_new@
expression RET;
identifier FUNCNEW =~ "^(enif_make_atom|enif_make_badarg|enif_make_binary|enif_make_int|enif_make_list|enif_make_list_from_array|enif_make_resource|enif_make_tuple|enif_raise_exception|enif_schedule_nif|enif_thread_self)$";
position p;
@@

(
  RET = FUNCNEW(...)@p;
|
  return FUNCNEW(...)@p;
)


// Flag any calls that aren't part of the above pattern.
@enif_alloc_not_free@

identifier FUNCVOID =~ "^(enif_mutex_destroy|enif_mutex_lock|enif_mutex_unlock|enif_rwlock_destroy|enif_rwlock_rlock|enif_rwlock_runlock|enif_rwlock_rwlock|enif_rwlock_rwunlock|enif_system_info)$";
position pvoid != {erlang_check_void.p,enif_alloc_binary.pr};

identifier FUNCNULL =~ "^(enif_alloc|enif_alloc_resource|enif_dlopen|enif_dlsym|enif_make_new_binary|enif_mutex_create|enif_open_resource_type|enif_realloc|enif_rwlock_create)$";
position pnull != {erlang_check_null.p,enif_alloc_resource.p};

identifier FUNCNOT =~ "^(enif_alloc_binary|enif_get_int|enif_get_list_cell|enif_get_list_length|enif_get_long|enif_get_map_value|enif_get_resource|enif_get_tuple|enif_get_uint|enif_get_ulong|enif_inspect_binary|enif_inspect_iolist_as_binary|enif_is_atom|enif_is_binary|enif_is_current_process_alive|enif_is_empty_list|enif_is_list|enif_is_map|enif_is_tuple|enif_realloc_binary)$";
position pnot != {erlang_check_not.p,enif_alloc_binary.pa};

identifier FUNCNEW =~ "^(enif_make_atom|enif_make_badarg|enif_make_binary|enif_make_int|enif_make_list|enif_make_list_from_array|enif_make_resource|enif_make_tuple|enif_raise_exception|enif_schedule_nif|enif_thread_self)$";
position pnew != {erlang_check_new.p,enif_alloc_binary.pm};

identifier FUNCFREE =~ "^(enif_free|enif_free_env|enif_free_iovec|enif_release_binary|enif_release_resource)$";
position pfree != {enif_alloc_resource.pr,enif_alloc_binary.pr,erlang_check_null_free.p};

@@

(
* FUNCVOID(...)@pvoid
|
* FUNCNULL(...)@pnull
|
* FUNCNOT(...)@pnot
|
* FUNCNEW(...)@pnew
|
* FUNCFREE(...)@pfree
)
