<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# ei_global

Access globally registered names.

## Description

This module provides support for registering, looking up, and unregistering
names in the `global` module. For more information, see
[`kernel:global`](`m:global`).

Notice that the functions below perform an RPC using an open file descriptor
provided by the caller. This file descriptor must not be used for other traffic
during the global operation, as the function can then receive unexpected data
and fail.

## ei_global_names()

```c
char ** ei_global_names(ec,fd,count);
```

Retrieves a list of all known global names.

- `ec` is the `ei_cnode` representing the current cnode.
- `fd` is an open descriptor to an Erlang connection.
- `count` is the address of an integer, or `NULL`. If `count` is not `NULL`, it
  is set by the function to the number of names found.

On success, the function returns an array of strings, each containing a single
registered name, and sets `count` to the number of names found. The array is
terminated by a single `NULL` pointer. On failure, the function returns `NULL`
and `count` is not modified.

> #### Note {: .info }
>
> It is the caller's responsibility to free the array afterwards. It has been
> allocated by the function with a single call to `malloc()`, so a single
> `free()` is all that is necessary.

## ei_global_register()

```c
int ei_global_register(fd,name,pid);
```

Registers a name in `global`.

- `fd` is an open descriptor to an Erlang connection.
- `name` is the name to register in `global`.
- `pid` is the pid that is to be associated with `name`. This value is returned
  by `global` when processes request the location of `name`.

Returns `0` on success, otherwise `-1`.

## ei_global_unregister()

```c
int ei_global_unregister(ec,fd,name);
```

Unregisters a name from `global`.

- `ec` is the `ei_cnode` representing the current cnode.
- `fd` is an open descriptor to an Erlang connection.
- `name` is the name to unregister from `global`.

Returns `0` on success, otherwise `-1`.

## ei_global_whereis()

```c
int ei_global_whereis(ec,fd,name,pid,node);
```

Looks up a name in `global`.

- `ec` is the `ei_cnode` representing the current cnode.
- `fd` is an open descriptor to an Erlang connection.
- `name` is the name that is to be looked up in `global`.

The `pid` parameter is a pointer to a `erlang_pid` that the function will update
with the pid associated with the global name, if successful.

If `node` is not `NULL`, it is a pointer to a buffer where the function can fill
in the name of the node where `name` is found. `node` can be passed directly to
`ei_connect()` if necessary.

On success, the function returns 0, updates the `erlang_pid` pointed to by the
pid parameter, and the `node` parameter is initialized to the node name where
`name` is found. On failure, a negative number is returned.
