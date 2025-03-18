<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2025. All Rights Reserved.

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
# erl_nif

API functions for an Erlang NIF library.

## Description

A NIF library contains native implementation of some functions of an Erlang
module. The native implemented functions (NIFs) are called like any other
functions without any difference to the caller. A NIF library is built as a
dynamically linked library file and loaded in runtime by calling
`erlang:load_nif/2`.

> #### Warning {: .warning }
>
> [](){: #WARNING } _Use this functionality with extreme care._
>
> A native function is executed as a direct extension of the native code of the
> VM. Execution is not made in a safe environment. The VM _cannot_ provide the
> same services as provided when executing Erlang code, such as pre-emptive
> scheduling or memory protection. If the native function does not behave well,
> the whole VM will misbehave.
>
> - A native function that crashes will crash the whole VM.
> - An erroneously implemented native function can cause a VM internal state
>   inconsistency, which can cause a crash of the VM, or miscellaneous
>   misbehaviors of the VM at any point after the call to the native function.
> - A native function doing [lengthy work](erl_nif.md#lengthy_work) before
>   returning degrades responsiveness of the VM, and can cause miscellaneous
>   strange behaviors. Such strange behaviors include, but are not limited to,
>   extreme memory usage, and bad load balancing between schedulers. Strange
>   behaviors that can occur because of lengthy work can also vary between
>   Erlang/OTP releases.

## Example

A minimal example of a NIF library can look as follows:

```c
/* niftest.c */
#include <erl_nif.h>

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello}
};

ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)
```

The Erlang module can look as follows:

```erlang
-module(niftest).

-export([init/0, hello/0]).

-nifs([hello/0]).

-on_load(init/0).

init() ->
      erlang:load_nif("./niftest", 0).

hello() ->
      erlang:nif_error("NIF library not loaded").
```

Compile and test can look as follows (on Linux):

```text
$> gcc -fPIC -shared -o niftest.so niftest.c -I $ERL_ROOT/usr/include/
$> erl

1> c(niftest).
{ok,niftest}
2> niftest:hello().
"Hello world!"
```

In the example above the [_`on_load`_](`e:system:code_loading.md#on_load`)
directive is used get function `init` called automatically when the module is
loaded. Function `init` in turn calls `erlang:load_nif/2` which loads the NIF
library and replaces the `hello` function with its native implementation in C.
Once loaded, a NIF library is persistent. It will not be unloaded until the
module instance that it belongs to is purged.

The [`-nifs()`](`e:system:modules.md#nifs_attribute`) attribute specifies which
functions in the module that are to be replaced by NIFs.

Each NIF must have an implementation in Erlang to be invoked if the function is
called before the NIF library is successfully loaded. A typical such stub
implementation is to call [`erlang:nif_error`](`erlang:nif_error/1`) which will
raise an exception. The Erlang function can also be used as a fallback
implementation if the NIF library lacks implementation for some OS or hardware
architecture for example.

> #### Note {: .info }
>
> A NIF does not have to be exported, it can be local to the module. However,
> unused local stub functions will be optimized away by the compiler, causing
> loading of the NIF library to fail.

## Functionality

All interaction between NIF code and the Erlang runtime system is performed by
calling NIF API functions. Functions exist for the following functionality:

- **Read and write Erlang terms** - Any Erlang terms can be passed to a NIF as
  function arguments and be returned as function return values. The terms are of
  C-type [`ERL_NIF_TERM`](erl_nif.md#ERL_NIF_TERM) and can only be read or
  written using API functions. Most functions to read the content of a term are
  prefixed `enif_get_` and usually return `true` (or `false`) if the term is of
  the expected type (or not). The functions to write terms are all prefixed
  `enif_make_` and usually return the created `ERL_NIF_TERM`. There are also
  some functions to query terms, like `enif_is_atom`, `enif_is_identical`, and
  `enif_compare`.

  All terms of type `ERL_NIF_TERM` belong to an environment of type
  [`ErlNifEnv`](erl_nif.md#ErlNifEnv), except atoms created during loading (by
  callbacks [`load`](erl_nif.md#load) or [`upgrade`](erl_nif.md#upgrade)). The
  lifetime of a term is controlled by the lifetime of its environment object.
  All API functions that read or write terms have the environment that the term
  belongs to as the first function argument. However, the atoms created during
  loading can be referred as a term in any `ErlNifEnv`. That is, the best
  practice it to create all your atoms during loading and store them in
  static/global variables, for example:

  ```c
  #include <erl_nif.h>

  ERL_NIF_TERM world_atom;

  static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
  {
      world_atom = enif_make_atom(env, "world");
      return 0;
  }

  static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
  {
      ERL_NIF_TERM hello_string = enif_make_string(env, "Hello", ERL_NIF_LATIN1);
      return enif_make_tuple2(env, hello_string, world_atom);
  }

  static ErlNifFunc nif_funcs[] = { { "hello", 0, hello } };

  ERL_NIF_INIT(niftest, nif_funcs, load, NULL, NULL, NULL)
  ```

- **Binaries** - Terms of type binary are accessed with the help of struct type
  [`ErlNifBinary`](erl_nif.md#ErlNifBinary), which contains a pointer (`data`)
  to the raw binary data and the length (`size`) of the data in bytes. Both
  `data` and `size` are read-only and are only to be written using calls to API
  functions. Instances of `ErlNifBinary` are, however, always allocated by the
  user (usually as local variables).

  The raw data pointed to by `data` is only mutable after a call to
  [`enif_alloc_binary`](erl_nif.md#enif_alloc_binary) or
  [`enif_realloc_binary`](erl_nif.md#enif_realloc_binary). All other functions
  that operate on a binary leave the data as read-only. A mutable binary must in
  the end either be freed with
  [`enif_release_binary`](erl_nif.md#enif_release_binary) or made read-only by
  transferring it to an Erlang term with
  [`enif_make_binary`](erl_nif.md#enif_make_binary). However, it does not have
  to occur in the same NIF call. Read-only binaries do not have to be released.

  [`enif_make_new_binary`](erl_nif.md#enif_make_new_binary) can be used as a
  shortcut to allocate and return a binary in the same NIF call.

  Binaries are sequences of whole bytes. Bitstrings with an arbitrary bit length
  have no support yet.

- **Resource objects**{: #resource_objects }  
  The use of resource objects is a safe way to return pointers to native data
  structures from a NIF. A resource object is only a block of memory allocated
  with [`enif_alloc_resource`](erl_nif.md#enif_alloc_resource). A handle ("safe
  pointer") to this memory block can then be returned to Erlang by the use of
  [`enif_make_resource`](erl_nif.md#enif_make_resource). The term returned by
  `enif_make_resource` is opaque in nature. It can be stored and passed between
  processes, but the only real end usage is to pass it back as an argument to a
  NIF. The NIF can then call [`enif_get_resource`](erl_nif.md#enif_get_resource)
  and get back a pointer to the memory block, which is guaranteed to still be
  valid. A resource object is not deallocated until the last handle term is
  garbage collected by the VM and the resource is released with
  [`enif_release_resource`](erl_nif.md#enif_release_resource) (not necessarily
  in that order).

  All resource objects are created as instances of some _resource type_. This
  makes resources from different modules to be distinguishable. A resource type
  is created by calling
  [`enif_open_resource_type`](erl_nif.md#enif_open_resource_type) when a library
  is loaded. Objects of that resource type can then later be allocated and
  `enif_get_resource` verifies that the resource is of the expected type. A
  resource type can have a user-supplied destructor function, which is
  automatically called when resources of that type are released (by either the
  garbage collector or `enif_release_resource`). Resource types are uniquely
  identified by a supplied name string and the name of the implementing module.

  [](){: #enif_resource_example } The following is a template example of how to
  create and return a resource object.

  ```c
  ERL_NIF_TERM term;
  MyStruct* obj = enif_alloc_resource(my_resource_type, sizeof(MyStruct));

  /* initialize struct ... */

  term = enif_make_resource(env, obj);

  if (keep_a_reference_of_our_own) {
      /* store 'obj' in static variable, private data or other resource object */
  }
  else {
      enif_release_resource(obj);
      /* resource now only owned by "Erlang" */
  }
  return term;
  ```

  Notice that once `enif_make_resource` creates the term to return to Erlang,
  the code can choose to either keep its own native pointer to the allocated
  struct and release it later, or release it immediately and rely only on the
  garbage collector to deallocate the resource object eventually when it
  collects the term.

  Another use of resource objects is to create binary terms with user-defined
  memory management.
  [`enif_make_resource_binary`](erl_nif.md#enif_make_resource_binary) creates a
  binary term that is connected to a resource object. The destructor of the
  resource is called when the binary is garbage collected, at which time the
  binary data can be released. An example of this can be a binary term
  consisting of data from a `mmap`'ed file. The destructor can then do `munmap`
  to release the memory region.

  Resource types support upgrade in runtime by allowing a loaded NIF library to
  take over an already existing resource type and by that "inherit" all existing
  objects of that type. The destructor of the new library is thereafter called
  for the inherited objects and the library with the old destructor function can
  be safely unloaded. Existing resource objects, of a module that is upgraded,
  must either be deleted or taken over by the new NIF library. The unloading of
  a library is postponed as long as there exist resource objects with a
  destructor function in the library.

- **Module upgrade and static data** - A loaded NIF library is tied to the
  Erlang module instance that loaded it. If the module is upgraded, the new
  module instance needs to load its own NIF library (or maybe choose not to).
  The new module instance can, however, choose to load the exact same NIF
  library as the old code if it wants to. Sharing the dynamic library means that
  static data defined by the library is shared as well. To avoid unintentionally
  shared static data between module instances, each Erlang module version can
  keep its own private data. This private data can be set when the NIF library
  is loaded and later retrieved by calling
  [`enif_priv_data`](erl_nif.md#enif_priv_data).

- **Threads and concurrency** - A NIF is thread-safe without any explicit
  synchronization as long as it acts as a pure function and only reads the
  supplied arguments. When you write to a shared state either through static
  variables or [`enif_priv_data`](erl_nif.md#enif_priv_data), you need to supply
  your own explicit synchronization. This includes terms in process independent
  environments that are shared between threads. Resource objects also require
  synchronization if you treat them as mutable.

  The library initialization callbacks `load` and `upgrade` are thread-safe even
  for shared state data.

- **Version Management**{: #version_management } -
  When a NIF library is built, information about the NIF API version is compiled
  into the library. When a NIF library is loaded, the runtime system verifies
  that the library is of a compatible version. `erl_nif.h` defines the
  following:

  - **`ERL_NIF_MAJOR_VERSION`** - Incremented when NIF library incompatible
    changes are made to the Erlang runtime system. Normally it suffices to
    recompile the NIF library when the `ERL_NIF_MAJOR_VERSION` has changed, but
    it can, under rare circumstances, mean that NIF libraries must be slightly
    modified. If so, this will of course be documented.

  - **`ERL_NIF_MINOR_VERSION`** - Incremented when new features are added. The
    runtime system uses the minor version to determine what features to use.

  The runtime system normally refuses to load a NIF library if the major
  versions differ, or if the major versions are equal and the minor version used
  by the NIF library is greater than the one used by the runtime system. Old NIF
  libraries with lower major versions are, however, allowed after a bump of the
  major version during a transition period of two major releases. Such old NIF
  libraries can however fail if deprecated features are used.

- **Time Measurement**{: #time_measurement } -
  Support for time measurement in NIF libraries:

  - [`ErlNifTime`](erl_nif.md#ErlNifTime)
  - [`ErlNifTimeUnit`](erl_nif.md#ErlNifTimeUnit)
  - [`enif_monotonic_time()`](erl_nif.md#enif_monotonic_time)
  - [`enif_time_offset()`](erl_nif.md#enif_time_offset)
  - [`enif_convert_time_unit()`](erl_nif.md#enif_convert_time_unit)

- **I/O Queues**{: #enif_ioq }  
  The Erlang nif library contains function for easily working with I/O vectors
  as used by the unix system call `writev`. The I/O Queue is not thread safe, so
  some other synchronization mechanism has to be used.

  - [`SysIOVec`](erl_nif.md#SysIOVec)
  - [`ErlNifIOVec`](erl_nif.md#ErlNifIOVec)
  - [`enif_ioq_create()`](erl_nif.md#enif_ioq_create)
  - [`enif_ioq_destroy()`](erl_nif.md#enif_ioq_destroy)
  - [`enif_ioq_enq_binary()`](erl_nif.md#enif_ioq_enq_binary)
  - [`enif_ioq_enqv()`](erl_nif.md#enif_ioq_enqv)
  - [`enif_ioq_deq()`](erl_nif.md#enif_ioq_deq)
  - [`enif_ioq_peek()`](erl_nif.md#enif_ioq_peek)
  - [`enif_ioq_peek_head()`](erl_nif.md#enif_ioq_peek_head)
  - [`enif_inspect_iovec()`](erl_nif.md#enif_inspect_iovec)
  - [`enif_free_iovec()`](erl_nif.md#enif_free_iovec)

  Typical usage when writing to a file descriptor looks like this:

  ```c
  int writeiovec(ErlNifEnv *env, ERL_NIF_TERM term, ERL_NIF_TERM *tail,
                 ErlNifIOQueue *q, int fd) {

      ErlNifIOVec vec, *iovec = &vec;
      SysIOVec *sysiovec;
      int saved_errno;
      int iovcnt, n;

      if (!enif_inspect_iovec(env, 64, term, tail, &iovec))
          return -2;

      if (enif_ioq_size(q) > 0) {
          /* If the I/O queue contains data we enqueue the iovec and
             then peek the data to write out of the queue. */
          if (!enif_ioq_enqv(q, iovec, 0))
              return -3;

          sysiovec = enif_ioq_peek(q, &iovcnt);
      } else {
          /* If the I/O queue is empty we skip the trip through it. */
          iovcnt = iovec->iovcnt;
          sysiovec = iovec->iov;
      }

      /* Attempt to write the data */
      n = writev(fd, sysiovec, iovcnt);
      saved_errno = errno;

      if (enif_ioq_size(q) == 0) {
          /* If the I/O queue was initially empty we enqueue any
             remaining data into the queue for writing later. */
          if (n >= 0 && !enif_ioq_enqv(q, iovec, n))
              return -3;
      } else {
          /* Dequeue any data that was written from the queue. */
          if (n > 0 && !enif_ioq_deq(q, n, NULL))
              return -4;
      }

      /* return n, which is either number of bytes written or -1 if
         some error happened */
      errno = saved_errno;
      return n;
  }
  ```

- **Long-running NIFs**{: #lengthy_work }  
  As mentioned in the [warning](erl_nif.md#WARNING) text at the beginning of
  this manual page, it is of _vital importance_ that a native function returns
  relatively fast. It is difficult to give an exact maximum amount of time that
  a native function is allowed to work, but usually a well-behaving native
  function is to return to its caller within 1 millisecond. This can be achieved
  using different approaches. If you have full control over the code to execute
  in the native function, the best approach is to divide the work into multiple
  chunks of work and call the native function multiple times. This is, however,
  not always possible, for example when calling third-party libraries.

  The [`enif_consume_timeslice()`](erl_nif.md#enif_consume_timeslice) function
  can be used to inform the runtime system about the length of the NIF call. It
  is typically always to be used unless the NIF executes very fast.

  If the NIF call is too lengthy, this must be handled in one of the following
  ways to avoid degraded responsiveness, scheduler load balancing problems, and
  other strange behaviors:

  - **Yielding NIF** - If the functionality of a long-running NIF can be split
    so that its work can be achieved through a series of shorter NIF calls, the
    application has two options:

    - Make that series of NIF calls from the Erlang level.
    - Call a NIF that first performs a chunk of the work, then invokes the
      [`enif_schedule_nif`](erl_nif.md#enif_schedule_nif) function to schedule
      another NIF call to perform the next chunk. The final call scheduled in
      this manner can then return the overall result.

    Breaking up a long-running function in this manner enables the VM to regain
    control between calls to the NIFs.

    This approach is always preferred over the other alternatives described
    below. This both from a performance perspective and a system characteristics
    perspective.

  - **Threaded NIF** - This is accomplished by dispatching the work to another
    thread managed by the NIF library, return from the NIF, and wait for the
    result. The thread can send the result back to the Erlang process using
    [`enif_send`](erl_nif.md#enif_send). Information about thread primitives is
    provided below.

  - **Dirty NIF**{: #dirty_nifs }  
    A NIF that cannot be split and cannot execute in a millisecond or less is
    called a "dirty NIF", as it performs work that the ordinary schedulers of
    the Erlang runtime system cannot handle cleanly. Applications that make use
    of such functions must indicate to the runtime that the functions are dirty
    so they can be handled specially. This is handled by executing dirty jobs on
    a separate set of schedulers called dirty schedulers. A dirty NIF executing
    on a dirty scheduler does not have the same duration restriction as a normal
    NIF.

    It is important to classify the dirty job correct. An I/O bound job should
    be classified as such, and a CPU bound job should be classified as such. If
    you should classify CPU bound jobs as I/O bound jobs, dirty I/O schedulers
    might starve ordinary schedulers. I/O bound jobs are expected to either
    block waiting for I/O, and/or spend a limited amount of time moving data.

    To schedule a dirty NIF for execution, the application has two options:

    - Set the appropriate flags value for the dirty NIF in its
      [`ErlNifFunc`](erl_nif.md#ErlNifFunc) entry.
    - Call [`enif_schedule_nif`](erl_nif.md#enif_schedule_nif), pass to it a
      pointer to the dirty NIF to be executed, and indicate with argument
      `flags` whether it expects the operation to be CPU-bound or I/O-bound.

    A job that alternates between I/O bound and CPU bound can be reclassified
    and rescheduled using `enif_schedule_nif` so that it executes on the correct
    type of dirty scheduler at all times. For more information see the
    documentation of the [erl](erl_cmd.md) command line arguments
    [`+SDcpu`](erl_cmd.md#%2BSDcpu), and [`+SDio`](erl_cmd.md#%2BSDio).

    While a process executes a dirty NIF, some operations that communicate with
    it can take a very long time to complete. Suspend or garbage collection of a
    process executing a dirty NIF cannot be done until the dirty NIF has
    returned. Thus, other processes waiting for such operations to complete
    might have to wait for a very long time. Blocking multi-scheduling, that is,
    calling
    [`erlang:system_flag(multi_scheduling, block)`](`m:erlang#system_flag_multi_scheduling`),
    can also take a very long time to complete. This is because all ongoing
    dirty operations on all dirty schedulers must complete before the block
    operation can complete.

    Many operations communicating with a process executing a dirty NIF can,
    however, complete while it executes the dirty NIF. For example, retrieving
    information about it through [`process_info`](`erlang:process_info/1`),
    setting its group leader, register/unregister its name, and so on.

    Termination of a process executing a dirty NIF can only be completed up to a
    certain point while it executes the dirty NIF. All Erlang resources, such as
    its registered name and its ETS tables, are released. All links and monitors
    are triggered. The execution of the NIF is, however, _not_ stopped. The NIF
    can safely continue execution, allocate heap memory, and so on, but it is of
    course better to stop executing as soon as possible. The NIF can check
    whether a current process is alive using
    [`enif_is_current_process_alive`](erl_nif.md#enif_is_current_process_alive).
    Communication using [`enif_send`](erl_nif.md#enif_send) and
    [`enif_port_command`](erl_nif.md#enif_port_command) is also dropped when the
    sending process is not alive. Deallocation of certain internal resources,
    such as process heap and process control block, is delayed until the dirty
    NIF has completed.

## Initialization

- **`ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, NULL, upgrade, unload)`**{: #ERL_NIF_INIT } -
  This is the magic macro to initialize a NIF library. It is
  to be evaluated in global file scope.

  `MODULE` is the name of the Erlang module as an identifier without string
  quotations. It is stringified by the macro.

  `funcs` is a static array of function descriptors for all the implemented NIFs
  in this library.

  `load`, `upgrade` and `unload` are pointers to functions. One of `load` or
  `upgrade` is called to initialize the library. `unload` is called to release
  the library. All are described individually below.

  The fourth argument `NULL` is ignored. It was earlier used for the deprecated
  `reload` callback which is no longer supported since OTP 20.

  If compiling a NIF lib for static inclusion through `--enable-static-nifs`,
  then the macro `STATIC_ERLANG_NIF_LIBNAME` must be defined as the name of the
  archive file (excluding file extension .a) without string quotations. It must
  only contain characters allowed in a C indentifier. The macro must be defined
  before `erl_nif.h` is included. If the older macro `STATIC_ERLANG_NIF` is
  instead used, then the name of the archive file must match the name of the
  module.

- __`int (*load)(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)`__ - 
  `load`{: #load } is called when the NIF library is loaded and no previously
  loaded library exists for this module.

  `*priv_data` can be set to point to some private data if the library needs to
  keep a state between NIF calls. `enif_priv_data` returns this pointer.
  `*priv_data` is initialized to `NULL` when `load` is called.

  `load_info` is the second argument to `erlang:load_nif/2`.

  The library fails to load if `load` returns anything other than `0`. `load`
  can be `NULL` if initialization is not needed.

- __`int (*upgrade)(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)`__ - `upgrade`{: #upgrade } is called when the NIF library is loaded and there is
  old code of this module with a loaded NIF library.

  Works as `load`, except that `*old_priv_data` already contains the value set
  by the last call to `load` or `upgrade` for the old module instance. `*priv_data`
  is initialized to `NULL` when `upgrade` is called. It is allowed to write to
  both `*priv_data` and `*old_priv_data.`

  The library fails to load if `upgrade` returns anything other than `0` or if
  `upgrade` is `NULL`.

- **`void (*unload)(ErlNifEnv* caller_env, void* priv_data)`**{: #unload } -
  `unload` is called when the module instance that the NIF library belongs to is
  purged as old. New code of the same module may or may not exist.

## Data Types

- **`ERL_NIF_TERM`**{: #ERL_NIF_TERM } - Variables of type `ERL_NIF_TERM` can
  refer to any Erlang term. This is an opaque type and values of it can only by
  used either as arguments to API functions or as return values from NIFs. All
  `ERL_NIF_TERM`s belong to an environment
  ([`ErlNifEnv`](erl_nif.md#ErlNifEnv)). A term cannot be destructed
  individually, it is valid until its environment is destructed.

- **`ErlNifEnv`**{: #ErlNifEnv } - `ErlNifEnv` represents an environment that
  can host Erlang terms. All terms in an environment are valid as long as the
  environment is valid. `ErlNifEnv` is an opaque type; pointers to it can only
  be passed on to API functions. Three types of environments exist:

  - **Process bound environment**{: #proc_bound_env }  
    Passed as the first argument to all NIFs. All function arguments passed to a
    NIF belong to that environment. The return value from a NIF must also be a
    term belonging to the same environment.

    A process bound environment contains transient information about the calling
    Erlang process. The environment is only valid in the thread where it was
    supplied as argument until the NIF returns. It is thus useless and dangerous
    to store pointers to process bound environments between NIF calls.

  - **Callback environment**{: #callback_env }  
    Passed as the first argument to all the non-NIF callback functions
    ([`load`](erl_nif.md#load), [`upgrade`](erl_nif.md#upgrade),
    [`unload`](erl_nif.md#unload), [`dtor`](erl_nif.md#ErlNifResourceDtor),
    [`down`](erl_nif.md#ErlNifResourceDown),
    [`stop`](erl_nif.md#ErlNifResourceStop) and
    [`dyncall`](erl_nif.md#ErlNifResourceDynCall)). Works like a process bound
    environment but with a temporary pseudo process that "terminates" when the
    callback has returned. Terms may be created in this environment but they
    will only be accessible during the callback.

  - **Process independent environment**{: #proc_indep_env }  
    Created by calling [`enif_alloc_env`](erl_nif.md#enif_alloc_env). This
    environment can be used to store terms between NIF calls and to send terms
    with [`enif_send`](erl_nif.md#enif_send). A process independent environment
    with all its terms is valid until you explicitly invalidate it with
    [`enif_free_env`](erl_nif.md#enif_free_env) or `enif_send`.

  All contained terms of a list/tuple/map must belong to the same environment as
  the list/tuple/map itself. Terms can be copied between environments with
  [`enif_make_copy`](erl_nif.md#enif_make_copy).

- **`ErlNifFunc`**{: #ErlNifFunc }

  ```c
  typedef struct {
      const char* name;
      unsigned arity;
      ERL_NIF_TERM (*fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
      unsigned flags;
  } ErlNifFunc;
  ```

  Describes a NIF by its name, arity, and implementation.

  - **`fptr`** - A pointer to the function that implements the NIF.

  - **`argv`** - Contains the function arguments passed to the NIF.

  - **`argc`** - The array length, that is, the function arity. `argv[N-1]` thus
    denotes the Nth argument to the NIF. Notice that the argument `argc` allows
    for the same C function to implement several Erlang functions with different
    arity (but probably with the same name).

  - **`flags`** - Is `0` for a regular NIF (and so its value can be omitted for
    statically initialized `ErlNifFunc` instances).

    `flags` can be used to indicate that the NIF is a
    [dirty NIF](erl_nif.md#dirty_nifs) that is to be executed on a dirty
    scheduler thread.

    If the dirty NIF is expected to be CPU-bound, its `flags` field is to be set
    to `ERL_NIF_DIRTY_JOB_CPU_BOUND`. If it's expected to be I/O-bound set
    `flags` to `ERL_NIF_DIRTY_JOB_IO_BOUND`.

- **`ErlNifBinary`**{: #ErlNifBinary }

  ```text
  typedef struct {
      size_t size;
      unsigned char* data;
  } ErlNifBinary;
  ```

  `ErlNifBinary` contains transient information about an inspected binary term.
  `data` is a pointer to a buffer of `size` bytes with the raw content of the
  binary.

  Notice that `ErlNifBinary` is a semi-opaque type and you are only allowed to
  read fields `size` and `data`.

- **`ErlNifBinaryToTerm`**{: #ErlNifBinaryToTerm } - An enumeration of the
  options that can be specified to
  [`enif_binary_to_term`](erl_nif.md#enif_binary_to_term). For default behavior,
  use value `0`.

  When receiving data from untrusted sources, use option
  `ERL_NIF_BIN2TERM_SAFE`.

- **`ErlNifMonitor`**{: #ErlNifMonitor } - This is an opaque data type that
  identifies a monitor.

  The nif writer is to provide the memory for storing the monitor when calling
  [`enif_monitor_process`](erl_nif.md#enif_monitor_process). The address of the
  data is not stored by the runtime system, so `ErlNifMonitor` can be used as
  any other data, it can be copied, moved in memory, forgotten, and so on. To
  compare two monitors,
  [`enif_compare_monitors`](erl_nif.md#enif_compare_monitors) must be used.

- **`ErlNifOnHaltCallback`**{: #ErlNifOnHaltCallback }

  ```text
  typedef void ErlNifOnHaltCallback(void *priv_data);
  ```

  The function prototype of an _on halt_ callback function.

  An _on halt_ callback can be installed using
  [`enif_set_option()`](erl_nif.md#on_halt). Such an installed callback will be
  called when the runtime system is halting.

- **`ErlNifOnUnloadThreadCallback`**{: #ErlNifOnUnloadThreadCallback }

  ```text
  typedef void ErlNifOnUnloadThreadCallback(void *priv_data);
  ```

  The function prototype of an _on_unload_thread_ callback function.

  An _on_unload_thread_ callback can be installed using
  [`enif_set_option()`](erl_nif.md#on_unload_thread). Such an installed callback
  will be called by each scheduler thread when this module instance is purged.

- **`ErlNifOption`**{: #ErlNifOption } - An enumeration of the options that can
  be set using [`enif_set_option()`](erl_nif.md#enif_set_option).

  Currently valid options:

  - **[`ERL_NIF_OPT_DELAY_HALT`](erl_nif.md#delay_halt)** - Enable delay of
    runtime system halt with flushing enabled until all calls to NIFs in the NIF
    library have returned.

  - **[`ERL_NIF_OPT_ON_HALT`](erl_nif.md#on_halt)** - Install a callback that
    will be called when the runtime system halts with flushing enabled.

  - **[`ERL_NIF_OPT_ON_UNLOAD_THREAD`](erl_nif.md#on_unload_thread)** - Install a
    callback that will be called **by each scheduler thread**
    when the module instance that the NIF library belongs to is purged as old.

- **`ErlNifPid`**{: #ErlNifPid } - A process identifier (pid). In contrast to
  pid terms (instances of `ERL_NIF_TERM`), `ErlNifPid`s are self-contained and
  not bound to any [environment](erl_nif.md#ErlNifEnv). `ErlNifPid` is an opaque
  type. It can be copied, moved in memory, forgotten, and so on.

- **`ErlNifPort`**{: #ErlNifPort } - A port identifier. In contrast to port ID
  terms (instances of `ERL_NIF_TERM`), `ErlNifPort`s are self-contained and not
  bound to any [environment](erl_nif.md#ErlNifEnv). `ErlNifPort` is an opaque
  type. It can be copied, moved in memory, forgotten, and so on.

- **`ErlNifResourceType`**{: #ErlNifResourceType } - Each instance of
  `ErlNifResourceType` represents a class of memory-managed resource objects
  that can be garbage collected. Each resource type has a unique name and a
  destructor function that is called when objects of its type are released.

- **`ErlNifResourceTypeInit`**{: #ErlNifResourceTypeInit }

  ```c
  typedef struct {
      ErlNifResourceDtor* dtor;       // #1 Destructor
      ErlNifResourceStop* stop;       // #2 Select stop
      ErlNifResourceDown* down;       // #3 Monitor down
      int members;
      ErlNifResourceDynCall* dyncall; // #4 Dynamic call
  } ErlNifResourceTypeInit;
  ```

  Initialization structure read by
  [enif_open_resource_type_x](erl_nif.md#enif_open_resource_type_x) and
  [enif_init_resource_type](erl_nif.md#enif_init_resource_type).

- **`ErlNifResourceDtor`{: #ErlNifResourceDtor }**

  ```text
  typedef void ErlNifResourceDtor(ErlNifEnv* caller_env, void* obj);
  ```

  The function prototype of a resource destructor function.

  The `obj` argument is a pointer to the resource. The only allowed use for the
  resource in the destructor is to access its user data one final time. The
  destructor is guaranteed to be the last callback before the resource is
  deallocated.

- **`ErlNifResourceDown`**{: #ErlNifResourceDown }

  ```c
  typedef void ErlNifResourceDown(ErlNifEnv* caller_env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon);
  ```

  The function prototype of a resource down function, called on the behalf of
  [enif_monitor_process](erl_nif.md#enif_monitor_process). `obj` is the
  resource, `pid` is the identity of the monitored process that is exiting, and
  `mon` is the identity of the monitor.

- **`ErlNifResourceStop`**{: #ErlNifResourceStop }

  ```text
  typedef void ErlNifResourceStop(ErlNifEnv* caller_env, void* obj, ErlNifEvent event, int is_direct_call);
  ```

  The function prototype of a resource stop function, called on the behalf of
  [enif_select](erl_nif.md#enif_select). `obj` is the resource, `event` is OS
  event, `is_direct_call` is true if the call is made directly from
  `enif_select` or false if it is a scheduled call (potentially from another
  thread).

- **`ErlNifResourceDynCall`**{: #ErlNifResourceDynCall }

  ```c
  typedef void ErlNifResourceDynCall(ErlNifEnv* caller_env, void* obj, void* call_data);
  ```

  The function prototype of a dynamic resource call function, called by
  [enif_dynamic_resource_call](erl_nif.md#enif_dynamic_resource_call). Argument
  `obj` is the resource object and `call_data` is the last argument to
  `enif_dynamic_resource_call` passed through.

- **`ErlNifCharEncoding`**{: #ErlNifCharEncoding }

  ```text
  typedef enum {
      ERL_NIF_LATIN1,
      ERL_NIF_UTF8,
  }ErlNifCharEncoding;
  ```

  The character encoding used in strings and atoms. The only supported encodings
  are `ERL_NIF_LATIN1` for ISO Latin-1 (8-bit ASCII) and `ERL_NIF_UTF8` for
  UTF-8.

- **`ErlNifSysInfo`**{: #ErlNifSysInfo } - Used by
  [`enif_system_info`](erl_nif.md#enif_system_info) to return information about
  the runtime system. Contains the same content as
  [`ErlDrvSysInfo`](erl_driver.md#ErlDrvSysInfo).

- **`ErlNifSInt64`**{: #ErlNifSInt64 } - A native signed 64-bit integer type.

- **`ErlNifUInt64`**{: #ErlNifUInt64 } - A native unsigned 64-bit integer type.

- **`ErlNifTime`**{: #ErlNifTime } - A signed 64-bit integer type for
  representation of time.

- **`ErlNifTimeUnit`**{: #ErlNifTimeUnit } - An enumeration of time units
  supported by the NIF API:

  - **`ERL_NIF_SEC`** - Seconds

  - **`ERL_NIF_MSEC`** - Milliseconds

  - **`ERL_NIF_USEC`** - Microseconds

  - **`ERL_NIF_NSEC`** - Nanoseconds

- **`ErlNifUniqueInteger`**{: #ErlNifUniqueInteger } - An enumeration of the
  properties that can be requested from
  [`enif_make_unique_integer`](erl_nif.md#enif_make_unique_integer). For default
  properties, use value `0`.

  - **`ERL_NIF_UNIQUE_POSITIVE`** - Return only positive integers.

  - **`ERL_NIF_UNIQUE_MONOTONIC`** - Return only
    [strictly monotonically increasing](time_correction.md#strictly-monotonically-increasing)
    integer corresponding to creation time.

- **`ErlNifHash`**{: #ErlNifHash } - An enumeration of the supported hash types
  that can be generated using [`enif_hash`](erl_nif.md#enif_hash).

  - **`ERL_NIF_INTERNAL_HASH`** - Non-portable hash function that only
    guarantees the same hash for the same term within one Erlang VM instance.

    It takes 32-bit salt values and generates hashes within `0..2^32-1`.

  - **`ERL_NIF_PHASH2`** - Portable hash function that gives the same hash for
    the same Erlang term regardless of machine architecture and ERTS version.

    _It ignores salt values_ and generates hashes within `0..2^27-1`.

    Slower than `ERL_NIF_INTERNAL_HASH.` It corresponds to `erlang:phash2/1`.

- **`SysIOVec`**{: #SysIOVec } - A system I/O vector, as used by `writev` on
  Unix and `WSASend` on Win32. It is used in `ErlNifIOVec` and by
  [`enif_ioq_peek`](erl_nif.md#enif_ioq_peek).

- **`ErlNifIOVec`**{: #ErlNifIOVec }

  ```c
  typedef struct {
    int iovcnt;
    size_t size;
    SysIOVec* iov;
  } ErlNifIOVec;
  ```

  An I/O vector containing `iovcnt` `SysIOVec`s pointing to the data. It is used
  by [`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec) and
  [`enif_ioq_enqv`](erl_nif.md#enif_ioq_enqv).

- **`ErlNifIOQueueOpts`**{: #ErlNifIOQueueOpts } - Options to configure a
  `ErlNifIOQueue`.
  - **ERL_NIF_IOQ_NORMAL** - Create a normal I/O Queue

## enif_alloc()

```c
void * enif_alloc(
        size_t size);
```

Allocates memory of `size` bytes.

Returns `NULL` if the allocation fails.

The returned pointer is suitably aligned for any built-in type that fit in the
allocated memory.

## enif_alloc_binary()

```c
int enif_alloc_binary(
        size_t size,
        ErlNifBinary* bin);
```

Allocates a new binary of size `size` bytes. Initializes the structure pointed
to by `bin` to refer to the allocated binary. The binary must either be released
by [`enif_release_binary`](erl_nif.md#enif_release_binary) or ownership
transferred to an Erlang term with
[`enif_make_binary`](erl_nif.md#enif_make_binary). An allocated (and owned)
`ErlNifBinary` can be kept between NIF calls.

If you do not need to reallocate or keep the data alive across NIF calls,
consider using [`enif_make_new_binary`](erl_nif.md#enif_make_new_binary) instead
as it will allocate small binaries on the process heap when possible.

Returns `true` on success, or `false` if allocation fails.

## enif_alloc_env()

```c
ErlNifEnv * enif_alloc_env();
```

Allocates a new [process independent environment](erl_nif.md#proc_indep_env).
The environment can be used to hold terms that are not bound to any process.
Such terms can later be copied to a process environment with
[`enif_make_copy`](erl_nif.md#enif_make_copy) or be sent to a process as a
message with [`enif_send`](erl_nif.md#enif_send).

Returns pointer to the new environment.

Available since OTP R14B

## enif_alloc_resource()

```c
void * enif_alloc_resource(
        ErlNifResourceType* type,
        unsigned size);
```

Allocates a memory-managed resource object of type `type` and size `size` bytes.

Available since OTP R13B04

## enif_binary_to_term()

```c
size_t enif_binary_to_term(
        ErlNifEnv *env,
        const unsigned char* data,
        size_t size,
        ERL_NIF_TERM *term,
        unsigned int opts);
```

Creates a term that is the result of decoding the binary data at `data`, which
must be encoded according to the Erlang external term format. No more than
`size` bytes are read from `data`. Argument `opts` corresponds to the second
argument to `erlang:binary_to_term/2` and must be either `0` or
`ERL_NIF_BIN2TERM_SAFE`.

On success, stores the resulting term at `*term` and returns the number of bytes
read. Returns `0` if decoding fails or if `opts` is invalid.

See also [`ErlNifBinaryToTerm`](erl_nif.md#ErlNifBinaryToTerm),
`erlang:binary_to_term/2`, and
[`enif_term_to_binary`](erl_nif.md#enif_term_to_binary).

Available since OTP 19.0

## enif_clear_env()

```c
void enif_clear_env(ErlNifEnv* env);
```

Frees all terms in an environment and clears it for reuse. The environment must
have been allocated with [`enif_alloc_env`](erl_nif.md#enif_alloc_env).

Available since OTP R14B

## enif_compare()

```c
int enif_compare(
        ERL_NIF_TERM lhs,
        ERL_NIF_TERM rhs);
```

Returns an integer < `0` if `lhs` < `rhs`, `0` if `lhs` = `rhs`, and > `0` if
`lhs` > `rhs`. Corresponds to the Erlang operators `==`, `/=`, `=<`, `<`, `>=`,
and `>` (but _not_ `=:=` or `=/=`).

Available since OTP R13B04

## enif_compare_monitors()

```c
int enif_compare_monitors(
        const ErlNifMonitor *monitor1,
        const ErlNifMonitor *monitor2);
```

Compares two [`ErlNifMonitor`](erl_nif.md#ErlNifMonitor)s. Can also be used to
imply some artificial order on monitors, for whatever reason.

Returns `0` if `monitor1` and `monitor2` are equal, < `0` if `monitor1` <
`monitor2`, and > `0` if `monitor1` > `monitor2`.

Available since OTP 20.0

## enif_compare_pids()

```c
int enif_compare_pids(
        const ErlNifPid *pid1,
        const ErlNifPid *pid2);
```

Compares two [`ErlNifPid` ](erl_nif.md#ErlNifPid)s according to term order.

Returns `0` if `pid1` and `pid2` are equal, < `0` if `pid1` < `pid2`, and > `0`
if `pid1` > `pid2`.

Available since OTP 22.0

## enif_cond_broadcast()

```c
void enif_cond_broadcast(
        ErlNifCond *cnd);
```

Same as [`erl_drv_cond_broadcast`](erl_driver.md#erl_drv_cond_broadcast).

Available since OTP R13B04

## enif_cond_create()

```c
ErlNifCond * enif_cond_create(
        char *name);
```

Same as [`erl_drv_cond_create`](erl_driver.md#erl_drv_cond_create).

Available since OTP R13B04

## enif_cond_destroy()

```c
void enif_cond_destroy(
        ErlNifCond *cnd);
```

Same as [`erl_drv_cond_destroy`](erl_driver.md#erl_drv_cond_destroy).

Available since OTP R13B04

## enif_cond_name()

```c
char* enif_cond_name(
        ErlNifCond* cnd);
```

Same as [`erl_drv_cond_name`](erl_driver.md#erl_drv_cond_name).

Available since OTP 21.0

## enif_cond_signal()

```c
void enif_cond_signal(
        ErlNifCond *cnd);
```

Same as [`erl_drv_cond_signal`](erl_driver.md#erl_drv_cond_signal).

Available since OTP R13B04

## enif_cond_wait()

```c
void enif_cond_wait(
        ErlNifCond *cnd,
        ErlNifMutex *mtx);
```

Same as [`erl_drv_cond_wait`](erl_driver.md#erl_drv_cond_wait).

Available since OTP R13B04

## enif_consume_timeslice()

```c
int enif_consume_timeslice(
        ErlNifEnv *env,
        int percent);
```

Gives the runtime system a hint about how much CPU time the current NIF call has
consumed since the last hint, or since the start of the NIF if no previous hint
has been specified. The time is specified as a percent of the timeslice that a
process is allowed to execute Erlang code until it can be suspended to give time
for other runnable processes. The scheduling timeslice is not an exact entity,
but can usually be approximated to about 1 millisecond.

Notice that it is up to the runtime system to determine if and how to use this
information. Implementations on some platforms can use other means to determine
consumed CPU time. Lengthy NIFs should regardless of this frequently call
`enif_consume_timeslice` to determine if it is allowed to continue execution.

Argument `percent` must be an integer between 1 and 100. This function must only
be called from a NIF-calling thread, and argument `env` must be the environment
of the calling process.

Returns `1` if the timeslice is exhausted, otherwise `0`. If `1` is returned,
the NIF is to return as soon as possible in order for the process to yield.

This function is provided to better support co-operative scheduling, improve
system responsiveness, and make it easier to prevent misbehaviors of the VM
because of a NIF monopolizing a scheduler thread. It can be used to divide
[length work](erl_nif.md#lengthy_work) into a number of repeated NIF calls
without the need to create threads.

See also the [warning](erl_nif.md#WARNING) text at the beginning of this manual
page.

Available since OTP R16B

## enif_convert_time_unit()

```c
ErlNifTime enif_convert_time_unit(
        ErlNifTime val,
        ErlNifTimeUnit from,
        ErlNifTimeUnit to);
```

Converts the `val` value of time unit `from` to the corresponding value of time
unit `to`. The result is rounded using the floor function.

- **`val`** - Value to convert time unit for.

- **`from`** - Time unit of `val`.

- **`to`** - Time unit of returned value.

Returns `ERL_NIF_TIME_ERROR` if called with an invalid time unit argument.

See also [`ErlNifTime`](erl_nif.md#ErlNifTime) and
[`ErlNifTimeUnit`](erl_nif.md#ErlNifTimeUnit).

Available since OTP 18.3

## enif_cpu_time()

```c
ERL_NIF_TERM enif_cpu_time(
        ErlNifEnv *env);
```

Returns the CPU time in the same format as
[`erlang:timestamp()`](`erlang:timestamp/0`). The CPU time is the time the
current logical CPU has spent executing since some arbitrary point in the past.
If the OS does not support fetching this value, `enif_cpu_time` invokes
[`enif_make_badarg`](erl_nif.md#enif_make_badarg).

Available since OTP 19.0

## enif_demonitor_process()

```c
int enif_demonitor_process(
        ErlNifEnv* caller_env,
        void* obj,
        const ErlNifMonitor* mon);
```

Cancels a monitor created earlier with
[`enif_monitor_process`](erl_nif.md#enif_monitor_process). Argument `obj` is a
pointer to the resource holding the monitor and `*mon` identifies the monitor.

Argument `caller_env` is the environment of the calling thread
([process bound](erl_nif.md#proc_bound_env) or
[callback](erl_nif.md#callback_env) environment) or `NULL` if calling from a
custom thread not spawned by ERTS.

Returns `0` if the monitor was successfully identified and removed. Returns a
non-zero value if the monitor could not be identified, which means it was either

- never created for this resource
- already cancelled
- already triggered
- just about to be triggered by a concurrent thread

This function is thread-safe.

Available since OTP 20.0

## enif_dynamic_resource_call()

```c
int enif_dynamic_resource_call(
        ErlNifEnv* caller_env,
        ERL_NIF_TERM rt_module,
        ERL_NIF_TERM rt_name,
        ERL_NIF_TERM resource,
        void* call_data);
```

Call code of a resource type implemented by another NIF module. The atoms
`rt_module` and `rt_name` identifies the resource type to be called. Argument
`resource` identifies a resource object of that type.

The callback [`dyncall`](erl_nif.md#ErlNifResourceDynCall) of the identified
resource type will be called with a pointer to the resource objects `obj` and
the argument `call_data` passed through. The `call_data` argument is typically a
pointer to a struct used to passed both arguments to the `dyncall` function as
well as results back to the caller.

Returns 0 if the `dyncall` callback function was called. Returns a non-zero
value if no call was made, which happens if `rt_module` and `rt_name` did not
identify a resource type with a `dyncall` callback or if `resource` was not a
resource object of that type.

Available since OTP 24.0

## enif_equal_tids()

```c
int enif_equal_tids(
        ErlNifTid tid1,
        ErlNifTid tid2);
```

Same as [`erl_drv_equal_tids`](erl_driver.md#erl_drv_equal_tids).

Available since OTP R13B04

## enif_fprintf()

```c
int enif_fprintf(
        FILE *stream,
        const char *format,
        ...);
```

Similar to `fprintf` but this format string also accepts `"%T"`, which formats
Erlang terms of type [`ERL_NIF_TERM`](erl_nif.md#ERL_NIF_TERM).

This function is primarily intended for debugging purpose. It is not recommended
to print very large terms with `%T`. The function may change `errno`, even if
successful.

Available since OTP 21.0

## enif_free()

```c
void enif_free(
        void* ptr);
```

Frees memory allocated by [`enif_alloc`](erl_nif.md#enif_alloc).

## enif_free_env()

```c
void enif_free_env(
        ErlNifEnv* env);
```

Frees an environment allocated with
[`enif_alloc_env`](erl_nif.md#enif_alloc_env). All terms created in the
environment are freed as well.

Available since OTP R14B

## enif_free_iovec()

```c
void enif_free_iovec(
        ErlNifIOVec* iov);
```

Frees an io vector returned from
[`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec). This is needed only if a
`NULL` environment is passed to
[`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec).

```c
ErlNifIOVec *iovec = NULL;
size_t max_elements = 128;
ERL_NIF_TERM tail;
if (!enif_inspect_iovec(NULL, max_elements, term, &tail, &iovec))
  return 0;

// Do things with the iovec

/* Free the iovector, possibly in another thread or nif function call */
enif_free_iovec(iovec);
```

Available since OTP 20.1

## enif_get_atom()

```c
int enif_get_atom(
        ErlNifEnv *env,
        ERL_NIF_TERM term,
        char *buf,
        unsigned size,
        ErlNifCharEncoding encoding);
```

Writes a `NULL`\-terminated string in the buffer pointed to by `buf` of size
`size` bytes, consisting of the string representation of the atom `term` with
[encoding](erl_nif.md#ErlNifCharEncoding).

Returns the number of bytes written (including terminating `NULL` character) or
`0` if `term` is not an atom with maximum length of `size-1` bytes in
`encoding`.

Available since OTP R13B04

## enif_get_atom_length()

```c
int enif_get_atom_length(
        ErlNifEnv *env,
        ERL_NIF_TERM term,
        unsigned *len,
        ErlNifCharEncoding encoding);
```

Sets `*len` to the length (number of bytes excluding terminating `NULL`
character) of the atom `term` with [encoding](erl_nif.md#ErlNifCharEncoding).

Returns `true` on success, or `false` if `term` is not an atom or if the atom
cannot be encoded using `encoding`.

Available since OTP R14B

## enif_get_double()

```c
int enif_get_double(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        double* dp);
```

Sets `*dp` to the floating-point value of `term`.

Returns `true` on success, or `false` if `term` is not a float.

Available since OTP R13B04

## enif_get_int()

```c
int enif_get_int(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        int* ip);
```

Sets `*ip` to the integer value of `term`.

Returns `true` on success, or `false` if `term` is not an integer or is outside
the bounds of type `int`.

## enif_get_int64()

```c
int enif_get_int64(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        ErlNifSInt64* ip);
```

Sets `*ip` to the integer value of `term`.

Returns `true` on success, or `false` if `term` is not an integer or is outside
the bounds of a signed 64-bit integer.

Available since OTP R14B

## enif_get_local_pid()

```c
int enif_get_local_pid(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        ErlNifPid* pid);
```

If `term` is the pid of a node local process, this function initializes the pid
variable `*pid` from it and returns `true`. Otherwise returns `false`. No check
is done to see if the process is alive.

> #### Note {: .info }
>
> `enif_get_local_pid` will return false if argument `term` is the atom
> [`undefined`](erl_nif.md#enif_make_pid).

Available since OTP R14B

## enif_get_local_port()

```c
int enif_get_local_port(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        ErlNifPort* port_id);
```

If `term` identifies a node local port, this function initializes the port
variable `*port_id` from it and returns `true`. Otherwise returns `false`. No
check is done to see if the port is alive.

Available since OTP 19.0

## enif_get_list_cell()

```c
int enif_get_list_cell(
        ErlNifEnv* env,
        ERL_NIF_TERM list,
        ERL_NIF_TERM* head,
        ERL_NIF_TERM* tail);
```

Sets `*head` and `*tail` from list `list`.

Returns `true` on success, or `false` if it is not a list or the list is empty.

## enif_get_list_length()

```c
int enif_get_list_length(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        unsigned* len);
```

Sets `*len` to the length of list `term`.

Returns `true` on success, or `false` if `term` is not a proper list.

Available since OTP R14B

## enif_get_long()

```c
int enif_get_long(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        long int* ip);
```

Sets `*ip` to the long integer value of `term`.

Returns `true` on success, or `false` if `term` is not an integer or is outside
the bounds of type `long int`.

Available since OTP R13B04

## enif_get_map_size()

```c
int enif_get_map_size(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        size_t *size);
```

Sets `*size` to the number of key-value pairs in the map `term`.

Returns `true` on success, or `false` if `term` is not a map.

Available since OTP 18.0

## enif_get_map_value()

```c
int enif_get_map_value(
        ErlNifEnv* env,
        ERL_NIF_TERM map,
        ERL_NIF_TERM key,
        ERL_NIF_TERM* value);
```

Sets `*value` to the value associated with `key` in the map `map`.

Returns `true` on success, or `false` if `map` is not a map or if `map` does not
contain `key`.

Available since OTP 18.0

## enif_get_resource()

```c
int enif_get_resource(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        ErlNifResourceType* type,
        void** objp);
```

Sets `*objp` to point to the resource object referred to by `term`.

Returns `true` on success, or `false` if `term` is not a handle to a resource
object of type `type`.

`enif_get_resource` does not add a reference to the resource object. However,
the pointer received in `*objp` is guaranteed to be valid at least as long as
the resource handle `term` is valid.

Available since OTP R13B04

## enif_get_string()

```c
int enif_get_string(
        ErlNifEnv* env,
        ERL_NIF_TERM list,
        char* buf,
        unsigned size,
        ErlNifCharEncoding encoding);
```

Writes a `NULL`\-terminated string in the buffer pointed to by `buf` with size
`size`, consisting of the characters in the string `list`. The characters are
written using [encoding](erl_nif.md#ErlNifCharEncoding).

Returns one of the following:

- The number of bytes written (including terminating `NULL` character)
- `-size` if the string was truncated because of buffer space
- `0` if `list` is not a string that can be encoded with `encoding` or if `size`
  was < `1`.

The written string is always `NULL`\-terminated, unless buffer `size` is < `1`.

Available since OTP R13B04

## enif_get_string_length()

```c
int enif_get_string_length(
        ErlNifEnv *env,
        ERL_NIF_TERM list,
        unsigned *len,
        ErlNifCharEncoding encoding);
```

Sets `*len` to the length (number of bytes excluding terminating `NULL`
character) of the string `list` with [encoding](erl_nif.md#ErlNifCharEncoding).

Returns `true` on success, or `false` if `list` is not a string that can be
encoded with `encoding`.

Available since OTP 26.0

## enif_get_tuple()

```c
int enif_get_tuple(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        int* arity,
        const ERL_NIF_TERM** array);
```

If `term` is a tuple, this function sets `*array` to point to an array
containing the elements of the tuple, and sets `*arity` to the number of
elements. Notice that the array is read-only and `(*array)[N-1]` is the Nth
element of the tuple. `*array` is undefined if the arity of the tuple is zero.

Returns `true` on success, or `false` if `term` is not a tuple.

Available since OTP R13B04

## enif_get_uint()

```c
int enif_get_uint(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        unsigned int* ip);
```

Sets `*ip` to the unsigned integer value of `term`.

Returns `true` on success, or `false` if `term` is not an unsigned integer or is
outside the bounds of type `unsigned int`.

Available since OTP R13B04

## enif_get_uint64()

```c
int enif_get_uint64(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        ErlNifUInt64* ip);
```

Sets `*ip` to the unsigned integer value of `term`.

Returns `true` on success, or `false` if `term` is not an unsigned integer or is
outside the bounds of an unsigned 64-bit integer.

Available since OTP R14B

## enif_get_ulong()

```c
int enif_get_ulong(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        unsigned long* ip);
```

Sets `*ip` to the unsigned long integer value of `term`.

Returns `true` on success, or `false` if `term` is not an unsigned integer or is
outside the bounds of type `unsigned long`.

## enif_getenv()

```c
int enif_getenv(
        const char* key,
        char* value,
        size_t *value_size);
```

Same as [`erl_drv_getenv`](erl_driver.md#erl_drv_getenv).

Available since OTP 18.2

## enif_has_pending_exception()

```c
int enif_has_pending_exception(
        ErlNifEnv* env,
        ERL_NIF_TERM* reason);
```

Returns `true` if a pending exception is associated with the environment `env`.
If `reason` is a `NULL` pointer, ignore it. Otherwise, if a pending exception
associated with `env` exists, set `*reason` to the value of the exception term.
For example, if [`enif_make_badarg`](erl_nif.md#enif_make_badarg) is called to
set a pending `badarg` exception, a later call to
`enif_has_pending_exception(env, &reason)` sets `*reason` to the atom `badarg`,
then return `true`.

See also [`enif_make_badarg`](erl_nif.md#enif_make_badarg) and
[`enif_raise_exception`](erl_nif.md#enif_raise_exception).

Available since OTP 18.0

## enif_hash()

```c
ErlNifUInt64 enif_hash(
        ErlNifHash type,
        ERL_NIF_TERM term,
        ErlNifUInt64 salt);
```

Hashes `term` according to the specified [`ErlNifHash`](erl_nif.md#ErlNifHash)
`type`.

Ranges of taken salt (if any) and returned value depend on the hash type.

Available since OTP 20.0

## enif_inspect_binary()

```c
int enif_inspect_binary(
        ErlNifEnv* env,
        ERL_NIF_TERM bin_term,
        ErlNifBinary* bin);
```

Initializes the structure pointed to by `bin` with information about binary term
`bin_term`.

Returns `true` on success, or `false` if `bin_term` is not a binary.

## enif_inspect_iolist_as_binary()

```c
int enif_inspect_iolist_as_binary(
        ErlNifEnv* env,
        ERL_NIF_TERM term,
        ErlNifBinary* bin);
```

Initializes the structure pointed to by `bin` with a continuous buffer with the
same byte content as `iolist`. As with `inspect_binary`, the data pointed to by
`bin` is transient and does not need to be released.

Returns `true` on success, or `false` if `iolist` is not an iolist.

Available since OTP R13B04

## enif_inspect_iovec()

```c
int enif_inspect_iovec(
        ErlNifEnv* env,
        size_t max_elements,
        ERL_NIF_TERM iovec_term,
        ERL_NIF_TERM* tail,
        ErlNifIOVec** iovec);
```

Fills `iovec` with the list of binaries provided in `iovec_term`. The number of
elements handled in the call is limited to `max_elements`, and `tail` is set to
the remainder of the list. Note that the output may be longer than
`max_elements` on some platforms.

To create a list of binaries from an arbitrary iolist, use
`erlang:iolist_to_iovec/1`.

When calling this function, `iovec` should contain a pointer to `NULL` or a
ErlNifIOVec structure that should be used if possible. e.g.

```c
/* Don't use a pre-allocated structure */
ErlNifIOVec *iovec = NULL;
enif_inspect_iovec(env, max_elements, term, &tail, &iovec);

/* Use a stack-allocated vector as an optimization for vectors with few elements */
ErlNifIOVec vec, *iovec = &vec;
enif_inspect_iovec(env, max_elements, term, &tail, &iovec);
```

The contents of the `iovec` is valid until the called nif function returns. If
the `iovec` should be valid after the nif call returns, it is possible to call
this function with a `NULL` environment. If no environment is given the `iovec`
owns the data in the vector and it has to be explicitly freed using
[`enif_free_iovec` ](erl_nif.md#enif_free_iovec).

Returns `true` on success, or `false` if `iovec_term` not an iovec.

Available since OTP 20.1

## enif_ioq_create()

```c
ErlNifIOQueue * enif_ioq_create(
        ErlNifIOQueueOpts opts);
```

Create a new I/O Queue that can be used to store data. `opts` has to be set to
`ERL_NIF_IOQ_NORMAL`.

Available since OTP 20.1

## enif_ioq_destroy()

```c
void enif_ioq_destroy(
        ErlNifIOQueue *q);
```

Destroy the I/O queue and free all of it's contents

Available since OTP 20.1

## enif_ioq_deq()

```c
int enif_ioq_deq(
        ErlNifIOQueue *q,
        size_t count,
        size_t *size);
```

Dequeue `count` bytes from the I/O queue. If `size` is not `NULL`, the new size
of the queue is placed there.

Returns `true` on success, or `false` if the I/O does not contain `count` bytes.
On failure the queue is left un-altered.

Available since OTP 20.1

## enif_ioq_enq_binary()

```c
int enif_ioq_enq_binary(
        ErlNifIOQueue *q,
        ErlNifBinary *bin,
        size_t skip);
```

Enqueue the `bin` into `q` skipping the first `skip` bytes.

Returns `true` on success, or `false` if `skip` is greater than the size of
`bin`. Any ownership of the binary data is transferred to the queue and `bin` is
to be considered read-only for the rest of the NIF call and then as released.

Available since OTP 20.1

## enif_ioq_enqv()

```c
int enif_ioq_enqv(
        ErlNifIOQueue *q,
        ErlNifIOVec *iovec,
        size_t skip);
```

Enqueue the `iovec` into `q` skipping the first `skip` bytes.

Returns `true` on success, or `false` if `skip` is greater than the size of
`iovec`.

Available since OTP 20.1

## enif_ioq_peek()

```c
SysIOVec * enif_ioq_peek(
        ErlNifIOQueue *q,
        int *iovlen);
```

Get the I/O queue as a pointer to an array of `SysIOVec`s. It also returns the
number of elements in `iovlen`.

Nothing is removed from the queue by this function, that must be done with
[`enif_ioq_deq`](erl_nif.md#enif_ioq_deq).

The returned array is suitable to use with the Unix system call `writev`.

Available since OTP 20.1

## enif_ioq_peek_head()

```c
int enif_ioq_peek_head(
        ErlNifEnv *env,
        ErlNifIOQueue *q,
        size_t *size,
        ERL_NIF_TERM *bin_term);
```

Get the head of the IO Queue as a binary term.

If `size` is not `NULL`, the size of the head is placed there.

Nothing is removed from the queue by this function, that must be done with
[`enif_ioq_deq`](erl_nif.md#enif_ioq_deq).

Returns `true` on success, or `false` if the queue is empty.

Available since OTP 21.0

## enif_ioq_size()

```c
size_t enif_ioq_size(
        ErlNifIOQueue *q);
```

Get the size of `q`.

Available since OTP 20.1

## enif_is_atom()

```c
int enif_is_atom(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is an atom.

Available since OTP R13B04

## enif_is_binary()

```c
int enif_is_binary(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a binary.

## enif_is_current_process_alive()

```c
int enif_is_current_process_alive(
        ErlNifEnv* env);
```

Returns `true` if the currently executing process is currently alive, otherwise
`false`.

This function can only be used from a NIF-calling thread, and with an
environment corresponding to currently executing processes.

Available since OTP 19.0

## enif_is_empty_list()

```c
int enif_is_empty_list(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is an empty list.

Available since OTP R13B04

## enif_is_exception()

```c
int enif_is_exception(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Return true if `term` is an exception.

Available since OTP R14B03

## enif_is_fun()

```c
int enif_is_fun(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a fun.

Available since OTP R13B04

## enif_is_identical()

```c
int enif_is_identical(
        ERL_NIF_TERM lhs,
        ERL_NIF_TERM rhs);
```

Returns `true` if the two terms are identical. Corresponds to the Erlang
operators `=:=` and `=/=`.

Available since OTP R13B04

## enif_is_list()

```c
int enif_is_list(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a list.

Available since OTP R14B

## enif_is_map()

```c
int enif_is_map(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a map, otherwise `false`.

Available since OTP 18.0

## enif_is_number()

```c
int enif_is_number(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a number.

Available since OTP R15B

## enif_is_pid()

```c
int enif_is_pid(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a pid.

Available since OTP R13B04

## enif_is_pid_undefined()

```c
int enif_is_pid_undefined(
        const ErlNifPid* pid);
```

Returns `true` if `pid` has been set as undefined by
[`enif_set_pid_undefined` ](erl_nif.md#enif_set_pid_undefined).

Available since OTP 22.0

## enif_is_port()

```c
int enif_is_port(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a port.

Available since OTP R13B04

## enif_is_port_alive()

```c
int enif_is_port_alive(
        ErlNifEnv* env,
        ErlNifPort *port_id);
```

Returns `true` if `port_id` is alive.

This function is thread-safe.

Available since OTP 19.0

## enif_is_process_alive()

```c
int enif_is_process_alive(
        ErlNifEnv* env,
        ErlNifPid *pid);
```

Returns `true` if `pid` is alive.

This function is thread-safe.

Available since OTP 19.0

## enif_is_ref()

```c
int enif_is_ref(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a reference.

Available since OTP R13B04

## enif_is_tuple()

```c
int enif_is_tuple(
        ErlNifEnv* env,
        ERL_NIF_TERM term);
```

Returns `true` if `term` is a tuple.

Available since OTP R14B

## enif_keep_resource()

```c
int enif_keep_resource(
        void* obj);
```

Adds a reference to resource object `obj` obtained from
[`enif_alloc_resource`](erl_nif.md#enif_alloc_resource). Each call to
`enif_keep_resource` for an object must be balanced by a call to
[`enif_release_resource`](erl_nif.md#enif_release_resource) before the object is
destructed.

Available since OTP R14B

## enif_make_atom()

```c
ERL_NIF_TERM enif_make_atom(
        ErlNifEnv *env,
        const char *name);
```

Creates an atom term from the `NULL`\-terminated C-string `name` with ISO
Latin-1 encoding. If the length of `name` exceeds the maximum length allowed for
an atom (255 characters), `enif_make_atom` invokes
[`enif_make_badarg`](erl_nif.md#enif_make_badarg).

## enif_make_atom_len()

```c
ERL_NIF_TERM enif_make_atom_len(
        ErlNifEnv *env,
        const char *name,
        size_t len);
```

Create an atom term from the string `name` with length `len` and ISO Latin-1
encoding. `NULL` characters are treated as any other characters. If `len`
exceeds the maximum length allowed for an atom (255 characters),
`enif_make_atom` invokes [`enif_make_badarg` ](erl_nif.md#enif_make_badarg).

Available since OTP R14B

## enif_make_badarg()

```c
ERL_NIF_TERM enif_make_badarg(
        ErlNifEnv* env);
```

Makes a `badarg` exception to be returned from a NIF, and associates it with
environment `env`. Once a NIF or any function it calls invokes
`enif_make_badarg`, the runtime ensures that a `badarg` exception is raised when
the NIF returns, even if the NIF attempts to return a non-exception term
instead.

The return value from `enif_make_badarg` can be used only as the return value
from the NIF that invoked it (directly or indirectly) or be passed to
[`enif_is_exception`](erl_nif.md#enif_is_exception), but not to any other NIF
API function.

See also [`enif_has_pending_exception`](erl_nif.md#enif_has_pending_exception)
and [`enif_raise_exception`](erl_nif.md#enif_raise_exception).

> #### Note {: .info }
>
> Before ERTS 7.0 (Erlang/OTP 18), the return value from `enif_make_badarg` had
> to be returned from the NIF. This requirement is now lifted as the return
> value from the NIF is ignored if `enif_make_badarg` has been invoked.

## enif_make_binary()

```c
ERL_NIF_TERM enif_make_binary(
        ErlNifEnv* env,
        ErlNifBinary* bin);
```

Makes a binary term from `bin`. Any ownership of the binary data is transferred
to the created term and `bin` is to be considered read-only for the rest of the
NIF call and then as released.

## enif_make_copy()

```c
ERL_NIF_TERM enif_make_copy(
        ErlNifEnv* dst_env,
        ERL_NIF_TERM src_term);
```

Makes a copy of term `src_term`. The copy is created in environment `dst_env`.
The source term can be located in any environment.

Available since OTP R14B

## enif_make_double()

```c
ERL_NIF_TERM enif_make_double(
        ErlNifEnv* env,
        double d);
```

Creates a floating-point term from a `double`. If argument `double` is not
finite or is NaN, `enif_make_double` invokes
[`enif_make_badarg`](erl_nif.md#enif_make_badarg).

Available since OTP R13B04

## enif_make_existing_atom()

```c
int enif_make_existing_atom(
        ErlNifEnv *env,
        const char *name,
        ERL_NIF_TERM *atom,
        ErlNifCharEncoding encoding);
```

Tries to create the term of an already existing atom from the `NULL`\-terminated
C-string `name` with [encoding](erl_nif.md#ErlNifCharEncoding).

If the atom already exists, this function stores the term in `*atom` and returns
`true`, otherwise returns `false`. It also returns `false` if the string `name`
exceeds the maximum length allowed for an atom (255 characters) or if `name` is
not correctly encoded.

Available since OTP R13B04

## enif_make_existing_atom_len()

```c
int enif_make_existing_atom_len(
        ErlNifEnv *env,
        const char *name,
        size_t len,
        ERL_NIF_TERM *atom,
        ErlNifCharEncoding encoding);
```

Tries to create the term of an already existing atom from the string `name` with
length `len` bytes and [encoding](erl_nif.md#ErlNifCharEncoding). `NULL`
characters are treated as any other characters.

If the atom already exists, this function stores the term in `*atom` and returns
`true`, otherwise returns `false`. It also returns `false` if the string `name`
exceeds the maximum length allowed for an atom (255 characters) or if `name` is
not correctly encoded.

Available since OTP R14B

## enif_make_int()

```c
ERL_NIF_TERM enif_make_int(
        ErlNifEnv* env,
        int i);
```

Creates an integer term.

## enif_make_int64()

```c
ERL_NIF_TERM enif_make_int64(
        ErlNifEnv* env,
        ErlNifSInt64 i);
```

Creates an integer term from a signed 64-bit integer.

Available since OTP R14B

## enif_make_list()

```c
ERL_NIF_TERM enif_make_list(
        ErlNifEnv* env,
        unsigned cnt,
        ...);
```

Creates an ordinary list term of length `cnt`. Expects `cnt` number of arguments
(after `cnt`) of type `ERL_NIF_TERM` as the elements of the list.

Returns an empty list if `cnt` is 0.

## enif_make_list1()

## enif_make_list2()

## enif_make_list3()

## enif_make_list4()

## enif_make_list5()

## enif_make_list6()

## enif_make_list7()

## enif_make_list8()

## enif_make_list9()

```c
ERL_NIF_TERM enif_make_list1(
        ErlNifEnv* env,
        ERL_NIF_TERM e1);
```

```c
ERL_NIF_TERM enif_make_list2(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ERL_NIF_TERM e2);
```

```c
ERL_NIF_TERM enif_make_list3(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ERL_NIF_TERM e2, ERL_NIF_TERM e3);
```

```c
ERL_NIF_TERM enif_make_list4(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e4);
```

```c
ERL_NIF_TERM enif_make_list5(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e5);
```

```c
ERL_NIF_TERM enif_make_list6(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e6);
```

```c
ERL_NIF_TERM enif_make_list7(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e7);
```

```c
ERL_NIF_TERM enif_make_list8(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e8);
```

```c
ERL_NIF_TERM enif_make_list9(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e9);
```

Creates an ordinary list term with length indicated by the function name. Prefer
these functions (macros) over the variadic `enif_make_list` to get a
compile-time error if the number of arguments does not match.

Available since OTP R13B04

## enif_make_list_cell()

```c
ERL_NIF_TERM enif_make_list_cell(
        ErlNifEnv* env,
        ERL_NIF_TERM head,
        ERL_NIF_TERM tail);
```

Creates a list cell `[head | tail]`.

## enif_make_list_from_array()

```c
ERL_NIF_TERM enif_make_list_from_array(
        ErlNifEnv* env,
        const ERL_NIF_TERM arr[],
        unsigned cnt);
```

Creates an ordinary list containing the elements of array `arr` of length `cnt`.

Returns an empty list if `cnt` is 0.

Available since OTP R13B04

## enif_make_long()

```c
ERL_NIF_TERM enif_make_long(
        ErlNifEnv* env,
        long int i);
```

Creates an integer term from a `long int`.

Available since OTP R13B04

## enif_make_map_put()

```c
int enif_make_map_put(
        ErlNifEnv* env,
        ERL_NIF_TERM map_in,
        ERL_NIF_TERM key,
        ERL_NIF_TERM value,
        ERL_NIF_TERM* map_out);
```

Makes a copy of map `map_in` and inserts `key` with `value`. If `key` already
exists in `map_in`, the old associated value is replaced by `value`.

If successful, this function sets `*map_out` to the new map and returns `true`.
Returns `false` if `map_in` is not a map.

The `map_in` term must belong to environment `env`.

Available since OTP 18.0

## enif_make_map_remove()

```c
int enif_make_map_remove(
        ErlNifEnv* env,
        ERL_NIF_TERM map_in,
        ERL_NIF_TERM key,
        ERL_NIF_TERM* map_out);
```

If map `map_in` contains `key`, this function makes a copy of `map_in` in
`*map_out`, and removes `key` and the associated value. If map `map_in` does not
contain `key`, `*map_out` is set to `map_in`.

Returns `true` on success, or `false` if `map_in` is not a map.

The `map_in` term must belong to environment `env`.

Available since OTP 18.0

## enif_make_map_update()

```c
int enif_make_map_update(
        ErlNifEnv* env,
        ERL_NIF_TERM map_in,
        ERL_NIF_TERM key,
        ERL_NIF_TERM new_value,
        ERL_NIF_TERM* map_out);
```

Makes a copy of map `map_in` and replace the old associated value for `key` with
`new_value`.

If successful, this function sets `*map_out` to the new map and returns `true`.
Returns `false` if `map_in` is not a map or if it does not contain `key`.

The `map_in` term must belong to environment `env`.

Available since OTP 18.0

## enif_make_map_from_arrays()

```c
int enif_make_map_from_arrays(
        ErlNifEnv* env,
        ERL_NIF_TERM keys[],
        ERL_NIF_TERM values[],
        size_t cnt,
        ERL_NIF_TERM *map_out);
```

Makes a map term from the given keys and values.

If successful, this function sets `*map_out` to the new map and returns `true`.
Returns `false` there are any duplicate keys.

All keys and values must belong to `env`.

Available since OTP 21.0

## enif_make_monitor_term()

```c
ERL_NIF_TERM enif_make_monitor_term(
        ErlNifEnv* env,
        const ErlNifMonitor* mon);
```

Creates a term identifying the given monitor received from
[`enif_monitor_process` ](erl_nif.md#enif_monitor_process).

This function is primarily intended for debugging purpose.

Available since OTP 22.0

## enif_make_new_atom()

```c
int enif_make_new_atom(
        ErlNifEnv *env,
        const char *name,
        ERL_NIF_TERM *atom,
        ErlNifCharEncoding encoding);
```

Creates an atom term from the `NULL`\-terminated C-string `name` with
[encoding](erl_nif.md#ErlNifCharEncoding).

If successful, `true` is returned and the atom term is stored in `*atom`.

Otherwise, `false` is returned if the length of `name` exceeds the maximum
length allowed for an atom (255 characters) or if `name` is not correctly
encoded.

Available since OTP 26.0

## enif_make_new_atom_len()

```c
int enif_make_new_atom_len(
        ErlNifEnv *env,
        const char *name,
        size_t len,
        ERL_NIF_TERM *atom,
        ErlNifCharEncoding encoding);
```

Create an atom term from string `name` with length `len` bytes and
[encoding](erl_nif.md#ErlNifCharEncoding).

If successful, `true` is returned and atom term is stored in `*atom`.

Otherwise, `false` is returned if the string exceeds the maximum length allowed
for an atom (255 characters) or if the string is not correctly encoded.

Available since OTP 26.0

## enif_make_new_binary()

```c
unsigned char * enif_make_new_binary(
	 ErlNifEnv* env,
	 size_t size,
	 ERL_NIF_TERM* termp);
```

Allocates a binary of size `size` bytes and creates an owning term. The binary
data is mutable until the calling NIF returns. This is a quick way to create a
new binary without having to use [`ErlNifBinary`](erl_nif.md#ErlNifBinary). The
drawbacks are that the binary cannot be kept between NIF calls and it cannot be
reallocated.

Returns a pointer to the raw binary data and sets `*termp` to the binary term.

Available since OTP R14B

## enif_make_new_map()

```c
ERL_NIF_TERM enif_make_new_map(
        ErlNifEnv* env);
```

Makes an empty map term.

Available since OTP 18.0

## enif_make_pid()

```c
ERL_NIF_TERM enif_make_pid(
        ErlNifEnv* env,
        const ErlNifPid* pid);
```

Makes a pid term or the atom [`undefined`](erl_nif.md#enif_set_pid_undefined)
from `*pid`.

Available since OTP R14B

## enif_make_ref()

```c
ERL_NIF_TERM enif_make_ref(
        ErlNifEnv* env);
```

Creates a reference like `erlang:make_ref/0`.

Available since OTP R13B04

## enif_make_resource()

```c
ERL_NIF_TERM enif_make_resource(
        ErlNifEnv* env,
        void* obj);
```

Creates an opaque handle to a memory-managed resource object obtained by
[`enif_alloc_resource`](erl_nif.md#enif_alloc_resource). No ownership transfer
is done, as the resource object still needs to be released by
[`enif_release_resource`](erl_nif.md#enif_release_resource). However, notice
that the call to `enif_release_resource` can occur immediately after obtaining
the term from `enif_make_resource`, in which case the resource object is
deallocated when the term is garbage collected. For more details, see the
[example of creating and returning a resource object](erl_nif.md#enif_resource_example)
in the User's Guide.

> #### Note {: .info }
>
> Since ERTS 9.0 (OTP-20.0), resource terms have a defined behavior when
> compared and serialized through `term_to_binary` or passed between nodes.
>
> - Two resource terms will compare equal if and only if they would yield the
>   same resource object pointer when passed to
>   [`enif_get_resource`](erl_nif.md#enif_get_resource).
> - A resource term can be serialized with `term_to_binary` and later be fully
>   recreated if the resource object is still alive when `binary_to_term` is
>   called. A _stale_ resource term will be returned from `binary_to_term` if
>   the resource object has been deallocated.
>   [`enif_get_resource`](erl_nif.md#enif_get_resource) will return false for
>   stale resource terms.
>
>   The same principles of serialization apply when passing resource terms in
>   messages to remote nodes and back again. A resource term will act stale on
>   all nodes except the node where its resource object is still alive in
>   memory.
>
> Before ERTS 9.0 (OTP-20.0), all resource terms did compare equal to each other
> and to empty binaries (`<<>>`). If serialized, they would be recreated as
> plain empty binaries.

Available since OTP R13B04

## enif_make_resource_binary()

```c
ERL_NIF_TERM enif_make_resource_binary(
        ErlNifEnv* env,
        void* obj,
        const void* data,
        size_t size);
```

Creates a binary term that is memory-managed by a resource object `obj` obtained
by [`enif_alloc_resource`](erl_nif.md#enif_alloc_resource). The returned binary
term consists of `size` bytes pointed to by `data`. This raw binary data must be
kept readable and unchanged until the destructor of the resource is called. The
binary data can be stored external to the resource object, in which case the
destructor is responsible for releasing the data.

Several binary terms can be managed by the same resource object. The destructor
is not called until the last binary is garbage collected. This can be useful to
return different parts of a larger binary buffer.

As with [`enif_make_resource`](erl_nif.md#enif_make_resource), no ownership
transfer is done. The resource still needs to be released with
[`enif_release_resource`](erl_nif.md#enif_release_resource).

Available since OTP R14B

## enif_make_reverse_list()

```c
int enif_make_reverse_list(
        ErlNifEnv* env,
        ERL_NIF_TERM list_in,
        ERL_NIF_TERM *list_out);
```

Sets `*list_out` to the reverse list of the list `list_in` and returns `true`,
or returns `false` if `list_in` is not a list.

This function is only to be used on short lists, as a copy is created of the
list, which is not released until after the NIF returns.

The `list_in` term must belong to environment `env`.

Available since OTP R15B

## enif_make_string()

```c
ERL_NIF_TERM enif_make_string(
        ErlNifEnv *env,
        const char *string,
        ErlNifCharEncoding encoding);
```

Creates a list containing the characters of the `NULL`\-terminated string
`string` with [encoding](erl_nif.md#ErlNifCharEncoding).

## enif_make_string_len()

```c
ERL_NIF_TERM enif_make_string_len(
        ErlNifEnv *env,
        const char *string,
        size_t len,
        ErlNifCharEncoding encoding);
```

Creates a list containing the characters of the string `string` with length
`len` and [encoding](erl_nif.md#ErlNifCharEncoding). `NULL` characters are
treated as any other characters.

Available since OTP R14B

## enif_make_sub_binary()

```c
ERL_NIF_TERM enif_make_sub_binary(
        ErlNifEnv* env,
        ERL_NIF_TERM bin_term,
        size_t pos,
        size_t size);
```

Makes a subbinary of binary `bin_term`, starting at zero-based position `pos`
with a length of `size` bytes. `bin_term` must be a binary or bitstring.
`pos+size` must be less or equal to the number of whole bytes in `bin_term`.

Available since OTP R13B04

## enif_make_tuple()

```c
ERL_NIF_TERM enif_make_tuple(
        ErlNifEnv* env,
        unsigned cnt,
        ...);
```

Creates a tuple term of arity `cnt`. Expects `cnt` number of arguments (after
`cnt`) of type `ERL_NIF_TERM` as the elements of the tuple.

## enif_make_tuple1()

## enif_make_tuple2()

## enif_make_tuple3()

## enif_make_tuple4()

## enif_make_tuple5()

## enif_make_tuple6()

## enif_make_tuple7()

## enif_make_tuple8()

## enif_make_tuple9()

```c
ERL_NIF_TERM enif_make_tuple1(
        ErlNifEnv* env,
        ERL_NIF_TERM e1);
```

```c
ERL_NIF_TERM enif_make_tuple2(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ERL_NIF_TERM e2);
```

```c
ERL_NIF_TERM enif_make_tuple3(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ERL_NIF_TERM e2, ERL_NIF_TERM e3);
```

```c
ERL_NIF_TERM enif_make_tuple4(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e4);
```

```c
ERL_NIF_TERM enif_make_tuple5(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e5);
```

```c
ERL_NIF_TERM enif_make_tuple6(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e6);
```

```c
ERL_NIF_TERM enif_make_tuple7(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e7);
```

```c
ERL_NIF_TERM enif_make_tuple8(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e8);
```

```c
ERL_NIF_TERM enif_make_tuple9(
        ErlNifEnv* env,
        ERL_NIF_TERM e1, ..., ERL_NIF_TERM e9);
```

Creates a tuple term with length indicated by the function name. Prefer these
functions (macros) over the variadic `enif_make_tuple` to get a compile-time
error if the number of arguments does not match.

Available since OTP R13B04

## enif_make_tuple_from_array()

```c
ERL_NIF_TERM enif_make_tuple_from_array(
        ErlNifEnv* env,
        const ERL_NIF_TERM arr[],
        unsigned cnt);
```

Creates a tuple containing the elements of array `arr` of length `cnt`.

Available since OTP R13B04

## enif_make_uint()

```c
ERL_NIF_TERM enif_make_uint(
        ErlNifEnv* env,
        unsigned int i);
```

Creates an integer term from an `unsigned int`.

Available since OTP R13B04

## enif_make_uint64()

```c
ERL_NIF_TERM enif_make_uint64(
        ErlNifEnv* env,
        ErlNifUInt64 i);
```

Creates an integer term from an unsigned 64-bit integer.

Available since OTP R14B

## enif_make_ulong()

```c
ERL_NIF_TERM enif_make_ulong(
        ErlNifEnv* env,
        unsigned long i);
```

Creates an integer term from an `unsigned long int`.

## enif_make_unique_integer()

```c
ERL_NIF_TERM enif_make_unique_integer(
        ErlNifEnv *env,
        ErlNifUniqueInteger properties);
```

Returns a unique integer with the same properties as specified by
`erlang:unique_integer/1`.

`env` is the environment to create the integer in.

`ERL_NIF_UNIQUE_POSITIVE` and `ERL_NIF_UNIQUE_MONOTONIC` can be passed as the
second argument to change the properties of the integer returned. They can be
combined by OR:ing the two values together.

See also [`ErlNifUniqueInteger`](erl_nif.md#ErlNifUniqueInteger).

Available since OTP 19.0

## enif_map_iterator_create()

```c
int enif_map_iterator_create(
        ErlNifEnv *env,
        ERL_NIF_TERM map,
        ErlNifMapIterator *iter,
        ErlNifMapIteratorEntry entry);
```

Creates an iterator for the map `map` by initializing the structure pointed to
by `iter`. Argument `entry` determines the start position of the iterator:
`ERL_NIF_MAP_ITERATOR_FIRST` or `ERL_NIF_MAP_ITERATOR_LAST`.

Returns `true` on success, or false if `map` is not a map.

A map iterator is only useful during the lifetime of environment `env` that the
`map` belongs to. The iterator must be destroyed by calling
[`enif_map_iterator_destroy`](erl_nif.md#enif_map_iterator_destroy):

```c
ERL_NIF_TERM key, value;
ErlNifMapIterator iter;
enif_map_iterator_create(env, my_map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);

while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
    do_something(key,value);
    enif_map_iterator_next(env, &iter);
}
enif_map_iterator_destroy(env, &iter);
```

> #### Note {: .info }
>
> The key-value pairs of a map have no defined iteration order. The only
> guarantee is that the iteration order of a single map instance is preserved
> during the lifetime of the environment that the map belongs to.

Available since OTP 18.0

## enif_map_iterator_destroy()

```c
void enif_map_iterator_destroy(
        ErlNifEnv *env,
        ErlNifMapIterator *iter);
```

Destroys a map iterator created by
[`enif_map_iterator_create`](erl_nif.md#enif_map_iterator_create).

Available since OTP 18.0

## enif_map_iterator_get_pair()

```c
int enif_map_iterator_get_pair(
        ErlNifEnv *env,
        ErlNifMapIterator *iter,
        ERL_NIF_TERM *key,
        ERL_NIF_TERM *value);
```

Gets key and value terms at the current map iterator position.

On success, sets `*key` and `*value` and returns `true`. Returns `false` if the
iterator is positioned at head (before first entry) or tail (beyond last entry).

Available since OTP 18.0

## enif_map_iterator_is_head()

```c
int enif_map_iterator_is_head(
        ErlNifEnv *env,
        ErlNifMapIterator *iter);
```

Returns `true` if map iterator `iter` is positioned before the first entry.

Available since OTP 18.0

## enif_map_iterator_is_tail()

```c
int enif_map_iterator_is_tail(
        ErlNifEnv *env,
        ErlNifMapIterator *iter);
```

Returns `true` if map iterator `iter` is positioned after the last entry.

Available since OTP 18.0

## enif_map_iterator_next()

```c
int enif_map_iterator_next(
        ErlNifEnv *env,
        ErlNifMapIterator *iter);
```

Increments map iterator to point to the next key-value entry.

Returns `true` if the iterator is now positioned at a valid key-value entry, or
`false` if the iterator is positioned at the tail (beyond the last entry).

Available since OTP 18.0

## enif_map_iterator_prev()

```c
int enif_map_iterator_prev(
        ErlNifEnv *env,
        ErlNifMapIterator *iter);
```

Decrements map iterator to point to the previous key-value entry.

Returns `true` if the iterator is now positioned at a valid key-value entry, or
`false` if the iterator is positioned at the head (before the first entry).

Available since OTP 18.0

## enif_monitor_process()

```c
int enif_monitor_process(
        ErlNifEnv* caller_env,
        void* obj,
        const ErlNifPid* target_pid,
        ErlNifMonitor* mon);
```

Starts monitoring a process from a resource. When a process is monitored, a
process exit results in a call to the provided
[`down`](erl_nif.md#ErlNifResourceDown) callback associated with the resource
type.

Argument `obj` is pointer to the resource to hold the monitor and `*target_pid`
identifies the local process to be monitored.

If `mon` is not `NULL`, a successful call stores the identity of the monitor in
the [`ErlNifMonitor`](erl_nif.md#ErlNifMonitor) struct pointed to by `mon`. This
identifier is used to refer to the monitor for later removal with
[`enif_demonitor_process`](erl_nif.md#enif_demonitor_process) or compare with
[`enif_compare_monitors`](erl_nif.md#enif_compare_monitors). A monitor is
automatically removed when it triggers or when the resource is deallocated.

Argument `caller_env` is the environment of the calling thread
([process bound](erl_nif.md#proc_bound_env) or
[callback](erl_nif.md#callback_env) environment) or `NULL` if calling from a
custom thread not spawned by ERTS.

Returns `0` on success, < 0 if no `down` callback is provided, and > 0 if the
process is no longer alive or if `target_pid` is
[undefined](erl_nif.md#enif_set_pid_undefined).

This function is thread-safe.

Available since OTP 20.0

## enif_monotonic_time()

```c
ErlNifTime enif_monotonic_time(
        ErlNifTimeUnit time_unit);
```

Returns the current
[Erlang monotonic time](time_correction.md#erlang-monotonic-time). Notice that
it is not uncommon with negative values.

`time_unit` is the time unit of the returned value.

Returns `ERL_NIF_TIME_ERROR` if called with an invalid time unit argument, or if
called from a thread that is not a scheduler thread.

See also [`ErlNifTime`](erl_nif.md#ErlNifTime) and
[`ErlNifTimeUnit`](erl_nif.md#ErlNifTimeUnit).

Available since OTP 18.3

## enif_mutex_create()

```c
ErlNifMutex * enif_mutex_create(
        char *name);
```

Same as [`erl_drv_mutex_create`](erl_driver.md#erl_drv_mutex_create).

Available since OTP R13B04

## enif_mutex_destroy()

```c
void enif_mutex_destroy(
        ErlNifMutex *mtx);
```

Same as [`erl_drv_mutex_destroy`](erl_driver.md#erl_drv_mutex_destroy).

Available since OTP R13B04

## enif_mutex_lock()

```c
void enif_mutex_lock(
        ErlNifMutex *mtx);
```

Same as [`erl_drv_mutex_lock`](erl_driver.md#erl_drv_mutex_lock).

Available since OTP R13B04

## enif_mutex_name()

```c
char* enif_mutex_name(
        ErlNifMutex* mtx);
```

Same as [`erl_drv_mutex_name`](erl_driver.md#erl_drv_mutex_name).

Available since OTP 21.0

## enif_mutex_trylock()

```c
int enif_mutex_trylock(
        ErlNifMutex *mtx);
```

Same as [`erl_drv_mutex_trylock`](erl_driver.md#erl_drv_mutex_trylock).

Available since OTP R13B04

## enif_mutex_unlock()

```c
void enif_mutex_unlock(
        ErlNifMutex *mtx);
```

Same as [`erl_drv_mutex_unlock`](erl_driver.md#erl_drv_mutex_unlock).

Available since OTP R13B04

## enif_now_time()

```c
ERL_NIF_TERM enif_now_time(
        ErlNifEnv *env);
```

Returns an [`erlang:now()`](`erlang:now/0`) time stamp.

_This function is deprecated._

Available since OTP 19.0

## enif_open_resource_type()

```c
ErlNifResourceType * enif_open_resource_type(
        ErlNifEnv* env,
        const char* module_str,
        const char* name,
        ErlNifResourceDtor* dtor,
        ErlNifResourceFlags flags,
        ErlNifResourceFlags* tried);
```

Creates or takes over a resource type identified by the string `name` and gives
it the destructor function pointed to by
[`dtor`](erl_nif.md#ErlNifResourceDtor). Argument `flags` can have the following
values:

- **`ERL_NIF_RT_CREATE`** - Creates a new resource type that does not already
  exist.

- **`ERL_NIF_RT_TAKEOVER`** - Opens an existing resource type and takes over
  ownership of all its instances. The supplied destructor `dtor` is called both
  for existing instances and new instances not yet created by the calling NIF
  library.

The two flag values can be combined with bitwise OR. The resource type name is
local to the calling module. Argument `module_str` is not (yet) used and must be
`NULL`. `dtor` can be `NULL` if no destructor is needed.

On success, the function returns a pointer to the resource type and `*tried` is
set to either `ERL_NIF_RT_CREATE` or `ERL_NIF_RT_TAKEOVER` to indicate what was
done. On failure, returns `NULL` and sets `*tried` to `flags`. It is allowed to
set `tried` to `NULL`.

Notice that `enif_open_resource_type` is only allowed to be called in the two
callbacks [`load`](erl_nif.md#load) and [`upgrade`](erl_nif.md#upgrade). The
resource type is only created or taken over if the calling `load`/`upgrade`
function returns successfully.

See also [`enif_open_resource_type_x`](erl_nif.md#enif_open_resource_type_x).

Available since OTP R13B04

## enif_open_resource_type_x()

```c
ErlNifResourceType * enif_open_resource_type_x(
        ErlNifEnv* env,
        const char* name,
        const ErlNifResourceTypeInit* init,
        ErlNifResourceFlags flags,
        ErlNifResourceFlags* tried);
```

Same as [`enif_open_resource_type`](erl_nif.md#enif_open_resource_type) except
it accepts additional callback functions for resource types that are used
together with [`enif_select`](erl_nif.md#enif_select) and
[`enif_monitor_process`](erl_nif.md#enif_monitor_process).

Argument `init` is a pointer to an
[`ErlNifResourceTypeInit`](erl_nif.md#ErlNifResourceTypeInit) structure that
contains the function pointers for destructor, down and stop callbacks for the
resource type.

> #### Note {: .info }
>
> Only members `dtor`, `down` and `stop` in
> [`ErlNifResourceTypeInit`](erl_nif.md#ErlNifResourceTypeInit) are read by
> `enif_open_resource_type_x`. To implement the new `dyncall` callback use
> [`enif_init_resource_type`](erl_nif.md#enif_init_resource_type).

Available since OTP 20.0

## enif_init_resource_type()

```c
ErlNifResourceType * enif_init_resource_type(
        ErlNifEnv* env,
        const char* name,
        const ErlNifResourceTypeInit* init,
        ErlNifResourceFlags flags,
        ErlNifResourceFlags* tried);
```

Same as [`enif_open_resource_type_x`](erl_nif.md#enif_open_resource_type_x)
except it accepts an additional callback function for resource types that are
used together with
[`enif_dynamic_resource_call`](erl_nif.md#enif_dynamic_resource_call).

Argument `init` is a pointer to an
[`ErlNifResourceTypeInit`](erl_nif.md#ErlNifResourceTypeInit) structure that
contains the callback function pointers `dtor`, `down`, `stop` and the new
`dyncall`. The struct also contains the field `members` that must be set to the
number of initialized callbacks counted from the top of the struct. For example,
to initialize all callbacks including `dyncall`, `members` should be set to 4.
All callbacks are optional and may be set to `NULL`.

Available since OTP 24.0

## enif_port_command()

```c
int enif_port_command(
        ErlNifEnv* env, const
        ErlNifPort* to_port,
        ErlNifEnv *msg_env,
        ERL_NIF_TERM msg);
```

Works as `erlang:port_command/2`, except that it is always completely
asynchronous.

- **`env`** - The environment of the calling process. Must not be `NULL`.

- **`*to_port`** - The port ID of the receiving port. The port ID is to refer to
  a port on the local node.

- **`msg_env`** - The environment of the message term. Can be a process
  independent environment allocated with
  [`enif_alloc_env`](erl_nif.md#enif_alloc_env) or `NULL`.

- **`msg`** - The message term to send. The same limitations apply as on the
  payload to `erlang:port_command/2`.

Using a `msg_env` of `NULL` is an optimization, which groups together calls to
`enif_alloc_env`, `enif_make_copy`, `enif_port_command`, and `enif_free_env`
into one call. This optimization is only useful when a majority of the terms are
to be copied from `env` to `msg_env`.

Returns `true` if the command is successfully sent. Returns `false` if the
command fails, for example:

- `*to_port` does not refer to a local port.
- The currently executing process (that is, the sender) is not alive.
- `msg` is invalid.

See also [`enif_get_local_port`](erl_nif.md#enif_get_local_port).

Available since OTP 19.0

## enif_priv_data()

```c
void * enif_priv_data(
        ErlNifEnv* env);
```

Returns the pointer to the private data that was set by
[`load`](erl_nif.md#load) or [`upgrade`](erl_nif.md#upgrade).

Available since OTP R13B04

## enif_raise_exception()

```c
ERL_NIF_TERM enif_raise_exception(
        ErlNifEnv* env,
        ERL_NIF_TERM reason);
```

Creates an error exception with the term `reason` to be returned from a NIF, and
associates it with environment `env`. Once a NIF or any function it calls
invokes `enif_raise_exception`, the runtime ensures that the exception it
creates is raised when the NIF returns, even if the NIF attempts to return a
non-exception term instead.

The return value from `enif_raise_exception` can only be used as the return
value from the NIF that invoked it (directly or indirectly) or be passed to
[`enif_is_exception`](erl_nif.md#enif_is_exception), but not to any other NIF
API function.

See also [`enif_has_pending_exception`](erl_nif.md#enif_has_pending_exception)
and [`enif_make_badarg`](erl_nif.md#enif_make_badarg).

Available since OTP 18.0

## enif_realloc()

```c
void * enif_realloc(
        void* ptr,
        size_t size);
```

Reallocates memory allocated by [`enif_alloc`](erl_nif.md#enif_alloc) to `size`
bytes.

Returns `NULL` if the reallocation fails.

The returned pointer is suitably aligned for any built-in type that fit in the
allocated memory.

Available since OTP 20.2

## enif_realloc_binary()

```c
int enif_realloc_binary(
        ErlNifBinary* bin,
        size_t size);
```

Changes the size of a binary `bin`. The source binary can be read-only, in which
case it is left untouched and a mutable copy is allocated and assigned to
`*bin`.

Returns `true` on success, or `false` if memory allocation failed.

Available since OTP R13B04

## enif_release_binary()

```c
void enif_release_binary(
        ErlNifBinary* bin);
```

Releases a binary obtained from
[`enif_alloc_binary`](erl_nif.md#enif_alloc_binary).

## enif_release_resource()

```c
void enif_release_resource(
        void* obj);
```

Removes a reference to resource object `obj` obtained from
[`enif_alloc_resource`](erl_nif.md#enif_alloc_resource). The resource object is
destructed when the last reference is removed. Each call to
`enif_release_resource` must correspond to a previous call to
`enif_alloc_resource` or [`enif_keep_resource`](erl_nif.md#enif_keep_resource).
References made by [`enif_make_resource`](erl_nif.md#enif_make_resource) can
only be removed by the garbage collector.

There are no guarantees exactly when the destructor of an unreferenced resource
is called. It could be called directly by `enif_release_resource` but it could
also be scheduled to be called at a later time possibly by another thread.

Available since OTP R13B04

## enif_rwlock_create()

```c
ErlNifRWLock * enif_rwlock_create(
        char *name);
```

Same as [`erl_drv_rwlock_create`](erl_driver.md#erl_drv_rwlock_create).

Available since OTP R13B04

## enif_rwlock_destroy()

```c
void enif_rwlock_destroy(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_destroy`](erl_driver.md#erl_drv_rwlock_destroy).

Available since OTP R13B04

## enif_rwlock_name()

```c
char* enif_rwlock_name(
        ErlNifRWLock* rwlck);
```

Same as [`erl_drv_rwlock_name`](erl_driver.md#erl_drv_rwlock_name).

Available since OTP 21.0

## enif_rwlock_rlock()

```c
void enif_rwlock_rlock(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_rlock`](erl_driver.md#erl_drv_rwlock_rlock).

Available since OTP R13B04

## enif_rwlock_runlock()

```c
void enif_rwlock_runlock(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_runlock`](erl_driver.md#erl_drv_rwlock_runlock).

Available since OTP R13B04

## enif_rwlock_rwlock()

```c
void enif_rwlock_rwlock(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_rwlock`](erl_driver.md#erl_drv_rwlock_rwlock).

Available since OTP R13B04

## enif_rwlock_rwunlock()

```c
void enif_rwlock_rwunlock(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_rwunlock`](erl_driver.md#erl_drv_rwlock_rwunlock).

Available since OTP R13B04

## enif_rwlock_tryrlock()

```c
int enif_rwlock_tryrlock(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_tryrlock`](erl_driver.md#erl_drv_rwlock_tryrlock).

Available since OTP R13B04

## enif_rwlock_tryrwlock()

```c
int enif_rwlock_tryrwlock(
        ErlNifRWLock *rwlck);
```

Same as [`erl_drv_rwlock_tryrwlock`](erl_driver.md#erl_drv_rwlock_tryrwlock).

Available since OTP R13B04

## enif_schedule_nif()

```c
ERL_NIF_TERM enif_schedule_nif(
        ErlNifEnv* caller_env,
        const char* fun_name,
        int flags,
        ERL_NIF_TERM (*fp)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]),
        int argc,
        const ERL_NIF_TERM argv[]);
```

Schedules NIF `fp` to execute. This function allows an application to break up
long-running work into multiple regular NIF calls or to schedule a
[dirty NIF](erl_nif.md#dirty_nifs) to execute on a dirty scheduler thread.

- **`caller_env`** - Must be [process bound](erl_nif.md#proc_bound_env)
  environment of the calling NIF.

- **`fun_name`** - Provides a name for the NIF that is scheduled for execution.
  If it cannot be converted to an atom, `enif_schedule_nif` returns a `badarg`
  exception.

- **`flags`** - Must be set to `0` for a regular NIF,
  `ERL_NIF_DIRTY_JOB_CPU_BOUND` if the job is expected to be CPU-bound, or
  `ERL_NIF_DIRTY_JOB_IO_BOUND` for jobs that will be I/O-bound.

- **`argc` and `argv`** - Can either be the originals passed into the calling
  NIF, or can be values created by the calling NIF.

The calling NIF must use the return value of `enif_schedule_nif` as its own
return value.

Be aware that `enif_schedule_nif`, as its name implies, only schedules the NIF
for future execution. The calling NIF does not block waiting for the scheduled
NIF to execute and return. This means that the calling NIF cannot expect to
receive the scheduled NIF return value and use it for further operations.

Available since OTP 17.3

## enif_select()

```c
int enif_select(
        ErlNifEnv* env,
        ErlNifEvent event,
        enum ErlNifSelectFlags mode,
        void* obj,
        const ErlNifPid* pid,
        ERL_NIF_TERM ref);
```

This function can be used to receive asynchronous notifications when OS-specific
event objects become ready for either read or write operations.

Argument `event` identifies the event object. On Unix systems, the functions
`select`/`poll` are used. The event object must be a socket, pipe or other file
descriptor object that `select`/`poll` can use.

Argument `mode` describes the type of events to wait for. It can be
`ERL_NIF_SELECT_READ`, `ERL_NIF_SELECT_WRITE` or a bitwise OR combination to
wait for both. It can also be `ERL_NIF_SELECT_STOP` or `ERL_NIF_SELECT_CANCEL`
which are described further below. When a read or write event is triggered, a
notification message like this is sent to the process identified by `pid`:

```text
{select, Obj, Ref, ready_input | ready_output}
```

`ready_input` or `ready_output` indicates if the event object is ready for
reading or writing.

> #### Note {: .info }
>
> For complete control over the message format use the newer functions
> [`enif_select_read`](erl_nif.md#enif_select_read) or
> [`enif_select_write`](erl_nif.md#enif_select_write) introduced in erts-11.0
> (OTP-22.0).

Argument `pid` may be `NULL` to indicate the calling process. It must not be set
as [undefined](erl_nif.md#enif_set_pid_undefined).

Argument `obj` is a resource object obtained from
[`enif_alloc_resource`](erl_nif.md#enif_alloc_resource). The purpose of the
resource objects is as a container of the event object to manage its state and
lifetime. A handle to the resource is received in the notification message as
`Obj`.

Argument `ref` must be either a reference obtained from `erlang:make_ref/0` or
the atom `undefined`. It will be passed as `Ref` in the notifications. If a
selective `receive` statement is used to wait for the notification then a
reference created just before the `receive` will exploit a runtime optimization
that bypasses all earlier received messages in the queue.

The notifications are one-shot only. To receive further notifications of the
same type (read or write), repeated calls to `enif_select` must be made after
receiving each notification.

`ERL_NIF_SELECT_CANCEL` can be used to cancel previously selected events. It
must be used in a bitwise OR combination with `ERL_NIF_SELECT_READ` and/or
`ERL_NIF_SELECT_WRITE` to indicate which type of event to cancel. Arguments
`pid` and `ref` are ignored when `ERL_NIF_SELECT_CANCEL` is specified. The
return value will tell if the event was actually cancelled or if a notification
may already have been sent.

Use `ERL_NIF_SELECT_STOP` as `mode` in order to safely close an event object
that has been passed to `enif_select`. The
[`stop`](erl_nif.md#ErlNifResourceStop) callback of the resource `obj` will be
called when it is safe to close the event object. This safe way of closing event
objects must be used even if all notifications have been received (or cancelled)
and no further calls to `enif_select` have been made. `ERL_NIF_SELECT_STOP` will
first cancel any selected events before it calls or schedules the `stop`
callback. Arguments `pid` and `ref` are ignored when `ERL_NIF_SELECT_STOP` is
specified.

The first call to `enif_select` for a specific OS `event` will establish a
relation between the event object and the containing resource. All subsequent
calls for an `event` must pass its containing resource as argument `obj`. The
relation is dissolved when `enif_select` has been called with `mode` as
`ERL_NIF_SELECT_STOP` and the corresponding `stop` callback has returned. A
resource can contain several event objects but one event object can only be
contained within one resource. A resource will not be destructed until all its
contained relations have been dissolved.

> #### Note {: .info }
>
> Use [`enif_monitor_process`](erl_nif.md#enif_monitor_process) together with
> `enif_select` to detect failing Erlang processes and prevent them from causing
> permanent leakage of resources and their contained OS event objects.

Returns a non-negative value on success where the following bits can be set:

- **`ERL_NIF_SELECT_STOP_CALLED`** - The stop callback was called directly by
  `enif_select`.

- **`ERL_NIF_SELECT_STOP_SCHEDULED`** - The stop callback was scheduled to run
  on some other thread or later by this thread.

- **`ERL_NIF_SELECT_READ_CANCELLED`** - A read event was cancelled by
  `ERL_NIF_SELECT_CANCEL` or `ERL_NIF_SELECT_STOP` and is guaranteed not to
  generate a `ready_input` notification message.

- **`ERL_NIF_SELECT_WRITE_CANCELLED`** - A write event was cancelled by
  `ERL_NIF_SELECT_CANCEL` or `ERL_NIF_SELECT_STOP` and is guaranteed not to
  generate a `ready_output` notification message.

Returns a negative value if the call failed where the following bits can be set:

- **`ERL_NIF_SELECT_INVALID_EVENT`** - Argument `event` is not a valid OS event
  object.

- **`ERL_NIF_SELECT_FAILED`** - The system call failed to add the event object
  to the poll set.

> #### Note {: .info }
>
> Use bitwise AND to test for specific bits in the return value. New significant
> bits may be added in future releases to give more detailed information for
> both failed and successful calls. Do NOT use equality tests like `==`, as that
> may cause your application to stop working.
>
> Example:
>
> ```c
> retval = enif_select(env, fd, ERL_NIF_SELECT_STOP, resource, ref);
> if (retval < 0) {
>     /* handle error */
> }
> /* Success! */
> if (retval & ERL_NIF_SELECT_STOP_CALLED) {
>     /* ... */
> }
> ```

> #### Note {: .info }
>
> The mode flag `ERL_NIF_SELECT_CANCEL` and the return flags
> `ERL_NIF_SELECT_READ_CANCELLED` and `ERL_NIF_SELECT_WRITE_CANCELLED` were
> introduced in erts-11.0 (OTP-22.0).

Available since OTP 20.0

## enif_select_read()

Available since OTP 22.0

## enif_select_write()

```c
int enif_select_read(
        ErlNifEnv* env,
        ErlNifEvent event,
        void* obj,
        const ErlNifPid* pid,
        ERL_NIF_TERM msg,
        ErlNifEnv* msg_env);
```

```c
int enif_select_write(
        ErlNifEnv* env,
        ErlNifEvent event,
        void* obj,
        const ErlNifPid* pid,
        ERL_NIF_TERM msg,
        ErlNifEnv* msg_env);
```

These are variants of [enif_select](erl_nif.md#enif_select) where you can supply
your own message term `msg` that will be sent to the process instead of the
predefined tuple `{select,_,_,_}.`

Argument `msg_env` must either be `NULL` or the environment of `msg` allocated
with [`enif_alloc_env`](erl_nif.md#enif_alloc_env). If argument `msg_env` is
`NULL` the term `msg` will be copied, otherwise both `msg` and `msg_env` will be
invalidated by a successful call to `enif_select_read` or `enif_select_write`.
The environment is then to either be freed with
[`enif_free_env`](erl_nif.md#enif_free_env) or cleared for reuse with
[`enif_clear_env`](erl_nif.md#enif_clear_env). An unsuccessful call will leave
`msg` and `msg_env` still valid.

Apart from the message format `enif_select_read` and `enif_select_write` behaves
exactly the same as [enif_select](erl_nif.md#enif_select) with argument `mode`
as either `ERL_NIF_SELECT_READ` or `ERL_NIF_SELECT_WRITE`. To cancel or close
events use [enif_select](erl_nif.md#enif_select).

Available since OTP 22.0

## enif_self()

```c
ErlNifPid * enif_self(
        ErlNifEnv* caller_env,
        ErlNifPid* pid);
```

Initializes the [`ErlNifPid`](erl_nif.md#ErlNifPid) variable at `*pid` to
represent the calling process.

Returns `pid` if successful, or NULL if `caller_env` is not a
[process bound environment](erl_nif.md#proc_bound_env).

Available since OTP R14B

## enif_send()

```c
int enif_send(
        ErlNifEnv* caller_env,
        ErlNifPid* to_pid,
        ErlNifEnv* msg_env,
        ERL_NIF_TERM msg);
```

Sends a message to a process.

- **`caller_env`** - The environment of the calling thread
  ([process bound](erl_nif.md#proc_bound_env) or
  [callback](erl_nif.md#callback_env) environment) or `NULL` if calling from a
  custom thread not spawned by ERTS.

- **`*to_pid`** - The pid of the receiving process. The pid is to refer to a
  process on the local node.

- **`msg_env`** - The environment of the message term. Must be a process
  independent environment allocated with
  [`enif_alloc_env`](erl_nif.md#enif_alloc_env) or NULL.

- **`msg`** - The message term to send.

Returns `true` if the message is successfully sent. Returns `false` if the send
operation fails, that is:

- `*to_pid` does not refer to an alive local process.
- The currently executing process (that is, the sender) is not alive.

The message environment `msg_env` with all its terms (including `msg`) is
invalidated by a successful call to `enif_send`. The environment is to either be
freed with [`enif_free_env`](erl_nif.md#enif_free_env) or cleared for reuse with
[`enif_clear_env`](erl_nif.md#enif_clear_env). An unsuccessful call will leave
`msg` and `msg_env` still valid.

If `msg_env` is set to `NULL`, the `msg` term is copied and the original term
and its environment is still valid after the call.

This function is thread-safe.

> #### Note {: .info }
>
> Passing `msg_env` as `NULL` is only supported as from ERTS 8.0 (Erlang/OTP
> 19).

Available since OTP R14B

## enif_set_option()

```c
int enif_set_option(
        ErlNifEnv *env,
        ErlNifOption opt,
	...);
```

Set an option. On success, zero will be returned. On failure, a non zero value
will be returned. Currently the following options can be set:

- **[`ERL_NIF_OPT_DELAY_HALT`](erl_nif.md#ErlNifOption)**{: #delay_halt }
  ```c
  enif_set_option(env, ERL_NIF_OPT_DELAY_HALT)
  ```

  Enable delay of
  runtime system halt with flushing enabled until all calls to NIFs in the NIF
  library have returned. If the _delay halt_ feature has not been enabled, a
  halt with flushing enabled may complete even though processes are still
  executing inside NIFs in the NIF library. Note that by _returning_ we here
  mean the first point where the NIF returns control back to the runtime system,
  and _not_ the point where a call to a NIF return a value back to the Erlang
  code that called the NIF. That is, if you schedule execution of a NIF, using
  [`enif_schedule_nif()`](erl_nif.md#enif_schedule_nif), from within a NIF while
  the system is halting, the scheduled NIF call will _not_ be executed even
  though _delay halt_ has been enabled for the NIF library.

  The runtime system halts when one of the [`erlang:halt()`](`erlang:halt/0`)
  BIFs are called. By default flushing is enabled, but can be disabled using the
  `erlang:halt/2` BIF. When flushing has been disabled, the _delay halt_ setting
  will have no effect. That is, the runtime system will halt without waiting for
  NIFs to return even if the _delay halt_ setting has been enabled. See the
  [`{flush, boolean()}`](`m:erlang#halt_flush`) option of `erlang:halt/2` for
  more information.

  The `ERL_NIF_OPT_DELAY_HALT` option can only be set during loading of a NIF
  library in a call to `enif_set_option()` inside a NIF library
  [`load()`](erl_nif.md#load) or [`upgrade()`](erl_nif.md#upgrade) call, and
  will fail if set somewhere else. The `env` argument _must_ be the callback
  environment passed to the `load()` or the `upgrade()` call. This option can
  also only be set once. That is, the _delay halt_ setting cannot be changed
  once it has been enabled. The _delay halt_ setting is tied to the module
  instance with which the NIF library instance has been loaded. That is, in case
  both a new and old version of a module using the NIF library are loaded, they
  can have the same or different _delay halt_ settings.

  The _delay halt_ feature can be used in combination with an
  [_on halt_](erl_nif.md#on_halt) callback. The _on halt_ callback is in this
  case typically used to notify processes blocked in NIFs in the library that it
  is time to return in order to let the runtime system complete the halting.
  Such NIFs should be dirty NIFs, since ordinary NIFs should never block for a
  long time.

- **[`ERL_NIF_OPT_ON_HALT`](erl_nif.md#ErlNifOption)**{: #on_halt }
  ```c
  enif_set_option(env, ERL_NIF_OPT_ON_HALT, on_halt)
  ```

  Install a callback that will be called when the runtime system halts with
  flushing enabled.

  The runtime system halts when one of the [`erlang:halt()`](`erlang:halt/0`)
  BIFs are called. By default flushing is enabled, but can be disabled using the
  `erlang:halt/2` BIF. When flushing has been disabled, the runtime system will
  halt without calling any _on halt_ callbacks even if such are installed. See
  the [`{flush, boolean()}`](`m:erlang#halt_flush`) option of `erlang:halt/2`
  for more information.

  The `ERL_NIF_OPT_ON_HALT` option can only be set during loading of a NIF
  library in a call to `enif_set_option()` inside a NIF library
  [`load()`](erl_nif.md#load) or [`upgrade()`](erl_nif.md#upgrade) call, and
  will fail if called somewhere else. The `env` argument _must_ be the callback
  environment passed to the `load()` or the `upgrade()` call. The `on_halt`
  argument should be a function pointer to the callback to install.

  The [`on_halt`](erl_nif.md#ErlNifOnHaltCallback) callback will be tied to the
  module instance with which the NIF library
  instance has been loaded. That is, in case both a new and old version of a
  module using the NIF library are loaded, they can both have different, none,
  or the same _on halt_ callbacks installed. When unloading the NIF library
  during a [code purge](`code:purge/1`), an installed _on halt_ callback will be
  uninstalled. The `ERL_NIF_OPT_ON_HALT` option can also only be set once. That
  is, the _on halt_ callback cannot be changed or removed once it has been
  installed by any other means than purging the module instance that loaded the
  NIF library.

  When the installed _on halt_ callback is called, it will be passed a pointer
  to `priv_data` as argument. The `priv_data` pointer can be set when loading
  the NIF library.

  The _on halt_ callback can be used in combination with
  [_delay of halt_](erl_nif.md#delay_halt) until all calls into the library have
  returned. The _on halt_ callback is in this case typically used to notify
  processes blocked in NIFs in the library that it is time to return in order to
  let the runtime system complete the halting. Such NIFs should be dirty NIFs,
  since ordinary NIFs should never block for a long time.

- **[`ERL_NIF_OPT_ON_UNLOAD_THREAD`](erl_nif.md#ErlNifOption)**{: #on_unload_thread }
  ```c
  enif_set_option(env, ERL_NIF_OPT_ON_UNLOAD_THREAD, on_unload_thread)
  ```

  Install a callback that will be called **by each scheduler thread** when the
  module instance that the NIF library belongs to is purged as old. A typical
  use is to release thread specific data.

  The `ERL_NIF_OPT_ON_UNLOAD_THREAD` option can only be set during loading of a
  NIF library inside a call to [`load()`](erl_nif.md#load) or
  [`upgrade()`](erl_nif.md#upgrade) and will fail if called somewhere else. The
  `env` argument _must_ be the callback environment passed to the `load()` or
  the `upgrade()` call.

  The [`on_unload_thread`](erl_nif.md#ErlNifOnUnloadThreadCallback) argument
  should be a function pointer to the callback to install. The
  _on_unload_thread_ callback will be tied to the module instance with which the
  NIF library instance has been loaded. That is, in case both a new and old
  version of a module using the NIF library are loaded, they can both have
  different, none, or the same _on_unload_thread_ callbacks installed. The
  `ERL_NIF_OPT_ON_UNLOAD_THREAD` option can only be set once and cannot be
  changed or removed once it has been installed for a module instance.

  When the installed _on_unload_thread_ callback is called, it will be passed a
  pointer to `priv_data` as argument. The `priv_data` pointer can be set when
  loading the NIF library.

  The calls to the _on_unload_thread_ function are made concurrently by the
  different scheduler threads. There is no synchronization enforced between the
  threads. However, the single finalizing call to the [`unload()`](erl_nif.md#unload)
  callback for the module instance will not be made until all calls to
  _on_unload_thread_ have returned.

Available since OTP 26.0

## enif_set_pid_undefined()

```c
void enif_set_pid_undefined(
        ErlNifPid* pid);
```

Sets an [`ErlNifPid`](erl_nif.md#ErlNifPid) variable as undefined. See
[`enif_is_pid_undefined`](erl_nif.md#enif_is_pid_undefined).

Available since OTP 22.0

## enif_sizeof_resource()

```c
unsigned enif_sizeof_resource(
        void* obj);
```

Gets the byte size of resource object `obj` obtained by
[`enif_alloc_resource`](erl_nif.md#enif_alloc_resource).

Available since OTP R13B04

## enif_snprintf()

```c
int enif_snprintf(
        char *str,
        size_t size,
        const char *format,
	...);
```

Similar to `snprintf` but this format string also accepts `"%T"`, which formats
Erlang terms of type [`ERL_NIF_TERM`](erl_nif.md#ERL_NIF_TERM).

This function is primarily intended for debugging purpose. It is not recommended
to print very large terms with `%T`. The function may change `errno`, even if
successful.

Available since OTP 19.0

## enif_system_info()

```c
void enif_system_info(
        ErlNifSysInfo *sys_info_ptr,
        size_t size);
```

Same as [`driver_system_info`](erl_driver.md#driver_system_info).

Available since OTP R13B04

## enif_term_to_binary()

```c
int enif_term_to_binary(
        ErlNifEnv *env,
        ERL_NIF_TERM term,
        ErlNifBinary *bin);
```

Allocates a new binary with [`enif_alloc_binary`](erl_nif.md#enif_alloc_binary)
and stores the result of encoding `term` according to the Erlang external term
format.

Returns `true` on success, or `false` if the allocation fails.

See also `erlang:term_to_binary/1` and
[`enif_binary_to_term`](erl_nif.md#enif_binary_to_term).

Available since OTP 19.0

## enif_term_type()

```c
ErlNifTermType enif_term_type(
        ErlNifEnv *env,
        ERL_NIF_TERM term);
```

Determines the type of the given term. The term must be an ordinary Erlang term
and not one of the special terms returned by
[`enif_raise_exception`](erl_nif.md#enif_raise_exception),
[`enif_schedule_nif`](erl_nif.md#enif_schedule_nif), or similar.

The following types are defined at the moment:

- **`ERL_NIF_TERM_TYPE_ATOM`**

- **`ERL_NIF_TERM_TYPE_BITSTRING`** - A bitstring or binary

- **`ERL_NIF_TERM_TYPE_FLOAT`**

- **`ERL_NIF_TERM_TYPE_FUN`**

- **`ERL_NIF_TERM_TYPE_INTEGER`**

- **`ERL_NIF_TERM_TYPE_LIST`** - A list, empty or not

- **`ERL_NIF_TERM_TYPE_MAP`**

- **`ERL_NIF_TERM_TYPE_PID`**

- **`ERL_NIF_TERM_TYPE_PORT`**

- **`ERL_NIF_TERM_TYPE_REFERENCE`**

- **`ERL_NIF_TERM_TYPE_TUPLE`**

Note that new types may be added in the future, so the caller must be prepared
to handle unknown types.

Available since OTP 22.0

## enif_thread_create()

```c
int enif_thread_create(
        char *name,
        ErlNifTid *tid,
        void * (*func)(void *),
        void *args,
        ErlNifThreadOpts *opts);
```

Same as [`erl_drv_thread_create`](erl_driver.md#erl_drv_thread_create).

Available since OTP R13B04

## enif_thread_exit()

```c
void enif_thread_exit(
        void *resp);
```

Same as [`erl_drv_thread_exit`](erl_driver.md#erl_drv_thread_exit).

Available since OTP R13B04

## enif_thread_join()

```c
int enif_thread_join(
        ErlNifTid tid,
        void **respp);
```

Same as [`erl_drv_thread_join`](erl_driver.md#erl_drv_thread_join).

Available since OTP R13B04

## enif_thread_name()

```c
char* enif_thread_name(
        ErlNifTid tid);
```

Same as [`erl_drv_thread_name`](erl_driver.md#erl_drv_thread_name).

Available since OTP 21.0

## enif_thread_opts_create()

```c
ErlNifThreadOpts * enif_thread_opts_create(
        char *name);
```

Same as
[`erl_drv_thread_opts_create`](erl_driver.md#erl_drv_thread_opts_create).

Available since OTP R13B04

## enif_thread_opts_destroy()

```c
void enif_thread_opts_destroy(
        ErlNifThreadOpts *opts);
```

Same as
[`erl_drv_thread_opts_destroy`](erl_driver.md#erl_drv_thread_opts_destroy).

Available since OTP R13B04

## enif_thread_self()

```c
ErlNifTid enif_thread_self(void);
```

Same as [`erl_drv_thread_self`](erl_driver.md#erl_drv_thread_self).

Available since OTP R13B04

## enif_thread_type()

```c
int enif_thread_type(void);
```

Determine the type of currently executing thread. A positive value indicates a
scheduler thread while a negative value or zero indicates another type of
thread. Currently the following specific types exist (which may be extended in
the future):

- **`ERL_NIF_THR_UNDEFINED`** - Undefined thread that is not a scheduler thread.

- **`ERL_NIF_THR_NORMAL_SCHEDULER`** - A normal scheduler thread.

- **`ERL_NIF_THR_DIRTY_CPU_SCHEDULER`** - A dirty CPU scheduler thread.

- **`ERL_NIF_THR_DIRTY_IO_SCHEDULER`** - A dirty I/O scheduler thread.

Available since OTP 19.0

## enif_time_offset()

```c
ErlNifTime enif_time_offset(
        ErlNifTimeUnit time_unit);
```

Returns the current time offset between
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
[Erlang system time](time_correction.md#erlang-system-time) converted into the
`time_unit` passed as argument.

`time_unit` is the time unit of the returned value.

Returns `ERL_NIF_TIME_ERROR` if called with an invalid time unit argument or if
called from a thread that is not a scheduler thread.

See also [`ErlNifTime`](erl_nif.md#ErlNifTime) and
[`ErlNifTimeUnit`](erl_nif.md#ErlNifTimeUnit).

Available since OTP 18.3

## enif_tsd_get()

```c
void * enif_tsd_get(
        ErlNifTSDKey key);
```

Same as [`erl_drv_tsd_get`](erl_driver.md#erl_drv_tsd_get).

Available since OTP R13B04

## enif_tsd_key_create()

```c
int enif_tsd_key_create(
        char *name,
        ErlNifTSDKey *key);
```

Same as [`erl_drv_tsd_key_create`](erl_driver.md#erl_drv_tsd_key_create).

Available since OTP R13B04

## enif_tsd_key_destroy()

```c
void enif_tsd_key_destroy(
        ErlNifTSDKey key);
```

Same as [`erl_drv_tsd_key_destroy`](erl_driver.md#erl_drv_tsd_key_destroy).

Available since OTP R13B04

## enif_tsd_set()

```c
void enif_tsd_set(
        ErlNifTSDKey key,
        void *data);
```

Same as [`erl_drv_tsd_set`](erl_driver.md#erl_drv_tsd_set).

Available since OTP R13B04

## enif_vfprintf()

```c
int enif_vfprintf(
        FILE *stream,
        const char *format,
        va_list ap);
```

Equivalent to [`enif_fprintf`](erl_nif.md#enif_fprintf) except that its called
with a `va_list` instead of a variable number of arguments.

Available since OTP 21.0

## enif_vsnprintf()

```c
int enif_vsnprintf(
        char *str,
        size_t size,
        const char *format,
        va_list ap);
```

Equivalent to [`enif_snprintf`](erl_nif.md#enif_snprintf) except that its called
with a `va_list` instead of a variable number of arguments.

Available since OTP 21.0

## enif_whereis_pid()

```c
int enif_whereis_pid(
        ErlNifEnv *caller_env,
        ERL_NIF_TERM name,
        ErlNifPid *pid);
```

Looks up a process by its registered name.

- **`caller_env`** - The environment of the calling thread
  ([process bound](erl_nif.md#proc_bound_env) or
  [callback](erl_nif.md#callback_env) environment) or `NULL` if calling from a
  custom thread not spawned by ERTS.

- **`name`** - The name of a registered process, as an atom.

- **`*pid`** - The [`ErlNifPid`](erl_nif.md#ErlNifPid) in which the resolved
  process id is stored.

On success, sets `*pid` to the local process registered with `name` and returns
`true`. If `name` is not a registered process, or is not an atom, `false` is
returned and `*pid` is unchanged.

Works as `erlang:whereis/1`, but restricted to processes. See
[`enif_whereis_port`](erl_nif.md#enif_whereis_port) to resolve registered ports.

Available since OTP 20.0

## enif_whereis_port()

```c
int enif_whereis_port(
        ErlNifEnv *caller_env,
        ERL_NIF_TERM name,
        ErlNifPort *port);
```

Looks up a port by its registered name.

- **`caller_env`** - The environment of the calling thread
  ([process bound](erl_nif.md#proc_bound_env) or
  [callback](erl_nif.md#callback_env) environment) or `NULL` if calling from a
  custom thread not spawned by ERTS.

- **`name`** - The name of a registered port, as an atom.

- **`*port`** - The [`ErlNifPort`](erl_nif.md#ErlNifPort) in which the resolved
  port id is stored.

On success, sets `*port` to the port registered with `name` and returns `true`.
If `name` is not a registered port, or is not an atom, `false` is returned and
`*port` is unchanged.

Works as `erlang:whereis/1`, but restricted to ports. See
[`enif_whereis_pid`](erl_nif.md#enif_whereis_pid) to resolve registered
processes.

Available since OTP 20.0

## See Also

`erlang:load_nif/2`
[NIFs (tutorial)](`e:system:nif.md`)
[Debugging NIFs and Port Drivers](`e:system:debugging.md`)
