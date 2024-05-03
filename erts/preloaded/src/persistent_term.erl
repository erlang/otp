%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(persistent_term).
-moduledoc """
Persistent terms.

This module is similar to `m:ets` in that it provides a storage for Erlang terms
that can be accessed in constant time, but with the difference that
`persistent_term` has been highly optimized for reading terms at the expense of
writing and updating terms. When a persistent term is updated or deleted, a
global garbage collection pass is run to scan all processes for the deleted
term, and to copy it into each process that still uses it. Therefore,
`persistent_term` is suitable for storing Erlang terms that are frequently
accessed but never or infrequently updated.

> #### Warning {: .warning }
>
> Persistent terms is an advanced feature and is not a general replacement for
> ETS tables. Before using persistent terms, make sure to fully understand the
> consequence to system performance when updating or deleting persistent terms.

Term lookup (using `get/1`) is done in constant time and without taking any
locks, and the term is **not** copied to the heap (as is the case with terms
stored in ETS tables).

Storing or updating a term (using `put/2`) is proportional to the number of
already created persistent terms because the hash table holding the keys will be
copied. In addition, the term itself will be copied.

When a (complex) term is deleted (using `erase/1`) or replaced by another (using
`put/2`), a global garbage collection is initiated. It works like this:

- All processes in the system will be scheduled to run a scan of their heaps for
  the term that has been deleted. While such scan is relatively light-weight, if
  there are many processes, the system can become less responsive until all
  processes have scanned their heaps.
- If the deleted term (or any part of it) is still used by a process, that
  process will do a major (fullsweep) garbage collection and copy the term into
  the process. However, at most two processes at a time will be scheduled to do
  that kind of garbage collection.

Deletion of atoms and other terms that fit in one machine word is specially
optimized to avoid doing a global GC. It is still not recommended to update
persistent terms with such values too frequently because the hash table holding
the keys is copied every time a persistent term is updated.

Some examples are suitable uses for persistent terms are:

- Storing of configuration data that must be easily accessible by all processes.
- Storing of references for NIF resources.
- Storing of references for efficient counters.
- Storing an atom to indicate a logging level or whether debugging is turned on.

## Storing Huge Persistent Terms

The current implementation of persistent terms uses the literal
[allocator](erts_alloc.md) also used for literals (constant terms) in BEAM code.
By default, 1 GB of virtual address space is reserved for literals in BEAM code
and persistent terms. The amount of virtual address space reserved for literals
can be changed by using the [`+MIscs option`](erts_alloc.md#MIscs) when starting
the emulator.

Here is an example how the reserved virtual address space for literals can be
raised to 2 GB (2048 MB):

```text
    erl +MIscs 2048
```

## Best Practices for Using Persistent Terms

It is recommended to use keys like `?MODULE` or `{?MODULE,SubKey}` to avoid name
collisions.

Prefer creating a few large persistent terms to creating many small persistent
terms. The execution time for storing a persistent term is proportional to the
number of already existing terms.

Updating a persistent term with the same value as it already has is specially
optimized to do nothing quickly; thus, there is no need compare the old and new
values and avoid calling `put/2` if the values are equal.

When atoms or other terms that fit in one machine word are deleted, no global GC
is needed. Therefore, persistent terms that have atoms as their values can be
updated more frequently, but note that updating such persistent terms is still
much more expensive than reading them.

Updating or deleting a persistent term will trigger a global GC if the term does
not fit in one machine word. Processes will be scheduled as usual, but all
processes will be made runnable at once, which will make the system less
responsive until all processes have run and scanned their heaps for the deleted
terms. One way to minimize the effects on responsiveness could be to minimize
the number of processes on the node before updating or deleting a persistent
term. It would also be wise to avoid updating terms when the system is at peak
load.

Avoid storing a retrieved persistent term in a process if that persistent term
could be deleted or updated in the future. If a process holds a reference to a
persistent term when the term is deleted, the process will be garbage collected
and the term copied to the process.

Avoid updating or deleting more than one persistent term at a time. Each deleted
term will trigger its own global GC. That means that deleting N terms will make
the system less responsive N times longer than deleting a single persistent
term. Therefore, terms that are to be updated at the same time should be
collected into a larger term, for example, a map or a tuple.

## Example

The following example shows how lock contention for ETS tables can be minimized
by having one ETS table for each scheduler. The table identifiers for the ETS
tables are stored as a single persistent term:

```erlang
    %% There is one ETS table for each scheduler.
    Sid = erlang:system_info(scheduler_id),
    Tid = element(Sid, persistent_term:get(?MODULE)),
    ets:update_counter(Tid, Key, 1).
```
""".
-moduledoc(#{since => "OTP 21.2"}).

-export([erase/1,get/0,get/1,get/2,info/0,put/2]).

-doc "Any Erlang term.".
-type key() :: term().
-doc "Any Erlang term.".
-type value() :: term().

-doc """
Erase the name for the persistent term with key `Key`.

The return value will be `true` if there was a persistent term with the key `Key`,
and `false` if there was no persistent term associated with the key.

If there existed a previous persistent term associated with key `Key`, a global
GC has been initiated when [`erase/1`](`erase/1`) returns. See
[Description](`m:persistent_term`).
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec erase(Key) -> Result when
      Key :: key(),
      Result :: boolean().
erase(_Key) ->
    erlang:nif_error(undef).

-doc """
Retrieve the keys and values for all persistent terms.

The keys will be copied to the heap for the process calling `get/0`,
but the values will not.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec get() -> List when
      List :: [{key(),value()}].
get() ->
    erlang:nif_error(undef).

-doc """
Retrieve the value for the persistent term associated with the key `Key`.

The lookup will be made in constant time and the value will not be copied to the
heap of the calling process.

This function fails with a `badarg` exception if no term has been stored with
the key `Key`.

If the calling process holds on to the value of the persistent term and the
persistent term is deleted in the future, the term will be copied to the
process.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec get(Key) -> Value when
      Key :: key(),
      Value :: value().
get(_Key) ->
    erlang:nif_error(undef).

-doc """
Retrieve the value for the persistent term associated with the key `Key`.

The lookup will be made in constant time and the value will not be copied to the
heap of the calling process.

This function returns `Default` if no term has been stored with the key `Key`.

If the calling process holds on to the value of the persistent term and the
persistent term is deleted in the future, the term will be copied to the
process.
""".
-doc(#{since => <<"OTP 21.3">>}).
-spec get(Key, Default) -> Value when
      Key :: key(),
      Default :: value(),
      Value :: value().
get(_Key, _Default) ->
    erlang:nif_error(undef).

-doc """
Return information about persistent terms in a map.

The map has the following keys:

- **`count`** - The number of persistent terms.

- **`memory`** - The total amount of memory (measured in bytes) used by all
  persistent terms.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec info() -> Info when
      Info :: #{'count':=Count,'memory':=Memory},
      Count :: non_neg_integer(),
      Memory :: non_neg_integer().
info() ->
    erlang:nif_error(undef).

-doc """
Store the value `Value` as a persistent term and associate it with the key
`Key`.

If the value `Value` is equal to the value previously stored for the key,
[`put/2`](`put/2`) will do nothing and return quickly.

If there existed a previous persistent term associated with key `Key`, a global
GC has been initiated when [`put/2`](`put/2`) returns. See
[Description](`m:persistent_term`).
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec put(Key, Value) -> 'ok' when
      Key :: key(),
      Value :: value().
put(_Key, _Value) ->
    erlang:nif_error(undef).
