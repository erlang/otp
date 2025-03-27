%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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

%% Purpose : Main counters API module.

-module(counters).
-moduledoc """
Counter Functions

This module provides a set of functions to do operations towards shared mutable
counter variables. The implementation does not utilize any software level
locking, which makes it very efficient for concurrent access. The counters are
organized into arrays with the following semantics:

- Counters are 64 bit signed integers.
- Counters wrap around at overflow and underflow operations.
- Counters are initialized to zero.
- Write operations guarantee atomicity. No intermediate results can be seen from
  a single write operation.
- Two types of counter arrays can be created with options `atomics` or
  `write_concurrency`. The `atomics` counters have good allround performance
  with nice consistent semantics while `write_concurrency` counters offers even
  better concurrent write performance at the expense of some potential read
  inconsistencies. See `new/2`.
- Indexes into counter arrays are one-based. A counter array of size N contains
  N counters with index from 1 to N.
""".
-moduledoc(#{since => "OTP 21.2"}).

-export([new/2,
         get/2,
         add/3,
         sub/3,
         put/3,
         info/1]).

-export_type([counters_ref/0]).

-doc "Identifies a counter array returned from `new/2`.".
-opaque counters_ref() :: {atomics, reference()} | {write_concurrency, reference()}.

%% We must inline this function so that the stacktrace points to
%% the correct function.
-compile({inline, [error_with_info/2]}).

-doc """
Create a new counter array of `Size` counters. All counters in the array are
initially set to zero.

Argument `Opts` is a list of the following possible options:

- **`atomics` (Default)** - Counters will be sequentially consistent. If write
  operation A is done sequentially before write operation B, then a concurrent
  reader may see the result of none of them, only A, or both A and B. It cannot
  see the result of only B.

- **`write_concurrency`** - This is an optimization to achieve very efficient
  concurrent [`add`](`add/3`) and [`sub`](`sub/3`) operations at the expense of
  potential read inconsistency and memory consumption per counter.

  Read operations may see sequentially inconsistent results with regard to
  concurrent write operations. Even if write operation A is done sequentially
  before write operation B, a concurrent reader may see any combination of A and
  B, including only B. A read operation is only guaranteed to see all writes
  done sequentially before the read. No writes are ever lost, but will
  eventually all be seen.

  The typical use case for `write_concurrency` is when concurrent calls to
  [`add`](`add/3`) and [`sub`](`sub/3`) toward the same counters are very
  frequent, while calls to [`get` ](`get/2`)and [`put`](`put/3`) are much less
  frequent. The lack of absolute read consistency must also be acceptable.

Counters are not tied to the current process and are automatically garbage
collected when they are no longer referenced.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec new(Size, Opts) -> counters_ref() when
      Size :: pos_integer(),
      Opts :: [Opt],
      Opt :: atomics | write_concurrency.
new(Size, Options) ->
    try
        case Options of
            [atomics] ->
                {atomics, atomics:new(Size, [{signed, true}])};
            [write_concurrency] ->
                {write_concurrency, erts_internal:counters_new(Size)};
            [] ->
                {atomics, atomics:new(Size, [{signed, true}])};
            _ ->
                error(badopt)
        end
    catch
        error:badopt ->
            ExtraInfo = [{error_info, #{module => erl_erts_errors,
                                        cause => badopt}}],
            error(badarg, [Size, Options], ExtraInfo);
        error:Error ->
            error_with_info(Error, [Size, Options])
    end.


-doc "Read counter value.".
-doc(#{since => <<"OTP 21.2">>}).
-spec get(Ref, Ix) -> integer() when
      Ref  :: counters_ref(),
      Ix :: pos_integer().
get(Ref, Ix) ->
    try
        case Ref of
            {atomics, R} ->
                atomics:get(R, Ix);
            {write_concurrency, R} ->
                erts_internal:counters_get(R, Ix);
            _ ->
                error(badarg)
        end
    catch
        error:Error ->
            error_with_info(Error, [Ref, Ix])
    end.


-doc "Add `Incr` to counter at index `Ix`.".
-doc(#{since => <<"OTP 21.2">>}).
-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: counters_ref(),
      Ix :: pos_integer(),
      Incr :: integer().
add(Ref, Ix, Incr) ->
    try
        case Ref of
            {atomics, R} ->
                atomics:add(R, Ix, Incr);
            {write_concurrency, R} ->
                erts_internal:counters_add(R, Ix, Incr);
            _ ->
                error(badarg)
        end
    catch
        error:Error ->
            error_with_info(Error, [Ref, Ix, Incr])
    end.


-doc "Subtract `Decr` from counter at index `Ix`.".
-doc(#{since => <<"OTP 21.2">>}).
-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: counters_ref(),
      Ix :: pos_integer(),
      Decr :: integer().
sub(Ref, Ix, Decr) ->
    try
        Incr = -Decr,
        case Ref of
            {atomics, R} ->
                atomics:add(R, Ix, Incr);
            {write_concurrency, R} ->
                erts_internal:counters_add(R, Ix, Incr);
            _ ->
                error(badarg)
        end
    catch
        error:Error ->
            error_with_info(Error, [Ref, Ix, Decr])
    end.


-doc """
Write `Value` to counter at index `Ix`.

> #### Note {: .info }
>
> Despite its name, the `write_concurrency` optimization does not improve `put`.
> A call to `put` is a relatively heavy operation compared to the very
> lightweight and scalable [`add`](`add/3`) and [`sub`](`sub/3`). The cost for a
> `put` with `write_concurrency` is like a [`get` ](`get/2`)plus a `put` without
> `write_concurrency`.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec put(Ref, Ix, Value) -> ok when
      Ref  :: counters_ref(),
      Ix :: pos_integer(),
      Value :: integer().
put(Ref, Ix, Value) ->
    try
        case Ref of
            {atomics, R} ->
                atomics:put(R, Ix, Value);
            {write_concurrency, R} ->
                erts_internal:counters_put(R, Ix, Value);
            _ ->
                error(badarg)
        end
    catch
        error:Error ->
            error_with_info(Error, [Ref, Ix, Value])
    end.


-doc """
Return information about a counter array in a map.

The map has the following keys (at least):

- **`size`** - The number of counters in the array.
- **`memory`** - Approximate memory consumption for the array in bytes.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec info(Ref) -> Info when
      Ref  :: counters_ref(),
      Info :: #{'size':=Size, 'memory':=Memory},
      Size :: non_neg_integer(),
      Memory :: non_neg_integer().
info(Ref) ->
    try
        case Ref of
            {atomics, R} ->
                atomics:info(R);
            {write_concurrency, R} ->
                erts_internal:counters_info(R);
            _ ->
                error(badarg)
        end
    catch
        error:Error ->
            error_with_info(Error, [Ref])
    end.

error_with_info(Reason, Args) ->
    error(Reason, Args, [{error_info, #{module => erl_erts_errors}}]).
