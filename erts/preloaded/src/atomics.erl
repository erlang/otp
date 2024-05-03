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

%% Purpose : Main atomics API module.

-module(atomics).
-moduledoc """
Atomic Functions

This module provides a set of functions to do atomic operations towards mutable
atomic variables. The implementation utilizes only atomic hardware instructions
without any software level locking, which makes it very efficient for concurrent
access. The atomics are organized into arrays with the following semantics:

- Atomics are 64 bit integers.
- Atomics can be represented as either signed or unsigned.
- Atomics wrap around at overflow and underflow operations.
- All operations guarantee atomicity. No intermediate results can be seen. The
  result of one mutation can only be the input to one following mutation.
- All atomic operations are mutually ordered. If atomic B is updated _after_
  atomic A, then that is how it will appear to any concurrent readers. No one
  can read the new value of B and then read the old value of A.
- Indexes into atomic arrays are one-based. An atomic array of arity N contains
  N atomics with index from 1 to N.
""".
-moduledoc(#{since => "OTP 21.2"}).

-export([new/2,
         put/3, get/2,
         add/3, add_get/3,
         sub/3, sub_get/3,
         exchange/3, compare_exchange/4,
         info/1]).

-export_type([atomics_ref/0]).

-doc "Identifies an atomic array returned from `new/2`.".
-opaque atomics_ref() :: reference().

%% We must inline this function so that the stacktrace points to
%% the correct function.
-compile({inline, [error_with_info/2]}).

-define(OPT_SIGNED, (1 bsl 0)).
-define(OPT_DEFAULT, ?OPT_SIGNED).

-doc """
Create a new array of `Arity` number of atomics. All atomics in the array are
initially set to zero.

Argument `Opts` is a list of the following possible options:

- **`{signed, boolean()}`** - Indicate if the elements of the array will be
  treated as signed or unsigned integers. Default is `true` (signed).

  The integer interval for signed atomics are from `-(1 bsl 63)` to
  `(1 bsl 63)-1` and for unsigned atomics from `0` to `(1 bsl 64)-1`.

Atomics are not tied to the current process and are automatically garbage
collected when they are no longer referenced.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec new(Arity, Opts) -> atomics_ref() when
      Arity :: pos_integer(),
      Opts :: [Opt],
      Opt :: {signed, boolean()}.
new(Arity, Opts) ->
    try
        EncodedOpts = encode_opts(Opts, ?OPT_DEFAULT),
        erts_internal:atomics_new(Arity, EncodedOpts)
    catch
        throw:badopt ->
            ExtraInfo = [{error_info, #{module => erl_erts_errors,
                                        cause => badopt}}],
            error(badarg, [Arity, Opts], ExtraInfo);
        error:Error ->
            error_with_info(Error, [Arity, Opts])
    end.

encode_opts([{signed, true}|T], Acc) ->
    encode_opts(T, Acc bor ?OPT_SIGNED);
encode_opts([{signed, false}|T], Acc) ->
    encode_opts(T, Acc band (bnot ?OPT_SIGNED));
encode_opts([], Acc) ->
    Acc;
encode_opts(_, _) ->
    throw(badopt).

-doc "Set atomic to `Value`.".
-doc(#{since => <<"OTP 21.2">>}).
-spec put(Ref, Ix, Value) -> ok when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Value :: integer().
put(_Ref, _Ix, _Value) ->
    erlang:nif_error(undef).

-doc "Read atomic value.".
-doc(#{since => <<"OTP 21.2">>}).
-spec get(Ref, Ix) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer().
get(_Ref, _Ix) ->
    erlang:nif_error(undef).

-doc "Add `Incr` to atomic.".
-doc(#{since => <<"OTP 21.2">>}).
-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Incr :: integer().
add(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-doc "Atomically add `Incr` to atomic and return the result.".
-doc(#{since => <<"OTP 21.2">>}).
-spec add_get(Ref, Ix, Incr) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Incr :: integer().
add_get(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-doc "Subtract `Decr` from atomic.".
-doc(#{since => <<"OTP 21.2">>}).
-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Decr :: integer().
sub(Ref, Ix, Decr) ->
    try
        ?MODULE:add(Ref, Ix, -Decr)
    catch
        error:Error ->
            error_with_info(Error, [Ref, Ix, Decr])
    end.

-doc "Atomically subtract `Decr` from atomic and return the result.".
-doc(#{since => <<"OTP 21.2">>}).
-spec sub_get(Ref, Ix, Decr) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Decr :: integer().
sub_get(Ref, Ix, Decr) ->
    try
        ?MODULE:add_get(Ref, Ix, -Decr)
    catch
        error:Error ->
            error_with_info(Error, [Ref, Ix, Decr])
    end.

-doc """
Atomically replace the value of the atomic with `Desired` and return the previous value.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec exchange(Ref, Ix, Desired) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Desired :: integer().
exchange(_Ref, _Ix, _Desired) ->
    erlang:nif_error(undef).

-doc """
Atomically compare the atomic with `Expected`, and if those are equal, set
atomic to `Desired`.

Return `ok` if `Desired` was written. Return the actual atomic value if
not equal to `Expected`.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec compare_exchange(Ref, Ix, Expected, Desired) -> ok | integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Expected :: integer(),
      Desired :: integer().
compare_exchange(_Ref, _Ix, _Expected, _Desired) ->
    erlang:nif_error(undef).

-doc """
Return information about an atomic array in a map.

The map has the following keys:

- **`size`** - The number of atomics in the array.
- **`max`** - The highest possible value an atomic in this array can hold.
- **`min`** - The lowest possible value an atomic in this array can hold.
- **`memory`** - Approximate memory consumption for the array in bytes.
""".
-doc(#{since => <<"OTP 21.2">>}).
-spec info(Ref) -> Info when
      Ref  :: atomics_ref(),
      Info :: #{'size':=Size,'max':=Max,'min':=Min,'memory':=Memory},
      Size :: non_neg_integer(),
      Max :: integer(),
      Min :: integer(),
      Memory :: non_neg_integer().
info(_Ref) ->
    erlang:nif_error(undef).

error_with_info(Reason, Args) ->
    error(Reason, Args, [{error_info, #{module => erl_erts_errors}}]).
