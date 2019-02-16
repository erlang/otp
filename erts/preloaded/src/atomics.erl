%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

-export([new/2,
         put/3, get/2,
         add/3, add_get/3,
         sub/3, sub_get/3,
         exchange/3, compare_exchange/4,
         info/1]).

-export_type([atomics_ref/0]).

-opaque atomics_ref() :: reference().

-define(OPT_SIGNED, (1 bsl 0)).
-define(OPT_DEFAULT, ?OPT_SIGNED).

-spec new(Arity, Opts) -> atomics_ref() when
      Arity :: pos_integer(),
      Opts :: [Opt],
      Opt :: {signed, boolean()}.
new(Arity, Opts) ->
    erts_internal:atomics_new(Arity, encode_opts(Opts, ?OPT_DEFAULT)).

encode_opts([{signed, true}|T], Acc) ->
    encode_opts(T, Acc bor ?OPT_SIGNED);
encode_opts([{signed, false}|T], Acc) ->
    encode_opts(T, Acc band (bnot ?OPT_SIGNED));
encode_opts([], Acc) ->
    Acc;
encode_opts(_, _) ->
    erlang:error(badarg).

-spec put(Ref, Ix, Value) -> ok when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Value :: integer().
put(_Ref, _Ix, _Value) ->
    erlang:nif_error(undef).

-spec get(Ref, Ix) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer().
get(_Ref, _Ix) ->
    erlang:nif_error(undef).

-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Incr :: integer().
add(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-spec add_get(Ref, Ix, Incr) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Incr :: integer().
add_get(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Decr :: integer().
sub(Ref, Ix, Decr) ->
    ?MODULE:add(Ref, Ix, -Decr).

-spec sub_get(Ref, Ix, Decr) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Decr :: integer().
sub_get(Ref, Ix, Decr) ->
    ?MODULE:add_get(Ref, Ix, -Decr).

-spec exchange(Ref, Ix, Desired) -> integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Desired :: integer().
exchange(_Ref, _Ix, _Desired) ->
    erlang:nif_error(undef).

-spec compare_exchange(Ref, Ix, Expected, Desired) -> ok | integer() when
      Ref  :: atomics_ref(),
      Ix :: integer(),
      Expected :: integer(),
      Desired :: integer().
compare_exchange(_Ref, _Ix, _Expected, _Desired) ->
    erlang:nif_error(undef).

-spec info(Ref) -> Info when
      Ref  :: atomics_ref(),
      Info :: #{'size':=Size,'max':=Max,'min':=Min,'memory':=Memory},
      Size :: non_neg_integer(),
      Max :: integer(),
      Min :: integer(),
      Memory :: non_neg_integer().
info(_Ref) ->
    erlang:nif_error(undef).
