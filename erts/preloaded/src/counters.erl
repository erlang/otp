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

-module(counters).

-export([new/2,
         get/2,
         add/3,
         sub/3,
         put/3,
         info/1]).

-export_type([counters_ref/0]).

-opaque counters_ref() :: {atomics, reference()} | {write_concurrency, reference()}.

-spec new(Size, Opts) -> counters_ref() when
      Size :: pos_integer(),
      Opts :: [Opt],
      Opt :: atomics | write_concurrency.
new(Size, [atomics]) ->
    {atomics, atomics:new(Size, [{signed, true}])};
new(Size, [write_concurrency]) ->
    {write_concurrency, erts_internal:counters_new(Size)};
new(Size, []) ->
    new(Size, [atomics]);
new(_, _) ->
    erlang:error(badarg).

-spec get(Ref, Ix) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer().
get({atomics,Ref}, Ix) ->
    atomics:get(Ref, Ix);
get({write_concurrency, Ref}, Ix) ->
    erts_internal:counters_get(Ref, Ix);
get(_, _) ->
    erlang:error(badarg).



-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Incr :: integer().
add({atomics, Ref}, Ix, Incr) ->
    atomics:add(Ref, Ix, Incr);
add({write_concurrency, Ref}, Ix, Incr) ->
    erts_internal:counters_add(Ref, Ix, Incr);
add(_, _, _) ->
    erlang:error(badarg).


-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Decr :: integer().
sub(Ref, Ix, Decr) ->
    add(Ref, Ix, -Decr).


-spec put(Ref, Ix, Value) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
      Value :: integer().
put({atomics, Ref}, Ix, Value) ->
    atomics:put(Ref, Ix, Value);
put({write_concurrency, Ref}, Ix, Value) ->
    erts_internal:counters_put(Ref, Ix, Value);
put(_, _, _) ->
    erlang:error(badarg).


-spec info(Ref) -> Info when
      Ref  :: counters_ref(),
      Info :: #{'size':=Size, 'memory':=Memory},
      Size :: non_neg_integer(),
      Memory :: non_neg_integer().
info({atomics, Ref}) ->
    atomics:info(Ref);
info({write_concurrency, Ref}) ->
    erts_internal:counters_info(Ref);
info(_) ->
    erlang:error(badarg).

