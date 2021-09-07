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

%% Purpose : Main counters API module.

-module(counters).

-export([new/2,
         get/2,
         add/3,
         sub/3,
         put/3,
         info/1]).

-export_type([counters_ref/0]).

-opaque counters_ref() :: {atomics, reference()} | {write_concurrency, reference()}.

%% We must inline this function so that the stacktrace points to
%% the correct function.
-compile({inline, [error_with_info/2]}).

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


-spec get(Ref, Ix) -> integer() when
      Ref  :: counters_ref(),
      Ix :: integer().
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


-spec add(Ref, Ix, Incr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
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


-spec sub(Ref, Ix, Decr) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
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


-spec put(Ref, Ix, Value) -> ok when
      Ref  :: counters_ref(),
      Ix :: integer(),
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
