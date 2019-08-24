%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

-module(diameter_lib).
-compile({no_auto_import, [now/0]}).

-export([info_report/2,
         error_report/2,
         warning_report/2,
         now/0,
         timestamp/0,
         timestamp/1,
         now_diff/1,
         micro_diff/1,
         micro_diff/2,
         time/1,
         eval/1,
         eval_name/1,
         stacktrace/1,
         ipaddr/1,
         spawn_opts/2,
         wait/1,
         fold_n/3,
         for_n/2,
         log/4]).

%% ---------------------------------------------------------------------------
%% # stacktrace/1
%% ---------------------------------------------------------------------------

%% Return a stacktrace with a leading, potentially large, argument
%% list replaced by an arity. Trace on stacktrace/1 to see the
%% original.

stacktrace([{M,F,A,L} | T]) when is_list(A) ->
    [{M, F, length(A), L} | T];
stacktrace(L) ->
    L.

%% ---------------------------------------------------------------------------
%% # info_report/2
%% ---------------------------------------------------------------------------

-spec info_report(Reason :: term(), T :: term())
   -> true.

info_report(Reason, T) ->
    report(fun error_logger:info_report/1, Reason, T),
    true.

%% ---------------------------------------------------------------------------
%% # error_report/2
%% # warning_report/2
%% ---------------------------------------------------------------------------

-spec error_report(Reason :: term(), T :: term())
   -> false.

error_report(Reason, T) ->
    report(fun error_logger:error_report/1, Reason, T).

-spec warning_report(Reason :: term(), T :: term())
   -> false.

warning_report(Reason, T) ->
    report(fun error_logger:warning_report/1, Reason, T).

report(Fun, Reason, T) ->
    Fun(io_lib:format("diameter: ~" ++ fmt(Reason) ++ "~n    ~p~n",
                      [Reason, T])),
    false.

fmt(T) ->
    if is_list(T) ->
            "s";
       true ->
            "p"
    end.

%% ---------------------------------------------------------------------------
%% # now/0
%% ---------------------------------------------------------------------------

-spec now()
   -> integer().

now() ->
    erlang:monotonic_time().

%% ---------------------------------------------------------------------------
%% # timestamp/0
%% ---------------------------------------------------------------------------

-spec timestamp()
   -> erlang:timestamp().

timestamp() ->
    timestamp(now()).

%% ---------------------------------------------------------------------------
%% # timestamp/1
%% ---------------------------------------------------------------------------

-spec timestamp(integer())
   -> erlang:timestamp().

timestamp(MonoT) ->  %% monotonic time
    MicroSecs = monotonic_to_microseconds(MonoT + erlang:time_offset()),
    Secs = MicroSecs div 1000000,
    {Secs div 1000000, Secs rem 1000000, MicroSecs rem 1000000}.

monotonic_to_microseconds(MonoT) ->
    erlang:convert_time_unit(MonoT, native, micro_seconds).

%% ---------------------------------------------------------------------------
%% # now_diff/1
%% ---------------------------------------------------------------------------

-spec now_diff(T0 :: integer())
   -> {Hours, Mins, Secs, MicroSecs}
 when Hours :: non_neg_integer(),
      Mins  :: 0..59,
      Secs  :: 0..59,
      MicroSecs :: 0..999999.

%% Return time difference as an {H, M, S, MicroS} tuple instead of as
%% integer microseconds.

now_diff(T0) ->
    time(micro_diff(T0)).

%% ---------------------------------------------------------------------------
%% # micro_diff/1
%% ---------------------------------------------------------------------------

-spec micro_diff(T0 :: integer())
   -> MicroSecs
 when MicroSecs :: non_neg_integer().

micro_diff(T0) ->  %% monotonic time
    monotonic_to_microseconds(erlang:monotonic_time() - T0).

%% ---------------------------------------------------------------------------
%% # micro_diff/2
%% ---------------------------------------------------------------------------

-spec micro_diff(T1 :: integer(), T0 :: integer())
   -> MicroSecs
 when MicroSecs :: non_neg_integer().

micro_diff(T1, T0) ->  %% monotonic time
    monotonic_to_microseconds(T1 - T0).

%% ---------------------------------------------------------------------------
%% # time/1
%%
%% Return an elapsed time as an {H, M, S, MicroS} tuple.
%% ---------------------------------------------------------------------------

-spec time(Diff :: non_neg_integer())
   -> {Hours, Mins, Secs, MicroSecs}
 when Hours :: non_neg_integer(),
      Mins  :: 0..59,
      Secs  :: 0..59,
      MicroSecs :: 0..999999.

time(Micro) ->  %% elapsed time
    Seconds = Micro div 1000000,
    H = Seconds div 3600,
    M = (Seconds rem 3600) div 60,
    S = Seconds rem 60,
    {H, M, S, Micro rem 1000000}.

%% ---------------------------------------------------------------------------
%% # eval/1
%%
%% Evaluate a function in various forms.
%% ---------------------------------------------------------------------------

-type f() :: {module(), atom(), list()}
           | nonempty_maybe_improper_list(fun(), list())
           | fun().

-spec eval(Fun)
   -> term()
 when Fun :: f()
           | {f()}
           | nonempty_maybe_improper_list(f(), list()).

eval({M,F,A}) ->
    apply(M,F,A);

eval([{M,F,A} | X]) ->
    apply(M, F, X ++ A);

eval([[F|X] | A]) ->
    eval([F | A ++ X]);

eval([F|A]) ->
    apply(F,A);

eval({F}) ->
    eval(F);

eval(F) ->
    F().

%% ---------------------------------------------------------------------------
%% eval_name/1
%% ---------------------------------------------------------------------------

eval_name({M,F,A}) ->
    {M, F, length(A)};

eval_name([{M,F,A} | X]) ->
    {M, F, length(A) + length(X)};

eval_name([[F|A] | X]) ->
    eval_name([F | X ++ A]);

eval_name([F|_]) ->
    F;

eval_name({F}) ->
    eval_name(F);

eval_name(F) ->
    F.

%% ---------------------------------------------------------------------------
%% # ipaddr/1
%%
%% Parse an IP address.
%% ---------------------------------------------------------------------------

-spec ipaddr([byte()] | tuple())
   -> inet:ip_address()
    | none().

%% Don't convert lists of integers since a length 8 list like
%% [$1,$0,$.,$0,$.,$0,$.,$1] is ambiguous: is it "10.0.0.1" or
%% "49:48:46:48:46:48:46:49"?
%%
%% RFC 2373 defines the format parsed for v6 addresses.

%% Be brutal.
ipaddr(Addr) ->
    try
        ip(Addr)
    catch
        error: _: Stack ->
            erlang:error({invalid_address, Stack})
    end.

%% Already a tuple: ensure non-negative integers of the right size.
ip(T)
  when size(T) == 4;
       size(T) == 8 ->
    Bs = 2*size(T),
    [] = lists:filter(fun(N) when 0 =< N -> 0 < N bsr Bs end,
                      tuple_to_list(T)),
    T;

%% Or not: convert from '.'/':'-separated decimal/hex.
ip(Addr) ->
    {ok, A} = inet:parse_address(Addr),
    A.

%% ---------------------------------------------------------------------------
%% # spawn_opts/2
%% ---------------------------------------------------------------------------

-spec spawn_opts(server|worker, list())
   -> list().

spawn_opts(server, Opts) ->
    opts(75000, Opts);
spawn_opts(worker, Opts) ->
    opts(5000, Opts).

%% These setting are historical rather than useful. In particular, the
%% server setting can bloat many processes unnecessarily. Let them be
%% disabled with -diameter min_heap_size false.

opts(Def, Opts) ->
    Key = min_heap_size,
    case getenv(Key, Def) of
        N when is_integer(N), 0 =< N ->
            [{Key, N} | lists:keydelete(Key, 1, Opts)];
        _ ->
            Opts
    end.

%% getenv/1

getenv(Key, Def) ->
    case application:get_env(Key) of
        {ok, T} ->
            T;
        undefined ->
            Def
    end.

%% ---------------------------------------------------------------------------
%% # wait/1
%% ---------------------------------------------------------------------------

-spec wait([pid() | reference()])
   -> ok.

wait(L) ->
    lists:foreach(fun down/1, L).

down(Pid)
  when is_pid(Pid) ->
    down(monitor(process, Pid));

down(MRef)
  when is_reference(MRef) ->
    receive {'DOWN', MRef, process, _, _} = T -> T end.

%% ---------------------------------------------------------------------------
%% # fold_n/3
%% ---------------------------------------------------------------------------

-spec fold_n(F, Acc0, N)
   -> term()
 when F    :: fun((non_neg_integer(), term()) -> term()),
      Acc0 :: term(),
      N    :: non_neg_integer().

fold_n(F, Acc, N)
  when is_integer(N), 0 < N ->
    fold_n(F, F(N, Acc), N-1);

fold_n(_, Acc, _) ->
    Acc.

%% ---------------------------------------------------------------------------
%% # for_n/2
%% ---------------------------------------------------------------------------

-spec for_n(F, N)
   -> non_neg_integer()
 when F :: fun((non_neg_integer()) -> term()),
      N :: non_neg_integer().

for_n(F, N) ->
    fold_n(fun(M,A) -> F(M), A+1 end, 0, N).

%% ---------------------------------------------------------------------------
%% # log/4
%%
%% Called to have something to trace on for happenings of interest.
%% ---------------------------------------------------------------------------

log(_Slogan, _Mod, _Line, _Details) ->
    ok.
