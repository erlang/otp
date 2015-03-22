%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(diameter_lib).
-compile({no_auto_import, [now/0]}).

-export([info_report/2,
         error_report/2,
         warning_report/2,
         now/0,
         timestamp/1,
         now_diff/1,
         micro_diff/1,
         micro_diff/2,
         time/1,
         seed/0,
         eval/1,
         eval_name/1,
         get_stacktrace/0,
         ipaddr/1,
         spawn_opts/2,
         wait/1,
         fold_tuple/3,
         fold_n/3,
         for_n/2,
         log/4]).

%% ---------------------------------------------------------------------------
%% # get_stacktrace/0
%% ---------------------------------------------------------------------------

%% Return a stacktrace with a leading, potentially large, argument
%% list replaced by an arity. Trace on stacktrace/0 to see the
%% original.

get_stacktrace() ->
    stacktrace(erlang:get_stacktrace()).

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

-type timestamp() :: {non_neg_integer(), 0..999999, 0..999999}.
-type now() :: integer() %% monotonic time
             | timestamp().

-spec now()
   -> now().

%% Use monotonic time if it exists, fall back to erlang:now()
%% otherwise.

now() ->
    try
        erlang:monotonic_time() 
    catch
        error: undef -> erlang:now()
    end.

%% ---------------------------------------------------------------------------
%% # timestamp/1
%% ---------------------------------------------------------------------------

-spec timestamp(NowT :: now())
   -> timestamp().

timestamp({_,_,_} = T) ->  %% erlang:now()
    T;

timestamp(MonoT) ->  %% monotonic time
    MicroSecs = monotonic_to_microseconds(MonoT + erlang:time_offset()),
    Secs = MicroSecs div 1000000,
    {Secs div 1000000, Secs rem 1000000, MicroSecs rem 1000000}.

monotonic_to_microseconds(MonoT) ->
    erlang:convert_time_unit(MonoT, native, micro_seconds).

%% ---------------------------------------------------------------------------
%% # now_diff/1
%% ---------------------------------------------------------------------------

-spec now_diff(NowT :: now())
   -> {Hours, Mins, Secs, MicroSecs}
 when Hours :: non_neg_integer(),
      Mins  :: 0..59,
      Secs  :: 0..59,
      MicroSecs :: 0..999999.

%% Return timer:now_diff(now(), NowT) as an {H, M, S, MicroS} tuple
%% instead of as integer microseconds.

now_diff(Time) ->
    time(micro_diff(Time)).

%% ---------------------------------------------------------------------------
%% # micro_diff/1
%% ---------------------------------------------------------------------------

-spec micro_diff(NowT :: now())
   -> MicroSecs
 when MicroSecs :: non_neg_integer().

micro_diff({_,_,_} = T0) ->
    timer:now_diff(erlang:now(), T0);

micro_diff(T0) ->  %% monotonic time
    monotonic_to_microseconds(erlang:monotonic_time() - T0).

%% ---------------------------------------------------------------------------
%% # micro_diff/2
%% ---------------------------------------------------------------------------

-spec micro_diff(T1 :: now(), T0 :: now())
   -> MicroSecs
 when MicroSecs :: non_neg_integer().

micro_diff(T1, T0)
  when is_integer(T1), is_integer(T0) ->  %% monotonic time
    monotonic_to_microseconds(T1 - T0);

micro_diff(T1, T0) ->  %% at least one erlang:now()
    timer:now_diff(timestamp(T1), timestamp(T0)).

%% ---------------------------------------------------------------------------
%% # time/1
%%
%% Return an elapsed time as an {H, M, S, MicroS} tuple.
%% ---------------------------------------------------------------------------

-spec time(NowT | Diff)
   -> {Hours, Mins, Secs, MicroSecs}
 when NowT  :: timestamp(),
      Diff  :: non_neg_integer(),
      Hours :: non_neg_integer(),
      Mins  :: 0..59,
      Secs  :: 0..59,
      MicroSecs :: 0..999999.

time({_,_,_} = NowT) ->  %% time of day
    %% 24 hours = 24*60*60*1000000 = 86400000000 microsec
    time(timer:now_diff(NowT, {0,0,0}) rem 86400000000);

time(Micro) ->  %% elapsed time
    Seconds = Micro div 1000000,
    H = Seconds div 3600,
    M = (Seconds rem 3600) div 60,
    S = Seconds rem 60,
    {H, M, S, Micro rem 1000000}.

%% ---------------------------------------------------------------------------
%% # seed/0
%% ---------------------------------------------------------------------------

-spec seed()
   -> {timestamp(), {integer(), integer(), integer()}}.

%% Return an argument for random:seed/1.

seed() ->
    T = now(),
    {timestamp(T), seed(T)}.

%% seed/1

seed({_,_,_} = T) ->
    T;

seed(T) ->  %% monotonic time
    {erlang:phash2(node()), T, erlang:unique_integer()}.

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
        error: _ ->
            erlang:error({invalid_address, erlang:get_stacktrace()})
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
    {ok, A} = inet_parse:address(Addr),  %% documented in inet(3)
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

opts(HeapSize, Opts) ->
    [{min_heap_size, HeapSize} | lists:keydelete(min_heap_size, 1, Opts)].

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
%% # fold_tuple/3
%% ---------------------------------------------------------------------------

-spec fold_tuple(N, T0, T)
   -> tuple()
 when N  :: pos_integer(),
      T0 :: tuple(),
      T  :: tuple()
          | undefined.

%% Replace fields in T0 by those of T starting at index N, unless the
%% new value is 'undefined'.
%%
%% eg. fold_tuple(2, Hdr, #diameter_header{end_to_end_id = 42})

fold_tuple(_, T, undefined) ->
    T;

fold_tuple(N, T0, T1) ->
    {_, T} = lists:foldl(fun(V, {I,_} = IT) -> {I+1, ft(V, IT)} end,
                         {N, T0},
                         lists:nthtail(N-1, tuple_to_list(T1))),
    T.

ft(undefined, {_, T}) ->
    T;
ft(Value, {Idx, T}) ->
    setelement(Idx, T, Value).

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
