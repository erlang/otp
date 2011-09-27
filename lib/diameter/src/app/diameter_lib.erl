%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-export([report/2, info_report/2,
         error_report/2,
         warning_report/2,
         now_diff/1,
         time/1,
         eval/1,
         ip4address/1,
         ip6address/1,
         ipaddr/1,
         spawn_opts/2,
         wait/1,
         fold_tuple/3,
         log/4]).

-include("diameter_internal.hrl").

%% ---------------------------------------------------------------------------
%% # info_report(Reason, MFA)
%%
%% Input: Reason = Arbitrary term indicating the reason for the report.
%%        MFA    = {Module, Function, Args} to report.
%%
%% Output:  true
%% ---------------------------------------------------------------------------

report(Reason, MFA) ->
    info_report(Reason, MFA).

info_report(Reason, MFA) ->
    report(fun error_logger:info_report/1, Reason, MFA),
    true.

%%% ---------------------------------------------------------------------------
%%% # error_report(Reason, MFA)
%%% # warning_report(Reason, MFA)
%%%
%%% Output:  false
%%% ---------------------------------------------------------------------------

error_report(Reason, MFA) ->
    report(fun error_logger:error_report/1, Reason, MFA).

warning_report(Reason, MFA) ->
    report(fun error_logger:warning_report/1, Reason, MFA).

report(Fun, Reason, MFA) ->
    Fun([{why, Reason}, {who, self()}, {what, MFA}]),
    false.

%%% ---------------------------------------------------------------------------
%%% # now_diff(Time)
%%%
%%% Description: Return timer:now_diff(now(), Time) as an {H, M, S, MicroS}
%%%              tuple instead of as integer microseconds.
%%% ---------------------------------------------------------------------------

now_diff({_,_,_} = Time) ->
    time(timer:now_diff(erlang:now(), Time)).

%%% ---------------------------------------------------------------------------
%%% # time(Time)
%%%
%%% Input:  Time = {MegaSec, Sec, MicroSec}
%%%              | MicroSec
%%%
%%% Output: {H, M, S, MicroS}
%%% ---------------------------------------------------------------------------

time({_,_,_} = Time) ->  %% time of day
    %% 24 hours = 24*60*60*1000000 = 86400000000 microsec
    time(timer:now_diff(Time, {0,0,0}) rem 86400000000);

time(Micro) ->  %% elapsed time
    Seconds = Micro div 1000000,
    H = Seconds div 3600,
    M = (Seconds rem 3600) div 60,
    S = Seconds rem 60,
    {H, M, S, Micro rem 1000000}.

%%% ---------------------------------------------------------------------------
%%% # eval(Func)
%%% ---------------------------------------------------------------------------

eval({M,F,A}) ->
    apply(M,F,A);

eval([{M,F,A} | X]) ->
    apply(M, F, X ++ A);

eval([[F|A] | X]) ->
    eval([F | X ++ A]);

eval([F|A]) ->
    apply(F,A);

eval({F}) ->
    eval(F);

eval(F) ->
    F().

%%% ---------------------------------------------------------------------------
%%% # ip4address(Addr)
%%%
%%% Input:  string()   (eg. "10.0.0.1")
%%%         | list of integer()
%%%         | tuple of integer()
%%%
%%% Output: {_,_,_,_} of integer
%%%
%%% Exceptions: error: {invalid_address, Addr, erlang:get_stacktrace()}
%%% ---------------------------------------------------------------------------

ip4address([_,_,_,_] = Addr) -> %% Length 4 string can't be an address.
    ipaddr(list_to_tuple(Addr));

%% Be brutal.
ip4address(Addr) ->
    try
        {_,_,_,_} = ipaddr(Addr)
    catch
        error: _ ->
            erlang:error({invalid_address, Addr, ?STACK})
    end.

%%% ---------------------------------------------------------------------------
%%% # ip6address(Addr)
%%%
%%% Input:  string()   (eg. "1080::8:800:200C:417A")
%%%         | list of integer()
%%%         | tuple of integer()
%%%
%%% Output: {_,_,_,_,_,_,_,_} of integer
%%%
%%% Exceptions: error: {invalid_address, Addr, erlang:get_stacktrace()}
%%% ---------------------------------------------------------------------------

ip6address([_,_,_,_,_,_,_,_] = Addr) -> %% Length 8 string can't be an address.
    ipaddr(list_to_tuple(Addr));

%% Be brutal.
ip6address(Addr) ->
    try
        {_,_,_,_,_,_,_,_} = ipaddr(Addr)
    catch
        error: _ ->
            erlang:error({invalid_address, Addr, ?STACK})
    end.

%%% ---------------------------------------------------------------------------
%%% # ipaddr(Addr)
%%%
%%% Input:  string() | tuple of integer()
%%%
%%% Output: {_,_,_,_} | {_,_,_,_,_,_,_,_}
%%%
%%% Exceptions: error: {invalid_address, erlang:get_stacktrace()}
%%% ---------------------------------------------------------------------------

-spec ipaddr(string() | tuple())
   -> inet:ip_address().

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
            erlang:error({invalid_address, ?STACK})
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

%%% ---------------------------------------------------------------------------
%%% # spawn_opts(Type, Opts)
%%% ---------------------------------------------------------------------------

%% TODO: config variables.

spawn_opts(server, Opts) ->
    opts(75000, Opts);
spawn_opts(worker, Opts) ->
    opts(5000, Opts).

opts(HeapSize, Opts) ->
    [{min_heap_size, HeapSize} | lists:keydelete(min_heap_size, 1, Opts)].

%%% ---------------------------------------------------------------------------
%%% # wait(MRefs)
%%% ---------------------------------------------------------------------------

wait(L) ->
    w([erlang:monitor(process, P) || P <- L]).

w([]) ->
    ok;
w(L) ->
    receive
        {'DOWN', MRef, process, _, _} ->
            w(lists:delete(MRef, L))
    end.

%%% ---------------------------------------------------------------------------
%%% # fold_tuple(N, T0, T)
%%% ---------------------------------------------------------------------------

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

%%% ----------------------------------------------------------
%%% # log(Slogan, Mod, Line, Details)
%%%
%%% Called to have something to trace on for happenings of interest.
%%% ----------------------------------------------------------

log(_, _, _, _) ->
    ok.
