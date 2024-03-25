%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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

-module(exception_SUITE).

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
         badmatch/1, pending_errors/1, nil_arith/1, top_of_stacktrace/1,
         stacktrace/1, nested_stacktrace/1, raise/1, gunilla/1, per/1,
         change_exception_class/1,
         exception_with_heap_frag/1, backtrace_depth/1,
         error_3/1, error_info/1,
         no_line_numbers/1, line_numbers/1]).

-export([bad_guy/2]).
-export([crash/1]).

-include_lib("common_test/include/ct.hrl").
-import(lists, [foreach/2]).

%% Module-level type optimization propagates the constants used when testing
%% increment1/1 and increment2/1, which makes it test something completely
%% different, so we're turning it off.
-compile(no_module_opt).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [badmatch, pending_errors, nil_arith, top_of_stacktrace,
     stacktrace, nested_stacktrace, raise, gunilla, per,
     change_exception_class,
     exception_with_heap_frag, backtrace_depth,
     error_3, error_info,
     no_line_numbers, line_numbers].

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

-define(try_match(E),
        catch ?MODULE:bar(),
        {'EXIT', {{badmatch, nomatch}, _}} = (catch E = id(nomatch))).

%% Test that deliberately bad matches are reported correctly.

badmatch(Config) when is_list(Config) ->
    ?try_match(a),
    ?try_match(42),
    ?try_match({a, b, c}),
    ?try_match([]),
    ?try_match(1.0),
    ok.

%% Test various exceptions, in the presence of a previous error suppressed
%% in a guard.
pending_errors(Config) when is_list(Config) ->
    pending(e_badmatch, {badmatch, b}),
    pending(x, function_clause),
    pending(e_case, {case_clause, xxx}),
    pending(e_if, if_clause),
    pending(e_badarith, badarith),
    pending(e_undef, undef),
    pending(e_timeoutval, timeout_value),
    pending(e_badarg, badarg),
    pending(e_badarg_spawn, badarg),
    ok.

bad_guy(pe_badarith, Other) when Other+1 == 0 -> % badarith (suppressed)
    ok;
bad_guy(pe_badarg, Other) when length(Other) > 0 -> % badarg (suppressed)
    ok;
bad_guy(_, e_case) ->
    case id(xxx) of
        ok -> ok
    end;					% case_clause
bad_guy(_, e_if) ->
    if
        a == b -> ok
    end;					% if_clause
bad_guy(_, e_badarith) ->
    1+b;					% badarith
bad_guy(_, e_undef) ->
    non_existing_module:foo();			% undef
bad_guy(_, e_timeoutval) ->
    receive
    after arne ->				% timeout_value
              ok
    end;
bad_guy(_, e_badarg) ->
    node(xxx);					% badarg
bad_guy(_, e_badarg_spawn) ->
    spawn({}, {}, {});				% badarg
bad_guy(_, e_badmatch) ->
    a = id(b).					% badmatch

pending(Arg, Expected) ->
    pending(pe_badarith, Arg, Expected),
    pending(pe_badarg, Arg, Expected).

pending(First, Second, Expected) ->
    pending_catched(First, Second, Expected),
    pending_exit_message([First, Second], Expected).

pending_catched(First, Second, Expected) ->
    ok = io:format("Catching bad_guy(~p, ~p)", [First, Second]),
    case catch bad_guy(First, Second) of
        {'EXIT', Reason} ->
            pending(Reason, bad_guy, [First, Second], Expected);
        Other ->
            ct:fail({not_exit, Other})
    end.

pending_exit_message(Args, Expected) ->
    ok = io:format("Trapping EXITs from spawn_link(~p, ~p, ~p)",
                   [?MODULE, bad_guy, Args]),
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, bad_guy, Args),
    receive
        {'EXIT', Pid, Reason} ->
            pending(Reason, bad_guy, Args, Expected);
        Other ->
            ct:fail({unexpected_message, Other})
    after 10000 ->
              ct:fail(timeout)
    end,
    process_flag(trap_exit, false).

pending({badarg,[{erlang,Bif,BifArgs,Loc1},
                 {?MODULE,Func,Arity,Loc2}|_]},
        Func, Args, _Code)
  when is_atom(Bif), is_list(BifArgs), length(Args) =:= Arity,
       is_list(Loc1), is_list(Loc2) ->
    ok;
pending({badarith,[{erlang,Bif,BifArgs,Loc1},
                   {?MODULE,Func,Arity,Loc2}|_]},
        Func, Args, _Code)
  when is_atom(Bif), is_list(BifArgs), length(Args) =:= Arity,
       is_list(Loc1), is_list(Loc2) ->
    ok;
pending({undef,[{non_existing_module,foo,[],Loc}|_]}, _, _, _)
  when is_list(Loc) ->
    ok;
pending({function_clause,[{?MODULE,Func,Args,Loc}|_]}, Func, Args, _Code)
  when is_list(Loc) ->
    ok;
pending({Code,[{?MODULE,Func,Arity,Loc}|_]}, Func, Args, Code)
  when length(Args) =:= Arity, is_list(Loc) ->
    ok;
pending(Reason, _Function, _Args, _Code) ->
    ct:fail({bad_exit_reason,Reason}).

%% Test that doing arithmetic on [] gives a badarith EXIT and not a crash.

nil_arith(Config) when is_list(Config) ->
    ba_plus_minus_times([], []),

    ba_plus_minus_times([], 0),
    ba_plus_minus_times([], 42),
    ba_plus_minus_times([], 38724978123478923784),
    ba_plus_minus_times([], 38.72),

    ba_plus_minus_times(0, []),
    ba_plus_minus_times(334, []),
    ba_plus_minus_times(387249797813478923784, []),
    ba_plus_minus_times(344.22, []),

    ba_div_rem([], []),

    ba_div_rem([], 0),
    ba_div_rem([], 1),
    ba_div_rem([], 42),
    ba_div_rem([], 38724978123478923784),
    ba_div_rem(344.22, []),

    ba_div_rem(0, []),
    ba_div_rem(1, []),
    ba_div_rem(334, []),
    ba_div_rem(387249797813478923784, []),
    ba_div_rem(344.22, []),

    ba_div_rem(344.22, 0.0),
    ba_div_rem(1, 0.0),
    ba_div_rem(392873498733971, 0.0),

    ba_bop([], []),
    ba_bop(0, []),
    ba_bop(42, []),
    ba_bop(-42342742987343, []),
    ba_bop(238.342, []),
    ba_bop([], 0),
    ba_bop([], -243),
    ba_bop([], 243),
    ba_bop([], 2438724982478933),
    ba_bop([], 3987.37),

    ba_bnot([]),
    ba_bnot(23.33),

    ba_shift([], []),
    ba_shift([], 0),
    ba_shift([], 4),
    ba_shift([], -4),
    ba_shift([], 2343333333333),
    ba_shift([], -333333333),
    ba_shift([], 234.00),
    ba_shift(23, []),
    ba_shift(0, []),
    ba_shift(-3433443433433323, []),
    ba_shift(433443433433323, []),
    ba_shift(343.93, []),
    ok.

ba_plus_minus_times(A, B) ->
    io:format("~p + ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A + B),
    io:format("~p - ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A - B),
    io:format("~p * ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A * B).

ba_div_rem(A, B) ->
    io:format("~p / ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A / B),
    io:format("~p div ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A div B),
    io:format("~p rem ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A rem B).

ba_bop(A, B) ->
    io:format("~p band ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A band B),
    io:format("~p bor ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A bor B),
    io:format("~p bxor ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A bxor B).

ba_shift(A, B) ->
    io:format("~p bsl ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A bsl B),
    io:format("~p bsr ~p", [A, B]),
    {'EXIT', {badarith, _}} = (catch A bsr B).

ba_bnot(A) ->
    io:format("bnot ~p", [A]),
    {'EXIT', {badarith, _}} = (catch bnot A).

%% Test that BIFs are added to the top of the stacktrace.

top_of_stacktrace(Conf) when is_list(Conf) ->
    %% Arithmetic operators
    {'EXIT', {badarith, [{erlang, '+', [1, ok], _} | _]}} = (catch my_add(1, ok)),
    {'EXIT', {badarith, [{erlang, '-', [1, ok], _} | _]}} = (catch my_minus(1, ok)),
    {'EXIT', {badarith, [{erlang, '*', [1, ok], _} | _]}} = (catch my_times(1, ok)),
    {'EXIT', {badarith, [{erlang, 'div', [1, ok], _} | _]}} = (catch my_div(1, ok)),
    {'EXIT', {badarith, [{erlang, 'div', [1, 0], _} | _]}} = (catch my_div(1, 0)),
    {'EXIT', {badarith, [{erlang, 'rem', [1, ok], _} | _]}} = (catch my_rem(1, ok)),
    {'EXIT', {badarith, [{erlang, 'rem', [1, 0], _} | _]}} = (catch my_rem(1, 0)),

    %% Bit operators
    {'EXIT', {badarith, [{erlang, 'band', [1, ok], _} | _]}} = (catch my_band(1, ok)),
    {'EXIT', {badarith, [{erlang, 'bor', [1, ok], _} | _]}} = (catch my_bor(1, ok)),
    {'EXIT', {badarith, [{erlang, 'bsl', [1, ok], _} | _]}} = (catch my_bsl(1, ok)),
    {'EXIT', {badarith, [{erlang, 'bsr', [1, ok], _} | _]}} = (catch my_bsr(1, ok)),
    {'EXIT', {badarith, [{erlang, 'bxor', [1, ok], _} | _]}} = (catch my_bxor(1, ok)),
    {'EXIT', {badarith, [{erlang, 'bnot', [ok], _} | _]}} = (catch my_bnot(ok)),

    %% element/2
    {'EXIT', {badarg, [{erlang, element, [1, ok], _} | _]}} = (catch my_element(1, ok)),
    {'EXIT', {badarg, [{erlang, element, [ok, {}], _} | _]}} = (catch my_element(ok, {})),
    {'EXIT', {badarg, [{erlang, element, [1, {}], _} | _]}} = (catch my_element(1, {})),
    {'EXIT', {badarg, [{erlang, element, [0, {a}], _} | _]}} = (catch my_element(0, {a})),
    {'EXIT', {badarg, [{erlang, element, [-1, {z}], _} | _]}} = (catch my_element(-1, {z})),
    {'EXIT', {badarg, [{erlang, element, [1, {}], _} | _]}} = (catch element(1, erlang:make_tuple(0, ok))),

    %% tuple_size/1
    {'EXIT', {badarg, [{erlang, tuple_size, [ok], _} | _]}} = (catch my_tuple_size(ok)),
    {'EXIT', {badarg, [{erlang, tuple_size, [[a,b,c]], _} | _]}} = (catch my_tuple_size([a,b,c])),

    %% Lists
    {'EXIT', {badarg, [{erlang, hd, [[]], _} | _]}} = (catch my_hd([])),
    {'EXIT', {badarg, [{erlang, hd, [42], _} | _]}} = (catch my_hd(42)),
    {'EXIT', {badarg, [{erlang, tl, [[]], _} | _]}} = (catch my_tl([])),
    {'EXIT', {badarg, [{erlang, tl, [a], _} | _]}} = (catch my_tl(a)),

    %% System limits
    Maxbig = maxbig(),
    MinusMaxbig = -Maxbig,
    {'EXIT', {system_limit, [{erlang, '+', [Maxbig, 1], _} | _]}} = (catch my_add(Maxbig, 1)),
    {'EXIT', {system_limit, [{erlang, '+', [Maxbig, 1], _} | _]}} = (catch my_add(maxbig_gc(), 1)),
    {'EXIT', {system_limit, [{erlang, '-', [MinusMaxbig, 1], _} | _]}} = (catch my_minus(-Maxbig, 1)),
    {'EXIT', {system_limit, [{erlang, '-', [MinusMaxbig, 1], _} | _]}} = (catch my_minus(-maxbig_gc(), 1)),
    {'EXIT', {system_limit, [{erlang, '*', [Maxbig, 2], _} | _]}} = (catch my_times(Maxbig, 2)),
    {'EXIT', {system_limit, [{erlang, '*', [Maxbig, 2], _} | _]}} = (catch my_times(maxbig_gc(), 2)),
    {'EXIT', {system_limit, [{erlang, 'bnot', [Maxbig], _} | _]}} = (catch my_bnot(Maxbig)),
    {'EXIT', {system_limit, [{erlang, 'bnot', [Maxbig], _} | _]}} = (catch my_bnot(maxbig_gc())),
    ok.

maxbig() ->
    erlang:system_info(max_integer).

maxbig_gc() ->
    Maxbig = maxbig(),
    erlang:garbage_collect(),
    Maxbig.

stacktrace(Conf) when is_list(Conf) ->
    V = [make_ref()|self()],
    {value2,{caught1,badarg,[{erlang,abs,[V],_}|_]}} =
        stacktrace_1({'abs',V}, error, {value,V}),
    {caught2,{error,badarith},[{erlang,'+',[0,a],_},{?MODULE,my_add,2,_}|_]} =
        stacktrace_1({'div',{1,0}}, error, {'add',{0,a}}),
    {caught2,{error,{try_clause,V}},[{?MODULE,stacktrace_1,3,_}|_]} =
        stacktrace_1({value,V}, error, {value,V}),
    {caught2,{throw,V},[{?MODULE,foo,1,_}|_]} =
        stacktrace_1({value,V}, error, {throw,V}),
    {caught2,{error,V},[{?MODULE,foo,[a],_}|_]} =
        stacktrace_1({value,V}, error, {error,{V,[a]}}),
    {caught2,{error,V},[{?MODULE,foo,1,_}|_]} =
        stacktrace_1({value,V}, error, {error,{V,none}}),

    try
        stacktrace_2()
    catch
        error:{badmatch,_}:Stk ->
            [{?MODULE,stacktrace_2,0,_},
             {?MODULE,stacktrace,1,_}|_] = Stk,
            ok
    end.

stacktrace_1(X, C1, Y) ->
    try try foo(X) of
            C1 -> value1
        catch
            C1:D1:Stk1 ->
                {caught1,D1,Stk1}
        after
            foo(Y)
        end of
        V2 -> {value2,V2}
    catch
        C2:D2:Stk2 ->
            {caught2,{C2,D2},Stk2}
    after
        ok
    end.

stacktrace_2() ->
    ok = erlang:process_info(self(), current_function),
    ok.


nested_stacktrace(Conf) when is_list(Conf) ->
    V = [{make_ref()}|[self()]],
    value1 =
        nested_stacktrace_1({{value,{V,x1}},void,{V,x1}},
                            {void,void,void}),
    {caught1,
     [{erlang,'+',[V,x1],_},{?MODULE,my_add,2,_}|_],
     value2} =
        nested_stacktrace_1({{'add',{V,x1}},error,badarith},
                            {{value,{V,x2}},void,{V,x2}}),
    {caught1,
     [{erlang,'+',[V,x1],_},{?MODULE,my_add,2,_}|_],
     {caught2,[{erlang,abs,[V],_}|_]}} =
        nested_stacktrace_1({{'add',{V,x1}},error,badarith},
                            {{'abs',V},error,badarg}),
    ok.

nested_stacktrace_1({X1,C1,V1}, {X2,C2,V2}) ->
    try foo(X1) of
        V1 -> value1
    catch
        C1:V1:S1 ->
            T2 = try foo(X2) of
                     V2 -> value2
                 catch
                     C2:V2:S2 -> {caught2,S2}
            end,
            {caught1,S1,T2}
    end.



raise(Conf) when is_list(Conf) ->
    erase(raise),
    A =
        try
            try foo({'div',{1,0}})
            catch
                error:badarith:A0 ->
                    put(raise, A0),
                    erlang:raise(error, badarith, A0)
            end
        catch
            error:badarith:A1 ->
                A1 = get(raise)
        end,
    A = get(raise),
    [{erlang,'div',[1, 0], _},{?MODULE,my_div,2,_}|_] = A,
    %%
    N = 8, % Must be even
    N = erlang:system_flag(backtrace_depth, N),
    try
        even(N)
    catch
        error:function_clause -> ok
    end,
    %%
    C = odd_even(N+1, []),
    try
        odd(N+1)
    catch
        error:function_clause -> ok
    end,
    try
        erlang:raise(error, function_clause, C)
    catch
        error:function_clause -> ok
    end,
    ok.

odd_even(N, R) when is_integer(N), N > 1 ->
    odd_even(N-1, 
             [if (N rem 2) == 0 ->
                     {?MODULE,even,1,[{file,"odd_even.erl"},{line,3}]};
                 true ->
                     {?MODULE,odd,1,[{file,"odd_even.erl"},{line,6}]}
              end|R]);
odd_even(1, R) ->
    [{?MODULE,odd,[1],[{file,"odd_even.erl"},{line,5}]}|R].

foo({value,Value}) -> Value;
foo({'div',{A,B}}) ->
    my_div(A, B);
foo({'add',{A,B}}) ->
    my_add(A, B);
foo({'abs',X}) ->
    my_abs(X);
foo({error,{Error, Args}}) ->
    erlang:error(Error, Args);
foo({error,Error}) ->
    erlang:error(Error);
foo({throw,Throw}) ->
    erlang:throw(Throw);
foo({exit,Exit}) ->
    erlang:exit(Exit);
foo({raise,{Class,Reason,Stacktrace}}) ->
    erlang:raise(Class, Reason, Stacktrace).
%%foo(function_clause) -> % must not be defined!

my_add(A, B) -> A + B.
my_minus(A, B) -> A - B.
my_times(A, B) -> A * B.
my_div(A, B) -> A div B.
my_rem(A, B) -> A rem B.

my_band(A, B) -> A band B.
my_bor(A, B) -> A bor B.
my_bsl(A, B) -> A bsl B.
my_bsr(A, B) -> A bsr B.
my_bxor(A, B) -> A bxor B.
my_bnot(A) -> bnot A.

my_element(A, B) -> element(A, B).

my_tuple_size(A) -> tuple_size(A).

my_abs(X) -> abs(X).

my_hd(L) -> hd(L).
my_tl(L) -> tl(L).

gunilla(Config) when is_list(Config) ->
    {throw,kalle} = gunilla_1(),
    ok.

gunilla_1() ->
    try try arne()
        after
            pelle
        end
    catch
        C:R ->
            {C,R}
    end.

arne() ->
    %% Empty stack trace used to cause change the error class to 'error'.
    erlang:raise(throw, kalle, []).

per(Config) when is_list(Config) ->
    try 
        t1(0,pad,0),
        t2(0,pad,0)
    catch
        error:badarith ->
            ok
    end.

t1(_,X,_) ->
    (1 bsl X) + 1.

t2(_,X,_) ->
    (X bsl 1) + 1.

change_exception_class(_Config) ->
    try
        change_exception_class_1(fun() -> throw(arne) end)
    catch
        error:arne ->
            ok;
        Class:arne ->
            ct:fail({wrong_exception_class,Class})
    end.

change_exception_class_1(F) ->
    try
        change_exception_class_2(F)
    after
        %% The exception would be caught and rethrown using
        %% an i_raise instruction. Before the correction
        %% of the raw_raise instruction, the change of class
        %% would not stick.
        io:put_chars("Exception automatically rethrown here\n")
    end.

change_exception_class_2(F) ->
    try
        F()
    catch
        throw:Reason:Stack ->
            %% Translated to a raw_raise instruction.
            %% The change of exception class would not stick
            %% if the i_raise instruction was later executed.
            erlang:raise(error, Reason, Stack)
    end.

%%
%% Make sure that even if a BIF builds an heap fragment, then causes an exception,
%% the stacktrace term will still be OK (specifically, that it does not contain
%% stale pointers to the arguments).
%%
exception_with_heap_frag(Config) when is_list(Config) ->
    Sizes = lists:seq(0, 512),

    %% Floats are only validated when the heap fragment has been allocated.
    BadFloat = <<131,99,53,46,48,$X,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,101,45,48,49,0,0,0,0,0>>,
    do_exception_with_heap_frag(BadFloat, Sizes),

    %% {Binary,BadFloat}: When the error in float is discovered, a refc-binary
    %% has been allocated and the list of refc-binaries goes through the
    %% heap fragment.
    BinAndFloat = 
    <<131,104,2,109,0,0,1,0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
      21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
      46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,
      71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,
      96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
      116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,
      135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,
      154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,
      173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
      192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,
      211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,
      230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,
      249,250,251,252,253,254,255,99,51,46,49,52,$B,$l,$u,$r,$f,48,48,48,48,48,48,
      48,48,49,50,52,51,52,101,43,48,48,0,0,0,0,0>>,
    do_exception_with_heap_frag(BinAndFloat, Sizes),

    %% {Fun,BadFloat}
    FunAndFloat =
    <<131,104,2,112,0,0,0,66,0,238,239,135,138,137,216,89,57,22,111,52,126,16,84,
      71,8,0,0,0,0,0,0,0,0,100,0,1,116,97,0,98,5,175,169,123,103,100,0,13,110,111,
      110,111,100,101,64,110,111,104,111,115,116,0,0,0,41,0,0,0,0,0,99,50,46,55,48,
      $Y,57,57,57,57,57,57,57,57,57,57,57,57,57,54,52,52,55,101,43,48,48,0,0,0,0,0>>,
    do_exception_with_heap_frag(FunAndFloat, Sizes),

    %% [ExternalPid|BadFloat]
    ExtPidAndFloat =
    <<131,108,0,0,0,1,103,100,0,13,107,97,108,108,101,64,115,116,114,105,100,101,
      114,0,0,0,36,0,0,0,0,2,99,48,46,$@,48,48,48,48,48,48,48,48,48,48,48,48,48,48,
      48,48,48,48,48,101,43,48,48,0,0,0,0,0>>,
    do_exception_with_heap_frag(ExtPidAndFloat, Sizes),

    ok.

do_exception_with_heap_frag(Bin, [Sz|Sizes]) ->
    Filler = erlang:make_tuple(Sz, a),
    spawn(fun() ->
                  try
                      binary_to_term(Bin)
                  catch
                      _:_:Stk ->
                          %% term_to_binary/1 is an easy way to traverse the
                          %% entire stacktrace term to make sure that every part
                          %% of it is OK.
                          term_to_binary(Stk)
                  end,
                  id(Filler)
          end),
    do_exception_with_heap_frag(Bin, Sizes);
do_exception_with_heap_frag(_, []) -> ok.

backtrace_depth(Config) when is_list(Config) ->
    _ = [do_backtrace_depth(D) || D <- lists:seq(0, 8)],
    ok.

do_backtrace_depth(D) ->
    Old = erlang:system_flag(backtrace_depth, D),
    try
        Expected = max(1, D),
        do_backtrace_depth_1(Expected)
    after
        _ = erlang:system_flag(backtrace_depth, Old)
    end.

do_backtrace_depth_1(D) ->
    Exit = fun() ->
                   error(reason)
           end,
    HandCrafted = fun() ->
                          {'EXIT',{_,Stk0}} = (catch error(get_stacktrace)),
                          %% Fool the compiler to force a hand-crafted
                          %% stacktrace.
                          Stk = [hd(Stk0)|tl(Stk0)],
                          erlang:raise(error, reason, Stk)
                  end,
    PassedOn = fun() ->
                       try error(get_stacktrace)
                       catch error:_:Stk ->
                               %% Just pass on the given stacktrace.
                               erlang:raise(error, reason, Stk)
                       end
               end,
    do_backtrace_depth_2(D, Exit),
    do_backtrace_depth_2(D, HandCrafted),
    do_backtrace_depth_2(D, PassedOn),
    ok.

do_backtrace_depth_2(D, Exc) ->
    try
        Exc()
    catch
        error:reason:Stk ->
            if
                length(Stk) =/= D ->
                    io:format("Expected depth: ~p\n", [D]),
                    io:format("~p\n", [Stk]),
                    error(bad_depth);
                true ->
                    ok
            end
    end.

error_3(Config) ->
    {error_info,#{}} = do_error_3({some,reason}, [Config], [{error_info,#{}}]),
    {error_info,#{cause:=whatever}} = do_error_3({some,reason}, [Config],
                                                 [{error_info,#{cause=>whatever}}]),

    false = do_error_3({some,reason}, [Config], []),
    false = do_error_3({some,reason}, [Config], [{error_info,not_map}]),
    false = do_error_3({some,reason}, [Config], [{bad,tuple}]),
    false = do_error_3({some,reason}, [Config], [bad]),

    ok.

do_error_3(Reason, Args, Options) ->
    {'EXIT',{Reason,[{?MODULE,?FUNCTION_NAME,Args,ExtraInfo}|_]}} = catch error(Reason, Args, Options),
    lists:keyfind(error_info, 1, ExtraInfo).

error_info(_Config) ->
    DeadProcess = dead_process(),
    {NewAtom,NewAtomExt} = non_existing_atom(),
    Eons = 1 bsl 50,

    %% We'll need an incorrect memory type for erlang:memory/1. We want to test an
    %% incorrect atom if our own allocators are enabled, but if they are disabled,
    %% we'll make do with a term that is obviously incorrect.
    BadMemoryType = try erlang:memory() of
                        _ -> whatever
                    catch
                        error:notsup -> 999
                    end,

    %% Pick up external pid and port.
    {ok, Peer, ExternalNode} = ?CT_PEER(),
    ExternalPid = rpc:call(ExternalNode, erlang, whereis, [code_server]),
    ExternalPort = hd(rpc:call(ExternalNode, erlang, ports, [])),

    L = [{abs, [abc]},
         {adler32, [{bad,data}]},
         {adler32, [old, new]},
         {adler32, [1 bsl 48, new]},
         {adler32_combine, [a, b, c]},
         {adler32_combine, [1 bsl 48, 1 bsl 48, bad_size]},
         {adler32_combine, [1 bsl 48, 1 bsl 48, -1]},

         {alias, [bad_options]},

         {alloc_info, 1},                       %Internal BIF.
         {alloc_sizes, 1},                      %Internal BIF.

         {append, [a, b]},
         {append_element, [no_tuple, any]},

         %% apply/2 and apply/3 are magic.
         {apply, 2},
         {apply, 3},

         {atom_to_binary, ["abc"]},
         {atom_to_binary, ["abc",latin1]},
         {atom_to_binary, ['земля',latin1]},
         {atom_to_binary, [xyz,utf42]},

         {atom_to_list, [{a,b,c}]},

         {binary_part, [<<>>, bad]},
         {binary_part, [bad, bad]},
         {binary_part, [<<1,2,3>>, {3,4}]},
         {binary_part, [<<1,2,3>>, {3,4,5}]},
         {binary_part, [bad, 3, 4]},
         {binary_part, [<<1,2,3>>, 4, 4]},

         {binary_to_atom, [abc]},
         {binary_to_atom, [<<128,128,255>>]},

         {binary_to_atom, [<<0:512/unit:8>>, latin1]},
         {binary_to_atom, [abc, latin1]},
         {binary_to_atom, [<<128,128,255>>, utf8]},
         {binary_to_atom, [<<"abc">>, utf42]},

         {binary_to_existing_atom, [abc]},
         {binary_to_existing_atom, [<<128,128,255>>]},
         {binary_to_existing_atom, [abc, latin1]},
         {binary_to_existing_atom, [<<128,128,255>>,utf8]},
         {binary_to_existing_atom, [list_to_binary(NewAtom), latin1]},
         {binary_to_existing_atom, [list_to_binary(NewAtom), utf8]},
         {binary_to_existing_atom, [list_to_binary(NewAtom), utf42]},
         {binary_to_existing_atom, [[<<"abc">>], utf8]},
         {binary_to_existing_atom, [<<0:512/unit:8>>, latin1]},

         {binary_to_float, [abc]},
         {binary_to_float, [<<"0">>]},
         {binary_to_float, [<<"abc">>]},
         {binary_to_integer, [abc]},
         {binary_to_integer, [<<"abc">>]},

         {binary_to_integer, [abc, 100]},
         {binary_to_integer, [<<"abc">>, 100]},

         {binary_to_list, [<<0:4>>]},
         {binary_to_list, [abc]},

         {binary_to_list, [abc, x, y]},
         {binary_to_list, [<<1,2,3>>, 3, 2]},
         {binary_to_list, [<<1,2,3>>, 1, 4]},
         {binary_to_list, [<<1,2,3>>, 4, 5]},

         {binary_to_term, [abc]},
         {binary_to_term, [<<"bad">>]},

         {binary_to_term, [term_to_binary(a), abc]},
         {binary_to_term, [<<"bad">>, abc]},
         {binary_to_term, [<<"bad">>, [bad]]},
         {binary_to_term, [<<"bad">>, []]},
         {binary_to_term, [<<"bad">>, [safe]]},
         {binary_to_term, [NewAtomExt, [safe]]},

         {bit_size, [abc]},
         {bitstring_to_list, [abc]},
         {bump_reductions, [abc]},
         {bump_reductions, [-1]},
         {byte_size, [abc]},
         {call_on_load_function, 1},

         {cancel_timer, [abc]},
         {cancel_timer, [abc, bad]},
         {cancel_timer, [abc, [bad]]},
         {cancel_timer, [make_ref(), [bad]]},

         {ceil, [abc]},

         {check_old_code, [{a,b,c}]},

         {check_process_code, [self(), {no,module}]},
         {check_process_code, [ExternalPid, code_server]},
         {check_process_code, [no_pid, {no,module}]},
         {check_process_code, [self(), abc, bad_option_list]},
         {check_process_code, [self(), abc, [abc]]},
         {check_process_code, [a, self(), [abc]]},

         {convert_time_unit, [a, b, c]},

         {crc32, [{bad,data}]},
         {crc32, [old, new]},
         {crc32, [1 bsl 48, new]},
         {crc32_combine, [a, b, c]},
         {crc32_combine, [1 bsl 48, 1 bsl 48, bad_size]},
         {crc32_combine, [1 bsl 48, 1 bsl 48, -1]},

         {decode_packet, [xyz, not_binary, not_list]},
         {decode_packet, [xyz, <<>>, []]},
         {decode_packet, [xyz, not_binary, [bad_option]]},
         {decode_packet, [asn1, <<"abc">>, [bad_option]]},

         {delay_trap, 2},                       %Internal BIF.

         {delete_element, [0,{a,b,c}]},
         {delete_element, [99,{a,b,c}]},
         {delete_element, [not_integer,{a,b,c}]},
         {delete_element, [1,not_tuple]},
         {delete_element, [a,b]},

         {delete_module, [{a,b,c}]},

         {dmonitor_node, 3},                    %Internal BIF.

         {demonitor, [abc]},
         {demonitor, [abc, bad]},
         {demonitor, [abc, [bad]]},
         {demonitor, [make_ref(), [bad]]},

         {disconnect_node, 1},                  %Never fails.

         {display, ["test erlang:display/1"], [no_fail]},
         {display_string, [{a,b,c}]},
         {display_string, [standard_out,"test erlang:display/2"]},
         {display_string, [stdout,{a,b,c}]},

         %% Internal undcoumented BIFs.
         {dist_ctrl_get_data, 1},
         {dist_ctrl_get_data_notification, 1},
         {dist_ctrl_get_opt, 2},
         {dist_ctrl_input_handler, 2},
         {dist_ctrl_put_data, 2},
         {dist_ctrl_set_opt, 3},
         {dist_get_stat, 1},
         {dt_append_vm_tag_data, 1},
         {dt_prepend_vm_tag_data, 1},
         {dt_put_tag, 1},
         {dt_restore_tag, 1},
         {dt_spread_tag, 1},

         {element, [0,{a,b,c}]},
         {element, [99,{a,b,c}]},
         {element, [not_integer,{a,b,c}]},
         {element, [1,not_tuple]},
         {element, [a,b]},

         {erase, [abc], [no_fail]},
         {error, 1},
         {error, 2},
         {error, 3},
         {exit, 1},

         {exit, [a, b]},
         {exit_signal, [a, b]},

         {external_size, [a], [no_fail]},
         {external_size, [abc, xyz]},

         %% Internal undocumented BIF.
         {finish_after_on_load, 2},
         {finish_loading, 1},

         {float, [abc]},
         {float_to_binary, [abc]},
         {float_to_binary, [abc, bad_options]},
         {float_to_list, [abc]},
         {float_to_list, [abc, bad_options]},
         {floor, [abc]},

         {format_cpu_topology, 1},              %Internal BIF.

         {fun_info, [abc]},
         {fun_info, [abc, {bad,item}]},
         {fun_info_mfa, [abc]},
         {fun_to_list, [abc]},
         {function_exported, [1,2,abc]},

         {garbage_collect, [not_a_pid]},
         {garbage_collect, [ExternalPid]},
         {garbage_collect, [not_a_pid, bad_option_list]},
         {garbage_collect, [ExternalPid, bad_option_list]},

         {gather_gc_info_result, 1},            %Internal BIF.

         {get_cookie, [{not_node}]},
         {get_keys, [value], [no_fail]},
         {get_module_info, 1},
         {get_module_info, 2},

         {group_leader, [not_pid, self()]},
         {group_leader, [self(), not_pid]},

         {halt, [{bad,exit,status}]},
         {halt, [a, []]},
         {halt, [a, b]},
         {halt, [1, [bad_option]]},

         {has_prepared_code_on_load, 1},
         {hd, [abc]},
         {hibernate, [1,2,a]},
         {insert_element, [a, b, c]},
         {insert_element, [0, b, c]},
         {integer_to_binary, [42.0]},
         {integer_to_binary, [42, 100]},
         {integer_to_binary, [42.0, 100]},
         {integer_to_list, [abc]},
         {integer_to_list, [abc, 100]},
         {iolist_size, [abc]},
         {iolist_to_binary, [abc]},
         {iolist_to_iovec, [abc]},
         {is_builtin, [1, 2, a]},
         {is_function, [abc, bad_arity]},
         {is_function, [abc, -1]},
         {is_map_key, [key, not_map]},
         {is_process_alive, [abc]},
         {is_record, [not_tuple,42]},
         {is_record, [not_tuple,42,bad_size]},
         {length, [abc]},
         {link, [42]},
         {link, [DeadProcess]},
         {list_to_atom, [42]},
         {list_to_atom, [lists:duplicate(1024, $a)]},
         {list_to_binary, [42]},
         {list_to_bitstring, [[a,b,c]]},

         {list_to_existing_atom, [abc]},
         {list_to_existing_atom, [[a,b,c]]},
         {list_to_existing_atom, [[["abc"]]]},
         {list_to_existing_atom, [NewAtom]},
         {list_to_existing_atom, [[a|b]]},

         {list_to_float, ["abc"]},
         {list_to_float, [abc]},

         {list_to_integer, ["abc"]},
         {list_to_integer, [[a,b,c]]},
         {list_to_integer, [[a|b]]},
         {list_to_integer, [abc]},

         {list_to_integer, ["abc",10]},
         {list_to_integer, [[a,b,c],10]},
         {list_to_integer, [[a|b],abc]},
         {list_to_integer, [abc,10]},
         {list_to_integer, ["42",abc]},

         {list_to_pid, ["pid"]},
         {list_to_pid, [42]},

         {list_to_port, ["port"]},
         {list_to_port, [port]},

         {list_to_ref, ["ref"]},
         {list_to_ref, [ref]},

         {list_to_tuple, [abc]},

         {load_module, [{no,module}, <<1,2,3>>]},
         {load_module, [good_module_name, not_binary]},

         {load_nif, [{bad,path}, any]},

         {localtime_to_universaltime, [bad_date]},
         {localtime_to_universaltime, [{{2020, 3, 12}, {12, 0, 0}}, not_boolean]},
         {localtime_to_universaltime, [bad_date, not_boolean]},

         {make_fun, [0,1,-1]},
         {make_fun, [0,1,abc]},

         {make_tuple, [a, element]},
         {make_tuple, [10, element, abc]},
         {make_tuple, [xyz, element, [abc]]},

         {map_get, [a, #{}]},
         {map_get, [a, b]},
         {map_size, [[a,b,c]]},

         {match_spec_test, [abc, match_spec, bad_type]},
         {match_spec_test, [abc, match_spec, trace]},
         {match_spec_test, [abc, [{{'$1','$2','$3'},[],['$$']}], table], [no_fail]},

         {md5, [abc]},
         {md5_final, [abc]},
         {md5_update, [abc, xyz]},
         {md5_update, [<<"bad context">>, "data"]},

         {memory, [BadMemoryType]},             %An atom if our own allocators are enabled.
         {memory, [{always,wrong}]},

         %% Not a BIF.
         {module_info, 1},

         {module_loaded, [42]},

         {monitor, [moon, whatever]},
         {monitor, [port, self()]},
         {monitor, [port, ExternalPort]},

         {monitor, [moon, whatever, []]},
         {monitor, [port, self(), []]},
         {monitor, [port, ExternalPort, []]},
         {monitor, [port, ExternalPort, [bad_option]]},
         {monitor, [process, self(), [bad_option]]},
         {monitor, [process, self(), not_a_list]},

         {monitor_node, [{a,b,c}, bad]},
         {monitor_node, [{a,b,c}, bad, bad]},
         {monitor_node, [{a,b,c}, bad, [bad]]},

         {monotonic_time, [fortnight]},

         {nif_error, 1},
         {nif_error, 2},
         {node, [abc]},
         {nodes, [abc], [{1, ".*not a valid node type.*"}]},
         {nodes, [[abc]], [{1, ".*not a list of.*"}]},
         {nodes, [DeadProcess], [{1, ".*not a valid node type or list of valid node types.*"}]},
         {nodes, [abc, 17], [{1, ".*not a valid node type.*"},{2, ".*not a map.*"}]},
         {nodes, [hidden, 17], [{2, ".*not a map.*"}]},
         {nodes, [hidden, #{ 17 => true }], [{2, ".*invalid options in map.*"}]},
         {nodes, [hidden, #{ connection_id => 17 }], [{2, ".*invalid options in map.*"}]},
         {nodes, [hidden, #{ node_type => 17 }], [{2, ".*invalid options in map.*"}]},
         {nodes, [abc, #{}], [{1, ".*not a valid node type.*"}]},
         {nodes, [DeadProcess,#{node_type=>true}], [{1, ".*not a valid node type or list of valid node types.*"}]},
         {nodes, [visible, 17], [{2,".*not a map.*"}]},
         {nodes, [visible, #{connection_id=>blapp}], [{2,".*invalid options in map.*"}]},
         {nodes, [visible, 17], [{2,".*not a map.*"}]},
         {nodes, [known, #{connection_id=>true,node_type=>true}], [no_fail]},
         {nodes, [[this,connected], #{connection_id=>true,node_type=>true}], [no_fail]},
         {phash, [any, -1]},
         {phash, [any, not_integer]},
         {phash2, [any], [no_fail]},
         {phash2, [any, not_integer]},
         {pid_to_list, [abc]},

         {open_port, [{bad,name}, []]},
         {open_port, [{spawn, "no_command"}, bad_option_list]},
         {open_port, [{spawn, "no_command"}, [xyz]]},
         {open_port, [{spawn, "no_command"}, [{args,[]}]],[{1,".*spawn_executable.*"}]},

         {port_call, 2},                        %Internal BIF.

         {port_call, [{no,port}, b, data]},
         {port_call, [{no,port}, -1, data]},
         {port_call, [{no,port}, 1 bsl 32, data]},
         {port_call, [ExternalPort, b, data]},

         {port_close, [{no,port}]},
         {port_close, [ExternalPort]},

         {port_command, [{no,port}, [a|b]]},

         {port_command, [{no,port}, [command], [whatever]]},
         {port_command, [{no,port}, [command], whatever]},
         {port_command, [ExternalPort, [command], whatever]},

         {port_connect, [{no,port}, whatever]},
         {port_connect, [{no,port}, self()]},
         {port_connect, [{no,port}, ExternalPid]},
         {port_connect, [ExternalPort, self()]},

         {port_control, [{no,port}, -1, {a,b,c}]},
         {port_control, [ExternalPort, -1, {a,b,c}]},

         {port_info, [{no,port}]},
         {port_info, [ExternalPort]},

         {port_info, [{no,port}, bad_info]},
         {port_info, [ExternalPort, name]},
         {port_info, [ExternalPort, bad_info]},

         %% Internal undocumented BIFs.
         {port_get_data, 1},
         {port_set_data, 2},

         {port_to_list, [abc]},
         {posixtime_to_universaltime, [abc]},

         {prepare_loading, [{no,module}, <<1,2,3>>]},
         {prepare_loading, [good_module_name, not_binary]},
         {prepare_loading, [<<1,2,3>>, not_binary]},

         {process_display, [bad_pid, whatever]},
         {process_display, [self(), whatever]},
         {process_display, [ExternalPid, backtrace]},
         {process_display, [ExternalPid, whatever]},
         {process_display, [DeadProcess, backtrace]},

         {process_flag, [trap_exit, some_value]},
         {process_flag, [bad_flag, some_value]},

         {process_flag, [self(), bad_flag, some_value]},
         {process_flag, [self(), save_calls, not_integer]},
         {process_flag, [self(), save_calls, 1 bsl 64]},
         {process_flag, [ExternalPid, save_calls, 20]},
         {process_flag, [ExternalPid, save_calls, not_integer]},
         {process_flag, [ExternalPid, bad_flag, some_value]},
         {process_flag, [DeadProcess, save_calls, 20]},
         {process_flag, [DeadProcess, not_save_save_calls, 20]},

         {process_info, [42]},
         {process_info, [ExternalPid]},

         {process_info, [self(), {a,b,c}]},
         {process_info, [ExternalPid, current_function]},

         {purge_module, [{no,module}]},

         {put, [key_never_read, value], [no_fail]},

         {raise, 3},
         {read_timer, [bad_timer]},
         {read_timer, [bad_time, bad_options]},
         {ref_to_list, [abc]},

         {register, [<<"name">>, abc]},
         {register, [<<"name">>, self()]},
         {register, [my_registered_name, abc]},
         {register, [undefined, self()]},
         {register, [code_server, self()]},
         {register, [my_registered_name, whereis(code_server)]},
         {register, [my_registered_name, DeadProcess]},
         {register, [my_registered_name, ExternalPid]},
         {register, [my_registered_name, ExternalPort]},

         {resume_process, [abc]},
         {resume_process, [self()]},
         {resume_process, [DeadProcess]},
         {resume_process, [ExternalPid]},

         {round, [abc]},

         {send, [[bad,dest], message]},
         {send, [[bad,dest], message, bad]},

         {send_after, [Eons, self(), message]},
         {send_after, [Eons, {bad,dest}, message]},
         {send_after, [bad_time, {bad,dest}, message]},
         {send_after, [20, ExternalPid, message]},

         {send_after, [Eons, self(), message, bad_options]},
         {send_after, [Eons, {bad,dest}, message, bad_options]},
         {send_after, [bad_time, {bad,dest}, message, bad_options]},
         {send_after, [20, self(), message, [bad]]},
         {send_after, [20, ExternalPid, message, []]},

         {send_nosuspend, [bad_pid, message]},
         {send_nosuspend, [bad_pid, message, []]},
         {send_nosuspend, [self(), message, [bad_option]]},
         {send_nosuspend, [self(), message, bad_options]},

         %% Internal undocumented BIFs.
         {seq_trace, 2},
         {seq_trace_info, 1},
         {seq_trace_print, 1},
         {seq_trace_print, 2},
         {set_cookie, [{not_cookie}]},
         {set_cookie, [{not_node}, {not_cookie}]},
         {set_cookie, [nonode@nohost, {not_cookie}]},
         {set_cpu_topology, 1},

         {setelement, [a, b, c]},
         {setnode, 2},                          %Internal und undocumented BIF.
         {setnode, 3},                          %Internal und undocumented BIF.
         {size, [abc]},

         {spawn, [not_fun]},
         {spawn, [[bad_node], not_fun]},
         {spawn, [0, 1, 2]},
         {spawn, [0, 1, [a|b]]},
         {spawn, [[bad_node], 0, 1, a]},

         {spawn_link, [not_fun]},
         {spawn_link, [[bad_node], not_fun]},
         {spawn_link, [0, 1, 2]},
         {spawn_link, [[bad_node], 0, 1, a]},

         {spawn_monitor, [not_fun]},
         {spawn_monitor, [[bad_node], not_fun]},
         {spawn_monitor, [0, 1, a]},
         {spawn_monitor, [[bad_node], 0, 1, a]},

         {spawn_opt, [bad_fun, []]},
         {spawn_opt, [bad_fun, [a|b]]},
         {spawn_opt, [bad_fun, [bad_option]]},
         {spawn_opt, [fun() -> ok end, [bad_option]]},

         {spawn_opt, [node(), bad_fun, []]},
         {spawn_opt, [[bad_node], fun() -> ok end, []]},
         {spawn_opt, [[bad_node], bad_fun, bad_options]},

         {spawn_opt, [0, 1, 2, bad_options]},
         {spawn_opt, [0, 1, 2, bad_options]},
         {spawn_opt, [0, 1, 2, [bad_option]]},
         {spawn_opt, [[bad_node], 0, 1, 1 bsl 64, [bad_option]]},

         {spawn_request, [not_a_fun]},

         {spawn_request, [a, b]},
         {spawn_request, [42, 43]},
         {spawn_request, [[bad_node], bad_fun]},
         {spawn_request, [fun() -> ok end, [a|b]]},
         {spawn_request, [[bad_node], fun() -> ok end]},

         {spawn_request, [[bad_node], fun() -> ok end, []]},
         {spawn_request, [[bad_node], fun() -> ok end, bad_options]},
         {spawn_request, [node, fun() -> ok end, bad_options]},
         {spawn_request, [0, 1, a]},

         {spawn_request, [0, 1, a, []]},
         {spawn_request, [m, f, [a], [a|b]]},

         {spawn_request, [node(), m, f, [a], bad_option_list]},
         {spawn_request, [[bad_node], 0, 1, a, []]},
         {spawn_request, [[bad_node], 0, 1, a, [a|b]]},

         {spawn_request_abandon, [abc]},

         {split_binary, [a, b]},
         {split_binary, [a, -1]},
         {split_binary, [<<>>, 1]},

         {start_timer, [Eons, self(), message]},
         {start_timer, [Eons, {bad,dest}, message]},
         {start_timer, [bad_time, {bad,dest}, message]},

         {start_timer, [Eons, self(), message, []]},
         {start_timer, [Eons, {bad,dest}, message, [bad]]},
         {start_timer, [bad_time, {bad,dest}, message, bad_options]},
         {start_timer, [20, self(), message, [bad]]},
         {start_timer, [20, ExternalPid, message, []]},

         {statistics, [abc]},
         {statistics, [{a,b,c}]},
         {subtract, [a,b]},

         {suspend_process, [self()]},
         {suspend_process, [ExternalPid]},
         {suspend_process, [not_a_pid]},

         {suspend_process, [self(), []]},
         {suspend_process, [ExternalPid, []]},
         {suspend_process, [not_a_pid, []]},
         {suspend_process, [not_a_pid, [bad_option]]},

         {system_flag, [bad_flag, whatever]},
         {system_flag, [{bad,flag}, whatever]},
         {system_flag, [backtrace_depth, bad_depth]},
         {system_info, [{bad,item}]},
         {system_info, [bad_item]},

         {system_monitor, [whatever]},
         {system_monitor, [ExternalPid]},

         {system_monitor, [not_pid, bad_list]},
         {system_monitor, [self(), bad_list]},
         {system_monitor, [self(), [bad]]},
         {system_monitor, [ExternalPid, [busy_port]]},
         {system_monitor, [ExternalPid, [bad]]},
         {system_monitor, [self(), [bad]]},

         %% Complex error reasons. Ignore for now.
         {system_profile, 2},

         {system_time, [fortnight]},

         {term_to_binary, [any, bad_options]},
         {term_to_binary, [any, [bad]]},

         {term_to_iovec, [any], [no_fail]},
         {term_to_iovec, [any, bad_options]},
         {term_to_iovec, [any, [bad]]},

         {throw, 1},
         {time_offset, [fortnight]},
         {tl, [abc]},

         {trace, [ExternalPid, true, all]},
         {trace, [ExternalPid, not_boolean, bad_flags]},
         {trace, [ExternalPort, true, all]},
         {trace, [a, not_boolean, bad_flags]},
         {trace, [a, not_boolean, [bad_flag]]},
         {trace, [a, true, [a|b]]},

         {trace_pattern, [a, b]},
         {trace_pattern, [a, b, c]},
         {trace_pattern, [{?MODULE,'_','_'}, [{[self(), '_'],[],[]}], [call_count]]},

         {trace_delivered, [ExternalPid]},
         {trace_delivered, [abc]},

         {trace_info, [ExternalPid, flags]},
         {trace_info, [ExternalPid, bad_item]},
         {trace_info, [ExternalPort, flags]},
         {trace_info, [ExternalPort, bad_item]},
         {trace_info, [self(), bad_item]},
         {trace_info, [bad_tracee_spec, flags]},

         {trunc, [abc]},
         {tuple_size, [<<"abc">>]},
         {tuple_to_list, [<<"abc">>]},
         {unalias,[not_a_ref]},
         {unique_integer, [[a|b]]},
         {unique_integer, [[a,b]]},
         {unique_integer, [abc]},
         {universaltime_to_localtime, [bad_time]},
         {universaltime_to_posixtime, [bad_time]},
         {unlink, [42]},
         {unregister, [{a,b,c}]},
         {whereis, [self()]}
        ],
    try
        do_error_info(L)
    after
        peer:stop(Peer)
    end.

dead_process() ->
    {Pid,Ref} = spawn_monitor(fun() -> ok end),
    receive
        {'DOWN',Ref,process,Pid,normal} ->
            Pid
    end.

non_existing_atom() ->
    non_existing_atom([]).

non_existing_atom(Atom) ->
    try list_to_existing_atom(Atom) of
        _ ->
            Char = rand:uniform($Z - $A + 1) + $A - 1,
            non_existing_atom([Char|Atom])
    catch
        error:badarg ->
            AtomBin = list_to_binary(Atom),
            AtomExt = <<131,100,(byte_size(AtomBin)):16,AtomBin/binary>>,
            {Atom,AtomExt}
    end.

do_error_info(L0) ->
    L1 = lists:foldl(fun({_,A}, Acc) when is_integer(A) -> Acc;
                        ({F,A}, Acc) -> [{F,A,[]}|Acc];
                        ({F,A,Opts}, Acc) -> [{F,A,Opts}|Acc]
                     end, [], L0),
    Tests = ordsets:from_list([{F,length(A)} || {F,A,_} <- L1] ++
                                  [{F,A} || {F,A} <- L0, is_integer(A)]),
    Bifs0 = [{F,A} || {F,A} <- erlang:module_info(exports),
                      A =/= 0,
                      not erl_bifs:is_safe(erlang, F, A),
                      not erl_internal:arith_op(F, A),
                      not erl_internal:bool_op(F, A),
                      not erl_internal:list_op(F, A),
                      not erl_internal:send_op(F, A)],
    Bifs = ordsets:from_list(Bifs0),
    NYI = [{F,lists:duplicate(A, '*'),nyi} || {F,A} <- Bifs -- Tests],
    L = lists:sort(NYI ++ L1),
    do_error_info(L, []).

do_error_info([{_,Args,nyi}=H|T], Errors) ->
    case lists:all(fun(A) -> A =:= '*' end, Args) of
        true ->
            do_error_info(T, [{nyi,H}|Errors]);
        false ->
            do_error_info(T, [{bad_nyi,H}|Errors])
    end;
do_error_info([{F,Args,Opts}|T], Errors) ->
    eval_bif_error(F, Args, Opts, T, Errors);
do_error_info([], Errors0) ->
    case lists:sort(Errors0) of
        [] ->
            ok;
        [_|_]=Errors ->
            io:format("\n~p\n", [Errors]),
            ct:fail({length(Errors),errors})
    end.

eval_bif_error(F, Args, Opts, T, Errors0) ->
    try eval_do_apply(erlang, F, Args) of
        Result ->
            case lists:member(no_fail, Opts) of
                true ->
                    do_error_info(T, Errors0);
                false ->
                    do_error_info(T, [{should_fail,{F,Args},Result}|Errors0])
            end
    catch
        error:Reason:Stk ->
            SF = fun(Mod, _, _) -> Mod =:= test_server end,
            Str = erl_error:format_exception(error, Reason, Stk, #{stack_trim_fun => SF}),
            BinStr = iolist_to_binary(Str),
            ArgStr = lists:join(", ", [io_lib:format("~p", [A]) || A <- Args]),
            io:format("\nerlang:~p(~s)\n~ts", [F,ArgStr,BinStr]),

            {erlang,ActualF,ActualArgs,Info} = hd(Stk),

            RE = <<"[*][*][*] argument \\d+:">>,
            Errors1 = case re:run(BinStr, RE, [{capture, none}]) of
                          match ->
                              Errors0;
                          nomatch when Reason =:= system_limit ->
                              Errors0;
                          nomatch ->
                              [{no_explanation,{F,Args},Info}|Errors0]
                      end,

            Errors = case {ActualF,ActualArgs} of
                         {F,Args} ->
                             Errors1;
                         _ ->
                             [{renamed,{F,length(Args)},{ActualF,ActualArgs}}|Errors1]
                     end,

            do_error_info(T, Errors)
    end.

eval_do_apply(erlang, load_nif, [Path]) ->
    erlang:load_nif(Path);
eval_do_apply(erlang, load_nif, [Path,LoadInfo]) ->
    erlang:load_nif(Path, LoadInfo);
eval_do_apply(Mod, Name, Args) ->
    apply(Mod, Name, Args).

no_line_numbers(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Src = filename:join(DataDir, atom_to_list(?FUNCTION_NAME) ++ ".erl"),
    {ok,Mod,Code} = compile:file(Src, [no_line_info,binary,report]),
    {module,Mod} = code:load_binary(Mod, "", Code),

    %% Make sure that the correct function is returned in the stacktrace
    %% even if the compiled code has no `line` instructions.
    {'EXIT',{badarith,[{erlang,'*',_,_},{Mod,a,_,_}|_]}} = (catch Mod:a(aa)),
    {'EXIT',{badarith,[{erlang,'*',_,_},{Mod,b,_,_}|_]}} = (catch Mod:b(bb)),
    {'EXIT',{badarith,[{erlang,'*',_,_},{Mod,c,_,_}|_]}} = (catch Mod:c(cc)),
    {'EXIT',{badarith,[{erlang,'*',_,_},{Mod,d,_,_}|_]}} = (catch Mod:d(dd)),
    {'EXIT',{badarith,[{erlang,'*',_,_},{Mod,e,_,_}|_]}} = (catch Mod:e(ee)),
    ok.

line_numbers(Config) when is_list(Config) ->
    {'EXIT',{{case_clause,bad_tag},
             [{?MODULE,line1,2,
               [{file,"fake_file.erl"},{line,3}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch line1(bad_tag, 0)),

    %% The stacktrace for operators such a '+' can vary depending on
    %% whether the JIT is used or not.
    case catch line1(a, not_an_integer) of
        {'EXIT',{badarith,
                 [{erlang,'+',[not_an_integer,1],_},
                  {?MODULE,line1,2,
                   [{file,"fake_file.erl"},{line,5}]},
                  {?MODULE,line_numbers,1,_}|_]}} ->
            ok;
        {'EXIT',{badarith,
                 [{?MODULE,line1,2,
                   [{file,"fake_file.erl"},{line,5}]},
                  {?MODULE,line_numbers,1,_}|_]}} ->
            ok
    end,

    {'EXIT',{{badmatch,{ok,1}},
             [{?MODULE,line1,2,
               [{file,"fake_file.erl"},{line,7}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch line1(a, 0)),
    {'EXIT',{crash,
             [{?MODULE,crash,1,
               [{file,"fake_file.erl"},{line,14}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch line1(a, 41)),

    ModFile = ?MODULE_STRING++".erl",
    [{?MODULE,maybe_crash,1,[{file,"call.erl"},{line,28}]},
     {?MODULE,call1,0,[{file,"call.erl"},{line,14}]},
     {?MODULE,close_calls,1,[{file,"call.erl"},{line,5}]},
     {?MODULE,line_numbers,1,[{file,ModFile},{line,_}]}|_] =
    close_calls(call1),
    [{?MODULE,maybe_crash,1,[{file,"call.erl"},{line,28}]},
     {?MODULE,call2,0,[{file,"call.erl"},{line,18}]},
     {?MODULE,close_calls,1,[{file,"call.erl"},{line,6}]},
     {?MODULE,line_numbers,1,[{file,ModFile},{line,_}]}|_] =
    close_calls(call2),
    [{?MODULE,maybe_crash,1,[{file,"call.erl"},{line,28}]},
     {?MODULE,call3,0,[{file,"call.erl"},{line,22}]},
     {?MODULE,close_calls,1,[{file,"call.erl"},{line,7}]},
     {?MODULE,line_numbers,1,[{file,ModFile},{line,_}]}|_] =
    close_calls(call3),
    no_crash = close_calls(other),

    <<0,0>> = build_binary1(16),
    {'EXIT',{badarg,
             [{?MODULE,build_binary1,1,
               [{file,"bit_syntax.erl"},{line,72503},{error_info,_}]},
              {?MODULE,line_numbers,1,
               [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary1(bad_size)),

    <<7,1,2,3>> = build_binary2(8, <<1,2,3>>),
    {'EXIT',{badarg,
             [{?MODULE,build_binary2,2,
               [{file,"bit_syntax.erl"},{line,72507},{error_info,_}]},
              {?MODULE,line_numbers,1,
               [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary2(bad_size, <<>>)),
    {'EXIT',{badarg,
             [{?MODULE,build_binary2,2,
               [{file,"bit_syntax.erl"},{line,72507},{error_info,_}]},
              {?MODULE,line_numbers,1,
               [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary2(8, bad_binary)),

    <<"abc",357:16>> = build_binary3(<<"abc">>),
    {'EXIT',{badarg,[{?MODULE,build_binary3,1,
                      [{file,"bit_syntax.erl"},{line,72511},{error_info,_}]},
                     {?MODULE,line_numbers,1,
                      [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary3(no_binary)),

    {'EXIT',{function_clause,
             [{?MODULE,do_call_abs,[y,y],
               [{file,"gc_bif.erl"},{line,18}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch do_call_abs(y, y)),
    {'EXIT',{badarg,
             [{erlang,abs,[[]],[{error_info,_}]},
              {?MODULE,do_call_abs,2,
               [{file,"gc_bif.erl"},{line,19}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch do_call_abs(x, [])),

    {'EXIT',{{badmatch,"42"},
             [{MODULE,applied_bif_1,1,[{file,"applied_bif.erl"},{line,5}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch applied_bif_1(42)),

    {'EXIT',{{badmatch,{current_location,
                        {?MODULE,applied_bif_2,0,
                         [{file,"applied_bif.erl"},{line,9}]}}},
             [{MODULE,applied_bif_2,0,[{file,"applied_bif.erl"},{line,10}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch applied_bif_2()),

    case catch increment1(x) of
        {'EXIT',{badarith,
                 [{erlang,'+',[x,1],_},
                  {?MODULE,increment1,1,[{file,"increment.erl"},{line,45}]},
                  {?MODULE,line_numbers,1,_}|_]}} ->
            ok;
        {'EXIT',{badarith,
                 [{?MODULE,increment1,1,[{file,"increment.erl"},{line,45}]},
                  {?MODULE,line_numbers,1,_}|_]}} ->
            ok
    end,

    case catch increment2(x) of
        {'EXIT',{badarith,
                 [{erlang,'+',[x,1],_},
                  {?MODULE,increment2,1,[{file,"increment.erl"},{line,48}]},
                  {?MODULE,line_numbers,1,_}|_]}} ->
            ok;
        {'EXIT',{badarith,
                 [{?MODULE,increment2,1,[{file,"increment.erl"},{line,48}]},
                  {?MODULE,line_numbers,1,_}|_]}} ->
            ok
    end,

    {'EXIT',{{badmap,not_a_map},
             [{?MODULE,update_map,1,[{file,"map.erl"},{line,3}]}|_]}} =
        (catch update_map(not_a_map)),
    {'EXIT',{{badkey,a},
             [{?MODULE,update_map,1,[{file,"map.erl"},{line,4}]}|_]}} =
        (catch update_map(#{})),

    {'EXIT',{badarg,
             [{erlang,hd,[x],[{error_info,_}]},
              {?MODULE,test_hd,1,[{file,"list_bifs.erl"},{line,101}]}|_]}} =
        (catch test_hd(x)),
    {'EXIT',{badarg,
             [{erlang,tl,[y],[{error_info,_}]},
              {?MODULE,test_tl,1,[{file,"list_bifs.erl"},{line,102}]}|_]}} =
        (catch test_tl(y)),

    %% This line number is too large to be represented and will be silently
    %% ignored by the emulator.
    {'EXIT',{crash,
             [{?MODULE,crash_huge_line,1,[]},
              {?MODULE,line_numbers,1,_}|_]}} =
        (catch crash_huge_line(gurka)),

    {'EXIT',{{badrecord,[1,2,3]},
             [{?MODULE,bad_record,1,[{file,"bad_records.erl"},{line,4}]}|_]}} =
        catch bad_record([1,2,3]),

    %% GH-5960: When an instruction raised an exception at the very end of the
    %% instruction (e.g. badmatch), and was directly followed by a line
    %% instruction, the exception was raised for the wrong line.
    ok = ambiguous_line(0),
    {'EXIT',{{badmatch,_},
        [{?MODULE,ambiguous_line,1,[{file,"ambiguous_line.erl"},
                                    {line,3}]}|_]}} = catch ambiguous_line(1),
    {'EXIT',{{badmatch,_},
        [{?MODULE,ambiguous_line,1,[{file,"ambiguous_line.erl"},
                                    {line,4}]}|_]}} = catch ambiguous_line(2),

    ok.

id(I) -> I.

-file("odd_even.erl", 1).			%Line 1
even(N) when is_integer(N), N > 1, (N rem 2) == 0 ->
    odd(N-1)++[N].				%Line 3

odd(N) when is_integer(N), N > 1, (N rem 2) == 1 ->
    even(N-1)++[N].				%Line 6

%%
%% If the compiler removes redundant line instructions (any
%% line instruction with the same location as the previous),
%% and the loader also removes line instructions before
%% tail-recursive calls to external functions, then the
%% badmatch exception in line 7 below will be reported as
%% occurring in line 6.
%%
%% That means that any removal of redundant line instructions
%% must all be done in the compiler OR in the loader.
%%
-file("fake_file.erl", 1).			%Line 1
line1(Tag, X) ->				%Line 2
    case Tag of					%Line 3
        a ->
            Y = X + 1,				%Line 5
            Res = id({ok,Y}),			%Line 6
            ?MODULE:crash({ok,42} = Res);	%Line 7
        b ->
            x = id(x),				%Line 9
            ok					%Line 10
    end.					%Line 11

crash(_) ->					%Line 13
    erlang:error(crash).			%Line 14

-file("call.erl", 1).				%Line 1
close_calls(Where) ->				%Line 2
    put(where_to_crash, Where),			%Line 3
    try
        call1(),				%Line 5
        call2(),				%Line 6
        call3(),				%Line 7
        no_crash				%Line 8
    catch error:crash:Stk ->
            Stk                                 %Line 10
    end.					%Line 11

call1() ->					%Line 13
    maybe_crash(call1),				%Line 14
    ok.						%Line 15

call2() ->					%Line 17
    maybe_crash(call2),				%Line 18
    ok.						%Line 19

call3() ->					%Line 21
    maybe_crash(call3),				%Line 22
    ok.						%Line 23

maybe_crash(Name) ->				%Line 25
    case get(where_to_crash) of			%Line 26
        Name ->
            erlang:error(crash);		%Line 28
        _ ->
            ok					%Line 30
    end.

-file("bit_syntax.erl", 72500).			%Line 72500
build_binary1(Size) ->				%Line 72501
    id(42),					%Line 72502
    <<0:Size>>.					%Line 72503

build_binary2(Size, Bin) ->			%Line 72505
    id(0),					%Line 72506
    <<7:Size,Bin/binary>>.			%Line 72507

build_binary3(Bin) ->			        %Line 72509
    id(0),					%Line 72510
    <<Bin/binary,357:16>>.			%Line 72511

-file("gc_bif.erl", 17).
do_call_abs(x, Arg) ->				%Line 18
    abs(Arg).					%Line 19

%% Make sure a BIF that is applied does not leave the p->cp
%% set (and thus generating an extra entry on the stack).

-file("applied_bif.erl", 1).
%% Explicit apply.
applied_bif_1(I) ->				%Line 3
    L = apply(erlang, integer_to_list, [I]),	%Line 4
    fail = L,					%Line 5
    ok.						%Line 6
%% Implicit apply.
applied_bif_2() ->				%Line 8
    R = process_info(self(), current_location),	%Line 9
    fail = R,					%Line 10
    ok.						%Line 11

%% The increment instruction used to decrement the instruction
%% pointer, which would cause the line number in a stack trace to
%% be the previous line number.

-file("increment.erl", 42).
increment1(Arg) ->                              %Line 43
    Res = id(Arg),                              %Line 44
    Res + 1.                                    %Line 45
increment2(Arg) ->                              %Line 46
    _ = id(Arg),                                %Line 47
    Arg + 1.                                    %Line 48

-file("map.erl", 1).
update_map(M0) ->                               %Line 2
    M = M0#{new => value},                      %Line 3
    M#{a := b}.                                 %Line 4

-file("list_bifs.erl", 100).
test_hd(X) -> foo(), hd(X).                     %Line 101
test_tl(X) -> foo(), tl(X).                     %Line 102
foo() -> id(100).

-file("huge_lines.erl", 100000000).             %Line 100000000

crash_huge_line(_) ->                           %Line 100000002
    erlang:error(crash).                        %Line 100000003

-file("bad_records.erl", 1).
-record(foobar, {a,b,c,d}).                     %Line 2.
bad_record(R) ->                                %Line 3.
    R#foobar.a.                                 %Line 4.

-file("ambiguous_line.erl", 1).
ambiguous_line(A) ->                            %Line 2.
    true = A =/= 1,                             %Line 3.
    true = A =/= 2,                             %Line 4.
    ok.
