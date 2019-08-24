%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
         badmatch/1, pending_errors/1, nil_arith/1, top_of_stacktrace/1,
         stacktrace/1, nested_stacktrace/1, raise/1, gunilla/1, per/1,
         change_exception_class/1,
         exception_with_heap_frag/1, backtrace_depth/1,
         line_numbers/1]).

-export([bad_guy/2]).
-export([crash/1]).

-include_lib("common_test/include/ct.hrl").
-import(lists, [foreach/2]).

%% The range analysis of the HiPE compiler results in a system limit error
%% during compilation instead of at runtime, so do not perform this analysis.
-compile([{hipe, [no_icode_range]}]).

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
     exception_with_heap_frag, backtrace_depth, line_numbers].

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

%% Test that doing arithmetics on [] gives a badarith EXIT and not a crash.

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

    %% Tuples
    {'EXIT', {badarg, [{erlang, element, [1, ok], _} | _]}} = (catch my_element(1, ok)),
    {'EXIT', {badarg, [{erlang, element, [ok, {}], _} | _]}} = (catch my_element(ok, {})),
    {'EXIT', {badarg, [{erlang, element, [1, {}], _} | _]}} = (catch my_element(1, {})),
    {'EXIT', {badarg, [{erlang, element, [1, {}], _} | _]}} = (catch element(1, erlang:make_tuple(0, ok))),

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
    %% We assume that the maximum arity is (1 bsl 19) - 1.
    Ws = erlang:system_info(wordsize),
    (((1 bsl ((16777184 * (Ws div 4))-1)) - 1) bsl 1) + 1.

maxbig_gc() ->
    Maxbig = maxbig(),
    erlang:garbage_collect(),
    Maxbig.

stacktrace(Conf) when is_list(Conf) ->
    Tag = make_ref(),
    {_,Mref} = spawn_monitor(fun() -> exit({Tag,erlang:get_stacktrace()}) end),
    {Tag,[]} = receive {'DOWN',Mref,_,_,Info} -> Info end,
    V = [make_ref()|self()],
    {value2,{caught1,badarg,[{erlang,abs,[V],_}|_]=St1}} =
    stacktrace_1({'abs',V}, error, {value,V}),
    St1 = erase(stacktrace1),
    St1 = erase(stacktrace2),
    St1 = erlang:get_stacktrace(),
    {caught2,{error,badarith},[{erlang,'+',[0,a],_},{?MODULE,my_add,2,_}|_]=St2} =
    stacktrace_1({'div',{1,0}}, error, {'add',{0,a}}),
    [{erlang,'div',[1,0],_},{?MODULE,my_div,2,_}|_] = erase(stacktrace1),
    St2 = erase(stacktrace2),
    St2 = erlang:get_stacktrace(),
    {caught2,{error,{try_clause,V}},[{?MODULE,stacktrace_1,3,_}|_]=St3} =
    stacktrace_1({value,V}, error, {value,V}),
    St3 = erase(stacktrace1),
    St3 = erase(stacktrace2),
    St3 = erlang:get_stacktrace(),
    {caught2,{throw,V},[{?MODULE,foo,1,_}|_]=St4} =
    stacktrace_1({value,V}, error, {throw,V}),
    [{?MODULE,stacktrace_1,3,_}|_] = erase(stacktrace1),
    St4 = erase(stacktrace2),
    St4 = erlang:get_stacktrace(),

    try
        stacktrace_2()
    catch
        error:{badmatch,_} ->
            [{?MODULE,stacktrace_2,0,_},
             {?MODULE,stacktrace,1,_}|_] =
            erlang:get_stacktrace(),
            ok
    end.

stacktrace_1(X, C1, Y) ->
    erase(stacktrace1),
    erase(stacktrace2),
    try try foo(X) of
            C1 -> value1
        catch
            C1:D1 -> {caught1,D1,erlang:get_stacktrace()}
        after
            put(stacktrace1, erlang:get_stacktrace()),
            foo(Y)
        end of
        V2 -> {value2,V2}
    catch
        C2:D2 -> {caught2,{C2,D2},erlang:get_stacktrace()}
    after
        put(stacktrace2, erlang:get_stacktrace())
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
     value2,
     [{erlang,'+',[V,x1],_},{?MODULE,my_add,2,_}|_]} =
    nested_stacktrace_1({{'add',{V,x1}},error,badarith},
                        {{value,{V,x2}},void,{V,x2}}),
    {caught1,
     [{erlang,'+',[V,x1],_},{?MODULE,my_add,2,_}|_],
     {caught2,[{erlang,abs,[V],_}|_]},
     [{erlang,abs,[V],_}|_]} =
    nested_stacktrace_1({{'add',{V,x1}},error,badarith},
                        {{'abs',V},error,badarg}),
    ok.

nested_stacktrace_1({X1,C1,V1}, {X2,C2,V2}) ->
    try foo(X1) of
        V1 -> value1
    catch
        C1:V1 ->
            S1 = erlang:get_stacktrace(),
            T2 =
            try foo(X2) of
                V2 -> value2
            catch
                C2:V2 -> {caught2,erlang:get_stacktrace()}
            end,
            {caught1,S1,T2,erlang:get_stacktrace()}
    end.



raise(Conf) when is_list(Conf) ->
    erase(raise),
    A = 
    try 
        try foo({'div',{1,0}}) 
        catch
            error:badarith:A0 ->
                put(raise, A0 = erlang:get_stacktrace()),
                erlang:raise(error, badarith, A0)
        end
    catch
        error:badarith:A1 ->
            A1 = erlang:get_stacktrace(),
            A1 = get(raise)
    end,
    A = erlang:get_stacktrace(),
    A = get(raise),
    [{erlang,'div',[1, 0], _},{?MODULE,my_div,2,_}|_] = A,
    %%
    N = 8, % Must be even
    N = erlang:system_flag(backtrace_depth, N),
    B = odd_even(N, []),
    try even(N) 
    catch error:function_clause -> ok
    end,
    B = erlang:get_stacktrace(),
    %%
    C0 = odd_even(N+1, []),
    C = lists:sublist(C0, N),
    try odd(N+1) 
    catch error:function_clause -> ok
    end,
    C = erlang:get_stacktrace(),
    try erlang:raise(error, function_clause, C0)
    catch error:function_clause -> ok
    end,
    C = erlang:get_stacktrace(),
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

my_abs(X) -> abs(X).

gunilla(Config) when is_list(Config) ->
    {throw,kalle} = gunilla_1(),
    [] = erlang:get_stacktrace(),
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
                      _:_ ->
                          %% term_to_binary/1 is an easy way to traverse the
                          %% entire stacktrace term to make sure that every part
                          %% of it is OK.
                          term_to_binary(erlang:get_stacktrace())
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

line_numbers(Config) when is_list(Config) ->
    {'EXIT',{{case_clause,bad_tag},
             [{?MODULE,line1,2,
               [{file,"fake_file.erl"},{line,3}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch line1(bad_tag, 0)),
    {'EXIT',{badarith,
             [{?MODULE,line1,2,
               [{file,"fake_file.erl"},{line,5}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch line1(a, not_an_integer)),
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
               [{file,"bit_syntax.erl"},{line,72503}]},
              {?MODULE,line_numbers,1,
               [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary1(bad_size)),

    <<7,1,2,3>> = build_binary2(8, <<1,2,3>>),
    {'EXIT',{badarg,
             [{?MODULE,build_binary2,2,
               [{file,"bit_syntax.erl"},{line,72507}]},
              {?MODULE,line_numbers,1,
               [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary2(bad_size, <<>>)),
    {'EXIT',{badarg,
             [{erlang,bit_size,[bad_binary],[]},
              {?MODULE,build_binary2,2,
               [{file,"bit_syntax.erl"},{line,72507}]},
              {?MODULE,line_numbers,1,
               [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary2(8, bad_binary)),

    <<"abc",357:16>> = build_binary3(<<"abc">>),
    {'EXIT',{badarg,[{?MODULE,build_binary3,1,
                      [{file,"bit_syntax.erl"},{line,72511}]},
                     {?MODULE,line_numbers,1,
                      [{file,ModFile},{line,_}]}|_]}} =
    (catch build_binary3(no_binary)),

    {'EXIT',{function_clause,
             [{?MODULE,do_call_abs,[y,y],
               [{file,"gc_bif.erl"},{line,18}]},
              {?MODULE,line_numbers,1,_}|_]}} =
    (catch do_call_abs(y, y)),
    {'EXIT',{badarg,
             [{erlang,abs,[[]],[]},
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

    {'EXIT',{badarith,
             [{?MODULE,increment1,1,[{file,"increment.erl"},{line,45}]},
              {?MODULE,line_numbers,1,_}|_]}} =
        (catch increment1(x)),
    {'EXIT',{badarith,
             [{?MODULE,increment2,1,[{file,"increment.erl"},{line,48}]},
              {?MODULE,line_numbers,1,_}|_]}} =
        (catch increment2(x)),

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
    catch error:crash ->
              erlang:get_stacktrace()		%Line 10
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
