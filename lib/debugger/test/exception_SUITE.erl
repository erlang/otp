%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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
%%
-module(exception_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 badmatch/1,pending_errors/1,nil_arith/1,
         stacktrace/1,nested_stacktrace/1,raise/1,gunilla/1,per/1]).

-export([bad_guy/2]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

%% Filler.
%%
%%
%%
%%
%% This is line 40.
even(N) when is_integer(N), N > 1, (N rem 2) == 0 ->
    odd(N-1)++[N].

odd(N) when is_integer(N), N > 1, (N rem 2) == 1 ->
    even(N-1)++[N].


all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [badmatch, pending_errors, nil_arith, stacktrace,
     nested_stacktrace, raise, gunilla, per].

-define(try_match(E),
	catch ?MODULE:bar(),
	{'EXIT', {{badmatch, nomatch}, _}} = (catch E = nomatch)).

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

%% Test that deliberately bad matches are reported correctly.

badmatch(Config) when is_list(Config) ->
    ?line ?try_match(a),
    ?line ?try_match(42),
    ?line ?try_match({a, b, c}),
    ?line ?try_match([]),
    ?line ?try_match(1.0),
    ok.

%% Test various exceptions, in the presence of a previous error suppressed
%% in a guard.
pending_errors(Config) when is_list(Config) ->
    ?line pending(e_badmatch, {badmatch, b}),
    ?line pending(x, function_clause),
    ?line pending(e_case, {case_clause, xxx}),
    ?line pending(e_if, if_clause),
    ?line pending(e_badarith, badarith),
    ?line pending(e_undef, undef),
    ?line pending(e_timeoutval, timeout_value),
    ?line pending(e_badarg, badarg),
    ?line pending(e_badarg_spawn, badarg),
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
	    test_server:fail({not_exit, Other})
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
	    test_server:fail({unexpected_message, Other})
    after 10000 ->
	    test_server:fail(timeout)
    end,
    process_flag(trap_exit, false).

pending({badarg, [{erlang,Bif,BifArgs,_},{?MODULE,Func,Arity,_}|_]},
	Func, Args, _Code)
  when is_atom(Bif), is_list(BifArgs), length(Args) == Arity ->
    ok;
pending({undef,[{non_existing_module,foo,[],_}|_]}, _, _, _) ->
    ok;
pending({function_clause,[{?MODULE,Func,Args,_}|_]}, Func, Args, _Code) ->
    ok;
pending({Code,[{?MODULE,Func,Arity,_}|_]}, Func, Args, Code)
  when length(Args) == Arity ->
    ok;
pending(Reason, _Function, _Args, _Code) ->
    test_server:fail({bad_exit_reason,Reason}).

%% Test that doing arithmetics on [] gives a badarith EXIT and not a crash.

nil_arith(Config) when is_list(Config) ->
    ?line ba_plus_minus_times([], []),

    ?line ba_plus_minus_times([], 0),
    ?line ba_plus_minus_times([], 42),
    ?line ba_plus_minus_times([], 38724978123478923784),
    ?line ba_plus_minus_times([], 38.72),

    ?line ba_plus_minus_times(0, []),
    ?line ba_plus_minus_times(334, []),
    ?line ba_plus_minus_times(387249797813478923784, []),
    ?line ba_plus_minus_times(344.22, []),

    ?line ba_div_rem([], []),

    ?line ba_div_rem([], 0),
    ?line ba_div_rem([], 1),
    ?line ba_div_rem([], 42),
    ?line ba_div_rem([], 38724978123478923784),
    ?line ba_div_rem(344.22, []),

    ?line ba_div_rem(0, []),
    ?line ba_div_rem(1, []),
    ?line ba_div_rem(334, []),
    ?line ba_div_rem(387249797813478923784, []),
    ?line ba_div_rem(344.22, []),

    ?line ba_div_rem(344.22, 0.0),
    ?line ba_div_rem(1, 0.0),
    ?line ba_div_rem(392873498733971, 0.0),

    ?line ba_bop([], []),
    ?line ba_bop(0, []),
    ?line ba_bop(42, []),
    ?line ba_bop(-42342742987343, []),
    ?line ba_bop(238.342, []),
    ?line ba_bop([], 0),
    ?line ba_bop([], -243),
    ?line ba_bop([], 243),
    ?line ba_bop([], 2438724982478933),
    ?line ba_bop([], 3987.37),

    ?line ba_bnot([]),
    ?line ba_bnot(23.33),

    ?line ba_shift([], []),
    ?line ba_shift([], 0),
    ?line ba_shift([], 4),
    ?line ba_shift([], -4),
    ?line ba_shift([], 2343333333333),
    ?line ba_shift([], -333333333),
    ?line ba_shift([], 234.00),
    ?line ba_shift(23, []),
    ?line ba_shift(0, []),
    ?line ba_shift(-3433443433433323, []),
    ?line ba_shift(433443433433323, []),
    ?line ba_shift(343.93, []),
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

stacktrace(Conf) when is_list(Conf) ->
    Tag = make_ref(),
    ?line {_,Mref} = spawn_monitor(fun() -> exit({Tag,erlang:get_stacktrace()}) end),
    ?line {Tag,[]} = receive {'DOWN',Mref,_,_,Info} -> Info end,
    V = [make_ref()|self()],
    ?line {value2,{caught1,badarg,[{erlang,abs,[V],_}|_]=St1}} =
	stacktrace_1({'abs',V}, error, {value,V}),
    ?line St1 = erase(stacktrace1),
    ?line St1 = erase(stacktrace2),
    ?line St1 = erlang:get_stacktrace(),
    ?line {caught2,{error,badarith},[{?MODULE,my_add,2,_}|_]=St2} =
	stacktrace_1({'div',{1,0}}, error, {'add',{0,a}}),
    ?line [{?MODULE,my_div,2,_}|_] = erase(stacktrace1),
    ?line St2 = erase(stacktrace2),
    ?line St2 = erlang:get_stacktrace(),
    ?line {caught2,{error,{try_clause,V}},[{?MODULE,stacktrace_1,3,_}|_]=St3} =
	stacktrace_1({value,V}, error, {value,V}),
    ?line St3 = erase(stacktrace1),
    ?line St3 = erase(stacktrace2),
    ?line St3 = erlang:get_stacktrace(),
    ?line {caught2,{throw,V},[{?MODULE,foo,1,_}|_]=St4} =
	stacktrace_1({value,V}, error, {throw,V}),
    ?line [{?MODULE,stacktrace_1,3,_}|_] = erase(stacktrace1),
    ?line St4 = erase(stacktrace2),
    ?line St4 = erlang:get_stacktrace(),
    ok.

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



nested_stacktrace(Conf) when is_list(Conf) ->
    V = [{make_ref()}|[self()]],
    ?line value1 =
	nested_stacktrace_1({{value,{V,x1}},void,{V,x1}},
			    {void,void,void}),
    ?line {caught1,
	   [{?MODULE,my_add,2,_}|_],
	   value2,
	   [{?MODULE,my_add,2,_}|_]} =
	nested_stacktrace_1({{'add',{V,x1}},error,badarith},
			    {{value,{V,x2}},void,{V,x2}}),
    ?line {caught1,
	   [{?MODULE,my_add,2,_}|_],
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
    ?line erase(raise),
    ?line A =
	try
	    ?line try foo({'div',{1,0}})
		  catch
		      error:badarith ->
			  put(raise, A0 = erlang:get_stacktrace()),
			  ?line erlang:raise(error, badarith, A0)
		  end
	catch
	    error:badarith ->
		?line A1 = erlang:get_stacktrace(),
		?line A1 = get(raise)
	end,
    ?line A = erlang:get_stacktrace(),
    ?line A = get(raise),
    ?line [{?MODULE,my_div,2,_}|_] = A,
    %%
    N = 8, % Must be even
    ?line N = erlang:system_flag(backtrace_depth, N),
    ?line try even(N)
	  catch error:function_clause -> ok
	  end,
    ?line B = odd_even(N, []),
    ?line B = erlang:get_stacktrace(),
    %%
    ?line C0 = odd_even(N+1, []),
    ?line C = lists:sublist(C0, N),
    ?line try odd(N+1)
	  catch error:function_clause -> ok
	  end,
    ?line C = erlang:get_stacktrace(),
    ?line try erlang:raise(error, function_clause, C0)
          catch error:function_clause -> ok
          end,
    ?line C = erlang:get_stacktrace(),
    ok.

odd_even(N, R) when is_integer(N), N > 1 ->
    odd_even(N-1,
	     [if (N rem 2) == 0 ->
		      {?MODULE,even,1,[{file,?MODULE_STRING++".erl"},
				       {line,42}]};
		 true ->
		      {?MODULE,odd,1,[{file,?MODULE_STRING++".erl"},
				      {line,45}]}
	      end|R]);
odd_even(1, R) ->
    [{?MODULE,odd,[1],[{file,?MODULE_STRING++".erl"},
		       {line,44}]}|R].

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

my_div(A, B) ->
    A div B.

my_add(A, B) ->
    A + B.

my_abs(X) -> abs(X).

gunilla(Config) when is_list(Config) ->
    ?line {throw,kalle} = gunilla_1(),
    ?line [] = erlang:get_stacktrace(),
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

id(I) -> I.
