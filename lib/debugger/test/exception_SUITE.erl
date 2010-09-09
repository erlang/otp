%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

%%
-module(exception_SUITE).

-export([all/1,init_per_testcase/2,fin_per_testcase/2,init_all/1,finish_all/1,
	 badmatch/1,pending_errors/1,nil_arith/1]).

-export([bad_guy/2]).

-include("test_server.hrl").

all(suite) ->
    [{conf,init_all,cases(),finish_all}].

cases() ->
    [badmatch, pending_errors, nil_arith].

-define(try_match(E),
	catch ?MODULE:bar(),
	{'EXIT', {{badmatch, nomatch}, _}} = (catch E = nomatch)).

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_all(Config) when is_list(Config) ->
    ?line test_lib:interpret(?MODULE),
    ?line true = lists:member(?MODULE, int:interpreted()),
    ok.

finish_all(Config) when is_list(Config) ->
    ok.

badmatch(doc) -> "Test that deliberately bad matches are reported correctly.";
badmatch(suite) -> [];
badmatch(Config) when list(Config) ->
    ?line ?try_match(a),
    ?line ?try_match(42),
    ?line ?try_match({a, b, c}),
    ?line ?try_match([]),
    ?line ?try_match(1.0),
    ok.

pending_errors(doc) ->
    ["Test various exceptions, in the presence of a previous error suppressed ",
     "in a guard."];
pending_errors(suite) -> [];
pending_errors(Config) when list(Config) ->
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
    case xxx of
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
    a = b.					% badmatch

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

pending({badarg,[{erlang,Bif,BifArgs},{?MODULE,Func,Arity}|_]}, Func, Args, _Code)
  when atom(Bif), list(BifArgs), length(Args) == Arity -> %Threaded code.
    ok;
pending({badarg,[{erlang,Bif,BifArgs},{?MODULE,Func,Args}|_]}, Func, Args, _Code)
  when atom(Bif), list(BifArgs) -> %From interpreted code.
    ok;
pending({undef,[{non_existing_module,foo,[]}|_]}, _, _, _) ->
    ok;
pending({function_clause,[{?MODULE,Func,Args}|_]}, Func, Args, _Code) ->
    ok;
pending({Code,[{?MODULE,Func,Arity}|_]}, Func, Args, Code) when length(Args) == Arity -> %Threaded code
    ok;
pending({Code,[{?MODULE,Func,Args}|_]}, Func, Args, Code) -> %From interpreted code.
    ok;
pending(Reason, Func, Args, Code) ->
    test_server:fail({bad_exit_reason,Reason,{Func,Args,Code}}).

nil_arith(doc) ->
    "Test that doing arithmetics on [] gives a badarith EXIT and not a crash.";
nil_arith(suite) ->
    [];
nil_arith(Config) when list(Config) ->
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
