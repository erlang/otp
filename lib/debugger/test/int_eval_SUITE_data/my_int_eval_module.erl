%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
-module(my_int_eval_module).
-stupid_attribute({a,b}).

-export([ets_new/0,ets_delete/1,spawn_test/0,apply_test/1,external_call_test/1]).
-export([check_exports/1,check_module_info/2]).
-export([give_me_a_fun_0/0,give_me_a_fun_0/1,give_me_a_fun_1/2,
	 give_me_a_bad_fun/0, do_apply/1, do_apply/2]).
-export([please_call_exit/1,i_will_do_the_exit/1]).
-export([more_catch/1,more_nocatch/1,exit_me/0]).
-export([f/1, f_try/1, f_catch/1]).
-export([otp_5837/1, otp_8310/0]).
-export([empty_map_update/1, update_in_fun/0]).
-export([call_inside_binary/1]).

%% Internal exports.
-export([echo/2,my_subtract/2,catch_a_ball/0,throw_a_ball/0]).
-export([i_am_exported/1]).

-import(lists, [member/2]).

-define(line,put(test_server_loc,{?MODULE,?LINE}),).
-define(t,test_server).
-define(m,test_server:match).
-define(config,test_server:lookup_config).

ets_new() ->
    Id = ets:new(my_int_eval_table, [private]),
    Id.

ets_delete(Tab) ->
    ets:delete(Tab).

%% Spawning.

spawn_test() ->
    Term = {a,tuple},
    Pid = spawn_link(?MODULE, echo, [self(),Term]),
    receive
	{result,Pid,Term} -> ok;
	Other -> {bad_response,Other}
    after 5000 ->
	    timeout
    end.

echo(Parent, Term) ->
    Parent ! {result,self(),Term}.

%% Applying.

apply_test(Fun) ->
    42 = Fun(number(2), number(40)),
    12 = apply(Fun, [number(7),number(5)]),

    Mod = module(),
    Func = func(),
    [a] = Mod:my_subtract(same([a,b,c]), same([b,c])),
    [a,b] = Mod:Func(same([a,b,c]), same([c])),
    [a,b,d] = ?MODULE:Func(same([a,b,c,d]), same([c])),
    [d,e] = apply(Mod, Func, [same([d,e,f]), same([f])]),
    [3] = apply(?MODULE, Func, [same([3,4]),same([4])]),
    ok.

number(X) -> {number,X}.
module() -> ?MODULE.
func() -> my_subtract.
same(X) -> X.

my_subtract(X, Y) -> X -- Y.

%% Catch and throw.

catch_a_ball() ->
    {a,ball} = (catch throw_a_ball()),
    ok.

throw_a_ball() ->
    throw({a,ball}),
    not_ok.

exit_me() ->
    exit({int,exit}).

more_catch(Fun) ->
    case catch lists:filter(Fun, [a]) of
	{'EXIT', {_, exit}} ->
	    ok;
	Else -> Else
    end.

more_nocatch(Fun) ->
    lists:filter(Fun, [a]).

%% External calls.

external_call_test(Data) ->
    {'EXIT',{undef,[{?MODULE,not_exported,[42,Data],_}|_]}} =
	(catch ?MODULE:not_exported(42, Data)),
    {yes,Data} = i_am_exported(Data),
    {yes,Data} = ?MODULE:i_am_exported(Data),

    %% Excercise the function cache in the interpreter.

    {ok,Data,[a,b]} = not_exported(Data, [a,b]),
    {yes,Data} = i_am_exported(Data),
    {ok,Data,[a,b]} = not_exported(Data, [a,b]),
    {'EXIT',{undef,[{?MODULE,not_exported,[7,Data],_}|_]}} =
	(catch ?MODULE:not_exported(7, Data)),
    {yes,Data} = ?MODULE:i_am_exported(Data),
    ok.

not_exported(N, D) ->
    {ok,N,D}.

i_am_exported(D) ->
    {yes,D}.

%% The module_info/0,1 functions and list comprehensions (funs).

check_exports(Exp) ->
    %% Check the structure of the export list and that there are more
    %% than 4 elements.

    Exp = [{F,A} || {F,A} <- Exp, erlang:is_atom(F), erlang:is_integer(A)],
    case length(Exp) of
	Len when Len > 4 -> ok
    end.

check_module_info(ModInfo, Exports) ->
    ModInfo = module_info(),
    Exports = module_info(exports),
    ok.

%% Testcase apply_interpreted_fun/1.

give_me_a_fun_0() ->
    fun() -> perfectly_alright end.

give_me_a_fun_0(Term) ->
    fun() -> {ok,Term} end.

give_me_a_fun_1(Min, Max) ->
    Seq = lists:seq(Min, Max),
    fun (L) when list(L) ->
	    [Info || {Key,Info} <- L, lists:member(Key, Seq)];
	(T) when tuple(T) ->
	    L = tuple_to_list(T),
	    F = fun({Key,Info}) ->
			case lists:member(Key, Seq) of
			    true -> Info;
			    false -> false
			end
		end,
	    list_to_tuple(lists:map(F, L))
    end.

give_me_a_bad_fun() ->
    fun(Arg) -> erlang:error(Arg) end.

do_apply(Fun) ->
    Fun().
do_apply(Fun, Arg) ->
    Fun(Arg).


please_call_exit(Reason) ->
    put(asked_to_call_exit, Reason),
    put(will_call_my_good_friend, ''),
    Res = int_eval_SUITE:applier(?MODULE, i_will_do_the_exit, [Reason]),

    %% We don't want a tail-recursive call above.
    io:format("Returned from exit/1 -- how strange\n").

i_will_do_the_exit(Reason) ->
    exit(Reason).

f(Arg) ->
    g(Arg).

f_try(Arg) ->
    try g(Arg)
    catch
	Class:Reason ->
	    {Class, Reason}
    end.

f_catch(Arg) ->
    catch g(Arg).

g({error, Reason}) ->
    erlang:error(Reason);
g({exit, Reason}) ->
    erlang:exit(Reason);
g({throw, Reason}) ->
    erlang:throw(Reason);
g(Value) ->
    Value.

otp_5837(N) ->
    n(N).

n(N) ->
    lists:map(fun(X) when N==X ->
		      yes;
		 (_) ->
		      no
	      end,
	      [1,2,3,4]).

otp_8310() ->
    a = if (false orelse a) =:= a -> a; true -> b end,
    F1 = fun() -> a end,
    {'EXIT',{{bad_filter,a},_}} =
        (catch {a, [X || X <- [1,2,3], _ = F1()]}),
    F2 = fun() -> << 3:8 >> end,
    {'EXIT',{{bad_filter,<<3>>},_}} =
        (catch {a, << << X >> || << X >> <= << 7:8 >>,_ = F2() >>}),
    {'EXIT',{{bad_generator,a},_}} =
        (catch {a, [X || X <- a]}),
    {'EXIT',{{bad_generator,b},_}} =
        (catch {a, << <<X>>  || << X >> <= b >>}),
    true = begin (X1 = true) andalso X1, X1 end,
    false = begin (X2 = false) andalso X2, X2 end,
    true = begin (X3 = true) orelse X3, X3 end,
    false = begin (X4 = false) orelse X4, X4 end,
    ok.

empty_map_update(Map) -> Map#{}.

update_in_fun() ->
    lists:map(fun (X) -> X#{price := 0} end, [#{hello => 0, price => nil}]).

call_inside_binary(Fun) ->
    <<(Fun(1))/binary>>.
