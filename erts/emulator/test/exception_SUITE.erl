%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-module(exception_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 badmatch/1, pending_errors/1, nil_arith/1,
         stacktrace/1, nested_stacktrace/1, raise/1, gunilla/1, per/1,
	 exception_with_heap_frag/1, line_numbers/1]).

-export([bad_guy/2]).
-export([crash/1]).

-include_lib("test_server/include/test_server.hrl").
-import(lists, [foreach/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [badmatch, pending_errors, nil_arith, stacktrace,
     nested_stacktrace, raise, gunilla, per,
     exception_with_heap_frag, line_numbers].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


-define(try_match(E),
	catch ?MODULE:bar(),
	{'EXIT', {{badmatch, nomatch}, _}} = (catch E = id(nomatch))).

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

    try
	?line stacktrace_2()
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
    ?line B = odd_even(N, []),
    ?line try even(N) 
	  catch error:function_clause -> ok
	  end,
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

%%
%% Make sure that even if a BIF builds an heap fragment, then causes an exception,
%% the stacktrace term will still be OK (specifically, that it does not contain
%% stale pointers to the arguments).
%%
exception_with_heap_frag(Config) when is_list(Config) ->
    Sizes = lists:seq(0, 512),

    %% Floats are only validated when the heap fragment has been allocated.
    BadFloat = <<131,99,53,46,48,$X,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,101,45,48,49,0,0,0,0,0>>,
    ?line do_exception_with_heap_frag(BadFloat, Sizes),

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
    ?line do_exception_with_heap_frag(BinAndFloat, Sizes),

    %% {Fun,BadFloat}
    FunAndFloat =
	<<131,104,2,112,0,0,0,66,0,238,239,135,138,137,216,89,57,22,111,52,126,16,84,
	 71,8,0,0,0,0,0,0,0,0,100,0,1,116,97,0,98,5,175,169,123,103,100,0,13,110,111,
	 110,111,100,101,64,110,111,104,111,115,116,0,0,0,41,0,0,0,0,0,99,50,46,55,48,
	 $Y,57,57,57,57,57,57,57,57,57,57,57,57,57,54,52,52,55,101,43,48,48,0,0,0,0,0>>,
    ?line do_exception_with_heap_frag(FunAndFloat, Sizes),

    %% [ExternalPid|BadFloat]
    ExtPidAndFloat =
	<<131,108,0,0,0,1,103,100,0,13,107,97,108,108,101,64,115,116,114,105,100,101,
	 114,0,0,0,36,0,0,0,0,2,99,48,46,$@,48,48,48,48,48,48,48,48,48,48,48,48,48,48,
	 48,48,48,48,48,101,43,48,48,0,0,0,0,0>>,
    ?line do_exception_with_heap_frag(ExtPidAndFloat, Sizes),
    
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
