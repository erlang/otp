%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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
%%% Purpose : Test records.

-module(record_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 errors/1,record_test_2/1,record_test_3/1,record_access_in_guards/1,
	 guard_opt/1,eval_once/1,foobar/1,missing_test_heap/1,
	 nested_access/1,coverage/1,grab_bag/1,slow_compilation/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [errors,record_test_2,record_test_3,
       record_access_in_guards,guard_opt,eval_once,foobar,
       missing_test_heap,nested_access,coverage,grab_bag,
       slow_compilation]}].


init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


-record(foo, {a,b,c,d}).
-record(bar, {a,b,c,d}).
-record(barf, {a,b,c,d,e}).

errors(Config) when is_list(Config) ->
    Foo = #foo{a=1,b=2,c=3,d=4},
    #foo{a=19,b=42,c=3,d=4} = update_foo(Foo, 19, 42),

    {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19)),
    {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19, 35)),
    {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19, 35, 17)),
    {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19, 35, 17, 42)),

    {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19)),
    {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19, 35)),
    {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19, 35, 17)),
    {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19, 35, 17, 42)),
    {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19,
								 35, 17, 42, -2)),

    ok.

update_foo(#foo{}=R, A, B) ->
    R#foo{a=A,b=B}.

update_foo_bar(#foo{}=R, A) ->
    R#bar{a=A}.

update_foo_bar(#foo{}=R, A, _B) ->
    R#bar{a=A,b=A}.

update_foo_bar(#foo{}=R, A, _B, C) ->
    R#bar{a=A,b=A,c=C}.

update_foo_bar(#foo{}=R, A, _B, C, D) ->
    R#bar{a=A,b=A,c=C,d=D}.

update_foo_barf(#foo{}=R, A) ->
    R#barf{a=A}.

update_foo_barf(#foo{}=R, A, _B) ->
    R#barf{a=A,b=A}.

update_foo_barf(#foo{}=R, A, _B, C) ->
    R#barf{a=A,b=A,c=C}.

update_foo_barf(#foo{}=R, A, _B, C, D) ->
    R#barf{a=A,b=A,c=C,d=D}.

update_foo_barf(#foo{}=R, A, _B, C, D, E) ->
    R#barf{a=A,b=A,c=C,d=D,e=E}.


-define(TrueGuard(Expr), if Expr -> ok; true -> ct:fail(failed) end).
-define(FalseGuard(Expr), if Expr -> ct:fail(failed); true -> ok end).
				
record_test_2(Config) when is_list(Config) ->
    true = is_record(#foo{}, foo),
    false = is_record(#foo{}, barf),
    false = is_record({foo}, foo),

    true = erlang:is_record(#foo{}, foo),
    false = erlang:is_record(#foo{}, barf),
    false = erlang:is_record({foo}, foo),

    false = is_record([], foo),
    false = is_record(Config, foo),

    ?TrueGuard(is_record(#foo{}, foo)),
    ?FalseGuard(is_record(#foo{}, barf)),
    ?FalseGuard(is_record({foo}, foo)),

    ?TrueGuard(erlang:is_record(#foo{}, foo)),
    ?FalseGuard(erlang:is_record(#foo{}, barf)),
    ?FalseGuard(erlang:is_record({foo}, foo)),

    ?FalseGuard(is_record([], foo)),
    ?FalseGuard(is_record(Config, foo)),

    %% 'not is_record/2' to test guard optimization.

    ?FalseGuard(not is_record(#foo{}, foo)),
    ?TrueGuard(not is_record(#foo{}, barf)),
    ?TrueGuard(not is_record({foo}, foo)),

    ?FalseGuard(not erlang:is_record(#foo{}, foo)),
    ?TrueGuard(not erlang:is_record(#foo{}, barf)),
    ?TrueGuard(not erlang:is_record({foo}, foo)),

    Foo = id(#foo{}),
    ?FalseGuard(not erlang:is_record(Foo, foo)),
    ?TrueGuard(not erlang:is_record(Foo, barf)),

    ?TrueGuard(not is_record(Config, foo)),

    ?TrueGuard(not is_record(a, foo)),
    ?TrueGuard(not is_record([], foo)),

    %% Pass non-literal first argument.

    true = is_record(id(#foo{}), foo),
    false = is_record(id(#foo{}), barf),
    false = is_record(id({foo}), foo),

    true = erlang:is_record(id(#foo{}), foo),
    false = erlang:is_record(id(#foo{}), barf),
    false = erlang:is_record(id({foo}), foo),

    NoRec1 = id(blurf),
    NoRec2 = id([]),

    ?TrueGuard(not is_record(NoRec1, foo)),
    ?TrueGuard(not is_record(NoRec2, foo)),

    %% The optimizer attempts to move expressions to guards,
    %% but it must not move an is_record/2 call that is not
    %% allowed in a guard in the first place.

    ok = case is_record(id({a}), id(a)) of
		   true -> ok;
		   false -> error
	       end,

    %% Force the use of guard bifs by using the 'xor' operation.

    False = id(false),
    ?TrueGuard(is_record(#foo{}, foo) xor False),
    ?FalseGuard(is_record(#foo{}, barf) xor False),
    ?FalseGuard(is_record({foo}, foo) xor False ),

    ?TrueGuard(is_record(Foo, foo) xor False),
    ?FalseGuard(is_record(Foo, barf) xor False),


    %% Implicit guards by using a list comprehension.

    List = id([1,#foo{a=2},3,#bar{d=4},5,#foo{a=6},7]),

    [#foo{a=2},#foo{a=6}] = [X || X <- List, is_record(X, foo)],
    [#bar{d=4}] = [X || X <- List, is_record(X, bar)],
    [1,#foo{a=2},3,5,#foo{a=6},7] =
	[X || X <- List, not is_record(X, bar)],
    [1,3,5,7] =
	[X || X <- List, ((not is_record(X, bar)) and (not is_record(X, foo)))],
    [#foo{a=2},#bar{d=4},#foo{a=6}] =
	[X || X <- List, ((is_record(X, bar)) or (is_record(X, foo)))],
    [1,3,#bar{d=4}] =
	[X || X <- List, ((is_record(X, bar)) or (X < 5))],

    MyList = [#foo{a=3},x,[],{a,b}],
    [#foo{a=3}] = [X || X <- MyList, is_record(X, foo)],
    [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo)],
    [#foo{a=3}] = [X || X <- MyList, begin is_record(X, foo) end],
    [x,[],{a,b}] = [X || X <- MyList, begin not is_record(X, foo) end],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, is_record(X, foo) or
				       not is_binary(X)],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo) or
				       not is_binary(X)],
    [#foo{a=3}] = [X || X <- MyList, is_record(X, foo) or is_reference(X)],
    [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo) or
			     is_reference(X)],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
				   begin is_record(X, foo) or
					     not is_binary(X) end],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
				   begin not is_record(X, foo) or
					     not is_binary(X) end],
    [#foo{a=3}] = [X || X <- MyList,
			begin is_record(X, foo) or is_reference(X) end],
    [x,[],{a,b}] = [X || X <- MyList,
			 begin not is_record(X, foo) or
				   is_reference(X) end],

    %% Call is_record/2 with illegal arguments.
    [] = [X || X <- [], is_record(t, id(X))],
    {'EXIT',{badarg,_}} = (catch [X || X <- [1], is_record(t, id(X))]),

    %% Update several fields with a string literal.
    #barf{} = Barf0 = id(#barf{}),
    Barf = update_barf(Barf0),
    #barf{a="abc",b=1} = id(Barf),

    %% Test optimization of is_record/3.
    false = case id({a,b}) of
		{_,_}=Tuple -> is_record(Tuple, foo)
	    end,
    false = case id(true) of
		true=Bool -> is_record(Bool, foo)
	    end,

    ok.

record_test_3(Config) when is_list(Config) ->
    true = is_record(#foo{}, foo, 5),
    false = is_record(#foo{}, barf, 5),
    false = is_record(#foo{}, barf, 6),
    false = is_record({foo}, foo, 5),

    true = erlang:is_record(#foo{}, foo, 5),
    false = erlang:is_record(#foo{}, barf, 5),
    false = erlang:is_record({foo}, foo, 5),

    false = is_record([], foo),
    false = is_record(Config, foo),

    ?TrueGuard(is_record(#foo{}, foo, 5)),
    ?FalseGuard(is_record(#foo{}, barf, 5)),
    ?FalseGuard(is_record(#foo{}, barf, 6)),
    ?FalseGuard(is_record({foo}, foo, 5)),

    ?TrueGuard(erlang:is_record(#foo{}, foo, 5)),
    ?FalseGuard(erlang:is_record(#foo{}, barf, 5)),
    ?FalseGuard(erlang:is_record(#foo{}, barf, 6)),
    ?FalseGuard(erlang:is_record({foo}, foo, 5)),

    ?FalseGuard(is_record([], foo, 5)),
    ?FalseGuard(is_record(Config, foo, 5)),

    %% 'not is_record/2' to test guard optimization.

    ?FalseGuard(not is_record(#foo{}, foo, 5)),
    ?TrueGuard(not is_record(#foo{}, barf, 6)),
    ?TrueGuard(not is_record({foo}, foo, 5)),

    ?FalseGuard(not erlang:is_record(#foo{}, foo, 5)),
    ?TrueGuard(not erlang:is_record(#foo{}, barf, 5)),
    ?TrueGuard(not erlang:is_record({foo}, foo, 5)),

    Foo = id(#foo{}),
    ?FalseGuard(not erlang:is_record(Foo, foo, 5)),
    ?TrueGuard(not erlang:is_record(Foo, barf, 6)),

    ?TrueGuard(not is_record(Config, foo, 5)),

    ?TrueGuard(not is_record(a, foo, 5)),
    ?TrueGuard(not is_record([], foo, 5)),

    %% Pass non-literal first argument.

    true = is_record(id(#foo{}), foo, 5),
    false = is_record(id(#foo{}), barf, 6),
    false = is_record(id({foo}), foo, 5),

    true = erlang:is_record(id(#foo{}), foo, 5),
    false = erlang:is_record(id(#foo{}), barf, 6),
    false = erlang:is_record(id({foo}), foo, 5),

    NoRec1 = id(blurf),
    NoRec2 = id([]),

    ?TrueGuard(not is_record(NoRec1, foo, 5)),
    ?TrueGuard(not is_record(NoRec2, foo, 5)),

    %% Force the use of guard bifs by using the 'xor' operation.

    False = id(false),
    ?TrueGuard(is_record(#foo{}, foo, 5) xor False),
    ?FalseGuard(is_record(#foo{}, barf, 6) xor False),
    ?FalseGuard(is_record({foo}, foo, 5) xor False ),

    ?TrueGuard(is_record(Foo, foo, 5) xor False),
    ?FalseGuard(is_record(Foo, barf, 6) xor False),


    %% Implicit guards by using a list comprehension.

    List = id([1,#foo{a=2},3,#bar{d=4},5,#foo{a=6},7]),

    [#foo{a=2},#foo{a=6}] = [X || X <- List, is_record(X, foo, 5)],
    [#bar{d=4}] = [X || X <- List, is_record(X, bar, 5)],
    [1,#foo{a=2},3,5,#foo{a=6},7] =
	[X || X <- List, not is_record(X, bar, 5)],
    [1,3,5,7] =
	[X || X <- List, ((not is_record(X, bar, 5)) and (not is_record(X, foo, 5)))],
    [#foo{a=2},#bar{d=4},#foo{a=6}] =
	[X || X <- List, ((is_record(X, bar, 5)) or (is_record(X, foo, 5)))],
    [1,3,#bar{d=4}] =
	[X || X <- List, ((is_record(X, bar, 5)) or (X < 5))],

    MyList = [#foo{a=3},x,[],{a,b}],
    [#foo{a=3}] = [X || X <- MyList, is_record(X, foo, 5)],
    [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo, 5)],
    [#foo{a=3}] = [X || X <- MyList, begin is_record(X, foo, 5) end],
    [x,[],{a,b}] = [X || X <- MyList, begin not is_record(X, foo, 5) end],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, is_record(X, foo, 5) or
				       not is_binary(X)],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo, 5) or
				       not is_binary(X)],
    [#foo{a=3}] = [X || X <- MyList, is_record(X, foo) or is_reference(X)],
    [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo) or
			     is_reference(X)],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
				   begin is_record(X, foo, 5) or
					     not is_binary(X) end],
    [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
				   begin not is_record(X, foo, 5) or
					     not is_binary(X) end],
    [#foo{a=3}] = [X || X <- MyList,
			begin is_record(X, foo, 5) or is_reference(X) end],
    [x,[],{a,b}] = [X || X <- MyList,
			 begin not is_record(X, foo, 5) or
				   is_reference(X) end],

    %% Update several fields with a string literal.
    #barf{} = Barf0 = id(#barf{}),
    Barf = update_barf(Barf0),
    #barf{a="abc",b=1} = id(Barf),

    %% Non-literal arguments.
    true = is_record(id(#barf{}), id(barf), id(6)),
    false = is_record(id(#barf{}), id(barf), id(42)),
    false = is_record(id(#barf{}), id(foo), id(6)),

    Rec = id(#barf{}),
    Good = id(barf),
    Bad = id(foo),
    Size = id(6),

    true = is_record(Rec, Good, Size) orelse error,
    error = is_record(Rec, Bad, Size) orelse error,

    ok.

record_access_in_guards(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    file:set_cwd(test_lib:get_data_dir(Config)),
    Opts0 = [{outdir,Priv},report_errors|test_lib:opt_opts(?MODULE)],
    M = record_access_in_guards,
 
    Opts = [strict_record_tests|Opts0],
    io:format("Options: ~p\n", [Opts]),
    {ok,M} = c:c(M, Opts),
    ok = M:t(),
    ok.


%% Test optimization of record access and is_record/2 in guards.

-record(r, {a = 4,b}).
-record(r1, {a,b}).
-record(r2, {a = #r1{},b,c=length([1,2,3])}).
-record(r3, {a = fun(_) -> #r1{} end(1), b}).

guard_opt(Config) when is_list(Config) ->
    ok = fun() ->
		 F = fun(F, [H,H|T]) when is_record(H, r) ->
			     [H|F(F, T)];
			(F, [H|T]) when is_record(H, r) ->
			     [H|F(F, T)];
			(_, []) -> []
		     end,
		 [#r{a=4,b=7},#r{a=1,b=42}] =
		     F(F, [#r{a=4,b=7},#r{a=4,b=7},#r{a=1,b=42}]),
		 {'EXIT',_} = (catch F(F, [#r1{}])),
		 ok
	 end(),

    true = fun() ->
		   R = #r{},
		   if is_record(R, r) -> true; true -> false end
	   end(),

    ok = fun() ->
		 F = fun(true, B) when B#r1.a -> ok;
			(false, _) -> error
		     end,
		 ok = F(true, #r1{a=true}),
		 error = F(false, anything_goes),
		 {'EXIT',_} = (catch F(true, #r1{})),
		 {'EXIT',_} = (catch F(true, #r{})),
		 ok
	 end(),

    ok = fun() ->
		 F = fun([{a,R}=T]) when R#r.a =:= 42 ->
			     {ok,tuple_size(T)};
			([{a,R}=T]) when R#r1.a =:= 7 ->
			     {ok,tuple_size(T)};
			(_) -> error
		     end,
		 {ok,2} = F([{a,#r{a=42}}]),
		 {ok,2} = F([{a,#r1{a=7}}]),
		 error = F([{a,#r1{}}]),
		 error = F({a,b,c}),
		 error = F([]),
		 ok
	 end(),

    ok = fun() ->
		 F = fun(X, Y, Z) when is_record(X, r1) andalso
				       (is_record(Y, r2) orelse
					is_record(Z, r3)) -> true;
			(_, _, _) -> false
		     end,
		 true = F(#r1{}, #r2{}, #r3{}),
		 true = F(#r1{}, #r2{}, blurf),
		 true = F(#r1{}, blurf, #r3{}),
		 false = F(#r1{}, blurf, blurf),
		 false = F(blurf, #r2{}, #r3{}),
		 false = F(blurf, #r2{}, blurf),
		 false = F(blurf, blurf, #r3{}),
		 false = F(blurf, blurf, blurf),
		 ok
	 end(),

    ok = fun() ->
		 F = fun(R=#r{a=42}) when R#r.b =:= 7 ->
			     {ok,R};
			(_) -> error
		     end,
		 {ok,#r{a=42,b=7}} = F(#r{a=42,b=7}),
		 error = F(#r{}),
		 error = F([a,b,c]),
		 ok
	 end(),

    ok.

update_barf(R) ->
    R#barf{a="abc",b=1}.

eval_once(Config) when is_list(Config) ->
    once(fun(GetRec) ->
		       true = erlang:is_record(GetRec(), foo)
	       end, #foo{}),
    once(fun(GetRec) ->
		       (GetRec())#foo{a=1}
	       end, #foo{}),
    once(fun(GetRec) ->
		       (GetRec())#foo{a=1,b=2}
	       end, #foo{}),
    once(fun(GetRec) ->
		       (GetRec())#foo{a=1,b=2,c=3}
	       end, #foo{}),
    once(fun(GetRec) ->
		       (GetRec())#foo{a=1,b=2,c=3,d=4}
	       end, #foo{}),
    ok.

once(Test, Record) ->
    put(?MODULE, 0),
    GetRec = fun() ->
		     put(?MODULE, 1+get(?MODULE)),
		     Record
	     end,
    Result = Test(GetRec),
    case get(?MODULE) of
	1 -> ok;
	N ->
	    io:format("Evaluated ~w times\n", [N]),
	    ct:fail(more_than_once)
    end,
    Result.

%% Thanks to Martin Bjorklund.

-record(foobar, {status}).

foobar(Config) when is_list(Config) ->
    {ok,_,_} = x({foo, 1}),
    ok.
				       
get_bar() ->
    #foobar{status = 1}.

x(Trans) ->
    {foo, Barno} = Trans,
    case get_bar() of
	Bar when Bar#foobar.status == 1 ->
	    noop(Bar),
	    Bar33 = Bar#foobar{status = 1},
	    {ok, Bar33, Barno};
	_ ->
	    Trans
    end.

noop(_) ->
    ok.

-record(foo_rec,
	{foo_1,
	 foo_2 = 0,
	 foo_3 = 0}).

missing_test_heap(Config) when is_list(Config) ->
    #foo_rec{foo_2=2,foo_3=5} = missing_test_heap_1(#foo_rec{foo_2=1,foo_3=4}),
    ok.


%% Two test_heap instructions would be incorrectly merged (not allowed
%% because of gc_bif instructions for addition).
missing_test_heap_1(A = #foo_rec {foo_1 = _B,
				  foo_3 = C,
				  foo_2 = D}) ->
    A#foo_rec {foo_1 = {C, D},
  	       foo_3 = C + 1,
  	       foo_2 = D + 1}.

-record(nrec0, {name = <<"nested0">>}).
-record(nrec1, {name = <<"nested1">>, nrec0=#nrec0{}}).
-record(nrec2, {name = <<"nested2">>, nrec1=#nrec1{}}).

nested_access(Config) when is_list(Config) ->
    N0 = #nrec0{},
    N1 = #nrec1{},
    N2 = #nrec2{},
    <<"nested0">> = N0#nrec0.name,
    <<"nested1">> = N1#nrec1.name,
    <<"nested2">> = N2#nrec2.name,
    <<"nested0">> = N1#nrec1.nrec0#nrec0.name,
    <<"nested0">> = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
    <<"nested1">> = N2#nrec2.nrec1#nrec1.name,
    <<"nested0">> = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name,

    N1a = N2#nrec2.nrec1#nrec1{name = <<"nested1a">>},
    <<"nested1a">> = N1a#nrec1.name,

    N2a = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = <<"nested0a">>},
    N2b = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0{name = <<"nested0a">>},
    <<"nested0a">> = N2a#nrec0.name,
    N2a = N2b,
    ok.

-record(rr, {a,b,c}).

coverage(Config) when is_list(Config) ->
    %% There should only remain one record test in the code below.
    R0 = id(#rr{a=1,b=2,c=3}),
    B = R0#rr.b,				%Test the record here.
    R = R0#rr{c=42},				%No need to test here.
    if 
	B > R#rr.a ->				%No need to test here.
	    ok
    end,
    #rr{a=1,b=2,c=42} = id(R),			%Test for correctness.
    ok.


-record(default_fun, {a = fun(X) -> X*X end}).

%% compiler treats records with 1 and 2 fields differently...
-record(gb_nil, {}).
-record(gb_foo, {hello=1}).
-record(gb_bar, {hello=2,there=3}).

%% Taken from compilation_SUITE.
grab_bag(_Config) ->
    T1 = fun() ->
		 X = #foo{},
		 Y = #foo{},
		 {X#foo.a == Y#foo.a,X#foo.b}
	 end,
    {true,undefined} = T1(),

    T2 = fun(X, Y) ->
		 first_arg(X#foo.a =/= Y#foo.a, X#foo.b =/= X#foo.b)
	 end,
    true = T2(#foo{a=x,b=z}, #foo{a=y,b=z}),

    T3 = fun() ->
		 #default_fun{a=Fun} = id(#default_fun{}),
		 9 = Fun(3)
	 end,
    T3(),

    %% Stupid code, but the compiler used to crash.
    T4 = fun() ->
		 F0 = fun() ->
			      R1 = #gb_nil{},
			      R2 = R1#gb_nil{},
			      R1 = R2
		      end,
		 F1 = fun() ->
			      R1 = #gb_foo{},
			      R2 = R1#gb_foo{},
			      R1 = R2
		      end,

		 F2 = fun() ->
			      R1 = #gb_bar{},
			      R2 = R1#gb_bar{},
			      R1 = R2
		      end,
		 F0(),
		 F1(),
		 F2()
	 end,
    T4(),

    ok.

%% ERIERL-436; the following code used to be very slow to compile.
%%
%% #slow_r{} should have about 4x as many fields for the test to be effective
%% (all of them matched in slow_compilation/1), but unfortunately the memory
%% use scales together with the speed so we'll run out of memory on many of
%% our test machines before we reach noticeable levels (2+ minutes before the
%% fix).
%%
%% We've therefore scaled it down to the current level, at least it it'll guard
%% against excessive regressions.

-record(slow_r,
        {f0,  f1, f2, f3, f4, f5, f6, f7, f8, f9,
         f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,
         f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,
         f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,
         f40,f41,f42,f43,f44,f45,f46,f47,f48,f49,
         f50,f51,f52,f53,f54,f55,f56,f57,f58,f59}).

slow_compilation(Config) when is_list(Config) ->
    R = id(#slow_r{}),

    [{f0,R#slow_r.f0},{f1,R#slow_r.f0},{f1,R#slow_r.f1},
     {f2,R#slow_r.f2},{f3,R#slow_r.f3},{f4,R#slow_r.f4},
     {f5,R#slow_r.f5},{f6,R#slow_r.f6},{f7,R#slow_r.f7},
     {f8,R#slow_r.f8},{f9,R#slow_r.f9},{f10,R#slow_r.f10},
     {f11,R#slow_r.f11},{f12,R#slow_r.f12},{f13,R#slow_r.f13},
     {f14,R#slow_r.f14},{f15,R#slow_r.f15},{f16,R#slow_r.f16},
     {f17,R#slow_r.f17},{f18,R#slow_r.f18},{f19,R#slow_r.f19},
     {f20,R#slow_r.f20},{f21,R#slow_r.f21},{f22,R#slow_r.f22},
     {f23,R#slow_r.f23},{f24,R#slow_r.f24},{f25,R#slow_r.f25},
     {f26,R#slow_r.f26},{f27,R#slow_r.f27},{f28,R#slow_r.f28},
     {f29,R#slow_r.f29},{f30,R#slow_r.f30},{f31,R#slow_r.f31},
     {f32,R#slow_r.f32},{f33,R#slow_r.f33},{f34,R#slow_r.f34},
     {f35,R#slow_r.f35},{f36,R#slow_r.f36},{f37,R#slow_r.f37},
     {f38,R#slow_r.f38},{f39,R#slow_r.f39},{f40,R#slow_r.f40},
     {f41,R#slow_r.f41},{f42,R#slow_r.f42},{f43,R#slow_r.f43},
     {f44,R#slow_r.f44},{f45,R#slow_r.f45},{f46,R#slow_r.f46},
     {f47,R#slow_r.f47},{f48,R#slow_r.f48},{f49,R#slow_r.f49},
     {f40,R#slow_r.f50},{f51,R#slow_r.f51},{f52,R#slow_r.f52},
     {f53,R#slow_r.f53},{f54,R#slow_r.f54},{f55,R#slow_r.f55},
     {f56,R#slow_r.f56},{f57,R#slow_r.f57},{f58,R#slow_r.f58},
     {f59,R#slow_r.f59}].

first_arg(First, _) -> First.

id(I) -> I.
