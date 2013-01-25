%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2013. All Rights Reserved.
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
%%% Purpose : Test records.

-module(record_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 errors/1,record_test_2/1,record_test_3/1,record_access_in_guards/1,
	 guard_opt/1,eval_once/1,foobar/1,missing_test_heap/1,
	 nested_access/1,coverage/1]).

init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [errors,record_test_2,record_test_3,
       record_access_in_guards,guard_opt,eval_once,foobar,
       missing_test_heap,nested_access,coverage]}].


init_per_suite(Config) ->
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
    ?line #foo{a=19,b=42,c=3,d=4} = update_foo(Foo, 19, 42),

    ?line {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19)),
    ?line {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19, 35)),
    ?line {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19, 35, 17)),
    ?line {'EXIT',{{badrecord,bar},_}} = (catch update_foo_bar(Foo, 19, 35, 17, 42)),

    ?line {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19)),
    ?line {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19, 35)),
    ?line {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19, 35, 17)),
    ?line {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19, 35, 17, 42)),
    ?line {'EXIT',{{badrecord,barf},_}} = (catch update_foo_barf(Foo, 19,
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


-define(TrueGuard(Expr), if Expr -> ok; true -> ?t:fail() end).
-define(FalseGuard(Expr), if Expr -> ?t:fail(); true -> ok end).
				
record_test_2(Config) when is_list(Config) ->
    ?line true = is_record(#foo{}, foo),
    ?line false = is_record(#foo{}, barf),
    ?line false = is_record({foo}, foo),

    ?line true = erlang:is_record(#foo{}, foo),
    ?line false = erlang:is_record(#foo{}, barf),
    ?line false = erlang:is_record({foo}, foo),

    ?line false = is_record([], foo),
    ?line false = is_record(Config, foo),

    ?line ?TrueGuard(is_record(#foo{}, foo)),
    ?line ?FalseGuard(is_record(#foo{}, barf)),
    ?line ?FalseGuard(is_record({foo}, foo)),

    ?line ?TrueGuard(erlang:is_record(#foo{}, foo)),
    ?line ?FalseGuard(erlang:is_record(#foo{}, barf)),
    ?line ?FalseGuard(erlang:is_record({foo}, foo)),

    ?line ?FalseGuard(is_record([], foo)),
    ?line ?FalseGuard(is_record(Config, foo)),

    %% 'not is_record/2' to test guard optimization.

    ?line ?FalseGuard(not is_record(#foo{}, foo)),
    ?line ?TrueGuard(not is_record(#foo{}, barf)),
    ?line ?TrueGuard(not is_record({foo}, foo)),

    ?line ?FalseGuard(not erlang:is_record(#foo{}, foo)),
    ?line ?TrueGuard(not erlang:is_record(#foo{}, barf)),
    ?line ?TrueGuard(not erlang:is_record({foo}, foo)),

    Foo = id(#foo{}),
    ?line ?FalseGuard(not erlang:is_record(Foo, foo)),
    ?line ?TrueGuard(not erlang:is_record(Foo, barf)),

    ?line ?TrueGuard(not is_record(Config, foo)),

    ?line ?TrueGuard(not is_record(a, foo)),
    ?line ?TrueGuard(not is_record([], foo)),

    %% Pass non-literal first argument.

    ?line true = is_record(id(#foo{}), foo),
    ?line false = is_record(id(#foo{}), barf),
    ?line false = is_record(id({foo}), foo),

    ?line true = erlang:is_record(id(#foo{}), foo),
    ?line false = erlang:is_record(id(#foo{}), barf),
    ?line false = erlang:is_record(id({foo}), foo),

    NoRec1 = id(blurf),
    NoRec2 = id([]),

    ?line ?TrueGuard(not is_record(NoRec1, foo)),
    ?line ?TrueGuard(not is_record(NoRec2, foo)),

    %% The optimizer attempts to move expressions to guards,
    %% but it must not move an is_record/2 call that is not
    %% allowed in a guard in the first place.

    ?line ok = case is_record(id({a}), id(a)) of
		   true -> ok;
		   false -> error
	       end,

    %% Force the use of guard bifs by using the 'xor' operation.

    False = id(false),
    ?line ?TrueGuard(is_record(#foo{}, foo) xor False),
    ?line ?FalseGuard(is_record(#foo{}, barf) xor False),
    ?line ?FalseGuard(is_record({foo}, foo) xor False ),

    ?line ?TrueGuard(is_record(Foo, foo) xor False),
    ?line ?FalseGuard(is_record(Foo, barf) xor False),


    %% Implicit guards by using a list comprehension.

    List = id([1,#foo{a=2},3,#bar{d=4},5,#foo{a=6},7]),

    ?line [#foo{a=2},#foo{a=6}] = [X || X <- List, is_record(X, foo)],
    ?line [#bar{d=4}] = [X || X <- List, is_record(X, bar)],
    ?line [1,#foo{a=2},3,5,#foo{a=6},7] =
	[X || X <- List, not is_record(X, bar)],
    ?line [1,3,5,7] =
	[X || X <- List, ((not is_record(X, bar)) and (not is_record(X, foo)))],
    ?line [#foo{a=2},#bar{d=4},#foo{a=6}] =
	[X || X <- List, ((is_record(X, bar)) or (is_record(X, foo)))],
    ?line [1,3,#bar{d=4}] =
	[X || X <- List, ((is_record(X, bar)) or (X < 5))],

    ?line MyList = [#foo{a=3},x,[],{a,b}],
    ?line [#foo{a=3}] = [X || X <- MyList, is_record(X, foo)],
    ?line [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo)],
    ?line [#foo{a=3}] = [X || X <- MyList, begin is_record(X, foo) end],
    ?line [x,[],{a,b}] = [X || X <- MyList, begin not is_record(X, foo) end],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, is_record(X, foo) or
					     not is_binary(X)],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo) or
					     not is_binary(X)],
    ?line [#foo{a=3}] = [X || X <- MyList, is_record(X, foo) or is_reference(X)],
    ?line [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo) or
				   is_reference(X)],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
					 begin is_record(X, foo) or
						   not is_binary(X) end],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
					 begin not is_record(X, foo) or
						   not is_binary(X) end],
    ?line [#foo{a=3}] = [X || X <- MyList,
			      begin is_record(X, foo) or is_reference(X) end],
    ?line [x,[],{a,b}] = [X || X <- MyList,
			       begin not is_record(X, foo) or
					 is_reference(X) end],

    %% Call is_record/2 with illegal arguments.
    ?line [] = [X || X <- [], is_record(t, id(X))],
    ?line {'EXIT',{badarg,_}} = (catch [X || X <- [1], is_record(t, id(X))]),

    %% Update several fields with a string literal.
    ?line #barf{} = Barf0 = id(#barf{}),
    ?line Barf = update_barf(Barf0),
    ?line #barf{a="abc",b=1} = id(Barf),

    ok.

record_test_3(Config) when is_list(Config) ->
    ?line true = is_record(#foo{}, foo, 5),
    ?line false = is_record(#foo{}, barf, 5),
    ?line false = is_record(#foo{}, barf, 6),
    ?line false = is_record({foo}, foo, 5),

    ?line true = erlang:is_record(#foo{}, foo, 5),
    ?line false = erlang:is_record(#foo{}, barf, 5),
    ?line false = erlang:is_record({foo}, foo, 5),

    ?line false = is_record([], foo),
    ?line false = is_record(Config, foo),

    ?line ?TrueGuard(is_record(#foo{}, foo, 5)),
    ?line ?FalseGuard(is_record(#foo{}, barf, 5)),
    ?line ?FalseGuard(is_record(#foo{}, barf, 6)),
    ?line ?FalseGuard(is_record({foo}, foo, 5)),

    ?line ?TrueGuard(erlang:is_record(#foo{}, foo, 5)),
    ?line ?FalseGuard(erlang:is_record(#foo{}, barf, 5)),
    ?line ?FalseGuard(erlang:is_record(#foo{}, barf, 6)),
    ?line ?FalseGuard(erlang:is_record({foo}, foo, 5)),

    ?line ?FalseGuard(is_record([], foo, 5)),
    ?line ?FalseGuard(is_record(Config, foo, 5)),

    %% 'not is_record/2' to test guard optimization.

    ?line ?FalseGuard(not is_record(#foo{}, foo, 5)),
    ?line ?TrueGuard(not is_record(#foo{}, barf, 6)),
    ?line ?TrueGuard(not is_record({foo}, foo, 5)),

    ?line ?FalseGuard(not erlang:is_record(#foo{}, foo, 5)),
    ?line ?TrueGuard(not erlang:is_record(#foo{}, barf, 5)),
    ?line ?TrueGuard(not erlang:is_record({foo}, foo, 5)),

    Foo = id(#foo{}),
    ?line ?FalseGuard(not erlang:is_record(Foo, foo, 5)),
    ?line ?TrueGuard(not erlang:is_record(Foo, barf, 6)),

    ?line ?TrueGuard(not is_record(Config, foo, 5)),

    ?line ?TrueGuard(not is_record(a, foo, 5)),
    ?line ?TrueGuard(not is_record([], foo, 5)),

    %% Pass non-literal first argument.

    ?line true = is_record(id(#foo{}), foo, 5),
    ?line false = is_record(id(#foo{}), barf, 6),
    ?line false = is_record(id({foo}), foo, 5),

    ?line true = erlang:is_record(id(#foo{}), foo, 5),
    ?line false = erlang:is_record(id(#foo{}), barf, 6),
    ?line false = erlang:is_record(id({foo}), foo, 5),

    NoRec1 = id(blurf),
    NoRec2 = id([]),

    ?line ?TrueGuard(not is_record(NoRec1, foo, 5)),
    ?line ?TrueGuard(not is_record(NoRec2, foo, 5)),

    %% Force the use of guard bifs by using the 'xor' operation.

    False = id(false),
    ?line ?TrueGuard(is_record(#foo{}, foo, 5) xor False),
    ?line ?FalseGuard(is_record(#foo{}, barf, 6) xor False),
    ?line ?FalseGuard(is_record({foo}, foo, 5) xor False ),

    ?line ?TrueGuard(is_record(Foo, foo, 5) xor False),
    ?line ?FalseGuard(is_record(Foo, barf, 6) xor False),


    %% Implicit guards by using a list comprehension.

    List = id([1,#foo{a=2},3,#bar{d=4},5,#foo{a=6},7]),

    ?line [#foo{a=2},#foo{a=6}] = [X || X <- List, is_record(X, foo, 5)],
    ?line [#bar{d=4}] = [X || X <- List, is_record(X, bar, 5)],
    ?line [1,#foo{a=2},3,5,#foo{a=6},7] =
	[X || X <- List, not is_record(X, bar, 5)],
    ?line [1,3,5,7] =
	[X || X <- List, ((not is_record(X, bar, 5)) and (not is_record(X, foo, 5)))],
    ?line [#foo{a=2},#bar{d=4},#foo{a=6}] =
	[X || X <- List, ((is_record(X, bar, 5)) or (is_record(X, foo, 5)))],
    ?line [1,3,#bar{d=4}] =
	[X || X <- List, ((is_record(X, bar, 5)) or (X < 5))],

    ?line MyList = [#foo{a=3},x,[],{a,b}],
    ?line [#foo{a=3}] = [X || X <- MyList, is_record(X, foo, 5)],
    ?line [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo, 5)],
    ?line [#foo{a=3}] = [X || X <- MyList, begin is_record(X, foo, 5) end],
    ?line [x,[],{a,b}] = [X || X <- MyList, begin not is_record(X, foo, 5) end],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, is_record(X, foo, 5) or
					     not is_binary(X)],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo, 5) or
					     not is_binary(X)],
    ?line [#foo{a=3}] = [X || X <- MyList, is_record(X, foo) or is_reference(X)],
    ?line [x,[],{a,b}] = [X || X <- MyList, not is_record(X, foo) or
				   is_reference(X)],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
					 begin is_record(X, foo, 5) or
						   not is_binary(X) end],
    ?line [#foo{a=3},x,[],{a,b}] = [X || X <- MyList,
					 begin not is_record(X, foo, 5) or
						   not is_binary(X) end],
    ?line [#foo{a=3}] = [X || X <- MyList,
			      begin is_record(X, foo, 5) or is_reference(X) end],
    ?line [x,[],{a,b}] = [X || X <- MyList,
			       begin not is_record(X, foo, 5) or
					 is_reference(X) end],

    %% Update several fields with a string literal.
    ?line #barf{} = Barf0 = id(#barf{}),
    ?line Barf = update_barf(Barf0),
    ?line #barf{a="abc",b=1} = id(Barf),

    %% Non-literal arguments.
    ?line true = is_record(id(#barf{}), id(barf), id(6)),
    ?line false = is_record(id(#barf{}), id(barf), id(42)),
    ?line false = is_record(id(#barf{}), id(foo), id(6)),

    ok.

record_access_in_guards(Config) when is_list(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line file:set_cwd(test_lib:get_data_dir(Config)),
    ?line Opts0 = [{outdir,Priv},report_errors|test_lib:opt_opts(?MODULE)],
    M = record_access_in_guards,
 
    Opts = [strict_record_tests|Opts0],
    ?line io:format("Options: ~p\n", [Opts]),
    ?line {ok,M} = c:c(M, Opts),
    ?line ok = M:t(),
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
    ?line once(fun(GetRec) ->
		       true = erlang:is_record(GetRec(), foo)
	       end, #foo{}),
    ?line once(fun(GetRec) ->
		       (GetRec())#foo{a=1}
	       end, #foo{}),
    ?line once(fun(GetRec) ->
		       (GetRec())#foo{a=1,b=2}
	       end, #foo{}),
    ?line once(fun(GetRec) ->
		       (GetRec())#foo{a=1,b=2,c=3}
	       end, #foo{}),
    ?line once(fun(GetRec) ->
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
	    ?t:fail()
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
    ?line <<"nested0">> = N0#nrec0.name,
    ?line <<"nested1">> = N1#nrec1.name,
    ?line <<"nested2">> = N2#nrec2.name,
    ?line <<"nested0">> = N1#nrec1.nrec0#nrec0.name,
    ?line <<"nested0">> = N2#nrec2.nrec1#nrec1.nrec0#nrec0.name,
    ?line <<"nested1">> = N2#nrec2.nrec1#nrec1.name,
    ?line <<"nested0">> = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name,

    N1a = N2#nrec2.nrec1#nrec1{name = <<"nested1a">>},
    ?line <<"nested1a">> = N1a#nrec1.name,

    N2a = N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = <<"nested0a">>},
    N2b = ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0{name = <<"nested0a">>},
    ?line <<"nested0a">> = N2a#nrec0.name,
    ?line N2a = N2b,
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

id(I) -> I.
