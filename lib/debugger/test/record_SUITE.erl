%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%% Purpose : Test records.

-module(record_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 errors/1,record_test/1,eval_once/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [errors, record_test, eval_once].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

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

record_test(Config) when is_list(Config) ->
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
    ok.

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
	    ct:fail(failed)
    end,
    Result.

id(I) -> I.
