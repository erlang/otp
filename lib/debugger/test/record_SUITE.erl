%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
%%% Purpose : Test records.

-module(record_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 errors/1,record_test/1,eval_once/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

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

record_test(Config) when is_list(Config) ->
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
    ok.

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

id(I) -> I.
