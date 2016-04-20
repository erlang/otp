%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-module(trycatch_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 basic/1,lean_throw/1,try_of/1,try_after/1,%after_bind/1,
	 catch_oops/1,after_oops/1,eclectic/1,rethrow/1,
	 nested_of/1,nested_catch/1,nested_after/1]).

-include_lib("common_test/include/ct.hrl").

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
    [basic, lean_throw, try_of, try_after, catch_oops,
     after_oops, eclectic, rethrow, nested_of, nested_catch,
     nested_after].

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

basic(Conf) when is_list(Conf) ->
    2 =
	try my_div(4, 2)
	catch
            Class:Reason -> {Class,Reason}
	end,
    error =
        try my_div(1, 0)
        catch
            error:badarith -> error
        end,
    error =
        try 1/0
        catch
            error:badarith -> error
        end,
    ok =
        try my_add(53, atom)
        catch
            error:badarith -> ok
        end,
    exit_nisse =
        try exit(nisse)
	catch
            exit:nisse -> exit_nisse
        end,
    ok =
        try throw(kalle)
        catch
            kalle -> ok
        end,

    %% Try some stuff where the compiler will optimize away the try.

    V = id({a,variable}),
    V = try V catch nisse -> error end,
    42 = try 42 catch nisse -> error end,
    [V] = try [V] catch nisse -> error end,
    {ok,V} = try {ok,V} catch nisse -> error end,

    %% Same idea, but use an after too.

    V = try V catch nisse -> error after after_call() end,
    after_clean(),
    42 = try 42 after after_call() end,
    after_clean(),
    [V] = try [V] catch nisse -> error after after_call() end,
    after_clean(),
    {ok,V} = try {ok,V} after after_call() end,

    %% Try/of
    ok = try V of
	     {a,variable} -> ok
	 catch nisse -> erro
	 end,

    ok.

after_call() ->
    put(basic, after_was_called).

after_clean() ->
    after_was_called = erase(basic).

lean_throw(Conf) when is_list(Conf) ->
    {throw,kalle} =
        try throw(kalle)
        catch
            Kalle -> {throw,Kalle}
        end,
    {exit,kalle} =
        try exit(kalle)
        catch
            Throw1 -> {throw,Throw1};
	    exit:Reason1 -> {exit,Reason1}
        end,
    {exit,kalle} =
        try exit(kalle)
        catch
	    exit:Reason2 -> {exit,Reason2};
            Throw2 -> {throw,Throw2}
        end,
    {exit,kalle} =
        try try exit(kalle)
            catch
                Throw3 -> {throw,Throw3}
            end
        catch
            exit:Reason3 -> {exit,Reason3}
        end,
    ok.

try_of(Conf) when is_list(Conf) ->
    {ok,{some,content}} =
	try_of_1({value,{good,{some,content}}}),
    {error,[other,content]} =
	try_of_1({value,{bad,[other,content]}}),
    {caught,{exit,{ex,it,[reason]}}} =
	try_of_1({exit,{ex,it,[reason]}}),
    {caught,{throw,[term,{in,a,{tuple}}]}} =
	try_of_1({throw,[term,{in,a,{tuple}}]}),
    {caught,{error,[bad,arg]}} =
	try_of_1({error,[bad,arg]}),
    {caught,{error,badarith}} =
	try_of_1({'div',{1,0}}),
    {caught,{error,badarith}} =
	try_of_1({'add',{a,0}}),
    {caught,{error,badarg}} =
	try_of_1({'abs',x}),
    {caught,{error,function_clause}} =
	try_of_1(illegal),
    {error,{try_clause,{some,other_garbage}}} =
	try try_of_1({value,{some,other_garbage}})
        catch error:Reason -> {error,Reason}
        end,
    ok.

try_of_1(X) ->
    try foo(X) of
        {good,Y} -> {ok,Y};
	{bad,Y} -> {error,Y}
    catch
	Class:Reason ->
	    {caught,{Class,Reason}}
    end.

try_after(Conf) when is_list(Conf) ->
    {{ok,[some,value],undefined},finalized} =
	try_after_1({value,{ok,[some,value]}},finalized),
    {{error,badarith,undefined},finalized} =
	try_after_1({'div',{1,0}},finalized),
    {{error,badarith,undefined},finalized} =
	try_after_1({'add',{1,a}},finalized),
    {{error,badarg,undefined},finalized} =
	try_after_1({'abs',a},finalized),
    {{error,[the,{reason}],undefined},finalized} =
	try_after_1({error,[the,{reason}]},finalized),
    {{throw,{thrown,[reason]},undefined},finalized} =
	try_after_1({throw,{thrown,[reason]}},finalized),
    {{exit,{exited,{reason}},undefined},finalized} =
	try_after_1({exit,{exited,{reason}}},finalized),
    {{error,function_clause,undefined},finalized} =
	try_after_1(function_clause,finalized),
    ok =
	try try_after_1({'add',{1,1}}, finalized)
        catch
            error:{try_clause,2} -> ok
	end,
    finalized = erase(try_after),
    ok =
        try try foo({exit,[reaso,{n}]})
            after put(try_after, finalized)
            end
        catch
            exit:[reaso,{n}] -> ok
        end,
    ok.

try_after_1(X, Y) ->
    erase(try_after),
    Try =
        try foo(X) of
	    {ok,Value} -> {ok,Value,get(try_after)}
        catch
	    Reason -> {throw,Reason,get(try_after)};
	    error:Reason -> {error,Reason,get(try_after)};
	    exit:Reason ->  {exit,Reason,get(try_after)}
        after
	    put(try_after, Y)
        end,
    {Try,erase(try_after)}.

-ifdef(begone).

after_bind(Conf) when is_list(Conf) ->
    V = [make_ref(),self()|value],
    {value,{value,V}} =
	after_bind_1({value,V}, V, {value,V}),
    ok.

after_bind_1(X, V, Y) ->
    try
        Try =
            try foo(X) of
                V -> value
            catch
                C1:V -> {caught,C1}
            after
                After = foo(Y)
	    end,
        {Try,After}
    of
        V -> {value,V}
    catch
        C:D -> {caught,{C,D}}
    end.

-endif.

catch_oops(Conf) when is_list(Conf) ->
	V = {v,[a,l|u],{e},self()},
	{value,V} = catch_oops_1({value,V}),
	{value,1} = catch_oops_1({'div',{1,1}}),
	{error,badarith} = catch_oops_1({'div',{1,0}}),
	{error,function_clause} = catch_oops_1(function_clause),
	{throw,V} = catch_oops_1({throw,V}),
	{exit,V} = catch_oops_1({exit,V}),
	ok.

catch_oops_1(X) ->
	Ref = make_ref(),
	try try foo({error,Ref})
	    catch
		error:Ref ->
		    foo(X)
	    end of
	    Value -> {value,Value}
	catch
	    Class:Data -> {Class,Data}
	end.



after_oops(Conf) when is_list(Conf) ->
    V = {self(),make_ref()},
    {{value,V},V} = after_oops_1({value,V}, {value,V}),
    {{exit,V},V} = after_oops_1({exit,V}, {value,V}),
    {{error,V},undefined} = after_oops_1({value,V}, {error,V}),
    {{error,function_clause},undefined} =
	after_oops_1({exit,V}, function_clause),
    ok.

after_oops_1(X, Y) ->
    erase(after_oops),
    Try =
        try try foo(X)
            after
                put(after_oops, foo(Y))
            end of
            V -> {value,V}
        catch
            C:D -> {C,D}
        end,
    {Try,erase(after_oops)}.



eclectic(Conf) when is_list(Conf) ->
    V = {make_ref(),3.1415926535,[[]|{}]},
    {{value,{value,V},V},V} =
	eclectic_1({foo,{value,{value,V}}}, undefined, {value,V}),
    {{'EXIT',{V,[{?MODULE,foo,_,_}|_]}},V} =
	eclectic_1({catch_foo,{error,V}}, undefined, {value,V}),
    {{error,{exit,V},{'EXIT',V}},V} =
	eclectic_1({foo,{error,{exit,V}}}, error, {value,V}),
    {{value,{value,V},V},{'EXIT',{badarith,[{?MODULE,my_add,_,_}|_]}}} =
	eclectic_1({foo,{value,{value,V}}}, undefined, {'add',{0,a}}),
    {{'EXIT',V},V} =
	eclectic_1({catch_foo,{exit,V}}, undefined, {throw,V}),
    {{error,{'div',{1,0}},{'EXIT',{badarith,[{?MODULE,my_div,_,_}|_]}}},
     {'EXIT',V}} =
	eclectic_1({foo,{error,{'div',{1,0}}}}, error, {exit,V}),
    {{{error,V},{'EXIT',{V,[{?MODULE,foo,_,_}|_]}}},{'EXIT',V}} =
	eclectic_1({catch_foo,{throw,{error,V}}}, undefined, {exit,V}),
    %%
    {{value,{value,{value,V},V}},V} =
	eclectic_2({value,{value,V}}, undefined, {value,V}),
    {{value,{throw,{value,V},V}},V} =
	eclectic_2({throw,{value,V}}, throw, {value,V}),
    {{caught,{'EXIT',V}},undefined} =
	eclectic_2({value,{value,V}}, undefined, {exit,V}),
    {{caught,{'EXIT',{V,[{?MODULE,foo,_,_}|_]}}},undefined} =
	eclectic_2({error,{value,V}}, throw, {error,V}),
    {{caught,{'EXIT',{badarg,[{erlang,abs,[V],_}|_]}}},V} =
	eclectic_2({value,{'abs',V}}, undefined, {value,V}),
    {{caught,{'EXIT',{badarith,[{?MODULE,my_add,_,_}|_]}}},V} =
	eclectic_2({exit,{'add',{0,a}}}, exit, {value,V}),
    {{caught,{'EXIT',V}},undefined} =
	eclectic_2({value,{error,V}}, undefined, {exit,V}),
    {{caught,{'EXIT',{V,[{?MODULE,foo,_,_}|_]}}},undefined} =
	eclectic_2({throw,{'div',{1,0}}}, throw, {error,V}),
    ok.

eclectic_1(X, C, Y) ->
    erase(eclectic),
    Done = make_ref(),
    Try =
        try case X of
		{catch_foo,V} -> catch {Done,foo(V)};
		{foo,V} -> {Done,foo(V)}
	    end of
            {Done,D} -> {value,D,catch foo(D)};
	    {'EXIT',_}=Exit -> Exit;
	    D -> {D,catch foo(D)}
        catch
            C:D -> {C,D,catch foo(D)}
        after
            put(eclectic, catch foo(Y))
        end,
    {Try,erase(eclectic)}.

eclectic_2(X, C, Y) ->
    Done = make_ref(),
    erase(eclectic),
    Catch =
	case
            catch
		{Done,
		 try foo(X) of
		     V -> {value,V,foo(V)}
		 catch
		     C:D -> {C,D,foo(D)}
		 after
		     put(eclectic, foo(Y))
		 end} of
		{Done,Z} -> {value,Z};
		Z -> {caught,Z}
	    end,
    {Catch,erase(eclectic)}.

rethrow(Conf) when is_list(Conf) ->
    V = {a,[b,{c,self()},make_ref]},
    {value2,value1} =
	rethrow_1({value,V}, V),
    {caught2,{error,V}} =
	rethrow_2({error,V}, undefined),
    {caught2,{exit,V}} =
	rethrow_1({exit,V}, error),
    {caught2,{throw,V}} =
	rethrow_1({throw,V}, undefined),
    {caught2,{throw,V}} =
	rethrow_2({throw,V}, undefined),
    {caught2,{error,badarith}} =
	rethrow_1({'add',{0,a}}, throw),
    {caught2,{error,function_clause}} =
	rethrow_2(function_clause, undefined),
    {caught2,{error,{try_clause,V}}} =
	rethrow_1({value,V}, exit),
    {value2,{caught1,V}} =
	rethrow_1({error,V}, error),
    {value2,{caught1,V}} =
	rethrow_1({exit,V}, exit),
    {value2,caught1} =
	rethrow_2({throw,V}, V),
    ok.

rethrow_1(X, C1) ->
    try try foo(X) of
            C1 -> value1
        catch
            C1:D1 -> {caught1,D1}
        end of
        V2 -> {value2,V2}
    catch
        C2:D2 -> {caught2,{C2,D2}}
    end.

rethrow_2(X, C1) ->
    try try foo(X) of
            C1 -> value1
        catch
            C1 -> caught1 % Implicit class throw:
        end of
        V2 -> {value2,V2}
    catch
        C2:D2 -> {caught2,{C2,D2}}
    end.



nested_of(Conf) when is_list(Conf) ->
    V = {[self()|make_ref()],1.4142136},
    {{value,{value1,{V,x2}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{throw,{V,x2}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     undefined,
     {V,x4},
     finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {'div',{1,0}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     undefined,
     undefined,
     finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {'div',{1,0}}, {'add',{0,b}}),
    %%
    {{caught,{error,{try_clause,{V,x1}}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_of_1({{value,{V,x1}},void,try_clause},
		    void, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{exit,{V,x3}}},
     undefined,
     {V,x4},
     finalized} =
	nested_of_1({{value,{V,x1}},void,try_clause},
		    void, {exit,{V,x3}}, {value,{V,x4}}),
    {{caught,{throw,{V,x4}}},
     undefined,
     undefined,
     finalized} =
	nested_of_1({{value,{V,x1}},void,try_clause},
		    void, {exit,{V,x3}}, {throw,{V,x4}}),
    %%
    {{value,{caught1,{V,x2}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     undefined,
     {V,x4},
     finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {'div',{17,0}}, {value,{V,x4}}),
    {{caught,{error,badarg}},
     undefined,
     undefined,
     finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {'div',{17,0}}, {'abs',V}),
    %%
    {{caught,{error,badarith}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_of_1({{'add',{2,c}},rethrow,void},
		    void, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarg}},
     undefined,
     {V,x4},
     finalized} =
	nested_of_1({{'add',{2,c}},rethrow,void},
		    void, {'abs',V}, {value,{V,x4}}),
    {{caught,{error,function_clause}},
     undefined,
     undefined,
     finalized} =
	nested_of_1({{'add',{2,c}},rethrow,void},
		    void, {'abs',V}, function_clause),
    ok.

nested_of_1({X1,C1,V1},
	    X2, X3, X4) ->
    erase(nested3),
    erase(nested4),
    erase(nested),
    Self = self(),
    Try =
	try
            try self()
            of
                Self ->
                    try
                        foo(X1)
	            of
	                V1 -> {value1,foo(X2)}
                    catch
                        C1:V1 -> {caught1,foo(X2)}
	            after
                        put(nested3, foo(X3))
                    end
            after
                put(nested4, foo(X4))
            end
        of
            V -> {value,V}
        catch
            C:D -> {caught,{C,D}}
        after
            put(nested, finalized)
	end,
    {Try,erase(nested3),erase(nested4),erase(nested)}.



nested_catch(Conf) when is_list(Conf) ->
    V = {[make_ref(),1.4142136,self()]},
    {{value,{value1,{V,x2}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		       {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{throw,{V,x2}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		       {throw,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     undefined,
     {V,x4},
     finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		       {throw,{V,x2}}, {'div',{1,0}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     undefined,
     undefined,
     finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		       {throw,{V,x2}}, {'div',{1,0}}, {'add',{0,b}}),
    %%
    {{caught,{error,{try_clause,{V,x1}}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_catch_1({{value,{V,x1}},void,try_clause},
		       void, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{exit,{V,x3}}},
     undefined,
     {V,x4},
     finalized} =
	nested_catch_1({{value,{V,x1}},void,try_clause},
		       void, {exit,{V,x3}}, {value,{V,x4}}),
    {{caught,{throw,{V,x4}}},
     undefined,
     undefined,
     finalized} =
	nested_catch_1({{value,{V,x1}},void,try_clause},
		       void, {exit,{V,x3}}, {throw,{V,x4}}),
    %%
    {{value,{caught1,{V,x2}}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		       {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		       {'add',{1,c}}, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarith}},
     undefined,
     {V,x4},
     finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		       {'add',{1,c}}, {'div',{17,0}}, {value,{V,x4}}),
    {{caught,{error,badarg}},
     undefined,
     undefined,
     finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		       {'add',{1,c}}, {'div',{17,0}}, {'abs',V}),
    %%
    {{caught,{error,badarith}},
     {V,x3},
     {V,x4},
     finalized} =
	nested_catch_1({{'add',{2,c}},rethrow,void},
		       void, {value,{V,x3}}, {value,{V,x4}}),
    {{caught,{error,badarg}},
     undefined,
     {V,x4},
     finalized} =
	nested_catch_1({{'add',{2,c}},rethrow,void},
		       void, {'abs',V}, {value,{V,x4}}),
    {{caught,{error,function_clause}},
     undefined,
     undefined,
     finalized} =
	nested_catch_1({{'add',{2,c}},rethrow,void},
		       void, {'abs',V}, function_clause),
    ok.

nested_catch_1({X1,C1,V1},
	       X2, X3, X4) ->
    erase(nested3),
    erase(nested4),
    erase(nested),
    Throw = make_ref(),
    Try =
	try
            try throw(Throw)
            catch
		Throw ->
                    try
                        foo(X1)
	            of
	                V1 -> {value1,foo(X2)}
                    catch
                        C1:V1 -> {caught1,foo(X2)}
	            after
                        put(nested3, foo(X3))
                    end
            after
                put(nested4, foo(X4))
            end
        of
            V -> {value,V}
        catch
            C:D -> {caught,{C,D}}
        after
            put(nested, finalized)
	end,
    {Try,erase(nested3),erase(nested4),erase(nested)}.

nested_after(Conf) when is_list(Conf) ->
    V = [{make_ref(),1.4142136,self()}],
    {value,
     {V,x3},
     {value1,{V,x2}},
     finalized} =
	nested_after_1({{value,{V,x1}},void,{V,x1}},
		       {value,{V,x2}}, {value,{V,x3}}),
    {{caught,{error,{V,x2}}},
     {V,x3},
     undefined,
     finalized} =
	nested_after_1({{value,{V,x1}},void,{V,x1}},
		       {error,{V,x2}}, {value,{V,x3}}),
    {{caught,{exit,{V,x3}}},
     undefined,
     undefined,
     finalized} =
	nested_after_1({{value,{V,x1}},void,{V,x1}},
		       {error,{V,x2}}, {exit,{V,x3}}),
    %%
    {{caught,{error,{try_clause,{V,x1}}}},
     {V,x3},
     undefined,
     finalized} =
	nested_after_1({{value,{V,x1}},void,try_clause},
		       void, {value,{V,x3}}),
    {{caught,{error,badarith}},
     undefined,
     undefined,
     finalized} =
	nested_after_1({{value,{V,x1}},void,try_clause},
		       void, {'div',{17,0}}),
    %%
    {value,
     {V,x3},
     {caught1,{V,x2}},
     finalized} =
	nested_after_1({{throw,{V,x1}},throw,{V,x1}},
		       {value,{V,x2}}, {value,{V,x3}}),
    {{caught,{error,badarith}},
     {V,x3},
     undefined,
     finalized} =
	nested_after_1({{throw,{V,x1}},throw,{V,x1}},
		       {'add',{a,b}}, {value,{V,x3}}),
    {{caught,{error,badarg}},
     undefined,
     undefined,
     finalized} =
	nested_after_1({{throw,{V,x1}},throw,{V,x1}},
		       {'add',{a,b}}, {'abs',V}),
    %%
    {{caught,{throw,{V,x1}}},
     {V,x3},
     undefined,
     finalized} =
	nested_after_1({{throw,{V,x1}},rethrow,void},
		       void, {value,{V,x3}}),
    {{caught,{error,badarith}},
     undefined,
     undefined,
     finalized} =
	nested_after_1({{throw,{V,x1}},rethrow,void},
		       void, {'div',{1,0}}),
    ok.

nested_after_1({X1,C1,V1},
	       X2, X3) ->
    erase(nested3),
    erase(nested4),
    erase(nested),
    Self = self(),
    Try =
	try
            try self()
            after
                After =
                    try
                        foo(X1)
	            of
	                V1 -> {value1,foo(X2)}
                    catch
                        C1:V1 -> {caught1,foo(X2)}
	            after
                        put(nested3, foo(X3))
                    end,
                put(nested4, After)
            end
        of
            Self -> value
        catch
            C:D -> {caught,{C,D}}
        after
            put(nested, finalized)
	end,
    {Try,erase(nested3),erase(nested4),erase(nested)}.

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
foo({raise,{Class,Reason}}) ->
    erlang:raise(Class, Reason).
%%foo(function_clause) -> % must not be defined!

my_div(A, B) ->
    A div B.

my_add(A, B) ->
    A + B.

my_abs(X) -> abs(X).

id(I) -> I.
