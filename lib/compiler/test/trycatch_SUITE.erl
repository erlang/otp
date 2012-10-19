%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
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
-module(trycatch_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,basic/1,lean_throw/1,
	 try_of/1,try_after/1,%after_bind/1,
	 catch_oops/1,after_oops/1,eclectic/1,rethrow/1,
	 nested_of/1,nested_catch/1,nested_after/1,
	 nested_horrid/1,last_call_optimization/1,bool/1,
	 plain_catch_coverage/1,andalso_orelse/1,get_in_try/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [basic,lean_throw,try_of,try_after,catch_oops,
       after_oops,eclectic,rethrow,nested_of,nested_catch,
       nested_after,nested_horrid,last_call_optimization,
       bool,plain_catch_coverage,andalso_orelse,get_in_try]}].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



basic(Conf) when is_list(Conf) ->
    ?line 2 = 
	try my_div(4, 2)
	catch 
            Class:Reason -> {Class,Reason}
	end,
    ?line error = 
        try my_div(1, 0)
        catch 
            error:badarith -> error
        end,
    ?line error = 
        try 1.0 / zero()
        catch 
            error:badarith -> error
        end,
    ?line ok = 
        try my_add(53, atom)
        catch
            error:badarith -> ok
        end,
    ?line exit_nisse = 
        try exit(nisse)
	catch 
            exit:nisse -> exit_nisse
        end,
    ?line ok = 
        try throw(kalle)
        catch
            kalle -> ok
        end,

    %% Try some stuff where the compiler will optimize away the try.

    V = id({a,variable}),
    ?line V = try V catch nisse -> error end,
    ?line 42 = try 42 catch nisse -> error end,
    ?line [V] = try [V] catch nisse -> error end,
    ?line {ok,V} = try {ok,V} catch nisse -> error end,

    %% Same idea, but use an after too.

    ?line V = try V catch nisse -> error after after_call() end,
    ?line after_clean(),
    ?line 42 = try 42 after after_call() end,
    ?line after_clean(),
    ?line [V] = try [V] catch nisse -> error after after_call() end,
    ?line after_clean(),
    ?line {ok,V} = try {ok,V} after after_call() end,

    %% Try/of
    ?line ok = try V of
              {a,variable} -> ok
              catch nisse -> erro
          end,
              
    ok.

after_call() ->
    put(basic, after_was_called).

after_clean() ->
    after_was_called = erase(basic).
    

lean_throw(Conf) when is_list(Conf) ->
    ?line {throw,kalle} =
        try throw(kalle)
        catch
            Kalle -> {throw,Kalle}
        end,
    ?line {exit,kalle} =
        try exit(kalle)
        catch
            Throw1 -> {throw,Throw1};
	    exit:Reason1 -> {exit,Reason1}
        end,
    ?line {exit,kalle} =
        try exit(kalle)
        catch
	    exit:Reason2 -> {exit,Reason2};
            Throw2 -> {throw,Throw2}
        end,
    ?line {exit,kalle} =
        try try exit(kalle)
            catch
                Throw3 -> {throw,Throw3}
            end
        catch
            exit:Reason3 -> {exit,Reason3}
        end,
    ok.



try_of(Conf) when is_list(Conf) ->
    ?line {ok,{some,content}} = 
	try_of_1({value,{good,{some,content}}}),
    ?line {error,[other,content]} = 
	try_of_1({value,{bad,[other,content]}}),
    ?line {caught,{exit,{ex,it,[reason]}}} = 
	try_of_1({exit,{ex,it,[reason]}}),
    ?line {caught,{throw,[term,{in,a,{tuple}}]}} = 
	try_of_1({throw,[term,{in,a,{tuple}}]}),
    ?line {caught,{error,[bad,arg]}} = 
	try_of_1({error,[bad,arg]}),
    ?line {caught,{error,badarith}} = 
	try_of_1({'div',{1,0}}),
    ?line {caught,{error,badarith}} = 
	try_of_1({'add',{a,0}}),
    ?line {caught,{error,badarg}} = 
	try_of_1({'abs',x}),
    ?line {caught,{error,function_clause}} = 
	try_of_1(illegal),
    ?line {error,{try_clause,{some,other_garbage}}} =
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
    ?line {{ok,[some,value],undefined},finalized} = 
	try_after_1({value,{ok,[some,value]}},finalized),
    ?line {{error,badarith,undefined},finalized} = 
	try_after_1({'div',{1,0}},finalized),
    ?line {{error,badarith,undefined},finalized} = 
	try_after_1({'add',{1,a}},finalized),
    ?line {{error,badarg,undefined},finalized} = 
	try_after_1({'abs',a},finalized),
    ?line {{error,[the,{reason}],undefined},finalized} = 
	try_after_1({error,[the,{reason}]},finalized),
    ?line {{throw,{thrown,[reason]},undefined},finalized} = 
	try_after_1({throw,{thrown,[reason]}},finalized),
    ?line {{exit,{exited,{reason}},undefined},finalized} = 
	try_after_1({exit,{exited,{reason}}},finalized),
    ?line {{error,function_clause,undefined},finalized} = 
	try_after_1(function_clause,finalized),
    ?line ok =
	try try_after_1({'add',{1,1}}, finalized)
        catch
            error:{try_clause,2} -> ok
	end,
    ?line finalized = erase(try_after),
    ?line ok =
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
    ?line {value,{value,V}} =
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
    ?line {value,V} = catch_oops_1({value,V}),
    ?line {value,1} = catch_oops_1({'div',{1,1}}),
    ?line {error,badarith} = catch_oops_1({'div',{1,0}}),
    ?line {error,function_clause} = catch_oops_1(function_clause),
    ?line {throw,V} = catch_oops_1({throw,V}),
    ?line {exit,V} = catch_oops_1({exit,V}),
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
    ?line {{value,V},V} = after_oops_1({value,V}, {value,V}),
    ?line {{exit,V},V} = after_oops_1({exit,V}, {value,V}),
    ?line {{error,V},undefined} = after_oops_1({value,V}, {error,V}),
    ?line {{error,function_clause},undefined} = 
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
    ?line {{value,{value,V},V},V} = 
	eclectic_1({foo,{value,{value,V}}}, undefined, {value,V}),
    ?line {{'EXIT',{V,[{?MODULE,foo,1,_}|_]}},V} =
	eclectic_1({catch_foo,{error,V}}, undefined, {value,V}),
    ?line {{error,{exit,V},{'EXIT',V}},V} =
	eclectic_1({foo,{error,{exit,V}}}, error, {value,V}),
    ?line {{value,{value,V},V},
	   {'EXIT',{badarith,[{?MODULE,my_add,2,_}|_]}}} =
	eclectic_1({foo,{value,{value,V}}}, undefined, {'add',{0,a}}),
    ?line {{'EXIT',V},V} =
	eclectic_1({catch_foo,{exit,V}}, undefined, {throw,V}),
    ?line {{error,{'div',{1,0}},{'EXIT',{badarith,[{?MODULE,my_div,2,_}|_]}}},
	   {'EXIT',V}} =
	eclectic_1({foo,{error,{'div',{1,0}}}}, error, {exit,V}),
    ?line {{{error,V},{'EXIT',{V,[{?MODULE,foo,1,_}|_]}}},
	   {'EXIT',V}} =
	eclectic_1({catch_foo,{throw,{error,V}}}, undefined, {exit,V}),
    %%
    ?line {{value,{value,{value,V},V}},V} =
	eclectic_2({value,{value,V}}, undefined, {value,V}),
    ?line {{value,{throw,{value,V},V}},V} =
	eclectic_2({throw,{value,V}}, throw, {value,V}),
    ?line {{caught,{'EXIT',V}},undefined} =
	eclectic_2({value,{value,V}}, undefined, {exit,V}),
    ?line {{caught,{'EXIT',{V,[{?MODULE,foo,1,_}|_]}}},undefined} =
	eclectic_2({error,{value,V}}, throw, {error,V}),
    ?line {{caught,{'EXIT',{badarg,[{erlang,abs,[V],_}|_]}}},V} =
	eclectic_2({value,{'abs',V}}, undefined, {value,V}),
    ?line {{caught,{'EXIT',{badarith,[{?MODULE,my_add,2,_}|_]}}},V} =
	eclectic_2({exit,{'add',{0,a}}}, exit, {value,V}),
    ?line {{caught,{'EXIT',V}},undefined} =
	eclectic_2({value,{error,V}}, undefined, {exit,V}),
    ?line {{caught,{'EXIT',{V,[{?MODULE,foo,1,_}|_]}}},undefined} =
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
    ?line {value2,value1} = 
	rethrow_1({value,V}, V),
    ?line {caught2,{error,V}} = 
	rethrow_2({error,V}, undefined),
    ?line {caught2,{exit,V}} = 
	rethrow_1({exit,V}, error),
    ?line {caught2,{throw,V}} = 
	rethrow_1({throw,V}, undefined),
    ?line {caught2,{throw,V}} = 
	rethrow_2({throw,V}, undefined),
    ?line {caught2,{error,badarith}} = 
	rethrow_1({'add',{0,a}}, throw),
    ?line {caught2,{error,function_clause}} = 
	rethrow_2(function_clause, undefined),
    ?line {caught2,{error,{try_clause,V}}} = 
	rethrow_1({value,V}, exit),
    ?line {value2,{caught1,V}} = 
	rethrow_1({error,V}, error),
    ?line {value2,{caught1,V}} = 
	rethrow_1({exit,V}, exit),
    ?line {value2,caught1} = 
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
    ?line {{value,{value1,{V,x2}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{throw,{V,x2}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {'div',{1,0}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   undefined,
	   finalized} =
	nested_of_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {'div',{1,0}}, {'add',{0,b}}),
    %%
    ?line {{caught,{error,{try_clause,{V,x1}}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_of_1({{value,{V,x1}},void,try_clause},
		    void, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{exit,{V,x3}}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_of_1({{value,{V,x1}},void,try_clause},
		    void, {exit,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{throw,{V,x4}}},
	   undefined,
	   undefined,
	   finalized} =
	nested_of_1({{value,{V,x1}},void,try_clause},
		    void, {exit,{V,x3}}, {throw,{V,x4}}),
    %%
    ?line {{value,{caught1,{V,x2}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {'div',{17,0}}, {value,{V,x4}}),
    ?line {{caught,{error,badarg}},
	   undefined,
	   undefined,
	   finalized} =
	nested_of_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {'div',{17,0}}, {'abs',V}),
    %%
    ?line {{caught,{error,badarith}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_of_1({{'add',{2,c}},rethrow,void},
		    void, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarg}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_of_1({{'add',{2,c}},rethrow,void},
		    void, {'abs',V}, {value,{V,x4}}),
    ?line {{caught,{error,function_clause}},
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
    ?line {{value,{value1,{V,x2}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		    {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{throw,{V,x2}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {'div',{1,0}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   undefined,
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,{V,x1}},
		    {throw,{V,x2}}, {'div',{1,0}}, {'add',{0,b}}),
    %%
    ?line {{caught,{error,{try_clause,{V,x1}}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,try_clause},
		    void, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{exit,{V,x3}}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,try_clause},
		    void, {exit,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{throw,{V,x4}}},
	   undefined,
	   undefined,
	   finalized} =
	nested_catch_1({{value,{V,x1}},void,try_clause},
		    void, {exit,{V,x3}}, {throw,{V,x4}}),
    %%
    ?line {{value,{caught1,{V,x2}}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		    {value,{V,x2}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {'div',{17,0}}, {value,{V,x4}}),
    ?line {{caught,{error,badarg}},
	   undefined,
	   undefined,
	   finalized} =
	nested_catch_1({{error,{V,x1}},error,{V,x1}},
		    {'add',{1,c}}, {'div',{17,0}}, {'abs',V}),
    %%
    ?line {{caught,{error,badarith}},
	   {V,x3},
	   {V,x4},
	   finalized} =
	nested_catch_1({{'add',{2,c}},rethrow,void},
		    void, {value,{V,x3}}, {value,{V,x4}}),
    ?line {{caught,{error,badarg}},
	   undefined,
	   {V,x4},
	   finalized} =
	nested_catch_1({{'add',{2,c}},rethrow,void},
		    void, {'abs',V}, {value,{V,x4}}),
    ?line {{caught,{error,function_clause}},
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
    ?line {value,
	   {V,x3},
	   {value1,{V,x2}},
	   finalized} =
	nested_after_1({{value,{V,x1}},void,{V,x1}},
		       {value,{V,x2}}, {value,{V,x3}}),
    ?line {{caught,{error,{V,x2}}},
	   {V,x3},
	   undefined,
	   finalized} =
	nested_after_1({{value,{V,x1}},void,{V,x1}},
		       {error,{V,x2}}, {value,{V,x3}}),
    ?line {{caught,{exit,{V,x3}}},
	   undefined,
	   undefined,
	   finalized} =
	nested_after_1({{value,{V,x1}},void,{V,x1}},
		       {error,{V,x2}}, {exit,{V,x3}}),
    %%
    ?line {{caught,{error,{try_clause,{V,x1}}}},
	   {V,x3},
	   undefined,
	   finalized} =
	nested_after_1({{value,{V,x1}},void,try_clause},
		       void, {value,{V,x3}}),
    ?line {{caught,{error,badarith}},
	   undefined,
	   undefined,
	   finalized} =
	nested_after_1({{value,{V,x1}},void,try_clause},
		       void, {'div',{17,0}}),
    %%
    ?line {value,
	   {V,x3},
	   {caught1,{V,x2}},
	   finalized} =
	nested_after_1({{throw,{V,x1}},throw,{V,x1}},
		       {value,{V,x2}}, {value,{V,x3}}),
    ?line {{caught,{error,badarith}},
	   {V,x3},
	   undefined,
	   finalized} =
	nested_after_1({{throw,{V,x1}},throw,{V,x1}},
		       {'add',{a,b}}, {value,{V,x3}}),
    ?line {{caught,{error,badarg}},
	   undefined,
	   undefined,
	   finalized} =
	nested_after_1({{throw,{V,x1}},throw,{V,x1}},
		       {'add',{a,b}}, {'abs',V}),
    %%
    ?line {{caught,{throw,{V,x1}}},
	   {V,x3},
	   undefined,
	   finalized} =
	nested_after_1({{throw,{V,x1}},rethrow,void},
		       void, {value,{V,x3}}),
    ?line {{caught,{error,badarith}},
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



nested_horrid(Config) when is_list(Config) ->
    _V = {make_ref(),nested_horrid,4.711},
    {[true,true],{[true,1.0],1.0}} =
	nested_horrid_1({true,void,void}, 1.0),
    ok.

nested_horrid_1({X1,C1,V1}, X2) ->
    try A1 = [X1,X1],
	B1 = if X1 ->
		     A2 = [X1,X2],
		     B2 = foo(X2),
		     {A2,B2};
		true ->
		     A3 = [X2,X1],
		     B3 = foo(X2),
		     {A3,B3}
	     end,
	{A1,B1}
    catch
	C1:V1 -> caught1
    end.



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
    erlang:raise(Class, Reason);
foo(Term) when not is_atom(Term) -> Term.
%%foo(Atom) when is_atom(Atom) -> % must not be defined!

my_div(A, B) ->
    A div B.

my_add(A, B) ->
    A + B.

my_abs(X) -> abs(X).


last_call_optimization(Config) when is_list(Config) ->
    ?line error = in_tail(dum),
    ?line StkSize0 = in_tail(0),
    ?line StkSize = in_tail(50000),
    io:format("StkSize0 = ~p", [StkSize0]),
    io:format("StkSize  = ~p", [StkSize]),
    ?line StkSize = StkSize0,
    ok.

in_tail(E) ->
    try erlang:abs(E) of
        T ->
	    A = id([]),
	    B = id([]),
	    C = id([]),
	    id([A,B,C]),
	    do_tail(T)
    catch error:badarg -> error
    end.

do_tail(0) ->
    process_info(self(), stack_size);
do_tail(N) ->
    in_tail(N-1).

bool(Config) when is_list(Config) ->
    ok = do_bool(false, false),
    error = do_bool(false, true),
    error = do_bool(true, false),
    error = do_bool(true, true),
    error = do_bool(true, blurf),
    {'EXIT',_} = (catch do_bool(blurf, false)),
    ok.

%% The following function used to cause a crash in beam_bool.
do_bool(A0, B) ->
    A = not A0,
    try
	id(42),
	if
	    A, not B -> ok
	end
    catch
	_:_ ->
	    error
    end.

plain_catch_coverage(Config) when is_list(Config) ->
    %% Cover some code in beam_block:alloc_may_pass/1.
    ?line {a,[42]} = do_plain_catch_list(42).

do_plain_catch_list(X) ->
    B = [X],
    catch id({a,B}).

andalso_orelse(Config) when is_list(Config) ->
    ?line {2,{a,42}} = andalso_orelse_1(true, {a,42}),
    ?line {b,{b}} = andalso_orelse_1(false, {b}),
    ?line {catched,no_tuple} = andalso_orelse_1(false, no_tuple),

    ?line ok = andalso_orelse_2({type,[a]}),
    ?line also_ok = andalso_orelse_2({type,[]}),
    ?line also_ok = andalso_orelse_2({type,{a}}),
    ok.

andalso_orelse_1(A, B) ->
    {try
	 if
	     A andalso element(1, B) =:= a ->
		 tuple_size(B);
	     true ->
		 element(1, B)
	 end
     catch error:_ ->
	     catched
     end,B}.

id(I) -> I.

andalso_orelse_2({Type,Keyval}) ->
   try
       if is_atom(Type) andalso length(Keyval) > 0 -> ok;
          true -> also_ok
       end
   catch
       _:_ -> fail
   end.

zero() ->
    0.0.

get_in_try(_) ->
    undefined = get_valid_line([a], []),
    ok.

get_valid_line([_|T]=Path, Annotations) ->
    try
        get(Path)
	%% beam_dead used to optimize away an assignment to {y,1}
	%% because it didn't appear to be used.
    catch
        _:not_found ->
            get_valid_line(T, Annotations)
    end.
