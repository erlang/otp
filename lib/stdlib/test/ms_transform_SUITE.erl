%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2011. All Rights Reserved.
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
-module(ms_transform_SUITE).
-author('pan@erix.ericsson.se').

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([basic_ets/1]).
-export([basic_dbg/1]).
-export([from_shell/1]).
-export([records/1]).
-export([record_index/1]).
-export([multipass/1]).
-export([top_match/1]).
-export([old_guards/1]).
-export([autoimported/1]).
-export([semicolon/1]).
-export([bitsyntax/1]).
-export([record_defaults/1]).
-export([andalso_orelse/1]).
-export([float_1_function/1]).
-export([action_function/1]).
-export([warnings/1]).
-export([no_warnings/1]).
-export([eep37/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

init_per_testcase(_Func, Config) ->
    Dog=test_server:timetrap(test_server:seconds(360)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [from_shell, basic_ets, basic_dbg, records,
     record_index, multipass, bitsyntax, record_defaults,
     andalso_orelse, float_1_function, action_function,
     warnings, no_warnings, top_match, old_guards, autoimported,
     semicolon, eep37].

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


%% This may be subject to change
-define(WARN_NUMBER_SHADOW,50).
warnings(suite) ->
    [];
warnings(doc) ->
    ["Check that shadowed variables in fun head generate warning"];
warnings(Config) when is_list(Config) ->
    ?line setup(Config),
    Prog = <<"A=5, "
	    "ets:fun2ms(fun({A,B}) "
	    "            when is_integer(A) and (A+5 > B) -> "
	    "              A andalso B "
	    "            end)">>,
    ?line [{_,[{_,ms_transform,{?WARN_NUMBER_SHADOW,'A'}}]}] =
	compile_ww(Prog),
    Prog2 = <<"C = 5,
               ets:fun2ms(fun ({A,B} =
                                       C) when is_integer(A) and (A+5 > B) ->
                                  {A andalso B,C}
                          end)">>,
    [{_,[{3,ms_transform,{?WARN_NUMBER_SHADOW,'C'}}]}] =
	compile_ww(Prog2),
    Rec3 = <<"-record(a,{a,b,c,d=foppa}).">>,
    Prog3 = <<"A = 3,
               C = 5,
               ets:fun2ms(fun (C
                                 = #a{a = A, b = B})
                              when is_integer(A) and (A+5 > B) ->
                                  {A andalso B,C}
                          end)">>,
    [{_,[{3,ms_transform,{?WARN_NUMBER_SHADOW,'C'}},
         {4,ms_transform,{?WARN_NUMBER_SHADOW,'A'}}]}] =
	compile_ww(Rec3,Prog3),
    Rec4 = <<"-record(a,{a,b,c,d=foppa}).">>,
    Prog4 = <<"A=3,C=5, "
	     "F = fun(B) -> B*3 end,"
	     "erlang:display(F(A)),"
	     "ets:fun2ms(fun(#a{a = A, b = B} = C) "
	     "            when is_integer(A) and (A+5 > B) -> "
	     "              {A andalso B,C} "
	     "            end)">>,
    ?line [{_,[{_,ms_transform,{?WARN_NUMBER_SHADOW,'A'}},
	       {_,ms_transform,{?WARN_NUMBER_SHADOW,'C'}}]}] =
	compile_ww(Rec4,Prog4),
    Rec5 = <<"-record(a,{a,b,c,d=foppa}).">>,
    Prog5 = <<"A=3,C=5, "
	     "F = fun(B) -> B*3 end,"
	     "erlang:display(F(A)),"
	     "B = ets:fun2ms(fun(#a{a = A, b = B} = C) "
	     "            when is_integer(A) and (A+5 > B) -> "
	     "              {A andalso B,C} "
	     "            end)">>,
    ?line [{_,[{_,ms_transform,{?WARN_NUMBER_SHADOW,'A'}},
	       {_,ms_transform,{?WARN_NUMBER_SHADOW,'C'}}]}] =
	compile_ww(Rec5,Prog5),
    Prog6 = <<"   X=bar, "
	     "    A = case X of"
	     "       foo ->"
	     "          foo;"
	     "       Y ->"
	     "          ets:fun2ms(fun(Y) ->" % This is a warning
	     "                         3*Y"
	     "                     end)"
	     "   end,"
	     "   ets:fun2ms(fun(Y) ->" % Y out of "scope" here, so no warning
	     "                  {3*Y,A}"
	     "              end)">>,
    ?line [{_,[{_,ms_transform,{?WARN_NUMBER_SHADOW,'Y'}}]}] =
	compile_ww(Prog6),
    Prog7 = <<"   X=bar, "
	     "    A = case X of"
	     "       foo ->"
	     "          Y = foo;"
	     "       Y ->"
	     "          bar"
	     "   end,"
	     "   ets:fun2ms(fun(Y) ->" % Y exported from case and safe, so warn
	     "                  {3*Y,A}"
	     "              end)">>,
    ?line [{_,[{_,ms_transform,{?WARN_NUMBER_SHADOW,'Y'}}]}] =
	compile_ww(Prog7),
    ok.

no_warnings(suite) ->
    [];
no_warnings(doc) ->
    ["Check that variables bound in other function clauses don't generate "
     "warning"];
no_warnings(Config) when is_list(Config) ->
    ?line setup(Config),
    Prog = <<"tmp(X) when X > 100 ->\n",
	     "   Y=X,\n"
	     "   Y;\n"
	     "tmp(X) ->\n"
	     "   ets:fun2ms(fun(Y) ->\n"
	     "                  {X, 3*Y}\n"
	     "              end)">>,
    ?line [] = compile_no_ww(Prog),

    Prog2 = <<"tmp(X) when X > 100 ->\n",
	     "   Y=X,\n"
	     "   Y;\n"
	     "tmp(X) when X < 200 ->\n"
	     "   ok;\n"
	     "tmp(X) ->\n"
	     "   ets:fun2ms(fun(Y) ->\n"
	     "                  {X, 3*Y}\n"
	     "              end)">>,
    ?line [] = compile_no_ww(Prog2),
    ok.

andalso_orelse(suite) ->
    [];
andalso_orelse(doc) ->
    ["Tests that andalso and orelse are allowed in guards."];
andalso_orelse(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line [{{'$1','$2'},
	    [{'and',{is_integer,'$1'},{'>',{'+','$1',5},'$2'}}],
	    [{'andalso','$1','$2'}]}] = 
	compile_and_run(<<"ets:fun2ms(fun({A,B}) "
			 "            when is_integer(A) and (A+5 > B) -> "
			 "              A andalso B "
			 "            end)">>),
    ?line [{{'$1','$2'},
	    [{'or',{is_atom,'$1'},{'>',{'+','$1',5},'$2'}}],
	    [{'orelse','$1','$2'}]}] = 
	compile_and_run(<<"ets:fun2ms(fun({A,B}) "
			 "            when is_atom(A) or (A+5 > B) -> "
			 "              A orelse B "
			 "            end)">>),
    ?line [{{'$1','$2'},
            [{'andalso',{is_integer,'$1'},{'>',{'+','$1',5},'$2'}}],
            ['$1']}] = 
        compile_and_run(
	    <<"ets:fun2ms(fun({A,B}) when is_integer(A) andalso (A+5 > B) ->"
	     "			 A "
	     "		 end)">>),
    ?line [{{'$1','$2'},
            [{'orelse',{is_atom,'$1'},{'>',{'+','$1',5},'$2'}}],
            ['$1']}] =
        compile_and_run(
	    <<"ets:fun2ms(fun({A,B}) when is_atom(A) orelse (A+5 > B) -> "
	     "			 A "
	     "		 end)">>),
    ok.
    
    
bitsyntax(suite) ->
    [];
bitsyntax(doc) ->
    ["Tests that bitsyntax works and does not work where appropriate"];
bitsyntax(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line [{'_',[],
	    [<<0,27,0,27>>]}] =
	compile_and_run(<<"A = 27, "
			  "ets:fun2ms(fun(_) -> <<A:16,27:16>> end)">>),
    ?line  [{{<<15,47>>,
	      '$1',
	      '$2'},
	     [{'=:=','$1',
	       <<0,27>>},
	      {'=:=','$2',
	       <<27,28,19>>}],
	     [<<188,0,13>>]}] =
	compile_and_run(<<"A = 27, "
			  "ets:fun2ms("
                          "  fun({<<15,47>>,B,C}) "
			  "  when B =:= <<A:16>>, C =:= <<27,28,19>> -> "
			  "    <<A:4,12:4,13:16>> "
			  "  end)">>),
    ?line expect_failure(
	    <<>>,
	    <<"ets:fun2ms(fun({<<15,47>>,B,C}) "
	     "            when B =:= <<16>>, C =:= <<27,28,19>> -> "
	     "              <<B:4,12:4,13:16>> "
	     "            end)">>),
    ?line expect_failure(
	    <<>>,
	    <<"ets:fun2ms(fun({<<A:15,47>>,B,C}) "
	     "            when B =:= <<16>>, C =:= <<27,28,19>> -> "
	     "              <<B:4,12:4,13:16>> "
	     "            end)">>),
    ok.

record_defaults(suite) ->
    [];
record_defaults(doc) ->
    ["Tests that record defaults works"];
record_defaults(Config) when is_list(Config) ->
    ?line setup(Config),    
    ?line [{{<<27>>,{a,5,'$1',hej,hej}},
	    [],
	    [{{a,hej,{'*','$1',2},flurp,flurp}}]}] = 
	compile_and_run(<<"-record(a,{a,b,c,d=foppa}).">>,
			<<"ets:fun2ms(fun({<<27>>,#a{a=5, b=B,_=hej}}) -> "
		                        "#a{a=hej,b=B*2,_=flurp} "
	                              "end)">>),
    ok.

basic_ets(suite) ->
    [];
basic_ets(doc) ->
    ["Tests basic ets:fun2ms"];
basic_ets(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line [{{a,b},[],[true]}] = compile_and_run(
				  <<"ets:fun2ms(fun({a,b}) -> true end)">>),
    ?line [{{'$1',foo},[{is_list,'$1'}],[{{{hd,'$1'},'$_'}}]},
     {{'$1','$1'},[{is_tuple,'$1'}],[{{{element,1,'$1'},'$*'}}]}] =
	compile_and_run(<<"ets:fun2ms(fun({X,foo}) when is_list(X) -> ",
			                     "{hd(X),object()};",
			                "({X,X}) when is_tuple(X) ->",
			                     "{element(1,X),bindings()}",
			             "end)">>),
    ?line [{{'$1','$2'},[],[{{'$2','$1'}}]}] =
	compile_and_run(<<"ets:fun2ms(fun({A,B}) -> {B,A} end)">>),
    ?line [{{'$1','$2'},[],[['$2','$1']]}] =
	compile_and_run(<<"ets:fun2ms(fun({A,B}) -> [B,A] end)">>),
    ok.

basic_dbg(suite) ->
    [];
basic_dbg(doc) ->
    ["Tests basic ets:fun2ms"];
basic_dbg(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line [{[a,b],[],[{message,banan},{return_trace}]}] =
	compile_and_run(<<"dbg:fun2ms(fun([a,b]) -> message(banan), ",
			"return_trace() end)">>),
    ?line [{['$1','$2'],[],[{{'$2','$1'}}]}] = 
	compile_and_run(<<"dbg:fun2ms(fun([A,B]) -> {B,A} end)">>),
    ?line [{['$1','$2'],[],[['$2','$1']]}] =
	compile_and_run(<<"dbg:fun2ms(fun([A,B]) -> [B,A] end)">>),
    ?line [{['$1','$2'],[],['$*']}] =
	compile_and_run(<<"dbg:fun2ms(fun([A,B]) -> bindings() end)">>),
    ?line [{['$1','$2'],[],['$_']}] =
	compile_and_run(<<"dbg:fun2ms(fun([A,B]) -> object() end)">>),
    ok.

from_shell(suite) ->
    [];
from_shell(doc) ->
    ["Test calling of ets/dbg:fun2ms from the shell"]; 
from_shell(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line Fun = do_eval("fun({a,b}) -> true end"),
    ?line [{{a,b},[],[true]}] = apply(ets,fun2ms,[Fun]),
    ?line [{{a,b},[],[true]}] = do_eval("ets:fun2ms(fun({a,b}) -> true end)"),
    ?line Fun2 = do_eval("fun([a,b]) -> message(banan), return_trace() end"),
    ?line [{[a,b],[],[{message,banan},{return_trace}]}]
	= apply(dbg,fun2ms,[Fun2]),
    ?line [{[a,b],[],[{message,banan},{return_trace}]}] =
	do_eval(
	  "dbg:fun2ms(fun([a,b]) -> message(banan), return_trace() end)"),
    ok.

records(suite) ->
    [];
records(doc) ->
    ["Tests expansion of records in fun2ms"];
records(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line RD = <<"-record(t, {"
	             "t1 = [],"
	             "t2 = foo,"
	             "t3,"
	             "t4"
	            "}).">>,
    ?line [{{t,'$1','$2',foo,'_'},[{is_list,'$1'}],[{{{hd,'$1'},'$_'}}]},
     {{t,'_','_','_','_'},[{'==',{element,2,'$_'},nisse}],[{{'$*'}}]}] =
	compile_and_run(RD,<<
   "ets:fun2ms(fun(#t{t1 = X, t2 = Y, t3 = foo}) when is_list(X) -> 
 		       {hd(X),object()}; 
 		  (#t{}) when (object())#t.t1 == nisse -> 
 		       {bindings()}  
 	       end)">>),
    ?line [{{t,'$1','$2','_',foo},
      [{'==',{element,4,'$_'},7},{is_list,'$1'}],
      [{{{hd,'$1'},'$_'}}]},
     {'$1',[{is_record,'$1',t,5}],
      [{{{element,2,'$1'},
	 {{t,'$1',foo,undefined,undefined}},
	 {{t,{element,2,'$1'},{element,3,'$1'},{element,4,'$1'},boooo}}}}]}] =
	compile_and_run(RD,<<
    "ets:fun2ms(fun(#t{t1 = X, t2 = Y, t4 = foo}) when 
			 (object())#t.t3==7,is_list(X) -> 
 		       {hd(X),object()}; 
 		  (A) when is_record(A,t) -> 
 		       {A#t.t1
			,#t{t1=A}
			,A#t{t4=boooo}
		       }  
 	       end)"
			>>),
    ?line [{[{t,'$1','$2',foo,'_'}],[{is_list,'$1'}],[{{{hd,'$1'},'$_'}}]},
     {[{t,'_','_','_','_'}],[{'==',{element,2,{hd,'$_'}},nisse}],[{{'$*'}}]}]=
	compile_and_run(RD,<<
    "dbg:fun2ms(fun([#t{t1 = X, t2 = Y, t3 = foo}]) when is_list(X) -> 
 		       {hd(X),object()}; 
 		  ([#t{}]) when (hd(object()))#t.t1 == nisse -> 
 		       {bindings()}  
 	       end)"
			>>),
    ok.


record_index(suite) ->
    [];
record_index(doc) ->
    ["Tests expansion of records in fun2ms, part 2"];
record_index(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line RD = <<"-record(a,{a,b}).">>,
    ?line [{{2},[],[true]}] = compile_and_run(RD,
			  <<"ets:fun2ms(fun({#a.a}) -> true end)">>),
    ?line [{{2},[],[2]}] = compile_and_run(RD,
			  <<"ets:fun2ms(fun({#a.a}) -> #a.a end)">>),
    ?line [{{2,'$1'},[{'>','$1',2}],[2]}] = compile_and_run(RD,
		    <<"ets:fun2ms(fun({#a.a,A}) when A > #a.a -> #a.a end)">>),
    ok.

top_match(suite) ->
    [];
top_match(doc) ->
    ["Tests matching on top level in head to give alias for object()"];
top_match(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line RD = <<"-record(a,{a,b}).">>,
    ?line [{{a,3,'_'},[],['$_']}] = 
	compile_and_run(RD,
			<<"ets:fun2ms(fun(A = #a{a=3}) -> A end)">>),
    ?line [{{a,3,'_'},[],['$_']}] = 
	compile_and_run(RD,
			<<"ets:fun2ms(fun(#a{a=3} = A) -> A end)">>),
    ?line [{[a,b],[],['$_']}] = 
	compile_and_run(RD,
			<<"dbg:fun2ms(fun(A = [a,b]) -> A end)">>),
    ?line [{[a,b],[],['$_']}] = 
	compile_and_run(RD,
			<<"dbg:fun2ms(fun([a,b] = A) -> A end)">>),
    ?line expect_failure(RD,
			 <<"ets:fun2ms(fun({a,A = {_,b}}) -> A end)">>),
    ?line expect_failure(RD,
			 <<"dbg:fun2ms(fun([a,A = {_,b}]) -> A end)">>),
    ?line expect_failure(RD,
			 <<"ets:fun2ms(fun(A#a{a = 2}) -> A end)">>),
    ok.

multipass(suite) ->
    [];
multipass(doc) ->
    ["Tests that multi-defined fields in records give errors."];
multipass(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line RD = <<"-record(a,{a,b}).">>,
    ?line expect_failure(RD,<<"ets:fun2ms(fun(A) -> #a{a=2,a=3} end)">>), 
    ?line expect_failure(RD,<<"ets:fun2ms(fun(A) -> A#a{a=2,a=3} end)">>),
    ?line expect_failure(RD,<<"ets:fun2ms(fun(A) when A =:= #a{a=2,a=3} ->",
			 " true end)">>), 
    ?line expect_failure(RD,<<"ets:fun2ms(fun({A,B})when A =:= B#a{a=2,a=3}->",
			 "true end)">>),
    ?line expect_failure(RD,<<"ets:fun2ms(fun(#a{a=3,a=3}) -> true end)">>),
    ?line compile_and_run(RD,<<"ets:fun2ms(fun(A) -> #a{a=2,b=3} end)">>), 
    ?line compile_and_run(RD,<<"ets:fun2ms(fun(A) -> A#a{a=2,b=3} end)">>),
    ?line compile_and_run(RD,<<"ets:fun2ms(fun(A) when A =:= #a{a=2,b=3} ->",
			 " true end)">>), 
    ?line compile_and_run(RD,<<"ets:fun2ms(fun({A,B})when A=:= B#a{a=2,b=3}->",
			 "true end)">>),
    ?line compile_and_run(RD,<<"ets:fun2ms(fun(#a{a=3,b=3}) -> true end)">>),
    ok.


old_guards(suite) ->
    [];
old_guards(doc) ->
    ["Tests that old type tests in guards are translated"];
old_guards(Config) when is_list(Config) ->
    ?line setup(Config),
    Tests = [
	     {atom,is_atom},
	     {float,is_float},
	     {integer,is_integer},
	     {list,is_list},
	     {number,is_number},
	     {pid,is_pid},
	     {port,is_port},
	     {reference,is_reference},
	     {tuple,is_tuple},
	     {binary,is_binary},
	     {function,is_function}],
    ?line lists:foreach(
	    fun({Old,New}) ->
		    Bin = list_to_binary([<<"ets:fun2ms(fun(X) when ">>,
					  atom_to_list(Old),
					  <<"(X)  -> true end)">>]),
		    case compile_and_run(Bin) of
			[{'$1',[{New,'$1'}],[true]}] -> 
			    ok;
			_ ->
			    exit({bad_result_for, binary_to_list(Bin)})
		    end
	    end,
	    Tests),
    ?line RD = <<"-record(a,{a,b}).">>,
    ?line [{'$1',[{is_record,'$1',a,3}],[true]}] =
	compile_and_run(RD,
			<<"ets:fun2ms(fun(X) when record(X,a) -> true end)">>),
    ?line expect_failure
	    (RD,
	     <<"ets:fun2ms(fun(X) when integer(X) and constant(X) -> "
	      "true end)">>),
    ?line [{'$1',[{is_integer,'$1'},
		  {is_float,'$1'},
		  {is_atom,'$1'},
		  {is_list,'$1'},
		  {is_number,'$1'},
		  {is_pid,'$1'},
		  {is_port,'$1'},
		  {is_reference,'$1'},
		  {is_tuple,'$1'},
		  {is_binary,'$1'},
		  {is_record,'$1',a,3}],
	    [true]}] =
	compile_and_run(RD, <<
			     "ets:fun2ms(fun(X) when integer(X),"
			     "float(X), atom(X),"
			     "list(X), number(X), pid(X),"
			     "port(X), reference(X), tuple(X),"
			     "binary(X), record(X,a) -> true end)"
			     >>),
    ok.
    
autoimported(suite) ->
    [];
autoimported(doc) ->
    ["Tests use of autoimported bif's used like erlang:'+'(A,B) in guards"
     " and body."];
autoimported(Config) when is_list(Config) ->
    ?line setup(Config),
    Allowed = [
	       {abs,1},
	       {element,2},
	       {hd,1},
	       {length,1},
	       {node,0},
	       {node,1},
	       {round,1},
	       {size,1},
	       {tl,1},
	       {trunc,1},
	       {self,0},
               %{float,1}, see float_1_function/1
	       {is_atom,1},
	       {is_float,1},
	       {is_integer,1},
	       {is_list,1},
	       {is_number,1},
	       {is_pid,1},
	       {is_port,1},
	       {is_reference,1},
	       {is_tuple,1},
	       {is_binary,1},
	       {is_function,1},
	       {is_record,2,magic},
	       {'and',2,infix},
	       {'or',2,infix},
	       {'xor',2,infix},
	       {'not',1},
	       %{'andalso',2,infix},
	       %{'orelse',2,infix},
	       {'+',1},
	       {'+',2,infix},
	       {'-',1},
	       {'-',2,infix},
	       {'*',2,infix},
	       {'/',2,infix},
	       {'div',2,infix},
	       {'rem',2,infix},
	       {'band',2,infix},
	       {'bor',2,infix},
	       {'bxor',2,infix},
	       {'bnot',1},
	       {'bsl',2,infix},
	       {'bsr',2,infix},
	       {'>',2,infix},
	       {'>=',2,infix},
	       {'<',2,infix},
	       {'=<',2,infix},
	       {'==',2,infix},
	       {'=:=',2,infix},
	       {'/=',2,infix},
	       {'=/=',2,infix}],
    ?line RD = <<"-record(a,{a,b}).">>,
    ?line lists:foreach(
	    fun({A,0}) ->
		    L = atom_to_list(A),
		    Bin1 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun(X) when ">>,
			      L,<<"() -> ">>,
			      L,<<"() end)">>
			     ]),
		    Bin2 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun(X) when erlang:'">>,
			      L,<<"'() -> erlang:'">>,
			      L,<<"'() end)">>
			     ]),
		    Res1 = compile_and_run(Bin1),
		    Res2 = compile_and_run(Bin2),
		    case Res1 =:= Res2 of
			true ->
			    ok;
			false ->
			    exit({not_equal,{Res1,Res2,A}})
		    end;
	    ({A,1}) ->
		    L = atom_to_list(A),
		    Bin1 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun(X) when ">>,
			      L,<<"(X) -> ">>,
			      L,<<"(X) end)">>
			     ]),
		    Bin2 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun(X) when erlang:'">>,
			      L,<<"'(X) -> erlang:'">>,
			      L,<<"'(X) end)">>
			     ]),
		    Res1 = compile_and_run(Bin1),
		    Res2 = compile_and_run(Bin2),
		    case Res1 =:= Res2 of
			true ->
			    ok;
			false ->
			    exit({not_equal,{Res1,Res2,A}})
		    end;
	    ({A,2}) ->
		    L = atom_to_list(A),
		    Bin1 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun({X,Y}) when ">>,
			      L,<<"(X,Y) -> ">>,
			      L,<<"(X,Y) end)">>
			     ]),
		    Bin2 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun({X,Y}) when erlang:'">>,
			      L,<<"'(X,Y) -> erlang:'">>,
			      L,<<"'(X,Y) end)">>
			     ]),
		    Res1 = compile_and_run(Bin1),
		    Res2 = compile_and_run(Bin2),
		    case Res1 =:= Res2 of
			true ->
			    ok;
			false ->
			    exit({not_equal,{Res1,Res2,A}})
		    end;
	    ({A,2,infix}) ->
		    L = atom_to_list(A),
		    Bin1 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun({X,Y}) when X ">>,
			      L,<<" Y -> X ">>,
			      L,<<" Y end)">>
			     ]),
		    Bin2 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun({X,Y}) when erlang:'">>,
			      L,<<"'(X,Y) -> erlang:'">>,
			      L,<<"'(X,Y) end)">>
			     ]),
		    Res1 = compile_and_run(Bin1),
		    Res2 = compile_and_run(Bin2),
		    case Res1 =:= Res2 of
			true ->
			    ok;
			false ->
			    exit({not_equal,{Res1,Res2,A}})
		    end;
	    ({A,2,magic}) -> %is_record
		    L = atom_to_list(A),
		    Bin1 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun(X) when ">>,
			      L,<<"(X,a) -> ">>,
			      L,<<"(X,a) end)">>
			     ]),
		    Bin2 = list_to_binary(
			     [
			      <<"ets:fun2ms(fun(X) when erlang:'">>,
			      L,<<"'(X,a) -> erlang:'">>,
			      L,<<"'(X,a) end)">>
			     ]),
		    Res1 = compile_and_run(RD,Bin1),
		    Res2 = compile_and_run(RD,Bin2),
		    case Res1 =:= Res2 of
			true ->
			    ok;
			false ->
			    exit({not_equal,{Res1,Res2,A}})
		    end
	    end,
	    Allowed),
    ok.

semicolon(suite) ->
    [];
semicolon(doc) ->
    ["Tests semicolon in guards of match_specs."];
semicolon(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line Res01 = compile_and_run
		   (<<"ets:fun2ms(fun(X) when is_integer(X); "
		     "is_float(X) -> true end)">>),
    ?line Res02 = compile_and_run
		   (<<"ets:fun2ms(fun(X) when is_integer(X) -> true; "
		     "(X) when is_float(X) -> true end)">>),
    ?line Res01 = Res02,
    ?line Res11 = compile_and_run
		   (<<"ets:fun2ms(fun(X) when is_integer(X); "
		     "is_float(X); atom(X) -> true end)">>),
    ?line Res12 = compile_and_run
		   (<<"ets:fun2ms(fun(X) when is_integer(X) -> true; "
		     "(X) when is_float(X) -> true; "
		     "(X) when is_atom(X) -> true end)">>),
    ?line Res11 = Res12,
    ok.
    
    
float_1_function(suite) ->
    [];
float_1_function(doc) ->
    ["OTP-5297. The function float/1."];
float_1_function(Config) when is_list(Config) ->
    ?line setup(Config),
    RunMS = fun(L, MS) -> 
                    ets:match_spec_run(L, ets:match_spec_compile(MS)) 
            end,
    ?line MS1 = compile_and_run
                  (<<"ets:fun2ms(fun(X) -> float(X) end)">>),
    ?line [F1] = RunMS([3], MS1),
    ?line true = is_float(F1) and (F1 == 3),
                  
    ?line MS1b = compile_and_run
                  (<<"dbg:fun2ms(fun(X) -> float(X) end)">>),
    ?line [F2] = RunMS([3], MS1b),
    ?line true = is_float(F2) and (F2 == 3),
                  
    ?line MS2 = compile_and_run
            (<<"ets:fun2ms(fun(X) when is_pid(X) or float(X) -> true end)">>),
    ?line [] = RunMS([3.0], MS2),

    ?line MS3 = compile_and_run
            (<<"dbg:fun2ms(fun(X) when is_pid(X); float(X) -> true end)">>),
    ?line [true] = RunMS([3.0], MS3),

    ?line MS4 = compile_and_run
            (<<"ets:fun2ms(fun(X) when erlang:float(X) > 1 -> big;"
               "              (_) -> small end)">>),
    ?line [small,big] = RunMS([1.0, 3.0], MS4),

    ?line MS5 = compile_and_run
            (<<"ets:fun2ms(fun(X) when float(X) > 1 -> big;"
               "              (_) -> small end)">>),
    ?line [small,big] = RunMS([1.0, 3.0], MS5),

    %% This is the test from autoimported/1.
    ?line [{'$1',[{is_float,'$1'}],[{float,'$1'}]}] = 
        compile_and_run
            (<<"ets:fun2ms(fun(X) when float(X) -> float(X) end)">>),
    ?line [{'$1',[{float,'$1'}],[{float,'$1'}]}] =
        compile_and_run
           (<<"ets:fun2ms(fun(X) when erlang:'float'(X) -> "
              "erlang:'float'(X) end)">>),
    ok.


action_function(suite) ->
    [];
action_function(doc) ->
    ["Test all 'action functions'."];
action_function(Config) when is_list(Config) ->
    ?line setup(Config),
    ?line [{['$1','$2'],[],
	    [{set_seq_token,label,0},
	     {get_seq_token},
	     {message,'$1'},
	     {return_trace},
	     {exception_trace}]}] =
	compile_and_run
	  (<<"dbg:fun2ms(fun([X,Y]) -> "
	    "set_seq_token(label, 0), "
	    "get_seq_token(), "
	    "message(X), "
	    "return_trace(), "
	    "exception_trace() end)">>),
    ?line [{['$1','$2'],[],
	    [{process_dump},
	     {enable_trace,send},
	     {enable_trace,'$2',send},
	     {disable_trace,procs},
	     {disable_trace,'$2',procs}]}] =
	compile_and_run
	  (<<"dbg:fun2ms(fun([X,Y]) -> "
	    "process_dump(), "
	    "enable_trace(send), "
	    "enable_trace(Y, send), "
	    "disable_trace(procs), "
	    "disable_trace(Y, procs) end)">>),
    ?line [{['$1','$2'],
	    [],
	    [{display,'$1'},
	     {caller},
	     {set_tcw,{const,16}},
	     {silent,true},
	     {trace,[send],[procs]},
	     {trace,'$2',[procs],[send]}]}] =
	compile_and_run
	  (<<"A = 16, dbg:fun2ms(fun([X,Y]) -> "
	    "display(X), "
	    "caller(), "
	    "set_tcw(A), "
	    "silent(true), "
	    "trace([send], [procs]), "
	    "trace(Y, [procs], [send])  end)">>),
    ok.


eep37(Config) when is_list(Config) ->
    setup(Config),
    [{'$1',[],['$1']}] =
        compile_and_run(<<"F = fun _Ms() ->\n"
                          "            ets:fun2ms(fun (X) -> X end)\n"
                          "    end,\n"
                          "F()">>).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup(Config) ->
    put(mts_config,Config),
    put(mts_tf_counter,0).

temp_name() ->
    Conf = get(mts_config),
    C = get(mts_tf_counter),
    put(mts_tf_counter,C+1),
    filename:join([?config(priv_dir,Conf),
		   "tempfile"++integer_to_list(C)++".tmp"]).


expect_failure(Recs,Code) ->
    case (catch compile_and_run(Recs,Code)) of
	      {'EXIT',_Foo} ->
		  %erlang:display(_Foo),
		  ok;
	      Other ->
		  exit({expected,failure,got,Other})
	  end.
 
compile_and_run(Expr) ->
    compile_and_run(<<>>,Expr).
compile_and_run(Records,Expr) ->
    Prog = <<
	"-module(tmp).\n",
    "-include_lib(\"stdlib/include/ms_transform.hrl\").\n",
    "-export([tmp/0]).\n",
    Records/binary,"\n",
    "tmp() ->\n",
    Expr/binary,".\n">>,
    FN=temp_name(),
    file:write_file(FN,Prog),
    {ok,Forms} = epp:parse_file(FN,"",""),
    {ok,tmp,Bin} = compile:forms(Forms),
    code:load_binary(tmp,FN,Bin),
    tmp:tmp().

compile_ww(Expr) ->
    compile_ww(<<>>,Expr).
compile_ww(Records,Expr) ->
    Prog = <<
	"-module(tmp).\n",
    "-include_lib(\"stdlib/include/ms_transform.hrl\").\n",
    "-export([tmp/0]).\n",
    Records/binary,"\n",
    "-file(?FILE, 0). ",
    "tmp() ->\n",
    Expr/binary,".\n">>,
    FN=temp_name(),
    file:write_file(FN,Prog),
    {ok,Forms} = epp:parse_file(FN,"",""),
    {ok,tmp,_Bin,Wlist} = compile:forms(Forms,[return_warnings,
					       nowarn_unused_vars,
					       nowarn_unused_record]),
    Wlist.

compile_no_ww(Expr) ->
    Prog = <<
	"-module(tmp).\n",
    "-include_lib(\"stdlib/include/ms_transform.hrl\").\n",
    "-export([tmp/1]).\n\n",
    Expr/binary,".\n">>,
    FN=temp_name(),
    file:write_file(FN,Prog),
    {ok,Forms} = epp:parse_file(FN,"",""),
    {ok,tmp,_Bin,Wlist} = compile:forms(Forms,[return_warnings,
					       nowarn_unused_vars,
					       nowarn_unused_record]),
    Wlist.

do_eval(String) ->
    {done,{ok,T,_},[]} = erl_scan:tokens(
			   [],
			   String++".\n",1),
    {ok,Tree} = erl_parse:parse_exprs(T),
    {value,Res,[]} =  erl_eval:exprs(Tree,[]),
    Res.
