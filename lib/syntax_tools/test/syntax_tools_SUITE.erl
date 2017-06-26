%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''

-module(syntax_tools_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%% Test cases
-export([app_test/1,appup_test/1,smoke_test/1,revert/1,revert_map/1,
	t_abstract_type/1,t_erl_parse_type/1,t_epp_dodger/1,
	t_comment_scan/1,t_igor/1,t_erl_tidy/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test,appup_test,smoke_test,revert,revert_map,
    t_abstract_type,t_erl_parse_type,t_epp_dodger,
    t_comment_scan,t_igor,t_erl_tidy].

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

app_test(Config) when is_list(Config) ->
    ok = ?t:app_test(syntax_tools).

appup_test(Config) when is_list(Config) ->
    ok = ?t:appup_test(syntax_tools).

%% Read and parse all source in the OTP release.
smoke_test(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(12)),
    Wc = filename:join([code:lib_dir(),"*","src","*.erl"]),
    Fs = filelib:wildcard(Wc) ++ test_files(Config),
    io:format("~p files\n", [length(Fs)]),
    case p_run(fun smoke_test_file/1, Fs) of
        0 -> ok;
        N -> ?t:fail({N,errors})
    end,
    ?t:timetrap_cancel(Dog).

smoke_test_file(File) ->
    case epp_dodger:parse_file(File) of
	{ok,Forms} ->
	    [print_error_markers(F, File) || F <- Forms],
	    ok;
	{error,Reason} ->
	    io:format("~s: ~p\n", [File,Reason]),
	    error
    end.

print_error_markers(F, File) ->
    case erl_syntax:type(F) of
	error_marker ->
	    {L,M,Info} = erl_syntax:error_marker_info(F),
	    io:format("~ts:~p: ~s", [File,L,M:format_error(Info)]);
	_ ->
	    ok
    end.
    

%% Read with erl_parse, wrap and revert with erl_syntax and check for equality.
revert(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(12)),
    Wc = filename:join([code:lib_dir("stdlib"),"src","*.erl"]),
    Fs = filelib:wildcard(Wc) ++ test_files(Config),
    Path = [filename:join(code:lib_dir(stdlib), "include"),
            filename:join(code:lib_dir(kernel), "include")],
    io:format("~p files\n", [length(Fs)]),
    case p_run(fun (File) -> revert_file(File, Path) end, Fs) of
        0 -> ok;
        N -> ?t:fail({N,errors})
        end,
    ?t:timetrap_cancel(Dog).

revert_file(File, Path) ->
    case epp:parse_file(File, Path, []) of
        {ok,Fs0} ->
            Fs1 = erl_syntax:form_list(Fs0),
            Fs2 = erl_syntax_lib:map(fun (Node) -> Node end, Fs1),
            Fs3 = erl_syntax:form_list_elements(Fs2),
            Fs4 = [ erl_syntax:revert(Form) || Form <- Fs3 ],
            {ok,_} = compile:forms(Fs4, [report,strong_validation]),
            ok
    end.

%% Testing bug fix for reverting map_field_assoc
revert_map(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(1)),
    [{map_field_assoc,16,{atom,17,name},{var,18,'Value'}}] =
    erl_syntax:revert_forms([{tree,map_field_assoc,
                             {attr,16,[],none},
			     {map_field_assoc,{atom,17,name},{var,18,'Value'}}}]),
    ?t:timetrap_cancel(Dog).



%% api tests

t_abstract_type(Config) when is_list(Config) ->
    F = fun validate_abstract_type/1,
    ok = validate(F,[{hi,atom},
		     {1,integer},
		     {1.0,float},
		     {$a,integer},
		     {[],nil},
		     {[<<1,2>>,a,b],list},
		     {[2,3,<<1,2>>,a,b],list},
		     {[$a,$b,$c],string},
		     {"hello world",string},
		     {<<1,2,3>>,binary},
		     {#{a=>1,"b"=>2},map_expr},
		     {#{#{i=>1}=>1,"b"=>#{v=>2}},map_expr},
		     {{a,b,c},tuple}]),
    ok.

t_erl_parse_type(Config) when is_list(Config) ->
    F = fun validate_erl_parse_type/1,
    %% leaf types
    ok = validate(F,[{"1",integer,true},
		     {"123456789",integer,true},
		     {"$h", char,true},
		     {"3.1415", float,true},
		     {"1.33e36", float,true},
		     {"\"1.33e36: hello\"", string,true},
		     {"Var1", variable,true},
		     {"_", underscore,true},
		     {"[]", nil,true},
		     {"{}", tuple,true},
		     {"#{}",map_expr,true},
		     {"'some atom'", atom, true}]),
    %% composite types
    ok = validate(F,[{"case X of t -> t; f -> f end", case_expr,false},
		     {"try X of t -> t catch C:R -> error end", try_expr,false},
		     {"receive X -> X end", receive_expr,false},
		     {"receive M -> X1 after T -> X2 end", receive_expr,false},
		     {"catch (X)", catch_expr,false},
		     {"fun(X) -> X end", fun_expr,false},
		     {"fun Foo(X) -> X end", named_fun_expr,false},
		     {"fun foo/2", implicit_fun,false},
		     {"fun bar:foo/2", implicit_fun,false},
		     {"if X -> t; true -> f end", if_expr,false},
		     {"<<1,2,3,4>>", binary,false},
		     {"<<1,2,3,4:5>>", binary,false},
		     {"<<V1:63,V2:22/binary, V3/bits>>", binary,false},
		     {"begin X end", block_expr,false},
		     {"foo(X1,X2)", application,false},
		     {"bar:foo(X1,X2)", application,false},
		     {"[1,2,3,4]", list,false},
		     {"[1|4]", list, false},
		     {"[<<1>>,<<2>>,-2,<<>>,[more,list]]", list,false},
		     {"[1|[2|[3|[4|[]]]]]", list,false},
		     {"#{ a=>1, b=>2 }", map_expr,false},
		     {"#{3=>3}#{ a=>1, b=>2 }", map_expr,false},
		     {"#{ a:=1, b:=2 }", map_expr,false},
		     {"M#{ a=>1, b=>2 }", map_expr,false},
		     {"[V||V <- Vs]", list_comp,false},
		     {"<< <<B>> || <<B>> <= Bs>>", binary_comp,false},
		     {"#state{ a = A, b = B}", record_expr,false},
		     {"#state{}", record_expr,false},
		     {"#s{ a = #def{ a=A }, b = B}", record_expr,false},
		     {"State#state{ a = A, b = B}", record_expr,false},
		     {"State#state.a", record_access,false},
		     {"#state.a", record_index_expr,false},
		     {"-X", prefix_expr,false},
		     {"X1 + X2", infix_expr,false},
		     {"(X1 + X2) * X3", infix_expr,false},
		     {"X1 = X2", match_expr,false},
		     {"{a,b,c}", tuple,false}]),
    ok.

%% the macro ?MODULE seems faulty
t_epp_dodger(Config) when is_list(Config) ->
    DataDir   = ?config(data_dir, Config),
    PrivDir   = ?config(priv_dir, Config),
    Filenames = test_files(),
    ok = test_epp_dodger(Filenames,DataDir,PrivDir),
    ok.

t_comment_scan(Config) when is_list(Config) ->
    DataDir   = ?config(data_dir, Config),
    Filenames = test_files(),
    ok = test_comment_scan(Filenames,DataDir),
    ok.

test_files(Config) ->
    DataDir = ?config(data_dir, Config),
    [ filename:join(DataDir,Filename) || Filename <- test_files() ].

test_files() ->
    ["syntax_tools_SUITE_test_module.erl",
     "syntax_tools_test.erl",
     "type_specs.erl"].

t_igor(Config) when is_list(Config) ->
    DataDir   = ?config(data_dir, Config),
    PrivDir   = ?config(priv_dir, Config),
    FileM1  = filename:join(DataDir,"m1.erl"),
    FileM2  = filename:join(DataDir,"m2.erl"),
    ["m.erl",_]=R = igor:merge(m,[FileM1,FileM2],[{outdir,PrivDir}]),
    io:format("igor:merge/3 = ~p~n", [R]),

    FileTypeSpecs = filename:join(DataDir,"igor_type_specs.erl"),
    Empty = filename:join(DataDir,"empty.erl"),
    ["n.erl",_]=R2 = igor:merge(n,[FileTypeSpecs,Empty],[{outdir,PrivDir}]),
    io:format("igor:merge/3 = ~p~n", [R2]),

    ok.

t_erl_tidy(Config) when is_list(Config) ->
    DataDir   = ?config(data_dir, Config),
    File  = filename:join(DataDir,"erl_tidy_tilde.erl"),
    ok = erl_tidy:file(File, [{stdout, true}]),

    %% OTP-14471.
    Old = process_flag(trap_exit, true),
    NonExisting  = filename:join(DataDir,"non_existing_file.erl"),
    {'EXIT',{error,{0,file,enoent}}} = (catch erl_tidy:file(NonExisting)),
    true = process_flag(trap_exit, Old),
    ok.

test_comment_scan([],_) -> ok;
test_comment_scan([File|Files],DataDir) ->
    Filename  = filename:join(DataDir,File),
    {ok, Fs0} = epp:parse_file(Filename, [], []),
    Comments  = erl_comment_scan:file(Filename),
    Fun = fun(Node) ->
		  case erl_syntax:is_form(Node) of
		      true ->
			  C1    = erl_syntax:comment(2,[" This is a form."]),
			  Node1 = erl_syntax:add_precomments([C1],Node),
			  Node1;
		      false ->
			  Node
		  end
	  end,
    Fs1 = erl_recomment:recomment_forms(Fs0, Comments),
    Fs2 = erl_syntax_lib:map(Fun, Fs1),
    io:format("File: ~s~n", [Filename]),
    io:put_chars(erl_prettypr:format(Fs2, [{paper,  120},
					   {ribbon, 110}])),
    test_comment_scan(Files,DataDir).


test_epp_dodger([], _, _) -> ok;
test_epp_dodger([Filename|Files],DataDir,PrivDir) ->
    io:format("Parsing ~p~n", [Filename]),
    InFile   = filename:join(DataDir, Filename),
    Parsers  = [{fun epp_dodger:parse_file/1,parse_file},
		{fun epp_dodger:quick_parse_file/1,quick_parse_file},
		{fun (File) ->
			{ok,Dev} = file:open(File,[read]),
			Res = epp_dodger:parse(Dev),
			file:close(File),
			Res
		 end, parse},
		{fun (File) ->
			{ok,Dev} = file:open(File,[read]),
			Res = epp_dodger:quick_parse(Dev),
			file:close(File),
			Res
		 end, quick_parse}],
    FsForms  = parse_with(Parsers, InFile),
    ok = pretty_print_parse_forms(FsForms,PrivDir,Filename),
    test_epp_dodger(Files,DataDir,PrivDir).

parse_with([],_) -> [];
parse_with([{Fun,ParserType}|Funs],File) ->
    {ok, Fs} = Fun(File),
    [{Fs,ParserType}|parse_with(Funs,File)].

pretty_print_parse_forms([],_,_) -> ok;
pretty_print_parse_forms([{Fs0,Type}|FsForms],PrivDir,Filename) ->
    Parser  = atom_to_list(Type),
    OutFile = filename:join(PrivDir, Parser ++"_" ++ Filename),
    io:format("Pretty print ~p (~w) to ~p~n", [Filename,Type,OutFile]),
    Comment = fun (Node,{CntCase,CntTry}=Cnt) ->
		      case erl_syntax:type(Node) of
			  case_expr ->
			      C1    = erl_syntax:comment(2,["Before a case expression"]),
			      Node1 = erl_syntax:add_precomments([C1],Node),
			      C2    = erl_syntax:comment(2,["After a case expression"]),
			      Node2 = erl_syntax:add_postcomments([C2],Node1),
			      {Node2,{CntCase+1,CntTry}};
			  try_expr ->
			      C1    = erl_syntax:comment(2,["Before a try expression"]),
			      Node1 = erl_syntax:set_precomments(Node,
						     erl_syntax:get_precomments(Node) ++ [C1]),
			      C2    = erl_syntax:comment(2,["After a try expression"]),
			      Node2 = erl_syntax:set_postcomments(Node1,
						     erl_syntax:get_postcomments(Node1) ++ [C2]),
			      {Node2,{CntCase,CntTry+1}};
			  _ ->
			      {Node,Cnt}
		      end
	      end,
    Fs1 = erl_syntax:form_list(Fs0),
    {Fs2,{CC,CT}} = erl_syntax_lib:mapfold(Comment,{0,0}, Fs1),
    io:format("Commented on ~w cases and ~w tries~n", [CC,CT]),
    PP  = erl_prettypr:format(Fs2),
    ok  = file:write_file(OutFile,iolist_to_binary(PP)),
    pretty_print_parse_forms(FsForms,PrivDir,Filename).


validate(_,[]) -> ok;
validate(F,[V|Vs]) ->
    ok = F(V),
    validate(F,Vs).


validate_abstract_type({Lit,Type}) ->
    Tree = erl_syntax:abstract(Lit),
    ok   = validate_special_type(Type,Tree),
    Type = erl_syntax:type(Tree),
    true = erl_syntax:is_literal(Tree),
    ErlT = erl_syntax:revert(Tree),
    Type = erl_syntax:type(ErlT),
    ok   = validate_special_type(Type,ErlT),
    Conc = erl_syntax:concrete(Tree),
    Lit  = Conc,
    ok.

validate_erl_parse_type({String,Type,Leaf}) ->
    ErlT = string_to_expr(String),
    ok   = validate_special_type(Type,ErlT),
    Type = erl_syntax:type(ErlT),
    Leaf = erl_syntax:is_leaf(ErlT),
    Tree = erl_syntax_lib:map(fun(Node) -> Node end, ErlT),
    Type = erl_syntax:type(Tree),
    _    = erl_syntax:meta(Tree),
    ok   = validate_special_type(Type,Tree),
    RevT = erl_syntax:revert(Tree),
    ok   = validate_special_type(Type,RevT),
    Type = erl_syntax:type(RevT),
    ok.

validate_special_type(string,Node) ->
    Val  = erl_syntax:string_value(Node),
    true = erl_syntax:is_string(Node,Val),
    _    = erl_syntax:string_literal(Node),
    ok;
validate_special_type(variable,Node) ->
    _ = erl_syntax:variable_literal(Node),
    ok;
validate_special_type(fun_expr,Node) ->
    A = erl_syntax:fun_expr_arity(Node),
    true = is_integer(A),
    ok;
validate_special_type(named_fun_expr,Node) ->
    A = erl_syntax:named_fun_expr_arity(Node),
    true = is_integer(A),
    ok;
validate_special_type(tuple,Node) ->
    Size = erl_syntax:tuple_size(Node),
    true = is_integer(Size),
    ok;
validate_special_type(float,Node) ->
    Str   = erl_syntax:float_literal(Node),
    Val   = list_to_float(Str),
    Val   = erl_syntax:float_value(Node),
    false = erl_syntax:is_proper_list(Node),
    false = erl_syntax:is_list_skeleton(Node),
    ok;
validate_special_type(integer,Node) ->
    Str   = erl_syntax:integer_literal(Node),
    Val   = list_to_integer(Str),
    true  = erl_syntax:is_integer(Node,Val),
    Val   = erl_syntax:integer_value(Node),
    false = erl_syntax:is_proper_list(Node),
    ok;
validate_special_type(nil,Node) ->
    true  = erl_syntax:is_proper_list(Node),
    ok;
validate_special_type(list,Node) ->
    true  = erl_syntax:is_list_skeleton(Node),
    _     = erl_syntax:list_tail(Node),
    ErrV  = erl_syntax:list_head(Node),
    false = erl_syntax:is_string(Node,ErrV),
    Norm  = erl_syntax:normalize_list(Node),
    list  = erl_syntax:type(Norm),
    case erl_syntax:is_proper_list(Node) of
	true ->
	    true = erl_syntax:is_list_skeleton(Node),
	    Compact = erl_syntax:compact_list(Node),
	    list = erl_syntax:type(Compact),
	    [_|_] = erl_syntax:list_elements(Node),
	    _  = erl_syntax:list_elements(Node),
	    N  = erl_syntax:list_length(Node),
	    true = N > 0,
	    ok;
	false ->
	    ok
    end;
validate_special_type(_,_) ->
    ok.

%%% scan_and_parse

string_to_expr(String) ->
    io:format("Str: ~p~n", [String]),
    {ok, Ts, _} = erl_scan:string(String++"."),
    {ok,[Expr]} = erl_parse:parse_exprs(Ts),
    Expr.


p_run(Test, List) ->
    N = erlang:system_info(schedulers),
    p_run_loop(Test, List, N, [], 0).

p_run_loop(_, [], _, [], Errors) ->
    Errors;
p_run_loop(Test, [H|T], N, Refs, Errors) when length(Refs) < N ->
    {_,Ref} = erlang:spawn_monitor(fun() -> exit(Test(H)) end),
    p_run_loop(Test, T, N, [Ref|Refs], Errors);
p_run_loop(Test, List, N, Refs0, Errors0) ->
    receive
	{'DOWN',Ref,process,_,Res} ->
	    Errors = case Res of
			 ok -> Errors0;
			 error -> Errors0+1
		     end,
	    Refs = Refs0 -- [Ref],
	    p_run_loop(Test, List, N, Refs, Errors)
    end.
