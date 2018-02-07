%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : gl_gen.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 16 Apr 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(gl_gen).

-export([code/0]).

-include_lib("xmerl/include/xmerl.hrl").
-include("gl_gen.hrl").

-import(lists, [foldl/3,foldr/3,reverse/1,reverse/2,keysearch/3,map/2,filter/2]).
-import(proplists, [get_value/2,get_value/3]).

-import(gen_util,[uppercase_all/1]).

-compile(export_all).

code() ->  safe(fun gen_code/0,true).

devcode() -> spawn(fun() -> safe(fun gen_code/0,false) end).

safe(What, QuitOnErr) ->
    try 
	What(),
	io:format("Completed successfully~n~n", []),
	QuitOnErr andalso gen_util:halt(0)
    catch Err:Reason:Stacktrace ->
	    io:format("Error ~p: ~p:~p~n  ~p~n", 
		      [get(current_func),Err,Reason,Stacktrace]),
	    (catch gen_util:close()),
	    timer:sleep(1999),
	    QuitOnErr andalso gen_util:halt(1)
    end.

gen_code() ->
    {ok, Opts0} = file:consult("glapi.conf"),
    erase(func_id),
    Opts = init_defs(Opts0),
    GLUDefs = parse_glu_defs(Opts),   
    GLDefs  = parse_gl_defs(Opts),    
    {GLUDefines,GLUFuncs} = setup(GLUDefs, Opts),
    {GLDefines,GLFuncs}   = setup(GLDefs, Opts),
    gl_gen_erl:gl_defines(GLDefines),
    gl_gen_erl:gl_api(GLFuncs),
    gl_gen_erl:glu_defines(GLUDefines),
    gl_gen_erl:glu_api(GLUFuncs),

    %%gl_gen_erl:gen_debug(GLFuncs,GLUFuncs),
    gl_gen_c:gen(GLFuncs,GLUFuncs),
    ok.

init_defs(Opts0) ->
    Opts0.

parse_glu_defs(Opts0) ->
    All = foldl(fun(File, Acc) -> load_file(File,Opts0,Acc) end, [], ["glu"]),
    reverse(All).

parse_gl_defs(Opts0) ->
    All = foldl(fun(File, Acc) -> load_file(File,Opts0,Acc) end, [], ["gl","glext"]),
    reverse(All).

load_file(FileName, Opts, Acc) ->
    File = filename:join(["gl_xml",FileName ++ "_8h.xml"]),
    put({loaded, FileName}, true),
    case xmerl_scan:file(File, [{space, normalize}]) of 
	{error, enoent} ->
	    io:format("Skipped File not found ~p ~n", [File]),
	    Acc;
	{Doc, _} ->
	    io:format("Scanning ~p ~n", [File]),
	    %% It's duplicated in xml get sectiondef only once.
	    Content = find_data(Doc#xmlElement.content),
	    lists:foreach(fun(D) -> extract_argnames(D) end, Content),
	    foldl(fun(Data,Acc0) -> parse_file(Data, Opts, Acc0) end, 
		  Acc, Content)
    end.

extract_argnames(#xmlElement{name=memberdef,attributes=Attr,content=C}) ->
    case keysearch(kind, #xmlAttribute.name, Attr) of
	{value, #xmlAttribute{value = "typedef"}} -> 
	    parse_typedef(C,undefined);
	_ ->
	    ok
    end;
extract_argnames(_) -> ok.
    
parse_typedef([#xmlElement{name=argsstring,content=[#xmlText{value=AS}]}|R],_) -> 
    parse_typedef(R,AS);
parse_typedef([#xmlElement{name=name}|_],undefined) ->
    skip;
parse_typedef([#xmlElement{name=name,content=[#xmlText{value=N}]}|_],AS) ->
    Args0 = string:tokens(AS," ,()*[]"),
    try 
	Args = get_arg_names(Args0),
	put({typedef,string:strip(N)},Args)
    catch _:Where ->
	    io:format("Error ~p: ~p ~p~n", [N,Args0,Where]),
	    ?error(arg_names)
    end;
parse_typedef([_|R],AS) ->
    parse_typedef(R,AS);
parse_typedef([],_) -> skip.

get_arg_names(As0) ->
    Args = lists:filter(fun("const") -> false; (_) -> true end, As0),
    get_arg_names(Args, []).

get_arg_names([_Type,Name|R],Acc) ->
    get_arg_names(R, [Name|Acc]);
get_arg_names([],Acc) -> reverse(Acc);
get_arg_names(["void"],[]) -> [];
get_arg_names(Error,_Acc) -> exit(Error).
    
%% Avoid bugs in (old) doxygen..the new one doesn't have 'em
find_data([#xmlElement{name=compounddef, content=C}|_]) -> find_data(C);
find_data([#xmlElement{name=sectiondef, attributes=Attr, content=C}|R]) ->  
    case keysearch(kind, #xmlAttribute.name, Attr) of
	{value, _} -> %% The new one have func define typedef
	    find_data(R) ++ C;
	false ->
	    C
    end;
find_data([_Hmm|R]) -> 
%%     case _Hmm of 
%% 	#xmlElement{} -> 
%% 	    io:format("0 ~p ~n",[_Hmm#xmlElement.name]);
%% 	_ ->
%% 	    ok
%%     end,
    find_data(R);
find_data([]) -> [].

parse_file(#xmlElement{name=memberdef,attributes=Attr, content=C}, Opts, Acc) ->
    case keysearch(kind, #xmlAttribute.name, Attr) of
	{value, #xmlAttribute{value = "function"}} -> 
	    try 
		Def = parse_func(C, Opts),
		[Def|Acc]
	    catch throw:skip -> Acc
	    after erase(current_func)
	    end;
	{value, #xmlAttribute{value = "define"}} -> 
	    try 
		Def = parse_define(C, #def{}, Opts),
		[Def|Acc]
	    catch throw:skip -> Acc
	    end;
	{value, #xmlAttribute{value = "typedef"}} -> 
	    Acc;
	_W ->
	    io:format("Hmm ~p~n",[_W]),
	    Acc
    end;
parse_file(_Hmm,_,Acc) ->
    Acc.

parse_define([#xmlElement{name=name,content=[#xmlText{value="API" ++ _}]}|_],_Def,_Os) ->
    throw(skip);
parse_define([#xmlElement{name=name,content=[#xmlText{value="GLAPI"++_}]}|_],_Def,_Os) ->
    throw(skip);
parse_define([#xmlElement{name=name,content=[#xmlText{value="WINGDIAPI"++_}]}|_],_Def,_Os) ->
    throw(skip);
parse_define([#xmlElement{name=name,content=[#xmlText{value=Name}]}|R], Def, Os) ->
    parse_define(R, Def#def{name=Name}, Os);
parse_define([#xmlElement{name=initializer,content=Contents}|_R],Def,_Os) ->
    Val0 = extract_def2(Contents),
    try
	case Val0 of
	    "0x" ++ Val1 ->
		Val2 = strip_type_cast(Val1),
		_ = list_to_integer(Val2, 16),
		Def#def{val=Val2, type=hex};
	    _ ->
		Val = list_to_integer(Val0),
		Def#def{val=Val, type=int}
	end
    catch _:_ ->
	    case catch list_to_float(Val0) of
		{'EXIT', _} -> Def#def{val=Val0, type=string};
		_ -> Def#def{val=Val0, type=float_str}
	    end
    end;
parse_define([_|R], D, Opts) ->
    parse_define(R, D, Opts);
parse_define([], D, _Opts) ->
    D.

extract_def2([#xmlText{value=Val}|R]) ->
    strip_comment(string:strip(Val)) ++ extract_def2(R);
extract_def2([#xmlElement{content=Cs}|R]) ->
    extract_def2(Cs) ++ extract_def2(R);
extract_def2([]) -> [].

strip_type_cast(Int) ->
    lists:reverse(strip_type_cast2(lists:reverse(Int))).

strip_type_cast2("u"++Rest) -> Rest; %% unsigned
strip_type_cast2("lu"++Rest) -> Rest; %% unsigned  long
strip_type_cast2("llu"++Rest) -> Rest; %% unsigned long long
strip_type_cast2(Rest) -> Rest.


strip_comment("/*" ++ Rest) ->
    strip_comment_until_end(Rest);
strip_comment("//" ++ _) -> [];
strip_comment([H|R]) -> [H | strip_comment(R)];
strip_comment([]) -> [].

strip_comment_until_end("*/" ++ Rest) ->
    strip_comment(Rest);
strip_comment_until_end([_|R]) ->
    strip_comment_until_end(R).

parse_func(Xml, Opts) ->
    {Func,_} = foldl(fun(X,Acc) -> parse_func(X,Acc,Opts) end, {#func{},1}, Xml),
    put(current_func, Func#func.name),
    #func{params=Args0,type=Type0} = Func,
    Args = filter(fun(#arg{type=void}) -> false; (_) -> true end, Args0),
    #arg{type=Type} = patch_param(Func#func.name,#arg{name="result",type=Type0},Opts),
    Func#func{params=reverse(Args), type=Type}.

parse_func(#xmlElement{name=type, content=C}, {F,AC}, Os) ->
    Type = parse_type(drop_empty(C), Os),
    {F#func{type=Type},AC};
parse_func(#xmlElement{name=name, content=[#xmlText{value=C}]},{F,AC},Os) ->
    Func = string:strip(C),
    put(current_func, Func),
    {F#func{name=name(Func,Os)},AC};
parse_func(#xmlElement{name=param, content=C},{F,AC},Os) -> 
    put(current_func, F#func.name),
    Parse  = fun(Con, Ac) -> parse_param(Con, Ac, Os) end,
    Param0 = foldl(Parse, #arg{}, drop_empty(C)),
    Param = fix_param_name(Param0, F, AC),
    {add_param(Param, Os, F),AC+1};
parse_func(_, F,_) ->
    F.

fix_param_name(A=#arg{name=undefined,type=T},#func{name=Func},Count) ->
    TDName = "PFN" ++ uppercase_all(Func) ++ "PROC",
    case get({typedef,TDName}) of
	undefined when T == void ->
	    A;
	undefined ->
	    io:format("Didn't find typedef for: ~s~n", [TDName]),
	    exit(aargh);
	AS ->
	    try A#arg{name = lists:nth(Count, AS)} 
	    catch _:_ -> A
	    end
    end;
fix_param_name(A,_,_) -> A.

parse_param(#xmlElement{name=type,content=C}, Arg, Os) ->
    Arg#arg{type=parse_type(drop_empty(C),Os)};
parse_param(#xmlElement{name=declname,content=[C]},Arg,_Os) -> 
    #xmlText{value=Name} = C,
    Arg#arg{name=Name};
parse_param(#xmlElement{name=array,content=[#xmlText{value=C}]},
	    Arg=#arg{type=Type0},_Os) ->
    try 
	[Int] = string:tokens(C, "[] "),
	Val = list_to_integer(Int),
	Arg#arg{type=Type0#type{single={tuple,Val}, by_val=true}}
    catch _:_ ->
	    ?warning("Undefined Array size ~p in ~p ~p~n", 
		     [Arg, get(current_func), C]),
	    Arg#arg{type=Type0#type{single={tuple,undefined}, by_val=true}}
    end;
	    
%% Skip these
parse_param(#xmlElement{name=definition}, Arg, _) ->    Arg;
parse_param(#xmlElement{name=argsstring}, Arg,_) ->     Arg;
parse_param(#xmlElement{name=briefdescription}, Arg,_) ->     Arg;
parse_param(#xmlElement{name=detaileddescription}, Arg,_) ->  Arg;
parse_param(#xmlElement{name=inbodydescription}, Arg,_) ->    Arg;
parse_param(#xmlElement{name=location}, Arg,_) ->             Arg;
parse_param(Other, Arg,_) ->
    io:format("Unhandled Param ~p ~n in ~p~n", [Other,Arg]),
    ?error(unhandled_param).

add_param(Arg0=#arg{type=T0}, Opts, F=#func{name=Name,params=Args}) ->
    Arg = case T0 of 
%% 	      #type{mod=[const],ref={pointer,1},name="GLubyte"} -> 
%% 		  Arg0#arg{type=T0#type{base=binary}};
	      #type{mod=[const]}     -> Arg0;   %% In is true default
	      #type{ref={pointer,_}} -> Arg0#arg{in=false,
						 type=T0#type{single=undefined}};
	      _ -> Arg0
	  end,
    Patched = patch_param(Name,Arg,Opts),
    F#func{params=[Patched|Args]}.

patch_param(Method,P = #arg{name=ArgName},AllOpts) ->    
    %%io:format("~p ~n", [Method]),
    case lookup(Method,AllOpts,undefined) of
	undefined -> P;
	What -> 
	    %%	    io:format("~p ~p => ~p~n", [Method, ArgName, What]),
	    case What of
		{ArgName,Fopt} when is_list(Fopt) ->
		    foldl(fun handle_arg_opt/2,P,Fopt);
		{ArgName,Fopt}  ->
		    handle_arg_opt(Fopt,P);
		{_,_} -> P;
		Opts when is_list(Opts) ->
		    case get_value(ArgName, Opts, undefined) of
			undefined -> P;
			List when is_list(List) -> 
			    foldl(fun handle_arg_opt/2,P,List);
			Val -> 
			    handle_arg_opt(Val,P)
		    end
	    end
    end.

handle_arg_opt(skip, P) -> P#arg{where=c};
%%handle_arg_opt(nowhere, P) -> P#arg{where=nowhere};
%%handle_arg_opt(skip_member, _P) -> throw(skip_member);
handle_arg_opt(in, P) -> P#arg{in=true};
handle_arg_opt(out, P) -> P#arg{in=false};
handle_arg_opt(both, P) -> P#arg{in=both};
handle_arg_opt(binary, P=#arg{type=T}) -> 
    P#arg{type=T#type{size=undefined,base=binary}};
handle_arg_opt({binary,Sz}, P=#arg{type=T}) -> 
    P#arg{type=T#type{size={Sz, Sz},base=binary}};
handle_arg_opt({binary,Max, Sz}, P=#arg{type=T}) -> 
    P#arg{type=T#type{size={Max, Sz},base=binary}};
handle_arg_opt({type,Type}, P=#arg{type=T}) -> P#arg{type=T#type{name=Type}};
handle_arg_opt({single,Opt},P=#arg{type=T}) -> P#arg{type=T#type{single=Opt}};
handle_arg_opt({base,{Opt, Sz}},  P=#arg{type=T}) -> P#arg{type=T#type{base=Opt, size=Sz}};
handle_arg_opt({base,Opt},  P=#arg{type=T}) -> P#arg{type=T#type{base=Opt}};
handle_arg_opt({c_only,Opt},P) -> P#arg{where=c, alt=Opt};
handle_arg_opt(list_binary, P) -> P#arg{alt=list_binary};
handle_arg_opt(string,  P=#arg{type=T}) -> P#arg{type=T#type{base=string}};
handle_arg_opt({string,Max,Sz}, P=#arg{type=T}) ->
    P#arg{type=T#type{base=string, size={Max,Sz}}}.

parse_type([], _Os) -> void;
parse_type(C, Os) -> 
    {Type,_Info} = foldl(fun extract_type_info/2,{[],undefined},C),
    Empty = #type{},
    case parse_type2(reverse(Type),Empty,Os) of
	Empty ->  ?error({strange_type, Type});
	Assert -> Assert
    end.

extract_type_info(#xmlText{value=Value}, {Acc, Info}) -> 
    {reverse(foldl(fun extract_type_info2/2, [], 
		   string:tokens(Value, " "))) ++ Acc, Info};
extract_type_info(#xmlElement{name=ref,attributes=As,
			      content=[#xmlText{value=V}]},
		  {Acc,undefined}) ->
    {value, #xmlAttribute{value = Refid}} = 
	keysearch(refid,#xmlAttribute.name,As),
    {value, #xmlAttribute{value = Kind}} = 
	keysearch(kindref,#xmlAttribute.name,As),
    {reverse(foldl(fun extract_type_info2/2, [], 
		   string:tokens(V, " "))) ++ Acc,
     {Kind,Refid}};
extract_type_info(What,Acc) ->
    ?error({parse_error,What,Acc}).

extract_type_info2("const",Acc) -> [const|Acc];
extract_type_info2("*", [{by_ref,{pointer,N}}|Acc]) -> 
    [{by_ref,{pointer,N+1}}|Acc];
extract_type_info2("*",   Acc) -> [{by_ref,{pointer,1}}|Acc];
extract_type_info2("**",  Acc) -> [{by_ref,{pointer,2}}|Acc];
extract_type_info2(Type,  Acc) -> [Type|Acc].

parse_type2(["void"],  _T, _Opts) ->  void;
parse_type2([N="void", const|R], T, Opts) ->
    parse_type2([const|R],T#type{name=N, base=idx_binary},Opts);
parse_type2([N="void"|R],  T, Opts) ->
    parse_type2(R,T#type{name=N},Opts);
parse_type2([const|R],T=#type{mod=Mod},Opts) -> 
    parse_type2(R,T#type{mod=[const|Mod]},Opts);
parse_type2(["unsigned"|R],T=#type{mod=Mod},Opts) -> 
    parse_type2(R,T#type{mod=[unsigned|Mod]},Opts);
parse_type2([N="GLenum"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4, base=int},Opts);
parse_type2([N="GLboolean"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=1, base=bool},Opts);
parse_type2([N="GLbitfield"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4, base=int},Opts);
parse_type2([N="GLvoid"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, base=idx_binary},Opts);
parse_type2([N="GLsync"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, base=int, size=8},Opts);

parse_type2([N="GLbyte"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=1, base=int},Opts);
parse_type2([N="GLubyte"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=1, base=int},Opts);
parse_type2([N="GLshort"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=2, base=int},Opts);
parse_type2([N="GLushort"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=2, base=int},Opts);
parse_type2([N="GLint"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4, base=int},Opts);
parse_type2([N="GLint64"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=8, base=int},Opts);
parse_type2([N="GLuint64"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=8, base=int},Opts);

parse_type2([N="GLuint"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4, base=int},Opts);
parse_type2([N="GLsizei"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4, base=int},Opts);

parse_type2([N="GLfloat"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4,base=float},Opts);
parse_type2([N="GLdouble"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=8,base=float},Opts);
parse_type2([N="GLclampf"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=4,base=float},Opts);
parse_type2([N="GLclampd"|R],T,Opts) -> 
    parse_type2(R,T#type{name=N, size=8,base=float},Opts);
parse_type2([N="GLhandleARB"|R],T,Opts) ->  
    parse_type2(R,T#type{name=N, size=8,base=int},Opts); 
parse_type2(["GLchar" ++ _ARB|R],T,Opts) -> 
    parse_type2(R,T#type{name="GLchar",size=1,base=string},Opts);
parse_type2(["GLUquadric"|R],T,Opts) -> 
    parse_type2(R,T#type{name="GLUquadric",size=8,base=int},Opts);
parse_type2(["GLintptr" ++ _ARB|R],T,Opts) -> 
    parse_type2(R,T#type{name="GLintptr",size=8,base=int},Opts);
parse_type2(["GLsizeiptr" ++ _ARB|R],T,Opts) -> 
    parse_type2(R,T#type{name="GLsizeiptr",size=8,base=int},Opts);

parse_type2([{by_ref,Ref}|R],T,Opts) -> 
    parse_type2(R,T#type{ref=Ref,by_val=false},Opts);

%% Let type errors be seen later because we don't know if these unhandled types
%% will be used.
parse_type2(_A = [Name|R],T,Opts) ->
%%    io:format("unhandled ~p ~p ~n",[_A,T]),
    New = T#type{name={unhandled,Name,get(current_func)}},
    parse_type2(R,New,Opts);
parse_type2([], T, _) -> T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function mangling                         %%

setup(Defs,Opts) ->
    {Fs0,Ds0} = 
	foldr(fun(F=#func{name=N},{Fs,Ds}) ->  
		      case put(N,F) of
			  undefined ->
			      {[N|Fs],Ds};
			  _ ->  %% Duplicate
			      {Fs,Ds}
		      end;
		 (D=#def{}, {Fs,Ds}) ->
		      {Fs,[D|Ds]}
	      end, {[],[]}, Defs),
    Fs = setup_functions(Fs0,Opts,[]),
    erase(current_func),
    %% Remove duplicates but keep order
    {Ds,_} = foldl(fun(D=#def{name=Name},{Keep,Defined}) ->
			   case gb_sets:is_member(Name,Defined) of
			       true -> {Keep,Defined};
			       false -> {[D|Keep],gb_sets:add(Name,Defined)}
			   end
		   end, {[],gb_sets:empty()}, Ds0),
    {reverse(Ds),Fs}.

setup_functions([F0|Defs],Opts,Acc) ->
    put(current_func, F0),
    {Name, Ext} = get_extension(F0,Opts),
    %%io:format("~p = ~p + ~p~n", [F0, Name, Ext]),
    Skip = (not keep(F0,Opts)) andalso (skip(F0,Opts) orelse skip(Ext,Opts)),
    case Skip of
	true ->
	    setup_functions(Defs,Opts,Acc);
	false -> 
	    case setup_extension(Name,Ext,Opts) of
		skip -> 
		    setup_functions(Defs,Opts,Acc);
		New  -> 
		    setup_functions(Defs,Opts,[New|Acc])
	    end
    end;
setup_functions([],_, Acc) -> reverse(Acc).

setup_extension(Name,"",Opts) ->
    setup_vector_variant(Name,"",Opts);
setup_extension(Name,Ext,Opts) ->
    case get(Name) of
	undefined ->
	    setup_vector_variant(Name,Ext,Opts);
	OrigF = #func{} ->
	    F = get(Name++Ext),
	    case is_equal(F,OrigF) of
		true ->
		    put(Name, OrigF#func{ext={ext,Ext}}),
		    skip;
		_ -> 
		    setup_vector_variant(Name,Ext,Opts)
	    end
    end.

setup_vector_variant(Name,Ext,Opts) ->
    case reverse(Name) of
	[$v|NoVec] -> %% Hmm might be a vector version
	    RealName = reverse(NoVec,Ext),
	    case get(RealName) of
		undefined -> 
		    setup_idx_binary(Name,Ext,Opts);
		Real = #func{} -> 
		    verify_args(Name,Ext,Real,RealName,Opts)
	    end;
	_ ->
	    setup_idx_binary(Name,Ext,Opts)
    end.

verify_args(Name,Ext,Real = #func{params=RAs},RealName,Opts) ->
    FuncName = Name ++ Ext,
    Vector = #func{params=Args} = get(FuncName),
    case is_vector(Name,Opts) of
	false ->
	    Check = fun(#arg{type=#type{name=Type}},Acc) ->
			    if Type =:= Acc -> Acc;
			       Acc =:= undefined -> Type;
			       true -> different
			    end
		    end,
	    case foldl(Check,undefined,RAs) of
		different ->
		    setup_idx_binary(Name,Ext,Opts);
		undefined ->
		    setup_idx_binary(Name,Ext,Opts);
		_ when length(Args) =/= 1 -> 
		    setup_idx_binary(Name,Ext,Opts);
		_Type ->
		    put(FuncName,Vector#func{where=erl,alt={vector,0,RealName}}),
		    put(RealName,Real#func{alt={has_vector,0,FuncName}}),
		    Name++Ext
	    end;
	VecPos ->
	    put(FuncName,Vector#func{where=erl,alt={vector,VecPos,RealName}}),
	    put(RealName,Real#func{alt={has_vector,VecPos,FuncName}}),
	    Name++Ext
    end.

is_vector(Name, Opts) ->
    Vecs = get_value(vector, Opts, []),
    lookup(Name, Vecs, false).

lookup(Name,[{Vector, VecPos}|R],Def) when is_list(Vector) ->
    case lists:prefix(Vector,Name) of
	true ->
	    %%	    VecPos;
	    %%io:format("~s ~s => ~p ~n", [Vector,Name,VecPos]),
 	    case Vector == Name of
		true -> 
 		    VecPos;
 		false -> %% Look for exactly the correct Name
 		    case lookup(Name,R,Def) of
 			Def -> VecPos;
 			Other -> Other
 		    end
 	    end;
	false -> lookup(Name,R, Def)
    end;
lookup(Name,[_|R],Def) ->
    lookup(Name,R,Def);
lookup(_,[], Def) -> Def.
    
setup_idx_binary(Name,Ext, Opts) ->
    FuncName = Name ++ Ext,
    Func = #func{params=Args} = get(FuncName),
    Id = next_id(function),

    %% Ok warn if single is undefined
    lists:foreach(fun(#arg{type=#type{base=memory}}) -> ok;
		     (#arg{type=#type{base=string}}) -> ok;
		     (#arg{type=#type{base=idx_binary}}) -> ok;
		     (#arg{type=#type{name="GLUquadric"}}) -> ok;
		     (#arg{type=#type{base=binary, size=Sz}}) when Sz =/= undefined -> ok;
		     (A=#arg{type=#type{single=undefined}}) -> 
			  ?warning("~p Unknown size of~n ~p~n",
				   [get(current_func),A]),
			  io:format("{~p, {~p, }}.~n",
				    [get(current_func),A#arg.name]),
			  ok;
		     (_) -> ok
		  end, Args),
    case setup_idx_binary_1(Args, []) of
	ignore -> 
	    put(FuncName, Func#func{id=Id}),
	    Name++Ext;
	{bin, A1,A2} ->
	    put(FuncName, Func#func{id=Id,params=A1}),
	    Extra = FuncName++"Bin",
	    put(Extra, Func#func{params=A2, id=next_id(function)}),
	    [FuncName,Extra];
	{matrix, A1,A2} ->
	    put(FuncName, Func#func{id=Id,params=A2}),
	    Extra = FuncName++"Matrix",
	    put(Extra, Func#func{where=erl, params=A1, id=Id}),
	    [FuncName,Extra]
    end.

setup_idx_binary_1([A=#arg{in=true,type=T=#type{base=idx_binary}}|R], Acc) ->
    A1 = A#arg{type=T#type{base=guard_int,size=4}},
    A2 = A#arg{type=T#type{base=binary}},
    Head = reverse(Acc),
    case setup_idx_binary_1(R, []) of
	ignore -> 
	    {bin, Head ++ [A1|R], Head ++ [A2|R]};
	{bin, R1,R2} ->
	    {bin, Head ++ [A1|R1], Head ++ [A2|R2]}
    end;
setup_idx_binary_1([A=#arg{in=true,type=T=#type{base=int,size=4},alt=list_binary}|R], Acc) ->
    A1 = A#arg{type=T#type{base=guard_int}},
    A2 = A#arg{type=T#type{base=binary}},
    Head = reverse(Acc),
    case setup_idx_binary_1(R, []) of
	ignore ->
	    {bin, Head ++ [A1|R], Head ++ [A2|R]};
	{bin, R1,R2} ->
	    {bin, Head ++ [A1|R1], Head ++ [A2|R2]}
    end;

setup_idx_binary_1([A=#arg{in=true,type=T=#type{single={tuple,matrix}}}|R], Acc) ->
    A1 = A#arg{type=T#type{single={tuple, matrix12}}},
    A2 = A#arg{type=T#type{single={tuple, 16}}},
    Head = reverse(Acc),
    case setup_idx_binary_1(R, []) of
	ignore -> 
	    {matrix, Head ++ [A1|R], Head ++ [A2|R]};
	{matrix, R1,R2} ->
	    {matrix, Head ++ [A1|R1], Head ++ [A2|R2]}
    end;
setup_idx_binary_1([H|R],Acc) ->
    setup_idx_binary_1(R,[H|Acc]);
setup_idx_binary_1([],_) -> ignore.

is_equal(F1=#func{type=T1,params=A1},F2=#func{type=T2,params=A2}) ->
    Equal = is_equal_type(T1,T2) andalso is_equal_args(A1,A2),
    case Equal of
	true -> ok;
	false ->
	    %% io:format("A1: ~p~nA2: ~p~n",[A1,A2]),	    
	    ?warning("Keeping Ext Not Equal ~p ~p~n",
		     [F1#func.name,F2#func.name])
    end,
    Equal.

is_equal_args([],[]) -> true;
is_equal_args([_A1=#arg{type=T1}|A1s],[_A2=#arg{type=T2}|A2s]) -> 
    case is_equal_type(T1,T2) of
	true -> is_equal_args(A1s,A2s);
	false ->
	    %%io:format("Diff~n ~p~n ~p ~n~n", [_A1,_A2]),
	    false
    end.

is_equal_type(T,T) -> true;
is_equal_type(#type{name="GLcharARB"},#type{name="GLchar"}) -> true;
%%is_equal_type(#type{name="GLhandleARB"},#type{name="GLuint"}) -> true;
is_equal_type(#type{name="GLenum"},#type{name="GLuint"}) -> true;
is_equal_type(#type{name="GLenum"},#type{name="GLint"}) -> true;
is_equal_type(#type{base=idx_binary},#type{base=guard_int}) -> true;
is_equal_type(#type{base=idx_binary},#type{base=memory}) -> true;
is_equal_type(#type{single={tuple,matrix}},#type{single={tuple,matrix12}}) -> true;
is_equal_type(#type{base=B,single=S,name=N,size=Sz},
	      #type{base=B,single=S,name=N,size=Sz}) -> true;
is_equal_type(_,_) -> false.

skip(Name,Opts) ->
    Skip = get_value(skip, Opts, []),
    lists:any(fun(Prefix) -> lists:prefix(Prefix,Name) end, Skip).

keep(Name,Opts) ->
    Skip = get_value(keep, Opts, []),
    lists:any(fun(Prefix) -> lists:prefix(Prefix,Name) end, Skip).

get_extension(ExtName,_Opts) ->
    case reverse(ExtName) of
	"BRA"  ++ Name -> {reverse(Name),"ARB"};
	"TXE"  ++ Name -> {reverse(Name),"EXT"};
	"ASEM" ++ Name -> {reverse(Name),"MESA"};
	"ITA"  ++ Name -> {reverse(Name),"ATI"};
	"DMA"  ++ Name -> {reverse(Name),"AMD"};
	"VN"   ++ Name -> {reverse(Name),"NV"}; %Nvidia
	"ELPPA"++ Name -> {reverse(Name),"APPLE"};
	"LETNI"++ Name -> {reverse(Name),"INTEL"};
	"NUS"  ++ Name -> {reverse(Name),"SUN"};
	"XNUS" ++ Name -> {reverse(Name),"SUNX"};
	"IGS"  ++ Name -> {reverse(Name),"SGI"};
	"SIGS" ++ Name -> {reverse(Name),"SGIS"};
	"XIGS" ++ Name -> {reverse(Name),"SGIX"};
	"XFD3" ++ Name -> {reverse(Name),"3DFX"};
	"MBI"  ++ Name -> {reverse(Name),"IBM"};
	"RGNI" ++ Name -> {reverse(Name),"INGR"};
	"IGP"  ++ Name -> {reverse(Name),"PGI"};
	"PH"   ++ Name -> {reverse(Name),"HP"};
	"YDEMERG" ++ Name -> {reverse(Name),"GREMEDY"};
	"SEO" ++ Name -> {reverse(Name),"OES"};
	%%["" ++ Name] ->     {Name;  %%
	_ -> {ExtName, ""}
    end.
		    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

drop_empty(List) ->
    filter(fun(#xmlText { value = Text}) ->		   
		   string:strip(Text) =/= "";
	      (_)->
		   true
	   end, List).

name(Name, _Opts) -> Name.

next_id(What) ->
    Next = case get(What) of
	       undefined -> 5010;  %% Opengl 
	       N -> N+1
	   end,
    put(What, Next),
    Next.
