%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : gl_gen_erl.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 18 Apr 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(gl_gen_erl).

-include("gl_gen.hrl").

-compile(export_all).

-import(lists, [foldl/3,foldr/3,reverse/1, keysearch/3, map/2, filter/2, max/1]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1, uppercase_all/1,
		   open_write/1, close/0, erl_copyright/0, w/2, 
		   args/3, args/4, strip_name/2]).

gl_defines(Defs) ->
    open_write("../include/gl.hrl"),
    erl_copyright(),
    w("~n%% OPENGL DEFINITIONS~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    [gen_define(Def) || Def=#def{} <- Defs],
    close(), 
    ok.

glu_defines(Defs) ->
    open_write("../include/glu.hrl"),
    erl_copyright(),
    w("~n%% GLU DEFINITIONS~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    [gen_define(Def) || Def=#def{} <- Defs],
    close(),
    ok.   

gen_define(#def{name=N, val=Val, type=int}) ->
    w("-define(~s, ~p).~n", [N,Val]);
gen_define(#def{name=N, val=Val, type=hex}) ->
    w("-define(~s, ~.16#).~n", [N,Val]);
gen_define(#def{name=N, val=Val, type=string}) ->
    w("-define(~s, ?~s).~n", [N,Val]).

types() ->
    [{"GLenum",    "32/native-unsigned"},
     {"GLboolean", "8/native-unsigned"},
     {"GLbitfield","32/native-unsigned"}, %
     %%{"GLvoid",":void		"},%
     {"GLbyte",    "8/native-signed"},	  % 1-byte signed 
     {"GLshort",   "16/native-signed"},   % 2-byte signed 
     {"GLint",	   "32/native-signed"},   % 4-byte signed 
     {"GLubyte",   "8/native-unsigned"},  % 1-byte unsigned 
     {"GLushort",  "16/native-unsigned"}, % 2-byte unsigned 
     {"GLuint",	   "32/native-unsigned"}, % 4-byte unsigned 
     {"GLsizei",   "32/native-signed"},   % 4-byte signed 
     {"GLfloat",   "32/native-float"},    % single precision float 
     {"GLclampf",  "32/native-float"},    % single precision float in [0,1] 
     {"GLdouble",  "64/native-float"},    % double precision float 
     {"GLclampd",  "64/native-float"},    % double precision float in [0,1] 
     {"GLsizeiptr","64/native-unsigned"}, % 64 bits int, convert on c-side
     {"GLintptr",  "64/native-unsigned"}, % 64 bits int, convert on c-sidew
     {"GLUquadric", "64/native-unsigned"},% Handle 32bits aargh 64bits on mac64
     {"GLhandleARB","64/native-unsigned"},% Handle 32bits aargh 64bits on mac64
     
     {"GLsync",     "64/native-unsigned"}, % Pointer to record
     {"GLuint64",     "64/native-unsigned"},
     {"GLint64",     "64/native-signed"}
    ].

gl_api(Fs) ->
    open_write("../src/gen/gl.erl"),
    erl_copyright(),
    w("~n%% OPENGL API~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc  Standard OpenGL api.~n", []),
    w("%% See <a href=\"http://www.opengl.org/sdk/docs/man/\">www.opengl.org</a>~n",[]),
    w("%%~n", []),
    w("%% Booleans are represented by integers 0 and 1.~n~n", []),
    w("%% @type mem().    memory block~n", []),
    w("%% @type enum().   An integer defined in gl.hrl~n", []),    
    w("%% @type offset(). An integer which is an offset in an array~n", []),
    w("%% @type clamp().  A float clamped between 0.0 - 1.0~n", []),
    
    w("-module(gl).~n~n",[]),    
    w("-compile(inline).~n", []),
%%    w("-include(\"wxe.hrl\").~n", []),
    [w("-define(~s,~s).~n", [GL,Erl]) || {GL,Erl} <- types()],

    gen_types(gl),
   
    Exp = fun(F) -> gen_export(F) end,
    ExportList = lists:map(Exp,Fs),
    w("~n-export([~s]).~n~n", [args(fun(EF) -> EF end, ",", ExportList, 60)]),
    w("-export([call/2, cast/2, send_bin/1]).~n",[]),
    w("%% @hidden~n", []),
    w("call(Op, Args) ->~n", []),
    w("    Port = get(opengl_port), ~n", []),
    w("    _ = erlang:port_control(Port,Op,Args),~n", []),
    w("    rec().~n", []),
    w("    ~n", []),
    w("%% @hidden~n", []),
    w("cast(Op, Args) ->~n", []),
    w("    Port = get(opengl_port), ~n", []),
    w("    _ = erlang:port_control(Port,Op,Args),~n", []),
    w("    ok.~n", []),
    w("    ~n", []),
    w("%% @hidden~n", []),
    w("rec() ->~n", []),
    w("    receive ~n", []),
    w("        {'_egl_result_', Res} -> Res;~n", []),
    w("        {'_egl_error_',  Op, Res} -> error({error,Res,Op})~n", []),
    w("    end. ~n", []),
    w("~n", []),
    w("%% @hidden~n", []),
    w("send_bin(Bin) when is_binary(Bin) ->~n", []),
    w("    Port = get(opengl_port), ~n", []),
    w("    erlang:port_command(Port,Bin);~n", []),
    w("send_bin(Tuple) when is_tuple(Tuple) ->~n", []),
    w("    Port = get(opengl_port), ~n", []),
    w("    case element(2, Tuple) of~n", []),
    w("        Bin when is_binary(Bin) ->~n", []),
    w("            erlang:port_command(Port,Bin)~n", []),
    w("    end.~n", []),
    w("~n", []),
    
    w("~n%% API~n~n", []),
    [gen_funcs(F) || F <- Fs],
    close(), 
    ok.

glu_api(Fs) ->
    open_write("../src/gen/glu.erl"),
    erl_copyright(),
    w("~n%% OPENGL UTILITY API~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc  A part of the standard OpenGL Utility api.~n", []),
    w("%% See <a href=\"http://www.opengl.org/sdk/docs/man/\">www.opengl.org</a>~n",[]),
    w("%%~n", []),
    w("%% Booleans are represented by integers 0 and 1.~n~n", []),
    w("%% @type mem().    memory block~n", []),
    w("%% @type enum().   An integer defined in gl.hrl~n", []),    
    w("%% @type offset(). An integer which is an offset in an array~n", []),
    w("%% @type clamp().  A float clamped between 0.0 - 1.0~n~n", []),

    w("-module(glu).~n",[]),    
    w("-compile(inline).~n", []),
    %%w("-include(\"wxe.hrl\").~n", []),
    [w("-define(~s,~s).~n", [GL,Erl]) || {GL,Erl} <- types()],

    gen_types(glu),
    
    Exp = fun(F) -> gen_export(F) end,
    ExportList = ["tesselate/2" | lists:map(Exp,Fs)],
    w("~n-export([~s]).~n~n", [args(fun(EF) -> EF end, ",", ExportList, 60)]),
    w("-import(gl, [call/2,cast/2,send_bin/1]).", []),
    w("~n%% API~n~n", []),

    w("%% @spec (Vec3, [Vec3]) -> {Triangles, VertexPos}~n",[]),
    w("%%  Vec3 = {float(),float(),float()}~n",[]),
    w("%%  Triangles = [VertexIndex::integer()]~n",[]),
    w("%%  VertexPos  = binary()~n",[]),
    w("%% @doc General purpose polygon triangulation.~n",[]),
    w("%% The first argument is the normal and the second a list of~n"
      "%% vertex positions. Returned is a list of indecies of the vertices~n"
      "%% and a binary (64bit native float) containing an array of~n"
      "%% vertex positions, it starts with the vertices in Vs and~n"
      "%% may contain newly created vertices in the end.~n", []),

    w("tesselate({Nx,Ny,Nz}, Vs) ->~n",[]),
    w("  call(5000, <<(length(Vs)):32/native,0:32,~n"
      "    Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble,~n"
      "    (<< <<Vx:?GLdouble,Vy:?GLdouble,Vz:?GLdouble >>~n"
      "        || {Vx,Vy,Vz} <- Vs>>)/binary >>).~n~n", []),

    [gen_funcs(F) || F <- Fs],
    close(),
    ok.

gen_funcs([F]) when is_list(F) ->
    put(current_func,F),
    gen_func(get(F)),
    erase(current_func),
    w(".~n~n",[]);
gen_funcs(All=[F|Fs]) when is_list(F) ->
    put(current_func,F),
    gen_doc([get(A) || A <- All]),
    gen_func(get(F)),
    erase(current_func),
    w(";~n",[]),
    gen_funcs(Fs);
gen_funcs([]) ->
    w(".~n~n",[]);
gen_funcs(F) ->
    put(current_func,F),
    gen_doc([get(F)]),
    gen_func(get(F)),
    erase(current_func),
    w(".~n~n",[]).

gen_types(Where) ->    
    case Where of
	glu -> ignore;
	gl ->
	    w("-type clamp() :: float().~n", []),
	    w("-type offset() :: non_neg_integer().~n", [])
    end,
    w("-type enum() :: non_neg_integer().~n", []),
    w("-type mem() :: binary() | tuple().~n", []),
    ok.

gen_export(F) ->
    try gen_export_1(F)
    catch E:R ->
	    io:format("Crash ~p:~p in ~p ~n",[E,R, erlang:get_stacktrace()]),
	    io:format("Func = ~p~n  ~p", [F, get(F)])
    end.

gen_export_1([F|_]) when is_list(F) ->
    gen_export2(get(F));
gen_export_1(F) when is_list(F) ->
    gen_export2(get(F)).

gen_export2(#func{name=Name,alt={vector,VecPos,Vec}}) ->
    #func{params=As0} = get(Vec), 
    {As1,_As2} = lists:split(VecPos, As0),
    Args = lists:filter(fun(Arg) -> func_arg(Arg) =/= skip end, As1),
    erl_func_name(Name) ++ "/" ++ integer_to_list(length(Args) +1);
gen_export2(#func{name=Name,params=As0}) ->
    Args = lists:filter(fun(Arg) -> func_arg(Arg) =/= skip end, As0),
    erl_func_name(Name) ++ "/" ++ integer_to_list(length(Args)).

gen_doc([#func{name=Name, alt={vector,VecPos,Vec}}]) ->
    #func{type=T,params=As} = get(Vec), 
    {As1,As2} = lists:split(VecPos, As),
    Args1 = case args(fun func_arg/1, ",", As1) of [] -> []; Else -> Else++"," end,
    Args2 = args(fun func_arg/1, ",", As2),
    w("%% @spec (~s{~s}) -> ~s~n",[Args1,Args2,doc_return_types(T,As, doc)]), 
    w("%% @equiv ~s(~s)~n",[erl_func_name(Vec), Args1++Args2]),
    SA1 = case doc_arg_types(As1, spec) of [] -> []; E -> E++"," end,
    SA2 = doc_arg_types(As2, spec),
    w("-spec ~s(~s{~s}) -> ~s.~n",
      [erl_func_name(Name), SA1, SA2, 
       doc_return_types(T,As, spec)]);
    
gen_doc([#func{name=Name,type=T,params=As,alt=Alt}|_]) ->
    w("%% @spec (~s) -> ~s~n", [doc_arg_types(As, doc),doc_return_types(T,As, doc)]),
    GLDoc = "http://www.opengl.org/sdk/docs/man/xhtml/",
    w("%% @doc See <a href=\"~s~s.xml\">external</a> documentation.~n",
      [GLDoc, doc_name(Name,Alt)]),
    w("-spec ~s(~s) -> ~s.~n",
      [erl_func_name(Name), doc_arg_types(As, spec), doc_return_types(T,As, spec)]).


gen_func(#func{name=Name,alt={vector,VecPos,Vec}}) ->
    #func{params=As} = get(Vec), 
    {As1,As2} = lists:split(VecPos, As),
    Args1 = case args(fun func_arg/1, ",", As1) of [] -> []; Else -> Else++"," end,
    Args2 = args(fun func_arg/1, ",", As2),
        
    w("~s(~s{~s}) ->", [erl_func_name(Name),Args1,Args2]),
    w("  ~s(~s)",      [erl_func_name(Vec), Args1++Args2]);
gen_func(_F=#func{name=Name,type=T,params=As,id=MId}) ->
    Args = args(fun func_arg/1, ",", As),
    w("~s(~s)~s ", [erl_func_name(Name), Args, guard_test(As)]),
    w("->~n", []),
    PreAs  = pre_marshal(As),
    {StrArgs,_} = marshal_args(PreAs),
    case have_return_vals(T,As) of
	true ->
	    w("  call(~p, <<~s>>)", [MId, StrArgs]);
	false ->
	    w("  cast(~p, <<~s>>)", [MId, StrArgs])
    end.

func_arg(#arg{in=In,where=W,name=Name,type=Type}) 
  when In =/= false, W =/= c ->
    case Type of
	#type{single={tuple,TSz0}} when TSz0 =/= undefined ->
	    TSz = if is_integer(TSz0) -> TSz0;
		     TSz0 =:= matrix12 -> 12
		  end,
	    [NameId|_] = erl_arg_name(Name),
	    Names = [[NameId|integer_to_list(Id)] || Id <- lists:seq(1,TSz)],
	    "{" ++ args(fun(ElName) -> ElName end, ",", Names) ++ "}";
	_ ->
	    erl_arg_name(Name)
    end;
func_arg(_) -> skip.

doc_arg_types(Ps0, Type) ->
    Ps = [P || P=#arg{in=In, where=Where} <- Ps0,In =/= false, Where =/= c],
    args(fun(Arg) -> doc_arg_type(Arg, Type) end, ",", Ps).

doc_return_types(T, Ps0, Type) ->
    Ps = [P || P=#arg{in=In, where=Where} <- Ps0,In =/= true, Where =/= c],
    doc_return_types2(T, Ps, Type).

doc_return_types2(void, [], _) ->    "ok";
doc_return_types2(void, [#arg{type=T}], _) ->     doc_arg_type2(T);
doc_return_types2(T, [], _) ->                      doc_arg_type2(T);
doc_return_types2(void, Ps, Type) -> 
    "{" ++ args(fun(Arg) -> doc_arg_type(Arg, Type) end,",",Ps) ++ "}";
doc_return_types2(T, Ps, Type) ->
    "{" ++ doc_arg_type2(T) ++ "," ++ 
	args(fun(Arg) -> doc_arg_type(Arg, Type) end,",",Ps) ++ "}".

doc_arg_type(#arg{name=Name,type=T}, doc) ->
    try  
	erl_arg_name(Name) ++ "::" ++ doc_arg_type2(T)
    catch _:Error ->
	    io:format("Error: ~p ~p~n~p~n",[Name, Error, erlang:get_stacktrace()]),
	    exit(error)
    end;
doc_arg_type(#arg{name=Name,type=T}, spec) ->
    try  
	doc_arg_type2(T)
    catch _:Error ->
	    io:format("Error spec: ~p ~p~n~p~n",[Name, Error, erlang:get_stacktrace()]),
	    exit(error)
    end.


doc_arg_type2(T=#type{single=true}) ->
    doc_arg_type3(T);
doc_arg_type2(T=#type{single=undefined}) ->
    doc_arg_type3(T);
doc_arg_type2(T=#type{single={tuple,undefined}}) ->
    "{" ++ doc_arg_type3(T) ++ "}";
doc_arg_type2(T=#type{single={tuple,Sz}}) ->
    "{" ++ args(fun doc_arg_type3/1, ",", lists:duplicate(Sz,T)) ++ "}";
doc_arg_type2(T=#type{single=list}) ->
    "[" ++ doc_arg_type3(T) ++ "]";
doc_arg_type2(T=#type{single={list, _Max}}) ->
    "[" ++ doc_arg_type3(T) ++ "]";
doc_arg_type2(T=#type{single={list,_,_}}) ->
    "[" ++ doc_arg_type3(T) ++ "]";
doc_arg_type2(T=#type{single={tuple_list,Sz}}) ->
    "[{" ++ args(fun doc_arg_type3/1, ",", lists:duplicate(Sz,T)) ++ "}]".

doc_arg_type3(#type{name="GLenum"}) ->  "enum()";
doc_arg_type3(#type{name="GLclamp"++_}) ->  "clamp()";
doc_arg_type3(#type{base=int}) ->       "integer()";
doc_arg_type3(#type{base=float}) ->     "float()";
doc_arg_type3(#type{base=guard_int}) -> "offset()|mem()";
doc_arg_type3(#type{base=string}) ->    "string()";
doc_arg_type3(#type{base=bool}) ->      "0|1";
doc_arg_type3(#type{base=binary}) ->    "binary()";
doc_arg_type3(#type{base=memory}) ->    "mem()".

guard_test(As) -> 
    Str = args(fun(#arg{name=N,type=#type{base=guard_int}}) -> 
		       " is_integer("++erl_arg_name(N)++")";   
		  (_) -> 
		       skip
	       end, ",", As),
    case Str of
	[] -> [];
	Other -> " when " ++ Other
    end.	    

pre_marshal([#arg{name=N,in=true,type=#type{base=binary}}|R]) -> 
    w("  send_bin(~s),~n", [erl_arg_name(N)]),
    pre_marshal(R);
pre_marshal([#arg{name=N,type=#type{base=memory}}|R]) -> 
    w("  send_bin(~s),~n", [erl_arg_name(N)]),
    pre_marshal(R);
pre_marshal([A=#arg{name=N,type=#type{base=string,single=list}}|R]) ->
    %% With null terminations
    w(" ~sTemp = list_to_binary([[Str|[0]] || Str <- ~s ]),~n",
      [erl_arg_name(N), erl_arg_name(N)]),
    [A|pre_marshal(R)];
pre_marshal([A|R]) ->
    [A|pre_marshal(R)];
pre_marshal([]) -> [].

marshal_args(As) ->
    marshal_args(As, [], 0).

marshal_args([#arg{where=erl}|Ps], Margs, Align) ->
    marshal_args(Ps, Margs, Align);
marshal_args([#arg{where=c}|Ps], Margs, Align) ->
    marshal_args(Ps, Margs, Align);
marshal_args([#arg{in=false}|Ps], Margs, Align) ->
    marshal_args(Ps, Margs, Align);
marshal_args([#arg{name=Name, type=Type}|Ps], Margs, Align0) ->
    {Arg,Align} = marshal_arg(Type,erl_arg_name(Name),Align0),
    marshal_args(Ps, [Arg|Margs], Align);
marshal_args([],Margs, Align) ->
    {args(fun(Str) -> Str end, ",", reverse(Margs)), Align}.

marshal_arg(#type{size=Sz,name=Type,single={tuple,undefined}},Name,A0) ->
    KeepA = case Sz of 8 -> "0:32,"; _ -> "" end,
    Str0 = "(size("++Name++")):?GLuint,"++KeepA++"\n"
	"      (<< <<C:?"++Type++ ">> ||"
	"C <- tuple_to_list("++Name++")>>)/binary",
    case Sz of
	4 -> 
	    {Str,Align} = align(4,A0,Str0),
	    {Str++",0:((("++integer_to_list(Align div 4)++
	     "+size("++Name++")) rem 2)*32)",0};
	8 -> 
	    align(8,A0,Str0)
    end;
marshal_arg(#type{size=BSz,name=Type,single={tuple,TSz}},Name,A0) 
  when is_integer(TSz) ->
    NameId = hd(Name),
    Names = [[NameId|integer_to_list(Id)] || Id <- lists:seq(1,TSz)],
    All = args(fun(ElName) -> ElName ++ ":?" ++ Type end, ",", Names),
    align(BSz,TSz,A0,All);

marshal_arg(#type{size=BSz,name=Type,single={tuple,matrix12}},Name,A0) ->
    NameId = hd(Name),    
    Ns0 = [[NameId|integer_to_list(Id)] || Id <- lists:seq(1,3)],
    Ns1 = [[NameId|integer_to_list(Id)] || Id <- lists:seq(4,6)],
    Ns2 = [[NameId|integer_to_list(Id)] || Id <- lists:seq(7,9)],
    Ns3 = [[NameId|integer_to_list(Id)] || Id <- lists:seq(10,12)],
    All = args(fun(ElName) -> ElName ++ ":?" ++ Type end, ",",
	       Ns0 ++ ["0"] ++ Ns1 ++ ["0"] ++ Ns2 ++ ["0"] ++ Ns3 ++ ["1"]),
    align(BSz,16,A0,All);

marshal_arg(#type{size=Sz,name=Type,base=Base,single=list},Name,A0) 
  when Base =:= float; Base =:= int ->
    KeepA = case Sz of 8 -> "0:32,"; _ -> "" end,
    Str0 = "(length("++Name++")):?GLuint,"++KeepA++"\n"
	"        (<< <<C:?"++Type++">> || C <- "++Name++">>)/binary",
    {Str,Align} = align(max([Sz,4]),A0,Str0),
    align_after(Sz,Align,0,1,Name,Str);

marshal_arg(#type{base=guard_int},Name,A0) ->
    align(4,A0,Name ++ ":?GLuint");

marshal_arg(#type{size=Sz,name=Type,single=true,
		  by_val=true,ref=undefined},Name,A0) ->
    align(Sz,A0,Name ++ ":?" ++ Type);

marshal_arg(#type{size=8,name="GLUquadric"=Type},Name,A0) ->
    align(8,A0,Name ++ ":?" ++ Type);

marshal_arg(#type{base=string,single=true,ref={pointer,1}},Name,A0) ->
    Str = "(list_to_binary(["++Name++"|[0]]))/binary", % Null terminate
    align_after(1,A0,1,1,Name,Str);

marshal_arg(#type{base=string,single=list,ref={pointer,2}},Name,A0) ->
    Str0 = 
	"(length("++Name++")):?GLuint,"
        "(size("++Name ++ "Temp)):?GLuint,"
	"(" ++ Name ++ "Temp)/binary",
    {Str,A} = align(4,A0,Str0),
    {Str ++ ",0:((8-((size("++Name++"Temp)+"++ 
     integer_to_list(A) ++") rem 8)) rem 8)", 0};
    
marshal_arg(#type{size=Sz,name=Type,single={tuple_list,TSz}},Name,A0) ->
    NameId = hd(Name),
    Names = [[NameId|integer_to_list(Id)] || Id <- lists:seq(1,TSz)],
    TTup = args(fun(ElName) -> ElName end, ",", Names),
    TBin = args(fun(ElName) -> ElName ++ ":?" ++ Type end, ",", Names),

    KeepA = case Sz of 8 -> "0:32,"; 4 -> "" end,
    Str0 = "(length("++Name++")):?GLuint,"++KeepA++"\n"
	"        (<< <<"++TBin++">> || {"++TTup++"} <- "++Name++">>)/binary",
    align(Sz,A0,Str0);

marshal_arg(T=#type{}, Name, Align) ->
    io:format("{\"~s\", {\"~s\", }}.~n", [get(current_func),lowercase(Name)]),
    %%?error({unhandled_type, {Name,T}}).
    w(" Don't know how to marshal this type ~p ~p~n", [T,Name]),
    align(8,Align,"").

% Make sure that it is aligned before adding it, and update alignment
align(Size, PreAlign, Str) ->
    align(Size,1,PreAlign,Str).

align(1,N,A,Str) ->                      {Str,          (A+1*N+0) rem 8};

align(2,N,A,Str) when (A rem 2) =:= 0 -> {Str,          (A+2*N+0) rem 8};
align(2,N,A,Str) when (A rem 2) =:= 1 -> {"0:8," ++Str, (A+2*N+1) rem 8};

align(4,N,A,Str) when (A rem 4) =:= 0 -> {Str,          (A+4*N+0) rem 8};
align(4,N,A,Str) when (A rem 4) =:= 1 -> {"0:24,"++Str, (A+4*N+3) rem 8};
align(4,N,A,Str) when (A rem 4) =:= 2 -> {"0:16,"++Str, (A+4*N+2) rem 8};
align(4,N,A,Str) when (A rem 4) =:= 3 -> {"0:8," ++Str, (A+4*N+1) rem 8};

align(8,_,0,Str) -> {Str,          0};
align(8,_,1,Str) -> {"0:56,"++Str, 0};
align(8,_,2,Str) -> {"0:48,"++Str, 0};
align(8,_,3,Str) -> {"0:40,"++Str, 0};
align(8,_,4,Str) -> {"0:32,"++Str, 0};
align(8,_,5,Str) -> {"0:24,"++Str, 0};
align(8,_,6,Str) -> {"0:16,"++Str, 0};
align(8,_,7,Str) -> {"0:8," ++Str, 0}.

align_after(8,0,_Add,_Multiplier,_Name,Str) -> {Str,0};
align_after(4,0,Add,Mult,Name,Str) -> 
    Extra = extra_align(Add,Mult),
    Align = ",0:(((length("++Name++")"++Extra++") rem 2)*32)",
    {Str ++ Align,0};
align_after(4,4,Add,Mult,Name,Str) -> 
    Extra = extra_align(Add,Mult),
    Align = ",0:(((1+length("++Name++")"++Extra++") rem 2)*32)",
    {Str ++ Align,0};
align_after(2,A,Add,Mult,Name,Str) when (A rem 2) =:= 0 ->
    Extra = extra_align(A+Add*2,Mult),
    Align = ",0:((8-((length("++Name++")*2"++Extra++") rem 8)) rem 8)",
    {Str ++ Align,0};
align_after(1,A,Add,Mult,Name,Str) ->
    Extra = extra_align(A+Add,Mult),
    Align = ",0:((8-((length("++Name++")"++Extra++") rem 8)) rem 8)",
    {Str ++ Align,0};
align_after(Sz,A,Add,Mult,Name,Str) ->
    io:format("~p ~p with ~p ~p ~s~n, ~p", [Sz,A,Add,Mult,Name,Str]),
    ?error(align_error).

extra_align(0,1) -> "";
extra_align(0,M) when M > 1 -> "* " ++ integer_to_list(M);
extra_align(A,1) when A > 0 -> "+ " ++ integer_to_list(A);
extra_align(A,M) when A > 0,M>1 -> 
    "* " ++ integer_to_list(M) ++ "+ " ++ integer_to_list(A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

have_return_vals(void, Ps) ->
    lists:any(fun(#arg{in=In, type=#type{base=B}}) -> 
		      In =/= true orelse B =:= memory 
	      end, Ps);
have_return_vals(#type{}, _) -> true.

erl_func_name("glu" ++ Name) ->   check_name(lowercase(Name));
erl_func_name("gl" ++ Name) ->   check_name(lowercase(Name)).

erl_arg_name(Name) ->    uppercase(Name).

check_name("begin") -> "'begin'";
check_name("end") -> "'end'";
check_name(Other) -> Other.
    

doc_name(Name0, Alt) ->
    Name = doc_name2(Name0,Alt),
%%     case lists:member(lists:last(Name0), "uvbisdf987654312") of
%% 	true -> io:format("~s    ~s~n", [Name0,Name]);
%% 	false -> ignore
%%     end,
    Name.

doc_name2(N="glGetBufferParameteriv", _) -> N;
doc_name2(N="glEnd", _) -> N;
doc_name2(Name, {has_vector,_,_}) ->
    strip_hard(reverse(Name));
doc_name2(Name, _) ->
    reverse(strip(reverse(Name))).

strip_hard(Rev) ->
    case strip(Rev) of
	Rev ->   reverse(strip2(Rev));
	Other -> reverse(Other)
    end.

strip("BRA"++R) -> "BRA"++strip(R);
strip([$v,$b,$u,$N,N|R]) when N > 47, N < 58 ->R;
strip([$v,$i,$u,$N,N|R]) when N > 47, N < 58 ->R;
strip([$v,$s,$u,$N,N|R]) when N > 47, N < 58 ->R;
strip([$v,$b,$N,N|R]) when N > 47, N < 58 -> R;
strip([$v,$i,$N,N|R]) when N > 47, N < 58 -> R;
strip([$v,$s,$N,N|R]) when N > 47, N < 58 -> R;
strip([$v,$d,$N,N|R]) when N > 47, N < 58 -> R;
strip([$v,$f,$N,N|R]) when N > 47, N < 58 -> R;
strip([$b,$u,$N,N|R]) when N > 47, N < 58 -> R;

strip([$v,$b,$u,N|R]) when N > 47, N < 58 ->R;
strip([$v,$i,$u,N|R]) when N > 47, N < 58 ->R;
strip([$v,$s,$u,N|R]) when N > 47, N < 58 ->R;
strip([$v,$b,N|R]) when N > 47, N < 58 -> R;
strip([$v,$i,N|R]) when N > 47, N < 58 -> R;
strip([$v,$s,N|R]) when N > 47, N < 58 -> R;
strip([$v,$d,N|R]) when N > 47, N < 58 -> R;
strip([$v,$f,N|R]) when N > 47, N < 58 -> R;

strip([$b,$u,N|R]) when N > 47, N < 58 ->R;
strip([$i,$u,N|R]) when N > 47, N < 58 ->R;
strip([$s,$u,N|R]) when N > 47, N < 58 ->R;
strip([$b,N|R]) when N > 47, N < 58 -> R;
strip([$i,N|R]) when N > 47, N < 58 -> R;
strip([$s,N|R]) when N > 47, N < 58 -> R;
strip([$d,N|R]) when N > 47, N < 58 -> R;
strip([$f,N|R]) when N > 47, N < 58 -> R;

strip([$v,$b,$u|R])  -> R;
strip([$v,$i,$u|R])  -> R;
strip([$v,$s,$u|R])  -> R;
strip([$v,$b|R])     -> R;
strip([$v,$i|R])     -> R;
strip([$v,$s|R])     -> R;
strip([$v,$d|R])     -> R;
strip([$v,$f|R])     -> R;

strip(R = "delban" ++ _) -> R;
strip([$d,$e|R]) -> [$e|R];
strip([$f,$e|R]) -> [$e|R];
strip([$i,$e|R]) -> [$e|R];
strip([$d,$x|R]) -> [$x|R];
strip([$f,$x|R]) -> [$x|R];
strip([$d,$d|R]) -> [$d|R];
strip([$f,$d|R]) -> [$d|R];
strip([$i,$l|R]) -> [$l|R];
strip([$f,$l|R]) -> [$l|R];
strip([$i,$r|R]) -> [$r|R];
strip([$f,$r|R]) -> [$r|R];
strip([$i,$g|R]) -> [$g|R];
strip([$f,$g|R]) -> [$g|R];
strip([$i,$n|R]) -> [$n|R];
strip([$f,$n|R]) -> [$n|R];
strip([$d,$n|R]) -> [$n|R];

strip([$v,R]) -> R;
strip([N|R]) when N > 47, N < 58 -> R;
strip([_|R="tceRlg"]) -> R;
strip([_|R="thgiLlg"]) -> R;
strip(R) -> R.

strip2([$b,$u|R])  -> R;
strip2([$i,$u|R])  -> R;
strip2([$s,$u|R])  -> R;
strip2([$b|R])     -> R;
strip2([$i|R])     -> R;
strip2([$s|R])     -> R;
strip2([$d|R])     -> R;
strip2([$f|R])     -> R;
strip2(R) ->          R.

gen_debug(GL, GLU) ->
    open_write("../src/gen/gl_debug.hrl"),
    erl_copyright(),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("gldebug_table() ->~n[~n", []),
    [printd(F,gl)  || F <- GL],
    [printd(F,glu) || F <- GLU],
    w(" {-1, {mod, func, -1}}~n",[]),
    w("].~n~n", []),
    close().


printd([F|R],Mod) when is_list(F) ->
    printd(F,Mod),
    printd(R,Mod);
printd([],_) -> ok;
printd(F,Mod) ->
    case get(F) of
	#func{alt={vector,_VecPos,_Vec}} -> ok;
	#func{where=erl} -> ok;
	#func{id=Id, name=Method} ->
	    w(" {~p, {~s, ~s, 0}},~n", [Id, Mod, erl_func_name(Method)]);
	_Other ->  
	    io:format("F= ~p => ~p~n", [F, _Other])			
    end.
