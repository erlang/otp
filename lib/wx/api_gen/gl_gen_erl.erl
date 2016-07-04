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
		   open_write/1, open_write/2, close/0, erl_copyright/0, w/2,
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
gen_define(#def{name=N, val=Val, type=float_str}) ->
    w("-define(~s, ~s).~n", [N,Val]);
gen_define(#def{name=N, val=Val, type=hex}) ->
    w("-define(~s, 16#~s).~n", [N,Val]);
gen_define(#def{name=N, val=Val, type=string}) ->
    w("-define(~s, ?~s).~n", [N,Val]);
gen_define(#def{name="GLEXT_64_TYPES"++_, val=undefined, type=undefined}) ->
    ok.

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
    open_write("../src/gen/gl.erl", [{encoding,utf8}]),
    erl_copyright(),
    w("~n%% OPENGL API~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc  Standard OpenGL api.~n", []),
    w("%% See <a href=\"http://www.opengl.org/sdk/docs/man/\">www.opengl.org</a>~n",[]),
    w("%%~n", []),
    w("%% Booleans are represented by integers 0 and 1.~n~n", []),

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
    open_write("../src/gen/glu.erl", [{encoding,utf8}]),
    erl_copyright(),
    w("~n%% OPENGL UTILITY API~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc  A part of the standard OpenGL Utility api.~n", []),
    w("%% See <a href=\"http://www.opengl.org/sdk/docs/man/\">www.opengl.org</a>~n",[]),
    w("%%~n", []),
    w("%% Booleans are represented by integers 0 and 1.~n~n", []),

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

    %% w("%% @spec (Vec3, [Vec3]) -> {Triangles, VertexPos}~n",[]),
    %% w("%%  Vec3 = {float(),float(),float()}~n",[]),
    %% w("%%  Triangles = [VertexIndex::integer()]~n",[]),
    %% w("%%  VertexPos  = binary()~n",[]),
    w("%% @doc General purpose polygon triangulation.~n",[]),
    w("%% The first argument is the normal and the second a list of~n"
      "%% vertex positions. Returned is a list of indecies of the vertices~n"
      "%% and a binary (64bit native float) containing an array of~n"
      "%% vertex positions, it starts with the vertices in Vs and~n"
      "%% may contain newly created vertices in the end.~n", []),

    w("-spec tesselate(Normal, [Vs]) -> {Triangles, VertexPos}~n", []),
    w("                  when Normal :: vertex(), Vs :: vertex(),~n", []),
    w("                  Triangles :: [integer()], VertexPos :: binary().~n", []),
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
	glu ->
	    w("-type vertex() :: {float(), float(), float()}.~n", []),
	    w("-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl or glu.hrl~n", []);
	gl ->
	    w("-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl~n", []),
	    w("-type clamp() :: float().    %% 0.0..1.0~n", []),
	    w("-type offset() :: non_neg_integer(). %% Offset in memory block~n", [])
    end,
    w("-type matrix12() :: {float(),float(),float(),float(),~n", []),
    w("                   float(),float(),float(),float(),~n", []),
    w("                   float(),float(),float(),float()}.~n", []),
    w("-type matrix16() :: {float(),float(),float(),float(),~n", []),
    w("                   float(),float(),float(),float(),~n", []),
    w("                   float(),float(),float(),float(),~n", []),
    w("                   float(),float(),float(),float()}.~n", []),
    w("-type matrix() :: matrix12() | matrix16().~n", []),
    w("-type mem() :: binary() | tuple().   %% Memory block~n", []),
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

gen_export2(#func{name=Name,alt=Alt={vector,VecPos,Vec}}) ->
    #func{params=As0} = get(Vec),
    {As1,_As2} = lists:split(VecPos, As0),
    Args = lists:filter(fun(Arg) -> func_arg(Arg) =/= skip end, As1),
    Export = erl_func_name(Name) ++ "/" ++ integer_to_list(length(Args) +1),
    DocN = doc_name(Name,Alt),
    (get({export_arg,DocN}) == undefined) andalso put({export_arg, DocN}, Export),
    Export;
gen_export2(#func{name=Name,params=As0, alt=Alt}) ->
    Args = lists:filter(fun(Arg) -> func_arg(Arg) =/= skip end, As0),
    Export = erl_func_name(Name) ++ "/" ++ integer_to_list(length(Args)),
    DocN = doc_name(Name,Alt),
    (get({export_arg,DocN}) == undefined) andalso put({export_arg, DocN}, Export),
    Export.

gen_doc([#func{name=Name, params=Orig, alt={vector,VecPos,Vec}}]) ->
    #func{type=T,params=As} = get(Vec),
    {As1,As2} = lists:split(VecPos, As),
    #arg{name=OrigName} = lists:last(Orig),
    Args1 = case args(fun func_arg/1, ",", As1) of [] -> []; Else -> Else++"," end,
    Args2 = args(fun func_arg/1, ",", As2),
    w("%% @equiv ~s(~s)~n",[erl_func_name(Vec), Args1++Args2]),
    SA1 = case doc_arg_types(As1) of [] -> []; E -> E++"," end,
    SA2 = doc_arg_types(As2),
    w("-spec ~s(~s~s) -> ~s when ~s :: {~s}.~n",
      [erl_func_name(Name), SA1, erl_arg_name(OrigName),
       doc_return_types(T,As), erl_arg_name(OrigName), SA2]);

gen_doc([F=#func{name=Name,type=T,params=As, alt=Alt}|_]) ->
    gen_doc(Name, Alt, gen_export2(F)),
    Ps = [Arg || #arg{name=Arg, in=In, where=Where} <- As,
		 In =/= false, Where =/= c],
    Args = args(fun erl_arg_name/1, ", ", Ps),
    case Args of
	[] ->
	    w("-spec ~s(~s) -> ~s.~n",
	      [erl_func_name(Name), Args, doc_return_types(T,As)]);
	_  -> w("-spec ~s(~s) -> ~s when ~s.~n",
		[erl_func_name(Name), Args, doc_return_types(T,As), doc_arg_types(As)])
    end.

-define(LINE_LEN, 90).

gen_doc(Name0, Alt, Export) ->
    Name = doc_name(Name0, Alt),
    case get({doc, Name}) of
	undefined ->
	    GLDoc = "http://www.opengl.org/sdk/docs/man/xhtml/",
	    case parse_doc(Name, _Dir1 ="gl_man4", _Dir2="gl_man2") of
		{error, _} ->
		    case reverse(Name) of
			"BRA" ++ _ -> ok;
			"TXE" ++ _ -> ok;
			_ ->
			    %% io:format("Missing doc: no ~s.xml (~s) found in ~s or ~s~n",
			    %% 	      [Name, Name0, Dir1, Dir2]),
			    ok
		    end,
		    w("%% @doc ~s~n%%~n%% See <a href=\"~s~s.xml\">external</a> documentation.~n",
		      [Name, GLDoc, Name]);
		Doc ->
		    put({doc, Name}, Export),
		    format_doc(Doc, ?LINE_LEN),
		    w("~n%%~n%% See <a href=\"~s~s.xml\">external</a> documentation.~n",
		      [GLDoc, Name])
	    end;
	Where ->
	    w("%% @doc ~n", []),
	    w("%% See {@link ~s}~n", [Where])
    end.

parse_doc(Name, Dir1, Dir2) ->
    case gl_scan_doc:file(filename:join(Dir1, Name++".xml"), []) of
	{error, {_, "no such" ++ _}} ->
	    gl_scan_doc:file(filename:join(Dir2, Name++".xml"), []);
	Doc ->
	    Doc
    end.

format_doc(Strs, Count) when Count < 0 ->
    w("~n%% ", []),
    format_doc(Strs, ?LINE_LEN);
format_doc([{constant, Const}|Rest], Count) ->
    w("`?~s'", [Const]),
    format_doc(Rest, Count-length(Const)-8);
format_doc([{emphasis, Const}|Rest], Count) ->
    w("`~ts'", [Const]),
    format_doc(Rest, Count-length(Const)-7);
format_doc([{function, Func}|Rest], Count) ->
    case Func of
	"glu" ++ _ ->
	    w("``glu:~s''", [erl_func_name(Func)]);
	"gl" ++ _ ->
	    w("``gl:~s''", [erl_func_name(Func)]);
	_ ->
	    w("`~s'", [Func])
    end,
    format_doc(Rest, Count-length(Func)-7);
format_doc([{reffunc, Func}|Rest], Count) ->
    Out = fun(Export) ->
		  case Func of
		      "glu" ++ _ -> w(" {@link glu:~s} ", [Export]);
		      "gl" ++ _ ->  w(" {@link gl:~s} ", [Export])
		  end
	  end,
    case get({export_arg, Func}) of
	undefined ->
	    case get({export_arg, doc_name(Func, undefined)}) of
		undefined ->
		    %% io:format("Func ~p undefined (~p) ~n",
		    %% 	      [Func, doc_name(Func, undef)]),
		    w("see `~s'", [Func]);
		Export -> Out(Export)
	    end;
	Export ->
	    Out(Export)
    end,
    format_doc(Rest, Count-length(Func)-10);
format_doc([{parameter, Param}|Rest], Count) ->
    w(" `~s' ", [erl_arg_name(Param)]),
    format_doc(Rest, Count-length(Param)-7);
format_doc([{equation, Eq}|Rest], Count) ->
%%    w("```", []),
    format_doc([Eq], Count),
%%    w("'''", []),
    format_doc(Rest, Count);
format_doc([{fenced, Open, Close, Eq}|Rest], Count) ->
    w(Open, []),
    format_doc(Eq, Count),
    w(Close, []),
    format_doc(Rest, Count);

format_doc([{code, Code}|Rest], Count) ->
    w("``~ts''", [Code]),
    format_doc(Rest, Count-length(Code)-7);

format_doc([para|Rest], _Count) ->
    w("~n%%~n%% ", []),
    format_doc(Rest, ?LINE_LEN);
format_doc([break|Rest], _Count) ->
    w("<br />~n%% ", []),
    format_doc(Rest, ?LINE_LEN);
format_doc([{purpose, Purpose}, para | Doc], _Count) ->
    w("%% @doc ~ts~n%%~n%% ", [uppercase(Purpose)]),
    format_doc(Doc, ?LINE_LEN);
format_doc([{purpose, Purpose} | Doc], _Count) ->
    w("%% @doc ~ts~n%%~n%% ", [Purpose]),
    format_doc(Doc, ?LINE_LEN);
format_doc([listentry|Rest], _Count) ->
    w("~n%%~n%% ", []),
    format_doc(Rest, ?LINE_LEN);
format_doc([Str|Rest], Count) ->
    case length(Str) of
	Len when Len < Count ->
	    w("~ts", [Str]),
	    format_doc(Rest, Count-Len);
	_ ->
	    {Str1, Str2} = split(Str, Count, []),
	    w("~ts~n%% ", [Str1]),
	    format_doc([Str2|Rest], ?LINE_LEN)
    end;
format_doc([], _) -> ok.

split([$  |Str], Count, Acc) when Count =< 5 ->
    {reverse(Acc), Str};
split([Chr|Str], Count, Acc) ->
    split(Str, Count-1, [Chr|Acc]);
split([], _, Acc) ->
    {reverse(Acc), []}.

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

doc_arg_types(Ps0) ->
    Ps = [P || P=#arg{in=In, where=Where} <- Ps0,In =/= false, Where =/= c],
    args(fun(Arg) -> doc_arg_type(Arg) end, ",", Ps).

doc_return_types(T, Ps0) ->
    Ps = [P || P=#arg{in=In, where=Where} <- Ps0,In =/= true, Where =/= c],
    doc_return_types2(T, Ps).

doc_return_types2(void, []) ->    "'ok'";
doc_return_types2(void, [#arg{type=T}]) ->  doc_arg_type2(T);
doc_return_types2(T, []) ->              doc_arg_type2(T);
doc_return_types2(void, Ps) ->
    "{" ++ args(fun(Arg) -> doc_arg_type(Arg) end,",",Ps) ++ "}";
doc_return_types2(T, Ps) ->
    "{" ++ doc_arg_type2(T) ++ "," ++
	args(fun(Arg) -> doc_arg_type(Arg) end,",",Ps) ++ "}".

doc_arg_type(#arg{name=Name,type=T}) ->
    try
	erl_arg_name(Name) ++ " :: " ++ doc_arg_type2(T)
    catch _:Error ->
	    io:format("Error spec: ~p ~p~n~p~n",[Name, Error, erlang:get_stacktrace()]),
	    exit(error)
    end.

doc_arg_type2(T=#type{single=true}) ->
    doc_arg_type3(T);
doc_arg_type2(T=#type{single=undefined}) ->
    doc_arg_type3(T);
doc_arg_type2(_T=#type{single={tuple,undefined}}) ->
    "tuple()";
doc_arg_type2(#type{base=float, single={tuple,16}}) ->
    "matrix()";
doc_arg_type2(#type{base=string, single=list}) ->
    "iolist()";
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

doc_name(N="glGetBufferParameteriv", _) -> N;
doc_name("glEnd"++What, _) -> "glBegin"++What;
doc_name("glDisable" ++ What, _) -> "glEnable" ++ What;
doc_name("glPop" ++ What, _) -> "glPush" ++ What;
doc_name("glGetBooleanv", _) -> "glGet";
doc_name("glGetBooleani_v", _) -> "glGet";
doc_name("glGetIntegerv", _) -> "glGet";
doc_name("glGetIntegeri_v", _) -> "glGet";
doc_name("glGetInteger64v", _) -> "glGet";
doc_name("glGetInteger64i_v", _) -> "glGet";
doc_name("glGetFloatv", _) -> "glGet";
doc_name("glGetDoublev", _) -> "glGet";
doc_name("glGetFloati_v", _) -> "glGet";
doc_name("glGetDoublei_v", _) -> "glGet";
doc_name("glUniformMatr" ++ _, _) -> "glUniform";
doc_name("glTexSubImage" ++ _, _) -> "glTexSubImage";
doc_name("glFramebufferText" ++ _, _) -> "glFramebufferTexture";
doc_name("glProgramUniformMatr" ++ _, _) -> "glProgramUniform";
doc_name(Name, {has_vector,_,_}) ->
    strip_hard(reverse(Name));
doc_name(Name, _) ->
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
strip([$v,$b,$I,N|R]) when N > 47, N < 58 -> R;
strip([$v,$i,$I,N|R]) when N > 47, N < 58 -> R;
strip([$v,$s,$I,N|R]) when N > 47, N < 58 -> R;
strip([$v,$d,$I,N|R]) when N > 47, N < 58 -> R;
strip([$v,$f,$I,N|R]) when N > 47, N < 58 -> R;
strip([$b,$u,$I,N|R]) when N > 47, N < 58 -> R;

strip([$v,$b,$u,N,$I|R]) when N > 47, N < 58 ->R;
strip([$v,$i,$u,N,$I|R]) when N > 47, N < 58 ->R;
strip([$v,$s,$u,N,$I|R]) when N > 47, N < 58 ->R;
strip([$v,$b,N,$I|R]) when N > 47, N < 58 -> R;
strip([$v,$i,N,$I|R]) when N > 47, N < 58 -> R;
strip([$v,$s,N,$I|R]) when N > 47, N < 58 -> R;
strip([$v,$d,N,$I|R]) when N > 47, N < 58 -> R;
strip([$v,$f,N,$I|R]) when N > 47, N < 58 -> R;

strip([$v,$b,$u,N|R]) when N > 47, N < 58 ->R;
strip([$v,$i,$u,N|R]) when N > 47, N < 58 ->R;
strip([$v,$s,$u,N|R]) when N > 47, N < 58 ->R;
strip([$v,$b,N|R]) when N > 47, N < 58 -> R;
strip([$v,$i,N|R]) when N > 47, N < 58 -> R;
strip([$v,$s,N|R]) when N > 47, N < 58 -> R;
strip([$v,$d,N|R]) when N > 47, N < 58 -> R;
strip([$v,$f,N|R]) when N > 47, N < 58 -> R;

strip([$b,$u,N,$I|R]) when N > 47, N < 58 ->R;
strip([$i,$u,N,$I|R]) when N > 47, N < 58 ->R;
strip([$s,$u,N,$I|R]) when N > 47, N < 58 ->R;
strip([$b,N,$I|R]) when N > 47, N < 58 -> R;
strip([$i,N,$I|R]) when N > 47, N < 58 -> R;
strip([$s,N,$I|R]) when N > 47, N < 58 -> R;
strip([$d,N,$I|R]) when N > 47, N < 58 -> R;
strip([$f,N,$I|R]) when N > 47, N < 58 -> R;

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
strip([$v,$i,$I|R])     -> R;
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

%% strip([$D,$3|R]) -> R;
%% strip([$D,$2|R]) -> R;
%% strip([$D,$1|R]) -> R;

strip([$I|R]) -> R;
strip([$L|R]) -> R;
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
