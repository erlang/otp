%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-compile([export_all, nowarn_export_all]).

-import(lists, [foldl/3,foldr/3,reverse/1, keysearch/3, map/2, filter/2, max/1]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1, uppercase_all/1,
		   open_write/1, open_write/2, close/0, erl_copyright/0, w/2,
		   args/3, args/4, strip_name/2]).


-define(HTTP_TOP, "https://www.khronos.org/registry/OpenGL-Refpages/").
-define(IS_INT(N), N > 47, N < 58).

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

gl_api(Fs, _GluNifs) ->
    open_write("../src/gen/gl.erl", [{encoding,utf8}]),
    erl_copyright(),
    w("~n%% OPENGL API~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc  Standard OpenGL api.~n", []),
    w("%% See <a href=\""++ ?HTTP_TOP ++ "\">www.khronos.org</a>~n",[]),
    w("%%~n", []),
    w("%% Booleans are represented by integers 0 and 1.~n~n", []),

    w("-module(gl).~n~n",[]),
    w("-compile([{inline, get_interface/0}]).~n", []),

    gen_types(gl),

    Exp = fun(F) -> gen_export(F) end,
    ExportList = lists:map(Exp,Fs),

    w("-on_load(init_nif/0).~n",[]),
    w("~n-export([~s]).~n~n", [args(fun(EF) -> EF end, ",", ExportList, 60)]),
    w("-export([get_interface/0, rec/1, lookup_func/1]).\n",[]),
    w("-nifs([lookup_func_nif/1]).\n",[]),
    w("-define(nif_stub,nif_stub_error(?LINE)).~n", []),
    w("%% @hidden~n", []),
    w("nif_stub_error(Line) ->~n"
      "    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).\n\n",[]),
    w("%% @hidden~n", []),
    w("init_nif() ->~n", []),
    w("  Base = \"erl_gl\",\n"
      "  Priv = code:priv_dir(wx),\n"
      "  SrcTree = filename:join(Priv,erlang:system_info(system_architecture)),\n"
      "  NifFile = case filelib:is_dir(SrcTree) of\n"
      "                true -> filename:absname(filename:join(SrcTree, Base));\n"
      "                false -> filename:absname(filename:join(Priv, Base))\n"
      "            end,\n"
      "  erlang:load_nif(NifFile, 0).\n\n", []),

    w("%% @hidden~n", []),
    w("get_interface() ->~n", []),
    w("    wxe_util.  %% temporary~n~n", []),
    w("%% @hidden~n", []),
    w("rec(Op) ->~n", []),
    w("    receive~n", []),
    w("        {'_egl_result_', Res} -> Res;~n", []),
    w("        {'_egl_error_',  Op, Res} -> error({error,Res,Op});~n", []),
    w("        {'_egl_error_', Other, Res} ->~n ", []),
    w("            Err = io_lib:format(\"~~p in op: ~~p\", [Res, Other]),~n", []),
    w("            error_logger:error_report([{gl, error}, {message, lists:flatten(Err)}]),~n", []),
    w("            rec(Op)~n", []),
    w("    end.~n~n", []),
    w("lookup_func(functions) -> lookup_func_nif(1);\n",[]),
    w("lookup_func(function_names) -> lookup_func_nif(2).\n\n",[]),
    w("lookup_func_nif(_Func) -> ?nif_stub.\n\n",[]),
    w("~n", []),
    w("~n", []),

    w("~n%% API~n~n", []),
    _Nifs = [gen_funcs(F) || F <- Fs],
    close(),
    ok.

glu_api(Fs) ->
    open_write("../src/gen/glu.erl", [{encoding,utf8}]),
    erl_copyright(),
    w("~n%% OPENGL UTILITY API~n~n", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc  A part of the standard OpenGL Utility api.~n", []),
    w("%% See <a href=\""++ ?HTTP_TOP ++ "\">www.khronos.org</a>~n",[]),
    w("%%~n", []),
    w("%% Booleans are represented by integers 0 and 1.~n~n", []),

    w("-module(glu).~n",[]),
    w("-compile(inline).~n", []),

    gen_types(glu),

    Exp = fun(F) -> gen_export(F) end,
    ExportList = ["tesselate/2" | lists:map(Exp,Fs)],
    w("~n-export([~s]).~n~n", [args(fun(EF) -> EF end, ",", ExportList, 60)]),
    w("-import(gl, [get_interface/0, rec/1]).\n", []),
    w("~n%% API~n~n", []),

    w("%% @doc General purpose polygon triangulation.~n",[]),
    w("%% The first argument is the normal and the second a list of~n"
      "%% vertex positions. Returned is a list of indices of the vertices~n"
      "%% and a binary (64bit native float) containing an array of~n"
      "%% vertex positions, it starts with the vertices in Vs and~n"
      "%% may contain newly created vertices in the end.~n", []),

    w("-spec tesselate(Normal, [Vs]) -> {Triangles, VertexPos}~n", []),
    w("                  when Normal :: vertex(), Vs :: vertex(),~n", []),
    w("                  Triangles :: [integer()], VertexPos :: binary().~n", []),
    w("tesselate(Normal, Vs) ->~n",[]),
    w("  IF = get_interface(),\n"
      "  IF:queue_cmd(Normal,Vs,5009),~n"
      "  rec(5009).~n~n", []),

    Nifs = [gen_funcs(F) || F <- Fs],
    close(),
    [{"egluTesselate_nif", 2}|Nifs].

gen_funcs(All=[F|_Fs]) when is_list(F) ->
    put(current_func,F),
    gen_spec([get(A) || A <- All]),
    R = gen_func(get(F)),
    erase(current_func),
    R;
gen_funcs(F) ->
    put(current_func,F),
    gen_spec([get(F)]),
    R = gen_func(get(F)),
    erase(current_func),
    R.

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
    w("-type m12() :: {f(),f(),f(),f(),~n", []),
    w("                   f(),f(),f(),f(),~n", []),
    w("                   f(),f(),f(),f()}.~n", []),
    w("-type m16() :: {f(),f(),f(),f(),~n", []),
    w("                   f(),f(),f(),f(),~n", []),
    w("                   f(),f(),f(),f(),~n", []),
    w("                   f(),f(),f(),f()}.~n", []),
    w("-type matrix() :: m12() | m16().~n", []),
    w("-type mem() :: binary() | tuple().   %% Memory block~n", []),
    w("-type f() :: float().~n", []),
    w("-type i() :: integer().~n", []),
    ok.

gen_export(F) ->
    try gen_export_1(F)
    catch E:R:S ->
	    io:format("Crash ~p:~p in ~p ~n",[E,R,S]),
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
    (get({doc_ref,DocN}) == undefined) andalso put({doc_ref, DocN}, Export),
    Export;
gen_export2(#func{name=Name,params=As0, alt=Alt}) ->
    Args = lists:filter(fun(Arg) -> func_arg(Arg) =/= skip end, As0),
    Export = erl_func_name(Name) ++ "/" ++ integer_to_list(length(Args)),
    DocN = doc_name(Name,Alt),
    (get({export_doc,Name}) == undefined) andalso put({export_doc, Name}, {Export, DocN}),
    (get({doc_ref,DocN}) == undefined) andalso put({doc_ref, DocN}, Export),
    Export.

gen_spec([#func{name=Name, alt={vector,VecPos,Vec}}]) ->
    #func{type=T,params=As} = get(Vec),
    {As1,As2} = lists:split(VecPos, As),
    SA1 = case spec_arg_types(As1) of [] -> []; E -> E++", " end,
    SA2 = spec_arg_types(As2),
    w("-spec ~s(~s{~s}) -> ~s.~n", [erl_func_name(Name), SA1, SA2, spec_return_types(T,As)]);

gen_spec([#func{name=Name,type=T,params=As}|_]) ->
    Ps = [Arg || #arg{name=Arg, in=In, where=Where} <- As,
		 In =/= false, Where =/= c],
    SpecAs = spec_arg_types(As),
    Args = args(fun erl_arg_name/1, ", ", Ps),
    SpecAsLen = string:length(SpecAs),
    if SpecAsLen =:= 0 ->
	    w("-spec ~s(~s) -> ~s.~n",
	      [erl_func_name(Name), Args, spec_return_types(T,As)]);
       SpecAsLen < 80 ->
            w("-spec ~s(~s) -> ~s.~n",
              [erl_func_name(Name), SpecAs, spec_return_types(T,As)]);
       true ->
            w("-spec ~s(~s) -> ~s~n    when ~s.~n",
              [erl_func_name(Name), Args, spec_return_types(T,As), spec_arg_types(As)])
    end.

gen_func(#func{name=Name,alt={vector,VecPos,Vec}}) ->
    #func{params=As} = get(Vec),
    {As1,As2} = lists:split(VecPos, As),
    Args1 = case args(fun func_arg/1, ",", As1) of [] -> []; Else -> Else++"," end,
    Args2 = args(fun func_arg/1, ",", As2),

    w("~s(~s{~s}) ->", [erl_func_name(Name),Args1,Args2]),
    w("  ~s(~s).~n",      [erl_func_name(Vec), Args1++Args2]),
    ignore;
gen_func(_F=#func{id=Id, name=Name,type=T,params=As}) ->
    Args = args(fun func_arg/1, ",", As),
    Guards = guards(As),
    w("~s(~s) ~s ->~n", [erl_func_name(Name), Args, Guards]),
    w("  IF = get_interface(),~n",[]),
    PreAs = pre_marshal(As),
    NifAs0  = args(fun func_arg/1, ",", PreAs),
    NifAs = case NifAs0 of
                [] -> "";
                Str -> [Str, $,]
            end,
    case have_return_vals(T,As) of
        false ->
            w("  IF:queue_cmd(~s~w),~n  ok.~n~n", [NifAs,Id]);
        true ->
            w("  IF:queue_cmd(~s~w),~n  rec(~w).~n~n", [NifAs,Id,Id])
    end,
    ok.

guards(As) ->
    Gs = args(fun guard_arg/1, ",", As),
    case Gs of
        [] -> [];
        String -> ["when ", String]
    end.

guard_arg(#arg{in=In,where=W,name=Name,type=T})
  when In =/= false, W =/= c ->
    guard(T, erl_arg_name(Name));
guard_arg(_) -> skip.

guard(T=#type{single=true}, Name) ->
    guard_type(T, Name);
guard(T=#type{single=undefined}, Name) ->
    guard_type(T, Name);
guard(_T=#type{single={tuple,undefined}}, Name) ->
    "is_tuple(" ++ Name ++ ")";
guard(#type{base=float, single={tuple,16}}, Name) ->
    "tuple_size(" ++ Name ++ ") =:= 16; tuple_size(" ++ Name ++ ") =:= 12";
guard(_T=#type{single={tuple,Sz}}, Name) ->
    io_lib:format("tuple_size(~s) =:= ~w", [Name, Sz]);
guard(#type{base=guard_int, single=list}, Name) ->
    io_lib:format("is_list(~s) orelse is_tuple(~s) orelse is_binary(~s)", [Name,Name,Name]);
guard(#type{single=list}, Name) ->
    "is_list(" ++ Name ++ ")";
guard(_T=#type{single={list, _Max}}, Name) ->
    "is_list(" ++ Name ++ ")";
guard(_T=#type{single={list,_,_}}, Name) ->
    "is_list(" ++ Name ++ ")";
guard(_T=#type{single={tuple_list,_Sz}}, Name) ->
    "is_list(" ++ Name ++ ")".

guard_type(#type{name="GLenum"}, Name) ->  "is_integer(" ++ Name ++ ")";
guard_type(#type{name="GLclamp"++_}, Name) ->  "is_float(" ++ Name ++ ")";
guard_type(#type{base=int}, Name) ->       "is_integer(" ++ Name ++ ")";
guard_type(#type{base=float}, Name) ->     "is_float(" ++ Name ++ ")";
guard_type(#type{base=guard_int}, Name) ->
    io_lib:format("is_integer(~s) orelse is_tuple(~s) orelse is_binary(~s)", [Name,Name,Name]);
guard_type(#type{base=string}, Name) ->    "is_list(" ++ Name ++ ")";
guard_type(#type{base=bool}, Name) ->
    io_lib:format("(0 =:= ~s) orelse (1 =:= ~s)", [Name,Name]);
guard_type(#type{base=binary}, Name) ->    "is_binary(" ++ Name ++ ")";
guard_type(#type{base=memory}, Name) ->
    io_lib:format("is_tuple(~s) orelse is_binary(~s)", [Name,Name]).


func_arg(#arg{in=In,where=W,name=Name})
  when In =/= false, W =/= c ->
    erl_arg_name(Name);
func_arg(_) -> skip.

spec_arg_types(Ps0) ->
    Ps = [P || P=#arg{in=In, where=Where} <- Ps0,In =/= false, Where =/= c],
    args(fun(Arg) -> spec_arg_type(Arg) end, ", ", Ps).

spec_return_types(T, Ps0) ->
    Ps = [P || P=#arg{in=In, where=Where} <- Ps0,In =/= true, Where =/= c],
    spec_return_types2(T, Ps).

spec_return_types2(void, []) ->    "'ok'";
spec_return_types2(void, [#arg{type=T}]) ->  spec_arg_type2(T);
spec_return_types2(T, []) ->              spec_arg_type2(T);
spec_return_types2(void, Ps) ->
    "{" ++ args(fun(Arg) -> spec_arg_type(Arg) end,",",Ps) ++ "}";
spec_return_types2(T, Ps) ->
    "{" ++ spec_arg_type2(T) ++ "," ++
	args(fun(Arg) -> spec_arg_type(Arg) end,",",Ps) ++ "}".

spec_arg_type(#arg{name=Name,type=T}) ->
    try
	erl_arg_name(Name) ++ "::" ++ spec_arg_type2(T)
    catch _:Error:Stacktrace ->
	    io:format("Error spec: ~p ~p~n~p~n",[Name, Error, Stacktrace]),
	    exit(error)
    end.

spec_arg_type2(T=#type{single=true}) ->
    spec_arg_type3(T);
spec_arg_type2(T=#type{single=undefined}) ->
    spec_arg_type3(T);
spec_arg_type2(_T=#type{single={tuple,undefined}}) ->
    "tuple()";
spec_arg_type2(#type{base=float, single={tuple,16}}) ->
    "matrix()";
spec_arg_type2(#type{base=string, single=list}) ->
    "[unicode:chardata()]";
spec_arg_type2(T=#type{single={tuple,Sz}}) ->
    "{" ++ args(fun spec_arg_type3/1, ",", lists:duplicate(Sz,T)) ++ "}";
spec_arg_type2(#type{base=guard_int, single=list}) ->
    "[integer()]|mem()";
spec_arg_type2(T=#type{single=list}) ->
    "[" ++ spec_arg_type3(T) ++ "]";
spec_arg_type2(T=#type{single={list, _Max}}) ->
    "[" ++ spec_arg_type3(T) ++ "]";
spec_arg_type2(T=#type{single={list,_,_}}) ->
    "[" ++ spec_arg_type3(T) ++ "]";
spec_arg_type2(T=#type{single={list,_,_,_}}) ->
    "[" ++ spec_arg_type3(T) ++ "]";
spec_arg_type2(T=#type{single={tuple_list,Sz}}) ->
    "[{" ++ args(fun spec_arg_type3/1, ",", lists:duplicate(Sz,T)) ++ "}]".

spec_arg_type3(#type{name="GLenum"}) ->  "enum()";
spec_arg_type3(#type{name="GLclamp"++_}) -> "clamp()";
spec_arg_type3(#type{base=int}) ->       "i()";
spec_arg_type3(#type{base=float}) ->     "f()";
spec_arg_type3(#type{base=guard_int}) -> "offset()|mem()";
spec_arg_type3(#type{base=string}) ->    "string()";
spec_arg_type3(#type{base=bool}) ->      "0|1";
spec_arg_type3(#type{base=binary}) ->    "binary()";
spec_arg_type3(#type{base=memory}) ->    "mem()".

%% strings
pre_marshal([A0=#arg{name=LenName,where=c,alt={length,N}},
             A=#arg{name=N,type=#type{base=string,single=list}}|R]) ->
    %% With null terminations
    w("  ~sTemp = [unicode:characters_to_binary([Str|[0]]) || Str <- ~s ],~n",
      [erl_arg_name(N), erl_arg_name(N)]),
    w("  ~s = length(~s),~n",[erl_arg_name(LenName), erl_arg_name(N)]),
    [A0#arg{where=both},A#arg{name=N++"Temp"}|pre_marshal(R)];
pre_marshal([A=#arg{name=N,type=#type{base=string,single=true,ref={pointer,1}}}|R]) ->
    %% With null terminations
    w("  ~sBin = unicode:characters_to_binary([~s|[0]]),~n",[erl_arg_name(N), erl_arg_name(N)]),
    [A#arg{name=N++"Bin"}|pre_marshal(R)];
%% lists
pre_marshal([A0=#arg{name=LenName,where=c,alt={length,N}},
             A=#arg{name=N,type=#type{single=List}}|R])
  when List =:= list orelse element(1, List) =:= tuple_list ->
    w("  ~s = length(~s),~n",[erl_arg_name(LenName), erl_arg_name(N)]),
    [A0#arg{where=both},A#arg{name=N, alt={length,LenName}}|pre_marshal(R)];
pre_marshal([A0=#arg{name=LenName,where=c,alt={length,N}},A1,
             A=#arg{name=N,type=#type{single=List}}|R])
  when List =:= list orelse element(1, List) =:= tuple_list ->
    w("  ~s = length(~s),~n",[erl_arg_name(LenName), erl_arg_name(N)]),
    [A0#arg{where=both},A1,A#arg{name=N, alt={length,LenName}}|pre_marshal(R)];
pre_marshal([A|R]) ->
    [A|pre_marshal(R)];
pre_marshal([]) -> [].


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
doc_name("glEnableVertexArrayAttrib", _) -> "glEnableVertexAttribArray";
doc_name("glDisableVertexArrayAttrib", _) -> "glEnableVertexAttribArray";
doc_name("glEndList",_) -> "glNewList";
doc_name("glEnd"++What, _) -> "glBegin"++What;
doc_name("glDisablei", _) -> "glEnable";
doc_name("glDisable" ++ What, _) -> "glEnable" ++ What;
doc_name("glClearBuffer" ++ _, _) -> "glClearBuffer";
doc_name("glCompressedTextureSubImage" ++ End, _) -> "glCompressedTexSubImage" ++ End;
doc_name("glFramebufferText" ++ _, _) -> "glFramebufferTexture";
doc_name("glFramebufferParameteri" ++ _, _) -> "glFramebufferParameteri";
doc_name("glFlushMappedNamedBufferRange" ++ _, _) -> "glFlushMappedBufferRange";
doc_name(N="glGetActiveUniformsiv", _) -> N;
doc_name("glGetBoolean" ++ _, _) -> "glGet";
doc_name("glGetInteger" ++ _, _) -> "glGet";
doc_name("glGetFloat" ++ _, _) -> "glGet";
doc_name("glGetDouble" ++ _, _) -> "glGet";
doc_name("glGetBufferParameter" ++ _, _) -> "glGetBufferParameter";
doc_name("glGetInternalformat" ++ _, _) -> "glGetInternalFormat";
doc_name("glGetQueryObject" ++ _, _) -> "glGetQueryObject";
doc_name("glGetQueryBuffer" ++ _, _) -> "glGetQueryObject";
doc_name("glGetQueryIndexediv" ++ _, _) -> "glGetQueryIndexed";
doc_name(N="glGetQuery" ++ _, _) -> N;
doc_name("glGetVertexAttrib" ++ _, _) -> "glGetVertexAttrib";
doc_name("glGenerateTextureMipmap",_) -> "glGenerateMipmap";
doc_name("glMap" ++ [I,_], _) -> "glMap" ++ [I];
doc_name("glMemoryBarrier" ++ _, _) -> "glMemoryBarrier";
doc_name("glUniformMatr" ++ _, _) -> "glUniform";
%%doc_name("glTexSubImage" ++ [I,, _) -> "glTexSubImage";
doc_name("glPop" ++ What, _) -> "glPush" ++ What;
doc_name("glProgramUniformMatr" ++ _, _) -> "glProgramUniform";
doc_name("glVertexArrayVertexBuffer" ++ S, _) -> "glBindVertexBuffer" ++ S;
doc_name(N="glVertexArrayElementBuffer", _) -> N;
doc_name("glVertexArray" ++ Rest, Alt) -> doc_name("glVertex"++Rest, Alt);
doc_name("glVertexAttrib" ++ [_|"Format"], _) -> "glVertexAttribFormat";
doc_name("glVertexAttrib" ++ [_|"Pointer"], _) -> "glVertexAttribFormat";
doc_name("glVertexAttribL" ++ _, _) -> "glVertexAttrib";
doc_name("glTexture" ++ Rest, _) ->
    case Rest of
        "Barrier" -> "glTextureBarrier";
        "View" -> "glTextureView";
         _ -> "glTex" ++ Rest
    end;
doc_name(Name, Alt) ->
    WithoutLast = ["glBlendFunc", "glColorMask", "glClearDepth",
                   "glCreateShaderProgram", "glDepthRangeArray",
                   "glEdgeFlag", "glIsEnabled", "glTexEnv",
                   "glViewportArray", "glScissorArray", "glScissorIndexed"
                  ],
    [Last|RevName] = lists:reverse(Name),
    case lists:member(Last, [$i,$f,$v]) of
        true ->
            New = lists:reverse(RevName),
            case lists:member(New, WithoutLast) of
                true -> New;
                false -> doc_name2(Name,Alt)
            end;
        false ->
            doc_name2(Name,Alt)
    end.

doc_name2(Name, Alt) ->
    doc_name_last(Name,Alt).

doc_name_last(Name, {has_vector,_,_}) ->
    strip_hard(reverse(Name));
doc_name_last(Name, _) ->
    reverse(strip(reverse(Name))).

strip_hard(Rev) ->
    case strip(Rev) of
	Rev ->   reverse(strip2(Rev));
	Other -> reverse(Other)
    end.

strip("BRA"++R) -> "BRA"++strip(R);
strip([$v,$b,$u,$N,N|R]) when ?IS_INT(N) ->R;
strip([$v,$i,$u,$N,N|R]) when ?IS_INT(N) ->R;
strip([$v,$s,$u,$N,N|R]) when ?IS_INT(N) ->R;
strip([$v,$b,$N,N|R]) when ?IS_INT(N) -> R;
strip([$v,$i,$N,N|R]) when ?IS_INT(N) -> R;
strip([$v,$s,$N,N|R]) when ?IS_INT(N) -> R;
strip([$v,$d,$N,N|R]) when ?IS_INT(N) -> R;
strip([$v,$f,$N,N|R]) when ?IS_INT(N) -> R;
strip([$b,$u,$N,N|R]) when ?IS_INT(N) -> R;
strip([$v,$b,$I,N|R]) when ?IS_INT(N) -> R;
strip([$v,$i,$I,N|R]) when ?IS_INT(N) -> R;
strip([$v,$s,$I,N|R]) when ?IS_INT(N) -> R;
strip([$v,$d,$I,N|R]) when ?IS_INT(N) -> R;
strip([$v,$f,$I,N|R]) when ?IS_INT(N) -> R;
strip([$b,$u,$I,N|R]) when ?IS_INT(N) -> R;

strip([$v,$b,$u,N,$I|R]) when ?IS_INT(N) ->R;
strip([$v,$i,$u,N,$I|R]) when ?IS_INT(N) ->R;
strip([$v,$s,$u,N,$I|R]) when ?IS_INT(N) ->R;
strip([$v,$b,N,$I|R]) when ?IS_INT(N) -> R;
strip([$v,$i,N,$I|R]) when ?IS_INT(N) -> R;
strip([$v,$s,N,$I|R]) when ?IS_INT(N) -> R;
strip([$v,$d,N,$I|R]) when ?IS_INT(N) -> R;
strip([$v,$f,N,$I|R]) when ?IS_INT(N) -> R;

strip([$v,$b,$u,N|R]) when ?IS_INT(N) ->R;
strip([$v,$i,$u,N|R]) when ?IS_INT(N) ->R;
strip([$v,$s,$u,N|R]) when ?IS_INT(N) ->R;
strip([$v,$b,N|R]) when ?IS_INT(N) -> R;
strip([$v,$i,N|R]) when ?IS_INT(N) -> R;
strip([$v,$s,N|R]) when ?IS_INT(N) -> R;
strip([$v,$d,N|R]) when ?IS_INT(N) -> R;
strip([$v,$f,N|R]) when ?IS_INT(N) -> R;

strip([$b,$u,N,$I|R]) when ?IS_INT(N) ->R;
strip([$i,$u,N,$I|R]) when ?IS_INT(N) ->R;
strip([$s,$u,N,$I|R]) when ?IS_INT(N) ->R;
strip([$b,N,$I|R]) when ?IS_INT(N) -> R;
strip([$i,N,$I|R]) when ?IS_INT(N) -> R;
strip([$s,N,$I|R]) when ?IS_INT(N) -> R;
strip([$d,N,$I|R]) when ?IS_INT(N) -> R;
strip([$f,N,$I|R]) when ?IS_INT(N) -> R;

strip([$b,$u,N|R]) when ?IS_INT(N) ->R;
strip([$i,$u,N|R]) when ?IS_INT(N) ->R;
strip([$s,$u,N|R]) when ?IS_INT(N) ->R;
strip([$b,N|R]) when ?IS_INT(N) -> R;
strip([$i,N|R]) when ?IS_INT(N) -> R;
strip([$s,N|R]) when ?IS_INT(N) -> R;
strip([$d,N|R]) when ?IS_INT(N) -> R;
strip([$f,N|R]) when ?IS_INT(N) -> R;

strip([$v,$i,$u,$I|R])  -> R;

strip([$v,$b,$u|R])  -> R;
strip([$v,$i,$u|R])  -> R;
strip([$v,$s,$u|R])  -> R;
strip([$v,$b|R])     -> R;
strip([$v,$i,$I|R])     -> R;
strip([$v,$i|R])     -> R;
strip([$v,$s|R])     -> R;
strip([$v,$d|R])     -> R;
strip([$v,$f|R])     -> R;

strip(R = "delban" ++ _) -> R;  % E|enabled
strip(R = "dexedn" ++ _) -> R;  % I|indexed
strip(R = "decnatsn" ++ _) -> R;  % I|instanced

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
strip([N|R]) when ?IS_INT(N) -> R;
strip([_|R="tceRlg"]) -> R;   % glRect*
strip([_|R="thgiLlg"]) -> R;  % glLight*
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
