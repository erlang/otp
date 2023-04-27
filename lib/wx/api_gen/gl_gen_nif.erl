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
%%% File    : gl_gen_c.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 25 Apr 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(gl_gen_nif).
-export([gen/2]).

-include("gl_gen.hrl").

-import(lists, [foldl/3,foldr/3,reverse/1, keysearch/3, map/2, filter/2, max/1]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1, uppercase_all/1,
		   open_write/1, close/0, c_copyright/0, w/2, 
		   args/3, strip_name/2]).
-import(wx_gen, [next_id/1]).

-define(OP, ((get(get(current_func)))#func.id)).

gen(GLFuncs, GLUFuncs) ->
    gen_defines(GLFuncs,GLUFuncs),
    gen_init(GLFuncs, GLUFuncs),

    %% Marshal funcs
    open_write("../c_src/gen/glu_nif.cpp"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("#include <stdio.h>~n", []),
    w("#include <string.h>~n", []),
    w("extern \"C\" {~n", []),
    w(" #include \"../egl_impl.h\"~n", []),
    w(" #include \"gl_fdefs.h\"~n", []),
    w("}~n~n",[]),
    [funcs(F) || F <- GLUFuncs],
    close(),

    open_write("../c_src/gen/gl_nif.cpp"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("#include <stdio.h>~n", []),
    w("#include <string.h>~n", []),
    w("#include <vector>~n", []),
    w("extern \"C\" {~n", []),
    w(" #include \"../egl_impl.h\"~n", []),
    w(" #include \"gl_fdefs.h\"~n", []),
    w("}~n~n",[]),

    w("void ecb_init_opengl(ErlNifEnv *env, ErlNifPid *self, ERL_NIF_TERM argv[])\n"
      "{\n"
      "    egl_load_functions();\n"
      "    init_tess();\n"
      "}\n\n", []),

    [funcs(F) || F <- GLFuncs],

    w("\n\n", []),
    w("#include \"gl_finit.h\"\n\n",[]),
    close().
funcs([F1|_Fs]) when is_list(F1) ->
    put(current_func,F1),
    FDef0 = get(F1),
    As0  = pre_marshal(FDef0#func.params),
    _FDef1 = apply_func(FDef0#func{params=As0}),
    erase(current_func),
    ok;
funcs(F) ->
    put(current_func,F),
    FDef0 = get(F),
    As0  = pre_marshal(FDef0#func.params),
    _FDef1 = apply_func(FDef0#func{params=As0}),
    erase(current_func).

pre_marshal(L) ->
    pre_marshal(L,0,L).

pre_marshal([A0=#arg{name=Len, where=c,alt={length,N}},
             A=#arg{name=N,type=#type{single=List},alt=Prev}|R], Argc, L)
  when List =:= list orelse element(1, List) =:= tuple_list ->
    undefined = Prev,
    [A0#arg{where=both},A#arg{name=N, alt={length,Len}}|pre_marshal(R, Argc+2, L)];
pre_marshal([A0=#arg{name=Len, where=c,alt={length,N}},A1,
             A=#arg{name=N,type=#type{single=List},alt=Prev}|R], Argc, L)
  when List =:= list orelse element(1, List) =:= tuple_list ->
    undefined = Prev,
    [A0#arg{where=both},A1,A#arg{name=N, alt={length,Len}}|pre_marshal(R,Argc+3,L)];
pre_marshal([A|R], Argc, L) ->
    [A|pre_marshal(R, Argc+1, L)];
pre_marshal([],_,_) -> [].

enif_get("GLdouble")   -> "enif_get_double";
enif_get("GLfloat")    -> "egl_get_float";
enif_get("GLclampd")   -> "enif_get_double";
enif_get("GLclampf")   -> "egl_get_float";
enif_get("GLint")      -> "enif_get_int";
enif_get("GLuint")     -> "enif_get_uint";
enif_get("GLint64")    -> "enif_get_int64";
enif_get("GLuint64")   -> "enif_get_uint64";
enif_get("GLshort")    -> "egl_get_short";
enif_get("GLushort")   -> "egl_get_ushort";
enif_get("GLenum")     -> "enif_get_uint";
enif_get("GLbyte")     -> "egl_get_byte";
enif_get("GLubyte")    -> "egl_get_ubyte";
enif_get("GLboolean")  -> "egl_get_ubyte";
enif_get("GLbitfield") -> "enif_get_uint";
enif_get("GLsizei")    -> "enif_get_int";
enif_get("GLintptr")   -> "egl_get_word";
enif_get("GLsizeiptr") -> "egl_get_word";
enif_get("GLsync")     -> "egl_get_ptr";
enif_get("GLhandleARB") -> "enif_get_uint64".

cast_to_type("GLintptr")    -> "(egl_word *)";
cast_to_type("GLsizeiptr")  -> "(egl_word *)";
cast_to_type("GLsync")      -> "(void **)";
%%cast_to_type("GLhandleARB") -> "(void *)";
cast_to_type("GLuint64")     -> "(egl_uint64_t *)";
cast_to_type("GLint64")      -> "(egl_int64_t *)";
cast_to_type(_T) -> "".

apply_func(F=#func{where=erl}) -> F;
apply_func(F=#func{name=Name,id=_Id,type=Type,params=As0}) ->
    w("void ecb_~s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])\n{\n", [Name]),
    Res = count_out(Type,As0),
    if Res > 0, Type =/= void ->
            w("  ~s result;\n",[result_type(Type)]),
            w("  ERL_NIF_TERM reply;\n",[]);
       Res > 0 ->
            w("  ERL_NIF_TERM reply;\n",[]);
       true -> ignore
    end,
    {As1, _} = lists:mapfoldl(fun declare_var/2, 0, As0),
    {As2, _Argc} = lists:mapfoldl(fun decode_var_0/2, 0, As1),
    %% io:format("~s/~w~n",[get(current_func),_Argc]),

    call_gl(Name, Type, As2),
    build_return_vals(count_out(Type,As2), Type, As2),
    free_args(),
    w("}~n~n", []),
    F.

declare_var(P=#arg{where=erl},A)  ->
    {P,A+1};
declare_var(P=#arg{where=c, alt={size, _}},A)  ->
    {P,A+1};
declare_var(P=#arg{alt={constant, _}},Argc) ->
    {P,Argc+1};
declare_var(P=#arg{name=Name,
                   type=#type{name=T,by_val=true,single=true,ref=undefined}},
            Argc) ->
    case lists:member(T, ["GLUquadric", "GLhandleARB", "GLsync"]) of
        true  -> w("  egl_uint64_t ~s;\n",[Name]);
        false -> w("  ~s ~s;\n",[T,Name])
    end,
    {P,Argc+1};
declare_var(P=#arg{name=_Name,type=#type{base=string,ref={pointer,2},mod=[const]}},Argc) ->
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=true, type=#type{base=Base}}, Argc)
  when Base =:= binary; Base =:= string ->
    w("  ErlNifBinary ~s;\n", [Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=true, alt=list_binary, type=#type{name=T}}, Argc) ->
    w("  ErlNifBinary ~s_bin;\n", [Name]),
    w("  unsigned int ~s_len;\n", [Name]),
    w("  std::vector <~s> ~s_vec;\n", [T, Name]),
    w("  ~s *~s;\n",[T, Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=true, type=#type{name=T, base=guard_int}}, Argc) ->
    w("  ErlNifBinary ~s;\n", [Name]),
    w("  ~s *~s_idx;\n",[T, Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=_In, type=#type{name="GLUquadric"}}, Argc) ->
    w("  GLUquadric *~s;\n", [Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, type=#type{base=memory}},Argc) ->
    w("  ErlNifBinary ~s;\n", [Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=true, type=#type{name=T,single={Comp, Sz}}},
            Argc)
  when (Sz < 17 orelse Sz =:= undefined) andalso (Comp =:= tuple orelse Comp =:= list) ->
    case Comp of
        tuple ->
            %% Undefined In tuples are max 4 elements
            Max = if Sz =:= undefined -> 4;
                     true -> Sz
                  end,
            w("  ~s ~s[~w];\n",[T,Name,Max]);
        list ->
            w("  ~s ~s[~w];\n",[T,Name,Sz])
    end,
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=true, type=#type{name=Type, single=list}}, Argc) ->
    w("  ~s *~s;\n",[Type,Name]),
    w("  std::vector <~s> ~s_vec;\n", [Type, Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=true, type=#type{name=T}}, Argc) ->
    w("  ~s *~s;\n",[T,Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=false, type=#type{base=string}}, Argc) ->
    w("  unsigned char *~s;\n", [Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=false, type=#type{base=binary}}, Argc) ->
    w("  ErlNifBinary ~s;\n", [Name]),
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=false, type=#type{name=T, single={Single,Sz}}}, Argc)
  when Single =:= list; Single =:= tuple ->
    w("  ~s ~s[~w];\n", [T, Name, Sz]),
    case Sz > 9 of
        false -> ignore;
        true  ->
            w("  ERL_NIF_TERM ~s_ts[~w];\n", [Name, Sz])
    end,
    {P,Argc+1};
declare_var(P=#arg{in=false, type=#type{single={list,Sz,_}}}, Argc) ->
    false = is_integer(Sz),  %% Assert
    {P,Argc+1};
declare_var(P=#arg{name=Name, in=false, type=#type{name=Type, single=Single, by_val=false}},Argc) ->
    case Single of
        true -> w("  ~s ~s;\n",[Type,Name]);
        What -> exit({What, P})
    end,
    {P,Argc+1};
declare_var(P, Argc) ->
    ?error({unhandled_type, P, Argc}).


%%% Decode

decode_var_0(P=#arg{name=_Name}, Argc) ->
    Res = decode_var(P, Argc),
    Res.

decode_var(P=#arg{name=Name, in=true, where=c, alt=Alt,
                 type=#type{name=Type}}, Argc) ->
    case Alt of
        {length, Var} -> w(" ~s = (~s) ~s_len;\n", [Name, Type, Var]);
        {length, Var1, Var2} ->
            w(" if (~s_len != ~s_len)  Badarg(~w, \"~s\");\n", [Var1,Var2,?OP,Var2]),
            w(" ~s = (~s) ~s_len;\n", [Name, Type, Var1]);
        {constant, _} -> ignore;
        %% {size, Var} -> w(" ~s = ~s.size;\n", [Name, Var]);
        {size, _} -> ignore;
        _ -> exit(P)
    end,
    {P,Argc};
decode_var(P=#arg{name=Name,
                  type=#type{name=Type,single=true,by_val=true,ref=undefined}},
           Argc) ->
    CastType = cast_to_type(Type),
    w("  if(!~s(env, argv[~w], ~s &~s)) Badarg(~w,\"~s\");~n",
      [enif_get(Type), Argc, CastType, Name, ?OP, Name]),
    {P,Argc+1};

decode_var(P=#arg{name=Name,type=#type{base=string,ref={pointer,2},mod=[const]}},Argc) ->
    w("  ERL_NIF_TERM ~s_l, ~s_h, ~s_t;\n", [Name,Name,Name]),
    w("  ErlNifBinary ~s_tmp;\n", [Name]),
    w("  std::vector <GLchar *> ~s;\n", [Name]),
    w("  ~s_l = argv[~w];\n",[Name,Argc]),
    w("  while(enif_get_list_cell(env, ~s_l, &~s_h, &~s_t)) {\n", [Name,Name,Name]),
    w("    if(!enif_inspect_binary(env, ~s_h, &~s_tmp)) Badarg(~w,\"~s\");\n", [Name,Name,?OP,Name]),
    w("    ~s.push_back((GLchar *) ~s_tmp.data);\n",[Name,Name]),
    w("    ~s_l = ~s_t;\n", [Name,Name]),
    w("  }\n",[]),
    {P,Argc+1};

decode_var(P=#arg{name=Name, in=true, type=#type{base=Base}}, Argc)
  when Base =:= binary; Base =:= string ->
    w("  if(!enif_inspect_binary(env, argv[~w], &~s)) Badarg(~w,\"~s\");\n", [Argc, Name,?OP,Name]),
    {P,Argc+1};

decode_var(P=#arg{name=Name, type=#type{base=memory}},Argc) ->
    w("  if(enif_is_binary(env, argv[~w]))~n"
      "    enif_inspect_binary(env, argv[~w], &~s);~n",
      [Argc, Argc, Name]),
    w("  else if(enif_is_tuple(env, argv[~w])) {\n"
      "    int ~s_a;\n"
      "    const ERL_NIF_TERM *~s_t;\n"
      "    if(enif_get_tuple(env, argv[~w], &~s_a, &~s_t) &&\n"
      "         enif_is_binary(env, ~s_t[1]))\n"
      "       enif_inspect_binary(env, ~s_t[1], &~s);~n",
      [Argc,Name,Name,Argc,Name,Name,Name,Name,Name]),
    w("    else Badarg(~w, \"~s\");\n"
      "  } else Badarg(~w, \"~s\");\n", [?OP,Name, ?OP,Name]),
    {P,Argc+1};

decode_var(P=#arg{name=Name, in=true, type=#type{name=Type, single={tuple,Sz0}}}, Argc) ->
    {SizeTest,Sz} = case is_integer(Sz0) of
                        true when Sz0 =:= 16 ->
                            {io_lib:format("~n       || (~s_a != 12 && ~s_a != 16)", [Name, Name]),
                             integer_to_list(Sz0)};
                        true  -> {io_lib:format(" || ~s_a != ~w", [Name, Sz0]), integer_to_list(Sz0)};
                        false -> {"", Name ++ "_a"}
                    end,
    w("  {\n", []),
    w("   int ~s_a;\n"
      "   const ERL_NIF_TERM *~s_t;\n", [Name,Name]),
    is_integer(Sz0) orelse w("   int i;\n",[]),
    w("   if(!enif_get_tuple(env, argv[~w], &~s_a, &~s_t)~s) {\n"
      "     Badarg(~w,\"~s\");~n   } else {\n",
      [Argc, Name, Name, SizeTest,?OP,Name]),
    R = fun(Out) when not ((Out+1) rem 4 == 0 andalso Sz0 == 16)  ->
                w("     if(!~s(env, ~s_t[i1++], &~s[~w])) Badarg(~w,\"~s\");\n",
                  [enif_get(Type), Name, Name, Out,?OP,Name]);
           (Out) when Out < 15 ->
                w("     if(~s_a == 16)\n"
                  "        if(!~s(env, ~s_t[i1++], &~s[~w])) Badarg(~w,\"~s\");\n",
                  [Name, enif_get(Type), Name, Name, Out,?OP,Name]),
                ok;
           (15) ->
                w("     if(~s_a == 16) {\n"
                  "        if(!~s(env, ~s_t[i1++], &~s[~w])) Badarg(~w,\"~s\");\n",
                  [Name, enif_get(Type), Name, Name, 15,?OP,Name]),
                w("     } else {\n"
                  "       ~s[3] = 0.0; ~s[7] = 0.0; ~s[11] = 0.0; ~s[15] = 1.0;\n"
                  "     }\n",[Name,Name,Name,Name]),
                ok
        end,
    case Sz0 of
        16 ->
            w("    int i1 = 0;\n", []),
            lists:map(R, lists:seq(0, Sz0-1));
        _ when is_integer(Sz0) ->
            w("    int i1 = 0;\n", []),
            lists:map(R, lists:seq(0, Sz0-1));
        _ ->
            w("     for(i = 0; i < ~s; i++)\n", [Sz]),
            w("       if(!~s(env, ~s_t[i], &~s[i])) Badarg(~w,\"~s\");\n",
              [enif_get(Type), Name, Name,?OP,Name])
    end,
    w("   }};~n",[]),
    {P,Argc+1};
decode_var(P=#arg{name=Name, in=true, alt=Alt,
                  type=#type{name=Type, base=Base, single=list}}, Argc)
  when Base =:= int; Base =:= float; Base =:= guard_int ->
    w("  if(!enif_is_list(env, argv[~w]))",[Argc]),
    case Alt of
        list_binary ->
            w(" {\n", []),
            w("    if(enif_is_binary(env, argv[~w])) {~n",[Argc]),
            w("       enif_inspect_binary(env, argv[~w], &~s_bin);~n",[Argc,Name]),
            w("    } else if(enif_is_tuple(env, argv[~w])) {\n", [Argc]),
            w("       int ~s_a;\n"
              "       const ERL_NIF_TERM *~s_t;\n", [Name,Name]),
            w("       if(enif_get_tuple(env, argv[~w], &~s_a, &~s_t) &&\n"
              "              enif_is_binary(env, ~s_t[1]))\n", [Argc, Name, Name, Name]),
            w("          enif_inspect_binary(env, ~s_t[1], &~s_bin);~n"
              "       else Badarg(~w, \"~s\");\n", [Name,Name,?OP,Name]),
            w("    } else Badarg(~w, \"~s\");\n", [?OP,Name]),
            w("    ~s = (~s *) ~s_bin.data;~n",[Name,Type,Name]),
            w("    ~s_len = ~s_bin.size / sizeof(~s);\n",[Name,Name,Type]),
            w(" }", []),
            ok;
        _ ->
            w(" Badarg(~w, \"~s\")\n ", [?OP,Name])
    end,
    w(" else {\n",[]),
    w("    ERL_NIF_TERM ~s_l, ~s_h, ~s_t;\n", [Name, Name, Name]),
    w("    ~s ~s_tmp;\n", [Type, Name]),
    w("    ~s_l = argv[~w];\n",[Name,Argc]),
    w("    while(enif_get_list_cell(env, ~s_l, &~s_h, &~s_t)) {\n", [Name,Name,Name]),
    CastType = cast_to_type(Type),
    w("        if(!~s(env, ~s_h,~s &~s_tmp)) Badarg(~w,\"~s\");\n", [enif_get(Type), Name, CastType, Name,?OP,Name]),
    w("        ~s_vec.push_back(~s_tmp);\n"
      "        ~s_l = ~s_t;\n    };\n",[Name,Name,Name,Name]),
    w("    ~s = ~s_vec.data();\n", [Name,Name]),
    case Alt of
        list_binary ->
            w("    ~s_len = ~s_vec.size();\n", [Name,Name]);
        _ ->
            ignore
    end,
    w("  }\n", []),
    {P,Argc+1};
decode_var(P=#arg{name=Name, in=true, alt=Alt,
                  type=#type{name=Type, single={tuple_list, TSz}}}, Argc) ->
    w("  if(!enif_is_list(env, argv[~w])) { Badarg(~w,\"~s\")}~n",[Argc,?OP,Name]),
    w("  int ~s_a;\n"
      "  const ERL_NIF_TERM *~s_tpl;\n"
      "  ERL_NIF_TERM ~s_l, ~s_h, ~s_t;\n",
      [Name, Name, Name, Name, Name]),
    Len = case Alt of
              {length, Var} -> Var;
              undefined ->
                  w("  int ~s_len;\n"
                    "  enif_get_list_length(env, argv[~w], &~s_len);\n",[Name, Argc,Name]),
                  Name ++ "_len"
          end,
    w("  std::vector <~s> ~s_vec (~w*~s);\n", [Type, Name, TSz, Len]),
    w("  ~s *~s_ptr = ~s_vec.data();~n", [Type, Name, Name]),
    w("  ~s_l = argv[~w];\n",[Name,Argc]),
    w("  while(enif_get_list_cell(env, ~s_l, &~s_h, &~s_t)) {\n", [Name,Name,Name]),
    w("      if(!enif_get_tuple(env, ~s_h, &~s_a, &~s_tpl) || ~s_a != ~w) Badarg(~w,\"~s\");~n",
      [Name, Name, Name, Name, TSz,?OP,Name]),
    CastType = cast_to_type(Type),
    R = fun(N) ->
                w("      if(!~s(env, ~s_tpl[~w],~s ~s_ptr++)) Badarg(~w,\"~s\");\n",
                  [enif_get(Type), Name, N, CastType, Name, ?OP,Name])
        end,
    [R(N) || N <- lists:seq(0, TSz-1)],
    w("      ~s_l = ~s_t;\n    };\n",[Name,Name]),
    w("  ~s = ~s_vec.data();\n",[Name,Name]),
    {P,Argc+1};


decode_var(P=#arg{name=Name, in=false,
                  type=#type{name=T, base=Base, size=Szs, single=Single}}, Argc)
  when not is_tuple(Single), (Base =:= binary orelse Base =:= string) ->
    Sz = case Szs of
             {Max,_} when is_integer(Max) -> integer_to_list(Max);
             {Max,_} -> Max;
             _ -> exit(P)
         end,
    case Base of
        string ->
            w("  ~s = (unsigned char *) enif_alloc((int) ~s*sizeof(~s));\n", [Name,Sz,T]),
            store_free(Name);
        binary when T == "GLvoid"; T == "void" ->
            w("  enif_alloc_binary((int) ~s*sizeof(char), &~s);\n", [Sz, Name]);
        binary ->
            w("  enif_alloc_binary((int) ~s*sizeof(~s), &~s);\n", [Sz,T, Name])
    end,
    {P,Argc};
decode_var(P=#arg{name=Name, type=#type{name=T, base=guard_int}}, Argc) ->
    w("  if(!egl_get_ptr(env, argv[~w], (void **) &~s_idx)) {\n"
      "    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[~w], &~s))\n", [Argc,Name,Argc,Name]),
    w("        ~s_idx = (~s *) ~s.data;\n"
      "    else Badarg(~w,\"~s\");\n  }\n" ,[Name,T,Name,?OP,Name]),
    {P,Argc+1};
decode_var(P=#arg{in=false, type=#type{single=true}}, Argc) ->
    {P,Argc};
decode_var(P=#arg{in=false, type=#type{single={C,Sz}}}, Argc)
  when C =:= tuple; C =:= list, is_integer(Sz) ->
    {P,Argc};
decode_var(P=#arg{name=Name, in=false, type=#type{name=T,single={list,Sz,_}}}, Argc) ->
    w("  std::vector <~s> ~s (~s);\n", [T, Name, Sz]),
    w("  std::vector <ERL_NIF_TERM> ~s_ts (~s);\n", [Name, Sz]),
    {P,Argc};
decode_var(P=#arg{name=Name, in=false,
                  type=#type{base=Base, name=T,single={list,Sz,_,_}, size=Size}}, Argc) ->
    case Base of
        string ->
            {BinSize, _} = Size,
            w("  ~s = (unsigned char *) enif_alloc((int) ~s*sizeof(~s));\n", [Name,BinSize,T]),
            w("  unsigned char *~s_ptr = ~s;\n", [Name,Name]),
            store_free(Name ++ "_ptr");
        _ ->
            exit({?LINE, Base, P})
    end,
    w("  std::vector <ERL_NIF_TERM> ~s_ts (~s);\n", [Name, Sz]),
    {P,Argc};
decode_var(P=#arg{name=Name, in=true, type=#type{name="GLUquadric"}}, Argc) ->
    w("  if(!egl_get_ptr(env, argv[~w], (void **) &~s)) Badarg(~w,\"~s\");~n",
      [Argc, Name,?OP,Name]),
    {P,Argc+1};
decode_var(P=#arg{in=true}, A)  ->
    {P,A+1}.

store_free(N) ->
    case get(free_args) of
        undefined -> put(free_args, [N]);
        List -> put(free_args, [N|List])
    end.

free_args() ->
    case get(free_args) of
        undefined -> ignore;
        List ->
	    erase(free_args),
            Free = fun({Cond, Var}) ->
                           w(" if(~s) enif_free(~s);~n", [Cond,Var]);
                      (Var) ->
                           w(" enif_free(~s);~n", [Var])
                   end,
            [Free(Arg) || Arg <- List]
    end.

call_gl(Name,void,As) ->
    {Args,_} = lists:mapfoldl(fun call_arg/2, 0, As),
    w("  we~s(~s);~n", [Name,lists:join(",",Args)]),
    As;
call_gl(Name,#type{},As) ->
    {Args,_} = lists:mapfoldl(fun call_arg/2, 0, As),
    w("  result = we~s(~s);~n", [Name,lists:join(",", Args)]),
    As.

result_type(#type{name=T, ref=undefined}) ->    T;
result_type(#type{name=T, ref={pointer,1}, mod=Mods}) ->
    mod(Mods) ++ T ++ " * ".

call_arg(#arg{alt={constant,Alt},type=#type{}}, Argc) ->
    {Alt, Argc};
call_arg(#arg{name=Name,in=true,type=#type{base=string,ref={pointer,2},mod=[const]}},Argc) ->
    {"(const GLchar **) " ++ Name ++ ".data()", Argc+1};
call_arg(#arg{name=Name,in=true,type=#type{name=T, base=Base}}, Argc)
  when Base =:= binary; Base =:= string; Base =:= memory ->
    {io_lib:format("(~s *) ~s.data", [T, Name]), Argc+1};
call_arg(#arg{name=Name, in=false,
              type=#type{name=T, base=string}}, Argc) ->
    {io_lib:format("(~s *) ~s",[T, Name]), Argc};
call_arg(#arg{name=Name, in=false,
              type=#type{name=T, base=binary}}, Argc) ->
    {io_lib:format("(~s *) ~s.data",[T, Name]), Argc};
call_arg(#arg{name=Name, alt=list_binary}, Argc) ->
    {Name, Argc+1};
call_arg(#arg{name=Name, type=#type{base=guard_int}}, Argc) ->
    {Name ++ "_idx", Argc+1};
call_arg(#arg{name=Name, type=#type{name="GLUquadric"}},Argc) ->
    {io_lib:format("~s", [Name]), Argc+1};
call_arg(#arg{in=true, where=both, name=Name, type=#type{name=T}},Argc) ->
    case lists:member(T, ["GLUquadric", "GLhandleARB", "GLsync"]) of
        true ->  {io_lib:format("(~s) ~s", [T,Name]), Argc+1};
        false -> {io_lib:format("~s", [Name]), Argc+1}
    end;
call_arg(#arg{name=Name, in=false, type=#type{single=true}},Argc) ->
    {io_lib:format("&~s", [Name]), Argc};
call_arg(_P=#arg{in=true, where=c, name=Name, alt=Alt, type=#type{name=T}},Argc) ->
    case Alt of
        {size, Var} -> {io_lib:format("(~s) ~s.size", [T, Var]), Argc+1};
        {length, _} -> {io_lib:format("~s", [Name]), Argc};
        {length, _,_} -> {io_lib:format("~s", [Name]), Argc};
        undefined   -> {io_lib:format("~s", [Name]), Argc}
    end;
call_arg(_P=#arg{name=Name, in=false, type=#type{single={list,_Sz,_}}}, Argc) ->
    {io_lib:format("~s.data()", [Name]), Argc};
call_arg(#arg{name=Name}, Argc) ->
    {io_lib:format("~s", [Name]), Argc}.

count_out(Type,As) ->
    Out = length([A || A=#arg{where=W, in=In} <- As, W =/= c, In =/= true]),
    case Type of
        void -> Out;
        _ -> Out+1
    end.

build_return_vals(0,_Type,As) ->
    %% Sync memory access functions
    Any = fun(#arg{type=#type{base=B}}) -> B =:= memory end,
    case lists:any(Any, As) of
        false -> ok;
        true ->
            w("  enif_send(NULL, self, env,\n"
              "    enif_make_tuple2(env,EGL_ATOM_REPLY,\n"
              "                         EGL_ATOM_OK));\n",[]),
            ok
    end;
build_return_vals(Cnt, Type, As) ->
    true = Cnt < 8,
    Build = fun(#arg{name=N,in=In,type=T}) ->
                    build_ret(N, In, T)
            end,
    Returns = [Build(A) || A=#arg{where=W, in=In} <- As, W =/= c, In =/= true],
    [prepare_ret(Pre) || Pre=#arg{where=W, in=In} <- As, W =/= c, In =/= true],
    Es = case Type of
             void -> Returns;
             _ -> [build_ret("result", false, Type)|Returns]
         end,
    case Cnt of
        1 -> w("  reply = ~s;\n", [Es]);
        _ -> w("  reply = enif_make_tuple~w(env,\n     "
               "~s );\n",[Cnt,lists:join(",\n", Es)])
    end,
    w("  enif_send(NULL, self, env,\n"
      "   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));\n",[]).

build_ret("result", false, void) -> "";
build_ret(Name,_Q,#type{name=T,base=Base,size=Sz,single=true})
  when Base =:= int; Base =:= bool ->
    Ptr = lists:member(T, ["GLUquadric", "GLhandleARB", "GLsync"]),
    if Sz =< 4 -> io_lib:format("     enif_make_int(env, ~s)", [Name]);
       Ptr     -> io_lib:format("     enif_make_uint64(env, (egl_uint64_t) ~s)", [Name]);
       true    -> io_lib:format("     enif_make_int64(env, (egl_int64_t) ~s)", [Name])
    end;
build_ret(Name,_Q,#type{name=_T,base=float,size=Sz,single=true}) ->
    if Sz =< 4 -> io_lib:format("     enif_make_double(env, (double) ~s)", [Name]);
       true    -> io_lib:format("     enif_make_double(env, ~s)", [Name])
    end;
build_ret(Name,false,#type{base=binary}) ->
    io_lib:format("     enif_make_binary(env, &~s)",[Name]);
build_ret(Name,false,T=#type{single={tuple,Sz}}) when Sz < 10 ->
    ELs = [build_ret(io_lib:format("~s[~w]",[Name,I]),false,T#type{single=true})
           || I <- lists:seq(0, Sz-1)],
    [io_lib:format("     enif_make_tuple~w(env,\n",[Sz]),
     lists:join(",\n       ", ELs), ")"];
build_ret(Name,false,T=#type{single={list,Sz}}) when Sz < 10 ->
    ELs = [build_ret(io_lib:format("~s[~w]",[Name,I]),false,T#type{single=true})
           || I <- lists:seq(0, Sz-1)],
    [io_lib:format("     enif_make_list~w(env,\n",[Sz]),
     lists:join(",\n       ", ELs), ")"];
build_ret(Name,false,#type{single={tuple,Sz}}) when Sz >= 10, is_integer(Sz) ->
    io_lib:format("     enif_make_tuple_from_array(env, ~s_ts, ~w)",[Name, Sz]);
build_ret(Name,false,#type{single={list,Sz}}) when Sz >= 10, is_integer(Sz) ->
    io_lib:format("     enif_make_list_from_array(env, ~s_ts, ~w)",[Name, Sz]);
build_ret(Name,false,#type{single={list,_,Sz}}) ->
    io_lib:format("     enif_make_list_from_array(env, ~s_ts.data(), ~s)",[Name, Sz]);
build_ret(Name,false,#type{single={list,_,Sz,_}}) ->
    io_lib:format("     enif_make_list_from_array(env, ~s_ts.data(), ~s)",[Name, Sz]);
build_ret(Name,_Q,#type{base=string,single=true}) ->
    io_lib:format("     enif_make_string(env, (const char *) ~s, ERL_NIF_LATIN1)",[Name]);
build_ret(Name,_Q,#type{base=string,size={_,_OutSz}}) ->
    io_lib:format("     enif_make_string(env, (const char *) ~s, ERL_NIF_LATIN1)",[Name]);
build_ret(Name,_Q,T=#type{}) ->
    io:format("{~p, {~p, {single,{tuple,X}}}}.~n", [get(current_func),Name]),
    io:format(" ~p~n",[T]).

prepare_ret(#arg{name=Name, type=#type{single={Comp,Sz}}=T})
  when Sz >= 10 andalso (Comp =:= list orelse Comp =:= tuple) ->
    Fetch = build_ret(Name ++ "[ri]", false, T#type{single=true}),
    w("  for(int ri=0; ri < (int) ~w; ri++)\n"
      "     ~s_ts[ri] = ~s;\n",[Sz, Name, Fetch]);
prepare_ret(#arg{name=Name, type=#type{single={list,_,Sz}}=T}) ->
    Fetch = build_ret(Name ++ "[ri]", false, T#type{single=true}),
    w("  for(int ri=0; ri < (int) ~s; ri++)\n"
      "    ~s_ts[ri] = ~s;\n",[Sz, Name, Fetch]);
prepare_ret(#arg{name=Name, type=#type{single={list,_,Sz,Lengths}}=T}) ->
    Fetch = build_ret(Name, false, T#type{single=true}),
    w("  for(int ri=0; ri < (int) ~s; ri++) {\n"
      "    ~s_ts[ri] = ~s;\n",[Sz, Name, Fetch]),
    w("    ~s += ~s[ri];\n"
      "   }\n", [Name, Lengths]);
prepare_ret(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_defines(GLFuncs,GLUFuncs) ->
    open_write("../c_src/gen/gl_fdefs.h"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("#ifdef WX_DEF_EXTS~n", []),
    w("# define WXE_EXTERN~n", []),
    w("#else~n# define WXE_EXTERN extern~n", []),
    w("#endif~n~n", []),

    GLFirst = case hd(GLFuncs) of
		  [First|_] when is_list(First) -> get(First);
		  First -> get(First)
	      end,
    GLLast = case lists:last(GLFuncs) of
                 [Last|_] when is_list(Last) -> get(Last);
                 Last -> get(Last)
             end,
    w("void ecb_init_opengl(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[]);\n\n", []),

    w("#define GLE_LIB_START 5000~n", []),
    w("#define GLE_GL_FUNC_START ~p~n", [GLFirst#func.id]),
    w("#define GLE_GL_FUNC_LAST ~p~n", [GLLast#func.id]),

    [fdefs(F) || F <- GLFuncs],
    [fdefs(F) || F <- GLUFuncs],
    close().

fdefs([F1|_Fs]) when is_list(F1) ->
    put(current_func,F1),
    fdef(get(F1)),
    erase(current_func);
fdefs([]) -> ok;
fdefs(F) ->
    put(current_func,F),
    fdef(get(F)),
    erase(current_func).

fdef(#func{where=erl}) -> ok;
fdef(#func{name=Name,type=T,params=As,alt=_Alt}) ->
    w("typedef ~s (APIENTRY * WXE~s)(~s);~n",
      [fdef_type(T), uppercase_all(Name), fdef_types(As)]),
    w("WXE_EXTERN WXE~s we~s;~n", [uppercase_all(Name), Name]),
    w("void ecb_~s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[]);\n",[Name]).

fdef_type(void) -> "void";
fdef_type(#type{name=T, mod=Mod, single=true, ref=undefined}) ->
    mod(Mod) ++ T;
fdef_type(#type{name=T, mod=Mod, single={tuple,_Sz}, ref=undefined}) ->
    mod(Mod) ++ T ++ " m[]";
fdef_type(#type{name=T, mod=Mod, ref={pointer,1}}) ->
    mod(Mod) ++ T ++ " *";
fdef_type(#type{name=T, mod=Mod, ref={pointer,2}}) ->
    mod(Mod) ++ T ++ " **".

mod([const]) -> "const ";
mod([]) -> "".

fdef_types(As) ->
    args(fun(#arg{type=T}) -> fdef_type(T) end, ",", As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_init(GLFuncs, GLUFuncs) ->
    open_write("../c_src/gen/gl_finit.h"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("gl_fns_t gl_fns[] =\n"
      "{\n", []),
    w("  {5000, \"init_opengl\", NULL, NULL, &ecb_init_opengl},~n ",[]),
    w(" ~s", [join(",\n  ", finits(GLUFuncs++GLFuncs, fun finit/1))]),
    w(",\n  { -1, NULL, NULL, NULL, NULL}};\n",[]),
    close().

join(Sep, List) ->
    lists:join(Sep, [E || E <- List, is_list(E)]).

finits(Fs0, Fun) ->
    Fs1 = [case Fl of [F00|_] when is_list(F00) -> get(F00); _ -> get(Fl) end || Fl <- Fs0],
    Fs = [F || F = #func{id=Id} <- Fs1, Id =/= undefined],
    finits(lists:keysort(#func.id, Fs), Fun, 5001).

finits([F|Fs]=Retry, Fun, Id) when Id < 6500 ->
    put(current_func,F#func.name),
    case F of
        #func{id=Id} ->
            R = Fun(F),
            [R|finits(Fs, Fun, Id+1)];
        #func{id=Next} when Next > Id ->
            R = Fun(#func{id=Id, where=erl, name="unused"}),
            erase(current_func),
            [R|finits(Retry, Fun, Id+1)];
        #func{id=Next} -> exit({id, Id, Next})
    end;
finits([], _, _) -> [].

finit(#func{id=5009,where=erl}) ->
    io_lib:format("{5009, \"gluTesselate\", NULL, NULL, &erl_tess_impl}", []);
finit(#func{id=Id,name=Name,where=erl}) ->
    io_lib:format("{~w, ~p, NULL, NULL, NULL}", [Id, Name]);
finit(#func{id=Id,name=Name, type=T, ext=Ext}) ->
    case T of
        #type{mod=[const]} -> %% Avoid windows const warning
            io_lib:format("{~w, ~p, ~s, (void *) &we~s, &ecb_~s}", [Id, Name, ext(Name,Ext), Name, Name]);
        _ ->
            io_lib:format("{~w, ~p, ~s, &we~s, &ecb_~s}", [Id, Name, ext(Name,Ext), Name, Name])
    end.

ext(Name, {ext,Ext}) ->
    "\"" ++ Name ++ Ext ++ "\"";
ext(_,_) -> "NULL".
