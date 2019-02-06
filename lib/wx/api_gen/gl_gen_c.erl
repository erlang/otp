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
%%% File    : gl_gen_c.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 25 Apr 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(gl_gen_c).
-compile(export_all).

-include("gl_gen.hrl").

-import(lists, [foldl/3,foldr/3,reverse/1, keysearch/3, map/2, filter/2, max/1]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1, uppercase_all/1,
		   open_write/1, close/0, c_copyright/0, w/2, 
		   args/3, strip_name/2]).
-import(wx_gen, [next_id/1]).


gen(GLFuncs, GLUFuncs) ->
    gen_defines(GLFuncs,GLUFuncs),
    gl_gen_init(GLFuncs),
    glu_gen_init(GLUFuncs),
    
    %% Marshal funcs
    open_write("../c_src/gen/gl_funcs.cpp"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("#include <stdio.h>~n", []),
    w("#include <string.h>~n", []),    
    w("#include \"../egl_impl.h\"~n", []),
    w("#include \"gl_fdefs.h\"~n~n", []),
    w("extern gl_fns_t gl_fns[];~n~n", []),

    w("void egl_dispatch(int op, char *bp, ErlDrvPort port, "
      "ErlDrvTermData caller, char *bins[], int bins_sz[]){~n",
      []),
    w(" try {~n",[]),
    w(" switch(op)~n{~n",[]),
    w(" case 5000:~n   erl_tess_impl(bp, port, caller);~n   break;~n", []),

    [funcs(F) || F <- GLUFuncs],
    [funcs(F) || F <- GLFuncs],
    
    w("}} catch (char *err_msg) {\n"
      "int AP = 0; ErlDrvTermData rt[12];\n"
      "rt[AP++] = ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) \"_egl_error_\");\n"
      "rt[AP++] = ERL_DRV_INT; rt[AP++] = (int) op;\n"
      "rt[AP++] = ERL_DRV_ATOM; rt[AP++] = driver_mk_atom((char *) err_msg);\n"
      "// rt[AP++] = ERL_DRV_ATOM; rt[AP++] = driver_mk_atom((char *) gl_fns[op-GLE_GL_FUNC_START].name);\n"
      "// rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;\n"
      "rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 3;\n"
      "driver_send_term(port,caller,rt,AP);\n", []),
    w("}} /* The End */~n~n",[]),
    
    close().
    
funcs([F1|Fs]) when is_list(F1) ->
    put(current_func,F1),
    func(get(F1)),
    erase(current_func),
    funcs(Fs);
funcs([]) -> ok;
funcs(F) ->
    put(current_func,F),
    func(get(F)),
    erase(current_func).

func(#func{where=erl}) -> ok;
func(#func{id=Id,alt={has_vector,_,FuncName}}) -> 
    #func{name=Name,type=T,params=As} = get(FuncName),
    w("case ~p: { // ~s~n", [Id,Name]),
    put(bin_count,-1),
    As1 = declare_vars(T, As),
    As2 = decode_args(As1),
    As3 = call_gl(Name,T,As2),
    build_return_vals(T,As3),
    free_args(),
    w("}; break;~n", []);
func(#func{name=Name,id=Id,type=T,params=As,alt=_Alt}) ->
    w("case ~p: { // ~s~n", [Id,Name]),
    put(bin_count,-1),
    As2 = decode_args(As),
    declare_vars(T, As),  %% Unusal order but it's c++
    As3 = call_gl(Name,T,As2),
    build_return_vals(T,As3),
    free_args(),
    w("}; break;~n", []).

declare_vars(void,Ps) ->
    [declare_var(P) || P <- Ps];
declare_vars(T, Ps) ->
    declare_var(#arg{name="result",in=false,type=T}),
    [declare_var(P) || P <- Ps].

declare_var(A=#arg{where=erl}) -> A;

declare_var(A=#arg{name=N,in=false,type=#type{name=T,base=B,single={tuple,Sz}}}) -> 
    true = is_number(Sz), %% Assert
    w(" ~s ~s[~p] = {~s};~n", [T,N,Sz,args(fun zero/1,",",lists:duplicate(Sz,B))]),
    A;
declare_var(A=#arg{name=N,in=false,type=#type{name=T,base=B,single={list,Sz}}}) 
  when is_number(Sz) -> 
    w(" ~s ~s[~p] = {~s};~n", [T,N,Sz,args(fun zero/1,",",lists:duplicate(Sz,B))]),
    A;
declare_var(A=#arg{name=N,in=false,type=#type{name=T,base=string,size={Max,_}}}) ->
    case is_integer(Max) of
	true ->
	    w(" ~s ~s[~p];~n", [T,N,Max]);
	false ->
	    w(" ~s *~s;~n", [T,N]),
	    w(" ~s = (~s *) driver_alloc(sizeof(~s) * *~s);~n", [N,T,T,Max]),
	    store_free(N)
    end,
    A;
declare_var(A=#arg{name=N,in=false,type=#type{base=binary,size={MaxSz, _}}}) -> 
    MaxSz == undefined andalso error({assert, A}),
    case is_integer(MaxSz) of
	true -> 
	    w(" ErlDrvBinary *~s = driver_alloc_binary(~p);~n", [N,MaxSz]);
	false ->
	    w(" ErlDrvBinary *~s = driver_alloc_binary(*~s);~n", [N,MaxSz])
    end,
    A;
declare_var(A=#arg{name=N,in=false,type=#type{name=T,single={list,ASz,_USz},mod=[]}}) -> 
    true = is_list(ASz), %% Assert
    w(" ~s *~s;~n", [T,N]), 
    w(" ~s = (~s *) driver_alloc(sizeof(~s) * *~s);~n", [N,T,T,ASz]),
    store_free(N),
    %% w(" ~s ~s[*~s];~n", [T,N,ASz]),
    A;
declare_var(A=#arg{in=false, type=#type{name="GLUquadric",by_val=false,single=true}}) ->
    A;
declare_var(A=#arg{in=false, type=#type{base=string,by_val=false,single=true}}) ->
    A;
declare_var(A=#arg{name=N,in=false,
		   type=#type{name=T,base=B,by_val=false,single=true}}) -> 
    w(" ~s ~s[1] = {~s};~n", [T,N,zero(B)]),
    A;
declare_var(A=#arg{where=c, type=#type{name=T}, alt={size,Var}}) ->
    w(" ~s ~s_size = bins_sz[~p];~n", [T, Var, get(bin_count)]),
    A;
declare_var(A=#arg{where=_}) -> 
    A.

zero(float) -> "0.0";
zero(_) -> "0".

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
            [w(" driver_free(~s);~n", [Arg]) || Arg <- List]
    end.
    
decode_args(As0) -> 
    {As,_Align} = lists:mapfoldl(fun decode_arg/2,0,As0),
    As.

decode_arg(P=#arg{where=erl},A) -> {P,A};
decode_arg(P=#arg{where=c},A)   -> {P,A};
decode_arg(P=#arg{in=false},A)  -> {P,A};

decode_arg(P=#arg{name=Name,alt=Alt,type=#type{name=Type,base=binary}},A0) ->
    w(" ~s *~s = (~s *) bins[~p];~n", [Type,Name,Type,next_id(bin_count)]),
    case Alt of
        list_binary ->
            A = align(4, A0),
            w(" int * ~sLen = (int *) bp; bp += 4; (void) ~sLen;~n", [Name, Name]),
            {P, A};
        _ ->
            {P, A0}
    end;
decode_arg(P=#arg{name=Name,type=#type{name=Type,base=memory}},A0) ->
    w(" ~s *~s = (~s *) bins[~p];~n", [Type,Name,Type,next_id(bin_count)]),
    {P, A0};
decode_arg(P=#arg{name=Name,type=#type{name=T,base=string,single=list}},A0) ->
    A = align(4,A0),
    w(" int * ~sLen = (int *) bp; bp += 4;~n",    [Name]),
    w(" int * ~sTotSize = (int *) bp; bp += 4;~n",[Name]),
%%    w(" if(*~sLen > 256) *~sLen = 256;", []),
    w(" ~s **~s;~n", [T,Name]),
    w(" ~s = (~s **) driver_alloc(sizeof(~s *) * *~sLen);~n",[Name, T, T, Name]),
    store_free(Name),
    w(" for(int i=0;i<*~sLen;i++) {~n", [Name]),
    w("    ~s[i] = (~s *) bp; bp += 1+strlen(bp);};~n",[Name,T]),
    w(" bp += (8 - ((~p + *~sTotSize) % 8)) % 8;~n",[A,Name]),
    {P, 0};

decode_arg(P=#arg{name=Name,type=#type{size=Sz,single=list,name=Type}},A0) ->
    A = align(max([Sz,4]),A0),
    w(" int * ~sLen = (int *) bp; bp += ~p;~n",    [Name, max([4,Sz])]),
    w(" ~s * ~s = (~s *) bp; ", [Type,Name,Type]),
    w(" bp += *~sLen*~p + (8-((*~sLen*~p+~p)%8))%8;~n", [Name,Sz,Name,Sz,A]),
    {P, 0};
decode_arg(P=#arg{name=Name,type=#type{size=TSz,name=Type,single={tuple,undefined}}},A0) ->
    A = align(TSz,A0),
    w(" int *~sLen = (int *) bp; bp += ~p;~n",    [Name, max([4,TSz])]),
    if TSz =:= 4 ->
	    w(" ~s *~s = (~s *) bp; bp += *~sLen*4+((*~sLen)+~p)%2*4;~n", 
	      [Type,Name,Type, Name,Name, A div 4]);
       TSz =:= 8 ->
	    w(" ~s *~s = (~s *) bp; bp += *~sLen*8;~n", [Type,Name,Type,Name])
    end,
    {P, 0};
decode_arg(P=#arg{name=Name,type=#type{size=BSz,name=Type,single={tuple,TSz}}},A0) ->
    A = align(BSz,TSz,A0),
    w(" ~s * ~s = (~s *) bp; bp += ~p;~n", [Type,Name,Type,BSz*TSz]),
    {P, A};
decode_arg(P=#arg{name=Name,type=#type{size=BSz,name=Type,single={list,TSz}}},A0) ->
    A = align(BSz,TSz,A0),
    w(" ~s * ~s = (~s *) bp; bp += ~p;~n", [Type,Name,Type,BSz*TSz]),
    {P, A};
decode_arg(P=#arg{name=Name,type=#type{name=Type,base=guard_int}},A0) ->
    A = align(4,A0),
    w(" ~s *~s = (~s *) (ErlDrvSInt) * (int *) bp; bp += 4;~n", [Type,Name,Type]),
    {P, A};
decode_arg(P=#arg{name=Name,type=#type{name=Type,base=string,single=true}},A0) ->
    w(" ~s *~s = (~s *) bp;~n", [Type,Name,Type]),
    w(" int ~sLen[1] = {(int)strlen((char *)~s)}; bp += ~sLen[0]+1+((8-((1+~sLen[0]+~p)%8))%8);~n",
      [Name,Name,Name,Name,A0]),
    {P, 0};
decode_arg(P=#arg{name=Name,
		  type=#type{name=Type,size=8,base=int,by_val=true,ref=undefined}},A0) ->
    A = align(8,A0),
    w(" ~s ~s = (~s) * (GLuint64EXT *) bp; bp += 8;~n", [Type,Name,Type]),
    {P, A};
decode_arg(P=#arg{name=Name,type=#type{name=Type="GLUquadric",size=8,base=int}},A0) ->
    A = align(8,A0),
    w(" ~s * ~s = (~s *) * (GLuint64EXT *) bp; bp += 8;~n", [Type,Name,Type]),
    {P, A};
decode_arg(P=#arg{name=Name,
		  type=#type{name=Type,size=Sz,by_val=true,ref=undefined}},A0) ->
    A = align(Sz,A0),
    w(" ~s *~s = (~s *) bp; bp += ~p;~n", [Type,Name,Type,Sz]),
    {P, A};
decode_arg(P=#arg{name=Name,type=#type{size=BSz,name=Type,single={tuple_list,TSz}}},A0) ->
    A = align(BSz,TSz,A0),
    w(" int *~sLen = (int *) bp; bp += ~p;~n",    [Name, max([4,BSz])]),
    w(" ~s * ~s = (~s *) bp; bp += *~sLen*~p;~n", [Type,Name,Type,Name,BSz*TSz]),
    {P, A};
decode_arg(P=#arg{name=Name, type=#type{name=Type,size=Sz,by_val=false,
					ref={pointer,1}, mod=[const]}},
	   A0) ->
    A = align(Sz,A0),
    w(" ~s *~s = (~s *) bp; bp += ~p;~n", [Type,Name,Type,Sz]),
    {P, A};
decode_arg(P, _A) ->
    ?error({unhandled_type, {P#arg.name,P#arg.type,_A}}).

align(Size, PreAlign) ->
    align(Size,1,PreAlign).

align(1,N,A) ->                               (A+1*N+0) rem 8;

align(2,N,A) when (A rem 2) =:= 0 ->          (A+2*N+0) rem 8;
align(2,N,A) when (A rem 2) =:= 1 -> a_str(1),(A+2*N+1) rem 8;

align(4,N,A) when (A rem 4) =:= 0 ->          (A+4*N+0) rem 8;
align(4,N,A) when (A rem 4) =:= 1 -> a_str(3),(A+4*N+3) rem 8;
align(4,N,A) when (A rem 4) =:= 2 -> a_str(2),(A+4*N+2) rem 8;
align(4,N,A) when (A rem 4) =:= 3 -> a_str(1),(A+4*N+1) rem 8;

align(8,_,0) ->           0;
align(8,_,1) ->  a_str(7),0;
align(8,_,2) ->  a_str(6),0;
align(8,_,3) ->  a_str(5),0;
align(8,_,4) ->  a_str(4),0;
align(8,_,5) ->  a_str(3),0;
align(8,_,6) ->  a_str(2),0;
align(8,_,7) ->  a_str(1),0.

a_str(P) -> w(" bp += ~p;~n", [P]).

call_gl(Name,void,As) ->
    Args = args(fun call_arg/1, ",", As),
    w(" we~s(~s);~n", [Name,Args]),
    As;
call_gl(Name,T=#type{},As) ->
    Args = args(fun call_arg/1, ",", As),
    Type = result_type(T),
    w(" ~s result = we~s(~s);~n", [Type,Name,Args]),
    As.

result_type(#type{name=T, ref=undefined}) ->    T;
result_type(#type{name=T, ref={pointer,1}, mod=Mods}) ->  
    mod(Mods) ++ T ++ " * ".

call_arg(#arg{alt={size,Alt},type=#type{}}) ->
    Alt ++ "_size";
call_arg(#arg{alt={length,Alt},type=#type{}}) ->
    "*" ++ Alt ++ "Len";
call_arg(#arg{alt={constant,Alt},type=#type{}}) ->
    Alt;
call_arg(#arg{name=Name,type=#type{single={tuple, _}}}) ->
    Name;
call_arg(#arg{name=Name,type=#type{single={list, _}}}) ->
    Name;
call_arg(#arg{name=Name,type=#type{size=8,base=int,ref=undefined}}) ->
    Name;
call_arg(#arg{name=Name,in=false,type=#type{name=T, base=binary}}) ->
    "(" ++ T ++ "*) " ++ Name ++ "->orig_bytes";
call_arg(#arg{name=Name,type=#type{ref=undefined}}) ->
    "*" ++ Name;
call_arg(#arg{name=Name,type=#type{base=guard_int}}) ->
    Name;
call_arg(#arg{name=Name,type=#type{base=string,ref={pointer,2},mod=[const]}}) ->
    "(const GLchar **) " ++ Name;
call_arg(#arg{name=Name,type=#type{size=8,base=int,ref={pointer,1}}}) ->
    Name;
call_arg(#arg{name=Name,type=#type{}}) ->
    Name.

build_return_vals(Type,As) ->
    case calc_sizes(Type,As) of
	{0,none,0} ->  %% Sync memory access functions
	    Any = fun(#arg{type=#type{base=B}}) -> B =:= memory end,
	    case lists:any(Any, As) of
		false -> ok;
		true ->
		    w(" int AP = 0; ErlDrvTermData rt[6];~n",[]),
		    w(" rt[AP++]=ERL_DRV_ATOM;"
		      " rt[AP++]=driver_mk_atom((char *) \"_egl_result_\");~n",[]),
		    w(" rt[AP++]=ERL_DRV_ATOM;"
		      " rt[AP++]=driver_mk_atom((char *) \"ok\");~n",[]),
		    w(" rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;~n",[]),
		    w(" driver_send_term(port,caller,rt,AP);~n",[]),
		    ok
	    end;
	{Val,Vars,Cnt} ->
	    ExtraTuple = if Cnt > 1 -> 2; true -> 0 end,
	    if Vars =:= none -> 
		    Sz = integer_to_list(Val+4+ExtraTuple),
		    w(" int AP = 0; ErlDrvTermData rt[~s];~n",[Sz]),
		    Sz;
	       true -> 
		    Sz = integer_to_list(Val+4+ExtraTuple) ++ " + " ++ Vars,
		    w(" int AP = 0; ErlDrvTermData *rt;~n",[]),
		    w(" rt = (ErlDrvTermData *) "
		      "driver_alloc(sizeof(ErlDrvTermData)*(~s));~n", [Sz]),
		    Sz
	    end,
	    w(" rt[AP++]=ERL_DRV_ATOM; rt[AP++]=driver_mk_atom((char *) \"_egl_result_\");~n",[]),
	    FreeList = build_ret_types(Type,As),
	    case Cnt of
		1 -> ok;
		_ -> 
		    w(" rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = ~p;~n",[Cnt])
	    end,
	    w(" rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = 2;~n",[]),
	    %%w(" if (AP != ~s )  fprintf(stderr, \"%d: ERROR AP mismatch %d %d\\r\\n\",__LINE__,AP,~s);~n",
	    %%  [CSize,CSize]),
	    w(" driver_send_term(port,caller,rt,AP);~n",[]),
	    case Vars of 
		none -> ignore;
		_ -> 		    
		    w(" driver_free(rt);~n", [])
	    end,
	    [w(" ~s~n", [Name]) || Name <- FreeList],
	    ok
    end.
     
calc_sizes(Type,As) ->
    TSz = case return_size("result", Type) of
	      {0, none} ->
		  {0, none, 0};
	      {Sz,Vars} ->
		  {Sz,Vars, 1}
	  end,
    Calc = fun(#arg{name=N,in=False,where=W,type=T},{Sz,Vars, Cnt}) 
	      when False =/= true, W=/= c ->
		   case return_size(N, T) of
		       {Val, none} -> {Sz+Val, Vars, Cnt+1};
		       {Val, Var} when Vars =:= none -> 			   
			   {Sz+Val, Var,Cnt+1};
		       {Val, Var}  -> 
			   {Sz+Val, Var ++ " + " ++ Vars,Cnt+1}
		   end;
	      (_,Acc) -> Acc
	   end,
    foldl(Calc, TSz, As).
 
return_size(_N,void) -> {0, none};
return_size(_N,#type{single={tuple,Sz}}) ->                {Sz*2+2, none};
return_size(_N,#type{single={list,Sz}})  ->                {Sz*2+3, none};
return_size(_N,#type{base=string,single=true}) ->          {3, none};
return_size(_N,#type{base=string,single=undefined}) ->     {3, none};
return_size(_N,#type{base=string,single={list,_,"result"}}) ->   {3, "result*3"};
return_size(_N,#type{base=string,single={list,_,Sz}}) ->   {3, "(*" ++Sz++")*3"};
return_size(_N,#type{single={list,_,"result"}}) ->         {3, "result*2"};
return_size(_N,#type{single={list,_,Sz}}) ->               {3, "(*" ++Sz++")*2"};
return_size(_N,#type{base=binary}) ->                      {4, none};
return_size(_N,#type{single=true}) ->                      {2, none}.


build_ret_types(void,Ps) -> 
    Calc = fun(#arg{name=N,in=False,where=W,type=T},Free) 
	      when False =/= true, W=/= c ->
		   case build_ret(N, False, T) of
		       ok -> Free; 
		       Other -> [Other|Free]
		   end;
	      (_,Free) -> Free
	   end,
    lists:foldl(Calc, [], Ps);
build_ret_types(Type,Ps) -> 
    build_ret("result", out, Type),
    Calc = fun(#arg{name=N,in=False,where=W,type=T},Free) 
	      when False =/= true, W=/= c ->
		   case build_ret(N, False, T) of
		       ok -> Free; 
		       Other -> [Other|Free]
		   end;
	      (_,Free) -> Free
	   end,
    lists:foldl(Calc, [], Ps).

build_ret(Name,_Q,#type{name=_T,base=int,single=true,by_val=true}) ->    
    w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) ~s;~n", [Name]);
build_ret(Name,_Q,#type{name=_T,base=bool,single=true,by_val=true}) ->
    w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) ~s;~n", [Name]);
build_ret(Name,_Q,#type{name="GLUquadric",base=int}) ->    
    w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) ~s;~n", [Name]);
build_ret(Name,_Q,#type{name=_T,base=int,single=true,by_val=false}) ->    
    w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *~s;~n", [Name]);
build_ret(Name,_Q,#type{name=_T,base=bool,single=true,by_val=false}) ->
    w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *~s;~n", [Name]);
build_ret(Name,_Q,#type{name=_T,size=4,base=float,single=true,by_val=false}) ->
    w(" GLdouble ~sConv = (double) *~s; \n",[Name,Name]),
    w(" rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) &~sConv;~n", [Name]);
build_ret(Name,_Q,#type{name=_T,size=8,base=float,single=true,by_val=false}) ->
    w(" rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) ~s;~n", [Name]);

build_ret(Name,_Q,#type{name=_T,size=FSz,base=float,single={tuple,Sz}}) ->
    Temp = Name ++ "Tmp",
    case FSz of 
	8 ->	    
	    w(" GLdouble *~s = ~s;\n", [Temp,Name]);
	4 ->
	    w(" GLdouble ~sConv[~p], *~s = ~sConv; \n",[Name,Sz,Temp,Name]),
	    w(" for(int i=0; i < ~p; i++) ~sConv[i] = (GLdouble) ~s[i];\n",[Sz,Name,Name])
    end,
    [w(" rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) ~s++;~n", [Temp]) 
     || _ <- lists:seq(1,Sz)],
    w(" rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = ~p;~n",[Sz]);
build_ret(Name,_Q,#type{name=T,base=_,single={tuple,Sz}}) ->
    Temp = Name ++ "Tmp",
    w(" ~s *~s = ~s;\n", [T,Temp,Name]),
    [w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *~s++;~n", [Temp]) 
     || _ <- lists:seq(1,Sz)],
    w(" rt[AP++] = ERL_DRV_TUPLE; rt[AP++] = ~p;~n",[Sz]);
build_ret(Name,_Q,#type{base=string,size=1,single=true}) ->
    w(" rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) ~s;"
      " rt[AP++] = strlen((char *) ~s);\n", [Name, Name]);
build_ret(Name,_Q,#type{base=string, size={_Max,Sz}, single=S}) 
  when S == true; S == undefined ->
    w(" rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) ~s;"
      " rt[AP++] = *~s;\n", [Name, Sz]);
build_ret(Name,_Q,#type{name=_T,base=string,size={_, SSz}, single={list,_,Sz}}) ->
    P = if Sz == "result" -> ["(int) "]; true -> "*" end,
    w(" for(int i=0; i < ~s~s; i++) {\n", [P,Sz]),
    w("    rt[AP++] = ERL_DRV_STRING; rt[AP++] = (ErlDrvTermData) ~s;"
      " rt[AP++] = ~s[i]-1;\n", [Name, SSz]),
    w("    ~s += ~s[i]; }~n", [Name, SSz]),
    w(" rt[AP++] = ERL_DRV_NIL;", []),
    w(" rt[AP++] = ERL_DRV_LIST; rt[AP++] = (~s~s)+1;~n",[P,Sz]);
build_ret(Name,_Q,#type{name=_T,base=B,single={list,_,Sz}}) when B =/= float ->
    P = if Sz == "result" -> ["(int) "]; true -> "*" end,
    w(" for(int i=0; i < ~s~s; i++) {\n", [P,Sz]),
    w("    rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) ~s[i];}~n", [Name]),    
    w(" rt[AP++] = ERL_DRV_NIL;", []),
    w(" rt[AP++] = ERL_DRV_LIST; rt[AP++] = (~s~s)+1;~n",[P,Sz]);
build_ret(Name,_Q,#type{name=_T,size=FSz,base=float,single={list,Sz}}) ->
    Temp = Name ++ "Tmp",
    case FSz of
	8 -> 
	    w(" GLdouble *~s = ~s;\n", [Temp,Name]);
	4 ->
	    w(" GLdouble ~sConv[~p], *~s = ~sConv; \n",[Name,Sz,Temp,Name]),
	    w(" for(int i=0; i < ~p; i++) ~sConv[i] = (GLdouble) ~s[i];\n",[Sz,Name,Name])
    end,
    [w(" rt[AP++] = ERL_DRV_FLOAT; rt[AP++] = (ErlDrvTermData) ~s++;~n", [Temp]) 
     || _ <- lists:seq(1,Sz)],
    w(" rt[AP++] = ERL_DRV_NIL;", []),
    w(" rt[AP++] = ERL_DRV_LIST; rt[AP++] = ~p+1;~n",[Sz]);
build_ret(Name,_Q,#type{name=T,base=_,single={list,Sz}}) ->
    Temp = Name ++ "Tmp",
    w(" ~s *~s = ~s;\n", [T,Temp,Name]),
    [w(" rt[AP++] = ERL_DRV_INT; rt[AP++] = (ErlDrvSInt) *~s++;~n", [Temp]) 
     || _ <- lists:seq(1,Sz)],
    w(" rt[AP++] = ERL_DRV_NIL;", []),
    w(" rt[AP++] = ERL_DRV_LIST; rt[AP++] = ~p+1;~n",[Sz]);
build_ret(Name,_Q,#type{base=binary,size={_,Sz}}) ->
    w(" rt[AP++] = ERL_DRV_BINARY; rt[AP++] = (ErlDrvTermData) ~s;", [Name]),
    if is_integer(Sz) ->
	    w(" rt[AP++] = ~p; rt[AP++] = 0;~n", [Sz]);
       is_list(Sz) ->
	    w(" rt[AP++] = *~s; rt[AP++] = 0;~n", [Sz])
    end,
    "driver_free_binary(" ++ Name ++ ");";
build_ret(Name,_Q,T=#type{}) ->
    io:format("{~p, {~p, {single,{tuple,X}}}}.~n", [get(current_func),Name]),
    io:format(" ~p~n",[T]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_defines(GLFuncs,GLUFuncs) ->
    open_write("../c_src/gen/gl_fdefs.h"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("#ifdef WX_DEF_EXTS~n", []),
    w("# define WXE_EXTERN~n", []),
    w("#else~n# define WXE_EXTERN extern~n", []),
    w("#endif~n~n", []),

    w("typedef struct {\n"      
      "   const char * name;\n"      
      "   const char * alt;\n"
      "   void * func;\n"
      "} gl_fns_t;\n\n", []),

    GLFirst = case hd(GLFuncs) of 
		  [First|_] when is_list(First) -> get(First);
		  First -> get(First)
	      end,
    w("#define GLE_GL_FUNC_START ~p~n", [GLFirst#func.id]),

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
fdef(#func{alt={has_vector,_,FuncName}}) -> 
    #func{name=Name,type=T,params=As} = get(FuncName),
    w("typedef ~s (APIENTRY * WXE~s)(~s);~n", 
      [fdef_type(T), uppercase_all(Name), fdef_types(As)]),
    w("WXE_EXTERN WXE~s we~s;~n", [uppercase_all(Name), Name]);
fdef(#func{name=Name,type=T,params=As,alt=_Alt}) ->
    w("typedef ~s (APIENTRY * WXE~s)(~s);~n", 
      [fdef_type(T), uppercase_all(Name), fdef_types(As)]),
    w("WXE_EXTERN WXE~s we~s;~n", [uppercase_all(Name), Name]).

fdef_type(void) -> "void";
fdef_type(#type{name=T, mod=Mod, single=true, ref=undefined}) ->
    mod(Mod) ++ T;
fdef_type(#type{name=T, mod=Mod, single={tuple,Sz}, ref=undefined}) ->
    mod(Mod) ++ T ++ " m[" ++ integer_to_list(Sz) ++ "]";
fdef_type(#type{name=T, mod=Mod, ref={pointer,1}}) ->
    mod(Mod) ++ T ++ " *";
fdef_type(#type{name=T, mod=Mod, ref={pointer,2}}) ->
    mod(Mod) ++ T ++ " **".

mod([const]) -> "const ";
mod([]) -> "".
    
fdef_types(As) ->
    args(fun(#arg{type=T}) -> fdef_type(T) end, ",", As).
		 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gl_gen_init(Funcs) ->
    open_write("../c_src/gen/gl_finit.h"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("gl_fns_t gl_fns[] = \n"
      "{\n", []),
    [finits(F) || F <- Funcs],
    w(" { NULL, NULL, NULL}};\n",[]),
    close().

glu_gen_init(Funcs) ->
    open_write("../c_src/gen/glu_finit.h"),
    c_copyright(),
    w("/***** This file is generated do not edit ****/~n~n", []),
    w("static struct {\n"      
      "   const char * name;\n"      
      "   const char * alt;\n"
      "   void * func;\n"
      "} glu_fns[] = \n"
      "{\n", []),
    [finits(F) || F <- Funcs],
    w(" { NULL, NULL, NULL}};\n",[]),
    close().


finits([F1|_Fs]) when is_list(F1) ->
    put(current_func,F1),
    finit(get(F1)),
    erase(current_func);
finits([]) -> ok;
finits(F) ->
    put(current_func,F),
    finit(get(F)),
    erase(current_func).

finit(#func{where=erl}) -> ok;
finit(#func{alt={has_vector,_,FuncName}, ext=Ext}) -> 
    #func{name=Name} = get(FuncName),
    w(" {~p, ~s, &we~s},\n", [Name, ext(Name,Ext), Name]);
finit(#func{name=Name, ext=Ext}) ->
    w(" {~p, ~s, &we~s},\n", [Name, ext(Name,Ext), Name]).

ext(Name, {ext,Ext}) ->
    "\"" ++ Name ++ Ext ++ "\"";
ext(_,_) -> "NULL".




    
