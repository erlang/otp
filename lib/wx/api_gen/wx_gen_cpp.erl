%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
%%% File    : wx_gen_cpp.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description :
%%%
%%% Created : 19 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(wx_gen_cpp).

-include("wx_gen.hrl").

-compile(export_all).

-import(lists, [foldl/3,foldr/3,reverse/1, keysearch/3, map/2, filter/2]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1, uppercase_all/1,
		   open_write/1, close/0, c_copyright/0, w/2, w/1,
		   args/3, strip_name/2]).
-import(wx_gen, [next_id/1]).

gen(Defs) ->
    open_write("../c_src/gen/wxe_derived_dest.h"),
    c_copyright(),
    w("~n/***** This file is generated do not edit ****/~n~n", []),
    gen_derived_dest(Defs),
    close(),

    open_write("../c_src/gen/wxe_funcs.cpp"),
    c_copyright(),
    Res = gen_funcs(Defs),
    close(),

    open_write("../c_src/gen/wxe_macros.h"),
    c_copyright(),
    gen_macros(),
    close(),

    open_write("../c_src/gen/wxe_init.cpp"),
    c_copyright(),
    build_enums(),
    close(),

    build_events(),
    Res.

gen_derived_dest(Defs) ->
    [gen_derived_dest_2(Class) || Class <- Defs],
    ok.

gen_derived_dest_2(C=#class{name=Class, options=Opts}) ->
    ?WTC("gen_derived_dest_2"),
    Derived = is_derived(C),
    TaylorMade = taylormade_class(C),

    if Derived andalso (TaylorMade =:= false) ->
	    case lists:keysearch(ifdef,1,Opts) of
		{value, {ifdef, What}} when is_list(What)-> w("#if ~s~n",[What]);
		{value, {ifdef, What}} when is_atom(What) -> w("#if ~p~n",[What]);
		_ -> ok
	    end,
	    w("class E~s : public ~s {~n",[Class,Class]),
	    case Class of
		"wxGLCanvas" ->  %% Special for cleaning up gl context
		    w(" public: ~~E~s() {deleteActiveGL(this);"
		      "((WxeApp *)wxTheApp)->clearPtr(this);};~n", [Class]);
		_ ->
		    w(" public: ~~E~s() {((WxeApp *)wxTheApp)->clearPtr(this);};~n", [Class])
	    end,
	    gen_constructors(C),
	    case lists:keysearch(ifdef,1,Opts) of
		{value, {ifdef, Endif}} ->
		    w("};~n", []),
		    w("#endif // ~p~n~n",[Endif]);
		_ ->
		    w("};~n~n", [])
	    end;
       TaylorMade /= false ->
	    w("~s~n", [TaylorMade]);
       true ->
	    ignore
    end.

taylormade_class(#class{name=CName, methods=Ms}) ->
    TaylorMade = lists:any(fun([#method{where=taylormade}|_]) -> true;
			      (_) -> false
			   end, Ms),
    case TaylorMade  of
	false -> false;
	true ->
	    {ok, Bin} = file:read_file(filename:join([wx_extra, CName ++".c_src"])),
	    Src = binary_to_list(Bin),
	    case gen_util:get_taylor_made(Src, CName ++ "_class") of
		nomatch ->   false;
		{match, [Str0]} -> Str0
	    end
    end.

gen_constructors(#class{name=Class, methods=Ms0}) ->
    Ms = lists:append(Ms0),
    Cs = lists:filter(fun(#method{method_type=MT}) -> MT =:= constructor end, Ms),
    [gen_constructor(Class, Const) || Const <- Cs],
    case need_copy_constr(Class) of
	true ->
	    w(" E~s(~s copy) : ~s(copy) {};~n", [Class, Class, Class]);
	false ->
	    ignore
    end.
gen_constructor(_Class, #method{where=merged_c}) -> ok;
gen_constructor(_Class, #method{where=erl_no_opt}) -> ok;
gen_constructor(Class, _M=#method{params=Ps, opts=FOpts}) ->
    Gen1  = fun(#param{name=N, type=T}) -> gen_type(T,1) ++ N end,
    Gen2  = fun(#param{name=N, type=T}) -> gen_type(T,2) ++ N end,
    CallA = fun(#param{name=N}) -> N end,
    HaveMergedType = fun(#param{type={merged,_,_,_,_,_,_}}) -> true; (_) -> false end,
    ?WTC("gen_constructor"),
    Endif = case lists:keysearch(deprecated, 1, FOpts) of
		{value, {deprecated, IfDef}} ->
		    w("#if ~s~n", [IfDef]),
		    true;
		_ -> false
	    end,
    case lists:any(HaveMergedType, Ps) of
	false ->
	    w(" E~s(~s) : ~s(~s) {};~n",
	      [Class,args(Gen1,",",Ps),Class,args(CallA,",",Ps)]);
	true ->
	    w(" E~s(~s) : ~s(~s) {};~n",
	      [Class,args(Gen1,",",Ps),Class,args(CallA,",",Ps)]),
	    w(" E~s(~s) : ~s(~s) {};~n",
	      [Class,args(Gen2,",",Ps),Class,args(CallA,",",Ps)])
    end,
    Endif andalso w("#endif~n", []),
    ok.


need_copy_constr("wxFont") -> true;
need_copy_constr("wxIcon") -> true;
need_copy_constr("wxImage") -> true;
need_copy_constr("wxBitmap") -> true;
%%need_copy_constr("wxGraphics" ++ _) -> true;
need_copy_constr(_) -> false.

gen_type(#type{name=Type, ref={pointer,1}, mod=Mod},_) ->
    mods(Mod) ++ to_string(Type) ++ " * ";
gen_type(#type{name=Type, ref={pointer,2}, mod=Mod},_) ->
    mods(Mod) ++ to_string(Type) ++ " ** ";
gen_type(#type{name=Type, ref=reference, mod=Mod},_) ->
    mods(Mod) ++ to_string(Type) ++ "& ";
gen_type(#type{name=Type, ref=undefined, base=binary, mod=Mod},_) ->
    mods(Mod) ++ to_string(Type) ++ " * ";
gen_type(#type{name=Type, ref=undefined, single=array, mod=Mod},_) ->
    mods(Mod) ++ to_string(Type) ++ " * ";
gen_type(#type{name=Type, ref=undefined, mod=Mod},_) ->
    mods(Mod) ++ to_string(Type) ++ " ";
gen_type({merged, _, T1, _,_, _T2,_}, 1) ->
    gen_type(T1,error);
gen_type({merged, _, _T1,_, _, T2,_}, 2) ->
    gen_type(T2,error).

gen_funcs(Defs) ->
    w("~n/***** This file is generated do not edit ****/~n~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n"),
    w("#include \"../wxe_events.h\"~n"),
    w("#include \"../wxe_return.h\"~n"),
    w("#include \"../wxe_gl.h\"~n"),
    w("#include \"wxe_macros.h\"~n"),
    w("#include \"wxe_derived_dest.h\"~n~n"),

    w("#if !wxCHECK_VERSION(2,9,0)~n", []),
    [w("#define ~p int~n", [Enum]) ||
	Enum <- [wxPenJoin, wxPenCap, wxImageResizeQuality, %%wxBitmapType,
		 wxPolygonFillMode, wxMappingMode, wxRasterOperationMode,
		 wxFloodFillStyle
		]],
    w("#endif~n",[]),

    w("void WxeApp::wxe_dispatch(wxeCommand& Ecmd)~n{~n"),
    w(" char * bp = Ecmd.buffer;~n"),
    w(" int op = Ecmd.op;~n"),
    w(" Ecmd.op = -1;~n"),
    w(" wxeMemEnv *memenv = getMemEnv(Ecmd.port);~n"),
%%    w(" wxMBConvUTF32 UTFconverter;~n"),
    w(" wxeReturn rt = wxeReturn(WXE_DRV_PORT, Ecmd.caller, true);~n"),
    w(" try {~n"),
    w(" switch (op)~n{~n"),
%%     w("  case WXE_CREATE_PORT:~n", []),
%%     w("   { newMemEnv(Ecmd.port); } break;~n", []),
%%     w("  case WXE_REMOVE_PORT:~n", []),
%%     w("   { destroyMemEnv(Ecmd.port); } break;~n", []),
    w("  case DESTROY_OBJECT: {~n"),
    w("     void *This = getPtr(bp,memenv);~n"),
    w("     wxeRefData *refd = getRefData(This);~n"),
    w("     if(This && refd) {~n"),
    w("       if(recurse_level > 1 && refd->type != 8) {~n"),
    w("          delayed_delete->Append(Ecmd.Save(op));~n"),
    w("       } else {~n"),
    w("          delete_object(This, refd);~n"),
    w("          ((WxeApp *) wxTheApp)->clearPtr(This);}~n"),
    w("  } } break;~n"),
    w("  case WXE_REGISTER_OBJECT: {~n"
      "     registerPid(bp, Ecmd.caller, memenv);~n"
      "     rt.addAtom(\"ok\");~n"
      "     break;~n"
      " }~n"),
    w(" case WXE_BIN_INCR:~n   driver_binary_inc_refc(Ecmd.bin[0].bin);~n   break;~n",[]),
    w(" case WXE_BIN_DECR:~n   driver_binary_dec_refc(Ecmd.bin[0].bin);~n   break;~n",[]),
    w(" case WXE_INIT_OPENGL:~n  wxe_initOpenGL(&rt, bp);~n   break;~n",[]),

    Res = [gen_class(Class) || Class <- Defs],

    w("  default: {~n"),
    w("    wxeReturn error = wxeReturn(WXE_DRV_PORT, Ecmd.caller, false);"),
    w("    error.addAtom(\"_wxe_error_\");~n"),
    w("    error.addInt((int) op);~n"),
    w("    error.addAtom(\"not_supported\");~n"),
    w("    error.addTupleCount(3);~n"),
    w("    error.send();~n"),
    w("    return ;~n"),
    w("  }~n"),
    w("}  // switch~n"),
    w(" rt.send();~n"),
    w("} catch (wxe_badarg badarg) {  // try~n"),
    w("    wxeReturn error = wxeReturn(WXE_DRV_PORT, Ecmd.caller, false);"),
    w("    error.addAtom(\"_wxe_error_\");~n"),
    w("    error.addInt((int) op);~n"),
    w("    error.addAtom(\"badarg\");~n"),
    w("    error.addInt((int) badarg.ref);~n"),
    w("    error.addTupleCount(2);~n"),
    w("    error.addTupleCount(3);~n"),
    w("    error.send();~n"),
    w("}} /* The End */~n~n~n"),

    UglySkipList = ["wxCaret", "wxCalendarDateAttr",
		    "wxFileDataObject", "wxTextDataObject", "wxBitmapDataObject",
		    "wxAuiSimpleTabArt"
		   ],

    w("bool WxeApp::delete_object(void *ptr, wxeRefData *refd) {~n", []),
    w(" if(wxe_debug) {\n"
      "     wxString msg;\n"
      "	    const wxChar *class_info = wxT(\"unknown\");\n"
      "	    if(refd->type < 10) {\n"
      "		wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();\n"
      "		    class_info = cinfo->GetClassName();\n"
      "	       }\n"
      "      msg.Printf(wxT(\"Deleting {wx_ref, %d, %s} at %p \"), refd->ref, class_info, ptr);\n"
      "      send_msg(\"debug\", &msg);\n"
      " };\n"),

    w(" switch(refd->type) {~n", []),
    w("#if wxUSE_GRAPHICS_CONTEXT~n", []),
    w("  case 4: delete (wxGraphicsObject *) ptr; break;~n", []),
    w("#endif~n", []),
    Case = fun(C=#class{name=Class, id=Id, abstract=IsAbs, parent=P}) when P /= "static" ->
		   UglyWorkaround = lists:member(Class, UglySkipList),
		   HaveVirtual = virtual_dest(C),
		   case hd(reverse(wx_gen_erl:parents(Class))) of
		       root when IsAbs == false, UglyWorkaround == true ->
			   w("  case ~p: /* delete (~s *) ptr;"
			     "These objects must be deleted by owner object */ "
			     "break;~n", [Id, Class]);
		       root when IsAbs == false, HaveVirtual == true ->
			   w("  case ~p: delete (E~s *) ptr; return false;~n", [Id, Class]);
		       root when IsAbs == false, UglyWorkaround == false ->
			   w("  case ~p: delete (~s *) ptr; break;~n", [Id, Class]);
		       _ -> ok
		   end;
	      (_) -> ok
	   end,
    [Case(Class) || Class <- Defs],
    w("  default: delete (wxObject *) ptr; return false;~n", []),
    w("  }~n  return true;~n}~n~n", []),
    Res.

gen_class(C=#class{name=Name,methods=Ms,options=Opts}) ->
    put(current_class, Name),
    NewMs =
	case lists:member(taylormade, Opts) of
	    true ->
		{ok, Bin} = file:read_file(filename:join([wx_extra,Name++".c_src"])),
		?WTC("gen_class"),
		w("~s~n", [binary_to_list(Bin)]),
		Ms;
	    false ->
		case lists:keysearch(ifdef,1,Opts) of
		    {value, {ifdef, What}} ->
			is_atom(What) andalso w("#if ~p~n",[What]),
			is_list(What) andalso w("#if ~s~n",[What]),
			Methods = lists:flatten(Ms),
			MsR = [gen_method(Name,M) ||
				  M <- lists:keysort(#method.id, Methods)],
			w("#endif // ~p~n",[What]),
			MsR;
		    false ->
			Methods = lists:flatten(Ms),
			[gen_method(Name,M) ||
			    M <- lists:keysort(#method.id, Methods)]
		end
	end,
    erase(current_class),
    C#class{methods=NewMs}.

gen_method(_CName, M=#method{where=erl_no_opt}) ->     M;
gen_method(CName, M=#method{where=taylormade, name=Name, id=Id}) ->
    {ok, Bin} = file:read_file(filename:join([wx_extra, CName ++".c_src"])),
    Src = binary_to_list(Bin),
    %%    io:format("C++ Class ~p ~p~n", [CName, Name]),
    Str = case gen_util:get_taylor_made(Src, Name) of
	      nomatch ->
		  {match, [Str0]} = gen_util:get_taylor_made(Src, wx_gen_erl:get_unique_name(Id)),
		  Str0;
	      {match, [Str0]} ->
		  Str0
	  end,
    ?WTC("gen_method"),
    w(Str, [wx_gen_erl:get_unique_name(Id)]),
    M;
gen_method(CName, M=#method{name=N,params=[Ps],method_type=destructor,id=MethodId}) ->
    case hd(reverse(wx_gen_erl:parents(CName))) of
	root ->
	    ?WTC("gen_method"),
	    w("case ~s: { // ~s::~s~n", [wx_gen_erl:get_unique_name(MethodId),CName,N]),
	    decode_arguments([Ps]),
	    w(" if(This) {", []),
	    w("   ((WxeApp *) wxTheApp)->clearPtr((void *) This);~n", []),
	    w("   delete This;}~n", []),
	    free_args(),
	    w(" break;~n}~n", []);
	object ->  %% Use default
	    ignore
    end,
    M;
gen_method(CName,  M=#method{name=N,params=Ps0,type=T,method_type=MT,id=MethodId, opts=FOpts}) ->
    put(current_func, N),
    put(bin_count,-1),
    ?WTC("gen_method"),
    Endif1 = gen_if(deprecated, FOpts),
    Endif2 = gen_if(test_if, FOpts),
    w("case ~s: { // ~s::~s~n", [wx_gen_erl:get_unique_name(MethodId),CName,N]),
    Ps1 = declare_variables(void, Ps0),
    {Ps2,Align} = decode_arguments(Ps1),
    Opts = [Opt || Opt = #param{def=Def,in=In,where=Where} <- Ps2,
		   Def =/= none, In =/= false, Where =/= c],
    decode_options(Opts, Align),
    case gen_util:get_hook(c, M#method.pre_hook) of
	ignore -> skip;
	Pre -> w(" ~s;~n", [Pre])
    end,
    Ps3 = call_wx(N,{MT,CName},T,Ps2),
    case gen_util:get_hook(c, M#method.post_hook) of
	ignore -> skip;
	Post -> w(" ~s;~n", [Post])
    end,
    free_args(),
    build_return_vals(T,Ps3),
    w(" break;~n}~n", []),
    Endif1 andalso w("#endif~n", []),
    Endif2 andalso w("#endif~n", []),
    erase(current_func),
    M.

gen_if(What, Opts) ->
    case lists:keysearch(What, 1, Opts) of
	{value, {What, IfDef}} ->
	    w("#if ~s~n", [IfDef]),
	    true;
	_ -> false
    end.

declare_variables(void,Ps) ->
    [declare_var(P) || P <- Ps];
declare_variables(T, Ps) ->
    declare_type("result", out, ignore, T),
    [declare_var(P) || P <- Ps].

declare_var(P = #param{where=erl}) -> P;
declare_var(P = #param{where=this}) -> P;
declare_var(P = #param{name=Name,def=Def,type=Type,in=true}) when Def =/= none ->
    declare_type(Name, true, Def, Type),
    P;
declare_var(P = #param{in=In}) when In =/= false -> P;
declare_var(P = #param{name=Name,in=In,def=Def,type=Type}) ->
    declare_type(Name, In, Def, Type),
    P.

declare_type(N,false,_,#type{name="wxArrayInt"}) ->
    w(" wxArrayInt ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxArrayDouble"}) ->
    w(" wxArrayDouble ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxArrayString"}) ->
    w(" wxArrayString ~s;~n", [N]);
declare_type(N,false,_,#type{base=Base,single=true,name=Type,by_val=false,mod=Mod})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double ->
    w(" ~s~s ~s;~n", [mods(Mod),Type,N]);
declare_type(N,false,_,#type{base={enum,_},single=true,name=Type,by_val=false,mod=Mod}) ->
    w(" ~s~s ~s;~n", [mods(Mod),Type,N]);
declare_type(N,false,_,#type{name="wxArrayTreeItemIds",ref=reference}) ->
    w(" wxArrayTreeItemIds ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxDateTime"}) ->
    w(" wxDateTime ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxColour"}) ->
    w(" wxColour ~s;~n", [N]);
declare_type(N,false,_,#type{name=Type, base=int, ref=reference}) ->
    w(" ~s ~s;~n", [Type,N]);
declare_type(N,false,_,#type{name=Type, base=int64, ref=reference}) ->
    w(" ~s ~s;~n", [Type,N]);
declare_type(N,false,_,#type{base={comp,_,_},single=true,name=Type,ref=reference}) ->
    w(" ~s ~s;~n", [Type,N]);
declare_type(N,true,Def,#type{base=Base,single=true,name=Type,by_val=true})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double; Base =:= bool ->
    w(" ~s ~s=~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{base={comp,_,_},single=true,name=Type,mod=Mod,ref={pointer,1}}) ->
    w(" ~s~s *~s=~s; ~s ~s;~n", [mods(Mod),Type,N,Def,Type,N++"Tmp"]);
declare_type(N,true,Def,#type{base={comp,_,_},single=true,name=Type,ref=reference}) ->
    w(" ~s ~s= ~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{base={enum,Type},single=true}) ->
    w(" ~s ~s=~s;~n", [enum_type(Type),N,Def]);
declare_type(N,true,Def,#type{base={class,_},single=true,name=Type,ref={pointer,1},mod=Mod}) ->
    w(" ~s~s * ~s=~s;~n", [mods(Mod),Type,N,Def]);
declare_type(N,true,Def,#type{base={class,_},single=true,name=Type,ref=reference,mod=Mod}) ->
    w(" ~s~s * ~s= &~s;~n", [mods(Mod),Type,N,Def]);
declare_type(N,true,Def,#type{base=Base,single=true,name=Type,by_val=false,ref={pointer,1}})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double; Base =:= bool ->
    w(" ~s *~s=~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{single=true,name="wxArtClient"}) ->
    w(" wxArtClient ~s= ~s;~n", [N,Def]);
declare_type(N,true,_Def,#type{name="wxeLocaleC", single=true,base=string}) ->
    w(" wxString ~s= wxEmptyString;~n", [N]);
declare_type(N,true,Def,#type{single=true,base=string}) ->
    w(" wxString ~s= ~s;~n", [N,Def]);
%% declare_type(N,true,_Def,#type{name="wxString"}) ->
%%     w(" wxString ~s= wxEmptyString;~n", [N]);
declare_type(N,true,Def,#type{base=binary, name=char}) ->
    w(" char ~sD[] = {~s}, * ~s = ~sD;~n", [N,Def,N,N]);
declare_type(_N,true,_Def,void) ->
    skip;
declare_type(_N,true,_Def,voidp) ->
    skip;
declare_type(N,true,Def,#type{name=Type, ref={pointer,2}}) ->
    %% xxxx
    w(" ~s ** ~s = ~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{name=Type, single=array, ref={pointer,1}}) ->
    w(" int * ~sLen = 0;~n", [N]),
    w(" ~s * ~s = ~s;~n", [Type,N,Def]);
declare_type(N,true,"",#type{name="wxArrayString", single=array, ref=reference}) ->
    w(" wxArrayString ~s;~n", [N]);
declare_type(N,true,Def,#type{name=Type, base={term,_}}) ->
    w(" ~s * ~s= ~s;~n", [Type,N,Def]);
declare_type(N,In,Def,T) ->
    ?error({unhandled_type, {N,In,Def,T}}).

decode_options([], _Align) -> ok;
decode_options(Opts, Align) ->
    align(Align, 64),
    w(" while( * (int*) bp) { switch (* (int*) bp) {~n", []),
    foldl(fun decode_opt/2, 1, Opts),
    w(" }};~n", []).

decode_opt(#param{name=Name,type=Type}, N) ->
    w("  case ~p: {bp += 4;~n", [N]),
    Align = decode_arg(Name,Type,opt,1),
    align(Align, 64),
    w("  } break;~n", []),
    N+1.

decode_arguments(Ps0) ->
    lists:mapfoldl(fun decode_arg/2,0,Ps0).

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

decode_arg(P = #param{where=erl},A) -> {P,A};
decode_arg(P = #param{where=c},A) ->  {P,A};
decode_arg(P = #param{in=false},A) -> {P,A};
decode_arg(P = #param{def=Def},A) when Def =/= none -> {P,A};
decode_arg(P = #param{name=Name,type=Type},A0) ->
    A = decode_arg(Name, Type, arg, A0),
    {P, A}.

wa(Decl,DA,Get,GetA,arg) ->
    w(Decl,DA),
    w(Get,GetA);
wa(_Decl,_DA,Get,GetA,opt) ->
    w(Get,GetA).

decode_arg(N,#type{name=Class,base={class,_},single=true},Arg,A0) ->
    A = align(A0,32),
    wa(" ~s *",[Class],"~s = (~s *) getPtr(bp,memenv); bp += 4;~n",[N,Class],Arg),
    A;
decode_arg(N,{merged,_,#type{name=Class,base={class,_},single=true},_,_,_,_},arg,A0) ->
    A = align(A0,32),
    w(" ~s * ~s = (~s *) getPtr(bp,memenv); bp += 4;~n", [Class,N,Class]),
    A;
decode_arg(N,#type{base=long,single=true,name=Type},arg,A0) ->
    A = align(A0,64),
    w(" long * ~s = (~s *) bp; bp += 8;~n", [N,Type]),
    A;
decode_arg(N,#type{base=int,single=true,mod=Mod0,name=Type,ref=Ref},Arg,A0) ->
    Mod = mods(Mod0),
    case Arg of
	arg -> w(" ~s~s * ~s = (~s~s *) bp; bp += 4;~n", [Mod,int,N,Mod,int]);
	opt when Ref =:= {pointer,1} ->
	    w(" ~s = (~s *) bp; bp += 4;~n", [N,int]);
	opt ->
	    w(" ~s = (~s)*(~s~s *) bp; bp += 4;~n", [N,Type,Mod,int])
    end,
    align(A0,32);
decode_arg(N,#type{base=float,single=true,name=Type},arg,A0) ->
    w(" ~s * ~s = (~s *) bp; bp += 4;~n", [Type,N,Type]),
    align(A0,32);
decode_arg(N,#type{base=double,single=true,name=Type},Arg,A0) ->
    A = align(A0,64),
    case Arg of
	arg -> w(" ~s * ~s = (~s *) bp; bp += 8;~n", [Type,N,Type]);
	opt -> w(" ~s = * (~s *) bp; bp += 8;~n",    [N,Type])
    end,
    A;
decode_arg(N,#type{base=bool,single=true,name=Type},Arg,A0) ->
    case Arg of
	arg -> w(" bool * ~s = (~s *) bp; bp += 4;~n", [N,Type]);
	opt -> w(" ~s = *(~s *) bp; bp += 4;~n", [N,Type])
    end,
    align(A0,32);
decode_arg(N,#type{base={enum,Type},single=true},Arg,A0) ->
    wa(" ~s ", [enum_type(Type)], "~s = *(~s *) bp; bp += 4;;~n",[N, enum_type(Type)], Arg),
    align(A0,32);
decode_arg(N,#type{base={comp,"wxDateTime",List},single=true,name=Type,ref=Ref},Arg,A0) ->
    Decl = fun({int,Spec}) ->
		   w(" int * ~s~s = (int *) bp; bp += 4;~n", [N,Spec])
	   end,
    align(A0,32),
    lists:foreach(Decl,List),
    Name = fun({_,"Mo"}) -> "(wxDateTime::Month) *"++N++"Mo";
	      ({_,"Y"}) -> "*"++N++"Y";
	      ({_,Spec}) -> "(wxDateTime::wxDateTime_t) *"++N++Spec
	   end,
    case Arg of
	arg -> w(" ~s ~s = ~s(~s);~n", [Type,N,Type,args(Name, ",", List)]);
	opt when Ref =:= {pointer,1} ->
	    w(" ~sTmp = ~s(~s); ~s = & ~sTmp;~n",
	      [N,Type,args(Name, ",", List), N,N]);
	opt ->
	    w(" ~s = ~s(~s);~n", [N,Type,args(Name, ",", List)])
    end,
    (A0+length(List)) rem 2;
decode_arg(N,#type{base={comp,_,List},single=true,name=Type,ref=Ref},Arg,A0) ->
    Decl = fun({int,Spec}) ->
		   w(" int * ~s~s = (int *) bp; bp += 4;~n", [N,Spec]);
	      ({double, Spec}) ->
		   w(" wxDouble * ~s~s = (wxDouble *) bp; bp += 8;~n", [N,Spec])
	   end,
    case hd(List) of
	{int, _} ->    align(A0,32);
	{double, _} -> align(A0,64)
    end,
    lists:foreach(Decl,List),
    Name = fun({_,Spec}) -> "*"++N++Spec end,
    case Arg of
	arg -> w(" ~s ~s = ~s(~s);~n", [Type,N,Type,args(Name, ",", List)]);
	opt when Ref =:= {pointer,1} ->
	    w(" ~sTmp = ~s(~s); ~s = & ~sTmp;~n",
	      [N,Type,args(Name, ",", List), N,N]);
	opt ->
	    w(" ~s = ~s(~s);~n", [N,Type,args(Name, ",", List)])
    end,
    case hd(List) of
	{int, _} ->    (A0+length(List)) rem 2;
	{double, _} -> 0
    end;

decode_arg(N,#type{name=Class="wxTreeItemId",single=true},Arg,A0) ->
    A = align(A0,64),
    wa(" ~s ",[Class],"~s = wxTreeItemId((void *) *(wxUint64 *) bp); bp += 8;~n",[N],Arg),
    A;
decode_arg(N,#type{name=Class="wxTreeItemIdValue",single=true},Arg,A0) ->
    A = align(A0,64),
    wa(" ~s ",[Class],"~s = (~s) * (wxUint64 *) bp; bp += 8;~n",[N,Class],Arg),
    A;
decode_arg(N,#type{name="wxChar", single=S},Arg,A0)
  when S =/= true ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    wa(" wxString", []," ~s = wxString(bp, wxConvUTF8);~n", [N],Arg),
    w(" bp += *~sLen+((8-((~p+ *~sLen) & 7)) & 7);~n", [N,4*((A0+1) rem 2),N]),
    0;
decode_arg(N,#type{base=string, name="wxFileName"},Arg,A0)  ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    wa(" wxString", []," ~sStr = wxString(bp, wxConvUTF8);~n", [N],Arg),
    w(" bp += *~sLen+((8-((~p+ *~sLen) & 7)) & 7);~n", [N,4*((A0+1) rem 2),N]),
    w(" wxFileName ~s = wxFileName(~sStr);~n",[N,N]),
    0;
decode_arg(N,#type{base=string},Arg,A0)  ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    wa(" wxString", []," ~s = wxString(bp, wxConvUTF8);~n", [N],Arg),
    w(" bp += *~sLen+((8-((~p+ *~sLen) & 7)) & 7);~n", [N,4*((A0+1) rem 2),N]),
    0;
decode_arg(N,#type{name="wxArrayString"},Place,A0) ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    case Place of
	arg -> w(" wxArrayString ~s;~n", [N]);
	opt -> ignore %% Already declared
    end,
    w(" int ~sASz = 0, * ~sTemp;~n", [N,N]),
    w(" for(int i=0; i < *~sLen; i++) {~n", [N]),
    w("   ~sTemp = (int *) bp; bp += 4;~n", [N]),
    w("   ~s.Add(wxString(bp, wxConvUTF8));~n", [N]),
    w("   bp += *~sTemp;~n", [N]),
    w("   ~sASz += *~sTemp+4;~n }~n", [N,N]),
    w(" bp += (8-((~p+ ~sASz) & 7 )) & 7;~n", [4*((A0+1) rem 2),N]),
    0;

decode_arg(N,#type{name="wxArrayInt"},arg,A0) ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    w(" wxArrayInt ~s;~n", [N]),
    w(" for(int i=0; i < *~sLen; i++) {", [N]),
    w("  ~s.Add(*(int *) bp); bp += 4;}~n", [N]),
    w(" bp += ((*~sLen + ~p) % 2 )*4;~n",[N, (A0+1)]),
    0;
decode_arg(N,#type{name="wxArrayDouble"},arg,A0) ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    align(A0+1,64),
    w(" wxArrayDouble ~s;~n", [N]),
    w(" for(int i=0; i < *~sLen; i++) {", [N]),
    w("  ~s.Add(*(int *) bp); bp += 4;}~n", [N]),
    0;
decode_arg(_N,#type{base=eventType},_Arg,A0) ->
%%     w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
%%     case Arg of
%% 	arg ->
%% 	    w(" int ~s = wxeEventTypeFromAtom(bp);bp += *~sLen;~n",[N,N]),
%% 	    w(" char *class_name = bp;~n", []),
%% 	    w(" wxeCallbackData * Evt_cb = new wxeCallbackData(Ecmd.caller,This,class_name);~n",
%% 	      [])
%%     end,
    A0;
decode_arg(N,#type{name=Type,base=binary,mod=Mod0},Arg,A0) ->
    Mod = mods([M || M <- Mod0]),
    case Arg of
	arg ->
	    w(" ~s~s * ~s = (~s~s*) Ecmd.bin[~p].base;~n",
	      [Mod,Type,N,Mod,Type, next_id(bin_count)]);
	opt ->
	    w(" ~s = (~s~s*) Ecmd.bin[~p].base;~n",
	      [N,Mod,Type,next_id(bin_count)])
    end,
    A0;
decode_arg(N,#type{base={term,"wxTreeItemData"},mod=Mod0},Arg,A0) ->
    Mod = mods([M || M <- Mod0]),
    Type = "wxETreeItemData",
    BinCnt = next_id(bin_count),
    case Arg of
	arg ->
	    w(" ~s~s * ~s =  new ~s(Ecmd.bin[~p].size, Ecmd.bin[~p].base);~n",
	      [Mod,Type,N,Type,BinCnt,BinCnt]);
	opt ->
	    w(" ~s = new ~s(Ecmd.bin[~p].size, Ecmd.bin[~p].base);~n",
	      [N,Type,BinCnt,BinCnt])
    end,
    A0;
decode_arg(N,#type{name=Type,base={term,_},mod=Mod0},Arg,A0) ->
    Mod = mods([M || M <- Mod0]),
    BinCnt = next_id(bin_count),
    case Arg of
	arg ->
	    w(" ~s~s * ~s =  new ~s(&Ecmd.bin[~p]);~n",
	      [Mod,Type,N,Type,BinCnt]);
	opt ->
	    w(" ~s = new ~s(&Ecmd.bin[~p]);~n",
	      [N,Type,BinCnt])
    end,
    A0;
decode_arg(N,#type{single=array,base=int},Arg,A0)  ->
    case Arg of
	arg ->
	    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
	    w(" int * ~s = (int *) bp; bp += *~sLen*4+((~p+ *~sLen)%2 )*4;~n",
	      [N,N,(A0+1) rem 2,N]);
	opt ->
	    w(" ~sLen = (int *) bp; bp += 4;~n", [N]),
	    w(" ~s = (int *) bp; bp += *~sLen*4+((~p+ *~sLen)%2 )*4;~n",
	      [N,N,(A0+1) rem 2,N])
    end,
    0;
decode_arg(N,#type{by_val=true,single=array,base={comp,Class="wxPoint",_}},arg,A0) ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    w(" ~s *~s;~n",[Class,N]),
    w(" ~s = (~s *) driver_alloc(sizeof(~s) * *~sLen);~n",[N,Class,Class,N]),
    store_free(N),
    w(" for(int i=0; i < *~sLen; i++) {~n", [N]),
    w("   int x = * (int *) bp; bp += 4;~n   int y = * (int *) bp; bp += 4;~n", []),
    w("   ~s[i] = wxPoint(x,y);}~n", [N]),
    align(A0,32);
decode_arg(N,#type{by_val=true,single=array,base={class,Class}},arg,A0) ->
    A = align(A0,32),
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    w(" ~s *~s;~n",[Class,N]),
    w(" ~s = (~s *) driver_alloc(sizeof(~s) * *~sLen);", [N, Class, Class, N]),
    store_free(N),
    w(" for(int i=0; i < *~sLen; i++) {", [N]),
    w(" ~s[i] = * (~s *) getPtr(bp,memenv); bp += 4;}~n", [N,Class]),
    w(" bp += ((~p+ *~sLen)%2 )*4;~n", [A, N]),
    0;
decode_arg(N,#type{name=Type,single=list,base={class,Class}},arg,A0) ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    A = align(A0,32),
    w(" ~s ~s;~n",[Type,N]),
    w(" for(int i=0; i < *~sLen; i++) {", [N]),
    w("  ~s.Append(*(~s *) getPtr(bp,memenv)); bp += 4;}~n", [N,Class]),
    w(" bp += ((~p+ *~sLen)%2 )*4;~n", [A,N]),
    0;
decode_arg(N,#type{single=array,base={comp,Class="wxPoint2DDouble",_}},arg,A0) ->
    w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
    w(" ~s *~s;~n",[Class,N]),
    w(" ~s = (~s *) driver_alloc(sizeof(~s) * *~sLen);~n",[N,Class,Class,N]),
    store_free(N),
    align(A0+1,64),
    w(" for(int i=0; i < *~sLen; i++) {~n", [N]),
    w("   double x = * (double *) bp; bp += 8;~n   double y = * (double *) bp; bp += 8;~n", []),
    w("   ~s[i] = wxPoint2DDouble(x,y);}~n", [N]),
    0;
decode_arg(Name,T, Arg,_A) ->
    ?error({unhandled_type, {Name,T, Arg}}).

align(0, 32) -> 1;
align(1, 32) -> 0;
align(0, 64) -> 0;
align(1, 64) ->
    w(" bp += 4; /* Align */~n"),
    0;
align(N,Sz) ->
    align(N rem 2, Sz).

call_wx(_N,{constructor,_},#type{base={class,RClass}},Ps) ->
    #class{id=Id} = ClassDef = get({class,RClass}),
    Class = case is_derived(ClassDef) of
		true ->  "E" ++ RClass;
		false -> RClass
	    end,
    w(" ~s * Result = new ~s(~s);~n",
      [RClass, Class,args(fun call_arg/1, ",",filter(Ps))]),
    CType = case is_window(RClass) of
		true -> %% Windows have parents that should be deleted first
		    case is_dialog(RClass) of
			true -> 2;   %% Dialogs must be closed first event before windows
			false -> 0
		    end;
		false ->
		    case is_dc(RClass) of
			true -> 8;
			false ->
			    case hd(reverse(wx_gen_erl:parents(RClass))) of
				root -> Id;
				_ -> 1
			    end
		    end
	    end,
    case virtual_dest(ClassDef) orelse (CType =/= 0) of
	true ->
	    w(" newPtr((void *) Result, ~p, memenv);~n", [CType]);
	false ->  %% Hmm window without virt dest
	    w(" /* Possible memory leak here, class is missing virt dest */ \n")
    end,
    Ps;
call_wx(N,{member,_},Type,Ps) ->
    {Beg,End} = return_res(Type),
    w(" if(!This) throw wxe_badarg(0);~n",[]),
    w(" ~sThis->~s(~s)~s;~n",[Beg,N,args(fun call_arg/1, ",",filter(Ps)),End]),
    Ps;
call_wx(N,{static,Class},Type,Ps) ->
    {Beg,End} = return_res(Type),
    #class{parent=Parent} = get({class,Class}),
    case Parent of
	"static" ->
	    w(" ~s::~s(~s)~s;~n",[Beg,N,args(fun call_arg/1, ",",filter(Ps)),End]);
	_ ->
	    w(" ~s~s::~s(~s)~s;~n",[Beg,Class,N,args(fun call_arg/1, ",",filter(Ps)),End])
    end,
    Ps.


return_res(void) -> {"", ""};
return_res(Type = #type{mod=Mod}) ->
    case lists:member(const, Mod) of
	true ->
	    {Beg, End} = return_res1(Type),
 	    {"const " ++ Beg, End};
	_ ->
	    return_res1(Type)
    end.

return_res1(#type{name=Type,ref={pointer,_}, base={term,_}}) ->
    {Type ++ " * Result = (" ++ Type ++ "*)", ""};
return_res1(#type{name=Type,ref={pointer,_}}) ->
    {Type ++ " * Result = (" ++ Type ++ "*)", ""};
return_res1(#type{name=Type,single=true,by_val=false,ref=reference}) ->
    {Type ++ " * Result = &", ""};
return_res1(#type{name=Type,single=true,by_val=true})
  when is_atom(Type) ->
    {atom_to_list(Type) ++ " Result = ", ""};
return_res1(#type{name=Type="wxArrayInt"}) ->
    {Type ++ " Result = ", ""};
return_res1(#type{name=Type,base={class,_},single=list,ref=reference}) ->
    {Type ++ " Result = ", ""};
return_res1(#type{name=Type,base={comp,_,_},single=array,by_val=true}) ->
    {Type ++ " Result = ", ""};
return_res1(#type{name=Type,single=true,by_val=true, base={class, _}}) ->
    case {need_copy_constr(Type), Type} of
	{true, _} ->
	    {Type ++ " * Result = new E" ++ Type ++ "(", "); newPtr((void *) Result,"
	     ++ "3, memenv);"};
	{false, "wxGraphics" ++ _} ->
	    %% {"wxGraphicsObject * Result = new wxGraphicsObject(", "); newPtr((void *) Result,"
	    %%  ++ "3, memenv);"};
	    {Type ++ " * Result = new " ++ Type ++ "(", "); newPtr((void *) Result,"
	     ++ "4, memenv);"};
	{false, _} ->
	    %% Temporary memory leak !!!!!!
	    io:format("~s::~s Building return value of temp ~s~n",
		      [get(current_class),get(current_func),Type]),
	    {Type ++ " * Result = new " ++ Type ++ "(", "); newPtr((void *) Result,"
	     ++ "3, memenv);"}
    end;
return_res1(#type{base={enum,_Type},single=true,by_val=true}) ->
    {"int Result = " , ""};
return_res1(#type{name="wxCharBuffer", base={binary,_},single=true,by_val=true}) ->
    {"char * Result = ", ".data()"};
return_res1(#type{name=Type,single=array,ref=reference}) ->
    {Type ++ " Result = ", ""};
return_res1(#type{name=Type,single=true,by_val=true}) ->
    {Type ++ " Result = ", ""}.

filter(Ps) ->
    lists:filter(fun filter_arg/1, Ps).
filter_arg(#param{where=erl}) -> false;
filter_arg(#param{where=this}) -> false;
filter_arg(#param{}) -> true.
%%filter_arg(#param{def=Def, in=In}) -> Def =:= none orelse In =:= false.


call_arg(#param{where=c, alt={length,Alt}}) when is_list(Alt) ->
    "*" ++ Alt ++ "Len";
call_arg(#param{where=c, alt={size,Id}}) when is_integer(Id) ->
    %% It's a binary
    "Ecmd.bin["++ integer_to_list(Id) ++ "].size";
call_arg(#param{name=N,def=Def,type=#type{by_val=true,single=true,base=Base}})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double; Base =:= bool ->
    case Def of
	none -> "*" ++ N;
	_ ->  N
    end;
call_arg(#param{name=N,type=#type{base={enum,_Type}, by_val=true,single=true}}) ->
    N;
call_arg(#param{name=N,type=#type{base={class,_},by_val=true,single=true}}) -> "*" ++ N;
call_arg(#param{name=N,type=#type{base={class,_},ref=reference,single=true}}) -> "*" ++ N;
call_arg(#param{name=N,type=#type{base=eventType}}) ->
    N ++ ", (wxObjectEventFunction)(wxEventFunction) &WxeApp::handle_evt, Evt_cb, this";
call_arg(#param{name=N,type=#type{by_val=true, single=_False}}) -> N;
call_arg(#param{name=N,def=Def,type=#type{by_val=false, ref={pointer,2}}})
  when Def =/= none -> N;
call_arg(#param{name=N,type=#type{by_val=false, ref={pointer,2}}}) -> "&" ++ N;
call_arg(#param{name=N,in=false,type=#type{ref=reference, single=true}}) -> N;
call_arg(#param{name=N,in=false,type=#type{by_val=false, single=true}}) -> "&" ++ N;
call_arg(#param{name=N,def=Def,type=#type{base={comp,_,_},ref={pointer,1},single=true}})
  when Def =:= none ->
    "&" ++N;
call_arg(#param{name=N,type=#type{base=int, ref=reference, single=true}}) -> "*" ++ N;
call_arg(#param{name=N,type=#type{by_val=false}}) -> N;
call_arg(#param{name=N,type={merged,_,#type{base={class,_},single=true,
					    by_val=ByVal,
					    ref=Ref},_,_,_,_}})
  when ByVal =:= true; Ref =:= reference ->
    "*" ++ N;
call_arg(#param{def=Def, type=void}) when Def =/= none  -> Def;
call_arg(#param{def=Def, type=voidp}) when Def =/= none -> "(void **) " ++ Def;
call_arg(#param{name=N,type=#type{base={ref,_},by_val=true,single=true}}) -> N;
call_arg(#param{name=N,type={merged,_,_,_,_,_,_}}) -> N.

%% call_arg(#param{name=N,type=#type{base=Tuple,ref=reference}})
%%   when is_tuple(Tuple) -> "&" ++ N;

to_string(Type) when is_atom(Type) -> atom_to_list(Type);
to_string(Type) when is_list(Type) -> Type.

virtual_dest(#class{abstract=true, parent=Parent}) ->
    virtual_dest(get_parent_class(Parent));
virtual_dest(#class{methods=Ms, parent=Parent}) ->
    case lists:keysearch(destructor,#method.method_type, lists:append(Ms)) of
	{value, #method{method_type=destructor, virtual=Virtual}} ->
	    case Virtual of
		true -> true;
		_ -> virtual_dest(get_parent_class(Parent))
	    end;
	false -> virtual_dest(get_parent_class(Parent))
    end;
virtual_dest("root") -> false;
virtual_dest("object") -> true.

get_parent_class(Parent) ->
    case get({class, Parent}) of
	undefined -> Parent;
	Class -> Class
    end.

debug(F,A) ->
    case get(debug) of
	true ->  ?warning(F,A);
	_ -> ok
    end.

is_derived(#class{abstract=true}) -> false;
is_derived(C = #class{}) -> virtual_dest(C).

is_window(Class) ->
    lists:member("wxWindow", wx_gen_erl:parents(Class)).

is_dialog(Class) ->
    lists:member("wxDialog", wx_gen_erl:parents(Class)).

is_dc(Class) ->
    Parents = wx_gen_erl:parents(Class),
    lists:member("wxDC", Parents) orelse lists:member("wxGraphicsContext", Parents).

build_return_vals(Type,Ps0) ->
    Ps = [P || P = #param{in=In} <- Ps0, In =/= true],
    HaveType = case Type of  void -> 0; _ -> 1 end,
    NoOut = length(Ps) + HaveType,
    OutTupSz = if NoOut > 1 -> NoOut; true -> 0 end,

    CountFloats = fun(#param{type=#type{base=Float, single=true}}, Acc)
			when Float =:= float; Float =:= double ->
			  Acc + 1;
		     (_, Acc) ->
			  Acc
		  end,
    NofFloats = lists:foldl(CountFloats, 1, Ps),
    case NofFloats > 1 of
	true -> %%io:format("Floats ~p:~p ~p ~n",[get(current_class),get(current_func), NofFloats]);
	    w(" rt.ensureFloatCount(~p);~n",[NofFloats]);
	false -> ignore
    end,
    build_ret_types(Type,Ps),
    if
	OutTupSz > 1 -> w(" rt.addTupleCount(~p);~n",[OutTupSz]);
	true -> ignore
    end,
    Ps.

build_ret_types(void,Ps) ->
    Calc = fun(#param{name=N,in=In,type=T}, Free) ->
		   case build_ret(N, {arg, In}, T) of
		       ok -> Free;
		       Other -> [Other|Free]
		   end
	   end,
    lists:foldl(Calc, [], Ps);
build_ret_types(Type,Ps) ->
    Free = case build_ret("Result", {ret, out}, Type) of
	       ok -> [];
	       FreeStr -> [FreeStr]
	   end,
    Calc = fun(#param{name=N,in=In,type=T}, FreeAcc) ->
		   case build_ret(N, {arg, In}, T) of
		       ok -> FreeAcc;
		       FreeMe -> [FreeMe|FreeAcc]
		   end
	   end,
    lists:foldl(Calc, Free, Ps).

build_ret(Name,_D,#type{base={class,Class},single=true}=_T) ->
    case Class of
        "wxGraphicsContext" ->
            w(" rt.addRef(getRef((void *)~s,memenv,8), \"~s\");~n",[Name,Class]);
        _ ->
            w(" rt.addRef(getRef((void *)~s,memenv), \"~s\");~n",[Name,Class])
    end;
build_ret(Name,_,#type{name="wxTreeItemId",single=true}) ->
    w(" rt.add((wxUIntPtr *) ~s.m_pItem);~n",[Name]);
build_ret(Name,_,#type{name="wxTreeItemIdValue",single=true}) ->
    w(" rt.add((wxUIntPtr *) ~s);~n",[Name]);
build_ret(Name,_,#type{base={term,_},single=true}) ->
    w(" rt.addExt2Term(~s);~n", [Name]);
build_ret(Name,_,#type{base={binary,Size},single=true}) ->
    w(" if(~s) {~n", [Name]),
    w("    rt.addBinary(~s, ~s);~n", [Name,Size]),
    w(" } else {rt.addAtom(\"null\");};~n");
build_ret(Name,_,#type{name="wxUIntPtr", ref={pointer,1}, single=true}) ->
    w(" rt.add(~s);~n", [Name]);
build_ret(Name,_,#type{base={enum,_Type},single=true}) ->
    w(" rt.addInt(~s);~n",[Name]);
build_ret(Name,_,#type{base={comp,_,{record, _}},single=true}) ->
    w(" rt.add(~s);~n", [Name]);
build_ret(Name,{ret,_},#type{base={comp,_,_},single=true, by_val=true}) ->
    w(" rt.add(~s);~n",[Name]);
build_ret(Name,{ret,_},#type{base={comp,_,_},single=true, ref=reference}) ->
    w(" rt.add((*~s));~n",[Name]);
build_ret(Name,_,#type{base={comp,_,_},single=true}) ->
    w(" rt.add(~s);~n",[Name]);
build_ret(Name = "ev->m_scanCode",_,#type{base=bool,single=true,by_val=true}) ->
    %% Hardcoded workaround for 2.9 and later
    w("#if !wxCHECK_VERSION(2,9,0)~n", []),
    w(" rt.addBool(~s);~n",[Name]),
    w("#else~n rt.addBool(false);~n",[]),
    w("#endif~n",[]);
build_ret(Name = "ev->m_metaDown",_,#type{base=bool,single=true,by_val=true}) ->
    %% Hardcoded workaround for MAC on 2.9 and later
    w("#if wxCHECK_VERSION(2,9,0) && defined(_MACOSX)~n", []),
    w(" rt.addBool(ev->m_rawControlDown);~n",[]),
    w("#else~n rt.addBool(~s);~n",[Name]),
    w("#endif~n",[]);

build_ret(Name,_,#type{base=bool,single=true,by_val=true}) ->
    w(" rt.addBool(~s);~n",[Name]);
build_ret(Name,{arg, both},#type{base=int,single=true,mod=M}) ->
    case lists:member(unsigned, M) of
	true ->  w(" rt.addUint(*~s);~n",[Name]);
	false -> w(" rt.addInt(*~s);~n",[Name])
    end;
build_ret(Name,_,#type{base=int,single=true,mod=M}) ->
    case lists:member(unsigned, M) of
	true ->  w(" rt.addUint(~s);~n",[Name]);
	false -> w(" rt.addInt(~s);~n",[Name])
    end;
build_ret(Name,_,#type{name="wxArrayInt"}) ->
    w(" rt.add(~s);~n", [Name]);
build_ret(Name,_,#type{name="wxArrayDouble"}) ->
    w(" rt.add(~s);~n", [Name]);
build_ret(Name,_,#type{base={comp,_,_},single=array}) ->
    w(" for(unsigned int i=0; i < ~s.GetCount(); i++) {~n", [Name]),
    w("  rt.add(~s[i]);~n }~n",[Name]),
    w(" rt.endList(~s.GetCount());~n",[Name]);
build_ret(Name,_,#type{base={class,Class},single=array}) ->
    w(" for(unsigned int i=0; i < ~s.GetCount(); i++) {~n", [Name]),
    w("  rt.addRef(getRef((void *) &~s.Item(i), memenv), \"~s\");~n }~n",[Name, Class]),
    w(" rt.endList(~s.GetCount());~n",[Name]);
build_ret(Name,_,#type{name=List,single=list,base={class,Class}}) ->
    w(" int i=0;~n"),
    w(" for(~s::const_iterator it = ~s.begin(); it != ~s.end(); ++it) {~n",
      [List, Name, Name]),
    w("   ~s * ~sTmp = *it;~n", [Class,Name]),
    w("   rt.addRef(getRef((void *)~sTmp,memenv), \"~s\"); i++;}~n",[Name,Class]),
    w(" rt.endList(~s.GetCount());~n",[Name]);

build_ret(Name,_,#type{name="wxArrayTreeItemIds"}) ->
    w(" for(unsigned int i=0; i < ~s.GetCount(); i++) {~n", [Name]),
    w("    rt.add((wxUIntPtr *)~s[i].m_pItem);}~n",[Name]),
    w(" rt.endList(~s.GetCount());~n",[Name]);

build_ret(Name,_,#type{base=float,single=true}) ->
    w(" rt.addFloat(~s);~n",[Name]);
build_ret(Name,_,#type{base=double,single=true}) ->
    w(" rt.addFloat(~s);~n",[Name]);
build_ret(Name,_,#type{name="wxeLocaleC"}) ->
    w(" rt.add(wxeLocaleC2String(~s));~n",[Name]);
build_ret(Name,_,#type{base=string,single=true}) ->
    w(" rt.add(~s);~n",[Name]);
build_ret(Name,_,#type{name="wxArrayString", single=array}) ->
    w(" rt.add(~s);~n", [Name]);
build_ret(Name,_,#type{name="wxString", single={list,Variable}}) ->
    Obj = case Name of
              "ev->" ++ _ -> "ev";
              _ -> "This"
          end,
    w(" wxArrayString tmpArrayStr(~s->~s, ~s);~n", [Obj,Variable,Name]),
    w(" rt.add(tmpArrayStr);~n", []);
build_ret(Name,In,T) ->
    ?error({nyi, Name,In, T}).

mods([const|R]) -> "const " ++ mods(R);
mods([unsigned|R]) -> "unsigned " ++ mods(R);
mods([]) -> "".

build_enums() ->
    Tree = get(consts),
    w(" /* This file is also generated */~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n"),
    w("#include \"wxe_macros.h\"~n"),
    w("#include \"../wxe_return.h\"~n"),
    w("void WxeApp::init_nonconsts(wxeMemEnv *memenv, ErlDrvTermData caller) {~n"),
    NotConsts = [NC || NC = #const{is_const=false} <- gb_trees:values(Tree)],
    Size = length(NotConsts),
    GVars = get(gvars),
    GSize = length(GVars),
    w("  wxeReturn rt = wxeReturn(WXE_DRV_PORT, caller);~n"),
    w(" rt.addAtom((char*)\"wx_consts\");~n"),
    [build_enum(NConst) || NConst <- lists:keysort(#const.val, NotConsts)],
    _Cnt = foldl(fun(Gvar, I) -> build_gvar(Gvar,I) end, 0, lists:sort(GVars)),
    w(" rt.endList(~p);~n", [Size+GSize]),
    w(" rt.addTupleCount(2);~n"),
    w("  rt.send();~n"),
    w("}~n"),
    ok.

build_enum(#const{name=Name}) ->
    w(" rt.addAtom(\"~s\"); rt.addInt(~s);~n", [Name, Name]),
    w(" rt.addTupleCount(2);~n").

build_gvar({Name, "wxColour", _Id}, Cnt) ->
    w("   rt.addAtom(\"~s\"); rt.add(*(~s));~n",[Name,Name]),
    w("   rt.addTupleCount(2);~n"),
    Cnt;
build_gvar({Name, {address,Class}, _Id}, Cnt) ->
    w("   rt.addAtom(\"~s\"); rt.addRef(getRef((void *)&~s,memenv), \"~s\");~n",[Name,Name,Class]),
    w("   rt.addTupleCount(2);~n"),
    Cnt+1;
build_gvar({Name, {test_if,Test}, _Id}, Cnt) ->
    w("#if ~s~n", [Test]),
    w(" rt.addAtom(\"~s\"); rt.addInt(~s);~n", [Name, Name]),
    w(" rt.addTupleCount(2);~n"),
    w("#else~n", []),
    w(" rt.addAtom(\"~s\"); rt.addAtom(\"undefined\");~n", [Name]),
    w(" rt.addTupleCount(2);~n"),
    w("#endif~n", []),
    Cnt+1;
build_gvar({Name, Class, _Id}, Cnt) ->
    w("   rt.addAtom(\"~s\"); rt.addRef(getRef((void *)~s,memenv),\"~s\");~n",[Name,Name,Class]),
    w("   rt.addTupleCount(2);~n"),
    Cnt+1.

gen_macros() ->
    w("#include <wx/caret.h>~n"),   %% Arrg wxw forgot?? some files
    w("#include <wx/tooltip.h>~n"),
    w("#include <wx/gbsizer.h>~n"),
    w("#include <wx/splash.h>~n"),
    w("#include <wx/grid.h>~n"),
    w("#include <wx/image.h>~n"),
    w("#include <wx/tglbtn.h>~n"),
    w("#include <wx/calctrl.h>~n"),
    w("#include <wx/dirctrl.h>~n"),
    w("#include <wx/listctrl.h>~n"),
    w("#include <wx/treectrl.h>~n"),
    w("#include <wx/spinbutt.h>~n"),
    w("#include <wx/spinctrl.h>~n"),
    w("#include <wx/colordlg.h>~n"),
    w("#include <wx/fdrepdlg.h>~n"),
    w("#include <wx/fontdlg.h>~n"),
    w("#include <wx/progdlg.h>~n"),
    w("#include <wx/printdlg.h>~n"),
    w("#include <wx/display.h>~n"),
    w("#include <wx/dcbuffer.h>~n"),
    w("#include <wx/dcmirror.h>~n"),
    w("#include <wx/glcanvas.h>~n"),
    w("#include <wx/dcps.h>~n"),
    w("#include <wx/xrc/xmlres.h>~n"),
    w("#include <wx/html/htmprint.h>~n"),
    w("#include <wx/stc/stc.h>~n"),
    w("#include <wx/minifram.h>~n"),
    w("#include <wx/sashwin.h>~n"),
    w("#include <wx/laywin.h>~n"),
    w("#include <wx/graphics.h>~n"),
    w("#include <wx/dcgraph.h>~n"),
    w("#include <wx/aui/aui.h>~n"),
    w("#include <wx/datectrl.h>~n"),
    w("#include <wx/filepicker.h>~n"),
    w("#include <wx/fontpicker.h>~n"),
    w("#include <wx/clrpicker.h>~n"),
    w("#include <wx/statline.h>~n"),
    w("#include <wx/clipbrd.h>~n"),
    w("#include <wx/splitter.h>~n"),
    w("#include <wx/choicebk.h>~n"),
    w("#include <wx/toolbook.h>~n"),
    w("#include <wx/listbook.h>~n"),
    w("#include <wx/treebook.h>~n"),
    w("#include <wx/taskbar.h>~n"),
    w("#include <wx/popupwin.h>~n"),
    w("#include <wx/html/htmlwin.h>~n"),
    w("#include <wx/html/htmlcell.h>~n"),
    w("#include <wx/filename.h>~n"),
    w("#include <wx/sysopt.h>~n"),
    w("#include <wx/overlay.h>~n"),

    w("~n~n", []),
    w("#ifndef wxICON_DEFAULT_BITMAP_TYPE~n",[]),
    w("  #define wxICON_DEFAULT_BITMAP_TYPE wxBITMAP_TYPE_ICO_RESOURCE~n",[]),
    w("#endif~n", []),
    w("~n~n", []),

    [w("#define ~s_~s ~p~n", [Class,Name,Id]) ||
	{Class,Name,_,Id} <- wx_gen_erl:get_unique_names()],
    w("~n~n").

build_events() ->
    open_write("../c_src/gen/wxe_events.cpp"),
    c_copyright(),
    w("~n/***** This file is generated do not edit ****/~n~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n~n"),
    w("#include \"wxe_macros.h\"~n"),
    w("#include \"../wxe_events.h\"~n~n"),
    w("#include \"../wxe_return.h\"~n~n"),

    w("wxeEtype::wxeEtype(const char *name, int Id) {eName = name;cID = Id;}~n~n"),
    w("WX_DECLARE_HASH_MAP(int, wxeEtype*, wxIntegerHash, wxIntegerEqual, wxeETmap );~n~n"),

    w("wxeETmap etmap;~n~n"),

    w(
"int wxeEventTypeFromAtom(char *etype_atom) {
  wxeETmap::iterator it;
  for(it = etmap.begin(); it != etmap.end(); ++it) {
       wxeEtype * value = it->second;
       if(strcmp(value->eName, etype_atom) == 0) {
	 if(it->first > wxEVT_USER_FIRST) {
	       return it->first - wxEVT_USER_FIRST;
	    } else {
	       return it->first;
	    }
       }
  }
  return -1;
}

"),

    Evs0 = [C || {_,C=#class{event=Evs}} <- get(), Evs =/= false],
    Evs = lists:keysort(#class.id, Evs0),
    initEventTable(Evs),
    encode_events(Evs),
    close().

initEventTable(Evs) ->
    w("void initEventTable()~n{~n"),
    w("  struct { ",[]),
    w("int ev_type;  int class_id; const char * ev_name;} event_types[] =~n  {~n",[]),

    lists:foreach(fun(Ev) -> init_event_classes(Ev) end,
		  [#class{id=0,event=[wxEVT_NULL]}|Evs]),
    w("   {-1, 0, ""}~n  };~n",[]),
    w("  for(int i=0; event_types[i].ev_type != -1; i++) {~n",[]),
    w("     if(NULL == etmap[event_types[i].ev_type]) {~n",[]),
    w("       etmap[event_types[i].ev_type] =~n"
      "        new wxeEtype(event_types[i].ev_name, event_types[i].class_id);~n"),
    w("     } else {~n",[]),
    w("       wxeEtype *prev = etmap[event_types[i].ev_type];~n"
      "       wxString msg(wxT(\"Duplicate event defs: \"));~n"
      "       msg += wxString::FromAscii(event_types[i].ev_name);~n"
      "       msg += wxString::Format(wxT(\" %d \"), event_types[i].class_id);~n"
      "       msg += wxString::FromAscii(prev->eName);~n"
      "       msg += wxString::Format(wxT(\" %d\"), prev->cID);~n"
      "       send_msg(\"internal_error\", &msg);~n"
      "     }~n"
      "  }~n", []),
    w("}~n~n").

init_event_classes(#class{event=ETs, id=Id}) ->
    F = fun({Eev, Cev, OtherClass}) ->
		w("   {~w + wxEVT_USER_FIRST, ~w, ~p},~n",
		  [Cev, find_id(OtherClass), wx_gen_erl:event_type_name(Eev)]);
	   ({Ev, {test_if, Test}}) ->
		w("#if ~s~n", [Test]),
		w("   {~w, ~w, ~p},~n",
		  [Ev, Id, wx_gen_erl:event_type_name(Ev)]),
		w("#endif~n", []);
	   (Ev) ->
		w("   {~w, ~w, ~p},~n",
		  [Ev, Id, wx_gen_erl:event_type_name(Ev)])
	end,
    [F(ET) || ET <- ETs].

find_id(OtherClass) ->
    Class = get({class,atom_to_list(OtherClass)}),
    %%{value, Class} = lists:keysearch(atom_to_list(OtherClass), #class.name, All),
    Class#class.id.

encode_events(Evs) ->
    ?WTC("encode_events"),
    w("int getRef(void* ptr, wxeMemEnv* memenv)~n"
      "{~n"
      "  WxeApp * app = (WxeApp *) wxTheApp;~n"
      "  return app->getRef(ptr,memenv);~n"
      "}~n~n"),
    w("bool sendevent(wxEvent *event, ErlDrvTermData port)~n{~n"
      " int send_res ;~n"
      " char * evClass = NULL;~n"
      " wxMBConvUTF32 UTFconverter;~n"
      " wxeEtype *Etype = etmap[event->GetEventType()];~n"
      " wxeEvtListener *cb = (wxeEvtListener *)event->m_callbackUserData;~n"
      " WxeApp * app = (WxeApp *) wxTheApp;~n"
      " wxeMemEnv *memenv = app->getMemEnv(port);~n"
      " if(!memenv) return 0;~n~n"
      " wxeReturn rt = wxeReturn(port, cb->listener);~n"),

    w("~n rt.addAtom((char*)\"wx\");~n"
      " rt.addInt((int) event->GetId());~n"
      " rt.addRef(cb->obj, cb->class_name);~n"
      " rt.addExt2Term(cb->user_data);~n"),

    w(" switch(Etype->cID) {~n"),
    lists:foreach(fun(Ev) -> encode_event(Ev) end, Evs),
    w(" }~n~n"),

    w(" rt.addTupleCount(5);~n"),
    w(" if(cb->fun_id) {~n"),
    w("   rt.addRef(getRef((void *)event,memenv), evClass);~n"),
    w("   rt.addTupleCount(2);~n"),
    w("   rt.addInt(cb->fun_id);~n"),
    w("   rt.addAtom(\"_wx_invoke_cb_\");~n"),
    w("   rt.addTupleCount(3);~n"),
    w("   pre_callback();~n"),
    w("   send_res =  rt.send();~n"),
    w("   if(send_res) handle_event_callback(WXE_DRV_PORT_HANDLE, cb->listener);~n"),
    w("   app->clearPtr((void *) event);~n"),
    w(" } else {~n"),
    w("   send_res =  rt.send();~n"),
    w("   if(cb->skip) event->Skip();~n"),
    #class{id=SizeId} = lists:keyfind("wxSizeEvent", #class.name, Evs),
    #class{id=MoveId} = lists:keyfind("wxMoveEvent", #class.name, Evs),
    w("   if(app->recurse_level < 1 && (Etype->cID == ~w || Etype->cID == ~w)) {~n",
      [SizeId, MoveId]),
    w("     app->recurse_level++;~n"),
    w("     app->dispatch_cmds();~n"),
    w("     app->recurse_level--;~n"),
    w("   }~n"),
    w(" };~n"),
    w(" return send_res;~n"),
    w(" }~n").

encode_event(C = #class{name=Class, id=Id, options=Opts}) ->
    ?WTC("encode_event"),
    case proplists:get_value("mixed_event", Opts) of
	undefined ->
	    w("case ~p: {// ~s~n", [Id,Class]),
	    encode_event2(C),
	    ok;
	Mixed ->
	    w("case ~p: {// ~s or ~s~n", [Id,Class,Mixed]),
	    w("  if(event->IsKindOf(CLASSINFO(~s))) {~n",[Class]),
	    encode_event2(C),
	    w("  } else {~n",[]),
	    w("    Etype = etmap[event->GetEventType() + wxEVT_USER_FIRST];~n",[]),
	    encode_event2(get({class,atom_to_list(Mixed)})),
	    w("  }~n",[]),
	    ok
    end,
    w("  break;~n}~n").

encode_event2(Class = #class{name=Name}) ->
    Attrs = build_event_attrs(Class),
    w("    evClass = (char*)\"~s\";~n",[Name]),
    w("    rt.addAtom((char*)\"~s\");~n", [wx_gen_erl:event_rec_name(Name)]),
    w("    rt.addAtom(Etype->eName);~n"),
    build_ret_types(void, Attrs),
    w("    rt.addTupleCount(~p);~n",[length(Attrs) + 2]).

build_event_attrs(ClassRec = #class{name=Class}) ->
    Attrs0 = wx_gen_erl:filter_attrs(ClassRec),
    Rename =
	fun(Att = #param{name=Name,prot=public,acc=undefined}, {All,Use}) ->
		{[Att#param{name= "ev->" ++ Name}|All],Use};
	   (Att = #param{acc=Acc}, {All,_}) ->
		{[Att#param{name= "ev->" ++ Acc}|All], true}
	end,
    case foldr(Rename,{[],false},Attrs0) of
	{[],_} ->  [];
%% 	{Attrs,false} ->
%% 	    w(" ~s ev = dynamic_cast<~s&>(event);~n",[Class,Class]),
%% 	    Attrs;
	{Attrs,_} ->
	    w(" ~s * ev = (~s *) event;~n",[Class,Class]),
	    FixClass =
		fun(P=#param{name=N,acc=Acc,type=#type{single=Single,by_val=ByVal,
						       base={class,C}}})
		   when Acc =/= undefined ->
			Var = var_name(N),
			if Single, ByVal ->
				w(" ~s * ~s = new ~s(~s);~n", [C,Var,C,N]),
				w(" app->newPtr((void *) ~s,3, memenv);~n", [Var]);
			   true ->
				w(" ~s * ~s = ~s;~n", [C,Var,N])
			end,
			P#param{name=Var};
		   (P) -> P
		end,
	    lists:map(FixClass, Attrs)
    end.

var_name("ev->" ++ Name0) ->
    case reverse(Name0) of
	")(" ++ Name -> reverse(Name);
	_ -> Name0
    end;
var_name(Name) -> Name.

enum_name({Class,Type}) ->
    uppercase_all(Class ++ "_" ++ Type);
enum_name(Type) ->
    uppercase_all(Type).


enum_type({Class,Type}) ->
    Class ++ "::" ++ Type;
enum_type(Type) -> Type.
