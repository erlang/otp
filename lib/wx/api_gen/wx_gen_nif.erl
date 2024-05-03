%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-module(wx_gen_nif).

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

    Sorted = lists:sort(fun(#class{name=A}, #class{name=B}) -> A < B end, Defs),
    Split = split(Sorted, "ab;cd;efg;hijkl;mnopqr;s;tuv;wxyz"),

    MakeWrappers = fun(Ds, Id) ->
                           File = io_lib:format("../c_src/gen/wxe_wrapper_~w.cpp",[Id]),
                           open_write(lists:flatten(File)),
                           c_copyright(),
                           Res = gen_funcs(Ds, Id == 1),
                           Id == 1 andalso destructor(Defs),
                           close(),
                           {Res, Id+1}
                   end,
    {Res, 9} = lists:mapfoldl(MakeWrappers, 1, Split),

    open_write("../c_src/gen/wxe_func_table.cpp"),
    c_copyright(),
    gen_func_table(Defs),
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
    lists:append(Res).

split(Defs, GroupString) ->
    Groups = string:split(GroupString, ";", all),
    lists:map(fun(Group) ->
        lists:filter(fun(#class{name=Name}) ->
            C = case string:lowercase(Name) of
                "wx" ++ Rest -> hd(Rest);
                Other -> hd(Other)
            end,
            string:chr(Group, C) > 0
        end, Defs)
    end, Groups).

gen_funcs(Defs, First) ->
    w("~n/***** This file is generated do not edit ****/~n~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n"),
    w("#include \"../wxe_events.h\"~n"),
    w("#include \"../wxe_return.h\"~n"),
    w("#include \"../wxe_gl.h\"~n"),
    w("#include \"wxe_macros.h\"~n"),
    w("#include \"wxe_derived_dest.h\"~n~n"),

    case First of
        true -> hardcoded_taylormade();
        false -> ok
    end,

    Res = [gen_class(Class) || Class <- Defs],
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

destructor(Defs) ->
    UglySkipList = ["wxCaret", "wxCalendarDateAttr",
		    "wxFileDataObject", "wxTextDataObject", "wxBitmapDataObject",
		    "wxAuiSimpleTabArt"
		   ],

    w("~n~n",[]),
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
    w("  }~n  return true;~n}~n~n", []).


hardcoded_taylormade() ->
    w("\n// ::destroy\n"
      "void wxe_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)\n"
      "{\n"
      "   ErlNifEnv *env = Ecmd.env;\n"
      "   ERL_NIF_TERM * argv = Ecmd.args;\n"
      "   void * This = (wxWindow *) memenv->getPtr(env, argv[0], \"This\");\n"
      "   wxeRefData *refd = app->getRefData(This);~n"
      "   if(This && refd) {~n"
      "       if(app->recurse_level > 1 && refd->type != 8) {~n"
      "         Ecmd.op = 50;\n"
      "         app->delayed_delete->Append(&Ecmd);~n"
      "       } else {~n"
      "         app->delete_object(This, refd);~n"
      "         ((WxeApp *) wxTheApp)->clearPtr(This);}~n"
      "  }~n"
      "}~n~n", []),

    w("// wxe_util::registerPid()\n"
      "void wxe_registerPid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)\n"
      "{\n"
      "   ErlNifEnv *env = Ecmd.env;\n"
      "   ERL_NIF_TERM * argv = Ecmd.args;\n"
      "   int index;\n"
      "   if(!enif_get_int(env, argv[0], &index)) Badarg(\"Ref\");\n"
      "   if(app->registerPid(index, Ecmd.caller, memenv)) {\n"
      "      wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);\n"
      "      rt.send(WXE_ATOM_ok);\n"
      "   } else Badarg(\"Ref\");"
      "}\n\n", []).

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
gen_constructor(_Class, #method{where=erl_no_opt}) -> ok;
gen_constructor(_Class, #method{where=erl_alias}) -> ok;
gen_constructor(Class, _M=#method{params=Ps, opts=FOpts}) ->
    Gen  = fun(#param{name=N, type=T}, Id) -> gen_type(T,Id) ++ N end,
    CallA = fun(#param{name=N}) -> N end,
    MergedType = fun(#param{type={merged,L}}) -> length(L); (_) -> 0 end,
    ?WTC("gen_constructor"),
    Endif = case lists:keysearch(deprecated, 1, FOpts) of
		{value, {deprecated, IfDef}} ->
		    w("#if ~s~n", [IfDef]),
		    true;
		_ -> false
	    end,
    case lists:max([0|[MergedType(P) || P <- Ps]]) of
	0 ->
	    w(" E~s(~s) : ~s(~s) {};~n",
              [Class,args(fun(P) -> Gen(P,1) end,",",Ps),Class,args(CallA,",",Ps)]);
	Max ->
            [w(" E~s(~s) : ~s(~s) {};~n",
               [Class,args(fun(P) -> Gen(P,Id) end,",",Ps),Class,args(CallA,",",Ps)])
             || Id <- lists:seq(1,Max)]
    end,
    Endif andalso w("#endif~n", []),
    ok.


%% need_copy_constr("wxFont") -> true;
%% need_copy_constr("wxIcon") -> true;
need_copy_constr("wxImage") -> true;
%% need_copy_constr("wxBitmap") -> true;
%%need_copy_constr("wxGraphics" ++ _) -> true;
need_copy_constr(_) -> false.

gen_type(#type{name=Type, ref={pointer,1}, mod=Mod},_) ->
    mods_u(Mod) ++ to_string(Type) ++ " * ";
gen_type(#type{name=Type, ref={pointer,2}, mod=Mod},_) ->
    mods_u(Mod) ++ to_string(Type) ++ " ** ";
gen_type(#type{name=Type, ref=reference, mod=Mod},_) ->
    mods_u(Mod) ++ to_string(Type) ++ "& ";
gen_type(#type{name=Type, ref=undefined, base=binary, mod=Mod},_) ->
    mods_u(Mod) ++ to_string(Type) ++ " * ";
gen_type(#type{name=Type, ref=undefined, single=array, mod=Mod},_) ->
    mods_u(Mod) ++ to_string(Type) ++ " * ";
gen_type(#type{name=Type, ref=undefined, mod=Mod},_) ->
    mods_u(Mod) ++ to_string(Type) ++ " ";
gen_type({merged, MTs}, N) when is_integer(N) ->
    {_, T, _} = lists:nth(N, MTs),
    gen_type(T,error);
gen_type(voidp, _) ->
    "void ** ".

mods_u(Mod) ->
    %% mods(lists:delete(const, Mod)).
    mods(Mod).

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
gen_method(_CName, M=#method{where=erl_alias}) ->     M;
gen_method(CName, M=#method{where=taylormade, name=Name, id=Id}) ->
    {ok, Bin} = file:read_file(filename:join([wx_extra, CName ++".c_src"])),
    Src = binary_to_list(Bin),
    %% io:format("C++ Class ~p ~p~n", [CName, Name]),
    Str = case gen_util:get_taylor_made(Src, Name) of
	      nomatch ->
                  %% io:format("C++ Class ~p ~p~n", [CName, wx_gen_erl:get_unique_name(Id)]),
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
            w("// ~s::~s~n", [CName,N]),
	    w("void ~s(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)~n",
              [wx_gen_erl:get_unique_name(MethodId)]),
            w("{~n",[]),
            w(" ErlNifEnv *env = Ecmd.env;~n"),
            w(" ERL_NIF_TERM * argv = Ecmd.args;~n"),
	    decode_arguments([Ps]),
	    w(" if(This) {", []),
	    w("   ((WxeApp *) wxTheApp)->clearPtr((void *) This);~n", []),
	    w("   delete This;}~n", []),
	    free_args(),
	    w("}~n~n", []);
	object ->  %% Use default
	    ignore
    end,
    M;
gen_method(CName,  M=#method{name=N,params=Ps0,type=T,method_type=MT,id=MethodId, opts=FOpts}) ->
    put(current_func, N),
    put(current_func_id, MethodId),

    put(bin_count,-1),
    ?WTC("gen_method"),
    Endif1 = gen_if(deprecated, FOpts, true),
    Endif2 = gen_if(test_if, FOpts, true),
    w("// ~s::~s~n", [CName,N]),
    w("void ~s(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)~n",
      [wx_gen_erl:get_unique_name(MethodId)]),
    w("{~n",[]),
    Ps1 = declare_variables(void, Ps0),
    case [P || #param{in=In,where=Where} = P <- Ps1, In =/= false, Where =/= c] of
        [] -> ignore;
        PsIn ->
            needs_env(PsIn) andalso w("  ErlNifEnv *env = Ecmd.env;~n"),
            w("  ERL_NIF_TERM * argv = Ecmd.args;~n")
    end,

    {Ps2,Argc} = decode_arguments(Ps1),
    Opts = [Opt || Opt = #param{def=Def,in=In,where=Where} <- Ps2,
		   Def =/= none, In =/= false, Where =/= c],
    decode_options(Opts, Argc),
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

    w("~n}~n~n", []),
    Endif1 andalso w("#endif~n", []),
    Endif2 andalso w("#endif~n", []),
    erase(current_func),
    M.

gen_if(What, Opts, true) ->
    case lists:keysearch(What, 1, Opts) of
	{value, {What, IfDef}} ->
	    w("#if ~s~n", [IfDef]),
	    true;
	_ ->
            false
    end;
gen_if(What, Opts, false) ->
    case lists:keysearch(What, 1, Opts) of
	{value, {What, IfDef}} ->
            io_lib:format("#if ~s~n", [IfDef]);
	_ -> ""
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
    w("  wxArrayInt ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxArrayDouble"}) ->
    w("  wxArrayDouble ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxArrayString"}) ->
    w("  wxArrayString ~s;~n", [N]);
declare_type(N,false,_,#type{base=Base,single=true,name=Type,by_val=false,mod=Mod})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double ->
    w("  ~s~s ~s;~n", [mods(Mod),Type,N]);
declare_type(N,false,_,#type{base={enum,_},single=true,name=Type,by_val=false,mod=Mod}) ->
    w("  ~s~s ~s;~n", [mods(Mod),Type,N]);
declare_type(N,false,_,#type{name="wxArrayTreeItemIds",ref=reference}) ->
    w("  wxArrayTreeItemIds ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxDateTime"}) ->
    w("  wxDateTime ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxColour"}) ->
    w("  wxColour ~s;~n", [N]);
declare_type(N,false,_,#type{name="wxString"}) ->
    w("  wxString ~s;~n", [N]);
declare_type(N,false,_,#type{name=Type, base=int, ref=reference}) ->
    w("  ~s ~s;~n", [Type,N]);
declare_type(N,false,_,#type{name=Type, base=int64, ref=reference}) ->
    w("  ~s ~s;~n", [Type,N]);
declare_type(N,false,_,#type{base={comp,_,_},single=true,name=Type,ref=reference}) ->
    w("  ~s ~s;~n", [Type,N]);
declare_type(N,true,Def,#type{base=Base,single=true,name=Type,by_val=true,mod=Mod})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double; Base =:= bool ->
    case Type of
        "char" -> w("  ~sint ~s=~s;~n", [mods(Mod),N,Def]);
        _  -> w("  ~s~s ~s=~s;~n", [mods(Mod--[const]),Type,N,Def])
    end;
declare_type(N,true,Def,#type{base={comp,_,_},single=true,name=Type,mod=Mod,ref={pointer,1}}) ->
    w("  ~s~s *~s=~s; ~s ~s;~n", [mods(Mod),Type,N,Def,Type,N++"Tmp"]);
declare_type(N,true,Def,#type{base={comp,_,_},single=true,name=Type,ref=reference}) ->
    w("  ~s ~s= ~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{base={enum,Type},single=true}) ->
    Class = case Type of
                {C,_} -> C++"::";
                _ -> ""
            end,
    w(" ~s ~s=~s~s;~n", [enum_type(Type),N,Class,Def]);
declare_type(N,true,Def,#type{base={class,_},single=true,name=Type,ref={pointer,1},mod=Mod}) ->
    w("  ~s~s * ~s=~s;~n", [mods(Mod),Type,N,Def]);
declare_type(N,true,Def,#type{base={class,_},single=true,name=Type,ref=reference,mod=Mod}) ->
    w("  ~s~s * ~s= &~s;~n", [mods(Mod),Type,N,Def]);
declare_type(N,true,Def,#type{base=Base,single=true,name=Type,by_val=false,ref={pointer,1}})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double; Base =:= bool ->
    w("  ~s *~s=~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{single=true,name="wxArtClient"}) ->
    w("  wxArtClient ~s= ~s;~n", [N,Def]);
declare_type(N,true,_Def,#type{name="wxeLocaleC", single=true,base=string}) ->
    w("  wxString ~s= wxEmptyString;~n", [N]);
declare_type(N,true,Def,#type{single=true,base=string}) ->
    w("  wxString ~s= ~s;~n", [N,Def]);
%% declare_type(N,true,_Def,#type{name="wxString"}) ->
%%     w(" wxString ~s= wxEmptyString;~n", [N]);
declare_type(N,true,Def,#type{name="wxColour"}) ->
    w(" wxColour ~s = ~s;~n", [N, Def]);
declare_type(N,true,Def,#type{base=binary, name=char}) ->
    w("  char ~sD[] = {~s}, * ~s = ~sD;~n", [N,Def,N,N]);
declare_type(_N,true,_Def,void) ->
    skip;
declare_type(_N,true,_Def,voidp) ->
    skip;
declare_type(N,true,Def,#type{name=Type, ref={pointer,2}}) ->
    %% xxxx
    w("  ~s ** ~s = ~s;~n", [Type,N,Def]);
declare_type(N,true,Def,#type{base=Int, name=Type, single=array, ref={pointer,1}}) ->
    w("  unsigned int ~sLen;~n", [N]),
    if Int =:= int; Int =:= int64 ->
            w("  std::vector <~w> ~s;~n", [Int, N]);
       true ->
            w("  ~s * ~s = ~s;~n", [Type,N,Def])
    end;
declare_type(N,true,"",#type{name="wxArrayString", single=array, ref=reference}) ->
    w("  wxArrayString ~s;~n", [N]);
declare_type(N,true,Def,#type{name=Type, base={term,_}}) ->
    w("  ~s * ~s= ~s;~n", [Type,N,Def]);
declare_type(N,In,Def,T) ->
    ?error({unhandled_type, {N,In,Def,T}}).

decode_options([], _) -> ok;
decode_options(Opts, Argc) ->
    w("  ERL_NIF_TERM lstHead, lstTail;~n", []),
    w("  lstTail = argv[~w];~n",[Argc]),
    w("  if(!enif_is_list(env, lstTail)) ~s;~n", [badarg("Options")]),
    w("  const ERL_NIF_TERM *tpl;~n", []),
    w("  int tpl_sz;~n", []),
    w("  while(!enif_is_empty_list(env, lstTail)) {~n", []),
    w("    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) ~s;~n",[badarg("Options")]),
    w("    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) ~s;~n",
      [badarg("Options")]),
    lists:map(fun decode_opt/1, Opts),
    w("       ~s;~n",[badarg("Options")]),
    w("  };~n", []).

decode_opt(#param{name=Name,type=Type}) ->
    w("    if(enif_is_identical(tpl[0], enif_make_atom(env, \"~s\"))) {~n",
      [wx_gen_erl:erl_option_name(Name)]),
    decode_arg(Name,Type,opt,"tpl[1]"),
    w("    } else ",[]).

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
            [w("  enif_free(~s);~n", [Arg]) || Arg <- List]
    end.

decode_arg(P = #param{where=erl},N) -> {P,N+1};
decode_arg(P = #param{where=c}, Acc) ->  {P,Acc};
decode_arg(P = #param{in=false}, Acc) -> {P,Acc};
decode_arg(P = #param{def=Def},Acc) when Def =/= none -> {P,Acc};
decode_arg(P = #param{name=Name,type=Type},N) ->
    _A = decode_arg(Name, Type, arg, io_lib:format("argv[~w]", [N])),
    {P, N+1}.

wa(_Str, _Arg, opt) -> ok;
wa(Str, Args, arg) -> w(Str, Args).

badarg(Arg) ->
    io_lib:format("Badarg(\"~s\")",[Arg]).

decode_arg(N,#type{name=Class,base={class,_},single=true}, Arg,Argc) ->
    wa("  ~s *~s;~n",[Class, N], Arg),
    w("  ~s = (~s *) memenv->getPtr(env, ~s, \"~s\");~n", [N,Class,Argc,N]);
decode_arg(N,{merged,[{_, #type{base={class,_},single=true},_}|_]},arg,Argc) ->
    w("  ERL_NIF_TERM ~s_type;~n", [N]),
    w("  void * ~s = memenv->getPtr(env, ~s, \"~s\", &~s_type);~n", [N,Argc,N,N]);
decode_arg(N,#type{base=int,name=long,single=true},Arg,Argc) ->
    wa("  long ~s;~n",[N], Arg),
    w("  if(!enif_get_long(env, ~s, &~s)) ~s;~n", [Argc, N, badarg(N)]);
decode_arg(N,#type{base=int,name="size_t",single=true},Arg,Argc) ->
    wa("  size_t ~s;~n",[N], Arg),
    w("  if(!wxe_get_size_t(env, ~s, &~s)) ~s;~n", [Argc, N, badarg(N)]);
decode_arg(N,#type{base=int,single=true,mod=Mod0,ref=_Ref,name=WXN},Arg,Argc) ->
    case lists:member(unsigned, Mod0) of
        true ->
            wa("  unsigned int ~s;~n",[N], Arg),
            w("  if(!enif_get_uint(env, ~s, &~s)) ~s;~n", [Argc, N, badarg(N)]);
        false ->
            wa("  int ~s;~n",[N], Arg),
            w("  if(!enif_get_int(env, ~s, &~s)) ~s; // ~s~n", [Argc, N, badarg(N), WXN])
    end;
decode_arg(N,#type{base=float,single=true}, Arg, Argc) ->
    wa("  float ~s;~n",[N], Arg),
    w("  if(!wxe_get_float(env, ~s, &~s)) ~s;~n", [Argc, N, badarg(N)]);
decode_arg(N,#type{base=double,single=true},Arg,Argc) ->
    wa("  double ~s;~n",[N], Arg),
    w("  if(!wxe_get_double(env, ~s, &~s)) ~s;~n", [Argc, N, badarg(N)]);
decode_arg(N,#type{base=bool,single=true},Arg,Argc) ->
    wa("  bool ~s;~n", [N], Arg),
    w("  ~s = enif_is_identical(~s, WXE_ATOM_true);~n", [N, Argc]);
decode_arg(N,#type{base={enum,Type},single=true},Arg,Argc) ->
    case Type of
        [_|_] -> wa("  ~s ~s;~n",[Type, N], Arg);
        {Class,Enum} -> wa("  ~s::~s ~s;~n",[Class,Enum, N], Arg)
    end,
    w("  if(!enif_get_int(env, ~s, (int *) &~s)) ~s; // enum~n", [Argc, N, badarg(N)]);
decode_arg(N,#type{base={comp,"wxDateTime",List},single=true,name=Type,ref=Ref},Arg,Argc) ->
    w("  const ERL_NIF_TERM *~s_t;~n", [N]),
    w("  int ~s_sz;~n", [N]),
    w("  if(!enif_get_tuple(env, ~s, &~s_sz, &~s_t)) ~s;~n", [Argc,N,N,badarg(N)]),
    Decl = fun({int,Spec},Idx) ->
                   w("  int ~s~s;~n", [N,Spec]),
                   w("  if(!enif_get_int(env, ~s_t[~w], &~s~s)) ~s;~n", [N,Idx,N,Spec,badarg(N)]),
                   Idx+1
	   end,
    lists:foldl(Decl,0,List),
    Name = fun({_,"Mo"}) -> "(wxDateTime::Month) ("++N++"Mo-1)";
	      ({_,"Y"}) -> ""++N++"Y";
	      ({_,Spec}) -> "(wxDateTime::wxDateTime_t) "++N++Spec
	   end,
    case Arg of
	arg -> w(" ~s ~s = ~s(~s);~n", [Type,N,Type,args(Name, ",", List)]);
	opt when Ref =:= {pointer,1} ->
	    w(" ~sTmp = ~s(~s); ~s = & ~sTmp;~n",
	      [N,Type,args(Name, ",", List), N,N]);
	opt ->
	    w(" ~s = ~s(~s);~n", [N,Type,args(Name, ",", List)])
    end;
decode_arg(N,#type{base={comp,_,List},single=true,name=Type,ref=Ref},Arg,Argc) ->
    w("  const ERL_NIF_TERM *~s_t;~n", [N]),
    w("  int ~s_sz;~n", [N]),
    w("  if(!enif_get_tuple(env, ~s, &~s_sz, &~s_t)) ~s;~n", [Argc,N,N,badarg(N)]),
    Decl = fun({int,Spec}, Idx) ->
                   w("  int ~s~s;~n", [N,Spec]),
                   w("  if(!enif_get_int(env, ~s_t[~w], &~s~s)) ~s;~n", [N,Idx,N,Spec,badarg(N)]),
                   Idx+1;
              ({double, Spec}, Idx) ->
                   w("  double ~s~s;~n", [N,Spec]),
                   w("  if(!wxe_get_double(env, ~s_t[~w], &~s~s)) ~s;~n", [N,Idx,N,Spec,badarg(N)]),
                   Idx+1
	   end,
    _ = lists:foldl(Decl,0,List),
    Name = fun({_,Spec}) -> N++Spec end,
    case Arg of
	arg -> w("  ~s ~s = ~s(~s);~n", [Type,N,Type,args(Name, ",", List)]);
	opt when Ref =:= {pointer,1} ->
	    w("  ~sTmp = ~s(~s); ~s = & ~sTmp;~n",
	      [N,Type,args(Name, ",", List), N,N]);
	opt ->
	    w("  ~s = ~s(~s);~n", [N,Type,args(Name, ",", List)])
    end;

decode_arg(N,#type{name="wxTreeItemId",single=true},arg,Argc) ->
    w("  ErlNifUInt64 ~s_tmp;~n",[N]),
    w("  if(!enif_get_uint64(env, ~s, &~s_tmp)) ~s;~n", [Argc,N,badarg(N)]),
    w("  wxTreeItemId ~s = wxTreeItemId((void *) (wxUint64) ~s_tmp);~n",[N,N]);

decode_arg(N,#type{name="wxTreeItemIdValue",single=true},arg,Argc) ->
    w("  ErlNifUInt64 ~s_tmp;~n",[N]),
    w("  if(!enif_get_uint64(env, ~s, &~s_tmp)) ~s;~n", [Argc,N,badarg(N)]),
    w("  wxTreeItemIdValue ~s = (wxTreeItemIdValue) ~s_tmp;~n",[N, N]);

%% decode_arg(N,#type{name="wxChar", single=S},Arg,A0)
%%   when S =/= true ->
%%     w(" int * ~sLen = (int *) bp; bp += 4;~n", [N]),
%%     wa(" wxString", []," ~s = wxString(bp, wxConvUTF8);~n", [N],Arg),
%%     w(" bp += *~sLen+((8-((~p+ *~sLen) & 7)) & 7);~n", [N,4*((A0+1) rem 2),N]),
%%     0;
decode_arg(N,#type{base=string, name="wxFileName"}, Arg,Argc)  ->
    w("  ErlNifBinary ~s_bin;~n",[N]),
    w("  wxString ~sStr;~n",[N]),
    w("  if(!enif_inspect_binary(env, ~s, &~s_bin)) ~s;~n",[Argc, N, badarg(N)]),
    w("  ~sStr = wxString(~s_bin.data, wxConvUTF8, ~s_bin.size);~n", [N, N, N]),
    wa("  wxFileName", [], Arg),
    w("  ~s = wxFileName(~sStr);~n",[N,N]);
decode_arg(N,#type{base=string},Arg,Argc)  ->
    w("  ErlNifBinary ~s_bin;~n",[N]),
    wa("  wxString ~s;~n",[N], Arg),
    w("  if(!enif_inspect_binary(env, ~s, &~s_bin)) ~s;~n",[Argc, N, badarg(N)]),
    w("  ~s = wxString(~s_bin.data, wxConvUTF8, ~s_bin.size);~n", [N, N, N]);
    %% wa(" wxString", []," ~s = wxString(bp, wxConvUTF8);~n", [N],Arg),

decode_arg(N,#type{name="wxArrayString"},Arg,Argc) ->
    w("  ERL_NIF_TERM ~sHead, ~sTail;~n", [N,N]),
    w("  ErlNifBinary ~s_bin;~n",[N]),
    wa("  wxArrayString ~s;~n", [N], Arg),
    w("  ~sTail = ~s;~n",[N,Argc]),
    w("  while(!enif_is_empty_list(env, ~sTail)) {~n", [N]),
    w("    if(!enif_get_list_cell(env, ~sTail, &~sHead, &~sTail)) ~s;~n",[N,N,N,badarg(N)]),
    w("    if(!enif_inspect_binary(env, ~sHead, &~s_bin)) ~s;~n",[N, N, badarg(N)]),
    w("    ~s.Add(wxString(~s_bin.data, wxConvUTF8, ~s_bin.size));~n", [N,N,N]),
    w("  };~n",[]);

decode_arg(N,#type{name="wxArrayInt"},_arg,Argc) ->
    w("  wxArrayInt ~s;~n",[N]),
    w("  int ~s_tmp;~n",[N]),
    w("  ERL_NIF_TERM ~sHead, ~sTail;~n", [N,N]),
    w("  ~sTail = ~s;~n",[N,Argc]),
    w("  while(!enif_is_empty_list(env, ~sTail)) {~n", [N]),
    w("    if(!enif_get_list_cell(env, ~sTail, &~sHead, &~sTail)) ~s;~n",[N,N,N,badarg(N)]),
    w("    if(!enif_get_int(env, ~sHead, &~s_tmp)) ~s;~n", [N,N,badarg(N)]),
    w("    ~s.Add(~s_tmp);~n",[N,N]),
    w("  };~n",[]);

decode_arg(N,#type{name=Type,base=binary,mod=Mod0,by_val=Copy},Arg,Argc) ->
    Mod = mods(Mod0),
    wa("  ~s~s * ~s;~n",[Mod,Type,N], Arg),
    w("  ErlNifBinary ~s_bin;~n",[N]),
    w("  if(!enif_inspect_binary(env, ~s, &~s_bin)) ~s;~n",[Argc, N, badarg(N)]),
    case Copy of
        copy ->
            w("  ~s = (unsigned char *) malloc(~s_bin.size);\n", [N,N]),
            w("  memcpy(~s,~s_bin.data,~s_bin.size);\n", [N,N,N]);
        _ ->
            w("  ~s = (~s~s*) ~s_bin.data;~n", [N,Mod,Type,N])
    end;

decode_arg(N,#type{name=Type,base={term,_},single=Single,mod=Mod0},Arg,Argc) ->
    case Single of
        true ->
            Mod = mods(Mod0),
            wa("  ~s~s * ~s;~n",[Mod,Type,N], Arg),
            w("  ~s = new ~s(~s);~n", [N,Type,Argc]);
        _ ->
            w("  unsigned int ~sLen;~n",[N]),
            w("  if(!enif_get_list_length(env, ~s, &~sLen)) ~s;~n", [Argc,N,badarg(N)]),
            wa("  std::vector <~s *> ~s;~n", [Type, N], Arg),
            w("  ERL_NIF_TERM ~sHead, ~sTail;~n", [N,N]),
            w("  ~sTail = ~s;~n",[N,Argc]),
            w("  while(!enif_is_empty_list(env, ~sTail)) {~n", [N]),
            w("    if(!enif_get_list_cell(env, ~sTail, &~sHead, &~sTail)) ~s;~n",[N,N,N,badarg(N)]),
            w("    ~s.push_back(new ~s(~s));~n", [N,Type,Argc]),
            w("  };~n",[])
    end;

decode_arg(N,#type{single=array,base=Int,name=Type},Arg,Argc)
  when Int =:= int; Int =:= int64 ->
    w("  ~w ~s_tmp;~n",[Int, N]),
    wa("  unsigned int ~sLen;~n",[N], Arg),
    w("  ERL_NIF_TERM ~sHead, ~sTail;~n", [N,N]),
    w("  if(!enif_get_list_length(env, ~s, &~sLen)) ~s;~n", [Argc,N,badarg(N)]),
    wa("  std::vector <~w> ~s;~n", [Int, N], Arg),
    w("  ~sTail = ~s;~n",[N,Argc]),
    w("  while(!enif_is_empty_list(env, ~sTail)) {~n", [N]),
    w("    if(!enif_get_list_cell(env, ~sTail, &~sHead, &~sTail)) ~s;~n",[N,N,N,badarg(N)]),
    case Int of
        int -> w("    if(!enif_get_int(env, ~sHead, &~s_tmp)) ~s;~n", [N,N,badarg(N)]);
        int64 -> w("    if(!enif_get_int64(env, ~sHead, &~s_tmp)) ~s;~n", [N,N,badarg(N)])
    end,
    w("    ~s.push_back( (~s) ~s_tmp);~n", [N,Type,N]),
    w("  };~n",[]);

decode_arg(N,#type{single=array, name="wxPoint"++_=Class,
                   base={comp,_,[{Type,_}|_]}},
           arg, Argc) ->
    w("  unsigned ~sLen;~n",[N]),
    w("  ERL_NIF_TERM ~sHead, ~sTail;~n", [N,N]),
    w("  const ERL_NIF_TERM *~s_tpl;~n", [N]),
    w("  int ~s_tsz;~n", [N]),
    w("  if(!enif_get_list_length(env, ~s, &~sLen)) ~s;~n", [Argc,N,badarg(N)]),
    w("  std::vector <~s> ~s;~n", [Class, N]),
    w("  ~w x, y;~n",[Type]),
    w("  ~sTail = ~s;~n",[N,Argc]),
    w("  while(!enif_is_empty_list(env, ~sTail)) {~n", [N]),
    w("    if(!enif_get_list_cell(env, ~sTail, &~sHead, &~sTail)) ~s;~n",[N,N,N,badarg(N)]),
    w("    if(!enif_get_tuple(env, ~sHead, &~s_tsz, &~s_tpl) || ~s_tsz != 2) ~s;~n", [N,N,N,N,badarg(N)]),
    case Class of
        "wxPoint" ->
            w("    if(!enif_get_int(env, ~s_tpl[0], &x)) ~s;~n", [N,badarg(N)]),
            w("    if(!enif_get_int(env, ~s_tpl[1], &y)) ~s;~n", [N,badarg(N)]);
        "wxPoint2DDouble" ->
            w("    if(!wxe_get_double(env, ~s_tpl[0], &x)) ~s;~n", [N,badarg(N)]),
            w("    if(!wxe_get_double(env, ~s_tpl[1], &y)) ~s;~n", [N,badarg(N)])
    end,
    w("    ~s.push_back(~s(x,y));~n", [N, Class]),
    w("  };~n",[]);

decode_arg(N,#type{by_val=true,single=array,base={class,Class}},arg,Argc) ->
    w("  unsigned ~sLen;~n",[N]),
    w("  ERL_NIF_TERM ~sHead, ~sTail;~n", [N,N]),
    w("  if(!enif_get_list_length(env, ~s, &~sLen)) ~s;~n", [Argc,N,badarg(N)]),
    w("  std::vector <~s> ~s;~n",[Class, N]),
    w("  ~sTail = ~s;~n",[N,Argc]),
    w("  while(!enif_is_empty_list(env, ~sTail)) {~n", [N]),
    w("    if(!enif_get_list_cell(env, ~sTail, &~sHead, &~sTail)) ~s;~n",[N,N,N,badarg(N)]),
    w("    ~s.push_back(* (~s *) memenv->getPtr(env, ~sHead,\"~s\"));~n", [N,Class,N,N]),
    w("  };~n",[]);
decode_arg(Name,T, Arg,_A) ->
    ?error({unhandled_type, {Name,T, Arg}}).

call_wx(_N,{constructor,_},#type{base={class,RClass}},Ps) ->
    #class{id=Id} = ClassDef = get({class,RClass}),
    Class = case is_derived(ClassDef) of
		true ->  "E" ++ RClass;
		false -> RClass
	    end,

    case [P || #param{type={merged,_}}=P <- Ps] of
        [] ->
            w("  ~s * Result = new ~s(~s);~n",
              [RClass, Class,args(fun call_arg/1, ",",filter(Ps))]);
        MergedPs ->
            PPs = patch_params(MergedPs, Ps),
            w("  ~s * Result;~n  ", [RClass]),
            CallIf = fun({Test, Ps1}) ->
                             io_lib:format("if(~s)~n    Result = new ~s(~s);~n",
                                           [Test, Class, args(fun call_arg/1, ",",Ps1)])
                     end,
            w("~s", [args(CallIf, "  else ", PPs)]),
            w("  else throw wxe_badarg(~p);~n",[(hd(MergedPs))#param.name])
    end,

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
	    w("  app->newPtr((void *) Result, ~p, memenv);~n", [CType]);
	false ->  %% Hmm window without virt dest
	    w(" /* Possible memory leak here, class is missing virt dest */ \n")
    end,
    Ps;
call_wx(N,{member,_},Type,Ps0) ->
    {Beg,End} = return_res(Type),
    w("  if(!This) throw wxe_badarg(\"This\");~n",[]),
    Ps = filter(Ps0),
    case [P || #param{type={merged,_}}=P <- Ps] of
        [] ->
            w("  ~sThis->~s(~s)~s;~n",[Beg,N,args(fun call_arg/1, ",",Ps),End]);
        MergedPs ->
            PPs = patch_params(MergedPs, Ps),
            [Alloc, Create] = split_alloc(Beg),
            Alloc =:= "" orelse w("  ~s;~n", [Alloc]),
            w("  "),
            CallIf = fun({Test, Ps1}) ->
                             io_lib:format("if(~s)~n   ~sThis->~s(~s)~s;~n",
                                           [Test, Create, N, args(fun call_arg/1, ",",Ps1),End])
                     end,
            w("~s", [args(CallIf, "  else ", PPs)]),
            w("  else throw wxe_badarg(~p);~n",[(hd(MergedPs))#param.name])
    end,
    Ps;
call_wx(N,{static,Class},Type,Ps) ->
    {Beg,End} = return_res(Type),
    #class{parent=Parent} = get({class,Class}),
    case [P || #param{type={merged,_}}=P <- Ps] of
        [] when Parent =:= "static" ->
	    w("  ~s::~s(~s)~s;~n",[Beg,N,args(fun call_arg/1, ",",filter(Ps)),End]);
	[] ->
	    w("  ~s~s::~s(~s)~s;~n",[Beg,Class,N,args(fun call_arg/1, ",",filter(Ps)),End]);
        MergedPs when Parent =/= "static" ->
            [Alloc, Create] = split_alloc(Beg),
            PPs = patch_params(MergedPs, Ps),
            w("  ~s;~n  ", [Alloc]),

            CallIf = fun({Test, Ps1}) ->
                             io_lib:format("  if(~s)~n    ~s~s::~s(~s)~s;~n",
                                           [Test, Create, Class, N, args(fun call_arg/1, ",",Ps1),End])
                     end,
            w("~s", [args(CallIf, "  else ", PPs)]),
            w("  else throw wxe_badarg(~p);~n",[(hd(MergedPs))#param.name])
    end,
    Ps.

patch_params(Merged0, Orig) ->
    MergedTs = [MPs || #param{type={merged,MPs}} <- Merged0],
    PPTss = patch_params_types(MergedTs, Orig),
    Test = fun(#param{name=V, type={static_cast, #type{base={class, Class}}}}) ->
                   "enif_is_identical(" ++ V ++ "_type, WXE_ATOM_" ++ Class ++ ")"
           end,
    [{args(Test, " && ", [P || #param{type={static_cast, _}} = P <- PPTs]),PPTs}
     || PPTs <- PPTss].

patch_params_types([[]|_], _) ->
    [];
patch_params_types(MTs, Orig) ->
    Heads = [H || [H|_] <- MTs],
    Tails = [T || [_|T] <- MTs],
    PPT = patch_params_types_1(Orig, Heads),
    [PPT | patch_params_types(Tails, Orig)].

patch_params_types_1([#param{type={merged,_}}=P0|Ps], [{_,MT,_}|MTs]) ->
    P = P0#param{type={static_cast, MT}},
    [P | patch_params_types_1(Ps, MTs)];
patch_params_types_1([P|Ps],MTs) ->
    [P | patch_params_types_1(Ps, MTs)];
patch_params_types_1([], []) ->
    [].


split_alloc("") -> ["",""];
split_alloc(Beg) ->
    [Decl, Create] = string:split(Beg, " ="),
    [Decl, "Result = " ++ Create].

needs_env(Ps) ->
    Filter = fun(#param{def=Def}) when Def =/= none -> true;
                (#param{type=#type{base=bool}}) -> false;
                (_) -> true
             end,
    lists:any(Filter, Ps).

needs_memenv(MT,T,Ps) ->
    Filter = fun(#param{in=In}) when In =/= true ->      false;
                (#param{type=#type{base={class,_}}}) ->  false;
                (_) -> true
             end,
    NoMemEnv = lists:all(Filter, Ps),
    case {MT,T,NoMemEnv} of
        {static, void, true} -> false;
        _ -> true
    end.

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
    case Type of
        "wxString" -> {Type ++ " Result = ", ""};
        _ -> {Type ++ " * Result = &", ""}
    end;
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
	    {Type ++ " * Result = new E" ++ Type ++ "(", "); app->newPtr((void *) Result,"
	     ++ "3, memenv);"};
	{false, "wxGraphics" ++ _} ->
	    %% {"wxGraphicsObject * Result = new wxGraphicsObject(", "); newPtr((void *) Result,"
	    %%  ++ "3, memenv);"};
	    {Type ++ " * Result = new " ++ Type ++ "(", "); app->newPtr((void *) Result,"
	     ++ "4, memenv);"};
	{false, _} ->
	    %% Document FIXME !!
	    %% io:format("~s::~s Temp object needs to be destroyed ~s~n",
	    %%           [get(current_class),get(current_func),Type]),
	    {Type ++ " * Result = new " ++ Type ++ "(", "); app->newPtr((void *) Result,"
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
    Alt ++ "Len";
call_arg(#param{where=c, alt={size,N}}) ->
    %% It's a binary
    N ++ "_bin.size";
% single
call_arg(#param{name=N,def=_Def,type=#type{by_val=true,single=true,base=Base}})
  when Base =:= int; Base =:= long; Base =:= float; Base =:= double; Base =:= bool ->
    N;
call_arg(#param{name=N,type=#type{base=eventType}}) ->
    N ++ ", (wxObjectEventFunction)(wxEventFunction) &WxeApp::handle_evt, Evt_cb, this";
call_arg(#param{name=N,type=#type{base={enum,_Type}, by_val=true,single=true}}) -> N;
call_arg(#param{name=N,type=#type{base={class,_},by_val=true,single=true}}) -> "*" ++ N;
call_arg(#param{name=N,type=#type{base={class,_},ref=reference,single=true}}) -> "*" ++ N;
%% By val false
call_arg(#param{name=N,def=Def,type=#type{by_val=false, ref={pointer,2}}})
  when Def =/= none -> N;
call_arg(#param{name=N,type=#type{by_val=false, single=true, ref={pointer,2}}}) ->
    "&" ++ N;
call_arg(#param{name=N,type=#type{base=string, by_val=false, single=true, ref={pointer,1}}}) ->
    "&" ++ N;
call_arg(#param{name=N,in=true, def=Def,type=#type{name=Type, base=Base, single=array, ref=Ref}}) ->
    case {Base, Type} of
        {_, "wxArrayString"} -> N;
        {_, "wxArrayInt"} -> N;
        {{term,Class}, _} when Ref =:= {pointer,2} ->
            io_lib:format("(~s **) ~s.data()", [Class, N]);
        _ when (Def =/= none) ->
            io_lib:format(" ~s.empty() ? ~s : ~s.data()", [N,Def,N]);
        _ ->
            N ++ ".data()"
    end;
call_arg(#param{name=N,type=#type{by_val=true, single=_False}}) -> N;
call_arg(#param{name=N,type=#type{by_val=copy, single=_False}}) -> N;
call_arg(#param{name=N,in=true, def=Def,type=#type{base={comp,_,_},ref={pointer,1},single=true}}) ->
    case Def =:= none of
        true -> "&" ++ N;
        _ -> N
    end;
call_arg(#param{name=N,in=IN,type=#type{ref=reference}})
  when IN =/= true -> N;
call_arg(#param{name=N,in=IN,type=#type{by_val=false, single=true}})
  when IN =/= true ->
    "&" ++ N;
call_arg(#param{name=N,type=#type{base=int, ref=reference, single=true}}) -> "*" ++ N;
call_arg(#param{name=N,type=#type{by_val=false}}) -> N;
call_arg(#param{def=Def, type=void}) when Def =/= none  -> Def;
call_arg(#param{def=Def, type=voidp}) when Def =/= none -> "(void **) " ++ Def;
call_arg(#param{name=N,type=#type{base={ref,_},by_val=true,single=true}}) -> N;
call_arg(#param{name=N, type={static_cast, #type{base={class, Class}, by_val=ByVal,ref=Ref}}}) ->
    case ByVal =:= true orelse Ref =:= reference of
        true ->
            "* static_cast<" ++ Class ++ "*> (" ++ N ++")";
        false ->
            "static_cast<" ++ Class ++ "*> (" ++ N ++")"
    end.

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

    if
	NoOut > 1 ->
            w("  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);~n"),
            w("  ERL_NIF_TERM msg = enif_make_tuple~w(rt.env,~n",[NoOut]);
        NoOut =:= 1 ->
            w("  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);~n"),
            w("  rt.send(");
	true ->
            ignore
    end,

    build_ret_types(Type,Ps,2),

    if NoOut > 1   -> w(");~n  rt.send(msg);~n");
       NoOut =:= 1 -> w(");~n");
       true -> ignore
    end,
    Ps.

build_ret_types(void,Ps,Indent) ->
    Calc = fun(#param{name=N,in=In,type=T}, Free) ->
		   case build_ret(N, {arg, In}, T) of
		       ok -> Free;
		       Other -> [Other|Free]
		   end
	   end,
    fold_arg(Calc, [], Ps, Indent);
build_ret_types(Type,Ps, Indent) ->
    w("~*c", [Indent,$\s]),
    Free = case build_ret("Result", {ret, out}, Type) of
	       ok -> [];
	       FreeStr -> [FreeStr]
	   end,
    Ps /= [] andalso w(",~n~*c", [Indent,$\s]),
    Calc = fun(#param{name=N,in=In,type=T}, FreeAcc) ->
		   case build_ret(N, {arg, In}, T) of
		       ok -> FreeAcc;
		       FreeMe -> [FreeMe|FreeAcc]
		   end
	   end,
    fold_arg(Calc, Free, Ps,Indent).


fold_arg(_, Acc, [], _) ->
    Acc;
fold_arg(Fun, Acc0, List, Indent) ->
    w("~*c", [Indent,$\s]),
    fold_arg_1(Fun, Acc0, List, Indent).

fold_arg_1(Fun, Acc, [Last], _) ->
    Fun(Last, Acc);
fold_arg_1(Fun, Acc0, [H|T], Indent) ->
    Acc = Fun(H, Acc0),
    w(",~n~*c", [Indent,$\s]),
    fold_arg_1(Fun, Acc, T, Indent).

build_ret(Name,_,#type{base={class,Class},single=true}) ->
%%    w(" rt.addRef(getRef((void *)~s,memenv), \"~s\");~n",[Name,Class]);
    case Class of
        "wxGraphicsContext" ->
            w("rt.make_ref(app->getRef((void *)~s,memenv,8), \"~s\")~n",[Name,Class]);
        _ ->
            w("rt.make_ref(app->getRef((void *)~s,memenv), \"~s\")",[Name,Class])
    end;
build_ret(Name,_,#type{name="wxTreeItemId",single=true}) ->
    w("rt.make((wxUIntPtr *) ~s.m_pItem)",[Name]);
build_ret(Name,_,#type{name="wxTreeItemIdValue",single=true}) ->
    w("rt.make((wxUIntPtr *) ~s)",[Name]);
build_ret(Name,_,#type{base={term,_},single=true}) ->
    w("rt.make_ext2term(~s)", [Name]);
build_ret(Name,_,#type{base={binary,Size},single=true}) ->
    w("rt.make_binary(~s, ~s)", [Name,Size]);
build_ret(Name,_,#type{name="wxUIntPtr", ref={pointer,1}, single=true}) ->
    w("rt.make(~s)", [Name]);
build_ret(Name,_,#type{base={enum,_Type},single=true}) ->
    w("rt.make_int(~s)",[Name]);
build_ret(Name,_,#type{base={comp,_,{record, _}},single=true}) ->
    w("rt.make(~s)", [Name]);
build_ret(Name,{ret,_},#type{base={comp,_,_},single=true, by_val=true}) ->
    w("rt.make(~s)",[Name]);
build_ret(Name,{ret,_},#type{base={comp,_,_},single=true, ref=reference}) ->
    w("rt.make((*~s))",[Name]);
build_ret(Name,_,#type{base={comp,_,_},single=true}) ->
    w("rt.make(~s)",[Name]);
build_ret(Name = "ev->m_scanCode",_,#type{base=bool,single=true,by_val=true}) ->
    %% Hardcoded workaround for 2.9 and later
    w("#if !wxCHECK_VERSION(2,9,0)~n", []),
    w(" rt.make_bool(~s)~n",[Name]),
    w("#else~n rt.make_bool(false)~n",[]),
    w("#endif~n",[]);
build_ret(Name = "ev->m_metaDown",_,#type{base=bool,single=true,by_val=true}) ->
    %% Hardcoded workaround for MAC on 2.9 and later
    w("#if wxCHECK_VERSION(2,9,0) && defined(_MACOSX)~n", []),
    w(" rt.make_bool(ev->m_rawControlDown)~n",[]),
    w("#else~n rt.make_bool(~s)~n",[Name]),
    w("#endif~n",[]);

build_ret(Name,_,#type{base=bool,single=true,by_val=true}) ->
    w("rt.make_bool(~s)",[Name]);
build_ret(Name,{arg, both},#type{base=int,single=true,mod=M}) ->
    case lists:member(unsigned, M) of
	true ->  w("rt.make_uint(~s)",[Name]);
	false -> w("rt.make_int(~s)",[Name])
    end;
build_ret(Name,_,#type{base=int,single=true,mod=M}) ->
    case lists:member(unsigned, M) of
	true ->  w("rt.make_uint(~s)",[Name]);
	false -> w("rt.make_int(~s)",[Name])
    end;
build_ret(Name,_,#type{name="wxArrayInt"}) ->
    w("rt.make(~s)", [Name]);
build_ret(Name,_,#type{name="wxArrayDouble"}) ->
    w("rt.make(~s)", [Name]);
build_ret(Name,_,#type{base={comp,_,_},single=array}) ->
    w("rt.make_array_objs(~s)", [Name]);

build_ret(Name,_,#type{base={class,Class},single=array}) ->
    w("rt.make_array_objs(~s, app, \"~s\")", [Name,Class]);
    %% w(" for(unsigned int i=0; i < ~s.GetCount(); i++) {~n", [Name]),
    %% w("  rt.make_ref(app->getRef((void *) &~s.Item(i), memenv), \"~s\")~n }~n",[Name, Class]),
    %% w(" rt.endList(~s.GetCount())~n",[Name]);
build_ret(Name,_,#type{name=_List,single=list,base={class,Class}}) ->
    w("rt.make_list_objs(~s, app, \"~s\")", [Name,Class]);

build_ret(Name,_,#type{name="wxArrayTreeItemIds"}) ->
    w("rt.make_array_objs(~s)", [Name]);

build_ret(Name,_,#type{base=float,single=true}) ->
    w("rt.make_double(~s)",[Name]);
build_ret(Name,_,#type{base=double,single=true}) ->
    w("rt.make_double(~s)",[Name]);
build_ret(Name,_,#type{name="wxeLocaleC"}) ->
    w("rt.make(wxeLocaleC2String(~s))",[Name]);
build_ret(Name,_,#type{base=string,single=true}) ->
    w("rt.make(~s)",[Name]);
build_ret(Name,_,#type{name="wxArrayString", single=array}) ->
    w("rt.make(~s)", [Name]);
build_ret(Name,_,#type{name="wxString", single={list,Variable}}) ->
    Obj = case Name of
              "ev->" ++ _ -> "ev";
              _ -> "This"
          end,
    w("rt.make_list_strings(~s->~s, ~s)~n", [Obj,Variable,Name]);
build_ret(Name,In,T) ->
    ?error({nyi, Name,In, T}).

mods([const|R]) -> "const " ++ mods(R);
mods([unsigned|R]) -> "unsigned " ++ mods(R);
mods([]) -> "".


gen_func_table(Defs) ->
    w("~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n"),
    w("~n"),
    {_Id, Entries} = gen_class_func_defs(Defs, {0,[]}),
    w("~n"),
    w("wxe_fns_t wxe_fns[] =~n{~n"),
    [w("~s", [Str]) || Str <- lists:reverse(Entries)],
    w("};~n").

gen_class_func_defs([Class | Rest], Acc) ->
    #class{name=Name, methods=Ms, options=Opts} = Class,
    Methods = lists:keysort(#method.id, lists:flatten(Ms)),
	AddOpts = case lists:keysearch(ifdef,1,Opts) of
        {value, {ifdef, What}} when is_atom(What) -> [{ifdef, atom_to_list(What)}];
        {value, {ifdef, What}} when is_list(What) -> [{ifdef, What}];
        false -> []
    end,
    Acc2 = lists:foldl(fun (Method = #method{opts=FOpts}, InAcc) -> 
        gen_func_defs({Name, Method#method{opts=FOpts ++ AddOpts}}, InAcc) 
    end, Acc, Methods),
    gen_class_func_defs(Rest, Acc2);
gen_class_func_defs([], Acc) -> Acc.

gen_func_defs({_, #method{where=erl_no_opt}}, Acc) -> Acc;
gen_func_defs({_, #method{where=erl_alias}}, Acc) -> Acc;
gen_func_defs({Class, #method{id=Id, method_type=MT, opts=FOpts, params=Ps}=Mtd}, {Id, Strs}) ->
    IsObjectDest = MT =:= destructor andalso
        hd(reverse(wx_gen_erl:parents(Class))) =:= object,
    Func = wx_gen_erl:get_unique_name(Id),
    IsTaylorMadeErl = lists:member(Func, ["wxXmlResource_xrcctrl",
                                          "wxListCtrl_new_2",
                                          "wxEvtHandler_Disconnect_1",
                                          "wxEvtHandler_Disconnect_0"]),
    N = wx_gen_erl:erl_args_count(Ps, c),
    Name = wx_gen_erl:erl_func_name(Mtd),
    Restrictions = [IfDef || {value, {_, IfDef}} <- [lists:keysearch(What, 1, FOpts) || What <- [ifdef, deprecated, test_if]]],
    {Prefix, Stub, Postfix} = if 
            Restrictions == [] -> {"", "", ""};
            true -> {
                io_lib:format("#if ~s~n", [string:join(Restrictions, " && ")]), 
                io_lib:format("#else~n  {NULL, \"~s\", \"~s\", 0}, // ~w~n", [Class, Name, Id]),
                io_lib:format("#endif // ~s~n", [string:join(Restrictions, " && ")])
            }
        end,
    w(Prefix),
    Str1 = if IsObjectDest ->
                  io_lib:format("  {NULL, \"~s\", \"~s\", 1}, // ~w obj destructor ~s~n", [Class, Name, Id, Func]);
             IsTaylorMadeErl ->
                  io_lib:format("  {NULL, \"~s\", \"~s\", ~w}, // ~w TaylorMade erl only ~s~n", [Class, Name, N, Id, Func]);
             true ->
                  w("extern void ~s(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);~n", [Func]),
                  io_lib:format("  {~s, \"~s\", \"~s\", ~w}, // ~w~n", [Func, Class, Name, N, Id])
          end,
    w(Postfix),
    {Id+1, [[Prefix,Str1,Stub,Postfix]|Strs]};
gen_func_defs({_, _}=CM, {50=Id, Strs}) ->
    w("extern void wxe_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);~n", []),
    Str = io_lib:format("  {wxe_destroy, \"wxe_util\", \"destroy\", 1}, // ~w~n", [Id]),
    gen_func_defs(CM, {Id+1, [Str|Strs]});
gen_func_defs({_, _}=CM, {51=Id, Strs}) ->
    w("extern void wxe_registerPid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);~n", []),
    Str = io_lib:format("  {wxe_registerPid, \"wxe_util\", \"registerPid\", 1}, // ~w~n", [Id]),
    gen_func_defs(CM, {Id+1, [Str|Strs]});
gen_func_defs({_, #method{id=MId}}=CM, {Id, Strs}) when MId > Id ->
    Str = io_lib:format("  {NULL, \"\", \"\", 0}, // ~w~n", [Id]),
    gen_func_defs(CM, {Id+1, [Str|Strs]}).


build_enums() ->
    w(" /* This file is also generated */~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n"),
    w("#include \"wxe_macros.h\"~n"),
    w("#include \"../wxe_return.h\"~n~n"),

    w("typedef struct {~n"
      "  ERL_NIF_TERM type;~n"
      "  const char * key;~n"
      "  ERL_NIF_TERM value;~n"
      "} wxe_defs;~n~n",[]),

    w("void WxeApp::init_consts(wxeMetaCommand& event) {~n"),
    w("  WxeApp * app = this;~n"),
    w("  ErlNifPid caller = event.caller;~n"),
    w("  wxeMemEnv * memenv = global_me;~n"),
    w("  wxeReturn rt = wxeReturn(memenv, caller, true);~n"),
    w("  wxe_defs defs[] = {\n"),
    lists:foreach(fun build_gvar/1, get(gvars)),
    w("    { WXE_ATOM_define, \"wxDefaultCoord\", rt.make_int(-1) },~n", []),
    w("    { WXE_ATOM_define, \"wxDefaultSize\", enif_make_tuple2(rt.env, rt.make_int(-1), rt.make_int(-1)) },~n", []),
    w("    { WXE_ATOM_define, \"wxDefaultPosition\", enif_make_tuple2(rt.env, rt.make_int(-1), rt.make_int(-1)) },~n", []),
    Consts = get(consts),
    Get = fun(Name) ->
                  case gb_trees:lookup(Name, Consts) of
                      {value, Const} -> Const;
                      none -> false
                  end
          end,
    [build_enums(EN, E, Get) || {{enum,EN},E = #enum{as_atom=false}} <- lists:sort(get())],
    %%[build_enum(NConst) || NConst <- lists:keysort(#const.name, Consts)],
    w("  };~n"),
    w("  int sz = sizeof(defs) / sizeof(defs[0]);~n",[]),
    w("  std::vector <ERL_NIF_TERM> consts;~n",[]),
    w("  /* INITIALIZING the CONSTS array in two steps make C++ compilation a lot faster */~n" ,[]),
    w("  for( int i = 0; i <  sz; i++) {~n", []),
    w("    consts.push_back(enif_make_tuple3(rt.env, defs[i].type, enif_make_atom(rt.env, defs[i].key), defs[i].value));~n",[]),
    w("  }~n",[]),
    w("  rt.send(enif_make_list_from_array(rt.env, consts.data(), consts.size()));~n",[]),
    w("}~n"),
    ok.

const_name(Name) ->
    wx_gen_erl:const_name(Name).

enum_name({define, _}, _) ->
    "WXE_ATOM_define";
enum_name([$@|_File], EnumDef0) ->
    Name = const_name(EnumDef0),
    Names = string:lexemes(Name, "_0123456789"),
    "enif_make_atom(rt.env,\"" ++ hd(Names) ++ "\")";
enum_name(Name, _Enum) when is_list(Name) ->
    "enif_make_atom(rt.env,\"" ++ Name ++ "\")";
enum_name({Class, [$@|_]}, _Enum) when is_list(Class) ->
    %% Numbered name not defined
    "enif_make_atom(rt.env,\"" ++ Class ++ "\")";
enum_name({Class, EnumName}, _Enum) ->
    case string:prefix(EnumName, Class) of
        false -> "enif_make_atom(rt.env,\"" ++ Class ++ "_" ++ EnumName ++ "\")";
        _ -> "enif_make_atom(rt.env,\"" ++ EnumName ++ "\")"
    end.

build_enums(_, #enum{vals=[]}, _GetConst) ->
    ok;
build_enums(EnumName, #enum{from=From, vals=Vals}, GetConst) ->
    w("// ~s~n", [wx_gen_erl:enum_from(From)]),
    [build_enum(EnumName, GetConst(N)) || {N,_} <- Vals].

build_enum(_Enum, false) ->
    ok;
build_enum(Enum, #const{name=Name, opt=undefined}) ->
    w("    { ~s, \"~s\", rt.make_int(~s) },~n",
      [enum_name(Enum, Name), const_name(Name), Name]);
build_enum(Enum, #const{name=Name, opt={test_if, Test}}) ->
    w("#if ~s~n", [Test]),
    w("    { ~s, \"~s\", rt.make_int(~s) },~n",
      [enum_name(Enum, Name), const_name(Name), Name]),
    w("#else~n", []),
    w("    { ~s, \"~s\", WXE_ATOM_undefined },~n",
      [enum_name(Enum, Name), const_name(Name)]),
    w("#endif~n", []).

build_gvar({Name, "wxColour"}) ->
    w("    { WXE_ATOM_global, \"~s\", rt.make(*(~s)) },~n", [const_name(Name), Name]);
build_gvar({Name, {address,Class}}) ->
    w("    { WXE_ATOM_global, \"~s\","
      "rt.make_ref(app->getRef((void *)&~s,memenv), \"~s\") },~n",[const_name(Name),Name,Class]);
build_gvar({Name, Class}) ->
    w("    { WXE_ATOM_global, \"~s\","
      "rt.make_ref(app->getRef((void *)~s,memenv), \"~s\") },~n",[const_name(Name),Name,Class]).

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
    w("#include <wx/textctrl.h>~n"),
    w("#include <wx/popupwin.h>~n"),
    w("#include <wx/html/htmlwin.h>~n"),
    w("#include <wx/html/htmlcell.h>~n"),
    w("#include <wx/filename.h>~n"),
    w("#include <wx/sysopt.h>~n"),
    w("#include <wx/overlay.h>~n"),
    w("#include <wx/notifmsg.h>~n"),
    w("#include <wx/webview.h>~n"),
    w("#if wxUSE_WEBVIEW && wxUSE_WEBVIEW_IE~n"),
    w("#include <wx/msw/webview_ie.h>~n"),
    w("#endif~n"),
    w("#if wxUSE_GLCANVAS_EGL && !wxCHECK_VERSION(3,2,3)~n"),
    w("#include <EGL/egl.h>~n"),
    w("#endif~n"),

    w("~n~n", []),
    w("#ifndef wxICON_DEFAULT_BITMAP_TYPE~n",[]),
    w("  #define wxICON_DEFAULT_BITMAP_TYPE wxBITMAP_TYPE_ICO_RESOURCE~n",[]),
    w("#endif~n", []),

    w("~n~n", []),
    w("#if defined(wxSTC_DISABLE_MACRO_DEPRECATIONS) && defined(wxSTC_DEPRECATED_MACRO_VALUE)~n",[]),
    w("#undef wxSTC_DEPRECATED_MACRO_VALUE~n",[]),
    w("#define wxSTC_DEPRECATED_MACRO_VALUE(value, msg) value~n",[]),
    w("#endif~n",[]),

    %% [w("#define ~s_~s ~p~n", [Class,Name,Id]) ||
    %%     {Class,Name,_,Id} <- wx_gen_erl:get_unique_names()],
    %% w("~n~n").
    ok.

build_events() ->
    open_write("../c_src/gen/wxe_events.cpp"),
    c_copyright(),
    w("~n/***** This file is generated do not edit ****/~n~n"),
    w("#include <wx/wx.h>~n"),
    w("#include \"../wxe_impl.h\"~n~n"),
    w("#include \"wxe_macros.h\"~n"),
    w("#include \"../wxe_events.h\"~n~n"),
    w("#include \"../wxe_return.h\"~n~n"),

    w("WX_DECLARE_HASH_MAP(int, wxeEtype*, wxIntegerHash, wxIntegerEqual, wxeETmap );~n~n"),

    w("wxeETmap etmap;~n~n"),

    w(
"int wxeEventTypeFromAtom(ERL_NIF_TERM etype_atom) {
  wxeETmap::iterator it;
  for(it = etmap.begin(); it != etmap.end(); ++it) {
    wxeEtype * value = it->second;
    if(enif_is_identical(value->evName, etype_atom)) {
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
    w("  wxe_evInfo event_types[] =~n    {~n",[]),

    lists:foreach(fun(Ev) -> init_event_classes(Ev) end,
		  [#class{name="wxWXENullEvent", id=0,event=[wxEVT_NULL]}|Evs]),
    w("     {-1, 0, \"\", \"\", \"\"}~n  };~n~n",[]),
    w("  ErlNifEnv *env = enif_alloc_env();~n"),
    w("  for(int i=0; event_types[i].ev_type != -1; i++) {~n",[]),
    w("     if(NULL == etmap[event_types[i].ev_type]) {~n",[]),
    w("       etmap[event_types[i].ev_type] =~n"
      "         new wxeEtype(env, &event_types[i]);~n"),
    w("     } else {~n",[]),
    w("       wxeEtype *prev = etmap[event_types[i].ev_type];~n"
      "       wxString msg(wxT(\"Duplicate event defs: \"));~n"
      "       msg += wxString::FromAscii(event_types[i].ev_name);~n"
      "       msg += wxString::Format(wxT(\" %d \"), event_types[i].class_id);~n"
      "       msg += wxString::FromAscii(prev->evName);~n"
      "       msg += wxString::Format(wxT(\" %d\"), prev->cID);~n"
      "       send_msg(\"internal_error\", &msg);~n"
      "     }~n"
      "  }~n", []),
    w("  enif_free_env(env);~n"),
    w("}~n~n").

init_event_classes(#class{name=Name, event=ETs, id=Id}) ->
    F = fun({Eev, Cev, OtherClass}) ->
		w("     {~w + wxEVT_USER_FIRST, ~w, ~p, ~p, ~p},~n",
		  [Cev, find_id(OtherClass), wx_gen_erl:event_type_name(Eev),
                   Name, wx_gen_erl:event_rec_name(Name)]);
	   ({Ev, {test_if, Test}}) ->
		w("#if ~s~n", [Test]),
		w("     {~w, ~w, ~p, ~p, ~p},~n",
		  [Ev, Id, wx_gen_erl:event_type_name(Ev),
                   Name, wx_gen_erl:event_rec_name(Name)]),
		w("#endif~n", []);
	   (Ev) ->
		w("     {~w, ~w, ~p, ~p, ~p},~n",
		  [Ev, Id, wx_gen_erl:event_type_name(Ev),
                   Name, wx_gen_erl:event_rec_name(Name)])
	end,
    [F(ET) || ET <- ETs].

find_id(OtherClass) ->
    Class = get({class,atom_to_list(OtherClass)}),
    %%{value, Class} = lists:keysearch(atom_to_list(OtherClass), #class.name, All),
    Class#class.id.

encode_events(Evs) ->
    ?WTC("encode_events"),
    w("bool sendevent(wxEvent *event, wxeMemEnv *memenv)~n{~n"
      "  int send_res ;~n"
      "  wxMBConvUTF32 UTFconverter;~n"
      "  wxeEtype *Etype = etmap[event->GetEventType()];~n"
      "  wxeEvtListener *cb = (wxeEvtListener *)event->m_callbackUserData;~n"
      "  WxeApp * app = (WxeApp *) wxTheApp;~n"
      "  if(!memenv) return 0;~n~n"
      "  wxeReturn rt = wxeReturn(memenv, cb->listener, false);~n"),
    w("  ERL_NIF_TERM ev_term;~n"),
    w("  switch(Etype->cID) {~n"),
    lists:foreach(fun(Ev) -> encode_event(Ev) end, Evs),
    w("  }~n~n"),

    w("  ERL_NIF_TERM wx_ev =\n"
      "    enif_make_tuple5(rt.env,\n"
      "                     WXE_ATOM_wx,\n"
      "                     rt.make_int((int) event->GetId()),\n"
      "                     rt.make_ref(cb->obj, cb->class_name),\n"
      "                     rt.make_ext2term(cb->user_data),\n"
      "                     ev_term);\n\n"),

    w("  if(cb->fun_id) {\n"
      "    ERL_NIF_TERM wx_cb =\n"
      "      enif_make_tuple4(rt.env,\n"
      "                       WXE_ATOM__wx_invoke_cb_,\n"
      "                       rt.make_int(cb->fun_id),\n"
      "                       wx_ev,\n"
      "                       rt.make_ref(app->getRef((void *)event,memenv), Etype->evClass)\n"
      "                       );\n"
      "    pre_callback();\n"
      "    send_res =  rt.send(wx_cb);\n"
      "    if(send_res) handle_event_callback(memenv->me_ref, cb->listener);\n"
      "    app->clearPtr((void *) event);\n"
      "  } else {\n"
      "    send_res =  rt.send(wx_ev);\n"
      "    if(cb->skip) event->Skip();\n"),
    #class{id=SizeId} = lists:keyfind("wxSizeEvent", #class.name, Evs),
    #class{id=MoveId} = lists:keyfind("wxMoveEvent", #class.name, Evs),
    w("    if(app->recurse_level < 1 && (Etype->cID == ~w || Etype->cID == ~w)) {~n",
      [SizeId, MoveId]),
    w("      app->recurse_level++;~n"),
    w("      app->dispatch_cmds();~n"),
    w("      app->recurse_level--;~n"),
    w("    }~n"),
    w("  };~n"),
    w("  return send_res;~n"),
    w("}~n").

encode_event(C = #class{name=Class, id=Id, options=Opts}) ->
    ?WTC("encode_event"),
    case proplists:get_value(ifdef, Opts) of
    undefined -> ok;
    What when is_list(What)-> w("#if ~s~n",[What]);
    What when is_atom(What) -> w("#if ~p~n",[What])
    end,    
    case proplists:get_value("mixed_event", Opts) of
	undefined ->
	    w("  case ~p: {// ~s~n", [Id,Class]),
	    encode_event2(C),
	    ok;
	Mixed ->
	    w("  case ~p: {// ~s or ~s~n", [Id,Class,Mixed]),
	    w("    if(event->IsKindOf(CLASSINFO(~s))) {~n",[Class]),
	    encode_event2(C),
	    w("    } else {~n",[]),
	    w("      Etype = etmap[event->GetEventType() + wxEVT_USER_FIRST];~n",[]),
	    encode_event2(get({class,atom_to_list(Mixed)})),
	    w("  }~n",[]),
	    ok
    end,
    w("    break;~n  }~n"),
    case proplists:get_value(ifdef, Opts) of
    undefined -> ok;
    Endif -> w("#endif // ~p~n~n",[Endif])
    end.

encode_event2(#class{}=Class) ->
    Attrs = build_event_attrs(Class),
    TupleSz = length(Attrs) + 2,
    case TupleSz < 8 of
        true  -> w("    ev_term = enif_make_tuple~w(rt.env,~n",[TupleSz]);
        false -> w("    ev_term = enif_make_tuple(rt.env,~w,~n",[TupleSz])
    end,
    w("        Etype->evRecord,~n", []),
    case TupleSz == 2 of
        true  -> w("        Etype->evName~n");
        false -> w("        Etype->evName,~n"),
                 build_ret_types(void, Attrs, 8)
    end,
    w(");~n").

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
	    w("    ~s * ev = (~s *) event;~n",[Class,Class]),
	    FixClass =
		fun(P=#param{name=N,acc=Acc,type=#type{single=Single,by_val=ByVal,
						       base={class,C},mod=Mods,
                                                       ref = Ref}})
		   when Acc =/= undefined ->
			Var = var_name(N),
			if Single, ByVal ->
				w("    ~s * ~s = new ~s(~s);~n", [C,Var,C,N]),
				w("    app->newPtr((void *) ~s,3, memenv);~n", [Var]);
                           Ref =:= reference ->
                                w("    ~s ~s * ~s = &~s;~n", [mods(Mods),C,Var,N]);
			   true ->
				w("     ~s ~s * ~s = ~s;~n", [mods(Mods),C,Var,N])
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


enum_type({Class,Type}) ->
    Class ++ "::" ++ Type;
enum_type(Type) -> Type.
