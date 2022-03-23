%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2022. All Rights Reserved.
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
%%% File    : wx_gen_erl.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description :
%%%
%%% Created : 25 Jan 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(wx_gen_erl).

-include("wx_gen.hrl").

%%-compile(export_all).
-export([gen/1]).
-export([parents/1, get_unique_names/0, get_unique_name/1, erl_option_name/1,
         const_name/1, enum_from/1,
	 erl_func_name/1, erl_func_name/2, erl_args_count/2,
         event_type_name/1, event_rec_name/1, filter_attrs/1]).


-import(lists, [foldl/3,reverse/1, filter/2]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1,
		   open_write/1, close/0, erl_copyright/0, w/2,
		   args/3, args/4]).

gen(Defs) ->
    [put({class,N},C) || C=#class{name=N} <- Defs],
    gen_unique_names(Defs),
    gen_event_recs(),
    gen_enums_ints(),
    Static = gen_static([C || C=#class{parent="static"} <- Defs]),
    Replace = fun(#class{name=Name}=C, Dfs) ->
		      lists:keyreplace(Name, #class.name, Dfs, C)
	      end,
    All = lists:foldl(Replace, Defs, Static),
    [gen_class(Class) || Class <- All],
    gen_funcnames(),
    gen_api_footprint(All).

gen_class(Class) ->
    try
	gen_class1(Class)
    catch throw:skipped ->
	    Class
    end.

gen_static(Files) ->
    open_write("../src/gen/wx_misc.erl"),
    erl_copyright(),
    w("", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% @doc See external documentation: "
      "<a href=\"http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html\">Misc</a>.\n\n",[]),

    w("%% This module contains wxWidgets utility functions.~n~n", []),
    w("-module(wx_misc).~n", []),
    w("-include(\"wxe.hrl\").~n",[]),
    %% w("-compile(export_all).~n~n", []),            %% XXXX remove ???
    [gen_static_exports(C) || C <- Files],
    {ok, MiscExtra} = file:read_file(filename:join([wx_extra, "wx_misc.erl"])),
    w("~s", [MiscExtra]),
    Classes = [gen_static_methods(C) || C <- Files],
    close(),
    Classes.


gen_static_exports(C=#class{parent="static",methods=Ms}) ->
    Exp = fun(M) -> gen_export(C,M) end,
    ExportList = lists:usort(lists:append(lists:map(Exp,reverse(Ms)))),
    w("-export([~s]).~n~n", [args(fun({EF,_}) -> EF end, ",", ExportList, 60)]),
    ok.

gen_static_methods(C=#class{name=Name, parent="static",methods=Ms}) ->
    put(current_class, Name),
    Gen = fun(M) -> gen_method(Name,M) end,
    NewMs = lists:map(Gen,reverse(Ms)),
    erase(current_class),
    C#class{methods=NewMs}.

gen_class1(C=#class{parent="static"}) ->
    C;
gen_class1(C=#class{name=Name,parent=Parent,methods=Ms,options=Opts}) ->
    lists:member("ignore", Opts) andalso throw(skipped),
    open_write("../src/gen/"++Name++".erl"),
    put(current_class, Name),
    erl_copyright(),
    w("", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),

    case lists:member(taylormade, Opts) of
	true ->
	    {ok, Bin} = file:read_file(filename:join([wx_extra, Name++".erl"])),
	    w("~s~n", [binary_to_list(Bin)]),
	    NewMs = Ms;
	false ->
	    case C#class.doc of
		undefined -> ignore;
		Str -> w("%%~n%% ~s~n~n%%~n", [Str])
	    end,

	    Parents = parents(Parent),
	    w("-module(~s).~n", [Name]),
	    w("-include(\"wxe.hrl\").~n",[]),
	    Exp = fun(M) -> gen_export(C,M) end,
	    ExportList = lists:usort(lists:append(lists:map(Exp,reverse(Ms)))),
	    w("-export([~s]).~n~n", [args(fun({EF,_}) -> EF end, ",", ExportList, 60)]),
	    w("%% inherited exports~n",[]),
	    Done0 = ["Destroy", "New", "Create", "destroy", "new", "create"],
	    Done  = gb_sets:from_list(Done0 ++ [M|| #method{name=M} <- lists:append(Ms)]),
	    {_, InExported0} = gen_inherited(Parents, Done, []),
	    InExported = lists:ukeysort(2, [{?MODULE,{"parent_class","1"},false}|InExported0]),
	    w("-export([~s]).~n~n", [args(fun({_M,{F,A},_Dep}) -> F ++ "/" ++ A end, ",",
					  InExported,
					  60)]),
            w("-type ~s() :: wx:wx_object().~n", [Name]),
            case C#class.event of
                false ->
                    w("-export_type([~s/0]).~n", [Name]);
                Evs ->
                    w("-include(\"wx.hrl\").~n",[]),
                    EvTypes = [event_type_name(Ev) || Ev <- Evs],
                    EvStr  = args(fun(Ev) -> "'" ++ Ev ++ "'" end, " | ", EvTypes),
                    w("-type ~sType() :: ~s.~n", [Name, EvStr]),
                    w("-export_type([~s/0, ~s/0, ~sType/0]).~n",
                      [Name, event_rec_name(Name), Name])
            end,

	    case lists:filter(fun({_F,Depr}) -> Depr end, ExportList) of
		[] -> ok;
		Depr ->
                    DepStr = "not available in wxWidgets-2.9 and later",
                    w("-deprecated([~s]).~n~n",
                      [args(fun({EF,_}) ->
                                    [DFun,DArgs] = string:split(EF, "/"),
                                    io_lib:format("{~s,~s,\"~s\"}", [DFun,DArgs,DepStr])
                            end, ",\n             ", Depr, 60)])
	    end,
	    case lists:filter(fun({_,_,Depr}) -> Depr end, InExported) of
		[] -> ok;
		NoWDepr -> w("-compile([~s]).~n~n",
			  [args(fun({M,{F,A},_}) ->
					DStr=io_lib:format("{nowarn_deprecated_function, {~s,~s,~s}}",
							  [M,F,A]),
					lists:flatten(DStr)
				end, ",", NoWDepr, 60)])
	    end,


	    w("%% @hidden~n", []),
	    parents_check(Parents),
	    Gen = fun(M) -> gen_method(Name,M) end,
	    NewMs = lists:map(Gen,reverse(Ms)),
	    gen_dest(C, Ms),

	    gen_inherited(Parents, Done, true)
    end,

    close(),
    erase(current_class),
    C#class{methods=NewMs}.


parents("root") -> [root];
parents("object") -> [object];
parents(Parent) ->
    case get({class,Parent}) of
	#class{parent=GrandParent} ->
	    [Parent|parents(GrandParent)];
	undefined ->
	    ?warning("unknown parent of ~p~n",[Parent]),
	    [Parent]
    end.

parents_check([object]) ->
    w("parent_class(_Class) -> erlang:error({badtype, ?MODULE}).~n~n",[]);
parents_check([root]) ->
    w("parent_class(_Class) -> erlang:error({badtype, ?MODULE}).~n~n",[]);
parents_check([Parent|Ps]) ->
    w("parent_class(~s) -> true;~n",[Parent]),
    parents_check(Ps).

check_class(#type{name="wxObject", base={class,"wx"}}) ->
    "wx:wx_object()";
check_class(#type{name="wxIconLocation", base={class,"wx"}}) ->
    "wx:wx_object()";
check_class(#type{name="wxToolBarToolBase", base={class,"wx"}, mod=_Mod}) ->
    %% Implement this some day
    "wx:wx_object()";
check_class(#type{name="wxValidator", base={class,"wx"}, mod=_Mod}) ->
    %% Implement this some day
    "wx:wx_object()";
check_class(#type{name=Name, base={class,"wx"}}) ->
    exit({class, Name});
check_class(#type{base={class,Name},xml=Xml}) ->
    case get({class,Name}) of
	undefined ->
	    case get({enum, Name}) of
		undefined ->
		    case Xml of
			"class" ++ _ ->
			    ?warning("~s:~s: Class ~p used but not defined~n",
				     [get(current_class),get(current_func),Name]);
			_ ->
			    ?warning("~s:~s: Class ~p used but not defined~n   (see ~p)~n",
				     [get(current_class),get(current_func),Name, Xml])
		    end,
		    "wx:wx_object()";
		_ ->
		    ?warning("~s:~s: Class ~p used is enum~n",
			     [get(current_class),get(current_func),Name]),
		    exit(class_enum)
	    end;
	_ ->
	    Name ++ ":" ++ Name ++ "()"
    end.

gen_export(#class{name=Class,abstract=Abs},Ms0) ->
    GetF = fun(M=#method{method_type=constructor,where=W,params=Ps}) ->
		   deprecated(M, "new" ++ "/" ++ integer_to_list(erl_args_count(Ps, W)));
              (M=#method{method_type=destructor}) ->
		   case Abs of
		       true -> [];
		       _ -> deprecated(M, "destroy/1")
		   end;
	      (M=#method{name=N,alias=A,where=W, params=Ps}) ->
                   deprecated(M, erl_func_name(N,A) ++ "/" ++ integer_to_list(erl_args_count(Ps,W)))
	   end,
    case Ms0 of
	[] -> [];
	[M=#method{where=taylormade}|_] ->
	    try	[deprecated(M, taylormade_export(Class, M))]
	    catch error:{badmatch, {error, enoent}} ->
		    lists:map(GetF, Ms0)
	    end;
	Ms ->
	    lists:map(GetF, Ms)
    end.

deprecated(#method{opts=FOpts}, FA) ->
    case lists:keysearch(deprecated, 1, FOpts) of
	{value, {deprecated, _}} ->
	    {FA,true};
	_ ->
	    {FA,false}
    end.

gen_method(Class,Ms0) ->
    case Ms0 of
	[] -> Ms0;
	[#method{where=taylormade}|_] ->
	    try
		taylormade_func(Class, Ms0)
	    catch error:{badmatch, {error, enoent}} ->
		    gen_doc(Class,Ms0),
		    gen_method1(Ms0)
	    end,
	    Ms0;
	Ms ->
 	    gen_doc(Class,Ms),
	    gen_method1(Ms),
	    Ms0
    end.

gen_method1([M=#method{method_type=destructor}]) ->
    %% Skip now do destructors later
    M;
gen_method1([M0]) ->
    gen_method2(M0),
    w(".~n~n",[]);
gen_method1([M0|Ms]) ->
    gen_method2(M0),
    w(";~n",[]),
    gen_method1(Ms).

gen_method2(M=#method{name=N,alias=A,params=Ps0,where=erl_no_opt,method_type=MT}) ->
    put(current_func, N),
    Ps = [patch_param(P,classes) || P <- Ps0],
    w("~n", []),
    gen_function_clause(erl_func_name(N,A),MT,Ps,[],[name_type]),
    w("  ", []),
    gen_function_clause(erl_func_name(N,A),MT,Ps,empty_list,[no_guards,name_only]),
    M;
gen_method2(M=#method{name=N,alias=A,params=Ps0,where=erl_alias,method_type=MT}) ->
    put(current_func, N),
    {Ps1, Optional} = split_optional(Ps0),
    Ps = [patch_param(P,classes) || P <- Ps1],
    w("~n", []),
    gen_function_clause(erl_func_name(N,A),MT,Ps,Optional,[name_type]),
    w("  ", []),
    gen_function_clause(erl_func_name(N,undefined),MT,Ps,Optional,[no_guards,name_only]),
    M;
gen_method2(M=#method{name=N,alias=A,params=Ps,type=T,method_type=MT,id=MethodId}) ->
    put(current_func, N),
    {Args, Optional} = split_optional(Ps),
    gen_function_clause(erl_func_name(N,A),MT, Args, Optional, []),
    MId = arg_type_tests(Args, "?" ++ get_unique_name(MethodId)),
    MArgs = marshal_args(Args),
    MOpts = marshal_opts(Optional, Args),
    case gen_util:get_hook(erl, M#method.pre_hook) of
	ignore -> skip;
	Pre -> w("  ~s~n", [Pre])
    end,

    case MArgs == [] andalso MOpts == [] of
        true  -> w("  wxe_util:queue_cmd(?get_env(), ~s)", [MId]);
        false -> w("  wxe_util:queue_cmd(~s~s,?get_env(),~s)", [MArgs,MOpts,MId])
    end,
    case gen_util:get_hook(erl, M#method.post_hook) of
	ignore -> skip;
	Post -> w(",~n  ~s~n", [Post])
    end,
    case have_return_vals(T, Ps) of
        true -> w(",~n  wxe_util:rec(~s)", [MId]);
        false -> ignore
    end,
    erase(current_func),
    M.

gen_dest(#class{name=CName,abstract=Abs}, Ms) ->
    case Abs of
	true ->
	    ignore;
	false ->
	    case lists:keysearch(destructor,#method.method_type, lists:append(Ms)) of
		{value, #method{method_type=destructor, id=Id}} ->
		    case hd(reverse(parents(CName))) of
			object ->
			    gen_dest2(CName, object);
			root ->
			    gen_dest2(CName, Id)
		    end;
		false ->
		    erlang:error({no_destructor_found, CName})
	    end
    end.

gen_dest2(Class, Id) ->
    w("%% @doc Destroys this object, do not use object again~n", []),
    w("-spec destroy(This::~s()) -> 'ok'.~n", [Class]),
    w("destroy(Obj=#wx_ref{type=Type}) ->~n", []),
    w("  ?CLASS(Type,~s),~n",[Class]),
    case Id of
	object ->
	    w("  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),~n  ok.~n", []);
	_ ->
	    w("  wxe_util:queue_cmd(Obj, ?get_env(), ?~s),~n  ok.~n", [get_unique_name(Id)])
    end,
    ok.

gen_inherited([root], Done, Exported) -> {Done, Exported};
gen_inherited([object], Done, Exported) -> {Done, Exported};
gen_inherited([Parent|Ps], Done0, Exported0) ->
    #class{name=Class, methods=Ms} = get({class,Parent}),
    case is_list(Exported0) of
	false -> w(" %% From ~s~n", [Class]);
	true  -> ignore
    end,
    {Done,Exported} = gen_inherited_ms(Ms, Class, Done0, gb_sets:empty(), Exported0),
    gen_inherited(Ps, gb_sets:union(Done,Done0), Exported).

gen_inherited_ms([[M=#method{name=Name,alias=A,params=Ps0,where=W,method_type=MT}|_]|R],
		 Class,Skip,Done, Exported) ->
    case gb_sets:is_member(Name,Skip) of
	false when MT =:= member, Exported =:= true ->
	    Ps = [patch_param(P,all) || P <- Ps0],
	    Opts = if W =:= erl_no_opt -> [];
		      true ->
			   [Opt || Opt = #param{def=Def,in=In, where=Where} <- Ps,
				   Def =/= none, In =/= false, Where =/= c]
		   end,
	    w("%% @hidden~n", []),
	    gen_function_clause(erl_func_name(Name,A),MT,Ps,Opts,[no_guards,name_only]),
	    w(" -> ~s:", [Class]),
	    gen_function_clause(erl_func_name(Name,A),MT,Ps,Opts,[no_guards,name_only]),
	    w(".~n", []),
	    gen_inherited_ms(R,Class, Skip, gb_sets:add(Name,Done), Exported);
	false when MT =:= member, is_list(Exported) ->
	    {_, Depr} = deprecated(M,ignore),
	    Export = {Class,{erl_func_name(Name,A),integer_to_list(erl_args_count(Ps0,W))}, Depr},
	    gen_inherited_ms(R,Class,Skip, gb_sets:add(Name,Done),
			     [Export|Exported]);
	_ ->
	    gen_inherited_ms(R,Class, Skip, Done, Exported)
    end;
gen_inherited_ms([[_|Check]|R],Class,Skip, Done0,Exp) ->
    gen_inherited_ms([Check|R],Class,Skip, Done0,Exp);
gen_inherited_ms([[]|R],Class,Skip,Done0,Exp) ->
    gen_inherited_ms(R,Class,Skip,Done0,Exp);
gen_inherited_ms([], _, _Skip, Done,Exp) -> {Done,Exp}.


%%%%%%%%%%%%%%%

taylormade_func(Class, [#method{name=Name, id=Id}|_]) ->
    {ok, Bin} = file:read_file(filename:join([wx_extra, Class ++".erl"])),
    Src = binary_to_list(Bin),
    %% io:format("~p ~p ~p ~p~n",[Class, Name, Id, get_unique_name(Id)]),
    Str = case gen_util:get_taylor_made(Src, Name) of
	      nomatch ->
		  {match, [Str0]} = gen_util:get_taylor_made(Src, get_unique_name(Id)),
		  Str0;
	      {match, [Str0]} ->
		  Str0
	  end,
    w(Str, ["?" ++ get_unique_name(Id)]),
    ok.

taylormade_export(Class, #method{name=Name}) ->
    {ok, Bin} = file:read_file(filename:join([wx_extra, Class ++".erl"])),
    Str0 = binary_to_list(Bin),
    {match, [Str1]} = re:run(Str0, "<<EXPORT:"++Name++"(.*)"++Name++":EXPORT>>",
			     [dotall, {capture, all_but_first, list}]),
    Str1.

%%%%%%%%%%%%%%%

arg_type_tests([P|Ps], Mid0) ->
    case arg_type_test(P,"\n",Mid0) of
	Mid0 ->
	    arg_type_tests(Ps, Mid0);
	Mid ->  %% Already checked the other args
	    Mid
    end;
arg_type_tests([],Mid) -> Mid.

arg_type_test(#param{where=c}, _, Acc) ->
    Acc;
arg_type_test(#param{name=Name0,in=In,type=#type{base={class,T},single=true},def=none},
	      EOS,Acc) when In =/= false ->
    Name = erl_arg_name(Name0),
    w("  ?CLASS(~sT,~s),~s", [Name,T,EOS]),
    Acc;
arg_type_test(#param{name=Name0,in=In,type=#type{base={class,T}}, def=none},EOS,Acc)
  when In =/= false ->
    Name = erl_arg_name(Name0),
    w(" _ = [?CLASS(~sT,~s) || #wx_ref{type=~sT} <- ~s],~s", [Name,T,Name,Name,EOS]),
    Acc;
arg_type_test(#param{name=Name0,def=none,in=In,
		     type={merged,MTs}},
              _EOS, Acc)
  when In =/= false ->
    Name = erl_arg_name(Name0),
    [w("  Is~s = ?CLASS_T(~sT,~s),~n",[T,Name,T]) || {_, #type{base={class,T},single=true}, _} <- MTs],
    w("  ~sType = if~n",[Name]),
    Clause = fun({_,#type{base={class,T}},Ps}, Patch) ->
                     w("    Is~s -> ", [T]),
                     lists:foreach(fun(Param) -> arg_type_test(Param," ", ignore) end,
                                   element(1,split_optional(patchArgName(Ps,Patch)))),
                     w("  ~s;~n",[T])
             end,
    First = {_,_,Ps0} = hd(MTs),
    Clause(First, ignore),
    [Clause(MT, Ps0) || MT <- tl(MTs)],
    w("    true -> error({badarg, ~sT})~n  end,~n", [Name]),
    Acc;
arg_type_test(#param{name=_Name0, type=#type{base=eventType}}, _EOS, Acc) ->
    %% Name = erl_arg_name(Name0),
    %% w("  ~sBin = list_to_binary([atom_to_list(~s)|[0]]),~s", [Name,Name,EOS]),
    %% w("  ThisTypeBin = list_to_binary([atom_to_list(ThisT)|[0]]),~s", [EOS]),
    Acc;
arg_type_test(#param{name=_Name0,def=none,type=#type{base={term,_}}}, _EOS, Acc) ->
    %% Name = erl_arg_name(Name0),
    %% w("  wxe_util:send_bin(term_to_binary(~s)),~s", [Name,EOS]),
    Acc;
arg_type_test(#param{name=_Name0,type=#type{base=binary}},_EOS,Acc) ->
    %% Name = erl_arg_name(Name0),
    %% w("  wxe_util:send_bin(~s),~s", [Name,EOS]),
    Acc;
arg_type_test(#param{name=Name0,type=#type{name=Type,base=Base,single=Single}},EOS,Acc) ->
    if
	Type =:= "wxArtClient", Single =:= true ->
	    Name = erl_arg_name(Name0),
	    w("  ~s_UC = unicode:characters_to_binary([~s, $_, $C]),~s",
	      [Name,Name, EOS]);
	Base =:= string orelse (Type =:= "wxChar" andalso Single =/= true) ->
	    Name = erl_arg_name(Name0),
	    w("  ~s_UC = unicode:characters_to_binary(~s),~s", [Name,Name,EOS]);
	Type =:= "wxArrayString" ->
	    Name = erl_arg_name(Name0),
	    w("  ~s_UCA = [unicode:characters_to_binary(~sTemp) ||~s",
	      [Name,Name, EOS]),
	    w("              ~sTemp <- ~s],~s", [Name,Name,EOS]);
	true -> %% Not a string
	    ignore
    end,
    Acc;
arg_type_test(_,_,Acc) -> Acc.


patchArgName([Param|R1], [#param{name=Name}|R2]) ->
    [Param#param{name=Name}|patchArgName(R1,R2)];
patchArgName([],[]) -> [];
patchArgName(List, ignore) -> List.

have_return_vals(void, Ps) ->
    lists:any(fun(#param{in=In}) -> In =/= true end, Ps);
have_return_vals(#type{}, _) -> true.

gen_function_clause(Name0,MT,Ps,Optional,Variant) ->
    PArg = fun(Arg) ->
		   case lists:member(name_only, Variant) of
		       true -> func_arg_name(Arg);
		       false -> func_arg(Arg, true)
		   end
	   end,
    Args = args(PArg, ",", Ps),
    Name = case MT of constructor -> "new"; _ -> Name0 end,
    w("~s(~s",[Name,Args]),
    Opts = case Optional of
	       [] -> "";
	       empty_list when Args =:= [] -> "[]";
	       empty_list -> ", []";
	       _ when Args =:= [] -> "Options";
	       _ -> ", Options"
	   end,
    w("~s)", [Opts]),
    case lists:member(no_guards, Variant) of
	true ->  ok;
	false ->
	    Guards = args(fun guard_test/1, ",", Ps),
	    if
		Guards =:= [], Opts =:= "" -> w(" ->~n", []);
		Guards =:= [] -> w("~n when is_list(Options) ->~n", []);
		Opts =:= "" -> w("~n when ~s ->~n", [Guards]);
		true -> w("~n when ~s,is_list(Options) ->~n", [Guards])
	    end
    end.

split_optional(Ps) ->
    split_optional(Ps, [], []).
split_optional([P=#param{def=Def,in=In, where=Where}|Ps], Standard, Opts)
  when Def =/= none, In =/= false, Where =/= c ->
    split_optional(Ps, Standard, [P|Opts]);
split_optional([P=#param{def=Def,in=In, where=Where}|Ps], Standard, Opts)
  when Def =:= none, In =/= false, Where =/= c ->
    split_optional(Ps, [P|Standard], Opts);
split_optional([_|Ps], Standard, Opts) ->
    split_optional(Ps, Standard, Opts);
split_optional([], Standard, Opts) ->
    {reverse(Standard), reverse(Opts)}.

erl_args_count(Ps, W) ->
    {Args,Opts} = split_optional(Ps),
    OptLen = case Opts of
                 [] -> 0;
                 _ when W =:= erl_no_opt -> 0;
                 _ -> 1
             end,
    length(Args)+OptLen.

patch_param(P=#param{type=#type{base=Tuple}}, all) when is_tuple(Tuple) ->
    P#param{type={class,ignore}};
patch_param(P=#param{type={merged,_}}, _) ->
    P#param{type={class,ignore}};
patch_param(P=#param{type=#type{base={class,_}}},_) ->
    P#param{type={class,ignore}};
patch_param(P=#param{type=#type{base={ref,_}}},_) ->
    P#param{type={class,ignore}};
patch_param(P,_) -> P.

func_arg_name(#param{def=Def}) when Def =/= none -> skip;
func_arg_name(#param{in=false}) -> skip;
func_arg_name(#param{where=c}) -> skip;
func_arg_name(#param{name=Name}) ->
    erl_arg_name(Name).

func_arg(#param{def=Def}, _) when Def =/= none -> skip;
func_arg(#param{in=false}, _) -> skip;
func_arg(#param{where=c}, _) -> skip;
func_arg(#param{name=Name,type=#type{base=string}}, _) ->
    erl_arg_name(Name);
func_arg(#param{name=Name,type=#type{name="wxArrayString"}}, _) ->
    erl_arg_name(Name);
func_arg(#param{name=Name0,type=#type{base={class,_CN}, single=true}}, Used) ->
    Name = erl_arg_name(Name0),
    case Used of
        true -> "#wx_ref{type=" ++ Name ++ "T}=" ++ Name;
        false -> "#wx_ref{type=" ++ Name ++ "T}"
    end;
func_arg(#param{name=Name0,type=#type{base={ref,CN}, single=true}}, Used) ->
    Name = erl_arg_name(Name0),
    case Used of
        true -> "#wx_ref{type=" ++ CN ++ ", ref=" ++ Name++"Ref}=" ++ Name;
        false -> "#wx_ref{type=" ++ CN ++ ", ref=" ++ Name++"Ref}"
    end;
func_arg(#param{name=Name0,type={merged,_}}, Used) ->
    Name = erl_arg_name(Name0),
    case Used of
        true -> "#wx_ref{type=" ++ Name ++ "T}=" ++ Name;
        false -> "#wx_ref{type=" ++ Name ++ "T}"
    end;
func_arg(#param{name=Name,type=#type{base={enum,_}}}, Used) ->
    erl_arg_name(Name, Used);
func_arg(#param{name=Name,type=#type{base={comp,"wxColour",_Tup}, single=true}}, _) ->
    erl_arg_name(Name);
func_arg(#param{name=Name,type=#type{base={comp,"wxDateTime",[D,Mo,Y|Tup]}, single=true}}, _) ->
    N = erl_arg_name(Name),
    Doc = fun({_,V}) -> N++V end,
    "{{" ++ args(Doc, ",", [Y,Mo,D]) ++ "},{" ++ args(Doc, ",", Tup) ++ "}}";
func_arg(#param{name=Name,type=#type{name="wxArtClient", single=true}}, _) ->
    erl_arg_name(Name);
func_arg(#param{name=Name,type=#type{base={comp,_,Tup}, single=true}}, Used) ->
    N = erl_arg_name(Name, Used),
    Doc = fun({_,V}) -> N++V end,
    "{" ++ args(Doc, ",", Tup) ++ "}" ++
        case Used of
            true -> " = " ++ N;
            false -> ""
        end;
func_arg(#param{name=Name}, Used) ->
    erl_arg_name(Name, Used).


guard_test(#param{type=#type{base={class,_},single=true}}) -> skip;
guard_test(#param{def=Def}) when Def =/= none -> skip;
guard_test(#param{where=c})  -> skip;
guard_test(#param{in=In}) when In == false -> skip;
guard_test(#param{name=N, type=#type{base=string}}) ->
    "?is_chardata(" ++ erl_arg_name(N) ++")";
guard_test(#param{name=N, type=#type{name="wxArtClient"}}) ->
    "is_list(" ++ erl_arg_name(N) ++")";
guard_test(#param{name=N, type=#type{name="wxArrayString"}}) ->
    "is_list(" ++ erl_arg_name(N) ++")";
guard_test(#param{name=Name,type=#type{single=Single}})
  when Single =/= true->
    "is_list(" ++ erl_arg_name(Name) ++  ")";
guard_test(#param{name=N,type=#type{base=int}}) ->
    "is_integer(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=N,type=#type{base=int64}}) ->
    "is_integer(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=N,type=#type{base=long}}) ->
    "is_integer(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=N,type=#type{base=float}}) ->
    "is_number(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=N,type=#type{base=double}}) ->
    "is_number(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=N,type=#type{base=bool}}) ->
    "is_boolean(" ++ erl_arg_name(N) ++ ")";
%%guard_test(#param{name=_N,type=#type{name="wxDateTime"}}) ->
    %%     "tuple_size(" ++ erl_arg_name(N) ++ ") =:= 2";
guard_test(#param{name=N,type=#type{base=binary}}) ->
    "is_binary(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=Name,type=#type{base={enum,_}}}) ->
    "is_integer(" ++ erl_arg_name(Name) ++  ")";
guard_test(#param{name=Name,type=#type{base=eventType}}) ->
    "is_atom(" ++ erl_arg_name(Name) ++  ")";
guard_test(#param{name=_N,type=#type{base={term,_}}}) ->
    skip;
guard_test(#param{name=_N,type=#type{base={ref,_}}}) ->
    skip;
guard_test(#param{name=_N,type=#type{base={class,_}}}) ->
    skip;
guard_test(#param{name=_N,type={merged,_}}) ->
    skip;
guard_test(#param{name=N,type=#type{base={comp,"wxColour",_Tup}}}) ->
    "?is_colordata(" ++ erl_arg_name(N) ++ ")";
guard_test(#param{name=N,type=#type{base={comp,_,Tup}}}) ->
    Doc = fun({int,V}) -> "is_integer("++erl_arg_name(N)++V ++")";
	     ({int64,V}) -> "is_integer("++erl_arg_name(N)++V ++")";
	     ({double,V}) -> "is_number("++erl_arg_name(N)++V ++")"
	  end,
    args(Doc, ",", Tup);
guard_test(#param{name=N,type={class,ignore}}) ->
    "is_record(" ++ erl_arg_name(N)++ ", wx_ref)";
guard_test(T) -> ?error({unknown_type,T}).

gen_doc(_Class, [#method{method_type=destructor}]) ->  skip;
gen_doc(_Class,Ms=[#method{name=N,alias=A,params=Ps,where=erl_no_opt,method_type=MT}])->
    w("%% @equiv ", []),
    gen_function_clause(erl_func_name(N,A),MT,Ps,empty_list,[no_guards,name_only]),
    w("~n-spec ",[]),
    write_specs(Ms, "\n");
gen_doc(Class,Ms=[#method{name=N, type=T}|Rest])->
    %%doc_optional(Optional, normal),
    doc_link(Class, N),
    gen_overload_doc(Rest),
    Ps = lists:foldl(fun(#method{params=Ps}, Acc) -> Ps ++ Acc end,[],Ms),
    doc_enum_desc(lists:usort(doc_enum(T,Ps))),
    w("-spec ",[]),
    write_specs(Ms, "\n"),
    ok.

gen_overload_doc([]) -> ok;
%%gen_overload_doc(_) -> ok;
gen_overload_doc(Cs) ->
    w("%% <br /> Also:<br />~n%% ",[]),
    write_specs(Cs, "<br />\n%% "),
    w("~n", []).

write_specs(M=[#method{method_type=constructor}|_], Eol) ->
    w("new", []),
    write_specs1(M, Eol);
write_specs(M=[#method{name=N, alias=A}|_], Eol) ->
    w("~s", [erl_func_name(N,A)]),
    write_specs1(M, Eol).

write_specs1([M], Eol) ->
    write_spec(M, Eol),
    w(".~s", [Eol]);
write_specs1([M|Next], Eol) ->
    write_spec(M, Eol),
    w(";~s      ", [Eol]),
    write_specs1(Next, Eol).

write_spec(#method{params=Ps,type=T,where=erl_no_opt}, Eol) ->
    {NonDef, _Optional} = split_optional(Ps),
    Res = doc_return_types(T,Ps),
    write_spec(NonDef, [], Res, Eol);
write_spec(#method{params=Ps,type=T}, Eol) ->
    {NonDef, Optional} = split_optional(Ps),
    Res = doc_return_types(T,Ps),
    write_spec(NonDef, Optional, Res, Eol).

write_spec([], [], {simple, Res}, _Eol) ->
    w("() -> ~s", [Res]);
write_spec([], [], {complex, Res}, Eol) ->
    w("() -> Resultwhen~s\tResult ::~s", [Eol,Res]);
write_spec(Args, [], {simple, Res}, Eol) ->
    w("(~s) -> ~s when~s\t~s",
      [erl_arg_names(Args), Res, Eol, doc_arg_types(Args)]);
write_spec(Args, [], {complex, Res}, Eol) ->
    w("(~s) -> Result when~s\tResult ::~s,~s\t~s",
      [erl_arg_names(Args), Eol, Res, Eol, doc_arg_types(Args)]);
write_spec([], Optional, {simple, Res}, Eol) ->
    w("([Option]) -> ~s when~s\t~s",
      [Res, Eol, optional_type(Optional, Eol)]);
write_spec([], Optional, {complex, Res}, Eol) ->
    w("([Option]) -> Result when~s\tResult :: ~s,~s\t~s",
      [Eol, Res, Eol, optional_type(Optional, Eol)]);
write_spec(Args, Optional, {simple, Res}, Eol) ->
    w("(~s, [Option]) -> ~s when~s\t~s,~s\t~s",
      [erl_arg_names(Args), Res, Eol, doc_arg_types(Args), Eol, optional_type(Optional, Eol)]);
write_spec(Args, Optional, {complex, Res}, Eol) ->
    w("(~s, [Option]) -> Result when~s\tResult :: ~s,~s\t~s,~s\t~s",
      [erl_arg_names(Args), Eol, Res, Eol, doc_arg_types(Args), Eol, optional_type(Optional, Eol)]).

optional_type(Opts, Eol) ->
    "Option :: " ++ args(fun optional_type2/1, Eol++"\t\t | ", Opts).
optional_type2(#param{name=Name, def=_Def, type=T}) ->
    "{'" ++ erl_option_name(Name) ++ "', " ++ doc_arg_type2(T) ++ "}". %%   %% Default: " ++ Def.

doc_link("utils", Func) ->
    w("%% @doc See <a href=\"http://www.wxwidgets.org/manuals/2.8.12/wx_miscellany.html#~s\">"
      "external documentation</a>.~n",
      [lowercase_all(Func)]);
doc_link(Class, Func) ->
    w("%% @doc See <a href=\"http://www.wxwidgets.org/manuals/2.8.12/wx_~s.html#~s~s\">"
      "external documentation</a>.~n",
      [lowercase_all(Class),lowercase_all(Class),lowercase_all(Func)]).

erl_arg_names(Ps0) ->
    Ps = [Name || #param{name=Name, in=In, where=Where} <- Ps0,In =/= false, Where =/= c],
    args(fun erl_arg_name/1, ", ", Ps).

doc_arg_types(Ps0) ->
    Ps = [P || P=#param{in=In, where=Where} <- Ps0,In =/= false, Where =/= c],
    args(fun doc_arg_type/1, ", ", Ps).

doc_arg_type(T) ->
    doc_arg_type(T, in).

doc_arg_type(#param{name=Name,def=none,type=T}, Out) ->
    erl_arg_name(Name) ++ "::" ++ doc_arg_type2(T, Out);
doc_arg_type(#param{name=Name,in=false,type=T}, Out) ->
    erl_arg_name(Name) ++ "::" ++ doc_arg_type2(T, Out);
doc_arg_type(_, _) -> skip.

doc_arg_type2(T) ->
    doc_arg_type2(T, in).

doc_arg_type2(T=#type{single=Single}, Out) ->
    case Single of
        array -> "[" ++ doc_arg_type3(T, Out) ++ "]";
        list -> "[" ++ doc_arg_type3(T, Out) ++ "]";
        {list, _} -> "[" ++ doc_arg_type3(T, Out) ++ "]";
        true -> doc_arg_type3(T, Out)
    end;
doc_arg_type2(T, Out) ->
    doc_arg_type3(T, Out).

doc_arg_type3(#type{base=string}, in) -> "unicode:chardata()";
doc_arg_type3(#type{base=string}, out) -> "unicode:charlist()";
doc_arg_type3(#type{name="wxChar", single=S},in) when S =/= true -> "unicode:chardata()";
doc_arg_type3(#type{name="wxChar", single=S},out) when S =/= true -> "unicode:charlist()";
doc_arg_type3(#type{name="wxArrayString"},in) -> "unicode:chardata()";
doc_arg_type3(#type{name="wxArrayString"},out) -> "unicode:charlist()";
doc_arg_type3(#type{name="wxDateTime"}, _) ->    "wx:wx_datetime()";
doc_arg_type3(#type{name="wxArtClient"}, _) ->    "unicode:chardata()";
doc_arg_type3(#type{base=int}, _) ->        "integer()";
doc_arg_type3(#type{base=int64}, _) ->        "integer()";
doc_arg_type3(#type{base=long}, _) ->       "integer()";
doc_arg_type3(#type{name="wxTreeItemId"}, _) -> "wxTreeCtrl:treeItemId()";
doc_arg_type3(#type{base=bool}, _) ->       "boolean()";
doc_arg_type3(#type{base=float}, _) ->      "number()";
doc_arg_type3(#type{base=double}, _) ->     "number()";
doc_arg_type3(#type{base=binary}, _) ->     "binary()";
doc_arg_type3(#type{base={binary,_}}, _) -> "binary()";
doc_arg_type3(#type{base=eventType}, _) ->  "atom()";
doc_arg_type3(#type{base={ref,N}}, _) ->     N++"()";
doc_arg_type3(#type{base={term,_N}}, _) ->  "term()";
doc_arg_type3(T=#type{base={class,N}}, _) ->
    ClassType = check_class(T),
    Current = get(current_class),
    if N =:= Current -> N ++ "()";
       true -> ClassType
    end;
doc_arg_type3({merged,Classes}, _) ->
    CTs = [check_class(T) || {_, T=#type{base={class,_N1}},_} <- Classes],
    %% Curr = get(current_class),
    if
	%% N1 =:= Curr, N2 =:= Curr ->  N1++"()";
	%% N1 =:= Curr -> N1++"() | "++ CT2;
	%% N2 =:= Curr -> CT1 ++ " | "++ N2++"()";
	true ->
            lists:flatten(lists:join(" | ", CTs))
            %% CT1 ++ " | " ++ CT2
    end;
%% doc_arg_type3(#type{base={enum,{_,N}}}, _) ->    uppercase(N);
%% doc_arg_type3(#type{base={enum,N}}, _) ->    uppercase(N);
doc_arg_type3(#type{base={enum,_N}}, _) -> "wx:wx_enum()";
doc_arg_type3(#type{base={comp,"wxColour",_Tup}}, in) ->
    "wx:wx_colour()";
doc_arg_type3(#type{base={comp,"wxColour",_Tup}}, out) ->
    "wx:wx_colour4()";
doc_arg_type3(#type{base={comp,"wxGraphicsGradientStop",_Tup}}, out) ->
    "{wx:wx_colour4(), float()}";
doc_arg_type3(#type{base={comp,_,{record,Name}}}, _) ->
    "wx:wx_" ++ atom_to_list(Name) ++ "()";
doc_arg_type3(#type{base={comp,_,Tup}}, _) ->
    Doc = fun({int,V}) -> V ++ "::integer()";
	     ({double,V}) -> V ++ "::float()"
	  end,
    "{" ++ args(Doc, ", ", Tup) ++ "}";
doc_arg_type3(T, _) -> ?error({unknown_type,T}).

doc_return_types(T, Ps) ->
    doc_return_types2(T, [P || P=#param{in=In} <- Ps,In =/= true]).
doc_return_types2(void, []) ->    {simple, "'ok'"};
doc_return_types2(void, [#param{type=T}]) ->     {simple, doc_arg_type2(T, out)};
doc_return_types2(T, []) ->                      {simple, doc_arg_type2(T, out)};
doc_return_types2(void, Ps) when length(Ps) < 4 ->
    {simple, "{" ++ args(fun(Arg) -> doc_arg_type(Arg, out) end,", ",Ps) ++ "}"};
doc_return_types2(void, Ps) ->
    {complex, "{" ++ args(fun(Arg) -> doc_arg_type(Arg, out) end,", ",Ps) ++ "}"};
doc_return_types2(T, Ps) ->
    {complex, "{Res ::" ++ doc_arg_type2(T, out) ++ ", " ++
	 args(fun(Arg) -> doc_arg_type(Arg, out) end,", ",Ps) ++ "}"}.

doc_enum(#type{base={enum,Enum}},Ps) ->
    [doc_enum_type(Enum, "res") |
     [doc_enum_type(Type,Name) || #param{name=Name, type=#type{base={enum,Type}}} <- Ps]];
doc_enum(_,Ps) ->
    [doc_enum_type(Type,Name) || #param{name=Name, type=#type{base={enum,Type}}} <- Ps].

doc_enum_type(Type, Name) ->
    try
	{Enum0, #enum{vals=Vals}} = wx_gen:get_enum(Type),
	Enum = case Enum0 of {_, E} -> E; E -> E end,
	Consts = get(consts),
	Format = fun({N,_What}) ->
			 #const{name=N} = gb_trees:get(N, Consts),
			 "?" ++ enum_name(N)
		 end,
	Vs = args(Format, " | ", Vals),
	{uppercase(Enum),Name, Vs}
    catch _:_ ->
	    io:format("Warning missing enum type ~p~n", [Type]),
	    {uppercase(Type),Name,"integer"}
    end.

doc_enum_desc([]) -> ok;
doc_enum_desc([{_Enum,Name,Vs}|R]) ->
    w("%%<br /> ~s = ~s~n", [erl_arg_name(Name),Vs]),
    doc_enum_desc(R).

%% Misc functions prefixed with wx
erl_func_name(#method{method_type=constructor}) ->
    "new";
erl_func_name(#method{name=N, alias=A}) ->
    erl_func_name(N, A).

erl_func_name("wx" ++ Name, undefined) ->   check_name(lowercase(Name));
erl_func_name(Name, undefined) ->   check_name(lowercase(Name));
erl_func_name(_, Alias) -> check_name(lowercase(Alias)).

erl_option_name(Name) -> lowercase(Name).

erl_arg_name(Name, false) -> "_" ++ Name;
erl_arg_name(Name, true) -> erl_arg_name(Name).
erl_arg_name(Name) ->    uppercase(Name).

check_name("destroy") -> "'Destroy'";
check_name("xor") -> "'Xor'";
check_name("~" ++ _Name) -> "destroy";
check_name(Name) -> Name.

marshal_opts([],_) -> "";     %% No opts skip this!
marshal_opts(Opts, []) ->
    marshal_opts_0(Opts),
    "Opts";
marshal_opts(Opts, _) ->
    marshal_opts_0(Opts),
    ", Opts".

marshal_opts_0(Opts) ->
    w("  MOpts = fun", []),
    marshal_opts1(Opts),
    w(";~n          (BadOpt) -> erlang:error({badoption, BadOpt}) end,~n", []),
    w("  Opts = lists:map(MOpts, Options),~n",[]).

marshal_opts1([P]) ->
    marshal_opt(P);
marshal_opts1([P|R]) ->
    marshal_opt(P),
    w(";~n          ", []),
    marshal_opts1(R).

marshal_opt(P0=#param{name=Name,type=Type}) ->
    P = P0#param{def=none},
    Opt = erl_option_name(Name),
    N = erl_arg_name(Name),
    Arg = marshal_arg(Type,N),
    case Arg of
	N ->
            w("({~s, ~s} = Arg) -> ", [Opt, func_arg(P, false)]),
            arg_type_test(P,"",[]),
	    w("Arg",[]);
	_ ->
            w("({~s, ~s}) -> ", [Opt, func_arg(P, false)]),
            arg_type_test(P,"",[]),
	    w("{~s,~s}", [Opt, Arg])
    end.

marshal_args(Ps) ->
    marshal_args(Ps, []).

marshal_args([#param{where=erl}|Ps], Margs) ->
    marshal_args(Ps, Margs);
marshal_args([#param{name=_N,where=c}|Ps], Margs) ->
    %% io:format("~p:~p: skip ~p~n",[get(current_class),get(current_func),_N]),
    marshal_args(Ps, Margs);
marshal_args([#param{in=false}|Ps], Margs) ->
    marshal_args(Ps, Margs);
marshal_args([#param{def=Def}|Ps], Margs) when Def =/= none ->
    marshal_args(Ps, Margs);
marshal_args([#param{name=Name, type=Type}|Ps], Margs) ->
    Arg = marshal_arg(Type, erl_arg_name(Name)),
    marshal_args(Ps, [Arg|Margs]);
marshal_args([],Margs) ->
    args(fun(Str) -> Str end, ",", reverse(Margs)).

marshal_arg(#type{name="wxChar", single=Single}, Name)
  when Single =/= true ->
    Name ++ "_UC";
marshal_arg(#type{base=string}, Name) ->
    Name ++ "_UC";
marshal_arg(#type{name="wxArrayString"}, Name) ->
    Name ++ "_UCA";
marshal_arg(#type{single=true,base={comp,"wxColour",_Comp}}, Name) ->
    "wxe_util:color(" ++ Name ++ ")";
marshal_arg(#type{single=true,base={comp,"wxDateTime",Comp}}, Name) ->
    N = erl_arg_name(Name),
    Doc = fun({_,V}) -> N++V end,
    "{" ++ args(Doc, ",", Comp) ++ "}";
marshal_arg({merged, _}, Name) ->
    "wx:typeCast(" ++ Name ++ ", " ++ Name ++ "Type)";
marshal_arg(_, Name) ->
    Name.

enum_name(Name) ->
    case enum_split(Name) of
	{undefined, _} -> Name;
	{_C, ErlName} -> ErlName
    end.

enum_split(Name) ->
    case string:tokens(Name, ":") of
	[Name] -> {undefined, Name};
	[C,N] ->  {C, enum_name(C,N)}
    end.

enum_name(undefined, Name) -> Name;
enum_name(Enum, Name) -> Enum ++ "_" ++ Name.

enum_name_c(undefined, Name) -> Name;
enum_name_c(Enum, Name) -> Enum ++ "::" ++ Name.

const_name("WXK" ++ Name) -> "wxk" ++ Name;
const_name("WX" ++ Name) -> "wx" ++ Name;
const_name(Name) -> enum_name(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_enums_ints() ->
    %% open_write("../include/wx.hrl"), opened in gen_event_recs
    w("~n%% Hardcoded Records~n", []),
    w("-record(wxMouseState, {x :: integer(), y :: integer(),~n"
      "          leftDown :: boolean(), middleDown :: boolean(), rightDown :: boolean(), ~n"
      "          controlDown :: boolean(), shiftDown :: boolean(),~n"
      "          altDown :: boolean(), metaDown :: boolean(), cmdDown :: boolean(),~n"
      "          aux1Down :: boolean(), aux2Down :: boolean()~n"
      "        }).~n", []),
    w("-record(wxHtmlLinkInfo, {~n"
      "          href :: unicode:chardata(), target :: unicode:chardata()~n"
      "        }).~n", []),
    w("~n%% Hardcoded Defines~n", []),
    Enums = [E || {{enum,_},E = #enum{as_atom=false}} <- get()],
    w("-define(wxDefaultCoord, -1).~n", []),
    w("-define(wxDefaultSize, {-1,-1}).~n", []),
    w("-define(wxDefaultPosition, {-1,-1}).~n", []),
    w("~n%% Global Variables~n", []),
    Done = foldl(fun({Gvar,_}, Done) ->
                         w("-define(~s,  wxe_util:get_const(~s)).~n", [qoute_atom(Gvar), qoute_atom(Gvar)]),
                         gb_sets:add(Gvar, Done)
                 end, gb_sets:empty(), get(gvars)),
    w("~n%% Enum and defines~n", []),
    foldl(fun(Enum= #enum{vals=Vals}, Acc) when Vals =/= [] ->
		  build_enum_ints(Enum,Acc);
	     (_,Acc) -> Acc
	  end, Done, lists:sort(Enums)),
    close().

qoute_atom([Char|_]=Str) when Char < $a ->
    "'" ++ Str ++ "'";
qoute_atom(Str) ->
    Str.


enum_from({File, undefined, [$@|_]}) ->
    io_lib:format(" From \"~s.h\"",[File]);
enum_from({File, undefined, Name}) ->
    io_lib:format(" From \"~s.h\": ~s",[File, Name]);
enum_from({_File, Class,[$@|_]}) ->
    io_lib:format(" From class ~s",[Class]);
enum_from({_File, Class, Name}) ->
    io_lib:format(" From class ~s::~s",[Class, Name]).

build_enum_ints(#enum{from=From, vals=Vals},Done) ->
    w("%%% ~s~n", [enum_from(From)]),

    Consts = get(consts),
    Ignore = fun(Name) ->
		     case gb_trees:lookup(Name, Consts) of
			 {value, Const} -> Const;
			 none -> true
		     end
	     end,

    Format = fun(#const{name="wxEVT_" ++ _}) ->
		     ignore; %% Ignore event macros they are not valid in our event model
		(#const{name=Name,val=Value,is_const=true}) when is_number(Value) ->
		     w("-define(~s, ~p).~n", [enum_name(Name),Value]);
		(#const{name=Name,val=Value,is_const=false}) when is_number(Value) ->
		     w("-define(~s, wxe_util:get_const(~s)).~n", [enum_name(Name),const_name(Name)]);
		(#const{name=Name,val={Str,0}}) ->
		     {EnumClass, EnumName} = enum_split(Name),
		     case string:tokens(Str, " |()") of
			 [Token] ->
			     w("-define(~s, ~s).~n", [EnumName,const_value(Token, EnumClass, Ignore)]);
			 Tokens ->
			     Def = args(fun(T) -> const_value(T, EnumClass, Ignore) end, " bor ", Tokens),
			     w("-define(~s, (~s)).~n", [EnumName, Def])
		     end;
		(#const{name=Name,val={Str,N}}=Const) ->
		     {EnumClass, EnumName} = enum_split(Name),
		     case string:tokens(Str, " |()") of
			 [Token] ->
			     w("-define(~s, (~s+~p)).~n", [EnumName,const_value(Token, EnumClass, Ignore),N]);
                         _Tokens ->
                             io:format("~p ~p~n", [Str, N]),
                             exit(Const)
		     end
	     end,

    Write = fun({Name,_What}, Skip) ->
		    case gb_sets:is_member(Name,Skip) orelse Ignore(Name) of
			true -> Skip;
			Const ->
			    try Format(Const)
			    catch {unknown_value, _Error} ->
				    io:format("Const ~s uses unknown define ~p ignoring~n", [Name, _Error]),
				    ok
			    end,
			    gb_sets:add(Name,Skip)
		    end
	    end,
    lists:foldl(Write, Done, Vals).

const_value("SHRT_MIN",_,_) -> "-16#8000";
const_value("INT_MAX",_,_) -> "16#7FFFFFFF";
const_value(V,_,_) when is_integer(V) -> integer_to_list(V);
const_value(V = "16#" ++ IntList,_,_) ->
    _ = list_to_integer(IntList, 16), %% ASSERT
    V;
const_value(V0, EnumClass, Ignore) ->
    try
	_ = list_to_integer(V0),
	V0
    catch _:_ ->
	    EEnum = enum_name(EnumClass, V0),
	    CEnum = enum_name_c(EnumClass, V0),
	    case Ignore(CEnum) of
		true when CEnum == V0 ->
		    throw({unknown_value, EEnum});
		true ->
		    case Ignore(V0) of
			true -> throw({unknown_value, EEnum});
			_ -> [$?|V0]
		    end;
		_ -> [$?|EEnum]
	    end
    end.

gen_event_recs() ->
    open_write("../include/wx.hrl"),
    erl_copyright(),
    w("", []),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%%  All event messages are encapsulated in a wx record~n"
      "%%  they contain the widget id and a specialized event record.~n"
      "%%  Each event record may be sent for one or more event types.~n"
      "%%  The mapping to wxWidgets is one record per class.~n~n",[]),
    w("-record(wx, {id   :: integer(),         %% Integer Identity of object.~n"
      "             obj  :: wx:wx_object(),    %% Object reference that was used in the connect call.~n"
      "             userData :: term(),        %% User data specified in the connect call.~n"
      "             event :: event()           %% The event record~n"
      "            }).~n~n", []),
    w("-type wx() :: #wx{}. %% wx event record ~n",[]),
    w("%% Here comes the definitions of all event records.~n"
      "%% they contain the event type and possible some extra information.~n~n",[]),
    Events = [build_event_rec(C) || {_,C=#class{event=Evs}} <- lists:sort(get()), Evs =/= false],
    EventSubTypes = [Type || {_Rec, Type} <- Events],
    EventRecs = [Rec || {Rec, _Type} <- Events],
    w("-type event() :: ~s.~n~n",
      [args(fun(Ev) -> Ev++"()" end, " | ", lists:sort(EventRecs))]),

    w("-type wxEventType() :: ~s.~n",
      [args(fun(Ev) -> Ev++"()" end, " | ", lists:sort(EventSubTypes))]),
    %% close(), closed in gen_enums_ints
    ok.

build_event_rec(Class=#class{name=Name, event=Evs}) ->
    % io:format("~p ~p~n", [Name, Class]),
    Attr = filter_attrs(Class),
    Rec = event_rec_name(Name),
    %%GetName = fun(#param{name=N}) ->event_attr_name(N) end,
    GetType = fun(#param{name=N,type=T}) ->
		      event_attr_name(N) ++ " :: " ++ doc_arg_type2(T)
	      end,
    EventType = Name ++ ":" ++ Name ++ "Type",
    case Attr =:= [] of
	true ->
	    w("-record(~s, {type :: ~s()}). %% Callback event: ~s~n",
	      [Rec, EventType, Name]);
	false ->
	    w("-record(~s,{type :: ~s(), %% Callback event:  ~s~n\t~s}).~n",
	      [Rec,EventType, Name, args(GetType,",\n\t",Attr)])
    end,
    w("-type ~s() :: #~s{}. %% Callback event: {@link ~s}~n~n", [Rec,Rec,Name]),
    {Name ++ ":" ++ Rec, EventType}.

event_rec_name(Name0 = "wx" ++ _) ->
    "tnevE" ++ Name1 = reverse(Name0),
    reverse(Name1).

event_type_name({EvN,_,_}) -> event_type_name(EvN);
event_type_name({EvN,_}) -> event_type_name(EvN);
event_type_name(EvN) ->
    "wxEVT_" ++ Ev = atom_to_list(EvN),
    lowercase_all(Ev).

event_attr_name("m_" ++ Attr) ->
    lowercase(Attr);
event_attr_name(Attr) ->
    lowercase(Attr).

find_inherited_attr(Param = {PName,_}, Name) ->
    case get({class, Name}) of
        #class{parent=Parent, attributes=Attrs} ->
            case lists:keysearch(atom_to_list(PName), #param.name, Attrs) of
                {value, P=#param{}} -> P;
                _ ->
                    io:format("Missed ~p in ~p~n  ~p~n", [PName, Name, Attrs]),
                    find_inherited_attr(Param, Parent)
            end;
        _ ->
            exit({class_not_found, Name})
    end.

filter_attrs(#class{name=Name, parent=Parent,attributes=Attrs}) ->
    Attr1 = lists:foldl(fun(#param{acc=skip},Acc) -> Acc;
			   (P=#param{prot=public},Acc) -> [P|Acc];
			   (#param{acc=undefined},Acc) -> Acc;
			   ({inherited, PName},Acc) ->
				case find_inherited_attr(PName, Parent) of
				    undefined ->
					io:format("~p:~p: Missing Event Attr ~p in ~p~n",
						  [?MODULE,?LINE, PName, Name]),
					Acc;
				    P ->
					[P|Acc]
				end;
			   (P, Acc) -> [P|Acc]
			end, [], Attrs),
    lists:reverse(Attr1).

gen_funcnames() ->
    Ns = get_unique_names(),
    open_write("../src/gen/wxe_funcs.hrl"),
    erl_copyright(),
    w("%% This file is generated DO NOT EDIT~n~n", []),
    w("%% We define each id so we don't get huge diffs when adding new funcs/classes~n~n",[]),
    [w("-define(~s_~s, ~p).~n", [Class,Name,Id]) || {Class,Name,_,Id} <- Ns],
    close().

gen_api_footprint(All) ->
    %% To be able to diff content between old and new generators.
    open_write("wx_30_api.dump"),
    TF = fun TF (Type) ->
                 case Type of
                     void -> ok;
                     {merged, MTs} ->
                         lists:sort([TF(MT) || {_, MT,_} <- MTs]);
                     _ ->
                         case wx_gen:type_foot_print(Type) of
                             {class, C} -> list_to_atom(C);
                             color -> {int,int,int,int};
                             T -> T
                         end
                 end
         end,
    Out = fun(T, []) -> TF(T);
             (void, [#param{type=T}]) -> TF(T);
             (void, Ps) -> list_to_tuple([TF(T) || #param{type=T} <- Ps]);
             (T, Ps) -> list_to_tuple([TF(T)|[TF(Tp) || #param{type=Tp} <- Ps]])
          end,
    Methods = fun(CName, Parent, #method{name=N, alias=Alias,where=W,type=Type0,params=Ps}) ->
                      {Args0,Opts0} = split_optional(Ps),
                      Type = Out(Type0, [P || P=#param{in=In} <- Ps,In =/= true]),
                      Args = [TF(PT) || #param{type=PT} <- Args0],
                      Opts = case W of
                                 erl_no_opt -> [];
                                 _ -> lists:sort([{list_to_atom(erl_option_name(ON)), TF(OT)} ||
                                                     #param{name=ON,type=OT} <- Opts0])
                             end,
                      {CName, list_to_atom(erl_func_name(N,Alias)), Type, Args, Opts, W, Parent}
              end,
    Class = fun(#class{name=CName0,parent=Parent,methods=Ms0}) ->
                    CName = case Parent of
                                "static" -> "wx_misc";
                                _ -> CName0
                            end,
                    [Methods(list_to_atom(CName), list_to_atom(Parent), M) || Ms <- Ms0, M <-Ms]
            end,
    Sorted = lists:sort(lists:append([Class(C) || C <- All])),
    [w("~0p.~n", [E]) || E <- Sorted],
    close().

get_unique_name(ID) when is_integer(ID) ->
    Tree =  get(unique_names),
    {Class,Name, _,_} = gb_trees:get(ID, Tree),
    Class ++ "_" ++ Name.

get_unique_names() ->
    Tree =  get(unique_names),
    gb_trees:values(Tree).

gen_unique_names(Defs) ->
    Names = [ unique_names(Ms, Class) || #class{name=Class, methods=Ms} <- Defs],
    Data =  [{Id, Struct} || Struct = {_,_,_,Id} <- lists:append(Names)],
    Tree = gb_trees:from_orddict(lists:sort(Data)),
    put(unique_names, Tree).

unique_names(Ms0, Class) ->
    Ms1 = [M || M = #method{where = W} <- lists:append(Ms0),
		W =/= erl_no_opt, W =/= erl_alias],
    Ms2 = lists:keysort(#method.name, Ms1),
    Ms  = split_list(fun(#method{name=N}, M) -> {N =:= M, N} end, undefined, Ms2),
    unique_names2(Ms,Class).
%% by Names
unique_names2([[#method{id=Id, name=Method,alias=Alias, max_arity=A}]|Ms], Class) ->
    [{Class,uname(alias(Method,Alias),Class),A,Id} | unique_names2(Ms,Class)];
unique_names2([Ms0|RMs], Class) ->
    Split = fun(#method{max_arity=A}, P) -> {A =:= P, A} end,
    Ms = split_list(Split, 0, Ms0),
    unique_names3(Ms, Class) ++ unique_names2(RMs, Class);
unique_names2([], _Class) -> [].
%% by Arity
unique_names3([[#method{id=Id, name=Method, alias=Alias, method_type=MT, max_arity=A}]|Ms], Class) ->
    ST = case MT of
             static -> "_STAT";
             _ -> ""
         end,
    [{Class,uname(alias(Method,Alias),Class) ++ ST ++ "_" ++ integer_to_list(A),A,Id} | unique_names3(Ms,Class)];
unique_names3([Ms0|RMs], Class) ->
    unique_names4(Ms0, 0, Class) ++ unique_names3(RMs, Class);
unique_names3([], _Class) -> [].

unique_names4([#method{id=Id, name=Method,alias=Alias, max_arity=A}|Ms], C, Class) ->
    [{Class,uname(alias(Method,Alias),Class) ++ "_" ++ integer_to_list(A) ++ "_" ++ integer_to_list(C),A,Id}
     | unique_names4(Ms,C+1,Class)];
unique_names4([], _, _Class) -> [].

alias(Method, undefined) -> Method;
alias(_, Alias) -> Alias.

uname(Class,Class) ->   "new";
uname([$~ | _], _  ) -> "destruct";
uname(Name, _) -> Name.

split_list(F, Keep, List) ->
    split_list(F, Keep, List, []).

split_list(F, Keep, [M|Ms], Acc) ->
    case F(M,Keep) of
	{true, Test} ->
	    split_list(F, Test, Ms, [M|Acc]);
	{false, Test} when Acc =:= [] ->
	    split_list(F, Test, Ms, [M]);
	{false, Test} ->
	    [lists:reverse(Acc)|split_list(F, Test, Ms, [M])]
    end;
split_list(_, _, [], []) -> [];
split_list(_, _, [], Acc) -> [lists:reverse(Acc)].

