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
%% Api wrapper generator

-module(wx_gen). 
-export([code/0,xml/0]).

-include("wx_gen.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-import(lists, [foldl/3,foldr/3,reverse/1,keysearch/3,map/2,filter/2,droplast/1]).
-import(proplists, [get_value/2,get_value/3]).

-compile(export_all).

-define(DBGCF(Class, Func, Format, Args),
	case {get(current_class), get(current_func)} of
	    {Class, Func} -> io:format("~p:~p: " ++ Format, [?MODULE,?LINE] ++ Args);
	    _ -> ok
	end).

code() ->  safe(fun gen_code/0,true).
xml()  ->  safe(fun gen_xml/0,true). 

devcode() -> erase(),safe(fun gen_code/0,false).

safe(What, QuitOnErr) ->
    try 
	What(),
	io:format("Completed successfully~n~n", []),
	QuitOnErr andalso gen_util:halt(0)
    catch Err:Reason:Stacktrace ->
	    io:format("Error in ~p ~p~n", [get(current_class),get(current_func)]),
	    erlang:display({Err,Reason,Stacktrace}),
	    catch gen_util:close(),
	    QuitOnErr andalso gen_util:halt(1)
    end.

gen_code() ->
    {ok, Defs0} = file:consult("wxapi.conf"),
    %% {ok, Defs0} = file:consult("test.conf"),
    erase(func_id),
    put(class_id, 10), %% Start from 10 using the other as special
    Defs1 = init_defs(Defs0),
    Defs2 = parse_defs(Defs1, []),
    parse_enums([File || {{include, File},_} <- get()]),
    Defs = translate_enums(Defs2),
    wx_gen_erl:gen(Defs),
    wx_gen_cpp:gen(Defs),
    ok.

gen_xml() ->
%%     {ok, Defs} = file:consult("wxapi.conf"),
    
%%     Rel = droplast(os:cmd("wx-config --release")),
%%     Dir = " /usr/include/wx-" ++ Rel ++ "/wx/",
%%     Files0 = [Dir ++ File || {class, File, _, _, _} <- Defs],
%%     Files1 = [Dir ++ File || {doxygen, File} <- Defs],
%%     ok = file:write_file("wxapi.files", list_to_binary("INPUT = "++Files0++Files1)),
    ok.
    
-record(hs,{alias,skip,fs,fopt,ev,acc,info}).

init_defs(List0) ->
    [mangle_info(L) || L <- to_lists(List0)].

mangle_info(E={enum,Type0,SkipStr}) ->
    Type = case is_atom(Type0) of true -> atom_to_list(Type0); false -> Type0 end,
    put({enum, Type}, #enum{skip=SkipStr,as_atom=false}), %% as_atom=true}),
    E;
mangle_info(E={const_skip,List}) ->
    put(const_skip, [atom_to_list(M) || M <- List]),
    E;
mangle_info(E={not_const,List}) ->
    put(not_const,  [atom_to_list(M) || M <- List]),
    E;
mangle_info(E={gvars,List}) ->
    A2L = fun({N,{test_if,C}}) -> {atom_to_list(N), {test_if,C}};
             ({N,{T,C}}) -> {atom_to_list(N), {T,atom_to_list(C)}};
	     ({N,C}) ->     {atom_to_list(N), atom_to_list(C)}
	  end,
    put(gvars, map(A2L,List)),
    E;
mangle_info({class,CN,P,O,FL}) ->
    Alias  = get_value(alias,O, []),
    Skip   = get_value(skip, O, []),
    Event  = get_value(event,O, false),
    Acc    = get_value(acc, O, []),
    {Fs,Fopts} = foldr(fun(FWO={F,FO},{Fl,Fopt}) when is_list(FO) ->
			       Opt = case F of
					 {Name, ArgLen} when is_integer(ArgLen) ->
					     {Name, FO};
					 _ -> FWO
				     end,
			       {[F|Fl],[Opt|Fopt]};
			  (F,{Fl,Fopt}) ->
			       {[F|Fl], Fopt}
		       end, {[],[]}, FL),
    {class,CN,P,#hs{alias=Alias,skip=Skip,fs=Fs,ev=Event,acc=Acc,info=O,
		    fopt=gb_trees:from_orddict(lists:sort(Fopts))}}.

to_lists(Defs) ->
    map(fun({class,C,P,O,Fs}) ->
		{class,atom_to_list(C),atom_to_list(P),to_lists2(O),to_lists2(Fs)};
	   (Skip) -> Skip
	end, Defs).

to_lists2(List) ->
    map(fun(Skip = {const_skip, _List}) -> Skip;
	   (Skip = {not_const, _List}) -> Skip;
	   (Skip = {skip, _List}) -> Skip;
	   (Skip = {event, _List}) -> Skip;
	   (Skip = {acc, _List}) -> Skip;
	   (Skip = {doc, _List}) -> Skip;
	   (Skip = taylormade) -> Skip;
	   (Skip = {ifdef,_}) -> Skip;
	   (Skip = {erl_func, _Name})  -> Skip;
	   ({alias, AList}) -> {alias, [{atom_to_list(A),atom_to_list(B)} || {A,B} <- AList]};
	   (Else) when is_atom(Else) -> atom_to_list(Else);
	   ({Else,W}) when is_atom(Else) -> {atom_to_list(Else),W};
	   ({{Else,W},O}) when is_atom(Else) -> {{atom_to_list(Else),W},O};
	   (Else) -> Else
	end, List).

parse_defs([{class,Name,Parent,Info}|Rest], Acc0) ->
    {FileName, Type} = 
	case Parent of
	    "static" -> {Name ++ "_8h", static};
	    _ ->        {"class" ++ Name, class}
	end,
    Tab   = ets:new(defs, [bag]), 
    Defs0 = load_members(FileName, Name, gb_trees:empty(), Tab, Type, Info),
        
    put(current_class, Name),
    Class0 = #class{name=name(Name,Info), parent=Parent,
		    doc=get_value(doc,Info#hs.info,undefined),
		    file=FileName, options=Info#hs.info, id=next_id(class_id)},
    ParseClass = fun(Member,{Class,Dfs}) -> 
			 parse_class(Member,Tab,Dfs,Class,Info)
		 end,
    {Class1,Defs} = foldl(ParseClass,{Class0,Defs0},Info#hs.fs),
    
    Class2 = case Info#hs.ev of 
		 false -> Class1;
		 Ev -> parse_attr(gb_trees:to_list(Defs), Class1, Ev, Info)
	     end,
    Class = meta_info(Class2),
    erase(current_class),
    [erase(Del) ||  {Del = {loaded, _},_} <- get()],
    %% ets:delete(Tab),  keep it for debugging
    parse_defs(Rest, [Class|Acc0]);
parse_defs([_|Rest], Acc) ->
    parse_defs(Rest, Acc);
parse_defs([], Acc) -> reverse(Acc). 

meta_info(C=#class{name=CName,methods=Ms0}) ->
    Ms = lists:append(Ms0),
    HaveConstructor = lists:keymember(constructor, #method.method_type, Ms),
    case keysearch(destructor, #method.method_type, Ms) of
	false when HaveConstructor -> 
	    Dest = #method{name = "destroy", id = next_id(func_id),
			   method_type = destructor, params = [this(CName)]},
	    C#class{methods = [[Dest]|Ms0]};
	false ->
	    C#class{abstract = true};
	_ -> 
	    C
    end.
	    
parse_class(Member0,Tab,Defs0,Class = #class{name=CName},Opts) ->
    {Member,NoArgs} = case Member0 of 
			  {_, _} ->  Member0; 
			  _ -> 	     {Member0,all}
		      end,
    case ets:lookup(Tab, Member) of
	[] -> 
	    case Member of
		[$~|_] -> ignore; 
		_ ->
		    ?warning("Skipped Member ~p in ~p (not found in ~p)~n", 
			     [Member,CName,Tab])
	    end,
	    {Class,Defs0};
	Ms ->
	    case select_member(Ms, Class, Defs0, Opts) of
		{[],Defs} -> 
		    ?warning("Skipped Member ~p in ~p (not found in base)~n", 
			     [Member,CName]),
		    {Class,Defs};
		{Selected,Defs} ->
		    Parsed = parse_members(Member,Selected,Defs,CName,Opts),
		    {add_method(Parsed,NoArgs,Class,Opts), Defs}
	    end
    end.

parse_members(MemberName, Members, Defs, Class, Opts) ->
    ParseAll = 
	fun(Member,Acc) ->
		try 
		    case gb_trees:lookup(Member, Defs) of
			{value,#xmlElement{name=memberdef,attributes=Attrs,
					   content=Data}} -> 
			    MType = case keysearch(static,#xmlAttribute.name,Attrs) of
					{value, #xmlAttribute{value = "yes"}} -> 
					    static;
					_ ->
					    member 
				    end,
			    Virtual = 
				case keysearch(virt,#xmlAttribute.name,Attrs) of
				    {value, #xmlAttribute{value = "virtual"}} -> 
					true;
				    {value, #xmlAttribute{value = "non-virtual"}} -> 
					false;
				    _ ->
					undefined
				end,
			    [parse_member(Data,MType,Virtual,Opts)|Acc];
			none -> 			    
			    Acc;
			_Hmm ->
			    Acc
		    end
		catch throw:skip_member ->
			Acc
		end
	end,
    case foldl(ParseAll,[],Members) of
	[] -> 
	    ?warning("Skipped ~p No public def found in ~p ~n", 
		     [MemberName,Class]),
	    io:format("~p ~p~n",[MemberName, Members]),
	    [];
	Res -> 
	    Res
    end.
	    

parse_attr(Defs, Class, Ev, Info = #hs{acc=AccList0}) ->
%    io:format("Parsing Class ~p~n", [Class#class.name]),
    {Attrs, AccList} = parse_attr1(Defs, AccList0, Info, []),
    case AccList of 
	[] -> 
	    Class#class{attributes=Attrs, event=Ev};
	_ ->
	    Inherited = [{inherited, Inherit} || Inherit <- AccList],
	    Class#class{attributes=Attrs++Inherited, event=Ev}
    end.
    
parse_attr1([{{attr,_}, #xmlElement{content=C, attributes=Attrs}}|R], AttrList0, Opts, Res) ->    
    Parse  = fun(Con, Ac) -> parse_param(Con, Opts, Ac) end,
    Param0 = foldl(Parse, #param{}, drop_empty(C)),
    case Param0 of
	#param{where=nowhere} ->
	    parse_attr1(R,AttrList0,Opts,Res);
	_ ->
	    {value, #xmlAttribute{value=Type}} =
                keysearch(prot, #xmlAttribute.name, Attrs),
            {Param,AttrList} = attr_acc(Param0, list_to_atom(Type), AttrList0),
            parse_attr1(R,AttrList,Opts,[Param|Res])
    end;
parse_attr1([{_Id,_}|R],AttrList,Info, Res) ->
    parse_attr1(R,AttrList,Info, Res);
parse_attr1([],Left,_, Res) ->
    {reverse(Res), Left}.

attr_acc(#param{name=N}=P, Type, List) ->
    Name = list_to_atom(N),
    case get_value(Name, List, undefined) of
	undefined -> {P#param{in=false,prot=Type,acc=undefined}, List};
	Val when is_list(Val), is_integer(hd(Val)) ->
            %% Function String
            {P#param{in=false,prot=Type,acc=Val}, lists:keydelete(Name,1,List)};
        OptList when is_list(OptList) ->
            Param = foldl(fun handle_param_opt/2,P,OptList),
            {Param#param{in=false,prot=Type,acc=undefined},
             lists:keydelete(Name,1,List)};
        Val ->
            {P#param{in=false,prot=Type,acc=Val}, lists:keydelete(Name,1,List)}
    end.

load_members(FileName, Class, Defs, Tab, Type,Opts) ->
    File = filename:join(["wx_xml",FileName ++ ".xml"]),
    put({loaded, FileName}, true),
    case xmerl_scan:file(File, [{space, normalize}]) of 
	{error, enoent} ->
	    io:format("Skipped File not found ~p ~n", [File]),
	    Defs;
	{Doc, _} ->
	    %% io:format("Scanning ~p ~n", [File]),
	    INCs = xmerl_xpath:string("./compounddef/includes/text()", Doc),
	    [put({include,reverse(tl(tl(reverse(Inc))))},ref) || 
		#xmlText{value=Inc} <- INCs],
	    case Type of
		class ->
		    AM = xmerl_xpath:string("./compounddef/listofallmembers/*", Doc),
		    foldl(fun(X,Y) -> extract_rmembers(X,Y,Opts) end, Tab, AM);
		_ ->
		    ignore
	    end,
	    LMembers0 = xmerl_xpath:string("./compounddef/sectiondef/*", Doc),
	    foldl(fun(E,Acc) -> extract_lmembers(E,Class,Type,Tab,Opts,Acc) end, Defs, LMembers0)
    end.
	    
extract_lmembers(Entry=#xmlElement{name=memberdef,attributes=Attrs,content=C},Class,Type,Tab,Opts,Acc) ->
    case keysearch(kind, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = "function"}} -> 	    
	    case keysearch(prot, #xmlAttribute.name, Attrs) of
		{value, #xmlAttribute{value = "public"}} ->
		    {value, #xmlAttribute{value = Id}} =
			keysearch(id, #xmlAttribute.name, Attrs),
		    case Type of
			static ->
			    Get = fun(#xmlElement{name=name,content=[#xmlText{value=Name}]},NAcc) ->
					  [name(string:strip(Name),Opts)|NAcc];
				     (_D, NAcc) -> 
					  NAcc
				  end,
			    case foldl(Get, [], C) of
				[Name] -> 
				    true = ets:insert(Tab,{Name,Id});
				[] -> 
				    ignore
			    end;
			_ -> ignore		    
		    end,
		    case gb_trees:lookup(Id,Acc) of
			{value, _Entry} -> gb_trees:update(Id,Entry,Acc);
			none -> gb_trees:insert(Id,Entry,Acc)
		    end;
		_ -> 
		    Acc
	    end;
	{value, #xmlAttribute{value = "variable"}} when Type =/= static -> 
%% 	    {value, #xmlAttribute{value = Id}} =
%% 		keysearch(id, #xmlAttribute.name, Attrs),
	    %% Hopefully wxW have some decent order!!
	    Id = next_id(attr_id),
	    gb_trees:insert({attr,Id},Entry,Acc);
	{value, #xmlAttribute{value = "enum"}} when Type =/= static ->
	    extract_enum(Entry,Class, undefined),
	    Acc;
	_ -> Acc

    end.

extract_rmembers(#xmlElement{name=member,attributes=Attrs,content=C},Tab, Opts) ->
    {value,#xmlAttribute{value=Id}} = keysearch(refid, #xmlAttribute.name, Attrs),
    Get = fun(#xmlElement{name=name,content=[#xmlText{value=Name}]},Acc) ->
		  [name(string:strip(Name),Opts)|Acc];
	     (_D, Acc) -> 
		  Acc
	  end,
    case foldl(Get, [], C) of
	[Name] -> 
	    true = ets:insert(Tab,{Name,Id});
	[] -> 
	    ignore
    end,
    Tab.

select_member([{_,ID}], #class{name=Class,file=Orig}, Defs0, Opts) ->
    [FileName, _8H|_] = string:tokens(ID, "_"),
    case get({loaded, FileName}) =:= undefined 
	andalso get({loaded, FileName ++ "_" ++ _8H}) =:= undefined of
	true ->
	    true = FileName =/= Orig, % Assert
	    Defs = load_members(FileName, Class, Defs0, skip, skip, Opts),
	    {[ID],Defs};
	false ->
	    {[ID],Defs0}
    end;
select_member(Several, #class{name=Class,file=Orig}, Defs0, Opts) ->
    MIds = [{string:tokens(MId, "_"),MId} || {_,MId} <- Several],
    [StatFile |_ ] = string:tokens(Orig, "_"),
    Check = 
	fun({[FN,_|_],ID}, {T,D}) when FN =:= Orig -> {[ID|T],D};
	   ({[FN,"8h"|_],ID}, {T,D}) when FN =:= StatFile -> {[ID|T],D};
	   ({[FN,_A|_],ID},{T,D}) -> 
		InBase = "class" ++ Class ++ "Base" =:= FN,
		"wx" ++ ClassName = Class,
		InGeneric = "classwxGeneric" ++ ClassName =:= FN,
		IsHelper = case re:run(FN, "Helper$") of
			       {match,_} -> true;
			       _ -> false
			   end,
		ImplBase = case re:run(FN, "Base$") of
			       {match,_} -> true;
			       _ -> 
				   %% Hack for base-base class
				   FN =:= "classwxItemContainer"
			   end,
		case InBase orelse InGeneric orelse IsHelper orelse ImplBase of 
		    true ->
			Defs = case get({loaded, FN}) of
				   undefined ->
				       true = FN =/= Orig, % Assert
				       load_members(FN,Class,D,skip,skip,Opts);
				   true -> D
			       end,
			{[ID|T], Defs};
		    _C -> 
			%% io:format("DBG ~p ~p ~p ~p ~n",[FN,_A,_C,Class]),
			{T,D}
		end
	end,
    foldl(Check,{[],Defs0},MIds).

parse_member(Data,MType,Virtual,Opts = #hs{fopt=Fopts}) ->
    Parse  = fun(Con,A) -> parse_member2(Con,Opts,A) end,
    Method = #method{name=MName,params=PS0} =
	foldl(Parse, #method{method_type=MType, virtual=Virtual}, Data),
    %% Skip motif name's if it's last and optional
    PS2 = case PS0 of %% Backward order..
	      [#param{name="name",def=Def,type=#type{name="wxString"}}|PS1]
	      when Def =/= none ->
		  PS1;
	      _ ->
		  PS0
	  end,
    Sz = length(PS2),
    PS = map(fun(P=#param{name=PName}) ->
		     patch_param(MName,{Sz,PName},P,Fopts)
	     end, PS2),
    Alias = find_erl_alias_name(MName,PS,Fopts),
    FOpts = case gb_trees:lookup(MName, Fopts) of
		{value, FuncO} when is_list(FuncO) ->
		    case lists:keyfind({func,Sz}, 1, FuncO) of
			false -> FuncO;
			{_, FuncNO} -> FuncNO
		    end;
		_ -> []
	    end,
    Method#method{params=PS, alias=Alias, opts=FOpts}.

find_erl_alias_name(MName,Ps,Fopts) ->
    case gb_trees:lookup(MName, Fopts) of
	{value, FuncO} when is_list(FuncO) ->
	    Aliases = lists:foldl(fun({Var, {erl_func, AliasName}}, Acc) ->
					  [{Var,AliasName}|Acc];
				     ({erl_func, AliasName}, Acc) ->
					  [{all,AliasName}|Acc];
				     ({Var, List}, Acc) when is_list(List) ->
					  case get_value(erl_func,List) of 
					      undefined ->
						  Acc;
					      AliasName ->
						  [{Var,AliasName}|Acc]
					  end;		
				     (_,Acc) -> Acc
				  end, [], FuncO),
	    case Aliases of
		[] -> 
		    undefined;
		_ -> 
		    Find = fun({all,AliasName},Acc) -> [AliasName|Acc];
			      ({Var,AliasName},Acc) -> 
				   case lists:keymember(Var, #param.name, Ps) of
				       true -> [AliasName|Acc];
				       false -> Acc
				   end				   
			   end, 
		    case lists:foldl(Find, [], Aliases) of
			[Alias] -> Alias;
			[] -> undefined
		    end		    
	    end;
	_ ->	    
	    undefined 
    end.

parse_member2(#xmlElement{name=type, content=C},Opts,M0) ->
    Type = parse_type(drop_empty(C), Opts),
    M0#method{type=Type};
parse_member2(#xmlElement{name=name, content=[#xmlText{value=C}]}, Opts, M0) ->
    Func = string:strip(C),
    put(current_func, Func),
    M0#method{name=name(Func,Opts)};
parse_member2(#xmlElement{name=param, content=C},Opts,M0) -> 
    Parse = fun(Con, Ac) -> parse_param(Con, Opts, Ac) end,
    Param0 = foldl(Parse, #param{}, drop_empty(C)),
    add_param(Param0, Opts, M0);
parse_member2(_, _,M0) ->
    M0.

add_param(InParam, Opts, M0) ->
    Param0 = case {InParam#param.name, InParam#param.type} of
                 {undefined, void} -> InParam#param{where=nowhere};
		 {undefined,_} -> InParam#param{name="val"};
		 _ -> InParam
	     end,
    Param = case Param0#param.type of
		#type{base={comp,_,_Comp}} ->   Param0;
		#type{base={class,_Class}} -> Param0;
		#type{base={ref,_}} -> Param0;
		#type{base={term,_}} -> Param0;
		#type{base=List} when is_list(List) -> Param0;
		%% Assume the pointer args to base types are out parameters
		#type{by_val=false,single=true, mod=Mod} -> 
		    case lists:member(const, Mod) of
			true  -> Param0; % But not if they are const
			false -> Param0#param{in=false}
		    end;
		_  -> Param0
	    end,
    add_param2(Param, Opts, M0).

add_param2(P=#param{name=Name},#hs{fopt=FOpt},M0=#method{name=MName,params=Ps}) ->
    case patch_param(MName, Name, P, FOpt) of 
	#param{where=nowhere} ->
	    M0#method{params=Ps};
	Patched ->
	    %%  case MName of  %% DEBUG
	    %% 	    "GetSelections" -> 
	    %% 	    io:format("~p~n",[Patched]);
	    %%   	_ -> ignore
	    %%  end,
	    %%ASSERT 
	    case Patched#param.type of
		#type{base=undefined} -> ?error({unknown_type,Patched});
		_ -> ok
	    end,
	    M0#method{params=[Patched|Ps]}
    end.

patch_param(Method, Name, P, Opt) ->
    case gb_trees:lookup(Method,Opt) of
	none -> P;
	{value,NoArg} when is_integer(NoArg) -> P;
	{value,Opts} when is_list(Opts) ->
	    case get_value(Name, Opts) of
		undefined -> P;
		List when is_list(List) -> 
		    foldl(fun handle_param_opt/2,P,List);
		Val -> 
		    handle_param_opt(Val,P)
	    end
    end.

handle_param_opt(skip, P) -> P#param{where=c};
handle_param_opt(nowhere, P) -> P#param{where=nowhere};
handle_param_opt(skip_member, _P) -> throw(skip_member);
handle_param_opt({skip_member, Type}, P) ->
    case P of
	#param{type=#type{name=Type}} ->
	    throw(skip_member);
	#param{type=Type} ->
	    throw(skip_member);
	_ -> 
	    P
    end;
handle_param_opt({erl_func,_Name}, P) -> P;  %% Handled elsewhere
handle_param_opt(in, P) -> P#param{in=true};
handle_param_opt(out, P) -> P#param{in=false};
handle_param_opt(both, P) -> P#param{in=both};
handle_param_opt({def,Def},P) -> P#param{def=Def};
handle_param_opt({type,Type}, P=#param{type=T})  ->  P#param{type=T#type{name=Type}};
handle_param_opt({single,Opt}, P=#param{type=T}) ->  P#param{type=T#type{single=Opt}};
handle_param_opt({base,Enum={enum,Type}},  P=#param{type=T}) ->   P#param{type=T#type{base=Enum, name=Type}};
handle_param_opt({base,Opt},  P=#param{type=T}) ->   P#param{type=T#type{base=Opt}};
handle_param_opt({c_only,Opt},P) -> P#param{where=c, alt=Opt};
handle_param_opt({ref, pointer}, P=#param{type=T}) ->
    P#param{type=T#type{by_val=false,ref={pointer, 1}}};
handle_param_opt({by_val, true}, P=#param{type=T}) ->
    P#param{type=T#type{by_val=true}};
handle_param_opt({mod,Mods}, P=#param{type=T=#type{mod=Mods0}}) ->
    P#param{type=T#type{mod=Mods++Mods0}}.

get_opt(Opt, Method, Sz, Opts) -> 
    case gb_trees:lookup(Method,Opts) of
	none -> undefined;
	{value, List} when is_list(List) ->
	    case get_value({Sz,Opt}, List, undefined) of
		undefined -> 
		    get_value(Opt, List, undefined);
		Res -> Res
	    end
    end.

parse_param(#xmlElement{name=type,content=C},Opts,T) ->   
    Type = parse_type(drop_empty(C),Opts),
    T#param{type=Type};
parse_param(#xmlElement{name=declname,content=[C]},_Opts,T) -> 
    #xmlText{value=Name} = C,
    T#param{name=Name};
parse_param(#xmlElement{name=defval,content=[#xmlText{value=Def}]},_Opts,T) -> 
    T#param{def=string:strip(Def)};
parse_param(#xmlElement{name=defval,content=Other},_Opts,T) -> 
    %% For defaults = (modifer wxType *) NULL 
    Def0 = foldr(fun(#xmlText{value=V}, Acc) -> V ++ Acc;
		    (#xmlElement{content=[#xmlText{value=V}]},Acc) -> 
			 V ++ Acc
		 end, [], Other),
%%     Def1 = lists:dropwhile(fun($)) -> false;(_) -> true end, Def0), 
%%     Def = string:strip(Def1),  %% Drop type cast !!
%%    io:format("Def ~s => ~s => ~s ~n", [Def0, Def1,string:strip(Def)]),
    T#param{def=string:strip(Def0)};
parse_param(#xmlElement{name=array,content=C},_Opts, T = #param{type=Type0}) -> 
    case Type0 of
	_ when T#param.name=:="WXUNUSED" -> %% doxygen can't handle this macro
	    [#xmlText{value=RealVar}] = C,
	    [Name] = string:tokens(RealVar, "() "),
	    T#param{name=Name};
	_ -> 
	    T#param{type=Type0#type{single=array, by_val=true}}
    end;
parse_param(#xmlElement{name=name,content=[C]}, _, T) ->
    %% Attributes have this
    case C of
	#xmlText{value=Name="ms_classInfo"} ->
	    T#param{name=Name, where=nowhere};
	#xmlText{value=Name} ->
	    T#param{name=Name}
    end;
%% Skipped: Attributes have this
parse_param(#xmlElement{name=definition}, _, T) ->    T;
parse_param(#xmlElement{name=argsstring}, _, T) ->    T;
parse_param(#xmlElement{name=briefdescription}, _, T) ->    T;
parse_param(#xmlElement{name=detaileddescription}, _, T) ->    T;
parse_param(#xmlElement{name=inbodydescription}, _, T) ->    T;
parse_param(#xmlElement{name=location}, _, T) ->    T;
parse_param(#xmlElement{name=referencedby}, _, T) ->    T;
parse_param(#xmlElement{name=reimplements}, _, T) ->    T;
parse_param(Other=#xmlElement{name=Name}, _, T) ->
    io:format("Unhandled Param ~p ~p ~n in ~p~n", [Name,Other,T]),
    ?error(unhandled_param).

parse_type([], _Opts) -> void;
parse_type(TypeInfo, Opts) ->
    {Type,Info} = foldl(fun extract_type_info/2,{[],undefined},TypeInfo),
    case Info of
	{"member", Ref} ->
	    case string:tokens(Ref, "_") of
		[FileName, "8h", _Id] ->
		    put({file_ref, FileName++"_8h"}, ref);
		_ -> 
		    ok
	    end;
	_ -> ok
    end,

    Empty = #type{},
    case parse_type2(reverse(Type),Info,Opts,#type{}) of
	Empty -> ?error({strange_type, Type});
	Assert  -> Assert
    end.

extract_type_info(#xmlText{value=Value}, {Acc, Info}) -> 
    {reverse(foldl(fun extract_type_info2/2, [], string:tokens(Value, " "))) ++ Acc, Info};
extract_type_info(#xmlElement{name=ref,attributes=As,content=[#xmlText{value=V}]},
		  {Acc,undefined}) ->
    {value, #xmlAttribute{value = Refid}} = keysearch(refid,#xmlAttribute.name,As),
    {value, #xmlAttribute{value = Kind}} = keysearch(kindref,#xmlAttribute.name,As),
    {reverse(foldl(fun extract_type_info2/2, [], string:tokens(V, " "))) ++ Acc,
     {Kind,Refid}};
extract_type_info(#xmlElement{name=ref,attributes=As,content=[#xmlText{value=V}]},
		  {Acc,_}) ->
    {value, #xmlAttribute{value = Refid}} = keysearch(refid,#xmlAttribute.name,As),
    {value, #xmlAttribute{value = Kind}} = keysearch(kindref,#xmlAttribute.name,As),
    {reverse(foldl(fun extract_type_info2/2, [], string:tokens(V, " "))) ++ Acc,
     {Kind,Refid}};
extract_type_info(What,Acc) ->
    ?error({parse_error,What,Acc}).

extract_type_info2("const",Acc) -> [const|Acc];
extract_type_info2("*", [{by_ref,{pointer,N}}|Acc]) -> [{by_ref,{pointer,N+1}}|Acc];
extract_type_info2("*",   Acc) -> [{by_ref,{pointer,1}}|Acc];
extract_type_info2("**",  Acc) -> [{by_ref,{pointer,2}}|Acc];
extract_type_info2("&",   Acc) -> [{by_ref,reference}|Acc];
extract_type_info2("WXDLLIMP" ++ _, Acc) ->  Acc;
extract_type_info2(Type,  Acc) -> [Type|Acc].

parse_type2(["void"], _Info,  _Opts, #type{by_val=ByVal}) ->
    case ByVal of
	true ->  void;
	false -> voidp
    end;
parse_type2(["virtual"|R], _Info,  _Opts, _T) ->  
    [] = R,
    %% Bug in old doxygen virtual destructors have type virtual
    void;
parse_type2(["wxe_cb"|R],Info,Opts, T) -> 
    parse_type2(R,Info,Opts,T#type{name=int,base=wxe_cb});
parse_type2([const|R],Info,Opts,T=#type{mod=Mod}) -> 
    parse_type2(R,Info,Opts,T#type{mod=[const|Mod]});
parse_type2(["unsigned"|R],Info,Opts,T=#type{mod=Mod}) ->
    case T#type.base of
        undefined ->
            parse_type2(R,Info,Opts,T#type{name=int, base=int, mod=[unsigned|Mod]});
        _ ->
            parse_type2(R,Info,Opts,T#type{mod=[unsigned|Mod]})
    end;
parse_type2(["int"|R],Info,Opts,  T) -> 
    parse_type2(R,Info,Opts,T#type{name=int,base=int});
parse_type2(["wxByte"|R],Info,Opts,  T) ->
    parse_type2(R,Info,Opts,T#type{name=int,base=int});
parse_type2(["char"|R],Info,Opts,  T) -> 
    parse_type2(R,Info,Opts,T#type{name="char",base=int});
parse_type2([N="size_t"|R], Info, Opts,  T) -> 
    parse_type2(R,Info,Opts,T#type{name=N, base=int});
parse_type2(["long"|R],Info, Opts, T) -> 
    parse_type2(R,Info,Opts,T#type{name=long,base=int});
parse_type2(["float"|R],Info,Opts, T) -> 
    parse_type2(R,Info,Opts,T#type{name=float,base=float});
parse_type2(["double"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=double,base=double});
parse_type2([N="wxDouble"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=double});
parse_type2(["bool"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=bool,base=bool});
parse_type2([N="wxWindowID"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=int});
parse_type2([N="wxTextCoord"|R],Info,Opts,T) ->      %%long 
    parse_type2(R,Info,Opts,T#type{name=N,base=int});
parse_type2([N="wxTextPos"|R],Info,Opts,T) ->        %%long
    parse_type2(R,Info,Opts,T#type{name=N,base=int});
parse_type2([N="wxPrintQuality"|R],Info,Opts,T) ->
    parse_type2(R,Info,Opts,T#type{name=N,base=int});
parse_type2(["wxDataFormat"|_R],_Info,_Opts,T) ->
    %% Hack Hack
    T#type{name="wxDataFormatId",base={enum,"wxDataFormatId"}};
parse_type2([N="wxArrayInt"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=int,single=array});
parse_type2([N="wxArrayDouble"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=double,single=array});
parse_type2([N="wxTreeItemId"|R],Info,Opts,T) -> %% Use Pointer as Ids
    parse_type2(R,Info,Opts,T#type{name=N,base=int64});
parse_type2([N="wxTreeItemIdValue"|R],Info,Opts,T) -> %% Use Pointer as Ids
    parse_type2(R,Info,Opts,T#type{name=N,base=int64});
parse_type2([N="wxArrayTreeItemIds"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=int64,single=array});
parse_type2([N="wxTreeItemData"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name="wxETreeItemData",base={term,N}});
parse_type2([N="wxClientData"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name="wxeErlTerm",base={term,N}});
parse_type2([N="wxChar",{by_ref,_}|R],Info,Opts,T = #type{mod=[const]}) ->
    case get(current_class) of
	"wxLocale" -> %% Special since changed between 2.8 and 3.0
	    parse_type2(R,Info,Opts,T#type{name="wxeLocaleC",base=string});
	_ ->
	    parse_type2(R,Info,Opts,T#type{name=N,base=int,single=false})
    end;
parse_type2([N="wxChar"|R],Info,Opts,T) ->
    parse_type2(R,Info,Opts,T#type{name=N,base=int});
parse_type2(["wxUint32"|R],Info,Opts,T=#type{mod=Mod}) -> 
    parse_type2(R,Info,Opts,T#type{name=int,base=int,mod=[unsigned|Mod]});
parse_type2([N="wxCoord"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=int});
parse_type2([N="wxPoint"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,N,[{int,"X"},{int,"Y"}]}});
parse_type2([N="wxSize"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,N,[{int,"W"},{int,"H"}]}});
parse_type2([N="wxGBPosition"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,N,[{int,"R"},{int,"C"}]}});
parse_type2([N="wxGBSpan"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,N,[{int,"RS"},{int,"CS"}]}});
parse_type2([N="wxGridCellCoords"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,N,[{int,"R"},{int,"C"}]}});
parse_type2([N="wxGridCellCoordsArray"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,"wxGridCellCoords",
						[{int,"R"},{int,"C"}]},
				   single=array});
parse_type2([N="wxAuiPaneInfoArray"|R],Info,Opts,T) ->
    parse_type2(R,Info,Opts,T#type{name=N,base={class,"wxAuiPaneInfo"},
				   single=array});

parse_type2([N="wxRect"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base={comp,N,[{int,"X"},{int,"Y"},
							{int,"W"},{int,"H"}]}});
parse_type2([N="wxColour"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,
				   base={comp,N,[{int,"R"},{int,"G"},{int,"B"},{int,"A"}]}});
parse_type2(["wxColor"|R],Info,Opts,T) ->
    N = "wxColour",
    parse_type2(R,Info,Opts,T#type{name=N,
				   base={comp,N,[{int,"R"},{int,"G"},{int,"B"},{int,"A"}]}});

parse_type2([N="wxPoint2DDouble"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,
				   base={comp,N,[{double,"X"},{double,"Y"}]}});
parse_type2([N="wxRect2DDouble"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,
				   base={comp,N,[{double,"X"},{double,"Y"},
						 {double,"W"},{double,"H"}]}});

parse_type2([N="wxDateTime"|R],Info,Opts,T) ->
    parse_type2(R,Info,Opts,T#type{name=N, 
				   base={comp,N,[{int,"D"},{int,"Mo"},{int,"Y"},
						 {int,"H"},{int,"Mi"},{int,"S"}]}
				  });

parse_type2([N="wxMouseState"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N, base={comp,N,{record, wxMouseState}}});
parse_type2([N="wxHtmlLinkInfo"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N, base={comp,N,{record, wxHtmlLinkInfo}}});
parse_type2([N="wxString"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=string});
parse_type2([N="wxArtClient"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=string});
parse_type2(["wxArtID"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name="wxString",base=string});
parse_type2([N="wxFileName"|R],Info,Opts,T) ->
    parse_type2(R,Info,Opts,T#type{name=N,base=string});
parse_type2([N="wxArrayString"|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=[int],single=array,by_val=true});
parse_type2([{by_ref,Ref}|R],Info,Opts,T) -> 
    parse_type2(R,Info,Opts,T#type{ref=Ref,by_val=false});
parse_type2([],_,_,T) -> T;

parse_type2([N="wxImageList"|R],Info,Opts,T) ->  %% ARRG breaks the following clause
    parse_type2(R,Info,Opts,T#type{name=N,base={class,N}});
parse_type2(L=[Name|R],I,Opts,T) ->
    case reverse(Name) of
	"tsiL" ++ RBase -> 
	    parse_type2(R,I,Opts,
			T#type{name=Name,base={class,reverse(RBase)},single=list});
	_ -> 
	    parse_type3(L,I,Opts,T)
    end.

parse_type3(["wxNotebookPage"|R],I,Opts,T) -> 
    Xml = case I of
	      {_, Ref} -> Ref;
	      undefined -> undefined
	  end,
    parse_type2(R,I,Opts,T#type{name="wxWindow",base={class,"wxWindow"},xml=Xml});
parse_type3([N|R],I={"member",Ref},Opts,T) -> 
    Type = name(N,Opts),
    ErlClass = special_name(Type),
    case string:tokens(Ref, "_") of
	["class" ++ _] -> ignore;
	Other -> 
	    Inc0 = lists:takewhile(fun("8h") -> false;(_) -> true end,Other),
	    Inc = gen_util:args(fun(A) -> A end, "_", Inc0),
%%	    io:format("Inc ~s ~n", [Inc]),	
	    put({include,Inc}, ref)
    end,
    case get_enum(Type) of
	{_, undefined} ->
	    parse_type2(R,I,Opts,T#type{name=Type,base={class,ErlClass},xml=Ref});
	{TypeWOClass,#enum{}} -> 
	    parse_type2(R,I,Opts,T#type{name=Type,base={enum,TypeWOClass},xml=Ref})
    end;
parse_type3([N = "wx"++_|R],I,Opts,T) -> 
    Xml = case I of
	      {_, Ref} -> Ref;
	      undefined -> undefined
	  end,
    Class = name(N,Opts),
    ErlClass = special_name(Class),
    parse_type2(R,I,Opts,T#type{name=Class,base={class,ErlClass},xml=Xml});
parse_type3([N="WXWidget"|R], Info,Opts, T) -> 
    parse_type2(R,Info,Opts,T#type{name=N,base=long});
%% Let type errors be seen later because we don't know if these unhandled types
%% will be used.
parse_type3([Name|R],Info,Opts, T) ->
    New = T#type{name={unhandled,Name,Info,get(current_class),get(current_func)}},
    parse_type2(R,Info,Opts, New).

%%skipped(#method{method_type=constructor, type=void}, _Opts) -> true;
skipped(#method{}, #hs{skip=[]}) ->   false;
skipped(#method{name=Name,params=P}, #hs{skip=Skip}) ->    
    AtomName = list_to_atom(Name),
    Skipped = lists:member(AtomName, Skip) orelse 
	lists:member({AtomName,length(P)}, Skip),
    %% io:format("~p ~p skipped ~p ~n", [AtomName, length(P),Skipped]),
    Skipped.
    
add_method(Ms0, NoArgs, Class, Opts) ->
    Add = fun(M=#method{params=Ps0}, Acc) -> 
		  case length(Ps0) of
		      NoArgs -> 
			  [add_method2(M,Class,Opts)|Acc];
		      _ when NoArgs =:= all ->
			  [add_method2(M,Class,Opts)|Acc];
		      _ -> 
			  Acc
		  end
	  end,
    NewMs  = lists:foldl(Add,[],Ms0),
    Unique = filter_functions(reverse(NewMs), Opts),
    erase(current_func),
    foldl(fun(M,C=#class{methods=Ms}) when is_list(M) -> C#class{methods=[M|Ms]} end,
	  Class,reverse(Unique)).

add_method2(M0=#method{name=Name,params=Ps0,type=T0},#class{name=CName,parent=Par},#hs{fopt=Opts}) ->
    Type = case patch_param(Name, return, #param{type=T0}, Opts) of
	       #param{type = T0} -> 
		   case patch_param(Name, {length(Ps0),return}, #param{type=T0}, Opts) of
		       #param{where=nowhere} -> void;
		       #param{type = Type0} -> Type0
		   end;
	       #param{where=nowhere} -> void;
	       #param{type = Type0} -> Type0
	   end,
    
    {Req,Opt} = lists:partition(fun(#param{def=Def}) -> Def == none end, 
				M0#method.params),
    Ps = reverse(Ps0),
    
    IsStatic = case Par of 
		   "static" -> static;
		   _ -> M0#method.method_type
	       end,
    Where = case get_opt(where, Name, length(Ps), Opts) of
		undefined -> both;
		Other -> 
		    Other
	    end,
    M1 = M0#method{defined_in=CName,
		   min_arity = length(Req),
		   max_arity = length(Req) + case Opt of
						 [_ | _] -> 1;
						 _ -> 0
					     end,
		   type = Type,
		   method_type = IsStatic,
		   where = Where,
		   id = next_id(func_id),
		   pre_hook  = get_opt(pre_hook, Name, length(Ps), Opts),
		   post_hook = get_opt(post_hook, Name, length(Ps), Opts),
		   doc = get_opt(doc, Name, length(Ps), Opts)
		  },
    M = case Name of
	    CName ->
		M1#method{method_type=constructor,name=CName, 
			  type=constructor(CName), params=Ps};
	    [$~|CName] ->
		M1#method{method_type=destructor,name=Name,
			  params=[this(CName)|Ps]};
	    _ ->
		case M1#method.method_type of
		    static -> M1#method{params=Ps}; 
		    member -> M1#method{params=[this(CName)|Ps]}
		end
	end,
    M.

this(Class) ->
    #param{name="This",where=this,
	   type=#type{name=Class,base={class,Class},by_val=false,ref={pointer,1}}}.

constructor(Class) ->
    #type{name=Class,base={class,Class},by_val=false,ref=reference}.

filter_functions(Parsed, Opts) ->
    Left = foldl(fun(M0,Acc) -> 
			 case skipped(M0, Opts) of
			     true ->  Acc;
			     false -> 
				 TF = extract_type_footprint(M0),
				 [TF|Acc]
			 end
		 end,[],Parsed),
    Clean = remove_or_merge(lists:sort(Left),[],[]),
    erl_skip_opt(reverse(Clean),[],[]).

remove_or_merge([{A,{L,In,O1},M1}|Rest=[{A,{L,In,O2},M2}|_]],Acc1,Acc2) 
  when M1#method.method_type =:= M2#method.method_type ->
    %% These are the same remove one of them.
    case O1 =:= O2 of
	true ->  ok;
	false -> 
	    ?warning("Multiple out arguments of ~s:~s: ~p or ~p~n", 
		     [get(current_class),M1#method.name, O1,O2])
    end,
    remove_or_merge(Rest,Acc1,Acc2);
remove_or_merge([F={A,{Len,_,_},M1}|Rest],[{A,{Len,_,_},M2}|_]=Acc1,Acc2)
  when M1#method.method_type =:= M2#method.method_type ->
    NewAcc1 = maybe_merge(F,Acc1,[]),
    remove_or_merge(Rest,NewAcc1,Acc2);
remove_or_merge([F|Rest],[],Acc2) ->
    remove_or_merge(Rest,[F],Acc2);
remove_or_merge([F|Rest],Acc1,Acc2) ->
    remove_or_merge(Rest,[F], [reverse(Acc1)|Acc2]);
remove_or_merge([],[], Acc2) -> Acc2;
remove_or_merge([],Acc1,Acc2) -> [reverse(Acc1)|Acc2].

erl_skip_opt([Ms|R],[],Acc2) ->
    {Orig, Skipped} = erl_skip_opt2(Ms,[],[],[]),
    erl_skip_opt(R,Orig,[Skipped|Acc2]);
erl_skip_opt(All=[Ms=[{_,{Len,_,_},_}|_]|R],Acc1=[{_,{N,_,_},_}|_], Acc2) ->
    case Len =:= N+1 of
	true  ->
	    {Orig, Skipped} = erl_skip_opt2(Ms,[],[],Acc1),
	    erl_skip_opt(R,Orig,[Skipped++strip_ti(Acc1)|Acc2]);
	false ->
	    erl_skip_opt(All, [], [strip_ti(Acc1)|Acc2])
    end;
erl_skip_opt([],Acc1,Acc2) -> [strip_ti(Acc1)|Acc2].

erl_skip_opt2([F={_,{N,In,_},M=#method{where=Where}}|Ms],Acc1,Acc2,Check) ->
    case N > 0 andalso lists:last(In) =:= opt_list of
	true when Where =/= merged_c, Where =/= taylormade ->
	    case Check of
		[] ->
		    erl_skip_opt2(Ms,[F|Acc1],[M#method{where=erl_no_opt}|Acc2],[]);
		_  ->
		    Skipped = droplast(In),
		    T = fun({_,{_,Args,_},_}) -> true =:= types_differ(Skipped,Args) end,
		    case lists:all(T, Check) of
			true ->
			    erl_skip_opt2(Ms,[F|Acc1],
					  [M#method{where=erl_no_opt}|Acc2],
					  Check);
			false ->
			    erl_skip_opt2(Ms,[F|Acc1],Acc2,Check)
		    end
	    end;
 	_ ->
	    erl_skip_opt2(Ms,[F|Acc1],Acc2,Check)
    end;
erl_skip_opt2([],Acc1,Acc2,_) -> {Acc1,Acc2}.

strip_ti(Ms) ->
    [M || {_,{_,_,_},M} <- Ms].

maybe_merge(T1,[],Acc) -> reverse([T1|Acc]);
maybe_merge(F={A1,T1={Len,In1,O1},M1},[C={A2,T2={Len,In2,O2},M2}|Rest],Acc) ->
    case types_differ(In1,In2) of
	true -> maybe_merge(F,Rest,[C|Acc]);
	{class,C1,C2} when O1 =:= O2 ->
	    {Merged,M2Mod} = merge_class_params(M1,M2,C1,C2),
	    reverse([{A1,T1,Merged},{A2,T2,M2Mod}|Acc]) ++ Rest;
	false ->
	    ?warning("Argument clash in ~s:~s:~n   ~p~nor ~p~n", 
		     [get(current_class),M1#method.name,{In1,O1},{In2,O2}]),
	    [F|Rest++Acc]
    end.

merge_class_params(M1=#method{params=P1,id=Mi1},M2=#method{params=P2,id=Mi2},C1,C2) ->
    Merged = merge_class_params2({class,C1},P1,Mi1,{class,C2},P2,Mi2),
    {M1#method{params=Merged}, M2#method{where=merged_c}}.

merge_class_params2(B1,[P1|R1],M1,B2,[P1|R2],M2) ->
    [P1|merge_class_params2(B1,R1,M1,B2,R2,M2)];
merge_class_params2(B1,[P1=#param{type=T1=#type{base=B1}}|R1],M1,
		    B2,[#param{type=T2=#type{base=B2}}|R2],M2) ->
    [P1#param{type={merged,M1,T1,R1,M2,T2,R2}}|merge_class_params2(B1,R1,M1,B2,R2,M2)];
merge_class_params2(B1,[P1|R1],_M1,B2,[P2|R2],_M2) ->
    io:format("Merged Failed ~p ~p~n", [B1,B2]),
    io:format("  ~p ~p~n  ~p~p~n", [P1,R1,P2,R2]),
    ?error(merged_failed);
merge_class_params2(_,[],_,_,[],_) ->
    [].

types_differ([C1|R1], [C2|R2]) when is_list(C1), is_list(C2) ->
    types_differ(R1,R2); %% Different Classes
types_differ([C|R1], [C|R2]) ->
    types_differ(R1,R2);
types_differ([{term,_}|R1], [_|R2]) ->
    types_differ(R1,R2);
types_differ([_|R1], [{term,_}|R2]) ->
    types_differ(R1,R2);
types_differ([{class,C1}|R1], [{class,C2}|R2]) ->
    case types_differ(R1,R2) of
	true -> 
	    true;
	false -> 
	    {class,C1,C2};
	{class,C1,C2} -> 
	    {class,C1,C2};
	{class, _,_} -> 
	    false
    end;
types_differ([int|_], _) -> true;
types_differ(_, [int|_]) -> true;
types_differ([{class,_}|_], _) -> true;
types_differ(_, [{class,_}|_]) -> true;
types_differ([binary|_], _) -> true;
types_differ(_, [binary|_]) -> true;

types_differ([list|R1], [opt_list|R2]) ->
    types_differ(R1,R2);
types_differ([opt_list|R1], [list|R2]) ->
    types_differ(R1,R2);
types_differ([C1|R1], [C2|R2]) when is_tuple(C1), is_tuple(C2) ->
    (tuple_size(C1) =/= tuple_size(C2)) orelse types_differ(R1,R2);
types_differ([C1|_R1], [_C2|_R2]) when is_tuple(C1) ->
    true;
types_differ([_C1|_R1], [C2|_R2]) when is_tuple(C2) ->
    true;
types_differ([_C1|R1], [_C2|R2]) -> %% More cases?
    types_differ(R1,R2);
types_differ([], []) ->
    false.

extract_type_footprint(M=#method{type=void,alias=A,params=Ps}) ->
    {A,extract_type_footprint2(Ps, [], [], false), M};
extract_type_footprint(M=#method{type=Type,alias=A,params=Ps}) ->
    {A,extract_type_footprint2(Ps, [type_foot_print(Type)], [], false), M}.

extract_type_footprint2([_P=#param{where=c, in=InArg}|R], Out, In, Opt) 
  when InArg =/= false ->
    extract_type_footprint2(R, Out, In, Opt);
extract_type_footprint2([_P=#param{def=Def, in=InArg}|R], Out, In, _Opt) when Def =/= none, InArg =/= false ->
    extract_type_footprint2(R, Out, In, true);
extract_type_footprint2([#param{in=false, type=Type}|Ps], Out, In, Opt) ->
    extract_type_footprint2(Ps, [type_foot_print(Type)|Out], In, Opt);
extract_type_footprint2([#param{in=true, type=Type}|Ps], Out, In, Opt) ->
    extract_type_footprint2(Ps, Out, [type_foot_print(Type)|In], Opt);
extract_type_footprint2([#param{in=both, type=Type}|Ps], Out, In, Opt) ->
    TFP = type_foot_print(Type),
    extract_type_footprint2(Ps, [TFP|Out], [TFP|In], Opt);

extract_type_footprint2([], Out0, In, Opt) ->
    Out = case Out0 of
	      [] -> void;
	      [One] -> One;
	      _ -> list_to_tuple(reverse(Out0))
	  end,
    if Opt -> 
	    {length(In)+1,reverse([opt_list|In]),Out};
       true ->
	    {length(In), reverse(In),Out}
    end.

type_foot_print(#type{single=Single}) when Single =/= true -> list;
type_foot_print(#type{base=string})  -> list;
type_foot_print(#type{base=Base}) when is_list(Base) -> list;
type_foot_print(#type{base=long}) ->      int;
type_foot_print(#type{base=binary}) ->    binary;
type_foot_print(#type{base={binary,_}}) ->    binary;
type_foot_print(#type{base=int}) ->       int;
type_foot_print(#type{base=int64}) ->       int;
type_foot_print(#type{base=bool}) ->      bool;
%%type_foot_print(#type{base=datetime}) ->  datetime;
type_foot_print(#type{base=float}) ->     float;
type_foot_print(#type{base=double}) ->    float;
type_foot_print(#type{base=C={class,_}}) -> C;
type_foot_print(#type{base={enum,_}}) ->  int;
type_foot_print(#type{base={ref,_}}) ->   ref;
type_foot_print(#type{base={term,_}}) ->  term;
type_foot_print(#type{base=eventType}) -> atom;
type_foot_print(voidp) -> int;
%% type_foot_print({Type,Str}) when is_list(Str) ->
%%     type_foot_print(Type);
type_foot_print(#type{base={comp,_,R={record,_}}}) ->
    R;
type_foot_print(#type{base={comp,_,Types}}) ->
    TFL = map(fun({T,N}) when is_list(N) -> 
		      case T of
			  double -> float;
			  _ -> T
		      end
	      end, Types),
    list_to_tuple(TFL).
%type_foot_print(What) -> What.


translate_enums(Defs) ->
    Res = [translate_enums1(Def) || Def <- Defs],
    Consts = [Enum || Enum = {{enum,_},_} <- get()],
    translate_constants(Consts, get(not_const), get(const_skip)),
    put(gvars, [{Gvar,Class,next_id(const)} || {Gvar,Class} <- lists:sort(get(gvars))]),
    Res.

translate_enums1(C=#class{name=Name, methods=Ms0, attributes=As0}) ->
    Ms = [translate_enums2(M, Name) || M <- Ms0],
    As = [translate_enums3(A, Name) || A <- As0],
    C#class{methods=Ms, attributes=As}.

translate_enums2(M=#method{params=Ps0, type=T0},Class) ->
    Ps = [translate_enums3(P, Class) || P <- Ps0],
    T = translate_enums_type(T0,Class),
    M#method{params=Ps,type=T};
translate_enums2(Ms,Class) when is_list(Ms) ->
    [translate_enums2(M,Class) || M <- Ms].

translate_enums3(P=#param{type=Type0},InClass) ->
    Type = translate_enums_type(Type0,InClass),
    P#param{type=Type};
translate_enums3(InHer = {inherited, _},_InClass) ->
    InHer.

translate_enums_type(T=#type{base={class,C}},Class) ->
    case get_enum(C,Class) of
	{_, undefined} -> T;
	{Enum, #enum{}} ->
	    %% io:format("ENUM Change class ~p to enum ~p~n", [C,Enum]),
	    T#type{base={enum, Enum}}
    end;
translate_enums_type(T,_Class) ->   T.

translate_constants(Enums, NotConsts0, Skip0) ->
    NotConsts = gb_sets:from_list(NotConsts0),
    Skip = gb_sets:from_list(Skip0),
    Consts0 = create_consts(lists:sort(Enums), Skip, NotConsts, []),
    put(consts, gb_trees:from_orddict(lists:ukeysort(1,[{N,C}|| C = #const{name=N} <- Consts0]))).

create_consts([{{enum, Name},Enum = #enum{vals=Vals}}|R], Skip, NotConsts, Acc0) ->
    CC = fun(What, Acc) ->
		 create_const(What, Name, Skip, NotConsts, Acc)
	 end,
    Acc = case Vals of
	      undefined -> 
		  ?warning("Missing Enum ~p ~p ~n",[Name, Enum]), 
		  Acc0;
	      [] -> %% ?warning("Ignored Empty Enum list ~p ~n",[_Name]), 
		  Acc0;
	      _ ->  
		  foldl(CC, Acc0, lists:sort(Vals))
	  end,
    create_consts(R, Skip, NotConsts, Acc);
create_consts([],_,_,Acc) -> Acc.

create_const({Name, Val}, EnumName, Skip, NotConsts, Acc) ->
    case gb_sets:is_member(Name, Skip) of
	true -> Acc;
	false ->
	    case gb_sets:is_member(Name, NotConsts) orelse
		gb_sets:is_member(EnumName, NotConsts)
	    of
		true ->
		    [#const{name=Name,val=next_id(const),is_const=false}|Acc];
		false ->
		    [#const{name=Name,val=Val,is_const=true}|Acc]
	    end
    end.

%%%%%%%%%%%%%	   
next_id(What) ->
    Next = case get(What) of
	       undefined -> 100;
	       N -> N+1
	   end,
    put(What, Next),
    Next.

name([$~|Name], Opts) ->
    [$~|name(Name,Opts)];
name(Name0, #hs{alias=Alias}) ->
    Name = case reverse(Name0) of
	       "esaBlooT" ++ _ ->  %% Arrg uses base
		   Name0;
	       "esaBelbaTdirG" ++ _ ->  %% Arrg uses base
		   Name0;
	       "esaBrekciP"  ++ _ ->  %% Arrg uses base
		   Name0;
	       "esaB" ++ Rest when hd(Name0) =:= $w ->
		   %% Arrg Some decl uses base class directly
 		   reverse(Rest);  
	       _F -> 
		   Name0
	   end,
    get_value(Name,Alias,Name).

special_name("wxIconLocation") -> "wx";
special_name("wxToolBarToolBase") -> "wx";
special_name("wxObject") -> "wx";
special_name("wxValidator") -> "wx";     % XXXXX
%% special_name("wxTreeItemData") -> "wx";  % XXXXX
%% special_name("wxTreeItemId") -> "wx";
%% special_name("wxDataObject") -> "wx";
special_name(Other) -> Other.

drop_empty(List) ->
    filter(fun(#xmlText { value = Text}) ->		   
		   string:strip(Text) =/= "";
	      (_)->
		   true
	   end, List).

%%% Enums
parse_enums(Files) ->
    DontSearch = ["wxchar","filefn", "platform", "strconv", "filename", 
		  "buffer", "string", "debug", "platinfo"],
    %% Arg need to patch some specials, atleast for wx-2.6
    ExtraSearch = ["gtk_2glcanvas", "generic_2splash", "added__func"],
    parse_enums(Files ++ ExtraSearch,gb_sets:from_list(DontSearch)).

parse_enums([File|Files], Parsed) ->
    case gb_sets:is_member(File,Parsed) of
	false ->
	    FileName = filename:join(["wx_xml",File ++ "_8h.xml"]),
	    %%io:format("Parse Enums in ~s ~n", [FileName]),
	    case xmerl_scan:file(FileName, [{space, normalize}]) of 
		{error, enoent} ->
		    %% io:format("Ignore ~p~n", [FileName]),
		    parse_enums(Files, gb_sets:add(File,Parsed));
		{Doc, _} ->		    
		    ES = "./compounddef/sectiondef/memberdef[@kind=\"enum\"]",
		    AM = xmerl_xpath:string(ES, Doc),
		    lists:foreach(fun(Def) -> extract_enum(Def, undefined, File) end, AM),

		    DS = "./compounddef/sectiondef/memberdef[@kind=\"define\"]",
		    Defs = xmerl_xpath:string(DS, Doc),
		    extract_defs(Defs,File),

		    INCs = xmerl_xpath:string("./compounddef/includes/text()", Doc),
		    New = [reverse(tl(tl(reverse(Inc)))) || 
			      #xmlText{value="wx/"++Inc} <- INCs],
		    %% io:format("Scan enums from ~p ~n", [File]),
		    parse_enums(New ++ Files, gb_sets:add(File,Parsed))
	    end;
	true ->
	    parse_enums(Files,Parsed)
    end;
parse_enums([],_) -> ok. 
    
extract_enum(#xmlElement{name=memberdef,content=C}, Class, File) ->
    {Name0,Vals0} = extract_enum2(C,undefined,0,[]),
    {Vals,Name} = 
	if 
	    hd(Name0) =:= $@, Class =:= undefined ->
		{Vals0, Name0 ++ "_" ++ File};
	    Class =:= undefined-> 
		{Vals0, Name0};
	    true -> 
		{[{Class++"::"++N,V} || {N,V} <- Vals0], {Class,Name0}}
	end,
    case get({enum, Name}) of
	undefined -> 
%% 	    io:format("1Enum name ~p~n", [Name]),
%% 	    [io:format("  ~s ~p~n", [D,V]) || {D,V} <- Vals],
	    put({enum, Name}, #enum{vals=Vals, from={File,Class,Name0}});
	E = #enum{vals=undefined} -> 
%%  	    io:format("2Enum name ~p~n", [Name]),
%%  	    [io:format("  ~s ~p~n", [D,V]) || {D,V} <- Vals],
	    put({enum, Name}, E#enum{vals=Vals, from={File,Class,Name0}});
	#enum{vals=Vals} -> ok;
%%	    io:format("Same? ~p ~n", [PVals == Vals])
	#enum{vals=OldVals} ->	    
	    io:format("Enum ~p in ~p ~p ~p~n", [Name,Class,get(current_class),get(current_func)]),
	    io:format("New ~p~n", [Vals]),
	    io:format("Old ~p~n", [OldVals]),
	    erlang:error({enum_mismatch,Name,Vals,OldVals})
    end,
    ok.

extract_enum2([#xmlElement{name=name,content=[#xmlText{value=Name}]}|R],_,Id,Acc0) ->
    extract_enum2(R,Name,Id,Acc0);

extract_enum2([#xmlElement{name=enumvalue,content=C}|R], N,Id,Acc0) ->
    {Acc,NewId} = extract_enum3(C,Id,Acc0),
    extract_enum2(R, N, NewId, Acc);
extract_enum2([_|R], N, Id, Acc) ->
    extract_enum2(R, N, Id, Acc);
extract_enum2([], N, _Id, Acc) ->
    {N, reverse(Acc)}.

extract_enum3([#xmlElement{name=name,content=[#xmlText{value=Name}]}|R], Id, Acc) ->
    case lists:keymember(Name, 1, Acc) of
	true ->  %% Doxygen double includes some defs.
	    {Acc,Id};
	false ->
	    case Id of
		This = {Str,Num} ->
		    extract_enum3(R, {Str, Num+1}, [{Name,This}|Acc]);
		Val ->
		    extract_enum3(R, Val+1, [{Name,Val}|Acc])
	    end
    end;

extract_enum3([#xmlElement{name=initializer,content=Cs}|_],_Id,[{Name,_}|Acc]) ->
    String = case extract_def2(Cs) of
		 "= " ++ Str0 -> Str0;  %% Doxygen 1.8.3.1 keeps the '=' sign
		 "=" ++ Str0 -> Str0;  %% Doxygen 1.8.3.1 keeps the '=' sign
		 Str0 -> Str0
	     end,
    Val0 = gen_util:tokens(String,"<& "),
    try
	case Val0 of
	    ["0x" ++ Val1] ->
		Val = list_to_integer(Val1, 16),
		{[{Name, Val}|Acc], Val+1};
	    ["1", "<<", Shift] ->
		Val = 1 bsl list_to_integer(Shift),
		{[{Name, Val}|Acc], Val+1};
	    [Str, "+", What] ->
		Val = list_to_integer(What),
		{[{Name, {Str, Val}}|Acc], {Str,Val+1}};
	    [Single] ->
		Val = list_to_integer(Single),
		{[{Name, Val}|Acc], Val+1};
	    _ ->
		%% io:format("~p Name ~p ~p ~p~n",[?LINE, Name, Val0, String]),
		throw(below)
	end
    catch _:_ ->
	    {[{Name,{String,0}}|Acc], {String,1}}
    end;
extract_enum3([_|R], Id, Acc) ->
    extract_enum3(R, Id, Acc);
extract_enum3([], Id, Acc) ->
    {Acc, Id}.

extract_defs(Defs, File) ->
    case foldl(fun extract_defs2/2, {[], gb_sets:empty()}, Defs) of
	[] -> ok;
	{Vals,_Skip} ->
%% 	    io:format("Defs file ~p~n", [File]),
%% 	    [io:format("  ~s ~p~n", [D,V]) || {D,V} <- Vals, not is_integer(V)]
	    put({enum, {define,"From " ++ File ++ ".h"}}, #enum{vals=Vals, from={File, undefined, "@define"}})
    end.

extract_defs2(#xmlElement{name=memberdef,content=C},{Acc,Skip}) ->
    try
	Res = {Name,_} = extract_def(C,undefined,Skip),
	case gb_sets:is_member(Name,Skip) orelse lists:keymember(Name, 1, Acc) of
	    true -> {Acc,Skip};
	    false -> {[Res | Acc], Skip}
	end
    catch throw:SkipName -> {Acc, gb_sets:add(SkipName,Skip)}
    end.

extract_def([#xmlElement{name=name,content=[#xmlText{value=Name}]}|R], _N, Skip) ->
    case Name of
	"wxUSE" ++ _ ->
	    throw(Name);
	"wx" ++ _ ->
	    extract_def(R, Name, Skip);
	_ ->
	    throw(Name)
    end;
extract_def([#xmlElement{name=param}|_],Name,_) ->
    throw(Name);
extract_def([#xmlElement{name=initializer,content=Cs}|_R],N,Skip) ->
    Val0 = extract_def2(Cs),
    case Val0 of
	"0x" ++ Val1 -> {N, list_to_integer(Val1, 16)};
	_ ->
	    try
		Val = list_to_integer(Val0),
		{N, Val}
	    catch _:_ ->
		    case def_is_ok(Val0, Skip) of
			false ->
			    throw(N);
			NVal when is_integer(NVal) ->
			    {N, NVal};
			NVal ->
			    {N, {NVal,0}}
		    end
	    end
    end;
extract_def([_|R],N,Skip) ->
    extract_def(R,N,Skip);
extract_def(_,N,_) ->
    throw(N).

extract_def2([#xmlText{value=Val}|R]) ->
    string:strip(strip_comment(Val)) ++ extract_def2(R);
extract_def2([#xmlElement{content=Cs}|R]) ->
    extract_def2(Cs) ++ extract_def2(R);
extract_def2([]) -> [].

strip_comment("/*" ++ Rest) ->
    strip_comment_until_end(Rest);
strip_comment("//" ++ _) -> [];
strip_comment([H|R]) -> [H | strip_comment(R)];
strip_comment([]) -> [].

strip_comment_until_end("*/" ++ Rest) ->
    strip_comment(Rest);
strip_comment_until_end([_|R]) ->
    strip_comment_until_end(R).

def_is_ok(Name, Skip) ->
    Toks = gen_util:tokens(Name,"()| \\:"),
    R = def_is_ok(Toks, Skip, []),
%    io:format("~s -> ~p~n", [Name,R]),
    R.

def_is_ok([], _Skip, [")",Int, "("]) -> Int;
def_is_ok([], _Skip, Acc) -> lists:append(reverse(Acc));
def_is_ok([N="wx"++_|R],Skip,Acc) ->
    case gb_sets:is_member(N,Skip) of
	true -> false;
	false -> def_is_ok(R,Skip,[N|Acc])
    end;
def_is_ok(["0x"++Val|R],Skip,Acc) ->
    def_is_ok(R,Skip,["16#" ++ Val|Acc]);
def_is_ok([N="|"|R], Skip, Acc) ->
    def_is_ok(R,Skip,[N|Acc]);
def_is_ok([N="("|R], Skip, Acc) ->
    def_is_ok(R,Skip,[N|Acc]);
def_is_ok([N=")"|R], Skip, Acc) ->
    def_is_ok(R,Skip,[N|Acc]);
def_is_ok([":"|_], _Skip, _Acc) ->
    false;
def_is_ok([N|R],Skip,Acc) ->
    case catch list_to_integer(N) of
	{'EXIT', _} -> false;
        Int -> def_is_ok(R,Skip,[Int|Acc])
    end.

get_enum(Type0) when is_list(Type0) ->
    case string:tokens(Type0,":") of
	[Type] -> 
	    {Type, get({enum,Type})};
	[Class,Type] -> 
	    get_enum(Type,Class)
    end;
get_enum({Class,Type}) ->
    get_enum(Type,Class).

get_enum(Type,Class) ->
    case get({enum,Type}) of
	undefined -> 
	    {{Class,Type},get({enum, {Class,Type}})};
	Res = #enum{} ->
	    {Type,Res}
    end.
