%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
%%
-module(asn1ct_constructed_per).

-export([gen_encode_sequence/3]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/3]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/4]).
-export([gen_decode_sof/4]).
-export([gen_encode_choice/3]).
-export([gen_decode_choice/3]).

-include("asn1_records.hrl").
%-compile(export_all).

-import(asn1ct_gen, [emit/1,demit/1,get_record_name_prefix/0]).

%% ENCODE GENERATOR FOR SEQUENCE TYPE  ** **********


gen_encode_set(Erules,TypeName,D) ->
    gen_encode_constructed(Erules,TypeName,D).

gen_encode_sequence(Erules,TypeName,D) ->
    gen_encode_constructed(Erules,TypeName,D).

gen_encode_constructed(Erule,Typename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),
    {ExtAddGroup,TmpCompList,TableConsInfo} =
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL,extaddgroup=ExtAddGroup0} ->
		{ExtAddGroup0,CL,TCI};
	    #'SET'{tablecinf=TCI,components=CL} ->
		{undefined,CL,TCI}
	end,

    CompList = case ExtAddGroup of
		   undefined ->
		       TmpCompList;
		   _ when is_integer(ExtAddGroup) ->
		       %% This is a fake SEQUENCE representing an ExtensionAdditionGroup
		       %% Reset the textual order so we get the right
		       %% index of the components
		       [Comp#'ComponentType'{textual_order=undefined}||
			   Comp<-TmpCompList]
	       end,
    case Typename of
	['EXTERNAL'] ->
	    emit({{next,val},
		  " = asn1rt_check:transform_to_EXTERNAL1990(",
		  {curr,val},"),",nl}),
	    asn1ct_name:new(val);
	_ ->
	    ok
    end,
    case {Optionals = optionals(to_textual_order(CompList)),CompList} of
	{[],EmptyCL} when EmptyCL == {[],[],[]};EmptyCL == {[],[]};EmptyCL == [] -> 
	    emit(["%%Variable setting just to eliminate ",
		  "compiler warning for unused vars!",nl,
		  "_Val = ",{curr,val},",",nl]);
	{[],_} ->
	    emit([{next,val}," = ?RT_PER:list_to_record("]),
	    emit(["'",asn1ct_gen:list2rname(Typename),"'"]),
	    emit([", ",{curr,val},"),",nl]);
	_ ->
	    Fixoptcall = ",Opt} = ?RT_PER:fixoptionals(",
	    emit({"{",{next,val},Fixoptcall,
		  {asis,Optionals},",",length(Optionals),
		  ",",{curr,val},"),",nl})
    end,
    asn1ct_name:new(val),
    Ext = extensible_enc(CompList),
    case Ext of
	{ext,_,NumExt} when NumExt > 0 ->
	    case extgroup_pos_and_length(CompList) of
		{extgrouppos,ExtGroupPos,ExtGroupLen} ->
		    Elements = make_elements(ExtGroupPos+1,
					     "Val1",lists:seq(1,ExtGroupLen)),
		    emit([
			  {next,val}," = case [X || X <- [",Elements,
			  "],X =/= asn1_NOVALUE] of",nl,
			  "[] -> ",{curr,val},";",nl,
			  "_ -> setelement(",{asis,ExtGroupPos+1},",",
			  {curr,val},",",
			  "{extaddgroup,", Elements,"})",nl,
			  "end,",nl]),
		    asn1ct_name:new(val);
		_ -> % no extensionAdditionGroup
		    ok
	    end,
	    asn1ct_name:new(tmpval),
	    emit(["Extensions = ?RT_PER:fixextensions(",{asis,Ext},",",
		  {curr,val},"),",nl]);
	_ -> true
    end,
    EncObj =
	case TableConsInfo of
	    #simpletableattributes{usedclassfield=Used,
				   uniqueclassfield=Unique} when Used /= Unique ->
		false;
	    %% ObjectSet, name of the object set in constraints
	    %% 
	    %%{ObjectSet,AttrN,N,UniqueFieldName} -> %% N is index of attribute that determines constraint
	    #simpletableattributes{objectsetname=ObjectSet,
				   c_name=AttrN,
				   c_index=N,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValueIndex
				  } -> %% N is index of attribute that determines constraint
		{{ObjSetMod,ObjSetName},OSDef} =
		    case ObjectSet of
			{Module,OSName} ->
			    {{{asis,Module},OSName},asn1_db:dbget(Module,OSName)};
			OSName ->
			    {{"?MODULE",OSName},asn1_db:dbget(get(currmod),OSName)}
		    end,
		case (OSDef#typedef.typespec)#'ObjectSet'.gen of
		    true ->
			ObjectEncode = 
			    asn1ct_gen:un_hyphen_var(lists:concat(['Obj',AttrN])),
			emit([ObjectEncode," = ",nl]),
			emit(["  ",ObjSetMod,":'getenc_",ObjSetName,"'(",
			      {asis,UniqueFieldName},", ",nl]),
			El = make_element(N+1,asn1ct_gen:mk_var(asn1ct_name:curr(val)),AttrN),

			Length = fun(X,_LFun) when is_atom(X) -> 
					 length(atom_to_list(X));
				    (X,_LFun) when is_list(X) ->
					 length(X);
				    ({X1,X2},LFun) ->
					 LFun(X1,LFun) + LFun(X2,LFun)
				 end,
			Indent = 12 + Length(ObjectSet,Length),
			case ValueIndex of
			    [] ->
				emit([indent(Indent),El,"),",nl]);
			    _ ->
				emit([indent(Indent),"value_match(",
				      {asis,ValueIndex},",",El,")),",nl]),
				notice_value_match()
			end,
			{AttrN,ObjectEncode};
		    _ ->
			false
		end;
	    _  ->
		case D#type.tablecinf of
		    [{objfun,_}|_] ->
			%% when the simpletableattributes was at an outer
			%% level and the objfun has been passed through the
			%% function call
			{"got objfun through args","ObjFun"};
		    _ ->
			false
		end
	end,
    emit({"[",nl}),
    MaybeComma1 = 
	case Ext of
	    {ext,_Pos,NumExt2} when NumExt2 > 0 -> 
		emit({"?RT_PER:setext(Extensions =/= [])"}),
		", ";
	    {ext,_Pos,_} -> 
		emit({"?RT_PER:setext(false)"}),
		", ";
	    _ -> 
		""
	end,
    MaybeComma2 = 
	case optionals(CompList) of
	    [] -> MaybeComma1;
	    _ -> 
		emit(MaybeComma1),
		emit("Opt"),
		{",",nl}
	end,
    gen_enc_components_call(Erule,Typename,CompList,MaybeComma2,EncObj,Ext),
    emit({"].",nl}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate decode function for SEQUENCE and SET
%%
gen_decode_set(Erules,Typename,D) ->
    gen_decode_constructed(Erules,Typename,D).

gen_decode_sequence(Erules,Typename,D) ->
    gen_decode_constructed(Erules,Typename,D).

gen_decode_constructed(Erules,Typename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:clear(),
    {CompList,TableConsInfo} = 
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} ->
		{add_textual_order(CL),TCI};
	    #'SET'{tablecinf=TCI,components=CL} ->
%%		{add_textual_order(CL),TCI}
		{CL,TCI} % the textual order is already taken care of
	end,
    Ext = extensible_dec(CompList),
    MaybeComma1 = case Ext of
		      {ext,_Pos,_NumExt} -> 
			  gen_dec_extension_value("Bytes"),
			  {",",nl};
		      _ -> 
			  ""
		  end,
    Optionals = optionals(CompList),
    MaybeComma2 = case Optionals of
		      [] -> MaybeComma1;
		      _ -> 
			  Bcurr = asn1ct_name:curr(bytes),
			  Bnext = asn1ct_name:next(bytes),
			  emit(MaybeComma1),
			  GetoptCall = "} = ?RT_PER:getoptionals2(",
			  emit({"{Opt,",{var,Bnext},GetoptCall,
				{var,Bcurr},",",{asis,length(Optionals)},")"}),
			  asn1ct_name:new(bytes),
			  ", "    
		  end,
    {DecObjInf,UniqueFName,ValueIndex} = 
	case TableConsInfo of
%%	    {ObjectSet,AttrN,N,UniqueFieldName} ->%% N is index of attribute that determines constraint
	    #simpletableattributes{objectsetname=ObjectSet,
				   c_name=AttrN,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValIndex} ->
%%		{AttrN,ObjectSet};
		F = fun(#'ComponentType'{typespec=CT})->
			    case {asn1ct_gen:get_constraint(CT#type.constraint,componentrelation),CT#type.tablecinf} of
				{no,[{objfun,_}|_R]} -> true;
				_ -> false
			    end
		    end,
		case lists:any(F,CompList) of
		    true -> % when component relation constraint establish
			%% relation from a component to another components
			%% subtype component
			{{AttrN,{deep,ObjectSet,UniqueFieldName,ValIndex}},
			 UniqueFieldName,ValIndex};
		    false ->
			{{AttrN,ObjectSet},UniqueFieldName,ValIndex}
		end;
	    _ ->
		case D#type.tablecinf of
		    [{objfun,_}|_] ->
			{{"got objfun through args","ObjFun"},false,false};
		    _ ->
			{false,false,false}
		end
	end,
    NewCompList = wrap_compList(CompList),
    {AccTerm,AccBytes} =
	gen_dec_components_call(Erules,Typename,NewCompList,MaybeComma2,DecObjInf,Ext,length(Optionals)),
    case asn1ct_name:all(term) of
	[] -> emit(MaybeComma2); % no components at all
	_ -> emit({com,nl})
    end,
    case {AccTerm,AccBytes} of
	{[],[]} ->
	    ok;
	{_,[]} ->
	    ok;
	{[{ObjSet,LeadingAttr,Term}],ListOfOpenTypes} ->
	    DecObj = asn1ct_gen:un_hyphen_var(lists:concat(['DecObj',LeadingAttr,Term])),
	    ValueMatch = value_match(ValueIndex,Term),
	    {ObjSetMod,ObjSetName} =
		case ObjSet of
		    {M,O} -> {{asis,M},O};
		    _ -> {"?MODULE",ObjSet}
		end,
	    emit({DecObj," =",nl,"   ",ObjSetMod,":'getdec_",ObjSetName,"'(",
%		  {asis,UniqueFName},", ",Term,"),",nl}),
		  {asis,UniqueFName},", ",ValueMatch,"),",nl}),
	    gen_dec_listofopentypes(DecObj,ListOfOpenTypes,false)
    end,
    %% we don't return named lists any more   Cnames = mkcnamelist(CompList), 
    demit({"Result = "}), %dbg
    %% return value as record
    RecordName = lists:concat([get_record_name_prefix(),
			       asn1ct_gen:list2rname(Typename)]),
    case Typename of
	['EXTERNAL'] ->
	    emit({"   OldFormat={'",RecordName,
		  "'"}),
	    mkvlist(asn1ct_name:all(term)),
	    emit({"},",nl}),
	    emit({"   ASN11994Format =",nl,
		  "      asn1rt_check:transform_to_EXTERNAL1994",
		  "(OldFormat),",nl}),
	    emit("   {ASN11994Format,");
	_ ->
	    emit(["{{'",RecordName,"'"]),
	    %% CompList is used here because we don't want
	    %% ExtensionAdditionGroups to be wrapped in SEQUENCES when
	    %% we are ordering the fields according to textual order
	    mkvlist(textual_order(to_encoding_order(CompList),asn1ct_name:all(term))),
	    emit("},")
    end,
    emit({{curr,bytes},"}"}),
    emit({".",nl,nl}).

textual_order([#'ComponentType'{textual_order=undefined}|_],TermList) ->
    TermList;
textual_order(CompList,TermList) when is_list(CompList) ->
    OrderList = [Ix||#'ComponentType'{textual_order=Ix} <- CompList],
    [Term||{_,Term}<-
	       lists:sort(lists:zip(OrderList,
				    lists:sublist(TermList,length(OrderList))))];
	       %% sublist is just because Termlist can sometimes be longer than
	       %% OrderList, which it really shouldn't
textual_order({Root,Ext},TermList) ->
    textual_order(Root ++ Ext,TermList).

to_textual_order({Root,Ext}) ->
    {to_textual_order(Root),Ext};
to_textual_order(Cs) when is_list(Cs) ->
    case Cs of
	[#'ComponentType'{textual_order=undefined}|_] ->
	    Cs;
	_ ->
	    lists:keysort(#'ComponentType'.textual_order,Cs)
    end;
to_textual_order(Cs) ->
    Cs.

gen_dec_listofopentypes(_,[],_) ->
    emit(nl);
gen_dec_listofopentypes(DecObj,[{_Cname,{FirstPFN,PFNList},Term,TmpTerm,Prop}|Rest],_Update) ->

    asn1ct_name:new(tmpterm),
    asn1ct_name:new(reason),

    emit([Term," = ",nl]),

    N = case Prop of
	    mandatory -> 0;
	    'OPTIONAL' ->
		emit_opt_or_mand_check(asn1_NOVALUE,TmpTerm),
		6;
	    {'DEFAULT',Val} ->
		emit_opt_or_mand_check(Val,TmpTerm),
		6
	end,

    emit([indent(N+3),"case (catch ",DecObj,"(",
	  {asis,FirstPFN},", ",TmpTerm,", telltype,",{asis,PFNList},")) of",nl]),
    emit([indent(N+6),"{'EXIT', ",{curr,reason},"} ->",nl]),
    emit([indent(N+9),"exit({'Type not compatible with table constraint',",
	  {curr,reason},"});",nl]),
    emit([indent(N+6),"{",{curr,tmpterm},",_} ->",nl]),
    emit([indent(N+9),{curr,tmpterm},nl]),

    case Prop of
	mandatory ->
	    emit([indent(N+3),"end,",nl]);
	_ ->
	    emit([indent(N+3),"end",nl,
		  indent(3),"end,",nl])
    end,
    gen_dec_listofopentypes(DecObj,Rest,true).


emit_opt_or_mand_check(Val,Term) ->
    emit([indent(3),"case ",Term," of",nl,
	  indent(6),{asis,Val}," ->",{asis,Val},";",nl,
	  indent(6),"_ ->",nl]).

%% ENCODE GENERATOR FOR THE CHOICE TYPE *******
%% assume Val = {Alternative,AltType}
%% generate
%%[
%% ?RT_PER:set_choice(element(1,Val),Altnum,Altlist,ext),
%%case element(1,Val) of
%%    alt1 ->
%%	encode_alt1(element(2,Val));
%%    alt2 ->
%%	encode_alt2(element(2,Val))
%%end
%%].

gen_encode_choice(Erule,Typename,D) when is_record(D,type) ->
    {'CHOICE',CompList} = D#type.def,
    emit({"[",nl}),
    Ext = extensible_enc(CompList),
    gen_enc_choice(Erule,Typename,CompList,Ext),
    emit({nl,"].",nl}).

gen_decode_choice(Erules,Typename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:clear(),
    asn1ct_name:new(bytes),
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible_enc(CompList),
    gen_dec_choice(Erules,Typename,CompList,Ext),
    emit({".",nl}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Encode generator for SEQUENCE OF type


gen_encode_sof(Erule,Typename,SeqOrSetOf,D) when is_record(D,type) ->
    asn1ct_name:start(),
    {_SeqOrSetOf,ComponentType} = D#type.def,
    emit({"[",nl}),
    SizeConstraint =
	case asn1ct_gen:get_constraint(D#type.constraint,
				       'SizeConstraint') of
	    no -> undefined;
	    Range -> Range
	end,
    ObjFun =
	case D#type.tablecinf of
	    [{objfun,_}|_R] ->
		", ObjFun";
	    _->
		""
	end,
    emit({nl,indent(3),"?RT_PER:encode_length(",
	  {asis,SizeConstraint},
	  ",length(Val)),",nl}),
    emit({indent(3),"'enc_",asn1ct_gen:list2name(Typename),
	      "_components'(Val",ObjFun,", [])"}),
    emit({nl,"].",nl}),
    NewComponentType =
	case ComponentType#type.def of
	    {'ENUMERATED',_,Component}->
		ComponentType#type{def={'ENUMERATED',Component}};
	    _ -> ComponentType
	end,
    gen_encode_sof_components(Erule,Typename,SeqOrSetOf,NewComponentType).

gen_decode_sof(Erules,Typename,SeqOrSetOf,D) when is_record(D,type) ->
    asn1ct_name:start(),
    {_SeqOrSetOf,ComponentType} = D#type.def,
    SizeConstraint =
	case asn1ct_gen:get_constraint(D#type.constraint,
				       'SizeConstraint') of
	    no -> undefined;
	    Range -> Range
	end,
    ObjFun =
	case D#type.tablecinf of
	    [{objfun,_}|_R] ->
		", ObjFun";
	    _ ->
		""
	end,
    emit({nl,"{Num,Bytes1} = ?RT_PER:decode_length(Bytes,",{asis,SizeConstraint},"),",nl}),
    emit({"'dec_",asn1ct_gen:list2name(Typename),
	      "_components'(Num, Bytes1, telltype",ObjFun,", []).",nl}),
    NewComponentType =
	case ComponentType#type.def of
	    {'ENUMERATED',_,Component}->
		ComponentType#type{def={'ENUMERATED',Component}};
	    _ -> ComponentType
	end,
    gen_decode_sof_components(Erules,Typename,SeqOrSetOf,NewComponentType).

gen_encode_sof_components(Erule,Typename,SeqOrSetOf,Cont) ->
    {ObjFun,ObjFun_Var} =
	case Cont#type.tablecinf of
	    [{objfun,_}|_R] ->
		{", ObjFun",", _"};
	    _ ->
		{"",""}
	end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"_components'([]",
	  ObjFun_Var,", Acc) -> lists:reverse(Acc);",nl,nl}),
    emit({"'enc_",asn1ct_gen:list2name(Typename),"_components'([H|T]",
	  ObjFun,", Acc) ->",nl}),
    emit({"'enc_",asn1ct_gen:list2name(Typename),"_components'(T"}), 
    emit({ObjFun,", ["}),
    %% the component encoder
    Constructed_Suffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,
						       Cont#type.def),
    
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    Currmod = get(currmod),
    Ctgenmod = list_to_atom(lists:concat(["asn1ct_gen_",per,
					  asn1ct_gen:rt2ct_suffix()])),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    gen_encode_prim_wrapper(Ctgenmod,Erule,Cont,false,"H");
	{constructed,bif} ->
	    NewTypename = [Constructed_Suffix|Typename],
	    emit({"'enc_",asn1ct_gen:list2name(NewTypename),"'(H",
		  ObjFun,")",nl,nl});
	#'Externaltypereference'{module=Currmod,type=Ename} ->
	    emit({"'enc_",Ename,"'(H)",nl,nl});
	#'Externaltypereference'{module=EMod,type=EType} ->
	    emit({"'",EMod,"':'enc_",EType,"'(H)",nl,nl});
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim_wrapper(Ctgenmod,Erule,
				    #type{def='ASN1_OPEN_TYPE'},
				    false,"H");
	_ ->
	    emit({"'enc_",Conttype,"'(H)",nl,nl})
    end,
    emit({" | Acc]).",nl}).

gen_decode_sof_components(Erule,Typename,SeqOrSetOf,Cont) ->
    {ObjFun,ObjFun_Var} =
	case Cont#type.tablecinf of
	    [{objfun,_}|_R] ->
		{", ObjFun",", _"};
	    _ ->
		{"",""}
	end,
    emit({"'dec_",asn1ct_gen:list2name(Typename),
	  "_components'(0, Bytes, _",ObjFun_Var,", Acc) ->",nl,
	  indent(3),"{lists:reverse(Acc), Bytes};",nl}),
    emit({"'dec_",asn1ct_gen:list2name(Typename),
	  "_components'(Num, Bytes, _",ObjFun,", Acc) ->",nl}),
    emit({indent(3),"{Term,Remain} = "}),
    Constructed_Suffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,
						       Cont#type.def),
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    Ctgenmod = list_to_atom(lists:concat(["asn1ct_gen_",per,
					  asn1ct_gen:rt2ct_suffix()])),
    CurrMod = get(currmod),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    Ctgenmod:gen_dec_prim(Erule,Cont,"Bytes"),
	    emit({com,nl});
	{constructed,bif} ->
	    NewTypename = [Constructed_Suffix|Typename],
	    emit({"'dec_",asn1ct_gen:list2name(NewTypename),
		  "'(Bytes, telltype",ObjFun,"),",nl});
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(Bytes,telltype),",nl});
	#'Externaltypereference'{module=CurrMod,type=EType} ->
	    emit({"'dec_",EType,"'(Bytes,telltype),",nl});
	#'Externaltypereference'{module=EMod,type=EType} ->
	    emit({"'",EMod,"':'dec_",EType,"'(Bytes,telltype),",nl});
	'ASN1_OPEN_TYPE' ->
	    Ctgenmod:gen_dec_prim(Erule,#type{def='ASN1_OPEN_TYPE'},
				  "Bytes"),
	    emit({com,nl});
	_ ->
	    emit({"'dec_",Conttype,"'(Bytes,telltype),",nl})
    end,
    emit({indent(3),"'dec_",asn1ct_gen:list2name(Typename),
	  "_components'(Num-1, Remain, telltype",ObjFun,", [Term|Acc]).",nl}). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General and special help functions (not exported)

mkvlist([H|T]) ->
    emit(","),
    mkvlist2([H|T]);
mkvlist([]) ->
    true.
mkvlist2([H,T1|T]) ->
    emit({{var,H},","}),
    mkvlist2([T1|T]);
mkvlist2([H|T]) ->
    emit({{var,H}}),
    mkvlist2(T);
mkvlist2([]) ->
    true.


extensible_dec(CompList) when is_list(CompList) ->
    noext;
extensible_dec({RootList,ExtList}) ->
    {ext,length(RootList)+1,ext_length(ExtList)};
extensible_dec({Rl1,Ext,Rl2}) ->
     {ext,length(Rl1)+length(Rl2)+1,ext_length(Ext)}.

extensible_enc(CompList) when is_list(CompList) ->
    noext;
extensible_enc({RootList,ExtList}) ->
    {ext,length(RootList)+1,ext_length(ExtList)};
extensible_enc({Rl1,Ext,_Rl2}) ->
     {ext,length(Rl1)+1,ext_length(Ext)}.

ext_length(ExtList) -> ext_length(ExtList,normal,0).
ext_length([{'ExtensionAdditionGroup',_Num}|T],_,Acc)->
    ext_length(T,group,Acc);
ext_length(['ExtensionAdditionGroupEnd'|T],group,Acc) ->
    ext_length(T,normal,Acc+1);
ext_length([#'ComponentType'{}|T],State=group,Acc) ->
    ext_length(T,State,Acc);
ext_length([#'ComponentType'{}|T],State=normal,Acc) ->
    ext_length(T,State,Acc+1);
ext_length([],_,Acc) ->
    Acc.

extgroup_pos_and_length(CompList) when is_list(CompList) ->
    noextgroup;
extgroup_pos_and_length({RootList,ExtList}) ->
    extgrouppos(ExtList,length(RootList)+1);
extgroup_pos_and_length({Rl1,Ext,_Rl2}) ->
    extgrouppos(Ext,length(Rl1)+1).

extgrouppos([{'ExtensionAdditionGroup',_Num}|T],Pos) ->
    extgrouppos(T,Pos,0);
extgrouppos([_|T],Pos) ->
    extgrouppos(T,Pos+1);
extgrouppos([],_) ->
    noextgroup.

extgrouppos(['ExtensionAdditionGroupEnd'|_T],Pos,Len) ->
    {extgrouppos,Pos,Len};
extgrouppos([_|T],Pos,Len) ->
    extgrouppos(T,Pos,Len+1).
    


gen_dec_extension_value(_) ->
    emit({"{Ext,",{next,bytes},"} = ?RT_PER:getext(",{curr,bytes},")"}),
    asn1ct_name:new(bytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Produce a list with positions (in the Value record) where
%% there are optional components, start with 2 because first element
%% is the record name

optionals({L1,Ext,L2}) ->
    Opt1 = optionals(L1,[],2),
    ExtComps = length([C||C = #'ComponentType'{}<-Ext]),
    Opt2 = optionals(L2,[],2+length(L1)+ExtComps),
    Opt1 ++ Opt2;
optionals({L,_Ext}) -> optionals(L,[],2); 
optionals(L) -> optionals(L,[],2).

optionals([{'EXTENSIONMARK',_,_}|Rest],Acc,Pos) ->
    optionals(Rest,Acc,Pos); % optionals in extension are currently not handled
optionals([#'ComponentType'{prop='OPTIONAL'}|Rest],Acc,Pos) ->
		 optionals(Rest,[Pos|Acc],Pos+1);
optionals([#'ComponentType'{prop={'DEFAULT',Val}}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Pos,Val}|Acc],Pos+1);
optionals([#'ComponentType'{}|Rest],Acc,Pos) ->
		 optionals(Rest,Acc,Pos+1);
optionals([],Acc,_) ->
    lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%
%% create_optionality_table(Cs=[#'ComponentType'{textual_order=undefined}|_]) ->
%%     {NewCs,_} = lists:mapfoldl(fun(C,Num) ->
%% 				       {C#'ComponentType'{textual_order=Num},
%% 					Num+1}
%% 			       end,
%% 			       1,Cs),
%%     create_optionality_table(NewCs);
create_optionality_table(Cs) ->
    IsOptional = fun('OPTIONAL') -> true;
		    ({'DEFAULT',_}) -> true;
		    (_) -> false
		 end,
    OptionalsElNum = [TO || #'ComponentType'{prop = O,textual_order=TO} <- Cs,
			   IsOptional(O)],
    {Table,_} = lists:mapfoldl(fun(X,Num) ->
				       {{Num,X},Num+1}
			       end,
			       1,lists:sort(OptionalsElNum)),
    Table.
get_optionality_pos(TextPos,OptTable) ->
    case lists:keysearch(TextPos,2,OptTable) of
	{value,{OptNum,_}} ->
	    OptNum;
	_ ->
	    no_num
    end.

to_encoding_order(Cs) when is_list(Cs) ->
    Cs;
to_encoding_order(Cs = {_Root,_Ext}) ->
    Cs;
to_encoding_order({R1,Ext,R2}) ->
    {R1++R2,Ext}.

add_textual_order(Cs) when is_list(Cs) ->
    {NewCs,_} = add_textual_order1(Cs,1),
    NewCs;
add_textual_order({Root,Ext}) ->
    {NewRoot,Num} = add_textual_order1(Root,1),
    {NewExt,_} = add_textual_order1(Ext,Num),
    {NewRoot,NewExt};
add_textual_order({R1,Ext,R2}) ->
    {NewR1,Num1} = add_textual_order1(R1,1),
    {NewExt,Num2} = add_textual_order1(Ext,Num1),
    {NewR2,_} = add_textual_order1(R2,Num2),
    {NewR1,NewExt,NewR2}.
%%add_textual_order1(Cs=[#'ComponentType'{textual_order=Int}|_],I)
%%  when is_integer(Int) ->
%%    {Cs,I};
add_textual_order1(Cs,NumIn) ->
    lists:mapfoldl(fun(C=#'ComponentType'{},Num) ->
			   {C#'ComponentType'{textual_order=Num},
			    Num+1};
		      (OtherMarker,Num) ->
			   {OtherMarker,Num}
		   end,
		   NumIn,Cs).

gen_enc_components_call(Erule,TopType,{Root1,ExtList,Root2},MaybeComma,DynamicEnc,Ext) ->
    gen_enc_components_call(Erule,TopType,{Root1++Root2,ExtList},MaybeComma,DynamicEnc,Ext);
gen_enc_components_call(Erule,TopType,{CompList,ExtList},MaybeComma,DynamicEnc,Ext) ->
    %% The type has extensionmarker
    Rpos = gen_enc_components_call1(Erule,TopType,CompList,1,MaybeComma,DynamicEnc,noext),
    case Ext of
	{ext,_,ExtNum} when ExtNum > 0 ->
	    emit([nl,
		  ",Extensions",nl]);
	_ -> true
    end,
    %handle extensions
    NewExtList = wrap_extensionAdditionGroups(ExtList),
    gen_enc_components_call1(Erule,TopType,NewExtList,Rpos,MaybeComma,DynamicEnc,Ext);
gen_enc_components_call(Erule,TopType, CompList, MaybeComma, DynamicEnc, Ext) ->
    %% The type has no extensionmarker
    gen_enc_components_call1(Erule,TopType,CompList,1,MaybeComma,DynamicEnc,Ext).

gen_enc_components_call1(Erule,TopType,
			 [C=#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],
			 Tpos,
			 MaybeComma, DynamicEnc, Ext) ->

    put(component_type,{true,C}), 
    %% information necessary in asn1ct_gen_per_rt2ct:gen_encode_prim
    TermNo =
	case C#'ComponentType'.textual_order of
	    undefined ->
		Tpos;
	    CanonicalNum ->
		CanonicalNum
	end,
    emit(MaybeComma),
    case Prop of
	'OPTIONAL' ->
	    gen_enc_component_optional(Erule,TopType,Cname,Type,TermNo,DynamicEnc,Ext);
	{'DEFAULT',DefVal} ->
	    gen_enc_component_default(Erule,TopType,Cname,Type,TermNo,DynamicEnc,Ext,DefVal);
	_ ->
	    case Ext of
		{ext,ExtPos,_} when Tpos >= ExtPos ->
		    gen_enc_component_optional(Erule,TopType,Cname,Type,TermNo,DynamicEnc,Ext);
		_ ->
		    gen_enc_component_mandatory(Erule,TopType,Cname,Type,TermNo,DynamicEnc,Ext)
	    end
    end,

    erase(component_type),

    case Rest of
	[] ->
	    Tpos+1;
	_ ->
	    emit({com,nl}),
	    gen_enc_components_call1(Erule,TopType,Rest,Tpos+1,"",DynamicEnc,Ext)
    end;
gen_enc_components_call1(_Erule,_TopType,[],Pos,_,_,_) ->
	Pos.

gen_enc_component_default(Erule,TopType,Cname,Type,Pos,DynamicEnc,Ext,DefaultVal) ->
    Element = make_element(Pos+1,asn1ct_gen:mk_var(asn1ct_name:curr(val)),Cname),
    emit({"case ",Element," of",nl}),
%    emit({"asn1_DEFAULT -> [];",nl}),
    emit({"DFLT when DFLT == asn1_DEFAULT; DFLT == ",{asis,DefaultVal}," -> [];",nl}),

    asn1ct_name:new(tmpval),
    emit({{curr,tmpval}," ->",nl}),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    NextElement = asn1ct_gen:mk_var(asn1ct_name:curr(tmpval)),
    gen_enc_line(Erule,TopType,Cname,Type,NextElement, Pos,DynamicEnc,Ext),
    emit({nl,"end"}).

gen_enc_component_optional(Erule,TopType,Cname,
			   Type=#type{def=#'SEQUENCE'{
					extaddgroup=Number,
					components=_ExtGroupCompList}},
			   Pos,DynamicEnc,Ext) when is_integer(Number) ->

    Element = make_element(Pos+1,asn1ct_gen:mk_var(asn1ct_name:curr(val)),Cname),
    emit({"case ",Element," of",nl}),

    emit({"asn1_NOVALUE -> [];",nl}),
    asn1ct_name:new(tmpval),
    emit({{curr,tmpval}," ->",nl}),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    NextElement = asn1ct_gen:mk_var(asn1ct_name:curr(tmpval)),
    gen_enc_line(Erule,TopType,Cname,Type,NextElement, Pos,DynamicEnc,Ext),
    emit({nl,"end"});
gen_enc_component_optional(Erule,TopType,Cname,Type,Pos,DynamicEnc,Ext) ->
    Element = make_element(Pos+1,asn1ct_gen:mk_var(asn1ct_name:curr(val)),Cname),
    emit({"case ",Element," of",nl}),

    emit({"asn1_NOVALUE -> [];",nl}),
    asn1ct_name:new(tmpval),
    emit({{curr,tmpval}," ->",nl}),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    NextElement = asn1ct_gen:mk_var(asn1ct_name:curr(tmpval)),
    gen_enc_line(Erule,TopType,Cname,Type,NextElement, Pos,DynamicEnc,Ext),
    emit({nl,"end"}).

gen_enc_component_mandatory(Erule,TopType,Cname,Type,Pos,DynamicEnc,Ext) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    gen_enc_line(Erule,TopType,Cname,Type,[],Pos,DynamicEnc,Ext).

gen_enc_line(Erule,TopType, Cname, Type, [], Pos,DynamicEnc,Ext) ->
    Element = make_element(Pos+1,asn1ct_gen:mk_var(asn1ct_name:curr(val)),Cname),
    gen_enc_line(Erule,TopType,Cname,Type,Element, Pos,DynamicEnc,Ext);
gen_enc_line(Erule,TopType,Cname,Type,Element, _Pos,DynamicEnc,Ext) ->
    Ctgenmod = list_to_atom(lists:concat(["asn1ct_gen_",per,
					  asn1ct_gen:rt2ct_suffix()])),
    Atype = 
	case Type of
	    #type{def=#'ObjectClassFieldType'{type=InnerType}} ->
		InnerType;
	    _  ->
		asn1ct_gen:get_inner(Type#type.def)
	end,

    case Ext of
	{ext,_Ep1,_} ->
	    emit(["?RT_PER:encode_open_type(dummy,?RT_PER:complete("]);
	_ -> true
    end,
    case Atype of
	{typefield,_} ->
	    case DynamicEnc of
		{_LeadingAttrName,Fun} ->
		    case (Type#type.def)#'ObjectClassFieldType'.fieldname of
			{notype,T} ->
			    throw({error,{notype,type_from_object,T}});
			{Name,RestFieldNames} when is_atom(Name) ->
			    emit({"?RT_PER:encode_open_type([],?RT_PER:complete(",nl}),
			    emit({"   ",Fun,"(",{asis,Name},", ",
				  Element,", ",{asis,RestFieldNames},")))"});
			Other ->
			    throw({asn1,{'internal error',Other}})
		    end
	    end;
	{objectfield,PrimFieldName1,PFNList} ->
	    case DynamicEnc of
		{_LeadingAttrName,Fun} ->
		    emit({"?RT_PER:encode_open_type([],"
			  "?RT_PER:complete(",nl}),
		    emit({"   ",Fun,"(",{asis,PrimFieldName1},
			  ", ",Element,", ",{asis,PFNList},")))"})
	    end;
	_ ->
	    CurrMod = get(currmod),
	    case asn1ct_gen:type(Atype) of
		#'Externaltypereference'{module=Mod,type=EType} when 
		      (CurrMod==Mod) ->
		    emit({"'enc_",EType,"'(",Element,")"});
		#'Externaltypereference'{module=Mod,type=EType} ->
		    emit({"'",Mod,"':'enc_",
			  EType,"'(",Element,")"});
		#typereference{val=Ename} ->
		    emit({"'enc_",Ename,"'(",Element,")"});
		{notype,_} ->
		    emit({"'enc_",Atype,"'(",Element,")"});
		{primitive,bif} ->
		    EncType =
			case Atype of
			    {fixedtypevaluefield,_,Btype} ->
				Btype;
			    _ ->
				Type
			end,
		    gen_encode_prim_wrapper(Ctgenmod,Erule,EncType,
					    false,Element);
		'ASN1_OPEN_TYPE' ->
		    case Type#type.def of
			#'ObjectClassFieldType'{type=OpenType} ->
			    gen_encode_prim_wrapper(Ctgenmod,Erule,
						    #type{def=OpenType},
						    false,Element);
			_ ->
			    gen_encode_prim_wrapper(Ctgenmod,Erule,Type,
						    false,Element)
		    end;
		{constructed,bif} ->
		    NewTypename = [Cname|TopType],
		    case {Type#type.tablecinf,DynamicEnc} of
			{[{objfun,_}|_R],{_,EncFun}} ->
			    emit({"'enc_",
				  asn1ct_gen:list2name(NewTypename),
				  "'(",Element,", ",EncFun,")"});
			_ ->
			    emit({"'enc_",
				  asn1ct_gen:list2name(NewTypename),
				  "'(",Element,")"})
		    end
	    end
    end,
    case Ext of 
	{ext,_Ep2,_} ->
	    emit(["))"]);
	_ -> true
    end.
gen_dec_components_call(Erule,TopType,{Root1,ExtList,Root2},MaybeComma,DecInfObj,Ext,NumberOfOptionals) ->
    gen_dec_components_call(Erule,TopType,{Root1++Root2,ExtList},MaybeComma,DecInfObj,Ext,NumberOfOptionals);
gen_dec_components_call(Erule,TopType,{CompList,ExtList},MaybeComma,
			DecInfObj,Ext,NumberOfOptionals) ->
    %% The type has extensionmarker
    OptTable = create_optionality_table(CompList),
    {Rpos,AccTerm,AccBytes} = 
	gen_dec_components_call1(Erule,TopType, CompList, 1, OptTable, 
				 MaybeComma,DecInfObj,noext,[],[],
				 NumberOfOptionals),
    emit([",",nl,"{Extensions,",{next,bytes},"} = "]),
    emit(["?RT_PER:getextension(Ext,",{curr,bytes},"),",nl]),
    asn1ct_name:new(bytes),
    NewExtList = wrap_extensionAdditionGroups(ExtList),
    {_Epos,AccTermE,AccBytesE} = 
	gen_dec_components_call1(Erule,TopType,NewExtList,Rpos, OptTable,
				 "",DecInfObj,Ext,[],[],NumberOfOptionals),
    case ExtList of
	[] -> true;
	_ -> emit([",",nl])
    end,
    emit([{next,bytes},"= ?RT_PER:skipextensions(",{curr,bytes},",",
	  length(ExtList)+1,",Extensions)",nl]),
    asn1ct_name:new(bytes),
    {AccTerm++AccTermE,AccBytes++AccBytesE};

gen_dec_components_call(Erule,TopType,CompList,MaybeComma,DecInfObj,
			Ext,NumberOfOptionals) ->
    %% The type has no extensionmarker
    OptTable = create_optionality_table(CompList),
    {_,AccTerm,AccBytes} =
	gen_dec_components_call1(Erule,TopType, CompList, 1, OptTable,
				 MaybeComma,DecInfObj,Ext,[],[],
				 NumberOfOptionals),
    {AccTerm,AccBytes}.


gen_dec_components_call1(Erule,TopType,
			 [C=#'ComponentType'{name=Cname,typespec=Type,prop=Prop,textual_order=TextPos}|Rest],
			 Tpos,OptTable,MaybeComma,DecInfObj,Ext,AccTerm,AccBytes,NumberOfOptionals) ->
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,_Enum} -> Tpos - Epos + 1
	  end,
    emit(MaybeComma),
    InnerType = 
	case Type#type.def of
	    #'ObjectClassFieldType'{type=InType} ->
		InType;
	    Def ->
		asn1ct_gen:get_inner(Def)
	end,

    case InnerType of
	#'Externaltypereference'{type=T} ->
	    emit({nl,"%%  attribute number ",TextPos," with type ",
		  T,nl});
	IT when is_tuple(IT) ->
	    emit({nl,"%%  attribute number ",TextPos," with type ",
		  element(2,IT),nl});
	_ ->
	    emit({nl,"%% attribute number ",TextPos," with type ",
		  InnerType,nl})
    end,

    IsMandatoryAndPredefinedTableC =
	fun(noext,mandatory,{"got objfun through args","ObjFun"}) ->
		true;
	   (_,_,{"got objfun through args","ObjFun"}) ->
		false;
	   (_,_,_) ->
		true
	end,
    case {InnerType,IsMandatoryAndPredefinedTableC(Ext,Prop,DecInfObj)} of
%%	{typefield,_} when Ext == noext, Prop == mandatory ->
	{{typefield,_},true}  ->
	    %% DecInfObj /= {"got objfun through args","ObjFun"} |
	    %% (DecInfObj == {"got objfun through args","ObjFun"} &
	    %% Ext == noext & Prop == mandatory)
	    asn1ct_name:new(term),
	    asn1ct_name:new(tmpterm),
	    emit({"{",{curr,tmpterm},", ",{next,bytes},"} = "});
	%%{objectfield,_,_} when Ext == noext, Prop == mandatory ->
	{{objectfield,_,_},true} ->
	    asn1ct_name:new(term),
	    asn1ct_name:new(tmpterm),
	    emit({"{",{curr,tmpterm},", ",{next,bytes},"} = "});
	_ ->
	    case Type of
		#type{def=#'SEQUENCE'{
			extaddgroup=Number1,
			components=ExtGroupCompList1}} when is_integer(Number1)->
		    emit({"{{_,"}),
		    emit_extaddgroupTerms(term,ExtGroupCompList1),
		    emit({"}"});
		_ ->
		    asn1ct_name:new(term),
		    emit({"{",{curr,term}})
	    end,
	    emit({",",{next,bytes},"} = "})
    end,

    case {Ext,Prop,is_optimized(Erule)} of
	{noext,mandatory,_} -> ok; % generate nothing
	{noext,_,_} -> %% OPTIONAL or DEFAULT
	    OptPos = get_optionality_pos(TextPos,OptTable),
	    Element = io_lib:format("Opt band (1 bsl ~w)",[NumberOfOptionals - OptPos]),
	    emit(["case ",Element," of",nl]),
	    emit(["  _Opt",TextPos," when _Opt",TextPos," > 0 ->"]);
	{_,_,false} -> %% extension element, not bitstring
	    emit(["case Extensions of",nl]),
	    emit(["  _ when size(Extensions) >= ",Pos,",element(",Pos,",Extensions) == 1 ->",nl]);
	_ ->
	    emit(["case Extensions of",nl]),
	    emit(["  <<_:",Pos-1,",1:1,_/bitstring>> when bit_size(Extensions) >= ",Pos," ->",nl])
    end,
    put(component_type,{true,C}),
    {TermVar,BytesVar} = gen_dec_line(Erule,TopType,Cname,Type,Tpos,DecInfObj,Ext,Prop),
    erase(component_type),
    case {Ext,Prop} of
	{noext,mandatory} -> true; % generate nothing
	{noext,_} ->
	    emit([";",nl,"0 ->"]),
	    emit(["{"]),
	    gen_dec_component_no_val(Ext,Prop),
	    emit({",",{curr,bytes},"}",nl}),
	    emit([nl,"end"]);
	_ ->
	    emit([";",nl,"_  ->",nl]),
	    emit(["{"]),
	    case Type of
		#type{def=#'SEQUENCE'{
			extaddgroup=Number2,
			components=ExtGroupCompList2}} when is_integer(Number2)->
		    emit({"{extAddGroup,"}),
		    gen_dec_extaddGroup_no_val(Ext,ExtGroupCompList2),
		    emit({"}"});
		_ ->
		    gen_dec_component_no_val(Ext,Prop)
	    end,
	    emit({",",{curr,bytes},"}",nl}),
	    emit([nl,"end"])
    end,    
    asn1ct_name:new(bytes),
    case Rest of
	[] ->
	    {Tpos+1,AccTerm++TermVar,AccBytes++BytesVar};
	_ ->
	    emit({com,nl}),
	    gen_dec_components_call1(Erule,TopType,Rest,Tpos+1,OptTable,
				     "",DecInfObj,Ext, AccTerm++TermVar,
				     AccBytes++BytesVar,NumberOfOptionals)
    end;

gen_dec_components_call1(_,_TopType,[],Pos,_OptTable,_,_,_,AccTerm,AccBytes,_NumberOfOptionals) ->
    {Pos,AccTerm,AccBytes}.

gen_dec_extaddGroup_no_val(Ext,[#'ComponentType'{prop=Prop}])->
    gen_dec_component_no_val(Ext,Prop),
    ok;
gen_dec_extaddGroup_no_val(Ext,[#'ComponentType'{prop=Prop}|Rest])->
    gen_dec_component_no_val(Ext,Prop),
    emit({","}),
    gen_dec_extaddGroup_no_val(Ext,Rest);
gen_dec_extaddGroup_no_val(_, []) ->
    ok.

gen_dec_component_no_val(_,{'DEFAULT',DefVal}) ->
    emit([{asis,DefVal}]);
gen_dec_component_no_val(_,'OPTIONAL') ->
    emit({"asn1_NOVALUE"});
gen_dec_component_no_val({ext,_,_},mandatory) ->
    emit({"asn1_NOVALUE"}).
    

gen_dec_line(Erule,TopType,Cname,Type,Pos,DecInfObj,Ext,Prop)  ->
    Ctgenmod = list_to_atom(lists:concat(["asn1ct_gen_",per,
					  asn1ct_gen:rt2ct_suffix()])),
    Atype = 
	case Type of
	    #type{def=#'ObjectClassFieldType'{type=InnerType}} ->
		InnerType;
	    _  ->
		asn1ct_gen:get_inner(Type#type.def)
	end,

    BytesVar0 = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
    BytesVar = case Ext of
		   {ext,Ep,_} when Pos >= Ep ->
		       emit(["begin",nl,"{TmpVal",Pos,",Trem",Pos,
			     "}=?RT_PER:decode_open_type(",
			     {curr,bytes},",[]),",nl,
			     "{TmpValx",Pos,",_}="]),
		       io_lib:format("TmpVal~p",[Pos]);
		   _ -> BytesVar0
	       end,
    SaveBytes = 
	case Atype of
	    {typefield,_} ->
		case DecInfObj of
		    false -> % This is in a choice with typefield components
			{Name,RestFieldNames} = 
			    (Type#type.def)#'ObjectClassFieldType'.fieldname,

			asn1ct_name:new(tmpterm),
			asn1ct_name:new(reason),
			emit([indent(2),"{",{curr,tmpterm},", ",{next,bytes},
			      "} = ?RT_PER:decode_open_type(",{curr,bytes},
			      ", []),",nl]),
			emit([indent(2),"case (catch ObjFun(",
			      {asis,Name},
			      ",",{curr,tmpterm},",telltype,",
			      {asis,RestFieldNames},")) of", nl]),
			emit([indent(4),"{'EXIT',",{curr,reason},"} ->",nl]),
			emit([indent(6),"exit({'Type not ",
			      "compatible with table constraint', ",
			      {curr,reason},"});",nl]),
			asn1ct_name:new(tmpterm),
			emit([indent(4),"{",{curr,tmpterm},", _} ->",nl]),
			emit([indent(6),"{",{asis,Cname},", {",{curr,tmpterm},", ",
			      {next,bytes},"}}",nl]),
			emit([indent(2),"end"]),
			[];
		    {"got objfun through args","ObjFun"} ->
			%% this is when the generated code gots the
			%% objfun though arguments on function
			%% invocation.
			if
			    Ext == noext andalso Prop == mandatory ->
				ok;
			    true ->
				asn1ct_name:new(tmpterm),
				asn1ct_name:new(tmpbytes),
				emit([nl,"    {",{curr,tmpterm},", ",{curr,tmpbytes},"} ="])
			end,
			{Name,RestFieldNames} = 
			    (Type#type.def)#'ObjectClassFieldType'.fieldname,
			emit(["?RT_PER:decode_open_type(",{curr,bytes},
			      ", []),",nl]),
			if
			   Ext == noext andalso Prop == mandatory -> 
				emit([{curr,term}," =",nl,"      "]);
			    true ->
				emit(["     {"])
			end,
			emit(["case (catch ObjFun(",{asis,Name},",",
			      {curr,tmpterm},",telltype,",
			      {asis,RestFieldNames},")) of", nl]),
			emit(["    {'EXIT',",{curr,reason},"} ->",nl]),
			emit([indent(6),"exit({'Type not ",
			      "compatible with table constraint', ",
			      {curr,reason},"});",nl]),
			asn1ct_name:new(tmpterm),
			emit([indent(4),"{",{curr,tmpterm},", _} ->",nl]),
			emit([indent(6),{curr,tmpterm},nl]),
			emit([indent(2),"end"]),
			if
			    Ext == noext andalso Prop == mandatory ->
				ok;
			    true ->
				emit([",",nl,{curr,tmpbytes},"}"])
			end,
			[];
		    _ ->
			emit(["?RT_PER:decode_open_type(",{curr,bytes},
			      ", [])"]),
			RefedFieldName = 
			    (Type#type.def)#'ObjectClassFieldType'.fieldname,

			[{Cname,RefedFieldName,
			  asn1ct_gen:mk_var(asn1ct_name:curr(term)),
			  asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),
			  get_components_prop()}]
		end;
	    {objectfield,PrimFieldName1,PFNList} ->
		emit(["?RT_PER:decode_open_type(",{curr,bytes},", [])"]),
		[{Cname,{PrimFieldName1,PFNList},
		  asn1ct_gen:mk_var(asn1ct_name:curr(term)),
		  asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),
		  get_components_prop()}];
	    _ ->
		CurrMod = get(currmod),
		case asn1ct_gen:type(Atype) of
		    #'Externaltypereference'{module=CurrMod,type=EType} ->
			emit({"'dec_",EType,"'(",BytesVar,",telltype)"});
		    #'Externaltypereference'{module=Mod,type=EType} ->
			emit({"'",Mod,"':'dec_",EType,"'(",BytesVar,
			      ",telltype)"});
		    {primitive,bif} ->
			case Atype of
			   {fixedtypevaluefield,_,Btype} ->
				Ctgenmod:gen_dec_prim(Erule,Btype,
						      BytesVar);
			    _ ->
				Ctgenmod:gen_dec_prim(Erule,Type,
						      BytesVar)
			end;
		    'ASN1_OPEN_TYPE' ->
			case Type#type.def of
			    #'ObjectClassFieldType'{type=OpenType} ->
				Ctgenmod:gen_dec_prim(Erule,#type{def=OpenType},
						      BytesVar);
			    _ ->
				Ctgenmod:gen_dec_prim(Erule,Type,
						      BytesVar)
			end;
		    #typereference{val=Dname} ->
			emit({"'dec_",Dname,"'(",BytesVar,",telltype)"});
		    {notype,_} ->
			emit({"'dec_",Atype,"'(",BytesVar,",telltype)"});
		    {constructed,bif} ->
			NewTypename = [Cname|TopType],
			case Type#type.tablecinf of
			    [{objfun,_}|_R] ->
				emit({"'dec_",asn1ct_gen:list2name(NewTypename),
				      "'(",BytesVar,", telltype, ObjFun)"});
			    _ ->
				emit({"'dec_",asn1ct_gen:list2name(NewTypename),
				      "'(",BytesVar,", telltype)"})
			end
		end,
		case DecInfObj of
		    {Cname,{_,OSet,UniqueFName,ValIndex}} ->
			Term = asn1ct_gen:mk_var(asn1ct_name:curr(term)),
			ValueMatch = value_match(ValIndex,Term),
			{ObjSetMod,ObjSetName} =
			    case OSet of
				{M,O} -> {{asis,M},O};
				_ -> {"?MODULE",OSet}
			    end,
			emit({",",nl,"ObjFun = ",ObjSetMod,
			      ":'getdec_",ObjSetName,"'(",
			      {asis,UniqueFName},", ",ValueMatch,")"});
		    _ ->
			ok
		end,
		[]
	end,
    case Ext of 
	{ext,Ep2,_} when Pos >= Ep2 ->
	    emit([", {TmpValx",Pos,",Trem",Pos,"}",nl,"end"]);
	_ -> true
    end,
    %% Prepare return value
    case DecInfObj of
	{Cname,ObjSet} ->
	    ObjSetRef =
		case ObjSet of
		    {deep,OSName,_,_} ->
			OSName;
		    _ -> ObjSet
		end,
	    {[{ObjSetRef,Cname,asn1ct_gen:mk_var(asn1ct_name:curr(term))}],
	     SaveBytes};
	_ ->
	    {[],SaveBytes}
    end.

gen_enc_choice(Erule,TopType,CompList,Ext) ->
    gen_enc_choice_tag(CompList, [], Ext),
    emit({com,nl}),
    emit({"case element(1,Val) of",nl}),
    gen_enc_choice2(Erule,TopType, CompList, Ext),
    emit({nl,"end"}).

gen_enc_choice_tag({C1,C2},_,_) ->
    N1 = get_name_list(C1),
    N2 = get_name_list(C2),
    emit(["?RT_PER:set_choice(element(1,Val),",
	  {asis,{N1,N2}},", ",{asis,{length(N1),length(N2)}},")"]);

gen_enc_choice_tag({C1,C2,C3},_,_) ->
    N1 = get_name_list(C1),
    N2 = get_name_list(C2),
    N3 = get_name_list(C3),
    Root = N1 ++ N3,
    emit(["?RT_PER:set_choice(element(1,Val),",
	  {asis,{Root,N2}},", ",{asis,{length(Root),length(N2)}},")"]);
gen_enc_choice_tag(C,_,_) ->
    N = get_name_list(C),
    emit(["?RT_PER:set_choice(element(1,Val),",
	  {asis,N},", ",{asis,length(N)},")"]).

get_name_list(L) ->
    get_name_list(L,[]).

get_name_list([#'ComponentType'{name=Name}|T], Acc) ->
    get_name_list(T,[Name|Acc]);
get_name_list([], Acc) ->
    lists:reverse(Acc).


gen_enc_choice2(Erule,TopType, {L1,L2}, Ext) ->
    gen_enc_choice2(Erule,TopType, L1 ++ L2, 0, Ext);
gen_enc_choice2(Erule,TopType, {L1,L2,L3}, Ext) ->
    gen_enc_choice2(Erule,TopType, L1 ++ L3 ++ L2, 0, Ext);
gen_enc_choice2(Erule,TopType, L, Ext) ->
    gen_enc_choice2(Erule,TopType, L, 0, Ext).

gen_enc_choice2(Erule,TopType,[H1,H2|T], Pos, Ext) 
when is_record(H1,'ComponentType'), is_record(H2,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    EncObj =
	case asn1ct_gen:get_constraint(Type#type.constraint,
				       componentrelation) of
	    no -> 
		case Type#type.tablecinf of
		    [{objfun,_}|_] ->
			{"got objfun through args","ObjFun"};
		    _ ->false
		end;
	    _ -> {no_attr,"ObjFun"}
	end,
    emit({{asis,Cname}," ->",nl}),
    DoExt = case Ext of
		{ext,ExtPos,_} when (Pos + 1) < ExtPos -> noext;
		_ -> Ext
	    end,
    gen_enc_line(Erule,TopType,Cname,Type,"element(2,Val)", 
		 Pos+1,EncObj,DoExt),
    emit({";",nl}),
    gen_enc_choice2(Erule,TopType,[H2|T], Pos+1, Ext);
gen_enc_choice2(Erule,TopType,[H1|T], Pos, Ext) 
  when is_record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    EncObj =
	case asn1ct_gen:get_constraint(Type#type.constraint,
				       componentrelation) of
	    no -> 
		case Type#type.tablecinf of
		    [{objfun,_}|_] ->
			{"got objfun through args","ObjFun"};
		    _ ->false
		end;
	    _ -> {no_attr,"ObjFun"}
	end,
    emit({{asis,H1#'ComponentType'.name}," ->",nl}),
    DoExt = case Ext of
		{ext,ExtPos,_} when (Pos + 1) < ExtPos -> noext;
		_ -> Ext
	    end,
    gen_enc_line(Erule,TopType,Cname,Type,"element(2,Val)", 
		 Pos+1,EncObj,DoExt),
    gen_enc_choice2(Erule,TopType,T, Pos+1, Ext);
gen_enc_choice2(_Erule,_,[], _, _)  ->
    true.

gen_dec_choice(Erule,TopType,CompList,{ext,Pos,NumExt}) ->
    emit({"{Ext,",{curr,bytes},"} = ?RT_PER:getbit(Bytes),",nl}),
    asn1ct_name:new(bytes),
    gen_dec_choice1(Erule,TopType,CompList,{ext,Pos,NumExt});
gen_dec_choice(Erule,TopType,CompList,noext) ->
    gen_dec_choice1(Erule,TopType,CompList,noext).

gen_dec_choice1(Erule,TopType,CompList,noext) ->
    emit({"{Choice,",{curr,bytes},
	  "} = ?RT_PER:getchoice(",{prev,bytes},",",
	  length(CompList),", 0),",nl}),
    emit({"{Cname,{Val,NewBytes}} = case Choice of",nl}),
    gen_dec_choice2(Erule,TopType,CompList,noext),
    emit({nl,"end,",nl}),
    emit({nl,"{{Cname,Val},NewBytes}"});
gen_dec_choice1(Erule,TopType,{RootList,ExtList},Ext) ->
    NewList = RootList ++ ExtList,
    gen_dec_choice1(Erule,TopType, NewList, Ext);
gen_dec_choice1(Erule,TopType,{RootList,ExtList,RootList2},Ext) ->
    NewList = RootList ++ RootList2 ++ ExtList,
    gen_dec_choice1(Erule,TopType, NewList, Ext);
gen_dec_choice1(Erule,TopType,CompList,{ext,ExtPos,ExtNum}) ->
    emit({"{Choice,",{curr,bytes},
	  "} = ?RT_PER:getchoice(",{prev,bytes},",",
	  length(CompList)-ExtNum,",Ext ),",nl}),
    emit({"{Cname,{Val,NewBytes}} = case Choice + Ext*",ExtPos-1," of",nl}),
    gen_dec_choice2(Erule,TopType,CompList,{ext,ExtPos,ExtNum}),
    case Erule of
	per ->
	    emit([";",nl,"_ -> {asn1_ExtAlt,",nl,
		  "      fun() -> ",nl,
		  "          {XTerm,XBytes} = ?RT_PER:decode_open_type(",
		  {curr,bytes},",[]),",nl,
		  "          {binary_to_list(XTerm),XBytes}",nl,
		  "      end()}"]);
	_ ->
	    emit([";",nl,"_ -> {asn1_ExtAlt, ?RT_PER:decode_open_type(",
		  {curr,bytes},",[])}"])
    end,
    emit({nl,"end,",nl}),
    emit({nl,"{{Cname,Val},NewBytes}"}).


gen_dec_choice2(Erule,TopType,L,Ext) ->
    gen_dec_choice2(Erule,TopType,L,0,Ext).

gen_dec_choice2(Erule,TopType,[H1,H2|T],Pos,Ext) 
when is_record(H1,'ComponentType'), is_record(H2,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    case Type#type.def of
	#'ObjectClassFieldType'{type={typefield,_}} ->
	    emit({Pos," -> ",nl}),
	    wrap_gen_dec_line(Erule,H1,TopType,Cname,Type,Pos+1,false,Ext),
	    emit({";",nl});
	_ ->
	    emit({Pos," -> {",{asis,Cname},",",nl}),
	    wrap_gen_dec_line(Erule,H1,TopType,Cname,Type,Pos+1,false,Ext),
	    emit({"};",nl})
    end,
    gen_dec_choice2(Erule,TopType,[H2|T],Pos+1,Ext);
gen_dec_choice2(Erule,TopType,[H1,_H2|T],Pos,Ext) when is_record(H1,'ComponentType') ->
    gen_dec_choice2(Erule,TopType,[H1|T],Pos,Ext); % skip extensionmark
gen_dec_choice2(Erule,TopType,[H1|T],Pos,Ext) when is_record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    case Type#type.def of
	#'ObjectClassFieldType'{type={typefield,_}} ->
	    emit({Pos," -> ",nl}),
	    wrap_gen_dec_line(Erule,H1,TopType,Cname,Type,Pos+1,false,Ext);
	_ ->
	    emit({Pos," -> {",{asis,Cname},",",nl}),
	    wrap_gen_dec_line(Erule,H1,TopType,Cname,Type,Pos+1,false,Ext),
	    emit("}")
    end,
    gen_dec_choice2(Erule,TopType,[T],Pos+1);
gen_dec_choice2(Erule,TopType,[_|T],Pos,Ext) ->
    gen_dec_choice2(Erule,TopType,T,Pos,Ext);% skip extensionmark
gen_dec_choice2(_,_,[],Pos,_)  ->
    Pos.

indent(N) ->
    lists:duplicate(N,32). % 32 = space

gen_encode_prim_wrapper(CtgenMod,Erule,Cont,DoTag,Value) ->    
%    put(component_type,true), % add more info in component_type
    CtgenMod:gen_encode_prim(Erule,Cont,DoTag,Value).
%    erase(component_type).

make_elements(I,Val,ExtCnames) ->
    make_elements(I,Val,ExtCnames,[]).

make_elements(I,Val,[ExtCname],Acc)-> % the last one, no comma needed
    Element = make_element(I,Val,ExtCname),
    make_elements(I+1,Val,[],[Element|Acc]);
make_elements(I,Val,[ExtCname|Rest],Acc)->
    Element = make_element(I,Val,ExtCname),
    make_elements(I+1,Val,Rest,[", ",Element|Acc]);
make_elements(_I,_,[],Acc) ->
    lists:reverse(Acc).

make_element(I,Val,Cname) ->
    case tuple_notation_allowed() of
	true ->
	    io_lib:format("?RT_PER:cindex(~w,~s,~w)",[I,Val,Cname]);
	_ ->
	    io_lib:format("element(~w,~s)",[I,Val])
    end.

emit_extaddgroupTerms(VarSeries,[_]) ->
    asn1ct_name:new(VarSeries),
    emit({curr,VarSeries}),
    ok;
emit_extaddgroupTerms(VarSeries,[_|Rest]) ->
    asn1ct_name:new(VarSeries),
    emit({{curr,VarSeries},","}),
    emit_extaddgroupTerms(VarSeries,Rest);
emit_extaddgroupTerms(_,[]) ->
    ok.
wrap_compList({Root1,Ext,Root2}) ->
    {Root1,wrap_extensionAdditionGroups(Ext),Root2};
wrap_compList({Root1,Ext}) ->
    {Root1,wrap_extensionAdditionGroups(Ext)};
wrap_compList(CompList) ->
    CompList.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Will convert all componentTypes following 'ExtensionAdditionGroup'
%%  up to the matching 'ExtensionAdditionGroupEnd' into one componentType
%% of type SEQUENCE with the componentTypes as components
%%
wrap_extensionAdditionGroups(ExtCompList) ->
    wrap_extensionAdditionGroups(ExtCompList,[],0).

wrap_extensionAdditionGroups([{'ExtensionAdditionGroup',_Number}|Rest],Acc,0) ->
    {ExtGroupCompList=
     [#'ComponentType'{textual_order=TextPos}|_],
     ['ExtensionAdditionGroupEnd'|Rest2]} =
	lists:splitwith(fun(#'ComponentType'{}) -> true;
			   (_) -> false
			end,
			Rest),
    wrap_extensionAdditionGroups(Rest2,
				 [#'ComponentType'{
                                  name='ExtAddGroup', % FIXME: handles ony one ExtAddGroup
                                  typespec=#type{def=#'SEQUENCE'{
						   extaddgroup=1,% FIXME: handles only one
						   components=ExtGroupCompList}},
                                  textual_order = TextPos,
				  prop='OPTIONAL'}|Acc],length(ExtGroupCompList)-1);
wrap_extensionAdditionGroups([H=#'ComponentType'{textual_order=Tord}|T],Acc,ExtAddGroupDiff) when is_integer(Tord) ->
    wrap_extensionAdditionGroups(T,[H#'ComponentType'{
				      textual_order=Tord - ExtAddGroupDiff}|Acc],ExtAddGroupDiff);
wrap_extensionAdditionGroups([H|T],Acc,ExtAddGroupDiff) ->
    wrap_extensionAdditionGroups(T,[H|Acc],ExtAddGroupDiff);
wrap_extensionAdditionGroups([],Acc,_) ->
    lists:reverse(Acc).


tuple_notation_allowed() ->
    Options = get(encoding_options),
    not (lists:member(optimize,Options) orelse lists:member(uper_bin,Options)).

wrap_gen_dec_line(Erule,C,TopType,Cname,Type,Pos,DIO,Ext) ->
    put(component_type,{true,C}),
    gen_dec_line(Erule,TopType,Cname,Type,Pos,DIO,Ext,mandatory),
    erase(component_type).

get_components_prop() ->
    case get(component_type) of
	undefined ->
	    mandatory;
	{true,#'ComponentType'{prop=Prop}} -> Prop
    end.

			  
value_match(Index,Value) when is_atom(Value) ->
    value_match(Index,atom_to_list(Value));
value_match([],Value) ->
    Value;
value_match([{VI,_}|VIs],Value) ->
    value_match1(Value,VIs,lists:concat(["element(",VI,","]),1).
value_match1(Value,[],Acc,Depth) ->
    Acc ++ Value ++ lists:concat(lists:duplicate(Depth,")"));
value_match1(Value,[{VI,_}|VIs],Acc,Depth) ->
    value_match1(Value,VIs,Acc++lists:concat(["element(",VI,","]),Depth+1).

notice_value_match() ->
    Module = get(currmod),
    put(value_match,{true,Module}).
    
is_optimized(per_bin) ->
    lists:member(optimize,get(encoding_options));
is_optimized(_Erule) ->
    false.
