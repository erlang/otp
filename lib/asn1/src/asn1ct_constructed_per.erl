%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-import(asn1ct_func, [call/3]).

%% ENCODE GENERATOR FOR SEQUENCE TYPE  ** **********


gen_encode_set(Erules,TypeName,D) ->
    gen_encode_constructed(Erules,TypeName,D).

gen_encode_sequence(Erules,TypeName,D) ->
    gen_encode_constructed(Erules,TypeName,D).

gen_encode_constructed(Erule, Typename, #type{}=D) ->
    asn1ct_name:start(),
    Imm = gen_encode_constructed_imm(Erule, Typename, D),
    asn1ct_imm:enc_cg(Imm, is_aligned(Erule)),
    emit([".",nl]).

gen_encode_constructed_imm(Erule, Typename, #type{}=D) ->
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
    ExternalImm =
	case Typename of
	    ['EXTERNAL'] ->
		Next = asn1ct_gen:mk_var(asn1ct_name:next(val)),
		Curr = asn1ct_gen:mk_var(asn1ct_name:curr(val)),
		asn1ct_name:new(val),
		[{call,ext,transform_to_EXTERNAL1990,[{var,Curr}],{var,Next}}];
	    _ ->
		[]
	end,
    Aligned = is_aligned(Erule),
    Value0 = make_var(val),
    Optionals = optionals(to_textual_order(CompList)),
    ImmOptionals = [asn1ct_imm:per_enc_optional(Value0, Opt, Aligned) ||
		       Opt <- Optionals],
    Ext = extensible_enc(CompList),
    ExtImm = case Ext of
		 {ext,ExtPos,NumExt} when NumExt > 0 ->
		     gen_encode_extaddgroup(CompList),
		     Value = make_var(val),
		     asn1ct_imm:per_enc_extensions(Value, ExtPos,
						   NumExt, Aligned);
		 _ ->
		     []
	     end,
    {EncObj,ObjSetImm} =
	case TableConsInfo of
	    #simpletableattributes{usedclassfield=Used,
				   uniqueclassfield=Unique} when Used /= Unique ->
		{false,[]};
	    %% ObjectSet, name of the object set in constraints
	    %% 
	    %%{ObjectSet,AttrN,N,UniqueFieldName} -> %% N is index of attribute that determines constraint
	    #simpletableattributes{objectsetname=ObjectSet,
				   c_name=AttrN,
				   c_index=N,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValueIndex0
				  } -> %% N is index of attribute that determines constraint
		{Module,ObjSetName} = ObjectSet,
		#typedef{typespec=#'ObjectSet'{gen=Gen}} =
		    asn1_db:dbget(Module, ObjSetName),
		case Gen of
		    true ->
			ValueIndex = ValueIndex0 ++ [{N+1,top}],
			Val = make_var(val),
			{ObjSetImm0,Dst} = enc_dig_out_value(ValueIndex, Val),
			{{AttrN,Dst},ObjSetImm0};
		    false ->
			{false,[]}
		end;
	    _  ->
		case D#type.tablecinf of
		    [{objfun,_}|_] ->
			%% when the simpletableattributes was at an outer
			%% level and the objfun has been passed through the
			%% function call
			{{"got objfun through args",{var,"ObjFun"}},[]};
		    _ ->
			{false,[]}
		end
	end,
    ImmSetExt =
	case Ext of
	    {ext,_Pos,NumExt2} when NumExt2 > 0 ->
		asn1ct_imm:per_enc_extension_bit({var,"Extensions"}, Aligned);
	    {ext,_Pos,_} ->
		asn1ct_imm:per_enc_extension_bit([], Aligned);
	    _ ->
		[]
	end,
    ImmBody = gen_enc_components_call(Erule, Typename, CompList, EncObj, Ext),
    ExternalImm ++ ExtImm ++ ObjSetImm ++
	asn1ct_imm:enc_append([ImmSetExt] ++ ImmOptionals ++ ImmBody).

gen_encode_extaddgroup(CompList) ->
    case extgroup_pos_and_length(CompList) of
	{extgrouppos,[]} ->
	    ok;
	{extgrouppos,ExtGroupPosLenList} ->
	    _ = [do_gen_encode_extaddgroup(G) || G <- ExtGroupPosLenList],
	    ok
    end.

do_gen_encode_extaddgroup({ActualGroupPos,GroupVirtualPos,GroupLen}) ->
    Val = asn1ct_gen:mk_var(asn1ct_name:curr(val)),
    Elements = make_elements(GroupVirtualPos+1,
			     Val,
			     lists:seq(1, GroupLen)),
    Expr = any_non_value(GroupVirtualPos+1, Val, GroupLen, ""),
    emit([{next,val}," = case ",Expr," of",nl,
	  "false -> setelement(",{asis,ActualGroupPos+1},", ",
	  {curr,val},", asn1_NOVALUE);",nl,
	  "true -> setelement(",{asis,ActualGroupPos+1},", ",
	  {curr,val},", {extaddgroup,", Elements,"})",nl,
	  "end,",nl]),
    asn1ct_name:new(val).

any_non_value(_, _, 0, _) ->
    [];
any_non_value(Pos, Val, N, Sep) ->
    Sep ++ [make_element(Pos, Val)," =/= asn1_NOVALUE"] ++
	any_non_value(Pos+1, Val, N-1, [" orelse",nl]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate decode function for SEQUENCE and SET
%%
gen_decode_set(Erules,Typename,D) ->
    gen_decode_constructed(Erules,Typename,D).

gen_decode_sequence(Erules,Typename,D) ->
    gen_decode_constructed(Erules,Typename,D).

gen_decode_constructed(Erule, Typename, #type{}=D) ->
    Imm0 = gen_dec_constructed_imm(Erule, Typename, #type{}=D),
    Imm = opt_imm(Imm0),
    asn1ct_name:start(),
    emit_gen_dec_imm(Imm),
    emit([".",nl,nl]).

opt_imm(Imm0) ->
    {Imm,_} = opt_imm_1(Imm0, unknown, []),
    Imm.

opt_imm_1([{imm,Imm0,F}|T], Al0, Acc) ->
    {Imm,Al} = asn1ct_imm:optimize_alignment(Imm0, Al0),
    opt_imm_1(T, Al, [{imm,Imm,F}|Acc]);
opt_imm_1([ignore|T], Al, Acc) ->
    opt_imm_1(T, Al, Acc);
opt_imm_1([{ignore,_}=H|T], Al, Acc) ->
    opt_imm_1(T, Al, [H|Acc]);
opt_imm_1([{safe,ignore}|T], Al, Acc) ->
    opt_imm_1(T, Al, Acc);
opt_imm_1([{safe,_}=H|T], Al, Acc) ->
    opt_imm_1(T, Al, [H|Acc]);
opt_imm_1([{group,G0}|T], Al0, Acc) ->
    {G,Al} = opt_imm_1(G0, Al0, []),
    opt_imm_1(T, Al, [{group,G}|Acc]);
opt_imm_1([Emit|T], _, Acc) when is_function(Emit, 1) ->
    opt_imm_1(T, unknown, [Emit|Acc]);
opt_imm_1([], Al, Acc) ->
    {lists:reverse(Acc),Al}.

emit_gen_dec_imm(L) ->
    emit_gen_dec_imm(L, "", []).

emit_gen_dec_imm([{ignore,Fun}|T], Sep, St0) ->
    St = Fun(St0),
    emit_gen_dec_imm(T, Sep, St);
emit_gen_dec_imm([{group,L}|T], Sep, St0) ->
    emit(Sep),
    St = emit_gen_dec_imm_group(L, St0),
    emit_gen_dec_imm(T, [com,nl], St);
emit_gen_dec_imm([{imm,Imm,Emit}|T], Sep, St0) ->
    emit(Sep),
    St = Emit(Imm, St0),
    emit_gen_dec_imm(T, [com,nl], St);
emit_gen_dec_imm([{safe,Item}|T], Sep, St) ->
    emit_gen_dec_imm([Item|T], Sep, St);
emit_gen_dec_imm([Emit|T], Sep, St0) ->
    emit(Sep),
    St = Emit(St0),
    emit_gen_dec_imm(T, [com,nl], St);
emit_gen_dec_imm([], _, _) -> ok.

emit_gen_dec_imm_group([H|T], St0) ->
    St = emit_gen_dec_group_item(H, St0),
    emit_gen_dec_imm_group(T, St);
emit_gen_dec_imm_group([], St) -> St.

emit_gen_dec_group_item({ignore,Fun}, St) ->
    Fun(St);
emit_gen_dec_group_item({imm,Imm,Fun}, St) ->
    Fun(Imm, St);
emit_gen_dec_group_item({safe,Item}, St) ->
    emit_gen_dec_group_item(Item, St);
emit_gen_dec_group_item(Emit, St) ->
    Emit(St).

gen_dec_constructed_imm(Erule, Typename, #type{}=D) ->
    {CompList,TableConsInfo} = 
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} ->
		{add_textual_order(CL),TCI};
	    #'SET'{tablecinf=TCI,components=CL} ->
%%		{add_textual_order(CL),TCI}
		{CL,TCI} % the textual order is already taken care of
	end,
    Ext = extensible_dec(CompList),
    EmitExt = case Ext of
		  {ext,_Pos,_NumExt} ->
		      gen_dec_extension_value();
		  _ -> ignore
	      end,
    Optionals = optionals(CompList),
    EmitOpt = case Optionals of
		  [] ->
		      ignore;
		  [_|_] ->
		      gen_dec_optionals(Optionals)
	      end,
    ObjSetInfo =
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
		case lists:any(F,flat_complist(CompList)) of
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
    {DecObjInf,_,_} = ObjSetInfo,
    EmitComp = gen_dec_components_call(Erule, Typename, CompList,
				       DecObjInf, Ext, length(Optionals)),
    EmitRest = fun({AccTerm,AccBytes}) ->
		       gen_dec_constructed_imm_2(Erule, Typename,
						 CompList,
						 ObjSetInfo,
						 AccTerm, AccBytes)
	       end,
    [EmitExt,EmitOpt|EmitComp++[{safe,EmitRest}]].

gen_dec_constructed_imm_2(Erule, Typename, CompList,
			  ObjSetInfo, AccTerm, AccBytes) ->
    {_,_UniqueFName,ValueIndex} = ObjSetInfo,
    case {AccTerm,AccBytes} of
	{[],[]} ->
	    ok;
	{_,[]} ->
	    ok;
	{[{ObjSet,LeadingAttr,Term}],ListOfOpenTypes} ->
	    ValueMatch = value_match(ValueIndex, Term),
	    _ = [begin
		     gen_dec_open_type(Erule, ValueMatch, ObjSet,
				       LeadingAttr, T),
		     emit([com,nl])
		 end || T <- ListOfOpenTypes],
	    ok
    end,
    %% we don't return named lists any more   Cnames = mkcnamelist(CompList), 
    demit({"Result = "}), %dbg
    %% return value as record
    RecordName = record_name(Typename),
    case Typename of
	['EXTERNAL'] ->
	    emit({"   OldFormat={'",RecordName,
		  "'"}),
	    mkvlist(asn1ct_name:all(term)),
	    emit({"},",nl}),
	    emit(["   ASN11994Format =",nl,
		  "      ",
		  {call,ext,transform_to_EXTERNAL1994,
		   ["OldFormat"]},com,nl]),
	    emit("   {ASN11994Format,");
	_ ->
	    emit(["{{'",RecordName,"'"]),
	    %% CompList is used here because we don't want
	    %% ExtensionAdditionGroups to be wrapped in SEQUENCES when
	    %% we are ordering the fields according to textual order
	    mkvlist(textual_order(to_encoding_order(CompList),asn1ct_name:all(term))),
	    emit("},")
    end,
    emit({{curr,bytes},"}"}).

%% record_name([TypeName]) -> RecordNameString
%%  Construct a record name for the constructed type, ignoring any
%%  fake sequences that are used to represent an extension addition
%%  group. Such fake sequences never appear as a top type, and their
%%  name always start with "ExtAddGroup".

record_name(Typename0) ->
    [TopType|Typename1] = lists:reverse(Typename0),
    Typename = filter_ext_add_groups(Typename1, [TopType]),
    lists:concat([get_record_name_prefix(),
		  asn1ct_gen:list2rname(Typename)]).

filter_ext_add_groups([H|T], Acc) when is_atom(H) ->
    case atom_to_list(H) of
	"ExtAddGroup"++_ ->
	    filter_ext_add_groups(T, Acc);
	_ ->
	    filter_ext_add_groups(T, [H|Acc])
    end;
filter_ext_add_groups([H|T], Acc) ->
    filter_ext_add_groups(T, [H|Acc]);
filter_ext_add_groups([], Acc) -> Acc.

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

gen_dec_open_type(Erule, Val, {Xmod,Xtype}, LeadingAttr,
		  {_,{Name,RestFieldNames},Term,TmpTerm,Prop}) ->
    #typedef{typespec=ObjSet0} = asn1_db:dbget(Xmod, Xtype),
    #'ObjectSet'{class=Class,set=ObjSet1} = ObjSet0,
    #'Externaltypereference'{module=ClMod,type=ClType} = Class,
    #classdef{typespec=ClassDef} = asn1_db:dbget(ClMod, ClType),
    #objectclass{fields=ClassFields} = ClassDef,
    Extensible = lists:member('EXTENSIONMARK', ObjSet1),
    Typename = [Name,ClType],
    ObjSet = index_object_set(Erule, ClType, Name,
			      ObjSet1, ClassFields),
    Key = erlang:md5(term_to_binary({decode,ObjSet,RestFieldNames,
				     Prop,Extensible})),
    Gen = fun(_Fd, N) ->
		  dec_objset_optional(N, Prop),
		  dec_objset(Erule, N, ObjSet, RestFieldNames, Typename),
		  dec_objset_default(N, Name, LeadingAttr, Extensible)
	  end,
    Prefix = lists:concat(["dec_os_",Name]),
    F = asn1ct_func:call_gen(Prefix, Key, Gen),
    emit([Term," = ",{asis,F},"(",TmpTerm,", ",Val,")"]).

dec_objset_optional(N, {'DEFAULT',Val}) ->
    dec_objset_optional_1(N, Val);
dec_objset_optional(N, 'OPTIONAL') ->
    dec_objset_optional_1(N, asn1_NOVALUE);
dec_objset_optional(_N, mandatory) -> ok.

dec_objset_optional_1(N, Val) ->
    emit([{asis,N},"(",{asis,Val},", _Id) ->",nl,
	  {asis,Val},";",nl]).

dec_objset(_Erule, _N, [], _, _) ->
    ok;
dec_objset(Erule, N, [Obj|Objs], RestFields, Cl) ->
    dec_objset_1(Erule, N, Obj, RestFields, Cl),
    emit([";",nl]),
    dec_objset(Erule, N, Objs, RestFields, Cl).

dec_objset_default(N, C, LeadingAttr, false) ->
    emit([{asis,N},"(Bytes, Id) ->",nl,
	  "exit({'Type not compatible with table constraint',"
	  "{{component,",{asis,C},"},"
	  "{value,Bytes},"
	  "{unique_name_and_value,",{asis,LeadingAttr},",Id}}}).",nl,nl]);
dec_objset_default(N, _, _, true) ->
    emit([{asis,N},"(Bytes, Id) ->",nl|
	  case asn1ct:use_legacy_types() of
	      false ->
		  ["{asn1_OPENTYPE,Bytes}.",nl,nl];
	      true ->
		  ["Bytes.",nl,nl]
	  end]).

dec_objset_1(Erule, N, {Id,Obj}, RestFields, Typename) ->
    emit([{asis,N},"(Bytes, ",{asis,Id},") ->",nl]),
    dec_objset_2(Erule, Obj, RestFields, Typename).

dec_objset_2(Erule, Obj, RestFields0, Typename) ->
    case Obj of
	#typedef{name={primitive,bif},typespec=Type} ->
	    Imm = asn1ct_gen_per:gen_dec_imm(Erule, Type),
	    {Term,_} = asn1ct_imm:dec_slim_cg(Imm, 'Bytes'),
	    emit([com,nl,Term]);
	#typedef{name={constructed,bif},typespec=Type}=Def ->
	    Prefix = "dec_outlined_",
	    Key = {dec_outlined,Def},
	    Gen = fun(_Fd, Name) ->
			  gen_dec_obj(Erule, Name, Typename, Type)
		  end,
	    Func = asn1ct_func:call_gen(Prefix, Key, Gen),
	    emit(["{Term,_} = ",{asis,Func},"(Bytes)",com,nl,
		  "Term"]);
	#typedef{name=Type} ->
	    emit(["{Result,_} = ",{asis,enc_func("dec_", Type)},"(Bytes),",nl,
		  "Result"]);
	#'Externaltypereference'{module=Mod,type=Type} ->
	    emit("{Term,_} = "),
	    Func = enc_func("dec_", Type),
	    case get(currmod) of
		Mod ->
		    emit([{asis,Func},"(Bytes)"]);
		_ ->
		    emit([{asis,Mod},":",{asis,Func},"(Bytes)"])
	    end,
	    emit([com,nl,
		  "Term"]);
	#'Externalvaluereference'{module=Mod,value=Value} ->
	    case asn1_db:dbget(Mod, Value) of
		#typedef{typespec=#'Object'{def=Def}} ->
		    {object,_,Fields} = Def,
		    [NextField|RestFields] = RestFields0,
		    {NextField,Typedef} = lists:keyfind(NextField, 1, Fields),
		    dec_objset_2(Erule, Typedef, RestFields, Typename)
	    end
    end.

gen_dec_obj(Erules, Name, Typename, Type) ->
    emit([{asis,Name},"(Bytes) ->",nl]),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    asn1ct_gen:gen_decode_constructed(Erules, Typename,
				      InnerType, Type).

gen_encode_choice(Erule, TopType, D) ->
    asn1ct_name:start(),
    Imm = gen_encode_choice_imm(Erule, TopType, D),
    asn1ct_imm:enc_cg(Imm, is_aligned(Erule)),
    emit([".",nl]).

gen_encode_choice_imm(Erule, TopType, #type{def={'CHOICE',CompList}}) ->
    Ext = extensible_enc(CompList),
    Aligned = is_aligned(Erule),
    Cs = gen_enc_choice(Erule, TopType, CompList, Ext),
    [{assign,{expr,"{ChoiceTag,ChoiceVal}"},"Val"}|
     asn1ct_imm:per_enc_choice({var,"ChoiceTag"}, Cs, Aligned)].

gen_decode_choice(Erules,Typename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible_enc(CompList),
    gen_dec_choice(Erules,Typename,CompList,Ext),
    emit({".",nl}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Encode generator for SEQUENCE OF type

gen_encode_sof(Erule, Typename, SeqOrSetOf, D) ->
    asn1ct_name:start(),
    Imm = gen_encode_sof_imm(Erule, Typename, SeqOrSetOf, D),
    asn1ct_imm:enc_cg(Imm, is_aligned(Erule)),
    emit([".",nl,nl]).

gen_encode_sof_imm(Erule, Typename, SeqOrSetOf, #type{}=D) ->
    {_SeqOrSetOf,ComponentType} = D#type.def,
    Aligned = is_aligned(Erule),
    CompType = ComponentType#type.def,
    Constructed_Suffix = asn1ct_gen:constructed_suffix(SeqOrSetOf, CompType),
    Conttype = asn1ct_gen:get_inner(CompType),
    Currmod = get(currmod),
    Imm0 = case asn1ct_gen:type(Conttype) of
	       {primitive,bif} ->
		   asn1ct_gen_per:gen_encode_prim_imm({var,"Comp"},
						      ComponentType, Aligned);
	       {constructed,bif} ->
		   TypeName = [Constructed_Suffix|Typename],
		   Enc = enc_func(asn1ct_gen:list2name(TypeName)),
		   ObjArg = case D#type.tablecinf of
				[{objfun,_}|_] -> [{var,"ObjFun"}];
				_ -> []
			    end,
		   [{apply,{local,Enc,CompType},
		     [{var,"Comp"}|ObjArg]}];
	       #'Externaltypereference'{module=Currmod,type=Ename} ->
		   [{apply,{local,enc_func(Ename),CompType},[{var,"Comp"}]}];
	       #'Externaltypereference'{module=EMod,type=Ename} ->
		   [{apply,{EMod,enc_func(Ename),CompType},[{var,"Comp"}]}];
	       'ASN1_OPEN_TYPE' ->
		   asn1ct_gen_per:gen_encode_prim_imm({var,"Comp"},
						      #type{def='ASN1_OPEN_TYPE'},
						      Aligned)
	   end,
    asn1ct_imm:per_enc_sof({var,"Val"}, D#type.constraint, 'Comp',
			   Imm0, Aligned).

gen_decode_sof(Erules, Typename, SeqOrSetOf, #type{}=D) ->
    asn1ct_name:start(),
    do_gen_decode_sof(Erules, Typename, SeqOrSetOf, D),
    emit([".",nl,nl]).

do_gen_decode_sof(Erules, Typename, SeqOrSetOf, D) ->
    {_SeqOrSetOf,ComponentType} = D#type.def,
    SizeConstraint = asn1ct_imm:effective_constraint(bitstring,
						     D#type.constraint),
    ObjFun =
	case D#type.tablecinf of
	    [{objfun,_}|_R] ->
		", ObjFun";
	    _ ->
		""
	end,
    {Num,Buf} = gen_decode_length(SizeConstraint, Erules),
    Key = erlang:md5(term_to_binary({Typename,SeqOrSetOf,ComponentType})),
    Gen = fun(_Fd, Name) ->
		  gen_decode_sof_components(Erules, Name,
					    Typename, SeqOrSetOf,
					    ComponentType)
	  end,
    F = asn1ct_func:call_gen("dec_components", Key, Gen),
    emit([",",nl,
	  {asis,F},"(",Num,", ",Buf,ObjFun,", [])"]).

is_aligned(per) -> true;
is_aligned(uper) -> false.

gen_decode_length(Constraint, Erule) ->
    emit(["%% Length with constraint ",{asis,Constraint},nl]),
    Imm = asn1ct_imm:per_dec_length(Constraint, true, is_aligned(Erule)),
    asn1ct_imm:dec_slim_cg(Imm, "Bytes").

gen_decode_sof_components(Erule, Name, Typename, SeqOrSetOf, Cont) ->
    {ObjFun,ObjFun_Var} =
	case Cont#type.tablecinf of
	    [{objfun,_}|_R] ->
		{", ObjFun",", _"};
	    _ ->
		{"",""}
	end,
    emit([{asis,Name},"(0, Bytes",ObjFun_Var,", Acc) ->",nl,
	  "{lists:reverse(Acc),Bytes};",nl]),
    emit([{asis,Name},"(Num, Bytes",ObjFun,", Acc) ->",nl,
	  "{Term,Remain} = "]),
    Constructed_Suffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,
						       Cont#type.def),
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    asn1ct_gen_per:gen_dec_prim(Erule, Cont, "Bytes"),
	    emit({com,nl});
	{constructed,bif} ->
	    NewTypename = [Constructed_Suffix|Typename],
	    emit({"'dec_",asn1ct_gen:list2name(NewTypename),
		  "'(Bytes",ObjFun,"),",nl});
	#'Externaltypereference'{}=Etype ->
	    asn1ct_gen_per:gen_dec_external(Etype, "Bytes"),
	    emit([com,nl]);
	'ASN1_OPEN_TYPE' ->
	    asn1ct_gen_per:gen_dec_prim(Erule, #type{def='ASN1_OPEN_TYPE'},
					"Bytes"),
	    emit({com,nl});
	_ ->
	    emit({"'dec_",Conttype,"'(Bytes),",nl})
    end,
    emit([{asis,Name},"(Num-1, Remain",ObjFun,", [Term|Acc]).",nl]).


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
    {extgrouppos,[]};
extgroup_pos_and_length({RootList,ExtList}) ->
    ActualPos = length(RootList) +1,
    %% position to get and deliver data in the record to the user
    VirtualPos = ActualPos,
    %% position to encode/decode the extaddgroup as an opentype sequence
    extgrouppos(ExtList,ActualPos,VirtualPos,[]);
extgroup_pos_and_length({RootList,ExtList,_Rl2}) ->
    extgroup_pos_and_length({RootList,ExtList}).

extgrouppos([{'ExtensionAdditionGroup',_Num}|T],ActualPos,VirtualPos,Acc) ->
    extgrouppos(T,ActualPos,VirtualPos,0,Acc);
extgrouppos([_|T],ActualPos,VirtualPos,Acc) ->
    extgrouppos(T,ActualPos+1,VirtualPos+1,Acc);
extgrouppos([],_,_,Acc) ->
    {extgrouppos,lists:reverse(Acc)}.

extgrouppos(['ExtensionAdditionGroupEnd'|T],ActualPos,VirtualPos,Len,Acc) ->
    extgrouppos(T,ActualPos+1,VirtualPos+Len,[{ActualPos,VirtualPos,Len}|Acc]);
extgrouppos([_|T],ActualPos,VirtualPos,Len,Acc) ->
    extgrouppos(T,ActualPos,VirtualPos,Len+1,Acc).


gen_dec_extension_value() ->
    Imm0 = {get_bits,1,[1]},
    E = fun(Imm, _) ->
		emit(["{Ext,",{next,bytes},"} = "]),
		BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		asn1ct_imm:dec_code_gen(Imm, BytesVar),
		asn1ct_name:new(bytes)
	end,
    {imm,Imm0,E}.

gen_dec_optionals(Optionals) ->
    Imm0 = {get_bits,length(Optionals),[1]},
    E = fun(Imm, _) ->
		BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		emit(["{Opt,",{next,bytes},"} = "]),
		asn1ct_imm:dec_code_gen(Imm, BytesVar),
		asn1ct_name:new(bytes)
    end,
    {imm,Imm0,E}.

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

optionals([#'ComponentType'{prop='OPTIONAL'}|Rest], Acc, Pos) ->
    optionals(Rest, [Pos|Acc], Pos+1);
optionals([#'ComponentType'{typespec=T,prop={'DEFAULT',Val}}|Rest],
	  Acc, Pos) ->
    Vals = def_values(T, Val),
    optionals(Rest, [{Pos,Vals}|Acc], Pos+1);
optionals([#'ComponentType'{}|Rest], Acc, Pos) ->
    optionals(Rest, Acc, Pos+1);
optionals([], Acc, _) ->
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

gen_enc_components_call(Erule,TopType,{Root,ExtList}, DynamicEnc,Ext) ->
    gen_enc_components_call(Erule,TopType,{Root,ExtList,[]}, DynamicEnc,Ext);
gen_enc_components_call(Erule,TopType,CL={Root,ExtList,Root2}, DynamicEnc,Ext) ->
    %% The type has extensionmarker
    {Imm0,Rpos} = gen_enc_components_call1(Erule,TopType,Root++Root2,1, DynamicEnc,noext,[]),
    ExtImm = case Ext of
		 {ext,_,ExtNum} when ExtNum > 0 ->
		     [{var,"Extensions"}];
		 _ ->
		     []
	     end,
    %handle extensions
    {extgrouppos,ExtGroupPosLen}  = extgroup_pos_and_length(CL),
    NewExtList = wrap_extensionAdditionGroups(ExtList,ExtGroupPosLen),
    {Imm1,_} = gen_enc_components_call1(Erule,TopType,NewExtList,Rpos,DynamicEnc,Ext,[]),
    Imm0 ++ [ExtImm|Imm1];
gen_enc_components_call(Erule,TopType, CompList, DynamicEnc, Ext) ->
    %% The type has no extensionmarker
    {Imm,_} = gen_enc_components_call1(Erule,TopType,CompList,1,DynamicEnc,Ext,[]),
    Imm.

gen_enc_components_call1(Erule,TopType,
			 [C=#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],
			 Tpos,
			 DynamicEnc, Ext, Acc) ->

    TermNo =
	case C#'ComponentType'.textual_order of
	    undefined ->
		Tpos;
	    CanonicalNum ->
		CanonicalNum
	end,
    Val = make_var(val),
    {Imm0,Element} = asn1ct_imm:enc_element(TermNo+1, Val),
    Imm1 = gen_enc_line_imm(Erule, TopType, Cname, Type, Element, DynamicEnc, Ext),
    Category = case {Prop,Ext} of
		   {'OPTIONAL',_} ->
		       optional;
		   {{'DEFAULT',DefVal},_} ->
		       {default,DefVal};
		   {_,{ext,ExtPos,_}} when Tpos >= ExtPos ->
		       optional;
		   {_,_} ->
		       mandatory
	       end,
    Imm2 = case Category of
	       mandatory ->
		   Imm1;
	       optional ->
		   asn1ct_imm:enc_absent(Element, [asn1_NOVALUE], Imm1);
	       {default,Def} ->
		   DefValues = def_values(Type, Def),
		   asn1ct_imm:enc_absent(Element, DefValues, Imm1)
	   end,
    Imm = case Imm2 of
	      [] -> [];
	      _ -> Imm0 ++ Imm2
	  end,
    gen_enc_components_call1(Erule, TopType, Rest, Tpos+1, DynamicEnc, Ext, [Imm|Acc]);
gen_enc_components_call1(_Erule,_TopType,[],Pos,_,_, Acc) ->
    ImmList = lists:reverse(Acc),
    {ImmList,Pos}.

def_values(#type{def=#'Externaltypereference'{module=Mod,type=Type}}, Def) ->
    #typedef{typespec=T} = asn1_db:dbget(Mod, Type),
    def_values(T, Def);
def_values(#type{def={'BIT STRING',[]}}, Bs) when is_bitstring(Bs) ->
    case asn1ct:use_legacy_types() of
	false ->
	    [asn1_DEFAULT,Bs];
	true ->
	    ListBs = [B || <<B:1>> <= Bs],
	    IntBs = lists:foldl(fun(B, A) ->
					(A bsl 1) bor B
				end, 0, lists:reverse(ListBs)),
	    Sz = bit_size(Bs),
	    Compact = case 8 - Sz rem 8 of
			  8 ->
			      {0,Bs};
			  Unused ->
			      {Unused,<<Bs:Sz/bits,0:Unused>>}
		      end,
	    [asn1_DEFAULT,Bs,Compact,ListBs,IntBs]
    end;
def_values(#type{def={'BIT STRING',[_|_]=Ns}}, List) when is_list(List) ->
    Bs = asn1ct_gen:named_bitstring_value(List, Ns),
    As = case asn1ct:use_legacy_types() of
	     false ->
		 [List,Bs];
	     true ->
		 ListBs = [B || <<B:1>> <= Bs],
		 IntBs = lists:foldl(fun(B, A) ->
					     (A bsl 1) bor B
				     end, 0, lists:reverse(ListBs)),
		 [List,Bs,ListBs,IntBs]
	 end,
    {call,per_common,is_default_bitstring,As};
def_values(#type{def={'INTEGER',Ns}}, Def) ->
    [asn1_DEFAULT,Def|case lists:keyfind(Def, 2, Ns) of
			  false -> [];
			  {Val,Def} -> [Val]
		      end];
def_values(_, Def) ->
    [asn1_DEFAULT,Def].

gen_enc_line_imm(Erule, TopType, Cname, Type, Element, DynamicEnc, Ext) ->
    Imm0 = gen_enc_line_imm_1(Erule, TopType, Cname, Type,
			      Element, DynamicEnc),
    Aligned = is_aligned(Erule),
    case Ext of
	{ext,_Ep2,_} ->
	    asn1ct_imm:per_enc_open_type(Imm0, Aligned);
	_ ->
	    Imm0
    end.

gen_enc_line_imm_1(Erule, TopType, Cname, Type, Element, DynamicEnc) ->
    Atype = 
	case Type of
	    #type{def=#'ObjectClassFieldType'{type=InnerType}} ->
		InnerType;  
	    _  ->
		asn1ct_gen:get_inner(Type#type.def)
	end,
    Aligned = is_aligned(Erule),
    case Atype of
	{typefield,_} ->
	    {_LeadingAttrName,Fun} = DynamicEnc,
	    case (Type#type.def)#'ObjectClassFieldType'.fieldname of
		{Name,RestFieldNames} when is_atom(Name) ->
		    Imm = enc_var_type_call(Erule, Name, RestFieldNames,
					    Type, Fun, Element),
		    asn1ct_imm:per_enc_open_type(Imm, Aligned)
	    end;
	_ ->
	    CurrMod = get(currmod),
	    case asn1ct_gen:type(Atype) of
		#'Externaltypereference'{module=CurrMod,type=EType} ->
		    [{apply,{local,enc_func(EType),Atype},[Element]}];
		#'Externaltypereference'{module=Mod,type=EType} ->
		    [{apply,{Mod,enc_func(EType),Atype},[Element]}];
		{primitive,bif} ->
		    asn1ct_gen_per:gen_encode_prim_imm(Element, Type, Aligned);
		'ASN1_OPEN_TYPE' ->
		    case Type#type.def of
			#'ObjectClassFieldType'{type=OpenType} ->
			    asn1ct_gen_per:gen_encode_prim_imm(Element,
							       #type{def=OpenType},
							       Aligned);
			_ ->
			    asn1ct_gen_per:gen_encode_prim_imm(Element,
							       Type,
							       Aligned)
		    end;
		{constructed,bif} ->
		    NewTypename = [Cname|TopType],
		    Enc = enc_func(asn1ct_gen:list2name(NewTypename)),
		    case {Type#type.tablecinf,DynamicEnc} of
			{[{objfun,_}|_R],{_,EncFun}} ->
			    [{apply,{local,Enc,Type},[Element,EncFun]}];
			_ ->
			    [{apply,{local,Enc,Type},[Element]}]
		    end
	    end
    end.

enc_func(Type) ->
    enc_func("enc_", Type).

enc_func(Prefix, Name) ->
    list_to_atom(lists:concat([Prefix,Name])).

enc_var_type_call(Erule, Name, RestFieldNames,
		  #type{tablecinf=TCI}, Fun, Val) ->
    [{objfun,#'Externaltypereference'{module=Xmod,type=Xtype}}] = TCI,
    #typedef{typespec=ObjSet0} = asn1_db:dbget(Xmod, Xtype),
    #'ObjectSet'{class=Class,set=ObjSet1} = ObjSet0,
    #'Externaltypereference'{module=ClMod,type=ClType} = Class,
    #classdef{typespec=ClassDef} = asn1_db:dbget(ClMod, ClType),
    #objectclass{fields=ClassFields} = ClassDef,
    Extensible = lists:member('EXTENSIONMARK', ObjSet1),
    ObjSet = index_object_set(Erule, ClType, Name,
			      ObjSet1, ClassFields),
    Key = erlang:md5(term_to_binary({encode,ObjSet,RestFieldNames,Extensible})),
    TypeName = [ClType,Name],
    Imm = enc_objset_imm(Erule, TypeName, Name, ObjSet,
			 RestFieldNames, Extensible),
    Lambda = {lambda,[{var,"Val"},{var,"Id"}],Imm},
    Gen = fun(_Fd, N) ->
		  Aligned = is_aligned(Erule),
		  emit([{asis,N},"(Val, Id) ->",nl]),
		  asn1ct_imm:enc_cg(Imm, Aligned),
		  emit([".",nl])
	  end,
    Prefix = lists:concat(["enc_os_",Name]),
    [{call_gen,Prefix,Key,Gen,Lambda,[Val,Fun]}].

index_object_set(_Erules, _ClType, Name, Set0, ClassFields) ->
    Set = index_object_set_1(Name, Set0, ClassFields),
    lists:sort(Set).

index_object_set_1(Name, [{_,Key,Code}|T], ClassFields) ->
    case index_object_set_2(Name, Code, ClassFields) of
	none ->
	    index_object_set_1(Name, T, ClassFields);
	Type ->
	    [{Key,Type}|index_object_set_1(Name, T, ClassFields)]
    end;
index_object_set_1(Name, [_|T], ClassFields) ->
    index_object_set_1(Name, T, ClassFields);
index_object_set_1(_, [], _) ->
    [].

index_object_set_2(Name, [{Name,Type}|_], _ClassFields) ->
    Type;
index_object_set_2(Name, [_|T], ClassFields) ->
    index_object_set_2(Name, T, ClassFields);
index_object_set_2(Name, [], ClassFields) ->
    case lists:keyfind(Name, 2, ClassFields) of
	{typefield,Name,'OPTIONAL'} ->
	    none;
	{objectfield,Name,_,_,'OPTIONAL'} ->
	    none;
	{typefield,Name,{'DEFAULT',#type{}=Type}} ->
	    InnerType = asn1ct_gen:get_inner(Type#type.def),
	    case asn1ct_gen:type(InnerType) of
		{primitive,bif} ->
		    #typedef{name={primitive,bif},typespec=Type};
		{constructed,bif} ->
		    #typedef{name={constructed,bif},typespec=Type}
	    end
    end.

enc_objset_imm(Erule, TypeName, Component, ObjSet,
	       RestFieldNames, Extensible) ->
    Aligned = is_aligned(Erule),
    E = {error,
	 fun() ->
		 emit(["exit({'Type not compatible with table constraint',"
		       "{component,",{asis,Component},"},"
		       "{value,Val},"
		       "{unique_name_and_value,'_'}})",nl])
	 end},
    [{'cond',
      [[{eq,{var,"Id"},Key}|
	enc_obj(Erule, Obj, TypeName, RestFieldNames, Aligned)] ||
	  {Key,Obj} <- ObjSet] ++
	  [['_',case Extensible of
		    false ->
			E;
		    true ->
			case asn1ct:use_legacy_types() of
			    false ->
				{call,per_common,open_type_to_binary,
				 [{var,"Val"}]};
			    true ->
				{call,per_common,legacy_open_type_to_binary,
				 [{var,"Val"}]}
			end
		end]]}].

enc_obj(Erule, Obj, TypeName, RestFieldNames0, Aligned) ->
    Val = {var,"Val"},
    case Obj of
	#typedef{name={constructed,bif},typespec=Type}=Def ->
	    Prefix = "enc_outlined_",
	    Key = {enc_outlined,Def},
	    Gen = fun(_Fd, Name) ->
			  gen_enc_obj(Erule, Name, TypeName, Type)
		  end,
	    [{call_gen,Prefix,Key,Gen,undefined,[Val]}];
	#typedef{name={primitive,bif},typespec=Def} ->
	    asn1ct_gen_per:gen_encode_prim_imm({var,"Val"}, Def, Aligned);
	#typedef{name=Type} ->
	    [{apply,{local,enc_func(Type),Type},[{var,"Val"}]}];
	#'Externalvaluereference'{module=Mod,value=Value} ->
	    case asn1_db:dbget(Mod, Value) of
		#typedef{typespec=#'Object'{def=Def}} ->
		    {object,_,Fields} = Def,
		    [NextField|RestFieldNames] = RestFieldNames0,
		    {NextField,Typedef} = lists:keyfind(NextField, 1, Fields),
		    enc_obj(Erule, Typedef, TypeName,
			    RestFieldNames, Aligned)
	    end;
	#'Externaltypereference'{module=Mod,type=Type} ->
	    Func = enc_func(Type),
	    case get(currmod) of
		Mod ->
		    [{apply,{local,Func,Obj},[{var,"Val"}]}];
		_ ->
		    [{apply,{Mod,Func,Obj},[{var,"Val"}]}]
	    end
    end.

gen_enc_obj(Erules, Name, Typename, Type) ->
    emit([{asis,Name},"(Val) ->",nl]),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    asn1ct_gen:gen_encode_constructed(Erules, Typename,
				      InnerType, Type).

gen_dec_components_call(Erule, TopType, {Root,ExtList},
			DecInfObj, Ext, NumberOfOptionals) ->
    gen_dec_components_call(Erule,TopType,{Root,ExtList,[]},
			    DecInfObj,Ext,NumberOfOptionals);
gen_dec_components_call(Erule,TopType,CL={Root1,ExtList,Root2},
			DecInfObj,Ext,NumberOfOptionals) ->
    %% The type has extensionmarker
    OptTable = create_optionality_table(Root1++Root2),
    Init = {ignore,fun(_) -> {[],[]} end},
    {EmitRoot,Tpos} =
	gen_dec_comp_calls(Root1++Root2, Erule, TopType, OptTable,
			   DecInfObj, noext, NumberOfOptionals,
			   1, []),
    EmitGetExt = gen_dec_get_extension(Erule),
    {extgrouppos,ExtGroupPosLen}  = extgroup_pos_and_length(CL),
    NewExtList = wrap_extensionAdditionGroups(ExtList, ExtGroupPosLen),
    {EmitExts,_} = gen_dec_comp_calls(NewExtList, Erule, TopType, OptTable,
				      DecInfObj, Ext, NumberOfOptionals,
				      Tpos, []),
    NumExtsToSkip = ext_length(ExtList),
    Finish =
	fun(St) ->
		emit([{next,bytes},"= "]),
		call(Erule, skipextensions,
		     [{curr,bytes},NumExtsToSkip+1,"Extensions"]),
		asn1ct_name:new(bytes),
		St
	end,
    [Init] ++ EmitRoot ++ [EmitGetExt|EmitExts] ++ [Finish];
gen_dec_components_call(Erule, TopType, CompList, DecInfObj,
			Ext, NumberOfOptionals) ->
    %% The type has no extensionmarker
    OptTable = create_optionality_table(CompList),
    Init = {ignore,fun(_) -> {[],[]} end},
    {Cs,_} = gen_dec_comp_calls(CompList, Erule, TopType, OptTable,
				DecInfObj, Ext, NumberOfOptionals,
				1, []),
    [Init|Cs].

gen_dec_get_extension(Erule) ->
    Imm0 = asn1ct_imm:per_dec_extension_map(is_aligned(Erule)),
    E = fun(Imm, St) ->
		emit([nl,"%% Extensions",
		      nl,
		      "{Extensions,",{next,bytes},"} = ",
		      "case Ext of",nl,
		      "0 -> {<<>>,",{curr,bytes},"};",nl,
		      "1 ->",nl]),
		BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		{Dst,DstBuf} = asn1ct_imm:dec_slim_cg(Imm, BytesVar),
		emit([com,nl,
		      "{",Dst,",",DstBuf,"}",nl,
		      "end"]),
		asn1ct_name:new(bytes),
		St
	end,
    {imm,Imm0,E}.

gen_dec_comp_calls([C|Cs], Erule, TopType, OptTable, DecInfObj,
		   Ext, NumberOfOptionals, Tpos, Acc) ->
    L = gen_dec_comp_call(C, Erule, TopType, Tpos, OptTable, DecInfObj,
			  Ext, NumberOfOptionals),
    gen_dec_comp_calls(Cs, Erule, TopType, OptTable, DecInfObj,
		       Ext, NumberOfOptionals, Tpos+1, [L|Acc]);
gen_dec_comp_calls([], _, _, _, _, _, _, Tpos, Acc) ->
    {lists:append(lists:reverse(Acc)),Tpos}.

gen_dec_comp_call(Comp, Erule, TopType, Tpos, OptTable, DecInfObj,
		  Ext, NumberOfOptionals) ->
    #'ComponentType'{typespec=Type,prop=Prop,textual_order=TextPos} = Comp,
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,_Enum} -> Tpos - Epos + 1
	  end,
    InnerType = 
	case Type#type.def of
	    #'ObjectClassFieldType'{type=InType} ->
		InType;
	    Def ->
		asn1ct_gen:get_inner(Def)
	end,

    DispType = case InnerType of
		   #'Externaltypereference'{type=T} -> T;
		   IT when is_tuple(IT) -> element(2,IT);
		   _ -> InnerType
	       end,
    Comment = fun(St) ->
		      emit([nl,"%% attribute number ",TextPos,
			    " with type ",DispType,nl]),
		      St
	      end,

    Preamble =
	case {InnerType,is_mandatory_predef_tab_c(Ext, Prop, DecInfObj)} of
	    {{typefield,_},true}  ->
		%% DecInfObj /= {"got objfun through args","ObjFun"} |
		%% (DecInfObj == {"got objfun through args","ObjFun"} &
		%% Ext == noext & Prop == mandatory)
		fun(St) ->
			asn1ct_name:new(term),
			asn1ct_name:new(tmpterm),
			emit(["{",{curr,tmpterm},", ",{next,bytes},"} = "]),
			St
		end;
	_ ->
	    case Type of
		#type{def=#'SEQUENCE'{
			extaddgroup=Number1,
			components=ExtGroupCompList1}} when is_integer(Number1)->
		    fun(St) ->
			    emit(["{{_,"]),
			    emit_extaddgroupTerms(term,ExtGroupCompList1),
			    emit(["}"]),
			    emit([",",{next,bytes},"} = "]),
			    St
		    end;
		_ ->
		    fun(St) ->
			    asn1ct_name:new(term),
			    emit(["{",{curr,term}]),
			    emit([",",{next,bytes},"} = "]),
			    St
		    end
	    end
	end,
    {Pre,Post} = comp_call_pre_post(Ext, Prop, Pos, Type, TextPos,
				    OptTable, NumberOfOptionals, Ext),
    Lines = gen_dec_seq_line_imm(Erule, TopType, Comp, Tpos, DecInfObj, Ext),
    AdvBuffer = {ignore,fun(St) ->
				asn1ct_name:new(bytes),
				St
			end},
    [{group,[{safe,Comment},{safe,Preamble}] ++ Pre ++
	  Lines ++ Post ++ [{safe,AdvBuffer}]}].

comp_call_pre_post(noext, mandatory, _, _, _, _, _, _) ->
    {[],[]};
comp_call_pre_post(noext, Prop, _, Type, TextPos,
		   OptTable, NumOptionals, Ext) ->
    %% OPTIONAL or DEFAULT
    OptPos = get_optionality_pos(TextPos, OptTable),
    Element = case NumOptionals - OptPos of
		  0 ->
		      "Opt band 1";
		  Shift ->
		      lists:concat(["(Opt bsr ",Shift,") band 1"])
	      end,
    {[fun(St) ->
	      emit(["case ",Element," of",nl,
		    "1 ->",nl]),
	      St
      end],
     [fun(St) ->
	      emit([";",nl,
		    "0 ->",nl,
		    "{"]),
	      gen_dec_component_no_val(Ext, Type, Prop),
	      emit([",",{curr,bytes},"}",nl,
		    "end"]),
	      St
      end]};
comp_call_pre_post({ext,_,_}, Prop, Pos, Type, _, _, _, Ext) ->
    %% Extension
    {[fun(St) ->
	      emit(["case Extensions of",nl,
		    "  <<_:",Pos-1,",1:1,_/bitstring>> ->",nl]),
	      St
      end],
     [fun(St) ->
	      emit([";",nl,
		    "_  ->",nl,
		    "{"]),
	      case Type of
		  #type{def=#'SEQUENCE'{
			       extaddgroup=Number2,
			       components=ExtGroupCompList2}}
		    when is_integer(Number2)->
		      emit("{extAddGroup,"),
		      gen_dec_extaddGroup_no_val(Ext, Type, ExtGroupCompList2),
		      emit("}");
		  _ ->
		      gen_dec_component_no_val(Ext, Type, Prop)
	      end,
	      emit([",",{curr,bytes},"}",nl,
		    "end"]),
	      St
      end]}.

is_mandatory_predef_tab_c(noext, mandatory,
			  {"got objfun through args","ObjFun"}) ->
    true;
is_mandatory_predef_tab_c(_, _, {"got objfun through args","ObjFun"}) ->
    false;
is_mandatory_predef_tab_c(_,_,_) ->
    true.

gen_dec_extaddGroup_no_val(Ext, Type, [#'ComponentType'{prop=Prop}])->
    gen_dec_component_no_val(Ext, Type, Prop),
    ok;
gen_dec_extaddGroup_no_val(Ext, Type, [#'ComponentType'{prop=Prop}|Rest])->
    gen_dec_component_no_val(Ext, Type, Prop),
    emit(","),
    gen_dec_extaddGroup_no_val(Ext, Type, Rest);
gen_dec_extaddGroup_no_val(_, _, []) ->
    ok.

gen_dec_component_no_val(_, Type, {'DEFAULT',DefVal0}) ->
    DefVal = asn1ct_gen:conform_value(Type, DefVal0),
    emit([{asis,DefVal}]);
gen_dec_component_no_val(_, _, 'OPTIONAL') ->
    emit({"asn1_NOVALUE"});
gen_dec_component_no_val({ext,_,_}, _, mandatory) ->
    emit({"asn1_NOVALUE"}).
    

gen_dec_choice_line(Erule, TopType, Comp, Pre) ->
    Imm0 = gen_dec_line_imm(Erule, TopType, Comp, false, Pre),
    Init = {ignore,fun(_) -> {[],[]} end},
    Imm = [{group,[Init|Imm0]}],
    emit_gen_dec_imm(Imm).

gen_dec_seq_line_imm(Erule, TopType, Comp, Pos, DecInfObj, Ext) ->
    Pre = gen_dec_line_open_type(Erule, Ext, Pos),
    gen_dec_line_imm(Erule, TopType, Comp, DecInfObj, Pre).

gen_dec_line_imm(Erule, TopType, Comp, DecInfObj, Pre) ->
    #'ComponentType'{name=Cname,typespec=Type} = Comp,
    Atype = 
	case Type of
	    #type{def=#'ObjectClassFieldType'{type=InnerType}} ->
		InnerType;
	    _  ->
		asn1ct_gen:get_inner(Type#type.def)
	end,
    Decode = gen_dec_line_special(Erule, Atype, TopType, Comp, DecInfObj),
    Post =
	fun({SaveBytes,Finish}) ->
		{AccTerm,AccBytes} = Finish(),
		#'ComponentType'{name=Cname} = Comp,
		case DecInfObj of
		    {Cname,ObjSet} ->
			ObjSetRef =
			    case ObjSet of
				{deep,OSName,_,_} ->
				    OSName;
				_ -> ObjSet
			    end,
			{AccTerm++[{ObjSetRef,Cname,
				    asn1ct_gen:mk_var(asn1ct_name:curr(term))}],
			 AccBytes++SaveBytes};
		    _ ->
			{AccTerm,AccBytes++SaveBytes}
		end
	end,
    [Pre,Decode,{safe,Post}].

gen_dec_line_open_type(Erule, {ext,Ep,_}, Pos) when Pos >= Ep ->
    Imm = asn1ct_imm:per_dec_open_type(is_aligned(Erule)),
    {safe,fun(St) ->
		  emit(["begin",nl]),
		  BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		  {Dst,DstBuf} = asn1ct_imm:dec_slim_cg(Imm, BytesVar),
		  emit([",",nl,"{TmpValx",Pos,",_} = "]),
		  {Dst,
		   fun() ->
			   emit([",",nl,
				 "{TmpValx",Pos,",",DstBuf,"}",nl,
				 "end"]),
			   St
		   end}
	  end};
gen_dec_line_open_type(_, _, _) ->
    {safe,fun(St) ->
		  {asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		   fun() -> St end}
	  end}.

gen_dec_line_special(Erule, {typefield,_}, _TopType, Comp,
		     DecInfObj) ->
    #'ComponentType'{name=Cname,typespec=Type,prop=Prop} = Comp,
    fun({_BytesVar,PrevSt}) ->
	    case DecInfObj of
		false -> % This is in a choice with typefield components
		    {Name,RestFieldNames} =
			(Type#type.def)#'ObjectClassFieldType'.fieldname,
		    Imm = asn1ct_imm:per_dec_open_type(is_aligned(Erule)),
		    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		    {TmpTerm,TempBuf} = asn1ct_imm:dec_slim_cg(Imm, BytesVar),
		    emit([com,nl]),
		    #type{tablecinf=[{objfun,
				      #'Externaltypereference'{module=Xmod,
							       type=Xtype}}]} =
			Type,
		    gen_dec_open_type(Erule, "ObjFun", {Xmod,Xtype},
				      '_', {'_',{Name,RestFieldNames},
					    'Result',TmpTerm,mandatory}),
		    emit([com,nl,
			  "{",{asis,Cname},",{Result,",TempBuf,"}}"]),
		    {[],PrevSt};
		{"got objfun through args","ObjFun"} ->
		    %% this is when the generated code gots the
		    %% objfun though arguments on function
		    %% invocation.
		    if
			Prop =:= mandatory ->
			    ok;
			true ->
			    asn1ct_name:new(tmpterm),
			    asn1ct_name:new(tmpbytes),
			    emit([nl,"    {",{curr,tmpterm},", ",{curr,tmpbytes},"} ="])
		    end,
		    {Name,RestFieldNames} =
			(Type#type.def)#'ObjectClassFieldType'.fieldname,
		    Imm = asn1ct_imm:per_dec_open_type(is_aligned(Erule)),
		    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		    asn1ct_imm:dec_code_gen(Imm, BytesVar),
		    emit([com,nl]),
		    #type{tablecinf=[{objfun,
				      #'Externaltypereference'{module=Xmod,
							       type=Xtype}}]} =
			Type,
		    Term = asn1ct_gen:mk_var(asn1ct_name:curr(term)),
		    TmpTerm = asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),
		    if
			Prop =:= mandatory ->
			    gen_dec_open_type(Erule, "ObjFun", {Xmod,Xtype},
					      '_', {'_',{Name,RestFieldNames},
						    Term,TmpTerm,Prop});
			true ->
			    emit(["     {"]),
			    gen_dec_open_type(Erule, "ObjFun", {Xmod,Xtype},
					      '_', {'_',{Name,RestFieldNames},
						    '_',TmpTerm,Prop}),
			    emit([",",nl,{curr,tmpbytes},"}"])
		    end,
		    {[],PrevSt};
		_ ->
		    Imm = asn1ct_imm:per_dec_open_type(is_aligned(Erule)),
		    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
		    asn1ct_imm:dec_code_gen(Imm, BytesVar),
		    RefedFieldName =
			(Type#type.def)#'ObjectClassFieldType'.fieldname,

		    {[{Cname,RefedFieldName,
		       asn1ct_gen:mk_var(asn1ct_name:curr(term)),
		       asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),
		       Prop}],PrevSt}
	    end
    end;
gen_dec_line_special(Erule, Atype, TopType, Comp, DecInfObj) ->
    case gen_dec_line_other(Erule, Atype, TopType, Comp) of
	Fun when is_function(Fun, 1) ->
	    fun({BytesVar,PrevSt}) ->
		    Fun(BytesVar),
		    gen_dec_line_dec_inf(Comp, DecInfObj),
		    {[],PrevSt}
	    end;
	Imm0 ->
	    {imm,Imm0,
	     fun(Imm, {BytesVar,PrevSt}) ->
		     asn1ct_imm:dec_code_gen(Imm, BytesVar),
		     gen_dec_line_dec_inf(Comp, DecInfObj),
		     {[],PrevSt}
	     end}
    end.

gen_dec_line_dec_inf(Comp, DecInfObj) ->
    #'ComponentType'{name=Cname} = Comp,
    case DecInfObj of
	{Cname,{_,_OSet,_UniqueFName,ValIndex}} ->
	    Term = asn1ct_gen:mk_var(asn1ct_name:curr(term)),
	    ValueMatch = value_match(ValIndex,Term),
	    emit([",",nl,
		  "ObjFun = ",ValueMatch]);
	_ ->
	    ok
    end.

gen_dec_line_other(Erule, Atype, TopType, Comp) ->
    #'ComponentType'{name=Cname,typespec=Type} = Comp,
    case asn1ct_gen:type(Atype) of
	#'Externaltypereference'{}=Etype ->
	    fun(BytesVar) ->
		    asn1ct_gen_per:gen_dec_external(Etype, BytesVar)
	    end;
	{primitive,bif} ->
	    asn1ct_gen_per:gen_dec_imm(Erule, Type);
	'ASN1_OPEN_TYPE' ->
	    case Type#type.def of
		#'ObjectClassFieldType'{type=OpenType} ->
		    asn1ct_gen_per:gen_dec_imm(Erule, #type{def=OpenType});
		_ ->
		    asn1ct_gen_per:gen_dec_imm(Erule, Type)
	    end;
	{constructed,bif} ->
	    NewTypename = [Cname|TopType],
	    case Type#type.tablecinf of
		[{objfun,_}|_R] ->
		    fun(BytesVar) ->
			    emit({"'dec_",asn1ct_gen:list2name(NewTypename),
				  "'(",BytesVar,", ObjFun)"})
		    end;
		_ ->
		    fun(BytesVar) ->
			    emit({"'dec_",asn1ct_gen:list2name(NewTypename),
				  "'(",BytesVar,")"})
		    end
	    end
    end.

gen_enc_choice(Erule, TopType, {Root,Exts}, Ext) ->
    Constr = choice_constraint(Root),
    gen_enc_choices(Root, Erule, TopType, 0, Constr, Ext) ++
	gen_enc_choices(Exts, Erule, TopType, 0, ext, Ext);
gen_enc_choice(Erule, TopType, {Root,Exts,[]}, Ext) ->
    gen_enc_choice(Erule, TopType, {Root,Exts}, Ext);
gen_enc_choice(Erule, TopType, Root, Ext) when is_list(Root) ->
    Constr = choice_constraint(Root),
    gen_enc_choices(Root, Erule, TopType, 0, Constr, Ext).

choice_constraint(L) ->
    case length(L) of
	0 -> [{'SingleValue',0}];
	Len -> [{'ValueRange',{0,Len-1}}]
    end.

gen_enc_choices([H|T], Erule, TopType, Pos, Constr, Ext) ->
    #'ComponentType'{name=Cname,typespec=Type} = H,
    Aligned = is_aligned(Erule),
    EncObj =
	case asn1ct_gen:get_constraint(Type#type.constraint,
				       componentrelation) of
	    no -> 
		case Type#type.tablecinf of
		    [{objfun,_}|_] ->
			{"got objfun through args",{var,"ObjFun"}};
		    _ ->
			false
		end;
	    _ ->
		{no_attr,{var,"ObjFun"}}
	end,
    DoExt = case Constr of
		ext -> Ext;
		_ -> noext
	    end,
    Tag = case {Ext,Constr} of
	      {noext,_} ->
		  asn1ct_imm:per_enc_integer(Pos, Constr, Aligned);
	      {{ext,_,_},ext} ->
		  [{put_bits,1,1,[1]}|
		   asn1ct_imm:per_enc_small_number(Pos, Aligned)];
	      {{ext,_,_},_} ->
		  [{put_bits,0,1,[1]}|
		   asn1ct_imm:per_enc_integer(Pos, Constr, Aligned)]
	  end,
    Body = gen_enc_line_imm(Erule, TopType, Cname, Type, {var,"ChoiceVal"},
			    EncObj, DoExt),
    Imm = Tag ++ Body,
    [{Cname,Imm}|gen_enc_choices(T, Erule, TopType, Pos+1, Constr, Ext)];
gen_enc_choices([], _, _, _, _, _)  -> [].

%% Generate the code for CHOICE. If the CHOICE is extensible,
%% the structure of the generated code is as follows:
%%
%% case Bytes of
%%   <<0:1,Bytes1/bitstring>> ->
%%      Choice = <Decode INTEGER (0..LastRootChoice) from Bytes1>
%%      case Choice of
%%        0 -> <Decode>;
%%        :
%%        LastRootChoice -> <Decode>
%%      end;
%%   <<1:1,Bytes1/bitstring>> ->
%%      Choice = <Decode normally small number from Bytes1>
%%      TmpVal = <Decode open type>
%%      case Choice of
%%        0 -> <Decode TmpVal>;
%%        :
%%        LastExtension -> <Decode TmpVal>;
%%        _ -> <Return TmpVal since the type is unknown>
%%       end
%% end
%%
%% The return value from the generated function always looks like:
%%    {{ChoiceTag,Value},RemainingBuffer}
%% where ChoiceTag will be 'asn1_ExtAlt' for an unknown extension.
%%
%% If the CHOICE is not extensible, the top-level case is omitted
%% and only the code in the first case arm is generated.

gen_dec_choice(Erule, TopType, CompList, {ext,_,_}=Ext) ->
    {RootList,ExtList} = split_complist(CompList),
    emit(["case Bytes of",nl]),
    case RootList of
	[] ->
	    ok;
	[_|_] ->
	    emit(["<<0:1,Bytes1/bitstring>> ->",nl]),
	    asn1ct_name:new(bytes),
	    gen_dec_choice1(Erule, TopType, RootList, noext),
	    emit([";",nl,nl])
    end,
    emit(["<<1:1,Bytes1/bitstring>> ->",nl]),
    asn1ct_name:clear(),
    asn1ct_name:new(bytes),
    asn1ct_name:new(bytes),
    gen_dec_choice1(Erule, TopType, ExtList, Ext),
    emit([nl,"end"]);
gen_dec_choice(Erule, TopType, CompList, noext) ->
    gen_dec_choice1(Erule, TopType, CompList, noext).

split_complist({Root1,Ext,Root2}) ->
    {Root1++Root2,Ext};
split_complist({_,_}=CompList) ->
    CompList.

gen_dec_choice1(Erule, TopType, CompList, noext=Ext) ->
    emit_getchoice(Erule, CompList, Ext),
    emit(["case Choice of",nl]),
    Pre = {safe,fun(St) ->
			{asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
			 fun() -> St end}
		end},
    gen_dec_choice2(Erule, TopType, CompList, Pre),
    emit([nl,"end"]);
gen_dec_choice1(Erule, TopType, CompList, {ext,_,_}=Ext) ->
    emit_getchoice(Erule, CompList, Ext),
    Imm = asn1ct_imm:per_dec_open_type(is_aligned(Erule)),
    emit(["begin",nl]),
    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
    {Dst,DstBuf} = asn1ct_imm:dec_slim_cg(Imm, BytesVar),
    emit([nl,
	  "end,",nl,
	  "case Choice of",nl]),
    Pre = {safe,fun(St) ->
			emit(["{TmpVal,_} = "]),
			{Dst,
			 fun() ->
				 emit([",",nl,
				       "{TmpVal,",DstBuf,"}"]),
				 St
			 end}
		end},
    gen_dec_choice2(Erule, TopType, CompList, Pre),
    case CompList of
	[] -> ok;
	[_|_] -> emit([";",nl])
    end,
    emit(["_ ->",nl,
	  "{{asn1_ExtAlt,",Dst,"},",DstBuf,"}",nl,
	  "end"]).

emit_getchoice(Erule, CompList, Ext) ->
    Al = is_aligned(Erule),
    Imm = case {Ext,CompList} of
	      {noext,[_]} ->
		  {value,0};
	      {noext,_} ->
		  asn1ct_imm:per_dec_constrained(0, length(CompList)-1, Al);
	      {{ext,_,_},_} ->
		  asn1ct_imm:per_dec_normally_small_number(Al)
	  end,
    emit(["{Choice,",{curr,bytes},"} = ",nl]),
    BytesVar = asn1ct_gen:mk_var(asn1ct_name:prev(bytes)),
    asn1ct_imm:dec_code_gen(Imm, BytesVar),
    emit([com,nl]).

gen_dec_choice2(Erule,TopType,L,Ext) ->
    gen_dec_choice2(Erule, TopType, L, 0, [], Ext).

gen_dec_choice2(Erule, TopType, [H0|T], Pos, Sep0, Pre) ->
    #'ComponentType'{name=Cname,typespec=Type} = H0,
    H = H0#'ComponentType'{prop=mandatory},
    emit([Sep0,Pos," ->",nl]),
    case Type#type.def of
	#'ObjectClassFieldType'{type={typefield,_}} ->
	    emit("{Cname,{Val,NewBytes}} = begin\n"),
	    gen_dec_choice_line(Erule, TopType, H, Pre),
	    emit([nl,
		  "end,",nl,
		  "{{Cname,Val},NewBytes}"]);
	_ ->
	    emit("{Val,NewBytes} = begin\n"),
	    gen_dec_choice_line(Erule, TopType, H, Pre),
	    emit([nl,
		  "end,",nl,
		  "{{",{asis,Cname},",Val},NewBytes}"])
    end,
    Sep = [";",nl],
    gen_dec_choice2(Erule, TopType, T, Pos+1, Sep, Pre);
gen_dec_choice2(_, _, [], _, _, _)  -> ok.

make_elements(I,Val,ExtCnames) ->
    make_elements(I,Val,ExtCnames,[]).

make_elements(I,Val,[_ExtCname],Acc)-> % the last one, no comma needed
    Element = make_element(I, Val),
    make_elements(I+1,Val,[],[Element|Acc]);
make_elements(I,Val,[_ExtCname|Rest],Acc)->
    Element = make_element(I, Val),
    make_elements(I+1,Val,Rest,[", ",Element|Acc]);
make_elements(_I,_,[],Acc) ->
    lists:reverse(Acc).

make_element(I, Val) ->
    lists:flatten(io_lib:format("element(~w, ~s)", [I,Val])).

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

flat_complist({Rl1,El,Rl2}) -> Rl1 ++ El ++ Rl2;
flat_complist({Rl,El}) -> Rl ++ El;
flat_complist(CompList) -> CompList.

%%wrap_compList({Root1,Ext,Root2}) ->
%%    {Root1,wrap_extensionAdditionGroups(Ext),Root2};
%%wrap_compList({Root1,Ext}) ->
%%    {Root1,wrap_extensionAdditionGroups(Ext)};
%%wrap_compList(CompList) ->
%%    CompList.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Will convert all componentTypes following 'ExtensionAdditionGroup'
%%  up to the matching 'ExtensionAdditionGroupEnd' into one componentType
%% of type SEQUENCE with the componentTypes as components
%%
wrap_extensionAdditionGroups(ExtCompList,ExtGroupPosLen) ->
    wrap_extensionAdditionGroups(ExtCompList,ExtGroupPosLen,[],0,0).

wrap_extensionAdditionGroups([{'ExtensionAdditionGroup',_Number}|Rest],
			     [{ActualPos,_,_}|ExtGroupPosLenRest],Acc,_ExtAddGroupDiff,ExtGroupNum) ->
    {ExtGroupCompList,['ExtensionAdditionGroupEnd'|Rest2]} =
	lists:splitwith(fun(#'ComponentType'{}) -> true;
			   (_) -> false
			end,
			Rest),
    wrap_extensionAdditionGroups(Rest2,ExtGroupPosLenRest,
				 [#'ComponentType'{
				     name=list_to_atom("ExtAddGroup"++
							integer_to_list(ExtGroupNum+1)), 
				     typespec=#type{def=#'SEQUENCE'{
						   extaddgroup=ExtGroupNum+1,
						   components=ExtGroupCompList}},
				     textual_order = ActualPos,
				     prop='OPTIONAL'}|Acc],length(ExtGroupCompList)-1,
				 ExtGroupNum+1);
wrap_extensionAdditionGroups([H=#'ComponentType'{textual_order=Tord}|T],
			     ExtAddGrpLenPos,Acc,ExtAddGroupDiff,ExtGroupNum) when is_integer(Tord) ->
    wrap_extensionAdditionGroups(T,ExtAddGrpLenPos,[H#'ComponentType'{
				      textual_order=Tord - ExtAddGroupDiff}|Acc],ExtAddGroupDiff,ExtGroupNum);
wrap_extensionAdditionGroups([H|T],ExtAddGrpLenPos,Acc,ExtAddGroupDiff,ExtGroupNum) ->
    wrap_extensionAdditionGroups(T,ExtAddGrpLenPos,[H|Acc],ExtAddGroupDiff,ExtGroupNum);
wrap_extensionAdditionGroups([],_,Acc,_,_) ->
    lists:reverse(Acc).

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

enc_dig_out_value([], Value) ->
    {[],Value};
enc_dig_out_value([{N,_}|T], Value) ->
    {Imm0,Dst0} = enc_dig_out_value(T, Value),
    {Imm,Dst} = asn1ct_imm:enc_element(N, Dst0),
    {Imm0++Imm,Dst}.

make_var(Base) ->
    {var,atom_to_list(asn1ct_gen:mk_var(asn1ct_name:curr(Base)))}.
