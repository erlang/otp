%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1ct_constructed_ber.erl,v 1.1 2008/12/17 09:53:29 mikpe Exp $
-module(asn1ct_constructed_ber).

-export([gen_encode_sequence/3]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/3]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/4]).
-export([gen_decode_sof/4]).
-export([gen_encode_choice/3]).
-export([gen_decode_choice/3]).

%%%% Application internal exports
-export([match_tag/2]).

-include("asn1_records.hrl").

-import(asn1ct_gen, [emit/1,demit/1]).

% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).
-define(APPLICATION, 16#40).
-define(CONTEXT,     16#80).
-define(PRIVATE,     16#C0).

% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).




%%===============================================================================
%%===============================================================================
%%===============================================================================
%%  Encode/decode SEQUENCE
%%===============================================================================
%%===============================================================================
%%===============================================================================

gen_encode_sequence(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),

    %% if EXTERNAL type the input value must be transformed to
    %% ASN1 1990 format
    case Typename of
	['EXTERNAL'] ->
	    emit(["   NewVal = asn1rt_check:transform_to_EXTERNAL1990(Val),",
		  nl]);
	_ ->
	    ok
    end,

    {SeqOrSet,TableConsInfo,CompList} =
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} ->
		{'SEQUENCE',TCI,CL};
	    #'SET'{tablecinf=TCI,components=CL} ->
		{'SET',TCI,CL}
	end,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    EncObj =
	case TableConsInfo of
	    #simpletableattributes{usedclassfield=Used,
				   uniqueclassfield=Unique} when Used /= Unique ->
		false;
	    %% ObjectSet, name of the object set in constraints
	    %%
	    %%{ObjectSet,AttrN,N,UniqueFieldName}
	    #simpletableattributes{objectsetname=ObjectSet,
				   c_name=AttrN,
				   c_index=N,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValueIndex
				  } ->
		OSDef =
		    case ObjectSet of
			{Module,OSName} ->
			    asn1_db:dbget(Module,OSName);
			OSName ->
			    asn1_db:dbget(get(currmod),OSName)
		    end,
%		io:format("currmod: ~p~nOSName: ~p~nAttrN: ~p~nN: ~p~nUniqueFieldName: ~p~n",
%			  [get(currmod),OSName,AttrN,N,UniqueFieldName]),
		case (OSDef#typedef.typespec)#'ObjectSet'.gen of
		    true ->
% 			Val = lists:concat(["?RT_BER:cindex(",
% 					    N+1,",Val,"]),
			ObjectEncode =
			    asn1ct_gen:un_hyphen_var(lists:concat(['Obj',
								   AttrN])),
			emit({ObjectEncode," = ",nl}),
			emit({"  'getenc_",ObjectSet,"'(",{asis,UniqueFieldName},
			      ", ",nl}),
%			emit({indent(35),"?RT_BER:cindex(",N+1,", Val,",
%			      {asis,AttrN},")),",nl}),
			emit([indent(10+length(atom_to_list(ObjectSet))),
			      "value_match(",{asis,ValueIndex},",",
			      "?RT_BER:cindex(",N+1,",Val,",
			      {asis,AttrN},"))),",nl]),
			notice_value_match(),
			{AttrN,ObjectEncode};
		    _ ->
			false
		end;
	    _ ->
		case D#type.tablecinf of
		    [{objfun,_}|_] ->
			%% when the simpletableattributes was at an
			%% outer level and the objfun has been passed
			%% through the function call
			{"got objfun through args","ObjFun"};
		    _ ->
			false
		end
	end,

    gen_enc_sequence_call(Erules,Typename,CompList1,1,Ext,EncObj),

    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(SeqOrSet),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit([nl,"   BytesSoFar = "]),
    case SeqOrSet of
	'SET' when (D#type.def)#'SET'.sorted == dynamic ->
	    emit("?RT_BER:dynamicsort_SET_components(["),
	    mkvlist(asn1ct_name:all(encBytes)),
	    emit(["]),",nl]);
	_ ->
	    emit("["),
	    mkvlist(asn1ct_name:all(encBytes)),
	    emit(["],",nl])
    end,
    emit("  LenSoFar = "),
    case asn1ct_name:all(encLen) of
	[] -> emit("0");
	AllLengths ->
	    mkvplus(AllLengths)
    end,
    emit([",",nl]),
% emit(["{TagBytes,Len} = ?RT_BER:encode_tags(TagIn ++ ",
 emit(["  ?RT_BER:encode_tags(TagIn ++ ",
		  {asis,MyTag},", BytesSoFar, LenSoFar).",nl]).


gen_decode_sequence(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
%    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    #'SEQUENCE'{tablecinf=TableConsInfo,components=CList} = D#type.def,
    Ext = extensible(CList),
    CompList = case CList of
		   {Rl,El}  -> Rl ++ El;
		   _ -> CList
	       end,

    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),

    asn1ct_name:new(rb),
    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type('SEQUENCE'),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,",asn1ct_gen_ber:unused_var("Len",D#type.def),"},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),

    case CompList of
	[] -> true;
	_ ->
	    emit({"{",{next,bytes},
		  ",RemBytes} = ?RT_BER:split_list(",
		  {curr,bytes},
		  ",", {prev,len},"),",nl}),
	    asn1ct_name:new(bytes)
    end,

    {DecObjInf,UniqueFName,ValueIndex} =
	case TableConsInfo of
	    #simpletableattributes{objectsetname=ObjectSet,
				   c_name=AttrN,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValIndex
				  } ->
		F = fun(#'ComponentType'{typespec=CT})->
			    case {CT#type.constraint,CT#type.tablecinf} of
				{[],[{objfun,_}|_R]} -> true;
				_ -> false
			    end
		    end,
		case lists:any(F,CompList) of
		    %%AttributeName = asn1ct_gen:un_hyphen_var(AttrN),
		    true -> % when component relation constraint establish
			%% relation from a component to another components
			%% subtype component
			{{AttrN,{deep,ObjectSet,UniqueFieldName,
					 ValIndex}},
			 UniqueFieldName,ValIndex};
		    false ->
			{{AttrN,ObjectSet},UniqueFieldName,ValIndex}
		end;
	    _ ->
		{false,false,false}
	end,
    case gen_dec_sequence_call(Erules,Typename,CompList,Ext,DecObjInf) of
	no_terms -> % an empty sequence
	    emit([nl,nl]),
	    demit({"Result = "}), %dbg
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit(["   {{'",asn1ct_gen:list2rname(Typename),"'}, ",{curr,bytes},",",nl,"    "]),
	    asn1ct_gen_ber:add_removed_bytes(),
	    emit(["}.",nl]);
	{LeadingAttrTerm,PostponedDecArgs} ->
	    emit([com,nl,nl]),
	    case {LeadingAttrTerm,PostponedDecArgs} of
		{[],[]} ->
		    ok;
		{_,[]} ->
		    ok;
		{[{ObjSet,LeadingAttr,Term}],PostponedDecArgs} ->
		    DecObj = asn1ct_gen:un_hyphen_var(lists:concat(['DecObj',LeadingAttr,Term])),
		    ValueMatch = value_match(ValueIndex,Term),
		    emit([DecObj," =",nl,"   'getdec_",ObjSet,"'(",
%			  {asis,UniqueFName},", ",Term,"),",nl}),
			  {asis,UniqueFName},", ",ValueMatch,"),",nl]),
		    gen_dec_postponed_decs(DecObj,PostponedDecArgs)
	    end,
	    demit({"Result = "}), %dbg
	    %% return value as record
	    asn1ct_name:new(rb),
	    asn1ct_name:new(bytes),
	    ExtStatus = case Ext of
			    {ext,_,_} -> ext;
			    noext -> noext
			end,
	    emit(["   {",{next,bytes},",",{curr,rb},"} = ?RT_BER:restbytes2(RemBytes, ",
		  {curr,bytes},",",ExtStatus,"),",nl]),
	    asn1ct_name:new(rb),
	    case Typename of
		['EXTERNAL'] ->
		    emit(["   OldFormat={'",asn1ct_gen:list2rname(Typename),
			  "', "]),
		    mkvlist(asn1ct_name:all(term)),
		    emit(["},",nl]),
		    emit(["   ASN11994Format =",nl,
			  "      asn1rt_check:transform_to_EXTERNAL1994",
			  "(OldFormat),",nl]),
		    emit(["   {ASN11994Format,",{next,bytes},", "]);
		_ ->
		    emit(["   {{'",asn1ct_gen:list2rname(Typename),"', "]),
		    mkvlist(asn1ct_name:all(term)),
		    emit(["}, ",{next,bytes},", "])
	    end,
	    asn1ct_gen_ber:add_removed_bytes(),
	    emit(["}.",nl])
    end.

gen_dec_postponed_decs(_,[]) ->
    emit(nl);
gen_dec_postponed_decs(DecObj,[{_Cname,{FirstPFN,PFNList},Term,TmpTerm,_Tag,OptOrMand}|Rest]) ->
%    asn1ct_name:new(term),
    asn1ct_name:new(tmpterm),
    asn1ct_name:new(reason),

    emit({"{",Term,", _, _} = ",nl}),
    N = case OptOrMand of
	    mandatory -> 0;
	    'OPTIONAL' ->
		emit_opt_or_mand_check(asn1_NOVALUE,TmpTerm),
		6;
	    {'DEFAULT',Val} ->
		emit_opt_or_mand_check(Val,TmpTerm),
		6
	end,
    emit({indent(N+3),"case (catch ",DecObj,"(",{asis,FirstPFN},
%	  ", ",TmpTerm,", ", {asis,Tag},", ",{asis,PFNList},")) of",nl}),
	  ", ",TmpTerm,", [], ",{asis,PFNList},")) of",nl}),
    emit({indent(N+6),"{'EXIT', ",{curr,reason},"} ->",nl}),
    emit({indent(N+9),"exit({'Type not compatible with table constraint',",
	  {curr,reason},"});",nl}),
    emit({indent(N+6),{curr,tmpterm}," ->",nl}),
    emit({indent(N+9),{curr,tmpterm},nl}),

    case OptOrMand of
	mandatory -> emit([indent(N+3),"end,",nl]);
	_ ->
	    emit([indent(N+3),"end",nl,
		  indent(3),"end,",nl])
    end,
%    emit({indent(3),"end,",nl}),
    gen_dec_postponed_decs(DecObj,Rest).


emit_opt_or_mand_check(Value,TmpTerm) ->
    emit([indent(3),"case ",TmpTerm," of",nl,
	  indent(6),{asis,Value}," -> {",{asis,Value},",[],[]};",nl,
	  indent(6),"_ ->",nl]).

%%============================================================================
%%  Encode/decode SET
%%
%%============================================================================

gen_encode_set(Erules,Typename,D) when record(D,type) ->
    gen_encode_sequence(Erules,Typename,D).

gen_decode_set(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    #'SET'{components=TCompList} = D#type.def,
    Ext = extensible(TCompList),
    CompList = case TCompList of
		   {Rl,El} -> Rl ++ El;
		   _ -> TCompList
	       end,

    emit(["   %%-------------------------------------------------",nl]),
    emit(["   %% decode tag and length ",nl]),
    emit(["   %%-------------------------------------------------",nl]),

    asn1ct_name:new(rb),
    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type('SET'),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,Len},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),
    asn1ct_name:new(rb),

    emit(["   {SetTerm, SetBytes, ",{curr,rb},"} = ?RT_BER:decode_set(0, Len, ",
	  {curr,bytes},", OptOrMand, ",
	  "fun 'dec_",asn1ct_gen:list2name(Typename),"_fun'/2, []),",nl]),

    asn1ct_name:new(rb),
    emit(["   'dec_",asn1ct_gen:list2name(Typename),"_result'(lists:sort(SetTerm), SetBytes, "]),
    asn1ct_gen_ber:add_removed_bytes(),
    emit([").",nl,nl,nl]),

    emit({"%%-------------------------------------------------",nl}),
    emit({"%% Set loop fun for ",asn1ct_gen:list2name(Typename),nl}),
    emit({"%%-------------------------------------------------",nl}),

    asn1ct_name:clear(),
    asn1ct_name:new(term),
    emit(["'dec_",asn1ct_gen:list2name(Typename),"_fun'(",{curr,bytes},
	  ", OptOrMand) ->",nl]),

    asn1ct_name:new(bytes),
    gen_dec_set(Erules,Typename,CompList,1,Ext),

    emit(["      %% tag not found, if extensionmark we should skip bytes here",nl]),
    emit([indent(6),"_ -> {[], Bytes,0}",nl]),
    emit([indent(3),"end.",nl,nl,nl]),


    emit({"%%-------------------------------------------------",nl}),
    emit({"%% Result ",asn1ct_gen:list2name(Typename),nl}),
    emit({"%%-------------------------------------------------",nl}),

    asn1ct_name:clear(),
    emit({"'dec_",asn1ct_gen:list2name(Typename),"_result'(",
	  asn1ct_gen_ber:unused_var("TermList",D#type.def),", Bytes, Rb) ->",nl}),

    case gen_dec_set_result(Erules,Typename,CompList) of
	no_terms ->
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit({"   {{'",asn1ct_gen:list2rname(Typename),"'}, Bytes, Rb}.",nl});
	_ ->
	    emit({nl,"   case ",{curr,termList}," of",nl}),
	    emit({"      [] -> {{'",asn1ct_gen:list2rname(Typename),"', "}),
	    mkvlist(asn1ct_name:all(term)),
	    emit({"}, Bytes, Rb};",nl}),
	    emit({"      ExtraAtt -> exit({error,{asn1,{too_many_attributes, ExtraAtt}}})",nl}),
	    emit({"   end.",nl}),
	    emit({nl,nl,nl})
    end.


%%===============================================================================
%%===============================================================================
%%===============================================================================
%%  Encode/decode SEQUENCE OF and SET OF
%%===============================================================================
%%===============================================================================
%%===============================================================================

gen_encode_sof(Erules,Typename,_InnerTypename,D) when record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, Cont} = D#type.def,

    Objfun = case D#type.tablecinf of
		 [{objfun,_}|_R] ->
		     ", ObjFun";
		 _ ->
		     ""
	     end,

    emit({"   {EncBytes,EncLen} = 'enc_",asn1ct_gen:list2name(Typename),
	  "_components'(Val",Objfun,",[],0),",nl}),

    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(SeqOrSetOf),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
%    gen_encode_tags(Erules,MyTag,"EncLen","EncBytes"),
	    emit(["   ?RT_BER:encode_tags(TagIn ++ ",
		  {asis,MyTag},", EncBytes, EncLen).",nl,nl]),

    gen_encode_sof_components(Erules,Typename,SeqOrSetOf,Cont).
%     gen_enc_line(Erules,Typename,TypeNameSuffix,Cont,"H",0,
% 		 mandatory,"{EncBytes,EncLen} = "),


gen_decode_sof(Erules,Typename,_InnerTypename,D) when record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, TypeTag, Cont} =
	case D#type.def of
	    {'SET OF',_Cont} -> {'SET OF','SET',_Cont};
	    {'SEQUENCE OF',_Cont} -> {'SEQUENCE OF','SEQUENCE',_Cont}
	end,
    TypeNameSuffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,Cont#type.def),

    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),

    asn1ct_name:new(rb),
    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(TypeTag),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,Len},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),

    emit(["   ?RT_BER:decode_components(",{curr,rb}]),
    InnerType = asn1ct_gen:get_inner(Cont#type.def),
    ContName = case asn1ct_gen:type(InnerType) of
		   Atom when atom(Atom) -> Atom;
		   _ -> TypeNameSuffix
	       end,
    emit([", Len, ",{next,bytes},", "]),
%    NewCont =
%	case Cont#type.def of
%	    {'ENUMERATED',_,Components}->
%		Cont#type{def={'ENUMERATED',Components}};
%	    _ -> Cont
%	end,
     ObjFun =
	case D#type.tablecinf of
	    [{objfun,_}|_R] ->
		", ObjFun";
	    _ ->
		[]
	end,
    gen_dec_line_sof(Erules,Typename,ContName,Cont,ObjFun),
    emit([", []).",nl,nl,nl]).


gen_encode_sof_components(Erules,Typename,SeqOrSetOf,Cont)
  when record(Cont,type)->

    {Objfun,ObjFun_novar,EncObj} =
	case Cont#type.tablecinf of
	    [{objfun,_}|_R] ->
		{", ObjFun",", _",{no_attr,"ObjFun"}};
	    _ ->
		{"","",false}
	end,
    emit(["'enc_",asn1ct_gen:list2name(Typename),
	  "_components'([]",ObjFun_novar,", AccBytes, AccLen) -> ",nl]),

    case catch lists:member(der,get(encoding_options)) of
	true ->
	    emit([indent(3),
		  "{?RT_BER:dynamicsort_SETOF(AccBytes),AccLen};",nl,nl]);
	_ ->
	    emit([indent(3),"{lists:reverse(AccBytes),AccLen};",nl,nl])
    end,
    emit(["'enc_",asn1ct_gen:list2name(Typename),
	  "_components'([H|T]",Objfun,",AccBytes, AccLen) ->",nl]),
    TypeNameSuffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,Cont#type.def),
    gen_enc_line(Erules,Typename,TypeNameSuffix,Cont,"H",3,
		 mandatory,"{EncBytes,EncLen} = ",EncObj),
    emit([",",nl]),
    emit([indent(3),"'enc_",asn1ct_gen:list2name(Typename),
	  "_components'(T",Objfun,","]),
    emit(["[EncBytes|AccBytes], AccLen + EncLen).",nl,nl]).

%%============================================================================
%%  Encode/decode CHOICE
%%
%%============================================================================

gen_encode_choice(Erules,Typename,D) when record(D,type) ->
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_enc_choice(Erules,Typename,ChoiceTag,CompList1,Ext),
    emit({nl,nl}).

gen_decode_choice(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_dec_choice(Erules,Typename,ChoiceTag,CompList1,Ext),
    emit({".",nl}).


%%============================================================================
%%  Encode SEQUENCE
%%
%%============================================================================

gen_enc_sequence_call(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],Pos,Ext,EncObj) ->
    asn1ct_name:new(encBytes),
    asn1ct_name:new(encLen),
    Element =
	case TopType of
	    ['EXTERNAL'] ->
		io_lib:format("?RT_BER:cindex(~w,NewVal,~w)",[Pos+1,Cname]);
	    _ ->
		io_lib:format("?RT_BER:cindex(~w,Val,~w)",[Pos+1,Cname])
	end,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    print_attribute_comment(InnerType,Pos,Prop),
    gen_enc_line(Erules,TopType,Cname,Type,Element,3,Prop,EncObj),
    case Rest of
	[] ->
	    emit({com,nl});
	_ ->
	    emit({com,nl}),
	    gen_enc_sequence_call(Erules,TopType,Rest,Pos+1,Ext,EncObj)
    end;

gen_enc_sequence_call(_Erules,_TopType,[],_Num,_,_) ->
	true.

%%============================================================================
%%  Decode SEQUENCE
%%
%%============================================================================

gen_dec_sequence_call(Erules,TopType,CompList,Ext,DecObjInf) ->
    gen_dec_sequence_call1(Erules,TopType, CompList, 1, Ext,DecObjInf,[],[]).


gen_dec_sequence_call1(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,tags=Tags}|Rest],Num,Ext,DecObjInf,LeadingAttrAcc,ArgsAcc) ->
    {LA,PostponedDec} =
	gen_dec_component(Erules,TopType,Cname,Tags,Type,Num,Prop,
			  Ext,DecObjInf),
    case Rest of
	[] ->
	    {LA ++ LeadingAttrAcc,PostponedDec ++ ArgsAcc};
	_ ->
	    emit({com,nl}),
%	    asn1ct_name:new(term),
	    asn1ct_name:new(bytes),
	    gen_dec_sequence_call1(Erules,TopType,Rest,Num+1,Ext,DecObjInf,
				   LA++LeadingAttrAcc,PostponedDec++ArgsAcc)
    end;

gen_dec_sequence_call1(_Erules,_TopType,[],1,_,_,_,_) ->
    no_terms.
%%gen_dec_sequence_call1(Erules,_TopType,[],Num,_) ->
%%    true.



%%----------------------------
%%SEQUENCE mandatory
%%----------------------------

gen_dec_component(Erules,TopType,Cname,CTags,Type,Pos,Prop,Ext,DecObjInf) ->
    InnerType =
	case Type#type.def of
	    #'ObjectClassFieldType'{type=OCFTType} -> OCFTType;
	    _ -> asn1ct_gen:get_inner(Type#type.def)
	end,
% 	case asn1ct_gen:get_constraint(Type#type.constraint,
% 				       tableconstraint_info) of
% 	    no ->
% 		asn1ct_gen:get_inner(Type#type.def);
% 	    _ ->
% 		Type#type.def
% 	end,
    Prop1 = case {Prop,Ext} of
		{mandatory,{ext,Epos,_}} when Pos >= Epos ->
		    'OPTIONAL';
		_ ->
		    Prop
	    end,
    print_attribute_comment(InnerType,Pos,Prop1),
    emit("   "),

    case {InnerType,DecObjInf} of
	{{typefield,_},NotFalse} when NotFalse /= false ->
	    asn1ct_name:new(term),
	    asn1ct_name:new(tmpterm),
	    emit({"{",{curr,tmpterm},", ",{next,bytes},",",{next,rb},"} = "});
	{{objectfield,_,_},_} ->
	    asn1ct_name:new(term),
	    asn1ct_name:new(tmpterm),
	    emit({"{",{curr,tmpterm},", ",{next,bytes},",",{next,rb},"} = "});
	_ ->
	    asn1ct_name:new(term),
	    emit({"{",{curr,term},",",{next,bytes},",",{next,rb},"} = "})
    end,
    asn1ct_name:new(rb),
    PostponedDec =
	gen_dec_line(Erules,TopType,Cname,CTags,Type,Prop1,DecObjInf),
    asn1ct_name:new(form),
    PostponedDec.


%%-------------------------------------
%%  Decode SET
%%-------------------------------------

gen_dec_set(Erules,TopType,CompList,Pos,_Ext) ->
    TagList = get_all_choice_tags(CompList),
    emit({indent(3),
	  {curr,tagList}," = ",{asis,TagList},",",nl}),
    emit({indent(3),
	  "case ?RT_BER:check_if_valid_tag(Bytes, ",
	  {curr,tagList},", OptOrMand) of",nl}),
    asn1ct_name:new(tagList),
    asn1ct_name:new(rbCho),
    asn1ct_name:new(choTags),
    gen_dec_set_cases(Erules,TopType,CompList,TagList,Pos),
    asn1ct_name:new(tag),
    asn1ct_name:new(bytes).



gen_dec_set_cases(_,_,[],_,_) ->
    ok;
gen_dec_set_cases(Erules,TopType,[H|T],List,Pos) ->
    case H of
	{'EXTENSIONMARK', _, _} ->
	    gen_dec_set_cases(Erules,TopType,T,List,Pos);
	_ ->
	    Name = H#'ComponentType'.name,
	    Type = H#'ComponentType'.typespec,

	    emit({indent(6),"'",Name,"' ->",nl}),
	    case Type#type.def of
		{'CHOICE',_NewCompList} ->
		    gen_dec_set_cases_choice(Erules,TopType,H,Pos);
		_ ->
		    gen_dec_set_cases_type(Erules,TopType,H,Pos)
	    end,
	    gen_dec_set_cases(Erules,TopType,T,List,Pos+1)
    end.




gen_dec_set_cases_choice(_Erules,TopType,H,Pos) ->
    Cname = H#'ComponentType'.name,
    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- (H#'ComponentType'.typespec)#type.tag],
    asn1ct_name:new(rbCho),
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    emit({"'dec_",asn1ct_gen:list2name([Cname|TopType]),
	  "'(Bytes,OptOrMand,",{asis,Tag},"),",nl}),
    emit(["      {{",Pos,",Dec}, Rest, ",{curr,rbCho},"}"]),
    emit([";",nl,nl]).


gen_dec_set_cases_type(Erules,TopType,H,Pos) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    %% always use Prop = mandatory here    Prop = H#'ComponentType'.prop,

    asn1ct_name:new(rbCho),
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    asn1ct_name:delete(bytes),
    %% we have already seen the tag so now we must find the value
    %% that why we always use 'mandatory' here
    gen_dec_line(Erules,TopType,Cname,[],Type,mandatory,decObjInf),
    asn1ct_name:new(bytes),

    emit([",",nl]),
    emit(["{{",Pos,",Dec}, Rest, ",{curr,rbCho},"}"]),
    emit([";",nl,nl]).


%%---------------------------------
%%  Decode SET result
%%---------------------------------

gen_dec_set_result(Erules,TopType,{CompList,_ExtList}) ->
    gen_dec_set_result1(Erules,TopType, CompList, 1);
gen_dec_set_result(Erules,TopType,CompList) ->
    gen_dec_set_result1(Erules,TopType, CompList, 1).

gen_dec_set_result1(Erules,TopType,
		   [#'ComponentType'{name=Cname,
				     typespec=Type,
				     prop=Prop}|Rest],Num) ->
    gen_dec_set_component(Erules,TopType,Cname,Type,Num,Prop),
    case Rest of
	[] ->
	    true;
	_ ->
	    gen_dec_set_result1(Erules,TopType,Rest,Num+1)
    end;

gen_dec_set_result1(_Erules,_TopType,[],1) ->
    no_terms;
gen_dec_set_result1(_Erules,_TopType,[],_Num) ->
    true.


gen_dec_set_component(_Erules,_TopType,_Cname,Type,Pos,Prop) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    print_attribute_comment(InnerType,Pos,Prop),
    emit({"   {",{next,term},com,{next,termList},"} =",nl}),
    emit({"      case ",{curr,termList}," of",nl}),
    emit({"          [{",Pos,com,{curr,termTmp},"}|",
	  {curr,rest},"] -> "}),
    emit({"{",{curr,termTmp},com,
	  {curr,rest},"};",nl}),
    case Prop of
	'OPTIONAL' ->
	    emit([indent(10),"_ -> {asn1_NOVALUE, ",{curr,termList},"}",nl]);
	{'DEFAULT', DefVal} ->
	    emit([indent(10),
		  "_ -> {",{asis,DefVal},", ",{curr,termList},"}",nl]);
	mandatory ->
	    emit([indent(10),
		  "_ -> exit({error,{asn1,{mandatory_attribute_no, ",
		  Pos,", missing}}})",nl])
    end,
    emit([indent(6),"end,",nl]),
    asn1ct_name:new(rest),
    asn1ct_name:new(term),
    asn1ct_name:new(termList),
    asn1ct_name:new(termTmp).


%%---------------------------------------------
%%  Encode CHOICE
%%---------------------------------------------
%% for BER we currently do care (a little) if the choice has an EXTENSIONMARKER


gen_enc_choice(Erules,TopType,Tag,CompList,_Ext) ->
    gen_enc_choice1(Erules,TopType,Tag,CompList,_Ext).

gen_enc_choice1(Erules,TopType,Tag,CompList,_Ext) ->
    asn1ct_name:clear(),
    emit({"   {EncBytes,EncLen} = case element(1,Val) of",nl}),
    gen_enc_choice2(Erules,TopType,CompList),
    emit([nl,"   end,",nl,nl]),
    NewTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- Tag],
%    gen_encode_tags(Erules,NewTag,"EncLen","EncBytes").
   emit(["?RT_BER:encode_tags(TagIn ++",{asis,NewTag},", EncBytes, EncLen).",nl]).



gen_enc_choice2(Erules,TopType,[H1|T]) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({"      ",{asis,Cname}," ->",nl}),
    {Encobj,Assign} =
% 	case asn1ct_gen:get_constraint(Type#type.constraint,
% 				       tableconstraint_info) of
	case {Type#type.def,asn1ct_gen:get_constraint(Type#type.constraint,
						      componentrelation)} of
	    {#'ObjectClassFieldType'{},{componentrelation,_,_}} ->
		asn1ct_name:new(tmpBytes),
		asn1ct_name:new(encBytes),
		asn1ct_name:new(encLen),
		Emit = ["{",{curr,tmpBytes},", _} = "],
		{{no_attr,"ObjFun"},Emit};
	    _ ->
		{false,[]}
	end,
    gen_enc_line(Erules,TopType,Cname,Type,"element(2,Val)",9,
		 mandatory,Assign,Encobj),
    case Encobj of
	false -> ok;
	_ ->
	    emit({",",nl,indent(9),"{",{curr,encBytes},", ",
		  {curr,encLen},"}"})
    end,
    emit({";",nl}),
    case T of
	[] ->
	    emit([indent(6), "Else -> ",nl,
		  indent(9),"exit({error,{asn1,{invalid_choice_type,Else}}})"]);
	_ ->
	    true
    end,
    gen_enc_choice2(Erules,TopType,T);

gen_enc_choice2(_,_,[])  ->
    true.




%%--------------------------------------------
%%  Decode CHOICE
%%--------------------------------------------

gen_dec_choice(Erules,TopType, ChTag, CompList, Ext) ->
    asn1ct_name:delete(bytes),
    Tags = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- ChTag],

    emit(["   {{_,Len},",{next,bytes},
	  ", RbExp} = ?RT_BER:check_tags(TagIn++",
	  {asis,Tags},", ",
	  {curr,bytes},", OptOrMand),",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),
    gen_dec_choice_indef_funs(Erules),
    case Erules of
	ber_bin ->
	    emit([indent(3),"case ",{curr,bytes}," of",nl]);
	ber ->
	    emit([indent(3),
		  "case (catch ?RT_BER:peek_tag(",{curr,bytes},")) of",nl])
    end,
    asn1ct_name:new(tagList),
    asn1ct_name:new(choTags),
    gen_dec_choice_cases(Erules,TopType,CompList),
    case Ext of
	noext ->
	    emit([indent(6), {curr,else}," -> ",nl]),
	    emit([indent(9),"case OptOrMand of",nl,
		  indent(12),"mandatory ->","exit({error,{asn1,",
		  "{invalid_choice_tag,",{curr,else},"}}});",nl,
		  indent(12),"_ ->","exit({error,{asn1,{no_optional_tag,",
		  {curr,else},"}}})",nl,
		  indent(9),"end",nl]);
	_ ->
	    emit([indent(6),"_ -> ",nl]),
	    emit([indent(9),"{{asn1_ExtAlt,",{curr,bytes},"},",
		  empty_lb(Erules),", RbExp}",nl])
    end,
    emit([indent(3),"end"]),
    asn1ct_name:new(tag),
    asn1ct_name:new(else).

gen_dec_choice_indef_funs(Erules) ->
    emit({indent(3),"IndefEndBytes = fun(indefinite,",indefend_match(Erules,used_var),
	  ")-> R; (_,B)-> B end,",nl}),
    emit({indent(3),"IndefEndRb = fun(indefinite,",indefend_match(Erules,unused_var),
	  ")-> 2; (_,_)-> 0 end,",nl}).


gen_dec_choice_cases(_,_, []) ->
    ok;
gen_dec_choice_cases(Erules,TopType, [H|T]) ->
    asn1ct_name:push(rbCho),
    Name = H#'ComponentType'.name,
    emit([nl,"%% '",Name,"'",nl]),
    Fcases  = fun([T1,T2|Tail],Fun) ->
		      emit([indent(6),match_tag(Erules,T1)," ->",nl]),
		      gen_dec_choice_cases_type(Erules,TopType, H),
		      Fun([T2|Tail],Fun);
		 ([T1],_) ->
		      emit([indent(6),match_tag(Erules,T1)," ->",nl]),
		      gen_dec_choice_cases_type(Erules,TopType, H)
	      end,
    Fcases(H#'ComponentType'.tags,Fcases),
    asn1ct_name:pop(rbCho),
    gen_dec_choice_cases(Erules,TopType, T).



gen_dec_choice_cases_type(Erules,TopType,H) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    Prop = H#'ComponentType'.prop,
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    gen_dec_line(Erules,TopType,Cname,[],Type,Prop,false),
    emit([",",nl,indent(9),"{{",{asis,Cname},
	  ", Dec}, IndefEndBytes(Len,Rest), RbExp + ",
	  {curr,rbCho}," + IndefEndRb(Len,Rest)};",nl,nl]).

encode_tag_val(Erules,{Class,TagNo}) when integer(TagNo) ->
    Rtmod = rtmod(Erules),
    Rtmod:encode_tag_val({asn1ct_gen_ber:decode_class(Class),
				  0,TagNo});
encode_tag_val(Erules,{Class,TypeName}) ->
    Rtmod = rtmod(Erules),
    Rtmod:encode_tag_val({asn1ct_gen_ber:decode_class(Class),
				  0,asn1ct_gen_ber:decode_type(TypeName)}).


match_tag(ber_bin,Arg) ->
    match_tag_with_bitsyntax(Arg);
match_tag(Erules,Arg) ->
    io_lib:format("~p",[encode_tag_val(Erules,Arg)]).

match_tag_with_bitsyntax({Class,TagNo}) when integer(TagNo) ->
    match_tag_with_bitsyntax1({asn1ct_gen_ber:decode_class(Class),
				  0,TagNo});
match_tag_with_bitsyntax({Class,TypeName}) ->
    match_tag_with_bitsyntax1({asn1ct_gen_ber:decode_class(Class),
				  0,asn1ct_gen_ber:decode_type(TypeName)}).

match_tag_with_bitsyntax1({Class, _Form, TagNo}) when (TagNo =< 30) ->
    io_lib:format("<<~p:2,_:1,~p:5,_/binary>>",[Class bsr 6,TagNo]);

match_tag_with_bitsyntax1({Class, _Form, TagNo}) ->
    {Octets,Len} = mk_object_val(TagNo),
    OctForm = case Len of
		 1 -> "~p";
		 2 -> "~p,~p";
		 3 -> "~p,~p,~p";
		 4 -> "~p,~p,~p,~p"
	     end,
    io_lib:format("<<~p:2,_:1,31:5," ++ OctForm ++ ",_/binary>>",
		  [Class bsr 6] ++ Octets).

%%%%%%%%%%%
%% mk_object_val(Value) -> {OctetList, Len}
%% returns a Val as a list of octets, the 8 bit is allways set to one except
%% for the last octet, where its 0
%%


mk_object_val(Val) when Val =< 127 ->
    {[255 band Val], 1};
mk_object_val(Val) ->
    mk_object_val(Val bsr 7, [Val band 127], 1).
mk_object_val(0, Ack, Len) ->
    {Ack, Len};
mk_object_val(Val, Ack, Len) ->
    mk_object_val(Val bsr 7, [((Val band 127) bor 128) | Ack], Len + 1).


get_all_choice_tags(ComponentTypeList) ->
    get_all_choice_tags(ComponentTypeList,[]).

get_all_choice_tags([],TagList) ->
    TagList;
get_all_choice_tags([H|T],TagList)  ->
    Tags = H#'ComponentType'.tags,
    get_all_choice_tags(T, TagList ++ [{H#'ComponentType'.name, Tags}]).



%%---------------------------------------
%% Generate the encode/decode code
%%---------------------------------------

gen_enc_line(Erules,TopType,Cname,
	     Type=#type{constraint=[{componentrelation,_,_}],
			def=#'ObjectClassFieldType'{type={typefield,_}}},
	     Element,Indent,OptOrMand=mandatory,EncObj)
  when list(Element) ->
    asn1ct_name:new(tmpBytes),
    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
		 ["{",{curr,tmpBytes},",_} = "],EncObj);
gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,EncObj)
  when list(Element) ->
    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
		 ["{",{curr,encBytes},",",{curr,encLen},"} = "],EncObj).

gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,Assign,EncObj)
  when list(Element) ->
    IndDeep = indent(Indent),

    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- Type#type.tag],
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    emit(IndDeep),
    emit(Assign),
    gen_optormand_case(OptOrMand,Erules,TopType,Cname,Type,InnerType,WhatKind,
		       Element),
    case {Type,asn1ct_gen:get_constraint(Type#type.constraint,
					 componentrelation)} of
% 	#type{constraint=[{tableconstraint_info,RefedFieldName}],
% 	      def={typefield,_}} ->
	{#type{def=#'ObjectClassFieldType'{type={typefield,_},
					   fieldname=RefedFieldName}},
	 {componentrelation,_,_}} ->
	    {_LeadingAttrName,Fun} = EncObj,
	    case RefedFieldName of
		{notype,T} ->
		    throw({error,{notype,type_from_object,T}});
		{Name,RestFieldNames} when atom(Name) ->
		    case OptOrMand of
			mandatory -> ok;
			_ ->
%			    emit(["{",{curr,tmpBytes},",",{curr,tmpLen},
			    emit(["{",{curr,tmpBytes},", _} = "])
%%			    asn1ct_name:new(tmpBytes),
%%			    asn1ct_name:new(tmpLen)
		    end,
		    emit({Fun,"(",{asis,Name},", ",Element,", [], ",
			  {asis,RestFieldNames},"),",nl}),
		    emit(IndDeep),
		    case OptOrMand of
			mandatory ->
			    emit({"{",{curr,encBytes},", ",{curr,encLen},"} = "}),
			    emit({"?RT_BER:encode_open_type(",{curr,tmpBytes},
				  ",",{asis,Tag},")"});
			_ ->
%			    emit({"{",{next,tmpBytes},", _} = "}),
			    emit({"{",{next,tmpBytes},", ",{curr,tmpLen},
				  "} = "}),
			    emit({"?RT_BER:encode_open_type(",{curr,tmpBytes},
				  ",",{asis,Tag},"),",nl}),
			    emit(IndDeep),
			    emit({"{",{next,tmpBytes},", ",{curr,tmpLen},"}"})
		    end;
		_ ->
		    throw({asn1,{'internal error'}})
	    end;
% 	#type{constraint=[{tableconstraint_info,_}],
% 	      def={objectfield,PrimFieldName1,PFNList}} ->
	{{#'ObjectClassFieldType'{type={objectfield,PrimFieldName1,
					PFNList}},_},
	 {componentrelation,_,_}} ->
	    %% this is when the dotted list in the FieldName has more
	    %% than one element
	    {_LeadingAttrName,Fun} = EncObj,
	    emit({"?RT_BER:encode_open_type(",Fun,"(",{asis,PrimFieldName1},
		  ", ",Element,", ",{asis,PFNList},"),",{asis,Tag},")"});
	_ ->
	    case WhatKind of
		{primitive,bif} ->
		    EncType =
			case Type#type.def of
			    #'ObjectClassFieldType'{
					     type={fixedtypevaluefield,
						   _,Btype}} ->
				Btype;
			    _ ->
				Type
			end,
		    asn1ct_gen_ber:gen_encode_prim(ber,EncType,{asis,Tag},
						   Element);
		{notype,_} ->
		    emit({"'enc_",InnerType,"'(",Element,", ",{asis,Tag},")"});
		'ASN1_OPEN_TYPE' ->
		    asn1ct_gen_ber:gen_encode_prim(ber,Type#type{def='ASN1_OPEN_TYPE'},{asis,Tag},Element);
		_ ->
		    {EncFunName, _, _} =
			mkfuncname(TopType,Cname,WhatKind,enc),
		    case {WhatKind,Type#type.tablecinf,EncObj} of
			{{constructed,bif},[{objfun,_}|_R],{_,Fun}} ->
			    emit([EncFunName,"(",Element,", ",{asis,Tag},
				  ", ",Fun,")"]);
			_ ->
			    emit([EncFunName,"(",Element,", ",{asis,Tag},")"])
		    end
	    end
    end,
    case OptOrMand of
	mandatory -> true;
	_ ->
	    emit({nl,indent(7),"end"})
    end.



gen_optormand_case(mandatory,_,_,_,_,_,_, _) ->
    ok;
gen_optormand_case('OPTIONAL',Erules,_,_,_,_,_,Element) ->
    emit({" case ",Element," of",nl}),
    emit({indent(9),"asn1_NOVALUE -> {",
	  empty_lb(Erules),",0};",nl}),
    emit({indent(9),"_ ->",nl,indent(12)});
gen_optormand_case({'DEFAULT',DefaultValue},Erules,TopType,Cname,Type,
		   InnerType,WhatKind,Element) ->
    CurrMod = get(currmod),
    case catch lists:member(der,get(encoding_options)) of
	true ->
	    emit(" case catch "),
	    asn1ct_gen:gen_check_call(TopType,Cname,Type,InnerType,
				      WhatKind,{asis,DefaultValue},
				      Element),
	    emit({" of",nl}),
	    emit({indent(12),"true -> {[],0};",nl});
	_ ->
	    emit({" case ",Element," of",nl}),
	    emit({indent(9),"asn1_DEFAULT -> {",
		  empty_lb(Erules),
		  ",0};",nl}),
	    case DefaultValue of
		#'Externalvaluereference'{module=CurrMod,
					  value=V} ->
		    emit({indent(9),"?",{asis,V}," -> {",
			  empty_lb(Erules),",0};",nl});
		_ ->
		    emit({indent(9),{asis,
				     DefaultValue}," -> {",
			  empty_lb(Erules),",0};",nl})
	    end
    end,
    emit({indent(9),"_ ->",nl,indent(12)}).




gen_dec_line_sof(_Erules,TopType,Cname,Type,ObjFun) ->

    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- Type#type.tag],
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} ->
	    asn1ct_name:delete(len),

	    asn1ct_name:new(len),
	    emit(["fun(FBytes,_,_)->",nl]),
	    EncType = case Type#type.def of
			    #'ObjectClassFieldType'{
					     type={fixedtypevaluefield,
						   _,Btype}} ->
				Btype;
			    _ ->
				Type
		      end,
	    asn1ct_gen_ber:gen_dec_prim(ber,EncType,"FBytes",Tag,
					[],no_length,?PRIMITIVE,
					mandatory),
	    emit([nl,"end, []"]);
	_ ->
	    case ObjFun of
		[] ->
		    {DecFunName, _, _} =
			mkfunname(TopType,Cname,WhatKind,dec,3),
		    emit([DecFunName,", ",{asis,Tag}]);
		_ ->
		    {DecFunName, _, _} =
			mkfunname(TopType,Cname,WhatKind,dec,4),
		    emit([DecFunName,", ",{asis,Tag},", ObjFun"])
	    end
    end.


gen_dec_line(Erules,TopType,Cname,CTags,Type,OptOrMand,DecObjInf)  ->
    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- Type#type.tag],
    InnerType =
	case Type#type.def of
	    #'ObjectClassFieldType'{type=OCFTType} ->
		OCFTType;
	    _ ->
		asn1ct_gen:get_inner(Type#type.def)
	end,
    PostpDec =
	case OptOrMand of
	    mandatory ->
		gen_dec_call(InnerType,Erules,TopType,Cname,Type,
			     BytesVar,Tag,mandatory,", mandatory, ",
			     DecObjInf,OptOrMand);
	    _ -> %optional or default
		case {CTags,Erules} of
		    {[CTag],ber_bin} ->
			emit(["case ",{curr,bytes}," of",nl]),
			emit([match_tag(Erules,CTag)," ->",nl]),
			PostponedDec =
			    gen_dec_call(InnerType,Erules,TopType,Cname,Type,
					 BytesVar,Tag,mandatory,
					 ", opt_or_default, ",DecObjInf,
					 OptOrMand),
			emit([";",nl]),
			emit(["_ ->",nl]),
			case OptOrMand of
			    {'DEFAULT', Def} ->
				emit(["{",{asis,Def},",",
				      BytesVar,", 0 }",nl]);
			    'OPTIONAL' ->
				emit(["{ asn1_NOVALUE, ",
				      BytesVar,", 0 }",nl])
			end,
			emit("end"),
			PostponedDec;
		    _ ->
			emit("case (catch "),
			PostponedDec =
			    gen_dec_call(InnerType,Erules,TopType,Cname,Type,
					 BytesVar,Tag,OptOrMand,
					 ", opt_or_default, ",DecObjInf,
					 OptOrMand),
			emit([") of",nl]),
			case OptOrMand of
			    {'DEFAULT', Def} ->
				emit(["{'EXIT',{error,{asn1,{no_optional_tag,_}}}}",
				      " -> {",{asis,Def},",",
				      BytesVar,", 0 };",nl]);
			    'OPTIONAL' ->
				emit(["{'EXIT',{error,{asn1,{no_optional_tag,_}}}}",
				      " -> { asn1_NOVALUE, ",
				      BytesVar,", 0 };",nl])
			end,
			asn1ct_name:new(casetmp),
			emit([{curr,casetmp},"-> ",{curr,casetmp},nl,"end"]),
			PostponedDec
		end
	end,
    case DecObjInf of
	{Cname,ObjSet} -> % this must be the component were an object is
	    %% chosen from the object set according to the table
	    %% constraint.
	    {[{ObjSet,Cname,asn1ct_gen:mk_var(asn1ct_name:curr(term))}],
	     PostpDec};
	_  -> {[],PostpDec}
    end.


gen_dec_call({typefield,_},Erules,_,_,Type,_,Tag,_,_,false,_) ->
    %%  this in case of a choice with typefield components
    asn1ct_name:new(reason),
    {FirstPFName,RestPFName} =
% 	asn1ct_gen:get_constraint(Type#type.constraint,
% 				  tableconstraint_info),
	(Type#type.def)#'ObjectClassFieldType'.fieldname,
    emit([nl,indent(6),"begin",nl]),
    emit([indent(9),"{OpenDec,TmpRest,TmpRbCho} =",nl,indent(12),
	  "?RT_BER:decode_open_type(",Erules,",",{curr,bytes},",",
	  {asis,Tag},"),",nl]),
    emit([indent(9),"case (catch ObjFun(",{asis,FirstPFName},
	  ", OpenDec, [], ",{asis,RestPFName},
	  ")) of", nl]),%% ??? What about Tag
    emit([indent(12),"{'EXIT',",{curr,reason},"} ->",nl]),
%%    emit({indent(15),"throw({runtime_error,{'Type not ",
%%	  "compatible with tableconstraint', OpenDec}});",nl}),
    emit([indent(15),"exit({'Type not ",
	  "compatible with table constraint', ",{curr,reason},"});",nl]),
    emit([indent(12),"{TmpDec,_ ,_} ->",nl]),
    emit([indent(15),"{TmpDec, TmpRest, TmpRbCho}",nl]),
    emit([indent(9),"end",nl,indent(6),"end",nl]),
    [];
gen_dec_call({typefield,_},_Erules,_,Cname,Type,_BytesVar,Tag,_,_,
	     _DecObjInf,OptOrMandComp) ->
    emit(["?RT_BER:decode_open_type(",{curr,bytes},",",{asis,Tag},")"]),
    RefedFieldName =
	(Type#type.def)#'ObjectClassFieldType'.fieldname,
% 	asn1ct_gen:get_constraint(Type#type.constraint,
% 				  tableconstraint_info),
    [{Cname,RefedFieldName,
      asn1ct_gen:mk_var(asn1ct_name:curr(term)),
%      asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),[],OptOrMandComp}];
      asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),Tag,OptOrMandComp}];
gen_dec_call({objectfield,PrimFieldName,PFNList},_Erules,_,Cname,_,_,Tag,_,_,_,
	     OptOrMandComp) ->
    emit(["?RT_BER:decode_open_type(",{curr,bytes},",",{asis,Tag},")"]),
    [{Cname,{PrimFieldName,PFNList},
      asn1ct_gen:mk_var(asn1ct_name:curr(term)),
%      asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),[],OptOrMandComp}];
      asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),Tag,OptOrMandComp}];
gen_dec_call(InnerType,Erules,TopType,Cname,Type,BytesVar,Tag,PrimOptOrMand,
	     OptOrMand,DecObjInf,_) ->
    WhatKind = asn1ct_gen:type(InnerType),
    gen_dec_call1(WhatKind,InnerType,Erules,TopType,Cname,Type,BytesVar,Tag,
		  PrimOptOrMand,OptOrMand),
    case DecObjInf of
	{Cname,{_,OSet,UniqueFName,ValIndex}} ->
	    Term = asn1ct_gen:mk_var(asn1ct_name:curr(term)),
	    ValueMatch = value_match(ValIndex,Term),
	    emit({",",nl,"ObjFun = 'getdec_",OSet,"'(",
%		  {asis,UniqueFName},", ",{curr,term},")"});
		  {asis,UniqueFName},", ",ValueMatch,")"});
	_ ->
	    ok
    end,
    [].
gen_dec_call1({primitive,bif},InnerType,Erules,_,_,Type,BytesVar,
	      Tag,OptOrMand,_) ->
    case InnerType of
	{fixedtypevaluefield,_,Btype} ->
	    asn1ct_gen_ber:gen_dec_prim(Erules,Btype,BytesVar,Tag,[],no_length,
					?PRIMITIVE,OptOrMand);
	_ ->
	    asn1ct_gen_ber:gen_dec_prim(Erules,Type,BytesVar,Tag,[],no_length,
					?PRIMITIVE,OptOrMand)
    end;
gen_dec_call1('ASN1_OPEN_TYPE',_InnerType,Erules,_,_,Type,BytesVar,
	      Tag,OptOrMand,_) ->
    asn1ct_gen_ber:gen_dec_prim(Erules,Type#type{def='ASN1_OPEN_TYPE'},
				BytesVar,Tag,[],no_length,
				?PRIMITIVE,OptOrMand);
gen_dec_call1(WhatKind,_,_Erules,TopType,Cname,Type,_,Tag,_,OptOrMand) ->
    {DecFunName,_,_} =
	mkfuncname(TopType,Cname,WhatKind,dec),
    case {WhatKind,Type#type.tablecinf} of
	{{constructed,bif},[{objfun,_}|_R]} ->
	    emit({DecFunName,"(",{curr,bytes},OptOrMand,{asis,Tag},", ObjFun)"});
	_ ->
	    emit({DecFunName,"(",{curr,bytes},OptOrMand,{asis,Tag},")"})
    end.


%%------------------------------------------------------
%% General and special help functions (not exported)
%%------------------------------------------------------


indent(N) ->
    lists:duplicate(N,32). % 32 = space


mkvlist([H,T1|T], Sep) -> % Sep is a string e.g ", " or "+ "
    emit([{var,H},Sep]),
    mkvlist([T1|T], Sep);
mkvlist([H|T], Sep) ->
    emit([{var,H}]),
    mkvlist(T, Sep);
mkvlist([], _) ->
    true.

mkvlist(L) ->
    mkvlist(L,", ").

mkvplus(L) ->
    mkvlist(L," + ").

extensible(CompList) when list(CompList) ->
    noext;
extensible({RootList,ExtList}) ->
    {ext,length(RootList)+1,length(ExtList)}.


print_attribute_comment(InnerType,Pos,Prop) ->
    CommentLine = "%%-------------------------------------------------",
    emit([nl,CommentLine]),
    case InnerType of
	{typereference,_,Name} ->
	    emit([nl,"%% attribute number ",Pos," with type ",Name]);
	{'Externaltypereference',_,XModule,Name} ->
	    emit([nl,"%% attribute number ",Pos,"   External ",XModule,":",Name]);
	_ ->
	    emit([nl,"%% attribute number ",Pos," with type ",InnerType])
    end,
    case Prop of
	mandatory ->
	    continue;
	{'DEFAULT', Def} ->
	    emit([" DEFAULT = ",{asis,Def}]);
	'OPTIONAL' ->
	    emit([" OPTIONAL"])
    end,
    emit([nl,CommentLine,nl]).


mkfuncname(TopType,Cname,WhatKind,DecOrEnc) ->
    CurrMod = get(currmod),
    case WhatKind of
	#'Externaltypereference'{module=CurrMod,type=EType} ->
	    F = lists:concat(["'",DecOrEnc,"_",EType,"'"]),
	    {F, "?MODULE", F};
	#'Externaltypereference'{module=Mod,type=EType} ->
	    {lists:concat(["'",Mod,"':'",DecOrEnc,"_",EType,"'"]),Mod,
	     lists:concat(["'",DecOrEnc,"_",EType,"'"])};
	{constructed,bif} ->
	    F = lists:concat(["'",DecOrEnc,"_",asn1ct_gen:list2name([Cname|TopType]),"'"]),
	    {F, "?MODULE", F}
    end.

mkfunname(TopType,Cname,WhatKind,DecOrEnc,Arity) ->
    CurrMod = get(currmod),
    case WhatKind of
	#'Externaltypereference'{module=CurrMod,type=EType} ->
	    F = lists:concat(["fun '",DecOrEnc,"_",EType,"'/",Arity]),
	    {F, "?MODULE", F};
	#'Externaltypereference'{module=Mod,type=EType} ->
	    {lists:concat(["{'",Mod,"','",DecOrEnc,"_",EType,"'}"]),Mod,
	     lists:concat(["'",DecOrEnc,"_",EType,"'"])};
	{constructed,bif} ->
	    F =
		lists:concat(["fun '",DecOrEnc,"_",
			      asn1ct_gen:list2name([Cname|TopType]),"'/",
			      Arity]),
	    {F, "?MODULE", F}
    end.

empty_lb(ber) ->
    "[]";
empty_lb(ber_bin) ->
    "<<>>".

rtmod(ber) ->
    list_to_atom(?RT_BER);
rtmod(ber_bin) ->
    list_to_atom(?RT_BER_BIN).

indefend_match(ber,used_var) ->
    "[0,0|R]";
indefend_match(ber,unused_var) ->
    "[0,0|_R]";
indefend_match(ber_bin,used_var) ->
    "<<0,0,R/binary>>";
indefend_match(ber_bin,unused_var) ->
    "<<0,0,_R/binary>>".

notice_value_match() ->
    Module = get(currmod),
    put(value_match,{true,Module}).

value_match(Index,Value) when atom(Value) ->
    value_match(Index,atom_to_list(Value));
value_match([],Value) ->
    Value;
value_match([{VI,_Cname}|VIs],Value) ->
    value_match1(Value,VIs,lists:concat(["element(",VI,","]),1).
value_match1(Value,[],Acc,Depth) ->
    Acc ++ Value ++ lists:concat(lists:duplicate(Depth,")"));
value_match1(Value,[{VI,_Cname}|VIs],Acc,Depth) ->
    value_match1(Value,VIs,Acc++lists:concat(["element(",VI,","]),Depth+1).
