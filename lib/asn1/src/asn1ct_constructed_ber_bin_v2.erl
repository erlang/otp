%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2017. All Rights Reserved.
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
-module(asn1ct_constructed_ber_bin_v2).

-export([gen_encode_sequence/3]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/3]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/4]).
-export([gen_decode_sof/4]).
-export([gen_encode_choice/3]).
-export([gen_decode_choice/3]).


-include("asn1_records.hrl").

-import(asn1ct_gen, [emit/1,get_record_name_prefix/1]).

-define(ASN1CT_GEN_BER,asn1ct_gen_ber_bin_v2).

%% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).
-define(APPLICATION, 16#40).
-define(CONTEXT,     16#80).
-define(PRIVATE,     16#C0).

%% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).




%%===============================================================================
%%===============================================================================
%%===============================================================================
%%  Encode/decode SEQUENCE (and SET)
%%===============================================================================
%%===============================================================================
%%===============================================================================

gen_encode_sequence(Gen, Typename, #type{}=D) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),
    
    %% if EXTERNAL type the input value must be transformed to 
    %% ASN1 1990 format
    ValName = 
	case Typename of
	    ['EXTERNAL'] ->
                Tr = case Gen of
                         #gen{pack=record} -> transform_to_EXTERNAL1990;
                         #gen{pack=map} -> transform_to_EXTERNAL1990_maps
                     end,
		emit([indent(4),"NewVal = ",
		      {call,ext,Tr,["Val"]},
		      com,nl]),
		"NewVal";
	    _ ->
	    "Val"
	end,

    {SeqOrSet,TableConsInfo,CompList0} =
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} -> 
		{'SEQUENCE',TCI,CL};
	    #'SET'{tablecinf=TCI,components=CL} -> 
		{'SET',TCI,CL}
	end,
    %% filter away extensionAdditiongroup markers
    CompList = filter_complist(CompList0),
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl1,El,Rl2} -> Rl1 ++ El ++ Rl2;
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,

    enc_match_input(Gen, ValName, CompList1),

    EncObj =
	case TableConsInfo of
	    #simpletableattributes{usedclassfield=Used,
				   uniqueclassfield=Unique} when Used /= Unique ->
		false;
	    %% ObjectSet, name of the object set in constraints
	    #simpletableattributes{objectsetname=ObjectSetRef,
				   c_name=AttrN,
				   c_index=N,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValueIndex} -> %% N is index of attribute that determines constraint
		{ObjSetMod,ObjSetName} = ObjectSetRef,
		OSDef = asn1_db:dbget(ObjSetMod, ObjSetName),
		case (OSDef#typedef.typespec)#'ObjectSet'.gen of
		    true ->
			ObjectEncode = 
			    asn1ct_gen:un_hyphen_var(lists:concat(['Obj',
								   AttrN])),
			emit([ObjectEncode," = ",nl,
			      "   ",{asis,ObjSetMod},":'getenc_",ObjSetName,
			      "'("]),
			ValueMatch = value_match(Gen, ValueIndex,
						 lists:concat(["Cindex",N])),
			emit([indent(35),ValueMatch,"),",nl]),
			{AttrN,ObjectEncode};
		    _ ->
			false
		end;
	    _ ->
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

    gen_enc_sequence_call(Gen, Typename, CompList1, 1, Ext, EncObj),

    emit([nl,"   BytesSoFar = "]),
    case SeqOrSet of
	'SET' when (D#type.def)#'SET'.sorted == dynamic ->
	    asn1ct_func:need({ber,dynamicsort_SET_components,1}),
	    emit("dynamicsort_SET_components(["),
	    mkvlist(asn1ct_name:all(encBytes)),
	    emit(["]),",nl]);
	_ ->
	    emit("["),
	    mkvlist(asn1ct_name:all(encBytes)),
	    emit(["],",nl])
    end,
    emit("LenSoFar = "),
    case asn1ct_name:all(encLen) of
	[] -> emit("0");
	AllLengths -> 
	    mkvplus(AllLengths)
    end,
    emit([",",nl]),
    call(encode_tags, ["TagIn","BytesSoFar","LenSoFar"]),
    emit([".",nl]).

enc_match_input(#gen{pack=record}, ValName, CompList) ->
    Len = length(CompList),
    Vars = [lists:concat(["Cindex",N]) || N <- lists:seq(1, Len)],
    RecordName = "_",
    emit(["{",lists:join(",", [RecordName|Vars]),"} = ",ValName,com,nl]);
enc_match_input(#gen{pack=map}, ValName, CompList) ->
    Len = length(CompList),
    Vars = [lists:concat(["Cindex",N]) || N <- lists:seq(1, Len)],
    Zipped = lists:zip(CompList, Vars),
    M = [[{asis,Name},":=",Var] ||
            {#'ComponentType'{prop=mandatory,name=Name},Var} <- Zipped],
    case M of
        [] ->
            ok;
        [_|_] ->
            emit(["#{",lists:join(",", M),"} = ",ValName,com,nl])
    end,
    Os0 = [{Name,Var} ||
              {#'ComponentType'{prop=Prop,name=Name},Var} <- Zipped,
              Prop =/= mandatory],
    F = fun({Name,Var}) ->
                [Var," = case ",ValName," of\n"
                 "  #{",{asis,Name},":=",Var,"_0} -> ",
                 Var,"_0;\n"
                 "  _ -> ",atom_to_list(?MISSING_IN_MAP),"\n"
                 "end"]
        end,
    emit(lists:join(",\n", [F(E) || E <- Os0]++[[]])).

gen_decode_sequence(Gen, Typename, #type{}=D) ->
    asn1ct_name:start(),
    asn1ct_name:new(tag),
    #'SEQUENCE'{tablecinf=TableConsInfo,components=CList0} = D#type.def,

    %% filter away extensionAdditiongroup markers
    CList = filter_complist(CList0),
    Ext = extensible(CList),
    {CompList,CompList2} = 
	case CList of
	    {Rl1,El,Rl2} -> {Rl1 ++ El ++ Rl2, CList};
	    {Rl,El}  -> {Rl ++ El, Rl ++ El};
	    _ -> {CList, CList}
	end,

    emit(["   %%-------------------------------------------------",nl]),
    emit(["   %% decode tag and length ",nl]),
    emit(["   %%-------------------------------------------------",nl]),

    asn1ct_name:new(tlv),
    case CompList of
	[] -> % empty sequence
	    true;
	_ ->
	    emit([{curr,tlv}," = "])
    end,
    call(match_tags, [{prev,tlv},"TagIn"]),
    emit([com,nl]),
    asn1ct_name:new(tlv),
    asn1ct_name:new(v),

    {DecObjInf,ValueIndex} =
	case TableConsInfo of
	    #simpletableattributes{objectsetname=ObjectSetRef,
				   c_name=AttrN,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValIndex} ->
		F = fun(#'ComponentType'{typespec=CT})->
			    case {asn1ct_gen:get_constraint(CT#type.constraint,componentrelation),CT#type.tablecinf} of
				{no,[{objfun,_}|_]} -> true;
				_ -> false
			    end
		    end,
		case lists:any(F,CompList) of
		    true -> % when component relation constraint establish
			%% relation from a component to another components
			%% subtype component
			{{AttrN,{deep,ObjectSetRef,UniqueFieldName,ValIndex}},
			 ValIndex};
		    false ->
			{{AttrN,ObjectSetRef},ValIndex}
		end;
	    _ ->
		{false,false}
	end,
    RecordName0 = lists:concat([get_record_name_prefix(Gen),
                                asn1ct_gen:list2rname(Typename)]),
    RecordName = list_to_atom(RecordName0),
    case gen_dec_sequence_call(Gen, Typename, CompList2, Ext, DecObjInf) of
	no_terms ->                           % an empty sequence
	    asn1ct_name:new(rb),
            case Gen of
                #gen{pack=record} ->
                    emit([nl,nl,
                          "   {'",RecordName,"'}.",nl,nl]);
                #gen{pack=map} ->
                    emit([nl,nl,
                          "   #{}.",nl,nl])
            end;
	{LeadingAttrTerm,PostponedDecArgs} ->
	    emit([nl]),
	    case {LeadingAttrTerm,PostponedDecArgs} of
		{[],[]} ->
		    ok;
		{_,[]} ->
		    ok;
		{[{ObjSetRef,LeadingAttr,Term}],PostponedDecArgs} ->
		    DecObj = asn1ct_gen:un_hyphen_var(lists:concat(['DecObj',LeadingAttr,Term])),
		    ValueMatch = value_match(Gen, ValueIndex,Term),
		    {ObjSetMod,ObjSetName} = ObjSetRef,
		    emit([DecObj," =",nl,
			  "   ",{asis,ObjSetMod},":'getdec_",ObjSetName,"'(",
			  ValueMatch,"),",nl]),
		    gen_dec_postponed_decs(DecObj,PostponedDecArgs)
	    end,
	    %% return value as record
	    case Ext of
		{ext,_,_} -> 
		    emit(["case ",{prev,tlv}," of [] -> true; _ -> true end, % ... extra fields skipped",nl]);
		_ ->
                    %% noext | extensible
		    emit(["case ",{prev,tlv}," of",nl,
			  "[] -> true;",
			  "_ -> exit({error,{asn1, {unexpected,",{prev,tlv},
			  "}}}) % extra fields not allowed",nl,
			  "end,",nl])
	    end,
	    asn1ct_name:new(rb),
            gen_dec_pack(Gen, RecordName, Typename, CompList),
            emit([".",nl])
    end.

gen_dec_pack(Gen, RecordName, Typename, CompList) ->
    case Typename of
	['EXTERNAL'] ->
            dec_external(Gen, RecordName);
	_ ->
            asn1ct_name:new(res),
            gen_dec_do_pack(Gen, RecordName, CompList),
            emit([com,nl,
                  {curr,res}])
    end.

dec_external(#gen{pack=record}, RecordName) ->
    All = [{var,Term} || Term <- asn1ct_name:all(term)],
    Record = [{asis,RecordName}|All],
    emit(["OldFormat={",lists:join(",", Record),"},",nl,
          {call,ext,transform_to_EXTERNAL1994,
           ["OldFormat"]}]);
dec_external(#gen{pack=map}, _RecordName) ->
    Vars = asn1ct_name:all(term),
    Names = ['direct-reference','indirect-reference',
             'data-value-descriptor',encoding],
    Zipped = lists:zip(Names, Vars),
    MapInit = lists:join(",", [["'",N,"'=>",{var,V}] || {N,V} <- Zipped]),
    emit(["OldFormat = #{",MapInit,"}",com,nl,
          "ASN11994Format =",nl,
          {call,ext,transform_to_EXTERNAL1994_maps,
           ["OldFormat"]}]).

gen_dec_do_pack(#gen{pack=record}, RecordName, _CompList) ->
    All = asn1ct_name:all(term),
    L = [{asis,RecordName}|[{var,Var} || Var <- All]],
    emit([{curr,res}," = {",lists:join(",", L),"}"]);
gen_dec_do_pack(#gen{pack=map}, _, CompList) ->
    Zipped = lists:zip(CompList, asn1ct_name:all(term)),
    PF = fun({#'ComponentType'{prop='OPTIONAL'},_}) -> false;
            ({_,_}) -> true
         end,
    {Mandatory,Optional} = lists:partition(PF, Zipped),
    L = [[{asis,Name},"=>",{var,Var}] ||
            {#'ComponentType'{name=Name},Var} <- Mandatory],
    emit([{curr,res}," = #{",lists:join(",", L),"}"]),
    gen_dec_map_optional(Optional).

gen_dec_map_optional([{#'ComponentType'{name=Name},Var}|T]) ->
    asn1ct_name:new(res),
    emit([com,nl,
          {curr,res}," = case ",{var,Var}," of",nl,
          "  asn1_NOVALUE -> ",{prev,res},";",nl,
          "  _ -> ",{prev,res},"#{",{asis,Name},"=>",{var,Var},"}",nl,
          "end"]),
    gen_dec_map_optional(T);
gen_dec_map_optional([]) ->
    ok.

gen_dec_postponed_decs(_,[]) ->
    emit(nl);
gen_dec_postponed_decs(DecObj,[{_Cname,{FirstPFN,PFNList},Term,
				TmpTerm,_Tag,OptOrMand}|Rest]) ->

    asn1ct_name:new(tmpterm),
    asn1ct_name:new(reason),
    asn1ct_name:new(tmptlv),

    emit([Term," = ",nl]),
    N = case OptOrMand of
	    mandatory -> 0;
	    'OPTIONAL' -> 
		emit_opt_or_mand_check(asn1_NOVALUE,TmpTerm),
		6;
	    {'DEFAULT',Val} ->
		emit_opt_or_mand_check(Val,TmpTerm),
		6
	end,
    emit([indent(N+3),"case (catch ",DecObj,"(",{asis,FirstPFN},
	  ", ",TmpTerm,", ",{asis,PFNList},")) of",nl]),
    emit([indent(N+6),"{'EXIT', ",{curr,reason},"} ->",nl]),
    emit([indent(N+9),"exit({'Type not compatible with table constraint',",
	  {curr,reason},"});",nl]),
    emit([indent(N+6),{curr,tmpterm}," ->",nl]),
    emit([indent(N+9),{curr,tmpterm},nl]),
    
    case OptOrMand of 
	mandatory -> emit([indent(N+3),"end,",nl]);
	_ ->
	    emit([indent(N+3),"end",nl,
		  indent(3),"end,",nl])
    end,
    gen_dec_postponed_decs(DecObj,Rest).

emit_opt_or_mand_check(Value,TmpTerm) ->
    emit([indent(3),"case ",TmpTerm," of",nl,
	  indent(6),{asis,Value}," ->",{asis,Value},";",nl,
	  indent(6),"_ ->",nl]).

%%============================================================================
%%  Encode/decode SET
%%
%%============================================================================

gen_encode_set(Erules,Typename,D) when is_record(D,type) ->
    gen_encode_sequence(Erules,Typename,D).

gen_decode_set(Gen, Typename, #type{}=D) ->
    asn1ct_name:start(),
%%    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    #'SET'{tablecinf=TableConsInfo,components=TCompList0} = D#type.def,
    %% filter away extensionAdditiongroup markers
    TCompList = filter_complist(TCompList0),
    Ext = extensible(TCompList),
    ToOptional = fun(mandatory) ->
			 'OPTIONAL';
		    (X) -> X
		 end,
    CompList = case TCompList of
		   {Rl1,El,Rl2} -> Rl1 ++ [X#'ComponentType'{prop=ToOptional(Y)}||X = #'ComponentType'{prop=Y}<-El] ++ Rl2;
		   {Rl,El} -> Rl ++ El;
		   _ -> TCompList
	       end,

    %% asn1ct_name:clear(),
    asn1ct_name:new(tlv),
    case CompList of
	[] -> % empty sequence
	    true;
	_ ->
	    emit([{curr,tlv}," = "])
    end,
    call(match_tags, [{prev,tlv},"TagIn"]),
    emit([com,nl]),
    asn1ct_name:new(v),


    {DecObjInf,ValueIndex} =
	case TableConsInfo of
	    #simpletableattributes{objectsetname=ObjectSetRef,
				   c_name=AttrN,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValIndex} ->
		F = fun(#'ComponentType'{typespec=CT})->
			    case {asn1ct_gen:get_constraint(CT#type.constraint,
							    componentrelation),
				  CT#type.tablecinf} of
				{no,[{objfun,_}|_]} -> true;
				_ -> false
			    end
		    end,
		case lists:any(F,CompList) of
		    true ->
                        %% when component relation constraint establish
			%% relation from a component to another components
			%% subtype component
			{{AttrN,{deep,ObjectSetRef,UniqueFieldName,ValIndex}},
			 ValIndex};
		    false ->
			{{AttrN,ObjectSetRef},ValIndex}
		end;
	    _ ->
		{false,false}
	end,

    case CompList of
	[] -> % empty set
	    true;
	_ ->
	    emit(["SetFun = fun(FunTlv) ->", nl]),
	    emit(["case FunTlv of ",nl]),
	    NextNum = gen_dec_set_cases(Gen, Typename, CompList, 1),
	    emit([indent(6), {curr,else}," -> ",nl,
		  indent(9),"{",NextNum,", ",{curr,else},"}",nl]),
	    emit([indent(3),"end",nl]),
	    emit([indent(3),"end,",nl]),

	    emit(["PositionList = [SetFun(TempTlv)|| TempTlv <- ",{curr,tlv},"],",nl]),
	    asn1ct_name:new(tlv),
	    emit([{curr,tlv}," = [Stlv || {_,Stlv} <- lists:sort(PositionList)],",nl]),
	    asn1ct_name:new(tlv)

    end,
    RecordName0 = lists:concat([get_record_name_prefix(Gen),
                                asn1ct_gen:list2rname(Typename)]),
    RecordName = list_to_atom(RecordName0),
    case gen_dec_sequence_call(Gen, Typename, CompList, Ext, DecObjInf) of
	no_terms ->                           % an empty SET
            case Gen of
                #gen{pack=record} ->
                    emit([nl,nl,"   {'",RecordName,"'}.",nl,nl]);
                #gen{pack=map} ->
                    emit([nl,nl,"   #{}.",nl,nl])
            end;
	{LeadingAttrTerm,PostponedDecArgs} ->
	    emit([nl]),
	    case {LeadingAttrTerm,PostponedDecArgs} of
		{[],[]} ->
		    ok;
		{_,[]} ->
		    ok;
		{[{ObjSetRef,LeadingAttr,Term}],PostponedDecArgs} ->
		    DecObj = asn1ct_gen:un_hyphen_var(lists:concat(['DecObj',LeadingAttr,Term])),
		    ValueMatch = value_match(Gen, ValueIndex, Term),
		    {ObjSetMod,ObjSetName} = ObjSetRef,
		    emit([DecObj," =",nl,
			  "   ",{asis,ObjSetMod},":'getdec_",ObjSetName,"'(",
			  ValueMatch,"),",nl]),
		    gen_dec_postponed_decs(DecObj,PostponedDecArgs)
	    end,
	    %% return value as record
	    case Ext of
		Extnsn when Extnsn =/= noext -> 
		    emit(["case ",{prev,tlv}," of [] -> true; _ -> true end, % ... extra fields skipped",nl]);
		noext -> 
		    emit(["case ",{prev,tlv}," of",nl,
			  "[] -> true;",
			  "_ -> exit({error,{asn1, {unexpected,",{prev,tlv},
			  "}}}) % extra fields not allowed",nl,
			  "end,",nl])
	    end,
            gen_dec_pack(Gen, RecordName, Typename, CompList),
	    emit([".",nl])
    end.


%%===============================================================================
%%===============================================================================
%%===============================================================================
%%  Encode/decode SEQUENCE OF and SET OF
%%===============================================================================
%%===============================================================================
%%===============================================================================

gen_encode_sof(Erules,Typename,_InnerTypename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, Cont} = D#type.def,

    Objfun = case D#type.tablecinf of
		 [{objfun,_}|_R] ->
		     ", ObjFun";
		 _ ->
		     ""
	     end,

    emit(["   {EncBytes,EncLen} = 'enc_",asn1ct_gen:list2name(Typename),
	  "_components'(Val",Objfun,",[],0),",nl]),

    emit(["   ",{call,ber,encode_tags,["TagIn","EncBytes","EncLen"]},
	  ".",nl,nl]),

    gen_encode_sof_components(Erules,Typename,SeqOrSetOf,Cont).
    

gen_decode_sof(Erules,TypeName,_InnerTypeName,D) when is_record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, _TypeTag, Cont} = 
	case D#type.def of
	    {'SET OF',_Cont} -> {'SET OF','SET',_Cont};
	    {'SEQUENCE OF',_Cont} -> {'SEQUENCE OF','SEQUENCE',_Cont}
	end,
    TypeNameSuffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,Cont#type.def),
    
    emit(["   %%-------------------------------------------------",nl]),
    emit(["   %% decode tag and length ",nl]),
    emit(["   %%-------------------------------------------------",nl]),
    
    asn1ct_name:new(tlv),
    emit([{curr,tlv}," = ",
	  {call,ber,match_tags,[{prev,tlv},"TagIn"]},com,nl]),
    asn1ct_name:new(v),

    emit(["["]),

    InnerType = asn1ct_gen:get_inner(Cont#type.def),
    ContName = case asn1ct_gen:type(InnerType) of
		   Atom when is_atom(Atom) -> Atom;
		   _ -> TypeNameSuffix
	       end,
    gen_dec_line(Erules,TypeName,ContName,[],Cont,mandatory),
    emit([" || ",{curr,v}," <- ",{curr,tlv},"].",nl,nl,nl]).
    

gen_encode_sof_components(Gen, Typename, SeqOrSetOf, #type{}=Cont) ->
    {Objfun,Objfun_novar,EncObj} =
	case Cont#type.tablecinf of
	    [{objfun,_}|_R] ->
		{", ObjFun",", _",{no_attr,"ObjFun"}};
	    _ ->
		{"","",false}
	end,
    emit(["'enc_",asn1ct_gen:list2name(Typename),
	  "_components'([]",Objfun_novar,", AccBytes, AccLen) -> ",nl]),

    case {Gen,SeqOrSetOf} of
        {#gen{der=true},'SET OF'} ->
	    asn1ct_func:need({ber,dynamicsort_SETOF,1}),
	    emit([indent(3),
		  "{dynamicsort_SETOF(AccBytes),AccLen};",nl,nl]);
	{_,_} ->
	    emit([indent(3),"{lists:reverse(AccBytes),AccLen};",nl,nl])
    end,
    emit(["'enc_",asn1ct_gen:list2name(Typename),
	  "_components'([H|T]",Objfun,",AccBytes, AccLen) ->",nl]),
    TypeNameSuffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,Cont#type.def),
    gen_enc_line(Gen, Typename, TypeNameSuffix, Cont, "H", 3,
		 mandatory, EncObj),
    emit([",",nl]),
    emit([indent(3),"'enc_",asn1ct_gen:list2name(Typename),
	  "_components'(T",Objfun,","]), 
    emit(["[EncBytes|AccBytes], AccLen + EncLen).",nl,nl]).

%%============================================================================
%%  Encode/decode CHOICE
%%
%%============================================================================

gen_encode_choice(Erules,Typename,D) when is_record(D,type) ->
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl1,El,Rl2} -> Rl1 ++ El ++ Rl2;
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_enc_choice(Erules,Typename,ChoiceTag,CompList1,Ext),
    emit([nl,nl]).

gen_decode_choice(Erules,Typename,D) when is_record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl1,El,Rl2} -> Rl1 ++ El ++ Rl2;
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_dec_choice(Erules,Typename,ChoiceTag,CompList1,Ext),
    emit([".",nl]).


%%============================================================================
%%  Encode SEQUENCE
%%
%%============================================================================

gen_enc_sequence_call(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,textual_order=Order}|Rest],Pos,Ext,EncObj) ->
    asn1ct_name:new(encBytes),
    asn1ct_name:new(encLen),
    asn1ct_name:new(tmpBytes),
    asn1ct_name:new(tmpLen),
    CindexPos =
	case Order of
	    undefined ->
		Pos;
	    _ -> Order % der
	end,
    Element = 
	case TopType of
	    ['EXTERNAL'] ->
		io_lib:format("Cindex~w",[CindexPos]);
	    _ ->
		io_lib:format("Cindex~w",[CindexPos])
	end,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    print_attribute_comment(InnerType,Pos,Cname,Prop),
    gen_enc_line(Erules,TopType,Cname,Type,Element,3,Prop,EncObj),
    emit([com,nl]),
    gen_enc_sequence_call(Erules,TopType,Rest,Pos+1,Ext,EncObj);

gen_enc_sequence_call(_Erules,_TopType,[],_Num,_,_) ->
	true.

%%============================================================================
%%  Decode SEQUENCE
%%
%%============================================================================

gen_dec_sequence_call(Erules,TopType,CompList,Ext,DecObjInf)
  when is_list(CompList) ->
    gen_dec_sequence_call1(Erules,TopType, CompList, 1, Ext,DecObjInf,[],[]);
gen_dec_sequence_call(Erules,TopType,CompList,Ext,DecObjInf) ->
    gen_dec_sequence_call2(Erules,TopType, CompList, Ext,DecObjInf).


gen_dec_sequence_call1(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,tags=Tags}|Rest],Num,Ext,DecObjInf,LeadingAttrAcc,ArgsAcc) ->
    {LA,PostponedDec} = 
	gen_dec_component(Erules,TopType,Cname,Tags,Type,Num,Prop,
			  Ext,DecObjInf),
    emit([com,nl]),
    case Rest of
	[] ->
	    {LA ++ LeadingAttrAcc,PostponedDec ++ ArgsAcc};
	_ ->
	    asn1ct_name:new(bytes),
	    gen_dec_sequence_call1(Erules,TopType,Rest,Num+1,Ext,DecObjInf,
				   LA++LeadingAttrAcc,PostponedDec++ArgsAcc)
    end;

gen_dec_sequence_call1(_Erules,_TopType,[],1,_,_,_,_) ->
    no_terms;
gen_dec_sequence_call1(_, _, [], _Num, _, _, LA, PostponedDec) ->
    {LA, PostponedDec}.

gen_dec_sequence_call2(_Erules,_TopType, {[], [], []}, _Ext,_DecObjInf) ->
    no_terms;
gen_dec_sequence_call2(Erules,TopType,{Root1,EList,Root2},_Ext,DecObjInf) ->
    {LA,ArgsAcc} =
	case gen_dec_sequence_call1(Erules,TopType,Root1++EList,1,
				    extensible({Root1,EList}),DecObjInf,[],[]) of
	    no_terms ->
		{[],[]};
	    Res -> Res
	end,
    %% TagList is the tags of Root2 elements from the first up to and
    %% including the first mandatory element.
    TagList = get_root2_taglist(Root2,[]),
    emit([{curr,tlv}," = ",
	  {call,ber,skip_ExtensionAdditions,
	   [{prev,tlv},{asis,TagList}]},com,nl]),
    asn1ct_name:new(tlv),
    gen_dec_sequence_call1(Erules,TopType,Root2,
			   length(Root1)+length(EList),noext,
			   DecObjInf,LA,ArgsAcc).

%% Returns a list of tags of the elements in the component (second
%% root) list up to and including the first mandatory tag. See 24.6 in
%% X.680 (7/2002)
get_root2_taglist([],Acc) ->
    lists:reverse(Acc);
get_root2_taglist([#'ComponentType'{prop=Prop,typespec=Type}|Rest],Acc) ->
    FirstTag = fun([])->[];
		  ([H|_T])->(?ASN1CT_GEN_BER:decode_class(H#tag.class) bsl 10)
				+ H#tag.number
	       end(Type#type.tag),
    case Prop of
	mandatory ->
	    %% match_tags/ may be used
	    %% this is the last tag of interest -> return
	    lists:reverse([FirstTag|Acc]);
	_ ->
	    get_root2_taglist(Rest,[FirstTag|Acc])
    end.

    

%%----------------------------
%%SEQUENCE mandatory
%%----------------------------

gen_dec_component(Erules,TopType,Cname,CTags,Type,Pos,Prop,Ext,DecObjInf) ->
    InnerType = 
	case Type#type.def of
	    #'ObjectClassFieldType'{type=OCFTType} -> OCFTType;
	    _ -> asn1ct_gen:get_inner(Type#type.def)
	end,

    Prop1 = case {Prop,Ext} of
		{mandatory,{ext,Epos,_}} when Pos >= Epos ->
		    'OPTIONAL';
		_ ->
		    Prop
	    end,
    print_attribute_comment(InnerType,Pos,Cname,Prop1),
    asn1ct_name:new(term),
    emit_term_tlv(Prop1,InnerType,DecObjInf),
    asn1ct_name:new(rb),
    PostponedDec = 
	gen_dec_line(Erules,TopType,Cname,CTags,Type,Prop1,DecObjInf),
    asn1ct_name:new(v),
    asn1ct_name:new(tlv),
    asn1ct_name:new(form),
    PostponedDec.


emit_term_tlv({'DEFAULT',_},InnerType,DecObjInf) ->
    emit_term_tlv(opt_or_def,InnerType,DecObjInf);
emit_term_tlv('OPTIONAL',InnerType,DecObjInf) ->
    emit_term_tlv(opt_or_def,InnerType,DecObjInf);
emit_term_tlv(Prop,{typefield,_},DecObjInf) ->
    emit_term_tlv(Prop,type_or_object_field,DecObjInf);
emit_term_tlv(opt_or_def,type_or_object_field,NotFalse) 
  when NotFalse /= false ->
    asn1ct_name:new(tmpterm),
    emit(["{",{curr,tmpterm},",",{curr,tlv},"} = "]);
emit_term_tlv(opt_or_def,_,_) ->
    emit(["{",{curr,term},",",{curr,tlv},"} = "]);
emit_term_tlv(_,type_or_object_field,false) ->
     emit(["[",{curr,v},"|",{curr,tlv},"] = ",{prev,tlv},", ",nl,
	  {curr,term}," = "]);
emit_term_tlv(_,type_or_object_field,_) ->
    asn1ct_name:new(tmpterm),
    emit(["[",{curr,v},"|",{curr,tlv},"] = ",{prev,tlv},", ",nl]),
    emit([nl,"  ",{curr,tmpterm}," = "]);
emit_term_tlv(mandatory,_,_) ->
    emit(["[",{curr,v},"|",{curr,tlv},"] = ",{prev,tlv},", ",nl,
	  {curr,term}," = "]).


gen_dec_set_cases(_Erules,_TopType,[],Pos) ->
    Pos;
gen_dec_set_cases(Erules,TopType,[Comp|RestComps],Pos) ->
    Name = Comp#'ComponentType'.name,
    Type = Comp#'ComponentType'.typespec,
    CTags = Comp#'ComponentType'.tags,
    
    emit([indent(6),"%",Name,nl]),
    Tags = case Type#type.tag of
	       [] -> % this is a choice without explicit tag
		   [(?ASN1CT_GEN_BER:decode_class(T1class) bsl 10) + T1number|| 
		       {T1class,T1number} <- CTags];
               [FirstTag|_] ->
		   [(?ASN1CT_GEN_BER:decode_class(FirstTag#tag.class) bsl 10) + FirstTag#tag.number]
	   end,
    CaseFun = fun(TagList=[H|T],Fun,N) ->
		      Semicolon = case TagList of
				      [_Tag1,_|_] -> [";",nl];
				      _ -> ""
				  end,
		      emit(["TTlv = {",H,",_} ->",nl]),
		      emit([indent(4),"{",Pos,", TTlv}",Semicolon]),
		      Fun(T,Fun,N+1);
		 ([],_,0) ->
		      true;
		 ([],_,_) ->
		      emit([";",nl])
	      end,
    CaseFun(Tags,CaseFun,0),
    gen_dec_set_cases(Erules,TopType,RestComps,Pos+1).



%%---------------------------------------------
%%  Encode CHOICE
%%---------------------------------------------
%% for BER we currently do care (a little) if the choice has an EXTENSIONMARKER


gen_enc_choice(Erules,TopType,Tag,CompList,_Ext) ->
    gen_enc_choice1(Erules,TopType,Tag,CompList,_Ext).

gen_enc_choice1(Erules,TopType,_Tag,CompList,_Ext) ->
    asn1ct_name:clear(),
    emit(["   {EncBytes,EncLen} = case element(1,Val) of",nl]),
    gen_enc_choice2(Erules,TopType,CompList),
    emit([nl,"   end,",nl,nl]),
    call(encode_tags, ["TagIn","EncBytes","EncLen"]),
    emit([".",nl]).


gen_enc_choice2(Erules,TopType,[H1|T]) when is_record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit(["      ",{asis,Cname}," ->",nl]),
    {Encobj,Assign} =
	case {Type#type.def,asn1ct_gen:get_constraint(Type#type.constraint,
						      componentrelation)} of
	    {#'ObjectClassFieldType'{},{componentrelation,_,_}} ->
		asn1ct_name:new(tmpBytes),
		asn1ct_name:new(tmpLen),
		asn1ct_name:new(encBytes),
		asn1ct_name:new(encLen),
		Emit = ["{",{curr,tmpBytes},", _} = "],
		{{no_attr,"ObjFun"},Emit};
	    _ ->
		case Type#type.tablecinf of
		    [{objfun,_}] -> {{no_attr,"ObjFun"},[]};
		    _ -> {false,[]}
		end
	end,
    gen_enc_line(Erules,TopType,Cname,Type,"element(2,Val)",9,
		 mandatory,Assign,Encobj),
    case {Type#type.def,Encobj} of
	{#'ObjectClassFieldType'{},{no_attr,"ObjFun"}} ->
	    emit([",",nl,indent(9),"{",{curr,encBytes},", ",
		  {curr,encLen},"}"]);
	_ -> ok
    end,
    emit([";",nl]),
    case T of 
	[] ->
	    emit([indent(6), "Else -> ",nl,
		  indent(9),"exit({error,{asn1,{invalid_choice_type,Else}}})"]);
	_ ->
	    true
    end,
    gen_enc_choice2(Erules,TopType,T);

gen_enc_choice2(_Erules,_TopType,[])  ->
    true.




%%--------------------------------------------
%%  Decode CHOICE
%%--------------------------------------------

gen_dec_choice(Erules,TopType, _ChTag, CompList, Ext) ->
    asn1ct_name:clear(),
    asn1ct_name:new(tlv),
    emit([{curr,tlv}," = ",
	  {call,ber,match_tags,[{prev,tlv},"TagIn"]},com,nl]),
    asn1ct_name:new(tlv),
    asn1ct_name:new(v),
    emit(["case (case ",{prev,tlv},
	  " of [Ctemp",{prev,tlv},"] -> Ctemp",{prev,tlv},
	  "; _ -> ",{prev,tlv}," end)"," of",nl]),
    asn1ct_name:new(tagList),
    asn1ct_name:new(choTags),
    asn1ct_name:new(res),
    gen_dec_choice_cases(Erules,TopType,CompList),
    emit([indent(6), {curr,else}," -> ",nl]),
    case Ext of
	noext ->
	    emit([indent(9),"exit({error,{asn1,{invalid_choice_tag,",
		  {curr,else},"}}})",nl]);
	_ ->
	    emit([indent(9),"{asn1_ExtAlt,",
		  {call,ber,ber_encode,[{curr,else}]},"}",nl])
    end,
    emit([indent(3),"end",nl]),
    asn1ct_name:new(tag),
    asn1ct_name:new(else).

gen_dec_choice_cases(_Erules,_TopType, []) ->
    ok;
gen_dec_choice_cases(Erules,TopType, [H|T]) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    Prop = H#'ComponentType'.prop,
    Tags = Type#type.tag,
    Fcases  = fun([{T1class,T1number}|Tail],Fun) ->		      
		      emit([indent(4),{curr,v}," = {",
			    (?ASN1CT_GEN_BER:decode_class(T1class) bsl 10) +
			    T1number,",_} -> ",nl]),
		      emit([indent(8),"{",{asis,Cname},", "]),
		      gen_dec_line(Erules,TopType,Cname,[],Type,Prop),
		      emit(["};",nl,nl]),
		      Fun(Tail,Fun);
		 ([],_) ->
		      ok
	      end,
    emit([nl,"%% '",Cname,"'",nl]),
    case {Tags,asn1ct:get_gen_state_field(namelist)} of
	{[],_} -> % choice without explicit tags
	    Fcases(H#'ComponentType'.tags,Fcases);
	{[FirstT|_RestT],[{Cname,undecoded}|Names]} ->
	    DecTag=(?ASN1CT_GEN_BER:decode_class(FirstT#tag.class) bsl 10) +
		FirstT#tag.number,
	    asn1ct:add_generated_refed_func({[Cname|TopType],undecoded,
					     [DecTag],Type}),
	    asn1ct:update_gen_state(namelist,Names),
	    emit([indent(4),{curr,res}," = ",
		  match_tag(FirstT#tag.class, FirstT#tag.number),
		  " -> ",nl]),
	    emit([indent(8),"{",{asis,Cname},", {'",
		  asn1ct_gen:list2name([Cname|TopType]),"',",
		  {curr,res},"}};",nl,nl]);
	{[FirstT|RestT],_} ->
	    emit([indent(4),"{",
		  (?ASN1CT_GEN_BER:decode_class(FirstT#tag.class) bsl 10) +
		  FirstT#tag.number,", ",{curr,v},"} -> ",nl]),
	    emit([indent(8),"{",{asis,Cname},", "]),
	    gen_dec_line(Erules,TopType,Cname,[],Type#type{tag=RestT},Prop),
	    emit(["};",nl,nl])
    end,
    gen_dec_choice_cases(Erules,TopType, T).

match_tag(Class, TagNo) when is_integer(TagNo) ->
    match_tag1(asn1ct_gen_ber_bin_v2:decode_class(Class), TagNo).

match_tag1(Class, TagNo) when TagNo =< 30 ->
    io_lib:format("<<~p:2,_:1,~p:5,_/binary>>", [Class bsr 6,TagNo]);
match_tag1(Class, TagNo) ->
    Octets = mk_object_val(TagNo),
    io_lib:format("<<~p:2,_:1,31:5,~s,_/binary>>", [Class bsr 6,Octets]).

mk_object_val(Val) when Val < 16#80 ->
    integer_to_list(Val);
mk_object_val(Val) ->
    mk_object_val(Val bsr 7, [integer_to_list(Val band 16#7F)]).

mk_object_val(0, Acc) ->
    Acc;
mk_object_val(Val, Acc) ->
    I = integer_to_list((Val band 16#7F) bor 16#80),
    mk_object_val(Val bsr 7, [I,","|Acc]).

%%---------------------------------------
%% Generate the encode/decode code 
%%---------------------------------------

gen_enc_line(Erules,TopType,Cname,
	     Type=#type{constraint=C,
			def=#'ObjectClassFieldType'{type={typefield,_}}},
	     Element,Indent,OptOrMand=mandatory,EncObj) 
  when is_list(Element) ->
    case asn1ct_gen:get_constraint(C,componentrelation) of
	{componentrelation,_,_} ->
	    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
			 ["{",{curr,tmpBytes},",_} = "],EncObj);
	_ ->
	    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
			 ["{",{curr,encBytes},",",{curr,encLen},"} = "],
			 EncObj)
    end;
gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,EncObj) 
  when is_list(Element) ->
    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
		 ["{",{curr,encBytes},",",{curr,encLen},"} = "],EncObj).

gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,Assign,EncObj)
  when is_list(Element) ->
    IndDeep = indent(Indent),
    Tag = lists:reverse([?ASN1CT_GEN_BER:encode_tag_val(
			    ?ASN1CT_GEN_BER:decode_class(X#tag.class),
			    X#tag.form,
			    X#tag.number)
			 || X <- Type#type.tag]),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    emit(IndDeep),
    emit(Assign),
    gen_optormand_case(OptOrMand, Erules, TopType, Cname, Type, Element),
    case {Type,asn1ct_gen:get_constraint(Type#type.constraint,
					 componentrelation)} of
	{#type{def=#'ObjectClassFieldType'{type={typefield,_},
					   fieldname=RefedFieldName}},
	 {componentrelation,_,_}} ->
	    {_LeadingAttrName,Fun} = EncObj,
            {Name,RestFieldNames} = RefedFieldName,
            true = is_atom(Name),                %Assertion.
            case OptOrMand of
                mandatory -> ok;
                _ ->
                    emit(["{",{curr,tmpBytes},",_ } = "])
            end,
            emit([Fun,"(",{asis,Name},", ",Element,", ",
                  {asis,RestFieldNames},"),",nl]),
            emit(IndDeep),
            case OptOrMand of
                mandatory ->
                    emit(["{",{curr,encBytes},",",{curr,encLen},
                          "} = ",
                          {call,ber,encode_open_type,
                           [{curr,tmpBytes},{asis,Tag}]},nl]);
                _ ->
                    emit([{call,ber,encode_open_type,
                           [{curr,tmpBytes},{asis,Tag}]}])
            end;
	_ ->
	    case WhatKind of
		{primitive,bif} ->
		    ?ASN1CT_GEN_BER:gen_encode_prim(ber, Type, {asis,Tag},
						    Element);
		'ASN1_OPEN_TYPE' ->
		    case Type#type.def of
			#'ObjectClassFieldType'{} -> %Open Type
			    ?ASN1CT_GEN_BER:gen_encode_prim(ber,#type{def='ASN1_OPEN_TYPE'},{asis,Tag},Element);
			_ ->
			    ?ASN1CT_GEN_BER:gen_encode_prim(ber,Type,
							    {asis,Tag},
							    Element)
		    end;
		_ ->
		    {EncFunName, _EncMod, _EncFun} = 
			mkfuncname(TopType,Cname,WhatKind,"enc_",""),
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
	    emit([nl,indent(7),"end"])
    end.

gen_optormand_case(mandatory, _Gen, _TopType, _Cname, _Type, _Element) ->
    ok;
gen_optormand_case('OPTIONAL', Gen, _TopType, _Cname, _Type, Element) ->
    emit([" case ",Element," of",nl]),
    Missing = case Gen of
                  #gen{pack=record} -> asn1_NOVALUE;
                  #gen{pack=map} -> ?MISSING_IN_MAP
              end,
    emit([indent(9),Missing," -> {",
	  empty_lb(Gen),",0};",nl]),
    emit([indent(9),"_ ->",nl,indent(12)]);
gen_optormand_case({'DEFAULT',DefaultValue}, Gen, _TopType,
		   _Cname, Type, Element) ->
    CurrMod = get(currmod),
    case Gen of
        #gen{erule=ber,der=true} ->
	    asn1ct_gen_check:emit(Gen, Type, DefaultValue, Element);
	#gen{erule=ber,der=false,pack=Pack} ->
            Ind9 = indent(9),
            DefMarker = case Pack of
                            record -> asn1_DEFAULT;
                            map -> ?MISSING_IN_MAP
                        end,
	    emit([" case ",Element," of",nl,
                  Ind9,{asis,DefMarker}," ->",nl,
                  Ind9,indent(3),"{",empty_lb(Gen),",0};",nl,
                  Ind9,"_ when ",Element," =:= "]),
	    Dv = case DefaultValue of
                     #'Externalvaluereference'{module=CurrMod,
                                               value=V} ->
                         ["?",{asis,V}];
                     _ ->
                         [{asis,DefaultValue}]
                 end,
            emit(Dv++[" ->",nl,
                      Ind9,indent(3),"{",empty_lb(Gen),",0};",nl,
                      Ind9,"_ ->",nl,
                      indent(12)])
    end.

%% Use for SEQUENCE OF and CHOICE.
gen_dec_line(Erules,TopType,Cname,CTags,Type,OptOrMand) ->
    %% The matching on the next line is an assertion.
    {[],[]} = gen_dec_line(Erules,TopType,Cname,CTags,Type,OptOrMand,false),
    ok.

%% Use for SEQUENCE.
gen_dec_line(Erules,TopType,Cname,CTags,Type,OptOrMand,DecObjInf)  ->
    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(v)),
    Tag = 
	[(?ASN1CT_GEN_BER:decode_class(X#tag.class) bsl 10) + X#tag.number ||
	    X <- Type#type.tag],
    ChoiceTags = 
	[(?ASN1CT_GEN_BER:decode_class(Class) bsl 10) + Number|| 
	    {Class,Number} <- CTags], 
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
			     BytesVar,Tag,
			     mandatory,", mandatory, ",DecObjInf,OptOrMand);
	    _ ->
                %% optional or default, or a mandatory component after
                %% an extension marker
		{FirstTag,RestTag} = 
		    case Tag of 
			[] -> 
			    {ChoiceTags,[]};
			[Ft|Rt] -> 
			    {Ft,Rt}
		    end,
		emit(["case ",{prev,tlv}," of",nl]),
		PostponedDec = 
		    case Tag of
			[] when length(ChoiceTags) > 0 -> % a choice without explicit tag
			    Fcases = 
				fun(FirstTag1) ->
					emit(["[",{curr,v}," = {",{asis,FirstTag1},
					      ",_}|Temp",
					      {curr,tlv},
					      "] ->",nl]),
					emit([indent(4),"{"]),
					Pdec= 
					    gen_dec_call(InnerType,Erules,
							 TopType,Cname,Type,
							 BytesVar,RestTag,
							 mandatory,
							 ", mandatory, ",
							 DecObjInf,OptOrMand),
					
					emit([", Temp",{curr,tlv},"}"]),
					emit([";",nl]),
					Pdec
				end,
			    hd([Fcases(TmpTag)|| TmpTag <- FirstTag]);

			[] -> % an open type without explicit tag
			    emit(["[",{curr,v},"|Temp",{curr,tlv},"] ->",nl]),
			    emit([indent(4),"{"]),
			    Pdec= 
				gen_dec_call(InnerType,Erules,TopType,Cname,
					     Type,BytesVar,RestTag,mandatory,
					     ", mandatory, ",DecObjInf,
					     OptOrMand),
			    
			    emit([", Temp",{curr,tlv},"}"]),
			    emit([";",nl]),
			    Pdec;

			_ ->
			    emit(["[{",{asis,FirstTag},
				  ",",{curr,v},"}|Temp",
				  {curr,tlv},
				  "] ->",nl]),
			    emit([indent(4),"{"]),
			    Pdec= 
				gen_dec_call(InnerType,Erules,TopType,Cname,
					     Type,BytesVar,RestTag,mandatory,
					     ", mandatory, ",DecObjInf,
					     OptOrMand),
			    
			    emit([", Temp",{curr,tlv},"}"]),
			    emit([";",nl]),
			    Pdec
		    end,
		
		emit([indent(4),"_ ->",nl]),
		case OptOrMand of
		    {'DEFAULT', Def0} ->
			Def = asn1ct_gen:conform_value(Type, Def0),
			emit([indent(8),"{",{asis,Def},",",{prev,tlv},"}",nl]);
		    'OPTIONAL' ->
			emit([indent(8),"{ asn1_NOVALUE, ",{prev,tlv},"}",nl])
		end,
		emit(["end"]),
		PostponedDec
	end,
    case DecObjInf of
	{Cname,ObjSet} ->
            %% This must be the component were an object is chosen
	    %% from the object set according to the table constraint.
	    ObjSetName = case ObjSet of
			     {deep,OSName,_,_} ->
				 OSName;
			     _ -> ObjSet
			 end,
	    {[{ObjSetName,Cname,asn1ct_gen:mk_var(asn1ct_name:curr(term))}],
	     PostpDec};
	_  -> {[],PostpDec}
    end.

gen_dec_call({typefield,_},_,_,_Cname,Type,BytesVar,Tag,_,_,false,_) ->
    %%  this in case of a choice with typefield components
    asn1ct_name:new(reason),
    asn1ct_name:new(opendec),
    asn1ct_name:new(tmpterm),
    asn1ct_name:new(tmptlv),

    {FirstPFName,RestPFName} = 
	(Type#type.def)#'ObjectClassFieldType'.fieldname,
    emit([nl,indent(6),"begin",nl]),
    emit([indent(9),{curr,tmptlv}," = ",
	  {call,ber,decode_open_type,
	   [BytesVar,{asis,Tag}]},com,nl]),

    emit([indent(9),"case (catch ObjFun(",{asis,FirstPFName},
	  ", ",{curr,tmptlv},", ",{asis,RestPFName},
	  ")) of", nl]),%% ??? What about Tag 
    emit([indent(12),"{'EXIT',",{curr,reason},"} ->",nl]),
    emit([indent(15),"exit({'Type not ",
	  "compatible with table constraint', ",{curr,reason},"});",nl]),
    emit([indent(12),{curr,tmpterm}," ->",nl]),
    emit([indent(15),{curr,tmpterm},nl]),
    emit([indent(9),"end",nl,indent(6),"end",nl]),
    [];
gen_dec_call({typefield,_},_,_,Cname,Type,BytesVar,Tag,_,_,_DecObjInf,OptOrMandComp) ->
    call(decode_open_type, [BytesVar,{asis,Tag}]),
    RefedFieldName = (Type#type.def)#'ObjectClassFieldType'.fieldname,
    [{Cname,RefedFieldName,asn1ct_gen:mk_var(asn1ct_name:curr(term)),
      asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm)),Tag,OptOrMandComp}];
gen_dec_call(InnerType, Gen, TopType, Cname, Type, BytesVar,
	     Tag, _PrimOptOrMand, _OptOrMand, DecObjInf,_) ->
    WhatKind = asn1ct_gen:type(InnerType),
    gen_dec_call1(WhatKind, InnerType, TopType, Cname,
		  Type, BytesVar, Tag),
    case DecObjInf of
	{Cname,{_,OSet,_UniqueFName,ValIndex}} ->
	    Term = asn1ct_gen:mk_var(asn1ct_name:curr(term)),
	    ValueMatch = value_match(Gen, ValIndex, Term),
	    {ObjSetMod,ObjSetName} = OSet,
	    emit([",",nl,"ObjFun = ",{asis,ObjSetMod},":'getdec_",ObjSetName,
		  "'(",ValueMatch,")"]);
	_ ->
	    ok
    end,
    [].

gen_dec_call1({primitive,bif}, InnerType, TopType, Cname,
	      Type, BytesVar, Tag) ->
    case {asn1ct:get_gen_state_field(namelist),InnerType} of
	{[{Cname,undecoded}|Rest],_} ->
	    asn1ct:add_generated_refed_func({[Cname|TopType],undecoded,
					     Tag,Type}),
	    asn1ct:update_gen_state(namelist,Rest),
	    emit(["{'",asn1ct_gen:list2name([Cname|TopType]),"',",
		  BytesVar,"}"]);
	_ ->
	    ?ASN1CT_GEN_BER:gen_dec_prim(Type, BytesVar, Tag)
    end;
gen_dec_call1('ASN1_OPEN_TYPE', _InnerType, TopType, Cname,
	      Type, BytesVar, Tag) ->
    case {asn1ct:get_gen_state_field(namelist),Type#type.def} of
	{[{Cname,undecoded}|Rest],_} ->
	    asn1ct:add_generated_refed_func({[Cname|TopType],undecoded,
					     Tag,Type}),
	    asn1ct:update_gen_state(namelist,Rest),
	    emit(["{'",asn1ct_gen:list2name([Cname|TopType]),"',",
		  BytesVar,"}"]);
	{_,#'ObjectClassFieldType'{type=OpenType}} ->
	    ?ASN1CT_GEN_BER:gen_dec_prim(#type{def=OpenType},
					 BytesVar, Tag);
	_ ->
	    ?ASN1CT_GEN_BER:gen_dec_prim(Type, BytesVar, Tag)
    end;
gen_dec_call1(WhatKind, _, TopType, Cname, Type, BytesVar, Tag) ->
    case asn1ct:get_gen_state_field(namelist) of
	[{Cname,undecoded}|Rest] ->
	    asn1ct:add_generated_refed_func({[Cname|TopType],undecoded,
					     Tag,Type}),
	    asn1ct:update_gen_state(namelist,Rest),
	    emit(["{'",asn1ct_gen:list2name([Cname|TopType]),"',",
		  BytesVar,"}"]);
	_ ->
	    EmitDecFunCall = 
		fun(FuncName) ->
			case {WhatKind,Type#type.tablecinf} of
			    {{constructed,bif},[{objfun,_}|_Rest]} ->
				emit([FuncName,"(",BytesVar,", ",{asis,Tag},
				      ", ObjFun)"]);
			    _ ->
				emit([FuncName,"(",BytesVar,", ",{asis,Tag},")"])
			end
		end,
	    case asn1ct:get_gen_state_field(namelist) of
		[{Cname,List}|Rest] when is_list(List) ->
		    Sindex =
			case WhatKind of
			    #'Externaltypereference'{} ->
				SI = asn1ct:maybe_saved_sindex(WhatKind,List),
				Saves = {WhatKind,SI,List},
				asn1ct:add_tobe_refed_func(Saves),
				SI;
			    _ ->
				SI = asn1ct:maybe_saved_sindex([Cname|TopType],List),
				Saves = {[Cname|TopType],SI,List,Type},
				asn1ct:add_tobe_refed_func(Saves),
				SI
			end,
		    asn1ct:update_gen_state(namelist,Rest),
		    Prefix=asn1ct:get_gen_state_field(prefix),
		    Suffix =
			case Sindex of
			    I when is_integer(I),I>0 -> lists:concat(["_",I]);
			    _ -> ""
			end,
		    {DecFunName,_,_}=
			mkfuncname(TopType,Cname,WhatKind,Prefix,Suffix),
		    EmitDecFunCall(DecFunName);
		[{Cname,parts}|Rest] ->
		    asn1ct:update_gen_state(namelist,Rest),
		    asn1ct:get_gen_state_field(prefix),
		    %% This is to prepare SEQUENCE OF value in
		    %% partial incomplete decode for a later
		    %% part-decode, i.e. skip %% the tag.
		    asn1ct:add_generated_refed_func({[Cname|TopType],
						     parts,
						     [],Type}),
		    emit(["{'",asn1ct_gen:list2name([Cname|TopType]),"',"]),
		    asn1ct_func:need({ber,match_tags,2}),
		    EmitDecFunCall("match_tags"),
		    emit("}");
		_ ->
		    {DecFunName,_,_}=
			mkfuncname(TopType,Cname,WhatKind,"dec_",""),
		    EmitDecFunCall(DecFunName)
	    end
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

extensible(CompList) when is_list(CompList) ->
    noext;
extensible({RootList,ExtList}) ->
    {ext,length(RootList)+1,length(ExtList)};
extensible({_Rl1,_Ext,_Rl2}) ->
    extensible.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filter away ExtensionAdditionGroup start and end marks since these
%% have no significance for the BER encoding
%%
filter_complist(CompList) when is_list(CompList) ->
    lists:filter(fun(#'ExtensionAdditionGroup'{}) ->
			 false;
		    ('ExtensionAdditionGroupEnd') ->
			 false;
		    (_) ->
			 true
		 end, CompList);
filter_complist({Root,Ext}) ->
    {Root,filter_complist(Ext)};
filter_complist({Root1,Ext,Root2}) ->
    {Root1,filter_complist(Ext),Root2}.


print_attribute_comment(InnerType,Pos,Cname,Prop) ->
    CommentLine = "%%-------------------------------------------------",
    emit([nl,CommentLine]),
    case InnerType of
	#'Externaltypereference'{module=XModule,type=Name} ->
	    emit([nl,"%% attribute ",Cname,"(",Pos,")   External ",XModule,":",Name]);
        _ when is_tuple(InnerType) ->
	    emit([nl,"%% attribute ",Cname,"(",Pos,") with type "|
                  tuple_to_list(InnerType)]);
	_ ->
	    emit([nl,"%% attribute ",Cname,"(",Pos,") with type ",InnerType])
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


    
mkfuncname(TopType,Cname,WhatKind,Prefix,Suffix) ->
    CurrMod = get(currmod),
    case WhatKind of
	#'Externaltypereference'{module=CurrMod,type=EType} ->
	    F = lists:concat(["'",Prefix,EType,Suffix,"'"]),
	    {F, "?MODULE", F};
	#'Externaltypereference'{module=Mod,type=EType} ->
	    {lists:concat(["'",Mod,"':'",Prefix,EType,Suffix,"'"]),Mod,
	     lists:concat(["'",Prefix,EType,"'"])};
	{constructed,bif} ->
	    F = lists:concat(["'",Prefix,
			      asn1ct_gen:list2name([Cname|TopType]),
			      Suffix,"'"]),
	    {F, "?MODULE", F}
    end.

empty_lb(#gen{erule=ber}) ->
    "<<>>".

value_match(#gen{pack=record}, VIs, Value) ->
    value_match_rec(VIs, Value);
value_match(#gen{pack=map}, VIs, Value) ->
    value_match_map(VIs, Value).

value_match_rec([], Value) ->
    Value;
value_match_rec([{VI,_}|VIs], Value0) ->
    Value = value_match_rec(VIs, Value0),
    lists:concat(["element(",VI,", ",Value,")"]).

value_match_map([], Value) ->
    Value;
value_match_map([{_,Name}|VIs], Value0) ->
    Value = value_match_map(VIs, Value0),
    lists:concat(["maps:get(",Name,", ",Value,")"]).

call(F, Args) ->
    asn1ct_func:call(ber, F, Args).
