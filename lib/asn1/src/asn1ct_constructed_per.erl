%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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

-import(asn1ct_gen, [emit/1,get_record_name_prefix/1]).

-type type_name() :: any().


%% ENCODE GENERATOR FOR SEQUENCE TYPE  ** **********


-spec gen_encode_set(Gen, TypeName, #type{}) -> 'ok' when
      Gen :: #gen{},
      TypeName :: type_name().

gen_encode_set(Gen, TypeName, D) ->
    gen_encode_constructed(Gen, TypeName, D).

-spec gen_encode_sequence(Gen, TypeName, #type{}) -> 'ok' when
      Gen :: #gen{},
      TypeName :: type_name().

gen_encode_sequence(Gen, TypeName, D) ->
    gen_encode_constructed(Gen, TypeName, D).

gen_encode_constructed(Erule, Typename, #type{}=D) ->
    asn1ct_name:start(),
    Imm = gen_encode_constructed_imm(Erule, Typename, D),
    asn1ct_imm:enc_cg(Imm, is_aligned(Erule)),
    emit([".",nl]).

gen_encode_constructed_imm(Gen, Typename, #type{}=D) ->
    {CompList,TableConsInfo} = enc_complist(D),
    ExternalImm = external_imm(Gen, Typename),
    Optionals = optionals(to_textual_order(CompList)),
    ImmOptionals = enc_optionals(Gen, Optionals),
    Ext = extensible_enc(CompList),
    Aligned = is_aligned(Gen),
    ExtImm = case Ext of
		 {ext,ExtPos,NumExt} when NumExt > 0 ->
		     gen_encode_extaddgroup(Gen, CompList),
		     Value = make_var(val),
                     enc_extensions(Gen, Value, ExtPos, NumExt, Aligned);
		 _ ->
		     []
	     end,
    MatchImm = enc_map_match(Gen, CompList),
    {EncObj,ObjSetImm} = enc_table(Gen, TableConsInfo, D),
    ImmSetExt =
	case Ext of
	    {ext,_Pos,NumExt2} when NumExt2 > 0 ->
		asn1ct_imm:per_enc_extension_bit({var,"Extensions"}, Aligned);
	    {ext,_Pos,_} ->
		asn1ct_imm:per_enc_extension_bit([], Aligned);
	    _ ->
		[]
	end,
    ImmBody = gen_enc_components_call(Gen, Typename, CompList, EncObj, Ext),
    ExternalImm ++ MatchImm ++ ExtImm ++ ObjSetImm ++
	asn1ct_imm:enc_append([ImmSetExt] ++ ImmOptionals ++ ImmBody).

external_imm(Gen, ['EXTERNAL']) ->
    Next = asn1ct_gen:mk_var(asn1ct_name:next(val)),
    Curr = asn1ct_gen:mk_var(asn1ct_name:curr(val)),
    asn1ct_name:new(val),
    F = case Gen of
            #gen{pack=record} -> transform_to_EXTERNAL1990;
            #gen{pack=map} -> transform_to_EXTERNAL1990_maps
        end,
    [{call,ext,F,[{var,Curr}],{var,Next}}];
external_imm(_, _) ->
    [].

enc_extensions(#gen{pack=record}, Value, ExtPos, NumExt, Aligned) ->
    asn1ct_imm:per_enc_extensions(Value, ExtPos, NumExt, Aligned);
enc_extensions(#gen{pack=map}, Value, ExtPos, NumExt, Aligned) ->
    Vars = [{var,lists:concat(["Input@",Pos])} ||
               Pos <- lists:seq(ExtPos, ExtPos+NumExt-1)],
    Undefined = atom_to_list(?MISSING_IN_MAP),
    asn1ct_imm:per_enc_extensions_map(Value, Vars, Undefined, Aligned).

enc_complist(#type{def=Def}) ->
    case Def of
        #'SEQUENCE'{tablecinf=TCI,components=CL0,extaddgroup=ExtAddGroup} ->
            case ExtAddGroup of
                undefined ->
                    {CL0,TCI};
                _ when is_integer(ExtAddGroup) ->
                    %% This is a fake SEQUENCE representing an
                    %% ExtensionAdditionGroup.  Renumber the textual
                    %% order so we get the right index of the
                    %% components.
                    CL = add_textual_order(CL0),
                    {CL,TCI}
            end;
        #'SET'{tablecinf=TCI,components=CL} ->
            {CL,TCI}
    end.

enc_table(Gen, #simpletableattributes{objectsetname=ObjectSet,
                                      c_name=AttrN,
                                      c_index=N,
                                      usedclassfield=UniqueFieldName,
                                      uniqueclassfield=UniqueFieldName,
                                      valueindex=ValueIndex0}, _) ->
    {Module,ObjSetName} = ObjectSet,
    #typedef{typespec=#'ObjectSet'{gen=MustGen}} =
        asn1_db:dbget(Module, ObjSetName),
    case MustGen of
        true ->
            ValueIndex = ValueIndex0 ++ [{N+1,'ASN1_top'}],
            Val = make_var(val),
            {ObjSetImm,Dst} = enc_dig_out_value(Gen, ValueIndex, Val),
            {{AttrN,Dst},ObjSetImm};
        false ->
            {false,[]}
    end;
enc_table(_Gen, #simpletableattributes{}, _) ->
    {false,[]};
enc_table(_Gen, _, #type{tablecinf=TCInf}) ->
    case TCInf of
        [{objfun,_}|_] ->
            %% The simpletableattributes was at an outer
            %% level and the objfun has been passed through the
            %% function call.
            {{"got objfun through args",{var,"ObjFun"}},[]};
        _ ->
            {false,[]}
    end.

enc_optionals(Gen, Optionals) ->
    Var = make_var(val),
    enc_optionals_1(Gen, Optionals, Var).

enc_optionals_1(#gen{pack=record}=Gen, [{Pos,DefVals}|T], Var) ->
    {Imm0,Element} = asn1ct_imm:enc_element(Pos+1, Var),
    Imm = asn1ct_imm:per_enc_optional(Element, DefVals),
    [Imm0++Imm|enc_optionals_1(Gen, T, Var)];
enc_optionals_1(#gen{pack=map}=Gen, [{Pos,DefVals0}|T], V) ->
    Var = {var,lists:concat(["Input@",Pos])},
    DefVals = translate_missing_value(Gen, DefVals0),
    Imm = asn1ct_imm:per_enc_optional(Var, DefVals),
    [Imm|enc_optionals_1(Gen, T, V)];
enc_optionals_1(_, [], _) ->
    [].

enc_map_match(#gen{pack=record}, _Cs) ->
    [];
enc_map_match(#gen{pack=map}, Cs0) ->
    Var0 = "Input",
    Cs = enc_flatten_components(Cs0),
    M = [[quote_atom(Name),":=",lists:concat([Var0,"@",Order])] ||
            #'ComponentType'{prop=mandatory,name=Name,
                             textual_order=Order} <- Cs],
    Mand = case M of
               [] ->
                   [];
               [_|_] ->
                   Patt = {expr,lists:flatten(["#{",lists:join(",", M),"}"])},
                   [{assign,Patt,{var,asn1ct_name:curr(val)}}]
           end,

    Os0 = [{Name,Order} ||
              #'ComponentType'{prop=Prop,name=Name,
                               textual_order=Order} <- Cs,
              Prop =/= mandatory],
    {var,Val} = make_var(val),
    F = fun({Name,Order}) ->
                Var = lists:concat([Var0,"@",Order]),
                P0 = ["case ",Val," of\n"
                      "  #{",quote_atom(Name),":=",Var,"_0} -> ",
                      Var,"_0;\n"
                      "  _ -> ",atom_to_list(?MISSING_IN_MAP),"\n"
                      "end"],
                P = lists:flatten(P0),
                {assign,{var,Var},P}
        end,
    Os = [F(O) || O <- Os0],
    Mand ++ Os.

enc_flatten_components({Root1,Ext0,Root2}=CL) ->
    {_,Gs} = extgroup_pos_and_length(CL),
    Ext = wrap_extensionAdditionGroups(Ext0, Gs),
    Root1 ++ Root2 ++ [mark_optional(C) || C <- Ext];
enc_flatten_components({Root,Ext}) ->
    enc_flatten_components({Root,Ext,[]});
enc_flatten_components(Cs) ->
    Cs.

gen_encode_extaddgroup(#gen{pack=record}, CompList) ->
    case extgroup_pos_and_length(CompList) of
	{extgrouppos,[]} ->
	    ok;
	{extgrouppos,ExtGroupPosLenList} ->
	    _ = [gen_encode_eag_record(G) ||
                    G <- ExtGroupPosLenList],
	    ok
    end;
gen_encode_extaddgroup(#gen{pack=map}, Cs0) ->
    Cs = enc_flatten_components(Cs0),
    gen_encode_eag_map(Cs).

gen_encode_eag_map([#'ComponentType'{name=Group,typespec=Type}|Cs]) ->
    case Type of
        #type{def=#'SEQUENCE'{extaddgroup=G,components=GCs0}}
          when is_integer(G) ->
            Ns = [N || #'ComponentType'{name=N,prop=mandatory} <- GCs0],
            test_for_mandatory(Ns, Group),
            gen_encode_eag_map(Cs);
        _ ->
            gen_encode_eag_map(Cs)
    end;
gen_encode_eag_map([]) ->
    ok.

test_for_mandatory([Mand|_], Group) ->
    emit([{next,val}," = case ",{curr,val}," of",nl,
	  "#{",quote_atom(Mand),":=_} -> ",
          {curr,val},"#{",{asis,Group},"=>",{curr,val},"};",nl,
          "#{} -> ",{curr,val},nl,
	  "end,",nl]),
    asn1ct_name:new(val);
test_for_mandatory([], _) ->
    ok.

gen_encode_eag_record({ActualPos,VirtualPos,Len}) ->
    Val = asn1ct_gen:mk_var(asn1ct_name:curr(val)),
    Elements = get_input_vars(Val, VirtualPos, Len),
    Expr = any_non_value(Val, VirtualPos, Len),
    emit([{next,val}," = case ",Expr," of",nl,
	  "false -> setelement(",{asis,ActualPos+1},", ",
	  {curr,val},", asn1_NOVALUE);",nl,
	  "true -> setelement(",{asis,ActualPos+1},", ",
	  {curr,val},", {extaddgroup,", Elements,"})",nl,
	  "end,",nl]),
    asn1ct_name:new(val).

any_non_value(Val, Pos, N) ->
    L = any_non_value_1(Val, Pos, N),
    lists:join(" orelse ", L).

any_non_value_1(_, _, 0) ->
    [];
any_non_value_1(Val, Pos, N) ->
    Var = get_input_var(Val, Pos),
    [Var ++ " =/= asn1_NOVALUE"|any_non_value_1(Val, Pos+1, N-1)].

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
	    #simpletableattributes{objectsetname=ObjectSet,
				   c_name=AttrN,
				   usedclassfield=UniqueFieldName,
				   uniqueclassfield=UniqueFieldName,
				   valueindex=ValIndex} ->
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
    EmitObjSets = gen_dec_objsets_fun(Erule, ObjSetInfo),
    EmitPack = fun(_) ->
                       gen_dec_pack(Erule, Typename, CompList)
               end,
    RestGroup = {group,[{safe,EmitObjSets},{safe,EmitPack}]},
    [EmitExt,EmitOpt|EmitComp++[RestGroup]].

gen_dec_objsets_fun(Gen, ObjSetInfo) ->
    fun({AccTerm,AccBytes}) ->
            {_,_UniqueFName,ValueIndex} = ObjSetInfo,
            case {AccTerm,AccBytes} of
                {[],[]} ->
                    ok;
                {_,[]} ->
                    ok;
                {[{ObjSet,LeadingAttr,Term}],ListOfOpenTypes} ->
                    ValueMatch = value_match(Gen, ValueIndex, Term),
                    _ = [begin
                             gen_dec_open_type(Gen, ValueMatch, ObjSet,
                                               LeadingAttr, T),
                             emit([com,nl])
                         end || T <- ListOfOpenTypes],
                    ok
            end
    end.

gen_dec_pack(Gen, Typename, CompList) ->
    case Typename of
	['EXTERNAL'] ->
            dec_external(Gen, Typename);
	_ ->
            asn1ct_name:new(res),
            gen_dec_do_pack(Gen, Typename, CompList),
            emit([com,nl,
                  "{",{curr,res},",",{curr,bytes},"}"])
    end.

dec_external(#gen{pack=record}=Gen, Typename) ->
    RecordName = list_to_atom(record_name(Gen, Typename)),
    All = [{var,Term} || Term <- asn1ct_name:all(term)],
    Record = [{asis,RecordName}|All],
    emit(["OldFormat={",lists:join(",", Record),"},",nl,
          "ASN11994Format =",nl,
          {call,ext,transform_to_EXTERNAL1994,
           ["OldFormat"]},com,nl,
          "{ASN11994Format,",{curr,bytes},"}"]);
dec_external(#gen{pack=map}, _Typename) ->
    Vars = asn1ct_name:all(term),
    Names = ['direct-reference','indirect-reference',
             'data-value-descriptor',encoding],
    Zipped = lists:zip(Names, Vars),
    MapInit = lists:join(",", [["'",N,"'=>",{var,V}] || {N,V} <- Zipped]),
    emit(["OldFormat = #{",MapInit,"}",com,nl,
          "ASN11994Format =",nl,
          {call,ext,transform_to_EXTERNAL1994_maps,
           ["OldFormat"]},com,nl,
          "{ASN11994Format,",{curr,bytes},"}"]).

gen_dec_do_pack(#gen{pack=record}=Gen, TypeName, CompList) ->
    Zipped0 = zip_components(CompList, asn1ct_name:all(term)),
    Zipped = textual_order(Zipped0),
    RecordName = ["'",record_name(Gen, TypeName),"'"],
    L = [RecordName|[{var,Var} || {_,Var} <- Zipped]],
    emit([{curr,res}," = {",lists:join(",", L),"}"]);
gen_dec_do_pack(#gen{pack=map}, _, CompList0) ->
    CompList = enc_flatten_components(CompList0),
    Zipped0 = zip_components(CompList, asn1ct_name:all(term)),
    Zipped = textual_order(Zipped0),
    PF = fun({#'ComponentType'{prop='OPTIONAL'},_}) -> false;
            ({_,_}) -> true
         end,
    {Mandatory,Optional} = lists:partition(PF, Zipped),
    L = [[{asis,Name},"=>",{var,Var}] ||
            {#'ComponentType'{name=Name},Var} <- Mandatory],
    emit([{curr,res}," = #{",lists:join(",", L),"}"]),
    gen_dec_map_optional(Optional),
    gen_dec_merge_maps(asn1ct_name:all(map)).

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

gen_dec_merge_maps([M|Ms]) ->
    asn1ct_name:new(res),
    emit([com,nl,
          {curr,res}," = maps:merge(",{prev,res},", ",{var,M},")"]),
    gen_dec_merge_maps(Ms);
gen_dec_merge_maps([]) ->
    ok.

quote_atom(A) when is_atom(A) ->
    io_lib:format("~p", [A]).

%% record_name([TypeName]) -> RecordNameString
%%  Construct a record name for the constructed type, ignoring any
%%  fake sequences that are used to represent an extension addition
%%  group. Such fake sequences never appear as a top type, and their
%%  name always start with "ExtAddGroup".

record_name(Gen, Typename0) ->
    [TopType|Typename1] = lists:reverse(Typename0),
    Typename = filter_ext_add_groups(Typename1, [TopType]),
    lists:concat([get_record_name_prefix(Gen),
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

zip_components({Root,Ext}, Vars) ->
    zip_components({Root,Ext,[]}, Vars);
zip_components({R1,Ext0,R2}, Vars) ->
    Ext = [mark_optional(C) || C <- Ext0],
    zip_components(R1++R2++Ext, Vars);
zip_components(Cs, Vars) when is_list(Cs) ->
    zip_components_1(Cs, Vars).

zip_components_1([#'ComponentType'{}=C|Cs], [V|Vs]) ->
    [{C,V}|zip_components_1(Cs, Vs)];
zip_components_1([_|Cs], Vs) ->
    zip_components_1(Cs, Vs);
zip_components_1([], []) ->
    [].

textual_order([{#'ComponentType'{textual_order=undefined},_}|_]=L) ->
    L;
textual_order(L0) ->
    L = [{Ix,P} || {#'ComponentType'{textual_order=Ix},_}=P <- L0],
    [C || {_,C} <- lists:sort(L)].

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
    emit([{asis,N},"(Bytes, Id) when Id =:= ",{asis,Id}," ->",nl]),
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
    emit([".",nl]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encode generator for SEQUENCE OF type

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

is_aligned(#gen{erule=per,aligned=Aligned}) -> Aligned.

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
	    emit([com,nl]);
	{constructed,bif} ->
	    NewTypename = [Constructed_Suffix|Typename],
	    emit([{asis,dec_func(asn1ct_gen:list2name(NewTypename))},
		  "(Bytes",ObjFun,"),",nl]);
	#'Externaltypereference'{}=Etype ->
	    asn1ct_gen_per:gen_dec_external(Etype, "Bytes"),
	    emit([com,nl]);
	'ASN1_OPEN_TYPE' ->
	    asn1ct_gen_per:gen_dec_prim(Erule, #type{def='ASN1_OPEN_TYPE'},
					"Bytes"),
	    emit([com,nl]);
	_ ->
	    emit([{asis,dec_func(Conttype)},"(Bytes),",nl])
    end,
    emit([{asis,Name},"(Num-1, Remain",ObjFun,", [Term|Acc]).",nl]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% General and special help functions (not exported)

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

optionals({Root1,Ext,Root2}) ->
    Opt1 = optionals(Root1, 1),
    ExtComps = length([C || C = #'ComponentType'{} <- Ext]),
    Opt2 = optionals(Root2, 1 + length(Root1) + ExtComps),
    Opt1 ++ Opt2;
optionals({L,_Ext}) ->
    optionals(L, 1);
optionals(L) ->
    optionals(L, 1).

optionals([#'ComponentType'{prop='OPTIONAL'}|Rest], Pos) ->
    [{Pos,[asn1_NOVALUE]}|optionals(Rest, Pos+1)];
optionals([#'ComponentType'{typespec=T,prop={'DEFAULT',Val}}|Cs], Pos) ->
    Vals = def_values(T, Val),
    [{Pos,Vals}|optionals(Cs, Pos+1)];
optionals([#'ComponentType'{}|Rest], Pos) ->
    optionals(Rest, Pos+1);
optionals([], _) ->
    [].

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

add_textual_order1(Cs,NumIn) ->
    lists:mapfoldl(fun(C=#'ComponentType'{},Num) ->
			   {C#'ComponentType'{textual_order=Num},
			    Num+1};
		      (OtherMarker,Num) ->
			   {OtherMarker,Num}
		   end,
		   NumIn,Cs).

gen_enc_components_call(Erule, TopType, {Root,ExtList}, DynamicEnc, Ext) ->
    gen_enc_components_call(Erule, TopType, {Root,ExtList,[]}, DynamicEnc, Ext);
gen_enc_components_call(Erule, TopType, {R1,ExtList0,R2}=CL, DynamicEnc, Ext) ->
    Root = R1 ++ R2,
    Imm0 = gen_enc_components_call1(Erule, TopType, Root, DynamicEnc, noext),
    ExtImm = case Ext of
		 {ext,_,ExtNum} when ExtNum > 0 ->
		     [{var,"Extensions"}];
		 _ ->
		     []
	     end,
    {extgrouppos,ExtGroupPosLen}  = extgroup_pos_and_length(CL),
    ExtList1 = wrap_extensionAdditionGroups(ExtList0, ExtGroupPosLen),
    ExtList = [mark_optional(C) || C <- ExtList1],
    Imm1 = gen_enc_components_call1(Erule, TopType, ExtList, DynamicEnc, Ext),
    Imm0 ++ [ExtImm|Imm1];
gen_enc_components_call(Erule, TopType, CompList, DynamicEnc, Ext) ->
    %% No extension marker.
    gen_enc_components_call1(Erule, TopType, CompList, DynamicEnc, Ext).

mark_optional(#'ComponentType'{prop=Prop0}=C) ->
    Prop = case Prop0 of
               mandatory -> 'OPTIONAL';
               'OPTIONAL'=Keep -> Keep;
               {'DEFAULT',_}=Keep -> Keep
           end,
    C#'ComponentType'{prop=Prop};
mark_optional(Other) ->
    Other.

gen_enc_components_call1(Gen, TopType, [C|Rest], DynamicEnc, Ext) ->
    #'ComponentType'{name=Cname,typespec=Type,
                     prop=Prop,textual_order=Num} = C,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    CommentString = attribute_comment(InnerType, Num, Cname),
    ImmComment = asn1ct_imm:enc_comment(CommentString),

    {Imm0,Element} = enc_fetch_field(Gen, Num, Prop),
    Imm1 = gen_enc_line_imm(Gen, TopType, Cname, Type,
                            Element, DynamicEnc, Ext),
    Imm2 = case Prop of
	       mandatory ->
		   Imm1;
	       'OPTIONAL' ->
		   enc_absent(Gen, Element, [asn1_NOVALUE], Imm1);
	       {'DEFAULT',Def} when Ext =:= noext ->
		   DefValues = def_values(Type, Def),
		   enc_absent(Gen, Element, DefValues, Imm1);
               {'DEFAULT',_} ->
		   enc_absent(Gen, Element, [asn1_DEFAULT], Imm1)
	   end,
    Imm = case Imm2 of
	      [] -> [];
	      _ -> [ImmComment|Imm0 ++ Imm2]
	  end,
    [Imm|gen_enc_components_call1(Gen, TopType, Rest, DynamicEnc, Ext)];
gen_enc_components_call1(_Gen, _TopType, [], _, _) ->
    [].

enc_absent(Gen, Var, Absent0, Imm) ->
    Absent = translate_missing_value(Gen, Absent0),
    asn1ct_imm:enc_absent(Var, Absent, Imm).

translate_missing_value(#gen{pack=record}, Optionals) ->
    Optionals;
translate_missing_value(#gen{pack=map}, Optionals) ->
    case Optionals of
        [asn1_NOVALUE|T] -> [?MISSING_IN_MAP|T];
        [asn1_DEFAULT|T] -> [?MISSING_IN_MAP|T];
        {call,_,_,_} -> Optionals
    end.

enc_fetch_field(#gen{pack=record}, Num, _Prop) ->
    Val = make_var(val),
    asn1ct_imm:enc_element(Num+1, Val);
enc_fetch_field(#gen{pack=map}, Num, _) ->
    {[],{var,lists:concat(["Input@",Num])}}.

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
gen_dec_components_call(Gen, TopType, {Root1,ExtList,Root2}=CL,
			DecInfObj, Ext, NumberOfOptionals) ->
    %% The type has extensionmarker
    OptTable = create_optionality_table(Root1++Root2),
    Init = {ignore,fun(_) -> {[],[]} end},
    {EmitRoot,Tpos} =
	gen_dec_comp_calls(Root1++Root2, Gen, TopType, OptTable,
			   DecInfObj, noext, NumberOfOptionals,
			   1, []),
    EmitGetExt = gen_dec_get_extension(Gen),
    {extgrouppos,ExtGroupPosLen}  = extgroup_pos_and_length(CL),
    NewExtList = wrap_extensionAdditionGroups(ExtList, ExtGroupPosLen),
    {EmitExts,_} = gen_dec_comp_calls(NewExtList, Gen, TopType, OptTable,
				      DecInfObj, Ext, NumberOfOptionals,
				      Tpos, []),
    NumExtsToSkip = ext_length(ExtList),
    Finish =
	fun(St) ->
		emit([{next,bytes},"= "]),
                Mod = case Gen of
                          #gen{erule=per,aligned=false} -> uper;
                          #gen{erule=per,aligned=true} -> per
                      end,
		asn1ct_func:call(Mod, skipextensions,
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

gen_dec_comp_call(Comp, Gen, TopType, Tpos, OptTable, DecInfObj,
		  Ext, NumberOfOptionals) ->
    #'ComponentType'{name=Cname,typespec=Type,
                     prop=Prop,textual_order=TextPos} = Comp,
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,_Enum} -> Tpos - Epos + 1
	  end,
    InnerType = asn1ct_gen:get_inner(Type#type.def),

    CommentString = attribute_comment(InnerType, TextPos, Cname),
    Comment = fun(St) ->
		      emit([nl,"%% ",CommentString,nl]),
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
                             extaddgroup=GroupNum,
                             components=CompList}} when is_integer(GroupNum)->
                    dec_match_extadd_fun(Gen, CompList);
		_ ->
		    fun(St) ->
			    asn1ct_name:new(term),
			    emit(["{",{curr,term}]),
			    emit([",",{next,bytes},"} = "]),
			    St
		    end
	    end
	end,
    {Pre,Post} = comp_call_pre_post(Gen, Ext, Prop, Pos, Type, TextPos,
				    OptTable, NumberOfOptionals, Ext),
    Lines = gen_dec_seq_line_imm(Gen, TopType, Comp, Tpos, DecInfObj, Ext),
    AdvBuffer = {ignore,fun(St) ->
				asn1ct_name:new(bytes),
				St
			end},
    [{group,[{safe,Comment},{safe,Preamble}] ++ Pre ++
	  Lines ++ Post ++ [{safe,AdvBuffer}]}].

dec_match_extadd_fun(#gen{pack=record}, CompList) ->
    fun(St) ->
            emit(["{{_,"]),
            emit_extaddgroupTerms(term, CompList),
            emit(["}"]),
            emit([",",{next,bytes},"} = "]),
            St
    end;
dec_match_extadd_fun(#gen{pack=map}, _CompList) ->
    fun(St) ->
            asn1ct_name:new(map),
            emit(["{",{curr,map},",",{next,bytes},"} = "]),
            St
    end.

comp_call_pre_post(_Gen, noext, mandatory, _, _, _, _, _, _) ->
    {[],[]};
comp_call_pre_post(_Gen, noext, Prop, _, Type, TextPos,
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
comp_call_pre_post(Gen, {ext,_,_}, Prop, Pos, Type, _, _, _, Ext) ->
    %% Extension
    {[fun(St) ->
	      emit(["case Extensions of",nl,
		    "  <<_:",Pos-1,",1:1,_/bitstring>> ->",nl]),
	      St
      end],
     [extadd_group_fun(Gen, Prop, Type, Ext)]}.

extadd_group_fun(#gen{pack=record}, Prop, Type, Ext) ->
    fun(St) ->
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
    end;
extadd_group_fun(#gen{pack=map}, Prop, Type, Ext) ->
    fun(St) ->
            emit([";",nl,
                  "_  ->",nl,
                  "{"]),
            case Type of
                #type{def=#'SEQUENCE'{
                             extaddgroup=Number2,
                             components=Comp}}
                  when is_integer(Number2)->
                    dec_map_extaddgroup_no_val(Ext, Type, Comp);
                _ ->
                    gen_dec_component_no_val(Ext, Type, Prop)
            end,
            emit([",",{curr,bytes},"}",nl,
                  "end"]),
            St
    end.

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
    emit(["asn1_NOVALUE"]);
gen_dec_component_no_val({ext,_,_}, _, mandatory) ->
    emit(["asn1_NOVALUE"]).

dec_map_extaddgroup_no_val(Ext, Type, Comp) ->
    L0 = [dec_map_extaddgroup_no_val_1(N, P, Ext, Type) ||
             #'ComponentType'{name=N,prop=P} <- Comp],
    L = [E || E <- L0, E =/= []],
    emit(["#{",lists:join(",", L),"}"]).

dec_map_extaddgroup_no_val_1(Name, {'DEFAULT',DefVal0}, _Ext, Type) ->
    DefVal = asn1ct_gen:conform_value(Type, DefVal0),
    [Name,"=>",{asis,DefVal}];
dec_map_extaddgroup_no_val_1(_Name, 'OPTIONAL', _, _) ->
    [];
dec_map_extaddgroup_no_val_1(_Name, mandatory, {ext,_,_}, _) ->
    [].

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
gen_dec_line_special(Gen, Atype, TopType, Comp, DecInfObj) ->
    case gen_dec_line_other(Gen, Atype, TopType, Comp) of
	Fun when is_function(Fun, 1) ->
	    fun({BytesVar,PrevSt}) ->
		    Fun(BytesVar),
		    gen_dec_line_dec_inf(Gen,Comp, DecInfObj),
		    {[],PrevSt}
	    end;
	Imm0 ->
	    {imm,Imm0,
	     fun(Imm, {BytesVar,PrevSt}) ->
		     asn1ct_imm:dec_code_gen(Imm, BytesVar),
		     gen_dec_line_dec_inf(Gen, Comp, DecInfObj),
		     {[],PrevSt}
	     end}
    end.

gen_dec_line_dec_inf(Gen, Comp, DecInfObj) ->
    #'ComponentType'{name=Cname} = Comp,
    case DecInfObj of
	{Cname,{_,_OSet,_UniqueFName,ValIndex}} ->
	    Term = asn1ct_gen:mk_var(asn1ct_name:curr(term)),
	    ValueMatch = value_match(Gen, ValIndex,Term),
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
            DecFunc = dec_func(asn1ct_gen:list2name(NewTypename)),
	    case Type#type.tablecinf of
		[{objfun,_}|_R] ->
		    fun(BytesVar) ->
			    emit([{asis,DecFunc},"(",BytesVar,", ObjFun)"])
		    end;
		_ ->
		    fun(BytesVar) ->
			    emit([{asis,DecFunc},"(",BytesVar,")"])
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

get_input_vars(Val, I, N) ->
    L = get_input_vars_1(Val, I, N),
    lists:join(",", L).

get_input_vars_1(_Val, _I, 0) ->
    [];
get_input_vars_1(Val, I, N) ->
    [get_input_var(Val, I)|get_input_vars_1(Val, I+1, N-1)].

get_input_var(Val, I) ->
    lists:flatten(io_lib:format("element(~w, ~s)", [I+1,Val])).

emit_extaddgroupTerms(VarSeries,[_]) ->
    asn1ct_name:new(VarSeries),
    emit({curr,VarSeries}),
    ok;
emit_extaddgroupTerms(VarSeries,[_|Rest]) ->
    asn1ct_name:new(VarSeries),
    emit([{curr,VarSeries},","]),
    emit_extaddgroupTerms(VarSeries,Rest);
emit_extaddgroupTerms(_,[]) ->
    ok.

flat_complist({Rl1,El,Rl2}) -> Rl1 ++ El ++ Rl2;
flat_complist({Rl,El}) -> Rl ++ El;
flat_complist(CompList) -> CompList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Convert all componentTypes following 'ExtensionAdditionGroup'
%%  up to the matching 'ExtensionAdditionGroupEnd' into one componentType
%%  of type SEQUENCE with the componentTypes as components.
%%
wrap_extensionAdditionGroups(ExtCompList, ExtGroupPosLen) ->
    wrap_eags(ExtCompList, ExtGroupPosLen, 0, 0).

wrap_eags([{'ExtensionAdditionGroup',_Number}|T0],
          [{ActualPos,_,_}|Gs], _ExtAddGroupDiff, ExtGroupNum) ->
    {ExtGroupCompList,['ExtensionAdditionGroupEnd'|T]} =
	lists:splitwith(fun(#'ComponentType'{}) -> true;
			   (_) -> false
			end, T0),
    Name = list_to_atom(lists:concat(["ExtAddGroup",ExtGroupNum+1])),
    Seq = #type{def=#'SEQUENCE'{extaddgroup=ExtGroupNum+1,
                                components=ExtGroupCompList}},
    Comp = #'ComponentType'{name=Name,
                            typespec=Seq,
                            textual_order=ActualPos,
                            prop='OPTIONAL'},
    [Comp|wrap_eags(T, Gs, length(ExtGroupCompList)-1, ExtGroupNum+1)];
wrap_eags([#'ComponentType'{textual_order=Tord}=H|T],
          ExtAddGrpLenPos, ExtAddGroupDiff, ExtGroupNum)
  when is_integer(Tord) ->
    Comp = H#'ComponentType'{textual_order=Tord - ExtAddGroupDiff},
    [Comp|wrap_eags(T, ExtAddGrpLenPos, ExtAddGroupDiff, ExtGroupNum)];
wrap_eags([H|T], ExtAddGrpLenPos, ExtAddGroupDiff, ExtGroupNum) ->
    [H|wrap_eags(T, ExtAddGrpLenPos, ExtAddGroupDiff, ExtGroupNum)];
wrap_eags([], _, _, _) ->
    [].

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

enc_dig_out_value(_Gen, [], Value) ->
    {[],Value};
enc_dig_out_value(#gen{pack=record}=Gen, [{N,_}|T], Value) ->
    {Imm0,Dst0} = enc_dig_out_value(Gen, T, Value),
    {Imm,Dst} = asn1ct_imm:enc_element(N, Dst0),
    {Imm0++Imm,Dst};
enc_dig_out_value(#gen{pack=map}, [{N,'ASN1_top'}], _Value) ->
    {[],{var,lists:concat(["Input@",N-1])}};
enc_dig_out_value(#gen{pack=map}=Gen, [{_,Name}|T], Value) ->
    {Imm0,Dst0} = enc_dig_out_value(Gen, T, Value),
    {Imm,Dst} = asn1ct_imm:enc_maps_get(Name, Dst0),
    {Imm0++Imm,Dst}.

make_var(Base) ->
    {var,atom_to_list(asn1ct_gen:mk_var(asn1ct_name:curr(Base)))}.

attribute_comment(InnerType, TextPos, Cname) ->
    DispType = case InnerType of
		   #'Externaltypereference'{type=T} -> T;
		   IT when is_tuple(IT) -> element(2,IT);
		   _ -> InnerType
	       end,
    Comment = ["attribute ",Cname,"(",TextPos,") with type ",DispType],
    lists:concat(Comment).

dec_func(Tname) ->
    list_to_atom(lists:concat(["dec_",Tname])).
