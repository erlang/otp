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
-module(asn1ct_gen).

-include("asn1_records.hrl").

-export([emit/1,
	 open_output_file/1,close_output_file/0,
	 get_inner/1,type/1,def_to_tag/1,prim_bif/1,
	 list2name/1,
	 list2rname/1,
	 constructed_suffix/2,
	 unify_if_string/1,
	 get_constraint/2,
	 insert_once/2,
	 ct_gen_module/1,
	 index2suffix/1,
	 get_record_name_prefix/1,
	 conform_value/2,
	 named_bitstring_value/2]).
-export([pgen/3,
	 mk_var/1, 
	 un_hyphen_var/1]).
-export([gen_encode_constructed/4,
	 gen_decode_constructed/4]).

-define(SUPPRESSION_FUNC, 'dialyzer-suppressions').


%% pgen(Outfile, Erules, Module, TypeOrVal, Options)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to
%% an ASN.1 module. The .hrl file is only generated if necessary.

-spec pgen(Outfile, Gen, Code) -> 'ok' when
      Outfile :: any(),
      Gen :: #gen{},
      Code :: #abst{}.

pgen(OutFile, #gen{options=Options}=Gen, Code) ->
    #abst{name=Module,types=Types} = Code,
    N2nConvEnums = [CName|| {n2n,CName} <- Options],
    case N2nConvEnums -- Types of
	[] ->
	    ok;
	UnmatchedTypes ->
	    exit({"Non existing ENUMERATION types used in n2n option",
		   UnmatchedTypes})
    end,
    put(outfile, OutFile),
    put(currmod, Module),
    HrlGenerated = pgen_hrl(Gen, Code),
    asn1ct_name:start(),
    ErlFile = lists:concat([OutFile,".erl"]),
    _ = open_output_file(ErlFile),
    asn1ct_func:start_link(),
    gen_head(Gen, Module, HrlGenerated),
    pgen_exports(Gen, Code),
    pgen_dispatcher(Gen, Types),
    pgen_info(),
    pgen_typeorval(Gen, N2nConvEnums, Code),
    pgen_partial_incomplete_decode(Gen),
    emit([nl,
	  "%%%",nl,
	  "%%% Run-time functions.",nl,
	  "%%%",nl]),
    dialyzer_suppressions(Gen),
    Fd = get(gen_file_out),
    asn1ct_func:generate(Fd),
    close_output_file(),
    _ = erase(outfile),
    asn1ct:verbose("--~p--~n", [{generated,ErlFile}], Gen),
    ok.

dialyzer_suppressions(Erules) ->
    emit([nl,
	  {asis,?SUPPRESSION_FUNC},"(Arg) ->",nl]),
    Rtmod = ct_gen_module(Erules),
    Rtmod:dialyzer_suppressions(Erules).

pgen_typeorval(Erules, N2nConvEnums, Code) ->
    #abst{name=Module,types=Types,values=Values,
          objects=Objects,objsets=ObjectSets} = Code,
    Rtmod = ct_gen_module(Erules),
    pgen_types(Rtmod,Erules,N2nConvEnums,Module,Types),
    pgen_values(Values, Module),
    pgen_objects(Rtmod,Erules,Module,Objects),
    pgen_objectsets(Rtmod,Erules,Module,ObjectSets),
    pgen_partial_decode(Rtmod,Erules,Module).

%% Generate a function 'V'/0 for each Value V defined in the ASN.1 module.
%% The function returns the value in an Erlang representation which can be
%% used as input to the runtime encode functions.

pgen_values([H|T], Module) ->
    #valuedef{name=Name,value=Value} = asn1_db:dbget(Module, H),
    emit([{asis,Name},"() ->",nl,
          {asis,Value},".",nl,nl]),
    pgen_values(T, Module);
pgen_values([], _) ->
    ok.

pgen_types(_, _, _, _, []) ->
    true;
pgen_types(Rtmod,Erules,N2nConvEnums,Module,[H|T]) ->
    asn1ct_name:clear(),
    Typedef = asn1_db:dbget(Module,H),
    Rtmod:gen_encode(Erules,Typedef),
    asn1ct_name:clear(),
    Rtmod:gen_decode(Erules,Typedef),
    case lists:member(H,N2nConvEnums) of
	true ->
	    pgen_n2nconversion(Erules,Typedef);
	_ ->
	    true
    end,
    pgen_types(Rtmod,Erules,N2nConvEnums,Module,T).

%% Enumerated type with extension marker
pgen_n2nconversion(_Erules,#typedef{name=TypeName,typespec=#type{def={'ENUMERATED',{NN1,NN2}}}}) ->
    NN = NN1 ++ NN2,
    pgen_name2numfunc(TypeName,NN, extension_marker),
    pgen_num2namefunc(TypeName,NN, extension_marker);
%% Without extension marker
pgen_n2nconversion(_Erules,#typedef{name=TypeName,typespec=#type{def={'ENUMERATED',NN}}}) ->
    pgen_name2numfunc(TypeName,NN, no_extension_marker),
    pgen_num2namefunc(TypeName,NN, no_extension_marker);
pgen_n2nconversion(_Erules,_) ->
    true.

pgen_name2numfunc(TypeNameAsAtom,Mapping,Ext) when is_atom(TypeNameAsAtom) ->
    FuncName = list_to_atom("name2num_"++atom_to_list(TypeNameAsAtom)),
    pgen_name2numfunc1(FuncName,Mapping,Ext).

pgen_name2numfunc1(_FuncName,[], _) ->
    true;
pgen_name2numfunc1(FuncName,[{Atom,Number}], extension_marker) ->
    emit([{asis,FuncName},"(",{asis,Atom},") ->",Number,";",nl]),
    emit([{asis,FuncName},"({asn1_enum, Num}) -> Num.",nl,nl]);
pgen_name2numfunc1(FuncName,[{Atom,Number}], _) ->
    emit([{asis,FuncName},"(",{asis,Atom},") ->",Number,".",nl,nl]);
pgen_name2numfunc1(FuncName,[{Atom,Number}|NNRest], EM) ->
    emit([{asis,FuncName},"(",{asis,Atom},") ->",Number,";",nl]),
    pgen_name2numfunc1(FuncName,NNRest, EM).

pgen_num2namefunc(TypeNameAsAtom,Mapping,Ext) when is_atom(TypeNameAsAtom) ->
    FuncName = list_to_atom("num2name_"++atom_to_list(TypeNameAsAtom)),
    pgen_num2namefunc1(FuncName,Mapping,Ext).

pgen_num2namefunc1(_FuncName,[], _) ->
    true;
pgen_num2namefunc1(FuncName,[{Atom,Number}], extension_marker) ->
    emit([{asis,FuncName},"(",Number,") ->",{asis,Atom},";",nl]),
    emit([{asis,FuncName},"(ExtensionNum) -> {asn1_enum, ExtensionNum}.",nl,nl]);
pgen_num2namefunc1(FuncName,[{Atom,Number}], _) ->
    emit([{asis,FuncName},"(",Number,") ->",{asis,Atom},".",nl,nl]);
pgen_num2namefunc1(FuncName,[{Atom,Number}|NNRest], EM) ->
    emit([{asis,FuncName},"(",Number,") ->",{asis,Atom},";",nl]),
    pgen_num2namefunc1(FuncName,NNRest, EM).

    

pgen_objects(_,_,_,[]) ->
    true;
pgen_objects(Rtmod,Erules,Module,[H|T]) ->
    asn1ct_name:clear(),
    Typedef = asn1_db:dbget(Module,H),
    Rtmod:gen_obj_code(Erules,Module,Typedef),
    pgen_objects(Rtmod,Erules,Module,T).

pgen_objectsets(_,_,_,[]) ->
    true;
pgen_objectsets(Rtmod,Erules,Module,[H|T]) ->
    asn1ct_name:clear(),
    TypeDef = asn1_db:dbget(Module,H),
    Rtmod:gen_objectset_code(Erules,TypeDef),
    pgen_objectsets(Rtmod,Erules,Module,T).

pgen_partial_decode(Rtmod, #gen{erule=ber}=Gen, Module) ->
    pgen_partial_inc_dec(Rtmod, Gen, Module),
    pgen_partial_dec(Rtmod, Gen, Module);
pgen_partial_decode(_, _, _) ->
    ok.

pgen_partial_inc_dec(Rtmod,Erules,Module) ->
    case asn1ct:get_gen_state_field(inc_type_pattern) of
	undefined ->
	    ok;
	ConfList -> 
	    PatternLists=lists:map(fun({_,P}) -> P end,ConfList),
	    pgen_partial_inc_dec1(Rtmod,Erules,Module,PatternLists),
	    gen_partial_inc_dec_refed_funcs(Rtmod,Erules)
    end.
    
%% pgen_partial_inc_dec1 generates a function of the toptype in each
%% of the partial incomplete decoded types.
pgen_partial_inc_dec1(Rtmod,Erules,Module,[P|Ps]) ->
    TopTypeName = asn1ct:partial_inc_dec_toptype(P),
    TypeDef=asn1_db:dbget(Module,TopTypeName),
    asn1ct_name:clear(),
    asn1ct:update_gen_state(namelist,P),
    asn1ct:update_gen_state(active,true),
    asn1ct:update_gen_state(prefix,"dec-inc-"),
    case asn1ct:maybe_saved_sindex(TopTypeName,P) of
	I when is_integer(I),I > 0 ->
	    asn1ct:set_current_sindex(I);
	_I ->
	    asn1ct:set_current_sindex(0),
	    ok
    end,
    Rtmod:gen_decode(Erules,TypeDef),
    gen_dec_part_inner_constr(Rtmod,Erules,TypeDef,[TopTypeName]),
    pgen_partial_inc_dec1(Rtmod,Erules,Module,Ps);
pgen_partial_inc_dec1(_,_,_,[]) ->
    ok.

gen_partial_inc_dec_refed_funcs(Rtmod, #gen{erule=ber}=Gen) ->
    case asn1ct:next_refed_func() of
	[] ->
	    ok;
	{#'Externaltypereference'{module=M,type=Name},Sindex,Pattern} ->
	    TypeDef = asn1_db:dbget(M,Name),
	    asn1ct:update_gen_state(namelist,Pattern),
	    asn1ct:set_current_sindex(Sindex),
	    Rtmod:gen_inc_decode(Gen, TypeDef),
	    gen_dec_part_inner_constr(Rtmod, Gen, TypeDef, [Name]),
	    gen_partial_inc_dec_refed_funcs(Rtmod, Gen);
	{Name,Sindex,Pattern,Type} ->
	    TypeDef=#typedef{name=asn1ct_gen:list2name(Name),typespec=Type},
	    asn1ct:update_gen_state(namelist,Pattern),
	    asn1ct:set_current_sindex(Sindex),
	    Rtmod:gen_inc_decode(Gen, TypeDef),
	    gen_dec_part_inner_constr(Rtmod, Gen, TypeDef, Name),
	    gen_partial_inc_dec_refed_funcs(Rtmod, Gen)
    end.

pgen_partial_dec(_Rtmod,Erules,_Module) ->
    Type_pattern = asn1ct:get_gen_state_field(type_pattern),
    %% Get the typedef of the top type and follow into the choosen
    %% components until the last type/component.
    pgen_partial_types(Erules,Type_pattern),
    ok.

pgen_partial_types(#gen{options=Options}=Gen, TypePattern)  ->
    %% until this functionality works on all back-ends
    case lists:member(asn1config, Options) of
	true ->
	    pgen_partial_types1(Gen, TypePattern);
	false ->
            ok
    end.


pgen_partial_types1(Erules,[{FuncName,[TopType|RestTypes]}|Rest]) ->
    CurrMod = get(currmod),
    TypeDef = asn1_db:dbget(CurrMod,TopType),
    traverse_type_structure(Erules,TypeDef,RestTypes,FuncName,
			    TypeDef#typedef.name),
    pgen_partial_types1(Erules,Rest);
pgen_partial_types1(_,[]) ->
    ok;
pgen_partial_types1(_,undefined) ->
    ok.

%% traverse_type_structure searches the structure of TypeDef for next
%% type/component in TypeList until the last one. For the last type in
%% TypeList a decode function will be generated.
traverse_type_structure(Erules,Type,[],FuncName,TopTypeName) ->
    %% this is the selected type
    Ctmod = ct_gen_module(Erules),
    TypeDef =
	case Type of
	    #type{} ->
		#typedef{name=TopTypeName,typespec=Type};
	    #typedef{} -> Type
	end,
    Ctmod:gen_decode_selected(Erules,TypeDef,FuncName); % what if Type is #type{}
traverse_type_structure(Erules,#type{def=Def},[[N]],FuncName,TopTypeName) 
  when is_integer(N) ->
    %% In this case a decode of one of the elements in the SEQUENCE OF is
    %% required.
    InnerType = asn1ct_gen:get_inner(Def),
    case InnerType of
	'SEQUENCE OF' ->
	    {_,Type} = Def,
	    traverse_type_structure(Erules,Type,[],FuncName,TopTypeName);
	WrongType ->
	    exit({error,{configuration_file_error,[N],"only for SEQUENCE OF components",WrongType}})
    end;
traverse_type_structure(Erules,Type,[[N]|Ts],FuncName,TopTypeName) 
  when is_integer(N)  ->
    traverse_type_structure(Erules,Type,Ts,FuncName,TopTypeName);
traverse_type_structure(Erules,#type{def=Def},[T|Ts],FuncName,TopTypeName)  ->
    InnerType = asn1ct_gen:get_inner(Def),
    case InnerType of
	'SET' ->
	    #'SET'{components=Components} = Def,
	    C = get_component(T,Components),
	    traverse_type_structure(Erules,C#'ComponentType'.typespec,Ts,
				    FuncName,[T|TopTypeName]);
	'SEQUENCE' ->
	    #'SEQUENCE'{components=Components} = Def,
	    C = get_component(T,Components),
	    traverse_type_structure(Erules,C#'ComponentType'.typespec,Ts,
				    FuncName,[T|TopTypeName]);
	'CHOICE' ->
	    {_,Components} = Def,
	    C = get_component(T,Components),
	    traverse_type_structure(Erules,C#'ComponentType'.typespec,Ts,
				    FuncName,[T|TopTypeName]);
	'SEQUENCE OF' ->
	    {_,Type} = Def,
	    traverse_SO_type_structure(Erules,Type,[T|Ts],FuncName,
				       TopTypeName);
	'SET OF' ->
	    {_,Type} = Def,
	    traverse_SO_type_structure(Erules,Type,[T|Ts],FuncName,
				       TopTypeName);
	#'Externaltypereference'{module=M,type=TName} ->
	    TypeDef = asn1_db:dbget(M,TName),
	    traverse_type_structure(Erules,TypeDef,[T|Ts],FuncName,
				    [TypeDef#typedef.name]);
	_ ->
	    traverse_type_structure(Erules,Def,Ts,FuncName,[T|TopTypeName])
    end;
traverse_type_structure(Erules,#typedef{typespec=Def},[T|Ts],FuncName,
			TopTypeName)  ->
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case InnerType of
	'SET' ->
	    #'SET'{components=Components} = Def#type.def,
	    C = get_component(T,Components),
	    traverse_type_structure(Erules,C#'ComponentType'.typespec,Ts,
				    FuncName,[T|TopTypeName]);
	'SEQUENCE' ->
	    #'SEQUENCE'{components=Components} = Def#type.def,
	    C = get_component(T,Components),
	    traverse_type_structure(Erules,C#'ComponentType'.typespec,Ts,
				    FuncName,[T|TopTypeName]);
	'CHOICE' ->
	    {_,Components} = Def#type.def,
	    C = get_component(T,Components),
	    traverse_type_structure(Erules,C#'ComponentType'.typespec,Ts,
				    FuncName,[T|TopTypeName]);
	'SEQUENCE OF' ->
	    {_,Type} = Def#type.def,
	    traverse_SO_type_structure(Erules,Type,[T|Ts],FuncName,
				       TopTypeName);
	'SET OF' ->
	    {_,Type} = Def#type.def,
	    traverse_SO_type_structure(Erules,Type,[T|Ts],FuncName,
				       TopTypeName);
	#'Externaltypereference'{module=M,type=TName} ->
	    TypeDef = asn1_db:dbget(M,TName),
	    traverse_type_structure(Erules,TypeDef,[T|Ts],FuncName,
				    [TypeDef#typedef.name]);
	_ ->
            %% This may be a referenced type that shall be traversed
            %% or the selected type
	    traverse_type_structure(Erules,Def,Ts,FuncName,[T|TopTypeName])
    end.
	    
traverse_SO_type_structure(Erules,Type,[N|Rest],FuncName,TopTypeName)
  when is_integer(N) ->
    traverse_type_structure(Erules,Type,Rest,FuncName,TopTypeName);
traverse_SO_type_structure(Erules,Type,TypeList,FuncName,TopTypeName) ->
    traverse_type_structure(Erules,Type,TypeList,FuncName,TopTypeName).

get_component(Name,{C1,C2}) when is_list(C1),is_list(C2) ->
    get_component(Name,C1++C2);
get_component(Name,[C=#'ComponentType'{name=Name}|_Cs]) ->
    C;
get_component(Name,[_C|Cs]) ->
    get_component(Name,Cs).

%% generate code for all inner types that are called from the top type
%% of the partial incomplete decode and are defined within the top
%% type.Constructed subtypes deeper in the structure will be generated
%% in turn after all top types have been generated.
gen_dec_part_inner_constr(Rtmod,Erules,TypeDef,TypeName) ->
    Def = TypeDef#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case InnerType of
	'SET' ->
	    #'SET'{components=Components} = Def#type.def,
	    gen_dec_part_inner_types(Rtmod,Erules,Components,TypeName);
	%%  Continue generate the inner of each component
	'SEQUENCE' ->
	    #'SEQUENCE'{components=Components} = Def#type.def,
	    gen_dec_part_inner_types(Rtmod,Erules,Components,TypeName);
	'CHOICE' ->
	    {_,Components} = Def#type.def,
	    gen_dec_part_inner_types(Rtmod,Erules,Components,TypeName);
	'SEQUENCE OF' ->
	    %% this and next case must be the last component in the
	    %% partial decode chain here. Not likely that this occur.
	    {_,Type} = Def#type.def,
	    NameSuffix = constructed_suffix(InnerType,Type#type.def),
	    asn1ct_name:clear(),
	    Rtmod:gen_decode(Erules,[NameSuffix|TypeName],Type);
%%	    gen_types(Erules,[NameSuffix|Typename],Type);
	'SET OF' ->
	    {_,Type} = Def#type.def,
	    NameSuffix = constructed_suffix(InnerType,Type#type.def),
	    asn1ct_name:clear(),
	    Rtmod:gen_decode(Erules,[NameSuffix|TypeName],Type);
	_ ->
	    ok
    end.

gen_dec_part_inner_types(Rtmod,Erules,[ComponentType|Rest],TypeName) ->
    asn1ct_name:clear(),
    Rtmod:gen_decode(Erules,TypeName,ComponentType),
    gen_dec_part_inner_types(Rtmod,Erules,Rest,TypeName);
gen_dec_part_inner_types(Rtmod,Erules,{Comps1,Comps2},TypeName)
  when is_list(Comps1),is_list(Comps2) ->
    gen_dec_part_inner_types(Rtmod,Erules,Comps1 ++ Comps2,TypeName);
gen_dec_part_inner_types(_,_,[],_) ->
    ok.


pgen_partial_incomplete_decode(Erule) ->
    case asn1ct:get_gen_state_field(active) of
	true ->
	    pgen_partial_incomplete_decode1(Erule),
	    asn1ct:reset_gen_state();
	_ ->
	    ok
    end.

pgen_partial_incomplete_decode1(#gen{erule=ber}) ->
    case asn1ct:read_config_data(partial_incomplete_decode) of
	undefined ->
	    ok;
	Data ->
	    lists:foreach(fun emit_partial_incomplete_decode/1,Data)
    end,
    GeneratedFs= asn1ct:get_gen_state_field(gen_refed_funcs),
    gen_part_decode_funcs(GeneratedFs,0);
pgen_partial_incomplete_decode1(#gen{}) -> ok.

emit_partial_incomplete_decode({FuncName,TopType,Pattern}) ->
    TypePattern = asn1ct:get_gen_state_field(inc_type_pattern),
    TPattern =
	case lists:keysearch(FuncName,1,TypePattern) of
	    {value,{_,TP}} -> TP;
	    _ -> exit({error,{asn1_internal_error,exclusive_decode}})
	end,
    TopTypeName =
	case asn1ct:maybe_saved_sindex(TopType,TPattern) of
	    I when is_integer(I),I>0 ->
		lists:concat([TopType,"_",I]);
	    _ ->
		atom_to_list(TopType)
	end,
    emit([{asis,FuncName},"(Bytes) ->",nl,
	  "  decode_partial_incomplete('",TopTypeName,"',Bytes,",{asis,Pattern},").",nl]);
emit_partial_incomplete_decode(D) ->
    throw({error,{asn1,{"bad data in asn1config file",D}}}).

gen_part_decode_funcs([Data={Name,_,_,Type}|GeneratedFs],N) ->
    InnerType = 
	case Type#type.def of
	    #'ObjectClassFieldType'{type=OCFTType} ->
		OCFTType;
	    _ ->
		get_inner(Type#type.def)
	end,
    WhatKind = type(InnerType),
    TypeName=list2name(Name),
    if
	N > 0 -> emit([";",nl]);
	true -> ok
    end,
    emit(["decode_inc_disp('",TypeName,"',Data) ->",nl]),
    gen_part_decode_funcs(WhatKind,TypeName,Data),
    gen_part_decode_funcs(GeneratedFs,N+1);
gen_part_decode_funcs([_H|T],N) ->
    gen_part_decode_funcs(T,N);
gen_part_decode_funcs([],N) ->
    if
	N > 0 ->
	    emit([".",nl]);
	true ->
	    ok
    end.

gen_part_decode_funcs(#'Externaltypereference'{module=M,type=T},
		      _TypeName,Data) ->
    #typedef{typespec=TS} = asn1_db:dbget(M,T),
    InnerType = 
	case TS#type.def of
	    #'ObjectClassFieldType'{type=OCFTType} ->
		OCFTType;
	    _ ->
		get_inner(TS#type.def)
	end,
    WhatKind = type(InnerType),
    gen_part_decode_funcs(WhatKind,[T],Data);
gen_part_decode_funcs({constructed,bif},TypeName,
		      {_Name,parts,Tag,_Type}) ->
    emit(["  case Data of",nl,
	  "    L when is_list(L) ->",nl,
	  "      'dec_",TypeName,"'(lists:map(fun(X) -> element(1, ",
	  {call,ber,ber_decode_erlang,["X"]},") end, L),",{asis,Tag},");",nl,
	  "    _ ->",nl,
	  "      [Res] = 'dec_",TypeName,"'([Data],",{asis,Tag},"),",nl,
	  "      Res",nl,
	  "  end"]);
gen_part_decode_funcs(WhatKind,_TypeName,{_Name,parts,_Tag,_Type}) ->
    throw({error,{asn1,{"only SEQUENCE OF/SET OF may have the partial incomplete directive 'parts'.",WhatKind}}});
gen_part_decode_funcs({constructed,bif},TypeName,
		      {_Name,undecoded,Tag,_Type}) ->
    emit(["  'dec_",TypeName,"'(Data,",{asis,Tag},")"]);
gen_part_decode_funcs({primitive,bif},_TypeName,
		      {_Name,undecoded,Tag,Type}) ->
    asn1ct_gen_ber_bin_v2:gen_dec_prim(Type, "Data", Tag);
gen_part_decode_funcs(WhatKind,_TypeName,{_,Directive,_,_}) ->
    throw({error,{asn1,{"Not implemented yet",WhatKind," partial incomplete directive:",Directive}}}).

%% EncDec = 'gen_encode' | 'gen_decode'
gen_types(Erules, Tname, {RootL1,ExtList,RootL2}, EncDec)
  when is_list(RootL1), is_list(RootL2) ->
    gen_types(Erules, Tname, RootL1, EncDec),
    Rtmod = ct_gen_module(Erules),
    gen_types(Erules, Tname, Rtmod:extaddgroup2sequence(ExtList), EncDec),
    gen_types(Erules, Tname, RootL2, EncDec);
gen_types(Erules, Tname, {RootList,ExtList}, EncDec) when is_list(RootList) ->
    gen_types(Erules, Tname, RootList, EncDec),
    Rtmod = ct_gen_module(Erules),
    gen_types(Erules, Tname, Rtmod:extaddgroup2sequence(ExtList), EncDec);
gen_types(Erules, Tname, [{'EXTENSIONMARK',_,_}|T], EncDec) ->
    gen_types(Erules, Tname, T, EncDec);
gen_types(Erules, Tname, [ComponentType|T], EncDec) ->
    asn1ct_name:clear(),
    Rtmod = ct_gen_module(Erules),
    Rtmod:EncDec(Erules, Tname, ComponentType),
    gen_types(Erules, Tname, T, EncDec);
gen_types(_, _, [], _) ->
    ok;
gen_types(Erules, Tname, #type{}=Type, EncDec) ->
    asn1ct_name:clear(),
    Rtmod = ct_gen_module(Erules),
    Rtmod:EncDec(Erules, Tname, Type).

%% VARIOUS GENERATOR STUFF 
%% *************************************************
%%**************************************************

mk_var(X) when is_atom(X) ->
    list_to_atom(mk_var(atom_to_list(X)));

mk_var([H|T]) ->
    [H-32|T].

%% Since hyphens are allowed in ASN.1 names, it may occur in a
%% variable to. Turn a hyphen into a under-score sign.
un_hyphen_var(X) when is_atom(X) ->
    list_to_atom(un_hyphen_var(atom_to_list(X)));
un_hyphen_var([45|T]) ->
    [95|un_hyphen_var(T)];
un_hyphen_var([H|T]) ->
    [H|un_hyphen_var(T)];
un_hyphen_var([]) ->
    [].

gen_encode_constructed(Erules,Typename,InnerType,D) when is_record(D,type) ->
    Rtmod = ct_constructed_module(Erules),
    case InnerType of
	'SET' ->
	    Rtmod:gen_encode_set(Erules,Typename,D),
	    #'SET'{components=Components} = D#type.def,
	    gen_types(Erules, Typename, Components, gen_encode);
	'SEQUENCE' ->
	    Rtmod:gen_encode_sequence(Erules,Typename,D),
	    #'SEQUENCE'{components=Components} = D#type.def,
	    gen_types(Erules, Typename, Components, gen_encode);
	'CHOICE' ->
	    Rtmod:gen_encode_choice(Erules,Typename,D),
	    {_,Components} = D#type.def,
	    gen_types(Erules, Typename, Components, gen_encode);
	'SEQUENCE OF' ->
	    Rtmod:gen_encode_sof(Erules,Typename,InnerType,D),
	    {_,Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType,Type#type.def),
	    gen_types(Erules, [NameSuffix|Typename], Type, gen_encode);
	'SET OF' ->
	    Rtmod:gen_encode_sof(Erules,Typename,InnerType,D),
	    {_,Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType,Type#type.def),
	    gen_types(Erules, [NameSuffix|Typename], Type, gen_encode)
    end;
gen_encode_constructed(Erules,Typename,InnerType,D) 
  when is_record(D,typedef) ->
    gen_encode_constructed(Erules,Typename,InnerType,D#typedef.typespec).

gen_decode_constructed(Erules,Typename,InnerType,D) when is_record(D,type) ->
    Rtmod = ct_constructed_module(Erules),
    asn1ct:step_in_constructed(), %% updates namelist for exclusive decode
    case InnerType of
	'SET' ->
	    Rtmod:gen_decode_set(Erules,Typename,D),
	    #'SET'{components=Components} = D#type.def,
	    gen_types(Erules, Typename, Components, gen_decode);
	'SEQUENCE' ->
	    Rtmod:gen_decode_sequence(Erules,Typename,D),
	    #'SEQUENCE'{components=Components} = D#type.def,
	    gen_types(Erules, Typename, Components, gen_decode);
	'CHOICE' ->
	    Rtmod:gen_decode_choice(Erules,Typename,D),
	    {_,Components} = D#type.def,
	    gen_types(Erules, Typename, Components, gen_decode);
	'SEQUENCE OF' ->
	    Rtmod:gen_decode_sof(Erules,Typename,InnerType,D),
	    {_,#type{def=Def}=Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType, Def),
	    gen_types(Erules, [NameSuffix|Typename], Type, gen_decode);
	'SET OF' ->
	    Rtmod:gen_decode_sof(Erules,Typename,InnerType,D),
	    {_,#type{def=Def}=Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType, Def),
	    gen_types(Erules, [NameSuffix|Typename], Type, gen_decode)
    end;

gen_decode_constructed(Erules,Typename,InnerType,D) when is_record(D,typedef) ->
    gen_decode_constructed(Erules,Typename,InnerType,D#typedef.typespec).


pgen_exports(#gen{options=Options}=Gen, Code) ->
    #abst{types=Types,values=Values,objects=Objects,objsets=ObjectSets} = Code,
    emit(["-export([encoding_rule/0,maps/0,bit_string_format/0,",nl,
	  "         legacy_erlang_types/0]).",nl]),
    emit(["-export([",{asis,?SUPPRESSION_FUNC},"/1]).",nl]),
    case Gen of
        #gen{erule=ber} ->
            gen_exports(Types, "enc_", 2),
            gen_exports(Types, "dec_", 2),
            gen_exports(Objects, "enc_", 3),
            gen_exports(Objects, "dec_", 3),
            gen_exports(ObjectSets, "getenc_", 1),
            gen_exports(ObjectSets, "getdec_", 1);
        #gen{erule=per} ->
            gen_exports(Types, "enc_", 1),
            gen_exports(Types, "dec_", 1)
    end,

    A2nNames = [X || {n2n,X} <- Options],
    gen_exports(A2nNames, "name2num_", 1),
    gen_exports(A2nNames, "num2name_", 1),

    gen_exports(Values, "", 0),
    emit(["-export([info/0]).",nl,nl]),
    gen_partial_inc_decode_exports(),
    gen_selected_decode_exports().

gen_partial_inc_decode_exports() ->
    case {asn1ct:read_config_data(partial_incomplete_decode),
	  asn1ct:get_gen_state_field(inc_type_pattern)}  of
	{undefined,_} ->
	    ok;
	{_,undefined} ->
	    ok;
	{Data0,_} ->
            Data = [Name || {Name,_,_} <- Data0],
            gen_exports(Data, "", 1),
            emit(["-export([decode_part/2]).",nl,nl])
    end.

gen_selected_decode_exports() ->
    case asn1ct:get_gen_state_field(type_pattern) of
	undefined ->
	    ok;
	Data0 ->
            Data = [Name || {Name,_} <- Data0],
            gen_exports(Data, "", 1)
    end.

gen_exports([], _Prefix, _Arity) ->
    ok;
gen_exports([_|_]=L0, Prefix, Arity) ->
    FF = fun(F0) ->
                 F = list_to_atom(lists:concat([Prefix,F0])),
                 [{asis,F},"/",Arity]
         end,
    L = lists:join(",\n", [FF(F) || F <- L0]),
    emit(["-export([",nl,
          L,nl,
          "]).",nl,nl]).

pgen_dispatcher(Erules, []) ->
    gen_info_functions(Erules);
pgen_dispatcher(Gen, Types) ->
    %% MODULE HEAD
    emit(["-export([encode/2,decode/2]).",nl,nl]),
    gen_info_functions(Gen),

    Options = Gen#gen.options,
    NoFinalPadding = lists:member(no_final_padding, Options),
    NoOkWrapper = proplists:get_bool(no_ok_wrapper, Options),

    %% ENCODER
    Call = case Gen of
	       #gen{erule=per,aligned=true} ->
		   asn1ct_func:need({per,complete,1}),
		   "complete(encode_disp(Type, Data))";
	       #gen{erule=ber} ->
		   "iolist_to_binary(element(1, encode_disp(Type, Data)))";
	       #gen{erule=per,aligned=false} when NoFinalPadding ->
		   asn1ct_func:need({uper,complete_NFP,1}),
		   "complete_NFP(encode_disp(Type, Data))";
	       #gen{erule=per,aligned=false} ->
		   asn1ct_func:need({uper,complete,1}),
		   "complete(encode_disp(Type, Data))"
	   end,

    emit(["encode(Type, Data) ->",nl]),
    case NoOkWrapper of
	true ->
	    emit(["  ",Call,"."]);
	false ->
	    emit(["try ",Call," of",nl,
		  "  Bytes ->",nl,
		  "    {ok,Bytes}",nl,
		  try_catch()])
    end,
    emit([nl,nl]),

    %% DECODER
    ReturnRest = proplists:get_bool(undec_rest, Gen#gen.options),
    Data = case Gen#gen.erule =:= ber andalso ReturnRest of
	       true -> "Data0";
	       false -> "Data"
	   end,

    emit(["decode(Type, ",Data,") ->",nl]),

    case NoOkWrapper of
        false -> emit(["try",nl]);
        true -> ok
    end,

    DecWrap =
	case {Gen,ReturnRest} of
	    {#gen{erule=ber},false} ->
		asn1ct_func:need({ber,ber_decode_nif,1}),
		"element(1, ber_decode_nif(Data))";
	    {#gen{erule=ber},true} ->
		asn1ct_func:need({ber,ber_decode_nif,1}),
		emit(["   {Data,Rest} = ber_decode_nif(Data0),",nl]),
		"Data";
	    {_,_} ->
		"Data"
	end,

    DecodeDisp = ["decode_disp(Type, ",DecWrap,")"],
    case {Gen,ReturnRest} of
	{#gen{erule=ber},true} ->
	    emit(["   Result = ",DecodeDisp,",",nl]),
            result_line(NoOkWrapper, ["Result","Rest"]);
	{#gen{erule=ber},false} ->
	    emit(["   Result = ",DecodeDisp,",",nl]),
            result_line(NoOkWrapper, ["Result"]);


	{#gen{erule=per},true} ->
	    emit(["   {Result,Rest} = ",DecodeDisp,",",nl]),
            result_line(NoOkWrapper, ["Result","Rest"]);
	{#gen{erule=per},false} ->
	    emit(["   {Result,_Rest} = ",DecodeDisp,",",nl]),
            result_line(NoOkWrapper, ["Result"])
    end,

    case NoOkWrapper of
	false ->
	    emit([nl,try_catch(),nl,nl]);
	true ->
	    emit([".",nl,nl])
    end,

    %% REST of MODULE
    gen_decode_partial_incomplete(Gen),
    gen_partial_inc_dispatcher(Gen),

    gen_dispatcher(Types, "encode_disp", "enc_"),
    gen_dispatcher(Types, "decode_disp", "dec_").

result_line(NoOkWrapper, Items) ->
    S = ["   "|case NoOkWrapper of
		    false -> result_line_1(["ok"|Items]);
		    true -> result_line_1(Items)
		end],
    emit(lists:flatten(S)).

result_line_1([SingleItem]) ->
    SingleItem;
result_line_1(Items) ->
    ["{",string:join(Items, ","),"}"].

try_catch() ->
    ["  catch",nl,
     "    Class:Exception when Class =:= error; Class =:= exit ->",nl,
     "      Stk = erlang:get_stacktrace(),",nl,
     "      case Exception of",nl,
     "        {error,{asn1,Reason}} ->",nl,
     "          {error,{asn1,{Reason,Stk}}};",nl,
     "        Reason ->",nl,
     "         {error,{asn1,{Reason,Stk}}}",nl,
     "      end",nl,
     "end."].

gen_info_functions(Gen) ->
    Erule = case Gen of
                #gen{erule=ber} -> ber;
                #gen{erule=per,aligned=false} -> uper;
                #gen{erule=per,aligned=true} -> per
            end,
    Maps = case Gen of
               #gen{pack=record} -> false;
               #gen{pack=map} -> true
           end,
    emit(["encoding_rule() -> ",
	  {asis,Erule},".",nl,nl,
          "maps() -> ",
          {asis,Maps},".",nl,nl,
	  "bit_string_format() -> ",
	  {asis,asn1ct:get_bit_string_format()},".",nl,nl,
	  "legacy_erlang_types() -> ",
	  {asis,asn1ct:use_legacy_types()},".",nl,nl]).

gen_decode_partial_incomplete(#gen{erule=ber}) ->
    case {asn1ct:read_config_data(partial_incomplete_decode),
	  asn1ct:get_gen_state_field(inc_type_pattern)} of
	{undefined,_} ->
	    ok;
	{_,undefined} ->
	    ok;
	_ ->
	    EmitCaseClauses =
		fun() ->
			emit(["   {'EXIT',{error,Reason}} ->",nl,
			      "      {error,Reason};",nl,
			      "    {'EXIT',Reason} ->",nl,
			      "      {error,{asn1,Reason}};",nl,
			      "    Result ->",nl,
			      "      {ok,Result}",nl,
			      "  end"])
		end,
	    emit(["decode_partial_incomplete(Type,Data0,",
		  "Pattern) ->",nl]),
	    emit(["  {Data,_RestBin} =",nl,
		  "    ",{call,ber,decode_primitive_incomplete,
			  ["Pattern","Data0"]},com,nl,
		  "  case catch decode_partial_inc_disp(Type,",
		  "Data) of",nl]),
	    EmitCaseClauses(),
	    emit([".",nl,nl]),
	    emit(["decode_part(Type, Data0) "
		  "when is_binary(Data0) ->",nl]),
	    emit(["  case catch decode_inc_disp(Type,element(1, ",
		  {call,ber,ber_decode_nif,["Data0"]},")) of",nl]),
	    EmitCaseClauses(),
	    emit([";",nl]),
	    emit(["decode_part(Type, Data0) ->",nl]),
	    emit(["  case catch decode_inc_disp(Type, Data0) of",nl]),
	    EmitCaseClauses(),
	    emit([".",nl,nl])
    end;
gen_decode_partial_incomplete(#gen{}) ->
    ok.

gen_partial_inc_dispatcher(#gen{erule=ber}) ->
    case {asn1ct:read_config_data(partial_incomplete_decode),
	  asn1ct:get_gen_state_field(inc_type_pattern)} of
	{undefined,_} ->
	    ok;
	{_,undefined} ->
	    ok;
	{Data1,Data2} ->
	    gen_partial_inc_dispatcher(Data1, Data2, "")
    end;
gen_partial_inc_dispatcher(#gen{}) ->
    ok.

gen_partial_inc_dispatcher([{FuncName,TopType,_Pattern}|Rest], TypePattern, Sep) ->
    TPattern =
	case lists:keysearch(FuncName,1,TypePattern) of
	    {value,{_,TP}} -> TP;
	    _ -> exit({error,{asn1_internal_error,exclusive_decode}})
	end,
    FuncName2=asn1ct:maybe_rename_function(inc_disp,TopType,TPattern),
    TopTypeName =
	case asn1ct:maybe_saved_sindex(TopType,TPattern) of
	    I when is_integer(I),I>0 ->
		lists:concat([TopType,"_",I]);
	    _ ->
		atom_to_list(TopType)
	end,
    emit([Sep,
	  "decode_partial_inc_disp('",TopTypeName,"',Data) ->",nl,
	  "  ",{asis,list_to_atom(lists:concat(["dec-inc-",FuncName2]))},
	  "(Data)"]),
    gen_partial_inc_dispatcher(Rest, TypePattern, ";\n");
gen_partial_inc_dispatcher([], _, _) ->
    emit([".",nl]).

gen_dispatcher(L, DispFunc, Prefix) ->
    gen_dispatcher_1(L, DispFunc, Prefix),
    emit([DispFunc,"(","Type",", _Data) ->"
          " exit({error,{asn1,{undefined_type,Type}}}).",nl,nl]).

gen_dispatcher_1([F|T], FuncName, Prefix) ->
    Func = list_to_atom(lists:concat([Prefix,F])),
    emit([FuncName,"(",{asis,F},", Data) -> ",
          {asis,Func},"(Data)",";",nl]),
    gen_dispatcher_1(T, FuncName, Prefix);
gen_dispatcher_1([], _, _) ->
    ok.

pgen_info() ->
    emit(["info() ->",nl,
	  "   case ?MODULE:module_info(attributes) of",nl,
	  "     Attributes when is_list(Attributes) ->",nl,
	  "       case lists:keyfind(asn1_info, 1, Attributes) of",nl,
	  "         {_,Info} when is_list(Info) ->",nl,
	  "           Info;",nl,
	  "         _ ->",nl,
	  "           []",nl,
	  "       end;",nl,
	  "     _ ->",nl,
	  "       []",nl,
	  "   end.",nl]).

open_hrl(OutFile,Module) ->
    File = lists:concat([OutFile,".hrl"]),
    _ = open_output_file(File),
    gen_hrlhead(Module),
    Protector = hrl_protector(OutFile),
    emit(["-ifndef(",Protector,").\n",
	  "-define(",Protector,", true).\n"
	  "\n"]).

hrl_protector(OutFile) ->
    BaseName = filename:basename(OutFile),
    P = "_" ++ string:to_upper(BaseName) ++ "_HRL_",
    [if
	 $A =< C, C =< $Z -> C;
	 $a =< C, C =< $a -> C;
	 $0 =< C, C =< $9 -> C;
	 true -> $_
     end || C <- P].


emit(Term) ->
    ok = file:write(get(gen_file_out), do_emit(Term)).

do_emit({prev,Variable}) when is_atom(Variable) ->
    do_emit({var,asn1ct_name:prev(Variable)});
do_emit({next,Variable}) when is_atom(Variable) ->
    do_emit({var,asn1ct_name:next(Variable)});
do_emit({curr,Variable}) when is_atom(Variable) ->
    do_emit({var,asn1ct_name:curr(Variable)});
do_emit({var,Variable}) when is_atom(Variable) ->
    [Head|V] = atom_to_list(Variable),
    [Head-32|V];
do_emit({asis,What}) ->
    io_lib:format("~w", [What]);
do_emit({call,M,F,A}) ->
    MFA = {M,F,length(A)},
    asn1ct_func:need(MFA),
    [atom_to_list(F),"(",call_args(A, "")|")"];
do_emit(nl) ->
    "\n";
do_emit(com) ->
    ",";
do_emit([C|_]=Str) when is_integer(C) ->
    Str;
do_emit([_|_]=L) ->
    [do_emit(E) || E <- L];
do_emit([]) ->
    [];
do_emit(What) when is_integer(What) ->
    integer_to_list(What);
do_emit(What) when is_atom(What) ->
    atom_to_list(What).


call_args([A|As], Sep) ->
    [Sep,do_emit(A)|call_args(As, ", ")];
call_args([], _) -> [].

open_output_file(F) ->
    case file:open(F, [write,raw,delayed_write]) of
	{ok,Fd} ->
	    put(gen_file_out, Fd),
	    Fd;
	{error, Reason} ->
	    io:format("** Can't open file ~p ~n", [F]),
	    exit({error,Reason})
    end.

close_output_file() ->
    ok = file:close(erase(gen_file_out)).

pgen_hrl(#gen{pack=record}=Gen, Code) ->
    #abst{name=Module,types=Types,values=Values,ptypes=Ptypes} = Code,
    Ret =
	case pgen_hrltypes(Gen, Module, Ptypes++Types, 0) of
	    0 -> 
		case Values of
		    [] ->
			0;
		    _ ->
			open_hrl(get(outfile), Module),
			pgen_macros(Gen, Module, Values),
			1
		end;
	    X ->
		pgen_macros(Gen, Module, Values),
		X
	end,
    case Ret of
	0 ->
	    0;
	Y ->
	    Protector = hrl_protector(get(outfile)),
	    emit(["-endif. %% ",Protector,"\n"]),
	    close_output_file(),
	    asn1ct:verbose("--~p--~n",
			   [{generated,lists:concat([get(outfile),".hrl"])}],
			   Gen),
	    Y
    end;
pgen_hrl(#gen{pack=map}, _) ->
    0.

pgen_macros(_,_,[]) ->
    true;
pgen_macros(Gen, Module, [H|T]) ->
    Valuedef = asn1_db:dbget(Module, H),
    gen_macro(Gen, Valuedef),
    pgen_macros(Gen, Module, T).

pgen_hrltypes(_,_,[],NumRecords) ->
    NumRecords;
pgen_hrltypes(Gen, Module, [H|T], NumRecords) ->
    Typedef = asn1_db:dbget(Module, H),
    AddNumRecords = gen_record(Gen, Typedef, NumRecords),
    pgen_hrltypes(Gen, Module, T, NumRecords+AddNumRecords).


%% Generates a macro for value Value defined in the ASN.1 module
gen_macro(Gen, #valuedef{name=Name,value=Value}) ->
    Prefix = get_macro_name_prefix(Gen),
    emit(["-define('",Prefix,Name,"', ",{asis,Value},").",nl]).

%% Generate record functions **************
%% Generates an Erlang record for each named and unnamed SEQUENCE and SET in the ASN.1 
%% module. If no SEQUENCE or SET is found there is no .hrl file generated


gen_record(Gen, #typedef{}=Tdef, NumRecords) ->
    Name = [Tdef#typedef.name],
    Type = Tdef#typedef.typespec,
    gen_record(Gen, type, Name, Type, NumRecords);
gen_record(Gen, #ptypedef{}=Tdef, NumRecords) ->
    Name = [Tdef#ptypedef.name],
    Type = Tdef#ptypedef.typespec,
    gen_record(Gen, ptype, Name, Type, NumRecords).

gen_record(Gen, TorPtype, Name,
           [#'ComponentType'{name=Cname,typespec=Type}|T], Num) ->
    Num2 = gen_record(Gen, TorPtype, [Cname|Name], Type, Num),
    gen_record(Gen, TorPtype, Name, T, Num2);
gen_record(Gen, TorPtype, Name, {Clist1,Clist2}, Num)
  when is_list(Clist1), is_list(Clist2) ->
    gen_record(Gen, TorPtype, Name, Clist1++Clist2, Num);
gen_record(Gen, TorPtype, Name, {Clist1,EClist,Clist2}, Num)
  when is_list(Clist1), is_list(EClist), is_list(Clist2) ->
    gen_record(Gen, TorPtype, Name, Clist1++EClist++Clist2, Num);
gen_record(Gen, TorPtype, Name, [_|T], Num) -> % skip EXTENSIONMARK
    gen_record(Gen, TorPtype, Name, T, Num);
gen_record(_Gen, _TorPtype, _Name, [], Num) ->
    Num;
gen_record(Gen, TorPtype, Name, #type{}=Type, Num) ->
    Def = Type#type.def,
    Rec = case Def of
	      Seq when is_record(Seq,'SEQUENCE') ->
		  case Seq#'SEQUENCE'.pname of
		      false ->
			  {record,Seq#'SEQUENCE'.components};
		      _ ->
			  {record,Seq#'SEQUENCE'.components}
		  end;
	      Set when is_record(Set,'SET') ->
		  case Set#'SET'.pname of
		      false ->
			  {record,to_textual_order(Set#'SET'.components)};
		      _Pname when TorPtype == type ->
			  false;
		      _ ->
			  {record,to_textual_order(Set#'SET'.components)}
		  end;
	      {'CHOICE',_CompList} -> {inner,Def};
	      {'SEQUENCE OF',_CompList} -> {['SEQOF'|Name],Def};
	      {'SET OF',_CompList} -> {['SETOF'|Name],Def};
	      _ -> false
    end,
    case Rec of
	false -> Num;
	{record,CompList} ->
	    case Num of
		0 -> open_hrl(get(outfile),get(currmod));
		_ -> true
	    end,
            do_gen_record(Gen, Name, CompList),
	    NewCompList =
		case CompList of
		    {CompList1,[]} ->
			CompList1;
		    {Tr,ExtensionList2} ->
			Tr ++ ExtensionList2;
		    {Rootl1,Extl,Rootl2} ->
			Rootl1++Extl++Rootl2;
		    _ ->
			CompList
		end,
	    gen_record(Gen, TorPtype, Name, NewCompList, Num+1);
	{inner,{'CHOICE', CompList}} ->
	    gen_record(Gen, TorPtype, Name, CompList, Num);
	{NewName,{_, CompList}} ->
	    gen_record(Gen, TorPtype, NewName, CompList, Num)
    end;
gen_record(_, _, _, _, NumRecords) ->        % skip CLASS etc for now.
     NumRecords.

do_gen_record(Gen, Name, CL0) ->
    CL = case CL0 of
             {Root,[]} ->
                 Root ++ [{comment,"with extension mark"}];
             {Root,Ext} ->
                 Root ++ [{comment,"with exensions"}] ++
                     only_components(Ext);
             {Root1,Ext,Root2} ->
                 Root1 ++ [{comment,"with exensions"}] ++
                     only_components(Ext) ++
                     [{comment,"end of extensions"}] ++ Root2;
             _ when is_list(CL0) ->
                 CL0
         end,
    Prefix = get_record_name_prefix(Gen),
    emit(["-record('",Prefix,list2name(Name),"', {"] ++
             do_gen_record_1(CL) ++
             [nl,"}).",nl,nl]).

only_components(CL) ->
    [C || #'ComponentType'{}=C <- CL].

do_gen_record_1([#'ComponentType'{name=Name,prop=Prop}|T]) ->
    Val = case Prop of
              'OPTIONAL' ->
                  " = asn1_NOVALUE";
              {'DEFAULT',_} ->
                  " = asn1_DEFAULT";
              _ ->
                  []
          end,
    Com = case needs_trailing_comma(T) of
        true -> [com];
        false -> []
    end,
    [nl,"  ",{asis,Name},Val,Com|do_gen_record_1(T)];
do_gen_record_1([{comment,Text}|T]) ->
    [nl,"  %% ",Text|do_gen_record_1(T)];
do_gen_record_1([]) ->
    [].

needs_trailing_comma([#'ComponentType'{}|_]) -> true;
needs_trailing_comma([_|T]) -> needs_trailing_comma(T);
needs_trailing_comma([]) -> false.

gen_head(#gen{options=Options}=Gen, Mod, Hrl) ->
    Name = case Gen of
               #gen{erule=per,aligned=false} ->
                   "PER (unaligned)";
               #gen{erule=per,aligned=true} ->
                   "PER (aligned)";
               #gen{erule=ber} ->
                   "BER"
           end,
    emit(["%% Generated by the Erlang ASN.1 ",Name,
          " compiler. Version: ",asn1ct:vsn(),nl,
          "%% Purpose: Encoding and decoding of the types in ",
          Mod,".",nl,nl,
          "-module('",Mod,"').",nl,
          "-compile(nowarn_unused_vars).",nl,
          "-dialyzer(no_improper_lists).",nl]),
    case Hrl of
	0 -> ok;
	_ -> emit(["-include(\"",Mod,".hrl\").",nl])
    end,
    emit(["-asn1_info([{vsn,'",asn1ct:vsn(),"'},",nl,
	  "            {module,'",Mod,"'},",nl,
	  "            {options,",io_lib:format("~p",[Options]),"}]).",nl,nl]).


gen_hrlhead(Mod) ->
    emit(["%% Generated by the Erlang ASN.1 compiler. Version: ",
          asn1ct:vsn(),nl,
          "%% Purpose: Erlang record definitions for each named and unnamed",nl,
          "%% SEQUENCE and SET, and macro definitions for each value",nl,
          "%% definition in module ",Mod,".",nl,nl]).

%% May only be a list or a two-tuple.
to_textual_order({Root,Ext}) ->
    {to_textual_order(Root),Ext};
to_textual_order(Cs={_R1,_Ext,_R2}) ->
    Cs;
to_textual_order(Cs=[#'ComponentType'{textual_order=undefined}|_]) ->
    Cs;
to_textual_order(Cs) when is_list(Cs) ->
    lists:keysort(#'ComponentType'.textual_order,Cs).
    
insert_once(Table,Object) ->
    case asn1ct_table:lookup(Table, element(1, Object)) of
	[] ->
	    asn1ct_table:insert(Table, Object); %returns true
	_ -> false
    end.

unify_if_string(PrimType) ->
    case PrimType of
	'NumericString' ->
	    restrictedstring;
	'PrintableString' ->
	    restrictedstring;
	'TeletexString' ->
	    restrictedstring;
	'T61String' ->
	    restrictedstring;
	'VideotexString' ->
	    restrictedstring;
	'IA5String' ->
	    restrictedstring;
	'UTCTime' ->
	    restrictedstring;
	'GeneralizedTime' ->
	    restrictedstring;
	'GraphicString' ->
	    restrictedstring;
	'VisibleString' ->
	    restrictedstring;
	'GeneralString' ->
	    restrictedstring;
	'UniversalString' ->
	    restrictedstring;
	'BMPString' ->
	    restrictedstring;
	'UTF8String' ->
	    restrictedstring;
	Other -> Other
    end.

conform_value(#type{def={'BIT STRING',[]}}, Bs) ->
    case asn1ct:get_bit_string_format() of
	compact when is_binary(Bs) ->
	    {0,Bs};
	compact when is_bitstring(Bs) ->
	    Sz = bit_size(Bs),
	    Unused = 8 - bit_size(Bs),
	    {Unused,<<Bs:Sz/bits,0:Unused>>};
	legacy ->
	    [B || <<B:1>> <= Bs];
	bitstring when is_bitstring(Bs) ->
	    Bs
    end;
conform_value(#type{def='OCTET STRING'}, String) ->
    case asn1ct:use_legacy_types() of
	false -> String;
	true -> binary_to_list(String)
    end;
conform_value(_, Value) -> Value.

named_bitstring_value(List, Names) ->
    Int = lists:foldl(fun(N, A) ->
			      {N,Pos} = lists:keyfind(N, 1, Names),
			      A bor (1 bsl Pos)
		      end, 0, List),
    named_bitstring_value_1(<<>>, Int).

named_bitstring_value_1(Bs, 0) ->
    Bs;
named_bitstring_value_1(Bs, Int) ->
    B = Int band 1,
    named_bitstring_value_1(<<Bs/bitstring,B:1>>, Int bsr 1).

get_inner(A) when is_atom(A) -> A;    
get_inner(Ext) when is_record(Ext,'Externaltypereference') -> Ext;    
get_inner({fixedtypevaluefield,_,Type}) ->
    if 
	is_record(Type,type) ->
	    get_inner(Type#type.def);
	true ->
	    get_inner(Type)
    end;
get_inner({typefield,TypeName}) ->
    TypeName;
get_inner(#'ObjectClassFieldType'{type=Type}) ->
    Type;
get_inner(T) when is_tuple(T) -> 
    case element(1,T) of
	Tuple when is_tuple(Tuple),element(1,Tuple) == objectclass ->
	    case catch(lists:last(element(2,T))) of
		{valuefieldreference,FieldName} ->
		    get_fieldtype(element(2,Tuple),FieldName);
		{typefieldreference,FieldName} ->
		    get_fieldtype(element(2,Tuple),FieldName)
	    end;
	_ -> element(1,T)
    end.





type(X) when is_record(X,'Externaltypereference') ->
    X;
type('ASN1_OPEN_TYPE') ->
    'ASN1_OPEN_TYPE';
type({fixedtypevaluefield,_Name,Type}) when is_record(Type,type) ->
    type(get_inner(Type#type.def));
type({typefield,_}) ->
    'ASN1_OPEN_TYPE';
type(X) ->
    case prim_bif(X) of
	true ->
	    {primitive,bif};
	false ->
	    case construct_bif(X) of
		true ->
		    {constructed,bif};
		false ->
		    {undefined,user}
	    end
    end.

prim_bif(X) ->
    lists:member(X,['INTEGER' ,
		    'ENUMERATED',
		    'REAL',
		    'OBJECT IDENTIFIER',
		    'RELATIVE-OID',
		    'NULL',
		    'BIT STRING' ,
		    'OCTET STRING' ,
		    'ObjectDescriptor',
		    'NumericString',
		    'TeletexString',
		    'T61String',
		    'VideotexString',
		    'UTCTime',
		    'GeneralizedTime',
		    'GraphicString',
		    'VisibleString',
		    'GeneralString',
		    'PrintableString',
		    'IA5String',
		    'UniversalString',
		    'UTF8String',
		    'BMPString',
		    'ENUMERATED',
		    'BOOLEAN']).

construct_bif(T) ->
    lists:member(T,['SEQUENCE' ,
		    'SEQUENCE OF' ,
		    'CHOICE' ,
		    'SET' ,
		    'SET OF']).

def_to_tag(#tag{class=Class,number=Number}) ->
    {Class,Number};
def_to_tag(#'ObjectClassFieldType'{type=Type}) -> 
   case Type of
       T when is_tuple(T),element(1,T)==fixedtypevaluefield ->
	   {'UNIVERSAL',get_inner(Type)};
       _ ->
	   []
   end;
def_to_tag(Def) ->
    {'UNIVERSAL',get_inner(Def)}.
    

%% Information Object Class

get_fieldtype([],_FieldName)->
    {no_type,no_name};
get_fieldtype([Field|Rest],FieldName) ->
    case element(2,Field) of
	FieldName ->
	    case element(1,Field) of
		fixedtypevaluefield ->
		    {element(1,Field),FieldName,element(3,Field)};
		_ ->
		    {element(1,Field),FieldName}
	    end;
	_  ->
	    get_fieldtype(Rest,FieldName)
    end.

%% Information Object Class

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert a list of name parts to something that can be output by emit
%% 
%% used to output function names in generated code.

list2name(L) ->
    NewL = list2name1(L),
    lists:concat(lists:reverse(NewL)).

list2name1([{ptype,H1},H2|T]) ->
    [H1,"_",list2name([H2|T])];
list2name1([H1,H2|T]) ->
    [H1,"_",list2name([H2|T])];
list2name1([{ptype,H}|_T]) ->
    [H];
list2name1([H|_T]) ->
    [H];
list2name1([]) ->
    [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert a list of name parts to something that can be output by emit
%% stops at {ptype,Pname} i.e Pname whill be the first part of the name
%% used to output record names in generated code.

list2rname(L) ->
    NewL = list2rname1(L),
    lists:concat(lists:reverse(NewL)).

list2rname1([{ptype,H1},_H2|_T]) ->
    [H1];
list2rname1([H1,H2|T]) ->
    [H1,"_",list2name([H2|T])];
list2rname1([{ptype,H}|_T]) ->
    [H];
list2rname1([H|_T]) ->
    [H];
list2rname1([]) ->
    [].



constructed_suffix(_,#'SEQUENCE'{pname=Ptypename}) when Ptypename =/= false ->
    {ptype, Ptypename};
constructed_suffix(_,#'SET'{pname=Ptypename}) when Ptypename =/= false ->
    {ptype,Ptypename};
constructed_suffix('SEQUENCE OF',_) -> 
    'SEQOF';
constructed_suffix('SET OF',_) -> 
    'SETOF'.

index2suffix(0) ->
    "";
index2suffix(N) ->
    lists:concat(["_",N]).

ct_gen_module(#gen{erule=ber}) ->
    asn1ct_gen_ber_bin_v2;
ct_gen_module(#gen{erule=per}) ->
    asn1ct_gen_per.

ct_constructed_module(#gen{erule=ber}) ->
    asn1ct_constructed_ber_bin_v2;
ct_constructed_module(#gen{erule=per}) ->
    asn1ct_constructed_per.

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V;
	{value,Cnstr} ->
	    Cnstr
    end.

get_record_name_prefix(#gen{rec_prefix=Prefix}) ->
    Prefix.

get_macro_name_prefix(#gen{macro_prefix=Prefix}) ->
    Prefix.
