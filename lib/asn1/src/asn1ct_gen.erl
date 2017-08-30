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
-module(asn1ct_gen).

-include("asn1_records.hrl").

-export([demit/1,
	 emit/1,
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
	 get_record_name_prefix/0,
	 conform_value/2,
	 named_bitstring_value/2]).
-export([pgen/5,
	 mk_var/1, 
	 un_hyphen_var/1]).
-export([gen_encode_constructed/4,
	 gen_decode_constructed/4]).

-define(SUPPRESSION_FUNC, 'dialyzer-suppressions').

%% pgen(Outfile, Erules, Module, TypeOrVal, Options)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList}
%% TypeList = ValueList = [atom()]
%% Options = [Options] from asn1ct:compile()

pgen(OutFile,Erules,Module,TypeOrVal,Options) ->
    pgen_module(OutFile,Erules,Module,TypeOrVal,Options,true).


pgen_module(OutFile,Erules,Module,
	    TypeOrVal = {Types,_Values,_Ptypes,_Classes,_Objects,_ObjectSets},
	    Options,Indent) ->
    N2nConvEnums = [CName|| {n2n,CName} <- get(encoding_options)],
    case N2nConvEnums -- Types of
	[] ->
	    ok;
	UnmatchedTypes ->
	    exit({"Non existing ENUMERATION types used in n2n option",
		   UnmatchedTypes})
    end,
    put(outfile,OutFile),
    HrlGenerated = pgen_hrl(Erules,Module,TypeOrVal,Options,Indent),
    asn1ct_name:start(),
    ErlFile = lists:concat([OutFile,".erl"]),
    _ = open_output_file(ErlFile),
    asn1ct_func:start_link(),
    gen_head(Erules,Module,HrlGenerated),
    pgen_exports(Erules,Module,TypeOrVal),
    pgen_dispatcher(Erules,Module,TypeOrVal),
    pgen_info(),
    pgen_typeorval(Erules,Module,N2nConvEnums,TypeOrVal),
    pgen_partial_incomplete_decode(Erules),
% gen_vars(asn1_db:mod_to_vars(Module)),
% gen_tag_table(AllTypes),
    emit([nl,
	  "%%%",nl,
	  "%%% Run-time functions.",nl,
	  "%%%",nl]),
    dialyzer_suppressions(Erules),
    Fd = get(gen_file_out),
    asn1ct_func:generate(Fd),
    close_output_file(),
    _ = erase(outfile),
    asn1ct:verbose("--~p--~n",[{generated,ErlFile}],Options).

dialyzer_suppressions(Erules) ->
    emit([nl,
	  {asis,?SUPPRESSION_FUNC},"(Arg) ->",nl]),
    Rtmod = ct_gen_module(Erules),
    Rtmod:dialyzer_suppressions(Erules).

pgen_typeorval(Erules,Module,N2nConvEnums,{Types,Values,_Ptypes,_Classes,Objects,ObjectSets}) ->
    Rtmod = ct_gen_module(Erules),
    pgen_types(Rtmod,Erules,N2nConvEnums,Module,Types),
    pgen_values(Erules,Module,Values),
    pgen_objects(Rtmod,Erules,Module,Objects),
    pgen_objectsets(Rtmod,Erules,Module,ObjectSets),
    pgen_partial_decode(Rtmod,Erules,Module).

pgen_values(_,_,[]) ->
    true;
pgen_values(Erules,Module,[H|T]) ->
    Valuedef = asn1_db:dbget(Module,H),
    gen_value(Valuedef),
    pgen_values(Erules,Module,T).

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

pgen_name2numfunc(_TypeName,[], _) ->
    true;
pgen_name2numfunc(TypeName,[{Atom,Number}], extension_marker) ->
    emit(["name2num_",TypeName,"(",{asis,Atom},") ->",Number,";",nl]),
    emit(["name2num_",TypeName,"({asn1_enum, Num}) -> Num.",nl,nl]);
pgen_name2numfunc(TypeName,[{Atom,Number}], _) ->
    emit(["name2num_",TypeName,"(",{asis,Atom},") ->",Number,".",nl,nl]);
pgen_name2numfunc(TypeName,[{Atom,Number}|NNRest], EM) ->
    emit(["name2num_",TypeName,"(",{asis,Atom},") ->",Number,";",nl]),
    pgen_name2numfunc(TypeName,NNRest, EM).

pgen_num2namefunc(_TypeName,[], _) ->
    true;
pgen_num2namefunc(TypeName,[{Atom,Number}], extension_marker) ->
    emit(["num2name_",TypeName,"(",Number,") ->",{asis,Atom},";",nl]),
    emit(["num2name_",TypeName,"(ExtensionNum) -> {asn1_enum, ExtensionNum}.",nl,nl]);
pgen_num2namefunc(TypeName,[{Atom,Number}], _) ->
    emit(["num2name_",TypeName,"(",Number,") ->",{asis,Atom},".",nl,nl]);
pgen_num2namefunc(TypeName,[{Atom,Number}|NNRest], EM) ->
    emit(["num2name_",TypeName,"(",Number,") ->",{asis,Atom},";",nl]),
    pgen_num2namefunc(TypeName,NNRest, EM).

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

pgen_partial_decode(Rtmod,Erule,Module) when Erule == ber ->
    pgen_partial_inc_dec(Rtmod,Erule,Module),
    pgen_partial_dec(Rtmod,Erule,Module);
pgen_partial_decode(_,_,_) ->
    ok.

pgen_partial_inc_dec(Rtmod,Erules,Module) ->
%    io:format("Start partial incomplete decode gen?~n"),
    case asn1ct:get_gen_state_field(inc_type_pattern) of
	undefined ->
%	    io:format("Partial incomplete decode gen not started: ~w~n",[asn1ct:get_gen_state_field(active)]),
	    ok;
%	[] ->
%	    ok;
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
%	    io:format("Index:~p~n",[I]),
	    asn1ct:set_current_sindex(I);
	_I ->
	    asn1ct:set_current_sindex(0),
%	    io:format("Index=~p~n",[_I]),
	    ok
    end,
    Rtmod:gen_decode(Erules,TypeDef),
    gen_dec_part_inner_constr(Rtmod,Erules,TypeDef,[TopTypeName]),
    pgen_partial_inc_dec1(Rtmod,Erules,Module,Ps);
pgen_partial_inc_dec1(_,_,_,[]) ->
    ok.

gen_partial_inc_dec_refed_funcs(Rtmod,Erule) when Erule == ber ->
    case asn1ct:next_refed_func() of
	[] ->
	    ok;
	{#'Externaltypereference'{module=M,type=Name},Sindex,Pattern} ->
	    TypeDef = asn1_db:dbget(M,Name),
	    asn1ct:update_gen_state(namelist,Pattern),
	    asn1ct:set_current_sindex(Sindex),
	    Rtmod:gen_inc_decode(Erule,TypeDef),
	    gen_dec_part_inner_constr(Rtmod,Erule,TypeDef,[Name]),
	    gen_partial_inc_dec_refed_funcs(Rtmod,Erule);
	{Name,Sindex,Pattern,Type} ->
	    TypeDef=#typedef{name=asn1ct_gen:list2name(Name),typespec=Type},
	    asn1ct:update_gen_state(namelist,Pattern),
	    asn1ct:set_current_sindex(Sindex),
	    Rtmod:gen_inc_decode(Erule,TypeDef),
	    gen_dec_part_inner_constr(Rtmod,Erule,TypeDef,Name),
	    gen_partial_inc_dec_refed_funcs(Rtmod,Erule)
    end;
gen_partial_inc_dec_refed_funcs(_,_) ->
    ok.

pgen_partial_dec(_Rtmod,Erules,_Module) ->
    Type_pattern = asn1ct:get_gen_state_field(type_pattern),
%    io:format("Type_pattern: ~w~n",[Type_pattern]),
    %% Get the typedef of the top type and follow into the choosen components until the last type/component.
    pgen_partial_types(Erules,Type_pattern),
    ok.

pgen_partial_types(Erules,Type_pattern)  ->
    % until this functionality works on all back-ends
    Options = get(encoding_options),
    case lists:member(asn1config,Options) of
	true ->
	    pgen_partial_types1(Erules,Type_pattern);
	_ -> ok
    end.

    
pgen_partial_types1(Erules,[{FuncName,[TopType|RestTypes]}|Rest]) ->
%    emit([FuncName,"(Bytes) ->",nl]),
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
  when is_integer(N) -> % this case a decode of one of the elements in
                     % the SEQUENCE OF is required.
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
	_ -> %this may be a referenced type that shall be traversed or
             %the selected type
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
    get_component(Name,Cs);
get_component(Name,_) ->
    throw({error,{asn1,{internal_error,Name}}}).

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
pgen_partial_incomplete_decode1(ber) ->
    case asn1ct:read_config_data(partial_incomplete_decode) of
	undefined ->
	    ok;
	Data ->
	    lists:foreach(fun emit_partial_incomplete_decode/1,Data)
    end,
    GeneratedFs= asn1ct:get_gen_state_field(gen_refed_funcs),
%    io:format("GeneratedFs :~n~p~n",[GeneratedFs]),
    gen_part_decode_funcs(GeneratedFs,0);
pgen_partial_incomplete_decode1(_) -> ok.

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

%% Generate value functions ***************
%% ****************************************
%% Generates a function 'V'/0 for each Value V defined in the ASN.1 module
%% the function returns the value in an Erlang representation which can be
%% used as  input to the runtime encode functions

gen_value(Value) when is_record(Value,valuedef) ->
%%    io:format(" ~w ",[Value#valuedef.name]),
    emit({"'",Value#valuedef.name,"'() ->",nl}),
    V = Value#valuedef.value,
    emit([{asis,V},".",nl,nl]).

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
	    gen_types(Erules, [NameSuffix|Typename], Type, gen_encode);
	_ ->
	    exit({nyi,InnerType})
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


pgen_exports(Erules,_Module,{Types,Values,_,_,Objects,ObjectSets}) ->
    emit(["-export([encoding_rule/0,bit_string_format/0,",nl,
	  "         legacy_erlang_types/0]).",nl]),
    emit(["-export([",{asis,?SUPPRESSION_FUNC},"/1]).",nl]),
    case Types of
	[] -> ok;
	_ ->
	    emit({"-export([",nl}),
	    case Erules of
		ber ->
		    gen_exports1(Types,"enc_",2);
		_ ->
		    gen_exports1(Types,"enc_",1)
	    end,
	    emit({"-export([",nl}),
	    case Erules of
		ber ->
		    gen_exports1(Types, "dec_", 2);
		_ ->
		    gen_exports1(Types, "dec_", 1)
	    end
    end,
    case [X || {n2n,X} <- get(encoding_options)] of
	[] -> ok;
	A2nNames ->
	    emit({"-export([",nl}),
	    gen_exports1(A2nNames,"name2num_",1),
	    emit({"-export([",nl}),
	    gen_exports1(A2nNames,"num2name_",1)	    
    end,
    case Values of
	[] -> ok;
	_ ->
	    emit({"-export([",nl}),
	    gen_exports1(Values,"",0)
    end,
    case Objects of
	[] -> ok;
	_ ->
	    case erule(Erules) of
		per ->
		    ok;
		ber ->
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"enc_",3),
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"dec_",3)
	    end
    end,
    case ObjectSets of
	[] -> ok;
	_ ->
	    case erule(Erules) of
		per ->
		    ok;
		ber ->
		    emit({"-export([",nl}),
		    gen_exports1(ObjectSets, "getenc_",1),
		    emit({"-export([",nl}),
		    gen_exports1(ObjectSets, "getdec_",1)
	    end
    end,
    emit({"-export([info/0]).",nl}),
    gen_partial_inc_decode_exports(),
    gen_selected_decode_exports(),
    emit({nl,nl}).

gen_exports1([F1,F2|T],Prefix,Arity) ->
	emit({"'",Prefix,F1,"'/",Arity,com,nl}),
	gen_exports1([F2|T],Prefix,Arity);
gen_exports1([Flast|_T],Prefix,Arity) ->
	emit({"'",Prefix,Flast,"'/",Arity,nl,"]).",nl,nl}).

gen_partial_inc_decode_exports() ->
    case {asn1ct:read_config_data(partial_incomplete_decode),
	  asn1ct:get_gen_state_field(inc_type_pattern)}  of
	{undefined,_} ->
	    ok;
	{_,undefined} ->
	    ok;
	{Data,_} ->
	    gen_partial_inc_decode_exports(Data),
	    emit(["-export([decode_part/2]).",nl])
    end.
gen_partial_inc_decode_exports([]) ->
    ok;
gen_partial_inc_decode_exports([{Name,_,_}|Rest]) ->
    emit(["-export([",Name,"/1"]),
    gen_partial_inc_decode_exports1(Rest);
gen_partial_inc_decode_exports([_|Rest]) ->
    gen_partial_inc_decode_exports(Rest).

gen_partial_inc_decode_exports1([]) ->
    emit(["]).",nl]);
gen_partial_inc_decode_exports1([{Name,_,_}|Rest]) ->
    emit([", ",Name,"/1"]),
    gen_partial_inc_decode_exports1(Rest);
gen_partial_inc_decode_exports1([_|Rest]) ->
    gen_partial_inc_decode_exports1(Rest).

gen_selected_decode_exports() ->
    case asn1ct:get_gen_state_field(type_pattern) of
	undefined ->
	    ok;
	L ->
	   gen_selected_decode_exports(L)
    end.

gen_selected_decode_exports([]) ->
    ok;
gen_selected_decode_exports([{FuncName,_}|Rest]) ->
    emit(["-export([",FuncName,"/1"]),
    gen_selected_decode_exports1(Rest).
gen_selected_decode_exports1([]) ->
    emit(["]).",nl,nl]);
gen_selected_decode_exports1([{FuncName,_}|Rest]) ->
    emit([",",nl,"          ",FuncName,"/1"]),
    gen_selected_decode_exports1(Rest).

pgen_dispatcher(Erules,_Module,{[],_Values,_,_,_Objects,_ObjectSets}) ->
    gen_info_functions(Erules);
pgen_dispatcher(Erules,_Module,{Types,_Values,_,_,_Objects,_ObjectSets}) ->
    emit(["-export([encode/2,decode/2]).",nl,nl]),
    gen_info_functions(Erules),

    Options = get(encoding_options),
    NoFinalPadding = lists:member(no_final_padding, Options),
    NoOkWrapper = proplists:get_bool(no_ok_wrapper, Options),

    Call = case Erules of
	       per ->
		   asn1ct_func:need({Erules,complete,1}),
		   "complete(encode_disp(Type, Data))";
	       ber ->
		   "iolist_to_binary(element(1, encode_disp(Type, Data)))";
	       uper when NoFinalPadding == true ->
		   asn1ct_func:need({Erules,complete_NFP,1}),
		   "complete_NFP(encode_disp(Type, Data))";
	       uper ->
		   asn1ct_func:need({Erules,complete,1}),
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

    Return_rest = proplists:get_bool(undec_rest, Options),
    Data = case {Erules,Return_rest} of
	       {ber,true} -> "Data0";
	       _ -> "Data"
	   end,

    emit(["decode(Type,",Data,") ->",nl]),
    DecWrap =
	case {Erules,Return_rest} of
	    {ber,false} ->
		asn1ct_func:need({ber,ber_decode_nif,1}),
		"element(1, ber_decode_nif(Data))";
	    {ber,true} ->
		asn1ct_func:need({ber,ber_decode_nif,1}),
		emit(["{Data,Rest} = ber_decode_nif(Data0),",nl]),
		"Data";
	    _ ->
		"Data"
	end,
    emit([case NoOkWrapper of
	      false -> "try";
	      true -> "case"
	  end, " decode_disp(Type, ",DecWrap,") of",nl]),
    case erule(Erules) of
	ber ->
	    emit(["  Result ->",nl]);
	per ->
	    emit(["  {Result,Rest} ->",nl])
    end,
    case Return_rest of
	false -> result_line(NoOkWrapper, ["Result"]);
	true ->  result_line(NoOkWrapper, ["Result","Rest"])
    end,
    case NoOkWrapper of
	false ->
	    emit([nl,try_catch(),nl,nl]);
	true ->
	    emit([nl,"end.",nl,nl])
    end,

    gen_decode_partial_incomplete(Erules),

    case Erules of
	ber ->
	    gen_dispatcher(Types,"encode_disp","enc_",""),
	    gen_dispatcher(Types,"decode_disp","dec_",""),
	    gen_partial_inc_dispatcher();
	_PerOrPer_bin -> 
	    gen_dispatcher(Types,"encode_disp","enc_",""),
	    gen_dispatcher(Types,"decode_disp","dec_","")
    end,
    emit([nl,nl]).

result_line(NoOkWrapper, Items) ->
    S = ["    "|case NoOkWrapper of
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
     "      case Exception of",nl,
     "        {error,Reason}=Error ->",nl,
     "          Error;",nl,
     "        Reason ->",nl,
     "         {error,{asn1,Reason}}",nl,
     "      end",nl,
     "end."].

gen_info_functions(Erules) ->
    emit(["encoding_rule() -> ",
	  {asis,Erules},".",nl,nl,
	  "bit_string_format() -> ",
	  {asis,asn1ct:get_bit_string_format()},".",nl,nl,
	  "legacy_erlang_types() -> ",
	  {asis,asn1ct:use_legacy_types()},".",nl,nl]).

gen_decode_partial_incomplete(ber) ->
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
gen_decode_partial_incomplete(_Erule) ->
    ok.

gen_partial_inc_dispatcher() ->
    case {asn1ct:read_config_data(partial_incomplete_decode),
	  asn1ct:get_gen_state_field(inc_type_pattern)} of
	{undefined,_} ->
	    ok;
	{_,undefined} ->
	    ok;
	{Data1,Data2} ->
%	    io:format("partial_incomplete_decode: ~p~ninc_type_pattern: ~p~n",[Data,Data2]),
	    gen_partial_inc_dispatcher(Data1, Data2, "")
    end.

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

gen_dispatcher([F1,F2|T],FuncName,Prefix,ExtraArg) ->
	emit([FuncName,"('",F1,"',Data) -> '",Prefix,F1,"'(Data",ExtraArg,")",";",nl]),
	gen_dispatcher([F2|T],FuncName,Prefix,ExtraArg);
gen_dispatcher([Flast|_T],FuncName,Prefix,ExtraArg) ->
	emit([FuncName,"('",Flast,"',Data) -> '",Prefix,Flast,"'(Data",ExtraArg,")",";",nl]),
	emit([FuncName,"(","Type",",_Data) -> exit({error,{asn1,{undefined_type,Type}}}).",nl,nl,nl]).

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


%% EMIT functions ************************
%% ***************************************

						% debug generation
demit(Term) ->
    case get(asndebug) of
	true -> emit(Term);
	_ ->true
    end.

						% always generation
emit(Term) ->
    ok = file:write(get(gen_file_out), do_emit(Term)).

do_emit({external,_M,T}) ->
    do_emit(T);

do_emit({prev,Variable}) when is_atom(Variable) ->
    do_emit({var,asn1ct_name:prev(Variable)});

do_emit({next,Variable}) when is_atom(Variable) ->
    do_emit({var,asn1ct_name:next(Variable)});

do_emit({curr,Variable}) when is_atom(Variable) ->
    do_emit({var,asn1ct_name:curr(Variable)});
    
do_emit({var,Variable}) when is_atom(Variable) ->
    [Head|V] = atom_to_list(Variable),
    [Head-32|V];

do_emit({var,Variable}) ->
    [Head|V] = Variable,
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

do_emit(tab) ->
    "     ";

do_emit(What) when is_integer(What) ->
    integer_to_list(What);

do_emit(What) when is_list(What), is_integer(hd(What)) ->
    What;

do_emit(What) when is_atom(What) ->
    atom_to_list(What);

do_emit(What) when is_tuple(What) ->
    [do_emit(E) || E <- tuple_to_list(What)];

do_emit(What) when is_list(What) ->
    [do_emit(E) || E <- What].

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

pgen_hrl(Erules,Module,TypeOrVal,Options,_Indent) ->
    put(currmod,Module),
    {Types,Values,Ptypes,_,_,_} = TypeOrVal,
    Ret =
	case pgen_hrltypes(Erules,Module,Ptypes++Types,0) of
	    0 -> 
		case Values of
		    [] ->
			0;
		    _ ->
			open_hrl(get(outfile),get(currmod)),
			pgen_macros(Erules,Module,Values),
			1
		end;
	    X ->
		pgen_macros(Erules,Module,Values),
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
			   Options),
	    Y
    end.

pgen_macros(_,_,[]) ->
    true;
pgen_macros(Erules,Module,[H|T]) ->
    Valuedef = asn1_db:dbget(Module,H),
    gen_macro(Valuedef),
    pgen_macros(Erules,Module,T).

pgen_hrltypes(_,_,[],NumRecords) ->
    NumRecords;
pgen_hrltypes(Erules,Module,[H|T],NumRecords) ->
%    io:format("records = ~p~n",NumRecords),
    Typedef = asn1_db:dbget(Module,H),
    AddNumRecords = gen_record(Typedef,NumRecords),
    pgen_hrltypes(Erules,Module,T,NumRecords+AddNumRecords).


%% Generates a macro for value Value defined in the ASN.1 module
gen_macro(Value) when is_record(Value,valuedef) ->
    Prefix = get_macro_name_prefix(),
    emit({"-define('",Prefix,Value#valuedef.name,"', ",
	  {asis,Value#valuedef.value},").",nl}).

%% Generate record functions **************
%% Generates an Erlang record for each named and unnamed SEQUENCE and SET in the ASN.1 
%% module. If no SEQUENCE or SET is found there is no .hrl file generated


gen_record(Tdef,NumRecords) when is_record(Tdef,typedef) ->
    Name = [Tdef#typedef.name],
    Type = Tdef#typedef.typespec,
    gen_record(type,Name,Type,NumRecords);

gen_record(Tdef,NumRecords) when is_record(Tdef,ptypedef) ->
    Name = [Tdef#ptypedef.name],
    Type = Tdef#ptypedef.typespec,
    gen_record(ptype,Name,Type,NumRecords).
    
gen_record(TorPtype,Name,[#'ComponentType'{name=Cname,typespec=Type}|T],Num) ->
    Num2 = gen_record(TorPtype,[Cname|Name],Type,Num),
    gen_record(TorPtype,Name,T,Num2);
gen_record(TorPtype,Name,{Clist1,Clist2},Num)
  when is_list(Clist1), is_list(Clist2) ->
    gen_record(TorPtype,Name,Clist1++Clist2,Num);
gen_record(TorPtype,Name,{Clist1,EClist,Clist2},Num) 
  when is_list(Clist1), is_list(EClist), is_list(Clist2) ->
    gen_record(TorPtype,Name,Clist1++EClist++Clist2,Num);
gen_record(TorPtype,Name,[_|T],Num) -> % skip EXTENSIONMARK
    gen_record(TorPtype,Name,T,Num);
gen_record(_TorPtype,_Name,[],Num) ->
    Num;

gen_record(TorPtype,Name,Type,Num) when is_record(Type,type) ->    
    Def = Type#type.def,
    Rec = case Def of
	      Seq when is_record(Seq,'SEQUENCE') ->
		  case Seq#'SEQUENCE'.pname of
		      false ->
			  {record,Seq#'SEQUENCE'.components};
%% 		      _Pname when TorPtype == type ->
%% 			  false;
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
%	      {'SET',{_,_CompList}} -> 
%		  {record,_CompList}; 
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
	    Prefix = get_record_name_prefix(),
	    emit({"-record('",Prefix,list2name(Name),"',{",nl}),
	    RootList = case CompList of
			   _ when is_list(CompList) ->
			       CompList;
			   {Rl,_} -> Rl;
			   {Rl1,_Ext,_Rl2} -> Rl1
		       end,
	    gen_record2(Name,'SEQUENCE',RootList),
	    NewCompList = 
		case CompList of
		    {CompList1,[]} ->
			emit({"}). % with extension mark",nl,nl}),
			CompList1;
		    {Tr,ExtensionList2} ->
			case Tr of
			    [] -> true;
			    _ -> emit({",",nl})
			end,
			emit({"%% with extensions",nl}),
			gen_record2(Name, 'SEQUENCE', ExtensionList2,
				    "", ext),
			emit({"}).",nl,nl}),
			Tr ++ ExtensionList2;
		    {Rootl1,Extl,Rootl2} ->
			case Rootl1 =/= [] andalso Extl++Rootl2 =/= [] of
			    true -> emit([com]);
			    false -> ok
			end,
			case Rootl1 of
			    [_|_] -> emit([nl]);
			    [] -> ok
			end,
			emit(["%% with extensions",nl]),
			gen_record2(Name,'SEQUENCE',Extl,"",ext),
			case Extl =/= [] andalso Rootl2 =/= [] of
			    true -> emit([com]);
			    false -> ok
			end,
			case Extl of
			    [_|_] -> emit([nl]);
			    [] -> ok
			end,
			emit(["%% end of extensions",nl]),
			gen_record2(Name,'SEQUENCE',Rootl2,"",noext),
			emit(["}).",nl,nl]),
			Rootl1++Extl++Rootl2;
		    _ -> 
			emit({"}).",nl,nl}),
			CompList
		end,
	    gen_record(TorPtype,Name,NewCompList,Num+1);
	{inner,{'CHOICE', CompList}} ->
	    gen_record(TorPtype,Name,CompList,Num);
	{NewName,{_, CompList}} ->
	    gen_record(TorPtype,NewName,CompList,Num)
    end;
gen_record(_,_,_,NumRecords) -> % skip CLASS etc for now.
     NumRecords.
		    
gen_head(Erules,Mod,Hrl) ->
    Options = get(encoding_options),
    case Erules of
	per ->
	    emit(["%% Generated by the Erlang ASN.1 PER-"
		  "compiler version, utilizing bit-syntax:",
		  asn1ct:vsn(),nl]);
	ber ->
	    emit(["%% Generated by the Erlang ASN.1 BER_V2-"
		  "compiler version, utilizing bit-syntax:",
		  asn1ct:vsn(),nl]);
	uper ->
	    emit(["%% Generated by the Erlang ASN.1 UNALIGNED"
		  " PER-compiler version, utilizing bit-syntax:",
		  asn1ct:vsn(),nl])
    end,
    emit({"%% Purpose: encoder and decoder to the types in mod ",Mod,nl,nl}),
    emit({"-module('",Mod,"').",nl}),
    put(currmod,Mod),
    emit({"-compile(nowarn_unused_vars).",nl}),
    emit({"-dialyzer(no_improper_lists).",nl}),
    case Hrl of
	0 -> ok;
	_ -> emit({"-include(\"",Mod,".hrl\").",nl})
    end,
    emit(["-asn1_info([{vsn,'",asn1ct:vsn(),"'},",nl,
	  "            {module,'",Mod,"'},",nl,
	  "            {options,",io_lib:format("~p",[Options]),"}]).",nl,nl]).
			

gen_hrlhead(Mod) ->
    emit({"%% Generated by the Erlang ASN.1 compiler version:",asn1ct:vsn(),nl}),
    emit({"%% Purpose: Erlang record definitions for each named and unnamed",nl}),
    emit({"%% SEQUENCE and SET, and macro definitions for each value",nl}),
    emit({"%% definition,in module ",Mod,nl,nl}),
    emit({nl,nl}).

gen_record2(Name,SeqOrSet,Comps) ->
    gen_record2(Name,SeqOrSet,Comps,"",noext).

gen_record2(_Name,_SeqOrSet,[],_Com,_Extension) ->
    true;
gen_record2(_Name,_SeqOrSet,[H = #'ComponentType'{name=Cname}],Com,Extension) ->
    emit(Com),
    emit({asis,Cname}),
    gen_record_default(H, Extension);
gen_record2(Name,SeqOrSet,[H = #'ComponentType'{name=Cname}|T],Com, Extension) ->
    emit(Com),
    emit({asis,Cname}),
    gen_record_default(H, Extension),
    gen_record2(Name,SeqOrSet,T,", ", Extension);
gen_record2(Name,SeqOrSet,[_|T],Com,Extension) ->
    %% skip EXTENSIONMARK, ExtensionAdditionGroup and other markers
    gen_record2(Name,SeqOrSet,T,Com,Extension).

gen_record_default(#'ComponentType'{prop='OPTIONAL'}, _)->
    emit(" = asn1_NOVALUE"); 
gen_record_default(#'ComponentType'{prop={'DEFAULT',_}}, _)->
    emit(" = asn1_DEFAULT"); 
gen_record_default(_, _) ->
    true.

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
%    get_inner(Type);
    Type;
get_inner(T) when is_tuple(T) -> 
    case element(1,T) of
	Tuple when is_tuple(Tuple),element(1,Tuple) == objectclass ->
	    case catch(lists:last(element(2,T))) of
		{valuefieldreference,FieldName} ->
		    get_fieldtype(element(2,Tuple),FieldName);
		{typefieldreference,FieldName} ->
		    get_fieldtype(element(2,Tuple),FieldName);
		{'EXIT',Reason} ->
		    throw({asn1,{'internal error in get_inner/1',Reason}})
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

erule(ber) -> ber;
erule(per) -> per;
erule(uper) -> per.

index2suffix(0) ->
    "";
index2suffix(N) ->
    lists:concat(["_",N]).

ct_gen_module(ber) ->
    asn1ct_gen_ber_bin_v2;
ct_gen_module(per) ->
    asn1ct_gen_per;
ct_gen_module(uper) ->
    asn1ct_gen_per.

ct_constructed_module(ber) ->
    asn1ct_constructed_ber_bin_v2;
ct_constructed_module(per) ->
    asn1ct_constructed_per;
ct_constructed_module(uper) ->
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
    
get_record_name_prefix() ->
    case lists:keysearch(record_name_prefix,1,get(encoding_options)) of
	false ->
	    "";
	{value,{_,Prefix}} ->
	    Prefix
    end.

get_macro_name_prefix() ->
    case lists:keysearch(macro_name_prefix,1,get(encoding_options)) of
	false ->
	    "";
	{value,{_,Prefix}} ->
	    Prefix
    end.
