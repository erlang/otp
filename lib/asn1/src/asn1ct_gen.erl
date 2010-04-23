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
-module(asn1ct_gen).

-include("asn1_records.hrl").

-export([pgen_exports/3,
	 pgen_hrl/5,
	 gen_head/3,
	 demit/1,
	 emit/1,
	 get_inner/1,type/1,def_to_tag/1,prim_bif/1,
	 type_from_object/1,
	 get_typefromobject/1,get_fieldcategory/2,
	 get_classfieldcategory/2,
	 list2name/1,
	 list2rname/1,
	 constructed_suffix/2,
	 unify_if_string/1,
	 gen_check_call/7,
	 get_constraint/2,
	 insert_once/2,
	 rt2ct_suffix/1,
	 rt2ct_suffix/0,
	 index2suffix/1,
	 get_record_name_prefix/0]).
-export([pgen/5,
	 pgen_module/6,
	 mk_var/1, 
	 un_hyphen_var/1]).
-export([gen_encode_constructed/4,
	 gen_decode_constructed/4]).

%% pgen(Outfile, Erules, Module, TypeOrVal, Options)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber | ber_bin | per_bin
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
    Fid = fopen(ErlFile,[write]),
    put(gen_file_out,Fid),
    gen_head(Erules,Module,HrlGenerated),
    pgen_exports(Erules,Module,TypeOrVal),
    pgen_dispatcher(Erules,Module,TypeOrVal),
    pgen_info(),
    pgen_typeorval(wrap_ber(Erules),Module,N2nConvEnums,TypeOrVal),
    pgen_partial_incomplete_decode(Erules),
% gen_vars(asn1_db:mod_to_vars(Module)),
% gen_tag_table(AllTypes),
    file:close(Fid),
    asn1ct:verbose("--~p--~n",[{generated,ErlFile}],Options).


pgen_typeorval(Erules,Module,N2nConvEnums,{Types,Values,_Ptypes,_Classes,Objects,ObjectSets}) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    pgen_types(Rtmod,Erules,N2nConvEnums,Module,Types),
    pgen_values(Erules,Module,Values),
    pgen_objects(Rtmod,Erules,Module,Objects),
    pgen_objectsets(Rtmod,Erules,Module,ObjectSets),
    case catch lists:member(der,get(encoding_options)) of
	true ->
	    pgen_check_defaultval(Erules,Module);
	_ -> ok
    end,
    pgen_partial_decode(Rtmod,Erules,Module).

pgen_values(_,_,[]) ->
    true;
pgen_values(Erules,Module,[H|T]) ->
    Valuedef = asn1_db:dbget(Module,H),
    gen_value(Valuedef),
    pgen_values(Erules,Module,T).

pgen_types(_,_,_,Module,[]) ->
    gen_value_match(Module),
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

pgen_n2nconversion(_Erules,#typedef{name=TypeName,typespec=#type{def={'ENUMERATED',{NN1,NN2}}}}) ->
    NN = NN1 ++ NN2,
    pgen_name2numfunc(TypeName,NN),
    pgen_num2namefunc(TypeName,NN);
pgen_n2nconversion(_Erules,_) ->
    true.

pgen_name2numfunc(_TypeName,[]) ->
    true;
pgen_name2numfunc(TypeName,[{Atom,Number}]) ->
    emit(["name2num_",TypeName,"(",{asis,Atom},") ->",Number,".",nl,nl]);
pgen_name2numfunc(TypeName,[{Atom,Number}|NNRest]) ->
    emit(["name2num_",TypeName,"(",{asis,Atom},") ->",Number,";",nl]),
    pgen_name2numfunc(TypeName,NNRest).

pgen_num2namefunc(_TypeName,[]) ->
    true;
pgen_num2namefunc(TypeName,[{Atom,Number}]) ->
    emit(["num2name_",TypeName,"(",Number,") ->",{asis,Atom},".",nl,nl]);
pgen_num2namefunc(TypeName,[{Atom,Number}|NNRest]) ->
    emit(["num2name_",TypeName,"(",Number,") ->",{asis,Atom},";",nl]),
    pgen_num2namefunc(TypeName,NNRest).

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

pgen_check_defaultval(Erules,Module) ->
    CheckObjects = ets:tab2list(check_functions),
    case get(asndebug) of
	true ->
	    FileName = lists:concat([Module,".table"]),
	    {ok,IoDevice} = file:open(FileName,[write]),
	    Fun =
		fun(X)->
			io:format(IoDevice,"~n~n************~n~n~p~n~n*****"
				  "********~n~n",[X]) 
		end,
	    lists:foreach(Fun,CheckObjects),
	    file:close(IoDevice);
	_ -> ok
    end,
    gen_check_defaultval(Erules,Module,CheckObjects).

pgen_partial_decode(Rtmod,Erule,Module) when Erule == ber_bin_v2 ->
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

gen_partial_inc_dec_refed_funcs(Rtmod,Erule) when Erule == ber_bin_v2 ->
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
    Ctmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
					       rt2ct_suffix(Erules)])),
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
pgen_partial_incomplete_decode1(ber_bin_v2) ->
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
	  "      'dec_",TypeName,"'(lists:map(fun(X)->element(1,?RT_BER:decode(X)) end,L),",{asis,Tag},");",nl,
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
    % Argument no 6 is 0, i.e. bit 6 for primitive encoding.
    asn1ct_gen_ber_bin_v2:gen_dec_prim(ber_bin_v2,Type,"Data",Tag,[],0,", mandatory, ");
gen_part_decode_funcs(WhatKind,_TypeName,{_,Directive,_,_}) ->
    throw({error,{asn1,{"Not implemented yet",WhatKind," partial incomplete directive:",Directive}}}).

gen_types(Erules,Tname,{RootL1,ExtList,RootL2}) 
  when is_list(RootL1), is_list(RootL2) ->
    gen_types(Erules,Tname,RootL1),
    gen_types(Erules,Tname,ExtList),
    gen_types(Erules,Tname,RootL2);
gen_types(Erules,Tname,{RootList,ExtList}) when is_list(RootList) ->
    gen_types(Erules,Tname,RootList),
    gen_types(Erules,Tname,ExtList);
gen_types(Erules,Tname,[{'EXTENSIONMARK',_,_}|Rest]) ->
    gen_types(Erules,Tname,Rest);
gen_types(Erules,Tname,[ComponentType|Rest]) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    asn1ct_name:clear(),
    Rtmod:gen_encode(Erules,Tname,ComponentType),
    asn1ct_name:clear(),
    Rtmod:gen_decode(Erules,Tname,ComponentType),
    gen_types(Erules,Tname,Rest);
gen_types(_,_,[]) ->
    true;
gen_types(Erules,Tname,Type) when is_record(Type,type) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    asn1ct_name:clear(),
    Rtmod:gen_encode(Erules,Tname,Type),
    asn1ct_name:clear(),
    Rtmod:gen_decode(Erules,Tname,Type).

gen_value_match(Module) ->
    case get(value_match) of
	{true,Module} ->
	    emit(["value_match([{Index,Cname}|Rest],Value) ->",nl,
		  "  Value2 =",nl,
		  "    case element(Index,Value) of",nl,
		  "      {Cname,Val2} -> Val2;",nl,
		  "      X -> X",nl,
		  "    end,",nl,
		  "  value_match(Rest,Value2);",nl,
		  "value_match([],Value) ->",nl,
		  "  Value.",nl]);
	_  -> ok
    end,
    put(value_match,undefined).

gen_check_defaultval(Erules,Module,[{Name,Type}|Rest]) ->
    gen_check_func(Name,Type),
    gen_check_defaultval(Erules,Module,Rest);
gen_check_defaultval(_,_,[]) ->
    ok.

gen_check_func(Name,FType = #type{def=Def}) ->
    EncName = ensure_atom(Name),
    emit({{asis,EncName},"(_V,asn1_DEFAULT) ->",nl,"   true;",nl}),
    emit({{asis,EncName},"(V,V) ->",nl,"   true;",nl}),
    emit({{asis,EncName},"(V,{_,V}) ->",nl,"   true;",nl}),
    case Def of
	{'SEQUENCE OF',Type} ->
	    gen_check_sof(Name,'SEQOF',Type);
	{'SET OF',Type} ->
	    gen_check_sof(Name,'SETOF',Type);
	#'SEQUENCE'{components=Components} ->
	    gen_check_sequence(Name,Components);
	#'SET'{components=Components} ->
	    gen_check_sequence(Name,Components);
	{'CHOICE',Components} ->
	    gen_check_choice(Name,Components);
	#'Externaltypereference'{type=T} ->
	    emit({{asis,EncName},"(DefaultValue,Value) ->",nl}),
	    emit({"   '",list2name([T,check]),"'(DefaultValue,Value).",nl});
	MaybePrim ->
	    InnerType = get_inner(MaybePrim),
	    case type(InnerType) of
		{primitive,bif} ->
		    emit({{asis,EncName},"(DefaultValue,Value) ->",nl,"   "}),
		    gen_prim_check_call(get_inner(InnerType),"DefaultValue","Value",
					FType),
		    emit({".",nl,nl});
		_ ->
		    throw({asn1_error,{unknown,type,MaybePrim}})
	    end
    end.

gen_check_sof(Name,SOF,Type) ->
    EncName = ensure_atom(Name),
    NewName = ensure_atom(list2name([sorted,Name])),
    emit({{asis,EncName},"(V1,V2) ->",nl}),
    emit({"   ",{asis,NewName},"(lists:sort(V1),lists:sort(V2)).",nl,nl}),
    emit({{asis,NewName},"([],[]) ->",nl,"   true;",nl}),
    emit({{asis,NewName},"([DV|DVs],[V|Vs]) ->",nl,"   "}),
    InnerType = get_inner(Type#type.def),
    case type(InnerType) of
	{primitive,bif} ->
	    gen_prim_check_call(get_inner(InnerType),"DV","V",Type),
	    emit({",",nl});
	{constructed,bif} ->
	    emit([{asis,ensure_atom(list2name([SOF,Name]))},"(DV, V),",nl]);
	#'Externaltypereference'{type=T} ->
	    emit([{asis,ensure_atom(list2name([T,check]))},"(DV,V),",nl]);
	'ASN1_OPEN_TYPE' ->
	    emit(["DV = V,",nl]);
	_ ->
	    emit(["DV = V,",nl])
    end,
    emit({"   ",{asis,NewName},"(DVs,Vs).",nl,nl}).

gen_check_sequence(Name,Components) ->
    emit([{asis,ensure_atom(Name)},"(DefaultValue,Value) ->",nl]),
    gen_check_sequence(Name,Components,1).
gen_check_sequence(Name,[#'ComponentType'{name=N,typespec=Type}|Cs],Num) ->
    InnerType = get_inner(Type#type.def),
    NthDefV = ["element(",Num+1,",DefaultValue)"],
    NthV = ["element(",Num+1,",Value)"],
    gen_check_func_call(Name,Type,InnerType,NthDefV,NthV,N),
    case Cs of
	[] ->
	    emit({".",nl,nl});
	_ ->
	    emit({",",nl}),
	    gen_check_sequence(Name,Cs,Num+1)
    end;
gen_check_sequence(_,[],_) ->
    ok.

gen_check_choice(Name,CList=[#'ComponentType'{}|_Cs]) ->
    emit([{asis,ensure_atom(Name)},"({Id,DefaultValue},{Id,Value}) ->",nl]),
    emit(["   case Id of",nl]),
    gen_check_choice_components(Name,CList,1).

gen_check_choice_components(_,[],_)->
    ok;
gen_check_choice_components(Name,[#'ComponentType'{name=N,typespec=Type}|
				  Cs],Num) ->
    Ind6 = "      ",
    InnerType = get_inner(Type#type.def),
    emit({Ind6,"'",N,"' ->",nl,Ind6}),
    gen_check_func_call(Name,Type,InnerType,{var,"defaultValue"},
			{var,"value"},N),
    case Cs of
	[] ->
	    emit({nl,"   end.",nl,nl});
	_ ->
	    emit({";",nl}),
	    gen_check_choice_components(Name,Cs,Num+1)
    end.

gen_check_func_call(Name,Type,InnerType,DefVal,Val,N) ->
    case type(InnerType) of
	{primitive,bif} ->
	    emit("   "),
	    gen_prim_check_call(get_inner(InnerType),DefVal,Val,Type);
	#'Externaltypereference'{type=T} ->
	    emit({"   ",{asis,ensure_atom(list2name([T,check]))},"(",DefVal,",",Val,")"});
	'ASN1_OPEN_TYPE' ->
	    emit(["   if",nl,
		  "      ",DefVal," == ",Val," -> true;",nl,
		  "      true -> throw({error,{asn1_open_type}})",nl,
		  "   end",nl]);
	{constructed,bif} ->
	    emit(["   ",{asis,ensure_atom(list2name([N,Name]))},"(",DefVal,",",Val,")"]);
	_ ->
	    emit(["   if",nl,
		  "      ",DefVal," == ",Val," -> true;",nl,
		  "      true -> throw({error,{asn1_open_type}})",nl,
		  "   end",nl])
    end.
		  
    
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

    Rtmod = list_to_atom(lists:concat(["asn1ct_constructed_",erule(Erules)])),
    case InnerType of
	'SET' ->
	    Rtmod:gen_encode_set(Erules,Typename,D),
	    #'SET'{components=Components} = D#type.def,
	    gen_types(Erules,Typename,Components);
	'SEQUENCE' ->
	    Rtmod:gen_encode_sequence(Erules,Typename,D),
	    #'SEQUENCE'{components=Components} = D#type.def,
	    gen_types(Erules,Typename,Components);
	'CHOICE' ->
	    Rtmod:gen_encode_choice(Erules,Typename,D),
	    {_,Components} = D#type.def,
	    gen_types(Erules,Typename,Components);
	'SEQUENCE OF' ->
	    Rtmod:gen_encode_sof(Erules,Typename,InnerType,D),
	    {_,Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType,Type#type.def),
	    gen_types(Erules,[NameSuffix|Typename],Type);
	'SET OF' ->
	    Rtmod:gen_encode_sof(Erules,Typename,InnerType,D),
	    {_,Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType,Type#type.def),
	    gen_types(Erules,[NameSuffix|Typename],Type);
	_ ->
	    exit({nyi,InnerType})
    end;
gen_encode_constructed(Erules,Typename,InnerType,D) 
  when is_record(D,typedef) ->
    gen_encode_constructed(Erules,Typename,InnerType,D#typedef.typespec).

gen_decode_constructed(Erules,Typename,InnerType,D) when is_record(D,type) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_constructed_",erule(Erules)])),
    asn1ct:step_in_constructed(), %% updates namelist for exclusive decode
    case InnerType of
	'SET' ->
	    Rtmod:gen_decode_set(Erules,Typename,D);
	'SEQUENCE' ->
	    Rtmod:gen_decode_sequence(Erules,Typename,D);
	'CHOICE' ->
	    Rtmod:gen_decode_choice(Erules,Typename,D);
	'SEQUENCE OF' ->
	    Rtmod:gen_decode_sof(Erules,Typename,InnerType,D);
	'SET OF' ->
	    Rtmod:gen_decode_sof(Erules,Typename,InnerType,D);
	_ ->
	    exit({nyi,InnerType})
    end;


gen_decode_constructed(Erules,Typename,InnerType,D) when is_record(D,typedef) ->
    gen_decode_constructed(Erules,Typename,InnerType,D#typedef.typespec).


pgen_exports(Erules,_Module,{Types,Values,_,_,Objects,ObjectSets}) ->
    emit({"-export([encoding_rule/0]).",nl}),
    case Types of
	[] -> ok;
	_ ->
	    emit({"-export([",nl}),
	    case Erules of
		ber ->
		    gen_exports1(Types,"enc_",2);
		ber_bin ->
		    gen_exports1(Types,"enc_",2);
		ber_bin_v2 ->
		    gen_exports1(Types,"enc_",2);
		_ ->
		    gen_exports1(Types,"enc_",1)
	    end,
	    emit({"-export([",nl}),
	    gen_exports1(Types,"dec_",2),
	    case Erules of
		ber ->
		    emit({"-export([",nl}),
		    gen_exports1(Types,"dec_",3);
		ber_bin ->
		    emit({"-export([",nl}),
		    gen_exports1(Types,"dec_",3);
% 		ber_bin_v2 ->
% 		    emit({"-export([",nl}),
% 		    gen_exports1(Types,"dec_",2);
		_ -> ok
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
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"enc_",3),
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"dec_",4);
		ber_bin_v2 ->
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"enc_",3),
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"dec_",3);
		_ -> 
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"enc_",4),
		    emit({"-export([",nl}),
		    gen_exports1(Objects,"dec_",4)
	    end
    end,
    case ObjectSets of
	[] -> ok;
	_ ->
	    emit({"-export([",nl}),
	    gen_exports1(ObjectSets,"getenc_",2),
	    emit({"-export([",nl}),
	    gen_exports1(ObjectSets,"getdec_",2)
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
    emit(["encoding_rule() ->",nl]),
    emit([{asis,Erules},".",nl,nl]);
pgen_dispatcher(Erules,_Module,{Types,_Values,_,_,_Objects,_ObjectSets}) ->
    emit(["-export([encode/2,decode/2,encode_disp/2,decode_disp/2]).",nl,nl]),
    emit(["encoding_rule() ->",nl]),
    emit(["   ",{asis,Erules},".",nl,nl]),
    NoFinalPadding = lists:member(no_final_padding,get(encoding_options)),
    Call = case Erules of
	       per -> "?RT_PER:complete(encode_disp(Type,Data))";
	       per_bin -> "?RT_PER:complete(encode_disp(Type,Data))";
	       ber -> "encode_disp(Type,Data)";
	       ber_bin -> "encode_disp(Type,Data)";
	       ber_bin_v2 -> "encode_disp(Type,Data)";
	       uper_bin when NoFinalPadding == true -> 
		   "?RT_PER:complete_NFP(encode_disp(Type,Data))";
	       uper_bin -> "?RT_PER:complete(encode_disp(Type,Data))"
	   end,
    EncWrap = case Erules of
	       ber -> "wrap_encode(Bytes)";
	       _ -> "Bytes"
	   end,
    emit(["encode(Type,Data) ->",nl,
	  "case catch ",Call," of",nl,
	  "  {'EXIT',{error,Reason}} ->",nl,
	  "    {error,Reason};",nl,
	  "  {'EXIT',Reason} ->",nl,
	  "    {error,{asn1,Reason}};",nl,
	  "  {Bytes,_Len} ->",nl,
	  "    {ok,",EncWrap,"};",nl]),
    case Erules of
	per ->
	    emit(["  Bytes when is_binary(Bytes) ->",nl,
		  "    {ok,binary_to_list(Bytes)};",nl,
		  "  Bytes ->",nl,
		  "    {ok,binary_to_list(list_to_binary(Bytes))}",nl,
		  "  end.",nl,nl]);
	_ ->
	    emit(["  Bytes ->",nl,
		  "    {ok,",EncWrap,"}",nl,
		  "end.",nl,nl])
    end,

%     case Erules of
% 	ber_bin_v2 ->
% 	    emit(["decode(Type,Data0) ->",nl]),
% 	    emit(["{Data,_RestBin} = ?RT_BER:decode(Data0",driver_parameter(),"),",nl]);
% 	_ ->
% 	    emit(["decode(Type,Data) ->",nl])
%     end,

    Return_rest = lists:member(undec_rest,get(encoding_options)),
    Data = case {Erules,Return_rest} of
	       {ber_bin_v2,true} -> "Data0";
	       _ -> "Data"
	   end,

    emit(["decode(Type,",Data,") ->",nl]),
    DecAnonymous =
	case {Erules,Return_rest} of
	    {ber_bin_v2,false} ->
		io_lib:format("~s~s~s~n",
			      ["element(1,?RT_BER:decode(Data",
			       driver_parameter(),"))"]);
	    {ber_bin_v2,true} ->
		emit(["{Data,Rest} = ?RT_BER:decode(Data0",
		      driver_parameter(),"),",nl]),
		"Data";
	    _ ->
		"Data"
	end,
    DecWrap = case Erules of
		  ber -> "wrap_decode(Data)";
		  ber_bin_v2 ->
		      DecAnonymous;
		  per -> "list_to_binary(Data)";
		  _ -> "Data"
	      end,
	    
    emit(["case catch decode_disp(Type,",DecWrap,") of",nl,
	  "  {'EXIT',{error,Reason}} ->",nl,
	  "    {error,Reason};",nl,
	  "  {'EXIT',Reason} ->",nl,
	  "    {error,{asn1,Reason}};",nl]),
    case {Erules,Return_rest} of 
	{ber_bin_v2,false} ->
	    emit(["  Result ->",nl,
		  "    {ok,Result}",nl]);
	{ber_bin_v2,true} ->
	    emit(["  Result ->",nl,
		  "    {ok,Result,Rest}",nl]);
	{per,false} ->
	    emit(["  {X,_Rest} ->",nl,
		  "    {ok,if_binary2list(X)};",nl,
		  "  {X,_Rest,_Len} ->",nl,
		  "    {ok,if_binary2list(X)}",nl]);
	{_,false} ->
	    emit(["  {X,_Rest} ->",nl,
		  "    {ok,X};",nl,
		  "  {X,_Rest,_Len} ->",nl,
		  "    {ok,X}",nl]);
	{per,true}  ->
	    emit(["  {X,{_,Rest}} ->",nl,
		  "    {ok,if_binary2list(X),Rest};",nl,
		  "  {X,{_,Rest},_Len} ->",nl,
		  "    {ok,if_binary2list(X),Rest};",nl,
		  "  {X,Rest} ->",nl,
		  "    {ok,if_binary2list(X),Rest};",nl,
		  "  {X,Rest,_Len} ->",nl,
		  "    {ok,if_binary2list(X),Rest}",nl]);
	{per_bin,true} ->
	    emit(["  {X,{_,Rest}} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,{_,Rest},_Len} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,Rest} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,Rest,_Len} ->",nl,
		  "    {ok,X,Rest}",nl]);
	{uper_bin,true} ->
	    emit(["  {X,{_,Rest}} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,{_,Rest},_Len} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,Rest} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,Rest,_Len} ->",nl,
		  "    {ok,X,Rest}",nl]);
	_ ->
	    emit(["  {X,Rest} ->",nl,
		  "    {ok,X,Rest};",nl,
		  "  {X,Rest,_Len} ->",nl,
		  "    {ok,X,Rest}",nl])
    end,
    emit(["end.",nl,nl]),

    case Erules of
	per ->
	    emit(["if_binary2list(B) when is_binary(B) ->",nl,
		  "  binary_to_list(B);",nl,
		  "if_binary2list(L) -> L.",nl,nl]);
	_ ->
	    ok
    end,

    gen_decode_partial_incomplete(Erules),

    case Erules of
	ber ->
	    gen_dispatcher(Types,"encode_disp","enc_",",[]"),
	    gen_dispatcher(Types,"decode_disp","dec_",",mandatory");
	ber_bin ->
	    gen_dispatcher(Types,"encode_disp","enc_",",[]"),
	    gen_dispatcher(Types,"decode_disp","dec_",",mandatory");
	ber_bin_v2 ->
	    gen_dispatcher(Types,"encode_disp","enc_",""),
	    gen_dispatcher(Types,"decode_disp","dec_",""),
	    gen_partial_inc_dispatcher();
	_PerOrPer_bin -> 
	    gen_dispatcher(Types,"encode_disp","enc_",""),
	    gen_dispatcher(Types,"decode_disp","dec_",",mandatory")
    end,
    emit([nl]),

    case Erules of
	ber ->
	    gen_wrapper();
	_ -> ok
    end,
    emit({nl,nl}).


gen_decode_partial_incomplete(Erule) when Erule == ber;Erule==ber_bin;
					   Erule==ber_bin_v2 ->
    case {asn1ct:read_config_data(partial_incomplete_decode),
	  asn1ct:get_gen_state_field(inc_type_pattern)} of
	{undefined,_} ->
	    ok;
	{_,undefined} ->
	    ok;
	_ ->
	    case Erule of
		ber_bin_v2 ->
		    EmitCaseClauses =
			fun() ->
				emit(["   {'EXIT',{error,Reason}} ->",nl,
				      "      {error,Reason};",nl,
				      "    {'EXIT',Reason} ->",nl,
				      "      {error,{asn1,Reason}};",nl,
				      "    Result ->",nl,
				      "      {ok,Result}",nl,
				      "  end.",nl,nl])
			end,
		    emit(["decode_partial_incomplete(Type,Data0,",
			  "Pattern) ->",nl]),
		    emit(["  {Data,_RestBin} =",nl,
			  "    ?RT_BER:decode_primitive_",
			  "incomplete(Pattern,Data0),",nl,
			  "  case catch decode_partial_inc_disp(Type,",
			  "Data) of",nl]),
		    EmitCaseClauses(),
		    emit(["decode_part(Type,Data0) ->",nl]),
		    Driver =
			case lists:member(driver,get(encoding_options)) of
			    true ->
				",driver";
			    _ -> ""
			end,
		    emit(["  case catch decode_inc_disp(Type,element(1,?RT_BER:decode(Data0",Driver,"))) of",nl]),
% 			  "  {Data,_RestBin} = ?RT_BER:decode(Data0),",nl,
% 			  "  case catch decode_inc_disp(Type,Data) of",nl]),
		    EmitCaseClauses();
		_ -> ok % add later
	    end
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
	    gen_partial_inc_dispatcher(Data1,Data2)
    end.
gen_partial_inc_dispatcher([{FuncName,TopType,_Pattern}|Rest],TypePattern) ->
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
    emit(["decode_partial_inc_disp('",TopTypeName,"',Data) ->",nl,
	  "  ",{asis,list_to_atom(lists:concat(["dec-inc-",FuncName2]))},
	  "(Data);",nl]),
    gen_partial_inc_dispatcher(Rest,TypePattern);
gen_partial_inc_dispatcher([],_) ->
    emit(["decode_partial_inc_disp(Type,_Data) ->",nl,
	  "  exit({error,{asn1,{undefined_type,Type}}}).",nl]).

driver_parameter() ->
    Options = get(encoding_options),
    case lists:member(driver,Options) of
	true ->
	    ",driver";
	_ -> ""
    end.

gen_wrapper() ->
    emit(["wrap_encode(Bytes) when is_list(Bytes) ->",nl,
	  "   binary_to_list(list_to_binary(Bytes));",nl,
	  "wrap_encode(Bytes) when is_binary(Bytes) ->",nl,
	  "   binary_to_list(Bytes);",nl,
	  "wrap_encode(Bytes) -> Bytes.",nl,nl]),
    emit(["wrap_decode(Bytes) when is_list(Bytes) ->",nl,
	  "   list_to_binary(Bytes);",nl,
	  "wrap_decode(Bytes) -> Bytes.",nl]).
    
gen_dispatcher([F1,F2|T],FuncName,Prefix,ExtraArg) ->
	emit([FuncName,"('",F1,"',Data) -> '",Prefix,F1,"'(Data",ExtraArg,")",";",nl]),
	gen_dispatcher([F2|T],FuncName,Prefix,ExtraArg);
gen_dispatcher([Flast|_T],FuncName,Prefix,ExtraArg) ->
	emit([FuncName,"('",Flast,"',Data) -> '",Prefix,Flast,"'(Data",ExtraArg,")",";",nl]),
	emit([FuncName,"(","Type",",_Data) -> exit({error,{asn1,{undefined_type,Type}}}).",nl,nl,nl]).

pgen_info() ->
    emit(["info() ->",nl,
	  "   case ?MODULE:module_info() of",nl,
	  "      MI when is_list(MI) ->",nl,
	  "         case lists:keysearch(attributes,1,MI) of",nl,
	  "            {value,{_,Attributes}} when is_list(Attributes) ->",nl,
	  "               case lists:keysearch(asn1_info,1,Attributes) of",nl,
	  "                  {value,{_,Info}} when is_list(Info) ->",nl,
	  "                     Info;",nl,
	  "                  _ ->",nl,
	  "                     []",nl,
	  "               end;",nl,
	  "            _ ->",nl,
	  "               []",nl,
	  "         end",nl,
	  "   end.",nl]).

open_hrl(OutFile,Module) ->
    File = lists:concat([OutFile,".hrl"]),
    Fid = fopen(File,[write]),
    put(gen_file_out,Fid),
    gen_hrlhead(Module).

%% EMIT functions ************************
%% ***************************************

						% debug generation
demit(Term) ->
    case get(asndebug) of
	true -> emit(Term);
	_ ->true
    end.

						% always generation

emit({external,_M,T}) ->
    emit(T);

emit({prev,Variable}) when is_atom(Variable) ->
    emit({var,asn1ct_name:prev(Variable)});

emit({next,Variable}) when is_atom(Variable) ->
    emit({var,asn1ct_name:next(Variable)});

emit({curr,Variable}) when is_atom(Variable) ->
    emit({var,asn1ct_name:curr(Variable)});
    
emit({var,Variable}) when is_atom(Variable) ->
    [Head|V] = atom_to_list(Variable),
    emit([Head-32|V]);

emit({var,Variable}) ->
    [Head|V] = Variable,
    emit([Head-32|V]);

emit({asis,What}) ->
    format(get(gen_file_out),"~w",[What]);

emit(nl) ->
    nl(get(gen_file_out));

emit(com) ->
    emit(",");

emit(tab) ->
    put_chars(get(gen_file_out),"     ");

emit(What) when is_integer(What) ->
    put_chars(get(gen_file_out),integer_to_list(What));

emit(What) when is_list(What), is_integer(hd(What)) ->
    put_chars(get(gen_file_out),What);

emit(What) when is_atom(What) ->
    put_chars(get(gen_file_out),atom_to_list(What));

emit(What) when is_tuple(What) ->
    emit_parts(tuple_to_list(What));

emit(What) when is_list(What) ->
    emit_parts(What);

emit(X) ->
    exit({'cant emit ',X}).

emit_parts([]) -> true;
emit_parts([H|T]) ->
    emit(H),
    emit_parts(T).

format(undefined,X,Y) ->
    io:format(X,Y);
format(X,Y,Z) ->
    io:format(X,Y,Z).

nl(undefined) -> io:nl();
nl(X) -> io:nl(X).

put_chars(undefined,X) ->
    io:put_chars(X);
put_chars(Y,X) ->
    io:put_chars(Y,X).

fopen(F, ModeList) ->
    case file:open(F, ModeList) of
	{ok, Fd} -> 
	    Fd;
	{error, Reason} ->
	    io:format("** Can't open file ~p ~n", [F]),
	    exit({error,Reason})
    end.

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
	    Fid = get(gen_file_out),
	    file:close(Fid),
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
			case Rootl1 of
			    [] -> true;
			    _ -> emit([",",nl])
			end,
			emit(["%% with extensions",nl]),
			gen_record2(Name,'SEQUENCE',Extl,"",ext),
			case Extl of
			    [_H|_] when Rootl2 /= [] -> emit([",",nl]);
			    _ -> ok
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
    {Rtmac,Rtmod} = case Erules of
			per ->
			    emit({"%% Generated by the Erlang ASN.1 PER-"
				  "compiler version:",asn1ct:vsn(),nl}),
			    {"RT_PER",?RT_PER_BIN};
			ber ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version:",asn1ct:vsn(),nl}),
			    {"RT_BER",?RT_BER_BIN};
			per_bin ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version, utilizing bit-syntax:",
				  asn1ct:vsn(),nl}),
			    %% temporary code to enable rt2ct optimization
			    case lists:member(optimize,Options) of
				true -> {"RT_PER","asn1rt_per_bin_rt2ct"};
				_ ->    {"RT_PER",?RT_PER_BIN}
			    end;
			ber_bin ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version, utilizing bit-syntax:",
				  asn1ct:vsn(),nl}),
			    {"RT_BER",?RT_BER_BIN};
			ber_bin_v2 ->
			    emit({"%% Generated by the Erlang ASN.1 BER_V2-"
				  "compiler version, utilizing bit-syntax:",
				  asn1ct:vsn(),nl}),
			    {"RT_BER","asn1rt_ber_bin_v2"};
			uper_bin ->
			    emit(["%% Generated by the Erlang ASN.1 UNALIGNED"
				  " PER-compiler version, utilizing"
				  " bit-syntax:",
				  asn1ct:vsn(),nl]),
			    {"RT_PER","asn1rt_uper_bin"}
    end,
    emit({"%% Purpose: encoder and decoder to the types in mod ",Mod,nl,nl}),
    emit({"-module('",Mod,"').",nl}),
    put(currmod,Mod),
    %emit({"-compile(export_all).",nl}),
    case Hrl of
	0 -> true;
	_ -> 
	    emit({"-include(\"",Mod,".hrl\").",nl})
    end,
    emit(["-define('",Rtmac,"',",Rtmod,").",nl]),
    emit(["-asn1_info([{vsn,'",asn1ct:vsn(),"'},",nl,
	  "            {module,'",Mod,"'},",nl,
	  "            {options,",io_lib:format("~w",[Options]),"}]).",nl,nl]).
			

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
gen_record2(Name,SeqOrSet,[{'EXTENSIONMARK',_,_}|T],Com,Extension) ->
    gen_record2(Name,SeqOrSet,T,Com,Extension);
gen_record2(_Name,_SeqOrSet,[H],Com,Extension) ->
    #'ComponentType'{name=Cname} = H,
    emit(Com),
    emit({asis,Cname}),
    gen_record_default(H, Extension);
gen_record2(Name,SeqOrSet,[H|T],Com, Extension) ->
    #'ComponentType'{name=Cname} = H,
    emit(Com),
    emit({asis,Cname}),
    gen_record_default(H, Extension),
    gen_record2(Name,SeqOrSet,T,", ", Extension).

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
    

gen_check_call(TopType,Cname,Type,InnerType,WhatKind,DefaultValue,Element) ->
    case WhatKind of
	{primitive,bif} ->
	    gen_prim_check_call(InnerType,DefaultValue,Element,Type);
	#'Externaltypereference'{module=M,type=T} ->
	    %% generate function call
	    Name = list2name([T,check]),
	    emit({"'",Name,"'(",DefaultValue,", ",Element,")"}),
	    %% insert in ets table and do look ahead check
	    Typedef = asn1_db:dbget(M,T),
	    RefType = Typedef#typedef.typespec,
	    InType = asn1ct_gen:get_inner(RefType#type.def),
	    case insert_once(check_functions,{Name,RefType}) of
		true ->
		    lookahead_innertype([T],InType,RefType);
		_ ->
		    ok
	    end;
	{constructed,bif} ->
	    NameList = [Cname|TopType],
	    Name = list2name(NameList ++ [check]),
	    emit({"'",Name,"'(",DefaultValue,", ",Element,")"}),
	    ets:insert(check_functions,{Name,Type}),
	    %% Must look for check functions in InnerType,
	    %% that may be referenced  or internal defined 
	    %% constructed types not used elsewhere.
	    lookahead_innertype(NameList,InnerType,Type);
	_ ->
	    %% Generate Dummy function call i.e. anything is accepted
	    emit(["fun() -> true end ()"])
    end.

gen_prim_check_call(PrimType,DefaultValue,Element,Type) ->
    case unify_if_string(PrimType) of
	'BOOLEAN' ->
	    emit({"asn1rt_check:check_bool(",DefaultValue,", ",
		  Element,")"});
	'INTEGER' ->
	    NNL =
		case Type#type.def of
		    {_,NamedNumberList} -> NamedNumberList;
		    _ -> []
		end,
	    emit({"asn1rt_check:check_int(",DefaultValue,", ",
		  Element,", ",{asis,NNL},")"});
	'BIT STRING' ->
	    {_,NBL} = Type#type.def,
	    emit({"asn1rt_check:check_bitstring(",DefaultValue,", ",
		  Element,", ",{asis,NBL},")"});
	'OCTET STRING' ->
	    emit({"asn1rt_check:check_octetstring(",DefaultValue,", ",
		  Element,")"});
	'NULL' ->
	    emit({"asn1rt_check:check_null(",DefaultValue,", ",
		  Element,")"});
	'OBJECT IDENTIFIER' ->
	    emit({"asn1rt_check:check_objectidentifier(",DefaultValue,
		  ", ",Element,")"});
	'RELATIVE-OID' ->
	    emit({"asn1rt_check:check_objectidentifier(",DefaultValue,
		  ", ",Element,")"});
	'ObjectDescriptor' ->
	    emit({"asn1rt_check:check_objectdescriptor(",DefaultValue,
		  ", ",Element,")"});
	'REAL' ->
	    emit({"asn1rt_check:check_real(",DefaultValue,
		  ", ",Element,")"});
	'ENUMERATED' ->
	    {_,Enumerations} = Type#type.def,
	    emit({"asn1rt_check:check_enum(",DefaultValue,
		  ", ",Element,", ",{asis,Enumerations},")"});
	restrictedstring ->
	    emit({"asn1rt_check:check_restrictedstring(",DefaultValue,
		  ", ",Element,")"})
    end.

%% lokahead_innertype/3 traverses Type and checks if check functions
%% have to be generated, i.e. for all constructed or referenced types.
lookahead_innertype(Name,'SEQUENCE',Type) ->
    Components = (Type#type.def)#'SEQUENCE'.components,
    lookahead_components(Name,Components);
lookahead_innertype(Name,'SET',Type) ->
    Components = (Type#type.def)#'SET'.components,
    lookahead_components(Name,Components);
lookahead_innertype(Name,'CHOICE',Type) ->
    {_,Components} = Type#type.def,
    lookahead_components(Name,Components);
lookahead_innertype(Name,'SEQUENCE OF',SeqOf) ->
    lookahead_sof(Name,'SEQOF',SeqOf);
lookahead_innertype(Name,'SET OF',SeqOf) ->
    lookahead_sof(Name,'SETOF',SeqOf);
lookahead_innertype(_Name,#'Externaltypereference'{module=M,type=T},_) ->
    Typedef = asn1_db:dbget(M,T),
    RefType = Typedef#typedef.typespec,
    insert_once(check_functions,{list2name([T,check]),RefType}),
    InType = asn1ct_gen:get_inner(RefType#type.def),
    case type(InType) of
	{constructed,bif} ->
	    lookahead_innertype([T],InType,RefType);
	Ref = #'Externaltypereference'{} ->
	    lookahead_reference(Ref);
	_ ->
	    ok
    end;
lookahead_innertype(_,_,_) ->
    ok.

lookahead_components(_,[]) -> ok;
lookahead_components(Name,[C|Cs]) ->
    #'ComponentType'{name=Cname,typespec=Type} = C,
    InType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InType) of
	{constructed,bif} ->
	    case insert_once(check_functions,
			     {list2name([Cname|Name] ++ [check]),Type}) of
		true ->
		    lookahead_innertype([Cname|Name],InType,Type);
		_ ->
		    ok
	    end;
	#'Externaltypereference'{module=RefMod,type=RefName} ->
  	    Typedef = asn1_db:dbget(RefMod,RefName),
  	    RefType = Typedef#typedef.typespec,
  	    case insert_once(check_functions,{list2name([RefName,check]),
  					      RefType}) of
  		true ->
  		    lookahead_innertype([RefName],InType,RefType);
  		_ ->
  		    ok
  	    end;
	_ ->
	    ok
    end,
    lookahead_components(Name,Cs).

lookahead_sof(Name,SOF,SOFType) ->
    Type = case SOFType#type.def of
	       {_,_Type} -> _Type;
	       _Type -> _Type
	   end,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    %% this is if a constructed type is defined in
	    %% the SEQUENCE OF type
	    NameList = [SOF|Name],
	    insert_once(check_functions,
			{list2name(NameList ++ [check]),Type}),
	    lookahead_innertype(NameList,InnerType,Type);
	Ref = #'Externaltypereference'{} ->
	    lookahead_reference(Ref);
	_ ->
	    ok
    end.

lookahead_reference(#'Externaltypereference'{module=M,type=T}) ->
    Typedef = asn1_db:dbget(M,T),
    RefType = Typedef#typedef.typespec,
    InType = get_inner(RefType#type.def),
    case insert_once(check_functions,
		     {list2name([T,check]),RefType}) of
	true ->
	    lookahead_innertype([T],InType,RefType);
	_ ->
	    ok
    end.

insert_once(Table,Object) ->
    _Info = ets:info(Table),
    case ets:lookup(Table,element(1,Object)) of
	[] ->
	    ets:insert(Table,Object); %returns true
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


	
	

get_inner(A) when is_atom(A) -> A;    
get_inner(Ext) when is_record(Ext,'Externaltypereference') -> Ext;    
get_inner(Tref) when is_record(Tref,typereference) -> Tref;
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
type(X) when is_record(X,typereference) ->
    X;
type('ASN1_OPEN_TYPE') ->
    'ASN1_OPEN_TYPE';
type({fixedtypevaluefield,_Name,Type}) when is_record(Type,type) ->
    type(get_inner(Type#type.def));
type({typefield,_}) ->
    'ASN1_OPEN_TYPE';
type(X) ->
    %%    io:format("asn1_types:type(~p)~n",[X]),
    case catch type2(X) of
	{'EXIT',_} ->
	    {notype,X};
	Normal ->
	    Normal
    end.

type2(X) ->
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
		    'ANY',
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

type_from_object(X) ->
    case (catch lists:last(element(2,X))) of
	{'EXIT',_} ->
	    {notype,X};
	Normal ->
	    Normal
    end.


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

get_fieldcategory([],_FieldName) ->
    no_cat;
get_fieldcategory([Field|Rest],FieldName) ->
    case element(2,Field) of
	FieldName ->
	    element(1,Field);
	_ ->
	    get_fieldcategory(Rest,FieldName)
    end.

get_typefromobject(Type) when is_record(Type,type) ->
    case Type#type.def of
	{{objectclass,_,_},TypeFrObj} when is_list(TypeFrObj) ->
	    {_,FieldName} = lists:last(TypeFrObj),
	    FieldName;
	_ ->
	    {no_field}
    end.

get_classfieldcategory(Type,FieldName) ->
    case (catch Type#type.def) of
	{{obejctclass,Fields,_},_} ->
	    get_fieldcategory(Fields,FieldName);
	{'EXIT',_} ->
	    no_cat;
	_ ->
	    no_cat
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

erule(ber) ->
    ber;
erule(ber_bin) ->
    ber;
erule(ber_bin_v2) ->
    ber_bin_v2;
erule(per) ->
    per;
erule(per_bin) ->
    per;
erule(uper_bin) ->
    per.

wrap_ber(ber) ->
    ber_bin;
wrap_ber(Erule) ->
    Erule.

rt2ct_suffix() ->
    Options = get(encoding_options),
    case {lists:member(optimize,Options),lists:member(per_bin,Options)} of
	{true,true} -> "_rt2ct";
	_ -> ""
    end.
rt2ct_suffix(per_bin) ->
    Options = get(encoding_options),
    case lists:member(optimize,Options) of
	true -> "_rt2ct";
	_ -> ""
    end;
rt2ct_suffix(_) -> "".

index2suffix(0) ->
    "";
index2suffix(N) ->
    lists:concat(["_",N]).

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V;
	{value,Cnstr} ->
	    Cnstr
    end.

ensure_atom(Atom) when is_atom(Atom) ->
    Atom;
ensure_atom(List) when is_list(List) ->
    list_to_atom(List).
    
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
