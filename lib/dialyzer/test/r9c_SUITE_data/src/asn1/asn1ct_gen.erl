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
%%     $Id: asn1ct_gen.erl,v 1.1 2008/12/17 09:53:29 mikpe Exp $
-module(asn1ct_gen).

-include("asn1_records.hrl").
%%-compile(export_all).
-export([pgen_exports/3,
	 pgen_hrl/4,
	 gen_head/3,
	 demit/1,
	 emit/1,
	 fopen/2,
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
	 rt2ct_suffix/1,rt2ct_suffix/0]).
-export([pgen/4,pgen_module/5,mk_var/1, un_hyphen_var/1]).
-export([gen_encode_constructed/4,gen_decode_constructed/4]).

%% pgen(Erules, Module, TypeOrVal)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber | ber_bin | per_bin
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList}
%% TypeList = ValueList = [atom()]

pgen(OutFile,Erules,Module,TypeOrVal) ->
    pgen_module(OutFile,Erules,Module,TypeOrVal,true).


pgen_module(OutFile,Erules,Module,TypeOrVal,Indent) ->
    put(outfile,OutFile),
    HrlGenerated = asn1ct_gen:pgen_hrl(Erules,Module,TypeOrVal,Indent),
    asn1ct_name:start(),
    ErlFile = lists:concat([OutFile,".erl"]),
    Fid = asn1ct_gen:fopen(ErlFile,write),
    put(gen_file_out,Fid),
    asn1ct_gen:gen_head(Erules,Module,HrlGenerated),
    pgen_exports(Erules,Module,TypeOrVal),
    pgen_dispatcher(Erules,Module,TypeOrVal),
    pgen_info(Erules,Module),
    pgen_typeorval(wrap_ber(Erules),Module,TypeOrVal),
    pgen_partial_incomplete_decode(Erules),
% gen_vars(asn1_db:mod_to_vars(Module)),
% gen_tag_table(AllTypes),
    file:close(Fid),
    io:format("--~p--~n",[{generated,ErlFile}]).


pgen_typeorval(Erules,Module,{Types,Values,_Ptypes,_Classes,Objects,ObjectSets}) ->
    pgen_types(Erules,Module,Types),
    pgen_values(Erules,Module,Values),
    pgen_objects(Erules,Module,Objects),
    pgen_objectsets(Erules,Module,ObjectSets),
    case catch lists:member(der,get(encoding_options)) of
	true ->
	    pgen_check_defaultval(Erules,Module);
	_ -> ok
    end,
    pgen_partial_decode(Erules,Module).

pgen_values(_,_,[]) ->
    true;
pgen_values(Erules,Module,[H|T]) ->
    Valuedef = asn1_db:dbget(Module,H),
    gen_value(Valuedef),
    pgen_values(Erules,Module,T).

pgen_types(_,Module,[]) ->
    gen_value_match(Module),
    true;
pgen_types(Erules,Module,[H|T]) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    asn1ct_name:clear(),
    Typedef = asn1_db:dbget(Module,H),
    Rtmod:gen_encode(Erules,Typedef),
    asn1ct_name:clear(),
    Rtmod:gen_decode(Erules,Typedef),
    pgen_types(Erules,Module,T).

pgen_objects(_,_,[]) ->
    true;
pgen_objects(Erules,Module,[H|T]) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    asn1ct_name:clear(),
    Typedef = asn1_db:dbget(Module,H),
    Rtmod:gen_obj_code(Erules,Module,Typedef),
    pgen_objects(Erules,Module,T).

pgen_objectsets(_,_,[]) ->
    true;
pgen_objectsets(Erules,Module,[H|T]) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    asn1ct_name:clear(),
    TypeDef = asn1_db:dbget(Module,H),
    Rtmod:gen_objectset_code(Erules,TypeDef),
    pgen_objectsets(Erules,Module,T).

pgen_check_defaultval(Erules,Module) ->
    CheckObjects = ets:tab2list(check_functions),
    case get(asndebug) of
	true ->
	    FileName = lists:concat([Module,'.table']),
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

pgen_partial_decode(Erules,Module) ->
    pgen_partial_inc_dec(Erules,Module),
    pgen_partial_dec(Erules,Module).

pgen_partial_inc_dec(Erules,Module) ->
%    io:format("Start partial incomplete decode gen?~n"),
    case asn1ct:get_gen_state_field(inc_type_pattern) of
	undefined ->
%	    io:format("Partial incomplete decode gen not started: ~w~n",[asn1ct:get_gen_state_field(active)]),
	    ok;
%	[] ->
%	    ok;
	ConfList ->
	    PatternLists=lists:map(fun({_,P}) -> P end,ConfList),
	    pgen_partial_inc_dec1(Erules,Module,PatternLists),
	    gen_partial_inc_dec_refed_funcs(Erules)
    end.

%% pgen_partial_inc_dec1 generates a function of the toptype in each
%% of the partial incomplete decoded types.
pgen_partial_inc_dec1(Erules,Module,[P|Ps]) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    TopTypeName = asn1ct:partial_inc_dec_toptype(P),
    TypeDef=asn1_db:dbget(Module,TopTypeName),
    asn1ct_name:clear(),
    asn1ct:update_gen_state(namelist,P),
    asn1ct:update_gen_state(active,true),
    asn1ct:update_gen_state(prefix,"dec-inc-"),
    Rtmod:gen_decode(Erules,TypeDef),
%%    asn1ct:update_gen_state(namelist,tl(P)), %%
    gen_dec_part_inner_constr(Erules,TypeDef,[TopTypeName]),
    pgen_partial_inc_dec1(Erules,Module,Ps);
pgen_partial_inc_dec1(_,_,[]) ->
    ok.

gen_partial_inc_dec_refed_funcs(Erule) when Erule == ber_bin_v2 ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erule),
				       rt2ct_suffix(Erule)])),
    case asn1ct:next_refed_func() of
	[] ->
	    ok;
	{#'Externaltypereference'{module=M,type=Name},Pattern} ->
	    TypeDef = asn1_db:dbget(M,Name),
	    asn1ct:update_gen_state(namelist,Pattern),
	    Rtmod:gen_inc_decode(Erule,TypeDef),
	    gen_dec_part_inner_constr(Erule,TypeDef,[Name]),
	    gen_partial_inc_dec_refed_funcs(Erule);
	_ ->
	    gen_partial_inc_dec_refed_funcs(Erule)
    end;
gen_partial_inc_dec_refed_funcs(_) ->
    ok.

pgen_partial_dec(_Erules,_Module) ->
    ok. %%%% implement later

%% generate code for all inner types that are called from the top type
%% of the partial incomplete decode
gen_dec_part_inner_constr(Erules,TypeDef,TypeName) ->
    Def = TypeDef#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case InnerType of
	'SET' ->
	    #'SET'{components=Components} = Def#type.def,
	    gen_dec_part_inner_types(Erules,Components,TypeName);
	%%  Continue generate the inner of each component
	'SEQUENCE' ->
	    #'SEQUENCE'{components=Components} = Def#type.def,
	    gen_dec_part_inner_types(Erules,Components,TypeName);
	'CHOICE' ->
	    {_,Components} = Def#type.def,
	    gen_dec_part_inner_types(Erules,Components,TypeName);
	'SEQUENCE OF' ->
	    %% this and next case must be the last component in the
	    %% partial decode chain here. Not likely that this occur.
	    {_,Type} = Def#type.def,
	    NameSuffix = constructed_suffix(InnerType,Type#type.def),
	    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
					       rt2ct_suffix(Erules)])),
	    asn1ct_name:clear(),
	    Rtmod:gen_decode(Erules,[NameSuffix|TypeName],Type);
%%	    gen_types(Erules,[NameSuffix|Typename],Type);
	'SET OF' ->
	    {_,Type} = Def#type.def,
	    NameSuffix = constructed_suffix(InnerType,Type#type.def),
	    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
					       rt2ct_suffix(Erules)])),
	    asn1ct_name:clear(),
	    Rtmod:gen_decode(Erules,[NameSuffix|TypeName],Type);
	_ ->
	    ok
    end.

gen_dec_part_inner_types(Erules,[ComponentType|Rest],TypeName) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_gen_",erule(Erules),
				       rt2ct_suffix(Erules)])),
    asn1ct_name:clear(),
    Rtmod:gen_decode(Erules,TypeName,ComponentType),
    gen_dec_part_inner_types(Erules,Rest,TypeName);
gen_dec_part_inner_types(Erules,{Comps1,Comps2},TypeName)
  when list(Comps1),list(Comps2) ->
    gen_dec_part_inner_types(Erules,Comps1 ++ Comps2,TypeName);
gen_dec_part_inner_types(_,[],_) ->
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

emit_partial_incomplete_decode({FuncName,TopTypeName,Pattern}) ->
    emit([{asis,FuncName},"(Bytes) ->",nl,
	  "  decode_partial_incomplete(",{asis,TopTypeName},",Bytes,",{asis,Pattern},").",nl]);
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
	  "    L when list(L) ->",nl,
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

gen_types(Erules,Tname,{RootList,ExtList}) when list(RootList) ->
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
gen_types(Erules,Tname,Type) when record(Type,type) ->
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
    emit({Name,"(V,asn1_DEFAULT) ->",nl,"   true;",nl}),
    emit({Name,"(V,V) ->",nl,"   true;",nl}),
    emit({Name,"(V,{_,V}) ->",nl,"   true;",nl}),
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
	    emit({Name,"(DefaultValue,Value) ->",nl}),
	    emit({"   ",list2name([T,check]),"(DefaultValue,Value).",nl});
	MaybePrim ->
	    InnerType = get_inner(MaybePrim),
	    case type(InnerType) of
		{primitive,bif} ->
		    emit({Name,"(DefaultValue,Value) ->",nl,"   "}),
		    gen_prim_check_call(InnerType,"DefaultValue","Value",
					FType),
		    emit({".",nl,nl});
		_ ->
		    throw({asn1_error,{unknown,type,MaybePrim}})
	    end
    end.

gen_check_sof(Name,SOF,Type) ->
    NewName = list2name([sorted,Name]),
    emit({Name,"(V1,V2) ->",nl}),
    emit({"   ",NewName,"(lists:sort(V1),lists:sort(V2)).",nl,nl}),
    emit({NewName,"([],[]) ->",nl,"   true;",nl}),
    emit({NewName,"([DV|DVs],[V|Vs]) ->",nl,"   "}),
    InnerType = get_inner(Type#type.def),
    case type(InnerType) of
	{primitive,bif} ->
	    gen_prim_check_call(InnerType,"DV","V",Type),
	    emit({",",nl});
	{constructed,bif} ->
	    emit({list2name([SOF,Name]),"(DV, V),",nl});
	#'Externaltypereference'{type=T} ->
	    emit({list2name([T,check]),"(DV,V),",nl})
    end,
    emit({"   ",NewName,"(DVs,Vs).",nl,nl}).

gen_check_sequence(Name,Components) ->
    emit({Name,"(DefaultValue,Value) ->",nl}),
    gen_check_sequence(Name,Components,1).
gen_check_sequence(Name,[#'ComponentType'{name=N,typespec=Type}|Cs],Num) ->
    InnerType = get_inner(Type#type.def),
%    NthDefV = lists:concat(["lists:nth(",Num,",DefaultValue)"]),
    NthDefV = ["element(",Num+1,",DefaultValue)"],
%    NthV = lists:concat(["lists:nth(",Num,",Value)"]),
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
    emit({Name,"({Id,DefaultValue},{Id,Value}) ->",nl}),
    emit({"   case Id of",nl}),
    gen_check_choice_components(Name,CList,1).

gen_check_choice_components(_,[],_)->
    ok;
gen_check_choice_components(Name,[#'ComponentType'{name=N,typespec=Type}|
				  Cs],Num) ->
    Ind6 = "      ",
    InnerType = get_inner(Type#type.def),
%    DefVal = ["element(2,lists:nth(",Num,",DefaultValue))"],
    emit({Ind6,N," ->",nl,Ind6}),
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
	    gen_prim_check_call(InnerType,DefVal,Val,Type);
	#'Externaltypereference'{type=T} ->
	    emit({"   ",list2name([T,check]),"(",DefVal,",",Val,")"});
	_ ->
	    emit({"   ",list2name([N,Name]),"(",DefVal,",",Val,")"})
    end.


%% VARIOUS GENERATOR STUFF
%% *************************************************
%%**************************************************

mk_var(X) when atom(X) ->
    list_to_atom(mk_var(atom_to_list(X)));

mk_var([H|T]) ->
    [H-32|T].

%% Since hyphens are allowed in ASN.1 names, it may occur in a
%% variable to. Turn a hyphen into a under-score sign.
un_hyphen_var(X) when atom(X) ->
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

gen_value(Value) when record(Value,valuedef) ->
%%    io:format(" ~w ",[Value#valuedef.name]),
    emit({"'",Value#valuedef.name,"'() ->",nl}),
    V = Value#valuedef.value,
    emit([{asis,V},".",nl,nl]).

gen_encode_constructed(Erules,Typename,InnerType,D) when record(D,type) ->

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
  when record(D,typedef) ->
    gen_encode_constructed(Erules,Typename,InnerType,D#typedef.typespec).

gen_decode_constructed(Erules,Typename,InnerType,D) when record(D,type) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_constructed_",erule(Erules)])),
    asn1ct:step_in_constructed(), %% updates namelist for incomplete
                                  %% partial decode
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


gen_decode_constructed(Erules,Typename,InnerType,D) when record(D,typedef) ->
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
		ber_bin_v2 ->
		    emit({"-export([",nl}),
		    gen_exports1(Types,"dec_",2);
		_ -> ok
	    end
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
	    emit("-export([decode_part/2]).")
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

pgen_dispatcher(Erules,_Module,{[],_Values,_,_,_Objects,_ObjectSets}) ->
    emit(["encoding_rule() ->",nl]),
    emit([{asis,Erules},".",nl,nl]);
pgen_dispatcher(Erules,_Module,{Types,_Values,_,_,_Objects,_ObjectSets}) ->
    emit(["-export([encode/2,decode/2,encode_disp/2,decode_disp/2]).",nl,nl]),
    emit(["encoding_rule() ->",nl]),
    emit(["   ",{asis,Erules},".",nl,nl]),
    Call = case Erules of
	       per -> "?RT_PER:complete(encode_disp(Type,Data))";
	       per_bin -> "?RT_PER:complete(encode_disp(Type,Data))";
	       ber -> "encode_disp(Type,Data)";
	       ber_bin -> "encode_disp(Type,Data)";
	       ber_bin_v2 -> "encode_disp(Type,Data)"
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
	  "    {ok,",EncWrap,"};",nl,
	  "  Bytes ->",nl,
	  "    {ok,",EncWrap,"}",nl,
	  "end.",nl,nl]),

    case Erules of
	ber_bin_v2 ->
	    emit(["decode(Type,Data0) ->",nl]),
	    emit(["{Data,_RestBin} = ?RT_BER:decode(Data0",driver_parameter(),"),",nl]);
	_ ->
	    emit(["decode(Type,Data) ->",nl])
    end,
    DecWrap = case Erules of
		  ber -> "wrap_decode(Data)";
		  _ -> "Data"
	      end,

    emit(["case catch decode_disp(Type,",DecWrap,") of",nl,
	  "  {'EXIT',{error,Reason}} ->",nl,
	  "    {error,Reason};",nl,
	  "  {'EXIT',Reason} ->",nl,
	  "    {error,{asn1,Reason}};",nl]),
    case Erules of
	ber_bin_v2 ->
	    emit(["  Result ->",nl,
		  "    {ok,Result}",nl]);
	_ ->
	    emit(["  {X,_Rest} ->",nl,
		  "    {ok,X};",nl,
		  "  {X,_Rest,_Len} ->",nl,
		  "    {ok,X}",nl])
    end,
    emit(["end.",nl,nl]),

    gen_decode_partial_incomplete(Erules),

    case Types of
	[] -> ok;
	_ ->
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
	    emit([nl])
    end,
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
		    emit(["decode_part(Type,Data0) ->",nl,
			  "  {Data,_RestBin} = ?RT_BER:decode(Data0),",nl,
			  "  case catch decode_inc_disp(Type,Data) of",nl]),
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
	{Data,_} ->
	    gen_partial_inc_dispatcher(Data)
    end.
gen_partial_inc_dispatcher([{_FuncName,TopType,_Pattern}|Rest]) ->
    emit(["decode_partial_inc_disp(",{asis,TopType},",Data) ->",nl,
	  "  ",{asis,list_to_atom(lists:concat([dec,"-inc-",TopType]))},
	  "(Data);",nl]),
    gen_partial_inc_dispatcher(Rest);
gen_partial_inc_dispatcher([]) ->
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
    emit(["wrap_encode(Bytes) when list(Bytes) ->",nl,
	  "   binary_to_list(list_to_binary(Bytes));",nl,
	  "wrap_encode(Bytes) when binary(Bytes) ->",nl,
	  "   binary_to_list(Bytes);",nl,
	  "wrap_encode(Bytes) -> Bytes.",nl,nl]),
    emit(["wrap_decode(Bytes) when list(Bytes) ->",nl,
	  "   list_to_binary(Bytes);",nl,
	  "wrap_decode(Bytes) -> Bytes.",nl]).

gen_dispatcher([F1,F2|T],FuncName,Prefix,ExtraArg) ->
	emit([FuncName,"('",F1,"',Data) -> '",Prefix,F1,"'(Data",ExtraArg,")",";",nl]),
	gen_dispatcher([F2|T],FuncName,Prefix,ExtraArg);
gen_dispatcher([Flast|_T],FuncName,Prefix,ExtraArg) ->
	emit([FuncName,"('",Flast,"',Data) -> '",Prefix,Flast,"'(Data",ExtraArg,")",";",nl]),
	emit([FuncName,"(","Type",",_Data) -> exit({error,{asn1,{undefined_type,Type}}}).",nl,nl,nl]).

pgen_info(_Erules,Module) ->
    Options = get(encoding_options),
    emit({"info() ->",nl,
	  "  [{vsn,'",asn1ct:vsn(),"'},",
	  "   {module,'",Module,"'},",
	  "   {options,",io_lib:format("~p",[Options]),"}].",nl}).

open_hrl(OutFile,Module) ->
    File = lists:concat([OutFile,".hrl"]),
    Fid = fopen(File,write),
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

emit({prev,Variable}) when atom(Variable) ->
    emit({var,asn1ct_name:prev(Variable)});

emit({next,Variable}) when atom(Variable) ->
    emit({var,asn1ct_name:next(Variable)});

emit({curr,Variable}) when atom(Variable) ->
    emit({var,asn1ct_name:curr(Variable)});

emit({var,Variable}) when atom(Variable) ->
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

emit(What) when integer(What) ->
    put_chars(get(gen_file_out),integer_to_list(What));

emit(What) when list(What), integer(hd(What)) ->
    put_chars(get(gen_file_out),What);

emit(What) when atom(What) ->
    put_chars(get(gen_file_out),atom_to_list(What));

emit(What) when tuple(What) ->
    emit_parts(tuple_to_list(What));

emit(What) when list(What) ->
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

fopen(F, Mode) ->
    case file:open(F, [Mode]) of
	{ok, Fd} ->
	    Fd;
	{error, Reason} ->
	    io:format("** Can't open file ~p ~n", [F]),
	    exit({error,Reason})
    end.

pgen_hrl(Erules,Module,TypeOrVal,_Indent) ->
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
	    io:format("--~p--~n",
		      [{generated,lists:concat([get(outfile),".hrl"])}]),
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
gen_macro(Value) when record(Value,valuedef) ->
    emit({"-define('",Value#valuedef.name,"', ",
	  {asis,Value#valuedef.value},").",nl}).

%% Generate record functions **************
%% Generates an Erlang record for each named and unnamed SEQUENCE and SET in the ASN.1
%% module. If no SEQUENCE or SET is found there is no .hrl file generated


gen_record(Tdef,NumRecords) when record(Tdef,typedef) ->
    Name = [Tdef#typedef.name],
    Type = Tdef#typedef.typespec,
    gen_record(type,Name,Type,NumRecords);

gen_record(Tdef,NumRecords) when record(Tdef,ptypedef) ->
    Name = [Tdef#ptypedef.name],
    Type = Tdef#ptypedef.typespec,
    gen_record(ptype,Name,Type,NumRecords).

gen_record(TorPtype,Name,[#'ComponentType'{name=Cname,typespec=Type}|T],Num) ->
    Num2 = gen_record(TorPtype,[Cname|Name],Type,Num),
    gen_record(TorPtype,Name,T,Num2);
gen_record(TorPtype,Name,{Clist1,Clist2},Num) when list(Clist1), list(Clist2) ->
    gen_record(TorPtype,Name,Clist1++Clist2,Num);
gen_record(TorPtype,Name,[_|T],Num) -> % skip EXTENSIONMARK
    gen_record(TorPtype,Name,T,Num);
gen_record(_TorPtype,_Name,[],Num) ->
    Num;

gen_record(TorPtype,Name,Type,Num) when record(Type,type) ->
    Def = Type#type.def,
    Rec = case Def of
	      Seq when record(Seq,'SEQUENCE') ->
		  case Seq#'SEQUENCE'.pname of
		      false ->
			  {record,Seq#'SEQUENCE'.components};
		      _Pname when TorPtype == type ->
			  false;
		      _ ->
			  {record,Seq#'SEQUENCE'.components}
		  end;
	      Set when record(Set,'SET') ->
		  case Set#'SET'.pname of
		      false ->
			  {record,Set#'SET'.components};
		      _Pname when TorPtype == type ->
			  false;
		      _ ->
			  {record,Set#'SET'.components}
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
	    emit({"-record('",list2name(Name),"',{",nl}),
	    RootList = case CompList of
			   _ when list(CompList) ->
			       CompList;
			   {_Rl,_} -> _Rl
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
    {Rtmac,Rtmod} = case Erules of
			per ->
			    emit({"%% Generated by the Erlang ASN.1 PER-"
				  "compiler version:",asn1ct:vsn(),nl}),
			    {"RT_PER",?RT_PER};
			ber ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version:",asn1ct:vsn(),nl}),
			    {"RT_BER",?RT_BER_BIN};
			per_bin ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version, utilizing bit-syntax:",
				  asn1ct:vsn(),nl}),
			    %% temporary code to enable rt2ct optimization
			    Options = get(encoding_options),
			    case lists:member(optimize,Options) of
				true -> {"RT_PER","asn1rt_per_bin_rt2ct"};
				_ ->
				    {"RT_PER",?RT_PER_BIN}
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
			    {"RT_BER","asn1rt_ber_bin_v2"}
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
    emit(["-define('",Rtmac,"',",Rtmod,").",nl]).


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
%    emit(", "),
    gen_record2(Name,SeqOrSet,T,", ", Extension).

%gen_record_default(C, ext) ->
%    emit(" = asn1_NOEXTVALUE");
gen_record_default(#'ComponentType'{prop='OPTIONAL'}, _)->
    emit(" = asn1_NOVALUE");
gen_record_default(#'ComponentType'{prop={'DEFAULT',_}}, _)->
    emit(" = asn1_DEFAULT");
gen_record_default(_, _) ->
    true.

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
%		    case asn1ct_gen:type(InType) of
%			{constructed,bif} ->
%			    lookahead_innertype([T],InType,RefType);
%			#'Externaltypereference'{type=TNew} ->
%			    lookahead_innertype([TNew],InType,RefType);
%			_ ->
%			    ok
%		    end;
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
	    lookahead_innertype(NameList,InnerType,Type)
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
    InType = asn1ct_gen:get_inner(RefType#type.def),
    case type(InType) of
	{constructed,bif} ->
	    NewName = list2name([T,check]),
	    case insert_once(check_functions,{NewName,RefType}) of
		true ->
		    lookahead_innertype([T],InType,RefType);
		_ ->
		    ok
	    end;
	#'Externaltypereference'{} ->
	    NewName = list2name([T,check]),
	    case insert_once(check_functions,{NewName,RefType}) of
		true ->
		    lookahead_innertype([T],InType,RefType);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end;
%    case insert_once(check_functions,{list2name(Name++[check]),Type}) of
%	true ->
%	    InnerType = asn1ct_gen:get_inner(Type#type.def),
%	    case asn1ct_gen:type(InnerType) of
%		{constructed,bif} ->
%		    lookahead_innertype([T],InnerType,Type);
%		#'Externaltypereference'{type=TNew} ->
%		    lookahead_innertype([TNew],InnerType,Type);
%		_ ->
%		    ok
%	    end;
%	_ ->
%	    ok
%    end;
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
	#'Externaltypereference'{module=M,type=T} ->
	    Typedef = asn1_db:dbget(M,T),
	    RefType = Typedef#typedef.typespec,
	    InType = get_inner(RefType#type.def),
	    case insert_once(check_functions,
			     {list2name([T,check]),RefType}) of
		true ->
		    lookahead_innertype([T],InType,RefType);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.


insert_once(Table,Object) ->
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
	Other -> Other
    end.





get_inner(A) when atom(A) -> A;
get_inner(Ext) when record(Ext,'Externaltypereference') -> Ext;
get_inner(Tref) when record(Tref,typereference) -> Tref;
get_inner({fixedtypevaluefield,_,Type}) ->
    if
	record(Type,type) ->
	    get_inner(Type#type.def);
	true ->
	    get_inner(Type)
    end;
get_inner({typefield,TypeName}) ->
    TypeName;
get_inner(#'ObjectClassFieldType'{type=Type}) ->
%    get_inner(Type);
    Type;
get_inner(T) when tuple(T) ->
    case element(1,T) of
	Tuple when tuple(Tuple),element(1,Tuple) == objectclass ->
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





type(X) when record(X,'Externaltypereference') ->
    X;
type(X) when record(X,typereference) ->
    X;
type('ASN1_OPEN_TYPE') ->
    'ASN1_OPEN_TYPE';
type({fixedtypevaluefield,_Name,Type}) when record(Type,type) ->
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
		    'OBJECT IDENTIFIER',
		    'ANY',
		    'NULL',
		    'BIT STRING' ,
		    'OCTET STRING' ,
		    'ObjectDescriptor',
		    'NumericString',
		    'TeletexString',
		    'VideotexString',
		    'UTCTime',
		    'GeneralizedTime',
		    'GraphicString',
		    'VisibleString',
		    'GeneralString',
		    'PrintableString',
		    'IA5String',
		    'UniversalString',
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
       T when tuple(T),element(1,T)==fixedtypevaluefield ->
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

get_typefromobject(Type) when record(Type,type) ->
    case Type#type.def of
	{{objectclass,_,_},TypeFrObj} when list(TypeFrObj) ->
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

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} ->
	    V;
	{value,Cnstr} ->
	    Cnstr
    end.
