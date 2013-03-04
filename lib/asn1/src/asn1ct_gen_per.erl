%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
-module(asn1ct_gen_per).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").
%-compile(export_all).

-export([gen_dec_imm/2]).
-export([gen_dec_prim/3,gen_encode_prim/4]).
-export([gen_obj_code/3,gen_objectset_code/2]).
-export([gen_decode/2, gen_decode/3]).
-export([gen_encode/2, gen_encode/3]).
-export([extaddgroup2sequence/1]).

-import(asn1ct_gen, [emit/1,demit/1]).
-import(asn1ct_func, [call/3]).


%% Generate ENCODING ******************************
%%****************************************x


gen_encode(Erules,Type) when is_record(Type,typedef) ->
    gen_encode_user(Erules,Type).
%%    case Type#typedef.typespec of
%%	Def when is_record(Def,type) ->	    
%%	    gen_encode_user(Erules,Type);
%%	Def when is_tuple(Def),(element(1,Def) == 'Object') ->
%%	    gen_encode_object(Erules,Type);
%%	Other ->
%%	    exit({error,{asn1,{unknown,Other}}})
%%    end.

gen_encode(Erules,Typename,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTypename = [Cname|Typename],
    gen_encode(Erules,NewTypename,Type);

gen_encode(Erules,Typename,Type) when is_record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    ObjFun =
	case lists:keysearch(objfun,1,Type#type.tablecinf) of
	    {value,{_,_Name}} ->
%%		lists:concat([", ObjFun",Name]);
		", ObjFun";
	    false ->
		""
	end,
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val",ObjFun,
		  ") ->",nl}),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.


gen_encode_user(Erules,D) when is_record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val) ->",nl}),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_encode_prim(Erules,Def,"false"),
	    emit({".",nl});
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"false"),
	    emit({".",nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
	    emit({"'enc_",Etype,"'(Val).",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'enc_",Etype,"'(Val).",nl,nl});
	{notype,_} ->
	    emit({"'enc_",InnerType,"'(Val).",nl,nl})
    end.


gen_encode_prim(Erules,D,DoTag) ->
    Value = asn1ct_gen:mk_var(asn1ct_name:curr(val)),
    gen_encode_prim(Erules,D,DoTag,Value).

gen_encode_prim(Erules, #type{def={'ENUMERATED',{N1,N2}}}, _, Value) ->
    NewList = [{0,X} || {X,_} <- N1] ++ ['EXT_MARK'] ++
	[{1,X} || {X,_} <- N2],
    NewC = {0,length(N1)-1},
    emit(["case ",Value," of",nl]),
    emit_enc_enumerated_cases(Erules, NewC, NewList, 0);
gen_encode_prim(Erules, #type{def={'ENUMERATED',NNL}}, _, Value) ->
    NewList = [X || {X,_} <- NNL],
    NewC = {0,length(NewList)-1},
    emit(["case ",Value," of",nl]),
    emit_enc_enumerated_cases(Erules, NewC, NewList, 0);
gen_encode_prim(per=Erules, D, DoTag, Value) ->
    asn1ct_gen_per_rt2ct:gen_encode_prim(Erules, D, DoTag, Value);
gen_encode_prim(Erules,D,DoTag,Value) when is_record(D,type) ->
    Constraint = D#type.constraint,
    SizeConstr = asn1ct_imm:effective_constraint(bitstring, Constraint),
    Pa = case lists:keyfind('PermittedAlphabet', 1, Constraint) of
	     false -> no;
	     {_,Pa0} -> Pa0
	 end,
    asn1ct_name:new(enumval),
    case D#type.def of
	'INTEGER' ->
	    Args = [{asis,asn1ct_imm:effective_constraint(integer,Constraint)},
		    Value],
	    call(Erules, encode_integer, Args);
	{'INTEGER',NamedNumberList} ->
	    Args = [{asis,asn1ct_imm:effective_constraint(integer,Constraint)},
		    Value,{asis,NamedNumberList}],
	    call(Erules, encode_integer, Args);
	'REAL' ->
	    emit_enc_real(Erules, Value);

	{'BIT STRING',NamedNumberList} ->
	    call(Erules, encode_bit_string,
		 [{asis,SizeConstr},Value,
		  {asis,NamedNumberList}]);
	'NULL' ->
	    emit("[]");
	'OBJECT IDENTIFIER' ->
	    call(Erules, encode_object_identifier, [Value]);
	'RELATIVE-OID' ->
	    call(Erules, encode_relative_oid, [Value]);
	'ObjectDescriptor' ->
	    call(Erules, encode_ObjectDescriptor,
		 [{asis,Constraint},Value]);
	'BOOLEAN' ->
	    call(Erules, encode_boolean, [Value]);
	'OCTET STRING' ->
	    case SizeConstr of
		0 ->
		    emit("[]");
		no ->
		    call(Erules, encode_octet_string, [Value]);
		C ->
		    call(Erules, encode_octet_string, [{asis,C},Value])
	    end;
	'NumericString' ->
	    call(Erules, encode_NumericString, [{asis,SizeConstr},
						{asis,Pa},Value]);
	TString when TString == 'TeletexString';
		     TString == 'T61String' ->
	    call(Erules, encode_TeletexString, [{asis,Constraint},Value]);
	'VideotexString' ->
	    call(Erules, encode_VideotexString, [{asis,Constraint},Value]);
	'UTCTime' ->
	    call(Erules, encode_VisibleString, [{asis,SizeConstr},
						{asis,Pa},Value]);
	'GeneralizedTime' ->
	    call(Erules, encode_VisibleString, [{asis,SizeConstr},
						{asis,Pa},Value]);
	'GraphicString' ->
	    call(Erules, encode_GraphicString, [{asis,Constraint},Value]);
	'VisibleString' ->
	    call(Erules, encode_VisibleString, [{asis,SizeConstr},
						{asis,Pa},Value]);
	'GeneralString' ->
	    call(Erules, encode_GeneralString, [{asis,Constraint},Value]);
	'PrintableString' ->
	    call(Erules, encode_PrintableString, [{asis,SizeConstr},
						  {asis,Pa},Value]);
	'IA5String' ->
	    call(Erules, encode_IA5String, [{asis,SizeConstr},
					    {asis,Pa},Value]);
	'BMPString' ->
	    call(Erules, encode_BMPString, [{asis,SizeConstr},
					    {asis,Pa},Value]);
	'UniversalString' ->
	    call(Erules, encode_UniversalString, [{asis,SizeConstr},
						  {asis,Pa},Value]);
	'UTF8String' ->
	    call(Erules, encode_UTF8String, [Value]);
	'ASN1_OPEN_TYPE' ->
	    NewValue = case Constraint of
			   [#'Externaltypereference'{type=Tname}] ->
			       asn1ct_func:need({Erules,complete,1}),
			       io_lib:format(
				 "complete(enc_~s(~s))",[Tname,Value]);
			   [#type{def=#'Externaltypereference'{type=Tname}}] ->
			       asn1ct_func:need({Erules,complete,1}),
			       io_lib:format(
				 "complete(enc_~s(~s))",
				 [Tname,Value]);
			 _ -> Value
		     end,
	    call(Erules, encode_open_type, [NewValue]);
	#'ObjectClassFieldType'{} ->
	    case asn1ct_gen:get_inner(D#type.def) of
		{fixedtypevaluefield,_,InnerType} -> 
		    gen_encode_prim(Erules,InnerType,DoTag,Value);
		T -> %% 'ASN1_OPEN_TYPE'
		    gen_encode_prim(Erules,D#type{def=T},DoTag,Value)
	    end;
	XX ->
	    exit({asn1_error,nyi,XX})
    end.

emit_enc_real(Erules, Real) ->
    asn1ct_name:new(tmpval),
    asn1ct_name:new(tmplen),
    emit(["begin",nl,
	  "{",{curr,tmpval},com,{curr,tmplen},"} = ",
	  {call,real_common,encode_real,[Real]},com,nl,
	  "[",{call,Erules,encode_length,[{curr,tmplen}]},",",
	  {curr,tmpval},"]",nl,
	  "end"]).

emit_enc_enumerated_cases(Erules, C, ['EXT_MARK'|T], _Count) ->
    %% Reset enumeration counter.
    emit_enc_enumerated_cases(Erules, C, T, 0);
emit_enc_enumerated_cases(Erules, C, [H|T], Count) ->
    emit_enc_enumerated_case(Erules, C, H, Count),
    emit([";",nl]),
    emit_enc_enumerated_cases(Erules, C, T, Count+1);
emit_enc_enumerated_cases(_Erules, _, [], _Count) ->
    emit(["EnumVal -> "
	  "exit({error,{asn1,{enumerated_not_in_range, EnumVal}}})",nl,
	  "end"]).
    
emit_enc_enumerated_case(Erules, C, {0,EnumName}, Count) ->
    %% ENUMERATED with extensionmark; the value lies within then extension root
    Enc = enc_ext_and_val(Erules, 0, encode_constrained_number, [C,Count]),
    emit(["'",EnumName,"' -> ",{asis,Enc}]);
emit_enc_enumerated_case(Erules, _C, {1,EnumName}, Count) ->
    %% ENUMERATED with extensionmark; the value is higher than extension root
    Enc = enc_ext_and_val(Erules, 1, encode_small_number, [Count]),
    emit(["'",EnumName,"' -> ",{asis,Enc}]);
emit_enc_enumerated_case(Erules, C, EnumName, Count) ->
    %% ENUMERATED without extension
    EvalMod = eval_module(Erules),
    emit(["'",EnumName,"' -> ",
	  {asis,EvalMod:encode_constrained_number(C, Count)}]).

enc_ext_and_val(per, E, F, Args) ->
    [E|apply(asn1ct_eval_per, F, Args)];
enc_ext_and_val(uper, E, F, Args) ->
    <<E:1,(apply(asn1ct_eval_uper, F, Args))/bitstring>>.


%% Object code generating for encoding and decoding
%% ------------------------------------------------

gen_obj_code(Erules,_Module,Obj) when is_record(Obj,typedef) ->
    ObjName = Obj#typedef.name,
    Def = Obj#typedef.typespec,
    #'Externaltypereference'{module=Mod,type=ClassName} = 
	Def#'Object'.classname,
    Class = asn1_db:dbget(Mod,ClassName),
    {object,_,Fields} = Def#'Object'.def,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjName}),
    emit({nl,"%%================================",nl}),
    EncConstructed =
	gen_encode_objectfields(Erules, ClassName,get_class_fields(Class),
				ObjName,Fields,[]),
    emit(nl),
    gen_encode_constr_type(Erules,EncConstructed),
    emit(nl),
    DecConstructed =
	gen_decode_objectfields(Erules, ClassName, get_class_fields(Class),
				ObjName, Fields, []),
    emit(nl),
    gen_decode_constr_type(Erules,DecConstructed),
    emit(nl);
gen_obj_code(_,_,Obj) when is_record(Obj,pobjectdef) ->
    ok.


gen_encode_objectfields(Erule, ClassName,
			[{typefield,Name,OptOrMand}|Rest],
			ObjName, ObjectFields, ConstrAcc) ->
    EmitFuncClause = 
	fun(V) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ",",V,",_RestPrimFieldName) ->",nl])
	end,
%     emit(["'enc_",ObjName,"'(",{asis,Name},
% 	  ", Val, _RestPrimFieldName) ->",nl]),
    MaybeConstr =
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("Val"),
		case Erule of
		    uper ->
			emit("   Val");
		    per ->
			emit(["   if",nl,
			      "     is_list(Val) ->",nl,
			      "       NewVal = list_to_binary(Val),",nl,
			      "       [20,byte_size(NewVal),NewVal];",nl,
			      "     is_binary(Val) ->",nl,
			      "       [20,byte_size(Val),Val]",nl,
			      "   end"])
		end,
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Val"),
		gen_encode_default_call(Erule, ClassName, Name, DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Val"),
		gen_encode_field_call(Erule, ObjName, Name, TypeSpec)
	end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_encode_objectfields(Erule,ClassName,Rest,ObjName,ObjectFields,
			    MaybeConstr++ConstrAcc);
gen_encode_objectfields(Erule,ClassName,[{objectfield,Name,_,_,OptOrMand}|Rest],
			ObjName,ObjectFields,ConstrAcc) ->
    CurrentMod = get(currmod),
    EmitFuncClause =
	fun(Attrs) ->
		emit(["'enc_",ObjName,"'(",{asis,Name},
		      ",",Attrs,") ->",nl])
	end,
%     emit(["'enc_",ObjName,"'(",{asis,Name},
% 	  ", Val,[H|T]) ->",nl]),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_"),
	    emit(["  exit({error,{'use of missing field in object', ",{asis,Name},
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,#'Externalvaluereference'{module=CurrentMod,
					 value=TypeName}},_} ->
	    EmitFuncClause(" Val, [H|T]"),
	    emit({indent(3),"'enc_",TypeName,"'(H, Val, T)"});
	{{Name,#'Externalvaluereference'{module=M,value=TypeName}},_} ->
	    EmitFuncClause(" Val, [H|T]"),
	    emit({indent(3),"'",M,"':'enc_",TypeName,"'(H, Val, T)"});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause("Val,[H|T]"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'enc_",TypeName,
			  "'(H, Val, T)"});
		TypeName ->
		    emit({indent(3),"'enc_",TypeName,"'(H, Val, T)"})
	    end
    end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_encode_objectfields(Erule,ClassName,Rest,ObjName,ObjectFields,ConstrAcc);
gen_encode_objectfields(Erule,ClassName,[_C|Cs],O,OF,Acc) ->
    gen_encode_objectfields(Erule,ClassName,Cs,O,OF,Acc);
gen_encode_objectfields(_, _,[],_,_,Acc) ->
    Acc.


gen_encode_constr_type(Erules,[TypeDef|Rest]) when is_record(TypeDef,typedef) ->
    case is_already_generated(enc,TypeDef#typedef.name) of
	true -> ok;
	_ ->
%%	    FuncName = list_to_atom(lists:concat(["enc_",TypeDef#typedef.name])),
	    FuncName = asn1ct_gen:list2rname(TypeDef#typedef.name ++ [enc]),
	    emit(["'",FuncName,"'(Val) ->",nl]),
	    Def = TypeDef#typedef.typespec,
	    InnerType = asn1ct_gen:get_inner(Def#type.def),
	    asn1ct_gen:gen_encode_constructed(Erules,TypeDef#typedef.name,
					      InnerType,Def),
	    gen_encode_constr_type(Erules,Rest)
    end;
gen_encode_constr_type(_,[]) ->
    ok.

gen_encode_field_call(_Erules, _ObjName, _FieldName,
		      #'Externaltypereference'{module=M,type=T}) ->
    CurrentMod = get(currmod),
    if
	M == CurrentMod ->
	    emit({"   'enc_",T,"'(Val)"}),
	    [];
	true ->
	    emit({"   '",M,"':'enc_",T,"'(Val)"}),
	    []
    end;
gen_encode_field_call(Erules, ObjName, FieldName, Type) ->
    Def = Type#typedef.typespec,
    case Type#typedef.name of
	{primitive,bif} -> 
	    gen_encode_prim(Erules, Def, "false", "Val"),
	    [];
	{constructed,bif} ->
	    emit({"   'enc_",ObjName,'_',FieldName,
		  "'(Val)"}),
%%	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	    [Type#typedef{name=[FieldName,ObjName]}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'enc_",TypeName,
		  "'(Val)"}),
	    [];
	TypeName ->
	    emit({"   'enc_",TypeName,"'(Val)"}),
	    []
    end.

gen_encode_default_call(Erules, ClassName, FieldName, Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
    	{constructed,bif} ->
%%	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	    emit(["   'enc_",ClassName,'_',FieldName,"'(Val)"]),
%%	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
	    [#typedef{name=[FieldName,ClassName],
		      typespec=Type}];
	{primitive,bif} ->
	    gen_encode_prim(Erules, Type, "false", "Val"),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val)",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val)",nl]),
	    []
    end.
    

gen_decode_objectfields(Erules, ClassName,
			[{typefield,Name,OptOrMand}|Rest],
			ObjName, ObjectFields, ConstrAcc) ->
    EmitFuncClause = 
	fun(Bytes) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},",",Bytes,
		      ",_,_RestPrimFieldName) ->",nl])
	end,
    MaybeConstr=
	case {get_object_field(Name,ObjectFields),OptOrMand} of
	    {false,'MANDATORY'} -> %% this case is illegal
		exit({error,{asn1,{"missing mandatory field in object",
				   ObjName}}});
	    {false,'OPTIONAL'} ->
		EmitFuncClause("Bytes"),
		emit(["   {Bytes,[]}"]),
		[];
	    {false,{'DEFAULT',DefaultType}} ->
		EmitFuncClause("Bytes"),
		gen_decode_default_call(Erules, ClassName, Name, "Bytes",
					DefaultType);
	    {{Name,TypeSpec},_} ->
		%% A specified field owerwrites any 'DEFAULT' or
		%% 'OPTIONAL' field in the class
		EmitFuncClause("Bytes"),
		gen_decode_field_call(Erules, ObjName, Name, "Bytes", TypeSpec)
	end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_decode_objectfields(Erules, ClassName, Rest, ObjName,
			    ObjectFields, MaybeConstr++ConstrAcc);
gen_decode_objectfields(Erules, ClassName,
			[{objectfield,Name,_,_,OptOrMand}|Rest],
			ObjName, ObjectFields, ConstrAcc) ->
    CurrentMod = get(currmod),
    EmitFuncClause =
	fun(Attrs) ->
		emit(["'dec_",ObjName,"'(",{asis,Name},
		      ",",Attrs,") ->",nl])
	end,
%     emit(["'dec_",ObjName,"'(",{asis,Name},
% 	  ", Bytes,_,[H|T]) ->",nl]),
    case {get_object_field(Name,ObjectFields),OptOrMand} of
	{false,'MANDATORY'} ->
	    exit({error,{asn1,{"missing mandatory field in object",
			       ObjName}}});
	{false,'OPTIONAL'} ->
	    EmitFuncClause("_,_,_"),
	    emit(["  exit({error,{'illegal use of missing field in object', ",{asis,Name},
		  "}})"]);
	{false,{'DEFAULT',_DefaultObject}} ->
	    exit({error,{asn1,{"not implemented yet",Name}}});
	{{Name,#'Externalvaluereference'{module=CurrentMod,
					 value=TypeName}},_} ->
	    EmitFuncClause("Bytes,_,[H|T]"),
	    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, telltype, T)"});
	{{Name,#'Externalvaluereference'{module=M,value=TypeName}},_} ->
	    EmitFuncClause("Bytes,_,[H|T]"),
	    emit({indent(3),"'",M,"':'dec_",TypeName,
		  "'(H, Bytes, telltype, T)"});
	{{Name,TypeSpec},_} ->
	    EmitFuncClause("Bytes,_,[H|T]"),
	    case TypeSpec#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
			  "'(H, Bytes, telltype, T)"});
		TypeName ->
		    emit({indent(3),"'dec_",TypeName,"'(H, Bytes, telltype, T)"})
	    end
    end,
    case more_genfields(Rest) of
	true ->
	    emit([";",nl]);
	false ->
	    emit([".",nl])
    end,
    gen_decode_objectfields(Erules, ClassName, Rest, ObjName,
			    ObjectFields, ConstrAcc);
gen_decode_objectfields(Erules, CN, [_C|Cs], O, OF, CAcc) ->
    gen_decode_objectfields(Erules, CN, Cs, O, OF, CAcc);
gen_decode_objectfields(_, _, [], _, _, CAcc) ->
    CAcc.



gen_decode_field_call(_Erules, _ObjName, _FieldName, Bytes,
		      #'Externaltypereference'{module=M,type=T}) ->
    CurrentMod = get(currmod),
    if
	M == CurrentMod ->
	    emit(["   'dec_",T,"'(",Bytes,", telltype)"]),
	    [];
	true ->
	    emit(["   '",M,"':'dec_",T,"'(",Bytes,", telltype)"]),
	    []
    end;
gen_decode_field_call(Erules, ObjName, FieldName, Bytes, Type) ->
    Def = Type#typedef.typespec,
    case Type#typedef.name of
	{primitive,bif} -> 
	    gen_dec_prim(Erules, Def, Bytes),
	    [];
	{constructed,bif} ->
	    emit({"   'dec_",ObjName,'_',FieldName,
		  "'(",Bytes,",telltype)"}),
%%	    [Type#typedef{name=list_to_atom(lists:concat([ObjName,'_',FieldName]))}];
	    [Type#typedef{name=[FieldName,ObjName]}];
	{ExtMod,TypeName} ->
	    emit({"   '",ExtMod,"':'dec_",TypeName,
		  "'(",Bytes,", telltype)"}),
	    [];
	TypeName ->
	    emit({"   'dec_",TypeName,"'(",Bytes,", telltype)"}),
	    []
    end.

gen_decode_default_call(Erules, ClassName, FieldName, Bytes, Type) ->
    CurrentMod = get(currmod),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
    	{constructed,bif} ->
	    emit(["   'dec_",ClassName,'_',FieldName,"'(",Bytes,", telltype)"]),
%%	    [#typedef{name=list_to_atom(lists:concat([ClassName,'_',FieldName])),
	    [#typedef{name=[FieldName,ClassName],
		      typespec=Type}];
	{primitive,bif} ->
	    gen_dec_prim(Erules, Type, Bytes),
	    [];
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'dec_",Etype,"'(",Bytes,", telltype)",nl]),
	    [];
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'dec_",Etype,"'(",Bytes,", telltype)",nl]),
	    []
    end.


gen_decode_constr_type(Erules,[TypeDef|Rest]) when is_record(TypeDef,typedef) ->
    case is_already_generated(dec,TypeDef#typedef.name) of
	true -> ok;
	_ ->
	    gen_decode(Erules,TypeDef#typedef{name=asn1ct_gen:list2rname(TypeDef#typedef.name)})
    end,
    gen_decode_constr_type(Erules,Rest);
gen_decode_constr_type(_,[]) ->
    ok.


more_genfields([]) ->
    false;
more_genfields([Field|Fields]) ->
    case element(1,Field) of
	typefield ->
	    true;
	objectfield ->
	    true;
	_ ->
	    more_genfields(Fields)
    end.

%% Object Set code generating for encoding and decoding
%% ----------------------------------------------------
gen_objectset_code(Erules,ObjSet) ->
    ObjSetName = ObjSet#typedef.name,
    Def = ObjSet#typedef.typespec,
%%    {ClassName,ClassDef} = Def#'ObjectSet'.class,
    #'Externaltypereference'{module=ClassModule,
			     type=ClassName} = Def#'ObjectSet'.class,
    ClassDef = asn1_db:dbget(ClassModule,ClassName),
    UniqueFName = Def#'ObjectSet'.uniquefname,
    Set = Def#'ObjectSet'.set,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjSetName}),
    emit({nl,"%%================================",nl}),
    case ClassName of
	{_Module,ExtClassName} ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,
			    ExtClassName,ClassDef);
	_ ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,
			    ClassName,ClassDef)
    end,
    emit(nl).

gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassDef)->
    ClassFields = (ClassDef#classdef.typespec)#objectclass.fields,
    InternalFuncs=
	gen_objset_enc(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassFields,1,[]),
    gen_objset_dec(Erules, ObjSetName,UniqueFName,Set,ClassName,ClassFields,1),
    gen_internal_funcs(Erules,InternalFuncs).

%% gen_objset_enc iterates over the objects of the object set
gen_objset_enc(_,_,{unique,undefined},_,_,_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    [];
gen_objset_enc(Erule, ObjSetName, UniqueName, [{ObjName,Val,Fields}|T],
	       ClName, ClFields, NthObj, Acc)->
    emit(["'getenc_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},
	  ") ->",nl]),
    CurrMod = get(currmod),
    {InternalFunc,NewNthObj}=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_enc_funs(Erule, Fields, ClFields,
				     ObjSetName, NthObj);
	    {CurrMod,Name} ->
		emit({"    fun 'enc_",Name,"'/3"}),
		{[],0};
	    {ModName,Name} ->
		emit_ext_encfun(ModName,Name),
		{[],0};
	    _Other ->
		emit({"    fun 'enc_",ObjName,"'/3"}),
		{[],0}
	end,
    emit({";",nl}),
    gen_objset_enc(Erule, ObjSetName, UniqueName, T, ClName, ClFields,
		   NewNthObj, InternalFunc ++ Acc);
gen_objset_enc(uper, ObjSetName, _UniqueName, ['EXTENSIONMARK'],
	       _ClName, _ClFields, _NthObj, Acc) ->
    emit({"'getenc_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(_, Val, _) ->",nl}),
    emit([indent(6),"Val",nl,
	  indent(3),"end.",nl,nl]),
    Acc;
gen_objset_enc(per, ObjSetName, _UniqueName, ['EXTENSIONMARK'],
	       _ClName, _ClFields, _NthObj, Acc) ->
    emit(["'getenc_",ObjSetName,"'(_, _) ->",nl,
	  indent(3),"fun(_, Val, _) ->",nl,
	  indent(6),"BinVal = if",nl,
	  indent(9),"is_list(Val) -> list_to_binary(Val);",nl,
	  indent(9),"true -> Val",nl,
	  indent(6),"end,",nl,
	  indent(6),"Size = byte_size(BinVal),",nl,
	  indent(6),"if",nl,
	  indent(9),"Size < 256 ->",nl,
	  indent(12),"[20,Size,BinVal];",nl,
	  indent(9),"true ->",nl,
	  indent(12),"[21,<<Size:16>>,Val]",nl,
	  indent(6),"end",nl,
	  indent(3),"end.",nl,nl]),
    Acc;
gen_objset_enc(_, ObjSetName, UniqueName, [], _, _, _, Acc) ->
    emit_default_getenc(ObjSetName, UniqueName),
    emit([".",nl,nl]),
    Acc.

emit_ext_encfun(ModuleName,Name) ->
    emit([indent(4),"fun(T,V,O) -> '",ModuleName,"':'enc_",
	  Name,"'(T,V,O) end"]).

emit_default_getenc(ObjSetName,UniqueName) ->
    emit(["'getenc_",ObjSetName,"'(",{asis,UniqueName},", ErrV) ->",nl]),
    emit([indent(4),"fun(C,V,_) -> exit({'Type not compatible with table constraint',{component,C},{value,V},{unique_name_and_value,",{asis,UniqueName},",ErrV}}) end"]).


%% gen_inlined_enc_funs for each object iterates over all fields of a
%% class, and for each typefield it checks if the object has that
%% field and emits the proper code.
gen_inlined_enc_funs(Erule, Fields, [{typefield,_,_}|_]=T,
		     ObjSetName, NthObj) ->
    emit([indent(3),"fun(Type, Val, _) ->",nl,
	  indent(6),"case Type of",nl]),
    gen_inlined_enc_funs1(Erule, Fields, T, ObjSetName, [], NthObj, []);
gen_inlined_enc_funs(Erule,Fields,[_H|Rest],ObjSetName,NthObj) ->
    gen_inlined_enc_funs(Erule,Fields,Rest,ObjSetName,NthObj);
gen_inlined_enc_funs(_,_,[],_,NthObj) ->
    {[],NthObj}.

gen_inlined_enc_funs1(Erule, Fields, [{typefield,Name,_}|Rest], ObjSetName,
		      Sep0, NthObj, Acc0) ->
    emit(Sep0),
    Sep = [";",nl],
    CurrentMod = get(currmod),
    InternalDefFunName = asn1ct_gen:list2name([NthObj,Name,ObjSetName]),
    {Acc,NAdd} =
	case lists:keyfind(Name, 1, Fields) of
	    {_,#type{}=Type} ->
		{Ret,N} = emit_inner_of_fun(Erule, Type, InternalDefFunName),
		{Ret++Acc0,N};
	    {_,#typedef{}=Type} ->
		emit([indent(9),{asis,Name}," ->",nl]),
		{Ret,N} = emit_inner_of_fun(Erule, Type, InternalDefFunName),
		{Ret++Acc0,N};
	    {_,#'Externaltypereference'{module=CurrentMod,type=T}} ->
		emit([indent(9),{asis,Name}," ->",nl,
		      indent(12),"'enc_",T,"'(Val)"]),
		{Acc0,0};
	    {_,#'Externaltypereference'{module=M,type=T}} ->
		emit([indent(9),{asis,Name}," ->",nl,
		      indent(12),"'",M,"'",":'enc_",T,"'(Val)"]),
		{Acc0,0};
	    false when Erule =:= uper ->
		emit([indent(9),{asis,Name}," ->",nl,
		      indent(12),"Val",nl]),
		{Acc0,0};
	    false when Erule =:= per ->
		emit([indent(9),{asis,Name}," ->",nl,
		      indent(12),"Size = case Val of",nl,
		      indent(15),"B when is_binary(B) -> size(B);",nl,
		      indent(15),"_ -> length(Val)",nl,
		      indent(12),"end,",nl,
		      indent(12),"if",nl,
		      indent(15),"Size < 256 -> [20,Size,Val];",nl,
		      indent(15),"true -> [21,<<Size:16>>,Val]",nl,
		      indent(12),"end"]),
		{Acc0,0}
	end,
    gen_inlined_enc_funs1(Erule, Fields, Rest, ObjSetName, Sep,
			  NthObj+NAdd, Acc);
gen_inlined_enc_funs1(Erule, Fields, [_|T], ObjSetName, Sep, NthObj, Acc)->
    gen_inlined_enc_funs1(Erule, Fields, T, ObjSetName, Sep, NthObj, Acc);
gen_inlined_enc_funs1(_, _, [], _, _, NthObj, Acc) ->
    emit([nl,indent(6),"end",nl,
	  indent(3),"end"]),
    {Acc,NthObj}.

emit_inner_of_fun(Erule, #typedef{name={ExtMod,Name},typespec=Type}=TDef,
		  InternalDefFunName) ->
    case {ExtMod,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_encode_prim(Erule, Type, dotag, "Val"),
	    {[],0};
	{constructed,bif} ->
	    emit([indent(12),"'enc_",
		  InternalDefFunName,"'(Val)"]),
	    {[TDef#typedef{name=InternalDefFunName}],1};
	_ ->
	    emit({indent(12),"'",ExtMod,"':'enc_",Name,"'(Val)"}),
	    {[],0}
    end;
emit_inner_of_fun(_Erule, #typedef{name=Name}, _) ->
    emit({indent(12),"'enc_",Name,"'(Val)"}),
    {[],0};
emit_inner_of_fun(Erule, #type{}=Type, _) ->
    CurrMod = get(currmod),
    case Type#type.def of
	Def when is_atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_encode_prim(Erule, Type, dotag, "Val");
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,"'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'enc_",
		  T,"'(Val)"})
    end,
    {[],0}.

indent(N) ->
    lists:duplicate(N,32). % 32 = space


gen_objset_dec(_, _, {unique,undefined}, _, _, _, _) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_dec(Erule, ObjSName, UniqueName, [{ObjName,Val,Fields}|T], ClName,
	       ClFields, NthObj)->
    emit({"'getdec_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},
	  ") ->",nl}),
    CurrMod = get(currmod),
    NewNthObj=
	case ObjName of
	    {no_mod,no_name} ->
		gen_inlined_dec_funs(Erule, Fields, ClFields,
				     ObjSName, NthObj);
	    {CurrMod,Name} ->
		emit(["    fun 'dec_",Name,"'/4"]),
		NthObj;
	    {ModName,Name} ->
		emit_ext_decfun(ModName,Name),
		NthObj;
	    _Other ->
		emit({"    fun 'dec_",ObjName,"'/4"}),
		NthObj
    end,
    emit({";",nl}),
    gen_objset_dec(Erule, ObjSName, UniqueName, T, ClName, ClFields, NewNthObj);
gen_objset_dec(_Erule, ObjSetName, _UniqueName, ['EXTENSIONMARK'],
	       _ClName, _ClFields, _NthObj) ->
    emit({"'getdec_",ObjSetName,"'(_, _) ->",nl}),
    emit({indent(3),"fun(Attr1, Bytes, _,_) ->",nl}),
    emit({indent(6),"{Bytes,Attr1}",nl}),
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_dec(_Erule, ObjSetName, UniqueName, [], _, _, _) ->
    emit_default_getdec(ObjSetName, UniqueName),
    emit([".",nl,nl]),
    ok.

emit_ext_decfun(ModuleName,Name) ->
    emit([indent(3),"fun(T,V,O1,O2) -> '",ModuleName,"':'dec_",
	  Name,"'(T,V,O1,O2) end"]).

emit_default_getdec(ObjSetName,UniqueName) ->
    emit(["'getdec_",ObjSetName,"'(",{asis,UniqueName},", ErrV) ->",nl]),
    emit([indent(2), "fun(C,V,_,_) -> exit({{component,C},{value,V},{unique_name_and_value,",{asis,UniqueName},",ErrV}}) end"]).


gen_inlined_dec_funs(Erule, Fields, List, ObjSetName, NthObj0) ->
    emit([indent(3),"fun(Type, Val, _, _) ->",nl,
	  indent(6),"case Type of",nl]),
    NthObj = gen_inlined_dec_funs1(Erule, Fields, List,
				   ObjSetName, "", NthObj0),
    emit([nl,indent(6),"end",nl,
	  indent(3),"end"]),
    NthObj.

gen_inlined_dec_funs1(Erule, Fields, [{typefield,Name,_}|Rest],
		      ObjSetName, Sep0, NthObj) ->
    CurrentMod = get(currmod),
    InternalDefFunName = [NthObj,Name,ObjSetName],
    emit(Sep0),
    Sep = [";",nl],
    N = case lists:keyfind(Name, 1, Fields) of
	    {_,#type{}=Type} ->
		emit_inner_of_decfun(Erule, Type, InternalDefFunName);
	    {_,#typedef{}=Type} ->
		emit([indent(9),{asis,Name}," ->",nl]),
		emit_inner_of_decfun(Erule, Type, InternalDefFunName);
	    {_,#'Externaltypereference'{module=CurrentMod,type=T}} ->
		emit([indent(9),{asis,Name}," ->",nl,
		      indent(12),"'dec_",T,"'(Val,telltype)"]),
		0;
	    {_,#'Externaltypereference'{module=M,type=T}} ->
		emit([indent(9),{asis,Name}," ->",nl,
		      indent(12),"'",M,"':'dec_",T,"'(Val,telltype)"]),
		0;
	    false ->
		emit([indent(9),{asis,Name}," -> {Val,Type}"]),
		0
	end,
    gen_inlined_dec_funs1(Erule, Fields, Rest, ObjSetName, Sep, NthObj+N);
gen_inlined_dec_funs1(Erule, Fields, [_|Rest], ObjSetName, Sep, NthObj) ->
    gen_inlined_dec_funs1(Erule, Fields, Rest, ObjSetName, Sep, NthObj);
gen_inlined_dec_funs1(_, _, [], _, _, NthObj) -> NthObj.

emit_inner_of_decfun(Erule, #typedef{name={ExtName,Name},typespec=Type},
		     InternalDefFunName) ->
    case {ExtName,Name} of
	{primitive,bif} ->
	    emit(indent(12)),
	    gen_dec_prim(Erule, Type, "Val"),
	    0;
	{constructed,bif} ->
	    emit({indent(12),"'dec_",
		  asn1ct_gen:list2name(InternalDefFunName),"'(Val)"}),
	    1;
	_ ->
	    emit({indent(12),"'",ExtName,"':'dec_",Name,"'(Val, telltype)"}),
	    0
    end;
emit_inner_of_decfun(_Erule, #typedef{name=Name}, _) ->
    emit({indent(12),"'dec_",Name,"'(Val, telltype)"}),
    0;
emit_inner_of_decfun(Erule, #type{}=Type, _) ->
    CurrMod = get(currmod),
    case Type#type.def of
	Def when is_atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_dec_prim(Erule, Type, "Val");
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'dec_",
		  T,"'(Val)"})
    end,
    0.


gen_internal_funcs(_,[]) ->
    ok;
gen_internal_funcs(Erules,[TypeDef|Rest]) ->
    gen_encode_user(Erules,TypeDef),
    emit([nl,nl,"'dec_",TypeDef#typedef.name,"'(Bytes) ->",nl]),
    gen_decode_user(Erules,TypeDef),
    gen_internal_funcs(Erules,Rest).



%% DECODING *****************************
%%***************************************


gen_decode(Erules,Type) when is_record(Type,typedef) ->
    D = Type,
    emit({nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes,_) ->",nl}),
    dbdec(Type#typedef.name),
    gen_decode_user(Erules,D).

gen_decode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type}) ->
    NewTname = [Cname|Tname],
    gen_decode(Erules,NewTname,Type);

gen_decode(Erules,Typename,Type) when is_record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    ObjFun =
		case Type#type.tablecinf of
		    [{objfun,_}|_R] ->
			", ObjFun";
		    _ ->
			""
		end,
	    emit({nl,"'dec_",asn1ct_gen:list2name(Typename),
		  "'(Bytes,_",ObjFun,") ->",nl}),
	    dbdec(Typename),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.

dbdec(Type) when is_list(Type)->
    demit({"io:format(\"decoding: ",asn1ct_gen:list2name(Type),"~w~n\",[Bytes]),",nl});
dbdec(Type) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[Bytes]),",nl}).

gen_decode_user(Erules,D) when is_record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_dec_prim(Erules,Def,"Bytes"),
	    emit({".",nl,nl});
	'ASN1_OPEN_TYPE' ->
	    gen_dec_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"Bytes"),
	    emit({".",nl,nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
	    emit({"'dec_",Etype,"'(Bytes,telltype).",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'dec_",Etype,"'(Bytes,telltype).",nl,nl});
	Other ->
	    exit({error,{asn1,{unknown,Other}}})
    end.

gen_dec_imm(Erule, #type{def=Name,constraint=C}) ->
    Aligned = case Erule of
		  uper -> false;
		  per -> true
	      end,
    gen_dec_imm_1(Name, C, Aligned).

gen_dec_imm_1('ASN1_OPEN_TYPE', Constraint, Aligned) ->
    imm_decode_open_type(Constraint, Aligned);
gen_dec_imm_1({'BIT STRING',NNL}, Constr0, Aligned) ->
    Constr = asn1ct_imm:effective_constraint(bitstring, Constr0),
    Imm = asn1ct_imm:per_dec_raw_bitstring(Constr, Aligned),
    case NNL of
	[] ->
	    case asn1ct:get_bit_string_format() of
		compact ->
		    gen_dec_bit_string(decode_compact_bit_string,
				       Imm);
		legacy ->
		    gen_dec_bit_string(decode_legacy_bit_string,
				       Imm);
		bitstring ->
		    gen_dec_copy_bitstring(Imm)
	    end;
	[_|_] ->
	    D = fun(V, Buf) ->
			As = [V,{asis,NNL}],
			Call = {call,per_common,decode_named_bit_string,As},
			emit(["{",Call,com,Buf,"}"])
		end,
	    {call,D,Imm}
    end;
gen_dec_imm_1('NULL', _Constr, _Aligned) ->
    {value,'NULL'};
gen_dec_imm_1('BOOLEAN', _Constr, _Aligned) ->
    asn1ct_imm:per_dec_boolean();
gen_dec_imm_1({'ENUMERATED',{Base,Ext}}, _Constr, Aligned) ->
    asn1ct_imm:per_dec_enumerated(Base, Ext, Aligned);
gen_dec_imm_1({'ENUMERATED',NamedNumberList}, _Constr, Aligned) ->
    asn1ct_imm:per_dec_enumerated(NamedNumberList, Aligned);
gen_dec_imm_1('INTEGER', Constr, Aligned) ->
    asn1ct_imm:per_dec_integer(Constr, Aligned);
gen_dec_imm_1({'INTEGER',NamedNumberList}, Constraint, Aligned) ->
    asn1ct_imm:per_dec_named_integer(Constraint,
				     NamedNumberList,
				     Aligned);
gen_dec_imm_1('BMPString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('NumericString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('PrintableString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('VisibleString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('IA5String'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('UniversalString'=Type, Constraint, Aligned) ->
    gen_dec_k_m_string(Type, Constraint, Aligned);
gen_dec_imm_1('UTCTime', Constraint, Aligned) ->
    gen_dec_k_m_string('VisibleString', Constraint, Aligned);
gen_dec_imm_1('GeneralizedTime', Constraint, Aligned) ->
    gen_dec_k_m_string('VisibleString', Constraint, Aligned);
gen_dec_imm_1('OCTET STRING', Constraint, Aligned) ->
    SzConstr = asn1ct_imm:effective_constraint(bitstring, Constraint),
    Imm = asn1ct_imm:per_dec_octet_string(SzConstr, Aligned),
    {convert,binary_to_list,Imm};
gen_dec_imm_1('TeletexString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('T61String', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('VideotexString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('GraphicString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('GeneralString', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('ObjectDescriptor', _Constraint, Aligned) ->
    gen_dec_restricted_string(Aligned);
gen_dec_imm_1('OBJECT IDENTIFIER', _Constraint, Aligned) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_oid,[V]},com,
			Buf,"}"])
	  end,
    {call,Dec,gen_dec_restricted_string(Aligned)};
gen_dec_imm_1('RELATIVE-OID', _Constraint, Aligned) ->
    Dec = fun(V, Buf) ->
		  emit(["{",{call,per_common,decode_relative_oid,[V]},com,
			Buf,"}"])
	  end,
    {call,Dec,gen_dec_restricted_string(Aligned)};
gen_dec_imm_1('UTF8String', _Constraint, Aligned) ->
    asn1ct_imm:per_dec_restricted_string(Aligned);
gen_dec_imm_1('REAL', _Constraint, Aligned) ->
    asn1ct_imm:per_dec_real(Aligned);
gen_dec_imm_1(#'ObjectClassFieldType'{}=TypeName, _Constraint, Aligned) ->
    case asn1ct_gen:get_inner(TypeName) of
	{fixedtypevaluefield,_,#type{def=InnerType,constraint=C}} ->
	    gen_dec_imm_1(InnerType, C, Aligned);
	#type{def=T,constraint=C} ->
	    gen_dec_imm_1(T, C, Aligned)
    end.

gen_dec_bit_string(F, Imm) ->
    D = fun(V, Buf) ->
		emit(["{",{call,per_common,F,[V]},com,Buf,"}"])
	end,
    {call,D,Imm}.

gen_dec_copy_bitstring(Imm) ->
    D = fun(V, Buf) ->
		emit(["{list_to_bitstring([",V,"]),",Buf,"}"])
	end,
    {call,D,Imm}.

gen_dec_k_m_string(Type, Constraint, Aligned) ->
    asn1ct_imm:per_dec_k_m_string(Type, Constraint, Aligned).

gen_dec_restricted_string(Aligned) ->
    Imm = asn1ct_imm:per_dec_restricted_string(Aligned),
    {convert,binary_to_list,Imm}.

gen_dec_prim(Erule, Type, BytesVar) ->
    Imm = gen_dec_imm(Erule, Type),
    asn1ct_imm:dec_code_gen(Imm, BytesVar).

is_already_generated(Operation,Name) ->
    case get(class_default_type) of
	undefined ->
	    put(class_default_type,[{Operation,Name}]),
	    false;
	GeneratedList ->
	    case lists:member({Operation,Name},GeneratedList) of
		true ->
		    true;
		false ->
		    put(class_default_type,[{Operation,Name}|GeneratedList]),
		    false
	    end
    end.

get_class_fields(#classdef{typespec=ObjClass}) ->
    ObjClass#objectclass.fields;
get_class_fields(#objectclass{fields=Fields}) ->
    Fields;
get_class_fields(_) ->
    [].


get_object_field(Name,ObjectFields) ->
    case lists:keysearch(Name,1,ObjectFields) of
	{value,Field} -> Field;
	false -> false
    end.


%% For PER the ExtensionAdditionGroup notation has significance for the encoding and decoding
%% the components within the ExtensionAdditionGroup is treated in a similar way as if they
%% have been specified within a SEQUENCE, therefore we construct a fake sequence type here
%% so that we can generate code for it
extaddgroup2sequence(ExtList) ->
    extaddgroup2sequence(ExtList,0,[]).

extaddgroup2sequence([{'ExtensionAdditionGroup',Number0}|T],ExtNum,Acc) ->
    Number = case Number0 of undefined -> 1; _ -> Number0 end,
    {ExtGroupComps,['ExtensionAdditionGroupEnd'|T2]} =
     lists:splitwith(fun(Elem) -> is_record(Elem,'ComponentType') end,T),
    extaddgroup2sequence(T2,ExtNum+1,
			 [#'ComponentType'{
			     name=list_to_atom("ExtAddGroup"++
						   integer_to_list(ExtNum+1)),
			     typespec=#type{def=#'SEQUENCE'{
					      extaddgroup=Number,
					      components=ExtGroupComps}},
			     prop='OPTIONAL'}|Acc]);
extaddgroup2sequence([C|T],ExtNum,Acc) ->
    extaddgroup2sequence(T,ExtNum,[C|Acc]);
extaddgroup2sequence([],_,Acc) ->
    lists:reverse(Acc).

imm_decode_open_type([#'Externaltypereference'{type=Tname}], Aligned) ->
    imm_dec_open_type_1(Tname, Aligned);
imm_decode_open_type([#type{def=#'Externaltypereference'{type=Tname}}],
		     Aligned) ->
    imm_dec_open_type_1(Tname, Aligned);
imm_decode_open_type(_, Aligned) ->
    asn1ct_imm:per_dec_open_type(Aligned).

imm_dec_open_type_1(Type, Aligned) ->
    D = fun(OpenType, Buf) ->
		asn1ct_name:new(tmpval),
		emit(["begin",nl,
		      "{",{curr,tmpval},",_} = ",
		      "dec_",Type,"(",OpenType,", mandatory),",nl,
		      "{",{curr,tmpval},com,Buf,"}",nl,
		      "end"])
	end,
    {call,D,asn1ct_imm:per_dec_open_type(Aligned)}.

eval_module(per) -> asn1ct_eval_per;
eval_module(uper) -> asn1ct_eval_uper.
