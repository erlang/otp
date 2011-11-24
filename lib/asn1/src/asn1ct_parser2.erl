%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
-module(asn1ct_parser2).

-export([parse/1]).
-include("asn1_records.hrl").

%% parse all types in module
parse(Tokens) ->
    case catch parse_ModuleDefinition(Tokens) of
	{'EXIT',Reason} ->
	    {error,{{undefined,get(asn1_module),
		    [internal,error,'when',parsing,module,definition,Reason]},
		    hd(Tokens)}};
	{asn1_error,Reason} ->
	    {error,{Reason,hd(Tokens)}};
	{ModuleDefinition,Rest1} ->
	    {Types,Rest2} = parse_AssignmentList(Rest1),
	    case Rest2 of
		[{'END',_}|_Rest3] ->
		    {ok,ModuleDefinition#module{typeorval = Types}};
		_  ->
		    {error,{{get_line(hd(Rest2)),get(asn1_module),
			     [got,get_token(hd(Rest2)),expected,'END']},
			    hd(Rest2)}}
	    end
    end.

parse_ModuleDefinition([{typereference,L1,ModuleIdentifier}|Rest0]) ->
    put(asn1_module,ModuleIdentifier),
    {_DefinitiveIdentifier,Rest02} = 
	case Rest0 of
	    [{'{',_}|_Rest01] ->
		parse_ObjectIdentifierValue(Rest0);
	    _ ->
		{[],Rest0}
	end,
    Rest = case Rest02 of
	       [{'DEFINITIONS',_}|Rest03] ->
		   Rest03;
	       _ ->
		   throw({asn1_error,{get_line(hd(Rest02)),get(asn1_module),
				      [got,get_token(hd(Rest02)),
				       expected,'DEFINITIONS']}})
	   end,
    {TagDefault,Rest2} = 
	case Rest of
	    [{'EXPLICIT',_L3},{'TAGS',_L4}|Rest1] ->
		put(tagdefault,'EXPLICIT'), {'EXPLICIT',Rest1};
	    [{'IMPLICIT',_L3},{'TAGS',_L4}|Rest1] ->
		put(tagdefault,'IMPLICIT'), {'IMPLICIT',Rest1};
	    [{'AUTOMATIC',_L3},{'TAGS',_L4}|Rest1] ->
		put(tagdefault,'AUTOMATIC'), {'AUTOMATIC',Rest1};
	    Rest1 ->
		put(tagdefault,'EXPLICIT'), {'EXPLICIT',Rest1} % The default
	end,
    {ExtensionDefault,Rest3} = 
	case Rest2 of
	    [{'EXTENSIBILITY',_L5}, {'IMPLIED',_L6}|Rest21] -> 
		put(extensiondefault,'IMPLIED'),{'IMPLIED',Rest21};
	    _  -> 
		put(extensiondefault,undefined),{undefined,Rest2}
	end,
    case Rest3 of
	[{'::=',_L7}, {'BEGIN',_L8}|Rest4] ->
	    {Exports, Rest5} = parse_Exports(Rest4),
	    {Imports, Rest6} = parse_Imports(Rest5),
	    {#module{ pos = L1,
		     name = ModuleIdentifier,
		     defid = [], % fix this
		     tagdefault = TagDefault,
		     extensiondefault = ExtensionDefault,
		     exports = Exports,
		     imports = Imports},Rest6};
	_ -> throw({asn1_error,{get_line(hd(Rest3)),get(asn1_module),
				[got,get_token(hd(Rest3)),expected,"::= BEGIN"]}})
    end;
parse_ModuleDefinition(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,typereference]}}).
    
parse_Exports([{'EXPORTS',_L1},{';',_L2}|Rest]) ->
    {{exports,[]},Rest};
parse_Exports([{'EXPORTS',_L1}|Rest]) ->
    {SymbolList,Rest2} = parse_SymbolList(Rest),
    case Rest2 of
	[{';',_}|Rest3] ->    
	    {{exports,SymbolList},Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,';']}})
    end;
parse_Exports(Rest) ->
    {{exports,all},Rest}.

parse_SymbolList(Tokens) ->
    parse_SymbolList(Tokens,[]).

parse_SymbolList(Tokens,Acc) ->
    {Symbol,Rest} = parse_Symbol(Tokens),
    case Rest of 
	[{',',_L1}|Rest2] ->
	    parse_SymbolList(Rest2,[Symbol|Acc]);
	Rest2  ->
	    {lists:reverse([Symbol|Acc]),Rest2}
    end.

parse_Symbol(Tokens) ->
    parse_Reference(Tokens).

parse_Reference([{typereference,L1,TrefName},{'{',_L2},{'}',_L3}|Rest]) ->
%    {Tref,Rest};
    {tref2Exttref(L1,TrefName),Rest};
parse_Reference([Tref1 = {typereference,_,_},{'.',_},Tref2 = {typereference,_,_},
		 {'{',_L2},{'}',_L3}|Rest]) ->
%    {{Tref1,Tref2},Rest};
    {{tref2Exttref(Tref1),tref2Exttref(Tref2)},Rest};
parse_Reference([Tref = {typereference,_L1,_TrefName}|Rest]) ->
    {tref2Exttref(Tref),Rest};
parse_Reference([Vref = {identifier,_L1,_VName},{'{',_L2},{'}',_L3}|Rest]) ->
    {identifier2Extvalueref(Vref),Rest};
parse_Reference([Vref = {identifier,_L1,_VName}|Rest]) ->
    {identifier2Extvalueref(Vref),Rest};
parse_Reference(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[typereference,identifier]]}}).

parse_Imports([{'IMPORTS',_L1},{';',_L2}|Rest]) ->
    {{imports,[]},Rest};
parse_Imports([{'IMPORTS',_L1}|Rest]) ->
    {SymbolsFromModuleList,Rest2} = parse_SymbolsFromModuleList(Rest),
    case Rest2 of
	[{';',_L2}|Rest3] ->
	    {{imports,SymbolsFromModuleList},Rest3};
	Rest3 ->
	    throw({asn1_error,{get_line(hd(Rest3)),get(asn1_module),
			       [got,get_token(hd(Rest3)),expected,';']}})
    end;
parse_Imports(Tokens) ->
    {{imports,[]},Tokens}.

parse_SymbolsFromModuleList(Tokens) ->
    parse_SymbolsFromModuleList(Tokens,[]).

parse_SymbolsFromModuleList(Tokens,Acc) ->
    {SymbolsFromModule,Rest} = parse_SymbolsFromModule(Tokens),
    case (catch parse_SymbolsFromModule(Rest)) of 
	{Sl,_Rest2} when is_record(Sl,'SymbolsFromModule') ->
	    parse_SymbolsFromModuleList(Rest,[SymbolsFromModule|Acc]);
	_  ->
	    {lists:reverse([SymbolsFromModule|Acc]),Rest}
    end.
    
parse_SymbolsFromModule(Tokens) ->
    SetRefModuleName =
	fun(N) ->
		fun(X) when is_record(X,'Externaltypereference')->
			X#'Externaltypereference'{module=N};
		   (X) when is_record(X,'Externalvaluereference')->
			X#'Externalvaluereference'{module=N} 
		end
	end,
    {SymbolList,Rest} = parse_SymbolList(Tokens),
    case Rest of
	[{'FROM',_L1},Tref = {typereference,_,Name},Ref={identifier,_L2,_Id},C={',',_}|Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name),SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},[Ref,C|Rest2]};

	%% This a special case when there is only one Symbol imported
	%% from the next module. No other way to distinguish Ref from
	%% a part of the GlobalModuleReference of Name.
	[{'FROM',_L1},Tref = {typereference,_,Name},Ref = {identifier,_L2,_Id},From = {'FROM',_}|Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name),SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},[Ref,From|Rest2]};
	[{'FROM',_L1},Tref = {typereference,_,Name},{identifier,_L2,_Id}|Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name),SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest2}; 
	[{'FROM',_L1},Tref = {typereference,_,Name},Brace = {'{',_}|Rest2] ->
	    {_ObjIdVal,Rest3} = parse_ObjectIdentifierValue([Brace|Rest2]), % value not used yet, fix me
	    NewSymbolList = lists:map(SetRefModuleName(Name),SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest3}; 
	[{'FROM',_L1},Tref = {typereference,_,Name}|Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name),SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest2};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest)),get(asn1_module),
			       [got,get_token(hd(Rest)),expected,
				['FROM typerefernece identifier ,',
				 'FROM typereference identifier',
				 'FROM typereference {',
				 'FROM typereference']]}})
    end.

parse_ObjectIdentifierValue([{'{',_}|Rest]) ->
    parse_ObjectIdentifierValue(Rest,[]).

parse_ObjectIdentifierValue([{number,_,Num}|Rest],Acc) ->
    parse_ObjectIdentifierValue(Rest,[Num|Acc]);
parse_ObjectIdentifierValue([{identifier,_,Id},{'(',_}, {number,_,Num}, {')',_}|Rest],Acc) ->
    parse_ObjectIdentifierValue(Rest,[{'NamedNumber',Id,Num}|Acc]);
parse_ObjectIdentifierValue([{identifier,_,Id},{'(',_}, {identifier,_,Id2}, {')',_}|Rest],Acc) ->
    parse_ObjectIdentifierValue(Rest,[{'NamedNumber',Id,Id2}|Acc]);
parse_ObjectIdentifierValue([{identifier,_,Id},{'(',_}, {typereference,_,Tref},{'.',_},{identifier,_,Id2}, {')',_}|Rest],Acc) ->
    parse_ObjectIdentifierValue(Rest,[{'NamedNumber',Id,{'ExternalValue',Tref,Id2}}|Acc]);
parse_ObjectIdentifierValue([Id = {identifier,_,_}|Rest],Acc) ->
    parse_ObjectIdentifierValue(Rest,[identifier2Extvalueref(Id)|Acc]);
parse_ObjectIdentifierValue([{'}',_}|Rest],Acc) ->
    {lists:reverse(Acc),Rest};
parse_ObjectIdentifierValue([H|_T],_Acc) ->
    throw({asn1_error,{get_line(H),get(asn1_module),
		       [got,get_token(H),expected,
			['{ some of the following }',number,'identifier ( number )',
			 'identifier ( identifier )',
			 'identifier ( typereference.identifier)',identifier]]}}).
    
parse_AssignmentList(Tokens = [{'END',_}|_Rest]) ->
    {[],Tokens};
parse_AssignmentList(Tokens = [{'$end',_}|_Rest]) ->
    {[],Tokens};
parse_AssignmentList(Tokens) ->
    parse_AssignmentList(Tokens,[]).

parse_AssignmentList(Tokens= [{'END',_}|_Rest],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AssignmentList(Tokens= [{'$end',_}|_Rest],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AssignmentList(Tokens,Acc) ->
    case (catch parse_Assignment(Tokens)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{asn1_error,R} ->
%	    [H|T] = Tokens,
	    throw({error,{R,hd(Tokens)}});
	{Assignment,Rest} ->
	    parse_AssignmentList(Rest,[Assignment|Acc])
    end.

parse_Assignment(Tokens) ->
    Flist = [fun parse_TypeAssignment/1,
	     fun parse_ValueAssignment/1,
	     fun parse_ObjectClassAssignment/1,
	     fun parse_ObjectAssignment/1,
	     fun parse_ObjectSetAssignment/1,
	     fun parse_ParameterizedAssignment/1,
	     fun parse_ValueSetTypeAssignment/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	{asn1_assignment_error,Reason} ->
	    throw({asn1_error,Reason});
	Result ->
	    Result
    end.


parse_or(Tokens,Flist) ->
	parse_or(Tokens,Flist,[]).

parse_or(_Tokens,[],ErrList) ->
    case ErrList of
	[] ->
	    throw({asn1_error,{parse_or,ErrList}});
	L when is_list(L) ->
	    %% chose to throw 1) the error with the highest line no,
	    %% 2) the last error which is not a asn1_assignment_error or
	    %% 3) the last error.
	    throw(prioritize_error(ErrList))
    end;
parse_or(Tokens,[Fun|Frest],ErrList) ->
    case (catch Fun(Tokens)) of
	Exit = {'EXIT',_Reason} ->
	    parse_or(Tokens,Frest,[Exit|ErrList]);
	AsnErr = {asn1_error,_} ->
	    parse_or(Tokens,Frest,[AsnErr|ErrList]);
	AsnAssErr = {asn1_assignment_error,_} ->
	    parse_or(Tokens,Frest,[AsnAssErr|ErrList]);
	Result = {_,L} when is_list(L) ->
	    Result;
	Error  ->
	    parse_or(Tokens,Frest,[Error|ErrList])
    end.

parse_or_tag(Tokens,Flist) ->
	parse_or_tag(Tokens,Flist,[]).

parse_or_tag(_Tokens,[],ErrList) ->
    case ErrList of
	[] ->
	    throw({asn1_error,{parse_or_tag,ErrList}});
	L when is_list(L) ->
	    %% chose to throw 1) the error with the highest line no,
	    %% 2) the last error which is not a asn1_assignment_error or
	    %% 3) the last error.
	    throw(prioritize_error(ErrList))
    end;
parse_or_tag(Tokens,[{Tag,Fun}|Frest],ErrList) when is_function(Fun) ->
    case (catch Fun(Tokens)) of
	Exit = {'EXIT',_Reason} ->
	    parse_or_tag(Tokens,Frest,[Exit|ErrList]);
	AsnErr = {asn1_error,_} ->
	    parse_or_tag(Tokens,Frest,[AsnErr|ErrList]);
	AsnAssErr = {asn1_assignment_error,_} ->
	    parse_or_tag(Tokens,Frest,[AsnAssErr|ErrList]);
	{ParseRes,Rest} when is_list(Rest) ->
	    {{Tag,ParseRes},Rest};
	Error  ->
	    parse_or_tag(Tokens,Frest,[Error|ErrList])
    end.

parse_TypeAssignment([{typereference,L1,Tref},{'::=',_}|Rest]) ->	
    {Type,Rest2} = parse_Type(Rest),
    {#typedef{pos=L1,name=Tref,typespec=Type},Rest2};
parse_TypeAssignment([H1,H2|_Rest]) ->
    throw({asn1_assignment_error,{get_line(H1),get(asn1_module),
				  [got,[get_token(H1),get_token(H2)], expected,
				   typereference,'::=']}});
parse_TypeAssignment([H|_T]) ->
    throw({asn1_assignment_error,{get_line(H),get(asn1_module),
				  [got,get_token(H),expected,
				   typereference]}}).

%% parse_Type(Tokens) -> Ret
%%
%% Tokens = [Tok]
%% Tok    = tuple()
%% Ret    = #type{}
%%
parse_Type(Tokens) ->
    {Tag,Rest3} = case Tokens of
		      [Lbr= {'[',_}|Rest] ->
			  parse_Tag([Lbr|Rest]);
		      Rest-> {[],Rest}
		  end,
    {Tag2,Rest4} = case Rest3 of
		       [{'IMPLICIT',_}|Rest31] when is_record(Tag,tag)->
			   {[Tag#tag{type='IMPLICIT'}],Rest31};
		       [{'EXPLICIT',_}|Rest31] when is_record(Tag,tag)->
			   {[Tag#tag{type='EXPLICIT'}],Rest31};
		       Rest31 when is_record(Tag,tag) ->
			   {[Tag#tag{type={default,get(tagdefault)}}],Rest31};
		       Rest31 ->
			   {Tag,Rest31}
		   end,
    Flist = [fun parse_BuiltinType/1,fun parse_ReferencedType/1,fun parse_TypeWithConstraint/1],
    {Type,Rest5} = case (catch parse_or(Rest4,Flist)) of
		      {'EXIT',Reason} ->
			  exit(Reason);
			AsnErr = {asn1_error,_Reason} ->
			 throw(AsnErr);
		      Result ->
			  Result
		  end,
    case hd(Rest5) of
	{'(',_} ->
	    {Constraints,Rest6} = parse_Constraints(Rest5),
	    if is_record(Type,type) ->
		    {Type#type{constraint=merge_constraints(Constraints),
			       tag=Tag2},Rest6};
	       true ->
		    {#type{def=Type,constraint=merge_constraints(Constraints),
			   tag=Tag2},Rest6}
	    end;
	_ ->
	    if is_record(Type,type) ->
		    {Type#type{tag=Tag2},Rest5};
	       true ->
		    {#type{def=Type,tag=Tag2},Rest5}
	    end
    end.

parse_BuiltinType([{'BIT',_},{'STRING',_}|Rest]) ->
    case Rest of 
	[{'{',_}|Rest2] ->
	    {NamedNumberList,Rest3} = parse_NamedNumberList(Rest2),
	    case Rest3 of
		[{'}',_}|Rest4] ->
		    {#type{def={'BIT STRING',NamedNumberList}},Rest4};
		_ ->
		    throw({asn1_error,{get_line(hd(Rest3)),get(asn1_module),
				       [got,get_token(hd(Rest3)),expected,'}']}})
	    end;
	 _ ->
	    {{'BIT STRING',[]},Rest}
    end;
parse_BuiltinType([{'BOOLEAN',_}|Rest]) ->
    {#type{def='BOOLEAN'},Rest};
%% CharacterStringType ::= RestrictedCharacterStringType | 
%%                         UnrestrictedCharacterStringType
parse_BuiltinType([{restrictedcharacterstringtype,_,StringName}|Rest]) ->
    {#type{def=StringName},Rest};
parse_BuiltinType([{'CHARACTER',_},{'STRING',_}|Rest]) ->
    {#type{def='CHARACTER STRING'},Rest};

parse_BuiltinType([{'CHOICE',_},{'{',_}|Rest]) ->
    {AlternativeTypeLists,Rest2} = parse_AlternativeTypeLists(Rest),
    AlternativeTypeLists1 =
	lists:filter(fun(#'ExtensionAdditionGroup'{}) -> false;
			('ExtensionAdditionGroupEnd') -> false;
			(_) -> true
		     end,AlternativeTypeLists),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    AlternativeTypeLists2 =
		case {[Ext||Ext = #'EXTENSIONMARK'{} <- AlternativeTypeLists1],
		      get(extensiondefault)} of
		    {[],'IMPLIED'} ->  AlternativeTypeLists1 ++ [#'EXTENSIONMARK'{}];
		    _ -> AlternativeTypeLists1
		end,

	    {#type{def={'CHOICE',AlternativeTypeLists2}},Rest3};
	_  ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;
parse_BuiltinType([{'EMBEDDED',_},{'PDV',_}|Rest]) ->
    {#type{def='EMBEDDED PDV'},Rest};
parse_BuiltinType([{'ENUMERATED',_},{'{',_}|Rest]) ->
    {Enumerations,Rest2} = parse_Enumerations(Rest,get(extensiondefault)),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {#type{def={'ENUMERATED',Enumerations}},Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;
parse_BuiltinType([{'EXTERNAL',_}|Rest]) ->
    {#type{def='EXTERNAL'},Rest};

% InstanceOfType
parse_BuiltinType([{'INSTANCE',_},{'OF',_}|Rest]) ->
    {DefinedObjectClass,Rest2} = parse_DefinedObjectClass(Rest),
    case Rest2 of
	[{'(',_}|_] ->
	    {Constraint,Rest3} = parse_Constraint(Rest2),
	    {#type{def={'INSTANCE OF',DefinedObjectClass,Constraint}},Rest3};
	_ ->
	    {#type{def={'INSTANCE OF',DefinedObjectClass,[]}},Rest2}
    end;

% parse_BuiltinType(Tokens) ->
    
parse_BuiltinType([{'INTEGER',_}|Rest]) ->
    case Rest of 
	[{'{',_}|Rest2] ->
	    {NamedNumberList,Rest3} = parse_NamedNumberList(Rest2),
	    case Rest3 of
		[{'}',_}|Rest4] ->
		    {#type{def={'INTEGER',NamedNumberList}},Rest4};
		_ ->
		    throw({asn1_error,{get_line(hd(Rest3)),get(asn1_module),
				       [got,get_token(hd(Rest3)),expected,'}']}})
	    end;
	 _ ->
	    {#type{def='INTEGER'},Rest}
    end;
parse_BuiltinType([{'NULL',_}|Rest]) ->
    {#type{def='NULL'},Rest};

% ObjectClassFieldType fix me later

parse_BuiltinType([{'OBJECT',_},{'IDENTIFIER',_}|Rest]) ->
    {#type{def='OBJECT IDENTIFIER'},Rest};
parse_BuiltinType([{'OCTET',_},{'STRING',_}|Rest]) ->
    {#type{def='OCTET STRING'},Rest};
parse_BuiltinType([{'REAL',_}|Rest]) ->
    {#type{def='REAL'},Rest};
parse_BuiltinType([{'RELATIVE-OID',_}|Rest]) ->
    {#type{def='RELATIVE-OID'},Rest};
parse_BuiltinType([{'SEQUENCE',_},{'{',_},{'}',_}|Rest]) ->
    {#type{def=#'SEQUENCE'{components=[]}},
     Rest};
parse_BuiltinType([{'SEQUENCE',_},{'{',_},{'...',Line},{'}',_}|Rest]) ->
    {#type{def=#'SEQUENCE'{components=[#'EXTENSIONMARK'{pos = Line}]}},Rest};
parse_BuiltinType([{'SEQUENCE',_},{'{',_},{'...',Line},{'!',_}|Rest]) ->
    {ExceptionIdentification,Rest2} = parse_ExceptionIdentification(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {#type{def=#'SEQUENCE'{
		     components=[#'EXTENSIONMARK'{
				    pos = Line,
				    val = ExceptionIdentification}]}},
	     Rest3};
	_ ->
	    {ComponentTypeLists,Rest3}=
		parse_ComponentTypeLists2(Rest2,[#'EXTENSIONMARK'{pos=Line}]),
	    case Rest3 of
		[{'}',_}|Rest4] ->
	    {#type{def=#'SEQUENCE'{components=ComponentTypeLists}},Rest4};
		_  ->
		    throw({asn1_error,{get_line(hd(Rest3)),get(asn1_module),
			       [got,get_token(hd(Rest3)),expected,'}']}})
	    end
% 	_ -> % Seq case 4,17-19,23-26 will fail here
% 	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
% 			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;
parse_BuiltinType([{'SEQUENCE',_},{'{',_}|Rest]) ->
    {ComponentTypeLists,Rest2} = parse_ComponentTypeLists(Rest),
    case Rest2  of
	[{'}',_}|Rest3] ->
	    ComponentTypeLists2 =
		case {[Ext||Ext = #'EXTENSIONMARK'{} <- ComponentTypeLists],
		      get(extensiondefault)} of
		    {[],'IMPLIED'} ->  ComponentTypeLists ++ [#'EXTENSIONMARK'{}];
		    _ -> ComponentTypeLists
		end,
	    {#type{def=#'SEQUENCE'{components = ComponentTypeLists2}},
	     Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;

parse_BuiltinType([{'SEQUENCE',_},{'OF',_},Id={identifier,_,_},Lt={'<',_}|Rest]) ->
%% TODO: take care of the identifier for something useful
    {Type,Rest2} = parse_SelectionType([Id,Lt|Rest]),
    {#type{def={'SEQUENCE OF',#type{def=Type,tag=[]}}},Rest2};

parse_BuiltinType([{'SEQUENCE',_},{'OF',_},{identifier,_,_} |Rest]) ->
%% TODO: take care of the identifier for something useful
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SEQUENCE OF',Type}},Rest2};

parse_BuiltinType([{'SEQUENCE',_},{'OF',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SEQUENCE OF',Type}},Rest2};


parse_BuiltinType([{'SET',_},{'{',_},{'...',Line},{'}',_}|Rest]) ->
    {#type{def=#'SET'{components=[#'EXTENSIONMARK'{pos = Line}]}},Rest};
parse_BuiltinType([{'SET',_},{'{',_},{'...',Line},{'!',_}|Rest]) ->
    {ExceptionIdentification,Rest2} = parse_ExceptionIdentification(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {#type{def=#'SET'{components=
			      [#'EXTENSIONMARK'{pos = Line,
						val = ExceptionIdentification}]}},
	     Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;
parse_BuiltinType([{'SET',_},{'{',_}|Rest]) ->
    {ComponentTypeLists,Rest2} = parse_ComponentTypeLists(Rest),
    case Rest2  of
	[{'}',_}|Rest3] ->
	    ComponentTypeLists2 =
		case {[Ext||Ext = #'EXTENSIONMARK'{} <- ComponentTypeLists],
		      get(extensiondefault)} of
		    {[],'IMPLIED'} ->  ComponentTypeLists ++ [#'EXTENSIONMARK'{}];
		    _ -> ComponentTypeLists
		end,
	    {#type{def=#'SET'{components = ComponentTypeLists2}},
	     Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;

parse_BuiltinType([{'SET',_},{'OF',_},Id={identifier,_,_},Lt={'<',_}|Rest]) ->
%% TODO: take care of the identifier for something useful
    {Type,Rest2} = parse_SelectionType([Id,Lt|Rest]),
    {#type{def={'SET OF',#type{def=Type,tag=[]}}},Rest2};


parse_BuiltinType([{'SET',_},{'OF',_},{identifier,_,_}|Rest]) ->
%%TODO: take care of the identifier for something useful
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SET OF',Type}},Rest2};

parse_BuiltinType([{'SET',_},{'OF',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SET OF',Type}},Rest2};

%% The so called Useful types
parse_BuiltinType([{'GeneralizedTime',_}|Rest]) ->
    {#type{def='GeneralizedTime'},Rest};
parse_BuiltinType([{'UTCTime',_}|Rest]) ->
    {#type{def='UTCTime'},Rest};
parse_BuiltinType([{'ObjectDescriptor',_}|Rest]) ->
    {#type{def='ObjectDescriptor'},Rest};

%% For compatibility with old standard
parse_BuiltinType([{'ANY',_},{'DEFINED',_},{'BY',_},{identifier,_,Id}|Rest]) ->
    {#type{def={'ANY_DEFINED_BY',Id}},Rest};
parse_BuiltinType([{'ANY',_}|Rest]) ->
    {#type{def='ANY'},Rest};

parse_BuiltinType(Tokens) ->
    parse_ObjectClassFieldType(Tokens).
%    throw({asn1_error,unhandled_type}).


parse_TypeWithConstraint([{'SEQUENCE',_},Lpar = {'(',_}|Rest]) ->
    {Constraint,Rest2} = parse_Constraint([Lpar|Rest]),
    Rest4 = case Rest2 of
		[{'OF',_}, {identifier,_,_Id}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    throw({asn1_error,
			   {get_line(hd(Rest2)),get(asn1_module),
			    [got,get_token(hd(Rest2)),expected,'OF']}})
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SEQUENCE OF',Type},
	   constraint = merge_constraints([Constraint])},Rest5};

parse_TypeWithConstraint([{'SEQUENCE',_},{'SIZE',_},Lpar = {'(',_}|Rest]) ->
    {Constraint,Rest2} = parse_Constraint([Lpar|Rest]),
    #constraint{c=C} = Constraint,
    Constraint2 = Constraint#constraint{c={'SizeConstraint',C}},
    Rest4 = case Rest2 of
		[{'OF',_}, {identifier,_,_Id}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
				       [got,get_token(hd(Rest2)),expected,'OF']}})
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SEQUENCE OF',Type}, constraint = merge_constraints([Constraint2])},Rest5};

parse_TypeWithConstraint([{'SET',_},Lpar = {'(',_}|Rest]) ->
    {Constraint,Rest2} = parse_Constraint([Lpar|Rest]),
    Rest4 = case Rest2 of
		[{'OF',_}, {identifier,_,_Id}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    throw({asn1_error,
			   {get_line(hd(Rest2)),get(asn1_module),
			    [got,get_token(hd(Rest2)),expected,'OF']}})
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SET OF',Type},
	   constraint = merge_constraints([Constraint])},Rest5};

parse_TypeWithConstraint([{'SET',_},{'SIZE',_},Lpar = {'(',_}|Rest]) ->
    {Constraint,Rest2} = parse_Constraint([Lpar|Rest]),
    #constraint{c=C} = Constraint,
    Constraint2 = Constraint#constraint{c={'SizeConstraint',C}},
    Rest4 = case Rest2 of
		[{'OF',_}, {identifier,_,_Id}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    throw({asn1_error,
			   {get_line(hd(Rest2)),get(asn1_module),
			    [got,get_token(hd(Rest2)),expected,'OF']}})
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SET OF',Type},
	   constraint = merge_constraints([Constraint2])},Rest5};

parse_TypeWithConstraint(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
		       ['SEQUENCE','SEQUENCE SIZE','SET','SET SIZE'],
		       followed,by,a,constraint]}}).


%% --------------------------

parse_ReferencedType(Tokens) ->
    Flist = [fun parse_DefinedType/1,
	     fun parse_SelectionType/1,
	     fun parse_TypeFromObject/1,
	     fun parse_ValueSetFromObjects/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.
    
parse_DefinedType(Tokens=[{typereference,_,_},{'{',_}|_Rest]) ->
    parse_ParameterizedType(Tokens);
parse_DefinedType(Tokens=[{typereference,L1,TypeName},
			  T2={typereference,_,_},T3={'{',_}|Rest]) ->
    case (catch parse_ParameterizedType(Tokens)) of
	{'EXIT',_Reason} ->
	    Rest2 = [T2,T3|Rest],
	    {#type{def = #'Externaltypereference'{pos=L1,
						  module=get(asn1_module),
						  type=TypeName}},Rest2};
	{asn1_error,_} ->
	    Rest2 = [T2,T3|Rest],
	    {#type{def = #'Externaltypereference'{pos=L1,
						  module=get(asn1_module),
						  type=TypeName}},Rest2};
	Result ->
	    Result
    end;
parse_DefinedType(Tokens=[{typereference,_L1,_Module},{'.',_},
			  {typereference,_,_TypeName},{'{',_}|_Rest]) ->
    parse_ParameterizedType(Tokens);
parse_DefinedType([{typereference,L1,Module},{'.',_},{typereference,_,TypeName}|Rest]) ->
    {#type{def = #'Externaltypereference'{pos=L1,module=Module,type=TypeName}},Rest};
parse_DefinedType([{typereference,L1,TypeName}|Rest]) ->
    case is_pre_defined_class(TypeName) of
	false  ->
	    {#type{def = #'Externaltypereference'{pos=L1,module=get(asn1_module),
						  type=TypeName}},Rest};
	_ ->
	    throw({asn1_error,
		   {L1,get(asn1_module),
		    [got,TypeName,expected,
		     [typereference,'typereference.typereference',
		      'typereference typereference']]}})
    end;
parse_DefinedType(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[typereference,'typereference.typereference',
			 'typereference typereference']]}}).

parse_SelectionType([{identifier,_,Name},{'<',_}|Rest]) ->    
    {Type,Rest2} = parse_Type(Rest),
    {{'SelectionType',Name,Type},Rest2};
parse_SelectionType(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'identifier <']}}).
    

%% --------------------------


%% This should probably be removed very soon 
% parse_ConstrainedType(Tokens) ->
%     case (catch parse_TypeWithConstraint(Tokens)) of
% 	{'EXIT',Reason} ->
% 	    {Type,Rest} = parse_Type(Tokens),
% 	    {Constraint,Rest2} = parse_Constraint(Rest),
% 	    {Type#type{constraint=Constraint},Rest2};
% 	{asn1_error,Reason2} ->
% 	    {Type,Rest} = parse_Type(Tokens),
% 	    {Constraint,Rest2} = parse_Constraint(Rest),
% 	    {Type#type{constraint=Constraint},Rest2};
% 	Result ->
% 	    Result
%     end.

parse_Constraints(Tokens) ->
    parse_Constraints(Tokens,[]).

parse_Constraints(Tokens,Acc) ->
    {Constraint,Rest} = parse_Constraint(Tokens),
    case Rest of
	[{'(',_}|_Rest2] ->
	    parse_Constraints(Rest,[Constraint|Acc]);
	_ ->
	    {lists:reverse([Constraint|Acc]),Rest}
    end.

parse_Constraint([{'(',_}|Rest]) ->
    {Constraint,Rest2} = parse_ConstraintSpec(Rest),
    {Exception,Rest3} = parse_ExceptionSpec(Rest2),
    case Rest3 of
	[{')',_}|Rest4] ->
	    {#constraint{c=Constraint,e=Exception},Rest4};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,')']}})
    end;
parse_Constraint(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'(']}}).

parse_ConstraintSpec(Tokens) ->
    Flist = [fun parse_GeneralConstraint/1,
	     fun parse_SubtypeConstraint/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{asn1_error,Reason2} ->
	    throw({asn1_error,Reason2});
	Result ->
	    Result
    end.

parse_ExceptionSpec([LPar={')',_}|Rest]) ->
    {undefined,[LPar|Rest]};
parse_ExceptionSpec([{'!',_}|Rest]) ->
    parse_ExceptionIdentification(Rest);
parse_ExceptionSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,[')','!']]}}).

parse_ExceptionIdentification(Tokens) ->
    Flist = [fun parse_SignedNumber/1,
	     fun parse_DefinedValue/1,
	     fun parse_TypeColonValue/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{asn1_error,Reason2} ->
	    throw({asn1_error,Reason2});
	Result ->
	    Result
    end.

parse_TypeColonValue(Tokens) ->
    {Type,Rest} = parse_Type(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Value,Rest3} = parse_Value(Rest2),
	    {{Type,Value},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,':']}})
    end.

parse_SubtypeConstraint(Tokens) ->
    parse_ElementSetSpecs(Tokens).

parse_ElementSetSpecs([{'...',_}|Rest]) ->
    {Elements,Rest2} = parse_ElementSetSpec(Rest),
    {{[],Elements},Rest2};
parse_ElementSetSpecs(Tokens) ->
    {RootElems,Rest} = parse_ElementSetSpec(Tokens),
    case Rest of
	[{',',_},{'...',_},{',',_}|Rest2] ->
	    {AdditionalElems,Rest3} = parse_ElementSetSpec(Rest2),
	    {{RootElems,AdditionalElems},Rest3};
	[{',',_},{'...',_}|Rest2] ->
	    {{RootElems,[]},Rest2};
	_ ->
	    {RootElems,Rest}
    end.

parse_ElementSetSpec([{'ALL',_},{'EXCEPT',_}|Rest]) ->
    {Exclusions,Rest2} = parse_Elements(Rest),
    {{'ALL',{'EXCEPT',Exclusions}},Rest2};
parse_ElementSetSpec(Tokens) ->
    parse_Unions(Tokens).


%% parse_Unions(Tokens) -> {Ret,Rest}
%% Tokens = [Tok]
%% Tok    = tuple()
%% Ret    = {'SingleValue',list()} | list() |
%%          
parse_Unions(Tokens) ->
    {InterSec,Rest} = parse_Intersections(Tokens),
    {Unions,Rest2} = parse_UnionsRec(Rest),
    case {InterSec,Unions} of
	{InterSec,[]} ->
	    {InterSec,Rest2};
	{{'SingleValue',V1},{'SingleValue',V2}} ->
	    {{'SingleValue',ordsets:union(to_set(V1),to_set(V2))},Rest2};
	{V1,V2} when is_list(V2) ->
	    {[V1] ++ [union|V2],Rest2};
	{V1,V2} ->
	    {[V1,union,V2],Rest2}
%	Other ->
%	    throw(Other)
    end.

parse_UnionsRec([{'|',_}|Rest]) ->
    {InterSec,Rest2} = parse_Intersections(Rest),
    {URec,Rest3} = parse_UnionsRec(Rest2),
    case {InterSec,URec} of
	{V1,[]} ->
	    {V1,Rest3};
	{{'SingleValue',V1},{'SingleValue',V2}} ->
	    {{'SingleValue',ordsets:union(to_set(V1),to_set(V2))},Rest3};
	{V1,V2} when is_list(V2) ->
	    {[V1] ++ [union|V2],Rest3};
	{V1,V2} ->
	    {[V1,union,V2],Rest3}
	end;
parse_UnionsRec([{'UNION',_}|Rest]) ->
    {InterSec,Rest2} = parse_Intersections(Rest),
    {URec,Rest3} = parse_UnionsRec(Rest2),
    case {InterSec,URec} of
	{V1,[]} ->
	    {V1,Rest3};
	{{'SingleValue',V1},{'SingleValue',V2}} ->
	    {{'SingleValue',ordsets:union(to_set(V1),to_set(V2))},Rest3};
	{V1,V2} when is_list(V2) ->
	    {[V1] ++ [union|V2],Rest3};
	{V1,V2} ->
	    {[V1,union,V2],Rest3}
	end;
parse_UnionsRec(Tokens) ->
    {[],Tokens}.

parse_Intersections(Tokens) ->
    {InterSec,Rest} = parse_IntersectionElements(Tokens),
    {IRec,Rest2} = parse_IElemsRec(Rest),
    case {InterSec,IRec} of
	{V1,[]} ->
	    {V1,Rest2};
	{{'SingleValue',V1},{'SingleValue',V2}} ->
	    {{'SingleValue',
	      ordsets:intersection(to_set(V1),to_set(V2))},Rest2};
	{V1,V2} when is_list(V2) ->
	    {[V1] ++ [intersection|V2],Rest2};
	{V1,V2} ->
	    {[V1,intersection,V2],Rest2}
    end.

%% parse_IElemsRec(Tokens) -> Result
%% Result ::= {'SingleValue',ordered_set()} | list()
parse_IElemsRec([{'^',_}|Rest]) ->
    {InterSec,Rest2} = parse_IntersectionElements(Rest),
    {IRec,Rest3} = parse_IElemsRec(Rest2),
    case {InterSec,IRec} of
	{{'SingleValue',V1},{'SingleValue',V2}} ->
	    {{'SingleValue',
	      ordsets:intersection(to_set(V1),to_set(V2))},Rest3};
	{V1,[]} ->
	    {V1,Rest3};
	{V1,V2} when is_list(V2) ->
	    {[V1] ++ [intersection|V2],Rest3};
	{V1,V2} ->
	    {[V1,intersection,V2],Rest3}
    end;
parse_IElemsRec([{'INTERSECTION',_}|Rest]) ->
    {InterSec,Rest2} = parse_IntersectionElements(Rest),
    {IRec,Rest3} = parse_IElemsRec(Rest2),
    case {InterSec,IRec} of
	{{'SingleValue',V1},{'SingleValue',V2}} ->
	    {{'SingleValue',
	      ordsets:intersection(to_set(V1),to_set(V2))},Rest3};
	{V1,[]} ->
	    {V1,Rest3};
	{V1,V2} when is_list(V2) ->
	    {[V1] ++ [intersection|V2],Rest3};
	{V1,V2} ->
	    {[V1,intersection,V2],Rest3}
    end;
parse_IElemsRec(Tokens) ->
    {[],Tokens}.

%% parse_IntersectionElements(Tokens) -> {Result,Rest}
%% Result ::= InterSec | {InterSec,{'EXCEPT',Exclusion}}
%% InterSec ::= {'ALL',{'EXCEPT',Exclusions}} | Unions
%% Unions ::= {'SingleValue',list()} | list() (see parse_Unions)
%% Exclusions ::= InterSec
parse_IntersectionElements(Tokens) ->
    {InterSec,Rest} = parse_Elements(Tokens),
    case Rest of
	[{'EXCEPT',_}|Rest2] ->
	    {Exclusion,Rest3} = parse_Elements(Rest2),
	    {{InterSec,{'EXCEPT',Exclusion}},Rest3};
	Rest ->
	    {InterSec,Rest}
    end.

%% parse_Elements(Tokens) -> {Result,Rest}
%% Result ::= {'ALL',{'EXCEPT',Exclusions}} | Unions
%% Exclusions ::= {'ALL',{'EXCEPT',Exclusions}} | Unions
%% Unions ::= {'SingleValue',list()} | list() (see parse_Unions)
parse_Elements([{'(',_}|Rest]) ->
    {Elems,Rest2} = parse_ElementSetSpec(Rest),
    case Rest2 of
	[{')',_}|Rest3] ->
	    {Elems,Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,')']}})
    end;
parse_Elements(Tokens) ->
    Flist = [fun parse_ObjectSetElements/1,
	     fun parse_SubtypeElements/1,
%	     fun parse_Value/1,
%	     fun parse_Type/1,
	     fun parse_Object/1,
	     fun parse_DefinedObjectSet/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	Err = {asn1_error,_} ->
	    throw(Err);
	Result = {Val,_} when is_record(Val,type) ->
	    Result;

	Result ->
	    Result
    end.
    

    

%% --------------------------

parse_DefinedObjectClass([{typereference,_,_ModName},{'.',_},Tr={typereference,_,_ObjClName}|Rest]) ->
%%    {{objectclassname,ModName,ObjClName},Rest};
%    {{objectclassname,tref2Exttref(Tr)},Rest};
    {tref2Exttref(Tr),Rest};
parse_DefinedObjectClass([Tr={typereference,_,_ObjClName}|Rest]) ->
%    {{objectclassname,tref2Exttref(Tr)},Rest};
    {tref2Exttref(Tr),Rest};
parse_DefinedObjectClass([{'TYPE-IDENTIFIER',_}|Rest]) ->
    {'TYPE-IDENTIFIER',Rest};
parse_DefinedObjectClass([{'ABSTRACT-SYNTAX',_}|Rest]) ->
    {'ABSTRACT-SYNTAX',Rest};
parse_DefinedObjectClass(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			['typereference . typereference',
			 typereference,
			 'TYPE-IDENTIFIER',
			 'ABSTRACT-SYNTAX']]}}).

parse_ObjectClassAssignment([{typereference,L1,ObjClName},{'::=',_}|Rest]) ->
    {Type,Rest2} = parse_ObjectClass(Rest),
    {#classdef{pos=L1,name=ObjClName,typespec=Type},Rest2};
parse_ObjectClassAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   'typereference ::=']}}).

parse_ObjectClass(Tokens) ->
    Flist = [fun parse_DefinedObjectClass/1,
	     fun parse_ObjectClassDefn/1,
	     fun parse_ParameterizedObjectClass/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{asn1_error,Reason2} ->
	    throw({asn1_error,Reason2});
	Result ->
	    Result
    end.

parse_ObjectClassDefn([{'CLASS',_},{'{',_}|Rest]) ->
    {Type,Rest2} = parse_FieldSpec(Rest),
    {WithSyntaxSpec,Rest3} = parse_WithSyntaxSpec(Rest2),
    {#objectclass{fields=Type,syntax=WithSyntaxSpec},Rest3};
parse_ObjectClassDefn(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'CLASS {']}}).

parse_FieldSpec(Tokens) ->
    parse_FieldSpec(Tokens,[]).

parse_FieldSpec(Tokens,Acc) ->
    Flist = [fun parse_FixedTypeValueFieldSpec/1,
	     fun parse_VariableTypeValueFieldSpec/1,
	     fun parse_ObjectFieldSpec/1,
	     fun parse_FixedTypeValueSetFieldSpec/1,
	     fun parse_VariableTypeValueSetFieldSpec/1,
	     fun parse_TypeFieldSpec/1,
	     fun parse_ObjectSetFieldSpec/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	{Type,[{'}',_}|Rest]} ->
	    {lists:reverse([Type|Acc]),Rest};
	{Type,[{',',_}|Rest2]} ->
	    parse_FieldSpec(Rest2,[Type|Acc]);
	{_,[H|_T]}  ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'}']}})
    end.

parse_PrimitiveFieldName([{typefieldreference,_,FieldName}|Rest]) ->
    {{typefieldreference,FieldName},Rest};
parse_PrimitiveFieldName([{valuefieldreference,_,FieldName}|Rest]) ->
    {{valuefieldreference,FieldName},Rest};
parse_PrimitiveFieldName(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[typefieldreference,valuefieldreference]]}}).

parse_FieldName(Tokens) ->
    {Field,Rest} = parse_PrimitiveFieldName(Tokens),
    parse_FieldName(Rest,[Field]).

parse_FieldName([{'.',_}|Rest],Acc) ->
    case (catch parse_PrimitiveFieldName(Rest)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	{FieldName,Rest2} ->
	    parse_FieldName(Rest2,[FieldName|Acc])
    end;
parse_FieldName(Tokens,Acc) ->
    {lists:reverse(Acc),Tokens}.
    
parse_FixedTypeValueFieldSpec([{valuefieldreference,L1,VFieldName}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {Unique,Rest3} = 
	case Rest2 of
	    [{'UNIQUE',_}|Rest4] ->
		{'UNIQUE',Rest4};
	    _  ->
		{undefined,Rest2}
	end,
    {OptionalitySpec,Rest5} = parse_ValueOptionalitySpec(Rest3),
    case {Unique,Rest5} of
	{'UNIQUE',[{Del,_}|_]} when Del =:= ','; Del =:= '}' ->
	    case OptionalitySpec of 
		{'DEFAULT',_} ->
		    throw({asn1_error,
			   {L1,get(asn1_module),
			    ['UNIQUE and DEFAULT in same field',VFieldName]}});
		_ ->
		    {{fixedtypevaluefield,VFieldName,Type,Unique,OptionalitySpec},Rest5}
	    end;
	{_,[{Del,_}|_]} when Del =:= ','; Del =:= '}'  ->
	    {{object_or_fixedtypevalue_field,VFieldName,Type,Unique,OptionalitySpec},Rest5};
	_ ->
	    throw({asn1_error,{L1,get(asn1_module),
			       [got,get_token(hd(Rest5)),expected,[',','}']]}})
    end;
parse_FixedTypeValueFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,valuefieldreference]}}).

parse_VariableTypeValueFieldSpec([{valuefieldreference,L,VFieldName}|Rest]) ->
    {FieldRef,Rest2} = parse_FieldName(Rest),
    {OptionalitySpec,Rest3} = parse_ValueOptionalitySpec(Rest2),
    case Rest3 of
	[{Del,_}|_] when Del =:= ','; Del =:= '}' ->
	    {{variabletypevaluefield,VFieldName,FieldRef,OptionalitySpec},Rest3};
	_ ->
	    throw({asn1_error,{L,get(asn1_module),
			   [got,get_token(hd(Rest3)),expected,[',','}']]}})
    end;
parse_VariableTypeValueFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,valuefieldreference]}}).

parse_ObjectFieldSpec([{valuefieldreference,L,VFieldName}|Rest]) ->
    {Class,Rest2} = parse_DefinedObjectClass(Rest),
    {OptionalitySpec,Rest3} = parse_ObjectOptionalitySpec(Rest2),
    case Rest3 of
	[{Del,_}|_] when Del =:= ','; Del =:= '}' ->
	    {{objectfield,VFieldName,Class,undefined,OptionalitySpec},Rest3};
	_ ->
	    throw({asn1_error,{L,get(asn1_module),
			   [got,get_token(hd(Rest3)),expected,[',','}']]}})
    end;
parse_ObjectFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,valuefieldreference]}}).

parse_TypeFieldSpec([{typefieldreference,L,TFieldName}|Rest]) ->
    {OptionalitySpec,Rest2} = parse_TypeOptionalitySpec(Rest),
    case Rest2 of
	[{Del,_}|_] when Del =:= ','; Del =:= '}' ->
	    {{typefield,TFieldName,OptionalitySpec},Rest2};
	_ ->
	    throw({asn1_error,{L,get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,[',','}']]}})
    end;
parse_TypeFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,typefieldreference]}}).

parse_FixedTypeValueSetFieldSpec([{typefieldreference,L,TFieldName}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {OptionalitySpec,Rest3} = parse_ValueSetOptionalitySpec(Rest2),
    case Rest3 of
	[{Del,_}|_] when Del =:= ','; Del =:= '}' ->
	    {{objectset_or_fixedtypevalueset_field,TFieldName,Type,
	      OptionalitySpec},Rest3};
	_ ->
	    throw({asn1_error,{L,get(asn1_module),
			       [got,get_token(hd(Rest3)),expected,[',','}']]}})
    end;  
parse_FixedTypeValueSetFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,typefieldreference]}}).

parse_VariableTypeValueSetFieldSpec([{typefieldreference,L,TFieldName}|Rest]) ->
    {FieldRef,Rest2} = parse_FieldName(Rest),
    {OptionalitySpec,Rest3} = parse_ValueSetOptionalitySpec(Rest2),
    case Rest3 of
	[{Del,_}|_] when Del =:= ','; Del =:= '}' ->
	    {{variabletypevaluesetfield,TFieldName,FieldRef,OptionalitySpec},Rest3};
	_ ->
	    throw({asn1_error,{L,get(asn1_module),
			       [got,get_token(hd(Rest3)),expected,[',','}']]}})
    end; 
parse_VariableTypeValueSetFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,typefieldreference]}}).

parse_ObjectSetFieldSpec([{typefieldreference,L,TFieldName}|Rest]) ->
    {Class,Rest2} = parse_DefinedObjectClass(Rest),
    {OptionalitySpec,Rest3} = parse_ObjectSetOptionalitySpec(Rest2),
    case Rest3 of
	[{Del,_}|_] when Del =:= ','; Del =:= '}' ->
	    {{objectsetfield,TFieldName,Class,OptionalitySpec},Rest3};
	_ ->
	    throw({asn1_error,{L,get(asn1_module),
			       [got,get_token(hd(Rest3)),expected,[',','}']]}})
    end;  
parse_ObjectSetFieldSpec(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,typefieldreference]}}).

parse_ValueOptionalitySpec(Tokens)->
    case Tokens of
	[{'OPTIONAL',_}|Rest] -> {'OPTIONAL',Rest};
	[{'DEFAULT',_}|Rest] ->
	    {Value,Rest2} = parse_Value(Rest),
	    {{'DEFAULT',Value},Rest2};
	_  -> {'MANDATORY',Tokens}
    end.

parse_ObjectOptionalitySpec(Tokens) ->
    case Tokens of
	[{'OPTIONAL',_}|Rest] -> {'OPTIONAL',Rest};
	[{'DEFAULT',_}|Rest] ->
	    {Object,Rest2} = parse_Object(Rest),
	    {{'DEFAULT',Object},Rest2};
	_  -> {'MANDATORY',Tokens}
    end.

parse_TypeOptionalitySpec(Tokens) ->
    case Tokens of
	[{'OPTIONAL',_}|Rest] -> {'OPTIONAL',Rest};
	[{'DEFAULT',_}|Rest] ->
	    {Type,Rest2} = parse_Type(Rest),
	    {{'DEFAULT',Type},Rest2};
	_  -> {'MANDATORY',Tokens}
    end.

parse_ValueSetOptionalitySpec(Tokens) ->
    case Tokens of
	[{'OPTIONAL',_}|Rest] -> {'OPTIONAL',Rest};
	[{'DEFAULT',_}|Rest] ->
	    {ValueSet,Rest2} = parse_ValueSet(Rest),
	    {{'DEFAULT',ValueSet},Rest2};
	_  -> {'MANDATORY',Tokens}
    end.

parse_ObjectSetOptionalitySpec(Tokens) ->
        case Tokens of
	[{'OPTIONAL',_}|Rest] -> {'OPTIONAL',Rest};
	[{'DEFAULT',_}|Rest] ->
	    {ObjectSet,Rest2} = parse_ObjectSet(Rest),
	    {{'DEFAULT',ObjectSet},Rest2};
	_  -> {'MANDATORY',Tokens}
    end.

parse_WithSyntaxSpec([{'WITH',_},{'SYNTAX',_}|Rest]) ->
    {SyntaxList,Rest2} = parse_SyntaxList(Rest),
    {{'WITH SYNTAX',SyntaxList},Rest2};
parse_WithSyntaxSpec(Tokens) ->
    {[],Tokens}.

parse_SyntaxList([{'{',_},{'}',_}|Rest]) ->
    {[],Rest};
parse_SyntaxList([{'{',_}|Rest]) ->
    parse_SyntaxList(Rest,[]);
parse_SyntaxList(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,['{}','{']]}}).

parse_SyntaxList(Tokens,Acc) ->
    {SyntaxList,Rest} = parse_TokenOrGroupSpec(Tokens),
    case Rest of
	[{'}',_}|Rest2] -> 
	    {lists:reverse([SyntaxList|Acc]),Rest2};
	_ ->
	    parse_SyntaxList(Rest,[SyntaxList|Acc])
    end.

parse_TokenOrGroupSpec(Tokens) ->
    Flist = [fun parse_RequiredToken/1,
	     fun parse_OptionalGroup/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_RequiredToken([{typereference,L1,WordName}|Rest]) ->
    case is_word(WordName) of
	false ->
	    throw({asn1_error,{L1,get(asn1_module),
			       [got,WordName,expected,a,'Word']}});
	true ->
	    {WordName,Rest}
    end;
parse_RequiredToken([{',',L1}|Rest]) ->
    {{',',L1},Rest};
parse_RequiredToken([{WordName,L1}|Rest]) ->
    case is_word(WordName) of
	false ->
	    throw({asn1_error,{L1,get(asn1_module),
			       [got,WordName,expected,a,'Word']}});
	true ->
	    {WordName,Rest}
    end;
parse_RequiredToken(Tokens) ->
    parse_PrimitiveFieldName(Tokens).

parse_OptionalGroup([{'[',_}|Rest]) ->
    {Spec,Rest2} = parse_TokenOrGroupSpec(Rest),
    {SpecList,Rest3} = parse_OptionalGroup(Rest2,[Spec]),
    {SpecList,Rest3}.

parse_OptionalGroup([{']',_}|Rest],Acc) ->
    {lists:reverse(Acc),Rest};
parse_OptionalGroup(Tokens,Acc) ->
    {Spec,Rest} = parse_TokenOrGroupSpec(Tokens),
    parse_OptionalGroup(Rest,[Spec|Acc]).

parse_DefinedObject([Id={identifier,_,_ObjName}|Rest]) ->
    {{object,identifier2Extvalueref(Id)},Rest};
parse_DefinedObject([{typereference,L1,ModName},{'.',_},{identifier,_,ObjName}|Rest]) ->
    {{object, #'Externaltypereference'{pos=L1,module=ModName,type=ObjName}},Rest};
parse_DefinedObject(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
		       [identifier,'typereference.identifier']]}}).

parse_ObjectAssignment([{identifier,L1,ObjName}|Rest]) ->
    {Class,Rest2} = parse_DefinedObjectClass(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Object,Rest4} = parse_Object(Rest3),
	    {#typedef{pos=L1,name=ObjName,
		      typespec=#'Object'{classname=Class,def=Object}},Rest4};
	[H|_T]  -> 
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}});
	Other ->
	    throw({asn1_error,{L1,get(asn1_module),
			       [got,Other,expected,'::=']}})
    end;
parse_ObjectAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,identifier]}}).


%% parse_Object(Tokens) -> Ret
%% Tokens    = [Tok]
%% Tok       = tuple()
%% Ret       = {object,_} | {object, _, _}
parse_Object(Tokens) ->
    Flist=[fun parse_ObjectDefn/1,
	   fun parse_ObjectFromObject/1,
	   fun parse_ParameterizedObject/1,
	   fun parse_DefinedObject/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_ObjectDefn(Tokens) ->
    Flist=[fun parse_DefaultSyntax/1,
	   fun parse_DefinedSyntax/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_DefaultSyntax([{'{',_},{'}',_}|Rest]) ->
    {{object,defaultsyntax,[]},Rest};
parse_DefaultSyntax([{'{',_}|Rest]) ->
    parse_DefaultSyntax(Rest,[]);
parse_DefaultSyntax(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,['{}','{']]}}).

parse_DefaultSyntax(Tokens,Acc) ->
    {Setting,Rest} = parse_FieldSetting(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_DefaultSyntax(Rest2,[Setting|Acc]);
	[{'}',_}|Rest3] ->
	    {{object,defaultsyntax,lists:reverse([Setting|Acc])},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,[',','}']]}})
    end.

parse_FieldSetting(Tokens) ->
    {{_,PrimFieldName},Rest} = parse_PrimitiveFieldName(Tokens),
    {Setting,Rest2} = parse_Setting(Rest),
    {{PrimFieldName,Setting},Rest2}.

parse_DefinedSyntax([{'{',_}|Rest]) ->
    parse_DefinedSyntax(Rest,[]).

parse_DefinedSyntax(Tokens,Acc) ->
    case Tokens of
	[{'}',_}|Rest2] ->
	    {{object,definedsyntax,lists:reverse(Acc)},Rest2};
	_ ->
	    {DefSynTok,Rest3} = parse_DefinedSyntaxToken(Tokens),
	    parse_DefinedSyntax(Rest3,[DefSynTok|Acc])
    end.


%% DefinedSyntaxToken ::= Literal | Setting
%% Literal ::= word | ','
%% Setting ::= Type | Value | ValueSet | Object | ObjectSet
%% word equals typereference, but no lower cases
parse_DefinedSyntaxToken([{',',L1}|Rest]) ->
    {{',',L1},Rest};
%% ObjectClassFieldType or a defined type with a constraint.
%% Should also be able to parse a parameterized type. It may be
%% impossible to distinguish between a parameterized type and a Literal
%% followed by an object set.
parse_DefinedSyntaxToken(Tokens=[{typereference,L1,_Name},{T,_}|_Rest]) 
  when T == '.'; T == '(' ->
    case catch parse_Setting(Tokens) of
	{asn1_error,_} ->
	    throw({asn1_error,{L1,get(asn1_module),
			       [got,hd(Tokens), expected,['Word',setting]]}});
	{'EXIT',Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end;
parse_DefinedSyntaxToken(Tokens=[TRef={typereference,L1,Name}|Rest]) ->
    case is_word(Name) of
	false ->
	    case lookahead_definedsyntax(Rest) of
		word_or_setting ->
		    {{setting,L1,tref2Exttref(TRef)},Rest};
		_ ->
		    parse_Setting(Tokens)
	    end;
	true ->
	    %% {{word_or_setting,L1,Name},Rest}
	    {{word_or_setting,L1,tref2Exttref(TRef)},Rest}
    end;
parse_DefinedSyntaxToken(Tokens) ->
    case catch parse_Setting(Tokens) of
	{asn1_error,_} ->
	    parse_Word(Tokens);
	{'EXIT',Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end.

lookahead_definedsyntax([{typereference,_,Name}|_Rest]) ->
    case is_word(Name)  of
	true -> word_or_setting;
	_ -> setting
    end;
lookahead_definedsyntax([{'}',_}|_Rest]) ->
    word_or_setting;
lookahead_definedsyntax(_) ->
    setting.
	    
parse_Word([{Name,Pos}|Rest]) ->
    case is_word(Name) of
	false ->
	    throw({asn1_error,{Pos,get(asn1_module),
			       [got,Name, expected,a,'Word']}});
	true ->
	    {{word_or_setting,Pos,tref2Exttref(Pos,Name)},Rest}
    end.

parse_Setting(Tokens) ->
    Flist = [{type_tag,fun parse_Type/1},
	     {value_tag,fun parse_Value/1},
	     {object_tag,fun parse_Object/1},
	     {objectset_tag,fun parse_ObjectSet/1}],
    case (catch parse_or_tag(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result = {{value_tag,_},_} ->
	    Result;
	{{Tag,Setting},Rest} when is_atom(Tag) ->
	    {Setting,Rest}
    end.

%% parse_Setting(Tokens) ->
%%     Flist = [fun parse_Type/1,
%% 	     fun parse_Value/1,
%% 	     fun parse_Object/1,
%% 	     fun parse_ObjectSet/1],
%%     case (catch parse_or(Tokens,Flist)) of
%% 	{'EXIT',Reason} ->
%% 	    exit(Reason);
%% 	AsnErr = {asn1_error,_} ->
%% 	    throw(AsnErr);
%% 	Result ->
%% 	    Result
%%     end.

parse_DefinedObjectSet([{typereference,L1,ModuleName},{'.',_},
			{typereference,L2,ObjSetName}|Rest]) ->
    {{objectset,L1,#'Externaltypereference'{pos=L2,module=ModuleName,
					    type=ObjSetName}},Rest};
parse_DefinedObjectSet([{typereference,L1,ObjSetName}|Rest]) ->
    {{objectset,L1,#'Externaltypereference'{pos=L1,module=get(asn1_module),
					    type=ObjSetName}},Rest};
parse_DefinedObjectSet(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[typereference,'typereference.typereference']]}}).

parse_ObjectSetAssignment([{typereference,L1,ObjSetName}|Rest]) ->
    {Class,Rest2} = parse_DefinedObjectClass(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {ObjectSet,Rest4} = parse_ObjectSet(Rest3),
	    {#typedef{pos=L1,name=ObjSetName,
		      typespec=#'ObjectSet'{class=Class,
					    set=ObjectSet}},Rest4};
	[H|_T]  -> 
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
%%%	Other ->
%%%	    throw(Other)
    end;
parse_ObjectSetAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   typereference]}}).

%% parse_ObjectSet(Tokens) -> {Ret,Rest}
%% Tokens    = [Tok]
%% Tok       = tuple()
%% Ret       = {[],tuple()} | 
%%             {list(),list()} | 
%%             list() | 
%%             ['EXTENSIONMARK'] |
%%             {'ALL',{'EXCEPT',Exclusions}} |
%%             {'SingleValue',SV}
%% SV        = list() | #'Externalvaluereference'{} | {definedvalue,term()}
parse_ObjectSet([{'{',_}|Rest]) ->
    {ObjSetSpec,Rest2} = parse_ObjectSetSpec(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {ObjSetSpec,Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'}']}})
    end;
parse_ObjectSet(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_ObjectSetSpec([{'...',_}|Rest]) ->
    case Rest of
	[{',',_}|Rest2] ->
	    {Elements,Rest3}=parse_ElementSetSpecs(Rest2),
	    {{[],Elements},Rest3};
	_ ->
	    {['EXTENSIONMARK'],Rest}
    end;
parse_ObjectSetSpec(Tokens) ->
    parse_ElementSetSpecs(Tokens).

% moved fun parse_Object/1 and fun parse_DefinedObjectSet/1 to parse_Elements
%% parse_ObjectSetElements(Tokens) -> {Result,Rest}
%% Result ::= {'ObjectSetFromObjects',Objects,Name} | {pos,ObjectSet,Params}
%% Objects ::= ReferencedObjects
%% ReferencedObjects ::= (see parse_ReferencedObjects/1)
%% Name ::= [FieldName]
%% FieldName ::= {typefieldreference,atom()} | {valuefieldreference,atom()}
%% ObjectSet ::= {objectset,integer(),#'Externaltypereference'{}}
%% Params ::= list() (see parse_ActualParameterList/1)
parse_ObjectSetElements(Tokens) ->
    Flist = [%fun parse_Object/1,
	     %fun parse_DefinedObjectSet/1,
	     fun parse_ObjectSetFromObjects/1,
	     fun parse_ParameterizedObjectSet/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_ObjectClassFieldType(Tokens) ->
    {Class,Rest} = parse_DefinedObjectClass(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {FieldName,Rest3} = parse_FieldName(Rest2),
	    OCFT = #'ObjectClassFieldType'{
	      classname=Class,
	      class=Class,fieldname=FieldName},
	    {#type{def=OCFT},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'.']}})
%%%	Other ->
%%%	    throw(Other)
    end.

%parse_ObjectClassFieldValue(Tokens) ->
%    Flist = [fun parse_OpenTypeFieldVal/1,
%	     fun parse_FixedTypeFieldVal/1],
%    case (catch parse_or(Tokens,Flist)) of
%	{'EXIT',Reason} ->
%	    throw(Reason);
%	AsnErr = {asn1_error,_} ->
%	    throw(AsnErr);
%	Result ->
%	    Result
%    end.

parse_ObjectClassFieldValue(Tokens) ->
    parse_OpenTypeFieldVal(Tokens).

parse_OpenTypeFieldVal(Tokens) ->
    {Type,Rest} = parse_Type(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Value,Rest3} = parse_Value(Rest2),
	    {{opentypefieldvalue,Type,Value},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,':']}})
    end.

% parse_FixedTypeFieldVal(Tokens) ->
%     parse_Value(Tokens).

% parse_InformationFromObjects(Tokens) ->
%     Flist = [fun parse_ValueFromObject/1,
% 	     fun parse_ValueSetFromObjects/1,
% 	     fun parse_TypeFromObject/1,
% 	     fun parse_ObjectFromObject/1],
%     case (catch parse_or(Tokens,Flist)) of
% 	{'EXIT',Reason} ->
% 	    throw(Reason);
% 	AsnErr = {asn1_error,_} ->
% 	    throw(AsnErr);
% 	Result ->
% 	    Result
%     end.

%% parse_ReferencedObjects(Tokens) -> {Result,Rest}
%% Result    ::= DefObject | DefObjSet |
%%               {po,DefObject,Params} | {pos,DefObjSet,Params} |
%%            
%% DefObject ::= {object,#'Externaltypereference'{}} |
%%               {object,#'Externalvaluereference'{}}
%% DefObjSet ::= {objectset,integer(),#'Externaltypereference'{}}
%% Params    ::= list()
parse_ReferencedObjects(Tokens) ->
    Flist = [fun parse_DefinedObject/1,
	     fun parse_DefinedObjectSet/1,
	     fun parse_ParameterizedObject/1,
	     fun parse_ParameterizedObjectSet/1],
        case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_ValueFromObject(Tokens) ->
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    case lists:last(Name) of
		{valuefieldreference,_} ->
		    {{'ValueFromObject',Objects,Name},Rest3};
		_ ->
		    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
				       [got,typefieldreference,expected,
					valuefieldreference]}})
	    end;
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'.']}})
%%%	Other ->
%%%	    throw({asn1_error,{got,Other,expected,'.'}})
    end.

parse_ValueSetFromObjects(Tokens) ->
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    case lists:last(Name) of
		{typefieldreference,_FieldName} ->
		    {{'ValueSetFromObjects',Objects,Name},Rest3};
		_ ->
		    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
				       [got,get_token(hd(Rest2)),expected,
					typefieldreference]}})
	    end;
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'.']}})
%%%	Other ->
%%%	    throw({asn1_error,{got,Other,expected,'.'}})
    end.

parse_TypeFromObject(Tokens) ->
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    case lists:last(Name) of
		{typefieldreference,_FieldName} ->
		    {{'TypeFromObject',Objects,Name},Rest3};
		_ ->
		    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
				       [got,get_token(hd(Rest2)),expected,
					typefieldreference]}})
	    end;
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'.']}})
%%%	Other ->
%%%	    throw({asn1_error,{got,Other,expected,'.'}})
    end.

parse_ObjectFromObject(Tokens) ->
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    {{'ObjectFromObject',Objects,Name},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'.']}})
    end.

%% parse_ObjectSetFromObjects(Tokens) -> {Result,Rest}
%% Result  ::= {'ObjectSetFromObjects',Objects,Name}
%% Objects ::= ReferencedObject (see parse_ReferencedObjects/1)
%% Name    ::= [FieldName]
%% FieldName ::= {typefieldreference,atom()} |
%%               {valuefieldreference,atom()}
parse_ObjectSetFromObjects(Tokens) ->
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    case lists:last(Name) of
		{typefieldreference,_FieldName} ->
		    {{'ObjectSetFromObjects',Objects,Name},Rest3};
		_ ->
		    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
				       [got,get_token(hd(Rest2)),expected,
					typefieldreference]}})
	    end;
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'.']}})
    end.

% parse_InstanceOfType([{'INSTANCE',_},{'OF',_}|Rest]) ->
%     {Class,Rest2} = parse_DefinedObjectClass(Rest),
%     {{'InstanceOfType',Class},Rest2}.

% parse_InstanceOfValue(Tokens) ->
%     parse_Value(Tokens).



%% X.682 constraint specification

parse_GeneralConstraint(Tokens) ->
    Flist = [fun parse_UserDefinedConstraint/1,
	     fun parse_TableConstraint/1,
	     fun parse_ContentsConstraint/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_UserDefinedConstraint([{'CONSTRAINED',_},{'BY',_},{'{',_},{'}',_}|Rest])->
    {{constrained_by,[]},Rest};
parse_UserDefinedConstraint([{'CONSTRAINED',_},
			     {'BY',_},
			     {'{',_}|Rest]) ->
    {Param,Rest2} = parse_UserDefinedConstraintParameter(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {{constrained_by,Param},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'}']}})
    end;
parse_UserDefinedConstraint(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			['CONSTRAINED BY {}','CONSTRAINED BY {']]}}).

parse_UserDefinedConstraintParameter(Tokens) ->
    parse_UserDefinedConstraintParameter(Tokens,[]).
parse_UserDefinedConstraintParameter(Tokens,Acc) ->
    Flist = [fun parse_GovernorAndActualParameter/1,
	     fun parse_ActualParameter/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	{Result,Rest} ->
	    case Rest of
		[{',',_}|_Rest2] ->
		    parse_UserDefinedConstraintParameter(Tokens,[Result|Acc]);
		_  ->
		    {lists:reverse([Result|Acc]),Rest}
	    end
    end.

parse_GovernorAndActualParameter(Tokens) ->
    {Governor,Rest} = parse_Governor(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Params,Rest3} = parse_ActualParameter(Rest2),
	    {{'Governor_Params',Governor,Params},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,':']}})
    end.

parse_TableConstraint(Tokens) ->
    Flist = [fun parse_ComponentRelationConstraint/1,
	     fun parse_SimpleTableConstraint/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_SimpleTableConstraint(Tokens) ->
    {ObjectSet,Rest} = parse_ObjectSet(Tokens),
    {{simpletable,ObjectSet},Rest}.

parse_ComponentRelationConstraint([{'{',_}|Rest]) ->
    {ObjectSet,Rest2} = parse_DefinedObjectSet(Rest),
    case Rest2 of
	[{'}',_},{'{',_}|Rest3] ->
	    {AtNot,Rest4} = parse_AtNotationList(Rest3,[]),
	    case Rest4 of
		[{'}',_}|Rest5] ->
		    {{componentrelation,ObjectSet,AtNot},Rest5};
		[H|_T]  ->
		    throw({asn1_error,{get_line(H),get(asn1_module),
				       [got,get_token(H),expected,'}']}})
	    end;
	[H|_T]  ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,
				'ComponentRelationConstraint',ended,with,'}']}})
%%%	Other ->
%%%	    throw(Other)
    end;
parse_ComponentRelationConstraint(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_AtNotationList(Tokens,Acc) ->
    {AtNot,Rest} = parse_AtNotation(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_AtNotationList(Rest2,[AtNot|Acc]);
	_  ->
	    {lists:reverse([AtNot|Acc]),Rest}
    end.

parse_AtNotation([{'@',_},{'.',_}|Rest]) ->
    {CIdList,Rest2} = parse_ComponentIdList(Rest),
    {{innermost,CIdList},Rest2};
parse_AtNotation([{'@',_}|Rest]) ->
    {CIdList,Rest2} = parse_ComponentIdList(Rest),
    {{outermost,CIdList},Rest2};
parse_AtNotation(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,['@','@.']]}}).

parse_ComponentIdList(Tokens) ->
    parse_ComponentIdList(Tokens,[]).

parse_ComponentIdList([Id = {identifier,_,_},{'.',_}|Rest],Acc) ->
    parse_ComponentIdList(Rest,[identifier2Extvalueref(Id)|Acc]);
parse_ComponentIdList([Id = {identifier,_,_}|Rest],Acc) ->
    {lists:reverse([identifier2Extvalueref(Id)|Acc]),Rest};
parse_ComponentIdList(Tokens,_) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[identifier,'identifier.']]}}).

parse_ContentsConstraint([{'CONTAINING',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    case Rest2 of
	[{'ENCODED',_},{'BY',_}|Rest3] ->
	    {Value,Rest4} = parse_Value(Rest3),
	    {{contentsconstraint,Type,Value},Rest4};
	_ ->
	    {{contentsconstraint,Type,[]},Rest2}
    end;
parse_ContentsConstraint([{'ENCODED',_},{'BY',_}|Rest]) ->
    {Value,Rest2} = parse_Value(Rest),
    {{contentsconstraint,[],Value},Rest2};
parse_ContentsConstraint(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			'CONTAINING','or','ENCODED BY']}}).


% X.683 Parameterization of ASN.1 specifications

parse_Governor(Tokens) ->
    Flist = [fun parse_Type/1,
	     fun parse_DefinedObjectClass/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_ActualParameter(Tokens) ->
    Flist = [fun parse_Type/1,
	     fun parse_Value/1,
	     fun parse_ValueSet/1,
	     fun parse_DefinedObjectClass/1,
	     fun parse_Object/1,
	     fun parse_ObjectSet/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_ParameterizedAssignment(Tokens) ->
    Flist = [fun parse_ParameterizedTypeAssignment/1,
	     fun parse_ParameterizedValueAssignment/1,
	     fun parse_ParameterizedValueSetTypeAssignment/1,
	     fun parse_ParameterizedObjectClassAssignment/1,
	     fun parse_ParameterizedObjectAssignment/1,
	     fun parse_ParameterizedObjectSetAssignment/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	AsnAssErr = {asn1_assignment_error,_} ->
	    throw(AsnAssErr);
	Result ->
	    Result
    end.

%% parse_ParameterizedTypeAssignment(Tokens) -> Result
%% Result = {#ptypedef{},Rest} | throw()
parse_ParameterizedTypeAssignment([{typereference,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Type,Rest4} = parse_Type(Rest3),
	    {#ptypedef{pos=L1,name=Name,args=ParameterList,typespec=Type},
	     Rest4};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
    end;
parse_ParameterizedTypeAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   typereference]}}).

%% parse_ParameterizedValueAssignment(Tokens) -> Result
%% Result = {#pvaluedef{},Rest} | throw()
parse_ParameterizedValueAssignment([{identifier,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    {Type,Rest3} = parse_Type(Rest2),
    case Rest3 of
	[{'::=',_}|Rest4] ->
	    {Value,Rest5} = parse_Value(Rest4),
	    {#pvaluedef{pos=L1,name=Name,args=ParameterList,type=Type,
			 value=Value},Rest5};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
    end;
parse_ParameterizedValueAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,identifier]}}).

%% parse_ParameterizedValueSetTypeAssignment(Tokens) -> Result
%% Result = {#pvaluesetdef{},Rest} | throw()
parse_ParameterizedValueSetTypeAssignment([{typereference,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    {Type,Rest3} = parse_Type(Rest2),
    case Rest3 of
	[{'::=',_}|Rest4] ->
	    {ValueSet,Rest5} = parse_ValueSet(Rest4),
	    {#pvaluesetdef{pos=L1,name=Name,args=ParameterList,
			   type=Type,valueset=ValueSet},Rest5};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
    end;
parse_ParameterizedValueSetTypeAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   typereference]}}).

%% parse_ParameterizedObjectClassAssignment(Tokens) -> Result
%% Result = {#ptypedef{},Rest} | throw()
parse_ParameterizedObjectClassAssignment([{typereference,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Class,Rest4} = parse_ObjectClass(Rest3),
	    {#ptypedef{pos=L1,name=Name,args=ParameterList,typespec=Class},
	     Rest4};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
    end;
parse_ParameterizedObjectClassAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   typereference]}}).

%% parse_ParameterizedObjectAssignment(Tokens) -> Result
%% Result = {#pobjectdef{},Rest} | throw()
parse_ParameterizedObjectAssignment([{identifier,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    {Class,Rest3} = parse_DefinedObjectClass(Rest2),
    case Rest3 of
	[{'::=',_}|Rest4] ->
	    {Object,Rest5} = parse_Object(Rest4),
	    {#pobjectdef{pos=L1,name=Name,args=ParameterList,
			 class=Class,def=Object},Rest5};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
%%%	Other ->
%%%	    throw(Other)
    end;
parse_ParameterizedObjectAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,identifier]}}).

%% parse_ParameterizedObjectSetAssignment(Tokens) -> Result
%% Result = {#pobjectsetdef{},Rest} | throw{}
parse_ParameterizedObjectSetAssignment([{typereference,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    {Class,Rest3} = parse_DefinedObjectClass(Rest2),
    case Rest3 of
	[{'::=',_}|Rest4] ->
	    {ObjectSet,Rest5} = parse_ObjectSet(Rest4),
	    {#pobjectsetdef{pos=L1,name=Name,args=ParameterList,
			    class=Class,def=ObjectSet},Rest5};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
%%%	Other ->
%%%	    throw(Other)
    end;
parse_ParameterizedObjectSetAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   typereference]}}).

%% parse_ParameterList(Tokens) -> Result
%% Result = [Parameter]
%% Parameter = {Governor,Reference} | Reference
%% Governor = Type | DefinedObjectClass
%% Type = #type{}
%% DefinedObjectClass = #'Externaltypereference'{} | 
%%                      'ABSTRACT-SYNTAX' | 'TYPE-IDENTIFIER'
%% Reference = #'Externaltypereference'{} | #'Externalvaluereference'{}
parse_ParameterList([{'{',_}|Rest]) ->
    parse_ParameterList(Rest,[]);
parse_ParameterList(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_ParameterList(Tokens,Acc) ->
    {Parameter,Rest} = parse_Parameter(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_ParameterList(Rest2,[Parameter|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse([Parameter|Acc]),Rest3};
	[H|_T]  ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,[',','}']]}})
    end.

parse_Parameter(Tokens) ->
    Flist = [fun parse_ParamGovAndRef/1,
	     fun parse_Reference/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_ParamGovAndRef(Tokens) ->
    {ParamGov,Rest} = parse_ParamGovernor(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Ref,Rest3} = parse_Reference(Rest2),
	    {{ParamGov,Ref},Rest3};
	[H|_T] ->
	   throw({asn1_error,{get_line(H),get(asn1_module),
			      [got,get_token(H),expected,':']}})
    end.

parse_ParamGovernor(Tokens) ->
    Flist = [fun parse_Governor/1,
	     fun parse_Reference/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

% parse_ParameterizedReference(Tokens) ->
%     {Ref,Rest} = parse_Reference(Tokens),
%     case Rest of
% 	[{'{',_},{'}',_}|Rest2] ->
% 	    {{ptref,Ref},Rest2};
% 	_  ->
% 	    {{ptref,Ref},Rest}
%     end.

parse_SimpleDefinedType([{typereference,L1,ModuleName},{'.',_},
			 {typereference,_,TypeName}|Rest]) ->
    {#'Externaltypereference'{pos=L1,module=ModuleName,
						 type=TypeName},Rest};
parse_SimpleDefinedType([Tref={typereference,_,_}|Rest]) ->
%    {#'Externaltypereference'{pos=L2,module=get(asn1_module),
%						 type=TypeName},Rest};
    {tref2Exttref(Tref),Rest};
parse_SimpleDefinedType(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[typereference,'typereference.typereference']]}}).

parse_SimpleDefinedValue([{typereference,L1,ModuleName},{'.',_},
			  {identifier,_,Value}|Rest]) ->
    {{simpledefinedvalue,#'Externalvaluereference'{pos=L1,module=ModuleName,
						   value=Value}},Rest};
parse_SimpleDefinedValue([Id={identifier,_,_Value}|Rest]) ->
    {{simpledefinedvalue,identifier2Extvalueref(Id)},Rest};
parse_SimpleDefinedValue(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			['typereference.identifier',identifier]]}}).

parse_ParameterizedType(Tokens) ->
    {Type,Rest} = parse_SimpleDefinedType(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{pt,Type,Params},Rest2}.

parse_ParameterizedValue(Tokens) ->
    {Value,Rest} = parse_SimpleDefinedValue(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{pv,Value,Params},Rest2}.

parse_ParameterizedObjectClass(Tokens) ->
    {Type,Rest} = parse_DefinedObjectClass(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{poc,Type,Params},Rest2}.

parse_ParameterizedObjectSet(Tokens) ->
    {ObjectSet,Rest} = parse_DefinedObjectSet(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{pos,ObjectSet,Params},Rest2}.

parse_ParameterizedObject(Tokens) ->
    {Object,Rest} = parse_DefinedObject(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{po,Object,Params},Rest2}.

parse_ActualParameterList([{'{',_}|Rest]) ->
    parse_ActualParameterList(Rest,[]);
parse_ActualParameterList(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_ActualParameterList(Tokens,Acc) ->
    {Parameter,Rest} = parse_ActualParameter(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_ActualParameterList(Rest2,[Parameter|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse([Parameter|Acc]),Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,[',','}']]}})
%%%	Other ->
%%%	    throw(Other)
    end.







%-------------------------

is_word(Token) ->
    case not_allowed_word(Token) of
	true -> false;
	_ ->
	    if
		is_atom(Token) ->
		    Item = atom_to_list(Token),
		    is_word(Item);
		is_list(Token), length(Token) == 1 ->
		    check_one_char_word(Token);
		is_list(Token) ->
		    [A|Rest] = Token,
		    case check_first(A) of
			true ->
			    check_rest(Rest);
			_ -> 
			    false
		    end
	    end
    end.

not_allowed_word(Name) ->
    lists:member(Name,["BIT",
		       "BOOLEAN",
		       "CHARACTER",
		       "CHOICE",
		       "EMBEDDED",
		       "END",
		       "ENUMERATED",
		       "EXTERNAL",
		       "FALSE",
		       "INSTANCE",
		       "INTEGER",
		       "INTERSECTION",
		       "MINUS-INFINITY",
		       "NULL",
		       "OBJECT",
		       "OCTET",
		       "PLUS-INFINITY",
		       "REAL",
		       "SEQUENCE",
		       "SET",
		       "TRUE",
		       "UNION"]).

check_one_char_word([A]) when $A =< A, $Z >= A ->
    true;
check_one_char_word([_]) ->
    false. %% unknown item in SyntaxList

check_first(A) when $A =< A, $Z >= A ->
    true;
check_first(_) ->
    false. %% unknown item in SyntaxList

check_rest([R,R|_Rs]) when $- == R ->
    false; %% two consecutive hyphens are not allowed in a word
check_rest([R]) when $- == R ->
    false; %% word cannot end with hyphen
check_rest([R|Rs]) when $A=<R, $Z>=R; $-==R ->
    check_rest(Rs);
check_rest([]) ->
    true;
check_rest(_) ->
    false.


to_set(V) when is_list(V) ->
	ordsets:from_list(V);
to_set(V) ->
	ordsets:from_list([V]).

parse_AlternativeTypeLists(Tokens) ->
    parse_AlternativeTypeLists(Tokens,[]).

parse_AlternativeTypeLists(Tokens = [{identifier,_,_}|_Rest0],Clist) ->
    {CompList,Rest1} = parse_AlternativeTypeList(Tokens,[]),
    parse_AlternativeTypeLists(Rest1,Clist++CompList);
parse_AlternativeTypeLists([{'...',L1},{'!',_}|Rest02],Clist0) ->
    {_,Rest03} = parse_ExceptionIdentification(Rest02),
    %% Exception info is currently thrown away
    parse_AlternativeTypeLists2(Rest03,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_AlternativeTypeLists([{',',L1},{'...',_},{'!',_}|Rest02],Clist0) when Clist0 =/= []->
    {_,Rest03} = parse_ExceptionIdentification(Rest02),
    %% Exception info is currently thrown away
    parse_AlternativeTypeLists2(Rest03,Clist0++[#'EXTENSIONMARK'{pos=L1}]);

parse_AlternativeTypeLists([{',',_},{'...',L1}|Rest02],Clist0) when Clist0 =/= []->
    parse_AlternativeTypeLists2(Rest02,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_AlternativeTypeLists([{'...',L1}|Rest02],Clist0) ->
    parse_AlternativeTypeLists2(Rest02,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_AlternativeTypeLists(Tokens = [{'}',_L1}|_Rest02],Clist0) ->
    {Clist0,Tokens}.

parse_AlternativeTypeLists2(Tokens,Clist) ->
    {ExtAdd,Rest} = parse_ExtensionAdditionAlternatives(Tokens,Clist),
    {Clist2,Rest2} = parse_OptionalExtensionMarker(Rest,lists:flatten(ExtAdd)),
    case Rest2 of
	[{',',_}|Rest3] ->
	    {CompList,Rest4} = parse_AlternativeTypeList(Rest3,[]),
	    {Clist2 ++ CompList,Rest4};
	_ ->
	    {Clist2,Rest2}
    end.



parse_AlternativeTypeList([{',',_},Id = {identifier,_,_}|Rest],Acc) when Acc =/= [] ->
    {AlternativeType,Rest2} = parse_NamedType([Id|Rest]),
    parse_AlternativeTypeList(Rest2,[AlternativeType|Acc]);
parse_AlternativeTypeList(Tokens = [{'}',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AlternativeTypeList(Tokens = [{']',_},{']',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AlternativeTypeList(Tokens = [{',',_},{'...',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AlternativeTypeList(Tokens,[]) ->
    {AlternativeType,Rest} = parse_NamedType(Tokens),
    parse_AlternativeTypeList(Rest,[AlternativeType]);
parse_AlternativeTypeList(Tokens,_) ->
    throw({asn1_error,
	   {get_line(hd(Tokens)),get(asn1_module),
	    [got,[get_token(hd(Tokens)),get_token(hd(tl(Tokens)))],
	     expected,['}',', identifier']]}}).

parse_ExtensionAdditionAlternatives(Tokens =[{',',_}|_],Clist) ->
    {ExtAddList,Rest2} = parse_ExtensionAdditionAlternativesList(Tokens,[]),
    {Clist++lists:flatten(ExtAddList),Rest2};
parse_ExtensionAdditionAlternatives(Tokens,Clist) ->
    %% Empty
    {Clist,Tokens}.

parse_ExtensionAdditionAlternativesList([{',',_},Id = {identifier,_,_}|Rest],Acc) ->
    {AlternativeType,Rest2} = parse_NamedType([Id|Rest]),
    parse_ExtensionAdditionAlternativesList(Rest2,[AlternativeType|Acc]);
parse_ExtensionAdditionAlternativesList([{',',_},C1 = {'[',_},C2 = {'[',_}|Rest],Acc) ->
    {ExtAddGroup,Rest2} = parse_ExtensionAdditionAlternativesGroup([C1,C2|Rest],[]),
    parse_ExtensionAdditionAlternativesList(Rest2,[ExtAddGroup|Acc]);
parse_ExtensionAdditionAlternativesList(Tokens = [{'}',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ExtensionAdditionAlternativesList(Tokens = [{',',_},{'...',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ExtensionAdditionAlternativesList(Tokens,_) ->
    throw({asn1_error,
	   {get_line(hd(Tokens)),get(asn1_module),
	    [got,[get_token(hd(Tokens)),get_token(hd(tl(Tokens)))],
	     expected,['}',', identifier']]}}).


parse_ExtensionAdditionAlternativesGroup([ {'[',_},{'[',_},_VsnNr = {number,_,Num},{':',_}|Rest],[]) ->
    parse_ExtensionAdditionAlternativesGroup2(Rest,Num);
parse_ExtensionAdditionAlternativesGroup([ {'[',_},{'[',_}|Rest],[]) ->
    parse_ExtensionAdditionAlternativesGroup2(Rest,undefined);
parse_ExtensionAdditionAlternativesGroup(Tokens,_) ->
    throw({asn1_error,
	   {get_line(hd(Tokens)),get(asn1_module),
	    [got,[get_token(hd(Tokens)),get_token(hd(tl(Tokens)))],
	     expected,['[[']]}}).


parse_ExtensionAdditionAlternativesGroup2(Tokens,Num) ->
    {CompTypeList,Rest} = parse_AlternativeTypeList(Tokens,[]),
    case Rest of
	[{']',_},{']',_}|Rest2] ->
	    {[{'ExtensionAdditionGroup',Num}|CompTypeList] ++
		['ExtensionAdditionGroupEnd'],Rest2};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest)),get(asn1_module),
			       [got,get_token(hd(Rest)),expected,[']]']]}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse_AlternativeTypeLists(Tokens,ExtensionDefault) ->
%%     {AltTypeList,Rest1} = parse_AlternativeTypeList(Tokens),
%%     {ExtensionAndException,Rest2} =
%% 	case Rest1 of
%% 	    [{',',_},{'...',L1},{'!',_}|Rest12] ->
%% 		{_,Rest13} = parse_ExceptionIdentification(Rest12),
%% 		%% Exception info is currently thrown away
%% 		{[#'EXTENSIONMARK'{pos=L1}],Rest13};
%% 	    [{',',_},{'...',L1}|Rest12] ->
%% 		{[#'EXTENSIONMARK'{pos=L1}],Rest12};
%% 	    _ ->
%% 		{[],Rest1}
%% 	end,
%%     {AltTypeList2,Rest5} =
%% 	case ExtensionAndException of
%% 	    [] ->
%% 		{AltTypeList,Rest2};
%% 	    _ ->
%% 		{ExtensionAddition,Rest3} =
%% 		    case Rest2 of
%% 			[{',',_}|Rest23] ->
%% 			    parse_ExtensionAdditionAlternativeList(Rest23);
%% 			_ ->
%% 			    {[],Rest2}
%% 		    end,
%% 		{OptionalExtensionMarker,Rest4} =
%% 		    case Rest3 of
%% 			[{',',_},{'...',L3}|Rest31] ->
%% 			    {[#'EXTENSIONMARK'{pos=L3}],Rest31};
%% 			_ ->
%% 			    {[],Rest3}
%% 		    end,
%% 		{AltTypeList ++ ExtensionAndException ++
%% 		 ExtensionAddition ++ OptionalExtensionMarker, Rest4}
%% 	end,
%%     AltTypeList3 =
%% 	case [X || X=#'EXTENSIONMARK'{} <- AltTypeList2] of
%% 	    [] when ExtensionDefault == 'IMPLIED' ->
%% 		AltTypeList2 ++ [#'EXTENSIONMARK'{}];
%% 	    _ ->
%% 		AltTypeList2
%% 	end,
%%     {AltTypeList3,Rest5}.
    

%% parse_AlternativeTypeList(Tokens) ->
%%     parse_AlternativeTypeList(Tokens,[]).

%% parse_AlternativeTypeList(Tokens,Acc) ->
%%     {NamedType,Rest} = parse_NamedType(Tokens),
%%     case Rest of
%% 	[{',',_},Id = {identifier,_,_}|Rest2] ->
%% 	    parse_AlternativeTypeList([Id|Rest2],[NamedType|Acc]);
%% 	_ ->
%% 	    {lists:reverse([NamedType|Acc]),Rest}
%%     end.

    

%% parse_ExtensionAdditionAlternativeList(Tokens) ->
%%     parse_ExtensionAdditionAlternativeList(Tokens,[]).

%% parse_ExtensionAdditionAlternativeList([{'[[',_}|Rest],Acc) ->
%%     parse_ExtensionAdditionAlternativeList(Rest,Acc);
%% parse_ExtensionAdditionAlternativeList(Tokens = [{identifier,_,_}|_Rest],Acc) ->
%%     {Element,Rest0} = parse_NamedType(Tokens);
%%     case Rest0 of
%% 	[{',',_}|Rest01] ->
%% 	    parse_ExtensionAdditionAlternativeList(Rest01,[Element|Acc]);
%% 	_  ->
%% 	    {lists:reverse([Element|Acc]),Rest0}
%%     end.

%% parse_ExtensionAdditionAlternatives([{'[[',_}|Rest]) ->
%%     parse_ExtensionAdditionAlternatives(Rest,[]);
%% parse_ExtensionAdditionAlternatives(Tokens) ->
%%     throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
%% 		       [got,get_token(hd(Tokens)),expected,'[[']}}).

%% parse_ExtensionAdditionAlternatives([Id = {identifier,_,_}|Rest],Acc) ->
%%     {NamedType, Rest2} = parse_NamedType([Id|Rest]),
%%     case Rest2 of
%% 	[{',',_}|Rest21] ->
%% 	    parse_ExtensionAdditionAlternatives(Rest21,[NamedType|Acc]);
%% 	[{']]',_}|Rest21] ->
%% 	    {lists:reverse(Acc),Rest21};
%% 	_ ->
%% 	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
%% 			       [got,get_token(hd(Rest2)),expected,[',',']]']]}})
%%     end.

parse_NamedType([{identifier,L1,Idname}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {#'ComponentType'{pos=L1,name=Idname,typespec=Type,prop=mandatory},Rest2};
parse_NamedType(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,identifier]}}).


parse_ComponentTypeLists(Tokens) ->
    parse_ComponentTypeLists(Tokens,[]).

parse_ComponentTypeLists(Tokens = [{identifier,_,_}|_Rest0],Clist) ->
    {CompList,Rest1} = parse_ComponentTypeList(Tokens,[]),
    parse_ComponentTypeLists(Rest1,Clist++CompList);
parse_ComponentTypeLists(Tokens = [{'COMPONENTS',_},{'OF',_}|_Rest],Clist) ->
    {CompList,Rest1} = parse_ComponentTypeList(Tokens,[]),
    parse_ComponentTypeLists(Rest1,Clist++CompList);
parse_ComponentTypeLists([{'...',L1},{'!',_}|Rest02],Clist0) ->
    {_,Rest03} = parse_ExceptionIdentification(Rest02),
    %% Exception info is currently thrown away
    parse_ComponentTypeLists2(Rest03,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_ComponentTypeLists([{',',L1},{'...',_},{'!',_}|Rest02],Clist0) when Clist0 =/= []->
    {_,Rest03} = parse_ExceptionIdentification(Rest02),
    %% Exception info is currently thrown away
    parse_ComponentTypeLists2(Rest03,Clist0++[#'EXTENSIONMARK'{pos=L1}]);

 parse_ComponentTypeLists([{',',_},{'...',L1}|Rest02],Clist0) when Clist0 =/= []->
    parse_ComponentTypeLists2(Rest02,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_ComponentTypeLists([{'...',L1}|Rest02],Clist0) ->
    parse_ComponentTypeLists2(Rest02,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_ComponentTypeLists(Tokens = [{'}',_L1}|_Rest02],Clist0) ->
    {Clist0,Tokens}.

parse_ComponentTypeLists2(Tokens,Clist) ->
    {ExtAdd,Rest} = parse_ExtensionAdditions(Tokens,Clist),
    {Clist2,Rest2} = parse_OptionalExtensionMarker(Rest,lists:flatten(ExtAdd)),
    case Rest2 of
	[{',',_}|Rest3] ->
	    {CompList,Rest4} = parse_ComponentTypeList(Rest3,[]),
	    {Clist2 ++ CompList,Rest4};
	_ ->
	    {Clist2,Rest2}
    end.

parse_OptionalExtensionMarker([{',',_},{'...',L1}|Rest],Clist)->
    {Clist++[#'EXTENSIONMARK'{pos=L1}],Rest};
parse_OptionalExtensionMarker(Tokens,Clist) ->
    {Clist,Tokens}.


parse_ComponentTypeList([{',',_},Id = {identifier,_,_}|Rest],Acc) when Acc =/= [] ->
    {ComponentType,Rest2} = parse_ComponentType([Id|Rest]),
    parse_ComponentTypeList(Rest2,[ComponentType|Acc]);
parse_ComponentTypeList([{',',_},C1={'COMPONENTS',_},C2={'OF',_}|Rest],Acc) when Acc =/= [] ->
    {ComponentType,Rest2} = parse_ComponentType([C1,C2|Rest]),
    parse_ComponentTypeList(Rest2,[ComponentType|Acc]);
parse_ComponentTypeList(Tokens = [{'}',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ComponentTypeList(Tokens = [{']',_},{']',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ComponentTypeList(Tokens = [{',',_},{'...',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ComponentTypeList(Tokens,[]) ->
    {ComponentType,Rest} = parse_ComponentType(Tokens),
    parse_ComponentTypeList(Rest,[ComponentType]);
parse_ComponentTypeList(Tokens,_) ->
    throw({asn1_error,
	   {get_line(hd(Tokens)),get(asn1_module),
	    [got,[get_token(hd(Tokens)),get_token(hd(tl(Tokens)))],
	     expected,['}',', identifier']]}}).

parse_ExtensionAdditions(Tokens=[{',',_}|_],Clist) ->
    {ExtAddList,Rest2} = parse_ExtensionAdditionList(Tokens,[]),
    {Clist++ExtAddList,Rest2};
parse_ExtensionAdditions(Tokens,Clist) ->
    %% Empty
    {Clist,Tokens}.

parse_ExtensionAdditionList([{',',_},Id = {identifier,_,_}|Rest],Acc) ->
    {ComponentType,Rest2} = parse_ComponentType([Id|Rest]),
    parse_ExtensionAdditionList(Rest2,[ComponentType|Acc]);
parse_ExtensionAdditionList([{',',_},C1={'COMPONENTS',_},C2={'OF',_}|Rest],Acc) ->
    {ComponentType,Rest2} = parse_ComponentType([C1,C2|Rest]),
    parse_ExtensionAdditionList(Rest2,[ComponentType|Acc]);
parse_ExtensionAdditionList([{',',_},C1 = {'[',_},C2 = {'[',_}|Rest],Acc) ->
    {ExtAddGroup,Rest2} = parse_ExtensionAdditionGroup([C1,C2|Rest],[]),
    parse_ExtensionAdditionList(Rest2,[ExtAddGroup|Acc]);
parse_ExtensionAdditionList(Tokens = [{'}',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ExtensionAdditionList(Tokens = [{',',_},{'...',_}|_],Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ExtensionAdditionList(Tokens,_) ->
    throw({asn1_error,
	   {get_line(hd(Tokens)),get(asn1_module),
	    [got,[get_token(hd(Tokens)),get_token(hd(tl(Tokens)))],
	     expected,['}',', identifier']]}}).


parse_ExtensionAdditionGroup([ {'[',_},{'[',_},_VsnNr = {number,_,Num},{':',_}|Rest],[]) ->
    parse_ExtensionAdditionGroup2(Rest,Num);
parse_ExtensionAdditionGroup([ {'[',_},{'[',_}|Rest],[]) ->
    parse_ExtensionAdditionGroup2(Rest,undefined);
parse_ExtensionAdditionGroup(Tokens,_) ->
    throw({asn1_error,
	   {get_line(hd(Tokens)),get(asn1_module),
	    [got,[get_token(hd(Tokens)),get_token(hd(tl(Tokens)))],
	     expected,['[[']]}}).


parse_ExtensionAdditionGroup2(Tokens,Num) ->
    {CompTypeList,Rest} = parse_ComponentTypeList(Tokens,[]),
    case Rest of
	[{']',_},{']',_}|Rest2] ->
	    {[{'ExtensionAdditionGroup',Num}|CompTypeList] ++
		['ExtensionAdditionGroupEnd'],Rest2};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest)),get(asn1_module),
			       [got,get_token(hd(Rest)),expected,[']]']]}})
    end.


parse_ComponentType([{'COMPONENTS',_},{'OF',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {{'COMPONENTS OF',Type},Rest2};
parse_ComponentType(Tokens) ->
    Result = {NamedType,Rest} = parse_NamedType(Tokens),
    case Rest of
	[{'OPTIONAL',_}|Rest2] ->
	    {NamedType#'ComponentType'{prop='OPTIONAL'},Rest2};
	[{'DEFAULT',_}|Rest2] ->
	    {Value,Rest21} = parse_Value(Rest2),
	    {NamedType#'ComponentType'{prop={'DEFAULT',Value}},Rest21};
	_ ->
	    Result
    end.

	    

parse_SignedNumber([{number,_,Value}|Rest]) ->
    {Value,Rest};
parse_SignedNumber([{'-',_},{number,_,Value}|Rest]) ->
    {-Value,Rest};
parse_SignedNumber(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,
			[number,'-number']]}}).

parse_Enumerations(Tokens=[{identifier,_,_}|_Rest],ExtensionDefault) ->
    parse_Enumerations(Tokens,[],ExtensionDefault);
parse_Enumerations([H|_T],_) ->
    throw({asn1_error,{get_line(H),get(asn1_module),
		       [got,get_token(H),expected,identifier]}}).

parse_Enumerations(Tokens = [{identifier,_,_},{'(',_}|_Rest], Acc, ExtensionDefault) ->
    {NamedNumber,Rest2} = parse_NamedNumber(Tokens),
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_Enumerations(Rest3,[NamedNumber|Acc], ExtensionDefault);
	_ when ExtensionDefault == 'IMPLIED'->
	    {lists:reverse(['EXTENSIONMARK',NamedNumber|Acc]),Rest2};
	_ ->
	    {lists:reverse([NamedNumber|Acc]),Rest2}
    end;
parse_Enumerations([{identifier,_,Id}|Rest], Acc, ExtensionDefault) ->
    case Rest of
	[{',',_}|Rest2] ->
	    parse_Enumerations(Rest2,[Id|Acc], ExtensionDefault);
	_ when ExtensionDefault == 'IMPLIED' ->
	    {lists:reverse(['EXTENSIONMARK', Id |Acc]),Rest};
	_ ->
	    {lists:reverse([Id|Acc]),Rest}
    end;
parse_Enumerations([{'...',_}|Rest], Acc, _ExtensionDefault) ->
    case Rest of
	[{',',_}|Rest2] ->
	    parse_Enumerations(Rest2,['EXTENSIONMARK'|Acc],undefined);
	_ ->
	    {lists:reverse(['EXTENSIONMARK'|Acc]),Rest}
    end;
parse_Enumerations([H|_T],_,_) ->
    throw({asn1_error,{get_line(H),get(asn1_module),
		       [got,get_token(H),expected,identifier]}}).

parse_NamedNumberList(Tokens) ->
    parse_NamedNumberList(Tokens,[]).

parse_NamedNumberList(Tokens,Acc) ->
    {NamedNum,Rest} = parse_NamedNumber(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_NamedNumberList(Rest2,[NamedNum|Acc]);
	_ ->
	    {lists:reverse([NamedNum|Acc]),Rest}
    end.

parse_NamedNumber([{identifier,_,Name},{'(',_}|Rest]) ->
    Flist = [fun parse_SignedNumber/1,
	     fun parse_DefinedValue/1],
    case (catch parse_or(Rest,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	{NamedNum,[{')',_}|Rest2]} ->
	    {{'NamedNumber',Name,NamedNum},Rest2};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest)),get(asn1_module),
			       [got,get_token(hd(Rest)),expected,'NamedNumberList']}})
    end;
parse_NamedNumber(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,identifier]}}).


parse_Tag([{'[',_}|Rest]) ->
    {Class,Rest2} = parse_Class(Rest),
    {ClassNumber,Rest3} = 
	case Rest2 of
	    [{number,_,Num}|Rest21] ->
		{Num,Rest21};
	    _ ->
		parse_DefinedValue(Rest2)
	end,
    case Rest3 of
	[{']',_}|Rest4] ->
	    {#tag{class=Class,number=ClassNumber},Rest4};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest3)),get(asn1_module),
			       [got,get_token(hd(Rest3)),expected,']']}})
    end;
parse_Tag(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'[']}}).

parse_Class([{'UNIVERSAL',_}|Rest]) ->
    {'UNIVERSAL',Rest};
parse_Class([{'APPLICATION',_}|Rest]) ->
    {'APPLICATION',Rest};
parse_Class([{'PRIVATE',_}|Rest]) ->
    {'PRIVATE',Rest};
parse_Class(Tokens) ->
    {'CONTEXT',Tokens}.

%% parse_Value(Tokens) -> Ret
%% Tokens  = [Tok]
%% Tok     = tuple()
%% Ret     = term()
parse_Value(Tokens) ->
    Flist = [fun parse_BuiltinValue/1,
	     fun parse_ValueFromObject/1,
	     fun parse_DefinedValue/1],

    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end.

parse_BuiltinValue([{bstring,_,Bstr}|Rest]) ->
    {{bstring,Bstr},Rest};
parse_BuiltinValue([{hstring,_,Hstr}|Rest]) ->
    {{hstring,Hstr},Rest};
parse_BuiltinValue([{'{',_},{'}',_}|Rest]) ->
    {[],Rest};
parse_BuiltinValue(Tokens = [{'{',_}|_Rest]) ->
    Flist = [
	     fun parse_SequenceOfValue/1, 
	     fun parse_SequenceValue/1, 
	     fun parse_ObjectIdentifierValue/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	Result ->
	    Result
    end;
parse_BuiltinValue([{identifier,_,IdName},{':',_}|Rest]) ->
    {Value,Rest2} = parse_Value(Rest),
    {{'CHOICE',{IdName,Value}},Rest2};
parse_BuiltinValue(Tokens=[{'NULL',_},{':',_}|_Rest])  ->
    parse_ObjectClassFieldValue(Tokens);
parse_BuiltinValue([{'NULL',_}|Rest])  ->
    {'NULL',Rest};
parse_BuiltinValue([{'TRUE',_}|Rest]) ->
    {true,Rest};
parse_BuiltinValue([{'FALSE',_}|Rest]) ->
    {false,Rest};
parse_BuiltinValue([{'PLUS-INFINITY',_}|Rest]) ->
    {'PLUS-INFINITY',Rest};
parse_BuiltinValue([{'MINUS-INFINITY',_}|Rest]) ->
    {'MINUS-INFINITY',Rest};
parse_BuiltinValue([{cstring,_,Cstr}|Rest]) ->
    {Cstr,Rest};
parse_BuiltinValue([{number,_,Num}|Rest]) ->
    {Num,Rest};
parse_BuiltinValue([{'-',_},{number,_,Num}|Rest]) ->
    {- Num,Rest};
parse_BuiltinValue(Tokens) ->
    parse_ObjectClassFieldValue(Tokens).

parse_DefinedValue(Tokens=[{identifier,_,_},{'{',_}|_Rest]) ->
    parse_ParameterizedValue(Tokens);
%% Externalvaluereference
parse_DefinedValue([{typereference,L1,Tname},{'.',_},{identifier,_,Idname}|Rest]) ->
    {#'Externalvaluereference'{pos=L1,module=Tname,value=Idname},Rest};
%% valuereference
parse_DefinedValue([Id = {identifier,_,_}|Rest]) ->
    {identifier2Extvalueref(Id),Rest};
%% ParameterizedValue
parse_DefinedValue(Tokens) ->
    parse_ParameterizedValue(Tokens).


parse_SequenceValue([{'{',_}|Tokens]) ->
    parse_SequenceValue(Tokens,[]);
parse_SequenceValue(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_SequenceValue([{identifier,_,IdName}|Rest],Acc) ->
    {Value,Rest2} = parse_Value(Rest),
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_SequenceValue(Rest3,[{IdName,Value}|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse([{IdName,Value}|Acc]),Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end;
parse_SequenceValue(Tokens,_Acc) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,identifier]}}).

parse_SequenceOfValue([{'{',_}|Tokens]) ->
    parse_SequenceOfValue(Tokens,[]);
parse_SequenceOfValue(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_SequenceOfValue(Tokens,Acc) ->
    {Value,Rest2} = parse_Value(Tokens),
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_SequenceOfValue(Rest3,[Value|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse([Value|Acc]),Rest3};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'}']}})
    end.

parse_ValueSetTypeAssignment([{typereference,L1,Name}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {ValueSet,Rest4} = parse_ValueSet(Rest3),
	    {#valuedef{pos=L1,name=Name,type=Type,value=ValueSet,
		       module=get(asn1_module)},Rest4};
	[H|_T] ->
	    throw({asn1_error,{get_line(L1),get(asn1_module),
			       [got,get_token(H),expected,'::=']}})
    end;
parse_ValueSetTypeAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,
				   typereference]}}).

parse_ValueSet([{'{',_}|Rest]) ->
    {Elems,Rest2} = parse_ElementSetSpecs(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {{valueset,Elems},Rest3};
	[H|_T] ->
	    throw({asn1_error,{get_line(H),get(asn1_module),
			       [got,get_token(H),expected,'}']}})
    end;
parse_ValueSet(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'{']}}).

parse_ValueAssignment([{identifier,L1,IdName}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Value,Rest4} = parse_Value(Rest3),
	    case catch lookahead_assignment(Rest4) of
		ok ->
		    {#valuedef{pos=L1,name=IdName,type=Type,value=Value,
			       module=get(asn1_module)},Rest4};
		Error ->
		    throw(Error)
%% 		    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
%% 				       [got,get_token(hd(Rest2)),expected,'::=']}})
	    end;
	_ ->
	    throw({asn1_error,{get_line(hd(Rest2)),get(asn1_module),
			       [got,get_token(hd(Rest2)),expected,'::=']}})
    end;
parse_ValueAssignment(Tokens) ->
    throw({asn1_assignment_error,{get_line(hd(Tokens)),get(asn1_module),
				  [got,get_token(hd(Tokens)),expected,identifier]}}).

%% SizeConstraint
parse_SubtypeElements([{'SIZE',_}|Tokens]) ->
    {Constraint,Rest} = parse_Constraint(Tokens),
    {{'SizeConstraint',Constraint#constraint.c},Rest};
%% PermittedAlphabet
parse_SubtypeElements([{'FROM',_}|Tokens]) ->
    {Constraint,Rest} = parse_Constraint(Tokens),
    {{'PermittedAlphabet',Constraint#constraint.c},Rest};
%% InnerTypeConstraints
parse_SubtypeElements([{'WITH',_},{'COMPONENT',_}|Tokens]) ->
    {Constraint,Rest} = parse_Constraint(Tokens),
    {{'WITH COMPONENT',Constraint},Rest};
parse_SubtypeElements([{'WITH',_},{'COMPONENTS',_},{'{',_},{'...',_},{',',_}|Tokens]) ->
    {Constraint,Rest} = parse_TypeConstraints(Tokens),
    case Rest of
	[{'}',_}|Rest2] ->
	    {{'WITH COMPONENTS',{'PartialSpecification',Constraint}},Rest2};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest)),get(asn1_module),
			       [got,get_token(hd(Rest)),expected,'}']}})
    end;
parse_SubtypeElements([{'WITH',_},{'COMPONENTS',_},{'{',_}|Tokens]) ->
    {Constraint,Rest} = parse_TypeConstraints(Tokens),
    case Rest of
	[{'}',_}|Rest2] ->
	    {{'WITH COMPONENTS',{'FullSpecification',Constraint}},Rest2};
	_ ->
	    throw({asn1_error,{get_line(hd(Rest)),get(asn1_module),
			       [got,get_token(hd(Rest)),expected,'}']}})
    end;
parse_SubtypeElements([{'PATTERN',_}|Tokens]) ->
    {Value,Rest} = parse_Value(Tokens),
    {{pattern,Value},Rest};
%% SingleValue
%% ContainedSubtype
%% ValueRange
%% TypeConstraint
%% Moved fun parse_Value/1 and fun parse_Type/1 to parse_Elements
parse_SubtypeElements(Tokens) ->
    Flist = [fun parse_ContainedSubtype/1,
	     fun parse_Value/1, 
	     fun([{'MIN',_}|T]) -> {'MIN',T} end,
	     fun parse_Type/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	{asn1_error,Reason} ->
	    throw(Reason);
	Result = {Val,_} when is_record(Val,type) ->
	    Result;
	{Lower,[{'..',_}|Rest]} ->
	    {Upper,Rest2} = parse_UpperEndpoint(Rest),
	    {{'ValueRange',{Lower,Upper}},Rest2};
	{Lower,[{'<',_},{'..',_}|Rest]} ->
	    {Upper,Rest2} = parse_UpperEndpoint(Rest),
	    {{'ValueRange',{{gt,Lower},Upper}},Rest2};
	{Res={'ContainedSubtype',_Type},Rest} ->
	    {Res,Rest};
	{Value,Rest} ->
	    {{'SingleValue',Value},Rest}
    end.

parse_ContainedSubtype([{'INCLUDES',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {{'ContainedSubtype',Type},Rest2};
parse_ContainedSubtype(Tokens) ->
    throw({asn1_error,{get_line(hd(Tokens)),get(asn1_module),
		       [got,get_token(hd(Tokens)),expected,'INCLUDES']}}).
%%parse_ContainedSubtype(Tokens) -> %this option is moved to parse_SubtypeElements
%%    parse_Type(Tokens).

parse_UpperEndpoint([{'<',_}|Rest]) ->
    parse_UpperEndpoint(lt,Rest);
parse_UpperEndpoint(Tokens) ->
    parse_UpperEndpoint(false,Tokens).

parse_UpperEndpoint(Lt,Tokens) ->
    Flist = [ fun([{'MAX',_}|T]) -> {'MAX',T} end,
	      fun parse_Value/1],
    case (catch parse_or(Tokens,Flist)) of
	{'EXIT',Reason} ->
	    exit(Reason);
	AsnErr = {asn1_error,_} ->
	    throw(AsnErr);
	{Value,Rest2} when Lt == lt ->
	    {{lt,Value},Rest2};
	{Value,Rest2} ->
	    {Value,Rest2}
    end.

parse_TypeConstraints(Tokens) ->
    parse_TypeConstraints(Tokens,[]).

parse_TypeConstraints([{identifier,_,_}|Rest],Acc) ->
    {ComponentConstraint,Rest2} = parse_ComponentConstraint(Rest),
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_TypeConstraints(Rest3,[ComponentConstraint|Acc]);
	_ ->
	    {lists:reverse([ComponentConstraint|Acc]),Rest2}
    end;
parse_TypeConstraints([H|_T],_) ->
    throw({asn1_error,{get_line(H),get(asn1_module),
		       [got,get_token(H),expected,identifier]}}).

parse_ComponentConstraint(Tokens = [{'(',_}|_Rest]) ->
    {ValueConstraint,Rest2} = parse_Constraint(Tokens),
    {PresenceConstraint,Rest3} = parse_PresenceConstraint(Rest2),
    {{ValueConstraint,PresenceConstraint},Rest3};
parse_ComponentConstraint(Tokens) ->
    {PresenceConstraint,Rest} = parse_PresenceConstraint(Tokens),
    {{asn1_empty,PresenceConstraint},Rest}.

parse_PresenceConstraint([{'PRESENT',_}|Rest]) ->
    {'PRESENT',Rest};
parse_PresenceConstraint([{'ABSENT',_}|Rest]) ->
    {'ABSENT',Rest};
parse_PresenceConstraint([{'OPTIONAL',_}|Rest]) ->
    {'OPTIONAL',Rest};
parse_PresenceConstraint(Tokens) ->
    {asn1_empty,Tokens}.


% merge_constraints({Rlist,ExtList}) -> % extensionmarker in constraint
%     {merge_constraints(Rlist,[],[]),
%      merge_constraints(ExtList,[],[])};

%% An arg with a constraint with extension marker will look like
%% [#constraint{c={Root,Ext}}|Rest]
	
merge_constraints(Clist) ->
    merge_constraints(Clist, [], []).

merge_constraints([Ch|Ct],Cacc, Eacc) ->
    NewEacc = case Ch#constraint.e of
		  undefined -> Eacc;
		  E -> [E|Eacc]
	      end,
    merge_constraints(Ct,[fixup_constraint(Ch#constraint.c)|Cacc],NewEacc);

merge_constraints([],Cacc,[]) ->
%%    lists:flatten(Cacc);
    lists:reverse(Cacc);
merge_constraints([],Cacc,Eacc) ->
%%    lists:flatten(Cacc) ++ [{'Errors',Eacc}].
    lists:reverse(Cacc) ++ [{'Errors',Eacc}].


fixup_constraint(C) ->
    case C of
	{'SingleValue',SubType} when element(1,SubType) == 'ContainedSubtype' ->
	    SubType;
	{'SingleValue',V} when is_list(V) ->
	    C;
	%%	    [C,{'ValueRange',{lists:min(V),lists:max(V)}}]; 
	%% bug, turns wrong when an element in V is a reference to a defined value
	{'PermittedAlphabet',{'SingleValue',V}} when is_list(V) ->
	    %%sort and remove duplicates
	    V2 = {'SingleValue',
		  ordsets:from_list(lists:flatten(V))},
	    {'PermittedAlphabet',V2};
	{'PermittedAlphabet',{'SingleValue',V}} ->
	    V2 = {'SingleValue',[V]},
	    {'PermittedAlphabet',V2};
	{'SizeConstraint',Sc} ->
	    {'SizeConstraint',fixup_size_constraint(Sc)};
	
	List when is_list(List) ->  %% In This case maybe a union or intersection
	    [fixup_constraint(Xc)||Xc <- List];
	Other ->
	    Other
    end.

fixup_size_constraint({'ValueRange',{Lb,Ub}}) ->
	{Lb,Ub};
fixup_size_constraint({{'ValueRange',R},[]}) ->
	{R,[]};
fixup_size_constraint({[],{'ValueRange',R}}) ->
	{[],R};
fixup_size_constraint({{'ValueRange',R1},{'ValueRange',R2}}) ->
	{R1,R2};
fixup_size_constraint({'SingleValue',[Sv]}) ->
	fixup_size_constraint({'SingleValue',Sv});
fixup_size_constraint({'SingleValue',L}) when is_list(L) ->
	ordsets:from_list(L);
fixup_size_constraint({'SingleValue',L}) ->
	{L,L};
fixup_size_constraint({'SizeConstraint',C}) ->
    %% this is a second SIZE
    fixup_size_constraint(C);
fixup_size_constraint({C1,C2}) ->
    %% this is with extension marks
    {turn2vr(fixup_size_constraint(C1)), extension_size(fixup_size_constraint(C2))};
fixup_size_constraint(CList) when is_list(CList) ->
    [fixup_constraint(Xc)||Xc <- CList].
   
turn2vr(L) when is_list(L) ->
    L2 =[X||X<-ordsets:from_list(L),is_integer(X)],
    case L2 of
	[H|_] ->
	    {H,hd(lists:reverse(L2))};
	_ ->
	    L
    end;
turn2vr(VR) ->
    VR.
extension_size({I,I}) ->
    [I];
extension_size({I1,I2}) ->
    [I1,I2];
extension_size(C) ->
    C.

get_line({_,Pos,Token}) when is_integer(Pos),is_atom(Token) ->
    Pos;
get_line({Token,Pos}) when is_integer(Pos),is_atom(Token) ->
    Pos;
get_line(_) ->
    undefined.

get_token({_,Pos,Token}) when is_integer(Pos),is_atom(Token) ->
    Token;
get_token({'$end',Pos}) when is_integer(Pos) ->
    undefined;
get_token({Token,Pos}) when is_integer(Pos),is_atom(Token) ->
    Token;
get_token(_) ->
    undefined.

prioritize_error(ErrList) ->
    case lists:keymember(asn1_error,1,ErrList) of
	false -> % only asn1_assignment_error -> take the last
	    lists:last(ErrList);
	true -> % contains errors from deeper in a Type
	    NewErrList = [_Err={_,_}|_RestErr] =
		lists:filter(fun({asn1_error,_})->true;(_)->false end,
			     ErrList),
	    SplitErrs =
		lists:splitwith(fun({_,X})->
					case element(1,X) of
					    Int when is_integer(Int) -> true;
					    _ -> false
					end
				end,
				NewErrList),
	    case SplitErrs of
		{[],UndefPosErrs} -> % if no error with Positon exists
		    lists:last(UndefPosErrs);
		{IntPosErrs,_} ->
		    IntPosReasons = lists:map(fun(X)->element(2,X) end,IntPosErrs),
		    SortedReasons = lists:keysort(1,IntPosReasons),
		    {asn1_error,lists:last(SortedReasons)}
	    end
    end.

%% most_prio_error([H={_,Reason}|T],Atom,Err) when is_atom(Atom) ->
%%     most_prio_error(T,element(1,Reason),H);
%% most_prio_error([H={_,Reason}|T],Greatest,Err) ->
%%     case element(1,Reason) of
%% 	Pos when is_integer(Pos),Pos>Greatest ->
%% 	    most_prio_error(


tref2Exttref(#typereference{pos=Pos,val=Name}) ->
    #'Externaltypereference'{pos=Pos,
			     module=get(asn1_module),
			     type=Name}.

tref2Exttref(Pos,Name) ->
    #'Externaltypereference'{pos=Pos,
			     module=get(asn1_module),
			     type=Name}.

identifier2Extvalueref(#identifier{pos=Pos,val=Name}) ->
    #'Externalvaluereference'{pos=Pos,
			      module=get(asn1_module),
			      value=Name}.

%% lookahead_assignment/1 checks that the next sequence of tokens
%% in Token contain a valid assignment or the
%% 'END' token. Otherwise an exception is thrown.
lookahead_assignment([{'END',_}|_Rest]) ->
    ok;
lookahead_assignment(Tokens) ->
    parse_Assignment(Tokens),
    ok.
	
is_pre_defined_class('TYPE-IDENTIFIER') ->
    true;
is_pre_defined_class('ABSTRACT-SYNTAX') ->
    true;
is_pre_defined_class(_) ->
    false.
    
