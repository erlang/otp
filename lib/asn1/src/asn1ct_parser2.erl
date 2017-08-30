%% vim: tabstop=8:shiftwidth=4
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(asn1ct_parser2).

-export([parse/2,format_error/1]).
-include("asn1_records.hrl").

%% Only used internally within this module.
-record(typereference, {pos,val}).
-record(constraint, {c,e}).
-record(identifier, {pos,val}).

parse(File0, Tokens0) ->
    try do_parse(Tokens0) of
	{ok,#module{}}=Result ->
	    Result
    catch
	throw:{asn1_error,Fun} when is_function(Fun, 0) ->
	    handle_parse_error(File0, Fun());
	throw:{asn1_error,{parse_error,Tokens}} ->
	    handle_parse_error(File0, Tokens)
    after
	clean_process_dictionary()
    end.

handle_parse_error(File0, [Token|_]) ->
    File = filename:basename(File0),
    Line = get_line(Token),
    Error = {structured_error,{File,Line},?MODULE,
	     {syntax_error,get_token(Token)}},
    {error,[Error]}.

do_parse(Tokens0) ->
    {ModuleDefinition,Tokens1} = parse_ModuleDefinition(Tokens0),
    {Types,Tokens2} = parse_AssignmentList(Tokens1),
    case Tokens2 of
	[{'END',_}|_Rest3] ->
	    {ok,ModuleDefinition#module{typeorval=Types}};
	_  ->
	    parse_error(Tokens2)
    end.

clean_process_dictionary() ->
    Mod = erase(asn1_module),
    _ = erase({Mod,imports}),
    _ = erase(tagdefault),
    _ = erase(extensiondefault),
    ok.

format_error({syntax_error,Token}) when is_atom(Token) ->
    io_lib:format("syntax error before: '~s'", [Token]);
format_error({syntax_error,Token}) ->
    io_lib:format("syntax error before: '~p'", [Token]).

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
		   parse_error(Rest02)
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
	    {{imports, Imports}, Rest6} = parse_Imports(Rest5),
            put({get(asn1_module), imports}, Imports),
	    {#module{ pos = L1,
		     name = ModuleIdentifier,
		     defid = [], % fix this
		     tagdefault = TagDefault,
		     extensiondefault = ExtensionDefault,
		     exports = Exports,
		     imports = {imports, Imports}}, Rest6};
	_ ->
	    parse_error(Rest3)
    end;
parse_ModuleDefinition(Tokens) ->
    parse_error(Tokens).
    
parse_Exports([{'EXPORTS',_L1},{';',_L2}|Rest]) ->
    {{exports,[]},Rest};
parse_Exports([{'EXPORTS',_},{'ALL',_},{';',_}|Rest]) ->
    %% Same as no exports definition.
    {{exports,all},Rest};
parse_Exports([{'EXPORTS',_L1}|Rest]) ->
    {SymbolList,Rest2} = parse_SymbolList(Rest),
    case Rest2 of
	[{';',_}|Rest3] ->    
	    {{exports,SymbolList},Rest3};
	_ ->
	    parse_error(Rest2)
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
	    {lists:reverse(Acc, [Symbol]),Rest2}
    end.

parse_Symbol(Tokens) ->
    parse_Reference(Tokens).

parse_Reference([{typereference,L1,TrefName},{'{',_L2},{'}',_L3}|Rest]) ->
    {tref2Exttref(L1,TrefName),Rest};
parse_Reference([Tref1 = {typereference,_,_},{'.',_},Tref2 = {typereference,_,_},
		 {'{',_L2},{'}',_L3}|Rest]) ->
    {{tref2Exttref(Tref1),tref2Exttref(Tref2)},Rest};
parse_Reference([Tref = {typereference,_L1,_TrefName}|Rest]) ->
    {tref2Exttref(Tref),Rest};
parse_Reference([#identifier{}=Vref,{'{',_L2},{'}',_L3}|Rest]) ->
    {identifier2Extvalueref(Vref),Rest};
parse_Reference([#identifier{}=Vref|Rest]) ->
    {identifier2Extvalueref(Vref),Rest};
parse_Reference(Tokens) ->
    parse_error(Tokens).

parse_Imports([{'IMPORTS',_L1},{';',_L2}|Rest]) ->
    {{imports,[]},Rest};
parse_Imports([{'IMPORTS',_L1}|Rest]) ->
    {SymbolsFromModuleList,Rest2} = parse_SymbolsFromModuleList(Rest),
    case Rest2 of
	[{';',_L2}|Rest3] ->
	    {{imports,SymbolsFromModuleList},Rest3};
	_ ->
	    parse_error(Rest2)
    end;
parse_Imports(Tokens) ->
    {{imports,[]},Tokens}.

parse_SymbolsFromModuleList(Tokens) ->
    parse_SymbolsFromModuleList(Tokens,[]).

parse_SymbolsFromModuleList(Tokens,Acc) ->
    {SymbolsFromModule,Rest} = parse_SymbolsFromModule(Tokens),
    try parse_SymbolsFromModule(Rest) of
	{Sl,_Rest2} when is_record(Sl,'SymbolsFromModule') ->
	    parse_SymbolsFromModuleList(Rest, [SymbolsFromModule|Acc])
    catch
	throw:{asn1_error,_} ->
	    {lists:reverse(Acc, [SymbolsFromModule]),Rest}
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
	[{'FROM',_L1},{typereference,_,Name}=Tref|
	 [#identifier{},{',',_}|_]=Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name), SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest2};

	%% This a special case when there is only one Symbol imported
	%% from the next module. No other way to distinguish Ref from
	%% a part of the GlobalModuleReference of Name.
	[{'FROM',_L1},{typereference,_,Name}=Tref|
	 [#identifier{},{'FROM',_}|_]=Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name), SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest2};
	[{'FROM',_L1},{typereference,_,Name}=Tref,#identifier{}|Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name), SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest2}; 
	[{'FROM',_L1},{typereference,_,Name}=Tref|[{'{',_}|_]=Rest2] ->
	    {_ObjIdVal,Rest3} = parse_ObjectIdentifierValue(Rest2), % value not used yet, fix me
	    NewSymbolList = lists:map(SetRefModuleName(Name), SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest3}; 
	[{'FROM',_L1},{typereference,_,Name}=Tref|Rest2] ->
	    NewSymbolList = lists:map(SetRefModuleName(Name), SymbolList),
	    {#'SymbolsFromModule'{symbols=NewSymbolList,
				  module=tref2Exttref(Tref)},Rest2};
	_ ->
	    parse_error(Rest)
    end.

parse_ObjectIdentifierValue([{'{',_}|Rest]) ->
    parse_ObjectIdentifierValue(Rest,[]).

parse_ObjectIdentifierValue([{number,_,Num}|Rest], Acc) ->
    parse_ObjectIdentifierValue(Rest,[Num|Acc]);
parse_ObjectIdentifierValue([#identifier{val=Id},{'(',_},{number,_,Num},{')',_}|Rest], Acc) ->
    parse_ObjectIdentifierValue(Rest,[{'NamedNumber',Id,Num}|Acc]);
parse_ObjectIdentifierValue([#identifier{val=Id},{'(',_},#identifier{val=Id2},{')',_}|Rest], Acc) ->
    parse_ObjectIdentifierValue(Rest,[{'NamedNumber',Id,Id2}|Acc]);
parse_ObjectIdentifierValue([#identifier{val=Id},{'(',_},{typereference,_,Tref},{'.',_},#identifier{val=Id2}, {')',_}|Rest], Acc) ->
    parse_ObjectIdentifierValue(Rest, [{'NamedNumber',Id,{'ExternalValue',Tref,Id2}}|Acc]);
parse_ObjectIdentifierValue([#identifier{}=Id|Rest], Acc) ->
    parse_ObjectIdentifierValue(Rest, [identifier2Extvalueref(Id)|Acc]);
parse_ObjectIdentifierValue([{'}',_}|Rest], Acc) ->
    {lists:reverse(Acc),Rest};
parse_ObjectIdentifierValue(Tokens, _Acc) ->
    parse_error(Tokens).
    
parse_AssignmentList(Tokens) ->
    parse_AssignmentList(Tokens, []).

parse_AssignmentList([{'END',_}|_]=Tokens, Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AssignmentList([{'$end',_}|_]=Tokens, Acc) ->
    {lists:reverse(Acc),Tokens};
parse_AssignmentList(Tokens0, Acc) ->
    {Assignment,Tokens} = parse_Assignment(Tokens0),
    parse_AssignmentList(Tokens, [Assignment|Acc]).

parse_Assignment([{typereference,L1,Name},{'::=',_}|Tokens0]) ->
    %% 1) Type ::= TypeDefinition
    %% 2) CLASS-NAME ::= CLASS {...}
    Flist = [{type,fun parse_Type/1},
	     {class,fun parse_ObjectClass/1}],
    case parse_or_tag(Tokens0, Flist) of
	{{type,Type},Tokens} ->
	    %% TypeAssignment
	    {#typedef{pos=L1,name=Name,typespec=Type},Tokens};
	{{class,Type},Tokens} ->
	    %% ObjectClassAssignment
	    {#classdef{pos=L1,name=Name,module=resolve_module(Type),
		       typespec=Type},Tokens}
    end;
parse_Assignment([{typereference,_,_},{'{',_}|_]=Tokens) ->
    %% 1) Type{...} ::= ...
    %% 2) ValueSet{...} Type ::= ...
    %%    ObjectSet{...} CLASS-NAME ::= CLASS {...}
    %% 3) CLASS-NAME{...} ::= CLASS {...}
    %% A parameterized value set and and a parameterized object set
    %% cannot be distinguished from each other without type information.
    Flist = [fun parse_ParameterizedTypeAssignment/1,
	     fun parse_ParameterizedValueSetTypeAssignment/1,
	     fun parse_ParameterizedObjectClassAssignment/1],
    parse_or(Tokens, Flist);
parse_Assignment([{typereference,_,_}|_]=Tokens) ->
    %% 1) ObjectSet CLASS-NAME ::= ...
    %% 2) ValueSet Type ::= ...
    Flist = [fun parse_ObjectSetAssignment/1,
	     fun parse_ValueSetTypeAssignment/1],
    parse_or(Tokens, Flist);
parse_Assignment([#identifier{},{'{',_}|_]=Tokens) ->
    %% 1) value{...} Type ::= ...
    %% 2) object{...} CLASS-NAME ::= ...
    Flist = [fun parse_ParameterizedValueAssignment/1,
	     fun parse_ParameterizedObjectAssignment/1],
    parse_or(Tokens, Flist);
parse_Assignment([#identifier{}|_]=Tokens) ->
    %% 1) value Type ::= ...
    %% 2) object CLASS-NAME ::= ...
    Flist = [fun parse_ValueAssignment/1,
	     fun parse_ObjectAssignment/1],
    parse_or(Tokens, Flist);
parse_Assignment(Tokens) ->
    parse_error(Tokens).

parse_or(Tokens,Flist) ->
	parse_or(Tokens,Flist,[]).

parse_or(Tokens, [Fun|Funs], ErrList) when is_function(Fun, 1) ->
    try Fun(Tokens) of
	{_,Rest}=Result when is_list(Rest) ->
	    Result
    catch
	throw:{asn1_error,Error} ->
	    parse_or(Tokens, Funs, [Error|ErrList])
    end;
parse_or(_Tokens, [], ErrList) ->
    throw({asn1_error,fun() -> prioritize_error(ErrList) end}).

parse_or_tag(Tokens, Flist) ->
    parse_or_tag(Tokens, Flist, []).

parse_or_tag(Tokens, [{Tag,Fun}|Funs], ErrList) when is_function(Fun, 1) ->
    try Fun(Tokens) of
	{Parsed,Rest} when is_list(Rest) ->
	    {{Tag,Parsed},Rest}
    catch
	throw:{asn1_error,Error} ->
	    parse_or_tag(Tokens, Funs, [Error|ErrList])
    end;
parse_or_tag(_Tokens, [], ErrList) ->
    throw({asn1_error,fun() -> prioritize_error(ErrList) end}).

prioritize_error(Errors0) ->
    Errors1 = prioritize_error_1(Errors0),
    Errors2 = [{length(L),L} || L <- Errors1],
    Errors = lists:sort(Errors2),
    [Res|_] = [L || {_,L} <- Errors],
    Res.

prioritize_error_1([F|T]) when is_function(F, 0) ->
    [F()|prioritize_error_1(T)];
prioritize_error_1([{parse_error,Tokens}|T]) ->
    [Tokens|prioritize_error_1(T)];
prioritize_error_1([]) ->
    [].


%% parse_Type(Tokens) -> Ret
%%
%% Tokens = [Tok]
%% Tok    = tuple()
%% Ret    = #type{}
%%
parse_Type(Tokens) ->
    {Tag,Rest3} = case Tokens of
		      [{'[',_}|_] -> parse_Tag(Tokens);
		      _ -> {[],Tokens}
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
    Flist = [fun parse_BuiltinType/1,
	     fun parse_ReferencedType/1,
	     fun parse_TypeWithConstraint/1],
    {Type,Rest5} = parse_or(Rest4, Flist),
    case Rest5 of
	[{'(',_}|_] ->
	    {Constraints,Rest6} = parse_Constraints(Rest5),
	    {Type#type{tag=Tag2,
		       constraint=merge_constraints(Constraints)},Rest6};
	[_|_] ->
	    {Type#type{tag=Tag2},Rest5}
    end.

parse_BuiltinType([{'BIT',_},{'STRING',_}|Rest]) ->
    case Rest of 
	[{'{',_}|Rest2] ->
	    {NamedNumberList,Rest3} = parse_NamedNumberList(Rest2),
	    case Rest3 of
		[{'}',_}|Rest4] ->
		    {#type{def={'BIT STRING',NamedNumberList}},Rest4};
		_ ->
		    parse_error(Rest3)
	    end;
	 _ ->
	    {#type{def={'BIT STRING',[]}},Rest}
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
    {L0,Rest2} = parse_AlternativeTypeLists(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    NeedExt = not lists:keymember('EXTENSIONMARK', 1, L0) andalso
		get(extensiondefault) =:= 'IMPLIED',
	    L = case NeedExt of
		    true ->
			L0 ++ [#'EXTENSIONMARK'{}];
		    false ->
			L0
		end,
	    {#type{def={'CHOICE',L}},Rest3};
	_  ->
	    parse_error(Rest2)
    end;
parse_BuiltinType([{'EMBEDDED',_},{'PDV',_}|Rest]) ->
    {#type{def='EMBEDDED PDV'},Rest};
parse_BuiltinType([{'ENUMERATED',_},{'{',_}|Rest]) ->
    {Enumerations,Rest2} = parse_Enumerations(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {#type{def={'ENUMERATED',Enumerations}},Rest3};
	_ ->
	    parse_error(Rest2)
    end;
parse_BuiltinType([{'EXTERNAL',_}|Rest]) ->
    {#type{def='EXTERNAL'},Rest};
parse_BuiltinType([{'INSTANCE',_},{'OF',_}|Rest]) ->
    {DefinedObjectClass,Rest2} = parse_DefinedObjectClass(Rest),
    case Rest2 of
	[{'(',_}|_] ->
	    {Constraint0,Rest3} = parse_Constraint(Rest2),
	    Constraint = merge_constraints([Constraint0]),
	    {#type{def={'INSTANCE OF',DefinedObjectClass,Constraint}},Rest3};
	_ ->
	    {#type{def={'INSTANCE OF',DefinedObjectClass,[]}},Rest2}
    end;
parse_BuiltinType([{'INTEGER',_}|Rest]) ->
    case Rest of 
	[{'{',_}|Rest2] ->
	    {NamedNumberList,Rest3} = parse_NamedNumberList(Rest2),
	    case Rest3 of
		[{'}',_}|Rest4] ->
		    {#type{def={'INTEGER',NamedNumberList}},Rest4};
		_ ->
		    parse_error(Rest3)
	    end;
	 _ ->
	    {#type{def='INTEGER'},Rest}
    end;
parse_BuiltinType([{'NULL',_}|Rest]) ->
    {#type{def='NULL'},Rest};
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
		    parse_error(Rest3)
	    end
    end;
parse_BuiltinType([{'SEQUENCE',_},{'{',_}|Rest]) ->
    {ComponentTypeLists,Rest2} = parse_ComponentTypeLists(Rest),
    case Rest2 of
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
	    parse_error(Rest2)
    end;
parse_BuiltinType([{'SEQUENCE',_},{'OF',_}|
		   [#identifier{},{'<',_}|_]=Tokens0]) ->
    {Type,Tokens} = parse_SelectionType(Tokens0),
    {#type{def={'SEQUENCE OF',Type}},Tokens};
parse_BuiltinType([{'SEQUENCE',_},{'OF',_},#identifier{} |Rest]) ->
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
	    {ComponentTypeLists,Rest3}=
		parse_ComponentTypeLists2(Rest2,[#'EXTENSIONMARK'{pos=Line}]),
	    case Rest3 of
		[{'}',_}|Rest4] ->
		    {#type{def=#'SET'{components=ComponentTypeLists}},Rest4};
		_  ->
		    parse_error(Rest3)
	    end
    end;
parse_BuiltinType([{'SET',_},{'{',_}|Rest]) ->
    {ComponentTypeLists,Rest2} = parse_ComponentTypeLists(Rest),
    case Rest2 of
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
	    parse_error(Rest2)
    end;
parse_BuiltinType([{'SET',_},{'OF',_}|
		   [#identifier{},{'<',_}|_]=Tokens0]) ->
    {Type,Tokens} = parse_SelectionType(Tokens0),
    {#type{def={'SET OF',Type}},Tokens};
parse_BuiltinType([{'SET',_},{'OF',_},#identifier{}|Rest]) ->
%%TODO: take care of the identifier for something useful
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SET OF',Type}},Rest2};
parse_BuiltinType([{'SET',_},{'OF',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SET OF',Type}},Rest2};
parse_BuiltinType([{'GeneralizedTime',_}|Rest]) ->
    {#type{def='GeneralizedTime'},Rest};
parse_BuiltinType([{'UTCTime',_}|Rest]) ->
    {#type{def='UTCTime'},Rest};
parse_BuiltinType([{'ObjectDescriptor',_}|Rest]) ->
    {#type{def='ObjectDescriptor'},Rest};
parse_BuiltinType([{'ANY',_},{'DEFINED',_},{'BY',_},#identifier{val=Id}|Rest]) ->
    %% For compatibility with the old standard.
    {#type{def={'ANY_DEFINED_BY',Id}},Rest};
parse_BuiltinType([{'ANY',_}|Rest]) ->
    %% For compatibility with the old standard.
    {#type{def='ANY'},Rest};
parse_BuiltinType(Tokens) ->
    parse_ObjectClassFieldType(Tokens).


parse_TypeWithConstraint([{'SEQUENCE',_}|[{'(',_}|_]=Rest0]) ->
    {Constraint,Rest2} = parse_Constraint(Rest0),
    Rest4 = case Rest2 of
		[{'OF',_},#identifier{}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    parse_error(Rest2)
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SEQUENCE OF',Type},
	   constraint = merge_constraints([Constraint])},Rest5};

parse_TypeWithConstraint([{'SEQUENCE',_},{'SIZE',_}|[{'(',_}|_]=Rest0]) ->
    {Constraint,Rest2} = parse_Constraint(Rest0),
    #constraint{c=C} = Constraint,
    Constraint2 = Constraint#constraint{c={element_set,{'SizeConstraint',C},
					   none}},
    Rest4 = case Rest2 of
		[{'OF',_},#identifier{}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    parse_error(Rest2)
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SEQUENCE OF',Type}, constraint = merge_constraints([Constraint2])},Rest5};

parse_TypeWithConstraint([{'SET',_}|[{'(',_}|_]=Rest0]) ->
    {Constraint,Rest2} = parse_Constraint(Rest0),
    Rest4 = case Rest2 of
		[{'OF',_},#identifier{}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    parse_error(Rest2)
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SET OF',Type},
	   constraint = merge_constraints([Constraint])},Rest5};

parse_TypeWithConstraint([{'SET',_},{'SIZE',_}|[{'(',_}|_]=Rest0]) ->
    {Constraint,Rest2} = parse_Constraint(Rest0),
    #constraint{c=C} = Constraint,
    Constraint2 = Constraint#constraint{c={element_set,
					   {'SizeConstraint',C},none}},
    Rest4 = case Rest2 of
		[{'OF',_},#identifier{}|Rest3] ->
%%% TODO: make some use of the identifier, maybe useful in the XML mapping
		    Rest3;
		[{'OF',_}|Rest3] ->
		    Rest3;
		_ ->
		    parse_error(Rest2)
	    end,
    {Type,Rest5} = parse_Type(Rest4),
    {#type{def = {'SET OF',Type},
	   constraint = merge_constraints([Constraint2])},Rest5};

parse_TypeWithConstraint(Tokens) ->
    parse_error(Tokens).


%% --------------------------

parse_ReferencedType(Tokens) ->
    Flist = [fun parse_ParameterizedType/1,
	     fun parse_DefinedType/1,
	     fun parse_SelectionType/1,
	     fun parse_TypeFromObject/1],
    parse_or(Tokens, Flist).
    
parse_DefinedType([{typereference,L1,Module},
		   {'.',_},
		   {typereference,_,TypeName}|Tokens]) ->
    {#type{def = #'Externaltypereference'{pos=L1,module=Module,
					  type=TypeName}},Tokens};
parse_DefinedType([{typereference,_,_}=Tr|Tokens]) ->
    {#type{def=tref2Exttref(Tr)},Tokens};
parse_DefinedType(Tokens) ->
    parse_error(Tokens).

parse_SelectionType([#identifier{val=Name},{'<',_}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {#type{def={'SelectionType',Name,Type}},Rest2};
parse_SelectionType(Tokens) ->
    parse_error(Tokens).
    

resolve_module(Type) ->
    Current = get(asn1_module),
    Imports = get({Current, imports}),
    resolve_module(Type, Current, Imports).

resolve_module(_Type, Current, undefined) ->
    Current;
resolve_module(Type, Current, Imports) ->
    case [Mod || #'SymbolsFromModule'{symbols = S, module = Mod} <- Imports,
                 #'Externaltypereference'{type = T} <- S, 
                 Type =:= T] of
        [#'Externaltypereference'{type = Mod}|_] -> Mod; 
	%% This allows the same symbol to be imported several times
	%% which ought to be checked elsewhere and flagged as an error
        []  -> Current
    end.


parse_Constraints(Tokens) ->
    parse_Constraints(Tokens,[]).

parse_Constraints(Tokens,Acc) ->
    {Constraint,Rest} = parse_Constraint(Tokens),
    case Rest of
	[{'(',_}|_Rest2] ->
	    parse_Constraints(Rest, [Constraint|Acc]);
	_ ->
	    {lists:reverse(Acc, [Constraint]),Rest}
    end.

parse_Constraint([{'(',_}|Rest]) ->
    {Constraint,Rest2} = parse_ConstraintSpec(Rest),
    {Exception,Rest3} = parse_ExceptionSpec(Rest2),
    case Rest3 of
	[{')',_}|Rest4] ->
	    {#constraint{c=Constraint,e=Exception},Rest4};
	[_|_] ->
	    parse_error(Rest3)
    end.

parse_ConstraintSpec(Tokens) ->
    Flist = [fun parse_GeneralConstraint/1,
	     fun parse_SubtypeConstraint/1],
    parse_or(Tokens, Flist).

parse_ExceptionSpec([LPar={')',_}|Rest]) ->
    {undefined,[LPar|Rest]};
parse_ExceptionSpec([{'!',_}|Rest]) ->
    parse_ExceptionIdentification(Rest);
parse_ExceptionSpec(Tokens) ->
    parse_error(Tokens).

parse_ExceptionIdentification(Tokens) ->
    Flist = [fun parse_SignedNumber/1,
	     fun parse_DefinedValue/1,
	     fun parse_TypeColonValue/1],
    parse_or(Tokens, Flist).

parse_TypeColonValue(Tokens) ->
    {Type,Rest} = parse_Type(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Value,Rest3} = parse_Value(Rest2),
	    {{Type,Value},Rest3};
	[_|_] ->
	    parse_error(Rest)
    end.

parse_SubtypeConstraint(Tokens) ->
    parse_ElementSetSpecs(Tokens).

parse_ElementSetSpecs(Tokens) ->
    {RootElems,Rest} = parse_ElementSetSpec(Tokens),
    case Rest of
	[{',',_},{'...',_},{',',_}|Rest2] ->
	    {AdditionalElems,Rest3} = parse_ElementSetSpec(Rest2),
	    {{element_set,RootElems,AdditionalElems},Rest3};
	[{',',_},{'...',_}|Rest2] ->
	    {{element_set,RootElems,empty},Rest2};
	_ ->
	    {{element_set,RootElems,none},Rest}
    end.

parse_ElementSetSpec([{'ALL',_},{'EXCEPT',_}|Rest]) ->
    {Exclusions,Rest2} = parse_Elements(Rest),
    {{'ALL-EXCEPT',Exclusions},Rest2};
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
	{V1,V2} ->
	    {{union,V1,V2},Rest2}
    end.

parse_UnionsRec([{'|',_}|Rest]) ->
    {InterSec,Rest2} = parse_Intersections(Rest),
    {URec,Rest3} = parse_UnionsRec(Rest2),
    case {InterSec,URec} of
	{V1,[]} ->
	    {V1,Rest3};
	{V1,V2} ->
	    {{union,V1,V2},Rest3}
	end;
parse_UnionsRec([{'UNION',Info}|Rest]) ->
    parse_UnionsRec([{'|',Info}|Rest]);
parse_UnionsRec(Tokens) ->
    {[],Tokens}.

parse_Intersections(Tokens) ->
    {InterSec,Rest} = parse_IntersectionElements(Tokens),
    {IRec,Rest2} = parse_IElemsRec(Rest),
    case {InterSec,IRec} of
	{V1,[]} ->
	    {V1,Rest2};
	{V1,V2} ->
	    {{intersection,V1,V2},Rest2}
    end.

%% parse_IElemsRec(Tokens) -> Result
%% Result ::= {'SingleValue',ordered_set()} | list()
parse_IElemsRec([{'^',_}|Rest]) ->
    {InterSec,Rest2} = parse_IntersectionElements(Rest),
    {IRec,Rest3} = parse_IElemsRec(Rest2),
    case {InterSec,IRec} of
	{V1,[]} ->
	    {V1,Rest2};
	{V1,V2} ->
	    {{intersection,V1,V2},Rest3}
    end;
parse_IElemsRec([{'INTERSECTION',Info}|Rest]) ->
    parse_IElemsRec([{'^',Info}|Rest]);
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
	    {{'EXCEPT',InterSec,Exclusion},Rest3};
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
	[_|_] ->
	    parse_error(Rest2)
    end;
parse_Elements(Tokens) ->
    Flist = [fun parse_ObjectSetElements/1,
	     fun parse_SubtypeElements/1,
	     fun parse_Object/1,
	     fun parse_DefinedObjectSet/1],
    parse_or(Tokens, Flist).
    

%% --------------------------

parse_DefinedObjectClass([{typereference,_,ModName},{'.',_},
			  {typereference,Pos,Name}|Tokens]) ->
    Ext = #'Externaltypereference'{pos=Pos,
				   module=ModName,
				   type=Name},
    {Ext,Tokens};
parse_DefinedObjectClass([Tr={typereference,_,_ObjClName}|Rest]) ->
    {tref2Exttref(Tr),Rest};
parse_DefinedObjectClass(Tokens) ->
    parse_error(Tokens).

parse_ObjectClass(Tokens) ->
    Flist = [fun parse_ObjectClassDefn/1,
	     fun parse_DefinedObjectClass/1],
    parse_or(Tokens, Flist).

parse_ObjectClassDefn([{'CLASS',_},{'{',_}|Rest]) ->
    {Type,Rest2} = parse_FieldSpec(Rest),
    {WithSyntaxSpec,Rest3} = parse_WithSyntaxSpec(Rest2),
    {#objectclass{fields=Type,syntax=WithSyntaxSpec},Rest3};
parse_ObjectClassDefn(Tokens) ->
    parse_error(Tokens).

parse_FieldSpec(Tokens) ->
    parse_FieldSpec(Tokens,[]).

parse_FieldSpec(Tokens0, Acc) ->
    Fl = case Tokens0 of
	     [{valuefieldreference,_,_}|_] ->
		 %% 1) &field Type
		 %%    &object CLASS-NAME
		 %% 2) &field &FieldName
		 %% A fixed type field cannot be distinguished from
		 %% an object field without type information.
		 [fun parse_FixedTypeValueFieldSpec/1,
		  fun parse_VariableTypeValueFieldSpec/1];
	     [{typefieldreference,_,_}|_] ->
		 %% 1) &Set Type
		 %%    &ObjectSet CLASS-NAME
		 %% 2) &Set &FieldName
		 %% 3) &Type
		 %% A value set and an object cannot be distinguished
		 %% without type information.
		 [fun parse_FixedTypeValueSetFieldSpec/1,
		  fun parse_VariableTypeValueSetFieldSpec/1,
		  fun parse_TypeFieldSpec/1];
	     [_|_] ->
		 parse_error(Tokens0)
	 end,
    case parse_or(Tokens0, Fl) of
	{Type,[{'}',_}|Rest]} ->
	    {lists:reverse(Acc, [Type]),Rest};
	{Type,[{',',_}|Rest2]} ->
	    parse_FieldSpec(Rest2, [Type|Acc])
    end.

parse_PrimitiveFieldName([{typefieldreference,_,FieldName}|Rest]) ->
    {{typefieldreference,FieldName},Rest};
parse_PrimitiveFieldName([{valuefieldreference,_,FieldName}|Rest]) ->
    {{valuefieldreference,FieldName},Rest};
parse_PrimitiveFieldName(Tokens) ->
    parse_error(Tokens).

parse_FieldName(Tokens) ->
    {Field,Rest} = parse_PrimitiveFieldName(Tokens),
    parse_FieldName(Rest,[Field]).

parse_FieldName([{'.',_}|Rest0],Acc) ->
    {FieldName,Rest1} = parse_PrimitiveFieldName(Rest0),
    parse_FieldName(Rest1, [FieldName|Acc]);
parse_FieldName(Tokens, Acc) ->
    {lists:reverse(Acc),Tokens}.
    
parse_FixedTypeValueFieldSpec([{valuefieldreference,_,VFieldName}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {Unique,Rest3} = 
	case Rest2 of
	    [{'UNIQUE',_}|Rest4] ->
		{'UNIQUE',Rest4};
	    _  ->
		{undefined,Rest2}
	end,
    {OptionalitySpec,Rest5} = parse_ValueOptionalitySpec(Rest3),
    case is_end_delimiter(Rest5) of
	false -> parse_error(Rest5);
	true -> ok
    end,
    Tag = case Unique of
	      'UNIQUE' -> fixedtypevaluefield;
	      _ -> object_or_fixedtypevalue_field
	  end,
    {{Tag,VFieldName,Type,Unique,OptionalitySpec},Rest5}.

parse_VariableTypeValueFieldSpec([{valuefieldreference,_,VFieldName}|Rest0]) ->
    {FieldRef,Rest1} = parse_FieldName(Rest0),
    {OptionalitySpec,Rest} = parse_ValueOptionalitySpec(Rest1),
    case is_end_delimiter(Rest) of
	true ->
	    {{variabletypevaluefield,VFieldName,FieldRef,OptionalitySpec},
	     Rest};
	false ->
	    parse_error(Rest)
    end.

parse_TypeFieldSpec([{typefieldreference,_,Name}|Rest0]) ->
    {OptionalitySpec,Rest} = parse_TypeOptionalitySpec(Rest0),
    case is_end_delimiter(Rest) of
	true ->
	    {{typefield,Name,OptionalitySpec},Rest};
	false ->
	    parse_error(Rest)
    end.

parse_FixedTypeValueSetFieldSpec([{typefieldreference,_,Name}|Rest0]) ->
    {Type,Rest1} = parse_Type(Rest0),
    {OptionalitySpec,Rest} = parse_ValueSetOptionalitySpec(Rest1),
    case is_end_delimiter(Rest) of
	true ->
	    {{objectset_or_fixedtypevalueset_field,Name,Type,
	      OptionalitySpec},Rest};
	false ->
	    parse_error(Rest)
    end.

parse_VariableTypeValueSetFieldSpec([{typefieldreference,_,Name}|Rest0]) ->
    {FieldRef,Rest1} = parse_FieldName(Rest0),
    {OptionalitySpec,Rest} = parse_ValueSetOptionalitySpec(Rest1),
    case is_end_delimiter(Rest) of
	true ->
	    {{variabletypevaluesetfield,Name,FieldRef,OptionalitySpec},
	     Rest};
	false ->
	    parse_error(Rest)
    end.

is_end_delimiter([{',',_}|_]) -> true;
is_end_delimiter([{'}',_}|_]) -> true;
is_end_delimiter([_|_]) -> false.

parse_ValueOptionalitySpec(Tokens)->
    case Tokens of
	[{'OPTIONAL',_}|Rest] -> {'OPTIONAL',Rest};
	[{'DEFAULT',_}|Rest] ->
	    {Value,Rest2} = parse_Value(Rest),
	    {{'DEFAULT',Value},Rest2};
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

parse_WithSyntaxSpec([{'WITH',_},{'SYNTAX',_}|Rest]) ->
    {SyntaxList,Rest2} = parse_SyntaxList(Rest),
    {{'WITH SYNTAX',SyntaxList},Rest2};
parse_WithSyntaxSpec(Tokens) ->
    {[],Tokens}.

parse_SyntaxList([{'{',_}|Rest]) ->
    parse_SyntaxList(Rest,[]);
parse_SyntaxList(Tokens) ->
    parse_error(Tokens).

parse_SyntaxList(Tokens, Acc) ->
    {SyntaxList,Rest} = parse_TokenOrGroupSpec(Tokens),
    case Rest of
	[{'}',_}|Rest2] -> 
	    {lists:reverse(Acc, [SyntaxList]),Rest2};
	_ ->
	    parse_SyntaxList(Rest, [SyntaxList|Acc])
    end.

parse_TokenOrGroupSpec(Tokens) ->
    Flist = [fun parse_RequiredToken/1,
	     fun parse_OptionalGroup/1],
    parse_or(Tokens, Flist).

parse_RequiredToken([{typereference,_,WordName}|Rest]=Tokens) ->
    case is_word(WordName) of
	false ->
	    parse_error(Tokens);
	true ->
	    {WordName,Rest}
    end;
parse_RequiredToken([{',',L1}|Rest]) ->
    {{',',L1},Rest};
parse_RequiredToken([{WordName,_}|Rest]=Tokens) ->
    case is_word(WordName) of
	false ->
	    parse_error(Tokens);
	true ->
	    {WordName,Rest}
    end;
parse_RequiredToken(Tokens) ->
    parse_PrimitiveFieldName(Tokens).

parse_OptionalGroup([{'[',_}|Rest]) ->
    {Spec,Rest2} = parse_TokenOrGroupSpec(Rest),
    {SpecList,Rest3} = parse_OptionalGroup(Rest2,[Spec]),
    {SpecList,Rest3};
parse_OptionalGroup(Tokens) ->
    parse_error(Tokens).

parse_OptionalGroup([{']',_}|Rest],Acc) ->
    {lists:reverse(Acc),Rest};
parse_OptionalGroup(Tokens,Acc) ->
    {Spec,Rest} = parse_TokenOrGroupSpec(Tokens),
    parse_OptionalGroup(Rest,[Spec|Acc]).

parse_DefinedObject([#identifier{}=Id|Rest]) ->
    {{object,identifier2Extvalueref(Id)},Rest};
parse_DefinedObject([{typereference,L1,ModName},{'.',_},#identifier{val=ObjName}|Rest]) ->
    {{object, #'Externaltypereference'{pos=L1,module=ModName,type=ObjName}},Rest};
parse_DefinedObject(Tokens) ->
    parse_error(Tokens).

parse_ObjectAssignment([#identifier{pos=L1,val=ObjName}|Rest]) ->
    {Class,Rest2} = parse_DefinedObjectClass(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Object,Rest4} = parse_Object(Rest3),
	    {#typedef{pos=L1,name=ObjName,
		      typespec=#'Object'{classname=Class,def=Object}},Rest4};
	_ ->
	    parse_error(Rest2)
    end.

%% parse_Object(Tokens) -> Ret
%% Tokens    = [Tok]
%% Tok       = tuple()
%% Ret       = {object,_} | {object, _, _}
parse_Object(Tokens) ->
    %% The ObjectFromObject production is not included here,
    %% since it will have been catched by the ValueFromObject
    %% before we reach this point.
    Flist = [fun parse_ObjectDefn/1,
	     fun parse_DefinedObject/1],
    parse_or(Tokens, Flist).

parse_ObjectDefn(Tokens) ->
    Flist=[fun parse_DefaultSyntax/1,
	   fun parse_DefinedSyntax/1],
    parse_or(Tokens, Flist).

parse_DefaultSyntax([{'{',_}|Rest]) ->
    parse_DefaultSyntax(Rest,[]);
parse_DefaultSyntax(Tokens) ->
    parse_error(Tokens).

parse_DefaultSyntax(Tokens, Acc) ->
    {Setting,Rest} = parse_FieldSetting(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_DefaultSyntax(Rest2,[Setting|Acc]);
	[{'}',_}|Rest3] ->
	    {{object,defaultsyntax,lists:reverse(Acc, [Setting])},Rest3};
	_ ->
	    parse_error(Rest)
    end.

parse_FieldSetting(Tokens) ->
    {{_,PrimFieldName},Rest} = parse_PrimitiveFieldName(Tokens),
    {Setting,Rest2} = parse_Setting(Rest),
    {{PrimFieldName,Setting},Rest2}.

parse_DefinedSyntax([{'{',_}|Rest]) ->
    parse_DefinedSyntax(Rest, []);
parse_DefinedSyntax(Tokens) ->
    parse_error(Tokens).

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
parse_DefinedSyntaxToken([{',',_}=Comma|Rest]) ->
    {Comma,Rest};
%% ObjectClassFieldType or a defined type with a constraint.
%% Should also be able to parse a parameterized type. It may be
%% impossible to distinguish between a parameterized type and a Literal
%% followed by an object set.
parse_DefinedSyntaxToken([{typereference,_,_Name},{T,_}|_]=Tokens)
  when T =:= '.'; T =:= '(' ->
    parse_Setting(Tokens);
parse_DefinedSyntaxToken([{typereference,L1,Name}=TRef|Rest]=Tokens) ->
    case is_word(Name) of
	false ->
	    case lookahead_definedsyntax(Rest) of
		word_or_setting ->
		    {{setting,L1,tref2Exttref(TRef)},Rest};
		setting ->
		    parse_Setting(Tokens)
	    end;
	true ->
	    {{word_or_setting,L1,tref2Exttref(TRef)},Rest}
    end;
parse_DefinedSyntaxToken(Tokens) ->
    try parse_Setting(Tokens) of
	{_,_}=Result ->
	    Result
    catch
	throw:{asn1_error,_} ->
	    parse_Word(Tokens)
    end.

lookahead_definedsyntax([{typereference,_,Name}|_Rest]) ->
    case is_word(Name) of
	true -> word_or_setting;
	false -> setting
    end;
lookahead_definedsyntax([{'}',_}|_Rest]) ->
    word_or_setting;
lookahead_definedsyntax(_) ->
    setting.
	    
parse_Word([{Name,Pos}|Rest]=Tokens) ->
    case is_word(Name) of
	false ->
	    parse_error(Tokens);
	true ->
	    {{word_or_setting,Pos,tref2Exttref(Pos,Name)},Rest}
    end;
parse_Word(Tokens) ->
    parse_error(Tokens).

parse_Setting(Tokens) ->
    Flist = [{type_tag,fun parse_Type/1},
	     {value_tag,fun parse_Value/1},
	     {object_tag,fun parse_Object/1},
	     {objectset_tag,fun parse_ObjectSet/1}],
    case parse_or_tag(Tokens, Flist) of
	{{value_tag,_},_}=Result ->
	    %% Keep the value_tag.
	    Result;
	{{Tag,Setting},Rest} when is_atom(Tag) ->
	    %% Remove all other tags.
	    {Setting,Rest}
    end.

parse_DefinedObjectSet([{typereference,L1,ModuleName},{'.',_},
			{typereference,L2,ObjSetName}|Rest]) ->
    {{objectset,L1,#'Externaltypereference'{pos=L2,module=ModuleName,
					    type=ObjSetName}},Rest};
parse_DefinedObjectSet([{typereference,L1,ObjSetName}|Rest]) ->
    {{objectset,L1,#'Externaltypereference'{pos=L1,module=resolve_module(ObjSetName),
					    type=ObjSetName}},Rest};
parse_DefinedObjectSet(Tokens) ->
    parse_error(Tokens).

parse_ObjectSetAssignment([{typereference,L1,ObjSetName}|Rest]) ->
    {Class,Rest2} = parse_DefinedObjectClass(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {ObjectSet,Rest4} = parse_ObjectSet(Rest3),
	    {#typedef{pos=L1,name=ObjSetName,
		      typespec=#'ObjectSet'{class=Class,
					    set=ObjectSet}},Rest4};
	_ ->
	    parse_error(Rest2)
    end.

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
	_ ->
	    parse_error(Rest2)
    end;
parse_ObjectSet(Tokens) ->
    parse_error(Tokens).

parse_ObjectSetSpec([{'...',_},{',',_}|Tokens0]) ->
    {Elements,Tokens} = parse_ElementSetSpec(Tokens0),
    {{element_set,empty,Elements},Tokens};
parse_ObjectSetSpec([{'...',_}|Tokens]) ->
    {{element_set,empty,empty},Tokens};
parse_ObjectSetSpec(Tokens) ->
    parse_ElementSetSpecs(Tokens).

%% parse_ObjectSetElements(Tokens) -> {Result,Rest}
%% Result ::= {'ObjectSetFromObjects',Objects,Name} | {pos,ObjectSet,Params}
%% Objects ::= ReferencedObjects
%% ReferencedObjects ::= (see parse_ReferencedObjects/1)
%% Name ::= [FieldName]
%% FieldName ::= {typefieldreference,atom()} | {valuefieldreference,atom()}
%% ObjectSet ::= {objectset,integer(),#'Externaltypereference'{}}
%% Params ::= list() (see parse_ActualParameterList/1)
parse_ObjectSetElements(Tokens) ->
    Flist = [fun parse_ObjectSetFromObjects/1,
	     fun parse_ParameterizedObjectSet/1],
    parse_or(Tokens, Flist).

parse_ObjectClassFieldType(Tokens) ->
    {Class,Rest} = parse_DefinedObjectClass(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {FieldName,Rest3} = parse_FieldName(Rest2),
	    OCFT = #'ObjectClassFieldType'{
	      classname=Class,
	      class=Class,fieldname=FieldName},
	    {#type{def=OCFT},Rest3};
	_ ->
	    parse_error(Rest)
    end.

parse_ObjectClassFieldValue(Tokens) ->
    parse_OpenTypeFieldVal(Tokens).

parse_OpenTypeFieldVal(Tokens) ->
    {Type,Rest} = parse_Type(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Value,Rest3} = parse_Value(Rest2),
	    {{opentypefieldvalue,Type,Value},Rest3};
	_ ->
	    parse_error(Rest)
    end.

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
	     fun parse_ParameterizedObjectSet/1],
    parse_or(Tokens, Flist).

parse_ValueFromObject(Tokens) ->
    %% This production also matches ObjectFromObject.
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    case lists:last(Name) of
		{valuefieldreference,_} ->
		    {{'ValueFromObject',Objects,Name},Rest3};
		_ ->
		    parse_error(Rest2)
	    end;
	_ ->
	    parse_error(Rest)
    end.

parse_TypeFromObject(Tokens) ->
    {Objects,Rest} = parse_ReferencedObjects(Tokens),
    case Rest of
	[{'.',_}|Rest2] ->
	    {Name,Rest3} = parse_FieldName(Rest2),
	    case lists:last(Name) of
		{typefieldreference,_FieldName} ->
		    {#type{def={'TypeFromObject',Objects,Name}},Rest3};
		_ ->
		    parse_error(Rest2)
	    end;
	_ ->
	    parse_error(Rest)
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
		    parse_error(Rest2)
	    end;
	_ ->
	    parse_error(Rest)
    end.


%% X.682 constraint specification

parse_GeneralConstraint(Tokens) ->
    Flist = [fun parse_UserDefinedConstraint/1,
	     fun parse_TableConstraint/1,
	     fun parse_ContentsConstraint/1],
    parse_or(Tokens, Flist).

parse_UserDefinedConstraint([{'CONSTRAINED',_},{'BY',_},{'{',_},{'}',_}|Rest])->
    {{constrained_by,[]},Rest};
parse_UserDefinedConstraint([{'CONSTRAINED',_},
			     {'BY',_},
			     {'{',_}|Rest]) ->
    {Param,Rest2} = parse_UserDefinedConstraintParameter(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {{constrained_by,Param},Rest3};
	_ ->
	    parse_error(Rest2)
    end;
parse_UserDefinedConstraint(Tokens) ->
    parse_error(Tokens).

parse_UserDefinedConstraintParameter(Tokens) ->
    parse_UserDefinedConstraintParameter(Tokens, []).

parse_UserDefinedConstraintParameter(Tokens0, Acc) ->
    Flist = [fun parse_GovernorAndActualParameter/1,
	     fun parse_ActualParameter/1],
    case parse_or(Tokens0, Flist) of
	{Result,[{',',_}|Tokens]} ->
	    parse_UserDefinedConstraintParameter(Tokens, [Result|Acc]);
	{Result,Tokens} ->
	    {lists:reverse(Acc, [Result]),Tokens}
    end.

parse_GovernorAndActualParameter(Tokens) ->
    {Governor,Rest} = parse_Governor(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Params,Rest3} = parse_ActualParameter(Rest2),
	    {{'Governor_Params',Governor,Params},Rest3};
	_ ->
	    parse_error(Rest)
    end.

parse_TableConstraint(Tokens) ->
    Flist = [fun parse_ComponentRelationConstraint/1,
	     fun parse_SimpleTableConstraint/1],
    parse_or(Tokens, Flist).

parse_SimpleTableConstraint(Tokens) ->
    {ObjectSet,Rest} = parse_ObjectSet(Tokens),
    {{element_set,{simpletable,ObjectSet},none},Rest}.

parse_ComponentRelationConstraint([{'{',_}|Rest]) ->
    {ObjectSet,Rest2} = parse_DefinedObjectSet(Rest),
    case Rest2 of
	[{'}',_},{'{',_}|Rest3] ->
	    {AtNot,Rest4} = parse_AtNotationList(Rest3,[]),
	    case Rest4 of
		[{'}',_}|Rest5] ->
		    Ret = {element_set,
			   {componentrelation,ObjectSet,AtNot},
			   none},
		    {Ret,Rest5};
		_ ->
		    parse_error(Rest4)
	    end;
	_  ->
	    parse_error(Rest2)
    end;
parse_ComponentRelationConstraint(Tokens) ->
    parse_error(Tokens).

parse_AtNotationList(Tokens,Acc) ->
    {AtNot,Rest} = parse_AtNotation(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_AtNotationList(Rest2,[AtNot|Acc]);
	_  ->
	    {lists:reverse(Acc, [AtNot]),Rest}
    end.

parse_AtNotation([{'@',_},{'.',_}|Rest]) ->
    {CIdList,Rest2} = parse_ComponentIdList(Rest),
    {{innermost,CIdList},Rest2};
parse_AtNotation([{'@',_}|Rest]) ->
    {CIdList,Rest2} = parse_ComponentIdList(Rest),
    {{outermost,CIdList},Rest2};
parse_AtNotation(Tokens) ->
    parse_error(Tokens).

parse_ComponentIdList(Tokens) ->
    parse_ComponentIdList(Tokens,[]).

parse_ComponentIdList([#identifier{}=Id,{'.',_}|Rest], Acc) ->
    parse_ComponentIdList(Rest,[identifier2Extvalueref(Id)|Acc]);
parse_ComponentIdList([#identifier{}=Id|Rest], Acc) ->
    {lists:reverse(Acc, [identifier2Extvalueref(Id)]),Rest};
parse_ComponentIdList(Tokens,_) ->
    parse_error(Tokens).

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
    parse_error(Tokens).

% X.683 Parameterization of ASN.1 specifications

parse_Governor(Tokens) ->
    Flist = [fun parse_Type/1,
	     fun parse_DefinedObjectClass/1],
    parse_or(Tokens, Flist).

parse_ActualParameter(Tokens) ->
    Flist = [fun parse_Type/1,
	     fun parse_Value/1,
	     fun parse_ValueSet/1,
	     fun parse_DefinedObjectClass/1,
	     fun parse_Object/1,
	     fun parse_ObjectSet/1],
    parse_or(Tokens, Flist).

%% parse_ParameterizedTypeAssignment(Tokens) -> Result
%% Result = {#ptypedef{},Rest} | throw()
parse_ParameterizedTypeAssignment([{typereference,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Type,Rest4} = parse_Type(Rest3),
	    {#ptypedef{pos=L1,name=Name,args=ParameterList,typespec=Type},
	     Rest4};
	_ ->
	    parse_error(Rest2)
    end.

%% parse_ParameterizedValueAssignment(Tokens) -> Result
%% Result = {#pvaluedef{},Rest} | throw()
parse_ParameterizedValueAssignment([#identifier{pos=L1,val=Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    {Type,Rest3} = parse_Type(Rest2),
    case Rest3 of
	[{'::=',_}|Rest4] ->
	    {Value,Rest5} = parse_Value(Rest4),
	    {#pvaluedef{pos=L1,name=Name,args=ParameterList,type=Type,
			 value=Value},Rest5};
	_ ->
	    parse_error(Rest3)
    end.

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
	_ ->
	    parse_error(Rest3)
    end.

%% parse_ParameterizedObjectClassAssignment(Tokens) -> Result
%% Result = {#ptypedef{},Rest} | throw()
parse_ParameterizedObjectClassAssignment([{typereference,L1,Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Class,Rest4} = parse_ObjectClass(Rest3),
	    {#ptypedef{pos=L1,name=Name,args=ParameterList,typespec=Class},
	     Rest4};
	_ ->
	    parse_error(Rest2)
    end.

%% parse_ParameterizedObjectAssignment(Tokens) -> Result
%% Result = {#pobjectdef{},Rest} | throw()
parse_ParameterizedObjectAssignment([#identifier{pos=L1,val=Name}|Rest]) ->
    {ParameterList,Rest2} = parse_ParameterList(Rest),
    {Class,Rest3} = parse_DefinedObjectClass(Rest2),
    case Rest3 of
	[{'::=',_}|Rest4] ->
	    {Object,Rest5} = parse_Object(Rest4),
	    {#pobjectdef{pos=L1,name=Name,args=ParameterList,
			 class=Class,def=Object},Rest5};
	_ ->
	    parse_error(Rest3)
    end.

%% parse_ParameterList(Tokens) -> Result
%% Result = [Parameter]
%% Parameter = {Governor,Reference} | Reference
%% Governor = Type | DefinedObjectClass
%% Type = #type{}
%% DefinedObjectClass = #'Externaltypereference'{}
%% Reference = #'Externaltypereference'{} | #'Externalvaluereference'{}
parse_ParameterList([{'{',_}|Tokens]) ->
    parse_ParameterList(Tokens, []).

parse_ParameterList(Tokens,Acc) ->
    {Parameter,Rest} = parse_Parameter(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_ParameterList(Rest2, [Parameter|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse(Acc, [Parameter]),Rest3};
	_ ->
	    parse_error(Rest)
    end.

parse_Parameter(Tokens) ->
    Flist = [fun parse_ParamGovAndRef/1,
	     fun parse_Reference/1],
    parse_or(Tokens, Flist).

parse_ParamGovAndRef(Tokens) ->
    {ParamGov,Rest} = parse_ParamGovernor(Tokens),
    case Rest of
	[{':',_}|Rest2] ->
	    {Ref,Rest3} = parse_Reference(Rest2),
	    {{ParamGov,Ref},Rest3};
	_ ->
	    parse_error(Rest)
    end.

parse_ParamGovernor(Tokens) ->
    Flist = [fun parse_Governor/1,
	     fun parse_Reference/1],
    parse_or(Tokens, Flist).

parse_SimpleDefinedType([{typereference,L1,ModuleName},{'.',_},
			 {typereference,_,TypeName}|Rest]) ->
    {#'Externaltypereference'{pos=L1,module=ModuleName,
						 type=TypeName},Rest};
parse_SimpleDefinedType([Tref={typereference,_,_}|Rest]) ->
    {tref2Exttref(Tref),Rest};
parse_SimpleDefinedType(Tokens) ->
    parse_error(Tokens).

parse_SimpleDefinedValue([{typereference,L1,ModuleName},{'.',_},
			  #identifier{val=Value}|Rest]) ->
    {{simpledefinedvalue,#'Externalvaluereference'{pos=L1,module=ModuleName,
						   value=Value}},Rest};
parse_SimpleDefinedValue([#identifier{}=Id|Rest]) ->
    {{simpledefinedvalue,identifier2Extvalueref(Id)},Rest};
parse_SimpleDefinedValue(Tokens) ->
    parse_error(Tokens).

parse_ParameterizedType(Tokens) ->
    %% May also be a parameterized class.
    {Type,Rest} = parse_SimpleDefinedType(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {#type{def={pt,Type,Params}},Rest2}.

parse_ParameterizedValue(Tokens) ->
    %% May also be a parameterized object.
    {Value,Rest} = parse_SimpleDefinedValue(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{pv,Value,Params},Rest2}.

parse_ParameterizedObjectSet(Tokens) ->
    {ObjectSet,Rest} = parse_DefinedObjectSet(Tokens),
    {Params,Rest2} = parse_ActualParameterList(Rest),
    {{pos,ObjectSet,Params},Rest2}.

parse_ActualParameterList([{'{',_}|Rest]) ->
    parse_ActualParameterList(Rest,[]);
parse_ActualParameterList(Tokens) ->
    parse_error(Tokens).

parse_ActualParameterList(Tokens,Acc) ->
    {Parameter,Rest} = parse_ActualParameter(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_ActualParameterList(Rest2,[Parameter|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse(Acc, [Parameter]),Rest3};
	_ ->
	    parse_error(Rest)
    end.

%% Test whether Token is allowed in a syntax list.
is_word(Token) ->
    List = atom_to_list(Token),
    case not_allowed_word(List) of
	true -> false;
	false -> is_word_1(List)
    end.

is_word_1([H|T]) ->
    check_first(H) andalso check_rest(T).

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

check_first(C) ->
    $A =< C andalso C =< $Z.

check_rest([R|Rs]) when $A =< R, R =< $Z; R =:= $- ->
    check_rest(Rs);
check_rest([]) ->
    true;
check_rest(_) ->
    false.

%%%
%%% Parse alternative type lists for CHOICE.
%%%

parse_AlternativeTypeLists(Tokens0) ->
    {Root,Tokens1} = parse_AlternativeTypeList(Tokens0),
    case Tokens1 of
	[{',',_}|Tokens2] ->
	    {ExtMarker,Tokens3} = parse_ExtensionAndException(Tokens2),
	    {ExtAlts,Tokens4} =	parse_ExtensionAdditionAlternatives(Tokens3),
	    {_,Tokens} = parse_OptionalExtensionMarker(Tokens4, []),
	    {Root++ExtMarker++ExtAlts,Tokens};
	Tokens ->
	    {Root,Tokens}
    end.

parse_ExtensionAndException([{'...',L}|Tokens0]) ->
    {[#'EXTENSIONMARK'{pos=L}],
     case Tokens0 of
	 [{'!',_}|Tokens1] ->
	     {_,Tokens} = parse_ExceptionIdentification(Tokens1),
	     Tokens;
	 _ ->
	     Tokens0
     end}.

parse_AlternativeTypeList([#identifier{}|_]=Tokens0) ->
    {AltType,Tokens} = parse_NamedType(Tokens0),
    parse_AlternativeTypeList_1(Tokens, [AltType]);
parse_AlternativeTypeList(Tokens) ->
    parse_error(Tokens).

parse_AlternativeTypeList_1([{',',_}|[#identifier{}|_]=Tokens0], Acc) ->
    {AltType,Tokens} = parse_NamedType(Tokens0),
    parse_AlternativeTypeList_1(Tokens, [AltType|Acc]);
parse_AlternativeTypeList_1(Tokens, Acc) ->
    {lists:reverse(Acc),Tokens}.

parse_ExtensionAdditionAlternatives([{',',_}|_]=Tokens0) ->
    parse_ExtensionAdditionAlternativesList(Tokens0, []);
parse_ExtensionAdditionAlternatives(Tokens) ->
    {[],Tokens}.

parse_ExtensionAdditionAlternativesList([{',',_}|Tokens1]=Tokens0, Acc) ->
    try parse_ExtensionAdditionAlternative(Tokens1) of
	{ExtAddAlt,Tokens2} ->
	    parse_ExtensionAdditionAlternativesList(Tokens2, [ExtAddAlt|Acc])
    catch
	throw:{asn1_error,_} ->
	    {lists:append(lists:reverse(Acc)),Tokens0}
    end;
parse_ExtensionAdditionAlternativesList(Tokens, Acc) ->
    {lists:append(lists:reverse(Acc)),Tokens}.

parse_ExtensionAdditionAlternative([#identifier{}|_]=Tokens0) ->
    {NamedType,Tokens} = parse_NamedType(Tokens0),
    {[NamedType],Tokens};
parse_ExtensionAdditionAlternative([{'[',_},{'[',_}|Tokens0]) ->
    Tokens2 = case Tokens0 of
		  [{number,_,_},{':',_}|Tokens1] -> Tokens1;
		  _ -> Tokens0
	      end,
    {GroupList,Tokens3} = parse_AlternativeTypeList(Tokens2),
    case Tokens3 of
	[{']',_},{']',_}|Tokens] ->
	    {GroupList,Tokens};
	_ ->
	    parse_error(Tokens3)
    end;
parse_ExtensionAdditionAlternative(Tokens) ->
    parse_error(Tokens).

%%%
%%% End of parsing of alternative type lists.
%%%

parse_NamedType([#identifier{pos=L1,val=Idname}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    {#'ComponentType'{pos=L1,name=Idname,typespec=Type,prop=mandatory},Rest2};
parse_NamedType(Tokens) ->
    parse_error(Tokens).

%%%
%%% Parse component type lists for SEQUENCE and SET.
%%%

parse_ComponentTypeLists(Tokens) ->
    parse_ComponentTypeLists(Tokens, []).

parse_ComponentTypeLists([#identifier{}|_Rest0]=Tokens, Clist) ->
    {CompList,Rest1} = parse_ComponentTypeList(Tokens,[]),
    parse_ComponentTypeLists(Rest1,Clist++CompList);
parse_ComponentTypeLists([{'COMPONENTS',_},{'OF',_}|_]=Tokens,Clist) ->
    {CompList,Rest1} = parse_ComponentTypeList(Tokens, []),
    parse_ComponentTypeLists(Rest1, Clist++CompList);
parse_ComponentTypeLists([{',',L1},{'...',_},{'!',_}|Rest02],Clist0) when Clist0 =/= []->
    {_,Rest03} = parse_ExceptionIdentification(Rest02),
    %% Exception info is currently thrown away
    parse_ComponentTypeLists2(Rest03,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_ComponentTypeLists([{',',_},{'...',L1}|Rest02],Clist0) when Clist0 =/= []->
    parse_ComponentTypeLists2(Rest02,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_ComponentTypeLists([{'...',L1}|Rest02],Clist0) ->
    parse_ComponentTypeLists2(Rest02,Clist0++[#'EXTENSIONMARK'{pos=L1}]);
parse_ComponentTypeLists(Tokens = [{'}',_L1}|_Rest02],Clist0) ->
    {Clist0,Tokens};
parse_ComponentTypeLists(Tokens, _) ->
    parse_error(Tokens).

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


parse_ComponentTypeList([{',',_}|[#identifier{}|_]=Tokens0], Acc) when Acc =/= [] ->
    {ComponentType,Tokens} = parse_ComponentType(Tokens0),
    parse_ComponentTypeList(Tokens, [ComponentType|Acc]);
parse_ComponentTypeList([{',',_}|[{'COMPONENTS',_},{'OF',_}|_]=Tokens0], Acc) when Acc =/= [] ->
    {ComponentType,Tokens} = parse_ComponentType(Tokens0),
    parse_ComponentTypeList(Tokens, [ComponentType|Acc]);
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
    parse_error(Tokens).

parse_ExtensionAdditions(Tokens=[{',',_}|_],Clist) ->
    {ExtAddList,Rest2} = parse_ExtensionAdditionList(Tokens,[]),
    {Clist++ExtAddList,Rest2};
parse_ExtensionAdditions(Tokens,Clist) ->
    %% Empty
    {Clist,Tokens}.

parse_ExtensionAdditionList([{',',_}|[#identifier{}|_]=Tokens0], Acc) ->
    {ComponentType,Tokens} = parse_ComponentType(Tokens0),
    parse_ExtensionAdditionList(Tokens, [ComponentType|Acc]);
parse_ExtensionAdditionList([{',',_}|[{'COMPONENTS',_},{'OF',_}|_]=Tokens0], Acc) ->
    {ComponentType,Tokens} = parse_ComponentType(Tokens0),
    parse_ExtensionAdditionList(Tokens, [ComponentType|Acc]);
parse_ExtensionAdditionList([{',',_},{'[',_},{'[',_}|Tokens], Acc) ->
    {ExtAddGroup,Rest2} = parse_ExtensionAdditionGroup(Tokens),
    parse_ExtensionAdditionList(Rest2,[ExtAddGroup|Acc]);
parse_ExtensionAdditionList([{'}',_}|_]=Tokens, Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ExtensionAdditionList([{',',_},{'...',_}|_]=Tokens, Acc) ->
    {lists:reverse(Acc),Tokens};
parse_ExtensionAdditionList(Tokens, _) ->
    parse_error(Tokens).


parse_ExtensionAdditionGroup([{number,_,Num},{':',_}|Tokens]) ->
    parse_ExtensionAdditionGroup2(Tokens, Num);
parse_ExtensionAdditionGroup(Tokens) ->
    parse_ExtensionAdditionGroup2(Tokens, undefined).

parse_ExtensionAdditionGroup2(Tokens, Num) ->
    {CompTypeList,Rest} = parse_ComponentTypeList(Tokens,[]),
    case Rest of
	[{']',_},{']',_}|Rest2] ->
	    {[{'ExtensionAdditionGroup',Num}|CompTypeList] ++
		['ExtensionAdditionGroupEnd'],Rest2};
	_ ->
	    parse_error(Rest)
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

%%%
%%% Parse ENUMERATED.
%%%

parse_Enumerations(Tokens0) ->
    {Root,Tokens1} = parse_Enumeration(Tokens0),
    case Tokens1 of
	[{',',_},{'...',_},{',',_}|Tokens2] ->
	    {Ext,Tokens} = parse_Enumeration(Tokens2),
	    {Root++['EXTENSIONMARK'|Ext],Tokens};
	[{',',_},{'...',_}|Tokens] ->
	    {Root++['EXTENSIONMARK'],Tokens};
	_ ->
	    case get(extensiondefault) of
		'IMPLIED' ->
		    {Root++['EXTENSIONMARK'],Tokens1};
		_ ->
		    {Root,Tokens1}
	    end
    end.

parse_Enumeration(Tokens0) ->
    {Item,Tokens} = parse_EnumerationItem(Tokens0),
    parse_Enumeration_1(Tokens, [Item]).

parse_Enumeration_1([{',',_}|Tokens1]=Tokens0, Acc) ->
    try parse_EnumerationItem(Tokens1) of
	{Item,Tokens} ->
	    parse_Enumeration_1(Tokens, [Item|Acc])
    catch
	throw:{asn1_error,_} ->
	    {lists:reverse(Acc),Tokens0}
    end;
parse_Enumeration_1(Tokens, Acc) ->
    {lists:reverse(Acc),Tokens}.

parse_EnumerationItem([#identifier{},{'(',_}|_]=Tokens) ->
    parse_NamedNumber(Tokens);
parse_EnumerationItem([#identifier{val=Id}|Tokens]) ->
    {Id,Tokens};
parse_EnumerationItem(Tokens) ->
    parse_error(Tokens).

%%%
%%% End of parsing of ENUMERATED.
%%%

parse_NamedNumberList(Tokens) ->
    parse_NamedNumberList(Tokens, []).

parse_NamedNumberList(Tokens, Acc) ->
    {NamedNum,Rest} = parse_NamedNumber(Tokens),
    case Rest of
	[{',',_}|Rest2] ->
	    parse_NamedNumberList(Rest2,[NamedNum|Acc]);
	_ ->
	    {lists:reverse(Acc, [NamedNum]),Rest}
    end.

parse_NamedNumber([#identifier{val=Name},{'(',_}|Rest]) ->
    Flist = [fun parse_SignedNumber/1,
	     fun parse_DefinedValue/1],
    case parse_or(Rest, Flist) of
	{NamedNum,[{')',_}|Rest2]} ->
	    {{'NamedNumber',Name,NamedNum},Rest2};
	_ ->
	    parse_error(Rest)
    end;
parse_NamedNumber(Tokens) ->
    parse_error(Tokens).

parse_SignedNumber([{number,_,Value}|Rest]) ->
    {Value,Rest};
parse_SignedNumber(Tokens) ->
    parse_error(Tokens).

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
	    parse_error(Rest3)
    end.

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
    parse_or(Tokens, Flist).

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
    parse_or(Tokens, Flist);
parse_BuiltinValue([#identifier{val=IdName},{':',_}|Rest]) ->
    {Value,Rest2} = parse_Value(Rest),
    {{'CHOICE',{IdName,Value}},Rest2};
parse_BuiltinValue([{'NULL',_},{':',_}|_]=Tokens)  ->
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
parse_BuiltinValue(Tokens) ->
    parse_ObjectClassFieldValue(Tokens).

parse_DefinedValue(Tokens) ->
    Flist = [fun parse_ParameterizedValue/1,
	     fun parse_DefinedValue2/1],
    parse_or(Tokens, Flist).

parse_DefinedValue2([{typereference,L1,Tname},
		     {'.',_},
		     #identifier{val=Idname}|Rest]) ->
    {#'Externalvaluereference'{pos=L1,module=Tname,value=Idname},Rest};
%% valuereference
parse_DefinedValue2([#identifier{}=Id|Rest]) ->
    {identifier2Extvalueref(Id),Rest};
parse_DefinedValue2(Tokens) ->
    parse_error(Tokens).


parse_SequenceValue([{'{',_}|Tokens]) ->
    parse_SequenceValue(Tokens, []).

parse_SequenceValue([#identifier{pos=Pos,val=IdName}|Rest],Acc) ->
    {Value,Rest2} = parse_Value(Rest),
    SeqTag = #seqtag{pos=Pos,module=get(asn1_module),val=IdName},
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_SequenceValue(Rest3, [{SeqTag,Value}|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse(Acc, [{SeqTag,Value}]),Rest3};
	_ ->
	    parse_error(Rest2)
    end;
parse_SequenceValue(Tokens,_Acc) ->
    parse_error(Tokens).

parse_SequenceOfValue([{'{',_}|Tokens]) ->
    parse_SequenceOfValue(Tokens, []).

parse_SequenceOfValue(Tokens,Acc) ->
    {Value,Rest2} = parse_Value(Tokens),
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_SequenceOfValue(Rest3,[Value|Acc]);
	[{'}',_}|Rest3] ->
	    {lists:reverse(Acc, [Value]),Rest3};
	_ ->
	    parse_error(Rest2)
    end.

parse_ValueSetTypeAssignment([{typereference,L1,Name}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {ValueSet,Rest4} = parse_ValueSet(Rest3),
	    {#valuedef{pos=L1,name=Name,type=Type,value=ValueSet,
		       module=get(asn1_module)},Rest4};
	_ ->
	    parse_error(Rest2)
    end.

parse_ValueSet([{'{',_}|Rest]) ->
    {Elems,Rest2} = parse_ElementSetSpecs(Rest),
    case Rest2 of
	[{'}',_}|Rest3] ->
	    {{valueset,Elems},Rest3};
	_ ->
	    parse_error(Rest2)
    end;
parse_ValueSet(Tokens) ->
    parse_error(Tokens).

parse_ValueAssignment([#identifier{pos=L1,val=IdName}|Rest]) ->
    {Type,Rest2} = parse_Type(Rest),
    case Rest2 of
	[{'::=',_}|Rest3] ->
	    {Value,Rest4} = parse_Value(Rest3),
	    {#valuedef{pos=L1,name=IdName,type=Type,value=Value,
		       module=get(asn1_module)},Rest4};
	_ ->
	    parse_error(Rest2)
    end.

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
	    parse_error(Rest)
    end;
parse_SubtypeElements([{'WITH',_},{'COMPONENTS',_},{'{',_}|Tokens]) ->
    {Constraint,Rest} = parse_TypeConstraints(Tokens),
    case Rest of
	[{'}',_}|Rest2] ->
	    {{'WITH COMPONENTS',{'FullSpecification',Constraint}},Rest2};
	_ ->
	    parse_error(Rest)
    end;
parse_SubtypeElements([{'PATTERN',_}|Tokens]) ->
    {Value,Rest} = parse_Value(Tokens),
    {{pattern,Value},Rest};
parse_SubtypeElements(Tokens) ->
    Flist = [fun parse_ContainedSubtype/1,
	     fun parse_Value/1, 
	     fun parse_MIN/1,
	     fun parse_Type/1],
    case parse_or(Tokens, Flist) of
	{#type{},_}=Result ->
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
    parse_error(Tokens).

parse_UpperEndpoint([{'<',_}|Rest]) ->
    parse_UpperEndpoint(lt,Rest);
parse_UpperEndpoint(Tokens) ->
    parse_UpperEndpoint(false,Tokens).

parse_UpperEndpoint(Lt,Tokens) ->
    Flist = [fun parse_MAX/1,
	     fun parse_Value/1],
    case parse_or(Tokens, Flist) of
	{Value,Rest2} when Lt =:= lt ->
	    {{lt,Value},Rest2};
	{Value,Rest2} ->
	    {Value,Rest2}
    end.

parse_MIN([{'MIN',_}|T]) ->
    {'MIN',T};
parse_MIN(Tokens) ->
    parse_error(Tokens).

parse_MAX([{'MAX',_}|T]) ->
    {'MAX',T};
parse_MAX(Tokens) ->
    parse_error(Tokens).

parse_TypeConstraints(Tokens) ->
    parse_TypeConstraints(Tokens, []).

parse_TypeConstraints([#identifier{}|Rest], Acc) ->
    {ComponentConstraint,Rest2} = parse_ComponentConstraint(Rest),
    case Rest2 of
	[{',',_}|Rest3] ->
	    parse_TypeConstraints(Rest3, [ComponentConstraint|Acc]);
	_ ->
	    {lists:reverse(Acc, [ComponentConstraint]),Rest2}
    end;
parse_TypeConstraints(Tokens, _) ->
    parse_error(Tokens).

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


merge_constraints(Clist) ->
    merge_constraints(Clist, [], []).

merge_constraints([#constraint{c=C,e=E}|T], Cacc0, Eacc0) ->
    Eacc = case E of
	       undefined -> Eacc0;
	       E -> [E|Eacc0]
	   end,
    Cacc = [C|Cacc0],
    merge_constraints(T, Cacc, Eacc);
merge_constraints([], Cacc, []) ->
    lists:reverse(Cacc);
merge_constraints([], Cacc, Eacc) ->
    lists:reverse(Cacc) ++ [{element_set,{'Errors',Eacc},none}].

get_line({Token,Pos,_}) when is_integer(Pos), is_atom(Token) ->
    Pos;
get_line({Token,Pos}) when is_integer(Pos),is_atom(Token) ->
    Pos.

get_token({valuefieldreference,_,FieldName}) ->
    list_to_atom([$&|atom_to_list(FieldName)]);
get_token({typefieldreference,_,FieldName}) ->
    list_to_atom([$&|atom_to_list(FieldName)]);
get_token({Token,Pos,Value}) when is_integer(Pos), is_atom(Token) ->
    Value;
get_token({'$end',Pos}) when is_integer(Pos) ->
    'END-OF-FILE';
get_token({Token,Pos}) when is_integer(Pos),is_atom(Token) ->
    Token.

tref2Exttref(#typereference{pos=Pos,val=Name}) ->
    #'Externaltypereference'{pos=Pos,
			     module=resolve_module(Name),
			     type=Name}.

tref2Exttref(Pos,Name) ->
    #'Externaltypereference'{pos=Pos,
			     module=resolve_module(Name),
			     type=Name}.

identifier2Extvalueref(#identifier{pos=Pos,val=Name}) ->
    #'Externalvaluereference'{pos=Pos,
			      module=resolve_module(Name),
			      value=Name}.

parse_error(Tokens) ->
    throw({asn1_error,{parse_error,Tokens}}).
