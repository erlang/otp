%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(xmerl_validate).

-export([validate/2]).


-include("xmerl.hrl").		% record def, macros
-include("xmerl_internal.hrl").


%% +type validate(xmerl_scanner(),xmlElement())->
%%              xmlElment() | {error,tuple()}.
validate(#xmerl_scanner{doctype_name=DTName,doctype_DTD=OpProv},
	 #xmlElement{name=Name})
  when DTName=/=Name,OpProv=/=option_provided->
    {error, {mismatched_root_element,Name,DTName}};
validate(#xmerl_scanner{rules=Rules}=S,
	 XML=#xmlElement{name=Name})->
    catch do_validation(read_rules(Rules,Name),XML,Rules,S);
validate(_, XML) ->
    {error, {no_xml_element, XML}}.



%% +type validate(rules(),xmlElement())->
%%              {ok,xmlElement()} | {error,tuple()}.
do_validation(undefined,#xmlElement{name=Name}, _Rules,_S) ->
    {error,{unknown_element,Name}};
do_validation(El_Rule,XML,Rules,S)->
    case catch valid_attributes(El_Rule#xmlElement.attributes,
			  XML#xmlElement.attributes,S) of
	{'EXIT',Reason} ->
	    {error,Reason};
	{error,Reason} ->
	    {error,Reason};
	Attr_2->
%	    XML_=XML#xmlElement{attributes=Attr_2},
	    El_Rule_Cont = El_Rule#xmlElement.content,
	    WSActionMode = ws_action_mode(El_Rule#xmlElement.elementdef,
					  El_Rule_Cont,S),
	    XML_Cont = XML#xmlElement.content,
	    check_direct_ws_SDD(XML_Cont,WSActionMode),
	    case valid_contents(El_Rule_Cont,
				XML_Cont,Rules,S,WSActionMode) of
		{error,Reason}->
		    {error,Reason};
		{error,Reason,N}->
		    {error,Reason,N};
		XMLS ->
		    XML#xmlElement{attributes=Attr_2,content=XMLS}
	    end
    end.

check_direct_ws_SDD(XML,always_preserve) ->
    case XML of
	[#xmlText{}|_Rest] ->
	    exit({error,{illegal_whitespace_standalone_doc,XML}});
	_ -> ok
    end,
    case lists:reverse(XML) of
	[#xmlText{}|_Rest2] ->
	    exit({error,{illegal_whitespace_standalone_doc,XML}});
	_ -> ok
    end;
check_direct_ws_SDD(_,_) -> ok.

ws_action_mode({external,_},Content,#xmerl_scanner{standalone=yes}) ->
    case element_content(Content) of
	children ->
	    always_preserve;
	_ ->
	    preserve
    end;
ws_action_mode(_,_,_) ->
    preserve.

element_content(A) when is_atom(A),A /= any, A /= empty ->
    children;
element_content({choice,L}) when is_list(L) ->
    element_content(L);
element_content({seq,L}) when is_list(L) ->
    element_content(L);
element_content(['#PCDATA'|_T]) ->
    mixed;
element_content('#PCDATA') ->
    mixed;
element_content({'*',Rest}) -> 
    element_content(Rest);
element_content(_) -> children.

%% +type read_rules(DTD::atom(),Element_Name::atom())->
%%              undefined | xmlElement().
read_rules(_, pcdata) ->
    pcdata;
read_rules(T, Name) ->
    case ets:lookup(T, {elem_def, Name}) of
	[] ->
	    undefined;
	[{_K, V}] ->
	    V
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Attributes Validation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% +deftype attribute_rule() = {Attr_Name::atom(),attribute_type(),
%%                              attribute_priority()}.

%% +type valid_attributes([attribute_rule()],[xmlAttribute()])->
%%              [xmlAttribute()] | {error,attribute_unknow}.
valid_attributes(All_Attr,[#xmlAttribute{}|_T]=Attr,S)->
    single_ID_definition(All_Attr),
    vc_Name_Token_IDREFS(All_Attr,Attr),
    lists:foreach(fun(#xmlAttribute{name=Name})->
			  case is_attribute_exist(Name,All_Attr) of
			      true ->
				  ok;
			      false ->
				  exit({error,{attribute_unknown,Name}})
			  end
		  end,
		  Attr),
    lists:flatten(lists:foldl(fun({Name,DataType,IF,DefDecl,Env},Attr_2)->
				      Attr_2++
					  [valid_attribute(Name,DataType,IF,
							   DefDecl,Attr,Env,S)] 
			      end,[],All_Attr));
valid_attributes([],[],_) ->
    [];
valid_attributes(All_Attr,[],S) ->
    single_ID_definition(All_Attr),
    lists:flatten(lists:foldl(fun({Name,DataType,IF,DefDecl,Env},Attr_2)->
				      Attr_2++[valid_attribute(Name,
							       DataType,IF,
							       DefDecl,
							       [],
							       Env,S)] 
			      end,[],All_Attr)).

%%%%  [60]      DefaultDecl::=   
%%%%                              '#REQUIRED' | '#IMPLIED' 
%%%%                            | (('#FIXED' S)? AttValue)
%% +deftype attribute_priority = '#REQUIRED'|'#FIXED'|'#IMPLIED'.

%% +type valid_attribute(Name::atom(),DataType::attribute_value(),
%%                       IF::attribute_priority(),[xmlAttribute()])->
%%         [xmlAttribute()] | exit().
valid_attribute(Name,DataType,IF,DefaultDecl,List_of_Attributes,Env,S)->
    SA = S#xmerl_scanner.standalone,
    Attr=search_attr(Name,List_of_Attributes),
    check_SDD_validity(SA,Env,Attr,IF),
    case {DefaultDecl,IF,Attr} of
	{'#REQUIRED',_,no_attribute}->
	    exit({error,{Name,is_required}});
	{'#IMPLIED',_,no_attribute}->
	    []; %% and no default value
	{'#FIXED',DefVal,#xmlAttribute{value=DefVal}=Attr} ->
	    Attr;
	{'#FIXED',A,no_attribute} ->
	    #xmlAttribute{name=Name,value=A}; % FIXED declare value becomes default.
	{'#FIXED',A,B} ->
	    exit({error,{fixed_default_value_missmatch,A,B}});
	{_,Value,no_attribute} when is_list(Value)->
	    #xmlAttribute{name=Name,value=Value};
	{_,_,#xmlAttribute{}=Attr}->
	    %% do test data value, and default_value
	    test_attribute_value(DataType,Attr,IF,S);
	{DefDecl,Else,XML} ->
	    exit({error,{unknow_attribute_type,DefDecl,Else,XML}})
    end.

vc_Name_Token_IDREFS([{Name,Type,_,_,_}|Rest],Attrs) 
  when Type=='NMTOKEN';Type=='NMTOKENS'->
    case lists:keysearch(Name,#xmlAttribute.name,Attrs) of
	{value,A} ->
	    valid_nmtoken_value(A#xmlAttribute.value,Type);
	_ -> ok
    end,
    vc_Name_Token_IDREFS(Rest,Attrs);
vc_Name_Token_IDREFS([{Name,Type,_,_,_}|Rest],Attrs) 
  when Type=='IDREFS'->
    case lists:keysearch(Name,#xmlAttribute.name,Attrs) of
	{value,A} ->
	    valid_IDREFS(A#xmlAttribute.value,Type);
	_ -> ok
    end,
    vc_Name_Token_IDREFS(Rest,Attrs);
vc_Name_Token_IDREFS([_H|Rest],Attrs) ->
    vc_Name_Token_IDREFS(Rest,Attrs);
vc_Name_Token_IDREFS([],_) -> ok.

valid_nmtoken_value([],'NMTOKENS') ->
    exit({error,{at_least_one_Nmtoken_required}});
% valid_nmtoken_value([H|_T] = L,'NMTOKENS') when is_list(H) ->
%     ValidChar =
% 	fun(X) ->
% 		case xmerl_lib:is_namechar(X) of
% 		    false ->
% 			exit({error,{invalid_character_in_Nmtoken,X}});
% 		    _ -> ok
% 		end
% 	end,
%     ValidCharList =
% 	fun([Nmtok|T],F) -> 
% 		lists:foreach(ValidChar,Nmtok),
% 		F(T,F);
% 	   ([],_) -> ok
% 	end,
%     ValidCharList(L,ValidChar);
valid_nmtoken_value(Nmtok,_) ->
    ValidChar =
	fun(X) when ?whitespace(X),Nmtok=='NMTOKENS' ->
		ok;
	   (X) ->
		case xmerl_lib:is_namechar(X) of
		    false ->
			exit({error,{invalid_character_in_Nmtoken,X}});
		    _ -> ok
		end
	end,
    lists:foreach(ValidChar,Nmtok).

valid_IDREFS([],'IDREFS') ->
    exit({error,{at_least_one_IDREF_Name_required}});
valid_IDREFS(_Str,'IDREFS') ->
    ok.
    
single_ID_definition([{_,'ID',_,_,_}=Att1|Rest]) ->
    case lists:keysearch('ID',2,Rest) of
	{value,Att2} ->
	    exit({error,{just_one_ID_definition_allowed,Att1,Att2}});
	_ -> ok
    end;
single_ID_definition([_H|T]) ->
    single_ID_definition(T);
single_ID_definition([]) ->
    ok.
    
check_SDD_validity(yes,{external,_},#xmlAttribute{name=Name,normalized=true},_) ->
    exit({error,{externally_defed_attribute_normalized_in_standalone_doc,Name}});
check_SDD_validity(yes,{external,_},no_attribute,V) when V /= no_value->
    exit({error,{externally_defed_attribute_with_default_value_missing_in_standalone_doc}});
check_SDD_validity(_,_,_,_) ->
    ok.
    
search_attr(Name,[#xmlAttribute{name=Name}=H|_T])->
    H;
search_attr(Name,[#xmlAttribute{}|T])-> 
    search_attr(Name,T);
search_attr(_Name,_T) ->
    no_attribute.

is_attribute_exist(Name,[{Name,_,_,_,_}|_T])->
    true;
is_attribute_exist(Name,[{_Attr,_,_,_,_}|T]) ->
    is_attribute_exist(Name,T);
is_attribute_exist(_Name,[]) ->
    false.

%%%%[54] AttType::=     StringType | TokenizedType | EnumeratedType 
%%%%[55] StringType::=  'CDATA'
%%%%[56] TokenizedType::= 'ID'|'IDREF'| 'IDREFS'|'ENTITY'| 'ENTITIES'
%%%%                     | 'NMTOKEN'| 'NMTOKENS'
%%%%[57] EnumeratedType::= NotationType | Enumeration 
%%%%[58] NotationType::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')' 
%%%%[59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

%% +deftype attribute_type()-> 'CDATA' | 'ID'|'IDREF'| 'IDREFS'|'ENTITY'| 
%%                             'ENTITIES'| 'NMTOKEN'| 'NMTOKENS'
%%                             {enumeration,[List_of_value::atom()]}.

%% +type test_attribute_value(attribute_type(),xmlAttribute())->
%%             xmlAttribute()| exit.
%%%% test the constraint validity of Attribute value.
test_attribute_value('CDATA',#xmlAttribute{}=Attr,_,_) ->
    Attr;
test_attribute_value('NMTOKEN',#xmlAttribute{name=Name,value=V}=Attr,
		     Default,_S) ->
    Fun =
	fun (X)->
		case xmerl_lib:is_namechar(X) of
		    true->
			ok;
		    false->
			%%?dbg("nmtoken,value_incorrect:  ~p~n",[V]),
			exit({error,{invalid_value_nmtoken,Name,V}})
		end
	end,
    lists:foreach(Fun,V),
    if 
	is_list(Default) ->
	    lists:foreach(Fun,Default);
	true -> ok
    end,
    Attr;
test_attribute_value('NMTOKENS',#xmlAttribute{name=Name,value=V}=Attr,
		     Default,_S) ->
    Fun = 
	fun (X)->
		case xmerl_lib:is_namechar(X) of
		    true->
			ok;
		    false when ?whitespace(X)->
			ok;
		    false ->
			exit({error,{invalid_value_nmtokens,Name,V}})
		end
	end,
    lists:foreach(Fun,V),
    if 
	is_list(Default) ->
	    lists:foreach(Fun,Default);
	true -> ok
    end,
    Attr;
test_attribute_value(Ent,#xmlAttribute{name=_Name,value=V}=Attr,_Default,
		     S=#xmerl_scanner{rules_read_fun=Read}) 
  when Ent == 'ENTITY'; Ent == 'ENTITIES'->
    %% The default value is already checked
    NameListFun = 
	fun([],Acc,_) ->
		lists:reverse(Acc);
	   (Str,Acc,Fun) -> 
		{N,Str2} = scan_name(Str,[]),
		Fun(Str2,[N|Acc],Fun)
	end,
    NameList = NameListFun(V,[],NameListFun),
    VC_Entity_Name = 
	fun(X) ->
		case Read(entity,X,S) of
		    {_,external,{_,{ndata,_}}} ->
			ok;
		    _ -> exit({error,{vc_Entity_Name,X,V}})
                end
	end,
    lists:foreach(VC_Entity_Name,NameList),
    Attr;
test_attribute_value({Type,L},#xmlAttribute{value=Value}=Attr,Default,_S)
  when Type == enumeration; Type == notation ->
    ValidDefault = 
	if 
	    is_atom(Default) -> true;
	    true -> lists:member(list_to_atom(Default),L)
	end,
    NoDuplicatesFun =
	fun(_,_,notation) -> true;
	   ([],_,_) -> true;
	   ([H|T],F,Enum) ->
		case lists:member(H,T) of
		    true -> false;
		    _ -> F(T,F,Enum)
		end
	end,
    NoDuplicates = NoDuplicatesFun(L,NoDuplicatesFun,Type),
    case {lists:member(list_to_atom(Value),L),ValidDefault,NoDuplicates} of
	{true,true,true}->
	    Attr;
	{false,_,_} ->
	    exit({error,{attribute_value_unknow,Value,{list,L}}});
	{_,false,_} ->
	    exit({error,{attribute_default_value_unknow,Default,{list,L}}});
	{_,_,false} ->
	    exit({error,{duplicate_tokens_not_allowed,{list,L}}})
    end;
test_attribute_value(_Rule,Attr,_,_) ->
%    ?dbg("Attr Value*****~nRule~p~nValue~p~n",[Rule,Attr]),
    Attr.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Contents Validation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%Element-content Models
%%%%[47] children::= (choice | seq) ('?' | '*' | '+')?
%%%%[48] cp::=       (Name | choice | seq) ('?' | '*' | '+')?
%%%%[49] choice::=  '(' S? cp ( S? '|' S? cp )+ S? ')'
%%%%[50] seq::=     '(' S? cp ( S? ',' S? cp )* S? ')'
%%%%[51] Mixed::=   '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' 
%%%%              | '(' S? '#PCDATA' S? ')' 


%% +type valid_contents([rule()],[xmlElement()])->
%%              [xmlElement() | {error,???}.
valid_contents(Rule, XMLS, Rules, S, WSActionMode)->
    case parse(Rule, XMLS, Rules, WSActionMode, S) of
	{error, Reason} ->
	    {error, Reason};
	{error, Reason, N} ->
	    {error, Reason, N};
	{XML_N, Rest} ->   %The list may consist of xmlComment{} records
	    case lists:dropwhile(fun(X) when is_record(X, xmlComment) -> true; (_) -> false end, Rest) of 
		[] ->
		    lists:flatten(XML_N);
		[#xmlElement{name=Name} |_T] ->
		    exit({error, {element, Name, isnt_comprise_in_the_rule, Rule}});
		[#xmlText{} = Txt |_T] ->
		    exit({error, {element, text, Txt, isnt_comprise_in_the_rule, Rule}})
	    end
    end.

parse({'*', SubRule}, XMLS, Rules, WSaction, S)->
    star(SubRule, XMLS, Rules, WSaction, [], S); 
parse({'+',SubRule}, XMLS, Rules, WSaction, S) ->
    plus(SubRule, XMLS, Rules, WSaction, S);
parse({choice,CHOICE}, XMLS, Rules, WSaction, S)->
%    case XMLS of
%	[] ->
%	    ?dbg("~p~n",[{choice,CHOICE,[]}]);
%	[#xmlElement{name=Name,pos=Pos}|_] ->
%	    ?dbg("~p~n",[{choice,CHOICE,{Name,Pos}}]);
%	[#xmlText{value=V}|_] ->
%	    ?dbg("~p~n",[{choice,CHOICE,{text,V}}])
%    end,
    choice(CHOICE, XMLS, Rules, WSaction, S);
parse(empty, [], _Rules, _WSaction, _S) ->
    {[], []};
parse({'?', SubRule}, XMLS, Rules, _WSaction, S)->
    question(SubRule, XMLS, Rules, S);
parse({seq,List}, XMLS, Rules, WSaction, S) ->
    seq(List, XMLS, Rules, WSaction, S);
parse(El_Name, [#xmlElement{name=El_Name} = XML |T], Rules, _WSaction, S) 
  when is_atom(El_Name)->
    case do_validation(read_rules(Rules, El_Name), XML, Rules, S) of
	{error, R} ->
%	    {error,R};
	    exit(R);
	{error, R, _N}->
%	    {error,R,N};
	    exit(R);
	XML_->
	    {[XML_], T}
    end;
parse(any, Cont, Rules, _WSaction, S) ->
    case catch parse_any(Cont, Rules, S) of
	Err = {error, _} -> Err;
	ValidContents -> {ValidContents, []}
    end;
parse(El_Name, [#xmlElement{name=Name} |_T] = XMLS, _Rules, _WSa, _S) when is_atom(El_Name) ->
    {error,
     {element_seq_not_conform,{wait, El_Name}, {is, Name}},
     {{next, XMLS}, {act, []}}};
parse(El_Name, [#xmlComment{} |T], Rules, WSa, S) ->
    parse(El_Name, T, Rules, WSa, S);
parse(_El_Name, [#xmlPI{} = H |T], _Rules, _WSa, _S) ->
    {[H], T};
parse('#PCDATA', XMLS, _Rules, _WSa, _S)->
    %%% PCDATA it is 0 , 1 or more #xmlText{}.
    parse_pcdata(XMLS);
parse(El_Name, [#xmlText{}|_T] = XMLS, _Rules, _WSa, _S)->
    {error,
     {text_in_place_of, El_Name},
     {{next, XMLS}, {act, []}}};
parse([], _, _, _, _) ->
    {error, no_rule};
parse(Rule, [], _, _, _) ->
    {error, {no_xml_element, Rule}}.

parse_any([],_Rules,_S) ->
    [];
parse_any([H|T],Rules,S) ->
    case parse_any(H,Rules,S) of
	[Cont] ->
	   [Cont|parse_any(T,Rules,S)];
	Err -> throw(Err)
    end;
parse_any(#xmlElement{}=XML,Rules,S) ->
    case do_validation(read_rules(Rules,el_name(XML)),XML,Rules,S) of
	{error,R} ->
	    {error,R};
	{error,R,N}->
	    {error,R,N};
	XML_->
	    [XML_]
    end;
parse_any(El,_Rules,_S) ->
    [El].



%% XXX remove first function clause
% choice(_Choice,[#xmlText{}=T|R],_Rules) ->
%     {[T],R};
choice([CH|CHS],[_XML|_T]=XMLS,Rules,WSaction,S)->
    {WS,XMLS1} = whitespace_action(XMLS,ws_action(WSaction,remove)),
    case parse(CH,XMLS1,Rules,ws_action(WSaction,remove),S) of
	{error,_R} ->
	    choice(CHS,XMLS,Rules,WSaction,S);
	{error,_R,_N} ->
	    choice(CHS,XMLS,Rules,WSaction,S); %% XXX add a case {[],XML}
	{[],XMLS1} -> %% Maybe a sequence with * or ? elements that
                      %% didn't match
 	    case CHS of
 		[] -> % choice has succeded but without matching XMLS1
 		    {[],XMLS1};
 		_ -> % there are more choice alternatives to try with
 		    choice(CHS,XMLS1,Rules,WSaction,S)
	    end;
%%	    choice(CHS,XMLS1,Rules,WSaction,S);
	{Tree,XMLS2}->
	    {WS2,XMLS3} = whitespace_action(XMLS2,ws_action(WSaction,remove)),
	    {WS2++[Tree]++WS,XMLS3}
    end;
choice([],XMLS,_,WSaction,_S)->
    case whitespace_action(XMLS,ws_action(WSaction,remove)) of
	Res={_,[]} -> Res;
	_ ->
	    {error,element_unauthorize_in_choice,{{next,XMLS},{act,[]}}}
    end;
choice(_,[],_,_,_S) ->
    {[],[]}.

plus(Rule,XMLS,Rules,WSaction,S) ->
    %% 1 or more
    {WS,XMLS1}=whitespace_action(XMLS,WSaction),
    case parse(Rule,XMLS1,Rules,WSaction,S) of
	{error, Reason,_XML} ->
	    {error, Reason};
	{error, X} ->
	    {error, X};
	{Tree, XMLS2} ->
	    case star(Rule, XMLS2,Rules,WSaction,[],S) of
		{[], _} ->
		    {WS++[Tree], XMLS2};
		{Tree_1, XMLS3} ->
		    {WS++[Tree]++Tree_1, XMLS3}
	    end
    end.

star(_Rule,XML,_Rules,_WSa,Tree,_S) when length(XML)==0->
    {[Tree],[]};
star(Rule,XMLS,Rules,WSaction,Tree,S) ->
    {WS,XMLS1} = whitespace_action(XMLS,WSaction),
    case parse(Rule,XMLS1,Rules,WSaction,S) of
	{error, _E, {{next,N},{act,A}}}->
	    %%?dbg("Error~p~n",[_E]),
	    {WS++Tree++A,N};
	{error, _E}->
	    %%?dbg("Error~p~n",[_E]),
%	    {WS++[Tree],[]};
	    case  whitespace_action(XMLS,ws_action(WSaction,remove)) of
		{[],_} ->
		    {WS++[Tree],XMLS};
		{WS2,XMLS2} ->
		    {WS2++[Tree],XMLS2}
	    end;
	{Tree1,XMLS2}->
	    star(Rule,XMLS2,Rules,WSaction,Tree++WS++[Tree1],S)
    end.

question(_Rule, [],_Rules,_S) ->
    {[],[]};
question(Rule, Toks,Rules,S) ->
    %% 0 or 1
    case parse(Rule, Toks,Rules,preserve,S) of
	{error, _E, _Next}->
	    {[],Toks};
	{error, _E} ->
	    {[], Toks};
	{T,Toks1} -> 
	    {T, Toks1}
    end.

seq(H,Toks,Rules,WSaction,S)->
    case seq2(H,Toks,Rules,[],WSaction,S) of
	{error,E}->
	    {error,E};
	{error,R,N}->
	    {error,R,N};
	{Tree,Toks2}->
	    {Tree,Toks2}
    end.

seq2([],[],_,Tree,_WSa,_S)->
    {Tree,[]};
% seq2([],[#xmlElement{name=Name}|_T]=XMLS,_,Tree,_WSa,_S)->
%     {error,{sequence_finish,Name,isnt_in_the_right_place},
%      {{next,XMLS},{act,Tree}}};
seq2([],[#xmlText{}]=XML,_,Tree,_WSa,_S)->
    case whitespace_action(XML,remove) of
	{[],_} ->
	    {error,sequence_finish,{{next,XML},{act,Tree}}};
	{WS,Rest} ->
	    {WS++Tree,Rest}
    end;
seq2([],Rest,_,Tree,_WSa,_S) ->
     {WS,Rest2}=whitespace_action(Rest,remove),
    {WS++Tree,Rest2};
seq2([H|T],Toks,Rules,Tree,WSaction,S) ->
    {WS,Toks1} = whitespace_action(Toks,ws_action(WSaction,remove)),
    case parse(H,Toks1,Rules,remove,S) of %% H maybe only match parts of Toks 
	{error,Reason,_XML}->
	    {error,Reason};
	{error,E}->
	    {error,E};
	{[],Toks2}->
	    seq2(T,Toks2,Rules,Tree,WSaction,S);
	{Tree1,Toks2} when is_list(Tree1)->
	    seq2(T,Toks2,Rules,Tree++WS++Tree1,WSaction,S);
	{Tree1,Toks2}->
	    seq2(T,Toks2,Rules,Tree++WS++[Tree1],WSaction,S)
    end.

el_name(#xmlElement{name=Name})->
    Name.

parse_pcdata([#xmlText{}=H|T])->
    parse_pcdata(T,[H]);
parse_pcdata([#xmlComment{}|T])->
    parse_pcdata(T,[]);
parse_pcdata(H) ->
    {[],H}.

parse_pcdata([#xmlText{}=H|T],Acc)->
    parse_pcdata(T,Acc++[H]);
parse_pcdata([#xmlComment{}|T],Acc)->
    parse_pcdata(T,Acc);
parse_pcdata(H,Acc) ->
    {Acc,H}.

whitespace([]) ->
    true;
whitespace([H|T]) when ?whitespace(H) ->
    whitespace(T);
whitespace(_) ->
    false.

whitespace_action(XML,remove) ->
    whitespace_remove(XML,[]);
whitespace_action(XML,_) ->
    {[],XML}.

whitespace_remove([#xmlText{value=V,type=text}=T|R]=L,Acc) ->
    case whitespace(V) of
	true ->
	    whitespace_remove(R,[T|Acc]);
	_ ->
	    {lists:reverse(Acc),L}
    end;
whitespace_remove(L,Acc) ->
    {lists:reverse(Acc),L}.

ws_action(always_preserve=A,_)  ->
    A;
ws_action(_,B) ->
    B.

scan_name(N,_) when is_atom(N) ->
    N;
scan_name([$\s|T],Acc) ->
    {list_to_atom(lists:reverse(Acc)),T};
scan_name([H|T],Acc) ->
    scan_name(T,[H|Acc]);
scan_name("",Acc) ->
    {list_to_atom(lists:reverse(Acc)),[]}.
