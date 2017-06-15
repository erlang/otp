%%-*-erlang-*-
%%--------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : xmerl_sax_old_dom.erl
%% Description : 
%%
%% Created : 02 Oct 2008 
%%----------------------------------------------------------------------
-module(xmerl_sax_old_dom).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("xmerl_sax_old_dom.hrl").
-include("xmerl_internal.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 initial_state/0,
	 get_dom/1,
	 event/3
        ]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
        ]).

%%======================================================================
%% Macros
%%======================================================================
%%----------------------------------------------------------------------
%% Error handling
%%----------------------------------------------------------------------
-define(error(Reason), 
	throw({xmerl_sax_old_dom_error, Reason})).

%%======================================================================
%% Records
%%======================================================================

%%----------------------------------------------------------------------
%% State record for the validator
%%----------------------------------------------------------------------
-record(xmerl_sax_old_dom_state, {
	  tags=[],         %% Tag stack
	  cno=[],          %% Current node number
	  namespaces = [], %% NameSpace stack
	  dom=[]           %% DOM structure         
	 }).

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: initial_state() -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
initial_state() ->
    #xmerl_sax_old_dom_state{}.

%%----------------------------------------------------------------------
%% Function: get_dom(State) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
get_dom(#xmerl_sax_old_dom_state{dom=Dom}) ->
    Dom.

%%----------------------------------------------------------------------
%% Function: event(Event, LineNo, State) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
event(Event, _LineNo, State) ->
    build_dom(Event, State).


%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function  : build_dom(Event, State) -> Result
%% Parameters: Event = term()
%%             State = #xmerl_sax_old_dom_state{}
%% Result    : #xmerl_sax_old_dom_state{} |
%% Description: 
%%----------------------------------------------------------------------

%% Document
%%----------------------------------------------------------------------
build_dom(startDocument, State) ->
    State#xmerl_sax_old_dom_state{dom=[startDocument]};
build_dom(endDocument, 
	  #xmerl_sax_old_dom_state{dom=[#xmlElement{content=C} = Current |D]} = State) ->
    case D of
	[startDocument] ->
	    State#xmerl_sax_old_dom_state{dom=[Current#xmlElement{
							    content=lists:reverse(C)
							   }]};
	[#xmlDecl{} = Decl, startDocument] ->
	    State#xmerl_sax_old_dom_state{dom=[Decl, Current#xmlElement{
						 content=lists:reverse(C)
						}]};
	_ -> 
            %% endDocument is also sent by the parser when a fault occur to tell 
            %% the event receiver that no more input will be sent
	    State
    end;

%% Element
%%----------------------------------------------------------------------
build_dom({startElement, Uri, LocalName, QName, Attributes}, 
	  #xmerl_sax_old_dom_state{tags=T, cno=CN, namespaces=NS, dom=D} = State) ->

    A = parse_attributes(LocalName, Attributes),
    {Num, NewCN} =
	case CN of
	    [] ->
		{1, [1]};
	    [ N |CNs] ->
		{N, [1, N+1 |CNs]}
	end,

    NsInfo = 
	case QName of
	    {[], _} -> [];
	    QN -> QN
	end,
    NameAsAtom = convert_qname_to_atom(QName), 

    State#xmerl_sax_old_dom_state{tags=[{NameAsAtom, Num} |T],
				  cno=NewCN,
				  dom=[#xmlElement{name=NameAsAtom, 
						   expanded_name=NameAsAtom,
						   nsinfo=NsInfo,
						   namespace=#xmlNamespace{default=list_to_atom(Uri),
									   nodes=NS},
						   pos=Num,
						   parents=T,
						   attributes=lists:reverse(A),
						   xmlbase="."
						  } | D]};
build_dom({endElement, _Uri, LocalName, QName}, 
	  #xmerl_sax_old_dom_state{tags=[_ |T],
				   cno=[_ |CN],
				   dom=[#xmlElement{name=CName, content=C} = Current, 
					#xmlElement{content=PC} = Parent | D]} = State) ->
    case convert_qname_to_atom(QName) of
	CName ->	    
	    State#xmerl_sax_old_dom_state{tags=T,
					  cno=CN,
					  dom=[Parent#xmlElement{
						 content=[Current#xmlElement{
							    content=lists:reverse(C)
							   }
							  |PC]
						} | D]};
	_ ->
	    ?error("Got end of element: " ++ LocalName ++ " but expected: " ++ 
		   Current#xmlElement.name)
    end;

%% Text 
%%----------------------------------------------------------------------
build_dom({characters, String},
	  #xmerl_sax_old_dom_state{tags=T, 
				   cno=[Num |CN],
				   dom=[#xmlElement{content=C} = Current| D]} = State) ->
    State#xmerl_sax_old_dom_state{cno=[Num+1 |CN], 
				  dom=[Current#xmlElement{content=[#xmlText{value=String, parents=T, pos=Num, type=text}
								   |C]} | D]};
build_dom({ignorableWhitespace, String},
	  #xmerl_sax_old_dom_state{tags=T, 
				   cno=[Num |CN],
				   dom=[#xmlElement{content=C} = Current| D]} = State) ->
    State#xmerl_sax_old_dom_state{cno=[Num+1 |CN],
				  dom=[Current#xmlElement{content=[#xmlText{value=String, 
									    parents=T, pos=Num, 
									    type=text}
								   |C]} | D]};

%% Comments
%%----------------------------------------------------------------------
build_dom({comment, String},
	  #xmerl_sax_old_dom_state{tags=T, 
				   cno=[Num |CN],
				   dom=[#xmlElement{content=C} = Current| D]} = State) ->
    State#xmerl_sax_old_dom_state{cno=[Num+1 |CN],
				  dom=[Current#xmlElement{content=[#xmlComment{parents=T, pos=Num, value=String}|C]} | D]};

%% NameSpaces
%%----------------------------------------------------------------------
build_dom({startPrefixMapping, [], _Uri}, State) -> 
    State;
build_dom({startPrefixMapping, Prefix, Uri},
	  #xmerl_sax_old_dom_state{namespaces=NS} = State) -> 
    State#xmerl_sax_old_dom_state{namespaces=[{Prefix, list_to_atom(Uri)} |NS]};
build_dom({endPrefixMapping, Prefix},
	  #xmerl_sax_old_dom_state{namespaces=[{Prefix, _} |NS]} = State) -> 
    State#xmerl_sax_old_dom_state{namespaces=NS};

%% Processing instructions
%%----------------------------------------------------------------------
build_dom({processingInstruction,"xml", PiData},
	  #xmerl_sax_old_dom_state{dom=D} = State) ->
    {Vsn, PiData1}  = find_and_remove_attribute("version", PiData, []),
    {Enc, PiData2}  = find_and_remove_attribute("encoding", PiData1, []),
    {Standalone, PiData3}  = find_and_remove_attribute("standalone", PiData2, yes),
    State#xmerl_sax_old_dom_state{dom=[#xmlDecl{vsn=Vsn, encoding=Enc, standalone=Standalone, attributes=PiData3}| D]};
build_dom({processingInstruction, PiTarget, PiData},
	  #xmerl_sax_old_dom_state{cno=[Num |CN],
				   dom=[#xmlElement{content=C} = Current| D]} = State) ->
    State#xmerl_sax_old_dom_state{cno=[Num+1 |CN], 
				  dom=[Current#xmlElement{content=[#xmlPI{name=PiTarget,pos=Num, value=PiData}
								   |C]} | D]};
%% Default
%%----------------------------------------------------------------------
build_dom(_E, State) ->
    State. 


%%----------------------------------------------------------------------
%% Function  : parse_attributes(ElName, Attributes) -> Result
%% Parameters: 
%% Result    : 
%% Description: 
%%----------------------------------------------------------------------
parse_attributes(ElName, Attributes) ->
    parse_attributes(ElName, Attributes, 1, []).

parse_attributes(_, [], _, Acc) ->
    Acc;
parse_attributes(ElName, [{_Uri, Prefix, LocalName, AttrValue} |As], N, Acc) ->  
    Name = convert_qname_to_atom({Prefix,LocalName}),
    NsInfo = 
	case Prefix of
	    [] -> [];
	    P -> {P,LocalName}
	end,
    parse_attributes(ElName, As, N+1, [#xmlAttribute{name=Name,
						     pos=N, 
						     nsinfo=NsInfo,
						     value=AttrValue,
						     normalized=false} |Acc]).

%%----------------------------------------------------------------------
%% Function  : convert_qname_to_atom(QName) -> Result
%% Parameters: 
%% Result    : 
%% Description: 
%%----------------------------------------------------------------------
convert_qname_to_atom({[], N}) ->
    list_to_atom(N);
convert_qname_to_atom({P,N}) ->
    list_to_atom(P ++ ":" ++ N).

%%----------------------------------------------------------------------
%% Function  : find_and_remove_attribute(Key, Data, Default) -> Result
%% Parameters: 
%% Result    : 
%% Description: 
%%----------------------------------------------------------------------
find_and_remove_attribute(Key, Data, Default) ->
    case lists:keysearch(Key, 1, Data) of
	{value, {Key, Value}} ->
	    Data2 = lists:keydelete(Key, 1, Data),
	    {Value, Data2};
	false ->
	    {Default, Data}
    end.
