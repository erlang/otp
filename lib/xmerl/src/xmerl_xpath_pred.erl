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

%% Description  : Helper module to xmerl_xpath: XPATH predicates.

-module(xmerl_xpath_pred).

%% API
-export([eval/2]).


%% internal functions (called via apply/3)
-export([boolean/1, boolean/2,
	 ceiling/2,
	 concat/2,
	 contains/2,
	 count/2,
	 floor/2,
	 fn_false/2,
	 fn_not/2,
	 fn_true/2,
	 id/2,
	 lang/2,
	 last/2,
	 'local-name'/2,
	 'namespace-uri'/2,
	 name/2,
	 string/2,
	 nodeset/1,
	 'normalize-space'/2,
	 number/1, number/2,
	 position/2,
	 round/2,
	 'starts-with'/2,
	 string/1,
	 'string-length'/2,
	 substring/2,
	 'substring-after'/2,
	 'substring-before'/2,
	 sum/2,
	 translate/2]).
-export([core_function/1]).	 

-include("xmerl.hrl").
-include("xmerl_internal.hrl").
-include("xmerl_xpath.hrl").

%% -record(obj, {type,
%% 	      value}).


-define(string(X), #xmlObj{type = string,
			   value = X}).
-define(nodeset(X), #xmlObj{type = nodeset,
			    value = X}).
-define(number(X), #xmlObj{type = number,
			   value = X}).
-define(boolean(X), #xmlObj{type = boolean,
			    value = X}).




eval(Expr, C = #xmlContext{context_node = #xmlNode{pos = Pos}}) ->
    Obj = expr(Expr, C),
    Res = case Obj#xmlObj.type of
	      number when Obj#xmlObj.value == Pos ->
		  true;
	      number ->
		  false;
	      boolean ->
		  Obj#xmlObj.value;
	      _ ->
		  mk_boolean(C, Obj)
	  end,
%    ?dbg("eval(~p, ~p) -> ~p~n", [Expr, Pos, Res]),
    Res.


string(X) ->
    ?string(X).

nodeset(X) -> 
    ?nodeset(X).

number(X) ->
    ?number(X).

boolean(X) ->
    ?boolean(X).


expr({arith, Op, E1, E2}, C) ->
    arith_expr(Op, E1, E2, C);
expr({comp, Op, E1, E2}, C) ->
    comp_expr(Op, E1, E2, C);
expr({bool, Op, E1, E2}, C) ->
    bool_expr(Op, E1, E2, C);
expr({'negative', E}, C) ->
    N = mk_number(C, E),
    - N;
expr({number, N}, _C) ->
    ?number(N);
expr({literal, S}, _C) ->
    ?string(S);
expr({function_call, F, Args}, C) ->
    case core_function(F) of
	{true, F1} ->
	    ?MODULE:F1(C, Args);
	true ->
	    ?MODULE:F(C, Args);
	false ->
	    %% here, we should look up the function in the context provided 
	    %% by the caller, but we haven't figured this out yet.
	    exit({not_a_core_function, F})
    end;
expr({path, Type, PathExpr}, C) ->
    #state{context=#xmlContext{nodeset = NS}} =
	xmerl_xpath:eval_path(Type, PathExpr, C),
    ?nodeset(NS);
expr(Expr, _C) ->
    exit({unknown_expr, Expr}).


arith_expr('+', E1, E2, C) ->
    ?number(mk_number(C, E1) + mk_number(C, E2));
arith_expr('-', E1, E2, C) ->
    ?number(mk_number(C, E1) - mk_number(C, E2));
arith_expr('*', E1, E2, C) ->
    ?number(mk_number(C, E1) * mk_number(C, E2));
arith_expr('div', E1, E2, C) ->
    ?number(mk_number(C, E1) / mk_number(C, E2));
arith_expr('mod', E1, E2, C) ->
    ?number(mk_number(C, E1) rem mk_number(C, E2)).

comp_expr('>', E1, E2, C) ->
    N1 = expr(E1,C),
    N2 = expr(E2,C),
    ?boolean(compare_ineq_format(N1,N2,C) > compare_ineq_format(N2,N1,C));
comp_expr('<', E1, E2, C) ->
    N1 = expr(E1,C),
    N2 = expr(E2,C),
    ?boolean(compare_ineq_format(N1,N2,C) > compare_ineq_format(N2,N1,C));
comp_expr('>=', E1, E2, C) ->
    N1 = expr(E1,C),
    N2 = expr(E2,C),
    ?boolean(compare_ineq_format(N1,N2,C) > compare_ineq_format(N2,N1,C));
comp_expr('<=', E1, E2, C) ->
    N1 = expr(E1,C),
    N2 = expr(E2,C),
    ?boolean(compare_ineq_format(N1,N2,C) > compare_ineq_format(N2,N1,C));
comp_expr('=', E1, E2, C) ->
    N1 = expr(E1,C),
    N2 = expr(E2,C),
    ?boolean(compare_eq_format(N1,N2,C) == compare_eq_format(N2,N1,C));
comp_expr('!=', E1, E2, C) ->
    N1 = expr(E1,C),
    N2 = expr(E2,C),
    ?boolean(compare_eq_format(N1,N2,C) /= compare_eq_format(N2,N1,C)).

bool_expr('or', E1, E2, C) ->
    ?boolean(mk_boolean(C, E1) or mk_boolean(C, E2));
bool_expr('and', E1, E2, C) ->
    ?boolean(mk_boolean(C, E1) and mk_boolean(C, E2)).

%% According to chapter 3.4 in XML Path Language ver 1.0 the format of
%% the compared objects are depending on the type of the other
%% object. 
%% 1. Comparisons involving node-sets is treated equally despite
%% of which comparancy operand is used. In this case:
%% - node-set comp node-set: string values are used
%% - node-set comp number : ((node-set string value) -> number) 
%% - node-set comp boolean : (node-set string value) -> boolean
%% 2. Comparisons when neither object is a node-set and the operand
%% is = or != the following transformation is done before comparison:
%% - if one object is a boolean the other is converted to a boolean.
%% - if one object is a number the other is converted to a number.
%% - otherwise convert both to the string value.
%% 3. Comparisons when neither object is a node-set and the operand is
%% <=, <, >= or > both objects are converted to a number.
compare_eq_format(N1=#xmlObj{type=T1},N2=#xmlObj{type=T2},C) when T1==nodeset;
							      T2==nodeset ->
    compare_nseq_format(N1,N2,C);
compare_eq_format(N1=#xmlObj{type=T1},#xmlObj{type=T2},C) when T1==boolean;
							      T2==boolean ->
    mk_boolean(C,N1);
compare_eq_format(N1=#xmlObj{type=T1},#xmlObj{type=T2},C) when T1==number;
							      T2==number ->
    mk_number(C,N1);
compare_eq_format(N1,_,C) ->
    mk_string(C,string_value(N1)).

compare_ineq_format(N1=#xmlObj{type=T1},
		    N2=#xmlObj{type=T2},C) when T1==nodeset;
						T2==nodeset ->
    compare_nseq_format(N1,N2,C);
compare_ineq_format(N1,_N2,C) ->
    mk_number(C,N1).

compare_nseq_format(N1=#xmlObj{type = number},_N2,C) ->
    mk_number(C,N1);
compare_nseq_format(N1=#xmlObj{type = boolean},_N2,C) ->
    mk_boolean(C,N1);
compare_nseq_format(N1=#xmlObj{type = string},_N2,C) ->
    mk_string(C,N1);
compare_nseq_format(N1=#xmlObj{type = nodeset},_N2=#xmlObj{type=number},C) ->
    %% transform nodeset value to its string-value
    mk_number(C,string_value(N1));
compare_nseq_format(N1=#xmlObj{type = nodeset},_N2=#xmlObj{type=boolean},C) ->
    mk_boolean(C,N1);
compare_nseq_format(N1=#xmlObj{type = nodeset},_N2,C) ->
    mk_string(C,string_value(N1)).


core_function('last') ->		true;
core_function('position') ->		true;
core_function('count') ->		true;
core_function('id') ->			true;
core_function('local-name') ->		true;
core_function('namespace-uri') ->	true;
core_function('name') ->		true;
core_function('string') ->		true;
core_function('concat') ->		true;
core_function('starts-with') ->		true;
core_function('contains') ->		true;
core_function('substring-before') ->	true;
core_function('substring-after') ->	true;
core_function('string-length') ->	true;
core_function('normalize-space') ->	true;
core_function('translate') ->		true;
core_function('boolean') ->		true;
core_function('not') ->			{true, fn_not};
core_function('true') ->		{true, fn_true};
core_function('false') ->		{true, fn_false};
core_function('lang') ->		true;
core_function('number') ->		true;
core_function('sum') ->			true;
core_function('floor') ->		true;
core_function('ceiling') ->		true;
core_function('round') ->		true;
core_function(_) ->
    false.


%%%  node set functions

%% number: last()
last(#xmlContext{nodeset = Set}, []) ->
    ?number(length(Set)).

%% number: position()
position(#xmlContext{context_node = #xmlNode{pos = Pos}}, []) ->
    ?number(Pos).

%% number: count(node-set)
count(C, [Arg]) ->
    ?number(length(mk_nodeset(C, Arg))).

%% node-set: id(object)
id(C, [Arg]) ->
    WD = C#xmlContext.whole_document,
    NS0 = [WD],
    Obj = mk_object(C,Arg),
    case Obj#xmlObj.type of
	nodeset ->
	    NodeSet = Obj#xmlObj.value,
	    IdTokens = 
		lists:foldl(
		  fun(N, AccX) ->
			  StrVal = string_value(N),
			  TokensX = id_tokens(StrVal),
			  TokensX ++ AccX
		  end, [], NodeSet),
	    NewNodeSet = 
		xmerl_xpath:axis(descendant_or_self, 
				 fun(Node) ->
					 attribute_test(Node, id, IdTokens)
				 end, C#xmlContext{nodeset = NS0}),
	    ?nodeset(NewNodeSet);
	_ ->
	    StrVal = string_value(Obj#xmlObj.value),
	    IdTokens = id_tokens(StrVal),
	    NodeSet = [(WD#xmlNode.node)#xmlDocument.content],
	    NewNodeSet = lists:foldl(
			   fun(Tok, AccX) ->
				   select_on_attribute(NodeSet, id, Tok, AccX)
			   end, [], IdTokens),
	    ?nodeset(NewNodeSet)
	    
    end.

id_tokens(Str=#xmlObj{type=string}) ->
    string:tokens(Str#xmlObj.value, " \t\n\r").
%%id_tokens(Str) when list(Str) ->
%%    string:tokens(Str, " \t\n\r").			  

attribute_test(#xmlNode{node = #xmlElement{attributes = Attrs}}, 
	       Key, Vals) ->
    case lists:keysearch(Key, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    lists:member(V, Vals);
	_ ->
	    false
    end;
attribute_test(_Node, _Key, _Vals) ->
    false.

%%% CONTINUE HERE!!!!

%% string: local-name(node-set?)
'local-name'(C, []) ->
    local_name1(default_nodeset(C));

'local-name'(C, [Arg]) ->
    local_name1(mk_nodeset(C, Arg)).

local_name1([]) ->
    ?string([]);
local_name1([#xmlNode{type=element,node=El}|_]) ->
    #xmlElement{name=Name,nsinfo=NSI} = El,
    local_name2(Name,NSI);
local_name1([#xmlNode{type=attribute,node=Att}|_]) ->
    #xmlAttribute{name=Name,nsinfo=NSI} = Att,
    local_name2(Name,NSI);
local_name1([#xmlNode{type=namespace,node=N}|_]) ->
    #xmlNsNode{prefix=Prefix} = N,
    ?string(Prefix);
local_name1([#xmlElement{name = Name, nsinfo = NSI}|_]) ->
    local_name2(Name,NSI).
local_name2(Name, NSI) ->
    case NSI of
	{_Prefix, Local} ->
	    ?string(Local);
	[] ->
	    ?string(atom_to_list(Name))
    end.

%% string: namespace-uri(node-set?)
'namespace-uri'(C, []) ->
    ns_uri(default_nodeset(C));

'namespace-uri'(C, [Arg]) ->
    ns_uri(mk_nodeset(C, Arg)).


ns_uri([]) ->
    ?string([]);
ns_uri([#xmlElement{nsinfo = NSI, namespace = NS}|_]) ->
    ns_uri2(NSI,NS);
ns_uri([#xmlNode{type=element,node=El}|_]) ->
    #xmlElement{nsinfo=NSI, namespace = NS} = El,
    ns_uri2(NSI,NS);
ns_uri([#xmlNode{type=attribute,node=Att}|_]) ->
    #xmlAttribute{nsinfo=NSI, namespace = NS} = Att,
    ns_uri2(NSI,NS);
ns_uri(_) ->
    ?string([]).

ns_uri2(NSI,NS) ->
    case NSI of
	{Prefix, _} ->
	    case lists:keysearch(Prefix, 1, NS#xmlNamespace.nodes) of
		false ->
		    ?string([]);
		{value, {_K, V}} ->
		    string_value(V)
	    end;
	[] ->
	    ?string([])
    end.

%% name(node-set) -> xmlObj{type=string}
%% The name function returns a string containing the QName of the node
%% first in document order. The representation of the QName is not
%% standardized and applications have their own format. At
%% http://xml.coverpages.org/clarkNS-980804.html (the author of XPath)
%% adopts the format "namespace URI"+"local-name" but according to
%% other sources it is more common to use the format:
%% '{'"namespace URI"'}'"local-name". This function also uses this
%% latter form.
name(C,[]) ->
    name1(default_nodeset(C));
name(C, [Arg]) ->
    name1(mk_nodeset(C, Arg)).
name1([]) ->
    ?string([]);
name1(NodeSet) ->
    NSVal =
	case ns_uri(NodeSet) of
	    #xmlObj{value=NSStr} when NSStr =/= [] ->
		"{"++NSStr++"}";
	    _ ->
		""
	end,
    #xmlObj{value=LocalName} = local_name1(NodeSet),
    ?string(NSVal++LocalName).
	
	  

%%% String functions

%% string: string(object?)
string(C, []) ->
    ns_string(default_nodeset(C));
string(C, [Arg]) ->
    string_value(mk_object(C, Arg)).

ns_string([Obj|_]) ->
    string_value(Obj).

string_value(#xmlObj{type=nodeset,value=[]}) ->
    ?string("");
string_value(N=#xmlObj{type=nodeset}) ->
    string_value(hd(N#xmlObj.value));
string_value(N=#xmlObj{}) ->
    string_value(N#xmlObj.value);
%% Needed also string_value for root_nodes, elements (concatenation of
%% al decsendant text nodes) and attribute nodes (normalized value).
string_value(A=#xmlNode{type=attribute}) ->
    #xmlAttribute{value=AttVal}=A#xmlNode.node,
    ?string(AttVal);
string_value(N=#xmlNode{type=namespace}) ->
    #xmlNsNode{uri=URI}=N#xmlNode.node,
    ?string(atom_to_list(URI));
string_value(El=#xmlNode{type=element}) ->
    #xmlElement{content=C} = El#xmlNode.node,
    TextValue = fun(#xmlText{value=T},_Fun) -> T;
			(#xmlElement{content=Cont},Fun) -> Fun(Cont,Fun);
			(_,_) -> []
		     end,
    TextDecendants=fun(X) -> TextValue(X,TextValue) end,
    ?string(lists:flatten(lists:map(TextDecendants,C)));
string_value(T=#xmlNode{type=text}) ->
    #xmlText{value=Txt} = T#xmlNode.node,
    ?string(Txt);
string_value(T=#xmlNode{type=comment}) ->
    #xmlComment{value=Txt} = T#xmlNode.node,
    ?string(Txt);
string_value(infinity) -> ?string("Infinity");
string_value(neg_infinity) -> ?string("-Infinity");
string_value(A) when is_atom(A) ->
    ?string(atom_to_list(A));
string_value(N) when is_integer(N) ->
    ?string(integer_to_list(N));
string_value(N) when is_float(N) ->
    N1 = round(N * 10000000000000000),
    ?string(strip_zeroes(integer_to_list(N1)));
string_value(Str) when is_list(Str) ->
    ?string(Str).

strip_zeroes(Str) ->
    strip_zs(lists:reverse(Str), 15).

strip_zs([H|T], 0) ->
    lists:reverse(T) ++ [$., H];
strip_zs("0" ++ T, N) ->
    strip_zs(T, N-1);
strip_zs([H|T], N) ->
    strip_zs(T, N-1, [H]).

strip_zs([H|T], 0, Acc) ->
    lists:reverse(T) ++ [$.,H|Acc];
strip_zs([H|T], N, Acc) ->
    strip_zs(T, N-1, [H|Acc]).


%% string: concat(string, string, string*)
concat(C, Args = [_, _|_]) ->
    Strings = [mk_string(C, A) || A <- Args],
    ?string(lists:concat(Strings)).

%% boolean: starts-with(string, string)
'starts-with'(C, [A1, A2]) ->
    ?boolean(lists:prefix(mk_string(C, A2), mk_string(C, A1))).

%% boolean: contains(string, string)
contains(C, [A1, A2]) ->
    Pos = string:str(mk_string(C, A1), mk_string(C, A2)),
    ?boolean(Pos > 0).

%% string: substring-before(string, string)
'substring-before'(C, [A1, A2]) ->
    S1 = mk_string(C, A1),
    S2 = mk_string(C, A2),
    Pos = string:str(S1, S2),
    ?string(string:substr(S1, 1, Pos)).

%% string: substring-after(string, string)
'substring-after'(C, [A1, A2]) ->
    S1 = mk_string(C, A1),
    S2 = mk_string(C, A2),
    case string:str(S1, S2) of
	0 ->
	    ?string([]);
	Pos ->
	    ?string(string:substr(S1, Pos))
    end.

%% string: substring(string, number, number?)
substring(C, [A1, A2]) ->
    S = mk_string(C, A1),
    Pos = mk_integer(C, A2),
    ?string(string:substr(S, Pos));
substring(C, [A1, A2, A3]) ->
    S = mk_string(C, A1),
    Pos = mk_integer(C, A2),
    Length = mk_integer(C, A3),
    ?string(string:substr(S, Pos, Length)).


%% number: string-length(string?)
'string-length'(C = #xmlContext{context_node = N}, []) ->
    length(mk_string(C, string_value(N)));

'string-length'(C, [A]) ->
    length(mk_string(C, A)).


%% string: normalize-space(string?)
'normalize-space'(C = #xmlContext{context_node = N}, []) ->
    normalize(mk_string(C, string_value(N)));

'normalize-space'(C, [A]) ->
    normalize(mk_string(C, A)).


%% string: translate(string, string, string)
translate(C, [A1, A2, A3]) ->
    S1 = mk_string(C, A1),
    S2 = mk_string(C, A2),
    S3 = mk_string(C, A3),
    ?string(translate1(S1, translations(S2, S3))).

translate1([H|T], Xls) ->
    case lists:keysearch(H, 1, Xls) of
	{value, {_, remove}} ->
	    translate1(T, Xls);
	{value, {_, replace, H1}} ->
	    [H1|translate1(T, Xls)];
	false ->
	    [H|translate1(T, Xls)]
    end;
translate1([], _) ->
    [].

translations([H|T], [H1|T1]) ->
    [{H, replace, H1}|translations(T, T1)];
translations(Rest, []) ->
    [{X, remove} || X <- Rest];
translations([], _Rest) ->
    [].



%% boolean: boolean(object)
boolean(C, [Arg]) ->
    ?boolean(mk_boolean(C, Arg)).

%% boolean: not(boolean) ->
fn_not(C, [Arg]) ->
    ?boolean(not(mk_boolean(C, Arg))).

%% boolean: true() ->
fn_true(_C, []) ->
    ?boolean(true).

%% boolean: false() ->
fn_false(_C, []) ->
    ?boolean(false).

%% boolean: lang(string) ->
lang(C = #xmlContext{context_node = N}, [Arg]) ->
    S = mk_string(C, Arg),
    Lang = 
	case N of
	    #xmlElement{language = L} -> L;
	    #xmlAttribute{language = L} -> L;
	    #xmlText{language = L} -> L;
	    #xmlComment{language = L} -> L;
	    _ -> []
	end,
    case Lang of
	[] ->
	    ?boolean(false);
	_ ->
	    ?boolean(match_lang(upcase(S), upcase(Lang)))
    end.


upcase([H|T]) when H >= $a, H =< $z ->
    [H+($A-$a)|upcase(T)];
upcase([H|T]) ->
    [H|upcase(T)];
upcase([]) ->
    [].

match_lang([H|T], [H|T1]) ->
    match_lang(T, T1);
match_lang([], "-" ++ _) ->
    true;
match_lang([], []) ->
    true;
match_lang(_, _) ->
    false.
	


%% number: number(object)
number(C = #xmlContext{context_node = N}, []) ->
    ?number(mk_number(C, string(C, N)));
number(C, [Arg]) ->
    ?number(mk_number(C, Arg)).


sum(C, [Arg]) ->
    NS = mk_nodeset(C, Arg),
    lists:foldl(
      fun(N, Sum) ->
	      Sum + mk_number(C, string(C, N))
      end, 0, NS).

floor(C, [Arg]) ->
    Num = mk_number(C, Arg),
    case trunc(Num) of
	Num1 when Num1 > Num ->
	    ?number(Num1-1);
	Num1 ->
	    ?number(Num1)
    end.

ceiling(C, [Arg]) ->
    Num = mk_number(C, Arg),
    case trunc(Num) of
	Num1 when Num1 < Num ->
	    ?number(Num1+1);
	Num1 ->
	    ?number(Num1)
    end.


round(C, [Arg]) ->
    case mk_number(C, Arg) of
	A when is_atom(A) ->
	    A;
	N when is_integer(N) ->
	    N;
	F when is_float(F) ->
	    round(F)
    end.


select_on_attribute([E = #xmlElement{attributes = Attrs}|T], K, V, Acc) ->
    case lists:keysearch(K, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    Acc2 = select_on_attribute(E#xmlElement.content,K,V,[E|Acc]),
	    select_on_attribute(T, K, V, Acc2);
	_ ->
	    Acc2 = select_on_attribute(E#xmlElement.content,K,V,Acc),
	    select_on_attribute(T, K, V, Acc2)
    end;
select_on_attribute([H|T], K, V, Acc) when is_record(H,xmlText) ->
    select_on_attribute(T, K, V, Acc);
select_on_attribute([], _K, _V, Acc) ->
    Acc.


%%%%

mk_nodeset(_C0, #xmlContext{nodeset = NS}) ->
    NS;
mk_nodeset(_C0, #xmlObj{type = nodeset, value = NS}) ->
    NS;
mk_nodeset(C0, Expr) ->
    case expr(Expr, C0) of
	#xmlObj{type = nodeset, value = NS} ->
	    NS;
	Other ->
	    exit({expected_nodeset, Other})
    end.


default_nodeset(#xmlContext{context_node = N}) ->
    [N].


mk_object(_C0, Obj = #xmlObj{}) ->
    Obj;
mk_object(C0, Expr) ->
    expr(Expr, C0).


mk_string(_C0, #xmlObj{type = string, value = V}) ->
    V;
mk_string(C0, Obj = #xmlObj{}) ->
    mk_string(C0,string_value(Obj));
mk_string(C0, Expr) ->
    mk_string(C0, expr(Expr, C0)).



mk_integer(_C0, #xmlObj{type = number, value = V}) when is_float(V)  ->
    round(V);
mk_integer(_C0, #xmlObj{type = number, value = V}) when is_integer(V)  ->
    V;
mk_integer(C, Expr) ->
    mk_integer(C, expr(Expr, C)).


mk_number(_C, #xmlObj{type = string, value = V}) ->
    scan_number(V);
mk_number(_C, #xmlObj{type = number, value = V}) ->
    V;
mk_number(C, N=#xmlObj{type = nodeset}) ->
    mk_number(C,string_value(N));
mk_number(_C, #xmlObj{type = boolean, value = false}) ->
    0;
mk_number(_C, #xmlObj{type = boolean, value = true}) ->
    1;
mk_number(C, Expr) ->
    mk_number(C, expr(Expr, C)).


mk_boolean(_C, #xmlObj{type = boolean, value = V}) -> 
    V;
mk_boolean(_C, #xmlObj{type = number, value = 0}) ->
    false;
mk_boolean(_C, #xmlObj{type = number, value = V}) when is_float(V) ; is_integer(V) ->
    true;
mk_boolean(_C, #xmlObj{type = nodeset, value = []}) ->
    false;
mk_boolean(_C, #xmlObj{type = nodeset, value = _V}) ->
    true;
mk_boolean(_C, #xmlObj{type = string, value = []}) ->
    false;
mk_boolean(_C, #xmlObj{type = string, value = _V}) ->
    true;
mk_boolean(C, Expr) ->
    mk_boolean(C, expr(Expr, C)).


normalize([H|T]) when ?whitespace(H) ->
    normalize(T);
normalize(Str) ->
    ContF = fun(_ContF, RetF, _S) ->
		    RetF()
	    end,
    normalize(Str,
	      #xmerl_scanner{acc_fun = fun() -> exit(acc_fun) end,
			     event_fun = fun() -> exit(event_fun) end,
			     hook_fun = fun() -> exit(hook_fun) end,
			     continuation_fun = ContF},
	      []).


normalize(Str = [H|_], S, Acc) when ?whitespace(H) ->
    case xmerl_scan:accumulate_whitespace(Str, S, preserve, Acc) of
	{" " ++ Acc1, [], _S1} ->
	    lists:reverse(Acc1);
	{Acc1, [], _S1} ->
	    lists:reverse(Acc1);
	{Acc1, T1, S1} ->
	    normalize(T1, S1, Acc1)
    end;
normalize([H|T], S, Acc) ->
    normalize(T, S, [H|Acc]);
normalize([], _S, Acc) ->
    lists:reverse(Acc).


scan_number([H|T]) when ?whitespace(H) ->
    scan_number(T);
scan_number("-" ++ T) ->
    case catch xmerl_xpath_scan:scan_number(T) of
	{{number, N}, Tail} ->
	    case is_all_white(Tail) of
		true ->
		    N;
		false ->
		    'NaN'
	    end;
	_Other ->
	    'NaN'
    end;
scan_number(T) ->
    case catch xmerl_xpath_scan:scan_number(T) of
	{{number, N}, Tail} ->
	    case is_all_white(Tail) of
		true ->
		    N;
		false ->
		    'NaN'
	    end;
	_Other ->
	    'NaN'
    end.

is_all_white([H|T]) when ?whitespace(H) ->
    is_all_white(T);
is_all_white([_H|_T]) ->
    false;
is_all_white([]) ->
    true.
