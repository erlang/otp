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

%% Description  : Implements a search engine based on XPath

%% @doc The xmerl_xpath module handles the entire XPath 1.0 spec.
%% XPath expressions typically occur in XML attributes and are used to address
%% parts of an XML document.
%     The grammar is defined in <code>xmerl_xpath_parse.yrl</code>.
%     The core functions are defined in <code>xmerl_xpath_pred.erl</code>.
%
%     <p>Some useful shell commands for debugging the XPath parser</p>
% <pre>
% c(xmerl_xpath_scan).
% yecc:yecc("xmerl_xpath_parse.yrl", "xmerl_xpath_parse", true, []).
% c(xmerl_xpath_parse).
%
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("position() > -1")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 * 6 div 2")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 + 6 mod 2")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("5 * 6")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("-----6")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("parent::node()")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("descendant-or-self::node()")).
% xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens("parent::processing-instruction('foo')")).
%% </pre>
%%
%% @type nodeEntity() =
%%      #xmlElement{}
%%    | #xmlAttribute{}
%%    | #xmlText{} 
%%    | #xmlPI{}
%%    | #xmlComment{}
%%    | #xmlNsNode{}
%%    | #xmlDocument{}
%%
%% @type docNodes() =   #xmlElement{}
%%    | #xmlAttribute{}
%%    | #xmlText{} 
%%    | #xmlPI{}
%%    | #xmlComment{}
%%    | #xmlNsNode{}
%%
%% @type docEntity() =  #xmlDocument{} | [docNodes()]
%%
%% @type xPathString() = string()
%%
%% @type parentList() = [{atom(), integer()}]
%%
%% @type option_list(). <p>Options allows to customize the behaviour of the
%%     XPath scanner.
%% </p>
%% <p>
%% Possible options are:
%% </p>
%% <dl>
%%  <dt><code>{namespace, #xmlNamespace}</code></dt>
%%    <dd>Set namespace nodes, from XmlNamspace, in xmlContext</dd>
%%  <dt><code>{namespace, Nodes}</code></dt>
%%    <dd>Set namespace nodes in xmlContext.</dd>
%% </dl>

%%  <dt><code>{bindings, Bs}</code></dt>
%%   <dd></dd>
%% <dt><code>{functions, Fs}</code></dt>
%%   <dd></dd>
-module(xmerl_xpath).


%% main API
-export([string/2,
	 string/3,
	 string/5]).

%% exported helper functions, internal for the XPath support
-export([eval_path/3,
	 axis/3, axis/4]).

%% debug function
-export([write_node/1]).


-include("xmerl.hrl").
-include("xmerl_internal.hrl").


-record(state, {context = #xmlContext{},
		acc = []}).


-define(nodeset(NS), #state{context = #xmlContext{nodeset = NS}}).
-define(context(C), #state{context = C}).




%% @spec string(Str, Doc) -> [docEntity()] | Scalar
%% @equiv string(Str,Doc, [])
string(Str, Doc) ->
    string(Str, Doc, []).

%% @spec string(Str,Doc,Options) -> 
%%      [docEntity()] | Scalar
%% @equiv string(Str,Doc, [],Doc,Options)
string(Str, Doc, Options) ->
    string(Str, Doc, [], Doc, Options).

%% @spec string(Str,Node,Parents,Doc,Options) ->
%%      [docEntity()] | Scalar
%%   Str     = xPathString()
%%   Node    = nodeEntity()
%%   Parents = parentList()
%%   Doc     = nodeEntity()
%%   Options = option_list()
%%   Scalar  = #xmlObj{}
%% @doc Extracts the nodes from the parsed XML tree according to XPath.
%%   xmlObj is a record with fields type and value,
%%   where type is boolean | number | string
string(Str, Node, Parents, Doc, Options) ->
%% record with fields type and value,
%%                where type is boolean | number | string
    FullParents = 
	case Parents of
	    [] ->
		[];
	    [{H, P}|_] when is_atom(H), is_integer(P) ->
		full_parents(Parents, Doc)
	end,
%?dbg("string FullParents=~p~n",[FullParents]),
    ContextNode=#xmlNode{type = node_type(Node),
			 node = Node,
			 parents = FullParents},
%?dbg("string ContextNode=~p~n",[ContextNode]),
    WholeDoc = whole_document(Doc),
%?dbg("string WholeDoc=~p~n",[WholeDoc]),
    Context=(new_context(Options))#xmlContext{context_node = ContextNode,
					      whole_document = WholeDoc},
%?dbg("string Context=~p~n",[Context]),
    #state{context = NewContext} = match(Str, #state{context = Context}),
%?dbg("string NewContext=~p~n",[NewContext]),
    case NewContext#xmlContext.nodeset of
	ScalObj = #xmlObj{type=Scalar} 
	when Scalar == boolean;	Scalar == number; Scalar == string ->
	    ScalObj;
	#xmlObj{type=nodeset,value=NodeSet} -> 
	    NodeSet;
	_ ->
	    [N || #xmlNode{node = N} <- NewContext#xmlContext.nodeset]
    end.


whole_document(#xmlDocument{} = Doc) ->
    #xmlNode{type = root_node,
	     node = Doc,
	     parents = []};
whole_document(Other) ->
    #xmlNode{type = root_node,
	     node = #xmlDocument{content = Other},
	     parents = []}.


new_context(Options) ->
    new_context(Options, #xmlContext{}).

new_context([{namespace, #xmlNamespace{nodes = Nodes}}|T], C) ->
    new_context(T, C#xmlContext{namespace = ns_nodes(Nodes)});
new_context([{namespace, Nodes}|T], C) ->
    new_context(T, C#xmlContext{namespace = ns_nodes(Nodes)});
new_context([{bindings, Bs}|T], C) ->
    new_context(T, C#xmlContext{bindings = Bs});
new_context([{functions, Fs}|T], C) ->
    new_context(T, C#xmlContext{functions = Fs});
new_context([], C) ->
    C.


ns_nodes([{Prefix, URI}|T]) ->
    [{to_string(Prefix), to_atom(URI)}|ns_nodes(T)];
ns_nodes([]) ->
    [].

full_parents(Ps, Doc) ->
    full_parents1(lists:reverse(Ps), [Doc], []).

full_parents1([{Name, Pos}|Ns], Content, Parents) ->
    E = locate_element(Name, Pos, Content),
    PN = #xmlNode{type = element,
		  node = E,
		  parents = Parents},
    full_parents1(Ns, get_content(E), [PN|Parents]);
full_parents1([], _E, Parents) ->
    Parents.


locate_element(Name, Pos, [E = #xmlElement{name = Name, pos = Pos}|_]) ->
    E;
locate_element(_Name, Pos, [#xmlElement{pos = P}|_]) when P >= Pos ->
    %% we've passed Pos (P > Pos) or the name is wrong (P == Pos)
    exit(invalid_parents);
locate_element(_Name, _Pos, []) ->
    exit(invalid_parents);
locate_element(Name, Pos, [_|T]) ->
    locate_element(Name, Pos, T).


match(Str, S = #state{}) ->
    Tokens = xmerl_xpath_scan:tokens(Str),
    case xmerl_xpath_parse:parse(Tokens) of
	{ok, Expr} ->
	    match_expr(Expr, S);
	Error ->
	    Error
    end.


match_expr({path, Type, Arg}, S) ->
    eval_path(Type, Arg, S#state.context);
%% PrimaryExpr
match_expr(PrimExpr,S) ->
    eval_primary_expr(PrimExpr,S).





path_expr({refine, StepExpr1, StepExpr2}, S) ->
    ?dbg("StepExpr1=~p StepExpr2=~p~n", [StepExpr1,StepExpr2]),
    ?dbg("length(nodeset) = ~p~n", 
	 [length((S#state.context)#xmlContext.nodeset)]),
    S1 = path_expr(StepExpr1, S),
    ?dbg("length(nodeset1) = ~p~n", 
	 [length((S1#state.context)#xmlContext.nodeset)]),
    path_expr(StepExpr2, S1);
path_expr({step, {Axis, NodeTest, PredExpr}}, S = #state{context = C,
							 acc = Acc}) ->
    ?dbg("PredExpr = ~p~n", [PredExpr]),
    NewContext = axis(Axis, NodeTest, C, Acc),
    pred_expr(PredExpr, S#state{context = NewContext});
path_expr('/', S) ->
    S.


pred_expr([], S) ->
    S;
pred_expr([{pred, Pred}|Preds], S = #state{}) ->
    ?dbg("Pred = ~p~n", [Pred]),
    NewS = eval_pred(Pred, S),
    pred_expr(Preds, NewS).

%% simple case: the predicate is a number, e.g. para[5].
%% No need to iterate over all nodes in the nodeset; we know what to do.
%%
eval_pred({number, N0}, 
	  S = #state{context = C = #xmlContext{nodeset = NS,
					       axis_type = AxisType}}) ->
    Len = length(NS),
    case Len>=N0 of
	true ->
	    N = case AxisType of
		    forward ->
			N0;
		    reverse ->
			Len + 1 - N0
		end,
	    NewNodeSet = [lists:nth(N, NS)],
	    NewContext = C#xmlContext{nodeset = NewNodeSet},
	    S#state{context = NewContext};
	false -> S#state{context = C#xmlContext{nodeset = []}}
    end;
eval_pred(Predicate, S = #state{context = C = 
				#xmlContext{nodeset = NodeSet}}) ->
    NewNodeSet = 
	lists:filter(
	  fun(Node) ->
		  %?dbg("current node: ~p~n", [write_node(Node)]),
		  ThisContext = C#xmlContext{context_node = Node},
		  xmerl_xpath_pred:eval(Predicate, ThisContext)
	  end, NodeSet),
    NewContext = C#xmlContext{nodeset = NewNodeSet},
    S#state{context = NewContext}.    
    


%% write_node(Node::xmlNode()) -> {Type,Pos,Name,Parents}
%% Helper function to access essential information from the xmlNode record.
%% @hidden
write_node(#xmlNode{pos = Pos,
		    node = #xmlAttribute{name = Name,
					 parents = Ps}}) ->
    {attribute, Pos, Name, Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlElement{name = Name,
				       parents = Ps}}) ->
    {element, Pos, Name, Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlText{value = Txt,
				    parents = Ps}}) ->
    {text, Pos, Txt, Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlComment{parents = Ps}}) ->
    {comment, Pos, '', Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlPI{name = Name,
				  parents = Ps}}) ->
    {processing_instruction, Pos, Name, Ps};
write_node(#xmlNode{pos = Pos,
		    node = #xmlNsNode{parents = Ps,
				      prefix = Prefix}}) ->
    {namespace, Pos, Prefix, Ps};
write_node(_) ->
    other.


%% eval_path(Type,Arg,S::state()) -> state()
%% Eval path
%% @hidden
eval_path(union, {PathExpr1, PathExpr2}, C = #xmlContext{}) ->
    S = #state{context = C},
    S1 = match_expr(PathExpr1, S),
%%    NewNodeSet = (S1#state.context)#xmlContext.nodeset,
    S2 = match_expr(PathExpr2, S1#state{context=C}),
    NodeSet1 = (S1#state.context)#xmlContext.nodeset,
    NodeSet2 = (S2#state.context)#xmlContext.nodeset,
    NewNodeSet = ordsets:to_list(ordsets:union(ordsets:from_list(NodeSet1),
					       ordsets:from_list(NodeSet2))),
    S2#state{context=(S2#state.context)#xmlContext{nodeset=NewNodeSet}};
eval_path(abs, PathExpr, C = #xmlContext{}) ->
    NodeSet = [C#xmlContext.whole_document],
    Context = C#xmlContext{nodeset = NodeSet},
    S = #state{context = Context},
    path_expr(PathExpr, S);
eval_path(rel, PathExpr, C = #xmlContext{}) ->
    NodeSet = [C#xmlContext.context_node],
    Context = C#xmlContext{nodeset = NodeSet},
    S = #state{context = Context},
    path_expr(PathExpr, S);
eval_path(filter, {PathExpr, {pred, Pred}}, C = #xmlContext{}) ->
    S = #state{context = C},
    S1 = match_expr(PathExpr, S),
    eval_pred(Pred, S1).

eval_primary_expr(PrimExpr, S = #state{context = Context}) ->
%%    NewNodeSet = xmerl_xpath_pred:eval(FC, Context),
    NewNodeSet = xmerl_xpath_lib:eval(primary_expr, PrimExpr, Context),
    NewContext = Context#xmlContext{nodeset = NewNodeSet},
    S#state{context = NewContext}.
    

%% axis(Axis,NodeTest,Context::xmlContext()) -> xmlContext()
%% axis(Axis,NodeTest,Context,[])
%% @hidden
axis(Axis, NodeTest, Context) ->
    axis(Axis, NodeTest, Context, []).


%% axis(Axis,NodeTest,Context::xmlContext(),Acc) -> xmlContext()
%%  
%% An axis specifies the tree relationship between the nodes selected by
%% the location step and the context node.
%% @hidden
axis(Axis, NodeTest, Context = #xmlContext{nodeset = NS0}, Acc) ->
    NewNodeSet=lists:foldr(
		 fun(N, AccX) ->
			 axis1(Axis, NodeTest, N, AccX, Context)
		 end, Acc, NS0),
    update_nodeset(fwd_or_reverse(Axis, Context), NewNodeSet).


axis1(self, Tok, N, Acc, Context) ->
    match_self(Tok, N, Acc, Context);
axis1(descendant, Tok, N, Acc, Context) ->
    match_descendant(Tok, N, Acc, Context);
axis1(child, Tok, N, Acc, Context) ->
    match_child(Tok, N, Acc, Context);
axis1(parent, Tok, N, Acc, Context) ->
    match_parent(Tok, N, Acc, Context);
axis1(ancestor, Tok, N, Acc, Context) ->
    match_ancestor(Tok, N, Acc, Context);
axis1(following_sibling, Tok, N, Acc, Context) ->
    match_following_sibling(Tok, N, Acc, Context);
axis1(preceding_sibling, Tok, N, Acc, Context) ->
    match_preceding_sibling(Tok, N, Acc, Context);
axis1(following, Tok, N, Acc, Context) ->
    match_following(Tok, N, Acc, Context);
axis1(preceding, Tok, N, Acc, Context) ->
    match_preceding(Tok, N, Acc, Context);
axis1(attribute, Tok, N, Acc, Context) ->
    match_attribute(Tok, N, Acc, Context);
axis1(namespace, Tok, N, Acc, Context) ->
   match_namespace(Tok, N, Acc, Context);
axis1(ancestor_or_self, Tok, N, Acc, Context) ->
    match_ancestor_or_self(Tok, N, Acc, Context);
axis1(descendant_or_self, Tok, N, Acc, Context) ->
    match_descendant_or_self(Tok, N, Acc, Context).


fwd_or_reverse(ancestor, Context) ->
    reverse_axis(Context);
fwd_or_reverse(ancestor_or_self, Context) ->
    reverse_axis(Context);
fwd_or_reverse(preceding_sibling, Context) ->
    reverse_axis(Context);
fwd_or_reverse(preceding, Context) ->
    reverse_axis(Context);
fwd_or_reverse(_, Context) ->
    forward_axis(Context).

reverse_axis(Context) ->
    Context#xmlContext{axis_type = reverse}.
forward_axis(Context) ->
    Context#xmlContext{axis_type = forward}.



match_self(Tok, N, Acc, Context) ->
    case node_test(Tok, N, Context) of
	true ->
	    [N|Acc];
	false ->
	    Acc
    end.


match_descendant(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node, type = Type} = N,
    case Type of
	El when El == element; El == root_node ->
	    NewPs = [N|Ps],
	    match_desc(get_content(Node), NewPs, Tok, Acc, Context);
	_Other ->
	    Acc
    end.


match_desc([E = #xmlElement{}|T], Parents, Tok, Acc, Context) ->
    Acc1 = match_desc(T, Parents, Tok, Acc, Context),
    N = #xmlNode{type = node_type(E),
		 node = E,
		 parents = Parents},
    NewParents = [N|Parents],
    Acc2 = match_desc(get_content(E), NewParents, Tok, Acc1, Context),
    match_self(Tok, N, Acc2, Context);
match_desc([E|T], Parents, Tok, Acc, Context) ->
    Acc1 = match_desc(T, Parents, Tok, Acc, Context),
    N = #xmlNode{node = E,
		 type = node_type(E),
		 parents = Parents},
    match_self(Tok, N, Acc1, Context);
match_desc([], _Parents, _Tok, Acc, _Context) ->
    Acc.
			  


%% "The 'descendant-or-self' axis contains the context node and the 
%% descendants of the context node."
match_descendant_or_self(Tok, N, Acc, Context) ->
    Acc1 = match_descendant(Tok, N, Acc, Context),
    match_self(Tok, N, Acc1, Context).


match_child(Tok, N, Acc, Context) ->
    %?dbg("match_child(~p)~n", [write_node(N)]),
    #xmlNode{parents = Ps, node = Node, type = Type} = N,
    case Type of
	El when El == element; El == root_node ->
	    NewPs = [N|Ps],
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = NewPs},
		      match_self(Tok, ThisN, AccX, Context)
	      end, Acc, get_content(Node));
	_Other ->
	    Acc
    end.


%% "The 'parent' axis contains the parent of the context node, 
%% if there is one."
match_parent(Tok, N, Acc, Context) ->
    case N#xmlNode.parents of
	[] ->
	    Acc;
	[PN|_] ->
	    match_self(Tok, PN, Acc, Context)
    end.


%% "The 'ancestor' axis contains the ancestors of the context node;
%% the ancestors of the context node consists of the parent of the context
%% node and the parent's parent and so on; thus, the ancestor axis will 
%% always include the root node, unless the context node is the root node."
match_ancestor(Tok, N, Acc, Context) ->
    Parents = N#xmlNode.parents,
    lists:foldl(
      fun(PN, AccX) ->
	      match_self(Tok, PN, AccX, Context)
      end, Acc, Parents).




%% "The 'ancestor-or-self' axis contains the context node and the ancestors
%% of the context node; thus, the acestor axis will always include the
%% root node."
match_ancestor_or_self(Tok, N, Acc, Context) ->
    Acc1 = match_self(Tok, N, Acc, Context),
    match_ancestor(Tok, N, Acc1, Context).


match_following_sibling(_Tok, #xmlAttribute{}, Acc, _Context) ->
    Acc;
match_following_sibling(_Tok, #xmlNamespace{}, Acc, _Context) ->
    Acc;

match_following_sibling(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode}|_] ->
	    FollowingSiblings = lists:nthtail(get_position(Node), 
					      get_content(PNode)),
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      match_self(Tok, ThisN, AccX, Context)
	      end, Acc, FollowingSiblings);
	_Other ->
	    Acc
    end.


%% "The 'following' axis contains all nodes in the same document as the
%% context node that are after the context node in document order, excluding
%% any descendants and excluding attribute nodes and namespace nodes."
match_following(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode} = P|_] ->
	    FollowingSiblings = lists:nthtail(get_position(Node), 
					      get_content(PNode)),
	    Acc0 = match_following(Tok, P, Acc, Context),
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      match_descendant_or_self(Tok, ThisN, AccX, Context)
	      end, Acc0, FollowingSiblings);
	_Other ->
	    Acc
    end.


%% "The preceding-sibling axis contains all the preceding siblings of the 
%% context node; if the context node is an attribute node or namespace node,
%% the preceding-sibling axis is empty."
match_preceding_sibling(_Tok, #xmlAttribute{}, Acc, _Context) ->
    Acc;
match_preceding_sibling(_Tok, #xmlNamespace{}, Acc, _Context) ->
    Acc;

match_preceding_sibling(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode}|_] ->
	    PrecedingSiblings = lists:sublist(get_content(PNode), 1,
					      get_position(Node) - 1), 
	    lists:foldr(
	      fun(E, AccX) ->
		      ThisN = #xmlNode{type = node_type(E),
				       node = E,
				       parents = Ps},
		      match_self(Tok, ThisN, AccX, Context)
	      end, Acc, PrecedingSiblings);
	_Other ->
	    Acc
    end.


%% "The 'preceding' axis contains all nodes in the same document as the context
%% node that are before the context node in document order, exluding any
%% ancestors and excluding attribute nodes and namespace nodes."
match_preceding(Tok, N, Acc, Context) ->
    #xmlNode{parents = Ps, node = Node} = N,
    case Ps of
	[#xmlNode{type = element,
		  node = #xmlElement{} = PNode} = P|_] ->
	    PrecedingSiblings = lists:sublist(get_content(PNode), 1,
					      get_position(Node) - 1), 
	    Acc0 = lists:foldr(
		     fun(E, AccX) ->
			     ThisN = #xmlNode{type = node_type(E),
					      node = E,
					      parents = Ps},
			     match_descendant_or_self(Tok, ThisN,
						      AccX, Context)
		     end, Acc, PrecedingSiblings),
	    match_preceding(Tok, P, Acc0, Context);
	_Other ->
	    Acc
    end.


%% "The 'attribute' axis contains the attributes of the context node; the
%% axis will be empty unless the context node is an element."
match_attribute(Tok, N, Acc, Context) ->
    case N#xmlNode.type of
	element ->
	    #xmlNode{parents = Ps, node = E} = N,
	    lists:foldr(
	      fun(A, AccX) ->
		      ThisN = #xmlNode{type = attribute,
				       node = A,
				       parents = [N|Ps]},
		      match_self(Tok, ThisN, AccX, Context)
	      end, Acc, E#xmlElement.attributes);
	_Other ->
	    %%[]
	    Acc
    end.

node_type(#xmlAttribute{}) ->	attribute;
node_type(#xmlElement{}) ->	element;
node_type(#xmlText{}) ->	text;
node_type(#xmlPI{}) ->		processing_instruction;
node_type(#xmlNsNode{}) ->	namespace;
node_type(#xmlComment{}) ->	comment;
node_type(#xmlDocument{}) ->	root_node.

%% "The namespace axis contains the namespace nodes of the context node;
%% the axis will be empty unless the context node is an element."
match_namespace(Tok, N, Acc, Context) ->
    case N#xmlNode.type of
	element ->
	    #xmlNode{parents = Ps, node = E} = N,
	    #xmlElement{name = Name,
			namespace = NS,
			parents = EPs,
			pos = Pos} = E,
	    #xmlNamespace{default = Default, nodes = NSPairs} = NS,
	    ThisEPs = [{Name, Pos}|EPs],
	    ThisPs = [N|Ps],
	    Acc0 =
		case Default of
		    D when D =:= []; D =:= '' ->
			{[], 1};
		    URI ->
			DefaultNSNode = #xmlNsNode{parents = ThisEPs,
						   pos = 1,
						   prefix = [],
						   uri = URI},
			Node = #xmlNode{type = namespace,
					node = DefaultNSNode,
					parents = ThisPs},
			{[Node], 2}
		end,
	    {Nodes, _I} =
		lists:foldr(
		  fun ({Prefix, URI}, {AccX, I}) ->
			  NSNode = #xmlNsNode{parents = ThisEPs,
					      pos = I,
					      prefix = Prefix,
					      uri = URI},
			  ThisN = #xmlNode{pos = I,
					   type = namespace,
					   node = NSNode,
					   parents = ThisPs},
			  {[ThisN | AccX], I + 1}
		  end, Acc0, NSPairs),
	    lists:foldr(
	      fun (ThisN, AccX) ->
		      match_self(Tok, ThisN, AccX, Context)
	      end, Acc, Nodes);
	_Other ->
	    %%[]
	    Acc
    end.


update_nodeset(Context = #xmlContext{axis_type = AxisType}, NodeSet) ->
    MapFold =
	case AxisType of
	    forward ->
		mapfoldl;
	    reverse ->
		mapfoldr
	end,
    {Result, _N} =
	lists:MapFold(fun(Node, N) ->
			      {Node#xmlNode{pos = N}, N + 1}
		      end, 1, NodeSet),
    Context#xmlContext{nodeset = Result}.



node_test(F, N, Context) when is_function(F) ->
    F(N, Context);
node_test(_Test, #xmlNode{type=attribute,node=#xmlAttribute{name=xmlns}},
	  _Context) ->
    false;
node_test(_Test,
	  #xmlNode{type=attribute,node=#xmlAttribute{nsinfo={"xmlns",_Local}}},
	  _Context) ->
    false;
node_test({wildcard, _}, #xmlNode{type=ElAt}, _Context) 
  when ElAt==element; ElAt==attribute; ElAt==namespace ->
    true;
node_test({prefix_test, Prefix}, #xmlNode{node = N}, Context) ->
    case N of
	#xmlElement{nsinfo = {Prefix, _}} ->
            true;
        #xmlElement{expanded_name = {Uri, _}} ->
            case expanded_name(Prefix, "_", Context) of
                {Uri, _} ->
                    true;
                _ ->
                    false
            end;
	#xmlAttribute{nsinfo = {Prefix, _}} ->
            true;
        #xmlAttribute{expanded_name = {Uri, _}} ->
            case expanded_name(Prefix, "_", Context) of
                {Uri, _} ->
                    true;
                _ ->
                    false
            end;
	_ ->
	    false
    end;
node_test({name, {Tag, _Prefix, _Local}}, 
	  #xmlNode{node = #xmlElement{name = Tag}}=_N, _Context) -> 
    %?dbg("node_test({tag, ~p}, ~p) -> true.~n", [Tag, write_node(_N)]),
    true;
node_test({name, {Tag, Prefix, Local}}, 
	  #xmlNode{node = #xmlElement{name = Name,
				      expanded_name = EExpName,
				      nsinfo = {_Prefix1, _}
				     }}, Context) -> 
    case expanded_name(Prefix, Local, Context) of
	[] ->
	    Res = (Tag == Name),
	    ?dbg("node_test(~p, ~p) -> ~p.~n", 
		 [{Tag, Prefix, Local}, write_node(Name), Res]),
	    Res;
	ExpName ->
	    Res = (ExpName == EExpName),
	    ?dbg("node_test(~p, ~p) -> ~p.~n", 
		 [{Tag, Prefix, Local}, write_node(Name), Res]),
	    Res
    end;
node_test({name, {_Tag, Prefix, Local}}, 
	  #xmlNode{node = #xmlElement{name = Name,
				      expanded_name = _EExpName,
				      namespace = NS
				     }}, Context) -> 
    case expanded_name(Prefix, Local, Context) of
	[] ->
	    ?dbg("node_test(~p, ~p) -> ~p.~n", 
		 [{_Tag, Prefix, Local}, write_node(Name), false]),
	    false;
	ExpName ->
	    Res = (ExpName == {NS#xmlNamespace.default,Name}),
	    ?dbg("node_test(~p, ~p) -> ~p.~n", 
		 [{_Tag, Prefix, Local}, write_node(Name), Res]),
	    Res
    end;
node_test({name, {Tag,_Prefix,_Local}}, 
	  #xmlNode{node = #xmlAttribute{name = Tag}}, _Context) -> 
    true;
node_test({name, {Tag, Prefix, Local}},
          #xmlNode{node = #xmlAttribute{name = Name,
                                        expanded_name = EExpName
                                       }}, Context) ->
    case expanded_name(Prefix, Local, Context) of
        [] ->
            Res = (Tag == Name),
            ?dbg("node_test(~p, ~p) -> ~p.~n",
                 [{Tag, Prefix, Local}, write_node(Name), Res]),
            Res;
        ExpName ->
            Res = (ExpName == EExpName),
            ?dbg("node_test(~p, ~p) -> ~p.~n",
                 [{Tag, Prefix, Local}, write_node(Name), Res]),
            Res
    end;
node_test({name, {_Tag, [], Local}},
	  #xmlNode{node = #xmlNsNode{prefix = Local}}, _Context) ->
    true;
node_test({node_type, NT}, #xmlNode{node = N}, _Context) ->
    case {NT, N} of
	{text, #xmlText{}} ->
	    true;
	{node, _} ->
	    true;
	{attribute, #xmlAttribute{}} ->
	    true;
	{namespace, #xmlNsNode{}} ->
	    true;
	{comment, #xmlComment{}} ->
	    true;
	{processing_instruction, #xmlPI{}} ->
	    true;
	_ ->
	    false
    end;
node_test({processing_instruction, Name1},
	  #xmlNode{node = #xmlPI{name = Name2}}, _Context) ->
    Name1 == atom_to_list(Name2);
node_test(_Other, _N, _Context) ->
    %?dbg("node_test(~p, ~p) -> false.~n", [_Other, write_node(_N)]),
    false.


expanded_name(Prefix, Local, #xmlContext{namespace = NS}) ->
    case lists:keysearch(Prefix, 1, NS) of
	{value, {_, URI}} ->
	    {URI, list_to_atom(Local)};
	false ->
	    []
    end.


to_atom(A) when is_atom(A) -> A;
to_atom(S) when is_list(S) -> list_to_atom(S).

to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(S) when is_list(S) -> S.


get_content(#xmlElement{content = C}) when is_list(C) ->
    C;
get_content(#xmlElement{content = F} = E) when is_function(F) ->
    case F() of
	C when is_list(C) ->
	    C;
	_Other ->
	    exit({bad_content, E})
    end;
get_content(#xmlDocument{content = C}) when is_list(C) ->
    C;
get_content(#xmlDocument{content = C}) ->
    [C].


get_position(#xmlElement{pos = N}) ->
    N;
get_position(#xmlText{pos = N}) ->
    N.
