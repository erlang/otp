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

%%
%% Description  : Functions to export simple and complete XML forms
%% 

%% @doc Functions for exporting XML data to an external format.
%%

-module(xmerl).

%-compile(export_all).

-export([export/2, 
	 export/3,
	 export_content/2,
	 export_element/2,
	 export_element/3,
	 export_simple/2,
	 export_simple/3,
	 export_simple_element/2,
	 export_simple_content/2,
	 callbacks/1]).

-include("xmerl.hrl").
-include("xmerl_internal.hrl").


%% @spec export(Content, Callback) -> ExportedFormat
%% @equiv export(Data, Callback, [])

export(Content, Callback) ->
    export(Content, Callback, []).

%% @spec export(Content, Callback, RootAttributes) -> ExportedFormat
%%	Content = [Element]
%%	Callback = atom()
%%      RootAttributes = [XmlAttributes]
%% @doc Exports normal, well-formed XML content, using the specified
%% callback-module.
%% <p><code>Element</code> is any of:</p>
%% <ul>
%% 	<li><code>#xmlText{}</code></li>
%%	<li><code>#xmlElement{}</code></li>
%%	<li><code>#xmlPI{}</code></li>
%%	<li><code>#xmlComment{}</code></li>
%%	<li><code>#xmlDecl{}</code></li>
%% </ul>
%% <p>(See <tt>xmerl.hrl</tt> for the record definitions.)
%% Text in <code>#xmlText{}</code> elements can be deep lists of
%% characters and/or binaries.</p>
%%
%% <p><code>RootAttributes</code> is a list of
%% <code>#xmlAttribute{}</code> attributes for the <code>#root#</code>
%% element, which implicitly becomes the parent of the given
%% <code>Content</code>. The tag-handler function for
%% <code>#root#</code> is thus called with the complete exported data of
%% <code>Content</code>. Root attributes can be used to specify
%% e.g. encoding or other metadata of an XML or HTML document.</p>
%%
%% <p>The <code>Callback</code> module should contain hook functions for
%% all tags present in the data structure. A hook function must have the
%% following format:</p>
%% <pre>    Tag(Data, Attributes, Parents, E)</pre>
%% <p>where <code>E</code> is the corresponding <code>#xmlElement{}</code>,
%% <code>Data</code> is the already-exported contents of <code>E</code>
%% and <code>Attributes</code> is the list of
%% <code>#xmlAttribute{}</code> records of <code>E</code>. Finally,
%% <code>Parents</code> is the list of parent nodes of <code>E</code>,
%% on the form <code>[{ParentTag::atom(),
%% ParentPosition::integer()}]</code>.</p>
%%
%% <p>The hook function should return either the data to be exported, or
%% a tuple <code>{'#xml-alias#', NewTag::atom()}</code>, or a tuple
%% <code>{'#xml-redefine#', Content}</code>, where <code>Content</code>
%% is a content list (which can be on simple-form; see
%% <code>export_simple/2</code> for details).</p>
%%
%% <p>A callback module can inherit definitions from other callback
%% modules, through the required function <code>'#xml-interitance#() ->
%% [ModuleName::atom()]</code>.</p>
%%
%% @see export/2
%% @see export_simple/3

export(Content, Callback, RootAttributes) when is_atom(Callback) ->
    export1(Content, callbacks(Callback), RootAttributes);
export(Content, Callbacks, RootAttrs) when is_list(Callbacks) ->
    export1(Content, Callbacks, RootAttrs).

%% @spec export_simple(Content, Callback) -> ExportedFormat
%% @equiv export_simple(Content, Callback, [])

export_simple(Content, Callback) ->
    export_simple(Content, Callback, []).

%% @spec export_simple(Content, Callback, RootAttributes) -> ExportedFormat
%%	Content = [Element]
%%	Callback = atom()
%%      RootAttributes = [XmlAttributes]
%% @doc Exports "simple-form" XML content, using the specified
%% callback-module.
%% <p><code>Element</code> is any of:</p>
%% <ul>
%%	<li><code>{Tag, Attributes, Content}</code></li>
%%	<li><code>{Tag, Content}</code></li>
%%	<li><code>Tag</code></li>
%%	<li><code>IOString</code></li>
%% 	<li><code>#xmlText{}</code></li>
%%	<li><code>#xmlElement{}</code></li>
%%	<li><code>#xmlPI{}</code></li>
%%	<li><code>#xmlComment{}</code></li>
%%	<li><code>#xmlDecl{}</code></li>
%% </ul>
%% <p>where</p>
%% <ul>
%%	<li><code>Tag = atom()</code></li>
%%	<li><code>Attributes = [{Name, Value}]</code></li>
%%	<li><code>Name = atom()</code></li>
%%	<li><code>Value = IOString | atom() | integer()</code></li>
%% </ul>
%% <p>Normal-form XML elements can thus be included in the simple-form
%% representation. Note that content lists must be flat. An
%% <code>IOString</code> is a (possibly deep) list of characters and/or
%% binaries.</p>
%%
%% <p><code>RootAttributes</code> is a list of:</p>
%% <ul>
%%	<li><code>XmlAttributes = #xmlAttribute{}</code></li>
%%</ul>
%%
%% <p>See <code>export/3</code> for details on the callback module and
%% the root attributes. The XML-data is always converted to normal form
%% before being passed to the callback module.</p>
%%
%% @see export/3
%% @see export_simple/2

export_simple(Content, Callback, RootAttrs) when is_atom(Callback) ->
    export_simple1(Content, callbacks(Callback), RootAttrs);
export_simple(Content, Callbacks, RootAttrs) when is_list(Callbacks) ->
    export_simple1(Content, Callbacks, RootAttrs).

export_simple1(Content, Callback, RootAttrs) ->
    export1(xmerl_lib:expand_content(Content), Callback, RootAttrs).

%% This exports proper XML content in root context.

export1(Content, Callbacks, RootAttrs) when is_list(Content) ->
    Result = export_content(Content, Callbacks),
    Attrs = xmerl_lib:expand_attributes(RootAttrs, 1, [{'#root#',1}]),
    Root = #xmlElement{name = '#root#',
		       pos = 1,
		       parents = [],
		       attributes = Attrs},
    Args = [Result, Root#xmlElement.attributes, [], Root],
    tagdef('#root#',1,[],Args,Callbacks).

%% @doc Exports simple XML content directly, without further context.

export_simple_content(Content, Callback) when is_atom(Callback) ->
    export_content(xmerl_lib:expand_content(Content),
		   callbacks(Callback));
export_simple_content(Content, Callbacks) when is_list(Callbacks) ->
    export_content(xmerl_lib:expand_content(Content), Callbacks).


%% @spec export_content(Content, Callbacks) -> term()
%%	Content = [Element]
%%	Callback = [atom()]
%% @doc Exports normal XML content directly, without further context.
export_content([#xmlText{value = Text} | Es], Callbacks) ->
    [apply_text_cb(Callbacks, Text) | export_content(Es, Callbacks)];
export_content([#xmlPI{} | Es], Callbacks) ->
    export_content(Es, Callbacks);
export_content([#xmlComment{} | Es], Callbacks) ->
    export_content(Es, Callbacks);
export_content([#xmlDecl{} | Es], Callbacks) ->
    export_content(Es, Callbacks);
export_content([E | Es], Callbacks) ->
    [export_element(E, Callbacks) | export_content(Es, Callbacks)];
export_content([], _Callbacks) ->
    [].

%% @doc Exports a simple XML element directly, without further context.

export_simple_element(Content, Callback) when is_atom(Callback) ->
    export_element(xmerl_lib:expand_element(Content),
		   callbacks(Callback));
export_simple_element(Content, Callbacks) when is_list(Callbacks) ->
    export_element(xmerl_lib:expand_element(Content), Callbacks).

%% @doc Exports a normal XML element directly, without further context.

%% This is the usual DOM style parsing.

export_element(E, CB) when is_atom(CB) ->
    export_element(E, callbacks(CB));
export_element(#xmlText{value = Text}, CBs) ->
    apply_text_cb(CBs, Text);
export_element(E = #xmlElement{name = Tag,
			       pos = Pos,
			       attributes = Attributes,
			       parents = Parents,
			       content = Content}, CBs) ->
    Data = export_content(Content, CBs),
    Args = [Data, Attributes, Parents, E],
    tagdef(Tag,Pos,Parents,Args,CBs);
export_element(#xmlPI{}, _CBs) ->
    [];
export_element(#xmlComment{}, _CBs) ->
    [];
export_element(#xmlDecl{}, _CBs) ->
    [].


%% @spec export_element(E,CallbackModule,CallbackState) -> ExportedFormat
%% @doc For on-the-fly exporting during parsing (SAX style) of the XML
%% document. 
export_element(E, CallbackModule, CallbackState) when is_atom(CallbackModule) ->
    export_element(E, callbacks(CallbackModule), CallbackState);
export_element(#xmlText{value = Text},CallbackModule,_CallbackState) ->
%%    apply_cb(CallbackModule, '#text#', '#text#', [Text,CallbackState]);
    apply_text_cb(CallbackModule,Text);
export_element(E=#xmlElement{name = Tag,
			   pos = Pos,
			   parents = Parents,
			   attributes = Attributes,
			   content = Content},Callbacks,CBstate) ->
    Args = [Content, Attributes,CBstate,E],
    tagdef(Tag,Pos,Parents,Args,Callbacks);
export_element(#xmlPI{}, _CallbackModule, CallbackState) ->
    CallbackState;
export_element(#xmlComment{},_CallbackModule, CallbackState) ->
    CallbackState;
export_element(#xmlDecl{},_CallbackModule, CallbackState) ->
    CallbackState.

%% A thing returned with #xml-redefine is assumed to be a content list
%% The data may be on "simple" format.

tagdef(Tag,Pos,Parents,Args,CBs) ->
    case apply_tag_cb(CBs, Tag, Args) of
	{'#xml-alias#', NewTag} ->
	    tagdef(NewTag,Pos,Parents,Args,CBs);
	{'#xml-redefine#', Data} ->
	    export_content(xmerl_lib:expand_content(Data, Pos, Parents),
			   CBs);
	Other ->
	    Other
    end.

%% @spec callbacks(Module) -> Result
%%	Module = atom()
%%	Result = [atom()]
%% @doc Find the list of inherited callback modules for a given module.

callbacks(Module) ->
    Result = check_inheritance(Module, []),
%%%     ?dbg("callbacks = ~p~n", [lists:reverse(Result)]),
    lists:reverse(Result).

callbacks([M|Mods], Visited) ->
    case lists:member(M, Visited) of
	false ->
	    NewVisited = check_inheritance(M, Visited),
	    callbacks(Mods, NewVisited);
	true ->
	    exit({cyclic_inheritance, {M, hd(Visited)}})
    end;
callbacks([], Visited) ->
    Visited.

check_inheritance(M, Visited) ->
%%%     ?dbg("calling ~p:'#xml-inheritance#'()~n", [M]),
    case M:'#xml-inheritance#'() of
	[] ->
	    [M|Visited];
	Mods ->
	    callbacks(Mods, [M|Visited])
    end.

apply_text_cb(Ms, Text) ->
    apply_cb(Ms, '#text#', '#text#', [Text]).

apply_tag_cb(Ms, F, Args) ->
    apply_cb(Ms, F, '#element#', Args).

apply_cb(Ms, F, Df, Args) ->
    apply_cb(Ms, F, Df, Args, length(Args)).

apply_cb(Ms, F, Df, Args, A) ->
    apply_cb(Ms, F, Df, Args, A, Ms).

apply_cb([M|Ms], F, Df, Args, A, Ms0) ->
    case erlang:function_exported(M, F, A) of
        true -> apply(M, F, Args);
        false -> apply_cb(Ms, F, Df, Args, A, Ms0)
    end;
apply_cb([], Df, Df, Args, _A, _Ms0) ->
    exit({unknown_tag, {Df, Args}});
apply_cb([], F, Df, Args, A, Ms0) ->
    apply_cb(Ms0, Df, Df, [F|Args], A+1).
