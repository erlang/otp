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

%% Description  : Implements XSLT like transformations in Erlang

%% @doc
%       Erlang has similarities to XSLT since both languages
% 	have a functional programming approach. Using <code>xmerl_xpath</code>
% 	it is possible to write XSLT like transforms in Erlang.
%
%     <p>XSLT stylesheets are often used when transforming XML
%       documents, to other XML documents or (X)HTML for presentation.
%       XSLT contains quite many
%       functions and learning them all may take some effort.
%       This document assumes a basic level of
%       understanding of XSLT.
%     </p>
%     <p>Since XSLT is based on a functional programming approach
%       with pattern matching and recursion it is possible to write
%       similar style sheets in Erlang. At least for basic
%       transforms. This
%       document describes how to use the XPath implementation together
%       with Erlangs pattern matching and a couple of functions to write
%       XSLT like transforms.</p>
%     <p>This approach is probably easier for an Erlanger but
%       if you need to use real XSLT stylesheets in order to "comply to
%       the standard" there is an adapter available to the Sablotron
%       XSLT package which is written i C++.
% See also the <a href="xmerl_xs_examples.html">Tutorial</a>.
%     </p>

-module(xmerl_xs).

-export([xslapply/2, value_of/1, select/2, built_in_rules/2 ]).
-include("xmerl.hrl").


%% @spec xslapply(Function, EList::list()) -> List
%%   Function = () -> list()
%% @doc xslapply is a wrapper to make things look similar to
%% xsl:apply-templates.
%%
%% <p>Example, original XSLT:</p><br/><pre>
%% &lt;xsl:template match="doc/title">
%%   &lt;h1>
%%     &lt;xsl:apply-templates/>
%%   &lt;/h1>
%% &lt;/xsl:template>
%% </pre>
%%
%% <p>becomes in Erlang:</p><br/><pre>
%% template(E = #xmlElement{ parents=[{'doc',_}|_], name='title'}) ->
%%   ["&lt;h1>",
%%    xslapply(fun template/1, E),
%%    "&lt;/h1>"];
%% </pre>

xslapply(Fun, EList) when is_list(EList) ->
    lists:map( Fun, EList);
xslapply(Fun, E = #xmlElement{})->
    lists:map( Fun, E#xmlElement.content).


%% @spec value_of(E) -> List
%%   E = unknown()
%%
%% @doc Concatenates all text nodes within the tree.
%%
%% <p>Example:</p><br/><pre>
%% &lt;xsl:template match="title">
%%   &lt;div align="center">
%%     &lt;h1>&lt;xsl:value-of select="." />&lt;/h1>
%%   &lt;/div>
%% &lt;/xsl:template>
%% </pre>
%%
%%  <p>becomes:</p><br/> <pre>
%%  template(E = #xmlElement{name='title'}) ->
%%    ["&lt;div align="center">&lt;h1>", 
%%      value_of(select(".", E)), "&lt;/h1>&lt;/div>"]
%% </pre>
value_of(E)->
    lists:reverse(xmerl_lib:foldxml(fun value_of1/2, [], E)).

value_of1(#xmlText{}=T1, Accu)->
    [xmerl_lib:export_text(T1#xmlText.value)|Accu];
value_of1(_, Accu) ->
    Accu.

%% @spec select(String::string(),E)-> E
%%
%% @doc Extracts the nodes from the xml tree according to XPath.
%% @see value_of/1
select(Str,E)->
    xmerl_xpath:string(Str,E).

%% @spec built_in_rules(Fun, E) -> List
%%
%% @doc The default fallback behaviour. Template funs should end with:
%% <br/><code>template(E) -> built_in_rules(fun template/1, E)</code>.
built_in_rules(Fun, E = #xmlElement{})->
    lists:map(Fun, E#xmlElement.content);
built_in_rules(_Fun, E = #xmlText{}) ->
    xmerl_lib:export_text(E#xmlText.value);
built_in_rules(_Fun, E = #xmlAttribute{}) ->
    E#xmlAttribute.value;
built_in_rules(_Fun, _E) ->[].
