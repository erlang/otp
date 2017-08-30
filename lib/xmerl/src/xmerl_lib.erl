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

%%% Description  : Utility module for handling XML trees.
%%%----------------------------------------------------------------------

-module(xmerl_lib).

-export([normalize_content/1, normalize_content/3, expand_content/1,
	 expand_content/3, normalize_element/1, normalize_element/3,
	 expand_element/1, expand_element/3, expand_attributes/1,
	 expand_attributes/3, export_text/1, flatten_text/1,
	 export_attribute/1, markup/2, markup/3, simplify_element/1,
	 simplify_content/1, start_tag/1, start_tag/2, end_tag/1,
	 empty_tag/1, empty_tag/2,is_empty_data/1, find_attribute/2,
	 remove_whitespace/1,to_lower/1]).

-export([is_letter/1,is_namechar/1,is_ncname/1,
	 detect_charset/1,detect_charset/2,is_name/1,is_char/1]).


-export([mapxml/2, foldxml/3, mapfoldxml/3]).

%% exports for XSD
-export([is_facet/1,is_builtin_simple_type/1,is_xsd_string/1]).

-include("xmerl.hrl").
-include("xmerl_xsd.hrl").


%% Escape special characters `<' and `&', flattening the text.
%% Also escapes `>', just for symmetry.

export_text(T) ->
    export_text(T, []).

export_text([$< | T], Cont) ->
    "&lt;" ++ export_text(T, Cont);
export_text([$> | T], Cont) ->
    "&gt;" ++ export_text(T, Cont);
export_text([$& | T], Cont) ->
    "&amp;" ++ export_text(T, Cont);
export_text([C | T], Cont) when is_integer(C) ->
    [C | export_text(T, Cont)];
export_text([T | T1], Cont) ->
    export_text(T, [T1 | Cont]);
export_text([], [T | Cont]) ->
    export_text(T, Cont);
export_text([], []) ->
    [];
export_text(Bin, Cont) ->
    export_text(binary_to_list(Bin), Cont).


%% Only flatten text.

flatten_text(T) ->
    flatten_text(T, []).

flatten_text([C | T], Cont) when is_integer(C) ->
    [C | flatten_text(T, Cont)];
flatten_text([T | T1], Cont) ->
    flatten_text(T, [T1 | Cont]);
flatten_text([], [T | Cont]) ->
    flatten_text(T, Cont);
flatten_text([], []) ->
    [];
flatten_text(Bin, Cont) ->
    flatten_text(binary_to_list(Bin), Cont).

%% Convert attribute value to a flat string, escaping characters `"',
%% `<' and `&'. (Note that single-quote characters are not escaped; the
%% markup-generating functions (`start_tag', `end_tag', ...) always use
%% `"' to delimit the attribute values.)

export_attribute(I) when is_integer(I) ->
    integer_to_list(I);
export_attribute(A) when is_atom(A) ->
    export_attribute(atom_to_list(A), []);
export_attribute(S) ->
    export_attribute(S, []).

export_attribute([$< | T], Cont) ->
    "&lt;" ++ export_attribute(T, Cont);
export_attribute([$& | T], Cont) ->
    "&amp;" ++ export_attribute(T, Cont);
export_attribute([$" | T], Cont) ->
    "&quot;" ++ export_attribute(T, Cont);
export_attribute([C | T], Cont) when is_integer(C) ->
    [C | export_attribute(T, Cont)];
export_attribute([T | T1], Cont) ->
    export_attribute(T, [T1 | Cont]);
export_attribute([], [T | Cont]) ->
    export_attribute(T, Cont);
export_attribute([], []) ->
    [];
export_attribute(Bin, Cont) ->
    export_attribute(binary_to_list(Bin), Cont).


%% SimpleContent: [SimpleElement]
%% SimpleElement: #xml...{} | String | {atom(), [Attr], SimpleContent}
%%                | {atom(), SimpleContent} | atom()
%% Attr: {atom(), Value} | #xmlAttribute{}
%% Value: atom() | integer() | String
%% String: [char() | binary() | String]
%%
%% Because strings can be deep, we do not allow content lists to also be
%% deep; otherwise, traversal of the simple representation becomes too
%% complicated and expensive. Simple content lists are thus flat lists
%% of simple elements.

%% TODO: namespace-qualified tags in simple-form? /RC

%% 'normalize' is like 'expand', but also turns all text elements into
%% flat strings.

normalize_element(Element) ->
    normalize_element(Element, 1, []).

normalize_element(Element, Pos, Parents) ->
    expand_element(Element, Pos, Parents, true).

%% 'expand' expands simple-form elements to normal XML elements.
%% All attribute values (also in #xmlAttribute records) become flat
%% strings, so that string comparisons can be made. Text elements are
%% not flattened.

expand_element(Element) ->
    expand_element(Element, 1, []).

expand_element(Element, Pos, Parents) ->
    expand_element(Element, Pos, Parents, false).

expand_element(E = #xmlElement{name = N}, Pos, Parents, Norm) ->
    NewParents = [{N,Pos}|Parents],
    Content = expand_content(E#xmlElement.content, 1, NewParents, Norm),
    Attrs = expand_attributes(E#xmlElement.attributes, 1, NewParents),
    E#xmlElement{pos = Pos,
		 parents = Parents,
		 attributes = Attrs,
		 content = Content};
expand_element(E = #xmlText{}, Pos, Parents, Norm) ->
    E#xmlText{pos = Pos,
	      parents = Parents,
	      value = expand_text(E#xmlText.value, Norm)};
expand_element(E = #xmlPI{}, Pos, Parents, Norm) ->
    E#xmlPI{pos = Pos,
	    parents = Parents,
	    value = expand_text(E#xmlPI.value, Norm)};
expand_element(E = #xmlComment{}, Pos, Parents, Norm) ->
    E#xmlComment{pos = Pos,
		 parents = Parents,
		 value = expand_text(E#xmlComment.value, Norm)};
expand_element(E = #xmlDecl{}, _Pos, _Parents, _Norm) ->
    Attrs = expand_attributes(E#xmlDecl.attributes, 1, []),
    E#xmlDecl{attributes = Attrs};
expand_element({Tag, Attrs, Content}, Pos, Parents, Norm) when is_atom(Tag) ->
    NewParents = [{Tag, Pos} | Parents],
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = expand_attributes(Attrs, 1, NewParents),
		content = expand_content(Content, 1, NewParents, Norm)};
expand_element({Tag, Content}, Pos, Parents, Norm) when is_atom(Tag) ->
    NewParents = [{Tag, Pos} | Parents],
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = [],
		content = expand_content(Content, 1, NewParents, Norm)};
expand_element(Tag, Pos, Parents, _Norm) when is_atom(Tag) ->
    #xmlElement{name = Tag,
		pos = Pos,
		parents = Parents,
		attributes = [],
		content = []};
expand_element(String, Pos, Parents, Norm) when is_list(String) ->
    #xmlText{pos = Pos,
	     parents = Parents,
	     value = expand_text(String, Norm)}.

expand_text(S, false) -> S;
expand_text(S, true) -> flatten_text(S).

%% Content must be a flat list of elements.

normalize_content(Content) ->
    normalize_content(Content, 1, []).

normalize_content(Content, Pos, Parents) ->
    expand_content(Content, Pos, Parents, true).

expand_content(Content) ->
    expand_content(Content, 1, []).

expand_content(Content, Pos, Parents) ->
    expand_content(Content, Pos, Parents, false).

expand_content([{H} | T], Pos, Parents, Norm) ->
    expand_content(H ++ T, Pos, Parents, Norm);
expand_content([{F,S}|T], Pos, Parents, Norm) when is_function(F) ->
    case F(S) of
	done -> expand_content(T, Pos, Parents, Norm);
	{C,S2} -> expand_content([{F,S2},C|T], Pos, Parents, Norm)
    end;
expand_content([H | T], Pos, Parents, Norm) ->
    [expand_element(H, Pos, Parents, Norm)
     | expand_content(T, Pos+1, Parents, Norm)];
expand_content([], _Pos, _Parents, _Norm) ->
    [].

expand_attributes(Attrs) ->
    expand_attributes(Attrs, 1, []).

%% Expanding always turns all attribute values into flat strings.

expand_attributes([H = #xmlAttribute{} | T], Pos, Parents) ->
    [H#xmlAttribute{pos = Pos,
		    value = expand_value(H#xmlAttribute.value)}
     | expand_attributes(T, Pos+1, Parents)];
expand_attributes([{P,S}|T], Pos, Parents) when is_function(P) -> 
    case P(S) of
	done ->
	    expand_attributes(T, Pos, Parents);
	{A,S2} ->
	    expand_attributes([{P,S2},A|T], Pos, Parents)
    end;
expand_attributes([{K, V} | T], Pos, Parents) ->
    [#xmlAttribute{name = K,
		   pos = Pos,
		   parents = Parents,
		   value = expand_value(V)}
     | expand_attributes(T, Pos+1, Parents)];
expand_attributes([], _Pos, _Parents) ->
    [].

expand_value(S) when is_atom(S) ->
    atom_to_list(S);
expand_value(S) when is_integer(S) ->
    integer_to_list(S);
expand_value(S) ->
    flatten_text(S).

%% We want simplification to yield a normal form, so we always generate
%% three-tuples for elements. PI, Comment and Decl elements are
%% discarded from content lists. Attribute values become flat
%% strings. Text elements are not flattened.

simplify_element(#xmlElement{expanded_name = [], name = Tag,
			     attributes = Attrs, content = Content}) ->
    {Tag, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element(#xmlElement{expanded_name = Name,
			     attributes = Attrs, content = Content}) ->
    {Name, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element(#xmlText{value = Text}) ->
    Text;
simplify_element({Tag, Attrs, Content}) when is_atom(Tag) ->
    {Tag, simplify_attributes(Attrs), simplify_content(Content)};
simplify_element({Tag, Content}) when is_atom(Tag) ->
    {Tag, [], simplify_content(Content)};
simplify_element(Tag) when is_atom(Tag) ->
    {Tag, [], []};
simplify_element(Text) when is_list(Text) ->
    Text.

simplify_content([#xmlPI{} | T]) ->
    simplify_content(T);
simplify_content([#xmlComment{} | T]) ->
    simplify_content(T);
simplify_content([#xmlDecl{} | T]) ->
    simplify_content(T);
simplify_content([H | T]) ->
    [simplify_element(H) | simplify_content(T)];
simplify_content([]) ->
    [].

simplify_attributes([#xmlAttribute{name = K, value = V} | T])
  when is_atom(K) ->
    [{K, expand_value(V)} | simplify_attributes(T)];
simplify_attributes([H = {K, _} | T]) when is_atom(K) ->
    [H | simplify_attributes(T)];
simplify_attributes([]) ->
    [].

%% Looking up an attribute value

find_attribute(Name, Attrs) ->
    case lists:keysearch(Name, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    {value, V};
	false ->
	    false
    end.


markup(Tag, Data) ->
    markup(Tag, [], Data).

markup(Tag, Attrs, []) ->
    empty_tag(Tag, Attrs);
markup(Tag, Attrs, Data) ->
    [start_tag(Tag, Attrs), Data, end_tag(Tag)].

start_tag(TagStr) ->
    start_tag(TagStr, []).

start_tag(Tag, Attrs) when is_atom(Tag) ->
    start_tag(atom_to_list(Tag), Attrs);
start_tag(TagStr, []) ->
    ["<", TagStr, ">"];
start_tag(TagStr, Attrs) ->
    ["<", TagStr, attributes(Attrs), ">"].

empty_tag(Tag) ->
    empty_tag(Tag, []).

empty_tag(Tag, Attrs) when is_atom(Tag) ->
    empty_tag(atom_to_list(Tag), Attrs);
empty_tag(TagStr, []) ->
    ["<", TagStr, "/>"];
empty_tag(TagStr, Attrs) ->
    ["<", TagStr, attributes(Attrs), "/>"].

end_tag(Tag) when is_atom(Tag) ->
    end_tag(atom_to_list(Tag));
end_tag(TagStr) ->
    ["</", TagStr, ">"].

attributes(Attrs) ->
    [attr_string(A) || A <- Attrs].

attr_string(#xmlAttribute{name = K, value = V}) ->
    [" ", atom_to_list(K), "=\"", export_attribute(V), "\""].

is_empty_data([]) ->
    true;
is_empty_data([X | Xs]) ->
    case is_empty_data(X) of
	false ->
	    false;
	true ->
	    is_empty_data(Xs)
    end;
is_empty_data(_) ->
    false.


%% Removing normalised whitespace-only text segments.

remove_whitespace([#xmlText{value = " "} | Data]) ->
    remove_whitespace(Data);
remove_whitespace([E = #xmlElement{content = Content} | Data]) ->
    [E#xmlElement{content = remove_whitespace(Content)}
     | remove_whitespace(Data)];
remove_whitespace([Other | Data]) ->
    [Other | remove_whitespace(Data)];
remove_whitespace([]) ->
    [].


%%% ----------------------------------------------------------------------------
%%% funs traversing the xmerl tree left-right and top-down

%% mapxml
%% Fun is fun(Old#xmlElement) -> New#xmlElement
mapxml(Fun, #xmlElement{}= E) ->
    C1 = Fun(E),
    C2 = mapxml(Fun,lists:flatten(C1#xmlElement.content)),
    C1#xmlElement{content=C2};
mapxml(Fun, List) when is_list(List) ->
    AFun = fun(E) -> mapxml(Fun, E) end,
    lists:map(AFun, List);
mapxml(Fun, E) ->
    Fun(E).


%% foldxml
%% Fun is fun(#xmlElement, OldAccu) -> NewAccu
foldxml(Fun, Accu0, #xmlElement{content=C}=E) ->
    Accu1 = Fun(E, Accu0),
    foldxml(Fun, Accu1, C);
foldxml(Fun, Accu, List) when is_list(List) ->
    AFun = fun(E,A) -> foldxml(Fun, A, E) end,
    lists:foldl(AFun, Accu, List);
foldxml(Fun, Accu, E) ->
    Fun(E, Accu).


%% mapfoldxml
%% Fun is fun(Old#xmlElement, OldAccu) -> {New#xmlElement, NewAccu}
mapfoldxml(Fun, Accu0, #xmlElement{}=E) ->
    {C1,Accu1} = Fun(E, Accu0),
    {C2,Accu2} = mapfoldxml(Fun, Accu1, lists:flatten(C1#xmlElement.content)),
    {C1#xmlElement{content=C2},Accu2};
mapfoldxml(Fun, Accu, List) when is_list(List) ->
    AFun = fun(E,A) -> mapfoldxml(Fun, A, E) end,
    lists:mapfoldl(AFun, Accu, List);
mapfoldxml(Fun, Accu, E) ->
    Fun(E,Accu).


%%% @spec detect_charset(T::list()) -> charset_info()
%%% @equiv detect_charset(undefined,T)
detect_charset(Content) ->
    detect_charset(undefined,Content).

%%% FIXME! Whatabout aliases etc? Shouldn't transforming with ucs be optional?
%%% @spec detect_charset(ExtCharset::atom(),T::list()) -> charset_info()
%%% @doc Automatically decides character set used in XML document.
%%%  charset_info() is
%%%  <table>
%%%    <tr><td><code>{auto,'iso-10646-utf-1',Content} |</code></td></tr>
%%%    <tr><td><code>{external,'iso-10646-utf-1',Content} |</code></td></tr>
%%%    <tr><td><code>{undefined,undefined,Content} |</code></td></tr>
%%%    <tr><td><code>{external,ExtCharset,Content}</code></td></tr>
%%%  </table>
%%%   ExtCharset is any externally declared character set (e.g. in HTTP
%%%   Content-Type header) and Content is an XML Document.
%%% 
detect_charset(ExtCharset,Content) when is_list(ExtCharset) ->
    %% FIXME! Don't allow both atom and list for character set names
    detect_charset(list_to_atom(ExtCharset),Content);
detect_charset(ExtCharset,Content) ->
    case autodetect(ExtCharset,Content) of
	{auto,Content1} ->
	    {auto,'iso-10646-utf-1',Content1};
	{external,Content1} ->
	    {external,'iso-10646-utf-1',Content1};
	{undefined,_} ->
	    {undefined,undefined,Content};
	{ExtCharset, Content} ->
	    {external,ExtCharset,Content}
    end.

%%------------------------------------------------------------------------------
%% Auto detect what kind of character set we are dealing with and transform
%% to Erlang integer Unicode format if found.
%% Appendix F, Page 56-57, XML 1.0 W3C Recommendation 6 October 2000
%% (http://www.w3.org/TR/REC-xml)
%% 00 00 00 3C ( "<" in UCS-4 big-endian)
%% 3C 00 00 00 ( "<" in UCS-4 little-endian)
%% FE FF (UTF-16 - big-endian Mark)
%% FF FE (UTF-16 - little-endian Mark)
%% 00 3C 00 3F ( "<?" in UTF-16 big-endian)
%% 3C 00 3F 00 ( "<?" in UTF-16 big-endian)
%% 3C 3F (7-bit,8-bit or mixed width encoding)
%% 4C 6F A7 94 (EBCDIC) - Not Implemented!!!!

%% Check byte-order mark and transform to Unicode, Erlang integer
%%% --- With byte-order mark
autodetect(undefined,[0,0,16#fe,16#ff | Input]) ->
    {auto, xmerl_ucs:from_ucs4be(Input)};
autodetect('iso-10646-utf-1',[0,0,16#fe,16#ff | Input]) ->
    {external, xmerl_ucs:from_ucs4be(Input)};
autodetect(undefined,[16#ff,16#fe,0,0 | Input]) ->
    {auto, xmerl_ucs:from_ucs4le(Input)};
autodetect('iso-10646-utf-1',[16#ff,16#fe,0,0 | Input]) ->
    {external, xmerl_ucs:from_ucs4le(Input)};

autodetect(undefined,[16#fe,16#ff | Input]) ->
    {auto, xmerl_ucs:from_utf16be(Input)};
autodetect('utf-16be',[16#fe,16#ff | Input]) ->
    {external, xmerl_ucs:from_utf16be(Input)};
autodetect(undefined,[16#ff,16#fe | Input]) ->
    {auto, xmerl_ucs:from_utf16le(Input)};
autodetect('utf-16le',[16#ff,16#fe | Input]) ->
    {external, xmerl_ucs:from_utf16le(Input)};

autodetect(undefined,[16#ef,16#bb,16#bf | Input]) ->
    {auto, xmerl_ucs:from_utf8(Input)};
autodetect('utf-8',[16#ef,16#bb,16#bf | Input]) ->
    {external, xmerl_ucs:from_utf8(Input)};
autodetect('utf-8',[16#ff,16#fe | Input]) ->
    {external, xmerl_ucs:from_utf16le(Input)};
autodetect('utf-8',[16#fe,16#ff | Input]) ->
    {external, xmerl_ucs:from_utf16be(Input)};

%%% --- Without byte-order mark
autodetect(undefined,[0,0,0,16#3c|Input]) ->
    {auto, xmerl_ucs:from_ucs4be([0,0,0,16#3c|Input])};
autodetect('iso-10646-utf-1',[0,0,0,16#3c|Input]) ->
    {external, xmerl_ucs:from_ucs4be([0,0,0,16#3c|Input])};
autodetect(undefined,[16#3c,0,0,0|Input]) ->
    {auto, xmerl_ucs:from_ucs4le([16#3c,0,0,0|Input])};
autodetect('iso-10646-utf-1',[16#3c,0,0,0|Input]) ->
    {external, xmerl_ucs:from_ucs4le([16#3c,0,0,0|Input])};

autodetect(undefined,[0,16#3c,0,16#3f | Input]) ->
    {auto, xmerl_ucs:from_utf16be([0,16#3c,0,16#3f|Input])};
autodetect('utf-16be',[0,16#3c,0,16#3f | Input]) ->
    {external, xmerl_ucs:from_utf16be([0,16#3c,0,16#3f|Input])};
autodetect(undefined,[16#3c,0,16#3f,0 | Input]) ->
    {auto, xmerl_ucs:from_utf16le([16#3c,0,16#3f,0|Input])};
autodetect('utf-16le',[16#3c,0,16#3f,0 | Input]) ->
    {external, xmerl_ucs:from_utf16le([16#3c,0,16#3f,0|Input])};

autodetect(ExtCharset,Content) ->
    {ExtCharset, Content}.


is_ncname(A) when is_atom(A) ->
    is_ncname(atom_to_list(A));
is_ncname([$_|T]) ->
    is_name1(T);
is_ncname([H|T]) ->
    case is_letter(H) of
	true ->
	    is_name1(T);
	_ -> false
    end.

is_name(A) when is_atom(A) ->
    is_name(atom_to_list(A));
is_name([$_|T]) ->
    is_name1(T);
is_name([$:|T]) ->
    is_name1(T);
is_name([H|T]) ->
    case is_letter(H) of
	true ->
	    is_name1(T);
	_ -> false
    end.

is_name1([]) ->
    true;
is_name1([H|T]) ->
    case is_namechar(H) of
	true ->
	    is_name1(T);
	_ -> false
    end.


				
% =======
%%% UNICODE character definitions

%%%%%%%% [2] Char

is_char(16#09) -> true;
is_char(16#0A) -> true;
is_char(16#0D) -> true;
is_char(X) when X >= 16#20, X =< 16#D7FF -> true;
is_char(X) when X >= 16#E000, X =< 16#FFFD -> true;
is_char(X) when X >= 16#10000, X =< 16#10FFFF -> true;
is_char(_) -> false.

%% 0 - not classified, 
%% 1 - base_char or ideographic, 
%% 2 - combining_char or digit or extender,
%% 3 - $. or $- or $_ or $:
-define(SMALL, {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,0,2,2,2,2,2,2,2,2,2,2,3,0,
                0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,0,0,0,0,3,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,2,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1}).

%% [4] NameChar
is_namechar(X) ->
    try element(X, ?SMALL) > 0
    catch _:_ ->
        case is_letter(X) of
            true -> true;
            false ->
                case is_digit(X) of
                    true -> true;
                    false ->
                        case is_combining_char(X) of
                            true -> true;
                            false ->
                                is_extender(X)
                        end
                end
        end
    end.

%% [84] Letter
is_letter(X) ->
    try element(X, ?SMALL) =:= 1
    catch _:_ -> 
        case is_base_char(X) of
	    false ->
	        is_ideographic(X);
    	    true ->
	        true
        end
    end.

%% [85] BaseChar
is_base_char(X) when X >= 16#0041, X =< 16#005A -> true;
is_base_char(X) when X >= 16#0061, X =< 16#007A -> true;
is_base_char(X) when X >= 16#00C0, X =< 16#00D6 -> true;
is_base_char(X) when X >= 16#00D8, X =< 16#00F6 -> true;
is_base_char(X) when X >= 16#00F8, X =< 16#00FF -> true;
is_base_char(X) when X >= 16#0100, X =< 16#0131 -> true;
is_base_char(X) when X >= 16#0134, X =< 16#013E -> true;
is_base_char(X) when X >= 16#0141, X =< 16#0148 -> true;
is_base_char(X) when X >= 16#014A, X =< 16#017E -> true;
is_base_char(X) when X >= 16#0180, X =< 16#01C3 -> true;
is_base_char(X) when X >= 16#01CD, X =< 16#01F0 -> true;
is_base_char(X) when X >= 16#01F4, X =< 16#01F5 -> true;
is_base_char(X) when X >= 16#01FA, X =< 16#0217 -> true;
is_base_char(X) when X >= 16#0250, X =< 16#02A8 -> true;
is_base_char(X) when X >= 16#02BB, X =< 16#02C1 -> true;
is_base_char(16#0386) -> true;
is_base_char(X) when X >= 16#0388, X =< 16#038A -> true;
is_base_char(16#038C) -> true;
is_base_char(X) when X >= 16#038E, X =< 16#03A1 -> true;
is_base_char(X) when X >= 16#03A3, X =< 16#03CE -> true;
is_base_char(X) when X >= 16#03D0, X =< 16#03D6 -> true;
is_base_char(16#03DA) -> true;
is_base_char(16#03DC) -> true;
is_base_char(16#03DE) -> true;
is_base_char(16#03E0) -> true;
is_base_char(X) when X >= 16#03E2, X =< 16#03F3 -> true;
is_base_char(X) when X >= 16#0401, X =< 16#040C -> true;
is_base_char(X) when X >= 16#040E, X =< 16#044F -> true;
is_base_char(X) when X >= 16#0451, X =< 16#045C -> true;
is_base_char(X) when X >= 16#045E, X =< 16#0481 -> true;
is_base_char(X) when X >= 16#0490, X =< 16#04C4 -> true;
is_base_char(X) when X >= 16#04C7, X =< 16#04C8 -> true;
is_base_char(X) when X >= 16#04CB, X =< 16#04CC -> true;
is_base_char(X) when X >= 16#04D0, X =< 16#04EB -> true;
is_base_char(X) when X >= 16#04EE, X =< 16#04F5 -> true;
is_base_char(X) when X >= 16#04F8, X =< 16#04F9 -> true;
is_base_char(X) when X >= 16#0531, X =< 16#0556 -> true;
is_base_char(16#0559) -> true;
is_base_char(X) when X >= 16#0561, X =< 16#0586 -> true;
is_base_char(X) when X >= 16#05D0, X =< 16#05EA -> true;
is_base_char(X) when X >= 16#05F0, X =< 16#05F2 -> true;
is_base_char(X) when X >= 16#0621, X =< 16#063A -> true;
is_base_char(X) when X >= 16#0641, X =< 16#064A -> true;
is_base_char(X) when X >= 16#0671, X =< 16#06B7 -> true;
is_base_char(X) when X >= 16#06BA, X =< 16#06BE -> true;
is_base_char(X) when X >= 16#06C0, X =< 16#06CE -> true;
is_base_char(X) when X >= 16#06D0, X =< 16#06D3 -> true;
is_base_char(16#06D5) -> true;
is_base_char(X) when X >= 16#06E5, X =< 16#06E6 -> true;
is_base_char(X) when X >= 16#0905, X =< 16#0939 -> true;
is_base_char(16#093D) -> true;
is_base_char(X) when X >= 16#0958, X =< 16#0961 -> true;
is_base_char(X) when X >= 16#0985, X =< 16#098C -> true;
is_base_char(X) when X >= 16#098F, X =< 16#0990 -> true;
is_base_char(X) when X >= 16#0993, X =< 16#09A8 -> true;
is_base_char(X) when X >= 16#09AA, X =< 16#09B0 -> true;
is_base_char(16#09B2) -> true;
is_base_char(X) when X >= 16#09B6, X =< 16#09B9 -> true;
is_base_char(X) when X >= 16#09DC, X =< 16#09DD -> true;
is_base_char(X) when X >= 16#09DF, X =< 16#09E1 -> true;
is_base_char(X) when X >= 16#09F0, X =< 16#09F1 -> true;
is_base_char(X) when X >= 16#0A05, X =< 16#0A0A -> true;
is_base_char(X) when X >= 16#0A0F, X =< 16#0A10 -> true;
is_base_char(X) when X >= 16#0A13, X =< 16#0A28 -> true;
is_base_char(X) when X >= 16#0A2A, X =< 16#0A30 -> true;
is_base_char(X) when X >= 16#0A32, X =< 16#0A33 -> true;
is_base_char(X) when X >= 16#0A35, X =< 16#0A36 -> true;
is_base_char(X) when X >= 16#0A38, X =< 16#0A39 -> true;
is_base_char(X) when X >= 16#0A59, X =< 16#0A5C -> true;
is_base_char(16#0A5E) -> true;
is_base_char(X) when X >= 16#0A72, X =< 16#0A74 -> true;
is_base_char(X) when X >= 16#0A85, X =< 16#0A8B -> true;
is_base_char(16#0A8D) -> true;
is_base_char(X) when X >= 16#0A8F, X =< 16#0A91 -> true;
is_base_char(X) when X >= 16#0A93, X =< 16#0AA8 -> true;
is_base_char(X) when X >= 16#0AAA, X =< 16#0AB0 -> true;
is_base_char(X) when X >= 16#0AB2, X =< 16#0AB3 -> true;
is_base_char(X) when X >= 16#0AB5, X =< 16#0AB9 -> true;
is_base_char(16#0ABD) -> true;
is_base_char(16#0AE0) -> true;
is_base_char(X) when X >= 16#0B05, X =< 16#0B0C -> true;
is_base_char(X) when X >= 16#0B0F, X =< 16#0B10 -> true;
is_base_char(X) when X >= 16#0B13, X =< 16#0B28 -> true;
is_base_char(X) when X >= 16#0B2A, X =< 16#0B30 -> true;
is_base_char(X) when X >= 16#0B32, X =< 16#0B33 -> true;
is_base_char(X) when X >= 16#0B36, X =< 16#0B39 -> true;
is_base_char(16#0B3D) -> true;
is_base_char(X) when X >= 16#0B5C, X =< 16#0B5D -> true;
is_base_char(X) when X >= 16#0B5F, X =< 16#0B61 -> true;
is_base_char(X) when X >= 16#0B85, X =< 16#0B8A -> true;
is_base_char(X) when X >= 16#0B8E, X =< 16#0B90 -> true;
is_base_char(X) when X >= 16#0B92, X =< 16#0B95 -> true;
is_base_char(X) when X >= 16#0B99, X =< 16#0B9A -> true;
is_base_char(16#0B9C) -> true;
is_base_char(X) when X >= 16#0B9E, X =< 16#0B9F -> true;
is_base_char(X) when X >= 16#0BA3, X =< 16#0BA4 -> true;
is_base_char(X) when X >= 16#0BA8, X =< 16#0BAA -> true;
is_base_char(X) when X >= 16#0BAE, X =< 16#0BB5 -> true;
is_base_char(X) when X >= 16#0BB7, X =< 16#0BB9 -> true;
is_base_char(X) when X >= 16#0C05, X =< 16#0C0C -> true;
is_base_char(X) when X >= 16#0C0E, X =< 16#0C10 -> true;
is_base_char(X) when X >= 16#0C12, X =< 16#0C28 -> true;
is_base_char(X) when X >= 16#0C2A, X =< 16#0C33 -> true;
is_base_char(X) when X >= 16#0C35, X =< 16#0C39 -> true;
is_base_char(X) when X >= 16#0C60, X =< 16#0C61 -> true;
is_base_char(X) when X >= 16#0C85, X =< 16#0C8C -> true;
is_base_char(X) when X >= 16#0C8E, X =< 16#0C90 -> true;
is_base_char(X) when X >= 16#0C92, X =< 16#0CA8 -> true;
is_base_char(X) when X >= 16#0CAA, X =< 16#0CB3 -> true;
is_base_char(X) when X >= 16#0CB5, X =< 16#0CB9 -> true;
is_base_char(16#0CDE) -> true;
is_base_char(X) when X >= 16#0CE0, X =< 16#0CE1 -> true;
is_base_char(X) when X >= 16#0D05, X =< 16#0D0C -> true;
is_base_char(X) when X >= 16#0D0E, X =< 16#0D10 -> true;
is_base_char(X) when X >= 16#0D12, X =< 16#0D28 -> true;
is_base_char(X) when X >= 16#0D2A, X =< 16#0D39 -> true;
is_base_char(X) when X >= 16#0D60, X =< 16#0D61 -> true;
is_base_char(X) when X >= 16#0E01, X =< 16#0E2E -> true;
is_base_char(16#0E30) -> true;
is_base_char(X) when X >= 16#0E32, X =< 16#0E33 -> true;
is_base_char(X) when X >= 16#0E40, X =< 16#0E45 -> true;
is_base_char(X) when X >= 16#0E81, X =< 16#0E82 -> true;
is_base_char(16#0E84) -> true;
is_base_char(X) when X >= 16#0E87, X =< 16#0E88 -> true;
is_base_char(16#0E8A) -> true;
is_base_char(16#0E8D) -> true;
is_base_char(X) when X >= 16#0E94, X =< 16#0E97 -> true;
is_base_char(X) when X >= 16#0E99, X =< 16#0E9F -> true;
is_base_char(X) when X >= 16#0EA1, X =< 16#0EA3 -> true;
is_base_char(16#0EA5) -> true;
is_base_char(16#0EA7) -> true;
is_base_char(X) when X >= 16#0EAA, X =< 16#0EAB -> true;
is_base_char(X) when X >= 16#0EAD, X =< 16#0EAE -> true;
is_base_char(16#0EB0) -> true;
is_base_char(X) when X >= 16#0EB2, X =< 16#0EB3 -> true;
is_base_char(16#0EBD) -> true;
is_base_char(X) when X >= 16#0EC0, X =< 16#0EC4 -> true;
is_base_char(X) when X >= 16#0F40, X =< 16#0F47 -> true;
is_base_char(X) when X >= 16#0F49, X =< 16#0F69 -> true;
is_base_char(X) when X >= 16#10A0, X =< 16#10C5 -> true;
is_base_char(X) when X >= 16#10D0, X =< 16#10F6 -> true;
is_base_char(16#1100) -> true;
is_base_char(X) when X >= 16#1102, X =< 16#1103 -> true;
is_base_char(X) when X >= 16#1105, X =< 16#1107 -> true;
is_base_char(16#1109) -> true;
is_base_char(X) when X >= 16#110B, X =< 16#110C -> true;
is_base_char(X) when X >= 16#110E, X =< 16#1112 -> true;
is_base_char(16#113C) -> true;
is_base_char(16#113E) -> true;
is_base_char(16#1140) -> true;
is_base_char(16#114C) -> true;
is_base_char(16#114E) -> true;
is_base_char(16#1150) -> true;
is_base_char(X) when X >= 16#1154, X =< 16#1155 -> true;
is_base_char(16#1159) -> true;
is_base_char(X) when X >= 16#115F, X =< 16#1161 -> true;
is_base_char(16#1163) -> true;
is_base_char(16#1165) -> true;
is_base_char(16#1167) -> true;
is_base_char(16#1169) -> true;
is_base_char(X) when X >= 16#116D, X =< 16#116E -> true;
is_base_char(X) when X >= 16#1172, X =< 16#1173 -> true;
is_base_char(16#1175) -> true;
is_base_char(16#119E) -> true;
is_base_char(16#11A8) -> true;
is_base_char(16#11AB) -> true;
is_base_char(X) when X >= 16#11AE, X =< 16#11AF -> true;
is_base_char(X) when X >= 16#11B7, X =< 16#11B8 -> true;
is_base_char(16#11BA) -> true;
is_base_char(X) when X >= 16#11BC, X =< 16#11C2 -> true;
is_base_char(16#11EB) -> true;
is_base_char(16#11F0) -> true;
is_base_char(16#11F9) -> true;
is_base_char(X) when X >= 16#1E00, X =< 16#1E9B -> true;
is_base_char(X) when X >= 16#1EA0, X =< 16#1EF9 -> true;
is_base_char(X) when X >= 16#1F00, X =< 16#1F15 -> true;
is_base_char(X) when X >= 16#1F18, X =< 16#1F1D -> true;
is_base_char(X) when X >= 16#1F20, X =< 16#1F45 -> true;
is_base_char(X) when X >= 16#1F48, X =< 16#1F4D -> true;
is_base_char(X) when X >= 16#1F50, X =< 16#1F57 -> true;
is_base_char(16#1F59) -> true;
is_base_char(16#1F5B) -> true;
is_base_char(16#1F5D) -> true;
is_base_char(X) when X >= 16#1F5F, X =< 16#1F7D -> true;
is_base_char(X) when X >= 16#1F80, X =< 16#1FB4 -> true;
is_base_char(X) when X >= 16#1FB6, X =< 16#1FBC -> true;
is_base_char(16#1FBE) -> true;
is_base_char(X) when X >= 16#1FC2, X =< 16#1FC4 -> true;
is_base_char(X) when X >= 16#1FC6, X =< 16#1FCC -> true;
is_base_char(X) when X >= 16#1FD0, X =< 16#1FD3 -> true;
is_base_char(X) when X >= 16#1FD6, X =< 16#1FDB -> true;
is_base_char(X) when X >= 16#1FE0, X =< 16#1FEC -> true;
is_base_char(X) when X >= 16#1FF2, X =< 16#1FF4 -> true;
is_base_char(X) when X >= 16#1FF6, X =< 16#1FFC -> true;
is_base_char(16#2126) -> true;
is_base_char(X) when X >= 16#212A, X =< 16#212B -> true;
is_base_char(16#212E) -> true;
is_base_char(X) when X >= 16#2180, X =< 16#2182 -> true;
is_base_char(X) when X >= 16#3041, X =< 16#3094 -> true;
is_base_char(X) when X >= 16#30A1, X =< 16#30FA -> true;
is_base_char(X) when X >= 16#3105, X =< 16#312C -> true;
is_base_char(X) when X >= 16#ac00, X =< 16#d7a3 -> true;
is_base_char(_) ->
    false.

%% [86] Ideographic
is_ideographic(X) when X >= 16#4e00, X =< 16#9fa5 -> true;
is_ideographic(16#3007) -> true;
is_ideographic(X) when X >= 16#3021, X =< 16#3029 -> true;
is_ideographic(_) ->
    false.

%% [87] CombiningChar
is_combining_char(X) when X >= 16#0300, X =< 16#0345 -> true;
is_combining_char(X) when X >= 16#0360, X =< 16#0361 -> true;
is_combining_char(X) when X >= 16#0483, X =< 16#0486 -> true;
is_combining_char(X) when X >= 16#0591, X =< 16#05a1 -> true;
is_combining_char(X) when X >= 16#05a3, X =< 16#05b9 -> true;
is_combining_char(X) when X >= 16#05bb, X =< 16#05bd -> true;
is_combining_char(16#05bf) -> true;
is_combining_char(X) when X >= 16#05c1, X =< 16#05c2 -> true;
is_combining_char(16#05c4) -> true;
is_combining_char(X) when X >= 16#064b, X =< 16#0652 -> true;
is_combining_char(16#0670) -> true;
is_combining_char(X) when X >= 16#06d6, X =< 16#06dc -> true;
is_combining_char(X) when X >= 16#06dd, X =< 16#06df -> true;
is_combining_char(X) when X >= 16#06e0, X =< 16#06e4 -> true;
is_combining_char(X) when X >= 16#06e7, X =< 16#06e8 -> true;
is_combining_char(X) when X >= 16#06ea, X =< 16#06ed -> true;
is_combining_char(X) when X >= 16#0901, X =< 16#0903 -> true;
is_combining_char(16#093c) -> true;
is_combining_char(X) when X >= 16#093e, X =< 16#094c -> true;
is_combining_char(16#094d) -> true;
is_combining_char(X) when X >= 16#0951, X =< 16#0954 -> true;
is_combining_char(X) when X >= 16#0962, X =< 16#0963 -> true;
is_combining_char(X) when X >= 16#0981, X =< 16#0983 -> true;
is_combining_char(16#09bc) -> true;
is_combining_char(16#09be) -> true;
is_combining_char(16#09bf) -> true;
is_combining_char(X) when X >= 16#09c0, X =< 16#09c4 -> true;
is_combining_char(X) when X >= 16#09c7, X =< 16#09c8 -> true;
is_combining_char(X) when X >= 16#09cb, X =< 16#09cd -> true;
is_combining_char(16#09d7) -> true;
is_combining_char(X) when X >= 16#09e2, X =< 16#09e3 -> true;
is_combining_char(16#0a02) -> true;
is_combining_char(16#0a3c) -> true;
is_combining_char(16#0a3e) -> true;
is_combining_char(16#0a3f) -> true;
is_combining_char(X) when X >= 16#0a40, X =< 16#0a42 -> true;
is_combining_char(X) when X >= 16#0a47, X =< 16#0a48 -> true;
is_combining_char(X) when X >= 16#0a4b, X =< 16#0a4d -> true;
is_combining_char(X) when X >= 16#0a70, X =< 16#0a71 -> true;
is_combining_char(X) when X >= 16#0a81, X =< 16#0a83 -> true;
is_combining_char(16#0abc) -> true;
is_combining_char(X) when X >= 16#0abe, X =< 16#0ac5 -> true;
is_combining_char(X) when X >= 16#0ac7, X =< 16#0ac9 -> true;
is_combining_char(X) when X >= 16#0acb, X =< 16#0acd -> true;
is_combining_char(X) when X >= 16#0b01, X =< 16#0b03 -> true;
is_combining_char(16#0b3c) -> true;
is_combining_char(X) when X >= 16#0b3e, X =< 16#0b43 -> true;
is_combining_char(X) when X >= 16#0b47, X =< 16#0b48 -> true;
is_combining_char(X) when X >= 16#0b4b, X =< 16#0b4d -> true;
is_combining_char(X) when X >= 16#0b56, X =< 16#0b57 -> true;
is_combining_char(X) when X >= 16#0b82, X =< 16#0b83 -> true;
is_combining_char(X) when X >= 16#0bbe, X =< 16#0bc2 -> true;
is_combining_char(X) when X >= 16#0bc6, X =< 16#0bc8 -> true;
is_combining_char(X) when X >= 16#0bca, X =< 16#0bcd -> true;
is_combining_char(16#0bd7) -> true;
is_combining_char(X) when X >= 16#0c01, X =< 16#0c03 -> true;
is_combining_char(X) when X >= 16#0c3e, X =< 16#0c44 -> true;
is_combining_char(X) when X >= 16#0c46, X =< 16#0c48 -> true;
is_combining_char(X) when X >= 16#0c4a, X =< 16#0c4d -> true;
is_combining_char(X) when X >= 16#0c55, X =< 16#0c56 -> true;
is_combining_char(X) when X >= 16#0c82, X =< 16#0c83 -> true;
is_combining_char(X) when X >= 16#0cbe, X =< 16#0cc4 -> true;
is_combining_char(X) when X >= 16#0cc6, X =< 16#0cc8 -> true;
is_combining_char(X) when X >= 16#0cca, X =< 16#0ccd -> true;
is_combining_char(X) when X >= 16#0cd5, X =< 16#0cd6 -> true;
is_combining_char(X) when X >= 16#0d02, X =< 16#0d03 -> true;
is_combining_char(X) when X >= 16#0d3e, X =< 16#0d43 -> true;
is_combining_char(X) when X >= 16#0d46, X =< 16#0d48 -> true;
is_combining_char(X) when X >= 16#0d4a, X =< 16#0d4d -> true;
is_combining_char(16#0d57) -> true;
is_combining_char(16#0e31) -> true;
is_combining_char(X) when X >= 16#0e34, X =< 16#0e3a -> true;
is_combining_char(X) when X >= 16#0e47, X =< 16#0e4e -> true;
is_combining_char(16#0eb1) -> true;
is_combining_char(X) when X >= 16#0eb4, X =< 16#0eb9 -> true;
is_combining_char(X) when X >= 16#0ebb, X =< 16#0ebc -> true;
is_combining_char(X) when X >= 16#0ec8, X =< 16#0ecd -> true;
is_combining_char(X) when X >= 16#0f18, X =< 16#0f19 -> true;
is_combining_char(16#0f35) -> true;
is_combining_char(16#0f37) -> true;
is_combining_char(16#0f39) -> true;
is_combining_char(16#0f3e) -> true;
is_combining_char(16#0f3f) -> true;
is_combining_char(X) when X >= 16#0f71, X =< 16#0f84 -> true;
is_combining_char(X) when X >= 16#0f86, X =< 16#0f8b -> true;
is_combining_char(X) when X >= 16#0f90, X =< 16#0f95 -> true;
is_combining_char(16#0f97) -> true;
is_combining_char(X) when X >= 16#0f99, X =< 16#0fad -> true;
is_combining_char(X) when X >= 16#0fb1, X =< 16#0fb7 -> true;
is_combining_char(16#0fb9) -> true;
is_combining_char(X) when X >= 16#20d0, X =< 16#20dc -> true;
is_combining_char(16#20e1) -> true;
is_combining_char(X) when X >= 16#302a, X =< 16#302f -> true;
is_combining_char(16#3099) -> true;
is_combining_char(16#309a) -> true;
is_combining_char(_) -> false.

%% [88] Digit
is_digit(X) when X >= 16#0030, X =< 16#0039 -> true;
is_digit(X) when X >= 16#0660, X =< 16#0669 -> true;
is_digit(X) when X >= 16#06F0, X =< 16#06F9 -> true;
is_digit(X) when X >= 16#0966, X =< 16#096f -> true;
is_digit(X) when X >= 16#09e6, X =< 16#09ef -> true;
is_digit(X) when X >= 16#0a66, X =< 16#0a6f -> true;
is_digit(X) when X >= 16#0ae6, X =< 16#0aef -> true;
is_digit(X) when X >= 16#0b66, X =< 16#0b6f -> true;
is_digit(X) when X >= 16#0be7, X =< 16#0bef -> true;
is_digit(X) when X >= 16#0c66, X =< 16#0c6f -> true;
is_digit(X) when X >= 16#0ce6, X =< 16#0cef -> true;
is_digit(X) when X >= 16#0d66, X =< 16#0d6f -> true;
is_digit(X) when X >= 16#0e50, X =< 16#0e59 -> true;
is_digit(X) when X >= 16#0ed0, X =< 16#0ed9 -> true;
is_digit(X) when X >= 16#0f20, X =< 16#0f29 -> true;
is_digit(_) -> false.

%% [89] Extender
is_extender(16#00b7) -> true;
is_extender(16#02d0) -> true;
is_extender(16#02d1) -> true;
is_extender(16#0387) -> true;
is_extender(16#0640) -> true;
is_extender(16#0e46) -> true;
is_extender(16#0ec6) -> true;
is_extender(16#3005) -> true;
is_extender(X) when X >= 16#3031, X =< 16#3035 -> true;
is_extender(X) when X >= 16#309d, X =< 16#309e -> true;
is_extender(X) when X >= 16#30fc, X =< 16#30fe -> true;
is_extender(_) -> false.

to_lower(Str) ->
    to_lower(Str, []).
to_lower([C|Cs], Acc) when C >= $A, C =< $Z ->
    to_lower(Cs, [C+($a-$A)| Acc]);
to_lower([C|Cs], Acc) ->
    to_lower(Cs, [C| Acc]);
to_lower([], Acc) ->
    lists:reverse(Acc).

%%% XSD helpers

is_facet(length) -> true;
is_facet(minLength) -> true;
is_facet(maxLength) -> true;
is_facet(pattern) -> true;
is_facet(enumeration) -> true;
is_facet(whiteSpace) -> true;
is_facet(maxInclusive) -> true;
is_facet(maxExclusive) -> true;
is_facet(minInclusive) -> true;
is_facet(minExclusive) -> true;
is_facet(totalDigits) -> true;
is_facet(fractionDigits) -> true;
is_facet(_) -> false.
    

is_builtin_simple_type({Type,_,?XSD_NAMESPACE}) when is_atom(Type) ->
    is_builtin_simple_type(atom_to_list(Type));
is_builtin_simple_type({Type,_,?XSD_NAMESPACE}) ->
    is_builtin_simple_type(Type);
is_builtin_simple_type({_,_,_}) ->
    false;
is_builtin_simple_type("string") -> true;
is_builtin_simple_type("normalizedString") -> true;
is_builtin_simple_type("token") -> true;
is_builtin_simple_type("base64Binary") -> true;
is_builtin_simple_type("hexBinary") -> true;
is_builtin_simple_type("integer") -> true;
is_builtin_simple_type("positiveInteger") -> true;
is_builtin_simple_type("negativeInteger") -> true;
is_builtin_simple_type("nonNegativeInteger") -> true;
is_builtin_simple_type("nonPositiveInteger") -> true;
is_builtin_simple_type("long") -> true;
is_builtin_simple_type("unsignedLong") -> true;
is_builtin_simple_type("int") -> true;
is_builtin_simple_type("unsignedInt") -> true;
is_builtin_simple_type("short") -> true;
is_builtin_simple_type("unsignedShort") -> true;
is_builtin_simple_type("decimal") -> true;
is_builtin_simple_type("float") -> true;
is_builtin_simple_type("double") -> true;
is_builtin_simple_type("boolean") -> true;
is_builtin_simple_type("duration") -> true;
is_builtin_simple_type("dateTime") -> true;
is_builtin_simple_type("date") -> true;
is_builtin_simple_type("time") -> true;
is_builtin_simple_type("gYear") -> true;
is_builtin_simple_type("gYearMonth") -> true;
is_builtin_simple_type("gMonth") -> true;
is_builtin_simple_type("gMonthDay") -> true;
is_builtin_simple_type("gDay") -> true;
is_builtin_simple_type("Name") -> true;
is_builtin_simple_type("QName") -> true;
is_builtin_simple_type("NCName") -> true;
is_builtin_simple_type("anyURI") -> true;
is_builtin_simple_type("language") -> true;
is_builtin_simple_type("ID") -> true;
is_builtin_simple_type("IDREF") -> true;
is_builtin_simple_type("IDREFS") -> true;
is_builtin_simple_type("ENTITY") -> true;
is_builtin_simple_type("ENTITIES") ->true;
is_builtin_simple_type("NOTATION") -> true;
is_builtin_simple_type("NMTOKEN") -> true;
is_builtin_simple_type("NMTOKENS") -> true;
is_builtin_simple_type("byte") -> true;
is_builtin_simple_type("unsignedByte") -> true;
is_builtin_simple_type(_) -> false.

is_xsd_string({Type,_,?XSD_NAMESPACE}) when is_atom(Type) ->
    is_xsd_string(Type);
is_xsd_string({Type,_,?XSD_NAMESPACE}) ->
    is_xsd_string(Type);
is_xsd_string({_,_,_}) ->
    false;
is_xsd_string(Atom) when is_atom(Atom) ->
    is_xsd_string(atom_to_list(Atom));
is_xsd_string("string") ->
    true;
is_xsd_string("normalizedString") ->
    true;
is_xsd_string("token") ->
    true;
is_xsd_string("language") ->
    true;
is_xsd_string("Name") ->
    true;
is_xsd_string("NMTOKEN") ->
    true;
is_xsd_string("NMTOKENS") ->
    true;
is_xsd_string("NCName") ->
    true;
is_xsd_string("ID") ->
    true;
is_xsd_string("IDREF") ->
    true;
is_xsd_string("IDREFS") ->
    true;
is_xsd_string("ENTITY") ->
    true;
is_xsd_string("ENTITIES") ->
    true;
is_xsd_string(_) ->
    false.
