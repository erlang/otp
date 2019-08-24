%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(docgen_xmerl_xml_cb).

%% This is the callback module for exporting XHTML to an
%% erlref or chapter document in XML format.
%% See docgen_edoc_xml_cb.erl for further information.
%%
%% The origin of this file is the xmerl module xmerl_otpsgml.erl
%% written by Ulf Wiger and Richard Carlsson.

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-include("xmerl.hrl").

'#xml-inheritance#'() ->
    [xmerl_xml].

'#root#'(Data, Attrs, [], _E) ->
    Encoding =
        case [E || #xmlAttribute{name = encoding, value = E} <- Attrs] of
            [E] -> E;
            _ -> atom_to_list(epp:default_encoding())
        end,
    ["<",DTD,">"] = hd(hd(Data)),
    ["<?xml version=\"1.0\" encoding=\"",Encoding,"\" ?>\n",
     "<!DOCTYPE "++DTD++" SYSTEM \""++DTD++".dtd\">\n",
     Data].

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    {NewTag, NewAttrs} = convert_tag(Tag, Attrs),
    xmerl_lib:markup(NewTag, NewAttrs, Data).

'#text#'(Text) ->
    xmerl_lib:export_text(Text).

%% Utility functions

convert_tag(a, [Attr]) ->
    case Attr#xmlAttribute.name of
	href ->
	    Val = Attr#xmlAttribute.value,
	    case is_url(Val) of
		true ->
		    {url, [Attr]};
		false ->
		    {seealso, [Attr#xmlAttribute{name=marker}]}
	    end;
	name ->
	    {marker, [Attr#xmlAttribute{name=id}]}
    end;
convert_tag(b, Attrs)          -> {em, Attrs};
convert_tag(blockquote, Attrs) -> {quote, Attrs};
convert_tag(code, Attrs)       -> {c, Attrs};
convert_tag(dd, Attrs)         -> {item, Attrs};
convert_tag(dl, Attrs)         -> {taglist, Attrs};
convert_tag(dt, Attrs)         -> {tag, Attrs};
convert_tag(li, Attrs)         -> {item, Attrs};
convert_tag(ol, Attrs)         -> {list, Attrs};
convert_tag(strong, Attrs)     -> {em, Attrs};
convert_tag(td, Attrs)         -> {cell, Attrs};
convert_tag(tr, Attrs)         -> {row, Attrs};
convert_tag(tt, Attrs)         -> {c, Attrs};
convert_tag(ul, Attrs)         -> {list, Attrs};
convert_tag(underline, Attrs)  -> {em, Attrs};
convert_tag(Tag, Attrs)        -> {Tag, Attrs}.

is_url("http:"++_) -> true;
is_url("https:"++_) -> true;
is_url("../"++_) -> true;
is_url(FileRef) ->
    case filename:extension(FileRef) of
	"" -> false; % no extension = xml file
	_Ext -> true % extension
    end.
