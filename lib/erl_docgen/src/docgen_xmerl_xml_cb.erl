%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the Licence for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%% Portions created by Ericsson are Copyright 1999-2006, Ericsson AB.
%% All Rights Reserved.´´
%%
%%     $Id$
%%
-module(docb_xmerl_xml_cb).

%% This is the callback module for exporting XHTML to a DocBuilder
%% erlref or chapter document in XML format.
%% See docb_edoc_xml_cb.erl for further information.
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

'#root#'(Data, _Attrs, [], _E) -> 
    ["<",DTD,">"] = hd(hd(Data)),
    ["<?xml version=\"1.0\" encoding=\"latin1\" ?>\n",
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
is_url("../"++_) -> true;
is_url(FileRef) ->
    case filename:extension(FileRef) of
	"" -> false; % no extension = xml file, DocBuilder resolves
	_Ext -> true % extension, DocBuilder must not resolve
    end.
