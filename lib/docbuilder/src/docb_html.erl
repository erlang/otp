%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_html).

-export([rule/2, rule/3]).

rule([p, item, list|_], {_, _, _}) ->
    {"", "<br />\n"};
rule([p, item, taglist|_], {_, _, _}) ->
    {"", "<br />\n"};
rule([p|_], _) ->
    {"\n<p>", "\n</p>"};

rule([pre|_], _) ->
    {"\n<div class=\"example\"><pre>\n", "\n</pre></div>\n"};

rule([input|_], _) ->
    {"<strong>", "</strong>"};

rule([quote|_], _) ->
    {"\n<blockquote>\n", "\n</blockquote>\n"};

rule([i|_], _) ->
    {"<em>", "</em>"};

rule([b|_], _) ->
    {"<strong>", "</strong>"};

rule([c|_], _) ->
    {"<span class=\"code\">", "</span>"};

rule([em|_], _) ->
    {"<strong>", "</strong>"};

rule([sub|_], _) ->
    {"<sub>", "</sub>"};

rule([sup|_], _) ->
    {"<sup>", "</sup>"};

rule([termdef|_], _) ->
    {drop, ""};

rule([citedef|_], _) ->
    {drop, ""};

rule([br|_], _) ->
    {"<br />\n", ""};

rule([digression|_], _) ->
    {"<table>\n"
     "  <tr>\n"
     "    <td width=\"23\"></td>\n"
     "    <td>\n"
     "      <font size=\"-1\">\n",
     "      </font>\n"
     "    </td>\n"
     "  </tr>\n"
     "</table>\n"};

rule([list, item, list|_], {_, ["ORDERED"], _}) ->
    {"\n<ol>\n", "\n</ol>\n"};
rule([list, item, taglist|_], {_, ["ORDERED"], _}) ->
    {"\n<ol>\n", "\n</ol>\n"};
rule([list|_], {_, ["ORDERED"], _}) ->
    {"\n<ol>\n", "\n</ol>\n"};
rule([list, item, list|_], {_, ["BULLETED"], _}) ->
    {"\n<ul>\n", "\n</ul>\n"};
rule([list, item, taglist|_], {_, ["BULLETED"], _}) ->
    {"\n<ul>\n", "\n</ul>\n"};
rule([list|_], {_, ["BULLETED"], _}) ->
    {"\n<ul>\n", "\n</ul>\n"};

rule([taglist, item, taglist|_], _) ->
    {"\n<dl>\n", "\n</dl>\n"};
rule([taglist, item, list|_], _) ->
    {"\n<dl>\n", "\n</dl>\n"};
rule([taglist|_], _) ->
    {"\n<dl>\n", "\n</dl>\n"};

rule([tag|_], _) ->
    {"\n<dt>\n", "\n</dt>\n"};

rule([item, list|_], _) ->
    {"\n<li>\n", "\n</li>\n\n"};
rule([item, taglist|_], _) ->
    {"\n<dd>\n", "\n</dd>\n"};

rule([image|_], {_, [File], _}) ->
    File2 = 
	case filename:extension(File) of
	    [] -> File ++ ".gif";
	    _ -> File
	end,
    {["\n<center>\n", "<img alt=\"", File2, "\" src=\"", File2,
      "\"/><br/>\n"],
     "\n</center>\n"};

rule([icaption|_], _) ->
    {"<em>", "</em>\n"};

rule([url|_], {_, [HREF], _}) ->
    URI = docb_html_util:make_uri(HREF),
    {io_lib:format("<a target=\"_top\" href=\"~s\">", [URI]), "</a>"};

rule([marker|_], {_, [ID], _})   ->
    %% remove all chars before first # including the #
    {ok, NewID, _} = regexp:sub(ID, "^[^#]*#", ""), 
    %% replace "/" with "-" because "/" xhtml does not
    %% allow "/" in the name attribute of element <a>
    %% so we have to do the same as for marker i.e
    %% Function/Arity is translated to an anchor in xhtml
    %% like this : <a name="Function-Arity"/>
    NewID2 = [case X of $/ -> $-;_->X end||X <- NewID], 
    {drop, ["<a name=\"", NewID2, "\"><!-- Empty --></a>"]};

rule([table|_], {_, ["", ""], Ts}) ->
    {newargs,
     "\n<center>\n"
     "<table cellspacing=\"0\" cellpadding=\"2\" border=\"1\">\n",
     reorder_table(Ts),
     "\n</table>\n"
     "</center>\n"};
rule([table|_], {_, [Width, ""], Ts}) ->
    {newargs,
     ["\n<center>\n"
      "<table cellspacing=\"0\" cellpadding=\"2\" border=\"1\" ",
      "width=\"", Width, "%\">\n"],
     reorder_table(Ts),
     "\n</table>\n"
     "</center>\n"};

%% The clauses above are for the report DTD. This one is for the other
%% DTDs.
rule([table|_], {_, ["LEFT"], Ts}) ->
    {newargs,
     "\n"
     "<table cellspacing=\"0\" cellpadding=\"2\" border=\"1\">\n",
     reorder_table(Ts),
     "\n</table>\n"};

rule([table|_], {_, _, Ts}) ->
    {newargs,
     "\n<center>\n"
     "<table cellspacing=\"0\" cellpadding=\"2\" border=\"1\">\n",
     reorder_table(Ts),
     "\n</table>\n"
     "</center>\n"};

rule([row|_], _)   ->
    {"  <tr>\n", "\n  </tr>\n"};

rule([cell|_], {_, ["", ""], _})   ->
    {"    <td>\n", "\n    </td>\n"};
rule([cell|_], {_, [Align, ""], _})   ->
    {["    <td align=\"", string:to_lower(Align), "\">\n"], "\n    </td>\n"};
rule([cell|_], {_, ["", VAlign], _})   ->
    {["    <td valign=\"", string:to_lower(VAlign), "\">\n"], "\n    </td>\n"};
rule([cell|_], {_, [Align, VAlign], _})   ->
    {["    <td align=\"", string:to_lower(Align), "\" valign=\"", string:to_lower(VAlign), "\">\n"],
     "\n    </td>\n"};

rule([tcaption|_], _)   ->
    {"  <caption align=\"bottom\"><em>", "</em></caption>\n"};

rule([codeinclude|_], {_, [File, Tag, _Type], _}) ->
%% Type can be "ERL", "C" or "NONE"
    {ok,Data} = docb_html_util:code_include(File, Tag),
    {drop, ["\n<div class=\"example\"><pre>\n", Data,
	     "\n</pre></div>\n"]};

rule([erleval|_], {_, [Expr], _}) ->
    docb_html_util:erl_eval(Expr);

rule([pcdata, pre|_], {_, _, Data}) ->
    %% Do not remove leading spaces.
    {drop, docb_html_util:pcdata_to_html(Data, false)};

rule([pcdata|_], {_, _, Data}) ->
    {drop, docb_html_util:pcdata_to_html(Data)}.

rule([seealso|_], {_, [Marker], _}, Opts) ->
    Href =
	case docb_util:html_snippet(seealso, Marker, Opts) of
	    "" ->
		%% DocBuilder default behavior:
		%% Marker is of format "Path#Fragment", both optional.
		%% Translated to <A HREF="Path.html#Fragment">
		case string:chr(Marker, $#) of
		    0 -> % No Fragment
			Marker++".html";
		    1 -> % No Path
			%% replace "/" with "-" because "/" xhtml does not
			%% allow "/" in the name attribute of element <a>
			%% so we have to do the same as for marker i.e
			%% Function/Arity is translated to an anchor in xhtml
			%% like this : <a name="Function-Arity"/>
			[case X of $/ -> $-;_->X end||X <- Marker]; 
		    _ ->
			Marker1 = [case X of $/ -> $-;_->X end||X <- Marker], 
			case string:tokens(Marker1, "#") of
			    [Path] -> % # at end, remove it
				Path++".html";
			    [Path | Frag0] ->
				Path++".html#"++
				    docb_util:join(Frag0, "#")
			end
		end;
	    Href0 ->
		%% User defined behavior, use result as-is
		Href0
	end,
    {{["<a href=\"", Href, "\">"], "</a>"}, Opts};

rule([warning|_], _, Opts) ->
    docb_html_util:copy_pics("warning.gif", "warning.gif", Opts),
    {{"\n<div class=\"warning\">\n"
      "<div class=\"label\">Warning</div>\n"
      "<div class=\"content\">\n",
      "\n</div>"
      "\n</div>\n"}, Opts};

rule([note|_], _, Opts) ->
    docb_html_util:copy_pics("note.gif", "note.gif", Opts),
    {{"\n<div class=\"note\">\n"
      "<div class=\"label\">Note</div>\n"
      "<div class=\"content\">",
      "\n</div>"
      "\n</div>\n"}, Opts};

rule([path|_], {_, [UNIX, Windows], [{pcdata, _, Text}]}, Opts) ->
    UnixPart =
	docb_util:an_option({ptype,"unix"}, Opts) and (UNIX/=""),
    WinPart =
	docb_util:an_option({ptype,"windows"}, Opts) and (Windows/=""),
    if
	UnixPart, WinPart ->
	    {{drop, [docb_html_util:pcdata_to_html(Text),
		     " <font size=\"-2\">(<code>UNIX: ", 
		     docb_html_util:attribute_cdata_to_html(UNIX),
		     ", ",
		     "Windows: ",
		     docb_html_util:attribute_cdata_to_html(Windows),
		     "</code>)</font>"]},
	     Opts};
	UnixPart ->
	    {{drop, [docb_html_util:pcdata_to_html(Text),
		     " <font size=\"-1\">(<code>UNIX: ",
		     docb_html_util:attribute_cdata_to_html(UNIX),
		     "</code>)</font>"]},
	     Opts};
	WinPart ->
	    {{drop, [docb_html_util:pcdata_to_html(Text),
		     " <font size=\"-1\">(<code>Windows: ",
		     docb_html_util:attribute_cdata_to_html(Windows),
		     "</code>)</font>"]},
	     Opts};
	true ->
	    {{drop, docb_html_util:pcdata_to_html(Text)}, Opts}
    end;

rule([term|_], {_, [ID], _}, Opts) ->
    case docb_util:an_option(dict, Opts) of
	false ->
	    case docb_util:lookup_option({defs, term}, Opts) of
		false ->
		    {{drop, ["<em><strong>",
			    ID,
			    "</strong></em> "]}, Opts};
		TermList ->
		    case lists:keysearch(ID, 1, TermList) of
			false ->
			    {{drop, ["<em><strong>", ID,
				    "</strong></em> "]},
			     Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<em><strong>", Name,
				     "</strong></em> "]},
			     Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, [ "<em><strong>", Name,
				      "</strong></em> "]},
			     Opts}
		    end
	    end;
	true ->
	    case docb_util:lookup_option({defs, term}, Opts) of
		false ->
		    {{drop, ["<em><strong>",  ID,
			     "</strong></em> "]}, Opts};
		TermList ->
		    PartApplication =
			docb_util:lookup_option(part_application, Opts),
		    case lists:keysearch(ID, 1, TermList) of
			false ->
			    {{drop, ["<a href=\"", PartApplication,
				    "_term.html#", ID, "\">", ID,
				    "</a> "]}, Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<a href=\"", PartApplication,
				    "_term.html#", ID, "\">", Name,
				    "</a> "]}, Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, ["<a href=\"", PartApplication,
				    "_term.html#", ID, "\">", Name,
				    "</a> "]}, Opts}
		    end
	    end
    end;

rule([cite|_], {_, [ID], _}, Opts) ->
    case docb_util:an_option(dict, Opts) of
	false ->
	    case docb_util:lookup_option({defs, cite}, Opts) of
		false ->
		    {{drop, ["<em><strong>", ID, "</strong></em> "]},
		     Opts};
		CiteList ->
		    case lists:keysearch(ID, 1, CiteList) of
			false ->
			    {{drop,
			      ["<em><strong>", ID, "</strong></em> "]},
			     Opts};
	
		{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<em><strong>", Name,
				     "</strong></em> "]},
			     Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, ["<em><strong>", Name,
				     "</strong></em> "]},
			     Opts}
		    end
	    end;
	true ->
	    case docb_util:lookup_option({defs, cite}, Opts) of
		false ->
		    {{drop, ["<em><strong>", ID, "</strong></em> "]},
		     Opts};
		CiteList ->
		    PartApp =
			docb_util:lookup_option(part_application, Opts),
		    case lists:keysearch(ID, 1, CiteList) of
			false ->
			    {{drop, ["<a href=\"", PartApp,
				     "_cite.html#", ID, "\">", ID,
				     "</a> "]},
			     Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<a href=\"", PartApp,
				    "_cite.html#", ID, "\">", Name,
				     "</a> "]},
			     Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, ["<a href=\"", PartApp,
				    "_cite.html#", ID, "\">", Name,
				     "</a> "]},
			     Opts}
		    end
	    end
    end;

rule([code|_], {_, [Type], [{pcdata, _, Code}]}, Opts) ->
    case lists:member(Type,["ERL","C","NONE"]) of
	true ->
	    {{drop, ["\n<div class=\"example\"><pre>\n", docb_html_util:element_cdata_to_html(Code),
		     "\n</pre></div>\n"]}, Opts};
	false ->
	    exit({error,"unknown type of <code>"})
    end.

reorder_table(TableContent) ->
    reorder_table(TableContent, [], []).
reorder_table([], Caption, NewTableContent) ->
    Caption ++ lists:reverse(NewTableContent);
reorder_table([{tcaption,_,_} = Caption | TableContent], _, NewTableContent) ->
    reorder_table(TableContent, [Caption], NewTableContent);
reorder_table([Row | TableContent], Caption, NewTableContent) ->
    reorder_table(TableContent, Caption, [Row | NewTableContent]).
