%%%----------------------------------------------------------------------
%%% File         : sdocbook2xhtml.erl
%%% Description  : Erlang XSLT like "stylesheet" for exporting 
%%%                Simplified Docbook XML to XHTML.
%%% 
%%% Modules used : lists, io_lib, xmerl, xmerl_lib, xmerl_xs
%%% 
%%%----------------------------------------------------------------------

-module(sdocbook2xhtml).
-author('mikael.karlsson@creado.com').


-include("xmerl.hrl").
-import(xmerl_lib, [markup/3,mapxml/2, foldxml/3, mapfoldxml/3]).
-import(xmerl_xs, [ xslapply/2, value_of/1, select/2, built_in_rules/2]).

-export([ process_xml/1 ]).

-export([abbrev/4,
	 abstract/4,
	 acronym/4,
	 address/4,
	 anchor/4,
	 appendix/4,
	 appendixinfo/4,
	 article/4,
	 articleinfo/4,
	 audiodata/4,
	 audioobject/4,
	 author/4,
	 authorgroup/4,
	 authorinitials/4,
	 bibliography/4,
	 bibliomixed/4,
	 bibliomisc/4,
	 bibliomset/4,
	 biblioset/4,
	 blockquote/4,
	 caption/4,
	 citetitle/4,
	 city/4,
	 colspec/4,
	 command/4,
	 computeroutput/4,
	 copyright/4,
	 corpauthor/4,
	 country/4,
	 date/4,
	 edition/4,
	 editor/4,
	 email/4,
	 emphasis/4,
	 entry/4,
	 example/4,
	 fax/4,
	 figure/4,
	 filename/4,
	 firstname/4,
	 footnote/4,
	 holder/4,
	 honorific/4,
	 imagedata/4,
	 imageobject/4,
	 informaltable/4,
	 inlinemediaobject/4,
	 isbn/4,
	 issn/4,
	 issuenum/4,
	 legalnotice/4,
	 lineage/4,
	 link/4,
	 literal/4,
	 itemizedlist/4,
	 listitem/4,
	 mediaobject/4,
	 member/4,
	 note/4,
	 orderedlist/4,
	 othercredit/4,
	 othername/4,
	 para/4,
	 phone/4,
	 phrase/4,
	 programlisting/4,
	 publishername/4,
	 quote/4,
	 replaceable/4,
	 revhistory/4,
	 revision/4,
	 revnumber/4,
	 revremark/4,
	 row/4,
	 section/4,
	 sectioninfo/4,
	 simplelist/4,
	 subtitle/4,
	 surname/4,
	 systemitem/4,
	 table/4,
	 tbody/4,
	 term/4,
	 tfoot/4,
	 tgroup/4,
	 thead/4,
	 title/4,
	 titleabbrev/4,
	 trademark/4,
	 ulink/4,
	 userinput/4,
	 variablelist/4,
	 varlistentry/4,
	 xref/4,
	 year/4
	 ]).


xmlhead() -> "<\?xml version=\"1.0\" encoding=\"iso-8859-1\"\?>".
doctype() ->"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\
 \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd \">\n".

style() ->
    "<style type=\"text/css\"> body {margin-left:10%; margin-right:5%;} \
.logo{float:right;} 
.toc UL {
    list-style-type:     none;
    border:              solid;
    border-width:        thin;
    padding-left:        10px;
    padding-right:       10px;
    padding-top:         5px;
    padding-bottom:      5px;
    background:          #f0f0f0;
    letter-spacing:      2px;
    line-height:         20px;
}
</style>".

process_xml(E)->
%%    lists:flatten(template( E )).
    template( E ).

%% article is the root element
template(E0 = #xmlElement{name=article})->

    E = changetitle(E0), %% Add section numbering to titles

    [ xmlhead(), doctype(),
      "<html xmlns=\"http://www.w3.org/1999/xhtml\" >"
      "<head>"
      "<title>",
      value_of(select("articleinfo/title",E)),
      "</title>",
      style(),
      "</head>"
     "<body>",
%%  "<img src=\"/logo.png\" alt=\"no logo\" class=\"logo\" </img>",
     xslapply( fun template/1, select("articleinfo",E)),
     process_toc(E), %% Insert toc between info and main part of article
     xslapply( fun template/1, select("section",E)),
     xslapply( fun template/1, select("appendix",E)),
     "</body></html>"];

template(E = #xmlElement{name=Name})->
    A = xslapply( fun template/1, E),
    case catch
	sdocbook2xhtml:Name(A, E#xmlElement.attributes, E#xmlElement.parents,E)
	of
	{'EXIT', {undef, _}} ->
	    A;
	{'EXIT', Reason} ->
	    exit(Reason);
	Res ->
	    Res
    end;

template(E) ->
    built_in_rules( fun template/1, E).

%% -------------------------------------------------------------------
%% simple serialize tags

abbrev(Data, Attrs, [{bibliomset,_}|_], E)->
    ["<dt><abbr>", Data, "</abbr></dt><dd>"];
abbrev(Data, Attrs, Parents, E)->
    markup("abbr",Attrs, Data).


abstract(Data, Attrs, Parents, E)->
    ["<h3>Abstract</h3><blockquote>", Data, "</blockquote>"].

acronym(Data, Attrs, Parents, E)->
    markup("acronym",Attrs, Data).

address(Data, Attrs, Parents, E)->
    markup("address", Attrs, Data).
				     
anchor(Data, Attrs, Parents, E)->
    case find_attribute(id, Attrs) of
	{value,ID} -> 
	    ["<a name=\"" ++ ID ++ "\">", Data, "</a>"];
	false ->
	    Data
    end.

appendix(Data, Attrs, Parents, E)->
    ["<h1>Appendix</h1>", Data].

appendixinfo(Data,_,_,_)->
    Data.

article(Data, Attrs, Parents, E)->
    ["<body>"
     "<img src=\"/logga2.jpg\" alt=\"no logo\" class=\"logo\" 
width=\"50\"</img>",
     Data, 
     "</body>"].

articleinfo(Data,_,_,_)->
    Data.

audiodata(Data, Attrs, Parents, E)->Data.

audioobject(_,_,_,_)->
    [].

author(Data, Attrs, [{authorgroup,_} | _], E)->
    markup("dd", Attrs, Data);
author(Data, Attrs, Parents, E)->
    Data.

authorgroup(Data,_,_,_)-> 
    ["<dl><dt>Author</dt>",Data,"</dl>"].

authorinitials(Data,_,_,_)-> Data.

bibliography(Data, Attrs, Parents, E)->
    ["<h3>Bibliography</h3>" ,Data].

bibliomisc(Data,_,_,_)-> Data.
bibliomixed(Data,_,_,_)-> ["<dl>",Data, "</dl>"].
bibliomset(Data,_,_,_)-> [Data, "</dd>"].

biblioset(Data,_,_,_)-> Data.

blockquote(Data, Attrs, Parents, E)->
    markup("blockquote",Attrs, Data).

caption(Data, Attrs, Parents, E)-> Data.

citetitle(Data,_,_,_)-> ["<i>",Data,"</i>"].

city(Data,_,_,_)->
	    Data.

%% Fix Me is it "col" element in html?
colspec(_, Attrs,_,_)->
    [].

command(Data,_,_,_)->
    ["<b><tt>", Data, "</tt></b>"].

computeroutput(Data,_,_,_)->
    ["<tt>", Data, "</tt>"].

copyright(Data,_,_,_)->
    [ "&copy; ", Data].

corpauthor(Data,_,_,_)->
    Data.

country(Data,_,_,_)->
    Data.

date(Data,_,[{revision,_}|_],_)->
    ["<td>", Data, "</td>"];
date(Data,_,_,_)->
    Data.

edition(Data,_,_,_)->
    Data.

editor(Data,_,_,_)->
    Data.

email(Data,_,_,_)->
    ["<i><a href=\"mailto:", Data,"\">",Data,"</a></i>"].

emphasis(Data, Attrs, Parents, E)->
    ["<em>", Data, "</em>"].

%% Cell in a table
entry(Data, Attrs, [{row,_}, {thead,_} | _], E)->
    ["<th>", Data, "</th>"];
entry(Data, Attrs, Parents, E)->
    ["<td>", Data, "</td>"].

example(Data, Attrs, Parents, E)->
    ["<hr />", Data, "<hr />"].

fax(Data, Attrs, Parents, E)->
    ["<address>", Data, "</address>"].

%% May contain ulink to image, resolved by ulink type
figure(Data, _, _, _)->
    Data.

filename(Data, _, _, _)->
    ["<i>", Data, "</i>"].

firstname(Data, _, _, _)->
    [Data , " " ].

footnote(Data, _, _, _)->
    Data.

holder(Data, _, _, _)->
    [" ",Data].

honorific(Data, _, _, _)->
    Data.

imagedata(Data, Attrs, Parents, E)->
    SRC =
	case find_attribute(fileref, Attrs) of
	    {value,AS} -> 
		" src=" ++  AS ++ " ";
	    false ->
		[]
	end,
    ALT = 
	case SRC of
	    [] ->
		" alt=\"No image!\" ";
	    _ ->" alt=\"" ++ SRC ++ "\" "
	end,
    WIDTH = 
	case find_attribute(width, Attrs) of
	    false ->
		 [];
	    {value,A} ->" width=" ++ A ++ " "
	end,

    ["<img " ++ SRC ++ ALT ++ WIDTH ++ "></img>"].


imageobject(Data, Attrs, Parents, E)->
    Data.

informaltable(Data, Attrs, Parents, E)->
    ["<table border=\"1\" >", Data, "</table>"].


inlinemediaobject(Data, Attrs, Parents, E)->
    Data.

isbn(Data, Attrs, Parents, E)->
    Data.

issn(Data, Attrs, Parents, E)->
    Data.

issuenum(Data, Attrs, Parents, E)->
    Data.

itemizedlist(Data, Attrs, Parents, _)->
    markup("ul", Attrs, Data).

%keyword
%{
%  display: inline;
%}

%keywordset
%{
%  display: inline;
%}

legalnotice(Data, Attrs, Parents, _)->
    markup("small", Attrs, Data).

lineage(Data, Attrs, Parents, _)->
    Data.

%lineannotation
%{
%  display: inline;
%}

% Hypertext link
link(Data, Attrs, Parents, _)->
    case find_attribute(linkend, Attrs) of
	{value,LINK} -> 
	    ["<a href=\"#" ++ LINK ++ "\">", Data, "</a>"];
	false ->
	    Data
    end.
    
listitem(Data, Attrs, [{varlistentry,_} | _], E) ->
    markup("dd", Attrs, Data);
listitem(Data, Attrs, Parents, _)->
    markup("li", Attrs, Data).

literal(Data, Attrs, Parents, _)->
    markup("tt", Attrs, Data).

%literallayout
%{
%  display: inline;
%}

mediaobject(Data, Attrs, Parents, _)->
    Data.

%% simplelist member
member(Data, Attrs, Parents, _)->
    [Data,"<br></br>"].

note(Data, Attrs, Parents, _)->
["<table border=\"1\" cellspacing=\"0\" cellpadding=\"5\" width=\"80%\"
bgcolor=\"#CCCCCC\"><tr><td>NOTE</td><td><i>", Data, "</i></td></tr></table>"].
    
%objectinfo
%{
%  display: inline;
%}

%option
%{
%  display: inline;
%}

orderedlist(Data, Attrs, Parents, _)->
    markup("ol",Attrs,Data).
 
%% Hmm otheraddr not in DTD   
%otheraddr
%{
%  display: inline;
%}

othercredit(Data, Attrs, Parents, _)->Data.

othername(Data, Attrs, Parents, E)->Data.

%% IGNORE
%pagenums
%{
%  display: inline;
%}

para(Data, Attrs, [{listitem,_}|_], E)->
    Data;
para(Data, Attrs, [{note,_}|_], E)->
    Data;
para(Data, Attrs, Parents, E)->
    markup("p", Attrs, Data).

phone(Data, Attrs, Parents, E)->Data.

phrase(Data, Attrs, Parents, E)->Data.

%pob
%{
%  display: inline;
%}

%postcode
%{
%  display: inline;
%}

%printhistory
%{
%  display: inline;
%}

%procedure
%{
%  display: inline;
%}

programlisting(Data, Attrs, Parents, E)->
["<table border=\"1\" cellspacing=\"0\" cellpadding=\"5\" width=\"100%\"
bgcolor=\"#CCCCCC\"><tr><td><pre><code>", Data, 
"</code></pre></td></tr></table>"].

%pubdate
%{
%  display: inline;
%}

%publisher
%{
%  display: inline;
%}

publishername(Data, Attrs, Parents, E)->
    Data.

quote(Data, Attrs, Parents, _)->
    markup("q", Attrs, Data).

replaceable(Data, Attrs, Parents,_)->
    markup("i", Attrs, Data).

revhistory(Data, Attrs, Parents,E)->
      {A,B,C} = case E#xmlElement.language of
	      "en" -> {"Revision history","Date","Comment"};
	      "sv" -> {"Revisionshistoria","Datum","Kommentar"};
	      _ ->{"lang is undefined","lang is undefined","lang is undefined"}
	  end,

    ["<h4>",A,"</h4>","<table><thead>"
    "<tr><th>Rev.</th><th>",B,"</th><th>",C,"</th></tr></thead>", Data,
    "</table>"].

revision(Data, Attrs, Parents,_)->
    markup("tr", Attrs, Data).

revnumber(Data, Attrs, Parents,_)->
    markup("td", Attrs, Data).

revremark(Data, Attrs, Parents,_)->
    markup("td", Attrs, Data).

row(Data, Attrs, Parents, E)->
    markup("tr", Attrs, Data).

section(Data, Attrs, Parents, E)->
    Data.

sectioninfo(Data, Attrs, Parents, E)->Data.

%sidebar
%{
%  display: block;
%}

simplelist(Data, Attrs, Parents, E)->
    ["<table border=\"1\" cellspacing=\"0\" cellpadding=\"5\" 
width=\"100%\"><tr><td>", Data, "</td></tr></table>"].


%state
%{
%  display: inline;
%}

%step
%{
%  display: inline;
%}

%street
%{
%  display: inline;
%}

%substeps
%{
%  display: inline;
%}

subtitle(Data, Attrs, Parents, E)->
    ["<h3>", Data, "</h3>"].

surname(Data, Attrs, Parents, E)->Data.

systemitem(Data, Attrs, Parents, E)->
    markup("b", Attrs, Data).

table(Data, Attrs, Parents, E)->
    ["<table border=\"1\" cellspacing=\"0\" cellpadding=\"4\" >", 
     Data, "</table>"].

%% Fix me alot
tbody(Data, Attrs, Parents, E)->
    markup("tbody", Attrs, Data).
%{
%  display: table-row-group;
%}

term(Data, Attrs, [{varlistentry,_} | _], E) ->
    markup("dt", Attrs, Data).


%textobject
%{
%  display: inline;
%}

tfoot(Data, Attrs, Parents, E)->
    markup("tfoot",Attrs, Data).

%% Fixme alot
tgroup(Data, Attrs, Parents, E)->
    markup("colgroup", Attrs, Data).
%{
%  display: table;
%}

thead(Data, Attrs, Parents, E)->
    markup("thead",Attrs, Data).
%{
%  display: table-row-group;
%}

title(Data, Attrs, Parents, E)->
%%    io:fwrite("Parents ~p~n", [Parents]),
    title1(Data, Attrs, Parents, E).

title1(Data, Attrs, [{section,_}, {section,_}, {section,_}, 
		    {section,_}, {section,_}, {appendix,_} | _], E) ->
    ["<h6>", Data, "</h6>"];
title1(Data, Attrs, [{section,_}, {section,_}, {section,_}, 
		    {section,_}, {appendix,_} | _], E) ->
    ["<h5>", Data, "</h5>"];
title1(Data, Attrs, [{section,_}, {section,_}, {section,_}, 
		    {apendix,_} | _], E) ->
    ["<h4>", Data, "</h4>"];
title1(Data, Attrs, [{section,_}, {section,_}, {appendix,_} | _], E) ->
    ["<h3>", Data, "</h3>"];
title1(Data, Attrs, [{section,_}, {appendix,_} | _], E) ->
    ["<h2>", Data,  "</h2>"];
title1(Data, Attrs, [{appendix,_} | _], E) ->
    ["<h1>", Data, "</h1>"];

title1(Data, Attrs, [{section,_}, {section,_}, {section,_}, 
		    {section,_}, {section,_}, {section,_} | _], E) ->
    ["<h6>", Data, "</h6>"];
title1(Data, Attrs, [{section,_}, {section,_}, {section,_}, 
		    {section,_}, {section,_} | _], E) ->
    ["<h6>", Data, "</h6>"];
title1(Data, Attrs, [{section,_}, {section,_}, {section,_}, 
		    {section,_} | _], E) ->
    ["<h5>", Data, "</h5>"];
title1(Data, Attrs, [{section,C}, {section,B}, {section,A} | _], E) ->
    {value, Id} = find_attribute(id,Attrs),
    ["<h4 id=\"", Id, "\">", Data, "</h4>"];
title1(Data, Attrs, [{section,B}, {section,A} | _], E) ->
    {value, Id} = find_attribute(id,Attrs),
    ["<h3 id=\"", Id, "\">", Data,  "</h3>"];
title1(Data, Attrs, [{section,A} | _], E) ->
    {value, Id} = find_attribute(id,Attrs),
    ["<h2 id=\"", Id, "\">", Data, "</h2>"];
title1(Data, Attrs, [{articleinfo,_} | _], E) ->
    ["<h1>", Data, "</h1>"];
title1(Data, Attrs, [{table,_} | _], E) ->
    ["<caption>", Data, "</caption>"];
title1(Data, Attrs, [{bibliomset,_} | _], E) ->
    ["<i><b>", Data, "</b></i>"];
title1(Data, Attrs, Parents, E)->
    ["<h4>", Data, "</h4>"].

titleabbrev(Data, Attrs, Parents, E)->[].

trademark(Data, Attrs, Parents, E)->
    [ Data, " &reg; "].

ulink(Data, Attrs, Parents, E)->
    case find_attribute(url, Attrs) of
	{value,LINK} -> 
	    ["<a href="++ LINK ++ ">", Data, "</a>"];
	false ->
	    Data
    end.


%% User input is Constant Bold
userinput(Data, Attrs, Parents, E)->
    ["<tt><b>", Data, "</b></tt>"].

variablelist(Data, Attrs, Parents, E)->
    markup("dl", Attrs, Data).

varlistentry(Data, Attrs, Parents, E)->Data.

%videodata
%{
%  display: inline;
%}

%videoobject
%{
%  display: inline;
%}

%volumenum
%{
%  display: inline;
%}

xref(Data, Attrs, Parents, E)->
    case find_attribute(linkend, Attrs) of
	{value,LINK} -> 
	    ["<a href=\"#" ++ LINK ++ "\" />"];
	false ->
	    Data
    end.

year(Data, Attrs, Parents, E)->Data.

%% ----------------------------------------------------------
%% Utils find_attribute copied from Ulf Wigers xmerl distribution

find_attribute(Name, Attrs) ->
    case lists:keysearch(Name, #xmlAttribute.name, Attrs) of
	{value, #xmlAttribute{value = V}} ->
	    {value, V};
	false ->
	    false
    end.
%% ------------

changetitle(A) ->
    Afun = fun changecount/2,
    {E, Acc} = mapfoldxml(Afun, {0,0,0,0,0,0}, A),
    E.

changecount(#xmlElement{name=title}=E, {A,B,C,Ex,Fig,Tab})->
    case E#xmlElement.parents of
	[{example,_} |_] -> 
	    {addexhead(E,{A,Ex+1}), {A,B,C,Ex+1,Fig,Tab} };
	[{figure,_} |_] -> 
	    {addfighead(E,{A,Fig+1}), {A,B,C,Ex,Fig+1,Tab} };
	[{table,_} |_] -> 
	    {addtablehead(E,{A,Tab+1}), {A,B,C,Ex,Fig,Tab+1} };
	[{section,_},{section,_},{section,_},{article,_} |_] -> 
	    {addheader(E,{A,B,C+1}), {A,B,C+1,Ex,Fig,Tab} }; 
	[{section,_},{section,_},{article,_} |_] -> 
	    { addheader(E,{A,B+1,0}), {A,B+1,0,Ex,Fig,Tab} }; 
	[{section,_},{article,_} |_] -> 
	    {addheader(E,{A+1,0,0}),{A+1,0,0,0,0,0}}; 
	_ -> 
	    {E,{A,B,C,Ex,Fig,Tab}}
    end;
changecount(E, Acc)->{E,Acc}.

addexhead(#xmlElement{name=title,content=[#xmlText{}=T1|_]}= E, {Ch,No})->
    NewHeader = "Example " ++ 
	integer_to_list(Ch)++" - "++ integer_to_list(No) ++ 
	" " ++ T1#xmlText.value,
    E#xmlElement{content=[T1#xmlText{value=NewHeader}]}.
addfighead(#xmlElement{name=title,content=[#xmlText{}=T1|_]}= E, {Ch,No})->
    NewHeader = "Figure " ++ 
	integer_to_list(Ch)++" - "++ integer_to_list(No) ++ 
	" " ++ T1#xmlText.value,
    E#xmlElement{content=[T1#xmlText{value=NewHeader}]}.
addtablehead(#xmlElement{name=title,content=[#xmlText{}=T1|_]}= E, {Ch,No})->
    NewHeader = "Table " ++ 
	integer_to_list(Ch)++" - "++ integer_to_list(No) ++ 
	" " ++ T1#xmlText.value,
    E#xmlElement{content=[T1#xmlText{value=NewHeader}]}.

addheader(#xmlElement{name=title,content=[#xmlText{}=T1|_]}= E, Chapters)->
    NewHeader = chapterstring(Chapters)++ " " ++ T1#xmlText.value,
    NewAtts = addid(E#xmlElement.attributes, Chapters),
    E#xmlElement{content=[T1#xmlText{value=NewHeader}],
		 attributes = NewAtts}.

chapterstring({A,0,0})->integer_to_list(A);
chapterstring({A,B,0})->integer_to_list(A)++"."++ integer_to_list(B);
chapterstring({A,B,C})->integer_to_list(A) ++ "." ++
			    integer_to_list(B) ++ "." ++
			    integer_to_list(C).

%% addid add id attribute if it not already exists
addid(OldAtts, Chapters)->
    case find_attribute(id, OldAtts) of
	{value,_} ->
	    OldAtts;
	false ->
	    add_attribute(id,"sect_"++ chapterstring(Chapters),
			  OldAtts)
    end.

add_attribute(Name, Value, OldAtts)->
    [#xmlAttribute{ name=Name, value = Value}| OldAtts ].


process_toc(E)->
    AFun = fun chapindex/2,
    TOCR = foldxml(AFun, [], E),
%    Str = case find_attribute(lang, E#xmlElement.attributes) of
%	      {value,"en"} -> "Table of Contents";
%	      {value,"sv"} -> "Innehållsförtecking";
%	      _ ->"lang is undefined"
%	  end,
    Str = case E#xmlElement.language of
	      "en" -> "Table of Contents";
	      "sv" -> "Innehållsförtecking";
	      _ ->"lang is undefined"
	  end,
    TOC = ["<div class=\"toc\"><h3>",Str,"</h3><ul>", 
lists:reverse(TOCR), "</ul></div>"].

chapindex(#xmlElement{name=title}=E, Accu)->
    case E#xmlElement.parents of
	[{section,_},{section,_},{section,_},{article,_} |_] -> 
	    ["<li>"++spind(3)++ addlink(E,"toc_level_3") ++"</li>"| Accu]; 
	[{section,_},{section,_},{article,_} |_] -> 
	    ["<li>"++spind(2)++ addlink(E,"toc_level_2") ++ "</li>"| Accu]; 
	[{section,_},{article,_} |_] -> 
	    ["<li>"++spind(1)++ addlink(E,"toc_level_1") ++"</li>"| Accu]; 
	_ -> 
	    Accu
    end;
chapindex(E, Accu) ->
    Accu.

spind(0) ->"";
spind(X)->
    "&#160;&#160;" ++ spind(X-1).

addlink(E, TocLevel)->
    {value,LINK} = find_attribute(id,E#xmlElement.attributes),
    [#xmlText{value=Title}|_] = E#xmlElement.content, %% Pfuii
    "<a href=\"#" ++ LINK ++ 
	"\" class=\"" ++ TocLevel ++ "\">" ++ 
    Title ++ "</a>".


