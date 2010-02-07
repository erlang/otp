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
-module(docb_xmerl_tree_cb).

%% This is the XMerL callback module for exporting XML to the internal
%% tree format used by DocBuilder.
%%   {Doc, _Misc} = xmerl_scan:file("file.xml", [{validation,true}])
%%   Tree = xmerl:export([Doc], docb_xmerl_tree_cb)

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#text#'/1,
	 '#element#'/5]).
-include("xmerl.hrl").

%%--Functions used by xmerl---------------------------------------------

'#xml-inheritance#'() ->
    [].

'#root#'(Data, _Attrs, [], _E) ->
    Data.

'#text#'(Text) ->
    Text2 = strip_leading_blanks(Text),
%% before
%%    case Text2 of
%%	[$\n|T] ->
%%	    case is_empty(T) of 
%%		true -> [];
%%		false -> {pcdata, [], nl(Text2)}
%%	    end;
%%
%%	_ ->
%%	    {pcdata, [], nl(Text2)}
%%    end.
%% after
    {pcdata, [], nl(Text2)}.

'#element#'(Tag, Data, Attrs, Parents, _E) when Tag==pre; Tag==code ->
    [H|T] = reinsert_nl(Data),
    NewData = [strip_nl(H)|T],
    NewData2 = case Tag of
		   code ->
		       fix_single_pcdata(NewData);
		   pre ->
		       NewData
	       end,
    {Tag, attrs(get_dtd(Parents), Tag, Attrs), NewData2};
'#element#'(Tag, Data, Attrs, Parents, _E) ->
    NewData = case tag_content(Tag) of
		  no_pcdata -> % remove all pcdata
		      [Dat|| 
			  Dat <- Data, 
			  begin 
			      Fun = fun({pcdata,_,_}) -> false;
				       (_) -> true end,
			      Fun(Dat)
			  end];
		  single_pcdata when length(Data)>1 -> 
		      %% merge several pcdata's into one single pcdata
		      fix_single_pcdata(Data);
		  _ ->
		      lists:flatten(Data)
	      end,
    {Tag, attrs(get_dtd(Parents), Tag, Attrs), NewData}. 

%%--Internal functions--------------------------------------------------

%% is_empty(Str) -> bool()
%% Returns true if the string Str only contains blanks, tabs and
%% newlines, false otherwise.
%% is_empty("\n" ++ Text) ->
%%     is_empty(Text);
%% is_empty("\t" ++ Text) ->
%%     is_empty(Text);
%% is_empty(" " ++ Text) ->
%%     is_empty(Text);
%% is_empty("") ->
%%     true;
%% is_empty(_) ->
%%     false.

%% reinsert_nl(L1) -> L2
%% Workaround for <pre>: Normally empty lines are ignored. However,
%% Xmerl splits lines whenever it encounters an entity. In the case of
%% <pre>, this may lead to that we ignores what we think is an empty
%% line but is actually a line break that should be kept, for example
%% in this case:
%% <pre>
%%   <input>some command</input> <-- this line break is lost!
%%   &lt;some result&gt;
%% </pre>
%% This function reinserts line breaks where necessary.
reinsert_nl([[]|T]) ->
    [{pcdata,[],"\\n"} | reinsert_nl(T)];
reinsert_nl([H|T]) ->
    [H | reinsert_nl(T)];
reinsert_nl([]) ->
    [].

%% sgmls treats line breaks in a way that DocBuilder relies on and
%% which must be imitated here. Replace all "\n" with "\\n" and add
%% "\n" to the end of each text element.
nl("") ->
    "\n";
nl("\n"++Text) ->
    "\\n"++nl(Text);
nl([Ch|Text]) ->
    [Ch|nl(Text)].


%% strip_leading_blanks(Str) -> Str
%% Leading spaces and tabs before a newline are always redundant
%% and are therefore stripped of here
%% If no newline is found the original string is returned unchanged

strip_leading_blanks(Str) ->
    strip_leading_blanks(Str,Str).

strip_leading_blanks([],Str) ->
    Str;
strip_leading_blanks([$\s|T],Str) ->
    strip_leading_blanks(T,Str);
strip_leading_blanks([$\t|T],Str) ->
    strip_leading_blanks(T,Str);
strip_leading_blanks(Rest=[$\n|_],_) ->
    Rest;
strip_leading_blanks(_,Str) ->
    Str.

%% strip_nl(Str) -> Str
%% The XMerL scan will often result in the contents of <pre> or <code>
%% starting with a newline, as the format is normally:
%%   <pre>
%%     ..contents..
%%   </pre>
%% However, this newline must be removed, or the resulting HTML will be
%%   <pre>
%%
%%     ..content..
%%   </pre>
strip_nl({pcdata,[],"\\n"++Str}) -> {pcdata,[],Str};
strip_nl(E) -> E.

get_dtd([]) ->
    none;
get_dtd(Parents) ->
    {DTD, _} = lists:last(Parents),
    DTD.

%% attrs(DTD, Tag, GivenAttrs) -> AllAttrs
%%   DTD = Tag = atom()  DTD and tag name
%%   GivenAttrs = [#xmlAttribute{}]
%%   AllAttrs = [{Name, Type, Val}]
%%     Name = string()  (uppercase) Example: "VALIGN"
%%     Type = "CDATA" | "TOKEN"
%%     Val  = string()  (uppercase if type is "TOKEN", as-is otherwise)
%% The XMerL scanning of <file>.xml renders only the given attributes.
%% However, DocBuilder needs also the optional attributes (which not
%% necessarily have been given), so we add them here, using the default
%% values according to the DTDs.
%% NOTE: Uses the information from the DTDs. That is, if some change is
%% done to the DTDs, also this file must be updated. Ideally, the DTDs
%% should be parsed automatically in some way.
%% It can also be noted that this check is superfluous in the case where
%% all attributes are required (except that the attributes are sorted
%% in the same order as in the DTD) and where an optional attribute has
%% type "CDATA" as no sensible default value can be specified in this
%% case.
attrs(DTD, Tag, GivenAttrs) ->
    merge_attrs(Tag, default_attrs(DTD, Tag), GivenAttrs).

merge_attrs(Tag, [{NameA, Type, DefVal}|Default], GivenAttrs) ->
    Val = case lists:keyfind(NameA, #xmlAttribute.name, GivenAttrs) of
	      #xmlAttribute{value=Val0} -> Val0;
	      false -> DefVal
	  end,
    Attr = {attr_name(NameA), Type, attr_val(Type, Val)},
    [Attr | merge_attrs(Tag, Default, GivenAttrs)];
merge_attrs(_Tag, [], _GivenAttrs) ->
    [].

attr_name(Atom) ->
    string:to_upper(atom_to_list(Atom)).

attr_val("CDATA", Val) -> Val;
attr_val("TOKEN", Val) -> string:to_upper(Val).

%% Given the DTD and element tag, return a list [{Name, Value}] where
%% Name (atom) is the name of each possible attribute and
%% Value (lowercase string) its default value.
default_attrs(_, cell) ->
    [{align, "TOKEN", "left"},
     {valign, "TOKEN", "middle"}];
default_attrs(_, cite) ->
    [{id, "CDATA", ""}]; % required
default_attrs(_, code) ->
    [{type, "TOKEN", "none"}];
default_attrs(_, codeinclude) ->
    [{file, "CDATA", ""}, % required
     {tag, "CDATA", ""},
     {type, "TOKEN", "none"}];
default_attrs(book, contents) ->
    [{level, "TOKEN", "2"}];
default_attrs(_, erleval) ->
    [{expr, "CDATA", ""}]; % required
default_attrs(report, erlinclude) ->
    [{file, "CDATA", ""}, % required
     {tag, "CDATA", ""}]; % required
default_attrs(_, fascicule) ->
    [{file, "CDATA", ""}, % required
     {href, "CDATA", ""}, % required
     {entry, "TOKEN", "no"}];
default_attrs(book, header) ->
    [{titlestyle, "TOKEN", "normal"}];
default_attrs(_, image) ->
    [{file, "CDATA", ""}]; % required
default_attrs(_, include) ->
    [{file, "CDATA", ""}]; % required
default_attrs(report, index) ->
    [{txt, "CDATA", ""}]; % required
default_attrs(_, list) ->
    [{type, "TOKEN", "bulleted"}];
default_attrs(_, marker) ->
    [{id, "CDATA", ""}]; % required
default_attrs(book, onepart) ->
    [{lift, "TOKEN", "no"}];
default_attrs(book, parts) ->
    [{lift, "TOKEN", "no"}];
default_attrs(_, path) ->
    [{unix, "CDATA", ""},
     {windows, "CDATA", ""}];
default_attrs(_, seealso) ->
    [{marker, "CDATA", ""}]; % required
default_attrs(report, table) ->
    [{width, "CDATA", "0"},
     {colspec, "CDATA", ""}];
default_attrs(_, table) ->
    [{align, "TOKEN", "center"}];
default_attrs(_, term) ->
    [{id, "CDATA", ""}]; % required
default_attrs(book, theheader) ->
    [{tag, "TOKEN", "none"}];
default_attrs(bookinsidecover, theheader) ->
    [{tag, "TOKEN", "none"}];
default_attrs(_, url) ->
    [{href, "CDATA", ""}]; % required
default_attrs(_, _) -> [].

%%--Single PCDATA broken into several fix-------------------------------

%% When text contains an entity, then XMERL splits it into two
%% PCDATA elements, the second starting with the entity.
%%
%% Example:
%%  Magnus Fröberg => [{pcdata,[],"Magnus Fr\n"},{pcdata,[],"öberg\n"}]
%%
%% This is not handled by DocBuilder which expects many tags, for
%% example title and aname, to contain a single PCDATA element. (That
%% is also what nsgmls returned.)

fix_single_pcdata([{pcdata,[],Str1}, {pcdata,[],Str2}|T]) ->
    fix_single_pcdata([{pcdata,[],Str1++Str2}|T]);
fix_single_pcdata(FixedData) ->
    FixedData.

tag_content(aname) -> single_pcdata;
tag_content(app) -> single_pcdata;
tag_content(approved) -> single_pcdata;
tag_content(appsummary) -> single_pcdata;
tag_content(b) -> single_pcdata;
tag_content(c) -> single_pcdata;
tag_content(cauthor) -> single_pcdata;
tag_content(cell) -> mixed_content;
tag_content(checked) -> single_pcdata;
tag_content(chowpublished) -> single_pcdata;
tag_content(code) -> single_pcdata; % mixed?
tag_content(com) -> single_pcdata;
tag_content(comsummary) -> single_pcdata;
tag_content(copyright) -> mixed_content;
tag_content(ctitle) -> single_pcdata;
tag_content(d) -> mixed_content;
tag_content(date) -> single_pcdata;
tag_content(docno) -> single_pcdata;
tag_content(em) -> mixed_content;
tag_content(email) -> single_pcdata;
tag_content(fascicule) -> single_pcdata;
tag_content(file) -> single_pcdata;
tag_content(filesummary) -> single_pcdata;
tag_content(fsummary) -> mixed_content;
tag_content(headline) -> single_pcdata;
tag_content(holder) -> single_pcdata;
tag_content(i) -> single_pcdata;
tag_content(icaption) -> single_pcdata;
tag_content(id) -> single_pcdata;
tag_content(input) -> mixed_content;
tag_content(item) -> mixed_content;
tag_content(legalnotice) -> single_pcdata;
tag_content(lib) -> single_pcdata;
tag_content(libsummary) -> single_pcdata;
tag_content(module) -> single_pcdata;
tag_content(modulesummary) -> single_pcdata;
tag_content(name) -> single_pcdata;
tag_content(nametext) -> single_pcdata;
tag_content(p) -> mixed_content;
tag_content(pagetext) -> single_pcdata;
tag_content(path) -> single_pcdata; % mixed?
tag_content(pre) -> mixed_content;
tag_content(prepared) -> single_pcdata;
tag_content(resp) -> single_pcdata;
tag_content(responsible) -> single_pcdata;
tag_content(ret) -> single_pcdata;
tag_content(rev) -> single_pcdata;
tag_content(seealso) -> single_pcdata; % mixed?
tag_content(shortdef) -> single_pcdata;
tag_content(shorttitle) -> single_pcdata;
tag_content(tag) -> mixed_content;
tag_content(tcaption) -> single_pcdata;
tag_content(termdef) -> single_pcdata;
tag_content(title) -> single_pcdata;
tag_content(url) -> single_pcdata; % mixed
tag_content(v) -> single_pcdata;
tag_content(year) -> single_pcdata;
tag_content(_) -> no_pcdata.


