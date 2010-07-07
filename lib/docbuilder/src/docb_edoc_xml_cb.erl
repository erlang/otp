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
-module(docb_edoc_xml_cb).

%% This is the EDoc callback module for creating DocBuilder erlref
%% documents (man pages) in XML format, and also a DocBuilder chapter
%% document based on "overview.edoc".
%%
%% Usage examples:
%%   docb_gen File
%%   docb_gen -chapter overview.edoc
%% or (from an Erlang shell)
%% edoc:file(File, [{layout,docb_edoc_xml_cb},{file_suffix,".xml"},
%%                  {preprocess,true}]).
%%
%% The origin of this file is the edoc module otpsgml_layout.erl
%% written by Richard Carlsson.

-export([module/2, overview/2]).

-include("xmerl.hrl").

-define(NL, "\n").

%%-User interface-------------------------------------------------------

%% ERLREF
module(Element, Opts) ->
    SortP = proplists:get_value(sort_functions, Opts, true),
    XML = layout_module(Element, SortP),
    xmerl:export_simple([XML], docb_xmerl_xml_cb, []).

%% CHAPTER
overview(Element, _Opts) ->
    XML = layout_chapter(Element),
    xmerl:export_simple([XML], docb_xmerl_xml_cb, []).

%%--Internal functions--------------------------------------------------

layout_module(#xmlElement{name = module, content = Es}=E, SortP) ->
    Name = get_attrval(name, E),
    Desc = get_content(description, Es),
    ShortDesc = text_only(get_content(briefDescription, Desc)),
    FullDesc =  otp_xmlify(get_content(fullDescription, Desc)),
    Types0 = get_content(typedecls, Es),
    Types1 = lists:sort([{type_name(Et), Et} || Et <- Types0]),
    Functions =
	case SortP of
	    true ->
		lists:sort([{function_name(Ef), Ef} ||
			       Ef <- get_content(functions, Es)]);
	    false ->
		[{function_name(Ef), Ef} ||
		    Ef <- get_content(functions, Es)]
	end,
    Header = {header, [
		       ?NL,{title, [Name]},
		       ?NL,{prepared, [""]},
		       ?NL,{responsible, [""]},
		       ?NL,{docno, ["1"]},
		       ?NL,{approved, [""]},
		       ?NL,{checked, [""]},
		       ?NL,{date, [""]},
		       ?NL,{rev, ["A"]},
		       ?NL,{file, [Name++".xml"]}
		      ]},
    Module = {module, [Name]},
    ModuleSummary = {modulesummary, ShortDesc},
    Description = {description, [?NL|FullDesc]},
    Types = case Types1 of
		[] -> [];
		_ ->
		    [?NL, {section,[{title,["DATA TYPES"]},
				    {marker,[{id,"types"}],[]},
				    ?NL|types(Types1)]}]
	    end,
    Funcs = functions(Functions),
    See = seealso_module(Es),
    Authors = {authors, authors(Es)},
    {erlref,
     [?NL,Header,
      ?NL,Module,
      ?NL,ModuleSummary,
      ?NL,Description]
     ++ Types ++
     [?NL,Funcs,
      ?NL,See,
      ?NL,Authors]
    }.

layout_chapter(#xmlElement{name=overview, content=Es}) ->
    Title = get_text(title, Es),
    Header = {header, [
		       ?NL,{title,[Title]},
		       ?NL,{prepared,[""]},
		       ?NL,{docno,[""]},
		       ?NL,{date,[""]},
		       ?NL,{rev,[""]},
		       ?NL,{file, ["chapter.xml"]}
		      ]},
    DescEs = get_content(description, Es),
    FullDescEs = get_content(fullDescription, DescEs),
    Sections = chapter_ify(FullDescEs, first),
    {chapter, [?NL, Header, ?NL | Sections]}.

chapter_ify([], _) ->
    [];
chapter_ify(Es, first) ->
    %% Everything up to the first section should be made into
    %% plain paragraphs -- or if no first section is found, everything
    %% should be made into one
    case find_next(h3, Es) of
	{Es, []} ->
	    SubSections = subchapter_ify(Es, first),
	    [{section, [?NL,{title,["Overview"]},
			?NL | SubSections]}];
	{FirstEs, RestEs} ->
	    otp_xmlify(FirstEs) ++ chapter_ify(RestEs, next)
    end;
chapter_ify([#xmlElement{name=h3} = E | Es], next) ->
    {SectionEs, RestEs} = find_next(h3, Es),
    SubSections = subchapter_ify(SectionEs, first),
    {Marker, Title} = chapter_title(E),
    [{section, [?NL,{marker,[{id,Marker}],[]},
		?NL,{title,[Title]},
		?NL | SubSections]} | chapter_ify(RestEs, next)].

subchapter_ify([], _) ->
    [];
subchapter_ify(Es, first) ->
    %% Everything up to the (possible) first subsection should be
    %% made into plain paragraphs
    {FirstEs, RestEs} = find_next(h4, Es),
    otp_xmlify(FirstEs) ++ subchapter_ify(RestEs, next);
subchapter_ify([#xmlElement{name=h4} = E | Es], next) ->
    {SectionEs, RestEs} = find_next(h4, Es),
    Elements = otp_xmlify(SectionEs),
    {Marker, Title} = chapter_title(E),
    [{section, [?NL,{marker,[{id,Marker}],[]},
		?NL,{title,[Title]},
		?NL | Elements]} | subchapter_ify(RestEs, next)].

chapter_title(#xmlElement{content=Es}) -> % name = h3 | h4
    case Es of
	[#xmlElement{name=a} = E] ->
	    {get_attrval(name, E), get_text(E)}
    end.

%%--XHTML->XML transformation-------------------------------------------

%% otp_xmlify(Es1) -> Es2
%%   Es1 = Es2 = [#xmlElement{} | #xmlText{}]
%% Fix things that are allowed in XHTML but not in chapter/erlref DTDs.
%% 1)  lists (<ul>, <ol>, <dl>) and code snippets (<pre>) can not occur
%%     within a <p>, such a <p> must be splitted into a sequence of <p>,
%%     <ul>, <ol>, <dl> and <pre>.
%% 2)  <a> must only have either a href attribute (corresponds to a
%%     <seealso> or <url> in the XML code) in which case its content
%%     must be plain text; or a name attribute (<marker>).
%% 3a) <b> content must be plain text.
%%  b) <em> content must be plain text (or actually a <code> element
%%     as well, but a simplification is used here).
%%  c) <pre> content must be plain text (or could actually contain
%%     <input> as well, but a simplification is used here).
%% 4)  <code> content must be plain text, or the element must be split
%%     into a list of elements.
%% 5a) <h1>, <h2> etc is not allowed - replaced by its content within
%%     a <b> tag.
%%  b) <center> is not allowed - replaced by its content.
%%  c) <font>   -"-
%% 6)  <table> is not allowed in erlref, translated to text instead.
%%     Also a <table> in chapter without a border is translated to text.
%%     A <table> in chapter with a border must contain a <tcaption>.
%% 7)  <sup> is not allowed - is replaced with its text content
%%     within parenthesis.
%% 8)  <blockquote> contents may need to be made into paragraphs
%% 9)  <th> (table header) is not allowed - is replaced by
%%     <td><em>...</em></td>.
otp_xmlify([]) ->
    [];
otp_xmlify(Es0) ->
    Es = case is_paragraph(hd(Es0)) of

	     %% If the first element is a <p> xmlElement, then
	     %% the entire element list is ready to be otp_xmlified.
	     true ->
		 Es0;

	     %% If the first element is not a <p> xmlElement, then all
	     %% elements up to the first <p> (or end of list) must be
	     %% made into a paragraph (using the {p, Es} construction)
	     %% before the list is otp_xmlified.
	     false ->
		 case find_next(p, Es0, []) of
		     {[#xmlText{value=Str}] = First, Rest} ->
			 %% Special case: Making a paragraph out of a
			 %% blank line isn't a great idea.
			 case is_empty(Str) of
			     true ->
				 Rest;
			     false ->
				 [{p,First}|Rest]
			 end;
		     {First, Rest} ->
			 [{p,First}|Rest]
		 end
	 end,

    %% Fix paragraph breaks not needed in XHTML but in XML
    EsFixed = otp_xmlify_fix(Es),

    otp_xmlify_es(EsFixed).

%% EDoc does not always translate empty lines (with leading "%%")
%% as paragraph break, this is the fix
otp_xmlify_fix(Es) ->
    otp_xmlify_fix(Es, []).
otp_xmlify_fix([#xmlText{value="\n \n"++_} = E1, E2 | Es], Res) ->
    %% This is how it looks when generating an erlref from a .erl file
    case is_paragraph(E2) of
	false ->
	    {P, After} = find_p_ending(Es, []),
	    otp_xmlify_fix(After, [{p, [E2|P]}, E1 | Res]);
	true ->
	    otp_xmlify_fix([E2|Es], [E1|Res])
    end;
otp_xmlify_fix([#xmlText{value="\n\n"} = E1, E2 | Es], Res) ->
    %% This is how it looks when generating a chapter from overview.edoc
    case is_paragraph(E2) of
	false ->
	    {P, After} = find_p_ending(Es, []),
	    otp_xmlify_fix(After, [{p, [E2|P]}, E1 | Res]);
	true ->
	    otp_xmlify_fix([E2|Es], [E1|Res])
    end;
otp_xmlify_fix([E|Es], Res) ->
    otp_xmlify_fix(Es, [E|Res]);
otp_xmlify_fix([], Res) ->
    lists:reverse(Res).

otp_xmlify_es([E | Es]) ->
    case is_paragraph(E) of
	true ->
	    case otp_xmlify_psplit(E) of

		%% paragraph contained inline tags and text only
		nosplit ->
		    otp_xmlify_e(E) ++ otp_xmlify_es(Es);

		%% paragraph contained dl, ul and/or pre and has been
		%% splitted
		SubEs ->
		    lists:flatmap(fun otp_xmlify_e/1, SubEs) ++
			otp_xmlify_es(Es)
	    end;
	false ->
	    otp_xmlify_e(E) ++ otp_xmlify_es(Es)
    end;
otp_xmlify_es([]) ->
    [].

%% otp_xmlify_psplit(P) -> nosplit | [E]
%% Handles case 1) above.
%% Uses the {p, Es} construct, thus replaces an p xmlElement with one
%% or more {p, Es} tuples if splitting the paraghrap is necessary.
otp_xmlify_psplit(P) ->
    otp_xmlify_psplit(p_content(P), [], []).
otp_xmlify_psplit([#xmlElement{name=Name}=E | Es], Content, Res) ->
    if
	Name==blockquote; Name==ul; Name==ol; Name==dl; Name==pre;
	Name==table ->
	    case Content of
		[] ->
		    otp_xmlify_psplit(Es, [], [E|Res]);
		[#xmlText{value=Str}] ->
		    %% Special case: Making a paragraph out of a blank
		    %% line isn't a great idea. Instead, this can be
		    %% viewed as the case above, where there is no
		    %% content to make a paragraph out of
		    case is_empty(Str) of
			true ->
			    otp_xmlify_psplit(Es, [], [E|Res]);
			false ->
			    Pnew = {p, lists:reverse(Content)},
			    otp_xmlify_psplit(Es, [], [E,Pnew|Res])
		    end;
		_ ->
		    Pnew = {p, lists:reverse(Content)},
		    otp_xmlify_psplit(Es, [], [E,Pnew|Res])
	    end;

	true ->
	    otp_xmlify_psplit(Es, [E|Content], Res)
    end;
otp_xmlify_psplit([E | Es], Content, Res) ->
    otp_xmlify_psplit(Es, [E|Content], Res);
otp_xmlify_psplit([], _Content, []) ->
    nosplit;
otp_xmlify_psplit([], [], Res) ->
    lists:reverse(Res);
otp_xmlify_psplit([], [#xmlText{value="\n\n"}], Res) ->
    lists:reverse(Res);
otp_xmlify_psplit([], Content, Res) ->
    Pnew = {p, lists:reverse(Content)},
    lists:reverse([Pnew|Res]).

%% otp_xmlify_e(E) -> [E]
%% This is the function which does the actual transformation of
%% single elements, normally by making sure the content corresponds
%% to what is allowed by the OTP DTDs.
%% Returns a list of elements as the xmlification in some cases
%% returns no element or more than one element (although in most cases
%% exactly one element).
otp_xmlify_e(#xmlElement{name=a} = E) ->       % 2) above
    otp_xmlify_a(E);
otp_xmlify_e(#xmlElement{name=Tag} = E)        % 3a-c)
  when Tag==b; Tag==em; Tag==pre ->
    Content = text_only(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=code} = E) ->    % 4)
    case catch text_only(E#xmlElement.content) of
	{'EXIT', _Error} ->
	    otp_xmlify_code(E);
	Content ->
	    [E#xmlElement{content=Content}]
    end;
otp_xmlify_e(#xmlElement{name=Tag} = E)        % 5a
  when Tag==h1; Tag==h2; Tag==h3; Tag==h4; Tag==h5;
       Tag==center;
       Tag==font ->
    Content = text_only(E#xmlElement.content),
    [E#xmlElement{name=b, content=Content}];
otp_xmlify_e(#xmlElement{name=Tag} = E)        % 5b-c)
  when Tag==center;
       Tag==font ->
    otp_xmlify_e(E#xmlElement.content);
otp_xmlify_e(#xmlElement{name=table} = E) ->   % 6)
    case parent(E) of
	module ->
	    otp_xmlify_table(E#xmlElement.content);
	overview ->
	    case get_attrval(border, E) of
		"" -> % implies border="0"
		    [{p, otp_xmlify_table(E#xmlElement.content)}];
		"0" ->
		    [{p, otp_xmlify_table(E#xmlElement.content)}];
		_Val ->
		    Content0 = otp_xmlify_e(E#xmlElement.content),
		    Summary = #xmlText{value=get_attrval(summary, E)},
		    TCaption = E#xmlElement{name=tcaption,
					    attributes=[],
					    content=[Summary]},
		    Content = Content0 ++ [TCaption],
		    [E#xmlElement{attributes=[], content=Content}]
	    end
    end;
otp_xmlify_e(#xmlElement{name=sup} = E) ->     % 7)
    Text = get_text(E),
    [#xmlText{parents = E#xmlElement.parents,
	      pos = E#xmlElement.pos,
	      language = E#xmlElement.language,
	      value = "(" ++ Text ++ ")"}];
otp_xmlify_e(#xmlElement{name=blockquote} = E) -> % 8)
    Content = otp_xmlify_blockquote(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=th} = E) ->      % 9)
    Content = otp_xmlify_e(E#xmlElement.content),
    EmE = E#xmlElement{name=em, content=Content},
    [E#xmlElement{name=td, content=[EmE]}];
otp_xmlify_e(#xmlElement{name=p} = E) ->       % recurse
    Content = otp_xmlify_e(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e({p, Content1}) ->
    Content2 = otp_xmlify_e(Content1),
    [{p, Content2}];
otp_xmlify_e(#xmlElement{name=ul} = E) ->
    Content = otp_xmlify_e(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=li} = E) ->
    %% Content may need to be made into <p>s etc.
    Content = otp_xmlify(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=dl} = E) ->
    Content0 = otp_xmlify_e(E#xmlElement.content),
    Content = otp_xmlify_dl(Content0),
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=dt} = E) ->
    %% Special fix: Markers in <taglist> <tag>s are not allowed,
    %% save it using 'put' and place the marker first in the <item>
    %% instead
    Content = case E#xmlElement.content of
		  [#xmlElement{name=a} = A] ->
		      put(dt_marker, otp_xmlify_e(A)),
		      otp_xmlify_e(A#xmlElement.content);
		  _ ->
		      otp_xmlify_e(E#xmlElement.content)
	      end,
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=dd} = E) ->
    %% Content may need to be made into <p>s etc.
    Content0 = otp_xmlify(E#xmlElement.content),
    Content = case get(dt_marker) of
		  undefined -> Content0;
		  [Marker] ->
		      put(dt_marker, undefined),
		      [Marker#xmlElement{content=[]}|Content0]
	      end,
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=tr} = E) ->
    Content = otp_xmlify_e(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e(#xmlElement{name=td} = E) ->
    Content = otp_xmlify_e(E#xmlElement.content),
    [E#xmlElement{content=Content}];
otp_xmlify_e([E | Es]) ->
    otp_xmlify_e(E) ++ otp_xmlify_e(Es);
otp_xmlify_e([]) ->
    [];
otp_xmlify_e(E) ->
    [E].

%%--Tags with special handling------------------------------------------

%% otp_xmlify_a(A1) -> [A2]
%% Takes an <a> element and filters the attributes to decide wheather
%% its a seealso/url or a marker.
%% In the case of a seealso/url, the href part is checked, making
%% sure a .xml/.html file extension is removed (as DocBuilder inserts
%% .html extension when resolving cross references).
%% Also, references to other applications //App has a href attribute
%% value "OTPROOT/..." (due to app_default being set to "OTPROOT" in
%% docb_gen.erl), in this case both href attribute and content must be
%% formatted correctly according to DocBuilder requirements.
otp_xmlify_a(A) ->
    [Attr0] = filter_a_attrs(A#xmlElement.attributes),
    case Attr0 of
	#xmlAttribute{name=href, value=Href0} -> % seealso | url
	    Content0 = text_only(A#xmlElement.content),
	    {Href, Content} = otp_xmlify_a_href(Href0, Content0),
	    [A#xmlElement{attributes=[Attr0#xmlAttribute{value=Href}],
			  content=Content}];
	#xmlAttribute{name=name} -> % marker
	    Content = otp_xmlify_e(A#xmlElement.content),
	    [A#xmlElement{attributes=[Attr0], content=Content}]
    end.

%% filter_a_attrs(Attrs) -> [Attr]
%% Removes all attributes from a <a> element except the href or
%% name attribute.
filter_a_attrs([#xmlAttribute{name=href} = Attr | _Attrs]) ->
    [Attr];
filter_a_attrs([#xmlAttribute{name=name} = Attr | _Attrs]) ->
    [Attr];
filter_a_attrs([_Attr|Attrs]) ->
    filter_a_attrs(Attrs);
filter_a_attrs([]) ->
    [].

%% otp_xmlify_a_href(Href0, Es0) -> {Href1, Es1}
%%   Href = string()
otp_xmlify_a_href("#"++_ = Marker, Es0) -> % <seealso marker="#what">
    {Marker, Es0};
otp_xmlify_a_href("http:"++_ = URL, Es0) -> % external URL
    {URL, Es0};
otp_xmlify_a_href("OTPROOT"++AppRef, Es0) -> % <.. marker="App:FileRef
    [AppS, "doc", FileRef1] = split(AppRef, "/"),
    FileRef = AppS++":"++otp_xmlify_a_fileref(FileRef1, AppS),
    [#xmlText{value=Str0} = T] = Es0,
    Str = case split(Str0, "/") of
	      %% //Application
	      [AppS2] ->
		  %% AppS2 can differ from AppS
		  %% Example: xmerl/XMerL
		  AppS2;
	      [_AppS,ModRef] ->
		  case split(ModRef, ":") of
		      %% //Application/Module
		      [Module] ->
			  Module++"(3)";
		      %% //Application/Module:Type()
		      [_Module,_Type] ->
			  ModRef
		  end;
	      %% //Application/Module:Function/Arity
	      [_AppS,ModFunc,Arity] ->
		  ModFunc++"/"++Arity
	  end,
    {FileRef, [T#xmlText{value=Str}]};
otp_xmlify_a_href("../"++File, Es0) ->
    %% Special case: This kind of relative path is used on some
    %% places within i.e. EDoc and refers to a file within the same
    %% application tree.
    %% Correct the path according to the OTP directory structure
    {"../../"++File, Es0};
otp_xmlify_a_href(FileRef1, Es0) -> % File within the same application
    FileRef2 = otp_xmlify_a_fileref(FileRef1, this),
    {FileRef2, Es0}.

%% otp_xmlify_a_fileref(FileRef1, AppS|this) -> FileRef2
%%   AppS = FileRef = string()
otp_xmlify_a_fileref(FileRef1, AppS) ->
    case split(FileRef1, ".#") of

	%% EDoc default name is "overview-summary.html,
	%% name of OTP User's Guide chapter is "chapter.xml"
	["overview-summary", _Ext] ->
	    "chapter";
	["overview-summary", _Ext, Marker] ->
	    "chapter#"++Marker;

	[File, Ext] when Ext=="xml";
			 Ext=="html", AppS/=this ->
	    File;
	[File, Ext, Marker0] ->
	    %% Here is an awkward solution to an awkward problem
	    %% The marker automatically inserted by DocBuilder at
	    %% each function does not seem to work for EDoc generated
	    %% ERLREFs.
	    %% So if the referenced marker is in an ERLREF generated
	    %% by EDoc, keep it "as is", ie "function-arity".
	    %% If the referenced marker is NOT in an ERLREF generated
	    %% by EDoc, the marker should be on the format
	    %% "function/arity".
	    %% The awkward part of the solution is to decide wheather
	    %% the ERLREF is generated by EDoc or not: Here we make
	    %% the decision based on which application the module
	    %% belongs to -- which is ok when the module was written
	    %% but probably not in the future...
	    EDocApps = ["edoc","hipe","syntax_tools","xmerl"],
	    IsEDocGenerated = lists:member(AppS, EDocApps),
	    Marker = if
			 %% The marker is in a file in *this*
			 %% application (which documentation obviously
			 %% is generated by EDoc), or it is in a file
			 %% in an application which documentation
			 %% is assumed to be generated by EDoc
			 AppS==this; IsEDocGenerated ->
			     Marker0;

			 %% The marker is in a file in an application
			 %% which documentation is assumed NOT to be
			 %% generated by EDoc
			 true ->
			     case split(Marker0, "-") of
				 [Func,Arity] ->
				     Func++"/"++Arity;
				 _ ->
				     Marker0
			     end
		     end,
	    if
		%% Ignore file extension in file reference if it either
		%% is ".xml" or if it is ".html" but AppS/=this, that
		%% is, we're resolving an OTPROOT file reference
		Ext=="xml";
		Ext=="html", AppS/=this ->
		    File++"#"++Marker;
		true ->
		    File++"."++Ext++"#"++Marker
	    end;

	%% References to other files than XML files are kept as-is
	_ ->
	    FileRef1
    end.

%% otp_xmlify_blockquote(Es1) -> Es2
%% Ensures that the content of a <blockquote> is divided into
%% <p>s using the {p, Es} construct.
otp_xmlify_blockquote([#xmlElement{name=p} = E|Es]) ->
    [E | otp_xmlify_blockquote(Es)];
otp_xmlify_blockquote([#xmlText{} = E|Es]) ->
    {P, After} = find_p_ending(Es, []),
    [{p, [E|P]} | otp_xmlify_blockquote(After)];
otp_xmlify_blockquote([]) ->
    [].

%% otp_xmlify_code(E) -> Es
%% Takes a <code> xmlElement and split it into a list of <code> and
%% other xmlElements. Necessary when it contains more than a single
%% xmlText element.
%% Example:
%% #xmlElement{name=code,
%%             content=[#xmlText{}, #xmlElement{name=br}, #xmlText{}]}
%% =>
%% [#xmlElement{name=code, content=[#xmlText{}]},
%%  #xmlElement{name=br},
%%  #xmlElement{name=code, content=[#xmlText{}]}]
otp_xmlify_code(E) ->
    otp_xmlify_code(E, E#xmlElement.content, []).
otp_xmlify_code(Code, [#xmlText{} = E|Es], Acc) ->
    otp_xmlify_code(Code, Es, [Code#xmlElement{content=[E]}|Acc]);
otp_xmlify_code(Code, [#xmlElement{} = E|Es], Acc) ->
    otp_xmlify_code(Code, Es, [E|Acc]);
otp_xmlify_code(_Code, [], Acc) ->
    lists:reverse(Acc).

%% otp_xmlify_dl(Es1) -> Es2
%% Insert empty <dd> elements if necessary.
%% OTP DTDs does not allow <taglist>s with <tag>s but no <item>s.
otp_xmlify_dl([#xmlElement{name=dt} = E|Es]) ->
    [E|otp_xmlify_dl(Es, E)];
otp_xmlify_dl([E|Es]) ->
    [E|otp_xmlify_dl(Es)];
otp_xmlify_dl([]) ->
    [].

otp_xmlify_dl([#xmlElement{name=dd} = E|Es], _DT) ->
    [E|otp_xmlify_dl(Es)];
otp_xmlify_dl([#xmlElement{name=dt} = E|Es], DT) ->
    DD = DT#xmlElement{name=dd, attributes=[], content=[]},
    [DD,E|otp_xmlify_dl(Es, E)];
otp_xmlify_dl([E|Es], DT) ->
    [E|otp_xmlify_dl(Es, DT)];
otp_xmlify_dl([], DT) ->
    DD = DT#xmlElement{name=dd, attributes=[], content=[]},
    [DD].

%% otp_xmlify_table(Es1) -> Es2
%% Transform <table> contents into "text", that is, inline elements.
otp_xmlify_table([#xmlText{} = E|Es]) ->
    [E | otp_xmlify_table(Es)];
otp_xmlify_table([#xmlElement{name=tbody} = E|Es]) ->
    otp_xmlify_table(E#xmlElement.content)++otp_xmlify_table(Es);
otp_xmlify_table([#xmlElement{name=tr, content=Content}|Es]) ->
    %% Insert newlines between table rows
    otp_xmlify_table(Content)++[{br,[]}]++otp_xmlify_table(Es);
otp_xmlify_table([#xmlElement{name=th, content=Content}|Es]) ->
    [{em, Content} | otp_xmlify_table(Es)];
otp_xmlify_table([#xmlElement{name=td, content=Content}|Es]) ->
    otp_xmlify_e(Content) ++ otp_xmlify_table(Es);
otp_xmlify_table([]) ->
    [].

%%--Misc help functions used by otp_xmlify/1 et al---------------------

%% find_next(Tag, Es) -> {Es1, Es2}
%% Returns {Es1, Es2} where Es1 is the list of all elements up to (but
%% not including) the first element with tag Tag in Es, and Es2
%% is the remaining elements of Es.
find_next(Tag, Es) ->
    find_next(Tag, Es, []).
find_next(Tag, [#xmlElement{name=Tag} = E | Es], AccEs) ->
    {lists:reverse(AccEs), [E|Es]};
find_next(Tag, [E|Es], AccEs) ->
    find_next(Tag, Es, [E|AccEs]);
find_next(_Tag, [], AccEs) ->
    {lists:reverse(AccEs), []}.

%% find_p_ending(Es, []) -> {Es1, Es2}
%% Returns {Es1, Es2} where Es1 is the list of all elements up to (but
%% not including) the first paragraph break in Es, and Es2 is
%% the remaining elements of Es2.
%% Paragraph break = <p> tag or empty line
%% the next blank line, <p> or end-of-list as P, and the remaining
%% elements of Es as After.
find_p_ending([#xmlText{value="\n \n"++_} = E|Es], P) ->
    {lists:reverse(P), [E|Es]};
find_p_ending([#xmlElement{name=p} = E|Es], P) ->
    {lists:reverse(P), [E|Es]};
find_p_ending([E|Es], P) ->
    find_p_ending(Es, [E|P]);
find_p_ending([], P) ->
    {lists:reverse(P), []}.

%% is_paragraph(E | P) -> bool()
%%   P = {p, Es}
is_paragraph(#xmlElement{name=p}) -> true;
is_paragraph({p, _Es}) -> true;
is_paragraph(_E) -> false.

%% p_content(E | P) -> Es
p_content(#xmlElement{content=Content}) -> Content;
p_content({p, Content}) -> Content.

%% is_empty(Str) -> bool()
%%   Str = string()
%% Returns true if Str is empty in the sense that it contains nothing
%% but spaces, tabs or newlines.
is_empty("\n"++Str) ->
    is_empty(Str);
is_empty(" "++Str) ->
    is_empty(Str);
is_empty("\t"++Str) ->
    is_empty(Str);
is_empty("") ->
    true;
is_empty(_) ->
    false.

%% split(Str, Seps) -> [Str]
split(Str, Seps) ->
    split(Str, Seps, []).

split([Ch|Str], Seps, Acc) ->
    case lists:member(Ch, Seps) of
	true ->  split(Str, Seps, Acc);
	false -> split(Str, Seps, Acc, [Ch])
    end;
split([], _Seps, Acc) ->
    lists:reverse(Acc).

split([Ch|Str], Seps, Acc, Chs) ->
    case lists:member(Ch, Seps) of
	true ->  split(Str, Seps, [lists:reverse(Chs)|Acc]);
	false -> split(Str, Seps, Acc, [Ch|Chs])
    end;
split([], _Seps, Acc, Chs) ->
    lists:reverse([lists:reverse(Chs)|Acc]).

%%--Functions for creating an erlref document---------------------------

%% function_name(F) -> string()
%%   F = #xmlElement{name=function}
%% Returns the name of a function as "name/arity".
function_name(E) ->
    get_attrval(name, E) ++ "/" ++ get_attrval(arity, E).

%% functions(Fs) -> Es
%%   Fs = [{Name, F}]
%%     Name = string()  "name/arity"
%%     F = #xmlElement{name=function}
functions(Fs) ->
    Es = lists:flatmap(fun ({Name, E}) -> function(Name, E) end, Fs),
    if
	Es==[] ->
	    [];
	true ->
	    {funcs, Es}
    end.

function(_Name, E=#xmlElement{content = Es}) ->
    TypeSpec = get_content(typespec, Es),
    [?NL,{func, [ ?NL,
		  {name, 
			  case funcheader(TypeSpec) of
			      [] ->
				  signature(get_content(args, Es),
					    get_attrval(name, E));
			      Spec -> Spec
			  end
			 },
		  ?NL,{fsummary, fsummary(Es)},
		  ?NL,local_types(TypeSpec),
		  ?NL,{desc,
		       label_anchor(E)++
		       deprecated(Es)++
		       fulldesc(Es)++
		       seealso_function(Es)}
	   ]}].

fsummary([]) -> ["\s"];
fsummary(Es) ->
    Desc = get_content(description, Es),
    case get_content(briefDescription, Desc) of
	[] ->
	    fsummary_equiv(Es);    % no description at all if no equiv
	ShortDesc ->
	    text_only(ShortDesc)
    end.

fsummary_equiv(Es) ->
    case get_content(equiv, Es) of
	[] -> ["\s"];
	Es1 ->
	    case get_content(expr, Es1) of
		[] -> ["\s"];
		[Expr] ->
		    ["Equivalent to ", Expr, ".",?NL]
	    end
    end.

label_anchor(E) ->
    case get_attrval(label, E) of
	"" -> [];
	Ref -> [{marker, [{id, Ref}],[]},?NL]
    end.

label_anchor(Content, E) ->
    case get_attrval(label, E) of
	"" -> Content;
	Ref -> {p,[{marker, [{id, Ref}],[]},
		   {em, Content}]}
    end.

signature(Es, Name) -> 
    [Name, "("] ++ seq(fun arg/1, Es) ++ [") -> term()", ?NL].

arg(#xmlElement{content = Es}) ->
    [get_text(argName, Es)].

funcheader([]) -> [];
funcheader(Es) ->
    [t_name(get_elem(erlangName, Es))] ++ t_utype(get_elem(type, Es)).

local_types([]) -> [];
local_types(Es) ->
    local_defs2(get_elem(localdef, Es)).

local_defs2([]) -> [];
local_defs2(Es) ->
    {type,[?NL | [{v, localdef2(E)} || E <- Es]]}.

%% Like localdef/1, but does not use label_anchor/2 -- we don't want any
%% markers or em tags in <v> tag, plain text only!
localdef2(#xmlElement{content = Es}) ->
    case get_elem(typevar, Es) of
	[] -> 
	    t_utype(get_elem(type, Es));
	[V] ->
	    t_var(V) ++ [" = "] ++ t_utype(get_elem(type, Es))
    end.

type_name(#xmlElement{content = Es}) ->
    t_name(get_elem(erlangName, get_content(typedef, Es))).

types(Ts) ->
    Es = lists:flatmap(fun ({Name, E}) -> typedecl(Name, E) end, Ts),
    [?NL, {taglist,[?NL|Es]}].

typedecl(Name, #xmlElement{content = Es}) ->
    TypedefEs = get_content(typedef, Es),
    Id = "type-"++Name,
    [{tag, typedef(TypedefEs)},
     ?NL,
     {item, [{marker,[{id,Id}],[]} |
	     local_defs(get_elem(localdef, TypedefEs)) ++ fulldesc(Es)]},
     ?NL].

typedef(Es) ->
    Name = ([t_name(get_elem(erlangName, Es)), "("]
  	    ++ seq(fun t_utype_elem/1, get_content(argtypes, Es), [")"])),
    case get_elem(type, Es) of
 	 [] ->
	    [{tt, Name}];
 	 Type ->
	    [{tt, Name ++ [" = "] ++ t_utype(Type)}]
    end.

local_defs([]) -> [];
local_defs(Es) ->
    [?NL, {ul, [{li, [{tt, localdef(E)}]} || E <- Es]}].

localdef(E = #xmlElement{content = Es}) ->
    Var = case get_elem(typevar, Es) of
	      [] -> 
		  [label_anchor(t_abstype(get_content(abstype, Es)), E)];
	      [V] ->
		  t_var(V)
	  end,
    Var ++ [" = "] ++ t_utype(get_elem(type, Es)).

deprecated(Es) ->
    case get_content(deprecated, Es) of
	[] -> [];
	DeprEs ->
	    Es2 = get_content(fullDescription,
			      get_content(description, DeprEs)),
	    Es3 = otp_xmlify_e(Es2),
	    [{p, [{em, ["This function is deprecated: "]} |Es3]}, ?NL]
    end.

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] ->
	    index_desc(Es);
	Desc ->
	    [?NL|otp_xmlify(Desc)] ++ [?NL]
    end.

index_desc(Es) ->
    Desc = get_content(description, Es),
    case get_content(briefDescription, Desc) of
	[] ->
	    equiv(Es);    % no description at all if no equiv
	ShortDesc ->
	    ShortDesc
    end.

seealso_module(Es) ->
    case get_elem(see, Es) of
	[] -> [];
	Es1 ->
	    {section,[{title,["See also"]},{p,seq(fun see/1, Es1, [])}]}
    end.
seealso_function(Es) ->
    case get_elem(see, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{em, ["See also:"]}, " "] ++ seq(fun see/1, Es1, ["."])},
	     ?NL]
    end.

%% ELEMENT see PCDATA
%% ATTLIST name PCDATA
%%         href PCDATA
see(#xmlElement{content=Es0} = E) ->
    Href0 = get_attrval(href, E),
    {Href, Es} = otp_xmlify_a_href(Href0, Es0),
    [{seealso, [{marker, Href}], Es}].
    
equiv(Es) ->
    case get_content(equiv, Es) of
	[] -> ["\s"];
	Es1 ->
	    case get_content(expr, Es1) of
		[] -> [];
		[Expr] ->
		    Expr1 = [Expr],
		    Expr2 = case get_elem(see, Es1) of
				[] ->
				    {c,Expr1};
				[E=#xmlElement{}] ->
 				    case get_attrval(href, E) of
 					"" ->
 					    {c,Expr1};
 					Ref ->
 					    {seealso, [{marker, Ref}], Expr1}
 				    end
			    end,
		    [{p, ["Equivalent to ", Expr2, "."]}, ?NL]
	    end
    end.

authors(Es) ->
    case get_elem(author, Es) of
	[] ->
	    [?NL,{aname,["\s"]},?NL,{email,["\s"]}];
	Es1 ->
	    [?NL|seq(fun author/1, Es1, "", [])]
    end.

author(E=#xmlElement{}) ->
    Name = case get_attrval(name, E) of
	       [] -> "\s";
	       N -> N
	   end,
    Mail = case get_attrval(email, E) of
	       [] -> "\s";
	       M -> M
	   end,
    [?NL,{aname,[Name]},?NL,{email,[Mail]}].

t_name([E]) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> N;
	M ->
	    S = M ++ ":" ++ N,
	    case get_attrval(app, E) of
		"" -> S;
		A -> "//" ++ A ++ "/" ++ S
	    end
    end.

t_utype([E]) ->
    t_utype_elem(E).

t_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
	"" -> t_type(Es);
	Name ->
	    T = t_type(Es),
	    case T of
		[Name] -> T;    % avoid generating "Foo::Foo"
		T -> [Name] ++ ["::"] ++ T
	    end
    end.

t_type([E=#xmlElement{name = typevar}]) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}]) ->
    t_atom(E);
t_type([E=#xmlElement{name = integer}]) ->
    t_integer(E);
t_type([E=#xmlElement{name = float}]) ->
    t_float(E);
t_type([#xmlElement{name = nil}]) ->
    t_nil();
t_type([#xmlElement{name = list, content = Es}]) ->
    t_list(Es);
t_type([#xmlElement{name = tuple, content = Es}]) ->
    t_tuple(Es);
t_type([#xmlElement{name = 'fun', content = Es}]) ->
    t_fun(Es);
t_type([#xmlElement{name = abstype, content = Es}]) ->
    t_abstype(Es);
t_type([#xmlElement{name = union, content = Es}]) ->
    t_union(Es);
t_type([#xmlElement{name = record, content = Es}]) ->
    t_record(Es).

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E) ->
    [get_attrval(value, E)].

t_integer(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_list(Es) ->
    ["["] ++ t_utype(get_elem(type, Es)) ++ ["]"].

t_tuple(Es) ->
    ["{"] ++ seq(fun t_utype_elem/1, Es, ["}"]).

t_fun(Es) ->
    ["("] ++ seq(fun t_utype_elem/1, get_content(argtypes, Es),
		 [") -> "] ++ t_utype(get_elem(type, Es))).

t_record([E|Es]) ->
    ["#", get_attrval(value, E), "{"++ seq(fun t_field/1, Es) ++"}"].
t_field(#xmlElement{name=field, content=[Atom,Type]}) ->
    [get_attrval(value, Atom), "="] ++ t_utype_elem(Type).

t_abstype(Es) ->
    case split_at_colon(t_name(get_elem(erlangName, Es)),[]) of
	{Mod,Type} -> 
	    [Type, "("] ++ 
		seq(fun t_utype_elem/1, get_elem(type, Es), [")"]) ++ 
		[" (see module ", Mod, ")"];
	Type ->
	    [Type, "("] ++ 
		seq(fun t_utype_elem/1, get_elem(type, Es), [")"])
    end.

%% Split at one colon, but not at two (or more)
split_at_colon([$:,$:|_]=Rest,Acc) ->
    lists:reverse(Acc)++Rest;
split_at_colon([$:|Type],Acc) ->
    {lists:reverse(Acc),Type};
split_at_colon([Char|Rest],Acc) ->
    split_at_colon(Rest,[Char|Acc]);
split_at_colon([],Acc) ->
    lists:reverse(Acc).

t_union(Es) ->
    seq(fun t_utype_elem/1, Es, " | ", []).

%% seq(Fun, Es)
%% seq(Fun, Es, Tail)
%% seq(Fun, Es, Sep, Tail) -> [string()]
%%   Fun = function(E) -> [string()]
%%   Sep = string()
%%   Tail = [string()]
%% Applies Fun to each element E in Es and return the appended list of
%% strings, separated by Sep which defaults to ", " and ended by Tail
%% which defaults to [].
seq(Fun, Es) ->
    seq(Fun, Es, []).
seq(Fun, Es, Tail) ->
    seq(Fun, Es, ", ", Tail).
seq(Fun, [E], _Sep, Tail) ->
    Fun(E) ++ Tail;
seq(Fun, [E | Es], Sep, Tail) ->
    Fun(E) ++ [Sep] ++ seq(Fun, Es, Sep, Tail);
seq(_Fun, [], _Sep, Tail) ->
    Tail.

%%--Misc functions for accessing fields etc-----------------------------

%% Type definitions used below:
%%   E = #xmlElement{} | #xmlText{}
%%   Es = [E]
%%   Tag = atom(), XHTML tag
%%   Name = atom(), XHTML attribute name
%%   Attrs = [#xmlAttribute{}]
%%   Ts = [#xmlText{}]

%% parent(E) -> module | overview
parent(E) ->
    Parents = E#xmlElement.parents,
    {Parent,_} = lists:last(Parents),
    Parent.

%% get_elem(Tag, Es1) -> Es2
%% Returns a list of all elements in Es which have the name Tag.
get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

%% get_attr(Name, Attrs1) -> Attrs2
%% Returns a list of all attributes in Attrs1 which have the name Name.
get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

%% get_attrval(Name, E) -> string()
%% If E has one attribute with name Name, return its value, otherwise ""
get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

%% get_content(Tag, Es1) -> Es2
%% If there is one element in Es1 with name Tag, returns its contents,
%% otherwise []
get_content(Name, Es) ->
    case get_elem(Name, Es) of
	[#xmlElement{content = Es1}] ->
	    Es1;
	[] -> []
    end.

%% get_text(Tag, Es) -> string()
%% If there is one element in Es with name Tag, and its content is 
%% a single xmlText, return the value of this xmlText.
%% Otherwise return "".
get_text(Name, Es) ->
    case get_content(Name, Es) of
	[#xmlText{value = Text}] ->
	    Text;
	[] -> ""
    end.

%% get_text(E) -> string()
%% Return the value of an single xmlText which is the content of E,
%% possibly recursively.
get_text(#xmlElement{content=[#xmlText{value=Text}]}) ->
    Text;
get_text(#xmlElement{content=[E]}) ->
    get_text(E).

%% text_only(Es) -> Ts
%% Takes a list of xmlElement and xmlText and return a lists of xmlText.
text_only([#xmlElement{content = Content}|Es]) ->
    text_only(Content) ++ text_only(Es);
text_only([#xmlText{} = E |Es]) ->
    [E | text_only(Es)];
text_only([]) ->
    [].
