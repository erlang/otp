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
-module(docb_tr_application2html).

-export([extension/0, transform/3, rule/2, rule/3]).

extension() ->
    ".html".

transform(File, {application, _Attrs, [Header|Rest]}, Opts0) ->

    %% Extract header data
    Title = docb_html_util:extract_header_data(title, Header),

    case docb_util:an_option(kwicindex_only, Opts0) of
	false ->

	    %% Create the framing HTML document
	    OutFile = docb_util:outfile(File++"_frame", ".html", Opts0),
	    case file:open(OutFile, [write]) of
		{ok, Fd} ->
		    io:format(Fd,
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
<!-- This document was generated using DocBuilder-" ++ docb_util:version() ++ " -->
<html>
<head>
  <title>~s</title>
  " ++ docb_util:html_snippet(head, Opts0) ++ "
</head>
<frameset cols=\"150, *\">
  <frame src=\"~s\" name=\"toc\">
  <frame src=\"~s\" name=\"document\">
  <noframes>
    <body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\"
          vlink=\"#FF00FF\" alink=\"#FF0000\">
    <p>This documentation requires a browser that can handle frames</p>
    </body>
  </noframes>
</frameset>
</html>
",
			      [Title,
			       File++".html", File++"_first.html"]),
		    file:close(Fd)
	    end,

	    %% Create the front HTML document
	    docb_main:transform(first, html, Opts0, File ++ "_first",
				{first, [], [Header|Rest]});

	true ->
	    ok
    end,

    %% Extract files to include
    Files = case Rest of
		[{description, _, _}|NewRest] ->
		    lists:map(fun({include, [{_, _, F}], _}) ->filename:rootname(F) end,
			      NewRest);
		[{include, _, _}|_NewRest] ->
		    lists:map(fun({include, [{_, _, F}], _}) -> filename:rootname(F) end,
			      Rest)
	    end,

    %% Concat all reference manuals into a *big* parse tree
    ConcatTree = concat_files(Files, Opts0),

    %% Create the kwic index src file to be put in outdir
    docb_main:transform(refs, kwic, Opts0, File, {refs,[],ConcatTree}),

    case docb_util:an_option(kwicindex_only, Opts0) of
	false ->

	    %% Create an index
	    docb_main:transform(index, html, Opts0, File ++ "_index",
				{index, [], [Header|ConcatTree]}),
	    %% Create a cite dictionary
	    docb_main:transform(cite, html, Opts0, File ++ "_cite",
				{cite, [], [Header|ConcatTree]}),

	    %% Create a term dictionary
	    docb_main:transform(term, html, Opts0, File ++ "_term",
				{term, [], [Header|ConcatTree]}),

	    %% Transform each reference page
	    case docb_util:an_option(framework_only, Opts0) of
		true ->
		    ok;
		false ->
		    transform_refs(Files,
				   [dict,{part_application,File}|Opts0])
	    end;
	true ->
	    ok
    end,

    %% Find all fascicules to be put in the top menu of the table of
    %% contents
    Ext = docb_util:lookup_option(src_type, Opts0),
    Opts2 =
	case filelib:is_regular("fascicules"++Ext) of
	    true ->
		case docb_main:parse1("fascicules", Opts0) of
		    {ok, Parse} ->
			FascData = get_fasc_data(Parse),
                        case lists:keysearch(File, 1, FascData) of
			    {value, {_, _, "YES", _}} ->
				OrigFile =
				    docb_util:outfile(File++"_frame",
						      ".html", Opts0),
				EntryFile =
				    docb_util:outfile("index",
						      ".html",Opts0),
				docb_util:message(info,
						  "Copying ~s to ~s",
						  [OrigFile,EntryFile]),
				file:copy(OrigFile, EntryFile);
			    _ ->
				ok
			end,
			[{fascdata, FascData}| Opts0];
		    errors ->
			%% Do not bother
			docb_util:message(
			  warning,
			  "fascicules~s could not be parsed,"
			  " no index.html created",
			  [Ext]),
			Opts0
		end;
	    false ->
		%% do not bother
		docb_util:message(warning,
				  "fascicules~s not found, "
				  "no index.html created",
				  [Ext]),
		Opts0
	end,

    %% Create ToC parse tree
    {{toc, [{"FILE", "CDATA", File}], [Header|make_toc(ConcatTree)]},
     Opts2}.

concat_files(Files, Opts) ->
    concat_files(Files, [], Opts).

concat_files([File|Rest], Body, Opts) ->
    case docb_main:parse1(File, Opts) of
	{ok, Parse} ->
	    NewParse=expand([Parse], File),
	    %% Remove the reference manual header
	    [{Ref, [], [_Hdr| NewBody]}] = NewParse,
	    RefParse = [{Ref, [], NewBody}],
	    lists:append(Body, concat_files(Rest, RefParse, Opts));
	errors ->
	    errors
    end;
concat_files([], Body, _Opts) ->
    Body.

expand([], _) ->
    [];
expand([{pcdata, Attrs, More}|Rest], File) ->
    [{pcdata, Attrs, More}|expand(Rest, File)];
expand([{name, Attrs, More}|Rest], File) ->
    [{name, [{"FILE", "CDATA", File}|Attrs], More}|expand(Rest, File)];
expand([{module, Attrs, More}|Rest], File) ->
    [{module, [{"FILE", "CDATA", File}|Attrs], More}|expand(Rest,File)];
expand([{file, Attrs, More}|Rest], File) ->
    [{file, [{"FILE", "CDATA", File}|Attrs], More}|expand(Rest, File)];
expand([{app, Attrs, More}|Rest], File) ->
    [{app, [{"FILE", "CDATA", File}|Attrs], More}|expand(Rest, File)];
expand([{lib, Attrs, More}|Rest], File) ->
    [{lib, [{"FILE", "CDATA", File}|Attrs], More}|expand(Rest, File)];
expand([{com, Attrs, More}|Rest], File) ->
    [{com, [{"FILE", "CDATA", File}|Attrs], More}|expand(Rest, File)];
expand([{Tag, Attrs, More}|Rest], File) ->
    [{Tag, Attrs, expand(More, File)}|expand(Rest, File)].

transform_refs([], _) ->
    ok;
transform_refs([File|Rest], Opts) ->
    Ext = docb_util:lookup_option(src_type, Opts),
    docb_util:message(info, "Processing \"~s~s\"", [File, Ext]),
    docb_main:process(File, Opts),
    transform_refs(Rest, Opts).

make_toc([]) ->
    [];
make_toc([{pcdata, _Attrs, _More}|Rest]) ->
    make_toc(Rest);
make_toc([{module, Attrs, More}|Rest]) ->
    [{module, Attrs, More}|make_toc(Rest)];
make_toc([{file, Attrs, More}|Rest]) ->
    [{file, Attrs, More}|make_toc(Rest)];
make_toc([{app, Attrs, More}|Rest]) ->
    [{app, Attrs, More}|make_toc(Rest)];
make_toc([{lib, Attrs, More}|Rest]) ->
    [{lib, Attrs, More}|make_toc(Rest)];
make_toc([{com, Attrs, More}|Rest]) ->
    [{com, Attrs, More}|make_toc(Rest)];
make_toc([{_Tag, _Attrs, More}|Rest]) ->
    lists:append(make_toc(More), make_toc(Rest)).

rule([module|_], {_, [File], _}) ->
    {"<small><a target=\"document\" href=\"" ++
     docb_html_util:make_anchor_href(File) ++ "\">",
     "</a></small><br/>\n"};

rule([file|_], {_, [File], _}) ->
    {"<small><a target=\"document\" href=\"" ++
     docb_html_util:make_anchor_href(File) ++ "\">",
     "</a></small><br/>\n"};

rule([app|_], {_, [File], _}) ->
    {"<small><a target=\"document\" href=\"" ++
     docb_html_util:make_anchor_href(File) ++ "\">",
     "</a></small><br/>\n"};

rule([lib|_], {_, [File], _}) ->
    {"<small><a target=\"document\" href=\"" ++
     docb_html_util:make_anchor_href(File) ++ "\">",
     "</a></small><br/>\n"};

rule([com|_], {_, [File], _}) ->
    {"<small><a target=\"document\" href=\"" ++
     docb_html_util:make_anchor_href(File) ++ "\">",
     "</a></small><br/>\n"};

rule([pcdata|_], {_, _, Data}) ->
    {drop, docb_html_util:pcdata_to_html(Data)};

rule(_, _) ->
    {drop, ""}.

rule([toc|_], {_Depth, [File], [Header|_]}, Opts) ->
    case docb_util:lookup_option(fascdata, Opts) of
	false ->
	    {{docb_html_layout:application_toc_top(
		docb_html_util:all_header_data(Header),
		File, Opts),
	      docb_html_layout:part_toc_bot()}, Opts};
	FascData ->
	    HRefTexts =
		lists:map(
		  fun({_File, HRef, _Entry, PCText}) ->
			  {HRef, docb_html_util:pcdata_to_html(PCText)}
		  end,
		  FascData),
	    {{docb_html_layout:application_toc_top(
		docb_html_util:all_header_data(Header),
		File, Opts, HRefTexts) ++ "\n",
	      docb_html_layout:part_toc_bot()}, Opts}
    end.

%% Returns: [{File, HRef, Entry, Text}].
get_fasc_data({fascicules, _, Fascs}) ->
    lists:map(
      fun({fascicule, Atts, Trees}) ->
	      AVals = get_avals(Atts),
	      PCText = get_pc_text(Trees),
	      list_to_tuple(lists:append([AVals, [PCText]]))
      end,
      Fascs).

get_avals(Atts) ->
    lists:map(fun(Tuple) ->
		      element(3, Tuple) end,
	      Atts).

get_pc_text([{pcdata, _, Text}]) ->
    Text.
