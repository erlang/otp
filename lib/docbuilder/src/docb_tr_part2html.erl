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
-module(docb_tr_part2html).

-export([extension/0, transform/3, rule/2, rule/3]).

extension() ->
    ".html".

transform(File, {part, _Attrs, [Header| Rest]}, Opts0) ->

    %% Extract header data
    Title = docb_html_util:extract_header_data(title, Header),

    %% Create the framing HTML document
    OutFile = docb_util:outfile(File ++ "_frame", ".html", Opts0),
    case file:open(OutFile, [write]) of
	{ok, Frame} ->
	    io:format(Frame,
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
<!-- This document was generated using DocBuilder-" ++ docb_util:version() ++ " -->
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
  <title>~s</title>
  " ++ docb_util:html_snippet(head,  Opts0) ++ "
</head>
<frameset cols=\"200, *\">
  <frame src=\"~s\" name=\"toc\"/>
  <frame src=\"~s\" name=\"document\"/>
  <noframes>
    <body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\"
          vlink=\"#FF00FF\" alink=\"#FF0000\">
    <p>This documentation requires a browser that can handle frames</p>
    </body>
  </noframes>
</frameset>
</html>
",
		      [Title, File ++ ".html", File ++ "_first.html"]),
	    file:close(Frame)
    end,

    %% Create the front HTML document
    docb_main:transform(first, html, Opts0, File ++ "_first",
			{first, [], [Header| Rest]}),

    %% Extract files to include
    Files =
	case Rest of
	    [{description, _, _}| NewRest] ->
		lists:map(fun({include, [{_, _, F}], _}) -> filename:rootname(F) end,
			  NewRest);
	    [{include, _, _}| _NewRest] ->
		lists:map(fun({include, [{_, _, F}], _}) -> filename:rootname(F) end, Rest)
	end,

    %% Concat all chapters into a *big* parse tree
    %% Also transform them to HTML
    TransformP = not docb_util:an_option(framework_only, Opts0),
    TOpts = [dict, {part_application,File}],
    ConcatTree = concat_files(Files, Opts0, TransformP, TOpts),

    %% Create a cites dictionary
    docb_main:transform(cite, html, Opts0, File ++ "_cite",
			{cite, [], [Header| ConcatTree]}),

    %% Create a terms dictionary
    docb_main:transform(term, html, Opts0, File ++ "_term",
			{term, [], [Header| ConcatTree]}),

    %% Find all fascicules to be put in the top menu of the table of
    %% contents
    Ext = docb_util:lookup_option(src_type, Opts0),
    Opts2 =
	case filelib:is_regular("fascicules"++Ext) of
	    true ->
		case docb_main:parse1("fascicules", Opts0) of
		    {ok, Parse} ->
			FascData = get_fasc_data(Parse),
			case lists:keyfind(File, 1, FascData) of
			    {_, _, "YES", _} ->
				OrigFile =
				    docb_util:outfile(File++"_frame",
						      ".html", Opts0),
				EntryFile =
				    docb_util:outfile("index",
						      ".html", Opts0),
				docb_util:message(info,
						  "Copying ~s to ~s",
						  [OrigFile,EntryFile]),
				file:copy(OrigFile, EntryFile);
			    _ ->
				ok
			end,
			[{fascdata, FascData}| Opts0];
		    errors ->
			%% do not bother
			docb_util:message(
			  warning,
			  "fascicules~s could not be parsed,"
			  " no index.html created~n", [Ext]),
			Opts0
		end;
	    _ ->
		%% do not bother
		docb_util:message(warning,
				  "fascicules~s not found, "
				  "no index.html created~n",
				  [Ext]),
		Opts0
	end,

    %% Create ToC parse tree
    {{toc, [{"FILE", "CDATA", File}], [Header| ConcatTree]}, Opts2}.

concat_files(Files, Opts, TransformP, TOpts) ->
    Ext = docb_util:lookup_option(src_type, Opts),
    concat_files(Files, [], 1, Opts, TransformP, TOpts, Ext).

concat_files([File | Rest], Body, ChLevel, Opts, TP, TOpts, Ext) ->
    case docb_main:parse1(File, Opts) of
	{ok, Parse} ->
	    {TopTag, Attrs, [Header = {header, _, HeaderContents} | More]} = Parse,
	    {title,_,Title} = lists:keyfind(title,1,HeaderContents),
	    NewMore = [{section, [], [{title, [], Title}| More]}],
	    NewParse = {TopTag, Attrs, [Header| NewMore]},
	    if
		TP ->
		    docb_util:message(info,
				      "Processing \"~s~s\"",
				      [File, Ext]),
		    Opts2 =
			[html, {number,integer_to_list(ChLevel)}] ++
			TOpts ++ Opts,
		    docb_main:transform(TopTag, html, Opts2, File,
					NewParse);
		true -> ignore
	    end,
	    NumberTree =
		docb_html_util:number(NewParse,
				      integer_to_list(ChLevel), File),
	    {_, [], [_| NewBody]} = NumberTree,
	    Body ++ concat_files(Rest, NewBody, ChLevel+1, Opts,
				 TP, TOpts, Ext);
	errors ->
	    throw({error,"Parse error when building chapter "++File})
    end;
concat_files([], Body, _ChLevel, _Opts, _TP, _TOpts, _Ext) ->
    Body.

rule([section| _], _) ->
    {"", ""};

rule(_, _) ->
    {drop, ""}.

rule([toc| _], {_Depth, [File], [Header| _]}, Opts) ->
    case docb_util:lookup_option(fascdata, Opts) of
	false ->
	    {{docb_html_layout:part_toc_top(
		docb_html_util:all_header_data(Header), File, Opts),
	      docb_html_layout:part_toc_bot()}, Opts};
	FascData ->
	    HRefTexts =
		lists:map(
		  fun({_File, HRef, _Entry, PCText}) ->
			  {HRef, docb_html_util:pcdata_to_html(PCText)}
		  end,
		  FascData),
	    {{docb_html_layout:part_toc_top(
		docb_html_util:all_header_data(Header),
		File, Opts, HRefTexts),
	     docb_html_layout:part_toc_bot()}, Opts}
    end;

rule([title| Rest], {_, [Number, File], [{pcdata, _, Title}]}, Opts) ->
    N = docb_html_util:count_sections(Rest),
    OutFile = docb_html_util:make_anchor_href(File),
    if
	N == 1 ->
	    {{drop,
	      "<hr/>\n<small>" ++
	      Number ++
	      " <a target=\"document\" href=\"" ++ OutFile ++ "#" ++
	      Number ++ "\">" ++
	      docb_html_util:pcdata_to_html(Title) ++
	      "</a></small><br/>\n"},
	     Opts};
	N < 3 ->
	    {{drop,
	      "<small>" ++
	      Number ++
	      " <a target=\"document\" href=\"" ++ OutFile ++ "#" ++
	      Number ++ "\">" ++
	      docb_html_util:pcdata_to_html(Title) ++
	      "</a></small><br/>\n"},
	     Opts};
	true ->
	    {{drop, ""}, Opts}
    end.

%% Parsed fascicules:
%%   {fascicules,[],
%%    [{fascicule, [{"FILE","CDATA","refman"},
%%                  {"HREF","CDATA","refman_frame.html"},
%%                  {"ENTRY","TOKEN","YES"}],
%%                 [{pcdata, [], ""    Reference  Manual\\n  \n"}]},
%% Returns: [{File, HRef, Entry, Text}].
get_fasc_data({fascicules, _, Fascs}) ->
    lists:map(
      fun({fascicule, Atts, Trees}) -> 
	      AVals = get_avals(Atts),
	      PCText = get_pc_text(Trees),
	      list_to_tuple(lists:append([AVals, [PCText]])) end,
      Fascs).

get_avals(Atts) ->
    [element(3, Tuple) || Tuple <- Atts].

get_pc_text([{pcdata, _, Text}]) ->
    Text.
