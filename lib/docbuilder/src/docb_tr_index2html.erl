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
-module(docb_tr_index2html).

-export([extension/0, transform/3, rule/2]).

extension() ->
    ".html".

transform(_File0, {index, Attrs, [Header| Trees0]}, _Opts) ->
    Trees1 = prune_flat(Trees0, false),
    %%
    %% Now each element of Trees1 is a tree with tag `name' and
    %% attribute `File', and with one `pcdata' subtree containing the
    %% name `Func' of the function. We extract `File' and `Func', and
    %% create new trees.
    %%
    %% `File' is attribute CDATA (from an <include file=...>), and
    %% `Func' is PCDATA.
    %%
    FileFuncs =
	[{File, RefType, Func} ||
	    {name, [{_, _, File}, {_, _, RefType}|_],
	     [{pcdata, [], Func}]}
		<- Trees1],
    Trees2 = new_trees(FileFuncs),
    {index, Attrs, [Header| Trees2]}.

%% Remove all elements except those with tag equal to `name'.
%% Within `name' remove all elements except those equal to `pcdata'.
%% Add attribute `filetype' to `name'.
%%
%% Refs: appref, comref, cref, erlref, fileref
prune_flat([{appref, _Attrs, More}| Rest], _) ->
    RefType = appref,
    lists:append(prune_flat(More, RefType), prune_flat(Rest, RefType));
prune_flat([{comref, _Attrs, More}| Rest], _) ->
    RefType = comref,
    lists:append(prune_flat(More, RefType), prune_flat(Rest, RefType));
prune_flat([{cref, _Attrs, More}| Rest], _) ->
    RefType = cref,
    lists:append(prune_flat(More, RefType), prune_flat(Rest, RefType));
prune_flat([{erlref, _Attrs, More}| Rest], _) ->
    RefType = erlref,
    lists:append(prune_flat(More, RefType), prune_flat(Rest, RefType));
prune_flat([{fileref, _Attrs, More}| Rest], _) ->
    RefType = fileref,
    lists:append(prune_flat(More, RefType), prune_flat(Rest, RefType));
prune_flat([{name, [Attr0|Attrs0], More}| Rest], RefType) ->
    Attrs = [Attr0, {"FILETYPE", "CDATA", RefType} |
	     Attrs0],
    [{name, Attrs, keep_pcdata(More)}| prune_flat(Rest, RefType)];
prune_flat([{pcdata, _, _}| Rest], RefType) ->		% special case
    prune_flat(Rest, RefType);
prune_flat([{_Tag, _Attrs, More}| Rest], RefType) ->
    lists:append(prune_flat(More, RefType), prune_flat(Rest, RefType));
prune_flat([], _) ->
    [].

keep_pcdata(Trees) ->
    [T || T = {pcdata, _, _} <- Trees].

new_trees(FileFuncs) ->
    Files0 = [{File, RefType} || {File, RefType, _} <- FileFuncs],
    Files1 = lists:usort(Files0),
    FileEntries = [{reffile, File, RefType,
		    [Fu || {Fi, _, Fu} <- FileFuncs, Fi == File]}
		   || {File, RefType} <- Files1],
    FuncEntries = [{func, Func, RefType, [File]}
		   || {File, RefType, Func} <- FileFuncs],
    Entries = FileEntries ++ FuncEntries,
    SortedEntries = sort_entries(Entries),
    %%
    %% We create a tree according to the following "dtd":
    %%
    %%  element   index    (reffile | funcdef)*
    %%  element   reffile     (funcdef2)*
    %%  attribute reffile     filename CDATA
    %%  attribute reffile     filetype CDATA
    %%  element   funcdef2 PCDATA
    %%  attribute funcdef2 filename CDATA
    %%  attribute funcdef2 filetype CDATA
    %%  element   funcdef  PCDATA
    %%  attribute funcdef  filename CDATA
    %%  attribute funcdef  filetype CDATA
    %%
    %%  For example:
    %%  <index>
    %%     <reffile filename="mymod" filetype="erlref">
    %%        <funcdef2 filename="mymod" filetype="erlref">myfunca(A)</>
    %%        <funcdef2 filename="mymod" filetype="erlref">myfuncb(A, B)</>
    %%     </>
    %%     <funcdef filename="mymod" filetype="erlref">myfunca(A)</>
    %%     <funcdef filename="mymod" filetype="erlref">myfuncb(A, B)</>
    %%  </>
    lists:flatmap(
      fun({reffile, File, RefType, Funcs}) ->
	      %% A reffile tree
	      [{reffile, [{"FILENAME", "CDATA", File},
			  {"FILETYPE", "CDATA", RefType}],
		[{funcdef2, [{"FILENAME", "CDATA", File},
			     {"FILETYPE", "CDATA", RefType}],
		  [{pcdata, [], Func}]} || Func <- Funcs]}];
	 ({func, Func, RefType, [File]}) ->
	      %% A func tree
	      [{funcdef, [{"FILENAME", "CDATA", File},
			  {"FILETYPE", "CDATA", RefType}],
		[{pcdata, [], Func}]}]
      end, SortedEntries).

%% Sorting of entries
%%
%% The sorting is based on how names of files and functions are
%% presented (in a browser).
%% Requires conversion to "function/2" etc.
%%
sort_entries(Entries) ->
    ExpEntries =
	lists:map(
	  fun({reffile, File, RefType, Funcs}) ->
		  HFile = filename_sort_order(File),
		  HFuncs = [{funcdef_sort_order(Fu, RefType), Fu} || Fu <- Funcs],
		  {reffile, HFile, File, RefType, lists:sort(HFuncs)};
	     ({func, Func, RefType, [File]}) ->
		  HFunc = funcdef_sort_order(Func, RefType),
		  HFile = filename_sort_order(File),
		  {func, HFunc, Func, RefType, [{HFile, File}]}
	  end, Entries),
    SortedExpEntries = lists:keysort(2, ExpEntries),
    lists:map(
      fun({Tag, _HName, Name, RefType, Vals}) ->
	      NVals = lists:map(fun({_HVal, Val}) -> Val end, Vals),
	      {Tag, Name, RefType, NVals}
      end, SortedExpEntries).

rule([index| _], _) ->
    {docb_html_layout:index_top("") ++
     "<dl>\n",
     "</dl>\n" ++ docb_html_layout:index_bot()};

rule([header| _], _) ->
    {drop, ""};

rule([reffile| _], {_, [File, _RefType|_], _}) ->
    CFile = docb_html_util:attribute_cdata_to_html(File),
    {"<dt><em>" ++ CFile ++ "</em></dt>\n", ""};

rule([funcdef2| _], {_, [File, RefType|_], [{pcdata, [], FuncDef}]}) ->
    FFuncDef = lists:flatten(docb_html_util:pcdata_to_html(FuncDef)),
    TFuncDef = docb_util:trim(FFuncDef),
    ShortFuncDef = docb_html_util:make_funcdef_short(TFuncDef, RefType),
    HRef =
	docb_html_util:make_anchor_href_short(File, TFuncDef, RefType),
    {drop,
     "<dd><a href=\"" ++ HRef ++ "\"><code>" ++
     ShortFuncDef ++ "</code></a></dd>\n"};

rule([funcdef| _], {_, [File, RefType|_], [{pcdata, [], FuncDef}]}) ->
    FFuncDef = lists:flatten(docb_html_util:pcdata_to_html(FuncDef)),
    TFuncDef = docb_util:trim(FFuncDef),
    ShortFuncDef = docb_html_util:make_funcdef_short(TFuncDef, RefType),
    HRef =
	docb_html_util:make_anchor_href_short(File, TFuncDef, RefType),
    CFile = docb_html_util:attribute_cdata_to_html(File),
    {drop,
     "<dt><code>" ++ ShortFuncDef ++ "</code></dt>\n"
     "<dd><a href=\"" ++ HRef ++ "\"><em>" ++
     CFile ++ "</em></a></dd>\n"};

rule(_, _) ->
    {drop, ""}.

filename_sort_order(File) ->
    docb_html_util:html_latin1_sort_order(
      lists:flatten(
	docb_html_util:attribute_cdata_to_html(string:strip(File)))).

funcdef_sort_order(FuncDef, RefType) ->
    docb_html_util:html_latin1_sort_order(
      docb_html_util:make_anchor_name_short(FuncDef, RefType)).
