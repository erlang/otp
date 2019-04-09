%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @copyright 2003-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Standard doclet module for EDoc.

%% Note that this is written so that it is *not* depending on edoc.hrl!

%% TODO: copy "doc-files" subdirectories, recursively.
%% TODO: generate summary page of TODO-notes
%% TODO: generate summary page of deprecated things
%% TODO: generate decent indexes over modules, methods, records, etc.

-module(edoc_doclet).

-export([run/2]).

-import(edoc_report, [report/2, warning/2]).

%% @headerfile "../include/edoc_doclet.hrl"
-include("../include/edoc_doclet.hrl").

-define(EDOC_APP, edoc).
-define(DEFAULT_FILE_SUFFIX, ".html").
-define(INDEX_FILE, "index.html").
-define(OVERVIEW_FILE, "overview.edoc").
-define(OVERVIEW_SUMMARY, "overview-summary.html").
-define(MODULES_FRAME, "modules-frame.html").
-define(STYLESHEET, "stylesheet.css").
-define(IMAGE, "erlang.png").
-define(NL, "\n").

-include_lib("xmerl/include/xmerl.hrl").

%% Sources is the list of inputs in the order they were found.
%% Modules are sorted lists of atoms without duplicates. (They
%% usually include the data from the edoc-info file in the target
%% directory, if it exists.)

%% @spec (Command::doclet_gen() | doclet_toc(), edoc_context()) -> ok
%% @doc Main doclet entry point. See the file <a
%% href="edoc_doclet.hrl">`edoc_doclet.hrl'</a> for the data
%% structures used for passing parameters.
%%
%% Also see {@link edoc:layout/2} for layout-related options, and
%% {@link edoc:get_doc/2} for options related to reading source
%% files.
%%
%% Options:
%% <dl>
%%  <dt>{@type {file_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the suffix used for output files. The default value is
%%      `".html"'.
%%  </dd>
%%  <dt>{@type {hidden, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of hidden modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {overview, edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the overview-file. By default, this doclet
%%      looks for a file `"overview.edoc"' in the target directory.
%%  </dd>
%%  <dt>{@type {private, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of private modules and
%%      functions will also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {stylesheet_file, edoc:filename()@}}
%%  </dt>
%%  <dd>Specifies the name of the stylesheet file. By default, this
%%      doclet uses the file `"stylesheet.css"' in the `priv'
%%      subdirectory of the EDoc installation directory. The named file
%%      will be copied to the target directory.
%%  </dd>
%%  <dt>{@type {title, string()@}}
%%  </dt>
%%  <dd>Specifies the title of the overview-page.
%%  </dd>
%% </dl>

%% INHERIT-OPTIONS: title/2
%% INHERIT-OPTIONS: sources/5
%% INHERIT-OPTIONS: overview/4
%% INHERIT-OPTIONS: copy_stylesheet/2
%% INHERIT-OPTIONS: stylesheet/1

run(#doclet_gen{}=Cmd, Ctxt) ->
    gen(Cmd#doclet_gen.sources,
	Cmd#doclet_gen.app,
	Cmd#doclet_gen.modules,
	Ctxt);
run(#doclet_toc{}=Cmd, Ctxt) ->
    toc(Cmd#doclet_toc.paths, Ctxt).

gen(Sources, App, Modules, Ctxt) ->
    Dir = Ctxt#context.dir,
    Env = Ctxt#context.env,
    Options = Ctxt#context.opts,
    Title = title(App, Options),
    CSS = stylesheet(Options),
    {Modules1, Error} = sources(Sources, Dir, Modules, Env, Options),
    modules_frame(Dir, Modules1, Title, CSS),
    overview(Dir, Title, Env, Options),
    index_file(Dir, Title),
    edoc_lib:write_info_file(App, Modules1, Dir),
    copy_stylesheet(Dir, Options),
    copy_image(Dir),
    %% handle postponed error during processing of source files
    case Error of
	true -> exit(error);
	false -> ok
    end.


%% NEW-OPTIONS: title
%% DEFER-OPTIONS: run/2

title(App, Options) ->
    proplists:get_value(title, Options,
			if App == ?NO_APP ->
				"Overview";
			   true ->
				io_lib:fwrite("Application: ~ts", [App])
			end).


%% Processing the individual source files.

%% NEW-OPTIONS: file_suffix, private, hidden
%% INHERIT-OPTIONS: edoc:layout/2
%% INHERIT-OPTIONS: edoc:get_doc/3
%% DEFER-OPTIONS: run/2

sources(Sources, Dir, Modules, Env, Options) ->
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Private = proplists:get_bool(private, Options),
    Hidden = proplists:get_bool(hidden, Options),
    {Ms, E} = lists:foldl(fun (Src, {Set, Error}) ->
				  source(Src, Dir, Suffix, Env, Set,
					 Private, Hidden, Error, Options)
			  end,
			  {sets:new(), false}, Sources),
    {[M || M <- Modules, sets:is_element(M, Ms)], E}.


%% Generating documentation for a source file, adding its name to the
%% set if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.

source({M, Name, Path}, Dir, Suffix, Env, Set, Private, Hidden,
       Error, Options) ->
    File = filename:join(Path, Name),
    case catch {ok, edoc:get_doc(File, Env, Options)} of
	{ok, {Module, Doc}} ->
	    check_name(Module, M, File),
	    case ((not is_private(Doc)) orelse Private)
		andalso ((not is_hidden(Doc)) orelse Hidden) of
		true ->
		    Text = edoc:layout(Doc, Options),
		    Name1 = atom_to_list(M) ++ Suffix,
                    Encoding = [{encoding,encoding(Doc)}],
		    edoc_lib:write_file(Text, Dir, Name1, Encoding),
		    {sets:add_element(Module, Set), Error};
		false ->
		    {Set, Error}
	    end;
	R ->
	    report("skipping source file '~ts': ~tP.", [File, R, 15]),
	    {Set, true}
    end.

check_name(M, M0, File) ->
    N = M,
    N0 = M0,
    case N of
	[$? | _] ->
	    %% A module name of the form '?...' is assumed to be caused
	    %% by the epp_dodger parser when the module declaration has
	    %% the form '-module(?MACRO).'; skip the filename check.
	    ok;
	_ ->
	    if N =/= N0 ->
		    warning("file '~ts' actually contains module '~s'.",
			    [File, M]);
	       true ->
		    ok
	    end
    end,
	ok.

%% Creating an index file, with some frames optional.
%% TODO: get rid of frames, or change doctype to Frameset

index_file(Dir, Title) ->
    Frame2 = {frame, [{src,?MODULES_FRAME},
		      {name,"modulesFrame"},{title,""}],
	      []},
    Frame3 = {frame, [{src,?OVERVIEW_SUMMARY},
		      {name,"overviewFrame"},{title,""}],
	      []},
    Frameset = {frameset, [{cols,"20%,80%"}],
    	[?NL, Frame2, ?NL, ?NL, Frame3, ?NL,
		    {noframes,
		     [?NL,
		      {h2, ["This page uses frames"]},
		      ?NL,
		      {p, ["Your browser does not accept frames.",
			   ?NL, br,
			   "You should go to the ",
			   {a, [{href, ?OVERVIEW_SUMMARY}],
			    ["non-frame version"]},
			   " instead.", ?NL]},
		      ?NL]},
		    ?NL]},
    XML = xhtml_1(Title, [], Frameset),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?INDEX_FILE).

modules_frame(Dir, Ms, Title, CSS) ->
    Body = [?NL,
	    {h2, [{class, "indextitle"}], ["Modules"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0},
		     {summary, "list of modules"}],
	     lists:append(
	       [[?NL,
		 {tr, [{td, [],
			[{a, [{href, module_ref(M)},
			      {target, "overviewFrame"},
			      {class, "module"}],
			  [atom_to_list(M)]}]}]}]
		 || M <- Ms])},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

module_ref(M) ->
    atom_to_list(M) ++ ?DEFAULT_FILE_SUFFIX.

xhtml(Title, CSS, Content) ->
    xhtml_1(Title, CSS, {body, [{bgcolor, "white"}], Content}).

xhtml_1(Title, CSS, Body) ->
    {html, [?NL,
	    {head, [?NL, {title, [Title]}, ?NL] ++ CSS},
	    ?NL,
	    Body,
	    ?NL]
    }.

%% NEW-OPTIONS: overview
%% INHERIT-OPTIONS: read_file/4
%% INHERIT-OPTIONS: edoc_lib:run_layout/2
%% INHERIT-OPTIONS: edoc_extract:file/4
%% DEFER-OPTIONS: run/2

overview(Dir, Title, Env, Opts) ->
    File = proplists:get_value(overview, Opts,
			       filename:join(Dir, ?OVERVIEW_FILE)),
    Encoding = edoc_lib:read_encoding(File, [{in_comment_only, false}]),
    Tags = read_file(File, overview, Env, Opts),
    Data0 = edoc_data:overview(Title, Tags, Env, Opts),
    EncodingAttribute = #xmlAttribute{name = encoding,
                                      value = atom_to_list(Encoding)},
    #xmlElement{attributes = As} = Data0,
    Data = Data0#xmlElement{attributes = [EncodingAttribute | As]},
    F = fun (M) ->
		M:overview(Data, Opts)
	end,
    Text = edoc_lib:run_layout(F, Opts),
    EncOpts = [{encoding,Encoding}],
    edoc_lib:write_file(Text, Dir, ?OVERVIEW_SUMMARY, EncOpts).

copy_image(Dir) ->
    case code:priv_dir(?EDOC_APP) of
	PrivDir when is_list(PrivDir) ->
	    From = filename:join(PrivDir, ?IMAGE),
	    edoc_lib:copy_file(From, filename:join(Dir, ?IMAGE));
	_ ->
	    report("cannot find default image file.", []),
	    exit(error)
    end.

%% NEW-OPTIONS: stylesheet_file
%% DEFER-OPTIONS: run/2

copy_stylesheet(Dir, Options) ->
    case proplists:get_value(stylesheet, Options) of
	undefined ->
	    From = case proplists:get_value(stylesheet_file, Options) of
		       File when is_list(File) ->
			   File;
		       _ ->
			   case code:priv_dir(?EDOC_APP) of
			       PrivDir when is_list(PrivDir) ->
				   filename:join(PrivDir, ?STYLESHEET);
			       _ ->
				   report("cannot find default "
					  "stylesheet file.", []),
				   exit(error)
			   end
		   end,
	    edoc_lib:copy_file(From, filename:join(Dir, ?STYLESHEET));
	_ ->
	    ok
    end.

%% NEW-OPTIONS: stylesheet
%% DEFER-OPTIONS: run/2

stylesheet(Options) ->
    case proplists:get_value(stylesheet, Options) of
	"" ->
	    [];
	S ->
	    Ref = case S of
		      undefined ->
			  ?STYLESHEET;
		      "" ->
			  "";    % no stylesheet
		      S when is_list(S) ->
			  S;
		      _ ->
			  report("bad value for option 'stylesheet'.",
				 []),
			  exit(error)
		  end,
	    [{link, [{rel, "stylesheet"},
		     {type, "text/css"},
		     {href, Ref},
		     {title, "EDoc"}], []},
	     ?NL]
    end.

is_private(E) ->
    case get_attrval(private, E) of
 	"yes" -> true;
 	_ -> false
    end.

is_hidden(E) ->
    case get_attrval(hidden, E) of
 	"yes" -> true;
 	_ -> false
    end.

encoding(E) ->
    case get_attrval(encoding, E) of
        "latin1" -> latin1;
        _ -> utf8
    end.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

%% Read external source file. Fails quietly, returning empty tag list.

%% INHERIT-OPTIONS: edoc_extract:file/4

read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
	{ok, Tags} ->
	    Tags;
	{error, _} ->
	    []
    end.


%% TODO: FIXME: meta-level index generation

%% Creates a Table of Content from a list of Paths (ie paths to applications)
%% and an overview file.

-define(EDOC_DIR, "doc").
-define(INDEX_DIR, "doc/index").
-define(CURRENT_DIR, ".").

toc(Paths, Ctxt) ->
    Opts = Ctxt#context.opts,
    Dir = Ctxt#context.dir,
    Env = Ctxt#context.env,
    app_index_file(Paths, Dir, Env, Opts).

%% TODO: FIXME: it's unclear how much of this is working at all

%% NEW-OPTIONS: title
%% INHERIT-OPTIONS: overview/4

app_index_file(Paths, Dir, Env, Options) ->
    Title = proplists:get_value(title, Options,"Overview"),
%    Priv = proplists:get_bool(private, Options),
    CSS = stylesheet(Options),
    Apps1 = [{filename:dirname(A),filename:basename(A)} || A <- Paths],
    index_file(Dir, Title),
    application_frame(Dir, Apps1, Title, CSS),
    modules_frame(Dir, [], Title, CSS),
    overview(Dir, Title, Env, Options),
%    edoc_lib:write_info_file(Prod, [], Modules1, Dir),
    copy_stylesheet(Dir, Options).

application_frame(Dir, Apps, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Applications"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     lists:append(
	       [[{tr, [{td, [], [{a, [{href,app_ref(Path,App)},
				      {target,"_top"}],
				  [App]}]}]}]
		|| {Path,App} <- Apps])},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

app_ref(Path,M) ->
    filename:join([Path,M,?EDOC_DIR,?INDEX_FILE]).
