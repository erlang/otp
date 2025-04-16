%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% %CopyrightEnd%
%% 

%% @doc Doclet converting an edoc application to use <a href="https://www.erlang.org/eeps/eep-0059">EEP-59</a> and Markdown.
%%
%% This doclet has to be used together with {@link edoc_layout_chunks}.
%%
%% Example:
%%
%% ```
%%1> edoc:application(example, [{preprocess, true}, {doclet, edoc_doclet_markdown},
%%      {layout, edoc_layout_chunks}]).
%% '''
%%
%% It will convert the overview to Markdown and any module documentation to use
%% `-doc' attributes and Markdown. Any XHTML tags in the edoc documentation that are
%% not part of the tags supported by <a href="doc_storage.html#erlang-documentation-format">
%% Erlang Documentation Format</a> will be added as HTML tags in the Markdown.
%%
%% It does not delete the old edoc documentation.
%%
%% @see edoc_layout_chunks
%% @end

%% Note that this is written so that it is *not* depending on edoc.hrl!

-module(edoc_doclet_markdown).

-export([run/2]).

%% @headerfile "../include/edoc_doclet.hrl"
-include("../include/edoc_doclet.hrl").

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/eep48.hrl").

-define(debug(Format, Args), ok).
%-define(debug(Format, Args), io:format(Format, Args)).

%% @doc Main doclet entry point.
%%
%% This doclet is tightly coupled with {@link edoc_layout_chunks}
%% and should be used together with it.
-spec run(edoc_doclet:command(), edoc_doclet:context()) -> ok.
run(#doclet_gen{} = Cmd, Ctxt) ->
    try
        gen(Cmd#doclet_gen.sources,
            Cmd#doclet_gen.app,
            Cmd#doclet_gen.modules,
            Ctxt)
    catch E:R:St ->
            ?debug("error: ~p\n"
                   "stacktrace:\n~p\n\n", [R, St]),
            erlang:raise(E,R,St)
    end;
run(#doclet_toc{} = _Cmd, _Ctxt) ->
    erlang:error(not_implemented).

gen(Sources, App, Modules, Ctxt) ->
    Dir = Ctxt#doclet_context.dir,
    Env = Ctxt#doclet_context.env,
    Options = Ctxt#doclet_context.opts,
    overview(Dir, App, Env, Options),
    case sources(Sources, App, Modules, Env, Options) of
	{_, true = _Error} -> exit(error);
	{_, false} -> ok
    end.

-define(OVERVIEW_FILE, "overview.edoc").
-define(OVERVIEW_MD, "overview.md").
overview(Dir, App, Env, Opts0) ->
    File = proplists:get_value(overview, Opts0,
			       filename:join(Dir, ?OVERVIEW_FILE)),
    Opts = [{source, File} | Opts0],
    Title = title(App, Opts),
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
    ErlangHtml = edoc_lib:run_layout(F, Opts),
    Text = edoc_html_to_markdown:convert_html(App, ErlangHtml),
    EncOpts = [{encoding,Encoding}],
    edoc_lib:write_file(Text, filename:dirname(File), ?OVERVIEW_MD, EncOpts).

read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
	{ok, Tags} ->
	    Tags;
	{error, _} ->
	    []
    end.

title(App, Options) ->
    proplists:get_value(title, Options,
			if App == no_app ->
				"Overview";
			   true ->
				io_lib:fwrite("Application: ~ts", [App])
			end).

%% @doc Process the individual source files.

%% NEW-OPTIONS: file_suffix, private, hidden
%% INHERIT-OPTIONS: edoc:layout/2
%% INHERIT-OPTIONS: edoc:get_doc/3
%% DEFER-OPTIONS: run/2

sources(Sources, App, Modules, Env, Options) ->
    {Ms, E} = lists:foldl(fun (Src, {Set, Error}) ->
				  source(Src, App, Env, Set, Error, Options)
			  end,
			  {sets:new(), false}, Sources),
    {[M || M <- Modules, sets:is_element(M, Ms)], E}.


%% @doc Write a chunk file for a source file.
%%
%% Add its name to the set if it was successful.
%% Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.
source({Module, Name, Path}, App, Env, OkSet, ErrorFlag, Options0) ->
    File = filename:join(Path, Name),
    try
	%% Without these opts the entries returned by EDoc core (`edoc_extract:source1/5') won't have
	%% all the necessary data to generate chunks.
	RequiredChunkOpts = [return_entries, private, hidden],
	%% But we also want to have the real user-defined `private' accessible.
	Options = ([{show_private, proplists:get_bool(private, Options0)}]
		   ++ RequiredChunkOpts
		   ++ Options0),
	{_Module, Doc, Entries} = edoc:get_doc(File, Env, Options),
	#docs_v1{ module_doc = ModuleDoc, metadata = ModuleMeta, docs = Docs} = DocsV1 =
            binary_to_term(edoc:layout(Doc, [{entries, Entries}, {source, Name} | Options])),

        {ok, Cwd} = file:get_cwd(),
        Meta = [{cwd, Cwd}],
        AST = edoc:read_source(File, Options),
        NewFiles = convert(filter_and_fix_anno(expand_anno(AST), Docs, ModuleDoc),
                           #{ meta => Meta, ast => AST, docs => DocsV1,
                              application => App, module => Module }),
        {_, ModuleAttrFile, ModuleAttrAnno} =
            lists:foldl(
              fun({attribute, [{generated,true}|_], file, {MAFile, Line}}, {false, _, _}) ->
                      {true, MAFile, Line};
                 (_, FileAnno) when is_tuple(FileAnno) ->
                      FileAnno;
                 ({attribute, _, file, {MAFile,_}}, _) ->
                      MAFile;
                 ({attribute, Anno, module, _}, MAFile) ->
                      {false, MAFile, Anno}
              end, undefined, AST),
        ModuleAttrFilename = filename:join(proplists:get_value(cwd, Meta, ""), ModuleAttrFile),

        {BeforeModule, AfterModule} =
            lists:split(
              erl_anno:line(ModuleAttrAnno),
              case maps:get(ModuleAttrFilename, NewFiles, undefined) of
                  undefined ->
                      {ok, Bin} = file:read_file(ModuleAttrFilename),
                      string:split(Bin, "\n", all);
                  F -> F
              end),

        NewFilesWithModuleDoc =
            NewFiles#{ ModuleAttrFilename =>
                           BeforeModule ++
                           convert_moduledoc(ModuleDoc, ModuleMeta, App, Module) ++
                           AfterModule
                     },

        _ = [ begin
                  io:format("Updated ~ts~n",[Key]),
                  ok = file:write_file(Key, format(lists:flatten(lists:join($\n,Value))))
              end || Key := Value <- NewFilesWithModuleDoc, not is_atom(Key)],
	{sets:add_element(Name, OkSet), ErrorFlag}
    catch _:_R:_St ->
	?debug("error: ~p\n"
	       "stacktrace:\n~p\n\n", [_R, _St]),
	{OkSet, true}
    end.

format(Text) ->
    unicode:characters_to_binary(
      lists:map(fun({doc, Doc}) ->
                        doc(Doc);
                   ({moduledoc, Doc}) ->
                        moduledoc(Doc);
                   (Else) ->
                        Else
                end, Text)).

doc(String) ->
    doc("doc", String).
moduledoc(String) ->
    doc("moduledoc", String).
doc(Tag,String) ->
    TrimmedString = string:trim(String),
    case {string:find(TrimmedString,"\n"),
          string:find(TrimmedString,"\\"),
          string:find(TrimmedString,"\"")} of
        {nomatch, nomatch, nomatch} ->
            ["-",Tag," \"", TrimmedString, "\"."];
        _ ->
            ["-",Tag," \"\"\"\n", TrimmedString, "\n\"\"\"."]
    end.

convert_moduledoc(#{ <<"en">> := ModuleHeader }, Meta, Application, Module) ->
    String = edoc_html_to_markdown:convert_html(
               Application, Module,
               shell_docs:normalize(ModuleHeader)),
    [{moduledoc,String} | modulemeta(Meta)];
convert_moduledoc(M, Meta, _, _) when M =:= #{} ->
    ["-moduledoc false." | modulemeta(Meta)];
convert_moduledoc(hidden, Meta, _, _) ->
    ["-moduledoc false." | modulemeta(Meta)].

convert(Docs, Files) ->
    SortedDocs =
        lists:sort(
          fun(MFA1, MFA2) ->
                  Anno1 = element(2, MFA1),
                  Anno2 = element(2, MFA2),
                  case erl_anno:file(Anno1) =:= erl_anno:file(Anno2) of
                      true ->
                          erl_anno:line(Anno1) >= erl_anno:line(Anno2);
                      false ->
                          erl_anno:file(Anno1) >= erl_anno:file(Anno2)
                  end
          end, Docs),
    {Prev, Acc} =
        case SortedDocs of
            [] -> {[],[]};
            SortedDocs ->
                lists:foldl(
                  fun(MFA,{[H|_] = Prev,Acc}) ->
                          MFAAnno = element(2, MFA),
                          HAnno = element(2, H),
                          case erl_anno:file(MFAAnno) =:= erl_anno:file(HAnno) andalso
                              erl_anno:line(MFAAnno) =:= erl_anno:line(HAnno) of
                              true ->
                                  {[MFA|Prev],Acc};
                              false ->
                                  {[MFA],lists:reverse(Prev) ++ Acc}
                          end
                  end, {[hd(SortedDocs)],[]}, tl(SortedDocs))
        end,
    %% io:format("~p",[SortedDocs]),
    convert([], [], lists:reverse(Prev ++ Acc), Files).
convert([], [], [], Files) ->
    %% When there are no documented functions in module
    Cwd = proplists:get_value(cwd, maps:get(meta, Files), ""),
    {attribute, _, file, {Filename, _}} = lists:keyfind(file, 3, maps:get(ast, Files)),
    {ok, Bin} = file:read_file(filename:join(Cwd, Filename)),
    Files#{ filename:join(Cwd, Filename) => string:split(Bin,"\n",all) };
convert(Lines, Acc, [], Files) ->
    Files#{ maps:get(filename, Files) => Lines ++ Acc};
convert(Lines, Acc, [{{K,F,A}, 0, _, _, _} = E | T], Files) ->
    io:format("Skipping ~p ~p/~p~n",[K,F,A]),
    convert(Lines, Acc, T, Files#{ skipped => [E | maps:get(skipped, Files, [])] });
convert(Lines, Acc, [{{function = K,behaviour_info = F,1 = A}, _, _, hidden, _} = E | T], Files) ->
    io:format("Skipping ~p ~p/~p~n",[K,F,A]),
    convert(Lines, Acc, T, Files#{ skipped => [E | maps:get(skipped, Files, [])] });
convert(Lines, Acc, [{Kind, Anno, _Slogan, D, Meta} = E | T] = Docs, Files) ->
    case erl_anno:file(Anno) =:= maps:get(current, Files, undefined) of
        true ->
            {Before, After} = lists:split(erl_anno:line(Anno)-1, Lines),
            DocString = generate_doc_attributes(D, Meta,
                                                Files#{ current => E }),
            SpecString =
                case lists:search(
                       fun(Elem) ->
                               {_, F, A} = Kind,
                               element(1, Kind) =:= function andalso
                                   tuple_size(Elem) =:= 4 andalso
                                   element(3, Elem) =:= spec andalso
                                   (element(1, element(4, Elem)) =:= {F,A} orelse
                                    element(1, element(4, Elem)) =:= {erlang,F,A})
                       end, maps:get(ast, Files)) of
                    {value,_} -> %% Found a spec
                        "";
                    _ when D =:= #{}, not is_map_key(equiv, Meta) ->
                        %% Undocumented function
                        "";
                    _ when D =:= false; D =:= hidden ->
                        %% Undocumented function
                        "";
                    false ->
                        []
                end,
            convert(Before, DocString ++ SpecString ++ After ++ Acc, T, Files);
        false ->
            Cwd = proplists:get_value(cwd, maps:get(meta, Files), ""),
            Filename = filename:join(Cwd, erl_anno:file(Anno)),
            {ok, Bin} = file:read_file(Filename),

            NewFiles =
                case maps:get(current, Files, undefined) of
                    undefined -> Files;
                    _ -> Files#{ maps:get(filename, Files) => Lines ++ Acc }
                end,
            convert(string:split(Bin,"\n",all), [], Docs,
                    NewFiles#{ current => erl_anno:file(Anno), filename => Filename })
    end.

generate_doc_attributes(D, Meta, Files) ->
    DocString =
        case D of
            #{ <<"en">> := ErlangHtml } when not is_map_key(equiv, Meta) ->
                [{doc,edoc_html_to_markdown:convert_html(
                        maps:get(application, Files),
                        maps:get(module, Files),
                        shell_docs:normalize(ErlangHtml))}];
            D when D =:= #{}, is_map_key(equiv, Meta) ->
                [];
            D when D =:= #{} ->
                [];
            hidden ->
                ["-doc false."]
        end,
    DocString ++ meta(Meta).

meta(#{ edit_url := _} = Meta) ->
    meta(maps:remove(edit_url, Meta));
meta(#{ signature := _} = Meta) ->
    meta(maps:remove(signature, Meta));
meta(#{ equiv := {function,F,A} } = Meta) ->
    [io_lib:format("-doc(#{equiv => ~p/~p}).",[F,A]) | meta(maps:remove(equiv, Meta))];
meta(Meta) when Meta =:= #{} ->
    "";
meta(Meta) ->
    [io_lib:format("-doc(~p).",[Meta])].

modulemeta(Meta) ->
    case maps:without([name,otp_doc_vsn,source,types],Meta) of
        M when map_size(M) =:= 0 ->
            [];
        M ->
            [io_lib:format("-moduledoc(~p).",[M])]
    end.

%% Expand all top level anno in the AST to also include which file the anno refers to
expand_anno(AST) ->
    {NewAST, _} =
        lists:mapfoldl(fun F({attribute, _, file, {NewFile, _}} = E, File) when NewFile =/= File ->
                               F(E, NewFile);
                           F(E, File) ->
                               {setelement(2, E, erl_anno:set_file(File, element(2, E))), File}
                       end, undefined, AST),
    NewAST.

%% We fix all the anno tags in the doc entries to point towards the place where the
%% documentation should be inserted.
filter_and_fix_anno(AST, [{{function, behaviour_info, 1}, _Anno, _S, hidden, _M} | T], ModuleDoc) ->
    filter_and_fix_anno(AST, T, ModuleDoc);
filter_and_fix_anno(AST, [{{What, F, A}, _Anno, S, D, M} | T], ModuleDoc)
  when is_map(D); D =:= hidden andalso ModuleDoc =/= hidden; is_map_key(equiv, M) ->
    NewAnno =
        case What of
            function ->
                case lists:search(fun({attribute, _SpecAnno, spec, {FA, _}}) when is_tuple(FA) ->
                                          {F, A} =:= FA orelse {erlang, F, A} =:= FA;
                                     (_) ->
                                          false
                                  end, AST) of
                    {value, {attribute, SpecAnno, _, _}} ->
                        SpecAnno;
                    false ->
                        case lists:search(fun({function, _FuncAnno, FF, FA, _}) ->
                                                  {F, A} =:= {FF, FA};
                                             (_) ->
                                                  false
                                          end, AST) of
                            {value, {function, FuncAnno, _, _, _}} ->
                                FuncAnno;
                            false ->
                                io:format("~p~n",[AST]),
                                io:format("Could not find func: ~p/~p~n",[F,A]),
                                error(badarg)
                        end
                end;
           type ->
                case lists:search(fun({attribute, _TypeAnno, TO, {FA, _}}) when
                                            is_tuple(FA), TO =:= type orelse TO =:= nominal ->
                                          {F, A} =:= FA;
                                     ({attribute, _TypeAnno, TO, {Type, _, Args}}) when
                                            is_atom(Type), TO =:= type orelse TO =:= opaque orelse TO =:= nominal->
                                          {F, A} =:= {Type, length(Args)};
                                     (_) ->
                                          false
                                  end, AST) of
                    {value, {attribute, TypeAnno, _, _}} ->
                        TypeAnno;
                    false ->
                        io:format("Could not find type: ~p/~p~n",[F,A]),
                        error(badarg)
                end;
            callback ->
                case lists:search(fun({attribute, _CBAnno, callback, {FA, _}}) ->
                                          {F, A} =:= FA;
                                     (_) ->
                                          false
                                  end, AST) of
                    {value, {attribute, CBAnno, _, _}} ->
                        CBAnno;
                    false ->
                        io:format("Could not find callback: ~p/~p~n",[F,A]),
                        erl_anno:new(0)
                end
        end,
    [{{What, F, A}, NewAnno, S, D, M} | filter_and_fix_anno(AST, T, ModuleDoc)];
filter_and_fix_anno(AST, [_ | T], ModuleDoc) ->
    filter_and_fix_anno(AST, T, ModuleDoc);
filter_and_fix_anno(_, [], _ModuleDoc) ->
    [].
