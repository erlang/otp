%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%% %CopyrightEnd%
%%
-module(eep48_to_markdown).
-feature(maybe_expr, enable).
-moduledoc false.

%% This module takes care of rendering and normalization of
%% application/erlang+html style documentation.

%% It is a copy of shell_docs.erl from the erlang/otp repo that
%% has been modified to emit markdown instead of shell.

-include_lib("kernel/include/eep48.hrl").

-export([render_docs/1, render_docs/2]).
-export([render/3, render/4, render/5]).
-export([render_type/3, render_type/4, render_type/5]).
-export([render_callback/3, render_callback/4, render_callback/5]).

-export_type([chunk_elements/0, chunk_element_attr/0]).

-record(config, {docs :: docs_v1() | undefined, current = undefined }).

-define(ALL_ELEMENTS, [
    a,
    p,
    'div',
    br,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    hr,
    i,
    b,
    em,
    strong,
    pre,
    code,
    ul,
    ol,
    li,
    dl,
    dt,
    dd,
    table,
    tr,
    td
]).
%% inline elements are:
-define(INLINE, [i, b, em, strong, code, a]).
-define(IS_INLINE(ELEM),
    (((ELEM) =:= a) orelse ((ELEM) =:= code) orelse
        ((ELEM) =:= i) orelse ((ELEM) =:= em) orelse
        ((ELEM) =:= b) orelse ((ELEM) =:= strong))
).
%% non-inline elements are:
-define(BLOCK, [p, 'div', pre, br, ul, ol, li, dl, dt, dd, h1, h2, h3, h4, h5, h6, hr, table, tr, td]).
-define(IS_BLOCK(ELEM), not ?IS_INLINE(ELEM)).
-define(IS_PRE(ELEM), ((ELEM) =:= pre)).
-define(IS_HDR(ELEM),
        (((ELEM) =:= h1) orelse ((ELEM) =:= h2) orelse
         ((ELEM) =:= h3) orelse ((ELEM) =:= h4) orelse
         ((ELEM) =:= h5) orelse ((ELEM) =:= h6))).

%% If you update the below types, make sure to update the documentation in
%% erl_docgen/doc/src/doc_storage.xml as well!!!
-type docs_v1() :: #docs_v1{
    docs :: [chunk_entry()], format :: binary(), module_doc :: none | hidden | #{ binary() => chunk_elements() }
}.
-type chunk_entry() :: {
    {Type :: function | type | callback, Name :: atom(), Arity :: non_neg_integer()},
    Anno :: erl_anno:anno(),
    Sigs :: [binary()],
    Docs :: none | hidden | #{binary() => chunk_elements()},
    Meta :: #{signature := term()}
}.
-type config() :: #{}.
-type chunk_elements() :: [chunk_element()].
-type chunk_element() ::
    {chunk_element_type(), chunk_element_attrs(), chunk_elements()}
    | binary().
-type chunk_element_attrs() :: [chunk_element_attr()].
-type chunk_element_attr() :: {atom(), unicode:chardata()}.
-type chunk_element_type() :: chunk_element_inline_type() | chunk_element_block_type().
-type chunk_element_inline_type() :: a | code | em | strong | i | b.
-type chunk_element_block_type() ::
    p
    | 'div'
    | br
    | pre
    | ul
    | ol
    | li
    | dl
    | dt
    | dd
    | h1
    | h2
    | h3
    | h4
    | h5
    | h6.

-export([convert/1, convert_application/1, modules/1, guesslang/1]).
-export([convert/2, convert_application/2]).

-spec normalize(Docs) -> NormalizedDocs when
    Docs :: chunk_elements(),
    NormalizedDocs :: chunk_elements().
normalize(Docs) ->
    shell_docs:normalize(Docs).

convert_application(App) ->
    convert_application(App, [docs]).
convert_application(App, What) ->
    put(application, atom_to_list(App)),
    Modules = modules(App),
    case App of
        _ when App =:= wx ->
            %% We cannot run wx in parallel as there are docs in wx.hrl and many different
            %% modules have types defined in it.
            [try convert(M, What)
             catch E:R:ST ->
                     io:format("~p:~p:~n~p~n",[E,R,ST]),
                     erlang:raise(E,R,ST)
             end || M <- Modules];
        _ ->
            pmap(fun(M) ->
                         put(application, atom_to_list(App)),
                         try convert(M, What)
                         catch E:R:ST ->
                                 io:format("~p:~p:~n~p~n",[E,R,ST]),
                                 exit({error,E,R,ST})
                         end
                 end, Modules)
    end,
    docgen_xml_to_markdown:convert_application(App),
    ok.

pmap(Fun, Items) ->
    Parent = self(),
    Pids = [spawn_monitor(fun() -> link(Parent), Fun(I) end) || I <- Items],
    [receive
         {'DOWN',Ref,_,_,normal} -> ok;
         {'DOWN',Ref,_,_,{error,E,R,ST}} ->
             erlang:raise(E,R,ST)
     end || {_P,Ref} <- Pids].

modules(App) ->
    [list_to_atom(filename:basename(filename:rootname(File)))
     || File <- filelib:wildcard(
                  filename:join(
                    filename:dirname(code:priv_dir(App)),
                    "doc/chunks/*.chunk"))].

which(Module) ->
    case code:which(Module) of
        preloaded ->
            filename:join(["erts","preloaded","ebin",atom_to_list(Module) ++ ".beam"]);
        Path -> Path
    end.

convert(Module) ->
    convert(Module,[docs]).
convert(Module, What) ->
    io:format("Converting: ~p~n",[Module]),

    ModulePath = which(Module),

    %% Strip Docs chunk from all beam files
    {ok, _, Chunks} = beam_lib:all_chunks(ModulePath),
    beam_lib:strip(ModulePath, [ChunkId || {ChunkId, _} <- Chunks,
                               not lists:member(ChunkId,["Docs" | beam_lib:significant_chunks()])]),
    App = get_app(Module),

    {ok, SrcPath} = filelib:find_source(ModulePath),

    HasXmlSource =
        filelib:wildcard(
          path_normalize(
            filename:join([code:priv_dir(App),"..","doc","src",
                           atom_to_list(Module)++".xml*"]))) =/= [],
    case os:cmd("grep '@doc' " ++ SrcPath) of
        Res when Res =:= []; HasXmlSource ->
            convert_chunk(What, Module, ModulePath);
        _ ->
            convert_edoc(What, Module, ModulePath, SrcPath)
    end.

path_normalize(Path) ->
    filename:join(
      lists:reverse(
        lists:foldl(
          fun("..",[_ | Acc]) -> Acc;
             (E, Acc) -> [E | Acc]
          end,[], filename:split(Path)))).

convert_edoc(What, Module, ModulePath, SrcPath) ->
    case code:get_doc(Module, #{ sources => [eep48] }) of
        {ok, #docs_v1{ module_doc = hidden }} ->
            convert_chunk(What, Module, ModulePath);
        _ ->
            ok = edoc:files(
                   [SrcPath],
                   [{doclet, edoc_doclet_chunks},
                    {layout, edoc_layout_chunks},
                    {preprocess,true},
                    {includes,[filename:join(code:lib_dir(get_app(Module)),"include")]},
                    {dir,filename:join(code:lib_dir(get_app(Module)),"doc")}]),
            convert_chunk(What, Module, ModulePath)
    end.

convert_chunk(What, Module, ModulePath) ->
    case code:get_doc(Module, #{ sources => [eep48] }) of
        {ok, #docs_v1{ format = <<"application/erlang+html">>,
                       metadata = ModuleMeta,
                       module_doc = ModuleDoc, docs = Docs } = DocsV1 } ->

            put(application, get_app(Module)),
            put(module, Module),

            %% We first recompile the file in order to make sure we have the correct AST
            %% The AST may have changed due to partial .hrl files being converted already.
            [{ok, _} = c:c(Module) || lists:member(get(application),["wx"])],

            {ok, {Module,
                  [{debug_info,
                    {debug_info_v1, erl_abstract_code,
                     {AST, Meta0}}}]}} = beam_lib:chunks(ModulePath,[debug_info]),

            Meta =
                case code:which(Module) of
                    preloaded ->
                        [{cwd,"erts/preloaded/src"}|Meta0];
                    _ ->
                        Meta0
                end,


            NewFiles = convert(What,
                               #{ meta => Meta, ast => AST, docs => DocsV1 },
                               filter_and_fix_anno(expand_anno(AST), Docs, ModuleDoc)),

            %% io:format("~p~n", [AST]),
            {_, File, Anno} = lists:foldl(
                                fun({attribute, [{generated,true}|_], file, {File, Line}}, {false, _, _}) ->
                                        {true, File, Line};
                                   (_, FileAnno) when is_tuple(FileAnno) ->
                                        FileAnno;
                                   ({attribute, _, file, {File,_}}, _) ->
                                        File;
                                   ({attribute, Anno, module, _}, File) ->
                                        {false, File, Anno}
                                end, undefined, AST),
            Filename = filename:join(proplists:get_value(cwd, Meta, ""), File),

            %% These modules have a -feature macro at top that needs to
            %% be above the -moduledoc line.
            %% So we adjust the docs down one step.
            ModuleDocLine = case lists:member(Module, [init, asn1ct, erl_features, erl_lint,
                                                       inet_epmd_dist, inet_tcp_dist,
                                                       inet_epmd_socket, ?MODULE,
                                                       inet_tls_dist, ssl_handshake,
                                                       dialyzer_typesig, beam_doc
                                                      ]) of
                                true -> erl_anno:line(Anno) + 1;
                                false -> erl_anno:line(Anno)
                            end,
            {BeforeModule, AfterModule} = lists:split(
                                            ModuleDocLine,
                                            case maps:get(Filename, NewFiles, undefined) of
                                                undefined ->
                                                    {ok, Bin} = file:read_file(Filename),
                                                    string:split(Bin, "\n", all);
                                                F -> F
                                            end),

            NewFilesWithModuleDoc =
                NewFiles#{ Filename => BeforeModule ++
                               case lists:member(docs,What) of
                                   true ->
                                       convert_moduledoc(ModuleDoc, ModuleMeta, DocsV1);
                                   false ->
                                       []
                               end
                           ++ generate_skipped_callbacks(maps:get(skipped, NewFiles, []), NewFiles)
                           ++ AfterModule
                         },


            %% io:format("~p~n",[hd(NewFileBin)]),
            %%    io:format("~p~n",[NewFiles]),
            %%    ok = nok,
            %% io:format("~p~n",[hd(lists:reverse(NewFileBin))]),
            %%    io:format("~p~n",[AST]),
            %% {AST, Meta}.
            %%    [ io:format("~ts:~n~ts~n", [Key, ""]) || Key := Value <- NewFiles, not is_atom(Key)],
            _ = [ begin
                      io:format("\tUpdated ~ts~n",[Key]),
                      ok = file:write_file(Key, formatter(Key, lists:flatten(lists:join($\n,Value))))
                  end || Key := Value <- NewFilesWithModuleDoc, not is_atom(Key)],
            ok;
        {ok, #docs_v1{ format = <<"text/markdown">> }} ->
            {ok, Module, Chunks} = beam_lib:all_chunks(ModulePath),
            {ok, NewBeamFile} = beam_lib:build_module(proplists:delete("Docs", Chunks)),
            file:write_file(ModulePath, NewBeamFile),
            convert(Module, What);
        Error ->
            io:format("Error: ~p~n",[Error]),
            error(badarg)
    end.

convert(What, Files, Docs) ->
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
    convert([], [], lists:reverse(Prev ++ Acc), Files#{ what => What }).
convert([], [], [], Files) ->
    %% When there are no documented functions in module, eg. gen_fsm
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
convert(Lines, Acc, [{Kind, Anno, Slogan, _D, _Meta} = E | T] = Docs, Files) ->
    Skip = lists:member(Kind, [{function,port_info,2},{function,system_info,1}]) andalso
        get(module) =:= erlang,
    case erl_anno:file(Anno) =:= maps:get(current, Files, undefined) of
        _ when Skip ->
            {K, F, A} = Kind,
            io:format("Skipping ~p ~p/~p~n",[K,F,A]),
            convert(Lines, Acc, T, Files);
        true ->
            {Before, After} = lists:split(erl_anno:line(Anno)-1, Lines),
            {{Kind, Anno, Slogan, D, Meta}, NewT} = maybe_merge_entries(E, T),
            DocString = case lists:member(docs, maps:get(what, Files)) of
                            true ->
                                generate_doc_attributes(D, Kind, Slogan, Meta,
                                                        Files#{ current => E });
                            false ->
                                []
                        end,
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
                        case lists:member(specs, maps:get(what, Files)) of
                            true ->
                                generate_spec(E, Files);
                            false ->
                                []
                        end
                end,
            convert(Before, DocString ++ SpecString ++ After ++ Acc, NewT, Files);
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

%% We merge any duplicate entries into the same "-doc" entry
maybe_merge_entries({Kind, Anno, Slogan, #{ <<"en">> := D }, Meta},
                    [{Kind, _, _, #{ <<"en">> := D2 }, M2} | T]) ->
    maybe_merge_entries(
      {Kind, Anno, Slogan, #{ <<"en">> => D2 ++ D },
       maps:merge_with(fun(since, V1, V2) ->
                               unicode:characters_to_binary(
                                 lists:join(", ",
                                            lists:usort(re:split(V1,", ?") ++ re:split(V2,", ?"))));
                          (_, _, V) ->
                               V
                       end, Meta, M2)}, T);
maybe_merge_entries(E, T) ->
    {E, T}.

%% strip_beh_info(Str) ->
%%     re:replace(
%%       re:replace(Str, "\n-export\\(\\[behaviour_info/1\\]\\)\\.\n","",[global,unicode]),
%%       "\nbehaviour_info\\((.|\n)*\\.\n","",[global,unicode,ungreedy]).

generate_doc_attributes(D, {Kind, _, _}, Slogan, Meta, Files) ->
    DocString =
        case D of
            #{ <<"en">> := ErlangHtml } when not is_map_key(equiv, Meta) ->
                Prototype =
                    case is_map_key(signature, Meta) orelse Kind =/= function of
                        true ->
                            [];
                        false ->
                            [re:replace(Slogan,"\n\\s+"," ",[global,unicode]),"\n\n"]
                    end,
                [{doc,Prototype ++ render_docs(normalize(ErlangHtml), init_config(maps:get(docs, Files), Files))}];
            D when D =:= #{}, is_map_key(equiv, Meta) ->
                [];
            D when D =:= #{} ->
                [];
            hidden ->
                ["-doc false."]
        end,
    DocString ++ meta(Meta).

generate_skipped_callbacks(CBs, Files) ->
    generate_skipped_callbacks(CBs, CBs, Files).
generate_skipped_callbacks([{{callback, F, A}, _, Slogan, D, Meta} | T], AllCBs, Files) ->
    [_, Call] = string:split(Slogan,":"),
    [_, Args] = string:split(Call,"("),
    CallbackProto = lists:flatten(
                      io_lib:format(
                        "-callback ~ts(~ts",
                        [io_lib:write_atom(F),Args])),
    {ok, Toks, _} = erl_scan:string(CallbackProto ++ ".",{1, 1}),
    {Callback, NewD} =
        try
            {ok,{attribute, _, callback, {{F,A}, _}}} = erl_parse:parse_form(Toks),

            {Types, DwithoutTypes} =
                case maps:find(equiv, Meta) of
                    error ->
                        case D of
                            #{ <<"en">> := [{ul,[{class,<<"types">>}],Ts} | Rest] } ->
                                {Ts, #{ <<"en">> => Rest }};
                            #{ <<"en">> := _ } ->
                                {[], D}
                        end;
                    {ok, Equiv} when D =:= #{} ->
                        {Equiv, _, _, EquivD, _} = lists:keyfind(Equiv, 1, AllCBs),
                        case EquivD of
                            #{ <<"en">> := [{ul,[{class,<<"types">>}],Ts} | _] } ->
                                {Ts, D};
                            #{ <<"en">> := _ } ->
                                {[], D}
                        end
                end,
            case Types of
                [] ->
                    {io_lib:format("~ts.",[CallbackProto]),
                     DwithoutTypes};
                Types ->
                    {io_lib:format(
                       "~ts when ~ts.",
                       [CallbackProto,
                        lists:join(", ", munge_types(Types))
                       ]), DwithoutTypes}
            end
        catch E:R:ST ->
                io:format("Failed to parse: ~p~n",[Toks]),
                erlang:raise(E,R,ST)
        end,
    generate_doc_attributes(NewD, callback, Slogan, Meta, Files) ++
        [pp(Callback), ""]
        ++ generate_skipped_callbacks(T, AllCBs, Files);
generate_skipped_callbacks([], _AllCBs, _Files) ->
    [].

%% munge_types([{li,Attr,C},{li,_,[<<" "/utf8,_/binary>>|_] = LIC}|T]) ->
%%     %% If the next li starts with a nbsp we join it to the previous list item as
%%     %% it is a continuation of it.
%%     munge_types([{li,Attr,C ++ LIC}|T]);
munge_types([{li,_,C}|T]) ->
    NoNBSP = re:replace(strip_tags(C),"\\h"," ",[unicode,global]),
    [Body | Variables] = lists:reverse(string:split(NoNBSP, " = ", all)),
    [[Var, " :: ", Body] || Var <- lists:reverse(Variables)] ++ munge_types(T);
munge_types([]) ->
    [].

generate_spec({{function, F, A}, _, Slogan, D, Meta}, Files) ->
    [_, Args] = string:split(Slogan, "("),
    Return = case string:find(Slogan,"->") of
                 nomatch -> " -> term()";
                 _ -> ""
             end,
    SpecProto = lists:flatten(
                      io_lib:format(
                        "-spec ~ts(~ts",
                        [io_lib:write_atom(F),[Args, Return]])),
    try
        {ok, Toks, _} = erl_scan:string(SpecProto ++ ".",{1, 1}),
        {ok,{attribute, _, spec, {{F, A}, _}}} = erl_parse:parse_form(Toks),

        Types =
            case maps:find(equiv, Meta) of
                error ->
                    case D of
                        #{ <<"en">> := [{ul,[{class,<<"types">>}],Ts} | _Rest] } ->
                            Ts;
                        #{ <<"en">> := _ } ->
                            []
                    end;
                {ok, Equiv} when D =:= #{} ->
                    #docs_v1{ docs = Ds } = maps:get(docs, Files),
                    {Equiv, _, _, EquivD, _} = lists:keyfind(Equiv, 1, Ds),
                    case EquivD of
                        #{ <<"en">> := [{ul,[{class,<<"types">>}],Ts} | _] } ->
                            Ts;
                        #{ <<"en">> := _ } ->
                            []
                    end
            end,
        Spec =
            case Types of
                [] ->
                    io_lib:format("~ts.",[SpecProto]);
                Types ->
                    io_lib:format(
                      "~ts when ~ts.",
                      [SpecProto,
                       lists:join(",\n   ", munge_types(Types))
                      ])
            end,
        validate_spec(get(module),[lists:flatten(Spec)], F, A)
    catch E:R:ST when E =/= throw ->
            io:format("Failed to parse: ~ts~n  ~p:~p\n  ~p~n",[SpecProto,E,R,ST]),
            %% erlang:raise(E,R,ST),
            [["%% -spec ",lists:join(["\n%% "], string:split(Slogan,"\n",all))]]
    end;
generate_spec(_E, _Files) ->
    "".

pp(String) ->
    maybe
        {ok, T, _} ?= erl_scan:string(lists:flatten(String), {0,1}),
        {ok, {attribute, _, _, _} = Attr} ?= erl_parse:parse_form(T),
        [string:trim(lists:flatten(erl_pp:attribute(Attr)))]
    else
        {ok, {function, _, _, _, _} = Function} ->
            [string:trim(lists:flatten(erl_pp:function(Function)))];
        Else ->
            io:format("Failed to parse: ~ts~n ~p",[String, Else]),
            error(Else)
    end.

validate_spec(Mod, Spec, Name, Arity) ->
    maybe
        {ok, Toks, _} ?= erl_scan:string(lists:flatten(Spec), {2,1}),
        {ok, Form} ?= erl_parse:parse_form(Toks),
        AST = [{attribute,{0,2},module,a},
               {attribute,{1,2},export,[{Name, Arity}]},
               {function,{1,1},
                Name, Arity,
                [{clause,{1,1},lists:duplicate(Arity,{var,{1,5},'_'}),
                  [],[{atom,{1,11},ok}]}]},
               Form],
        {ok, ValidSpec} ?=
            case erl_lint:module(AST,"") of
                {ok,[]} ->
                    {ok, Spec};
                Else ->
                    try
                        NewSpec =
                            case Else of
                                {error, [{_, Errors}], _Warnings} ->
                                    %% io:format("Errors: ~p~n",[lists:reverse(lists:sort(Errors))]),
                                    lists:foldl(
                                      fun({{Line, Col},erl_lint,{singleton_typevar,SName}}, SpecLines) ->
                                              {LinesBefore,[Curr | LinesAfter]} =
                                                  lists:split(Line - 2, SpecLines),
                                              NameLen = string:length(atom_to_list(SName)),
                                              Before = string:slice(Curr, 0, Col - 1 + NameLen),
                                              After = string:slice(Curr, Col + NameLen -1),
                                              LinesBefore ++ [[Before,"::term()",After] | LinesAfter];
                                         %% ({_, erl_lint, {undefined_type,_}}, SpecLines) ->
                                         %%      SpecLines;
                                         (_Error, SpecLines) ->
                                              SpecLines
                                      end, string:split(Spec, "\n", all),
                                      lists:reverse(lists:sort(Errors)));
                                _ ->
                                    string:split(Spec, "\n", all)
                            end,
                        PPSpec = pp(lists:join($\n,NewSpec)),
                        case c(Mod, [{append, PPSpec}]) of
                            {ok, _} ->
                                {ok, PPSpec};
                            _Error ->
                                %% throw(_Error),
                                {ok, [unicode:characters_to_binary(
                                   re:replace(PPSpec, "^", "%% ", [multiline,global,unicode]))]}
                        end
                    catch C:Error when C =/= throw ->
                            io:format("~p~n",[Error]),
                            {ok, [unicode:characters_to_binary(
                               re:replace(Spec, "^", "%% ", [multiline,global,unicode]))]}
                    end
            end,
        ValidSpec
    else
        _ ->
            [unicode:characters_to_binary(
               re:replace(Spec, "^", "%% ", [multiline,global,unicode]))]
    end.

get_app(Module) ->
    case code:which(Module) of
        preloaded ->
            "erts";
        Path ->
            case lists:reverse(filename:split(Path)) of
                [_, _, "examples" | _] -> "megaco";
                [_Module, _Ebin, App | _] -> App
            end
    end.

%% Convert module documentation
convert_moduledoc(#{ <<"en">> := ModuleHeader }, Meta, Docs) ->
    String = render_docs(normalize(ModuleHeader), init_config(Docs, #{})),
    FixDiameterDepsBug = re:replace(String, "```text\n(-include_lib\\(\"diameter/include/diameter.hrl\"\\).)\n```", "\n    \\1\n"),
    [{moduledoc,FixDiameterDepsBug} | modulemeta(Meta)];
convert_moduledoc(#{}, Meta, _) ->
    [{moduledoc,""} | modulemeta(Meta)];
convert_moduledoc(hidden, Meta, _) ->
    ["-moduledoc false." | modulemeta(Meta)].

formatter(String) ->
    unicode:characters_to_binary(
      case {os:getenv("FORMAT_MD"),os:find_executable("npx")} of
          {"true",Npx} when Npx =/= false ->
              run_formatter("",String);
          _ ->
              String
      end).
formatter(Module, String) ->

    Text =
        case {os:getenv("FORMAT_MD"),os:find_executable("npx")} of
            {"true",Npx} when Npx =/= false ->
                Header = "# FORMAT HEADER\n\n",

                FormatString =
                    lists:foldl(
                      fun({_, Doc}, Acc) ->
                              [[Header, Doc, "\n\n"]|Acc];
                         (_, Acc) ->
                              Acc
                      end, [], String),

                FormattedText = run_formatter(Module, lists:reverse(FormatString)),
                [<<>>|Split] = string:split(FormattedText, string:trim(Header), all),
                {[], FormattedString} =
                    lists:foldl(fun({Type, _Doc}, {[FormattedDoc|T], Acc}) ->
                                        {T, [{Type, FormattedDoc} | Acc]};
                                   (Else, {FormattedDoc, Acc}) when not is_tuple(Else)->
                                        {FormattedDoc, [Else | Acc]}
                                end, {Split,[]}, String),
                lists:reverse(FormattedString);
            _ ->
                String
        end,
    unicode:characters_to_binary(
      lists:map(fun({doc, Doc}) ->
                        doc(Doc);
                   ({moduledoc, Doc}) ->
                        moduledoc(Doc);
                   (Else) ->
                        Else
                end, Text)).

run_formatter(Module, String) ->
    Filename = string:trim(os:cmd("mktemp --suffix=."++filename:basename(Module)++".md")),
    ok = file:write_file(Filename, unicode:characters_to_binary(String)),
    os:cmd("npx prettier --parser markdown --prose-wrap always --write " ++ Filename),
    {ok, FormattedText} = file:read_file(Filename),
    %% file:delete(Filename),
    FormattedText.

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

%% Deprecations are handled using the -deprecated attribute in OTP
meta(#{ deprecated := _} = Meta) ->
    meta(maps:remove(deprecated, Meta));
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

filter_and_fix_anno(AST, [{{function, behaviour_info, 1}, _Anno, _S, hidden, _M} | T], ModuleDoc) ->
    filter_and_fix_anno(AST, T, ModuleDoc);
filter_and_fix_anno(AST, [{{What, F, A}, _Anno, S, D, M} | T], ModuleDoc)
  when is_map(D); D =:= hidden andalso ModuleDoc =/= hidden; is_map_key(equiv, M) ->
    NewAnno =
        case What of
            function ->
                case lists:search(fun({attribute, _SpecAnno, spec, {FA, _}}) when is_tuple(FA) ->
                                          {F, A} =:= FA orelse {erlang, F, A} =:= FA;
                                     %% ({attribute, _, spec, {Spec, _}}) when is_atom(Spec) ->
                                     %%      {F, A} =:= {Spec, 0};
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
                                            is_tuple(FA), TO =:= type orelse TO =:= opaque ->
                                          {F, A} =:= FA;
                                     ({attribute, _TypeAnno, TO, {Type, _, Args}}) when
                                            is_atom(Type), TO =:= type orelse TO =:= opaque ->
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

expand_anno(AST) ->
    {NewAST, _} =
        lists:mapfoldl(fun F({attribute, _, file, {NewFile, _}} = E, File) when NewFile =/= File ->
                               F(E, NewFile);
                           F(E, File) ->
                               {setelement(2, E, erl_anno:set_file(File, element(2, E))), File}
                       end, undefined, AST),
    %% io:format("NewAST: ~p~n",[NewAST]),
    NewAST.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the function documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render(Module, Function, Docs) -> Res when
    Module :: module(),
    Function :: atom(),
    Docs :: docs_v1(),
    Res :: unicode:chardata() | {error, function_missing}.
render(_Module, Function, #docs_v1{} = D) ->
    render(_Module, Function, D, #{});
render(Module, Function, Arity) when is_integer(Arity) ->
    {ok, D} = code:get_doc(Module),
    render(Module, Function, Arity, D).

-spec render
    (Module, Function, Docs, Config) -> Res when
        Module :: module(),
        Function :: atom(),
        Docs :: docs_v1(),
        Config :: config(),
        Res :: unicode:chardata() | {error, function_missing};
    (Module, Function, Arity, Docs) -> Res when
        Module :: module(),
        Function :: atom(),
        Arity :: arity(),
        Docs :: docs_v1(),
        Res :: unicode:chardata() | {error, function_missing}.
render(Module, Function, #docs_v1{docs = Docs} = D, Config) when
    is_atom(Module), is_atom(Function), is_map(Config)
->
    render_function(
        lists:filter(
            fun
                ({{function, F, _}, _Anno, _Sig, Doc, _Meta}) when Doc =/= none ->
                    F =:= Function;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    );
render(_Module, Function, Arity, #docs_v1{} = D) ->
    render(_Module, Function, Arity, D, #{}).

-spec render(Module, Function, Arity, Docs, Config) -> Res when
    Module :: module(),
    Function :: atom(),
    Arity :: arity(),
    Docs :: docs_v1(),
    Config :: config(),
    Res :: unicode:chardata() | {error, function_missing}.
render(Module, Function, Arity, #docs_v1{docs = Docs} = D, Config) when
    is_atom(Module), is_atom(Function), is_integer(Arity), is_map(Config)
->
    render_function(
        lists:filter(
            fun
                ({{function, F, A}, _Anno, _Sig, Doc, _Meta}) when Doc =/= none ->
                    F =:= Function andalso A =:= Arity;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the type documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render_type(Module, Type, Docs) -> Res when
    Module :: module(),
    Type :: atom(),
    Docs :: docs_v1(),
    Res :: unicode:chardata() | {error, type_missing}.
render_type(Module, Type, D = #docs_v1{}) ->
    render_type(Module, Type, D, #{}).

-spec render_type
    (Module, Type, Docs, Config) -> Res when
        Module :: module(),
        Type :: atom(),
        Docs :: docs_v1(),
        Config :: config(),
        Res :: unicode:chardata() | {error, type_missing};
    (Module, Type, Arity, Docs) -> Res when
        Module :: module(),
        Type :: atom(),
        Arity :: arity(),
        Docs :: docs_v1(),
        Res :: unicode:chardata() | {error, type_missing}.
render_type(_Module, Type, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{type, T, _}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Type;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    );
render_type(_Module, Type, Arity, #docs_v1{} = D) ->
    render_type(_Module, Type, Arity, D, #{}).

-spec render_type(Module, Type, Arity, Docs, Config) -> Res when
    Module :: module(),
    Type :: atom(),
    Arity :: arity(),
    Docs :: docs_v1(),
    Config :: config(),
    Res :: unicode:chardata() | {error, type_missing}.
render_type(_Module, Type, Arity, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{type, T, A}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Type andalso A =:= Arity;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the callback documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render_callback(Module, Callback, Docs) -> Res when
    Module :: module(),
    Callback :: atom(),
    Docs :: docs_v1(),
    Res :: unicode:chardata() | {error, callback_missing}.
render_callback(_Module, Callback, #docs_v1{} = D) ->
    render_callback(_Module, Callback, D, #{}).

-spec render_callback
    (Module, Callback, Docs, Config) -> Res when
        Module :: module(),
        Callback :: atom(),
        Docs :: docs_v1(),
        Config :: config(),
        Res :: unicode:chardata() | {error, callback_missing};
    (Module, Callback, Arity, Docs) -> Res when
        Module :: module(),
        Callback :: atom(),
        Arity :: arity(),
        Docs :: docs_v1(),
        Res :: unicode:chardata() | {error, callback_missing}.
render_callback(_Module, Callback, Arity, #docs_v1{} = D) ->
    render_callback(_Module, Callback, Arity, D, #{});
render_callback(_Module, Callback, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{callback, T, _}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Callback;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

-spec render_callback(Module, Callback, Arity, Docs, Config) -> Res when
    Module :: module(),
    Callback :: atom(),
    Arity :: arity(),
    Docs :: docs_v1(),
    Config :: config(),
    Res :: unicode:chardata() | {error, callback_missing}.
render_callback(_Module, Callback, Arity, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{callback, T, A}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Callback andalso A =:= Arity;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

%% Get the docs in the correct locale if it exists.
-spec get_local_doc(_, 'hidden' | 'none' | map(), #docs_v1{}) -> chunk_elements().
get_local_doc(MissingMod, Docs, D) when is_atom(MissingMod) ->
    get_local_doc(atom_to_binary(MissingMod), Docs, D);
get_local_doc({F, A}, Docs, D) ->
    get_local_doc(
        unicode:characters_to_binary(
            io_lib:format("~tp/~p", [F, A])
        ),
        Docs,
        D
    );
get_local_doc({_Type, F, A}, Docs, D) ->
    get_local_doc({F, A}, Docs, D);
get_local_doc(_Missing, #{<<"en">> := Docs}, D) ->
    %% English if it exists
    normalize_format(Docs, D);
get_local_doc(_Missing, ModuleDoc, D) when map_size(ModuleDoc) > 0 ->
    %% Otherwise take first alternative found
    normalize_format(maps:get(hd(maps:keys(ModuleDoc)), ModuleDoc), D);
get_local_doc(Missing, hidden, _D) ->
    [
        {p, [], [
            <<"The documentation for ">>,
            Missing,
            <<
                " is hidden. This probably means that it is internal "
                "and not to be used by other applications."
            >>
        ]}
    ];
get_local_doc(_Missing, None, _D) when None =:= none; None =:= #{} ->
    [].

-spec normalize_format(chunk_elements(), #docs_v1{}) -> chunk_elements().
normalize_format(Docs, #docs_v1{format = ?NATIVE_FORMAT}) ->
    normalize(Docs);
normalize_format(Docs, #docs_v1{format = <<"text/", _/binary>>}) when is_binary(Docs) ->
    [{pre, [], [Docs]}].

%%% Functions for rendering reference documentation
-spec render_function([chunk_entry()], #docs_v1{}, map()) ->
    unicode:chardata() | {'error', 'function_missing'}.
render_function([], _D, _Config) ->
    {error, function_missing};
render_function(FDocs, #docs_v1{docs = Docs} = D, Config) ->
    Grouping =
        lists:foldl(
            fun
                ({_Group, _Anno, _Sig, _Doc, #{equiv := Group}} = Func, Acc) ->
                    Members = maps:get(Group, Acc, []),
                    Acc#{Group => [Func | Members]};
                ({Group, _Anno, _Sig, _Doc, _Meta} = Func, Acc) ->
                    Members = maps:get(Group, Acc, []),
                    Acc#{Group => [Func | Members]}
            end,
            #{},
            lists:sort(FDocs)
        ),
    lists:map(
        fun({Group, Members}) ->
            lists:map(
                fun(_Member = {_, _, _, hidden, _}) ->
                        {error,hidden};
                    (Member = {_, _, _, Doc, _}) ->
                    Sig = render_signature(Member),
                    LocalDoc =
                        if
                            Doc =:= #{} ->
                                case lists:keyfind(Group, 1, Docs) of
                                    false ->
                                        get_local_doc(Group, none, D);
                                    {_, _, _, GroupDoc, _} ->
                                        get_local_doc(Group, GroupDoc, D)
                                end;
                            true ->
                                get_local_doc(Group, Doc, D)
                        end,
                    render_headers_and_docs(
                        [Sig], LocalDoc, D, Config
                    )
                end,
                Members
            )
        end,
        maps:to_list(Grouping)
    ).

%% Render the signature of either function, type, or anything else really.
-spec render_signature(chunk_entry()) -> chunk_elements().
render_signature({{_Type, _F, _A}, _Anno, _Sigs, _Docs, #{signature := Specs} = Meta}) ->
    lists:flatmap(
        fun(ASTSpec) ->
            PPSpec = erl_pp:attribute(ASTSpec, [{encoding, utf8}]),
            Spec =
                case ASTSpec of
                    {_Attribute, _Line, opaque, _} ->
                        %% We do not want show the internals of the opaque type
                        hd(string:split(PPSpec, "::"));
                    _ ->
                        trim_spec(PPSpec)
                end,
            BinSpec =
                unicode:characters_to_binary(
                    string:trim(Spec, trailing, "\n")
                ),
            [
                {pre, [], BinSpec},
                {hr, [], []}
                | render_meta(Meta)
            ]
        end,
        Specs
    );
render_signature({{_Type, _F, _A}, _Anno, Sigs, _Docs, Meta}) ->
    [{pre, [], Sigs}, {hr, [], []} | render_meta(Meta)].

-spec trim_spec(unicode:chardata()) -> unicode:chardata().
trim_spec(Spec) ->
    unicode:characters_to_binary(
        string:trim(
            lists:join($\n, trim_spec(string:split(Spec, "\n", all), 0)),
            trailing,
            "\n"
        )
    ).
-spec trim_spec([unicode:chardata()], non_neg_integer()) -> unicode:chardata().
trim_spec(["-spec " ++ Spec | T], 0) ->
    [Spec | trim_spec(T, 6)];
trim_spec([H | T], N) ->
    case re:run(H, io_lib:format("(\\s{~p}\\s+)when", [N]), [{capture, all_but_first}]) of
        {match, [{0, Indent}]} ->
            trim_spec([H | T], Indent);
        nomatch ->
            case string:trim(string:slice(H, 0, N), both) of
                "" ->
                    [
                        re:replace(string:slice(H, N, infinity), "  ", " ", [global])
                        | trim_spec(T, N)
                    ];
                _ ->
                    [re:replace(H, "  ", " ", [global]) | trim_spec(T, N)]
            end
    end;
trim_spec([], _N) ->
    [].

-spec render_meta(map()) -> chunk_elements().
render_meta(Meta) ->
    case
        lists:flatmap(
            fun
                ({since, Vsn}) ->
                    [{em, [], <<"Since:">>}, <<" ">>, Vsn];
                ({deprecated, Depr}) ->
                    [{em, [], <<"Deprecated: ">>}, <<" ">>, Depr];
                (_) ->
                    []
            end,
            maps:to_list(Meta)
        )
    of
        [] ->
            [];
        Docs ->
            Docs
    end.

-spec render_headers_and_docs([chunk_elements()], chunk_elements(), #docs_v1{}, map()) ->
    unicode:chardata().
render_headers_and_docs(Headers, DocContents, D, Config) ->
    render_headers_and_docs(Headers, DocContents, init_config(D, Config)).
-spec render_headers_and_docs([chunk_elements()], chunk_elements(), #config{}) ->
    unicode:chardata().
render_headers_and_docs(Headers, DocContents, #config{} = Config) ->
    [
        render_docs(
            lists:flatmap(
                fun(Header) ->
                    [{br, [], []}, Header]
                end,
                Headers
            ),
            Config
        ),
        "\n",
        render_docs(DocContents, 0, Config)
    ].

-spec render_typecb_docs([TypeCB] | TypeCB, #config{}) ->
    unicode:chardata() | {'error', 'type_missing'}
when
    TypeCB :: {
        {type | callback, Name :: atom(), Arity :: non_neg_integer()},
        Encoding :: binary(),
        Sig :: [binary()],
        none | hidden | #{binary() => chunk_elements()}
    }.
render_typecb_docs([], _C) ->
    {error, type_missing};
render_typecb_docs(TypeCBs, #config{} = C) when is_list(TypeCBs) ->
    [render_typecb_docs(TypeCB, C) || TypeCB <- TypeCBs];
render_typecb_docs({_F, _, _Sig, hidden, _Meta} = _TypeCB, #config{docs = _D} = _C) ->
    {error, hidden};
render_typecb_docs({F, _, _Sig, Docs, _Meta} = TypeCB, #config{docs = D} = C) ->
    render_headers_and_docs(render_signature(TypeCB), get_local_doc(F, Docs, D), C).
-spec render_typecb_docs(chunk_elements(), #docs_v1{}, _) ->
    unicode:chardata() | {'error', 'type_missing'}.
render_typecb_docs(Docs, D, Config) ->
    render_typecb_docs(Docs, init_config(D, Config)).

%%% General rendering functions
render_docs(DocContents) ->
    erase(module),
    formatter(render_docs(DocContents, init_config(#docs_v1{ docs = [] }, #{}))).
-spec render_docs([chunk_element()], #config{}) -> unicode:chardata().
render_docs(DocContents, #config{} = Config) ->
    render_docs(DocContents, 0, Config);
render_docs(DocContents, #docs_v1{} = D) ->
    render_docs(DocContents, init_config(D, #{})).
-spec render_docs([chunk_element()], 0, #config{}) -> unicode:chardata().
render_docs(DocContents, Ind, D = #config{}) when is_integer(Ind) ->
    try
        [put(module,'') || get(module) == undefined],
        {Doc, _} = trimnl(render_docs(preprocess_docs(DocContents, D), [], 0, Ind, D)),
        unicode:characters_to_binary(Doc)
    catch throw:R:ST ->
            io:format("Failed to render: ~tp~n",[R]),
            erlang:raise(throw,R,ST);
          E:R:ST ->
            io:format("Failed to render: ~tp~n",[DocContents]),
            erlang:raise(E,R,ST)
    end.

%% Merge any anchor with its header
%% preprocess_docs([{Hdr,Attr,C},{a,[{id,_}] = Id,[]}|T], D) when ?IS_HDR(Hdr) ->
%%     preprocess_docs([{Hdr,Attr ++ Id, C} | T], D);
%% preprocess_docs([{a,[{id,_}] = Id,[]},{Hdr,Attr,C}|T], D) when ?IS_HDR(Hdr) ->
%%     preprocess_docs([{Hdr,Attr ++ Id, C} | T], D);
preprocess_docs([{a,[{id,_Id}|_] = AAttr,[]},{Tag,PAttr,C}|T], D)
  when Tag =:= pre; Tag =:= em; Tag =:= table; Tag =:= code; Tag =:= img ->
    preprocess_docs([{Tag, AAttr ++ PAttr, C}|T], D);
preprocess_docs([{Tag,_,_} = H,{a,[{id,_Id}|_],[]} = A|T],D) when ?IS_HDR(Tag) ->
    preprocess_docs([A, H | T], D);
preprocess_docs([{a,[{id,Id}|_],[]} = A,{Tag,_,Name} = H|T],D) when ?IS_HDR(Tag) ->
    case string:equal(render_elink_anchor(Id), render_elink_anchor(Name)) of
        true ->
            preprocess_docs([H|T], D);
        false ->
            [A | preprocess_docs([H|T], D)]
    end;
preprocess_docs([{a,[{id,Id}] = Attr,[]}| T],
                #config{ current = {{function,Function,Arity},_,_,_,_} } = D) ->
    maybe
        %% Remove any anchor that is just function-arity
        [FunctionString, ArityString] ?= string:split(Id,"-",all),
        Arity ?= catch binary_to_integer(ArityString),
        true ?= is_integer(Arity),
        Function ?= binary_to_atom(FunctionString),
        preprocess_docs(T, D)
    else
        _ ->
            [{a, Attr, []} | preprocess_docs(T, D)]
    end;
preprocess_docs([{Tag,Attr,C}|T], D) ->
    [{Tag,proplists:delete(ghlink,Attr),preprocess_docs(C, D)}|preprocess_docs(T, D)];
preprocess_docs([Text|T], D) when is_binary(Text) ->
    [Text | preprocess_docs(T,D)];
preprocess_docs([], _) ->
    [].


-spec init_config(#docs_v1{} | undefined, _) -> #config{}.
init_config(D, Config) ->
    #config{docs = D, current = maps:get(current, Config, undefined) }.

-spec render_docs(
    Elems :: [chunk_element()],
    Stack :: [chunk_element_type()],
    non_neg_integer(),
    non_neg_integer(),
    #config{}
) ->
    {unicode:chardata(), non_neg_integer()}.
render_docs(Elems, State, Pos, Ind, D) when is_list(Elems) ->
    lists:mapfoldl(
        fun(Elem, P) ->
            render_docs(Elem, State, P, Ind, D)
        end,
        Pos,
        Elems
    );
render_docs(Elem, State, Pos, Ind, D) ->
    %    io:format("Elem: ~p (~p) (~p,~p)~n",[Elem,State,Pos,Ind]),
    render_element(Elem, State, Pos, Ind, D).

%%% The function is the main element rendering function
%%%
%%% Elem: The current element to process
%%% Stack: A stack of element names to see where we are in the dom
%%% Pos: The current print position on the current line
%%% Ind: How much the text should be indented after a newline
%%% Config: The renderer's configuration
%%%
%%% Each element is responsible for putting new lines AFTER itself
%%% The indents are done either by render_words when a newline happens
%%% or when a new element is to be rendered and Pos < Ind.
%%%
%%% Any block elements (i.e. p, ul, li etc) are responsible for trimming
%%% extra new lines. eg. <ul><li><p>content</p></li></ul> should only
%%% have two newlines at the end.
-spec render_element(
    Elem :: chunk_element(),
    Stack :: [chunk_element_type()],
    Pos :: non_neg_integer(),
    Indent :: non_neg_integer(),
    Config :: #config{}
) ->
    {unicode:chardata(), Pos :: non_neg_integer()}.

%% render_element({IgnoreMe,_,Content}, State, Pos, Ind,D)
%%   when IgnoreMe =:= a ->
%%     render_docs(Content, State, Pos, Ind,D);

%% Catch h* before the padding is done as they reset padding
render_element({Tag = h1, Attr, Content}, State, 0 = Pos, _Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["# ", Docs, ial(Attr)], NewPos});
render_element({Tag = h2, Attr, Content}, State, 0 = Pos, _Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["## ", Docs, ial(Attr)], NewPos});
render_element({Tag = h3, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["### ", Docs, ial(Attr)], NewPos});
render_element({Tag = h4, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["#### ", Docs, ial(Attr)], NewPos});
render_element({Tag = h5, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["##### ", Docs, ial(Attr)], NewPos});
render_element({Tag = h6, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["###### ", Docs, ial(Attr)], NewPos});
render_element({pre, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    %% We pad `pre` with two newlines if the previous section did not indent the region.
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n\n", Docs], NewPos};
render_element({br, _Attr, _Content}, [td|_State], Pos, _Ind, _D)  ->
    {" ", Pos + 1};
render_element({br, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["  \n", Docs], NewPos};
render_element({p, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n\n", Docs], NewPos};
render_element({Elem, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind, ?IS_BLOCK(Elem) ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n", Docs], NewPos};
render_element({'div', [{class, What}], Content}, State, Pos, Ind, D) ->
    Type = case What of
               <<"warning">> -> What;
               <<"error">> -> What;
               <<"note">> -> <<"info">>;
               <<"change">> -> <<"info">>;
               <<"do">> -> <<"tip">>;
               <<"dont">> -> <<"error">>
           end,
    Title = unicode:characters_to_binary([string:titlecase(What), " ",ial([{class,Type}])]),
    {Header, 0} = render_element({h4, [], [Title]}, State, Pos, Ind, D),
    {Docs, 0} = render_element({'div', [], Content}, ['div' | State], 0, 0, D),
    trimnlnl([pad(Ind - Pos), "> ", string:trim(Header), "\n",
              [[pad(Ind), string:trim(["> ",Line]),"\n"] || Line <- string:split([trim(Docs)],"\n",all)]]);
render_element({Tag, _Attr, Content}, State, Pos, Ind, D) when Tag =:= p; Tag =:= 'div' ->
    trimnlnl(render_docs(Content, [Tag | State], Pos, Ind, D));
%% render_element({a, [{id,_Id}|_], []} = A, State, Pos, Ind, D) when Pos > 0 ->
%%     {Docs, NewPos} = render_element(A, State, 0, Ind, D),
%%     {["\n",Docs], NewPos};
render_element(Elem, State, Pos, Ind, D) when Pos < Ind ->
    {Docs, NewPos} = render_element(Elem, State, Ind, Ind, D),
    {[pad(Ind - Pos), Docs], NewPos};
render_element({a, Ids, []}, _State, Pos, _Ind, _D) ->
    trimnl({[["[]()",ial([{id,render_elink_anchor(Id)}])] || {id,Id} <- Ids], Pos});
render_element({a, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [a|State], Pos, Ind, D),
    Id =
        case proplists:get_all_values(id, Attr) of
            [] -> "";
            [IdStr] -> ial([{id,IdStr}])
        end,
    {[render_link(Attr, Docs),Id],NewPos};
render_element({code, _, Content}, [pre | _] = State, Pos, Ind, D) ->
    %% When code is within a pre we don't emit any underline
    render_docs(Content, [code | State], Pos, Ind, D);
%% Faulty {code,..} generated by diameter containing links.
%% we split them into multiple code segments.
render_element({code,CodeAttr,[Content,{a,AAttr,AContent}|H]}, State, Pos, Ind, D) ->
    AttrWithoutId = proplists:delete(id,CodeAttr),
    render_docs([{code,AttrWithoutId,[Content]},{a,AAttr,[{code,AttrWithoutId,AContent}]},
                 {code,CodeAttr,H}], State, Pos, Ind, D);
render_element({code,_CodeAttr,[]}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({code, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [code | State], Pos, Ind, D),

    IsDocumented = fun(What, #docs_v1{ docs = V1Docs }) ->
                           case lists:keyfind(What, 1, V1Docs) of
                               {What, _, _, #{}, _} ->
                                   true;
                               _ ->
                                   false
                           end
                   end,

    %% Try to convert code segments that refer to types but don't have a link
    %% to have the correct prefix. i.e. <c>byte()</c> should be `t:byte()`.
    TypedDocs =
        maybe
            %% We do not do any transform if we are in an `a` already
            true ?= State =:= [] orelse a =/= hd(State),
            {ok, T, _} ?= erl_scan:string(unicode:characters_to_list([Docs,"."]), {1, 1}),
            {ok, [{call,_,{atom,_,Name},Args}]} ?=
                case erl_parse:parse_exprs(T) of
                    {ok, [{op,A,'/',F,{integer,_,NumArgs}}]} ->
                        %% Translate any byte/0 to byte()
                        {ok,[{call,A,F,lists:duplicate(NumArgs,a)}]};
                    Else ->
                        Else
                end,
            case IsDocumented({function, Name, length(Args)}, D#config.docs) orelse
                erl_internal:bif(Name, length(Args))
            of
                true when length(Args) =:= 0 ->
                    lists:concat([io_lib:write_atom(Name),"/",length(Args)]);
                true ->
                    %% This is a function, so return code as is
                    {lists:concat(["[`",Docs,"`](`",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos};
                false when length(Args) =:= 1,
                           element(1, hd(Args)) =:= integer,
                           element(3, hd(Args)) =:= 3 ->
                    %% Is a foo(3) link
                    try
                        Name:module_info(), %% Check if module exists
                        NameStr = io_lib:write_atom(Name),
                        {["`m:",NameStr,"`"], Pos + string:length(NameStr)}
                    catch error:undef ->
                            Docs
                    end;
                false when length(Args) =:= 1,
                           element(1, hd(Args)) =:= integer,
                           element(3, hd(Args)) =:= 1 ->
                    %% Is a foo(1) link, i.e. a seecom
                    NameStr = io_lib:write_atom(Name),
                    {["[",NameStr,"](",NameStr,"_cmd.md)"], Pos + string:length(NameStr)};
                false ->
                    try
                        %% This is an op type (such as <c>=:=/2</c>)
                        erl_internal:op_type(Name, length(Args)),
                        {lists:concat(["[`",Docs,"`](`erlang:",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos}
                    catch error:function_clause ->
                            case IsDocumented({type,Name,length(Args)}, D#config.docs) orelse
                                erl_internal:is_type(Name,length(Args)) of
                                true when length(Args) =:= 0 ->
                                    lists:concat(["t:",io_lib:write_atom(Name),"/",length(Args)]);
                                true ->
                                    %% This is a type, add type prefix
                                    {lists:concat(["[`",Docs,"`](`t:",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos};
                                false ->
                                    case IsDocumented({callback,Name,length(Args)}, D#config.docs) of
                                        true ->
                                            %% This is a callback
                                            {lists:concat(["[`",Docs,"`](`c:",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos};
                                        false ->
                                            %% This is not a type, nor a function, nor a callback
                                            Docs
                                    end
                            end
                    end
            end
        else
            %% Could be a remote type erlang:message_queue_data()
            {ok, [{call,_,{remote,_,{atom,_,RM},{atom,_,RF}},RArgs}]} ->
                case code:get_doc(RM) of
                    {ok, RemoteDocs} ->
                        case IsDocumented({function,RF,length(RArgs)}, RemoteDocs) of
                            true ->
                                %% This is a remote function
                                Docs;
                            false ->
                                case IsDocumented({type,RF,length(RArgs)}, RemoteDocs)  of
                                    true ->
                                        %% This is a valid remote type
                                        {lists:concat(
                                           ["[`",Docs,"`](`t:",io_lib:write_atom(RM),":",
                                            io_lib:write_atom(RF),"/",length(RArgs),"`)"]),
                                         NewPos};
                                    false ->
                                        Docs
                                end
                        end;
                    _ ->
                        %% Could not fetch docs
                        Docs
                end;
            %% Could be a callback Module:init()
            {ok, [{call,_,{remote,_,{var,_,_RM},{atom,_,RF}},RArgs}]} ->
                case IsDocumented({callback,RF,length(RArgs)}, D#config.docs) of
                    true ->
                        %% This is a callback
                        {lists:concat(["[`",Docs,"`](`c:",io_lib:write_atom(RF),"/",length(RArgs),"`)"]), NewPos};
                    false ->
                        Docs
                end;
            false ->
                %% We are in a link already, maybe strip trailing (1/3)
                case re:run(Docs, "^([a-z_]+)\\([13]\\)$",[{capture,all_but_first,list}, unicode]) of
                    {match,[MaybeMod]} ->
                        case code:which(list_to_atom(MaybeMod)) of
                            non_existing ->
                                Docs;
                            _ ->
                                MaybeMod
                        end;
                    _ ->
                        Docs
                end;
            _ ->
                %% Could not parse
                Docs
        end,
    if is_tuple(TypedDocs) ->
            TypedDocs;
       true ->
            case re:run(TypedDocs, "`+", [global,unicode]) of
                nomatch ->
                    {["`", TypedDocs, "`", ial(Attr)], NewPos};
                {match,Matches} ->
                    LargestMatch = lists:max([Size || [{_, Size}] <- Matches]),
                    Ticks = lists:duplicate(LargestMatch+1,$`),
                    {[Ticks," ", TypedDocs, " ",Ticks,ial(Attr)], NewPos}
            end
    end;
render_element({em, Attr, Content}, State, Pos, Ind, D) ->
    render_element({i, Attr, Content}, State, Pos, Ind, D);
render_element({i, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [i | State], Pos, Ind, D),
    case lists:member(pre, State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["*", Docs, "*",ial(Attr)], NewPos}
    end;
render_element({hr, [], []}, _State, Pos, _Ind, _D) ->
    {"---\n", Pos};
render_element({br, [], []}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({strong, Attr, Content}, State, Pos, Ind, D) ->
    render_element({b, Attr, Content}, State, Pos, Ind, D);
render_element({b, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind, D),
    case lists:member(pre, State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["__", Docs, "__",ial(Attr)], NewPos}
    end;
render_element({pre, [], [{code,Attr,Content}]}, State, Pos, Ind, D) ->
    render_element({pre, Attr, Content}, State, Pos, Ind, D);
render_element({pre, Attr, Content}, State, Pos, Ind, D) ->
    %% This is a pre without any links or emphasis, so we use markdown

    %% For pre we make sure to respect the newlines in pre
    {Docs, _} = trimnl(render_docs(strip_tags(Content), [pre | State], Pos, Ind, D)),
    Type =
        case unicode:characters_to_binary(proplists:get_value(type, Attr, "text")) of
            <<"none">> -> "text";
            <<"text">> -> "text";
            <<"erlang">> -> "erlang";
            <<"erl">> -> "erlang";
            <<"erl-repl">> -> "erlang";
            <<"c">> -> "c"
        end,
    IdAttr = proplists:delete(type, Attr),
    trimnlnl(["```",Type,"\n", pad(Ind), Docs, pad(Ind), "```",
              [["\n",pad(Ind),ial(IdAttr)] || IdAttr =/= []]]);
render_element({ul, [{class, <<"types">>}], _Content}, _State, Pos, _Ind, _D) ->
    {"", Pos}; %% Don't render any types
    %% case _D#config.current of
    %%     {_, _, _, _, #{ specs := _}} ->
    %%         {"", _Pos};
    %%     _ ->
    %%         {Docs, _} = render_docs(_Content, [types | _State], 0, _Ind, _D),
    %%         trimnlnl(Docs)
    %% end;
render_element({li, Attr, Content}, [types | _] = State, Pos, Ind, C) ->
    Doc =
        case {proplists:get_value(name, Attr), proplists:get_value(class, Attr)} of
            {undefined, Class} when Class =:= undefined; Class =:= <<"type">> ->
                %% Inline html for types
                render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind, C);
            {_, <<"description">>} ->
                %% Inline html for type descriptions
                render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind + 2, C);
            {Name, _} ->
                %% Try to render from type metadata
                case render_type_signature(binary_to_atom(Name), C) of
                    undefined when Content =:= [] ->
                        %% Failed and no content, emit place-holder
                        {["```erlang\n-type ", Name, "() :: term().```"], 0};
                    undefined ->
                        %% Failed with metadata, render the content
                        render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind, C);
                    Type ->
                        %% Emit the erl_pp typespec
                        {["```erlang\n", Type, "```"], 0}
                end
        end,
    trimnl(Doc);
render_element({ul, [], Content}, State, Pos, Ind, D) ->
    trimnlnl(render_docs(Content, [ul | State], Pos, Ind, D));
render_element({ol, [], Content}, State, Pos, Ind, D) ->
    trimnlnl(render_docs(Content, [ol | State], Pos, Ind, D));
render_element({li, [], Content}, [ul | _] = State, Pos, Ind, D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2, Ind + 2, D),
    trimnl(["* ", Docs]);
render_element({li, [], Content}, [ol | _] = State, Pos, Ind, D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2, Ind + 2, D),
    trimnl(["1. ", Docs]);
render_element({dl, [], [{dt,DTAttr,DTContent}, {dd,[],DDContent} | Content]}, State, Pos, Ind, D) ->
    Since = proplists:get_value(since, DTAttr),
    {DTDocs, _DTNewPos} =
        render_docs(
          [{b, [], DTContent}],
          [li, dl | State],
          Pos + 2,
          Ind + 2,
          D),
    Ids = [{id,Id} || {id,Id} <- DTAttr],
    DTDocsWAnchors = case Ids of
                         [] -> trim(DTDocs);
                         Ids -> [trim(DTDocs),ial(Ids)]
                     end,
    {DDDocs, DDNewPos} = render_docs(DDContent, [li, dd | State], 0, Ind + 2, D),
    {Docs, NewPos} =
        case string:find(DTDocs, "\n") of
            nomatch when Since =:= undefined, is_binary(hd(DDContent)) orelse element(1,hd(DDContent)) =/= pre ->
                trimnlnl({["* ", trim(DTDocsWAnchors), " - ", string:trim(string:trim(DDDocs, both, "\n"), leading, " ")], DDNewPos});
            _ ->
                trimnlnl({["* ", trim(DTDocsWAnchors), [["(Since ",Since,")"] || Since =/= undefined],"  \n",
                           DDDocs], DDNewPos})
        end,
    {DLDocs, DLPos} = render_element({dl, [], Content}, State, NewPos, Ind, D),
    {[Docs,DLDocs], DLPos};
render_element({dl, [], []}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({table, Attr, Rows}, State, Pos, Ind, D) ->
    [{tr,_,Head} | RowsNoCaption] = [Row || {tr,_,_} = Row <- Rows],
    {TableDocs, TablePos} =
        trimnl(render_docs([{th, [], Head} | RowsNoCaption], [table|State], Pos, Ind, D)),
    {CaptionDocs, CaptionPos} =
        render_docs([{em, [], [<<"Table: ">>, C]} || {caption,_,C} <- Rows, not string:equal(C,"")],
                    [table|State], TablePos, Ind, D),
    trimnlnl({[TableDocs, [[pad(Ind),ial(proplists:delete(align,Attr)),"\n\n"] || Attr =/= []], CaptionDocs], CaptionPos});
render_element({th, [], Head}, State, _Pos, _Ind, D) ->
    Header =
        [begin {Docs, _} = render_docs(Td, [th|State], 0, 0, D),
               {["| ", Docs, " "], ["|-", lists:duplicate(string:length(Docs), $-), "-"]}
         end || Td <- Head],
    trimnl({[[ Docs || {Docs,_} <- Header ], "|\n",
             [ Lines || {_, Lines} <- Header ], "|\n"], 0});
render_element({tr, [], Row}, State, _Pos, _Ind, D) ->
    Rows =
        [begin {Docs, _} = render_docs(Td, [tr|State], 0, 0, D),
               ["| ", Docs, " "]
         end || Td <- Row],
    trimnl({[ Rows, "|"], 0});
render_element({td, _, TDContent}, State, Pos, Ind, D) ->
    render_docs(TDContent, [td|State], Pos, Ind, D);
render_element({img,Attr,Content}, _State, Pos, _Ind, _D) ->
    Caption = case lists:keyfind(caption, 1, Content) of
                  false -> "";
                  {caption, _, C} ->
                      C
              end,
    trimnlnl({["![",Caption,"](",filename:join("assets",filename:basename(proplists:get_value(file,Attr))),
               " \"",Caption,"\")",ial(proplists:delete(file, Attr)),"\n"], Pos});
render_element({quote, [], Content}, State, _Pos, Ind, D) ->
    {Docs, 0} = render_element({'div', [], Content}, ['div' | State], 0, 0, D),
    trimnlnl([[pad(Ind), "> ",Line,"\n"] || Line <- string:split(trim(Docs),"\n",all)]);
render_element(B, State, Pos, Ind, _D) when is_binary(B) ->
    %% Indent the string correctly
    Pre = re:replace(B,"\n",nlpad(Ind),[global,unicode]),

    Str =
        case State of
            [pre | _] ->
                Pre;
            [code | _] ->
                Pre;
            [h4 | _] ->
                Pre;
            _ ->
                EscapeChars = [
                               "\\",
                               "`",
                               "{",
                               "}",
                               "!"|
                               [["|"] || lists:member(table,State)]
                              ],
                lists:foldl(
                  fun({Pat, Subst}, S) -> re:replace(S, Pat, Subst, [global,unicode]) end,
                  B,
                  [{["(", lists:join($|, [["\\", C] || C <- EscapeChars]), ")"], "\\\\\\1"},
                   %% [^S\r\n] == All whitespace except \r\n
                   {"(\n\\s*[0-9]+)\\.([^S\r\n])", "\\1\\\\.\\2"},  %% \n1. -> 1\.
                   {"^(\\s*[0-9]+)\\.([^S\r\n])",  "\\1\\\\.\\2"},  %% ^1. -> 1\.
                   {"(\n\\s*)\\*([^S\r\n])",       "\\1\\\\*\\2"},  %% \n* -> \*
                   {"^(\\s*)\\*([^S\r\n])",        "\\1\\\\*\\2"},  %% ^* -> \*
                   {"(\n\\s*)\\-([^S\r\n])",       "\\1\\\\-\\2"},  %% \n- -> \-
                   {"^(\\s*)\\-([^S\r\n])",        "\\1\\\\-\\2"},  %% ^- -> \-
                   {"(\n\\s*)\\+([^S\r\n])",       "\\1\\\\+\\2"},  %% \n+ -> \+
                   {"^(\\s*)\\+([^S\r\n])",        "\\1\\\\+\\2"},  %% ^+ -> \+
                   {"(\n\\s*)\\#([^S\r\n])",       "\\1\\\\#\\2"},  %% \n# -> \#
                   {"^(\\s*)\\#([^S\r\n])",        "\\1\\\\#\\2"},  %% ^# -> \#
                   {"\\[([^]]+\\])",               "\\\\[\\1"},     %% [..] -> \[..]
                   {"<(http[^>]+>)",               "\\\\<\\1"},     %% <..> -> \<..>
                   {"(\s)_([^_]+_\s)",             "\\1\\\\_\\2"}]  %% _.._ -> \_.._
                 )
        end,
    {Str, Pos + lastline(Str)};
render_element({Tag, Attr, Content}, State, Pos, Ind, D) ->
    case lists:member(Tag, ?ALL_ELEMENTS) of
        true ->
            throw({unhandled_element, Tag, Attr, Content});
        false ->
            throw({unknown_element, Tag, Attr, Content}),
            ok
    end,
    render_docs(Content, State, Pos, Ind, D).


render_link(Attr, Docs) ->
    render_link(Docs, proplists:get_value(rel, Attr), proplists:get_value(href, Attr)).

render_link(Docs, undefined, Href) when Href =/= undefined ->
    %% This types of links are usually from edoc, but could also be
    %% <url> from erl_docgen
    case Href of
        <<"overview-summary.html",Rest/binary>> ->
            %% This is an edoc overview anchor
            Anchor = lists:last(string:split(Rest,"#")),
            ["[", Docs, "](chapter.md#", render_link_anchor(Anchor), ")"];
        Href ->
            ["[", Docs, "](", Href, ")"]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seemfa">>, Href) ->
    MFA = case string:split(Href, ":") of
              [_App, HrefMFA] -> HrefMFA;
              [Href] -> Href
          end,
    [Mod, FA] = case string:split(MFA, "#") of
                    [<<>>, MFANoAnchor] -> ["", MFANoAnchor];
                    [Module, FunArgs] ->
                        case string:equal(atom_to_list(get(module)), Module) of
                            true ->
                                ["",FunArgs];
                            false ->
                                [[Module,":"],FunArgs]
                        end
                end,
    {Prefix, Func, Arity} =
        case string:split(FA, "/") of
            [<<"Module:", F/binary>>, A] ->
                {"c:",F, A};
            [<<"Mod:", F/binary>>, A] ->
                {"c:",F, A};
            [F, A] ->
                {"", F, A}
        end,
    Link = [Mod,Func,"/",Arity],
    case string:equal(Docs, Link) orelse string:equal(Docs, ["`",Link,"`"]) of
        true ->
            ["`",Prefix,Link,"`"];
        false ->
             [
              "[", Docs, "](`",Prefix,Link,"`)"
             ]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seetype">>, Href) ->
    MFA = case string:split(Href, ":") of
              [_App, HrefMFA] -> HrefMFA;
              [Href] -> Href
          end,
    [ModDocs, Mod, FA] =
        case string:split(MFA, "#") of
            [<<>>, MFANoAnchor] -> [get(module), "", MFANoAnchor];
            [Module, FunArgs] ->
                case string:equal(atom_to_list(get(module)), Module) of
                    true ->
                        [get(module), "",FunArgs];
                    false ->
                        [binary_to_atom(Module), [Module,":"],FunArgs]
                end
        end,
    {Func, Arity} =
        case string:split(FA, "/") of
            [FA] ->
                {ok, #docs_v1{ docs = Ds}} = code:get_doc(ModDocs),
                App = get(application),
                case lists:search(
                       fun(E) ->
                               case element(1, E) of
                                   {type, Type, _} ->
                                       string:equal(atom_to_list(Type), FA);
                                   _ ->
                                       false
                               end
                       end, lists:sort(Ds)) of
                    {value, {{type,_,TypeArity},_,_,_,_}} ->
                        {FA, integer_to_list(TypeArity)};
                    _Else when App =/= "wx" ->
                        io:format("Could not find find type: ~p~n",
                                  [[Mod, FA]]),
                        exit({Mod, FA});
                    _Else ->
                        {FA,"0"}
                end;
            [F, A] ->
                {F, A}
        end,
    Link = [Mod,Func,"/",Arity],
    ZeroLink = [Mod,Func,"()"],
    case (string:equal(Docs, Link) orelse string:equal(Docs, ["`",Link,"`"])) orelse
        ((string:equal(Docs, ZeroLink) orelse string:equal(Docs, ["`",ZeroLink,"`"])) andalso Arity =:= "0")
    of
        true ->
            ["`t:", Link, "`"];
        false ->
             [
              "[", Docs, "](`t:", Link,"`)"
             ]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>,
            Href = <<"erl_docgen:doc_storage">>) ->
    ["[",Docs,"](`e:",Href,".md`)"];
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>,
            <<"erl_docgen:doc_storage.html">>) ->
    ["[",Docs,"](`e:erl_docgen:doc_storage.md`)"];
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>,
            <<"edoc:edoc_cmd">>) ->
    ["[",Docs,"](edoc_cmd.md)"];
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>, Href) ->
    ModAnchor =
        case string:split(Href, ":") of
            [MA] ->
                MA;
            [_App, MA] ->
                MA
        end,
    ModFixedAnchor =
        case string:split(ModAnchor, "#") of
            [ModAnchor] ->
                ModAnchor;
            [M, A] ->
                [M, "#", render_elink_anchor(A)]
        end,
    DocsNoMan3 = re:replace(Docs,["(`?",ModFixedAnchor,")\\(3\\)(`?)"],"\\1\\2"),
    case string:equal(DocsNoMan3, ModFixedAnchor) orelse
        string:equal(DocsNoMan3, ["`",ModFixedAnchor,"`"]) of
        true ->
            ["`m:", ModFixedAnchor, "`"];
        false ->
            ["[", Docs, "](`m:", ModFixedAnchor, "`)"]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seeguide">>, Href) ->
    CurrentApplication = unicode:characters_to_binary(get(application)),
    RemoveSystemApp = fun(<<"system/general_info",_/binary>>) ->
                              <<"general_info">>;
                          (<<"system",_/binary>>) ->
                              <<"system">>;
                         (Else) ->
                              Else
                      end,
    case string:lexemes(Href, ":#") of
        [App, <<"index">>] when App =:= CurrentApplication ->
            ["[", Docs, "](index.html)"];
        [App, <<"index">>] ->
            ["[", Docs, "](`e:",RemoveSystemApp(App),":index.html`)"];
        [App, Guide] when App =:= CurrentApplication ->
            ["[", Docs, "](",string:lowercase(Guide),".md)"];
        [App, Guide, Anchor] when App =:= CurrentApplication ->
            ["[", Docs, "](",string:lowercase(Guide),".md#",
              render_elink_anchor(Anchor),")"];
        [App, Guide] ->
            ["[", Docs, "](`e:",RemoveSystemApp(App),":",string:lowercase(Guide),".md`)"];
        [App, Guide, Anchor] ->
            ["[", Docs, "](`e:",RemoveSystemApp(App),":",string:lowercase(Guide),".md#",
              render_elink_anchor(Anchor),"`)"]
    end;
render_link(Docs, Rel, Href)
  when Rel =:= <<"https://erlang.org/doc/link/seecref">>;
       Rel =:= <<"https://erlang.org/doc/link/seecom">>;
       Rel =:= <<"https://erlang.org/doc/link/seeapp">> ->
    CurrentApplication = unicode:characters_to_binary(get(application)),
    Postfix = case Rel of
                  <<"https://erlang.org/doc/link/seecom">> ->
                      "_cmd";
                  <<"https://erlang.org/doc/link/seeapp">> ->
                      "_app";
                  _ ->
                      ""
              end,
    AddPostfix = fun(Guide) ->
                         string:lowercase(
                           case string:prefix(string:reverse(Guide),
                                              string:reverse(Postfix)) of
                               nomatch ->
                                   [Guide,Postfix];
                               _ ->
                                   Guide
                           end)
                 end,
    case string:lexemes(Href, ":#") of
        [App, <<"index">>] when App =:= CurrentApplication ->
            ["[", Docs, "](index.html)"];
        [App, <<"index">>] ->
            ["[", Docs, "](`e:",App,":index.html`)"];
        [App, Guide] when App =:= CurrentApplication ->
            ["[", Docs, "](",AddPostfix(Guide),".md)"];
        [App, Guide, Anchor] when App =:= CurrentApplication ->
            ["[", Docs, "](",AddPostfix(Guide),".md#",
              render_elink_anchor(Anchor),")"];
        [App, Guide] ->
            ["[", Docs, "](`e:",App,":",AddPostfix(Guide),".md`)"];
        [App, Guide, Anchor] ->
            ["[", Docs, "](`e:",App,":",AddPostfix(Guide),".md#",
              render_elink_anchor(Anchor),"`)"]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seefile">>, Href) ->
    CurrentApplication = unicode:characters_to_binary(get(application)),
    MaybeAddExtension = fun(G) ->
                                string:lowercase(
                                  case string:equal(filename:extension(G),"") of
                                      true -> [G,".md"];
                                      _ -> G
                                  end)
                        end,
    case string:lexemes(Href, ":#") of
        [App, Guide] when App =:= CurrentApplication, App =:= <<"jinterface">> ->
            ["[", Docs, "](assets/",Guide,".html)"];
        [App, Guide, Anchor] when App =:= CurrentApplication, App =:= <<"jinterface">> ->
            ["[", Docs, "](assets/",Guide,".html#",render_link_anchor(Anchor),")"];
        [App, Guide] when App =:= CurrentApplication ->
            ["[", Docs, "](",MaybeAddExtension(Guide),")"];
        [App, Guide, Anchor] when App =:= CurrentApplication ->
            ["[", Docs, "](",MaybeAddExtension(Guide),"#",render_link_anchor(Anchor),")"];
        [App, Guide] ->
            ["[", Docs, "](`e:",App,":",MaybeAddExtension(Guide),"`)"];
        [App, Guide, Anchor] ->
            ["[", Docs, "](`e:",App,":",MaybeAddExtension(Guide),
             "#",render_link_anchor(Anchor),"`)"]
    end;
render_link(Docs, _Rel, _Href) ->
    Docs.

render_elink_anchor(Anchor) ->
      render_link_anchor(
        lists:foldl(
          fun({Re,Sub}, Str) -> re:replace(Str, Re, Sub, [global, unicode]) end,
          Anchor,
          [{" ","-"},{"(--|\\.)","-"}, {"(^-|-$)",""}])).

render_link_anchor(Anchor) ->
    uri_string:quote(Anchor).

-spec render_type_signature(atom(), #config{}) -> 'undefined' | unicode:chardata().
render_type_signature(Name, #config{docs = #docs_v1{metadata = #{types := AllTypes}}}) ->
    case [Type || Type = {TName, _} <- maps:keys(AllTypes), TName =:= Name] of
        [] ->
            undefined;
        Types ->
            [erl_pp:attribute(maps:get(Type, AllTypes)) || Type <- Types]
    end.

ial([]) ->
    "";
ial(Attrs) ->
    ["{: ", [[ial(Tag, Value), " "] || {Tag,Value} <- Attrs], "}"].

ial(class, Value) ->
    [".", maybe_quote_ial(Value)];
ial(id, Value) ->
    ["#", maybe_quote_ial(Value)];
ial(Tag, Value) ->
    [atom_to_list(Tag), "=", maybe_quote_ial(Value)].

maybe_quote_ial(Str) ->
    case string:find(Str, " ") of
        nomatch ->
            Str;
        _ ->
            [$",Str,$"]
    end.

%% Pad N spaces (and possibly pre-prend newline), disabling any ansi formatting while doing so.
-spec pad(non_neg_integer()) -> unicode:chardata().
pad(N) ->
    pad(N, "").
-spec nlpad(non_neg_integer()) -> unicode:chardata().
nlpad(N) ->
    pad(N, "\n").
-spec pad(non_neg_integer(), unicode:chardata()) -> unicode:chardata().
pad(N, Extra) ->
    Pad = lists:duplicate(N, [$\s]),
    [Extra, Pad].

-spec lastline(unicode:chardata()) -> non_neg_integer().
%% Look for the length of the last line of a string
lastline(Str) ->
    LastStr =
        case string:find(Str, "\n", trailing) of
            nomatch ->
                Str;
            Match ->
                tl(string:next_codepoint(Match))
        end,
    string:length(LastStr).

strip_tags([H|T]) when is_binary(H) ->
    [H|strip_tags(T)];
strip_tags([{_Tag,_,C}|T]) ->
    [strip_tags(C)|strip_tags(T)];
strip_tags([]) ->
    [].

%% These functions make sure that we trim extra newlines added
%% by the renderer. For example if we do <li><p></p></li>
%% that would add 4 \n at after the last </li>. This is trimmed
%% here to only be 2 \n
-spec trimnlnl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) ->
    {unicode:chardata(), 0}.
trimnlnl({Chars, _Pos}) ->
    nl(nl(string:trim(Chars, trailing, "\n")));
trimnlnl(Chars) ->
    nl(nl(string:trim(Chars, trailing, "\n"))).
-spec trimnl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) ->
    {unicode:chardata(), 0}.
trimnl({Chars, _Pos}) ->
    nl(string:trim(Chars, trailing, "\n"));
trimnl(Chars) ->
    nl(string:trim(Chars, trailing, "\n")).
trim(Chars) ->
    string:trim(Chars, trailing, "\n").
-spec nl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) -> {unicode:chardata(), 0}.
nl({Chars, _Pos}) ->
    nl(Chars);
nl(Chars) ->
    {[Chars, "\n"], 0}.


%% Code copied from c.erl

c(Module, SingleOption) when not is_list(SingleOption) ->
    c(Module, [SingleOption]);
c(Module, Opts) when is_atom(Module) ->
    %% either a module name or a source file name (possibly without
    %% suffix); if such a source file exists, it is used to compile from
    %% scratch with the given options, otherwise look for an object file
    Suffix = case filename:extension(Module) of
                 "" -> src_suffix(Opts);
                 S -> S
             end,
    SrcFile = filename:rootname(Module, Suffix) ++ Suffix,
    case filelib:is_file(SrcFile) of
        true ->
            compile_and_load(SrcFile, Opts);
        false ->
            c(Module, Opts, fun (_) -> true end)
    end;
c(Module, Opts) ->
    %% we never interpret a string as a module name, only as a file
    compile_and_load(Module, Opts).

c(Module, Options, Filter) when is_atom(Module) ->
    case find_beam(Module) of
        BeamFile when is_list(BeamFile) ->
            c(Module, Options, Filter, BeamFile);
        Error ->
            {error, Error}
    end.

c(Module, Options, Filter, BeamFile) ->
    case compile_info(Module, BeamFile) of
        Info when is_list(Info) ->
            case find_source(BeamFile, Info) of
                SrcFile when is_list(SrcFile) ->
                    c(SrcFile, Options, Filter, BeamFile, Info);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

c(SrcFile, NewOpts, Filter, BeamFile, Info) ->
    %% Filter old options; also remove options that will be replaced.
    %% Write new beam over old beam unless other outdir is specified.
    F = fun (Opt) -> not is_outdir_opt(Opt) andalso Filter(Opt) end,
    Options = (NewOpts ++ [{outdir,filename:dirname(BeamFile)}]
               ++ lists:filter(F, old_options(Info))),
    %% io:format("Recompiling ~ts\n", [SrcFile]),
    safe_recompile(SrcFile, Options, BeamFile).

%% find the beam file for a module, preferring the path reported by code:which()
%% if it still exists, or otherwise by searching the code path
find_beam(Module) when is_atom(Module) ->
    case code:which(Module) of
        Beam when is_list(Beam), Beam =/= "" ->
            case erlang:module_loaded(Module) of
                false ->
                    Beam;  % code:which/1 found this in the path
                true ->
                    case filelib:is_file(Beam) of
                        true -> Beam;
                        false -> find_beam_1(Module)  % file moved?
                    end
            end;
        Other when Other =:= ""; Other =:= cover_compiled ->
            %% module is loaded but not compiled directly from source
            find_beam_1(Module);
        Error ->
            Error
    end.

find_beam_1(Module) ->
    File = atom_to_list(Module) ++ code:objfile_extension(),
    case code:where_is_file(File) of
        Beam when is_list(Beam) ->
            Beam;
        Error ->
            Error
    end.

%% get the compile_info for a module
%% -will report the info for the module in memory, if loaded
%% -will try to find and examine the beam file if not in memory
%% -will not cause a module to become loaded by accident
compile_info(Module, Beam) when is_atom(Module) ->

    case erlang:module_loaded(Module) of
        true ->
            %% getting the compile info for a loaded module should normally
            %% work, but return an empty info list if it fails
            try compile_info_add_cwd(Beam, erlang:get_module_info(Module, compile))
            catch _:_ -> compile_info_add_cwd(Beam, [])
            end;
        false ->
            case beam_lib:chunks(Beam, [compile_info]) of
                {ok, {_Module, [{compile_info, Info}]}} ->
                    compile_info_add_cwd(Beam, Info);
                Error ->
                    Error
            end
    end.

compile_info_add_cwd(Beam, Info) ->
    CwdOpts =
        case beam_lib:chunks(Beam, [debug_info]) of
            {ok, {_,[{debug_info,{debug_info_v1,erl_abstract_code,{_AST,Meta}}}]}} ->
                case proplists:get_value(cwd, Meta) of
                    undefined ->
                        [];
                    Cwd ->
                        [{i, Cwd}]
                end;
            _ ->
                []
        end,
    case lists:keytake(options, 1, Info) of
        false ->
            [{options, CwdOpts}];
        {value, {options, Options}, InfoNoOpts} ->
            [{options, Options ++ CwdOpts} | InfoNoOpts]
    end.

%% prefer the source path in the compile info if the file exists,
%% otherwise do a standard source search relative to the beam file
find_source(BeamFile, Info) ->
    case lists:keyfind(source, 1, Info) of
        {source, SrcFile} ->
            case filelib:is_file(SrcFile) of
                true -> SrcFile;
                false -> find_source(BeamFile)
            end;
        _ ->
            find_source(BeamFile)
    end.

find_source(BeamFile) ->
    case filelib:find_source(BeamFile) of
        {ok, SrcFile} -> SrcFile;
        _ -> {error, no_source}
    end.

old_options(Info) ->
    case lists:keyfind(options, 1, Info) of
        {options, Opts} -> Opts;
        false -> []
    end.

%% compile module, backing up any existing target file and restoring the
%% old version if compilation fails (this should only be used when we have
%% an old beam file that we want to preserve)
safe_recompile(File, Options, BeamFile) ->
    %% Note that it's possible that because of options such as 'to_asm',
    %% the compiler might not actually write a new beam file at all
    Backup = BeamFile ++ ".bak",
    case file:rename(BeamFile, Backup) of
        Status when Status =:= ok; Status =:= {error,enoent} ->
            {ok, OrigFile} = file:read_file(File),
            ok = file:write_file(File, [OrigFile, "\n", [V || {append,V} <- Options]]),
            NoAppendOpts = [V || V <- Options, element(1,V) =/= append],
            case compile_and_load(File, NoAppendOpts) of
                {ok, _} = Result ->
                    _ = if Status =:= ok -> file:rename(Backup, BeamFile);
                           true -> ok
                        end,
                    file:write_file(File, OrigFile),
                    Result;
                Error ->
                    _ = if Status =:= ok -> file:rename(Backup, BeamFile);
                           true -> ok
                        end,
                    file:write_file(File, OrigFile),
                    Error
            end;
        Error ->
            Error
    end.

%% Compile the file and load the resulting object code (if any).
%% Automatically ensures that there is an outdir option, by default the
%% directory of File, and that a 'from' option will be passed to match the
%% actual source suffix if needed (unless already specified).
compile_and_load(File, Opts0) when is_list(Opts0) ->
    Opts = [warnings_as_errors,
            report_errors, report_warnings
            | ensure_from(filename:extension(File),
                          ensure_outdir(".", Opts0))],
    case compile:file(File, Opts) of
	{ok,Mod} ->				%Listing file.
	    {ok, Mod};
	{ok,Mod,[]} ->				%Warnings maybe turned on.
	    {ok, Mod};
	Other ->				%Errors go here
	    Other
    end;
compile_and_load(File, Opt) ->
    compile_and_load(File, [Opt]).

ensure_from(Suffix, Opts0) ->
    case lists:partition(fun is_from_opt/1, Opts0++from_opt(Suffix)) of
        {[Opt|_], Opts} -> [Opt | Opts];
        {[], Opts} -> Opts
    end.

ensure_outdir(Dir, Opts0) ->
    {[Opt|_], Opts} = lists:partition(fun is_outdir_opt/1,
                                      Opts0++[{outdir,Dir}]),
    [Opt | Opts].

%% mimic how suffix is selected in compile:file().
src_suffix([from_core|_]) -> ".core";
src_suffix([from_asm|_])  -> ".S";
src_suffix([_|Opts]) -> src_suffix(Opts);
src_suffix([]) -> ".erl".

is_outdir_opt({outdir, _}) -> true;
is_outdir_opt(_) -> false.

is_from_opt(from_core) -> true;
is_from_opt(from_asm) -> true;
is_from_opt(_) -> false.

from_opt(".core") -> [from_core];
from_opt(".S")    -> [from_asm];
from_opt(_)       -> [].

guesslang(Pattern) ->
    pmap(
      fun(File) ->
              {ok, B} = file:read_file(File),
              %% This regexp produces 5 matches:
              %% 1. <pre>..</pre> or <code ..>..</code>
              %% 2. <pre or <code
              %% 3. type="something" (if available)
              %% 4. Content of pre/code
              %% 5 </pre> or </code>
              case re:run(B, "(?:(<code|<pre)(?: [^\"]+\"([^\"]+)\"[^>]?)?>)(.*)(</(?:code|pre)>)",
                          [unicode,global,dotall,ungreedy]) of
                  {match, Matches} ->
                      file:write_file(File, guesslang(B, lists:reverse(Matches)));
                  _ ->
                      ok
              end
      end, filelib:wildcard(Pattern)).

-include_lib("xmerl/include/xmerl.hrl").

%% [eep48_to_markdown:guesslang("lib/"++App++"/doc/src/*.{xml,xmlsrc}") || App <- [lists:nth(2, filename:split(P)) || P <- filelib:wildcard("lib/*/doc/src")]], eep48_to_markdown:guesslang("erts/doc/src/*.{xml,xmlsrc}"), eep48_to_markdown:guesslang("system/doc/*/*.{xml,xmlsrc}").
guesslang(B, [[{Start, Len} = AllPos, HdrPos, TypePos, CodePos, FtrPos] | T]) ->
    Before = binary:part(B, 0, Start),
    After = binary:part(B, Start+Len, byte_size(B) - (Start+Len)),
    All = binary:part(B, AllPos),
    {Code, Type} =
        try xmerl_scan:string(binary_to_list(unicode:characters_to_binary(All))) of
            {XML, _} ->
                {unicode:characters_to_binary(remove_tags([XML])),
                 case XML of
                    #xmlElement{ attributes = [#xmlAttribute{ name = type, value = Value }]} ->
                        Value;
                    _ -> ""
                end}
    catch _E:_R:_ST ->
            io:format("Could not parse: ~ts~n",[All]),
            {All, [unicode:charactes_to_list(binary:part(B, TypePos)) || TypePos =/= {-1,0}]}
    end,
    io:format("Looking at:~n~ts~n",[Code]),
    case Type =:= "" orelse string:equal(Type,"none") of
        true ->
            case guesslang_run(Code) of
                {Guess, Perc, F} when (Guess == "erlang" orelse Guess == "c"), Perc > 1.7 ->
                    io:format("Guessed ~ts (~p) (~ts)~n~n",[Guess, Perc, F]),
                    guesslang(
                      unicode:characters_to_binary(
                        [Before, binary:part(B, HdrPos), " type=\"", Guess, "\">",
                         binary:part(B, CodePos), binary:part(B, FtrPos), After]), T);
                {Guess, Perc, F} ->
                    io:format("Guessed ~ts (~p) (~ts)~n~n",[Guess, Perc, F]),
                    guesslang(B, T)
            end;
        _ ->
            io:format("No guess~n~n",[]),
            guesslang(B, T)
    end;
guesslang(B, []) ->
    B.

remove_tags([#xmlElement{ content = C } | T]) ->
    [remove_tags(C) | remove_tags(T)];
remove_tags([#xmlText{ value = V } | T]) ->
    [V | remove_tags(T)];
remove_tags([]) ->
    [].

guesslang_run(Docs) ->
    File = string:trim(os:cmd("mktemp")),
    file:write_file(File, Docs),
    Res = os:cmd("cat "++File++" | guesslang -p | grep \"^ \""),
    file:delete(File),
    first_match(string:split(Res,"\n", all), undefined).

first_match([H | T], undefined) ->
    first_match([H|T], H);
first_match([" Erlang " ++ Perc | _], F) ->
    {"erlang", parse_perc(Perc), F };
first_match([" C " ++ Perc | _], F) ->
    {"c", parse_perc(Perc), F };
first_match([" Shell " ++ Perc | _], F) ->
    {"shell", parse_perc(Perc), F };
first_match([_ | T], F) ->
    first_match(T, F);
first_match([], F) ->
    {"text", 0.0, F}.

parse_perc(Perc) ->
    list_to_float(string:trim(string:trim(Perc),both,"%")).
