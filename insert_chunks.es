#!/usr/bin/env escript
-feature(maybe_expr, enable).
-mode(compile).

-include_lib("kernel/include/eep48.hrl").

main([BeamFile | T]) ->
    try
        {ok, Module, Chunks} = beam_lib:all_chunks(BeamFile),
        {debug_info_v1, _, {AST, ASTMeta}} = binary_to_term(proplists:get_value("Dbgi", Chunks)),
        {_, File} = lists:foldl(
                            fun({attribute, [{generated,true}|_], file, {File, _}}, {false, _}) ->
                                    {true, File};
                               (_, File) when is_tuple(File) ->
                                    File;
                               ({attribute, _, file, {File,_}}, _) ->
                                    File;
                               ({attribute, _Anno, module, _}, File) ->
                                    {false, File}
                            end, undefined, AST),
        Filename = filename:join(proplists:get_value(cwd, ASTMeta, ""), File),
        NewDocs =
            case lists:keysearch(moduledoc, 3, AST) of
                {value, {attribute, _, _, _}} ->
                    {{ModuleDoc, ModuleAnno}, ModuleMD} =
                        lists:foldl(
                          fun
                              ({attribute, _ModuleDocAnno, moduledoc, ModuleMeta},{Doc, Meta})
                                when is_map(ModuleMeta) ->
                                  {Doc, maps:merge(Meta, ModuleMeta)};
                              ({attribute, ModuleDocAnno, moduledoc, false},{_Doc, Meta}) ->
                                  {{hidden, ModuleDocAnno}, Meta};
                              ({attribute, ModuleDocAnno, moduledoc, ModuleDoc},{_Doc, Meta}) ->
                                  {{#{ <<"en">> => unicode:characters_to_binary(ModuleDoc)},
                                    ModuleDocAnno}, Meta};
                              (_, Acc) ->
                                  Acc
                          end,
                          {{none, {1,1}}, #{}}, AST),
                    MD = #docs_v1{
                           format = <<"text/markdown">>,
                           anno = ModuleAnno,
                           metadata = ModuleMD,
                           module_doc = ModuleDoc },
                    MD#docs_v1{ docs = extract_docs(AST, filename:dirname(Filename)) };
                _ ->
                    case get_doc_chunk(BeamFile, atom_to_list(Module)) of
                        {ok, Docs} ->
                            case lists:reverse(filename:split(code:which(Module))) of
                                ["preloaded"] ->
                                    put(application, "erts");
                                [_File,_Ebin,App | _] ->
                                    put(application, App)
                            end,
                            convert_docs(Docs);
                        Else ->
                            exit(Else)
                    end
            end,
        {ok, NewBeamFile} = beam_lib:build_module([{"Docs",term_to_binary(NewDocs)} | proplists:delete("Docs", Chunks)]),
        file:write_file(BeamFile, NewBeamFile)
    catch E:R:ST ->
            io:format("Failed to convert ~ts~n",[BeamFile]),
            erlang:raise(E, R, ST)
    end,
    main(T);
main([]) ->
    ok.

get_doc_chunk(Filename, Mod) ->
    RootDir = code:root_dir(),
    case filename:dirname(Filename) of
        Filename ->
            {error,missing};
        RootDir ->
            {error,missing};
        Dir ->
            ChunkFile = filename:join([Dir,"doc","chunks",Mod ++ ".chunk"]),
            case file:read_file(ChunkFile) of
                {ok, Bin} ->
                    {ok, binary_to_term(Bin)};
                {error,enoent} ->
                    get_doc_chunk(Dir, Mod);
                {error,Reason} ->
                    {error,Reason}
            end
    end.

extract_docs(AST, Cwd) ->
    extract_docs(expand_anno(AST), {undefined, #{}}, Cwd).
extract_docs([{attribute, _Anno, doc, MoreMeta}|T], {Doc, Meta}, Cwd) when is_map(MoreMeta) ->
        extract_docs(T, {Doc, maps:merge(Meta, MoreMeta)}, Cwd);
extract_docs([{attribute, _Anno, doc, {file, Path}}|T], {_, Meta}, Cwd) ->
    maybe
        {ok, Doc} ?= file:read_file(filename:join(Cwd, Path)),
        extract_docs(T, {string:trim(Doc), Meta}, Cwd)
    else
        _ ->
            io:format("Failed to open: ~p~n",[filename:join(Cwd, Path)]),
            exit(1)
    end;
extract_docs([{attribute, _Anno, doc, false}|T], {_, Meta}, Cwd) ->
    extract_docs(T, {hidden, Meta}, Cwd);
extract_docs([{attribute, _Anno, doc, Doc}|T], {_, Meta}, Cwd) ->
    extract_docs(T, {string:trim(Doc), Meta}, Cwd);
extract_docs([{Kind, Anno, F, A, Body}|T],{undefined, #{ equiv := {EquivF,EquivA} } = Meta}, Cwd) ->
    extract_docs([{Kind, Anno, F, A, Body}|T],
                 {io_lib:format("Equivalent to `~ts~p/~p`",[prefix(Kind), EquivF,EquivA]), Meta}, Cwd);
extract_docs([{Kind, Anno, F, A, Body}|T],{undefined, #{ equiv := {call,_,{atom,_,EquivF},Args} = Call} = Meta}, Cwd) ->
    extract_docs([{Kind, Anno, F, A, Body}|T],
                 {io_lib:format("Equivalent to `~ts~ts`",[prefix(Kind),erl_pp:exprs([Call])]),
                  Meta#{ equiv := {EquivF, length(Args)}}}, Cwd);
extract_docs([{function, Anno, F, A, Body}|T],{Doc, Meta}, Cwd) when Doc =/= undefined ->

    %% io:format("Converting ~p/~p~n",[F,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, F, A) of
            undefined ->
                %% Then we check if we can get good names from function arguments
                %% io:format("What: ~p~n",[_E]),
                maybe
                    [{clause, _, ClauseArgs, _, _}] ?= Body,
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, ClauseArgs),
                    {extract_slogan_from_args(F, ClauseArgs), Doc}
                else
                    _E2 ->
                        %% io:format("What: ~p~n",[_E2]),
                        %% Lastly we just print name/arity
                        {io_lib:format("~p/~p",[F,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    [{{function, F, A}, Anno, [unicode:characters_to_binary(Slogan)],
      maybe_hidden_docs(DocsWithoutSlogan), Meta} | extract_docs(T, {undefined, #{}}, Cwd)];
extract_docs([{attribute, Anno, TypeOrOpaque, {Type, _, TypeArgs}}|T],{Doc, Meta}, Cwd)
  when Doc =/= undefined, TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->

    %% io:format("Converting ~p/~p~n",[Type,length(Args)]),
    Args = fun_to_varargs(TypeArgs),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, Type, length(Args)) of
            undefined ->
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(Type, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[Type,length(Args)]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    [{{type, Type, length(Args)}, Anno, [unicode:characters_to_binary(Slogan)],
      maybe_hidden_docs(DocsWithoutSlogan), Meta} | extract_docs(T, {undefined, #{}}, Cwd)];
extract_docs([{attribute, Anno, callback, {{CB, A}, [Fun]}}|T],{Doc, Meta}, Cwd) when Doc =/= undefined ->

    %% io:format("Converting ~p/~p~n",[CB,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, CB, A) of
            undefined ->
                Args = fun_to_varargs(Fun),
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(CB, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[CB,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,

    [{{callback, CB, A}, Anno, [unicode:characters_to_binary(Slogan)],
      maybe_hidden_docs(DocsWithoutSlogan), Meta} | extract_docs(T, {undefined, #{}}, Cwd)];
extract_docs([_H|T], Doc, Cwd) ->
    %% [io:format("Skipping: ~p ~p~n",[{element(3,_H),element(4,_H)}, Doc]) || element(1,_H) =:= function],
    extract_docs(T, Doc, Cwd);
extract_docs([], {undefined, _}, _Cwd) ->
    [].

maybe_hidden_docs(hidden) ->
    hidden;
maybe_hidden_docs(Docs) ->
    #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }.


prefix(function) -> "";
prefix(type) -> "t:";
prefix(callback) -> "c:".

fun_to_varargs({type, _, bounded_fun, [T|_]}) ->
    fun_to_varargs(T);
fun_to_varargs({type, _, 'fun', [{type,_,product,Args}|_] }) ->
    [fun_to_varargs(Arg) || Arg <- Args];
fun_to_varargs({ann_type, _, [Name|_]}) ->
    Name;
fun_to_varargs({var,_,_} = Name) ->
    Name;
fun_to_varargs(Else) ->
    Else.

extract_slogan(hidden, _F, _A) ->
    undefined;
extract_slogan(Doc, F, _A) ->
    maybe
        [MaybeSlogan | Rest] = string:split(Doc, "\n"),
        case string:prefix(MaybeSlogan,atom_to_list(F)) of
            nomatch -> undefined;
            _ -> {MaybeSlogan, Rest}
        end
        %% %% io:format("  MaybeSlogan: ~p~n",[MaybeSlogan]),
        %% {ok, Toks, _} ?= erl_scan:string(unicode:characters_to_list(["-spec ",MaybeSlogan,"."])),
        %% {ok, [{call,_,{atom,_,F},Args}]} ?= erl_parse:parse_exprs(Toks),
        %% _A ?= length(Args),
        %% {MaybeSlogan, Rest}
    else
        _ -> undefined
    end.

extract_slogan_from_args(F, Args) ->
    io_lib:format("~p(~ts)",[F, lists:join(", ",[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).

expand_anno(AST) ->
    {NewAST, _} =
        lists:mapfoldl(fun F({attribute, _, file, {NewFile, _}} = E, File) when NewFile =/= File ->
                               F(E, NewFile);
                           F(E, File) ->
                               {setelement(2, E, erl_anno:set_file(File, element(2, E))), File}
                       end, undefined, AST),
    %% io:format("NewAST: ~p~n",[NewAST]),
    NewAST.

convert_docs(#docs_v1{ format = ?NATIVE_FORMAT, module_doc = #{ <<"en">> := ModuleDoc } } = D) ->
    D#docs_v1{ format = <<"text/markdown">>,
               module_doc = #{ <<"en">> =>
                                   try eep48_to_markdown:render_docs(shell_docs:normalize(ModuleDoc), D)
                                   catch E:R:ST ->
                                           io:format("Failed to convert moduledoc~n"),
                                           erlang:raise(E,R,ST)
                                   end},
               docs = [convert_docs(F, D) || F <- D#docs_v1.docs] };
convert_docs(#docs_v1{ format = ?NATIVE_FORMAT, module_doc = hidden } = D) ->
    D#docs_v1{ format = <<"text/markdown">> };
convert_docs(#docs_v1{ format = <<"text/markdown">> } = D) ->
    %% Already converted
    D.

convert_docs({What, Anno, Sig, #{ <<"en">> := Docs }, Meta}, D) ->
    try
        {What, Anno, Sig, #{ <<"en">> => eep48_to_markdown:render_docs(shell_docs:normalize(Docs), D) }, Meta}
    catch E:R:ST ->
            io:format("Failed to convert ~p~n",[What]),
            erlang:raise(E,R,ST)
    end;
convert_docs(F, _) ->
    F.
