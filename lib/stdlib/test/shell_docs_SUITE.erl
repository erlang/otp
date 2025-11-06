%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

-module(shell_docs_SUITE).
-moduledoc false.

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
   init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).

-export([render/1, links/1, normalize/1, render_prop/1,render_non_native/1, ansi/1, columns/1, render_man/1]).
-export([render_function/1, render_type/1, render_callback/1, doctests/1]).

-export([render_all/1, update_render/0, update_render/1]).

-export([execute/3]).

-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap,{minutes,30}}].

all() ->
    [ {group, render},
      {group, prop},
      {group, render_smoke},
      ansi, columns,
      doctests
    ].


groups() ->
    [ {prop,[],[render_prop]},
      {render, [], [render, render_non_native, links, normalize, render_man]},
      {render_smoke, [], [render_function, render_type, render_callback]}
    ].

%% Include a spec here in order to test that specs of undocumented functions
%% is rendered correctly.
-spec init_per_suite(Config1) -> Config2 when
      Config1 :: list({atom(),term()}),
      Config2 :: list({atom(),term()}).
init_per_suite(Config) ->
    {ok, ?MODULE} = c:c(?MODULE,[debug_info]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(prop, Config) ->
    ct_property_test:init_per_suite(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Env = [{App, Key, application:get_env(App, Key)}
           || {App, Key} <- [{kernel, shell_docs_ansi},
                             {stdlib, shell_docs_columns}]],
    [{env, Env} | Config].

end_per_testcase(_TestCase, Config) ->
    lists:foreach(
      fun({App, Key, undefined}) ->
              application:unset_env(App, Key);
         ({App, Key, {ok, Val}}) ->
              application:set_env(App, Key, Val)
      end,
      proplists:get_value(env, Config)).

%% We keep the docs of a couple of complex modules
%% in the data_dir in order to compare then with the original
%% when we fix bugs so that we don't break anything.
%%
%% This testcase is always run so that we do now forget to
%% check that any bugfix does not break the current behaviour.
%%
%% If you do a bugfix that does not break this testcase when
%% fixed you should include that module in the list of modules
%% tested.
%%
%% Currently the modules are:
-define(RENDER_MODULES, [sofs, re, file, erlang, user_drv, ?MODULE]).
%% If you need to update the definition because this
%% testcase fails, just run update_render/0,1.
render(Config) ->

    DataDir = proplists:get_value(data_dir, Config),

    lists:foreach(
      fun(Module) ->
              maps:map(
                fun(FName, Current) ->
                        case read_file(filename:join(DataDir,FName)) of
                            {ok, Original} when Original =:= Current ->
                                ok;
                            {ok, Original} ->
                                ct:log("Filename: ~ts",[FName]),
                                ct:log("Original: ~n~ts",[Original]),
                                ct:log("Current : ~n~ts",[Current]),
                                ct:fail(output_changed);
                            {error, enoent} ->
                                %% All modules are not available on all
                                %% platforms. For instance socket is not
                                %% available on windows.
                                ok
                        end
                end, render_module(Module, DataDir))
      end, ?RENDER_MODULES).

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, B} ->
            strip_comment(B);
        Else -> Else
    end.

strip_comment(Data) ->
    case re:replace(Data, "^%.*\n", "", [{return, binary}]) of
        Data -> {ok, Data};
        NewData -> strip_comment(NewData)
    end.

update_render() ->
    update_render(
      filename:join([os:getenv("ERL_TOP"),
                     "lib", "stdlib", "test", "shell_docs_SUITE_data"])).
update_render(DataDir) ->
    os:cmd("git rm " ++ filename:join(DataDir, "*.txt"), #{ exception_on_failure => true }),
    os:cmd("git rm " ++ filename:join(DataDir, "*.docs_v1"), #{ exception_on_failure => true }),
    ok = filelib:ensure_path(DataDir),
    lists:foreach(
      fun(Module) ->
              case code:get_doc(Module) of
                  {ok, Docs} ->
                      NewEntries =
                          case beam_lib:chunks(find_path(Module),[abstract_code]) of
                              {ok,{Module,[{abstract_code,{raw_abstract_v1,AST}}]}} ->
                                  lists:map(fun({{Type, F, A}, Anno, Sig, #{} = Doc, Meta} = E) ->

                                                    case lists:search(
                                                           fun({attribute, _, spec, {FA, _}}) when Type =:= function ->
                                                                   FA =:= {F,A};
                                                              ({attribute, _, What, {Name, _, Args}}) when What =:= Type; What =:= opaque andalso Type =:= type ->
                                                                   {Name,length(Args)} =:= {F,A};
                                                              (_) ->
                                                                   false
                                                           end, AST) of
                                                        {value, Signature} ->
                                                            {{Type, F, A}, Anno, Sig, Doc, Meta#{ specification => [Signature] }};
                                                        _ -> throw({did_not_find, E})
                                                    end;
                                               (E) -> E

                                            end, Docs#docs_v1.docs);
                              {ok,{shell_docs_SUITE,[{abstract_code,no_abstract_code}]}} ->
                                  Docs#docs_v1.docs
                          end,

                      Name = filename:join(DataDir, atom_to_list(Module) ++ ".docs_v1"),

                      ok = file:write_file(Name,
                             io_lib:format("~ts\n~w.",[header(), Docs#docs_v1{ docs = NewEntries }])),
                      os:cmd("git add " ++ Name, #{ exception_on_failure => true });
                  {error, _} ->
                      ok
              end,
              maps:map(
                fun(FName, Output) ->
                        FullName = filename:join(DataDir, FName),
                        ok = file:write_file(FullName, [header(), Output]),
                        os:cmd("git add " ++ FullName, #{ exception_on_failure => true })
                end, render_module(Module, DataDir))
      end, ?RENDER_MODULES).

header() ->
    {{YY, _, _}, _} = erlang:localtime(),

    Format = """
        %% %CopyrightBegin%
        %%
        %% SPDX-License-Identifier: Apache-2.0
        %%
        %% Copyright Ericsson AB 2021-~p. All Rights Reserved.
        %%
        %% %CopyrightEnd%
        
        """,
    io_lib:format(Format, [YY]).

find_path(Module) ->
    maybe
        preloaded ?= code:which(Module),
        PreloadedPath = filename:join(code:lib_dir(erts),"ebin"),
        filename:join(PreloadedPath, atom_to_list(Module) ++ ".beam")
    else
        Other -> Other
    end.

handle_error({error,_}) ->
  ok;
handle_error(Doc) ->
  unicode:characters_to_binary(Doc).

only_if_smp(Func) ->
    only_if_smp(4, Func).
only_if_smp(Schedulers, Func) ->
    case erlang:system_info(schedulers_online) of
      N when N < Schedulers -> {skip,"Too few schedulers online"};
      _ -> Func()
    end.

%%
%% Render function
%%
%% This function tests that OTP code base can print its documentation
%% in the shell. It is a time consuming operation that can take
%% up-to 40 - 50 min if run in a single processor (1 scheduler config) machine.
%%
%% Skip the test case when running in a machine with not enough SMP.
%%
%% OBS. render_type/render_callback have shorter times and do not need
%%      to be skipped, regardless of the number of available schedulers.
%% 
render_function(_Config) ->
  only_if_smp(fun render_function_do/0).

render_function_do() ->
    docsmap(
      fun(Mod, D) ->
              DHTML = markdown_to_shelldoc(D),
              Exports = try Mod:module_info(exports)
                        catch _:undef -> []
                        end, %% nif file not available on this platform
              pmap(
                fun(Config) ->
                        try
                            handle_error(shell_docs:render(Mod, D, Config)),
                            [try
                                 handle_error(shell_docs:render(Mod, F, A, DHTML, Config))
                             catch _E:R:ST ->
                                     io:format("Failed to render ~p:~p/~p~n~p:~p~n~p~n",
                                               [Mod,F,A,R,ST,shell_docs:get_doc(Mod,F,A)]),
                                     erlang:raise(error,R,ST)
                             end || {F,A} <- Exports]
                        catch throw:R:ST ->
                                io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
                                exit(R)
                        end
                end, format_configurations())
      end),
    ok.

render_type(_Config) ->
    docsmap(
      fun(Mod, #docs_v1{ docs = Docs } = D) ->
              DHTML = markdown_to_shelldoc(D),
              pmap(
                fun(Config) ->
                        try
                          handle_error(shell_docs:render_type(Mod, D, Config)),
                          [try
                             handle_error(shell_docs:render_type(Mod, T, A, DHTML, Config))
                           catch _E:R:ST ->
                               io:format("Failed to render type ~p:~p/~p~n~p:~p~n~p~n",
                                         [Mod,T,A,R,ST,shell_docs:get_type_doc(Mod,T,A)]),
                               erlang:raise(error,R,ST)
                           end || {{type,T,A},_,_,_,_} <- Docs]
                        catch throw:R:ST ->
                            io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
                            exit(R)
                        end
                end, format_configurations())
      end),
  ok.

render_callback(_Config) ->
    docsmap(
      fun(Mod, #docs_v1{ docs = Docs } = D) ->
              DHTML = markdown_to_shelldoc(D),
              pmap(
                fun(Config) ->
                        try
                          handle_error(shell_docs:render_callback(Mod, D, Config)),
                          [try
                             handle_error(shell_docs:render_callback(Mod, T, A, DHTML, Config))
                           catch _E:R:ST ->
                               io:format("Failed to render callback ~p:~p/~p~n~p:~p~n~p~n",
                                         [Mod,T,A,R,ST,shell_docs:get_callback_doc(Mod,T,A)]),
                               erlang:raise(error,R,ST)
                           end || {{callback,T,A},_,_,_,_} <- Docs]
                        catch throw:R:ST ->
                            io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
                            exit(R)
                        end
                end, format_configurations())
      end),
    ok.

render_man(_Config) ->
    Old_ERL_TOP = os:getenv("ERL_TOP"),
    case Old_ERL_TOP of
        false -> os:putenv("ERL_TOP", code:root_dir());
        _ -> ok
    end,
    docsmap(
        fun(Mod, #docs_v1{metadata = Metadata} = D) ->
            try
                Path1 = case Metadata of
                    #{source_path := Path} -> Path;
                    #{} -> proplists:get_value(source, proplists:get_value(compile, Mod:module_info()))
                end,
                man_docs:module_to_manpage(Mod, Path1, D, "3")
            catch _E:R:ST ->
                io:format("Failed to render man page for ~p~n~p:~p~n~p~n",
                          [Mod,R,ST,D]),
                exit(R)
            end
        end),
    case Old_ERL_TOP of
        false -> os:unsetenv("ERL_TOP");
        _ -> os:putenv("ERL_TOP", Old_ERL_TOP)
    end,
    ok.

docsmap(Fun) ->
  F = fun F({Mod,_,_}) ->
            F(Mod);
          F(Mod) when is_list(Mod) ->
            F(list_to_atom(Mod));
          F(Mod) ->
            case code:get_doc(Mod) of
              {error, missing} ->
                ok;
              {error, cover_compiled} ->
                ok;
              {error, E} when E =:= eperm; E =:= eacces; E =:= eio ->
                %% This can happen in BSD's for some reason...
                ok;
              {error, eisdir} ->
                %% Uhm?
                ok;
              {ok, Docs} ->
                try
                  _ = Fun(Mod, Docs),
                  {ok, self(), Mod}
                catch E:R:ST ->
                    io:format("Failed to render ~p~n~p:~p:~p~n",[Mod,E,R,ST]),
                    erlang:raise(E,R,ST)
                end
            end
      end,
  lists:foreach(F, code:all_available()),
  ok.


format_configurations() ->
  [#{},
   #{ ansi => false },
   #{ ansi => true },
   #{ columns => 5 },
   #{ columns => 150 },
   #{ encoding => unicode },
   #{ encoding => latin1 }].

markdown_to_shelldoc(#docs_v1{format = Format}=Docs) ->
    DefaultFormat = <<"text/markdown">>,
    DFormat = binary_to_list(DefaultFormat),
    case Format of
        _ when Format =:= DefaultFormat orelse Format =:= DFormat ->
            ModuleDoc = Docs#docs_v1.module_doc,
            Doc = Docs#docs_v1.docs,
            Docs#docs_v1{format = ?NATIVE_FORMAT,
                         module_doc = process_moduledoc(ModuleDoc),
                         docs = process_doc_attr(Doc)};
        _  ->
            Docs
    end.

-spec process_moduledoc(Doc :: map() | none | hidden) -> map() | none | hidden.
process_moduledoc(Doc) when Doc =:= none orelse Doc =:= hidden ->
    Doc;
process_moduledoc(Doc) when is_map(Doc) ->
    maps:map(fun (_K, V) -> shell_docs_markdown:parse_md(V) end, Doc).

process_doc_attr(Doc) ->
    lists:map(fun process_doc/1, Doc).

process_doc(Docs) when is_list(Docs) ->
    lists:map(fun process_doc/1, Docs);
process_doc({_At, _A, _S, Doc, _M}=Entry) when Doc =:= none orelse Doc =:= hidden ->
    Entry;
process_doc({Attributes, Anno, Signature, Doc, Metadata}) ->
    Docs = maps:map(fun (_K, V) -> shell_docs_markdown:parse_md(V) end, Doc),
    {Attributes, Anno, Signature, Docs, Metadata}.


render_prop(Config) ->
%    dbg:tracer(),dbg:p(all,c),dbg:tpl(shell_docs_prop,[]),
    ct_property_test:quickcheck(
      shell_docs_prop:prop_render(),Config).

links(_Config) ->
    docsmap(
      fun(Mod, #docs_v1{ module_doc = MDoc, docs = Docs, format = ?NATIVE_FORMAT }) ->
              try
                  [check_links(Mod, maps:get(<<"en">>,MDoc)) || MDoc =/= none, MDoc =/= hidden]
              catch _E1:R1:ST1 ->
                      io:format("Failed to render ~p~n~p:~p~n~p~n",
                                [Mod,R1,ST1,MDoc]),
                      erlang:raise(error,R1,ST1)
              end,
              [try
                  check_links(Mod, D)
               catch _E:R:ST ->
                       io:format("Failed to render ~p:~p~n~p:~p~n~p~n",
                                 [Mod,Kind,R,ST,D]),
                       erlang:raise(error,R,ST)
               end || {Kind,_Anno,_Sig,#{ <<"en">> := D },_MD} <- Docs];
         (Mod, #docs_v1{ format = Fmt }) ->
              io:format("Skipping ~p because format is ~ts~n",
                        [Mod, Fmt])
      end).

check_links(Mod, [{a,Attr,C}|T]) ->
    case proplists:get_value(rel,Attr) of
        <<"https://erlang.org/doc/link/seemfa">> ->
            case string:lexemes(proplists:get_value(href, Attr),":#/") of
                [_App,TargetMod,Func,Arity] ->
                    case code:get_doc(b2a(TargetMod)) of
                        {ok, _ModDoc} ->
                            case shell_docs:get_doc(b2a(TargetMod),
                                                    b2a(Func),
                                                    binary_to_integer(Arity)) of
                                [] -> throw({could_not_find,b2a(TargetMod),
                                                b2a(Func),
                                                binary_to_integer(Arity)});
                                _Else  -> ok
                            end;
                        _Else ->
                            throw({could_not_find, TargetMod, Attr})
                    end;
                _Callback ->
                    ok
            end;
        _OtherlinkType ->
            ok
    end,
    check_links(Mod, C),
    check_links(Mod, T);
check_links(Mod, [{_,_,C}|T]) ->
    check_links(Mod, C),
    check_links(Mod, T);
check_links(Mod, [C|T]) when is_binary(C) ->
    check_links(Mod, T);
check_links(_, []) ->
    ok.

normalize(_Config) ->
    ?assertMatch(
       [{p,[],[{em,[],[<<"a ">>,{code,[],[<<"b ">>]},<<"c">>]}]}],
       shell_docs:normalize([{p,[],[{em,[],[<<" a ">>,{code,[],[<<" b  ">>]},<<" c">>]}]}])
      ),
    ?assertMatch(
       [{'div',[],[<<"!">>]}],
       shell_docs:normalize([{'div',[],[{code,[],[<<" ">>,{i,[],[<<" ">>]}]},<<" !">>]}])
      ),
    ok.

%% Special binary_to_atom that deals with <<"'and'">>
b2a(Bin) ->
    case erl_scan:string(binary_to_list(Bin)) of
        {ok,[{atom,_,A}],_} -> A;
        {ok,[{A,_}],_} -> A
    end.

%% Test rendering of non-native modules
render_non_native(_Config) ->
    Docs = #docs_v1{
        anno = erl_anno:new(13),
        beam_language = not_erlang,
        format = <<"text/asciidoc">>,
        module_doc = #{<<"en">> => <<"This is\n\npure text">>},
        docs = []
    },

    <<"\n\tnot_an_erlang_module\n\n"
      "    This is\n"
      "    \n"
      "    pure text\n">> =
        unicode:characters_to_binary(
          shell_docs:render(not_an_erlang_module, Docs, #{ ansi => false })),

    ok.

%% Testing functions
render_all(Dir) ->
    file:make_dir(Dir),
    %% We load all possible application in order to be able to use
    %% application:get_application to get the application a module
    %% belongs to.
    PossibleApplications =
        lists:flatmap(
          fun(P) ->
                  filelib:wildcard(filename:join(P, "*.app"))
          end, code:get_path()),
    [application:load(list_to_atom(filename:basename(filename:rootname(App)))) ||
        App <- PossibleApplications],
    docsmap(fun(Mod, D) ->
                    maps:map(
                      fun(FName, Value) ->
                              file:write_file(filename:join(Dir, FName), Value)
                      end, render_module(Mod, D))
            end).

render_module(Mod, #docs_v1{ docs = Docs } = D) ->
    put({shell_docs, nospecs}, non_existing),
    Opts = #{ ansi => true, columns => 80, encoding => unicode },
    case application:get_application(Mod) of
        {ok, App} ->
            App;
        _ ->
            App = unknown
    end,
    SMod = atom_to_list(App) ++ "_" ++ atom_to_list(Mod),
    Files =
        #{
          SMod ++ ".txt" =>
              unicode:characters_to_binary(shell_docs:render(Mod, D, Opts)),
          SMod ++ "_type.txt" =>
              unicode:characters_to_binary(shell_docs:render_type(Mod, D, Opts)),
          SMod ++ "_cb.txt" =>
                    unicode:characters_to_binary(shell_docs:render_callback(Mod, D, Opts))
         },
    lists:foldl(
      fun({_Type,_Anno,_Sig,none,_Meta}, Acc) ->
              Acc;
         ({{function,Name,Arity},_Anno,_Sig,_Doc,_Meta}, Acc) ->
              FAName = SMod ++ "_"++atom_to_list(Name)++"_"++integer_to_list(Arity)++"_func.txt",
              FName = SMod ++ "_"++atom_to_list(Name)++"_func.txt",
              FADocs = unicode:characters_to_binary(shell_docs:render(Mod, Name, Arity, D, Opts)),
              FDocs = unicode:characters_to_binary(shell_docs:render(Mod, Name, D, Opts)),
              case string:equal(FADocs,FDocs) of
                  true -> 
                      Acc#{ sanitize(FAName) => FADocs };
                  false ->
                      Acc#{ sanitize(FAName) => FADocs,
                            sanitize(FName) => FDocs}
              end;
         ({{type,Name,Arity},_Anno,_Sig,_Doc,_Meta}, Acc) ->
              FName = SMod ++ "_"++atom_to_list(Name)++"_"++integer_to_list(Arity)++"_type.txt",
              Acc#{ sanitize(FName) =>
                        unicode:characters_to_binary(shell_docs:render_type(Mod, Name, Arity, D, Opts))};
         ({{callback,Name,Arity},_Anno,_Sig,_Doc,_Meta}, Acc) ->
              FName = SMod ++ "_"++atom_to_list(Name)++"_"++integer_to_list(Arity)++"_cb.txt",
              Acc#{ sanitize(FName) =>
                        unicode:characters_to_binary(shell_docs:render_callback(Mod, Name, Arity, D, Opts))}
      end, Files, Docs);
render_module(Mod, Datadir) ->
    {ok, [Docs]} = file:consult(filename:join(Datadir, atom_to_list(Mod) ++ ".docs_v1")),
    render_module(Mod, Docs).

sanitize(FName) ->
    lists:foldl(
      fun({Re,Replace}, Txt) ->
              re:replace(Txt,Re,Replace,[global,{return,list}])
      end, FName, [{"/","slash"},{":","colon"},
                   {"\\*","star"},{"<","lt"},{">","gt"},{"=","eq"}]).

ansi(_Config) ->
    {ok, Docs} = code:get_doc(?MODULE),

    HasESC =
        fun(Config) ->
                Doc = shell_docs:render(?MODULE, Docs, Config),
                string:find(Doc, "\e") =/= nomatch
        end,

    application:set_env(kernel, shell_docs_ansi, true),
    ?assert(HasESC(#{})),
    ?assertNot(HasESC(#{ansi => false})),
    ?assert(HasESC(#{ansi => true})),

    application:set_env(kernel, shell_docs_ansi, false),
    ?assertNot(HasESC(#{})),
    ?assertNot(HasESC(#{ansi => false})),
    ?assert(HasESC(#{ansi => true})),

    ok.

-doc """
Doc doc doc doc doc doc doc doc doc doc doc doc doc doc doc.
""".
columns(_Config) ->
    {ok, Docs} = code:get_doc(?MODULE),

    MaxColumns =
        fun(Config0) ->
                Config = maps:merge(#{ansi => false}, Config0),
                Doc = shell_docs:render(?MODULE, ?FUNCTION_NAME, Docs, Config),
                Lines = string:split(Doc, "\n", all),
                lists:max(lists:map(fun string:length/1, Lines))
        end,

    application:set_env(stdlib, shell_docs_columns, 30),
    ?assert(MaxColumns(#{}) =< 30),
    ?assert(MaxColumns(#{columns => 20}) =< 20),

    application:set_env(stdlib, shell_docs_columns, not_an_integer),
    ?assert(MaxColumns(#{}) > 30),

    application:set_env(stdlib, shell_docs_columns, 0),
    ?assert(MaxColumns(#{}) > 30),

    application:set_env(stdlib, shell_docs_columns, -30),
    ?assert(MaxColumns(#{}) > 30),

    application:unset_env(stdlib, shell_docs_columns),
    ?assert(MaxColumns(#{}) > 30),
    ?assert(MaxColumns(#{columns => 20}) =< 20),

    ok.

doctests(_Config) ->
    shell_docs:test(
      shell_docs_test,
      [
       {{function, module, 2}, erl_eval:add_binding('Prebound', hello,
                                                   erl_eval:new_bindings())}
      ]),
    ok.

%%
%% Parallel map function.
%%
%% Parallel map function that discards the result of the function
%% `F` applied to each of the items in `Ls`. It spawns as many
%% processes as items there are in `Ls`. If the list is large,
%% consider adding a set of working processes that round-robin on
%% the job to do be done.
%%
%% - `F` is the function to perform
%% - `Ls` the list of items to iterate on
%%
pmap(F, Ls) when is_function(F),
                 is_list(Ls) ->
  _ = lists:foreach(fun(Config) ->
                        spawn_link(?MODULE, execute,[Config, F, self()])
                   end, Ls),
  ResponseCounter = length(Ls),
  ok = sync(ResponseCounter),
  ok.

execute(Item, F, Pid) ->
  _ = F(Item),
  Pid ! ok.

sync(0) ->
  ok;
sync(N) ->
  receive
    ok ->
      sync(N-1)
  end.
