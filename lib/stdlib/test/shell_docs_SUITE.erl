%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
   init_per_group/2, end_per_group/2]).

-export([render/1, render_smoke/1, links/1, normalize/1, render_prop/1,
         render_non_native/1]).

-export([render_all/1, update_render/0, update_render/1]).

-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap,{minutes,20}}].

all() ->
    [render_smoke, render, render_non_native, links, normalize, {group, prop}].

groups() ->
    [{prop,[],[render_prop]}].

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

end_per_group(_GroupName, Config) ->
    Config.

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
              {ok, [D]} = file:consult(filename:join(DataDir, atom_to_list(Module) ++ ".docs_v1")),
              maps:map(
                fun(FName, Current) ->
                        case file:read_file(filename:join(DataDir,FName)) of
                            {ok, Original} when Original =:= Current ->
                                ok;
                            {ok, Original} ->
                                ct:log("Original: ~n~ts",[Original]),
                                ct:log("Current : ~n~ts",[Current]),
                                ct:fail(output_changed);
                            {error, enoent} ->
                                %% All modules are not available on all
                                %% platforms. For instance socket is not
                                %% available on windows.
                                ok
                        end
                end, render_module(Module, D))
      end, ?RENDER_MODULES).

update_render() ->
    update_render(
      filename:join([os:getenv("ERL_TOP"),
                     "lib", "stdlib", "test", "shell_docs_SUITE_data"])).
update_render(DataDir) ->
    lists:foreach(
      fun(Module) ->
              case code:get_doc(Module) of
                  {ok, D} ->
                      ok = file:write_file(
                             filename:join(DataDir, atom_to_list(Module) ++ ".docs_v1"),
                             io_lib:format("~w.",[D])),
                      maps:map(
                        fun(FName, Output) ->
                                ok = file:write_file(filename:join(DataDir, FName), Output)
                        end, render_module(Module, D));
                  E ->
                      io:format("Error processing: ~p ~p",[Module, E])
              end
      end, ?RENDER_MODULES).

render_smoke(_Config) ->
    docsmap(
      fun(Mod, #docs_v1{ docs = Docs } = D) ->
              lists:foreach(
                fun(Config) ->
                        try
                            E = fun({error,_}) ->
                                        ok;
                                   (Doc) ->
                                        unicode:characters_to_binary(Doc)
                                end,
                            E(shell_docs:render(Mod, D, Config)),
                            E(shell_docs:render_type(Mod, D, Config)),
                            E(shell_docs:render_callback(Mod, D, Config)),
                            Exports = try Mod:module_info(exports)
                                      catch _:undef -> []
                                      end, %% nif file not available on this platform

                            [try
                                 E(shell_docs:render(Mod, F, A, D, Config))
                             catch _E:R:ST ->
                                     io:format("Failed to render ~p:~p/~p~n~p:~p~n~p~n",
                                               [Mod,F,A,R,ST,shell_docs:get_doc(Mod,F,A)]),
                                     erlang:raise(error,R,ST)
                             end || {F,A} <- Exports],
                            [try
                                 E(shell_docs:render_type(Mod, T, A, D, Config))
                             catch _E:R:ST ->
                                     io:format("Failed to render type ~p:~p/~p~n~p:~p~n~p~n",
                                               [Mod,T,A,R,ST,shell_docs:get_type_doc(Mod,T,A)]),
                                     erlang:raise(error,R,ST)
                             end || {{type,T,A},_,_,_,_} <- Docs],
                            [try
                                 E(shell_docs:render_callback(Mod, T, A, D, Config))
                             catch _E:R:ST ->
                                     io:format("Failed to render callback ~p:~p/~p~n~p:~p~n~p~n",
                                               [Mod,T,A,R,ST,shell_docs:get_callback_doc(Mod,T,A)]),
                                     erlang:raise(error,R,ST)
                             end || {{callback,T,A},_,_,_,_} <- Docs]
                        catch throw:R:ST ->
                                io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
                                exit(R)
                        end
                end, [#{},
                      #{ ansi => false },
                      #{ ansi => true },
                      #{ columns => 5 },
                      #{ columns => 150 },
                      #{ encoding => unicode },
                      #{ encoding => latin1 }])
      end),
    ok.

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
      end, Files, Docs).

sanitize(FName) ->
    lists:foldl(
      fun({Re,Replace}, Txt) ->
              re:replace(Txt,Re,Replace,[global,{return,list}])
      end, FName, [{"/","slash"},{":","colon"},
                   {"\\*","star"},{"<","lt"},{">","gt"},{"=","eq"}]).

docsmap(Fun) ->
    lists:map(
      fun F({Mod,_,_}) ->
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
                          Fun(Mod, Docs)
                      catch E:R:ST ->
                              io:format("Failed to render ~p~n~p:~p:~p~n",[Mod,E,R,ST]),
                              erlang:raise(E,R,ST)
                      end
              end
      end, code:all_available()).
