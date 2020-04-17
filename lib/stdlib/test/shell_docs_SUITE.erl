%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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

-export([render/1, links/1, normalize/1, render_prop/1]).

-export([render_all/1]).

-include_lib("kernel/include/eep48.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap,{minutes,10}}].

all() ->
    [render, links, normalize, {group, prop}].

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

render(_Config) ->
    docsmap(
      fun(Mod, #docs_v1{ docs = Docs } = D) ->
              try
                  shell_docs:render(Mod, D),
                  shell_docs:render_type(Mod, D),
                  shell_docs:render_callback(Mod, D),
                  [try
                       shell_docs:render(Mod, F, A, D)
                   catch _E:R:ST ->
                           io:format("Failed to render ~p:~p/~p~n~p:~p~n~p~n",
                                     [Mod,F,A,R,ST,shell_docs:get_doc(Mod,F,A)]),
                           erlang:raise(error,R,ST)
                   end || {F,A} <- Mod:module_info(exports)],
                  [try
                       shell_docs:render_type(Mod, T, A, D)
                   catch _E:R:ST ->
                           io:format("Failed to render type ~p:~p/~p~n~p:~p~n~p~n",
                                     [Mod,T,A,R,ST,shell_docs:get_type_doc(Mod,T,A)]),
                           erlang:raise(error,R,ST)
                   end || {{type,T,A},_,_,_,_} <- Docs],
                  [try
                       shell_docs:render_callback(Mod, T, A, D)
                   catch _E:R:ST ->
                           io:format("Failed to render callback ~p:~p/~p~n~p:~p~n~p~n",
                                     [Mod,T,A,R,ST,shell_docs:get_callback_doc(Mod,T,A)]),
                           erlang:raise(error,R,ST)
                   end || {{callback,T,A},_,_,_,_} <- Docs]
              catch throw:R:ST ->
                      io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
                      exit(R)
              end
      end),
    ok.

render_prop(Config) ->
%    dbg:tracer(),dbg:p(all,c),dbg:tpl(shell_docs_prop,[]),
    ct_property_test:quickcheck(
      shell_docs_prop:prop_render(),Config).

links(_Config) ->
    docsmap(
      fun(Mod, #docs_v1{ module_doc = MDoc, docs = Docs }) ->
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
               end || {Kind,_Anno,_Sig,#{ <<"en">> := D },_MD} <- Docs]
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

%% Testing functions
render_all(Dir) ->
    file:make_dir(Dir),
    docsmap(
      fun(Mod, #docs_v1{ docs = Docs } = D) ->
              SMod = atom_to_list(Mod),
              file:write_file(filename:join(Dir,SMod ++ ".txt"),
                              unicode:characters_to_binary(shell_docs:render(Mod, D))),
              file:write_file(filename:join(Dir,SMod ++ "_type.txt"),
                              unicode:characters_to_binary(shell_docs:render_type(Mod, D))),
              file:write_file(filename:join(Dir,SMod ++ "_cb.txt"),
                              unicode:characters_to_binary(shell_docs:render_callback(Mod, D))),
              lists:foreach(
                fun({{function,Name,Arity},_Anno,_Sig,_Doc,_Meta}) ->
                        FName = SMod ++ "_"++atom_to_list(Name)++"_"++integer_to_list(Arity)++"_func.txt",
                        ok = file:write_file(filename:join(Dir,re:replace(FName,"[/:]","_",
                                                                          [global,{return,list}])),
                                             unicode:characters_to_binary(shell_docs:render(Mod, Name, Arity, D)));
                    ({{type,Name,Arity},_Anno,_Sig,_Doc,_Meta}) ->
                        FName = SMod ++ "_"++atom_to_list(Name)++"_"++integer_to_list(Arity)++"_type.txt",
                        ok = file:write_file(filename:join(Dir,re:replace(FName,"[/:]","_",
                                                                          [global,{return,list}])),
                                             unicode:characters_to_binary(shell_docs:render_type(Mod, Name, Arity, D)));
                   ({{callback,Name,Arity},_Anno,_Sig,_Doc,_Meta}) ->
                        FName = SMod ++ "_"++atom_to_list(Name)++"_"++integer_to_list(Arity)++"_cb.txt",
                        file:write_file(filename:join(Dir,re:replace(FName,"[/:]","_",
                                                                     [global,{return,list}])),
                                        unicode:characters_to_binary(shell_docs:render_callback(Mod, Name, Arity, D)))
                end, Docs)
      end).

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
                  {error, eacces} ->
                      %% This can happen in BSD's for some reason...
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
