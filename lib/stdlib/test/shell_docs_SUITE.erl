%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2019. All Rights Reserved.
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
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([render/1, links/1]).

-include_lib("kernel/include/eep48.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{timetrap,{minutes,10}}].

all() ->
    [render, links].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

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
                   end || {{type,T,A},_,_,_,_} <- Docs]
              catch throw:R:ST ->
                      io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
                      exit(R)
              end
      end).

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

%% Special binary_to_atom that deals with <<"'and'">>
b2a(Bin) ->
    case erl_scan:string(binary_to_list(Bin)) of
        {ok,[{atom,_,A}],_} -> A;
        {ok,[{A,_}],_} -> A
    end.

docsmap(Fun) ->
    lists:map(fun F({Mod,_,_}) ->
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
                              Fun(Mod, Docs)
                      end
              end, code:all_available()).
