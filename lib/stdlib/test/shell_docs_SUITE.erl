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

-export([render/1]).

-include_lib("kernel/include/eep48.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    ok.

suite() ->
    [{timetrap,{minutes,10}}].

all() ->
    [render].

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
    Avail = code:all_available(),
    [render_test(Mod) || {Mod,_,_} <- Avail],
    ok.
render_test(Mod) when is_list(Mod) ->
    render_test(list_to_atom(Mod));
render_test(Mod) ->
    try
        case code:get_doc(Mod) of
            {error,missing} ->
                ok;
            {ok, #docs_v1{ docs = Docs } = D} ->
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
        end
    catch throw:R:ST ->
            io:format("Failed to render ~p~n~p:~p~n",[Mod,R,ST]),
            exit(R)
    end.
