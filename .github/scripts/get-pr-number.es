#!/usr/bin/env escript
%%! -pa jsx/_build/default/lib/jsx/ebin/

%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%

-mode(compile).

main([Repo, HeadSha]) ->
    io:format("Looking for: ~ts",[HeadSha]),
    AllOpenPrs = ghapi("gh api --paginate -X GET /repos/"++Repo++"/pulls -f state=open"),
    case lists:search(
           fun(#{ <<"number">> := NR, <<"head">> := #{ <<"sha">> := Sha }}) ->
                   io:format("~p: Checking ~ts~n",[NR, Sha]),
                   string:equal(HeadSha, Sha)
           end, AllOpenPrs) of
        {value, #{ <<"number">> := Number } } ->
            append_to_github_output("result=~p~n", [Number]);
        false ->
            append_to_github_output("result=~ts~n", [""])
    end.

append_to_github_output(Fmt, Args) ->
    case os:getenv("GITHUB_OUTPUT") of
        false ->
            io:format(standard_error, "GITHUB_OUTPUT env var missing?~n", []);
        GitHubOutputFile ->
            {ok, F} = file:open(GitHubOutputFile, [write, append]),
            ok = io:fwrite(F, Fmt, Args),
            ok = file:close(F)
    end.

ghapi(CMD) ->
    json:decode(cmd(CMD)).

cmd(CMD) ->
    ListCmd = unicode:characters_to_list(CMD),
    io:format("cmd: ~ts~n",[ListCmd]),
    unicode:characters_to_binary(os:cmd(ListCmd)).
