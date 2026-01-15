%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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

%%

%%% Description: user interaction for SSH

-module(ssh_io).
-moduledoc false.

-export([yes_no/2, read_password/2, read_line/2, format/2]).
-include("ssh.hrl").

read_line(Prompt, _Opts) ->
    format("~s", [listify(Prompt)]),
    unicode:characters_to_list(io:get_line("")).

yes_no(Prompt, Opts) ->
    format("~s [y/n]?", [Prompt]),
    case trim(io:get_line("")) of
        "y" -> yes;
        "n" -> no;
        "Y" -> yes;
        "N" -> no;
        _ ->
            format("please answer y or n\n",[]),
            yes_no(Prompt, Opts)
    end.

read_password(Prompt, Opts) ->
    format("~s", [listify(Prompt)]),
    case trim(io:get_password(group_leader())) of
        "" ->
            read_password(Prompt, Opts);
        Pwd ->
            Pwd
    end.

format(Fmt, Args) ->
    io:format(Fmt, Args).

%%%================================================================
listify(A) when is_atom(A)   -> atom_to_list(A);
listify(L) when is_list(L)   -> L;
listify(B) when is_binary(B) -> binary_to_list(B).


trim(Line) when is_list(Line) ->
    lists:reverse(trim1(lists:reverse(trim1(Line))));
trim(Line) when is_binary(Line) ->
    trim(unicode:characters_to_list(Line));
trim(Other) -> Other.

trim1([$\s|Cs]) -> trim(Cs);
trim1([$\r|Cs]) -> trim(Cs);
trim1([$\n|Cs]) -> trim(Cs);
trim1([$\t|Cs]) -> trim(Cs);
trim1(Cs) -> Cs.
