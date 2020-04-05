#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : github_link.escript
%%
%% Created : 12 Dec 2017 by Lukas Larsson
%%----------------------------------------------------------------------

main([In, Filename, Sha, Out]) ->
    {ok, Bin} = file:read_file(In),

    TagsToAnnotate = ["description", "func", "datatype", "section"],

    Subs = subs(TagsToAnnotate, Filename, Sha, re:split(Bin,[$\n])),

    file:write_file(Out, Subs).

subs([], _, _, Bin) ->
    lists:join("\n", Bin);
subs([Pat|Pats], Fn, Sha, Bin) ->
    subs(Pats, Fn, Sha, sub(Bin, Pat, Fn, Sha)).

sub(Bin, Pat, Fn, Sha) ->
    sub(Bin, Pat, Fn, Sha, 1).
sub([], _Pat, _Fn, _Sha, _Cnt) ->
    [];
sub([H|T], Pat, Fn, Sha, Cnt) ->
    %% We use the maint branch here, it is not as exact as the tag,
    %% but it is the best we can do as github does not allow doing
    %% pullrequests on anything but branches.
    [re:replace(H,["<",Pat,">"],
                    ["<",Pat," ghlink=\"maint/",Fn,"#L",
                     integer_to_list(Cnt),"\">"],[{return,list}]) |
     sub(T, Pat, Fn, Sha, Cnt+1)].
