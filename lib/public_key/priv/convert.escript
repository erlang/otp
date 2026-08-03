#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
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

main([InFile,OutFile]) ->
    {ok,In} = file:open(InFile,read),
    {ok,Out} = file:open(OutFile,write),
    write_file(Out, read_file(In)),
    file:close(In),
    file:close(Out).

write_file(D, {ok,#{license := Copyright, data := Ms}}) ->
    io:format(D,'~s~n',[lists:reverse(Copyright)]),
    io:format(D,'-define(dh_default_groups,~n    ~p~n    ).~n',[Ms]).

one_line(Line, Acc) when is_binary(Line) -> 
    one_line(binary_to_list(Line), Acc);
one_line("#"++ Data, #{license:=License} = Acc) ->
    Acc#{license => [[ "%%" | Data]|License]};
one_line(Line, #{data := Data} = Acc) when is_list(Line) ->
    try
        [_Time,_Type,_Tests,_Tries,Size,G,P] = string:tokens(Line," \r\n"),
        Acc#{data =>
                [{list_to_integer(Size),
                        {list_to_integer(G), list_to_integer(P,16)}
                        } | Data]}
    catch
	_:_ -> io:format("*** skip line ~p",[Line]),
	       Acc
    end.


collect_per_size(#{data := L} = Acc0) ->
    Acc0#{data =>
        lists:foldr(
        fun({Sz,GP}, [{Sz,GPs}|Acc]) -> [{Sz,[GP|GPs]}|Acc];
                ({Sz,GP}, Acc) -> [{Sz,[GP]}|Acc]
        end, [], lists:sort(L))}.


read_file(D) ->
    read_file(D, #{license => [], data => []}).

read_file(D, Acc) ->
    case io:get_line(D,"") of
	{error,Error} ->
	    {error,Error};
	eof ->
	    {ok, collect_per_size(Acc)};
	Data ->
	    read_file(D, one_line(Data,Acc))
    end.


