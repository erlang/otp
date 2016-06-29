%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-export([yes_no/2, read_password/2, read_line/2, format/2]).
-include("ssh.hrl").

read_line(Prompt, Ssh) ->
    format("~s", [listify(Prompt)]),
    proplists:get_value(user_pid, Ssh) ! {self(), question},
    receive
	Answer when is_list(Answer) ->
	    Answer
    end.

yes_no(Prompt, Ssh) ->
    format("~s [y/n]?", [Prompt]),
    proplists:get_value(user_pid, Ssh#ssh.opts) ! {self(), question},
    receive
	%% I can't see that the atoms y and n are ever received, but it must
	%% be investigated before removing
	y -> yes;
	n -> no;

	Answer when is_list(Answer) ->
	    case trim(Answer) of
		"y" -> yes;
		"n" -> no;
		"Y" -> yes;
		"N" -> no;
		_ ->
		    format("please answer y or n\n",[]),
		    yes_no(Prompt, Ssh)
	    end
    end.


read_password(Prompt, #ssh{opts=Opts}) -> read_password(Prompt, Opts);
read_password(Prompt, Opts) when is_list(Opts) ->
    format("~s", [listify(Prompt)]),
    proplists:get_value(user_pid, Opts) ! {self(), user_password},
    receive
	Answer when is_list(Answer) ->
	     case trim(Answer) of
		 "" ->
		     read_password(Prompt, Opts);
		 Pwd ->
		     Pwd
	     end
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
