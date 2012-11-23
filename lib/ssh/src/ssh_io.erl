%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

%%% Description: user interaction for SSH

-module(ssh_io).

-export([yes_no/2, read_password/2, read_line/2, format/2]).
-import(lists, [reverse/1]).
-include("ssh.hrl").

read_line(Prompt, Ssh) ->
    format("~s", [listify(Prompt)]),
    proplists:get_value(user_pid, Ssh) ! {self(), question},
    receive
	Answer ->
	    Answer
    end.

yes_no(Prompt, Ssh) ->
    io:format("~s [y/n]?", [Prompt]),
    proplists:get_value(user_pid, Ssh#ssh.opts) ! {self(), question},
    receive
	Answer ->
	    case trim(Answer) of
		"y" -> yes;
		"n" -> no;
		"Y" -> yes;
		"N" -> no;
		y -> yes;
		n -> no;
		_ ->
		    io:format("please answer y or n\n"),
		    yes_no(Prompt, Ssh)
	    end
    end.


read_password(Prompt, Ssh) ->
    format("~s", [listify(Prompt)]),
    case is_list(Ssh) of
	false ->
	    proplists:get_value(user_pid, Ssh#ssh.opts) ! {self(), user_password};
	_ ->
	    proplists:get_value(user_pid, Ssh) ! {self(), user_password}
    end,
    receive
	Answer ->
	    case Answer of
		"" ->
		    read_password(Prompt, Ssh);
		Pass -> Pass
	    end
    end.

listify(A) when is_atom(A) ->
    atom_to_list(A);
listify(L) when is_list(L) ->
    L.

format(Fmt, Args) ->
    io:format(Fmt, Args).


trim(Line) when is_list(Line) ->
    reverse(trim1(reverse(trim1(Line))));
trim(Other) -> Other.

trim1([$\s|Cs]) -> trim(Cs);
trim1([$\r|Cs]) -> trim(Cs);
trim1([$\n|Cs]) -> trim(Cs);
trim1([$\t|Cs]) -> trim(Cs);
trim1(Cs) -> Cs.
    


