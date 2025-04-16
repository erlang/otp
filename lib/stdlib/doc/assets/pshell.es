#!/usr/bin/env escript
%% pshell.es

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

-export([start/0]).
main(_Args) ->
    shell:start_interactive({?MODULE, start, []}),
    timer:sleep(infinity). %% Make sure the escript does not exit

-spec start() -> pid().
start() ->
    spawn(fun() ->
                  io:setopts([{expand_fun, fun expand_fun/1}]),
                  io:format("Starting process inspection shell~n"),
                  loop()
          end).

-spec expand_fun(ReverseLine :: string()) -> {yes, string(), list(string())} |
          {no, nil(), nil()}.
expand_fun("") ->
    {yes, "", ["list", "inspect", "suspend", "resume"]};
expand_fun(Curr) ->
    expand_fun(lists:reverse(Curr), ["list", "inspect", "suspend", "resume"]).

expand_fun(_Curr, []) ->
    {no, "", []};
expand_fun(Curr, [H | T]) ->
    case lists:prefix(Curr, H) of
        true ->
            {yes, lists:reverse(lists:reverse(H) -- lists:reverse(Curr)), []};
        false ->
            expand_fun(Curr, T)
    end.

loop() ->
    case io:get_line("> ") of
        eof -> ok;
        {error, Reason} -> exit(Reason);
        Data -> eval(string:trim(Data))
    end,
    loop().

eval("list") ->
    Format = " ~.10ts | ~.10ts | ~.10ts~n",
    io:format(Format,["Pid", "Name", "MsgQ Len"]),
    [begin
         [{registered_name,Name},{message_queue_len,Len}]
             = erlang:process_info(Pid, [registered_name, message_queue_len]),
         io:format(Format,[to_list(Pid), to_list(Name), to_list(Len)])
     end || Pid <- processes()];
eval("inspect " ++ PidStr) ->
    case parse_pid(PidStr) of
        invalid -> ok;
        Pid ->
            [{registered_name, Name}, {memory, Memory}, {messages, Messages}, {status, Status}] =
                erlang:process_info(Pid, [registered_name, memory, messages, status]),
            io:format("Pid: ~p~nName: ~ts~nStatus: ~p~nMemory: ~p~nMessages: ~p~n",
                      [Pid, to_list(Name), Status, Memory, Messages])
    end;
eval("suspend " ++ PidStr) ->
    case parse_pid(PidStr) of
        invalid -> ok;
        Pid ->
            erlang:suspend_process(Pid),
            io:format("Suspeneded ~ts~n", [Pid])
    end;
eval("resume " ++ PidStr) ->
    case parse_pid(PidStr) of
        invalid -> ok;
        Pid ->
            erlang:resumne_process(Pid),
            io:format("Resumed ~ts~n", [Pid])
    end;
eval(Unknown) ->
    io:format("Unknown command: '~ts'~n",[Unknown]).

to_list(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_list(List) when is_list(List) ->
    List.

parse_pid(PidStr) ->
    try list_to_pid(PidStr)
    catch _:_ -> io:format("Invalid pid format~n"), invalid
    end.