%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
-module(beam_compiler_9).

-export([compile/2, test/0, ?MODULE/0]).

?MODULE() ->
    ok.

test() ->
    Failures = failures(),
    compile_files([hd(Failures)]).

compile_files([File | Files]) ->
    Pid = spawn(?MODULE, compile, [File, self()]),
    Time = infinity,
    receive
	done ->
	    compile_files(Files)
    after
	Time ->
	    io:fwrite("Compilation not completed within ~w ms~n", [Time]),
	    exit(Pid, die),
	    compile_files(Files)
    end;

compile_files([]) ->
    done.



compile(File, Parent) ->
    io:fwrite("Compiling:  ~s~n", [File]),
    statistics(runtime),
    statistics(wall_clock),
    statistics(reductions),
    Result = c:c(File),
    {_, Rslc} = statistics(runtime),
    {_, Tslc} = statistics(wall_clock),
    {_, Reds} = statistics(reductions),
    io:fwrite("Result:     ~w~n", [Result]),
    io:fwrite("Reductions: ~w~n", [Reds]),
    io:fwrite("Time:       ~w~n", [Tslc]),
    io:fwrite("Cpu time:   ~w~n", [Rslc]),
    io:nl(),
    Parent ! done.



failures() ->
    [test].
