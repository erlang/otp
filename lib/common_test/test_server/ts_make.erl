%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(ts_make).

-export([make/1,make/3,unmake/1]).

-include_lib("common_test/include/ct.hrl").

%% Functions to be called from make test cases.

make(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Makefile = proplists:get_value(makefile, Config),
    Make = proplists:get_value(make_command, Config),
    case make(Make, DataDir, Makefile) of
	ok -> ok;
	{error,Reason} -> exit({make_failed,Reason})
    end.

unmake(Config) when is_list(Config) ->
    ok.

%% Runs `make' in the given directory.
%% Result: ok | {error, Reason}

make(Make, Dir, Makefile) ->
    {RunFile, RunCmd, Script} = run_make_script(os:type(), Make, Dir, Makefile),
    case file:write_file(RunFile, Script) of
	ok ->
	    Log = filename:join(Dir, "make.log"),
	    file:delete(Log),
	    Port = open_port({spawn, RunCmd}, [eof,stream,in,stderr_to_stdout]),
	    case get_port_data(Port, [], false) of
		"*ok*" ++ _ -> ok;
		"*error*" ++ _ -> {error, make};
		Other ->{error,{make,Other}}
	    end;
	Error -> Error
    end.

get_port_data(Port, Last0, Complete0) ->
    receive
	{Port,{data,Bytes}} ->
	    {Last, Complete} = update_last(Bytes, Last0, Complete0),
	    get_port_data(Port, Last, Complete);
	{Port, eof} ->
	    Result = update_last(eof, Last0, Complete0),
	    unlink(Port),
	    exit(Port, die),
	    Result
    end.

update_last([C|Rest], Line, true) ->
    try
	%% Utf-8 list to utf-8 binary
	%% (e.g. we assume utf-8 bytes from port)
	io:put_chars(list_to_binary(Line))
    catch
	error:badarg ->
	    %% io:put_chars/1 badarged
	    %% this likely means we had unicode code points
	    %% in our bytes buffer (e.g warning from gcc with åäö)
	    io:put_chars(unicode:characters_to_binary(Line))
    end,
    io:nl(),
    update_last([C|Rest], [], false);
update_last([$\r|Rest], Result, Complete) ->
    update_last(Rest, Result, Complete);
update_last([$\n|Rest], Result, _Complete) ->
    update_last(Rest, lists:reverse(Result), true);
update_last([C|Rest], Result, Complete) ->
    update_last(Rest, [C|Result], Complete);
update_last([], Result, Complete) ->
    {Result, Complete};
update_last(eof, Result, _) ->
    unicode:characters_to_list(list_to_binary(Result)).

run_make_script({win32, _}, Make, Dir, Makefile) ->
    {"run_make.bat",
     ".\\run_make",
     ["@echo off\r\n",
      "cd \"", filename:nativename(Dir), "\"\r\n",
      Make, " -f ", Makefile, " \r\n",
      "if errorlevel 1 echo *error*\r\n",
      "if not errorlevel 1 echo *ok*\r\n"]};
run_make_script({unix, _}, Make, Dir, Makefile) ->
    {"run_make", 
     "/bin/sh ./run_make",
     ["#!/bin/sh\n",
      "cd \"", Dir, "\"\n",
      Make, " -f ", Makefile, " 2>&1\n",
      "case $? in\n",
      "  0) echo '*ok*';;\n",
      "  *) echo '*error*';;\n",
      "esac\n"]};
run_make_script(_Other, _Make, _Dir, _Makefile) ->
    exit(dont_know_how_to_make_script_on_this_platform).
