%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(all).

%% User interface
-export([run/0]).

%% Interna constants
-define(NORMAL, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Interface 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------------
%% run() -> _
%%	
%% Runs all benchmark modules in the current directory on all erlang
%% installations specified by releases/0
%%---------------------------------------------------------------------------
run() ->
    %% Delete previous intermediate test result files.
    lists:foreach(fun(F) -> file:delete(F) end, filelib:wildcard("*.bmres")),
    lists:foreach(fun run/1, releases()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------------
%% run(Release) -> _
%%	Release = string() - Erlang release 	
%% Help functions to run/0
%%---------------------------------------------------------------------------
run(Release) -> 
    command(Release ++ " -noshell -compile bench -s erlang halt"),
    command(Release ++ " -noshell -s bench run -s erlang halt").
%%---------------------------------------------------------------------------
%% command(Command) -> _
%%	Command = string() - is the name and arguments of the external 
%%                           program which will be run
%%---------------------------------------------------------------------------
command(Command) ->
    io:format("~s\n", [Command]),  % Progress info to user
    Port = open_port({spawn,Command}, [exit_status, in]), 
    print_output(Port).
%%---------------------------------------------------------------------------
%% print_output(Port) -> _
%%	Port = port()	
%% Print data from the port i.e. output from external program,
%% on standard out.
%%---------------------------------------------------------------------------
print_output(Port) ->
    receive
	{Port, {data,Bytes}} ->
	    io:put_chars(Bytes),
	    print_output(Port);
	{Port, {exit_status, ?NORMAL}} ->
	    ok
    end.
%%---------------------------------------------------------------------------
%% run() -> Releases
%%	Releases = [Release |_]
%%      Release = string() - Erlang release  
%% Defines which erlang releases to run on
%%  --- Change this function to reflect your own erlang installations ---
%%---------------------------------------------------------------------------
releases() ->
    ["/usr/local/otp/releases/otp_beam_sunos5_r7b01_patched/bin/erl",
     "/usr/local/otp/releases/otp_beam_sunos5_r8b_patched/bin/erl"].





















