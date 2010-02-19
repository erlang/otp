%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(all).

%% User interface
-export([run/0]).

%% Interna constants
-define(NORMAL, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Interface 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run() -> _
%%	
%% Runs all benchmark modules in the current directory on all erlang
%% installations specified by releases/0
run() ->
    %% Delete previous intermediate test result files.
    lists:foreach(fun(F) -> file:delete(F) end, filelib:wildcard("*.bmres")),
    lists:foreach(fun run/1, releases()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% run(Release) -> _
%%	Release = string() - Erlang release 	
%% Help functions to run/0
run({Release,Comment}) -> 
    command(Release ++ " -noshell -compile bench -s erlang halt"),
    command(Release ++ " -noshell -s bench run " ++ Comment ++" -s erlang halt").

%% command(Command) -> _
%%	Command = string() - is the name and arguments of the external 
%%                           program which will be run
command(Command) ->
    io:format("~s\n", [Command]),  % Progress info to user
    Port = open_port({spawn,Command}, [exit_status, in]), 
    print_output(Port).

%% print_output(Port) -> _
%%	Port = port()	
%% Print data from the port i.e. output from external program,
%% on standard out.
print_output(Port) ->
    receive
	{Port, {data,Bytes}} ->
	    io:put_chars(Bytes),
	    print_output(Port);
	{Port, {exit_status, ?NORMAL}} ->
	    ok
    end.

%% run() -> Releases
%%	Releases = [Release |_]
%%      Release = string() - Erlang release  
%% Defines which erlang releases to run on
%%  --- Change this function to reflect your own erlang installations ---
releases() ->
    [
     {"/usr/local/otp/releases/otp_beam_sunos5_r8b_patched/bin/erl","standardr8"},
     {"/usr/local/otp/releases/otp_beam_sunos5_r8b_patched/bin/erl -pa /clearcase/otp/erts/lib/asn1/ebin", "asn1r9"}
].

















