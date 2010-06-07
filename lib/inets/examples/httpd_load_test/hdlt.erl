%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Main API module for the httpd load test utility
%%----------------------------------------------------------------------

-module(hdlt).


%%-----------------------------------------------------------------
%% Public interface
%%-----------------------------------------------------------------

-export([start/0, start/1, stop/0, help/0]).


%%-----------------------------------------------------------------
%% Start the HDLT utility
%%-----------------------------------------------------------------

start() ->
    ConfigFile = "hdlt.config",
    case file:consult(ConfigFile) of
	{ok, Config} when is_list(Config) ->
	    start(Config);
	Error ->
	    Error
    end.

start(Config) ->
    Flag = process_flag(trap_exit, true),
    Result = 
	case hdlt_ctrl:start(Config) of
	    {ok, Pid} ->
		receive
		    {'EXIT', Pid, normal} ->
			ok;
		    {'EXIT', Pid, Reason} ->
			io:format("HDLT failed: "
				  "~n   ~p"
				  "~n", [Reason]),
			{error, Reason}
		end;
	    Error ->
		Error
	end,
    process_flag(trap_exit, Flag),
    Result.



stop() ->
    hdlt_ctrl:stop().

    
help() ->
    hdlt_ctrl:help().
