%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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


-module(obsolete_SUITE).
-author('rickard.s.green@ericsson.com').
-compile(nowarn_obsolete_guard).

-export([all/1]).

-export([erl_threads/1]).

-include("test_server.hrl").

-define(DEFAULT_TIMETRAP_SECS, 240).

all(doc) -> [];
all(suite) ->
    case catch erlang:system_info(wordsize) of
	4 -> [erl_threads];
	_ -> {skip, "Only expected to work on 32-bit architectures"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Testcases                                                              %%
%%                                                                        %%

erl_threads(suite) -> [];
erl_threads(doc) ->   [];
erl_threads(Cfg) ->
    ?line case erlang:system_info(threads) of
	      true ->
		  ?line drv_case(Cfg, erl_threads);
	      false ->
		  ?line {skip, "Emulator not compiled with threads support"}
	  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Internal functions                                                     %%
%%                                                                        %%

drv_case(Config, CaseName) ->
    drv_case(Config, CaseName, "").

drv_case(Config, CaseName, TimeTrap) when integer(TimeTrap) ->
    drv_case(Config, CaseName, "", TimeTrap);
drv_case(Config, CaseName, Command) when list(Command) ->
    drv_case(Config, CaseName, Command, ?DEFAULT_TIMETRAP_SECS).

drv_case(Config, CaseName, TimeTrap, Command) when list(Command),
						   integer(TimeTrap) ->
    drv_case(Config, CaseName, Command, TimeTrap);
drv_case(Config, CaseName, Command, TimeTrap) when list(Config),
						   atom(CaseName),
						   list(Command),
						   integer(TimeTrap) ->
    case ?t:os_type() of
	{Family, _} when Family == unix; Family == win32 ->
	    ?line run_drv_case(Config, CaseName, Command, TimeTrap);
	SkipOs ->
	    ?line {skipped,
		   lists:flatten(["Not run on "
				  | io_lib:format("~p",[SkipOs])])}
    end.

run_drv_case(Config, CaseName, Command, TimeTrap) ->
    ?line Dog = test_server:timetrap(test_server:seconds(TimeTrap)),
    ?line DataDir = ?config(data_dir,Config),
    case erl_ddll:load_driver(DataDir, CaseName) of
	ok -> ok;
	{error, Error} ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    ?line ?t:fail()
    end,
    ?line Port = open_port({spawn, atom_to_list(CaseName)}, []),
    ?line true = is_port(Port),
    ?line Port ! {self(), {command, Command}},
    ?line Result = receive_drv_result(Port, CaseName),
    ?line Port ! {self(), close},
    ?line receive 
	      {Port, closed} ->
		  ok
	  end,
    ?line ok = erl_ddll:unload_driver(CaseName),
    ?line test_server:timetrap_cancel(Dog),
    ?line Result.

receive_drv_result(Port, CaseName) ->
    ?line receive
	      {print, Port, CaseName, Str} ->
		  ?line ?t:format("~s", [Str]),
		  ?line receive_drv_result(Port, CaseName);
	      {'EXIT', Port, Error} ->
		  ?line ?t:fail(Error);
	      {'EXIT', error, Error} ->
		  ?line ?t:fail(Error);
	      {failed, Port, CaseName, Comment} ->
		  ?line ?t:fail(Comment);
	      {skipped, Port, CaseName, Comment} ->
		  ?line {skipped, Comment};
	      {succeeded, Port, CaseName, ""} ->
		  ?line succeeded;
	      {succeeded, Port, CaseName, Comment} ->
		  ?line {comment, Comment}
	  end.
