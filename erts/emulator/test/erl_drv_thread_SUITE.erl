%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

-module(erl_drv_thread_SUITE).
-author('rickard.s.green@ericsson.com').
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([basic/1, rwlock/1, tsd/1]).

-include_lib("test_server/include/test_server.hrl").

-define(DEFAULT_TIMETRAP_SECS, 240).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, rwlock, tsd].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Testcases                                                              %%
%%                                                                        %%

basic(suite) -> [];
basic(doc) ->   [];
basic(Cfg) -> ?line drv_case(Cfg, basic).

rwlock(suite) -> [];
rwlock(doc) ->   [];
rwlock(Cfg) -> ?line drv_case(Cfg, rwlock).

tsd(suite) -> [];
tsd(doc) ->   [];
tsd(Cfg) -> ?line drv_case(Cfg, tsd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Internal functions                                                     %%
%%                                                                        %%

drv_case(Config, CaseName) ->
    drv_case(Config, CaseName, "").

drv_case(Config, CaseName, TimeTrap) when is_integer(TimeTrap) ->
    drv_case(Config, CaseName, "", TimeTrap);
drv_case(Config, CaseName, Command) when is_list(Command) ->
    drv_case(Config, CaseName, Command, ?DEFAULT_TIMETRAP_SECS).

drv_case(Config, CaseName, TimeTrap, Command) when is_list(Command),
						   is_integer(TimeTrap) ->
    drv_case(Config, CaseName, Command, TimeTrap);
drv_case(Config, CaseName, Command, TimeTrap) when is_list(Config),
						   is_atom(CaseName),
						   is_list(Command),
						   is_integer(TimeTrap) ->
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
