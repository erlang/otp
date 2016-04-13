%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
-export([all/0, suite/0]).

-export([basic/1, rwlock/1, tsd/1]).

-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_TIMETRAP_SECS, 240).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, rwlock, tsd].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                        %%
%% Testcases                                                              %%
%%                                                                        %%

basic(Cfg) -> drv_case(Cfg, basic).

rwlock(Cfg) -> drv_case(Cfg, rwlock).

tsd(Cfg) -> drv_case(Cfg, tsd).

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
    case os:type() of
        {Family, _} when Family == unix; Family == win32 ->
            run_drv_case(Config, CaseName, Command, TimeTrap);
        SkipOs ->
            {skipped, lists:flatten(["Not run on " | io_lib:format("~p",[SkipOs])])}
    end.

run_drv_case(Config, CaseName, Command, TimeTrap) ->
    ct:timetrap({seconds, TimeTrap}),
    DataDir = proplists:get_value(data_dir,Config),
    case erl_ddll:load_driver(DataDir, CaseName) of
        ok -> ok;
        {error, Error} ->
            ct:fail(erl_ddll:format_error(Error))
    end,
    Port = open_port({spawn, atom_to_list(CaseName)}, []),
    true = is_port(Port),
    Port ! {self(), {command, Command}},
    Result = receive_drv_result(Port, CaseName),
    Port ! {self(), close},
    receive 
        {Port, closed} ->
            ok
    end,
    ok = erl_ddll:unload_driver(CaseName),
    Result.

receive_drv_result(Port, CaseName) ->
    receive
        {print, Port, CaseName, Str} ->
            io:format("~s", [Str]),
            receive_drv_result(Port, CaseName);
        {'EXIT', Port, Error} ->
            ct:fail(Error);
        {'EXIT', error, Error} ->
            ct:fail(Error);
        {failed, Port, CaseName, Comment} ->
            ct:fail(Comment);
        {skipped, Port, CaseName, Comment} ->
            {skipped, Comment};
        {succeeded, Port, CaseName, ""} ->
            succeeded;
        {succeeded, Port, CaseName, Comment} ->
            {comment, Comment}
    end.
