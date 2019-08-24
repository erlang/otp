%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(kernel_config_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 start_distribution_false/1, sync/1]).

-export([init_per_suite/1, end_per_suite/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [sync, start_distribution_false].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(Config) when is_list(Config) ->
    stop_node(init_test),
    Config.

config(Fd) ->
    M = from($@, atom_to_list(node())),
    io:format(Fd, "[{kernel, [{sync_nodes_optional, ['cp1@~s','cp2@~s']},"
	      "{sync_nodes_timeout, 15000}]}].~n",
	      [M, M]).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].

%% Test sync_nodes. This is quite tricky.
%% Should be started in a CC view with:
%% erl -sname XXX where XX not in [cp1, cp2]
sync(Conf) when is_list(Conf) ->
    %% Write a config file
    Dir = proplists:get_value(priv_dir,Conf),
    {ok, Fd} = file:open(Dir ++ "sys.config", [write]),
    config(Fd),
    file:close(Fd),
    Config = Dir ++ "sys",

    %% Reset wall_clock
    {T1,_} = erlang:statistics(wall_clock),
    io:format("~p~n", [{t1, T1}]),
    Command = lists:append([ct:get_progname(),
			    " -detached -sname cp1 ",
			    "-config ", Config,
			    " -env ERL_CRASH_DUMP erl_crash_dump.cp1"]),
    io:format("Command: ~s", [Command]),
    open_port({spawn, Command}, [stream]),
    io:format("started~n"),
    ct:sleep(12000),
    io:format("waited12~n"),
    Host = from($@, atom_to_list(node())),
    Cp1 = list_to_atom("cp1@"++Host),
    wait_for_node(Cp1),
    io:format("waitednode~n"),
    %% Check time since last call
    {TT, T} = erlang:statistics(wall_clock),
    io:format("~p~n", [{t2, {TT, T}}]),
    stop_node(cp1),
    if
	TT-T1 < 15000 -> ct:fail({too_short_time, TT-T1});
	true -> ok
    end,
    ok.

wait_for_node(Node) ->
    case rpc:call(Node, init, get_status, []) of
	{started,_} -> ok;
	{badrpc, R} -> ct:fail({rpc_failed, R});
	_Other -> wait_for_node(Node)
    end.

stop_node(Node) ->
    M = list_to_atom(lists:concat([Node,
				   [$@],
				   from($@,atom_to_list(node()))])),
    rpc:cast(M, erlang, halt, []).

start_distribution_false(Config) when is_list(Config) ->
    %% When distribution is disabled, -sname/-name has no effect
    Str = os:cmd(ct:get_progname()
		 ++ " -kernel start_distribution false"
		 ++ " -sname no_distribution"
		 ++ " -eval \"erlang:display(node())\""
		 ++ " -noshell -s erlang halt"),
    "'nonode@nohost'" ++ _ = Str,
    ok.
