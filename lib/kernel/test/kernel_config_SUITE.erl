%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(kernel_config_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, sync/1]).

-export([init_per_suite/1, end_per_suite/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [sync].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(doc) -> [];
init_per_suite(suite) -> [];
init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(doc) -> [];
end_per_suite(suite) -> [];
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

%%-----------------------------------------------------------------
%% Test suite for sync_nodes.   This is quite tricky.
%%
%% Should be started in a CC view with:
%% erl -sname XXX where XX not in [cp1, cp2]
%%-----------------------------------------------------------------
sync(doc) -> [];
sync(suite) -> [];
sync(Conf) when is_list(Conf) ->
    ?line Dog = ?t:timetrap(?t:seconds(120)),
    % Write a config file
    Dir = ?config(priv_dir,Conf),
    {ok, Fd} = file:open(Dir ++ "sys.config", [write]),
    config(Fd),
    file:close(Fd),
    Config = Dir ++ "sys",

    %% Reset wall_clock
    {T1,_} = erlang:statistics(wall_clock),
    io:format("~p~n", [{t1, T1}]),
    ?line Command = lists:concat([lib:progname(),
				  " -detached -sname cp1 ",
				  "-config ", Config,
				  " -env ERL_CRASH_DUMP erl_crash_dump.cp1"]),
    io:format("Command: ~s", [Command]),
    ?line open_port({spawn, Command}, [stream]),
    io:format("started~n"),
    ?line ?t:sleep(12000),
    io:format("waited12~n"),
    ?line Host = from($@, atom_to_list(node())),
    ?line Cp1 = list_to_atom("cp1@"++Host),
    ?line wait_for_node(Cp1),
    io:format("waitednode~n"),
    %% Check time since last call
    ?line {TT, T} = erlang:statistics(wall_clock),
    io:format("~p~n", [{t2, {TT, T}}]),
    ?line stop_node(cp1),
    if
	TT-T1 < 15000 -> ?line ?t:fail({too_short_time, TT-T1});
	true -> ok
    end,
    ?line ?t:timetrap_cancel(Dog),
    ok.

wait_for_node(Node) ->
    case rpc:call(Node, init, get_status, []) of
	{started,_} -> ok;
	{badrpc, R} -> ?line ?t:fail({rpc_failed, R});
	_Other -> wait_for_node(Node)
    end.


stop_node(Node) ->
    M = list_to_atom(lists:concat([Node,
				   [$@],
				   from($@,atom_to_list(node()))])),
    rpc:cast(M, erlang, halt, []).
