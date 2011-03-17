%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2011. All Rights Reserved.
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
-module(old_ssl_misc_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,
	 end_per_testcase/2,
	 seed/1,
	 app/1
	]).

-import(ssl_test_MACHINE, [mk_ssl_cert_opts/1, test_one_listener/7,
			   test_server_only/6]).
-include_lib("test_server/include/test_server.hrl").
-include("ssl_test_MACHINE.hrl").

-define(MANYCONNS, 5).

init_per_testcase(_Case, Config) ->
    WatchDog = ssl_test_lib:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, WatchDog}| Config].

end_per_testcase(_Case, Config) ->
    WatchDog = ?config(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [seed, app].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(doc) ->
    "Want to se what Config contains.";
init_per_suite(suite) ->
    [];
init_per_suite(Config) ->
    io:format("Config: ~p~n", [Config]),

    %% Check if SSL exists. If this case fails, all other cases are skipped
    case catch crypto:start() of
	ok ->
	    application:start(public_key),
	    case ssl:start() of
		ok -> ssl:stop();
		{error, {already_started, _}} -> ssl:stop();
		Error -> ?t:fail({failed_starting_ssl,Error})
	    end,
	    Config;
	_Else ->
	    {skip,"Could not start crypto!"}
    end.

end_per_suite(doc) ->
    "This test case has no mission other than closing the conf case";
end_per_suite(suite) ->
    [];
end_per_suite(Config) ->
    crypto:stop(),
    Config.

seed(doc) ->
    "Test that ssl:seed/1 works.";
seed(suite) ->
    [];
seed(Config) when list(Config) ->
    process_flag(trap_exit, true),
    LPort = 3456,
    Timeout = 40000, NConns = 1,

    ?line {ok, {_, SsslOpts}} = mk_ssl_cert_opts(Config),

    LCmds = [{seed, "tjosan"},
	     {sockopts, [{backlog, NConns}, {active, once}]},
	     {sslopts, SsslOpts},
	     {listen, LPort}, 
	     wait_sync,
	     lclose],
    ?line test_server_only(NConns, LCmds, [], Timeout, ?MODULE,
			   Config).

app(doc) ->
    "Test that the ssl app file is ok";
app(suite) ->
    [];
app(Config) when list(Config) ->
    ?line ok = test_server:app_test(ssl).


