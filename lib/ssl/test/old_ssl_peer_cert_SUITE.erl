%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
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
-module(old_ssl_peer_cert_SUITE).

-export([all/1,
	 init_per_testcase/2,
	 fin_per_testcase/2,
	 config/1,
	 finish/1,
	 cinit_plain/1,
	 cinit_both_verify/1,
	 cinit_cnocert/1
	 ]).

-import(ssl_test_MACHINE, [mk_ssl_cert_opts/1, test_one_listener/7,
			   test_server_only/6]).
-include("test_server.hrl").
-include("ssl_test_MACHINE.hrl").


init_per_testcase(_Case, Config) ->
    WatchDog = ssl_test_lib:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, WatchDog}| Config].

fin_per_testcase(_Case, Config) ->
    WatchDog = ?config(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).

all(doc) ->
    "Test of ssl verification and peer certificate retrieval.";
all(suite) ->
    {conf,
     config,
     [cinit_plain,
      cinit_both_verify,
      cinit_cnocert],
     finish}.

config(doc) ->
    "Want to se what Config contains.";
config(suite) ->
    [];
config(Config) ->
    io:format("Config: ~p~n", [Config]),

    %% Check if SSL exists. If this case fails, all other cases are skipped
    crypto:start(),
    application:start(public_key),
    case ssl:start() of
	ok -> ssl:stop();
	{error, {already_started, _}} -> ssl:stop();
	Error -> ?t:fail({failed_starting_ssl,Error})
    end,
    Config.

finish(doc) ->
    "This test case has no mission other than closing the conf case";
finish(suite) ->
    [];
finish(Config) ->
    Config.

cinit_plain(doc) ->
    "Server closes after accept, Client waits for close. Both have certs "
	"but both use the defaults for verify and depth, but still tries "
	"to retreive each others certificates.";
cinit_plain(suite) ->
    [];
cinit_plain(Config) when list(Config) ->
    process_flag(trap_exit, true),
    DataSize = 1000, LPort = 3456,
    Timeout = 40000, NConns = 1,

    ?line {ok, {CsslOpts, SsslOpts}} = mk_ssl_cert_opts(Config),

    ?line {ok, Host} = inet:gethostname(),

    LCmds = [{sockopts, [{backlog, NConns}]},
	     {sslopts, SsslOpts},
	     {listen, LPort}, 
	     wait_sync,
	     lclose],
    ACmds = [{timeout, Timeout}, 
	     accept,
	     nopeercert,
	     {recv, DataSize},
	     close],
    CCmds = [{timeout, Timeout}, 
	     {sslopts, CsslOpts},
	     {connect, {Host, LPort}},
	     peercert,
	     {send, DataSize},
	     await_close],
    ?line test_one_listener(NConns, LCmds, ACmds, CCmds, Timeout, 
			    ?MODULE, Config).

cinit_both_verify(doc) ->
    "Server closes after accept, Client waits for close. Both have certs "
	"and both verify each other.";
cinit_both_verify(suite) ->
    [];
cinit_both_verify(Config) when list(Config) ->
    process_flag(trap_exit, true),
    DataSize = 1000, LPort = 3456,
    Timeout = 40000, NConns = 1,

    ?line {ok, {CsslOpts0, SsslOpts0}} = mk_ssl_cert_opts(Config),
    ?line CsslOpts = [{verify, 2}, {depth, 2} | CsslOpts0],
    ?line SsslOpts = [{verify, 2}, {depth, 3} | SsslOpts0],

    ?line {ok, Host} = inet:gethostname(),

    LCmds = [{sockopts, [{backlog, NConns}]},
	     {sslopts, SsslOpts},
	     {listen, LPort}, 
	     wait_sync,
	     lclose],
    ACmds = [{timeout, Timeout}, 
	     accept,
	     peercert,
	     {recv, DataSize},
	     close],
    CCmds = [{timeout, Timeout}, 
	     {sslopts, CsslOpts},
	     {connect, {Host, LPort}},
	     peercert,
	     {send, DataSize},
	     await_close],
    ?line test_one_listener(NConns, LCmds, ACmds, CCmds, Timeout, 
			    ?MODULE, Config).

cinit_cnocert(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close.";
cinit_cnocert(suite) ->
    [];
cinit_cnocert(Config) when list(Config) ->
    process_flag(trap_exit, true),
    DataSize = 1000, LPort = 3457,
    Timeout = 40000, NConns = 1,

    ?line {ok, {_, SsslOpts0}} = mk_ssl_cert_opts(Config),
    ?line SsslOpts = [{verify, 0}, {depth, 2} | SsslOpts0],

    ?line {ok, Host} = inet:gethostname(),

    LCmds = [{sockopts, [{backlog, NConns}]},
	     {sslopts, SsslOpts},
	     {listen, LPort}, 
	     wait_sync,
	     lclose],
    ACmds = [{timeout, Timeout}, 
	     accept,
	     {recv, DataSize},
	     close],
    CCmds = [{timeout, Timeout}, 
	     {connect, {Host, LPort}},
	     peercert,
	     {send, DataSize},
	     await_close],
    ?line test_one_listener(NConns, LCmds, ACmds, CCmds, Timeout,
			    ?MODULE, Config).


