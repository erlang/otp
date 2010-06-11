%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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
-module(old_ssl_protocol_SUITE).

-export([all/1, init_per_testcase/2, fin_per_testcase/2, config/1,
	 finish/1, sslv2/1, sslv3/1, tlsv1/1, sslv2_sslv3/1,
	 sslv2_tlsv1/1, sslv3_tlsv1/1, sslv2_sslv3_tlsv1/1]).

-import(ssl_test_MACHINE, [mk_ssl_cert_opts/1, test_one_listener/7,
			   test_server_only/6]).
-include("test_server.hrl").
-include("ssl_test_MACHINE.hrl").


init_per_testcase(_Case, Config) ->
    WatchDog = test_server:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, WatchDog}| Config].

fin_per_testcase(_Case, Config) ->
    WatchDog = ?config(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).

all(doc) ->
    "Test of configuration protocol_version.";
all(suite) ->
    {conf,
     config,
     [sslv2, sslv3, tlsv1, sslv2_sslv3, sslv2_tlsv1, sslv3_tlsv1, 
      sslv2_sslv3_tlsv1],
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
    "This test case has no other purpose than closing the conf case.";
finish(suite) ->
    [];
finish(Config) ->
    Config.

%%%%%

sslv2(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose SSLv2."; 
sslv2(suite) ->
    [];
sslv2(Config) when list(Config) ->
    do_run_test(Config, [sslv2]).

sslv3(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose SSLv3."; 
sslv3(suite) ->
    [];
sslv3(Config) when list(Config) ->
    do_run_test(Config, [sslv3]).

tlsv1(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose TLSv1."; 
tlsv1(suite) ->
    [];
tlsv1(Config) when list(Config) ->
    do_run_test(Config, [tlsv1]).

sslv2_sslv3(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose between SSLv2 and SSLv3."; 
sslv2_sslv3(suite) ->
    [];
sslv2_sslv3(Config) when list(Config) ->
    do_run_test(Config, [sslv2, sslv3]).

sslv2_tlsv1(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose between SSLv2 and TLSv1."; 
sslv2_tlsv1(suite) ->
    [];
sslv2_tlsv1(Config) when list(Config) ->
    do_run_test(Config, [sslv2, tlsv1]).

sslv3_tlsv1(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose between SSLv3 and TLSv1."; 
sslv3_tlsv1(suite) ->
    [];
sslv3_tlsv1(Config) when list(Config) ->
    do_run_test(Config, [sslv3, tlsv1]).

sslv2_sslv3_tlsv1(doc) ->
    "Client has no cert. Nor the client, nor the server is verifying its "
	"peer. Server closes, client waits for close. "
	"Client and server choose between SSLv2, SSLv3, and TLSv1."; 
sslv2_sslv3_tlsv1(suite) ->
    [];
sslv2_sslv3_tlsv1(Config) when list(Config) ->
    do_run_test(Config, [sslv2, sslv3, tlsv1]).

%%%%

do_run_test(Config0, Protocols) ->
    process_flag(trap_exit, true),
    LPort = 3456,
    Timeout = 40000, NConns = 1,
    DataSize = 10,

    ?line {ok, {_, SsslOpts0}} = mk_ssl_cert_opts(Config0),
    ?line SsslOpts = [{verify, 0}, {depth, 2} | SsslOpts0],

    ?line {ok, Host} = inet:gethostname(),

    LCmds = [{sockopts, [{backlog, NConns}]},
	     {sslopts, SsslOpts},
	     {listen, LPort}, 
	     wait_sync,
	     lclose],
    ACmds = [{timeout, Timeout}, 
	     accept,
	     connection_info,
	     {recv, DataSize},
	     close],
    CCmds = [{timeout, Timeout}, 
	     {connect, {Host, LPort}},
	     connection_info,
	     {send, DataSize},
	     await_close],
    Config1 = [{env, [{protocol_version, Protocols}]} | Config0],
    ?line test_one_listener(NConns, LCmds, ACmds, CCmds, Timeout,
			    ?MODULE, Config1).


