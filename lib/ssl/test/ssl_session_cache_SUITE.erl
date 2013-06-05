%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.2
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssl_session_cache_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DELAY, 500).
-define(SLEEP, 500).
-define(TIMEOUT, 60000).
-define(LONG_TIMEOUT, 600000).

-behaviour(ssl_session_cache_api).

%% For the session cache tests
-export([init/1, terminate/1, lookup/2, update/3,
	 delete/2, foldl/3, select_session/2]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [session_cleanup,
     session_cache_process_list,
     session_cache_process_mnesia].

groups() ->
    [].

init_per_suite(Config0) ->
    Dog = ct:timetrap(?LONG_TIMEOUT *2),
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl:start(),
	    %% make rsa certs using oppenssl
	    Result =
		(catch make_certs:all(?config(data_dir, Config0),
				      ?config(priv_dir, Config0))),
	    ct:log("Make certs  ~p~n", [Result]),

	    Config1 = ssl_test_lib:make_dsa_cert(Config0),
	    Config = ssl_test_lib:cert_options(Config1),
	    [{watchdog, Dog} | Config]
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(session_cache_process_list, Config) ->
    init_customized_session_cache(list, Config);

init_per_testcase(session_cache_process_mnesia, Config) ->
    mnesia:start(),
    init_customized_session_cache(mnesia, Config);

init_per_testcase(session_cleanup, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, 5),
    application:set_env(ssl, session_delay_cleanup_time, ?DELAY),
    ssl:start(),
    [{watchdog, Dog} | Config];

init_per_testcase(_TestCase, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
   [{watchdog, Dog} | Config].

init_customized_session_cache(Type, Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_cb, ?MODULE),
    application:set_env(ssl, session_cb_init_args, [Type]),
    ssl:start(),
    [{watchdog, Dog} | Config].

end_per_testcase(session_cache_process_list, Config) ->
    application:unset_env(ssl, session_cb),
    end_per_testcase(default_action, Config);
end_per_testcase(session_cache_process_mnesia, Config) ->
    application:unset_env(ssl, session_cb),
    application:unset_env(ssl, session_cb_init_args),
    mnesia:kill(),
    ssl:stop(),
    ssl:start(),
    end_per_testcase(default_action, Config);
end_per_testcase(session_cleanup, Config) ->
    application:unset_env(ssl, session_delay_cleanup_time),
    application:unset_env(ssl, session_lifetime),
    end_per_testcase(default_action, Config);
end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
session_cleanup() ->
    [{doc, "Test that sessions are cleand up eventually, so that the session table "
     "does not grow and grow ..."}].
session_cleanup(Config)when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ?config(client_opts, Config),
    ServerOpts = ?config(server_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, session_info_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
		      {from, self()},  {options, ClientOpts}]),
    SessionInfo =
	receive
	    {Server, Info} ->
		Info
	end,

    %% Make sure session is registered
    ct:sleep(?SLEEP),

    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    Cache = element(2, State),
    SessionTimer = element(6, State),

    Id = proplists:get_value(session_id, SessionInfo),
    CSession = ssl_session_cache:lookup(Cache, {{Hostname, Port}, Id}),
    SSession = ssl_session_cache:lookup(Cache, {Port, Id}),

    true = CSession =/= undefined,
    true = SSession =/= undefined,

    %% Make sure session has expired and been cleaned up
    check_timer(SessionTimer),
    ct:sleep(?DELAY *2),  %% Delay time + some extra time

    {ServerDelayTimer, ClientDelayTimer} = get_delay_timers(),

    check_timer(ServerDelayTimer),
    check_timer(ClientDelayTimer),

    ct:sleep(?SLEEP),  %% Make sure clean has had time to run

    undefined = ssl_session_cache:lookup(Cache, {{Hostname, Port}, Id}),
    undefined = ssl_session_cache:lookup(Cache, {Port, Id}),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

check_timer(Timer) ->
    case erlang:read_timer(Timer) of
	false ->
	    {status, _, _, _} = sys:get_status(whereis(ssl_manager)),
	    timer:sleep(?SLEEP),
	    {status, _, _, _} = sys:get_status(whereis(ssl_manager)),
	    ok;
	Int ->
	    ct:sleep(Int),
	    check_timer(Timer)
    end.

get_delay_timers() ->
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    case element(7, State) of
	{undefined, undefined} ->
	    ct:sleep(?SLEEP),
	    get_delay_timers();
	{undefined, _} ->
	    ct:sleep(?SLEEP),
	    get_delay_timers();
	{_, undefined} ->
	    ct:sleep(?SLEEP),
	    get_delay_timers();
	DelayTimers ->
	    DelayTimers
    end.
%%--------------------------------------------------------------------
session_cache_process_list() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
session_cache_process_list(Config) when is_list(Config) ->
    session_cache_process(list,Config).
%%--------------------------------------------------------------------
session_cache_process_mnesia() ->
    [{doc,"Test reuse of sessions (short handshake)"}].
session_cache_process_mnesia(Config) when is_list(Config) ->
    session_cache_process(mnesia,Config).

%%--------------------------------------------------------------------
%%% Session cache API callbacks
%%--------------------------------------------------------------------

init([Type]) ->
    ets:new(ssl_test, [named_table, public, set]),
    ets:insert(ssl_test, {type, Type}),
    case Type of
	list ->
	    spawn(fun() -> session_loop([]) end);
	mnesia ->
	    mnesia:start(),
	    {atomic,ok} = mnesia:create_table(sess_cache, []),
	    sess_cache
    end.

session_cb() ->
    [{type, Type}] = ets:lookup(ssl_test, type),
    Type.

terminate(Cache) ->
    case session_cb() of
	list ->
	    Cache ! terminate;
	mnesia ->
	    catch {atomic,ok} =
		mnesia:delete_table(sess_cache)
    end.

lookup(Cache, Key) ->
    case session_cb() of
	list ->
	    Cache ! {self(), lookup, Key},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    case mnesia:transaction(fun() ->
					    mnesia:read(sess_cache,
							Key, read)
				    end) of
		{atomic, [{sess_cache, Key, Value}]} ->
		    Value;
		_ ->
		    undefined
	    end
	end.

update(Cache, Key, Value) ->
    case session_cb() of
	list ->
	    Cache ! {update, Key, Value};
	mnesia ->
	    {atomic, ok} =
		mnesia:transaction(fun() ->
					   mnesia:write(sess_cache,
							{sess_cache, Key, Value}, write)
				   end)
    end.

delete(Cache, Key) ->
    case session_cb() of
	list ->
	    Cache ! {delete, Key};
	mnesia ->
	    {atomic, ok} =
		mnesia:transaction(fun() ->
					   mnesia:delete(sess_cache, Key)
				   end)
    end.

foldl(Fun, Acc, Cache) ->
    case session_cb() of
	list ->
	    Cache ! {self(),foldl,Fun,Acc},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    Foldl = fun() ->
			    mnesia:foldl(Fun, Acc, sess_cache)
		    end,
	    {atomic, Res} = mnesia:transaction(Foldl),
	    Res
    end.

select_session(Cache, PartialKey) ->
    case session_cb() of
	list ->
	    Cache ! {self(),select_session, PartialKey},
	    receive
		{Cache, Res} ->
		    Res
	    end;
	mnesia ->
	    Sel = fun() ->
			  mnesia:select(Cache,
					[{{sess_cache,{PartialKey,'$1'}, '$2'},
					  [],['$$']}])
		  end,
	    {atomic, Res} = mnesia:transaction(Sel),
	    Res
    end.

session_loop(Sess) ->
    receive
	terminate ->
	    ok;
	{Pid, lookup, Key} ->
	    case lists:keysearch(Key,1,Sess) of
		{value, {Key,Value}} ->
		    Pid ! {self(), Value};
		_ ->
		    Pid ! {self(), undefined}
	    end,
	    session_loop(Sess);
	{update, Key, Value} ->
	    NewSess = [{Key,Value}| lists:keydelete(Key,1,Sess)],
	    session_loop(NewSess);
	{delete, Key} ->
	    session_loop(lists:keydelete(Key,1,Sess));
	{Pid,foldl,Fun,Acc} ->
	    Res = lists:foldl(Fun, Acc,Sess),
	    Pid ! {self(), Res},
	    session_loop(Sess);
	{Pid,select_session,PKey} ->
	    Sel = fun({{PKey0, Id},Session}, Acc) when PKey == PKey0 ->
			  [[Id, Session]|Acc];
		     (_,Acc) ->
			  Acc
		  end,
	    Sessions = lists:foldl(Sel, [], Sess),
	    Pid ! {self(), Sessions},
	    session_loop(Sess)
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

session_cache_process(_Type,Config) when is_list(Config) ->
    ssl_basic_SUITE:reuse_session(Config).
