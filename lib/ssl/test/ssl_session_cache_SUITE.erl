%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

%%

-module(ssl_session_cache_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DELAY, 500).
-define(SLEEP, 1000).
-define(TIMEOUT, 60000).
-define(LONG_TIMEOUT, 600000).
-define(MAX_TABLE_SIZE, 5).

-behaviour(ssl_session_cache_api).

%% For the session cache tests
-export([init/1, terminate/1, lookup/2, update/3,
	 delete/2, foldl/3, select_session/2]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() ->
    [session_cleanup,
     session_cache_process_list,
     session_cache_process_mnesia,
     client_unique_session,
     max_table_size,
     save_specific_session
     ].

groups() ->
    [].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    %% make rsa certs using 
            ssl_test_lib:make_rsa_cert(Config0)
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

init_per_testcase(session_cleanup, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_lifetime, 5),
    application:set_env(ssl, session_delay_cleanup_time, ?DELAY),
    ssl:start(),
    ct:timetrap({seconds, 20}),
    Config;

init_per_testcase(client_unique_session, Config) ->
    ct:timetrap({seconds, 40}),
    Config;
init_per_testcase(save_specific_session, Config) ->
    ssl_test_lib:clean_start(),
    ct:timetrap({seconds, 5}),
    Config;
init_per_testcase(max_table_size, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_cache_server_max, ?MAX_TABLE_SIZE),
    application:set_env(ssl, session_cache_client_max, ?MAX_TABLE_SIZE),
    application:set_env(ssl, session_delay_cleanup_time, ?DELAY),	
    ssl:start(),			  
    ct:timetrap({seconds, 40}),
    Config.

init_customized_session_cache(Type, Config) ->
    ssl:stop(),
    application:load(ssl),
    application:set_env(ssl, session_cb, ?MODULE),
    application:set_env(ssl, session_cb_init_args, [{type, Type}]),
    ssl:start(),
    catch (end_per_testcase(list_to_atom("session_cache_process" ++ atom_to_list(Type)),
	   Config)),
    ets:new(ssl_test, [named_table, public, set]),
    ets:insert(ssl_test, {type, Type}),
    ct:timetrap({seconds, 20}),
    Config.

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
end_per_testcase(max_table_size, Config) ->
    application:unset_env(ssl, session_cach_server_max),
    application:unset_env(ssl, session_cach_client_max),
    end_per_testcase(default_action, Config);
end_per_testcase(Case, Config) when Case == session_cache_process_list;
				    Case == session_cache_process_mnesia ->
    catch ets:delete(ssl_test),
    Config;
end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
client_unique_session() ->
    [{doc, "Test session table does not grow when client "
      "sets up many connections"}].
client_unique_session(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {tcp_options, [{active, false}]},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    LastClient = clients_start(Server, ClientNode, Hostname, Port, ClientOpts, 20),
    receive 
	{LastClient, {ok, _}} ->
	    ok
    end,
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    ClientCache = element(2, State),

    1 = ssl_session_cache:size(ClientCache),
  
    ssl_test_lib:close(Server, 500),
    ssl_test_lib:close(LastClient).
		
session_cleanup() ->
    [{doc, "Test that sessions are cleand up eventually, so that the session table "
     "does not grow and grow ..."}].
session_cleanup(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_verify_opts, Config),
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
    ct:sleep(?SLEEP*2),

    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    ClientCache = element(2, State),
    ServerCache = element(3, State),
    SessionTimer = element(7, State),

    Id = proplists:get_value(session_id, SessionInfo),
    CSession = ssl_session_cache:lookup(ClientCache, {{Hostname, Port}, Id}),
    SSession = ssl_session_cache:lookup(ServerCache, {Port, Id}),

    true = CSession =/= undefined,
    true = SSession =/= undefined,

    %% Make sure session has expired and been cleaned up
    check_timer(SessionTimer),
    
    ct:sleep(?SLEEP),  %% Make sure clean has had time to run
    
    undefined = ssl_session_cache:lookup(ClientCache, {{Hostname, Port}, Id}),
    undefined = ssl_session_cache:lookup(ServerCache, {Port, Id}),

    process_flag(trap_exit, false),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).


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
save_specific_session() ->
    [{doc, "Test that we can save a specific client session"
     }].
save_specific_session(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {tcp_options, [{active, false}]},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, ClientOpts}]),
    Server ! listen,
    
    Client2 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, [{reuse_sessions, save} | ClientOpts]}]),    
    SessionID1 =
        receive 
            {Client1, S1} ->
                S1
        end,
    
    SessionID2 =
        receive 
            {Client2, S2} ->
                S2
        end,
    
    true = SessionID1 =/= SessionID2,

    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    ClientCache = element(2, State),
    2 = ssl_session_cache:size(ClientCache),

    Server ! listen,

    Client3 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port}, {host, Hostname},
                                         {mfa, {ssl_test_lib, session_id, []}},
                                         {from, self()},  {options, [{reuse_session, SessionID2} | ClientOpts]}]), 
    receive 
        {Client3, SessionID2} ->
            ok;
        {Client3, SessionID3}->
            ct:fail({got, SessionID3, expected, SessionID2});
        Other ->
            ct:fail({got,Other})
    end.

%%--------------------------------------------------------------------

max_table_size() ->
    [{doc,"Test max limit on session table"}].
max_table_size(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {tcp_options, [{active, false}]},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    LastClient = clients_start(Server, 
			    ClientNode, Hostname, Port, ClientOpts, 20),
    receive 
	{LastClient, {ok, _}} ->
	    ok
    end,
    ct:sleep(1000),
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    ClientCache = element(2, State),	
    ServerCache = element(3, State),
    N = ssl_session_cache:size(ServerCache),
    M = ssl_session_cache:size(ClientCache),
    ct:pal("~p",[{N, M}]),			
    ssl_test_lib:close(Server, 500),
    ssl_test_lib:close(LastClient),
    true = N =< ?MAX_TABLE_SIZE,
    true = M =< ?MAX_TABLE_SIZE.

%%--------------------------------------------------------------------
%%% Session cache API callbacks
%%--------------------------------------------------------------------

init(Opts) ->
    case proplists:get_value(type, Opts) of
	list ->
	    spawn(fun() -> session_loop([]) end);
	mnesia ->
	    mnesia:start(),
	    Name = atom_to_list(proplists:get_value(role, Opts)),
	    TabName = list_to_atom(Name ++ "sess_cache"),
	    {atomic,ok} = mnesia:create_table(TabName, []),
	    TabName
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
		mnesia:delete_table(Cache)
    end.

lookup(Cache, Key) ->
    case session_cb() of
	list ->
	    Cache ! {self(), lookup, Key},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    case mnesia:transaction(fun() ->
					    mnesia:read(Cache,
							Key, read)
				    end) of
		{atomic, [{Cache, Key, Value}]} ->
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
					   mnesia:write(Cache,
							{Cache, Key, Value}, write)
				   end)
    end.

delete(Cache, Key) ->
    case session_cb() of
	list ->
	    Cache ! {delete, Key};
	mnesia ->
	    {atomic, ok} =
		mnesia:transaction(fun() ->
					   mnesia:delete(Cache, Key)
				   end)
    end.

foldl(Fun, Acc, Cache) ->
    case session_cb() of
	list ->
	    Cache ! {self(),foldl,Fun,Acc},
	    receive {Cache, Res} -> Res end;
	mnesia ->
	    Foldl = fun() ->
			    mnesia:foldl(Fun, Acc, Cache)
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
					[{{Cache,{PartialKey,'_'}, '$1'},
					  [],['$1']}])
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
	    Sel = fun({{PKey0, _Id},Session}, Acc) when PKey == PKey0 ->
			  [Session | Acc];
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
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_verify_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),
    ssl_test_lib:reuse_session(ClientOpts, ServerOpts, Config).


clients_start(_Server, ClientNode, Hostname, Port, ClientOpts, 0) ->
    %% Make sure session is registered
    ct:sleep(?SLEEP * 2),
    ssl_test_lib:start_client([{node, ClientNode},
			       {port, Port}, {host, Hostname},
			       {mfa, {?MODULE, connection_info_result, []}},
			       {from, self()},  {options, ClientOpts}]);
clients_start(Server, ClientNode, Hostname, Port, ClientOpts, N) ->
    spawn_link(ssl_test_lib, start_client, 
	       [[{node, ClientNode},
		 {port, Port}, {host, Hostname},
		 {mfa, {ssl_test_lib, no_result, []}},
		 {from, self()},  {options, ClientOpts}]]),
    Server ! listen,
    wait_for_server(),
    clients_start(Server, ClientNode, Hostname, Port, ClientOpts, N-1).
	
connection_info_result(Socket) ->
    ssl:connection_information(Socket, [protocol, cipher_suite]).

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
    
wait_for_server() ->
    ct:sleep(100).	
