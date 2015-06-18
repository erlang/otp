%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2014. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File: ct_netconfc_SUITE.erl
%%
%% Description:
%%    This file contains the test cases for the ct_netconfc API.
%%
%% @author Support
%% @doc Netconf Client Interface.
%% @end
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
-module(netconfc1_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/src/ct_netconfc.hrl").
-include("netconfc_test_lib.hrl").

-compile(export_all).

suite() ->
    [{ct_hooks, [{cth_conn_log,
		  [{ct_netconfc,[{log_type,html}, %will be overwritten by config
				 {hosts,[my_named_connection,netconf1]}]
		   }]
		 }]
     }].

all() ->
    case os:find_executable("ssh") of
	false ->
	    {skip, "SSH not installed on host"};
	_ ->
	    [hello,
	     hello_from_server_first,
	     hello_named,
	     hello_configured,
	     hello_configured_extraopts,
	     hello_required,
	     hello_required_exists,
	     hello_global_pwd,
	     hello_no_session_id,
	     hello_incomp_base_vsn,
	     hello_no_base_cap,
	     hello_no_caps,
	     no_server_hello,
	     no_client_hello,
	     get_session_id,
	     get_capabilities,
	     faulty_user,
	     faulty_passwd,
	     faulty_port,
	     no_host,
	     no_port,
	     invalid_opt,
	     timeout_close_session,
	     get,
	     timeout_get,
	     get_xpath,
	     get_config,
	     get_config_xpath,
	     edit_config,
	     edit_config_opt_params,
	     copy_config,
	     delete_config,
	     lock,
	     unlock,
	     kill_session,
	     get_no_such_client,
	     action,
	     send_any_rpc,
	     send_any,
	     hide_password,
	     not_proper_xml,
	     prefixed_namespace,
	     receive_chunked_data,
	     timeout_receive_chunked_data,
	     close_while_waiting_for_chunked_data,
	     connection_crash,
	     get_event_streams,
	     create_subscription,
	     receive_event
	    ]
    end.


groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    ets:delete_all_objects(ns_tab),
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    case catch {crypto:start(), ssh:start()} of
	{ok, ok} ->
	    {ok, _} =  netconfc_test_lib:get_id_keys(Config),
	    netconfc_test_lib:make_dsa_files(Config),
	    Server = ?NS:start(?config(data_dir,Config)),
	    [{server,Server}|Config];
	_ ->
	    {skip, "Crypto and/or SSH could not be started!"}
    end.

end_per_suite(Config) ->
    ?NS:stop(?config(server,Config)),
    ssh:stop(),
    crypto:stop(),
    netconfc_test_lib:remove_id_keys(Config),
    Config.

hello(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hello_from_server_first(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:hello(1),
    {ok,Client} = ct_netconfc:only_open(?DEFAULT_SSH_OPTS(DataDir)),
    ct:sleep(500),
    ?NS:expect(hello),
    ?ok = ct_netconfc:hello(Client, [{capability, ["urn:com:ericsson:ebase:1.1.0"]}], infinity),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hello_named(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(any_name,DataDir),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hello_configured() ->
    [{require, netconf1}].
hello_configured(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_configured_success(netconf1,DataDir),
    ?NS:expect_do_reply('close-session',close,ok),
    {error, {no_such_name,netconf1}} = ct_netconfc:close_session(netconf1),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hello_configured_extraopts() ->
    [{require, netconf1}].
hello_configured_extraopts(Config) ->
    DataDir = ?config(data_dir,Config),
    %% Test that the cofiguration overwrites the ExtraOpts parameter
    %% to ct_netconfc:open/2.
    {ok,Client} = open_configured_success(netconf1,DataDir,[{password,"faulty"}]),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hello_required() ->
    [{require, my_named_connection, netconf1}].
hello_required(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,_Client} = open_configured_success(my_named_connection,DataDir),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(my_named_connection),
    ok.

hello_required_exists() ->
    [{require, my_named_connection, netconf1}].
hello_required_exists(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,_Client1} = open_configured_success(my_named_connection,DataDir),

    %% Check that same name can not be used twice
    {error,{connection_exists,_Client1}} =
	ct_netconfc:open(my_named_connection,[{user_dir,DataDir}]),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(my_named_connection),
    ct:sleep(500),

    %% Then check that it can be used again after the first is closed
    {ok,_Client2} = open_configured_success(my_named_connection,DataDir),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(my_named_connection),
    ok.

hello_global_pwd(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir,[{user,"any-user"},
					{password,"global-xxx"}]),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hello_no_session_id(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:hello(no_session_id),
    ?NS:expect(no_session_id,hello),
    {error,{incorrect_hello,no_session_id_found}} = open(DataDir),
    ok.

hello_incomp_base_vsn(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:hello(1,{base,"1.1"}),
    ?NS:expect(hello),
    {error,{incompatible_base_capability_vsn,"1.1"}} = open(DataDir),
    ok.

hello_no_base_cap(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:hello(1,no_base),
    ?NS:expect(hello),
    {error,{incorrect_hello,no_base_capability_found}} = open(DataDir),
    ok.

hello_no_caps(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:hello(1,no_caps),
    ?NS:expect(hello),
    {error,{incorrect_hello,capabilities_not_found}} = open(DataDir),
    ok.

no_server_hello(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:expect(undefined,hello),
    {error,{hello_session_failed,timeout}} = open(DataDir,[{timeout,2000}]),
    ok.

no_client_hello(Config) ->
    DataDir = ?config(data_dir,Config),
    ?NS:hello(1),
    {ok,Client} = ct_netconfc:only_open(?DEFAULT_SSH_OPTS(DataDir)),

    %% Allow server hello to arrive
    ct:sleep(500),

    %% Tell server to receive a get request and then die without
    %% replying since no hello has been received. (is this correct
    %% behavoiur??)
    ?NS:expect_do(get,close),
    {error,closed} = ct_netconfc:get(Client,whatever),
    ok.

get_session_id(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    1 = ct_netconfc:get_session_id(Client),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

get_capabilities(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    Caps = ct_netconfc:get_capabilities(Client),
    BaseCap = ?NETCONF_BASE_CAP ++ ?NETCONF_BASE_CAP_VSN,
    [BaseCap,"urn:ietf:params:netconf:capability:writable-running:1.0" |_] = Caps,

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

faulty_user(Config) ->
    DataDir = ?config(data_dir,Config),
    {error,{ssh,could_not_connect_to_server,
	    "Unable to connect using the available authentication methods"}} =
	open(DataDir,[{user,"yyy"}]),
    ok.

faulty_passwd(Config) ->
    DataDir = ?config(data_dir,Config),
    {error,{ssh,could_not_connect_to_server,
	    "Unable to connect using the available authentication methods"}} =
	open(DataDir,[{password,"yyy"}]),
    ok.

faulty_port(Config) ->
    DataDir = ?config(data_dir,Config),
    {error,{ssh,could_not_connect_to_server,econnrefused}} =
	open(DataDir,[{port,2062}]),
    ok.

no_host(Config) ->
    DataDir = ?config(data_dir,Config),
    Opts = lists:keydelete(ssh,1,?DEFAULT_SSH_OPTS(DataDir)),
    {error,no_host_address} = ct_netconfc:open(Opts),
    ok.

no_port(Config) ->
    DataDir = ?config(data_dir,Config),
    Opts = lists:keydelete(port,1,?DEFAULT_SSH_OPTS(DataDir)),
    {error,no_port} = ct_netconfc:open(Opts),
    ok.

invalid_opt(Config) ->
    DataDir = ?config(data_dir,Config),
    Opts1 = ?DEFAULT_SSH_OPTS(DataDir) ++ [{timeout,invalidvalue}],
    {error,{invalid_option,{timeout,invalidvalue}}} = ct_netconfc:open(Opts1),
    Opts2 = ?DEFAULT_SSH_OPTS(DataDir) ++ [{some_other_opt,true}],
    {error,{invalid_option,{some_other_opt,true}}} = ct_netconfc:open(Opts2),
    ok.

timeout_close_session(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect('close-session'),
    true = erlang:is_process_alive(Client),
    {error,timeout} = ct_netconfc:close_session(Client,1000),
    false = erlang:is_process_alive(Client),
    ok.

get(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Data = [{server,[{xmlns,"myns"}],[{name,[],["myserver"]}]}],
    ?NS:expect_reply('get',{data,Data}),
    {ok,Data} = ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]}),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

timeout_get(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect('get'),
    {error,timeout} = ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]},1000),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

get_xpath(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Data = [{server,[{xmlns,"myns"}],[{name,[],["myserver"]}]}],
    ?NS:expect_reply({'get',xpath},{data,Data}),
    {ok,Data} = ct_netconfc:get(Client,{xpath,"/server"}),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

get_config(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Data = [{server,[{xmlns,"myns"}],[{name,[],["myserver"]}]}],
    ?NS:expect_reply('get-config',{data,Data}),
    {ok,Data} = ct_netconfc:get_config(Client,running,
				       {server,[{xmlns,"myns"}],[]}),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

get_config_xpath(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Data = [{server,[{xmlns,"myns"}],[{name,[],["myserver"]}]}],
    ?NS:expect_reply({'get-config',xpath},{data,Data}),
    {ok,Data} = ct_netconfc:get_config(Client,running,{xpath,"/server"}),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

edit_config(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply('edit-config',ok),
    ?ok = ct_netconfc:edit_config(Client,running,
				  {server,[{xmlns,"myns"}],
				   [{name,["myserver"]}]}),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

edit_config_opt_params(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply({'edit-config',{'default-operation',"none"}},ok),
    ?ok = ct_netconfc:edit_config(Client,running,
				  {server,[{xmlns,"myns"}],
				   [{name,["myserver"]}]},
				  [{'default-operation',["none"]}]),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

copy_config(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply('copy-config',ok),
    ?ok = ct_netconfc:copy_config(Client,startup,running),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

delete_config(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply('delete-config',ok),
    ?ok = ct_netconfc:delete_config(Client,startup),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

lock(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply('lock',ok),
    ?ok = ct_netconfc:lock(Client,running),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

unlock(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply('unlock',ok),
    ?ok = ct_netconfc:unlock(Client,running),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

kill_session(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    ?NS:hello(2),
    ?NS:expect(2,hello),
    {ok,_OtherClient} = open(DataDir),

    ?NS:expect_do_reply('kill-session',{kill,2},ok),
    ?ok = ct_netconfc:kill_session(Client,2),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),

    ok.

get_no_such_client(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    case ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]}) of
	{error,no_such_client} ->
	    ok;
	{error,closed} ->
	    %% Means that the Client process was not terminated before the call.
	    %% Give it one more go.
	    {error,no_such_client} =
		ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]})
    end,
    ok.

action(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Data = [{myactionreturn,[{xmlns,"myns"}],["value"]}],
    %% test either to receive {data,Data} or {ok,Data},
    %% both need to be handled
    ct:log("Client will receive {~w,~p}", [data,Data]),
    ct:log("Expecting ~p", [{ok, Data}]),
    ?NS:expect_reply(action,{data, Data}),
    {ok, Data} = ct_netconfc:action(Client,{myaction,[{xmlns,"myns"}],[]}),

    ct:log("Client will receive {~w,~p}", [ok,Data]),
    ct:log("Expecting ~p", [ok]),
    ?NS:expect_reply(action,{ok, Data}),
    ok = ct_netconfc:action(Client,{myaction,[{xmlns,"myns"}],[]}),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

send_any_rpc(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Data = [{server,[{xmlns,"myns"}],[{name,[],["myserver"]}]}],
    GetConf = {'get-config',
	       [{source,["running"]},
		{filter,[{type,"subtree"}],
		 [{server,[{xmlns,"myns"}],[]}]}]},
    ?NS:expect_reply('get-config',{data,Data}),
    [{data,?NETCONF_NAMESPACE_ATTR,Data}] = ct_netconfc:send_rpc(Client,GetConf),

    EditConf = {'edit-config',
		[{target,["running"]},
		 {config,[{server,[{xmlns,"myns"}],
			   [{name,["myserver"]}]}]}]},
    ?NS:expect_reply('edit-config',ok),
    [{ok,?NETCONF_NAMESPACE_ATTR,[]}] = ct_netconfc:send_rpc(Client,EditConf),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

send_any(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    %% Correct get-config rpc
    Data = [{server,[{xmlns,"myns"}],[{name,[],["myserver"]}]}],
    RpcAttr1 = ?NETCONF_NAMESPACE_ATTR ++ [{'message-id',"1"}],
    RpcGetConf = {rpc,RpcAttr1,
		  [{'get-config',
		    [{source,["running"]},
		     {filter,[{type,"subtree"}],
		      [{server,[{xmlns,"myns"}],[]}]}]}]},
    ?NS:expect_reply('get-config',{data,Data}),
    {'rpc-reply',RpcAttr1,[{data,_,Data}]} = ct_netconfc:send(Client,RpcGetConf),

    %% Correct edit-config rpc
    RpcAttr2 = ?NETCONF_NAMESPACE_ATTR ++ [{'message-id',"2"}],
    RpcEditConf = {rpc,RpcAttr2,
		   [{'edit-config',
		     [{target,["running"]},
		      {config,[{server,[{xmlns,"myns"}],
				[{name,["myserver"]}]}]}]}]},
    ?NS:expect_reply('edit-config',ok),
    {'rpc-reply',RpcAttr2,[{ok,_,[]}]} = ct_netconfc:send(Client,RpcEditConf),

    %% Send any data
    ?NS:expect_reply(any,{ok,[],[]}),
    {ok,_,[]} = ct_netconfc:send(Client,{any,[],[]}),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

hide_password(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    Password = "my_very_secret_password",
    Data = [{passwords,[{xmlns,"myns"}],
	     [{password,[{xmlns,"pwdns"}],[Password]},
	      {password,[],[Password]}]}],
    ?NS:expect_reply('get',{data,Data}),
    ct:capture_start(), % in case of html logging
    {ok,Data} = ct_netconfc:get(Client,{passwords,[{xmlns,"myns"}],[]}),
    ct:capture_stop(),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),

    Log = filename:join(?config(priv_dir,Config),"hide_password-netconf.txt"),

    Text =
	case file:read_file(Log) of
	    {ok,Bin} ->
		Bin;
	    _NoLog ->
		%% Assume html logging
		list_to_binary(ct:capture_get())
	end,

    nomatch = binary:match(Text,list_to_binary(Password)),

    ok.

not_proper_xml(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    NS = list_to_binary(?NETCONF_NAMESPACE),
    NotProper = <<"<rpc-reply message-id=\"1\" xmlns=\"",
		  NS/binary,"\"><data></rpc-reply>">>,
    ?NS:expect_reply('get',NotProper),
    {error,{failed_to_parse_received_data,_}} =
	ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]}),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

prefixed_namespace(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    NS = list_to_binary(?NETCONF_NAMESPACE),

    %% Test that data element can be properly decoded and that
    %% prefixed namespace attributes (exepct the netconf namespace)
    %% are forwarded to the content of the data element - i.e. that
    %% the xmlns:my is forwarded from the rpc-reply element to the
    %% server element below.
    Data = <<"<nc:rpc-reply message-id=\"1\" xmlns:nc=\"",
	     NS/binary,"\" xmlns:my=\"myns\"><nc:data><my:server>",
	     "<my:name my:lang=\"en\">myserver</my:name></my:server>"
	     "</nc:data></nc:rpc-reply>">>,
    ?NS:expect_reply('get',Data),
    {ok,[{'my:server',[{'xmlns:my',"myns"}],
	  [{'my:name',[{'my:lang',"en"}],["myserver"]}]}]} =
	ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]}),

    Ok = <<"<nc:rpc-reply message-id=\"2\" xmlns:nc=\"",
	   NS/binary,"\"><nc:ok/></nc:rpc-reply>">>,
    ?NS:expect_reply('edit-config',Ok),
    ?ok = ct_netconfc:edit_config(Client,running,
				  {server,[{xmlns,"myns"}],
				   [{name,["myserver"]}]}),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

%% Test that the client can parse data which is received in chunks,
%% i.e. when the complete rpc-reply is not contained in one single ssh
%% data message.
receive_chunked_data(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    %% Construct the data to return from netconf server
    Data = [{servers,[{xmlns,"myns"}],
	     [{server,[],[{name,[],["server0"]}]},
	      {server,[],[{name,[],["server1"]}]},
	      {server,[],[{name,[],["server2"]}]},
	      {server,[],[{name,[],["server3"]}]},
	      {server,[],[{name,[],["server4"]}]},
	      {server,[],[{name,[],["server5"]}]},
	      {server,[],[{name,[],["server6"]}]},
	      {server,[],[{name,[],["server7"]}]},
	      {server,[],[{name,[],["server8"]}]},
	      {server,[],[{name,[],["server9"]}]}]
	    }],
    Rpc = {'rpc-reply',?NETCONF_NAMESPACE_ATTR ++ [{'message-id',"1"}],
	   [{data,Data}]},
    Xml = list_to_binary(xmerl:export_simple_element(Rpc,xmerl_xml)),
    Netconf =
	<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
	  Xml/binary,"\n",?END_TAG/binary>>,

    %% Split the data in some chunks
    PartLength = size(Netconf) div 3,
    <<Part1:PartLength/binary,Part2:PartLength/binary,Part3:PartLength/binary,
      Part4/binary>> = Netconf,

    %% Spawn a process which will wait a bit for the client to send
    %% the request (below), then order the server to the chunks of the
    %% rpc-reply one by one.
    spawn(fun() -> ct:sleep(500),?NS:hupp(send,Part1),
		   ct:sleep(100),?NS:hupp(send,Part2),
		   ct:sleep(100),?NS:hupp(send,Part3),
		   ct:sleep(100),?NS:hupp(send,Part4)
	  end),

    %% Order server to expect a get - then the process above will make
    %% sure the rpc-reply is sent.
    ?NS:expect('get'),
    {ok,Data} = ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]}),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

%% Same as receive_chunked_data, but timeout waiting for last part.
timeout_receive_chunked_data(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    %% Construct the data to return from netconf server
    Data = [{servers,[{xmlns,"myns"}],
	     [{server,[],[{name,[],["server0"]}]},
	      {server,[],[{name,[],["server1"]}]},
	      {server,[],[{name,[],["server2"]}]},
	      {server,[],[{name,[],["server3"]}]},
	      {server,[],[{name,[],["server4"]}]},
	      {server,[],[{name,[],["server5"]}]},
	      {server,[],[{name,[],["server6"]}]},
	      {server,[],[{name,[],["server7"]}]},
	      {server,[],[{name,[],["server8"]}]},
	      {server,[],[{name,[],["server9"]}]}]
	    }],
    Rpc = {'rpc-reply',?NETCONF_NAMESPACE_ATTR ++ [{'message-id',"1"}],
	   [{data,Data}]},
    Xml = list_to_binary(xmerl:export_simple_element(Rpc,xmerl_xml)),
    Netconf =
	<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
	  Xml/binary,"\n",?END_TAG/binary>>,

    %% Split the data in some chunks
    PartLength = size(Netconf) div 3,
    <<Part1:PartLength/binary,Part2:PartLength/binary,_Part3:PartLength/binary,
      _Part4/binary>> = Netconf,

    %% Spawn a process which will wait a bit for the client to send
    %% the request (below), then order the server to the chunks of the
    %% rpc-reply one by one.
    spawn(fun() -> ct:sleep(500),?NS:hupp(send,Part1),
		   ct:sleep(100),?NS:hupp(send,Part2)
	  end),

    %% Order server to expect a get - then the process above will make
    %% sure the rpc-reply is sent - but only a part of it - then timeout.
    ?NS:expect('get'),
    {error,timeout} = ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]},2000),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

%% Same as receive_chunked_data, but close while waiting for last part.
close_while_waiting_for_chunked_data(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    %% Construct the data to return from netconf server
    Data = [{servers,[{xmlns,"myns"}],
	     [{server,[],[{name,[],["server0"]}]},
	      {server,[],[{name,[],["server1"]}]},
	      {server,[],[{name,[],["server2"]}]},
	      {server,[],[{name,[],["server3"]}]},
	      {server,[],[{name,[],["server4"]}]},
	      {server,[],[{name,[],["server5"]}]},
	      {server,[],[{name,[],["server6"]}]},
	      {server,[],[{name,[],["server7"]}]},
	      {server,[],[{name,[],["server8"]}]},
	      {server,[],[{name,[],["server9"]}]}]
	    }],
    Rpc = {'rpc-reply',?NETCONF_NAMESPACE_ATTR ++ [{'message-id',"1"}],
	   [{data,Data}]},
    Xml = list_to_binary(xmerl:export_simple_element(Rpc,xmerl_xml)),
    Netconf =
	<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
	  Xml/binary,"\n",?END_TAG/binary>>,

    %% Split the data in some chunks
    PartLength = size(Netconf) div 3,
    <<Part1:PartLength/binary,Part2:PartLength/binary,_Part3:PartLength/binary,
      _Part4/binary>> = Netconf,

    %% Spawn a process which will wait a bit for the client to send
    %% the request (below), then order the server to the chunks of the
    %% rpc-reply one by one.
    spawn(fun() -> ct:sleep(500),?NS:hupp(send,Part1),
		   ct:sleep(100),?NS:hupp(send,Part2),
		   ct:sleep(100),?NS:hupp(kill)
	  end),

    %% Order server to expect a get - then the process above will make
    %% sure the rpc-reply is sent - but only a part of it - then close.
    ?NS:expect('get'),
    {error,closed} = ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]},2000),
    ok.

connection_crash(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),

    %% Test that if the test survives killing the connection
    %% process. Earlier this caused ct_util_server to terminate, and
    %% this aborting the complete test run.
    spawn(fun() -> ct:sleep(500),exit(Client,kill) end),
    ?NS:expect(get),
    {error,{closed,killed}}=ct_netconfc:get(Client,{server,[{xmlns,"myns"}],[]}),
    ok.

get_event_streams(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    StreamNames = ["NETCONF","stream1","stream2"],
    Streams = [{N,[{description,"descr of " ++ N}]} || N <- StreamNames],
    StreamsXml = [{stream,[{name,[N]}|[{Tag,[Value]} || {Tag,Value} <- Data]]}
                  || {N,Data} <- Streams],
    ReplyData = [{netconf,?NETMOD_NOTIF_NAMESPACE_ATTR,[{streams,StreamsXml}]}],
    ?NS:expect_reply('get',{data,ReplyData}),
    {ok,Streams} = ct_netconfc:get_event_streams(Client,StreamNames),

    ?NS:expect_reply('get',{data,ReplyData}),
    {ok,Streams} = ct_netconfc:get_event_streams(Client,StreamNames,5000),

    ?NS:expect('get'),
    {error,timeout} = ct_netconfc:get_event_streams(Client,100),

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),
    ok.

create_subscription(Config) ->
    DataDir = ?config(data_dir,Config),

    %% All defaults
    {ok,Client1} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream]},ok),
    ?ok = ct_netconfc:create_subscription(Client1),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client1),

    %% All defaults with timeout
    {ok,Client1a} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream]},ok),
    ?ok = ct_netconfc:create_subscription(Client1a,5000),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client1a),

    %% All defaults timing out
    {ok,Client1b} = open_success(DataDir),
    ?NS:expect({'create-subscription',[stream]}),
    {error,timeout} = ct_netconfc:create_subscription(Client1b,100),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client1b),

    %% Stream
    {ok,Client2} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream]},ok),
    Stream = "some_stream",
    ?ok = ct_netconfc:create_subscription(Client2,Stream),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client2),

    %% Filter
    {ok,Client3} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,filter]},ok),
    Filter = {notification,?NETMOD_NOTIF_NAMESPACE_ATTR,
	      [eventTime]},
    ?ok = ct_netconfc:create_subscription(Client3,Filter),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client3),

    %% Filter with timeout
    {ok,Client3a} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,filter]},ok),
    ?ok = ct_netconfc:create_subscription(Client3a,Filter,5000),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client3a),

    %% Filter timing out
    {ok,Client3b} = open_success(DataDir),
    ?NS:expect({'create-subscription',[stream,filter]}),
    {error,timeout}=ct_netconfc:create_subscription(Client3b,Filter,100),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client3b),

    %% Stream and filter
    {ok,Client4} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,filter]},ok),
    ?ok = ct_netconfc:create_subscription(Client4,Stream,Filter),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client4),

    %% Start/stop time
    {ok,Client5} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,startTime,stopTime]},ok),
    StartTime = xs_datetime({D,{H,M,S}}= calendar:local_time()),
    StopTime = xs_datetime({D,{H+2,M,S}}),
    ?ok = ct_netconfc:create_subscription(Client5,StartTime,StopTime),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client5),

    %% Start/stop time with timeout
    {ok,Client5a} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,startTime,stopTime]},ok),
    ?ok = ct_netconfc:create_subscription(Client5a,StartTime,StopTime,5000),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client5a),

    %% Start/stop time timing out
    {ok,Client5b} = open_success(DataDir),
    ?NS:expect({'create-subscription',[stream,startTime,stopTime]}),
    {error,timeout} =
	ct_netconfc:create_subscription(Client5b,StartTime,StopTime,100),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client5b),

    %% Stream and start/stop time
    {ok,Client6} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,startTime,stopTime]},ok),
    ?ok = ct_netconfc:create_subscription(Client6,Stream,StartTime,StopTime),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client6),

    %% Filter and start/stop time
    {ok,Client7} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,filter,startTime,stopTime]},
		    ok),
    ?ok = ct_netconfc:create_subscription(Client7,Filter,
					  StartTime,StopTime),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client7),

    %% Stream, filter and start/stop time
    {ok,Client8} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,filter,startTime,stopTime]},
		    ok),
    ?ok = ct_netconfc:create_subscription(Client8,Stream,Filter,
					  StartTime,StopTime),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client8),

    %% Multiple filters
    {ok,Client9} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream,filter]},ok),
    MultiFilters = [{event,[{xmlns,"http://my.namespaces.com/event"}],
		     [{eventClass,["fault"]},
		      {severity,["critical"]}]},
		    {event,[{xmlns,"http://my.namespaces.com/event"}],
		     [{eventClass,["fault"]},
		      {severity,["major"]}]}],
    ?ok = ct_netconfc:create_subscription(Client9,MultiFilters),
    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client9),

    ok.

receive_event(Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(DataDir),
    ?NS:expect_reply({'create-subscription',[stream]},ok),
    ?ok = ct_netconfc:create_subscription(Client),

    ?NS:hupp(send_event),

    receive
	%% Matching ?NS:make_msg(event)
	{notification,?NETCONF_NOTIF_NAMESPACE_ATTR,
	 [{eventTime,[],[_Time]},
	  {event,[{xmlns,"http://my.namespaces.com/event"}],
	   [{severity,_,_},
	    {description,_,_}]}]} ->
	    ok;
	Other ->
	    ct:fail({got_unexpected_while_waiting_for_event, Other})
    after 3000 ->
	    ct:fail(timeout_waiting_for_event)
    end,

    ?NS:expect_do_reply('close-session',close,ok),
    ?ok = ct_netconfc:close_session(Client),

    ok.

%%%-----------------------------------------------------------------

break(_Config) ->
    test_server:break("break test case").

br() ->
    test_server:break("").

%%%-----------------------------------------------------------------
%% Open a netconf session which is not specified in a config file
open_success(Dir) ->
    open_success(Dir,[]).

%% Open a netconf session which is not specified in a config file, and
%% give som extra options in addition to the test defaults.
open_success(Dir,ExtraOpts) when is_list(Dir), is_list(ExtraOpts) ->
    ?NS:hello(1), % tell server to send hello with session id 1
    ?NS:expect(hello), % tell server to expect a hello message from client
    open(Dir,ExtraOpts);

%% Open a named netconf session which is not specified in a config file
open_success(KeyOrName,Dir) when is_atom(KeyOrName), is_list(Dir) ->
    ?NS:hello(1),
    ?NS:expect(hello),
    ct_netconfc:open(KeyOrName,?DEFAULT_SSH_OPTS(Dir)).

open(Dir) ->
    open(Dir,[]).
open(Dir,ExtraOpts) ->
    Opts = lists:ukeymerge(1,lists:keysort(1,ExtraOpts),
			   lists:keysort(1,?DEFAULT_SSH_OPTS(Dir))),
    ct_netconfc:open(Opts).

%%%-----------------------------------------------------------------
%%% Open a netconf session which is specified in a config file
%%% KeyOrName is the config key (server_id()) or name given in a
%%% require statement (target_name()).
open_configured_success(KeyOrName,Dir) when is_atom(KeyOrName) ->
    open_configured_success(KeyOrName,Dir,[]).
open_configured_success(KeyOrName,Dir,ExtraOpts) when is_atom(KeyOrName) ->
    ?NS:hello(1),
    ?NS:expect(hello),
    ct_netconfc:open(KeyOrName,[{user_dir,Dir}|ExtraOpts]).

%%%-----------------------------------------------------------------
%%% Convert erlang datetime to the simplest variant of XML dateTime
xs_datetime({{Y,M,D},{H,Mi,S}}) ->
    lists:flatten(
      io_lib:format("~p-~s-~sT~s:~s:~s",[Y,pad(M),pad(D),pad(H),pad(Mi),pad(S)])).

pad(I) when I<10 ->
    "0"++integer_to_list(I);
pad(I) ->
    integer_to_list(I).
