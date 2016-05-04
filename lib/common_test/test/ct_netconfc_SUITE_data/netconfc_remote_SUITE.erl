%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
-module(netconfc_remote_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/src/ct_netconfc.hrl").
-include("netconfc_test_lib.hrl").

-compile(export_all).

suite() ->
    [{timetrap,?default_timeout},
     {ct_hooks, [{cth_conn_log,[{ct_netconfc,[{log_type,html}]}]}]}].

all() ->
    case os:find_executable("ssh") of
	false ->
	    {skip, "SSH not installed on host"};
	_ ->
	    [remote_crash
	    ]
    end.

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) ->
    stop_node(Case),
    Config.

end_per_testcase(Case, Config) ->
    stop_node(Case),
    ok.

stop_node(Case) ->
    {ok,Host} = inet:gethostname(),
    Node = list_to_atom("nc_" ++ atom_to_list(Case)++ "@" ++ Host),
    rpc:call(Node,erlang,halt,[]).


init_per_suite() ->
    [{timetrap,2*?default_timeout}]. % making dsa files can be slow
init_per_suite(Config) ->
    case ssh:start() of
	Ok when Ok==ok; Ok=={error,{already_started,ssh}} ->
	    ct:log("SSH started locally",[]),
	    {ok, _} =  netconfc_test_lib:get_id_keys(Config),
	    netconfc_test_lib:make_dsa_files(Config),
	    ct:log("dsa files created",[]),
	    Config;
	Other ->
	    ct:log("could not start ssh locally: ~p",[Other]),
	    {skip, "SSH could not be started locally!"}
    end.

end_per_suite(Config) ->
    ssh:stop(),
    crypto:stop(),
    netconfc_test_lib:remove_id_keys(Config),
    Config.

%% This test case is related to seq12645
%% Running the netconf server in a remote node, test that the client
%% process terminates if the remote node goes down.
remote_crash(Config) ->
    {ok,Node} = ct_slave:start(nc_remote_crash),
    Pa = filename:dirname(code:which(?NS)),
    true = rpc:call(Node,code,add_patha,[Pa]),
    
    case rpc:call(Node,ssh,start,[]) of
	Ok when Ok==ok; Ok=={error,{already_started,ssh}} ->
	    ct:log("SSH started remote",[]),
	    Server = rpc:call(Node,?NS,start,[?config(data_dir,Config)]),
	    ct:log("netconf server started remote",[]),
	    remote_crash(Node,Config);
	Other ->
	    ct:log("could not start ssh remote: ~p",[Other]),
	    {skip, "SSH could not be started remote!"}
    end.

remote_crash(Node,Config) ->
    DataDir = ?config(data_dir,Config),
    {ok,Client} = open_success(Node,DataDir),

    ns(Node,expect_reply,[{'create-subscription',[stream]},ok]),
    ?ok = ct_netconfc:create_subscription(Client),

    true = erlang:is_process_alive(Client),
    Ref = erlang:monitor(process,Client),
    rpc:call(Node,erlang,halt,[]), % take the node down as brutally as possible
    receive {'DOWN',Ref,process,Client,_} ->
	    ok
    after 10000 ->
	    ct:fail(client_still_alive)
    end.

%%%-----------------------------------------------------------------

break(_Config) ->
    test_server:break("break test case").

%%%-----------------------------------------------------------------
%% Open a netconf session which is not specified in a config file
open_success(Node,Dir) ->
    open_success(Node,Dir,[]).

%% Open a netconf session which is not specified in a config file, and
%% give som extra options in addition to the test defaults.
open_success(Node,Dir,ExtraOpts) when is_list(Dir), is_list(ExtraOpts) ->
    ns(Node,hello,[1]), % tell server to send hello with session id 1
    ns(Node,expect,[hello]), % tell server to expect a hello message from client
    open(Dir,ExtraOpts);

%% Open a named netconf session which is not specified in a config file
open_success(Node,KeyOrName,Dir) when is_atom(KeyOrName), is_list(Dir) ->
    ns(Node,hello,[1]),
    ns(Node,expect,[hello]),
    ct_netconfc:open(KeyOrName,?DEFAULT_SSH_OPTS(Dir)).

open(Dir) ->
    open(Dir,[]).
open(Dir,ExtraOpts) ->
    Opts = lists:ukeymerge(1,lists:keysort(1,ExtraOpts),
			   lists:keysort(1,?DEFAULT_SSH_OPTS(Dir))),
    ct_netconfc:open(Opts).

%%%-----------------------------------------------------------------
%%% Call server on remote node
ns(Node,Func,Args) ->
    rpc:call(Node,?NS,Func,Args).

