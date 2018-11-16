%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(ssh_engine_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,40}}].

all() -> 
    [{group, dsa_key},
     {group, rsa_key}
    ].

groups() ->
    [{dsa_key, [], basic_tests()},
     {rsa_key, [], basic_tests()}
    ].

basic_tests() ->
    [simple_connect
    ].


%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ssh:start(),
    ?CHECK_CRYPTO(
       case crypto:info_lib() of
           [{_,_, <<"OpenSSL 1.0.1s-freebsd  1 Mar 2016">>}] ->
               {skip, "Strange Engine stuff"};

           _ ->
               case load_engine() of
                   {ok,E} ->
                       [{engine,E}|Config];
                   {error, notsup} ->
                       {skip, "Engine not supported on this OpenSSL version"};
                   {error, bad_engine_id} ->
                       {skip, "Dynamic Engine not supported"};
                   Other ->
                       ct:log("Engine load failed: ~p",[Other]),
                       {fail, "Engine load failed"}
               end
       end
      ).

end_per_suite(Config) ->
    catch crypto:engine_unload( proplists:get_value(engine,Config) ),
    ssh:stop().

%%--------------------------------------------------------------------
init_per_group(dsa_key, Config) ->
    case lists:member('ssh-dss',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
            start_daemon(Config, 'ssh-dss', "dsa_private_key.pem");
	false ->
	    {skip, unsupported_pub_key}
    end;
init_per_group(rsa_key, Config) ->
    case lists:member('ssh-rsa',
		      ssh_transport:default_algorithms(public_key)) of
	true ->
            start_daemon(Config, 'ssh-rsa', "rsa_private_key.pem");
	false ->
	    {skip, unsupported_pub_key}
    end.

start_daemon(Config, KeyType, KeyId) ->
    SystemDir = proplists:get_value(data_dir, Config),
    FullKeyId = filename:join(SystemDir, KeyId),
    KeyCBOpts = [{engine, proplists:get_value(engine,Config)},
                 {KeyType, FullKeyId}
                ],
    Opts = [{key_cb, {ssh_key_cb_engine_keys, KeyCBOpts}}],
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config, Opts),
    [{host_port,{Host,Port}}, {daemon_pid,Pid}| Config].


end_per_group(_, Config) ->
    catch ssh:stop_daemon(proplists:get_value(daemon_pid,Config)),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%% A simple exec call
simple_connect(Config) ->
    {Host,Port} = proplists:get_value(host_port, Config),
    CRef = ssh_test_lib:std_connect(Config, Host, Port, []),
    ssh:close(CRef).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
load_engine() ->
    case crypto:get_test_engine() of
        {ok, Engine} ->
            try
                %% The test engine has it's own fake rsa sign/verify that
                %% you don't want to use, so exclude it from methods to load:
                Methods = 
                    crypto:engine_get_all_methods() -- [engine_method_rsa],
                crypto:engine_load(<<"dynamic">>,
                                   [{<<"SO_PATH">>, Engine},
                                    <<"LOAD">>],
                                   [],
                                   Methods
                                  )
            catch
                error:notsup ->
                    {error, notsup}
            end;

        {error, Error} ->
            {error, Error}
    end.

start_std_daemon(Opts, Config) ->
    ct:log("starting std_daemon",[]),
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config, Opts),
    ct:log("started ~p:~p  ~p",[Host,Port,Opts]),
    [{srvr_pid,Pid},{srvr_addr,{Host,Port}} | Config].
