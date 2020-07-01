%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-module(ssh_agent_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ssh.hrl").
-include("ssh_agent.hrl").
-include("ssh_test_lib.hrl").

-export([
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         connect_with_ssh_agent/1,
         request_identities/1,
         sign_request/1
        ]).


%% Test configuration

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [request_identities, sign_request, connect_with_ssh_agent].

init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
           ok = ssh:start(),
           chk_unix_domain_socket(Config)
       end
      ).

end_per_suite(_Config) ->
    ok = ssh:stop().

init_per_testcase(connect_with_ssh_agent, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    UserDir = proplists:get_value(priv_dir, Config),
    AgentUserDir = filename:join(UserDir,"agent"), % just to separate them in the tests
                                                % so we know that the right dir is used
    file:make_dir(AgentUserDir),
    %% Arrange the host keys in <priv_dir>/system
    ct:log("Host keys setup for: ~p",
           [ssh_test_lib:setup_all_host_keys(Config)]),
    %% Copy the user's files used by the daemon
    {ok,_} = file:copy(filename:join(DataDir,"authorized_keys"), 
                       filename:join(UserDir,"authorized_keys")),
    %% And copy the user's files used by the agent (and not by the user)
    {ok,_} = file:copy(filename:join(DataDir,"id_rsa"),
                       filename:join(AgentUserDir,"id_rsa")),
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test cases

%%% Request a list of identities
request_identities(_Config) ->
    Request = #ssh_agent_identities_request{},

    SocketPath =
      ssh_agent_mock_server:respond(
        <<?UINT32(41),            % message length
          ?BYTE(12),              % message type   (1 byte)
          ?UINT32(2),             % number of keys (4 bytes)
          ?STRING(<<"key-1">>),   % key 1 blob     (4 + 5 bytes)
          ?STRING(<<"lorem">>),   % key 1 comment  (4 + 5 bytes)
          ?STRING(<<"key-2">>),   % key 2 blob     (4 + 5 bytes)
          ?STRING(<<"ipsum">>)    % key 2 comment  (4 + 5 bytes)
        >>
      ),

    Opts = [{socket_path, SocketPath}],
    Response = ssh_agent:send(Request, Opts),

    #ssh_agent_identities_response{keys = Keys} = Response,

    [{ssh_agent_key,<<"key-1">>,<<"lorem">>},
     {ssh_agent_key,<<"key-2">>,<<"ipsum">>}] = Keys,

    ok.

%%% Request a signature on a binary blob
sign_request(_Config) ->
    PubKeyBlob = <<"key">>,
    SigData = <<"data">>,

    SignFlags = ?SSH_AGENT_RSA_SHA2_256 bor ?SSH_AGENT_RSA_SHA2_512,
    SignRequest = #ssh_agent_sign_request{key_blob = PubKeyBlob, data = SigData, flags = SignFlags},

    SocketPath =
      ssh_agent_mock_server:respond(
        <<?UINT32(29),                 % message length
          ?BYTE(14),                   % message type   (1 byte)
          ?STRING(
            <<?STRING(<<"ssh-rsa">>),  % signature format (4 + 7 bytes)
              ?STRING(<<"signature">>) % signature blob (4 + 9 bytes)
            >>
          )                            % signature total (4 + 24 bytes)
        >>
      ),

    Opts = [{socket_path, SocketPath}],
    SignResponse = ssh_agent:send(SignRequest, Opts),

    #ssh_agent_sign_response{signature = #ssh_agent_signature{format = Format, blob = Sig}} = SignResponse,
    Format = <<"ssh-rsa">>,
    Sig = <<"signature">>,

    ok.

%%% Connect with RSA key from SSH agent
connect_with_ssh_agent(Config) ->
    UserDir = PrivDir = proplists:get_value(priv_dir, Config),
    AgentUserDir = filename:join(UserDir,"agent"),
    SystemDir = filename:join(PrivDir, "system"),
    {ok, SocketPath} = ssh_agent_mock_server:start_link('rsa-sha2-256', AgentUserDir),
    {Pid, Host, Port} = ssh_test_lib:daemon([{system_dir, SystemDir},
                                             {user_dir, UserDir}]),
    ConnectionRef = ssh_test_lib:connect(Host, Port, [{user_dir, UserDir},
                                                      {silently_accept_hosts, true},
                                                      {user_interaction, false},
                                                      {auth_methods, "publickey"},
                                                      {key_cb, {ssh_agent, [{socket_path, SocketPath}]}}
                                                     ]),
    ssh:close(ConnectionRef),
    ssh:stop_daemon(Pid),
    ssh_agent_mock_server:stop(SocketPath).


%%%================================================================
chk_unix_domain_socket(Config0) ->
    case ssh_agent_mock_server:check_mktemp(Config0) of
        {skip, Msg} ->
            {skip, Msg};

        Config ->
            SocketPath = string:chomp(os:cmd("mktemp -u")),
            case gen_tcp:listen(0, [local,  {ip, {local,SocketPath}}]) of
                {error,eafnosupport} ->
                    file:delete(SocketPath),
                    {skip, "Unix Domain Sockets are not supported"};
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    file:delete(SocketPath),
                    Config
            end
    end.
