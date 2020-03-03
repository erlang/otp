%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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

%%% Description: ssh-agent mock
-module(ssh_agent_mock_server).

-behaviour(gen_server).

-include_lib("ssh/src/ssh.hrl").
-include_lib("ssh/src/ssh_agent.hrl").

-export([respond/1, check_mktemp/1]).
-export([start_link/1, stop/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

-record(state, {socket, priv_key, pub_key, socket_path}).

-define(SIG_ALG, 'ssh-rsa').

start_link(PrivKeyDir) ->
    {ok, PrivKey} =ssh_file:user_key(?SIG_ALG, [{user_dir, PrivKeyDir}]),

    %% We cannot use priv_dir because unix socket paths are limited to 108 bytes.
    SocketPath = string:chomp(os:cmd("mktemp -u")),
    PubKey = extract_pubkey(PrivKey),

    InitialState = #state{socket_path=SocketPath, priv_key=PrivKey, pub_key=PubKey},
    {ok, _} = gen_server:start_link(?MODULE, InitialState, []),
    {ok, SocketPath}.

stop(SocketPath) ->
    ConnectOpts = [binary, {packet, 0}, {active, false}],
    {ok, Socket} = gen_tcp:connect({local, SocketPath}, 0, ConnectOpts, 1000),
    ok = gen_tcp:send(Socket, <<0>>),
    ok = gen_tcp:close(Socket).

init(InitialState = #state{socket_path=SocketPath}) ->
    Address = {local, SocketPath},
    ConnectOpts = [local, binary, {ip, Address}, {packet, 0}],

    {ok, Socket} = gen_tcp:listen(0, ConnectOpts),
    gen_server:cast(self(), accept),
    {ok, InitialState#state{socket=Socket}}.

handle_cast(accept, State = #state{socket=Socket}) ->
    {ok, _} = gen_tcp:accept(Socket),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.

handle_info({tcp, Socket, <<Len:32/unsigned-big-integer, Data:Len/binary>>}, State) ->
    Response = handle_request(Data, State),
    ok = gen_tcp:send(Socket, Response),
    {noreply, State};

handle_info({tcp, _, <<0>>}, State) ->
    {stop, normal, State};

handle_info({tcp_closed, _Socket}, State) ->
    gen_server:cast(self(), accept),
    {noreply, State}.

handle_request(<<11>>, #state{pub_key=PubKey}) ->
    %% REQUEST_IDENTITIES
    PubKeyLen = byte_size(PubKey),
    <<?UINT32(18 + PubKeyLen), % message length
      ?BYTE(12),                 % message type   (1 byte)
      ?UINT32(1),                % number of keys (4 bytes)
      ?STRING(PubKey),           % PubKey (4 + PubKeyLen bytes)
      ?STRING(<<"lorem">>)       % key 1 comment  (4 + 5 bytes)
    >>;

handle_request(<<13, Rest/binary>>, #state{priv_key=PrivKey, pub_key=PubKey}) ->
    Flags = ?SSH_AGENT_RSA_SHA2_256 bor ?SSH_AGENT_RSA_SHA2_512,
    <<?DEC_BIN(PubKey, _KeyBlobLen), ?DEC_BIN(Data, _DataLen), ?Euint32(Flags)>> = Rest,

    Hash = ssh_transport:sha(?SIG_ALG),
    Sig = ssh_transport:sign(Data, Hash, PrivKey),
    SigLen = byte_size(Sig),
    <<?UINT32(20 + SigLen),         % message length
      ?BYTE(14),                   % message type   (1 byte)
      ?STRING(                     % nested string (4 bytes)
         <<?STRING(<<"ssh-rsa">>), % signature format (4 + 7 bytes)
           ?STRING(Sig)            % signature blob (4 + SigLen bytes)
         >>
      )
    >>.

terminate(_Reason, #state{socket_path=SocketPath, socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    ok = file:delete(SocketPath).

respond(BinResponse) ->
    %% We cannot use priv_dir because unix socket paths are limited to 108 bytes.
    SocketPath = string:chomp(os:cmd("mktemp -u")),

    Parent = self(),

    spawn(fun() ->
              Address = {local, SocketPath},
              ConnectOpts = [local, binary, {ip, Address}, {packet, 0}, {active, false}],

              {ok, ListenSocket} = gen_tcp:listen(0, ConnectOpts),
              Parent ! listening,

              {ok, Socket} = gen_tcp:accept(ListenSocket),
              {ok, _BinRequest} = gen_tcp:recv(Socket, 0),

              ok = gen_tcp:send(Socket, BinResponse),
              ok = gen_tcp:close(Socket),
              ok = gen_tcp:close(ListenSocket),
              ok = file:delete(SocketPath)
          end),

    receive
      listening -> SocketPath
    end.

check_mktemp(Config) ->
    case os:find_executable("mktemp") of
       false ->
            {skip, "Can't find mktemp in your $PATH"};
        _ ->
            Config
    end.

extract_pubkey(PrivKey) ->
    PubKey = ssh_transport:extract_public_key(PrivKey),
    ssh_message:ssh2_pubkey_encode(PubKey).
