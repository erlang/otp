%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2024. All Rights Reserved.
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

%% Reference: https://tools.ietf.org/html/draft-miller-ssh-agent-02

-module(ssh_agent).
-moduledoc """
Callback module for using an SSH agent instead of the default ssh_file callback.

This module defines a callback handler for the communication with an
[SSH Agent](https://tools.ietf.org/html/draft-miller-ssh-agent-02) and can be
used to replace the [default callback](`m:ssh_file`). This allows to issue
signing requests to an agent that stores SSH private keys to perform
authentication.

Ssh_agent implements the `m:ssh_client_key_api`, to allow it to be used by
setting the option [`key_cb`](`t:ssh:key_cb_common_option/0`) when starting a
client (with for example [ssh:connect](`ssh:connect/3`),
[ssh:shell](`ssh:shell/1`) ).

```erlang
      {key_cb, {ssh_agent, []}}
```

The agent communication is established through a UNIX domain socket. By default,
the socket path will be fetched from the `SSH_AUTH_SOCK` environment variable,
which is the default socket path in the agent implementation of
[OpenSSH](http://www.openssh.com).

[](){: #SOCKET_PATH } In order to set a different socket path the `socket_path`
option can be set.

```erlang
      {key_cb, {ssh_agent, [{socket_path, SocketPath}]}}
```

> #### Note {: .info }
>
> The functions are _Callbacks_ for the SSH app. They are not intended to be
> called from the user's code\!
""".
-moduledoc(#{since => "OTP 23.0",
             titles =>
                 [{type,<<"Options for the ssh_agent callback module">>}]}).

-behaviour(ssh_client_key_api).

-include("ssh.hrl").
-include("ssh_agent.hrl").

-export([send/2]).
-export([add_host_key/3, add_host_key/4, is_host_key/4, is_host_key/5, user_key/2, sign/3]).

-doc """
Sets the [socket path](`m:ssh_agent#SOCKET_PATH`) for the communication with the
agent.
""".
-doc(#{title => <<"Options for the ssh_agent callback module">>}).
-type socket_path_option() :: {socket_path,  string()}.
-doc """
Sets the time-out in milliseconds when communicating with the agent via the
socket. The default value is `1000`.
""".
-doc(#{title => <<"Options for the ssh_agent callback module">>}).
-type timeout_option() :: {timeout, integer()}.
-doc """
The module which the `add_host_key` and `is_host_key` callbacks are delegated
to. Defaults to the `m:ssh_file` module.
""".
-doc(#{title => <<"Options for the ssh_agent callback module">>}).
-type call_ssh_file_option() :: {call_ssh_file, atom()}.

%% ssh_client_key_api implementation

%% Old (compatibility) version
-doc(#{equiv => add_host_key/4}).
-doc(#{since => <<"OTP 23.0">>}).
-spec add_host_key(string(),
                   public_key:public_key(),
                   Options
                  ) ->
    ok | {error, Error :: term()} when
      Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()).

add_host_key(Host, PublicKey, Options) ->
    KeyCbOpts = proplists:get_value(key_cb_private, Options, []),
    SshFileCb = proplists:get_value(call_ssh_file, KeyCbOpts, ssh_file),
    SshFileCb:add_host_key(Host, PublicKey, Options).


-doc(#{equiv => is_host_key/5}).
-doc(#{since => <<"OTP 23.0">>}).
-spec is_host_key(Key :: public_key:public_key(),
                  Host :: string(),
                  Algorithm :: ssh:pubkey_alg(),
                  Options
                 ) ->
    boolean() when
      Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()) .

is_host_key(Key, PeerName, Algorithm, Opts) ->
    KeyCbOpts = proplists:get_value(key_cb_private, Opts, []),
    SshFileCb = proplists:get_value(call_ssh_file, KeyCbOpts, ssh_file),
    SshFileCb:is_host_key(Key, PeerName, Algorithm, Opts).

%% New version
-doc "This callback is delegated to the [ssh_file](`ssh_file:add_host_key/4`) module.".
-doc(#{since => <<"OTP 23.0">>}).
-spec add_host_key(Host,
                   inet:port_number(),
                   public_key:public_key(),
                   Options
                  ) -> Result when
      Host :: inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
      Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()),
      Result :: ok | {error, Error :: term()}.

add_host_key(Host, Port, PublicKey, Options) ->
    KeyCbOpts = proplists:get_value(key_cb_private, Options, []),
    SshFileCb = proplists:get_value(call_ssh_file, KeyCbOpts, ssh_file),
    SshFileCb:add_host_key(Host, Port, PublicKey, Options).


-doc "This callback is delegated to the [ssh_file](`ssh_file:is_host_key/5`) module.".
-doc(#{since => <<"OTP 23.0">>}).
-spec is_host_key(public_key:public_key(),
                  Host,
                  inet:port_number(),
                  ssh:pubkey_alg(),
                  Options
                 ) ->
    boolean() when
      Host :: inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
      Options :: ssh_client_key_api:client_key_cb_options(call_ssh_file_option()).

is_host_key(Key, PeerName, Port, Algorithm, Opts) ->
    KeyCbOpts = proplists:get_value(key_cb_private, Opts, []),
    SshFileCb = proplists:get_value(call_ssh_file, KeyCbOpts, ssh_file),
    SshFileCb:is_host_key(Key, PeerName, Port, Algorithm, Opts).


-doc """
**Types and description**

See the api description in
[ssh_client_key_api, Module:user_key/2](`c:ssh_client_key_api:user_key/2`).
""".
-doc(#{since => <<"OTP 23.0">>}).
-spec user_key(Algorithm :: ssh:pubkey_alg(),
               Options) -> Result when 
      Result :: {ok, public_key:private_key()} |
                {ok, {ssh2_pubkey, PubKeyBlob :: binary()}} |
                {error, string()},
      Options :: ssh_client_key_api:client_key_cb_options(socket_path_option()
                                                          | timeout_option()).

user_key(Algorithm, Opts) ->
    KeyCbOpts = proplists:get_value(key_cb_private, Opts, []),

    Request = #ssh_agent_identities_request{},
    Response = ssh_agent:send(Request, KeyCbOpts),

    #ssh_agent_identities_response{keys = Keys} = Response,

    AlgorithmStr = atom_to_list(Algorithm),
    MatchingKeys = lists:filter(fun(Key) -> has_key_type(Key, AlgorithmStr) end, Keys),

    % The "client_key_api" behaviour only allows returning a single user key,
    % so we simply select the first one returned from the SSH agent here. This
    % means that if a user adds multiple keys for the same algorithm, only the
    % first one added will be used.
    case MatchingKeys of
        [#ssh_agent_key{blob = PubKeyBlob} | _OtherKeys] ->
            {ok, {ssh2_pubkey, PubKeyBlob}};
        _ ->
            {error, enoent}
    end.

-doc false.
-spec sign(binary(),
           binary(),
           Options
          ) ->
    Blob :: binary() when
      Options :: ssh_client_key_api:client_key_cb_options(socket_path_option()
                                                          | timeout_option()).

sign(PubKeyBlob, SigData, Opts) ->
    KeyCbOpts = proplists:get_value(key_cb_private, Opts, []),
    % OpenSSH does not seem to care when these flags are set for
    % signature algorithms other than RSA, so we always send them.
    SignFlags = ?SSH_AGENT_RSA_SHA2_256 bor ?SSH_AGENT_RSA_SHA2_512,
    SignRequest = #ssh_agent_sign_request{key_blob = PubKeyBlob, data = SigData, flags = SignFlags},
    SignResponse = ssh_agent:send(SignRequest, KeyCbOpts),
    #ssh_agent_sign_response{signature = #ssh_agent_signature{blob = Blob}} = SignResponse,
    Blob.

%% Utility functions

has_key_type(#ssh_agent_key{blob = KeyBlob}, Type) ->
  <<?DEC_BIN(KeyType, _KeyTypeLen), _KeyBlobRest/binary>> = KeyBlob,
  binary_to_list(KeyType) == Type.

%% Agent communication

-doc false.
send(Request, Opts) ->
    SocketPath = proplists:get_value(socket_path, Opts, os:getenv("SSH_AUTH_SOCK")),
    Timeout = proplists:get_value(timeout, Opts, 1000),

    ConnectOpts = [binary, {packet, 0}, {active, false}],
    {ok, Socket} = gen_tcp:connect({local, SocketPath}, 0, ConnectOpts, Timeout),

    BinRequest = pack(encode(Request)),
    ok = gen_tcp:send(Socket, BinRequest),

    {ok, <<Len:32/unsigned-big-integer>>} = gen_tcp:recv(Socket, 4, Timeout),
    {ok, BinResponse} = gen_tcp:recv(Socket, Len, Timeout),

    ok = gen_tcp:close(Socket),

    Response = decode(BinResponse),

    Response.

%% Message packing

pack(Data) ->
    <<(byte_size(Data)):32/unsigned-big-integer, Data/binary>>.

%% SSH Agent message encoding

encode(#ssh_agent_identities_request{}) ->
    <<?Ebyte(?SSH_AGENTC_REQUEST_IDENTITIES)>>;

encode(#ssh_agent_sign_request{key_blob = KeyBlob, data = Data, flags = Flags}) ->
    <<?Ebyte(?SSH_AGENTC_SIGN_REQUEST), ?Estring(KeyBlob), ?Estring(Data), ?Euint32(Flags)>>.

%% SSH Agent message decoding

decode_keys(<<>>, Acc, 0) ->
    lists:reverse(Acc);

decode_keys(<<?DEC_BIN(KeyBlob, _KeyBlobLen), ?DEC_BIN(Comment, _CommentLen), Rest/binary>>, Acc, N) ->
    Key = #ssh_agent_key{blob = KeyBlob, comment = Comment},
    decode_keys(Rest, [Key | Acc], N - 1).

decode_signature(<<?DEC_BIN(Format, _FormatLen), Blob/binary>>) ->
    % Decode signature according to https://tools.ietf.org/html/rfc4253#section-6.6
    <<?DEC_BIN(SignatureBlob, _SignatureBlobLen)>> = Blob,
    #ssh_agent_signature{format = Format, blob = SignatureBlob}.

decode(<<?BYTE(?SSH_AGENT_SUCCESS)>>) ->
    #ssh_agent_success{};

decode(<<?BYTE(?SSH_AGENT_FAILURE)>>) ->
    #ssh_agent_failure{};

decode(<<?BYTE(?SSH_AGENT_IDENTITIES_ANSWER), ?UINT32(NumKeys), KeyData/binary>>) ->
    #ssh_agent_identities_response{keys = decode_keys(KeyData, [], NumKeys)};

decode(<<?BYTE(?SSH_AGENT_SIGN_RESPONSE), ?DEC_BIN(Signature, _SignatureLen)>>) ->
    #ssh_agent_sign_response{signature = decode_signature(Signature)}.
