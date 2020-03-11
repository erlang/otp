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

%% Reference: https://tools.ietf.org/html/draft-miller-ssh-agent-02

-module(ssh_agent).

-behaviour(ssh_client_key_api).

-include("ssh.hrl").
-include("ssh_agent.hrl").

-export([send/2]).
-export([add_host_key/3, add_host_key/4, is_host_key/4, is_host_key/5, user_key/2, sign/3]).

-type socket_path_option() :: {socket_path,  string()}.
-type timeout_option() :: {timeout, integer()}.
-type call_ssh_file_option() :: {call_ssh_file, atom()}.

%% ssh_client_key_api implementation

%% Old (compatibility) version
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

send(Request, Opts) ->
    SocketPath = proplists:get_value(socket_path, Opts, os:getenv("SSH_AUTH_SOCK")),
    Timeout = proplists:get_value(timeout, Opts, 1000),

    ConnectOpts = [binary, {packet, 0}, {active, false}],
    {ok, Socket} = gen_tcp:connect({local, SocketPath}, 0, ConnectOpts, Timeout),

    BinRequest = pack(encode(Request)),
    ok = gen_tcp:send(Socket, BinRequest),

    {ok, BinResponse} = gen_tcp:recv(Socket, 0, Timeout),

    ok = gen_tcp:close(Socket),

    Response = decode(unpack(BinResponse)),

    Response.

%% Message packing and unpacking

pack(Data) ->
    <<(size(Data)):32/unsigned-big-integer, Data/binary>>.

unpack(<<Len:32/unsigned-big-integer, Data:Len/binary>>) ->
    Data.

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
