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

-include("ssh.hrl").
-include("ssh_agent.hrl").

-export([send/1, pack/1, unpack/1, encode/1, decode/1]).

%% Agent communication

% TODO: Is it safe to issue multiple requests in parallel?
% Do we need a dedicated process to handle agent access?
send(Request) ->
    SocketPath = os:getenv("SSH_AUTH_SOCK"),

    ConnectOpts = [binary, {packet, 0}, {active, false}],
    {ok, Socket} = gen_tcp:connect({local, SocketPath}, 0, ConnectOpts),

    BinRequest = ssh_agent:pack(ssh_agent:encode(Request)),
    ok = gen_tcp:send(Socket, BinRequest),

    Timeout = 1000, % TODO: Make this a parameter? What is a sensible default value?
    {ok, BinResponse} = gen_tcp:recv(Socket, 0, Timeout),

    ok = gen_tcp:close(Socket),

    Response = ssh_agent:decode(ssh_agent:unpack(BinResponse)),

    Response.

%% Message packing and unpacking

pack(Data) ->
    <<(size(Data)):32/unsigned-big-integer, Data/binary>>.

unpack(<<Len:32/unsigned-big-integer, Data:Len/binary>>) ->
    Data.

%% SSH Agent message encoding

encode(#ssh_agent_identities_request{}) ->
    <<?Ebyte(?SSH_AGENTC_REQUEST_IDENTITIES)>>;

encode(#ssh_agent_sign_request{
      key_blob = KeyBlob,
      data = Data,
      flags = Flags}) ->
    <<?Ebyte(?SSH_AGENTC_SIGN_REQUEST), ?Estring(KeyBlob), ?Estring(Data), ?Euint32(Flags)>>.

%% SSH Agent message decoding

decode_identities(<<>>, Acc, 0) ->
    lists:reverse(Acc);

decode_identities(<<?DEC_BIN(KeyBlob, _KeyBlobLen), ?DEC_BIN(Comment, _CommentLen), Rest/binary>>, Acc, N) ->
    Identity = #ssh_agent_identity{key_blob = KeyBlob, comment = Comment},
    decode_identities(Rest, [Identity | Acc], N - 1).

decode_signature(<<?DEC_BIN(Format, _FormatLen), Blob/binary>>) ->
    % TODO: Decode signature depending on signature format as per
    % https://tools.ietf.org/html/rfc4253#section-6.6
    %
    % Currently this just decodes RSA signatures correctly.
    io:fwrite("Signature format: ~p~n~n", [Format]),
    <<?DEC_BIN(RSASignBlob, _RSASignBlobLen)>> = Blob,
    #ssh_agent_signature{format = Format, blob = RSASignBlob}.

decode(<<?BYTE(?SSH_AGENT_SUCCESS)>>) ->
    #ssh_agent_success{};

decode(<<?BYTE(?SSH_AGENT_FAILURE)>>) ->
    #ssh_agent_failure{};

decode(<<?BYTE(?SSH_AGENT_IDENTITIES_ANSWER), ?UINT32(NumKeys), KeyData/binary>>) ->
    Keys = decode_identities(KeyData, [], NumKeys),
    #ssh_agent_identities_response{nkeys = NumKeys, keys = Keys};

decode(<<?BYTE(?SSH_AGENT_SIGN_RESPONSE), ?DEC_BIN(Signature, _SignatureLen)>>) ->
    #ssh_agent_sign_response{signature = decode_signature(Signature)}.
