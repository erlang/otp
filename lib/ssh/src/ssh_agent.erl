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

-export([send/1]).

%% Agent communication

send(Request) ->
    SocketPath = os:getenv("SSH_AUTH_SOCK"),

    ConnectOpts = [binary, {packet, 0}, {active, false}],
    {ok, Socket} = gen_tcp:connect({local, SocketPath}, 0, ConnectOpts),

    BinRequest = pack(encode(Request)),
    ok = gen_tcp:send(Socket, BinRequest),

    Timeout = 1000, % TODO: Make this a parameter? What is a sensible default value?
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
