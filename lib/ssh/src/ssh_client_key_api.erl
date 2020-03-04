%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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

-module(ssh_client_key_api).

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-export_type([client_key_cb_options/1]).

%%%****************************************************************
%%% The option key_cb_private is to pass options needed by other
%%% callback modules than the default ssh_file.erl
%%%
%%% If ssh:connect(x, n, [ {key_cb_private, {hi,{there}}} ]
%%% is called, the term() will be {hi,{there}}

-type client_key_cb_options(T) :: [{key_cb_private,[T]} | ssh:client_option()].


%%%****************************************************************
%%% Checks if the public key Key is a host key for (any of) the
%%% host(s) in the argument Host with the port Port.
%%%
%%% Due to compatibility reasons, the OTP/SSH application first
%%% checks is_host_key/4 and then the old is_host_key/3

-callback is_host_key(Key :: public_key:public_key(),
                      Host :: inet:ip_address() | inet:hostname()
                            | [inet:ip_address() | inet:hostname()],
                      Port :: inet:port_number(),
		      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options(any())
                     ) ->
    boolean() | {error, term()} .

%%% is_host_key/4 is an old variant which is kept for compatibility.
%%% Use is_host_key/5 in new programs.

-callback is_host_key(Key :: public_key:public_key(),
                      Host :: string(),
                      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options(any())
                     ) ->
    boolean().

-optional_callbacks(
   [is_host_key/4, is_host_key/5     % One in the pair must be defined
   ]).


%%%****************************************************************
%%% Fetch the user's private key that is of type Algorithm.

-callback user_key(Algorithm :: ssh:pubkey_alg(),
                   Options :: client_key_cb_options(any())
                  ) ->
    {ok, public_key:private_key()} |
    {ok, {ssh2_pubkey, PubKeyBlob :: binary()}} |
    {error, string()}.


%%%****************************************************************
%%% Remembers that the the public key Key is a key for the host(s)
%%% in the argument Host with the port Port.
%%%
%%% Due to compatibility reasons, the OTP/SSH application first
%%% trys add_host_key/4 and then the old add_host_key/3

-callback add_host_key(Host :: inet:ip_address() | inet:hostname()
                             | [inet:ip_address() | inet:hostname()],
                       Port :: inet:port_number(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options(any())
                      ) ->
    ok | {error, term()}.

%%% is_host_key/3 is an old variant which is kept for compatibility.
%%% Use is_host_key/4 in new constructions.

-callback add_host_key(Host :: string(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options(any())
                      ) ->
    ok | {error, term()}.

-optional_callbacks(
   [add_host_key/3, add_host_key/4       % One in the pair be defined
   ]).


%%%****************************************************************
%%% Sign the SigData with the *private* key corresponding to PubKeyBlob
%%%

-callback sign(PubKeyBlob :: binary(),
               SigData :: binary(),
               Options :: client_key_cb_options(any())) ->
    Blob :: binary().

-optional_callbacks([sign/3]).

