%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%% Description: Ssh User Authentication Protocol


-define(SSH_MSG_USERAUTH_REQUEST,  50).
-define(SSH_MSG_USERAUTH_FAILURE,  51).
-define(SSH_MSG_USERAUTH_SUCCESS,  52).
-define(SSH_MSG_USERAUTH_BANNER,  53).
-define(SSH_MSG_USERAUTH_PK_OK,  60).
-define(SSH_MSG_USERAUTH_PASSWD_CHANGEREQ, 60).
-define(SSH_MSG_USERAUTH_INFO_REQUEST, 60).
-define(SSH_MSG_USERAUTH_INFO_RESPONSE, 61).

-record(ssh_msg_userauth_request,
	{
	  user,     %% string
	  service,  %% string
	  method,   %% string "publickey", "password"
	  data      %% opaque
	 }).

-record(ssh_msg_userauth_failure,
	{
	  authentications,     %% string
	  partial_success      %% boolean
	 }).

-record(ssh_msg_userauth_success,
	{
	 }).

-record(ssh_msg_userauth_banner,
	{
	  message,    %% string
	  language    %% string
	 }).

-record(ssh_msg_userauth_passwd_changereq,
	{
	  prompt,     %% string
	  languge     %% string
	 }).

-record(ssh_msg_userauth_pk_ok,
	{
	  algorithm_name, % string
	  key_blob % string
	 }).

-record(ssh_msg_userauth_info_request,
	{name,
	 instruction,
	 language_tag,
	 num_prompts,
	 data}).

-record(ssh_msg_userauth_info_response,
	{num_responses,
	 data}).

