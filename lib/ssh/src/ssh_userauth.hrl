%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

%%% Description: user authentication protocol

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
