%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
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

%%% Description: ssh_io replacement that throws on everything

-module(ssh_no_io).
-include("ssh_transport.hrl").

-export([yes_no/2, read_password/2, read_line/2, format/2]).

yes_no(_, _) ->
    throw({{no_io_allowed, yes_no},
	   #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			       description = "User interaction is not allowed",
			       language = "en"}}).

read_password(_, _) ->
    throw({{no_io_allowed, read_password},
	  #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			       description = "User interaction is not allowed",
			      language = "en"}}).

read_line(_, _) ->
    throw({{no_io_allowed, read_line},
	  #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			      description =  "User interaction is not allowed",
			      language = "en"}} ).

format(_, _) ->
    throw({{no_io_allowed, format},
	   #ssh_msg_disconnect{code = ?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
			       description =   "User interaction is not allowed",
			       language = "en"}}).
    
