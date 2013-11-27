%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
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
    
