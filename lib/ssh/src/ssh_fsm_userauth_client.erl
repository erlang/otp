%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles an ssh connection, e.i. both the
%% setup SSH Transport Layer Protocol (RFC 4253), Authentication
%% Protocol (RFC 4252) and SSH connection Protocol (RFC 4255)
%% Details of the different protocols are
%% implemented in ssh_transport.erl, ssh_auth.erl and ssh_connection.erl
%% ----------------------------------------------------------------------

-module(ssh_fsm_userauth_client).
-moduledoc false.

-include("ssh.hrl").
-include("ssh_transport.hrl").
-include("ssh_auth.hrl").
-include("ssh_connect.hrl").

-include("ssh_fsm.hrl").

%%====================================================================
%%% Exports
%%====================================================================

%%% Behaviour callbacks
-export([callback_mode/0, handle_event/4, terminate/3,
	 format_status/2, code_change/4]).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    [handle_event_function,
     state_enter].

%%--------------------------------------------------------------------

%%% ######## {userauth, client} ####

%%---- #ssh_msg_ext_info could follow after the key exchange, both the initial and the re-negotiation
handle_event(internal, #ssh_msg_ext_info{}=Msg, {userauth,client}, D0) ->
    %% FIXME: need new state to receive this msg!
    D = ssh_connection_handler:handle_ssh_msg_ext_info(Msg, D0),
    {keep_state, D};

%%---- received userauth success from the server
handle_event(internal, #ssh_msg_userauth_success{}, {userauth,client}, D0=#data{ssh_params = Ssh}) ->
    ssh_auth:ssh_msg_userauth_result(success),
    ssh_connection_handler:handshake(ssh_connected, D0),
    D = D0#data{ssh_params=Ssh#ssh{authenticated = true}},
    {next_state, {connected,client}, D, {change_callback_module,ssh_connection_handler}};


%%---- userauth failure response to clientfrom the server
handle_event(internal, #ssh_msg_userauth_failure{}, {userauth,client}=StateName,
	     #data{ssh_params = #ssh{userauth_methods = []}} = D0) ->
    {Shutdown, D} =
        ?send_disconnect(?SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE, 
                         io_lib:format("User auth failed for: ~p",[D0#data.auth_user]),
                         StateName, D0),
    {stop, Shutdown, D};

handle_event(internal, #ssh_msg_userauth_failure{authentications = Methods}, StateName={userauth,client},
	     D0 = #data{ssh_params = Ssh0}) ->
    %% The preferred authentication method failed, try next method
    Ssh1 = case Ssh0#ssh.userauth_methods of
	       none ->
		   %% Server tells us which authentication methods that are allowed
		   Ssh0#ssh{userauth_methods = string:tokens(Methods, ",")};
	       _ ->
		   %% We already know...
		   Ssh0 
	   end,
    case ssh_auth:userauth_request_msg(Ssh1) of
        {send_disconnect, Code, Ssh} ->
            {Shutdown, D} =
                ?send_disconnect(Code, 
                                 io_lib:format("User auth failed for: ~p",[D0#data.auth_user]),
                                 StateName, D0#data{ssh_params = Ssh}),
	    {stop, Shutdown, D};
	{"keyboard-interactive", {Msg, Ssh}} ->
            D = ssh_connection_handler:send_msg(Msg, D0#data{ssh_params = Ssh}),
	    {next_state, {userauth_keyboard_interactive,client}, D};
	{_Method, {Msg, Ssh}} ->
            D = ssh_connection_handler:send_msg(Msg, D0#data{ssh_params = Ssh}),
	    {keep_state, D}
    end;

%%---- banner to client
handle_event(internal, #ssh_msg_userauth_banner{message = Msg}, {userauth,client}, D) ->
    case D#data.ssh_params#ssh.userauth_quiet_mode of
	false -> io:format("~s", [Msg]);
	true -> ok
    end,
    keep_state_and_data;


%%% ######## {userauth_keyboard_interactive, client}

handle_event(internal, #ssh_msg_userauth_info_request{} = Msg, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0} = D0) ->
    case ssh_auth:handle_userauth_info_request(Msg, Ssh0) of
	{ok, {Reply, Ssh}} ->
            D = ssh_connection_handler:send_msg(Reply, D0#data{ssh_params = Ssh}),
	    {next_state, {userauth_keyboard_interactive_info_response,client}, D};
	not_ok ->
	    {next_state, {userauth,client}, D0, [postpone]}
    end;

handle_event(internal, #ssh_msg_userauth_failure{}, {userauth_keyboard_interactive, client},
	     #data{ssh_params = Ssh0} = D0) ->
    Prefs = [{Method,M,F,A} || {Method,M,F,A} <- Ssh0#ssh.userauth_preference,
			       Method =/= "keyboard-interactive"],
    D = D0#data{ssh_params = Ssh0#ssh{userauth_preference=Prefs}},
    {next_state, {userauth,client}, D, [postpone]};

handle_event(internal, #ssh_msg_userauth_failure{}, {userauth_keyboard_interactive_info_response, client},
	     #data{ssh_params = Ssh0} = D0) ->
    Opts = Ssh0#ssh.opts,
    D = case ?GET_OPT(password, Opts) of
	    undefined ->
		D0;
	    _ ->
		D0#data{ssh_params =
			    Ssh0#ssh{opts = ?PUT_OPT({password,not_ok}, Opts)}} % FIXME:intermodule dependency
	end,
    {next_state, {userauth,client}, D, [postpone]};

handle_event(internal, #ssh_msg_ext_info{}=Msg, {userauth_keyboard_interactive_info_response, client}, D0) ->
    %% FIXME: need new state to receive this msg!
    D = ssh_connection_handler:handle_ssh_msg_ext_info(Msg, D0),
    {keep_state, D};

handle_event(internal, #ssh_msg_userauth_success{}, {userauth_keyboard_interactive_info_response, client}, D) ->
    {next_state, {userauth,client}, D, [postpone]};

handle_event(internal, #ssh_msg_userauth_info_request{}, {userauth_keyboard_interactive_info_response, client}, D) ->
    {next_state, {userauth_keyboard_interactive,client}, D, [postpone]};


%%% ######## UNHANDLED EVENT!
handle_event(Type, Event, StateName, D) ->
    ssh_connection_handler:handle_event(Type, Event, StateName, D).

%%--------------------------------------------------------------------
format_status(A, B) ->
    ssh_connection_handler:format_status(A, B).

%%--------------------------------------------------------------------
terminate(Reason, StateName, D) ->
    ssh_connection_handler:terminate(Reason, StateName, D).

%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% Internal functions
%%====================================================================


