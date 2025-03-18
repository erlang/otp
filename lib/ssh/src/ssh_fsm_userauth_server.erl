%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(ssh_fsm_userauth_server).
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

%%% ######## {userauth, server} ####
%%---- userauth request to server
handle_event(internal, 
	     Msg = #ssh_msg_userauth_request{service = ServiceName,
                                             method = Method,
                                             user = User},
	     StateName = {userauth,server},
	     D0) ->
    D1 = maybe_send_banner(D0, User),
    #data{ssh_params=Ssh0} = D1,
    case {ServiceName, Ssh0#ssh.service, Method} of
	{"ssh-connection", "ssh-connection", "none"} ->
	    %% Probably the very first userauth_request but we deny unauthorized login
            %% However, we *may* accept unauthorized login if instructed so
            case ssh_auth:handle_userauth_request(Msg, Ssh0#ssh.session_id, Ssh0) of
                {not_authorized, _, {Reply,Ssh}} ->
                    D = ssh_connection_handler:send_msg(Reply, D1#data{ssh_params = Ssh}),
                    {keep_state, D};
                {authorized, User, {Reply, Ssh1}} ->
                    D = connected_state(Reply, Ssh1, User, Method, D1),
                    {next_state, {connected,server}, D,
                     [set_max_initial_idle_timeout(D),
                      {change_callback_module,ssh_connection_handler}
                     ]
                    }
                     
            end;
	
	{"ssh-connection", "ssh-connection", Method} ->
	    %% Userauth request with a method like "password" or so
	    case lists:member(Method, Ssh0#ssh.userauth_methods) of
		true ->
		    %% Yepp! we support this method
		    case ssh_auth:handle_userauth_request(Msg, Ssh0#ssh.session_id, Ssh0) of
			{authorized, User, {Reply, Ssh1}} ->
                            D = connected_state(Reply, Ssh1, User, Method, D1),
                            {next_state, {connected,server}, D,
                             [set_max_initial_idle_timeout(D),
                              {change_callback_module,ssh_connection_handler}
                             ]};
			{not_authorized, {User, Reason}, {Reply, Ssh}} when Method == "keyboard-interactive" ->
			    retry_fun(User, Reason, D1),
                            D = ssh_connection_handler:send_msg(Reply, D1#data{ssh_params = Ssh}),
			    {next_state, {userauth_keyboard_interactive,server}, D};
			{not_authorized, {User, Reason}, {Reply, Ssh}} ->
			    retry_fun(User, Reason, D1),
                            D = ssh_connection_handler:send_msg(Reply, D1#data{ssh_params = Ssh}),
			    {keep_state, D}
		    end;
		false ->
		    %% No we do not support this method (=/= none)
		    %% At least one non-erlang client does like this. Retry as the next event
		    {keep_state_and_data,
		     [{next_event, internal, Msg#ssh_msg_userauth_request{method="none"}}]
		    }
	    end;

	%% {"ssh-connection", Expected, Method} when Expected =/= ServiceName -> Do what?
	%% {ServiceName,      Expected, Method} when Expected =/= ServiceName -> Do what?

	{ServiceName, _, _} when ServiceName =/= "ssh-connection" ->
            {Shutdown, D} =  
                ?send_disconnect(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                                 io_lib:format("Unknown service: ~p",[ServiceName]),
                                 StateName, D1),
            {stop, Shutdown, D}
    end;

handle_event(internal, #ssh_msg_userauth_info_response{} = Msg, {userauth_keyboard_interactive, server}, D0) ->
    case ssh_auth:handle_userauth_info_response(Msg, D0#data.ssh_params) of
	{authorized, User, {Reply, Ssh1}} ->
            D = connected_state(Reply, Ssh1, User, "keyboard-interactive", D0),
            {next_state, {connected,server}, D,
             [set_max_initial_idle_timeout(D),
              {change_callback_module,ssh_connection_handler}
             ]};
	{not_authorized, {User, Reason}, {Reply, Ssh}} ->
	    retry_fun(User, Reason, D0),
            D = ssh_connection_handler:send_msg(Reply, D0#data{ssh_params = Ssh}),
	    {next_state, {userauth,server}, D};

	{authorized_but_one_more, _User,  {Reply, Ssh}} ->
            D = ssh_connection_handler:send_msg(Reply, D0#data{ssh_params = Ssh}),
	    {next_state, {userauth_keyboard_interactive_extra,server}, D}
    end;

handle_event(internal, #ssh_msg_userauth_info_response{} = Msg, {userauth_keyboard_interactive_extra, server}, D0) ->
    {authorized, User, {Reply, Ssh1}} =
        ssh_auth:handle_userauth_info_response({extra,Msg}, D0#data.ssh_params),
    D = connected_state(Reply, Ssh1, User, "keyboard-interactive", D0),
    {next_state, {connected,server}, D,
     [set_max_initial_idle_timeout(D),
      {change_callback_module,ssh_connection_handler}
     ]
    };


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

connected_state(Reply, Ssh1, User, Method, D0) ->
    D1 = #data{ssh_params=Ssh} =
        ssh_connection_handler:send_msg(Reply, D0#data{ssh_params = Ssh1}),
    ssh_connection_handler:handshake(ssh_connected, D1),
    connected_fun(User, Method, D1),
    D1#data{auth_user=User,
            %% Note: authenticated=true MUST NOT be sent
            %% before send_msg!
            ssh_params = Ssh#ssh{authenticated = true}}.


set_max_initial_idle_timeout(#data{ssh_params = #ssh{opts=Opts}}) ->
    {{timeout,max_initial_idle_time}, ?GET_OPT(max_initial_idle_time,Opts), none}.

connected_fun(User, Method, #data{ssh_params = #ssh{peer = {_,Peer}}} = D) ->
    ?CALL_FUN(connectfun,D)(User, Peer, Method).


retry_fun(_, undefined, _) ->
    ok;
retry_fun(User, Reason, #data{ssh_params = #ssh{opts = Opts,
						peer = {_,Peer}
					       }}) ->
    {Tag,Info} =
	case Reason of
	    {error, Error} ->
		{failfun, Error};
	    _ ->
		{infofun, Reason}
	end,
    Fun = ?GET_OPT(Tag, Opts),
    try erlang:fun_info(Fun, arity)
    of
	{arity, 2} -> %% Backwards compatible
	    catch Fun(User, Info);
	{arity, 3} ->
	    catch Fun(User, Peer, Info);
	_ ->
	    ok
    catch
	_:_ ->
	    ok
    end.

maybe_send_banner(D0 = #data{ssh_params = #ssh{userauth_banner_sent = false} = Ssh}, User) ->
    BannerFun = ?GET_OPT(bannerfun, Ssh#ssh.opts),
    case BannerFun(User) of
        BannerText when is_binary(BannerText), byte_size(BannerText) > 0 ->
            Banner = #ssh_msg_userauth_banner{message = BannerText,
                                              language = <<>>},
            D = D0#data{ssh_params = Ssh#ssh{userauth_banner_sent = true}},
            ssh_connection_handler:send_msg(Banner, D);
        _ ->
            D0
    end;
maybe_send_banner(D, _) ->
    D.
