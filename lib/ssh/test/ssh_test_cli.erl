-module(ssh_test_cli).

-export([init/1, terminate/2, handle_ssh_msg/2, handle_msg/2]).

-record(state, {
	  type,
	  tmpdir,
	  id, 
	  ref,
	  port
	 }).


init([Type]) -> init([Type,"/tmp"]);

init([Type,TmpDir]) ->
    {ok, #state{type = Type,
		tmpdir = TmpDir}}.

handle_msg({ssh_channel_up, Id, Ref}, S) ->
    User = get_ssh_user(Ref), 
    ok = ssh_connection:send(Ref,
			     Id,
			     << "\r\nYou are accessing a dummy, type \"q\" to exit\r\n\n" >>),
    Port = run_portprog(User, S#state.type, S#state.tmpdir),
    {ok, S#state{port = Port, id = Id, ref = Ref}};

handle_msg({Port, {data, Data}}, S = #state{port = Port}) ->
    ok = ssh_connection:send(S#state.ref, S#state.id, Data),
    {ok, S};
handle_msg({Port, {exit_status, Exit}}, S = #state{port = Port}) ->
    if 
        S#state.type =:= cli ->
            ok = ssh_connection:send(S#state.ref, S#state.id, << "\r\n" >>);
        true ->
            ok
    end,
    ok = ssh_connection:exit_status(S#state.ref, S#state.id, Exit),
    {stop, S#state.id, S#state{port = undefined}};
handle_msg({'EXIT', Port, _}, S = #state{port = Port}) ->
    ok = ssh_connection:exit_status(S#state.ref, S#state.id, 0),
    {stop, S#state.id, S#state{port = undefined}};
handle_msg(_Msg, S) ->
    {ok, S}.

handle_ssh_msg({ssh_cm, Ref, {data, Id, _Type, <<"q">>}}, S) ->
    ssh_connection:send_eof(Ref, Id),
    {stop, Id,  S};
handle_ssh_msg({ssh_cm, _Ref, {data, _Id, _Type, Data}}, S) ->
    true = port_command(S#state.port, Data),
    {ok, S};
handle_ssh_msg({ssh_cm, _, {eof, _}}, S) ->
    {ok, S};
handle_ssh_msg({ssh_cm, Ref, {env, Id, WantReply, _Var, _Value}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S};
handle_ssh_msg({ssh_cm, Ref, {pty, Id, WantReply, _Terminal_jox}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S};
handle_ssh_msg({ssh_cm, Ref, {shell, Id, WantReply}}, S) ->
    ok = ssh_connection:reply_request(Ref, WantReply, success, Id),
    {ok, S};
handle_ssh_msg({ssh_cm, _, {signal, _, _}}, S) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, S};
handle_ssh_msg({ssh_cm, _,
	{window_change, _Id, _Width, _Height, _Pixw, _PixH}}, S) ->
    {ok, S};
handle_ssh_msg({ssh_cm, _, {exit_signal, Id, _, _, _}},
	       S) ->
    {stop, Id,  S}.

terminate(_Why, _S) ->
    nop.

run_portprog(User, cli, TmpDir) ->
    Pty_bin = os:find_executable("cat"), 
    ssh_test_lib:open_port({spawn_executable, Pty_bin},
			   [stream,
			    {cd, TmpDir},
			    {env, [{"USER", User}]},
			    {args, []}]).

get_ssh_user(Ref) ->
    [{user, User}] = ssh:connection_info(Ref, [user]),
    User.

